#!tcc -run 

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <wchar.h>
#include <locale.h>
#include <limits.h>

#ifdef _WIN32
#ifndef UNICODE
#define UNICODE
#endif
#ifndef PATH_MAX
#define PATH_MAX MAX_PATH
#endif
#define PATH_DELIM '\\'
#include <tchar.h>
#include <windows.h>
#include <string.h>
#include <direct.h>
#else
#define PATH_DELIM '/'
#define TCHAR char
#define _T(x) x
#define _tgetcwd getcwd
#define _sntprintf snprintf
#define _tfopen fopen
#define _tprintf printf
#define _tcslen strlen
#define _tcscpy strcpy
#define _tcsrchr strrchr
#define _tcslen strlen
#define L ""
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <errno.h>
#endif

typedef struct {
    TCHAR* cmd;
#ifdef _WIN32
    PROCESS_INFORMATION pi;
#else
    pid_t pid;
#endif
} ProcessInfo;


void die(const TCHAR* msg) {
#ifdef _WIN32
    int error = GetLastError();
#else
    int error = errno;
#endif
    _tprintf(L"%s fatal error (%d).\n", msg, error);
    exit(error);
}

void StartServer(ProcessInfo* processInfo, const TCHAR* cmd) {
    if (NULL == processInfo) die(L"processInfo NULL");
    if (NULL == cmd) die(L"cmd NULL");
    if (_tcslen(cmd) == 0) die(L"cmd length is 0");
    if (_tcslen(cmd) >= 512) die(L"cmd length greater than 511 characters");

    // Only need to create a new buffer if there isn't already one on the struct
    if (NULL == processInfo->cmd) {
        processInfo->cmd = (TCHAR*)calloc(1, 1024);
        if (processInfo->cmd == NULL) {
            die(L"calloc");
            exit(EXIT_FAILURE);
        }
    }

    // Copy command string for restarts in MonitorServer
    _tcscpy_s(processInfo->cmd, 1024, cmd);

    // Start the server process
#ifdef _WIN32
    STARTUPINFO si = { sizeof(STARTUPINFO) };
    if (!CreateProcessW(NULL, processInfo->cmd, NULL, NULL, FALSE, 0, NULL, NULL, &si, &processInfo->pi)) {
        CloseHandle(processInfo->pi.hProcess);
        CloseHandle(processInfo->pi.hThread);
        die(L"CreateProcess");
    }
#else
    processInfo->pid = fork();
    if (processInfo->pid == 0) {
        TCHAR* arg = strtok(strdup(cmd), " ");
        TCHAR* args[255] = { arg };
        int i = 1;
        while ((arg = strtok(NULL, " ")))
            args[i++] = arg;
        args[i] = NULL;
        execvp(args[0], args);
        die(L"execvp");
        exit(EXIT_FAILURE);
    }
    else if (processInfo->pid < 0) {
        die(L"fork");
        exit(EXIT_FAILURE);
    }
#endif
}

void MonitorServer(ProcessInfo* processInfo, const TCHAR* dir) {
#ifdef _WIN32
    HANDLE hDir = CreateFileW(dir, FILE_LIST_DIRECTORY,
        FILE_SHARE_READ | FILE_SHARE_DELETE,
        NULL, OPEN_EXISTING,
        FILE_FLAG_BACKUP_SEMANTICS,
        NULL);
    if (hDir == INVALID_HANDLE_VALUE) die(L"CreateFile");
    char buffer[1024];
    DWORD bytesReturned;
#else
    int fd = inotify_init();
    int wd = inotify_add_watch(fd, dir, IN_MODIFY | IN_CREATE);
#endif

    while (1) {
#ifdef _WIN32
        if (ReadDirectoryChangesW(hDir, buffer, sizeof(buffer),
            TRUE, FILE_NOTIFY_CHANGE_LAST_WRITE,
            &bytesReturned, NULL, NULL)) {
            _tprintf(L"Change detected. Restarting...\n");
            TerminateProcess(processInfo->pi.hProcess, 1);
            CloseHandle(processInfo->pi.hProcess);
            CloseHandle(processInfo->pi.hThread);
            StartServer(processInfo, processInfo->cmd);
        }
#else
        char buff[4096];
        int length = read(fd, buff, 4096);
        if (length < 0) die(L"read");
        if (length > 0) {
            _tprintf(L"Change detected. Restarting...\n");
            kill(processInfo->pid, SIGKILL);
            StartServer(processInfo, processInfo->cmd);
        }
#endif
    }
}

TCHAR* findServerFile() {
    TCHAR* currentPath = _tgetcwd(NULL, 0);
    TCHAR path[PATH_MAX] = { 0 };
    while (currentPath) {
        _sntprintf_s(path, sizeof(path) / sizeof(TCHAR), _TRUNCATE, _T("%s%cserver.ts"), currentPath, PATH_DELIM);
        _tprintf(_T("check %s\n"), path);
        DWORD attributes = GetFileAttributesW(path);
        _tprintf(_T("attributes=%d\n"), attributes);
        if (attributes != INVALID_FILE_ATTRIBUTES && !(attributes & FILE_ATTRIBUTE_DIRECTORY)) {
            free(currentPath);
            TCHAR* fullPath = (TCHAR*)malloc((_tcslen(path) + 1) * sizeof(TCHAR));
            _tcscpy_s(fullPath, _tcslen(path) + 1, path);
            return fullPath;
        } else {
            _tprintf(_T("error! %d\n"), GetLastError());
        }
        TCHAR* lastSlash = _tcsrchr(currentPath, PATH_DELIM);
        if (lastSlash) *lastSlash = _T('\0');
        else break;
    }
    free(currentPath);
    return NULL;
}

int main() {
    _tprintf(L"$$$ server_monitor compiled on %hs at %hs\n\n", __DATE__, __TIME__);

    TCHAR* serverPath = findServerFile();
    if (!serverPath) {
        fprintf(stderr, "server.ts not found\n");
        exit(EXIT_FAILURE);
    }
    _tprintf(L"server.ts found at %s\n", serverPath);

    const TCHAR* cmdFormat = L"deno run "         // Possibly too permissive but current in
        "--allow-run "      //      dev/beta so we are going to keep
        "--allow-net "      //      them thus for now.
        "--allow-read "
        "--allow-write "
        "--unstable "       // Unstable features enabled during beta
        "%s "               // Main server process
        "./";               // DocumentRoot for the server
    

    int cmdSize = _sntprintf_s(NULL, 0, _TRUNCATE, cmdFormat, serverPath);  // Determine the size needed
    TCHAR* cmd = (TCHAR*)malloc(cmdSize * sizeof(TCHAR));                   // Allocate the buffer
    _sntprintf_s(cmd, cmdSize, _TRUNCATE, cmdFormat, serverPath);                      // Write the command string

    _tprintf(L"Final command: %s\n", cmd);

    // Initialize all local stack variables to 0
    ProcessInfo processInfo = { 0 };

    // Start the server process using the above command stirng
    _tprintf(L"Starting server...\n");
    StartServer(&processInfo, cmd);

    // We now begin monitoring the server for crashes and the document root directory
    // for changes. Upon any change, we restart the server.
    _tprintf(L"Monitoring server...\n");
    MonitorServer(&processInfo, L"./");

    // Clean exit of the monitor process (could still have been a crash of the server
    // but this should be very unlikely hopefully!)
    _tprintf(L"Monitor exit.\n");

    free(serverPath);
    return 0;
}
