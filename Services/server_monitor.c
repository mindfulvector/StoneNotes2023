#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#ifndef UNICODE
#define UNICODE
#endif
#include <windows.h>
#include <string.h>
#else
#define TCHAR char
#define L ""
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#endif

typedef struct {
    TCHAR* cmd;
#ifdef _WIN32
    PROCESS_INFORMATION pi;
#else
    pid_t pid;
#endif
} ProcessInfo;

#ifdef _WIN32
#define perror(msg) if(1==1){ printf(msg " failed (%d).\n", GetLastError()); ExitProcess(1); }
#endif

void StartServer(ProcessInfo* processInfo, const TCHAR* cmd) {
    if (NULL == processInfo) perror("processInfo NULL");
    if (NULL == cmd) perror("cmd NULL");
#ifdef _WIN32
    if (wcslen(cmd) == 0) perror("cmd length is 0");
    if (wcslen(cmd) >= 512) perror("cmd length greater than 511 characters");
#else
    if (strlen(cmd) == 0) perror("cmd length is 0");
    if (strlen(cmd) >= 512) perror("cmd length greater than 511 characters");
#endif

    // Only need to create a new buffer if there isn't already one on the struct
    if (NULL == processInfo->cmd) {
        processInfo->cmd = (TCHAR*)calloc(1, 1024);
        if (processInfo->cmd == NULL) {
            perror("calloc");
            exit(EXIT_FAILURE);
        }
    }

    // Copy command string for restarts in MonitorServer
#ifdef _WIN32
    wcscpy(processInfo->cmd, cmd);
#else
    strcpy(processInfo->cmd, cmd);
#endif
#ifdef _WIN32
    STARTUPINFO si = { sizeof(STARTUPINFO) };
    if (!CreateProcess(NULL, processInfo->cmd, NULL, NULL, FALSE, 0, NULL, NULL, &si, &processInfo->pi)) {
        CloseHandle(processInfo->pi.hProcess);
        CloseHandle(processInfo->pi.hThread);
        perror("CreateProcess");
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
        perror("execvp");
        exit(EXIT_FAILURE);
    }
    else if (processInfo->pid < 0) {
        perror("fork");
        exit(EXIT_FAILURE);
    }
#endif
}

void MonitorServer(ProcessInfo* processInfo) {
    while (1) {
#ifdef _WIN32
        DWORD exitCode;
        GetExitCodeProcess(processInfo->pi.hProcess, &exitCode);
        if (exitCode != STILL_ACTIVE) {
            printf("Server exited with status %d. Restarting...\n", exitCode);
            CloseHandle(processInfo->pi.hProcess);
            CloseHandle(processInfo->pi.hThread);
            StartServer(processInfo, processInfo->cmd);
        }
#else
        int status;
        waitpid(processInfo->pid, &status, 0);
        if (WIFEXITED(status) || WIFSIGNALED(status)) {
            printf("Server exited. Restarting...\n");
            StartServer(processInfo, processInfo->cmd);
        }
#endif
        // Sleep for a while to reduce polling frequency
#ifdef _WIN32
        Sleep(1000);
#else
        sleep(1);
#endif
    }
}

int main() {
    printf("Starting server...\n");
    const TCHAR* cmd = L"deno run --allow-run --allow-net --allow-read --allow-write --unstable server.ts ./";
    ProcessInfo processInfo = { 0 };
    StartServer(&processInfo, cmd);
    printf("Monitoring server...\n");
    MonitorServer(&processInfo);
    printf("Monitor exit.\n");
    return 0;
}
