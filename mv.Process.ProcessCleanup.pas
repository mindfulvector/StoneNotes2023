unit mv.Process.ProcessCleanup;

interface

uses
  Windows, SysUtils, TlHelp32;

function GetChildProcesses(ParentPID: DWORD): TArray<DWORD>;
procedure TerminateProcessTree(PID: DWORD);

implementation


// Get the direct child processes of a given process
function GetChildProcesses(ParentPID: DWORD): TArray<DWORD>;
var
  hSnapshot: THandle;
  pe: TProcessEntry32;
  ChildPIDs: TArray<DWORD>;
begin
  hSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if hSnapshot = INVALID_HANDLE_VALUE then Exit;

  try
    pe.dwSize := SizeOf(pe);
    if Process32First(hSnapshot, pe) then
    begin
      repeat
        if pe.th32ParentProcessID = ParentPID then
        begin
          SetLength(ChildPIDs, Length(ChildPIDs) + 1);
          ChildPIDs[High(ChildPIDs)] := pe.th32ProcessID;
        end;
      until not Process32Next(hSnapshot, pe);
    end;
  finally
    CloseHandle(hSnapshot);
  end;

  Result := ChildPIDs;
end;

// Recursively terminate the process tree
procedure TerminateProcessTree(PID: DWORD);
var
  ChildPIDs: TArray<DWORD>;
  ProcessHandle: THandle;
  I: Integer;
begin
  // First, get child processes and terminate their trees
  ChildPIDs := GetChildProcesses(PID);
  for I := 0 to High(ChildPIDs) do
    TerminateProcessTree(ChildPIDs[I]);

  // Now, terminate the given process
  ProcessHandle := OpenProcess(PROCESS_TERMINATE, False, PID);
  if ProcessHandle <> 0 then
  try
    TerminateProcess(ProcessHandle, 0);
  finally
    CloseHandle(ProcessHandle);
  end;
end;

end.

end.
