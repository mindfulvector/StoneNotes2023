unit ThreadList;

interface

uses
  PsAPI,
  TlHelp32,
  Windows,
  SysUtils;

procedure KillAllThreads;

implementation

uses
  Logger;

procedure KillAllThreads;
var
  SnapProcHandle: THandle;
  IsMore: Boolean;
  ThreadEntry: TThreadEntry32;
  PID: Cardinal;
  Result: Boolean;
  LastError: DWORD;
begin
  PID := GetCurrentProcessId;
  SnapProcHandle := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0); //Takes a snapshot of the all threads
  Result := (SnapProcHandle <> INVALID_HANDLE_VALUE);
  if Result then
  try
    ThreadEntry.dwSize := SizeOf(ThreadEntry);

    // Get first thread
    IsMore := Thread32First(SnapProcHandle, ThreadEntry);
    while IsMore do
    begin
      if ThreadEntry.th32OwnerProcessID = PID then
        if not TerminateThread(ThreadEntry.th32ThreadID, 0) then
        begin
          LastError := GetLastError();
          Logger.Log('Cannot terminate thread #'+IntToStr(ThreadEntry.th32ThreadID)+' error: '+IntToStr(LastError));
        end;

      // Get next thread
      IsMore := Thread32Next(SnapProcHandle, ThreadEntry);
    end;
  finally
    CloseHandle(SnapProcHandle);//Close the Handle
  end;
end;

end.
