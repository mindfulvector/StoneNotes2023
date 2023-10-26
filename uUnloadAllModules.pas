unit uUnloadAllModules;

interface

uses
  Windows, SysUtils, TlHelp32;


procedure UnloadAllModules;

implementation

uses
  Logger;

procedure UnloadAllModules;
var
  hSnapShot: THandle;
  ModuleEntry: TModuleEntry32;
  ProcessID: DWORD;
begin
  ProcessID := GetCurrentProcessId;
  hSnapShot := CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, ProcessID);
  if hSnapShot <> INVALID_HANDLE_VALUE then
  try
    ModuleEntry.dwSize := SizeOf(ModuleEntry);
    if Module32First(hSnapShot, ModuleEntry) then
    repeat
      if 'WebView2Loader.dll' = ModuleEntry.szModule then
      begin
        Logger.Log('Unload module ' + ModuleEntry.szModule);
        FreeLibrary(ModuleEntry.hModule);
      end;
    until not Module32Next(hSnapShot, ModuleEntry);
  finally
    CloseHandle(hSnapShot);
  end;
end;


end.
