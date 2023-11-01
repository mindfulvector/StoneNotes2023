unit uServerMonitor;

interface

uses
  System.SysUtils, System.Classes, Winapi.Windows;

type
  TServerMonitor = class(TThread)
  private
    FProcessInfo: record
      Cmd: string;
      ProcessID: Cardinal;
    end;
    FDir: string;
    procedure StartServer(const Cmd: string);
    procedure MonitorServer;
    function FindUtilityFile(ADir: string; AFilename: string): string;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean);
    property Directory: string read FDir write FDir;
  end;

implementation

uses
  Logger;

{ Utility Procedures }

procedure Die(const Msg: string);
var
  Error: Integer;
begin
  Error := GetLastError;
  raise Exception.CreateFmt('%s fatal error (%d).', [Msg, Error]);
end;

{ TServerMonitor }

constructor TServerMonitor.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FDir := '.';
end;

procedure TServerMonitor.StartServer(const Cmd: string);
var
  SI: TStartupInfo;
  PI: TProcessInformation;
begin
  if Cmd = '' then Die('Cmd length is 0');
  if Length(Cmd) >= 512 then Die('Cmd length greater than 511 characters');

  FProcessInfo.Cmd := Cmd;

  FillChar(SI, SizeOf(SI), 0);
  SI.cb := SizeOf(SI);

  if not CreateProcess(nil, PChar(Cmd), nil, nil, False, 0, nil, nil, SI, PI) then
  begin
    CloseHandle(PI.hProcess);
    CloseHandle(PI.hThread);
    Die('CreateProcess');
  end
  else
  begin
    FProcessInfo.ProcessID := PI.dwProcessId;
    CloseHandle(PI.hProcess);
    CloseHandle(PI.hThread);
  end;
end;

procedure TServerMonitor.MonitorServer;
var
  LastWriteTime: TDateTime;
  CurrentWriteTime: TDateTime;
  SR: TSearchRec;
  hProcess: THandle;
  dwExitCode: DWORD;
begin
  LastWriteTime := 0;
  CurrentWriteTime := 0;
  ZeroMemory(@SR, SizeOf(SR));
  hProcess := 0;
  dwExitCode := 0;
  if FindFirst(IncludeTrailingPathDelimiter(FDir) + '*.*', faAnyFile, SR) = 0 then
  //try
    LastWriteTime := FileDateToDateTime(SR.Time);
    while not Terminated do  // Check for Terminated property to allow thread termination
    begin
      Sleep(5000);  // Wait for 5 seconds to reduce CPU usage

      // Check if the process has crashed
      hProcess := OpenProcess(PROCESS_QUERY_INFORMATION, False, FProcessInfo.ProcessID);
      if hProcess <> 0 then
      begin
        if GetExitCodeProcess(hProcess, dwExitCode) then
        begin
          if dwExitCode <> STILL_ACTIVE then
          begin
            Logger.Log('Process has crashed. Restarting...');
            StartServer(FProcessInfo.Cmd);
            CloseHandle(hProcess);
            Continue;       // So we don't restart twice if files also changed
          end;
        end;
        CloseHandle(hProcess);
      end
      else
      begin
        Logger.Log('Process handle could not be retrieved. Restarting...');
        StartServer(FProcessInfo.Cmd);
        Continue;       // So we don't restart twice if files also changed
      end;

      // Check if files have changed
      if FindFirst(IncludeTrailingPathDelimiter(FDir) + '*.*', faAnyFile, SR) = 0 then
      begin
        CurrentWriteTime := FileDateToDateTime(SR.Time);
        if CurrentWriteTime > LastWriteTime then
        begin
          Logger.Log('Change detected. Restarting...');
          TerminateProcess(OpenProcess(PROCESS_TERMINATE, False, FProcessInfo.ProcessID), 0);
          StartServer(FProcessInfo.Cmd);
          LastWriteTime := CurrentWriteTime;
        end;
      end;
    end;
  //finally
  //  FindClose(SR);
  //end;
end;

function TServerMonitor.FindUtilityFile(ADir: string; AFilename: string): string;
var
  CurrentPath: string;
  Path: string;
  LastPath: string;
begin
  Result := '';

  CurrentPath := GetCurrentDir;
  while CurrentPath <> '' do
  begin
    Path := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(CurrentPath) + ADir) + AFilename;
    Logger.Log('check ' + Path);
    if FileExists(Path) then
      Exit(Path);
//    else
//      Logger.Log('error! ' + IntToStr(GetLastError));
    LastPath := CurrentPath;
    CurrentPath := ExtractFileDir(CurrentPath);
    if LastPath = CurrentPath then
      Exit;
  end;
end;

procedure TServerMonitor.Execute;
var
  DenoPath, ServerPath, Cmd: string;
begin
  Logger.Log(Format('$$$ server_monitor compiled on %s at %s', [DateToStr(Now), TimeToStr(Now)]));

  DenoPath := FindUtilityFile('Support', 'stonenotes_deno.exe');

  if DenoPath = '' then
  begin
    Logger.Log('stonenotes_deno.exe not found');
    Exit;
  end;
  Logger.Log('stonenotes_deno.exe found at ' + DenoPath);

  ServerPath := FindUtilityFile('Services', 'server.ts');
  if ServerPath = '' then
  begin
    Logger.Log('server.ts not found');
    Exit;
  end;
  Logger.Log('server.ts found at ' + ServerPath);

  Cmd := Format('%s run '    +
    '--allow-run '     +
    '--allow-net '     +
    '--allow-read '    +
    '--allow-write '   +
    '--unstable '      +
    '%s '              +
    '%s', [DenoPath, ServerPath, ExtractFileDir(ServerPath)]);

  Logger.Log('Final command: ' + Cmd);

  Logger.Log('Starting server...');
  StartServer(Cmd);

  Logger.Log('Monitoring server...');
  MonitorServer;
end;

end.

