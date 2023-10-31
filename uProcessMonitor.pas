unit uProcessMonitor;

interface

uses
  System.Classes, System.SysUtils, Vcl.Dialogs, System.UITypes, System.UIConsts,
  Winapi.ShellAPI;

type
  TProcessMonitor = class(TThread)
  private
    FProcessName: string;
    procedure RestartPrompt;
  protected
    procedure Execute; override;
  public
    constructor Create(const ProcessName: string);
  end;

implementation

uses
  Winapi.Windows, Winapi.TlHelp32;

{ TProcessMonitor }

constructor TProcessMonitor.Create(const ProcessName: string);
begin
  inherited Create(True); // Create the thread suspended
  FProcessName := ProcessName;
  FreeOnTerminate := True;
  Resume; // Start the thread
end;

procedure TProcessMonitor.Execute;
var
  hSnapshot: THandle;
  pe: TProcessEntry32;
  Found: Boolean;
  searchName: string;
  exeFile: string;
begin
  while not Terminated do
  begin
    hSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    if hSnapshot = INVALID_HANDLE_VALUE then Exit;

    pe.dwSize := SizeOf(TProcessEntry32);
    Found := Process32First(hSnapshot, pe);
    searchName := ExtractFileName(FProcessName);
    while Found do
    begin
      exeFile := pe.szExeFile;
      if SameText(searchName, exeFile) then
      begin
        // The process is running
        CloseHandle(hSnapshot);
        Sleep(5000);  // Check every 5 seconds
        Break;
      end;
      Found := Process32Next(hSnapshot, pe);
    end;

    if not Found then
      Synchronize(RestartPrompt); // The process is not running, prompt for restart

    CloseHandle(hSnapshot);
  end;
end;

procedure TProcessMonitor.RestartPrompt;
begin
  if MessageDlg('The backend server monitor has stopped. Would you like to restart it?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      ShellExecute(0, 'open', PChar(FProcessName), nil, nil, SW_SHOW)
  else begin
    MessageDlg('The backend server will no longer be monitored. To restart monitoring, please close and re-open the application.', mtInformation, [mbOK], 0);
    Self.Terminate;
  end;
end;

end.

