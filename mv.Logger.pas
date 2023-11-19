unit mv.Logger;

interface

procedure Log(const Msg: string);
procedure FlushLogBuffer;

implementation

uses
  System.SysUtils, System.Classes, System.SyncObjs;

const
  BUFFER_SIZE = 1000;

var
  LogBuffer: TStringList;
  LogCriticalSection: TCriticalSection;
  LogFileName: string;

procedure FlushLogBuffer;
var
  LogFile: TextFile;
begin
  AssignFile(LogFile, LogFileName);

  if not FileExists(LogFileName) then
    Rewrite(LogFile)
  else
    Append(LogFile);

  try
    Write(LogFile, LogBuffer.Text);
  finally
    CloseFile(LogFile);
  end;

  LogBuffer.Clear;
end;

procedure Log(const Msg: string);
begin
  LogCriticalSection.Enter;
  try
    LogBuffer.Add(DateTimeToStr(Now) + ': ' + Msg);

    if LogBuffer.Count >= BUFFER_SIZE then
      FlushLogBuffer;
  finally
    LogCriticalSection.Leave;
  end;
end;

initialization
  LogBuffer := TStringList.Create;
  LogCriticalSection := TCriticalSection.Create;
  LogFileName := ExtractFilePath(ParamStr(0)) + 'log.txt';

finalization
  if LogBuffer.Count > 0 then
    FlushLogBuffer;

  LogBuffer.Free;
  LogCriticalSection.Free;

end.

