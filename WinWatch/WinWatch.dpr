program WinWatch;

uses
  Vcl.Forms,
  WinWatchForm in 'WinWatchForm.pas' {Form1},
  CaptureWindow in 'CaptureWindow.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
