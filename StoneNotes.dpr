program StoneNotes;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {frmStoneNotes},
  FMX.BufferPanel in 'FMX.BufferPanel.pas',
  FMX.SplitterPanel in 'FMX.SplitterPanel.pas',
  SplitterSerializer in 'SplitterSerializer.pas',
  PluginManager in 'PluginManager.pas',
  Logger in 'Logger.pas',
  StringUtils in 'StringUtils.pas',
  PluginStorageService in 'PluginStorageService.pas',
  StyleMaker in 'StyleMaker.pas',
  InputQueryDropdown in 'InputQueryDropdown.pas',
  PluginEditor in 'PluginEditor.pas',
  ServerThread in 'ServerThread.pas',
  WebSocketServer in 'WebSocketServer.pas',
  UBrowserForm in 'UBrowserForm.pas' {BrowserForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmStoneNotes, frmStoneNotes);
  Application.Run;
end.
