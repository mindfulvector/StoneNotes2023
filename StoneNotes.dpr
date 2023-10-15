program StoneNotes;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {frmStoneNotes},
  FMX.BufferPanel in 'FMX.BufferPanel.pas',
  FMX.SplitterPanel in 'FMX.SplitterPanel.pas',
  SplitterSerializer in 'SplitterSerializer.pas',
  PluginManager in 'PluginManager.pas',
  Logger in 'Logger.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmStoneNotes, frmStoneNotes);
  Application.Run;
end.
