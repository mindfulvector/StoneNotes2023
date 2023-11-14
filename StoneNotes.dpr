program StoneNotes;

uses
  System.StartUpCopy,
  FMX.Forms,
  mv.BufferPanel in 'mv.BufferPanel.pas',
  mv.FMX.StyleMaker in 'mv.FMX.StyleMaker.pas',
  mv.InputQueryDropdown in 'mv.InputQueryDropdown.pas',
  mv.Logger in 'mv.Logger.pas',
  mv.MainForm in 'mv.MainForm.pas' {frmStoneNotes},
  mv.PluginEditor in 'mv.PluginEditor.pas',
  mv.PluginManager in 'mv.PluginManager.pas',
  mv.PluginService in 'mv.PluginService.pas',
  mv.PluginStorageService in 'mv.PluginStorageService.pas',
  mv.Process.NameDelphiThreads in 'mv.Process.NameDelphiThreads.pas',
  mv.Process.ProcessCleanup in 'mv.Process.ProcessCleanup.pas',
  mv.Process.ServerMonitor in 'mv.Process.ServerMonitor.pas',
  mv.Process.UnloadAllModules in 'mv.Process.UnloadAllModules.pas',
  mv.SplitterPanel in 'mv.SplitterPanel.pas',
  mv.SplitterSerializer in 'mv.SplitterSerializer.pas',
  mv.StringUtils in 'mv.StringUtils.pas',
  mv.WebView.BrowserForm in 'mv.WebView.BrowserForm.pas' {BrowserForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmStoneNotes, frmStoneNotes);
  Application.Run;
end.
