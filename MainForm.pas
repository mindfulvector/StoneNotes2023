unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Graphics, FMX.Dialogs, FMX.SplitterPanel, FMX.Controls.Presentation,
  FMX.Edit, FMX.StdCtrls, FMX.TMSFNCTypes, FMX.TMSFNCUtils, FMX.TMSFNCGraphics,
  FMX.TMSFNCGraphicsTypes, FMX.TMSFNCCustomControl, FMX.TMSFNCWebBrowser, SplitterSerializer,
  System.IOUtils, PluginManager, Logger;

type
  TfrmStoneNotes = class(TForm)
    btnSplitRight: TButton;
    btnSplitLeft: TButton;
    btnSave: TButton;
    saveDlg: TSaveDialog;
    openDlg: TOpenDialog;
    btnOpen: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure btnSplitRightClick(Sender: TObject);
    procedure btnSplitLeftClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
  private
    FSplitterPanel: TSplitterPanel;
    //FSerializer: TSerializedSplitter;
    FLastSplitterRight: TSplitterPanel;
    FLastSplitterLeft: TSplitterPanel;
    FPluginManager: TPluginManager;
  public
    { Public declarations }
  end;

var
  frmStoneNotes: TfrmStoneNotes;

implementation

{$R *.fmx}

procedure TfrmStoneNotes.btnOpenClick(Sender: TObject);
var
  serializer: TSerializedSplitter;
  data: string;
begin
  if openDlg.Execute then
  begin
    data := TFile.ReadAllText(openDlg.FileName);
    Self.Caption := 'StoneNotes Matrix - ' + ExtractFileName(openDlg.FileName);
    FLastSplitterRight := nil;
    FLastSplitterLeft := nil;
    FSplitterPanel.DisposeOf;
    FSplitterPanel := nil;
    //if Assigned(FSerializer) then
    //  FSerializer.Free;
    //FSerializer := TSerializedSplitter.Create.FromString(data);
    //FSplitterPanel := FSerializer.CreateSplitter(Self, FPluginManager);
    serializer := TSerializedSplitter.Create.FromString(data);
    FSplitterPanel := serializer.CreateSplitter(Self, FPluginManager);
    FSplitterPanel.Parent := Self;
    FLastSplitterRight := FSplitterPanel;
    FLastSplitterLeft := FSplitterPanel;
    Resize;
  end;
end;

procedure TfrmStoneNotes.btnSaveClick(Sender: TObject);
var
  serializer: TSerializedSplitter;
  data: string;
begin
  serializer := TSerializedSplitter.Create(FSplitterPanel);
  data := serializer.ToString;

  if saveDlg.Execute then
  begin
    TFile.WriteAllText(saveDlg.FileName, data);
    Self.Caption := 'StoneNotes Matrix - ' + ExtractFileName(saveDlg.FileName);
  end;

end;

procedure TfrmStoneNotes.btnSplitLeftClick(Sender: TObject);
begin
  FLastSplitterLeft := FLastSplitterLeft.SplitSide(TSide.LeftTop);
  FLastSplitterLeft.SetRightControl(nil);
end;

procedure TfrmStoneNotes.btnSplitRightClick(Sender: TObject);
begin
  FLastSplitterRight := FLastSplitterRight.SplitSide(TSide.RightBottom);
  FLastSplitterRight.SetRightControl(nil);
end;

procedure TfrmStoneNotes.FormCreate(Sender: TObject);
var
  PluginCount: integer;
begin
  FPluginManager := TPluginManager.Create;
  PluginCount := FPluginManager.LoadPlugins;
  if PluginCount = 0 then
  begin
    ShowMessage('Warning: no plugins loaded! Please reinstall StoneNotes to restore complete functionality.');
  end else begin
    Logger.Log(Format('Loaded %d plugins.', [PluginCount]));
  end;

  FSplitterPanel := TSplitterPanel.Create(Self, FPluginManager);
  FSplitterPanel.Parent := Self;
  FSplitterPanel.SetLeftControl(nil);
  FSplitterPanel.SetRightControl(nil);
  FLastSplitterRight := FSplitterPanel;
  FLastSplitterLeft := FSplitterPanel;
  Resize;
end;

procedure TfrmStoneNotes.FormDeactivate(Sender: TObject);
begin
  Logger.FlushLogBuffer;
end;

procedure TfrmStoneNotes.FormResize(Sender: TObject);
begin
  btnSplitRight.Position.X := Width - btnSplitRight.Width - 20;
  FSplitterPanel.SetBounds(5, btnSplitRight.Height + 10, Self.Width - 25, Self.Height - 90);
end;

end.

