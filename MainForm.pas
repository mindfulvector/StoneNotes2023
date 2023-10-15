unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Graphics, FMX.Dialogs, FMX.SplitterPanel, FMX.Controls.Presentation,
  FMX.Edit, FMX.StdCtrls, FMX.TMSFNCTypes, FMX.TMSFNCUtils, FMX.TMSFNCGraphics,
  FMX.TMSFNCGraphicsTypes, FMX.TMSFNCCustomControl, FMX.TMSFNCWebBrowser, SplitterSerializer,
  System.IOUtils;

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
  private
    FSplitterPanel: TSplitterPanel;
    FLastSplitterRight: TSplitterPanel;
    FLastSplitterLeft: TSplitterPanel;
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
    Self.Caption := openDlg.FileName;
    FLastSplitterRight := nil;
    FLastSplitterLeft := nil;
    FSplitterPanel.DisposeOf;
    FSplitterPanel := nil;
    serializer := TSerializedSplitter.Create.FromString(data);
    FSplitterPanel := serializer.CreateSplitter;
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
    Self.Caption := saveDlg.FileName;
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
begin
  FSplitterPanel := TSplitterPanel.Create(Self);
  FSplitterPanel.Parent := Self;
  FSplitterPanel.SetLeftControl(nil);
  FSplitterPanel.SetRightControl(nil);
  FLastSplitterRight := FSplitterPanel;
  FLastSplitterLeft := FSplitterPanel;
  Resize;
end;

procedure TfrmStoneNotes.FormResize(Sender: TObject);
begin
  btnSplitRight.Position.X := Width - btnSplitRight.Width - 20;
  FSplitterPanel.SetBounds(5, btnSplitRight.Height + 10, Self.Width - 25, Self.Height - 90);
end;

end.

