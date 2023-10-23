unit UpdateSystem;

interface

uses
  System.SysUtils, // For basic types, exceptions, and utility functions
  System.Classes,  // For TStringList and other basic classes
  System.IniFiles, // For TIniFile class to read .ini files
  System.Zip,      // For working with ZIP files (unzipping)
  Vcl.Forms,       // For TForm class
  Vcl.Controls,    // For basic control definitions
  Vcl.ComCtrls,    // For TTreeView class
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Graphics,    // For TBitmap and drawing capabilities
  Vcl.ImgList,     // For TImageList
  Vcl.Dialogs,     // For ShowMessage and MessageDlg
  IdComponent,     // For TIdProgressEvent and other Indy component basics
  IdHTTP;          // For handling HTTP-based downloads

type

  { UpdateSystem Types }
  TPackageCategory = (pcRequired, pcRecommended, pcOptional);

  TSupportPackage = record
    Name: string;
    Category: TPackageCategory;
    URL: string;
    Selected: Boolean;
  end;

  TUpdateInfo = record
    Major: Integer;
    Minor: Integer;
    Patch: Integer;
    Build: Integer;
    MainURL: string;
    MainUpdateSelected: boolean;
    SupportPackages: array of TSupportPackage;
  end;

  { TNotifyEventWrapper }
  TAnonymousNotifyEvent = reference to procedure(Sender: TObject);

  TNotifyEventWrapper = record
  private
    FEvent: TAnonymousNotifyEvent;
  public
    constructor Create(const Value: TAnonymousNotifyEvent);
    procedure Handler(Sender: TObject);
    property EventProcedure: TAnonymousNotifyEvent read FEvent write FEvent;
  end;

  { TWorkEventWrapper }
  TAnonymousWorkEvent = reference to procedure(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);

  TWorkEventWrapper = record
  private
    FEvent: TAnonymousWorkEvent;
  public
    constructor Create(const Value: TAnonymousWorkEvent);
    procedure Handler(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
    property EventProcedure: TAnonymousWorkEvent read FEvent write FEvent;
  end;



  { UpdateSystem Functions }
  function ParseVersionInfo(const APath: string): TUpdateInfo;
  function ShowUpdateForm(AUpdateInfo: TUpdateInfo): TUpdateInfo;
  function DownloadAndUnzip(const AURL, ADestination: string): boolean;

const
  SN_UPDATER_NONE = 0;
  SN_UPDATER_UNCHECKED = 1;
  SN_UPDATER_CHECKED = 2;
  SN_CONTACT_US = #10#13#10#13'Please contact us on the support forum: meta.mn/forum';

implementation

uses
  TreeNodeCheckboxClick;

{ UpdateSystem Functions }

function ParseVersionInfo(const APath: string): TUpdateInfo;
var
  Strings: TStringList;
  Ini: TMemIniFile;
  SupportPackageCount, I: Integer;
  PackageCategory: string;
begin
  if not FileExists(APath) then
  begin
    ShowMessage('StoneNotes Updater Error: '+APath+' does not exist.'+SN_CONTACT_US);
    Exit;
  end else begin
    Strings := TStringList.Create;
    Strings.LoadFromFile(APath);
    if Strings.Count < 9 then
    begin
      ShowMessage('StoneNotes Updater Error: '+APath+' appears to be too short, found ' + IntToStr(Strings.Count) + ' lines but expected at least 9 lines.'+SN_CONTACT_US);
      Exit;
    end;

    Ini := TMemIniFile.Create(APath);
    Ini.SetStrings(Strings);
  //try
    Result.MainURL := Ini.ReadString('MainPackage', 'URL', '');
    Result.Major := Ini.ReadInteger('Version', 'Major', 0);
    Result.Minor := Ini.ReadInteger('Version', 'Minor', 0);
    Result.Patch := Ini.ReadInteger('Version', 'Patch', 0);
    Result.Build := Ini.ReadInteger('Version', 'Build', 0);

    SupportPackageCount := Ini.ReadInteger('SupportPackages', 'Count', 0);
    SetLength(Result.SupportPackages, SupportPackageCount);

    for I := 0 to SupportPackageCount - 1 do begin
      Result.SupportPackages[I].Name := Ini.ReadString('SupportPackage' + IntToStr(I + 1), 'Name', '');

      PackageCategory := Ini.ReadString('SupportPackage' + IntToStr(I + 1), 'Category', '');
      if PackageCategory = 'Required' then
        Result.SupportPackages[I].Category := pcRequired
      else if PackageCategory = 'Recommended' then
        Result.SupportPackages[I].Category := pcRecommended
      else
        Result.SupportPackages[I].Category := pcOptional;

      Result.SupportPackages[I].URL := Ini.ReadString('SupportPackage' + IntToStr(I + 1), 'URL', '');
    end;
  //except on E:Exception do
  //  ShowMessage('StoneNotes Updater Error: ' + E.Message + +SN_CONTACT_US);
  //end;
    Ini.Free;
  end;
end;

procedure AddCheckboxesToImageList(AImageList: TImageList);
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.Width := 16;
    Bitmap.Height := 16;

    // No box: index 0 == SN_UPDATER_NONE
    Bitmap.Canvas.Brush.Color := clWhite;
    Bitmap.Canvas.FillRect(Rect(0, 0, 16, 16));
    AImageList.AddMasked(Bitmap, clWhite);

    // Unchecked box: index 1 == SN_UPDATER_UNCHECKED
    Bitmap.Canvas.Brush.Color := clWhite;
    Bitmap.Canvas.FillRect(Rect(0, 0, 16, 16));
    Bitmap.Canvas.Pen.Color := clBlack;
    Bitmap.Canvas.Rectangle(2, 2, 14, 14);
    AImageList.AddMasked(Bitmap, clWhite);

    // Checked box: index 2 == SN_UPDATER_UNCHECKED
    Bitmap.Canvas.FillRect(Rect(0, 0, 16, 16));
    Bitmap.Canvas.Rectangle(2, 2, 14, 14);
    Bitmap.Canvas.Pen.Color := clBlack;
    Bitmap.Canvas.MoveTo(4, 8);
    Bitmap.Canvas.LineTo(7, 11);
    Bitmap.Canvas.LineTo(12, 4);
    AImageList.AddMasked(Bitmap, clWhite);
  finally
    Bitmap.Free;
  end;
end;

procedure TreeNodeCheckboxClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Tree: TTreeView;
  Node: TTreeNode;
begin
  Tree := Sender as TTreeView;

  Node := Tree.GetNodeAt(X, Y);
  if Assigned(Node) then
  begin
    if Node.Text.Contains('(Required)') then
      Node.StateIndex := SN_UPDATER_CHECKED
    else begin
      if Node.Text.Contains('(Recommended)') then
      begin
        if Node.StateIndex = SN_UPDATER_UNCHECKED then
          Node.StateIndex := SN_UPDATER_CHECKED
        else begin
          if MessageDlg('Are you sure you want to exclude this recommended update? '
              +'StoneNotes may not function properly if you do not manually '
              +'install an alternative module.',
              TMsgDlgType.mtWarning, mbYesNo, 0) = mrYes then
            Node.StateIndex := SN_UPDATER_UNCHECKED;
        end;
      end else begin
        if Node.StateIndex = SN_UPDATER_UNCHECKED then
          Node.StateIndex := SN_UPDATER_CHECKED
        else
          Node.StateIndex := SN_UPDATER_UNCHECKED;
      end;
    end;
  end;
end;

var
  UpdateForm: TForm;
  UpdateFormResult: boolean;

procedure OkButtonClicked(Sender: TObject);
begin
  UpdateFormResult := true;
  UpdateForm.Close;
end;

procedure CancelButtonClicked(Sender: TObject);
begin
  UpdateFormResult := false;
  UpdateForm.Close;
end;

function ShowUpdateForm(AUpdateInfo: TUpdateInfo): TUpdateInfo;
var
  Instructions: TLabel;
  Tree: TTreeView;
  MainNode, ChildNode: TTreeNode;
  I: Integer;
  ImageList: TImageList;
  OkButton: TButton;
  CancelButton: TButton;
  ButtonPanel: TPanel;
begin
  UpdateForm := TForm.Create(nil);
  try
    UpdateForm.Caption := 'Available Updates';
    UpdateForm.Position := poScreenCenter;
    UpdateForm.Width := 400;
    UpdateForm.Height := 300;
    //UpdateForm.Margins.SetControlBounds(10,10, UpdateForm.Width-20, UpdateForm.Height-20);
    UpdateForm.Padding.SetBounds(10,10, 10,10);

    ImageList := TImageList.Create(UpdateForm);
    ImageList.Width := 16;
    ImageList.Height := 16;
    // Create image list in code so we don't need to bring along any resources
    AddCheckboxesToImageList(ImageList);

    Instructions := TLabel.Create(UpdateForm);
    Instructions.Parent := UpdateForm;
    Instructions.Align := alTop;
    Instructions.Caption := 'The following updates are available for StoneNotes. Select the ones you wish to apply then click Continue.';
    Instructions.SetBounds(10,10,UpdateForm.Width, 120);
    Instructions.WordWrap := true;

    Tree := TTreeView.Create(UpdateForm);
    Tree.Parent := UpdateForm;
    Tree.Align := alClient;
    Tree.ReadOnly := True; // So that only checkboxes can be toggled
    Tree.StateImages := ImageList;
    Tree.OnMouseDown := TTreeNodeCheckboxClickWrapper.Create(TreeNodeCheckboxClick).Handler;

    ButtonPanel := TPanel.Create(UpdateForm);
    ButtonPanel.Parent := UpdateForm;
    ButtonPanel.SetBounds(0,0,50,50);
    ButtonPanel.Align := alBottom;
    ButtonPanel.BevelInner := bvNone;
    ButtonPanel.BevelOuter := bvNone;
    //ButtonPanel.BorderStyle := bsNone;
    ButtonPanel.Padding.SetBounds(80, 10, 80, 10);

    OkButton := TButton.Create(UpdateForm);
    OkButton.Parent := ButtonPanel;
    OkButton.SetBounds(0,0,80,25);
    OkButton.Align := alLeft;
    OkButton.Caption := 'Continue';
    OkButton.OnClick := TNotifyEventWrapper.Create(OkButtonClicked).Handler;

    CancelButton := TButton.Create(UpdateForm);
    CancelButton.Parent := ButtonPanel;
    CancelButton.SetBounds(0,0,60,25);
    CancelButton.Align := alRight;
    CancelButton.Caption := 'Cancel';
    CancelButton.OnClick := TNotifyEventWrapper.Create(CancelButtonClicked).Handler;

    MainNode := Tree.Items.Add(nil, 'Main Update (Required)');
    MainNode.StateIndex := SN_UPDATER_CHECKED;
    MainNode.SelectedIndex := 1;

    for I := 0 to High(AUpdateInfo.SupportPackages) do begin
      ChildNode := Tree.Items.AddChild(MainNode, AUpdateInfo.SupportPackages[I].Name);

      // This sets the checkbox based on the category of the support package
      if AUpdateInfo.SupportPackages[I].Category in [pcRequired, pcRecommended] then
      begin
        if pcRequired = AUpdateInfo.SupportPackages[I].Category then
          ChildNode.Text := ChildNode.Text + ' (Required)'
        else
          ChildNode.Text := ChildNode.Text + ' (Recommended)';
        ChildNode.StateIndex := SN_UPDATER_CHECKED;
      end else begin
          ChildNode.Text := ChildNode.Text + ' (Optional)';
        ChildNode.StateIndex := SN_UPDATER_UNCHECKED;
      end;

      AUpdateInfo.SupportPackages[I].Selected := ChildNode.StateIndex = SN_UPDATER_CHECKED;
    end;

    MainNode.Expand(True);

    UpdateForm.ShowModal;

    // Update selected state for each package to pass on to next step
    AUpdateInfo.MainUpdateSelected := UpdateFormResult;
    for I := 0 to High(AUpdateInfo.SupportPackages) do begin
      AUpdateInfo.SupportPackages[I].Selected := UpdateFormResult and (Tree.Items.Item[I+1].StateIndex = SN_UPDATER_CHECKED);
    end;

    Result := AUpdateInfo;
  except on E: Exception do
    ShowMessage('StoneNotes Updater Unexpected Error: ' + E.Message + SN_CONTACT_US);
  end;

  FreeAndNil(UpdateForm);
end;


var
  DownloadHTTP: TIdHTTP;
  DownloadProgressBar: TProgressBar;

procedure HTTPWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
begin
  if DownloadHTTP.Response.ContentLength > 0 then
    DownloadProgressBar.Position := (AWorkCount * 100) div DownloadHTTP.Response.ContentLength;
  Application.ProcessMessages; // Keep the application responsive
end;

function DownloadAndUnzip(const AURL, ADestination: string): boolean;
var
  MemStream: TMemoryStream;
  ZipFile: TZipFile;
  Form: TForm;
  StatusLabel: TLabel;
begin
  Result := false;

  ZipFile := nil;
  MemStream := nil;
  DownloadHTTP := nil;
  Form := TForm.Create(nil);

  Form.Position := poScreenCenter;
  Form.Caption := 'Downloading Package...';
  Form.Width := 350;
  Form.Height := 150;
  Form.BorderStyle := bsDialog;

  StatusLabel := TLabel.Create(Form);
  StatusLabel.Parent := Form;
  StatusLabel.Align := alTop;
  StatusLabel.Caption := 'Downloading: ' + ExtractFileName(AURL);

  DownloadProgressBar := TProgressBar.Create(Form);
  DownloadProgressBar.Parent := Form;
  DownloadProgressBar.Align := alClient;
  DownloadProgressBar.Min := 0;
  DownloadProgressBar.Max := 100;

  Form.Show;
  Application.ProcessMessages;

  DownloadHTTP := TIdHTTP.Create;
  MemStream := TMemoryStream.Create;

  try
    DownloadHTTP.OnWork := TWorkEventWrapper.Create(HTTPWork).Handler;
    DownloadHTTP.Get(AURL, MemStream);
    MemStream.Position := 0;

    ZipFile := TZipFile.Create;
    ZipFile.Open(MemStream, zmRead);
    ZipFile.ExtractAll(ADestination);

    Result := true;
  except on E: Exception do
    ShowMessage('StoneNotes Updater Unexpected Download Error'+#13#10#13#10
      +'Package: '+AURL+#13#10#13#10
      +'Error: '+E.Message+#13#10#13#10
      +SN_CONTACT_US);
  end;

  FreeAndNil(ZipFile);
  FreeAndNil(MemStream);
  FreeAndNil(DownloadHTTP);
  FreeAndNil(Form);
end;

{ TNotifyEventWrapper }


constructor TNotifyEventWrapper.Create(const Value: TAnonymousNotifyEvent);
begin
  Self.EventProcedure := Value;
end;

procedure TNotifyEventWrapper.Handler(Sender: TObject);
begin
  if Assigned(FEvent) then
    FEvent(Sender);
end;

{ TWorkEventWrapper }
constructor TWorkEventWrapper.Create(const Value: TAnonymousWorkEvent);
begin
  Self.EventProcedure := Value;
end;

procedure TWorkEventWrapper.Handler(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
begin
  if Assigned(FEvent) then
    FEvent(ASender, AWorkMode, AWorkCount);
end;


end.
