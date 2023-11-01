﻿unit MainForm;

interface

uses
  {system}
  Windows, System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.IOUtils, System.StrUtils, ShellApi, System.Contnrs, System.JSON,
  System.IniFiles,

  {framework}
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.Edit, FMX.StdCtrls, FMX.Styles,

  {custom FMX compnents}
  FMX.SplitterPanel, FMX.BufferPanel,

  {other custom classes}
  SplitterSerializer, PluginManager, Logger, StyleMaker, StringUtils,
  ServerThread;

type
  TfrmStoneNotes = class(TForm)
    btnSplitRight: TButton;
    btnSplitLeft: TButton;
    btnSave: TButton;
    saveDlg: TSaveDialog;
    openDlg: TOpenDialog;
    btnOpen: TButton;
    btnSaveAs: TButton;
    btnStyle: TButton;
    btnNew: TButton;
    procedure CreateHandle; override;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure btnSplitRightClick(Sender: TObject);
    procedure btnSplitLeftClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure btnSaveAsClick(Sender: TObject);
    procedure btnStyleClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
  private
    FSystemIniFile: TMemIniFile;
    FFilename: string;
    FGlobalLayoutValues: TStringList;
    FSplitterPanel: TSplitterPanel;
    FLastSplitterRight: TSplitterPanel;
    FLastSplitterLeft: TSplitterPanel;
    FPluginManager: TPluginManager;
    FServerThread: TServerThread;
    FWndProcId: TFmxHandle;
    procedure UpdateWindowCaption;
    procedure StartServer;
    procedure StopServer;
    procedure OpenFile(AFilename: string);
  public
    { Public declarations }
    procedure ProcessJSONMessage(const AJSON: string);
  published
    property LastSplitterRight: TSplitterPanel read FLastSplitterRight write FLastSplitterRight;
    property LastSplitterLeft: TSplitterPanel read FLastSplitterLeft write FLastSplitterLeft;
    property SystemIniFile: TMemIniFile read FSystemIniFile;
  end;

var
  frmStoneNotes: TfrmStoneNotes;

implementation

{$R *.fmx}

uses
  FMX.Platform, FMX.Platform.Win, Winapi.Messages, Winapi.CommCtrl,
  uBrowserForm,
  uNameDelphiThreads, uUnloadAllModules, uProcessCleanup,
  uServerMonitor;

procedure TfrmStoneNotes.btnNewClick(Sender: TObject);
var
  s: string;
begin
  s := ParamStr(0);
  ShellExecute(0, 'open', PChar(s), nil, nil, SW_SHOW);
end;

procedure TfrmStoneNotes.btnOpenClick(Sender: TObject);
var
  compressedStyleArray: TStringDynArray;
  AFilename: string;
begin
  if openDlg.Execute then
  begin
    AFilename := openDlg.FileName;
    OpenFile(AFilename);
  end;
end;

procedure TfrmStoneNotes.btnSaveAsClick(Sender: TObject);
begin
  if saveDlg.Execute and ('' <> saveDlg.FileName) then
  begin
    FFilename := saveDlg.FileName;
    btnSaveClick(btnSave);
  end;
end;

procedure TfrmStoneNotes.UpdateWindowCaption;
begin
  if '' <> FFilename then
    Self.Caption := 'StoneNotes Matrix - ' + ExtractFileName(FFilename)
  else
    Self.Caption := 'StoneNotes Matrix';
end;


procedure TfrmStoneNotes.btnSaveClick(Sender: TObject);
var
  serializer: TSerializedSplitter;
  data: string;
begin
  if '' <> FFilename then begin
    serializer := TSerializedSplitter.Create(FSplitterPanel, FGlobalLayoutValues);
    data := serializer.ToString;
    TFile.WriteAllText(FFilename, data);
    UpdateWindowCaption;
    FSystemIniFile.WriteString('files', 'last_opened', FFilename);
    FSystemIniFile.UpdateFile;
  end else begin
    btnSaveAsClick(btnSaveAs);
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

procedure TfrmStoneNotes.btnStyleClick(Sender: TObject);
var
  StyleText: string;
begin
//  FGlobalLayoutValues.Values['Style'] := CompressString(ModifyAndApplyStyleToForm(Self));
  FGlobalLayoutValues.Values['Style'] := ModifyAndApplyStyleToForm(Self);
end;

procedure TfrmStoneNotes.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Logger.Log('Main form closing');
  TBrowserForm.EnumerateOpenForms;
  StopServer;
  FlushLogBuffer;
  Application.Terminate;
  Application.ProcessMessages;
  UnloadAllModules;
  TerminateProcessTree(GetCurrentProcessId);
end;

function StoneNotesSubclassProc(hWnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM;
  uIdSubclass: UINT_PTR; dwRefData: DWORD_PTR): LRESULT; stdcall;
var
  Self: TfrmStoneNotes;
begin
  Self := TfrmStoneNotes(dwRefData);

  case uMsg of
    WM_MOVE: begin
      Self.FSplitterPanel.ForceResize;
    end;
    WM_NCDESTROY:
      RemoveWindowSubclass(hWnd, @StoneNotesSubclassProc, uIdSubclass);
  end;
  Result := DefSubclassProc(hWnd, uMsg, wParam, lParam);
end;

procedure TfrmStoneNotes.CreateHandle;
begin
  inherited;
  SetWindowSubclass(FormToHWND(Self), @StoneNotesSubclassProc, 1, DWORD_PTR(Self));
end;


procedure TfrmStoneNotes.FormCreate(Sender: TObject);
var
  PluginCount: integer;
  ServerMonitor: TServerMonitor;
  InstallDir: string;
  SystemIniFilename: string;
  LastOpenedFilename: string;
  HomeFilename: string;
begin
  {$IFDEF DEBUG}
  SystemIniFilename := 'C:\ProgramData\StoneNotes\StoneNotes_Debug.ini';
  {$ELSE}
  SystemIniFilename := 'C:\ProgramData\StoneNotes\StoneNotes.ini';
  {$ENDIF}
  FSystemIniFile := TMemIniFile.Create(SystemIniFilename);
  FSystemIniFile.AutoSave := true;
  FSystemIniFile.UpdateFile;

  FPluginManager := TPluginManager.Create;
  PluginCount := FPluginManager.LoadPlugins;
  if PluginCount = 0 then
  begin
    ShowMessage('Warning: no plugins loaded! Please reinstall StoneNotes to restore complete functionality.');
  end else begin
    Logger.Log(Format('Loaded %d plugins.', [PluginCount]));
  end;

  FGlobalLayoutValues := TStringList.Create;
  FSplitterPanel := TSplitterPanel.Create(Self, FPluginManager);
  FSplitterPanel.Parent := Self;
  FSplitterPanel.SetLeftControl(nil);
  FSplitterPanel.SetRightControl(nil);
  FLastSplitterRight := FSplitterPanel;
  FLastSplitterLeft := FSplitterPanel;
  Resize;

  StartServer;

  InstallDir := ExtractFileDir(ParamStr(0));
  if not FileExists(InstallDir+'\Services\server.ts') then
    InstallDir := ExtractFileDir(InstallDir);
  if not FileExists(InstallDir+'\Services\server.ts') then
    InstallDir := ExtractFileDir(InstallDir);

  ServerMonitor := TServerMonitor.Create(True);
  ServerMonitor.Directory := InstallDir + '\Services';
  ServerMonitor.Resume;

  HomeFilename := FSystemIniFile.ReadString('files', 'home', '');
  LastOpenedFilename := FSystemIniFile.ReadString('files', 'last_opened', '');

  if '' <> HomeFilename then
    OpenFile(HomeFilename)
  else if '' <> LastOpenedFilename then
    OpenFile(LastOpenedFilename);

end;

procedure TfrmStoneNotes.FormDeactivate(Sender: TObject);
begin
  Logger.FlushLogBuffer;
end;

procedure TfrmStoneNotes.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
var
  I: integer;
begin
  for I := 0 to 9 do
    if (Key = vk1+I) and (ssAlt in Shift) then
      begin
        Key := 0;
        KeyChar := #0;
        TriggerGoButton(1+I);
      end;
end;

procedure TfrmStoneNotes.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  FSplitterPanel.ForceResize;
end;

procedure TfrmStoneNotes.FormResize(Sender: TObject);
begin
  FSplitterPanel.SetBounds(5, btnSplitRight.Height + 10, Self.Width - 25, Self.Height - 90);
end;

procedure TfrmStoneNotes.StartServer;
begin
  FServerThread := TServerThread.Create;
  FServerThread.StartServer;
end;

procedure TfrmStoneNotes.StopServer;
begin
  FServerThread.StopServer;
  FServerThread := nil;
end;



procedure TfrmStoneNotes.ProcessJSONMessage(const AJSON: string);
var
  JSONObject: TJSONObject;
begin
  try
    JSONObject := TJSONObject.ParseJSONValue(AJSON) as TJSONObject;
    if Assigned(JSONObject) then
    begin
      // Handle the JSON object as needed.
      // For example, assuming the JSON contains a 'message' string value:
      ShowMessage('Message from server process via TServerThread: '+JSONObject.GetValue<string>('message'));
    end;
  finally
    JSONObject.Free;
  end;
end;

procedure TfrmStoneNotes.OpenFile(AFilename: string);
var
  data: string;
  serializer: TSerializedSplitter;
  NewSplitterPanel: TSplitterPanel;
  style: string;
  TmpStyleFile: string;
begin
  data := TFile.ReadAllText(AFilename);
  FFilename := AFilename;
  UpdateWindowCaption;
  //if Assigned(FSerializer) then
  //  FSerializer.Free;
  //FSerializer := TSerializedSplitter.Create.FromString(data);
  //FSplitterPanel := FSerializer.CreateSplitter(Self, FPluginManager);
  serializer := TSerializedSplitter.Create.FromString(data);
  if nil = serializer then
  begin
    FFilename := '';
    ShowMessage('Invalid layout file, file is not valid JSON or is not a layout file: ' + AFilename);
    UpdateWindowCaption;
  end
  else
  begin
    NewSplitterPanel := serializer.CreateSplitter(Self, FPluginManager);
    if nil = NewSplitterPanel then
      ShowMessage('Invalid layout file, could not create a splitter from the JSON data: ' + AFilename)
    else
    begin
      FGlobalLayoutValues := serializer.GlobalLayoutValues;
      if nil = FGlobalLayoutValues then
        FGlobalLayoutValues := TStringList.Create;
      FLastSplitterRight := nil;
      FLastSplitterLeft := nil;
      FSplitterPanel.Free;
      FSplitterPanel := nil;
      FSplitterPanel := NewSplitterPanel;
      Self.InsertComponent(FSplitterPanel);
      FSplitterPanel.Parent := Self;
      FLastSplitterRight := FSplitterPanel;
      FLastSplitterLeft := FSplitterPanel;
      if Assigned(FGlobalLayoutValues) then
      begin
        style := FGlobalLayoutValues.Values['Style'];
        if '' <> style then
        begin
          //style := DecompressString(style);
          TmpStyleFile := 'C:\ProgramData\StoneNotes\Style_' + ExtractFileName(FFilename) + '.style';
          TFile.WriteAllText(TmpStyleFile, style);
          TStyleManager.SetStyleFromFile(TmpStyleFile);
        end;
      end;
      Resize;
      FSystemIniFile.WriteString('files', 'last_opened', FFilename);
    end;
  end;
end;


// Needed for subclassing to work?
procedure InitStandardClasses;
var
  ICC: TInitCommonControlsEx;
begin
  ICC.dwSize := SizeOf(TInitCommonControlsEx);
  ICC.dwICC := ICC_STANDARD_CLASSES;
  InitCommonControlsEx(ICC);
end;

initialization
  InitStandardClasses;  // Needed for subclassing to work?

end.

