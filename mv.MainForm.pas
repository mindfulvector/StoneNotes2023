unit mv.MainForm;

interface

uses
  {system}
  Windows, System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.IOUtils, System.StrUtils, ShellApi, System.Contnrs, System.JSON,
  System.IniFiles,

  {framework}
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.Edit, FMX.StdCtrls, FMX.Styles,
  IdContext, IdCustomHTTPServer, IdBaseComponent, IdComponent,
  IdCustomTCPServer, IdHTTPServer, FMX.Layouts, FMX.ListBox,

  {custom FMX compnents}
  mv.SplitterPanel, mv.BufferPanel,

  {other custom classes}
  mv.SplitterSerializer, mv.PluginManager, mv.Logger, mv.FMX.StyleMaker,
  mv.StringUtils, mv.WebService;



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
    HttpPluginServices: TIdHTTPServer;
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
    procedure HttpPluginServicesCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  private
    FSystemIniFile: TMemIniFile;
    FFilename: string;
    FDirty: boolean;
    FGlobalLayoutValues: TStringList;
    FSplitterPanel: TSplitterPanel;
    FLastSplitterRight: TSplitterPanel;
    FLastSplitterLeft: TSplitterPanel;
    FPluginManager: TPluginManager;
    FWndProcId: TFmxHandle;
    procedure UpdateWindowCaption;
//    procedure StartServer;
//    procedure StopServer;
    procedure OpenFile(AFilename: string);
  public
    { Public declarations }
  published
    property LastSplitterRight: TSplitterPanel read FLastSplitterRight write FLastSplitterRight;
    property LastSplitterLeft: TSplitterPanel read FLastSplitterLeft write FLastSplitterLeft;
    property SystemIniFile: TMemIniFile read FSystemIniFile;
    property PluginManager: TPluginManager read FPluginManager;
  end;

var
  frmStoneNotes: TfrmStoneNotes;

implementation

{$R *.fmx}

uses
  FMX.Platform, FMX.Platform.Win, Winapi.Messages, Winapi.CommCtrl,
  mv.WebView.BrowserForm, mv.Process.NameDelphiThreads, mv.Process.UnloadAllModules,
  mv.Process.ProcessCleanup, mv.PluginStorageService;

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

/// <summary>
/// Update state of the window caption with the current filename (if there is
/// one) and dirty flag.
/// </summary>
procedure TfrmStoneNotes.UpdateWindowCaption;
begin
  if '' <> FFilename then
    Self.Caption := 'StoneNotes Matrix - ' + ExtractFileName(FFilename)
  else
    Self.Caption := 'StoneNotes Matrix';

  if FDirty then
   Self.Caption := Self.Caption + ' *UNSAVED*';
end;

/// <summary>
///  If we already have a filename, save the layout to it immediately, otherwise
/// trigger Save As for the user to set a filename.
/// </summary>
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
    FDirty := false;
  end else begin
    btnSaveAsClick(btnSaveAs);
  end;
end;

/// <summary>
/// Create a new buffer on the left side of the window by splitting the last
/// left-hand buffer.
/// </summary>
procedure TfrmStoneNotes.btnSplitLeftClick(Sender: TObject);
begin
  FLastSplitterLeft := FLastSplitterLeft.SplitSide(TSide.LeftTop);
  FLastSplitterLeft.SetRightControl(nil);
end;

/// <summary>
/// Create a new buffer on the right side of the window by splitting the last
/// right-hand buffer.
/// </summary>
procedure TfrmStoneNotes.btnSplitRightClick(Sender: TObject);
begin
  FLastSplitterRight := FLastSplitterRight.SplitSide(TSide.RightBottom);
  FLastSplitterRight.SetRightControl(nil);
end;

/// <summary>
/// Generate a randomized visual style, stored with the layout file when saved.
/// </summary>
procedure TfrmStoneNotes.btnStyleClick(Sender: TObject);
var
  StyleText: string;
begin
  FGlobalLayoutValues.Values['Style'] := ModifyAndApplyStyleToForm(Self);
end;

/// <summary>
/// Cleanup and shutdown the application.
/// </summary>
procedure TfrmStoneNotes.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  mv.Logger.Log('Main form closing');

  // Close all WebView2 forms that are still open
  TBrowserForm.FreeAllOpenForms;

  // Final log file flush
  FlushLogBuffer;

  // These calls are left over from having difficulty getting everything to
  // actually shutdown due to WebView2 leaving stuff open. Possibly we can
  // remove some of this.
  Application.Terminate;
  Application.ProcessMessages;
  UnloadAllModules;
  TerminateProcessTree(GetCurrentProcessId);
end;

/// <summary>
/// We subclass the native form window so we can handle the WM_MOVE message,
/// which isn't exposed by FireMonkey. We need to handle this message to move
/// the WebView2 form windows appropriately when the main form moves, so they
/// appear to be part of it.
/// </summary>
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

/// <summary>
/// This event is fired when FireMonkey creates the native window for this form,
/// at which point we can get a valid HWND which is used as the parent of the
/// WebView2 browser form instances we create for plugins and the B command.
/// </summary>
procedure TfrmStoneNotes.CreateHandle;
begin
  inherited;
  SetWindowSubclass(FormToHWND(Self), @StoneNotesSubclassProc, 1, DWORD_PTR(Self));
end;

/// <summary>
/// Create a new instance. Each instance of the application has exactly one
/// layout file loaded, and may have many plugins and WebView2 instances loaded.
/// </summary>
procedure TfrmStoneNotes.FormCreate(Sender: TObject);
var
  PluginCount: integer;
//  ServerMonitor: TServerMonitor;
//  InstallDir: string;
  SystemIniFilename: string;
  LastOpenedFilename: string;
  HomeFilename: string;
  ProgramDataDir: string;
  HttpBindAttemptCount: integer;
begin
  ProgramDataDir := 'C:\ProgramData\StoneNotes';

  // To preserve my sanity, the debug version uses a different system INI
  // filename than the release version.
  {$IFDEF DEBUG}
  SystemIniFilename := ProgramDataDir + '\StoneNotes_Debug.ini';
  {$ELSE}
  SystemIniFilename := ProgramDataDir + 'StoneNotes.ini';
  {$ENDIF}

  // We need a data directory to store the system INI file, if we can't create
  // one then we aren't likely to be able to do much else useful, report the
  // error and exit in that cae.
  if not DirectoryExists(ProgramDataDir) then
    if not CreateDir(ProgramDataDir) then begin
      ShowMessage('Error: Could not create data directory at ' + ProgramDataDir + '. StoneNotes must shut down.');
      Self.Close;
      Application.Terminate;
    end;

  // The system INI file keeps track of settings that are not specific to a
  // single layout file, such as Home Layout File, Last Open Layout File, any
  // cloud API keys, etc.
  FSystemIniFile := TMemIniFile.Create(SystemIniFilename);
  FSystemIniFile.AutoSave := true;
  FSystemIniFile.UpdateFile;

  // Plugin manager handles interaction with the plugin file system for Delphi
  // in-process stuff.
  FPluginManager := TPluginManager.Create;

  // Scan the plugin directory, create an instance for each discovered plugin.
  PluginCount := FPluginManager.LoadPlugins;

  // There should always be at least one plugin, otherwise we probably scanned
  // the wrong directory... or something erased all the plugins, inform the user
  // about this if that happens.
  if PluginCount = 0 then
  begin
    ShowMessage('Warning: no plugins loaded! Please reinstall StoneNotes to restore complete functionality.');
  end else begin
    mv.Logger.Log(Format('Loaded %d plugins.', [PluginCount]));
  end;

  // Layout values that are shared between plugin instances are in this list
  FGlobalLayoutValues := TStringList.Create;

  // Root level SplitterPanel that never goes away, this contains all other
  // BufferPanel and SplitterPanel instances through the lifetime of this
  // form instance/application instance.
  FSplitterPanel := TSplitterPanel.Create(Self, FPluginManager, HttpPluginServices.DefaultPort);
  FSplitterPanel.Parent := Self;
  FSplitterPanel.SetLeftControl(nil);
  FSplitterPanel.SetRightControl(nil);

  // These splitter variables are the ones where the next split happens, so
  // initially either kind of split happens in the root level splitter.
  FLastSplitterRight := FSplitterPanel;
  FLastSplitterLeft := FSplitterPanel;

  // Resize the root level SplitterPanel and BufferPanels.
  Resize;

  // We have a "home" file setting which is like a home stack in HyperCard,
  // and we know the last file that we had opened. If the last file opened
  // is not available, fall back to the home layout file. Actually, we'd prefer
  // to be able to open both of them but due to the single file/single instance
  // nature of the application currently that isn't practical without some sort
  // of IPC so that we know which might already have been opened by another
  // instance... solvable though.
  HomeFilename := FSystemIniFile.ReadString('files', 'home', '');
  LastOpenedFilename := FSystemIniFile.ReadString('files', 'last_opened', '');

  if '' <> HomeFilename then
    OpenFile(HomeFilename)
  else if '' <> LastOpenedFilename then
    OpenFile(LastOpenedFilename);

  // Attempt to start the HttpPluginService server, first with the deafult
  // port then with random ports until we find one we can bind to.
  HttpBindAttemptCount := 0;
  while not HttpPluginServices.Active do
    try
      HttpPluginServices.Active := true;
      if HttpPluginServices.Active then
        mv.Logger.Log(Format('Bound HTTP services to port %d', [HttpPluginServices.DefaultPort]));
    except
      Inc(HttpBindAttemptCount);
      if HttpBindAttemptCount > 1000 then
      begin
        mv.Logger.Log(Format('Cannot bind to port %d, going to give up.', [HttpPluginServices.DefaultPort]));
        ShowMessage(Format('We could not bind HTTP services to a port after %d attempts, so giving up. StoneNotes must shut down.', [HttpBindAttemptCount]));
        Self.Close;
        Exit;
      end;
      mv.Logger.Log(Format('Cannot bind to port %d, trying another one...', [HttpPluginServices.DefaultPort]));
      if HttpPluginServices.DefaultPort = 65535 then
        HttpPluginServices.DefaultPort := 2000
      else
        HttpPluginServices.DefaultPort := HttpPluginServices.DefaultPort + 1;
      HttpPluginServices.Bindings[0].Port := HttpPluginServices.DefaultPort;
    end;
end;

/// <summary>
/// Form is not in focus anymore, sync things as best we can. For now at least
/// make sure the log file is written out.
/// </summary>
procedure TfrmStoneNotes.FormDeactivate(Sender: TObject);
begin
  mv.Logger.FlushLogBuffer;
end;

/// <summary>
/// This should catch Alt+1..0 shortcut keys and send to the appropriate Go
/// button but it only seems to work for half of them?
/// </summary>
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

/// <summary>
/// For a resize event, sort of fixes some small issues with WebView2 overlay
/// windows though really not an ideal solution. Need to preven those windows
/// from being resized on their own, in which case we shouldn't need this any
/// more.
/// </summary>
procedure TfrmStoneNotes.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  FSplitterPanel.ForceResize;
end;

/// <summary>
/// Inform splitter and buffer tree of new size.
/// </summary>
procedure TfrmStoneNotes.FormResize(Sender: TObject);
begin
  FSplitterPanel.SetBounds(5, btnSplitRight.Height + 10, Self.Width - 25, Self.Height - 90);
end;

/// <summary>
/// Handle an HTTP GET request on the main web server port. This is the
/// interface for the WebView2 instances to fetch plugin resources and call
/// plugin services such as PluginStorageServer.
/// </summary>
procedure TfrmStoneNotes.HttpPluginServicesCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  Path, Query: string;
  Controller, Action: string;
  Res: string;
label
  Error404, CheckRes, Done;
  // This reduces the complexity of adding a route to just adding one line
  // of code to the main CommandGet handler below, similar to a Python/PHP
  // framework.
  procedure _Route(const CheckPath, CheckPattern: string);
  begin
    // Already sent a response? Skip checking more patterns.
    if '' <> Res then Exit;

    // This is the CommandGet helper, so the method is always GET here
    Res := WebRoute('GET', CheckPath, Query, CheckPattern);
    if '' <> Res then
    begin
      mv.Logger.Log('WebRequest:Matched> ' + CheckPattern);
      AResponseInfo.ResponseNo := 200;
      if Path.EndsWith('.css') then
        AResponseInfo.ContentType := 'text/css';
      if Path.EndsWith('.js') then
        AResponseInfo.ContentType := 'text/javascript';
      if Path.EndsWith('.json') then
        AResponseInfo.ContentType := 'application/json';
      if Path.EndsWith('.png') then
        AResponseInfo.ContentType := 'image/png';
      if Path.EndsWith('.gif') then
        AResponseInfo.ContentType := 'image/gif';
      if Path.EndsWith('.jpg') then
        AResponseInfo.ContentType := 'image/jpeg';
      if Path.EndsWith('.svg') then
        AResponseInfo.ContentType := 'image/svg+xml';
      if Path.EndsWith('.pdf') then
        AResponseInfo.ContentType := 'application/pdf';
      if Path.EndsWith('.txt') then
        AResponseInfo.ContentType := 'text/plain';
      if Path.EndsWith('.woff') then
        AResponseInfo.ContentType := 'font/woff';
      if Path.EndsWith('.woff2') then
        AResponseInfo.ContentType := 'font/woff2';
      if Path.EndsWith('.otf') then
        AResponseInfo.ContentType := 'font/otf';
      if Path.EndsWith('.tif') then
        AResponseInfo.ContentType := 'image/tiff';
      if Path.EndsWith('.tiff') then
        AResponseInfo.ContentType := 'image/tiff';
      if Path.EndsWith('.ppt') then
        AResponseInfo.ContentType := 'application/vnd.ms-powerpoint';
      if Path.EndsWith('.pptx') then
        AResponseInfo.ContentType := 'application/vnd.openxmlformats-officedocument.presentationml.presentation';
      if Path.EndsWith('.epub') then
        AResponseInfo.ContentType := 'application/epub+zip';
      if Path.EndsWith('.doc') then
        AResponseInfo.ContentType := 'application/msword';
      if Path.EndsWith('.docx') then
        AResponseInfo.ContentType := 'application/vnd.openxmlformats-officedocument.wordprocessingml.document';
      if Path.EndsWith('.csv') then
        AResponseInfo.ContentType := 'text/csv';
      AResponseInfo.ContentText := Res;
      AResponseInfo.WriteContent;
    end;
  end;
begin
  Res := '';
  Path := ARequestInfo.Document;
  Query := ARequestInfo.QueryParams;

  mv.Logger.Log('WebRequest:Path>        '+Path);
  mv.Logger.Log('WebRequest:Query>       '+Query);

  // Be sure to call the _Route helper, not WebRoute directly!
  // Less specific routes (such as / in particular) MUST be after more specific
  // routes, otherwise they will intercept the more specific route! A route will
  // accept anything added to the end of it if no other route has accepted the
  // request already, in order to support parameters, etc.
  _Route(Path, 'GET /plugin/ TPluginController->PluginResource');
  _Route(Path, 'GET /assets/ TPluginController->AssetResource');
  _Route(Path, 'GET /Service/PluginStorageService/ReadLayoutValue TPluginController->ReadLayoutValue');

  if '' = Res then begin  
    AResponseInfo.ResponseNo := 404;
    AResponseInfo.ContentText := '404 Not Found';
    mv.Logger.Log('WebRequest:NoMatch>   ' + AResponseInfo.ContentText);
    AResponseInfo.WriteContent;
  end;
end;

/// <summary>
/// Open a layout file.
/// </summary>
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
  FPluginManager.ActiveLayoutFilename := FFilename;
  FDirty := false;
  UpdateWindowCaption;
  FSplitterPanel.Free;
  FSplitterPanel := nil;
  serializer := TSerializedSplitter.Create.FromString(data);
  if nil = serializer then
  begin
    FFilename := '';
    FPluginManager.ActiveLayoutFilename := FFilename;
    ShowMessage('Invalid layout file, file is not valid JSON or is not a layout file: ' + AFilename);
    UpdateWindowCaption;
  end
  else
  begin
    NewSplitterPanel := serializer.CreateSplitter(Self, FPluginManager, HttpPluginServices.DefaultPort);
    if nil = NewSplitterPanel then
      ShowMessage('Invalid layout file, could not create a splitter from the JSON data: ' + AFilename)
    else
    begin
      FGlobalLayoutValues := serializer.GlobalLayoutValues;
      if nil = FGlobalLayoutValues then
        FGlobalLayoutValues := TStringList.Create;
      FLastSplitterRight := nil;
      FLastSplitterLeft := nil;
      FSplitterPanel := NewSplitterPanel;
      Self.InsertComponent(FSplitterPanel);
      FSplitterPanel.Parent := Self;
      FLastSplitterRight := FSplitterPanel;
      FLastSplitterLeft := FSplitterPanel;
      if Assigned(FGlobalLayoutValues) then
      begin
        // If there is a custom style, save it to a temporary .style file with
        // the name of this layout file in it, then apply from that file
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



/// <summary>
/// We need to subclass the native form window in order to handle the WM_MOVE
/// message. This enables subclassing to work.
/// </summary>
procedure InitStandardClasses;
var
  ICC: TInitCommonControlsEx;
begin
  ICC.dwSize := SizeOf(TInitCommonControlsEx);
  ICC.dwICC := ICC_STANDARD_CLASSES;
  InitCommonControlsEx(ICC);
end;

initialization

// Needed for subclassing to work
InitStandardClasses;

end.

