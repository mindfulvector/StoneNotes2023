unit UBrowserForm;
interface
uses
  Winapi.Windows, System.SysUtils, System.Types, System.UITypes,
  System.Generics.Collections,
  System.Classes, System.Variants, Winapi.Messages,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation, FMX.Layouts,
  uWVLoader, uWVBrowserBase, uWVFMXBrowser, uWVFMXWindowParent, uWVTypeLibrary,
  uWVTypes, PluginStorageService, System.Win.ComObj;
const
  WEBVIEW2_SHOWBROWSER = WM_APP + $101;
type
  TBrowserForm = class(TForm)
    AddressEdt: TEdit;
    GoBtn: TButton;
    WVFMXBrowser1: TWVFMXBrowser;
    Timer1: TTimer;
    BrowserLay: TLayout;
    AddressLay: TLayout;
    FocusWorkaroundBtn: TButton;
    StatusBar1: TStatusBar;
    StatusLbl: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure GoBtnClick(Sender: TObject);
    procedure WVFMXBrowser1AfterCreated(Sender: TObject);
    procedure WVFMXBrowser1InitializationError(Sender: TObject; aErrorCode: HRESULT; const aErrorMessage: wvstring);
    procedure WVFMXBrowser1GotFocus(Sender: TObject);
    procedure WVFMXBrowser1StatusBarTextChanged(Sender: TObject; const aWebView: ICoreWebView2);
  private
    FMXWindowParent         : TWVFMXWindowParent;
    FCustomWindowState      : TWindowState;
    FOldWndPrc              : TFNWndProc;
    FFormStub               : Pointer;
    FRequestedURL: string;
    procedure LoadURL;
    procedure ResizeChild;
    procedure CreateFMXWindowParent;
    function  GetFMXWindowParentRect : System.Types.TRect;
    function  PostCustomMessage(aMsg : cardinal; aWParam : WPARAM = 0; aLParam : LPARAM = 0) : boolean;
    function  GetCurrentWindowState : TWindowState;
    procedure UpdateCustomWindowState;
    procedure CustomWndProc(var aMessage: TMessage);
    procedure SetRequestedURL(AURL: string);
  public
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
    procedure NotifyMoveOrResizeStarted;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
    class procedure EnumerateOpenForms;
    property RequestedURL: string read FRequestedURL write SetRequestedURL;
  end;

var
  BrowserForms: TObjectList<TBrowserForm>;

implementation
{$R *.fmx}
// This is a simple browser in normal mode in a Firemonkey application.
// This demo uses a TWVFMXBrowser and a TWVFMXWindowParent. It replaces the original WndProc with a
// custom CustomWndProc procedure to handle Windows messages.
uses
  FMX.Platform, FMX.Platform.Win;
procedure TBrowserForm.FormCreate(Sender: TObject);
begin
  FMXWindowParent    := nil;
  FCustomWindowState := WindowState;
  if nil = BrowserForms then
  begin
    BrowserForms := TObjectList<TBrowserForm>.Create;
    BrowserForms.OwnsObjects := false;
  end;
  BrowserForms.Add(Self);
  Self.AddressEdt.Text := FRequestedURL;
end;
procedure TBrowserForm.FormResize(Sender: TObject);
begin
  ResizeChild;
end;
procedure TBrowserForm.FormShow(Sender: TObject);
var
  TempHandle : HWND;
begin
  // TFMXWindowParent has to be created at runtime
  CreateFMXWindowParent;
  // You *MUST* call CreateBrowser to create and initialize the browser.
  // This will trigger the AfterCreated event when the browser is fully
  // initialized and ready to receive commands.
  if GlobalWebView2Loader.InitializationError then
    showmessage(GlobalWebView2Loader.ErrorMessage)
   else
    if GlobalWebView2Loader.Initialized then
      begin
        TempHandle               := FmxHandleToHWND(FMXWindowParent.Handle);
        WVFMXBrowser1.DefaultUrl := AddressEdt.Text;
        if not(WVFMXBrowser1.CreateBrowser(TempHandle)) then
          Timer1.Enabled := True;
      end;
end;
function TBrowserForm.PostCustomMessage(aMsg : cardinal; aWParam : WPARAM; aLParam : LPARAM) : boolean;
var
  TempHWND : HWND;
begin
  TempHWND := FmxHandleToHWND(Handle);
  Result   := (TempHWND <> 0) and WinApi.Windows.PostMessage(TempHWND, aMsg, aWParam, aLParam);
end;
procedure TBrowserForm.GoBtnClick(Sender: TObject);
begin
  LoadURL;
end;
procedure TBrowserForm.ResizeChild;
begin
  if (FMXWindowParent <> nil) then
    begin
      FMXWindowParent.SetBounds(GetFMXWindowParentRect);
      FMXWindowParent.UpdateSize;
    end;
end;

procedure TBrowserForm.CreateFMXWindowParent;
begin
  if (FMXWindowParent = nil) then
    begin
      FMXWindowParent         := TWVFMXWindowParent.CreateNew(nil);
      FMXWindowParent.Browser := WVFMXBrowser1;
      FMXWindowParent.Reparent(Handle);
      ResizeChild;
      FMXWindowParent.Show;
    end;
end;
function TBrowserForm.GetFMXWindowParentRect : System.Types.TRect;
begin
  Result.Left   := round(BrowserLay.Position.x);
  Result.Top    := round(BrowserLay.Position.y);
  Result.Right  := round(Result.Left + BrowserLay.Width);
  Result.Bottom := round(Result.Top  + BrowserLay.Height);
end;
procedure TBrowserForm.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  PositionChanged: Boolean;
begin
  PositionChanged := (ALeft <> Left) or (ATop <> Top);
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  if PositionChanged then
    NotifyMoveOrResizeStarted;
end;
procedure TBrowserForm.SetRequestedURL(AURL: string);
begin
  FRequestedURL := AURL;
  if Assigned(Self.AddressEdt) then
    Self.AddressEdt.Text := AURL;
  //if not Timer1.Enabled then
  //  WVFMXBrowser1.Navigate(Self.AddressEdt.Text);
end;

procedure TBrowserForm.NotifyMoveOrResizeStarted;
begin
  if (WVFMXBrowser1 <> nil) then WVFMXBrowser1.NotifyParentWindowPositionChanged;
end;
procedure TBrowserForm.Timer1Timer(Sender: TObject);
var
  TempHandle : HWND;
begin
  Timer1.Enabled  := False;
  TempHandle      := FmxHandleToHWND(FMXWindowParent.Handle);
  if not(WVFMXBrowser1.CreateBrowser(TempHandle)) then
    Timer1.Enabled := True;
  //if not Timer1.Enabled then
  //  WVFMXBrowser1.Navigate(Self.AddressEdt.Text);
end;
procedure TBrowserForm.LoadURL;
var
  StorageService: OleVariant;
begin
  StorageService := TPluginStorageService.CreateInstance('B');
  WVFMXBrowser1.AddHostObjectToScript('PluginStorageService', StorageService);
  WVFMXBrowser1.Navigate(AddressEdt.Text);
end;
procedure TBrowserForm.CreateHandle;
begin
  inherited CreateHandle;
  FFormStub  := MakeObjectInstance(CustomWndProc);
  FOldWndPrc := TFNWndProc(SetWindowLongPtr(FmxHandleToHWND(Handle), GWLP_WNDPROC, NativeInt(FFormStub)));
end;
procedure TBrowserForm.DestroyHandle;
begin
  if nil <> BrowserForms then
    BrowserForms.Remove(Self);
  SetWindowLongPtr(FmxHandleToHWND(Handle), GWLP_WNDPROC, NativeInt(FOldWndPrc));
  FreeObjectInstance(FFormStub);
  inherited DestroyHandle;
end;
class procedure TBrowserForm.EnumerateOpenForms;
var
  I: integer;
begin
  if nil <> BrowserForms then
  begin
    while BrowserForms.Count > 0 do
    begin
   //   ShowMessage(BrowserForms[0].Caption);
      BrowserForms[0].Parent := nil;
      BrowserForms[0].SetParentComponent(nil);
      BrowserForms[0].Free; // The form removes itself from the list in DestroyHandle above
    end;
    FreeAndNil(BrowserForms);
  end;
end;

procedure TBrowserForm.CustomWndProc(var aMessage: TMessage);
const
  SWP_STATECHANGED = $8000;  // Undocumented
var
  TempWindowPos : PWindowPos;
begin
  try
    case aMessage.Msg of
      WM_MOVE,
      WM_MOVING : NotifyMoveOrResizeStarted;
      WM_SIZE :
        if (aMessage.wParam = SIZE_RESTORED) then
          UpdateCustomWindowState;
      WM_WINDOWPOSCHANGING :
        begin
          TempWindowPos := TWMWindowPosChanging(aMessage).WindowPos;
          if ((TempWindowPos.Flags and SWP_STATECHANGED) <> 0) then
            UpdateCustomWindowState;
        end;
      WM_SHOWWINDOW :
        if (aMessage.wParam <> 0) and (aMessage.lParam = SW_PARENTOPENING) then
          PostCustomMessage(WEBVIEW2_SHOWBROWSER);
      WEBVIEW2_SHOWBROWSER :
        if (FMXWindowParent <> nil) then
          begin
            FMXWindowParent.WindowState := TWindowState.wsNormal;
            ResizeChild;
          end;
    end;
    aMessage.Result := CallWindowProc(FOldWndPrc, FmxHandleToHWND(Handle), aMessage.Msg, aMessage.wParam, aMessage.lParam);
  except
    on e : exception do
      Log.d('TBrowserForm.CustomWndProc error : ' + e.message);
  end;
end;
procedure TBrowserForm.UpdateCustomWindowState;
var
  TempNewState : TWindowState;
begin
  TempNewState := GetCurrentWindowState;
  if (FCustomWindowState <> TempNewState) then
    begin
      if (FCustomWindowState = TWindowState.wsMinimized) then
        PostCustomMessage(WEBVIEW2_SHOWBROWSER);
      FCustomWindowState := TempNewState;
    end;
end;
procedure TBrowserForm.WVFMXBrowser1AfterCreated(Sender: TObject);
begin
  FMXWindowParent.UpdateSize;
  Caption            := 'Simple FMX Browser';
  AddressLay.Enabled := True;
end;
procedure TBrowserForm.WVFMXBrowser1GotFocus(Sender: TObject);
begin
  // We use a hidden button to fix the focus issues when the browser has the real focus.
  FocusWorkaroundBtn.SetFocus;
end;
procedure TBrowserForm.WVFMXBrowser1InitializationError(Sender: TObject;
  aErrorCode: HRESULT; const aErrorMessage: wvstring);
begin
  showmessage(aErrorMessage);
end;
procedure TBrowserForm.WVFMXBrowser1StatusBarTextChanged(Sender: TObject;
  const aWebView: ICoreWebView2);
begin
  StatusLbl.Text := WVFMXBrowser1.StatusBarText;
end;
function TBrowserForm.GetCurrentWindowState : TWindowState;
var
  TempPlacement : TWindowPlacement;
  TempHWND      : HWND;
begin
  // TForm.WindowState is not updated correctly in FMX forms.
  // We have to call the GetWindowPlacement function in order to read the window state correctly.
  Result   := TWindowState.wsNormal;
  TempHWND := FmxHandleToHWND(Handle);
  ZeroMemory(@TempPlacement, SizeOf(TWindowPlacement));
  TempPlacement.Length := SizeOf(TWindowPlacement);
  if GetWindowPlacement(TempHWND, @TempPlacement) then
    case TempPlacement.showCmd of
      SW_SHOWMAXIMIZED : Result := TWindowState.wsMaximized;
      SW_SHOWMINIMIZED : Result := TWindowState.wsMinimized;
    end;
  if IsIconic(TempHWND) then Result := TWindowState.wsMinimized;
end;
initialization
  GlobalWebView2Loader                := TWVLoader.Create(nil);
  GlobalWebView2Loader.UserDataFolder := ExtractFileDir(ParamStr(0)) + '\CustomCache';
  GlobalWebView2Loader.StartWebView2;
end.
