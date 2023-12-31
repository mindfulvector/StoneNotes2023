unit mv.BufferPanel;

interface

uses
  System.SysUtils, System.Classes, System.UITypes, System.Types,
  System.IOUtils, System.StrUtils,
  FMX.Controls, FMX.StdCtrls, FMX.Types, FMX.Layouts, FMX.Edit,
  FMX.Memo, FMX.Graphics, FMX.Forms,
  mv.StringUtils, mv.PluginManager, mv.PluginStorageService, mv.PluginEditor,
  mv.WebView.BrowserForm;

type
  TStoneNotesPanel = class(TPanel)
  public
    procedure ForceResize; virtual;
  end;

  TBufferPanel = class(TStoneNotesPanel)
  private
    FCommandEdit: TEdit;
    FGoButton: TButton;
    FCommandControl: TControl;
    FWindowChild: TCustomForm;
    FCloseButton: TButton;
    FBufferIdLabel: TLabel;
    FBufferID: integer;
    FPluginManager: TPluginManager;
    //FPluginStorageService: TPluginStorageService;
    FMessagePanel: TPanel;
    FMessageMemo: TMemo;
    FWebPort: integer;
    FPlugin: TPlugin;
    procedure GoButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure CommandEditKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    function CreateBrowser(ARequestedURL: string): TBrowserForm;
    procedure BufferCommandMemo(command: TArray<System.string>);
    procedure BufferCommandBrowser(command: TArray<System.string>);
    procedure ScanForPluginCmd(command: TArray<System.string>; WebPort: integer);
    procedure DisplayError(AMessage: String);
    procedure BufferCommandPluginEditor(command: TArray<System.string>);
    procedure BufferCommandQuit(command: TArray<System.string>);
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent; APluginManager: TPluginManager; AWebPort: integer);
    destructor Destroy; override;

    function BufferID: integer;
    procedure SetBufferID(ABufferID: integer);

    function Command: string;
    procedure SetCommand(SCommand: string);

    function Properties: string;
    procedure SetProperties(SProperties: string);

    procedure ForceResize; override;
    property Plugin: TPlugin read FPlugin;
  end;

  procedure TriggerGoButton(BufferID: integer);
  procedure TriggerCloseButton(BufferID: integer);

implementation

uses
  mv.SplitterPanel, mv.MainForm, mv.Logger;

var
  LastBufferID: Integer = 0;
  GoButtons: array of TButton;
  CloseButtons: array of TButton;


{ TStoneNotesPanel }

procedure TStoneNotesPanel.ForceResize;
begin

end;

{ TBufferPanel }

constructor TBufferPanel.Create(AOwner: TComponent; APluginManager: TPluginManager; AWebPort: integer);
begin
  inherited Create(AOwner);
  FWebPort := AWebPort;
  FPluginManager := APluginManager;

  Inc(LastBufferID);
  FBufferID := LastBufferID;

  FBufferIdLabel := TLabel.Create(Self);
  FBufferIdLabel.Parent := Self;
  FBufferIdLabel.Text := 'Buffer ID: ' + IntToStr(LastBufferID);
  FBufferIdLabel.Position.X := 10;
  FBufferIdLabel.Position.Y := 5;

  // Adjust positions of other controls to accommodate the label
  FCommandEdit := TEdit.Create(Self);
  FCommandEdit.Parent := Self;
  FCommandEdit.Width := 150;
  FCommandEdit.Position.X := 10;
  FCommandEdit.Position.Y := FBufferIdLabel.Position.Y + FBufferIdLabel.Height + 5;

  FGoButton := TButton.Create(Self);
  FGoButton.Parent := Self;
  FGoButton.Width := 50;
  FGoButton.Height := 22;
  FGoButton.Text := 'Go &' + IntToStr(LastBufferID);
  FGoButton.Position.X := FCommandEdit.Width + 20;
  FGoButton.Position.Y := FCommandEdit.Position.Y;

  if Length(GoButtons) < LastBufferID+1 then SetLength(GoButtons, LastBufferID+1);
  GoButtons[LastBufferID] := FGoButton;

  FCloseButton := TButton.Create(Self);
  FCloseButton.Parent := Self;
  FCloseButton.Width := 30;
  FCloseButton.Height := 22;
  FCloseButton.Text := ':Q';
  FCloseButton.Position.X := Self.Width - FCloseButton.Width;
  FCloseButton.Position.Y := 0;

  FMessagePanel := TPanel.Create(Self);
  FMessagePanel.Parent := Self;
  FMessagePanel.Width := Self.Width - 20;
  FMessagePanel.Height := Self.Width - FCommandEdit.Position.Y - FCommandEdit.Height - 20;
  FMessagePanel.Position.X := 10;
  FMessagePanel.Position.Y := FCommandEdit.Position.Y + FCommandEdit.Height + 20;

  FMessageMemo := TMemo.Create(Self);
  FMessageMemo.Parent := FMessagePanel;
  FMessageMemo.Width := FMessagePanel.Width - 20;
  FMessageMemo.Height := FMessagePanel.Height - 20;
  FMessageMemo.Position.X := 10;
  FMessageMemo.Position.Y := 10;

  FMessagePanel.Visible := false;

  FCommandEdit.OnKeyDown := CommandEditKeyDown;
  FGoButton.OnClick := GoButtonClick;
  FCloseButton.OnClick := CloseButtonClick;
end;

procedure TriggerGoButton(BufferID: integer);
begin
  if BufferID < Length(GoButtons) then GoButtons[BufferID].OnClick(GoButtons[BufferID]);
end;

procedure TriggerCloseButton(BufferID: integer);
begin
  if BufferID < Length(CloseButtons) then CloseButtons[BufferID].OnClick(CloseButtons[BufferID]);
end;

// Buffer ID is a session-unique integer that identifies this buffer.
// Note that previous BufferIDs may be "reused" when a layout file is loaded,
// therefore when a layout file is loaded it is treated as a brand new session.
function TBufferPanel.BufferID: integer;
begin
  Result := FBufferID;
end;

// Used to restore a IDs to buffers when loaded from a layout file.
procedure TBufferPanel.SetBufferID(ABufferID: integer);
begin
  // We reset the LastBufferID static down to this new ID if it either
  // matches LastBufferID or is higher than it, so that the state matches
  // the buffers loaded from this layout.
  if ABufferID >= LastBufferID then
    LastBufferID := ABufferID;

  FBufferID := ABufferID;
  FBufferIdLabel.Text := 'Buffer ID: ' + IntToStr(FBufferID);
  FGoButton.Text := 'Go &' + IntToStr(FBufferID);
end;

// Retrieves the last entered command.
procedure TBufferPanel.CloseButtonClick(Sender: TObject);
begin
  FCommandEdit.Text := ':Q';
  FCommandEdit.SetFocus;
  GoButtonClick(FGoButton);
end;

function TBufferPanel.Command: string;
begin
  Command := FCommandEdit.Text;
end;

// Parses the given string as if it was entered by the user as a buffer command.
procedure TBufferPanel.SetCommand(SCommand: string);
begin
  FCommandEdit.Text := SCommand;
  FCommandEdit.SetFocus;
  GoButtonClick(FGoButton);
end;

// Properties is a string defined by the buffer command for storing
// key values needed to reload state.
function TBufferPanel.Properties: string;
var
  PluginStorageService: TPluginStorageService;
begin
  if FCommandControl is TMemo then
    Properties := TMemo(FCommandControl).Text;

  if Assigned(FPlugin) then
  begin
    PluginStorageService := FPlugin.GetService('TPluginStorageService') as TPluginStorageService;
    Properties := PluginStorageService.GetAllLayoutValues;
  end;
end;

// Restore properties to the buffer command component.
procedure TBufferPanel.SetProperties(SProperties: string);
var
  PluginStorageService: TPluginStorageService;
begin
  if FCommandControl is TMemo then
  begin
    TMemo(FCommandControl).Text := SProperties;
  end;

  if Assigned(FPlugin) then
  begin
    PluginStorageService := FPlugin.GetService('TPluginStorageService') as TPluginStorageService;
    PluginStorageService.SetAllLayoutValues(SProperties);
  end;
end;

// Note: Unlike other methods named in the pattern BufferCommand*, which
// expect to always recieve a valid command matching their format, this one
// scans for plugin commands and MAY load a plugin into the buffer if one
// is found that matches the command array.
procedure TBufferPanel.ScanForPluginCmd(command: TArray<System.string>; WebPort: integer);
var
  Plugin: TPlugin;
  PluginPage: string;
  PluginPageHTML: string;
  AssetsDir: string;
  PluginsDir: string;
  FileURL: string;
begin
  Plugin := FPluginManager.FindPluginByCommandWord(command[0]);
  if Assigned(Plugin) then
  begin
    if Assigned(Plugin.Settings) then
    begin
      if Plugin.HasPluginPageForCommand(command) then
      begin
        FPlugin := Plugin;
        Plugin.LoadPluginPageForCommand(command, CreateBrowser('about:blank'), WebPort);
      end else begin
        DisplayError('Error: Command is registered to plugin '
                      +'"'+Plugin.DirName+'", '
                      +'but has no page mapping.');
      end;
    end;
  end;
end;

procedure TBufferPanel.BufferCommandQuit(command: TArray<System.string>);
begin
  with TSplitterPanel(Self.Parent) do
  begin
    if Self = LeftControl then
      Unsplit(RightControl)
    else if Self = RightControl then
      Unsplit(LeftControl);
  end;

end;

// B - Basic web browser
procedure TBufferPanel.BufferCommandBrowser(command: TArray<System.string>);
begin

  if Length(command) = 1 then
  begin
    CreateBrowser('https://duckduckgo.com');
  end else begin
    if command[1].StartsWith('http://')
        or command[1].StartsWith('https://')
        or command[1].StartsWith('file://')
        or command[1].StartsWith('data://')  then
      CreateBrowser(command[1])
    else
      CreateBrowser('https://' + command[1]);
  end;
end;

// M - Basic memo field
procedure TBufferPanel.BufferCommandMemo(command: TArray<System.string>);
begin
  FMessagePanel.Visible := false;
  if Assigned(FCommandControl) then
    FCommandControl.DisposeOf;
  FCommandControl := TMemo.Create(Self);
  TMemo(FCommandControl).WordWrap := true;
  FCommandControl.Parent := Self;
  FCommandControl.Position.X := 10;
  FCommandControl.Position.Y := FGoButton.Height + 20;
  FCommandControl.Width := Width - 20;
  FCommandControl.Height := Height - FGoButton.Height - 30;
  FCommandControl.SetFocus;
  Resize;
end;

// EPLG - Plugin editor
procedure TBufferPanel.BufferCommandPluginEditor(command: TArray<System.string>);
begin
  FMessagePanel.Visible := false;
  if Assigned(FCommandControl) then
    FCommandControl.DisposeOf;
  FCommandControl := TPluginEditor.Create(Self);
  FCommandControl.Parent := Self;
  FCommandControl.Position.X := 10;
  FCommandControl.Position.Y := FGoButton.Height + 20;
  FCommandControl.Width := Width - 20;
  FCommandControl.Height := Height - FGoButton.Height - 30;
  FCommandControl.SetFocus;
  Resize;
end;



// Error display helper
destructor TBufferPanel.Destroy;
begin
  if Assigned(FWindowChild) then
  begin
    Self.RemoveComponent(FWindowChild);
    FWindowChild.Free;
    FWindowChild := nil;
  end;
  inherited;
end;

procedure TBufferPanel.DisplayError(AMessage: String);
begin
  FMessagePanel.Visible := true;
  FMessageMemo.Text := AMessage;
  FMessageMemo.ReadOnly := True;
  Resize;
end;

procedure TBufferPanel.ForceResize;
begin
  Resize;
end;

// Create* methods should create a buffer component used by multiple buffer
// commands. CreateBrowser is used by the base B command as well as HTML/CSS/JS
// plugins and the error display helper.
function TBufferPanel.CreateBrowser(ARequestedURL: string): TBrowserForm;
begin
  FMessagePanel.Visible := false;
  
  Result := TBrowserForm.Create(nil);
  Result.RequestedURL := ARequestedURL;
  if Assigned(FWindowChild) then
  begin
    FWindowChild.Close;
    FreeAndNil(FWindowChild);
  end;
  FWindowChild := Result;

  Result.Parent := self;

  Self.InsertComponent(Result);
  Result.Visible := True;

  Resize;
end;

// Keep buffer components visible and using the full buffer space
procedure TBufferPanel.Resize;
var
  AbsolutePos: TPointF;
begin
  inherited;

  // Convert the control's local position to its position relative to the form
  AbsolutePos := Self.LocalToAbsolute(PointF(0, 0));

  // Basic chrome for buffer itself
  FCommandEdit.SetBounds(
          {x}10,
          {y}FBufferIdLabel.Position.Y + FBufferIdLabel.Height + 5,
          {w}Width - 70,
          {h}FCommandEdit.Height);
  FGoButton.Position.X := Width - FGoButton.Width - 10;

  FCloseButton.Position.X := Self.Width - FCloseButton.Width;
  FMessagePanel.Width := Self.Width - 20;
  FMessagePanel.Height := Self.Height - FMessagePanel.Position.Y - 20;
  FMessageMemo.Width := FMessagePanel.Width - 20;
  FMessageMemo.Height := FMessagePanel.Height - 20;

  // Resize the component
  if Assigned(FCommandControl) then
    FCommandControl.SetBounds(
            {x}0,
            {y}FGoButton.Position.Y + FGoButton.Height + 10,
            {w}Width,
            {h}Height - (FGoButton.Position.Y + FGoButton.Height + 10));

  if Assigned(FWindowChild) then
  begin
    FWindowChild.Left := Round(frmStoneNotes.Left + TSplitterPanel(Self.Owner).LocalToAbsolute(PointF(0, 0)).X + 5);
    FWindowChild.Top := Round(Self.Top + frmStoneNotes.Top + TSplitterPanel(Self.Owner).LocalToAbsolute(PointF(0, 0)).Y + FGoButton.Position.Y + FGoButton.Height + 45);
    FWindowChild.Width := Round(Self.Width) - 5;
    FWindowChild.Height := Round(Self.Height - FGoButton.Position.Y - FGoButton.Height - 10);
  end;
end;

procedure TBufferPanel.GoButtonClick(Sender: TObject);
var
  command: TStringDynArray;
begin
  //if FCommandEdit.IsFocused then
  //begin
    command := SplitString(FCommandEdit.Text);
    if Length(command) = 0 then
    begin
      if Assigned(FCommandControl) then
        FCommandControl.DisposeOf;
      FCommandControl := nil;
      Exit;
    end;
    // Commands are setup internally in uppercase
    command[0] := UpperCase(command[0]);

    // Place back into command field to uppercase the command
    FCommandEdit.Text := JoinString(command);

    mv.Logger.Log(FCommandEdit.Text);

    FPlugin := nil;

    // Spawn command controls from command entry...
    // Check built-in commands first
    if command[0] = ':Q' then begin BufferCommandQuit(command); Exit; end;
    if command[0] = 'M' then BufferCommandMemo(command);
    if command[0] = 'B' then BufferCommandBrowser(command);
    if command[0] = 'EPLG' then BufferCommandPluginEditor(command);

    // Scan for plugin commands
    if nil = FCommandControl then ScanForPluginCmd(command, FWebPort);

    // Invalid command, bummer!
    if (nil = FCommandControl) and (nil = FWindowChild) and not FMessagePanel.Visible then DisplayError('Error: Unknown command.');

    // Prepare to enter new command
    FCommandEdit.SelectAll;
  //end else begin
  //  FCommandEdit.SetFocus;
  //end;
  mv.Logger.FlushLogBuffer;
end;


procedure TBufferPanel.CommandEditKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkReturn then
  begin
    GoButtonClick(Self);
    Key := 0;  // Prevent default beep
  end;
end;


end.

