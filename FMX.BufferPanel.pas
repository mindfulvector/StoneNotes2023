unit FMX.BufferPanel;

interface

uses
  System.SysUtils, System.Classes, System.UITypes, System.Types,
  System.IOUtils, System.StrUtils,
  FMX.Controls, FMX.StdCtrls, FMX.Types, FMX.Layouts, FMX.Edit,
  FMX.Memo, FMX.Graphics,
  FMX.TMSFNCTypes, FMX.TMSFNCUtils, FMX.TMSFNCGraphics,
  FMX.TMSFNCGraphicsTypes, FMX.TMSFNCCustomControl, FMX.TMSFNCWebBrowser,

  StringUtils, PluginManager, PluginStorageService;

type
  TBufferPanel = class(TPanel)
  private
    FCommandEdit: TEdit;
    FGoButton: TButton;
    FCommandControl: TControl;
    FBufferIdLabel: TLabel;
    FBufferID: integer;
    FPluginManager: TPluginManager;
    FPluginStorageService: TPluginStorageService;
    procedure GoButtonClick(Sender: TObject);
    procedure CommandEditKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure CreateBrowser;
    procedure BufferCommandMemo(command: TArray<System.string>);
    procedure BufferCommandBrowser(command: TArray<System.string>);
    procedure ScanForPluginCmd(command: TArray<System.string>);
    procedure DisplayError(AMessage: String);
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent; APluginManager: TPluginManager);

    function BufferID: integer;
    procedure SetBufferID(ABufferID: integer);

    function Command: string;
    procedure SetCommand(SCommand: string);

    function Properties: string;
    procedure SetProperties(SProperties: string);
  end;

implementation

var
  LastBufferID: Integer = 0;

constructor TBufferPanel.Create(AOwner: TComponent; APluginManager: TPluginManager);
begin
  inherited Create(AOwner);
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
  FGoButton.Text := 'Go ' + IntToStr(LastBufferID);
  FGoButton.Position.X := FCommandEdit.Width + 20;
  FGoButton.Position.Y := FCommandEdit.Position.Y;

  FCommandEdit.OnKeyDown := CommandEditKeyDown;
  FGoButton.OnClick := GoButtonClick;
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
  FGoButton.Text := 'Go ' + IntToStr(FBufferID);
end;

// Retrieves the last entered command.
function TBufferPanel.Command: string;
begin
  Command := FCommandEdit.Text;
end;

// Parses the given string as if it was entered by the user as a buffer command.
procedure TBufferPanel.SetCommand(SCommand: string);
begin
  FCommandEdit.Text := SCommand;
  GoButtonClick(FGoButton);
end;

// Properties is a string defined by the buffer command for storing
// key values needed to reload state.
function TBufferPanel.Properties: string;
begin
  if FCommandControl is TMemo then
    Properties := TMemo(FCommandControl).Text;

  if FCommandControl is TTMSFNCWebBrowser then
  begin
    Properties := FPluginStorageService.GetAllLayoutValues;
  end;
end;

// Restore properties to the buffer command component.
procedure TBufferPanel.SetProperties(SProperties: string);
begin
  if FCommandControl is TMemo then
    TMemo(FCommandControl).Text := SProperties;

  if FCommandControl is TTMSFNCWebBrowser then
  begin
    //TTMSFNCWebBrowser(FCommandControl).Navigate(SProperties);
    FPluginStorageService.SetAllLayoutValues(SProperties);
  end;
end;

// Note: Unlike other methods named in the pattern BufferCommand*, which
// expect to always recieve a valid command matching their format, this one
// scans for plugin commands and MAY load a plugin into the buffer if one
// is found that matches the command array.
procedure TBufferPanel.ScanForPluginCmd(command: TArray<System.string>);
var
  Plugin: TPlugin;
  PluginPage: string;
  PluginPageHTML: string;
  AssetsDir: string;
  PluginsDir: string;
  FileURL: string;
begin
  Plugin := FPluginManager.FindPluginByCommand(command[0]);
  if Assigned(Plugin) then
  begin
    CreateBrowser;
    if Assigned(Plugin.Settings) then
    begin
      PluginPage := Plugin.Settings.Values['plugin/command_' + command[0]];
      if '' <> PluginPage then
      begin
        PluginPageHTML := Plugin.ReadFile(PluginPage);

        // Inject StorageService JS function
        {PluginPageHTML := ReplaceStr(PluginPageHTML, '</head>',
          '<script>'
          +TTMSFNCWebBrowser(FCommandControl).GetBridgeCommunicationLayer('storageservice')
          +'</script></head>');}

        // Convert line endings to be consistent so we can use the JS debuffer
        PluginPageHTML := StandardizeLineEndings(PluginPageHTML);

        // Find Assets directory, in installed environment or in debug environment
        AssetsDir := ExtractFileDir(ParamStr(0)) + '\Assets';
        if not DirectoryExists(AssetsDir) then
          AssetsDir := ExtractFileDir(ExtractFileDir(ParamStr(0))) + '\Assets';
        if not DirectoryExists(AssetsDir) then
          AssetsDir := ExtractFileDir(ExtractFileDir(ExtractFileDir(ParamStr(0)))) + '\Assets';

        // Find plugins directory
        PluginsDir := ReplaceStr(AssetsDir, '\Assets', '\Plugins');

        // Swap in template tokens
        PluginPageHTML := ReplaceStr(PluginPageHTML, '{assets}', AssetsDir);
        PluginPageHTML := ReplaceStr(PluginPageHTML, '{plugins}', PluginsDir);
        PluginPageHTML := ReplaceStr(PluginPageHTML, '{plugin_dir}',
                                     PluginsDir+'\'+Plugin.DirName);

        // Store next to the original plugin file with a prefix to the filename
        Plugin.WriteFile('eval_' + PluginPage, PluginPageHTML);

        // Setup URL to the prepared HTML file using the file protocol.
        // This is required so that the page can load assets from the Assets
        // and Plugins paths, which is not allowed when HTML is loaded via
        // LoadHTML.
        FileURL := 'file://'+ReplaceStr(PluginsDir + '\'
                            +Plugin.DirName
                            +'\eval_'
                            +PluginPage, '\', '/');

        // Create storage service for this instance and install it
        if Assigned(FPluginStorageService) then
          FPluginStorageService.DisposeOf;

        FPluginStorageService := TPluginStorageService.Create(TTMSFNCWebBrowser(FCommandControl));

        // Load it!
        TTMSFNCWebBrowser(FCommandControl).Navigate(FileURL);
      end else begin
        DisplayError('Error: Command is registered to plugin '
                      +'"'+Plugin.DirName+'", '
                      +'but has no page mapping.');
      end;
    end;
  end;
end;

// B - Basic web browser
procedure TBufferPanel.BufferCommandBrowser(command: TArray<System.string>);
begin
  CreateBrowser;
  if Length(command) = 1 then
    TTMSFNCWebBrowser(FCommandControl).Navigate('https://duckduckgo.com')
  else
  begin
    if command[1].StartsWith('http://') or command[1].StartsWith('https://') then
      TTMSFNCWebBrowser(FCommandControl).Navigate(command[1])
    else
      TTMSFNCWebBrowser(FCommandControl).Navigate('https://' + command[1]);
  end;
end;

// M - Basic memo field
procedure TBufferPanel.BufferCommandMemo(command: TArray<System.string>);
begin
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

// Error display helper
procedure TBufferPanel.DisplayError(AMessage: String);
begin
  CreateBrowser;
  TTMSFNCWebBrowser(FCommandControl).LoadHTML('<style>* { background: #000; color: #FFF;}</style>');
  Resize;
  TTMSFNCWebBrowser(FCommandControl).LoadHTML('<style>* { background: #000; color: #FFF;}</style>'
  +'<div style="border: 1px solid red; margin: 10px; padding: 10px;">'
  +AMessage+'</div>');
end;

// Create* methods should create a buffer component used by multiple buffer
// commands. CreateBrowser is used by the base B command as well as HTML/CSS/JS
// plugins and the error display helper.
procedure TBufferPanel.CreateBrowser;
begin
  if Assigned(FCommandControl) and (not (FCommandControl is TTMSFNCWebBrowser)) then
    FCommandControl.DisposeOf;
  if not (FCommandControl is TTMSFNCWebBrowser) then
  begin
    FCommandControl := TTMSFNCWebBrowser.Create(Self);
    FCommandControl.Parent := Self;
    FCommandControl.Position.X := 10;
    FCommandControl.Position.Y := FGoButton.Height + 20;
    FCommandControl.Width := Width - 20;
    FCommandControl.Height := Height - FGoButton.Height - 30;
  end;
  if Assigned(FPluginStorageService) then
  begin
    FPluginStorageService.DisposeOf;
    FPluginStorageService := nil;
  end;
  FCommandControl.SetFocus;
  TTMSFNCWebBrowser(FCommandControl).LoadHTML('<style>* { background: #000; color: #FFF;}</style>');
  Resize;
end;

// Keep buffer components visible and using the full buffer space
procedure TBufferPanel.Resize;
begin
  inherited;

  // Basic chrome for buffer itself
  FCommandEdit.SetBounds(
          {x}10,
          {y}FBufferIdLabel.Position.Y + FBufferIdLabel.Height + 5,
          {w}Width - 70,
          {h}FCommandEdit.Height);
  FGoButton.Position.X := Width - FGoButton.Width - 10;

  // Resize the component
  if Assigned(FCommandControl) then
    FCommandControl.SetBounds(
            {x}0,
            {y}FGoButton.Position.Y + FGoButton.Height + 10,
            {w}Width,
            {h}Height - (FGoButton.Position.Y + FGoButton.Height + 10));
end;

procedure TBufferPanel.GoButtonClick(Sender: TObject);
var
  command: TStringDynArray;
begin
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

  // Place back into command field to uppsercase the command
  FCommandEdit.Text := JoinString(command);

  // Spawn command controls from command entry...

  // Check built-in commands first
  if command[0] = 'M' then BufferCommandMemo(command);
  if command[0] = 'B' then BufferCommandBrowser(command);

  // Scan for plugin commands
  if nil = FCommandControl then ScanForPluginCmd(command);

  // Invalid command, bummer!
  if nil = FCommandControl then DisplayError('Error: Unknown command.');

  // Prepare to enter new command
  FCommandEdit.SelectAll;
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

