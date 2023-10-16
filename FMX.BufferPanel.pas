unit FMX.BufferPanel;

interface

uses
  System.SysUtils, System.Classes, System.UITypes, System.Types,
  System.IOUtils, System.StrUtils,
  FMX.Controls, FMX.StdCtrls, FMX.Types, FMX.Layouts, FMX.Edit,
  FMX.Memo, FMX.Graphics,
  FMX.TMSFNCTypes, FMX.TMSFNCUtils, FMX.TMSFNCGraphics,
  FMX.TMSFNCGraphicsTypes, FMX.TMSFNCCustomControl, FMX.TMSFNCWebBrowser,

  StringUtils, PluginManager;

type
  TBufferPanel = class(TPanel)
  private
    FCommandEdit: TEdit;
    FGoButton: TButton;
    FCommandControl: TControl;
    FBufferIdLabel: TLabel;
    FBufferID: integer;
    FPluginManager: TPluginManager;
    procedure GoButtonClick(Sender: TObject);
    procedure CommandEditKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
  protected
    procedure Paint; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent; APluginManager: TPluginManager);

    function BufferID: integer;
    procedure SetBufferID(PBufferID: integer);

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

function TBufferPanel.BufferID: integer;
begin
  Result := FBufferID;
end;

procedure TBufferPanel.SetBufferID(PBufferID: integer);
begin
  FBufferID := PBufferID;
  FBufferIdLabel.Text := 'Buffer ID: ' + IntToStr(FBufferID);
  FGoButton.Text := 'Go ' + IntToStr(FBufferID);
end;


function TBufferPanel.Command: string;
begin
  Command := FCommandEdit.Text;
end;

procedure TBufferPanel.SetCommand(SCommand: string);
begin
  FCommandEdit.Text := SCommand;
  GoButtonClick(FGoButton);
end;

function TBufferPanel.Properties: string;
begin
  if FCommandControl is TMemo then
  begin
    Properties := TMemo(FCommandControl).Text;
  end;

  if FCommandControl is TTMSFNCWebBrowser then
  begin
    Properties := TTMSFNCWebBrowser(FCommandControl).URL;
  end;
end;

procedure TBufferPanel.SetProperties(SProperties: string);
begin
  if FCommandControl is TMemo then
  begin
    TMemo(FCommandControl).Text := SProperties;
  end;

  if FCommandControl is TTMSFNCWebBrowser then
  begin
    TTMSFNCWebBrowser(FCommandControl).Navigate(SProperties);
  end;
end;

procedure TBufferPanel.Paint;
var
  R: TRectF;
begin
  inherited;

  R := LocalRect;
  Canvas.Fill.Kind := TBrushKind.Solid;
  Canvas.Fill.Color := TAlphaColor($FF1b2035);
  Canvas.FillRect(R, 0, 0, AllCorners, 1.0);
end;

procedure TBufferPanel.Resize;
begin
  inherited;
  if Assigned(FCommandEdit) then
  begin
    FCommandEdit.SetBounds(10, FBufferIdLabel.Position.Y + FBufferIdLabel.Height + 5, Width - 70, FCommandEdit.Height);
  end;

  if Assigned(FGoButton) then
  begin
    FGoButton.Position.X := Width - FGoButton.Width - 10;
  end;

  if Assigned(FCommandControl) then
  begin
    FCommandControl.SetBounds(0, FGoButton.Position.Y + FGoButton.Height + 10, Width, Height - (FGoButton.Position.Y + FGoButton.Height + 10));
  end;
end;

procedure TBufferPanel.GoButtonClick(Sender: TObject);
var
  command: TStringDynArray;
  Plugin: PluginManager.TPlugin;
  PluginPage: String;
  PluginPageHTML: String;
  AssetsDir, PluginsDir: string;
  FileURL: string;
begin

  command := SplitString(FCommandEdit.Text);
  if Length(command) = 0 then Exit;

  command[0] := UpperCase(command[0]);

  // Spawn command controls from command entry
  if command[0] = 'M' then
  begin
    if Assigned(FCommandControl) then FCommandControl.DisposeOf;
    FCommandControl := TMemo.Create(Self);
    FCommandControl.Parent := Self;
    FCommandControl.Position.X := 10;
    FCommandControl.Position.Y := FGoButton.Height + 20;
    FCommandControl.Width := Width - 20;
    FCommandControl.Height := Height - FGoButton.Height - 30;
    FCommandControl.SetFocus;
    Resize;
  end;

  if command[0] = 'B' then
  begin
    if Assigned(FCommandControl) and (not (FCommandControl is TTMSFNCWebBrowser)) then FCommandControl.DisposeOf;
    if not (FCommandControl is TTMSFNCWebBrowser) then
    begin
      FCommandControl := TTMSFNCWebBrowser.Create(Self);
      FCommandControl.Parent := Self;
      FCommandControl.Position.X := 10;
      FCommandControl.Position.Y := FGoButton.Height + 20;
      FCommandControl.Width := Width - 20;
      FCommandControl.Height := Height - FGoButton.Height - 30;
    end;
    FCommandControl.SetFocus;
    TTMSFNCWebBrowser(FCommandControl).LoadHTML('<style>* { background: #000; color: #FFF;}</style>');
    Resize;
    if Length(command) = 1 then
      TTMSFNCWebBrowser(FCommandControl).Navigate('https://duckduckgo.com')
    else begin
      if command[1].StartsWith('http://') or command[1].StartsWith('https://') then
        TTMSFNCWebBrowser(FCommandControl).Navigate(command[1])
      else
        TTMSFNCWebBrowser(FCommandControl).Navigate('https://'+command[1]);
    end;
  end;

  if nil = FCommandControl then
  begin
    Plugin := FPluginManager.FindPluginByCommand(command[0]);
    if Assigned(Plugin) then
    begin
      if Assigned(FCommandControl) and (not (FCommandControl is TTMSFNCWebBrowser)) then FCommandControl.DisposeOf;
      if not (FCommandControl is TTMSFNCWebBrowser) then
      begin
        FCommandControl := TTMSFNCWebBrowser.Create(Self);
        FCommandControl.Parent := Self;
        FCommandControl.Position.X := 10;
        FCommandControl.Position.Y := FGoButton.Height + 20;
        FCommandControl.Width := Width - 20;
        FCommandControl.Height := Height - FGoButton.Height - 30;
      end;
      FCommandControl.SetFocus;
      TTMSFNCWebBrowser(FCommandControl).LoadHTML('<style>* { background: #000; color: #FFF;}</style>');
      Resize;
      if Assigned(Plugin.Settings) then
      begin
        PluginPage := Plugin.Settings.Values['plugin/command_'+command[0]];
        if '' <> PluginPage then
        begin
          PluginPageHTML := Plugin.ReadFile(PluginPage);

          AssetsDir := ExtractFileDir(ParamStr(0)) + '\Assets';
          if not DirectoryExists(AssetsDir) then
            AssetsDir :=  ExtractFileDir(ExtractFileDir(ParamStr(0))) + '\Assets';
          if not DirectoryExists(AssetsDir) then
            AssetsDir :=  ExtractFileDir(ExtractFileDir(ExtractFileDir(ParamStr(0)))) + '\Assets';

          PluginsDir := ReplaceStr(AssetsDir, '\Assets', '\Plugins');

          PluginPageHTML := ReplaceStr(PluginPageHTML, '{assets}', AssetsDir);
          Plugin.WriteFile('eval_'+PluginPage, PluginPageHTML);
          FileURL := 'file://'+ReplaceStr(PluginsDir + '\' + Plugin.DirName + '\eval_' + PluginPage, '\', '/');
          TTMSFNCWebBrowser(FCommandControl).Navigate(FileURL);
        end else begin
          TTMSFNCWebBrowser(FCommandControl).LoadHTML('<div style="border: 1px solid red; margin: 10px; padding: 10px;">Error: Command has no page mapping in this plugin.</div>');
        end;
      end;
    end;
  end;

  if nil = FCommandControl then
  begin
    Plugin := FPluginManager.FindPluginByCommand(command[0]);
    if nil <> Plugin then
    begin
      if Assigned(FCommandControl) and (not (FCommandControl is TTMSFNCWebBrowser)) then FCommandControl.DisposeOf;
      if not (FCommandControl is TTMSFNCWebBrowser) then
      begin
        FCommandControl := TTMSFNCWebBrowser.Create(Self);
        FCommandControl.Parent := Self;
        FCommandControl.Position.X := 10;
        FCommandControl.Position.Y := FGoButton.Height + 20;
        FCommandControl.Width := Width - 20;
        FCommandControl.Height := Height - FGoButton.Height - 30;
      end;
      FCommandControl.SetFocus;
      TTMSFNCWebBrowser(FCommandControl).LoadHTML('<style>* { background: #000; color: #FFF;}</style>');
      Resize;
      TTMSFNCWebBrowser(FCommandControl).LoadHTML('<style>* { background: #000; color: #FFF;}</style><div style="border: 1px solid red; margin: 10px; padding: 10px;">Error: Unknown command.</div>');
    end;
  end;

  FCommandEdit.SelectAll;

  Resize;
end;

procedure TBufferPanel.CommandEditKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkReturn then
  begin
    GoButtonClick(Self);
    Key := 0;  // Prevent default beep or any other default action
  end;
end;

end.

