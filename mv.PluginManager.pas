unit mv.PluginManager;

interface

uses
  System.SysUtils, System.StrUtils, System.Classes, System.IniFiles,
  System.Generics.Collections, System.IOUtils,
  FMX.Dialogs, mv.Logger, mv.WebView.BrowserForm, mv.PluginService;

type
  TPluginManager = class;

  TPlugin = class
  private
    FDirectory: string;
    FSettings: TStringList;
    FDirName: string;
    FServices: TStringList;
    FActivePluginManager: TPluginManager;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromDirectory(const Dir: string);
    property Settings: TStringList read FSettings;
    property DirName: string read FDirName;

    function ReadFile(AFilename: string): string;
    function GetFilePath(AFilename: string): string;
    procedure WriteFile(AFilename, AValue: string);

    //procedure AttachService(AServiceName: string; AService: TPluginService);

    function HasPluginPageForCommand(ACommand: TArray<System.string>): Boolean;
    function LoadPluginPageForCommand(ACommand: TArray<System.string>; ABrowserForm: TBrowserForm): Boolean;

    property ActivePluginManager: TPluginManager read FActivePluginManager write FActivePluginManager;
  end;

  TPluginManager = class
  private
    FPlugins: TObjectList<TPlugin>;
    FActiveLayoutFilename: string;
    function GetPlugin(Index: Integer): TPlugin;
  public
    constructor Create;
    destructor Destroy; override;

    function LoadPlugins: integer;
    property Plugins[Index: Integer]: TPlugin read GetPlugin; default;

    function FindPluginByCommandWord(ACommand: String): TPlugin;
    function FindPluginInstance(ACommand: string): TPlugin;

    property ActiveLayoutFilename: string read FActiveLayoutFilename write FActiveLayoutFilename;
  end;

implementation

uses
  mv.StringUtils, mv.PluginStorageService;

{ TPlugin }

//procedure TPlugin.AttachService(AServiceName: string; AService: TPluginService);
//begin
//  FServices.AddObject(AServiceName, AService);
//end;

constructor TPlugin.Create;
begin
  FSettings := TStringList.Create;
  FServices := TStringList.Create;
end;

destructor TPlugin.Destroy;
begin
  FSettings.Free;
  inherited;
end;

procedure TPlugin.LoadFromDirectory(const Dir: string);
var
  IniFile: String;
  Ini: TIniFile;
  Rec: TSearchRec;
  Section, Key, Value: string;
  Sections, Keys: TStringList;
begin
  FDirectory := Dir;
  FDirName := ExtractFileName(Dir);

  // Load settings from INI
  IniFile := Dir + '\' + LowerCase(ExtractFileName(Dir)) + '.ini';
  if not FileExists(IniFile) then begin
    ShowMessage('Plugin config file does not exist: ' + IniFile);
    Exit;
  end;

  Ini := TIniFile.Create(IniFile);
  try
    Sections := TStringList.Create;
    Keys := TStringList.Create;
    try
      Ini.ReadSections(Sections);
      for Section in Sections do
      begin
        Keys.Clear;
        Ini.ReadSection(Section, Keys);
        for Key in Keys do
        begin
          Value := Ini.ReadString(Section, Key, '');
          FSettings.Values[Section + '/' + Key] := Value;
        end;
      end;
    finally
      Keys.Free;
      Sections.Free;
    end;
  finally
    Ini.Free;
  end;
end;

function TPlugin.HasPluginPageForCommand(ACommand: TArray<System.string>): Boolean;
begin
  Result := '' <> Settings.Values['plugin/command_' + ACommand[0]];
end;

function TPlugin.LoadPluginPageForCommand(ACommand: TArray<System.string>; ABrowserForm: TBrowserForm): Boolean;
var
  PluginPage: string;
  PluginPageHTML: string;
  AssetsDir: string;
  PluginsDir: string;
  FileURL: string;
  oService: TObject;
  Service: TPluginService;
  ServiceName: string;
begin
  PluginPage := Settings.Values['plugin/command_' + ACommand[0]];
  PluginPageHTML := Self.ReadFile(PluginPage);

  // Inject StorageService JS function
  {PluginPageHTML := ReplaceStr(PluginPageHTML, '</head>',
    '<script>'
    +TTMSFNCWebBrowser(FCommandControl).GetBridgeCommunicationLayer('storageservice')
    +'</script></head>');}

  // Convert line endings to be consistent so we can use the JS debuffer
  PluginPageHTML := StandardizeLineEndings(PluginPageHTML);

  // Find Assets directory, in installed environment or in debug environment
  AssetsDir := '/Assets';
  PluginsDir := '/Plugins';

  // Swap in template tokens
  PluginPageHTML := ReplaceStr(PluginPageHTML, '{assets}', AssetsDir);
  PluginPageHTML := ReplaceStr(PluginPageHTML, '{plugins}', PluginsDir);
  PluginPageHTML := ReplaceStr(PluginPageHTML, '{plugin_dir}',
                               PluginsDir+'/'+Self.DirName);

  // Store next to the original plugin file with a prefix to the filename
  Self.WriteFile('eval_' + PluginPage, PluginPageHTML);

  // Setup URL to the prepared HTML file via the Deno service.
  FileURL := 'http://127.0.0.1:64769/'
                      +Self.DirName
                      +'\eval_'
                      +PluginPage
                      +'?LayFile='
                      +FActivePluginManager.ActiveLayoutFilename;

  // Initialize services if they haven't already been loaded (this function can
  // be called for the same plugin many times to re/load it).
//  if -1 = FServices.IndexOf('TPluginStorageService') then
//  begin
//    FServices.AddObject('TPluginStorageService', TPluginStorageService.Create(JoinString(ACommand)));
//  end;
//
//  // Attach services
//  for oService in FServices.ToObjectArray do
//  begin
//    Service := oService as TPluginService;
//    ServiceName := Service.ClassName;
 //   ABrowserForm.AddService('TPluginStorageService', TPluginStorageService.Create(JoinString(ACommand)).OleObject);
//  end;

  // Load it! (eventually!)
  ABrowserForm.WVFMXBrowser1.DefaultURL := FileURL;
end;

function TPlugin.ReadFile(AFilename: string): string;
begin
  Result := TFile.ReadAllText(FDirectory + '\' + AFilename);
end;

function TPlugin.GetFilePath(AFilename: string): string;
begin
  Result := FDirectory + '\' + AFilename;
end;

procedure TPlugin.WriteFile(AFilename: string; AValue: string);
begin
  TFile.WriteAllText(FDirectory + '\' + AFilename, AValue);
end;

{ TPluginManager }

constructor TPluginManager.Create;
begin
  FPlugins := TObjectList<TPlugin>.Create;
end;

destructor TPluginManager.Destroy;
begin
  FPlugins.Free;
  inherited;
end;

function TPluginManager.GetPlugin(Index: Integer): TPlugin;
begin
  Result := FPlugins[Index];
end;

function TPluginManager.LoadPlugins: integer;
var
  Rec: TSearchRec;
  Plugin: TPlugin;
  Count: integer;
  Pattern: string;
begin
  Count := 0;
  Pattern := 'Plugins\*';
  if DirectoryExists('..\Plugins') then Pattern := '..\Plugins\*';
  if DirectoryExists('..\..\Plugins') then Pattern := '..\..\Plugins\*';
  if FindFirst(Pattern, faDirectory, Rec) = 0 then
  begin
    repeat
      if (Rec.Name <> '.') and (Rec.Name <> '..') then
      begin
        Plugin := TPlugin.Create;
        try
          Plugin.LoadFromDirectory(ReplaceStr(Pattern, '*', Rec.Name));
          Plugin.ActivePluginManager := Self;
          FPlugins.Add(Plugin);
          mv.Logger.Log(Format('Load plugin "%s"', [Rec.Name]));
          Inc(Count);
        except
          mv.Logger.Log(Format('Error loading plugin "%s"', [Rec.Name]));
          Plugin.Free;
          raise;
        end;
      end;
    until FindNext(Rec) <> 0;
    FindClose(Rec);
  end;
  Result := Count;
end;

function TPluginManager.FindPluginByCommandWord(ACommand: String): TPlugin;
var
  I: Integer;
  Value: String;
begin
  Result := nil;
  for I := 0 to FPlugins.Count - 1 do
  begin
    Value := FPlugins[I].Settings.Values['plugin/command'];
    if Value = ACommand then
    begin
      Result := FPlugins[I];
    end;
  end;
end;

function TPluginManager.FindPluginInstance(ACommand: string): TPlugin;
begin

end;


end.
