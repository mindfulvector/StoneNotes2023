unit PluginManager;

interface

uses
  System.SysUtils, System.StrUtils, System.Classes, System.IniFiles,
  System.Generics.Collections, System.IOUtils,
  FMX.Dialogs,
  Logger, PluginStorageService;

type
  TPlugin = class
  private
    FDirectory: string;
    FSettings: TStringList;
    FDirName: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromDirectory(const Dir: string);
    property Settings: TStringList read FSettings;
    property DirName: string read FDirName;

    function ReadFile(AFilename: string): string;
    function GetFilePath(AFilename: string): string;
    procedure WriteFile(AFilename, AValue: string);

  end;

  TPluginManager = class
  private
    FPlugins: TObjectList<TPlugin>;
    function GetPlugin(Index: Integer): TPlugin;
  public
    constructor Create;
    destructor Destroy; override;

    function LoadPlugins: integer;
    property Plugins[Index: Integer]: TPlugin read GetPlugin; default;

    function FindPluginByCommand(ACommand: String): TPlugin;
  end;

implementation

{ TPlugin }

constructor TPlugin.Create;
begin
  FSettings := TStringList.Create;
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
          FPlugins.Add(Plugin);
          Logger.Log(Format('Load plugin "%s"', [Rec.Name]));
          Inc(Count);
        except
          Logger.Log(Format('Error loading plugin "%s"', [Rec.Name]));
          Plugin.Free;
          raise;
        end;
      end;
    until FindNext(Rec) <> 0;
    FindClose(Rec);
  end;
  Result := Count;
end;

function TPluginManager.FindPluginByCommand(ACommand: String): TPlugin;
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

end.

