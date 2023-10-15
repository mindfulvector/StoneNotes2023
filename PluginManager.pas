unit PluginManager;

interface

uses
  System.SysUtils, System.Classes, System.IniFiles, System.Generics.Collections,
  Logger;

type
  TPlugin = class
  private
    FFiles: TDictionary<string, string>;
    FSettings: TDictionary<string, string>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromDirectory(const Dir: string);
    property Files: TDictionary<string, string> read FFiles;
    property Settings: TDictionary<string, string> read FSettings;
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
  end;

implementation

{ TPlugin }

constructor TPlugin.Create;
begin
  FFiles := TDictionary<string, string>.Create;
  FSettings := TDictionary<string, string>.Create;
end;

destructor TPlugin.Destroy;
begin
  FFiles.Free;
  FSettings.Free;
  inherited;
end;

procedure TPlugin.LoadFromDirectory(const Dir: string);
var
  Ini: TIniFile;
  Rec: TSearchRec;
  Section, Key, Value: string;
  Strings: TStringList;
begin
  // Load files into dictionary
  if FindFirst(Dir + '\*', faAnyFile, Rec) = 0 then
  begin
    repeat
      FFiles.AddOrSetValue(Rec.Name, Dir + '\' + Rec.Name);
    until FindNext(Rec) <> 0;
    FindClose(Rec);
  end;

  // Load settings from INI
  Ini := TIniFile.Create(Dir + '\' + ExtractFileName(Dir) + '.ini');
  try
    Strings := TStringList.Create;
    try
      Ini.ReadSections(Strings);
      for Section in Strings do
      begin
        Ini.ReadSectionValues(Section, Strings);
        for Key in Strings do
        begin
          Value := Ini.ReadString(Section, Key, '');
          FSettings.AddOrSetValue(Section + '/' + Key, Value);
        end;
      end;
    finally
      Strings.Free;
    end;
  finally
    Ini.Free;
  end;
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
          Plugin.LoadFromDirectory('Plugins\' + Rec.Name);
          FPlugins.Add(Plugin);
          Logger.Log(Format('Load plugin "%s"', [Rec.Name]));
          Inc(Count);
        except
          Plugin.Free;
          raise;
        end;
      end;
    until FindNext(Rec) <> 0;
    FindClose(Rec);
  end;
  Result := Count;
end;

end.

