unit mv.WebService.PluginController;

interface

type
  TPluginController = class
  public
    function PluginResource(const HttpMethod, Path, Query: string): string;
    function AssetResource(const HttpMethod, Path, Query: string): string;
    function ReadLayoutValue(const HttpMethod, Path, Query: string): string;
  end;
implementation

uses
  System.SysUtils, System.IOUtils, System.Classes, System.NetEncoding,
  mv.MainForm, mv.PluginManager, mv.StringUtils, mv.PluginService,
  mv.PluginStorageService;

{ THomeController }

function TPluginController.PluginResource(const HttpMethod, Path, Query: string): string;
var
  PluginManager: TPluginManager;
  PluginName: string;
  PluginResourceName: string;
  Plugin: TPlugin;
  I: integer;
  Filename: string;
begin
  Result := '';
  PluginManager := frmStoneNotes.PluginManager;
  PluginName := ExtractField(Path, '/', 3);
  PluginResourceName := ExtractField(Path, '/', 4);

  for I := 0 to PluginManager.PluginCount-1 do
  begin
    Plugin := PluginManager.Plugins[I];

    if not SameText(Plugin.DirName, PluginName) then
      Continue;

    Filename := PluginManager.PluginsDirectory + '/' + Plugin.DirName + '/' + PluginResourceName;
    if TFile.Exists(Filename) then
    begin
      // This key will be calculated on the client-side and passed in for any
      // service requests triggered by this plugin instance
      if '' = Plugin.ServiceContext then // Only set for the initializing HTML file
        Plugin.ServiceContext := Path + Query; //TNetEncoding.Base64.Encode(Path + Query).Replace('=', '*').Replace('/', '-').Replace('+', '.');
      Result := TFile.ReadAllText(Filename);
      Exit;
    end;
  end;
end;


function TPluginController.AssetResource(const HttpMethod, Path, Query: string): string;
var
  PluginManager: TPluginManager;
  ResourceName: string;
  Filename: string;
begin
  Result := '';
  PluginManager := frmStoneNotes.PluginManager;
  ResourceName := ExtractField(Path, '/', 3);

  Filename := PluginManager.PluginsDirectory.Replace('Plugins','Assets') + '/' + ResourceName;
  if TFile.Exists(Filename) then
  begin
    Result := TFile.ReadAllText(Filename);
  end;
end;

function TPluginController.ReadLayoutValue(const HttpMethod, Path, Query: string): string;
var
  PluginManager: TPluginManager;
  ResourceName: string;
  Params: TStringList;
  AContext: string;
  AKey: string;
  ADefault: string;
  Plugin: TPlugin;
  I: integer;
  StorageService: TPluginStorageService;
begin
  Result := '';
  PluginManager := frmStoneNotes.PluginManager;
  Params := TStringList.Create('"', '&');
  try
    Params.DelimitedText := Query;
    AContext := Params.Values['AContext'].Replace('%2F', '/').Replace('%3D', '=');
    AKey := Params.Values['AKey'];
    ADefault := Params.Values['ADefault'];
    if '' = ADefault then
      ADefault := '{}';

    // Look for a plugin with that ServiceContext
    for I := 0 to PluginManager.PluginCount-1 do
    begin
      Plugin := PluginManager.Plugins[I];

      if not SameText(Plugin.ServiceContext, AContext) then
        Continue;

      // Use the service instance on this plugin instance to perform the action
      StorageService := Plugin.GetService('TPluginStorageService') as TPluginStorageService;
      if Assigned(StorageService) then
        Result := StorageService.ReadLayoutValue(AKey, ADefault);

    end;
  finally
    Params.Free;
  end;
end;


end.

