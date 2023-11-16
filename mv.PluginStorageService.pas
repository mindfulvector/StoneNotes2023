unit mv.PluginStorageService;

interface

uses
  System.Classes, System.SysUtils, System.Rtti, System.JSON,
  System.NetEncoding, System.StrUtils, System.Win.ComObj, ActiveX,
  ComServ, System.Variants, StdVcl,
  mv.Logger, mv.StringUtils, mv.PluginService;

type
  TPluginStorageService = class(TPluginService)
  private
    FLayoutValues: TStringList;
    FForPluginCommand: string;
  public
    constructor Create(AForPluginCommand: string);
    procedure WriteLayoutValue(const AKey: string; const AValue: string);
    function ReadLayoutValue(const AKey, ADefault: string): string;
    procedure SetAllLayoutValues(AValues: string);
    function GetAllLayoutValues: string;
    procedure SetForPluginCommand(Value: string);
    function GetForPluginCommand: string;
    property ForPluginCommand: string read FForPluginCommand write FForPluginCommand;
  end;

implementation

constructor TPluginStorageService.Create(AForPluginCommand: string);
begin
  FForPluginCommand := AForPluginCommand;
  FLayoutValues := TStringList.Create;
end;

// Called by Javascript to store a value that will be saved in the layout file
procedure TPluginStorageService.WriteLayoutValue(const AKey, AValue: string);
begin
  FLayoutValues.Values[AKey] := AValue;
end;

// Called by Javascript to get a layout value
function TPluginStorageService.ReadLayoutValue(const AKey, ADefault: string): string;
begin
  if FLayoutValues.IndexOfName(AKey) > -1 then
    Result := FLayoutValues.Values[AKey]
  else
    Result := ADefault;
end;

// Called by serializer to set all the layout values for this plugin
procedure TPluginStorageService.SetAllLayoutValues(AValues: string);
begin
  FLayoutValues.Text := AValues;
end;

procedure TPluginStorageService.SetForPluginCommand(Value: string);
begin
  FForPluginCommand := Value;
end;

// Called by serializer to get all layouts values stored by the plugin
function TPluginStorageService.GetAllLayoutValues: string;
begin
  if Assigned(FLayoutValues) then
    Result := FLayoutValues.Text;
end;


function TPluginStorageService.GetForPluginCommand: string;
begin
  Result := FForPluginCommand;
end;

end.

