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
    FForPluginCommand: WideString;
  public
    procedure WriteLayoutValue(const AKey: WideString; const AValue: WideString); safecall;
    function ReadLayoutValue(const AKey: WideString): WideString; stdcall;
    procedure SetAllLayoutValues(AValues: WideString); safecall;
    function GetAllLayoutValues: WideString; safecall;
    procedure SetForPluginCommand(Value: WideString); safecall;
    function GetForPluginCommand: WideString; safecall;
    constructor Create(AForPluginCommand: WideString);
    property ForPluginCommand: WideString read FForPluginCommand write FForPluginCommand;
  end;

implementation

var
  PluginStorageInstances: TStringList;

constructor TPluginStorageService.Create(AForPluginCommand: WideString);
begin
  if not Assigned(PluginStorageInstances) then
    PluginStorageInstances := TStringList.Create;

  FForPluginCommand := AForPluginCommand;
  FLayoutValues := TStringList.Create;

  PluginStorageInstances.AddObject(FForPluginCommand, Self);
end;

// Called by Javascript to store a value that will be saved in the layout file
procedure TPluginStorageService.WriteLayoutValue(const AKey: WideString; const AValue: WideString);

begin
  FLayoutValues.Values[AKey] := AValue;

end;

// Called by Javascript to get a layout value
function TPluginStorageService.ReadLayoutValue(const AKey: WideString): WideString;
begin
  Result := FLayoutValues.Values[AKey];
end;

// Called by serializer to set all the layout values for this plugin
procedure TPluginStorageService.SetAllLayoutValues(AValues: WideString);
begin
  FLayoutValues.Text := AValues;
end;

procedure TPluginStorageService.SetForPluginCommand(Value: WideString);
begin
  FForPluginCommand := Value;
end;

// Called by serializer to get all layouts values stored by the plugin
function TPluginStorageService.GetAllLayoutValues: WideString;
begin
  if Assigned(FLayoutValues) then
    Result := FLayoutValues.Text;
end;


function TPluginStorageService.GetForPluginCommand: WideString;
begin
  Result := FForPluginCommand;
end;

end.

