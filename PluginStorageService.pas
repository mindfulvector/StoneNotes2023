unit PluginStorageService;

interface

uses
  System.Classes, System.SysUtils, System.Rtti, System.JSON,
  System.NetEncoding, System.StrUtils, System.Win.ComObj, ActiveX,
  ComServ,

  Logger, StringUtils, StdVcl, StoneNotes_TLB;

type
  TPluginStorageService = class(TAutoObject, IPluginStorageService)
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
    class function CreateInstance(AForPluginCommand: string): OleVariant;
    property ForPluginCommand: WideString read FForPluginCommand write FForPluginCommand;
  end;

implementation

constructor TPluginStorageService.Create(AForPluginCommand: WideString);
begin
  //FForPluginCommand := AForPluginCommand;
  FLayoutValues := TStringList.Create;
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

class function TPluginStorageService.CreateInstance(AForPluginCommand: string): OleVariant;
var
  TempVariant : OleVariant;
begin
  TempVariant := TPluginStorageService.Create(AForPluginCommand) as IDispatch;
  Result := TempVariant;
end;


initialization
  TAutoObjectFactory.Create(
    ComServer,
    TPluginStorageService,                      // The class to register
    CLASS_PluginStorageService,                 // The class GUID (not IFace GUID!)
    ciMultiInstance, tmApartment);

end.

