unit PluginStorageService;

interface

uses
  System.SysUtils, System.Classes, System.JSON, FMX.TMSFNCWebBrowser;

type

  TPluginStorageService = class(TInterfacedPersistent, ITMSFNCCustomWebBrowserBridge)
  private
    FValues: TStringList;
    FObjectMessage: string;
    function GetObjectMessage: string;
    procedure SetObjectMessage(const Value: string);
  protected
  public
    constructor Create;
    destructor Destroy;

    procedure WriteLayoutValue(AKey: String; AValue: String);
    function ReadLayoutValue(AKey: String): String;

    property Values: TStringList read FValues;
  published
    property ObjectMessage: string read GetObjectMessage write SetObjectMessage;
  end;

implementation

constructor TPluginStorageService.Create;
begin
  inherited;
  FValues := TStringList.Create;
end;

destructor TPluginStorageService.Destroy;
begin
  FValues.Free;
  inherited;
end;

// Calls from JS are in the form of messages that we need to translate to
// method calls manually.
// Messages that return a value do so by setting the FObjectMessage field.
procedure TPluginStorageService.SetObjectMessage(const Value: string);
var
  MsgValue: TJSONValue;
  Msg: TJSONObject;
begin

  MsgValue := TJSONObject.ParseJSONValue(FObjectMessage);
  try
    if MsgValue is TJSONObject then
    begin
      Msg := MsgValue as TJSONObject;

      if Msg.Values['func'].Value = 'WriteLayoutValue' then
      begin
        Self.WriteLayoutValue(Msg.Values['Key'].Value, Msg.Values['Value'].Value);
      end;

      if Msg.Values['func'].Value = 'ReadLayoutValue' then
      begin
        FObjectMessage := Self.ReadLayoutValue(Msg.Values['Key'].Value);
      end;
    end;
  finally
    MsgValue.Free;
  end;
end;

function TPluginStorageService.GetObjectMessage: string;
begin
  Result := FObjectMessage;
end;

// These methods are translated from messages, and can be used directly in
// Delphi code to manipulate storage.
procedure TPluginStorageService.WriteLayoutValue(AKey: String; AValue: String);
begin
  FValues.Values[AKey] := AValue;
end;

function TPluginStorageService.ReadLayoutValue(AKey: String): String;
begin
  Result := FValues.Values[AKey];
end;



end.

