unit PluginStorageService;

interface

uses
  System.Classes, System.SysUtils, System.Rtti, System.JSON,
  System.NetEncoding, System.StrUtils,
  FMX.TMSFNCWebBrowser;

type
  TPluginStorageService = class
  private
    FLayoutValues: TStringList;
    FWebBrowser: TTMSFNCWebBrowser;
  public
    constructor Create(AWebBrowser: TTMSFNCWebBrowser);
    procedure OnBeforeNavigate(Sender: TObject; var EventParams: TTMSFNCCustomWebBrowserBeforeNavigateParams);
    function ExecuteMethod(const AMethodName: string; AParams: TArray<TValue>): string;
    procedure WriteLayoutValue(AKey: string; AValue: string);
    function ReadLayoutValue(AKey: string): string;
    procedure SetAllLayoutValues(AValues: string);
    function GetAllLayoutValues: string;
  end;

implementation

constructor TPluginStorageService.Create(AWebBrowser: TTMSFNCWebBrowser);
begin
  inherited Create;
  FLayoutValues := TStringList.Create;
  FWebBrowser := AWebBrowser;
  FWebBrowser.OnBeforeNavigate := OnBeforeNavigate;
end;

procedure TPluginStorageService.OnBeforeNavigate(Sender: TObject; var EventParams: TTMSFNCCustomWebBrowserBeforeNavigateParams);
var
  MethodName: string;
  Params: TArray<TValue>;
  Result: string;
  ParamsString: string;
  ParamsArray: TJSONArray;
  I: Integer;
  JobId: Integer;
  URL: String;
  StorageServicePrefix: String;
begin
  URL := EventParams.URL;

  // Detect the file://storage-service prefix
  StorageServicePrefix := 'file://storage-service';
  if URL.StartsWith(StorageServicePrefix, True) then
  begin
    EventParams.Cancel := true;

    // Parse the URL to extract the method name and parameters
    var SplitString := URL.Substring(Length(StorageServicePrefix)+1).Split(['/']);  // Remove the prefix and split by '/'
    if Length(SplitString) > 0 then
    begin
      JobId := SplitString[0].ToInteger;
      MethodName := SplitString[1];
      if Length(SplitString) > 2 then
      begin
        // URL decode remaining parameter strings (there will only be one unless there are forward slashes in a parameter)
        for I := 2 to High(SplitString) do
        begin
          SplitString[I] := TNetEncoding.URL.Decode(SplitString[I]);
          //SplitString[I] := ReplaceStr(SplitString[I], '\', '\\');
        end;

        // Combine all remaining indices to preserve forward slashes in ParamsString
        ParamsString := String.Join('/', SplitString, 2, Length(SplitString) - 2);

        // Parameters should be a Base64URL encoded JSON array
        ParamsArray := TJSONObject.ParseJSONValue(TNetEncoding.Base64URL.Decode(ParamsString)) as TJSONArray;
        SetLength(Params, ParamsArray.Count);
        for I := 0 to ParamsArray.Count-1 do
          Params[I] := TValue.From<string>(ParamsArray.Get(I).Value);
      end;

      Result := ExecuteMethod(MethodName, Params);
      // Send Result back to JavaScript, for example, through a callback�
      FWebBrowser.ExecuteJavaScript(
        Format('PluginStorageService.returnResult(%d, %s)', [JobId, QuotedStr(Result)])
      );
    end;
  end;
end;

function TPluginStorageService.ExecuteMethod(const AMethodName: string; AParams: TArray<TValue>): string;
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  RttiMethod: TRttiMethod;
begin
  RttiType := RttiContext.GetType(Self.ClassType);
  RttiMethod := RttiType.GetMethod(AMethodName);
  if Assigned(RttiMethod) then
    Result := RttiMethod.Invoke(Self, AParams).ToString
  else
    Result := 'Method not found';
end;


// Called by Javascript to store a value that will be saved in the layout file
procedure TPluginStorageService.WriteLayoutValue(AKey, AValue: string);
begin
  FLayoutValues.Values[AKey] := AValue;

end;

// Called by Javascript to get a layout value
function TPluginStorageService.ReadLayoutValue(AKey: string): string;
begin
  Result := FLayoutValues.Values[AKey];
end;

// Called by serializer to set all the layout values for this plugin
procedure TPluginStorageService.SetAllLayoutValues(AValues: string);
begin
  FLayoutValues.Text := AValues;
end;

// Called by serializer to get all layouts values stored by the plugin
function TPluginStorageService.GetAllLayoutValues: string;
begin
  Result := FLayoutValues.Text;
end;


end.

