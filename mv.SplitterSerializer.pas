unit mv.SplitterSerializer;

interface

uses
  System.Classes, FMX.Controls, FMX.Types,
  System.Rtti, System.JSON,
  mv.BufferPanel, mv.SplitterPanel, mv.PluginManager;

type
  TSerializedSplitter = class
  private
    SplitDirection: TSplitDirection;
    SplitterPosition: Integer;
    LeftControl: TObject;  // Will hold either TSerializedSplitter or TSerializedBuffer
    RightControl: TObject;
    FGlobalLayoutValues: TStringList;
  public
    constructor Create; overload;
    constructor Create(ASplitterPanel: TSplitterPanel; AGlobalLayoutValues: TStringList); overload;

    function ToString: string;
    function FromString(const AJSONStr: string): TSerializedSplitter;
    function CreateSplitter(AOwner: TFMXObject; APluginManager: TPluginManager; AWebPort: integer): TSplitterPanel;
    property GlobalLayoutValues: TStringList read FGlobalLayoutValues write FGlobalLayoutValues;
  end;

  TSerializedBuffer = class
  private
    BufferID: Integer;
    CommandType: string;
    Properties: string;
  public
    constructor Create(ABufferPanel: TBufferPanel); overload;
    function ToJSON: string; // Serialize to JSON string
    class function FromJSON(const AJSONStr: string): TSerializedBuffer; // Deserialize from JSON string
  end;


implementation


constructor TSerializedSplitter.Create;
begin
  inherited Create;
  SplitDirection := sdVertical;
  SplitterPosition := 0;
  LeftControl := nil;
  RightControl := nil;
  FGlobalLayoutValues := nil;
end;

constructor TSerializedSplitter.Create(ASplitterPanel: TSplitterPanel; AGlobalLayoutValues: TStringList);
begin
  inherited Create;

  FGlobalLayoutValues := AGlobalLayoutValues;
  SplitDirection := ASplitterPanel.SplitDirection;
  SplitterPosition := ASplitterPanel.SplitterPosition;

  if ASplitterPanel.LeftControl is TSplitterPanel then
    // Passing nil as AGlobalLayoutValues so values aren't resaved under every splitter obj
    LeftControl := TSerializedSplitter.Create(TSplitterPanel(ASplitterPanel.LeftControl), nil)
  else if ASplitterPanel.LeftControl is TBufferPanel then
    LeftControl := TSerializedBuffer.Create(TBufferPanel(ASplitterPanel.LeftControl));

  if ASplitterPanel.RightControl is TSplitterPanel then
    RightControl := TSerializedSplitter.Create(TSplitterPanel(ASplitterPanel.RightControl), nil)
  else if ASplitterPanel.RightControl is TBufferPanel then
    RightControl := TSerializedBuffer.Create(TBufferPanel(ASplitterPanel.RightControl));
end;


function TSerializedSplitter.ToString: string;
var
  JSONObj: TJSONObject;
  JSONGlobalLayoutValues: TJSONObject;
  I: integer;
begin
  JSONObj := TJSONObject.Create;
  JSONGlobalLayoutValues := nil;
  try
    JSONObj.AddPair('SplitDirection', TJSONNumber.Create(Ord(SplitDirection)));
    JSONObj.AddPair('SplitterPosition', TJSONNumber.Create(SplitterPosition));

    if LeftControl is TSerializedSplitter then
      JSONObj.AddPair('LeftControl', TJSONObject.ParseJSONValue(TSerializedSplitter(LeftControl).ToString))
    else
      JSONObj.AddPair('LeftControl', TJSONObject.ParseJSONValue(TSerializedBuffer(LeftControl as TSerializedBuffer).ToJSON));

    if RightControl is TSerializedSplitter then
      JSONObj.AddPair('RightControl', TJSONObject.ParseJSONValue(TSerializedSplitter(RightControl).ToString))
    else
      JSONObj.AddPair('RightControl', TJSONObject.ParseJSONValue(TSerializedBuffer(RightControl as TSerializedBuffer).ToJSON));

    if nil <> FGlobalLayoutValues then
    begin
      JSONGlobalLayoutValues := TJSONObject.Create;
     // try
        for I := 0 to FGlobalLayoutValues.Count-1 do
          JSONGlobalLayoutValues.AddPair(FGlobalLayoutValues.KeyNames[I], FGlobalLayoutValues.ValueFromIndex[I]);
        JSONObj.AddPair('GlobalValues', JSONGlobalLayoutValues);
     // finally
     //   JSONGlobalLayoutValues.Free;
     // end;
    end;

    Result := JSONObj.Format;
  finally
    JSONObj.Free;
  end;
end;

function TSerializedSplitter.FromString(const AJSONStr: string): TSerializedSplitter;
var
  RootJson: TJSONObject;
  TmpJson: TJSONValue;
  ChildJSON: TJSONValue;
  GlobalPair: TJSONPair;
begin
  Result := nil;
  RootJson := TJSONObject.ParseJSONValue(AJSONStr) as TJSONObject;
  if nil = RootJson then Exit;
  try
    TmpJson := RootJson.GetValue('GlobalValues');
    if nil <> TmpJson then
    begin
      if nil = FGlobalLayoutValues then FGlobalLayoutValues := TStringList.Create
      else FGlobalLayoutValues.Clear;
      if TmpJson is TJSONObject then
        for GlobalPair in TJSONObject(TmpJson) do
        begin
          FGlobalLayoutValues.Add(GlobalPair.JsonString.Value+'='+GlobalPair.JsonValue.Value)
        end;
    end;

    TmpJson := RootJson.GetValue('SplitDirection');
    if nil = TmpJson then Exit;
    SplitDirection := TSplitDirection((TmpJson as TJSONNumber).AsInt);

    TmpJson := RootJson.GetValue('SplitterPosition');
    if nil = TmpJson then Exit;
    SplitterPosition := (TmpJson as TJSONNumber).AsInt;

    // Deserialize LeftControl
    ChildJSON := RootJson.GetValue('LeftControl');
    if nil = ChildJSON then Exit;
    if ChildJSON is TJSONObject then
    begin
      if TJSONObject(ChildJSON).GetValue('BufferID') <> nil then
      begin
        LeftControl := TSerializedBuffer.FromJSON(ChildJSON.ToString);
      end else begin
        LeftControl := TSerializedSplitter.Create;
        TSerializedSplitter(LeftControl).FromString(ChildJSON.ToString);
      end;
    end;

    // Deserialize RightControl
    ChildJSON := RootJson.GetValue('RightControl');
    if nil = ChildJSON then Exit;
    if ChildJSON is TJSONObject then
    begin
      if TJSONObject(ChildJSON).GetValue('BufferID') <> nil then
      begin
        RightControl := TSerializedBuffer.FromJSON(ChildJSON.ToString);
      end else begin
        RightControl := TSerializedSplitter.Create;
        TSerializedSplitter(RightControl).FromString(ChildJSON.ToString);
      end;
    end;
  finally
    RootJson.Free;
  end;

  Result := Self;
end;

function TSerializedSplitter.CreateSplitter(AOwner: TFMXObject; APluginManager: TPluginManager; AWebPort: integer): TSplitterPanel;
var
  buffer: TBufferPanel;
begin
  Result := TSplitterPanel.Create(AOwner, APluginManager, AWebPort); // Parent can be set later when adding to another control
  Result.Parent := AOwner;
  Result.SplitDirection := SplitDirection;
  Result.SplitterPosition := SplitterPosition;

  // Create Left Child
  if LeftControl is TSerializedSplitter then
    Result.SetLeftControl(TSerializedSplitter(LeftControl).CreateSplitter(Result, APluginManager, AWebPort))
  else if LeftControl is TSerializedBuffer then
  begin
    buffer := TBufferPanel.Create(Result, APluginManager, AWebPort);
    Result.SetLeftControl(buffer);
    buffer.SetBufferID(TSerializedBuffer(LeftControl).BufferID);
    buffer.SetCommand(TSerializedBuffer(LeftControl).CommandType);
    buffer.SetProperties(TSerializedBuffer(LeftControl).Properties);
  end;

  // Create Right Child
  if RightControl is TSerializedSplitter then
    Result.SetRightControl(TSerializedSplitter(RightControl).CreateSplitter(Result, APluginManager, AWebPort))
  else if RightControl is TSerializedBuffer then
  begin
    buffer := TBufferPanel.Create(Result, APluginManager, AWebPort);
    Result.SetRightControl(buffer);
    buffer.SetBufferID(TSerializedBuffer(RightControl).BufferID);
    buffer.SetCommand(TSerializedBuffer(RightControl).CommandType);
    buffer.SetProperties(TSerializedBuffer(RightControl).Properties);

  end;
end;




constructor TSerializedBuffer.Create(ABufferPanel: TBufferPanel);
begin
  inherited Create;

  BufferID := ABufferPanel.BufferID;
  CommandType := ABufferPanel.Command;
  Properties := ABufferPanel.Properties;
end;


function TSerializedBuffer.ToJSON: string;
var
  JSONObj: TJSONObject;
begin
  JSONObj := TJSONObject.Create;
  try
    JSONObj.AddPair('BufferID', TJSONNumber.Create(BufferID));
    JSONObj.AddPair('CommandType', CommandType);
    JSONObj.AddPair('Properties', Properties);
    Result := JSONObj.ToString;
  finally
    JSONObj.Free;
  end;
end;

class function TSerializedBuffer.FromJSON(const AJSONStr: string): TSerializedBuffer;
var
  JSONObj: TJSONObject;
begin
  JSONObj := TJSONObject.ParseJSONValue(AJSONStr) as TJSONObject;
  try
    Result := TSerializedBuffer.Create;
    Result.BufferID := JSONObj.GetValue('BufferID').AsType<Integer>;
    Result.CommandType := JSONObj.GetValue('CommandType').Value;
    Result.Properties := JSONObj.GetValue('Properties').Value;
  finally
    JSONObj.Free;
  end;
end;






end.
