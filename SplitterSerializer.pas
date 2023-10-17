unit SplitterSerializer;

interface

uses
  System.Classes, FMX.Controls, FMX.Types, FMX.BufferPanel, FMX.SplitterPanel,
  System.Rtti, System.JSON,
  PluginManager;

type
  TSerializedSplitter = class
  private
    SplitDirection: TSplitDirection;
    SplitterPosition: Integer;
    LeftControl: TObject;  // Will hold either TSerializedSplitter or TSerializedBuffer
    RightControl: TObject;
  public
    constructor Create; overload;
    constructor Create(ASplitterPanel: TSplitterPanel); overload;

    function ToString: string;
    function FromString(const AJSONStr: string): TSerializedSplitter;
    function CreateSplitter(AOwner: TFMXObject; APluginManager: TPluginManager): TSplitterPanel;

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
end;

constructor TSerializedSplitter.Create(ASplitterPanel: TSplitterPanel);
begin
  inherited Create;

  SplitDirection := ASplitterPanel.SplitDirection;
  SplitterPosition := ASplitterPanel.SplitterPosition;

  if ASplitterPanel.LeftControl is TSplitterPanel then
    LeftControl := TSerializedSplitter.Create(TSplitterPanel(ASplitterPanel.LeftControl))
  else if ASplitterPanel.LeftControl is TBufferPanel then
    LeftControl := TSerializedBuffer.Create(TBufferPanel(ASplitterPanel.LeftControl));

  if ASplitterPanel.RightControl is TSplitterPanel then
    RightControl := TSerializedSplitter.Create(TSplitterPanel(ASplitterPanel.RightControl))
  else if ASplitterPanel.RightControl is TBufferPanel then
    RightControl := TSerializedBuffer.Create(TBufferPanel(ASplitterPanel.RightControl));
end;


function TSerializedSplitter.ToString: string;
var
  JSONObj: TJSONObject;
begin
  JSONObj := TJSONObject.Create;
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

    Result := JSONObj.ToString;
  finally
    JSONObj.Free;
  end;
end;

function TSerializedSplitter.FromString(const AJSONStr: string): TSerializedSplitter;
var
  RootJson: TJSONObject;
  TmpJson: TJSONValue;
  ChildJSON: TJSONValue;
begin
  Result := nil;
  RootJson := TJSONObject.ParseJSONValue(AJSONStr) as TJSONObject;
  if nil = RootJson then Exit;
  try
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

function TSerializedSplitter.CreateSplitter(AOwner: TFMXObject; APluginManager: TPluginManager): TSplitterPanel;
var
  buffer: TBufferPanel;
begin
  Result := TSplitterPanel.Create(AOwner, APluginManager); // Parent can be set later when adding to another control
  Result.Parent := AOwner;
  Result.SplitDirection := SplitDirection;
  Result.SplitterPosition := SplitterPosition;

  // Create Left Child
  if LeftControl is TSerializedSplitter then
    Result.SetLeftControl(TSerializedSplitter(LeftControl).CreateSplitter(Result, APluginManager))
  else if LeftControl is TSerializedBuffer then
  begin
    buffer := TBufferPanel.Create(Result, APluginManager);
    Result.SetLeftControl(buffer);
    buffer.SetBufferID(TSerializedBuffer(LeftControl).BufferID);
    buffer.SetCommand(TSerializedBuffer(LeftControl).CommandType);
    buffer.SetProperties(TSerializedBuffer(LeftControl).Properties);
  end;

  // Create Right Child
  if RightControl is TSerializedSplitter then
    Result.SetRightControl(TSerializedSplitter(RightControl).CreateSplitter(Result, APluginManager))
  else if RightControl is TSerializedBuffer then
  begin
    buffer := TBufferPanel.Create(Result, APluginManager);
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
