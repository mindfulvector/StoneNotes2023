unit FMX.SplitterPanel;

interface

uses
  System.Classes, System.Types, System.UITypes, FMX.Controls, FMX.Types,
  FMX.Graphics, FMX.Objects,
  FMX.BufferPanel;

type
  TSplitDirection = (sdVertical, sdHorizontal);
  TSide = (LeftTop, RightBottom);

  TSplitterPanel = class(TControl)
  private
    FSplitDirection: TSplitDirection;
    FLeftControl, FRightControl: TControl;
    FSplitterPosition: Integer;
    IsCaptured: Boolean;
    function CalcSplitterPositionPixels: Single;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure SetSplitDirection(const Value: TSplitDirection);
    function SplitSide(ASide: TSide): TSplitterPanel;
    procedure SetLeftControl(const Value: TControl);
    procedure SetRightControl(const Value: TControl);
  published
    property SplitDirection: TSplitDirection read FSplitDirection write SetSplitDirection;
    property SplitterPosition: integer read FSplitterPosition write FSplitterPosition;
    property LeftControl: TControl read FLeftControl write FLeftControl;
    property RightControl: TControl read FRightControl write FRightControl;
  end;

implementation

constructor TSplitterPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSplitterPosition := 50;
  FSplitDirection := sdVertical; // Default direction
  
end;

function TSplitterPanel.CalcSplitterPositionPixels: Single;
begin
  case FSplitDirection of
    sdVertical:
    begin
      CalcSplitterPositionPixels := FSplitterPosition * Width / 100;
    end;
    sdHorizontal:
    begin
      CalcSplitterPositionPixels := FSplitterPosition * Height / 100;
    end;
  end;
end;

procedure TSplitterPanel.SetSplitDirection(const Value: TSplitDirection);
begin
  if FSplitDirection <> Value then
  begin
    FSplitDirection := Value;
    Repaint;
  end;
end;

procedure TSplitterPanel.SetLeftControl(const Value: TControl);
begin
  if (Value = nil) or (FLeftControl <> Value) then
  begin
    FLeftControl := Value;

    if not Assigned(FLeftControl) then
    begin
      FLeftControl := TBufferPanel.Create(Self);
    end;

    FLeftControl.Parent := Self;
    Resize;
  end;
end;

procedure TSplitterPanel.SetRightControl(const Value: TControl);
begin
  if (Value = nil) or (FRightControl <> Value) then
  begin
    FRightControl := Value;

    if not Assigned(FRightControl) then
    begin
      FRightControl := TBufferPanel.Create(Self);
    end;

    FRightControl.Parent := Self;
    Resize;
  end;
end;

procedure TSplitterPanel.Paint;
var
  R: TRectF;
begin
  inherited;
  R := LocalRect;
  case FSplitDirection of
    sdVertical:
    begin
      R.Left := CalcSplitterPositionPixels - 2;
      R.Right := CalcSplitterPositionPixels + 2;
    end;
    sdHorizontal:
    begin
      R.Top := CalcSplitterPositionPixels - 2;
      R.Bottom := CalcSplitterPositionPixels + 2;
    end;
  end;

  // Splitter
  Canvas.FillRect(R, 0, 0, [], 1, TBrush.Create(TBrushKind.Solid, TAlphaColorRec.Black));

  // Draw panel backgrounds
  R := LocalRect;
  case FSplitDirection of
    sdVertical:
    begin
      // Left side
      R.Right := CalcSplitterPositionPixels;
      Canvas.Fill.Kind := TBrushKind.Solid;
      Canvas.Fill.Color := TAlphaColor($DD201b35);
      Canvas.FillRect(R, 0, 0, AllCorners, 1.0);

      // Right side
      R := LocalRect;
      R.Left := CalcSplitterPositionPixels + 2;
      Canvas.Fill.Kind := TBrushKind.Solid;
      Canvas.Fill.Color := TAlphaColor($DD20351b);
      Canvas.FillRect(R, 0, 0, AllCorners, 1.0);
    end;
    sdHorizontal:
    begin
      R.Top := CalcSplitterPositionPixels - 2;
      R.Bottom := CalcSplitterPositionPixels + 2;
    end;
  end;

end;

procedure TSplitterPanel.Resize;
begin
  inherited;
  case FSplitDirection of
    sdVertical:
    begin
      if Assigned(FLeftControl) then
        FLeftControl.SetBounds(0, 0, CalcSplitterPositionPixels-4, Height);
      if Assigned(FRightControl) then
        FRightControl.SetBounds(CalcSplitterPositionPixels + 4, 0, Width - CalcSplitterPositionPixels - 4, Height);
    end;
    sdHorizontal:
    begin
      if Assigned(FLeftControl) then
        FLeftControl.SetBounds(0, 0, Width, CalcSplitterPositionPixels);
      if Assigned(FRightControl) then
        FRightControl.SetBounds(0, CalcSplitterPositionPixels + 4, Width, Height - CalcSplitterPositionPixels - 4);
    end;
  end;
end;

procedure TSplitterPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  // Assuming 4 pixels as the splitter width for grabbing (2 pixels on either side of the position)
  case FSplitDirection of
    sdVertical:
    begin
      if (X > CalcSplitterPositionPixels - 4) and (X < CalcSplitterPositionPixels + 4) then
      begin
        IsCaptured := true;
        Capture;
      end;
    end;
    sdHorizontal:
    begin
      if (Y > CalcSplitterPositionPixels - 4) and (Y < CalcSplitterPositionPixels + 4) then
      begin
        IsCaptured := true;
        Capture;
      end;
    end;
  end;
end;

procedure TSplitterPanel.MouseMove(Shift: TShiftState; X, Y: Single);
var
  OverSplitter: Boolean;
begin
  inherited;

  OverSplitter := False;

  case FSplitDirection of
    sdVertical:
      OverSplitter := (X > CalcSplitterPositionPixels - 2) and (X < CalcSplitterPositionPixels + 2);
    sdHorizontal:
      OverSplitter := (Y > CalcSplitterPositionPixels - 2) and (Y < CalcSplitterPositionPixels + 2);
  end;

  if OverSplitter then
  begin
    case FSplitDirection of
      sdVertical: Cursor := crHSplit;
      sdHorizontal: Cursor := crVSplit;
    end;
  end
  else
  begin
    Cursor := crDefault;
  end;

  if IsCaptured then
  begin
    case FSplitDirection of
      sdVertical: FSplitterPosition := Round(X / Self.Width * 100);
      sdHorizontal: FSplitterPosition := Round(Y / Self.Height * 100);
    end;
    Resize;
    Repaint;
  end;
end;

procedure TSplitterPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  IsCaptured := false;
  ReleaseCapture;
end;

function TSplitterPanel.SplitSide(ASide: TSide): TSplitterPanel;
var
  NewSplitterPanel: TSplitterPanel;
begin
  NewSplitterPanel := TSplitterPanel.Create(Self);
  if Self.SplitDirection = TSplitDirection.sdVertical then
  begin
    NewSplitterPanel.SetSplitDirection(TSplitDirection.sdHorizontal);
  end else begin
    NewSplitterPanel.SetSplitDirection(TSplitDirection.sdVertical);
  end;

  case ASide of
    LeftTop:
    begin
      NewSplitterPanel.SetLeftControl(FLeftControl);
      SetLeftControl(NewSplitterPanel);
    end;
    RightBottom:
    begin
      NewSplitterPanel.SetLeftControl(FRightControl);
      SetRightControl(NewSplitterPanel);
    end;
  end;
  Resize;
  SplitSide := NewSplitterPanel;
end;

end.
