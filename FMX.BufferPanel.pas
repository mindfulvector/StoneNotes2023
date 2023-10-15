unit FMX.BufferPanel;

interface

uses
  System.SysUtils, System.Classes, System.UITypes, System.Types,
  FMX.Controls, FMX.StdCtrls, FMX.Types, FMX.Layouts, FMX.Edit,
  FMX.Memo, FMX.Graphics,
  FMX.TMSFNCTypes, FMX.TMSFNCUtils, FMX.TMSFNCGraphics,
  FMX.TMSFNCGraphicsTypes, FMX.TMSFNCCustomControl, FMX.TMSFNCWebBrowser;

type
  TBufferPanel = class(TPanel)
  private
    FCommandEdit: TEdit;
    FGoButton: TButton;
    FCommandControl: TControl;
    FBufferIdLabel: TLabel;
    FBufferID: integer;
    procedure GoButtonClick(Sender: TObject);
    procedure CommandEditKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
  protected
    procedure Paint; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;

    function BufferID: integer;
    procedure SetBufferID(PBufferID: integer);

    function Command: string;
    procedure SetCommand(SCommand: string);

    function Properties: string;
    procedure SetProperties(SProperties: string);
  end;

implementation

var
  LastBufferID: Integer = 0;

constructor TBufferPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Inc(LastBufferID);
  FBufferID := LastBufferID;

  FBufferIdLabel := TLabel.Create(Self);
  FBufferIdLabel.Parent := Self;
  FBufferIdLabel.Text := 'Buffer ID: ' + IntToStr(LastBufferID);
  FBufferIdLabel.Position.X := 10;
  FBufferIdLabel.Position.Y := 5;

  // Adjust positions of other controls to accommodate the label
  FCommandEdit := TEdit.Create(Self);
  FCommandEdit.Parent := Self;
  FCommandEdit.Width := 150;
  FCommandEdit.Position.X := 10;
  FCommandEdit.Position.Y := FBufferIdLabel.Position.Y + FBufferIdLabel.Height + 5;

  FGoButton := TButton.Create(Self);
  FGoButton.Parent := Self;
  FGoButton.Width := 50;
  FGoButton.Height := 22;
  FGoButton.Text := 'Go ' + IntToStr(LastBufferID);
  FGoButton.Position.X := FCommandEdit.Width + 20;
  FGoButton.Position.Y := FCommandEdit.Position.Y;

  FCommandEdit.OnKeyDown := CommandEditKeyDown;
  FGoButton.OnClick := GoButtonClick;
end;

function TBufferPanel.BufferID: integer;
begin
  Result := FBufferID;
end;

procedure TBufferPanel.SetBufferID(PBufferID: integer);
begin
  FBufferID := PBufferID;
  FBufferIdLabel.Text := 'Buffer ID: ' + IntToStr(FBufferID);
  FGoButton.Text := 'Go ' + IntToStr(FBufferID);
end;


function TBufferPanel.Command: string;
begin
  Command := FCommandEdit.Text;
end;

procedure TBufferPanel.SetCommand(SCommand: string);
begin
  FCommandEdit.Text := SCommand;
  GoButtonClick(FGoButton);
end;

function TBufferPanel.Properties: string;
begin
  if FCommandControl is TMemo then
  begin
    Properties := TMemo(FCommandControl).Text;
  end;

  if FCommandControl is TTMSFNCWebBrowser then
  begin
    Properties := TTMSFNCWebBrowser(FCommandControl).URL;
  end;
end;

procedure TBufferPanel.SetProperties(SProperties: string);
begin
  if FCommandControl is TMemo then
  begin
    TMemo(FCommandControl).Text := SProperties;
  end;

  if FCommandControl is TTMSFNCWebBrowser then
  begin
    TTMSFNCWebBrowser(FCommandControl).Navigate(SProperties);
  end;
end;

procedure TBufferPanel.Paint;
var
  R: TRectF;
begin
  inherited;

  R := LocalRect;
  Canvas.Fill.Kind := TBrushKind.Solid;
  Canvas.Fill.Color := TAlphaColor($FF1b2035);
  Canvas.FillRect(R, 0, 0, AllCorners, 1.0);
end;

procedure TBufferPanel.Resize;
begin
  inherited;
  if Assigned(FCommandEdit) then
  begin
    FCommandEdit.SetBounds(10, FBufferIdLabel.Position.Y + FBufferIdLabel.Height + 5, Width - 70, FCommandEdit.Height);
  end;

  if Assigned(FGoButton) then
  begin
    FGoButton.Position.X := Width - FGoButton.Width - 10;
  end;

  if Assigned(FCommandControl) then
  begin
    FCommandControl.SetBounds(0, FGoButton.Position.Y + FGoButton.Height + 10, Width, Height - (FGoButton.Position.Y + FGoButton.Height + 10));
  end;
end;

procedure TBufferPanel.GoButtonClick(Sender: TObject);
begin
  if Assigned(FCommandControl) then
  begin
    FCommandControl.DisposeOf;
    FCommandControl := nil;
  end;

  // Spawn command controls from command entry
  if UpperCase(FCommandEdit.Text) = 'M' then
  begin
    FCommandControl := TMemo.Create(Self);
    FCommandControl.Parent := Self;
    FCommandControl.Position.X := 10;
    FCommandControl.Position.Y := FGoButton.Height + 20;
    FCommandControl.Width := Width - 20;
    FCommandControl.Height := Height - FGoButton.Height - 30;
    FCommandControl.SetFocus;
  end;

  if UpperCase(FCommandEdit.Text) = 'B' then
  begin
    FCommandControl := TTMSFNCWebBrowser.Create(Self);
    FCommandControl.Parent := Self;
    FCommandControl.Position.X := 10;
    FCommandControl.Position.Y := FGoButton.Height + 20;
    FCommandControl.Width := Width - 20;
    FCommandControl.Height := Height - FGoButton.Height - 30;
    FCommandControl.SetFocus;
    TTMSFNCWebBrowser(FCommandControl).Navigate('https://duckduckgo.com');
  end;

  // Expand for different components based on command.

  FCommandEdit.SelectAll;

  Resize;
end;

procedure TBufferPanel.CommandEditKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkReturn then
  begin
    GoButtonClick(Self);
    Key := 0;  // Prevent default beep or any other default action
  end;
end;

end.

