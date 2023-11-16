unit TreeNodeCheckboxClick;

interface

uses
  System.SysUtils, // For basic types, exceptions, and utility functions
  System.Classes,  // For TStringList and other basic classes
  System.IniFiles, // For TIniFile class to read .ini files
  Vcl.Forms,       // For TForm class
  Vcl.Controls,    // For basic control definitions
  Vcl.ComCtrls,    // For TTreeView class
  Vcl.StdCtrls,
  Vcl.Graphics,    // For TBitmap and drawing capabilities
  Vcl.ImgList,     // For TImageList
  Vcl.Dialogs;     // For ShowMessage


type
  TTreeNodeCheckboxClick = reference to procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  TTreeNodeCheckboxClickWrapper = record
  private
    FEvent: TTreeNodeCheckboxClick;
  public
    constructor Create(const Value: TTreeNodeCheckboxClick);
    procedure Handler(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    property EventProcedure: TTreeNodeCheckboxClick read FEvent write FEvent;
  end;

implementation

{ TTreeNodeCheckboxClickWrapper }
constructor TTreeNodeCheckboxClickWrapper.Create(const Value: TTreeNodeCheckboxClick);
begin
  Self.EventProcedure := Value;
end;

procedure TTreeNodeCheckboxClickWrapper.Handler(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FEvent) then
    FEvent(Sender, Button, Shift, X, Y);
end;


end.
