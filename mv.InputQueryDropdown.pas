unit mv.InputQueryDropdown;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.ListBox, FMX.Layouts;

function InputQuery(APrompt: String; AOptionsList: TStringList): string;

implementation

{TNotifyEventWrapper}
{
type
  TAnonymousNotifyEvent = reference to procedure(Sender: TObject);

  TNotifyEventWrapper = record
  private
    FAnonymousNotifyEventProcedure: TAnonymousNotifyEvent;
  public
    constructor Create(const AProcedure: TAnonymousNotifyEvent);
    procedure Handler(Sender: TObject);
  end;

constructor TNotifyEventWrapper.Create(const AProcedure: TAnonymousNotifyEvent);
begin
  FAnonymousNotifyEventProcedure := AProcedure;
end;

procedure TNotifyEventWrapper.Handler(Sender: TObject);
begin
  if Assigned(FAnonymousNotifyEventProcedure) then
    FAnonymousNotifyEventProcedure(Sender);
end;
}

type
  TAnonymousNotifyEvent = reference to procedure(Sender: TObject);

  TNotifyEventWrapper = record
  private
    FEvent: TAnonymousNotifyEvent;
  public
    constructor Create(const Value: TAnonymousNotifyEvent);
    procedure Handler(Sender: TObject);
    property EventProcedure: TAnonymousNotifyEvent read FEvent write FEvent;
  end;

{ TNotifyEventWrapper }


constructor TNotifyEventWrapper.Create(const Value: TAnonymousNotifyEvent);
begin
  Self.EventProcedure := Value;
end;

procedure TNotifyEventWrapper.Handler(Sender: TObject);
begin
  if Assigned(FEvent) then
    FEvent(Sender);
end;

{InputQuery with dropdown for options}

function InputQuery(APrompt: String; AOptionsList: TStringList): string;
var
  frm: TForm;
  lblPrompt: TLabel;
  cmbOptions: TComboBox;
  btnOK: TButton;
  btnCancel: TButton;
  layoutButtons: TLayout;
  InputQueryResult: string;
begin
  InputQueryResult := '';

  frm := TForm.CreateNew(nil);
  try
    frm.Width := 300;
    frm.Height := 200;
    frm.Position := TFormPosition.ScreenCenter;
    frm.BorderStyle := TFmxFormBorderStyle.Single;

    lblPrompt := TLabel.Create(frm);
    lblPrompt.Parent := frm;
    lblPrompt.Text := APrompt;
    lblPrompt.Position.Y := 10;
    lblPrompt.Position.X := 10;
    lblPrompt.Width := frm.Width - 20;
    lblPrompt.Height := 50;
    lblPrompt.Visible := true;
    lblPrompt.WordWrap := true;

    cmbOptions := TComboBox.Create(frm);
    cmbOptions.Parent := frm;
    cmbOptions.Position.Y := lblPrompt.Position.Y + lblPrompt.Height + 10;
    cmbOptions.Position.X := 10;
    cmbOptions.Width := frm.Width - 50;
    cmbOptions.Height := 25;
    cmbOptions.Items.Assign(AOptionsList);

    layoutButtons := TLayout.Create(frm);
    layoutButtons.Parent := frm;
    layoutButtons.Position.Y := cmbOptions.Position.Y + cmbOptions.Height + 20;
    layoutButtons.Position.X := 10;
    layoutButtons.Width := frm.Width - 20;
    layoutButtons.Height := 25;

    btnOK := TButton.Create(layoutButtons);
    btnOK.Parent := layoutButtons;
    btnOK.Text := 'OK';
    btnOK.Width := (layoutButtons.Width / 2) - 25;
    btnOK.Height := 25;
    btnOK.Margins.Right := 5;
    btnOK.OnClick := TNotifyEventWrapper.Create(
                      procedure(Sender: TObject)
                      begin
                        InputQueryResult := cmbOptions.Selected.Text;
                        frm.Close;
                      end).Handler;

    btnCancel := TButton.Create(layoutButtons);
    btnCancel.Parent := layoutButtons;
    btnCancel.Text := 'Cancel';
    btnCancel.Width := (layoutButtons.Width / 2) - 25;
    btnCancel.Height := 25;
    btnCancel.Margins.Left := 5;
    btnCancel.Position.X := btnOK.Position.X + btnOK.Width + 25;
    btnCancel.OnClick := TNotifyEventWrapper.Create(
                      procedure(Sender: TObject)
                      begin
                        frm.Close;
                      end).Handler;
    frm.ShowModal;
    Result := InputQueryResult;
  finally
    frm.Free;
  end;
end;

end.
