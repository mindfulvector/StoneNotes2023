unit mv.PluginEditor;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls, FMX.StdCtrls, FMX.TabControl, FMX.Memo, FMX.Dialogs;

type
  TPluginEditBuffer = record
    FileName: string;
    FileContent: string;
  end;

  TPluginEditor = class(TPanel)
  private
    FNewButton, FOpenButton, FSaveButton, FSaveAsButton: TButton;
    FTabControl: TTabControl;
    procedure NewButtonClick(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    function OpenFile: TPluginEditBuffer;
    procedure SaveFile(Buffer: TPluginEditBuffer);
  public
    constructor Create(AOwner: TComponent); override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TPluginEditor]);
end;

{ TPluginEditor }

constructor TPluginEditor.Create(AOwner: TComponent);
var
  TabItem: TTabItem;
  Memo: TMemo;
begin
  inherited Create(AOwner);

  // Buttons
  FNewButton := TButton.Create(Self);
  FNewButton.Parent := Self;
  FNewButton.SetBounds(5, 5, 64, 22);
  FNewButton.Text := 'New';
  FNewButton.OnClick := NewButtonClick;

  FOpenButton := TButton.Create(Self);
  FOpenButton.Parent := Self;
  FOpenButton.SetBounds(74, 5, 64, 22);
  FOpenButton.Text := 'Open';
  FOpenButton.OnClick := OpenButtonClick;

  FSaveButton := TButton.Create(Self);
  FSaveButton.Parent := Self;
  FSaveButton.SetBounds(143, 5, 64, 22);
  FSaveButton.Text := 'Save';
  FSaveButton.OnClick := SaveButtonClick;

  FSaveAsButton := TButton.Create(Self);
  FSaveAsButton.Parent := Self;
  FSaveAsButton.SetBounds(212, 5, 64, 22);
  FSaveAsButton.Text := 'Save As';

  // TabControl
  FTabControl := TTabControl.Create(Self);
  FTabControl.Parent := Self;
  FTabControl.Align := TAlignLayout.Client;
  FTabControl.Margins.Top := 32;

  NewButtonClick(FNewButton);
end;

procedure TPluginEditor.NewButtonClick(Sender: TObject);
var
  TabItem: TTabItem;
  Memo: TMemo;
begin
  TabItem := FTabControl.Add;
  TabItem.Text := 'New File '+IntToStr(FTabControl.TabCount);
  Memo := TMemo.Create(TabItem);
  TabItem.TagObject := Memo;
  Memo.Parent := TabItem;
  Memo.Align := TAlignLayout.Client;
  Memo.Font.Family := 'Courier New'; // Fixed-width font
  FTabControl.ActiveTab := TabItem;
end;

procedure TPluginEditor.OpenButtonClick(Sender: TObject);
var
  Buffer: TPluginEditBuffer;
  TabItem: TTabItem;
  Memo: TMemo;
begin
  Buffer := OpenFile;
  TabItem := FTabControl.Add;
  TabItem.Text := Buffer.FileName;
  Memo := TMemo.Create(TabItem);
  TabItem.TagObject := Memo;
  Memo.Parent := TabItem;
  Memo.Align := TAlignLayout.Client;
  Memo.Font.Family := 'Courier New'; // Fixed-width font
  Memo.Text := Buffer.FileContent;
  FTabControl.ActiveTab := TabItem;
end;

procedure TPluginEditor.SaveButtonClick(Sender: TObject);
var
  Buffer: TPluginEditBuffer;
  Memo: TMemo;
begin
  if FTabControl.ActiveTab <> nil then
  begin
    Memo := FTabControl.ActiveTab.TagObject as TMemo;
    Buffer.FileName := FTabControl.ActiveTab.Text;
    Buffer.FileContent := Memo.Text;
    SaveFile(Buffer);
  end;
end;

function TPluginEditor.OpenFile: TPluginEditBuffer;
begin
  // For the sake of the example, we will return a dummy file.
  // In real life, you would use a file dialog to select a file and read its content.
  Result.FileName := 'Test.txt';
  Result.FileContent := 'This is a test content.';
end;

procedure TPluginEditor.SaveFile(Buffer: TPluginEditBuffer);
begin
  // For the sake of the example, we simply display a message.
  // In real life, you would save the content of Buffer to the specified file.
  ShowMessage('Saving file ' + Buffer.FileName);
end;

end.

