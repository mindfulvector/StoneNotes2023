unit WinWatchForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    ListBox1: TListBox;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  function CustomCaptureWindowProc(hWnd: HWND; lParam: LPARAM): BOOL; stdcall;

var
  Form1: TForm1;

implementation

uses
  CaptureWindow;

{$R *.dfm}

function CustomCaptureWindowProc(hWnd: HWND; lParam: LPARAM): BOOL; stdcall;
var
  Len: Integer;
  BufTitle: array [0..1023] of Char;
  Title: string;
  ExStyle: DWORD;
begin
  Result := hWnd <> 0;
  Len := GetWindowText(hWnd, BufTitle, SizeOf(BufTitle));
  Title := StrPas(BufTitle).Trim;
  if ('' <> Title) and ('Default IME' <> Title) and ('MSCTFIME UI' <> Title) then
  begin
    ExStyle := GetWindowLong(hWnd, GWL_EXSTYLE);
    if (ExStyle and WS_EX_LAYERED) <> 0 then
    begin
      Title := Title + ' (WS_EX_LAYERED)';
    end;
    Form1.ListBox1.AddItem(Title, nil);
    try
      if CaptureWindowProc(hWnd, lParam) then
        Form1.ListBox1.AddItem('^^ Captured!', nil)
      else
        Form1.ListBox1.AddItem('^^ Failed!', nil);

    except on E:Exception do
      Form1.ListBox1.AddItem('^^ Error: '+E.Message, nil);
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  EnumWindows(@CustomCaptureWindowProc, 0);
end;

end.
