unit CaptureWindow;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Winapi.DwmApi, System.Win.ComObj,
  Vcl.ExtCtrls, Vcl.Imaging.jpeg;

const user32 = 'user32.dll';
const
  PW_CLIENTONLY = $00000001;

function PrintWindow(hwnd: HWND; hdcBlt: HDC; nFlags: UINT): BOOL; external user32 name 'PrintWindow';

function IsTaskbarWindow(hWnd: HWND): Boolean;
function IsBitmapBlank(Bitmap: TBitmap): Boolean;
function DoCaptureWindow(hWnd: HWND): TBitmap;
function CaptureWindowProc(hWnd: HWND; lParam: LPARAM): BOOL; stdcall;

implementation

function IsTaskbarWindow(hWnd: HWND): Boolean;
begin
  Result := (IsWindowVisible(hWnd) or IsIconic(hWnd)) and
            (GetWindow(hWnd, GW_OWNER) = 0) and
            (GetWindowTextLength(hWnd) > 0);
end;


function DoCaptureWindow(hWnd: HWND): TBitmap;
var
  WinDC: HDC;
  MemDC: HDC;
  Rect: TRect;
  ExStyle: DWORD;
  ThumbnailSize: PSIZE;
  hThumbnail: THandle;
  ThumbProps: DWM_THUMBNAIL_PROPERTIES;
  DoFallback: BOOL;
begin
  Result := TBitmap.Create;
  GetWindowRect(hWnd, Rect);

  WinDC := GetWindowDC(hWnd);
  MemDC := CreateCompatibleDC(WinDC);
  Result.Width := Rect.Right - Rect.Left;
  Result.Height := Rect.Bottom - Rect.Top;

  SelectObject(MemDC, Result.Handle);

  DoFallback := true;

  ExStyle := GetWindowLong(hWnd, GWL_EXSTYLE);
  if (ExStyle and WS_EX_LAYERED) <> 0 then
  begin
    if S_OK = DwmRegisterThumbnail(MemDC, hWnd, @hThumbnail) then DoFallback := false;
    ThumbnailSize.cx := Result.Width;
    ThumbnailSize.cy := Result.Height;

    ZeroMemory(@ThumbProps, SizeOf(ThumbProps));
    ThumbProps.dwFlags := DWM_TNP_RECTDESTINATION or DWM_TNP_VISIBLE or DWM_TNP_OPACITY or DWM_TNP_SOURCECLIENTAREAONLY;
    ThumbProps.fSourceClientAreaOnly := FALSE;
    ThumbProps.opacity := 255;
    ThumbProps.fVisible := TRUE;
    ThumbProps.rcDestination := Rect;

    DwmUpdateThumbnailProperties(hThumbnail, ThumbProps);
    DwmUnregisterThumbnail(hThumbnail);
  end;

  if DoFallback then
    if not PrintWindow(hWnd, MemDC, PW_CLIENTONLY) then
      BitBlt(MemDC, 0, 0, Result.Width, Result.Height, WinDC, 0, 0, SRCCOPY);


  DeleteDC(MemDC);
  ReleaseDC(hWnd, WinDC);
end;

function IsBitmapBlank(Bitmap: TBitmap): Boolean;
var
  x, y: Integer;
  BlankColor: TColor;
  Count: integer;
begin
  BlankColor := Bitmap.Canvas.Pixels[0, 0];
  for y := 0 to Bitmap.Height - 1 do
    for x := 0 to Bitmap.Width - 1 do
    begin
      Inc(Count);
      if Count > 10000 then Exit;
      if Bitmap.Canvas.Pixels[x, y] <> BlankColor then
        Exit;
    end;
  Result := True;
end;



function CaptureWindowProc(hWnd: HWND; lParam: LPARAM): BOOL; stdcall;
var
  WindowText: array[0..255] of Char;
  Bitmap: TBitmap;
  Title: string;
begin
  Result := True; // continue enumeration

  if IsTaskbarWindow(hWnd) then
  begin
    GetWindowText(hWnd, WindowText, 256);
    Title := StrPas(WindowText).Trim.Replace('\', '-').Replace('.', '_').Replace(' ', '_');
    Bitmap := DoCaptureWindow(hWnd);
    try
      if IsBitmapBlank(Bitmap) then
        Result := False
      else
        Bitmap.SaveToFile(Format('%s.bmp', [Title]));
    finally
      Bitmap.Free;
    end;
  end;
end;

end.
