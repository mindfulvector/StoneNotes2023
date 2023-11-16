unit uInputUtils;

interface

uses
  Windows;

type
  TKeyState = (ksDown, ksUp);

function SendInputKey(AVirtualKey: Integer; AScanCode: Integer;
  AKeyState: TKeyState): Boolean;

implementation


{
https://stackoverflow.com/a/12565963/22837574

"Disclaimer:

I'm not answering the question as it is. I'm trying to propose a way I'll
rather follow when I'd need a virtual keyboard.

1. What about ready made component ?

I would suggest you to use the TTouchKeyboard component, which is a VCL
component, representing a virtual keyboard. That said, you're developing
something, what is already a part of Delphi distributions. It's a part of
Delphi since version 2010, but I can't say in which distributions.

2. It looks ugly and I'll rather make my own:

When I've seen TTouchKeyboard component for the first time, I was hoping
that allows owner drawing. Well, unfortunately doesn't. In that case I'd
try to simulate key strokes by myself rather than solve cases like this
for another components you may soon or later use.

2.1. How to simulate keystrokes in your own way ?

The following code simulates keystrokes by using the SendInput function
and it's based on the code used by TTouchKeyboard component:

    (* SendInputKey function *)

And the usage of the above function. You can pass virtual key, scan code
or both to this function. When you'll be unsure with either of them, pass
value of -1 and the key code will be additionally mapped by the MapVirtualKey
function. The following example shows how to send a Backspace and then
Shift + A:

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  SendInputKey(VK_BACK, -1, ksDown);
  SendInputKey(VK_BACK, -1, ksUp);
  SendInputKey(VK_SHIFT, -1, ksDown);
  SendInputKey(Ord('A'), -1, ksDown);
  SendInputKey(Ord('A'), -1, ksUp);
  SendInputKey(VK_SHIFT, -1, ksUp);
end;
}

function SendInputKey(AVirtualKey: Integer; AScanCode: Integer;
  AKeyState: TKeyState): Boolean;
var
  Input: TInput;
begin
  Input.Itype := INPUT_KEYBOARD;
  if (AVirtualKey = -1) and (AScanCode >= 0) then
  begin
    Input.ki.wVk := MapVirtualKey(AScanCode, MAPVK_VSC_TO_VK);
    Input.ki.wScan := AScanCode;
  end
  else if (AVirtualKey >= 0) and (AScanCode = -1) then
  begin
    Input.ki.wVk := AVirtualKey;
    Input.ki.wScan := MapVirtualKey(AVirtualKey, MAPVK_VK_TO_VSC);
  end
  else if (AVirtualKey >= 0) and (AScanCode >= 0) then
  begin
    Input.ki.wVk := AVirtualKey;
    Input.ki.wScan := AScanCode;
  end;
  case AKeyState of
    ksDown: Input.ki.dwFlags := 0;
    ksUp: Input.ki.dwFlags := KEYEVENTF_KEYUP;
  end;
  Result := SendInput(1, Input, SizeOf(TInput)) = 1;
end;

end.
