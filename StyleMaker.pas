unit StyleMaker;

interface

uses
  {system}
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.IOUtils, System.UIConsts, System.Rtti, System.StrUtils,

  {framework}
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.Edit, FMX.StdCtrls, FMX.Styles,
  FMX.Utils,

  {custom}
  InputQueryDropdown;

function ModifyAndApplyStyleToForm(AForm: TCommonCustomForm): string;
function AdjustColor(const AColor: TAlphaColor; const Amount: Single): TAlphaColor;
procedure CreateModifiedStyle(const SourceStyleFile: string; const DestStyleFile: string);

implementation

// Allows user to pick a form style, and optionally randomize it, then returns
// the resulting style as a string for saving.
function ModifyAndApplyStyleToForm(AForm: TCommonCustomForm): string;
var
  StylesArray: TStringDynArray;
  StylesList: TStringList;
  ChosenStyle, ModifiedStyleFile: string;
  StyleIndex: Integer;
  I: Integer;
  dlgResult: TModalResult;
  StylesDir: string;
  IterCount: Integer;
  StyleValid: boolean;
begin
  Result := '';
  StylesList := TStringList.Create;
  try
    // Scan for .style files in the application directory
    StylesDir := 'C:\Users\Public\Documents\Embarcadero\Studio\22.0\Styles';
    StylesArray := TDirectory.GetFiles(StylesDir, '*.style');

    for I := Low(StylesArray) to High(StylesArray) do
      StylesList.Add(ExtractFileName(StylesArray[I]));

    // Ask the user which style they want to modify
    if StylesList.Count > 0 then
    begin
      ChosenStyle := InputQuery('Choose a style (modified styles will be applied immediately)', StylesList);
      if ('' <> ChosenStyle) then
      begin
        ChosenStyle := StylesDir + '\' + ChosenStyle;
        if not ChosenStyle.Contains('_modified') then
          dlgResult := MessageDlg('Do you want to modify the style before applying it?', TMsgDlgType.mtConfirmation,
                          [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo, TMsgDlgBtn.mbCancel], 0)
        else
          dlgResult := mrNo;
        repeat
          case dlgResult of
            mrYes: begin      // Modify style first
              // Define a path to save the modified style
              ModifiedStyleFile := ChosenStyle + '_modified.style';

              // Modify the chosen style and save it to ModifiedStyleFile
              CreateModifiedStyle(ChosenStyle, ModifiedStyleFile);
              IterCount := 1;
              StyleValid := False;

              // Apply the new style to the current form
              repeat
                try
                  TStyleManager.SetStyleFromFile(ModifiedStyleFile);

                  // If we get here then the style did not cause an EReadError
                  // and is therefore valid.
                  StyleValid := true;

                  Result := TFile.ReadAllText(ModifiedStyleFile);
                except on EReadError do begin
                    // If we get here then the style could not be read, some
                    // color is likely out of range, etc. So we need to try
                    // again, unless we have run out of attempts.
                    if IterCount < 100 then
                    begin
                      CreateModifiedStyle(ChosenStyle, ModifiedStyleFile);
                      Result := TFile.ReadAllText(ModifiedStyleFile);
                      Inc(IterCount);
                    end else begin
                      TStyleManager.SetStyleFromFile(ChosenStyle);
                      ShowMessage('Unable to generate new style, out of iterations. Please try again.');
                    end;
                  end;
                end;
              until StyleValid;


            end;
            mrNo: begin       // Use style as-is
               TStyleManager.SetStyleFromFile(ChosenStyle);
               Result := TFile.ReadAllText(ChosenStyle);
            end;
            mrCancel: ;       // Do nothing
          end;
          dlgResult := MessageDlg('Do you want to modify the style again?', TMsgDlgType.mtConfirmation,
                          [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0)
        until dlgResult <> mrYes;
      end;
    end else begin
      ShowMessage('No styles found.');
    end;

  finally
    StylesList.Free;
  end;
end;

function AdjustColor(const AColor: TAlphaColor; const Amount: Single): TAlphaColor;
var
  H, S, L: Single;
  RandomShift: Single;
begin
  RGBtoHSL(AColor, H, S, L);

  // If the color is dark (e.g., L < 0.5), then we lighten it.
  // Otherwise, we darken it.
  if L < 0.5 then
  begin
    L := L + Amount;
    if L > 1 then L := 1;

    // Adding a random color variation
    RandomShift := Random * 0.1; // Vary hue by up to 10%
    H := H + RandomShift;
    if H > 1 then H := H - 1; // Wrap around if hue goes beyond 1
  end
  else
  begin
    L := L - Amount;
    if L < 0 then L := 0;
  end;

  Result := HSLtoRGB(H, S, L);
end;

procedure CreateModifiedStyle(const SourceStyleFile: string; const DestStyleFile: string);
var
  StyleList: TStringList;
  I: Integer;
  Line: string;
  ColorPos: Integer;
  OriginalColor, NewColor: TAlphaColor;
  OriginalColorString, NewColorString: string;
begin
  StyleList := TStringList.Create;
  try
    // Load the .style file into the TStringList
    StyleList.LoadFromFile(SourceStyleFile);

    for I := 0 to StyleList.Count - 1 do
    begin
      Line := StyleList.Strings[I];

      // Search for the pattern "xFF" which denotes the start of a color code
      ColorPos := Pos('xFF', Line);
      if ColorPos > 0 then
      begin
        // If the line is long enough to contain the full color code
        if Length(Line) >= (ColorPos + 7) then
        begin
          // Extract the color code
          OriginalColorString := Copy(Line, ColorPos, 8);
          OriginalColor := StringToAlphaColor(OriginalColorString);
          NewColorString := AlphaColorToString(OriginalColor).Replace('#0', 'x');

          // Modify the color
          NewColor := AdjustColor(OriginalColor, 0.1+Random*0.4);
          NewColorString := AlphaColorToString(NewColor).Replace('#', 'x');
          if Length(NewColorString) > Length(OriginalColorString) then
          begin
            NewColorString := NewColorString.Replace('xF', 'x');
          end;

          // Replace the old color with the new one in the line
          Line := StringReplace(Line, OriginalColorString, NewColorString, []);

          // Update the TStringList entry
          StyleList.Strings[I] := Line;
        end;
      end;
    end;

    // Save the modified style to a new .style file
    StyleList.SaveToFile(DestStyleFile);

  finally
    StyleList.Free;
  end;
end;

end.

