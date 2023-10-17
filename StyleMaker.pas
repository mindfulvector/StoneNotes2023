unit StyleMaker;

interface

uses
  {system}
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.IOUtils, System.UIConsts, System.Rtti,

  {framework}
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.Edit, FMX.StdCtrls, FMX.Styles,
  FMX.Utils,

  {custom}
  InputQueryDropdown;

procedure ModifyAndApplyStyleToForm(AForm: TCommonCustomForm);
function AdjustColor(const AColor: TAlphaColor; const Amount: Single): TAlphaColor;
procedure CreateModifiedStyle(const SourceStyleFile: string; const DestStyleFile: string);

implementation

procedure ModifyAndApplyStyleToForm(AForm: TCommonCustomForm);
var
  StylesArray: TStringDynArray;
  StylesList: TStringList;
  ChosenStyle, ModifiedStyleFile: string;
  StyleIndex: Integer;
  I: Integer;
  dlgResult: TModalResult;
  StylesDir: string;
begin
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
        case dlgResult of
          mrYes: begin      // Modify style first
            // Define a path to save the modified style
            ModifiedStyleFile := ChosenStyle + '_modified.style';

            // Modify the chosen style and save it to ModifiedStyleFile
            CreateModifiedStyle(ChosenStyle, ModifiedStyleFile);

            // Apply the new style to the current form
            TStyleManager.SetStyleFromFile(ModifiedStyleFile);
          end;
          mrNo: begin       // Use style as-is
             TStyleManager.SetStyleFromFile(ChosenStyle);
          end;
          mrCancel: ;       // Do nothing
        end;
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

