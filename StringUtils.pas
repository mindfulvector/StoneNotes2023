unit StringUtils;

interface

uses
  System.Types;

function SplitString(const AInput: string): TStringDynArray;

implementation

uses
  System.SysUtils, System.RegularExpressions;

function SplitString(const AInput: string): TStringDynArray;
var
  i, StartPos: Integer;
  CurrentChar: Char;
  InDoubleQuote, InSingleQuote, Escaping: Boolean;
begin
  SetLength(Result, 0);
  StartPos := 1;
  InDoubleQuote := False;
  InSingleQuote := False;
  Escaping := False;

  for i := 1 to Length(AInput) do
  begin
    if Escaping then
    begin
      Escaping := False;
      Continue;
    end;

    CurrentChar := AInput[i];

    case CurrentChar of
      ' ', #9, #10, #13:  // Space, Tab, LF, CR
        begin
          if not InDoubleQuote and not InSingleQuote then
          begin
            if StartPos < i then
            begin
              SetLength(Result, Length(Result) + 1);
              Result[High(Result)] := Copy(AInput, StartPos, i - StartPos);
            end;
            StartPos := i + 1;
          end;
        end;
      '"':
        begin
          if not InSingleQuote then
          begin
            InDoubleQuote := not InDoubleQuote;
            if not InDoubleQuote and (StartPos < i) then
            begin
              SetLength(Result, Length(Result) + 1);
              Result[High(Result)] := Copy(AInput, StartPos, i - StartPos + 1);
              StartPos := i + 1;
            end;
          end;
        end;
      '''':
        begin
          if not InDoubleQuote then
          begin
            InSingleQuote := not InSingleQuote;
            if not InSingleQuote and (StartPos < i) then
            begin
              SetLength(Result, Length(Result) + 1);
              Result[High(Result)] := Copy(AInput, StartPos, i - StartPos + 1);
              StartPos := i + 1;
            end;
          end;
        end;
      '\':
        begin
          if InDoubleQuote or InSingleQuote then
            Escaping := True; // Mark the next character to be skipped.
        end;
    end;
  end;

  if StartPos <= Length(AInput) then
  begin
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := Copy(AInput, StartPos, Length(AInput) - StartPos + 1);
  end;
end;

end.
