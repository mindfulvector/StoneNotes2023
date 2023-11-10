unit mv.StringUtils;

interface

uses
  System.Types, System.SysUtils, System.ZLib, System.NetEncoding,
  System.RegularExpressions, System.Classes, System.StrUtils;

function SplitString(const AInput: string): TStringDynArray;
function JoinString(const Arr: TStringDynArray): string;
function EscapeString(const s: string): string;
function StandardizeLineEndings(const S: string): string;
function AddSlashes(AString: string): string;

function CompressString(const AString: string): string;
function DecompressString(AString: string): string;

implementation

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

function JoinString(const Arr: TStringDynArray): string;
var
  s, Encoded: string;
begin
  Result := '';

  for s in Arr do
  begin
    Encoded := EscapeString(s);

    if (Pos(' ', Encoded) > 0) or (Pos('"', s) > 0) or (Pos('''', s) > 0) or (Pos('\', s) > 0) then
      Result := Result + '"' + Encoded + '" '  // Wrap with double quotes after escaping
    else
      Result := Result + Encoded + ' ';
  end;

  // Remove the trailing space, if present
  if (Result <> '') and (Result[Length(Result)] = ' ') then
    SetLength(Result, Length(Result) - 1);
end;

function EscapeString(const s: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(s) do
  begin
    if s[i] in ['\', '"'] then
      Result := Result + '\';  // Add a backslash before the character
    Result := Result + s[i];
  end;
end;

function AddSlashes(AString: string): string;
begin
  Result := AString;
  Result := ReplaceStr(Result, '"', '\"');
  Result := ReplaceStr(Result, '''', '\''');
end;

function StandardizeLineEndings(const S: string): string;
begin
  Result := S;

  // Replace CR+LF with LF to make sure we don't double-convert in the next step.
  Result := StringReplace(Result, #13#10, #10, [rfReplaceAll]);
  Result := StringReplace(Result, #13, #10, [rfReplaceAll]);

  // Replace LF with CR+LF.
  Result := StringReplace(Result, #10, #13#10, [rfReplaceAll]);
end;

function CompressString(const AString: string): string;
var
  Input, Output: TBytesStream;
  Compressor: TZCompressionStream;
begin
  Result := '';
  if '' = AString then Exit;

  Input := TBytesStream.Create(TEncoding.UTF8.GetBytes(AString));
  try
    Output := TBytesStream.Create;
    try
      Compressor := TZCompressionStream.Create(Output);
      try
        Compressor.CopyFrom(Input, Input.Size);
      finally
        Compressor.Free;
      end;
      Result := '!' + TNetEncoding.Base64URL.EncodeBytesToString(Output.Bytes);
    finally
      Output.Free;
    end;
  finally
    Input.Free;
  end;
end;

function DecompressString(AString: string): string;
var
  Input, Output: TBytesStream;
  Decompressor: TZDecompressionStream;
  DecodedBytes: TBytes;
begin
  Result := '';
  if '' = AString then Exit;

  Result := AString;
  if '!' <> AString[1] then Exit;

  AString := AString.Substring(2);
  DecodedBytes := TNetEncoding.Base64URL.DecodeStringToBytes(AString);

  Input := TBytesStream.Create(DecodedBytes);
  try
    Output := TBytesStream.Create;
    try
      Decompressor := TZDecompressionStream.Create(Input);
      try
        Output.CopyFrom(Decompressor, 0);
      finally
        Decompressor.Free;
      end;
      Result := TEncoding.UTF8.GetString(Output.Bytes, 0, Output.Size);
    finally
      Output.Free;
    end;
  except
    Result := AString;
  end;
  Input.Free;
end;


end.
