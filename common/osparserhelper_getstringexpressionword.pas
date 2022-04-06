unit osparserhelper_GetStringExpressionWord;

{$mode Delphi}

interface

uses
  Classes, SysUtils,
  osparserhelper;

type
  TStringExpressionWordReader = class(TObject)
  private
    FIndex: integer;
    FInConstantString: boolean;
    FQuotes: string;
    FNumberBrackets: integer;
    FStringToExermine: string;
    FWordDelimiterSet: TCharSet;
  public
    constructor Create(StringToExermine: string; WordDelimiterSet: TCharSet); overload;
    function IsBeginningOfConstantString: boolean;
    function IsDelimiter: boolean;
    function IsValidDelimiter: boolean;
    procedure CheckEndOfConstantString;
    procedure GetStringExpressionWordAndRemaining(var StringExpressionWord: string;
      var Remaining: string);
    procedure ReadStringExpressionWord(var StringExpressionWord: string;
      var Remaining: string);
  end;


function GetStringExpressionWord(const StringToExermine: string;
  var Remaining: string; const WordDelimiterSet: TCharset): string;


implementation

constructor TStringExpressionWordReader.Create(StringToExermine: string;
  WordDelimiterSet: TCharSet); overload;
begin
  inherited Create;
  FIndex := 1;
  FInConstantString := False;
  FQuotes := '';
  FNumberBrackets := 0;
  FStringToExermine := StringToExermine;
  FWordDelimiterSet := WordDelimiterSet;
end;

function TStringExpressionWordReader.IsBeginningOfConstantString: boolean;
begin
  Result := False;
  if (FStringToExermine[FIndex] in ['"', '''']) then
  begin
    FInConstantString := True;
    FQuotes := FStringToExermine[FIndex];
    Inc(FIndex);
    Result := True;
  end;
end;

function TStringExpressionWordReader.IsDelimiter: boolean;
begin
  Result := True;
  if not (FStringToExermine[FIndex] in FWordDelimiterSet) then
  begin
    // opening brackets (of function calls) have to be closed before the word can end
    if FStringToExermine[FIndex] = '(' then
      Inc(FNumberBrackets);
    if FStringToExermine[FIndex] = ')' then
      Dec(FNumberBrackets);
    Inc(FIndex);
    Result := False;
  end;
end;

function TStringExpressionWordReader.IsValidDelimiter: boolean;
begin
  Result := False;
  if (FStringToExermine[FIndex] in FWordDelimiterSet) then
  begin
    // if ')' is a delimiter, first all brackets of function calls need to be closed
    if (FNumberBrackets = 0) then
      Result := True
    else
    begin
      if FStringToExermine[FIndex] = ')' then
        Dec(FNumberBrackets);
      Inc(FIndex);
    end;
  end;
end;

procedure TStringExpressionWordReader.CheckEndOfConstantString;
begin
  if (FStringToExermine[FIndex] = FQuotes) then
  begin
    FInConstantString := False;
    FQuotes := '';
  end;
end;

procedure TStringExpressionWordReader.GetStringExpressionWordAndRemaining(
  var StringExpressionWord: string; var Remaining: string);
begin
  StringExpressionWord := copy(FStringToExermine, 1, FIndex - 1);
  Remaining := copy(FStringToExermine, FIndex, length(FStringToExermine) - FIndex + 1);
  Remaining := CutLeftBlanks(Remaining);
end;

procedure TStringExpressionWordReader.ReadStringExpressionWord(
  var StringExpressionWord: string; var Remaining: string);
begin
  while (FIndex <= length(FStringToExermine)) do
  begin
    if not FInConstantString then
    begin
      if not IsBeginningOfConstantString then
        if IsDelimiter then
          if IsValidDelimiter then
            Break; // valid delimiter marks end of word -> stop while-loop
    end
    else
    if FInConstantString then
    begin
      CheckEndOfConstantString;
      Inc(FIndex);
    end;
  end;
  GetStringExpressionWordAndRemaining(StringExpressionWord, Remaining);
end;


function GetStringExpressionWord(const StringToExermine: string;
  var Remaining: string; const WordDelimiterSet: TCharset): string;
var
  StringExpressionWordReader: TStringExpressionWordReader;
begin
  if StringToExermine = '' then
  begin
    Result := '';
    Remaining := '';
  end
  else
  begin
    StringExpressionWordReader :=
      TStringExpressionWordReader.Create(StringToExermine, WordDelimiterSet);
    Result := '';
    StringExpressionWordReader.ReadStringExpressionWord(Result, Remaining);
    StringExpressionWordReader.Free;
  end;
end;

end.
