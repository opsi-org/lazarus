unit osparserhelper_GetStringExpressionWord;

{$mode Delphi}

interface

uses
  Classes, SysUtils,
  osparserhelper;


function GetStringExpressionWord(const StringToExermine: string; var Remaining: string;
  const WordDelimiterSet: TCharset): string;


implementation

function GetStringExpressionWord(const StringToExermine: string; var Remaining: string;
  const WordDelimiterSet: TCharset): string;
var
  i: integer = 0;
  t: string = '';
  InConstantString : boolean = False;
  Quotes: string = '';
  NumberBrackets: integer = 0;
begin
  if StringToExermine = '' then
  begin
    Result := '';
    Remaining := '';
  end
  else
  begin
    i := 1;
    while (i <= length(StringToExermine)) do
    begin
      if not InConstantString then
      begin
        if (StringToExermine[i] in ['"', '''']) then
        begin
          InConstantString := True;
          Quotes := StringToExermine[i];
          Inc(i);
        end
        else
        if not (StringToExermine[i] in WordDelimiterSet) then
        begin
          if StringToExermine[i] = '(' then
            Inc(NumberBrackets);
          Inc(i);
        end
        else
        if (StringToExermine[i] in WordDelimiterSet) then
        begin
          if (NumberBrackets = 0) then
            Break
          else
          begin
            if StringToExermine[i] = ')' then
              Dec(NumberBrackets);
            Inc(i);
          end;
        end;
      end
      else
      if InConstantString then
      begin
        if (StringToExermine[i] = Quotes) then
        begin
          InConstantString := False;
          Quotes := '';
        end;
        Inc(i);
      end;
    end;
  end;

  Result := copy(StringToExermine, 1, i - 1);
  Remaining := copy(StringToExermine, i, length(StringToExermine) - i + 1);
  Remaining := CutLeftBlanks(Remaining);
end;


end.

