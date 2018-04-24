unit osparserhelper;

{$mode delphi}

interface

uses
  Classes,
  strutils,
  SysUtils;

const
  WordDelimiterSet0 = [' ', #9, '=', '[', ']'];
  WordDelimiterSetHosts = [' ', '#', #9];
  WordDelimiterSetDBAlias = [':', '='];
  WordDelimiterSet1 = [' ', #9, '=', '[', ']', '(', ')', '"', '''', ',', '+'];
  WordDelimiterSet3 = [' ', #9, '=', '[', ']', '(', ')', '"', '''', ',', '+', ':'];
  WordDelimiterSet2 = [' ', #9, '"', ''''];
  WordDelimiterSet4 = [' ', #9, '=', '[', ']', '('];
  WordDelimiterSet5 = [' ', #9, '('];
  WordDelimiterSet6 = [')',','];
  WordDelimiterWhiteSpace = [' ', #9];


type
  TCharset = set of char;

function CutLeftBlanks(const s: string): string;
function CutRightBlanks(const s: string): string;

procedure GetWord
  (const s: string; var Expression, Remaining: string;
  const WordDelimiterSet: TCharset; searchbackward :boolean = false); overload;
procedure GetWord(const s: string; var Expression, Remaining: string;
  const WordDelimiterString: String; searchbackward :boolean = false);  overload;
function SkipA(const partialS, S: string; var Remaining: string;
  var Error: string): boolean;
  // versucht partialS am Anfang von S zu eliminieren, loescht NICHT die fuehrenden Leerzeichen vom Rest;
  //   wird partialS nicht gefunden, ist Remaining = S
function Skip(const partialS, S: string; var Remaining: string;
  var Error: string): boolean;
  // versucht partialS am Anfang von S zu eliminieren, loescht fuehrende Leerzeichen vom Rest


implementation

function CutLeftBlanks(const s: string): string;
begin
  Result := sysutils.trimleft(s);
end;


function CutRightBlanks(const s: string): string;
begin
  Result := sysutils.trimright(s);
end;

procedure GetWord(const s: string; var Expression, Remaining: string;
  const WordDelimiterSet: TCharset; searchbackward :boolean = false);
     // Expression ist Teilstring von s bis zu einem Zeichen von WordDelimiterSet (ausschliesslich),
     // Remaining beginnt mit dem auf dieses Zeichen folgenden Zeichen, wobei fuehrender Whitespace
     // eliminiert wird

var
  i: integer = 0;
  t: string = '';

begin
  if s = '' then
  begin
    Expression := '';
    Remaining := '';
  end
  else
  begin
    t := s;
    setLength(t, length(t));
    if searchbackward then
    begin
      i := length(t)+1;
      while (i >=0) and not (t[i] in WordDelimiterSet) do
        dec(i);
      // if nothing found get complete string
      if i = -1 then i:= length(t);
    end
    else
    begin
      i := 1;
      while (i <= length(t)) and not (t[i] in WordDelimiterSet) do
        Inc(i);
    end;

    Expression := copy(t, 1, i - 1);
    Remaining := copy(t, i, length(t) - i + 1);
    Remaining := CutLeftBlanks(Remaining);
  end;
end;


procedure GetWord(const s: string; var Expression, Remaining: string;
  const WordDelimiterString: String; searchbackward :boolean = false);
      // Expression ist Teilstring von s bis zu einem Zeichen von WordDelimiterSet (ausschliesslich),
      // Remaining beginnt mit dem auf dieses Zeichen folgenden Zeichen, wobei fuehrender Whitespace
      // eliminiert wird

var
  i: integer = 0;
  t: string = '';
  tr : string;

begin   // experimental
  t := s;
  setLength(t, length(t));
  if searchbackward then
  begin
    tr := ReverseString(s);
    setLength(tr, length(tr));
    i := pos(ReverseString(WordDelimiterString),tr);
    if i > 0 then
    begin
      i := length(tr) - i;
      i := i + length(WordDelimiterString);
    end;
  end
  else
  begin
    i := pos(WordDelimiterString,t);
    i := i + length(WordDelimiterString);
  end;

  Expression := copy(t, 1, i - 1);
  Remaining := copy(t, i, length(t) - i + 1);
  Remaining := CutLeftBlanks(Remaining);
end;

function SkipA(const partialS, S: string; var Remaining: string;
  var Error: string): boolean;
  // versucht partialS am Anfang von S zu eliminieren, loescht NICHT die fuehrenden Leerzeichen vom Rest;
  //   wird partialS nicht gefunden, ist Remaining = S
var
  p2: string = '';
begin
  Remaining := S;
  if length(S) < length(partialS) then
    Result := False
  else
  begin
    p2 := copy(AnsiUpperCase(S), 1, length(partialS));
    if p2 <> AnsiUpperCase(PartialS) then
      Result := False
    else
    begin
      Remaining := copy(S, length(partialS) + 1, length(S));
      Result := True;
    end;
  end;
  if Result then
    Error := ''
  else
    Error := '"' + partialS + '" expected ';
end;

function Skip(const partialS, S: string; var Remaining: string;
  var Error: string): boolean;
  // versucht partialS am Anfang von S zu eliminieren, loescht fuehrende Leerzeichen vom Rest;
  //   wird partialS nicht gefunden, ist Remaining = S
var
  p2: string = '';
begin
  Remaining := S;
  if length(S) < length(partialS) then
    Result := False
  else
  begin
    p2 := copy(AnsiUpperCase(S), 1, length(partialS));
    if p2 <> AnsiUpperCase(PartialS) then
      Result := False
    else
    begin
      Remaining := copy(S, length(partialS) + 1, length(S));
      Remaining := CutLeftBlanks(Remaining);
      Result := True;
    end;
  end;
  if Result then
    Error := ''
  else
    Error := '"' + partialS + '" expected ';
end;


end.

