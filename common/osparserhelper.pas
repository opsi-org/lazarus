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
  WordDelimiterSet6 = [')', ','];
  WordDelimiterWhiteSpace = [' ', #9];


type
  TCharset = set of char;

function CutLeftBlanks(const s: string): string;
function CutRightBlanks(const s: string): string;

procedure GetWord
  (const s: string; var Expression, Remaining: string;
  const WordDelimiterSet: TCharset; searchbackward: boolean = False); overload;

procedure GetWord(const s: string; var Expression, Remaining: string;
  const WordDelimiterString: string; searchbackward: boolean = False); overload;

function SkipA(const partialS, S: string; var Remaining: string;
  var Error: string): boolean;
// versucht partialS am Anfang von S zu eliminieren, loescht NICHT die fuehrenden Leerzeichen vom Rest;
//   wird partialS nicht gefunden, ist Remaining = S

function Skip(const partialS, S: string; var Remaining: string;
  var Error: string): boolean;
// versucht partialS am Anfang von S zu eliminieren, loescht fuehrende Leerzeichen vom Rest

procedure GetWordOrStringConstant(const s: string; var Expression, Remaining: string;
  const WordDelimiterSet: TCharset; searchbackward: boolean = False);
// checks if we have a quoted string constant
// if yes it returns the quoted string konstant
// if no it calls getword

implementation

function CutLeftBlanks(const s: string): string;
begin
  Result := SysUtils.trimleft(s);
end;


function CutRightBlanks(const s: string): string;
begin
  Result := SysUtils.trimright(s);
end;

procedure GetWord(const s: string; var Expression, Remaining: string;
  const WordDelimiterSet: TCharset; searchbackward: boolean = False);
// Expression ist Teilstring von s bis zu einem Zeichen von WordDelimiterSet (ausschliesslich),
// Remaining beginnt mit dem auf dieses Zeichen folgenden Zeichen, wobei fuehrender Whitespace
// eliminiert wird

var
  i: integer = 0;
  t: string = '';
  found : boolean;
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
      // get from "hu)hu)))" as expr "hu)hu)" with remaining "))"
      found := false;
      //i := length(t) + 1;
      i := length(t);
      while (i >= 0) and not found do
      begin
        // is it the char we search ?
        if (t[i] in WordDelimiterSet) then
        begin
          // is the leading char the same ?
          if ((i-1 >= 0)) and (t[i] = t[i-1]) then
          dec(i) // we take the next one
          else found := true;
        end
        else Dec(i);
      end;
      // if nothing found get complete string
      if i = -1 then
        i := length(t);
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
  const WordDelimiterString: string; searchbackward: boolean = False);
// Expression ist Teilstring von s bis zu einem Zeichen von WordDelimiterSet (ausschliesslich),
// Remaining beginnt mit dem auf dieses Zeichen folgenden Zeichen, wobei fuehrender Whitespace
// eliminiert wird

var
  i: integer = 0;
  t: string = '';
  tr: string;

begin   // experimental
  t := s;
  setLength(t, length(t));
  if searchbackward then
  begin
    tr := ReverseString(s);
    setLength(tr, length(tr));
    i := pos(ReverseString(WordDelimiterString), tr);
    if i > 0 then
    begin
      i := length(tr) - i;
      i := i + length(WordDelimiterString);
    end;
  end
  else
  begin
    i := pos(WordDelimiterString, t);
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

procedure GetWordOrStringConstant(const s: string; var Expression, Remaining: string;
  const WordDelimiterSet: TCharset; searchbackward: boolean = False);
// checks if we have a quoted string constant
// if yes it returns the quoted string constant
// if no it calls getword
var
  s0: string = '';
  r: string = '';
  StringResult: string = '';
  InfoSyntaxError : string;
begin
  s0 := s;
  // string constant?
  if (length(s0) > 0) and (s0[1] = '"') then
  begin
    r := copy(s0, 2, length(s0) - 1);
    GetWord(r, StringResult, r, ['"']);
    if skip('"', r, r, InfoSyntaxError) then
    begin
      // we want to give back the constant in quotes
      Expression := '"' + StringResult + '"';
      Remaining := r;
    end;
  end

  // string constant delimited by "'" ?
  else if (length(s0) > 0) and (s0[1] = '''') then
  begin
    r := copy(s0, 2, length(s0) - 1);
    GetWord(r, StringResult, r, ['''']);
    if skip('''', r, r, InfoSyntaxError) then
    begin
      // we want to give back the constant in quotes
      Expression := ''''+StringResult+'''';
      Remaining := r;
    end;
  end
  else
    GetWord(s, Expression, Remaining, WordDelimiterSet, searchbackward);
end;


end.

