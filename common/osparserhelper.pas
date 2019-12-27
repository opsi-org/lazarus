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

  NULL_STRING_VALUE = 'NULL';

type
  TCharset = set of char;

function CutLeftBlanks(const s: string): string;
function CutRightBlanks(const s: string): string;

(*
procedure GetWord
  (const s: string; var Expression, Remaining: string;
  const WordDelimiterSet: TCharset; searchbackward: boolean = False); overload;
*)
procedure GetWord
  (const s: string; var Expression, Remaining: string;
  const WordDelimiterSet: TCharset; searchbackward: boolean = False;
  backwardfirst: boolean = True); overload;


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
  const WordDelimiterSet: TCharset; searchbackward: boolean = False;
    backwardfirst: boolean = True);
// checks if we have a quoted string constant
// if yes it returns the quoted string konstant
// if no it calls getword

procedure GetOuterFunctionOrExp(const s: string; var Expression, Remaining: string);

function opsiunquotestr2(s1,s2 : string): string;
// removes only quotes if they found at start and end
// s2 may be two chars long. Then the first char is the start mark
// and the second char is the end mark
// used by unquote2


function divideAtFirst(const partialS, S: string; var part1, part2: string): boolean;
//  teilt den String S beim ersten Vorkommen des Teilstrings partialS;
//   liefert true, wenn partialS vorkommt,
//   andernfalls false;
//   wenn partialS nicht vorkommt, enthaelt part1 den Gesamtstring, part2 ist leer

procedure stringsplitByWhiteSpace(const s: string; var Result: TStringList);
// produziert eine Stringliste aus den Teilstrings, die zwischen den Whitespace-Abschnitten stehen

procedure stringsplit(const s, delimiter: string; var Result: TStringList);
// produziert eine Stringliste aus den Teilstrings, die zwischen den Delimiter-Strings stehen

procedure stringlistintersection(const inlist1 : Tstringlist; const inlist2 : Tstringlist;
  var list1rest : Tstringlist; var listintersection  : Tstringlist);


implementation



function CutLeftBlanks(const s: string): string;
begin
  Result := SysUtils.trimleft(s);
end;


function CutRightBlanks(const s: string): string;
begin
  Result := SysUtils.trimright(s);
end;


// Expression ist Teilstring von s bis zu einem Zeichen von WordDelimiterSet (ausschliesslich),
// Remaining beginnt mit dem auf dieses Zeichen folgenden Zeichen, wobei fuehrender Whitespace
// eliminiert wird


procedure GetWord
    (const s: string; var Expression, Remaining: string;
    const WordDelimiterSet: TCharset; searchbackward: boolean = False;
    backwardfirst: boolean = True);

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
      if not backwardfirst then
      begin
        // get from "hu)hu)))" as expr "hu)hu" with remaining ")))"
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
      end
      else
      begin
         // get from "hu)hu)))" as expr "hu)hu))" with remaining ")"
        //i := length(t) + 1;
        i := length(t);
        while (i >= 0) and not (t[i] in WordDelimiterSet) do
        begin
          dec(i) // we take the next one
        end;
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
  const WordDelimiterSet: TCharset; searchbackward: boolean = False;
    backwardfirst: boolean = True);
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
    GetWord(s, Expression, Remaining, WordDelimiterSet, searchbackward,backwardfirst);
end;

//https://stackoverflow.com/questions/15294501/how-to-count-number-of-occurrences-of-a-certain-char-in-string
function OccurrencesOfChar(const S: string; const C: char): integer;
var
  i: Integer;
begin
  result := 0;
  for i := 1 to Length(S) do
    if S[i] = C then
      inc(result);
end;

procedure GetOuterFunctionOrExp(const s: string; var Expression, Remaining: string);
var
  openBracketsNum, closeBracketsNum : integer;
  cutpos : integer;
begin
  Expression := '';
  //Remaining := '';
  openBracketsNum := OccurrencesOfChar(s,'(');
  closeBracketsNum := OccurrencesOfChar(s,')');
  if openBracketsNum > 0 then
  begin
    // we have a function here
    if closeBracketsNum > openBracketsNum then
    begin
      // we should cut at the matching close brackets
      //cutpos := NPos(')',s,openBracketsNum);
      // give me the pos of the last matching closing bracket
      cutpos := NPos(')',s,openBracketsNum);
      // get it with the closing bracket
      Expression := copy(s,1,cutpos);
      // get the rest
      Remaining := Remaining + copy(s,cutpos+1,length(s));;
    end
    else
    begin
      // every thing seems ok
      Expression := s;
      //Remaining := '';
    end;
  end
  else
  begin
    if closeBracketsNum > 0 then
    begin
      // we should cut at the first close brackets
      cutpos := NPos(')',s,1);
      Expression := copy(s,1,cutpos-1);
      Remaining := Remaining + copy(s,cutpos,length(s));;
    end
    else
    begin
      // every thing seems ok
      Expression := s;
      //Remaining := '';
    end;
  end;
end;

function opsiunquotestr2(s1,s2 : string): string;
// removes only quotes if they found at start and end
// s2 may be two chars long. Then the first char is the start mark
// and the second char is the end mark
// used by unquote2
var
  markstr ,startmark, endmark : string;
begin
  Result := '';
  markstr := trim(s2);
  if (length(s1) >= 1) and (length(markstr) >= 1) then
  begin
    startmark := markstr[1];
    if length(markstr) >= 2 then endmark := markstr[2] // different marks (brackets) at begin and end
    else endmark := startmark; // the same mark (quote) at begin and end
    if (pos(startmark,s1) = 1) and AnsiEndsStr(endmark,s1) then
      Result := copy(s1,2,length(s1)-2)
    else Result := s1;
  end;
end;

function divideAtFirst(const partialS, S: string; var part1, part2: string): boolean;
  // teilt den String S beim ersten Vorkommen des Teilstrings partialS;
  //   liefert true, wenn partialS vorkommt,
  //   andernfalls false;
  //   wenn partialS nicht vorkommt, enthaelt part1 den Gesamtstring, part2 ist leer
var
  i: integer = 0;
begin
  i := ansipos(lowercase(partialS), lowercase(s));
  if i > 0 then
  begin
    part1 := copy(S, 1, i - 1);
    part2 := copy(S, i + length(partialS), length(S));
    Result := True;
  end
  else
  begin
    part1 := s;
    part2 := '';
    Result := False;
  end;
end;
procedure stringsplitByWhiteSpace(const s: string; var Result: TStringList);
// produziert eine Stringliste aus den Teilstrings, die zwischen den Whitespace-Abschnitten stehen
var
  remainder: string = '';
  item: string = '';
  //found: boolean;
begin
  GetWord(s, item, remainder, WordDelimiterWhiteSpace);
  Result.add(item);
  while remainder <> '' do
  begin
    GetWord(remainder, item, remainder, WordDelimiterWhiteSpace);
    Result.add(item);
  end;
end;



procedure stringsplit(const s, delimiter: string; var Result: TStringList);
// produziert eine Stringliste aus den Teilstrings, die zwischen den Delimiter-Strings stehen

var
  remainder: string = '';
  item: string = '';
  found: boolean;
begin

  found := divideAtFirst(delimiter, s, item, remainder);

  while found do
  begin
    Result.add(item);
    found := divideAtFirst(delimiter, remainder, item, remainder);
  end;

  Result.add(item);
end;



procedure stringlistintersection(const inlist1 : Tstringlist; const inlist2 : Tstringlist;
  var list1rest : Tstringlist; var listintersection  : Tstringlist);
var
  i : integer;
  str : string;
begin
  //str := inlist1.DelimitedText;
  //str := inlist2.DelimitedText;
  list1rest.Clear;
  listintersection.Clear;
  //str := inlist1.DelimitedText;
  //str := inlist2.DelimitedText;
  for i := 0 to inlist1.Count -1 do
  begin
    str := inlist1.Strings[i];
    if inlist2.IndexOf(str) = -1 then
    begin
      // the inlist1 string is not found in inlist2 so it goes to list1rest
      list1rest.Add(str);
    end
    else
    begin
      // the inlist1 string is found in inlist2 so it goes to listintersection
      listintersection.Add(str);
    end;
  end;
end;

end.

