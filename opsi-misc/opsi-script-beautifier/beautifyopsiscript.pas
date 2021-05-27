unit beautifyopsiscript;
// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: martina hammel

{$mode objfpc}{$H+}

interface


uses
  Classes, SysUtils, StrUtils, oslog, key_valueCollection,
  fileutil,
  LazFileUtils,
  INIFiles;

function indentation(indent: integer): string;
function isStartStr(line: string; codeWords: TStringList): boolean;
function isEndStr(line: string; codeWords: TStringList): boolean;
function beautify(code: TStringList): TStringList;
procedure Initialize(bfn: string; osf: string);

implementation

var
  decIndentList, incIndentList, decIncIndentList, dontTouchList,
  noIndentList: TStringList;
  indentrange: integer; // how many chars for indention
  indentlevel: integer; // how often indentrange
  indentchar: string;


function indentation(indent: integer): string;
var
  i: integer;
begin
  indentation := '';
  for i := 0 to indent * indentrange - 1 do
    indentation := indentation + indentchar;
end;

function isStartStr(line: string; codeWords: TStringList): boolean;
var
  i: integer;

begin
  isStartStr := False;
  for i := 0 to codeWords.Count - 1 do
  begin
    if (AnsiStartsText(codeWords[i], line)) then
    begin
      isStartStr := True;
      exit;
    end;
  end;
end;

function isEndStr(line: string; codeWords: TStringList): boolean;
var
  i: integer;

begin
  isEndStr := False;
  for i := 0 to codeWords.Count - 1 do
  begin
    if (AnsiStartsText(codeWords.ValueFromIndex[i], line)) then
    begin
      isEndStr := True;
      exit;
    end;
  end;
end;

function createBlockIndent(blockindent: integer): string;
var
  i: integer;
begin
  createBlockIndent := '';
  for i := 1 to blockindent do
  begin
    createBlockIndent := createBlockIndent + indentchar;
  end;
end;


function beautify(code: TStringList): TStringList;
var
  k, relPos, i: integer;
  dontTouch, noIndent, trimLine: boolean;
  tmpstr, tmpstr2: string;
  found: boolean;
  threetabs: boolean;
  openif: array[0..250] of integer;
begin
  //Initialize openif array
  for i := 0 to 250 do
    openif[i] := -1;
  dontTouch:=false;
  noIndent:=false;
  k := 0;
  while (k < pred(code.Count)) do
  begin
    trimLine := True;

    relPos := 0;
    if AnsiStartsStr(';', UpperCase(code[k]).trim) then
    begin
      //trimLine := False;
      // change 05/2021 - indentation also on comments
      //if not(code[k].Chars[0]=';') then
      code[k] := indentation(indentlevel) + code[k].trim;
      // else: if first Char=';' don't indent
    end;

    if AnsiContainsStr(UpperCase(code[k]), UpperCase('[opsiServiceCall')) then
    begin
      // detect indentiation
      threetabs := False;
      relPos := PosEx('[', code[k]) - 1;      // how many chars
      code[k] := indentation(indentlevel) + code[k].trim;
      relPos := PosEx('[', code[k]) - 1 - relPos;
      // how many chars to indent additionally
      // TODO: nach "params" 1 (schöner 3) Einrückung
      repeat
        Inc(k);
        code[k] := createBlockIndent(relPos) + code[k].trim;
        if code[k].Contains('param') then
        begin
          relPos := relPos + 3;
          threetabs := True;
        end;
        if (threetabs and code[k].Contains('#9#9#9]')) then
        begin
          threetabs := False;
          relPos := relPos - 3;
        end;
      until AnsiEndsStr(']', code[k]) or (k >= code.Count - 1);
      trimLine := False;               // last line should not be trimmed
    end;

    tmpstr := UpperCase(code[k].trim);


    if AnsiContainsStr(UpperCase(code[k]), UpperCase('[LinkFolder')) then
    begin
      // detect indentiation
      relPos := PosEx('[', code[k]) - 1;      // how many chars
      code[k] := indentation(indentlevel) + code[k].trim;
      LogDatei.log(' Line [LinkFolder ' + IntToStr(k + 1) + ' : ' + code[k], LLnotice);
      relPos := PosEx('[', code[k]) - 1 - relPos;
      // how many chars to indent additionall
      // normal keine Einrückung; nach set_link 1 Einrückung, vor end_link 1 Ausrückung
      repeat
        Inc(k);
        code[k] := createBlockIndent(relPos) + code[k].trim;
        if code[k].Contains('set_link') then
          relPos := relPos + 1;
        if code[k].Contains('end_link') then
          relPos := relPos - 1;
      until AnsiEndsStr(']', code[k]) or (k >= code.Count - 1);
      trimLine := False;               // last line should not be trimmed
    end;

   // sections with relativ indentions, dont touch and no indent
   if isStartStr(code[k], dontTouchList) or isStartStr(code[k], noIndentList)
         (*not(AnsiStartsStr(UpperCase('[Action'),tmpstr) or  AnsiStartsStr(UpperCase('[Sub'),tmpstr)
         or AnsiStartsStr(UpperCase('[Initial'),tmpstr) or  AnsiStartsStr(UpperCase('[Patches'),tmpstr)
         or AnsiStartsStr(UpperCase('[xml2'),tmpstr) or  AnsiStartsStr(UpperCase('[PatchHosts'),tmpstr)
         or AnsiStartsStr(UpperCase('[Registry'),tmpstr)  or  AnsiStartsStr(UpperCase('[Files'),tmpstr)
         or AnsiStartsStr(UpperCase('[LinkFolder'),tmpstr) or AnsiStartsStr(UpperCase('[shellinanicon'),tmpstr)
         or AnsiStartsStr(UpperCase('[Aktionen'),tmpstr) or  AnsiStartsStr(UpperCase('[ProfileActions'),tmpstr))*) then
    begin
      //logdatei.log(' line ' + k.toString + ' before: ' + code[k] , LLessential);
      if isStartStr(code[k], dontTouchList) then
      begin
        logdatei.log('Sections - line ' + k.toString +
          ': dont touch: ' + code[k].trim, LLessential);
        dontTouch:= true;
      end
      else // no indent
      begin
        logdatei.log('Sections - line ' + k.toString +
          ': no indent: ' + code[k].trim, LLessential);
        noIndent:=true;
      end;

      if (AnsiStartsStr('[', code[k].trim) and AnsiEndsStr(']', code[k].trim)) then
      begin
        relPos := PosEx('[', code[k]) - 1;      // how many chars
        //logdatei.log(' line ' + k.toString + ': dont touch: ' + code[k] , LLessential);
        relPos := PosEx('[', code[k]) - 1 - relPos;
        // how many chars to indent additionally
        Inc(k);
        // check following lines
        while not ((AnsiStartsStr('[', code[k].trim) and
            AnsiEndsStr(']', code[k].trim)) or
            AnsiStartsStr('endfunc', code[k].trim)) and
          (k < code.Count - 1) do
        begin
          if dontTouch then // don't touch
            code[k] := createBlockIndent(relPos) + code[k];
          if noIndent then // no indent
            code[k] := createBlockIndent(relPos) + code[k].trim;
          tmpstr2 := code[k];
          Inc(k);
          //logdatei.log(' : line ' + k.toString + ' after: ' + code[k] , LLessential);
        end;
        trimline := False;
        // section ends
        if AnsiStartsStr(UpperCase('endfunc'), UpperCase(code[k].trim)) then
        begin
          Dec(indentlevel);
          code[k] := indentation(indentlevel) + code[k];
        end;
        if (AnsiStartsStr('[', code[k].trim) and AnsiEndsStr(']', code[k].trim)) then
          // begin next section, therefore end of previous section
        begin
          Dec(k);
          // back, because this line is the begin of the next section and must be prooved
        end;

      end;
      dontTouch:=false;
      noIndent:=false;
    end;

    // trim and make indentation or not
    if trimLine then
    begin
      code[k] := code[k].trim;
      if (isStartStr(code[k], incIndentList)) then
      begin
        code[k] := indentation(indentlevel) + code[k];
        if AnsiStartsStr(UpperCase('if'), UpperCase(code[k].Trim)) then
        begin
          openif[indentlevel] := k;
        end;
        Inc(indentlevel);
      end
      else if (isStartStr(code[k], decIncIndentList)) then
      begin
        Dec(indentlevel);
        code[k] := indentation(indentlevel) + code[k];
        Inc(indentlevel);
      end
      else if (isStartStr(code[k], decIndentList)) then
      begin
        Dec(indentlevel);
        code[k] := indentation(indentlevel) + code[k];
        tmpstr2 := code[k];
        //LogDatei.log(' Line '+inttostr(k+1) + ': ' + tmpstr2,LLnotice);
        //if (indentlevel>0)
        //then
        //begin
        if AnsiStartsStr(UpperCase('endif'), UpperCase(code[k].Trim)) then
        begin
          openif[indentlevel] := -1;
        end;
        //end
        //else
        //begin
        //  LogDatei.log('endif error - too many endifs at Line: '+ inttostr(k+1),LLnotice);
        //  code[k]:= ';' + indentation(indentlevel) + code[k];
        //end
      end
      else
        code[k] := indentation(indentlevel) + code[k];
      Inc(k);
    end // if trimLine
    else
      Inc(k);
  end; // while
  beautify := code;
  LogDatei.log('List of open if: ', LLnotice);
  for i := 0 to 250 do
  begin
    if openif[i] > -1 then
      LogDatei.log('open if for indentlevel: ' + IntToStr(i) +
        ' at Line: ' + IntToStr(openif[i]), LLnotice);
  end;
end;

procedure Initialize(bfn: string; osf: string);
var
  opsiscriptfile: string;
  opsiscriptcode: TStringList;
  ini: TINIFile;
  i: integer;
begin
  ini := TINIFile.Create(bfn);
  indentrange := INI.ReadInteger('beautifierconf', 'indentrange', 5);
  indentlevel := 0;
  {$IFNDEF GUI}
  writeln('indentrange:  ' + indentrange.ToString());
{$ENDIF GUI}
  logdatei.log('indentrange:  ' + indentrange.ToString(), LLessential);
  {$IFNDEF GUI}
  writeln('indentlevel:  ' + indentlevel.ToString());
{$ENDIF GUI}
  logdatei.log('indentlevel:  ' + indentlevel.ToString(), LLessential);


  // Tab = #09, Whitespace = ' '
  indentchar := INI.ReadString('beautifierconf', 'indentchar', '#09');
  {$IFNDEF GUI}
  writeln(indentchar);
{$ENDIF GUI}
  logdatei.log('indentchar:  ' + indentchar, LLessential);
  if indentchar.Equals('tab') then
    indentchar := #09
  else if indentchar.Equals('whitespace') then
    indentchar := ' '
  else
    indentchar := #09;

  incIndentList := TStringList.Create;
  decIndentList := TStringList.Create;
  decIncIndentList := TStringList.Create;
  dontTouchList := TStringList.Create;
  noIndentList := TStringList.Create;
  // read lists
  ini.ReadSection('incIndentList', incIndentList);
  ini.ReadSection('decIndentList', decIndentList);
  ini.ReadSection('decIncIndentList', decIncIndentList);
  // the following lists have to get [ at the beginning of each element
  ini.ReadSection('dontTouchList', dontTouchList);
  ini.ReadSection('noINdentList', noIndentList);

  for i := 0 to dontTouchList.Count - 1 do
  begin
    dontTouchList[i] := '[' + dontTouchList[i];
    //logdatei.log('dontTouchList item: ' + dontTouchList[i], LLnotice);
  end;
  for i := 0 to noIndentList.Count - 1 do
  begin
    noIndentList[i] := '[' + noIndentList[i];
    //logdatei.log('noIndentList item: ' + noIndentList[i], LLnotice);
  end;


  ini.Free;

  opsiscriptfile := osf;
  opsiscriptcode := TStringList.Create;
  if FileExists(opsiscriptfile) then
  begin
    logdatei.log('backup file: ' + opsiscriptfile, LLessential);
    CopyFile(opsiscriptfile, ExtractFileNameWithoutExt(opsiscriptfile) +
      '.bak', [cffOverwriteFile]);
    logdatei.log('opening file: ' + opsiscriptfile, LLessential);
      {$IFNDEF GUI}
    writeln('opening file: ' + opsiscriptfile);
{$ENDIF GUI}
    try
      try
        opsiscriptcode.LoadFromFile(opsiscriptfile);
        //logdatei.log('write backup file: '+ opsiscriptfile + '.bak',LLessential);
        //opsiscriptcode.SaveToFile(opsiscriptfile + '.bak');
        beautify(opsiscriptcode).SaveToFile(opsiscriptfile);
        logdatei.log('write beautified file: ' + opsiscriptfile, LLessential);
      except
        logdatei.log('error beautifying or writing opsiscriptfiles : ' +
          opsiscriptfile, LLessential);
      end;
    finally
      opsiscriptcode.Free;
    end;
  end
  else
  begin
    opsiscriptcode.Free;
      {$IFNDEF GUI}
    writeln('file does not exist: ' + opsiscriptfile);
{$ENDIF GUI}
    logdatei.log('file does not exist: ' + opsiscriptfile, LLessential);
  end;
  // free all
  incIndentList.Free;
  decIndentList.Free;
  decIncIndentList.Free;
  dontTouchList.Free;
  noIndentList.Free;
end;

end.
