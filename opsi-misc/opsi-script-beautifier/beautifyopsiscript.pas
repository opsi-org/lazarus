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
function isNewSection(line: string; const linenum: integer): boolean;
function beautify(code: TStringList): TStringList;
procedure Initialize(bfn: string; osf: string; use_stdinout: boolean = False);

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

function isNewSection(line: string; const linenum: integer): boolean;
begin
  isNewSection := False;
  if (AnsiStartsStr('[', line.trim) and AnsiEndsStr(']', line.trim)) or
    AnsiStartsStr(UpperCase('deffunc'), UpperCase(line.trim)) then
  begin
    LogDatei.log('New section block found at linenr: ' + IntToStr(linenum) +
      ' : ' + line, LLdebug);
    isNewSection := True;
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

function countLeadingWhitespaces(const line: string): integer;
var
  nonwhitefound: boolean;
  index: integer = 0;
  aktchar: char;
begin
  countLeadingWhitespaces := 0;
  nonwhitefound := False;
  if line.Length > 0 then
    repeat
      Inc(index);
      aktchar := line[index];
      if (aktchar = chr(9)) or (aktchar = chr(32)) then
        Inc(countLeadingWhitespaces)
      else
        nonwhitefound := True;
    until nonwhitefound;
end;

function beautify(code: TStringList): TStringList;
var
  k, relPos, i: integer;
  leadheader, leadline: integer;
  leadheaderstring, sectionname: string;
  dontTouch, noIndent, trimLine: boolean;
  tmpstr, tmpstr2: string;
  threetabs: boolean;
  openif: array[0..250] of integer;
  codeline: string;
begin
  //Initialize openif array
  for i := 0 to 250 do
    openif[i] := -1;
  dontTouch := False;
  noIndent := False;
  k := 0;
  while (k < pred(code.Count)) do
  begin
    trimLine := True;

    relPos := 0;
    codeline := code[k];
    if AnsiStartsStr(';', UpperCase(code[k]).trim) then
    begin
      // 05/2021 - indentation also on comments
      code[k] := indentation(indentlevel) + code[k].trim;
    end;

    if AnsiContainsStr(UpperCase(code[k]), UpperCase('[opsiServiceCall')) then
    begin
      // detect indentiation
      threetabs := False;
      relPos := PosEx('[', code[k]) - 1;      // before indentation
      code[k] := indentation(indentlevel) + code[k].trim;
      LogDatei.log('Section - line ' + IntToStr(k + 1) + ': [opsiServiceCall: ' +
        code[k], LLnotice);
      relPos := PosEx('[', code[k]) - 1 - relPos;  // before indentation
      // how many chars to indent additionally
      repeat
        Inc(k);
        code[k] := indentation(indentlevel + relPos) + code[k].trim;
        //code[k] := createBlockIndent(relPos) + code[k].trim;
        codeline := code[k];
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
      until isNewSection(code[k], k) or AnsiEndsStr(']', code[k].trim) or
        (k >= code.Count - 1);  // end of params
      if isNewSection(code[k], k) then
        // check this line because it's next section - so dec
        Dec(k);
      trimLine := False;               // no trim
    end;

    tmpstr := UpperCase(code[k].trim);


    if AnsiContainsStr(UpperCase(code[k]), UpperCase('[LinkFolder')) then
    begin
      // detect indentiation
      relPos := PosEx('[', code[k]) - 1;      // how many chars
      code[k] := indentation(indentlevel) + code[k].trim;
      LogDatei.log('Section - line ' + IntToStr(k + 1) + ': [LinkFolder: ' +
        code[k], LLnotice);
      relPos := PosEx('[', code[k]) - 1 - relPos;
      // how many chars to indent additionall
      // set_link +1 indent, end_link -1 indent
      repeat
        if code[k].Contains('end_link') then
          relPos := relPos - 1;
        code[k] := createBlockIndent(relPos) + code[k].trim;
        if code[k].Contains('set_link') then
          relPos := relPos + 1;
        Inc(k);
      until isNewSection(code[k], k) // next section
        or (k >= code.Count - 1);
      if isNewSection(code[k], k) then
        // check this line because it's next section - so dec
        Dec(k);
      trimLine := False;  // no trim
    end;

    // sections with relativ indentions, don't touch or no indent
    if isStartStr(code[k].trim, dontTouchList) or
      isStartStr(code[k].trim, noIndentList) then
    begin
      //logdatei.log(' line ' + k.toString + ' before: ' + code[k] , LLessential);
      if isStartStr(code[k].trim, dontTouchList) then
      begin
        logdatei.log('Section - line ' + IntToStr(k + 1) +
          ': don''t touch: ' + code[k], LLnotice);
        dontTouch := True;
      end;
      if isStartStr(code[k].trim, noIndentList) then
      begin
        logdatei.log('Section - line ' + IntToStr(k + 1) + ': no indent: ' +
          code[k], LLnotice);
        noIndent := True;
      end;

      if (AnsiStartsStr('[', code[k].trim) and AnsiEndsStr(']', code[k].trim)) then
        // begin of section
      begin
        // set first line of section with indentlevel
        //relPos := PosEx('[', code[k]) - 1;  // position of [ before indentation
        leadheader := countLeadingWhitespaces(code[k]);
        leadheaderstring := copy(code[k], 1, leadheader);
        sectionname := code[k].Trim;
        logdatei.log('leadheader at section: ' + sectionname + ' is: <' +
          leadheaderstring + '> with ' + IntToStr(leadheader) + ' chars', LLDebug);
        code[k] := indentation(indentlevel) + code[k].trim;
        //relPos := PosEx('[', code[k]) - 1 - relPos;
        //relPos := indentlevel;
        //logdatei.log(' line ' + k.toString + ': dont touch: ' + code[k] , LLessential);
        // get next lines
        Inc(k);
        // check following lines while not end of section
        while not ((AnsiStartsStr('[', code[k].trim) and
            AnsiEndsStr(']', code[k].trim)) or
            AnsiStartsStr(UpperCase('deffunc'), UpperCase(code[k].trim)) or
            AnsiStartsStr(UpperCase('endfunc'), UpperCase(code[k].trim)) or
            AnsiStartsStr(UpperCase('exit $?'), UpperCase(code[k].trim))) and
          (k < code.Count - 1) do
        begin
          if dontTouch then // don't touch
          begin
            // We assume that the whole code block has the same indent string (leadheaderstring)
            // at the beginnig - if not we can not help (only hope) and give a warning
            if code[k].trim <> '' then
              if not AnsiStartsStr(leadheaderstring, code[k]) then
                logdatei.log('Inconsistent indentation in section: ' +
                  sectionname + ' at line ' + IntToStr(k + 1) + ' : ' +
                  code[k].trim, LLWarning);
            // we want to indent the whole code block of the section
            // without touching the indents relative to the section header
            // so we remove the number of whitespaces before the section header (leadheader)
            // and add the new indent
            code[k] := createBlockIndent(indentlevel) +
              copy(code[k], leadheader + 1, code[k].Length);
          end;
          if noIndent then // no indent
            code[k] := createBlockIndent(indentlevel) + code[k].trim;
          tmpstr2 := code[k];
          Inc(k);
          //logdatei.log(' : line ' + k.toString + ' after: ' + code[k] , LLessential);
        end;
        trimline := False;

        // section ends
        if (AnsiStartsStr(UpperCase('endfunc'), UpperCase(code[k].trim))) then
        begin
          LogDatei.log('endfunc found at linenr: ' + IntToStr(k) +
            ' : ' + code[k].trim, LLdebug);
          Dec(indentlevel);
          code[k] := indentation(indentlevel) + code[k].trim;
        end;

        if isNewSection(code[k], k) then
          // begin next section, therefore end of previous section
        begin
          Dec(k);
          // back one line, because this line is the begin of the next section and must be prooved
        end;

      end;
      dontTouch := False;
      noIndent := False;
    end;

    // trim and make indentation or not
    if trimLine then
    begin
      code[k] := code[k].trim;
      if (isStartStr(code[k], incIndentList)) then
      begin
        LogDatei.log('Inc ident at linenr: ' + IntToStr(k) + ' : ' +
          code[k].trim, LLdebug);
        code[k] := indentation(indentlevel) + code[k];
        if AnsiStartsStr(UpperCase('if'), UpperCase(code[k].Trim)) then
        begin
          if (indentlevel >= 0) then
            openif[indentlevel] := k
          else
            LogDatei.log(
              'Syntax Error, incorrect indent level - inconsistent if/else/endif/endcase/endswitch around line '
              + IntToStr(k + 1) + ': ' + tmpstr2, LLError);
        end;
        Inc(indentlevel);
      end
      else if (isStartStr(code[k], decIncIndentList)) then
      begin
        LogDatei.log('Dec inc ident at linenr: ' + IntToStr(k) +
          ' : ' + code[k].trim, LLdebug);
        Dec(indentlevel);
        code[k] := indentation(indentlevel) + code[k];
        Inc(indentlevel);
      end
      else if (isStartStr(code[k], decIndentList)) then
      begin
        LogDatei.log('Dec ident at linenr: ' + IntToStr(k) + ' : ' +
          code[k].trim, LLdebug);
        Dec(indentlevel);
        code[k] := indentation(indentlevel) + code[k];
        tmpstr2 := code[k];
        //LogDatei.log(' Line '+inttostr(k+1) + ': ' + tmpstr2,LLnotice);
        if AnsiStartsStr(UpperCase('endif'), UpperCase(code[k].Trim)) then
        begin
          if (indentlevel >= 0) then
            openif[indentlevel] := -1
          else
            LogDatei.log(
              'Syntax Error, incorrect indent level - inconsistent if/else/endif/endcase/endswitch around line '
              + IntToStr(k + 1) + ': ' + tmpstr2, LLError);
        end;
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

procedure Initialize(bfn: string; osf: string; use_stdinout: boolean = False);
var
  opsiscriptfile: string;
  opsiscriptcode: TStringList;
  ini: TINIFile;
  i: integer;
  mystring: string;
begin
  ini := TINIFile.Create(bfn);
  indentrange := INI.ReadInteger('beautifierconf', 'indentrange', 5);
  indentlevel := 0;
  {$IFNDEF GUI}
  if not use_stdinout then writeln('indentrange:  ' + indentrange.ToString());
  {$ENDIF GUI}
  logdatei.log('indentrange:  ' + indentrange.ToString(), LLessential);
  {$IFNDEF GUI}
  if not use_stdinout then writeln('indentlevel:  ' + indentlevel.ToString());
  {$ENDIF GUI}
  logdatei.log('indentlevel:  ' + indentlevel.ToString(), LLessential);


  // Tab = #09, Whitespace = ' '
  indentchar := INI.ReadString('beautifierconf', 'indentchar', '#09');
  {$IFNDEF GUI}
  if not use_stdinout then writeln(indentchar);
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

  if use_stdinout then
  // read from stdin and write to stdout
  begin
    opsiscriptcode := TStringList.Create;
    while not EOF do
    begin
      ReadLn(mystring);
      opsiscriptcode.Add(mystring);
    end;
    try
      try
        opsiscriptcode.Text := beautify(opsiscriptcode).Text;
        for i := 0 to opsiscriptcode.Count - 1 do writeln(opsiscriptcode[i]);
      except
        logdatei.log('error beautifying  ', LLessential);
      end;
    finally
      opsiscriptcode.Free;
    end;
  end
  else
    // use file
  begin

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
  end;
  // free all
  incIndentList.Free;
  decIndentList.Free;
  decIncIndentList.Free;
  dontTouchList.Free;
  noIndentList.Free;
end;

end.
