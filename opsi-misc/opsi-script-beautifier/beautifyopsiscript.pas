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

function indentation ( indent: Integer) : String;
function isStartStr(line:String; codeWords:TStringList) : boolean;
function beautify (code: TStringlist) : TStringList;
procedure initialize(bfn: String;osf:String);

implementation

var
  decIndentList, incIndentList, decIncIndentList,
            donttouchList:  TStringlist;
  indentrange: Integer; // how many chars for indention
  indentlevel: Integer; // how often indentrange
  indentchar: String;

  dontTouchKeyValue : TList;


function indentation ( indent: Integer) : String;
 var i:Integer;
 begin
   indentation:= '';
   for i:=0 to indent*indentrange-1 do
     indentation:= indentation + indentchar;
 end;

function isStartStr(line:String; codeWords:TStringList) : boolean;
var i:integer;

begin
  isStartStr:=false;
  for i:=0 to codeWords.Count-1 do
    begin
      if (AnsiStartsText(codeWords[i],line)) then
      begin
        isStartStr:=true;
        exit;
      end;
     end;
end;

function createBlockIndent (blockindent:Integer) : String;
var i: integer;
begin
  createBlockIndent:='';
  for i:=1 to blockindent do
    begin
      createBlockIndent:= createBlockIndent + indentchar;
    end
end;


function beautify (code: TStringlist) : TStringList;
var
    k, relPos, i: integer;
    trimLine: boolean;
    tmpstr, tmpstr2 : string;
    found : boolean;
    threetabs: boolean;
    openif : array[0..250] of Integer;
begin
  //Initialize openif array
  for i := 0 to 250 do openif[i] := -1;
  k:=0;
  while (k < pred(code.Count)) do
    begin
      trimLine:= true;
      relPos:=0;
      if AnsiStartsStr(';', UpperCase(code[k]).trim) then
      begin
        trimLine:=false;
        // change 05/2021 - indentation also on comments
        //if not(code[k].Chars[0]=';') then
        code[k]:= indentation(indentlevel) + code[k].trim;
        // else: if first Char=';' don't indent
      end;

      // TODO:
      if AnsiContainsStr(UpperCase(code[k]),UpperCase('[opsiServiceCall')) then
      begin
        // detect indentiation
        threetabs:=false;
        relPos:= PosEx('[',code[k])-1;      // how many chars
        code[k]:= indentation(indentlevel) + code[k].trim;
        relPos:= PosEx('[',code[k])-1-relPos;  // how many chars to indent additionally
        // TODO: nach "params" 1 (schöner 3) Einrückung
        repeat
          inc(k);
          code[k]:= createBlockIndent(relPos) + code[k].trim;
          if code[k].Contains('param') then
          begin
            relPos:=relPos+3;
            threetabs:=true;
          end;
          if (threetabs and code[k].Contains('#9#9#9]')) then
          begin
            threetabs:= false;
            relPos:=relPos-3;
          end;
        until AnsiEndsStr(']',code[k]) or (k>=code.Count-1);
        trimLine:=false;               // last line should not be trimmed
      end;

      tmpstr := UpperCase(code[k].trim);


      if AnsiContainsStr(UpperCase(code[k]),UpperCase('[LinkFolder')) then
      begin
        // detect indentiation
        relPos:= PosEx('[',code[k])-1;      // how many chars
        code[k]:= indentation(indentlevel) + code[k].trim;
        LogDatei.log(' Line [LinkFolder '+inttostr(k+1) + ' : ' + code[k],LLnotice);
        relPos:= PosEx('[',code[k])-1-relPos;  // how many chars to indent additionall
        // normal keine Einrückung; nach set_link 1 Einrückung, vor end_link 1 Ausrückung
        repeat
          inc(k);
          code[k]:= createBlockIndent(relPos) + code[k].trim;
          if code[k].Contains('set_link') then
            relPos:=relPos+1;
          if code[k].Contains('end_link') then
            relPos:=relPos-1;
        until AnsiEndsStr(']',code[k]) or (k>=code.Count-1);
        trimLine:=false;               // last line should not be trimmed
      end;

      // sections with relativ indentions
      // except previous condions
      if not(AnsiStartsStr(UpperCase('[Action'),tmpstr) or  AnsiStartsStr(UpperCase('[Sub'),tmpstr)
         or AnsiStartsStr(UpperCase('[Initial'),tmpstr) or  AnsiStartsStr(UpperCase('[Patches'),tmpstr)
         or AnsiStartsStr(UpperCase('[xml2'),tmpstr) or  AnsiStartsStr(UpperCase('[PatchHosts'),tmpstr)
         or AnsiStartsStr(UpperCase('[Registry'),tmpstr)  or  AnsiStartsStr(UpperCase('[Files'),tmpstr)
         or AnsiStartsStr(UpperCase('[LinkFolder'),tmpstr) or AnsiStartsStr(UpperCase('[shellinanicon'),tmpstr)
         or AnsiStartsStr(UpperCase('[Aktionen'),tmpstr) or  AnsiStartsStr(UpperCase('[ProfileActions'),tmpstr))  then
         if (AnsiStartsStr('[',code[k].trim) and AnsiEndsStr(']',code[k].trim))
         then
            begin
              logdatei.log('Sections - line ' + k.toString + ': dont touch: ' + code[k].trim , LLessential);
              relPos:= PosEx('[',code[k])-1;      // how many chars
              code[k]:= indentation(indentlevel) + code[k].trim;
              //logdatei.log(' line ' + k.toString + ': dont touch: ' + code[k] , LLessential);
              relPos:= PosEx('[',code[k])-1-relPos;  // how many chars to indent additionally
              inc(k);
              while not((AnsiStartsStr('[',code[k].trim) and AnsiEndsStr(']',code[k].trim)) or AnsiStartsStr('endfunc', code[k].trim)) and (k<code.Count-1) do
                begin
                  code[k]:= createBlockIndent(relPos) + code[k];
                  inc(k);
                  //logdatei.log(' : line ' + k.toString + ': dont touch: ' + code[k] , LLessential);
                end;
              trimline:=false;
              // section ends
              if AnsiStartsStr(UpperCase('endfunc'), UpperCase(code[k].trim)) then
              begin
                dec(indentlevel);
                code[k]:= indentation(indentlevel) + code[k];
              end;

              if (AnsiStartsStr('[',code[k].trim) and AnsiEndsStr(']',code[k].trim)) then
              // begin next section, therefore end of previous section
              begin
                dec(k); // back, because this line is the begin of the next section and must be prooved
              end;
            end;

      // trim or not
      if trimLine then
      begin
        code[k]:=code[k].trim;
        if (isStartStr(code[k],incIndentList))
        then
          begin
            code[k]:= indentation(indentlevel) + code[k];
            if AnsiStartsStr(UpperCase('if'),UpperCase(code[k].Trim)) then
            begin
              openif[indentlevel] := k;
            end;
            inc(indentlevel);
          end
        else if (isStartStr(code[k], decIncIndentList))
        then
          begin
            dec(indentlevel);
            code[k]:= indentation(indentlevel) + code[k];
            inc(indentlevel)
          end
        else if (isStartStr(code[k],decIndentList))
        then
        begin
          dec(indentlevel);
          code[k]:= indentation(indentlevel) + code[k];
          tmpstr2:= code[k];
          //LogDatei.log(' Line '+inttostr(k+1) + ': ' + tmpstr2,LLnotice);
          //if (indentlevel>0)
          //then
          //begin
          if AnsiStartsStr(UpperCase('endif'),UpperCase(code[k].Trim)) then
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
          code[k]:= indentation(indentlevel) + code[k];
        inc(k);
      end // if trimLine
      else
        inc(k);
    end; // while
  beautify:=code;
  LogDatei.log('List of open if: ',LLnotice);
  for i := 0 to 250 do
    begin
      if openif[i] > -1 then
       LogDatei.log('open if for indentlevel: '+inttostr(i)+' at Line: '+inttostr(openif[i]),LLnotice);
    end;
end;

procedure initialize(bfn: String;osf:String);
var opsiscriptfile: String;
    opsiscriptcode: TStringlist;
    ini: TINIFile;
    i: Integer;
begin
  ini    := TINIFile.Create(bfn);
  indentrange := INI.ReadInteger('beautifierconf', 'indentrange', 5);
  indentlevel :=  0;
  {$IFNDEF GUI}writeln('indentrange:  ' + indentrange.ToString()); {$ENDIF GUI}
  logdatei.log('indentrange:  ' + indentrange.ToString(), LLessential);
  {$IFNDEF GUI} writeln('indentlevel:  ' + indentlevel.ToString());{$ENDIF GUI}
  logdatei.log('indentlevel:  ' + indentlevel.ToString(), LLessential);


  // Tab = #09, Whitespace = ' '
  indentchar := INI.ReadString('beautifierconf', 'indentchar', '#09');
  {$IFNDEF GUI}writeln(indentchar);{$ENDIF GUI}
  logdatei.log('indentchar:  ' + indentchar, LLessential);
  if indentchar.Equals('tab')
     then indentchar := #09
     else if indentchar.Equals('whitespace')
        then indentchar := ' '
        else indentchar := #09;

  incIndentList:=TStringList.Create;
  decIndentList:=TStringList.Create;
  decIncIndentList:=TStringList.Create;

  //dontTouchList :=TStringList.Create;

  // einlesen der Stringlisten
  ini.ReadSection('incIndentList',incIndentList);
  ini.ReadSection('decIndentList',decIndentList);
  ini.ReadSection('decIncIndentList',decIncIndentList);

  //ini.ReadSection('dontTouchList',dontTouchList);

  // TODO: diese Listen werden im Moment noch nicht verwendet
  // erste Versuche sind hart vercoded in der beautify-prozedur
  // hier nur der Test des Einlesens der don't Touch-Listen auf eine key_valueCollection
  //donttouchBeginEndList:=TStringList.Create;
  //donttouchEndList:=TStringList.Create;
  {
  ini.ReadSection('donttouchBeginEndList',donttouchBeginEndList);    // read as keys donttouchbegin
  dontTouchKeyValue :=TList.Create;
  logdatei.log('donttouch key value list: ',LLessential);
  for i:=0 to donttouchBeginEndList.Count-1 do
    begin
      dontTouchKeyValue.Add;
      dontTouchKeyValue.Items[i].key:='[' + donttouchBeginEndList[i];
      dontTouchKeyValue.Items[i].value :=INI.ReadString('donttouchBeginEndList', donttouchBeginEndList[i], '');
      logdatei.log('key: '+ dontTouchKeyValue.Items[i].key + ' /// value: ' + dontTouchKeyValue.Items[i].value,LLessential);
    end;
  }
  //ini.ReadSection('donttouchEndList',donttouchEndList);
  //
  //donttouchBeginList.Add('[ShellInAnIcon');
  //donttouchEndList.Add('exit');
  //
  // donttouchBeginList.Add('[opsiservicecall');
  // EndList aber am Ende der Zeile ]

  ini.Free;

  opsiscriptfile:=osf;
  opsiscriptcode:= TStringList.Create;
  if FileExists(opsiscriptfile) then
    begin
      logdatei.log('backup file: '+ opsiscriptfile,LLessential);
      CopyFile(opsiscriptfile,ExtractFileNameWithoutExt(opsiscriptfile)+'.bak',[cffOverwriteFile]);
      logdatei.log('opening file: '+ opsiscriptfile,LLessential);
      {$IFNDEF GUI}writeln('opening file: '+  opsiscriptfile);{$ENDIF GUI}
      try
        try
          opsiscriptcode.LoadFromFile(opsiscriptfile);
          //logdatei.log('write backup file: '+ opsiscriptfile + '.bak',LLessential);
          //opsiscriptcode.SaveToFile(opsiscriptfile + '.bak');
          beautify(opsiscriptcode).SaveToFile(opsiscriptfile);
          logdatei.log('write beautified file: '+ opsiscriptfile,LLessential);
         except
          logdatei.log('error beautifying or writing opsiscriptfiles : '+ opsiscriptfile,LLessential);
         end;
      finally
        opsiscriptcode.Free;
      end;
    end
  else
    begin
      opsiscriptcode.Free;
      {$IFNDEF GUI}writeln('file does not exist: '+ opsiscriptfile); {$ENDIF GUI}
      logdatei.log('file does not exist: '+ opsiscriptfile,LLessential);
    end;
  // free all
  incIndentList.Free;
  decIndentList.Free;
  decIncIndentList.Free;
  //dontTouchKeyValue.Free;
end;

end.

