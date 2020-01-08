unit beautifyopsiscript;
// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: martina hammel
// credits: https://www.opsi.org/credits/ --- gibts net

//***************************************************************************
// Subversion:
// $Revision:  $
// $Author: hammel $
// $Date: 2019-07-06 14:15:37 +0100 (Di, 06. Dez 2016) $
//***************************************************************************
{$mode objfpc}{$H+}

interface


uses
  Classes, SysUtils, StrUtils, INIFiles;

function indentation ( indent: Integer) : String;
function isStartStr(line:String; codeWords:TStringList) : boolean;
function beautify (code: TStringlist) : TStringList;
procedure initialize(bfn: String;osf:String);

implementation

var
  decIndentList, incIndentList, decIncIndentList,
    donttouchBeginList, donttouchEndList:  TStringlist;
  indentrange: Integer; // how many chars for indention
  indentlevel: Integer; // how often indentrange
  indentchar: Char;
// TODO: configurationsdatei für die verschiedenen Fälle und die Einrückungsgröße

function indentation ( indent: Integer) : String;
 var i:Integer;
 begin
   indentation:= '';
   for i:=0 to indent*indentrange-1 do
     indentation:= indentation +' ';
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

function beautify (code: TStringlist) : TStringList;
var k: integer;
    trimLine: boolean;
begin
  // zuerst prüfen, ob eine  don't touch section beginnt
  // don't touch
  // kein trim

  // touch
  k:=0;
  trimLine:= true;
  while (k < pred(code.Count)) do
    begin
      if AnsiStartsText('[opsiservicecall',code[k]) OR AnsiStartsText('[ShellInAnIcon', code[k]) then
      // besser contains???
      begin
        trimLine:=false;
      end
      else if AnsiEndsText(']',code[k]) OR AnsiStartsText('exit',code[k]) then
      begin
        trimLine:=true;
      end;

      //
      if trimLine then
      // trim or not
      begin
        code[k]:=code[k].trim;
        if (isStartStr(code[k],incIndentList))
        then
          begin
            code[k]:= indentation(indentlevel) + code[k];
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
        end
        else
          code[k]:= indentation(indentlevel) + code[k];
        inc(k);

      end // if trimLine
      else inc(k);
    end; // while
  beautify:=code;
end;

procedure initialize(bfn: String;osf:String);
var opsiscriptfile: String;
    opsiscriptcode: TStringlist;
    ini: TINIFile;
begin
  ini    := TINIFile.Create(bfn);
  indentrange := INI.ReadInteger('beautifierconf', 'indentrange', 5);
  indentlevel :=  INI.ReadInteger('beautifierconf', 'indentlevel', 0);
  writeln('indentrange:  ' + indentrange.ToString());
  writeln('indentlevel:  ' + indentlevel.ToString());
  incIndentList:=TStringList.Create;
  // einlesen der Stringliste
  //ini.ReadSectionValues('incIndentList',strList);
  incIndentList.Add('if ');
  incIndentList.Add('deffunc ');
  incIndentList.Add('switch ');
  incIndentList.Add('case ');
  incIndentList.Add('defaultcase ');
  ini.Free;


  donttouchBeginList:=TStringList.Create;
  donttouchEndList:=TStringList.Create;

  // die zwei
  donttouchBeginList.Add('[ShellInAnIcon');
  donttouchEndList.Add('exit');

  // die zwei
  donttouchBeginList.Add('[opsiservicecall');
  // EndList aber am Ende der Zeile ]


  decIndentList:=TStringList.Create;
  decIncIndentList:=TStringList.Create;


  decIndentList.Add('endif');
  decIndentList.Add('endfunc');
  decIndentList.Add('endCase');
  decIndentList.Add('endswitch');

  decIncIndentList.Add('else');
  opsiscriptfile:=osf;
  opsiscriptcode:= TStringList.Create;
  if FileExists(opsiscriptfile) then
    begin
      //logdatei.log('opening file: '+opsiscriptfile,myoslog.LLinfo);
      writeln('opening file: '+  opsiscriptfile);
      try
          opsiscriptcode.LoadFromFile(opsiscriptfile);
          beautify(opsiscriptcode).SaveToFile(opsiscriptfile +'.beautified');
      finally
        opsiscriptcode.Free;
      end;
    end;

end;

end.

