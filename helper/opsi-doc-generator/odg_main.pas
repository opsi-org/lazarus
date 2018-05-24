unit odg_main;

{$mode objfpc}{$H+}


interface


uses
  FileUtil,
  odg_os_deffunc,
  oslog,
  odg_asciidoc,
  process,
  LCLIntf,
  Classes,
  SysUtils;

function convertOslibToAsciidoc(filename : string): boolean;
function save_compile_show(filename : string) : boolean;

var
  sourcelist : TStringlist;
  targetlist : TStringlist;


implementation

function convertOslibToAsciidoc(filename : string): boolean;
begin
  result := parseInput_opsiscriptlibrary(filename);
  writeDocToList;
end;

function writeToAsciidocTempFile(infilename : string) : string;
var
  tmpfilename, tmpfilebasename, tmpdirname : string;
begin
  result := '';
  tmpdirname := GetTempDir;
  tmpfilebasename := ExtractFileName(infilename);
  tmpfilebasename := ExtractFileNameWithoutExt(tmpfilebasename);
  tmpfilename := tmpdirname+PathDelim+ tmpfilebasename+'.asciidoc';
  targetlist.SaveToFile(tmpfilename);
  result := tmpfilename;
end;

function callasciidoctor(filename : string): boolean;
var
  output : string;
  myasciidoc : string;
begin
  result := false;
  {$ifdef WINDOWS}
  if not RunCommand('cmd.exe',['/c' ,'where asciidoctor.'],output) then
   writeln('Could not find asciidoctor binary')
  else
  begin
    myasciidoc := output;
    myasciidoc := myasciidoc.Replace(#13#10,'');
    myasciidoc := myasciidoc.Replace('''','');
  end;
  {$endif WINDOWS}
  //if not RunCommand('cmd.exe',['/c',myasciidoc,'--backend xhtml5',tmpfilename],output) then
  if not RunCommand('cmd.exe',['/c '+myasciidoc+' --backend xhtml5 '+filename],output) then
  begin
   writeln('Call asciidoctor failed');
   writeln(output);
  end
  else
  result := true;
end;

procedure openhtml(filename : string);
  begin
    OpenDocument(filename);
  end;


function save_compile_show(filename : string) : boolean;
var
  asciidocfile : string;
begin
  asciidocfile := writeToAsciidocTempFile(filename);
  if not callasciidoctor(asciidocfile) then
    writeln('callasciidoctor failed')
  else
    OpenDocument(ExtractFileNameWithoutExt(asciidocfile)+'.html');
end;



initialization
  sourcelist := TStringlist.create;
  targetlist := TStringlist.create;
  LogDatei := TLogInfo.Create;
  logdatei.CreateTheLogfile('opsi-doc-gen.log');

finalization
  sourcelist.Free;
  targetlist.Free;

end.

