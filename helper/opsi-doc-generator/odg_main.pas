unit odg_main;

{$mode objfpc}{$H+}


interface


uses
  fileinfo,
  winpeimagereader, // {need this for reading exe info}
  elfreader, // {needed for reading ELF executables}
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
  myversion : string;
  FileVerInfo : TFileVersionInfo;


implementation

function convertOslibToAsciidoc(filename : string): boolean;
begin
  result := parseInput_opsiscriptlibrary(filename);
  writeDocToList;
end;

function writeToAsciidocFile(infilename : string) : string;
var
  tmpfilename, tmpfilebasename, tmpdirname : string;
begin
  result := '';
  //tmpdirname := GetTempDir;
  tmpdirname := ExtractFileDir(infilename);
  tmpfilebasename := ExtractFileName(infilename);
  tmpfilebasename := ExtractFileNameWithoutExt(tmpfilebasename);
  tmpfilename := tmpdirname+PathDelim+ tmpfilebasename+'.asciidoc';
  targetlist.SaveToFile(tmpfilename);
  result := tmpfilename;
  LogDatei.log('Wrote asciidoc file to: '+tmpfilename,LLinfo);
end;

function callasciidoctor(filename : string): boolean;
var
  output : string;
  myasciidoc : string;
  shell, shelloption, optionstr : string;
begin
  result := false;
  {$ifdef WINDOWS}
  if not RunCommand('cmd.exe',['/c' ,'where asciidoctor.'],output) then
  begin
    writeln('Could not find asciidoctor binary');
    LogDatei.log('Could not find asciidoctor binary',LLcritical);
  end
  else
  begin
    myasciidoc := output;
    myasciidoc := myasciidoc.Replace(#13#10,'');
    myasciidoc := myasciidoc.Replace('''','');
    shell := 'cmd.exe';
    shelloption := '/c';
  end;
  LogDatei.log('Try to call asciidoctor binary',LLnotice);
  if not RunCommand(shell,[shelloption+' '+myasciidoc+' --backend xhtml5 '+filename],output) then
  {$endif WINDOWS}
  {$ifdef LINUX}
  shell := '/bin/bash';
  shelloption := '-c ';
  if not RunCommand('/usr/bin/which',['asciidoctor'],output) then
   writeln('Could not find asciidoctor binary')
  else
  begin
    myasciidoc := output;
    myasciidoc := myasciidoc.Replace(#10,'');
    myasciidoc := myasciidoc.Replace('''','');
  end;
  optionstr := '--backend xhtml5 '+ExtractFileName(filename);
  writeln('calling: indir: '+ ExtractFileDir(filename));
  writeln('calling: : '+ myasciidoc+' '+optionstr);
  LogDatei.log('calling: indir: '+ ExtractFileDir(filename),LLnotice);
  LogDatei.log('calling: '+ myasciidoc+' '+optionstr,LLnotice);
  if not RunCommand(myasciidoc,['--backend','xhtml5',filename],output) then
  //if not RunCommandIndir(ExtractFileDir(filename),myasciidoc,[optionstr],output) then
  //if not RunCommand(myasciidoc,['--backend xhtml5 ','"'+filename+'"'],output) then
  //if not RunCommand(shell,[shelloption+''''+myasciidoc+' --backend xhtml5 '+filename+''''],output) then
  {$endif LINUX}
  //if not RunCommand(shell,[shelloption+' '+myasciidoc+' --backend xhtml5 '+filename],output) then
  begin
   writeln('Call asciidoctor failed');
   writeln(output);
   LogDatei.log('Call asciidoctor failed: '+output,LLcritical);
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
  asciidocfile := writeToAsciidocFile(filename);
  if not callasciidoctor(asciidocfile) then
  begin
    writeln('callasciidoctor failed');
    LogDatei.log('callasciidoctor failed',LLcritical);
  end
  else
    OpenDocument(ExtractFileNameWithoutExt(asciidocfile)+'.html');
end;




initialization
  sourcelist := TStringlist.create;
  targetlist := TStringlist.create;
  LogDatei := TLogInfo.Create;
  logdatei.CreateTheLogfile('opsi-doc-gen.log');
  LogDatei.LogLevel:=8;

  //from http://wiki.freepascal.org/Show_Application_Title,_Version,_and_Company
   FileVerInfo := TFileVersionInfo.Create(nil);
   try
     FileVerInfo.FileName := ParamStr(0);
     FileVerInfo.ReadFileInfo;
     myversion := FileVerInfo.VersionStrings.Values['FileVersion'];
   finally
     FileVerInfo.Free;
   end;

finalization
  sourcelist.Free;
  targetlist.Free;

end.

