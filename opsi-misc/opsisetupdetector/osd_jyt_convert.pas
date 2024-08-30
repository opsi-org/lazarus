unit osd_jyt_convert;

{$mode Delphi}

interface

uses
  Classes, SysUtils,
  oslog,
  LCLType,
  OSProcessux,
  Process,
  lazutf8;

procedure convertJsonFileToTomlFile(infile : string; outfile : string);

var
  myJyt : string;
  myJytDir : string;
  outResultList : Tstringlist;

implementation

procedure convertJsonFileToTomlFile(infile : string; outfile : string);
var
  mycommand : string;
  myexitcode : integer;
  myreport : string;
  str1, str2, str3, str4 : string;


begin
  str1 :=  '/bin/bash -c ';
  str2:= '''' ;
  str3 := '"''';
  mycommand := str1+str2+ '/bin/cat "'+ infile +'" | "'+myJyt+ '" json-to-toml > "'+outfile+str3;
  LogDatei.log('Start: '+mycommand,LLinfo);
  if not FileExists(myJyt) then
     LogDatei.log('file not found: ' + myJyt,LLerror);
  if not FileExists(infile) then
     LogDatei.log('file not found: ' + infile,LLerror);
  if not RunCommandAndCaptureOut(mycommand, True, outResultList, myreport,
    SW_SHOWDEFAULT, myexitcode,true,0) then
  begin
    LogDatei.log('Failed to convert: ' + myreport,LLerror);
  end
  else
  begin
    LogDatei.log('jyt output by exitcode: '+inttostr(myexitcode),LLinfo);
    LogDatei.log_list(outResultList,LLdebug);
  end;
end;

initialization
  outResultList := Tstringlist.Create;
  {$IFDEF DARWIN}
  myJytDir := ExtractFileDir(ParamStr(0))+'/../Resources/jyt';
  myJyt := myJytDir+ '/jyt';
  {$EndIF DARWIN}

finalization
  FreeAndNil(outResultList);
end.

