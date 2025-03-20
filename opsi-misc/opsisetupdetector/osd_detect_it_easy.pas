unit osd_detect_it_easy;

{$mode Delphi}

interface

uses
  Classes, SysUtils,
 //osdhelper,
  //osprocesses,
  oslog,
  LCLType,
  OSProcessux,
  Process,
  {$IFDEF WINDOWS}
  windows,
  dsiwin32,
  {$ENDIF WINDOWS}
  lazutf8;


function runDieDetect(target : string; var jsonOut : TStringlist) : boolean;
function runDieInfo(target : string; var jsonOut : TStringlist) : boolean;

var
  dieJsonResultList: TStringList;
  dieJsonResultStr: string;
  myDiec : string;
  myDiecParams : string;
  myDiecDir : string;

implementation


function runDieDetect(target : string; var jsonOut : TStringlist) : boolean;
var
  mycommand : string;
  myexitcode : integer;
  myreport : string;
  //oldworkdir : string;
  //oldDisableWow64FsRedirectionStatus : pointer = nil;

begin
  result := false;
  //mycommand := '"C:\WINDOWS\system32\cmd.exe" /C "'+myDiec +' '+myDiecParams + ' "'+target+'""';
  mycommand := '"'+myDiec +'" '+myDiecParams + ' "'+target+'"';
  LogDatei.log('Start: '+mycommand,LLinfo);
  if not FileExists(myDiec) then
     LogDatei.log('file not found: ' + myDiec,LLerror);
  if not RunCommandAndCaptureOut(mycommand, True, jsonOut, myreport,
    SW_SHOWDEFAULT, myexitcode,true,0) then
  begin
    LogDatei.log('Failed to analyze: ' + myreport,LLerror);
    //write_log_and_memo('Failed to analyze: ' + myreport);
    //mysetup.analyze_progess := 0;
  end
  else
  begin
    result := true;
    LogDatei.log('Diec output by exitcode: '+inttostr(myexitcode),LLinfo);
    LogDatei.log_list(jsonOut,LLdebug);
  end;
  //SetCurrentDir(oldworkdir);
  //DSiWow64RevertWow64FsRedirection(oldDisableWow64FsRedirectionStatus);
end;

function runDieInfo(target : string; var jsonOut : TStringlist) : boolean;
var
  mycommand : string;
  myexitcode : integer = 0;
  myreport : string = '';
  //oldworkdir : string;
begin
  result := false;
  //mycommand := '"C:\WINDOWS\system32\cmd.exe" /C "'+myDiec +' --info '+myDiecParams + ' -b "'+ExtractFileName(target)+'""';
  //mycommand := '"'+myDiec +'" --info '+myDiecParams + ' -b "'+ExtractFileName(target)+'"';
  mycommand := '"'+myDiec +'" --info '+myDiecParams + ' "'+target+'"';
  LogDatei.log('Start: '+mycommand,LLinfo);
  //write_memo(mycommand);
  //oldworkdir := GetCurrentDir;
  //SetCurrentDir(ExtractFileDir(target));

  if not RunCommandAndCaptureOut(mycommand, True, jsonOut, myreport,
    SW_SHOWMINIMIZED, myexitcode) then
  begin
    LogDatei.log('Failed to get info: ' + myreport,LLerror);
    //write_memo('Failed to analyze: ' + myreport);
  end
  else
  begin
    result := true;
    LogDatei.log('Diec output: ',LLinfo);
    LogDatei.log_list(jsonOut,LLdebug);
  end;
  //SetCurrentDir(oldworkdir);
end;


initialization
  dieJsonResultList := TStringList.Create;
  {$IFDEF WINDOWS}
  myDiecDir := ExtractFileDir(ParamStr(0))+'\detectItEasy';
  //myDiecDir := 'C:\gitwork\lazarus\opsi-misc\opsisetupdetector\detectItEasy';
  //myDiecDir := '.\detectItEasy';
  myDiec := myDiecDir+ '\diec.exe';
  {$EndIF WINDOWS}
  {$IFDEF LINUX}
  myDiecDir := ExtractFileDir(ParamStr(0))+'/detectItEasy';
  myDiec := myDiecDir+ '/diec.sh';
  {$EndIF LINUX}
  {$IFDEF DARWIN}
  myDiecDir := ExtractFileDir(ParamStr(0))+'/../Resources/detectItEasy';
  myDiec := myDiecDir+ '/die.app/Contents/MacOS/diec';
  {$EndIF DARWIN}

  myDiecParams := '--json';

finalization
  FreeAndNil(dieJsonResultList);

end.
