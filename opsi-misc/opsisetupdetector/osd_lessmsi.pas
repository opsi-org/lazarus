unit osd_lessmsi;

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
  windows,
  lazutf8,
  dsiwin32;

function getInstallDirFromMsi(target : string) :string;
function runLessmsiTable(target : string; table : string; var Outlist : TStringlist) : boolean;
//function runDieInfo(target : string; jsonOut : TStringlist) : boolean;

var
  resultList: TStringList;
  resultStr: string;
  myLessmsi : string;
  myLessmsiParams : string;
  myLessmsiDir : string;

implementation


function runLessmsiTable(target : string; table : string; var Outlist : TStringlist) : boolean;
var
  mycommand : string;
  myexitcode : integer =0;
  myreport : string = '';

begin
  result := false;
  mycommand := '"'+myLessmsi +'" l -t '+table + ' "'+target+'"';
  LogDatei.log('Start: '+mycommand,LLinfo);
  if not FileExists(myLessmsi) then
     LogDatei.log('file not found: ' + myLessmsi,LLerror);
  if not RunCommandAndCaptureOut(mycommand, True, Outlist, myreport,
    SW_SHOWDEFAULT, myexitcode,true,0) then
  begin
    LogDatei.log('Failed to analyze: ' + myreport,LLerror);
    //write_log_and_memo('Failed to analyze: ' + myreport);
    //mysetup.analyze_progess := 0;
  end
  else
  begin
    result := true;
    LogDatei.log('lessmsi output by exitcode: '+inttostr(myexitcode),LLinfo);
    LogDatei.log_list(Outlist,LLdebug);
  end;
end;

(*
function runDieInfo(target : string; jsonOut : TStringlist) : boolean;
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
  *)

  function getInstallDirFromMsi(target : string) :string;
  begin
    Result := 'unknown';
    resultList.Clear;
    runLessmsiTable(target, 'Directory', resultList);
    resultstr := resultList.Text;
  end;

initialization
  resultList := TStringList.Create;
  {$IFDEF WINDOWS}
  myLessmsiDir := ExtractFileDir(ParamStr(0))+'\utils\lessmsi';
  myLessmsi := myLessmsiDir+ '\lessmsi.exe';
  {$EndIF WINDOWS}

finalization
  FreeAndNil(resultList);

end.
