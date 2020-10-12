unit pilot_helper;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  oslog,
  osversioninfo,
  osencoding,
  strutils,
  lazFileUtils,
  Process,
  oscheck_gui_startable,
  custapp;

function main() : longint;
procedure initLogging;

implementation

procedure initLogging;
var
  logfilename : string;
begin
  logdatei := TLogInfo.Create;
  logfilename := ExtractFileNameOnly(reencode(ParamStr(0), 'system'))+'.log';
  LogDatei.WritePartLog := False;
  LogDatei.WriteErrFile:= False;
  LogDatei.WriteHistFile:= False;
  logdatei.CreateTheLogfile(logfilename, False);
  logdatei.LogLevel := 7;
  (*
  for i := 0 to preLogfileLogList.Count-1 do
    logdatei.log(preLogfileLogList.Strings[i], LLessential);
  preLogfileLogList.Free;
  *)
  logdatei.log('opsi-script-pilot version: ' + getversioninfo, LLessential);
  logdatei.log('Called as: ' + ExtractFileNameOnly(reencode(ParamStr(0), 'system')), LLessential);
end;

  function launchProgram(executable: string; params : TStringlist; var exitcode : longint): boolean;
    // http://wiki.freepascal.org/Executing_External_Programs/de
  var
    AProcess: TProcess;
    AStringList: TStringList;

  begin
    launchProgram := True;
    try
      try
        // Erzeugen des TProcess Objekts
        AProcess := TProcess.Create(nil);

        AStringList := TStringList.Create;

        AProcess.Executable:=executable;
        AProcess.Parameters := params;

        AProcess.Options := AProcess.Options + [poUsePipes,poWaitOnExit];

        AProcess.Execute;
        exitcode := AProcess.ExitCode;

      except
        on e: Exception do
        begin
          logdatei.log('Exception:: ' + e.message, LLError);
          //writeln('Exception:: ' + e.message);
          launchProgram := False;
          exitcode := -1;
        end
      end;
    finally
      AStringList.Free;
      AProcess.Free;
    end;
  end;

var
  filePath, executebin: string;
  executeparamlist : TStringlist;
  param: string;
  i: integer;


function main() : longint;
var
  myexitcode : longint;
  mycredfile : string = '';
begin
  //Application.Title:='opsi-script-pilot';
  initLogging;
  writeln('opsi-script-pilot version: ' + getversioninfo);
  writeln('logfile: '+ LogDatei.FileName);
  filePath := ExtractFilePath(ParamStr(0));
  executeparamlist := TStringlist.Create;
  logdatei.log('Launch: paramcount ' + Paramcount.ToString, LLessential);
  i:=1;
  while (i <= Paramcount) do
  begin
    param:=ParamStr(i);
    (*
    if pos('-',param) = 1 then
      if (i < paramcount) and (ParamStr(i+1).Chars[0] <> '-') then
      begin
        //inc(i);
        if param = LowerCase('-credentialfile') then
        begin
          mycredfile := ParamStr(i+1);
          if fileexists(mycredfile) then
             logdatei.log('Found credential file ' + mycredfile, LLessential);
        end;
        //else  param := param + ' '+ParamStr(i);
      end;
      *)
    executeparamlist.Add(param);
    inc(i);
  end;
  myexitcode := 1;
  logdatei.log('Found Params: '+executeparamlist.Text,LLnotice);
  if check_gui_startable() then
  begin
    logdatei.log('gui ok - starting opsi-script ... ',LLnotice);
    launchProgram(filePath+'opsi-script',  executeparamlist, myexitcode);
  end
  else
  begin
    logdatei.log('nogui - starting opsi-script-nogui ... ',LLnotice);
    launchProgram(filePath+'opsi-script-nogui',  executeparamlist, myexitcode);
  end;
  logdatei.log('opsi-script-pilot ending with exitcode: '+inttostr(myexitcode),LLnotice);
  //if fileexists(mycredfile) then
  //           logdatei.log('Found credential file ' + mycredfile, LLessential);
  LogDatei.Close;
  LogDatei.Free;
  executeparamlist.Free;
  result := myexitcode;
end;


end.

