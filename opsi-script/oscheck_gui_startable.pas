unit oscheck_gui_startable;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  oslog,
  osprocesses,
  osstartproc_cp,
  osfunclin,
  ostxstringlist;

function check_gui_startable(): boolean;

implementation



function check_gui_startable(): boolean;
var
  checkerbinaryfile: string;
  checkerpath: string;
  checkerbinaryfull: string;
  reportstr: string;
  ExitCode: longint;
  catchout: boolean;
  output: TXStringlist;
  found : integer;
  stopped : integer;
begin
  checkerbinaryfile := 'opsi-laz-gui-test';
  checkerpath := ExtractFilePath(ParamStr(0));
  checkerbinaryfull := checkerpath + PathDelim + checkerbinaryfile;
  output := TXStringlist.Create;
  LogDatei.ErrorNumberMarked := Logdatei.NumberOfErrors;
  LogDatei.log('Marked error number ' + IntToStr(LogDatei.ErrorNumberMarked),
    LLinfo);
  logdatei.log('Launch: ' + checkerbinaryfile, LLnotice);
  writeln('Launch: ' + checkerbinaryfile);
  Result := StartProcess_cp(checkerbinaryfull,  // CmdLinePasStr
    1, //showwindow
    False, //showoutput
    True, //WaitForReturn
    False, //WaitForWindowVanished
    False, //WaitForWindowAppearing
    False, //WaitForProcessEnding
    True, //waitsecsAsTimeout
    '', //Ident
    1, //WaitSecs
    reportstr, //var Report: string;
    exitcode,  //var ExitCode: longint;
    True, // catchout: boolean;
    output); // var output: TXStringList
  logdatei.log('Finished: ' + checkerbinaryfile, LLnotice);
  writeln('Finished: ' + checkerbinaryfile);
  if Result = True then
  begin
    if Logdatei.NumberOfErrors - Logdatei.ErrorNumberMarked > 0 then
    begin
      Result := False;
    end;
    if exitcode <> 0 then
    begin
      Result := False;
    end;
  end;
  if ProcessIsRunning(checkerbinaryfile) then
  begin
     stopped := KillProcessbyname(checkerbinaryfile, found);
     logdatei.log(inttostr(found)+' instances of '+checkerbinaryfile
           +' where running - stopped ' + inttostr(stopped), LLnotice);
  end
  else logdatei.log('No instances of '+checkerbinaryfile+' where running.', LLnotice);
end;

end.



