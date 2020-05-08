unit linhandlewin;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  strutils,
  osprocessux,
  osprocesses,
  oslog,
  process;

function setwindowtoalldesktops(winstr: string): boolean;
function moveToCurrentDeskAndFront(winstr: string) : boolean;



implementation

function setwindowtoalldesktops(winstr: string): boolean;
{$IFDEF LINUX}
var
  slist: TStringList;
  report: string;
  exitcode: integer;
{$ENDIF LINUX}
begin
  {$IFDEF LINUX}
  slist := TStringList.Create;
  Result := RunCommandAndCaptureOut('wmctrl -r "' + winstr + '" -t -2',
    False, slist, report, 0, exitcode);
  slist.Free;
  if exitcode <> 0 then
    Result := False;
  {$ELSE LINUX}
  Result := True;
  {$ENDIF LINUX}
end;


function moveToCurrentDeskAndFront(winstr: string) : boolean;
var
  wmctrlpath: string;
  outstring: string;
  outlist: TStringList;
  desknum, i, exitcode: integer;
  cmd: string;
begin
    {$IFDEF LINUX}
    result := true;
  try

    if which('wmctrl', wmctrlpath) then
    begin
      LogDatei.AktProduktId := 'moveToCurrentDeskAndFront';
      cmd := wmctrlpath + ' -r "' + winstr + '" -b add,above';
      LogDatei.log('cmd: ' + cmd, LLdebug2);
      getCommandResult(cmd, exitcode);
      if exitcode = 0 then
        LogDatei.log('movefront', LLdebug2)
      else
        begin
        LogDatei.log('movefront failed', LLwarning);
        result := false;
        end;
    (*
     if RunCommand(wmctrlpath,['-r','"uibtime - ontop"','-b','add,above'], outstring,[poWaitOnExit]) then
        debugOut(8, 'TimerOnTopTimer', 'movefront ')
      else  debugOut(4, 'TimerOnTopTimer', 'movefront failed');
      *)

      outlist := RunCommandCaptureOutGetOutlist(wmctrlpath + ' -d ');
      LogDatei.log('outlist: ' + outlist.Text, LLdebug2);
      outstring := '';
      i := 0;
      desknum := -1;
      while (desknum = -1) and (i < outlist.Count) do
      begin
        if AnsiContainsText(outlist[i], '*') then
          desknum := i
        else
          Inc(i);
      end;
      LogDatei.log('desknum: ' + IntToStr(desknum), LLdebug2);
      if desknum > -1 then
      begin
        // RunCommand(wmctrlpath,['-r','"'+FOnTop.Caption+'"','-t ',inttostr(desknum)], outstring,[poWaitOnExit])
        cmd := wmctrlpath + ' -r "' + winstr + '" -t ' + IntToStr(desknum);
        LogDatei.log('cmd: ' + cmd, LLdebug2);
        getCommandResult(cmd, exitcode);
         if exitcode = 0 then
        LogDatei.log('movetodesk', LLdebug2)
      else
        begin
        LogDatei.log('movetodesk failed', LLwarning);
        result := false;
        end;
      end;
    end
    else
    begin
      LogDatei.log('movefront no wmctrl', LLwarning);
      result := false;
    end;
  except
    LogDatei.log('exception: movefront ', LLerror);
    result := false;
  end;
{$ENDIF LINUX}
end;



end.
