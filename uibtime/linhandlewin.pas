unit linhandlewin;

{$mode objfpc}{$H+}

interface

uses
  classes,
  SysUtils,
  osprocessux,
  process;


function setwindowtoalldesktops(winstr : string) : boolean;



implementation

function setwindowtoalldesktops(winstr : string) : boolean;
{$IFDEF LINUX}
var
  slist : TStringlist;
  report : string;
  exitcode : integer;
{$ENDIF LINUX}
begin
  {$IFDEF LINUX}
  slist := TStringlist.create;
  result := RunCommandAndCaptureOut('wmctrl -r "'+winstr+'" -t -2',
                                    false , slist , report, 0, exitcode);
  slist.free;
  if exitcode <> 0 then result := false;
  {$ELSE LINUX}
  Result := true;
  {$ENDIF LINUX}
end;


end.

