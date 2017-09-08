program mshotfixpatchXML;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  xmlpatch, winpatchCollection, myoslog, cliapp;

begin
  Application:=TMyApplication.Create(nil);
  Application.Run;
  writeln('logfile: ' + xmlpatch.logfilename);
  Application.Free;
end.

