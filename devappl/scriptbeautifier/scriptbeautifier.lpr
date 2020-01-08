program scriptbeautifier;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  beautifyopsiscript, cliapp, osencoding, oslog
  { you can add units after this };

{$R *.res}

begin
  Application:=TMyApplication.Create(nil);
  Application.Run;
  writeln('logfile: ');
  Application.Free;
end.

