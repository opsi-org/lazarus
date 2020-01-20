program scriptbeautifier;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  beautifyopsiscript, cliapp, osencoding, oslog
  { you can add units after this };

{$R *.res}

begin
  Application:=TMyApplication.Create(nil);
<<<<<<< HEAD
  //Application.Scaled:=True;
=======
  Application.Scaled:=True;
>>>>>>> martinadevel
  Application.Run;
  writeln('logfile: ');
  Application.Free;
end.

