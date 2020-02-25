program opsi_script_beautifier;

{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  beautifyopsiscript, cliapp, osencoding, oslog
  { you can add units after this };

{$R *.res}

begin
  Application:=TMyApplication.Create(nil);
  //Application.Scaled:=True;
  //Application.Scaled:=True;
  Application.Title:='opsi-script-beautifier';
  Application.Run;
  //writeln('logfile: ');
  Application.Free;
end.

