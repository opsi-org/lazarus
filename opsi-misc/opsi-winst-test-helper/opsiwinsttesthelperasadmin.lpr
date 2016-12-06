program opsiwinsttesthelperasadmin;

//{$mode objfpc}{$H+}
{$MODE Delphi}

{$APPTYPE CONSOLE}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, helperwin, VersionInfo, DSiWin32, wispecfolder;

//{$IFDEF WINDOWS}{$R opsiwinsttesthelperasadmin.rc}{$ENDIF}

{$R *.res}
{$R manifest.rc}

begin
  Application.Initialize;
  main;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
  //application.Terminate;
end.

