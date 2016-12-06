program opsiwinsttesthelper;

//{$mode objfpc}{$H+}
{$MODE Delphi}

{$APPTYPE CONSOLE}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, helperwin, VersionInfo, DSiWin32, wispecfolder;

//{$IFDEF WINDOWS}{$R opsiwinsttesthelper.rc}{$ENDIF}

{$R *.res}

begin
  Application.Initialize;
  main;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
  //application.Terminate;
end.

