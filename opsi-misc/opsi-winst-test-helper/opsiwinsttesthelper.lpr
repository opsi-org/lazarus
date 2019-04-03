program opsiwinsttesthelper;

//{$mode objfpc}{$H+}
{$MODE Delphi}

{$APPTYPE CONSOLE}

{$Define GUI}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  {$IFDEF GUI}
  Forms,
  {$ENDIF}
  helperwin;

//{$IFDEF WINDOWS}{$R opsiwinsttesthelper.rc}{$ENDIF}

{$R *.res}

begin
  Application.Initialize;
  main;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
  //application.Terminate;
end.

