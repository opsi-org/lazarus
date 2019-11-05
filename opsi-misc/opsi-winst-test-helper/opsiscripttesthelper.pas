program opsiscripttesthelper;

//{$mode objfpc}{$H+}
{$MODE Delphi}

{$APPTYPE CONSOLE}

{$DEFINE GUI}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset

  opsiscripttesthelper_main,
  Forms,
  helperwin;

//{$IFDEF WINDOWS}{$R opsiwinsttesthelper.rc}{$ENDIF}

{$R *.res}

begin
  Application.Title:='opsiscripttesthelper';
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  //Application.CreateForm(TDataModule1, DataModule1);
  opsiscripttesthelper_main.Application := Application;
  main;
  Application.Run;
  //application.Terminate;
end.

