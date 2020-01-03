program opsiscripttesthelper;

//{$mode objfpc}{$H+}
{$MODE Delphi}

{$APPTYPE CONSOLE}

//{$DEFINE GUI}

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
  opsiscripttesthelper_main.Application := Application;
  //Application.CreateForm(TForm1, Form1);
  //Form1.Caption:= 'opsi-script-test-helper';
  //Application.CreateForm(TDataModule1, DataModule1);
  main;
  Application.Run;
  //application.Terminate;
end.

