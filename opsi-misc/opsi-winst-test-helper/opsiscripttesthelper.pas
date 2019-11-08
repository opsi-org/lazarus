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
  forms.Application.Title:='opsiscripttesthelper';
  forms.Application.Initialize;
  opsiscripttesthelper_main.Application := forms.Application;
  forms.Application.CreateForm(TForm1, Form1);
  //Application.CreateForm(TDataModule1, DataModule1);
  main;
  forms.Application.Run;
  //application.Terminate;
end.

