program helperchild;

{$MODE Delphi}

{$APPTYPE CONSOLE}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  opsiscripttesthelper_main,
  Forms,
  helperwin,
  osversioninfo;


{$IFDEF WINDOWS}{$R helperchild.res}{$ENDIF}

begin
  Application.Scaled:=True;
  Application.Initialize;
  opsiscripttesthelper_main.Application := Application;
  //Application.CreateForm(TForm1, Form1);

    main;
  Application.Run;
  //application.Terminate;
end.


