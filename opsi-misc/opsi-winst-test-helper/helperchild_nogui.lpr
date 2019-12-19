program helperchild_nogui;

{$MODE Delphi}

{$APPTYPE CONSOLE}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  //Interfaces, // this includes the LCL widgetset
  opsiscripttesthelper_main,
  //Forms,
  //helperwin,
  osversioninfo;




{$R *.res}

begin
  Application.Initialize;
  opsiscripttesthelper_main.Application := Application;
  //Application.CreateForm(TForm1, Form1);

    main;
  Application.Run;
  //application.Terminate;
end.


