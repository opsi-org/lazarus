program helperchild;

{$MODE Delphi}

{$APPTYPE CONSOLE}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  helperwin, wispecfolder, osversioninfo;


{$IFDEF WINDOWS}{$R helperchild.res}{$ENDIF}

begin
  Application.Initialize;
  main;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
  //application.Terminate;
end.


