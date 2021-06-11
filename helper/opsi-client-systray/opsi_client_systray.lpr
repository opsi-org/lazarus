program opsi_client_systray;
(*********************
NOTE: libnotifiy.so is needed for compiling and linking

Maybe it exists a libnotify4.so or libnotify.so.4 or something else on your system but no libnotify.so
Thus create a symbolik link libnotify.so -> libnotifiy.so.4 to get the program compiled
*********************)

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  oca_systray_dm,
  lcltranslator;

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Title:='opsi-client-systray';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TDataModule1, DataModule1);
  Application.Run;
end.

