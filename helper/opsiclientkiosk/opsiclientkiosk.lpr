program opsiclientkiosk;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}//{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}//{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Classes,
  SysUtils,
  Forms,
  lazcontrols,
  lcltranslator,
  inifiles,
  opsiclientkioskgui,
  ockdata, installdlg, datadb, osprocesses, progresswindow;


{$R *.res}

begin
  //Application.Scaled:=True;
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.Title:='opsi-client-kiosk';
  Application.CreateForm(TFormOpsiClientKiosk, FormOpsiClientKiosk);
  Application.CreateForm(TFInstalldlg, FInstalldlg);
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(TFormProgressWindow, FormProgressWindow);
  Application.Run;
  Application.Free;
end.

