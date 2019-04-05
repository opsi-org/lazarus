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
  ockdata, installdlg, datadb, osprocesses;


{$R *.res}

begin
  //Application.Scaled:=True;
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.Title:='opsi-client-kiosk';
  Application.CreateForm(TFopsiClientKiosk, FopsiClientKiosk);
  Application.CreateForm(TFInstalldlg, FInstalldlg);
  Application.CreateForm(TDataModule1, DataModule1);
  Application.Run;
  Application.Free;
end.

