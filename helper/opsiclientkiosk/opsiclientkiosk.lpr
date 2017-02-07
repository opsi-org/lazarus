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
  ockdata, installdlg;


{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.Title:='opsi-client-kiosk';
  Application.CreateForm(TFopsiClientKiosk, FopsiClientKiosk);
  Application.CreateForm(TFInstalldlg, FInstalldlg);
  Application.Run;
  Application.Free;
end.

