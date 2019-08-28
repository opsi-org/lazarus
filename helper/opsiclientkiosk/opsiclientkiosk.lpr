program opsiclientkiosk;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}//{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}//{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Classes, SysUtils, Forms, lazcontrols, lcltranslator, inifiles,
  opsiclientkioskgui, installdlg, datadb, osprocesses, progresswindow,
  proginfo, opsiconnection, imagestoshare;


{$R *.res}

begin
  //Application.Scaled:=True;
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.Title:='opsi-client-kiosk';
  Application.CreateForm(TFormOpsiClientKiosk, FormOpsiClientKiosk);
  Application.CreateForm(TFInstalldlg, FInstalldlg);
  Application.CreateForm(TDataModuleOCK, DataModuleOCK);
  Application.CreateForm(TFormProgressWindow, FormProgressWindow);
  Application.CreateForm(TFormSaveImagesOnShare, FormSaveImagesOnShare);
  Application.Run;
  Application.Free;
end.

