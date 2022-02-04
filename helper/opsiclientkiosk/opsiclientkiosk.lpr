program opsiclientkiosk;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}//{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}//{$ENDIF}
  {$IFDEF WINDOWS}
  OckWindows,
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
  OckLinux, OckPasswordQuery,
  {$ENDIF LINUX}
  Interfaces, // this includes the LCL widgetset
  Classes, SysUtils, Forms, lazcontrols, lcltranslator, inifiles,
  opsiclientkioskgui, installdlg, datadb, osprocesses, progresswindow,
  lazproginfo, osRunCommandElevated, OSProcessux, opsiconnection, helpinfo,
  OckImagestoDepot, unit1

  {add more units if nedded};


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
  Application.CreateForm(TFormHelpInfo, FormHelpInfo);
  Application.CreateForm(TFormSaveImagesOnDepot, FormSaveImagesOnDepot);
  {$IFDEF LINUX}
  Application.CreateForm(TFormPasswordQuery, FormPasswordQuery);
  {$ENDIF LINUX}
  Application.Run;
  Application.Free;
end.

