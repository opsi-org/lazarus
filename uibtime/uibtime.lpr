program uibtime;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazrichview, lazreportpdfexport, printer4lazarus, datetimectrls,
  uibdata, login, ontop, debug, changelog, treescrolldown, logoff,
  work_description, dataedit, loggedin_, nachf, statistik, result, sqltext,
  multiday, linhandlewin, uibtWorkRepChooser, uib2erp,
notificationdlg, uibdatetime, pack_powerpdf;

{$R *.res}

begin
  Application.Scaled:=True;
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFDebug, FDebug);
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(TFlogin, Flogin);
  Application.CreateForm(TFLoggedin, FLoggedin);
  Application.CreateForm(TFTreeview, FTreeview);
  Application.CreateForm(TFLogoff, FLogoff);
  Application.CreateForm(TFNachfrage, FNachfrage);
  Application.CreateForm(TFMultiday, FMultiday);
  Application.CreateForm(TFnotificationdlg, Fnotificationdlg);
  Application.ProcessMessages;
  //Application.CreateForm(TFuibtWorkRepChooser, FuibtWorkRepChooser);
  //Application.CreateForm(TFuibtime2erp, Fuibtime2erp);
  //Application.CreateForm(TFreport, Freport);
  Application.Run;
end.

