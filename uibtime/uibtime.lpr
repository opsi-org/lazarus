program uibtime;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazreportpdfexport, printer4lazarus, datetimectrls,
  uibdata, login, ontop, debug, changelog, treescrolldown, logoff,
  work_description, dataedit, loggedin_, nachf, statistik, result, sqltext,
  multiday, runprocess, uibtWorkRepChooser, uib2erp, pack_powerpdf;

{$R *.res}

begin
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
  Application.ProcessMessages;
  //Application.CreateForm(TFuibtWorkRepChooser, FuibtWorkRepChooser);
  //Application.CreateForm(TFuibtime2erp, Fuibtime2erp);
  //Application.CreateForm(TFreport, Freport);
  Application.Run;
end.

