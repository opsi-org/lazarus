program opsi_quick_install_project;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, opsi_quick_install_unit_language, opsi_quick_install_unit_query
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TQuickInstall, QuickInstall);
  Application.CreateForm(TQuery, Query);
  Application.Run;
end.

