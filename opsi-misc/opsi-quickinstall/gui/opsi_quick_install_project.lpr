program opsi_quick_install_project;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  cthreads,
  Interfaces, // this includes the LCL widgetset
  Forms,
  opsi_quick_install_unit_language,
  opsi_quick_install_unit_query,
  opsi_quick_install_unit_query2,
  opsi_quick_install_unit_query4,
  opsi_quick_install_unit_query5_dhcp,
  opsi_quick_install_unit_query6,
  opsi_quick_install_unit_overview,
  opsi_quick_install_unit_password,
  opsi_quick_install_unit_distr,
  opsi_quick_install_resourcestrings,
  opsi_quick_install_unit_wait,
  opsi_quick_install_data;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Title:='OpsiQuickInstallGui';
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TQuickInstall, QuickInstall);
  Application.CreateForm(TQuery, Query);
  Application.CreateForm(TQuery2, Query2);
  Application.CreateForm(TQuery4, Query4);
  Application.CreateForm(TQuery5_dhcp, Query5_dhcp);
  Application.CreateForm(TQuery6, Query6);
  Application.CreateForm(TOverview, Overview);
  Application.CreateForm(TPassword, Password);
  Application.CreateForm(TDistribution, Distribution);
  Application.CreateForm(TWait, Wait);
  Application.Run;
end.
