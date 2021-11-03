program opsi_configed_installer_project;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  cthreads,
  Interfaces, // this includes the LCL widgetset
  Forms, opsi_configed_installer_unit_language,
  opsi_configed_installer_unit_distr, opsi_configed_installer_unit_wait,
  opsi_configed_installer_unit_password,
  opsi_configed_installer_resourcestrings;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Title:='OpsiConfigedInstallerGui';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TConfigedInstaller, ConfigedInstaller);
  Application.CreateForm(TPassword, Password);
  Application.CreateForm(TDistribution, Distribution);
  Application.CreateForm(TWait, Wait);
  Application.Run;
end.
