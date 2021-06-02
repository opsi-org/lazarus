unit opsi_configed_installer_unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  osRunCommandElevated, osDistributionInfo, osLog, osLinuxRepository;

type

  { TConfigedInstaller }

  TConfigedInstaller = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    procedure addRepo;
    procedure installConfiged;
  public
    InstallRunCommand: TRunCommandElevated;
    ShellCommand, Output: string;
  end;

var
  ConfigedInstaller: TConfigedInstaller;

implementation

{$R *.lfm}

{ TConfigedInstaller }

procedure TConfigedInstaller.addRepo;
var
  url: string;
  MyRepo: TLinuxRepository;
begin
  // create repository:
  MyRepo := TLinuxRepository.Create(xUbuntu_18_04, 'linux123', true);
  // Set OpsiVersion and OpsiBranch afterwards using GetDefaultURL
  url := MyRepo.GetDefaultURL(Opsi42, stable);

  // !following lines need an existing LogDatei
  {if (Data.distroName = 'openSUSE') or (Data.distroName = 'SUSE') then
  begin
    MyRepo.Add(url, 'OpsiQuickInstallRepositoryNew');
  end
  else
    MyRepo.Add(url);}
  MyRepo.Add(url);
  MyRepo.Free;
end;

procedure TConfigedInstaller.installConfiged;
begin
  InstallRunCommand.Run(ShellCommand + 'update', Output);
  InstallRunCommand.Run(ShellCommand + 'install opsi-script', Output);
  InstallRunCommand.Run('opsi-script-gui -batch setup.opsiscript  /var/log/opsi-quick-install-l-opsi-server.log', Output);
  InstallRunCommand.Free;
end;

procedure TConfigedInstaller.FormCreate(Sender: TObject);
var
  logFileName: string;
begin
  // initialize log file:
  logFileName := 'opsi_configed_installer.log';
  LogDatei := TLogInfo.Create;
  LogDatei.CreateTheLogfile(logFileName);
  LogDatei.log('Log file created', LLnothing);
  // log file will be saved in /tmp/opsi_configed_installer.log
  logFileName := LogDatei.StandardMainLogPath + logFileName;

  InstallRunCommand := TRunCommandElevated.Create('linux123', true);
  ShellCommand := GetPackageManagementShellCommand('Ubuntu');
  addRepo;
  installConfiged;
end;

end.

