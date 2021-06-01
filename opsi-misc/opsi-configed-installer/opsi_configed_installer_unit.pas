unit opsi_configed_installer_unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  osRunCommandElevated, osDistributionInfo, osLog;

type

  { TConfigedInstaller }

  TConfigedInstaller = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    procedure installConfiged;
  public
    InstallRunCommand: TRunCommandElevated;
    ShellCommand, Output: string;
  end;

var
  ConfigedInstaller: TConfigedInstaller;

implementation

{$R *.lfm}

procedure TConfigedInstaller.installConfiged;
begin
  InstallRunCommand.Run(ShellCommand + 'update', Output);
  InstallRunCommand.Run(ShellCommand + 'install opsi-script', Output);
  InstallRunCommand.Run('opsi-script-gui -batch setup.opsiscript  /var/log/opsi-quick-install-l-opsi-server.log', Output);
  InstallRunCommand.Free;
end;

{ TConfigedInstaller }

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
  //TODO: add repo
  installConfiged;
end;

end.

