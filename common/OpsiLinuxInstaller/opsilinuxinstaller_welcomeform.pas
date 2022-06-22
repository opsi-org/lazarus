unit OpsiLinuxInstaller_WelcomeForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  OpsiLinuxInstaller_BaseForm,
  LogFileFunctions,
  opsi_configed_installer_data,
  osfunclin;

type
  TOpsiLinuxInstallerWelcomeForm = class(TOpsiLinuxInstallerBaseForm)
    BtnBack: TButton;
    BtnFinish: TButton;
    ComboBoxLanguages: TComboBox;
    WelcomePanel: TPanel;
    LabelWelcome: TLabel;
    LabelSelLanguage: TLabel;
    CarryOutPanel: TPanel;
    LabelCarryOut: TLabel;
    procedure FormCreate(Sender: TObject); override;
  public
  const
    LogFileName = 'opsi_configed_installer.log';
  end;

implementation

procedure InitializeDataStructureWithDistribution;
begin
  // data structure to store the installer data
  Data := TConfigedInstallerData.Create;
  // Following lines take a few seconds and are therefore executed only once at the
  // beginning of the installer program.
  {$IFDEF DARWIN}
  if (RunCommand('/bin/sh', ['-c', 'echo | sysctl kern.ostype'], macDistroName) and
    RunCommand('/bin/sh', ['-c', 'echo | sysctl kern.osrelease'],
    macDistroRelease)) then
  begin
    Delete(macDistroName, 1, 'kern.ostype: '.Length);
    Delete(macDistroRelease, 1, 'kern.osrelease: '.Length);
    Delete(macDistroName, macDistroName.Length, 1);
    Delete(macDistroRelease, macDistroRelease.Length, 1);
    //ShowMessage('*' + macDistroName + '*');
    //ShowMessage('*' + macDistroRelease + '*');
    Data.DistrInfo.SetNameAndRelease(macDistroName, macDistroRelease);
  end;
  {$ELSE}
  Data.DistrInfo.SetNameAndRelease(getLinuxDistroName, getLinuxDistroRelease);
  {$ENDIF}
end;

procedure TOpsiLinuxInstallerWelcomeForm.FormCreate(Sender: TObject);
begin
  Inherited FormCreate(Sender);
  InitializeLogFile(LogFileName);
  InitializeDataStructureWithDistribution;
end;

end.
