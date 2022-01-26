unit opsi_quick_install_unit_password;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  MaskEdit, osRunCommandElevated, LCLType, cthreads, osLog,
  lopsiserverdownloader, Process;

type

  { TPassword }

  TPassword = class(TForm)
    BtnBack: TButton;
    BtnFinish: TButton;
    CheckBoxShowPassword: TCheckBox;
    EditPassword: TEdit;
    LabelRights: TLabel;
    LabelPassword: TLabel;
    PanelPassword: TPanel;
    PanelRights: TPanel;
    RadioBtnRoot: TRadioButton;
    RadioBtnSudo: TRadioButton;
    procedure BtnBackClick(Sender: TObject);
    procedure BtnFinishClick(Sender: TObject);
    procedure EditPasswordUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure FormActivate(Sender: TObject);
    procedure CheckBoxShowPasswordChange(Sender: TObject);
    procedure FormClose(Sender: TObject);
    // show result of l-opsi-server installation and paths to log files
    // showResult executed in TPassword.FormClose
    procedure showResult;
  private
    // full directory l-opsi-server/CLIENT_DATA/
    clientDataDir, Output: string;
    btnFinishClicked: boolean;
  end;

type

  {TInstallOpsiThread}

  // Thread for showing 'please wait' on a form while 'apt/zypper/.. update'
  // runs in the background.
  TInstallOpsiThread = class(TThread)
  private
    FInstallRunCommand: TRunCommandElevated;
    FPackageManagementShellCommand, DirClientData, Output: string;
    two_los_to_test, one_installation_failed: boolean;
    name_los_default, name_los_downloaded, name_current_los: string;
    version_los_default, version_los_downloaded: string;
    FileText: TStringList;
    message: string;
    procedure ShowMessageOnForm;
    procedure defineDirClientData;
    procedure writePropsToFile;
    procedure addRepo;
    procedure executeLOSscript;
    procedure installOpsi;
  protected
    procedure Execute; override;
  public
    constructor Create(password: string; sudo: boolean; PackageManagementShellCommand: string);
  end;

var
  Password: TPassword;
  InstallOpsiThread: TInstallOpsiThread;

implementation

uses
  opsi_quick_install_resourcestrings,
  opsiquickinstall_data,
  opsi_quick_install_unit_language,
  opsi_quick_install_unit_overview,
  opsi_quick_install_unit_wait,
  osLinuxRepository,
  DistributionInfo;

{$R *.lfm}

{InstallOpsiThread}

constructor TInstallOpsiThread.Create(password: string; sudo: boolean;
  PackageManagementShellCommand: string);
begin
  FInstallRunCommand := TRunCommandElevated.Create(password, sudo);
  FPackageManagementShellCommand := PackageManagementShellCommand;

  FreeOnTerminate := True;
  inherited Create(True);
end;

procedure TInstallOpsiThread.ShowMessageOnForm;
begin
  Wait.LabelWait.Caption := message;
end;

procedure TInstallOpsiThread.defineDirClientData;
var
  los_default_search, los_downloaded_search: TSearchRec;
begin
  DirClientData := ExtractFilePath(ParamStr(0));
  Delete(DirClientData, Length(DirClientData), 1);
  //DirClientData := ExtractFilePath(DirClientData) + 'l-opsi-server';
  DirClientData := ExtractFilePath(DirClientData);

  {if two_los_to_test then
  begin
    message := rsWait;
    Synchronize(@ShowMessageOnForm);
  end;}
  // try downloading latest l-opsi-server and set DirClientData for the latest version
  if two_los_to_test and DownloadLOS(FInstallRunCommand, Data.DistrInfo) then
  begin
    // extract and compare version numbers of default and downloaded los
    if (FindFirst('../l-opsi-server_4.*', faAnyFile and faDirectory,
      los_default_search) = 0) and
      (FindFirst('../downloaded_l-opsi-server_4.*', faAnyFile and
      faDirectory, los_downloaded_search) = 0) then
    begin
      name_los_default := los_default_search.Name;
      name_los_downloaded := los_downloaded_search.Name;
      // extract version numbers
      version_los_default := los_default_search.Name;
      Delete(version_los_default, 1, Pos('_', version_los_default));
      version_los_downloaded := los_downloaded_search.Name;
      Delete(version_los_downloaded, 1, Pos('_', version_los_downloaded));
      Delete(version_los_downloaded, 1, Pos('_', version_los_downloaded));
      // compare and use latest l-opsi-server version
      if version_los_downloaded > version_los_default then
        name_current_los := name_los_downloaded
      else
      begin
        name_current_los := name_los_default;
        if version_los_downloaded = version_los_default then
          two_los_to_test := False;
      end;
    end;
  end
  else
  if one_installation_failed then
  begin
    // if there is a downloaded los but the latest los version failed to install,
    // switch between name_los_default and name_los_downloaded to get the dir of
    // the older version
    if version_los_downloaded > version_los_default then
      name_current_los := name_los_default
    else
      name_current_los := name_los_downloaded;
  end
  else
  // otherwise, in the case that downloading the latest l-opsi-server failed,
  // use the default one
  if FindFirst('../l-opsi-server_4.*', faAnyFile and faDirectory,
    los_default_search) = 0 then
  begin
    name_los_default := los_default_search.Name;
    // extract version numbers
    version_los_default := los_default_search.Name;
    Delete(version_los_default, 1, Pos('_', version_los_default));
    name_current_los := name_los_default;
    two_los_to_test := False;
  end;
  DirClientData += name_current_los + '/CLIENT_DATA/';
end;

// get properties from query and write them to file properties.conf
procedure TInstallOpsiThread.writePropsToFile;
begin
  // Write user input in properties.conf file:
  FileText.Clear;
  FileText.Add('allow_reboot=' + Data.reboot.PropertyEntry);
  FileText.Add('backend=' + Data.backend);
  FileText.Add('dnsdomain=' + Data.domain);
  FileText.Add('force_copy_modules=' + Data.copyMod.PropertyEntry);
  FileText.Add('install_and_configure_dhcp=' + Data.dhcp.PropertyEntry);
  FileText.Add('myipname=' + Data.ipName);
  FileText.Add('myipnumber=' + Data.ipNumber);
  FileText.Add('nameserver=' + Data.nameserver);
  FileText.Add('netmask=' + Data.netmask);
  FileText.Add('network=' + Data.networkAddress);
  FileText.Add('opsi_admin_user_name=' + Data.adminName);
  FileText.Add('opsi_admin_user_password=' + Data.adminPassword);
  FileText.Add('opsi_online_repository=' + Data.repo);
  FileText.Add('opsi_noproxy_online_repository=' + Data.repoNoCache);
  FileText.Add('patch_default_link_for_bootimage=' + Data.symlink);
  FileText.Add('proxy=' + Data.proxy);
  FileText.Add('repo_kind=' + Data.repoKind);
  FileText.Add('ucs_master_admin_password=' + Data.ucsPassword);
  // update_test shall always be false
  FileText.Add('update_test=false');

  DefineDirClientData;
  Password.clientDataDir := DirClientData;

  // following equals no-gui WritePropsToFile
  // write in properties.conf file:
  // navigate to CLIENT_DATA in l-opsi-server
  if not FileExists(DirClientData + 'properties.conf') then
    FInstallRunCommand.Run('touch ' + DirClientData + 'properties.conf', Output);
  FInstallRunCommand.Run('chown -c $USER ' + DirClientData + 'properties.conf', Output);
  FileText.SaveToFile(DirClientData + 'properties.conf');
end;

procedure TInstallOpsiThread.addRepo;
var
  url: string;
  ReleaseKeyRepo: TLinuxRepository;
begin
  message := rsCreateRepo;
  Synchronize(@ShowMessageOnForm);
  // first remove opsi.list to have a cleared opsi repository list
  if FileExists('/etc/apt/sources.list.d/opsi.list') then
    FInstallRunCommand.Run('rm /etc/apt/sources.list.d/opsi.list', Output);
  // create repository:
  ReleaseKeyRepo := TLinuxRepository.Create(Data.DistrInfo.Distr,
    Password.EditPassword.Text, Password.RadioBtnSudo.Checked);
  // Set OpsiVersion and OpsiBranch afterwards using GetDefaultURL
  if Data.opsiVersion = 'Opsi 4.1' then
    url := ReleaseKeyRepo.GetDefaultURL(Opsi41, stringToOpsiBranch(Data.repoKind))
  else
    url := ReleaseKeyRepo.GetDefaultURL(Opsi42, stringToOpsiBranch(Data.repoKind));

  // !following lines need an existing LogDatei
  if (Data.DistrInfo.DistroName = 'openSUSE') or (Data.DistrInfo.DistroName = 'SUSE') then
  begin
    ReleaseKeyRepo.Add(url, 'OpsiQuickInstallRepositoryNew');
  end
  else
    ReleaseKeyRepo.Add(url);

  ReleaseKeyRepo.Free;
end;

procedure TInstallOpsiThread.executeLOSscript;
begin
  // Important for getting the result 'failed' in case of a wrong password
  // because in this case the RunCommands below aren't executed and therefore
  // setup.opsiscript, that usually does it, isn't too:
  FileText.Clear;
  FileText.Add('failed');
  if not FileExists(DirClientData + 'result.conf') then
    FInstallRunCommand.Run('touch ' + DirClientData + 'result.conf', Output);

  FInstallRunCommand.Run('chown -c $USER ' + DirClientData + 'result.conf', Output);
  FileText.SaveToFile(DirClientData + 'result.conf');

  // if one installation failed, then opsi-script was already installed
  if not one_installation_failed then
  begin
    FInstallRunCommand.Run(FPackageManagementShellCommand + 'update', Output);
    message := rsInstall + 'opsi-script...';
    Synchronize(@ShowMessageOnForm);
    FInstallRunCommand.Run(FPackageManagementShellCommand + 'install opsi-script', Output);
  end;
  // remove the QuickInstall repo entry because it was only for installing opsi-script
  if FileExists('/etc/apt/sources.list.d/opsi.list') then
    FInstallRunCommand.Run('rm /etc/apt/sources.list.d/opsi.list', Output);

  {message := rsInstall + name_current_los + '... ' + rsSomeMin;
  Synchronize(@ShowMessageOnForm);}
  FInstallRunCommand.Run('opsi-script -batch ' + DirClientData +
    'setup.opsiscript  /var/log/opsi-quick-install-l-opsi-server.log', Output);
end;

// install opsi server with thread (only the time consuming parts of the installation)
procedure TInstallOpsiThread.installOpsi;
begin
  Synchronize(@addRepo);

  // install opsi-server
  two_los_to_test := True;
  one_installation_failed := False;
  writePropsToFile;
  executeLOSscript;

  // get result from result file
  FileText.Clear;
  FileText.LoadFromFile(DirClientData + 'result.conf');
  if (FileText[0] = 'failed') and two_los_to_test then
  begin
    // if installation of latest l-opsi-server failed, try the older version:
    message := rsInstallation + rsFailed + '. ' + rsTryOlderLOS + '.';
    Synchronize(@ShowMessageOnForm);
    Sleep(1000);
    LogDatei.log('Installation failed: ' + name_current_los, LLessential);
    LogDatei.log('Try older version of l-opsi-server:', LLnotice);
    two_los_to_test := False;
    one_installation_failed := True;
    writePropsToFile;

    executeLOSscript;
    FileText.LoadFromFile(DirClientData + 'result.conf');
  end;

  if FileText[0] = 'failed' then
  begin
    message := rsInstallation + rsFailed + '.';
    Synchronize(@ShowMessageOnForm);
    LogDatei.log('Installation failed: ' + name_current_los, LLessential);
    LogDatei.log(Data.opsiVersion + ' installation failed', LLessential);
  end
  else
  begin
    LogDatei.log('Installation successful: ' + name_current_los, LLessential);
    LogDatei.log(Data.opsiVersion + ' installation successful', LLessential);
  end;
end;

procedure TInstallOpsiThread.Execute;
begin
  // sleep to ensure that TWait is shown before addRepo is executed and blocks TWait
  Sleep(100);
  FileText := TStringList.Create;
  one_installation_failed := False;
  two_los_to_test := True;
  installOpsi;

  FileText.Free;
  FInstallRunCommand.Free;
end;

{ TPassword }

// show result of l-opsi-server installation and paths to log files,
// executed in TPassword.FormClose
procedure TPassword.showResult;
var
  FileText: TStringList;
  installationResult: string;
begin
  //ShowMessage(clientDataDir);
  FileText := TStringList.Create;
  FileText.LoadFromFile(clientDataDir + 'result.conf');
  // adjust quick-install ExitCode
  if FileText[0] = 'failed' then
  begin
    installationResult := rsFailed;
    ExitCode := 1;
  end
  else
    installationResult := rsSuccess;

  //ShowMessage(ExitCode.ToString);
  ShowMessage(rsInstallationOf + Data.opsiVersion + ' ' + installationResult + '!' +
    #10 + #10 + rsLog + #10 + LogOpsiServer + #10 + QuickInstall.logFileName);
  FileText.Free;
end;

procedure TPassword.FormActivate(Sender: TObject);
begin
  Left := Overview.Left + Round(Overview.Width / 2) - Round(Width / 2);
  Top := Overview.Top + Round(Overview.Height / 2) - Round(Height / 2);
  BtnFinish.Left := Width - BtnBack.Left - QuickInstall.BtnFinishWidth;
  // for displaying password as dots
  EditPassword.EchoMode := emPassword;

  // text by resourcestrings
  Caption := rsPassword;
  LabelRights.Caption := rsRights;
  LabelPassword.Caption := rsPassword + ':';
  CheckBoxShowPassword.Caption := rsShowPassword;
  BtnBack.Caption := rsBack;
  BtnFinish.Caption := rsFinish;
end;

procedure TPassword.BtnBackClick(Sender: TObject);
begin
  btnFinishClicked := False;
  Password.Visible := False;
  Overview.Enabled := True;
end;

procedure TPassword.BtnFinishClick(Sender: TObject);
var
  TestCommand: TRunCommandElevated;
begin
  // test if the password is correct, otherwise exit
  TestCommand := TRunCommandElevated.Create(EditPassword.Text, RadioBtnSudo.Checked);
  TestCommand.Run('mkdir /root/testDir', Output);
  if (Pos('Error', Output) >= 0) and (Output <> '') then
  begin
    TestCommand.Free;
    ShowMessage(rsWrongPassword);
    Exit;
  end;
  TestCommand.Run('rm -rf /root/testDir', Output);
  TestCommand.Free;

  btnFinishClicked := True;
  // start thread for opsi server installation while showing TWait
  Data.DistrInfo.SetPackageManagementShellCommand;
  InstallOpsiThread := TInstallOpsiThread.Create(EditPassword.Text,
    RadioBtnSudo.Checked, Data.DistrInfo.PackageManagementShellCommand);
  with InstallOpsiThread do
  begin
    // FormClose automatically executed on termination of thread
    OnTerminate := @FormClose;
    Start;
  end;
  // show TWait
  Wait.Visible := True;
end;

procedure TPassword.EditPasswordUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  // finishing also possible by pressing enter after writing the password in EditPassword
  // #13 stands for the Enter key
  if UTF8Key = #13 then
    BtnFinish.Click;
end;

procedure TPassword.CheckBoxShowPasswordChange(Sender: TObject);
begin
  // Change whether the password is shown as dots or text:
  // EditPassword.Text still gets the real text
  if CheckBoxShowPassword.Checked then
    // TMaskEdit can't do this!
    EditPassword.EchoMode := emNormal
  else
    EditPassword.EchoMode := emPassword;
end;

procedure TPassword.FormClose(Sender: TObject);
begin
  if btnFinishClicked then
  begin
    Data.DistrInfo.Free;
    // show result of the whole installation (from result.conf)
    showResult;
    // close project
    Overview.Close;
    Wait.Close;
  end
  else
    Overview.Enabled := True;
end;

end.
