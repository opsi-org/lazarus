unit opsi_quick_install_unit_password;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  MaskEdit, osRunCommandElevated, LCLType, cthreads, osLog, get_latest_lopsiserver;

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
  public
  end;

type

  {TMyThread}

  // Thread for showing 'please wait' on a form while 'apt/zypper/.. update'
  // runs in the background.
  TMyThread = class(TThread)
  private
    FInstallRunCommand: TRunCommandElevated;
    FShellCommand, FClientDataDir, Output: string;
    procedure prepareInstallation;
    procedure addRepo;
    procedure installOpsi;
  protected
    procedure Execute; override;
  public
    constructor Create(password: string; sudo: boolean; shellCommand: string);
  end;

var
  Password: TPassword;
  MyThread: TMyThread;

implementation

uses
  opsi_quick_install_resourcestrings,
  opsi_quick_install_data,
  opsi_quick_install_unit_language,
  opsi_quick_install_unit_overview,
  opsi_quick_install_unit_wait,
  osLinuxRepository,
  osDistributionInfo;

{$R *.lfm}

{MyThread}

constructor TMyThread.Create(password: string; sudo: boolean; shellCommand: string);
begin
  FInstallRunCommand := TRunCommandElevated.Create(password, sudo);
  FShellCommand := shellCommand;

  FreeOnTerminate := True;
  inherited Create(True);
end;

// get properties from query and write them to file properties.conf
procedure TMyThread.prepareInstallation;
var
  FileText: TStringList;
  TouchCommand: TRunCommandElevated;
begin
  // Write user input in l-opsi-server.conf (for tests) and properties.conf file:
  FileText := TStringList.Create;
  TouchCommand := TRunCommandElevated.Create(Password.EditPassword.Text,
    Password.RadioBtnSudo.Checked);

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
  FileText.Add('proxy=' + Data.proxy.PropertyEntry);
  FileText.Add('repo_kind=' + Data.repoKind);
  FileText.Add('ucs_master_admin_password=' + Data.ucsPassword);
  // update_test shall always be false
  FileText.Add('update_test=false');

  FClientDataDir := ExtractFilePath(ParamStr(0));
  Delete(FClientDataDir, Length(FClientDataDir), 1);
  FClientDataDir := ExtractFilePath(FClientDataDir) + 'l-opsi-server';
  // try downloading latest l-opsi-server and use respective DirClientData
  if getLOpsiServer(TouchCommand, Data.distroName) then
  begin
    LogDatei.log('Latest l-opsi-server successfully downloaded', LLInfo);
    FClientDataDir += '_downloaded/CLIENT_DATA/';
  end
  else
  begin
    LogDatei.log('Downloading latest l-opsi-server failed. Using default l-opsi-server:',
      LLnotice);
    FClientDataDir += '/CLIENT_DATA/';
  end;
  Password.clientDataDir := FClientDataDir;

  // following equals no-gui WritePropsToFile
  // write in l-opsi-server.conf file:
  if not FileExists('l-opsi-server.conf') then
    TouchCommand.Run('touch l-opsi-server.conf', Output);
  FileText.SaveToFile('l-opsi-server.conf');

  // write in properties.conf file:
  // navigate to CLIENT_DATA in l-opsi-server
  if not FileExists(FClientDataDir + 'properties.conf') then
    TouchCommand.Run('touch ' + FClientDataDir + 'properties.conf', Output);
  TouchCommand.Run('chown -c $USER ' + FClientDataDir + 'properties.conf', Output);
  FileText.SaveToFile(FClientDataDir + 'properties.conf');

  // Important for getting the result 'failed' in case of a wrong password
  // because in this case the RunCommands below aren't executed and therefore
  // setup.opsiscript, that usually does it, isn't too:
  FileText.Clear;
  FileText.Add('failed');
  if not FileExists(FClientDataDir + 'result.conf') then
    TouchCommand.Run('touch ' + FClientDataDir + 'result.conf', Output);
  TouchCommand.Run('chown -c $USER ' + FClientDataDir + 'result.conf', Output);
  FileText.SaveToFile(FClientDataDir + 'result.conf');

  FileText.Free;
  TouchCommand.Free;
end;

procedure TMyThread.addRepo;
var
  url: string;
  MyRepo: TLinuxRepository;
begin
  Wait.LabelWait.Caption := rsCreateRepo;
  // first remove opsi.list to have a cleared opsi repository list
  if FileExists('/etc/apt/sources.list.d/opsi.list') then
    FInstallRunCommand.Run('rm /etc/apt/sources.list.d/opsi.list', Output);
  // create repository:
  MyRepo := TLinuxRepository.Create(Data.DistrInfo.MyDistr,
    Password.EditPassword.Text, Password.RadioBtnSudo.Checked);
  // Set OpsiVersion and OpsiBranch afterwards using GetDefaultURL
  if Data.opsiVersion = 'Opsi 4.1' then
    url := MyRepo.GetDefaultURL(Opsi41, stringToOpsiBranch(Data.repoKind))
  else
    url := MyRepo.GetDefaultURL(Opsi42, stringToOpsiBranch(Data.repoKind));

  // !following lines need an existing LogDatei
  if (Data.distroName = 'openSUSE') or (Data.distroName = 'SUSE') then
  begin
    MyRepo.Add(url, 'OpsiQuickInstallRepositoryNew');
  end
  else
    MyRepo.Add(url);

  MyRepo.Free;
end;

// install opsi server with thread (only the time consuming parts of the installation)
procedure TMyThread.installOpsi;
begin
  FInstallRunCommand.Run(FShellCommand + 'update', Output);
  FInstallRunCommand.Run(FShellCommand + 'install opsi-script', Output);
  // remove the QuickInstall repo entry because it was only for installing opsi-script
  if FileExists('/etc/apt/sources.list.d/opsi.list') then
    FInstallRunCommand.Run('rm /etc/apt/sources.list.d/opsi.list', Output);
  FInstallRunCommand.Run('opsi-script-gui -batch ' + FClientDataDir +
    'setup.opsiscript  /var/log/opsi-quick-install-l-opsi-server.log', Output);
  FInstallRunCommand.Free;
end;

procedure TMyThread.Execute;
begin
  // sleep to ensure that TWait is shown before addRepo is executed and blocks TWait
  Sleep(100);
  Synchronize(@prepareInstallation);
  Synchronize(@addRepo);
  installOpsi;
end;

{ TPassword }

// show result of l-opsi-server installation and paths to log files,
// executed in TPassword.FormClose
procedure TPassword.showResult;
var
  FileText: TStringList;
begin
  //ShowMessage(clientDataDir);
  FileText := TStringList.Create;
  FileText.LoadFromFile(clientDataDir + 'result.conf');
  // adjust quick-install ExitCode
  if FileText[0] = 'failed' then
  begin
    ExitCode := 1;
    LogDatei.log('l-opsi-server installation failed', 1);
  end
  else
    LogDatei.log('l-opsi-server installation success', 6);
  //ShowMessage(ExitCode.ToString);
  ShowMessage(FileText.Text + #10 + rsLog + #10 + LogOpsiServer +
    #10 + QuickInstall.logFileName);
  FileText.Free;
end;

procedure TPassword.FormActivate(Sender: TObject);
begin
  btnFinishClicked := False;

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
  Password.Visible := False;
  Overview.Enabled := True;
end;

procedure TPassword.BtnFinishClick(Sender: TObject);
var
  TestCommand: TRunCommandElevated;
begin
  // so far opsi 4.2 only has the branches experimental and testing
  if (Data.opsiVersion = 'Opsi 4.2') and (Data.repoKind = 'stable') then
  begin
    ShowMessage('Opsi 4.2 only works on the branches "experimental" and "testing" so far.');
    Exit;
  end;

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
  MyThread := TMyThread.Create(EditPassword.Text, RadioBtnSudo.Checked,
    GetPackageManagementShellCommand(Data.distroName));
  with MyThread do
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
