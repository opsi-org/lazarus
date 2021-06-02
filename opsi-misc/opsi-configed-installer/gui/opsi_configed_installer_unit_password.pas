unit opsi_configed_installer_unit_password;

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
    procedure installConfiged;
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
  opsi_configed_installer_resourcestrings,
  opsi_configed_installer_data,
  opsi_configed_installer_unit_language,
  opsi_configed_installer_unit_wait,
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
  // Write user input in properties.conf file:
  FileText := TStringList.Create;
  TouchCommand := TRunCommandElevated.Create(Password.EditPassword.Text,
    Password.RadioBtnSudo.Checked);

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
  // create repository:
  MyRepo := TLinuxRepository.Create(Data.DistrInfo.MyDistr,
    Password.EditPassword.Text, Password.RadioBtnSudo.Checked);
  // Set OpsiVersion and OpsiBranch afterwards using GetDefaultURL
  if Data.opsiVersion = 'Opsi 4.1' then
    url := MyRepo.GetDefaultURL(Opsi41, stable)
  else
    url := MyRepo.GetDefaultURL(Opsi42, stable);

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
procedure TMyThread.installConfiged;
begin
  FInstallRunCommand.Run(FShellCommand + 'update', Output);
  FInstallRunCommand.Run(FShellCommand + 'install opsi-script', Output);
  FInstallRunCommand.Run('opsi-script-gui -batch ../setup.opsiscript  /var/log/opsi-configed-installer-configed.log', Output);
  FInstallRunCommand.Free;
end;

procedure TMyThread.Execute;
begin
  // sleep to ensure that TWait is shown before addRepo is executed and blocks TWait
  Sleep(100);
  Synchronize(@prepareInstallation);
  Synchronize(@addRepo);
  installConfiged;
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
    LogDatei.log('configed installation failed', 1);
  end
  else
    LogDatei.log('configed installation success', 6);
  //ShowMessage(ExitCode.ToString);
  ShowMessage(FileText.Text + #10 + rsLog + #10 + LogOpsiServer +
    #10 + ConfigedInstaller.logFileName);
  FileText.Free;
end;

procedure TPassword.FormActivate(Sender: TObject);
begin
  btnFinishClicked := False;

  Left := ConfigedInstaller.Left + Round(ConfigedInstaller.Width / 2) - Round(Width / 2);
  Top := ConfigedInstaller.Top + Round(ConfigedInstaller.Height / 2) - Round(Height / 2);
  BtnFinish.Left := Width - BtnBack.Left - ConfigedInstaller.BtnFinishWidth;
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
  ConfigedInstaller.Enabled := True;
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
    ConfigedInstaller.Close;
    Wait.Close;
  end
  else
    ConfigedInstaller.Enabled := True;
end;

end.
