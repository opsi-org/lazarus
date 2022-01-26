unit opsi_configed_installer_unit_password;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  MaskEdit, osRunCommandElevated, LCLType, cthreads,
  osLog;

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

const
  {$IFDEF DARWIN}
  logPath = '/opsi-configed-installer/gui/opsi-configed-installer-configed.log';
  {$ELSE}
  logPath = '/var/log/opsi-configed-installer-configed.log';
  {$ENDIF}

var
  Password: TPassword;
  MyThread: TMyThread;

implementation

uses
  opsi_configed_installer_resourcestrings,
  opsi_configed_installer_data,
  opsi_configed_installer_unit_language,
  opsi_configed_installer_unit_wait,
  opsi_configed_installer_unit_query,
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
  TouchCommand: TRunCommandElevated;
  FileText: TStringList;
begin
  TouchCommand := TRunCommandElevated.Create(Password.EditPassword.Text,
    Password.RadioBtnSudo.Checked);

  FClientDataDir := ExtractFilePath(ParamStr(0));
  Delete(FClientDataDir, Length(FClientDataDir), 1);
  FClientDataDir := ExtractFilePath(FClientDataDir) + 'CLIENT_DATA/';
  Password.clientDataDir := FClientDataDir;

  FileText := TStringList.Create;
  FileText.Add('memory_requirement=' + Data.MemoryRequirement);
  FileText.Add('scaling_factor=' + Data.ScalingFactor);

  if not FileExists(FClientDataDir + 'properties.conf') then
    TouchCommand.Run('touch ' + FClientDataDir + 'properties.conf', Output);
  TouchCommand.Run('chown -c $USER ' + FClientDataDir + 'properties.conf', Output);
  FileText.SaveToFile(FClientDataDir + 'properties.conf');

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
  url := MyRepo.GetDefaultURL(Opsi42, experimental);

  // !following lines need an existing LogDatei
  if (Data.distroName = 'openSUSE') or (Data.distroName = 'SUSE') then
  begin
    MyRepo.Add(url, 'OpsiConfigedInstallerRepository');
  end
  else
    MyRepo.Add(url);

  MyRepo.Free;
end;

// install opsi server with thread (only the time consuming parts of the installation)
procedure TMyThread.installConfiged;
begin
  {$IFDEF DARWIN}
  FInstallRunCommand.Run(
    'open ../../../../../../../../Applications/opsi-script.app/ --args -batch ..' + ExtractFilePath(ParamStr(0)) + '../../../../..' + '/opsi-configed-installer/CLIENT_DATA/setup.opsiscript -logfile ..' + ExtractFilePath(ParamStr(0)) + '../../../../..' + logPath, Output);
  {$ELSE}
  FInstallRunCommand.Run(FShellCommand + 'update', Output);
  FInstallRunCommand.Run(FShellCommand + 'install opsi-script', Output);
  //FInstallRunCommand.Run('opsi-script-gui -batch ../CLIENT_DATA/setup.opsiscript ' + logPath, Output);

  FInstallRunCommand.Run('opsi-script -batch ' + FClientDataDir + 'setup.opsiscript ' +
    logPath, Output);
  {$ENDIF}

  FInstallRunCommand.Free;
end;

procedure TMyThread.Execute;
begin
  // sleep to ensure that TWait is shown before addRepo is executed and blocks TWait
  Sleep(100);
  Synchronize(@prepareInstallation);
  {$IFNDEF DARWIN}
  Synchronize(@addRepo);
  {$ENDIF}

  installConfiged;
end;

{ TPassword }

// show result of l-opsi-server installation and paths to log files,
// executed in TPassword.FormClose
procedure TPassword.showResult;
var
  FileText: TStringList;
  InstallationResult: string;
begin
  FileText := TStringList.Create;
  {$IFDEF DARWIN}
  FileText.LoadFromFile('../../../../..' + logPath);
  {$ELSE}
  FileText.LoadFromFile(logPath);
  {$ENDIF}

  if Pos('script finished: success', FileText[FileText.Count - 6]) = 0 then
  begin
    LogDatei.log('configed installation failed', 1);
    InstallationResult := 'failed';
    ExitCode := 1;
  end
  else
  begin
    LogDatei.log('configed installation success', 6);
    InstallationResult := 'success';
  end;
  ShowMessage(InstallationResult + #10 + rsLog + #10 + logPath + #10 +
    ConfigedInstaller.logFileName);
  FileText.Free;
end;

procedure TPassword.FormActivate(Sender: TObject);
begin
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
  btnFinishClicked := False;
  Password.Visible := False;
  Query.Enabled := True;
end;

procedure TPassword.BtnFinishClick(Sender: TObject);
var
  TestCommand: TRunCommandElevated;
begin
  // test if the password is correct, otherwise exit
  {$IFNDEF DARWIN}
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
  {$ENDIF}

  btnFinishClicked := True;
  // start thread for opsi configed installation while showing TWait
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
    Query.Close;
    Wait.Close;
  end
  else
    Query.Enabled := True;
end;

end.
