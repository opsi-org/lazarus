unit opsi_quick_install_unit_password;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  MaskEdit, osRunCommandElevated, LCLType, cthreads, osLog;

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
    // get properties from query and write them to file properties.conf
    procedure prepareInstallation;
    procedure addRepo;
    // Show result of l-opsi-server installation and paths to log files
    // Executed in TPassword.FormClose
    procedure showResult;
  private
    // directory l-opsi-server/CLIENT_DATA
    clientDataDir: string;
  public
  end;

type

  {TMyThread}

  TMyThread = class(TThread)
  private
    FInstallRunCommand: TRunCommandElevated;
    FShellCommand, FClientDataDir: string;
    procedure installOpsi;
  protected
    procedure Execute; override;
  public
    constructor Create(password: string; sudo: boolean; shellCommand: string;
      clientDataDir: string);
  end;

var
  Password: TPassword;

implementation

uses
  opsi_quick_install_unit_language,
  opsi_quick_install_unit_query,
  opsi_quick_install_unit_query2,
  opsi_quick_install_unit_query_prods,
  opsi_quick_install_unit_query4,
  opsi_quick_install_unit_query5_dhcp,
  opsi_quick_install_unit_query6,
  opsi_quick_install_unit_overview,
  opsi_quick_install_unit_wait,
  osLinuxRepository, opsi_quick_install_resourcestrings;

{$R *.lfm}

{MyThread}

constructor TMyThread.Create(password: string; sudo: boolean;
  shellCommand: string; clientDataDir: string);
begin
  FInstallRunCommand := TRunCommandElevated.Create(password, sudo);
  FShellCommand := shellCommand;
  FClientDataDir := clientDataDir;

  FreeOnTerminate := True;
  inherited Create(True);
end;


// install opsi server with thread (only the time consuming parts of the installation)
procedure TMyThread.installOpsi;
begin
  FInstallRunCommand.Run(FShellCommand + 'update');
  FInstallRunCommand.Run(FShellCommand + 'install opsi-script');
  // Never ever again problems with opsi.list!
  FInstallRunCommand.Run('rm /etc/apt/sources.list.d/opsi.list');
  FInstallRunCommand.Run('../common/opsi-script-gui -batch ' +
    FClientDataDir + 'setup.opsiscript  /var/log/opsi-quick-install-l-opsi-server.log');
  FInstallRunCommand.Free;
end;

procedure TMyThread.Execute;
begin
  installOpsi;
end;

{ TPassword }

// get properties from query and write them to file properties.conf
procedure TPassword.prepareInstallation;
var
  propertyName: string;
  FileText: TStringList;
begin
  // write user input in l-opsi-server.conf and properties.conf file:
  FileText := TStringList.Create;

  propertyName := 'allow_reboot';
  if Query4.RadioBtnYes.Checked then
    FileText.Add(propertyName + '=true')
  else
    FileText.Add(propertyName + '=false');

  propertyName := 'backend';
  if (Query2.RadioBtnFile.Checked) then
    FileText.Add(propertyName + '=file')
  else
    FileText.Add(propertyName + '=mysql');

  // for several domains
  {propertyName := 'dnsdomain';
  if Query5_dhcp.CheckBoxDomain1.Checked then
    FileText.Add(propertyName + '=' + Query5_dhcp.CheckBoxDomain1.Caption)
  else if Query5_dhcp.CheckBoxDomain2.Checked then
    FileText.Add(propertyName + '=' + Query5_dhcp.CheckBoxDomain2.Caption)
  else if Query5_dhcp.CheckBoxDomain3.Checked then
    FileText.Add(propertyName + '=' + Query5_dhcp.CheckBoxDomain3.Caption)
  else
    FileText.Add(propertyName + '=' + Query5_dhcp.EditDomain.Text);}

  // for one domain
  propertyName := 'dnsdomain=';
  if Query5_dhcp.CheckBoxDomain1.Checked then
    propertyName += Query5_dhcp.CheckBoxDomain1.Caption + ', ';
  if Query5_dhcp.CheckBoxDomain2.Checked then
    propertyName += Query5_dhcp.CheckBoxDomain2.Caption + ', ';
  if Query5_dhcp.CheckBoxDomain3.Checked then
    propertyName += Query5_dhcp.CheckBoxDomain3.Caption + ', ';
  if Query5_dhcp.CheckBoxOtherDomain.Checked then
    propertyName += Query5_dhcp.EditDomain.Text + ', ';
  Delete(propertyName, propertyName.length - 1, 2);
  propertyName := 'dnsdomain=uib.local';
  FileText.Add(propertyName);

  propertyName := 'force_copy_modules';
  if Query2.RadioBtnYesCopy.Checked then
    FileText.Add(propertyName + '=true')
  else
    FileText.Add(propertyName + '=false');

  propertyName := 'gateway';
  if Query5_dhcp.RadioBtnGateway1.Checked then
    FileText.Add(propertyName + '=' + Query5_dhcp.RadioBtnGateway1.Caption)
  else if Query5_dhcp.RadioBtnGateway2.Checked then
    FileText.Add(propertyName + '=' + Query5_dhcp.RadioBtnGateway2.Caption)
  else if Query5_dhcp.RadioBtnGateway3.Checked then
    FileText.Add(propertyName + '=' + Query5_dhcp.RadioBtnGateway3.Caption)
  else
    FileText.Add(propertyName + '=' + Query5_dhcp.EditGateway.Text);

  propertyName := 'install_and_configure_dhcp';
  if Query4.RadioBtnDhcpYes.Checked then
    FileText.Add(propertyName + '=true')
  else
    FileText.Add(propertyName + '=false');

  FileText.Add('myipname=' + Query6.EditNameIP.Text);

  FileText.Add('myipnumber=' + Query6.EditNumberIP.Text);

  propertyName := 'nameserver';
  if Query5_dhcp.RadioBtnNameserver1.Checked then
    FileText.Add(propertyName + '=' + Query5_dhcp.RadioBtnNameserver1.Caption)
  else if Query5_dhcp.RadioBtnNameserver2.Checked then
    FileText.Add(propertyName + '=' + Query5_dhcp.RadioBtnNameserver2.Caption)
  else if Query5_dhcp.RadioBtnNameserver3.Checked then
    FileText.Add(propertyName + '=' + Query5_dhcp.RadioBtnNameserver3.Caption)
  else
    FileText.Add(propertyName + '=' + Query5_dhcp.EditNameserver.Text);

  propertyName := 'netmask';
  if Query5_dhcp.RadioBtnMask1.Checked then
    FileText.Add(propertyName + '=' + Query5_dhcp.RadioBtnMask1.Caption)
  else if Query5_dhcp.RadioBtnMask2.Checked then
    FileText.Add(propertyName + '=' + Query5_dhcp.RadioBtnMask2.Caption)
  else
    FileText.Add(propertyName + '=' + Query5_dhcp.EditNetmask.Text);

  propertyName := 'network';
  if Query5_dhcp.RadioBtnAddress1.Checked then
    FileText.Add(propertyName + '=' + Query5_dhcp.RadioBtnAddress1.Caption)
  else if Query5_dhcp.RadioBtnAddress2.Checked then
    FileText.Add(propertyName + '=' + Query5_dhcp.RadioBtnAddress2.Caption)
  else if Query5_dhcp.RadioBtnAddress3.Checked then
    FileText.Add(propertyName + '=' + Query5_dhcp.RadioBtnAddress3.Caption)
  else
    FileText.Add(propertyName + '=' + Query5_dhcp.EditAddress.Text);

  FileText.Add('opsi_admin_user_name=' + Query6.EditNameAdmin.Text);

  FileText.Add('opsi_admin_user_password=' + Query6.EditPasswordAdmin.Text);

  propertyName := 'opsi_online_repository';
  if Query.RadioBtnOpsi41.Checked then
    FileText.Add(propertyName + '=' + QuickInstall.baseURLOpsi41)
  else if Query.RadioBtnOpsi42.Checked then
    FileText.Add(propertyName + '=' + QuickInstall.baseURLOpsi42)
  else
    FileText.Add(propertyName + '=' + Query.EditRepo.Text);

  propertyName := 'opsi_noproxy_online_repository';
  if Query.RadioBtnOpsi41.Checked then
    FileText.Add(propertyName + '=' + QuickInstall.baseURLOpsi41)
  else if Query.RadioBtnOpsi42.Checked then
    FileText.Add(propertyName + '=' + QuickInstall.baseURLOpsi42)
  else
    FileText.Add(propertyName + '=' + Query.EditOtherNoCache.Text);

  propertyName := 'patch_default_link_for_bootimage';
  if Query4.RadioBtnMenu.Checked then
    FileText.Add(propertyName + '=default.menu')
  else
    FileText.Add(propertyName + '=default.nomenu');

  propertyName := 'proxy';
  if Query.RadioBtnNone.Checked then
    FileText.Add(propertyName + '=')
  else if Query.RadioBtnMyProxy.Checked then
    FileText.Add(propertyName + '=http://myproxy.dom.org:8080')
  else
    FileText.Add(propertyName + '=' + Query.EditProxy.Text);

  propertyName := 'repo_kind';
  if Query2.RadioBtnExperimental.Checked then
    FileText.Add(propertyName + '=experimental')
  else if Query2.RadioBtnStable.Checked then
    FileText.Add(propertyName + '=stable')
  else
    FileText.Add(propertyName + '=testing');

  FileText.Add('ucs_master_admin_password=' + Query4.EditPasswordUCS.Text);

  // update_test shall always be false
  FileText.Add('update_test=false');
  //////////////////////////////////////////////////////////////////////////////
  // WritePropsToFile:
  // write in l-opsi-server.conf file:
  clientDataDir := ExtractFilePath(ParamStr(0)) + 'l-opsi-server.conf';
  FileText.SaveToFile(clientDataDir);
  // write in properties.conf file:
  // navigate to CLIENT_DATA in l-opsi-server
  //ShowMessage(ParamStr(0));
  clientDataDir := ExtractFilePath(ParamStr(0));
  Delete(clientDataDir, Length(clientDataDir), 1);
  clientDataDir := ExtractFilePath(clientDataDir) + 'l-opsi-server/CLIENT_DATA/';
  FileText.SaveToFile(clientDataDir + 'properties.conf');

  // Important for getting the result 'failed' in case of a wrong password...
  // ...cause in this case the RunCommands below aren't executed...
  // ...and therefore setup.opsiscript, that usually does it, isn't too:
  FileText.Clear;
  FileText.Add('failed');
  FileText.SaveToFile(clientDataDir + 'result.conf');
  //////////////////////////////////////////////////////////////////////////////

  //ShowMessage(clientDataDir);
  FileText.Free;
end;

procedure TPassword.addRepo;
var
  url: string;
  MyRepo: TLinuxRepository;
begin
  // create repository
  MyRepo := TLinuxRepository.Create(QuickInstall.DistrInfo.MyDistr,
    Password.EditPassword.Text, Password.RadioBtnSudo.Checked);
  // Set OpsiVersion and OpsiBranch afterwards using GetDefaultURL
  if Query.RadioBtnOpsi41.Checked then
  begin
    if Query2.RadioBtnExperimental.Checked then
      url := MyRepo.GetDefaultURL(Opsi41, experimental)
    else if Query2.RadioBtnStable.Checked then
      url := MyRepo.GetDefaultURL(Opsi41, stable)
    else if Query2.RadioBtnTesting.Checked then
      url := MyRepo.GetDefaultURL(Opsi41, testing);
  end
  else
  begin
    if Query2.RadioBtnExperimental.Checked then
      url := MyRepo.GetDefaultURL(Opsi42, experimental)
    else if Query2.RadioBtnStable.Checked then
      url := MyRepo.GetDefaultURL(Opsi42, stable)
    else if Query2.RadioBtnTesting.Checked then
      url := MyRepo.GetDefaultURL(Opsi42, testing);
  end;
  MyRepo.Add(url);
  MyRepo.Free;
end;

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
begin
  if Query.RadioBtnOpsi42.Checked and not Query2.RadioBtnExperimental.Checked then
  begin
    ShowMessage('Opsi 4.2 only works on branch "experimental".');
    Exit;
  end
  else
  begin
    prepareInstallation;
    addRepo;
    // start thread for opsi server installation while showing form 'Wait'
    with TMyThread.Create(EditPassword.Text, RadioBtnSudo.Checked,
      QuickInstall.DistrInfo.GetPackageManagementShellCommand(QuickInstall.distroName),
      clientDataDir) do
      begin
        // FormClose automatically executed on termination of thread
        OnTerminate := @FormClose;
        Start;
      end;
    Wait.Visible := True;
  end;
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
  if CheckBoxShowPassword.Checked then
    // TMaskEdit can't do this !?
    EditPassword.EchoMode := emNormal
  else
    EditPassword.EchoMode := emPassword;
  // EditPassword.Text still gets the real text
  //ShowMessage(EditPassword.Text);
end;

procedure TPassword.FormClose(Sender: TObject);
begin
  QuickInstall.DistrInfo.Free;
  showResult;
  // close project
  Overview.Close;
  Wait.Close;
end;

end.
