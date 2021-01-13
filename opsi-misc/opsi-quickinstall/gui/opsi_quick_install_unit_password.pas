unit opsi_quick_install_unit_password;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  MaskEdit, osRunCommandElevated, LCLType;

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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    FMyThread: TThread;
  public

  end;

type

  {TMyThread}

  TMyThread = class(TThread)
  private
    FForm: TPassword;
    procedure Complete;
  protected
    procedure Execute; override;
  public
    constructor Create(Form: TPassword);
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

constructor TMyThread.Create(Form: TPassword);
begin
  FForm := Form;
  //FForm.Caption := 'Thread is running';
  inherited Create(False);
end;

procedure TMyThread.Complete;
begin
  FForm.FMyThread := nil;
  //FForm.Caption := 'Thread has completed';
  // can't close Overview and Wait here, only Password
  FForm.Close;
end;

procedure TMyThread.Execute;
var
  fileName, propertyName, url, Output, shellCommand: string;
  FileText: TStringList;
  MyRepo: TLinuxRepository;
  InstallRunCommand: TRunCommandElevated;
begin
  FreeOnTerminate := True;

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

  propertyName := 'dnsdomain';
  if Query5_dhcp.CheckBoxDomain1.Checked then
    FileText.Add(propertyName + '=' + Query5_dhcp.CheckBoxDomain1.Caption)
  else if Query5_dhcp.CheckBoxDomain2.Checked then
    FileText.Add(propertyName + '=' + Query5_dhcp.CheckBoxDomain2.Caption)
  else if Query5_dhcp.CheckBoxDomain3.Checked then
    FileText.Add(propertyName + '=' + Query5_dhcp.CheckBoxDomain3.Caption)
  else
    FileText.Add(propertyName + '=' + Query5_dhcp.EditDomain.Text);

  propertyName := 'dnsdomain = ';
  if Query5_dhcp.CheckBoxDomain1.Checked then
    propertyName += Query5_dhcp.CheckBoxDomain1.Caption + ', ';
  if Query5_dhcp.CheckBoxDomain2.Checked then
    propertyName += Query5_dhcp.CheckBoxDomain2.Caption + ', ';
  if Query5_dhcp.CheckBoxDomain3.Checked then
    propertyName += Query5_dhcp.CheckBoxDomain3.Caption + ', ';
  if Query5_dhcp.CheckBoxOtherDomain.Checked then
    propertyName += Query5_dhcp.EditDomain.Text + ', ';
  delete(propertyName,propertyName.length-1,2);
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
  fileName := ExtractFilePath(ParamStr(0)) + 'l-opsi-server.conf';
  FileText.SaveToFile(fileName);
  // write in properties.conf file:
  // navigate to CLIENT_DATA in l-opsi-server
  //ShowMessage(ParamStr(0));
  fileName := ExtractFilePath(ParamStr(0));
  Delete(fileName, Length(fileName), 1);
  fileName := ExtractFilePath(fileName) + 'l-opsi-server/CLIENT_DATA/';
  FileText.SaveToFile(fileName + 'properties.conf');

  // Important for getting the result 'failed' in case of a wrong password...
  // ...cause in this case the RunCommands below aren't executed...
  // ...and therefore setup.opsiscript, that usually does it, isn't too:
  FileText.Clear;
  FileText.Add('failed');
  FileText.SaveToFile(fileName + 'result.conf');
  //////////////////////////////////////////////////////////////////////////////
  // InstallOpsi:
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

  InstallRunCommand := TRunCommandElevated.Create(Password.EditPassword.Text,
    Password.RadioBtnSudo.Checked);
  shellCommand := QuickInstall.DistrInfo.GetPackageManagementShellCommand(
    QuickInstall.distroName);
  Output := InstallRunCommand.Run(shellCommand + 'update');
  //ShowMessage(Output);
  Output := InstallRunCommand.Run(shellCommand + 'install opsi-script');
  //ShowMessage(Output);
  Output := InstallRunCommand.Run('opsi-script -batch ' + fileName +
    'setup.opsiscript  /var/log/opsi-quick-install-l-opsi-server.log');

  FileText.LoadFromFile(fileName + 'result.conf');
  // adjust quick-install ExitCode
  if FileText[0] = 'failed' then
    ExitCode := 1;
  ShowMessage(FileText.Text + #10 + rsLog + #10 + LogOpsiServer +
    #10 + QuickInstall.logFileName);

  FileText.Free;
  InstallRunCommand.Free;
  MyRepo.Free;
  QuickInstall.DistrInfo.Free;

  //Sleep(2000);
  Synchronize(@Complete);
end;

{ TPassword }

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
  LabelPassword.Caption := rsPassword+':';
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
    if FMyThread = nil then
      FMyThread := TMyThread.Create(Self);
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

procedure TPassword.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // close project
  Overview.Close;
  Wait.Close;
end;

end.
