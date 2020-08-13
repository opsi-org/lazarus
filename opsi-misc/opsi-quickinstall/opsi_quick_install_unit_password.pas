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
  private

  public

  end;

var
  Password: TPassword;

implementation

uses
  opsi_quick_install_unit_language,
  opsi_quick_install_unit_query,
  opsi_quick_install_unit_query2,
  opsi_quick_install_unit_query3,
  opsi_quick_install_unit_query4,
  opsi_quick_install_unit_query5_dhcp,
  opsi_quick_install_unit_query6,
  opsi_quick_install_unit_query7,
  opsi_quick_install_unit_overview,
  osLinuxRepository;

{$R *.lfm}

{ TPassword }

procedure TPassword.FormActivate(Sender: TObject);
begin
  Left := Overview.Left + Round(Overview.Width / 2) - Round(Width / 2);
  Top := Overview.Top + Round(Overview.Height / 2) - Round(Height / 2);
  BtnFinish.Left := Width - BtnBack.Left - QuickInstall.BtnFinishWidth;
  // for displaying password as dots
  EditPassword.EchoMode := emPassword;
end;

procedure TPassword.BtnBackClick(Sender: TObject);
begin
  Password.Close;
end;

procedure TPassword.BtnFinishClick(Sender: TObject);
var
  fileName, propertyName, url, Output: string;
  FileText: TStringList;
  MyRepo: TLinuxRepository;
  RunCommand: TRunCommandElevated;
begin
  // write user input in l-opsi-server.conf file:
  fileName := ExtractFilePath(ParamStr(0)) + 'l-opsi-server.conf';
  FileText := TStringList.Create;

  propertyName := 'allow_reboot';
  if Query3.RadioBtnYes.Checked then
    FileText.Add(propertyName + '=True')
  else
    FileText.Add(propertyName + '=False');

  propertyName := 'backend';
  if Query2.RadioBtnFile.Checked then
    FileText.Add(propertyName + '=file')
  else
    FileText.Add(propertyName + '=mysql');

  propertyName := 'dnsdomain';
  if Query5_dhcp.RadioBtnUcs.Checked then
    FileText.Add(propertyName + '=ucs.test')
  else if Query5_dhcp.RadioBtnUib.Checked then
    FileText.Add(propertyName + '=uib.local')
  else if Query5_dhcp.RadioBtnVmnat.Checked then
    FileText.Add(propertyName + '=vmnat.local')
  else
    FileText.Add(propertyName + '=' + Query5_dhcp.EditDomain.Text);

  propertyName := 'download_patched_elilo_efi';
  if Query4.RadioBtnYes.Checked then
    FileText.Add(propertyName + '=True')
  else
    FileText.Add(propertyName + '=False');

  propertyName := 'force_copy_modules';
  if Query7.RadioBtnYes.Checked then
    FileText.Add(propertyName + '=True')
  else
    FileText.Add(propertyName + '=False');

  propertyName := 'gateway';
  if Query5_dhcp.RadioBtnGateway10.Checked then
    FileText.Add(propertyName + '=10.100.1.2')
  else if Query5_dhcp.RadioBtnGateway172.Checked then
    FileText.Add(propertyName + '=172.16.166.1')
  else if Query5_dhcp.RadioBtnGateway192.Checked then
    FileText.Add(propertyName + '=192.168.1.245')
  else
    FileText.Add(propertyName + '=' + Query5_dhcp.EditGateway.Text);

  propertyName := 'install_and_configure_dhcp';
  if Query4.RadioBtnDhcpYes.Checked then
    FileText.Add(propertyName + '=True')
  else
    FileText.Add(propertyName + '=False');

  FileText.Add('myipname=' + Query6.EditNameIP.Text);

  FileText.Add('myipnumber=' + Query6.EditNumberIP.Text);

  propertyName := 'nameserver';
  if Query5_dhcp.RadioBtnNameserver10.Checked then
    FileText.Add(propertyName + '=10.100.1.2')
  else if Query5_dhcp.RadioBtnNameserver172.Checked then
    FileText.Add(propertyName + '=172.16.166.1')
  else if Query5_dhcp.RadioBtnNameserver192.Checked then
    FileText.Add(propertyName + '=192.168.1.245')
  else
    FileText.Add(propertyName + '=' + Query5_dhcp.EditNameserver.Text);

  propertyName := 'netmask';
  if Query5_dhcp.RadioBtnMask0.Checked then
    FileText.Add(propertyName + '=255.255.0.0')
  else if Query5_dhcp.RadioBtnMask225.Checked then
    FileText.Add(propertyName + '=255.255.225.0')
  else
    FileText.Add(propertyName + '=' + Query5_dhcp.EditNetmask.Text);

  propertyName := 'network';
  if Query5_dhcp.RadioBtnAddress10.Checked then
    FileText.Add(propertyName + '=10.100.0.0')
  else if Query5_dhcp.RadioBtnAddress172.Checked then
    FileText.Add(propertyName + '=172.16.166.0')
  else if Query5_dhcp.RadioBtnAddress192.Checked then
    FileText.Add(propertyName + '=192.168.0.0')
  else
    FileText.Add(propertyName + '=' + Query5_dhcp.EditAddress.Text);

  FileText.Add('opsi_admin_user_name=' + Query6.EditNameAdmin.Text);

  FileText.Add('opsi_admin_user_password=' + Query6.EditPasswordAdmin.Text);

  propertyName := 'opsi_online_repository';
  if Query.RadioBtnOpsi41.Checked then
    FileText.Add(propertyName + '=' + QuickInstall.baseURLOpsi41 +
      QuickInstall.DistrUrlPart)
    {FileText.Add(propertyName +
      '=http://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.1:/')}
  else if Query.RadioBtnOpsi42.Checked then
    FileText.Add(propertyName + '=' + QuickInstall.baseURLOpsi42 +
      QuickInstall.DistrUrlPart)
  else
    FileText.Add(propertyName + '=' + Query.EditRepo.Text);

  propertyName := 'opsi_noproxy_online_repository';
  if Query.RadioBtnOpsi41.Checked then
    FileText.Add(propertyName + '=' + QuickInstall.baseURLOpsi41 +
      QuickInstall.DistrUrlPart)
    {FileText.Add(propertyName +
      '=http://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.1:/')}
  else if Query.RadioBtnOpsi42.Checked then
    FileText.Add(propertyName + '=' + QuickInstall.baseURLOpsi42 +
      QuickInstall.DistrUrlPart)
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

  // property products_in_depot
  {stringProducts := '';
  for prod := 0 to Query3.PanelProdToChoose.ControlCount - 1 do
  begin
    if (Query3.PanelProdToChoose.Controls[prod] as TCheckBox).Checked then
      stringProducts := stringProducts + ', ' +
        Query3.PanelProdToChoose.Controls[prod].Caption;
  end;
  if stringProducts <> '' then
    // Index of 'Delete' is 1-based (Delete(stringProducts, 0, 2) wouldn't do anything)
    Delete(stringProducts, 1, 1);}
  FileText.Add('products_in_depot=' + Overview.stringProducts);


  FileText.Add('ucs_master_admin_password=' + Query7.EditPasswordUCS.Text);

  propertyName := 'update_test';
  if Query2.RadioBtnYes.Checked then
    FileText.Add(propertyName + '=True')
  else
    FileText.Add(propertyName + '=False');

  FileText.SaveToFile(fileName);
  FileText.Free;

  // create repository
  MyRepo := TLinuxRepository.Create(QuickInstall.MyDistr, EditPassword.Text,
    RadioBtnSudo.Checked);
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
  RunCommand := TRunCommandElevated.Create(EditPassword.Text, RadioBtnSudo.Checked);
  Output := RunCommand.Run('sudo apt update');
  //ShowMessage(Output);
  Output := RunCommand.Run('sudo apt install opsi-script');
  //ShowMessage(Output);
  RunCommand.Free;
  MyRepo.Free;
  // close forms
  Overview.Close;
  Password.Close;
end;

procedure TPassword.EditPasswordUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
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

end.
