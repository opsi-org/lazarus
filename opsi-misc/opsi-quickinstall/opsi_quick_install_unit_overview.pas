unit opsi_quick_install_unit_overview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TOverview }

  TOverview = class(TForm)
    BackgrImage: TImage;
    BtnBack: TButton;
    BtnFinish: TButton;
    LabelRepoNoCache2: TLabel;
    LabelRepo2: TLabel;
    LabelProds: TLabel;
    LabelNameserver: TLabel;
    LabelGateway: TLabel;
    LabelDomain: TLabel;
    LabelNetworkAdress: TLabel;
    LabelNetmask: TLabel;
    LabelTFTPROOT: TLabel;
    LabelElilo: TLabel;
    LabelReboot: TLabel;
    LabelDHCP: TLabel;
    LabelModules: TLabel;
    LabelPasswordAdminUCS: TLabel;
    LabelIPName: TLabel;
    LabelIPNumber: TLabel;
    LabelAdminPassword: TLabel;
    LabelAdminName: TLabel;
    LabelOpsiProds: TLabel;
    LabelUpdate: TLabel;
    LabelRepoKind: TLabel;
    LabelBackend: TLabel;
    LabelRepoNoCache: TLabel;
    LabelProxy: TLabel;
    LabelRepo: TLabel;
    BigPanel: TPanel;
    PanelRepoNoCache: TPanel;
    PanelRepo: TPanel;
    PanelIP: TPanel;
    PanelAdmin: TPanel;
    PanelNameGate: TPanel;
    PanelNet: TPanel;
    PanelDHCP: TPanel;
    PanelOpsiProds: TPanel;
    PanelQuery2: TPanel;
    procedure BtnBackClick(Sender: TObject);
    procedure BtnFinishClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private

  public

  end;

var
  Overview: TOverview;

implementation

uses
  opsi_quick_install_unit_language,
  opsi_quick_install_unit_query,
  opsi_quick_install_unit_query2,
  opsi_quick_install_unit_query3,
  opsi_quick_install_unit_query4,
  opsi_quick_install_unit_query5_dhcp,
  opsi_quick_install_unit_query6,
  opsi_quick_install_unit_query7;

{$R *.lfm}

{ TOverview }

procedure TOverview.BtnFinishClick(Sender: TObject);
var
  fileName, propertyName, stringProducts: string;
  FileText: TStringList;
  prod: integer;
begin
  // write user input in l-opsi-server.conf file
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
    FileText.Add(propertyName +
      '=http://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.1:/')
  else if Query.RadioBtnOpsi42.Checked then
    FileText.Add(propertyName + '=???4.2???')
  else
    FileText.Add(propertyName + '=' + Query.EditRepo.Text);

  propertyName := 'opsi_noproxy_online_repository';
  if Query.RadioBtnOpsi41NoCache.Checked then
    FileText.Add(propertyName +
      '=http://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.1:/')
  else if Query.RadioBtnOpsi42NoCache.Checked then
    FileText.Add(propertyName + '=???4.2???')
  else
    FileText.Add(propertyName + '=' + Query.EditNoCache.Text);

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
  stringProducts := '';
  for prod := 0 to Query3.PanelProdToChoose.ControlCount - 1 do
  begin
    if (Query3.PanelProdToChoose.Controls[prod] as TCheckBox).Checked then
      stringProducts := stringProducts + ', ' +
        Query3.PanelProdToChoose.Controls[prod].Caption;
  end;
  if stringProducts <> '' then
    // Index of 'Delete' is 1-based (Delete(stringProducts, 0, 2) wouldn't do anything)
    Delete(stringProducts, 1, 2);
  FileText.Add('products_in_depot=' + stringProducts);


  FileText.Add('ucs_master_admin_password=' + Query7.EditPasswordMasterAdmin.Text);

  propertyName := 'update_test';
  if Query2.RadioBtnYes.Checked then
    FileText.Add(propertyName + '=True')
  else
    FileText.Add(propertyName + '=False');

  FileText.SaveToFile(fileName);
  FileText.Free;

  // close forms
  Overview.Close;
end;

procedure TOverview.FormActivate(Sender: TObject);
begin
  AdjustPanelPosition(self);
  BackgrImage.Picture.LoadFromFile(QuickInstall.BackgrImageFileName);

  if Query.RadioBtnOpsi41.Checked then
    LabelRepo2.Caption := ' ' + Query.RadioBtnOpsi41.Caption
  else if Query.RadioBtnOpsi42.Checked then
    LabelRepo2.Caption := ' ' + Query.RadioBtnOpsi42.Caption
  else
    LabelRepo2.Caption := ' ' + Query.EditRepo.Text;

  if Query.RadioBtnNone.Checked then
    LabelProxy.Caption := LabelProxy.Caption + ' ' + Query.RadioBtnNone.Caption
  else if Query.RadioBtnMyProxy.Checked then
    LabelProxy.Caption := LabelProxy.Caption + ' ' + Query.RadioBtnMyProxy.Caption
  else
    LabelProxy.Caption := LabelProxy.Caption + ' ' + Query.EditProxy.Text;

  if Query.RadioBtnOpsi41NoCache.Checked then
    LabelRepoNoCache2.Caption := ' ' + Query.RadioBtnOpsi41NoCache.Caption
  else if Query.RadioBtnOpsi42NoCache.Checked then
    LabelRepoNoCache2.Caption := ' ' + Query.RadioBtnOpsi42NoCache.Caption
  else
    LabelRepoNoCache2.Caption := ' ' + Query.EditNoCache.Text;

end;

procedure TOverview.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Query7.Close;
end;


procedure TOverview.BtnBackClick(Sender: TObject);
begin
  showForm(Query7, self);
end;

end.
