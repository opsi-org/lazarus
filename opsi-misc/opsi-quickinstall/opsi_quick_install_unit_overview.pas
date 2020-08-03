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
    LabelFinish: TLabel;
    LabelRepoNoCache2: TLabel;
    LabelRepo2: TLabel;
    LabelProds: TLabel;
    LabelNameserverGateway: TLabel;
    LabelDomain: TLabel;
    LabelNetmaskNetwork: TLabel;
    LabelTFTPROOT: TLabel;
    LabelElilo: TLabel;
    LabelReboot: TLabel;
    LabelDHCP: TLabel;
    LabelModules: TLabel;
    LabelPasswordUCS: TLabel;
    LabelIP: TLabel;
    LabelAdminNamePassword: TLabel;
    LabelOpsiProds: TLabel;
    LabelUpdate: TLabel;
    LabelBackendRepoKind: TLabel;
    LabelRepoNoCache: TLabel;
    LabelProxy: TLabel;
    LabelRepo: TLabel;
    BigPanel: TPanel;
    PanelFinish: TPanel;
    PanelRepoNoCache: TPanel;
    PanelRepo: TPanel;
    PanelDHCP: TPanel;
    PanelOpsiProds: TPanel;
    PanelQuery2: TPanel;
    procedure BtnBackClick(Sender: TObject);
    procedure BtnFinishClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private

  public
    stringProducts: string;
  end;

var
  Overview: TOverview;

resourcestring
  rsProxy = 'Proxy:';
  rsBackend = 'Backend:';
  rsRepoKind = 'Repo kind:';
  rsUpdate = 'Install from stable, update to repo kind:';
  rsReboot = 'Reboot after script is finished:';
  rsOpsiDhcpServer = 'Run opsi dhcp server:';
  rsElilo = 'Elilo.efi has timeout of 2 seconds:';
  rsTFTPROOT = 'TFTPROOT symlink points to:';
  rsNetmask = 'Netmask:';
  rsNetwork = 'Network address:';
  rsDomain = 'DNS Domain:';
  rsNameserver = 'Primary nameserver:';
  rsGateway = 'Gateway:';
  rsAdminName = 'Opsi admin user name:';
  rsAdminPassword = 'password:';
  rsIPName = 'IP name:';
  rsIPNumber = 'IP number:';
  rsUCSPassword = 'Password of administrator of UCS domain controller:';
  rsCopyModules = 'Copy modules:';

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
  opsi_quick_install_unit_password;

{$R *.lfm}

{ TOverview }

procedure TOverview.BtnFinishClick(Sender: TObject);
begin
  Password.ShowModal;
end;

procedure TOverview.FormActivate(Sender: TObject);
var
  prod: integer;
begin
  AdjustPanelPosition(self);
  BackgrImage.Picture.LoadFromFile(QuickInstall.BackgrImageFileName);
  if Query4.RadioBtnDhcpYes.Checked then
    PanelDHCP.Visible := True
  else
    PanelDHCP.Visible := False;
  // Repository
  if Query.RadioBtnOpsi41.Checked then
    LabelRepo2.Caption := ' ' + QuickInstall.baseURLOpsi41 + QuickInstall.DistrUrlPart
  else if Query.RadioBtnOpsi42.Checked then
    LabelRepo2.Caption := ' ' + QuickInstall.baseURLOpsi42 + QuickInstall.DistrUrlPart
  else
    LabelRepo2.Caption := ' ' + Query.EditRepo.Text;
  // Proxy
  if Query.RadioBtnNone.Checked then
    LabelProxy.Caption := rsProxy + ' ' + Query.RadioBtnNone.Caption
  else if Query.RadioBtnMyProxy.Checked then
    LabelProxy.Caption := rsProxy + ' ' + Query.RadioBtnMyProxy.Caption
  else
    LabelProxy.Caption := rsProxy + ' ' + Query.EditProxy.Text;
  // Repository (no cache)
  if Query.RadioBtnOpsi41NoCache.Checked then
    LabelRepoNoCache2.Caption := ' ' + QuickInstall.baseURLOpsi41 + QuickInstall.DistrUrlPart
  else if Query.RadioBtnOpsi42NoCache.Checked then
    LabelRepoNoCache2.Caption := ' ' + QuickInstall.baseURLOpsi42 + QuickInstall.DistrUrlPart
  else
    LabelRepoNoCache2.Caption := ' ' + Query.EditNoCache.Text;
  // Backend
  if Query2.RadioBtnFile.Checked then
    LabelBackendRepoKind.Caption := rsBackend + ' ' + Query2.RadioBtnFile.Caption
  else
    LabelBackendRepoKind.Caption := rsBackend + ' ' + Query2.RadioBtnMySql.Caption;
  // Repo kind
  if Query2.RadioBtnExperimental.Checked then
    LabelBackendRepoKind.Caption :=
      LabelBackendRepoKind.Caption + ', ' + rsRepoKind + ' ' +
      Query2.RadioBtnExperimental.Caption
  else if Query2.RadioBtnStable.Checked then
    LabelBackendRepoKind.Caption :=
      LabelBackendRepoKind.Caption + ', ' + rsRepoKind + ' ' +
      Query2.RadioBtnStable.Caption
  else
    LabelBackendRepoKind.Caption :=
      LabelBackendRepoKind.Caption + ', ' + rsRepoKind + ' ' +
      Query2.RadioBtnTesting.Caption;
  // Update
  if Query2.RadioBtnYes.Checked then
    LabelUpdate.Caption := rsUpdate + ' ' + Query2.RadioBtnYes.Caption
  else
    LabelUpdate.Caption := rsUpdate + ' ' + Query2.RadioBtnNo.Caption;
  // Prods
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
  LabelProds.Caption := stringProducts;
  // Reboot
  if Query3.RadioBtnYes.Checked then
    LabelReboot.Caption := rsReboot + ' ' + Query3.RadioBtnYes.Caption
  else
    LabelReboot.Caption := rsReboot + ' ' + Query3.RadioBtnNo.Caption;
  // Dhcp
  if Query4.RadioBtnDhcpYes.Checked then
    LabelDHCP.Caption := rsOpsiDhcpServer + ' ' + Query4.RadioBtnDhcpYes.Caption
  else
    LabelDHCP.Caption := rsOpsiDhcpServer + ' ' + Query4.RadioBtnDhcpNo.Caption;
  // Elilo.efi
  if Query4.RadioBtnYes.Checked then
    LabelElilo.Caption := rsElilo + ' ' + Query4.RadioBtnYes.Caption
  else
    LabelElilo.Caption := rsElilo + ' ' + Query4.RadioBtnNo.Caption;
  // TFTPROOT
  if Query4.RadioBtnMenu.Checked then
    LabelTFTPROOT.Caption := rsTFTPROOT + ' ' + Query4.RadioBtnMenu.Caption
  else
    LabelTFTPROOT.Caption := rsTFTPROOT + ' ' + Query4.RadioBtnNoMenu.Caption;
  // Netmask
  if Query5_dhcp.RadioBtnMask0.Checked then
    LabelNetmaskNetwork.Caption := rsNetmask + ' ' + Query5_dhcp.RadioBtnMask0.Caption
  else if Query5_dhcp.RadioBtnMask225.Checked then
    LabelNetmaskNetwork.Caption := rsNetmask + ' ' + Query5_dhcp.RadioBtnMask225.Caption
  else
    LabelNetmaskNetwork.Caption := rsNetmask + ' ' + Query5_dhcp.EditNetmask.Text;
  // Network address
  if Query5_dhcp.RadioBtnAddress10.Checked then
    LabelNetmaskNetwork.Caption :=
      LabelNetmaskNetwork.Caption + ', ' + rsNetwork + ' ' +
      Query5_dhcp.RadioBtnAddress10.Caption
  else if Query5_dhcp.RadioBtnAddress172.Checked then
    LabelNetmaskNetwork.Caption :=
      LabelNetmaskNetwork.Caption + ', ' + rsNetwork + ' ' +
      Query5_dhcp.RadioBtnAddress172.Caption
  else if Query5_dhcp.RadioBtnAddress192.Checked then
    LabelNetmaskNetwork.Caption :=
      LabelNetmaskNetwork.Caption + ', ' + rsNetwork + ' ' +
      Query5_dhcp.RadioBtnAddress192.Caption
  else
    LabelNetmaskNetwork.Caption :=
      LabelNetmaskNetwork.Caption + ', ' + rsNetwork + ' ' +
      Query5_dhcp.EditAddress.Text;
  // Domain
  if Query5_dhcp.RadioBtnUcs.Checked then
    LabelDomain.Caption := rsDomain + ' ' + Query5_dhcp.RadioBtnUcs.Caption
  else if Query5_dhcp.RadioBtnUib.Checked then
    LabelDomain.Caption := rsDomain + ' ' + Query5_dhcp.RadioBtnUib.Caption
  else if Query5_dhcp.RadioBtnVmnat.Checked then
    LabelDomain.Caption := rsDomain + ' ' + Query5_dhcp.RadioBtnVmnat.Caption
  else
    LabelDomain.Caption := rsDomain + ' ' + Query5_dhcp.EditDomain.Text;
  // Nameserver
  if Query5_dhcp.RadioBtnNameserver10.Checked then
    LabelNameserverGateway.Caption :=
      rsNameserver + ' ' + Query5_dhcp.RadioBtnNameserver10.Caption
  else if Query5_dhcp.RadioBtnNameserver172.Checked then
    LabelNameserverGateway.Caption :=
      rsNameserver + ' ' + Query5_dhcp.RadioBtnNameserver172.Caption
  else if Query5_dhcp.RadioBtnNameserver192.Checked then
    LabelNameserverGateway.Caption :=
      rsNameserver + ' ' + Query5_dhcp.RadioBtnNameserver192.Caption
  else
    LabelNameserverGateway.Caption :=
      rsNameserver + ' ' + Query5_dhcp.EditNameserver.Text;
  // Gateway
  if Query5_dhcp.RadioBtnGateway10.Checked then
    LabelNameserverGateway.Caption :=
      LabelNameserverGateway.Caption + ', ' + rsGateway + ' ' +
      Query5_dhcp.RadioBtnGateway10.Caption
  else if Query5_dhcp.RadioBtnGateway172.Checked then
    LabelNameserverGateway.Caption :=
      LabelNameserverGateway.Caption + ', ' + rsGateway + ' ' +
      Query5_dhcp.RadioBtnGateway172.Caption
  else if Query5_dhcp.RadioBtnGateway192.Checked then
    LabelNameserverGateway.Caption :=
      LabelNameserverGateway.Caption + ', ' + rsGateway + ' ' +
      Query5_dhcp.RadioBtnGateway192.Caption
  else
    LabelNameserverGateway.Caption :=
      LabelNameserverGateway.Caption + ', ' + rsGateway + ' ' +
      Query5_dhcp.EditGateway.Text;
  // Admin name
  LabelAdminNamePassword.Caption := rsAdminName + ' ' + Query6.EditNameAdmin.Text;
  // Admin password
  LabelAdminNamePassword.Caption :=
    LabelAdminNamePassword.Caption + ', ' + rsAdminPassword + ' ' +
    Query6.EditPasswordAdmin.Text;
  // IP name
  LabelIP.Caption := rsIPName + ' ' + Query6.EditNameIP.Text;
  // IP number
  LabelIP.Caption := LabelIP.Caption + ', ' + rsIPNumber + ' ' +
    Query6.EditNumberIP.Text;
  // UCS password
  LabelPasswordUCS.Caption := rsUCSPassword + ' ' + Query7.EditPasswordUCS.Text;
  // Copy modules
  if Query7.RadioBtnYes.Checked then
    LabelModules.Caption := rsCopyModules + ' ' + Query7.RadioBtnYes.Caption
  else
    LabelModules.Caption := rsCopyModules + ' ' + Query7.RadioBtnNo.Caption;
end;

procedure TOverview.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Query7.Close;
end;


procedure TOverview.BtnBackClick(Sender: TObject);
var
  bigger: integer;
begin
  bigger := -Query7.bigger;
  // showForm query7 with normal size
  Query7.Visible := True;
  Query7.Height := Height + 2 * bigger;
  Query7.Left := Left - bigger;
  Query7.Top := Top - bigger;
  Query7.Width := Width + 2 * bigger;
  Visible := False;

  Query7.BtnBack.Left := BtnBack.Left;
  Query7.BtnBack.Top := BtnBack.Top + 2 * bigger;
  Query7.BtnOverview.Left := Query7.Width - Query7.BtnBack.Left -
    Query7.BtnOverview.Width;
  Query7.BtnOverview.Top := BtnFinish.Top + 2 * bigger;
end;

end.
