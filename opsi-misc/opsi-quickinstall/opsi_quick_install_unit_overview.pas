unit opsi_quick_install_unit_overview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  MaskEdit;

type

  { TOverview }

  TOverview = class(TForm)
    BackgrImage: TImage;
    BtnBack: TButton;
    BtnFinish: TButton;
    LabelFinish: TLabel;
    MemoOverview: TMemo;
    PanelFinish: TPanel;
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
  rsOpsiVersion = 'Opsi version to install: ';
  rsRepo = 'Repository: ';
  rsProxy = 'Proxy: ';
  rsRepoNoCache = 'Repository (without cache proxy): ';
  rsBackend = 'Backend: ';
  rsRepoKind = 'Repo kind: ';
  rsUpdate = 'Install from stable, update to repo kind: ';
  rsProds = 'Products in depot: ';
  rsReboot = 'Reboot after script is finished: ';
  rsOpsiDhcpServer = 'Run opsi dhcp server: ';
  rsElilo = 'Elilo.efi has timeout of 2 seconds: ';
  rsTFTPROOT = 'TFTPROOT symlink points to: ';
  rsNetmask = 'Netmask: ';
  rsNetwork = 'Network address: ';
  rsDomain = 'DNS Domain: ';
  rsNameserver = 'Primary nameserver: ';
  rsGateway = 'Gateway: ';
  rsAdminName = 'Opsi admin user name: ';
  rsAdminPassword = 'Opsi admin user password: ';
  rsIPName = 'IP name: ';
  rsIPNumber = 'IP number: ';
  rsUseUCS = 'Use UCS: ';
  rsUCSPassword = 'Password of administrator of UCS domain controller: ';
  rsCopyModules = 'Copy modules: ';

implementation

uses
  opsi_quick_install_unit_language,
  opsi_quick_install_unit_query,
  opsi_quick_install_unit_query2,
  opsi_quick_install_unit_query_prods,
  opsi_quick_install_unit_query4,
  opsi_quick_install_unit_query5_dhcp,
  opsi_quick_install_unit_query6,
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
  //ShowMessage(IntToStr(BtnFinish.Width));
  PanelFinish.Left := QuickInstall.panelLeft;
  MemoOverview.Left := QuickInstall.panelLeft;
  BackgrImage.Picture.LoadFromFile(QuickInstall.BackgrImageFileName);

  MemoOverview.Clear;
  // Opsi version
  if Query.RadioBtnOpsi41.Checked then
    MemoOverview.Lines.Add(rsOpsiVersion + Query.RadioBtnOpsi41.Caption)
  else
    MemoOverview.Lines.Add(rsOpsiVersion + Query.RadioBtnOpsi42.Caption);

  MemoOverview.Lines.Add('');
  // Repository
  if (Query.RadioBtnOpsi41.Checked) and (Query.RadioBtnRepo.Checked) then
    MemoOverview.Lines.Add(rsRepo + QuickInstall.baseURLOpsi41 +
      QuickInstall.DistrUrlPart)
  else if (Query.RadioBtnOpsi42.Checked) and (Query.RadioBtnRepo.Checked) then
    MemoOverview.Lines.Add(rsRepo + QuickInstall.baseURLOpsi42 +
      QuickInstall.DistrUrlPart)
  else
    MemoOverview.Lines.Add(rsRepo + Query.EditRepo.Text);
  // Proxy
  if Query.RadioBtnNone.Checked then
    MemoOverview.Lines.Add(rsProxy + Query.RadioBtnNone.Caption)
  else if Query.RadioBtnMyProxy.Checked then
    MemoOverview.Lines.Add(rsProxy + Query.RadioBtnMyProxy.Caption)
  else
    MemoOverview.Lines.Add(rsProxy + Query.EditProxy.Text);
  // Repository (no cache)
  if (Query.RadioBtnOpsi41.Checked) and (Query.RadioBtnRepo.Checked) then
    MemoOverview.Lines.Add(rsRepoNoCache + QuickInstall.baseURLOpsi41 +
      QuickInstall.DistrUrlPart)
  else if (Query.RadioBtnOpsi42.Checked) and (Query.RadioBtnRepo.Checked) then
    MemoOverview.Lines.Add(rsRepoNoCache + QuickInstall.baseURLOpsi42 +
      QuickInstall.DistrUrlPart)
  else
    MemoOverview.Lines.Add(rsRepoNoCache + Query.EditOtherNoCache.Text);

  MemoOverview.Lines.Add('');
  // Backend
  if Query2.RadioBtnFile.Checked then
    MemoOverview.Lines.Add(rsBackend + Query2.RadioBtnFile.Caption)
  else
    MemoOverview.Lines.Add(rsBackend + Query2.RadioBtnMySql.Caption);
  // Copy modules
  if Query2.RadioBtnYes.Checked then
    MemoOverview.Lines.Add(rsCopyModules + Query2.RadioBtnYes.Caption)
  else
    MemoOverview.Lines.Add(rsCopyModules + Query2.RadioBtnNo.Caption);

  MemoOverview.Lines.Add('');
  // Repo kind
  if Query2.RadioBtnExperimental.Checked then
    MemoOverview.Lines.Add(rsRepoKind + Query2.RadioBtnExperimental.Caption)
  else if Query2.RadioBtnStable.Checked then
    MemoOverview.Lines.Add(rsRepoKind + Query2.RadioBtnStable.Caption)
  else
    MemoOverview.Lines.Add(rsRepoKind + Query2.RadioBtnTesting.Caption);
  // Update
  if Query2.RadioBtnYes.Checked then
    MemoOverview.Lines.Add(rsUpdate + Query2.RadioBtnYes.Caption)
  else
    MemoOverview.Lines.Add(rsUpdate + Query2.RadioBtnNo.Caption);

  {MemoOverview.Lines.Add('');
  // Prods
  MemoOverview.Lines.Add(rsProds);
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
  MemoOverview.Lines.Add(stringProducts);}

  MemoOverview.Lines.Add('');
  // UCS password
  MemoOverview.Lines.Add(rsUCSPassword + Query4.EditPasswordUCS.Text);
  // Reboot
  if Query4.RadioBtnYes.Checked then
    MemoOverview.Lines.Add(rsReboot + Query4.RadioBtnYes.Caption)
  else
    MemoOverview.Lines.Add(rsReboot + Query4.RadioBtnNo.Caption);

  MemoOverview.Lines.Add('');
  // Dhcp
  if Query4.RadioBtnDhcpYes.Checked then
  begin
    MemoOverview.Lines.Add(rsOpsiDhcpServer + Query4.RadioBtnDhcpYes.Caption);
    // TFTPROOT
    if Query4.RadioBtnMenu.Checked then
      MemoOverview.Lines.Add(rsTFTPROOT + Query4.RadioBtnMenu.Caption)
    else
      MemoOverview.Lines.Add(rsTFTPROOT + Query4.RadioBtnNoMenu.Caption);
    // Netmask
    if Query5_dhcp.RadioBtnMask0.Checked then
      MemoOverview.Lines.Add(rsNetmask + Query5_dhcp.RadioBtnMask0.Caption)
    else if Query5_dhcp.RadioBtnMask225.Checked then
      MemoOverview.Lines.Add(rsNetmask + Query5_dhcp.RadioBtnMask225.Caption)
    else
      MemoOverview.Lines.Add(rsNetmask + Query5_dhcp.EditNetmask.Text);
    // Network address
    if Query5_dhcp.RadioBtnAddress10.Checked then
      MemoOverview.Lines.Add(rsNetwork + Query5_dhcp.RadioBtnAddress10.Caption)
    else if Query5_dhcp.RadioBtnAddress172.Checked then
      MemoOverview.Lines.Add(rsNetwork + Query5_dhcp.RadioBtnAddress172.Caption)
    else if Query5_dhcp.RadioBtnAddress192.Checked then
      MemoOverview.Lines.Add(rsNetwork + Query5_dhcp.RadioBtnAddress192.Caption)
    else
      MemoOverview.Lines.Add(rsNetwork + Query5_dhcp.EditAddress.Text);
    // Domain
    if Query5_dhcp.RadioBtnUcs.Checked then
      MemoOverview.Lines.Add(rsDomain + Query5_dhcp.RadioBtnUcs.Caption)
    else if Query5_dhcp.RadioBtnUib.Checked then
      MemoOverview.Lines.Add(rsDomain + Query5_dhcp.RadioBtnUib.Caption)
    else if Query5_dhcp.RadioBtnVmnat.Checked then
      MemoOverview.Lines.Add(rsDomain + Query5_dhcp.RadioBtnVmnat.Caption)
    else
      MemoOverview.Lines.Add(rsDomain + Query5_dhcp.EditDomain.Text);
    // Nameserver
    if Query5_dhcp.RadioBtnNameserver10.Checked then
      MemoOverview.Lines.Add(
        rsNameserver + Query5_dhcp.RadioBtnNameserver10.Caption)
    else if Query5_dhcp.RadioBtnNameserver172.Checked then
      MemoOverview.Lines.Add(
        rsNameserver + Query5_dhcp.RadioBtnNameserver172.Caption)
    else if Query5_dhcp.RadioBtnNameserver192.Checked then
      MemoOverview.Lines.Add(
        rsNameserver + Query5_dhcp.RadioBtnNameserver192.Caption)
    else
      MemoOverview.Lines.Add(
        rsNameserver + Query5_dhcp.EditNameserver.Text);
    // Gateway
    if Query5_dhcp.RadioBtnGateway10.Checked then
      MemoOverview.Lines.Add(rsGateway + Query5_dhcp.RadioBtnGateway10.Caption)
    else if Query5_dhcp.RadioBtnGateway172.Checked then
      MemoOverview.Lines.Add(rsGateway + Query5_dhcp.RadioBtnGateway172.Caption)
    else if Query5_dhcp.RadioBtnGateway192.Checked then
      MemoOverview.Lines.Add(rsGateway + Query5_dhcp.RadioBtnGateway192.Caption)
    else
      MemoOverview.Lines.Add(rsGateway + Query5_dhcp.EditGateway.Text);
  end
  else
    MemoOverview.Lines.Add(rsOpsiDhcpServer + Query4.RadioBtnDhcpNo.Caption);

  MemoOverview.Lines.Add('');
  // Admin name
  MemoOverview.Lines.Add(rsAdminName + Query6.EditNameAdmin.Text);
  // Admin password
  MemoOverview.Lines.Add(rsAdminPassword + Query6.EditPasswordAdmin.Text);
  // IP name
  MemoOverview.Lines.Add(rsIPName + Query6.EditNameIP.Text);
  // IP number
  MemoOverview.Lines.Add(rsIPNumber + Query6.EditNumberIP.Text);
end;

procedure TOverview.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Query6.Close;
end;


procedure TOverview.BtnBackClick(Sender: TObject);
begin
  showForm(Query6, self);
end;

end.
