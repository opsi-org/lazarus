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

implementation

uses
  opsi_quick_install_unit_language,
  opsi_quick_install_unit_query,
  opsi_quick_install_unit_query2,
  opsi_quick_install_unit_query4,
  opsi_quick_install_unit_query5_dhcp,
  opsi_quick_install_unit_query6,
  opsi_quick_install_unit_password,
  opsi_quick_install_resourcestrings;

{$R *.lfm}

{ TOverview }

procedure TOverview.BtnFinishClick(Sender: TObject);
begin
  Password.ShowModal;
end;

procedure TOverview.FormActivate(Sender: TObject);
{var
  prod: integer;}
begin
  //ShowMessage(IntToStr(BtnFinish.Width));
  PanelFinish.Left := QuickInstall.panelLeft;
  MemoOverview.Left := QuickInstall.panelLeft;
  BackgrImage.Picture.LoadFromFile(QuickInstall.BackgrImageFileName);

  MemoOverview.Clear;
  {Custom installation}
  if QuickInstall.RadioBtnCustom.Checked then
  begin
    // Opsi version
    if Query.RadioBtnOpsi41.Checked then
      MemoOverview.Lines.Add(rsOpsiVersionO + Query.RadioBtnOpsi41.Caption)
    else
      MemoOverview.Lines.Add(rsOpsiVersionO + Query.RadioBtnOpsi42.Caption);

    MemoOverview.Lines.Add('');
    // Repository
    if Query.RadioBtnRepo.Checked then
      MemoOverview.Lines.Add(rsRepoO + Query.EditDefaultRepoNoCache.Text)
    else
      MemoOverview.Lines.Add(rsRepoO + Query.EditRepo.Text);
    // Proxy
    if Query.RadioBtnNone.Checked then
      MemoOverview.Lines.Add(rsProxyO + Query.RadioBtnNone.Caption)
    else if Query.RadioBtnMyProxy.Checked then
      MemoOverview.Lines.Add(rsProxyO + Query.RadioBtnMyProxy.Caption)
    else
      MemoOverview.Lines.Add(rsProxyO + Query.EditProxy.Text);
    // Repository (no cache)
    if Query.RadioBtnRepoNoCache.Checked then
      MemoOverview.Lines.Add(rsRepoNoCacheO + Query.EditDefaultRepoNoCache.Text)
    else
      MemoOverview.Lines.Add(rsRepoNoCacheO + Query.EditOtherNoCache.Text);

    MemoOverview.Lines.Add('');
    // Backend
    if Query2.RadioBtnFile.Checked then
      MemoOverview.Lines.Add(rsBackendO + Query2.RadioBtnFile.Caption)
    else
      MemoOverview.Lines.Add(rsBackendO + Query2.RadioBtnMySql.Caption);
    // Copy modules
    if Query2.RadioBtnYesCopy.Checked then
      MemoOverview.Lines.Add(rsCopyModulesO + Query2.RadioBtnYesCopy.Caption)
    else
      MemoOverview.Lines.Add(rsCopyModulesO + Query2.RadioBtnNoCopy.Caption);

    MemoOverview.Lines.Add('');
    // Repo kind
    if Query2.RadioBtnExperimental.Checked then
      MemoOverview.Lines.Add(rsRepoKindO + Query2.RadioBtnExperimental.Caption)
    else if Query2.RadioBtnStable.Checked then
      MemoOverview.Lines.Add(rsRepoKindO + Query2.RadioBtnStable.Caption)
    else
      MemoOverview.Lines.Add(rsRepoKindO + Query2.RadioBtnTesting.Caption);

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
    if QuickInstall.distroName = 'Univention' then
      MemoOverview.Lines.Add(rsUCSO + Query4.EditPasswordUCS.Text);
    // Reboot
    if Query4.RadioBtnYes.Checked then
      MemoOverview.Lines.Add(rsRebootO + Query4.RadioBtnYes.Caption)
    else
      MemoOverview.Lines.Add(rsRebootO + Query4.RadioBtnNo.Caption);
  end
  {Standard installation}
  else
    // default opsi version is 4.2, rest ist not shown anyway
    MemoOverview.Lines.Add(rsOpsiVersionO + Query.RadioBtnOpsi42.Caption);

  {Both}
  MemoOverview.Lines.Add('');
  // Dhcp
  if Query4.RadioBtnDhcpYes.Checked then
  begin
    MemoOverview.Lines.Add(rsDhcpO + Query4.RadioBtnDhcpYes.Caption);
    // TFTPROOT
    if Query4.RadioBtnMenu.Checked then
      MemoOverview.Lines.Add(rsTFTPROOTO + Query4.RadioBtnMenu.Caption)
    else
      MemoOverview.Lines.Add(rsTFTPROOTO + Query4.RadioBtnNoMenu.Caption);
    // Netmask
    if Query5_dhcp.RadioBtnMask0.Checked then
      MemoOverview.Lines.Add(rsNetmaskO + Query5_dhcp.RadioBtnMask0.Caption)
    else if Query5_dhcp.RadioBtnMask225.Checked then
      MemoOverview.Lines.Add(rsNetmaskO + Query5_dhcp.RadioBtnMask225.Caption)
    else
      MemoOverview.Lines.Add(rsNetmaskO + Query5_dhcp.EditNetmask.Text);
    // Network address
    if Query5_dhcp.RadioBtnAddress10.Checked then
      MemoOverview.Lines.Add(rsNetworkO + Query5_dhcp.RadioBtnAddress10.Caption)
    else if Query5_dhcp.RadioBtnAddress172.Checked then
      MemoOverview.Lines.Add(rsNetworkO + Query5_dhcp.RadioBtnAddress172.Caption)
    else if Query5_dhcp.RadioBtnAddress192.Checked then
      MemoOverview.Lines.Add(rsNetworkO + Query5_dhcp.RadioBtnAddress192.Caption)
    else
      MemoOverview.Lines.Add(rsNetworkO + Query5_dhcp.EditAddress.Text);
    // Domain
    if Query5_dhcp.RadioBtnUcs.Checked then
      MemoOverview.Lines.Add(rsDomainO + Query5_dhcp.RadioBtnUcs.Caption)
    else if Query5_dhcp.RadioBtnUib.Checked then
      MemoOverview.Lines.Add(rsDomainO + Query5_dhcp.RadioBtnUib.Caption)
    else if Query5_dhcp.RadioBtnVmnat.Checked then
      MemoOverview.Lines.Add(rsDomainO + Query5_dhcp.RadioBtnVmnat.Caption)
    else
      MemoOverview.Lines.Add(rsDomainO + Query5_dhcp.EditDomain.Text);
    // Nameserver
    if Query5_dhcp.RadioBtnNameserver10.Checked then
      MemoOverview.Lines.Add(
        rsNameserverO + Query5_dhcp.RadioBtnNameserver10.Caption)
    else if Query5_dhcp.RadioBtnNameserver172.Checked then
      MemoOverview.Lines.Add(
        rsNameserverO + Query5_dhcp.RadioBtnNameserver172.Caption)
    else if Query5_dhcp.RadioBtnNameserver192.Checked then
      MemoOverview.Lines.Add(
        rsNameserverO + Query5_dhcp.RadioBtnNameserver192.Caption)
    else
      MemoOverview.Lines.Add(
        rsNameserverO + Query5_dhcp.EditNameserver.Text);
    // Gateway
    if Query5_dhcp.RadioBtnGateway10.Checked then
      MemoOverview.Lines.Add(rsGatewayO + Query5_dhcp.RadioBtnGateway10.Caption)
    else if Query5_dhcp.RadioBtnGateway172.Checked then
      MemoOverview.Lines.Add(rsGatewayO + Query5_dhcp.RadioBtnGateway172.Caption)
    else if Query5_dhcp.RadioBtnGateway192.Checked then
      MemoOverview.Lines.Add(rsGatewayO + Query5_dhcp.RadioBtnGateway192.Caption)
    else
      MemoOverview.Lines.Add(rsGatewayO + Query5_dhcp.EditGateway.Text);
  end
  else
    MemoOverview.Lines.Add(rsDhcpO + Query4.RadioBtnDhcpNo.Caption);

  MemoOverview.Lines.Add('');
  // Admin name
  MemoOverview.Lines.Add(rsAdminNameO + Query6.EditNameAdmin.Text);
  // Admin password
  MemoOverview.Lines.Add(rsAdminPasswordO + Query6.EditPasswordAdmin.Text);
  // IP name
  MemoOverview.Lines.Add(rsIPNameO + Query6.EditNameIP.Text);
  // IP number
  MemoOverview.Lines.Add(rsIPNumberO + Query6.EditNumberIP.Text);


  // text by resourcestrings
  LabelFinish.Caption := rsStartInstallation;
  BtnBack.Caption := rsBack;
  BtnFinish.Caption := rsFinish;
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
