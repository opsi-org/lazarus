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
  Overview.Enabled:=False;
  Password.Visible:=True;
end;

procedure TOverview.FormActivate(Sender: TObject);
var
  domains: string;
begin
  //ShowMessage(IntToStr(BtnFinish.Width));
  PanelFinish.Left := QuickInstall.panelLeft;
  MemoOverview.Left := QuickInstall.panelLeft;
  BackgrImage.Picture.LoadFromFile(QuickInstall.BackgrImageFileName);

  MemoOverview.Clear;
  // Opsi version
  if Query.RadioBtnOpsi41.Checked then
    MemoOverview.Lines.Add(rsOpsiVersionO + Query.RadioBtnOpsi41.Caption)
  else
    MemoOverview.Lines.Add(rsOpsiVersionO + Query.RadioBtnOpsi42.Caption);

  {Custom installation}
  if QuickInstall.RadioBtnCustom.Checked then
  begin
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
  end;

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
    if Query5_dhcp.RadioBtnMask1.Checked then
      MemoOverview.Lines.Add(rsNetmaskO + Query5_dhcp.RadioBtnMask1.Caption)
    else if Query5_dhcp.RadioBtnMask2.Checked then
      MemoOverview.Lines.Add(rsNetmaskO + Query5_dhcp.RadioBtnMask2.Caption)
    else
      MemoOverview.Lines.Add(rsNetmaskO + Query5_dhcp.EditNetmask.Text);
    // Network address
    if Query5_dhcp.RadioBtnAddress1.Checked then
      MemoOverview.Lines.Add(rsNetworkO + Query5_dhcp.RadioBtnAddress1.Caption)
    else if Query5_dhcp.RadioBtnAddress2.Checked then
      MemoOverview.Lines.Add(rsNetworkO + Query5_dhcp.RadioBtnAddress2.Caption)
    else if Query5_dhcp.RadioBtnAddress3.Checked then
      MemoOverview.Lines.Add(rsNetworkO + Query5_dhcp.RadioBtnAddress3.Caption)
    else
      MemoOverview.Lines.Add(rsNetworkO + Query5_dhcp.EditAddress.Text);
    // Domain
    {if Query5_dhcp.RadioBtnDomain1.Checked then
      MemoOverview.Lines.Add(rsDomainO + Query5_dhcp.RadioBtnDomain1.Caption)
    else if Query5_dhcp.RadioBtnDomain2.Checked then
      MemoOverview.Lines.Add(rsDomainO + Query5_dhcp.RadioBtnDomain2.Caption)
    else if Query5_dhcp.RadioBtnDomain3.Checked then
      MemoOverview.Lines.Add(rsDomainO + Query5_dhcp.RadioBtnDomain3.Caption)
    else
      MemoOverview.Lines.Add(rsDomainO + Query5_dhcp.EditDomain.Text);}
    domains := rsDomainO;
    if Query5_dhcp.CheckBoxDomain1.Checked then
       domains += Query5_dhcp.CheckBoxDomain1.Caption + ', ';
    if Query5_dhcp.CheckBoxDomain2.Checked then
      domains += Query5_dhcp.CheckBoxDomain2.Caption + ', ';
    if Query5_dhcp.CheckBoxDomain3.Checked then
      domains += Query5_dhcp.CheckBoxDomain3.Caption + ', ';
    if Query5_dhcp.CheckBoxOtherDomain.Checked then
      domains += Query5_dhcp.EditDomain.Text + ', ';
    delete(domains, domains.length-1,2);
    MemoOverview.Lines.Add(domains);
    // Nameserver
    if Query5_dhcp.RadioBtnNameserver1.Checked then
      MemoOverview.Lines.Add(
        rsNameserverO + Query5_dhcp.RadioBtnNameserver1.Caption)
    else if Query5_dhcp.RadioBtnNameserver2.Checked then
      MemoOverview.Lines.Add(
        rsNameserverO + Query5_dhcp.RadioBtnNameserver2.Caption)
    else if Query5_dhcp.RadioBtnNameserver3.Checked then
      MemoOverview.Lines.Add(
        rsNameserverO + Query5_dhcp.RadioBtnNameserver3.Caption)
    else
      MemoOverview.Lines.Add(
        rsNameserverO + Query5_dhcp.EditNameserver.Text);
    // Gateway
    if Query5_dhcp.RadioBtnGateway1.Checked then
      MemoOverview.Lines.Add(rsGatewayO + Query5_dhcp.RadioBtnGateway1.Caption)
    else if Query5_dhcp.RadioBtnGateway2.Checked then
      MemoOverview.Lines.Add(rsGatewayO + Query5_dhcp.RadioBtnGateway2.Caption)
    else if Query5_dhcp.RadioBtnGateway3.Checked then
      MemoOverview.Lines.Add(rsGatewayO + Query5_dhcp.RadioBtnGateway3.Caption)
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
  Caption := rsOverview;
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
