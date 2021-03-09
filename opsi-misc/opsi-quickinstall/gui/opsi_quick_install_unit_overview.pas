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
  opsi_quick_install_resourcestrings,
  opsi_quick_install_data,
  opsi_quick_install_unit_language,
  opsi_quick_install_unit_query6,
  opsi_quick_install_unit_password;

{$R *.lfm}

{ TOverview }

procedure TOverview.BtnFinishClick(Sender: TObject);
begin
  Overview.Enabled:=False;
  Password.Visible:=True;
end;

procedure TOverview.FormActivate(Sender: TObject);
begin
  //ShowMessage(IntToStr(BtnFinish.Width));
  PanelFinish.Left := QuickInstall.panelLeft;
  MemoOverview.Left := QuickInstall.panelLeft;
  BackgrImage.Picture.LoadFromFile(QuickInstall.BackgrImageFileName);

  MemoOverview.Clear;
  // Opsi version
  MemoOverview.Lines.Add(rsOpsiVersionO + Data.opsiVersion);

  {Custom installation}
  if Data.custom then
  begin
    MemoOverview.Lines.Add('');
    // Repository
    MemoOverview.Lines.Add(rsRepoO + Data.repo);
    MemoOverview.Lines.Add(rsProxyO + Data.proxy.OverviewEntry);
    // Proxy
    MemoOverview.Lines.Add(rsProxyO + Data.proxy.OverviewEntry);
    // Repository (no cache)
    MemoOverview.Lines.Add(rsRepoNoCacheO + Data.repoNoCache);

    MemoOverview.Lines.Add('');
    // Backend
    MemoOverview.Lines.Add(rsBackendO + Data.backend);
    // Copy modules
    MemoOverview.Lines.Add(rsCopyModulesO + Data.copyMod.OverviewEntry);
    // Repo kind
    MemoOverview.Lines.Add(rsRepoKindO + Data.repoKind);

    MemoOverview.Lines.Add('');
    // UCS password
    if Data.distroName = 'Univention' then
      MemoOverview.Lines.Add(rsUCSO + Data.ucsPassword);
    // Reboot
    MemoOverview.Lines.Add(rsRebootO + Data.reboot.OverviewEntry);
  end;

  {Both}
  // Dhcp
  if Data.dhcp.PropertyEntry = 'true' then
  begin
    MemoOverview.Lines.Add('');
    MemoOverview.Lines.Add(rsDhcpO + Data.dhcp.OverviewEntry);
    // TFTPROOT
    MemoOverview.Lines.Add(rsTFTPROOTO + Data.symlink);
    // Netmask
    MemoOverview.Lines.Add(rsNetmaskO + Data.netmask);
    // Network address
    MemoOverview.Lines.Add(rsNetworkO + Data.networkAddress);
    // Domain
    MemoOverview.Lines.Add(rsDomainO + Data.domain);
    // Nameservere
    MemoOverview.Lines.Add(rsNameserverO + Data.nameserver);
    // Gateway
    MemoOverview.Lines.Add(rsGatewayO + Data.gateway);
  end;

  MemoOverview.Lines.Add('');
  // Admin name
  MemoOverview.Lines.Add(rsAdminNameO + Data.adminName);
  // Admin password
  MemoOverview.Lines.Add(rsAdminPasswordO + Data.adminPassword);
  // IP name
  MemoOverview.Lines.Add(rsIPNameO + Data.ipName);
  // IP number
  MemoOverview.Lines.Add(rsIPNumberO + Data.ipNumber);

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
