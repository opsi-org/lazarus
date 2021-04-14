unit opsi_quick_install_unit_query5_dhcp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Process, osnetworkcalculator;

type

  { TQuery5_dhcp }

  TQuery5_dhcp = class(TForm)
    BackgrImage: TImage;
    BtnBack: TButton;
    BtnNext: TButton;
    EditNetmask: TEdit;
    EditAddress: TEdit;
    EditDomain: TEdit;
    EditNameserver: TEdit;
    EditGateway: TEdit;
    InfoNetwork: TImage;
    LabelGateway: TLabel;
    LabelNameserver: TLabel;
    LabelDomain: TLabel;
    LabelAddress: TLabel;
    LabelNetmask: TLabel;
    PanelGateway: TPanel;
    PanelNameserver: TPanel;
    PanelDomain: TPanel;
    PanelAddress: TPanel;
    PanelNetmask: TPanel;
    RadioBtnMask1: TRadioButton;
    RadioBtnMask2: TRadioButton;
    RadioBtnGateway1: TRadioButton;
    RadioBtnGateway2: TRadioButton;
    RadioBtnGateway3: TRadioButton;
    RadioBtnOtherNameserver: TRadioButton;
    RadioBtnOtherGateway: TRadioButton;
    RadioBtnOtherMask: TRadioButton;
    RadioBtnAddress1: TRadioButton;
    RadioBtnAddress2: TRadioButton;
    RadioBtnAddress3: TRadioButton;
    RadioBtnOtherAddress: TRadioButton;
    RadioBtnNameserver1: TRadioButton;
    RadioBtnNameserver2: TRadioButton;
    RadioBtnNameserver3: TRadioButton;
    RadioBtnMask3: TRadioButton;
    RadioBtnDomain1: TRadioButton;
    RadioBtnDomain2: TRadioButton;
    RadioBtnDomain3: TRadioButton;
    RadioBtnOtherDomain: TRadioButton;
    procedure BtnBackClick(Sender: TObject);
    procedure BtnNextClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private

  public

  end;

var
  Query5_dhcp: TQuery5_dhcp;

implementation

uses
  opsi_quick_install_resourcestrings,
  opsi_quick_install_data,
  opsi_quick_install_unit_language,
  opsi_quick_install_unit_query4,
  opsi_quick_install_unit_query6;

{$R *.lfm}

{ TQuery5_dhcp }

procedure TQuery5_dhcp.BtnNextClick(Sender: TObject);
begin
  // Make Data Entries
  // Netmask
  if RadioBtnMask1.Checked then
    Data.netmask := RadioBtnMask1.Caption
  else if RadioBtnMask2.Checked then
    Data.netmask := RadioBtnMask2.Caption
  else
    Data.netmask := EditNetmask.Text;
  // Network address
  if RadioBtnAddress1.Checked then
    Data.networkAddress := RadioBtnAddress1.Caption
  else if RadioBtnAddress2.Checked then
    Data.networkAddress := RadioBtnAddress2.Caption
  else if RadioBtnAddress3.Checked then
    Data.networkAddress := RadioBtnAddress3.Caption
  else
    Data.networkAddress := EditAddress.Text;
  // Domain
  if RadioBtnDomain1.Checked then
    Data.domain := RadioBtnDomain1.Caption
  else if RadioBtnDomain2.Checked then
    Data.domain := RadioBtnDomain2.Caption
  else if RadioBtnDomain3.Checked then
    Data.domain := RadioBtnDomain3.Caption
  else
    Data.domain := EditDomain.Text;
  // Nameserver
  if RadioBtnNameserver1.Checked then
    Data.nameserver := RadioBtnNameserver1.Caption
  else if RadioBtnNameserver2.Checked then
    Data.nameserver := RadioBtnNameserver2.Caption
  else if RadioBtnNameserver3.Checked then
    Data.nameserver := RadioBtnNameserver3.Caption
  else
    Data.nameserver := EditNameserver.Text;
  // Gateway
  if RadioBtnGateway1.Checked then
    Data.gateway := RadioBtnGateway1.Caption
  else if RadioBtnGateway2.Checked then
    Data.gateway := RadioBtnGateway2.Caption
  else if RadioBtnGateway3.Checked then
    Data.gateway := RadioBtnGateway3.Caption
  else
    Data.gateway := EditGateway.Text;

  showForm(Query6, self);
  Query6.BtnBack.Left := BtnBack.Left;
  Query6.BtnBack.Top := BtnBack.Top;
  Query6.BtnOverview.Left :=
    Query6.Width - Query6.BtnBack.Left - QuickInstall.BtnOverviewWidth;
  Query6.BtnOverview.Top := BtnNext.Top;
end;

procedure TQuery5_dhcp.FormActivate(Sender: TObject);
var
  NetworkDetails, network: array of string;
  index: integer;
begin
  SetBasics(self);

  // Make automatic suggestions on network details for the dhcp:
  // get details
  NetworkDetails := getNetworkDetails(['IP4.ADDRESS[1]', 'IP4.ADDRESS[2]',
    'IP4.ADDRESS[3]', 'IP4.DOMAIN[1]', 'IP4.DOMAIN[2]', 'IP4.DOMAIN[3]',
    'IP4.DNS[1]', 'IP4.DNS[2]', 'IP4.DNS[3]', 'IP4.GATEWAY']);
  // adjust captions of and show radiobuttons according to network details:
  // IP4.ADDRESS[1]
  index := 0;
  if NetworkDetails[index] <> '' then
  begin
    network := NetworkDetails[index].Split(['/']);
    RadioBtnMask1.Visible := True;
    RadioBtnMask1.Caption := getNetmaskByIP4adr(network[1]);
    RadioBtnAddress1.Visible := True;
    RadioBtnAddress1.Caption := getIP4NetworkByAdrAndMask(network[0], network[1]);
    // IP4.ADDRESS[2]
    index += 1;
    if NetworkDetails[index] <> '' then
    begin
      network := NetworkDetails[index].Split(['/']);
      RadioBtnMask2.Visible := True;
      RadioBtnMask2.Caption := getNetmaskByIP4adr(network[1]);
      RadioBtnAddress2.Visible := True;
      RadioBtnAddress2.Caption := getIP4NetworkByAdrAndMask(network[0], network[1]);
      // IP4.ADDRESS[3]
      index += 1;
      if NetworkDetails[index] <> '' then
      begin
        network := NetworkDetails[index].Split(['/']);
        RadioBtnMask3.Visible := True;
        RadioBtnMask3.Caption := getNetmaskByIP4adr(network[1]);
        RadioBtnAddress3.Visible := True;
        RadioBtnAddress3.Caption := getIP4NetworkByAdrAndMask(network[0], network[1]);
        // if too many radiobuttons (i.e. 3), move RadioBtnOtherMask down
        RadioBtnOtherMask.AnchorSide[akTop].Side := asrBottom;
        RadioBtnOtherMask.AnchorSide[akTop].Control := RadioBtnMask1;
        RadioBtnOtherMask.AnchorSide[akLeft].Side := asrLeft;
        RadioBtnOtherMask.AnchorSide[akLeft].Control := RadioBtnMask1;
        RadioBtnOtherMask.BorderSpacing.Left := 0;
        EditNetmask.AnchorSide[akTop].Side := asrBottom;
        EditNetmask.AnchorSide[akTop].Control := RadioBtnMask1;
        // and RadioBtnOtherAddress also
        RadioBtnOtherAddress.AnchorSide[akTop].Side := asrBottom;
        RadioBtnOtherAddress.AnchorSide[akTop].Control := RadioBtnAddress1;
        RadioBtnOtherAddress.AnchorSide[akLeft].Side := asrLeft;
        RadioBtnOtherAddress.AnchorSide[akLeft].Control := RadioBtnAddress1;
        RadioBtnOtherAddress.BorderSpacing.Left := 0;
        EditAddress.AnchorSide[akTop].Side := asrBottom;
        EditAddress.AnchorSide[akTop].Control := RadioBtnAddress1;
      end;
    end;
  end;
  // IP4.DOMAIN[1]
  index := 3;
  if NetworkDetails[index] <> '' then
  begin
    RadioBtnDomain1.Visible := True;
    RadioBtnDomain1.Caption := NetworkDetails[index];
    // IP4.DOMAIN[2]
    index += 1;
    if NetworkDetails[index] <> '' then
    begin
      RadioBtnDomain2.Visible := True;
      RadioBtnDomain2.Caption := NetworkDetails[index];
      // IP4.DOMAIN[3]
      index += 1;
      if NetworkDetails[index] <> '' then
      begin
        RadioBtnDomain3.Visible := True;
        RadioBtnDomain3.Caption := NetworkDetails[index];
        // if too many checkboxes (i.e. 3), move CheckBoxOtherDomain down
        RadioBtnOtherDomain.AnchorSide[akTop].Side := asrBottom;
        RadioBtnOtherDomain.AnchorSide[akTop].Control := RadioBtnDomain1;
        RadioBtnOtherDomain.AnchorSide[akLeft].Side := asrLeft;
        RadioBtnOtherDomain.AnchorSide[akLeft].Control := RadioBtnDomain1;
        RadioBtnOtherDomain.BorderSpacing.Left := 0;
        EditDomain.AnchorSide[akTop].Side := asrBottom;
        EditDomain.AnchorSide[akTop].Control := RadioBtnDomain1;
      end;
    end;
  end;
  // IP4.DNS[1]
  index := 6;
  if NetworkDetails[index] <> '' then
  begin
    RadioBtnNameserver1.Visible := True;
    RadioBtnNameserver1.Caption := NetworkDetails[index];
    // IP4.DNS[2]
    index += 1;
    if NetworkDetails[index] <> '' then
    begin
      RadioBtnNameserver2.Visible := True;
      RadioBtnNameserver2.Caption := NetworkDetails[index];
      // IP4.DNS[3]
      index += 1;
      if NetworkDetails[index] <> '' then
      begin
        RadioBtnNameserver3.Visible := True;
        RadioBtnNameserver3.Caption := NetworkDetails[index];
        // if too many radiobuttons (i.e. 3), move RadioBtnOtherNameserver down
        RadioBtnOtherNameserver.AnchorSide[akTop].Side := asrBottom;
        RadioBtnOtherNameserver.AnchorSide[akTop].Control := RadioBtnNameserver1;
        RadioBtnOtherNameserver.AnchorSide[akLeft].Side := asrLeft;
        RadioBtnOtherNameserver.AnchorSide[akLeft].Control := RadioBtnNameserver1;
        RadioBtnOtherNameserver.BorderSpacing.Left := 0;
        EditNameserver.AnchorSide[akTop].Side := asrBottom;
        EditNameserver.AnchorSide[akTop].Control := RadioBtnNameserver1;
      end;
    end;
  end;
  // IP4.GATEWAY
  index := 9;
  if NetworkDetails[index] <> '' then
  begin
    RadioBtnGateway1.Visible := True;
    RadioBtnGateway1.Caption := NetworkDetails[index];
  end;

  // text by resourcestrings
  Caption := 'Opsi Quick Install - ' + rsCapQueryDhcp;
  InfoNetwork.Hint := rsInfoNetwork;
  LabelNetmask.Caption := rsNetmask;
  RadioBtnOtherMask.Caption := rsNetmaskOther;
  LabelAddress.Caption := rsNetworkAddress;
  RadioBtnOtherAddress.Caption := rsNetworkAddressOther;
  LabelDomain.Caption := rsDomain;
  RadioBtnOtherDomain.Caption := rsDomainOther;
  LabelNameserver.Caption := rsNameserver;
  RadioBtnOtherNameserver.Caption := rsNameserverOther;
  LabelGateway.Caption := rsGateway;
  RadioBtnOtherGateway.Caption := rsGatewayOther;
  BtnBack.Caption := rsBack;
  BtnNext.Caption := rsNext;
end;

procedure TQuery5_dhcp.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Query4.Close;
end;

procedure TQuery5_dhcp.BtnBackClick(Sender: TObject);
begin
  showForm(Query4, self);
end;

end.
