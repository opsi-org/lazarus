unit opsi_quick_install_unit_query5_dhcp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Process;

type

  { TQuery5_dhcp }

  TQuery5_dhcp = class(TForm)
    BackgrImage: TImage;
    BtnBack: TButton;
    BtnNext: TButton;
    CheckBoxOtherDomain: TCheckBox;
    CheckBoxDomain1: TCheckBox;
    CheckBoxDomain2: TCheckBox;
    CheckBoxDomain3: TCheckBox;
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
  opsi_quick_install_unit_language,
  opsi_quick_install_unit_query4,
  opsi_quick_install_unit_query6, opsi_quick_install_resourcestrings;

{$R *.lfm}

{ TQuery5_dhcp }

procedure TQuery5_dhcp.BtnNextClick(Sender: TObject);
begin
  showForm(Query6, self);
  Query6.BtnBack.Left := BtnBack.Left;
  Query6.BtnBack.Top := BtnBack.Top;
  Query6.BtnOverview.Left :=
    Query6.Width - Query6.BtnBack.Left - QuickInstall.BtnOverviewWidth;
  Query6.BtnOverview.Top := BtnNext.Top;
end;

procedure TQuery5_dhcp.FormActivate(Sender: TObject);
var
  NetworkDetails, radioBtnCaption, topic: string;
  index: integer;
begin
  SetBasics(self);
  InfoNetwork.OnClick := @QuickInstall.ShowHintOnClick;
  if RunCommand('/bin/sh', ['-c', 'echo | nmcli dev show'], NetworkDetails) then
  begin
    //ShowMessage(NetworkDetails);

    topic := 'IP4.ADDRESS[1]:';
    radioBtnCaption := '';
    index := NetworkDetails.IndexOf(topic);
    if index <> -1 then
    begin
      index += topic.Length + 1;
      while NetworkDetails[index] = ' ' do
        index += 1;
      while (NetworkDetails[index] <> ' ') and (NetworkDetails[index] <> #10) do
      begin
        radioBtnCaption += NetworkDetails[index];
        index += 1;
      end;
      RadioBtnAddress1.Visible:=True;
      RadioBtnAddress1.Caption := radioBtnCaption;
    end;


    topic := 'IP4.DOMAIN[1]:';
    radioBtnCaption := '';
    index := NetworkDetails.IndexOf(topic);
    if index <> -1 then
    begin
      index += topic.Length + 1;
      while NetworkDetails[index] = ' ' do
        index += 1;
      while (NetworkDetails[index] <> ' ') and (NetworkDetails[index] <> #10) do
      begin
        radioBtnCaption += NetworkDetails[index];
        index += 1;
      end;
      CheckBoxDomain1.Visible:=True;
      CheckBoxDomain1.Caption := radioBtnCaption;

      topic := 'IP4.DOMAIN[2]:';
      radioBtnCaption := '';
      index := NetworkDetails.IndexOf(topic);
      if index <> -1 then
      begin
        index += topic.Length + 1;
        while NetworkDetails[index] = ' ' do
          index += 1;
        while (NetworkDetails[index] <> ' ') and (NetworkDetails[index] <> #10) do
        begin
          radioBtnCaption += NetworkDetails[index];
          index += 1;
        end;
        CheckBoxDomain2.Visible:=True;
        CheckBoxDomain2.Caption := radioBtnCaption;

        topic := 'IP4.DOMAIN[3]:';
        radioBtnCaption := '';
        index := NetworkDetails.IndexOf(topic);
        if index <> -1 then
        begin
          index += topic.Length + 1;
          while NetworkDetails[index] = ' ' do
            index += 1;
          while (NetworkDetails[index] <> ' ') and (NetworkDetails[index] <> #10) do
          begin
            radioBtnCaption += NetworkDetails[index];
            index += 1;
          end;
          CheckBoxDomain3.Visible:=True;
          CheckBoxDomain3.Caption := radioBtnCaption;
        end;
      end;
    end;

    topic := 'IP4.DNS[1]:';
    radioBtnCaption := '';
    index := NetworkDetails.IndexOf(topic);
    if index <> -1 then
    begin
      index += topic.Length + 1;
      while NetworkDetails[index] = ' ' do
        index += 1;
      while (NetworkDetails[index] <> ' ') and (NetworkDetails[index] <> #10) do
      begin
        radioBtnCaption += NetworkDetails[index];
        index += 1;
      end;
      RadioBtnNameserver1.Visible:=True;
      RadioBtnNameserver1.Caption := radioBtnCaption;

      topic := 'IP4.DNS[2]:';
      radioBtnCaption := '';
      index := NetworkDetails.IndexOf(topic);
      if index <> -1 then
      begin
        index += topic.Length + 1;
        while NetworkDetails[index] = ' ' do
          index += 1;
        while (NetworkDetails[index] <> ' ') and (NetworkDetails[index] <> #10) do
        begin
          radioBtnCaption += NetworkDetails[index];
          index += 1;
        end;
        RadioBtnNameserver2.Visible:=True;
        RadioBtnNameserver2.Caption := radioBtnCaption;

        topic := 'IP4.DNS[3]:';
        radioBtnCaption := '';
        index := NetworkDetails.IndexOf(topic);
        if index <> -1 then
        begin
          index += topic.Length + 1;
          while NetworkDetails[index] = ' ' do
            index += 1;
          while (NetworkDetails[index] <> ' ') and (NetworkDetails[index] <> #10) do
          begin
            radioBtnCaption += NetworkDetails[index];
            index += 1;
          end;
          RadioBtnNameserver3.Visible:=True;
          RadioBtnNameserver3.Caption := radioBtnCaption;
        end;
      end;
    end;


    topic := 'IP4.GATEWAY:';
    radioBtnCaption := '';
    index := NetworkDetails.IndexOf(topic);
    if index <> -1 then
    begin
      index += topic.Length + 1;
      while NetworkDetails[index] = ' ' do
        index += 1;
      while (NetworkDetails[index] <> ' ') and (NetworkDetails[index] <> #10) do
      begin
        radioBtnCaption += NetworkDetails[index];
        index += 1;
      end;
      RadioBtnGateway1.Visible:=True;
      RadioBtnGateway1.Caption := radioBtnCaption;
    end;

  end;


  // text by resourcestrings
  Caption := 'Opsi Quick Install - ' + rsCapQueryDhcp;
  InfoNetwork.Hint := rsInfoNetwork;
  LabelNetmask.Caption := rsNetmask;
  RadioBtnOtherMask.Caption := rsNetmaskOther;
  LabelAddress.Caption := rsNetworkAddress;
  RadioBtnOtherAddress.Caption := rsNetworkAddressOther;
  LabelDomain.Caption := rsDomain;
  CheckBoxOtherDomain.Caption := rsDomainOther;
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
