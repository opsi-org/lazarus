unit opsi_quick_install_unit_query5_dhcp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

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
    RadioBtnMask0: TRadioButton;
    RadioBtnMask225: TRadioButton;
    RadioBtnGateway10: TRadioButton;
    RadioBtnGateway172: TRadioButton;
    RadioBtnGateway192: TRadioButton;
    RadioBtnOtherNameserver: TRadioButton;
    RadioBtnOtherGateway: TRadioButton;
    RadioBtnOtherMask: TRadioButton;
    RadioBtnAddress10: TRadioButton;
    RadioBtnAddress172: TRadioButton;
    RadioBtnAddress192: TRadioButton;
    RadioBtnOtherAddress: TRadioButton;
    RadioBtnUcs: TRadioButton;
    RadioBtnUib: TRadioButton;
    RadioBtnVmnat: TRadioButton;
    RadioBtnOtherDomain: TRadioButton;
    RadioBtnNameserver10: TRadioButton;
    RadioBtnNameserver172: TRadioButton;
    RadioBtnNameserver192: TRadioButton;
    procedure BtnBackClick(Sender: TObject);
    procedure BtnNextClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private

  public

  end;

var
  Query5_dhcp: TQuery5_dhcp;

implementation

uses
  opsi_quick_install_unit_language,
  opsi_quick_install_unit_query4,
  opsi_quick_install_unit_query6;

{$R *.lfm}

{ TQuery5_dhcp }

procedure TQuery5_dhcp.BtnNextClick(Sender: TObject);
begin
  Query6.Visible:=True;
  Query5_dhcp.Visible:=False;
end;

procedure TQuery5_dhcp.FormActivate(Sender: TObject);
begin
  Query5_dhcp.Height:=QuickInstall.Height;
  Query5_dhcp.Left:=QuickInstall.Left;
  Query5_dhcp.Top:=QuickInstall.Top;
  Query5_dhcp.Width:=QuickInstall.Width;

  BtnBack.Left:=QuickInstall.BtnBack.Left;
  BtnBack.Top:=QuickInstall.BtnBack.Top;

  BtnNext.Left:=QuickInstall.BtnNext.Left;
  BtnNext.Top:=QuickInstall.BtnNext.Top;
end;

procedure TQuery5_dhcp.BtnBackClick(Sender: TObject);
begin
  Query4.Visible:=True;
  Query5_dhcp.Visible:=False;
end;

end.

