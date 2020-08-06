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
  opsi_quick_install_unit_query6;

{$R *.lfm}

{ TQuery5_dhcp }

procedure TQuery5_dhcp.BtnNextClick(Sender: TObject);
begin
  showForm(Query6, self);
  Query6.BtnBack.Left := BtnBack.Left;
  Query6.BtnBack.Top := BtnBack.Top;
  Query6.BtnNext.Left := Query6.Width - Query6.BtnBack.Left - Query6.BtnNext.Width;
  Query6.BtnNext.Top := BtnNext.Top;
end;

procedure TQuery5_dhcp.FormActivate(Sender: TObject);
begin
  AdjustPanelPosition(self);
  BackgrImage.Picture.LoadFromFile(QuickInstall.BackgrImageFileName);
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


