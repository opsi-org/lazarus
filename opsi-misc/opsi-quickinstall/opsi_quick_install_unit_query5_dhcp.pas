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
    RadioBtn0: TRadioButton;
    RadioBtn225: TRadioButton;
    RadioBtnGateway10: TRadioButton;
    RadioBtnGateway172: TRadioButton;
    RadioBtnGateway192: TRadioButton;
    RadioBtnOtherNameserver: TRadioButton;
    RadioBtnOtherGateway: TRadioButton;
    RadioBtnOtherNetmask: TRadioButton;
    RadioBtn10: TRadioButton;
    RadioBtn172: TRadioButton;
    RadioBtn192: TRadioButton;
    RadioBtnOtherAddress: TRadioButton;
    RadioBtnUcs: TRadioButton;
    RadioBtnUib: TRadioButton;
    RadioBtnVmnat: TRadioButton;
    RadioBtnOtherDomain: TRadioButton;
    RadioBtnNameserver10: TRadioButton;
    RadioBtnNameserver172: TRadioButton;
    RadioBtnNameserver192: TRadioButton;
    procedure BtnNextClick(Sender: TObject);
  private

  public

  end;

var
  Query5_dhcp: TQuery5_dhcp;

implementation

uses
  opsi_quick_install_unit_query6;

{$R *.lfm}

{ TQuery5_dhcp }

procedure TQuery5_dhcp.BtnNextClick(Sender: TObject);
begin
  Query6.ShowModal;
  Query5_dhcp.Close;
end;

end.

