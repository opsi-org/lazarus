unit opsi_quick_install_unit_query4;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TQuery4 }

  TQuery4 = class(TForm)
    BackgrImage: TImage;
    BtnBack: TButton;
    BtnNext: TButton;
    LabelFilePointer: TLabel;
    LabelTimeout: TLabel;
    PanelFilePointer: TPanel;
    PanelTimeout: TPanel;
    RadioBtnNo: TRadioButton;
    RadioBtnYes: TRadioButton;
    RadioBtnMenu: TRadioButton;
    RadioBtnNoMenu: TRadioButton;
    procedure BtnNextClick(Sender: TObject);
  private

  public

  end;

var
  Query4: TQuery4;

implementation

uses
  opsi_quick_install_unit_query5_dhcp;

{$R *.lfm}

{ TQuery4 }

procedure TQuery4.BtnNextClick(Sender: TObject);
begin
  Query5_dhcp.ShowModal;
  Query4.Close;
end;

end.

