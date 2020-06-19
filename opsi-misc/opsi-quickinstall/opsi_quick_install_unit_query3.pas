unit opsi_quick_install_unit_query3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TQuery3 }

  TQuery3 = class(TForm)
    BackgrImage: TImage;
    BtnBack: TButton;
    BtnNext: TButton;
    EditOpsiProducts: TEdit;
    LabelDhcp: TLabel;
    LabelReboot: TLabel;
    LabelOpsiProducts: TLabel;
    PanelDhcp: TPanel;
    PanelReboot: TPanel;
    PanelOpsiProducts: TPanel;
    RadioBtnNo: TRadioButton;
    RadioBtnDhcpNo: TRadioButton;
    RadioBtnYes: TRadioButton;
    RadioBtnDhcpYes: TRadioButton;
    procedure BtnNextClick(Sender: TObject);
  private

  public

  end;

var
  Query3: TQuery3;

implementation

uses
  opsi_quick_install_unit_query4;

{$R *.lfm}

{ TQuery3 }

procedure TQuery3.BtnNextClick(Sender: TObject);
begin
  Query4.ShowModal;
  Query3.Close;
end;

end.

