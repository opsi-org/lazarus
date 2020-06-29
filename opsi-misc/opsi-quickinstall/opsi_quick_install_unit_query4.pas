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
    procedure BtnBackClick(Sender: TObject);
    procedure BtnNextClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private

  public

  end;

var
  Query4: TQuery4;

implementation

uses
  opsi_quick_install_unit_language,
  opsi_quick_install_unit_query3,
  opsi_quick_install_unit_query5_dhcp;

{$R *.lfm}

{ TQuery4 }

procedure TQuery4.BtnNextClick(Sender: TObject);
begin
  Query5_dhcp.Visible:=True;
  Query4.Visible:=False;
end;

procedure TQuery4.FormActivate(Sender: TObject);
begin
  Query4.Height:=QuickInstall.Height;
  Query4.Left:=QuickInstall.Left;
  Query4.Top:=QuickInstall.Top;
  Query4.Width:=QuickInstall.Width;

  BtnBack.Left:=QuickInstall.BtnBack.Left;
  BtnBack.Top:=QuickInstall.BtnBack.Top;

  BtnNext.Left:=QuickInstall.BtnNext.Left;
  BtnNext.Top:=QuickInstall.BtnNext.Top;
end;

procedure TQuery4.BtnBackClick(Sender: TObject);
begin
  Query3.Visible:=True;
  Query4.Visible:=False;
end;

end.

