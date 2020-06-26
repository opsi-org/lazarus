unit opsi_quick_install_unit_query6;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TQuery6 }

  TQuery6 = class(TForm)
    BackgrImage: TImage;
    BtnBack: TButton;
    BtnNext: TButton;
    EditNameAdmin: TEdit;
    EditNameIP: TEdit;
    EditNumberIP: TEdit;
    EditPasswordAdmin: TEdit;
    LabelNameAdmin: TLabel;
    LabelNameIP: TLabel;
    LabelNameIP1: TLabel;
    LabelPasswordAdmin: TLabel;
    PanelNameAdmin: TPanel;
    PanelNameIP: TPanel;
    PanelNumberIP: TPanel;
    PanelPasswordAdmin: TPanel;
    procedure BtnBackClick(Sender: TObject);
    procedure BtnNextClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private

  public

  end;

var
  Query6: TQuery6;

implementation

uses
  opsi_quick_install_unit_language,
  opsi_quick_install_unit_query5_dhcp,
  opsi_quick_install_unit_query7;

{$R *.lfm}

{ TQuery6 }

procedure TQuery6.BtnNextClick(Sender: TObject);
begin
  Query7.Visible:=True;
  Query6.Visible:=False;
end;

procedure TQuery6.FormActivate(Sender: TObject);
begin
  Query6.Height:=QuickInstall.Height;
  Query6.Left:=QuickInstall.Left;
  Query6.Top:=QuickInstall.Top;
  Query6.Width:=QuickInstall.Width;

  BtnBack.Left:=QuickInstall.BtnBack.Left;
  BtnBack.Top:=QuickInstall.BtnBack.Top;

  BtnNext.Left:=QuickInstall.BtnNext.Left;
  BtnNext.Top:=QuickInstall.BtnNext.Top;
end;

procedure TQuery6.BtnBackClick(Sender: TObject);
begin
  Query5_dhcp.Visible:=True;
  Query6.Visible:=False;
end;

end.

