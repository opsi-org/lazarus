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
    procedure BtnBackClick(Sender: TObject);
    procedure BtnNextClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private

  public

  end;

var
  Query3: TQuery3;

implementation

uses
  opsi_quick_install_unit_language,
  opsi_quick_install_unit_query2,
  opsi_quick_install_unit_query4;

{$R *.lfm}

{ TQuery3 }

procedure TQuery3.BtnNextClick(Sender: TObject);
begin
  Query4.Visible:=True;
  Query3.Visible:=False;
end;

procedure TQuery3.FormActivate(Sender: TObject);
begin
  Query3.Height:=QuickInstall.Height;
  Query3.Left:=QuickInstall.Left;
  Query3.Top:=QuickInstall.Top;
  Query3.Width:=QuickInstall.Width;

  BtnBack.Left:=QuickInstall.BtnBack.Left;
  BtnBack.Top:=QuickInstall.BtnBack.Top;

  BtnNext.Left:=QuickInstall.BtnNext.Left;
  BtnNext.Top:=QuickInstall.BtnNext.Top;
end;

procedure TQuery3.BtnBackClick(Sender: TObject);
begin
  Query2.Visible:=True;
  Query3.Visible:=False;
end;

end.

