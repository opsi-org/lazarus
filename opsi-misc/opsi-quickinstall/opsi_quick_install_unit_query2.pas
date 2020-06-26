unit opsi_quick_install_unit_query2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TQuery2 }

  TQuery2 = class(TForm)
    BackgrImage: TImage;
    BtnBack: TButton;
    BtnNext: TButton;
    Label1: TLabel;
    LabelRepoKind: TLabel;
    LabelBackend: TLabel;
    PanelUpdate: TPanel;
    PanelRepoKind: TPanel;
    PanelBackend: TPanel;
    RadioBtnFile: TRadioButton;
    RadioBtnMySql: TRadioButton;
    RadioBtnExperimental: TRadioButton;
    RadioBtnStable: TRadioButton;
    RadioBtnTesting: TRadioButton;
    RadioBtnYes: TRadioButton;
    RadioBtnNo: TRadioButton;
    procedure BtnBackClick(Sender: TObject);
    procedure BtnNextClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private

  public

  end;

var
  Query2: TQuery2;

implementation

uses
  opsi_quick_install_unit_language,
  opsi_quick_install_unit_query,
  opsi_quick_install_unit_query3;

{$R *.lfm}

{ TQuery2 }

procedure TQuery2.BtnNextClick(Sender: TObject);
begin
  Query3.Visible := True;
  Query2.Visible := False;
end;

procedure TQuery2.FormActivate(Sender: TObject);
begin
  Query2.Height:=QuickInstall.Height;
  Query2.Left:=QuickInstall.Left;
  Query2.Top:=QuickInstall.Top;
  Query2.Width:=QuickInstall.Width;

  BtnBack.Left:=QuickInstall.BtnBack.Left;
  BtnBack.Top:=QuickInstall.BtnBack.Top;

  BtnNext.Left:=QuickInstall.BtnNext.Left;
  BtnNext.Top:=QuickInstall.BtnNext.Top;
end;

procedure TQuery2.BtnBackClick(Sender: TObject);
begin
  Query.Visible := True;
  Query2.Visible := False;
end;

end.

