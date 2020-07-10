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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
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
  showForm(Query5_dhcp, self);
  Query5_dhcp.BtnBack.Left := BtnBack.Left;
  Query5_dhcp.BtnBack.Top := BtnBack.Top;
  Query5_dhcp.BtnNext.Left := BtnNext.Left;
  Query5_dhcp.BtnNext.Top := BtnNext.Top;
end;

procedure TQuery4.FormActivate(Sender: TObject);
begin
  AdjustPanelPosition(self);
  BackgrImage.Picture.LoadFromFile(QuickInstall.BackgrImageFileName);
end;

procedure TQuery4.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Query3.Close;
end;

procedure TQuery4.BtnBackClick(Sender: TObject);
begin
  showForm(Query3, self);
end;

end.

