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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private

  public

  end;

var
  Query6: TQuery6;

implementation

uses
  opsi_quick_install_unit_language,
  opsi_quick_install_unit_query3,
  opsi_quick_install_unit_query5_dhcp,
  opsi_quick_install_unit_query7;

{$R *.lfm}

{ TQuery6 }

procedure TQuery6.BtnNextClick(Sender: TObject);
begin
  showForm(Query7, self);
  Query7.BtnBack.Left := BtnBack.Left;
  Query7.BtnBack.Top := BtnBack.Top;
  if QuickInstall.ComboBoxLanguages.Text = 'Deutsch' then
    Query7.BtnFinish.Left := BtnNext.Left - 30
  else
    Query7.BtnFinish.Left := BtnNext.Left;
  Query7.BtnFinish.Top := BtnNext.Top;
end;

procedure TQuery6.FormActivate(Sender: TObject);
begin
  AdjustPanelPosition(self);
  BackgrImage.Picture.LoadFromFile(QuickInstall.BackgrImageFileName);
end;

procedure TQuery6.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Query5_dhcp.Close;
end;

procedure TQuery6.BtnBackClick(Sender: TObject);
begin
  if QuickInstall.RadioBtnDefault.Checked then
    showForm(QuickInstall, self)
  else
  if Query3.RadioBtnDhcpYes.Checked then
    showForm(Query5_dhcp, self)
  else
    showForm(Query3, self);
end;

end.
