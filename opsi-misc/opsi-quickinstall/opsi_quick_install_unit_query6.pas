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

resourcestring
  rsOverview = ' overview ';
  rsNext = ' next > ';

implementation

uses
  opsi_quick_install_unit_language,
  opsi_quick_install_unit_query4,
  opsi_quick_install_unit_query5_dhcp,
  opsi_quick_install_unit_query7, opsi_quick_install_unit_overview;

{$R *.lfm}

{ TQuery6 }

procedure TQuery6.BtnNextClick(Sender: TObject);
begin
  if QuickInstall.RadioBtnDefault.Checked then
  begin
    showForm(Overview, self);
    Overview.BtnBack.Left := BtnBack.Left;
    Overview.BtnBack.Top := BtnBack.Top;
    Overview.BtnFinish.Left :=
      Overview.Width - Overview.BtnBack.Left - Overview.BtnFinish.Width;
    Overview.BtnFinish.Top := BtnNext.Top;
  end
  else
  begin
    showForm(Query7, self);
    Query7.BtnBack.Left := BtnBack.Left;
    Query7.BtnBack.Top := BtnBack.Top;
    Query7.BtnOverview.Left :=
      Query7.Width - Query7.BtnBack.Left - QuickInstall.BtnOverviewWidth;
    Query7.BtnOverview.Top := BtnNext.Top;
  end;
end;

procedure TQuery6.FormActivate(Sender: TObject);
begin
  AdjustPanelPosition(self);
  BackgrImage.Picture.LoadFromFile(QuickInstall.BackgrImageFileName);
  if QuickInstall.RadioBtnDefault.Checked then
  begin
    BtnNext.Caption := rsOverview;
    BtnNext.Left := Width - BtnBack.Left - QuickInstall.BtnOverviewWidth;
  end
  else
  begin
    BtnNext.Caption := rsNext;
    BtnNext.Left := Width - BtnBack.Left - QuickInstall.BtnNextWidth;
  end;
end;

procedure TQuery6.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Query5_dhcp.Close;
end;

procedure TQuery6.BtnBackClick(Sender: TObject);
begin
  if Query4.RadioBtnDhcpYes.Checked then
    showForm(Query5_dhcp, self)
  else
    showForm(Query4, self);
end;

end.
