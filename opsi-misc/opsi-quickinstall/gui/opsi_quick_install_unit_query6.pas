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
    BtnOverview: TButton;
    EditNameAdmin: TEdit;
    EditNameIP: TEdit;
    EditNumberIP: TEdit;
    EditPasswordAdmin: TEdit;
    InfoAdmin: TImage;
    LabelNameAdmin: TLabel;
    LabelNameIP: TLabel;
    LabelNumberIP: TLabel;
    LabelPasswordAdmin: TLabel;
    PanelNameAdmin: TPanel;
    PanelNameIP: TPanel;
    PanelNumberIP: TPanel;
    PanelPasswordAdmin: TPanel;
    procedure BtnBackClick(Sender: TObject);
    procedure BtnOverviewClick(Sender: TObject);
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
  opsi_quick_install_unit_query4,
  opsi_quick_install_unit_query5_dhcp,
  opsi_quick_install_unit_overview, opsi_quick_install_resourcestrings;

{$R *.lfm}

{ TQuery6 }

procedure TQuery6.BtnOverviewClick(Sender: TObject);
begin
  showForm(Overview, self);
  Overview.BtnBack.Left := BtnBack.Left;
  Overview.BtnBack.Top := BtnBack.Top;
  Overview.BtnFinish.Left := Overview.Width - Overview.BtnBack.Left -
    QuickInstall.BtnFinishWidth;
  Overview.BtnFinish.Top := BtnOverview.Top;

  {if QuickInstall.RadioBtnDefault.Checked then
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
  end;}
end;

procedure TQuery6.FormActivate(Sender: TObject);
begin
  SetBasics(self);
  BtnOverview.Left := Width - BtnBack.Left - QuickInstall.BtnOverviewWidth;
  InfoAdmin.OnClick := @QuickInstall.ShowHintOnClick;
  // text by resourcestrings
  LabelNameAdmin.Caption := rsAdminName;
  InfoAdmin.Hint := rsAdmin;
  LabelPasswordAdmin.Caption := rsAdminPassword;
  LabelNameIP.Caption := rsIPName;
  LabelNumberIP.Caption := rsIPNumber;
  BtnBack.Caption := rsBack;
  BtnOverview.Caption := rsOverview;
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
