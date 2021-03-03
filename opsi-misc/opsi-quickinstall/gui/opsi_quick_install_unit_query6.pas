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
    procedure EditNameAdminChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private

  public

  end;

var
  Query6: TQuery6;

implementation

uses
  opsi_quick_install_resourcestrings,
  opsi_quick_install_data,
  opsi_quick_install_unit_language,
  opsi_quick_install_unit_query4,
  opsi_quick_install_unit_query5_dhcp,
  opsi_quick_install_unit_overview;

{$R *.lfm}

{ TQuery6 }

procedure TQuery6.BtnOverviewClick(Sender: TObject);
begin
  // Make Data Entries
  // Admin name
  Data.adminName := EditNameAdmin.Text;
  // Admin password
  Data.adminPassword := EditPasswordAdmin.Text;
  // IP name
  Data.ipName := EditNameIP.Text;
  // IP number
  Data.ipNumber := EditNumberIP.Text;

  showForm(Overview, self);
  Overview.BtnBack.Left := BtnBack.Left;
  Overview.BtnBack.Top := BtnBack.Top;
  Overview.BtnFinish.Left := Overview.Width - Overview.BtnBack.Left -
    QuickInstall.BtnFinishWidth;
  Overview.BtnFinish.Top := BtnOverview.Top;
end;

procedure TQuery6.EditNameAdminChange(Sender: TObject);
begin
  // ask for admin password only if a new admin is wanted
  if EditNameAdmin.Text = '' then
    PanelPasswordAdmin.Visible := False
  else
    PanelPasswordAdmin.Visible := True;
end;

procedure TQuery6.FormActivate(Sender: TObject);
begin
  SetBasics(self);
  // text by resourcestrings
  Caption := 'Opsi Quick Install - ' + rsCapQueryUserInfo;
  LabelNameAdmin.Caption := rsAdminName;
  InfoAdmin.Hint := rsInfoAdmin;
  LabelPasswordAdmin.Caption := rsAdminPassword;
  LabelNameIP.Caption := rsIPName;
  LabelNumberIP.Caption := rsIPNumber;
  BtnBack.Caption := rsBack;
  BtnOverview.Caption := rsOverviewBtn;
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
