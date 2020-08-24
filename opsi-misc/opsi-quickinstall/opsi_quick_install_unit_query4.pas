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
    EditPasswordUCS: TEdit;
    LabelDhcp: TLabel;
    LabelFilePointer: TLabel;
    LabelPasswordMasterAdmin: TLabel;
    LabelReboot: TLabel;
    BigPanel: TPanel;
    PanelPasswordMasterAdmin: TPanel;
    PanelRadiofilePointer: TPanel;
    PanelRadioDhcp: TPanel;
    PanelDhcp: TPanel;
    PanelFilePointer: TPanel;
    PanelRadioReboot: TPanel;
    PanelReboot: TPanel;
    RadioBtnDhcpNo: TRadioButton;
    RadioBtnDhcpYes: TRadioButton;
    RadioBtnMenu: TRadioButton;
    RadioBtnNo: TRadioButton;
    RadioBtnNoMenu: TRadioButton;
    RadioBtnYes: TRadioButton;
    procedure BtnBackClick(Sender: TObject);
    procedure BtnNextClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure RadioBtnDhcpYesChange(Sender: TObject);
  private

  public

  end;

var
  Query4: TQuery4;

implementation

uses
  opsi_quick_install_unit_language,
  opsi_quick_install_unit_query2,
  opsi_quick_install_unit_query5_dhcp,
  opsi_quick_install_unit_query6, opsi_quick_install_resourcestrings;

{$R *.lfm}

{ TQuery4 }

procedure TQuery4.BtnNextClick(Sender: TObject);
begin
  if RadioBtnDhcpYes.Checked then
  begin
    showForm(Query5_dhcp, self);
    Query5_dhcp.BtnBack.Left := BtnBack.Left;
    Query5_dhcp.BtnBack.Top := BtnBack.Top;
    Query5_dhcp.BtnNext.Left := BtnNext.Left;
    Query5_dhcp.BtnNext.Top := BtnNext.Top;
  end
  else
  begin
    showForm(Query6, self);
    Query6.BtnBack.Left := BtnBack.Left;
    Query6.BtnBack.Top := BtnBack.Top;
    Query6.BtnOverview.Left := BtnNext.Left;
    Query6.BtnOverview.Top := BtnNext.Top;
  end;

end;

procedure TQuery4.FormActivate(Sender: TObject);
begin
  AdjustPanelPosition(self);
  BackgrImage.Picture.LoadFromFile(QuickInstall.BackgrImageFileName);
  // ask for UCS password only if distribution is Univention
  if QuickInstall.distroName = 'Univention' then
    self.PanelPasswordMasterAdmin.Visible := True
  else
    self.PanelPasswordMasterAdmin.Visible := False;

  if QuickInstall.RadioBtnDefault.Checked then
    PanelReboot.Visible := False
  else
    PanelReboot.Visible := True;

  // text by resourcestrings
  LabelPasswordMasterAdmin.Caption := rsUCS;
  LabelReboot.Caption := rsReboot;
  RadioBtnYes.Caption := rsYes;
  RadioBtnNo.Caption := rsNo;
  LabelDhcp.Caption := rsDhcp;
  RadioBtnDhcpYes.Caption := rsYes;
  RadioBtnDhcpNo.Caption := rsNo;
  LabelFilePointer.Caption := rsTFTPROOT;
  BtnBack.Caption := rsBack;
  BtnNext.Caption := rsNext;
end;

procedure TQuery4.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Query2.Close;
end;

procedure TQuery4.RadioBtnDhcpYesChange(Sender: TObject);
begin
  if RadioBtnDhcpYes.Checked then
    PanelFilePointer.Visible := True
  else
    PanelFilePointer.Visible := False;
end;

procedure TQuery4.BtnBackClick(Sender: TObject);
begin
  if QuickInstall.RadioBtnDefault.Checked then
    showForm(QuickInstall, self)
  else
  begin
    showForm(Query2, self);
  end;
end;

end.
