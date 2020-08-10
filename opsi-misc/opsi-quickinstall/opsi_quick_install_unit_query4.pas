unit opsi_quick_install_unit_query4;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TQuery4 }

  TQuery4 = class(TForm)
    BackgrImage: TImage;
    BigPanel: TPanel;
    BtnBack: TButton;
    BtnNext: TButton;
    LabelDhcp: TLabel;
    LabelFilePointer: TLabel;
    LabelOpsiVersion: TLabel;
    LabelTimeout: TLabel;
    PanelOpsiVersion: TPanel;
    PanelRadiofilePointer: TPanel;
    PanelRadioTimeout: TPanel;
    PanelRadioDhcp: TPanel;
    PanelDhcp: TPanel;
    PanelFilePointer: TPanel;
    PanelTimeout: TPanel;
    RadioBtnDhcpNo: TRadioButton;
    RadioBtnDhcpYes: TRadioButton;
    RadioBtnNo: TRadioButton;
    RadioBtnYes: TRadioButton;
    RadioBtnMenu: TRadioButton;
    RadioBtnNoMenu: TRadioButton;
    RadioBtnOpsi41: TRadioButton;
    RadioBtnOpsi42: TRadioButton;
    procedure BtnBackClick(Sender: TObject);
    procedure BtnNextClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure RadioBtnDhcpYesChange(Sender: TObject);
    procedure RadioBtnOpsi41Change(Sender: TObject);
  private

  public

  end;

var
  Query4: TQuery4;

implementation

uses
  opsi_quick_install_unit_language,
  opsi_quick_install_unit_query,
  opsi_quick_install_unit_query3,
  opsi_quick_install_unit_query5_dhcp,
  opsi_quick_install_unit_query6;

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
    Query6.BtnNext.Left := BtnNext.Left;
    Query6.BtnNext.Top := BtnNext.Top;
  end;

end;

procedure TQuery4.FormActivate(Sender: TObject);
begin
  BigPanel.Left := QuickInstall.panelLeft;
  BackgrImage.Picture.LoadFromFile(QuickInstall.BackgrImageFileName);
  // in default mode opsi version isn't requested in Query
  if QuickInstall.RadioBtnDefault.Checked then
    PanelOpsiVersion.Visible := True
  else
  begin
    PanelOpsiVersion.Visible := False;
  end;
end;

procedure TQuery4.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Query3.Close;
end;

procedure TQuery4.RadioBtnDhcpYesChange(Sender: TObject);
begin
  if RadioBtnDhcpYes.Checked then
  begin
    PanelTimeout.Visible := True;
    PanelFilePointer.Visible := True;
  end
  else
  begin
    PanelTimeout.Visible := False;
    PanelFilePointer.Visible := False;
  end;
end;

procedure TQuery4.RadioBtnOpsi41Change(Sender: TObject);
begin
  // set the opsi version in Query respectively to only have check them for
  // Overview and the .conf file
  if RadioBtnOpsi41.Checked then
    Query.RadioBtnOpsi41.Checked := True
  else
    Query.RadioBtnOpsi41.Checked := False;
end;

procedure TQuery4.BtnBackClick(Sender: TObject);
begin
  if QuickInstall.RadioBtnDefault.Checked then
    showForm(QuickInstall, self)
  else
  begin
    showForm(Query3, self);
  end;
end;

end.
