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
    LabelCopyModules: TLabel;
    LabelRepoKind: TLabel;
    LabelBackend: TLabel;
    PanelCopyModules: TPanel;
    PanelRadio: TPanel;
    PanelRadioUpdate: TPanel;
    PanelRadioRepoKind: TPanel;
    PanelRadioBackend: TPanel;
    PanelUpdate: TPanel;
    PanelRepoKind: TPanel;
    PanelBackend: TPanel;
    RadioBtnFile: TRadioButton;
    RadioBtnMySql: TRadioButton;
    RadioBtnExperimental: TRadioButton;
    RadioBtnNoCopy: TRadioButton;
    RadioBtnStable: TRadioButton;
    RadioBtnTesting: TRadioButton;
    RadioBtnYes: TRadioButton;
    RadioBtnNo: TRadioButton;
    RadioBtnYesCopy: TRadioButton;
    procedure BtnBackClick(Sender: TObject);
    procedure BtnNextClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private

  public

  end;

var
  Query2: TQuery2;

implementation

uses
  opsi_quick_install_unit_language,
  opsi_quick_install_unit_query,
  opsi_quick_install_unit_query4;

{$R *.lfm}

{ TQuery2 }

procedure TQuery2.BtnNextClick(Sender: TObject);
begin
  showForm(Query4, self);
  Query4.BtnBack.Left := BtnBack.Left;
  Query4.BtnBack.Top := BtnBack.Top;
  Query4.BtnNext.Left := BtnNext.Left;
  Query4.BtnNext.Top := BtnNext.Top;
end;

procedure TQuery2.FormActivate(Sender: TObject);
begin
  AdjustPanelPosition(self);
  BackgrImage.Picture.LoadFromFile(QuickInstall.BackgrImageFileName);
  if QuickInstall.RadioBtnDefault.Checked then
    PanelCopyModules.Visible := False
  else
    PanelCopyModules.Visible := True;
end;

procedure TQuery2.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Query.Close;
end;

procedure TQuery2.BtnBackClick(Sender: TObject);
begin
  showForm(Query, self);
end;

end.

