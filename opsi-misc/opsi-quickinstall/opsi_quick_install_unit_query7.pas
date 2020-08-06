unit opsi_quick_install_unit_query7;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TQuery7 }

  TQuery7 = class(TForm)
    BackgrImage: TImage;
    BtnBack: TButton;
    BtnOverview: TButton;
    EditPasswordUCS: TEdit;
    LabelCopyModules: TLabel;
    LabelPasswordMasterAdmin: TLabel;
    PanelRadio: TPanel;
    PanelCopyModules: TPanel;
    PanelPasswordMasterAdmin: TPanel;
    RadioBtnNo: TRadioButton;
    RadioBtnYes: TRadioButton;
    procedure BtnBackClick(Sender: TObject);
    procedure BtnOverviewClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private

  public
    //bigger: integer;
  end;

var
  Query7: TQuery7;

implementation

uses
  opsi_quick_install_unit_language,
  opsi_quick_install_unit_query6,
  opsi_quick_install_unit_overview;

{$R *.lfm}

{ TQuery7 }

procedure TQuery7.BtnOverviewClick(Sender: TObject);
begin
  showForm(Overview, self);
  Overview.BtnBack.Left := BtnBack.Left;
  Overview.BtnBack.Top := BtnBack.Top;
  Overview.BtnFinish.Left := Overview.Width - Overview.BtnBack.Left -
    QuickInstall.BtnFinishWidth;
  Overview.BtnFinish.Top := BtnOverview.Top;
end;

procedure TQuery7.FormActivate(Sender: TObject);
begin
  AdjustPanelPosition(self);
  BackgrImage.Picture.LoadFromFile(QuickInstall.BackgrImageFileName);
end;

procedure TQuery7.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Query6.Close;
end;

procedure TQuery7.BtnBackClick(Sender: TObject);
begin
  showForm(Query6, self);
end;

end.
