unit opsi_quick_install_unit_query_prods;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TQueryProds }

  TQueryProds = class(TForm)
    BackgrImage: TImage;
    BtnBack: TButton;
    BtnNext: TButton;
    CheckBoxOpsiSetWinUac: TCheckBox;
    CheckBoxOpsiUefiNetboot: TCheckBox;
    CheckBoxOpsiWinpe: TCheckBox;
    CheckBoxOpsiSetupDetector: TCheckBox;
    CheckBoxOpsiLogviewer: TCheckBox;
    CheckBoxConfigWin10: TCheckBox;
    CheckBoxConfigWinbase: TCheckBox;
    CheckBoxAll: TCheckBox;
    CheckBoxNone: TCheckBox;
    CheckBoxOpsiClientAgent: TCheckBox;
    CheckBoxShutdown: TCheckBox;
    CheckBoxOpsiScriptTest: TCheckBox;
    CheckBoxOpsiWimCapture: TCheckBox;
    CheckBoxOpsiWinst: TCheckBox;
    CheckBoxJavavm: TCheckBox;
    CheckBoxOpsiConfiged: TCheckBox;
    CheckBoxJedit: TCheckBox;
    CheckBoxSwaudit: TCheckBox;
    CheckBoxHwaudit: TCheckBox;
    CheckBoxOpsiTemplate: TCheckBox;
    CheckBoxOpsiTemplateAdmin: TCheckBox;
    LabelOpsiProducts: TLabel;
    PanelRadioProds: TPanel;
    PanelProdToChoose: TPanel;
    PanelOpsiProducts: TPanel;
    procedure BtnBackClick(Sender: TObject);
    procedure BtnNextClick(Sender: TObject);
    procedure CheckBoxAllChange(Sender: TObject);
    procedure CheckBoxNoneChange(Sender: TObject);
    procedure CheckBoxProdChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private

  public
    productAmount, selectedAmount: integer;
  end;

var
  QueryProds: TQueryProds;

implementation

uses
  opsi_quick_install_unit_language,
  opsi_quick_install_unit_overview, opsi_quick_install_resourcestrings;

{$R *.lfm}

{ TQueryProds }

procedure TQueryProds.BtnNextClick(Sender: TObject);
begin
  Overview.Close;
end;

procedure TQueryProds.CheckBoxAllChange(Sender: TObject);
var
  compIndex: integer;
begin
  if CheckBoxAll.Checked and (selectedAmount <> productAmount) then
  begin
    for compIndex := 0 to PanelProdToChoose.ControlCount - 1 do
    begin
      if (PanelProdToChoose.Controls[compIndex].ClassName = 'TCheckBox') then
        (PanelProdToChoose.Controls[compIndex] as TCheckBox).Checked := True;
    end;
  end
  else
  begin
    if (CheckBoxAll.Checked = False) and (selectedAmount = productAmount) then
      CheckBoxNone.Checked := True;
  end;
end;

procedure TQueryProds.CheckBoxNoneChange(Sender: TObject);
var
  compIndex: integer;
begin
  if CheckBoxNone.Checked and (selectedAmount <> 0) then
  begin
    for compIndex := 0 to PanelProdToChoose.ControlCount - 1 do
    begin
      if (PanelProdToChoose.Controls[compIndex].ClassName = 'TCheckBox') then
        (PanelProdToChoose.Controls[compIndex] as TCheckBox).Checked := False;
    end;
  end
  else
  begin
    if (CheckBoxNone.Checked = False) and (selectedAmount = 0) then
      CheckBoxAll.Checked := True;
  end;
end;

procedure TQueryProds.CheckBoxProdChange(Sender: TObject);
begin
  if (Sender as TCheckBox).Checked then
  begin
    selectedAmount := selectedAmount + 1;
    if selectedAmount = productAmount then
      CheckBoxAll.Checked := True;
    CheckBoxNone.Checked := False;
  end
  else
  begin
    selectedAmount := selectedAmount - 1;
    if selectedAmount = 0 then
      CheckBoxNone.Checked := True;
    CheckBoxAll.Checked := False;
  end;
end;

procedure TQueryProds.FormActivate(Sender: TObject);
begin
  AdjustPanelPosition(self);
  BackgrImage.Picture.LoadFromFile(QuickInstall.BackgrImageFileName);

  if QuickInstall.initialProds then
  begin
    // amount of products to choose equals PanelProdToChoose.ComponentCount
    productAmount := PanelProdToChoose.ControlCount;
    // at the beginning, all prods are selected
    selectedAmount := productAmount;
    QuickInstall.initialProds := False;
  end;
end;

procedure TQueryProds.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Overview.Close;
end;

procedure TQueryProds.BtnBackClick(Sender: TObject);
begin
end;

end.
