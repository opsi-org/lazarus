unit opsi_configed_installer_unit_query;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TQuery }

  TQuery = class(TForm)
    BackgrImage: TImage;
    BtnNext: TButton;
    BtnBack: TButton;
    InfoRepo: TImage;
    InfoOpsiVersion: TImage;
    LabelScalingFactor: TLabel;
    LabelMem: TLabel;
    PanelScalingFactor: TPanel;
    PanelMem: TPanel;
    RadioBtnMem512: TRadioButton;
    RadioBtn05: TRadioButton;
    RadioBtnMem1024: TRadioButton;
    RadioBtn15: TRadioButton;
    RadioBtn10: TRadioButton;
    RadioBtnMem1536: TRadioButton;
    RadioBtnMem2048: TRadioButton;
    RadioBtnMem3000: TRadioButton;
    RadioBtn20: TRadioButton;
    procedure BtnBackClick(Sender: TObject);
    procedure BtnNextClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
  public
  end;

var
  Query: TQuery;

implementation

uses
  opsi_configed_installer_resourcestrings,
  opsi_configed_installer_data,
  opsi_configed_installer_unit_language,
  opsi_configed_installer_unit_password,
  oslog;

{$R *.lfm}

{ TQuery }

procedure TQuery.BtnNextClick(Sender: TObject);
begin
  // Make Data entries:
  if RadioBtnMem512.Checked then
    Data.MemoryRequirement := RadioBtnMem512.Caption
  else if RadioBtnMem1024.Checked then
    Data.MemoryRequirement := RadioBtnMem1024.Caption
  else if RadioBtnMem1536.Checked then
    Data.MemoryRequirement := RadioBtnMem1536.Caption
  else if RadioBtnMem2048.Checked then
    Data.MemoryRequirement := RadioBtnMem2048.Caption
  else if RadioBtnMem3000.Checked then
    Data.MemoryRequirement := RadioBtnMem3000.Caption;

  if RadioBtn05.Checked then
    Data.ScalingFactor := RadioBtn05.Caption
  else if RadioBtn10.Checked then
    Data.ScalingFactor := RadioBtn10.Caption
  else if RadioBtn15.Checked then
    Data.ScalingFactor := RadioBtn15.Caption
  else if RadioBtn20.Checked then
    Data.ScalingFactor := RadioBtn20.Caption;

  Query.Enabled := False;
  Password.Visible := True;
end;

procedure TQuery.FormActivate(Sender: TObject);
begin
  // bring all panels to the same position (QuickInstall.panelLeft)
  SetBasics(self);

  // text by resourcestrings
  BtnBack.Caption := rsBack;
  BtnNext.Caption := rsNext;
end;

procedure TQuery.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  ConfigedInstaller.Close;
end;


procedure TQuery.BtnBackClick(Sender: TObject);
begin
  showForm(ConfigedInstaller, self);
end;

end.
