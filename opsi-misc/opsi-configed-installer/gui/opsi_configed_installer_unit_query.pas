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
    LabelOpsiVersion: TLabel;
    LabelScalingFactor: TLabel;
    LabelMem: TLabel;
    PanelOpsiVersion: TPanel;
    PanelScalingFactor: TPanel;
    PanelMem: TPanel;
    RadioBtnMem512: TRadioButton;
    RadioBtn05: TRadioButton;
    RadioBtnMem1024: TRadioButton;
    RadioBtn15: TRadioButton;
    RadioBtn10: TRadioButton;
    RadioBtnOpsi41: TRadioButton;
    RadioBtnOpsi42: TRadioButton;
    RadioBtnMem1536: TRadioButton;
    RadioBtnMem2048: TRadioButton;
    RadioBtnMem3000: TRadioButton;
    RadioBtn20: TRadioButton;
    procedure BtnBackClick(Sender: TObject);
    procedure BtnNextClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure RadioBtnOpsi41Change(Sender: TObject);
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
  // Opsi version
  (*if RadioBtnOpsi41.Checked then
    Data.opsiVersion := RadioBtnOpsi41.Caption
  else
    Data.opsiVersion := RadioBtnOpsi42.Caption;
  // Repository
  if RadioBtnMem512.Checked then
    Data.repo := EditDefaultRepo.Text
  else
    Data.repo := EditRepo.Text;
  // Proxy
  if RadioBtn05.Checked then
    Data.proxy.SetEntries(RadioBtn05.Caption, '')
  else if RadioBtn10.Checked then
    Data.proxy.SetEntries(RadioBtn10.Caption, RadioBtn10.Caption)
  else
    Data.proxy.SetEntries(EditProxy.Text, EditProxy.Text);
  // Repository (no cache)
  if RadioBtnRepoNoCache.Checked then
    Data.repoNoCache := EditDefaultRepoNoCache.Text
  else
    Data.repoNoCache := EditOtherNoCache.Text;

  // show next form in custom setup
  showForm(Query2, self);
  Query2.BtnBack.Left := BtnBack.Left;
  Query2.BtnBack.Top := BtnBack.Top;
  Query2.BtnNext.Left := BtnNext.Left;
  Query2.BtnNext.Top := BtnNext.Top;*)

  Query.Enabled := False;
  Password.Visible := True;
end;

procedure TQuery.FormActivate(Sender: TObject);
begin
  // bring all panels to the same position (QuickInstall.panelLeft)
  SetBasics(self);

  // default opsi version is 4.2
  // set default repo depending on default opsi version
  (*if self.RadioBtnOpsi41.Checked then
    EditDefaultRepo.Text := QuickInstall.baseURLOpsi41
  else
    EditDefaultRepo.Text := QuickInstall.baseURLOpsi42;
  // same repo for no cache proxy
  EditDefaultRepoNoCache.Text := EditDefaultRepo.Text;

  // text by resourcestrings
  Caption := 'Opsi Quick Install - ' + rsCapQuery;
  LabelOpsiVersion.Caption := rsOpsiVersion;
  RadioBtnOpsi41.Caption := rsOpsi41;
  RadioBtnOpsi42.Caption := rsOpsi42;
  InfoOpsiVersion.Hint := rsInfoOpsiVersion;
  LabelMem.Caption := rsRepo;
  RadioBtnMem1024.Caption := rsRepoOther;
  InfoRepo.Hint := rsInfoRepo;
  LabelScalingFactor.Caption := rsProxy;
  RadioBtn05.Caption := rsProxyNone;
  RadioBtn15.Caption := rsProxyOther;
  LabelNoCache.Caption := rsRepoNoCache;
  RadioBtnOtherNoCache.Caption := rsRepoOther;*)
  BtnBack.Caption := rsBack;
  BtnNext.Caption := rsNext;
end;

procedure TQuery.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  ConfigedInstaller.Close;
end;

procedure TQuery.RadioBtnOpsi41Change(Sender: TObject);
begin
  // when opsi version changes, adjust default repos
  (*if RadioBtnOpsi41.Checked then
  begin
    EditDefaultRepo.Text := QuickInstall.baseURLOpsi41;
    EditDefaultRepoNoCache.Text := EditDefaultRepo.Text;
  end
  else
  begin
    EditDefaultRepo.Text := QuickInstall.baseURLOpsi42;
    EditDefaultRepoNoCache.Text := EditDefaultRepo.Text;
  end;*)
end;

procedure TQuery.BtnBackClick(Sender: TObject);
begin
  showForm(ConfigedInstaller, self);
end;

end.
