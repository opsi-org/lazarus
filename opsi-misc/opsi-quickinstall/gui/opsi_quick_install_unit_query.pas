unit opsi_quick_install_unit_query;

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
    EditDefaultRepoNoCache: TEdit;
    EditDefaultRepo: TEdit;
    EditProxy: TEdit;
    EditRepo: TEdit;
    EditOtherNoCache: TEdit;
    InfoRepo: TImage;
    InfoOpsiVersion: TImage;
    LabelOpsiVersion: TLabel;
    LabelNoCache: TLabel;
    LabelProxy: TLabel;
    LabelRepo: TLabel;
    PanelOpsiVersion: TPanel;
    PanelNoCache: TPanel;
    PanelProxy: TPanel;
    PanelRepo: TPanel;
    RadioBtnRepo: TRadioButton;
    RadioBtnNone: TRadioButton;
    RadioBtnOtherRepo: TRadioButton;
    RadioBtnOtherProxy: TRadioButton;
    RadioBtnOtherNoCache: TRadioButton;
    RadioBtnRepoNoCache: TRadioButton;
    RadioBtnMyProxy: TRadioButton;
    RadioBtnOpsi41: TRadioButton;
    RadioBtnOpsi42: TRadioButton;
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
  opsi_quick_install_unit_language, opsi_quick_install_resourcestrings,
  opsi_quick_install_unit_query2, oslog;

{$R *.lfm}

{ TQuery }

procedure TQuery.FormActivate(Sender: TObject);
begin
  // bring all panels to the same position (QuickInstall.panelLeft)
  SetBasics(self);

  // default opsi version is 4.2
  // when RadioBtnOpsi42 (resp. RadioBtnOpsi41) changes (see below)...
  // ...EditDefaultRepo.Text is adjusted
  if self.RadioBtnOpsi41.Checked then
    EditDefaultRepo.Text := QuickInstall.baseURLOpsi41
  else
    EditDefaultRepo.Text := QuickInstall.baseURLOpsi42;
  EditDefaultRepoNoCache.Text := EditDefaultRepo.Text;

  // text by resourcestrings
  Caption := 'Opsi Quick Install - ' + rsCapQuery;
  LabelOpsiVersion.Caption := rsOpsiVersion;
  RadioBtnOpsi41.Caption := rsOpsi41;
  RadioBtnOpsi42.Caption := rsOpsi42;
  InfoOpsiVersion.Hint := rsInfoOpsiVersion;
  LabelRepo.Caption := rsRepo;
  RadioBtnOtherRepo.Caption := rsRepoOther;
  InfoRepo.Hint := rsInfoRepo;
  LabelProxy.Caption := rsProxy;
  RadioBtnNone.Caption := rsProxyNone;
  RadioBtnOtherProxy.Caption := rsProxyOther;
  LabelNoCache.Caption := rsRepoNoCache;
  RadioBtnOtherNoCache.Caption := rsRepoOther;
  BtnBack.Caption := rsBack;
  BtnNext.Caption := rsNext;
end;

procedure TQuery.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  QuickInstall.Close;
end;

procedure TQuery.RadioBtnOpsi41Change(Sender: TObject);
begin
  if RadioBtnOpsi41.Checked then
  begin
    EditDefaultRepo.Text := QuickInstall.baseURLOpsi41;
    EditDefaultRepoNoCache.Text := EditDefaultRepo.Text;
  end
  else
  begin
    EditDefaultRepo.Text := QuickInstall.baseURLOpsi42;
    EditDefaultRepoNoCache.Text := EditDefaultRepo.Text;
  end;
end;

procedure TQuery.BtnNextClick(Sender: TObject);
begin
  showForm(Query2, self);
  Query2.BtnBack.Left := BtnBack.Left;
  Query2.BtnBack.Top := BtnBack.Top;
  Query2.BtnNext.Left := BtnNext.Left;
  Query2.BtnNext.Top := BtnNext.Top;
end;

procedure TQuery.BtnBackClick(Sender: TObject);
begin
  showForm(QuickInstall, self);
end;

end.
