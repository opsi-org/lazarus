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
    LabelOpsiVersion: TLabel;
    LabelNoCache: TLabel;
    LabelProxy: TLabel;
    LabelRepo: TLabel;
    BigPanel: TPanel;
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
  const
    baseURLOpsi41 = 'http://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.1:/';
    baseURLOpsi42 = 'http://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.2:/';
  public
  end;

var
  Query: TQuery;

implementation

uses
  opsi_quick_install_unit_language,
  opsi_quick_install_unit_query2, oslog;

{$R *.lfm}

{ TQuery }

procedure TQuery.FormActivate(Sender: TObject);
begin
  // bring all panels to the same position (QuickInstall.panelLeft)
  BigPanel.Left := QuickInstall.panelLeft;
  {PanelEditOtherRepo.Left:=PanelEditOtherRepo.Left-QuickInstall.panelLeft;
  PanelEditOtherProxy.Left:=PanelEditOtherProxy.Left-QuickInstall.panelLeft;
  PanelEditOtherNoCache.Left:=PanelEditOtherNoCache.Left-QuickInstall.panelLeft;}
  // always the same background (as in QuickInstall)
  BackgrImage.Picture.LoadFromFile(QuickInstall.BackgrImageFileName);

  EditDefaultRepo.Text := baseURLOpsi42 + QuickInstall.DistrUrlPart;
  EditDefaultRepoNoCache.Text := EditDefaultRepo.Text;
end;

procedure TQuery.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  QuickInstall.Close;
end;

procedure TQuery.RadioBtnOpsi41Change(Sender: TObject);
begin
  if RadioBtnOpsi41.Checked then
  begin
    EditDefaultRepo.Text := baseURLOpsi41 + QuickInstall.DistrUrlPart;
    EditDefaultRepoNoCache.Text := EditDefaultRepo.Text;
  end
  else
  begin
    EditDefaultRepo.Text := baseURLOpsi42 + QuickInstall.DistrUrlPart;
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
