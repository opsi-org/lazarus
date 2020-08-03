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
    EditProxy: TEdit;
    EditRepo: TEdit;
    EditNoCache: TEdit;
    LabelNoCache: TLabel;
    LabelProxy: TLabel;
    LabelRepo: TLabel;
    PanelNoCache: TPanel;
    PanelProxy: TPanel;
    PanelRepo: TPanel;
    RadioBtnOpsi41: TRadioButton;
    RadioBtnNone: TRadioButton;
    RadioBtnOpsi42: TRadioButton;
    RadioBtnOtherRepo: TRadioButton;
    RadioBtnOtherProxy: TRadioButton;
    RadioBtnOtherNoCache: TRadioButton;
    RadioBtnOpsi41NoCache: TRadioButton;
    RadioBtnMyProxy: TRadioButton;
    RadioBtnOpsi42NoCache: TRadioButton;
    procedure BtnBackClick(Sender: TObject);
    procedure BtnNextClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
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
  AdjustPanelPosition(self);
  // always the same background (as in QuickInstall)
  BackgrImage.Picture.LoadFromFile(QuickInstall.BackgrImageFileName);

  RadioBtnOpsi41.Caption := baseURLOpsi41 + QuickInstall.DistrUrlPart;
  RadioBtnOpsi42.Caption := baseURLOpsi42 + QuickInstall.DistrUrlPart;

  RadioBtnOpsi41NoCache.Caption := RadioBtnOpsi41.Caption;
  RadioBtnOpsi42NoCache.Caption := RadioBtnOpsi42.Caption;
end;

procedure TQuery.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  QuickInstall.Close;
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
