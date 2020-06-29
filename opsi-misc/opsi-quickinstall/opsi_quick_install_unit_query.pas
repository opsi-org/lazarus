unit opsi_quick_install_unit_query;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

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
    LabelWelcome: TLabel;
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
  private

  public

  end;

var
  Query: TQuery;

implementation

uses
  opsi_quick_install_unit_language,
  opsi_quick_install_unit_query2;

{$R *.lfm}

{ TQuery }

procedure TQuery.FormActivate(Sender: TObject);
begin
  Query.Height := QuickInstall.Height;
  Query.Left := QuickInstall.Left;
  Query.Top := QuickInstall.Top;
  Query.Width := QuickInstall.Width;

  BtnBack.Left := QuickInstall.BtnBack.Left;
  BtnBack.Top := QuickInstall.BtnBack.Top;

  BtnNext.Left := QuickInstall.BtnNext.Left;
  BtnNext.Top := QuickInstall.BtnNext.Top;

  //BackgrImage.Picture.LoadFromFile('/home/anja/Bilder/winst2.png');
  BackgrImage.Picture.LoadFromFile('/home/anja/Bilder/opsi.png');
end;

procedure TQuery.BtnNextClick(Sender: TObject);
begin
  Query2.Visible := True;
  Query.Visible := False;
end;

procedure TQuery.BtnBackClick(Sender: TObject);
begin
  QuickInstall.Visible := True;
  Query.Visible := False;
end;

end.

