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
var
  compIndex: integer;
begin
  for compIndex := 0 to ComponentCount - 1 do
  begin
    if Components[compIndex].ClassName = 'TPanel' then
    begin
      (Components[compIndex] as TPanel).Left := QuickInstall.panelLeft;
    end;
  end;

  BackgrImage.Picture.LoadFromFile(QuickInstall.BackgrImageFileName);
end;

procedure TQuery.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  QuickInstall.Close;
end;

procedure TQuery.BtnNextClick(Sender: TObject);
begin
  Query2.Visible := True;

  Query2.Height := Height;
  Query2.Left := Left;
  Query2.Top := Top;
  Query2.Width := Width;

  Query2.BtnBack.Left := BtnBack.Left;
  Query2.BtnBack.Top := BtnBack.Top;
  Query2.BtnNext.Left := BtnNext.Left;
  Query2.BtnNext.Top := BtnNext.Top;

  Visible := False;
end;

procedure TQuery.BtnBackClick(Sender: TObject);
begin
  QuickInstall.Visible := True;

  QuickInstall.Height := Height;
  QuickInstall.Left := Left;
  QuickInstall.Top := Top;
  QuickInstall.Width := Width;

  QuickInstall.BtnBack.Left := BtnBack.Left;
  QuickInstall.BtnBack.Top := BtnBack.Top;
  QuickInstall.BtnNext.Left := BtnNext.Left;
  QuickInstall.BtnNext.Top := BtnNext.Top;

  Visible := False;
end;

end.

