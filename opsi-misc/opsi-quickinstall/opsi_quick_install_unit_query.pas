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
    procedure BtnNextClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private

  public

  end;

var
  Query: TQuery;

implementation

uses
  opsi_quick_install_unit_query2;

{$R *.lfm}

{ TQuery }

procedure TQuery.FormActivate(Sender: TObject);
begin
  //BackgrImage.Picture.LoadFromFile('/home/anja/Bilder/winst2.png');
  BackgrImage.Picture.LoadFromFile('/home/anja/Bilder/opsi.png');
end;

procedure TQuery.BtnNextClick(Sender: TObject);
begin
  Query2.ShowModal;
  Query.Close;
end;

end.

