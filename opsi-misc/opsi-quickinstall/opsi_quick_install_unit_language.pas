unit opsi_quick_install_unit_language;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TQuickInstall }

  TQuickInstall = class(TForm)
    BackgrImage: TImage;
    BtnBack: TButton;
    BtnNext: TButton;
    ComboBoxLanguages: TComboBox;
    LabelWelcome: TLabel;
    LabelSelLanguage: TLabel;
    Panel1: TPanel;
    procedure BtnNextClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
    formHeight, formLeft, formTop, formWidth, btnBackLeft, btnNextLeft, btnTop: integer;
  end;

var
  QuickInstall: TQuickInstall;

implementation

uses
  opsi_quick_install_unit_query;

{$R *.lfm}

{ TQuickInstall }

procedure TQuickInstall.FormCreate(Sender: TObject);
begin
  formHeight := 450;
  formLeft := 360;
  formTop := 170;
  formWidth := 730;
  btnBackLeft := 20;
  btnNextLeft := 660;
  btnTop := 410;

  QuickInstall.Height := formHeight;
  QuickInstall.Left := formLeft;
  QuickInstall.Top := formTop;
  QuickInstall.Width := formWidth;

  //BackgrImage.Picture.LoadFromFile('/home/anja/Bilder/winst2.png');
  BackgrImage.Picture.LoadFromFile('/home/anja/Bilder/opsi.png');

  with ComboBoxLanguages.Items do
  begin
    Add('Deutsch');
    Add('English');
  end;
end;

procedure TQuickInstall.BtnNextClick(Sender: TObject);
begin
  Query.Visible := True;
  QuickInstall.Visible := False;
end;

procedure TQuickInstall.Button1Click(Sender: TObject);
begin

end;

end.

