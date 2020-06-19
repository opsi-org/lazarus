unit opsi_quick_install_unit_language;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TQuickInstall }

  TQuickInstall = class(TForm)
    BackgrImage: TImage;
    BtnNext: TButton;
    ComboBoxLanguages: TComboBox;
    LabelWelcome: TLabel;
    LabelSelLanguage: TLabel;
    procedure BtnNextClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

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
  Query.ShowModal;
  QuickInstall.Close;
end;

procedure TQuickInstall.Button1Click(Sender: TObject);
begin

end;

end.

