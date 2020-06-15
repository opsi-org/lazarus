unit opsi_quick_install_unit_language;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TQuickInstall }

  TQuickInstall = class(TForm)
    BackgrImage: TImage;
    ComboBoxLanguages: TComboBox;
    LabelSelLanguage: TLabel;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  QuickInstall: TQuickInstall;

implementation

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

end.

