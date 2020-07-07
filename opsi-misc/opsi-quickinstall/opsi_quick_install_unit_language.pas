unit opsi_quick_install_unit_language;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, LCLtranslator;

type

  { TQuickInstall }

  TQuickInstall = class(TForm)
    BackgrImage: TImage;
    BtnBack: TButton;
    BtnNext: TButton;
    ComboBoxLanguages: TComboBox;
    WelcomePanel: TPanel;
    WelcomeLabel2: TLabel;
    LabelWelcome: TLabel;
    LabelSelLanguage: TLabel;
    PanelCoverBtnBack: TPanel;
    procedure BtnNextClick(Sender: TObject);
    procedure ComboBoxLanguagesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
    // same position for all panels
    panelLeft: integer;
    // same background image for all forms
    BackgrImageFileName: string;
  end;

var
  QuickInstall: TQuickInstall;

implementation

uses
  opsi_quick_install_unit_query;

{$R *.lfm}

{ TQuickInstall }

procedure TQuickInstall.FormCreate(Sender: TObject);
var
  Languages: TStringList;
begin
  //ShowMessage(GetDefaultLang);
  //SetDefaultLang('en');

  // set form size
  Height := 450;
  Left := 360;
  Top := 170;
  Width := 730;

  BtnBack.Left := 20;
  BtnNext.Left := 660;
  BtnBack.Top := 410;
  BtnNext.Top := 410;

  panelLeft := 175;

  BackgrImageFileName := ExtractFilePath(ParamStr(0)) + 'opsi.png';
  BackgrImage.Picture.LoadFromFile(BackgrImageFileName);

  with ComboBoxLanguages.Items do
  begin
    Add('Deutsch');
    Add('English');
  end;
  Languages := TStringList.Create;
  Languages.Add('de');
  Languages.Add('en');
  // let the combo box show the system language
  ComboBoxLanguages.ItemIndex := Languages.IndexOf(GetDefaultLang);
end;

procedure TQuickInstall.BtnNextClick(Sender: TObject);
begin
  Query.Visible := True;

  Query.Height := Height;
  Query.Left := Left;
  Query.Top := Top;
  Query.Width := Width;

  Query.BtnBack.Left := BtnBack.Left;
  Query.BtnBack.Top := BtnBack.Top;
  Query.BtnNext.Left := BtnNext.Left;
  Query.BtnNext.Top := BtnNext.Top;

  Visible := False;
end;

procedure TQuickInstall.ComboBoxLanguagesChange(Sender: TObject);
begin
  if ComboBoxLanguages.Text = 'Deutsch' then
    SetDefaultLang('de')
  else
    SetDefaultLang('en');
end;

end.

