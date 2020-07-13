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
    Label1: TLabel;
    WelcomePanel: TPanel;
    QuickInstallPanel: TPanel;
    RadioBtnDefault: TRadioButton;
    RadioBtnCustom: TRadioButton;
    InfoPanel: TPanel;
    WelcomeLabel2: TLabel;
    LabelWelcome: TLabel;
    LabelSelLanguage: TLabel;
    PanelCoverBtnBack: TPanel;
    procedure BtnNextClick(Sender: TObject);
    procedure ComboBoxLanguagesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
  const
    // same position for all panels
    panelLeft = 200;
    // more constants in FormCreate below
  var
    // same background image for all forms
    BackgrImageFileName: string;
    // for setting selectedAmount:=productAmount only the first time when Query3 is activated
    // Note that it doesn't work to define initialProds in opsi_quick_install_unit_query3 and
    // set it to True here in opsi_quick_install_unit_language.
    initialProds: boolean;
  end;


// for less code
procedure showForm(newForm: TForm; Sender: TForm);
procedure AdjustPanelPosition(Sender: TForm);

var
  QuickInstall: TQuickInstall;

implementation

uses
  opsi_quick_install_unit_query, opsi_quick_install_unit_query6;

{$R *.lfm}

procedure showForm(newForm: TForm; Sender: TForm);
begin
  newForm.Visible := True;

  newForm.Height := Sender.Height;
  newForm.Left := Sender.Left;
  newForm.Top := Sender.Top;
  newForm.Width := Sender.Width;

  Sender.Visible := False;
end;
procedure AdjustPanelPosition(Sender: TForm);
var
  compIndex: integer;
begin
  for compIndex := 0 to Sender.ComponentCount - 1 do
  begin
    if Sender.Components[compIndex].ClassName = 'TPanel' then
    begin
      (Sender.Components[compIndex] as TPanel).Left := QuickInstall.panelLeft;
    end;
  end
end;

{ TQuickInstall }

procedure TQuickInstall.FormCreate(Sender: TObject);
var
  Languages: TStringList;
begin
  // set constant form size
  Height := 450;
  Left := 360;
  Top := 170;
  Width := 730;
  // set constant button position
  BtnBack.Left := 20;
  BtnNext.Left := 660;
  BtnBack.Top := 410;
  BtnNext.Top := 410;
  // set constant background
  BackgrImageFileName := ExtractFilePath(ParamStr(0)) + 'opsi.png';
  BackgrImage.Picture.LoadFromFile(BackgrImageFileName);

  // bring all panels to the same position (QuickInstall.panelLeft)
  AdjustPanelPosition(self);

  ComboBoxLanguages.Left := 120;
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

  initialProds := True;
end;

procedure TQuickInstall.BtnNextClick(Sender: TObject);
begin
  if RadioBtnDefault.Checked then
  begin
    // 'self' is current form
    showForm(Query6, self);
    {Query6.Visible := True;
    Query6.Height := Height;
    Query6.Left := Left;
    Query6.Top := Top;
    Query6.Width := Width;}
    // for having the buttons always at the same place (no hard-coding for easier editing)
    Query6.BtnBack.Left := BtnBack.Left;
    Query6.BtnBack.Top := BtnBack.Top;
    Query6.BtnNext.Left := BtnNext.Left;
    Query6.BtnNext.Top := BtnNext.Top;
  end
  else
  begin
    showForm(Query, self);
    Query.BtnBack.Left := BtnBack.Left;
    Query.BtnBack.Top := BtnBack.Top;
    Query.BtnNext.Left := BtnNext.Left;
    Query.BtnNext.Top := BtnNext.Top;
  end;
  {Visible := False;}
end;

procedure TQuickInstall.ComboBoxLanguagesChange(Sender: TObject);
begin
  if ComboBoxLanguages.Text = 'Deutsch' then
    SetDefaultLang('de')
  else
    SetDefaultLang('en');
end;

end.
