unit opsi_quick_install_unit_language;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, LCLtranslator, osDistributionInfo;

type

  { TQuickInstall }

  TQuickInstall = class(TForm)
    BackgrImage: TImage;
    BtnBack: TButton;
    BtnNext: TButton;
    ComboBoxLanguages: TComboBox;
    LabelSetup: TLabel;
    WelcomePanel: TPanel;
    QuickInstallPanel: TPanel;
    RadioBtnDefault: TRadioButton;
    RadioBtnCustom: TRadioButton;
    LabelCarryOut: TLabel;
    LabelWelcome: TLabel;
    LabelSelLanguage: TLabel;
    procedure BtnNextClick(Sender: TObject);
    procedure ComboBoxLanguagesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure SetBtnWidth(Language: string);
  public
  const
    // same width for all panels
    panelWidth = 450;
    // base urls for opsi 4.1 and 4.2
    baseURLOpsi41 =
      'http://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.1:/stable/';
    baseURLOpsi42 =
      'http://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.2:/stable/';
    // more constants in FormCreate below
  var
    // same position for all panels
    panelLeft: integer;
    // same position for all buttons
    BtnNextWidth, BtnOverviewWidth, BtnFinishWidth: integer;
    // same background image for all forms
    BackgrImageFileName: string;
    // for setting selectedAmount:=productAmount only the first time when Query3 is activated
    // Note that it doesn't work to define initialProds in opsi_quick_install_unit_query3 and
    // set it to True here in opsi_quick_install_unit_language.
    initialProds: boolean;
    distroName, distroRelease: string;

    DistrInfo: TDistributionInfo;
  end;


// for less code
procedure showForm(newForm: TForm; Sender: TForm);
procedure AdjustPanelPosition(Sender: TForm);

var
  QuickInstall: TQuickInstall;

implementation

uses
  opsi_quick_install_unit_query, opsi_quick_install_unit_query4,
  oslog, osfunclin, opsi_quick_install_unit_distr, opsi_quick_install_resourcestrings;

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
    if (Sender.Components[compIndex].ClassName = 'TPanel') then
    begin
      (Sender.Components[compIndex] as TPanel).Left := QuickInstall.panelLeft;
      (Sender.Components[compIndex] as TPanel).Width := QuickInstall.panelWidth;
    end;
  end;
end;


{ TQuickInstall }

procedure TQuickInstall.SetBtnWidth(Language: string);
begin
  if Language = 'de' then
  begin
    // needs to be set for every language
    BtnNextWidth := 63;
    BtnOverviewWidth := 72;
    BtnFinishWidth := 88;
    //BtnNext.Width = 'with for english caption'
    //BtnNext.Left := Width - BtnNext.Width - BtnBack.Left; doesn't help
    BtnNext.Left := Width - BtnBack.Left - BtnNextWidth;
  end
  else
  begin
    BtnNextWidth := 51;
    BtnOverviewWidth := 69;
    BtnFinishWidth := 45;
    BtnNext.Left := Width - BtnBack.Left - BtnNextWidth;
  end;
end;

procedure TQuickInstall.FormCreate(Sender: TObject);
var
  Languages: TStringList;
begin
  // set constant form size
  Height := 450;
  Left := 360;
  Top := 170;
  Width := 730;
  panelLeft := Round((Width - panelWidth) * 2 / 3);
  // set constant button positions:
  BtnBack.Left := 20;
  //BtnNext.Width = 'with for english caption'
  BtnNext.Left := Width - BtnBack.Left - BtnNext.Width;
  BtnBack.Top := 410;
  BtnNext.Top := 410;
  // set constant background
  BackgrImageFileName := ExtractFilePath(ParamStr(0)) + 'opsi.png';
  BackgrImage.Picture.LoadFromFile(BackgrImageFileName);
  // bring all panels to the same position (QuickInstall.panelLeft)
  AdjustPanelPosition(self);

  // text by resourcestrings
  LabelWelcome.Caption := rsWelcome;
  LabelSelLanguage.Caption := rsSelLanguage;
  LabelSetup.Caption := rsSetup;
  RadioBtnDefault.Caption := rsStandard;
  RadioBtnCustom.Caption := rsCustom;
  LabelCarryOut.Caption := rsCarryOut;
  BtnNext.Caption := rsNext;

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
  // set width for overview and finish buttons and the next button on this...
  // ...form depending on the language
  SetBtnWidth(GetDefaultLang);

  initialProds := True;

  // .../lazarus/common/oslog.pas
  // log file in /tmp/opsi_quickinstall.log
  LogDatei := TLogInfo.Create;
  LogDatei.CreateTheLogfile('opsi_quickinstall.log');

  // (compare function GetDefaultURL in osLinuxRepository:)
  // following two lines take time and are therefore executed only...
  // ...once at the beginning of this program
  distroName := getLinuxDistroName;
  distroRelease := getLinuxDistroRelease;
  //ShowMessage(distroName);
  //ShowMessage(distroRelease);
  DistrInfo := TDistributionInfo.Create;
end;

procedure TQuickInstall.BtnNextClick(Sender: TObject);
begin
  Distribution.ShowModal;
  if Distribution.GoOn then
  begin
    if RadioBtnDefault.Checked then
    begin
      // 'self' is current form
      showForm(Query4, self);
      // for having the buttons always at the same place (no hard-coding for easier editing)
      Query4.BtnBack.Left := BtnBack.Left;
      Query4.BtnBack.Top := BtnBack.Top;
      Query4.BtnNext.Left := BtnNext.Left;
      Query4.BtnNext.Top := BtnNext.Top;
    end
    else
    begin
      showForm(Query, self);
      Query.BtnBack.Left := BtnBack.Left;
      Query.BtnBack.Top := BtnBack.Top;
      Query.BtnNext.Left := BtnNext.Left;
      Query.BtnNext.Top := BtnNext.Top;
    end;
  end;
end;

procedure TQuickInstall.ComboBoxLanguagesChange(Sender: TObject);
begin
  if ComboBoxLanguages.Text = 'Deutsch' then
  begin
    SetDefaultLang('de');
    SetBtnWidth('de');
  end
  else
  begin
    SetDefaultLang('en');
    SetBtnWidth('en');
  end;
end;

end.
