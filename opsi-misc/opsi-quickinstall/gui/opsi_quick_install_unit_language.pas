unit opsi_quick_install_unit_language;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, LCLtranslator, Buttons, process;

type

  { TQuickInstall }

  TQuickInstall = class(TForm)
    BackgrImage: TImage;
    BtnBack: TButton;
    BtnFinish: TButton;
    BtnNext: TButton;
    BtnOverview: TButton;
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
  public
  const
    // same width for all panels
    panelWidth = 460;
    // same size for all info images
    infoSize = 22;
    // same background image for all forms
    BackgrImageFileName = 'opsi.png';
    // same image for all infos
    InfoImageFileName = 'info_tiny.png';
    // base urls for opsi 4.1 and 4.2
    baseURLOpsi41 =
      'http://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.1:/';
    baseURLOpsi42 =
      'http://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.2:/';
    // more constants in FormCreate below
  var
    // same position for all panels
    panelLeft: integer;
    // same position for all buttons
    BtnNextWidth, BtnOverviewWidth, BtnFinishWidth: integer;
    // for setting selectedAmount:=productAmount only the first time when Query3 is activated
    // Note that it doesn't work to define initialProds in opsi_quick_install_unit_query3 and
    // set it to True here in opsi_quick_install_unit_language.
    initialProds: boolean;

    logFileName: string;

    procedure SetBtnWidth(Language: string);
    procedure ShowHintOnClick(Sender: TObject);
  end;


// for less code
procedure showForm(newForm: TForm; Sender: TForm);
// set InfoImage.Picture, InfoImage.Width, InfoImage.Height
procedure setInfoBasics(InfoImage: TImage);
// set Panel.Left, Panel.Width
procedure SetBasics(Sender: TForm);

var
  QuickInstall: TQuickInstall;

implementation

uses
  opsi_quick_install_resourcestrings,
  opsi_quick_install_data,
  opsi_quick_install_unit_distr,
  opsi_quick_install_unit_query,
  opsi_quick_install_unit_query4,
  oslog, osfunclin;

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

procedure setInfoBasics(InfoImage: TImage);
begin
  InfoImage.Width := QuickInstall.infoSize;
  InfoImage.Height := QuickInstall.infoSize;
  // set info image
  InfoImage.Picture.LoadFromFile(
    ExtractFilePath(ParamStr(0)) + QuickInstall.InfoImageFileName);
  InfoImage.BorderSpacing.Left := 5;
  // Show info hints also on click of image
  InfoImage.OnClick := @QuickInstall.ShowHintOnClick;
end;

procedure SetBasics(Sender: TForm);
var
  compIndex: integer;
begin
  for compIndex := 0 to Sender.ComponentCount - 1 do
  begin
    if (Sender.Components[compIndex].ClassName = 'TPanel') then
    begin
      (Sender.Components[compIndex] as TPanel).Left := QuickInstall.panelLeft;
      (Sender.Components[compIndex] as TPanel).Width := QuickInstall.panelWidth;
      (Sender.Components[compIndex] as TPanel).Color := clForm;
    end
    else
    if (Sender.Components[compIndex].ClassName = 'TImage') and
      (Pos('Info', Sender.Components[compIndex].Name) = 1) then
      setInfoBasics(Sender.Components[compIndex] as TImage)
    else if (Sender.Components[compIndex].Name = 'BackgrImage') then
      // set background image
      (Sender.Components[compIndex] as TImage).Picture.LoadFromFile(
        ExtractFilePath(ParamStr(0)) + QuickInstall.BackgrImageFileName);
  end;
end;

{ TQuickInstall }

procedure TQuickInstall.SetBtnWidth(Language: string);
begin
  if Language = 'de' then
  begin
    // needs to be set for every language for nice placement of BtnNext
    BtnNextWidth := 63;
    //BtnNext.Width = 'with for english caption'
    //BtnNext.Left := Width - BtnNext.Width - BtnBack.Left; doesn't help
    BtnNext.Left := Width - BtnBack.Left - BtnNextWidth;
  end
  else if Language = 'en' then
  begin
    BtnNextWidth := 51;
    BtnNext.Left := Width - BtnBack.Left - BtnNextWidth;
  end;
end;

procedure TQuickInstall.ShowHintOnClick(Sender: TObject);
begin
  Application.ActivateHint(TWinControl(Sender).ClientToScreen(Point(1, 1)), True);
end;

procedure TQuickInstall.FormCreate(Sender: TObject);
var
  Languages: TStringList;
  removeFuzzys: string;
begin
  // from all po files remove all fuzzys that might have been introduced by the nogui version
  if RunCommand('/bin/sh', ['-c', 'echo | msgattrib --clear-fuzzy -o ../gui/locale/opsi_quick_install_project.de.po ../gui/locale/opsi_quick_install_project.de.po'], removeFuzzys) then;
  if RunCommand('/bin/sh', ['-c', 'echo | msgattrib --clear-fuzzy -o ../gui/locale/opsi_quick_install_project.en.po ../gui/locale/opsi_quick_install_project.en.po'], removeFuzzys) then;

  // set constant form size
  Height := 450;
  Width := 730;
  // center forms nicely on screen
  Left := Round((Screen.Width - Width) / 2);
  Top := Round((Screen.Height - Height) / 2);
  panelLeft := Round((Width - panelWidth) * 2 / 3);
  // set constant button positions:
  BtnBack.Left := 20;
  //BtnNext.Width = 'with for english caption'
  BtnNext.Left := Width - BtnBack.Left - BtnNext.Width;
  BtnBack.Top := 410;
  BtnNext.Top := BtnBack.Top;

  SetBasics(self);

  ComboBoxLanguages.Left := Round((WelcomePanel.Width - ComboBoxLanguages.Width) / 2);
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

  logFileName := 'opsi_quickinstall.log';
  // .../lazarus/common/oslog.pas
  // log file in /tmp/opsi_quickinstall.log
  LogDatei := TLogInfo.Create;
  LogDatei.CreateTheLogfile(logFileName);
  logFileName := LogDatei.StandardMainLogPath + logFileName;

  Data:=TQuickInstallData.Create;

  // (compare function GetDefaultURL in osLinuxRepository:)
  // following two lines take time and are therefore executed only...
  // ...once at the beginning of this program
  // functions are from osfunclin
  // osfunclin needs definition 'SYNAPSE' in project settings
  Data.distroName := getLinuxDistroName;
  Data.distroRelease := getLinuxDistroRelease;

  // text by resourcestrings
  LabelWelcome.Caption := rsWelcome;
  LabelSelLanguage.Caption := rsSelLanguage;
  LabelSetup.Caption := rsSetup;
  RadioBtnDefault.Caption := rsStandard;
  RadioBtnCustom.Caption := rsCustom;
  LabelCarryOut.Caption := rsCarryOut;
  BtnNext.Caption := rsNext;
end;

procedure TQuickInstall.BtnNextClick(Sender: TObject);
begin
  if RadioBtnCustom.Checked then Data.custom := true
  else Data.custom := false;

  Distribution.ShowModal;
  // Get Width of BtnOverview and BtnFinish through invisible buttons:
  // Btn.Caption:=rsString and Btn.Width only work properly when Btn.Visible=True
  BtnOverview.Visible := True;
  BtnFinish.Visible := True;
  BtnOverview.Caption := rsOverviewBtn;
  BtnFinish.Caption := rsFinish;
  BtnOverviewWidth := BtnOverview.Width;
  BtnFinishWidth := BtnFinish.Width;
  BtnOverview.Visible := False;
  BtnFinish.Visible := False;
  //ShowMessage(BtnOverviewWidth.ToString + ', ' + BtnFinishWidth.ToString);

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
