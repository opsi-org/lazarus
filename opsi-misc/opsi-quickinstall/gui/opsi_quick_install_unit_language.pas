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
    // same size for all info images (squares)
    infoSize = 22;
    // same background image for all forms
    BackgrImageFileName = 'opsi.png';
    // same image for all infos
    InfoImageFileName = 'dialog-information.png';
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
    // name for log file of QuickInstall
    logFileName: string;
    // Set position for BtnNext on first form !manually! for right position
    // after language change.
    // needs to be done for each language!
    procedure SetBtnWidth(Language: string);
    // show hint on click of InfoImage
    // (used with '@' and therefore must be defined in TQuickInstall)
    procedure ShowHintOnClick(Sender: TObject);
  end;


// For constant layout (and less code):
// switch to the next form (set position of new form and adjust visibilities)
procedure showForm(newForm: TForm; Sender: TForm);
// make InfoImage settings
procedure setInfoBasics(InfoImage: TImage);
// make Panel settings and load background and info images
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

// For constant layout (and less code):
// switch to the next form (set position of new form and adjust visibilities)
procedure showForm(newForm: TForm; Sender: TForm);
begin
  newForm.Visible := True;

  newForm.Height := Sender.Height;
  newForm.Left := Sender.Left;
  newForm.Top := Sender.Top;
  newForm.Width := Sender.Width;

  Sender.Visible := False;
end;
// make InfoImage settings
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
// make Panel settings and load background and info images
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
      // load info icon
      (Pos('Info', Sender.Components[compIndex].Name) = 1) then
      setInfoBasics(Sender.Components[compIndex] as TImage)
    else if (Sender.Components[compIndex].Name = 'BackgrImage') then
      // set background image
      (Sender.Components[compIndex] as TImage).Picture.LoadFromFile(
        ExtractFilePath(ParamStr(0)) + QuickInstall.BackgrImageFileName);
  end;
end;

{ TQuickInstall }

// Set position for BtnNext on first form !manually! for right position after
// language change.
// needs to be done for each language!
procedure TQuickInstall.SetBtnWidth(Language: string);
begin
  if Language = 'de' then
  begin
    // needs to be hard coded for every language for nice placement of BtnNext
    BtnNextWidth := 63;
    //note that BtnNext.Width = width for english caption and
    //BtnNext.Left := Width - BtnNext.Width - BtnBack.Left; doesn't help either
    BtnNext.Left := Width - BtnBack.Left - BtnNextWidth;
  end
  else if Language = 'en' then
  begin
    BtnNextWidth := 51;
    BtnNext.Left := Width - BtnBack.Left - BtnNextWidth;
  end
  else if Language = 'fr' then
  begin
    BtnNextWidth := 68;
    BtnNext.Left := Width - BtnBack.Left - BtnNextWidth;
  end;
end;
// show hint on click of InfoImage
// (used with '@' and therefore must be defined in TQuickInstall)
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
  RunCommand('/bin/sh', ['-c', 'echo | msgattrib --clear-fuzzy -o ../gui/locale/opsi_quick_install_project.de.po ../gui/locale/opsi_quick_install_project.de.po'], removeFuzzys);
  RunCommand('/bin/sh', ['-c', 'echo | msgattrib --clear-fuzzy -o ../gui/locale/opsi_quick_install_project.en.po ../gui/locale/opsi_quick_install_project.en.po'], removeFuzzys);
  RunCommand('/bin/sh', ['-c', 'echo | msgattrib --clear-fuzzy -o ../gui/locale/opsi_quick_install_project.fr.po ../gui/locale/opsi_quick_install_project.fr.po'], removeFuzzys);

  // set constant form size
  Height := 450;
  Width := 730;
  // center form nicely on screen
  Left := Round((Screen.Width - Width) / 2);
  Top := Round((Screen.Height - Height) / 2);
  // position Panels with twice as much space to the left than to the right of the form
  panelLeft := Round((Width - panelWidth) * 2 / 3);
  // set constant button positions:
  BtnBack.Left := 20;
  //note that BtnNext.Width = width for english caption
  BtnNext.Left := Width - BtnBack.Left - BtnNext.Width;
  BtnBack.Top := 410;
  BtnNext.Top := BtnBack.Top;

  SetBasics(self);

  ComboBoxLanguages.Left := Round((WelcomePanel.Width - ComboBoxLanguages.Width) / 2);
  with ComboBoxLanguages.Items do
  begin
    Add('Deutsch');
    Add('English');
    Add('Français');
  end;
  Languages := TStringList.Create;
  Languages.Add('de');
  Languages.Add('en');
  Languages.Add('fr');
  // let the combo box show the system language at the beginning
  ComboBoxLanguages.ItemIndex := Languages.IndexOf(GetDefaultLang);
  // now set position of BtnNext for the default language
  SetBtnWidth(GetDefaultLang);

  // initialize log file:
  logFileName := 'opsi_quickinstall.log';
  LogDatei := TLogInfo.Create;
  LogDatei.CreateTheLogfile(logFileName);
  // log file will be saved in /tmp/opsi_quickinstall.log
  logFileName := LogDatei.StandardMainLogPath + logFileName;

  // initialize data structure to store the QuickInstall data for easier access
  Data := TQuickInstallData.Create;
  // Following two lines take time and are therefore executed only once at the
  // beginning of this program.
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
  // store in Data whether we are in custom installation or not
  if RadioBtnCustom.Checked then
    Data.custom := True
  else
    Data.custom := False;

  // before going on, let the user check the distribution
  Distribution.ShowModal;

  // Get width of BtnOverview(TQuery6) and BtnFinish(TOverview, TPassword)
  // through invisible buttons in TQuickInstall:
  // This is necessary because the positioning of the buttons does not work
  // properly on FormActivate in the respective forms (same problem as
  // here in TQuickInstall with BtnNext).
  // Btn.Caption:=rsString and Btn.Width only work properly when Btn.Visible=True
  BtnOverview.Visible := True;
  BtnFinish.Visible := True;
  BtnOverview.Caption := rsOverviewBtn;
  BtnFinish.Caption := rsFinish;
  BtnOverviewWidth := BtnOverview.Width;
  BtnFinishWidth := BtnFinish.Width;
  BtnOverview.Visible := False;
  BtnFinish.Visible := False;
  // Get width of BtnNext here once for procedure SetBtnWidth
  //ShowMessage(BtnNext.Width.ToString);

  // Distribution.GoOn tells TQuickInstall whether in TDistribution the next or
  // the back button was clicked, i.e. whether to go on to the next form or to
  // stay on TQuickInstall after TDistribution closed.
  if Distribution.GoOn then
  begin
    // in standard setup go on to TQuery4
    if not Data.custom then
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
      // in custom setup go on to TQuery
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
    // Somehow the following made problems with de->en->de translation so we set
    // it here always again.
    LabelCarryOut.Caption := rsCarryOut;
  end
  else if ComboBoxLanguages.Text = 'English' then
  begin
    SetDefaultLang('en');
    SetBtnWidth('en');
  end
  else if ComboBoxLanguages.Text = 'Français' then
  begin
    SetDefaultLang('fr');
    SetBtnWidth('fr');
    LabelCarryOut.Caption := rsCarryOut;
  end;
end;

end.
