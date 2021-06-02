unit opsi_configed_installer_unit_language;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, LCLtranslator, Buttons, process;

type

  { TConfigedInstaller }

  TConfigedInstaller = class(TForm)
    BackgrImage: TImage;
    BtnBack: TButton;
    BtnFinish: TButton;
    BtnNext: TButton;
    ComboBoxLanguages: TComboBox;
    WelcomePanel: TPanel;
    ConfigedInstallerPanel: TPanel;
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
    // name for log file of ConfigedInstaller
    logFileName: string;
    // Set position for BtnNext on first form !manually! for right position
    // after language change.
    // needs to be done for each language!
    procedure SetBtnWidth(Language: string);
    // show hint on click of InfoImage
    // (used with '@' and therefore must be defined in TConfigedInstaller)
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
  ConfigedInstaller: TConfigedInstaller;

implementation

uses
  opsi_configed_installer_resourcestrings,
  opsi_configed_installer_data,
  opsi_configed_installer_unit_distr,
  opsi_configed_installer_unit_password,
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
  InfoImage.Width := ConfigedInstaller.infoSize;
  InfoImage.Height := ConfigedInstaller.infoSize;
  // set info image
  InfoImage.Picture.LoadFromFile(
    ExtractFilePath(ParamStr(0)) + ConfigedInstaller.InfoImageFileName);
  InfoImage.BorderSpacing.Left := 5;
  // Show info hints also on click of image
  InfoImage.OnClick := @ConfigedInstaller.ShowHintOnClick;
end;
// make Panel settings and load background and info images
procedure SetBasics(Sender: TForm);
var
  compIndex: integer;
  PanelBigLine, PanelSmallLine: TPanel;
begin
  for compIndex := 0 to Sender.ComponentCount - 1 do
  begin
    if (Sender.Components[compIndex].ClassName = 'TPanel') then
    begin
      (Sender.Components[compIndex] as TPanel).Left := ConfigedInstaller.panelLeft;
      (Sender.Components[compIndex] as TPanel).Width := ConfigedInstaller.panelWidth;
      (Sender.Components[compIndex] as TPanel).Color := clForm;
    end
    else
    if (Sender.Components[compIndex].ClassName = 'TImage') and
      // load info icon
      (Pos('Info', Sender.Components[compIndex].Name) = 1) then
      setInfoBasics(Sender.Components[compIndex] as TImage)
    else if (Sender.Components[compIndex].Name = 'BackgrImage') then
    begin
      // set background image
      (Sender.Components[compIndex] as TImage).Picture.LoadFromFile(
        ExtractFilePath(ParamStr(0)) + ConfigedInstaller.BackgrImageFileName);
      (Sender.Components[compIndex] as TImage).BorderSpacing.Top:=10;
    end;
  end;

  // big decoration line at bottom in opsi-blue
  PanelBigLine := TPanel.Create(Sender);
  PanelBigLine.Parent:=Sender;
  PanelBigLine.ParentColor:=False;
  PanelBigLine.BevelOuter:=bvNone;
  PanelBigLine.Height := 5;
  PanelBigLine.Top := Sender.Height - PanelBigLine.Height;
  PanelBigLine.Left := 0;
  PanelBigLine.Width := Sender.Width;
  // Opsi-Blau: #006599
  PanelBigLine.Color := TColor($00996500);
  PanelBigLine.BevelColor := TColor($00996500);

  // thin decoration line above the big one in opsi-red
  PanelSmallLine := TPanel.Create(Sender);
  PanelSmallLine.Parent:=Sender;
  PanelSmallLine.ParentColor:=False;
  PanelSmallLine.BevelOuter:=bvNone;
  PanelSmallLine.Height := 3;
  PanelSmallLine.Top := Sender.Height - 11;
  PanelSmallLine.Left := 0;
  PanelSmallLine.Width := Sender.Width;
  // Opsi-Rot: #B42554
  PanelSmallLine.Color := TColor($005425B4);
  PanelSmallLine.BevelColor := TColor($005425B4);
end;

{ TConfigedInstaller }

// Set position for BtnNext on first form !manually! for right position after
// language change.
// needs to be done for each language!
procedure TConfigedInstaller.SetBtnWidth(Language: string);
begin
  // width of BtnNext needs to be hard coded for every language for nice placement of BtnNext
  if Language = 'de' then
    BtnNextWidth := 63
  else if Language = 'en' then
    BtnNextWidth := 51
  else if Language = 'es' then
    BtnNextWidth := 80
  else if Language = 'fr' then
    BtnNextWidth := 68;
  //note that BtnNext.Width = width for english caption and
  //BtnNext.Left := Width - BtnNext.Width - BtnBack.Left; doesn't help either
  BtnNext.Left := Width - BtnBack.Left - BtnNextWidth;
end;
// show hint on click of InfoImage
// (used with '@' and therefore must be defined in TConfigedInstaller)
procedure TConfigedInstaller.ShowHintOnClick(Sender: TObject);
begin
  Application.ActivateHint(TWinControl(Sender).ClientToScreen(Point(1, 1)), True);
end;

procedure TConfigedInstaller.FormCreate(Sender: TObject);
var
  Languages: TStringList;
begin
  // set constant form size
  Height := 450;
  //Width := 730;
  Width := 675;
  // center form nicely on screen
  Left := Round((Screen.Width - Width) / 2);
  Top := Round((Screen.Height - Height) / 2);
  // position Panels with twice as much space to the left than to the right of the form
  panelLeft := Round((Width - panelWidth) * 2 / 3);
  // set constant button positions:
  BtnBack.Left := 20;
  //note that BtnNext.Width = width for english caption
  BtnNext.Left := Width - BtnBack.Left - BtnNext.Width;
  //BtnBack.Top := 410;
  BtnBack.Top := Height-50;
  BtnNext.Top := BtnBack.Top;

  SetBasics(self);

  ComboBoxLanguages.Left := Round((WelcomePanel.Width - ComboBoxLanguages.Width) / 2);
  with ComboBoxLanguages.Items do
  begin
    Add('Deutsch');
    Add('English');
    Add('Español');
    Add('Français');
  end;
  Languages := TStringList.Create;
  Languages.Add('de');
  Languages.Add('en');
  Languages.Add('es');
  Languages.Add('fr');
  // let the combo box show the system language at the beginning
  ComboBoxLanguages.ItemIndex := Languages.IndexOf(GetDefaultLang);
  // now set position of BtnNext for the default language
  SetBtnWidth(GetDefaultLang);

  // initialize log file:
  logFileName := 'opsi_configed_installer.log';
  LogDatei := TLogInfo.Create;
  LogDatei.CreateTheLogfile(logFileName);
  LogDatei.log('Log file created', LLnothing);
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  LogDatei.log('Working directory: ' + GetCurrentDir, LLinfo);
  // log file will be saved in /tmp/opsi_configed_installer.log
  logFileName := LogDatei.StandardMainLogPath + logFileName;

  // initialize data structure to store the ConfigedInstaller data for easier access
  Data := TConfigedInstallerData.Create;
  // Following two lines take time and are therefore executed only once at the
  // beginning of this program.
  Data.distroName := getLinuxDistroName;
  Data.distroRelease := getLinuxDistroRelease;

  // text by resourcestrings
  LabelWelcome.Caption := rsWelcome;
  LabelSelLanguage.Caption := rsSelLanguage;
  LabelCarryOut.Caption := rsCarryOut;
  BtnNext.Caption := rsNext;
end;

procedure TConfigedInstaller.BtnNextClick(Sender: TObject);
begin
  // before going on, let the user check the distribution
  Distribution.ShowModal;

  // Get width of BtnOverview(TQuery6) and BtnFinish(TOverview, TPassword)
  // through invisible buttons in TConfigedInstaller:
  // This is necessary because the positioning of the buttons does not work
  // properly on FormActivate in the respective forms (same problem as
  // here in TConfigedInstaller with BtnNext).
  // Btn.Caption:=rsString and Btn.Width only work properly when Btn.Visible=True
  BtnFinish.Visible := True;
  BtnFinish.Caption := rsFinish;
  BtnFinishWidth := BtnFinish.Width;
  BtnFinish.Visible := False;
  // Get width of BtnNext here once for procedure SetBtnWidth
  //ShowMessage(BtnNext.Width.ToString);

  // Distribution.GoOn tells TConfigedInstaller whether in TDistribution the next or
  // the back button was clicked, i.e. whether to go on to the next form or to
  // stay on TConfigedInstaller after TDistribution closed.
  if Distribution.GoOn then
  begin
    Enabled:=False;
    Password.Visible:=True;
  end;
end;

procedure TConfigedInstaller.ComboBoxLanguagesChange(Sender: TObject);
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
  else if ComboBoxLanguages.Text = 'Español' then
  begin
    SetDefaultLang('es');
    SetBtnWidth('es');
    LabelCarryOut.Caption := rsCarryOut;
  end
  else if ComboBoxLanguages.Text = 'Français' then
  begin
    SetDefaultLang('fr');
    SetBtnWidth('fr');
    LabelCarryOut.Caption := rsCarryOut;
  end;
end;

end.
