unit opsi_quick_install_unit_language;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, LCLtranslator, osLinuxRepository;

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
    WelcomeLabel2: TLabel;
    LabelWelcome: TLabel;
    LabelSelLanguage: TLabel;
    procedure BtnNextClick(Sender: TObject);
    procedure ComboBoxLanguagesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FMyDistr: TDistribution;
    FDistrUrlPart: string;
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

    property MyDistr: TDistribution read FMyDistr;
    property DistrUrlPart: string read FDistrUrlPart;

    // procedure for setting MyDistr and DistrUrlPart depending on distroName and distroRelease
    procedure SetDistr(distrName: string; distrRelease: string);
  end;


// for less code
procedure showForm(newForm: TForm; Sender: TForm);
procedure AdjustPanelPosition(Sender: TForm);

var
  QuickInstall: TQuickInstall;

implementation

uses
  opsi_quick_install_unit_query, opsi_quick_install_unit_query4,
  oslog, osfunclin, opsi_quick_install_unit_distr;

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

procedure TQuickInstall.SetDistr(distrName: string; distrRelease: string);
begin
  // Change from distrName and -Release to TDistribution and respective URL part
  // Pos('7', distrRelease) = 1 checks whether string '7' occurs in string...
  // ...distrRelease at first position (1-based) for the fist time...
  // ...(meaning 0th position in array thinking).
  // CentOS has releases with names like 7.x-xxxx
  if (distrName = 'CentOS') and (Pos('7', distrRelease) = 1) then
  begin
    FMyDistr := CentOS_7;
    FDistrUrlPart := 'CentOS_7/';
  end
  else
  if distrName = 'Debian' then
  begin
    if distrRelease = '8' then
    begin
      FMyDistr := Debian_8;
      FDistrUrlPart := 'Debian_8/';
    end
    else
    if distrRelease = '9' then
    begin
      FMyDistr := Debian_9;
      FDistrUrlPart := 'Debian_9/';
    end
    else
    if distrRelease = '10' then
    begin
      FMyDistr := Debian_10;
      FDistrUrlPart := 'Debian_10/';
    end;
  end
  else
  if distrName = 'openSUSE Leap' then
  begin
    if distrRelease = '15.1' then
    begin
      FMyDistr := openSUSE_Leap_15_1;
      FDistrUrlPart := 'openSUSE_Leap_15.1/';
    end
    else if distrRelease = '42.3' then
    begin
      FMyDistr := openSUSE_Leap_42_3;
      FDistrUrlPart := 'openSUSE_Leap_42.3/';
    end;
  end
  else
  // RHEL has releases like 7.x
  if (distrName = 'RedHatEnterpriseServer') and (Pos('7', distrRelease) = 1) then
  begin
    FMyDistr := RHEL_7;
    FDistrUrlPart := 'RHEL_7/';
  end
  else
  if distrName = 'Univention' then
  begin
    if Pos('4.3', distrRelease) = 1 then
    begin
      FMyDistr := xUbuntu_16_04;
      FDistrUrlPart := 'Univention_4.3/';
    end
    else
    if Pos('4.4', distrRelease) = 1 then
    begin
      FMyDistr := xUbuntu_18_04;
      FDistrUrlPart := 'Univention_4.4/';
    end;
  end
  else
  if distrName = 'Ubuntu' then
  begin
    if distrRelease = '16.04' then
    begin
      FMyDistr := xUbuntu_16_04;
      FDistrUrlPart := 'xUbuntu_16.04/';
    end
    else
    if distrRelease = '18.04' then
    begin
      FMyDistr := xUbuntu_18_04;
      FDistrUrlPart := 'xUbuntu_18.04/';
    end;
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
  // following needs to be set for every language
  if GetDefaultLang = 'de' then
  begin
    // needs to be set for every language
    BtnNextWidth := 63;
    BtnOverviewWidth := 72;
    BtnFinishWidth := 88;
    //BtnNext.Width = 'with for english caption'
    //BtnNext.Left := Width - BtnNext.Width - BtnBack.Left; doesn't help
    BtnNext.Left := Width - BtnBack.Left - BtnNextWidth;
  end;

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
  SetDistr(distroName, distroRelease);
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
    SetDefaultLang('de')
  else
    SetDefaultLang('en');
  //ShowMessage(IntToStr(BtnNext.Width));
end;

end.
