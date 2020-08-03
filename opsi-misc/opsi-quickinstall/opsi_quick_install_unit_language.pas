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
    baseURLOpsi41 =
      'http://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.1:/stable/';
    baseURLOpsi42 =
      'http://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.2:/stable/';
    // more constants in FormCreate below
  var
    // same background image for all forms
    BackgrImageFileName: string;
    // for setting selectedAmount:=productAmount only the first time when Query3 is activated
    // Note that it doesn't work to define initialProds in opsi_quick_install_unit_query3 and
    // set it to True here in opsi_quick_install_unit_language.
    initialProds: boolean;
    MyDistr: TDistribution;
    DistrUrlPart: string;
  end;


// for less code
procedure showForm(newForm: TForm; Sender: TForm);
procedure AdjustPanelPosition(Sender: TForm);

var
  QuickInstall: TQuickInstall;

implementation

uses
  opsi_quick_install_unit_query, opsi_quick_install_unit_query4,
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
  end;
end;

{ TQuickInstall }

procedure TQuickInstall.FormCreate(Sender: TObject);
var
  Languages: TStringList;
  distroName, distroRelease: string;
begin
  // set constant form size
  Height := 450;
  Left := 360;
  Top := 170;
  Width := 730;
  // set constant button position
  BtnBack.Left := 20;
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

  if GetDefaultLang = 'de' then
    BtnNext.Left := Width - BtnBack.Left - 63; //BtnNext.Width = with for english caption

  initialProds := True;

  // .../lazarus/common/oslog.pas
  LogDatei := TLogInfo.Create;
  LogDatei.CreateTheLogfile('opsi_quickinstall.log');

  // (compare function GetDefaultURL in osLinuxRepository:)
  // following two lines take time and
  // are therefore executed only once at the beginning of this program
  distroName := getLinuxDistroName;
  distroRelease := getLinuxDistroRelease;
  //ShowMessage(distroName);
  //ShowMessage(distroRelease);

  // Change from distroName and -Release to TDistribution and respective URL part
  if (distroName = 'CentOS') and (Pos('7', distroRelease) = 1) then
    begin
      MyDistr := CentOS_7;
      DistrUrlPart:= 'CentOS_7/';
    end
  else
  if distroName = 'Debian' then
  begin
    if Pos('8', distroRelease) = 1 then
      begin
        MyDistr := Debian_8;
        DistrUrlPart:= 'Debian_8/';
      end
    else
    if Pos('9', distroRelease) = 1 then
      begin
        MyDistr := Debian_9;
        DistrUrlPart:= 'Debian_9/';
      end
    else
    if Pos('10', distroRelease) = 1 then
      begin
        MyDistr := Debian_10;
        DistrUrlPart:= 'Debian_10/';
      end;
  end
  else
  if distroName = 'openSUSE project' then
  begin
    if distroRelease = '15.1' then
      begin
        MyDistr := openSUSE_Leap_15_1;
        DistrUrlPart:= 'openSUSE_Leap_15.1/';
      end
    else if distroRelease = '42.3' then
      begin
        MyDistr := openSUSE_Leap_42_3;
        DistrUrlPart:= 'openSUSE_Leap_42.3/';
      end;
  end
  else
  if (distroName = 'RedHatEnterpriseServer') and (Pos('7', distroRelease) = 1) then
    begin
      MyDistr := RHEL_7;
      DistrUrlPart:= 'RHEL_7/';
    end
  else
  if distroName = 'Ubuntu' then
  begin
    if distroRelease = '16.04' then
      begin
        MyDistr := xUbuntu_16_04;
        DistrUrlPart:= 'xUbuntu_16.04/';
      end
    else
    if distroRelease = '18.04' then
      begin
        MyDistr := xUbuntu_18_04;
        DistrUrlPart:= 'xUbuntu_18.04/';
      end;
  end;
end;

procedure TQuickInstall.BtnNextClick(Sender: TObject);
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
