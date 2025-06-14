unit osdconfigdlg;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Buttons, osdbasedata, PropEdits,
  osddatamod,
  LclIntf,
  lcltranslator;

type

  { TFOSDConfigdlg }

  TFOSDConfigdlg = class(TForm)
    BitBtn1: TBitBtn;
    FlowPanel1: TFlowPanel;
    LabelCfgDlgHead: TLabel;
    MemoConfigHint: TMemo;
    Panel1: TPanel;
    SpeedButtonHelpConfig: TSpeedButton;
    TIPropertyGrid1: TTIPropertyGrid;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SpeedButtonHelpConfigClick(Sender: TObject);
    procedure TIPropertyGrid1Click(Sender: TObject);
    procedure TIPropertyGrid1Exit(Sender: TObject);
    procedure TIPropertyGrid1KeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure TIPropertyGrid1KeyPress(Sender: TObject; var Key: char);
    procedure TIPropertyGrid1KeyUp(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure TIPropertyGrid1Modified(Sender: TObject);
  private

  public

  end;

var
  FOSDConfigdlg: TFOSDConfigdlg;
  myconfigurationhints: TStringList;


implementation

uses
  osdform;

  {$R *.lfm}

  { TFOSDConfigdlg }

procedure TFOSDConfigdlg.FormActivate(Sender: TObject);
begin
  LabelCfgDlgHead.Caption := rsCnfdTitle;
  TIPropertyGrid1.TIObject := myconfiguration;
  TIPropertyGrid1.CheckboxForBoolean := True;
  myconfigurationhints.Clear;
  myconfigurationhints.Add('workbench_Path=' + rsworkbench_Path);
  myconfigurationhints.Add('workbench_mounted=' + rsworkbench_mounted);
  myconfigurationhints.Add('config_filled=' + rsconfig_filled);
  myconfigurationhints.Add('registerInFilemanager=' + rsRegisterInFilemanager);
  myconfigurationhints.Add('email_address=' + rsEmail_address);
  myconfigurationhints.Add('fullName=' + rsFullName);
  myconfigurationhints.Add('import_libraries=' + rsImport_libraries);
  myconfigurationhints.Add('preInstallLines=' + rsPreInstallLines);
  myconfigurationhints.Add('postInstallLines=' + rsPostInstallLines);
  myconfigurationhints.Add('preUninstallLines=' + rsPreUninstallLines);
  myconfigurationhints.Add('postUninstallLines=' + rsPostUninstallLines);
  myconfigurationhints.Add('PathToOpsiPackageBuilder=' + rsPathToOpsiPackageBuilder);
  myconfigurationhints.Add('CreateRadioIndex=' + rsCreateRadioIndex);
  myconfigurationhints.Add('BuildRadioIndex=' + rsBuildRadioIndex);

  myconfigurationhints.Add('config_version=' + rsConfigVersion);
  myconfigurationhints.Add('Readme_txt_templ=' + rsReadme_txt_templ);
  myconfigurationhints.Add('Show2StepMacSeletionWarn=' + rsInternalSet);
  myconfigurationhints.Add('ShowCheckEntryWarning=' + rsInternalSet);
  myconfigurationhints.Add('UsePropDesktopicon=' + rsUsePropDesktopicon);
  myconfigurationhints.Add('UsePropLicenseOrPool=' + rsUsePropLicenseOrPool);

  myconfigurationhints.Add('Service_URL=' + rsService_URL);
  myconfigurationhints.Add('Service_user=' + rsService_user);
  myconfigurationhints.Add('Service_pass=' + rsService_pass);
  myconfigurationhints.Add('preferSilent=' + rsPreferSilent);
  myconfigurationhints.Add('control_in_toml_format=' + rsControl_in_toml_format);
  myconfigurationhints.Add('dependencies_for_all_actionrequests=' +
    rsDependencies_for_all_actionrequests);
  myconfigurationhints.Add('preferMsiUninstall=' + rsPreferMsiUninstall);
  //myconfigurationhints.Add('writeMetaDataFile=' + rsWriteMetaDataFile);
  //myconfigurationhints.Add('ShowBackgroundInfoBtn=' + rsShowBackgroundInfoBtn);
  myconfigurationhints.Add('EnableBackgroundMetaData=' + rsEnableBackgroundMetaData);

  SetDefaultLang(osdsettings.mylang, osdsettings.mylocaledir);
  Repaint;
end;

procedure TFOSDConfigdlg.FormCreate(Sender: TObject);
var
  resourcedir: string;
  tmpimage: TPicture;
begin
  // Create Config Hints
  myconfigurationhints := TStringList.Create;
  DataModule1.SetFontName(TControl(Sender), myFont);
  DataModule1.SetFontName(TControl(Sender), myFont);
  {$IFDEF UNIX}
  tmpimage := TPicture.Create;
  // the first path is in the development environment
  resourcedir := ExtractFileDir(Application.ExeName);
  {$IFDEF DARWIN}
  resourcedir := ExtractFileDir(Application.ExeName) + PathDelim + '../Resources';
  {$ENDIF DARWIN}
  tmpimage.LoadFromFile(resourcedir + PathDelim + 'images' + PathDelim +
    'help-circle20.png');
  SpeedButtonHelpConfig.Glyph.Assign(tmpimage.Bitmap);
  FreeAndNil(tmpimage);
  {$ENDIF UNIX}
end;

procedure TFOSDConfigdlg.FormDestroy(Sender: TObject);
begin
  FreeAndNil(myconfigurationhints);
end;

procedure TFOSDConfigdlg.SpeedButtonHelpConfigClick(Sender: TObject);
var
  myUrl: string;
begin
  if LowerCase(osdsettings.mylang) = 'de' then
    myUrl := opsidocs_base_url +
      'opsi-docs-de/4.3/opsi-modules/setup-detector.html#opsi-setup-detector-use-start'
  else
    myUrl := opsidocs_base_url +
      'opsi-docs-en/4.3/opsi-modules/setup-detector.html#opsi-setup-detector-use-start';
  OpenURL(myUrl);
end;

procedure TFOSDConfigdlg.TIPropertyGrid1Click(Sender: TObject);
var
  activeprop: string;
begin
  if Sender = TIPropertyGrid1 then
  begin
    activeprop := TIPropertyGrid1.GetActiveRow.Name;
    MemoConfigHint.Text := myconfigurationhints.Values[activeprop];
  end;
end;

procedure TFOSDConfigdlg.TIPropertyGrid1Exit(Sender: TObject);
begin

end;

procedure TFOSDConfigdlg.TIPropertyGrid1KeyDown(Sender: TObject;
  var Key: word; Shift: TShiftState);
begin

end;

procedure TFOSDConfigdlg.TIPropertyGrid1KeyPress(Sender: TObject; var Key: char);
begin

end;

procedure TFOSDConfigdlg.TIPropertyGrid1KeyUp(Sender: TObject;
  var Key: word; Shift: TShiftState);
var
  activeprop: string;
begin
  if Sender = TIPropertyGrid1 then
  begin
    activeprop := TIPropertyGrid1.GetActiveRow.Name;
    MemoConfigHint.Text := myconfigurationhints.Values[activeprop];
  end;
end;

procedure TFOSDConfigdlg.TIPropertyGrid1Modified(Sender: TObject);
begin

end;

initialization

  RegisterPropertyEditor(TypeInfo(string), TConfiguration, 'workbench_path',
    TDirectoryPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TConfiguration, 'import_libraries',
    TStringsPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TConfiguration, 'preInstallLines',
    TStringsPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TConfiguration, 'postInstallLines',
    TStringsPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TConfiguration, 'PathToOpsiPackageBuilder',
    TFileNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TConfiguration, 'Readme_txt_templ',
    TFileNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TConfiguration, 'Service_pass',
    TPasswordStringPropertyEditor);

end.
