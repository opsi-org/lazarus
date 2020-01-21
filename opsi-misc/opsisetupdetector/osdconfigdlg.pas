unit osdconfigdlg;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Buttons, osdbasedata, PropEdits,
    lcltranslator;

type

  { TFOSDConfigdlg }

  TFOSDConfigdlg = class(TForm)
    BitBtn1: TBitBtn;
    FlowPanel1: TFlowPanel;
    Label1: TLabel;
    MemoConfigHint: TMemo;
    Panel1: TPanel;
    TIPropertyGrid1: TTIPropertyGrid;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TIPropertyGrid1Click(Sender: TObject);
    procedure TIPropertyGrid1Exit(Sender: TObject);
  private

  public

  end;

var
  FOSDConfigdlg: TFOSDConfigdlg;
    myconfigurationhints: TStringList;

(*
resourcestring

  // new for 4.1.0.2 ******************************************************************
  rsworkbench_Path = 'Path to the opsi_workbench';
  //rsPreInstallLines = 'opsi-script code, that will be included before the start of the installation.';
  rsworkbench_mounted = 'Automatically detected. Is the opsi workbench reachable at workbench_Path.';
  rsconfig_filled = 'Automatically detected. Do we have all needed configurations';
  rsregisterInFilemanager = 'Should this program be registred to the Filemanger (Explorer) context menu ?';
  rsemail_address = 'Your email address, used for the changelog entry';
  rsfullName = 'Your full name, used for the changelog entry';
  rsimport_libraries = 'List of opsi-script libraries that have to be imported.' +
    LineEnding + 'One per line. May be empty. Example:' + LineEnding +
    'myinstallhelperlib.opsiscript';
  rspreInstallLines = 'List of opsi-script code lines that should be included before the installation starts. '
     + LineEnding + 'One per line. May be empty. Example: ' + LineEnding
     + 'comment "Start the installation ..."';
  rspostInstallLines = 'List of opsi-script code lines that should be included after the installation finished.'
    + LineEnding + 'One per line. May be empty. Example:' + LineEnding +
    'comment "Installation finished..."';
  rspreUninstallLines = 'List of opsi-script code lines that should be included before the uninstallation starts.'
    + LineEnding + 'One per line. May be empty. Example:' + LineEnding +
    'comment "Start the uninstallation ..."';
  rspostUninstallLines = 'List of opsi-script code lines that should be included after the uninstallation finished.'
    + LineEnding + 'One per line. May be empty. Example:' + LineEnding +
    'comment "Uninstall finished..."';
  rspathToOpsiPackageBuilder = 'Path to the OpsiPackageBuilder. OpsiPackageBuilder is used to build the opsi packages via ssh. see: https://forum.opsi.org/viewtopic.php?f=22&t=7573';
  rscreateRadioIndex = 'selects the Create mode Radiobutton.';
  rscreateQuiet = 'Selects the Build mode Checkbox quiet.';
  rscreateBuild = 'Selects the Build mode Checkbox build.';
  rscreateInstall = 'Selects the Build mode Checkbox install.';
*)

implementation

{$R *.lfm}

{ TFOSDConfigdlg }

procedure TFOSDConfigdlg.FormActivate(Sender: TObject);
begin
  TIPropertyGrid1.TIObject := myconfiguration;
  TIPropertyGrid1.CheckboxForBoolean := True;
  //TIPropertyGrid1.PropertyEditorHook;
  myconfigurationhints.Clear;
  myconfigurationhints.Add('workbench_Path='+rsworkbench_Path);
  //myconfigurationhints.Add('preInstallLines = '+rsPreInstallLines);
  myconfigurationhints.Add('workbench_mounted='+rsworkbench_mounted);
  myconfigurationhints.Add('config_filled='+rsconfig_filled);
  myconfigurationhints.Add('registerInFilemanager='+rsRegisterInFilemanager);
  myconfigurationhints.Add('email_address='+rsEmail_address);
  myconfigurationhints.Add('fullName='+rsFullName);
  myconfigurationhints.Add('import_libraries='+rsImport_libraries);
  myconfigurationhints.Add('preInstallLines='+rsPreInstallLines);
  myconfigurationhints.Add('postInstallLines='+rsPostInstallLines);
  myconfigurationhints.Add('preUninstallLines='+rsPreUninstallLines);
  myconfigurationhints.Add('postUninstallLines='+rsPostUninstallLines);
  myconfigurationhints.Add('PathToOpsiPackageBuilder='+rsPathToOpsiPackageBuilder);
  myconfigurationhints.Add('CreateRadioIndex='+rsCreateRadioIndex);
  myconfigurationhints.Add('BuildRadioIndex='+rsBuildRadioIndex);
  (*
  myconfigurationhints.Add('CreateQuiet='+rsCreateQuiet);
  myconfigurationhints.Add('CreateBuild='+rsCreateBuild);
  myconfigurationhints.Add('CreateInstall='+rsCreateInstall);
  *)
end;

procedure TFOSDConfigdlg.FormCreate(Sender: TObject);
begin

    // Create Config Hints
  myconfigurationhints := TStringList.Create;



end;

procedure TFOSDConfigdlg.FormDestroy(Sender: TObject);
begin
    FreeAndNil(myconfigurationhints);
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
 // RegisterPropertyEditor(TypeInfo(TPProperties), TConfiguration, 'Properties',
 //   TCollectionPropertyEditor);




end.
