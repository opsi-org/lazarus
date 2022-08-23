unit OpsiLinuxInstaller_PasswordForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  MaskEdit, LCLType, cthreads,
  osRunCommandElevated,
  OpsiPackageDownloader;

type
  TOpsiLinuxInstallerPasswordForm = class(TForm)
    BtnBack: TButton;
    BtnFinish: TButton;
    CheckBoxShowPassword: TCheckBox;
    EditPassword: TEdit;
    LabelRights: TLabel;
    LabelPassword: TLabel;
    PanelPassword: TPanel;
    PanelRights: TPanel;
    RadioBtnRoot: TRadioButton;
    RadioBtnSudo: TRadioButton;
    procedure BtnBackClick(Sender: TObject); virtual; abstract;
    procedure BtnFinishClick(Sender: TObject); virtual; abstract;
    procedure FormActivate(Sender: TObject); virtual;
    procedure EditPasswordUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure CheckBoxShowPasswordChange(Sender: TObject);
    function IsPasswordCorrect(MessageWrongPassword: string): boolean;
    procedure FormClose(Sender: TObject); virtual; abstract;
    procedure showResult; virtual; abstract;

  public
  var
    clientDataDir, Output: string;
    btnFinishClicked: boolean;
  end;

  TOpsiLinuxInstallerThread = class(TThread)
  protected
    FInstallRunCommand: TRunCommandElevated;
    FPackageManagementShellCommand, FClientDataDir, Output: string;
    FProductID, FDownloadPath: string;
    FTwoVersionsToTest, FOneInstallationFailed: boolean;
    FDefaultVersionName, FDownloadedVersionName, FCurrentVersionName: string;
    FDefaultVersion, FDownloadedVersion: string;
    FFileText: TStringList;
    FMessage: string;
    procedure ShowMessageOnForm; virtual; abstract;
    procedure DefineDirClientData;
    procedure GetOpsiScript; virtual; abstract;
    procedure ExecuteInstallationScript; virtual; abstract;
    procedure InstallOpsiProduct; virtual;
  public
    constructor Create(password: string; sudo: boolean;
      PackageManagementShellCommand: string; ProductID: string; DownloadPath: string);
    procedure Execute; override;
  end;

implementation

constructor TOpsiLinuxInstallerThread.Create(password: string;
  sudo: boolean; PackageManagementShellCommand: string; ProductID: string;
  DownloadPath: string);
begin
  FInstallRunCommand := TRunCommandElevated.Create(password, sudo);
  FPackageManagementShellCommand := PackageManagementShellCommand;
  FProductID := ProductID;
  FDownloadPath := DownloadPath;

  FreeOnTerminate := True;
  inherited Create(True);
end;

procedure TOpsiLinuxInstallerThread.DefineDirClientData;
var
  DefaultVersionSearch, DownloadedVersionSearch: TSearchRec;
begin
  FClientDataDir := ExtractFilePath(ParamStr(0));
  {$IFDEF DARWIN}
  FClientDataDir := FClientDataDir + '../../../../CLIENT_DATA/';
  {$ELSE DARWIN}
  Delete(FClientDataDir, Length(FClientDataDir), 1);
  FClientDataDir := ExtractFilePath(FClientDataDir);

  // try downloading latest configed and set FClientDataDir for the latest version
  if FTwoVersionsToTest and DownloadOpsiPackage(FProductID, FDownloadPath,
    FInstallRunCommand, FPackageManagementShellCommand) then
  begin
    // extract and compare version numbers of default and downloaded version
    if (FindFirst('../' + FProductID + '_*', faAnyFile and faDirectory,
      DefaultVersionSearch) = 0) and
      (FindFirst('../downloaded_' + FProductID + '_*', faAnyFile and
      faDirectory, DownloadedVersionSearch) = 0) then
    begin
      FDefaultVersionName := DefaultVersionSearch.Name;
      FDownloadedVersionName := DownloadedVersionSearch.Name;
      // extract version numbers
      FDefaultVersion := DefaultVersionSearch.Name;
      Delete(FDefaultVersion, 1, Pos('_', FDefaultVersion));
      FDownloadedVersion := DownloadedVersionSearch.Name;
      Delete(FDownloadedVersion, 1, Pos('_', FDownloadedVersion));
      Delete(FDownloadedVersion, 1, Pos('_', FDownloadedVersion));
      // compare and use latest configed version
      if FDownloadedVersion > FDefaultVersion then
        FCurrentVersionName := FDownloadedVersionName
      else
      begin
        FCurrentVersionName := FDefaultVersionName;
        if FDownloadedVersion = FDefaultVersion then
          FTwoVersionsToTest := False;
      end;
    end;
  end
  else
  if FOneInstallationFailed then
  begin
    // if there is a downloaded version but the latest version failed to install,
    // switch between FDefaultVersionName and FDownloadedVersionName to get the directory of
    // the older version
    if FDownloadedVersion > FDefaultVersion then
      FCurrentVersionName := FDefaultVersionName
    else
      FCurrentVersionName := FDownloadedVersionName;
  end
  else
  // otherwise, in the case that downloading the latest configed failed,
  // use the default one
  if FindFirst('../' + FProductID + '_*', faAnyFile and faDirectory,
    DefaultVersionSearch) = 0 then
  begin
    FDefaultVersionName := DefaultVersionSearch.Name;
    // extract version numbers
    FDefaultVersion := DefaultVersionSearch.Name;
    Delete(FDefaultVersion, 1, Pos('_', FDefaultVersion));
    FCurrentVersionName := FDefaultVersionName;
    FTwoVersionsToTest := False;
  end;
  FClientDataDir += FCurrentVersionName + '/CLIENT_DATA/';
  {$ENDIF DARWIN}
end;

procedure TOpsiLinuxInstallerThread.InstallOpsiProduct;
begin
  Synchronize(@GetOpsiScript);
  FTwoVersionsToTest := True;
  FOneInstallationFailed := False;
end;

procedure TOpsiLinuxInstallerThread.Execute;
begin
  // sleep to ensure that TWait is shown before GetOpsiScript is executed and blocks TWait
  Sleep(100);

  FFileText := TStringList.Create;
  InstallOpsiProduct;

  FFileText.Free;
  FInstallRunCommand.Free;
end;

procedure TOpsiLinuxInstallerPasswordForm.FormActivate(Sender: TObject);
begin
  // display password as dots
  EditPassword.EchoMode := emPassword;
end;

procedure TOpsiLinuxInstallerPasswordForm.EditPasswordUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  // finishing also possible by pressing enter after writing the password in EditPassword
  // #13 stands for the Enter key
  if UTF8Key = #13 then
    BtnFinish.Click;
end;

procedure TOpsiLinuxInstallerPasswordForm.CheckBoxShowPasswordChange(Sender: TObject);
begin
  // Change whether the password is shown as dots or text:
  // EditPassword.Text still gets the real text
  if CheckBoxShowPassword.Checked then
    // TMaskEdit can't do this!
    EditPassword.EchoMode := emNormal
  else
    EditPassword.EchoMode := emPassword;
end;

function TOpsiLinuxInstallerPasswordForm.IsPasswordCorrect(MessageWrongPassword: string): boolean;
var
  TestCommand: TRunCommandElevated;
begin
  Result := True;
  {$IFNDEF DARWIN}
  TestCommand := TRunCommandElevated.Create(EditPassword.Text, RadioBtnSudo.Checked);
  TestCommand.Run('mkdir /root/testDir', Output);
  if (Pos('Error', Output) >= 0) and (Output <> '') then
  begin
    ShowMessage(MessageWrongPassword);
    Result := False;
  end
  else
  begin
    TestCommand.Run('rm -rf /root/testDir', Output);
  end;
  FreeAndNil(TestCommand);
  {$ENDIF}
end;

end.
