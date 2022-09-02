unit OpsiLinuxInstaller_InstallationScriptExecuter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  osRunCommandElevated,
  OpsiPackageDownloader,
  oslog,
  IndependentMessageDisplayer;

type

  TInstallationScriptExecuter = class(TObject)
  private
    procedure DefineDirClientData;
    procedure RemoveOpsiScript;
  protected
    FInstallRunCommand: TRunCommandElevated;
    FPackageManagementShellCommand, FClientDataDir, Output: string;
    FProductID, FDownloadPath: string;
    FTwoVersionsToTest, FOneInstallationFailed: boolean;
    FDefaultVersionName, FDownloadedVersionName, FCurrentVersionName: string;
    FDefaultVersion, FDownloadedVersion: string;
    FFileText: TStringList;
    FMessageDisplayer: TIndependentMessageDisplayer;
    procedure WritePropertiesToFile; virtual;
    procedure GetOpsiScript;
    procedure ExecuteInstallationScript; virtual; abstract;
    function DidNewerVersionOfTwoVersionsFail: boolean; virtual; abstract;
    procedure TryOlderVersion; virtual; abstract;
    procedure LogResultOfLastInstallationAttempt; virtual; abstract;
  public
    procedure InstallOpsiProduct;
    constructor Create(password: string; sudo: boolean;
      PackageManagementShellCommand: string; ProductID: string; DownloadPath: string;
      MessageDisplayer: TIndependentMessageDisplayer);
  end;

implementation

constructor TInstallationScriptExecuter.Create(password: string;
  sudo: boolean; PackageManagementShellCommand: string; ProductID: string;
  DownloadPath: string; MessageDisplayer: TIndependentMessageDisplayer);
begin
  inherited Create;
  FFileText := TStringList.Create;
  FInstallRunCommand := TRunCommandElevated.Create(password, sudo);
  FPackageManagementShellCommand := PackageManagementShellCommand;
  FProductID := ProductID;
  FDownloadPath := DownloadPath;
  FMessageDisplayer := MessageDisplayer;
end;

procedure TInstallationScriptExecuter.DefineDirClientData;
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

procedure TInstallationScriptExecuter.WritePropertiesToFile;
begin
  DefineDirClientData;
  // TODO: Use FFileText to write the property values from the query to the file properties.conf in FClientDataDir
end;

procedure TInstallationScriptExecuter.GetOpsiScript;
begin
  // Get opsi-script_*.tar.gz from download.opensuse.org and extract it
  //FPackageManagementShellCommand :=
  //  GetPackageManagementShellCommand(Data.DistrInfo.DistroName);
  FInstallRunCommand.Run(FPackageManagementShellCommand + 'update', Output);
  FInstallRunCommand.Run(FPackageManagementShellCommand + 'install wget', Output);
  FInstallRunCommand.Run('wget -A opsi-script_*.tar.gz -r -l 1 ' +
    'https://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.2:/testing/xUbuntu_22.04/'
    + ' -nd -P ../', Output);
  FInstallRunCommand.Run('rm ../robots.*', Output);
  FInstallRunCommand.Run('tar -xvf ../opsi-script_*.tar.gz', Output);
  FInstallRunCommand.Run('rm ../opsi-script_*.tar.gz', Output);
end;

procedure TInstallationScriptExecuter.RemoveOpsiScript;
begin
  FInstallRunCommand.Run('rm -r BUILD/', Output);
end;

procedure TInstallationScriptExecuter.InstallOpsiProduct;
begin
  GetOpsiScript;
  FTwoVersionsToTest := True;
  FOneInstallationFailed := False;

  WritePropertiesToFile;
  ExecuteInstallationScript;
  if DidNewerVersionOfTwoVersionsFail then
    TryOlderVersion;

  RemoveOpsiScript;
  LogResultOfLastInstallationAttempt;
end;


end.
