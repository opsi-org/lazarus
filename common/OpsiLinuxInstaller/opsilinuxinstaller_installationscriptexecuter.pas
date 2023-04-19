unit OpsiLinuxInstaller_InstallationScriptExecuter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  osRunCommandElevated,
  OpsiPackageDownloader,
  oslog,
  IndependentMessageDisplayer,
  OpsiLinuxInstaller_LanguageObject;

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
    procedure ExecuteInstallationScript; virtual;
    function DidNewerVersionOfTwoVersionsFail: boolean; virtual; abstract;
    procedure TryOlderVersion;
    procedure LogResultOfLastInstallationAttempt(InstallationResult: string;
      ProductName: string; LogPath: string); overload;
    procedure LogResultOfLastInstallationAttempt; virtual; abstract; overload;
  public
    procedure InstallOpsiProduct;
    constructor Create(password: string; sudo: boolean;
      PackageManagementShellCommand: string; ProductID: string;
      DownloadPath: string; MessageDisplayer: TIndependentMessageDisplayer);
    destructor Destroy; override;
  end;

const
  {$IFDEF GUI}
  LongMessageSeperator = #10;
  {$ENDIF GUI}
  {$IFDEF NOGUI}
  LongMessageSeperator = '';
  {$ENDIF NOGUI}

resourcestring
  rsInstall = 'Installing ';
  rsWait = 'Please wait for the installation to start... ';
  rsDownloadLatest = 'Downloading latest ';
  rsSomeMin = '(This may take some minutes)';
  rsTryOlderVersion = 'Try older version of l-opsi-server';
  rsInstallation = 'Installation ';
  rsInstallationOf = 'Installation of ';
  rsFailed = 'failed';
  rsSuccess = 'successful';
  rsLog = 'You can find the log files here:';

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

  Language.TranslateResourceStrings('OpsiLinuxInstaller_InstallationScriptExecuter',
    'InstallationScriptExecuter.' + Language.Abbreviation + '.po');
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

  if FTwoVersionsToTest then
    FMessageDisplayer.DisplayMessage(rsDownloadLatest + FProductID +
      '... ' + LongMessageSeperator + rsSomeMin, True);

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
  begin
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
    begin
      // otherwise, in the case that downloading the latest version failed,
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
    end;
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
    + ' -nd', Output);
  FInstallRunCommand.Run('rm robots.*', Output);
  FInstallRunCommand.Run('tar -xvf opsi-script_*.tar.gz', Output);
  FInstallRunCommand.Run('rm opsi-script_*.tar.gz', Output);
end;

procedure TInstallationScriptExecuter.ExecuteInstallationScript;
begin
  FMessageDisplayer.DisplayMessage(rsInstall + FCurrentVersionName +
    '... ' + LongMessageSeperator + rsSomeMin, True);
  // TODO: Use opsi-script to execute the installation script for the opsi product
end;

procedure TInstallationScriptExecuter.TryOlderVersion;
begin
  FMessageDisplayer.DisplayMessage(rsInstallation + rsFailed + '.' +
    #10 + rsTryOlderVersion + '.', True);
  LogDatei.log('Installation failed: ' + FCurrentVersionName, LLessential);
  LogDatei.log('Try older version of ' + FProductID + ':', LLnotice);
  FTwoVersionsToTest := False;
  FOneInstallationFailed := True;
  WritePropertiesToFile;
  ExecuteInstallationScript;
end;

procedure TInstallationScriptExecuter.RemoveOpsiScript;
begin
  FInstallRunCommand.Run('rm -r BUILD/', Output);
end;

procedure TInstallationScriptExecuter.InstallOpsiProduct;
begin
  FMessageDisplayer.DisplayMessage(rsInstall + FProductID + ':' +
    #10 + rsWait + LongMessageSeperator + rsSomeMin, True);

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

procedure TInstallationScriptExecuter.LogResultOfLastInstallationAttempt(
  InstallationResult: string; ProductName: string; LogPath: string);
var
  ResultOfWholeInstallationProcess: string = '';
begin
  if InstallationResult = 'failed' then
  begin
    ResultOfWholeInstallationProcess := rsFailed;
    FMessageDisplayer.DisplayMessage(rsInstallation + rsFailed + '.', True);
    LogDatei.log('Installation failed: ' + FCurrentVersionName, LLessential);
    LogDatei.log(ProductName + ' installation failed', LLessential);
    ExitCode := 1;
  end
  else
  begin
    ResultOfWholeInstallationProcess := rsSuccess;
    LogDatei.log('Installation successful: ' + FCurrentVersionName, LLessential);
    LogDatei.log(ProductName + ' installation successful', LLessential);
  end;

  FMessageDisplayer.DisplayMessage(rsInstallationOf + ProductName +
    ' ' + ResultOfWholeInstallationProcess + '!' + #10 + #10 + rsLog +
    #10 + LogPath + #10 + StringReplace(LogDatei.FileName, '//', '/', [rfReplaceAll]));
end;

destructor TInstallationScriptExecuter.Destroy;
begin
  if Assigned(FInstallRunCommand) then FreeAndNil(FInstallRunCommand);
  if Assigned(FFileText) then FreeAndNil(FFileText);
  if Assigned(FMessageDisplayer) then FreeAndNil(FMessageDisplayer);
end;

end.
