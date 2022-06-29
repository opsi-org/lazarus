unit LOpsiServerDownloader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, osRunCommandElevated, osLog, Process, DistributionInfo;

type

  {LOSDownloader}
  TLOSDownloader = class(TObject)
  private
    FDownloadResult: boolean;
    FLOSSearch: TSearchRec;
    FDistrInfo: TDistributionInfo;
    FDownloadedLOSVersion: string;
    FDefaultLOSVersion: string;
    FDownloadedLOSFolder: string;
    FLOSCommand: TRunCommandElevated;
    procedure InstallDownloadPackage;
    procedure InstallExtractionPackages;
    procedure ReadDownloadedLOSVersion;
    procedure ReadDefaultLOSVersion;
  public
    Output: string;
  const
    DownloadDir = 'download.uib.de/opsi4.2/testing/packages/linux/localboot/';
    LOSPackageName = 'l-opsi-server_4*.opsi';
    constructor Create(LOSCommand: TRunCommandElevated;
      DistrInfo: TDistributionInfo); overload;
    procedure ExtractFile(fileName: string);
    procedure CheckOutputForError;
    procedure DownloadLOSFromUib;
    procedure RemoveDownloadedLOSPackage;
    function AreLOSVersionsEqual: boolean;
    procedure RemoveOldDownloadedLOS;
    procedure CreateNewLOSFolderDir;
    procedure MoveLOSOpsiPackageToCurrentDir;
    procedure ExtractCpioFilesFromLOSPackage;
    procedure MoveCpioFilesToLOSFolder;
    procedure ExtractFoldersFromCpioFiles;
    procedure RemoveDownloadLeftovers;
    procedure ExtractLOSOpsiPackageAndSafeAsFolder;
    procedure LogDownloadResult;

    property DownloadResult: boolean read FDownloadResult;
  end;


function DownloadLOS(LOpsiServerCommand: TRunCommandElevated;
  DistrInfo: TDistributionInfo): boolean;

implementation

constructor TLOSDownloader.Create(LOSCommand: TRunCommandElevated;
  DistrInfo: TDistributionInfo); overload;
begin
  inherited Create;
  FDownloadResult := True;
  FLOSCommand := LOSCommand;
  FDistrInfo := DistrInfo;
end;

procedure TLOSDownloader.ExtractFile(fileName: string);
begin
  // cpio only works with bash not with sh
  if RunCommand('/bin/bash', ['-c', 'cpio --extract < ' + fileName], Output) then
  begin
    // if extraction works, Output should be something like '89 Blöcke'
    // if Output = '' then we don't know whether extraction worked -> no log entry
    if not (Output = '') then
      LogDatei.log(fileName + ' successfully extracted', LLInfo);
  end
  else
  begin
    LogDatei.log(fileName + ' extraction failed', LLessential);
    FDownloadResult := False;
  end;
end;

procedure TLOSDownloader.CheckOutputForError;
begin
  if Pos('Error', Output) > 0 then
    FDownloadResult := False;
end;

procedure TLOSDownloader.InstallDownloadPackage;
begin
  FDistrInfo.SetPackageManagementShellCommand;
  FLOSCommand.Run(FDistrInfo.PackageManagementShellCommand + 'update', Output);
  FLOSCommand.Run(FDistrInfo.PackageManagementShellCommand + 'install wget', Output);
end;

procedure TLOSDownloader.InstallExtractionPackages;
begin
  FLOSCommand.Run(FDistrInfo.PackageManagementShellCommand + 'install cpio', Output);
  FLOSCommand.Run(FDistrInfo.PackageManagementShellCommand + 'install gzip', Output);
end;

procedure TLOSDownloader.DownloadLOSFromUib;
begin
  InstallDownloadPackage;
  FLOSCommand.Run('wget -A ' + LOSPackageName + ' -r -l 1 https://' +
    downloadDir + ' -P ../', Output);
end;

procedure TLOSDownloader.ReadDownloadedLOSVersion;
begin
  if FindFirst('../' + downloadDir + LOSPackageName, faAnyFile and
    faDirectory, FLOSSearch) = 0 then
  begin
    FDownloadedLOSVersion := FLOSSearch.Name;
    Delete(FDownloadedLOSVersion, 1, Pos('_', FDownloadedLOSVersion));
    Delete(FDownloadedLOSVersion, Pos('.opsi', FDownloadedLOSVersion),
      FDownloadedLOSVersion.Length - Pos('.opsi', FDownloadedLOSVersion) + 1);
    FDownloadedLOSFolder := 'downloaded_l-opsi-server_' + FDownloadedLOSVersion;
  end
  else
    FDownloadResult := False;
end;

procedure TLOSDownloader.ReadDefaultLOSVersion;
begin
  if FindFirst('../l-opsi-server_4*', faAnyFile and
    faDirectory, FLOSSearch) = 0 then
  begin
    FDefaultLOSVersion := FLOSSearch.Name;
    Delete(FDefaultLOSVersion, 1, Pos('_', FDefaultLOSVersion));
  end;
end;

procedure TLOSDownloader.RemoveDownloadedLOSPackage;
begin
  if FindFirst('../download.uib.de', faAnyFile and
    faDirectory, FLOSSearch) = 0 then
    FLOSCommand.Run('rm -rf ../download.uib.de', Output);
end;

function TLOSDownloader.AreLOSVersionsEqual: boolean;
begin
  Result := False;
  ReadDownloadedLOSVersion;
  ReadDefaultLOSVersion;
  if FDefaultLOSVersion = FDownloadedLOSVersion then
    Result := True;
end;

procedure TLOSDownloader.RemoveOldDownloadedLOS;
begin
  if FindFirst('../downloaded_l-opsi-server_*', faAnyFile and
    faDirectory, FLOSSearch) = 0 then
    FLOSCommand.Run('rm -rf ../downloaded_l-opsi-server_*', Output);
end;

procedure TLOSDownloader.CreateNewLOSFolderDir;
begin
  FLOSCommand.Run('mkdir ../' + FDownloadedLOSFolder, Output);
  FLOSCommand.Run('mkdir ../' + FDownloadedLOSFolder + '/CLIENT_DATA', Output);
  FLOSCommand.Run('mkdir ../' + FDownloadedLOSFolder + '/OPSI', Output);
end;

procedure TLOSDownloader.MoveLOSOpsiPackageToCurrentDir;
begin
  FLOSCommand.Run('mv ../' + downloadDir + LOSPackageName + ' ./', Output);
  // remove dir of downloaded los
  FLOSCommand.Run('rm -rf ../download.uib.de', Output);
end;

procedure TLOSDownloader.ExtractCpioFilesFromLOSPackage;
begin
  InstallExtractionPackages;
  ExtractFile(LOSPackageName);
  FLOSCommand.Run('gunzip CLIENT_DATA.cpio.gz OPSI.cpio.gz', Output);
end;

procedure TLOSDownloader.MoveCpioFilesToLOSFolder;
begin
  FLOSCommand.Run('mv CLIENT_DATA.cpio ../' + FDownloadedLOSFolder +
    '/CLIENT_DATA/', Output);
  CheckOutputForError;
  FLOSCommand.Run('mv OPSI.cpio ../' + FDownloadedLOSFolder + '/OPSI/', Output);
  CheckOutputForError;
end;

procedure TLOSDownloader.ExtractFoldersFromCpioFiles;
begin
  SetCurrentDir('../' + FDownloadedLOSFolder + '/CLIENT_DATA/');
  ExtractFile('CLIENT_DATA.cpio');
  SetCurrentDir('../OPSI/');
  ExtractFile('OPSI.cpio');
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
end;

procedure TLOSDownloader.RemoveDownloadLeftovers;
begin
  FLOSCommand.Run('rm ' + LOSPackageName, Output);
  CheckOutputForError;
  FLOSCommand.Run(
    'rm ../' + FDownloadedLOSFolder + '/CLIENT_DATA/CLIENT_DATA.cpio', Output);
  CheckOutputForError;
  FLOSCommand.Run(
    'rm ../' + FDownloadedLOSFolder + '/OPSI/OPSI.cpio', Output);
  CheckOutputForError;
end;

procedure TLOSDownloader.ExtractLOSOpsiPackageAndSafeAsFolder;
begin
  RemoveOldDownloadedLOS;
  CreateNewLOSFolderDir;
  MoveLOSOpsiPackageToCurrentDir;
  ExtractCpioFilesFromLOSPackage;
  MoveCpioFilesToLOSFolder;
  ExtractFoldersFromCpioFiles;
  RemoveDownloadLeftovers;
end;

procedure TLOSDownloader.LogDownloadResult;
begin
  if FDownloadResult then
    LogDatei.log('l-opsi-server successfully downloaded (version ' +
      FDownloadedLOSVersion + ')', LLessential)
  else
    LogDatei.log('Downloading latest l-opsi-server failed. Using default l-opsi-server:',
      LLessential);
end;

function DownloadLOS(LOpsiServerCommand: TRunCommandElevated;
  DistrInfo: TDistributionInfo): boolean;
var
  LOSDownloader: TLOSDownloader;
begin
  LogDatei.log('Try downloading latest l-opsi-server:', LLnotice);
  Result := True;
  SetCurrentDir(ExtractFilePath(ParamStr(0)));

  LOSDownloader := TLOSDownloader.Create(LOpsiServerCommand, DistrInfo);
  LOSDownloader.DownloadLOSFromUib; // download latest released los from download.uib testing

  if LOSDownloader.AreLOSVersionsEqual then
  begin
    LOSDownloader.RemoveOldDownloadedLOS;
    LogDatei.log('Downloaded and default l-opsi-server are equal: Remove downloaded one again', LLnotice);
    LOSDownloader.RemoveDownloadedLOSPackage;
    Result := False;
  end
  else
  begin
    LOSDownloader.ExtractLOSOpsiPackageAndSafeAsFolder;
    LOSDownloader.LogDownloadResult;
    Result := LOSDownloader.DownloadResult;
  end;

  LOSDownloader.Free;
end;

end.
