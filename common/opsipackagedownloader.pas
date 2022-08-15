unit OpsiPackageDownloader;

{
Use the function DownloadOpsiPackage to download an opsi package from a
website (e.g. download.uib) and extract the folders CLIENT_DATA and OPSI to the
newly created folder [BinaryDirectory]/../downloaded_[OpsiPackageId]_[PackageVersion]/
}

{
This unit is only tested under Linux so far!
}

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  osRunCommandElevated,
  osLog,
  Process,
  DistributionInfo;

type
  TOpsiPackageDownloader = class(TObject)
  private
    FOpsiPackageId: string;
    FDownloadDir: string;
    FDownloadResult: boolean;
    FOpsiPackageSearch: TSearchRec;
    FDistrInfo: TDistributionInfo;
    FDownloadedOpsiPackageVersion: string;
    FDefaultOpsiPackageVersion: string;
    FDownloadedOpsiPackageFolder: string;
    FOpsiPackageDownloadCommand: TRunCommandElevated;
    procedure InstallDownloadPackage;
    procedure ReadDownloadedOpsiPackageVersion;
    procedure ReadDefaultOpsiPackageVersion;
    procedure RemoveOldDownloadedOpsiPackageFolder;
    procedure InstallExtractionPackages;
    procedure CreateNewOpsiPackageFolderDir;
    procedure MoveOpsiPackageToCurrentDir;
    procedure ExtractCpioFile(fileName: string);
    procedure ExtractCpioFilesFromOpsiPackage;
    procedure CheckOutputForError;
    procedure MoveCpioFilesToOpsiPackageFolder;
    procedure ExtractFoldersFromCpioFiles;
    procedure RemoveDownloadLeftovers;
    procedure ExtractOpsiPackage;
  public
    Output: string;
  const
    //DownloadDir = 'download.uib.de/opsi4.2/testing/packages/linux/localboot/';
    constructor Create(OpsiPackageId: string; DownloadDir: string;
      OpsiPackageDownloadCommand: TRunCommandElevated; DistrInfo: TDistributionInfo);
      overload;
    procedure DownloadOpsiPackageFromUib;
    function AreOpsiPackageVersionsEqual: boolean;
    procedure RemoveDownloadedOpsiPackageFolder;
    procedure LogDownloadResult;

    property DownloadResult: boolean read FDownloadResult;
  end;


function ExtractFileWithCpio(FileName: string): boolean;

function DownloadOpsiPackage(OpsiPackageId: string; DownloadDir: string;
  OpsiPackageDownloadCommand: TRunCommandElevated;
  DistrInfo: TDistributionInfo): boolean;


implementation

function ExtractFileWithCpio(FileName: string): boolean;
var
  Output: string;
begin
  Result := True;
  // cpio only works with bash not with sh
  if RunCommand('/bin/bash', ['-c', 'cpio --extract < ' + FileName], Output) then
  begin
    // if extraction works, Output should be something like '89 Blöcke'
    // if Output = '' then we don't know whether extraction worked -> no log entry
    if not (Output = '') then
      LogDatei.log(FileName + ' successfully extracted', LLInfo);
  end
  else
  begin
    LogDatei.log(FileName + ' extraction failed', LLessential);
    Result := False;
  end;
end;

{TOpsiPackageDownloader}

constructor TOpsiPackageDownloader.Create(OpsiPackageId: string;
  DownloadDir: string; OpsiPackageDownloadCommand: TRunCommandElevated;
  DistrInfo: TDistributionInfo);
  overload;
begin
  inherited Create;
  FDownloadResult := True;
  FOpsiPackageId := OpsiPackageId;
  FDownloadDir := DownloadDir;
  FOpsiPackageDownloadCommand := OpsiPackageDownloadCommand;
  FDistrInfo := DistrInfo;
end;

procedure TOpsiPackageDownloader.InstallDownloadPackage;
begin
  FOpsiPackageDownloadCommand.Run(FDistrInfo.PackageManagementShellCommand +
    'update', Output);
  FOpsiPackageDownloadCommand.Run(FDistrInfo.PackageManagementShellCommand +
    'install wget', Output);
end;

procedure TOpsiPackageDownloader.DownloadOpsiPackageFromUib;
begin
  InstallDownloadPackage;
  FOpsiPackageDownloadCommand.Run('wget -A ' + FOpsiPackageId +
    '_*.opsi -r -l 1 https://' + FDownloadDir + ' -P ../', Output);
end;


procedure TOpsiPackageDownloader.ReadDownloadedOpsiPackageVersion;
begin
  if FindFirst('../' + FDownloadDir + FOpsiPackageId + '_*.opsi',
    faAnyFile and faDirectory, FOpsiPackageSearch) = 0 then
  begin
    FDownloadedOpsiPackageVersion := FOpsiPackageSearch.Name;
    Delete(FDownloadedOpsiPackageVersion, 1, Pos('_', FDownloadedOpsiPackageVersion));
    Delete(FDownloadedOpsiPackageVersion, Pos('.opsi', FDownloadedOpsiPackageVersion),
      FDownloadedOpsiPackageVersion.Length - Pos('.opsi',
      FDownloadedOpsiPackageVersion) + 1);
    FDownloadedOpsiPackageFolder :=
      'downloaded_' + FOpsiPackageId + '_' + FDownloadedOpsiPackageVersion;
  end
  else
    FDownloadResult := False;
end;

procedure TOpsiPackageDownloader.ReadDefaultOpsiPackageVersion;
begin
  if FindFirst('../' + FOpsiPackageId + '_*', faAnyFile and faDirectory,
    FOpsiPackageSearch) = 0 then
  begin
    FDefaultOpsiPackageVersion := FOpsiPackageSearch.Name;
    Delete(FDefaultOpsiPackageVersion, 1, Pos('_', FDefaultOpsiPackageVersion));
  end;
end;

function TOpsiPackageDownloader.AreOpsiPackageVersionsEqual: boolean;
begin
  Result := False;
  ReadDownloadedOpsiPackageVersion;
  ReadDefaultOpsiPackageVersion;
  if FDefaultOpsiPackageVersion = FDownloadedOpsiPackageVersion then
    Result := True;
end;

procedure TOpsiPackageDownloader.RemoveDownloadedOpsiPackageFolder;
begin
  if FindFirst('../download.uib.de', faAnyFile and faDirectory,
    FOpsiPackageSearch) = 0 then
    FOpsiPackageDownloadCommand.Run('rm -rf ../download.uib.de', Output);
end;

procedure TOpsiPackageDownloader.RemoveOldDownloadedOpsiPackageFolder;
begin
  if FindFirst('../downloaded_' + FOpsiPackageId + '_*', faAnyFile and
    faDirectory, FOpsiPackageSearch) = 0 then
    FOpsiPackageDownloadCommand.Run('rm -rf ../downloaded_' +
      FOpsiPackageId + '_*', Output);
end;

procedure TOpsiPackageDownloader.CreateNewOpsiPackageFolderDir;
begin
  FOpsiPackageDownloadCommand.Run('mkdir ../' + FDownloadedOpsiPackageFolder, Output);
  FOpsiPackageDownloadCommand.Run('mkdir ../' + FDownloadedOpsiPackageFolder +
    '/CLIENT_DATA', Output);
  FOpsiPackageDownloadCommand.Run('mkdir ../' + FDownloadedOpsiPackageFolder +
    '/OPSI', Output);
end;

procedure TOpsiPackageDownloader.MoveOpsiPackageToCurrentDir;
begin
  FOpsiPackageDownloadCommand.Run('mv ../' + FDownloadDir + FOpsiPackageId +
    '_*.opsi ./', Output);
  RemoveDownloadedOpsiPackageFolder;
end;

procedure TOpsiPackageDownloader.ExtractCpioFile(FileName: string);
begin
  if not ExtractFileWithCpio(FileName) then
    FDownloadResult := False;
end;

procedure TOpsiPackageDownloader.InstallExtractionPackages;
begin
  FOpsiPackageDownloadCommand.Run(FDistrInfo.PackageManagementShellCommand +
    'install cpio', Output);
  FOpsiPackageDownloadCommand.Run(FDistrInfo.PackageManagementShellCommand +
    'install gzip', Output);
end;

procedure TOpsiPackageDownloader.ExtractCpioFilesFromOpsiPackage;
begin
  InstallExtractionPackages;
  ExtractCpioFile(FOpsiPackageId + '_*.opsi');
  FOpsiPackageDownloadCommand.Run('gunzip CLIENT_DATA.cpio.gz OPSI.cpio.gz', Output);
end;

procedure TOpsiPackageDownloader.CheckOutputForError;
begin
  if Pos('Error', Output) > 0 then
    FDownloadResult := False;
end;

procedure TOpsiPackageDownloader.MoveCpioFilesToOpsiPackageFolder;
begin
  FOpsiPackageDownloadCommand.Run('mv CLIENT_DATA.cpio ../' +
    FDownloadedOpsiPackageFolder + '/CLIENT_DATA/', Output);
  CheckOutputForError;
  FOpsiPackageDownloadCommand.Run('mv OPSI.cpio ../' + FDownloadedOpsiPackageFolder +
    '/OPSI/', Output);
  CheckOutputForError;
end;

procedure TOpsiPackageDownloader.ExtractFoldersFromCpioFiles;
begin
  SetCurrentDir('../' + FDownloadedOpsiPackageFolder + '/CLIENT_DATA/');
  ExtractCpioFile('CLIENT_DATA.cpio');
  SetCurrentDir('../OPSI/');
  ExtractCpioFile('OPSI.cpio');
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
end;

procedure TOpsiPackageDownloader.RemoveDownloadLeftovers;
begin
  FOpsiPackageDownloadCommand.Run('rm ' + FOpsiPackageId + '_*.opsi', Output);
  CheckOutputForError;
  FOpsiPackageDownloadCommand.Run(
    'rm ../' + FDownloadedOpsiPackageFolder + '/CLIENT_DATA/CLIENT_DATA.cpio', Output);
  CheckOutputForError;
  FOpsiPackageDownloadCommand.Run(
    'rm ../' + FDownloadedOpsiPackageFolder + '/OPSI/OPSI.cpio', Output);
  CheckOutputForError;
end;

procedure TOpsiPackageDownloader.ExtractOpsiPackage;
begin
  RemoveOldDownloadedOpsiPackageFolder;
  CreateNewOpsiPackageFolderDir;
  MoveOpsiPackageToCurrentDir;
  ExtractCpioFilesFromOpsiPackage;
  MoveCpioFilesToOpsiPackageFolder;
  ExtractFoldersFromCpioFiles;
  RemoveDownloadLeftovers;
end;

procedure TOpsiPackageDownloader.LogDownloadResult;
begin
  if FDownloadResult then
    LogDatei.log(FOpsiPackageId + ' successfully downloaded (version ' +
      FDownloadedOpsiPackageVersion + ')', LLessential)
  else
    LogDatei.log('Downloading latest ' + FOpsiPackageId +
      ' failed. Using default ' + FOpsiPackageId + ':',
      LLessential);
end;

function DownloadOpsiPackage(OpsiPackageId: string; DownloadDir: string;
  OpsiPackageDownloadCommand: TRunCommandElevated;
  DistrInfo: TDistributionInfo): boolean;
var
  OpsiPackageDownloader: TOpsiPackageDownloader;
begin
  LogDatei.log('Try downloading latest ' + OpsiPackageId + ':', LLnotice);
  Result := True;
  SetCurrentDir(ExtractFilePath(ParamStr(0)));

  OpsiPackageDownloader := TOpsiPackageDownloader.Create(OpsiPackageId,
    DownloadDir, OpsiPackageDownloadCommand, DistrInfo);
  OpsiPackageDownloader.DownloadOpsiPackageFromUib;

  // Compare the downloaded package with an already existing package:
  // Delete the downloaded package again if both versions are equal,
  // otherwise extract the downloaded package.
  // In case of no default package, the versions are obviously not equal and the
  // downloaded package is kept and extracted.
  if OpsiPackageDownloader.AreOpsiPackageVersionsEqual then
  begin
    LogDatei.log('Downloaded and default ' + OpsiPackageId +
      ' are equal: Remove downloaded ' + OpsiPackageId + ' again',
      LLnotice);
    OpsiPackageDownloader.RemoveDownloadedOpsiPackageFolder;
    Result := False;
  end
  else
  begin
    OpsiPackageDownloader.ExtractOpsiPackage;
    OpsiPackageDownloader.LogDownloadResult;
    Result := OpsiPackageDownloader.DownloadResult;
  end;

  OpsiPackageDownloader.Free;
end;

end.
