unit OpsiPackageDownloader;

{
Use the function DownloadOpsiPackage to download an opsi package from a
website (e.g. download.uib) and extract the folders CLIENT_DATA and OPSI to the
newly created folder [BinaryDirectory]/../downloaded_[OpsiPackageId]_[PackageVersion]/
}

{
This unit is only for Linux so far!
}

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  osRunCommandElevated,
  osLog,
  Process;

type
  TOpsiPackageDownloader = class(TObject)
  private
    FOpsiPackageId: string;
    FDownloadDir: string;
    FDownloadResult: boolean;
    FOpsiPackageSearch: TSearchRec;
    FPackageManagementShellCommand: string;
    FDownloadedOpsiPackageVersion: string;
    FDefaultOpsiPackageVersion: string;
    FDownloadedOpsiPackageFolder: string;
    FOpsiPackageDownloadCommand: TRunCommandElevated;
    procedure InstallDownloadPackage;
    procedure ReadDownloadedOpsiPackageVersion;
    procedure ReadDefaultOpsiPackageVersion;
    procedure RemoveDownloadedOpsiPackage;
    procedure RemoveOldDownloadedOpsiPackageFolder;
    procedure InstallExtractionPackages;
    procedure CreateNewOpsiPackageFolderDir;
    procedure ExtractFile(fileName: string);
    procedure ExtractFilesFromOpsiPackage;
    procedure CheckOutputForError;
    procedure MoveFilesToOpsiPackageFolder;
    procedure ExtractFoldersFromFiles;
    procedure RemoveDownloadLeftovers;
    procedure ExtractOpsiPackage;
  public
    Output: string;
  const
    constructor Create(OpsiPackageId: string; DownloadDir: string;
      OpsiPackageDownloadCommand: TRunCommandElevated;
      PackageManagementShellCommand: string);
      overload;
    procedure DownloadOpsiPackageFromUib;
    function AreOpsiPackageVersionsEqual: boolean;
    procedure LogDownloadResult;

    property DownloadResult: boolean read FDownloadResult;
  end;


function RunShellCommandWithoutSpecialRights(CommandLine: string;
  out Output: string; Shell: string = '/bin/sh'): boolean;

function ExtractFileWithTar(FileName: string): boolean;

function DownloadOpsiPackage(OpsiPackageId: string; DownloadDir: string;
  OpsiPackageDownloadCommand: TRunCommandElevated;
  PackageManagementShellCommand: string): boolean;


implementation

function RunShellCommandWithoutSpecialRights(CommandLine: string;
  out Output: string; Shell: string = '/bin/sh'): boolean;
begin
  LogDatei.log('Shell command: ' + CommandLine, LLInfo);
  if RunCommand(Shell, ['-c', CommandLine], Output,
    [poWaitOnExit, poUsePipes, poStderrToOutPut], swoShow) then
  begin
    LogDatei.log('Shell output: ' + Output, LLDebug);
    Result := True;
  end
  else
  begin
    if Length(Output) > 1000 then
      LogDatei.log('Error in RunCommand: ' + Copy(Output, 1, 1000) + '...', LLInfo)
    else
      LogDatei.log('Error in RunCommand: ' + Output, LLInfo);

    Result := False;
  end;
end;

function ExtractFileWithTar(FileName: string): boolean;
var
  Output: string;
begin
  Result := True;
  if RunShellCommandWithoutSpecialRights('tar -vixf ' + FileName,
    Output, '/bin/bash') then
  begin
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
  PackageManagementShellCommand: string);
  overload;
begin
  inherited Create;
  FDownloadResult := True;
  FOpsiPackageId := OpsiPackageId;
  FDownloadDir := DownloadDir;
  FOpsiPackageDownloadCommand := OpsiPackageDownloadCommand;
  FPackageManagementShellCommand := PackageManagementShellCommand;
end;

procedure TOpsiPackageDownloader.InstallDownloadPackage;
begin
  FOpsiPackageDownloadCommand.Run(FPackageManagementShellCommand + 'update', Output);
  FOpsiPackageDownloadCommand.Run(FPackageManagementShellCommand +
    'install wget', Output);
end;

procedure TOpsiPackageDownloader.DownloadOpsiPackageFromUib;
begin
  InstallDownloadPackage;
  RunShellCommandWithoutSpecialRights('wget -A ' + FOpsiPackageId +
    '_*.opsi -r -l 1 https://' + FDownloadDir + ' -nd', Output);
end;


procedure TOpsiPackageDownloader.ReadDownloadedOpsiPackageVersion;
begin
  if FindFirst(FOpsiPackageId + '_*.opsi', faAnyFile and faDirectory,
    FOpsiPackageSearch) = 0 then
  begin
    FDownloadedOpsiPackageVersion := FOpsiPackageSearch.Name;
    Delete(FDownloadedOpsiPackageVersion, 1, Pos('_', FDownloadedOpsiPackageVersion));
    Delete(FDownloadedOpsiPackageVersion, Pos('.opsi', FDownloadedOpsiPackageVersion),
      FDownloadedOpsiPackageVersion.Length -
      Pos('.opsi', FDownloadedOpsiPackageVersion) + 1);
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

procedure TOpsiPackageDownloader.RemoveDownloadedOpsiPackage;
begin
  RunShellCommandWithoutSpecialRights('rm ' + FOpsiPackageId + '_' +
    FDownloadedOpsiPackageVersion + '.opsi', Output);
end;

procedure TOpsiPackageDownloader.RemoveOldDownloadedOpsiPackageFolder;
begin
  if FindFirst('../downloaded_' + FOpsiPackageId + '_*', faAnyFile and
    faDirectory, FOpsiPackageSearch) = 0 then
    RunShellCommandWithoutSpecialRights('rm -rf ../downloaded_' +
      FOpsiPackageId + '_*', Output);
end;

procedure TOpsiPackageDownloader.CreateNewOpsiPackageFolderDir;
begin
  RunShellCommandWithoutSpecialRights('mkdir ../' +
    FDownloadedOpsiPackageFolder, Output);
  RunShellCommandWithoutSpecialRights('mkdir ../' + FDownloadedOpsiPackageFolder +
    '/CLIENT_DATA', Output);
  RunShellCommandWithoutSpecialRights('mkdir ../' + FDownloadedOpsiPackageFolder +
    '/OPSI', Output);
end;

procedure TOpsiPackageDownloader.ExtractFile(FileName: string);
begin
  if not ExtractFileWithTar(FileName) then
    FDownloadResult := False;
end;

procedure TOpsiPackageDownloader.InstallExtractionPackages;
begin
  FOpsiPackageDownloadCommand.Run(FPackageManagementShellCommand +
    'install gzip', Output);
end;

procedure TOpsiPackageDownloader.ExtractFilesFromOpsiPackage;
begin
  InstallExtractionPackages;
  ExtractFile(FOpsiPackageId + '_*.opsi');
  RunShellCommandWithoutSpecialRights('gunzip CLIENT_DATA.tar.gz OPSI.tar.gz', Output);
end;

procedure TOpsiPackageDownloader.CheckOutputForError;
begin
  if Pos('Error', Output) > 0 then
    FDownloadResult := False;
end;

procedure TOpsiPackageDownloader.MoveFilesToOpsiPackageFolder;
begin
  RunShellCommandWithoutSpecialRights('mv CLIENT_DATA.tar ../' +
    FDownloadedOpsiPackageFolder + '/CLIENT_DATA/', Output);
  CheckOutputForError;
  RunShellCommandWithoutSpecialRights('mv OPSI.tar ../' +
    FDownloadedOpsiPackageFolder + '/OPSI/', Output);
  CheckOutputForError;
end;

procedure TOpsiPackageDownloader.ExtractFoldersFromFiles;
begin
  SetCurrentDir('../' + FDownloadedOpsiPackageFolder + '/CLIENT_DATA/');
  ExtractFile('CLIENT_DATA.tar');
  SetCurrentDir('../OPSI/');
  ExtractFile('OPSI.tar');
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
end;

procedure TOpsiPackageDownloader.RemoveDownloadLeftovers;
begin
  RunShellCommandWithoutSpecialRights('rm ' + FOpsiPackageId + '_*.opsi', Output);
  CheckOutputForError;
  RunShellCommandWithoutSpecialRights(
    'rm ../' + FDownloadedOpsiPackageFolder + '/CLIENT_DATA/CLIENT_DATA.tar', Output);
  CheckOutputForError;
  RunShellCommandWithoutSpecialRights(
    'rm ../' + FDownloadedOpsiPackageFolder + '/OPSI/OPSI.tar', Output);
  CheckOutputForError;
end;

procedure TOpsiPackageDownloader.ExtractOpsiPackage;
begin
  RemoveOldDownloadedOpsiPackageFolder;
  CreateNewOpsiPackageFolderDir;
  ExtractFilesFromOpsiPackage;
  MoveFilesToOpsiPackageFolder;
  ExtractFoldersFromFiles;
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
  PackageManagementShellCommand: string): boolean;
var
  OpsiPackageDownloader: TOpsiPackageDownloader;
begin
  LogDatei.log('Try downloading latest ' + OpsiPackageId + ':', LLnotice);
  Result := True;
  SetCurrentDir(ExtractFilePath(ParamStr(0)));

  OpsiPackageDownloader := TOpsiPackageDownloader.Create(OpsiPackageId,
    DownloadDir, OpsiPackageDownloadCommand, PackageManagementShellCommand);
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
    OpsiPackageDownloader.RemoveDownloadedOpsiPackage;
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
