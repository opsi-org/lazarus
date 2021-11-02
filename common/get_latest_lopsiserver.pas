unit get_latest_lopsiserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, osRunCommandElevated, osLog, Process, osDistributionInfo;

const
  downloadDir = 'download.uib.de/opsi4.2/testing/packages/linux/localboot/';

function extractFile(fileName: string): boolean;
function getLOpsiServer(LOpsiServerCommand: TRunCommandElevated;
  distroName: string): boolean;
procedure LOSDefineDirClientData(DirClientData: string; two_los_to_test: boolean;
  version_los_downloaded: string; version_los_default: string;
  name_los_downloaded: string; name_los_default: string;
  QuickInstallCommand: TRunCommandElevated; distroName: string);

implementation

function extractFile(fileName: string): boolean;
var
  Output: string;
begin
  Result := True;
  // cpio only works with bash not with sh !!!
  if RunCommand('/bin/bash', ['-c', 'cpio --extract < ' + fileName], Output) then
  begin
    // if extraction works, Output should be something like '89 Blöcke'
    // if Output = '' then we don't know whether extraction worked -> no log entry
    if not (Output = '') then
    {begin
      LogDatei.log(fileName + ' extraction failed', LLNotice);
      Result := False;
    end
    else}
      LogDatei.log(fileName + ' successfully extracted', LLInfo);
  end
  else
  begin
    LogDatei.log(fileName + ' extraction failed', LLNotice);
    Result := False;
  end;
end;

function getLOpsiServer(LOpsiServerCommand: TRunCommandElevated;
  distroName: string): boolean;
var
  shellCommand, Output, los_version, downloaded_los_folder: string;
  los_search: TSearchRec;
begin
  LogDatei.log('Try downloading latest l-opsi-server:', LLInfo);
  Result := True;

  // installing required packages:
  shellCommand := GetPackageManagementShellCommand(distroName);
  LOpsiServerCommand.Run(shellCommand + 'update', Output);
  LOpsiServerCommand.Run(shellCommand + 'install wget', Output);
  LOpsiServerCommand.Run(shellCommand + 'install cpio', Output);
  LOpsiServerCommand.Run(shellCommand + 'install gzip', Output);

  SetCurrentDir(ExtractFilePath(ParamStr(0)));

  // download
  LOpsiServerCommand.Run('wget -A l-opsi-server_*.opsi -r -l 1 https://' +
    downloadDir + ' -P ../', Output);

  // read in version of downloaded l-opsi-server to write it in the quickinstall log
  if FindFirst('../' + downloadDir + 'l-opsi-server_*.opsi', faAnyFile and
    faDirectory, los_search) = 0 then
  begin
    los_version := los_search.Name;
    Delete(los_version, 1, Pos('_', los_version));
    Delete(los_version, Pos('.opsi', los_version),
      los_version.Length - Pos('.opsi', los_version) + 1);
  end
  else
    Result := False;

  // create l-opsi-server directory for the downloaded version
  if FindFirst('../downloaded_l-opsi-server_*', faAnyFile and
    faDirectory, los_search) = 0 then
    LOpsiServerCommand.Run('rm -rf ../downloaded_l-opsi-server_*', Output);

  downloaded_los_folder := 'downloaded_l-opsi-server_' + los_version;
  LOpsiServerCommand.Run(
    'mkdir ../' + downloaded_los_folder + ' ../' + downloaded_los_folder +
    '/CLIENT_DATA ../' + downloaded_los_folder + '/OPSI',
    Output);

  // go into downloaded directory and move l-opsi-server_*.opsi to the current directory
  LOpsiServerCommand.Run('mv ../' + downloadDir + 'l-opsi-server_*.opsi ./', Output);
  // then the downloaded directory is redundant
  LOpsiServerCommand.Run('rm -rf ../download.uib.de', Output);

  // extract CLIENT_DATA.cpio and OPSI.cpio from l-opsi-server_*.opsi and put them in the right directories:
  if not extractFile('l-opsi-server_*.opsi') then
    Result := False;
  LOpsiServerCommand.Run('gunzip CLIENT_DATA.cpio.gz OPSI.cpio.gz', Output);
  LOpsiServerCommand.Run('mv CLIENT_DATA.cpio ../' + downloaded_los_folder +
    '/CLIENT_DATA/',
    Output);
  if Pos('Error', Output) > 0 then
    Result := False;
  LOpsiServerCommand.Run('mv OPSI.cpio ../' + downloaded_los_folder + '/OPSI/', Output);
  if Pos('Error', Output) > 0 then
    Result := False;

  // go into the respective directories and extract the files from CLIENT_DATA.cpio and OPSI.cpio
  // cpio can only extract into the current directory
  SetCurrentDir('../' + downloaded_los_folder + '/CLIENT_DATA/');
  if not extractFile('CLIENT_DATA.cpio') then
    Result := False;
  SetCurrentDir('../OPSI/');
  if not extractFile('OPSI.cpio') then
    Result := False;

  // tidy up
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  LOpsiServerCommand.Run('rm l-opsi-server_*.opsi', Output);
  if Pos('Error', Output) > 0 then
    Result := False;
  LOpsiServerCommand.Run(
    'rm ../' + downloaded_los_folder + '/CLIENT_DATA/CLIENT_DATA.cpio ../' +
    downloaded_los_folder + '/OPSI/OPSI.cpio',
    Output);
  if Pos('Error', Output) > 0 then
    Result := False;
  //if not DirectoryExists('../l-opsi-server_downloaded') then  Result := False;

  if Result then
    LogDatei.log('Latest l-opsi-server successfully downloaded (version ' +
      los_version + ')', LLInfo)
  else
    LogDatei.log('Downloading latest l-opsi-server failed. Using default l-opsi-server:',
      LLnotice);
  FindClose(los_search);

  // try l-opsi-server from master that is not on download.uib
  //Result := False;
end;

procedure LOSDefineDirClientData(DirClientData: string; two_los_to_test: boolean;
  version_los_downloaded: string; version_los_default: string;
  name_los_downloaded: string; name_los_default: string;
  QuickInstallCommand: TRunCommandElevated; distroName: string);
var
  los_default_search, los_downloaded_search: TSearchRec;
begin
  DirClientData := ExtractFilePath(ParamStr(0));
  Delete(DirClientData, Length(DirClientData), 1);
  //DirClientData := ExtractFilePath(DirClientData) + 'l-opsi-server';
  DirClientData := ExtractFilePath(DirClientData);
  if not two_los_to_test then
  begin
    // if the latest l-opsi-server version failed to install, switch between
    // name_los_default and name_los_downloaded to get the dir of the older version
    if version_los_downloaded > version_los_default then
      DirClientData += name_los_default
    else
      DirClientData += name_los_downloaded;
  end
  else
  begin
    // try downloading latest l-opsi-server and set DirClientData
    //writeln(rsWait);
    if getLOpsiServer(QuickInstallCommand, distroName) then
    begin
      // extract and compare version numbers of default and downloaded los
      if (FindFirst('../l-opsi-server_4.*', faAnyFile and faDirectory,
        los_default_search) = 0) and
        (FindFirst('../downloaded_l-opsi-server_4.*', faAnyFile and
        faDirectory, los_downloaded_search) = 0) then
      begin
        name_los_default := los_default_search.Name;
        name_los_downloaded := los_downloaded_search.Name;
        // extract version numbers
        version_los_default := los_default_search.Name;
        Delete(version_los_default, 1, Pos('_', version_los_default));
        version_los_downloaded := los_downloaded_search.Name;
        Delete(version_los_downloaded, 1, Pos('_', version_los_downloaded));
        Delete(version_los_downloaded, 1, Pos('_', version_los_downloaded));
        // compare and use latest l-opsi-server version
        if version_los_downloaded > version_los_default then
          DirClientData += name_los_downloaded
        else
        begin
          DirClientData += name_los_default;
          if version_los_downloaded = version_los_default then
            two_los_to_test := False;
        end;
      end
      else
        two_los_to_test := False;
    end
    else
      DirClientData += name_los_default;
  end;
  DirClientData += '/CLIENT_DATA/';
end;

end.
