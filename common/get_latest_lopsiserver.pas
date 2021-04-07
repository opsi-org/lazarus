unit get_latest_lopsiserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, osRunCommandElevated, osLog, Process, osDistributionInfo;

const
  downloadDir = 'download.uib.de/opsi4.2/stable/packages/linux/localboot/';

function extractFile(fileName: string): boolean;
function getLOpsiServer(LOpsiServerCommand: TRunCommandElevated;
  distroName: string): boolean;

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
    if Output = '' then
    begin
      LogDatei.log(fileName + ' extraction failed', LLNotice);
      Result := False;
    end
    else
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
  shellCommand: string;
begin
  LogDatei.log('Try downloading latest l-opsi-server:', LLInfo);
  Result := True;

  // installing required packages:
  shellCommand := GetPackageManagementShellCommand(distroName);
  LOpsiServerCommand.Run(shellCommand + 'update');
  LOpsiServerCommand.Run(shellCommand + 'install wget');
  LOpsiServerCommand.Run(shellCommand + 'install cpio');
  LOpsiServerCommand.Run(shellCommand + 'install gzip');

  // download
  LOpsiServerCommand.Run('wget -A l-opsi-server_*.opsi -r -l 1 https://' +
    downloadDir + ' -P ../');

  // create l-opsi-server directory for the downloaded version
  if DirectoryExists('../l-opsi-server_downloaded') then
    LOpsiServerCommand.Run('rm -rf ../l-opsi-server_downloaded');
  LOpsiServerCommand.Run(
    'mkdir ../l-opsi-server_downloaded ../l-opsi-server_downloaded/CLIENT_DATA ../l-opsi-server_downloaded/OPSI');

  // go into downloaded directory and move l-opsi-server_*.opsi to the current directory
  LOpsiServerCommand.Run('mv ../' + downloadDir + 'l-opsi-server_*.opsi ./');
  // then the downloaded directory is redundant
  LOpsiServerCommand.Run('rm -rf ../download.uib.de');

  // extract CLIENT_DATA.cpio and OPSI.cpio from l-opsi-server_*.opsi and put them in the right directories:
  if not extractFile('l-opsi-server_*.opsi') then
    Result := False;
  LOpsiServerCommand.Run('gunzip CLIENT_DATA.cpio.gz OPSI.cpio.gz');
  LOpsiServerCommand.Run('mv CLIENT_DATA.cpio ../l-opsi-server_downloaded/CLIENT_DATA/');
  LOpsiServerCommand.Run('mv OPSI.cpio ../l-opsi-server_downloaded/OPSI/');

  // go into the respective directories and extract the files from CLIENT_DATA.cpio and OPSI.cpio
  // cpio can only extract into the current directory
  SetCurrentDir('../l-opsi-server_downloaded/CLIENT_DATA/');
  if not extractFile('CLIENT_DATA.cpio') then
    Result := False;
  SetCurrentDir('../OPSI/');
  if not extractFile('OPSI.cpio') then
    Result := False;

  // tidy up
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  LOpsiServerCommand.Run('rm l-opsi-server_*.opsi');
  LOpsiServerCommand.Run(
    'rm ../l-opsi-server_downloaded/CLIENT_DATA/CLIENT_DATA.cpio ../l-opsi-server_downloaded/OPSI/OPSI.cpio');
end;

end.
