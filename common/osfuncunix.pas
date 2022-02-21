unit osfuncunix;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  oslog,
  lazfileutils,
  osProcessUX,
  osnetutil;

function resolveUnixSymlink(filepath: string;
  recursive: boolean = True): string;
function GetNetUser(Host: string; var UserName: string; var ErrorInfo: string): boolean;
function GetFQDNUnix: string;


implementation

function resolveUnixSymlink(filepath: string;
  recursive: boolean = True): string;
var
  outpath: string;
  mypath : string;
begin
  Result := filepath;
  filepath := GetForcedPathDelims(filepath);
  if FileExists(filepath, False) then
  begin
    LogDatei.log('resolving symlink: '+filepath,LLinfo);
    mypath := ExtractFileDir(filepath);
    outpath := TryReadAllLinks(filepath);
    if outpath = '' then
    begin
      // was no symbolic link - return input
      Result := filepath;
    end
    else
    begin
      if not FilenameIsAbsolute(outpath) then
        outpath := TrimAndExpandFilename(outpath, mypath);
      if outpath <> filepath then
      begin
        // was symbolic link
        LogDatei.log('resolved as symlink: '+filepath+' to: '+outpath,LLinfo);
        if recursive then
          Result := resolveUnixSymlink(outpath)
        else
          Result := filepath;
      end;
    end;
  end
  else // return filepath also if filepath does not exists
    Result := filepath;
end;

function GetNetUser(Host: string; var UserName: string; var ErrorInfo: string): boolean;
  { for Host = '' Username will become the name of the current user of the process }

begin
  //###LINUX
  Result := True;
  Username := getCommandResult('/bin/bash -c whoami');
end;

function GetFQDNUnix: string;
var
  FQDN: string;
begin
  Result := '';
  LogDatei.log('Try getting FQDN from command line:', LLInfo);
  FQDN := getCommandResult('hostname -f');
  Result := FQDN;
  LogDatei.log('Command line result for FQDN: ' + FQDN, LLInfo);
end;


end.
