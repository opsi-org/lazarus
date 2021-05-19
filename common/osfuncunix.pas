unit osfuncunix;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  oslog,
  lazfileutils;

function resolveUnixSymlink(filepath: string;
  recursive: boolean = True): string;

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

end.
