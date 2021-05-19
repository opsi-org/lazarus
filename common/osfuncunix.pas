unit osfuncunix;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  lazfileutils;

function resolveUnixSymlink(const filepath : string; recursive : boolean = true) : string;

implementation

function resolveUnixSymlink(const filepath : string; recursive : boolean = true) : string;
var
  outpath : string;
begin
  result := filepath;
  outpath := TryReadAllLinks(filepath);
  if outpath = '' then
  begin
    // was no symbolic link - return input
    result := filepath;
  end
  else if outpath <> filepath then
  begin
    // was symbolic link
    if recursive then
      result := resolveUnixSymlink(outpath)
    else  result := filepath;
  end;
end;

end.

