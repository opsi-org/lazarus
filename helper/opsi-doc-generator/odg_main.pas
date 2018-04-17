unit odg_main;

{$mode objfpc}{$H+}


interface


uses
  odg_os_deffunc,
  Classes,
  SysUtils;

function convertOslibToAsciidoc(filename : string): boolean;

var
    sourcelist : TStringlist;
  targetlist : TStringlist;


implementation

function convertOslibToAsciidoc(filename : string): boolean;
begin
  result := parseInput_opsiscriptlibrary(filename);

end;

initialization
  sourcelist := TStringlist.create;
  targetlist := TStringlist.create;

finalization
  sourcelist.Free;
  targetlist.Free;

end.

