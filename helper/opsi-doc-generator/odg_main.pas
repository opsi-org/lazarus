unit odg_main;

{$mode objfpc}{$H+}


interface


uses
  odg_os_deffunc,
  oslog,
  odg_asciidoc,
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
  writeDocToList;
end;

initialization
  sourcelist := TStringlist.create;
  targetlist := TStringlist.create;
  LogDatei := TLogInfo.Create;
  logdatei.CreateTheLogfile('opsi-doc-gen.log');

finalization
  sourcelist.Free;
  targetlist.Free;

end.

