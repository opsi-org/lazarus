unit opsiDynamicLibJYT;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, OpsiDynamicLibraries;

type

  TFuncToJson = function(intype:integer; input:PChar):PChar; cdecl;
  TFuncToYaml = function(intype:integer; input:PChar):PChar; cdecl;
  TFuncToToml = function(intype:integer; input:PChar):PChar; cdecl;
  TProcFree   = procedure(mystr:PChar); cdecl;

  TLibJYT = class(TOpsiDynamicLibrary)
  private

    //library function pointers
    to_json: TFuncToJson;
    to_yaml: TFuncToYaml;
    to_toml: TFuncToToml;
    cstring_free: TProcFree;

    procedure GetFunctionAddresses;

  public
    constructor Create;
    function json2yaml(psrc: PChar): PChar;
    function json2toml(psrc: PChar): PChar;
    function yaml2json(psrc: PChar): PChar;
    function yaml2toml(psrc: PChar): PChar;
    function toml2json(psrc: PChar): PChar;
    function toml2yaml(psrc: PChar): PChar;
    procedure free_result(mystr:PChar);
  end;


implementation


constructor TLibJYT.Create;
const
  {$IFDEF MSWINDOWS}
  {$IFDEF WIN32}
  libname: string = 'jyt_32.dll';
  {$ENDIF WIN32}
  {$IFDEF WIN64}
  libname: string = 'jyt_64.dll';
  {$ENDIF WIN64}
  libpath: string = '..\..\external_libraries\lib';
  {$ELSE}
    // libname and libpath to be set
  {$ENDIF MSWINDOWS}
begin
  inherited Create(libname, libpath);
  Load;
  GetFunctionAddresses;
end;

procedure TLibJYT.GetFunctionAddresses;
begin
  if Loaded then
  begin
    FCSLoadLib.Enter;
    try
      to_json := GetProcAddress(FLibHandle, 'to_json');
      to_yaml := GetProcAddress(FLibHandle, 'to_yaml');
      to_toml := GetProcAddress(FLibHandle, 'to_toml');
      cstring_free := GetProcAddress(FLibHandle, 'cstring_free');
    finally
      FCSLoadLib.Leave;
    end;
  end
end;

function TLibJYT.json2yaml;
begin
  if Assigned(to_yaml) then
    Result := to_yaml(0, psrc) // intype 0 = json
  else
    if Loaded then
       Result := 'ERROR: could not get function address for "to_yaml"'
    else
       Result := pchar('ERROR:' + FErrorMessage);
end;

function TLibJYT.json2toml;
begin
  if Assigned(to_toml) then
    Result := to_toml(0, psrc) // intype 0 = json
  else
    if Loaded then
      Result := 'ERROR: could not get function address for "to_toml"'
    else
      Result := pchar('ERROR:' + FErrorMessage);
end;

function TLibJYT.yaml2json;
begin
  if Assigned(to_json) then
    Result := to_json(1, psrc) // intype 1 = yaml
  else
    if Loaded then
      Result := 'ERROR: could not get function address for "to_json"'
    else
      Result := pchar('ERROR:' + FErrorMessage);
end;

function TLibJYT.yaml2toml;
begin
  if Assigned(to_toml) then
    Result := to_toml(1, psrc) // intype 1 = yaml
  else
    if Loaded then
      Result := 'ERROR: could not get function address for "to_toml"'
    else
      Result := pchar('ERROR:' + FErrorMessage);
end;

function TLibJYT.toml2json;
begin
  if Assigned(to_json) then
    Result := to_json(2, psrc) // intype 2 = toml
  else
    if Loaded then
      Result := 'ERROR: could not get function address for "to_json"'
    else
      Result := pchar(FErrorMessage);
end;

function TLibJYT.toml2yaml;
begin
  if Assigned(to_yaml) then
    Result := to_yaml(2, psrc) // intype 2 = toml
  else
    if Loaded then
      Result := 'ERROR: could not get function address for "to_yaml"'
    else
      Result := pchar('ERROR:' + FErrorMessage);
end;

procedure TLibJYT.free_result(mystr:PChar);
begin
  if Assigned(cstring_free) then
    cstring_free(mystr);
end;

end.

