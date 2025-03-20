unit opsiDynamicLibJYT;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils,
  fileutil,
  //oslog,
  OpsiDynamicLibraries;

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
    destructor Destroy; override;
    function json2yaml(var src: string): string;
    function json2toml(var src: string): string;
    function yaml2json(var src: string): string;
    function yaml2toml(var src: string): string;
    function toml2json(var src: string): string;
    function toml2yaml(var src: string): string;
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
  {$ENDIF MSWINDOWS}

  {$IFDEF LINUX}
  libname: string = 'libjyt.so';
  {$ENDIF LINUX}

  {$IFDEF DARWIN}
  libname: string = 'libjyt.dylib';
  {$ENDIF DARWIN}
var
  libpath: string = '';
  fullpath: string;
begin
  {$IFDEF MSWINDOWS}
  libpath := '';
  {$ENDIF MSWINDOWS}

  {$IFDEF LINUX}
  libpath:= '';
  {$ENDIF LINUX}

  {$IFDEF DARWIN}
  libpath:= ProgramDirectory+'../Frameworks';
  {$ENDIF DARWIN}

  if libpath = '' then
    fullpath := ProgramDirectory + libname
  else
    fullpath := libpath + Pathdelim + libname;
  //if libpath = '' then
  //  libpath := ProgramDirectory;
  (*
  LogDatei.log('Loading lib: ' + fullpath, LLinfo);
  if FileExists(fullpath) then
    LogDatei.log('Lib exist: ' + fullpath, LLinfo)
  else
    LogDatei.log('Lib not exist: ' + fullpath, LLerror);
   *)
  try
     inherited Create(libname, libpath);
     Load;
     GetFunctionAddresses;
  finally
  end;
  if not Loaded then
    raise Exception.Create(ErrorMessage);
end;


destructor TLibJYT.Destroy;
begin
  if Loaded then inherited Destroy;
end;


procedure TLibJYT.GetFunctionAddresses;
var
  prefix : string ='';
begin
  if Loaded then
  begin
    FCSLoadLib.Enter;
    try
      {$IFDEF DARWIN}
      prefix := '_';
      {$ENDIF DARWIN}
      to_json := GetProcAddress(FLibHandle, prefix + 'to_json');
      to_yaml := GetProcAddress(FLibHandle, prefix + 'to_yaml');
      to_toml := GetProcAddress(FLibHandle, prefix + 'to_toml');
      cstring_free := GetProcAddress(FLibHandle, prefix + 'cstring_free');
    finally
      FCSLoadLib.Leave;
    end;
  end
end;


function TLibJYT.json2yaml;
var
  presult: pchar;
begin
  if not Loaded then raise Exception.Create(FErrorMessage);
  if not Assigned(to_yaml) then raise Exception.Create('ERROR:: could not get function address for "to_yaml"');
  try
     presult := to_yaml(0, pchar(src));  // intype 0 = json
     result := string(presult);
     cstring_free(presult);
  except
     raise Exception.Create('ERROR:: could not convert as JSON to YAML');
  end;
end;


function TLibJYT.json2toml;
var
  presult: pchar;
begin
  if not Loaded then raise Exception.Create(FErrorMessage);
  if not Assigned(to_toml) then raise Exception.Create('ERROR:: could not get function address for "to_toml"');
  try
     presult := to_toml(0, pchar(src));  // intype 0 = json
     result := string(presult);
     cstring_free(presult);
  except
     raise Exception.Create('ERROR:: could not convert as JSON to TOML');
  end;
end;


function TLibJYT.yaml2json;
var
  presult: pchar;
begin
  if not Loaded then raise Exception.Create(FErrorMessage);
  if not Assigned(to_json) then raise Exception.Create('ERROR:: could not get function address for "to_json"');
  try
     presult := to_json(1, pchar(src));  // intype 1 = yaml
     result := string(presult);
     cstring_free(presult);
  except
     raise Exception.Create('ERROR:: could not convert as YAML to JSON');
  end;
end;


function TLibJYT.yaml2toml;
var
  presult: pchar;
begin
  if not Loaded then raise Exception.Create(FErrorMessage);
  if not Assigned(to_toml) then raise Exception.Create('ERROR:: could not get function address for "to_toml"');
  try
     presult := to_toml(1, pchar(src));  // intype 1 = yaml
     result := string(presult);
     cstring_free(presult);
  except
     raise Exception.Create('ERROR:: could not convert as YAML to TOML');
  end;
end;


function TLibJYT.toml2json;
var
  presult: pchar;
begin
  if not Loaded then raise Exception.Create(FErrorMessage);
  if not Assigned(to_json) then raise Exception.Create('ERROR:: could not get function address for "to_json"');
  try
     presult := to_json(2, pchar(src));  // intype 2 = toml
     result := string(presult);
     cstring_free(presult);
  except
     raise Exception.Create('ERROR:: could not convert as TOML to JSON');
  end;
end;


function TLibJYT.toml2yaml;
var
  presult: pchar;
begin
  if not Loaded then raise Exception.Create(FErrorMessage);
  if not Assigned(to_yaml) then raise Exception.Create('ERROR:: could not get function address for "to_yaml"');
  try
     presult := to_yaml(2, pchar(src));  // intype 2 = toml
     result := string(presult);
     cstring_free(presult);
  except
      raise Exception.Create('ERROR:: could not convert as TOML to YAML');
  end;
end;


end.

