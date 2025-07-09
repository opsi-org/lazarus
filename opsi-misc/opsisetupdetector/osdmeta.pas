unit osdmeta;

{$mode Delphi}

interface

uses
  Classes, SysUtils,
  fpjsonrtti,
  fpjson,
  oslog,
  opsiDynamicLibJYT,
  osjson,
  osd_jyt_convert;

type

  //********************************
  // Meta data structure
  //********************************

  TMeta_os = (Windows, linux, macos, os_unknown);

  TMeta_os_arch = (x86, x64, arm64, arm, arch_unknown);

  TMetaSpecification = class(TPersistent)
  private
    Fversion: string;
  published
    property version: string read Fversion write Fversion;
  public
    { public declarations }
    constructor Create;
    destructor Destroy;
  end;

  TMetaProduct = class(TPersistent)
  private
    FProductIconFilePath: string;
    // Path to the icon file inside the opsi product base dir
  published
    property productIconFilePath: string read FProductIconFilePath
      write FProductIconFilePath;
  public
    { public declarations }
    constructor Create;
    destructor Destroy;
  end;

  TMetaInstallerRequirement = class(TPersistent)
  private
    Fos: TMeta_os;  // for wich os fits this installer
    Fos_arch: TMeta_os_arch; // for wich os_arch fits this installer
    FRequiredSpaceMB: cardinal; // MB size of the required space to run this installer
  published
    property os: TMeta_os read Fos write Fos;
    property os_arch: TMeta_os_arch read Fos_arch write Fos_arch;
    property requiredSpaceMB: cardinal read FRequiredSpaceMB write FRequiredSpaceMB;
  public
    { public declarations }
    constructor Create;
    destructor Destroy;
  end;

  TMetaInstaller = class(TPersistent)
  private
  private
    Factive: boolean;   // True= this object is in use
    Finstall_in_background: boolean;  // optional, default = true
    Fpath: string;  // Path to the installer inside the opsi product base dir
    Finstall_dir: string; // target directory where this installer will install to
    Fcheck_processes_from_dirs: TStringList;
    // directories that should be used to check for critical processes
    // usally contains the installdir
    Fprocesses: TStringList;
    // List of processes that should be used to check for critical processes
    FMetaInstallerRequirement: TMetaInstallerRequirement;

    function readProcesses: TStrings;
    procedure writeProcesses(val: TStrings);
    procedure addtoProcesses(val: string);
    function readCheckDirs: TStrings;
    procedure writeCheckDirs(val: TStrings);
    procedure addtoCheckDirs(val: string);
  published
    property active: boolean read Factive write Factive;
    property install_in_background: boolean
      read Finstall_in_background write Finstall_in_background;
    property path: string read Fpath write Fpath;
    property install_dir: string read Finstall_dir write Finstall_dir;
    property check_processes_from_dirs: TStrings read readCheckDirs write writeCheckDirs;
    property processes: TStrings read readProcesses write writeProcesses;
    property requirement: TMetaInstallerRequirement
      read FMetaInstallerRequirement write FMetaInstallerRequirement;
  public
    { public declarations }
    constructor Create;
    destructor Destroy;
  end;


  TopsiMeta = class(TPersistent)
  private
  published
  public
    metaspecification: TMetaspecification;
    productMeta: TMetaProduct;
    InstallerMeta: array[0..2] of TMetaInstaller;


    { public declarations }
    constructor Create;
    destructor Destroy;
    procedure initMeta;
    procedure write_product_metadata_ToPath(path: string);
    procedure write_product_metadata_to_file(myfilename: string);
    function convert_aktmeta_to_jsonstring(include_hidden: boolean): string;
    procedure convert_jsonstring_to_aktmeta(jsonInputString: string);
  end;

procedure aktProdToAktMeta;

var
  aktMeta: TopsiMeta;

implementation

uses
  osdbasedata;

  //********************************
  // Meta data TMetaspecification
  //********************************


constructor TMetaspecification.Create;
begin
  inherited;
  //Fversion := '';
end;

destructor TMetaspecification.Destroy;
begin
  inherited;
end;

//********************************
// Meta data TMetaProduct
//********************************


constructor TMetaProduct.Create;
begin
  inherited;
end;

destructor TMetaProduct.Destroy;
begin
  inherited;
end;

//********************************
// Meta data TMetaInstallerRequirement
//********************************


constructor TMetaInstallerRequirement.Create;
begin
  inherited;
  (*
  Fos := os_unknown;
  Fos_arch := arch_unknown;
  *)
end;

destructor TMetaInstallerRequirement.Destroy;
begin
  inherited;
end;

//********************************
// Meta data TMetaInstaller
//********************************


constructor TMetaInstaller.Create;
begin
  inherited;
  (*
  Factive := False;
  Finstall_in_background := True;
  Fpath := '';
  Finstall_dir := '';
  *)
  Fcheck_processes_from_dirs := TStringList.Create;
  Fprocesses := TStringList.Create;
  FMetaInstallerRequirement := TMetaInstallerRequirement.Create;
end;

destructor TMetaInstaller.Destroy;
begin
  FreeAndNil(Fcheck_processes_from_dirs);
  FreeAndNil(Fprocesses);
  FreeAndNil(FMetaInstallerRequirement);
  inherited;
end;


function TMetaInstaller.readProcesses: TStrings;
begin
  Result := TStrings(Fprocesses);
end;

procedure TMetaInstaller.writeProcesses(val: TStrings);
begin
  Fprocesses.Text := val.Text;
end;

procedure TMetaInstaller.addtoProcesses(val: string);
begin
  Fprocesses.add(val);
end;

function TMetaInstaller.readCheckDirs: TStrings;
begin
  Result := TStrings(Fcheck_processes_from_dirs);
end;

procedure TMetaInstaller.writeCheckDirs(val: TStrings);
begin
  Fcheck_processes_from_dirs.Text := val.Text;
end;

procedure TMetaInstaller.addtoCheckDirs(val: string);
begin
  Fcheck_processes_from_dirs.add(val);
end;




//********************************
// Meta data TopsiMeta
//********************************

procedure TopsiMeta.initMeta;
var
  i: integer;
begin
  aktMeta.metaspecification.Fversion := '';
  aktMeta.productMeta.FProductIconFilePath := '';
  for i := 0 to 2 do
  begin
    aktMeta.InstallerMeta[i].Factive := False;
    aktMeta.InstallerMeta[i].Finstall_in_background := True;
    aktMeta.InstallerMeta[i].Fpath := '';
    aktMeta.InstallerMeta[i].Finstall_dir := '';
    aktMeta.InstallerMeta[i].Fcheck_processes_from_dirs.Clear;
    aktMeta.InstallerMeta[i].Fprocesses.Clear;
    aktMeta.InstallerMeta[i].FMetaInstallerRequirement.Fos := os_unknown;
    aktMeta.InstallerMeta[i].FMetaInstallerRequirement.Fos_arch := arch_unknown;
    aktMeta.InstallerMeta[i].FMetaInstallerRequirement.FRequiredSpaceMB := 0;
  end;
end;



constructor TopsiMeta.Create;
var
  i: integer;
begin
  inherited;
  metaspecification := TMetaspecification.Create;
  productMeta := TMetaProduct.Create;
  for i := 0 to 2 do InstallerMeta[i] := TMetaInstaller.Create;
end;

destructor TopsiMeta.Destroy;
var
  i: integer;
begin
  FreeAndNil(metaspecification);
  FreeAndNil(productMeta);
  for i := 0 to 2 do FreeAndNil(InstallerMeta[i]);
  inherited;
end;

procedure TopsiMeta.write_product_metadata_ToPath(path: string);
begin
  path := IncludeTrailingPathDelimiter(path);
  write_product_metadata_to_file(path + 'opsi-meta-data.toml');
end;

procedure TopsiMeta.write_product_metadata_to_file(myfilename: string);
var
  JSONFinalString: string;
  myStringlist: TStringList;
  TOMLString: string;
  configDir: array[0..MaxPathLen] of char; //Allocate memory
  pfile: TextFile;
  lib_jyt: TLibJYT;
  i: integer;
begin
  try
    if Assigned(logdatei) then
      logdatei.log('Start write metadata file', LLDebug);

    // metadata file name
    myfilename := ExpandFileName(myfilename);
    configdir := ExtractFileDir(myfilename);
    configDir := IncludeTrailingPathDelimiter(configdir);
    //myfilename := configDir + 'opsi-meta-data.toml';
    if Assigned(logdatei) then
      logdatei.log('write metadata file to: ' + myfilename, LLDebug);
    if not DirectoryExists(configDir) then
      if not ForceDirectories(configDir) then
        if Assigned(logdatei) then
          LogDatei.log('failed to create metadata file directory: ' +
            configDir, LLError);

    JSONFinalString := convert_aktmeta_to_jsonstring(False);

    {$IFNDEF DARWIN}
    if Assigned(logdatei) then
      logdatei.log('Convert json metadata to toml', LLDebug);
    try
      lib_jyt := TLibJYT.Create;
      try
        TOMLString := lib_jyt.json2toml(JSONFinalString);
      except
        on E: Exception do
          LogDatei.log('ERROR: exception: ' + E.ClassName + ': ' + E.Message, LLerror);
      end;
    finally
      FreeAndNil(lib_jyt);
    end;
    // in TOML we want the strings in single quotes (no escapes needed)
    TOMLString := StringReplace(TOMLString, '"', '''',
      [rfReplaceAll, rfIgnoreCase]);

    if Assigned(logdatei) then
      logdatei.log('write toml metadata to file', LLDebug);
    AssignFile(pfile, myfilename);
    Rewrite(pfile);
    writeln(pfile, TOMLString);
    CloseFile(pfile);
    {$ELSE DARWIN}
    logdatei.log('write json metadata to file', LLDebug);
    AssignFile(pfile, myfilename + '.json');
    Rewrite(pfile);
    writeln(pfile, JSONFinalString);
    CloseFile(pfile);

    if Assigned(logdatei) then
      logdatei.log('Convert json metadata file to toml', LLDebug);
    convertJsonFileToTomlFile(myfilename + '.json', myfilename);
    DeleteFile(myfilename + '.json');
    {$EndIF DARWIN}
   (*
    finally
      Streamer.Destroy;
    end;
    *)
    if Assigned(logdatei) then
      logdatei.log('Finished write metadata file', LLDebug2);

  except
    on E: Exception do
      if Assigned(logdatei) then
        LogDatei.log('metadata file could not be written. Details: ' +
          E.ClassName + ': ' + E.Message, LLError);
  end;
end;

function aktprodTargetosToMetaOs(targetos: TTargetOS): TMeta_os;
begin
  if targetos = oswin then Result := Windows
  else if targetos = oslin then Result := linux
  else if targetos = osmac then Result := macos
  else if targetos = osUnknown then Result := os_unknown
  else if targetos = osMulti then Result := os_unknown
  else
    Result := os_unknown;
end;

function aktprodArchitectureToMetaOsArch(targetarch: TArchitecture): TMeta_os_arch;
begin
  if targetarch = a32 then Result := x86
  else if targetarch = a64 then Result := x64
  else if targetarch = aarm then Result := arm
  else if targetarch = aarm64 then Result := arm64
  else if targetarch = aUnknown then Result := arch_unknown
  else
    Result := arch_unknown;
end;


procedure aktProdToAktMeta;
var
  i, k: integer;
  strlist: TStringList;
  installdir, setupFileName: string;
begin
  // reinitialize meta data structure
  aktmeta.initMeta;

  strlist := TStringList.Create;
  aktMeta.metaspecification.version := '0.1';
  aktMeta.productMeta.productIconFilePath :=
    aktProduct.productdata.productId + ExtractFileExt(
    aktProduct.productdata.productImageFullFileName);
  for i := 0 to 2 do
  begin
    if aktProduct.SetupFiles[i].active then
    begin
      aktMeta.InstallerMeta[i].Factive := True;
      installdir := aktProduct.SetupFiles[i].installDirectory;
      if (installdir <> '') and (LowerCase(installdir) <> 'unknown') then
        aktMeta.InstallerMeta[i].Finstall_dir := installdir;

      setupFileName := aktProduct.SetupFiles[i].setupFileName;
      if setupFileName <> '' then
        aktMeta.InstallerMeta[i].path :=
          aktProduct.SetupFiles[i].installerSourceDir + '\' + setupFileName;
      // check only for process at windows
      if aktProduct.SetupFiles[i].targetOS = oswin then
      begin
        aktMeta.InstallerMeta[i].addtoCheckDirs(
          aktMeta.InstallerMeta[i].Finstall_dir);
        aktMeta.InstallerMeta[i].FMetaInstallerRequirement.os := Windows;
      end
      else
        aktMeta.InstallerMeta[i].FMetaInstallerRequirement.os :=
          aktprodTargetosToMetaOs(aktProduct.SetupFiles[i].targetOS);
      aktMeta.InstallerMeta[i].FMetaInstallerRequirement.os_arch :=
        aktprodArchitectureToMetaOsArch(aktProduct.SetupFiles[i].architecture);
      aktMeta.InstallerMeta[i].FMetaInstallerRequirement.requiredSpaceMB :=
        aktProduct.SetupFiles[i].requiredSpace;
      // do not install in background for 'with user' - to many reboots
      if osdsettings.runmode in [analyzeCreateWithUser, createTemplateWithUser] then
        aktMeta.InstallerMeta[i].install_in_background := False;
    end;
  end;
  FreeAndNil(strlist);
end;

function TopsiMeta.convert_aktmeta_to_jsonstring(include_hidden: boolean): string;
var
  Streamer: TJSONStreamer;
  JSONString, JSONFinalString, JSONArrayString: string;
  myStringlist: TStringList;
  i: integer;
begin
  Result := '';
  try
    if Assigned(logdatei) then
      logdatei.log('Start convert metadata to json', LLDebug);

    // http://wiki.freepascal.org/Streaming_JSON
    Streamer := TJSONStreamer.Create(nil);
    myStringlist := TStringList.Create;
    try
      // Save strings as JSON array
      Streamer.Options := Streamer.Options + [jsoTStringsAsArray];
      // JSON convert and output
      // init JSONFinalString
      JSONFinalString := '{}';
      JSONString := Streamer.ObjectToJSONString(aktMeta.metaspecification);
      //JSONFinalString := '{"specification":'+JSONString+'}';
      jsonAsObjectAddKeyAndValue(JSONFinalString, 'specification', JSONString,
        JSONFinalString);
      //writeln(pfile, JSONString);
      for i := 0 to 2 do
        if aktMeta.InstallerMeta[i].FActive then
        begin
          JSONString := Streamer.ObjectToJSONString(aktMeta.InstallerMeta[i]);
          if not include_hidden then
          begin
            // do not write install_in_background with default = true to meta data
            if aktMeta.InstallerMeta[i].install_in_background then
              jsonAsObjectDeleteByKey(JSONString, 'install_in_background');
            // do not write active  to meta data
            jsonAsObjectDeleteByKey(JSONString, 'active');
          end;
          myStringlist.Add(JSONString);
        end;
      stringListToJsonArray(myStringlist, JSONArrayString);
      jsonAsObjectAddKeyAndValue(JSONFinalString, 'installers', JSONArrayString,
        JSONFinalString);
      //writeln(pfile, JSONString);
      JSONString := Streamer.ObjectToJSONString(aktMeta.productMeta);
      jsonAsObjectAddKeyAndValue(JSONFinalString, 'product', JSONString,
        JSONFinalString);
      Result := JSONFinalString;
      if Assigned(logdatei) then
        logdatei.log('meta data as JSONString: ' + Result, LLDebug);
    finally
      Streamer.Destroy;
    end;
    if Assigned(logdatei) then
      logdatei.log('Finished convert metadata to json', LLDebug2);

  except
    on E: Exception do
      if Assigned(logdatei) then
        LogDatei.log('convert metadata to json failed. Details: ' +
          E.ClassName + ': ' + E.Message, LLError);
  end;
end;

procedure TopsiMeta.convert_jsonstring_to_aktmeta(jsonInputString: string);
var
  DeStreamer: TJSONDeStreamer;
  JSONString, JSONArrayString: string;
  i: integer;
begin
  try
    if Assigned(logdatei) then
      logdatei.log('Start convert json to metadata', LLDebug);

    // http://wiki.freepascal.org/Streaming_JSON
    DeStreamer := TJSONDeStreamer.Create(nil);
    try
      DeStreamer.Options := [jdoIgnorePropertyErrors, jdoIgnoreNulls];
      // Load JSON data in the object
      if jsonAsObjectGetValueByKey(jsonInputString, 'specification', JSONString) then
        DeStreamer.JSONToObject(JSONString, aktMeta.metaspecification);
      if jsonAsObjectGetValueByKey(jsonInputString, 'product', JSONString) then
        DeStreamer.JSONToObject(JSONString, aktMeta.productMeta);
      if jsonAsObjectGetValueByKey(jsonInputString, 'installers', JSONArrayString) then
      begin
        if jsonIsArray(JSONArrayString) then
          for i := 0 to jsonAsArrayCountElements(JSONArrayString) - 1 do
          begin
            if jsonAsArrayGetElementByIndex(JSONArrayString, i, JSONString) then
              DeStreamer.JSONToObject(JSONString, aktMeta.InstallerMeta[i]);
          end;
      end;

    finally
      DeStreamer.Destroy;
    end;
    if Assigned(logdatei) then
      logdatei.log('Finished convert json to metadata', LLDebug2);

  except
    on E: Exception do
      if Assigned(logdatei) then
        LogDatei.log('convert json to metadata failed. Details: ' +
          E.ClassName + ': ' + E.Message, LLError);
  end;
end;

initialization
  aktMeta := TopsiMeta.Create;
  aktmeta.initMeta;

finalization
  FreeAndNil(aktMeta)

end.
