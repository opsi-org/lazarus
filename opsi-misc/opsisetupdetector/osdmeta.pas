unit osdmeta;

{$mode Delphi}

interface

uses
  Classes, SysUtils,
  fpjsonrtti,
  oslog,
  opsiDynamicLibJYT,
  osjson,
  osdbasedata,
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
    FdoNotInstallInBackground: boolean;
    // True= do not install in background situation
    FdoNotCheckInstallDirBinaries: boolean;
    //True= do not use install directories to check for critical processes
    FProductIconFilePath: string;
    // Path to the icon file inside the opsi product base dir
  published
    property doNotInstallInBackground: boolean
      read FdoNotInstallInBackground write FdoNotInstallInBackground;
    property doNotCheckInstallDirBinaries: boolean
      read FdoNotCheckInstallDirBinaries write FdoNotCheckInstallDirBinaries;
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
  published
    property os: TMeta_os read Fos write Fos;
    property os_arch: TMeta_os_arch read Fos_arch write Fos_arch;
  public
    { public declarations }
    constructor Create;
    destructor Destroy;
  end;

  TMetaInstaller = class(TPersistent)
  private
  type
    //TprocessArray = TArray<string>;
  private
    Factive: boolean;   // True= this object is in use
    FdoNotInstallInBackground: boolean; // True= do not install in background situation
    FdoNotCheckInstallDirBinaries: boolean;
    //True= do not use install directories to check for critical processes
    Fpath: string;  // Path to the installer inside the opsi product base dir
    Finstall_dir: string; // target directory where this installer will install to
    Fother_install_dirs: TStringList;
    // other directories that should be used to check for critical processes
    Fprocesses: TStringList;
    // List of processes that should be used to check for critical processes
    FMetaInstallerRequirement: TMetaInstallerRequirement;
    FRequiredSpaceMB: cardinal; // MB size of the required space to run this installer

    function readProcesses: TStrings;
    procedure writeProcesses(val: TStrings);
    procedure addtoProcesses(val: string);
    function readOtherDirs: TStrings;
    procedure writeOtherDirs(val: TStrings);
    procedure addtoOtherDirs(val: string);
  published
    property doNotInstallInBackground: boolean
      read FdoNotInstallInBackground write FdoNotInstallInBackground;
    property doNotCheckInstallDirBinaries: boolean
      read FdoNotCheckInstallDirBinaries write FdoNotCheckInstallDirBinaries;
    property path: string read Fpath write Fpath;
    property install_dir: string read Finstall_dir write Finstall_dir;
    property other_install_dirs: TStrings read readOtherDirs write writeOtherDirs;
    property processes: TStrings read readProcesses write writeProcesses;
    property InstallerRequirement: TMetaInstallerRequirement
      read FMetaInstallerRequirement write FMetaInstallerRequirement;
    property requiredSpaceMB: cardinal read FRequiredSpaceMB write FRequiredSpaceMB;
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
    procedure write_product_metadata_ToPath(path: string);
    procedure write_product_metadata_to_file(myfilename: string);
  end;

procedure aktProdToAktMeta;

var
  aktMeta: TopsiMeta;

implementation

//********************************
// Meta data TMetaspecification
//********************************


constructor TMetaspecification.Create;
begin
  inherited;
  Fversion := '';
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
  FdoNotInstallInBackground := False;
  FdoNotCheckInstallDirBinaries := False;
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
  Fos := os_unknown;
  Fos_arch := arch_unknown;
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
  Factive := False;
  FdoNotInstallInBackground := False;
  FdoNotCheckInstallDirBinaries := False;
  Fpath := '';
  Finstall_dir := '';
  Fother_install_dirs := TStringList.Create;
  Fprocesses := TStringList.Create;
  FMetaInstallerRequirement := TMetaInstallerRequirement.Create;
end;

destructor TMetaInstaller.Destroy;
begin
  FreeAndNil(Fother_install_dirs);
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

function TMetaInstaller.readOtherDirs: TStrings;
begin
  Result := TStrings(Fother_install_dirs);
end;

procedure TMetaInstaller.writeOtherDirs(val: TStrings);
begin
  Fother_install_dirs.Text := val.Text;
end;

procedure TMetaInstaller.addtoOtherDirs(val: string);
begin
  Fother_install_dirs.add(val);
end;




//********************************
// Meta data TopsiMeta
//********************************


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
  pinput: PChar;
  poutput: PChar;
  Streamer: TJSONStreamer;
  JSONString, JSONFinalString, JSONArrayString: string;
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

    if Assigned(logdatei) then
      logdatei.log('Convert metadata to json', LLDebug);
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
          myStringlist.Add(JSONString);
        end;
      stringListToJsonArray(myStringlist, JSONArrayString);
      jsonAsObjectAddKeyAndValue(JSONFinalString, 'installers', JSONArrayString,
        JSONFinalString);
      //writeln(pfile, JSONString);
      JSONString := Streamer.ObjectToJSONString(aktMeta.productMeta);
      jsonAsObjectAddKeyAndValue(JSONFinalString, 'product', JSONString,
        JSONFinalString);
      //writeln(pfile, JSONString);
      {$IFNDEF DARWIN}
      if Assigned(logdatei) then
        logdatei.log('Convert json metadata to toml', LLDebug);
      try
        lib_jyt := TLibJYT.Create;
        try
          TOMLString := lib_jyt.json2toml(JSONFinalString);
          (*
          pinput := PChar(JSONFinalString);
          lib_jyt := TLibJYT.Create;
          poutput := lib_jyt.json2toml(pinput);
          TOMLString := poutput;
          lib_jyt.free_result(poutput);
          *)
        except
          on E: Exception do
            LogDatei.log('ERROR: exception: ' + E.ClassName + ': ' + E.Message, LLerror);
        end;
      finally
        FreeAndNil(lib_jyt);
      end;
      if Assigned(logdatei) then
        logdatei.log('write toml metadata to file', LLDebug);
      AssignFile(pfile, myfilename);
      Rewrite(pfile);
      writeln(pfile, TOMLString);
      CloseFile(pfile);
      (*
      logdatei.log('write json metadata to file', LLDebug);
      AssignFile(pfile, myfilename + '.json');
      Rewrite(pfile);
      writeln(pfile, JSONFinalString);
      CloseFile(pfile);
      *)
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

    finally
      Streamer.Destroy;
    end;
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
begin
  strlist := TStringList.Create;
  aktMeta.metaspecification.version := '0.1';
  aktMeta.productMeta.doNotCheckInstallDirBinaries := False;
  aktMeta.productMeta.doNotInstallInBackground := False;
  aktMeta.productMeta.productIconFilePath :=
    aktProduct.productdata.productId + ExtractFileExt(
    aktProduct.productdata.productImageFullFileName);
  for i := 0 to 2 do
  begin
    if aktProduct.SetupFiles[i].active then
    begin
      aktMeta.InstallerMeta[i].Factive := True;
      aktMeta.InstallerMeta[i].Finstall_dir :=
        aktProduct.SetupFiles[i].installDirectory;
      aktMeta.InstallerMeta[i].path :=
        aktProduct.SetupFiles[i].installerSourceDir + '\' +
        aktProduct.SetupFiles[i].setupFileName;

      (*
      // process test
      strlist.Add('7zip.exe');
      strlist.Add('%ProgramFiles64Dir%\7-Zip\7zFM.exe');
      //aktMeta.InstallerMeta[i].processes
      for k := 0 to strlist.Count - 1 do
        aktMeta.InstallerMeta[i].addtoProcesses(strlist[k]);
        *)


      aktMeta.InstallerMeta[i].FMetaInstallerRequirement.os :=
        aktprodTargetosToMetaOs(aktProduct.SetupFiles[i].targetOS);
      aktMeta.InstallerMeta[i].FMetaInstallerRequirement.os_arch :=
        aktprodArchitectureToMetaOsArch(aktProduct.SetupFiles[i].architecture);
      aktMeta.InstallerMeta[i].FRequiredSpaceMB := aktProduct.SetupFiles[i].requiredSpace;
    end;
  end;
  FreeAndNil(strlist);
end;


begin
  aktMeta := TopsiMeta.Create;
end.
