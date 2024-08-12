unit osdmeta;

{$mode Delphi}

interface

uses
  Classes, SysUtils,
  fpjsonrtti,
  oslog,
  opsiDynamicLibJYT,
  osjson,
  osdbasedata;

type

  //********************************
  // Meta data structure
  //********************************

  TMeta_os = (windows, linux, macos, os_unknown);

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
    FdoNotCheckInstallDirBinaries: boolean;
  published
    property doNotInstallInBackground: boolean
      read FdoNotInstallInBackground write FdoNotInstallInBackground;
    property doNotCheckInstallDirBinaries: boolean
      read FdoNotCheckInstallDirBinaries write FdoNotCheckInstallDirBinaries;
  public
    { public declarations }
    constructor Create;
    destructor Destroy;
  end;

  TMetaInstallerRequirement = class(TPersistent)
  private
    Fos: TMeta_os;
    Fos_arch: TMeta_os_arch;
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
    TprocessArray = TArray<String>;
  private
    Factive: boolean;   // is this object in use
    FdoNotInstallInBackground: boolean;
    FdoNotCheckInstallDirBinaries: boolean;
    Fpath: string;
    Finstall_dir: string;
    Fother_install_dirs: TStringList;
    Fprocesses:TprocessArray;
    FMetaInstallerRequirement: TMetaInstallerRequirement;
    function readProcessArray(i: integer): String;
    procedure writeProcessArray(i: integer; val: String);
  published
    property doNotInstallInBackground: boolean
      read FdoNotInstallInBackground write FdoNotInstallInBackground;
    property doNotCheckInstallDirBinaries: boolean
      read FdoNotCheckInstallDirBinaries write FdoNotCheckInstallDirBinaries;
    property path: string read Fpath write Fpath;
    property install_dir: string read Finstall_dir write Finstall_dir;
    property other_install_dirs: TStringList read Fother_install_dirs
      write Fother_install_dirs;
    property processes[i: integer]: string read readProcessArray write writeProcessArray;
    property MetaInstallerRequirement: TMetaInstallerRequirement
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
  //Fprocesses := TStringList.Create;
  SetLength(Fprocesses,2);
  FMetaInstallerRequirement := TMetaInstallerRequirement.Create;
end;

destructor TMetaInstaller.Destroy;
begin
  FreeAndNil(Fother_install_dirs);
  //FreeAndNil(Fprocesses);
  FreeAndNil(FMetaInstallerRequirement);
  inherited;
end;

function TMetaInstaller.readProcessArray(i: integer): String;
var
  mysize, k : integer;
begin
  mysize := length(Fprocesses);
  //if i+1 < mysize then
  result := '[';
  for k := 0 to mysize -1 do
    result := result +'"'+Fprocesses[k]+'",';
  result := copy(result,0,length(result)-2);
  result := result +']'
end;

procedure TMetaInstaller.writeProcessArray(i: integer; val: String);
begin
  Fprocesses[i] := val;
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
  myStringlist : Tstringlist;
  TOMLString: string;
  configDir: array[0..MaxPathLen] of char; //Allocate memory
  configDirUtf8: utf8string;
  pfile: TextFile;
  lib_jyt: TLibJYT;
  i : integer;
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
    AssignFile(pfile, myfilename);
    Rewrite(pfile);
    // http://wiki.freepascal.org/Streaming_JSON
    Streamer := TJSONStreamer.Create(nil);
    myStringlist := Tstringlist.Create;
    try
      //Streamer.Options := Streamer.Options + [jsoTStringsAsArray];
      // Save strings as JSON array
      // JSON convert and output
      // init JSONFinalString
      JSONFinalString := '{}';
      JSONString := Streamer.ObjectToJSONString(aktMeta.metaspecification);
      //JSONFinalString := '{"specification":'+JSONString+'}';
      jsonAsObjectAddKeyAndValue(JSONFinalString,'specification',JSONString, JSONFinalString);
      //writeln(pfile, JSONString);
      for i := 0 to 2 do
      if aktMeta.InstallerMeta[i].FActive then
        begin
        JSONString := Streamer.ObjectToJSONString(aktMeta.InstallerMeta[i]);
        myStringlist.Add(JSONString);
        end;
      stringListToJsonArray(myStringlist,JSONArrayString);
      jsonAsObjectAddKeyAndValue(JSONFinalString,'installers',JSONArrayString, JSONFinalString);
      //writeln(pfile, JSONString);
      JSONString := Streamer.ObjectToJSONString(aktMeta.productMeta);
      jsonAsObjectAddKeyAndValue(JSONFinalString,'product',JSONString, JSONFinalString);
      //writeln(pfile, JSONString);
      try
        try
          pinput := PChar(JSONFinalString);
          lib_jyt := TLibJYT.Create;
          poutput := lib_jyt.json2toml(pinput);
          TOMLString := poutput;
          lib_jyt.free_result(poutput);
        except
          on E: Exception do
            LogDatei.log('ERROR: exception: ' + E.ClassName + ': ' + E.Message, LLerror);
        end;
      finally
        FreeAndNil(lib_jyt);
      end;
      writeln(pfile, TOMLString);
      CloseFile(pfile);

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

function aktprodTargetosToMetaOs(targetos : TTargetOS) : TMeta_os;
begin
  if targetos = oswin then result := windows
  else if targetos = oslin then result := linux
  else if targetos = osmac then result := macos
  else if targetos = osUnknown then result := os_unknown
  else if targetos = osMulti then result := os_unknown
  else result := os_unknown;
end;

function aktprodArchitectureToMetaOsArch(targetarch : TArchitecture) : TMeta_os_arch;
begin
  if targetarch = a32 then result := x86
  else if targetarch = a64 then result := x64
  else if targetarch = aarm then result := arm
  else if targetarch = aarm64 then result := arm64
  else if targetarch = aUnknown then result := arch_unknown
  else result := arch_unknown;
end;


procedure aktProdToAktMeta;
var
  i,k : integer;
  strlist : Tstringlist;
begin
  strlist := TStringlist.Create;
  aktMeta.metaspecification.version:='0.1';
  for i:= 0 to 2 do
    begin
      if aktProduct.SetupFiles[i].active then
        begin
          aktMeta.InstallerMeta[i].Factive:=true;
          aktMeta.InstallerMeta[i].Finstall_dir:=aktProduct.SetupFiles[i].installDirectory;
          aktMeta.InstallerMeta[i].path := 'files'+inttostr(i+1)+'\'+
            aktProduct.SetupFiles[i].setupFileName;
          // process test
          strlist.Add('7zip.exe');
          strlist.Add('%ProgramFiles64Dir%\7-Zip\7zFM.exe');
          //aktMeta.InstallerMeta[i].processes
          for k := 0 to strlist.Count -1 do
            aktMeta.InstallerMeta[i].processes[k] := strlist[k];

          aktMeta.InstallerMeta[i].FMetaInstallerRequirement.os:=
            aktprodTargetosToMetaOs(aktProduct.SetupFiles[i].targetOS);
          aktMeta.InstallerMeta[i].FMetaInstallerRequirement.os_arch:=
            aktprodArchitectureToMetaOsArch(aktProduct.SetupFiles[i].architecture);

        end;
    end;
  FreeAndNil(strlist);
end;


begin
  aktMeta := TopsiMeta.Create;
end.
