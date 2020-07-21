unit osdbasedata;

{$mode delphi}

interface

uses
  Classes, SysUtils, LCLProc, LResources, TypInfo, Forms, Controls, Graphics,
  Dialogs, StdCtrls,
  {$IFDEF WINDOWS}
  osdhelper,
  shlobj,
  winpeimagereader,
  {$ENDIF WINDOWS}
  LazFileUtils,
  lazutf8,
  fileinfo,
  fpjsonrtti,
  oslog,
  RTTICtrls,
  lcltranslator;

type

  TRunMode = (analyzeOnly, singleAnalyzeCreate, twoAnalyzeCreate_1,
    twoAnalyzeCreate_2, createTemplate, gmUnknown);

  TArchitecture = (a32, a64, aUnknown);

  TArchitectureMode = (am32only_fix, am64only_fix, amBoth_fix, amSystemSpecific_fix,
    amSelectable);

  // marker for add installers
  TKnownInstaller = (stSFXcab, stBoxStub, stAdvancedMSI, stInstallShield,
    stInstallShieldMSI,
    stMsi, stNsis, st7zip, st7zipsfx, stInstallAware, stMSGenericInstaller,
    stWixToolset, stBitrock, stSelfExtractingInstaller, stInno,
    stUnknown);


  TdetectInstaller = function(parent: TClass; markerlist: TStrings): boolean;


  TInstallerData = class
  private
  public

    installerId: TKnownInstaller;
    Name: string;
    description: string;
    patterns: TStringList;
    infopatterns: TStringList;
    silentsetup: string;
    unattendedsetup: string;
    silentuninstall: string;
    unattendeduninstall: string;
    uninstall_waitforprocess: string;
    install_waitforprocess: string;
    uninstallProg: string;
    comment: string;
    Link: string;
    uib_exitcode_function: string;
    detected: TdetectInstaller;
    { public declarations }
    constructor Create;
    destructor Destroy;

  end;

  TInstallers = array of TInstallerData;

  TSetupFile = class(TPersistent)
  private
    FID: integer; // 1 = first setup file, 2 = second setup file
    FsetupFileNamePath: string;
    FsetupFileName: string;
    FsetupFullFileName: string;
    FsetupFileSize: cardinal;     // MB
    Farchitecture: TArchitecture;
    FmsiId: string;
    FmstAllowed: boolean;
    FMstFullFileName: string;
    FmstFileNamePath: string;
    FmstFileName: string;
    FmsiFullFileName: string;
    FinstallerId: TKnownInstaller;
    FrequiredSpace: cardinal;      // MB
    FinstallDirectory: string;
    Fmarkerlist: TStrings;
    Finfolist: TStrings;
    Flink: string;
    FSoftwareVersion: string;
    Fwinbatch_del_argument: string;
    FinstallCommandLine: string;
    FuninstallCommandLine: string;
    FuninstallProg: string;
    FuninstallCheck: TStrings;
    FisExitcodeFatalFunction: string;
    Funinstall_waitforprocess: string;
    Finstall_waitforprocess: string;
    Fanalyze_progess: integer;
    procedure SetMarkerlist(const AValue: TStrings);
    procedure SetInfolist(const AValue: TStrings);
    procedure SetUninstallCheck(const AValue: TStrings);
  published
    // proc
    procedure SetArchitecture(const AValue: TArchitecture);
    procedure SetSetupFullFileName(const AValue: string);
    procedure SetMstFullFileName(const AValue: string);
    property ID: integer read FID write FID;
    property setupFileNamePath: string read FsetupFileNamePath;
    property setupFileName: string read FsetupFileName write FsetupFileName;
    property setupFullFileName: string read FsetupFullFileName
      write SetSetupFullFileName;
    property setupFileSize: cardinal read FsetupFileSize write FsetupFileSize;
    property architecture: TArchitecture read Farchitecture write Farchitecture;
    property msiId: string read FmsiId write FmsiId;
    property mstAllowed: boolean read FmstAllowed write FmstAllowed;
    property mstFullFileName: string read FMstFullFileName write SetMstFullFileName;
    property mstFileNamePath: string read FmstFileNamePath write FmstFileNamePath;
    property mstFileName: string read FmstFileName write FmstFileName;
    property msiFullFileName: string read FmsiFullFileName write FmsiFullFileName;
    property installerId: TKnownInstaller read FinstallerId write FinstallerId;
    property requiredSpace: cardinal read FrequiredSpace write FrequiredSpace;
    property installDirectory: string read FinstallDirectory write FinstallDirectory;
    property markerlist: TStrings read Fmarkerlist write SetMarkerlist;
    property infolist: TStrings read Finfolist write SetInfolist;
    property link: string read Flink write Flink;
    property SoftwareVersion: string read FSoftwareVersion write FSoftwareVersion;
    property winbatch_del_argument: string read Fwinbatch_del_argument
      write Fwinbatch_del_argument;
    property installCommandLine: string read FinstallCommandLine
      write FinstallCommandLine;
    property isExitcodeFatalFunction: string
      read FisExitcodeFatalFunction write FisExitcodeFatalFunction;
    property uninstallCommandLine: string read FuninstallCommandLine
      write FuninstallCommandLine;
    property uninstallProg: string read FuninstallProg write FuninstallProg;
    property uninstallCheck: TStrings read FuninstallCheck write SetUninstallCheck;
    property uninstall_waitforprocess: string
      read Funinstall_waitforprocess write Funinstall_waitforprocess;
    property install_waitforprocess: string
      read Finstall_waitforprocess write Finstall_waitforprocess;
    property analyze_progess: integer read Fanalyze_progess write Fanalyze_progess;
    procedure initValues;

  public
    { public declarations }

    constructor Create;
    destructor Destroy;
  end;

(*
[ProductDependency]
action: setup
requiredProduct: javavm
requiredStatus: installed
requirementType: before
*)

  TPDtype = (before, after, doNotMatter);
  //['before','after',''];
  TPActionRequest = (setup, uninstall, update, noRequest);
  TPInstallationState = (installed, not_installed, unknown, noState);

  TPDependency = class(TCollectionItem)
  private
    FAction: string;
    FRequProductId: string;
    FRequAction: TPActionRequest;
    FRequState: TPInstallationState;
    FRequType: TPDtype;
  published
    property RequType: TPDtype read FRequType write FRequType;
    property action: string read FAction;
    property requProductId: string read FRequProductId write FRequProductId;
    property requState: TPInstallationState read FRequState write FRequState;
    property requAction: TPActionRequest read FRequAction write FRequAction;
    procedure init;
  public
    { public declarations }
    //constructor Create;
    //destructor Destroy;
  end;


(*
  [ProductProperty]
type: bool
name: force_newest_ubuntu
description: Update even to not published versions
default: False

[ProductProperty]
type: unicode
name: update_from_to
multivalue: False
editable: False
description: Which method to use
values: ["jessie_stretch", "trusty_xenial", "wheezy_jessie", "xenial_bionic"]
default: ["xenial_bionic"]
*)

  TPPtype = (bool, unicode);

  TPProperty = class(TCollectionItem)
  private
    Ftype: TPPtype;
    Fname: string;
    Fmultivalue: boolean;
    Feditable: boolean;
    Fdescription: string;
    FStrvalues: TStrings;
    FStrDefault: TStrings;
    FBoolDefault: boolean;
    procedure SetValueLines(const AValue: TStrings);
    procedure SetDefaultLines(const AValue: TStrings);
  protected
    function GetDisplayName: string; override;
    // public
    //     procedure Assign(Source: TPersistent); override;
  published
    property ptype: TPPtype read Ftype write Ftype;
    property Name: string read Fname write Fname;
    property description: string read Fdescription write Fdescription;
    property multivalue: boolean read Fmultivalue write Fmultivalue;
    property editable: boolean read Feditable write Feditable;
    property StrDefault: TStrings read FStrDefault write SetDefaultLines;
    property Strvalues: TStrings read FStrvalues write FStrvalues;
    property boolDefault: boolean read FBoolDefault write FBoolDefault;
    procedure init;
  public
    { public declarations }
    constructor Create;
    destructor Destroy;
  end;

  // http://wiki.lazarus.freepascal.org/TCollection
  // https://stackoverflow.com/questions/6980401/collection-editor-does-not-open-for-a-tcollection-property-in-a-tpersistent-prop
  TPProperties = class(TOwnedCollection)
  private
    function GetItem(Index: integer): TPProperty;
    procedure SetItem(Index: integer; AValue: TPProperty);
    //function GetOwner: TPersistent;
  public
    constructor Create(AOwner: TPersistent);
  public
    function Add: TPProperty;
    function Insert(Index: integer): TPProperty;
    property Items[Index: integer]: TPProperty read GetItem write SetItem; default;
  end;


  TPriority = -100..100;

  TProductData = class(TPersistent)
  private
    FarchitectureMode: TArchitectureMode;
    Fcomment: string;
    Fdescription: string;
    Fadvice: string;
    FproductId: string;
    FproductName: string;
    Fproductversion: string;
    Fpackageversion: cardinal;
    Fversionstr: string;
    Fpriority: TPriority;
    Fproducttype: string;
    Fsetupscript: string;
    Funinstallscript: string;
    Fdelsubscript: string;
    Flicenserequired: boolean;
    FproductImageFullFileName: string;
    procedure SetPriority(const AValue: TPriority);
  published
    property architectureMode: TArchitectureMode
      read FarchitectureMode write FarchitectureMode;
    property comment: string read Fcomment write Fcomment;
    property description: string read Fdescription write Fdescription;
    property advice: string read Fadvice write Fadvice;
    property productId: string read FproductId write FproductId;
    property productName: string read FproductName write FproductName;
    property productversion: string read Fproductversion write Fproductversion;
    property packageversion: cardinal read Fpackageversion write Fpackageversion;
    property versionstr: string read Fversionstr write Fversionstr;
    property priority: TPriority read Fpriority write setpriority;
    property producttype: string read Fproducttype write Fproducttype;
    property setupscript: string read Fsetupscript write Fsetupscript;
    property uninstallscript: string read Funinstallscript write Funinstallscript;
    property delsubscript: string read Fdelsubscript write Fdelsubscript;
    property licenserequired: boolean read Flicenserequired write Flicenserequired;
    property productImageFullFileName: string
      read FproductImageFullFileName write FproductImageFullFileName;
  public
    { public declarations }
    //constructor Create;
    //destructor Destroy;
  end;

  TopsiProduct = class(TPersistent)
  private
  published
  public
    SetupFiles: array[0..1] of TSetupFile;
    productdata: TProductData;
    //dependeciesCount : integer;
    dependencies: TCollection;
    properties: TPProperties;


    { public declarations }
    constructor Create;
  end;

  TConfiguration = class(TPersistent)
  private
    Fconfig_version: string;
    // help to detect and handle changes of config file structure
    //Fworkbench_share: string;
    Fworkbench_Path: string;
    //Fworkbench_mounted: boolean;
    Fconfig_filled: boolean;
    FregisterInFilemanager: boolean;
    Femail_address: string;
    FFullName: string;
    Fimport_libraries: TStrings;
    FpreInstallLines: TStrings;
    FpostInstallLines: TStrings;
    FpreUninstallLines: TStrings;
    FpostUninstallLines: TStrings;
    FPathToOpsiPackageBuilder: string;
    FCreateRadioIndex: integer;  // Create mode
    FBuildRadioIndex: integer;  // Build mode
    //FCreateQuiet: boolean;
    //FCreateBuild: boolean;
    //FCreateInstall: boolean;
    FUsePropDesktopicon: boolean;
    FUsePropLicenseOrPool: boolean;
    FProperties: TPProperties;
    FReadme_txt_templ: string;
    procedure SetLibraryLines(const AValue: TStrings);
    procedure SetPreInstallLines(const AValue: TStrings);
    procedure SetPostInstallLines(const AValue: TStrings);
    procedure SetPreUninstallLines(const AValue: TStrings);
    procedure SetPostUninstallLines(const AValue: TStrings);
    procedure SetProperties(const AValue: TPProperties);
  published
    property config_version: string read Fconfig_version;
    //property workbench_share: string read Fworkbench_share write Fworkbench_share;
    property workbench_Path: string read Fworkbench_Path write Fworkbench_Path;
    //property workbench_mounted: boolean read Fworkbench_mounted write Fworkbench_mounted;
    property config_filled: boolean read Fconfig_filled write Fconfig_filled;
    property registerInFilemanager: boolean
      read FregisterInFilemanager write FregisterInFilemanager;
    property email_address: string read Femail_address write Femail_address;
    property fullName: string read FFullName write FFullName;
    property PathToOpsiPackageBuilder: string
      read FPathToOpsiPackageBuilder write FPathToOpsiPackageBuilder;
    property import_libraries: TStrings read Fimport_libraries write SetLibraryLines;
    property preInstallLines: TStrings read FpreInstallLines write SetPreInstallLines;
    property postInstallLines: TStrings read FpostInstallLines write SetPostInstallLines;
    property preUninstallLines: TStrings read FpreUninstallLines
      write SetPreUninstallLines;
    property postUninstallLines: TStrings read FpostUninstallLines
      write SetPostUninstallLines;
    property CreateRadioIndex: integer read FCreateRadioIndex write FCreateRadioIndex;
    property BuildRadioIndex: integer read FBuildRadioIndex write FBuildRadioIndex;
    //property CreateQuiet: boolean read FCreateQuiet write FCreateQuiet;
    //property CreateBuild: boolean read FCreateBuild write FCreateBuild;
    //property CreateInstall: boolean read FCreateInstall write FCreateInstall;
    property UsePropDesktopicon: boolean read FUsePropDesktopicon
      write FUsePropDesktopicon;
    property UsePropLicenseOrPool: boolean read FUsePropLicenseOrPool
      write FUsePropLicenseOrPool;
    //property Properties: TPProperties read FProperties  write SetProperties;
    property Readme_txt_templ: string read FReadme_txt_templ write FReadme_txt_templ;
    procedure writeconfig;
    procedure readconfig;
  public
    { public declarations }
    constructor Create;
    destructor Destroy;
  end;



function archModeStrToArchmode(modestr: string): TArchitectureMode;
function installerToInstallerstr(installerId: TKnownInstaller): string;
function instIdToint(installerId: TKnownInstaller): integer;
procedure initaktproduct;
procedure freebasedata;

var
  aktProduct: TopsiProduct;
  aktSetupFile: TSetupFile;
  knownInstallerList: TStringList;
  architectureModeList: TStringList;
  installerArray: TInstallers;
  counter: integer;
  myconfiguration: TConfiguration;
  useRunMode: TRunMode;
  myVersion: string;
  lfilename: string;

resourcestring

  // new for 4.1.0.2 ******************************************************************
  rsworkbench_Path = 'Path to the opsi_workbench';
  //rsPreInstallLines = 'opsi-script code, that will be included before the start of the installation.';
  rsworkbench_mounted =
    'Automatically detected. Is the opsi workbench reachable at workbench_Path.';
  rsconfig_filled = 'Automatically detected. Do we have all needed configurations';
  rsregisterInFilemanager =
    'Should this program be registred to the Filemanger (Explorer) context menu ?';
  rsemail_address = 'Your email address, used for the changelog entry';
  rsfullName = 'Your full name, used for the changelog entry';
  rsimport_libraries = 'List of opsi-script libraries that have to be imported.' +
    LineEnding + 'One per line. May be empty. Example:' + LineEnding +
    'myinstallhelperlib.opsiscript';
  rspreInstallLines =
    'List of opsi-script code lines that should be included before the installation starts. '
    + LineEnding + 'One per line. May be empty. Example: ' +
    LineEnding + 'comment "Start the installation ..."';
  rspostInstallLines =
    'List of opsi-script code lines that should be included after the installation finished.'
    + LineEnding + 'One per line. May be empty. Example:' + LineEnding +
    'comment "Installation finished..."';
  rspreUninstallLines =
    'List of opsi-script code lines that should be included before the uninstallation starts.'
    + LineEnding + 'One per line. May be empty. Example:' + LineEnding +
    'comment "Start the uninstallation ..."';
  rspostUninstallLines =
    'List of opsi-script code lines that should be included after the uninstallation finished.'
    + LineEnding + 'One per line. May be empty. Example:' + LineEnding +
    'comment "Uninstall finished..."';
  rspathToOpsiPackageBuilder =
    'Path to the OpsiPackageBuilder. OpsiPackageBuilder is used to build the opsi packages via ssh. see: https://forum.opsi.org/viewtopic.php?f=22&t=7573';
  rscreateRadioIndex = 'selects the Create mode Radiobutton.';
  rsBuildRadioIndex = 'selects the Build mode Radiobutton.';
    (*
  rscreateQuiet = 'Selects the Build mode Checkbox quiet.';
  rscreateBuild = 'Selects the Build mode Checkbox build.';
  rscreateInstall = 'Selects the Build mode Checkbox install.';
  *)


implementation

var
  FileVerInfo: TFileVersionInfo;

// TInstallerData ************************************

constructor TInstallerData.Create;
begin
  patterns := TStringList.Create;
  infopatterns := TStringList.Create;
  inherited;
end;

destructor TInstallerData.Destroy;
begin
  patterns.Free;
  infopatterns.Free;
  inherited;
end;

// TSetupFile ************************************

constructor TSetupFile.Create;
begin
  Fmarkerlist := TStringList.Create;
  Finfolist := TStringList.Create;
  FuninstallCheck := TStringList.Create;
  inherited;
  //initValues;
end;

destructor TSetupFile.Destroy;
begin
  FreeAndNil(Fmarkerlist);
  FreeAndNil(Finfolist);
  FreeAndNil(FuninstallCheck);
  inherited;
end;

procedure TSetupFile.SetArchitecture(const AValue: TArchitecture);
begin
  if AValue = Architecture then
    exit;
  FArchitecture := AValue;
  //Log('SetArchitecture '+GetEnumProp(Self,'Architecture'));
end;

procedure TSetupFile.SetSetupFullFileName(const AValue: string);
begin
  if AValue = SetupFullFileName then
    exit;
  FsetupFullFileName := AValue;
  FSetupFileNamePath := ExtractFileDir(AValue);
  FsetupFileName := ExtractFileName(AValue);
  //Log('SetSetupFileNamePath '+MyString);
end;

procedure TSetupFile.SetMstFullFileName(const AValue: string);
begin
  if AValue = mstFullFileName then
    exit;
  FmstFullFileName := AValue;
  FmstFileNamePath := ExtractFileDir(AValue);
  FmstFileName := ExtractFileName(AValue);
  //Log('SetSetupFileNamePath '+MyString);
end;

procedure TSetupFile.SetUninstallCheck(const AValue: TStrings);
begin
  FuninstallCheck.Assign(AValue);
end;

procedure TSetupFile.SetMarkerlist(const AValue: TStrings);
begin
  Fmarkerlist.Assign(AValue);
end;

procedure TSetupFile.SetInfolist(const AValue: TStrings);
begin
  Finfolist.Assign(AValue);
end;

procedure TSetupFile.initValues;
begin
  FsetupFileNamePath := '';
  FsetupFileName := '';
  FsetupFullFileName := '';
  FsetupFileSize := 0;
  Farchitecture := aUnknown;
  FmsiId := '';
  FmstAllowed := False;
  FmstFullFileName := '';
  FmstFileNamePath := '';
  FmstFileName := '';
  FmsiFullFileName := '';
  FinstallerId := stUnknown;
  FrequiredSpace := 0;
  FinstallDirectory := 'unknown';
  Fmarkerlist.Clear;
  Finfolist.Clear;
  Flink := '';
  FSoftwareVersion := '0.0';
  Fwinbatch_del_argument := '';
  FinstallCommandLine := '';
  FuninstallCommandLine := '';
  FuninstallProg := '';
  FuninstallCheck.Clear;
  Fanalyze_progess := 0;
  FisExitcodeFatalFunction := 'isMsExitcodeFatal_short';
  Funinstall_waitforprocess := '';
  Finstall_waitforprocess := '';
end;

// TPProperty **********************************

constructor TPProperty.Create;
begin
  inherited;
  init;
end;

procedure TPProperty.init;
begin
  FStrvalues := TStringList.Create;
  FStrdefault := TStringList.Create;
  Ftype := bool;
  Fmultivalue := False;
  Feditable := False;
  FBoolDefault := False;
end;


destructor TPProperty.Destroy;
begin
  FreeAndNil(FStrvalues);
  FreeAndNil(FStrdefault);
  inherited;
end;

procedure TPProperty.SetValueLines(const AValue: TStrings);
begin
  FStrvalues.Assign(AValue);
end;

procedure TPProperty.SetDefaultLines(const AValue: TStrings);
begin
  FStrDefault.Assign(AValue);
end;

function TPProperty.GetDisplayName: string;
begin
  Result := Fname;
end;

{ TPProperties }

constructor TPProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TPProperty);
end;

(*
function TPProperties.GetItems(Index: integer): TPProperty;
begin
  Result := TPProperty(inherited Items[Index]);
end;

procedure TPProperties.SetItems(Index: integer; AValue: TPProperty);
begin
  Items[Index].Assign(AValue);
end;
*)


function TPProperties.GetItem(Index: integer): TPProperty;
begin
  Result := TPProperty(inherited GetItem(Index));
end;



function TPProperties.Insert(Index: integer): TPProperty;
begin
  Result := TPProperty(inherited Insert(Index));
end;

procedure TPProperties.SetItem(Index: integer; AValue: TPProperty);
begin
  inherited SetItem(Index, AValue);
end;

function TPProperties.Add: TPProperty;
begin
  Result := inherited Add as TPProperty;
end;

// TPDependency **********************************
procedure TPDependency.init;
begin
  FAction := 'setup';
  FRequType := doNotMatter;
  FRequAction := noRequest;
  FRequState := noState;
  FRequProductId := '';
end;

// TopsiProduct **********************************
constructor TopsiProduct.Create;
begin
  inherited;
  //initaktproduct;
end;

// TProductData **********************************
procedure TProductData.SetPriority(const AValue: TPriority);
begin
  if AValue = priority then
    exit;
  Fpriority := AValue;
end;
// TopsiProduct **********************************

// TConfiguration ************************************

constructor TConfiguration.Create;
begin
  inherited;
  Fimport_libraries := TStringList.Create;
  FpreInstallLines := TStringList.Create;
  FpostInstallLines := TStringList.Create;
  FpreUninstallLines := TStringList.Create;
  FpostUninstallLines := TStringList.Create;
  FregisterInFilemanager := True;
  Femail_address := 'missing';
  FFullName := 'missing';
  Fworkbench_Path := 'missing';
  FPathToOpsiPackageBuilder := 'unknown';
  Fconfig_filled := False;
  FProperties := TPProperties.Create(self);
  Fconfig_version := myVersion;
  FReadme_txt_templ := ExtractFileDir(ParamStr(0)) + PathDelim +
    'template-files' + PathDelim + 'package_qa.txt';
  //readconfig;
end;

destructor TConfiguration.Destroy;
begin
  //writeconfig;
  FreeAndNil(Fimport_libraries);
  FreeAndNil(FpreInstallLines);
  FreeAndNil(FpostInstallLines);
  FreeAndNil(FpreUninstallLines);
  FreeAndNil(FpostUninstallLines);
  FreeAndNil(FProperties);
  inherited;
end;

procedure TConfiguration.SetLibraryLines(const AValue: TStrings);
begin
  Fimport_libraries.Assign(AValue);
end;

procedure TConfiguration.SetPreInstallLines(const AValue: TStrings);
begin
  FpreInstallLines.Assign(AValue);
end;

procedure TConfiguration.SetPostInstallLines(const AValue: TStrings);
begin
  FpostInstallLines.Assign(AValue);
end;

procedure TConfiguration.SetPreUninstallLines(const AValue: TStrings);
begin
  FpreUninstallLines.Assign(AValue);
end;

procedure TConfiguration.SetPostUninstallLines(const AValue: TStrings);
begin
  FpostUninstallLines.Assign(AValue);
end;

procedure TConfiguration.SetProperties(const AValue: TPProperties);
begin
  FProperties.Assign(AValue);
end;

procedure TConfiguration.writeconfig;
var
  Streamer: TJSONStreamer;
  JSONString: string;
  myfilename: string;
  configDir: array[0..MaxPathLen] of char; //Allocate memory
  configDirUtf8: UTF8String;
  //myfile: TextFile;

  // http://wiki.freepascal.org/File_Handling_In_Pascal
  // SaveStringToFile: function to store a string of text into a diskfile.
  //   If the function result equals true, the string was written ok.
  //   If not then there was some kind of error.
  function SaveStringToFile(theString, filePath: ansistring): boolean;
  var
    fsOut: TFileStream;
  begin
    // By default assume the writing will fail.
    Result := False;

    // Write the given string to a file, catching potential errors in the process.
    try
      fsOut := TFileStream.Create(filePath, fmCreate);
      fsOut.Write(theString[1], length(theString));
      fsOut.Free;

      // At his point it is known that the writing went ok.
      Result := True

    except
      on E: Exception do
        LogDatei.log('String could not be written. Details: ' +
          E.ClassName + ': ' + E.Message, LLError);
    end;
  end;

begin
  try
    if Assigned(logdatei) then
      logdatei.log('Start writeconfig', LLDebug);
    configDir := '';
  {$IFDEF Windows}
    SHGetFolderPath(0, CSIDL_APPDATA, 0, SHGFP_TYPE_CURRENT, configDir);
    configDir := configDir + PathDelim + 'opsi.org' + PathDelim;
    configDirUtf8 := WinCPToUTF8(configDir);
  {$ELSE}
    configDir := GetAppConfigDir(False);
    configDirUtf8 := configDir;
  {$ENDIF WINDOWS}
    configDirUtf8 := StringReplace(configDirUtf8, 'opsi-setup-detector',
      'opsi.org', [rfReplaceAll]);
    configDirUtf8 := StringReplace(configDirUtf8, 'opsisetupdetector',
      'opsi.org', [rfReplaceAll]);
    myfilename := configDirUtf8 + PathDelim + 'opsisetupdetector.cfg';
    myfilename := ExpandFileName(myfilename);
    if Assigned(logdatei) then
      logdatei.log('writeconfig to: ' + myfilename, LLDebug);
    if not DirectoryExists(configDirUtf8) then
      if not ForceDirectories(configDirUtf8) then
        if Assigned(logdatei) then
          LogDatei.log('failed to create configuration directory: ' +
            configDirUtf8, LLError);

    if (Femail_address = 'missing') or (FFullName = 'missing') or
      (Fworkbench_Path = 'missing') then
      //or (FPathToOpsiPackageBuilder = 'missing') then
      Fconfig_filled := False
    else
      Fconfig_filled := True;
    // http://wiki.freepascal.org/Streaming_JSON
    Streamer := TJSONStreamer.Create(nil);
    try
      Streamer.Options := Streamer.Options + [jsoTStringsAsArray];
      // Save strings as JSON array
      // JSON convert and output
      JSONString := Streamer.ObjectToJSONString(myconfiguration);
      logdatei.log('Config: ' + JSONString, LLDebug);
      if not SaveStringToFile(JSONString, myfilename) then
        if Assigned(logdatei) then
          LogDatei.log('failed save configuration', LLError);
      //AssignFile(myfile, myfilename);
      //Rewrite(myfile);
      //Write(myfile, JSONString);
      {$IFDEF WINDOWS}
      registerForWinExplorer(FregisterInFilemanager);
  {$ELSE}
  {$ENDIF WINDOWS}
    finally
      //CloseFile(myfile);
      Streamer.Destroy;
    end;
    if Assigned(logdatei) then
      logdatei.log('Finished writeconfig', LLDebug2);

  except
    on E: Exception do
      if Assigned(logdatei) then
        LogDatei.log('Configuration could not be written. Details: ' +
          E.ClassName + ': ' + E.Message, LLError);
  end;
end;


procedure TConfiguration.readconfig;
var
  DeStreamer: TJSONDeStreamer;
  //Streamer: TJSONStreamer;
  JSONString: string;
  myfilename: string;
  configDir: array[0..MaxPathLen] of char; //Allocate memory
  configDirUtf8: UTF8String;
  configDirstr: string;
  myfile: Text;
  oldconfigDir, oldconfigFileName, tmpstr: string;
  fConfig: Text;

  // http://wiki.freepascal.org/File_Handling_In_Pascal
  // LoadStringFromFile: function to load a string of text from a diskfile.
  //   If the function result equals true, the string was load ok.
  //   If not then there was some kind of error.
  function LoadStringFromFile(theString, filePath: ansistring): boolean;
  var
    fsOut: TFileStream;
  begin
    // By default assume the writing will fail.
    Result := False;

    // Write the given string to a file, catching potential errors in the process.
    try
      fsOut := TFileStream.Create(filePath, fmOpenRead);
      fsOut.Read(theString[1], length(theString));
      fsOut.Free;

      // At his point it is known that the writing went ok.
      Result := True

    except
      on E: Exception do
        if Assigned(logdatei) then
          LogDatei.log('String could not be read. Details: ' +
            E.ClassName + ': ' + E.Message, LLError)
        else
          ShowMessage('readconfig: String could not be read. Details: ' +
            E.ClassName + ': ' + E.Message);
    end;
  end;

begin
  try
    if Assigned(logdatei) then
      logdatei.log('Start readconfig', LLDebug);
    configDir := '';
  {$IFDEF Windows}
    SHGetFolderPath(0, CSIDL_APPDATA, 0, SHGFP_TYPE_CURRENT, configDir);
    configDir := configDir + PathDelim + 'opsi.org' + PathDelim;
    configDirUtf8 := WinCPToUTF8(configDir);
  {$ELSE}
    configDir := GetAppConfigDir(False);
    configDirUtf8 := configDir;
  {$ENDIF WINDOWS}
    configDirUtf8 := StringReplace(configDirUtf8, 'opsi-setup-detector',
      'opsi.org', [rfReplaceAll]);
    configDirUtf8 := StringReplace(configDirUtf8, 'opsisetupdetector',
      'opsi.org', [rfReplaceAll]);
    myfilename := configDirUtf8 + PathDelim + 'opsisetupdetector.cfg';
    myfilename := ExpandFileName(myfilename);
    if Assigned(logdatei) then
      logdatei.log('readconfig from: ' + myfilename, LLDebug);
    if FileExists(myfilename) then
    begin
      AssignFile(myfile, myfilename);
      Reset(myfile);
      readln(myfile, JSONString);
      //// http://wiki.freepascal.org/Streaming_JSON
      // DeStreamer object create
      DeStreamer := TJSONDeStreamer.Create(nil);
      try
        // Load JSON data in the object
        DeStreamer.JSONToObject(JSONString, myconfiguration);
        // Cleanup
      finally
        DeStreamer.Destroy;
        CloseFile(myfile);
      end;
      if Assigned(logdatei) then
        logdatei.log('read config: ' + JSONString, LLDebug)
      else
        ShowMessage('read config: ' + JSONString);
    {$IFDEF WINDOWS}
      registerForWinExplorer(FregisterInFilemanager);
    {$ELSE}
    {$ENDIF WINDOWS}
    end
    else
    begin
      tmpstr := '';
      // check for old config
      // get global ConfigDir
      oldconfigDir := GetAppConfigDir(True);
      oldconfigFileName := oldconfigDir + 'config.txt';
      if FileExists(oldconfigFileName) then
      begin
        AssignFile(fConfig, oldconfigFileName);
        Reset(fConfig);
        if not EOF(fConfig) then
          ReadLn(fConfig, tmpstr);
        CloseFile(fConfig);
      end;
    end;
    if Assigned(logdatei) then
      logdatei.log('Finished readconfig', LLDebug2);

  except
    on E: Exception do
      if Assigned(logdatei) then
        LogDatei.log('read config exception. Details: ' +
          E.ClassName + ': ' + E.Message, LLError)
      else
        ShowMessage('read config exception. Details: ' +
          E.ClassName + ': ' + E.Message);
  end;
end;


// Installer related ************************************

function archModeStrToArchmode(modestr: string): TArchitectureMode;
begin
  Result := TArchitectureMode(architectureModeList.IndexOf(modestr));
end;

function installerToInstallerstr(installerId: TKnownInstaller): string;
begin
  Result := knownInstallerList.Strings[integer(installerId)];
end;

function instIdToint(installerId: TKnownInstaller): integer;
begin
  Result := integer(installerId);
end;

function detecteddummy(parent: TClass; markerlist: TStringList): boolean;
begin
  Result := False;
end;


function detectedinno(parent: TClass; markerlist: TStringList): boolean;
var
  i1, i2, i3, i4, i5, i6: integer;
begin
  Result := False;
  markerlist.Sort;
  if markerlist.Find(TInstallerData(parent).patterns[0], i1) or
    markerlist.Find(TInstallerData(parent).patterns[1], i2) then
    Result := True;
end;

function detectedbypatternwithor(parent: TClass; markerlist: TStringList): boolean;
var
  //tmpint: integer;
  patternindex: integer;
  pattern: string;
begin
  Result := False;
  //markerlist.Sort;
  for patternindex := 0 to TInstallerData(parent).patterns.Count - 1 do
  begin
    pattern := TInstallerData(parent).patterns[patternindex];
    if markerlist.IndexOf(LowerCase(pattern)) >= 0 then
      Result := True;
  end;
end;

function detectedbypatternwithAnd(parent: TClass; markerlist: TStringList): boolean;
var
  //tmpint: integer;
  patternindex: integer;
  pattern: string;
  numberOfPatterndetected: integer = 0;
begin
  Result := False;
  //markerlist.Sort;
  for patternindex := 0 to TInstallerData(parent).patterns.Count - 1 do
  begin
    pattern := TInstallerData(parent).patterns[patternindex];
    if markerlist.IndexOf(LowerCase(pattern)) >= 0 then
      Inc(numberOfPatterndetected);
  end;
  if numberOfPatterndetected = TInstallerData(parent).patterns.Count then
    Result := True;
end;

procedure initaktproduct;
var
  i: integer;
  //newdep: TPDependency;
begin
  LogDatei.log('Start initaktproduct ... ', LLInfo);
  i := 0;
  for i := 0 to 1 do
  begin
    if not Assigned(aktProduct.SetupFiles[i]) then
      aktProduct.SetupFiles[i] := TSetupFile.Create;
    aktProduct.SetupFiles[i].initValues;
    aktProduct.SetupFiles[i].ID := i + 1;
  end;
  if not Assigned(aktProduct.productdata) then
    aktProduct.productdata := TProductData.Create;
  with aktProduct.productdata do
  begin
    comment := '';
    description := '';
    advice := '';
    productId := '';
    productName := '';
    productversion := '';
    packageversion := 1;
    versionstr := '';
    priority := 0;
    producttype := 'localboot';
    setupscript := 'setup.opsiscript';
    uninstallscript := 'uninstall.opsiscript';
    delsubscript := 'delsub.opsiscript';
    licenserequired := False;
    // Application.Params[0] is directory of application as string
    productImageFullFileName :=
      ExtractFileDir(Application.Params[0]) + PathDelim + 'template-files' + PathDelim +
      'template.png';
  end;
  // Create Dependencies
  aktProduct.dependencies := TCollection.Create(TPDependency);
  // Create Properties
  aktProduct.properties := TPProperties.Create(aktProduct);
end;

procedure freebasedata;
var
  i: integer;
begin
  for i := 0 to 1 do
  begin
    if Assigned(aktProduct.SetupFiles[i]) then
      aktProduct.SetupFiles[i].Destroy;
  end;
  if Assigned(aktProduct.productdata) then
    aktProduct.productdata.Destroy;
  if Assigned(aktProduct.dependencies) then
    FreeAndNil(aktProduct.dependencies);
  if Assigned(myconfiguration) then
  begin
    myconfiguration.writeconfig;
    myconfiguration.Destroy;
  end;
end;


begin
  // marker for add installers
  knownInstallerList := TStringList.Create;
  knownInstallerList.Add('SFXcab');
  knownInstallerList.Add('BoxStub');
  knownInstallerList.Add('AdvancedMSI');
  knownInstallerList.Add('InstallShield');
  knownInstallerList.Add('InstallShieldMSI');
  knownInstallerList.Add('MSI');
  knownInstallerList.Add('NSIS');
  knownInstallerList.Add('7zip');
  knownInstallerList.Add('7zipsfx');
  knownInstallerList.Add('InstallAware');
  knownInstallerList.Add('MSGenericInstaller');
  knownInstallerList.Add('WixToolset');
  knownInstallerList.Add('Bitrock');
  knownInstallerList.Add('SelfExtractingInstaller');
  knownInstallerList.Add('Inno');
  knownInstallerList.Add('Unknown');


  for counter := 0 to knownInstallerList.Count - 1 do
  begin
    SetLength(installerArray, counter + 1);
    installerArray[counter] := TInstallerData.Create;
    installerArray[counter].installerId := TKnownInstaller(counter);
    installerArray[counter].Name := knownInstallerList.Strings[counter];
  end;

  // unknown
  with installerArray[integer(stUnknown)] do
  begin
    description := 'Unknown Installer';
    silentsetup := '';
    unattendedsetup := '';
    silentuninstall := '';
    unattendeduninstall := '';
    uninstall_waitforprocess := '';
    uninstallProg := '';
    link :=
      'https://startpage.com/do/search?cmd=process_search&query=silent+install';
    comment := '';
    uib_exitcode_function := 'isMsExitcodeFatal_short';
    detected := @detectedbypatternwithor;
  end;

  // inno
  with installerArray[integer(stInno)] do
  begin
    description := 'Inno Setup';
    silentsetup :=
      '/sp- /verysilent /norestart /nocancel /SUPPRESSMSGBOXES';
    unattendedsetup :=
      '/sp- /silent /norestart /nocancel /SUPPRESSMSGBOXES';
    silentuninstall :=
      '/verysilent /norestart /nocancel /SUPPRESSMSGBOXES';
    unattendeduninstall :=
      '/silent /norestart /nocancel /SUPPRESSMSGBOXES';
    uninstall_waitforprocess := '';
    uninstallProg := 'unins000.exe';
    patterns.Add('<description>inno setup</description>');
    patterns.Add('jr.inno.setup');
    link :=
      'http://www.jrsoftware.org/ishelp/topic_setupcmdline.htm';
    comment := '';
    uib_exitcode_function := 'isInnoExitcodeFatal';
    detected := @detectedbypatternwithor;
  end;

  // NSIS
  with installerArray[integer(stNsis)] do
  begin
    description := 'Nullsoft Install System';
    silentsetup := '/S';
    unattendedsetup := '/S';
    silentuninstall := '/S';
    unattendeduninstall := '/S';
    uninstall_waitforprocess := 'Au_.exe';
    uninstallProg := 'uninstall.exe';
    patterns.Add('Nullsoft.NSIS.exehead');
    patterns.Add('nullsoft install system');
    patterns.Add('http://nsis.sf.net/');
    patterns.Add('NSISu_.exe');
    patterns.Add('NSIS Error');
    link :=
      'http://nsis.sourceforge.net/Docs/Chapter3.html#installerusage';
    comment := '';
    uib_exitcode_function := 'isNsisExitcodeFatal';
    detected := @detectedbypatternwithor;

  end;
  // InstallShield
  with installerArray[integer(stInstallShield)] do
  begin
    description :=
      'InstallShield Setup (classic)';
    silentsetup := '/s /sms';
    unattendedsetup := '/s /sms';
    silentuninstall := '/s /sms';
    unattendeduninstall := '/s /sms';
    uninstall_waitforprocess := '';
    uninstallProg := 'uninstall.exe';
    patterns.Add('InstallShield');
    patterns.Add(
      '<description>InstallShield.Setup</description>');
    link :=
      'http://helpnet.flexerasoftware.com/installshield19helplib/helplibrary/IHelpSetup_EXECmdLine.htm';
    comment := '';
    uib_exitcode_function := 'isInstallshieldExitcodeFatal';
    detected := @detectedbypatternwithAnd;
  end;
  // InstallShieldMSI
  with installerArray[integer(stInstallShieldMSI)] do
  begin
    description :=
      'InstallShield+MSI Setup (InstallShield with embedded MSI)';
    silentsetup :=
      '/s /v" /qn ALLUSERS=1 REBOOT=ReallySuppress"';
    unattendedsetup :=
      '/s /v" /qb-! ALLUSERS=1 REBOOT=ReallySuppress"';
    silentuninstall :=
      '/qn REBOOT=ReallySuppress';
    unattendeduninstall :=
      '/qb-! REBOOT=ReallySuppress';
    uninstall_waitforprocess := '';
    uninstallProg := 'uninstall.exe';
    patterns.Add('nstallshield');
    patterns.Add('issetup.dll');
    patterns.Add('transforms');
    patterns.Add('msiexec.exe');
    link :=
      'http://helpnet.flexerasoftware.com/installshield19helplib/helplibrary/IHelpSetup_EXECmdLine.htm';
    comment := '';
    uib_exitcode_function := 'isInstallshieldExitcodeFatal';
    detected := @detectedbypatternwithAnd;
  end;
  // MSI
  with installerArray[integer(stMSI)] do
  begin
    description :=
      'MSI Setup';
    silentsetup :=
      '/l* "$LogDir$\$ProductId$.install_log.txt" /qn ALLUSERS=1 REBOOT=ReallySuppress';
    unattendedsetup :=
      '/l* "$LogDir$\$ProductId$.install_log.txt" /qb-! ALLUSERS=1 REBOOT=ReallySuppress';
    silentuninstall :=
      ' /qn REBOOT=ReallySuppress';
    unattendeduninstall :=
      ' /qb-! REBOOT=ReallySuppress';
    uninstall_waitforprocess := '';
    uninstallProg := '';
    //patterns.Add('nstallshield');
    //patterns.Add('installer,msi,database');
    link :=
      'http://helpnet.flexerasoftware.com/installshield19helplib/helplibrary/IHelpSetup_EXECmdLine.htm';
    comment := '';
    uib_exitcode_function := 'isMsiExitcodeFatal';
    detected := @detectedbypatternwithor;
  end;

  // 7zip
  with installerArray[integer(st7zip)] do
  begin
    description := '7zip installer';
    silentsetup := '/S';
    unattendedsetup := '/S';
    silentuninstall := '/S';
    unattendeduninstall := '/S';
    uninstall_waitforprocess := '';
    uninstallProg := 'uninstall.exe';
    patterns.Add('7-Zip Installer');
    link := 'https://www.7-zip.org/faq.html';
    comment := '';
    uib_exitcode_function := 'isMsExitcodeFatal_short';
    detected := @detectedbypatternwithor;
  end;
  // st7zipsfx
  with installerArray[integer(st7zipsfx)] do
  begin
    description := '7zipsfx installer';
    silentsetup := '-y';
    unattendedsetup := '-y';
    silentuninstall := '-y';
    unattendeduninstall := '-y';
    uninstall_waitforprocess := '';
    uninstallProg := '';
    patterns.Add('7zipsfx');
    link := 'https://sourceforge.net/p/s-zipsfxbuilder/code/ci/master/tree/7zSD_EN.chm';
    comment := '';
    uib_exitcode_function := 'isMsExitcodeFatal_short';
    detected := @detectedbypatternwithor;
  end;
  // stInstallAware
  with installerArray[integer(stInstallAware)] do
  begin
    description := 'InstallAware';
    silentsetup := '/s /l="$LogDir$\$ProductId$.install_log.txt"';
    unattendedsetup := '/s /l="$LogDir$\$ProductId$.install_log.txt"';
    silentuninstall := '/s MODIFY=FALSE REMOVE=TRUE UNINSTALL=YES';
    unattendeduninstall := '/s MODIFY=FALSE REMOVE=TRUE UNINSTALL=YES';
    uninstall_waitforprocess := '';
    install_waitforprocess := '';
    uninstallProg := '';
    patterns.Add('InstallAware: http://www.installaware.com');
    infopatterns.Add('RunProgram="');
    link := 'https://www.installaware.com/mhtml5/desktop/setupcommandlineparameters.htm';
    comment := '';
    uib_exitcode_function := 'isMsExitcodeFatal_short';
    detected := @detectedbypatternwithor;
  end;
  // stMSGenericInstaller
  with installerArray[integer(stMSGenericInstaller)] do
  begin
    description := 'generic MS Installer';
    silentsetup := '/quiet /norestart /log "$LogDir$\$ProductId$.install_log.txt"';
    unattendedsetup := '/passive /norestart /log "$LogDir$\$ProductId$.install_log.txt"';
    silentuninstall := '/quiet /norestart /log "$LogDir$\$ProductId$.install_log.txt"';
    unattendeduninstall :=
      '/passive /norestart /log "$LogDir$\$ProductId$.install_log.txt"';
    uninstall_waitforprocess := '';
    install_waitforprocess := '';
    uninstallProg := '';
    patterns.Add('layoutpromptrestartforcerestartnorestartpassivesilentsquietqhelph');
    //infopatterns.Add('RunProgram="');
    link :=
      'https://docs.microsoft.com/en-us/windows/desktop/msi/standard-installer-command-line-options';
    comment := '';
    uib_exitcode_function := 'isMsExitcodeFatal_short';
    detected := @detectedbypatternwithor;
  end;
  // stWixToolset
  with installerArray[integer(stWixToolset)] do
  begin
    description := 'Wix Toolset';
    silentsetup := '/quiet /norestart /log "$LogDir$\$ProductId$.install_log.txt"';
    unattendedsetup := '/passive /norestart /log "$LogDir$\$ProductId$.install_log.txt"';
    silentuninstall := '/quiet /norestart /log "$LogDir$\$ProductId$.install_log.txt"';
    unattendeduninstall :=
      '/passive /norestart /log "$LogDir$\$ProductId$.install_log.txt"';
    uninstall_waitforprocess := '';
    install_waitforprocess := '';
    uninstallProg := '';
    patterns.Add('WixBundle');
    patterns.Add('Wix Toolset');
    //infopatterns.Add('RunProgram="');
    link :=
      'https://docs.microsoft.com/en-us/windows/desktop/msi/standard-installer-command-line-options';
    comment := '';
    uib_exitcode_function := 'isMsExitcodeFatal_short';
    detected := @detectedbypatternwithand;
  end;
  // stBoxStub
  with installerArray[integer(stBoxStub)] do
  begin
    description := 'MS Box Stup';
    silentsetup := '/quiet /norestart /log "$LogDir$\$ProductId$.install_log.txt"';
    unattendedsetup := '/passive /norestart /log "$LogDir$\$ProductId$.install_log.txt"';
    silentuninstall := '/quiet /norestart /log "$LogDir$\$ProductId$.install_log.txt"';
    unattendeduninstall :=
      '/passive /norestart /log "$LogDir$\$ProductId$.install_log.txt"';
    uninstall_waitforprocess := '';
    install_waitforprocess := '';
    uninstallProg := '';
    patterns.Add('boxstub.exe');
    //patterns.Add('Wix Toolset');
    //infopatterns.Add('RunProgram="');
    link :=
      'https://docs.microsoft.com/en-us/windows/desktop/msi/standard-installer-command-line-options';
    comment := '';
    uib_exitcode_function := 'isMsExitcodeFatal_short';
    detected := @detectedbypatternwithand;
  end;
  // stSFXcab
  with installerArray[integer(stSFXcab)] do
  begin
    description := 'MS SFX Cab ';
    silentsetup := '/quiet /norestart /log "$LogDir$\$ProductId$.install_log.txt"';
    unattendedsetup := '/passive /norestart /log "$LogDir$\$ProductId$.install_log.txt"';
    silentuninstall := '/quiet /norestart /log "$LogDir$\$ProductId$.install_log.txt"';
    unattendeduninstall :=
      '/passive /norestart /log "$LogDir$\$ProductId$.install_log.txt"';
    uninstall_waitforprocess := '';
    install_waitforprocess := '';
    uninstallProg := '';
    patterns.Add('sfxcab.exe');
    //patterns.Add('Wix Toolset');
    //infopatterns.Add('RunProgram="');
    link :=
      'https://docs.microsoft.com/en-us/windows/desktop/msi/standard-installer-command-line-options';
    comment := '';
    uib_exitcode_function := 'isMsExitcodeFatal_short';
    detected := @detectedbypatternwithand;
  end;
  // stBitrock
  with installerArray[integer(stBitrock)] do
  begin
    description := 'Bitrock installer ';
    silentsetup := '--mode unattended --unattendedmodeui none';
    unattendedsetup := '--mode unattended --unattendedmodeui minimal';
    silentuninstall := '--mode unattended --unattendedmodeui none';
    unattendeduninstall := '--mode unattended --unattendedmodeui minimal';
    uninstall_waitforprocess := '';
    install_waitforprocess := '';
    uninstallProg := 'uninstall.exe';
    patterns.Add('<description>BitRock Installer</description>');
    //patterns.Add('Wix Toolset');
    //infopatterns.Add('RunProgram="');
    link :=
      'https://clients.bitrock.com/installbuilder/docs/installbuilder-userguide/ar01s08.html#_help_menu';
    comment := '';
    uib_exitcode_function := 'isMsExitcodeFatal_short';
    detected := @detectedbypatternwithand;
  end;
  // stSelfExtractingInstaller
  with installerArray[integer(stSelfExtractingInstaller)] do
  begin
    description := 'SelfExtracting Installer';
    silentsetup := '/a';
    unattendedsetup := '/a';
    silentuninstall := '/a /u:"<product>"';
    unattendeduninstall := '/a /u:"<product>"';
    uninstall_waitforprocess := '';
    install_waitforprocess := '';
    uninstallProg := 'uninstall.exe';
    patterns.Add('Self-extracting installation program');
    //patterns.Add('Wix Toolset');
    //infopatterns.Add('RunProgram="');
    link := '';
    comment := 'Unknown Vendor';
    uib_exitcode_function := 'isMsExitcodeFatal_short';
    detected := @detectedbypatternwithand;
  end;
  // marker for add installers

  architectureModeList := TStringList.Create;
  architectureModeList.Add('32BitOnly - fix');
  architectureModeList.Add('64BitOnly - fix');
  architectureModeList.Add('both - fix');
  architectureModeList.Add('systemSpecific - fix');
  architectureModeList.Add('selectable');

  myconfiguration := TConfiguration.Create;


  //aktSetupFile := TSetupFile.Create;
  aktProduct := TopsiProduct.Create;

  FileVerInfo := TFileVersionInfo.Create(nil);
  try
    FileVerInfo.FileName := ParamStr(0);
    FileVerInfo.ReadFileInfo;
    myVersion := FileVerInfo.VersionStrings.Values['FileVersion'];
  finally
    FileVerInfo.Free;
  end;

  // Initialize logging
  LogDatei := TLogInfo.Create;
  lfilename := ExtractFileName(Application.ExeName);
  lfilename := ExtractFileNameOnly(lfilename);
  LogDatei.FileName := lfilename;
  LogDatei.StandardLogFileext := '.log';
  LogDatei.StandardLogFilename := lfilename;
  LogDatei.WritePartLog := False;
  LogDatei.CreateTheLogfile(lfilename + '.log', True);
  LogDatei.log('Log for: ' + Application.exename + ' opend at : ' +
    DateTimeToStr(now), LLEssential);
  LogDatei.log('opsi-setup-detector Version: ' + myVersion, LLEssential);
  LogDatei.LogLevel := 8;

  myconfiguration.readconfig;
end.
