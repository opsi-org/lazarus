unit osdbasedata;

{$mode delphi}

interface

uses
  Classes, SysUtils, LCLProc, LResources, TypInfo, Forms, Controls, Graphics,
  Dialogs, StdCtrls,
  strutils,
  {$IFDEF WINDOWS}
  osdhelper,
  shlobj,
  winpeimagereader,
  {$ENDIF WINDOWS}
  LazFileUtils,
  lazutf8,
  fileinfo,
  fpjsonrtti,
  fpjson,
  oslog,
  RTTICtrls,
  osjson,
  osregex,
  lcltranslator,
  oscrypt;

type

  TRunMode = (analyzeOnly,
    singleAnalyzeCreate, twoAnalyzeCreate_1,
    twoAnalyzeCreate_2, createTemplate, threeAnalyzeCreate_1,
    threeAnalyzeCreate_2, threeAnalyzeCreate_3, createMultiTemplate, createMeta,
    analyzeCreateWithUser, gmUnknown);

  TTemplateChannels = (training, default, structured, custom);

  TArchitecture = (a32, a64, aUnknown);


  { stores runtime infos of OSD}
  TOSDSettings = class(TPersistent)
  private
    FrunMode: TRunMode;
  published
    property runmode: TRunMode read FrunMode write FrunMode;
  public
    { public declarations }
    //constructor Create;
    //destructor Destroy;
  end;

  TTargetOS = (osLin, osWin, osMac, osMulti, osUnknown);
  TTargetOSset = set of TTargetOS;

  TArchitectureMode = (am32only_fix, am64only_fix, amBoth_fix, amSystemSpecific_fix,
    amSelectable);

  // marker for add installers
  TKnownInstaller = (stLinRPM, stLinDeb, stMacZip, stMacDmg, stMacPKG, stMacApp,
    stSFXcab, stBoxStub, stAdvancedMSI, stInstallShield,
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
    installErrorHandlingLines: TStringList;
    { public declarations }
    constructor Create;
    destructor Destroy;
  end;

  TInstallers = array of TInstallerData;

  TSetupFile = class(TPersistent)
  private
    Factive: boolean;
    FID: integer; // 1 = first setup file, 2 = second setup file
    FsetupFileNamePath: string;
    FsetupFileName: string;
    FsetupFullFileName: string;
    FsetupFileSize: cardinal;     // MB
    Farchitecture: TArchitecture;
    FmsiId: string;
    FmsiProductName: string;
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
    FtargetProg: string;
    FuninstallCheck: TStrings;
    FisExitcodeFatalFunction: string;
    Funinstall_waitforprocess: string;
    Finstall_waitforprocess: string;
    FinstallErrorHandlingLines: TStrings;
    Fanalyze_progess: integer;
    FcopyCompleteDir: boolean;
    FtargetOS: TTargetOS;
    FinstallerSourceDir: string;
    procedure SetMarkerlist(const AValue: TStrings);
    procedure SetInfolist(const AValue: TStrings);
    procedure SetUninstallCheck(const AValue: TStrings);
    procedure SetInstallErrorHandlingLines(const AValue: TStrings);
    //procedure OnRestoreProperty(Sender: TObject; AObject: TObject;
    //  Info: PPropInfo; AValue: TJSONData; var Handled: Boolean);
  published
    // proc
    procedure SetArchitecture(const AValue: TArchitecture);
    procedure SetSetupFullFileName(const AValue: string);
    procedure SetMstFullFileName(const AValue: string);
    property ID: integer read FID write FID;
    property setupFileNamePath: string read FsetupFileNamePath write FsetupFileNamePath;
    property setupFileName: string read FsetupFileName write FsetupFileName;
    property setupFullFileName: string read FsetupFullFileName
      write SetSetupFullFileName;
    property setupFileSize: cardinal read FsetupFileSize write FsetupFileSize;
    property architecture: TArchitecture read Farchitecture write Farchitecture;
    property msiId: string read FmsiId write FmsiId;
    property msiProductName: string read FmsiProductName write FmsiProductName;
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
    property targetProg: string read FtargetProg write FtargetProg;
    property uninstallCheck: TStrings read FuninstallCheck write SetUninstallCheck;
    property uninstall_waitforprocess: string
      read Funinstall_waitforprocess write Funinstall_waitforprocess;
    property install_waitforprocess: string
      read Finstall_waitforprocess write Finstall_waitforprocess;
    property installErrorHandlingLines: TStrings
      read FinstallErrorHandlingLines write SetInstallErrorHandlingLines;
    property analyze_progess: integer read Fanalyze_progess write Fanalyze_progess;
    property copyCompleteDir: boolean read FcopyCompleteDir write FcopyCompleteDir;
    property targetOS: TTargetOS read FtargetOS write FtargetOS;
    property active: boolean read Factive write Factive;
    property installerSourceDir: string read FinstallerSourceDir
      write FinstallerSourceDir;
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
  TPActionRequest = (setup, uninstall, update, once, noRequest);
  TPInstallationState = (installed, not_installed, unknown, noState);

  TPDependency = class(TCollectionItem)
  private
    FAction: string;
    FRequProductId: string;
    FRequAction: TPActionRequest;
    FRequState: TPInstallationState;
    FRequType: TPDtype;
  published
    property Required_Type: TPDtype read FRequType write FRequType;
    property action: string read FAction write FAction;
    property Required_ProductId: string read FRequProductId write FRequProductId;
    property Required_State: TPInstallationState read FRequState write FRequState;
    property Required_Action: TPActionRequest read FRequAction write FRequAction;
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
    Fpname: string;
    Fmultivalue: boolean;
    Feditable: boolean;
    Fdescription: string;
    //FStrvalues: TStrings;
    //FStrDefault: TStrings;
    FStrvaluesStr: string;
    FStrDefaultStr: string;
    FBoolDefault: boolean;
    //FimportMode : boolean;
    dummystr: string;
  protected
    // function GetDisplayName: string; override;
    // public
    //     procedure Assign(Source: TPersistent); override;
  published
    //procedure activateImportMode;
    procedure SetValueLines(const AValue: TStrings);
    procedure SetDefaultLines(const AValue: TStrings);
    procedure SetDefaultStr(const AValue: string);
    procedure SetValueStr(const AValue: string);
    function GetValueLines: TStrings;
    function GetDefaultLines: TStrings;
    function GetDefaultStr: string;
    function GetValueStr: string;
    property Property_Type: TPPtype read Ftype write Ftype;
    property Property_Name: string read Fpname write Fpname;
    property Description: string read Fdescription write Fdescription;
    property Multivalue: boolean read Fmultivalue write Fmultivalue;
    property Editable: boolean read Feditable write Feditable;
    //property StrDefault: TStrings read GetDefaultLines write SetDefaultLines;
    //property Strvalues: TStrings read GetValueLines write SetValueLines;
    property Default_Values: string read FStrDefaultStr write FStrDefaultStr;
    property Possible_Values: string read FStrvaluesStr write FStrvaluesStr;
    property BoolDefault: boolean read FBoolDefault write FBoolDefault;
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
    function propExists(propname: string): boolean;
    procedure propDelete(propname: string);
    //procedure activateImportMode;
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
    Fupdatescript: string;
    Flicenserequired: boolean;
    FproductImageFullFileName: string;
    FtargetOSset: TTargetOSset;
    FuseCustomDir: boolean;
    FchannelDir: string;
    FinstallFromLocal: boolean;
    FhandleLicensekey: boolean;
    Fdesktopicon: boolean;
    FcustomizeProfile: boolean;
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
    property updatescript: string read Fupdatescript write Fupdatescript;
    property licenserequired: boolean read Flicenserequired write Flicenserequired;
    property productImageFullFileName: string
      read FproductImageFullFileName write FproductImageFullFileName;
    property targetOSset: TTargetOSset read FtargetOSset write FtargetOSset;
    property useCustomDir: boolean read FuseCustomDir write FuseCustomDir;
    property channelDir: string read FchannelDir write FchannelDir;
    property installFromLocal: boolean read FinstallFromLocal write FinstallFromLocal;
    property handleLicensekey: boolean read FhandleLicensekey write FhandleLicensekey;
    property desktopicon: boolean read Fdesktopicon write Fdesktopicon;
    property customizeProfile: boolean read FcustomizeProfile write FcustomizeProfile;

  public
    { public declarations }
    //constructor Create;
    //destructor Destroy;
  end;

  TopsiProduct = class(TPersistent)
  private
  published
  public
    SetupFiles: array[0..2] of TSetupFile;
    productdata: TProductData;
    //dependeciesCount : integer;
    dependencies: TCollection;
    properties: TPProperties;

    { public declarations }
    constructor Create;
    procedure readProjectFile(filename: string);
    procedure writeProjectFileToPath(path: string);
    procedure writeProjectFileToFile(myfilename: string);
  end;

  TConfiguration = class(TPersistent)
  private
    Fconfig_version: string;
    { help to detect and handle changes of config file structure }
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
    FShowCheckEntryWarning: boolean;
    FShow2StepMacSeletionWarn: boolean;
    FService_URL: string;
    FService_user: string;
    FService_pass: string;
    FUseService: boolean;
    FTemplateChannel: TTemplateChannels;
    //FtargetOS : TTargetOS;
    procedure SetLibraryLines(const AValue: TStrings);
    procedure SetPreInstallLines(const AValue: TStrings);
    procedure SetPostInstallLines(const AValue: TStrings);
    procedure SetPreUninstallLines(const AValue: TStrings);
    procedure SetPostUninstallLines(const AValue: TStrings);
    procedure SetProperties(const AValue: TPProperties);
    procedure SetWorkbench_path(const AValue: string);
  published
    property config_version: string read Fconfig_version write Fconfig_version;
    //property workbench_share: string read Fworkbench_share write Fworkbench_share;
    property workbench_Path: string read Fworkbench_Path write SetWorkbench_path;
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
    property ShowCheckEntryWarning: boolean
      read FShowCheckEntryWarning write FShowCheckEntryWarning;
    property Show2StepMacSeletionWarn: boolean
      read FShow2StepMacSeletionWarn write FShow2StepMacSeletionWarn;
    property Service_URL: string read FService_URL write FService_URL;
    property Service_user: string read FService_user write FService_user;
    property Service_pass: string read FService_pass write FService_pass;
    property templateChannel: TtemplateChannels
      read FTemplateChannel write FTemplateChannel;
    //property UseService: boolean read FUseService write FUseService;

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
function installerstrToInstaller(installerstr: string): TKnownInstaller;
procedure initaktproduct;
procedure makeProperties;
procedure freebasedata;
procedure activateImportMode;
procedure deactivateImportMode;
function cleanOpsiId(opsiid: string): string; // clean up productId

const
  CONFVERSION = '4.2.0.9';
  templChannelStrings: array [TTemplateChannels] of string =
    ('training', 'default', 'structured', 'high_structured');

var
  aktProduct: TopsiProduct;
  aktSetupFile: TSetupFile;
  osdsettings: TOSDSettings;
  knownInstallerList: TStringList;
  architectureModeList: TStringList;
  templateChannelList: TStringList;
  installerArray: TInstallers;
  counter: integer;
  myconfiguration: TConfiguration;
  //useRunMode: TRunMode;
  myVersion: string;
  lfilename: string;
  aktconfigfile: string;
  forceProductId: string = ''; // by cli parameter
  forceTargetOS: TTargetOS = osWin; // by cli parameter
  globimportMode: boolean = False;
  defaultIconFullFileName: string;

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
    + LineEnding + 'One per line. May be empty. Example: ' + LineEnding +
    'comment "Start the installation ..."';
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
  rsCnfdTitle = 'Edit your configuration here.' + LineEnding +
    'Click on a line to get help ' + LineEnding +
    'in the yellow field at the bottom.';
  rsService_URL = 'The URL of the opsi webservice (like: https://<opsi-server>:4447)';
  rsService_user = 'The user name used to connect to the opsi webservice';
  rsService_pass = 'The password of the given user used to connect to the opsi webservice'
    + LineEnding + 'ATTENTION SECURITY RISK: Even it is stored encrypted,' +
    LineEnding + 'it is easy to decrypt be analyzing the source code.' +
    LineEnding + 'If empty you will be asked if the connection is started.';
  //rsUseService = 'Should this program try to connect to the opsi web service';
  rsConfigVersion = 'Do not change. Version of the config structure.';
  rsReadme_txt_templ = 'Path to the text file that is used as Read-me template';
  rsInternalSet = 'Do not change here - Internally set by a dialog.';
  rsUsePropDesktopicon =
    'Should we create a "DektopIcon" property and add code to handle dektop icons ?';
  rsPropDesktopiconDescription = 'Should there be a desktop icon ?';
  rsUsePropLicenseOrPool =
    'Should we create a "LicenseOrPool" property and add code handle license keys ?';
  rsPropLicenseOrPoolDescription = 'License key or opsi license pool';
  rsPropInstallFromLocalDescription =
    'Determines if the installation files will be copied locally';
  rsPropInstallArchDescription = 'Which architecture (32 / 64 Bit) has to be installed?';

    (*
  rscreateQuiet = 'Selects the Build mode Checkbox quiet.';
  rscreateBuild = 'Selects the Build mode Checkbox build.';
  rscreateInstall = 'Selects the Build mode Checkbox install.';
  *)


implementation

{$IFDEF OSDGUI}
uses
  osdform;

{$ENDIF OSDGUI}

var
  FileVerInfo: TFileVersionInfo;

// TInstallerData ************************************

constructor TInstallerData.Create;
begin
  patterns := TStringList.Create;
  infopatterns := TStringList.Create;
  installErrorHandlingLines := TStringList.Create;
  inherited;
end;

destructor TInstallerData.Destroy;
begin
  FreeAndNil(patterns);
  FreeAndNil(infopatterns);
  FreeAndNil(installErrorHandlingLines);
  inherited;
end;

// TSetupFile ************************************

constructor TSetupFile.Create;
begin
  Fmarkerlist := TStringList.Create;
  Finfolist := TStringList.Create;
  FuninstallCheck := TStringList.Create;
  FinstallErrorHandlingLines := TStringList.Create;
  inherited;
  //initValues;
end;

destructor TSetupFile.Destroy;
begin
  FreeAndNil(Fmarkerlist);
  FreeAndNil(Finfolist);
  FreeAndNil(FuninstallCheck);
  FreeAndNil(FinstallErrorHandlingLines);
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

procedure TSetupFile.SetInstallErrorHandlingLines(const AValue: TStrings);
begin
  FinstallErrorHandlingLines.Assign(AValue);
end;

procedure TSetupFile.initValues;
begin
  Factive := False;
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
  FtargetProg := '';
  FuninstallCheck.Clear;
  Fanalyze_progess := 0;
  FisExitcodeFatalFunction := 'isGenericExitcodeFatal';
  Funinstall_waitforprocess := '';
  Finstall_waitforprocess := '';
  FcopyCompleteDir := False;
  FinstallErrorHandlingLines.Clear;
  FmsiProductName := '';
  FinstallerSourceDir := '';
end;

// TPProperty **********************************

constructor TPProperty.Create;
begin
  inherited;
  init;
end;

procedure TPProperty.init;
begin
  Fpname := '';
  Fdescription := '';
  //FStrvalues := TStringList.Create;
  //FStrdefault := TStringList.Create;
  Ftype := bool;
  Fmultivalue := False;
  Feditable := False;
  FBoolDefault := False;
end;


destructor TPProperty.Destroy;
begin
  //FreeAndNil(FStrvalues);
  //FreeAndNil(FStrdefault);
  inherited;
end;

(*
procedure TPProperty.SetValueLines(const AValue: TStrings);
var
  tmpstr : string;
begin
  if Assigned(AValue) then
  //FStrvalues.Assign(AValue);
    FStrvalues.SetStrings(AValue);
  if not stringListToJsonArray(TStringlist(AValue),tmpstr) then
   logdatei.log('Could not convert stringlist to json array',LLerror)
   else FStrvaluesStr := tmpstr;
end;

procedure TPProperty.SetDefaultLines(const AValue: TStrings);
begin
  if Assigned(AValue) then
  //FStrDefault.Assign(AValue);
   FStrDefault.Text:= AValue.Text;
  if not stringListToJsonArray(TStringlist(AValue),FStrDefaultStr) then
    logdatei.log('Could not convert stringlist to json array',LLerror);
end;

function TPProperty.GetValueLines: TStringlist;
var
  i : integer;
  tmpstr : string;
begin
  result := TStringlist.Create;
  //result.Clear;
  //result.SetStrings(FStrvalues);
  for i := 0 to FStrvalues.Count-1 do
  begin
    tmpstr := FStrvalues[i];
    result.Add(tmpstr);
  end;
end;


function TPProperty.GetDefaultLines: TStrings;
begin
  result := TStrings.Create;
  result.SetStrings(FStrDefault);
end;
*)


// Defaults
function TPProperty.GetDefaultLines: TStrings;
begin
  Result := TStringList.Create;
        (*
  if globimportMode then
    begin
      // return empty
    end
  else
  *)
  Result.DelimitedText := FStrDefaultStr;
end;

procedure TPProperty.SetDefaultLines(const AValue: TStrings);
begin
  (*
   if globimportMode then
    begin
      // do nothing
    end
  else
  *)
  FStrDefaultStr := AValue.DelimitedText;

end;

procedure TPProperty.SetDefaultStr(const AValue: string);
begin
  FStrDefaultStr := AValue;
  //FStrDefault.DelimitedText := AValue;
end;

function TPProperty.GetDefaultStr: string;
begin
  Result := FStrDefaultStr;
end;

//Values

function TPProperty.GetValueLines: TStrings;
begin
  Result := TStringList.Create;
  (*
  if globimportMode then
    begin
      // return empty
    end
  else
  *)
  Result.DelimitedText := FStrvaluesStr;
end;

procedure TPProperty.SetValueLines(const AValue: TStrings);
begin
  (*
  if globimportMode then
    begin
      // do nothing
    end
  else
  begin
  *)
  //FStrvalues.SetStrings(AValue);
  FStrvaluesStr := AValue.DelimitedText;
end;

procedure TPProperty.SetValueStr(const AValue: string);
begin
  FStrvaluesStr := AValue;
  //FStrvalues.DelimitedText := AValue;
end;

function TPProperty.GetValueStr: string;
begin
  Result := FStrvaluesStr;
end;



procedure activateImportMode;
begin
  globimportMode := True;
end;

procedure deactivateImportMode;
begin
  globimportMode := False;
end;

(*
function TPProperty.GetDisplayName: string;
begin
  Result := Fpname;
end;
*)

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

function TPProperties.propExists(propname: string): boolean;
var
  index, i: integer;
  tmpstr: string;
begin
  index := Count;
  tmpstr := lowercase(propname);
  propExists := False;
  for i := 0 to index - 1 do
    if lowercase(tmpstr) = lowercase(Items[i].Property_Name) then
      propExists := True;
end;

procedure TPProperties.propDelete(propname: string);
var
  index, i, delindex, numberItems: integer;
  tmpstr: string;
begin
  delindex := -1;
  index := Count;
  tmpstr := lowercase(propname);
  for i := index - 1 downto 0 do
    if lowercase(tmpstr) = lowercase(Items[i].Property_Name) then
    begin
      delindex := i;
      (*
      // deleting the first entry leads to an access violation
    // to avoid this we move the item to delete to the end of the collection
    // then we have to sync with the grid
    // now we delete the last element
    // and now we can resync without access violation
      Exchange(i , count-1);
      Application.ProcessMessages;
      Delete(count-1);
      Application.ProcessMessages;
      *)
    end;

  // now let us delete with soft handling of the GUI components
  // this is (i think) a bug workaround
  if delindex >= 0 then
  begin
    numberItems := aktProduct.properties.Count;
    // deleting the first entry leads to an access violation
    // to avoid this we move the item to delete to the end of the collection
    aktProduct.properties.Exchange(delindex, numberItems - 1);
  {$IFDEF OSDGUI}
    // then we have to sync with the grid
    resultForm1.TIGridProp.ReloadTIList;
  {$ENDIF OSDGUI}
    // now we delete the last element
    aktProduct.properties.Delete(numberItems - 1);
  {$IFDEF OSDGUI}
    // and now we can resync without access violation
    resultForm1.TIGridProp.ReloadTIList;
    resultForm1.TIGridProp.Update;
  {$ENDIF OSDGUI}
  end;

end;

(*
procedure TPProperties.activateImportMode;
var
  i : integer;
begin
  for i := 0 to count - 1 do
    ITems[i].activateImportMode;
end;
*)
procedure makeProperties;
var
  //myprop: TStringList;
  myprop: TPProperty;
  index, i: integer;
  propexists: boolean;
  tmpstrlist: TStringList;
  myrunmode: TRunMode;
begin
  myrunmode := osdsettings.runmode;
  (*
  // clear existing props in StringGridProp
  StringGridProp.Clean([gzNormal, gzFixedRows]);
  StringGridProp.RowCount := 1;

  if myconfiguration.UsePropDesktopicon and
    (StringGridProp.Cols[1].IndexOf('DesktopIcon') = -1) then
  begin
    index := StringGridProp.RowCount;
    //Inc(index);
    //StringGridProp.InsertColRow(false,index);
    //StringGridProp.RowCount := index;
    myprop := TStringList.Create;
    myprop.Add(IntToStr(index));
    myprop.Add('DesktopIcon');
    myprop.Add('Soll es ein Desktop Icon geben ?');
    myprop.Add('bool');  //type
    myprop.Add('False');      //multivalue
    myprop.Add('False');      //editable
    myprop.Add('[]');      //possible values
    myprop.Add('False');      //default values
    //StringGridProp.InsertRowWithValues(index,myprop);
    StringGridProp.InsertColRow(False, index);
    StringGridProp.Rows[index].Clear;
    StringGridProp.Rows[index].SetStrings(myprop);
    myprop.Free;
    *)

  propexists := aktProduct.properties.propExists('DesktopIcon');
  if (aktProduct.productdata.desktopicon) and
    not propexists then
  begin
    myprop := TPProperty(aktProduct.properties.add);
    myprop.init;
    myprop.Property_Name := lowercase('DesktopIcon');
    myprop.description := rsPropDesktopiconDescription;
    myprop.Property_Type := bool;
    myprop.multivalue := False;
    myprop.editable := False;
    //tmpstrlist := TStringList.Create;
    //myprop.SetValueLines(tmpstrlist);
    //myprop.SetDefaultLines(tmpstrlist);
    //FreeAndNil(tmpstrlist);
    myprop.boolDefault := False;
  end
  else if propexists and not (aktProduct.productdata.desktopicon) then
    aktProduct.properties.propDelete('DesktopIcon');


  propexists := aktProduct.properties.propExists('SecretLicense_or_Pool');
  if ((myconfiguration.UsePropLicenseOrPool and
    aktProduct.productdata.licenserequired) or
    aktProduct.productdata.handleLicensekey) and not propexists then
  begin
    myprop := TPProperty(aktProduct.properties.add);
    myprop.init;
    myprop.Property_Name := lowercase('SecretLicense_or_Pool');
    myprop.description := rsPropLicenseOrPoolDescription;
    myprop.Property_Type := unicode;
    myprop.multivalue := False;
    myprop.editable := True;
    tmpstrlist := TStringList.Create;
    myprop.SetValueLines(tmpstrlist);
    myprop.SetDefaultLines(tmpstrlist);
    FreeAndNil(tmpstrlist);
    myprop.boolDefault := False;
  end
  else if propexists and not ((myconfiguration.UsePropLicenseOrPool and
    aktProduct.productdata.licenserequired) or
    aktProduct.productdata.handleLicensekey) then
    aktProduct.properties.propDelete('SecretLicense_or_Pool');

  propexists := aktProduct.properties.propExists('Install_from_local_tmpdir');
  if aktProduct.productdata.installFromLocal and not propexists then
  begin
    myprop := TPProperty(aktProduct.properties.add);
    myprop.init;
    myprop.Property_Name := lowercase('Install_from_local_tmpdir');
    myprop.description := rsPropInstallFromLocalDescription;
    myprop.Property_Type := bool;
    myprop.multivalue := False;
    myprop.editable := False;
    myprop.boolDefault := False;
  end
  else if propexists and not aktProduct.productdata.installFromLocal then
    aktProduct.properties.propDelete('Install_from_local_tmpdir');

  propexists := aktProduct.properties.propExists('install_architecture');
  if ((myrunmode = twoAnalyzeCreate_1) or (myrunmode = twoAnalyzeCreate_2)) and
    not propexists then
  begin
    myprop := TPProperty(aktProduct.properties.add);
    myprop.init;
    myprop.Property_Name := lowercase('install_architecture');
    myprop.description := rsPropInstallArchDescription;
    myprop.Property_Type := unicode;
    myprop.multivalue := False;
    myprop.editable := False;
    tmpstrlist := TStringList.Create;
    tmpstrlist.Add('32 only');
    tmpstrlist.Add('64 only');
    tmpstrlist.Add('system specific');
    tmpstrlist.Add('both');
    myprop.SetValueLines(TStrings(tmpstrlist));
    tmpstrlist.Clear;
    tmpstrlist.Add('system specific');
    myprop.SetDefaultLines(TStrings(tmpstrlist));
    FreeAndNil(tmpstrlist);
    myprop.boolDefault := False;
  end
  else if propexists and not ((myrunmode = twoAnalyzeCreate_1) or
    (myrunmode = twoAnalyzeCreate_2)) then
    aktProduct.properties.propDelete('install_architecture');

  // start 'with-user' properties

  propexists := aktProduct.properties.propExists('copy_files_locally');
  if (myrunmode = analyzeCreateWithUser) and not propexists then
  begin
    myprop := TPProperty(aktProduct.properties.add);
    myprop.init;
    myprop.Property_Name := lowercase('copy_files_locally');
    myprop.description := 'Determines if the installation files will be copied locally';
    myprop.Property_Type := bool;
    myprop.multivalue := False;
    myprop.editable := False;
    myprop.boolDefault := False;
  end;

  propexists := aktProduct.properties.propExists('debug');
  if (myrunmode = analyzeCreateWithUser) and not propexists then
  begin
    myprop := TPProperty(aktProduct.properties.add);
    myprop.init;
    myprop.Property_Name := lowercase('debug');
    myprop.description :=
      'Enables keyboard and mouse input during auto logon and logs passwords if set to true';
    myprop.Property_Type := bool;
    myprop.multivalue := False;
    myprop.editable := False;
    myprop.boolDefault := False;
  end;

  propexists := aktProduct.properties.propExists('uninstall_before_install');
  if ((myrunmode = analyzeCreateWithUser) or
    (aktProduct.productdata.channelDir = 'structured')) and not propexists then
  begin
    myprop := TPProperty(aktProduct.properties.add);
    myprop.init;
    myprop.Property_Name := lowercase('uninstall_before_install');
    myprop.description := 'Uninstall previous versions before installation';
    myprop.Property_Type := bool;
    myprop.multivalue := False;
    myprop.editable := False;
    myprop.boolDefault := True;
  end;


  propexists := aktProduct.properties.propExists('execution_method');
  if (myrunmode = analyzeCreateWithUser) and not propexists then
  begin
    myprop := TPProperty(aktProduct.properties.add);
    myprop.init;
    myprop.Property_Name := lowercase('execution_method');
    myprop.description :=
      'Execute the local installation by using local opsi-script, or the event_starter';
    myprop.Property_Type := unicode;
    myprop.multivalue := False;
    myprop.editable := False;
    tmpstrlist := TStringList.Create;
    tmpstrlist.Add('loginOpsiSetupUser');
    tmpstrlist.Add('runAsOpsiSetupUser');
    tmpstrlist.Add('runOpsiScriptAsOpsiSetupUser');
    myprop.SetValueLines(TStrings(tmpstrlist));
    tmpstrlist.Clear;
    tmpstrlist.Add('runOpsiScriptAsOpsiSetupUser');
    myprop.SetDefaultLines(TStrings(tmpstrlist));
    FreeAndNil(tmpstrlist);
    myprop.boolDefault := False;
  end;

  // END 'with-user' properties
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

procedure TopsiProduct.writeProjectFileToPath(path: string);
begin
  path := IncludeTrailingPathDelimiter(path);
  writeProjectFileToFile(path + 'opsi-project.osd');
end;

procedure TopsiProduct.writeProjectFileToFile(myfilename: string);
var
  Streamer: TJSONStreamer;
  JSONString: string;
  //myfilename: string;
  configDir: array[0..MaxPathLen] of char; //Allocate memory
  configDirUtf8: utf8string;
  pfile: TextFile;

  (*
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
  *)

begin
  try
    if Assigned(logdatei) then
      logdatei.log('Start write project file', LLDebug);
    // project file name
    myfilename := ExpandFileName(myfilename);
    configdir := ExtractFileDir(myfilename);
    configDir := IncludeTrailingPathDelimiter(configdir);
    //myfilename := configDir + 'opsi-project.osd';
    if Assigned(logdatei) then
      logdatei.log('write project file to: ' + myfilename, LLDebug);
    if not DirectoryExists(configDir) then
      if not ForceDirectories(configDir) then
        if Assigned(logdatei) then
          LogDatei.log('failed to create project file directory: ' +
            configDir, LLError);
    AssignFile(pfile, myfilename);
    Rewrite(pfile);
    // http://wiki.freepascal.org/Streaming_JSON
    Streamer := TJSONStreamer.Create(nil);
    try
      //Streamer.Options := Streamer.Options + [jsoTStringsAsArray];
      // Save strings as JSON array
      // JSON convert and output
      JSONString := Streamer.ObjectToJSONString(osdsettings);
      writeln(pfile, JSONString);
      JSONString := Streamer.ObjectToJSONString(aktProduct.SetupFiles[0]);
      writeln(pfile, JSONString);
      JSONString := Streamer.ObjectToJSONString(aktProduct.SetupFiles[1]);
      writeln(pfile, JSONString);
      JSONString := Streamer.ObjectToJSONString(aktProduct.SetupFiles[2]);
      writeln(pfile, JSONString);
      JSONString := Streamer.ObjectToJSONString(aktProduct.productdata);
      writeln(pfile, JSONString);
      activateImportMode;
      // has to be fixed: JSONString := Streamer.ObjectToJSONString(aktProduct.properties);
      //JSONString := Streamer.CollectionToJSON(aktProduct.properties);
      JSONString := Streamer.ObjectToJSONString(aktProduct.properties);
      writeln(pfile, JSONString);
      deactivateImportMode;
      JSONString := Streamer.ObjectToJSONString(aktProduct.dependencies);
      //JSONString := Streamer.CollectionToJSON(aktProduct.dependencies);
      writeln(pfile, JSONString);
      //writeln(pfile,aktProduct.FtargetOS);
      CloseFile(pfile);
      (*
      logdatei.log('Config: ' + JSONString, LLDebug);
      if not SaveStringToFile(JSONString, myfilename) then
        if Assigned(logdatei) then
          LogDatei.log('failed write project file', LLError);
      *)

    finally
      Streamer.Destroy;
    end;
    if Assigned(logdatei) then
      logdatei.log('Finished write project file', LLDebug2);

  except
    on E: Exception do
      if Assigned(logdatei) then
        LogDatei.log('project file could not be written. Details: ' +
          E.ClassName + ': ' + E.Message, LLError);
  end;
end;


(*
procedure TSetupFile.OnRestoreProperty(Sender: TObject; AObject: TObject;
  Info: PPropInfo; AValue: TJSONData; var Handled: Boolean);
begin
  Handled := False;
  if (Info^.Name = 'setupFullFileName') then
  begin
    Handled := True;
    SetSetupFullFileName(AValue.AsString);
  end;
  if (Info^.Name = 'setupFileNamePath') then
    Handled := True;
end;
*)

procedure TopsiProduct.readProjectFile(filename: string);
var
  DeStreamer: TJSONDeStreamer;
  //Streamer: TJSONStreamer;
  JSONString, JSONObjString: string;
  myfilename: string;
  configDir: array[0..MaxPathLen] of char; //Allocate memory
  configDirUtf8: utf8string;
  configDirstr: string;
  oldconfigDir, oldconfigFileName, tmpstr: string;
  pfile: TextFile;
  aktproperty: TPProperty;
  i: integer;

  (*
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
          ShowMessage('readProjectFile: String could not be read. Details: ' +
            E.ClassName + ': ' + E.Message);
    end;
  end;
  *)

begin
  try
    if Assigned(logdatei) then
      logdatei.log('Start readProjectFile', LLDebug);
    // project file name
    //configDir := IncludeTrailingPathDelimiter(path);
    //myfilename := configDir + 'opsi-project.osd';
    myfilename := filename;
    myfilename := ExpandFileName(myfilename);
    if Assigned(logdatei) then
      logdatei.log('readconfig from: ' + myfilename, LLDebug);
    if FileExists(myfilename) then
    begin
      AssignFile(pfile, myfilename);
      Reset(pfile);
      //// http://wiki.freepascal.org/Streaming_JSON
      // DeStreamer object create
      DeStreamer := TJSONDeStreamer.Create(nil);
      try
        DeStreamer.Options := [jdoIgnorePropertyErrors, jdoIgnoreNulls];
        // Load JSON data in the object
        //DeStreamer.OnRestoreProperty:=SetupFiles[0].OnRestoreProperty;
        readln(pfile, JSONString);
        DeStreamer.JSONToObject(JSONString, osdsettings);
        readln(pfile, JSONString);
        DeStreamer.JSONToObject(JSONString, aktProduct.SetupFiles[0]);
        readln(pfile, JSONString);
        DeStreamer.JSONToObject(JSONString, aktProduct.SetupFiles[1]);
        readln(pfile, JSONString);
        DeStreamer.JSONToObject(JSONString, aktProduct.SetupFiles[2]);
        readln(pfile, JSONString);
        DeStreamer.JSONToObject(JSONString, aktProduct.productdata);
        readln(pfile, JSONString);
        activateImportMode;
        if pos('gdb unparsed remainder', JSONString) = 0 then
          if jsonIsValid(JSONString) then
          begin
            jsonAsObjectGetValueByKey(JSONString, 'Items', JSONString);
            if jsonIsArray(JSONString) then
            begin
              aktproperty := TPProperty.Create;
              for i := 0 to jsonAsArrayCountElements(JSONString) - 1 do
              begin
                jsonAsArrayGetElementByIndex(JSONString, i, JSONObjString);
                aktproperty := aktProduct.properties.Add;
                DeStreamer.JSONToObject(JSONObjString, aktproperty);

              end;
            end;
            //DeStreamer.JSONToObject(JSONString, aktProduct.properties);
            //DeStreamer.JSONToCollection(JSONString, aktProduct.properties);
          end;
        deactivateImportMode;
        readln(pfile, JSONString);
        DeStreamer.JSONToObject(JSONString, aktProduct.dependencies);
        //DeStreamer.JSONToCollection(JSONString, aktProduct.dependencies);
        // Cleanup
      finally
        DeStreamer.Destroy;
        CloseFile(pfile);
      end;
      if Assigned(logdatei) then
        logdatei.log('read config: ' + JSONString, LLDebug)
      else
        ShowMessage('read config: ' + JSONString);
    end
    else
    begin
      if Assigned(logdatei) then
        logdatei.log('Project file not found: ' + myfilename, LLError);
    end;
    if Assigned(logdatei) then
      logdatei.log('Finished readconfig', LLDebug2);

  except
    on E: Exception do
      if Assigned(logdatei) then
      begin
        LogDatei.log('read project exception. Details: ' + E.ClassName +
          ': ' + E.Message, LLError);
        ShowMessage('Read project file exception. Details: ' +
          E.ClassName + ': ' + E.Message);
      end
      else
        ShowMessage('Read project file exception. Details: ' +
          E.ClassName + ': ' + E.Message);
  end;
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
    'template-files' + PathDelim + 'generic' + PathDelim + 'package_qa.txt';
  FShowCheckEntryWarning := True;
  FShow2StepMacSeletionWarn := True;
  FUsePropDesktopicon := False;
  FTemplateChannel := default;
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

procedure TConfiguration.SetWorkbench_path(const AValue: string);
begin
  Fworkbench_Path := IncludeTrailingPathDelimiter(AValue);
end;

procedure TConfiguration.writeconfig;
var
  Streamer: TJSONStreamer;
  JSONString: string;
  myfilename: string;
  configDir: array[0..MaxPathLen] of char; //Allocate memory
  configDirUtf8: utf8string;
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
    Fconfig_version := CONFVERSION;

    FService_pass := encryptStringBlow('opsi-setup-detector' +
      FService_user, FService_pass);
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
      FService_pass := decryptStringBlow('opsi-setup-detector' +
        FService_user, FService_pass);
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
  configDirUtf8: utf8string;
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
      aktconfigfile := myfilename;
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
        FService_pass := decryptStringBlow('opsi-setup-detector' +
          FService_user, FService_pass);
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
        LogDatei.log('read config exception. Details: ' + E.ClassName +
          ': ' + E.Message, LLError)
      else
        ShowMessage('read config exception. Details: ' + E.ClassName +
          ': ' + E.Message);
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

function installerstrToInstaller(installerstr: string): TKnownInstaller;
begin
  Result := TKnownInstaller(knownInstallerList.IndexOf(installerstr));
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
  str: string;
  //newdep: TPDependency;
  //defaultIconFullFileName :string;
begin
  LogDatei.log('Start initaktproduct ... ', LLInfo);
  i := 0;
  for i := 0 to 2 do
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
    productversion := '1.0.0';
    packageversion := 1;
    versionstr := '';
    priority := 0;
    producttype := 'localboot';
    setupscript := 'setup.opsiscript';
    uninstallscript := 'uninstall.opsiscript';
    delsubscript := 'delsub.opsiscript';
    //channelDir:= 'default';
    str := templChannelStrings[myconfiguration.templateChannel];
    channelDir := str;
    licenserequired := False;
    // Application.Params[0] is directory of application as string
    { set productImageFullFileName to full file name of the default icon }
    {$IFDEF WINDOWS}
    defaultIconFullFileName :=
      ExtractFileDir(Application.Params[0]) + PathDelim + 'template-files' +
      PathDelim + 'default' + PathDelim + 'images' + PathDelim + 'template.png';
    {$ENDIF WINDOWS}
    {$IFDEF UNIX}
    defaultIconFullFileName :=
      '/usr/share/opsi-setup-detector' + PathDelim + 'template-files' +
      PathDelim + 'default' + PathDelim + 'images' + PathDelim + 'template.png';
    // in develop environment
    if not fileexists(defaultIconFullFileName) then
      defaultIconFullFileName :=
        ExtractFileDir(Application.Params[0]) + PathDelim + 'template-files' +
        PathDelim + 'default' + PathDelim + 'images' + PathDelim + 'template.png';
    {$ENDIF UNIX}
    osdbasedata.aktProduct.productdata.productImageFullFileName :=
      defaultIconFullFileName;
    (*
    productImageFullFileName :=
      ExtractFileDir(Application.Params[0]) + PathDelim + 'template-files' +
      PathDelim + 'template.png';
    *)
    targetOSset := [];

    useCustomDir := False;
    installFromLocal := False;
    if myconfiguration.UsePropLicenseOrPool then
      handleLicensekey := True
    else
      handleLicensekey := False;
    if myconfiguration.UsePropDesktopicon then
      desktopicon := True
    else
      desktopicon := False;
    customizeProfile := False;
  end;
  // Create Dependencies
  aktProduct.dependencies := TCollection.Create(TPDependency);
  // Create Properties
  aktProduct.properties := TPProperties.Create(aktProduct);
  //aktProduct.targetOS:= osWin;
end;

procedure freebasedata;
var
  i: integer;
begin
  for i := 0 to 2 do
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

function cleanOpsiId(opsiId: string): string; // clean up productId
begin
  opsiId := LowerCase(opsiId);
  (*
  opsiId := StringReplace(opsiId,' '.'_',[frReplaceAll]);
  opsiId := Stringsreplace(opsiId,[' ','/','\','"','''',':',],
                                  ['_','_','_','_','_', '_','_',],
                                  [frReplaceAll]);
                                  *)
  Result := stringReplaceRegex(opsiId, '[^a-z0-9_-]', '_');

  // [^A-Za-z0-9._-]
end;


//initialize unit

begin
  osdsettings := Tosdsettings.Create;

  // marker for add installers
  knownInstallerList := TStringList.Create;
  knownInstallerList.Add('LinRPM');
  knownInstallerList.Add('LinDeb');
  knownInstallerList.Add('MacZip');
  knownInstallerList.Add('MacDmg');
  knownInstallerList.Add('MacPKG');
  knownInstallerList.Add('MacApp');
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
      'https://docs.revenera.com/installshield21helplib/helplibrary/IHelpSetup_EXECmdLine.htm';
    // 'https://www.ibm.com/docs/en/personal-communications/12.0?topic=guide-installshield-command-line-parameters'
    // 'https://www.itninja.com/static/090770319967727eb89b428d77dcac07.pdf'
    // broken: 'http://helpnet.flexerasoftware.com/installshield19helplib/helplibrary/IHelpSetup_EXECmdLine.htm';
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
      '/l* "%opsiLogDir%\$ProductId$.install_log.txt" /qn ALLUSERS=1 REBOOT=ReallySuppress';
    unattendedsetup :=
      '/l* "%opsiLogDir%\$ProductId$.install_log.txt" /qb-! ALLUSERS=1 REBOOT=ReallySuppress';
    silentuninstall :=
      '/qn REBOOT=ReallySuppress';
    unattendeduninstall :=
      '/qb-! REBOOT=ReallySuppress';
    uninstall_waitforprocess := '';
    uninstallProg := '';
    //patterns.Add('nstallshield');
    //patterns.Add('installer,msi,database');
    installErrorHandlingLines.Add(
      'includelog "%opsiLogDir%\"+$ProductId$+".install_log.txt" "50" "utf16le"');
    link :=
      'https://docs.microsoft.com/de-de/windows-server/administration/windows-commands/msiexec';
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
    silentsetup := '/s /l="%opsiLogDir%\$ProductId$.install_log.txt"';
    unattendedsetup := '/s /l="%opsiLogDir%\$ProductId$.install_log.txt"';
    silentuninstall := '/s MODIFY=FALSE REMOVE=TRUE UNINSTALL=YES';
    unattendeduninstall := '/s MODIFY=FALSE REMOVE=TRUE UNINSTALL=YES';
    uninstall_waitforprocess := '';
    install_waitforprocess := '';
    uninstallProg := '';
    patterns.Add('InstallAware: http://www.installaware.com');
    infopatterns.Add('RunProgram="');
    installErrorHandlingLines.Add(
      'includelog "%opsiLogDir%\"+$ProductId$+".install_log.txt" "50" "utf16le"');
    link := 'https://www.installaware.com/mhtml5/desktop/setupcommandlineparameters.htm';
    comment := '';
    uib_exitcode_function := 'isMsExitcodeFatal_short';
    detected := @detectedbypatternwithor;
  end;
  // stMSGenericInstaller
  with installerArray[integer(stMSGenericInstaller)] do
  begin
    description := 'generic MS Installer';
    silentsetup := '/quiet /norestart /log "%opsiLogDir%\$ProductId$.install_log.txt"';
    unattendedsetup :=
      '/passive /norestart /log "%opsiLogDir%\$ProductId$.install_log.txt"';
    silentuninstall := '/quiet /norestart /log "%opsiLogDir%\$ProductId$.install_log.txt"';
    unattendeduninstall :=
      '/passive /norestart /log "%opsiLogDir%\$ProductId$.install_log.txt"';
    uninstall_waitforprocess := '';
    install_waitforprocess := '';
    uninstallProg := '';
    patterns.Add('layoutpromptrestartforcerestartnorestartpassivesilentsquietqhelph');
    //infopatterns.Add('RunProgram="');
    installErrorHandlingLines.Add(
      'includelog "%opsiLogDir%\"+$ProductId$+".install_log.txt" "50" "utf16le"');
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
    silentsetup := '/quiet /norestart /log "%opsiLogDir%\$ProductId$.install_log.txt"';
    unattendedsetup :=
      '/passive /norestart /log "%opsiLogDir%\$ProductId$.install_log.txt"';
    silentuninstall := '/quiet /norestart /log "%opsiLogDir%\$ProductId$.install_log.txt"';
    unattendeduninstall :=
      '/passive /norestart /log "%opsiLogDir%\$ProductId$.install_log.txt"';
    uninstall_waitforprocess := '';
    install_waitforprocess := '';
    uninstallProg := '';
    patterns.Add('WixBundle');
    patterns.Add('Wix Toolset');
    //infopatterns.Add('RunProgram="');
    installErrorHandlingLines.Add(
      'includelog "%opsiLogDir%\"+$ProductId$+".install_log.txt" "50" "utf16le"');
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
    silentsetup := '/quiet /norestart /log "%opsiLogDir%\$ProductId$.install_log.txt"';
    unattendedsetup :=
      '/passive /norestart /log "%opsiLogDir%\$ProductId$.install_log.txt"';
    silentuninstall := '/quiet /norestart /log "%opsiLogDir%\$ProductId$.install_log.txt"';
    unattendeduninstall :=
      '/passive /norestart /log "%opsiLogDir%\$ProductId$.install_log.txt"';
    uninstall_waitforprocess := '';
    install_waitforprocess := '';
    uninstallProg := '';
    patterns.Add('boxstub.exe');
    //patterns.Add('Wix Toolset');
    //infopatterns.Add('RunProgram="');
    installErrorHandlingLines.Add(
      'includelog "%opsiLogDir%\"+$ProductId$+".install_log.txt" "50" "utf16le"');
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
    silentsetup := '/quiet /norestart /log "%opsiLogDir%\$ProductId$.install_log.txt"';
    unattendedsetup :=
      '/passive /norestart /log "%opsiLogDir%\$ProductId$.install_log.txt"';
    silentuninstall := '/quiet /norestart /log "%opsiLogDir%\$ProductId$.install_log.txt"';
    unattendeduninstall :=
      '/passive /norestart /log "%opsiLogDir%\$ProductId$.install_log.txt"';
    uninstall_waitforprocess := '';
    install_waitforprocess := '';
    uninstallProg := '';
    patterns.Add('sfxcab.exe');
    //patterns.Add('Wix Toolset');
    //infopatterns.Add('RunProgram="');
    installErrorHandlingLines.Add(
      'includelog "%opsiLogDir%\"+$ProductId$+".install_log.txt" "50" "utf16le"');
    link :=
      'https://docs.microsoft.com/en-us/windows/desktop/msi/standard-installer-command-line-options';
    comment := '';
    uib_exitcode_function := 'isMsExitcodeFatal_short';
    detected := @detectedbypatternwithand;
  end;
  // stBitrock
  with installerArray[integer(stBitrock)] do
  begin
    description := 'Bitrock installer / VMWare installbuilder';
    silentsetup := '--mode unattended --unattendedmodeui none';
    unattendedsetup := '--mode unattended --unattendedmodeui minimal';
    silentuninstall := '--mode unattended --unattendedmodeui none';
    unattendeduninstall := '--mode unattended --unattendedmodeui minimal';
    uninstall_waitforprocess := '';
    install_waitforprocess := '';
    uninstallProg := 'uninstall.exe';
    patterns.Add('<description>BitRock Installer</description>');
    patterns.Add('bitrock-lzma');
    //infopatterns.Add('RunProgram="');
    installErrorHandlingLines.Add(
      'includelog "%opsiLogDir%\"+$ProductId$+".install_log.txt" "50" "utf16le"');
    link :=
      'https://clients.bitrock.com/installbuilder/docs/installbuilder-userguide/ar01s08.html#_help_menu';
    comment := '';
    uib_exitcode_function := 'isGenericExitcodeFatal';
    detected := @detectedbypatternwithor;
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
  with installerArray[integer(stMacZip)] do
  begin
    description := 'MacOS installer packed in zip file';
    silentsetup := '';
    unattendedsetup := '';
    silentuninstall := '';
    unattendeduninstall := '';
    uninstall_waitforprocess := '';
    install_waitforprocess := '';
    uninstallProg := '<none>';
    patterns.Add('');
    link := '';
    comment := 'Unknown Vendor';
    uib_exitcode_function := '';
    detected := @detectedbypatternwithand;
  end;
  with installerArray[integer(stMacDmg)] do
  begin
    description := 'MacOS installer packed in dmg file';
    silentsetup := '';
    unattendedsetup := '';
    silentuninstall := '';
    unattendeduninstall := '';
    uninstall_waitforprocess := '';
    install_waitforprocess := '';
    uninstallProg := '<none>';
    patterns.Add('');
    link := '';
    comment := 'Unknown Vendor';
    uib_exitcode_function := '';
    detected := @detectedbypatternwithand;
  end;
  with installerArray[integer(stMacPKG)] do
  begin
    description := 'MacOS installer (pkg)';
    silentsetup := '';
    unattendedsetup := '';
    silentuninstall := '';
    unattendeduninstall := '';
    uninstall_waitforprocess := '';
    install_waitforprocess := '';
    uninstallProg := '<none>';
    patterns.Add('');
    link := '';
    comment := 'Unknown Vendor';
    uib_exitcode_function := '';
    detected := @detectedbypatternwithand;
  end;
  with installerArray[integer(stMacApp)] do
  begin
    description := 'MacOS App (unpacked directory)';
    silentsetup := '';
    unattendedsetup := '';
    silentuninstall := '';
    unattendeduninstall := '';
    uninstall_waitforprocess := '';
    install_waitforprocess := '';
    uninstallProg := '<none>';
    patterns.Add('');
    link := '';
    comment := 'Unknown Vendor';
    uib_exitcode_function := '';
    detected := @detectedbypatternwithand;
  end;
  with installerArray[integer(stLinRPM)] do
  begin
    description := 'Linux RPM package';
    silentsetup := '';
    unattendedsetup := '';
    silentuninstall := '';
    unattendeduninstall := '';
    uninstall_waitforprocess := '';
    install_waitforprocess := '';
    uninstallProg := '<none>';
    patterns.Add('');
    link := '';
    comment := 'Unknown Vendor';
    uib_exitcode_function := 'isGenericExitcodeFatal';
    detected := @detectedbypatternwithand;
  end;
  with installerArray[integer(stLinDeb)] do
  begin
    description := 'Linux Debian Package';
    silentsetup := '';
    unattendedsetup := '';
    silentuninstall := '';
    unattendeduninstall := '';
    uninstall_waitforprocess := '';
    install_waitforprocess := '';
    uninstallProg := '<none>';
    patterns.Add('');
    link := '';
    comment := 'Unknown Vendor';
    uib_exitcode_function := 'isGenericExitcodeFatal';
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


  // initalize channel names
  templateChannelList := TStringList.Create;
  templateChannelList.Add('training');
  templateChannelList.Add('default');
  templateChannelList.Add('structured');
  templateChannelList.Add('custom');

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
  LogDatei.log('Finished initialize basedata ', LLInfo);
end.
