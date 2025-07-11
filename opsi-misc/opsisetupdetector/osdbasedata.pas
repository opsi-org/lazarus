unit osdbasedata;

{$mode delphi}

interface

uses
  Classes, SysUtils, LCLProc, LResources, TypInfo, Forms, Controls, Graphics,
  Dialogs,
  strutils,
  {$IFDEF WINDOWS}
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
  oscrypt,
  osparserhelper,
  oswebservice,
  osdhelper,
  osdmeta;

type

  TRunMode = (
    analyzeOnly,         // only analyze installer
    singleAnalyzeCreate, // analyze one installer and create opsi package
    twoAnalyzeCreate_1,  // analyze two installers and create opsi package (part win32)
    twoAnalyzeCreate_2,  // analyze two installers and create opsi package (part win64)
    createTemplate,      // create standard template
    threeAnalyzeCreate_1,// analyze multi installers and create multi opsi package (part win)
    threeAnalyzeCreate_2,// analyze multi installers and create multi opsi package (part lin)
    threeAnalyzeCreate_3,// analyze multi installers and create multi opsi package (part mac)
    createMultiTemplate, // create multi platform template
    createMeta,          // create meta packeage
    analyzeCreateWithUser,
    // analyze one installer and create opsi package with loggedin user
    createTemplateWithUser, // create template for opsi package with loggedin user
    createWingetProd, // create winget based product
    createBackgroundInfo, // create background info file
    gmUnknown);

  TTemplateChannels = (training, default, structured, custom);

  TArchitecture = (a32, a64, aarm, aarm64, aUnknown);


  { stores runtime infos of OSD}
  TOSDSettings = class(TPersistent)
  private
    FrunMode: TRunMode;
    Fshowgui: boolean;
    Fstartupfinished: boolean;   // #### ????
    Fmylocaledir: string;
    Fmylang: string;
    Fmyexitcode: integer;
    //FCreateModeCreateOnly: boolean;
    //FCreateModeBuildPackage: boolean;
    //FCreateModePackageBuilder: boolean;
    //FBuildModebuildOnly: boolean;
    //FBuildModebuildInstall: boolean;
    FBuildMode: TStrings;
    FBuildModeIndex: integer;
    FBuildModeValue: string;
    FCreateMode: TStrings;
    FCreateModeIndex: integer;
    FCreateModeValue: string;
    FDetectCount: integer;
    FDetectionSummary: TStrings;
    procedure SetBuildMode(const AValue: TStrings);
    procedure SetBuildModeValue(const AValue: string);
    procedure SetBuildModeIndex(const AValue: integer);
    procedure SetCreateMode(const AValue: TStrings);
    procedure SetCreateModeValue(const AValue: string);
    procedure SetCreateModeIndex(const AValue: integer);
    procedure SetDetectionSummary(const AValue: TStrings);
  published
    property runmode: TRunMode read FrunMode write FrunMode;
    property showgui: boolean read Fshowgui write Fshowgui;
    property startupfinished: boolean read Fstartupfinished write Fstartupfinished;
    property mylocaledir: string read Fmylocaledir write Fmylocaledir;
    property mylang: string read Fmylang write Fmylang;
    property myexitcode: integer read Fmyexitcode write Fmyexitcode;
    property BuildMode: TStrings read FBuildMode write SetBuildMode;
    property BuildModeIndex: integer read FBuildModeIndex write SetBuildModeIndex;
    property BuildModeValue: string read FBuildModeValue write SetBuildModeValue;
    property CreateMode: TStrings read FCreateMode write SetCreateMode;
    property CreateModeIndex: integer read FCreateModeIndex write SetCreateModeIndex;
    property CreateModeValue: string read FCreateModeValue write SetCreateModeValue;
    property DetectCount: integer read FDetectCount write FDetectCount;
    property DetectionSummary: TStrings read FDetectionSummary write SetDetectionSummary;
  public
    { public declarations }
    constructor Create;
    destructor Destroy;
  end;

  TTargetOS = (osLin, osWin, osMac, osMulti, osUnknown);
  TTargetOSset = set of TTargetOS;

  TArchitectureMode = (am32only_fix, am64only_fix, amBoth_fix, amSystemSpecific_fix,
    amSelectable);

  // marker for add installers
  TKnownInstaller = (
    stVisualStudioInstaller,
    stWinget,
    stMsixAppx,
    stWise,
    stQtInstaller,
    stSetupFactory,
    stInstallAnywhere,
    stAdvancedInstaller, stInstall4J, stPortableApps,
    stLinRPM, stLinDeb,
    stMacZip, stMacDmg, stMacPKG, stMacApp,
    stSFXcab, stBoxStub,
    //stAdvancedMSI,
    stInstallShield,
    stInstallShieldMSI,
    stMsi, stNsis, st7zip, st7zipsfx, stInstallAware,
    //stMSGenericInstaller,
    stWixToolset, stBitrock, stSelfExtractingInstaller, stInno,
    // stDetectedUnknown should be always the position before stDetectedUnknown
    stDetectedUnknown,
    // stUnknown should be always the last position
    stUnknown);


  TdetectInstaller = function(parent: TClass; markerlist: TStrings): boolean;


  TInstallerData = class    // Information about a type of installer
  private
  public
    installerId: TKnownInstaller;
    Name: string;
    description: string;
    patterns: TStringList;
    // pattern that indicates this installer type (has to be there)
    infopatterns: TStringList;
    // pattern that indicates a chance for this installer type (not always there)
    notpatterns: TStringList;
    // pattern that indicates it is not this installer type (veto)
    silentsetup: string;           // cli parameters for (really) silent setup
    unattendedsetup: string;       // cli parameters for unattended setup
    silentuninstall: string;       // cli parameters for (really) silent uninstall
    unattendeduninstall: string;   // cli parameters for unattended uninstall
    uninstall_waitforprocess: string;
    // process name to wait for after uninstall program is started (_Au.exe)
    install_waitforprocess: string;
    // process name to wait for after install program is started
    uninstallProg: string;         // typical path + name of the uninstall.prog
    comment: string;               // comment to this installer type
    Link: string;                  // Link to further inormation to this installer type
    uib_exitcode_function: string;
    // name of the exitcode function in the library uib_exitcode, that matches to this installer type
    detected: TdetectInstaller;    // pointer to detection function
    installErrorHandlingLines: TStringList;
    // aditional opsi-script code to run after a failed installation was detected
    info_message_html: TStringList;  // Important Information about this installer type
    // Displayed after detection
    // formatted in markdown
    { public declarations }
    constructor Create;
    destructor Destroy;
  end;

  TInstallers = array of TInstallerData;

  TSetupFile = class(TPersistent)
  private
    Factive: boolean;             // is this instance active
    FID: integer;                 // 1 = first setup file, 2 = second setup file
    FsetupFileNamePath: string;   // path only of the installer file source
    FsetupFileName: string;       // file name of the installer file source
    FsetupFullFileName: string;   // path + file name of the installer file source
    FsetupFileSize: cardinal;     // MB size of the installer file
    Farchitecture: TArchitecture; // architecture of installer if known
    FmsiId: string;               // msi ID of installer if known
    FmsiProductName: string;      // msi Display name of installer if known
    FmsiUpgradeCode: string;      // msi UpgradeCode of installer if known
    FmsiUninstallCode: boolean;   // use msi uninstall code for uninstall
    FmstAllowed: boolean;         // Is it allowed to add mst file
    FMstFullFileName: string;     // path + file name of the mst file source
    FmstFileNamePath: string;     // path only of the mst file source
    FmstFileName: string;         // file name of the mst file source
    //FmsiFullFileName: string;
    FinstallerId: TKnownInstaller; // type of detected installer
    FinstallerVersion: string;     // version of detected installer
    FinstallerName: string;        // name of the Installer detected by die
    FrequiredSpace: cardinal;      // MB size of the required space to run this installer
    FinstallDirectory: string;
    // target directory where this installer will install to
    Fmarkerlist: TStrings;
    // list of detected markers (patterns) for installer type detection
    Finfolist: TStrings;
    // list of detected info markers (patterns) for installer type detection
    Flink: string;
    // link to further inormation to the detected installer type
    FSoftwareVersion: string;      // version of the software that would be installed
    //Fwinbatch_del_argument: string;
    FinstallCommandLine: string;   // command line to install the software
    FuninstallCommandLine: string; // command line to uninstall the software
    FinstallCommandStringEx: string;
    // String expression for prim section to install the software
    FuninstallCommandStringEx: string;
    // String expression for prim section to uninstall the software
    FuninstallProg: string;        // path + name of the uninstall.prog
    FuninstallDirectory: string;   // dir of the uninstall.prog
    FtargetProg: string;
    // file name of the main program (used for desktop icons)
    FuninstallCheck: TStrings;
    // list of opsi-script code to detect an existing installation
    FoptionalUninstallLines: TStrings;
    // list of opsi-script code to uninstall an existing installation
    FisExitcodeFatalFunction: string;
    // name of the exitcode function in the library uib_exitcode, that matches to this installer type
    Funinstall_waitforprocess: string;
    // process name to wait for after uninstall program is started (_Au.exe)
    Finstall_waitforprocess: string;
    // process name to wait for after install program is started
    FinstallErrorHandlingLines: TStrings;
    // additional opsi-script code to run after a failed installation was detected (e.g.: load installer log)
    Fanalyze_progess: integer;         // state of the analyze progress in percent
    FcopyCompleteDir: boolean;
    // should we copy only the installer file or the complete directory
    FtargetOS: TTargetOS;              // on which OS can this installer install
    FtargetOSVersion: string;          // on which OS Version can this installer install
    FinstallerSourceDir: string;
    // directory of the installer inside the opsi package (%scriptpath%\files1)
    FpreferSilent: boolean;
    FwingetId: string;       // productId in winget command
    FwingetSource: string;       // source in winget command
    procedure SetMarkerlist(const AValue: TStrings);
    procedure SetInfolist(const AValue: TStrings);
    procedure SetUninstallCheck(const AValue: TStrings);
    procedure SetOptionalUninstallLines(const AValue: TStrings);
    procedure SetInstallErrorHandlingLines(const AValue: TStrings);
    procedure SetUninstallProg(const AValue: string);
    procedure SetTargetProg(const AValue: string);
    procedure SetInstallDirectory(const AValue: string);
    procedure SetUninstallDirectory(const AValue: string);
  published
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
    property msiUpgradeCode: string read FmsiUpgradeCode write FmsiUpgradeCode;
    property msiUninstallCode: boolean read FmsiUninstallCode write FmsiUninstallCode;
    property mstAllowed: boolean read FmstAllowed write FmstAllowed;
    property mstFullFileName: string read FMstFullFileName write SetMstFullFileName;
    property mstFileNamePath: string read FmstFileNamePath write FmstFileNamePath;
    property mstFileName: string read FmstFileName write FmstFileName;
    //property msiFullFileName: string read FmsiFullFileName write FmsiFullFileName;
    property installerId: TKnownInstaller read FinstallerId write FinstallerId;
    property installerName: string read FinstallerName write FinstallerName;
    property installerVersion: string read FinstallerVersion write FinstallerVersion;
    property requiredSpace: cardinal read FrequiredSpace write FrequiredSpace;
    property installDirectory: string read FinstallDirectory write SetInstallDirectory;
    property markerlist: TStrings read Fmarkerlist write SetMarkerlist;
    property infolist: TStrings read Finfolist write SetInfolist;
    property link: string read Flink write Flink;
    property SoftwareVersion: string read FSoftwareVersion write FSoftwareVersion;
    //property winbatch_del_argument: string read Fwinbatch_del_argument
    //  write Fwinbatch_del_argument;
    property installCommandLine: string read FinstallCommandLine
      write FinstallCommandLine;
    property isExitcodeFatalFunction: string
      read FisExitcodeFatalFunction write FisExitcodeFatalFunction;
    property uninstallCommandLine: string read FuninstallCommandLine
      write FuninstallCommandLine;
    property installCommandStringEx: string
      read FinstallCommandStringEx write FinstallCommandStringEx;
    property uninstallCommandStringEx: string
      read FuninstallCommandStringEx write FuninstallCommandStringEx;

    property uninstallProg: string read FuninstallProg write SetUninstallProg;
    property uninstallDirectory: string read FuninstallDirectory
      write SetUninstallDirectory;
    property targetProg: string read FtargetProg write SetTargetProg;
    property uninstallCheck: TStrings read FuninstallCheck write SetUninstallCheck;
    property optionalUninstallLines: TStrings
      read FoptionalUninstallLines write SetOptionalUninstallLines;
    property uninstall_waitforprocess: string
      read Funinstall_waitforprocess write Funinstall_waitforprocess;
    property install_waitforprocess: string
      read Finstall_waitforprocess write Finstall_waitforprocess;
    property installErrorHandlingLines: TStrings
      read FinstallErrorHandlingLines write SetInstallErrorHandlingLines;
    property analyze_progess: integer read Fanalyze_progess write Fanalyze_progess;
    property copyCompleteDir: boolean read FcopyCompleteDir write FcopyCompleteDir;
    property targetOS: TTargetOS read FtargetOS write FtargetOS;
    property targetOSVersion: string read FtargetOSVersion write FtargetOSVersion;
    property active: boolean read Factive write Factive;
    property installerSourceDir: string read FinstallerSourceDir
      write FinstallerSourceDir;
    property preferSilent: boolean read FpreferSilent write FpreferSilent;
    property wingetId: string read FwingetId write FwingetId;
    property wingetSource: string read FwingetSource write FwingetSource;

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
    property Action: string read FAction write FAction;
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
    FStrvaluesStr: string;
    FStrDefaultStr: string;
    FBoolDefault: boolean;
  protected
  published
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
    // how to handle 32/64 bit (32only, 64only, both,...)
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
    Fupdatescript: string;
    Falwaysscript: string;
    Foncescript: string;
    Fcustomscript: string;
    FuserLoginscript: string;
    Fdelsubscript: string;
    Flicenserequired: boolean;
    FproductImageFullFileName: string;    // path + name of the product icon source file
    FtargetOSset: TTargetOSset;
    // set of OS this product will work for (win,lin,mac)
    FuseCustomDir: boolean;               // should we add code for custom dir handling
    FchannelDir: string;                  // which template channel should be used
    FinstallFromLocal: boolean;
    // should we add code to install from temporary local dir
    FhandleLicensekey: boolean;           // should we add code for license handling
    Fdesktopicon: boolean;                // should we add code for desktop icon handling
    FcustomizeProfile: boolean;
    // should we add code for user profile cutomizing
    FuninstallBeforeInstall: boolean;
    // should we add code for uninstall before install by property
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
    property updatescript: string read Fupdatescript write Fupdatescript;
    property alwaysscript: string read Falwaysscript write Falwaysscript;
    property oncescript: string read Foncescript write Foncescript;
    property customscript: string read Fcustomscript write Fcustomscript;
    property userLoginscript: string read FuserLoginscript write FuserLoginscript;
    property delsubscript: string read Fdelsubscript write Fdelsubscript;
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
    property uninstallBeforeInstall: boolean
      read FuninstallBeforeInstall write FuninstallBeforeInstall;

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
    dependencies: TCollection;
    properties: TPProperties;

    { public declarations }
    constructor Create;
    procedure readProjectFile(filename: string);
    procedure readControlFile(filename: string; filter: boolean = False);
    procedure writeProjectFileToPath(path: string);
    procedure writeProjectFileToFile(myfilename: string);
  end;

  TConfiguration = class(TPersistent)
  private
    Fconfig_version: string;
    { help to detect and handle changes of config file structure }
    Fworkbench_Path: string;   // local path to the (mounted) workbench share
    Fconfig_filled: boolean;   // is the initial configuration done ?
    FregisterInFilemanager: boolean;
    // create context menue entry in file manage to call osd
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
    FpreferSilent: boolean;  // Install mode (unattended / silent)
    FUsePropDesktopicon: boolean;
    FUsePropLicenseOrPool: boolean;
    FProperties: TPProperties;
    FReadme_txt_templ: string;
    FShowCheckEntryWarning: boolean;
    FShow2StepMacSeletionWarn: boolean;
    FService_URL: string;
    FService_user: string;
    FService_pass: string;
    // FUseService: boolean;
    FTemplateChannel: TTemplateChannels;
    FLastProjectFileDir: string;  // last dir from wich we opend a project file
    FLastSetupFileDir: string;  // last dir from wich we opend a setup file
    FLastIconFileDir: string;  // last dir from wich we opend a icon file
    FLastControlFileDir: string;  // last dir from wich we opend a icon file
    Fcontrol_in_toml_format: boolean; // since opsi 4.3 control files in toml format
    Fdependencies_for_all_actionrequests: boolean;
    // since opsi 4.3 dependecies are allowed for all action requests
    FpreferMsiUninstall: boolean; // true=prefer uninstall via msi if possible
    //FwriteMetaDataFile: boolean;  // true=write opsi-meta-data.toml file
    //FShowBackgroundInfoBtn: boolean;  // true=show the background Info Button at start tab
    FEnableBackgroundMetaData: boolean;  // false=hide all background related features
    FLastLanguage: string;  // last language we used (default=auto)
    procedure SetLibraryLines(const AValue: TStrings);
    procedure SetPreInstallLines(const AValue: TStrings);
    procedure SetPostInstallLines(const AValue: TStrings);
    procedure SetPreUninstallLines(const AValue: TStrings);
    procedure SetPostUninstallLines(const AValue: TStrings);
    procedure SetProperties(const AValue: TPProperties);
    procedure SetWorkbench_path(const AValue: string);
  published
    property config_version: string read Fconfig_version write Fconfig_version;
    property workbench_Path: string read Fworkbench_Path write SetWorkbench_path;
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
    property preferSilent: boolean read FpreferSilent write FpreferSilent;
    property UsePropDesktopicon: boolean read FUsePropDesktopicon
      write FUsePropDesktopicon;
    property UsePropLicenseOrPool: boolean read FUsePropLicenseOrPool
      write FUsePropLicenseOrPool;
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
    property LastProjectFileDir: string read FLastProjectFileDir
      write FLastProjectFileDir;
    property LastSetupFileDir: string read FLastSetupFileDir write FLastSetupFileDir;
    property LastIconFileDir: string read FLastIconFileDir write FLastIconFileDir;
    property LastControlFileDir: string read FLastControlFileDir
      write FLastControlFileDir;
    property control_in_toml_format: boolean
      read Fcontrol_in_toml_format write Fcontrol_in_toml_format;
    property dependencies_for_all_actionrequests: boolean
      read Fdependencies_for_all_actionrequests write Fdependencies_for_all_actionrequests;
    property preferMsiUninstall: boolean read FpreferMsiUninstall
      write FpreferMsiUninstall;
    //property writeMetaDataFile: boolean read FwriteMetaDataFile
    //  write FwriteMetaDataFile;
    property EnableBackgroundMetaData: boolean
      read FEnableBackgroundMetaData write FEnableBackgroundMetaData;
    property LastLanguage: string
      read FLastLanguage write FLastLanguage;



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
procedure reload_installer_info_messages;

const
  CONFVERSION = '4.2.0.9';
  templChannelStrings: array [TTemplateChannels] of string =
    ('training', 'default', 'structured', 'custom');

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
  opsitmp: string;
  localservicedata: TOpsi4Data = nil;
  //localservicedataInitalized : boolean = false;
  localservicedataConnected: boolean = False;
  passwordToUse: string;
  opsiserviceversion: string;

resourcestring

  // new for 4.1.0.2 ******************************************************************
  rsworkbench_Path = 'Path to the opsi_workbench';
  rsworkbench_mounted =
    'Automatically detected. Is the opsi workbench reachable at workbench_Path.';
  rsconfig_filled = 'Automatically detected. Do we have all needed configurations';
  rsregisterInFilemanager =
    'Should this program be registered to the Filemanager (Explorer) context menu ?';
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
    'List of opsi-script code lines that should be included after the installation is finished.'
    + LineEnding + 'One per line. May be empty. Example:' + LineEnding +
    'comment "Installation finished..."';
  rspreUninstallLines =
    'List of opsi-script code lines that should be included before the uninstallation starts.'
    + LineEnding + 'One per line. May be empty. Example:' + LineEnding +
    'comment "Start the uninstallation ..."';
  rspostUninstallLines =
    'List of opsi-script code lines that should be included after the uninstallation is finished.'
    + LineEnding + 'One per line. May be empty. Example:' + LineEnding +
    'comment "Uninstall finished..."';
  rspathToOpsiPackageBuilder =
    'Path to the OpsiPackageBuilder. OpsiPackageBuilder is used to build the opsi packages via ssh. see: https://forum.opsi.org/viewtopic.php?f=22&t=7573';

  rsCreateRadioGroupTitle = 'Create Mode';
  rsCreateRadioIndex = 'selects the Create mode Radiobutton.';
  rsCreateRadioFiles = 'create opsi product files';
  rsCreateRadioFilesBuild = 'create opsi product files and  build package';
  rsCreateRadioFilesPackageBuilder =
    'create opsi product files and start the interactive PackageBuilder';

  rsBuildRadioGroupTitle = 'Build Mode';
  rsBuildRadioIndex = 'selects the Build mode Radiobutton.';
  rsBuildRadioBuild = 'build';
  rsBuildRadioBuildInstall = 'build and install';
  rsCnfdTitle = 'Edit your configuration here.' + LineEnding +
    'Click on a line to get help ' + LineEnding +
    'in the yellow field at the bottom.';
  rsService_URL = 'The URL of the opsi webservice (Format: https://<opsi-server>:4447)';
  rsService_user = 'The user name used to connect to the opsi webservice';
  rsService_pass = 'The password of the given user used to connect to the opsi webservice'
    + LineEnding + 'ATTENTION SECURITY RISK: Even though it is stored encrypted,' +
    LineEnding + 'it is easy to decrypt by analyzing the source code.' +
    LineEnding + 'If empty you will be asked when the connection is started.';
  rsConfigVersion = 'Do not change. Version of the config structure.';
  rsReadme_txt_templ = 'Path to the text file that is used as Read-me template';
  rsInternalSet = 'Do not change here - Internally set by a dialog.';
  rsUsePropDesktopicon =
    'Should we create a "DesktopIcon" property and add code to handle desktop icons ?';
  rsPropDesktopiconDescription = 'Should there be a desktop icon ?';
  rsUsePropLicenseOrPool =
    'Should we create a "LicenseOrPool" property and add code to handle license keys ?';
  rsPropLicenseOrPoolDescription = 'License key or opsi license pool';
  rsPropInstallFromLocalDescription =
    'Determines if the installation files will be copied locally';
  rsPropInstallArchDescription = 'Which architecture (32 / 64 Bit) should be installed?';
  rsPreferSilent =
    'Should really silent installs (with no output) be preferred ? Default is false = unattended is preferred.';
  rsControl_in_toml_format =
    'Create control file in (opsi 4.3) toml format ?. ' + LineEnding +
    'If true, you need opsi 4.3 (or up) ' + LineEnding +
    'and the toml file is the main control file that should be maintained.';
  rsDependencies_for_all_actionrequests =
    'Allow dependencies for all action request ?. ' + LineEnding +
    'If true, you need opsi 4.3 (or up) ' + LineEnding +
    'Be careful when creating dependencies for other action requests than "setup"';
  rsPreferMsiUninstall =
    'If true=prefer uninstall via msi if possible.' + LineEnding +
    'Affects Installers that are wrappers around msi,' + LineEnding +
    'like installshieldMSI, advanced_installer, wix toolset';
  (*
  rsWriteMetaDataFile =
    'If true=write opsi-meta-data.toml file';
  rsShowBackgroundInfoBtn =
    'If true= Show "Background Info" Button on start tab';
    *)
  rsEnableBackgroundMetaData =
    'If true= Show "Background Info File" Button on start tab.' +
    LineEnding + 'Make "Product configuration 3" Tab visible and enabled.' +
    LineEnding + 'Write opsi-meta-data.toml file on Product creation';
  //************************************************
  //info_message_html.Text
  //************************************************
  mdInstallerInfo_Installshield =
    '## This is a Installshield Installer.' + LineEnding +
    'So it will be perhaps complicated -' + LineEnding + 'because:' +
    LineEnding + '' + LineEnding + '1. Installshield exists since 1993.' +
    LineEnding + 'Over the time some command line parameter have changed' +
    LineEnding + 'and we could not detect the version of the Installshield that was used.'
    + LineEnding + '' + LineEnding +
    '2. Installshield may create two different kinds of Installer:' +
    LineEnding + 'A kind of classic setup and a kind setup as wrapper around msi.' +
    LineEnding + 'We could not detect for sure, which kind of installer we have.' +
    LineEnding + '' + LineEnding + '3. Installshield is flexible.' +
    LineEnding +
    'So in fact, the developer may have changed the command line parameter to a totally different style.'
    + LineEnding + '' + LineEnding +
    'If you have a MSI-Wrapper then we have as cli parameter:' +
    LineEnding + '' + LineEnding + '- silent:' + LineEnding +
    '`/s /v" /qn ALLUSERS=1 REBOOT=ReallySuppress`"' + LineEnding +
    '' + LineEnding + '- unattended:' + LineEnding +
    '`/s /v"/qb-! ALLUSERS=1 REBOOT=ReallySuppress`"' + LineEnding +
    '' + LineEnding + 'If you have a classic setup then we have as cli parameter just:'
    + LineEnding + '' + LineEnding + '- silent:' + LineEnding + '`/s`' +
    LineEnding + '' + LineEnding +
    'If you have a classic setup that is very old (last century or near by), then you perhaps have to add the parameter:'
    + LineEnding + '`/sms`';
  mdInstallerInfo_InstallAnywhere =
    '## This is a InstallAnywhere Installer.' + LineEnding +
    'If the parameter `-i silent` does not work, try the following:' +
    '' + LineEnding + LineEnding +
    'Run the installer interactive with the `-r` switch followed by' +
    LineEnding + 'the path and file name of the response file you want to generate.' +
    LineEnding + '' + LineEnding + 'For example:' + LineEnding +
    '' + LineEnding + '`setup.exe -r "./response.txt"`' + LineEnding +
    '' + LineEnding +
    'Then you have to add at the top of the generated response file the line:' +
    LineEnding + '' + LineEnding + '`INSTALLER_UI=silent`' + LineEnding +
    '' + LineEnding + 'Then run silent by calling:' + LineEnding +
    '`setup.exe -f "./response.txt"`' + LineEnding;
  mdInstallerInfo_PortableApps =
    '## This is not a setup program.' + LineEnding +
    'It is a PortableApps Selfextractor.' + LineEnding +
    'So there are no unattended / silent modes.' + '' + LineEnding +
    LineEnding + 'Uncompress with 7zip and copy the files';
  mdInstallerInfo_SetupFactory =
    '## This is a Setup Factory Installer.' + LineEnding +
    'Perhaps the parameter `/S` may work for silent mode.' + LineEnding +
    '' + LineEnding + 'But often this functionality is not enabled.' +
    LineEnding + '' + LineEnding +
    'In this case you have extract / install the content and deploy it on an other way.';
  mdInstallerInfo_QtInstaller =
    '## This is a QT Installer.' + LineEnding + '' + LineEnding +
    'Perhaps the standard parameters may work for silent mode.' +
    LineEnding + 'In this case you have to give the **installdir** - it will not work without.'
    + LineEnding + 'Therefore a install dir value will be created and you may have to change it.'
    + LineEnding + '' + LineEnding +
    'In other cases, you may call an answer script (*.qs) with the parameter `--script` .'
    + LineEnding + 'And you should have a look at the following documentation pages:.' +
    LineEnding + '' + LineEnding +
    '* <https://doc.qt.io/qtinstallerframework/ifw-cli.html>  ' +
    LineEnding + '* <https://doc.qt.io/qtinstallerframework/ifw-use-cases-cli.html> ' +
    LineEnding + '* <https://wiki.qt.io/Online_Installer_4.x> ' +
    LineEnding + '* <https://gist.github.com/WindAzure/f3bed9e058cdc81eaa357414610c9125> ';
  mdInstallerInfo_Wise =
    '## This is a Wise Installer.' + LineEnding +
    'This is an old discontinued Installer framework 1995 - 2009.' +
    LineEnding + 'If you are lucky it is an msi wrapper.' + LineEnding +
    'In this case you may start the installation and look for a new msi in:' +
    LineEnding + '"C:\Program Files (x86)\Common Files\Wise Installation Wizard"' +
    LineEnding + 'You may use this msi as install file.' + LineEnding +
    'You may perhaps also pass the msi parameters as arguments to your setup.exe.';
  mdInstallerInfo_MsixAppx =
    '## This is a Msix / Appx / AppxBundle / MsixBundle file.' +
    LineEnding + 'This kind of packages may be installed via powershell.' +
    LineEnding + LineEnding + 'In order to check for the dependecies of this package' +
    LineEnding + 'and if this package is aviable from MS store,' +
    LineEnding + LineEnding +
    'then you may use the following web site to check and download' +
    LineEnding + LineEnding + 'missing packages: ' + LineEnding +
    LineEnding + '<https://store.rg-adguard.net/>' + LineEnding +
    LineEnding + 'There also is often a problem with the uninstallation.' +
    LineEnding + LineEnding +
    'So the use of the "uninstall\_before\_install" checkbox is recommended.';
  mdInstallerInfo_winget =
    '## Making a winget based package.' + LineEnding + LineEnding +
    'You need to know the winget Id and Source of the software to install.' +
    LineEnding + LineEnding + 'A tool that may help you to find this data is:' +
    LineEnding + LineEnding +
    'UniGetUI (formerly WingetUI), The Graphical Interface for your package managers' +
    LineEnding + LineEnding + '<https://www.marticliment.com/unigetui/>';
    mdInstallerInfo_VisualStudioInstaller =
    '## This is a Visual Studio installer Installer.' + LineEnding +
    'By default it works as "web installer",' + LineEnding +
    'so it will download while installing.' + LineEnding + LineEnding +
    'The parameter "--all" means to install all possible components and workloads.' + LineEnding +
    'You may use the parameter "--add"' + LineEnding +
    'to control the components and workloads to install.' + LineEnding + LineEnding +
    'In order to make an offline install,'+LineEnding +
    'you need first to download the installfiles: ' + LineEnding +
    'https://learn.microsoft.com/en-us/visualstudio/install/create-a-network-installation-of-visual-studio' + LineEnding +LineEnding +
    'More info here: ' + LineEnding +
    '<https://learn.microsoft.com/en-us/visualstudio/install/use-command-line-parameters-to-install-visual-studio>';
  // marker for add installers

implementation

uses
  {$IFDEF OSDGUI}
  osdform,
  {$ENDIF OSDGUI}
  osdcontrolfile_io;

var
  FileVerInfo: TFileVersionInfo;

// TOSDSettings ************************************
constructor TOSDSettings.Create;
begin
  startupfinished := False;
  myexitcode := 0;
  opsitmp := '';
  FBuildMode := TStringList.Create;
  FBuildMode.Add(rsBuildRadioBuild);
  FBuildMode.Add(rsBuildRadioBuildInstall);
  FCreateMode := TStringList.Create;
  FCreateMode.Add(rsCreateRadioFiles);
  FCreateMode.Add(rsCreateRadioFilesBuild);
  FCreateMode.Add(rsCreateRadioFilesPackageBuilder);
  FCreateModeIndex := 0;
  FBuildModeIndex := 0;
  FCreateModeValue := CreateMode.Strings[FCreateModeIndex];
  FBuildModeValue := BuildMode.Strings[FBuildModeIndex];
  FDetectionSummary := TStringList.Create;
  DetectCount := 0;
  inherited;
end;

destructor TOSDSettings.Destroy;
begin
  FreeAndNil(FBuildMode);
  FreeAndNil(FCreateMode);
  FreeAndNil(FDetectionSummary);
  inherited;
end;

procedure TOSDSettings.SetBuildMode(const AValue: TStrings);
begin
  FBuildMode.Assign(AValue);
end;

procedure TOSDSettings.SetCreateMode(const AValue: TStrings);
begin
  FCreateMode.Assign(AValue);
end;

procedure TOSDSettings.SetBuildModeValue(const AValue: string);
begin
  FBuildModeValue := AValue;
  FBuildModeIndex := BuildMode.IndexOf(AValue);
end;

procedure TOSDSettings.SetCreateModeValue(const AValue: string);
begin
  FCreateModeValue := AValue;
  FCreateModeIndex := CreateMode.IndexOf(AValue);
end;

procedure TOSDSettings.SetBuildModeIndex(const AValue: integer);
begin
  FCreateModeIndex := AValue;
  FBuildModeValue := BuildMode.Strings[AValue];
end;

procedure TOSDSettings.SetCreateModeIndex(const AValue: integer);
begin
  FCreateModeIndex := AValue;
  FCreateModeValue := CreateMode.Strings[AValue];
end;

procedure TOSDSettings.SetDetectionSummary(const AValue: TStrings);
begin
  FDetectionSummary.Assign(AValue);
end;


// TInstallerData ************************************

constructor TInstallerData.Create;
begin
  patterns := TStringList.Create;
  infopatterns := TStringList.Create;
  notpatterns := TStringList.Create;
  installErrorHandlingLines := TStringList.Create;
  info_message_html := TStringList.Create;
  inherited;
end;

destructor TInstallerData.Destroy;
begin
  FreeAndNil(patterns);
  FreeAndNil(infopatterns);
  FreeAndNil(notpatterns);
  FreeAndNil(installErrorHandlingLines);
  FreeAndNil(info_message_html);
  inherited;
end;

// TSetupFile ************************************

constructor TSetupFile.Create;
begin
  Fmarkerlist := TStringList.Create;
  Finfolist := TStringList.Create;
  FuninstallCheck := TStringList.Create;
  FoptionalUninstallLines := TStringList.Create;
  FinstallErrorHandlingLines := TStringList.Create;
  inherited;
  //initValues;
end;

destructor TSetupFile.Destroy;
begin
  FreeAndNil(Fmarkerlist);
  FreeAndNil(Finfolist);
  FreeAndNil(FuninstallCheck);
  FreeAndNil(FoptionalUninstallLines);
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
  (*
  if AValue = SetupFullFileName then
    exit;
    *)
  FsetupFullFileName := AValue;
  FSetupFileNamePath := ExtractFileDir(AValue);
  FsetupFileName := ExtractFileName(AValue);
  LogDatei.log('FsetupFullFileName ' + FsetupFullFileName, LLdebug);
  LogDatei.log('FsetupFileName ' + FsetupFileName, LLdebug);
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

procedure TSetupFile.SetOptionalUninstallLines(const AValue: TStrings);
begin
  FoptionalUninstallLines.Assign(AValue);
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

procedure TSetupFile.SetUninstallProg(const AValue: string);
var
  str: string;
begin
  str := AValue;
  str := opsiunquotestr2(str, '"');
  str := opsiunquotestr2(str, '''');
  FuninstallProg := str;
end;

procedure TSetupFile.SetTargetProg(const AValue: string);
var
  str: string;
begin
  str := AValue;
  str := opsiunquotestr2(str, '"');
  str := opsiunquotestr2(str, '''');
  FtargetProg := str;
end;

procedure TSetupFile.SetInstallDirectory(const AValue: string);
var
  str: string;
begin
  str := AValue;
  str := opsiunquotestr2(str, '"');
  str := opsiunquotestr2(str, '''');
  FinstallDirectory := str;
end;

procedure TSetupFile.SetUninstallDirectory(const AValue: string);
var
  str: string;
begin
  str := AValue;
  str := opsiunquotestr2(str, '"');
  str := opsiunquotestr2(str, '''');
  FuninstallDirectory := str;
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
  //FmsiFullFileName := '';
  FinstallerId := stUnknown;
  FrequiredSpace := 0;
  FinstallDirectory := 'unknown';
  FuninstallDirectory := '';
  Fmarkerlist.Clear;
  Finfolist.Clear;
  Flink := '';
  FSoftwareVersion := '0.0';
  //Fwinbatch_del_argument := '';
  FinstallCommandLine := '';
  FuninstallCommandLine := '';
  FuninstallProg := '';
  FtargetProg := '';
  FuninstallCheck.Clear;
  FoptionalUninstallLines.Clear;
  Fanalyze_progess := 0;
  FisExitcodeFatalFunction := 'isGenericExitcodeFatal';
  Funinstall_waitforprocess := '';
  Finstall_waitforprocess := '';
  FcopyCompleteDir := False;
  FinstallErrorHandlingLines.Clear;
  FmsiProductName := '';
  FinstallerSourceDir := '';
  FpreferSilent := myconfiguration.preferSilent;
  FmsiUpgradeCode := '';
  FmsiUninstallCode := False; // Do not use msi uninstall code by default
  FwingetSource := '';
  FwingetId := '';
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


// Defaults
function TPProperty.GetDefaultLines: TStrings;
begin
  Result := TStringList.Create;
  Result.DelimitedText := FStrDefaultStr;
end;

procedure TPProperty.SetDefaultLines(const AValue: TStrings);
begin
  FStrDefaultStr := AValue.DelimitedText;
end;

procedure TPProperty.SetDefaultStr(const AValue: string);
begin
  FStrDefaultStr := AValue;
end;

function TPProperty.GetDefaultStr: string;
begin
  Result := FStrDefaultStr;
end;

//Values

function TPProperty.GetValueLines: TStrings;
begin
  Result := TStringList.Create;
  Result.DelimitedText := FStrvaluesStr;
end;

procedure TPProperty.SetValueLines(const AValue: TStrings);
begin
  FStrvaluesStr := AValue.DelimitedText;
end;

procedure TPProperty.SetValueStr(const AValue: string);
begin
  FStrvaluesStr := AValue;
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


{ TPProperties }

constructor TPProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TPProperty);
end;


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
  Result := False;
  for i := 0 to index - 1 do
    if lowercase(tmpstr) = lowercase(Items[i].Property_Name) then
      Result := True;
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

procedure makeProperties;
var
  //myprop: TStringList;
  myprop: TPProperty;
  propexists: boolean;
  tmpstrlist: TStringList;
  myrunmode: TRunMode;
begin
  myrunmode := osdsettings.runmode;
  propexists := aktProduct.properties.propExists('DesktopIcon');
  if (aktProduct.productdata.desktopicon) and not propexists then
  begin
    myprop := TPProperty(aktProduct.properties.add);
    myprop.init;
    myprop.Property_Name := lowercase('DesktopIcon');
    myprop.description := rsPropDesktopiconDescription;
    myprop.Property_Type := bool;
    myprop.multivalue := False;
    myprop.editable := False;
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
    myprop.boolDefault := True;
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
  if (myrunmode in [analyzeCreateWithUser, createTemplateWithUser]) and not
    propexists then
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
  if (myrunmode in [analyzeCreateWithUser, createTemplateWithUser]) and
    not propexists then
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
  if ((myrunmode in [analyzeCreateWithUser, createTemplateWithUser]) or
    (aktProduct.productdata.uninstallBeforeInstall)) and not propexists then
  begin
    myprop := TPProperty(aktProduct.properties.add);
    myprop.init;
    myprop.Property_Name := lowercase('uninstall_before_install');
    myprop.description := 'Uninstall previous versions before installation';
    myprop.Property_Type := bool;
    myprop.multivalue := False;
    myprop.editable := False;
    myprop.boolDefault := False;
  end;


  propexists := aktProduct.properties.propExists('execution_method');
  if (myrunmode in [analyzeCreateWithUser, createTemplateWithUser]) and
    not propexists then
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

  propexists := aktProduct.properties.propExists('install_at_safe_mode_boot');
  if (myrunmode in [analyzeCreateWithUser, createTemplateWithUser]) and
    not propexists then
  begin
    myprop := TPProperty(aktProduct.properties.add);
    myprop.init;
    myprop.Property_Name := lowercase('install_at_safe_mode_boot');
    myprop.description := 'Install after boot to safe mode';
    myprop.Property_Type := bool;
    myprop.multivalue := False;
    myprop.editable := False;
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
  configDir: array[0..MaxPathLen] of char; //Allocate memory
  pfile: TextFile;
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
      JSONString := Streamer.ObjectToJSONString(aktProduct.properties);
      writeln(pfile, JSONString);
      deactivateImportMode;
      JSONString := Streamer.ObjectToJSONString(aktProduct.dependencies);
      writeln(pfile, JSONString);
      JSONString := aktmeta.convert_aktmeta_to_jsonstring(True);
      if Assigned(logdatei) then
        logdatei.log('write meta to project file: ' + JSONString, LLDebug);
      writeln(pfile, JSONString);
      CloseFile(pfile);

    finally
      FreeAndNil(Streamer);
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


procedure TopsiProduct.readProjectFile(filename: string);
var
  DeStreamer: TJSONDeStreamer;
  JSONString, JSONObjString: string;
  myfilename: string;
  pfile: TextFile;
  aktproperty: TPProperty;
  // aktproperty: even if the compiler tell this it is not used - it is used !!
  i: integer;
begin
  try
    if Assigned(logdatei) then
      logdatei.log('Start readProjectFile', LLDebug);
    // project file name
    myfilename := filename;
    myfilename := ExpandFileName(myfilename);
    if Assigned(logdatei) then
      logdatei.log('readProjectFile from: ' + myfilename, LLDebug);
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
            if Assigned(logdatei) then
              logdatei.log('Import property JSONString: ' + JSONString, LLDebug);
            if jsonIsArray(JSONString) then
            begin
              aktProduct.properties.Clear;
              //if Assigned(logdatei) then
              //logdatei.log('count: ' + inttostr(jsonAsArrayCountElements(JSONString) - 1), LLDebug);
              //   if jsonAsArrayCountElements(JSONString) > 0 then
              for i := 0 to jsonAsArrayCountElements(JSONString) - 1 do
              begin
                aktproperty := TPProperty.Create;
                jsonAsArrayGetElementByIndex(JSONString, i, JSONObjString);
                // this adds a new property object to akt product:
                aktproperty := aktProduct.properties.Add;
                // here is the new property object used
                DeStreamer.JSONToObject(JSONObjString, aktProduct.properties.Items[i]);
                if Assigned(logdatei) then
                  logdatei.log('Property_Name: ' +
                    aktProduct.properties.Items[i].Property_Name, LLDebug);
              end;
            end;
          end;
        deactivateImportMode;
        readln(pfile, JSONString);
        if Assigned(logdatei) then
          logdatei.log('dependencies line: ' + JSONString, LLDebug);
        DeStreamer.JSONToObject(JSONString, aktProduct.dependencies);
        // read meta data if existing
        if not EOF(pfile) then
        begin
          readln(pfile, JSONString);
          if Assigned(logdatei) then
            logdatei.log('Meta data line: ' + JSONString, LLDebug);
          if JSONString <> '' then
            aktmeta.convert_jsonstring_to_aktmeta(JSONString);
        end;
        // Cleanup
      finally
        FreeAndNil(DeStreamer);
        CloseFile(pfile);
      end;
      if Assigned(logdatei) then
        logdatei.log('readProjectFile: ' + JSONString, LLDebug)
      else
        ShowMessage('readProjectFile: ' + JSONString);
    end
    else
    begin
      if Assigned(logdatei) then
        logdatei.log('Project file not found: ' + myfilename, LLError);
    end;
    if Assigned(logdatei) then
      logdatei.log('Finished readProjectFile', LLDebug2);

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


procedure TopsiProduct.readControlFile(filename: string; filter: boolean = False);
var
  myfilename: string;
begin
  try
    if Assigned(logdatei) then
      logdatei.log('Start readControlFile', LLDebug);
    // control file name
    myfilename := filename;
    myfilename := ExpandFileName(myfilename);
    if Assigned(logdatei) then
      logdatei.log('readControlFile from: ' + myfilename, LLDebug);
    if FileExists(myfilename) then
    begin
      if lowercase(ExtractFileName(myfilename)) = 'control.toml' then
      begin
        readControlFileToml(myfilename, filter);
      end
      else // pre opsi 4.3 (non toml) style control file
      begin
        readControlFile42(myfilename, filter);
      end;
    end
    else
    begin
      if Assigned(logdatei) then
        logdatei.log('Control file not found: ' + myfilename, LLError);
    end;
    if Assigned(logdatei) then
      logdatei.log('Finished readControlFile', LLDebug2);

  except
    on E: Exception do
      if Assigned(logdatei) then
      begin
        LogDatei.log('readControlFile exception. Details: ' + E.ClassName +
          ': ' + E.Message, LLError);
        ShowMessage('readControlFile exception. Details: ' +
          E.ClassName + ': ' + E.Message);
      end
      else
        ShowMessage('readControlFile exception. Details: ' +
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
  FpreferSilent := False; // Unattended
  Fcontrol_in_toml_format := True; // opsi 4.3
  Fdependencies_for_all_actionrequests := False; // opsi 4.2
  {$IFDEF UNIX}
  FLasticonFileDir := '/usr/share/opsi-setup-detector/icons';
  {$ENDIF UNIX}
  {$IFDEF WINDOWS}
  FLasticonFileDir :=
    ExtractFileDir(Application.Params[0]) + PathDelim + 'icons';
  {$ENDIF WINDOWS}
  FpreferMsiUninstall := True;
  //FwriteMetaDataFile := False;
  //FShowBackgroundInfoBtn := False;
  FEnableBackgroundMetaData := False;
  FLastLanguage := 'auto';
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
      Fconfig_filled := False
    else
      Fconfig_filled := True;
    //Fconfig_version := CONFVERSION;
    // update written version only if the new version is larger
    if CompareDotSeparatedNumbers(Fconfig_version, '<', myVersion) then
      Fconfig_version := myVersion;

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
      {$IFDEF WINDOWS}
      registerForWinExplorer(FregisterInFilemanager);
      {$ELSE}
      {$ENDIF WINDOWS}
    finally
      FreeAndNil(Streamer);
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
  JSONString: string;
  myfilename: string;
  configDir: array[0..MaxPathLen] of char; //Allocate memory
  configDirUtf8: utf8string;
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
        FreeAndNil(DeStreamer);
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

    // init FPathToOpsiPackageBuilder;
    FPathToOpsiPackageBuilder := detect_opb_path(FPathToOpsiPackageBuilder);

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
  i1, i2: integer;
begin
  Result := False;
  markerlist.Sort;
  if markerlist.Find(TInstallerData(parent).patterns[0], i1) or
    markerlist.Find(TInstallerData(parent).patterns[1], i2) then
    Result := True;
end;

function detectedbypatternwithor(parent: TClass; markerlist: TStringList): boolean;
var
  patternindex: integer;
  pattern: string;
  numberOfNotpatternDetected: integer = 0;
begin
  Result := False;
  for patternindex := 0 to TInstallerData(parent).patterns.Count - 1 do
  begin
    pattern := TInstallerData(parent).patterns[patternindex];
    if markerlist.IndexOf(LowerCase(pattern)) >= 0 then
      Result := True;
  end;
  for patternindex := 0 to TInstallerData(parent).notpatterns.Count - 1 do
  begin
    pattern := TInstallerData(parent).notpatterns[patternindex];
    if markerlist.IndexOf(LowerCase(pattern)) >= 0 then
      Inc(numberOfNotpatternDetected);
  end;
  if numberOfNotpatternDetected > 0 then Result := False;
end;

function detectedbypatternwithAnd(parent: TClass; markerlist: TStringList): boolean;
var
  patternindex: integer;
  pattern: string;
  numberOfPatternDetected: integer = 0;
  numberOfNotpatternDetected: integer = 0;
  installerPatternCount: integer = 0;
  tmpstr: string;
begin
  Result := False;
  installerPatternCount := TInstallerData(parent).patterns.Count;
  for patternindex := 0 to installerPatternCount - 1 do
  begin
    pattern := TInstallerData(parent).patterns[patternindex];
    if markerlist.IndexOf(LowerCase(pattern)) >= 0 then
      Inc(numberOfPatterndetected);
  end;
  for patternindex := 0 to TInstallerData(parent).notpatterns.Count - 1 do
  begin
    pattern := TInstallerData(parent).notpatterns[patternindex];
    if markerlist.IndexOf(LowerCase(pattern)) >= 0 then
      Inc(numberOfNotpatternDetected);
  end;
  if numberOfPatterndetected > 0 then
  begin
    tmpstr := 'Found patterns: ' + IntToStr(numberOfPatterndetected) +
      ' for ' + TInstallerData(parent).Name;
    LogDatei.log(tmpstr, LLnotice);
    osdsettings.DetectionSummary.Add(tmpstr);
    if numberOfPatterndetected = installerPatternCount then
    begin
      Result := True;
      tmpstr := 'All patterns found, needed: ' + IntToStr(installerPatternCount);
      LogDatei.log(tmpstr, LLnotice);
      osdsettings.DetectionSummary.Add(tmpstr);
      if numberOfNotpatternDetected > 0 then
      begin
        Result := False;
        tmpstr := 'Not patterns (Veto) found: ' + IntToStr(numberOfPatterndetected) +
          ' for ' + TInstallerData(parent).Name;
        LogDatei.log(tmpstr, LLnotice);
        osdsettings.DetectionSummary.Add(tmpstr);
      end;
    end
    else
    begin
      tmpstr := 'Not all patterns found, needed: ' + IntToStr(
        installerPatternCount);
      LogDatei.log(tmpstr, LLnotice);
      osdsettings.DetectionSummary.Add(tmpstr);
    end;
  end;
end;

procedure initaktproduct;
var
  i: integer;
  str: string;
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
    updatescript := '';
    (*
    alwaysScript := '';
    onceScript := '';
    customScript := '';
    userLoginScript := '';
    *)
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
      PathDelim + channelDir + PathDelim + 'images' + PathDelim + 'template.png';
    if not FileExistsUTF8(defaultIconFullFileName) then
      defaultIconFullFileName :=
        ExtractFileDir(Application.Params[0]) + PathDelim + 'template-files' +
        PathDelim + 'default' + PathDelim + 'images' + PathDelim + 'template.png';
    {$ENDIF WINDOWS}
    {$IFDEF LINUX}
    defaultIconFullFileName :=
      '/usr/share/opsi-setup-detector' + PathDelim + 'template-files' +
      PathDelim + channelDir + PathDelim + 'images' + PathDelim + 'template.png';
    if not fileexists(defaultIconFullFileName) then
      defaultIconFullFileName :=
        '/usr/share/opsi-setup-detector' + PathDelim + 'template-files' +
        PathDelim + 'default' + PathDelim + 'images' + PathDelim + 'template.png';
    // in develop environment
    if not fileexists(defaultIconFullFileName) then
      defaultIconFullFileName :=
        ExtractFileDir(Application.Params[0]) + PathDelim + 'template-files' +
        PathDelim + 'default' + PathDelim + 'images' + PathDelim + 'template.png';
    {$ENDIF LINUX}
    {$IFDEF DARWIN}
    // the first path is in the development environment
    defaultIconFullFileName :=
      ExtractFileDir(Application.ExeName) + PathDelim + 'template-files' +
      PathDelim + 'default' + PathDelim + 'images' + PathDelim + 'template.png';
    if not DirectoryExists(defaultIconFullFileName) then
      defaultIconFullFileName :=
        ExtractFileDir(Application.ExeName) + PathDelim +
        '../Resources/template-files' + PathDelim + channelDir +
        PathDelim + 'images' + PathDelim + 'template.png';
    if not DirectoryExists(defaultIconFullFileName) then
      defaultIconFullFileName :=
        ExtractFileDir(Application.ExeName) + PathDelim + 'template-files' +
        PathDelim + 'default' + PathDelim + 'images' + PathDelim + 'template.png';
    {$ENDIF DARWIN}
    osdbasedata.aktProduct.productdata.productImageFullFileName :=
      defaultIconFullFileName;
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
    uninstallBeforeInstall := False;
  end;
  // Create Dependencies
  aktProduct.dependencies := TCollection.Create(TPDependency);
  // Create Properties
  aktProduct.properties := TPProperties.Create(aktProduct);
  // detection count
  osdsettings.DetectionSummary.Clear;
  osdsettings.DetectCount := 0;
  //aktProduct.targetOS:= osWin;
  aktmeta.initMeta;
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
    FreeAndNil(aktProduct.productdata);
  if Assigned(aktProduct.dependencies) then
    FreeAndNil(aktProduct.dependencies);
  if Assigned(myconfiguration) then
  begin
    myconfiguration.writeconfig;
    FreeAndNil(myconfiguration);
  end;
end;

function cleanOpsiId(opsiId: string): string; // clean up productId
begin
  opsiId := LowerCase(opsiId);
  Result := stringReplaceRegex(opsiId, '[^a-z0-9_-]', '_');
  // [^A-Za-z0-9._-]
end;

procedure reload_installer_info_messages;
begin
  // unknown
  with installerArray[integer(stUnknown)] do
    info_message_html.Text := '';
  // inno
  with installerArray[integer(stInno)] do
    info_message_html.Text := '';
  // NSIS
  with installerArray[integer(stNsis)] do
    info_message_html.Text := '';
  // InstallShield
  with installerArray[integer(stInstallShield)] do
    info_message_html.Text := mdInstallerInfo_Installshield;
  // InstallShieldMSI
  with installerArray[integer(stInstallShieldMSI)] do
    info_message_html.Text := mdInstallerInfo_Installshield;
  // MSI
  with installerArray[integer(stMSI)] do
    info_message_html.Text := '';
  // 7zip
  with installerArray[integer(st7zip)] do
    info_message_html.Text := '';
  // st7zipsfx
  with installerArray[integer(st7zipsfx)] do
    info_message_html.Text := '';
  // stInstallAware
  with installerArray[integer(stInstallAware)] do
    info_message_html.Text := '';
  (*
  // stMSGenericInstaller
  with installerArray[integer(stMSGenericInstaller)] do
    info_message_html.Text := '';
    *)
  // stWixToolset
  with installerArray[integer(stWixToolset)] do
    info_message_html.Text := '';
  // stBoxStub
  with installerArray[integer(stBoxStub)] do
    info_message_html.Text := '';
  // stSFXcab
  with installerArray[integer(stSFXcab)] do
    info_message_html.Text := '';
  // stBitrock
  with installerArray[integer(stBitrock)] do
    info_message_html.Text := '';
  // stSelfExtractingInstaller
  with installerArray[integer(stSelfExtractingInstaller)] do
    info_message_html.Text := '';
  with installerArray[integer(stMacZip)] do
    info_message_html.Text := '';
  with installerArray[integer(stMacDmg)] do
    info_message_html.Text := '';
  with installerArray[integer(stMacPKG)] do
    info_message_html.Text := '';
  with installerArray[integer(stMacApp)] do
    info_message_html.Text := '';
  with installerArray[integer(stLinRPM)] do
    info_message_html.Text := '';
  with installerArray[integer(stLinDeb)] do
    info_message_html.Text := '';
  with installerArray[integer(stPortableApps)] do
    info_message_html.Text := mdInstallerInfo_PortableApps;
  with installerArray[integer(stInstall4J)] do
    info_message_html.Text := '';
  with installerArray[integer(stAdvancedInstaller)] do
    info_message_html.Text := '';
  with installerArray[integer(stInstallAnywhere)] do
    info_message_html.Text := mdInstallerInfo_InstallAnywhere;
  with installerArray[integer(stSetupFactory)] do
    info_message_html.Text := mdInstallerInfo_SetupFactory;
  with installerArray[integer(stQtInstaller)] do
    info_message_html.Text := mdInstallerInfo_QtInstaller;
  with installerArray[integer(stWise)] do
    info_message_html.Text := mdInstallerInfo_Wise;
  with installerArray[integer(stMsixAppx)] do
    info_message_html.Text := mdInstallerInfo_MsixAppx;
  with installerArray[integer(stWinget)] do
    info_message_html.Text := mdInstallerInfo_winget;
  with installerArray[integer(stVisualStudioInstaller)] do
    info_message_html.Text := mdInstallerInfo_VisualStudioInstaller;
  // marker for add installers
end;




//initialize unit

begin
  osdsettings := Tosdsettings.Create;

  // marker for add installers
  knownInstallerList := TStringList.Create;
  knownInstallerList.Add('Visual Studio Installer');
  knownInstallerList.Add('Winget');
  knownInstallerList.Add('Msix_Appx');
  knownInstallerList.Add('Wise');
  knownInstallerList.Add('QtInstaller');
  knownInstallerList.Add('SetupFactory');
  knownInstallerList.Add('InstallAnywhere');
  knownInstallerList.Add('AdvancedInstaller');
  knownInstallerList.Add('Install4j');
  knownInstallerList.Add('PortableApps');
  knownInstallerList.Add('LinRPM');
  knownInstallerList.Add('LinDeb');
  knownInstallerList.Add('MacZip');
  knownInstallerList.Add('MacDmg');
  knownInstallerList.Add('MacPKG');
  knownInstallerList.Add('MacApp');
  knownInstallerList.Add('SFXcab');
  knownInstallerList.Add('BoxStub');
  //knownInstallerList.Add('AdvancedMSI');
  knownInstallerList.Add('InstallShield');
  knownInstallerList.Add('InstallShieldMSI');
  knownInstallerList.Add('MSI');
  knownInstallerList.Add('NSIS');
  knownInstallerList.Add('7zip');
  knownInstallerList.Add('7zipsfx');
  knownInstallerList.Add('InstallAware');
  //knownInstallerList.Add('MSGenericInstaller');
  knownInstallerList.Add('WixToolset');
  knownInstallerList.Add('Bitrock');
  knownInstallerList.Add('SelfExtractingInstaller');
  knownInstallerList.Add('Inno');
  knownInstallerList.Add('DetectedUnknown');
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
    uninstallProg := '$Installdir$\unins000.exe';
    patterns.Add('<description>inno setup</description>');
    patterns.Add('jr.inno.setup');
    link :=
      'http://www.jrsoftware.org/ishelp/topic_setupcmdline.htm';
    comment := '';
    uib_exitcode_function := 'isInnoExitcodeFatal';
    detected := @detectedbypatternwithor;
    //info_message_html.Text := '';
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
    uninstallProg := '$installdir$\uninstall.exe';
    patterns.Add('Nullsoft.NSIS.exehead');
    patterns.Add('nullsoft install system');
    patterns.Add('http://nsis.sf.net/');
    patterns.Add('NSISu_.exe');
    patterns.Add('NSIS Error');
    notpatterns.Add('portableApps.com');
    notpatterns.Add('paf.exe');
    link :=
      'http://nsis.sourceforge.net/Docs/Chapter3.html#installerusage';
    comment := '';
    uib_exitcode_function := 'isNsisExitcodeFatal';
    detected := @detectedbypatternwithor;
    //info_message_html.Text := '';
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
    uninstallProg := '$installdir$\uninstall.exe';
    patterns.Add('InstallShield');
    // Not always there:
    infopatterns.Add('<description>InstallShield.Setup</description>');
    // have to be there
    patterns.Add('InstallShield Wizard');
    patterns.Add('Setup Launcher');
    // only installshieldMSI:
    notpatterns.Add('transforms');
    link :=
      'https://docs.revenera.com/installshield/helplibrary/IHelpSetup_EXECmdLine.htm';
    // outdated  'https://docs.revenera.com/installshield21helplib/helplibrary/IHelpSetup_EXECmdLine.htm';
    // 'https://www.ibm.com/docs/en/personal-communications/12.0?topic=guide-installshield-command-line-parameters'
    // 'https://www.itninja.com/static/090770319967727eb89b428d77dcac07.pdf'
    // broken: 'http://helpnet.flexerasoftware.com/installshield19helplib/helplibrary/IHelpSetup_EXECmdLine.htm';
    comment := '';
    uib_exitcode_function := 'isInstallshieldExitcodeFatal';
    detected := @detectedbypatternwithAnd;
    //info_message_html.Text := mdInstallerInfo_Installshield;
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
    uninstallProg := '$installdir$\uninstall.exe';
    patterns.Add('nstallshield');
    // pattern 'issetup.dll' seems not to be there always
    infopatterns.Add('issetup.dll');
    patterns.Add('transforms');
    patterns.Add('msiexec.exe');
    link :=
      'https://docs.revenera.com/installshield/helplibrary/IHelpSetup_EXECmdLine.htm';
    comment := '';
    uib_exitcode_function := 'isInstallshieldExitcodeFatal';
    detected := @detectedbypatternwithAnd;
    //info_message_html.Text := mdInstallerInfo_Installshield;
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
    uninstallProg := '$installdir$\uninstall.exe';
    patterns.Add('7-Zip Installer');
    link := 'https://www.7-zip.org/faq.html';
    comment := '';
    uib_exitcode_function := 'isMsiExitcodeFatal_short';
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
    uib_exitcode_function := 'isMsiExitcodeFatal_short';
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
    uib_exitcode_function := 'isMsiExitcodeFatal_short';
    detected := @detectedbypatternwithor;
  end;
  (*
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
    notpatterns.Add('Advanced Installer');
    //infopatterns.Add('RunProgram="');
    installErrorHandlingLines.Add(
      'includelog "%opsiLogDir%\"+$ProductId$+".install_log.txt" "50" "utf16le"');
    link :=
      'https://docs.microsoft.com/en-us/windows/desktop/msi/standard-installer-command-line-options';
    comment := '';
    uib_exitcode_function := 'isMsiExitcodeFatal_short';
    detected := @detectedbypatternwithor;
  end;
  *)
  // stWixToolset
  with installerArray[integer(stWixToolset)] do
  begin
    description := 'Wix Toolset (Windows Installer XML)';
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
    notpatterns.Add('Advanced Installer');
    infopatterns.Add('layoutpromptrestartforcerestartnorestartpassivesilentsquietqhelph');
    //infopatterns.Add('RunProgram="');
    installErrorHandlingLines.Add(
      'includelog "%opsiLogDir%\"+$ProductId$+".install_log.txt" "50" "utf16le"');
    link :=
      'https://docs.microsoft.com/en-us/windows/desktop/msi/standard-installer-command-line-options';
    comment := '';
    uib_exitcode_function := 'isMsiExitcodeFatal';
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
    notpatterns.Add('Visual Studio Installer');
    installErrorHandlingLines.Add(
      'includelog "%opsiLogDir%\"+$ProductId$+".install_log.txt" "50" "utf16le"');
    link :=
      'https://docs.microsoft.com/en-us/windows/desktop/msi/standard-installer-command-line-options';
    comment := '';
    uib_exitcode_function := 'isMsiExitcodeFatal_short';
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
    notpatterns.Add('Advanced Installer');
    installErrorHandlingLines.Add(
      'includelog "%opsiLogDir%\"+$ProductId$+".install_log.txt" "50" "utf16le"');
    link :=
      'https://docs.microsoft.com/en-us/windows/desktop/msi/standard-installer-command-line-options';
    comment := '';
    uib_exitcode_function := 'isMsiExitcodeFatal_short';
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
    uninstallProg := '$installdir$\uninstall.exe';
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
    uninstallProg := '$installdir$\uninstall.exe';
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
  with installerArray[integer(stPortableApps)] do
  begin
    description := 'PortableApps';
    silentsetup := '';
    unattendedsetup := '';
    silentuninstall := '';
    unattendeduninstall := '';
    uninstall_waitforprocess := '';
    install_waitforprocess := '';
    uninstallProg := '<none>';
    patterns.Add('portableApps.com');
    patterns.Add('paf.exe');
    link := 'https://portableapps.com/';
    comment := 'selfextracting Executable. Uncompress with 7zip.';
    uib_exitcode_function := 'isGenericExitcodeFatal';
    detected := @detectedbypatternwithand;
    //info_message_html.Text := mdInstallerInfo_PortableApps;
  end;
  with installerArray[integer(stInstall4J)] do
  begin
    description := 'Install4j';
    silentsetup := '-q -overwrite';
    unattendedsetup := '-q -overwrite';
    silentuninstall := '-q';
    unattendeduninstall := '-q';
    uninstall_waitforprocess := '';
    install_waitforprocess := '';
    uninstallProg := 'uninstall.exe';
    patterns.Add('install4j');
    link := 'https://www.ej-technologies.com/resources/install4j/help/doc/help.pdf';
    comment := 'Installs Java based software';
    uib_exitcode_function := 'isGenericExitcodeFatal';
    detected := @detectedbypatternwithand;
    //info_message_html.Text := '';
  end;
  with installerArray[integer(stAdvancedInstaller)] do
  begin
    description := 'Advanced Installer';
    silentsetup :=
      '/exenoui /l* "%opsiLogDir%\$ProductId$.install_log.txt" /qn ALLUSERS=1 REBOOT=ReallySuppress';
    unattendedsetup :=
      '/exebasicui /l* "%opsiLogDir%\$ProductId$.install_log.txt" /qb-! ALLUSERS=1 REBOOT=ReallySuppress';
    silentuninstall := '/exenoui /qn REMOVE=all /norestart';
    // /qb-! führt bei uninstall bei shiluette studio zu 'invalid command line'
    unattendeduninstall := '/exebasicui /qb REMOVE=all /norestart';
    uninstall_waitforprocess := '';
    install_waitforprocess := '';
    uninstallProg := '<setup-file>';
    patterns.Add('Advanced Installer');
    patterns.Add('/exebasicui');
    patterns.Add('/exenoui');
    patterns.Add('/extractlzma');
    patterns.Add('/extract');
    patterns.Add('/exefullui');
    patterns.Add('/deletelzma');

    link := 'https://www.advancedinstaller.com/user-guide/exe-setup-file.html';
    comment := 'Wrapper around MSI (and others).Included files may extracted with /extract';
    uib_exitcode_function := 'isMsiExitcodeFatal';
    detected := @detectedbypatternwithand;
    //info_message_html.Text := '';
  end;
  with installerArray[integer(stInstallAnywhere)] do
  begin
    description := 'InstallAnywhere';
    silentsetup := '-i silent';
    unattendedsetup := '-i silent';
    silentuninstall := '-i silent';
    unattendeduninstall := '-i silent';
    uninstall_waitforprocess := '';
    install_waitforprocess := '';
    uninstallProg := '$Installdir$\Uninstall\Uninstall.exe';
    patterns.Add('InstallAnywhere');
    patterns.Add('InstallAnywhere.Setup');

    link :=
      'https://docs.revenera.com/installanywhere2021/Content/helplibrary/ia_ref_command_line_install_uninstall.htm';
    comment := 'Multi-Platform Installers';
    uib_exitcode_function := 'isGenericExitcodeFatal';
    detected := @detectedbypatternwithand;
    //info_message_html.Text := mdInstallerInfo_InstallAnywhere;
  end;
  with installerArray[integer(stSetupFactory)] do
  begin
    description := 'SetupFactory';
    silentsetup := '/S /NOINIT /W';
    unattendedsetup := '/S /NOINIT /W';
    silentuninstall := '/S /U';
    unattendeduninstall := '/S /U';
    uninstall_waitforprocess := '';
    install_waitforprocess := '';
    uninstallProg := '$Installdir$\Uninstall\Uninstall.exe';
    patterns.Add('Setup Factory');
    patterns.Add('Indigo Rose Corporation');
    infopatterns.Add('<description>Setup Factory Run-time</description>');
    link :=
      'https://www.indigorose.com/webhelp/suf9/index.htm';
    comment := 'SetupFactory';
    uib_exitcode_function := 'isGenericExitcodeFatal';
    detected := @detectedbypatternwithand;
    //info_message_html.Text := mdInstallerInfo_SetupFactory;
  end;
  with installerArray[integer(stQtInstaller)] do
  begin
    description := 'QtInstaller';
    silentsetup :=
      '--verbose --accept-licenses --default-answer --accept-obligations --confirm-command install --root "$installdir$"';
    unattendedsetup :=
      '--verbose --accept-licenses --default-answer --accept-obligations --confirm-command install --root "$installdir$"';
    silentuninstall :=
      '--verbose --accept-licenses --default-answer --accept-obligations --confirm-command purge';
    unattendeduninstall :=
      '--verbose --accept-licenses --default-answer --accept-obligations --confirm-command purge';
    uninstall_waitforprocess := '';
    install_waitforprocess := '';
    uninstallProg := '$Installdir$\Uninstall.exe';
    patterns.Add('Qt Installer Framework');
    patterns.Add('QFontDatabase');
    patterns.Add('QInstaller');
    patterns.Add('QFileDialog');
    //infopatterns.Add('<description>Setup Factory Run-time</description>');
    link :=
      'https://doc.qt.io/qtinstallerframework/';
    comment := 'QtInstaller';
    uib_exitcode_function := 'isGenericExitcodeFatal';
    detected := @detectedbypatternwithand;
    //info_message_html.Text := mdInstallerInfo_QtInstaller;
  end;

  with installerArray[integer(stWise)] do
  begin
    description := 'Wise';
    silentsetup := '/quiet /norestart /log "%opsiLogDir%\$ProductId$.install_log.txt"';
    unattendedsetup :=
      '/passive /norestart /log "%opsiLogDir%\$ProductId$.install_log.txt"';
    silentuninstall := '/quiet /norestart /log "%opsiLogDir%\$ProductId$.install_log.txt"';
    unattendeduninstall :=
      '/passive /norestart /log "%opsiLogDir%\$ProductId$.install_log.txt"';
    uninstall_waitforprocess := '';
    install_waitforprocess := '';
    uninstallProg := '';
    patterns.Add('Wise Installation Wizard');
    patterns.Add('Wise for Windows');
    patterns.Add('InstallTailor');
    infopatterns.Add('WiseApi');
    infopatterns.Add('Wise Installation Wizard prepares the Windows Installer');
    infopatterns.Add('WiseForWindowsInstaller');
    (* also found patterns
    WiseDialogStackWiseNextDialogWiseCurrentWizardWiseCurrentDialog
    WiseDlgSequence
    PalmUsersWiseInstallMobileDeviceWiseInstallPalmWiseInstallnetcfWiseIsoCompEditRegWiseIsoCompInfoWisePalmInfoWiseSQLExecuteWiseSetAssemblyFrameworkPropertiesWiseUninstallMobileDeviceWiseVerifyUser
    WiseDocumentCustomActionsWiseFindSqlClientToolsWiseGetServerFeaturesWiseGetSqlServersWiseGetWinEditionWiseLaunchConditionsWiseRegComPlusAddRemoveWiseRegComPlusInitWiseSqlServerCheckWiseTest
    WiseMDACSuppressReboot /c:"setup.exe /qnt""%s" /r:n /q:a
    WiseDialog
    MsiLogFileLocation
    WiseEditor
    WiseDebugMode
    Wise for Windows Installer
    MsiAssemblyName
    *)
    link :=
      'https://en.wikipedia.org/wiki/Wise_Solutions';
    comment := 'Wise';
    uib_exitcode_function := 'isMsiExitcodeFatal';
    detected := @detectedbypatternwithand;
    //info_message_html.Text := mdInstallerInfo_SetupFactory;
  end;

  with installerArray[integer(stMsixAppx)] do
  begin
    // https://www.advancedinstaller.com/per-machine-msix.html
    description :=
      'Msix / Appx Package';
    silentsetup :=
      'powershell.exe Add-AppProvisionedPackage -online -packagepath <#packagePath#> -skiplicense';
    unattendedsetup :=
      'powershell.exe Add-AppProvisionedPackage -online -packagepath <#packagePath#> -skiplicense';
    silentuninstall :=
      'powershell.exe Remove-AppPackage -AllUsers -package <#packageFullName#> -Confirm:$False';
    //additional uninstall command will be filled to the optionalUninstallCommandLines
    // in get_msixAppx_info
    // 'Get-AppxProvisionedPackage -Online | Where-Object {$_.PackageName -eq "<#packageFullName#>"} | Remove-AppxProvisionedPackage -Online';
    unattendeduninstall :=
      'powershell.exe Remove-AppPackage -AllUsers -package <#packageFullName#> -Confirm:$False';
    uninstall_waitforprocess := '';
    uninstallProg := '';
    installErrorHandlingLines.Add('');
    link :=
      'https://learn.microsoft.com/en-us/windows/msix/overview';
    comment := '';
    uib_exitcode_function := 'isGenericExitcodeFatal';
    detected := @detectedbypatternwithor;
  end;

  with installerArray[integer(stWinget)] do
  begin
    // https://learn.microsoft.com/en-us/windows/package-manager/winget/
    description :=
      'Winget Package';
    silentsetup :=
      ' install --id "<#wingetId#>" --exact ' +
      '--source "<#wingetSource#>" --accept-source-agreements --silent ' +
      '--disable-interactivity --accept-package-agreements';
    unattendedsetup :=
      ' install --id "<#wingetId#>" --exact ' +
      '--source "<#wingetSource#>" --accept-source-agreements --silent ' +
      '--disable-interactivity --accept-package-agreements';
    silentuninstall :=
      ' uninstall --id "<#wingetId#>" --exact ' +
      '--source "<#wingetSource#>" --accept-source-agreements --silent ' +
      '--disable-interactivity';
    unattendeduninstall :=
      ' uninstall --id "<#wingetId#>" --exact ' +
      '--source "<#wingetSource#>" --accept-source-agreements --silent ' +
      '--disable-interactivity';
    uninstall_waitforprocess := '';
    uninstallProg := '';
    installErrorHandlingLines.Add('');
    link :=
      'https://learn.microsoft.com/en-us/windows/package-manager/winget/';
    comment := '';
    uib_exitcode_function := 'isWingetExitcodeFatal';
    detected := @detectedbypatternwithor;
  end;


  with installerArray[integer(stVisualStudioInstaller)] do
  begin
    // https://learn.microsoft.com/en-us/visualstudio/install/use-command-line-parameters-to-install-visual-studio
    description :=
      'Visual Studio Installer';
    silentsetup := ' --quite --norestart --all';
    unattendedsetup := ' --passive --norestart --all';
    silentuninstall :=
      ' uninstall --quite --norestart --installPath "$installDir$"';
    unattendeduninstall :=
      ' uninstall --passive --norestart --installPath "$installDir$"';
    uninstall_waitforprocess := '';
    uninstallProg := '%ProgramFiles32Dir%\Microsoft Visual Studio\Installer\setup.exe';
    patterns.Add('Visual Studio Installer');
    installErrorHandlingLines.Add('');
    link :=
      'https://learn.microsoft.com/en-us/visualstudio/install/use-command-line-parameters-to-install-visual-studio';
    comment := 'Visual Studio Installer';
    //uib_exitcode_function := 'isVSInstallerExitcodeFatal';
    uib_exitcode_function := 'isMsiExitcodeFatal_short';
    detected := @detectedbypatternwithand;
  end;

  // marker for add installers
  reload_installer_info_messages;

  architectureModeList := TStringList.Create;
  architectureModeList.Add('32BitOnly - fix');
  architectureModeList.Add('64BitOnly - fix');
  architectureModeList.Add('both - fix');
  architectureModeList.Add('systemSpecific - fix');
  architectureModeList.Add('selectable');

  myconfiguration := TConfiguration.Create;

  osdsettings := TOSDSettings.Create;


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
  // we need log level 8 in order to see the strings in the binary in the log file
  LogDatei.LogLevel := 8;
  //LogDatei.debug_prog:=true;

  myconfiguration.readconfig;
  LogDatei.log('Finished initialize basedata ', LLInfo);
end.
