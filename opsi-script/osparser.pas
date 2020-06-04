unit osparser;

{$MODE Delphi}
{$RANGECHECKS ON}
{$OVERFLOWCHECKS ON}
{$IOCHECKS ON}
//{$OBJECTCHECKS ON}
{$VARSTRINGCHECKS ON}
{$LONGSTRINGS ON}


(* contains the code that organizes the syntax of scripts *)

// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: Rupert Roeder, detlef oertel
// credits: http://www.opsi.org/credits/



interface

uses
{$IFDEF WINDOWS}
  DynLibs, Windows,
  //JwaTlHelp32,
  registry,
  osregistry,
  oskeyboard,
  osfuncwin3,
  oswmi,
  osswaudit,
{$IFDEF WIN32}
  DSiWin32,
  osfuncwin2,
{$ENDIF WIN32}
  osfuncwin,
  wispecfolder,
  shlobj,
  VersionInfoX,
{$IFNDEF WIN64}
  oslocaladmin,
{$ENDIF WIN64}
{$ENDIF}
{$IFDEF UNIX}
  oslinmount,
  lispecfolder,
  osfunclin,
  oslindesktopfiles,
  baseunix,
  unix,
  osprocessux,
{$ENDIF}
{$IFDEF DARWIN}
  osfuncmac,
{$ENDIF DARWIN}
{$IFDEF GUI}
  Dialogs,
  osmessagedialog,
  osservicepassdlg,
  osshowsysinfo,
  //osinteractivegui,
  //osbatchgui,
  Controls,
  LCLIntf,
  oslistedit,
  osinputstring,
  StdCtrls,
{$ENDIF GUI}
  TypInfo,
  osparserhelper,
  osencoding,
  osconf,
  oszip,
  //DOM,
  //wixml,
  //Process,
  fileutil,
  LazFileUtils,
  SysUtils,
  Types,
  oslog,
  osfunc,
  Classes,
  contnrs,
  IniFiles,
  //windatamodul,
  ldapsend,
  strutils,
  oslocale,
  //osxbase64,
  base64,
  dateutils,
  synautil,
  synaip,
  synsock,
  pingsend,
  oscalc,
  //osdefinedfunctions,
  opsihwbiosinfo,
  osjson,
  oscrypt,
  DOM,
  osxmlsections,
  osxml,
  osprocesses,
  osnetworkcalculator,
  osregex,
  osurlparser,
  ostxstringlist,
  LAZUTF8;

type
  TStatement = (tsNotDefined,
    tsDefinedVoidFunction,
    // start of sectionnames
    tsActions,
    tsProfileActions,
    tsPatchAnyTextFile,
    tsTests, tsPatchIniFile,
    tsHostsPatch, tsRegistryHack,
    tsXMLPatch,
    tsXML2,
    tsIdapiConfig, tsLDAPsearch,
    tsFileActions, tsLinkFolder,
    tsWinBatch, tsDOSBatchFile, tsDOSInAnIcon,
    tsShellBatchFile, tsShellInAnIcon, tsExecutePython,
    tsExecuteWith, tsExecuteWith_escapingStrings,
    tsOpsiServiceCall,
    tsOpsiServiceHashList,
    tsDDEwithProgman,
    // end of section names : tsDDEwithProgman has to be the last one
    // start of other commands  after tsWorkOnStringList
    tsWorkOnStringList,
    tsOpsiServiceCallStat,
    //tsTestCommand,
    tsStayWhileWindowOpen,
    tsCondOpen, tsCondThen, tsCondElse, tsCondClose,
    tsSwitch, tsSwitchCaseOpen, tsSwitchCaseClose,
    tsSwitchDefaultOpen, tsSwitchClose,
    tsLoopStringList, tsLoopForTo,
    tsMessage, tsMessageFile, tsShowBitmap,
    tsImportLib,
    tsIncludeInsert, tsIncludeAppend,
    tsIncludeLog,
    tsShrinkFileToMB,  //internal undocumented
    tsKillTask,
    tsPause, tsSleep, tsComment,
    tsActionProgress,
    tsLogWarning, tsLogError, tsSetSkinDir,
    tsStop, tsExitWindows,
    tsBlockInput,
    tsLocalAdmin,
    tsChangeDirectory,
    tsAddConnection,
    tsSetOldLogLevel,
    tsSetLogLevel,
    tsSetConfidential,
    tsSetUsercontext,
    tsSaveVersionToProfile,
    tsSetOutputLevel,
    tsSetExitOnError,
    tsSetFatalError, tsSetSuccess, tsSetNoUpdate, tsSetSuspended,
    tsSetMarkerErrorNumber,
    tsSetReportMessages, tsSetTimeMark, tsLogDiffTime,
    tsSetDebug_Prog,
    tsFatalOnSyntaxError,
    tsFatalOnRuntimeError,
    tsAutoActivityDisplay,
    tsforceLogInAppendMode,
    tsSetTraceMode, tsSetStayOnTop,
    tsIconizeWinst, tsRestoreWinst, tsNormalizeWinst, tsMaximizeWinst,
    tsWinstVersionRequired,
    tsOpsiscriptVersionRequired,
    tsEncoding,
    tsUpdateEnvironment,
    tsLoadProductProperties,
    tsDefineVar, tsDefineStringList,
    tsDefineFunction,
    tsEndFunction,
    tsShellcall,
    tsPowershellcall,
    tsExecuteSection,
    // tsSetVar should be the last here for loop in FindKindOfStatement
    tsSetVar);




  TStatementNames = array [TStatement] of string [50];
  TPStatementNames = ^TStatementNames;

  TSectionSpecifier = (tsecNoSection, tsecIncluded, tsecExpected);

  TWaitConditions = (ttpWaitOnTerminate, ttpWaitForWindowVanished,
    ttpWaitForWindowAppearing, ttpWaitForProcessEnding,
    ttpWaitTime, ttpWaitTimeout);

  TSetWaitConditions = set of TWaitConditions;

  TLDAPsearchCacheRequest = (tlcrNone, tlcrCache, tlcrCached, tlcrClear);
  TLDAPsearchOutputRequest = (tlorNone, tlorObjects, tlorAttributes, tlorValues);

  TSectionResult = integer;

  TRegistryFormat = (trnf, trfWinst, trfRegedit, trfSysdiff);

  TExitRequest = (txrNoExit,
    txrRegisterforLogout,
    txrRegisterforReboot,
    txrReboot,
    txrImmediateLogout,
    txrImmediateReboot);

  TShutdownRequest = (tsrNoShutdown, tsrRegisterForShutdown);

  TScriptMode = (tsmMachine, tsmLogin);




const
  tsrFatalError = -2;
  tsrExitWindows = -1;
  tsrExitProcess = 0;
  tsrPositive = 1;


  TempUserRegKey = 'PatchNTUserdatTempUser';


  XMLDOMVendor = 'Open XML'; // 'Xerces XML'


type

  TSectionInfo = record
    Sectionkind: TStatement;
    StartLineNo: integer;
    SectionName: string;
    SectionFile: string;
  end;


  TWorkSection = class(TuibIniScript) // class (TXStringList)
  private
    FSectionkind: TStatement;
    FStartLineNo: integer;
    FSectionName: string;
    FNestingLevel: integer;
    FInSwitch: boolean;
    FInCase: boolean;
    FParentSection: TWorkSection;

  public
    constructor Create(const NestLevel: integer; const ParentSection: TWorkSection);
    destructor Destroy; override;

    property StartLineNo: integer read FStartLineNo write FStartLineNo;
    property Name: string read FSectionName write FSectionName;
    property NestingLevel: integer read FNestingLevel write FNestingLevel;
    property SectionKind: TStatement read FSectionKind write FSectionKind;
    property InSwitch: boolean read FInSwitch write FInSwitch;
    property InCase: boolean read FInCase write FInCase;
    property ParentSection: TWorkSection read FParentSection write FParentSection;

  end;


{$IFDEF FPC}
{$ELSE}
  TuibXMLNodeDescription = class(TObject)
  private
    Fxmldoc: TuibXMLDocument;
    elementname: WideString;
    attributes: TStringList;
    Text: WideString;
    positioning: boolean;
    positionOfElement: integer;
    position: integer;
  public
    constructor Create(xmldoc: TuibXMLDocument);
    destructor Destroy; override;

    function evaluate(s: string; var Remaining: string;
      var InfoSyntaxError: string): boolean;

    procedure evaluateElementname;
    procedure evaluateAttribute;
    procedure evaluateText;
  end;

{$ENDIF}




  TuibInstScript = class(TuibIniScript)
  private
    FLogLevel: integer;
    FOutputLevel: integer;
    FNumberOfWarnings: integer;
    FNumberOfErrors: integer;
    FTraceMode: boolean;
    FExitOnError: boolean;
    FReportMessages: boolean;
    FExtremeErrorLevel: integer;
    FFatalOnSyntaxError: boolean;
    FFatalOnRuntimeError: boolean;
    FSuspended: boolean;
    FAutoActivityDisplay: boolean;
    FforceLogInAppendMode: boolean;



    FVarList: TStringList;
    FlistOfStringLists: TStringList;
    FValuesList: TStringList;
    FContentOfStringLists: TObjectList;
    FConstList: TStringList;
    FConstValuesList: TStringList;
    FLastExitCodeOfExe: longint;
    FLastPrivateExitCode: longint; // not seen by getLastExitcode
    FFilename: string;
    FLinesOriginList: TStringList;
    FaktScriptLineNumber: int64;
    FEvalBoolBaseNestLevel: int64;
    FSectionNameList: TStringList;
    // hold section and function names with index of FSectionInfoArray
    FSectionInfoArray: array of TSectionInfo;
    // holds for each section file and startline infos
    FActiveSection: TWorkSection;
    FLastSection: TWorkSection;



  protected
    function getVarValues: TStringList; //nicht verwendet

  public
    FLibList: TStringList;
    constructor Create;
    destructor Destroy; override;

    (* Properties *)
    property LogLevel: integer read FLogLevel write FLogLevel;
    property OutputLevel: integer read FOutputLevel write FOutputLevel;
    property NumberOfWarnings: integer read FNumberOfWarnings;
    property NumberOfErrors: integer read FNumberOfErrors;
    property TraceMode: boolean read FTraceMode write FTraceMode;
    property ExitOnError: boolean read FExitOnError write FExitOnError;
    property FatalOnSyntaxError: boolean read FFatalOnSyntaxError
      write FFatalOnSyntaxError;
    property FatalOnRuntimeError: boolean read FFatalOnRuntimeError
      write FFatalOnRuntimeError;
    property Suspended: boolean read FSuspended write FSuspended;
    property AutoActivityDisplay: boolean read FAutoActivityDisplay
      write FAutoActivityDisplay;
    property forceLogInAppendMode: boolean read FforceLogInAppendMode
      write FforceLogInAppendMode;
    property ExtremeErrorLevel: integer read FExtremeErrorLevel write FExtremeErrorLevel;

    property ReportMessages: boolean read FReportMessages write FReportMessages;

    property varList: TStringList read FvarList write FvarList;
    property valuesList: TStringList read Fvalueslist write FvaluesList;
    property constList: TStringList read FconstList write FConstList;
    property constValuesList: TStringList read FconstValuesList write FconstValuesList;
    property listOfStringLists: TStringList read FlistOfStringLists
      write FlistOfStringLists;
    property ContentOfStringLists: TObjectList
      read FContentOfStringLists write FContentOfStringLists;
    property aktScriptLineNumber: int64 read FaktScriptLineNumber
      write FaktScriptLineNumber;
    property Filename: string read FFilename write FFilename;
    property ActiveSection: TWorkSection read FActiveSection write FActiveSection;
    property LastSection: TWorkSection read FLastSection write FLastSection;
    property LastExitCodeOfExe: longint read FLastExitCodeOfExe;


    (* Infofunktionen *)
    function doInfo(Meldung: string): TSectionResult;
    function doLogEntries(const Lines: string; EntryLevel: integer): TSectionResult;
    function reportError(const Sektion: TWorkSection; LineNo: integer;
      const Content: string; Comment: string): TSectionResult;


    (* Skriptvariable setzen, Ausdruecke analysieren und auswerten *)
    function doSetVar(const section: TuibIniScript; const Expressionstr: string;
      var Remaining: string; var InfoSyntaxError: string): boolean;
      overload;

    function doSetVar(const section: TuibIniScript; const Expressionstr: string;
      var Remaining: string; var InfoSyntaxError: string;
      var NestLevel: integer): boolean; overload;

    function produceStringList
      (const section: TuibIniScript; const s0: string; var Remaining: string;
      var list: TXStringlist; var InfoSyntaxError: string): boolean; overload;

    function produceStringList
      (const section: TuibIniScript; const s0: string; var Remaining: string;
      var list: TXStringlist; var InfoSyntaxError: string;
      var NestLevel: integer; const inDefFuncIndex: integer): boolean; overload;

    function EvaluateString
      (const s0: string; var Remaining: string; var StringResult: string;
      var InfoSyntaxError: string): boolean; overload;

    function EvaluateString
      (const s0: string; var Remaining: string; var StringResult: string;
      var InfoSyntaxError: string; var NestLevel: integer;
      const inDefFuncIndex: integer): boolean; overload;

    function EvaluateBoolean(Input: string; var Remaining: string;
      var BooleanResult: boolean; NestingLevel: integer;
      var InfoSyntaxError: string): boolean;

    function EvaluateBoolean_(Input: string; var Remaining: string;
      var BooleanResult: boolean; NestingLevel: integer;
      var InfoSyntaxError: string): boolean;

    procedure GetWordOrStringExpressionstr(const s: string;
      var resultString, Remaining, errorinfo: string); overload;

    procedure GetWordOrStringExpressionstr(const s: string;
      var resultString, Remaining: string; const WordDelimiterSet: TCharset);
      overload;

    function SkipWordOrStringExpressionstr(const partialS, S: string;
      var Remaining: string; var Error: string): boolean;

    function produceExecLine(const s: string;
      var programfilename, programparas, passparas, winstoption: string;
      var InfoSyntaxError: string): boolean;


    function doXMLAddNamespace(filename: string; const elementname: string;
      const namespace: string): boolean;

    function doXMLRemoveNamespace(filename: string; const elementname: string;
      const namespace: string): boolean;



    (* Spezielle Methoden *)
  {$IFDEF WINDOWS}
    function StartConnection(LocalName, NetResourceName: string;
      WaitTime: longint): TSectionResult;
  {$ENDIF WINDOWS}

    (* Sektion erstellen *)
    procedure loadValidLinesFromFile(FName: string; var Section: TWorkSection);
    //procedure getLinesFromUnicodeFile (Const FName : String; var Section : TWorkSection);
    procedure ApplyTextVariables(var Sektion: TXStringList; CStringEscaping: boolean);
    procedure ApplyTextConstants(var Sektion: TXStringList; CStringEscaping: boolean);
    procedure ApplyTextVariablesToString(var mystr: string; CStringEscaping: boolean);
    procedure ApplyTextConstantsToString(var mystr: string; CStringEscaping: boolean);

    // handle file and line origins
    procedure registerSectionOrigins(mylist: TStringList; filename: string); overload;
    procedure registerSectionOrigins(mylist: TStringList; filename: string;
      secname: string); overload;

    (* Sektionsbearbeitungsmethoden *)
    (* Hilfsmethoden *)
    function initSection(const Sektion: TWorkSection;
      var SaveErrorNumber, SaveWarningNumber: integer): boolean;
    procedure finishSection(const Sektion: TWorkSection;
      const SaveErrorNumber, SaveWarningNumber: integer;
      var DiffNumberOfErrors, DiffNumberOfWarnings: integer);

    (* fuer primaere Sektionen *)
    function doAktionen(Sektion: TWorkSection;
      const CallingSektion: TWorkSection): TSectionResult;
    (* fuer andere Sektionen *)

    function doTextpatch(const Sektion: TWorkSection; Filename: string;
      PatchParameter: string): TSectionResult;


    function doTests(const Sektion: TWorkSection;
      TestParameter: string): TSectionResult;

    function doInifilePatches(const Sektion: TWorkSection; Filename: string;
      PatchParameter: string): TSectionResult;

    function doHostsPatch(const Sektion: TWorkSection;
      HostsFilename: string): TSectionResult;
    function doIdapiConfig(const Sektion: TWorkSection;
      IdapiParameter: string): TSectionResult;

    function doLDAPsearch
      (const Sektion: TWorkSection; cacheRequest: TLDAPsearchCacheRequest;
      outputRequest: TLDAPsearchOutputRequest;
      var output: TXStringList): TSectionResult;
{$IFDEF WINDOWS}
    function doRegistryHack(const Sektion: TWorkSection;
      const RegParameter: string; const flag_force64: boolean): TSectionResult;
    function doRegistryHackInfSource(const Sektion: TWorkSection;
      const RegParameter: string; const flag_force64: boolean): TSectionResult;
    function doRegistryHackRegeditFormat(const Sektion: TWorkSection;
      const RegParameter: string; const flag_force64: boolean): TSectionResult;
    function doRegistryAllNTUserDats(const Sektion: TWorkSection;
      rfSelected: TRegistryFormat; const flag_force64: boolean): TSectionResult;
    function doRegistryAllUsrClassDats(const Sektion: TWorkSection;
      rfSelected: TRegistryFormat; const flag_force64: boolean): TSectionResult;
    function doRegistryNTUserDat(const Sektion: TWorkSection;
      rfSelected: TRegistryFormat; const flag_force64: boolean;
      const UserPath: string): TSectionResult;


{$ENDIF WINDOWS}

    function doXMLPatch(const Sektion: TWorkSection; const XMLFilename: string;
      var output: TXStringList): TSectionResult;

    function doXMLPatch2(const Sektion: TWorkSection; const XMLFilename: string;
      var output: TXStringList): TSectionResult;


    function doOpsiServiceHashList(const Sektion: TWorkSection;
      const parameter: string; var output: TXStringList): TSectionResult;

    function doOpsiServiceCall(const Sektion: TWorkSection;
      const parameter: string; var output: TXStringList): TSectionResult;


    function doFileActions(const Sektion: TWorkSection;
      CopyParameter: string): TSectionResult;


    function doLinkFolderActions(const Sektion: TWorkSection;
      common: boolean): TSectionResult;

    function doDDEwithProgman(const Sektion: TWorkSection; DDEParameter: string;
      SaveddeWithProgman: boolean): TSectionResult;
    function execWinBatch(const Sektion: TWorkSection; WinBatchParameter: string;
      WaitConditions: TSetWaitConditions; ident: string; WaitSecs: word;
      runAs: TRunAs; flag_force64: boolean): TSectionResult;
    function execDOSBatch(const Sektion: TWorkSection; BatchParameter: string;
      ShowCmd: integer; catchOut: boolean; logleveloffset: integer;
      WaitConditions: TSetWaitConditions; var output: TXStringList): TSectionResult;


    function execPython(const Sektion: TWorkSection; PythonParameter: string;
      catchOut: boolean; logleveloffset: integer; WaitConditions: TSetWaitConditions;
      var output: TXStringList): TSectionResult;


    function executeWith(const Sektion: TWorkSection; ExecParameter: string;
      catchOut: boolean; logleveloffset: integer;
      var output: TXStringList): TSectionResult;

    function execShellCall(command: string; archparam: string;
      logleveloffset: integer; FetchExitCodePublic: boolean): TStringList;
      overload;
    function execShellCall(command: string; archparam: string;
      logleveloffset: integer;
      FetchExitCodePublic, FatalOnFail: boolean): TStringList; overload;
{$IFDEF WINDOWS}
    function execPowershellCall(command: string; archparam: string;
      logleveloffset: integer; FetchExitCodePublic, FatalOnFail: boolean;
      handle_policy: boolean): TStringList;
 {$ENDIF WINDOWS}
  end;


procedure CreateAndProcessScript(const Scriptdatei: string;
  NestingLevel: integer; SaveddeWithProgman: boolean;
  var extremeErrorLevel: TErrorLevel);

function DefaultHosts: string;

function GetMACAddress2: string;

function FindKindOfStatement(const Statement: string;
  var SectionSpecifier: TSectionSpecifier; const completeCall: string): TStatement;


function GetNetUser(Host: string; var UserName: string; var ErrorInfo: string): boolean;
(* for Host = '' Username will become the name of the current user of the process *)


function getDecimalCompareSign
  (const decimalString1, decimalString2: string; var sign: integer;
  var InfoSyntaxError: string; stringcomparison: boolean): boolean;

function SearchForSectionLines
  (const selfsection: TUIBInstScript; localsection, callingsection: TWorkSection;
  const Sectionname: string; var Resultlist: TXStringList;
  var StartlineNo: integer; takeCommentLines, takeEmptyLines, trimmed:
  boolean): boolean;

const
  NameInitSektion = 'Initial';
  NameAktionenSektion = 'Actions';
  NameProfileActionsSection = 'ProfileActions';
  //NameAktionenSektion2              = 'Actions';

  (* Registry call parameters *)
  Parameter_SysDiffAddReg = '/AddReg';
  Parameter_RegeditFormat = '/Regedit';
  Parameter_AllNTUserDats = '/AllNTUserDats';
  Parameter_AllUsrClassDats = '/AllUsrClassDats';
  Parameter_RegistryBaseKey = '/BaseKey';
  Parameter_RegistryUsercontext = '/UserContext';
  Parameter_Registry64Bit = '/64Bit';
  Parameter_Registry32Bit = '/32Bit';
  Parameter_RegistrySysNative = '/SysNative';
  Parameter_RegistryNTUserDat = '/NTUserDat:';

  (* File section parameters *)
  Parameter_AllNTUserSendTo = '/AllNTUserSendTo';
  Parameter_AllNTUserProfiles = '/AllNTUserProfiles';
  Parameter_AllNTUser = '/AllNTUser';
  Parameter_64Bit = '/64Bit';
  Parameter_32Bit = '/32Bit';
  Parameter_SysNative = '/SysNative';

  (* 'WinBatch' *)
  ParameterDontWait = '/LetThemGo';
  ParameterWaitOnTerminate = '/WaitOnClose';
  ParameterCloseBehindWindow = '/WaitForWindowVanish';
  ParameterCloseOnWindow = '/WaitForWindowAppearing';
  ParameterWaitSecs = '/WaitSeconds';
  ParameterWaitForProcessEnding = '/WaitForProcessEnding';
  ParameterWaitProcessTimeoutSecs = '/TimeOutSeconds';
  ParameterRunAsAdmin = '/RunAsLoggedOnAdmin';
  ParameterRunAsAdmin1 = '/RunAsLoggedOnAdmin1';
  ParameterRunAsAdmin2 = '/RunAsLoggedOnAdmin2';
  ParameterRunAsAdmin3 = '/RunAsLoggedOnAdmin3';
  ParameterRunAsAdmin4 = '/RunAsLoggedOnAdmin4';
  ParameterRunAsInvoker = '/RunAsInvoker';
  ParameterRunElevated = '/RunElevated';
  ParameterRunAsLoggedOnUser = '/RunAsLoggedOnUser';
  ParameterShowWindowHide = '/WindowHide';


  DefaultWaitProcessTimeoutSecs = 1200; //20 min

  winst_emptyregkeydat = 'winst_emptyregkey.dat';
  TempBatchfilename = '_opsiscript_';
  TempBatchdatei = TempBatchfilename + '.cmd';
  TempBatchDatei_UniqueCount: integer = 0;

  StopInfo = 'Process stopped';
  AbortedInfo = 'Process aborted';
  SpaceForEnvVarAsString = '2048';

  RegNoCallLogout = 0;
  RegCallLogout = 2;
  RegCallReboot = 4;


  (* Synt-Errors *)
  ErrorRemaining = 'Char(s) at end of line not interpreted';

  (* Keys for ExecWith *)
  optionsSplitter = 'WINST';
  passSplitter = 'PASS';
  parameterEscapeStrings = '/EscapeStrings';


var
  PStatNames: TPStatementNames;
  flag_all_ntuser, flag_ntuser, flag_all_usrclass: boolean;
  flag_encoding: string = 'system';
  runLoginScripts: boolean;
  allLoginScripts: boolean;
  inUsercontext: boolean;
  runSilent: boolean;
  scriptMode: TScriptMode;
  runProfileActions: boolean;
  runproductlist: boolean;
  runprocessproducts: boolean;
  opsiWinstStartdir: string;
  Script: TuibInstScript;
  scriptsuspendstate: boolean;
  scriptstopped: boolean;
  inDefFuncLevel: integer = 0;
  inDefFuncIndex: integer = -1; // index of the active defined function
  IfElseEndifLevel: longint = 0; // global nestlevel store (do 18.1.2018)
  inDefFunc3: integer = 0;
  // we are between deffunc and endfunc line (even in a not active code)
  cmd64checked: boolean = False;



//PreDefinedVariableSkinDirectorybinaryName : String;
//PreDefinedVariableSkinDirectoryValue : String;

resourcestring
  rsGetServiceUrl = 'Please enter opsi service URL:';
  rsGetUserName = 'Please enter opsi service user name:';
  rsGetPassword = 'Please enter opsi service user password:';
  rsReadyToContinue = 'Ready to continue ?';
  rsAbortProgram = 'Abort program ?';



implementation

uses
  {$IFDEF GUI}
  osbatchgui,
  osinteractivegui,
  {$ENDIF GUI}
  oswebservice,
  {NB30,}{for getmacadress2}
  osmain,
  osdefinedfunctions;

var
  ps: string;
  markedTime: TDateTime;

  // Expressionstr, Remaining : String;

  OldNumberOfErrors, DiffNumberOfErrors, OldNumberOfWarnings,
  DiffNumberOfWarnings: integer;

  local_opsidata: TOpsi4Data;

  cacheRequest: TLDAPsearchCacheRequest;
  outputRequest: TLDAPsearchOutputRequest;
  ldapResult: TStringList;

//labelinfoAlignmentSave : TAlignment;

type
  TConditions = array [0..100] of boolean;

var
  Conditions: TConditions;
  ThenBranch: TConditions;

//const
//zaehler  : Integer = 0;



function GetString
  (const s: string; var ResultString, Remaining, errorinfo: string;
  StringInStringAllowed: boolean): boolean; overload; forward;
function GetString
  (const s: string; var ResultString, Remaining, errorinfo: string;
  StringInStringAllowed, requote: boolean): boolean; overload; forward;

function SearchForSectionLines
  (const selfsection: TUIBInstScript; localsection, callingsection: TWorkSection;
  const Sectionname: string; var Resultlist: TXStringList;
  var StartlineNo: integer; takeCommentLines, takeEmptyLines, trimmed:
  boolean): boolean;
var
  myworksection: TWorkSection;
begin
  Result := False;
  Resultlist.Clear;

  // look if we are in a subprogram
  // that may have its own sections in it

  if (Resultlist.Count = 0) and (inDefFuncLevel > 0) then
  begin
    // local function
    Logdatei.log('Looking for section: ' + Sectionname +
      ' in local function .', LLDebug3);
    localsection.GetSectionLines(Sectionname, Resultlist,
      StartlineNo, True, True, False);
  end;

  if (Resultlist.Count = 0) and (inDefFuncLevel > 0) then
  begin
    // local function2
    Logdatei.log('Looking for section: ' + Sectionname +
      ' in local function: ' + definedFunctionArray[inDefFuncIndex].Name, LLDebug3);
    myworksection := TWorkSection.Create(NestingLevel, nil);
    myworksection.AddText(definedFunctionArray[inDefFuncIndex].Content.Text);
    myworksection.GetSectionLines(Sectionname,
      Resultlist, StartlineNo, True, True, False);
    myworksection.Free;
  end;

  if Resultlist.Count = 0 then
  begin
    // normal case
    Logdatei.log('Looking for section: ' + Sectionname +
      ' in standard section.', LLDebug3);
    selfsection.GetSectionLines(Sectionname, Resultlist,
      StartlineNo, True, True, False);
  end;(*
  else
  begin
    if 0 <= selfsection.FindSectionheaderIndex(Sectionname) then
      Logdatei.log('Multiple sections with same name: '+ Sectionname +'also found in standard section.',LLWarning);
  end;*)


  if Assigned(callingsection) and (callingsection <> nil) then
  begin
    // subsub case
    if Resultlist.Count = 0 then
    begin
      Logdatei.log('Looking for section: ' + Sectionname +
        ' in calling section.', LLDebug3);
      callingsection.GetSectionLines(Sectionname, Resultlist,
        StartlineNo, True, True, False);
    end;(*
    else
    begin
      if 0 <= callingsection.FindSectionheaderIndex(Sectionname) then
        Logdatei.log('Multiple sections with same name: '+ Sectionname +'also found in calling section.',LLWarning);
    end;*)
  end;



  if Resultlist.Count = 0 then
  begin
    // subsub case
    Logdatei.log('Looking for section: ' + Sectionname +
      ' in global section.', LLDebug3);
    localsection.GetSectionLines(Sectionname, Resultlist,
      StartlineNo, True, True, False);
  end;(*
  else
  begin
    if 0 <= localsection.FindSectionheaderIndex(Sectionname) then
      Logdatei.log('Multiple sections with same name: '+ Sectionname +'also found in global section.',LLWarning);
  end;*)

  if Assigned(callingsection) and (callingsection <> nil) and
    Assigned(callingsection.ParentSection) and (callingsection.ParentSection <> nil) then
  begin
    // subsubsub case
    if Resultlist.Count = 0 then
    begin
      Logdatei.log('Looking for section: ' + Sectionname +
        ' in callingsection.ParentSection section.', LLDebug3);
      callingsection.ParentSection.GetSectionLines(Sectionname, Resultlist,
        StartlineNo, True, True, False);
    end;(*
    else
    begin
      if 0 <= callingsection.ParentSection.FindSectionheaderIndex(Sectionname) then
        Logdatei.log('Multiple sections with same name: '+ Sectionname +'also found in callingsection.ParentSection section.',LLWarning);
    end;*)
  end;

  if Assigned(callingsection) and (callingsection <> nil) and
    Assigned(callingsection.ParentSection) and
    (callingsection.ParentSection <> nil) and
    Assigned(callingsection.ParentSection.ParentSection) and
    (callingsection.ParentSection.ParentSection <> nil) then
  begin
    // subsubsubsub case
    if Resultlist.Count = 0 then
    begin
      Logdatei.log('Looking for section: ' + Sectionname +
        ' in callingsection.FParentSection.FParentSectio section.', LLDebug3);
      callingsection.FParentSection.FParentSection.GetSectionLines(
        Sectionname, Resultlist,
        StartlineNo, True, True, False);
    end;(*
    else
    begin
      if 0 <= callingsection.FParentSection.FParentSection.FindSectionheaderIndex(Sectionname) then
        Logdatei.log('Multiple sections with same name: '+ Sectionname +'also found in callingsection.FParentSection.FParentSection section.',LLWarning);
    end;*)
  end;

  if Resultlist.Count > 0 then
    Result := True;
end;

function getCompareSignStrings(s1: string; s2: string): integer;
var
  s1A, s2A: string;
begin
  s1A := AnsiUpperCase(s1);
  s2A := AnsiUpperCase(s2);
  Result := 0;
  if s1A > s2A then
    Result := 1
  else if s1A < s2A then
    Result := -1;
end;

function getCompareSign(number1: integer; number2: integer): integer;
begin
  Result := 0;
  if number1 > number2 then
    Result := 1
  else if number1 < number2 then
    Result := -1;
end;


function getCompareSignDouble(number1: double; number2: double): integer;
begin
  Result := 0;
  if number1 > number2 then
    Result := 1
  else if number1 < number2 then
    Result := -1;
end;


function getDecimalCompareSign
  (const decimalString1, decimalString2: string; var sign: integer;
  var InfoSyntaxError: string; stringcomparison: boolean): boolean;

var
  decimals1: TXStringList;
  decimals2: TXStringList;
  i: integer;
  comparing: boolean;
  number1: integer;
  number2: integer;

  ///partCompareResult : Integer;
  ///isEqual : Boolean;

begin

  Result := True;
  if (trim(decimalString1) = '') or (trim(decimalString2) = '') then
  begin
    Result := False;
    InfoSyntaxError := 'Missing value for comparing';
    exit;
  end;


  decimals1 := TXStringlist.Create;
  decimals2 := TXStringlist.Create;
  stringsplit(trim(decimalString1), '.', decimals1);
  stringsplit(trim(decimalString2), '.', decimals2);


  sign := 0; // we start with assumption equality
  comparing := True;
  i := 1;

  while Result and comparing do
  begin

    if (decimals1.Count >= i) and (decimals2.Count < i) then
    begin
      comparing := False;
      if sign = 0 // components were equal up to now
      then
        sign := 1;
    end
    else if (decimals1.Count < i) and (decimals2.Count >= i) then
    begin
      comparing := False;
      if sign = 0 then
        sign := -1;
    end
    else if (decimals1.Count < i) and (decimals2.Count < i) then
    begin
      comparing := False;
    end;


    // we continue comparing
    if comparing then
    begin
      if stringcomparison then
      begin
        sign := getCompareSignStrings(decimals1[i - 1], decimals2[i - 1]);
      end

      else
      begin

        try
          number1 := StrToInt(decimals1[i - 1]);
          number2 := StrToInt(decimals2[i - 1]);
        except
          InfoSyntaxError := 'Expecting a sequence of "." and numbers';
          Result := False;
        end;

        if Result then
          sign := getCompareSign(number1, number2);
      end;

      if sign <> 0 then
        comparing := False;
    end;

    Inc(i);
  end;

end;



function hasRelation(orderrelationSymbol: string; orderSign: integer;
  var BooleanResult: boolean; var InfoSyntaxError: string): boolean;
begin
  Result := True;

  if orderRelationSymbol = '=' then
    BooleanResult := (orderSign = 0)
  else if orderRelationSymbol = '>' then
    BooleanResult := (orderSign > 0)
  else if orderRelationSymbol = '>=' then
    BooleanResult := (orderSign >= 0)
  else if orderRelationSymbol = '<' then
    BooleanResult := (orderSign < 0)
  else if orderRelationSymbol = '<=' then
    BooleanResult := (orderSign <= 0)

  else
  begin
    Result := False;
    infoSyntaxError := '"=", ">", ">=" or "<", "<=" expected';
  end;
end;



procedure adjustBounds(var a1, a2: integer; const list1: TXStringList);
// adjust the lower and upper indices to the limits of the real list
begin
  //a1
  if a1 < 0 then
    a1 := 0;
  //a2

  if list1.Count = 0 then
    a2 := -1
  else
  begin

    if a2 >= list1.Count then
      a2 := list1.Count - 1
    else

      while (a2 < 0) do
        a2 := a2 + list1.Count; // move modulo list1.count

  end;

end;

function getOrderSymbol(const s: string;
  var ResultString, Remaining, errorinfo: string): boolean;
begin
  Result := True;
  if Skip('>=', s, remaining, errorinfo) then
    resultstring := '>='
  else if Skip('<=', s, remaining, errorinfo) then
    resultstring := '<='
  else if Skip('>', s, remaining, errorinfo) then
    resultstring := '>'
  else if Skip('<', s, remaining, errorinfo) then
    resultstring := '<'
  else if Skip('=', s, remaining, errorinfo) then
    resultstring := '='
  else
  begin
    Result := False;
    errorinfo := 'relation symbol ("<", "=", ">" ...) expected';
  end;
end;

function GetString
  (const s: string; var ResultString, Remaining, errorinfo: string;
  StringInStringAllowed: boolean): boolean;
begin
  Result := GetString(s, ResultString, Remaining, errorinfo,
    StringInStringAllowed, False);
end;

function GetString
  (const s: string; var ResultString, Remaining, errorinfo: string;
  StringInStringAllowed, requote: boolean): boolean;
(* reads a string that is delimited either by ' or by "  (citation mark);
   if StringInStringallowed then the function interprets a double mark
   of the appropriate kind as being contained in the string.
   resultstring takes the correctly interpreted chars
   remaining gets the original value s if result is false *)

var
  TheMark: char;
  r, Info, DoubleMark: string;
  continue: boolean;
  PartValue: string = '';

  //i : Integer;
  //foundDelimiter : Boolean;

  rorg, rnew: string;

begin
  Result := False;
  resultString := '';
  info := '';
  r := s;
  SetLength(r, length(s));


  if (length(r) < 2) then
    ErrorInfo := 'String marker expected'
  else
  begin
    logdatei.log_prog('r:' + r, llDebug3);
    TheMark := r[1];
    if (TheMark <> '''') and (TheMark <> '"') then
      ErrorInfo := '" or '' expected'
    else
    begin
      DoubleMark := TheMark + TheMark;
      if requote then
      begin
        if pos(DoubleMark, r) = 1 then
        begin
          logdatei.log_prog('may be double quotet: r:' + r, llDebug2);
          rorg := r;
          //GetWord (rorg, rnew, rorg, WordDelimiterWhiteSpace);
          rnew := r;
          logdatei.log_prog('rnew:' + rnew, llDebug2);

          if AnsiEndsStr(DoubleMark, rnew) then
          begin
            logdatei.log_prog('rnew is doublequoted:' + rnew, llDebug2);
            //unquote the wrong mark
            rnew := opsiunquotestr2(rnew, TheMark);
            // quote with the alternative mark
            if TheMark = '"' then
            begin
              r := '''' + rnew + '''';
              TheMark := '''';
            end
            else
            begin
              r := '"' + rnew + '"';
              TheMark := '"';
            end;
          end;
        end;
      end;
      //skipA(TheMark, r, r, errorinfo);
      logdatei.log_prog('r:' + r, llDebug3);
      skipA(TheMark, r, r, errorinfo);
      logdatei.log_prog('r:' + r, llDebug3);
      Continue := True;
      while Continue do
      begin
        (*
        if (r = '') or (r[1] = theMark) then PartValue := '';
        //else
        Begin
           i := 2;
           foundDelimiter := false;
           while not foundDelimiter and (i <= length (r))
           do
           Begin
             if r[i] = theMark
             then foundDelimiter := true
             else inc (i)
           End;

           if foundDelimiter
           then
           Begin
             PartValue := copy (r, 1, i-1);
             r := copy (r, i, length(r));
           End
           else
           Begin
             PartValue := r;
             r := '';
           End;
        End;
        *)
        logdatei.log_prog('r:' + r, llDebug3);
        GetWord(r, PartValue, r, [TheMark]);
        ResultString := ResultString + PartValue;
        logdatei.log_prog('r:' + r + ' ResultString:' + ResultString, llDebug3);

        if StringInStringAllowed then
        begin
          if SkipA(DoubleMark, r, r, Info)  //SkipA (DoubleMark, r, r, Info)
          then
            ResultString := ResultString + TheMark
          else
            Continue := False;
        end
        else
          Continue := False;
        logdatei.log_prog('r:' + r + ' ResultString:' + ResultString, llDebug3);
      end;
      if SkipA(TheMark, r, r, ErrorInfo)//SkipA (TheMark, r, r, ErrorInfo)
      then
        Result := True;
    end;
  end;

  if Result
  //then Remaining := cutLeftBlanks (r)
  then
    Remaining := r
  else
    Remaining := s;
end;


function winstGetTempFileNameWithExt(myext: string): string;
begin
  Randomize;
  Result := TempPath + TempBatchfilename + randomstr(False) + myext;
  //inc(TempBatchDatei_UniqueCount);
  //result := TempPath + TempBatchfilename + inttoStr(TempBatchDatei_UniqueCount) +myext;
end;

function winstGetTempFileName: string;
begin
  Result := winstGetTempFileNameWithExt('.cmd');
end;


procedure deleteTempBatFiles(const tempfilename: string);
var
  files: TuibFileInstall;
begin
  if tempfilename <> '' then
  begin
    // delete the actual file
    if SysUtils.DeleteFile(tempfilename) then
      Logdatei.log('The file: ' + tempfilename + ' has been deleted', LLDebug)
    else
      Logdatei.log('The file: ' + tempfilename + ' was not deleted', LLDebug);
  end;

  // delete files that are older than 2 days
  files := TuibFileInstall.Create;
  try
    if tempfilename <> '' then
      files.alldelete(TempPath + TempBatchfilename + '*', False, True, 2);
  except
    LogDatei.log('not all files "' + TempPath + TempBatchdatei +
      '*"  could be deleted', LLInfo);
  end;
  files.Free;
end;




function GetStringA
  (const s: string; var ResultString, Remaining, errorinfo: string;
  StringInStringAllowed: boolean): boolean;
  // do not delete the heading spaces
  // reads a string that is delimited either by ' or by "  (citation mark);
  // if StringInStringallowed then the function interprets a double mark
  // of the appropriate kind as being contained in the string.
  // resultstring takes the correctly interpreted chars
  // remaining gets the original value s if result is false

var
  TheMark: char;
  r, Info, DoubleMark: string;
  continue: boolean;
  PartValue: string = '';

  //i : Integer;
  //foundDelimiter : Boolean;

begin
  info := '';
  Result := False;
  resultString := '';
  r := s;
  SetLength(r, length(s));


  if (length(r) < 2) then
    ErrorInfo := 'string delimiter expected'
  else
  begin
    TheMark := r[1];
    logdatei.log('r:' + r, llDebug2);
    if (TheMark <> '''') and (TheMark <> '"') then
      ErrorInfo := '" or '' expected '
    else
    begin
      DoubleMark := TheMark + TheMark;
      //skipA(TheMark, r, r, errorinfo);
      skipA(TheMark, r, r, errorinfo);
      Continue := True;
      while Continue do
      begin
        (*
        if (r = '') or (r[1] = theMark)
        then PartValue := ''
        else
        Begin
           i := 1;
           foundDelimiter := false;
           while not foundDelimiter and (i <= length (r))
           do
           Begin
             if r[i] = theMark
             then foundDelimiter := true
             else inc (i)
           End;

           if foundDelimiter
           then
           Begin
             PartValue := copy (r, 1, i-1);
             r := copy (r, i, length(r));
           End
           else
           Begin
             PartValue := r;
             r := '';
           End;
        End;
        *)
        GetWord(r, PartValue, r, [TheMark]);
        ResultString := ResultString + PartValue;

        if StringInStringAllowed then
        begin
          if SkipA(DoubleMark, r, r, Info)  //SkipA (DoubleMark, r, r, Info)
          then
            ResultString := ResultString + TheMark
          else
            Continue := False;
        end
        else
          Continue := False;
      end;
      if SkipA(TheMark, r, r, ErrorInfo)//SkipA (TheMark, r, r, ErrorInfo)
      then
        Result := True;
    end;
  end;

  if Result then
    Remaining := r // cutLeftBlanks (r)
  else
    Remaining := s;
end;

function SeekString
  (const s: string; var ResultString, Remaining, errorinfo: string): boolean;
  {looks for next string in s, delimited by ",
   escaping of " by \ possible}

var
  i: integer;
  state: integer;
  goOn: boolean;
begin
  Result := False;
  ResultString := '';
  errorinfo := '';
  i := 1;
  goOn := True;
  state := 0;
  while goOn do
  begin
    if i > length(s) then
      goOn := False
    else

      case state of
        0: // string not started
        begin
          if s[i] = '"' then
          begin
            state := 1;
          end; //string started
        end;

        1:
        begin
          if s[i] = '\' then
            state := -1 // escape char
          else if s[i] = '"' then
          begin
            state := 2;
            Result := True;
          end //string finished
          else
            ResultString := ResultString + s[i];
        end;
        -1:
        begin
          ResultString := ResultString + s[i];
          state := 1;
        end;

        2: // stripping white space
        begin
          if not ((s[i] = ' ') or (s[i] = #9)) then
            goon := False;
        end
      end;

    Inc(i);
  end;



  if not Result then
    errorinfo := 'string end mark missing';

  Remaining := copy(s, i - 1, length(s));
end;


function FindKindOfStatement(const Statement: string;
  var SectionSpecifier: TSectionSpecifier; const CompleteCall: string): TStatement;
var
  s: TStatement;
  test: string;
  i1, i2, FuncIndex: integer;
  funcname, r: string;
  deffuncFound: boolean;
begin
  Result := tsNotDefined;
  SectionSpecifier := tsecNoSection;
  for s := tsWorkOnStringList to tsSetVar do
    if UpperCase(Statement) = UpperCase(PStatNames^ [s]) then
    begin
      Result := s;
      exit;
    end;
  for s := tsActions to tsDDEwithProgman do
  begin
    if copy(UpperCase(Statement), 1, length(PStatNames^[s])) =
      UpperCase(PStatNames^[s]) then
    begin
      Result := s;
      test := copy(UpperCase(Statement), 1, length(PStatNames^[s]));
      test := UpperCase(PStatNames^[s]);
      if length(PStatNames^[s]) < length(Statement) then
        SectionSpecifier := tsecIncluded
      else
        SectionSpecifier := tsecExpected;

      //special hack for tsExecuteWith
      if Result = tsExecuteWith then
      begin
        i1 := pos(lowercase(OPTIONSSPLITTER) + ' ', lowercase(completecall));
        if (i1 > 0) then
        begin
          i2 := pos(lowercase(parameterEscapeStrings),
            lowercase(copy(completecall, i1, length(completecall))));
          if i2 > 0 then
            Result := tsExecuteWith_escapingStrings;
        end;
      end;
      exit;
    end;
  end;
  // look if we have defined function with result type void
  deffuncFound := False;
  GetWord(CompleteCall, funcname, r, WordDelimiterSet5);
  FuncIndex := definedFunctionNames.IndexOf(LowerCase(funcname));
  if FuncIndex >= 0 then
    if definedFunctionArray[FuncIndex].datatype = dfpVoid then
      deffuncFound := True;
  if deffuncFound then
  begin
    Result := tsDefinedVoidFunction;
    exit;
  end;
end;


function DefaultHosts: string;
begin
  {$IFDEF WINDOWS}
  if UpperCase(ValueOfEnvVar('OS')) = 'WINDOWS_NT' then
    Result := GetWinDirectory + 'system32\drivers\etc\'
  (* DefaultHostsDir0 *)
  else
    Result := GetWinDirectory;
  (* DefaultHostsDir1; *)
  Result := Result + 'hosts';
  {$ELSE WINDOWS}
  Result := '/etc/hosts';
  {$ENDIF WINDOWS}
end;

//from https://github.com/silvioprog/lazsolutions/blob/master/Units/LSUtils.pas
function LSLoadFile(const AFileName: TFileName): string;
begin
  with TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite) do
  begin
    try
      SetLength(Result, Size);
      Read(Pointer(Result)^, Size);
    except
      Result := '';
      Free;
      raise;
    end;
    Free;
  end;
end;

function readFirstLineFromFile(filename: string): string;
var
  myfile: Text;
  myresult: string;
begin
  try
    assignfile(myfile, filename);
    reset(myfile);
    readln(myfile, myresult);
    closefile(myfile);
    Result := myresult;
  except
    Result := '';
    raise;
  end;
end;


{$IFDEF FPC}
//function LSGetMACAddress: string;
function GetMACAddress2: string;
{$IFDEF MSWINDOWS}
type
  TCreateGUIDFunction = function(AGUID: PGUID): longint; stdcall;
{$ENDIF}
var
{$IFDEF UNIX}
  VPath, VDevice: string;
{$ENDIF}
{$IFDEF MSWINDOWS}
  VLibHandle: TLibHandle;
  VCreateGUIDFunction: TCreateGUIDFunction;
  VGUID1, VGUID2: TGUID;
{$ENDIF}
const
  CLSFormatMACMask = '%2.2x-%2.2x-%2.2x-%2.2x-%2.2x-%2.2x';
begin
{$IFDEF UNIX}
  //VDevice := 'eth0';
  VDevice := getMyIpDeciceByDefaultRoute;
  VPath := Format('/sys/class/net/%s/address', [VDevice]);
  if FileExists(VPath) then
    Result := readFirstLineFromFile(VPath)
  else
    Result := 'Could not find the device "' + VDevice + '".';
{$ENDIF}
{$IFDEF MSWINDOWS}
  VLibHandle := LoadLibrary('rpcrt4.dll');
  try
    if VLibHandle <> NilHandle then
    begin
      VCreateGUIDFunction := TCreateGUIDFunction(
        GetProcedureAddress(VLibHandle, 'UuidCreateSequential'));
      if Assigned(VCreateGUIDFunction) then
        if (VCreateGUIDFunction(@VGUID1) = 0) and
          (VCreateGUIDFunction(@VGUID2) = 0) and (VGUID1.D4[2] = VGUID2.D4[2]) and
          (VGUID1.D4[3] = VGUID2.D4[3]) and (VGUID1.D4[4] = VGUID2.D4[4]) and
          (VGUID1.D4[5] = VGUID2.D4[5]) and (VGUID1.D4[6] = VGUID2.D4[6]) and
          (VGUID1.D4[7] = VGUID2.D4[7]) then
          Result := Format(CLSFormatMACMask,
            [VGUID1.D4[2], VGUID1.D4[3], VGUID1.D4[4], VGUID1.D4[5],
            VGUID1.D4[6], VGUID1.D4[7]]);
    end;
  finally
    UnloadLibrary(VLibHandle);
  end;
{$ENDIF}
end;

{$ELSE}

function GetMACAddress2: string;
var
  NCB: PNCB;
  Adapter: PAdapterStatus;

  URetCode: PChar;
  RetCode: char;
  I: integer;
  Lenum: PlanaEnum;
  _SystemID: string;
  TMPSTR: string;
begin
  Result := '';
  _SystemID := '';
  Getmem(NCB, SizeOf(TNCB));
  Fillchar(NCB^, SizeOf(TNCB), 0);

  Getmem(Lenum, SizeOf(TLanaEnum));
  Fillchar(Lenum^, SizeOf(TLanaEnum), 0);

  Getmem(Adapter, SizeOf(TAdapterStatus));
  Fillchar(Adapter^, SizeOf(TAdapterStatus), 0);

  Lenum.Length := chr(0);
  NCB.ncb_command := chr(NCBENUM);
  NCB.ncb_buffer := Pointer(Lenum);
  NCB.ncb_length := SizeOf(Lenum);
  RetCode := Netbios(NCB);

  i := 0;
  repeat
    Fillchar(NCB^, SizeOf(TNCB), 0);
    Ncb.ncb_command := chr(NCBRESET);
    Ncb.ncb_lana_num := lenum.lana[I];
    RetCode := Netbios(Ncb);

    Fillchar(NCB^, SizeOf(TNCB), 0);
    Ncb.ncb_command := chr(NCBASTAT);
    Ncb.ncb_lana_num := lenum.lana[I];
    // Must be 16
    Ncb.ncb_callname := '*               ';

    Ncb.ncb_buffer := Pointer(Adapter);

    Ncb.ncb_length := SizeOf(TAdapterStatus);
    RetCode := Netbios(Ncb);
    //---- calc _systemId from mac-address[2-5] XOR mac-address[1]...
    if (RetCode = chr(0)) or (RetCode = chr(6)) then
    begin
      _SystemId := IntToHex(Ord(Adapter.adapter_address[0]), 2) +
        ':' + IntToHex(Ord(Adapter.adapter_address[1]), 2) + ':' +
        IntToHex(Ord(Adapter.adapter_address[2]), 2) + ':' +
        IntToHex(Ord(Adapter.adapter_address[3]), 2) + ':' +
        IntToHex(Ord(Adapter.adapter_address[4]), 2) + ':' +
        IntToHex(Ord(Adapter.adapter_address[5]), 2);
    end;
    Inc(i);
  until (I >= Ord(Lenum.Length)) or (_SystemID <> '00-00-00-00-00-00');
  FreeMem(NCB);
  FreeMem(Adapter);
  FreeMem(Lenum);
  GetMacAddress2 := _SystemID;
end;

{$ENDIF}


{$IFDEF WINDOWS}
function GetNetUser(Host: string; var UserName: string; var ErrorInfo: string): boolean;
  { for Host = '' Username will become the name of the current user of the process }

var
  pLocalName: PChar;
  pUserName: LPWSTR;


  function ApiCall(var Username, ErrorInfo: string; BuffSize: DWord): boolean;
  var
    errorcode: DWord;
    nBuffSize: DWord;
    pErrorBuff: PChar;
    pNameBuff: PChar;
    nErrorBuffSize: DWord = 0;
    nNameBuffSize: DWord = 0;
    usernamew: unicodestring;

  begin
    Result := False;
    GetMem(pUserName, BuffSize);
    nBuffSize := Buffsize;

    usernamew := '';
    errorCode := WNetGetUserW(nil, pUserName, nBuffSize);


    case errorCode of
      no_error:
      begin
        ErrorInfo := '';
        SetLength(usernamew, StrLen(pUserName));
        usernamew := pUserName;
        username := UTF16ToUTF8(usernamew);
        Result := True;
      end;
      ERROR_NOT_CONNECTED: ErrorInfo :=
          'The device specified by lpszLocalName is not a redirected device or a connected network name.';
      ERROR_MORE_DATA: ApiCall(UserName, ErrorInfo, nBuffSize + 1);
      ERROR_NO_NETWORK: ErrorInfo := 'No network is present.';
      ERROR_EXTENDED_ERROR:
      begin
        GetMem(pErrorBuff, 300);
        GetMem(pNameBuff, 300);
        WNetGetLastError(errorcode, pErrorBuff, nErrorBuffSize,
          pNameBuff, nNameBuffSize);
        ErrorInfo := pErrorBuff;
        FreeMem(pErrorBuff);
        FreeMem(pNameBuff);
      end;
      ERROR_NO_NET_OR_BAD_PATH: ErrorInfo :=
          'None of the providers recognized this local name as having a connection. '
          +
          'However, the network is not available for at least one provider to whom the connection may belong';
      else
        errorInfo := 'NT-Error ' + RemoveLineBreaks(SysErrorMessage(errorCode));
    end;

    if errorCode <> no_error then
      errorInfo := IntToStr(errorCode) + ' ' + errorInfo;

    FreeMem(pUserName);
  end;

begin
  if Host <> '' then
    pLocalName := PChar(Host)
  else
    pLocalName := nil;

  if ApiCall(Username, ErrorInfo, 100) then
    Result := True
  else
    Result := False;
end;

{$ELSE WINDOWS}
function GetNetUser(Host: string; var UserName: string; var ErrorInfo: string): boolean;
  { for Host = '' Username will become the name of the current user of the process }

begin
  //###LINUX
  Result := True;
  Username := getCommandResult('/bin/bash -c whoami');
end;

{$ENDIF WINDOWS}

constructor TWorkSection.Create(const NestLevel: integer;
  const ParentSection: TWorkSection);
begin
  inherited Create;
  FStartLineNo := 0;
  FSectionName := '';
  FNestingLevel := NestLevel;
  FInSwitch := False;
  FInCase := False;
  FParentSection := ParentSection;
end;

destructor TWorkSection.Destroy;
begin
  inherited Destroy;
end;

{$IFDEF FPC}
{$ELSE}
// *** TuibXMLNodeDescription ****


constructor TuibXMLNodeDescription.Create(xmldoc: TuibXMLDocument);
begin
  inherited Create;
  fxmldoc := xmldoc;
end;

destructor TuibXMLNodeDescription.Destroy;
begin

  inherited Destroy;
end;


function TuibXMLNodeDescription.evaluate(s: string; var Remaining: string;
  var InfoSyntaxError: string): boolean;

var
  r: string;
  syntaxCheck: boolean;
  syntaxInfo: string;
  elementname: string;

begin
  r := lowercase(s);

  syntaxCheck := False;
  InfoSyntaxError := '';

  if Skip('(', s, r, syntaxInfo)  // geklammerter Ausdruck
  then
  begin
    if evaluate(r, r, syntaxInfo) and Skip(')', r, r, syntaxInfo) then
      syntaxCheck := True;
  end


  else if Skip('elementname', r, r, syntaxInfo) then
  begin

    if GetString(r, elementname, r, InfoSyntaxError, False) then
    begin
      syntaxCheck := True;
      //Fxmldoc.filterByChildelement(elementname)
    end;

  end

  else if Skip('attribute', r, r, syntaxInfo) then
  begin

  end

  else if Skip('position', r, r, syntaxInfo) then
  begin

  end

  else
  begin
    InfoSyntaxError := 'XML Path description not accepted';
  end;


  Remaining := r;
end;

procedure TuibXMLNodeDescription.evaluateElementname;
begin
end;

procedure TuibXMLNodeDescription.evaluateAttribute;
begin
end;

procedure TuibXMLNodeDescription.evaluateText;
begin
end;

{$ENDIF}



constructor TuibInstScript.Create;

begin
  inherited Create;
  FNumberOfErrors := 0;
  FNumberOfWarnings := 0;

  FExtremeErrorLevel := level_not_initialized;

  FLogLevel := LLinfo;
  FExitOnError := False;
  //FReportMessages := true;
  FReportMessages := osconf.ScriptErrorMessages;
  FTraceMode := False;
  FFatalOnSyntaxError := True;
  FFatalOnRuntimeError := False;
  FSuspended := False;
  //FAutoActivityDisplay := false;
  FAutoActivityDisplay := osconf.AutoActivityDisplay;
  FforceLogInAppendMode := False;
  scriptstopped := False;

  FVarList := TStringList.Create;
  //VarList.add(PreDefinedVariableSkinDirectory);
  FValuesList := TStringList.Create;
  //ValuesList.add(PreDefinedVariableSkinDirectoryValue);

  FlistOfStringLists := TStringList.Create;
  FContentOfStringLists := TObjectList.Create;
  FConstList := TStringList.Create;
  FConstValuesList := TStringList.Create;
  FLinesOriginList := TStringList.Create;
  FLibList := TStringList.Create;
  FsectionNameList := TStringList.Create;
  //FSectionInfoArray := Length(0);
  FActiveSection := nil;
  FLastSection := nil;
end;

destructor TuibInstScript.Destroy;
var
  counter, i: integer;
begin
  FVarList.Free;
  VarList := nil;
  FValuesList.Free;
  ValuesList := nil;
  FlistOfStringLists.Free;
  listOfStringLists := nil;
  FContentOfStringLists.Free;
  ContentOfStringLists := nil;
  FLinesOriginList.Free;
  FLinesOriginList := nil;
  FLibList.Free;
  FLibList := nil;
  FsectionNameList.Free;
  FsectionNameList := nil;
  (*
  counter := length(FSectionInfoArray);
  if counter > 0 then
    for i := 0 to counter - 1 do
    begin
      FSectionInfoArray[i] := nil;
      FSectionInfoArray[i].Free;
    end;
    *)
  SetLength(FSectionInfoArray, 0);
end;



procedure TuibInstScript.LoadValidLinesFromFile(FName: string;
  var Section: TWorkSection);
var
  OriginalList: TXStringList;
  s: string;
  i: integer;
  Encoding2use, usedEncoding: string;
  statkind: TStatement;
  //secname, remaining : string;
  //secindex : integer;
begin
  Section.Clear;
  OriginalList := TXStringList.Create;
  OriginalList.LoadFromFile(ExpandFileName(FName));
  Encoding2use := searchencoding(OriginalList.Text);
  if Encoding2use = '' then
    Encoding2use := 'system';
  OriginalList.Text := reencode(OriginalList.Text, Encoding2use, usedEncoding);
  logdatei.log('Loaded sub from: ' + FName + ' with encoding: ' + usedEncoding, LLDebug);
  for i := 1 to OriginalList.Count do
  begin
    s := trim(OriginalList.Strings[i - 1]);
    Section.Add(s);
    script.FLinesOriginList.Append(FName + ' line: ' + IntToStr(i));
    script.FLibList.Append('false');
    (*
    secname := opsiunquotestr2(s,'[]');
    if secname <> s then
    begin
      // we have a new section
      secindex := Script.FSectionNameList.Add(secname);
      if secindex <> length(script.FSectionInfoArray) then
        LogDatei.log('Error: internal: secindex <> length(script.FSectionInfoArray)',LLCritical);
      setlength(script.FSectionInfoArray, secindex+1);
      script.FSectionInfoArray[secindex].SectionName:=secname;
      script.FSectionInfoArray[secindex].SectionFile:=ExtractFileName(FName);
      script.FSectionInfoArray[secindex].StartLineNo:=i;
    end;
    if pos('deffunc',lowercase(s)) = 1  then
    begin
      // we have a new function
      secname := copy (s,pos('deffunc',lowercase(s)));
      GetWord(secname, secname, remaining,WordDelimiterSet5);
      secindex := Script.FSectionNameList.Add(secname);
      if secindex <> length(script.FSectionInfoArray) then
        LogDatei.log('Error: internal: secindex <> length(script.FSectionInfoArray)',LLCritical);
      setlength(script.FSectionInfoArray, secindex+1);
      script.FSectionInfoArray[secindex].SectionName:=secname;
      script.FSectionInfoArray[secindex].SectionFile:=ExtractFileName(FName);
      script.FSectionInfoArray[secindex].StartLineNo:=i;
    end;
    *)
  end;
  registerSectionOrigins(OriginalList, FName, Section.Name);
  OriginalList.Free;
end;


function TuibInstScript.reportError(const Sektion: TWorkSection;
  LineNo: integer; const Content: string; Comment: string): TSectionResult;
var
  funcname, funcfile, funcmesseage: string;
  originmessage: string;
  funcline: integer;
  i, index: integer;
begin
  try
    Result := tsrPositive;
    if inDefFuncIndex >= 0 then
    begin
      funcname := definedFunctionArray[inDefFuncIndex].Name;
      funcfile := definedFunctionArray[inDefFuncIndex].OriginFile;
      funcline := definedFunctionArray[inDefFuncIndex].OriginFileStartLineNumber;
      funcmesseage := ' in defined function: ' + funcname + ' file: ' +
        ExtractFileName(funcfile) + ' function start at line: ' +
        IntToStr(funcline + 1);
      originmessage := '; origin: ' + funcfile + ' line: ' +
        IntToStr(funcline + LineNo + 1) + '): ';
    end
    else
    begin
      funcname := Sektion.Name;
      index := script.FSectionNameList.IndexOf(funcname);
      if index = -1 then
      begin
        funcmesseage := ' in section: ' + Sektion.Name + '; file: unknown';
        originmessage := '; origin: not found' + '): ';
      end
      else
      begin
        funcfile := Script.FSectionInfoArray[index].SectionFile;
        funcline := Script.FSectionInfoArray[index].StartLineNo;
        funcmesseage := ' in section: ' + funcname + '; file: ' +
          ExtractFileName(funcfile) + '; section start at line: ' +
          IntToStr(funcline + 1);
        //originmessage := '; origin: '+FLinesOriginList.Strings[Sektion.StartLineNo + LineNo]+'): ';
        originmessage := '; origin: ' + funcfile + '; line: ' + IntToStr(
          funcline + LineNo + 1) + '): ';
      end;
    end;
  except
    originmessage := '; origin: not found' + '): ';
    funcmesseage := '; section or function not found';
  end;
  //for i:= 0 to FLinesOriginList.Count -1 do
  //  logdatei.log_prog('FLinesOriginList: '+FLinesOriginList.Strings[i],LLDebug);
  ps := 'Syntax Error in Section: ' + Sektion.Name + ' (Command in line ' +
    IntToStr(Sektion.StartLineNo + LineNo)
    //   + ' Command in line ' + IntToStr (script.aktScriptLineNumber)
    + funcmesseage + originmessage
    //  + ' origin: '+FLinesOriginList.Strings[Sektion.StartLineNo + LineNo-1]+'): '
    //  + ' origin: '+FLinesOriginList.Strings[script.aktScriptLineNumber]+'): '
    + Content + ' -> ' + Comment;
  LogDatei.log(ps, LLCritical);
  if FatalOnSyntaxError then
  begin
    FExtremeErrorLevel := LevelFatal;
    LogDatei.ActionProgress := 'Syntax Error';
  end;
  Inc(FNumberOfErrors);
  //Topsi4data(opsidata).ActionProgress:= 'Script Syntax Error';
  if ReportMessages then
  begin
    ps := 'Section   ' + Sektion.Name + ' (Command in line ' +
      IntToStr(Sektion.StartLineNo + LineNo) + '):' + LineEnding +
      Content + LineEnding + Comment;
    {$IFDEF GUI}
    if MyMessageDlg.WiMessage(ps, [mrOk, mrAbort]) = mrAbort then
      Result := tsrExitProcess;
    {$ELSE GUI}
    writeln(ps);
    //result := tsrExitProcess;
    {$ENDIF GUI}
  end;

end;


function TuibInstScript.getVarValues: TStringList;
var
  k: integer;
begin
  if FVarList <> nil then
  begin
    k := FVarList.IndexOf('%loglevel%');
    if k > -1 then
      FValuesList.strings[k] := IntToStr(Logdatei.LogLevel);
  end;
  Result := FValuesList;
end;


function TuibInstScript.doInfo(Meldung: string): TSectionResult;
begin
  {$IFDEF GUI}
  Result := tsrPositive;
  CentralForm.Label1.Caption := Meldung;

  //FBatchOberflaeche.LabelInfo.alignment := labelinfoAlignmentSave;

  (*
  if FBatchOberflaeche.LabelInfo.Canvas.TextWidth(Meldung) > FBatchOberflaeche.LabelInfo.Width
  then
  Begin
    labelinfoAlignmentSave := FBatchOberflaeche.LabelInfo.alignment;
    FBatchOberflaeche.LabelInfo.alignment := taLeftJustify;
    //FBatchOberflaeche.LabelInfo.Width := FBatchOberflaeche.LabelInfo.Canvas.TextWidth(Meldung);
  End;
  *)

  // do not auto size short labels because this will look ugly
  // use optimalfill to decrease the font only on large labels
  FBatchOberflaeche.LabelInfo.Font.Size := LableInfoDefaultFontSize;
  if FBatchOberflaeche.LabelInfo.Canvas.TextWidth(Meldung) >
    (FBatchOberflaeche.LabelInfo.Width - FBatchOberflaeche.LabelInfo.Width div 5) then
    FBatchOberflaeche.LabelInfo.OptimalFill := True
  else
  begin
    FBatchOberflaeche.LabelInfo.OptimalFill := False;
    FBatchOberflaeche.LabelInfo.Font.Size := LableInfoDefaultFontSize;
    ;
  end;


  FBatchOberflaeche.setInfoLabel(Meldung);
  {$ELSE GUI}
  Result := tsrPositive;
  writeln(Meldung);
  {$ENDIF GUI}
end;

function TuibInstScript.doLogEntries(const Lines: string;
  EntryLevel: integer): TSectionResult;
var
  ManyLines, line: string;
  i: byte;
begin
  Result := 0;
  ManyLines := Lines;
  repeat
    i := pos(LineEnding, ManyLines);
    if i > 0 then
    begin
      line := copy(ManyLines, 1, i - 1);
      ManyLines := copy(ManyLines, i + 2, length(ManyLines));
    end
    else
      line := ManyLines;
    LogDatei.log(Line, EntryLevel);

  until i = 0;
  {$IFDEF GUI}
  if logdatei.LogLevel >= EntryLevel then
  begin
    if Line = PStatNames^ [tsCondOpen] then
      Line := '|';
    if Line = PStatNames^ [tsCondThen] then
      Line := '/';
    if Line = PStatNames^ [tsCondElse] then
      Line := '-';
    if Line = PStatNames^ [tsCondClose] then
      Line := '\';
    CentralForm.Label2.Caption := Line;
    FBatchOberflaeche.setDetailLabel(Line);
  end;

  ProcessMess;
  {$ENDIF GUI}

  Result := tsrPositive;
end;


function TuibInstScript.initSection(const Sektion: TWorkSection;
  var SaveErrorNumber, SaveWarningNumber: integer): boolean;
begin
  Result := True;
  if Sektion.Count = 0 then
    Result := False;

  SaveErrorNumber := LogDatei.NumberOfErrors;
  SaveWarningNumber := LogDatei.NumberOfWarnings;
  //LogDatei.ErrorNumberMarked := SaveErrorNumber;

  (* LogDatei.LogSIndentLevel := Sektion.NestingLevel; *)
  ps := '';
  LogDatei.log(ps, LLNotice);
  if Sektion.Count > 0 then
  begin
    //ps := 'Execution of ' + Sektion.Name;
    //LogDatei.log (ps, LLNotice);
  end
  else
    // this case should be captured beforehand
    LogDatei.log('Warning: Section  "' + Sektion.Name +
      '"  does not exist or is empty', LLWarning);

  //ps := (*  'Ausfuehrung von ' +  *) copy (Sektion.Name, length (PStatNames^ [Sektion.fSectionKind]) + 1,
  //                                length (Sektion.Name));
  ps := Sektion.Name;
  {$IFDEF GUI}
  CentralForm.Label2.Caption := ps;
  FBatchOberflaeche.setDetailLabel(CentralForm.Label2.Caption);
  {$ENDIF GUI}
end;

procedure TuibInstScript.finishSection(const Sektion: TWorkSection;
  const SaveErrorNumber, SaveWarningNumber: integer;
  var DiffNumberOfErrors, DiffNumberOfWarnings: integer);
begin
  DiffNumberOfErrors := LogDatei.NumberOfErrors - SaveErrorNumber;
  DiffNumberOfWarnings := LogDatei.NumberOfWarnings - SaveWarningNumber;
  FNumberOfErrors := SaveErrorNumber + DiffNumberOfErrors;
  FNumberOfWarnings := SaveWarningNumber + DiffNumberOfWarnings;
  {$IFDEF GUI}
  CentralForm.Label2.Caption := CentralForm.Label2.Caption + ' finished';
  FBatchOberflaeche.setDetailLabel(CentralForm.Label2.Caption);

  (* LogDatei.LogSIndentLevel := Sektion.NestingLevel; *)
  ProcessMess;
  {$ENDIF GUI}
end;

function TuibInstScript.doTextpatch(const Sektion: TWorkSection;
  Filename: string; PatchParameter: string): TSectionResult;

var
  i, j, insertLineIndx: integer;
  methodname: string = '';
  s: string = '';
  r: string = '';
  s0: string = '';
  s1: string = '';
  s2: string = '';
  startofline: string = '';
  oldLine: string = '';
  old_s2: string = '';
  x: string = '';
  found: boolean;
  lastfind: boolean;
  PatchListe: TpatchList;
  ErrorInfo: string = '';
  FileError: string = '';
  syntaxCheck: boolean;
  indx: integer;
  d, sum: integer;
  saveToOriginalFile: boolean;
  working: boolean;
  goOn: boolean;
  secondStringList: TStringList;
  PatchFilename: string = '';
  Patchdatei: TuibPatchIniFile;
  ProfileList: TStringList;
  pc: integer = 0;

  procedure CheckRemainder(var SyntaxCheck: boolean);
  begin
    if r <> '' then
      errorinfo := ErrorRemaining
    else
      syntaxCheck := True;
  end;



  procedure doTextpatchMain(const Section: TXStringList; const presetDir: string);
  var
    i: integer = 0;
    index: integer = 0;
    patchlistcounter: integer = 0;
    workingSection: TXStringList;
    NameValueSeparator: char;

  begin
    //ps := LogDatei.LogSIndentPlus (+3) + 'FILE ' +  PatchdateiName;
    //LogDatei.log (ps, LevelWarnings);
    Logdatei.log('', LLInfo);
    Logdatei.log('Patching: ' + PatchFilename, LLInfo);
    ps := LogDatei.LogSIndentPlus(+3) + 'FILE ' + PatchFilename;
    LogDatei.log(ps, LevelWarnings);

    workingSection := TXStringList.Create;
    workingSection.Assign(Section);
    workingSection.GlobalReplace(1, '%userprofiledir%',
      copy(presetDir, 1, length(presetDir) - 1), False);
    workingSection.GlobalReplace(1, '%currentprofiledir%',
      copy(presetDir, 1, length(presetDir) - 1), False);


    if not FileExists(ExpandFileName(PatchFilename)) then
    begin
      ps := LogDatei.LogSIndentPlus(+3) +
        'Info: This file does not exist and will be created ';
      LogDatei.log(ps, LLInfo);
      LogDatei.NumberOfHints := Logdatei.NumberOfHints + 1;

      if CreateTextfile(ExpandFileName(PatchFilename), ErrorInfo) then
      begin
        if ErrorInfo <> '' then
        begin
          ps := LogDatei.LogSIndentPlus(+3) + 'Warning: ' + ErrorInfo;
          LogDatei.log(ps, LLWarning);
        end;
      end
      else
      begin
        ps := LogDatei.LogSIndentPlus(+3) + 'Error: ' + ErrorInfo;
        LogDatei.log(ps, LLError);
        exit; // ------------------------------  exit
      end;
    end;


    ProcessMess;
    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

    // create the list we work on
    PatchListe := TPatchList.Create;
    PatchListe.Clear;
    PatchListe.ItemPointer := -1;
    PatchListe.LoadFromFile(ExpandFileName(PatchFilename));
    PatchListe.Text := reencode(PatchListe.Text, 'system');
    saveToOriginalFile := True;
    lastfind := False;

    i := 1;
    syntaxcheck := True;
    while (i <= workingSection.Count) and syntaxcheck do
    begin
      ErrorInfo := '';
      syntaxCheck := False;
      r := cutLeftBlanks(workingSection.strings[i - 1]);
      if (r = '') or (r[1] = LineIsCommentChar) then
        syntaxCheck := True
      // continue
      else
      begin
        logdatei.log('Patchtextfile: command: ' + r, LLDebug3);
        GetWord(r, methodname, r, WordDelimiterSet1);

        if LowerCase(methodname) = 'sorted' then
        begin
          CheckRemainder(syntaxCheck);

          if SyntaxCheck then
          begin
            PatchListe.Sort;
            PatchListe.Sorted := True;
          end;
        end

        else if LowerCase(methodname) = 'findline' then
        begin
          if GetString(r, s, r, errorinfo, True) then
            CheckRemainder(syntaxCheck);

          if syntaxCheck then
            PatchListe.ItemPointer :=
              PatchListe.FindFirstItem(s, False, PatchListe.ItemPointer, lastfind);
        end

        else if LowerCase(methodname) = 'findline_containing' then
        begin
          if GetString(r, s, r, errorinfo, True) then
            CheckRemainder(syntaxCheck);

          if syntaxCheck then
            PatchListe.ItemPointer :=
              PatchListe.FindFirstItemWith(s, False, PatchListe.ItemPointer);
        end

        else if LowerCase(methodname) = 'findline_startingwith' then
        begin
          LogDatei.log('r:' + r + ' s:' + s, LLDebug3);
          if GetString(r, s, r, errorinfo, True) then
            CheckRemainder(syntaxCheck);
          LogDatei.log('r:' + r + ' s:' + s, LLDebug3);
          if syntaxCheck then
            PatchListe.ItemPointer :=
              PatchListe.FindFirstItemStartingWith(s, False, PatchListe.ItemPointer);
        end

        else if LowerCase(methodname) = lowerCase('DeleteAllLines_StartingWith')
        then
        begin
          if GetString(r, s, r, errorinfo, True) then
            CheckRemainder(syntaxCheck);

          if syntaxCheck then
          begin
            indx := PatchListe.FindFirstItemWith(s, False, -1);
            while indx > -1 do
            begin
              PatchListe.Delete(indx);
              indx := PatchListe.FindFirstItemStartingWith(s, False, -1);
            end;
          end;
        end

        else if LowerCase(methodname) = 'advanceline' then
        begin
          syntaxCheck := True;
          if r = '' then
            d := 1
          else
            try
              d := StrToInt(r);
            except
              syntaxCheck := False;
              errorInfo := r + ' is not an integer value ';
            end;

          if SyntaxCheck then
          begin
            sum := PatchListe.ItemPointer + d;
            if sum < 0 then
              PatchListe.ItemPointer := 0
            else if sum > PatchListe.Count - 1 then
              PatchListe.ItemPointer := PatchListe.Count - 1
            else
              PatchListe.ItemPointer := sum;
          end;
        end

        else if LowerCase(methodname) = lowercase('GoToTop') then
        begin
          CheckRemainder(syntaxCheck);

          //if PatchListe.count > 0
          //then
          PatchListe.SetItemPointer(-1);
        end

        else if LowerCase(methodname) = LowerCase('GoToBottom') then
        begin
          CheckRemainder(syntaxCheck);

          if PatchListe.Count > 0 then
            PatchListe.ItemPointer := PatchListe.Count - 1;
        end

        else if (LowerCase(methodname) = 'insert_line') or
          (LowerCase(methodname) = 'insertline') then
        begin
          if GetStringA(r, s, r, errorinfo, True) then
            CheckRemainder(syntaxCheck);

          if syntaxCheck then
            if (PatchListe.ItemPointer > -1) then
              PatchListe.insert(PatchListe.ItemPointer, s)
            else if PatchListe.Count = 0 then
              PatchListe.add(s)
            else
              PatchListe.insert(0, s);

        end

        else if (LowerCase(methodname) = 'append_line') or
          (LowerCase(methodname) = 'appendline') then
        begin
          if GetStringA(r, s, r, errorinfo, True) then
            CheckRemainder(syntaxCheck);

          if syntaxCheck then
            if (PatchListe.ItemPointer > -1) and
              (PatchListe.ItemPointer < PatchListe.Count - 1) then
            begin
              PatchListe.ItemPointer := PatchListe.ItemPointer + 1;
              PatchListe.insert(PatchListe.ItemPointer, s);
            end
            else
              PatchListe.add(s);
        end

        else if (LowerCase(methodname) = 'append_file') or
          (LowerCase(methodname) = 'appendfile') then
        begin
          if GetString(r, s1, r, errorinfo, True) then
            CheckRemainder(syntaxCheck);

          if syntaxCheck then
          begin
            s1 := ExpandFileName(s1);
            working := CheckFileExists(s1, FileError);
            if working then
            begin
              try
                secondStringList := TStringList.Create;
                secondStringList.LoadFromFile(ExpandFileName(s1));
                secondStringList.Text := reencode(secondStringList.Text, 'system');
                PatchListe.addStrings(secondStringList);
                secondStringList.Free;
              except
                on ex: Exception do
                begin
                  FileError := ex.message;
                  working := False;
                end;
              end;
            end;

            if working then
              LogDatei.log('Appended "' + s1 + '"', LevelComplete)
            else
            begin
              LogDatei.log('Error: Could not append "' + s1 + '" , ' + FileError,
                LLError);
            end;
          end;
        end

        else if LowerCase(methodname) = 'deletetheline' then
        begin
          CheckRemainder(syntaxCheck);

          if syntaxCheck then
            PatchListe.Delete(PatchListe.ItemPointer);
        end

        else if (LowerCase(methodname) = 'deleteline') or
          (LowerCase(methodname) = 'delete_line') then
        begin
          if GetString(r, s1, r, errorinfo, True) then
            CheckRemainder(syntaxCheck);

          if syntaxCheck then
          begin
            indx := PatchListe.FindFirstItem(s1, False,
              PatchListe.ItemPointer, lastfind);
            if lastfind then
              PatchListe.Delete(indx);
          end;
        end

        else if (LowerCase(methodname) = 'addline') or
          (LowerCase(methodname) = 'add_line') then
        begin
          if GetString(r, s, r, errorinfo, True) then
            CheckRemainder(syntaxCheck);

          if syntaxCheck then
            PatchListe.Add(s);
        end

        else if (LowerCase(methodname) = LowerCase('setKeyValueSeparator')) then
        begin
          if GetString(r, s, r, errorinfo, True) then
            CheckRemainder(syntaxCheck);
          if Length(s) = 1 then
          begin
            NameValueSeparator := char(s[1]);
            LogDatei.log('KeyValueSeparator changed from: >' +
              PatchListe.NameValueSeparator + '< to : >' +
              NameValueSeparator + '<', LLInfo);
          end
          else
          begin
            syntaxCheck := False;
            ErrorInfo := 'KeyValueSeparator has to be a char';
          end;
          if syntaxCheck then
            PatchListe.NameValueSeparator := NameValueSeparator;
        end

        else if (LowerCase(methodname) = LowerCase('setValueByKey')) then
        begin
          if GetStringA(trim(r), s1, r, errorinfo, True) then
          begin
            logdatei.log('key= ' + s1, LLDebug2);
            if GetStringA(trim(r), s2, r, errorinfo, True) then
            begin
              logdatei.log('value= ' + s2, LLDebug2);
              syntaxCheck := True;
            end
            else
              syntaxCheck := False;
          end
          else
            syntaxCheck := False;
          if syntaxCheck then
          begin
            // the fast way:
            index := PatchListe.IndexOfName(s1);
            if index = -1 then
            begin
              // let us retry with trimed keys
              for patchlistcounter := 0 to PatchListe.Count - 1 do
              begin
                if PatchListe.Names[patchlistcounter] <> '' then
                  if lowerCase(trim(PatchListe.Names[patchlistcounter])) =
                    lowerCase(trim(s1)) then
                    index := PatchListe.IndexOfName(PatchListe.Names[patchlistcounter]);
              end;
            end;
            if index = -1 then
            begin
              logdatei.log('Key: ' + s1 + ' not found - creating', LLInfo);
              if (PatchListe.ItemPointer > -1) and
                (PatchListe.ItemPointer < PatchListe.Count - 1) then
              begin
                PatchListe.ItemPointer := PatchListe.ItemPointer + 1;
                PatchListe.insert(PatchListe.ItemPointer,
                  s1 + PatchListe.NameValueSeparator + s2);
              end
              else
                PatchListe.add(s1 + PatchListe.NameValueSeparator + s2);
            end
            else
            begin
              logdatei.log('Key: ' + s1 + ' found - Value was: >' +
                PatchListe.ValueFromIndex[index] + '< setting to: >' +
                s2 + '<', LLInfo);
              PatchListe.ValueFromIndex[index] := s2;
            end;
          end;
        end

        else if (LowerCase(methodname) = LowerCase('searchAndReplace')) then
        begin
          if GetStringA(trim(r), s1, r, errorinfo, True) then
          begin
            logdatei.log('search string= ' + s1, LLDebug2);
            if GetStringA(trim(r), s2, r, errorinfo, True) then
            begin
              logdatei.log('replace string= ' + s2, LLDebug2);
              syntaxCheck := True;
            end
            else
              syntaxCheck := False;
          end
          else
            syntaxCheck := False;
          if syntaxCheck then
          begin
            if patchliste.GlobalReplace(1, s1, s2, False) then
              logdatei.log('Replaced all occurrences of  "' + s1 +
                '" by "' + s2 + '".', LLInfo)
            else
              logdatei.log('No occurrences of  "' + s1 +
                '" found - nothing replaced.', LLInfo);
          end;
        end

        else if (LowerCase(methodname) = lowerCase('SaveToFile')) then
        begin
          saveToOriginalFile := False;
          LogDatei.log('saveToOriginalFile = false', LevelComplete);
          if GetString(r, s1, r, ErrorInfo, False) then
            CheckRemainder(syntaxCheck);

          if SyntaxCheck then
          begin
            s1 := ExpandFileName(s1);
            working := FileExists(s1) or CreateTextFile(s1, FileError);
            if working then
              try
                PatchListe.SaveToFile(s1);
              except
                on ex: Exception do
                begin
                  FileError := ex.message;
                  working := False;
                end;
              end;

            if working then
              LogDatei.log('The lines are saved to "' + s1 + '"', LevelComplete)
            else
            begin
              LogDatei.log('Error: Could not save lines to "' + s1 +
                '" , ' + FileError,
                LLError);
            end;
          end;
        end

        else if (LowerCase(methodname) = lowerCase('Subtract_File')) then
        begin
          if GetString(r, s1, r, ErrorInfo, False) then
            CheckRemainder(syntaxCheck);

          if SyntaxCheck then
          begin
            s1 := ExpandFileName(s1);
            working := CheckFileExists(s1, FileError);
            if working then
            begin
              try
                secondStringList := TStringList.Create;
                secondStringList.LoadFromFile(ExpandFileName(s1));
                secondStringList.Text := reencode(secondStringList.Text, 'system');
                patchliste.SetItemPointer(0);

                j := 0;
                goOn := True;
                while (patchliste.Count > 0) and (j + 1 <= secondStringList.Count) and
                  goOn do
                begin
                  if patchliste.strings[0] = secondStringList.strings[j]
                  then
                  begin
                    patchliste.Delete(0);
                    Inc(j);
                  end
                  else
                    goOn := False;
                end;
                secondStringList.Free;
              except
                on ex: Exception do
                begin
                  FileError := ex.message;
                  working := False;
                end;
              end;
            end;

            if working then
              LogDatei.log('Subtracted "' + s1 + '"', LevelComplete)
            else
              LogDatei.log('Error: Could not substract "' + s1 + '" , ' + FileError,
                LLError);
          end;
        end

        else if LowerCase(methodname) = lowercase('Set_Mozilla_Pref') then
        begin

          //purify layout
          while PatchListe.GlobalReplace(1, ' (', '(', True) do ;
          while PatchListe.GlobalReplace(1, '( ', '(', True) do ;

          if Skip('(', r, r, ErrorInfo) and GetStringA(r, s0, r,
            ErrorInfo, False) and Skip(',', r, r, ErrorInfo) and
            GetStringA(r, s1, r, ErrorInfo, False) and Skip(',', r, r, ErrorInfo) then
          begin
            if (length(r) > 0) and (r[length(r)] = ')') then
            begin
              syntaxcheck := True;
              s2 := copy(r, 1, length(r) - 1);
            end
            else
              ErrorInfo := ErrorRemaining;

            if syntaxCheck then
            begin
              insertLineIndx := -1;
              PatchListe.ItemPointer :=
                PatchListe.FindFirstItemStartingWith(s0 + '("' + s1 + '"', True, -1);
              while PatchListe.ItemPointer > -1 do
              begin
                insertLineIndx := PatchListe.ItemPointer;
                PatchListe.Delete(insertLineIndx);
                PatchListe.ItemPointer :=
                  PatchListe.FindFirstItemStartingWith(s0 + '("' + s1 + '"', True, -1);
              end;
              if insertLineIndx > -1 then
                PatchListe.Insert(insertLineIndx, s0 + '("' + s1 + '", ' + s2 + ');')
              else
                PatchListe.add(s0 + '("' + s1 + '", ' + s2 + ');');
            end;
          end;
        end

        else if LowerCase(methodname) = lowercase('Set_Netscape_User_Pref') then
        begin

          if Skip('(', r, r, ErrorInfo) and GetStringA(r, s1, r,
            ErrorInfo, False) and Skip(',', r, r, ErrorInfo) then
            if (length(r) > 0) and (r[length(r)] = ')') then
            begin
              syntaxcheck := True;
              s2 := copy(r, 1, length(r) - 1);
            end
            else
              ErrorInfo := ErrorRemaining;

          if syntaxCheck then
          begin
            //PatchListe.Sort;   --- entfernt wegen Mozilla-Kommentaren in prefs.js
            //PatchListe.Sorted := true;   --- entfernt wegen Mozilla-Kommentaren in prefs.js
            PatchListe.ItemPointer := PatchListe.FindFirstItemWith(s1, False, -1);
            PatchListe.Delete(PatchListe.ItemPointer);
            PatchListe.Add('user_pref("' + s1 + '", ' + s2 + ');');
          end;
        end

        else if (LowerCase(methodname) = lowercase(
          'AddStringListElement_To_Mozilla_Pref')) then
        begin

          //purify layout
          while PatchListe.GlobalReplace(1, ' (', '(', True) do ;
          while PatchListe.GlobalReplace(1, '( ', '(', True) do ;

          if Skip('(', r, r, ErrorInfo) and GetStringA(r, s0, r,
            ErrorInfo, False) and Skip(',', r, r, ErrorInfo) and
            GetStringA(r, s1, r, ErrorInfo, False) and Skip(',', r, r, ErrorInfo) and
            GetStringA(r, s2, r, ErrorInfo, False) and Skip(')', r, r, ErrorInfo)
          then
            if r = '' then
              syntaxcheck := True
            else
              ErrorInfo := ErrorRemaining;


          if syntaxCheck then
          begin
            insertLineIndx := -1;
            startofline := s0 + '("' + s1 + '"';

            PatchListe.ItemPointer :=
              PatchListe.FindFirstItemStartingWith(startofline, True, -1);
            while PatchListe.ItemPointer > -1
              // we treat only the last line of this type, eliminating all others
              do
            begin
              insertLineIndx := PatchListe.ItemPointer;
              oldline := PatchListe.Strings[insertLineIndx];
              PatchListe.Delete(insertLineIndx);
              PatchListe.ItemPointer :=
                PatchListe.FindFirstItemStartingWith(startofline, True, -1);
            end;

            if insertLineIndx > -1 then
            begin
              syntaxCheck := False;

              if skip(startofLine, oldLine, oldLine, ErrorInfo) and
                Skip(',', oldLine, oldLine, ErrorInfo) and
                GetStringA(oldLine, old_s2, oldline, ErrorInfo, False) and
                Skip(')', oldLine, oldLine, ErrorInfo) then
                syntaxCheck := True;
            end;
          end;


          if syntaxCheck then
          begin
            if insertLineIndx > -1 then
            begin
              //search if s2 already is an element of existing list

              oldLine := old_s2;

              //get first list element
              GetWord(oldLine, x, oldLine, [',']);
              if (oldLine <> '') then // list not finished, delete ','
                System.Delete(oldLine, 1, 1);

              //first element equal to added string?
              found := (lowerCase(x) = lowerCase(s2));

              while (oldLine <> '') and not found do
              begin
                GetWord(oldLine, x, oldLine, [',']);
                if (oldLine <> '') then
                  System.Delete(oldLine, 1, 1);
                if lowerCase(x) = lowerCase(s2) then
                  found := True;
              end;

              if found then
                //reinsert the old line
                PatchListe.Insert(insertLineIndx,
                  startOfLine + ', "' + old_s2 + '");')
              else
                //append s2
                PatchListe.Insert(insertLineIndx,
                  startOfLine + ', "' + old_s2 + ', ' + s2 + '");');
            end
            else
              PatchListe.Add(
                startOfLine + ', "' + s2 + '");');

          end;

        end


        else if (LowerCase(methodname) = lowercase(
          'AddStringListElement_To_Netscape_User_Pref')) then
        begin

          if Skip('("', r, r, ErrorInfo) then
          begin
            GetWord(r, s1, r, ['"']);
            if Skip('"', r, r, ErrorInfo) then
              if Skip(',', r, r, ErrorInfo) then
                if Skip('"', r, r, ErrorInfo) then
                begin
                  GetWord(r, s2, r, ['"']);
                  if Skip('"', r, r, ErrorInfo) then
                    if Skip(')', r, r, ErrorInfo) then
                      syntaxCheck := True;
                end;
          end;

          if syntaxCheck then
          begin
            PatchListe.Sort;
            PatchListe.Sorted := True;
            PatchListe.ItemPointer := PatchListe.FindFirstItemWith(s1, False, -1);

            if PatchListe.ItemPointer > -1 then
            begin
              oldLine := PatchListe.Strings[PatchListe.ItemPointer];
              syntaxCheck := False;

              if Skip('user_pref', oldLine, oldLine, ErrorInfo) then
                if Skip('("', oldLine, oldLine, ErrorInfo) then
                  if Skip(s1, oldLine, oldLine, ErrorInfo) then
                    if Skip('"', oldLine, oldLine, ErrorInfo) then
                      if Skip(',', oldLine, oldLine, ErrorInfo) then
                        if Skip('"', oldLine, oldLine, ErrorInfo) then
                        begin
                          GetWord(oldLine, old_s2, oldLine, ['"']);
                          if Skip('"', oldLine, oldLine, ErrorInfo) then
                            if Skip(')', oldLine, oldLine, ErrorInfo) then
                              syntaxCheck := True;
                        end;
            end;
          end;


          if syntaxCheck then
          begin
            if PatchListe.ItemPointer > -1 then
            begin
              oldLine := old_s2;
              GetWord(oldLine, x, oldLine, [',', '"']);
              if (oldLine <> '') then
                System.Delete(oldLine, 1, 1);

              found := False;
              while (oldLine <> '') and not found do
              begin
                GetWord(oldLine, x, oldLine, [',', '"']);
                if (oldLine <> '') then
                  System.Delete(oldLine, 1, 1);
                if lowerCase(x) = lowerCase(s2) then
                  found := True;
              end;
              if not found then
              begin
                PatchListe.Delete(PatchListe.ItemPointer);
                PatchListe.Add('user_pref("' + s1 + '", "' + old_s2 + ',' + s2 + '");');
              end;
            end
            else
              PatchListe.Add('user_pref("' + s1 + '", "' + s2 + '");');

          end;

        end

        else
          ErrorInfo := methodname + ' not defined';

      end;

      if syntaxcheck then
        Inc(i);

      if not syntaxCheck then
        reportError(Sektion, i, Sektion.strings[i - 1], errorinfo);
    end;

    if saveToOriginalFile then
      PatchListe.SaveToFile(PatchFilename);
    PatchListe.Free;
    PatchListe := nil;

    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
    workingSection.Free;
  end;   //doTextpatchMain

begin
  Result := tsrPositive;
  //Filename := ExpandFileName(Filename);

  if not initSection(Sektion, OldNumberOfErrors, OldNumberOfWarnings) then
    exit;

  if (lowercase(PatchParameter) = lowercase(Parameter_AllNTUserProfiles)) or
    flag_all_ntuser then
  begin
    flag_all_ntuser := False;
    ProfileList := getProfilesDirList;
    for pc := 0 to ProfileList.Count - 1 do
    begin
      PatchFilename := SysUtils.StringReplace(Filename, '%userprofiledir%',
        ProfileList.Strings[pc], [rfReplaceAll, rfIgnoreCase]);
      PatchFilename := SysUtils.StringReplace(PatchFilename,
        '%currentprofiledir%', ProfileList.Strings[pc], [rfReplaceAll, rfIgnoreCase]);
      PatchFilename := ExpandFileName(PatchFilename);
      doTextpatchMain(Sektion, ProfileList.Strings[pc] + PathDelim);
    end;
  end
  else
  begin
    if runLoginScripts then
    begin
      PatchFilename := SysUtils.StringReplace(Filename, '%userprofiledir%',
        GetUserProfilePath, [rfReplaceAll, rfIgnoreCase]);
      PatchFilename := SysUtils.StringReplace(PatchFilename,
        '%currentprofiledir%', GetUserProfilePath, [rfReplaceAll, rfIgnoreCase]);
    end
    else
      PatchFilename := Filename;
    PatchFilename := ExpandFileName(PatchFilename);
    doTextpatchMain(Sektion, GetUserProfilePath + PathDelim);
  end;

  finishSection(Sektion, OldNumberOfErrors, OldNumberOfWarnings,
    DiffNumberOfErrors, DiffNumberOfWarnings);

  if ExitOnError and (DiffNumberOfErrors > 0) then
    Result := tsrExitProcess;

end;



function TuibInstScript.doTests(const Sektion: TWorkSection;
  TestParameter: string): TSectionResult;
var
  i: integer;
  FindResultCode: integer;
  FileRecord: TSearchRec;
  Command: string = '';
  Remaining: string = '';
  Filename: string = '';
  Comment: string = '';

begin
  Result := tsrPositive;

  if not initSection(Sektion, OldNumberOfErrors, OldNumberOfWarnings) then
    exit;

  i := 1;
  while (i <= Sektion.Count) and (Result > 0) do
  begin
    if (Sektion.Strings[i - 1] = '') or (Sektion.Strings[i - 1] [1] =
      LineIsCommentChar) then
    (* continue *)
    else
    begin
      GetWord(Sektion.Strings[i - 1], Command, Remaining, WordDelimiterSet0);
      GetWord(Remaining, Filename, Remaining, WordDelimiterSet0);
      Comment := CutRightBlanks(Remaining);

      if length(Comment) > 0 then
        if ((Comment[1] = '"') and (Comment[length(Comment)] = '"')) or
          ((Comment[1] = '''') and (Comment[length(Comment)] = '''')) then
        begin
          System.Delete(Comment, 1, 1);
          if length(Comment) > 0 then
            System.Delete(Comment, length(Comment), 1);
        end;


      FindResultcode := FindFirst(FileName, faAnyFile, FileRecord);


      if UpperCase(Command) = UpperCase('ExitOnFileNotExist') then
      begin

        if FindResultCode < 0 (* not FileExists (FileName) *) then
        begin
          ps := LogDatei.LogSIndent + 'Error: File ' + FileName +
            ' does not exist';
          LogDatei.log(ps, LLError);
          if (length(Comment) > 0) then
            LogDatei.log(LogDatei.LogSIndentPlus(1) + Comment, BaseLevel);

          Result := tsrExitProcess;
        end
        else if FileRecord.Attr and faDirectory = faDirectory then
        begin
          ps := LogDatei.LogSIndent + 'File ' + FileName +
            ' (directory) exists ';
          LogDatei.log(ps, LevelComplete);
        end
        else
        begin
          ps := LogDatei.LogSIndent + 'File ' + FileName + ' exists';
          LogDatei.log(ps, LevelComplete);
        end;
      end

      else if UpperCase(Command) = UpperCase('ExitOnFileZero') then
      begin
        if FindResultCode < 0 (* not FileExists (FileName) *) then
        begin
          ps := LogDatei.LogSIndent + 'Error: File ' + FileName +
            ' does not exist';
          LogDatei.log(ps, LLError);

          if (length(Comment) > 0) then
            LogDatei.log(LogDatei.LogSIndentPlus(1) + Comment,
              BaseLevel);

          Result := tsrExitProcess;
        end
        else if FileRecord.Attr and faDirectory = faDirectory then
        begin
          ps := LogDatei.LogSIndent + 'File ' + FileName +
            ' (directory) exists ';
          LogDatei.log(ps, LevelComplete);
        end
        else if FileRecord.Size = 0 then
        begin
          ps := LogDatei.LogSIndent + 'Error: File ' + FileName +
            ' has size 0';
          LogDatei.log(ps, LLError);
          if (length(Comment) > 0) then
            LogDatei.log(LogDatei.LogSIndentPlus(1) + Comment, BaseLevel);
          Result := tsrExitProcess;
        end
        else
        begin
          ps := LogDatei.LogSIndent + 'File ' + FileName +
            ' exists and has not size 0';
          LogDatei.log(ps, LevelComplete);
        end;
      end

      else if UpperCase(Command) = UpperCase('WarningOnFileNotExist') then
      begin
        if FindResultCode < 0 (* not FileExists (FileName) *) then
        begin
          ps := LogDatei.LogSIndent + 'Warning: File ' + FileName +
            ' does not exist';
          LogDatei.log(ps, LevelWarnings);
          if (length(Comment) > 0) then
            LogDatei.log(LogDatei.LogSIndentPlus(1) + Comment, LLWarning);
        end
        else if FileRecord.Attr and faDirectory = faDirectory then
        begin
          ps := LogDatei.LogSIndent + 'File ' + FileName +
            ' (directory) exists ';
          LogDatei.log(ps, LevelComplete);
        end
        else
        begin
          ps := LogDatei.LogSIndent + 'File ' + FileName + ' exists';
          LogDatei.log(ps, LevelComplete);
        end;
      end

      else if UpperCase(Command) = UpperCase('WarningOnFileZero') then
      begin
        if FindResultCode < 0  (* not FileExists (FileName) *) then
        begin
          ps := LogDatei.LogSIndent + 'Warning: File ' + FileName +
            ' does not exist';
          LogDatei.log(ps, LLWarning);
          if (length(Comment) > 0) then
          begin
            ps := LogDatei.LogSIndentPlus(1) + Comment;
            LogDatei.log(ps, LevelWarnings);
          end;
        end
        else if FileRecord.Attr and faDirectory = faDirectory then
        begin
          ps := LogDatei.LogSIndent + 'File ' + FileName +
            ' (directory) exists ';
          LogDatei.log(ps, LevelComplete);
        end

        else if FileRecord.Size = 0 then
        begin
          ps := LogDatei.LogSIndent + 'Warning: File ' + FileName +
            ' has size 0';
          LogDatei.log(ps, LLWarning);
          if (length(Comment) > 0) then
          begin
            ps := LogDatei.LogSIndentPlus(1) + Comment;
            LogDatei.log(ps, LevelWarnings);
          end;
        end
        else
        begin
          ps := LogDatei.LogSIndent + 'File ' + FileName +
            ' exists and has not size 0';
          LogDatei.log(ps, LevelComplete);
        end;
      end

      else
        reportError(Sektion, i, Command, 'nicht definiert');
      ;

      SysUtils.findclose(FileRecord);
    end;

    Inc(i);
  end;

  finishSection(Sektion, OldNumberOfErrors, OldNumberOfWarnings,
    DiffNumberOfErrors, DiffNumberOfWarnings);

  if ExitOnError and (DiffNumberOfErrors > 0) then
    Result := tsrExitProcess;
end;



function TuibInstScript.doInifilePatches(const Sektion: TWorkSection;
  Filename: string; PatchParameter: string): TSectionResult;
var
  pc: integer = 0;
  Befehlswort: string = '';
  Rest: string = '';
  Bereich: string = '';
  Eintrag: string = '';
  AlterEintrag: string = '';
  /// Value : String;
  Patchdateiname: string = '';
  Patchdatei: TuibPatchIniFile;
  ErrorInfo: string = '';
  ProfileList: TStringList;

  procedure doInifilePatchesMain;
  var
    i: integer = 0;
    dummy: string;
    mytxtfile: TStringList;
  begin
    //ps := LogDatei.LogSIndentPlus (+3) + 'FILE ' +  PatchdateiName;
    //LogDatei.log (ps, LevelWarnings);
    Logdatei.log('', LLInfo);
    Logdatei.log('Patching: ' + PatchdateiName, LLInfo);


    if not FileExists(PatchdateiName) then
    begin
      ps := LogDatei.LogSIndentPlus(+3) +
        'Info: This file does not exist and will be created ';
      LogDatei.log(ps, LLInfo);
      LogDatei.NumberOfHints := Logdatei.NumberOfHints + 1;

      if CreateTextfile(PatchdateiName, ErrorInfo) then
      begin
        if ErrorInfo <> '' then
        begin
          ps := LogDatei.LogSIndentPlus(+3) + 'Warning: ' + ErrorInfo;
          LogDatei.log(ps, LLWarning);
        end;
      end
      else
      begin
        ps := LogDatei.LogSIndentPlus(+3) + 'Error: ' + ErrorInfo;
        LogDatei.log(ps, LLError);
        exit; // ------------------------------  exit
      end;
    end;


    ProcessMess;

    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

    // jetzt Arbeitsstruktur erzeugen

    Patchdatei := TuibPatchIniFile.Create;
    //mytxtfile := TStringlist.Create;


    Patchdatei.Clear;
    if FileExists(PatchdateiName) then
      mytxtfile := LoadFromFileWithEncoding(ExpandFileName(PatchdateiName),
        flag_encoding);
    //Patchdatei.LoadFromFile  (ExpandFileName(PatchdateiName));
    Patchdatei.Text := mytxtfile.Text;
    //Patchdatei.text := reencode(Patchdatei.Text, flag_encoding,dummy,'system');
    //Patchdatei.text := reencode(Patchdatei.Text, flag_encoding,dummy,system);
    for i := 0 to Patchdatei.Count - 1 do
      logdatei.log_prog('Loaded: ' + Patchdatei.Strings[i], LLDebug);

    for i := 1 to Sektion.Count do
    begin
      if (Sektion.strings[i - 1] = '') or (Sektion.strings[i - 1]
        [1] = LineIsCommentChar) then
      (* continue *)
      else
      begin
        WortAbspalten(cutLeftBlanks(Sektion.strings[i - 1]), Befehlswort, Rest);

        if UpperCase(Befehlswort) = 'ADD' then
        begin
          SectionnameAbspalten(Rest, Bereich, Eintrag);
          Patchdatei.addEntry(Bereich, Eintrag);
        end
        else if UpperCase(Befehlswort) = 'DEL' then
        begin
          SectionnameAbspalten(Rest, Bereich, Eintrag);
          Patchdatei.delEntry(Bereich, Eintrag);
        end
        else if UpperCase(Befehlswort) = 'SET' then
        begin
          SectionnameAbspalten(Rest, Bereich, Eintrag);
          Patchdatei.setEntry(Bereich, Eintrag);
        end
        else if UpperCase(Befehlswort) = 'ADDNEW' then
        begin
          SectionnameAbspalten(Rest, Bereich, Eintrag);
          Patchdatei.addNewEntry(Bereich, Eintrag);
        end
        else if UpperCase(Befehlswort) = 'DELSEC' then
        begin
          SectionnameAbspalten(Rest, Bereich, Eintrag);
          if Eintrag <> '' then
            reportError(Sektion, i, Sektion.strings[i - 1], 'syntax error')
          else
            Patchdatei.delSec(Bereich);
        end
        else if UpperCase(Befehlswort) = 'CHANGE' then
        begin
          SectionnameAbspalten(Rest, Bereich, Eintrag);
          Patchdatei.changeEntry(Bereich, Eintrag);
        end
        else if UpperCase(Befehlswort) = 'REPLACE' then
        begin
          WortAbspalten(Rest, AlterEintrag, Eintrag);
          if pos(' ', eintrag) > 0 then
            reportError(Sektion, i, Sektion.strings[i - 1],
              'replace string not identified ')
          else
            Patchdatei.replaceEntry(AlterEintrag, Eintrag);
        end
        else
          reportError(Sektion, i, Befehlswort, ' Operation not defined');
      end;
    end;
    Logdatei.log('--- ', LLInfo);
    //Patchdatei.Text:= reencode(Patchdatei.Text, 'system',dummy,flag_encoding);
    if not ((flag_encoding = 'utf8') or (flag_encoding = 'UTF-8')) then
    begin
      //mytxtfile.Text := reencode(Patchdatei.Text, 'utf8',dummy,flag_encoding);
      //mytxtfile.SaveToFile(PatchdateiName);
      Patchdatei.SaveToFile(PatchdateiName, flag_encoding);
    end
    else
    begin
      Patchdatei.SaveToFile(PatchdateiName, 'utf8');

    end;
    for i := 0 to Patchdatei.Count - 1 do
      logdatei.log_prog('Saved: ' + Patchdatei.Strings[i], LLDebug);
    Patchdatei.Free;
    Patchdatei := nil;
  end;

begin
  Result := tsrPositive;
  //Filename := ExpandFileName(Filename);

  if not initSection(Sektion, OldNumberOfErrors, OldNumberOfWarnings) then
    exit;

  if flag_all_ntuser then
  begin
    flag_all_ntuser := False;
    ProfileList := getProfilesDirList;
    for pc := 0 to ProfileList.Count - 1 do
    begin
      PatchdateiName := SysUtils.StringReplace(Filename, '%userprofiledir%',
        ProfileList.Strings[pc], [rfReplaceAll, rfIgnoreCase]);
      PatchdateiName := SysUtils.StringReplace(PatchdateiName,
        '%currentprofiledir%', ProfileList.Strings[pc], [rfReplaceAll, rfIgnoreCase]);
      PatchdateiName := ExpandFileName(PatchdateiName);
      doInifilePatchesMain;
    end;
  end
  else
  begin
    if runLoginScripts then
    begin
      PatchdateiName := SysUtils.StringReplace(Filename, '%userprofiledir%',
        GetUserProfilePath, [rfReplaceAll, rfIgnoreCase]);
      PatchdateiName := SysUtils.StringReplace(PatchdateiName,
        '%currentprofiledir%', GetUserProfilePath, [rfReplaceAll, rfIgnoreCase]);
    end
    else
      PatchdateiName := Filename;
    PatchdateiName := ExpandFileName(PatchdateiName);
    doInifilePatchesMain;
  end;

  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;

  finishSection(Sektion, OldNumberOfErrors, OldNumberOfWarnings,
    DiffNumberOfErrors, DiffNumberOfWarnings);


  if ExitOnError and (DiffNumberOfErrors > 0) then
    Result := tsrExitProcess;
end;


function TuibInstScript.doHostsPatch(const Sektion: TWorkSection;
  HostsFilename: string): TSectionResult;

var
  HostsLocation: string;
  Expressionstr, remaining: string;
  oldDisableWow64FsRedirectionStatus: pointer = nil;
  Wow64FsRedirectionDisabled, boolresult: boolean;


  procedure patchHosts(HostsLocation: string; var SectionResult: TSectionResult);

  var
    i: integer = 0;
    HostsImage: TuibPatchHostsFile;
    ///HostsFound : Boolean;
    Ident: string = '';
    Hostname: string = '';
    ipAdresse: string = '';
    Alias: string = '';
    ErrorInfo: string = '';

  begin
    HostsLocation := ExpandFileName(HostsLocation);
    ps := LogDatei.LogSIndentPlus(+2) + 'FILE ' + HostsLocation;
    LogDatei.log(ps, LevelWarnings);

    ProcessMess;

    HostsImage := TuibPatchHostsFile.Create;

    if FileExists(HostsLocation) then
      HostsImage.LoadFromFile(HostsLocation)
    else
    begin
      ps := LogDatei.LogSIndentPlus(+3) +
        'Info: This file does not exist and will be created ';
      LogDatei.log(ps, LLinfo);
      LogDatei.NumberOfHints := Logdatei.NumberOfHints + 1;

      if CreateTextfile(HostsLocation, ErrorInfo) then
      begin
        if ErrorInfo <> '' then
        begin
          ps := LogDatei.LogSIndentPlus(+3) + 'Warning: ' + ErrorInfo;
          LogDatei.log(ps, LLWarning);
        end;
      end
      else
      begin
        ps := LogDatei.LogSIndentPlus(+3) + 'Error: ' + ErrorInfo;
        LogDatei.log(ps, LLError);
        exit; (* ------------------------------  exit *)
      end;
    end;
    HostsImage.Text := reencode(HostsImage.Text, 'system');
    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

    for i := 1 to Sektion.Count do
    begin
      Remaining := cutLeftBlanks(Sektion.strings[i - 1]);

      if (Remaining = '') or (Remaining[1] = LineIsCommentChar) or
        (Remaining[1] = '#')
          (* Zeilen, die mit # beginnen, werden in einer HostsPatch-Sektion ebenfalls
             als Kommentar gewertet *) then
      (* continue *)
      else
      begin
        GetWord(Remaining, Expressionstr, Remaining, WordDelimiterSetHosts);

        Hostname := '';
        ipAdresse := '';
        Alias := '';
        Ident := '';

        if UpperCase(Expressionstr) = UpperCase('SetAddr') then
        begin
          GetWord(Remaining, Hostname, ipAdresse, WordDelimiterSetHosts);
          HostsImage.SetAddress(Hostname, ipAdresse);
        end
        else if UpperCase(Expressionstr) = UpperCase('SetName') then
        begin
          GetWord(Remaining, ipAdresse, Hostname, WordDelimiterSetHosts);
          HostsImage.SetName(ipAdresse, Hostname);
        end
        else if UpperCase(Expressionstr) = UpperCase('SetAlias') then
        begin
          GetWord(Remaining, Ident, Alias, WordDelimiterSetHosts);
          HostsImage.SetAlias(Ident, Alias);
        end
        else if UpperCase(Expressionstr) = UpperCase('DelAlias') then
        begin
          GetWord(Remaining, Ident, Alias, WordDelimiterSetHosts);
          HostsImage.DelAlias(Ident, Alias);
        end
        else if UpperCase(Expressionstr) = UpperCase('DelHost') then
        begin
          GetWord(Remaining, Ident, Remaining, WordDelimiterSetHosts);
          HostsImage.DelHost(Ident);
        end
        else if UpperCase(Expressionstr) = UpperCase('SetComment') then
        begin
          GetWord(Remaining, Ident, Remaining, WordDelimiterSetHosts);
          HostsImage.SetComment(Ident, Remaining);
        end

        else
          reportError(Sektion, i, Expressionstr, ' Operation not defined');
      end;
    end;

    try
      HostsImage.SaveToFile(HostsLocation);
    except
      ps := LogDatei.LogSIndent + 'Error: file ' + HostsLocation +
        ' could not be saved to disk';
      Logdatei.log(ps, LLError);
    end;
    HostsImage.Free;
    HostsImage := nil;

    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
  end;

begin
  Wow64FsRedirectionDisabled := False;
  Result := tsrPositive;

  if not initSection(Sektion, OldNumberOfErrors, OldNumberOfWarnings) then
    exit;

  if HostsFilename <> '' then
    patchHosts(HostsFilename, Result)
  else
  begin
    {$IFDEF WIN32}
    if Is64BitSystem then
    begin
      boolresult := DSiDisableWow64FsRedirection(oldDisableWow64FsRedirectionStatus);
      Wow64FsRedirectionDisabled := True;
    end
    else
      Wow64FsRedirectionDisabled := False;
    {$ENDIF}
    HostsLocation := DefaultHosts;
    if FileExists(HostsLocation) then
      patchHosts(HostsLocation, Result);
    {$IFDEF WIN32}
    if Wow64FsRedirectionDisabled then
    begin
      boolresult := DSiRevertWow64FsRedirection(oldDisableWow64FsRedirectionStatus);
      Wow64FsRedirectionDisabled := False;
    end;
    {$ENDIF}

  end;

  finishSection(Sektion, OldNumberOfErrors, OldNumberOfWarnings,
    DiffNumberOfErrors, DiffNumberOfWarnings);

  if ExitOnError and (DiffNumberOfErrors > 0) then
    Result := tsrExitProcess;
end;


function TuibInstScript.doIdapiConfig(const Sektion: TWorkSection;
  IdapiParameter: string): TSectionResult;

begin
  // is not supported any more
  Result := -1;
end;


function tlcrToStr(x: TLDAPsearchCacheRequest): string;
begin
  case x of
    tlcrNone: Result := '';
    tlcrCache: Result := 'Cache';
    tlcrCached: Result := 'Cached';
    tlcrClear: Result := 'Free';
  end;
end;

function tlorToStr(x: TLDAPsearchOutputRequest): string;
begin
  case x of
    tlorNone: Result := '';
    tlorObjects: Result := 'Objects';
    tlorAttributes: Result := 'Attributes';
    tlorValues: Result := 'Values';
  end;
end;

function produceLDAPsearchParameters
  (const LDAPparameter: string; var cacheRequest: TLDAPsearchCacheRequest;
  var outputRequest: TLDAPsearchOutputRequest; var errorinfo: string): boolean;

var
  remainingParameter: string = '';
  checkParameter: string = '';
  syntaxcheck: boolean;

begin
  cacheRequest := tlcrNone;
  outputRequest := tlorNone;
  syntaxcheck := True;

  remainingParameter := LDAPParameter;
  while syntaxcheck and (length(remainingParameter) > 0) do
  begin
    getWord(remainingParameter, checkParameter, remainingParameter,
      worddelimiterWhiteSpace);

    if skip('/', checkParameter, checkParameter, errorinfo) then
    begin

      if lowercase(checkParameter) = lowercase(tlcrToStr(tlcrCache)) then
        cacheRequest := tlcrCache
      else if lowercase(checkParameter) = lowercase(tlcrToStr(tlcrCached)) then
        cacheRequest := tlcrCached
      else if lowercase(checkParameter) = lowercase(tlcrToStr(tlcrClear)) then
        cacheRequest := tlcrClear

      else if lowercase(checkParameter) = lowercase(tlorToStr(tlorObjects)) then
        outputRequest := tlorObjects
      else if lowercase(checkParameter) = lowercase(tlorToStr(tlorAttributes)) then
        outputRequest := tlorAttributes
      else if lowercase(checkParameter) = lowercase(tlorToStr(tlorValues)) then
        outputRequest := tlorValues

      else
      begin
        syntaxcheck := False;
        errorinfo := 'no valid parameter';
      end;

    end
    else
      syntaxcheck := False;

  end;

  Result := syntaxcheck;
end;


function TuibInstScript.doLDAPsearch
  (const Sektion: TWorkSection; cacheRequest: TLDAPsearchCacheRequest;
  outputRequest: TLDAPsearchOutputRequest; var output: TXStringList): TSectionResult;

  // uses ldapsend
var
  targethost: string;
  targetport: string;
  ldapsearch_user: string;
  ldapsearch_credentials: string;
  ldapsearch_dn: string;
  ldapsearch_typesOnly: boolean;
  ldapsearch_filter: string;
  ldapsearch_attributes: TStringList;

  i: integer;
  savelogsindentlevel: integer;
  syntaxcheck: boolean;
  errorinfo: string;
  Expressionstr: string;
  remaining: string;
  itemS: string;

  errorOccurred: boolean;

  function analyzeSection: boolean;
  var
    ldapsearch_typesOnlyString: string;
    find: boolean;
    findInfo: string;
  begin
    syntaxCheck := True;

    targethost := '';
    targetport := '';
    ldapsearch_dn := '';
    ldapsearch_typesOnly := False;
    ldapsearch_typesOnlyString := 'false';
    ldapsearch_filter := '(objectclass=*)';
    ldapsearch_attributes := TStringList.Create;

    i := 1;

    while (i <= Sektion.Count) and syntaxcheck do
    begin
      Remaining := trim(Sektion.strings[i - 1]);
      if (Remaining = '') or (Remaining[1] = LineIsCommentChar) then
        Inc(i)
      else
      begin //real line
        logdatei.log('  analyze: ' + Remaining, LevelComplete);
        GetWord(Remaining, Expressionstr, Remaining, WordDelimiterSet0);

        if (length(Expressionstr) < 2) or (Expressionstr[length(Expressionstr)] <> ':')
        then
        begin
          syntaxcheck := False;
          errorInfo := 'keyname plus ":" expected"';
        end

        else
        begin
          Expressionstr := copy(Expressionstr, 0, length(Expressionstr) - 1);
          if LowerCase(Expressionstr) = 'targethost' then
          begin
            if (length(Remaining) = 0) then
            begin
              syntaxCheck := False;
              errorinfo := 'expected targethost name';
            end
            else
            begin
              getWordOrStringExpressionstr(remaining, targethost, remaining, errorinfo);
              if length(remaining) > 0 then
                reportError(Sektion, i, Sektion.Strings[i - 1], errorRemaining);

            end;
          end

          else if LowerCase(Expressionstr) = 'targetport' then
          begin
            if (length(Remaining) = 0) then
            begin
              //stay to default port
            end
            else
            begin
              getWordOrStringExpressionstr(remaining, targetport, remaining, errorinfo);

              try
                StrToInt(targetPort)
              except
                reportError(Sektion, i, Sektion.Strings[i - 1], '"' +
                  targetport + '" is no number');
                syntaxcheck := False;
              end;

              if length(remaining) > 0 then
                reportError(Sektion, i, Sektion.Strings[i - 1], errorRemaining);
            end;
          end

          else if LowerCase(Expressionstr) = 'user' then
          begin
            if (length(Remaining) = 0) then
            begin
              //anonymous logon
            end
            else
            begin
              getWordOrStringExpressionstr(remaining, ldapsearch_user,
                remaining, errorinfo);

              if length(remaining) > 0 then
                reportError(Sektion, i, Sektion.Strings[i - 1], errorRemaining);
            end;
          end

          else if LowerCase(Expressionstr) = 'password' then
          begin
            if (length(Remaining) = 0) then
            begin
              //anonymous logon
            end
            else
            begin
              getWordOrStringExpressionstr(remaining, ldapsearch_credentials,
                remaining, errorinfo);

              if length(remaining) > 0 then
                reportError(Sektion, i, Sektion.Strings[i - 1], errorRemaining);
            end;
          end


          else if LowerCase(Expressionstr) = 'dn' then
          begin
            if (length(Remaining) = 0) then
            begin
              //stay to default port
            end
            else
            begin
              getWordOrStringExpressionstr(remaining, ldapsearch_dn,
                remaining, errorinfo);

              if length(remaining) > 0 then
                reportError(Sektion, i, Sektion.Strings[i - 1], errorRemaining);
            end;
          end

          else if LowerCase(Expressionstr) = 'typesonly' then
          begin
            if (length(Remaining) = 0) then
            begin
              //stay to default
            end
            else
            begin
              getWordOrStringExpressionstr(remaining, ldapsearch_typesonlyString,
                remaining, errorinfo);
              if lowercase(ldapsearch_typesonlyString) = 'false' then
                ldapsearch_typesOnly := False
              else if lowercase(ldapsearch_typesonlyString) = 'true' then
                ldapsearch_typesOnly := True
              else
              begin
                syntaxcheck := False;
                errorinfo := 'only values "true" or "false" possible';
              end;

              if syntaxcheck and (length(remaining) > 0) then
                reportError(Sektion, i, Sektion.Strings[i - 1], errorRemaining);

            end;
          end

          else if LowerCase(Expressionstr) = 'filter' then
          begin
            if (length(Remaining) = 0) then
            begin
              //stay to default
            end
            else
            begin
              getWordOrStringExpressionstr(remaining, ldapsearch_filter,
                remaining, errorinfo);

              if length(remaining) > 0 then
                reportError(Sektion, i, Sektion.Strings[i - 1], errorRemaining);

            end;
          end

          else if LowerCase(Expressionstr) = 'attributes' then
          begin
            if (length(Remaining) = 0) then
            begin
              //stay to default
            end
            else
            begin
              // construct string list!
              find := True;
              while find do
              begin
                getWord(Remaining, itemS, Remaining, [',']);
                if (length(itemS) > 0) then
                  ldapsearch_attributes.add(itemS);
                Skip(',', Remaining, Remaining, findInfo);
                find := length(Remaining) > 0;
                logdatei.log('  attribute:' + itemS + ' remaining >' +
                  Remaining + '<', LevelComplete);
              end;

            end;
          end

          else
            reportError(Sektion, i, Sektion.Strings[i - 1],
              'Unknown LDAP search specification');

        end;

        if syntaxcheck then
          Inc(i);

      end; //real line

    end; //while

    if not syntaxcheck then
    begin
      reportError(Sektion, i, Sektion.Strings[i - 1], errorinfo);
    end
    else
    begin
      logdatei.log('LDAP search starting for:', LevelComplete);
      logdatei.log('  targethost: ' + targethost, LevelComplete);
      logdatei.log('  targetport: ' + targetport, LevelComplete);
      logdatei.log('  dn: ' + ldapsearch_dn, LevelComplete);
      logdatei.log('  typesOnly: ' + ldapsearch_typesOnlyString, LevelComplete);
      logdatei.log('  filter: ' + ldapsearch_filter, LevelComplete);
      logdatei.log('  attribute:' + ldapsearch_attributes.Text, LevelComplete);
    end;

    Result := syntaxcheck;

  end;

  function callLDAP: boolean;
  var
    ldap: TLDAPsend;
    goOn: boolean;
    l: integer;
  begin
    ldap := TLDAPsend.Create;

    goOn := True;
    if (ldapResult <> nil) then
    begin
      ldapResult.Clear;
      ldapResult.Free;
    end;
    ldapResult := TStringList.Create;

    try

      try //for catching an exception
        ldap.TargetHost := targethost;
        if targetPort <> '' then
          ldap.TargetPort := targetPort;

        ldap.UserName := ldapsearch_user;
        ldap.Password := ldapsearch_credentials;

        if not ldap.Login then
        begin
          goOn := False;
          logdatei.log('Error in LDAP login: ' + ldap.ResultString, LLError);
        end;

        if goOn and not ldap.Bind then
        begin
          goOn := False;
          logdatei.log('Error in LDAP login: ' + ldap.ResultString, LLError);
        end;

        if goOn then
        begin
          ldap.Search(ldapsearch_dn, ldapsearch_typesonly, ldapsearch_filter,
            ldapsearch_attributes);
          ldapResult.setText(PAnsiChar(LDAPResultdump(ldap.SearchResult)));

          (* // check result: *)
          logdatei.log('LDAP search result: ', LevelComplete);
          logdatei.LogSIndentLevel := logDatei.logSIndentLevel + 4;
          for l := 0 to ldapResult.Count - 1 do
          begin
            logdatei.log(ldapResult.Strings[l], LevelComplete);
          end;
          logdatei.LogSIndentLevel := logDatei.logSIndentLevel - 4;
          logdatei.log('', LevelComplete);
        end;


        ldap.Logout;

      except
        logdatei.log('Error (LDAP): ' + ldap.ResultString, LLError);
        goOn := False;
      end;

    finally

      ldap.Free;
    end;

    Result := goOn;

  end; //callLDAP

  function buildOutput: boolean;

  var
    iLines: integer = 0;
    ///iObjects : Integer;
    ///iAttributes : Integer;

    found: boolean;

    errorOccurred: boolean;

    line: string = '';
    n: integer = 0;
    rem: string = '';
    err: string = '';

  begin

    errorOccurred := False;

    if (outputRequest = tlorNone) then
    begin
      for iLines := 0 to ldapResult.Count - 1 do
        output.add(ldapResult.strings[iLines]);
    end
    else
    begin

      iLines := 0;

      found := False;

      n := ldapresult.Count;
      while not found and (iLines < n) do
      begin
        line := trim(ldapResult.strings[iLines]);
        if pos('results:', lowercase(line)) > -1 then
          found := True;
        Inc(iLines);
      end;

      if not found then
        errorOccurred := True;

      while iLines < n do
      begin
        line := trim(ldapResult.strings[iLines]);
        if (pos('result:', lowercase(line)) = 1) or (trim(line) = '') then
        begin
          //ignore
        end
        else
        begin
          rem := line;
          if skip('object:', line, rem, err) then
          begin
            if outputRequest = tlorObjects then
              output.add(rem);
          end

          else if skip('attribute:', line, rem, err) then
          begin
            if outputRequest = tlorAttributes then
              output.add(rem);
          end
          else
          begin
            if outputRequest = tlorValues then
              output.add(line);

          end;

        end;

        Inc(iLines);

      end;

    end;

    Result := errorOccurred;

  end;

begin
  Result := tsrPositive;

  if Sektion.Count = 0 then
    exit;

  LogDatei.LogSIndentLevel := Sektion.NestingLevel;
  savelogsindentlevel := LogDatei.LogSIndentLevel;

  OldNumberOfErrors := LogDatei.NumberOfErrors;
  OldNumberOfWarnings := LogDatei.NumberOfWarnings;

  ps := '';
  LogDatei.log(ps, LLNotice);
  ps := 'Execution of ' + Sektion.Name;

  if cacheRequest <> tlcrNone then
    ps := ps + '  (CacheOption: ' + tlcrToStr(cacheRequest) + ')';

  if outputRequest <> tlorNone then
    ps := ps + '  (OutputOption: ' + tlorToStr(outputRequest) + ')';


  LogDatei.log(ps, LLNotice);

  if pos(uppercase(PStatNames^ [tsLDAPsearch]), uppercase(Sektion.Name)) > 0 then
    //ps := (* 'Ausfuehrung von ' +  *) copy (Sektion.Name, length (PStatNames^ [tsLDAPsearch]) + 1, length (Sektion.Name));
    ps := Sektion.Name;

  {$IFDEF GUI}
  CentralForm.Label2.Caption := ps;
  FBatchOberflaeche.setDetailLabel(ps);
  ProcessMess;
  {$ENDIF GUI}
  LogDatei.LogSIndentLevel := Sektion.NestingLevel + 1;
  //Sektion.eliminateLinesStartingWith (';', false);


  errorOccurred := False;
  syntaxCheck := True;

  output := TXStringList.Create;


  if cacheRequest = tlcrClear then
  begin
    logdatei.log('Clearing cached LDAP search result', LevelComplete);
    if (ldapResult <> nil) then
    begin
      ldapResult.Clear;
      ldapResult.Free;
      ldapResult := nil;
    end;

  end
  else if cacheRequest = tlcrCached then
  begin
    if (ldapResult = nil) then
    begin
      reportError(Sektion, 0, '(parameter /cached)',
        'There was probably no (successful) LDAP search');
      logDatei.log('No cached LDAP search result found', LLError);
      syntaxCheck := False;
      errorOccurred := True;
    end;
  end
  else //tlcrNone, tlcrCache
  begin
    syntaxCheck := analyzeSection;
    if syntaxCheck then
      errorOccurred := not callLDAP;
    //testLDAP;
  end;

  if syntaxCheck and not errorOccurred and not (cacheRequest = tlcrClear) then
    buildOutput;

  if (cacheRequest = tlcrNone) and (ldapResult <> nil) then
  begin
    ldapResult.Clear;
    ldapResult.Free;
    ldapResult := nil;
  end;

  // if no error occurred we have a ldapResult stringlist
  // and may evaluate it

  finishSection(Sektion, OldNumberOfErrors, OldNumberOfWarnings,
    DiffNumberOfErrors, DiffNumberOfWarnings);

  LogDatei.LogSIndentLevel := savelogsindentlevel;

  if ExitOnError and (DiffNumberOfErrors > 0) then
    Result := tsrExitProcess;

end;

{$IFDEF WINDOWS}
function TuibInstScript.doRegistryHackInfSource(const Sektion: TWorkSection;
  const RegParameter: string; const flag_force64: boolean): TSectionResult;
var
  Regist: TuibRegistry;
  i, Startline_of_Command: integer;

  r: string;
  LineWasComplete: boolean;
  error: string;
  syntaxCheck: boolean;

  function InterpretLine(var r: string): boolean;
  var
    key0: string = '';
    key: string = '';
    Name: string = '';
    Value: string = '';
    regTStr: string = '';
    regTInt: longint;
    regType: TuibRegDataType;

  begin
    Result := False;
    syntaxCheck := True;

    GetWord(r, key0, r, [',']);
    if not Skip(',"', r, r, error) then
    begin
      Syntaxcheck := False;
      exit;
    end;
    GetWord(r, key, r, ['"']);
    if not Regist.openkey(key0, key) then
    begin
      error := 'Registry key ' + key0 + '\' + key + ' could not be opened';
      exit;
    end;

    if not Skip('","', r, r, error) then
    begin
      Syntaxcheck := False;
      exit;
    end;
    GetWord(r, Name, r, ['"']);
    if not Skip('",', r, r, error) then
    begin
      Syntaxcheck := False;
      exit;
    end;
    GetWord(r, regTStr, r, [',']);
    if not Skip(',', r, r, error) then
    begin
      Syntaxcheck := False;
      exit;
    end;

    try
      regTInt := StrToInt64(regTStr);
    except
      reportError(Sektion, i, Sektion.strings[i - 1], regTStr + ' is not a number');
      begin
        Syntaxcheck := False;
        exit;
      end;
    end;

    Value := '';

    if regTInt = 0 then
    begin
      regtype := trdString;
      if not Skip('"', r, r, error) then
      begin
        Syntaxcheck := False;
        exit;
      end;
      GetWord(r, Value, r, ['"']);
    end
    else if regTInt = 65537 then
    begin
      regtype := trdInteger;
      Value := r;
    end
    else if regTInt = 131072 then
    begin
      regtype := trdExpandString;
      if not Skip('"', r, r, error) then
      begin
        Syntaxcheck := False;
        exit;
      end;
      GetWord(r, Value, r, ['"']);
      (* Beispiel: 131072,"C:\Programme\wiz\wiz.exe %1" *)
    end
    else if regTInt = 196609 then
    begin
      regtype := trdBinary;
      Value := r;
    end

    else
    begin
      Syntaxcheck := False;
      Error := regTStr + ' kein erkannter Registrytyp';
      exit;
    end;

    Regist.writeEntry(Name, regtype, Value);
    Regist.closeKey;

    Result := True;
  end;

begin
  Result := tsrPositive;

  if not initSection(Sektion, OldNumberOfErrors, OldNumberOfWarnings) then
    exit;
  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

  if flag_force64 then
    Regist := TuibRegistry.Create(True)
  else
    Regist := TuibRegistry.Create;

  i := 1;
  Startline_of_Command := 1;
  LineWasComplete := True;

  while i <= Sektion.Count do
  begin
    if LineWasComplete then
    begin
      r := cutLeftBlanks(Sektion.strings[i - 1]);
      Startline_of_Command := i;
    end
    else
      r := r + cutLeftBlanks(Sektion.strings[i - 1]);

    if (length(r) > 1) and (r[length(r)] = '\') then
    begin
      system.Delete(r, length(r), 1);
      LineWasComplete := False;
    end
    else
      LineWasComplete := True;

    if (r = '') or (r[1] = LineIsCommentChar) then
    (* continue *)
    else
    begin
      if LineWasComplete then
        if not InterpretLine(r) then
          if SyntaxCheck = False then
            reportError(Sektion, Startline_of_Command, Sektion.strings[i - 1], error);
    end;

    Inc(i);
  end;

  Regist.Free;
  Regist := nil;

  finishSection(Sektion, OldNumberOfErrors, OldNumberOfWarnings,
    DiffNumberOfErrors, DiffNumberOfWarnings);


  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;

  if ExitOnError and (DiffNumberOfErrors > 0) then
    Result := tsrExitProcess;
end;

function TuibInstScript.doRegistryHackRegeditFormat
  (const Sektion: TWorkSection; const RegParameter: string;
  const flag_force64: boolean): TSectionResult;
var
  Regist: TuibRegistry;
  keyOpenCommandExists: boolean;
  keyOpened: boolean;
  regtype: TuibRegDataType;
  ///oldregtype : TuibRegDataType;
  ///Separator : Char;
  ErrorInfo: string = '';
  SyntaxCheck: boolean;
  Expressionstr: string = '';
  { wenn die globale Variable Expressionstr hier nicht lokal neu angelegt wird,
     so verliert der Parameter Sectionname, der ein Zeiger auf die globale Variable Expressionstr ist,
     zusammen mit dieser seinen Wert }
  r: string = '';
  i: integer = 0;
  k: integer = 0;
  StartIndentLevel: integer;

  registrykeys: TXStringList;
  entries: TXStringList;

  basekey: string = '';
  sectionname: string = '';
  key: string = '';
  key0: string = '';
  key_completepath: string = '';
  startlineno: integer = 0;

  field: string = '';
  typeinfo: string = '';
  Value: string = '';
  ///test : Integer;
  outkey: HKEY;

  function GetTypedValue_RegEditSyntax
    (s, regtypeinfo: string; var regtype: TuibRegDataType;
  var Value, r, ErrorInfo: string): boolean;

  var
    ///PartValue,Info : String;
    ///Continue : Boolean;

    binValue: string = '';
    lpdata: Pointer = nil;
    cbdata: DWord = 0;

    j: integer = 0;
    startvalue: string = '';

  begin
    Result := True;
    Value := '';

    if Skip(':', s, r, ErrorInfo) then
    begin
      if (upperCase(regtypeinfo) = 'REG_SZ') or
        (upperCase(regtypeinfo) = uppercase('hex(0)'))
      // hex(0) stands for REG_NONE 'no defined value type'
      then
        regtype := trdString
      else
      if upperCase(regtypeinfo) = 'REG_EXPAND_SZ' then
        regtype := trdExpandString
      else
      //if upperCase (regtypeinfo) = 'REG_MULTI_SZ'
      //then regtype := trdMultiString
      //else
      if (upperCase(regtypeinfo) = uppercase('hex')) or
        (upperCase(regtypeinfo) = uppercase('hex(7)')) then
        regtype := trdBinary
      else
      if uppercase(regtypeinfo) = uppercase('dword') then
        regtype := trdInteger
      else
      begin
        Result := False;
        ErrorInfo := regtypeinfo + ' is a not supported type of a registry entry';
        exit;
      end;
    end
    else
    begin
      if regtypeinfo = '' then
      begin
        ErrorInfo := ''; (* zuruecksetzen, da noch Hoffnung auf Korrektheit *)
        regtype := trdString;
      end
      else
      begin
        Result := False;
        exit;
      end;
    end;


    Result := False;
    case regtype of
      trdString:
      begin

        if skip('"', r, r, errorInfo) then
        begin

          Value := '';
          startvalue := r;
          j := 1;
          while j < length(startvalue) do
          begin
            if (startvalue[j] = '\') and (j < length(startvalue)) then
              // backquoted char
              Inc(j);
            Value := Value + startvalue[j];
            Inc(j);

          end;
          r := '';

          if startvalue[j] = '"' then
            Result := True
          else
            errorInfo := '""" expected';

        end;
      end;
      trdExpandString:
        if GetString(r, Value, r, errorinfo, True) then
          Result := True;
      trdMultiString:
        if GetString(r, Value, r, errorinfo, True) then
        begin
          Result := True;
          Value := Value + MultiszVisualDelimiter + MultiszVisualDelimiter;
          Value := StringReplace(Value, MultiszVisualDelimiter, #10);
        end;
      trdInteger:
        try
          r := '$' + r;
          StrToInt64(r);
          Value := r;
          r := '';
          Result := True;
        except
          on EConvertError do
            ErrorInfo := r + ' is no valid number';
          on E: Exception do
            ErrorInfo := E.Message;
        end;
      trdBinary:
      begin
        binValue := StringReplace(r, ',', ' ');

        SetLength(binValue, length(r) + 1);
        if StrToBinary(PChar(binValue), lpdata, cbdata) then
        begin
          Result := True;
          Value := r;
          r := '';
        end
        else
          ErrorInfo := '"' + r + '" is not a binary or hex value ';
      end;
    end;

  end;

begin
  try
    basekey := RegParameter;
    Result := tsrPositive;

    if not initSection(Sektion, OldNumberOfErrors, OldNumberOfWarnings) then
      exit;
    StartIndentLevel := LogDatei.LogSIndentLevel;


    if flag_force64 then
      Regist := TuibRegistry.Create(True)
    else
      Regist := TuibRegistry.Create;
    keyOpened := False;
    keyOpenCommandExists := False;



    // Sektion has ini file format, therefore we may do

    registrykeys := TXStringlist.Create;
    Sektion.GetSectionTitles(registrykeys);

    for k := 0 to registrykeys.Count - 1 do
    begin
      sectionname := registrykeys.strings[k];
      sectionname := copy(sectionname, 2, length(sectionname) - 2);
      key := sectionname;

      LogDatei.LogSIndentLevel := startindentlevel + 1;

      LogDatei.log('key ' + key, LLDebug);

      key_completepath := key;
      LogDatei.log('Key is: ' + key, LLdebug2);
      GetWord(key, key0, key, ['\']);
      LogDatei.log('Key0 is: ' + key0, LLdebug2);
      if Is64BitSystem and ('software' = lowerCase(key0)) then
      begin
        LogDatei.log('key starting with software on 64 bit. Key is: ' + key, LLdebug);
        if not flag_force64 then
        begin
          key := key0 + '\wow6432node' + key;
          LogDatei.log(
            'key starting with software on 64 bit has to be redirected. So we work with: '
            + key,
            LLdebug);
        end
        else
        begin
          key := key_completepath;
          LogDatei.log(
            'key starting with software on 64 bit. Redirection is disabled. So we work with: '
            + key,
            LLdebug);
        end;
      end
      else
      if (flag_all_ntuser or flag_ntuser or flag_all_usrclass or runLoginScripts) and
        (('HKEY_CURRENT_USER' = UpperCase(key0)) or ('HKCU' = UpperCase(key0))) then
      begin
        // remove HKCU from the beginning
        key := key;
        LogDatei.log('Running loginscripts: ignoring key0 : ' + key0 +
          ', using only key : ' + key, LLdebug2);
      end
      else
        key := key_completepath;
      GetWord(key, key0, key, ['\']);
      if GetHKEY(key0, outkey) then
      begin
        LogDatei.log('key0 : ' + key0 + ' is a valid base key and we will use it',
          LLDebug2);
        key := key_completepath;
      end
      else
      begin
        LogDatei.log('Key0 is: ' + key0 + ' Key is: ' + key, LLdebug2);
        LogDatei.log('key0 : ' + key0 +
          ' is not a valid base key and we will praefix it with: ' + basekey, LLDebug2);
        // insert RegParameter before key
        if basekey <> '' then
        begin
          if basekey[length(basekey)] = '\' then
            basekey := copy(basekey, 1, length(basekey) - 1);
          key_completepath := basekey;
          LogDatei.log('key_completepath1 : ' + key_completepath, LLdebug3);
          if (trim(key0) <> '') then
          begin
            if key0[1] = '\' then
              key0 := copy(key0, 2, length(key0));
            if key0[length(key0)] = '\' then
              key0 := copy(key0, 1, length(key0) - 1);
            key_completepath := key_completepath + '\' + key0;
            LogDatei.log('key_completepath2 : ' + key_completepath, LLdebug3);
          end;
          if (trim(key) <> '') then
          begin
            if key[1] = '\' then
              key := copy(key, 2, length(key));
            key_completepath := key_completepath + '\' + key;
            LogDatei.log('key_completepath3 : ' + key_completepath, LLdebug3);
          end;
        end;
      end;

      // extract HKEY
      GetWord(key_completepath, key0, key, ['\']);
      system.Delete(key, 1, 1);
      LogDatei.log('We will open Key : ' + key_completepath, LLdebug2);


      LogDatei.LogSIndentLevel := startindentlevel + 2;
      Regist.BaseIndentLevel := startindentlevel + 2;

      // close key before opening the next -> regunload will fail if keys are open
      if keyOpened then
        keyOpened := not Regist.CloseKey;
      keyOpened := Regist.OpenKey(key0, key);


      entries := TXStringList.Create;

      sektion.GetSectionLines(sectionname, entries, startlineno, False, False, True);

      i := 0;
      while i < entries.Count do
      begin

        r := cutLeftBlanks(entries.strings[i]);
        while (r[length(r)] = '\') and (i < entries.Count - 1) do // join lines
        begin
          Inc(i);
          r := copy(r, 1, length(r) - 1) + cutLeftBlanks(entries.strings[i]);
        end;

        LogDatei.log(r, baselevel);

        if (r = '') or (r[1] = LineIsCommentChar) then
        (* continue *)
        else
        begin
          syntaxcheck := True;

          if skip('@', r, r, ErrorInfo)
          //in Regedit-Syntax, a '@' stands for the default variable ("") of the key
          then
          begin
            field := '';
          end

          // the following is the set command in the registry section according to the winst syntax
          else if Skip('"', r, r, ErrorInfo) then
          begin
            GetWord(r, field, r, ['"']);
            if not Skip('"', r, r, ErrorInfo) then
              syntaxcheck := False;
          end

          else
            syntaxcheck := False;

          if syntaxcheck then
          begin
            syntaxcheck := Skip('=', r, r, ErrorInfo);
            if syntaxcheck then
            begin
              GetWord(r, typeinfo, r, [':', '"']);

              syntaxcheck := GetTypedValue_RegEditSyntax(r, typeinfo,
                regtype, Value, r, ErrorInfo);
              if syntaxcheck and (r <> '') then
              begin
                SyntaxCheck := False;
                ErrorInfo := ErrorRemaining;
              end;
            end;

          end;


          if SyntaxCheck then
            Regist.WriteEntry(field, regtype, Value)
          else
            reportError(Sektion, i, entries.strings[i], ErrorInfo);

          Inc(i);
        end;
      end;

    end;


    Regist.Free;
    Regist := nil;

    finishSection(Sektion, OldNumberOfErrors, OldNumberOfWarnings,
      DiffNumberOfErrors, DiffNumberOfWarnings);

    LogDatei.LogSIndentLevel := StartIndentLevel;
    //LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;

    if ExitOnError and (DiffNumberOfErrors > 0) then
      Result := tsrExitProcess;
  except
    on e: Exception do
    begin
      LogDatei.log('Error in doRegistryHackRegeditFormat: ' + e.message, LLError);
    end;
  end;
end;


function TuibInstScript.doRegistryHack(const Sektion: TWorkSection;
  const RegParameter: string; const flag_force64: boolean): TSectionResult;
var
  Regist: TuibRegistry;
  keyOpenCommandExists: boolean;
  keyOpened: boolean;
  basekey: string = '';
  key0: string = '';
  key: string = '';
  key_completepath: string = '';
  templatekey: string = '';
  field: string = '';
  Value: string = '';
  oldvalue: string = '';
  typeinfo: string = '';
  path: string = '';
  textfile: TStringList;
  filename: string = '';
  regtype, oldregtype: TuibRegDataType;
  Separator: char;
  ErrorInfo: string = '';
  citmark: char;
  SyntaxCheck: boolean;
  Expressionstr: string = '';
  { wenn die globale Variable Expressionstr hier nicht lokal neu angelegt wird,
     so verliert der Parameter Sectionname, der ein Zeiger auf die globale Variable Expressionstr ist,
     zusammen mit dieser seinen Wert }
  r: string = '';
  i: integer = 0;
  varno: integer = 0;
  varlist: TStringList;

  StartIndentLevel: integer = 0;

  subkeys: TXStringList;
  regpath: string = '';
  j: integer;
  commandline: string = '';
  report: string = '';
  output: TXStringList;
  outputlines: integer = 0;
  outkey: HKEY;
  p1, p2, p3, p4: integer;



  function GetTypedValue
    (s, regtypeinfo: string; var regtype: TuibRegDataType;
  var Value, r, ErrorInfo: string): boolean;

  var
    ///PartValue, Info : String;
    ///Continue : Boolean;

    binValue: string = '';
    lpdata: Pointer = nil;
    cbdata: DWord = 0;

  begin
    Result := True;
    Value := '';

    if Skip(':', s, r, ErrorInfo) then
    begin
      if upperCase(regtypeinfo) = 'REG_SZ' then
        regtype := trdString
      else if upperCase(regtypeinfo) = 'REG_EXPAND_SZ' then
        regtype := trdExpandString
      else if upperCase(regtypeinfo) = 'REG_MULTI_SZ' then
        regtype := trdMultiString
      else if upperCase(regtypeinfo) = uppercase('REG_Binary') then
        regtype := trdBinary
      else if uppercase(regtypeinfo) = uppercase('REG_DWord') then
        regtype := trdInteger
      //else if uppercase (regtypeinfo) =  uppercase ('REG_NONE')
      //then regtype := trdUnknown
      else
      begin
        Result := False;
        ErrorInfo := regtypeinfo + ' is a unknown or not implemented registry type';
        exit;
      end;
    end
    else
    begin
      if regtypeinfo = '' then
      begin
        ErrorInfo := ''; (* zuruecksetzen, da noch Hoffnung auf Korrektheit *)
        regtype := trdDefaultString;
      end
      else
      begin
        Result := False;
        exit;
      end;
    end;


    Result := False;
    case regtype of
      trdDefaultString:
        if GetString(r, Value, r, errorinfo, True, True) then
          Result := True;
      trdString:
        if GetString(r, Value, r, errorinfo, True, True) then
          Result := True;
      trdExpandString:
        if GetString(r, Value, r, errorinfo, True, True) then
          Result := True;
      trdMultiString:
        if GetString(r, Value, r, errorinfo, True, True) then
        begin
          Result := True;
          Value := Value + MultiszVisualDelimiter + MultiszVisualDelimiter;
          Value := StringReplace(Value, MultiszVisualDelimiter, #10);
        end;
      trdInteger:
        try
          StrToInt64(r);
          Value := r;
          r := '';
          Result := True;
        except
          on EConvertError do
            ErrorInfo := r + ' is no valid number';
          on E: Exception do
            ErrorInfo := E.Message;
        end;
      trdBinary:
      begin
        binValue := r;
        SetLength(binValue, length(r));
        if StrToBinary(PChar(binValue), lpdata, cbdata) then
        begin
          Result := True;
          Value := r;
          r := '';
        end
        else
          ErrorInfo := '"' + r + '" is no valid binary or hex value';
      end;
      //trdUnknown :
      //    begin
      //      result := true;
      //    end;

    end;

  end;

begin
  basekey := RegParameter;
  Result := tsrPositive;

  if not initSection(Sektion, OldNumberOfErrors, OldNumberOfWarnings) then
    exit;
  StartIndentLevel := LogDatei.LogSIndentLevel;
  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
  if flag_force64 then
    Regist := TuibRegistry.Create(True)
  else
    Regist := TuibRegistry.Create;
  keyOpened := False;
  keyOpenCommandExists := False;

  Regist.BaseIndentLevel := LogDatei.LogSIndentLevel;

  for i := 1 to Sektion.Count do
  begin
    r := cutLeftBlanks(Sektion.strings[i - 1]);
    if (r = '') or (r[1] = LineIsCommentChar) then
    // continue
    else
    begin
      GetWord(r, Expressionstr, r, WordDelimiterSet1);

      if LowerCase(Expressionstr) = LowerCase('OpenKey') then
      begin
        if keyOpenCommandExists // i.e., existed already
        then
        //LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1
        else
          keyOpenCommandExists := True;

        SyntaxCheck := False;
        GetWord(r, key, r, [']'], True);
        key := trim(key) + ']';
        p1 := pos('[', key);
        p2 := posFromEnd(']', key);
        p3 := length(key);
        p4 := length(trim(key));
        if not ((pos('[', key) = 1) and (posFromEnd(']', key) = length(key))) then
        begin
          SyntaxCheck := False;
          ErrorInfo := 'Wrong Key Format: Key must be given inside [] - but we got: '
            + key;
        end
        else
        begin
          key := opsiUnquotestr2(trim(key), '[]');
          if (pos('[', key) = 1) and (posFromEnd(']', key) = length(key)) then
          begin
            SyntaxCheck := False;
            ErrorInfo :=
              'Wrong Key Format: Have still brackets after removing them: ' + key;
          end
          else
        (*
        if Skip ('[', r, r, ErrorInfo)
        then
        Begin
          GetWord (r, key, r, [']']);
          if Skip (']', r, r, ErrorInfo)
          then
          Begin
            if r = '' then SyntaxCheck := true else ErrorInfo := ErrorRemaining;
        *)
          begin
            SyntaxCheck := True;
            key_completepath := key;
            LogDatei.log('Key is: ' + key, LLdebug);
            GetWord(key, key0, key, ['\']);
            LogDatei.log('Key0 is: ' + key0, LLdebug2);
            if Is64BitSystem and ('software' = lowerCase(key0)) then
            begin
              LogDatei.log('key starting with software on 64 bit. Key is: ' + key,
                LLdebug);
              if not flag_force64 then
              begin
                key := key0 + '\wow6432node' + key;
                LogDatei.log(
                  'key starting with software on 64 bit has to be redirected. So we work with: '
                  + key,
                  LLdebug);
              end
              else
              begin
                key := key_completepath;
                LogDatei.log(
                  'key starting with software on 64 bit. Redirection is disabled. So we work with: '
                  + key,
                  LLdebug);
              end;
            end
            else
            if (flag_all_ntuser or flag_ntuser or flag_all_usrclass or
              runLoginScripts) and (('HKEY_CURRENT_USER' = UpperCase(key0)) or
              ('HKCU' = UpperCase(key0))) then
            begin
              // remove HKCU from the beginning
              key := key;
              LogDatei.log('Running loginscripts: ignoring key0 : ' +
                key0 + ', using only key : ' + key, LLdebug2);
            end
            else
              key := key_completepath;
            GetWord(key, key0, key, ['\']);
            if GetHKEY(key0, outkey) then
            begin
              LogDatei.log('key0 : ' + key0 +
                ' is a valid base key and we will use it', LLDebug2);
              key := key_completepath;
            end
            else
            begin
              LogDatei.log('Key0 is: ' + key0 + ' Key is: ' + key, LLdebug2);
              LogDatei.log('key0 : ' + key0 +
                ' is not a valid base key and we will praefix it with: ' +
                basekey, LLDebug2);
              // insert RegParameter before key
              if basekey <> '' then
              begin
                if basekey[length(basekey)] = '\' then
                  basekey := copy(basekey, 1, length(basekey) - 1);
                key_completepath := basekey;
                LogDatei.log('key_completepath1 : ' + key_completepath, LLdebug3);
                if (trim(key0) <> '') then
                begin
                  if key0[1] = '\' then
                    key0 := copy(key0, 2, length(key0));
                  if key0[length(key0)] = '\' then
                    key0 := copy(key0, 1, length(key0) - 1);
                  key_completepath := key_completepath + '\' + key0;
                  LogDatei.log('key_completepath2 : ' + key_completepath, LLdebug3);
                end;
                if (trim(key) <> '') then
                begin
                  if key[1] = '\' then
                    key := copy(key, 2, length(key));
                  key_completepath := key_completepath + '\' + key;
                  LogDatei.log('key_completepath3 : ' + key_completepath, LLdebug3);
                end;
              end;
            end;


            // extract HKEY
            GetWord(key_completepath, key0, key, ['\']);
            system.Delete(key, 1, 1);
            LogDatei.log('We will open Key : ' + key_completepath, LLdebug2);
          end;
        end;

        if SyntaxCheck then
        begin
          if keyOpened then
            keyOpened := not Regist.CloseKey;
          Regist.BaseIndentLevel := StartIndentLevel + 1;
          keyOpened := Regist.OpenKey(key0, key);
          //LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
        end
        else
          reportError(Sektion, i, Sektion.strings[i - 1], ErrorInfo);

      end   // openkey

      else if LowerCase(Expressionstr) = LowerCase('DeleteKey') then
      begin
        SyntaxCheck := False;
        //SyntaxCheck := true;
        (*
        if Skip ('[', r, r, ErrorInfo)
        then
        Begin
          GetWord (r, key, r, [']']);
          if Skip (']', r, r, ErrorInfo)
          then
          Begin
            if r = '' then SyntaxCheck := true else ErrorInfo := ErrorRemaining;
          End;
          *)
        GetWord(r, key, r, [']'], True);
        key := trim(key) + ']';
        p1 := pos('[', key);
        p2 := posFromEnd(']', key);
        p3 := length(key);
        p4 := length(trim(key));
        if not ((pos('[', key) = 1) and (posFromEnd(']', key) = length(key))) then
        begin
          SyntaxCheck := False;
          ErrorInfo := 'Wrong Key Format: Key must be given inside [] - but we got: '
            + key;
        end
        else
        begin
          key := opsiUnquotestr2(trim(key), '[]');
          if (pos('[', key) = 1) and (posFromEnd(']', key) = length(key)) then
          begin
            SyntaxCheck := False;
            ErrorInfo :=
              'Wrong Key Format: Have still brackets after removing them: ' + key;
          end
          else
          begin
            SyntaxCheck := True;
            key_completepath := key;
            // extract HKEY
            GetWord(key_completepath, key0, key, ['\']);
            system.Delete(key, 1, 1);

            if (flag_all_ntuser or flag_ntuser or flag_all_usrclass or
              runLoginScripts) and (('HKEY_CURRENT_USER' = UpperCase(key0)) or
              ('HKCU' = UpperCase(key0))) then
            begin
              // remove HKCU from the beginning
              key := key;
              LogDatei.log('Running loginscripts: ignoring key0 : ' +
                key0 + ', using only key : ' + key, LLdebug2);
            end
            else
              key := key_completepath;
            GetWord(key, key0, key, ['\']);
            if GetHKEY(key0, outkey) then
            begin
              LogDatei.log('key0 : ' + key0 +
                ' is a valid base key and we will use it', LLDebug2);
              key := key_completepath;
            end
            else
            begin
              LogDatei.log('Key0 is: ' + key0 + ' Key is: ' + key, LLdebug2);
              LogDatei.log('key0 : ' + key0 +
                ' is not a valid base key and we will praefix it with: ' +
                basekey, LLDebug2);
              // insert RegParameter before key
              if basekey <> '' then
              begin
                if basekey[length(basekey)] = '\' then
                  basekey := copy(basekey, 1, length(basekey) - 1);
                key_completepath := basekey;
                LogDatei.log('key_completepath1 : ' + key_completepath, LLdebug3);
                if (trim(key0) <> '') then
                begin
                  if key0[1] = '\' then
                    key0 := copy(key0, 2, length(key0));
                  if key0[length(key0)] = '\' then
                    key0 := copy(key0, 1, length(key0) - 1);
                  key_completepath := key_completepath + '\' + key0;
                  LogDatei.log('key_completepath2 : ' + key_completepath, LLdebug3);
                end;
                if (trim(key) <> '') then
                begin
                  if key[1] = '\' then
                    key := copy(key, 2, length(key));
                  key_completepath := key_completepath + '\' + key;
                  LogDatei.log('key_completepath3 : ' + key_completepath, LLdebug3);
                end;
              end;
            end;

            // extract HKEY
            GetWord(key_completepath, key0, key, ['\']);
            system.Delete(key, 1, 1);
            LogDatei.log('We will open Key : ' + key_completepath, LLdebug2);
          end;
        end;

        if Is64BitSystem and (GetNTVersionMajor < 6) and regist.myforce64 then
        begin
          // Workaround for RegDeleteKey 64 Bit problem
          //errorcode := RegDeleteKeyEx(mykey, PChar(keytoopen), KeyOpenMode,0);
          output := TXStringList.Create;
          commandline := 'cmd64.exe /c "reg delete ""' + key_completepath + '"" /f"';
          LogDatei.log('Executing ' + commandline, LLDebug);
          if not RunCommandAndCaptureOut(commandline, True, output,
            report, SW_SHOWMINIMIZED, FLastExitCodeOfExe) then
          begin
            ps := 'Error: ' + Report;
            LogDatei.log(ps, LLcritical);
            FExtremeErrorLevel := LevelFatal;
            scriptstopped := True;
          end
          else
          begin
            LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 6;
            LogDatei.log('', LLDebug);
            LogDatei.log('output:', LLDebug);
            LogDatei.log('--------------', LLDebug);
            for outputlines := 0 to output.Count - 1 do
            begin
              LogDatei.log(output.strings[outputlines], LLDebug);
            end;
            LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 6;
            LogDatei.log('', LLDebug);
            output.Free;
          end;
        end
        else // workaround
        begin
          if SyntaxCheck then
          begin
            if keyOpened then
              keyOpened := not Regist.CloseKey;
            subkeys := TXStringList.Create;
            stringsplit(key_completepath, '\', subkeys);
            if subkeys.Count <= 1 then
            begin
              SyntaxCheck := False;
              ErrorInfo := 'No deleting on root level of registry';
            end;
          end;

          if Syntaxcheck then
          begin
            regpath := subkeys[1];
            for j := 2 to subkeys.Count - 1 do
              regpath := regpath + '\' + subkeys[j];
            Regist.BaseIndentLevel := StartIndentLevel + 1;
            Regist.DeleteKey(subkeys[0], regpath);
(*
              if (basekey = '') or (basekey = 'HKEY_USERS\' + usercontextSID+'\')
              then
              Begin
                  Regist.DeleteKey (subkeys[0], regpath);
              End
              else
              Begin
                 Regist.DeleteKey (subkeys[0], regpath); //regunloadkey does not work in this case
                 //Syntaxcheck := false;
                 //ErrorInfo := 'deletkey with /AllNtUserdat is not allowed (Windows API Bug)';
                 //Logdatei.log('deletkey with /AllNtUserdat is not allowed (Windows API Bug)', LLError);
              End;
*)
          end;
        end; // workaround


        if not syntaxcheck then
          reportError(Sektion, i, Sektion.strings[i - 1], ErrorInfo);

      end

      else
      if not keyOpenCommandExists then
        reportError(Sektion, i, Sektion.strings[i - 1], 'OpenKey expected')
      else
      begin
        SyntaxCheck := False;
        if LowerCase(Expressionstr) = 'set' then
        begin
          CitMark := '#';
          if (Skip('"', r, r, ErrorInfo)) then
            CitMark := '"'
          else if (Skip('''', r, r, ErrorInfo)) then
            CitMark := ''''
          else
            ErrorInfo := 'Expected "' + 'or ''';

          if CitMark <> '#'
          (* syntax o.k.*) then
          begin
            GetWord(r, field, r, [citMark]);
            if Skip(CitMark, r, r, ErrorInfo) then
              if Skip('=', r, r, ErrorInfo) then
              begin
                if (length(r) > 0) and (r[1] in ['"', '''']) then
                  typeinfo := ''
                else
                  GetWord(r, typeinfo, r, [':']);
                if GetTypedValue(r, typeinfo, regtype, Value, r, ErrorInfo)
                then
                  if r = '' then
                    SyntaxCheck := True
                  else
                    ErrorInfo := ErrorRemaining;
              end;
          end;
          if SyntaxCheck then
            Regist.WriteEntry(field, regtype, Value)
          else
            reportError(Sektion, i, Sektion.strings[i - 1], ErrorInfo);
        end

        else if LowerCase(Expressionstr) = 'add' then
        begin
          SyntaxCheck := False;

          if Skip('"', r, r, ErrorInfo) then
          begin
            GetWord(r, field, r, ['"']);
            if Skip('"', r, r, ErrorInfo) then
              if Skip('=', r, r, ErrorInfo) then
              begin
                GetWord(r, typeinfo, r, [':', '"']);
                if GetTypedValue(r, typeinfo, regtype, Value, r, ErrorInfo)
                then
                  if r = '' then
                    SyntaxCheck := True
                  else
                    ErrorInfo := ErrorRemaining;
              end;
          end;
          if SyntaxCheck then
          begin
            if Regist.ReadEntry(field, oldRegType, oldValue) then
            begin
              LogDatei.log('Info: Variable ' + field + ' exists, ' +
                ' no overwrite. (The value is "' + oldValue + '")', LLinfo);
              LogDatei.NumberOfHints := LogDatei.NumberOfHints + 1;
            end
            else
              Regist.WriteEntry(field, regtype, Value);
          end
          else
            reportError(Sektion, i, Sektion.strings[i - 1], ErrorInfo);
        end

        else if LowerCase(Expressionstr) = 'supp' then
        begin
          SyntaxCheck := False;
          if Skip('"', r, r, ErrorInfo) then
          begin
            GetWord(r, field, r, ['"']);
            if Skip('"', r, r, ErrorInfo) then
            begin
              if (length(r) = 0) then
                ErrorInfo := 'Separator expected'
              else
              begin
                if r[1] = '"' then
                  Separator := #0
                else
                begin
                  Separator := r[1];
                  Skip(Separator, r, r, ErrorInfo);
                end;
                if Skip('"', r, r, ErrorInfo) then
                begin
                  GetWord(r, Value, r, ['"']);
                  if Skip('"', r, r, ErrorInfo) then
                    SyntaxCheck := True;
                end;
              end;
            end;
          end;
          if SyntaxCheck then
            Regist.SupplementItems(Separator, field, Value)
          else
            reportError(Sektion, i, Sektion.strings[i - 1], ErrorInfo);
        end

        else if LowerCase(Expressionstr) = LowerCase('GetMultiSZFromFile') then
        begin
          Syntaxcheck := False;
          if GetString(trim(r), field, r, ErrorInfo, False) and
            GetString(trim(r), filename, r, ErrorInfo, False) then
            if r = '' then
              syntaxCheck := True
            else
              ErrorInfo := ErrorRemaining;

          if SyntaxCheck then
          begin
            LogDatei.log('Get value of variable "' + field +
              '" from file "' + filename + '"',
              LevelComplete);
            TextFile := TStringList.Create;
            try
              filename := ExpandFileName(filename);
              TextFile.LoadFromFile(filename);
              TextFile.Text := reencode(TextFile.Text, 'system');
            except
              on e: Exception do
              begin
                LogDatei.log('Error: ' + e.message, LLError);
              end;
            end;
            Value := TextFile.Text;
            Value := StringReplace(Value, #13#10, #10);
            Regist.WriteEntry(field, trdMultiString, Value);
            TextFile.Free;
          end
          else
            reportError(Sektion, i, Sektion.strings[i - 1], ErrorInfo);
        end


        else if LowerCase(Expressionstr) = LowerCase('SaveValueToFile')
        // undocumented
        then
        begin
          Syntaxcheck := False;
          if GetString(trim(r), field, r, ErrorInfo, False) and
            GetString(trim(r), filename, r, ErrorInfo, False) then
            if r = '' then
              syntaxCheck := True
            else
              ErrorInfo := ErrorRemaining;


          if SyntaxCheck then
          begin
            LogDatei.log('Save value of variable "' + field +
              '" to file "' + filename + '"',
              LevelComplete);
            TextFile := TStringList.Create;
            TextFile.Text := Regist.ReadString(field);

            try
              TextFile.SaveToFile(filename);
            except
              on e: Exception do
              begin
                LogDatei.log('Error: ' + e.message, LLError);
              end;
            end;
            Textfile.Free;
          end
          else
            reportError(Sektion, i, Sektion.strings[i - 1], ErrorInfo);
        end


        else if LowerCase(Expressionstr) = 'flushkey' then
        begin
          Regist.FlushKey;
        end

        else if LowerCase(Expressionstr) = LowerCase('ReconstructFrom') then
        begin
          SyntaxCheck := False;
          if Skip('"', r, r, ErrorInfo) then
          begin
            GetWord(r, path, r, ['"']);
            if Skip('"', r, r, ErrorInfo) then
            begin
              if r = '' then
                SyntaxCheck := True
              else
                ErrorInfo := 'not interpreted characters after "';
              if SyntaxCheck then
                Regist.RestoreFromFile(path)
              else
                reportError(Sektion, i, Sektion.strings[i - 1], ErrorInfo);
            end;
          end;
        end

        else if LowerCase(Expressionstr) = LowerCase('CopySecurityFrom')
        // experimentell
        then
        begin
          SyntaxCheck := False;
          if Skip('[', r, r, ErrorInfo) then
          begin
            GetWord(r, TemplateKey, r, [']']);
            if Skip(']', r, r, ErrorInfo) then
            begin
              if r = '' then
                SyntaxCheck := True
              else
                ErrorInfo := 'not interpreted characters after ]';

              { fuege RegParameter vor key ein }
              TemplateKey := basekey + TemplateKey;

              { hole HKEY aus key heraus }
              GetWord(TemplateKey, key0, TemplateKey, ['\']);
              System.Delete(TemplateKey, 1, 1);
            end;

            if SyntaxCheck then
              Regist.SetSecurity(TemplateKey, key)
            else
              reportError(Sektion, i, Sektion.strings[i - 1], ErrorInfo);
          end;
        end

        else if LowerCase(Expressionstr) = LowerCase('DeleteVar') then
        begin
          SyntaxCheck := False;
          if Skip('"', r, r, ErrorInfo) then
          begin
            GetWord(r, field, r, ['"']);
            if Skip('"', r, r, ErrorInfo) then
              if r = '' then
                syntaxCheck := True
              else
                ErrorInfo := ErrorRemaining;
          end;

          if SyntaxCheck then
            Regist.DeleteEntry(Field)
          else
            reportError(Sektion, i, Sektion.strings[i - 1], ErrorInfo);
        end

        else if LowerCase(Expressionstr) = LowerCase('DeleteAllVars') then
        begin

          syntaxcheck := False;

          if r <> '' then
            ErrorInfo := ErrorRemaining

          else
          begin
            syntaxcheck := True;
            varlist := Regist.GetAllVars;
            for varno := 0 to varlist.Count - 1 do
              Regist.DeleteEntry(varlist.Strings[varno]);
          end;
        end

        else
          reportError(Sektion, i, Expressionstr, ' Operation not defined');
      end;
    end;

  end;

  {
  if keyOpenCommandExists (* i.e., existed already *)
  then
    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
  }

  if keyOpened then
    keyOpened := not Regist.CloseKey;

  Regist.Free;
  Regist := nil;

  finishSection(Sektion, OldNumberOfErrors, OldNumberOfWarnings,
    DiffNumberOfErrors, DiffNumberOfWarnings);

  LogDatei.LogSIndentLevel := StartIndentLevel;
  //LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;

  if ExitOnError and (DiffNumberOfErrors > 0) then
    Result := tsrExitProcess;

end;


{$IFDEF WIN32}
function TuibInstScript.doRegistryAllNTUserDats(const Sektion: TWorkSection;
  rfSelected: TRegistryFormat; const flag_force64: boolean): TSectionResult;

var
  //SearchPath : String='';
  //SearchRec  : TSearchRec;
  //findresult : Integer=0;

  profilename, profilepath: string;
  ProfileList: TStringList;
  hkulist: TStringList;
  i: integer;
  pc: integer;
  domain: string = '';
  UserPath: string = '';
  aktsidStr: string = '';

  Errorcode: integer = 0;
  Info: string = '';

  StartWithErrorNumbers: integer = 0;
  StartWithWarningsNumber: integer = 0;

  function LoadNTUserDat(const path: string): boolean;
  var
    Errorcode: integer;
  begin
    if not SetProcessPrivilege('SeSecurityPrivilege') then
      logdatei.log(
        'Error: Could not set process privilege SeSecurityPrivilege in LoadNTUserDat',
        LLError);
    if not SetProcessPrivilege('SeRestorePrivilege') then
      logdatei.log(
        'Error: Could not set process privilege SeRestorePrivilege in LoadNTUserDat',
        LLError);
    if not SetProcessPrivilege('SeBackupPrivilege') then
      logdatei.log(
        'Error: Could not set process privilege SeBackupPrivilege in LoadNTUserDat',
        LLError);

    {load the selected ntuser.dat in HKEY_USERS, subkey TempUserRegKey}

    Errorcode := RegLoadKeyW(HKEY_USERS, PWChar(UnicodeString(TempUserRegKey)),
      PWChar(UnicodeString(path)));
    if Errorcode = Error_success then
    begin
      LogDatei.log('"' + path + '" loaded.', LevelComplete);
      Result := True;
    end
    else
    begin
      (* check if NTUser.dat is in use, since the specific user is logged in *)
      LogDatei.log('Warning: NTUser.dat could not be loaded from path "' +
        path + '". ' + 'Code ' + IntToStr(Errorcode) + ': ' +
        RemoveLineBreaks(SysErrorMessage(Errorcode)), LLWarning);
      Result := False;
    end;
  end;

  procedure workOnNtUserDat;
  begin
    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;


    case rfSelected of
      trfRegedit: doRegistryHackRegeditFormat(Sektion, 'HKEY_USERS\' +
          TempUserRegKey, flag_force64);
      trfWinst: doRegistryHack(Sektion, 'HKEY_USERS\' + TempUserRegKey,
          flag_force64);
    end;

    LogDatei.log('', LevelDebug);
    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;

    ErrorCode := RegFlushKey(HKEY_Users);

    if Errorcode = Error_success then
      LogDatei.log('Flushed', LevelDebug)
    else
    begin
      LogDatei.log('Warning: Could not be flushed. Code ' +
        IntToStr(Errorcode) + ': ' + RemoveLineBreaks(SysErrorMessage(Errorcode)),
        LLWarning);
    end;

    Errorcode := RegUnloadKey(HKEY_Users, PChar(TempUserRegKey));
    if Errorcode = Error_success then
      LogDatei.log('Unloaded', LevelDebug)
    else
    begin
      LogDatei.log('Warning: Could not be unloaded. Code ' +
        IntToStr(Errorcode) + ': ' + RemoveLineBreaks(SysErrorMessage(Errorcode)),
        LLWarning);
    end;
  end;

  procedure workOnHkuserSid(const Name: string);
  var
    sidStr: string = '';
  begin
    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

    //sidStr := GetLocalUserSidStr(GetUserNameEx_);
    //LogDatei.log('sidStr :'+sidStr,LLDebug);
    sidStr := GetLocalUserSidStr(Name);
    LogDatei.log('sidStr :' + sidStr, LLDebug2);
    sidStr := copy(sidStr, 2, length(sidStr) - 2);
    LogDatei.log('sidStr :' + sidStr, LLDebug);


    case rfSelected of
      trfRegedit: doRegistryHackRegeditFormat(Sektion, 'HKEY_USERS\' +
          sidStr, flag_force64);
      trfWinst: doRegistryHack(Sektion, 'HKEY_USERS\' + sidStr, flag_force64);
    end;

    LogDatei.log('', LLDebug);
    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;

    ErrorCode := RegFlushKey(HKEY_Users);

    if Errorcode = Error_success then
      LogDatei.log('Flushed', LevelDebug)
    else
    begin
      LogDatei.log('Warning: Could not be flushed. Code ' +
        IntToStr(Errorcode) + ': ' + RemoveLineBreaks(SysErrorMessage(Errorcode)),
        LLWarning);
    end;
  end;

begin
  Result := tsrPositive;

  if not initSection(Sektion, StartWithErrorNumbers, StartWithWarningsNumber) then
    exit;
  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

  // mount the NTUser.dat in the users profile path , patch them and write them back
  ProfileList := getProfilesDirList;
  for pc := 0 to ProfileList.Count - 1 do
  begin
    profilepath := ProfileList.Strings[pc];
    profilename := ExtractFileName(profilepath);

    LogDatei.log('', LLInfo);
    LogDatei.log('Branch: ' + profilename, LLInfo);
    UserPath := profilepath + '\NTUser.dat';
    if FileExists(UserPath) then
    begin
      if not FileGetWriteAccess(UserPath, Info) then
      begin
        LogDatei.log('Error: ' + Info, LLError);
      end
      else
      begin
        if Info <> '' then
        begin
          LogDatei.log('Warning: ' + Info, LLWarning);
        end;
        if LoadNTUserdat(UserPath) then
        begin
          workOnNtUserDat;
        end
        else
        begin
          if (GetUserNameEx_ <> '') then
          begin
            hkulist := TStringList.Create;
            hkulist.AddStrings(GetRegistryKeyList('HKU\', False));
            for i := 0 to hkulist.Count - 1 do
              LogDatei.log('found in hku  ' + hkulist.Strings[i], LLDebug2);
            hkulist.Free;
            aktsidStr := GetDomainUserSidS('', GetUserNameEx_, domain);
            aktsidStr := copy(aktsidStr, 2, length(aktsidStr) - 2);
            LogDatei.log('sid is: ' + aktsidStr, LLDebug2);
            LogDatei.log('index is: ' + IntToStr(GetRegistryKeyList(
              'HKU\', False).IndexOf(aktsidStr)), LLDebug2);
            if (profilename = GetUserNameEx_) or
              (profilepath = getProfileImagePathfromSid(
              GetLocalUserSidStr(GetUserNameEx_))) or
              (GetRegistryKeyList('HKU\', False).IndexOf(aktsidStr) > -1) then
            begin
              LogDatei.log('The Branch for :' + profilename +
                ' seems to be the logged in user,', LLDebug);
              LogDatei.log('so let us try to patch it via HKUsers\SID', LLDebug);
              workOnHkuserSid(GetUserNameEx_);
            end;
          end
          else
          begin
            // at XP we have problems to get the username while pcpatch is logged in
            if GetSystemOSVersionInfoEx('major_version') = '5' then
            begin
              LogDatei.log('The Branch for :' + profilename +
                ' may be the logged in user,', LLDebug);
              LogDatei.log('so let us try to patch it via HKUsers\SID', LLDebug);
              workOnHkuserSid(profilename);
            end;
          end;
        end;
      end;
    end;
  end;

   (*
     findresult := findnext (SearchRec);
   End;

   sysutils.findclose (SearchRec);
   *)


  // Patch HKEY_Current_User

  LogDatei.log('', LevelWarnings);
  LogDatei.log('Make it for user .DEFAULT', LevelWarnings);
  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
  case rfSelected of
    trfRegedit: doRegistryHackRegeditFormat(Sektion, 'HKEY_USERS\.DEFAULT',
        flag_force64);
    trfWinst: doRegistryHack(Sektion, 'HKEY_USERS\.DEFAULT', flag_force64);
  end;

  // do not to try the temporary pcpatch account or SYSTEM - sense less and may fail
  if not ((GetUserName_ = 'pcpatch') or (GetUserName_ = 'SYSTEM')) then
  begin
    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
    LogDatei.log('', LevelWarnings);
    LogDatei.log('And finally: The current user: ' + GetUserName_ +
      ' : ' + GetLocalUserSidStr(GetUserName_), LLInfo);
    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

    case rfSelected of
      trfRegedit: doRegistryHackRegeditFormat(Sektion, 'HKEY_CURRENT_USER',
          flag_force64);
      trfWinst: doRegistryHack(Sektion, 'HKEY_CURRENT_USER', flag_force64);
    end;
  end;

  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;

  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;

  finishSection(Sektion, OldNumberOfErrors, OldNumberOfWarnings,
    DiffNumberOfErrors, DiffNumberOfWarnings);

  if ExitOnError and (DiffNumberOfErrors > 0) then
    Result := tsrExitProcess;
end;

{$ELSE WIN32}
function TuibInstScript.doRegistryAllNTUserDats(const Sektion: TWorkSection;
  rfSelected: TRegistryFormat; const flag_force64: boolean): TSectionResult;
begin
  // not implemented
  Result := tsrExitProcess;
end;

{$ENDIF WIN32}

{$IFDEF WIN32}
function TuibInstScript.doRegistryAllUsrClassDats(const Sektion: TWorkSection;
  rfSelected: TRegistryFormat; const flag_force64: boolean): TSectionResult;

var
  //SearchPath : String='';
  //SearchRec  : TSearchRec;
  //findresult : Integer=0;

  profilename, profilepath: string;
  ProfileList: TStringList;
  hkulist: TStringList;
  i: integer;
  pc: integer;
  domain: string = '';
  UserPath: string = '';
  aktsidStr: string = '';

  Errorcode: integer = 0;
  Info: string = '';

  StartWithErrorNumbers: integer = 0;
  StartWithWarningsNumber: integer = 0;


  function LoadUsrClassDat(const path: string): boolean;
  var
    Errorcode: integer;
  begin
    if not SetProcessPrivilege('SeSecurityPrivilege') then
      logdatei.log(
        'Error: Could not set process privilege SeSecurityPrivilege in LoadUsrClassDat',
        LLError);
    if not SetProcessPrivilege('SeRestorePrivilege') then
      logdatei.log(
        'Error: Could not set process privilege SeRestorePrivilege in LoadUsrClassDat',
        LLError);
    if not SetProcessPrivilege('SeBackupPrivilege') then
      logdatei.log(
        'Error: Could not set process privilege SeBackupPrivilege in LoadUsrClassDat',
        LLError);

    {load the selected UsrClass.dat in HKEY_USERS, subkey TempUserRegKey}

    Errorcode := RegLoadKeyW(HKEY_USERS, PWChar(UnicodeString(TempUserRegKey)),
      PWChar(UnicodeString(path)));
    if Errorcode = Error_success then
    begin
      LogDatei.log('"' + path + '" loaded.', LevelComplete);
      Result := True;
    end
    else
    begin
      (* check if UsrClass.dat is in use, since the specific user is logged in *)
      LogDatei.log('Warning: UsrClass.dat could not be loaded from path "' +
        path + '". ' + 'Code ' + IntToStr(Errorcode) + ': ' +
        RemoveLineBreaks(SysErrorMessage(Errorcode)), LLWarning);
      Result := False;
    end;
  end;

  procedure workOnUsrClassDat;
  begin
    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;


    case rfSelected of
      trfRegedit: doRegistryHackRegeditFormat(Sektion, 'HKEY_USERS\' +
          TempUserRegKey, flag_force64);
      trfWinst: doRegistryHack(Sektion, 'HKEY_USERS\' + TempUserRegKey,
          flag_force64);
    end;

    LogDatei.log('', LevelDebug);
    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;

    ErrorCode := RegFlushKey(HKEY_Users);

    if Errorcode = Error_success then
      LogDatei.log('Flushed', LevelDebug)
    else
    begin
      LogDatei.log('Warning: Could not be flushed. Code ' +
        IntToStr(Errorcode) + ': ' + RemoveLineBreaks(SysErrorMessage(Errorcode)),
        LLWarning);
    end;

    Errorcode := RegUnloadKey(HKEY_Users, PChar(TempUserRegKey));
    if Errorcode = Error_success then
      LogDatei.log('Unloaded', LevelDebug)
    else
    begin
      LogDatei.log('Warning: Could not be unloaded. Code ' +
        IntToStr(Errorcode) + ': ' + RemoveLineBreaks(SysErrorMessage(Errorcode)),
        LLWarning);
    end;
  end;

  procedure workOnHkuserSid(const Name: string);
  var
    sidStr: string = '';
  begin
    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

    //sidStr := GetLocalUserSidStr(GetUserNameEx_);
    //LogDatei.log('sidStr :'+sidStr,LLDebug);
    sidStr := GetLocalUserSidStr(Name);
    LogDatei.log('sidStr :' + sidStr, LLDebug2);
    sidStr := copy(sidStr, 2, length(sidStr) - 2);
    LogDatei.log('sidStr :' + sidStr, LLDebug);


    case rfSelected of
      trfRegedit: doRegistryHackRegeditFormat(Sektion, 'HKEY_USERS\' +
          sidStr, flag_force64);
      trfWinst: doRegistryHack(Sektion, 'HKEY_USERS\' + sidStr, flag_force64);
    end;

    LogDatei.log('', LLDebug);
    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;

    ErrorCode := RegFlushKey(HKEY_Users);

    if Errorcode = Error_success then
      LogDatei.log('Flushed', LevelDebug)
    else
    begin
      LogDatei.log('Warning: Could not be flushed. Code ' +
        IntToStr(Errorcode) + ': ' + RemoveLineBreaks(SysErrorMessage(Errorcode)),
        LLWarning);
    end;
  end;

begin
  Result := tsrPositive;

  if not initSection(Sektion, StartWithErrorNumbers, StartWithWarningsNumber) then
    exit;
  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

  // mount the UsrClass.dat in the users profile path , patch them and write them back
  ProfileList := getProfilesDirList;
  for pc := 0 to ProfileList.Count - 1 do
  begin
    profilepath := ProfileList.Strings[pc];
    profilename := ExtractFileName(profilepath);

    LogDatei.log('', LLInfo);
    LogDatei.log('Branch: ' + profilename, LLInfo);
    UserPath := profilepath + '\AppData\Local\Microsoft\Windows\UsrClass.dat';
    if GetSystemOSVersionInfoEx('major_version') = '5' then
    begin
      UserPath := profilepath + '\Local Settings\Microsoft\Windows\UsrClass.dat';
    end;
    if FileExists(UserPath) then
    begin
      if not FileGetWriteAccess(UserPath, Info) then
      begin
        LogDatei.log('Error: ' + Info, LLError);
      end
      else
      begin
        if Info <> '' then
        begin
          LogDatei.log('Warning: ' + Info, LLWarning);
        end;
        if LoadUsrClassDat(UserPath) then
        begin
          workOnUsrClassDat;
        end
        else
        begin
          if (GetUserNameEx_ <> '') then
          begin
            hkulist := TStringList.Create;
            hkulist.AddStrings(GetRegistryKeyList('HKU\', False));
            for i := 0 to hkulist.Count - 1 do
              LogDatei.log('found in hku  ' + hkulist.Strings[i], LLDebug2);
            hkulist.Free;
            aktsidStr := GetDomainUserSidS('', GetUserNameEx_, domain);
            aktsidStr := copy(aktsidStr, 2, length(aktsidStr) - 2);
            LogDatei.log('sid is: ' + aktsidStr, LLDebug2);
            LogDatei.log('index is: ' + IntToStr(GetRegistryKeyList(
              'HKU\', False).IndexOf(aktsidStr)), LLDebug2);
            if (profilename = GetUserNameEx_) or
              (profilepath = getProfileImagePathfromSid(
              GetLocalUserSidStr(GetUserNameEx_))) or
              (GetRegistryKeyList('HKU\', False).IndexOf(aktsidStr) > -1) then
            begin
              LogDatei.log('The Branch for :' + profilename +
                ' seems to be the logged in user,', LLDebug);
              LogDatei.log('so let us try to patch it via HKUsers\SID', LLDebug);
              workOnHkuserSid(GetUserNameEx_);
            end;
          end
          else
          begin
            // at XP we have problems to get the username while pcpatch is logged in
            if GetSystemOSVersionInfoEx('major_version') = '5' then
            begin
              LogDatei.log('The Branch for :' + profilename +
                ' may be the logged in user,', LLDebug);
              LogDatei.log('so let us try to patch it via HKUsers\SID', LLDebug);
              workOnHkuserSid(profilename);
            end;
          end;
        end;
      end;
    end;
  end;

   (*
     findresult := findnext (SearchRec);
   End;

   sysutils.findclose (SearchRec);
   *)


  // Patch HKEY_Current_User

  LogDatei.log('', LevelWarnings);
  LogDatei.log('Make it for user .DEFAULT', LevelWarnings);
  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
  case rfSelected of
    trfRegedit: doRegistryHackRegeditFormat(Sektion, 'HKEY_USERS\.DEFAULT',
        flag_force64);
    trfWinst: doRegistryHack(Sektion, 'HKEY_USERS\.DEFAULT', flag_force64);
  end;

  // do not to try the temporary pcpatch account or SYSTEM - sense less and may fail
  if not ((GetUserName_ = 'pcpatch') or (GetUserName_ = 'SYSTEM')) then
  begin
    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
    LogDatei.log('', LevelWarnings);
    LogDatei.log('And finally: The current user: ' + GetUserName_ +
      ' : ' + GetLocalUserSidStr(GetUserName_), LLInfo);
    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

    case rfSelected of
      trfRegedit: doRegistryHackRegeditFormat(Sektion, 'HKEY_CURRENT_USER',
          flag_force64);
      trfWinst: doRegistryHack(Sektion, 'HKEY_CURRENT_USER', flag_force64);
    end;
  end;

  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;

  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;

  finishSection(Sektion, OldNumberOfErrors, OldNumberOfWarnings,
    DiffNumberOfErrors, DiffNumberOfWarnings);

  if ExitOnError and (DiffNumberOfErrors > 0) then
    Result := tsrExitProcess;
end;

{$ELSE WIN32}
function TuibInstScript.doRegistryAllUsrClassDats(const Sektion: TWorkSection;
  rfSelected: TRegistryFormat; const flag_force64: boolean): TSectionResult;
begin
  // not implemented
  Result := tsrExitProcess;
end;

{$ENDIF WIN32}


function TuibInstScript.doRegistryNTUserDat(const Sektion: TWorkSection;
  rfSelected: TRegistryFormat; const flag_force64: boolean;
  const UserPath: string): TSectionResult;

var
  //SearchPath : String='';
  //SearchRec  : TSearchRec;
  //findresult : Integer=0;

  //profilename, profilepath : string;
  //ProfileList : TStringList;
  //pc : integer;

  //UserPath : String='';

  Errorcode: integer = 0;
  Info: string = '';

  StartWithErrorNumbers: integer = 0;
  StartWithWarningsNumber: integer = 0;

  function LoadNTUserDat(const path: string): boolean;
  var
    Errorcode: integer;
  begin
    if not SetProcessPrivilege('SeSecurityPrivilege') then
      logdatei.log(
        'Error: Could not set process privilege SeSecurityPrivilege in LoadNTUserDat',
        LLError);
    if not SetProcessPrivilege('SeRestorePrivilege') then
      logdatei.log(
        'Error: Could not set process privilege SeRestorePrivilege in LoadNTUserDat',
        LLError);
    if not SetProcessPrivilege('SeBackupPrivilege') then
      logdatei.log(
        'Error: Could not set process privilege SeBackupPrivilege in LoadNTUserDat',
        LLError);

    {load the selected ntuser.dat in HKEY_USERS, subkey TempUserRegKey}

    Errorcode := RegLoadKeyW(HKEY_USERS, PWChar(UnicodeString(TempUserRegKey)),
      PWChar(UnicodeString(path)));
    if Errorcode = Error_success then
    begin
      LogDatei.log('"' + path + '" loaded.', LevelComplete);
      Result := True;
    end
    else
    begin
      (* check if NTUser.dat is in use, since the specific user is logged in *)
      LogDatei.log('Warning: NTUser.dat could not be loaded from path "' +
        path + '". ' + 'Code ' + IntToStr(Errorcode) + ': ' +
        RemoveLineBreaks(SysErrorMessage(Errorcode)), LLWarning);
      Result := False;
    end;
  end;

  procedure workOnNtUserDat;
  begin
    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;


    case rfSelected of
      trfRegedit: doRegistryHackRegeditFormat(Sektion, 'HKEY_USERS\' +
          TempUserRegKey, flag_force64);
      trfWinst: doRegistryHack(Sektion, 'HKEY_USERS\' + TempUserRegKey,
          flag_force64);
    end;

    LogDatei.log('', LevelDebug);
    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;

    ErrorCode := RegFlushKey(HKEY_Users);

    if Errorcode = Error_success then
      LogDatei.log('Flushed', LevelDebug)
    else
    begin
      LogDatei.log('Warning: Could not be flushed. Code ' +
        IntToStr(Errorcode) + ': ' + RemoveLineBreaks(SysErrorMessage(Errorcode)),
        LLWarning);
    end;

    Errorcode := RegUnloadKey(HKEY_Users, PChar(TempUserRegKey));
    if Errorcode = Error_success then
      LogDatei.log('Unloaded', LevelDebug)
    else
    begin
      LogDatei.log('Warning: Could not be unloaded. Code ' +
        IntToStr(Errorcode) + ': ' + RemoveLineBreaks(SysErrorMessage(Errorcode)),
        LLWarning);
    end;
  end;

begin
  Result := tsrPositive;

  if not initSection(Sektion, StartWithErrorNumbers, StartWithWarningsNumber) then
    exit;
  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

   (*
   ProfileList := getProfilesDirList;
   for pc:= 0 to ProfileList.Count -1 do
   begin
     profilepath := ProfileList.Strings[pc];
     profilename := ExtractFileName(profilepath);

     LogDatei.log ('', LLInfo);
     LogDatei.log ('Branch: ' + profilename, LLInfo);
     UserPath := profilepath + '\NTUser.dat';
     *)
  if FileExists(UserPath) then
  begin
    if not FileGetWriteAccess(UserPath, Info) then
    begin
      LogDatei.log('Error: ' + Info, LLError);
    end
    else
    begin
      if Info <> '' then
      begin
        LogDatei.log('Warning: ' + Info, LLWarning);
      end;
      if LoadNTUserdat(UserPath) then
      begin
        workOnNtUserDat;
      end
      else
        LogDatei.log('Error: could not patch ntuserdat: ' + UserPath, LLError);
    end;
  end;
  //End;
  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;

  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;

  finishSection(Sektion, OldNumberOfErrors, OldNumberOfWarnings,
    DiffNumberOfErrors, DiffNumberOfWarnings);

  if ExitOnError and (DiffNumberOfErrors > 0) then
    Result := tsrExitProcess;
end;

{$ENDIF WINDOWS}


function TuibInstScript.doOpsiServiceCall
  (const Sektion: TWorkSection; const parameter: string;
  var output: TXStringList): TSectionResult;

type
  TServiceChoice = (tscGlobal, tscReuse, tscLogin, tscInteractiveLogin,
    tscOpsiclientd, tscOpsiclientdOnce);

var
  startSIndentLevel: integer = 0;
  serviceChoice: TServicechoice;
  errorOccured: boolean;
  syntaxcheck: boolean;
  errorInfo: string = '';
  i: integer = 0;
  paramstartI: integer;
  j: integer = 0;

  methodname: string = '';
  paramList: TStringList;
  parameters: array of string;

  omc: TOpsiMethodCall;
  //local_opsidata : TOpsiDataJSONService;
  serviceURL: string = '';
  username: string = '';
  password: string = '';
  sessionid: string = '';
  r: string = '';
  Expressionstr: string = '';
  param: string = '';
  inParams, paramsFound, paramsValueListFound: boolean;
  stopIt: boolean;
  testresult: string = '';
   {$IFDEF GUI}
  passwordDialog: TDialogServicePassword;
   {$ENDIF GUI}
  opsiServiceVersion: string = '';
  myconf: TIniFile;
  opsiclientd_conf: string = '';
  tmplist: TStringList;

  procedure getNextValidLine(var line: string; var lineno: integer;
  const Lines: TStrings);
  begin
    line := trim(Lines[lineno - 1]);
    while (lineno <= Lines.Count) and (line = '') or (line[1] = LineIsCommentChar) do
    begin
      Inc(lineno);
      if lineno <= Lines.Count then
        line := trim(Lines[lineno - 1]);
      logdatei.log_prog('Script line(gNVL): ' + IntToStr(lineno) +
        ' : ' + line, LLDebug3);
    end;
    logdatei.log_prog('Script line(gNVL): ' + IntToStr(lineno) + ' : ' + line, LLDebug2);
  end;

begin
  try
    // initializing
    tmplist := TStringList.Create;
    Result := tsrPositive;
    startSIndentLevel := LogDatei.LogSIndentLevel;
    if not initSection(Sektion, OldNumberOfErrors, OldNumberOfWarnings) then
      exit;
    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
    try
      //convert String to catch not allowed chars
      r := trim(Utf8ToAnsi(AnsiToUtf8(parameter)));
    except
      on ex: Exception do
      begin
        LogDatei.log('Exception in doOpsiServiceCall: convert parameter' +
          ex.message, LLError);
      end;
    end;
    syntaxcheck := True;


    serviceChoice := tscGlobal;

    if local_opsidata <> nil then
      serviceChoice := tscReuse;
    logdatei.log('Parsingprogress: r: ' + r + ' exp: ' + Expressionstr, LLDebug3);
    if r <> '' then
    begin
      serviceChoice := tscLogin;
      while r <> '' do
      begin
        if skip('/opsiclientd-once', r, r, errorInfo) then
          serviceChoice := tscOpsiclientdOnce
        // we call the local opsiclientd and switch back

        else if skip('/opsiclientd', r, r, errorInfo) then
          serviceChoice := tscOpsiclientd // we call the local opsiclientd

        else if skip('/preloginservice', r, r, errorInfo) then
          serviceChoice := tscGlobal
        // we return to the default service from the installation server

        else if skip('/interactive', r, r, errorInfo) then
          serviceChoice := tscInteractiveLogin

        else if skip('/serviceurl', r, r, errorInfo) or
          skip('/opsiservice', r, r, errorInfo) or skip('/s', r, r, errorInfo)
        then
          EvaluateString(r, r, serviceurl, errorInfo)

        else if skip('/username', r, r, errorInfo) or
          skip('/user', r, r, errorInfo) or skip('/u', r, r, errorInfo) then
          EvaluateString(r, r, username, errorInfo)

        else if skip('/password', r, r, errorInfo) or
          skip('/pass', r, r, errorInfo) or skip('/p', r, r, errorInfo) then
          EvaluateString(r, r, password, errorInfo)

        else
        begin
          r := '';
          errorInfo := 'Not valid parameter for section ' + Sektion.Name;
          syntaxcheck := False;
        end;
      end;

    end;

    if (serviceChoice in [tscGlobal, tscReuse, tscOpsiclientd]) and
      ((username <> '') or (password <> '') or (serviceurl <> '')) then
    begin
      syntaxcheck := False;
      errorInfo :=
        'Connection parameters must not be specified if existing connection shall be used.';
    end;
    if (serviceChoice in [tscLogin, tscInteractiveLogin]) and
      (local_opsidata <> nil) then
      Logdatei.log(
        'Warning: Existing service connection will be reopend. This may cause critical errors. Do not do this, if you are not really sure what you doing.', LLwarning);

    // lets do something

    i := 1;
    if syntaxcheck then
    begin
      errorOccured := False;
      methodname := '';
      paramList := TStringList.Create;
      inParams := False;
      paramsFound := False;
      paramsValueListFound := False;

    end;


    while (i <= Sektion.Count) and syntaxcheck do
    begin
      getNextValidLine(r, i, Sektion);

      if i <= Sektion.Count then
      begin
        SyntaxCheck := False;
        GetWord(r, Expressionstr, r, [':']);

        if Skip(':', r, r, errorInfo) then
        begin
          logdatei.log('Parsingprogress: r: ' + r + ' exp: ' + Expressionstr, LLDebug3);
          if LowerCase(Expressionstr) = LowerCase('"method"') then
          begin
            if GetString(r, methodname, r, errorInfo, True) then
              syntaxcheck := True;
            logdatei.log_prog('Parsingprogress: r: ' + r + ' exp: ' +
              Expressionstr, LLDebug3);
          end

          else if LowerCase(Expressionstr) = LowerCase('"params"') then
          begin
            syntaxcheck := True;
            paramsFound := True;

            if r = '' then
              getNextValidLine(r, i, Sektion);
            logdatei.log_prog('Parsingprogress: r: ' + r + ' exp: ' +
              Expressionstr, LLDebug3);
            if (i <= Sektion.Count) and skip('[', r, r, errorInfo) then
            begin
              logdatei.log_prog('Parsingprogress: r: ' + r + ' exp: ' +
                Expressionstr, LLDebug3);
              inParams := True;
              paramsValueListFound := True;
              paramStartI := i;
              if r = ']' then  // empty param list
              begin
                inParams := False;
              end
              else if r <> '' then
              begin
                syntaxcheck := False;
                errorInfo := 'Parameters must be on their own lines';
              end
              else
              begin
                Inc(i);
              end;


              while
                syntaxcheck and (i <= Sektion.Count) and inParams do
              begin
                getNextValidLine(r, i, Sektion);
                logdatei.log_prog('Parsingprogress: r: ' + r + ' exp: ' +
                  Expressionstr, LLDebug3);
                if i <= Sektion.Count then
                begin
                  if r = ']' then
                    inParams := False
                  else
                  begin
                    if length(r) > 0 then
                    begin
                      if r[length(r)] = ',' then
                        r := trim(copy(r, 1, length(r) - 1));
                      if length(r) > 0 then
                      begin

                        if GetString(r, param, r, errorInfo, True)
                        then
                        begin
                          LogDatei.log_prog('Parsing: getparam: ' + param, LLdebug2);
                          paramList.Add(param);
                        end
                        else
                          syntaxcheck := False;
                      end;
                    end;
                    Inc(i);
                  end;
                end;

              end;
            end;

            if syntaxcheck and (i > Sektion.Count) and inParams then
            begin
              SyntaxCheck := False;
              errorInfo := 'ParamList starting on line ' +
                IntToStr(paramStartI) + ' not closed ' +
                ' (must be a pure "]" on a line)';
            end;

          end

          else
          begin
            SyntaxCheck := False;
            ErrorInfo := Expressionstr + ' is not a valid parameter ';
          end;
        end;

      end;


      Inc(i);

    end; // loop through lines


    if syntaxCheck then
    begin
      if (methodname = '') then
      begin
        syntaxCheck := False;
        errorInfo := errorInfo + ' Method name missing';
      end;

      if paramsFound then
      begin
        if not paramsValueListFound then
        begin
          errorInfo := errorInfo + ' Params missing';
          syntaxCheck := False;
        end;
      end
      else
      begin
        errorInfo := errorInfo + ' Params missing';
        syntaxCheck := False;
      end;
    end;


    if syntaxCheck then
    begin
      LogDatei.log('   "method": "' + methodname + '"', LLInfo);
      //LogDatei.log_prog('   "params" : "' + jsonParams, LLInfo);

      testresult := 'service not initialized';
      case serviceChoice of
        tscGlobal:
        begin

          if opsidata = nil then
          begin
            errorOccured := True;
          end
          else
            try
              if opsidata.getOpsiServiceVersion = '4' then
                local_opsidata := TOpsi4Data(opsidata);
              LogDatei.log_prog('Calling opsi service at ' +
                local_opsidata.serviceUrl, LLDebug);
            except
              errorOccured := True;
              testresult := 'not in service mode';
            end;

        end;

        tscReuse:
        begin
          if (local_opsidata = nil) or not (local_opsidata is TOpsi4Data) then
          begin
            errorOccured := True;
            testresult := 'not connected to service';
          end
          else
          begin
            LogDatei.log_prog('Calling opsi service at ' +
              local_opsidata.serviceUrl, LLDebug);
          end;
        end;

        tscLogin:
        begin
          try
            opsiServiceVersion :=
              getOpsiServiceVersion(serviceurl, username, password, sessionid);
            if opsiServiceVersion = '4' then
            begin
              local_opsidata := TOpsi4Data.Create;
              local_opsidata.initOpsiConf(serviceurl, username, password, sessionid);
              //Topsi4data(local_opsidata).initOpsiConf(serviceurl, username, password);
              //OpsiData.setOptions (opsiclientd_serviceoptions);

              //@Detlef: just for testing? I think so therefore I comment that out (Jan):
                  {omc := TOpsiMethodCall.create ('backend_info',[]);
                  //omc := TOpsiMethodCall.create ('authenticated',[]);
                  testresult := local_opsidata.CheckAndRetrieve (omc, errorOccured);
                  omc.free;}

            end
            else if opsiServiceVersion = '' then
              LogDatei.log(
                'opsi service version could not retrieved, perhaps no connection',
                LLwarning)
            else
              LogDatei.log('Internal Error: Unkown Opsi Service Version:>' +
                opsiServiceVersion + '<', LLerror);
          except
            on e: Exception do
            begin
              LogDatei.log('Exception in doOpsiServicecall: tscLogin: ' +
                e.message, LLError);
              errorOccured := True;
              testresult := 'not connected to service';
            end;
          end;
        end;

        tscInteractiveLogin:
        begin
            {$IFDEF GUI}
          stopIt := False;
          passwordDialog := TDialogServicePassword.Create(nil);


          while not stopIt do
          begin
            passwordDialog.Visible := False;
            passwordDialog.EditServiceURL.Text := serviceurl;
            passwordDialog.EditUsername.Text := username;
            passwordDialog.EditPassword.Text := '';

            if passwordDialog.showModal = mrOk then
            begin
              serviceUrl := passwordDialog.EditServiceURL.Text;
              username := passwordDialog.EditUsername.Text;
              password := passwordDialog.EditPassword.Text;
              LogDatei.log('serviceUrl: ' + serviceUrl, LLconfidential);
              LogDatei.log('username: ' + username, LLconfidential);
              LogDatei.log('password: ' + password, LLconfidential);
              opsiServiceVersion :=
                getOpsiServiceVersion(serviceurl, username, password, sessionid);
              if opsiServiceVersion = '4' then
              begin
                local_opsidata := TOpsi4Data.Create;
                local_opsidata.initOpsiConf(serviceurl, username, password, sessionid);
                //Topsi4data(local_opsidata).initOpsiConf(serviceurl, username, password);
                //OpsiData.setOptions (opsiclientd_serviceoptions);
                omc := TOpsiMethodCall.Create('backend_info', []);
                //omc := TOpsiMethodCall.create ('authenticated',[]);
                testresult := local_opsidata.CheckAndRetrieve(omc, errorOccured);
                omc.Free;
              end
              else
                errorOccured := True;
              //LogDatei.log ('Internal Error: Unkown Opsi Service Version:>'+opsiServiceVersion+'<', LLerror);

              stopIt := not errorOccured;
            end

            else
            begin
              stopIt := True;
              errorOccured := True;
              testresult := 'Cancelled by user';
            end;

          end;

          passwordDialog.Free;

            {$ELSE GUI}
          stopIt := False;
          //passwordDialog := TDialogServicePassword.create(nil);


          while not stopIt do
          begin

            if cmdLineInputDialog(serviceUrl, rsGetServiceUrl, serviceurl, False) and
              cmdLineInputDialog(username, rsGetUserName, '', False) and
              cmdLineInputDialog(password, rsGetPassword, '', True) then
            begin
              LogDatei.log('serviceUrl: ' + serviceUrl, LLconfidential);
              LogDatei.log('username: ' + username, LLconfidential);
              LogDatei.log('password: ' + password, LLconfidential);
              opsiServiceVersion :=
                getOpsiServiceVersion(serviceurl, username, password, sessionid);
              if opsiServiceVersion = '4' then
              begin
                local_opsidata := TOpsi4Data.Create;
                local_opsidata.initOpsiConf(serviceurl, username, password, sessionid);
                omc := TOpsiMethodCall.Create('backend_info', []);
                testresult := local_opsidata.CheckAndRetrieve(omc, errorOccured);
                omc.Free;
              end
              else
                errorOccured := True;
              stopIt := not errorOccured;
            end

            else
            begin
              stopIt := True;
              errorOccured := True;
              testresult := 'Cancelled by user';
            end;
          end;
            {$ENDIF GUI}
        end;

        tscOpsiclientd, tscOpsiclientdOnce:
        begin
          serviceurl := 'https://localhost:4441/opsiclientd';
            {$IFDEF WINDOWS}
          opsiclientd_conf :=
            getSpecialFolder(CSIDL_PROGRAM_FILES) +
            '\opsi.org\opsi-client-agent\opsiclientd\opsiclientd.conf';
            {$ENDIF WINDOWS}
            {$IFDEF UNIX}
          opsiclientd_conf := '/etc/opsi/opsiclientd.conf';
            {$ENDIF LINUX}
          if FileExists(opsiclientd_conf) then
          begin
            myconf := TInifile.Create(opsiclientd_conf);
            password := myconf.ReadString('global', 'opsi_host_key', '');
            username := myconf.ReadString('global', 'host_id', '');
            myconf.Free;
          end;
          if password = '' then
          begin
            stopIt := True;
            errorOccured := True;
            testresult := 'Can not get opsi_host_key';
          end
          else
          begin
            local_opsidata := TOpsi4Data.Create;
            local_opsidata.initOpsiConf(serviceurl, username, password);
            omc := TOpsiMethodCall.Create('backend_info', []);
            testresult := local_opsidata.CheckAndRetrieve(omc, errorOccured);
            omc.Free;
          end;
        end;

      end;

      if errorOccured then
      begin
        local_opsidata := nil;
        LogDatei.log('Error: ' + testresult, LLError);
      end;

      if local_opsidata = nil then
      begin
        errorOccured := True;
        LogDatei.log('Error: no connection to service', LLError);
      end;

    end;

    if not errorOccured and syntaxcheck then
    begin
      try
        setlength(parameters, paramList.Count);
        for j := 0 to paramList.Count - 1 do
        begin
          parameters[j] := paramlist.Strings[j];
          logdatei.log_prog('param[' + IntToStr(j) + ']: ' +
            paramlist.Strings[j], LLDebug2);
        end;

        omc := TOpsiMethodCall.Create(methodname, parameters);
        testresult := '';

        if copy(methodname, length(methodname) - length('_hash') +
          1, length(methodname)) = '_hash' then
        begin
          if output = nil then
            output := TXStringList.Create;
          tmplist := local_opsidata.checkAndRetrieveMap(omc, errorOccured);
          if tmplist <> nil then
            output.Assign(tmplist);
          //output.Text := tmplist.Text;
          if (not errorOccured) and (output <> nil) then
          begin
            try
              if output.Count > 0 then
                testresult := output.strings[0];

              for j := 0 to output.Count - 1 do
                testresult := testresult + ', ' + output.strings[j]
            except
              LogDatei.log(
                'Error: exeption after checkAndRetrieveMap in reading the stringlist' +
                testresult, LLError);
            end;
          end
          else if output = nil then
            output := TXStringList.Create;
        end
        else if copy(methodname, length(methodname) - length('_list') +
          1, length(methodname)) = '_list' then
        begin
          if output = nil then
            output := TXStringList.Create;
          tmplist := local_opsidata.checkAndRetrieveStringList(omc, errorOccured);
          if tmplist <> nil then
            output.Assign(tmplist);
          //output.Text := tmplist.Text;
          if (not errorOccured) and (output <> nil) then
          begin
            try
              if output.Count > 0 then
                testresult := output.strings[0];

              for j := 1 to output.Count - 1 do
                testresult := testresult + ', ' + output.strings[j]
            except
              LogDatei.log(
                'Error: exeption after checkAndRetrieveStringList in reading the stringlist'
                + testresult, LLError);
            end;
          end
          else if output = nil then
            output := TXStringList.Create;
        end
        else

        begin
          testresult := local_opsidata.CheckAndRetrieveString(omc, errorOccured);
          output := TXStringList.Create;
          output.add(testresult);
        end;

        omc.Free;

        if errorOccured then
        begin
          LogDatei.log('Error: ' + testresult, LLError);
        end
        else
        begin
          LogDatei.log('JSON result: ' + testresult, LLinfo);
          //Logdatei.log('Setup script name: '+opsidata.getProductScriptPath(tacSetup), LLessential);
        end;
      except
        on e: Exception do
        begin
          LogDatei.log('Exception in doOpsiServiceCall: do the call: ' +
            e.message, LLError);
        end;
      end;
    end;

    // trigger reset local_opsidata to tscGlobal if tscOpsiclientdOnce
    if serviceChoice = tscOpsiclientdOnce then
    begin
      local_opsidata.Free;
      //local_opsidata := nil;
      local_opsidata := opsidata;
    end;

    // finishing our section
    finishSection(Sektion, OldNumberOfErrors, OldNumberOfWarnings,
      DiffNumberOfErrors, DiffNumberOfWarnings);

    LogDatei.LogSIndentLevel := startSIndentLevel;

    if ExitOnError and (DiffNumberOfErrors > 0) then
      Result := tsrExitProcess;

    if not SyntaxCheck then
    begin
      if i >= Sektion.Count then
        reportError(Sektion, Sektion.Count, Sektion.strings[Sektion.Count - 1],
          ErrorInfo)
      else
        reportError(Sektion, i, Sektion.strings[i - 1], ErrorInfo);
    end
  except
    on ex: Exception do
    begin
      LogDatei.log('Exception in doOpsiServiceCall: general' + ex.message, LLError);
    end;
  end;

end;


//#############################################################################

function TuibInstScript.doOpsiServiceHashList
  (const Sektion: TWorkSection; const parameter: string;
  var output: TXStringList): TSectionResult;

type
  TServiceChoice = (tscGlobal, tscReuse, tscLogin, tscInteractiveLogin);

var
  startSIndentLevel: integer = 0;
  serviceChoice: TServicechoice;
  errorOccured: boolean;
  syntaxcheck: boolean;
  errorInfo: string = '';
  i: integer = 0;
  paramstartI: integer = 0;
  j: integer = 0;
  methodname: string = '';
  paramList: TStringList;
  parameters: array of string;

  omc: TOpsiMethodCall;
  //local_opsidata : TOpsiDataJSONService;
  serviceURL: string = '';
  username: string = '';
  password: string = '';
  sessionid: string = '';
  r: string = '';
  Expressionstr: string = '';
  param: string = '';
  inParams, paramsFound, paramsValueListFound: boolean;
  stopIt: boolean;
  testresult: string = '';
   {$IFDEF GUI}
  passwordDialog: TDialogServicePassword;
   {$ENDIF GUI}
  listname: string = '';
  list: TStringList;
  varindex: integer = 0;
  opsiServiceVersion: string = '';

  procedure getNextValidLine(var line: string; var lineno: integer;
  const Lines: TStrings);
  begin
    line := trim(Lines[lineno - 1]);
    while (lineno <= Lines.Count) and (line = '') or (line[1] = LineIsCommentChar) do
    begin
      Inc(lineno);
      if lineno <= Lines.Count then
        line := trim(Lines[lineno - 1]);
    end;
  end;

begin
  // initializing
  Result := tsrPositive;
  sessionid := '';
  startSIndentLevel := LogDatei.LogSIndentLevel;
  if not initSection(Sektion, OldNumberOfErrors, OldNumberOfWarnings) then
    exit;
  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
  try
    //convert String to catch not allowed chars
    r := trim(Utf8ToAnsi(AnsiToUtf8(parameter)));
  except
    on ex: Exception do
    begin
      LogDatei.log('Exception in doOpsiServiceCall: ' + ex.message, LLError);
    end;
  end;
  syntaxcheck := True;


  serviceChoice := tscGlobal;

  if local_opsidata <> nil then
    serviceChoice := tscReuse;

  if r <> '' then
  begin
    serviceChoice := tscLogin;
    while r <> '' do
    begin
      if skip('/preloginservice', r, r, errorInfo) then
        serviceChoice := tscGlobal
      // we return to the default service from the installation server

      else if skip('/interactive', r, r, errorInfo) then
        serviceChoice := tscInteractiveLogin

      else if skip('/serviceurl', r, r, errorInfo) or
        skip('/opsiservice', r, r, errorInfo) or skip('/s', r, r, errorInfo) then
        EvaluateString(r, r, serviceurl, errorInfo)

      else if skip('/username', r, r, errorInfo) or
        skip('/user', r, r, errorInfo) or skip('/u', r, r, errorInfo) then
        EvaluateString(r, r, username, errorInfo)

      else if skip('/password', r, r, errorInfo) or
        skip('/pass', r, r, errorInfo) or skip('/p', r, r, errorInfo) then
        EvaluateString(r, r, password, errorInfo)

      else
      begin
        r := '';
        errorInfo := 'Not valid parameter for section ' + Sektion.Name;
        syntaxcheck := False;
      end;
    end;

  end;

  if (serviceChoice in [tscGlobal, tscReuse]) and
    ((username <> '') or (password <> '') or (serviceurl <> '')) then
  begin
    syntaxcheck := False;
    errorInfo :=
      'Connection parameters must not be specified if existing connection shall be used.';
  end;

  // lets do something

  i := 1;
  if syntaxcheck then
  begin
    errorOccured := False;
    methodname := '';
    paramList := TStringList.Create;
    inParams := False;
    paramsFound := False;
    paramsValueListFound := False;
  end;


  while (i <= Sektion.Count) and syntaxcheck do
  begin
    getNextValidLine(r, i, Sektion);
    if i <= Sektion.Count then
    begin
      SyntaxCheck := False;
      GetWord(r, Expressionstr, r, [':']);

      if Skip(':', r, r, errorInfo) then
      begin
        if LowerCase(Expressionstr) = LowerCase('"method"') then
        begin
          if GetString(r, methodname, r, errorInfo, False) then
            syntaxcheck := True;
        end

        else if LowerCase(Expressionstr) = LowerCase('"HashList"') then
        begin
          if GetString(r, listname, r, errorInfo, False) then
          begin
            syntaxcheck := True;
            list := TXStringList.Create;
            VarIndex := listOfStringLists.IndexOf(LowerCase(listname));
            if VarIndex >= 0 then
            begin
              list.Assign(TStringList(contentOfStringLists[VarIndex]));
              syntaxCheck := True;
            end;
          end;
        end

        else if LowerCase(Expressionstr) = LowerCase('"params"') then
        begin
          syntaxcheck := True;
          paramsFound := True;

          if r = '' then
            getNextValidLine(r, i, Sektion);

          if (i <= Sektion.Count) and skip('[', r, r, errorInfo) then
          begin
            inParams := True;
            paramsValueListFound := True;
            paramStartI := i;
            if r = ']' then  // empty param list
            begin
              inParams := False;
            end
            else if r <> '' then
            begin
              syntaxcheck := False;
              errorInfo := 'Parameters must be on their own lines';
            end
            else
            begin
              Inc(i);
            end;


            while
              syntaxcheck and (i <= Sektion.Count) and inParams do
            begin
              getNextValidLine(r, i, Sektion);
              if i <= Sektion.Count then
              begin
                if r = ']' then
                  inParams := False
                else
                begin
                  if length(r) > 0 then
                  begin
                    if r[length(r)] = ',' then
                      r := trim(copy(r, 1, length(r) - 1));
                    if length(r) > 0 then
                    begin

                      if GetString(r, param, r, errorInfo, True)
                      then
                      begin
                        LogDatei.log_prog('Parsing: getparam: ' + param, LLdebug2);
                        paramList.Add(param);
                      end
                      else
                        syntaxcheck := False;
                    end;
                  end;
                  Inc(i);
                end;
              end;

            end;
          end;

          if syntaxcheck and (i > Sektion.Count) and inParams then
          begin
            SyntaxCheck := False;
            errorInfo := 'ParamList starting on line ' + IntToStr(paramStartI) +
              ' not closed ' + ' (must be a pure "]" on a line)';
          end;

        end

        else
        begin
          SyntaxCheck := False;
          ErrorInfo := Expressionstr + ' is not a valid parameter ';
        end;
      end;

    end;
    Inc(i);
  end; // loop through lines


  if syntaxCheck then
  begin
    if (methodname = '') then
    begin
      syntaxCheck := False;
      errorInfo := errorInfo + ' Method name missing';
    end;

    if paramsFound then
    begin
      if not paramsValueListFound then
      begin
        errorInfo := errorInfo + ' Params missing';
        syntaxCheck := False;
      end;
    end
    else
    begin
      errorInfo := errorInfo + ' Params missing';
      syntaxCheck := False;
    end;
  end;


  if syntaxCheck then
  begin
    LogDatei.log('   "method": "' + methodname + '"', LevelComplete);
    //LogDatei.log('   "params" : "' + jsonParams, LevelComplete);

    testresult := 'service not initialized';
    case serviceChoice of
      tscGlobal:
      begin

        if opsidata = nil then
        begin
          errorOccured := True;
        end
        else
          try
            if opsidata.getOpsiServiceVersion = '4' then
              local_opsidata := opsidata;
            LogDatei.log('Calling opsi service on existing connection at ' +
              local_opsidata.serviceUrl, LevelComplete);
          except
            errorOccured := True;
            testresult := 'not in service mode';
          end;

      end;

      tscReuse:
      begin
        if (local_opsidata = nil) or not (local_opsidata is TOpsi4Data) then
        begin
          errorOccured := True;
          testresult := 'not connected to service';
        end
        else
        begin
          LogDatei.log('Calling opsi service on existing connection at ' +
            local_opsidata.serviceUrl, LevelComplete);
        end;
      end;

      tscLogin:
      begin
        opsiServiceVersion :=
          getOpsiServiceVersion(serviceurl, username, password, sessionid);
        if opsiServiceVersion = '4' then
        begin
          Local_opsidata := TOpsi4Data.Create;
          local_opsidata.initOpsiConf(serviceurl, username, password, sessionid);
          //Topsi4data(local_opsidata).initOpsiConf(serviceurl, username, password);
          //OpsiData.setOptions (opsiclientd_serviceoptions);
          omc := TOpsiMethodCall.Create('backend_info', []);
          //omc := TOpsiMethodCall.create ('authenticated',[]);
          testresult := local_opsidata.CheckAndRetrieve(omc, errorOccured);
          omc.Free;
        end
        else
          LogDatei.log('Internal Error: Unkown Opsi Service Version:>' +
            opsiServiceVersion + '<', LLerror);
      end;

      tscInteractiveLogin:
      begin
          {$IFDEF GUI}
        stopIt := False;
        passwordDialog := TDialogServicePassword.Create(nil);


        while not stopIt do
        begin
          passwordDialog.Visible := False;
          passwordDialog.EditServiceURL.Text := serviceurl;
          passwordDialog.EditUsername.Text := username;
          passwordDialog.EditPassword.Text := '';

          if passwordDialog.showModal = mrOk then
          begin
            serviceUrl := passwordDialog.EditServiceURL.Text;
            username := passwordDialog.EditUsername.Text;
            password := passwordDialog.EditPassword.Text;
            opsiServiceVersion :=
              getOpsiServiceVersion(serviceurl, username, password, sessionid);
            if opsiServiceVersion = '4' then
            begin
              local_opsidata := TOpsi4Data.Create;
              local_opsidata.initOpsiConf(serviceurl, username, password, sessionid);
              //Topsi4data(local_opsidata).initOpsiConf(serviceurl, username, password, sessionid);
              //OpsiData.setOptions (opsiclientd_serviceoptions);
              omc := TOpsiMethodCall.Create('backend_info', []);
              //omc := TOpsiMethodCall.create ('authenticated',[]);
              testresult := local_opsidata.CheckAndRetrieve(omc, errorOccured);
              omc.Free;
            end
            else
              LogDatei.log('Internal Error: Unkown Opsi Service Version:>' +
                opsiServiceVersion + '<', LLerror);

            stopIt := not errorOccured;
          end

          else
          begin
            stopIt := True;
            errorOccured := True;
            testresult := 'Cancelled by user';
          end;

        end;

        passwordDialog.Free;

          {$ENDIF GUI}
      end;
    end;

    if errorOccured then
    begin
      local_opsidata := nil;
      LogDatei.log('Error: ' + testresult, LLError);
    end;

    if local_opsidata = nil then
    begin
      errorOccured := True;
      LogDatei.log('Error: no connection to service', LLError);
    end;

  end;

  if not errorOccured and syntaxcheck then
  begin
    setlength(parameters, paramList.Count);
    for j := 0 to paramList.Count - 1 do
    begin
      parameters[j] := paramlist.Strings[j];
    end;

    omc := TOpsiMethodCall.Create(methodname, list, parameters);
    testresult := '';

    TStringList(output) := local_opsidata.checkAndRetrieveHashList(omc, errorOccured);
    if output.Count > 0 then
      testresult := output.strings[0];

    for j := 0 to output.Count - 1 do
      testresult := testresult + ', ' + output.strings[j];

    omc.Free;
    //Logdatei.log('Setup script name: '+opsidata.getProductScriptPath(tacSetup), LLessential);

    if errorOccured then
    begin
      LogDatei.log('Error: ' + testresult, LLError);
    end
    else
    begin
      LogDatei.log('JSON result: ' + testresult, LLinfo);
    end;
  end;

  // finishing our section
  finishSection(Sektion, OldNumberOfErrors, OldNumberOfWarnings,
    DiffNumberOfErrors, DiffNumberOfWarnings);

  LogDatei.LogSIndentLevel := startSIndentLevel;

  if ExitOnError and (DiffNumberOfErrors > 0) then
    Result := tsrExitProcess;

  if not SyntaxCheck then
  begin
    if i >= Sektion.Count then
      reportError(Sektion, Sektion.Count, Sektion.strings[Sektion.Count - 1],
        ErrorInfo)
    else
      reportError(Sektion, i, Sektion.strings[i - 1], ErrorInfo);
  end;
end;
//#############################################################################

{$IFDEF WINDOWS}
function TuibInstScript.doXMLPatch(const Sektion: TWorkSection;
  const XMLFilename: string; var output: TXStringList): TSectionResult;
begin
  //DataModuleLogServer.IdTCPServer1.Active:=true;
  Result := executeWith(Sektion, '"' +
    ExtractFileDir(reencode(ParamStr(0), 'system')) + PathDelim +
    'opsiwinstxmlplugin.exe" --xmlfile="' + trim(XMLFilename) +
    '" --scriptfile=', True, 0, output);
  LogDatei.includelogtail(logdatei.StandardLogPath + 'opsiwinstxmlplugin.log',
    1000, 'auto');
  DeleteFile(logdatei.StandardLogPath + 'opsiwinstxmlplugin.log');
  //DataModuleLogServer.IdTCPServer1.Active:=false;
end;

{$ELSE WINDOWS}
function TuibInstScript.doXMLPatch(const Sektion: TWorkSection;
  const XMLFilename: string; var output: TXStringList): TSectionResult;
begin
  LogDatei.log('Not implemented for Linux', LLError);
  Result := tsrExitProcess;
end;

{$ENDIF WINDOWS}

function TuibInstScript.doXMLPatch2(const Sektion: TWorkSection;
  const XMLFilename: string; var output: TXStringList): TSectionResult;
var
  XMLDocObject: TuibXMLDocument;
  r: string = '';
  i: integer = 0;
  k: integer = 0;
  Expressionstr: string = '';
  SyntaxCheck: boolean;
  ErrorInfo: string = '';
  nodeOpened: boolean;
  nodeOpenCommandExists: boolean;
  nodepath: string;
  newtext, newname, newvalue, newnode: string;
  myfilename: string;
  openstrict: boolean = False;
  testbool: boolean;
begin
  Result := tsrPositive;

  myfilename := CleanAndExpandFilename(XMLFilename);
  if not FileExists(myfilename) then
  begin
    LogDatei.log('Error: XML file not found: ' + myfilename, LLCritical);
    Result := tsrFatalError;
    exit;
  end;
  if not initSection(Sektion, OldNumberOfErrors, OldNumberOfWarnings) then
    exit;
  //StartIndentLevel := LogDatei.LogSIndentLevel;
  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

  nodeOpened := False;
  nodeOpenCommandExists := False;
  nodepath := '';

  // createXMLDoc
  XMLDocObject := TuibXMLDocument.Create;
  XMLDocObject.debuglevel := oslog.LLinfo;
  // open xmlfile
  if XMLDocObject.openXmlFile(myfilename) then
    LogDatei.log('success: create xmldoc from file: ' + myfilename, oslog.LLinfo)
  else
    LogDatei.log('failed: create xmldoc from file: ' + myfilename, oslog.LLError);

  // parse section
  for i := 1 to Sektion.Count do
  begin
    r := cutLeftBlanks(Sektion.strings[i - 1]);
    if (r = '') or (r[1] = LineIsCommentChar) then
    // continue
    else
    begin
      GetWord(r, Expressionstr, r, WordDelimiterSet1);


      if LowerCase(Expressionstr) = LowerCase('StrictMode') then
      begin
        testbool := False;
        SyntaxCheck := False;
        if Skip('=', r, r, ErrorInfo) then
        begin
          Getword(r, newtext, r, WordDelimiterWhiteSpace);
          if newtext <> '' then
          begin
            try
              testbool := StrToBool(newtext);
              openstrict := testbool;
            except
              LogDatei.log('StrictMode Argument: ' + newtext +
                ' can not converted to a boolean value. Use true or false. Using (fallback): false.',
                LLError);
            end;
            LogDatei.log('StrictMode is set to : ' +
              BoolToStr(openstrict, True), LLdebug);
            syntaxCheck := True;
          end
          else
          begin
            LogDatei.log(
              'Empty StrictMode Argument can not converted to a boolean value. Use true or false. Using (fallback): false.',
              LLError);
            syntaxCheck := False;
          end;
        end
        else
          syntaxCheck := False;
      end;  // StrictMode


      (*
      if LowerCase (Expressionstr) = LowerCase ('OpenNode')
      then
      Begin
        if nodeOpenCommandExists // i.e., existed already
        then
           //LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1
        else
           nodeOpenCommandExists := true;
        SyntaxCheck := false;
        if GetStringA (trim(r), nodepath, r, errorinfo, true) then
        begin
          LogDatei.log('We will OpenNode : '+nodepath, LLdebug);
          syntaxCheck := true;
        end
        else  syntaxCheck := false;
        if r = '' then SyntaxCheck := true
        else ErrorInfo := ErrorRemaining;
        //else SyntaxCheck := true ;
        if SyntaxCheck
        then
        Begin
          // Nodetext setzen und Attribut setzen :   SetText, SetAttribute
          if XMLDocObject.nodeExists(nodepath) then
          begin
            LogDatei.log('successfully found node: '+nodepath,oslog.LLinfo);
            if XMLDocObject.openNode(nodepath, openstrict) then
            begin
              nodeOpened := true;
              LogDatei.log('successfully opend node: '+nodepath,oslog.LLinfo);
            end
            else
            begin
              nodeOpened := false;
              LogDatei.log('failed opend node: '+nodepath,oslog.LLwarning);
            end
          end
          else
          begin
            nodeOpened := false;
            LogDatei.log('failed node exists: '+nodepath,oslog.LLwarning);
          end
        End
        else
          reportError (Sektion, i, Sektion.strings [i-1], ErrorInfo);
      End;   // opnenode
      *)
      (*
      if LowerCase (Expressionstr) = LowerCase ('OpenNode')
      then
      Begin
        if nodeOpenCommandExists // i.e., existed already
        then
           //LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1
        else
           nodeOpenCommandExists := true;
        SyntaxCheck := false;
        if GetStringA (trim(r), nodepath, r, errorinfo, true) then
        begin
          LogDatei.log('We will OpenNode : '+nodepath, LLdebug);
          syntaxCheck := true;
        end
        else  syntaxCheck := false;
        if r = '' then SyntaxCheck := true
        else ErrorInfo := ErrorRemaining;
        //else SyntaxCheck := true ;
        if SyntaxCheck
        then
        Begin
          // Nodetext setzen und Attribut setzen :   SetText, SetAttribute
          if XMLDocObject.nodeExists(nodepath) then
          begin
            LogDatei.log('successfully found node: '+nodepath,oslog.LLinfo);
            if XMLDocObject.openNode(nodepath, openstrict) then
            begin
              nodeOpened := true;
              LogDatei.log('successfully opend node: '+nodepath,oslog.LLinfo);
            end
            else
            begin
                nodeOpened := false;
                LogDatei.log('failed opend node: '+nodepath,oslog.LLwarning);
            end
          end
          else
          begin
            nodeOpened := false;
            LogDatei.log('nodepath does not exists - try to create: '+nodepath,oslog.LLwarning);
            if XMLDocObject.makeNodePathWithTextContent(nodepath,'') then
            begin
              nodeOpened := true;
              LogDatei.log('successfully created nodepath: '+nodepath,oslog.LLinfo);
            end
            else
            begin
               nodeOpened := false;
               LogDatei.log('failed to create nodepath: '+nodepath,oslog.LLError);
            end;
          end
        End
        else
          reportError (Sektion, i, Sektion.strings [i-1], ErrorInfo);
      End;   // opnenode
      *)


      if LowerCase(Expressionstr) = LowerCase('OpenNode') then
      begin
        if nodeOpenCommandExists // i.e., existed already
        then
        //LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1
        else
          nodeOpenCommandExists := True;
        SyntaxCheck := False;
        if GetStringA(trim(r), nodepath, r, errorinfo, True) then
        begin
          LogDatei.log('We will OpenNode : ' + nodepath, LLdebug);
          syntaxCheck := True;
        end
        else
          syntaxCheck := False;
        if r = '' then
          SyntaxCheck := True
        else
          ErrorInfo := ErrorRemaining;
        //else SyntaxCheck := true ;
        if SyntaxCheck then
        begin
          // Nodetext setzen und Attribut setzen :   SetText, SetAttribute
          if XMLDocObject.openNode(nodepath, openstrict, errorinfo) then
          begin
            nodeOpened := True;
            LogDatei.log('successfully opend node: ' + nodepath, oslog.LLinfo);
          end
          else
          begin
            LogDatei.log('nodepath does not exists - try to create: ' +
              nodepath, oslog.LLwarning);
            errorinfo := '';
            if XMLDocObject.makeNodePathWithTextContent(nodepath, '', errorinfo) then
            begin
              nodeOpened := True;
              LogDatei.log('successfully created nodepath: ' + nodepath, oslog.LLinfo);
            end
            else
            begin
              nodeOpened := False;
              LogDatei.log('failed to create nodepath: ' + nodepath, oslog.LLError);
              if errorinfo <> '' then
              begin
                syntaxCheck := False;
                reportError(Sektion, i, Sektion.strings[i - 1], ErrorInfo);
              end;
            end;
          end;
        end
        else
          reportError(Sektion, i, Sektion.strings[i - 1], ErrorInfo);
      end;   // opnenode

      if LowerCase(Expressionstr) = LowerCase('DeleteNode') then
      begin
        SyntaxCheck := False;
        if GetStringA(trim(r), nodepath, r, errorinfo, True) then
        begin
          LogDatei.log('We will DeleteNode : ' + nodepath, LLdebug);
          syntaxCheck := True;
        end
        else
          syntaxCheck := False;
        if r = '' then
          SyntaxCheck := True
        else
          ErrorInfo := ErrorRemaining;
        if SyntaxCheck then
        begin
          try
            XMLDocObject.delNode(nodepath, errorinfo);
            // After a deleteNode you must use opennode in order to work with open nodes
            nodeOpened := False;
            nodeOpenCommandExists := False;
            LogDatei.log('successfully deleted node: ' + nodepath, oslog.LLinfo);
          except
            on e: Exception do
            begin
              LogDatei.log('Exception in xml2: DeleteNode: ' + e.message, LLError);
            end;
          end;
        end
        else
          reportError(Sektion, i, Sektion.strings[i - 1], ErrorInfo);
      end;   // deleteNode

      if LowerCase(Expressionstr) = LowerCase('setNodeText') then
      begin
        syntaxCheck := True;
        if not (nodeOpened and nodeOpenCommandExists) then
        begin
          //SyntaxCheck := false;
          logdatei.log('Error: No open Node. Use OpenNode before ' +
            Expressionstr, LLError);
        end
        else
        begin
          if SyntaxCheck then
          begin
            if GetStringA(trim(r), newtext, r, errorinfo, True) then
            begin
              LogDatei.log('We will setNodeText : ' + newtext, LLdebug);
              syntaxCheck := True;
            end
            else
              syntaxCheck := False;
          end;

          if SyntaxCheck then
          begin
            try
              XMLDocObject.setNodeTextActNode(newtext);
              LogDatei.log('successfully setText node: ' + newtext, oslog.LLinfo);
            except
              on e: Exception do
              begin
                LogDatei.log('Exception in xml2:stettext: ' + e.message, LLError);
              end;
            end;
          end
          else
            reportError(Sektion, i, Sektion.strings[i - 1], ErrorInfo);
        end;
      end;   // setNodeText

      if LowerCase(Expressionstr) = LowerCase('gotoParentNode') then
      begin
        syntaxCheck := True;
        if not (nodeOpened and nodeOpenCommandExists) then
        begin
          //SyntaxCheck := false;
          logdatei.log('Error: No open Node. Use OpenNode before ' +
            Expressionstr, LLError);
        end
        else
        begin
          if SyntaxCheck then
          begin
            try
              LogDatei.log('We will gotoParentNode : ' + newtext, LLdebug);
              XMLDocObject.setParentNodeAsActNode();
              LogDatei.log('successfully gotoParentNode ', oslog.LLinfo);
            except
              on e: Exception do
              begin
                LogDatei.log('Exception in xml2:gotoParentNode: ' + e.message, LLError);
              end;
            end;
          end
          else
            reportError(Sektion, i, Sektion.strings[i - 1], ErrorInfo);
        end;
      end;   // gotoParentNode


      if LowerCase(Expressionstr) = LowerCase('addNewNode') then
      begin
        syntaxCheck := True;
        if not (nodeOpened and nodeOpenCommandExists) then
        begin
          //SyntaxCheck := false;
          logdatei.log('Error: No open Node. Use OpenNode before ' +
            Expressionstr, LLError);
        end
        else
        begin
          if SyntaxCheck then
          begin
            if GetStringA(trim(r), newtext, r, errorinfo, True) then
            begin
              LogDatei.log('We will addNewNode : ' + newtext, LLdebug);
              syntaxCheck := True;
            end
            else
              syntaxCheck := False;
          end;

          if SyntaxCheck then
          begin
            try
              XMLDocObject.makeNode(newtext, '', '');
              LogDatei.log('successfully addNewNode: ' + newtext, oslog.LLinfo);
            except
              on e: Exception do
              begin
                LogDatei.log('Exception in xml2:addNewNode: ' + e.message, LLError);
              end;
            end;
          end
          else
            reportError(Sektion, i, Sektion.strings[i - 1], ErrorInfo);
        end;
      end;   // addNewNode


      if LowerCase(Expressionstr) = LowerCase('setAttribute') then
      begin
        syntaxCheck := True;
        if not (nodeOpened and nodeOpenCommandExists) then
        begin
          //SyntaxCheck := false;
          logdatei.log('Error: No open Node. Use OpenNode before ' +
            Expressionstr, LLError);
        end
        else
        begin
          if SyntaxCheck then
          begin
            if GetStringA(trim(r), newname, r, errorinfo, True) then
            begin
              logdatei.log('name= ' + newname, LLDebug2);
              if GetStringA(trim(r), newvalue, r, errorinfo, True) then
              begin
                logdatei.log('value= ' + newvalue, LLDebug2);
                syntaxCheck := True;
                LogDatei.log('We will setAttribute : ' + newname +
                  ' : ' + newvalue, LLdebug);
              end
              else
                syntaxCheck := False;
            end
            else
              syntaxCheck := False;

            if r = '' then
              SyntaxCheck := True
            else
            begin
              SyntaxCheck := False;
              ErrorInfo := ErrorRemaining;
            end;
          end;

          if SyntaxCheck then
          begin
            try
              XMLDocObject.setAttribute(newname, newvalue);
              LogDatei.log('successfully setAttribute : ' + newname +
                ' : ' + newvalue, LLinfo);
            except
              on e: Exception do
              begin
                LogDatei.log('Exception in xml2:setAttribute: ' + e.message, LLError);
              end;
            end;
          end
          else
            reportError(Sektion, i, Sektion.strings[i - 1], ErrorInfo);
        end;
      end;   // setAttribute

      if LowerCase(Expressionstr) = LowerCase('addAttribute') then
      begin
        syntaxCheck := True;
        if not (nodeOpened and nodeOpenCommandExists) then
        begin
          //SyntaxCheck := false;
          logdatei.log('Error: No open Node. Use OpenNode before ' +
            Expressionstr, LLError);
        end
        else
        begin
          if SyntaxCheck then
          begin
            if GetStringA(trim(r), newname, r, errorinfo, True) then
            begin
              logdatei.log('name= ' + newname, LLDebug2);
              if GetStringA(trim(r), newvalue, r, errorinfo, True) then
              begin
                logdatei.log('value= ' + newvalue, LLDebug2);
                syntaxCheck := True;
                LogDatei.log('We will addAttribute : ' + newname +
                  ' : ' + newvalue, LLdebug);
              end
              else
                syntaxCheck := False;
            end
            else
              syntaxCheck := False;

            if r = '' then
              SyntaxCheck := True
            else
            begin
              SyntaxCheck := False;
              ErrorInfo := ErrorRemaining;
            end;
          end;

          if SyntaxCheck then
          begin
            try
              XMLDocObject.addAttribute(newname, newvalue);
              LogDatei.log('successfully addAttribute : ' + newname +
                ' : ' + newvalue, LLinfo);
            except
              on e: Exception do
              begin
                LogDatei.log('Exception in xml2:addAttribute: ' + e.message, LLError);
              end;
            end;
          end
          else
            reportError(Sektion, i, Sektion.strings[i - 1], ErrorInfo);
        end;
      end;   // addAttribute


      if LowerCase(Expressionstr) = LowerCase('deleteAttribute') then
      begin
        syntaxCheck := True;
        if not (nodeOpened and nodeOpenCommandExists) then
        begin
          //SyntaxCheck := false;
          logdatei.log('Error: No open Node. Use OpenNode before ' +
            Expressionstr, LLError);
        end
        else
        begin
          if SyntaxCheck then
          begin
            if GetStringA(trim(r), newname, r, errorinfo, True) then
            begin
              LogDatei.log('We will delAttribute : ' + newname, LLdebug);
              syntaxCheck := True;
            end
            else
              syntaxCheck := False;
          end;

          if SyntaxCheck then
          begin
            //LogDatei.log('We will delAttribute : '+newname, LLdebug);
            try
              XMLDocObject.delAttribute(newname);
              LogDatei.log('successfully delAttribute node: ' + newname, oslog.LLinfo);
            except
              on e: Exception do
              begin
                LogDatei.log('Exception in xml2:delAttribute: ' + e.message, LLError);
              end;
            end;
          end
          else
            reportError(Sektion, i, Sektion.strings[i - 1], ErrorInfo);
        end;
      end;   // delAttribute

      if not syntaxcheck then
        reportError(Sektion, i, Sektion.strings[i - 1], ErrorInfo);

    end; // not a comment line
  end; // any line

  // save xml back
  if XMLDocObject.writeXmlAndCloseFile(myfilename) then
    LogDatei.log('successful written xmldoc to file: ' + myfilename, LLinfo)
  else
    LogDatei.log('failed to write xmldoc to file: ' + myfilename, oslog.LLError);
  XMLDocObject.Destroy;

  if ExitOnError and (DiffNumberOfErrors > 0) then
    Result := tsrExitProcess;
end;



function TuibInstScript.doFileActions(const Sektion: TWorkSection;
  CopyParameter: string): TSectionResult;
var
  Source: string = '';
  Target: string = '';
  SourceDirectory: string = '';
  targetDirectory: string = '';
  startDirectory: string = '';
  presetDirectory: string = '';
  cpSpecify: TcpSpecify;
  NoCounted: integer = 0;
  recursive, ignoreReadOnly: boolean;
  finished: boolean;
  daysback: integer = 0;
  SyntaxCheck: boolean;
  RebootWanted: boolean = False;

  errorinfo: string = '';
  Expressionstr: string = '';
  remaining: string = '';

  findresultcode: integer = 0;
  //searchresult : TSearchRec;
  ch: char;
  oldDisableWow64FsRedirectionStatus: pointer = nil;
  Wow64FsRedirectionDisabled: boolean;
  boolresult: boolean;
  ProfileList: TStringList;
  pc: integer;
  //OldWinapiErrorMode: Cardinal;

  Install: TuibFileInstall;

  procedure fileActionsMain(const Section: TXStringList; const presetDir: string);
  var
    i: integer = 0;
    j: integer = 0;
    dummyint: integer = 0;
    workingSection: TXStringList;
    ///info : string;
    remaining_with_leading_blanks: string = '';
    search4file: boolean;
    mode: string;  // used on linux
    list1: TStringList;
    shellcallArchParam: string;
    go_on: boolean;
    tmpstr, searchmask: string;

  begin
    targetDirectory := presetDir;
    targetDirectory := ExpandFileName(targetDirectory);

    workingSection := TXStringList.Create;
    workingSection.Assign(Section);
    for i := 0 to Sektion.Count - 1 do
    begin
      Remaining := cutLeftBlanks(workingSection.strings[i]);
      logdatei.log(Remaining, LLDebug2);
    end;
    workingSection.GlobalReplace(1, '%userprofiledir%',
      copy(presetDir, 1, length(presetDir) - 1), False);
    for i := 0 to Sektion.Count - 1 do
    begin
      Remaining := cutLeftBlanks(workingSection.strings[i]);
      logdatei.log(Remaining, LLDebug2);
    end;
    workingSection.GlobalReplace(1, '%currentprofiledir%',
      copy(presetDir, 1, length(presetDir) - 1), False);
    for i := 0 to Sektion.Count - 1 do
    begin
      Remaining := cutLeftBlanks(workingSection.strings[i]);
      logdatei.log(Remaining, LLDebug2);
    end;

    ApplyTextConstants(TXStringList(workingSection), False);
    ApplyTextVariables(TXStringList(workingSection), False);
    for i := 0 to Sektion.Count - 1 do
    begin
      Remaining := cutLeftBlanks(workingSection.strings[i]);
      logdatei.log(Remaining, LLDebug2);
    end;


    for i := 1 to Sektion.Count do
    begin
      Remaining := cutLeftBlanks(workingSection.strings[i - 1]);
      logdatei.log(Remaining, LLDebug);
      SyntaxCheck := True;

      if (Remaining = '') or (Remaining[1] = LineIsCommentChar) then
      // continue
      else
      begin
        GetWord(Remaining, Expressionstr, Remaining, WordDelimiterSet0);
        logdatei.log(Expressionstr, LLDebug);
        if UpperCase(Expressionstr) = 'SOURCEPATH' then
        begin
          if (length(Remaining) = 0) or (Remaining[1] <> '=') then
            reportError(Sektion, i, Sektion.strings[i - 1], '=  expected')
          else
          begin
            Remaining := copy(Remaining, 2, length(Remaining) - 1);
            Remaining := cutLeftBlanks(Remaining);

            if not GetString(Remaining, SourceDirectory, Remaining, errorinfo, False)
            then
              GetWord(Remaining, SourceDirectory, Remaining, WordDelimiterSet2);

            if length(Remaining) > 0 then
              reportError(Sektion, i, Sektion.Strings[i - 1], 'end of line expected');

            if (length(SourceDirectory) > 0) and
              (SourceDirectory[length(SourceDirectory)] <> PathDelim) then
              SourceDirectory := SourceDirectory + PathDelim;

          end;

        end

        else if UpperCase(Expressionstr) = 'CHECKTARGETPATH' then
        begin
          if (length(Remaining) = 0) or (Remaining[1] <> '=') then
            reportError(Sektion, i, Sektion.strings[i - 1], '=  expected')
          else
          begin
            Remaining := copy(Remaining, 2, length(Remaining) - 1);
            Remaining := cutLeftBlanks(Remaining);

            if not GetString(Remaining, TargetDirectory, Remaining, errorinfo, False)
            then
              GetWord(Remaining, TargetDirectory, Remaining, WordDelimiterSet2);

            logdatei.log(TargetDirectory, LLDebug);
            if length(Remaining) > 0 then
              reportError(Sektion, i, Sektion.strings[i - 1], 'end of line expected')
            else
              Install.MakePath(TargetDirectory);

            if (length(TargetDirectory) > 0) and
              (TargetDirectory[length(TargetDirectory)] <> PathDelim) then
              TargetDirectory := TargetDirectory + PathDelim;
          end;
        end

        else if UpperCase(Expressionstr) = 'COPY' then
        begin
          if not GetString(Remaining, Expressionstr, Remaining, errorinfo, False)
          then
            GetWord(Remaining, Expressionstr, Remaining, WordDelimiterSet0);

          cpSpecify := 0;

          // which copy options

          if Expressionstr[1] = '-' then
          begin
            Expressionstr := copy(Expressionstr, 2, length(Expressionstr) - 1);
            for j := 1 to length(Expressionstr) do
            begin
              if Expressionstr[j] = 's' then
                cpSpecify := cpSpecify or cpRecursive
              else if Expressionstr[j] = 'e' then
                cpSpecify := cpSpecify or cpCreateEmptySubdirectories
              else if Expressionstr[j] = 'v' then
              begin
                {$IFDEF WINDOWS}
                cpSpecify := cpSpecify or cpVersionControl;
                LogDatei.log('The copy -v Option is deprecated. Use -V', LLWarning);
                {$ELSE}
                LogDatei.log('The copy -v Option is Windows only', LLWarning);
                {$ENDIF WINDOWS}
              end
              else if Expressionstr[j] = 'V' then
              begin
                {$IFDEF WINDOWS}
                cpSpecify := cpSpecify or cpVersionControlRestricted;
                {$ELSE}
                LogDatei.log('The copy -V Option is Windows only', LLWarning);
                {$ENDIF WINDOWS}
              end
              else if Expressionstr[j] = 'd' then
              begin
                {$IFDEF WINDOWS}
                cpSpecify := cpSpecify or cpDateControl;
                {$ELSE}
                LogDatei.log('The copy -d Option is Windows only', LLWarning);
                {$ENDIF WINDOWS}
              end
              else if Expressionstr[j] = 'u' then
              begin
                {$IFDEF WINDOWS}
                cpSpecify := cpSpecify or cpUpdate;
                {$ELSE}
                LogDatei.log('The copy -u Option is Windows only', LLWarning);
                {$ENDIF WINDOWS}
              end
              else if Expressionstr[j] = 'w' then
                cpSpecify := cpSpecify or cpDontForceOverwrite
              else if Expressionstr[j] = 'n' then
                cpSpecify := cpSpecify or cpNoOverwrite
              else if Expressionstr[j] = 'h' then
                cpSpecify := cpSpecify or cpFollowSymlinks
              else if Expressionstr[j] = 'x' then
              begin
                {$IFDEF WINDOWS}
                cpSpecify := cpSpecify or cpExtract;
                {$ELSE}
                LogDatei.log('The copy -x Option is Windows only', LLWarning);
                {$ENDIF WINDOWS}
              end
              else if Expressionstr[j] = 'r' then
              begin
                {$IFDEF WINDOWS}
                cpSpecify := cpSpecify or cpLeaveReadonly;
                {$ELSE}
                LogDatei.log('The copy -r Option is Windows only', LLWarning);
                {$ENDIF WINDOWS}
              end
              else if Expressionstr[j] = 'c' then
              begin
                {$IFDEF WINDOWS}
                cpSpecify := cpSpecify or cpNoExtraReboot;
                {$ELSE}
                LogDatei.log('The copy -c Option is Windows only', LLWarning);
                {$ENDIF WINDOWS}
              end
              else
              begin
                SyntaxCheck := False;
                reportError(Sektion, i, Sektion.strings[i - 1],
                  Expressionstr[j] + ' is not a valid copy option');
              end;
            end;

            if (cpSpecify and cpRecursive) = cpRecursive then
              cpSpecify := cpSpecify or cpCreateEmptySubdirectories;
            // creat also empty directories

            if not GetString(Remaining, Expressionstr, Remaining, errorinfo, False)
            then
              GetWord(Remaining, Expressionstr, Remaining, WordDelimiterSet0);

          end;

          if isAbsoluteFileName(Expressionstr) then
            Source := Expressionstr
          else
            Source := SourceDirectory + Expressionstr;
          Source := ExpandFileName(Source);

          LogDatei.log('source: ' + Source + ' - target: ' + target, LLDebug2);
          if not GetString(Remaining, Target, Remaining, errorinfo, False) then
            Target := Remaining;
          LogDatei.log('source: ' + Source + ' - target: ' + target, LLDebug3);
          target := trim(target);
          Target := opsiUnquoteStr(target, '"');
          Target := opsiUnquoteStr(target, '''');
          LogDatei.log('source: ' + Source + ' - target: ' + target, LLDebug3);
          if not isAbsoluteFileName(Target) then
          begin
            Target := TargetDirectory + Remaining;
          end;
          LogDatei.log('source: ' + Source + ' - target: ' + target, LLDebug3);
          target := trim(target);
          LogDatei.log('source: ' + Source + ' - target: ' + target, LLDebug3);
          target := ExpandFileName(target);
          LogDatei.log('source: ' + Source + ' - target: ' + target, LLDebug3);
          if Target = '' then
          begin
            SyntaxCheck := False;
            reportError(Sektion, i, Sektion.strings[i - 1],
              'No target directory defined');
          end;

          if SyntaxCheck then
          begin
            if Install.MakePath(Target) then
            begin
              Target := ExpandFileName(Target);
              Install.AllCopy(Source, Target, cpSpecify, tccmCounting,
                NoCounted, RebootWanted);
              Install.AllCopy(Source, Target, cpSpecify, tccmCounted,
                NoCounted, RebootWanted);
            end;
            if RebootWanted and not (cpSpecify and cpNoExtraReboot =
              cpNoExtraReboot) then
            begin
              PerformExitWindows := txrReboot; // txrRegisterForReboot; bis Version 3.03
              LogDatei.log('', LLInfo);
              LogDatei.log('ExitWindows set to Reboot', LLnotice);
            end;
          end;

        end

        else if (UpperCase(Expressionstr) = 'DELETE') or
          (UpperCase(Expressionstr) = 'DEL') then
        begin
          search4file := True;
          cpSpecify := 0;
          if UpperCase(Expressionstr) = 'DEL' then
            search4file := False;
          if not GetString(Remaining, Expressionstr, Remaining, errorinfo, False)
          then
          begin
            LogDatei.log('No Getstring: ' + errorinfo, LLDebug3);
            LogDatei.log('after Getstring: rem : ' + remaining +
              ' expr: ' + Expressionstr,
              LLDebug3);
            // instead of using (as up to version 4.1)
            // GetWord (Remaining, Expressionstr, Remaining, WordDelimiterWhiteSpace);
            // we want to be exact with the number of blanks
            DivideAtFirst(' ', remaining, Expressionstr,
              remaining_with_leading_blanks);
            LogDatei.log('after DivideAtFirst: rem : ' +
              remaining_with_leading_blanks + ' expr: ' + Expressionstr, LLDebug3);
            remaining := cutLeftBlanks(remaining_with_leading_blanks);
          end;
          LogDatei.log('Remaining before Options: ' + remaining, LLDebug2);
          LogDatei.log('Expressionstr before Options: ' + Expressionstr, LLDebug2);
          recursive := False;
          ignoreReadOnly := False;
          daysback := 0;
          while (length(Expressionstr) > 0) and (Expressionstr[1] = '-') do
          begin
            Expressionstr := lowerCase(copy(Expressionstr, 2,
              length(Expressionstr) - 1));
            LogDatei.log('Expressionstr while Options: ' + Expressionstr, LLDebug3);
            j := 1;
            while j <= length(Expressionstr) do
            begin
              if Expressionstr[j] = 's' then
              begin
                recursive := True;
                LogDatei.log('Option -s detected: ', LLDebug2);
              end
              else if Expressionstr[j] = 'f' then
              begin
                ignoreReadOnly := True;
                LogDatei.log('Option -f detected: ', LLDebug2);
              end
              else if Expressionstr[j] = 'c' then
              begin
                {$IFDEF WINDOWS}
                cpSpecify := cpSpecify or cpNoExtraReboot;
                LogDatei.log('Option -c detected: ', LLDebug2);
                {$ELSE}
                LogDatei.log('The del -c Option is Windows only', LLWarning);
                {$ENDIF WINDOWS}
              end
              else if Expressionstr[j] = 'd' then
              begin
                LogDatei.log('Option -d detected: ', LLDebug2);
                finished := False;
                while (j + 1 <= length(Expressionstr)) and not
                  finished do
                begin
                  ch := Expressionstr[j + 1];
                  if (Ord('0') <= Ord(ch)) and (Ord(ch) <= Ord('9'))
                  then
                  begin
                    Inc(j);
                    daysback := daysback * 10 + Ord(Expressionstr[j]) - Ord('0');
                  end
                  else
                    finished := True;
                end;
                //default
                if daysback = 0 then
                  daysback := 1;
              end
              else
              begin
                SyntaxCheck := False;
                reportError(Sektion, i, Sektion.strings[i - 1],
                  Expressionstr[j] + ' is no copy option');
              end;
              Inc(j);
            end;

            if not GetString(Remaining, Expressionstr, Remaining, errorinfo, False)
            then
            begin
              DivideAtFirst(' ', remaining, Expressionstr,
                remaining_with_leading_blanks);
              remaining := cutLeftBlanks(remaining_with_leading_blanks);
              //GetWord (Remaining, Expressionstr, Remaining, WordDelimiterWhiteSpace);
            end;

          end;
          LogDatei.log('Remaining: ' + Remaining, LLDebug2);
          LogDatei.log('Expressionstr: ' + Expressionstr, LLDebug2);
          //remaining := cutLeftBlanks(remaining_with_leading_blanks);
          LogDatei.log('we try to delete: ' + Expressionstr, LLDebug2);
          if Remaining <> '' then
            Expressionstr := trim(Expressionstr + ' ' + Remaining_with_leading_blanks);
          // if file names have e.g. blanks in it, don't let them be split
          // (with the consequence of deleting files that have the first part as name)

          if isAbsoluteFileName(Expressionstr) then
            Source := Expressionstr
          else
            Source := SourceDirectory + Expressionstr;
          Source := ExpandFileName(Source);

          if SyntaxCheck then
          begin
            LogDatei.log('we try to delete: ' + Source, LLDebug2);
            Install.AllDelete(Source, recursive, ignoreReadOnly,
              daysback, search4file, RebootWanted);
          end;
          if RebootWanted and not (cpSpecify and cpNoExtraReboot = cpNoExtraReboot) then
          begin
            PerformExitWindows := txrReboot;
            LogDatei.log('', LLInfo);
            LogDatei.log('ExitWindows set to Reboot', LLnotice);
          end;
        end

        {$IFDEF UNIX}
        else if (UpperCase(Expressionstr) = 'CHMOD') then
        begin
          if not GetString(Remaining, Expressionstr, Remaining, errorinfo, False)
          then
          begin
            // instead of using (as up to version 4.1)
            // GetWord (Remaining, Expressionstr, Remaining, WordDelimiterWhiteSpace);
            // we want to be exact with the number of blanks
            DivideAtFirst(' ', remaining, Expressionstr,
              remaining_with_leading_blanks);
            remaining := cutLeftBlanks(remaining_with_leading_blanks);
          end;

          mode := Expressionstr;
          mode := opsiUnquoteStr(mode, '"');
          mode := opsiUnquoteStr(mode, '''');
          if mode = '' then
          begin
            SyntaxCheck := False;
            errorinfo := errorinfo + ' Invalid empty string for mode in chmod';
          end;
          try
            dummyint := StrToInt(mode);
          except
            SyntaxCheck := False;
            errorinfo := errorinfo +
              ' Invalid string for mode in chmod. Expected something like 755 found: '
              + mode;
          end;

          if not GetString(Remaining, Expressionstr, Remaining, errorinfo, False)
          then
          begin
            DivideAtFirst(' ', remaining, Expressionstr,
              remaining_with_leading_blanks);
            remaining := cutLeftBlanks(remaining_with_leading_blanks);
            //GetWord (Remaining, Expressionstr, Remaining, WordDelimiterWhiteSpace);
          end;

          if Remaining <> '' then
            Expressionstr := trim(Expressionstr + ' ' + Remaining_with_leading_blanks);
          // if file names have e.g. blanks in it, don't let them be split
          // (with the consequence of deleting files that have the first part as name)

          if isAbsoluteFileName(Expressionstr) then
            Source := Expressionstr
          else
            Source := SourceDirectory + Expressionstr;
          Source := ExpandFileName(Source);

          if SyntaxCheck then
          begin
            LogDatei.log('we try to chmod: ' + Source + ' to mode: ' + mode, LLDebug2);
            if not Install.chmod(mode, Source) then
              LogDatei.log('Failed to chmod: ' + Source + ' to mode: ' + mode, LLerror);
          end;
        end
        {$ENDIF LINUX}

        else if (UpperCase(Expressionstr) = 'UNZIPFILE') then
        begin
          go_on := True;
          if not GetString(Remaining, Expressionstr, Remaining, errorinfo, False)
          then
          begin
            DivideAtFirst(' ', remaining, Expressionstr,
              remaining_with_leading_blanks);
            remaining := cutLeftBlanks(remaining_with_leading_blanks);
          end;

          Source := Expressionstr;
          LogDatei.log('unzip source: ' + Source, lldebug3);
          Source := ExpandFileNameUTF8(Source);
          if not FileExists(Source) then
          begin
            //syntaxcheck := false;
            //reportError (Sektion, i, Sektion.strings [i-1], source + ' is no existing file or directory');
            go_on := False;
            logDatei.log('Error: unzip: source: ' + Source +
              ' is no existing file', LLError);
          end;

          if syntaxcheck and go_on then
          begin
            if not GetString(Remaining, Target, Remaining, errorinfo, False)
            then
              Target := Remaining;
            LogDatei.log('source: ' + Source + ' - target: ' + target, LLDebug3);
            target := trim(target);
            Target := opsiUnquoteStr(target, '"');
            Target := opsiUnquoteStr(target, '''');
            LogDatei.log('source: ' + Source + ' - target: ' + target, LLDebug3);
            //LogDatei.log('hardlink target: '+Expressionstr,lldebug3);

            target := ExpandFileNameUTF8(target);

            if not (isDirectory(target)) then
            begin
              //syntaxcheck := false;
              //reportError (Sektion, i, Sektion.strings [i-1], target + ' is not a valid file name');
              go_on := False;
              logDatei.log('Error: unzip: target: ' + target +
                ' is not a valid file directory', LLError);
            end;


            if SyntaxCheck and go_on then
            begin
              LogDatei.log('we try to unzip: ' + Source + ' to ' + target, LLInfo);
              try
                if UnzipWithDirStruct(Source, target) then
                  LogDatei.log('unzipped: ' + Source + ' to ' + target, LLInfo)
                else
                  LogDatei.log('Failed to unzip: ' + Source + ' to ' + target, LLError)
              except
                on E: Exception do
                begin
                  LogDatei.log('Exception: Failed to unzip: ' + Source +
                    ' to ' + target + ' : ' + e.message, LLError);
                end;
              end;
            end;
          end;
        end

        else if (UpperCase(Expressionstr) = 'ZIPFILE') then
        begin
          go_on := True;
          if not GetString(Remaining, Expressionstr, Remaining, errorinfo, False)
          then
          begin
            // instead of using (as up to version 4.1)
            // GetWord (Remaining, Expressionstr, Remaining, WordDelimiterWhiteSpace);
            // we want to be exact with the number of blanks
            DivideAtFirst(' ', remaining, Expressionstr,
              remaining_with_leading_blanks);
            remaining := cutLeftBlanks(remaining_with_leading_blanks);
          end;

          Source := Expressionstr;
          LogDatei.log('zip source: ' + Source, lldebug3);
          Source := ExpandFileNameUTF8(Source);
          if not (FileExists(Source) or DirectoryExists(Source)) then
          begin
            //syntaxcheck := false;
            //reportError (Sektion, i, Sektion.strings [i-1], source + ' is no existing file or directory');
            go_on := False;
            logDatei.log('Error: zip: source: ' + Source +
              ' is no existing file or directory', LLError);
          end;

          if syntaxcheck and go_on then
          begin
            if not GetString(Remaining, Target, Remaining, errorinfo, False)
            then
              Target := Remaining;
            LogDatei.log('source: ' + Source + ' - target: ' + target, LLDebug2);
            target := trim(target);
            Target := opsiUnquoteStr(target, '"');
            Target := opsiUnquoteStr(target, '''');
            if DirectoryExists(Source) then
              searchmask := '*.*'
            else
            begin
              searchmask := '';
              if fileexists(Source) then
              begin
                searchmask := ExtractFileName(Source);
                Source := includeTrailingPathDelimiter(ExtractFilePath(Source));
              end;
            end;
            LogDatei.log('source: ' + Source + ' - mask: ' + searchmask +
              ' - target: ' + target, LLDebug);
            if not isAbsoluteFileName(target) then
              target := targetDirectory + target;
            target := ExpandFileNameUTF8(target);

            if not (isAbsoluteFileName(target)) then
            begin
              go_on := False;
              logDatei.log('Error: zip: target: ' + target +
                ' is not a valid file name', LLError);
            end;
            if fileexists(target) then
            begin
              DeleteFileUTF8(target);
              logDatei.log('Info: zip: target: ' + target +
                ' existed and was be removed', LLNotice);
            end;


            if SyntaxCheck and go_on then
            begin
              //tmpstr:=target+pathdelim+ExtractFileNameWithoutExt(source)+'.zip';
              LogDatei.log('we try to zip: ' + Source + searchmask +
                ' to ' + target, LLDebug);
              if ZipWithDirStruct(Source, searchmask, target) then
                LogDatei.log('zipped: ' + Source + searchmask + ' to ' + target, LLInfo)
              else
                LogDatei.log('Failed zo zip: ' + Source + searchmask +
                  ' to ' + target, LLError);
            end;
          end;
        end

        else if (UpperCase(Expressionstr) = 'HARDLINK') then
        begin
          go_on := True;
          if not GetString(Remaining, Expressionstr, Remaining, errorinfo, False)
          then
          begin
            // instead of using (as up to version 4.1)
            // GetWord (Remaining, Expressionstr, Remaining, WordDelimiterWhiteSpace);
            // we want to be exact with the number of blanks
            DivideAtFirst(' ', remaining, Expressionstr,
              remaining_with_leading_blanks);
            remaining := cutLeftBlanks(remaining_with_leading_blanks);
          end;

          Source := Expressionstr;
          LogDatei.log('hardlink source: ' + Source, lldebug3);
          Source := ExpandFileNameUTF8(Source);
          if not (isAbsoluteFileName(Source) and FileExists(Source)) then
          begin
            //syntaxcheck := false;
            //reportError (Sektion, i, Sektion.strings [i-1], source + ' is no existing file or directory');
            go_on := False;
            logDatei.log('Error: hardlink: source: ' + Source +
              ' is no existing file or directory', LLError);
          end;

          if syntaxcheck and go_on then
          begin
            if not GetString(Remaining, Target, Remaining, errorinfo, False)
            then
              Target := Remaining;
            LogDatei.log('source: ' + Source + ' - target: ' + target, LLDebug3);
            target := trim(target);
            Target := opsiUnquoteStr(target, '"');
            Target := opsiUnquoteStr(target, '''');
            LogDatei.log('source: ' + Source + ' - target: ' + target, LLDebug3);
            //LogDatei.log('hardlink target: '+Expressionstr,lldebug3);
            if not isAbsoluteFileName(target) then
              target := targetDirectory + target;
            target := ExpandFileNameUTF8(target);

            if not (isAbsoluteFileName(target)) then
            begin
              //syntaxcheck := false;
              //reportError (Sektion, i, Sektion.strings [i-1], target + ' is not a valid file name');
              go_on := False;
              logDatei.log('Error: hardlink: target: ' + target +
                ' is not a valid file name', LLError);
            end;


            if SyntaxCheck and go_on then
            begin
              LogDatei.log('we try to hardlink: ' + Source + ' to ' + target, LLDebug2);
              if Install.hardlink(Source, target) then
                LogDatei.log('hardlinked: ' + Source + ' to ' + target, LLInfo);
            end;
          end;
        end


        else if (UpperCase(Expressionstr) = 'SYMLINK') then
        begin
          go_on := True;
          if not GetString(Remaining, Expressionstr, Remaining, errorinfo, False)
          then
          begin
            // instead of using (as up to version 4.1)
            // GetWord (Remaining, Expressionstr, Remaining, WordDelimiterWhiteSpace);
            // we want to be exact with the number of blanks
            DivideAtFirst(' ', remaining, Expressionstr,
              remaining_with_leading_blanks);
            remaining := cutLeftBlanks(remaining_with_leading_blanks);
          end;

          Source := Expressionstr;
          Source := ExpandFileNameUTF8(Source);
          if not (isAbsoluteFileName(Source) and FileExists(Source)) then
          begin
            //syntaxcheck := false;
            //reportError (Sektion, i, Sektion.strings [i-1], source + ' is no existing file or directory');
            go_on := False;
            logDatei.log('Error: symlink: source: ' + Source +
              ' is no existing file or directory', LLError);
          end;
          {$IFDEF WINDOWS}
          if GetNTVersionMajor < 6 then
          begin
            //syntaxcheck := false;
            //reportError (Sektion, i, Sektion.strings [i-1], 'Symlink on Windows is not avaible on this Release - needs NT6 and up');
            go_on := False;
            logDatei.log(
              'Warning: symlink: Symlink on Windows is not avaible on this Release - needs NT6 and up',
              LLWarning);
          end;
          {$ENDIF WINDOWS}

          if syntaxcheck and go_on then
          begin
            if not GetString(Remaining, Target, Remaining, errorinfo, False)
            then
              Target := Remaining;
            LogDatei.log('source: ' + Source + ' - target: ' + target, LLDebug3);
            target := trim(target);
            Target := opsiUnquoteStr(target, '"');
            Target := opsiUnquoteStr(target, '''');
            LogDatei.log('source: ' + Source + ' - target: ' + target, LLDebug3);
            if not isAbsoluteFileName(target) then
              target := targetDirectory + target;
            target := ExpandFileNameUTF8(target);

            if not (isAbsoluteFileName(target)) then
            begin
              //syntaxcheck := false;
              //reportError (Sektion, i, Sektion.strings [i-1], target + ' is not a valid file name');
              go_on := False;
              logDatei.log('Error: symlink: target: ' + target +
                ' is not a valid file name', LLError);
            end;


            if SyntaxCheck and go_on then
            begin
              LogDatei.log('we try to symlink: ' + Source + ' to ' + target, LLDebug2);
              if Install.symlink(Source, target) then
                LogDatei.log('symlinked: ' + Source + ' to ' + target, LLInfo);
             {$IFDEF WINDOWS}
              LogDatei.log('Reading symlink via dir to reread the cache', LLInfo);
              if Wow64FsRedirectionDisabled then
                shellcallArchParam := '64'
              else
                shellcallArchParam := '32';
              list1 := TStringList.Create;
              list1.Clear;
              list1.Text := execShellCall('dir ' + target, shellcallArchParam,
                4, False, False).Text;
              //calling shellCall with FetchExitCodePublic=false result is on FLastPrivateExitCode
              list1.Free;
             {$ENDIF WINDOWS}
            end;
          end;
        end

        else if ((UpperCase(Expressionstr) = 'RENAME') or
          (UpperCase(Expressionstr) = 'MOVE')) then
        begin
          if not GetString(Remaining, Expressionstr, Remaining, errorinfo, False)
          then
            GetWord(Remaining, Expressionstr, Remaining, WordDelimiterSet0);

          cpSpecify := 0;
          go_on := True;

          // which move options

          if Expressionstr[1] = '-' then
          begin
            Expressionstr := copy(Expressionstr, 2, length(Expressionstr) - 1);
            for j := 1 to length(Expressionstr) do
            begin
              if Expressionstr[j] = 'c' then
              begin
                {$IFDEF WINDOWS}
                cpSpecify := cpSpecify or cpNoExtraReboot;
                {$ELSE}
                LogDatei.log('The move / rename -c Option is Windows only', LLWarning);
                {$ENDIF WINDOWS}
              end
              else
              begin
                SyntaxCheck := False;
                reportError(Sektion, i, Sektion.strings[i - 1],
                  Expressionstr[j] + ' is not a valid rename / move option');
              end;
            end;
            if not GetString(Remaining, Expressionstr, Remaining, errorinfo, False) then
            begin
              //GetWord (Remaining, Expressionstr, Remaining, WordDelimiterSet0);
              // instead of using (as up to version 4.1)
              // GetWord (Remaining, Expressionstr, Remaining, WordDelimiterWhiteSpace);
              // we want to be exact with the number of blanks
              DivideAtFirst(' ', remaining, Expressionstr,
                remaining_with_leading_blanks);
              remaining := cutLeftBlanks(remaining_with_leading_blanks);
            end;
          end;
          (*
          if not GetString (Remaining, Expressionstr, Remaining, errorinfo, false)
          then
          Begin
             // instead of using (as up to version 4.1)
             // GetWord (Remaining, Expressionstr, Remaining, WordDelimiterWhiteSpace);
             // we want to be exact with the number of blanks
             DivideAtFirst (' ', remaining, Expressionstr, remaining_with_leading_blanks);
             remaining := cutLeftBlanks(remaining_with_leading_blanks);
          End;
          *)
          LogDatei.log('source: ' + Expressionstr + ' - target: ' + Remaining, LLDebug3);
          Source := Expressionstr;
          Source := ExpandFileNameUTF8(Source);
          if not (isAbsoluteFileName(Source) and FileExists(Source)) then
          begin
            //syntaxcheck := false;
            //reportError (Sektion, i, Sektion.strings [i-1], source + ' is no existing file or directory');
            go_on := False;
            logDatei.log('Error: move/rename: source: ' + Source +
              ' is no existing file or directory', LLError);
          end;

          if syntaxcheck and go_on then
          begin
            remaining := cutLeftBlanks(remaining);
            if not GetString(Remaining, Target, Remaining, errorinfo, False)
            then
              Target := Remaining;
            LogDatei.log('source: ' + Source + ' - target: ' + Remaining, LLDebug3);
            LogDatei.log('source: ' + Source + ' - target: ' + target, LLDebug3);
            target := trim(target);
            Target := opsiUnquoteStr(target, '"');
            Target := opsiUnquoteStr(target, '''');
            LogDatei.log('source: ' + Source + ' - target: ' + target, LLDebug3);
            if not isAbsoluteFileName(target) then
              target := targetDirectory + target;
            target := ExpandFileNameUTF8(target);

            if not (isAbsoluteFileName(target)) then
            begin
              //syntaxcheck := false;
              //reportError (Sektion, i, Sektion.strings [i-1], target + ' is not a valid file name');
              go_on := False;
              logDatei.log('Error: move/rename: target: ' + target +
                ' is not a valid file name', LLError);
            end;

            if SyntaxCheck and go_on then
            begin
              LogDatei.log('we try to rename: ' + Source + ' to ' + target, LLDebug);
              if Install.rename(Source, target, RebootWanted) then
              begin
                LogDatei.log('renamed: ' + Source + ' to ' + target, LLInfo);
                if RebootWanted and not (cpSpecify and cpNoExtraReboot =
                  cpNoExtraReboot) then
                begin
                  PerformExitWindows := txrReboot;
                  LogDatei.log('', LLInfo);
                  LogDatei.log('ExitWindows set to Reboot', LLnotice);
                end;
              end;
            end;
          end;
        end

        {$IFDEF WINDOWS}
        else if UpperCase(Expressionstr) = 'ZIP' then
        begin
          if not GetString(Remaining, Expressionstr, Remaining, errorinfo, False)
          then
            GetWord(Remaining, Expressionstr, Remaining, WordDelimiterSet0);

          recursive := False;
          if Expressionstr[1] = '-' then
          begin
            Expressionstr := copy(Expressionstr, 2, length(Expressionstr) - 1);
            for j := 1 to length(Expressionstr) do
            begin
              if Expressionstr[j] = 's' then
                recursive := True
              else
              begin
                SyntaxCheck := False;
                reportError(Sektion, i, Sektion.strings[i - 1],
                  Expressionstr[j] + ' is not a valid zip option');
              end;
            end;

            if not GetString(Remaining, Expressionstr, Remaining, errorinfo, False)
            then
              GetWord(Remaining, Expressionstr, Remaining, WordDelimiterSet0);
          end;

          if isAbsoluteFileName(Expressionstr) then
            Target := Expressionstr
          else
            Target := TargetDirectory + Expressionstr;

          if isAbsoluteFileName(Remaining) then
            Source := Remaining
          else
            Source := SourceDirectory + Remaining;

          if SyntaxCheck then
          begin
            Install.MakePath(Target);
            Install.AllCompress(Source, Target, recursive, True);
          end;
        end
        {$ENDIF WINDOWS}
        else
          reportError(Sektion, i, Expressionstr, 'is not a valid command');
      end;

    end;

    workingSection.Free;

  end;

begin
  Wow64FsRedirectionDisabled := False;
  Result := tsrPositive;

  if not initSection(Sektion, OldNumberOfErrors, OldNumberOfWarnings) then
    exit;
  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

  Install := TuibFileInstall.Create;

  {$IFDEF WIN32}
  // disable  critical-error-handler message box. (Drive not ready)
  //OldWinapiErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  if (0 < pos(lowercase(Parameter_64Bit), lowercase(CopyParameter))) or
    (0 < pos(lowercase(Parameter_SysNative), lowercase(CopyParameter))) then
  begin
    boolresult := DSiDisableWow64FsRedirection(oldDisableWow64FsRedirectionStatus);
    Wow64FsRedirectionDisabled := True;
  end
  else
    Wow64FsRedirectionDisabled := False;
   {$ENDIF WIN32}

  if (0 < pos(lowercase(Parameter_AllNTUserProfiles), lowercase(CopyParameter))) or
    flag_all_ntuser then
  begin
    flag_all_ntuser := False;
    ProfileList := getProfilesDirList;
    for pc := 0 to ProfileList.Count - 1 do
    begin
      presetdirectory := ProfileList.Strings[pc] + PathDelim;
      if DirectoryExistsUTF8(presetdirectory) then
      begin
        logdatei.log(' Make it for user directory: ' + presetdirectory, LLInfo);
        fileActionsMain(Sektion, presetDirectory);
      end;
    end;
  end
  else if 0 < pos(lowercase(Parameter_AllNTUserSendTo), lowercase(CopyParameter)) then
  begin
    flag_all_ntuser := False;
    ProfileList := getProfilesDirList;
    for pc := 0 to ProfileList.Count - 1 do
    begin
      presetdirectory := ProfileList.Strings[pc] + '\SendTo\';
      fileActionsMain(Sektion, presetDirectory);
    end;
  end
  else
  begin
    // no special preset directory
    SourceDirectory := '';
    PresetDirectory := '';
    if runLoginScripts then
      PresetDirectory := GetUserProfilePath + PathDelim;
    fileActionsMain(Sektion, presetdirectory);
    {$IFDEF WIN32}
    //setErrorMode(OldWinapiErrorMode);
    if Wow64FsRedirectionDisabled then
    begin
      boolresult := DSiRevertWow64FsRedirection(oldDisableWow64FsRedirectionStatus);
      Wow64FsRedirectionDisabled := False;
    end;
    {$ENDIF WIN32}
  end;


  Install.Free;
  Install := nil;

  finishSection(Sektion, OldNumberOfErrors, OldNumberOfWarnings,
    DiffNumberOfErrors, DiffNumberOfWarnings);


  if ExitOnError and (DiffNumberOfErrors > 0) then
    Result := tsrExitProcess;


  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;

end;


function TuibInstScript.doLinkFolderActions(const Sektion: TWorkSection;
  common: boolean): TSectionResult;
var
   {$IFDEF WINDOWS}
  ShellLinks: TuibShellLinks;
{$ENDIF WINDOWS}
   {$IFDEF UNIX}
  ShellLinks: TuibLinuxDesktopFiles;
{$ENDIF LINUX}
  stack: TStringList;
  startindentlevel: integer;
  Expressionstr: string = '';
  ///remaining : String;

  procedure linkActionsMain();
  var
    i: integer = 0;
    SyntaxCheck: boolean;
    Remaining: string = '';
    errorInfo: string = '';

    csidl: integer = 0;
    csidl_set: boolean;
    folder_opened: boolean;

    basefolder: string = '';
    subfoldername: string = '';
    deletefoldername: string = '';
    s: string = '';

    in_link_features: boolean;
    regular_end: boolean;
    link_name: string = '';
    link_target: string = '';
    link_paramstr: string = '';
    link_working_dir: string = '';
    link_icon_file: string = '';
    link_icon_index: integer = 0;
    link_categories: string = '';
    link_shortcut: word = 0;
    link_showwindow: integer = 0;

  begin

    i := 1;
    if Sektion.Count = 0 then
      exit;

    stack := TStringList.Create;

    csidl := 0;
    csidl_set := False;
    folder_opened := False;
    subfoldername := '';

    while i <= Sektion.Count do
    begin
      Remaining := trim(Sektion.strings[i - 1]);
      if (Remaining = '') or (Remaining[1] = LineIsCommentChar) then
        Inc(i)
      else
      begin
        GetWord(Remaining, Expressionstr, Remaining, WordDelimiterSet0);


        if LowerCase(Expressionstr) = 'set_basefolder' then
        begin

          syntaxcheck := True;
          if (length(Remaining) = 0) then
          begin
            syntaxCheck := False;
            reportError(Sektion, i, Sektion.strings[i - 1], 'folder name  expected');
          end
          else
          begin
            if not GetString(Remaining, basefolder, Remaining, errorinfo, False)
            then
            begin
              basefolder := Remaining;
              Remaining := '';
            end;

            if length(Remaining) > 0 then
            begin
              reportError(Sektion, i, Sektion.Strings[i - 1], 'end of line expected');
            end;

            if producecsidlfromName(basefolder, csidl, errorinfo) then
            begin
              csidl_set := True;
              LogDatei.log('Base folder is ' +
                ShellLinks.Tell_Systemfolder(csidl), levelComplete);
            end
            else
            begin
              LogDatei.log('Error: ' + errorinfo, LLError);
            end;

          end;
        end

        else if LowerCase(Expressionstr) = 'set_subfolder' then
        begin
          if not getString(Remaining, subfoldername, Remaining, errorinfo, False)
          then
          begin
            subfoldername := Remaining;
            Remaining := '';
          end;

          if length(Remaining) > 0 then
          begin
            reportError(Sektion, i, Sektion.Strings[i - 1], 'end of line expected');
          end;

          if csidl_set then
          begin
            ShellLinks.OpenShellFolderPath(csidl, subfoldername);
            folder_opened := True;
          end
          else
            LogDatei.log('No base folder set, therefore subfolder not set',
              LLWarning);

        end
        {$IFDEF WIN32}
        else if LowerCase(Expressionstr) = 'delete_subfolder' then
        begin

          if not getString(Remaining, deletefoldername, Remaining, errorinfo, False)
          then
          begin
            deletefoldername := Remaining;
            Remaining := '';
          end;

          if length(Remaining) > 0 then
          begin
            reportError(Sektion, i, Sektion.Strings[i - 1], 'end of line expected');
            syntaxCheck := False;
          end;

          if csidl_set then
          begin
            LogDatei.LogSIndentLevel := startindentlevel;
            ShellLinks.DeleteShellFolder(csidl, deletefoldername);
            //csidl_set := false;
          end
          else
            LogDatei.log('No base folder set, therefore no deletion of subfolder',
              LLWarning);

        end
        {$ENDIF WIN32}

        else
        if LowerCase(Expressionstr) = 'delete_element' then
        begin
          syntaxcheck := True;
          if not getString(Remaining, s, Remaining, errorinfo, False) then
          begin
            s := Remaining;
            Remaining := '';
          end;

          if length(Remaining) > 0 then
          begin
            reportError(Sektion, i, Sektion.Strings[i - 1], 'end of line expected');
            syntaxcheck := False;
          end;
          {$IFDEF WINDOWS}
          if folder_opened then
          begin
            if syntaxcheck then
              ShellLinks.DeleteShellLink(s);

          end
          else
            LogDatei.log('No folder selected, therefore no deletion of "' +
              s + '"', LLWarning);
          {$ENDIF WINDOWS}
          {$IFDEF UNIX}
          ShellLinks.DeleteShellLink(s);
          {$ENDIF LINUX}
        end

        else if LowerCase(Expressionstr) = 'set_link' then
        begin
          syntaxcheck := True;

          if length(Remaining) > 0 then
          begin
            reportError(Sektion, i, Sektion.Strings[i - 1], 'end of line expected');
            syntaxCheck := False;
          end;
          Inc(i);
          in_link_features := True;
          regular_end := False;
          link_name := '';
          link_target := '';
          link_paramstr := '';
          link_working_dir := '';
          link_icon_file := '';
          link_icon_index := 0;
          link_shortcut := 0;
          link_showwindow := 0;

          while (i <= Sektion.Count) and in_link_features do
          begin
            Remaining := trim(Sektion.strings[i - 1]);
            if (Remaining = '') or (Remaining[1] = LineIsCommentChar) then
              Inc(i)
            else
            begin
              GetWord(Remaining, Expressionstr, Remaining, [':']);
              Skip(':', Remaining, Remaining, errorInfo);

              if LowerCase(Expressionstr) = 'name' then
              begin
                if not getString(Remaining, link_name, Remaining, errorinfo, False)
                then
                begin
                  link_name := Remaining;
                  Remaining := '';
                end;
                LogDatei.log_prog('Link_name: ' + link_name, LLDebug);
              end

              else if LowerCase(Expressionstr) = 'target' then
              begin
                if not getString(Remaining, link_target, Remaining, errorinfo, True)
                then
                begin
                  link_target := Remaining;
                  Remaining := '';
                end;
                LogDatei.log_prog('link_target: ' + link_target, LLDebug);
              end

              else if LowerCase(Expressionstr) = 'parameters' then
              begin
                if not getString(Remaining, link_paramstr, Remaining, errorinfo, True)
                then
                begin
                  link_paramstr := Remaining;
                  Remaining := '';
                end;
                LogDatei.log_prog('link_paramstr: ' + link_paramstr, LLDebug);
              end

              else if LowerCase(Expressionstr) = 'working_dir' then
              begin
                if not getString(Remaining, link_working_dir, Remaining,
                  errorinfo, False) then
                begin
                  link_working_dir := Remaining;
                  Remaining := '';
                end;
                LogDatei.log_prog('link_working_dir: ' + link_working_dir, LLDebug);
              end


              else if LowerCase(Expressionstr) = 'working_directory' then
              begin
                if not getString(Remaining, link_working_dir, Remaining,
                  errorinfo, False) then
                begin
                  link_working_dir := Remaining;
                  Remaining := '';
                end;
                LogDatei.log_prog('link_working_dir: ' + link_working_dir, LLDebug);
              end

              else if LowerCase(Expressionstr) = 'icon_file' then
              begin
                if not getString(Remaining, link_icon_file, Remaining, errorinfo, False)
                then
                begin
                  link_icon_file := Remaining;
                  Remaining := '';
                end;
                LogDatei.log_prog('link_icon_file: ' + link_icon_file, LLDebug);
              end


              else if LowerCase(Expressionstr) = 'icon_index' then
              begin
                if not getString(Remaining, s, Remaining, errorinfo, False)
                then
                  s := Remaining;
                {$IFDEF UNIX}
                logdatei.log('Option icon_index is ignored at Linux', LLWarning);
                {$ENDIF LINUX}
                {$IFDEF WIN32}
                if s = '' then
                  link_icon_index := 0
                else
                  try
                    link_icon_index := StrToInt(s);
                    Remaining := s;
                  except
                    reportError(Sektion, i, Sektion.Strings[i - 1],
                      '"' + s + '" could not converted to an integer.');
                  end;
                LogDatei.log_prog('link_icon_index: ' + s, LLDebug);
                {$ENDIF WIN32}
              end

              else if LowerCase(Expressionstr) = 'shortcut' then
              begin
                if not getString(Remaining, s, Remaining, errorinfo, False)
                then
                  s := Remaining;
                {$IFDEF UNIX}
                logdatei.log('Option shurtcut is ignored at Linux', LLWarning);
                {$ENDIF LINUX}
                {$IFDEF WIN32}
                if s = '' then
                  link_shortcut := 0
                else
                  try
                    link_shortcut := ShortCutStringToWinApiWord(s);
                    Remaining := s;
                  except
                    reportError(Sektion, i, Sektion.Strings[i - 1],
                      '"' + s + '" could not converted to a shortcut key.');
                  end;
                LogDatei.log_prog('link_shortcut: ' + s, LLDebug);
                {$ENDIF WIN32}
              end

              else if LowerCase(Expressionstr) = 'window_state' then
              begin
                if not getString(Remaining, s, Remaining, errorinfo, False)
                then
                  s := Remaining;
                {$IFDEF UNIX}
                logdatei.log('Option window_state is ignored at Linux', LLWarning);
                {$ENDIF LINUX}
                {$IFDEF WIN32}
                if s = '' then
                  link_showwindow := 0
                else
                begin
                  s := trim(LowerCase(s));
                  if s = 'normal' then
                    link_showwindow := 1
                  else if s = 'min' then
                    link_showwindow := 7
                  else if s = 'max' then
                    link_showwindow := 3
                  else
                    reportError(Sektion, i, Sektion.Strings[i - 1],
                      '"' + s + '" could not converted to a window_state key.');
                  LogDatei.log_prog('link_showwindow: ' + s, LLDebug);
                end;
                {$ENDIF WIN32}
              end

              else if LowerCase(Expressionstr) = 'link_categories' then
              begin
                if not getString(Remaining, s, Remaining, errorinfo, False)
                then
                  s := Remaining;
                link_categories := s;
                {$IFDEF WIN32}
                logdatei.log('Option link_categories is ignored at WIN32', LLWarning);
                {$ENDIF WIN32}
              end

              else if LowerCase(Expressionstr) = 'end_link' then
              begin
                in_link_features := False;
                regular_end := True;
              end

              else
              begin
                reportError(Sektion, i, Sektion.Strings[i - 1], 'unknown Option');
                syntaxCheck := False;
              end;


              if in_link_features then
                Inc(i);
            end;
          end;



          if not regular_end then
          begin
            reportError(Sektion, i, Sektion.Strings[i - 1], 'end_link missing');
            syntaxCheck := False;
          end;

          //InstallItem (Const CommandLine, ItemName, Workdir, IconPath, IconIndex : String);

          {$IFDEF WIN32}
          if csidl_set then
          begin
            if syntaxCheck then
            begin

              if not folder_opened then
              begin
                if ShellLinks.OpenShellFolderPath(csidl, subfoldername)
                then
                  folder_opened := True;
              end;

              if ShellLinks.MakeShellLink(link_name, link_target,
                link_paramstr, link_working_dir, link_icon_file,
                link_icon_index, link_shortcut, link_showwindow) then
              ;

            end;
          end;
          {$ENDIF WIN32}
          {$IFDEF UNIX}
          if ShellLinks.MakeShellLink(link_name, link_target,
            link_paramstr, link_working_dir, link_icon_file,
            link_categories, '', '') then
          ;
          {$ENDIF UNIX}
        end

        else
          reportError(Sektion, i, Expressionstr, 'is not a defined command');

        Inc(i);
      end;

    end;
  end;

begin
  Result := tsrPositive;

  if not initSection(Sektion, OldNumberOfErrors, OldNumberOfWarnings) then
    exit;

  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
  startindentlevel := LogDatei.LogSIndentLevel;

  begin  // doLinkFolder main
    {$IFDEF WIN32}
    //Install := TuibFileInstall.create;
    if runLoginScripts then
    begin
      if Impersonate2User(usercontextDomain, usercontextUser, usercontextsid) then
      begin
        try
          ShellLinks := TuibShellLinks.Create;
          linkActionsMain;
          ShellLinks.Free;
        finally
          RevertToSelf;
        end;
      end
      else
        LogDatei.log('Error: could not impersonate for user:' +
          usercontextDomain + '\' + usercontextUser, LLError);
    end
    else
    begin
      ShellLinks := TuibShellLinks.Create;
      linkActionsMain;
      ShellLinks.Free;
    end;
    {$ENDIF WIN32}
    {$IFDEF UNIX}
    begin
      ShellLinks := TuibLinuxDesktopFiles.Create;
      linkActionsMain;
      ShellLinks.Free;
    end;
    {$ENDIF LINUX}

  end;  // eigentliche Verarbeitung

  finishSection(Sektion, OldNumberOfErrors, OldNumberOfWarnings,
    DiffNumberOfErrors, DiffNumberOfWarnings);

  if ExitOnError and (DiffNumberOfErrors > 0) then
    Result := tsrExitProcess;

  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
end;


function TuibInstScript.doDDEwithProgman(const Sektion: TWorkSection;
  DDEParameter: string; SaveddeWithProgman: boolean): TSectionResult;
  // not implemented

begin
  // not implemented
end;

function TuibInstScript.execWinBatch(const Sektion: TWorkSection;
  WinBatchParameter: string; WaitConditions: TSetWaitConditions;
  ident: string; WaitSecs: word; runAs: TRunAs; flag_force64: boolean): TSectionResult;

var
  i: integer = 0;
  commandline: string = '';
  report: string = '';
  waitForReturn: boolean;
  StartIndentLevel: integer = 0;
  targetfilename: string = '';
  ///exefilename : string;
  retrycount: integer = 0;
  waitsecsAsTimeout: boolean = False;
  oldDisableWow64FsRedirectionStatus: pointer = nil;
  Wow64FsRedirectionDisabled, boolresult: boolean;

begin
  try
    Result := tsrPositive;
    retrycount := 0;

    if not initSection(Sektion, OldNumberOfErrors, OldNumberOfWarnings) then
      exit;



    StartIndentLevel := LogDatei.LogSIndentLevel;
    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
    {$IFDEF WIN32}
    Wow64FsRedirectionDisabled := False;
    if flag_force64 then
    begin
      boolresult := DSiDisableWow64FsRedirection(oldDisableWow64FsRedirectionStatus);
      Wow64FsRedirectionDisabled := True;
    end
    else
      Wow64FsRedirectionDisabled := False;
    {$ENDIF WIN32}
    {$IFDEF GUI}
    if AutoActivityDisplay then
      FBatchOberflaeche.showAcitvityBar(True);
    {$ENDIF GUI}


    for i := 1 to Sektion.Count do
    begin
      if (Sektion.strings[i - 1] = '') or (Sektion.strings[i - 1]
        [1] = LineIsCommentChar) then
      // continue
      else
      begin

        {GetWordOrStringExpressionstr (Sektion.strings[i-1], Expressionstr, Remaining, report);}

        CommandLine := trim(Sektion.strings[i - 1]);
        if 1 = pos('"', CommandLine) then
        begin
          //we have a heading '"'
          // we assume, that all until the next '"' is the target file
          targetfilename := copy(CommandLine, 2, posex('"', CommandLine, 2) - 2);
        end
        else
        begin
          //we have no heading '"'
          // we assume, that all until the next ' ' is the target file
          targetfilename := leftstr(CommandLine, pos(' ', CommandLine));
        end;
        // targetfile has no path separators we assume it is a command in the path
        // so we dont try to find it
        if (0 = pos(PathDelim, targetfilename)) and
          (0 = pos(PathDelim, targetfilename)) then
          retrycount := 99;
        // if there was no second '"' or ' ' we use the command line
        if targetfilename = '' then
          targetfilename := CommandLine;
        targetfilename := trim(targetfilename);

        while (not FileExists(targetfilename)) and (retrycount < 10) do
        begin
          LogDatei.log('Warning: file not found :' + targetfilename + ' -retrying',
            LLwarning);
          FileGetAttr(targetfilename);
          ProcessMess;
          sleep(200);
          FileGetAttr(targetfilename);
          ProcessMess;
          sleep(200);
          Inc(retrycount);
        end;
        if retrycount = 10 then
          LogDatei.log('Warning: file not found :' + targetfilename + ' - giving up',
            LLwarning);

        LogDatei.log('Call "' + CommandLine + '"', LevelComplete);

        if ttpWaitTimeout in WaitConditions then
          waitsecsAsTimeout := True;
        if WaitSecs > 0 then
          if waitsecsAsTimeout then
            LogDatei.log('   Timeout ' + IntToStr(WaitSecs) +
              ' seconds ', LevelComplete)
          else
            LogDatei.log('   Waiting ' + IntToStr(WaitSecs) +
              ' seconds ', LevelComplete);



        if ttpWaitForWindowVanished in WaitConditions then
        begin
          LogDatei.log('   Waiting until window "' + ident +
            '" has vanished', LevelComplete);
          if not StartProcess(Commandline, sw_hide, True, True,
            False, False, waitsecsAsTimeout, runAs, ident, WaitSecs,
            Report, FLastExitCodeOfExe) then
          begin
            ps := 'Error: ' + Report;
            LogDatei.log(ps, LLError);
          end
          else
            LogDatei.log(Report, LevelComplete);
        end
        else if ttpWaitForWindowAppearing in WaitConditions then
        begin
          LogDatei.log('   Waiting until window "' + ident +
            '" is coming up', LevelComplete);

          if not StartProcess(Commandline, sw_hide, True, False,
            True, False, waitsecsAsTimeout, runAs, ident, WaitSecs,
            Report, FLastExitCodeOfExe) then
          begin
            ps := 'Error: ' + Report;
            LogDatei.log(ps, LLError);
          end
          else
            LogDatei.log(Report, LevelComplete);
        end
        else if ttpWaitForProcessEnding in WaitConditions then
        begin
          LogDatei.log('   Waiting until process "' + ident +
            '" started and has ended', LevelComplete);

          if not StartProcess(Commandline, sw_hide, True, False,
            False, True, waitsecsAsTimeout, runAs, ident, WaitSecs,
            Report, FLastExitCodeOfExe) then
          begin
            ps := 'Error: ' + Report;
            LogDatei.log(ps, LLError);
          end
          else
            LogDatei.log(Report, LevelComplete);
        end
        else
        begin

          if ttpWaitOnTerminate in WaitConditions then
          begin
            WaitForReturn := True;
            LogDatei.log('   Waiting until the called process is finished',
              LevelComplete);
          end

          else
            WaitForReturn := False;
(*
from defines.inc
{ ShowWindow  }
   SW_HIDE = 0;
   SW_MAXIMIZE = 3;
   SW_MINIMIZE = 6;
   SW_NORMAL = 1;
   SW_RESTORE = 9;
   SW_SHOW = 5;
   SW_SHOWDEFAULT = 10;
   SW_SHOWMAXIMIZED = 3;
   SW_SHOWMINIMIZED = 2;
   SW_SHOWMINNOACTIVE = 7;
   SW_SHOWNA = 8;
   SW_SHOWNOACTIVATE = 4;
   SW_SHOWNORMAL = 1;
*)

          if not StartProcess(Commandline, sw_hide, WaitForReturn,
            False, False, False, waitsecsAsTimeout, runAs, '', WaitSecs,
            Report, FLastExitCodeOfExe) then
          begin
            ps := 'Error: ' + Report;
            LogDatei.log(ps, LLError);
          end
          else
            LogDatei.log(Report, LLInfo);
        end;
      end;
    end;
    {$IFDEF WIN32}
    if Wow64FsRedirectionDisabled then
    begin
      boolresult := DSiRevertWow64FsRedirection(oldDisableWow64FsRedirectionStatus);
      Wow64FsRedirectionDisabled := False;
    end;
    {$ENDIF WIN32}
    finishSection(Sektion, OldNumberOfErrors, OldNumberOfWarnings,
      DiffNumberOfErrors, DiffNumberOfWarnings);

    LogDatei.LogSIndentLevel := StartIndentLevel;
    if ExitOnError and (DiffNumberOfErrors > 0) then
      Result := tsrExitProcess;

  finally
    {$IFDEF GUI}
    FBatchOberflaeche.showAcitvityBar(False);
    {$ENDIF GUI}
  end;
end;

{$IFDEF WINDOWS}
function TuibInstScript.execPowershellCall(command: string; archparam: string;
  logleveloffset: integer; FetchExitCodePublic, FatalOnFail: boolean;
  handle_policy: boolean): TStringList;
var
  commandline: string = '';
  //fullps : string;
  runas: TRunAs;
  //force64: boolean;
  oldDisableWow64FsRedirectionStatus: pointer = nil;
  output: TXStringList;
  tmplist: TStringList;
  //dummybool : boolean;
  filename: string = '';
  parameters: string = '';
  report: string = '';
  errorinfo: string = '';
  i: integer = 0;
  localExitCode: longint = 0;
  org_execution_policy: string;
  mySektion: TWorkSection;
  ActionResult: TSectionResult;
  shortarch: string;  // for execShellCall
begin
  try
    Result := TStringList.Create;
    output := TXStringList.Create;
    runAs := traInvoker;
    OldNumberOfErrors := LogDatei.NumberOfErrors;
    OldNumberOfWarnings := LogDatei.NumberOfWarnings;
    //force64 := false;
    shortarch := 'sysnative';
    {$IFDEF GUI}
    if AutoActivityDisplay then
      FBatchOberflaeche.showAcitvityBar(True);
    {$ENDIF GUI}

    if (lowercase(archparam) = '64bit') and Is64BitSystem then
      shortarch := '64';

    if (lowercase(archparam) = 'sysnative') and Is64BitSystem then
      shortarch := 'sysnative';

    if (lowercase(archparam) = '32bit') then
      shortarch := '32';

    LogDatei.log('PowerhellCall Executing: ' + command + ' ; mode: ' + shortarch,
      LLNotice + logleveloffset);


    if handle_policy then // backup and set execution policy
    begin
      // backup
      commandline := 'powershell.exe get-executionpolicy';
      tmplist := execShellCall(commandline, shortarch, 1, False, True);
      org_execution_policy := trim(tmplist[0]);
      // set (open)
      commandline := 'powershell.exe set-executionpolicy RemoteSigned';
      tmplist := execShellCall(commandline, shortarch, 1, False, True);
    end;

    mySektion := TWorkSection.Create(NestingLevel, ActiveSection);
    mySektion.Add('trap { write-output $_ ; exit 1 }');
    mySektion.Add(command);
    mySektion.Add('exit $LASTEXITCODE');
    mySektion.Name := 'tmp-internal';
    parameters := 'powershell.exe winst /' + archparam;
    if not FetchExitCodePublic then // backup last extcode
      localExitCode := FLastExitCodeOfExe;
    ActionResult := executeWith(mySektion, parameters, True, logleveloffset + 1, output);
    if not FetchExitCodePublic then  // restore last extcode
    begin
      FLastPrivateExitCode := FLastExitCodeOfExe;
      FLastExitCodeOfExe := localExitCode;
    end;
    LogDatei.log('output:', LLDebug + logleveloffset);
    LogDatei.log('--------------', LLDebug + logleveloffset);
    for i := 0 to output.Count - 1 do
    begin
      LogDatei.log(output.strings[i], LLDebug + logleveloffset);
    end;
    //LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 6;
    LogDatei.log('', LLDebug + logleveloffset);
    Result.Text := output.Text;
    output.Free;

    if handle_policy then // restore execution policy
    begin
      // set (close)
      commandline := 'powershell.exe set-executionpolicy ' + org_execution_policy;
      tmplist := execShellCall(commandline, shortarch, 1, False, True);
    end;
  finally
    {$IFDEF GUI}
    FBatchOberflaeche.showAcitvityBar(False);
    {$ENDIF GUI}
  end;
end;

{$ENDIF WINDOWS}

function TuibInstScript.execShellCall(command: string; archparam: string;
  logleveloffset: integer; FetchExitCodePublic: boolean): TStringList;
begin
  Result := execShellCall(command, archparam, logleveloffset, FetchExitCodePublic, True);
end;

function TuibInstScript.execShellCall(command: string; archparam: string;
  logleveloffset: integer; FetchExitCodePublic, FatalOnFail: boolean): TStringList;
var
  commandline: string = '';
  runas: TRunAs;
  force64: boolean;
  oldDisableWow64FsRedirectionStatus: pointer = nil;
  output: TXStringList;
  dummybool: boolean;
  filename: string = '';
  parameters: string = '';
  report: string = '';
  errorinfo: string = '';
  i: integer = 0;
  localExitCode: longint = 0;
  cmdMuiFiles: TStringList;
  muisrcpath, muitargetpath: string;
begin
  try
    Result := TStringList.Create;
    output := TXStringList.Create;
    runAs := traInvoker;
    OldNumberOfErrors := LogDatei.NumberOfErrors;
    OldNumberOfWarnings := LogDatei.NumberOfWarnings;
    force64 := False;

    if (lowercase(archparam) = '64') and Is64BitSystem then
      force64 := True;

    if (lowercase(archparam) = 'sysnative') and Is64BitSystem then
      force64 := True;

    if (lowercase(archparam) = '32') then
      force64 := False;

    {$IFDEF WIN32}
    if force64 then
      if not cmd64checked then
      begin
        if (not FileExists(GetWinDirectory + '\cmd64.exe')) or
          (FindAllFiles('c:\windows', 'cmd64.exe.mui', True).Count = 0) then
        begin
          Logdatei.log(GetWinDirectory + '\cmd64.exe not found - try to get it',
            LLDebug2 + logleveloffset);
          try
            if DSiDisableWow64FsRedirection(oldDisableWow64FsRedirectionStatus) then
            begin
              LogDatei.log('DisableWow64FsRedirection succeeded',
                LLinfo + logleveloffset);
              if FileExists(GetWinSystemDirectory + '\cmd.exe') then
              begin
                if fileutil.CopyFile(GetWinSystemDirectory + '\cmd.exe',
                  GetWinDirectory + '\cmd64.exe') then
                begin
                  LogDatei.log('cmd64.exe created in ' + GetWinDirectory,
                    LLinfo + logleveloffset);
                  cmdMuiFiles := TStringList.Create;
                  cmdMuiFiles :=
                    FindAllFiles('c:\windows\system32', 'cmd.exe.mui', True);
                  for i := 0 to cmdMuiFiles.Count - 1 do
                  begin
                    LogDatei.log('cmd.exe.mui found in ' + cmdMuiFiles.Strings[i],
                      LLinfo + logleveloffset);
                    muisrcpath := ExtractFileDir(cmdMuiFiles.Strings[i]);
                    muitargetpath := ReplaceStr(muisrcpath, 'system32\', '');
                    if fileutil.CopyFile(muisrcpath + '\cmd.exe.mui',
                      muitargetpath + '\cmd64.exe.mui') then
                      LogDatei.log('created : ' + muitargetpath + '\cmd64.exe.mui',
                        LLinfo + logleveloffset);
                  end;
                  cmdMuiFiles.Free;
                end
                else
                  LogDatei.log('could not get cmd64.exe', LLError);
              end
              else
                LogDatei.log('could see: ' + GetWinSystemDirectory +
                  '\cmd.exe', LLError);
              dummybool := DSiRevertWow64FsRedirection(
                oldDisableWow64FsRedirectionStatus);
              LogDatei.log('RevertWow64FsRedirection succeeded',
                LLinfo + logleveloffset);
            end
            else
            begin
              LogDatei.log('Error: DisableWow64FsRedirection failed', LLError);
            end;
          except
            on ex: Exception do
            begin
              LogDatei.log('Error: ' + ex.message, LLError);
            end;
          end;
        end;
        if FileExists(GetWinDirectory + '\cmd64.exe') then
          cmd64checked := True
        else
          LogDatei.log('no cmd64.exe - will use cmd.exe instead', LLError);
      end;
    {$ENDIF WIN32}

    if (GetUibOsType(errorinfo) = tovLinux) or
      (GetUibOsType(errorinfo) = tovMacOS) then
    begin
      FileName := '/bin/bash';
      Parameters := Parameters + ' -c "' + command + ' || exit $?"';
    end
    else
    begin
     {$IFDEF WINDOWS}
      if force64 and FileExists(GetWinDirectory + '\cmd64.exe') then
        FileName := '"' + GetWinDirectory + '\cmd64.exe"'
      else
        FileName := '"cmd.exe"';
      Parameters := ' /C "' + command + '" ';
     {$ENDIF WINDOWS}
    end;
    {$IFDEF GUI}
    if AutoActivityDisplay then
      FBatchOberflaeche.showAcitvityBar(True);
    {$ENDIF GUI}

    commandline := FileName + ' ' + trim(Parameters);

    LogDatei.log('ShellCall Executing: ' + commandline, LLNotice + logleveloffset);
    if not RunCommandAndCaptureOut(commandline, True, output,
      report, SW_HIDE, localExitCode, False, logleveloffset) then
    begin
      if FetchExitCodePublic then
        FLastExitCodeOfExe := localExitCode
      else
        FLastPrivateExitCode := localExitcode;
      ps := 'Error: ' + Report;
      if FatalOnFail then
      begin
        LogDatei.log(ps, LLcritical);
        FExtremeErrorLevel := LevelFatal;
        scriptstopped := True;
      end
      else
      begin
        LogDatei.log(ps, LLError);
      end;
    end
    else
    begin
      if FetchExitCodePublic then
        FLastExitCodeOfExe := localExitCode
      else
        FLastPrivateExitCode := localExitcode;
      LogDatei.log('output:', LLDebug + logleveloffset);
      LogDatei.log('--------------', LLDebug + logleveloffset);
      for i := 0 to output.Count - 1 do
      begin
        LogDatei.log(output.strings[i], LLDebug + logleveloffset);
      end;
      //LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 6;
      LogDatei.log('', LLDebug + logleveloffset);
      Result.Text := output.Text;
    end;
    output.Free;
  finally
    {$IFDEF GUI}
    FBatchOberflaeche.showAcitvityBar(False);
    {$ENDIF GUI}
  end;
end;


function TuibInstScript.execDOSBatch(const Sektion: TWorkSection;
  BatchParameter: string; ShowCmd: integer; catchOut: boolean;
  logleveloffset: integer; WaitConditions: TSetWaitConditions;
  var output: TXStringList): TSectionResult;

var
  SaveStayOnTop: boolean;
  commandline: string = '';
  filename: string = '';
  parameters: string = '';
  report: string = '';
  errorinfo: string = '';
  i: integer = 0;
  force64, dummybool: boolean;
  winstparam: string = '';
  oldDisableWow64FsRedirectionStatus: pointer = nil;
  tempfilename: string = '';
  runas: TRunAs;
  showoutput: boolean = False;
  remainingstr, evaluatedstr, newbatchparastr, errorstr: string;
  aktos: TuibOSVersion;
  cmdMuiFiles: TStringList;
  muisrcpath, muitargetpath: string;

begin
  try
    {$IFDEF GUI}
    SaveStayOnTop := BatchScreenOnTop;
    if BatchScreenOnTop then
      FBatchOberflaeche.ForceStayOnTop(False);
    if AutoActivityDisplay then
      FBatchOberflaeche.showAcitvityBar(True);
    FBatchOberflaeche.TimerProcessMess.Enabled := True;
    {$ENDIF GUI}

    Result := tsrPositive;

    if Sektion.Count = 0 then
      exit;

    LogDatei.LogSIndentLevel := Sektion.NestingLevel;
    OldNumberOfErrors := LogDatei.NumberOfErrors;
    OldNumberOfWarnings := LogDatei.NumberOfWarnings;

    (*
    ps := '';
    LogDatei.log (ps, LLNotice);
    ps := 'Execution of: '+Sektion.Name + ' ' +BatchParameter;
    LogDatei.log (ps, LLNotice);
    *)

    (*
    if pos (uppercase (PStatNames^ [tsDOSInAnIcon]), uppercase (Sektion.Name)) > 0 then
       ps := Sektion.Name;
    if pos (uppercase (PStatNames^ [tsDOSBatchFile]), uppercase (Sektion.Name)) > 0 then
       ps := Sektion.Name;
    if pos (uppercase (PStatNames^ [tsShellInAnIcon]), uppercase (Sektion.Name)) > 0 then
       ps := Sektion.Name;
    if pos (uppercase (PStatNames^ [tsShellBatchFile]), uppercase (Sektion.Name)) > 0 then
       ps := Sektion.Name;
     *)

    {$IFDEF GUI}
    CentralForm.Label2.Caption := ps;
    FBatchOberflaeche.setDetailLabel(ps);
    ProcessMess;
    {$ENDIF GUI}

    LogDatei.LogSIndentLevel := Sektion.NestingLevel + 1;

    Sektion.eliminateLinesStartingWith(';', False);

    //Sektion.SaveToFile (TempPath + TempBatchdatei);
    //inc(TempBatchDatei_UniqueCount);
    //tempfilename := TempPath + TempBatchfilename + inttoStr(TempBatchDatei_UniqueCount) + '.bat';
    tempfilename := winstGetTempFileName;
    //Sektion.SaveToFile (tempfilename);
    if not Sektion.FuncSaveToFile(tempfilename) then
    begin
      LogDatei.log('Error: Sektion could not be saved - so we switch to failed',
        LLcritical);
      FExtremeErrorLevel := LevelFatal;
    end
    else
    begin
    {$IFDEF UNIX}
      fpchmod(tempfilename, &700);
    {$ENDIF LINUX}
      LogDatei.log('Content of saved file: ' + tempfilename, LLDebug2);
      LogDatei.log('-----------------------', LLDebug2);
      for i := 0 to Sektion.Count - 1 do
        LogDatei.log(Sektion.Strings[i], LLDebug2);
      LogDatei.log('-----------------------', LLDebug2);
      if pos('winst ', lowercase(BatchParameter)) > 0 then
      begin
        winstparam := trim(copy(BatchParameter, pos('winst ',
          lowercase(BatchParameter)) + 5, length(BatchParameter)));
        BatchParameter := trim(copy(BatchParameter, 0,
          pos('winst ', lowercase(BatchParameter)) - 1));
      end;
      force64 := False;
      runAs := traInvoker;

      if (pos(lowercase('/64bit'), lowercase(winstparam)) > 0) and Is64BitSystem then
        force64 := True;

      if (pos(lowercase('/sysnative'), lowercase(winstparam)) > 0) and Is64BitSystem then
        force64 := True;

      if (pos(lowercase('/32bit'), lowercase(winstparam)) > 0) then
        force64 := False;

      if (pos(lowercase('/showoutput'), lowercase(winstparam)) > 0) then
      begin
        showoutput := True;
        LogDatei.log('Set Showoutput true', LLDebug3);
      end;

      if (pos(lowercase(ParameterRunAsLoggedOnUser), lowercase(winstparam)) > 0) then
      begin
        if runLoginScripts then
          runAs := traLoggedOnUser
        else
          LogDatei.log('Warning: Not in UserLoginScript mode: /RunAsLoggedinUser ignored',
            LLWarning);
      end;

    {$IFDEF WIN32}
      if force64 then
        if not cmd64checked then
        begin
          if (not FileExists(GetWinDirectory + '\cmd64.exe')) or
            (FindAllFiles('c:\windows', 'cmd64.exe.mui', True).Count = 0) then
          begin
            Logdatei.log(GetWinDirectory + '\cmd64.exe not found - try to get it',
              LLDebug2);
            try
              if DSiDisableWow64FsRedirection(oldDisableWow64FsRedirectionStatus) then
              begin
                LogDatei.log('DisableWow64FsRedirection succeeded', LLinfo);
                if FileExists(GetWinSystemDirectory + '\cmd.exe') then
                begin
                  if fileutil.CopyFile(GetWinSystemDirectory + '\cmd.exe',
                    GetWinDirectory + '\cmd64.exe') then
                  begin
                    LogDatei.log('cmd64.exe created in ' + GetWinDirectory,
                      LLinfo + logleveloffset);
                    cmdMuiFiles := TStringList.Create;
                    cmdMuiFiles :=
                      FindAllFiles('c:\windows\system32', 'cmd.exe.mui', True);
                    for i := 0 to cmdMuiFiles.Count - 1 do
                    begin
                      LogDatei.log('cmd.exe.mui found in ' + cmdMuiFiles.Strings[i],
                        LLinfo + logleveloffset);
                      muisrcpath := ExtractFileDir(cmdMuiFiles.Strings[i]);
                      muitargetpath := ReplaceStr(muisrcpath, 'system32\', '');
                      if fileutil.CopyFile(muisrcpath + '\cmd.exe.mui',
                        muitargetpath + '\cmd64.exe.mui') then
                        LogDatei.log('created : ' + muitargetpath + '\cmd64.exe.mui',
                          LLinfo + logleveloffset);
                    end;
                    cmdMuiFiles.Free;
                  end
                  else
                    LogDatei.log('could not get cmd64.exe', LLError);
                end
                else
                  LogDatei.log('could see: ' + GetWinSystemDirectory +
                    '\cmd.exe', LLError);
                dummybool := DSiRevertWow64FsRedirection(
                  oldDisableWow64FsRedirectionStatus);
                LogDatei.log('RevertWow64FsRedirection succeeded', LLinfo);
              end
              else
              begin
                LogDatei.log('Error: DisableWow64FsRedirection failed', LLError);
              end;
            except
              on ex: Exception do
              begin
                LogDatei.log('Error: ' + ex.message, LLError);
              end;
            end;
          end;
          if FileExists(GetWinDirectory + '\cmd64.exe') then
            cmd64checked := True
          else
            LogDatei.log('no cmd64.exe - will use cmd.exe instead', LLError);
        end;
    {$ENDIF WIN32}

      if BatchParameter <> '' then
      begin
        ApplyTextVariablesToString(BatchParameter, False);
      end;

      if GetUibOsType(errorinfo) = tovLinux then
      begin
       {$IFDEF GUI}
        if ShowCmd = SW_HIDE then
        begin
          FileName := '/bin/bash';
          Parameters := Parameters + ' ' + tempfilename + ' ' + BatchParameter;
        end
        else
        begin
          FileName := '/usr/bin/xterm';
          Parameters := Parameters +
            ' -e script /tmp/opsi-script-out.txt -c "/bin/bash ' +
            tempfilename + ' ' + BatchParameter + '"';
        end;
       {$ELSE GUI}
        FileName := '/bin/bash';
        Parameters := Parameters + ' ' + tempfilename + ' ' + BatchParameter;
       {$ENDIF GUI}
      end
      else if (GetUibOsType(errorinfo) = tovMacOS) then
      begin
       {$IFDEF GUI}
        if ShowCmd = SW_HIDE then
        begin
          FileName := '/bin/bash';
          Parameters := Parameters + ' ' + tempfilename + ' ' + BatchParameter;
        end
        else
        begin
          FileName := '/usr/bin/open';
          Parameters := Parameters + ' -a Terminal.app  ' +
            tempfilename + ' ' + BatchParameter + '"';
        end;
       {$ELSE GUI}
        FileName := '/bin/bash';
        Parameters := Parameters + ' ' + tempfilename + ' ' + BatchParameter;
       {$ENDIF GUI}
      end
      else if GetUibOsType(errorinfo) = tovWinNT then
      begin
       {$IFDEF WINDOWS}
        if force64 and FileExists(GetWinDirectory + '\cmd64.exe') then
          FileName := '"' + GetWinDirectory + '\cmd64.exe"'
        else
          FileName := '"cmd.exe"';
        // Quote tempfile only if contains spaces, only if not quoted parameters may be quoted
        if 0 = pos(' ', tempfilename) then
          Parameters := ' /C ' + tempfilename + ' ' + BatchParameter
        else
          Parameters := ' /C "' + tempfilename + '" ' + BatchParameter;
       {$ENDIF WINDOWS}
      end;

      commandline := FileName + ' ' + trim(Parameters);

      begin
        output := TXStringlist.Create;
        LogDatei.log('Executing ' + commandline, LLDebug + logleveloffset);
        if not RunCommandAndCaptureOut(commandline, catchout,
          output, report, showcmd, FLastExitCodeOfExe, showoutput) then
        begin
          // is failed
          ps := 'Error: ' + IntToStr(FLastExitCodeOfExe) + ' : ' + Report;
          // retry on error 19
          if FLastExitCodeOfExe = 19 then
          begin
            ps := 'We got a Write protect error (19) - we retry';
            LogDatei.log(ps, LLError);
            ProcessMess;
            Sleep(100);
            if not RunCommandAndCaptureOut(commandline, catchout,
              output, report, showcmd, FLastExitCodeOfExe, showoutput) then
            begin
              LogDatei.log(ps, LLcritical);
              FExtremeErrorLevel := LevelFatal;
              scriptstopped := True;
            end;
            // is failed
            ps := 'Error: ' + IntToStr(FLastExitCodeOfExe) + ' : ' + Report;
          end
          else
          begin
            LogDatei.log(ps, LLcritical);
            FExtremeErrorLevel := LevelFatal;
            scriptstopped := True;
          end;
        end

        else if catchout or (filename = '/usr/bin/xterm') then
        begin
          if filename = '/usr/bin/xterm' then
          begin
            output.LoadFromFile('/tmp/opsi-script-out.txt');
            SysUtils.DeleteFile('/tmp/opsi-script-out.txt');
          end;
          //xoutput.AddStrings(output);
          LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 4;
          LogDatei.log('', LLDebug + logleveloffset);
          LogDatei.log('output:', LLDebug + logleveloffset);
          LogDatei.log('--------------', LLDebug + logleveloffset);
          for i := 0 to output.Count - 1 do
          begin
            LogDatei.log(output.strings[i], LLDebug + logleveloffset);
          end;
          LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 4;
          LogDatei.log('', LLDebug + logleveloffset);
        end;
      end;
    end;


    finishSection(Sektion, OldNumberOfErrors, OldNumberOfWarnings,
      DiffNumberOfErrors, DiffNumberOfWarnings);

    if ExitOnError and (DiffNumberOfErrors > 0) then
      Result := tsrExitProcess;
    {$IFDEF GUI}
    if SaveStayOnTop then
      FBatchOberflaeche.ForceStayOnTop(True);
    {$ENDIF GUI}
    if Logdatei.UsedLogLevel < LLconfidential then
      deleteTempBatFiles(tempfilename);
  finally
    {$IFDEF GUI}
    FBatchOberflaeche.showAcitvityBar(False);
    {$ENDIF GUI}
  end;
end;


function TuibInstScript.execPython(const Sektion: TWorkSection;
  PythonParameter: string; catchOut: boolean; logleveloffset: integer;
  WaitConditions: TSetWaitConditions; var output: TXStringList): TSectionResult;

var
  ///commandline, filename, parameters,
  report: string = '';
  ///errorinfo: String;
  i: integer = 0;
  showcmd: integer = 0;
  outlines: TXStringList;
  pythonVersionInfo: TXStringList;
  versionTestResult: string = '';
  savelogsindentlevel: integer = 0;
  fatalError: boolean;
  tempfilename: string = '';


  function pythonCall(const parameters: string; var outlines: TXStringList;
    showcmd: integer): boolean;

  begin
    Result := True;
    LogDatei.log('python ' + parameters, LLInfo + logleveloffset);

    if not RunCommandAndCaptureOut('python ' + parameters, True,
      outlines, report, showcmd, FLastExitCodeOfExe) then
    begin
      ps := 'Error: ' + Report;
      LogDatei.log(ps, LLcritical);
      FExtremeErrorLevel := LevelFatal;
      scriptstopped := True;
    end;
  end;

begin

  Result := tsrPositive;
  showcmd := SW_SHOWMINIMIZED; // SW_SHOWNORMAL;
  fatalError := False;

  if Sektion.Count = 0 then
    exit;

  LogDatei.LogSIndentLevel := Sektion.NestingLevel;
  savelogsindentlevel := LogDatei.LogSIndentLevel;

  OldNumberOfErrors := LogDatei.NumberOfErrors;
  OldNumberOfWarnings := LogDatei.NumberOfWarnings;

  ps := '';
  LogDatei.log(ps, LLNotice);
  ps := 'Execution of ' + Sektion.Name + ' ' + PythonParameter;
  LogDatei.log(ps, LLNotice);

  if pos(uppercase(PStatNames^ [tsExecutePython]), uppercase(Sektion.Name)) > 0 then
    ps := Sektion.Name;
  //ps := (* 'Ausfuehrung von ' +  *) copy (Sektion.Name, length (PStatNames^ [tsExecutePython]) + 1, length (Sektion.Name));

  {$IFDEF GUI}
  CentralForm.Label2.Caption := ps;
  FBatchOberflaeche.setDetailLabel(ps);
  ProcessMess;
  {$ENDIF GUI}

  LogDatei.LogSIndentLevel := Sektion.NestingLevel + 1;

  Sektion.eliminateLinesStartingWith(';', False);

  outlines := TXStringList.Create;

  if pythonCall('--version', outlines, showcmd) then
  begin
    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 6;
    if outlines.Count > 0 then
      versionTestResult := outlines.Strings[0];

    for i := 0 to outlines.Count - 1 do
    begin
      LogDatei.log('::: ' + outlines.strings[i], LevelComplete);
    end;

    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 6;
    LogDatei.log('', LLInfo);
  end;

  outlines.Clear;


  pythonVersionInfo := TXStringList.Create;
  stringsplitByWhiteSpace(versionTestResult, TStringList(pythonVersionInfo));

  if (pythonVersionInfo.Count = 0) or (lowerCase(pythonVersionInfo.Strings[0]) <>
    'python') then
  begin
    LogDatei.log('Error: python command not found', LLCritical);
    fatalError := True;
  end;

  if not fatalError then
  begin
    //Sektion.SaveToFile (TempPath + TempBatchdatei);
    //inc(TempBatchDatei_UniqueCount);
    //tempfilename := TempPath + TempBatchfilename + inttoStr(TempBatchDatei_UniqueCount) + '.bat';
    //Sektion.SaveToFile (tempfilename);
    tempfilename := winstGetTempFileName;
    if not Sektion.FuncSaveToFile(tempfilename) then
    begin
      LogDatei.log('Error: Sektion could not be saved - so we switch to failed',
        LLcritical);
      FExtremeErrorLevel := LevelFatal;
    end
    else
    begin
      output := TXStringList.Create;
      if pythonCall(tempfilename +
        (* '--winstvars ' + [loglevel:loglevel, url:serviceurl, user, client, password] *)
        ' ' + pythonparameter, output, showcmd) then
      begin
        LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 4;
        LogDatei.log('', LLDebug + logleveloffset);
        LogDatei.log('output:', LLDebug + logleveloffset);
        LogDatei.log('--------------', LLDebug + logleveloffset);

        for i := 0 to output.Count - 1 do
        begin
          LogDatei.log(output.strings[i], LLDebug + logleveloffset);
        end;

        LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 4;
        LogDatei.log('', LLDebug + logleveloffset);
      end;
    end;

  end;

  finishSection(Sektion, OldNumberOfErrors, OldNumberOfWarnings,
    DiffNumberOfErrors, DiffNumberOfWarnings);


  LogDatei.LogSIndentLevel := savelogsindentlevel;

  if ExitOnError and (DiffNumberOfErrors > 0) then
    Result := tsrExitProcess;
  if Logdatei.UsedLogLevel < LLconfidential then
    deleteTempBatFiles(tempfilename);
end;


function TuibInstScript.produceExecLine(const s: string;
  var programfilename, programparas, passparas, winstoption: string;
  var InfoSyntaxError: string): boolean;

var
  parts: TXStringList;
  remaining: string = '';
  r: string = '';
  part: string = '';
  partA: string = '';
  partB: string = '';
  continue: boolean;

begin

  Result := False;
  programfilename := '';
  programparas := '';
  passparas := '';
  winstoption := '';

  remaining := s;
  parts := TXStringList.Create;

  //determine part1 until PASS
  continue := True;
  partA := '';

  while (remaining <> '') and continue do
  begin
    GetWord(remaining, part, remaining, WordDelimiterWhiteSpace);
    if (lowercase(part) = lowercase(passSplitter)) or
      (lowercase(part) = lowercase(optionsSplitter)) then
      continue := False
    else
      partA := partA + ' ' + part;
  end;

  parts.add(trim(partA));


  //determine parts[1] until WINST
  continue := True;
  partB := '';

  if lowercase(part) <> lowercase(optionsSplitter) then
    while (remaining <> '') and continue do
    begin
      GetWord(remaining, part, remaining, WordDelimiterWhiteSpace);
      if lowercase(part) = lowercase(optionsSplitter) then
        continue := False
      else
        partB := partB + ' ' + part;
    end;

  parts.add(trim(partB));

  parts.add(trim(remaining));

  if (parts[2] <> '') then
  begin
    if not GetString(parts[2], remaining, r, InfoSyntaxError, True) then
      remaining := parts[2];

    continue := True;

    while (remaining <> '') and continue do
    begin
      if not Skip(ParameterDontWait, remaining, remaining, InfoSyntaxError) and
        not Skip(ParameterEscapeStrings, remaining, remaining, InfoSyntaxError) and
        not Skip(Parameter_64Bit, remaining, remaining, InfoSyntaxError) and
        not Skip(Parameter_32Bit, remaining, remaining, InfoSyntaxError) and
        not Skip(Parameter_SysNative, remaining, remaining, InfoSyntaxError) then
        continue := False;
    end;


    if (remaining <> '') and not continue then
    begin
      InfoSyntaxError := 'winst option not recognized';
      exit;
    end;

  end;

  if parts[0] = '' then
  begin
    InfoSyntaxError := 'Program file name missing';
    exit;
  end;

  Result := True;

  if not GetString(parts[0], programFileName, remaining, infosyntaxError, True) then
    GetWord(parts[0], programFileName, remaining, WordDelimiterWhiteSpace);

  if not EvaluateString(remaining, remaining, programParas, infoSyntaxError) then
    programParas := remaining;

  if not EvaluateString(parts[1], remaining, passParas, infoSyntaxError) then
    passParas := parts[1];

  winstOption := parts[2];

  parts.Free;
end;

function TuibInstScript.executeWith(const Sektion: TWorkSection;
  ExecParameter: string; catchOut: boolean; logleveloffset: integer;
  var output: TXStringList): TSectionResult;

var
  commandline: string = '';
  programfilename: string = '';
  programparas: string = '';
  passparas: string = '';
  winstoption: string = '';
  report: string = '';
  threaded: boolean;
  errorinfo: string = '';
  i: integer = 0;
  showcmd: integer = 0;
  waitSecs: integer = 0;
  tempfilename: string = '';
  savelogsindentlevel: integer = 0;
  runAs: TRunAs;
  useext: string;
  force64: boolean;
  oldDisableWow64FsRedirectionStatus: pointer = nil;
  Wow64FsRedirectionDisabled, boolresult: boolean;

begin
  try
    (*
    if  GetNTVersionMajor < 6 then
      runAs := traInvoker
    else
      runAs := traInvoker;
    *)
    runAs := traInvoker;
    Result := tsrPositive;
    showcmd := SW_SHOWMINIMIZED; // SW_SHOWNORMAL;
    waitSecs := 0;

    if Sektion.Count = 0 then
      exit;

    LogDatei.LogSIndentLevel := Sektion.NestingLevel;
    savelogsindentlevel := LogDatei.LogSIndentLevel;

    OldNumberOfErrors := LogDatei.NumberOfErrors;
    OldNumberOfWarnings := LogDatei.NumberOfWarnings;

    ps := '';
    LogDatei.log(ps, LLNotice + logleveloffset);
    ps := 'Execution of ' + Sektion.Name + ' ' + ExecParameter;
    LogDatei.log(ps, LLNotice + logleveloffset);

    if pos(uppercase(PStatNames^ [tsExecuteWith]), uppercase(Sektion.Name)) > 0 then
      ps := Sektion.Name;
    //ps := (* 'Ausfuehrung von ' +  *) copy (Sektion.Name, length (PStatNames^ [tsExecuteWith]) + 1, length (Sektion.Name));

    {$IFDEF GUI}
    CentralForm.Label2.Caption := ps;
    FBatchOberflaeche.setDetailLabel(ps);
    ProcessMess;
    {$ENDIF GUI}

    LogDatei.LogSIndentLevel := Sektion.NestingLevel + 1;

    Sektion.eliminateLinesStartingWith(';', False);

    output := TXStringList.Create;

    //inc(TempBatchDatei_UniqueCount);
    //tempfilename := TempPath + TempBatchfilename + inttoStr(TempBatchDatei_UniqueCount) + '.bat';
    //Sektion.SaveToFile (tempfilename);

    if ExecParameter <> '' then
    begin
      ApplyTextVariablesToString(ExecParameter, False);
    end;
    // syntax should been checked
    if not produceExecLine(ExecParameter, programfilename, programparas,
      passparas, winstoption, errorInfo) then
    begin
      LogDatei.log('Error: Illegal Parameter Syntax  - so we switch to failed',
        LLcritical);
      FExtremeErrorLevel := LevelFatal;
      exit;
    end;

    useext := '.cmd';
    if pos('powershell.exe', LowerCase(programfilename)) > 0 then
      useext := '.ps1';
    if LowerCase(programfilename) = 'powershell' then
      useext := '.ps1';
    tempfilename := winstGetTempFileNameWithExt(useext);

    if not Sektion.FuncSaveToFile(tempfilename) then
    begin
      LogDatei.log('Error: Sektion could not be saved - so we switch to failed',
        LLcritical);
      FExtremeErrorLevel := LevelFatal;
    end
    else
    begin
      {$IFDEF UNIX}
      fpchmod(tempfilename, &700);
      {$ENDIF LINUX}
      LogDatei.log('Content of saved file: ' + tempfilename, LLDebug2);
      LogDatei.log('-----------------------', LLDebug2);
      for i := 0 to Sektion.Count - 1 do
        LogDatei.log(Sektion.Strings[i], LLDebug);
      LogDatei.log('-----------------------', LLDebug2);
      // if parameters end with '=' we concat tempfile without space
      if copy(programparas, length(programparas), 1) = '=' then
        commandline :=
          '"' + programfilename + '" ' + programparas + '"' +
          tempfilename + '"  ' + passparas
      else
        commandline :=
          '"' + programfilename + '" ' + programparas + ' "' +
          tempfilename + '"  ' + passparas;

      force64 := False;
      if (pos(lowercase('/64bit'), lowercase(winstoption)) > 0) and Is64BitSystem then
        force64 := True;

      if (pos(lowercase('/sysnative'), lowercase(winstoption)) > 0) and
        Is64BitSystem then
        force64 := True;

      if (pos(lowercase('/32bit'), lowercase(winstoption)) > 0) then
        force64 := False;

      {$IFDEF WIN32}
      Wow64FsRedirectionDisabled := False;
      if force64 then
      begin
        boolresult := DSiDisableWow64FsRedirection(oldDisableWow64FsRedirectionStatus);
        Wow64FsRedirectionDisabled := True;
      end
      else
        Wow64FsRedirectionDisabled := False;
      {$ENDIF WIN32}
      {$IFDEF GUI}
      if AutoActivityDisplay then
        FBatchOberflaeche.showAcitvityBar(True);
      {$ENDIF GUI}


      threaded :=
        (pos(lowercase(ParameterDontWait), lowercase(winstoption)) > 0);

      if threaded then
      begin
        if not StartProcess(Commandline, sw_hide, False, False,
          False, False, runas, '', WaitSecs, Report, FLastExitCodeOfExe) then
        begin
          ps := 'Error: ' + Report;
          LogDatei.log(ps, LLcritical);
          FExtremeErrorLevel := LevelFatal;
        end
        else
          LogDatei.log(Report, LevelComplete);
      end
      else
      begin
        LogDatei.log_prog('Executing ' + commandline, LLDebug);
        if not RunCommandAndCaptureOut(commandline, True, output,
          report, showcmd, FLastExitCodeOfExe) then
        begin
          ps := 'Error: ' + Report;
          LogDatei.log(ps, LLcritical);
          FExtremeErrorLevel := LevelFatal;
          scriptstopped := True;
        end
        else
        begin
          LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 4;
          LogDatei.log('', LLDebug + logleveloffset);
          LogDatei.log('output:', LLDebug + logleveloffset);
          LogDatei.log('--------------', LLDebug + logleveloffset);

          for i := 0 to output.Count - 1 do
          begin
            LogDatei.log(output.strings[i], LLDebug + logleveloffset);
          end;

          LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 4;
          LogDatei.log('', LLDebug + logleveloffset);
        end;
      end;

      {$IFDEF WIN32}
      if Wow64FsRedirectionDisabled then
      begin
        boolresult := DSiRevertWow64FsRedirection(oldDisableWow64FsRedirectionStatus);
        Wow64FsRedirectionDisabled := False;
      end;
      {$ENDIF WIN32}
    end;
    finishSection(Sektion, OldNumberOfErrors, OldNumberOfWarnings,
      DiffNumberOfErrors, DiffNumberOfWarnings);


    LogDatei.LogSIndentLevel := savelogsindentlevel;

    if ExitOnError and (DiffNumberOfErrors > 0) then
      Result := tsrExitProcess;
    if Logdatei.UsedLogLevel < LLconfidential then
      if not threaded then
        deleteTempBatFiles(tempfilename);
  finally
    {$IFDEF GUI}
    FBatchOberflaeche.showAcitvityBar(False);
    {$ENDIF GUI}
  end;
end;

function TuibInstScript.produceStringList
  (const section: TuibIniScript; const s0: string; var Remaining: string;
  var list: TXStringList; var InfoSyntaxError: string): boolean;
  //var
  // NestLevel : integer;
begin
  Result := produceStringList(section, s0, Remaining, list, InfoSyntaxError,
    IfElseEndifLevel, inDefFuncIndex);
end;

function TuibInstScript.produceStringList
  (const section: TuibIniScript; const s0: string; var Remaining: string;
  var list: TXStringList; var InfoSyntaxError: string; var NestLevel: integer;
  const inDefFuncIndex: integer): boolean;

var
  VarIndex: integer = 0;
  i: integer = 0;
  k: integer = 0;
  startlineofsection: integer = 0;
  syntaxCheck: boolean;
  localsyntaxcheck: boolean;
  goOn: boolean;
  a2_to_default: boolean;
  s: string = '';
  r: string = '';
  s1: string = '';
  s2: string = '';
  s3: string = '';
  s4: string = '';
  r1: string = '';
  ///sx,
  tmpstr, tmpstr1, tmpstr2, tmpstr3: string;
  tmpint: integer;
  tmpbool, tmpbool1: boolean;
  a1: integer = 0;
  a2: integer = 0;
  int64_1: int64;
  int64_2: int64;
  list1: TXStringList;
  list2: TXStringList;
  list3: TXStringList;
  slist: TStringList;
  inifile: TuibIniScript;
  localSection: TWorkSection;
  secSpec: TSectionSpecifier;
  localKindOfStatement: TStatement;
  logstring: string = '';
  savelogsindentlevel: integer = 0;
  {$IFDEF WINDOWS}
  versionInfo: TVersionInfo; //from versionInfoX
  {$ENDIF WINDOWS}
  // is needed with $OBJECTCHECKS ON
  dummyActionresult: TSectionResult = 0;
  oldDisableWow64FsRedirectionStatus: pointer = nil;
  dummybool: boolean;
  Wow64FsRedirectionDisabled, boolresult: boolean;
  funcname: string;
  funcindex, funcindexvar: integer;
  ErrorMsg: string;
  keyValueSeparator: char = '=';

begin

  syntaxcheck := False;

  savelogsindentlevel := LogDatei.LogSIndentLevel;

  if Skip('(', s0, s1, InfoSyntaxError) then
  begin
    if produceStringList(section, s1, r, list, InfoSyntaxError) //Recursion
      and Skip(')', r, r, InfoSyntaxError) then
    begin
      syntaxCheck := True;
    end;
  end

  else
  begin
    // defined local function ?
    GetWord(s0, funcname, r, WordDelimiterSet5);
    FuncIndex := definedFunctionNames.IndexOf(LowerCase(funcname));
    GetWord(s0, s, r, WordDelimiterSet1);  // getting word s
    list := TXStringList.Create; //list to return
    slist := TStringList.Create;  // if we need a real TStringlist
    VarIndex := listOfStringLists.IndexOf(LowerCase(s));
    logstring := s;

    // local variable
    if isVisibleLocalVar(s, funcindexvar) then
    begin
      if not (definedFunctionArray[funcindexvar].getLocalVarDatatype(s) =
        dfpStringlist) then
      begin
        // type error
        InfoSyntaxError :=
          'Syntax Error: Type mismatch: Stringlist expected but the visible local variable: '
          + s + ' is from type: ' +
          osdfParameterTypesNames[definedFunctionArray[
          funcindexvar].getLocalVarDatatype(s)];
      end
      else
      begin
        list.Text := definedFunctionArray[funcindexvar].getLocalVarValueList(s).Text;
        syntaxCheck := True;
      end;
    end

    // global var : clone of an existing list?
    else if VarIndex >= 0 then
    begin
      list.Assign(TStringList(contentOfStringLists[VarIndex]));
      syntaxCheck := True;
    end

    // defined functions
    else if FuncIndex >= 0 then
    begin
      if not (definedFunctionArray[FuncIndex].datatype = dfpStringlist) then
      begin
        // error
        syntaxCheck := False;
        LogDatei.log('Syntax Error: defined function: ' + funcname +
          ' is not from type stringlist.', LLError);
      end
      else
      begin
        if definedFunctionArray[FuncIndex].call(r, r, NestLevel) then
        begin
          //r := ''; we may have closing brackets here
          list.Text := definedFunctionArray[FuncIndex].ResultList.Text;
          syntaxCheck := True;
          //logdatei.log('We leave the defined function: inDefFunc3: '+IntToStr(inDefFunc3),LLInfo);
        end
        else
        begin
          // defined function call failed
          LogDatei.log('Call of defined function: ' + funcname + ' failed', LLError);
          syntaxCheck := False;
        end;
      end;
    end

    else if LowerCase(s) = LowerCase('shellcall') then
    begin
      if Skip('(', r, r, InfoSyntaxError) then
        if EvaluateString(r, r, s1, InfoSyntaxError) then
          if Skip(')', r, r, InfoSyntaxError) then
          begin
            syntaxCheck := True;
            try
              //LogDatei.log ('Executing0 ' + s1, LLInfo);
              list.Text := execShellCall(s1, 'sysnative', 1, True).Text;
            except
              on e: Exception do
              begin
                LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
                LogDatei.log('Error executing :' + s1 + ' : ' + e.message,
                  LLError);
                FNumberOfErrors := FNumberOfErrors + 1;
                LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
              end
            end;
          end;
    end

    else if LowerCase(s) = LowerCase('powershellcall') then
    begin
      {$IFDEF UNIX}
      LogDatei.log('Error powershellcall not implemented on Linux ', LLError);
      {$ENDIF Linux}
      {$IFDEF WINDOWS}
      s2 := '';
      s3 := '';
      tmpstr2 := '';
      tmpbool := True; // sysnative
      tmpbool1 := True; // handle execution policy
      syntaxCheck := False;
      if Skip('(', r, r, InfoSyntaxError) then
        if EvaluateString(r, tmpstr, s1, InfoSyntaxError)
        // next after ',' or ')'
        then
          if Skip(',', tmpstr, tmpstr1, tmpstr3) then
            if EvaluateString(tmpstr1, tmpstr2, s2, tmpstr3) then;
      if s2 = '' then
      begin
        // only one parameter
        if Skip(')', tmpstr, r, InfoSyntaxError) then
        begin
          syntaxCheck := True;
          s2 := 'sysnative';
        end;
      end
      else
      begin
        // got second parameter
        tmpbool := True;
        if lowercase(s2) = '32bit' then
          tmpbool := False
        else if lowercase(s2) = '64bit' then
          tmpbool := True
        else if lowercase(s2) = 'sysnative' then
          tmpbool := True
        else
        begin
          InfoSyntaxError := 'Error: unknown parameter: ' + s2 +
            ' expected one of 32bit,64bit,sysnative - fall back to sysnative';
          syntaxCheck := False;
        end;
        // three parameter ?
        if Skip(',', tmpstr2, tmpstr1, tmpstr3) then
        begin
          if EvaluateString(tmpstr1, tmpstr2, s3, tmpstr3) then
          begin
            // got third parameter
            if Skip(')', tmpstr2, r, InfoSyntaxError) then
            begin
              if TryStrToBool(s3, tmpbool1) then
                syntaxCheck := True
              else
              begin
                syntaxCheck := False;
                InfoSyntaxError :=
                  'Error: boolean string (true/false) expected but got: ' + s3;
              end;
            end;
          end;
        end
        else
        if Skip(')', tmpstr2, r, InfoSyntaxError) then
        begin
          // two parameter
          syntaxCheck := True;
        end;
      end;
      if syntaxCheck then
      begin
        try
          list.Text := execPowershellCall(s1, s2, 1, True, False, tmpbool1).Text;
        except
          on e: Exception do
          begin
            LogDatei.log('Error executing :' + s1 + ' : with powershell: ' + e.message,
              LLError);
          end
        end;
      end;
      {$ENDIF WINDOWS}
    end


    else if LowerCase(s) = LowerCase('LoadTextFile') then
    begin
      if Skip('(', r, r, InfoSyntaxError) then
        if EvaluateString(r, r, s1, InfoSyntaxError) then
        begin
          try
            s1 := ExpandFileName(s1);
            list.loadfromfile(s1);
            list.Text := reencode(list.Text, 'system');
          except
            on e: Exception do
            begin
              LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
              LogDatei.log('Error on loading file: ' + e.message, LLError);
              FNumberOfErrors := FNumberOfErrors + 1;
              LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
            end
          end;
          if Skip(')', r, r, InfoSyntaxError) then
          begin
            syntaxCheck := True;
          end;
        end;
    end

    else if LowerCase(s) = LowerCase('LoadTextFileWithEncoding') then
    begin
      if Skip('(', r, r, InfoSyntaxError) then
        if EvaluateString(r, r, s1, InfoSyntaxError) then
          if Skip(',', r, r, InfoSyntaxError) then
            if EvaluateString(r, r, s2, InfoSyntaxError) then
              if Skip(')', r, r, InfoSyntaxError) then
              begin
                syntaxCheck := True;
                try
                  s1 := ExpandFileName(s1);
                  //list.AddText(LoadFromFileWithEncoding(s1, s2).Text);
                  list.loadfromfile(s1);
                  list.Text := reencode(list.Text, s2);
                except
                  on e: Exception do
                  begin
                    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
                    LogDatei.log('Error on loading file: ' + e.message, LLError);
                    FNumberOfErrors := FNumberOfErrors + 1;
                    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
                  end
                end;
              end;
    end


    else if LowerCase(s) = LowerCase('LoadUnicodeTextFile') then
    begin
      if Skip('(', r, r, InfoSyntaxError) then
        if EvaluateString(r, r, s1, InfoSyntaxError) then
        begin
          try
            s1 := ExpandFileName(s1);
            //list.loadfromfile (s1);
            //list.Text:= reencode(list.Text, 'ucs2le');
            TStringList(list).Assign(stringListLoadUtf8FromFile(s1));
            //wsloadfromfile (s1, TStringList (list));
          except
            on e: Exception do
            begin
              LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
              LogDatei.log('Error on loading file: ' + e.message,
                LLError);
              FNumberOfErrors := FNumberOfErrors + 1;
              LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
            end
          end;
          if Skip(')', r, r, InfoSyntaxError) then
          begin
            syntaxCheck := True;
          end;
        end;
    end

    else if LowerCase(s) = LowerCase('GetSectionNames') then
    begin
      if Skip('(', r, r, InfoSyntaxError) and EvaluateString(r, r,
        s1, InfoSyntaxError) and Skip(')', r, r, InfoSyntaxError) then
      begin
        syntaxCheck := True;
        inifile := TuibIniScript.Create;
        try
          s1 := ExpandFileName(s1);
          inifile.loadfromfile(s1);
          inifile.Text := reencode(inifile.Text, 'system');
        except
          on e: Exception do
          begin
            LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
            LogDatei.log('Error on loading file: ' + e.message,
              LLError);
            FNumberOfErrors := FNumberOfErrors + 1;
            LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
          end
        end;

        inifile.GetSectionNames(list);
        inifile.Free;
      end;
    end


    else if LowerCase(s) = LowerCase('retrieveSection') then
    begin
      if Skip('(', r, r, InfoSyntaxError) then
        if EvaluateString(r, r, s1, InfoSyntaxError) then
        begin

          //list.assign(TXStringList(self));  // the complete script

          (*(Tuibiniscript(self)).*)
          GetSectionLines(s1, list, startlineofsection, True, True, True);

          if Skip(')', r, r, InfoSyntaxError) then
          begin
            syntaxCheck := True;
          end;
        end;
    end

    else if LowerCase(s) = LowerCase('getOutstreamFromSection') then
    begin
      if Skip('(', r, r, InfoSyntaxError) then
        if EvaluateString(r, r, s1, InfoSyntaxError) then
        begin
          savelogsindentlevel := LogDatei.LogSIndentLevel;
          localSection := TWorkSection.Create(LogDatei.LogSIndentLevel + 1,
            ActiveSection);
          GetWord(s1, s2, r1, WordDelimiterSet1);
          localSection.Name := s2;

          localKindOfStatement := findKindOfStatement(s2, SecSpec, s1);

          if not (localKindOfStatement in
            [tsDOSBatchFile, tsDOSInAnIcon, tsShellBatchFile,
            tsShellInAnIcon, tsExecutePython, tsExecuteWith,
            tsExecuteWith_escapingStrings]) then
            InfoSyntaxError := 'not implemented for this kind of section'
          else
          begin
            //if not (section.GetSectionLines (s2, TXStringList(localSection), startlineofsection, true, true, false)
            //  or GetSectionLines (s2, TXStringList(localSection), startlineofsection, true, true, false))
            if not SearchForSectionLines(self, TWorkSection(section),
              nil, s2, TXStringList(localSection), startlineofsection, True, True, False)
            then
              InfoSyntaxError := 'Section "' + s2 + '" not found'
            else
            begin
              if localKindOfStatement in [tsExecutePython, tsExecuteWith_escapingStrings]
              then
              begin
                ApplyTextConstants(TXStringList(localSection), True);
                ApplyTextVariables(TXStringList(localSection), True);
              end
              else
              begin
                ApplyTextConstants(TXStringList(localSection), False);
                ApplyTextVariables(TXStringList(localSection), False);
              end;

              case localKindOfStatement of

                tsExecutePython:
                  execPython(localSection, r1,
                    True {catchout}, 1,
                    [ttpWaitOnTerminate], list);

                tsExecuteWith_escapingStrings, tsExecuteWith:
                  executeWith(localSection, r1,
                    True {catchout}, 1, list);

                tsDOSBatchFile, tsDOSInAnIcon, tsShellBatchFile, tsShellInAnIcon:
                  execDOSBatch(localSection, r1,
                    SW_HIDE, True {catchout}, 1,
                    [ttpWaitOnTerminate], list);

              end;

              if Skip(')', r, r, InfoSyntaxError) then
              begin
                syntaxCheck := True;
              end;
            end;

          end;

          if localSection <> nil then
            localSection.Free;

          LogDatei.LogSIndentLevel := saveLogSIndentLevel;
        end;
    end

    else if LowerCase(s) = LowerCase('getReturnlistFromSection') then
    begin
      if Skip('(', r, r, InfoSyntaxError) then
        if EvaluateString(r, r, s1, InfoSyntaxError) then
        begin
          savelogsindentlevel := LogDatei.LogSIndentLevel;
          localSection := TWorkSection.Create(LogDatei.LogSIndentLevel + 1,
            ActiveSection);
          GetWord(s1, s2, r1, WordDelimiterSet1);
          localKindOfStatement := findKindOfStatement(s2, SecSpec, s1);
          if not (localKindOfStatement in [tsXMLPatch, tsXML2,
            tsOpsiServiceCall, tsLDAPsearch, tsOpsiServiceHashList]) then
            InfoSyntaxError := 'not implemented for this kind of section'
          else
          begin
            //if not (section.GetSectionLines (s2, TXStringList(localSection), startlineofsection, true, true, true)
            //  or GetSectionLines (s2, TXStringList(localSection), startlineofsection, true, true, true))
            if not SearchForSectionLines(self, TWorkSection(section),
              nil, s2, TXStringList(localSection), startlineofsection, True, True, False)
            then
              InfoSyntaxError := 'Section "' + s2 + '" not found'
            else
            begin
              localsyntaxcheck := True;
              localSection.Name := s2;
              ApplyTextVariables(TXStringList(localSection), False);
              ApplyTextConstants(TXStringList(localSection), False);

              case localKindOfStatement of
                tsXMLPatch: dummyActionresult := doxmlpatch(localSection, r1, list);
                tsXML2: dummyActionresult := doxmlpatch2(localSection, r1, list);
                tsOpsiServiceCall: dummyActionresult :=
                    doOpsiServiceCall(localSection, r1, list);
                tsOpsiServiceHashList: dummyActionresult :=
                    doOpsiServiceHashList(localSection, r1, list);
                tsLDAPsearch:
                begin
                  if produceLDAPsearchParameters(r1, cacheRequest,
                    outputRequest, InfoSyntaxError) then
                    dummyActionresult :=
                      doLDAPSearch(localSection, cacheRequest, outputRequest, list)
                  else
                    localsyntaxcheck := False;
                end;
              end;
              localSection.Free;

              if localsyntaxcheck and (Skip(')', r, r, InfoSyntaxError)) then
              begin
                syntaxCheck := True;
              end;
            end;
          end;
          LogDatei.LogSIndentLevel := saveLogSIndentLevel;
        end;
    end


    else if LowerCase(s) = LowerCase('editStringList') then
    begin
      if Skip('(', r, r, InfoSyntaxError) then
        if EvaluateString(r, r, s1, InfoSyntaxError) then
        begin
          if Skip(')', r, r, InfoSyntaxError) then
          begin
            syntaxCheck := True;
          end;
        end;
    end

    else if LowerCase(s) = LowerCase('splitStringOnWhiteSpace') then
    begin
      if Skip('(', r, r, InfoSyntaxError) then
        if EvaluateString(r, r, s1, InfoSyntaxError) then
          if Skip(')', r, r, InfoSyntaxError) then
          begin
            syntaxCheck := True;
            stringsplitByWhiteSpace(s1, TStringList(list));
            // if s1 is confidential all parts are confidential as well
            if logdatei.isConfidential(s1) then
            begin
              for i := 0 to list.Count - 1 do
              begin
                tmpstr := list.Strings[i];
                if tmpstr <> '' then
                  logdatei.AddToConfidentials(tmpstr);
              end;
            end;
          end;
    end


    else if LowerCase(s) = LowerCase('parseUrl') then
    begin
      if Skip('(', r, r, InfoSyntaxError) then
        if EvaluateString(r, r, s1, InfoSyntaxError) then
          if Skip(')', r, r, InfoSyntaxError) then
          begin
            syntaxCheck := True;
            list.Clear;
            list.AddStrings(parseUrl(s1));
          end;
    end

    else if LowerCase(s) = LowerCase('jsonAsArrayToStringList') then
    begin
      if Skip('(', r, r, InfoSyntaxError) then
        if EvaluateString(r, r, s1, InfoSyntaxError) then
          if Skip(')', r, r, InfoSyntaxError) then
          begin
            syntaxCheck := True;
            try
              list.Text := '';
              if jsonAsArrayToStringList(s1, slist) then
                list.Text := slist.Text
              else
                LogDatei.log('Error at jsonAsArrayToStringList with: "' +
                  s1 + '"', LLerror);
            except
              list.Text := '';
              LogDatei.log('Error: Exception at jsonAsArrayToStringList with: "' +
                s1 + '"', LLerror);
            end;
          end;
    end

    else if LowerCase(s) = LowerCase('jsonAsObjectGetKeyList') then
    begin
      if Skip('(', r, r, InfoSyntaxError) then
        if EvaluateString(r, r, s1, InfoSyntaxError) then
          if Skip(')', r, r, InfoSyntaxError) then
          begin
            syntaxCheck := True;
            try
              list.Text := '';
              if jsonAsObjectGetKeyList(s1, slist) then
                list.Text := slist.Text
              else
                LogDatei.log('Error at jsonAsObjectGetKeyList with: "' +
                  s1 + '"', LLerror);
            except
              list.Text := '';
              LogDatei.log('Error: Exception at jsonAsObjectGetKeyList with: "' +
                s1 + '"', LLerror);
            end;
          end;
    end


    else if LowerCase(s) = LowerCase('splitString') then
    begin
      if Skip('(', r, r, InfoSyntaxError) then
        if EvaluateString(r, r, s1, InfoSyntaxError) then
          if Skip(',', r, r, InfoSyntaxError) then
            if EvaluateString(r, r, s2, InfoSyntaxError) then
              if Skip(')', r, r, InfoSyntaxError) then
              begin
                syntaxCheck := True;

                stringsplit(s1, s2, list);
                // if s1 is confidential all parts are confidential as well
                if logdatei.isConfidential(s1) then
                begin
                  for i := 0 to list.Count - 1 do
                  begin
                    tmpstr := list.Strings[i];
                    if tmpstr <> '' then
                      logdatei.AddToConfidentials(tmpstr);
                  end;
                end;
              end;
    end

    else if LowerCase(s) = LowerCase('GetProductPropertyList') then
    begin
      if Skip('(', r, r, InfoSyntaxError) then
        if EvaluateString(r, r, s1, InfoSyntaxError) then
        begin
          if Skip(',', r, r1, InfoSyntaxError) then
          begin
            list1 := TXStringList.Create;
            // is the second argument a valid string list ?
            if not produceStringList(section, r1, r, list1, InfoSyntaxError) then
            begin
              // if not -  is it a string Expressionstr ?
              if not EvaluateString(r1, r, s2, InfoSyntaxError) then
              begin
                LogDatei.log('Error: String Expressionstr or Stringlist expected ',
                  LLerror);
                syntaxcheck := False;
              end
              else
              begin
                // if it is a string: make a string list with one element
                list1.Clear;
                list1.Add(s2);
                syntaxcheck := True;
              end;
            end
            else
              syntaxcheck := True;   // it was a string list
            if syntaxcheck then
            begin
              tmpstr := r;
              // is it the 2 argument call ?
              if Skip(')', r, r, InfoSyntaxError) then
              begin
                syntaxCheck := True;
                if opsidata <> nil then
                begin
                  list.AddStrings(opsidata.getProductPropertyList(s1, list1));
                end
                else if local_opsidata <> nil then
                begin
                  list.AddStrings(local_opsidata.getProductPropertyList(s1, list1));
                end
                else
                begin
                  LogDatei.log(
                    'No service connection in GetProductPropertyList - using default',
                    LLDebug);
                  list.Text := list1.Text;
                end;
              end
              else
              begin
                syntaxCheck := False;
                // is it the 4 argument call ?
                if Skip(',', tmpstr, r, InfoSyntaxError) then
                  if EvaluateString(r, r, s3, InfoSyntaxError) then
                    if Skip(',', r, r, InfoSyntaxError) then
                      if EvaluateString(r, r, s4, InfoSyntaxError) then
                        if Skip(')', r, r, InfoSyntaxError) then
                        begin
                          syntaxCheck := True;
                          InfoSyntaxError := '';
                          LogDatei.log('Calling with 4 arguments', LLDebug);
                          if opsidata <> nil then
                          begin
                            list.AddStrings(opsidata.getProductPropertyList(
                              s1, list1, s3, s4));
                          end
                          else if local_opsidata <> nil then
                          begin
                            list.AddStrings(
                              local_opsidata.getProductPropertyList(s1, list1, s3, s4));
                          end
                          else
                          begin
                            LogDatei.log(
                              'No service connection in GetProductPropertyList - using default',
                              LLDebug);
                            list.Text := list1.Text;
                          end;
                        end
                        else
                          syntaxCheck := False;
              end;
            end;
          end;
        end;
    end


    else if LowerCase(s) = LowerCase('getlistcontaining') then
    begin
      if Skip('(', r, r, InfoSyntaxError) then
      begin
        list1 := TXStringList.Create;
        if produceStringList(section, r, r, list1, InfoSyntaxError) //Recursion
          and skip(',', r, r, InfoSyntaxError) then
        begin
          if EvaluateString(r, r, s1, InfoSyntaxError) and
            skip(')', r, r, InfoSyntaxError) then
          begin
            syntaxcheck := True;
            list.Clear;
            for i := 0 to list1.Count - 1 do
              try
                if AnsiContainsText(list1[i], s1) then
                  list.add(list1[i]);
              except
                on e: Exception do
                begin
                  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
                  LogDatei.log('Error on producing sublist: ' + e.message,
                    LLerror);
                  FNumberOfErrors := FNumberOfErrors + 1;
                  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
                end
              end;
            list1.Free;
            list1 := nil;
          end;
        end;
      end;
    end


    else if LowerCase(s) = LowerCase('removeFromListByMatch') then
    begin
      if Skip('(', r, r1, InfoSyntaxError) then
      begin
        if EvaluateString(r1, r, s1, InfoSyntaxError) and
          skip(',', r, r, InfoSyntaxError) then
        begin
          list1 := TXStringList.Create;
          if produceStringList(section, r, r, list1, InfoSyntaxError) //Recursion
            and skip(')', r, r, InfoSyntaxError) then
          begin
            syntaxcheck := True;
            list.Clear;
            for i := 0 to list1.Count - 1 do
              try
                if not (LowerCase(list1[i]) = LowerCase(s1)) then
                  list.add(list1[i]);
              except
                on e: Exception do
                begin
                  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
                  LogDatei.log('Error on producing sublist: ' + e.message, LLerror);
                  FNumberOfErrors := FNumberOfErrors + 1;
                  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
                end
              end;
            list1.Free;
            list1 := nil;
          end;
        end;
      end;
    end


    else if LowerCase(s) = LowerCase('removeFromListByContaining') then
    begin
      if Skip('(', r, r1, InfoSyntaxError) then
      begin
        list1 := TXStringList.Create;
        // is the first argument a valid string list ?
        if not produceStringList(section, r1, r, list1, InfoSyntaxError) then
        begin
          // if not is it a string Expressionstr
          if not EvaluateString(r1, r, s1, InfoSyntaxError) then
          begin
            LogDatei.log('Error: String Expressionstr or Stringlist expected ', LLerror);
            syntaxcheck := False;
          end
          else
          begin
            // if it is a string: make a string list with one element
            list1.Clear;
            list1.Add(s1);
            syntaxcheck := True;
          end;
        end
        else
          syntaxcheck := True;   // it was a string list
        if skip(',', r, r, InfoSyntaxError) and syntaxcheck then
        begin
          list2 := TXStringList.Create;
          if produceStringList(section, r, r, list2, InfoSyntaxError) //Recursion
            and skip(')', r, r, InfoSyntaxError) then
          begin
            syntaxcheck := True;
            list.Clear;
            try
              // fill list
              for k := 0 to list2.Count - 1 do
                list.add(list2[k]);
              // remove unwanted entries
              for i := 0 to list1.Count - 1 do
                for k := 0 to list2.Count - 1 do
                  if AnsiContainsText(list2[k], list1[i]) then
                    list.Delete(list.IndexOf(list2[k]));
            except
              on e: Exception do
              begin
                LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
                LogDatei.log('Error on producing sublist: ' + e.message, LLerror);
                FNumberOfErrors := FNumberOfErrors + 1;
                LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
              end
            end;
            list1.Free;
            list1 := nil;
            list2.Free;
            list2 := nil;
          end;
        end;
      end;
    end

    else if LowerCase(s) = LowerCase('getSubListByContainingRegex') then
    begin
      if Skip('(', r, r1, InfoSyntaxError) then
      begin
        if EvaluateString(r1, r, s1, InfoSyntaxError) and
          skip(',', r, r, InfoSyntaxError) then
        begin
          list1 := TXStringList.Create;
          if produceStringList(section, r, r, list1, InfoSyntaxError) and
            skip(')', r, r, InfoSyntaxError) then
          begin
            syntaxcheck := True;
            list.Clear;
            list.AddStrings(getSubListByContainingRegex(s1, list1));
            list1.Free;
            list1 := nil;
          end;
        end
        else
        begin
          list2 := TXStringList.Create;
          if produceStringList(section, r1, r, list2, InfoSyntaxError) and
            skip(',', r, r, InfoSyntaxError) then
          begin
            list3 := TXStringList.Create;
            if produceStringList(section, r, r, list3, InfoSyntaxError) and
              skip(')', r, r, InfoSyntaxError) then
            begin
              syntaxcheck := True;
              list.Clear;
              list.AddStrings(getSubListByContainingRegex(list2, list3));
              list2.Free;
              list2 := nil;
              list3.Free;
              list3 := nil;
            end;
          end;
        end;
      end;
    end

    else if LowerCase(s) = LowerCase('getRegexMatchList') then
    begin
      if Skip('(', r, r1, InfoSyntaxError) then
      begin
        if EvaluateString(r1, r, s1, InfoSyntaxError) and
          skip(',', r, r, InfoSyntaxError) then
        begin
          list1 := TXStringList.Create;
          if produceStringList(section, r, r, list1, InfoSyntaxError) and
            skip(')', r, r, InfoSyntaxError) then
          begin
            syntaxcheck := True;
            list.Clear;
            list.AddStrings(getRegexMatchList(s1, list1));
            list1.Free;
            list1 := nil;
          end;
        end
        else
        begin
          list2 := TXStringList.Create;
          if produceStringList(section, r1, r, list2, InfoSyntaxError) and
            skip(',', r, r, InfoSyntaxError) then
          begin
            list3 := TXStringList.Create;
            if produceStringList(section, r, r, list3, InfoSyntaxError) and
              skip(')', r, r, InfoSyntaxError) then
            begin
              syntaxcheck := True;
              list.Clear;
              list.AddStrings(getRegexMatchList(list2, list3));
              list2.Free;
              list2 := nil;
              list3.Free;
              list3 := nil;
            end;
          end;
        end;
      end;
    end


    else if LowerCase(s) = LowerCase('removeFromListByContainingRegex') then
    begin
      if Skip('(', r, r1, InfoSyntaxError) then
      begin
        if EvaluateString(r1, r, s1, InfoSyntaxError) and
          skip(',', r, r, InfoSyntaxError) then
        begin
          list1 := TXStringList.Create;
          if produceStringList(section, r, r, list1, InfoSyntaxError) and
            skip(')', r, r, InfoSyntaxError) then
          begin
            syntaxcheck := True;
            list.Clear;
            list.AddStrings(removeFromListByContainingRegex(s1, list1));
            list1.Free;
            list1 := nil;
          end;
        end
        else
        begin
          list2 := TXStringList.Create;
          if produceStringList(section, r1, r, list2, InfoSyntaxError) and
            skip(',', r, r, InfoSyntaxError) then
          begin
            list3 := TXStringList.Create;
            if produceStringList(section, r, r, list3, InfoSyntaxError) and
              skip(')', r, r, InfoSyntaxError) then
            begin
              syntaxcheck := True;
              list.Clear;
              list.AddStrings(removeFromListByContainingRegex(list2, list3));
              list2.Free;
              list2 := nil;
              list3.Free;
              list3 := nil;
            end;
          end;
        end;
      end;
    end


    else if LowerCase(s) = LowerCase('getSubListByContaining') then
    begin
      if Skip('(', r, r1, InfoSyntaxError) then
      begin
        list1 := TXStringList.Create;
        // is the first argument a valid string list ?
        if not produceStringList(section, r1, r, list1, InfoSyntaxError) then
        begin
          // if not is it a string Expressionstr
          if not EvaluateString(r1, r, s1, InfoSyntaxError) then
          begin
            LogDatei.log('Error: String Expressionstr or Stringlist expected ', LLerror);
            syntaxcheck := False;
          end
          else
          begin
            // if it is a string: make a string list with one element
            list1.Clear;
            list1.Add(s1);
            syntaxcheck := True;
          end;
        end
        else
          syntaxcheck := True;   // it was a string list
        if skip(',', r, r, InfoSyntaxError) and syntaxcheck then
        begin
          list2 := TXStringList.Create;
          if produceStringList(section, r, r, list2, InfoSyntaxError) //Recursion
            and skip(')', r, r, InfoSyntaxError) then
          begin
            syntaxcheck := True;
            list.Clear;
            try
              // fill list
              for i := 0 to list1.Count - 1 do
                for k := 0 to list2.Count - 1 do
                  if AnsiContainsText(list2[k], list1[i]) then
                    list.Add(list2.Strings[k]);
            except
              on e: Exception do
              begin
                LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
                LogDatei.log('Error on producing sublist: ' + e.message, LLerror);
                FNumberOfErrors := FNumberOfErrors + 1;
                LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
              end
            end;
            list1.Free;
            list1 := nil;
            list2.Free;
            list2 := nil;
          end;
        end;
      end;
    end

    else if LowerCase(s) = LowerCase('getSubListByKey') then
    begin
      if Skip('(', r, r1, InfoSyntaxError) then
      begin
        list1 := TXStringList.Create;
        // is the first argument a valid string list ?
        if not produceStringList(section, r1, r, list1, InfoSyntaxError) then
        begin
          // if not is it a string Expressionstr
          if not EvaluateString(r1, r, s1, InfoSyntaxError) then
          begin
            LogDatei.log('Error: String Expressionstr or Stringlist expected ', LLerror);
            syntaxcheck := False;
          end
          else
          begin
            // if it is a string: make a string list with one element
            list1.Clear;
            list1.Add(s1);
            syntaxcheck := True;
          end;
        end
        else
          syntaxcheck := True;   // it was a string list
        if skip(',', r, r, InfoSyntaxError) and syntaxcheck then
        begin
          list2 := TXStringList.Create;
          if produceStringList(section, r, r, list2, InfoSyntaxError) //Recursion
            and skip(')', r, r, InfoSyntaxError) then
          begin
            syntaxcheck := True;
            list.Clear;
            try
              // fill list
              for i := 0 to list1.Count - 1 do
                for k := 0 to list2.Count - 1 do
                  if AnsiStartsText(list1.Strings[i] + '=', list2.Strings[k]) then
                    list.Add(list2.Strings[k]);
            except
              on e: Exception do
              begin
                LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
                LogDatei.log('Error on producing sublist: ' + e.message, LLerror);
                FNumberOfErrors := FNumberOfErrors + 1;
                LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
              end
            end;
            list1.Free;
            list1 := nil;
            list2.Free;
            list2 := nil;
          end;
        end;
      end;
    end


    else if LowerCase(s) = LowerCase('getSubListByMatch') then
    begin
      if Skip('(', r, r1, InfoSyntaxError) then
      begin
        list1 := TXStringList.Create;
        // is the first argument a valid string list ?
        if not produceStringList(section, r1, r, list1, InfoSyntaxError) then
        begin
          // if not is it a string Expressionstr
          if not EvaluateString(r1, r, s1, InfoSyntaxError) then
          begin
            LogDatei.log('Error: String Expressionstr or Stringlist expected ', LLerror);
            syntaxcheck := False;
          end
          else
          begin
            // if it is a string: make a string list with one element
            list1.Clear;
            list1.Add(s1);
            syntaxcheck := True;
          end;
        end
        else
          syntaxcheck := True;   // it was a string list
        if skip(',', r, r, InfoSyntaxError) and syntaxcheck then
        begin
          list2 := TXStringList.Create;
          if produceStringList(section, r, r, list2, InfoSyntaxError) //Recursion
            and skip(')', r, r, InfoSyntaxError) then
          begin
            syntaxcheck := True;
            list.Clear;
            try
              // fill list
              for i := 0 to list1.Count - 1 do
                for k := 0 to list2.Count - 1 do
                  if lowercase(list1.Strings[i]) = lowercase(list2.Strings[k]) then
                    list.Add(list1.Strings[i]);
            except
              on e: Exception do
              begin
                LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
                LogDatei.log('Error on producing sublist: ' + e.message, LLerror);
                FNumberOfErrors := FNumberOfErrors + 1;
                LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
              end
            end;
            list1.Free;
            list1 := nil;
            list2.Free;
            list2 := nil;
          end;
        end;
      end;
    end



    else if LowerCase(s) = LowerCase('reencodestrlist') then
    begin
      if Skip('(', r, r, InfoSyntaxError) then
      begin
        list1 := TXStringList.Create;
        if produceStringList(section, r, r, list1, InfoSyntaxError) //Recursion
        then
          if Skip(',', r, r, InfoSyntaxError) then
            if EvaluateString(r, r, s1, InfoSyntaxError) then
              if Skip(',', r, r, InfoSyntaxError) then
                if EvaluateString(r, r, s3, InfoSyntaxError) then
                  if Skip(')', r, r, InfoSyntaxError) then
                  begin
                    syntaxCheck := True;
                    list.Clear;
                    try
                      // s2 is here used source encoding
                      list.Text := reencode(list1.Text, s1, s2, s3);
                    except
                      on e: Exception do
                      begin
                        LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
                        LogDatei.log('Error on producing sublist: ' + e.message,
                          LLerror);
                        FNumberOfErrors := FNumberOfErrors + 1;
                        LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
                        list1.Free;
                        list1 := nil;
                      end
                    end;
                  end;
        list1.Free;
        list1 := nil;
      end;
    end

    else if LowerCase(s) = LowerCase('stringReplaceRegexInList') then
    begin
      if Skip('(', r, r, InfoSyntaxError) then
      begin
        list1 := TXStringList.Create;
        if produceStringList(section, r, r, list1, InfoSyntaxError) then
          if Skip(',', r, r, InfoSyntaxError) then
            if EvaluateString(r, r, s1, InfoSyntaxError) then
              if Skip(',', r, r, InfoSyntaxError) then
                if EvaluateString(r, r, s2, InfoSyntaxError) then
                  if Skip(')', r, r, InfoSyntaxError) then
                  begin
                    syntaxCheck := True;
                    list.Clear;
                    list.AddStrings(stringReplaceRegexInList(list1, s1, s2));
                  end;
        list1.Free;
        list1 := nil;
      end;
    end

    else
    if LowerCase(s) = LowerCase('listFiles') then
    begin
      s1 := '';
      s2 := '';
      s3 := '';
      s4 := '';
      if Skip('(', r, r, InfoSyntaxError) then
        if EvaluateString(r, r, s1, InfoSyntaxError) then
          if Skip(',', r, r, InfoSyntaxError) then
            if EvaluateString(r, r, s2, InfoSyntaxError) then
              if Skip(',', r, r, InfoSyntaxError) then
                if EvaluateString(r, r, s3, InfoSyntaxError) then
                begin
                  if Skip(',', r, r, InfoSyntaxError) then
                    if EvaluateString(r, r, s4, InfoSyntaxError) then
                    begin
                         {$IFDEF WIN32}
                      if (lowercase(s4) = '64bit') or
                        (lowercase(s4) = 'sysnative') then
                      begin
                        DSiDisableWow64FsRedirection(oldDisableWow64FsRedirectionStatus);
                        LogDatei.Log('Disable redirection to SysWOW64', LLInfo);
                      end
                      else
                      begin
                        LogDatei.Log('Wrong value: ' + s4 +
                          ' Valid values are "64bit" or "SysNative". Redirection to SysWOW64 is still enabled.',
                          LLInfo);
                      end;
                         {$ENDIF WIN32}
                    end;
                  if Skip(')', r, r, InfoSyntaxError) then
                  begin
                    syntaxCheck := True;
                    //list.clear;
                    try
                      try
                        //list1 := TXStringList.create;
                        list := TXStringList(FindAllFiles(s1, s2, StrToBool(s3)));
                        //if list = '' then list.Add('Datei nicht gefunden');
                        //list.Text := list1.Text;
                      finally
                         {$IFDEF WIN32}
                        if (lowercase(s4) = '64bit') or
                          (lowercase(s4) = 'sysnative') then
                        begin
                          DSiRevertWow64FsRedirection(
                            oldDisableWow64FsRedirectionStatus);
                          LogDatei.Log('Revert redirection to SysWOW64', LLInfo);
                        end;
                         {$ENDIF WIN32}
                        //list1.free;
                        //list1 := nil;
                      end;
                    except
                      on e: Exception do
                      begin
                        LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
                        LogDatei.log('Exception: Error on findFiles: ' +
                          e.message, LLerror);
                        list.Text := '';
                        FNumberOfErrors := FNumberOfErrors + 1;
                        LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
                      end;
                    end;
                  end;
                end;
    end


    else if LowerCase(s) = LowerCase('setStringInListAtIndex') then
    begin
      if Skip('(', r, r, InfoSyntaxError) then
      begin
        if EvaluateString(r, r, s1, InfoSyntaxError) then
          if Skip(',', r, r, InfoSyntaxError) then
            list1 := TXStringList.Create;
        if produceStringList(section, r, r, list1, InfoSyntaxError) //Recursion
        then
          if Skip(',', r, r, InfoSyntaxError) then
            if EvaluateString(r, r, s2, InfoSyntaxError) then
              if Skip(')', r, r, InfoSyntaxError) then
              begin
                syntaxCheck := True;
                list.Clear;
                try
                  if StrToInt(s2) < list1.Count then
                    list1.Strings[StrToInt(s2)] := s1
                  else
                    list1.Add(s1);
                  list.Text := list1.Text;
                except
                  on e: Exception do
                  begin
                    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
                    LogDatei.log('Exception: Error on setStringInListAtIndex: ' +
                      e.message,
                      LLerror);
                    list.Text := '';
                    FNumberOfErrors := FNumberOfErrors + 1;
                    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
                    list1.Free;
                    list1 := nil;
                  end
                end;
              end;
        list1.Free;
        list1 := nil;
      end;
    end

    else if LowerCase(s) = LowerCase('setValueByKey') then
    begin
      s3 := '';
      tmpstr2 := '';
      try
        if Skip('(', r, r, InfoSyntaxError) then
        begin
          syntaxcheck := True;

          if not evaluateString(r, r, s1, InfoSyntaxError) or
            not Skip(',', r, r, InfoSyntaxError) then
            syntaxCheck := False;

          if syntaxCheck then
            if not evaluateString(r, r, s2, InfoSyntaxError) or
              not Skip(',', r, r, InfoSyntaxError) then
              syntaxCheck := False;

          if syntaxCheck then
            if not produceStringList(script, r, tmpstr, list1, InfoSyntaxError)
            then
              syntaxCheck := False;

          // next after ',' or ')'
          if syntaxCheck then
            if Skip(',', tmpstr, tmpstr1, tmpstr3) then
              if EvaluateString(tmpstr1, tmpstr2, s3, tmpstr3) then;

          if s3 = '' then
          begin
            // only two parameter
            if Skip(')', tmpstr, r, InfoSyntaxError) then
            begin
              syntaxCheck := True;
            end;
          end
          else
          begin
            // three parameter
            if Skip(')', tmpstr2, r, InfoSyntaxError) then
            begin
              syntaxCheck := True;
              s3 := trim(s3);
              if length(s3) <> 1 then
              begin
                InfoSyntaxError :=
                  'Error: Separator parameter has to be a single char - we got: ' + s3;
                syntaxCheck := False;
              end
              else
                keyValueSeparator := s3[1];
            end;
          end;

          if syntaxCheck then
          begin
            list1.NameValueSeparator := keyValueSeparator;
            list1.Values[s1] := s2;
            list.Text := list1.Text;
          end;
        end
      finally
        list1.Free;
        list1 := nil;
      end;
    end

    else if LowerCase(s) = LowerCase('getListFromWMI') then
    begin
     {$IFDEF UNIX}
      LogDatei.log('Error getListFromWMI only implemented for Windows.', LLError);
      {$ENDIF UNIX}
      {$IFDEF WINDOWS}
      if Skip('(', r, r, InfoSyntaxError) then
      begin
        try
          list1 := TXStringList.Create;
          if EvaluateString(r, r, s1, InfoSyntaxError) then
            if Skip(',', r, r, InfoSyntaxError) then
              if EvaluateString(r, r, s2, InfoSyntaxError) then
                if Skip(',', r, r, InfoSyntaxError) then
                  if produceStringList(section, r, r, list1, InfoSyntaxError) //Recursion
                  then
                    if Skip(',', r, r, InfoSyntaxError) then
                      if EvaluateString(r, r, s3, InfoSyntaxError) then
                        if Skip(')', r, r, InfoSyntaxError) then
                        begin
                          syntaxCheck := True;
                          list.Clear;
                          try
                            ErrorMsg := '';
                            if not osGetWMI(s1, s2, list1, s3,
                              TStringList(list), ErrorMsg) then
                            begin
                              LogDatei.log('Error on getListFromWMI: ' +
                                ErrorMsg, LLerror);
                              list.Text := '';
                              FNumberOfErrors := FNumberOfErrors + 1;
                            end;
                          except
                            on e: Exception do
                            begin
                              LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
                              LogDatei.log('Exception: Error on getListFromWMI: ' +
                                e.message,
                                LLerror);
                              list.Text := '';
                              FNumberOfErrors := FNumberOfErrors + 1;
                              LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
                            end
                          end;
                        end;
        finally
          list1.Free;
          list1 := nil;
        end;
      end;
   {$ENDIF WINDOWS}
    end


    else if LowerCase(s) = LowerCase('addtolist') then
    begin
      if Skip('(', r, r, InfoSyntaxError) then
      begin
        list1 := TXStringList.Create;
        if produceStringList(section, r, r, list1, InfoSyntaxError) //Recursion
          and skip(',', r, r, InfoSyntaxError) then
        begin
          if EvaluateString(r, r, s1, InfoSyntaxError) and
            skip(')', r, r, InfoSyntaxError) then
          begin
            syntaxcheck := True;
            list.Clear;
            //list.AddStrings(list1.lines);
            try
              //for i := 0 to list1.count - 1
              //do list.add (list1[i]);
              list.AddStrings(list1);
              list.Add(s1);
            except
              on e: Exception do
              begin
                LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
                LogDatei.log('Error on producing sublist: ' + e.message,
                  LLerror);
                FNumberOfErrors := FNumberOfErrors + 1;
                LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
              end
            end;
            list1.Free;
            list1 := nil;
          end;
        end;
      end;
    end

    else if LowerCase(s) = LowerCase('addlisttolist') then
    begin
      if Skip('(', r, r, InfoSyntaxError) then
      begin
        list1 := TXStringList.Create;
        if produceStringList(section, r, r, list1, InfoSyntaxError) //Recursion
          and skip(',', r, r, InfoSyntaxError) then
        begin
          list2 := TXStringList.Create;
          if produceStringList(section, r, r, list2, InfoSyntaxError) //Recursion
            and skip(')', r, r, InfoSyntaxError) then
          begin
            syntaxcheck := True;
            list.Clear;
            try
              list.AddStrings(list1);
              list.AddStrings(list2);
            except
              on e: Exception do
              begin
                LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
                LogDatei.log('Error on producing sublist: ' + e.message,
                  LLerror);
                FNumberOfErrors := FNumberOfErrors + 1;
                LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
              end
            end;
            list1.Free;
            list1 := nil;
            list2.Free;
            list2 := nil;
          end;
        end;
      end;
    end

    else if LowerCase(s) = LowerCase('getListContainingList') then
    begin
      if Skip('(', r, r, InfoSyntaxError) then
      begin
        list1 := TXStringList.Create;
        if produceStringList(section, r, r, list1, InfoSyntaxError) //Recursion
          and skip(',', r, r, InfoSyntaxError) then
        begin
          list2 := TXStringList.Create;
          if produceStringList(section, r, r, list2, InfoSyntaxError) //Recursion
            and skip(')', r, r, InfoSyntaxError) then
          begin
            syntaxcheck := True;
            list.Clear;
            try
              for i := 0 to list1.Count - 1 do
                for k := 0 to list2.Count - 1 do
                  if list1.Strings[i] = list2.Strings[k] then
                    list.Add(list1.Strings[i]);
            except
              on e: Exception do
              begin
                LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
                LogDatei.log('Error on producing sublist: ' + e.message,
                  LLerror);
                FNumberOfErrors := FNumberOfErrors + 1;
                LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
              end
            end;
            list1.Free;
            list1 := nil;
            list2.Free;
            list2 := nil;
          end;
        end;
      end;
    end



    else if LowerCase(s) = LowerCase('getsublist') then
    begin
      if Skip('(', r, r, InfoSyntaxError) then
      begin
        syntaxcheck := True;
        // syntax: "{d}:[-]{d}, listvalue , that is any number of digits followed by a ':'
        // followed by again any number of digits, followed by a comma, then the listvalue
        // if the first series of digits is empty, the start index is zero
        // if the second series of digits is empty, the last index is count - 1
        int64_1 := 0;
        int64_2 := 0;
        try
          s2 := '';
          GetWord(r, s1, r, [':']);
          s1 := trim(s1);
          if length(s1) = 0 then
            int64_1 := 0
          else
          begin
            if not TryStrToInt64(s1, int64_1) then
            begin
              if EvaluateString(s1, s1, s2, InfoSyntaxError) then
              begin
                if s2 = '' then
                  int64_1 := 0
                else
                if not TryStrToInt64(s2, int64_1) then
                begin
                  syntaxcheck := False;
                  InfoSyntaxError :=
                    'Given valid string expression: +' + s1 +
                    ' solved to: ' + s2 + ' which could not be converted to integer';
                end;
              end
              else
              begin
                syntaxcheck := False;
                InfoSyntaxError :=
                  'Given string: +' + s1 +
                  ' is no integer and no valid sting expression';
              end;
            end;
          end;
          if syntaxCheck then
            LogDatei.log_prog('getsublist p1: ' + IntToStr(int64_1) +
              ' from: ' + s2 + ' from: ' + s1, LLDebug);

          if syntaxCheck then
          begin
            syntaxCheck := Skip(':', r, r, InfoSyntaxError);
            a2_to_default := False;
            r1 := r;
            s2 := '';
            if EvaluateString(r, r, s2, InfoSyntaxError) then
            begin
              if s2 = '' then
              begin
                int64_2 := 0;
                a2_to_default := True;
              end
              else
              if not TryStrToInt64(s2, int64_2) then
              begin
                syntaxcheck := False;
                InfoSyntaxError :=
                  'Given valid string expression: +' + s1 + ' solved to: ' +
                  s2 + ' which could not be converted to integer';
              end;
            end
            else
            begin
              // it is no string expression
              r := r1;
              GetWord(r, s1, r, [',']);
              s1 := trim(s1);
              // is it empty ?
              if length(s1) = 0 then
              begin
                int64_2 := 0;
                a2_to_default := True;
              end
              else
              begin
                // is it a number ?
                if not TryStrToInt64(s1, int64_2) then
                begin
                  syntaxcheck := False;
                  InfoSyntaxError :=
                    'Given string: +' + s1 +
                    ' is no integer and no valid sting expression';
                end;
              end;
            end;
            if syntaxCheck then
              LogDatei.log_prog('getsublist p1: ' + IntToStr(int64_2) +
                ' from: ' + s2 + ' from: ' + s1, LLDebug);
            a1 := int64_1;
            a2 := int64_2;

            if syntaxCheck then
              syntaxCheck := Skip(',', r, r, InfoSyntaxError);
          end;
        except
          syntaxcheck := False;
          InfoSyntaxError := ' No valid sublist selection ';
        end;

        if syntaxCheck then
        begin
          list1 := TXStringList.Create;

          if not produceStringList(section, r, r, list1, InfoSyntaxError) //Recursion
            or not Skip(')', r, r, InfoSyntaxError) then
            syntaxCheck := False
          else
          begin
            list.Clear;

            if a2_to_default then
              a2 := list1.Count - 1;
            adjustBounds(a1, a2, list1);

            for i := a1 to a2 do
              try
                list.add(list1[i]);
              except
                on e: Exception do
                begin
                  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
                  LogDatei.log('Error on producing sublist: ' + e.message,
                    LLWarning);
                  FNumberOfWarnings := FNumberOfWarnings + 1;
                  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
                end
              end;


            list1.Free;
            list1 := nil;
          end;
        end;
      end;
    end

    else if LowerCase(s) = LowerCase('createStringList') then
    begin
      if Skip('(', r, r, InfoSyntaxError) then
      begin
        if length(r) = 0 then
          InfoSyntaxError := ' ")" missing '
        else
        begin
          goOn := True;
          syntaxCheck := True;
        end;
        if (r[1] = ')') and goon then
        begin
          goOn := False;
          Skip(')', r, r, InfoSyntaxError); //consume the ')'
          // everything ok - create empty string list
        end
        else
        begin

          while syntaxCheck and goon do
          begin
            evaluateString(r, r, s1, InfoSyntaxError);
            // empty strings are allowed elements
            // so we comment the next two lines (do 10.1.19)
            //if length(s1) > 0
            //then
            list.add(s1);
            logdatei.log_prog('createStringList: add: ' + s1 + ' to: ' +
              list.Text, LLDebug);

            if length(r) = 0 then
            begin
              syntaxCheck := False;
              InfoSyntaxError := ' Expressionstr not terminated ';
            end
            else
            begin
              if r[1] = ',' then
                Skip(',', r, r, InfoSyntaxError) //here is no syntax error possible
              else if r[1] = ')' then
              begin
                goOn := False;
                Skip(')', r, r, InfoSyntaxError); //consume the ')'
              end
              else
              begin
                syntaxCheck := False;
                InfoSyntaxError := ' "," or ")" expected ';
              end;
            end;
          end;
        end;
      end;
    end

    else if LowerCase(s) = LowerCase('reverse') then
    begin
      if Skip('(', r, r, InfoSyntaxError) then
      begin
        list1 := TXStringList.Create;
        if produceStringList(section, r, r, list1, InfoSyntaxError) //Recursion
        then
        begin
          list.Clear;
          for i := list1.Count downto 1 do
            list.add(list1[i - 1]);
          if Skip(')', r, r, InfoSyntaxError) then
            syntaxCheck := True;
        end;
        list1.Free;
      end;
    end

    else if LowerCase(s) = LowerCase('replaceOpsiConstants') then
    begin
      if Skip('(', r, r, InfoSyntaxError) then
      begin
        list1 := TXStringList.Create;
        if produceStringList(section, r, r, list1, InfoSyntaxError) //Recursion
        then
        begin
          list.Clear;
          for i := 1 to list1.Count do
          begin
            tmpstr := list1[i - 1];
            for k := 1 to ConstList.Count do
            begin
              if list1.replaceInLine(tmpstr, Constlist.Strings[k - 1],
                ConstValuesList.Strings[k - 1], False, tmpstr1) then
                tmpstr := tmpstr1;
            end;
            list.add(tmpstr);
          end;
          if Skip(')', r, r, InfoSyntaxError) then
            syntaxCheck := True;
        end;
        list1.Free;
      end;
    end


    else if LowerCase(s) = LowerCase('getKeyList') then
    begin
      if Skip('(', r, r, InfoSyntaxError) then
      begin
        list1 := TXStringList.Create;
        if produceStringList(section, r, r, list1, InfoSyntaxError) //Recursion
        then
        begin
          if Skip(')', r, r, InfoSyntaxError) then
          begin
            syntaxCheck := True;
            list.Clear;
            for i := 0 to list1.Count - 1 do
            begin
              k := pos('=', list1[i]);
              if k = 0 then
                k := length(list1[i]);
              list.add(copy(list1[i], 1, k - 1));
            end;
          end;
        end;
        list1.Free;
      end;
    end

    else if LowerCase(s) = LowerCase('emptyList') then
    begin
      if Skip('(', r, r, InfoSyntaxError) then
      begin
        // we do not need it - but for syntax check reasons it has to be done
        list1 := TXStringList.Create;
        if produceStringList(section, r, r, list1, InfoSyntaxError) //Recursion
        then
        begin
          list.Clear;
          if Skip(')', r, r, InfoSyntaxError) then
            syntaxCheck := True;
        end;
        list1.Free;
      end;
    end

    // #########  start xml2 list functions ###############################

    else if LowerCase(s) = LowerCase('getXml2Document') then
    begin
      if Skip('(', r, r, InfoSyntaxError) then
      begin
        list1 := TXStringList.Create;
        if produceStringList(section, r, r, list1, InfoSyntaxError) //Recursion
        then
        begin
          list.Clear;
          list.Text := getDocumentElementAsStringlist(TStringList(list1)).Text;
          if Skip(')', r, r, InfoSyntaxError) then
            syntaxCheck := True;
        end;
        list1.Free;
      end;
    end

    else if LowerCase(s) = LowerCase('getXml2DocumentFromFile') then
    begin
      if Skip('(', r, r, InfoSyntaxError) then
      begin
        if EvaluateString(r, r, s1, InfoSyntaxError) and
          skip(')', r, r, InfoSyntaxError) then
        begin
          syntaxCheck := True;
          list.Clear;
          list.Text := getXMLDocumentElementfromFile(ExpandFileNameUTF8(s1)).Text;
        end;
      end;
    end

    else if LowerCase(s) = LowerCase('getXml2UniqueChildnodeByName') then
    begin
      if Skip('(', r, r, InfoSyntaxError) then
      begin
        list1 := TXStringList.Create;
        if produceStringList(section, r, r, list1, InfoSyntaxError) //Recursion
          and skip(',', r, r, InfoSyntaxError) then
        begin
          if EvaluateString(r, r, s1, InfoSyntaxError) and
            skip(')', r, r, InfoSyntaxError) then
          begin
            syntaxcheck := True;
            list.Clear;
            if not xmlAsStringlistGetUniqueChildnodeByName(
              TStringList(list1), s1, TStringList(list)) then
            begin
              LogDatei.log('Error on producing getXml2UniqueChildnodeByName', LLerror);
            end;
            list1.Free;
            list1 := nil;
          end;
        end;
      end;
    end

    else if LowerCase(s) = LowerCase('xml2GetFirstChildNodeByName') then
    begin
      if Skip('(', r, r, InfoSyntaxError) then
      begin
        list1 := TXStringList.Create;
        if produceStringList(section, r, r, list1, InfoSyntaxError) //Recursion
          and skip(',', r, r, InfoSyntaxError) then
        begin
          if EvaluateString(r, r, s1, InfoSyntaxError) and
            skip(')', r, r, InfoSyntaxError) then
          begin
            syntaxcheck := True;
            list.Clear;
            if not xml2GetFirstChildNodeByName(TStringList(list1),
              s1, TStringList(list)) then
            begin
              LogDatei.log('Error on producing xml2GetFirstChildNodeByName', LLerror);
            end;
            list1.Free;
            list1 := nil;
          end;
        end;
      end;
    end

    // This one does not work right now
    else if LowerCase(s) = LowerCase('xml2GetFirstChildNodeByNameAtributeValue') then
    begin
      if Skip('(', r, r, InfoSyntaxError) then
      begin
        list1 := TXStringList.Create;
        try
          if produceStringList(section, r, r, list1, InfoSyntaxError) //Recursion
            and skip(',', r, r, InfoSyntaxError) then
          begin
            if EvaluateString(r, r, s1, InfoSyntaxError) and
              skip(',', r, r, InfoSyntaxError) then
            begin
              if EvaluateString(r, r, s2, InfoSyntaxError) and
                skip(',', r, r, InfoSyntaxError) then
              begin
                if EvaluateString(r, r, s3, InfoSyntaxError) and
                  skip(')', r, r, InfoSyntaxError) then
                begin
                  syntaxcheck := True;
                  list.Clear;
                  LogDatei.log(
                    'Error: xml2GetFirstChildNodeByNameAtributeValue: not implemented',
                    LLerror);
                  //if not xmlAsStringlistGetChildnodeByNameAndAttributeKeyAndValue(Tstringlist(list1),s1,s2,s3,TStringlist(list)) then
                  //begin
                  //  LogDatei.log('Error on producing xml2GetFirstChildNodeByName', LLerror);
                  //end;
                end
                else
                  syntaxcheck := False;
              end
              else
                syntaxcheck := False;
            end
            else
              syntaxcheck := False;
          end
          else
            syntaxcheck := False;
        finally
          list1.Free;
          list1 := nil;
        end;
      end
      else
        syntaxcheck := False;
    end


    // #########  end xml2 list functions ###############################


    // todo: 2nd parameter focus row for editmap
    else if LowerCase(s) = LowerCase('editMap') then
    begin
      if Skip('(', r, r, InfoSyntaxError) then
      begin
        // we do not need it - but for syntax check reasons it has to be done
        list1 := TXStringList.Create;
        if produceStringList(section, r, r, list1, InfoSyntaxError) //Recursion
        then
        begin
          list.Clear;
        {$IFDEF GUI}
          checkMapGUI(TStringList(list1), 2);
          list.AddStrings(list1);
        {$ELSE GUI}
          for i := 0 to list1.Count - 1 do
          begin
            s1 := list1.Strings[i];
            s2 := Copy(s1, 1, pos('=', s1) - 1);
            s3 := Copy(s1, pos('=', s1) + 1, length(s1));
            if not cmdLineInputDialog(s3, s2, s3, False) then
              Logdatei.log('Error editMap (noGUI) for: ' + s1, LLError);
            list.Add(s2 + '=' + s3);
          end;
        {$ENDIF GUI}
          if Skip(')', r, r, InfoSyntaxError) then
            syntaxCheck := True;
        end;
        list1.Free;
      end;
    end


    else if LowerCase(s) = LowerCase('getProductMap') then
    begin
      //if (r = '') or (r=')') then
      begin
        syntaxcheck := True;
        if not (opsidata = nil) then
        begin
          list.add('id=' + Topsi4data(opsidata).getActualProductId);
          list.add('name=' + Topsi4data(opsidata).getActualProductName);
          list.add('description=' + Topsi4data(opsidata).getActualProductDescription);
          list.add('advice=' + Topsi4data(opsidata).getActualProductAdvice);
          list.add('productversion=' + Topsi4data(
            opsidata).getActualProductProductVersion);
          list.add('packageversion=' + Topsi4data(
            opsidata).getActualProductPackageVersion);
          list.add('priority=' + Topsi4data(opsidata).getActualProductPriority);
          list.add('installationstate=' + Topsi4data(
            opsidata).getActualProductInstallationState);
          list.add('lastactionrequest=' + Topsi4data(
            opsidata).getActualProductLastActionRequest);
          list.add('lastactionresult=' + Topsi4data(
            opsidata).getActualProductProductLastActionReport);
          list.add('installedversion=' + Topsi4data(
            opsidata).getActualProductInstalledVersion);
          list.add('installedpackage=' + Topsi4data(
            opsidata).getActualProductInstalledPackage);
          list.add('installedmodificationtime=' + Topsi4data(
            opsidata).getActualProductInstalledModificationTime);
          list.add('actionrequest=' + Topsi4data(
            opsidata).getActualProductActionRequest);
        end;
      end;
    end

   {$IFDEF WIN32}
    else if LowerCase(s) = LowerCase('getIpMacMap')
    //does not work (do 15.6.2012)
    then
    begin
      begin
        syntaxcheck := True;
        list.AddStrings(getIpMacHash);
      end;
    end
   {$ENDIF WIN32}

    else if LowerCase(s) = LowerCase('getLocaleInfo') then
    begin
      //if (r = '') or (r=')') then
      begin
        syntaxcheck := True;
      {$IFDEF WINDOWS}
        list.add(copy(GetSystemDefaultLocale(LOCALE_SABBREVLANGNAME), 1, 2));
        list.add(GetSystemDefaultLocale(LOCALE_SABBREVLANGNAME));
        list.add(GetSystemDefaultLocale(LOCALE_SENGLANGUAGE));
        list.add(GetSystemDefaultLocale(LOCALE_SENGCOUNTRY));
        list.add(GetSystemDefaultLocale(LOCALE_ILANGUAGE));
        list.add(GetSystemDefaultLocale(LOCALE_SNATIVELANGNAME));
        list.add(GetSystemDefaultLocale(LOCALE_IDEFAULTLANGUAGE));
        //list.add (GetSystemDefaultLocale(LOCALE_NOUSEROVERRIDE));
      {$ENDIF WINDOWS}
      end;
    end

    else if LowerCase(s) = LowerCase('getLocaleInfoMap') then
    begin
      //if (r = '') or (r=')') then
      begin
        syntaxcheck := True;
      {$IFDEF WINDOWS}
        list.add('language_id_2chars=' +
          copy(GetSystemDefaultLocale(LOCALE_SABBREVLANGNAME), 1, 2));
        list.add('language_id=' + GetSystemDefaultLocale(LOCALE_SABBREVLANGNAME));
        list.add('localized_name_of_language=' +
          GetSystemDefaultLocale(LOCALE_SLANGUAGE));
        list.add('English_name_of_language=' +
          GetSystemDefaultLocale(LOCALE_SENGLANGUAGE));
        list.add('abbreviated_language_name=' +
          GetSystemDefaultLocale(LOCALE_SABBREVLANGNAME));
        list.add('native_name_of_language=' + GetSystemDefaultLocale(
          LOCALE_SNATIVELANGNAME));

        list.add('country_code=' + GetSystemDefaultLocale(LOCALE_ICOUNTRY));
        list.add('localized_name_of_country=' + GetSystemDefaultLocale(LOCALE_SCOUNTRY));
        list.add('English_name_of_country=' +
          GetSystemDefaultLocale(LOCALE_SENGCOUNTRY));
        list.add('abbreviated_country_name=' +
          GetSystemDefaultLocale(LOCALE_SABBREVCTRYNAME));
        list.add('native_name_of_country=' + GetSystemDefaultLocale(
          LOCALE_SNATIVECTRYNAME));
        list.add('default_language_id=' + GetSystemDefaultLocale(
          LOCALE_IDEFAULTLANGUAGE));
        list.add('default_language_id_decimal=' +
          IntToStr(StrToInt('$' + GetSystemDefaultLocale(LOCALE_IDEFAULTLANGUAGE))));
        list.add('default_country_code=' + GetSystemDefaultLocale(
          LOCALE_IDEFAULTCOUNTRY));
        list.add('default_oem_code_page=' + GetSystemDefaultLocale(
          LOCALE_IDEFAULTCODEPAGE));
        list.add('default_ansi_code_page=' + GetSystemDefaultLocale(
          LOCALE_IDEFAULTANSICODEPAGE));
        tmpstr := GetRegistrystringvalue(
          'HKEY_LOCAL_MACHINE\System\CurrentControlset\Control\Nls\Language',
          'InstallLanguage', False);
        list.add('system_default_language_id=' + tmpstr);
        //list.add ('system_default_UI_language=' + GetSystemDefaultUILanguage );
        list.add('system_default_posix=' + StringReplace(
          getlangcodeByHexvalueStr('0x' + tmpstr), '-', '_'));
        list.add('system_default_lang_region=' +
          getlangcodeByHexvalueStr('0x' + tmpstr));
      {$ENDIF WINDOWS}
      end;
    end

  {$IFDEF WINDOWS}
    else if LowerCase(s) = LowerCase('getMSVersionMap') then
    begin
      //if (r = '') or (r=')') then
      begin
        syntaxcheck := True;
        list.add('major_version=' + GetSystemOSVersionInfoEx('major_version'));
        list.add('minor_version=' + GetSystemOSVersionInfoEx('minor_version'));
        list.add('build_number=' + GetSystemOSVersionInfoEx('build_number'));
        list.add('platform_id=' + GetSystemOSVersionInfoEx('platform_id'));
        list.add('csd_version=' + GetSystemOSVersionInfoEx('csd_version'));
        list.add('service_pack_major=' + GetSystemOSVersionInfoEx('service_pack_major'));
        list.add('service_pack_minor=' + GetSystemOSVersionInfoEx('service_pack_minor'));
        list.add('suite_mask=' + GetSystemOSVersionInfoEx('suite_mask'));
        list.add('product_type_nr=' + GetSystemOSVersionInfoEx('product_type_nr'));
        list.add('2003r2=' + GetSystemOSVersionInfoEx('2003r2'));
        if GetNTVersionMajor < 6 then
        begin
          list.add('ReleaseID=');
          list.add('prodInfoNumber=');
          list.add('prodInfoText=');
        end
        else
        begin
          if GetNTVersionMajor >= 10 then
            if RegVarExists('HKLM\SOFTWARE\Microsoft\Windows NT\CurrentVersion',
              'ReleaseID', True) then
              list.add('ReleaseID=' + GetRegistrystringvalue(
                'HKLM\SOFTWARE\Microsoft\Windows NT\CurrentVersion', 'ReleaseID', True))
            else
              list.add('ReleaseID=1507')
          else
            list.add('ReleaseID=');
          tmpint := OSGetProductInfoNum;
          list.add('prodInfoNumber=' + IntToStr(tmpInt));
          list.add('prodInfoText=' + getProductInfoStrByNum(tmpInt));
        end;
      end;
    end
   {$ENDIF WINDOWS}

   {$IFDEF LINUX}
    else if LowerCase(s) = LowerCase('getLinuxVersionMap') then
    begin
      begin
        syntaxcheck := True;
        list.AddStrings(getLinuxVersionMap);
      end;
    end
    {$ENDIF LINUX}

    {$IFDEF DARWIN}
    else if LowerCase(s) = LowerCase('getMacosVersionMap') then
    begin
      begin
        syntaxcheck := True;
        list.AddStrings(getMacosVersionMap);
      end;
    end
    {$ENDIF DARWIN}


   {$IFDEF WINDOWS}
    else if (LowerCase(s) = LowerCase('getFileInfoMap32')) then
      s := 'getFileInfoMap'
   {$ENDIF WINDOWS}

    else if (LowerCase(s) = LowerCase('getFileInfoMap')) then
    begin
      if Skip('(', r, r, InfoSyntaxError) then
        if EvaluateString(r, r, s1, InfoSyntaxError) then
        begin
          if not fileExists(s1) then
          begin
            LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
            LogDatei.log('File "' + s1 + '" does not exist', LLWarning);
            FNumberOfWarnings := FNumberOfWarnings + 1;
            LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
          end
          else
          begin
         {$IFDEF WINDOWS}
            versionInfo := TVersionInfo.Create(s1);

            for i := 0 to versionInfo.TranslationCount - 1 do
            begin
              list.add('Language name ' + IntToStr(i) + '=' +
                versionInfo.LanguageNames[i]);
              list.add('Language ID ' + IntToStr(i) + '=' +
                IntToStr(versionInfo.LanguageID[i]));
            end;

            list.add('file version=' + IntToStr(versionInfo.FileVersion));
            list.add('file version with dots=' + versionInfo.GetFileVersionWithDots);
            list.add('product version=' + IntToStr(versionInfo.ProductVersion));


            for i := Low(versionInfoX.PredefinedStrings)
              to High(versionInfoX.PredefinedStrings) do
              list.add(versionInfoX.PredefinedStrings[i] + '=' +
                versionInfo.getString(PredefinedStrings[i]));


            versionInfo.Free;
         {$ENDIF WINDOWS}
         {$IFDEF UNIX}
         (*
         *)
         {$ENDIF UNIX}
          end;
          if Skip(')', r, r, InfoSyntaxError) then
          begin
            syntaxCheck := True;
          end;
        end;
    end

   {$IFDEF WINDOWS}
    else if LowerCase(s) = LowerCase('getFileInfoMap64') then
    begin
      if Skip('(', r, r, InfoSyntaxError) then
        if EvaluateString(r, r, s1, InfoSyntaxError) then
        begin
        {$IFDEF WIN32}
          Wow64FsRedirectionDisabled := False;
          if DSiDisableWow64FsRedirection(oldDisableWow64FsRedirectionStatus) then
          begin
            LogDatei.log('DisableWow64FsRedirection succeeded', LLinfo);
            Wow64FsRedirectionDisabled := True;
          end
          else
          begin
            Wow64FsRedirectionDisabled := False;
            LogDatei.log('Error: DisableWow64FsRedirection failed', LLError);
          end;
       {$ENDIF WIN32}
    (*
        if DSiDisableWow64FsRedirection(oldDisableWow64FsRedirectionStatus) then
        begin
          LogDatei.log('DisableWow64FsRedirection succeeded', LLinfo);
    *)
          if not fileExists(s1) then
          begin
            LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
            LogDatei.log('File "' + s1 + '" does not exist', LLWarning);
            FNumberOfWarnings := FNumberOfWarnings + 1;
            LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
          end
          else
          begin
            versionInfo := TVersionInfo.Create(s1);

            for i := 0 to versionInfo.TranslationCount - 1 do
            begin
              list.add('Language name ' + IntToStr(i) + '=' +
                versionInfo.LanguageNames[i]);
              list.add('Language ID ' + IntToStr(i) + '=' +
                IntToStr(versionInfo.LanguageID[i]));
            end;

            list.add('file version=' + IntToStr(versionInfo.FileVersion));
            list.add('file version with dots=' + versionInfo.GetFileVersionWithDots);
            list.add('product version=' + IntToStr(versionInfo.ProductVersion));


            for i := Low(versionInfoX.PredefinedStrings)
              to High(versionInfoX.PredefinedStrings) do
              list.add(versionInfoX.PredefinedStrings[i] + '=' +
                versionInfo.getString(PredefinedStrings[i]));


            versionInfo.Free;
          end;
          {$IFDEF WIN32}
          if Wow64FsRedirectionDisabled then
          begin
            boolresult := DSiRevertWow64FsRedirection(
              oldDisableWow64FsRedirectionStatus);
            Wow64FsRedirectionDisabled := False;
            LogDatei.log('RevertWow64FsRedirection succeeded', LLinfo);
          end;
          {$ENDIF}
    (*
          dummybool := DSiRevertWow64FsRedirection(oldDisableWow64FsRedirectionStatus);
          LogDatei.log('RevertWow64FsRedirection succeeded', LLinfo);
        (*
        end
        else
        begin
          LogDatei.log('Error: DisableWow64FsRedirection failed', LLError);
        end;
        *)
          if Skip(')', r, r, InfoSyntaxError) then
          begin
            syntaxCheck := True;
          end;
        end;
    end


    else if LowerCase(s) = LowerCase('getFileInfoMapSysnative') then
    begin
      if Skip('(', r, r, InfoSyntaxError) then
        if EvaluateString(r, r, s1, InfoSyntaxError) then
        begin
          if Is64BitSystem then
          begin
            LogDatei.log('  Starting getFileInfoMap (SysNative 64 Bit mode)...', LLInfo);
            {$IFDEF WIN32}
            Wow64FsRedirectionDisabled := False;
            if DSiDisableWow64FsRedirection(oldDisableWow64FsRedirectionStatus) then
            begin
              LogDatei.log('DisableWow64FsRedirection succeeded', LLinfo);
              Wow64FsRedirectionDisabled := True;
            end
            else
            begin
              Wow64FsRedirectionDisabled := False;
              LogDatei.log('Error: DisableWow64FsRedirection failed', LLError);
            end;
           {$ENDIF WIN32}
           (*
          if DSiDisableWow64FsRedirection(oldDisableWow64FsRedirectionStatus) then
          begin
            LogDatei.log('DisableWow64FsRedirection succeeded', LLinfo);
           *)
            if not fileExists(s1) then
            begin
              LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
              LogDatei.log('File "' + s1 + '" does not exist', LLWarning);
              FNumberOfWarnings := FNumberOfWarnings + 1;
              LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
            end
            else
            begin
              versionInfo := TVersionInfo.Create(s1);

              for i := 0 to versionInfo.TranslationCount - 1 do
              begin
                list.add('Language name ' + IntToStr(i) + '=' +
                  versionInfo.LanguageNames[i]);
                list.add('Language ID ' + IntToStr(i) + '=' +
                  IntToStr(versionInfo.LanguageID[i]));
              end;

              list.add('file version=' + IntToStr(versionInfo.FileVersion));
              list.add('file version with dots=' + versionInfo.GetFileVersionWithDots);
              list.add('product version=' + IntToStr(versionInfo.ProductVersion));


              for i := Low(versionInfoX.PredefinedStrings)
                to High(versionInfoX.PredefinedStrings) do
                list.add(versionInfoX.PredefinedStrings[i] + '=' +
                  versionInfo.getString(PredefinedStrings[i]));


              versionInfo.Free;
            end;
            {$IFDEF WIN32}
            if Wow64FsRedirectionDisabled then
            begin
              boolresult := DSiRevertWow64FsRedirection(
                oldDisableWow64FsRedirectionStatus);
              Wow64FsRedirectionDisabled := False;
              LogDatei.log('RevertWow64FsRedirection succeeded', LLinfo);
            end;
            {$ENDIF}
            (*
            dummybool := DSiRevertWow64FsRedirection(oldDisableWow64FsRedirectionStatus);
            LogDatei.log('RevertWow64FsRedirection succeeded', LLinfo);
          end
          else
          begin
            LogDatei.log('Error: DisableWow64FsRedirection failed', LLError);
          end;
          *)
          end
          else
          begin
            LogDatei.log('  Starting getFileInfoMap (SysNative 32 Bit mode)...', LLInfo);
            if not fileExists(s1) then
            begin
              LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
              LogDatei.log('File "' + s1 + '" does not exist', LLWarning);
              FNumberOfWarnings := FNumberOfWarnings + 1;
              LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
            end
            else
            begin
              versionInfo := TVersionInfo.Create(s1);

              for i := 0 to versionInfo.TranslationCount - 1 do
              begin
                list.add('Language name ' + IntToStr(i) + '=' +
                  versionInfo.LanguageNames[i]);
                list.add('Language ID ' + IntToStr(i) + '=' +
                  IntToStr(versionInfo.LanguageID[i]));
              end;

              list.add('file version=' + IntToStr(versionInfo.FileVersion));
              list.add('file version with dots=' + versionInfo.GetFileVersionWithDots);
              list.add('product version=' + IntToStr(versionInfo.ProductVersion));

              for i := Low(versionInfoX.PredefinedStrings)
                to High(versionInfoX.PredefinedStrings) do
                list.add(versionInfoX.PredefinedStrings[i] + '=' +
                  versionInfo.getString(PredefinedStrings[i]));
              versionInfo.Free;
            end;
          end;
          if Skip(')', r, r, InfoSyntaxError) then
          begin
            syntaxCheck := True;
          end;
        end;
    end


   {$ENDIF WINDOWS}

    else if LowerCase(s) = LowerCase('getRegistryKeyList32') then
    begin
    {$IFDEF WINDOWS}
      if Skip('(', r, r, InfoSyntaxError) then
        if EvaluateString(r, r, s1, InfoSyntaxError) then
        begin
          //for i :=
          list.AddStrings(GetRegistryKeyList(s1, False));
          if Skip(')', r, r, InfoSyntaxError) then
          begin
            syntaxCheck := True;
          end;
        end;
    {$ELSE WINDOWS}
      SyntaxCheck := False;
      InfoSyntaxError := 'Only implemented for Windows';
      LogDatei.log(s + ' is only implemented for Windows', LLError);
    {$ENDIF WINDOWS}
    end

    else if (LowerCase(s) = LowerCase('getRegistryKeyList64')) or
      (LowerCase(s) = LowerCase('getRegistryKeyListSysnative')) then
    begin
    {$IFDEF WINDOWS}
      if Skip('(', r, r, InfoSyntaxError) then
        if EvaluateString(r, r, s1, InfoSyntaxError) then
        begin
          //for i :=
          list.AddStrings(GetRegistryKeyList(s1, True));
          if Skip(')', r, r, InfoSyntaxError) then
          begin
            syntaxCheck := True;
          end;
        end;
    {$ELSE WINDOWS}
      SyntaxCheck := False;
      InfoSyntaxError := 'Only implemented for Windows';
      LogDatei.log(s + ' is only implemented for Windows', LLError);
    {$ENDIF WINDOWS}
    end

    else if LowerCase(s) = LowerCase('getRegistryVarList32') then
    begin
    {$IFDEF WINDOWS}
      if Skip('(', r, r, InfoSyntaxError) then
        if EvaluateString(r, r, s1, InfoSyntaxError) then
        begin
          //for i :=
          list.AddStrings(GetRegistryVarList(s1, False));
          if Skip(')', r, r, InfoSyntaxError) then
          begin
            syntaxCheck := True;
          end;
        end;
    {$ELSE WINDOWS}
      SyntaxCheck := False;
      InfoSyntaxError := 'Only implemented for Windows';
      LogDatei.log(s + ' is only implemented for Windows', LLError);
    {$ENDIF WINDOWS}
    end

    else if (LowerCase(s) = LowerCase('getRegistryVarList64')) or
      (LowerCase(s) = LowerCase('getRegistryVarListSysnative')) then
    begin
    {$IFDEF WINDOWS}
      if Skip('(', r, r, InfoSyntaxError) then
        if EvaluateString(r, r, s1, InfoSyntaxError) then
        begin
          //for i :=
          list.AddStrings(GetRegistryVarList(s1, True));
          if Skip(')', r, r, InfoSyntaxError) then
          begin
            syntaxCheck := True;
          end;
        end;
    {$ELSE WINDOWS}
      SyntaxCheck := False;
      InfoSyntaxError := 'Only implemented for Windows';
      LogDatei.log(s + ' is only implemented for Windows', LLError);
    {$ENDIF WINDOWS}
    end

    else if LowerCase(s) = LowerCase('getRegistryVarMap32') then
    begin
    {$IFDEF WINDOWS}
      if Skip('(', r, r, InfoSyntaxError) then
        if EvaluateString(r, r, s1, InfoSyntaxError) then
        begin
          //for i :=
          list.AddStrings(GetRegistryVarMap(s1, False));
          if Skip(')', r, r, InfoSyntaxError) then
          begin
            syntaxCheck := True;
          end;
        end;
    {$ELSE WINDOWS}
      SyntaxCheck := False;
      InfoSyntaxError := 'Only implemented for Windows';
      LogDatei.log(s + ' is only implemented for Windows', LLError);
    {$ENDIF WINDOWS}
    end

    else if (LowerCase(s) = LowerCase('getRegistryVarMap64')) or
      (LowerCase(s) = LowerCase('getRegistryVarMapSysnative')) then
    begin
    {$IFDEF WINDOWS}
      if Skip('(', r, r, InfoSyntaxError) then
        if EvaluateString(r, r, s1, InfoSyntaxError) then
        begin
          //for i :=
          list.AddStrings(GetRegistryVarMap(s1, True));
          if Skip(')', r, r, InfoSyntaxError) then
          begin
            syntaxCheck := True;
          end;
        end;
    {$ELSE WINDOWS}
      SyntaxCheck := False;
      InfoSyntaxError := 'Only implemented for Windows';
      LogDatei.log(s + ' is only implemented for Windows', LLError);
    {$ENDIF WINDOWS}
    end


    else if LowerCase(s) = LowerCase('getSlowInfoMap')
    // reads slowinfocache
    // productkey is the key below ARPCache
    //hash key may be: hasname, installsize, lastused, usagefrequency, binaryname
    then
    begin
    {$IFDEF WINDOWS}
      if Skip('(', r, r, InfoSyntaxError) then
        if EvaluateString(r, r, s1, InfoSyntaxError) then
          if Skip(',', r, r, InfoSyntaxError) then
            if EvaluateString(r, r, s2, InfoSyntaxError) then
              if Skip(')', r, r, InfoSyntaxError) then
              begin
                syntaxCheck := True;
                tmpstr := GetSlowInfoCache(s1, 'hasname', s2);
                if tmpstr <> '' then
                  list.add('hasname=' + tmpstr);
                tmpstr := GetSlowInfoCache(s1, 'installsize', s2);
                if tmpstr <> '' then
                  list.add('installsize=' + tmpstr);
                tmpstr := GetSlowInfoCache(s1, 'lastused', s2);
                if tmpstr <> '' then
                  list.add('lastused=' + tmpstr);
                tmpstr := GetSlowInfoCache(s1, 'usagefrequency', s2);
                if tmpstr <> '' then
                  list.add('usagefrequency=' + tmpstr);
                tmpstr := GetSlowInfoCache(s1, 'binaryname', s2);
                if tmpstr <> '' then
                  list.add('binaryname=' + tmpstr);
                //list.add('installsize=' + GetSlowInfoCache(s1, 'installsize', s2));
                //list.add('lastused=' + GetSlowInfoCache(s1, 'lastused', s2));
                //list.add('usagefrequency=' + GetSlowInfoCache(s1, 'usagefrequency', s2));
                //list.add('binaryname=' + GetSlowInfoCache(s1, 'binaryname', s2));
              end;
    {$ELSE WINDOWS}
      SyntaxCheck := False;
      InfoSyntaxError := 'Only implemented for Windows';
      LogDatei.log(s + ' is only implemented for Windows', LLError);
    {$ENDIF WINDOWS}
    end

    else if LowerCase(s) = LowerCase('getSwauditInfoList')
    // reads slowinfocache
    // productkey is the key below ARPCache
    //hash key may be: hasname, installsize, lastused, usagefrequency, binaryname
    then
    begin
    {$IFDEF WINDOWS}
      if Skip('(', r, r, InfoSyntaxError) then
        if EvaluateString(r, r, s1, InfoSyntaxError) then
          if Skip(',', r, r, InfoSyntaxError) then
            if EvaluateString(r, r, s2, InfoSyntaxError) then
              if Skip(',', r, r, InfoSyntaxError) then
                if EvaluateString(r, r, s3, InfoSyntaxError) then
                  if Skip(')', r, r, InfoSyntaxError) then
                  begin
                    syntaxCheck := True;
                    list1 := TXStringList.Create;
                    getSwauditInfoList(s1, s2, s3, list1);
                    list.AddStrings(list1);
                    list1.Free;
                  end;
    {$ELSE WINDOWS}
      SyntaxCheck := False;
      InfoSyntaxError := 'Only implemented for Windows';
      LogDatei.log(s + ' is only implemented for Windows', LLError);
    {$ENDIF WINDOWS}
    end


    else if LowerCase(s) = LowerCase('getProcesslist') then
    begin
      //   if r = '' then
      begin
        syntaxcheck := True;
        list.AddStrings(getProcessList);
      end;
    end

    else if LowerCase(s) = LowerCase('getProfilesDirList') then
    begin
      //   if r = '' then
      begin
        syntaxcheck := True;
        list.AddStrings(getProfilesDirList);
      end;
    end

    else if LowerCase(s) = LowerCase('getHWBiosInfoMap') then
    begin
      syntaxcheck := True;
      {$IFDEF DARWIN}
      LogDatei.log('Not implemented for macOS - return empty list', LLError);
      list.Clear;
      {$ELSE}
      syntaxcheck := True;
      list.AddStrings(getHwBiosShortlist);
      {$ENDIF}
    end
    else
      InfoSyntaxError := s0 + ' no valid Expressionstr for a string list';



    if syntaxcheck then
    begin
      LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
      LogDatei.log('retrieving strings from ' + logstring, LLDebug2);
      LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
      LogDatei.log_list(list, LLDebug2);
      LogDatei.log('', LLDebug2);
      LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 4;
    end;

  end;

  if syntaxcheck then
    Remaining := r
  else
    Remaining := s0;

  Result := syntaxCheck;

end;

// string functions
function TuibInstScript.EvaluateString
  (const s0: string; var Remaining: string; var StringResult: string;
  var InfoSyntaxError: string): boolean;
  //var
  // NestLevel : integer;
begin
  // nesting level from TuibInstScript
  Result := EvaluateString(s0, Remaining, StringResult, InfoSyntaxError,
    Ifelseendiflevel, inDefFuncIndex);
end;

function TuibInstScript.EvaluateString
  (const s0: string; var Remaining: string; var StringResult: string;
  var InfoSyntaxError: string; var NestLevel: integer;
  const inDefFuncIndex: integer): boolean;

var
  VarIndex: integer = 0;
  a1: integer = 0;
  syntaxCheck: boolean;
  s: string = '';
  sx: string = '';
  r: string = '';
  s1: string = '';
  s2: string = '';
  r1: string = '';
  s3: string = '';
  s4: string = '';
  n1: integer = 0;
  n2: integer = 0;
  s1enc: string = '';
  s2enc: string = '';
  s3enc: string = '';
  s4enc: string = '';
  listindex: integer = 0;
  randomInt: integer = 0;
  errorinfo: string = '';
  intresult: integer = 0;
  list1: TXStringlist;
  slist: TStringList; // if we need a real one
  HostsImage: TuibPatchHostsFile;
  HostsLocation: string = '';
  i: integer = 0;
  ///m, n, rest : Integer;
  itemlist: TXStringList;
 {$IFDEF WINDOWS}
  Regist: TuibRegistry;
  RegType: TuibRegDataType;
 {$ENDIF WINDOWS}
  majorver: TuibNTVersion;
  minorver: DWORD = 0;
  ///programfilesdir, diffx86 : String;
  key0: string = '';
  key: string = '';
  valuename: string = '';
  StartIndentLevel: integer = 0;
  errorOccured: boolean;
  continue: boolean;
  j: integer = 0;
  parameters: array of string;
  omc: TOpsiMethodCall;
  IniFile: TIniFile;
  //tempchar : char;
  //tempansistr : ansistring;
  //tempansistrp : Pansichar;
  runAs: TRunAs;
  WaitSecs: word = 0;
  flag_force64: boolean;
  expr: string = '';
  ident: string = '';
  WaitConditions: TSetWaitConditions;
  ArbeitsSektion: TWorkSection;
  seconds: string = '';
  ActionResult: TSectionResult = 0;
  mydouble: double;
  funcindex: integer = 0;
  funcname: string;
  boolresult: boolean;
  p1, p2, p3, p4: integer;
  tmpstr, tmpstr1, tmpstr2, tmpstr3: string;
  tmpbool, tmpbool1: boolean;

begin
  LogDatei.log_prog('EvaluateString: Parsing: ' + s0 + ' ', LLDebug);
  syntaxCheck := False;
  InfoSyntaxError := '';
  StringResult := '';
  slist := TStringList.Create;
  StartIndentLevel := LogDatei.LogSIndentLevel;

  // defined local function ?
  GetWord(s0, funcname, r, WordDelimiterSet5);
  FuncIndex := definedFunctionNames.IndexOf(LowerCase(funcname));
  // string variable?
  GetWord(s0, s, r, WordDelimiterSet3);
  VarIndex := VarList.IndexOf(LowerCase(s));


  // defined local function ?
  if FuncIndex >= 0 then
  begin
    if not (definedFunctionArray[FuncIndex].datatype = dfpString) then
    begin
      // error
      syntaxCheck := False;
      LogDatei.log('Syntax Error: defined function: ' + funcname +
        ' is not from type string.', LLError);
    end
    else
    begin
      if definedFunctionArray[FuncIndex].call(r, r, NestLevel) then
      begin
        StringResult := definedFunctionArray[FuncIndex].Resultstring;
        syntaxCheck := True;
        //logdatei.log('We leave the defined function: inDefFunc3: '+IntToStr(inDefFunc3),LLInfo);
      end
      else
      begin
        // defined function call failed
        LogDatei.log('Call of defined function: ' + funcname + ' failed', LLError);
        syntaxCheck := False;
      end;
    end;
  end

  // local variable
  else if isVisibleLocalVar(s, FuncIndex) then
  begin
    if not (definedFunctionArray[FuncIndex].getLocalVarDatatype(s) = dfpString) then
    begin
      // type error
      InfoSyntaxError :=
        'Syntax Error: Type mismatch: String expected but the visible local variable: '
        +
        s + ' is from type: ' + osdfParameterTypesNames[
        definedFunctionArray[FuncIndex].getLocalVarDatatype(s)];
    end
    else
    begin
      StringResult := definedFunctionArray[FuncIndex].getLocalVarValueString(s);
      syntaxCheck := True;
    end;
  end

  // string variable?
  else if VarIndex >= 0 then
  begin
    if ValuesList.Count - 1 < VarIndex then
    begin
      InfoSyntaxError := 'The variable: ' + s + ' has no string value';
    end
    else
    begin
      StringResult := ValuesList[VarIndex];
      syntaxCheck := True;
    end;
  end

  // string constant?
  else if (length(s0) > 0) and (s0[1] = '"') then
  begin
    r := copy(s0, 2, length(s0) - 1);
    GetWord(r, StringResult, r, ['"']);
    if skip('"', r, r, InfoSyntaxError) then
      syntaxCheck := True;

  end

  // string constant delimited by "'" ?
  else if (length(s0) > 0) and (s0[1] = '''') then
  begin
    r := copy(s0, 2, length(s0) - 1);
    GetWord(r, StringResult, r, ['''']);
    if skip('''', r, r, InfoSyntaxError) then
      syntaxCheck := True;
  end

  // checking our pseudo function name for retrieving a string avoiding any escape problems of citations marks
  else if LowerCase(s) = LowerCase('EscapeString') then
  begin
    if Skip(':', r, s1, InfoSyntaxError) then
    begin
      StringResult := s1;
      r := '';
      syntaxCheck := True;
    end;
  end

  // string functions ?

  else if (LowerCase(s) = LowerCase('LogLevel')) or
    (LowerCase(s) = LowerCase('getLogLevel')) then
  begin
    StringResult := IntToStr(Logdatei.LogLevel);
    syntaxCheck := True;
  end

  else if (LowerCase(s) = LowerCase('getLastExitCode')) then
  begin
    StringResult := IntToStr(FLastExitCodeOfExe);
    syntaxCheck := True;
  end

  else if (LowerCase(s) = LowerCase('getDiffTimeSec')) then
  begin
    StringResult := IntToStr(SecondsBetween(markedTime, Time));
    syntaxCheck := True;
  end


  else if LowerCase(s) = LowerCase('EnvVar') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          StringResult := ValueOfEnvVar(s1);
          syntaxCheck := True;
        end;
  end

  else if LowerCase(s) = LowerCase('GetOS') then
  begin
    syntaxCheck := True;
    OldNumberOfErrors := LogDatei.NumberOfErrors;
    case GetuibOsType(ErrorInfo) of
      tovNotKnown:
      begin
        LogDatei.log(ErrorInfo, LLError);
        StringResult := 'OS not identified';
      end;
      tovWin16: StringResult := 'Windows_16';
      tovWin95: StringResult := 'Windows_95';
      tovWinNT: StringResult := 'Windows_NT';
      tovLinux: StringResult := 'Linux';
      tovMacOS: StringResult := 'macOS';
    end;

    DiffNumberOfErrors := LogDatei.NumberOfErrors - OldNumberOfErrors;
    FNumberOfErrors := NumberOfErrors + DiffNumberOfErrors;

  end

  else if LowerCase(s) = LowerCase('GetNTVersion') then
  begin
   {$IFDEF WINDOWS}
    syntaxCheck := True;


    OldNumberOfErrors := LogDatei.NumberOfErrors;

    if GetUibOsType(errorinfo) <> tovWinNT then
      StringResult := 'No OS of type Windows NT'
    else
    begin
      OldNumberOfErrors := LogDatei.NumberOfErrors;
      case GetuibNTversion(ErrorInfo) of
        tntverNONE:
        begin
          LogDatei.log(ErrorInfo, LLError);
          StringResult := ErrorInfo;
        end;
        tntverNT3: StringResult := 'NT3';
        tntverNT4: StringResult := 'NT4';
        tntverWIN2K:
        begin
          minorver := GetNTVersionMinor;
          if minorver = 0 then
            StringResult := 'Win2k'
          else if minorver = 1 then
            StringResult := 'WinXP'
          else
            StringResult := 'Win NT 5.' + IntToStr(minorver);
        end;
        tntverWINVISTA:
        begin
          StringResult := 'Windows Vista';
        end;
        tntverWINX: StringResult :=
            'Win NT ' + IntToStr(GetNtVersionMajor) + '.' + IntToStr(minorver);
      end;
      DiffNumberOfErrors := LogDatei.NumberOfErrors - OldNumberOfErrors;
      FNumberOfErrors := NumberOfErrors + DiffNumberOfErrors;
    end;
    {$ELSE}
    StringResult := 'No Windows';
    {$ENDIF WINDOWS}
  end

  else if LowerCase(s) = LowerCase('GetLinuxDistroType') then
  begin
    syntaxcheck := True;
    StringResult := 'no_linux';
    {$IFDEF UNIX}
    StringResult := getLinuxDistroType;
    {$ENDIF LINUX}
  end

  else if LowerCase(s) = LowerCase('GetSystemtype') then
  begin
    syntaxcheck := True;
    if Is64BitSystem then
      StringResult := '64 Bit System'
    else
      StringResult := 'x86 System';
  end

  else if LowerCase(s) = LowerCase('GetUsercontext') then
  begin
    syntaxcheck := True;
    StringResult := usercontext;
  end

  else if LowerCase(s) = LowerCase('GetLoggedInUser') then
  begin
    syntaxcheck := True;
    StringResult := getLoggedInUser;
  end

  else if LowerCase(s) = LowerCase('readVersionFromProfile') then
  begin
    syntaxcheck := True;
    StringResult := readVersionFromProfile;
  end


  else if LowerCase(s) = LowerCase('GetScriptMode') then
  begin
    syntaxcheck := True;
    case scriptMode of
      tsmMachine: StringResult := 'Machine';
      tsmLogin: StringResult := 'Login';
    end;
  end


  else if LowerCase(s) = LowerCase('GetMSVersionInfo') then
  begin
    syntaxCheck := True;

    OldNumberOfErrors := LogDatei.NumberOfErrors;

    if GetUibOsType(errorinfo) <> tovWinNT then
      StringResult := 'Not an OS of type Windows NT'
    else
    begin
      {$IFDEF WINDOWS}
      OldNumberOfErrors := LogDatei.NumberOfErrors;

      majorVer := GetuibNTversion(ErrorInfo);
      if majorVer = tntverNONE then
      begin
        LogDatei.log(ErrorInfo, LLError);
        StringResult := ErrorInfo;
      end
      else
      begin
        StringResult := IntToStr(GetNTVersionMajor) + '.' + IntToStr(GetNTVersionMinor);
      end;

      DiffNumberOfErrors := LogDatei.NumberOfErrors - OldNumberOfErrors;
      FNumberOfErrors := NumberOfErrors + DiffNumberOfErrors;
      {$ENDIF WINDOWS}
    end;
  end

  else if LowerCase(s) = LowerCase('GetMacosVersionInfo') then
  begin
    syntaxCheck := True;

    OldNumberOfErrors := LogDatei.NumberOfErrors;

    if GetUibOsType(errorinfo) <> tovMacos then
      StringResult := 'Not an OS of type macOS'
    else
    begin
      {$IFDEF DARWIN}
      StringResult := GetMacosVersionInfo;
      {$ENDIF DARWIN}
    end;
  end

  else if LowerCase(s) = LowerCase('IniVar') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          LogDatei.log('The Function IniVar is deprecated ! use GetProductProperty',
            LLWarning);
          StringResult := ProductvarsForPC.Values[s1];
          syntaxCheck := True;
        end;
  end

  else if (LowerCase(s) = LowerCase('GetProductProperty')) or
    (LowerCase(s) = LowerCase('GetConfidentialProductProperty')) then
  begin
    try
      if Skip('(', r, r, InfoSyntaxError) then
        if EvaluateString(r, r, s1, InfoSyntaxError) then
          if Skip(',', r, r, InfoSyntaxError) then
            if EvaluateString(r, r, s2, InfoSyntaxError) then
              if Skip(')', r, r, InfoSyntaxError) then
              begin
                syntaxCheck := True;

                // try to get from lookup table
                if ProductvarsForPC.indexOfName(s1) = -1 then
                begin
                  LogDatei.log(
                    'Property not existing in GetProductProperty - using default',
                    LLWarning);
                  StringResult := s2;
                end
                else
                begin
                  // get the property value from the looup table
                  StringResult := ProductvarsForPC.Values[s1];
                end;
              end;
      if (LowerCase(s) = LowerCase('GetConfidentialProductProperty')) then
        LogDatei.AddToConfidentials(StringResult);
    except
      on E: Exception do
      begin
        Logdatei.log('Exception in GetProductProperty: ' + E.Message +
          ' - using default', LLError);
        StringResult := s2;
      end;
    end;
  end


  else if LowerCase(s) = LowerCase('GetHostsName') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if Skip('"', r, r, InfoSyntaxError) then
      begin
        GetWord(r, s1, r, ['"']);

        if Skip('"', r, r, InfoSyntaxError) then
          if Skip(')', r, r, InfoSyntaxError) then
          begin
            syntaxCheck := True;
            //StringResult := GetHostByName(s1);

            HostsImage := TuibPatchHostsFile.Create;
            Logdatei.LogLevel := LogLevel;
            Logdatei.LogSIndentLevel := 0;

            OldNumberOfErrors := LogDatei.NumberOfErrors;
            OldNumberOfWarnings := LogDatei.NumberOfWarnings;

            HostsLocation := DefaultHosts;

            if not FileExists(HostsLocation) then
            begin
              LogDatei.log('Error: Hosts ' + HostsLocation + ' not found', LLError);
            end
            else
            begin
              HostsImage.LoadFromFile(HostsLocation);
              HostsImage.GetHostname(s1, StringResult);
            end;
            HostsImage.Free;
            HostsImage := nil;

            DiffNumberOfErrors := LogDatei.NumberOfErrors - OldNumberOfErrors;
            DiffNumberOfWarnings := LogDatei.NumberOfWarnings - OldNumberOfWarnings;
            FNumberOfErrors := NumberOfErrors + DiffNumberOfErrors;
            FNumberOfWarnings := NumberOfWarnings + DiffNumberOfWarnings;
          end;
      end;
  end

  else if LowerCase(s) = LowerCase('GetHostsAddr') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          syntaxCheck := True;

          HostsImage := TuibPatchHostsFile.Create;
          Logdatei.LogLevel := LogLevel;
          Logdatei.LogSIndentLevel := 0;

          OldNumberOfErrors := LogDatei.NumberOfErrors;
          OldNumberOfWarnings := LogDatei.NumberOfWarnings;

          HostsLocation := DefaultHosts;

          if not FileExists(HostsLocation) then
          begin
            Logdatei.log('Hosts ' + HostsLocation + ' not found', LLerror);
          end
          else
          begin
            HostsImage.LoadFromFile(HostsLocation);
            HostsImage.GetAddress(s1, StringResult);
          end;
          HostsImage.Free;
          HostsImage := nil;

          DiffNumberOfErrors := LogDatei.NumberOfErrors - OldNumberOfErrors;
          DiffNumberOfWarnings := LogDatei.NumberOfWarnings - OldNumberOfWarnings;
          FNumberOfErrors := NumberOfErrors + DiffNumberOfErrors;
          FNumberOfWarnings := NumberOfWarnings + DiffNumberOfWarnings;
        end;
  end


  else if LowerCase(s) = LowerCase('GetMyIpByTarget') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          syntaxCheck := True;
     {$IFDEF WIN64}
          StringResult := '';
          Logdatei.log('Error: Not implemented for winst64', LLerror);
     {$ELSE}
          if isip(s1) then
            StringResult := getMyIpByTarget(s1)
          else
          begin
            StringResult := '';
            Logdatei.log('Error: ' + s1 + ' is not a valid IPv4 Address', LLerror);
          end;
     {$ENDIF}
        end;
  end



  else if LowerCase(s) = LowerCase('GetIpByName') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          syntaxCheck := True;
          if GetIPFromHost(s1, s2, s3) then
            StringResult := s2
          else
          begin
       {$IFDEF LINUX}
            //StringResult :=  getCommandResult('resolveip -s '+s1);
            StringResult := getCommandResult('getent hosts ' + s1);
            stringsplitByWhiteSpace(StringResult, slist);
            if slist.Count > 0 then
              StringResult := slist.Strings[0]
            else
              StringResult := '';
            if not IsIP(StringResult) then
            begin
              LogDatei.log('Warning: no valid IP found for: ' + s1, LLwarning);
              StringResult := '';
            end;

       {$ENDIF LINUX}
       {$IFDEF DARWIN}
            StringResult := getCommandResult('dig +short -x  ' + s1);
       {$ENDIF LINUX}
       {$IFDEF WINDOWS}
            StringResult := '';
            Logdatei.log('Error: ' + s3, LLerror);
       {$ENDIF WINDOWS}
          end;
        end;
  end

  else if LowerCase(s) = LowerCase('getDefaultNetmaskByIP4adr') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          if getDefaultNetmaskByIP4adr(s1) = '' then
          begin
            StringResult := '';
            Logdatei.log('Error: ' + s1 + ' is not a valid IPv4 Address', LLerror);
          end
          else
          begin
            syntaxCheck := True;
            StringResult := getDefaultNetmaskByIP4adr(s1);
          end;
        end;
  end

  else if LowerCase(s) = LowerCase('GetIni') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip('[', r, r, InfoSyntaxError) then
        begin
          s1 := ExpandFileName(s1);
          Inifile := TInifile.Create(s1);
          GetWord(r, s1, r, WordDelimiterSet0);
          if Skip(']', r, r, InfoSyntaxError) then
          begin
            GetWord(r, sx, r, WordDelimiterSet1);
            if Skip(')', r, r, InfoSyntaxError) then
            begin
              StringResult := Inifile.ReadString(s1, sx, '' (* 'ERROR' *));
              syntaxCheck := True;
            end;
          end;
          Inifile.Free;
          Inifile := nil;
        end;
  end

  else if LowerCase(s) = LowerCase('GetValueFromInifile') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(',', r, r, InfoSyntaxError) then
          if EvaluateString(r, r, s2, InfoSyntaxError) then
            if Skip(',', r, r, InfoSyntaxError) then
              if EvaluateString(r, r, s3, InfoSyntaxError) then
                if Skip(',', r, r, InfoSyntaxError) then
                  if EvaluateString(r, r, s4, InfoSyntaxError) then
                    if Skip(')', r, r, InfoSyntaxError) then
                    begin
                      syntaxCheck := True;
                      try
                        s1 := ExpandFileName(s1);
                        Inifile := TInifile.Create(s1);

                        LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
                        LogDatei.log
                        ('    reading the value to the key "' + s3 +
                          '" in section "' + s2 + '"  from inifile  "' +
                          s1 + '", default value  "' + s4 + '"',
                          LevelComplete);
                        s2enc := UTF8ToWinCP(s2);
                        s3enc := UTF8ToWinCP(s3);
                        s4enc := UTF8ToWinCP(s4);
                        StringResult := Inifile.ReadString(s2enc, s3enc, s4enc);
                        StringResult := WinCPToUTF8(StringResult);
                        LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;

                        Inifile.Free;
                        Inifile := nil;
                      except
                        on e: Exception do
                        begin
                          LogDatei.log('Error in creating inifile "' +
                            s1 + '", message: "' + e.Message + '"', LevelWarnings);
                          StringResult := s4;
                        end;
                      end;

                    end;
  end


  else if LowerCase(s) = LowerCase('Lower') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          syntaxCheck := True;
          StringResult := lowercase(s1);
        end;
  end

  else if LowerCase(s) = LowerCase('Upper') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          syntaxCheck := True;
          StringResult := uppercase(s1);
        end;
  end

  else if LowerCase(s) = LowerCase('Trim') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          syntaxCheck := True;
          StringResult := trim(s1);
        end;
  end

  else if LowerCase(s) = LowerCase('which') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          syntaxCheck := True;
          if not which(s1, StringResult) then
            StringResult := '';
        end;
  end

  else if LowerCase(s) = LowerCase('replaceOpsiConstants') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          syntaxCheck := True;
          list1 := TXStringList.Create;
          tmpstr := s1;
          for i := 1 to ConstList.Count do
          begin
            if list1.replaceInLine(tmpstr, Constlist.Strings[i - 1],
              ConstValuesList.Strings[i - 1], False, tmpstr1) then
              tmpstr := tmpstr1;
          end;
          StringResult := tmpstr;
          list1.Free;
        end;
  end


  else if LowerCase(s) = LowerCase('asConfidential') then
  begin
    // backup and set loglevel to warning
    // get the input
    // make it confidential
    // give it to output
    // restore loglevel
    if Skip('(', r, r, InfoSyntaxError) then
    begin
      p1 := logdatei.LogLevel;
      try
        logdatei.LogLevel := LLWarning;
        if EvaluateString(r, r, s1, InfoSyntaxError) then
        begin
          if Skip(')', r, r, InfoSyntaxError) then
          begin
            syntaxCheck := True;
            logdatei.AddToConfidentials(s1);
            StringResult := s1;
          end;
        end;
      finally
        logdatei.LogLevel := p1;
      end;
    end;
  end


  else if LowerCase(s) = LowerCase('calculate') then
  begin
    syntaxCheck := False;
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          syntaxCheck := True;
          if not opsicalc(s1, StringResult) then
          begin
            LogDatei.log('Error in calculate : could not calculate : ' +
              s1 + ' ; ' + StringResult, LLError);
            StringResult := '';
          end;
        end;
    if not syntaxCheck then
    begin
      LogDatei.log('Error in calculate : could not calculate : ' +
        s1 + ' ; ' + InfoSyntaxError, LLError);
    end;
  end


  else if LowerCase(s) = LowerCase('strLength') then
  begin
    syntaxCheck := False;
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          syntaxCheck := True;
          StringResult := IntToStr(length(s1));
        end;
    if not syntaxCheck then
    begin
      LogDatei.log('Error in strLength : could not get length of : ' +
        s1 + ' ; ' + InfoSyntaxError, LLError);
    end;
  end

  else if LowerCase(s) = LowerCase('strLoadTextFile') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
      begin
        try
          list1 := TXStringList.Create;
          s1 := ExpandFileName(s1);
          list1.loadfromfile(s1);
          if list1.Count > 0 then
            StringResult := reencode(list1.Strings[0], 'system')
          else
            StringResult := '';
          list1.Free;
        except
          on e: Exception do
          begin
            LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
            LogDatei.log('Error on loading file: ' + e.message, LLError);
            FNumberOfErrors := FNumberOfErrors + 1;
            LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
          end
        end;
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          syntaxCheck := True;
        end;
      end;
  end

  else if LowerCase(s) = LowerCase('strLoadTextFileWithEncoding') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(',', r, r, InfoSyntaxError) then
          if EvaluateString(r, r, s2, InfoSyntaxError) then
            if Skip(')', r, r, InfoSyntaxError) then
            begin
              syntaxCheck := True;
              try
                list1 := TXStringList.Create;
                s1 := ExpandFileName(s1);
                list1.loadfromfile(s1);
                if list1.Count > 0 then
                  StringResult := reencode(list1.Strings[0], s2)
                else
                  StringResult := '';
                list1.Free;
              except
                on e: Exception do
                begin
                  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
                  LogDatei.log('Error on loading file: ' + e.message, LLError);
                  FNumberOfErrors := FNumberOfErrors + 1;
                  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
                end
              end;
            end;
  end


  else if LowerCase(s) = LowerCase('strPos') then
  begin
    syntaxCheck := False;
    if Skip('(', r, r, InfoSyntaxError) and EvaluateString(r, r, s1,
      InfoSyntaxError) and Skip(',', r, r, InfoSyntaxError) and
      EvaluateString(r, r, s2, InfoSyntaxError) and Skip(')', r, r, InfoSyntaxError) then
    begin
      syntaxCheck := True;
      StringResult := IntToStr(pos(s2, s1));
    end;
    if not syntaxCheck then
    begin
      LogDatei.log('Error in strPos : could not get pos of : ' + s2 +
        'in : ' + s1 + ' ; ' + InfoSyntaxError, LLError);
    end;
  end

  else if LowerCase(s) = LowerCase('strPart') then
  begin
    syntaxCheck := False;
    if Skip('(', r, r, InfoSyntaxError) and EvaluateString(r, r, s1,
      InfoSyntaxError) and Skip(',', r, r, InfoSyntaxError) and
      EvaluateString(r, r, s2, InfoSyntaxError) and Skip(',', r, r, InfoSyntaxError) and
      EvaluateString(r, r, s3, InfoSyntaxError) and Skip(')', r, r, InfoSyntaxError) then
    begin
      syntaxCheck := True;
      try
        n1 := StrToInt(LowerCase(trim(s2)));
        try
          n2 := StrToInt(LowerCase(trim(s3)));
          StringResult := copy(s1, n1, n2);
          LogDatei.log('strPart from: ' + s1 + ' start: ' +
            IntToStr(n1) + ' number: ' + IntToStr(n2) + ' gives: >' + stringresult + '<',
            LLDebug2);
        except
          LogDatei.log('Error: ' + s2 + ' has no Integer format', LLerror)
        end;
      except
        LogDatei.log('Error: ' + s3 + ' has no Integer format', LLerror)
      end;
    end;
    if not syntaxCheck then
    begin
      LogDatei.log('Error in strPart : could not get part of : ' +
        s1 + ' ; ' + InfoSyntaxError, LLError);
    end;
  end

  else if LowerCase(s) = LowerCase('randomIntStr') then
  begin
    syntaxCheck := False;
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          syntaxCheck := True;
          //LogDatei.log ('incStr for : ' + s1 , LLDebug2);
          try
            intresult := StrToInt(s1);
            //LogDatei.log ('incStr for : ' + IntToStr(intresult) , LLDebug2);
            //Randomize;
            StringResult := IntToStr(random(intresult));
            //LogDatei.log ('incStr for : ' + IntToStr(intresult) , LLDebug2);
            //StringResult := IntToStr(intresult);
          except
            on E: Exception do
              LogDatei.log('Error in randomIntStr : perhaps could not convert to int : '
                + s1 + ' Error : ' + E.Message, LLError);
          end;
        end;
    if not syntaxCheck then
    begin
      LogDatei.log('Error in randomIntStr : could not make random for : ' +
        s1 + ' ; ' + InfoSyntaxError, LLError);
    end;
  end
(*
 else if LowerCase (s) = LowerCase ('decStr') then
 begin
  syntaxCheck := false;
  if Skip ('(', r, r, InfoSyntaxError)
  then
   if EvaluateString (r, r, s1, InfoSyntaxError)
   then
     if Skip (')', r,r, InfoSyntaxError)
     then
     Begin
         syntaxCheck := true;
         LogDatei.log ('decStr for : ' + s1 , LLDebug2);
         try
           intresult := StrToInt(s1);
           dec(intresult);
           StringResult := IntToStr(intresult);
         except
           on E:Exception do
           LogDatei.log ('Error in decStr : could not dec : ' + s1 + ' Error : '+E.Message, LLError);
         end;
     End;
  if  not syntaxCheck then
  begin
   LogDatei.log ('Error in decStr : could not get dec : ' + s1 + ' ; '+InfoSyntaxError, LLError);
  end;
 end
*)

  else if LowerCase(s) = LowerCase('LangCodeByHex') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          syntaxCheck := True;
          s1 := LowerCase(trim(s1));
          if pos('$', s1) = 1 then
            s1 := copy(s1, 2, length(s1));
          if pos('0x', s1) = 1 then
            s1 := copy(s1, 3, length(s1));
          try
            StringResult := getlangcodeByHexvalueStr('0x' + s1);
          except
            LogDatei.log('Error: ' + s1 + ' has no Integer format', LLError)
          end;
        end;
  end

  else if LowerCase(s) = LowerCase('md5sumFromFile') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          syntaxCheck := True;
          // do not lowercase this - it is a path
          s1 := trim(s1);
          StringResult := '';
          if not FileExistsUTF8(ExpandFileName(s1)) then
          begin
            LogDatei.log('Error: md5sumFromFile: ' + ExpandFileName(s1) +
              ' is no valid file', LLError);
          end
          else
            try
              StringResult := Copy(lowerCase(md5fromFile(ExpandFileName(s1))), 0, 32);
            except
              LogDatei.log('Error: Exception at md5sumFromFile: ' +
                ExpandFileName(s1), LLError)
            end;
        end;
  end

  else if LowerCase(s) = LowerCase('shellcall') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          syntaxCheck := True;
          StringResult := '';
          try
            //LogDatei.log ('Executing0 ' + s1, LLInfo);
            execShellCall(s1, 'sysnative', 0, True);
            StringResult := IntToStr(FLastExitCodeOfExe);
          except
            on e: Exception do
            begin
              LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
              LogDatei.log('Error executing :' + s1 + ' : ' + e.message,
                LLError);
              FNumberOfErrors := FNumberOfErrors + 1;
              LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
            end
          end;
        end;
  end

  else if LowerCase(s) = LowerCase('powershellcall') then
  begin
  {$IFDEF UNIX}
    LogDatei.log('Error powershellcall not implemented on Linux ', LLError);
  {$ENDIF Linux}
  {$IFDEF WINDOWS}
    s2 := '';
    s3 := '';
    tmpstr2 := '';
    tmpbool := True; // sysnative
    tmpbool1 := True; // handle execution policy
    syntaxCheck := False;
    StringResult := '';
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, tmpstr, s1, InfoSyntaxError)
      // next after ',' or ')'
      then
        if Skip(',', tmpstr, tmpstr1, tmpstr3) then
          if EvaluateString(tmpstr1, tmpstr2, s2, tmpstr3) then;
    if s2 = '' then
    begin
      // only one parameter
      if Skip(')', tmpstr, r, InfoSyntaxError) then
      begin
        syntaxCheck := True;
        s2 := 'sysnative';
      end;
    end
    else
    begin
      // got second parameter
      tmpbool := True;
      if lowercase(s2) = '32bit' then
        tmpbool := False
      else if lowercase(s2) = '64bit' then
        tmpbool := True
      else if lowercase(s2) = 'sysnative' then
        tmpbool := True
      else
      begin
        InfoSyntaxError := 'Error: unknown parameter: ' + s2 +
          ' expected one of 32bit,64bit,sysnative - fall back to sysnative';
        syntaxCheck := False;
      end;
      // three parameter ?
      if Skip(',', tmpstr2, tmpstr1, tmpstr3) then
      begin
        if EvaluateString(tmpstr1, tmpstr2, s3, tmpstr3) then
        begin
          // got third parameter
          if Skip(')', tmpstr2, r, InfoSyntaxError) then
          begin
            if TryStrToBool(s3, tmpbool1) then
              syntaxCheck := True
            else
            begin
              syntaxCheck := False;
              InfoSyntaxError :=
                'Error: boolean string (true/false) expected but got: ' + s3;
            end;
          end;
        end;
      end
      else
      if Skip(')', tmpstr2, r, InfoSyntaxError) then
      begin
        // two parameter
        syntaxCheck := True;
      end;
    end;
    if syntaxCheck then
    begin
      try
        execPowershellCall(s1, s2, 0, True, False, tmpbool1);
        StringResult := IntToStr(FLastExitCodeOfExe);
      except
        on e: Exception do
        begin
          LogDatei.log('Error executing :' + s1 + ' : with powershell: ' + e.message,
            LLError);
        end
      end;
    end;
   {$ENDIF WINDOWS}
  end



  else if LowerCase(s) = LowerCase('processCall') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          syntaxCheck := True;
          StringResult := '';
          ArbeitsSektion := TWorkSection.Create(0, nil);
          ArbeitsSektion.Text := s1;
          tmpstr := r;
          //////////////////
          begin
            //{$IFDEF WINDOWS}
            runAs := traInvoker;
               {$IFDEF WIN32}
            opsiSetupAdmin_runElevated := False;
               {$ENDIF WIN32}
            WaitSecs := 0;
            flag_force64 := False;
            GetWord(r, expr, r, WordDelimiterSet0);
            SyntaxCheck := True;

            ident := '';
            WaitConditions := [ttpWaitOnTerminate];

            while SyntaxCheck and (length(expr) > 0) do
            begin
                 {$IFDEF WIN32}
              if LowerCase(expr) = LowerCase(ParameterRunAsAdmin) then
              begin
                runAs := traAdmin;
              end

              else if LowerCase(expr) = LowerCase(ParameterRunAsAdmin1) then
              begin
                runAs := traAdmin;
              end

              else if LowerCase(expr) = LowerCase(ParameterRunAsAdmin2) then
              begin
                runAs := traAdminProfile;
              end

              else if LowerCase(expr) = LowerCase(ParameterRunAsAdmin3) then
              begin
                runAs := traAdminProfileImpersonate;
              end

              else if LowerCase(expr) = LowerCase(ParameterRunAsAdmin4) then
              begin
                runAs := traAdminProfileImpersonateExplorer;
              end

              else if LowerCase(expr) = LowerCase(ParameterRunAsInvoker)
              then
              begin
                runAs := traInvoker;
              end

              else if LowerCase(expr) = LowerCase(ParameterRunElevated) then
              begin
                opsiSetupAdmin_runElevated := True;
              end

              else if LowerCase(expr) = LowerCase(ParameterRunAsLoggedOnUser)
              then
              begin
                if runLoginScripts then
                  runAs := traLoggedOnUser
                else
                  LogDatei.log(
                    'Warning: Not in UserLoginScript mode: /RunAsLoggedinUser ignored',
                    LLWarning);
              end

              else if (LowerCase(expr) = LowerCase(Parameter_64Bit)) and
                Is64BitSystem then
              begin
                flag_force64 := True;
              end

              else if LowerCase(expr) = LowerCase(Parameter_32Bit) then
              begin
                flag_force64 := False;
              end

              else if (LowerCase(expr) = LowerCase(Parameter_SysNative)) and
                Is64BitSystem then
              begin
                flag_force64 := True;
              end

              else if (LowerCase(expr) = LowerCase(Parameter_SysNative)) and
                (not Is64BitSystem) then
              begin
                flag_force64 := False;
              end

              else
{$ENDIF WIN32}
              if LowerCase(expr) = LowerCase(ParameterWaitSecs) then
              begin
                WaitConditions := WaitConditions + [ttpWaitTime];
                // WaitConditions := WaitConditions - [ttpWaitOnTerminate];

                GetWord(r, expr, r, WordDelimiterSet0);
                try
                  WaitSecs := StrToInt64(expr);
                except
                  on EConvertError do
                  begin
                    InfoSyntaxError := 'Integer number expected';
                    SyntaxCheck := False;
                  end
                end;
              end

                 {$IFDEF WIN32}
              else if LowerCase(expr) = LowerCase(ParameterCloseOnWindow)
              then
              begin
                runAs := traInvoker;
                WaitConditions := WaitConditions + [ttpWaitForWindowAppearing];

                if EvaluateString(r, r, ident, InfoSyntaxError) then
                begin
                  if r <> '' then
                  begin
                    SyntaxCheck := False;
                    InfoSyntaxError := 'not expected chars after "';
                  end;
                end
                else
                  SyntaxCheck := False;
              end


              else if LowerCase(expr) = LowerCase(ParameterCloseBehindWindow)
              then
              begin
                runAs := traInvoker;
                WaitConditions := WaitConditions + [ttpWaitForWindowVanished];

                if EvaluateString(r, r, ident, InfoSyntaxError) then
                begin
                  if r <> '' then
                  begin
                    SyntaxCheck := False;
                    InfoSyntaxError := 'unexpected characters after "';
                  end;
                end
                else
                  SyntaxCheck := False;
              end

                 {$ENDIF WIN32}

              else if LowerCase(expr) = LowerCase(ParameterWaitProcessTimeoutSecs)
              then
              begin
                WaitConditions := WaitConditions - [ttpWaitTime];
                WaitConditions := WaitConditions + [ttpWaitTimeout];

                GetWord(r, expr, r, WordDelimiterSet0);
                try
                  WaitSecs := StrToInt64(expr);
                except
                  on EConvertError do
                  begin
                    try
                      EvaluateString(expr, expr, seconds, InfoSyntaxError);
                      WaitSecs := StrToInt64(seconds);
                    except
                      on EConvertError do
                      begin
                        InfoSyntaxError := 'Integer number expected ' + InfoSyntaxError;
                        SyntaxCheck := False;
                      end;
                    end;
                  end
                end;
              end

              else if LowerCase(expr) = LowerCase(ParameterWaitForProcessEnding)
              then
              begin
                WaitConditions := WaitConditions + [ttpWaitForProcessEnding];
(*
                   GetWord (Remaining, ident, Remaining, WordDelimiterSet0);
                   if ident = '' then
                   begin
                     SyntaxCheck := false;
                     InfoSyntaxError := 'process name expected';
                   end;
*)
                if not EvaluateString(r, r, ident, InfoSyntaxError) then
                  SyntaxCheck := False;
(*
                   then
                   Begin
                     if Remaining <> ''
                     then
                     Begin
                       SyntaxCheck := false;
                       InfoSyntaxError := 'unexpected chars after "';
                     End
                   End
                   else
                     SyntaxCheck := false;
*)
              end

              else if UpperCase(expr) = UpperCase(ParameterDontWait) then
              begin
                WaitConditions := WaitConditions - [ttpWaitOnTerminate];
                WaitConditions := WaitConditions - [ttpWaitTimeout];
              end

              else if UpperCase(expr) = UpperCase(ParameterWaitOnTerminate)
              then
              begin
                WaitConditions := WaitConditions + [ttpWaitOnTerminate];
              end



              else
              begin
                SyntaxCheck := False;
                InfoSyntaxError := expr + ' not legal WinBatch parameter';
              end;

              GetWord(r, expr, r, WordDelimiterSet0);
            end;

            if SyntaxCheck then
            begin
              LogDatei.log('Executing: ' + s + '(' + s1 + ') ' + tmpstr, LLNotice);
              ActionResult :=
                execWinBatch(ArbeitsSektion, r, WaitConditions, Ident,
                WaitSecs, runAs, flag_force64);
              StringResult := IntToStr(FLastExitCodeOfExe);
            end
            else
              ActionResult :=
                reportError(ArbeitsSektion, i, 'Expressionstr', InfoSyntaxError);
               (*
               {$ELSE WINDOWS}
                logdatei.log('Winbatch sections are not implemented for Linux right now', LLWarning);
                {$ENDIF WINDOWS}
                *)
          end;
          ArbeitsSektion.Free;
          ///////////////////
        end;
  end

  else if LowerCase(s) = LowerCase('boolToString') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
    begin
      StringResult := '';
      // backup given expr
      r1 := r;
      if EvaluateString(r1, r, s1, InfoSyntaxError) then
      begin
        try
          boolresult := strtobool(s1);
          if Skip(')', r, r, InfoSyntaxError) then
          begin
            syntaxCheck := True;
            try
              StringResult := BoolToStr(boolresult, True);
            except
              LogDatei.log('Error: boolToString: string expression' +
                s1 + ' has no boolean value', LLError);
              StringResult := '';
            end;
          end;
        except
          LogDatei.log('Error: boolToString: string expression' + s1 +
            ' has no boolean value', LLDebug2);
          StringResult := '';
        end;
      end;
      if StringResult = '' then
      begin
        // r1 is not a standard boolean string representation
        // let us try boolean string expression
        intresult := 0;
        if EvaluateBoolean(r1, r, boolresult, intresult, InfoSyntaxError) then
        begin
          if Skip(')', r, r, InfoSyntaxError) then
          begin
            syntaxCheck := True;
            try
              StringResult := BoolToStr(boolresult, True);
            except
              LogDatei.log('Error: boolToString: string expression' +
                r + ' has no boolean value', LLError);
              StringResult := '';
            end;
          end;
        end
        else
        begin
          // EvaluateBoolean = false
          LogDatei.log('Error: boolToString: string expression' + r +
            ' has no boolean value', LLError);
          StringResult := '';
        end;
      end;
    end;
  end


  else if LowerCase(s) = LowerCase('HexStrToDecStr') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          syntaxCheck := True;
          s1 := LowerCase(trim(s1));
          if pos('$', s1) = 1 then
            s1 := copy(s1, 2, length(s1));
          if pos('0x', s1) = 1 then
            s1 := copy(s1, 3, length(s1));
          try
            StringResult := IntToStr(StrToInt('$' + s1));
          except
            LogDatei.log('Error: ' + s1 + ' has no Integer format', LLError)
          end;
        end;
  end

  else if LowerCase(s) = LowerCase('DecStrToHexStr') then
  begin
    if Skip('(', r, r, InfoSyntaxError) and EvaluateString(r, r, s1,
      InfoSyntaxError) and Skip(',', r, r, InfoSyntaxError) and
      EvaluateString(r, r, s2, InfoSyntaxError) and Skip(')', r, r, InfoSyntaxError) then
    begin
      syntaxCheck := True;
      s1 := LowerCase(trim(s1));
      try
        n2 := StrToInt(LowerCase(trim(s2)));
        try
          n1 := StrToInt(s1);
          StringResult := IntToHex(n1, n2);
        except
          LogDatei.log('Error: ' + s1 + ' has no Integer format', LLerror)
        end;
      except
        LogDatei.log('Error: ' + s2 + ' has no Integer format', LLerror)
      end;
    end;
  end

  else if LowerCase(s) = LowerCase('encryptStringBlow') then
  begin
    if Skip('(', r, r, InfoSyntaxError) and EvaluateString(r, r, s1,
      InfoSyntaxError) and Skip(',', r, r, InfoSyntaxError) and
      EvaluateString(r, r, s2, InfoSyntaxError) and Skip(')', r, r, InfoSyntaxError) then
    begin
      syntaxCheck := True;
      try
        StringResult := encryptStringBlow(s1, s2);
      except
        LogDatei.log('Error: Exception in encrypt_hex_blow: "' + s1 +
          '","' + s2 + '"', LLerror)
      end;
    end;
  end

  else if LowerCase(s) = LowerCase('decryptStringBlow') then
  begin
    if Skip('(', r, r, InfoSyntaxError) and EvaluateString(r, r, s1,
      InfoSyntaxError) and Skip(',', r, r, InfoSyntaxError) and
      EvaluateString(r, r, s2, InfoSyntaxError) and Skip(')', r, r, InfoSyntaxError) then
    begin
      syntaxCheck := True;
      try
        StringResult := decryptStringBlow(s1, s2);
      except
        LogDatei.log('Error: Exception in decrypt_hex_blow: "' + s1 +
          '","' + s2 + '"', LLerror)
      end;
    end;
  end


  else if LowerCase(s) = LowerCase('base64EncodeStr') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          syntaxCheck := True;
          //StringResult := B64Encode(s1);
          StringResult := EncodeStringBase64(s1);
        end;
  end

  else if LowerCase(s) = LowerCase('base64DecodeStr') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          syntaxCheck := True;
          //StringResult := B64Decode(s1);
          StringResult := DecodeStringBase64(s1);
        end;
  end

  else if LowerCase(s) = LowerCase('GetShortWinPathName') then
  begin
  {$IFDEF WINDOWS}
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          syntaxCheck := True;
          StringResult := GetShortWinPathName(s1);
        end;
  {$ELSE WINDOWS}
    StringResult := '';
    LogDatei.log('Error: GetShortWinPathName only impemented for Windows.', LLerror);
  {$ENDIF WINDOWS}
  end

  else if LowerCase(s) = LowerCase('Convert2JsonStr') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          syntaxCheck := True;
          str2jsonstr(s1, s2);
          if s2 = 'ok' then
            StringResult := s1
          else
          begin
            StringResult := '';
            LogDatei.log('Error: ' + s2, LLerror);
          end;
        end;
  end

  else if LowerCase(s) = LowerCase('jsonAsArrayCountElements') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          syntaxCheck := True;
          try
            StringResult := IntToStr(jsonAsArrayCountElements(s1));
          except
            StringResult := '';
            LogDatei.log('Error: Exception at jsonAsArrayCountElements with: "' +
              s1 + '"', LLerror);
          end;
        end;
  end

  else if LowerCase(s) = LowerCase('jsonAsObjectCountElements') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          syntaxCheck := True;
          try
            StringResult := IntToStr(jsonAsObjectCountElements(s1));
          except
            StringResult := '';
            LogDatei.log('Error: Exception at jsonAsObjectCountElements with: "' +
              s1 + '"', LLerror);
          end;
        end;
  end

  else if LowerCase(s) = LowerCase('jsonAsArrayGetElementByIndex') then
  begin
    if Skip('(', r, r, InfoSyntaxError) and EvaluateString(r, r, s1,
      InfoSyntaxError) and Skip(',', r, r, InfoSyntaxError) and
      EvaluateString(r, r, s2, InfoSyntaxError) and Skip(')', r, r, InfoSyntaxError) then
    begin
      syntaxCheck := True;
      try
        StringResult := '';
        if jsonAsArrayGetElementByIndex(s1, StrToInt(s2), s3) then
          StringResult := s3
        else
          LogDatei.log('Error at jsonAsArrayGetElementByIndex with: "' +
            s1 + '","' + s2 + '"', LLerror);
      except
        StringResult := '';
        LogDatei.log('Error: Exception at jsonAsArrayGetElementByIndex with: "' +
          s1 + '","' + s2 + '"', LLerror);
      end;
    end;
  end

  else if LowerCase(s) = LowerCase('jsonAsArrayDeleteObjectByIndex') then
  begin
    if Skip('(', r, r, InfoSyntaxError) and EvaluateString(r, r, s1,
      InfoSyntaxError) and Skip(',', r, r, InfoSyntaxError) and
      EvaluateString(r, r, s2, InfoSyntaxError) and Skip(')', r, r, InfoSyntaxError) then
    begin
      syntaxCheck := True;
      try
        StringResult := '';
        if jsonAsArrayDeleteObjectByIndex(s1, StrToInt(s2)) then
          StringResult := s1
        else
          LogDatei.log('Error at jsonAsArrayDeleteObjectByIndex with: "' +
            s1 + '","' + s2 + '"', LLerror);
      except
        StringResult := '';
        LogDatei.log('Error: Exception at jsonAsArrayDeleteObjectByIndex with: "' +
          s1 + '","' + s2 + '"', LLerror);
      end;
    end;
  end

  else if LowerCase(s) = LowerCase('jsonAsObjectDeleteByKey') then
  begin
    if Skip('(', r, r, InfoSyntaxError) and EvaluateString(r, r, s1,
      InfoSyntaxError) and Skip(',', r, r, InfoSyntaxError) and
      EvaluateString(r, r, s2, InfoSyntaxError) and Skip(')', r, r, InfoSyntaxError) then
    begin
      syntaxCheck := True;
      try
        StringResult := '';
        if jsonAsObjectDeleteByKey(s1, s2) then
          StringResult := s1
        else
          LogDatei.log('Error at jsonAsObjectDeleteByKey with: "' +
            s1 + '","' + s2 + '"', LLerror);
      except
        StringResult := '';
        LogDatei.log('Error: Exception at jsonAsObjectDeleteByKey with: "' +
          s1 + '","' + s2 + '"', LLerror);
      end;
    end;
  end


  else if LowerCase(s) = LowerCase('jsonAsObjectGetValueByKey') then
  begin
    if Skip('(', r, r, InfoSyntaxError) and EvaluateString(r, r, s1,
      InfoSyntaxError) and Skip(',', r, r, InfoSyntaxError) and
      EvaluateString(r, r, s2, InfoSyntaxError) and Skip(')', r, r, InfoSyntaxError) then
    begin
      syntaxCheck := True;
      try
        StringResult := '';
        s3 := '';
        if jsonAsObjectGetValueByKey(s1, s2, s3) then
          StringResult := s3
        else
          LogDatei.log('Nothing found at jsonAsObjectGetValueByKey with: "' +
            s1 + '","' + s2 + '"', LLInfo);
      except
        StringResult := '';
        LogDatei.log('Error: Exception at jsonAsObjectGetValueByKey with: "' +
          s1 + '","' + s2 + '"', LLerror);
      end;
    end;
  end


  else if LowerCase(s) = LowerCase('jsonAsArrayPutObjectByIndex') then
  begin
    if Skip('(', r, r, InfoSyntaxError) and EvaluateString(r, r, s1,
      InfoSyntaxError) and Skip(',', r, r, InfoSyntaxError) and
      EvaluateString(r, r, s2, InfoSyntaxError) and Skip(',', r, r, InfoSyntaxError) and
      EvaluateString(r, r, s3, InfoSyntaxError) and Skip(')', r, r, InfoSyntaxError) then
    begin
      syntaxCheck := True;
      try
        StringResult := '';
        if jsonAsArrayPutObjectByIndex(s3, s1, StrToInt(s2)) then
          StringResult := s1
        else
          LogDatei.log('Error at jsonAsArrayPutObjectByIndex with: "' +
            s1 + '","' + s2 + '","' + s3 + '"', LLerror);
      except
        StringResult := '';
        LogDatei.log('Error: Exception at jsonAsArrayPutObjectByIndex with: "' +
          s1 + '","' + s2 + '","' + s3 + '"', LLerror);
      end;
    end;
  end

  else if LowerCase(s) = LowerCase('jsonAsObjectSetValueByKey') then
  begin
    if Skip('(', r, r, InfoSyntaxError) and EvaluateString(r, r, s1,
      InfoSyntaxError) and Skip(',', r, r, InfoSyntaxError) and
      EvaluateString(r, r, s2, InfoSyntaxError) and Skip(',', r, r, InfoSyntaxError) and
      EvaluateString(r, r, s3, InfoSyntaxError) and Skip(')', r, r, InfoSyntaxError) then
    begin
      syntaxCheck := True;
      try
        StringResult := '';
        s4 := '';
        if jsonAsObjectSetValueByKey(s1, s2, s3, s4) then
          StringResult := s4
        else
          LogDatei.log('Error at jsonAsObjectSetValueByKey with: "' +
            s1 + '","' + s2 + '","' + s3 + '"', LLerror);
      except
        StringResult := '';
        LogDatei.log('Error: Exception at jsonAsObjectSetValueByKey with: "' +
          s1 + '","' + s2 + '","' + s3 + '"', LLerror);
      end;
    end;
  end

  else if LowerCase(s) = LowerCase('jsonAsObjectSetStringtypeValueByKey') then
  begin
    if Skip('(', r, r, InfoSyntaxError) and EvaluateString(r, r, s1,
      InfoSyntaxError) and Skip(',', r, r, InfoSyntaxError) and
      EvaluateString(r, r, s2, InfoSyntaxError) and Skip(',', r, r, InfoSyntaxError) and
      EvaluateString(r, r, s3, InfoSyntaxError) and Skip(')', r, r, InfoSyntaxError) then
    begin
      syntaxCheck := True;
      try
        StringResult := '';
        s4 := '';
        if jsonAsObjectSetStringtypeValueByKey(s1, s2, s3, s4) then
          StringResult := s4
        else
          LogDatei.log('Error at jsonAsObjectSetStringtypeValueByKey with: "' +
            s1 + '","' + s2 + '","' + s3 + '"', LLerror);
      except
        StringResult := '';
        LogDatei.log('Error: Exception at jsonAsObjectSetStringtypeValueByKey with: "' +
          s1 + '","' + s2 + '","' + s3 + '"', LLerror);
      end;
    end;
  end

  else if LowerCase(s) = LowerCase('jsonAsObjectAddKeyAndValue') then
  begin
    if Skip('(', r, r, InfoSyntaxError) and EvaluateString(r, r, s1,
      InfoSyntaxError) and Skip(',', r, r, InfoSyntaxError) and
      EvaluateString(r, r, s2, InfoSyntaxError) and Skip(',', r, r, InfoSyntaxError) and
      EvaluateString(r, r, s3, InfoSyntaxError) and Skip(')', r, r, InfoSyntaxError) then
    begin
      syntaxCheck := True;
      try
        StringResult := '';
        s4 := '';
        if jsonAsObjectAddKeyAndValue(s1, s2, s3, s4) then
          StringResult := s4
        else
          LogDatei.log('Error at jsonAsObjectAddKeyAndValue with: "' +
            s1 + '","' + s2 + '","' + s3 + '"', LLerror);
      except
        StringResult := '';
        LogDatei.log('Error: Exception at jsonAsObjectAddKeyAndValue with: "' +
          s1 + '","' + s2 + '","' + s3 + '"', LLerror);
      end;
    end;
  end

  else if LowerCase(s) = LowerCase('jsonStringListToJsonArray') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
    begin
      syntaxcheck := True;
      stringresult := '';
      list1 := TXStringList.Create;
      slist.Clear;

      if not produceStringList(script, r, r, list1, InfoSyntaxError) or
        not Skip(')', r, r, InfoSyntaxError) then
        syntaxCheck := False
      else
      begin
        try
          slist.Text := list1.Text;
          if not stringListToJsonArray(slist, stringresult) then
          begin
            LogDatei.log('Error at jsonStringListToJsonArray ', LLerror);
            stringresult := '';
          end;
        except
          LogDatei.log('Error Exception at jsonStringListToJsonArray ', LLerror);
          stringresult := '';
        end;
      end;
      list1.Free;
      list1 := nil;

    end;
  end

  else if LowerCase(s) = LowerCase('createUrl') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
    begin
      syntaxcheck := True;
      stringresult := '';
      list1 := TXStringList.Create;
      if not produceStringList(script, r, r, list1, InfoSyntaxError) or
        not Skip(')', r, r, InfoSyntaxError) then
        syntaxCheck := False
      else
      begin
        syntaxCheck := True;
        StringResult := createUrl(list1);
      end;
      list1.Free;
      list1 := nil;
    end;
  end

  else if LowerCase(s) = LowerCase('getStringFromListAtIndex') then
  begin
    if Skip('(', r, r, InfoSyntaxError) and produceStringList(
      script, r, r, list1, InfoSyntaxError) and Skip(',', r, r, InfoSyntaxError) and
      EvaluateString(r, r, s1, InfoSyntaxError) and Skip(')', r, r, InfoSyntaxError) then
    begin
      syntaxCheck := True;
      try
        StringResult := '';
        StringResult := list1.Strings[StrToInt(s1)];
      except
        StringResult := '';
        LogDatei.log('Error: Exception at getStringFromListAtIndex with: "' +
          s1 + '"', LLerror);
      end;
    end;
  end

  else if LowerCase(s) = LowerCase('RandomStr') then
  begin
    StringResult := randomstr(True);
    syntaxCheck := True;
  end

  else if LowerCase(s) = LowerCase('createNewOpsiHostKey') then
  begin
    StringResult := createNewOpsiHostKey;
    syntaxCheck := True;
  end

  else if LowerCase(s) = LowerCase('timeStampAsFloatStr') then
  begin
    DecimalSeparator := '.';
    StringResult := floattostrF(now, ffFixed, 15, 3);
    syntaxCheck := True;
  end


  (* comparison Expressionstrs *)
  else if LowerCase(s) = LowerCase('CompareStrings') then
  begin
    if Skip('(', r, r, InfoSyntaxError) and EvaluateString(r, r, s1,
      InfoSyntaxError) and Skip(',', r, r, InfoSyntaxError) and
      EvaluateString(r, r, s2, InfoSyntaxError) and Skip(')', r, r, InfoSyntaxError) then
    begin
      syntaxCheck := True;
      StringResult := IntToStr(getCompareSignStrings(s1, s2));
    end;
  end

  else if LowerCase(s) = LowerCase('CompareNumbers') then
  begin
    if Skip('(', r, r, InfoSyntaxError) and EvaluateString(r, r, s1,
      InfoSyntaxError) and Skip(',', r, r, InfoSyntaxError) and
      EvaluateString(r, r, s2, InfoSyntaxError) and Skip(')', r, r, InfoSyntaxError) then
    begin
      syntaxCheck := True;
      OldNumberOfErrors := LogDatei.NumberOfErrors;

      try
        n1 := StrToInt(s1);
        try
          n2 := StrToInt(s2);
          StringResult := IntToStr(getCompareSign(n1, n2));
        except
          LogDatei.log('Error: ' + s2 + ' has no Integer format', LLError);
        end;
      except
        LogDatei.log('Error: ' + s1 + ' has no Integer format', LLError)
      end;

      DiffNumberOfErrors := LogDatei.NumberOfErrors - OldNumberOfErrors;
      FNumberOfErrors := NumberOfErrors + DiffNumberOfErrors;
    end;
  end

  else if LowerCase(s) = LowerCase('getIP4NetworkByAdrAndMask') then
  begin
    if Skip('(', r, r, InfoSyntaxError) and EvaluateString(r, r, s1,
      InfoSyntaxError) and Skip(',', r, r, InfoSyntaxError) and
      EvaluateString(r, r, s2, InfoSyntaxError) and Skip(')', r, r, InfoSyntaxError) then
    begin
      if getIP4NetworkByAdrAndMask(s1, s2) = '' then
      begin
        StringResult := '';
        Logdatei.log('Error: Invalid inputs. ' + s1 + ' or ' + s2 +
          ' is invalid', LLerror);
      end
      else
      begin
        syntaxCheck := True;
        StringResult := getIP4NetworkByAdrAndMask(s1, s2);
      end;
    end;
  end

  else if LowerCase(s) = LowerCase('CompareDotSeparatedNumbers') then
  begin
    if Skip('(', r, r, InfoSyntaxError) and EvaluateString(r, r, s1,
      InfoSyntaxError) and Skip(',', r, r, InfoSyntaxError) and
      EvaluateString(r, r, s2, InfoSyntaxError) and Skip(')', r, r, InfoSyntaxError) then
    begin
      syntaxCheck := True;
      OldNumberOfErrors := LogDatei.NumberOfErrors;
      StringResult := '0';
      if getDecimalCompareSign(s1, s2, intresult, errorinfo, False) then
        StringResult := IntToStr(intresult)
      else
      begin
        LogDatei.log('Error: ' + errorinfo, LLError);
        LogDatei.log('Error: CompareDotSeparatedNumbers: using default result = 0 ',
          LLError);
      end;

      DiffNumberOfErrors := LogDatei.NumberOfErrors - OldNumberOfErrors;
      FNumberOfErrors := NumberOfErrors + DiffNumberOfErrors;
    end;
  end

  else if LowerCase(s) = LowerCase('CompareDotSeparatedStrings') then
  begin
    if Skip('(', r, r, InfoSyntaxError) and EvaluateString(r, r, s1,
      InfoSyntaxError) and Skip(',', r, r, InfoSyntaxError) and
      EvaluateString(r, r, s2, InfoSyntaxError) and Skip(')', r, r, InfoSyntaxError) then
    begin
      syntaxCheck := True;

      OldNumberOfErrors := LogDatei.NumberOfErrors;

      if getDecimalCompareSign(s1, s2, intresult, errorinfo, True) then
        StringResult := IntToStr(intresult)
      else
        LogDatei.log('Error: ' + errorinfo, LLError);

      DiffNumberOfErrors := LogDatei.NumberOfErrors - OldNumberOfErrors;
      FNumberOfErrors := NumberOfErrors + DiffNumberOfErrors;
    end;
  end


  else if LowerCase(s) = LowerCase('ParamStr') then
  begin
    StringResult := ExtraParameter;
    syntaxCheck := True;
  end

  else if LowerCase(s) = LowerCase('SubstringBefore') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(',', r, r, InfoSyntaxError) then
          if EvaluateString(r, r, s2, InfoSyntaxError) then
            if Skip(')', r, r, InfoSyntaxError) then
            begin
              syntaxCheck := True;

              StringResult := '';
              if (length(s1) >= length(s2)) and
                (copy(s1, length(s1) - length(s2) + 1, length(s2)) = s2) then
                StringResult := copy(s1, 1, length(s1) - length(s2));
            end;
  end


  else if LowerCase(s) = LowerCase('unquote') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(',', r, r, InfoSyntaxError) then
          if EvaluateString(r, r, s2, InfoSyntaxError) then
            if Skip(')', r, r, InfoSyntaxError) then
            begin
              syntaxCheck := True;
              StringResult := opsiUnquotestr(s1, s2);
         (*
         if  (length(s1) >= 1) and (length(s2) >= 1) then
         begin
          tempchar := trim(s2)[1];
          tempansistr := s1;
          tempansistrp := pansichar(tempansistr);
           StringResult := AnsiExtractQuotedStr(tempansistrp,tempchar);
           // s1 was not quoted then give back s1
           if StringResult = '' then  StringResult := s1;
           // s1 was not quoted by s2 then give back s1
           // and (ansipos('NULL',s1)=0)
           if (StringResult = NULL_STRING_VALUE)  then  StringResult := s1;
         end;
         *)
            end;
  end

  else if LowerCase(s) = LowerCase('unquote2') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(',', r, r, InfoSyntaxError) then
          if EvaluateString(r, r, s2, InfoSyntaxError) then
            if Skip(')', r, r, InfoSyntaxError) then
            begin
              syntaxCheck := True;
              StringResult := opsiUnquotestr2(s1, s2);
         (*
         if  (length(s1) >= 1) and (length(s2) >= 1) then
         begin
          tempchar := trim(s2)[1];
          tempansistr := s1;
          tempansistrp := pansichar(tempansistr);
           StringResult := AnsiExtractQuotedStr(tempansistrp,tempchar);
           // s1 was not quoted then give back s1
           if StringResult = '' then  StringResult := s1;
           // s1 was not quoted by s2 then give back s1
           // and (ansipos('NULL',s1)=0)
           if (StringResult = NULL_STRING_VALUE)  then  StringResult := s1;
         end;
         *)
            end;
  end

  else if LowerCase(s) = LowerCase('stringinput') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(',', r, r, InfoSyntaxError) then
          if EvaluateString(r, r, s2, InfoSyntaxError) then
            if Skip(')', r, r, InfoSyntaxError) then
            begin
              syntaxCheck := True;
              boolresult := StrToBool(s2);
         {$IFDEF GUI}
              try
                Finputstring := TFinputstring.Create(nil);
                if boolresult then
                begin
                  Finputstring.EditButton1.EchoMode := emPassword;
                  Finputstring.EditButton1.Button.Enabled := True;
                end
                else
                begin
                  Finputstring.EditButton1.EchoMode := emNormal;
                  Finputstring.EditButton1.Button.Enabled := False;
                end;
                Finputstring.Label1.Caption := s1;
                Finputstring.EditButton1.Text := '';
                Finputstring.ShowModal;
                StringResult := Finputstring.EditButton1.Text;
              finally
                FreeAndNil(Finputstring);
              end;
         {$ELSE GUI}
              cmdLineInputDialog(StringResult, s1, '', boolresult);
         {$ENDIF GUI}
            end;
  end



  else if LowerCase(s) = LowerCase('stringreplace') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(',', r, r, InfoSyntaxError) then
          if EvaluateString(r, r, s2, InfoSyntaxError) then
            if Skip(',', r, r, InfoSyntaxError) then
              if EvaluateString(r, r, s3, InfoSyntaxError) then
                if Skip(')', r, r, InfoSyntaxError) then
                begin
                  syntaxCheck := True;
                  StringResult := StringReplace1(s1, s2, s3);
                  if (StringResult = NULL_STRING_VALUE) then
                    StringResult := s1;
                end;
  end

  else if LowerCase(s) = LowerCase('stringReplaceRegex') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(',', r, r, InfoSyntaxError) then
          if EvaluateString(r, r, s2, InfoSyntaxError) then
            if Skip(',', r, r, InfoSyntaxError) then
              if EvaluateString(r, r, s3, InfoSyntaxError) then
                if Skip(')', r, r, InfoSyntaxError) then
                begin
                  syntaxCheck := True;
                  StringResult := stringReplaceRegex(s1, s2, s3);
                end;
  end

  else if LowerCase(s) = LowerCase('reencodestr') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(',', r, r, InfoSyntaxError) then
          if EvaluateString(r, r, s2, InfoSyntaxError) then
            if Skip(',', r, r, InfoSyntaxError) then
              if EvaluateString(r, r, s4, InfoSyntaxError) then
                if Skip(')', r, r, InfoSyntaxError) then
                begin
                  syntaxCheck := True;
                  // s3 is the used encoding
                  StringResult := reencode(s1, s2, s3, s4);
                end;
  end

  else if LowerCase(s) = LowerCase('StringSplit')
  // deprecated, please replace by splitting the string by splitstring and
  // returning a specific part by takestring
  then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(',', r, r, InfoSyntaxError) then
          if EvaluateString(r, r, s2, InfoSyntaxError) then
            if Skip(',', r, r, InfoSyntaxError) then
              if EvaluateString(r, r, sx, InfoSyntaxError) then
                if Skip(')', r, r, InfoSyntaxError) then
                begin
                  syntaxCheck := True;

                  itemlist.Free;
                  itemlist := TXStringList.Create;
                  stringsplit(s1, s2, itemlist);

                  try
                    ListIndex := StrToInt(sx);
                  except
                    syntaxCheck := False;
                    InfoSyntaxError := sx + ' keine ganze Zahl';
                    ListIndex := 0;
                  end;

                  if ListIndex >= itemList.Count then
                  begin
                    StringResult := '';
                    OldNumberOfWarnings := LogDatei.NumberOfWarnings;
                    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
                    LogDatei.log('Warning: String "' + s1 +
                      '" using delimiter "' + s2 +
                      '" was not splitted in (' + IntToStr(ListIndex) +
                      ' + 1)  parts', LLWarning);
                    DiffNumberOfWarnings :=
                      LogDatei.NumberOfWarnings - OldNumberOfWarnings;
                    FNumberOfWarnings := NumberOfWarnings + DiffNumberOfWarnings;
                    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
                  end
                  else
                    StringResult := itemlist[ListIndex];
                end;
  end


  else if LowerCase(s) = LowerCase('takeString') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
    begin
      syntaxcheck := True;
      stringresult := '';
      try
        GetWord(r, s1, r, [',']);

        a1 := StrToInt(s1);

        syntaxCheck := Skip(',', r, r, InfoSyntaxError);
      except
        try
          sx := s1;
          if EvaluateString(s1, s1, s2, InfoSyntaxError) then
          begin
            a1 := StrToInt(s2);
            syntaxCheck := Skip(',', r, r, InfoSyntaxError);
          end
          else
            a1 := StrToInt(sx);
        except
          syntaxcheck := False;
          InfoSyntaxError := ' No valid index for list ';
        end;
      end;

      if syntaxCheck then
      begin
        list1 := TXStringList.Create;
        r1 := r;
        if not produceStringList(script, r, r, list1, InfoSyntaxError) or
          not Skip(')', r, r, InfoSyntaxError) then
          syntaxCheck := False
        else
        begin
          if list1.Count = 0 then // list is empty
          begin
            Logdatei.log('Stringlist ' + r1 +
              ' is empty in takeString function ! Use count() before takestring() to avoid this problem.',
              LLWarning);
            if FatalOnRuntimeError then
            begin
              Logdatei.log('Set to FatalError because FatalOnRuntimeError is set',
                LLCritical);
              FExtremeErrorLevel := LevelFatal;
              LogDatei.ActionProgress := 'Runtime Error';
            end;
          end
          else
          begin
            if (a1 < 0) then
              a1 := list1.Count + a1; //we count downward

            if (a1 < 0) or (a1 > list1.Count - 1) then
            begin
              Logdatei.log('Stringlist list ' + r1 + ' has ' +
                IntToStr(list1.Count) + ' elements. And the effective list index ' +
                IntToStr(a1) +
                ' is out of bounds in takeString function ! Use count() before takestring() to avoid this problem.',
                LLWarning);
              if FatalOnRuntimeError then
              begin
                Logdatei.log('Set to FatalError because FatalOnRuntimeError is set',
                  LLCritical);
                FExtremeErrorLevel := LevelFatal;
                LogDatei.ActionProgress := 'Runtime Error';
              end;
            end
            else
              stringresult := list1[a1];
          end;
        end;

        list1.Free;
        list1 := nil;
      end;
    end;
  end

  else if LowerCase(s) = LowerCase('count') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
    begin
      syntaxcheck := True;
      stringresult := '';
      list1 := TXStringList.Create;

      if not produceStringList(script, r, r, list1, InfoSyntaxError) or
        not Skip(')', r, r, InfoSyntaxError) then
        syntaxCheck := False
      else
      begin
        stringresult := IntToStr(list1.Count);
        list1.Free;
        list1 := nil;
      end;
    end;
  end

  else if LowerCase(s) = LowerCase('getValue') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
    begin
      syntaxcheck := True;
      stringresult := '';

      if not evaluateString(r, r, s1, InfoSyntaxError) or not
        Skip(',', r, r, InfoSyntaxError) then
        syntaxCheck := False;

      if syntaxCheck then
      begin
        list1 := TXStringList.Create;

        if not produceStringList(script, r, r, list1, InfoSyntaxError) or
          not Skip(')', r, r, InfoSyntaxError) then
          syntaxCheck := False
        else
        begin
          stringresult := list1.getStringValue(s1);//list1.values[s1];
          // if key does not exist we get NULL_STRING_VALUE
             (*
             if stringresult = NULL_STRING_VALUE then
             begin
               // let us retry with trimed keys
               for i := 0 to list1.Count -1 do
               begin
                 if list1.Names[i] <> '' then
                   if lowerCase(trim(list1.Names[i])) = lowerCase(trim(s1)) then
                     stringresult := list1.getStringValue(list1.Names[i]);
               end;
             end;
             *)
        end;

        list1.Free;
        list1 := nil;
      end;
    end;
  end

  else if LowerCase(s) = LowerCase('getValueBySeparator') then
  begin
    syntaxCheck := False;
    stringresult := '';
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(',', r, r, InfoSyntaxError) then
          if EvaluateString(r, r, s2, InfoSyntaxError) then
            if Skip(',', r, r, InfoSyntaxError) then
              syntaxCheck := True;


    if syntaxCheck then
    begin
      list1 := TXStringList.Create;

      if not produceStringList(script, r, r, list1, InfoSyntaxError) or
        not Skip(')', r, r, InfoSyntaxError) then
        syntaxCheck := False
      else
      begin
        // use the first char of the second argument as separator char
        list1.NameValueSeparator := trim(s2)[1];
        stringresult := list1.getStringValue(s1);//list1.values[s1];
        // if key does not exist we get NULL_STRING_VALUE
           (*
           if stringresult = NULL_STRING_VALUE then
             begin
               // let us retry with trimed keys
               for i := 0 to list1.Count -1 do
               begin
                 if list1.Names[i] <> '' then
                   if lowerCase(trim(list1.Names[i])) = lowerCase(trim(s1)) then
                     stringresult := list1.getStringValue(list1.Names[i]);
               end;
             end;
           *)
      end;

      list1.Free;
      list1 := nil;
    end;
  end

  else if LowerCase(s) = LowerCase('getValueFromFile') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
    begin
      syntaxcheck := True;
      stringresult := '';

      if not evaluateString(r, r, s1, InfoSyntaxError) or not
        Skip(',', r, r, InfoSyntaxError) then
        syntaxCheck := False;

      if syntaxCheck then
      begin
        list1 := TXStringList.Create;
        if EvaluateString(r, r, s3, InfoSyntaxError) then
        begin
          try
            s3 := ExpandFileName(s3);
            list1.loadfromfile(s3);
            list1.Text := reencode(list1.Text, 'system');
          except
            on e: Exception do
            begin
              LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
              LogDatei.log('Error on loading file: ' + e.message, LLError);
              LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
            end
          end;
          if not Skip(')', r, r, InfoSyntaxError) then
            syntaxCheck := False
          else
          begin
            syntaxCheck := True;
            stringresult := list1.getStringValue(s1);
            if stringresult = 'NULL' then
              stringresult := '';

            //list1.values[s1];
            // if key does not exist we get NULL_STRING_VALUE
             (*
             if stringresult = NULL_STRING_VALUE then
             begin
               // let us retry with trimed keys
               for i := 0 to list1.Count -1 do
               begin
                 if list1.Names[i] <> '' then
                   if lowerCase(trim(list1.Names[i])) = lowerCase(trim(s1)) then
                     stringresult := list1.getStringValue(list1.Names[i]);
               end;
             end;
             *)
          end;
        end;
        list1.Free;
        list1 := nil;
      end;
    end;
  end


  else if LowerCase(s) = LowerCase('getValueFromFileBySeparator') then
  begin
    syntaxCheck := False;
    stringresult := '';
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(',', r, r, InfoSyntaxError) then
          if EvaluateString(r, r, s2, InfoSyntaxError) then
            if Skip(',', r, r, InfoSyntaxError) then
              syntaxCheck := True;


    if syntaxCheck then
    begin
      list1 := TXStringList.Create;
      if EvaluateString(r, r, s3, InfoSyntaxError) then
      begin
        try
          s3 := ExpandFileName(s3);
          list1.loadfromfile(s3);
          list1.Text := reencode(list1.Text, 'system');
        except
          on e: Exception do
          begin
            LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
            LogDatei.log('Error on loading file: ' + e.message, LLError);
            LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
          end
        end;
        if not Skip(')', r, r, InfoSyntaxError) then
          syntaxCheck := False
        else
        begin
          syntaxCheck := True;
          // use the first char of the second argument as separator char
          list1.NameValueSeparator := trim(s2)[1];
          stringresult := list1.getStringValue(s1);
          if stringresult = 'NULL' then
            stringresult := '';
          //list1.values[s1];
          // if key does not exist we get NULL_STRING_VALUE
             (*
             if stringresult = NULL_STRING_VALUE then
             begin
               // let us retry with trimed keys
               for i := 0 to list1.Count -1 do
               begin
                 if list1.Names[i] <> '' then
                   if lowerCase(trim(list1.Names[i])) = lowerCase(trim(s1)) then
                     stringresult := list1.getStringValue(list1.Names[i]);
               end;
             end;
             *)
        end;
      end;
      list1.Free;
      list1 := nil;
    end;
  end

  else if LowerCase(s) = LowerCase('takeFirstStringContaining') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
    begin

      list1 := TXStringList.Create;

      if produceStringList(script, r, r, list1, InfoSyntaxError) and
        skip(',', r, r, InfoSyntaxError) then
      begin
        if EvaluateString(r, r, s1, InfoSyntaxError) and
          skip(')', r, r, InfoSyntaxError) then
        begin
          SyntaxCheck := True;
          stringResult := '';
          i := 0;
          while (stringResult = '') and (i < list1.Count) do
          begin
            if AnsiContainsText(list1[i], s1) then
              stringResult := list1[i]
            else
              Inc(i);
          end;
        end;
      end;
    end;
  end

  else if LowerCase(s) = LowerCase('getIndexFromListByContaining') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
    begin

      list1 := TXStringList.Create;

      if produceStringList(script, r, r, list1, InfoSyntaxError) and
        skip(',', r, r, InfoSyntaxError) then
      begin
        if EvaluateString(r, r, s1, InfoSyntaxError) and
          skip(')', r, r, InfoSyntaxError) then
        begin
          SyntaxCheck := True;
          stringResult := '';
          i := 0;
          while (stringResult = '') and (i < list1.Count) do
          begin
            if AnsiContainsText(list1[i], s1) then
              stringResult := IntToStr(i)
            else
              Inc(i);
          end;
        end;
      end;
    end;
  end


  else if LowerCase(s) = LowerCase('composeString') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
    begin

      list1 := TXStringList.Create;

      if produceStringList(script, r, r, list1, InfoSyntaxError) and
        skip(',', r, r, InfoSyntaxError) then
      begin
        if EvaluateString(r, r, s1, InfoSyntaxError) and
          skip(')', r, r, InfoSyntaxError) then
        begin
          SyntaxCheck := True;
          stringResult := '';
          for i := 0 to list1.Count - 2 do
            stringResult := stringResult + list1.strings[i] + s1;
          if list1.Count > 0 then
            stringResult := stringResult + list1[list1.Count - 1];
        end;
      end;
    end;
  end

  else if LowerCase(s) = LowerCase('ExtractFilePath') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          try
            StringResult := ExtractFilePath(s1);
            syntaxCheck := True;
          except
            InfoSyntaxError := '"' + s1 + '" is not a valid file path';
          end;
        end;
  end

  else if LowerCase(s) = LowerCase('ExtractFileExtension') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          try
            StringResult := ExtractFileExt(s1);
            syntaxCheck := True;
          except
            InfoSyntaxError := '"' + s1 + '" is not a valid file path';
          end;
        end;
  end

  else if LowerCase(s) = LowerCase('ExtractFileName') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          try
            StringResult := ExtractFileName(s1);
            syntaxCheck := True;
          except
            InfoSyntaxError := '"' + s1 + '" is not a valid file path';
          end;
        end;
  end

  else if LowerCase(s) = LowerCase('RegString') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          StringResult := CEscaping(s1);
          syntaxCheck := True;
        end;
  end


  //  #### start xml2 string

  else if LowerCase(s) = LowerCase('getXml2AttributeValueByKey') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
    begin
      list1 := TXStringList.Create;
      if produceStringList(script, r, r, list1, InfoSyntaxError) and
        skip(',', r, r, InfoSyntaxError) then
      begin
        if EvaluateString(r, r, s1, InfoSyntaxError) and
          skip(')', r, r, InfoSyntaxError) then
        begin
          SyntaxCheck := True;
          stringResult := '';
          if not getXml2AttributeValueByKey(list1, s1, stringResult) then
          begin
            LogDatei.log('Error on producing getXml2AttributeValueByKey', LLerror);
          end;
        end;
      end;
    end;
  end

  else if LowerCase(s) = LowerCase('getXml2Text') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
    begin
      list1 := TXStringList.Create;
      if produceStringList(script, r, r, list1, InfoSyntaxError) and
        skip(')', r, r, InfoSyntaxError) then
      begin
        SyntaxCheck := True;
        stringResult := '';
        if not getXml2Text(list1, stringResult) then
        begin
          LogDatei.log('Error on producing getXml2Text', LLerror);
        end;
      end;
    end;
  end

  //  #### stop xml2 string


 {$IFDEF WINDOWS}
 {$IFDEF WIN32}
  else if LowerCase(s) = LowerCase('SidToName') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          try
            StringResult := StrSIDToName(s1);
            syntaxCheck := True;
          except
            on e: Exception do
            begin
              InfoSyntaxError := '"' + s1 + '" is not a valid sid string';
            end
          end;
        end;
  end

  else if LowerCase(s) = LowerCase('NameToSID') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          try
            StringResult := GetLocalUserSidStr(s1);
            syntaxCheck := True;
          except
            on e: Exception do
            begin
              InfoSyntaxError := '"' + s1 + '" is not a valid sid string';
            end;
          end;
        end;
  end
{$ENDIF WIN32}


  else if (LowerCase(s) = LowerCase('GetRegistryValue')) then
  begin
    s3 := '';
    tmpstr2 := '';
    tmpbool := True;
    syntaxCheck := False;
    StringResult := '';
    LogDatei.log_prog('GetRegistryValue from: ' + r, LLdebug3);
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(',', r, r, InfoSyntaxError) then
          if EvaluateString(r, tmpstr, s2, InfoSyntaxError) then
            // next after ',' or ')'
            if Skip(',', tmpstr, tmpstr1, tmpstr3) then
              if EvaluateString(tmpstr1, tmpstr2, s3, tmpstr3) then;
    if s3 = '' then
    begin
      // only two parameter
      if Skip(')', tmpstr, r, InfoSyntaxError) then
      begin
        syntaxCheck := True;
      end;
    end
    else
    begin
      // three parameter
      if Skip(')', tmpstr2, r, InfoSyntaxError) then
      begin
        syntaxCheck := True;
        try
          tmpbool := True;
          if lowercase(s3) = '32bit' then
            tmpbool := False
          else if lowercase(s3) = '64bit' then
            tmpbool := True
          else if lowercase(s3) = 'sysnative' then
            tmpbool := True
          else
          begin
            InfoSyntaxError :=
              'Error: unknown parameter: ' + s3 +
              ' expected one of 32bit,64bit,sysnative - fall back to sysnative';
            syntaxCheck := False;
          end;
        except
          Logdatei.log('Error: Exception in GetRegistryValue: ', LLError);
        end;
      end;
    end;
    if syntaxCheck then
    begin
      GetWord(s1, key0, key, ['\']);
      LogDatei.log_prog('GetRegistryValue from: ' + key0 + key +
        ' ValueName: ' + s2, LLdebug);
      StringResult := '';
      LogDatei.log('key0 = ' + key0, LLdebug2);
      if runLoginScripts and (('HKEY_CURRENT_USER' = UpperCase(key0)) or
        ('HKCU' = UpperCase(key0))) then
      begin
        // remove HKCU from the beginning
        // switch to HKEY_USERS
        key0 := 'HKEY_USERS';
        key := '\' + usercontextSID + key;
        LogDatei.log('Running loginscripts: key0 is now: ' + key0 +
          ', key is now: ' + key, LLdebug);
      end;
      StringResult := GetRegistrystringvalue(key0 + key, s2, tmpbool);
    end;
  end



  else if (LowerCase(s) = LowerCase('GetRegistryStringValue')) or
    (LowerCase(s) = LowerCase('GetRegistryStringValue32')) then
  begin
    LogDatei.log_prog('GetRegistryStringValue from: ' + r, LLdebug3);
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
      begin
        LogDatei.log_prog('GetRegistryStringValue from: ' + s1 +
          ' Remaining: ' + r, LLdebug3);
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          //GetWord (r1, key, r1, [']']);
          LogDatei.log_prog('GetRegistryStringValue from: ' + s1 + ' Remaining: ' + r,
            LLdebug2);
          GetWord(s1, key, r1, [']'], True);
          key := trim(key) + ']';
          p1 := pos('[', key);
          p2 := posFromEnd(']', key);
          p3 := length(key);
          p4 := length(trim(key));
          if not ((pos('[', key) = 1) and (posFromEnd(']', key) = length(key))) then
          begin
            SyntaxCheck := False;
            ErrorInfo := 'Wrong Key Format: Key must be given inside [] - but we got: '
              + key;
          end
          else
          begin
            key := opsiUnquotestr2(trim(key), '[]');
            if (pos('[', key) = 1) and (posFromEnd(']', key) = length(key)) then
            begin
              SyntaxCheck := False;
              ErrorInfo :=
                'Wrong Key Format: Have still brackets after removing them: ' + key;
            end
            else
            begin
              SyntaxCheck := True;
              GetWord(key, key0, key, ['\']);
              System.Delete(key, 1, 1);
              if Skip(']', r1, r1, InfoSyntaxError) then
                ValueName := r1;
              //GetWord (r1, ValueName, r1, [''], true);
              //GetWord (r1, ValueName, r1, WordDelimiterSet1);
              ValueName := trim(ValueName);
              LogDatei.log_prog('GetRegistryStringValue from: ' +
                key0 + '\' + key + ' ValueName: ' + ValueName, LLdebug);
              StringResult := '';
              LogDatei.log('key0 = ' + key0, LLdebug2);
              if runLoginScripts and (('HKEY_CURRENT_USER' = UpperCase(key0)) or
                ('HKCU' = UpperCase(key0))) then
              begin
                // remove HKCU from the beginning
                // switch to HKEY_USERS
                key0 := 'HKEY_USERS';
                key := usercontextSID + '\' + key;
                LogDatei.log('Running loginscripts: key0 is now: ' +
                  key0 + ', key is now: ' + key, LLdebug);
              end;
              StringResult := GetRegistrystringvalue(key0 + '\' + key, ValueName, False);
            end;
          end;
        end;
      end;
  end

  else if (LowerCase(s) = LowerCase('GetRegistryStringValue64')) or
    (LowerCase(s) = LowerCase('GetRegistryStringValueSysNative')) then
  begin
    LogDatei.log_prog('GetRegistryStringValue from: ' + r, LLdebug3);
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
      begin
        LogDatei.log_prog('GetRegistryStringValue from: ' + s1 +
          ' Remaining: ' + r, LLdebug3);
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          //GetWord (r1, key, r1, [']']);
          LogDatei.log_prog('GetRegistryStringValue from: ' + s1 + ' Remaining: ' + r,
            LLdebug2);
          GetWord(s1, key, r1, [']'], True);
          key := trim(key) + ']';
          p1 := pos('[', key);
          p2 := posFromEnd(']', key);
          p3 := length(key);
          p4 := length(trim(key));
          if not ((pos('[', key) = 1) and (posFromEnd(']', key) = length(key))) then
          begin
            SyntaxCheck := False;
            ErrorInfo := 'Wrong Key Format: Key must be given inside [] - but we got: '
              + key;
          end
          else
          begin
            key := opsiUnquotestr2(trim(key), '[]');
            if (pos('[', key) = 1) and (posFromEnd(']', key) = length(key)) then
            begin
              SyntaxCheck := False;
              ErrorInfo :=
                'Wrong Key Format: Have still brackets after removing them: ' + key;
            end
            else
            begin
              SyntaxCheck := True;
              GetWord(key, key0, key, ['\']);
              System.Delete(key, 1, 1);
              if Skip(']', r1, r1, InfoSyntaxError) then
                ValueName := r1;
              //GetWord (r1, ValueName, r1, [''], true);
              //GetWord (r1, ValueName, r1, WordDelimiterSet1);
              ValueName := trim(ValueName);
              LogDatei.log_prog('GetRegistryStringValue from: ' +
                key0 + '\' + key + ' ValueName: ' + ValueName, LLdebug);
              StringResult := '';
              if runLoginScripts and (('HKEY_CURRENT_USER' = UpperCase(key0)) or
                ('HKCU' = UpperCase(key0))) then
              begin
                // remove HKCU from the beginning
                // switch to HKEY_USERS
                key0 := 'HKEY_USERS';
                key := usercontextSID + '\' + key;
                LogDatei.log('Running loginscripts: key0 is now: ' +
                  key0 + ', key is now: ' + key, LLdebug);
              end;
              if (GetNTVersionMajor = 5) and (GetNTVersionMinor = 0) then
                StringResult :=
                  GetRegistrystringvalue(key0 + '\' + key, ValueName, False)
              else
                StringResult :=
                  GetRegistrystringvalue(key0 + '\' + key, ValueName, True);
            end;
          end;
        end;
      end;
  end

  {$IFDEF WIN32}
  else if LowerCase(s) = LowerCase('GetUserSID') then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          if r <> '' then
            InfoSyntaxError := ErrorRemaining
          else
          begin
            SyntaxCheck := True;
            itemlist := TXStringlist.Create;
            stringsplit(s1, '\', itemlist);
            if itemlist.Count > 1 then
            begin
              s2 := itemlist.Strings[0];
              s3 := itemlist.strings[itemlist.Count - 1];
            end
            else
            begin
              s2 := '';
              s3 := s1;
            end;
            logdatei.log('search user: ' + s3 + ' in domain: ' + s2, LevelComplete);
            s4 := ''; //founddomain
            StringResult := GetDomainUserSidS(s2, s3, s4);

            logdatei.log('found in domain ' + s4 + ', SID: ' +
              StringResult, LevelComplete);

            if length(StringResult) > 1 then
            begin
              Skip('[', StringResult, StringResult, InfoSyntaxError);
              if StringResult[length(StringResult)] = ']' then
                StringResult := copy(StringResult, 1, length(StringResult) - 1);
            end;

            if StringResult = '' then
            begin
              OldNumberOfErrors := LogDatei.NumberOfErrors;
              LogDatei.log('Error: SID not found for "' + s1 + '"', LLError);
              DiffNumberOfErrors := LogDatei.NumberOfErrors - OldNumberOfErrors;
              FNumberOfErrors := NumberOfErrors + DiffNumberOfErrors;
            end;
          end;
        end;
  end
  {$ENDIF WIN32}
  {$ELSE WINDOWS}
  else if LowerCase(s) = LowerCase('SidToName') then
  begin
    SyntaxCheck := False;
    InfoSyntaxError := 'Not implemented for Linux';
    StringResult := 'Error';
    LogDatei.log('SyntaxError: SidToName not implemented for Linux', LLError);
  end

  else if LowerCase(s) = LowerCase('NameToSID') then
  begin
    SyntaxCheck := False;
    InfoSyntaxError := 'Not implemented for Linux';
    StringResult := 'Error';
    LogDatei.log('SyntaxError: NameToSID not implemented for Linux', LLError);
  end



  else if (LowerCase(s) = LowerCase('GetRegistryStringValue')) or
    (LowerCase(s) = LowerCase('GetRegistryStringValue32')) or
    (LowerCase(s) = LowerCase('GetRegistryStringValue64')) or
    (LowerCase(s) = LowerCase('GetRegistryStringValueSysNative')) then
  begin
    SyntaxCheck := False;
    InfoSyntaxError := 'Not implemented for Linux';
    StringResult := 'Error';
    LogDatei.log('SyntaxError: GetRegistryStringValue not implemented for Linux',
      LLError);
  end


  else if LowerCase(s) = LowerCase('GetUserSID') then
  begin
    SyntaxCheck := False;
    InfoSyntaxError := 'Not implemented for Linux';
    StringResult := 'Error';
    LogDatei.log('SyntaxError: GetUserSID not implemented for Linux', LLError);
  end
  {$ENDIF WINDOWS}

  else if LowerCase(s) = LowerCase('getLastServiceErrorClass') then
  begin
    SyntaxCheck := True;
    testresult := '';
    errorOccured := False;
    if opsidata = nil then
    begin
      errorOccured := True;
      testresult := '!!! no opsidata !!!';
    end
    else
      try
        local_opsidata := opsidata;

        //LogDatei.log('Calling opsi service at ' + local_opsidata.serviceUrl, LevelComplete);
      except
        errorOccured := True;
        testresult := '!!! not in service mode !!!';
      end;


    if errorOccured then
      stringresult := testresult
    else
    begin
      if local_opsidata.ServiceLastErrorInfo.indexOfName('class') < 0 then
        stringresult := 'None' //'!!! error key "class" not found !!!'
      else
        stringresult := local_opsidata.ServiceLastErrorInfo.values['class'];
    end;
  end


  else if LowerCase(s) = LowerCase('getLastServiceErrorMessage') then
  begin
    testresult := '';
    SyntaxCheck := True;
    errorOccured := False;
    if opsidata = nil then
    begin
      errorOccured := True;
      testresult := '!!! no opsidata !!!';
    end
    else
      try
        local_opsidata := opsidata;

        //LogDatei.log('Calling opsi service at ' + local_opsidata.serviceUrl, LevelComplete);
      except
        errorOccured := True;
        testresult := '!!! not in service mode !!!';
      end;

    if errorOccured then
      stringresult := testresult
    else
    begin
      if local_opsidata.ServiceLastErrorInfo.indexOfName('message') < 0 then
        stringresult := 'None' //'!!! error key "message" not found !!!'
      else
        stringresult := local_opsidata.ServiceLastErrorInfo.values['message'];
    end;
  end

  else if LowerCase(s) = LowerCase('demandLicenseKey') then
  begin
    errorOccured := False;
    syntaxCheck := True;
    testresult := '';

    r := trim(r);
    setLength(parameters, 4);

    // try to find a valid fqdn
    parameters[0] := osconf.computername;
    if parameters[0] = '' then
      parameters[0] := osconf.opsiserviceUser;
    if parameters[0] = '' then
      parameters[0] := oslog.getComputerName;
    parameters[1] := '';
    parameters[2] := '';
    parameters[3] := '';

    syntaxcheck := skip('(', r, r, InfoSyntaxError);

    j := 1;
    continue := True;

    while syntaxCheck and continue do
    begin
      if skip(')', r, r, InfoSyntaxError) and (r = '') then
        continue := False
      else
      begin
        if j > 3 then
          syntaxcheck := False
        else
        begin
          if j > 1 then
            syntaxcheck := skip(',', r, r, InfoSyntaxError);

          if syntaxCheck then
            syntaxCheck := EvaluateString(r, r, parameters[j], InfoSyntaxError);
        end;
      end;
      Inc(j);
    end;

    if syntaxcheck then
    begin
      testresult := 'service request possible';
      if opsidata = nil then
      begin
        errorOccured := True;
        testresult := '!!! no opsidata !!!';
      end
      else
        try
          local_opsidata := opsidata;

          LogDatei.log_prog('Calling opsi service at ' +
            local_opsidata.serviceUrl, LLDebug);
        except
          errorOccured := True;
          testresult := '!!! not in service mode !!!';
        end;

      if not errorOccured then
      begin
        omc := TOpsiMethodCall.Create('getAndAssignSoftwareLicenseKey', parameters);

        testresult := local_opsidata.CheckAndRetrieveString(omc, errorOccured);
      end;
    end;

    if errorOccured then
    begin
      OldNumberOfErrors := LogDatei.NumberOfErrors;
      LogDatei.log('Error: Got no license key: ' + testresult, LLError);
      DiffNumberOfErrors := LogDatei.NumberOfErrors - OldNumberOfErrors;
      FNumberOfErrors := NumberOfErrors + DiffNumberOfErrors;
      stringresult := '';
    end
    else
      stringresult := testresult;
  end


  else if LowerCase(s) = LowerCase('freeLicense') then
  begin
    errorOccured := False;
    syntaxCheck := True;
    testresult := '';

    r := trim(r);
    setLength(parameters, 5);

    parameters[0] := osconf.computername;
    parameters[1] := '';
    parameters[2] := '';
    parameters[3] := '';
    parameters[4] := '';

    syntaxcheck := skip('(', r, r, InfoSyntaxError);

    j := 2;
    continue := True;

    while syntaxCheck and continue do
    begin
      if skip(')', r, r, InfoSyntaxError) and (r = '') then
        continue := False
      else
      begin
        if j > 4 then
          syntaxcheck := False
        else
        begin
          if j > 2 then
            syntaxcheck := skip(',', r, r, InfoSyntaxError);

          if syntaxCheck then
            syntaxCheck := EvaluateString(r, r, parameters[j], InfoSyntaxError);
        end;
      end;
      Inc(j);
    end;

    if syntaxcheck then
    begin
      testresult := 'service request possible';
      if opsidata = nil then
      begin
        errorOccured := True;
        testresult := '!!! no opsidata !!!';
      end
      else
        try
          local_opsidata := opsidata;

          LogDatei.log_prog('Calling opsi service at ' +
            local_opsidata.serviceUrl, LLDebug);
        except
          errorOccured := True;
          testresult := '!!! not in service mode !!!';
        end;

      if not errorOccured then
      begin
        omc := TOpsiMethodCall.Create('deleteSoftwareLicenseUsage', parameters);
        testresult := local_opsidata.CheckAndRetrieveString(omc, errorOccured);
      end;
    end;

    if errorOccured then
    begin
      OldNumberOfErrors := LogDatei.NumberOfErrors;
      LogDatei.log('Error: Could not delete software license usage ' +
        testresult, LLError);
      DiffNumberOfErrors := LogDatei.NumberOfErrors - OldNumberOfErrors;
      FNumberOfErrors := NumberOfErrors + DiffNumberOfErrors;
      stringresult := '';
    end
    else
      stringresult := '';
  end

  else
    InfoSyntaxError := s0 + ' illegal String Expressionstr';


  (* Addition weiterer Teilstrings mit + *)
  if syntaxCheck and skip('+', r, r, sx) then
  begin
    syntaxCheck := EvaluateString(r, r, s1, InfoSyntaxError);
    StringResult := StringResult + s1;
  end;

  if syntaxcheck then
    Remaining := r;

  Result := syntaxCheck;

       (*  if Script.ExitOnError then
        Begin
          tsr := tsrExitProcess;
          .log (StopInfo, BaseLevel);
        End; *)

  LogDatei.LogSIndentLevel := StartIndentLevel;
  slist.Free;
end;


procedure TuibInstScript.GetWordOrStringExpressionstr(const s: string;
  var resultString, Remaining, errorinfo: string);
{ nur fuer AktionenSektion-Syntax 0 }
var
  s0: string = '';

begin
  Resultstring := '';
  s0 := s;
  setLength(s0, length(s)); //should create a new instance, but does not always work
  if not EvaluateString(s0, Remaining, resultString, errorInfo) then
    GetWord(s0, resultString, Remaining, WordDelimiterWhiteSpace);
end;

procedure TuibInstScript.GetWordOrStringExpressionstr(const s: string;
  var resultString, Remaining: string; const WordDelimiterSet: TCharset);
{ nur fuer AktionenSektion-Syntax 0 }
var
  s0: string = '';
  errorinfo: string = '';

begin
  Resultstring := '';
  s0 := s;
  setLength(s0, length(s)); //should create a new instance, but does not always work
  if not EvaluateString(s0, Remaining, resultString, errorInfo) then
    GetWord(s0, resultString, Remaining, WordDelimiterSet);
end;

function TuibInstScript.SkipWordOrStringExpressionstr(const partialS, S: string;
  var Remaining: string; var Error: string): boolean;
  { like skip but handles also string expressions }

var
  orgstring: string = '';
  resultString: string = '';
  errorinfo: string = '';

begin
  Result := False;
  orgstring := S;
  GetWordOrStringExpressionstr(S, resultString, Remaining, errorinfo);
  if trim(AnsiUpperCase(partialS)) = trim(AnsiUpperCase(resultString)) then
  begin
    Result := True;
    Remaining := trim(Remaining);
  end
  else
  begin
    Result := False;
    Remaining := trim(orgstring);
  end;
  if Result then
    Error := ''
  else
    Error := '"' + partialS + '" expected ' + errorinfo;
end;



function TuibInstScript.doXMLAddNamespace(filename: string;
  const elementname: string; const namespace: string): boolean;

var
  list: TStringList;
  s1, s2, s3, s4: string;
  i, p1, p2: integer;
  bChanged: boolean;
  ///done : Boolean;
begin
  Result := False;
  bChanged := False;
  list := TStringList.Create;
  try
    filename := ExpandFileName(filename);
    list.LoadFromFile(filename);
    list.Text := reencode(list.Text, 'system');
    for i := 0 to list.Count - 1 do
    begin
      p1 := pos('<' + elementname, list[i]);
      if p1 > 0 then
      begin //Element found
        s1 := copy(list[i], 1, p1 - 1); //String before '<element'
        s2 := copy(list[i], p1, Length(list[i])); //String after '<element'
        p2 := pos('>', s2);
        s3 := copy(s2, 1, p2); //String from '<element' until '>'
        s4 := copy(s2, p2 + 1, Length(list[i])); //All behind '>'

        p2 := pos(namespace, s3);
        if p2 = 0 then // still existing anyway
        begin
          s3 := copy(s3, 1, Length(s3) - 1); //'>' cut
          s3 := s3 + ' ' + namespace + '>'; //and append again

          list[i] := s1 + s3 + s4;
          bChanged := True;
          Result := True;
          break; //we look only for the first occurrence
        end;
      end;
    end;
    if bChanged then
      list.SaveToFile(filename);

  finally
    list.Free;
  end;

end;

function TuibInstScript.doXMLRemoveNamespace(filename: string;
  const elementname: string; const namespace: string): boolean;
var
  list: TStringList;
  s1, s2, s3, s4: string;
  i, p1, p2: integer;
  bChanged: boolean;
begin
  Result := False;
  bChanged := False;
  list := TStringList.Create;
  try
    filename := ExpandFileName(filename);
    list.LoadFromFile(filename);
    list.Text := reencode(list.Text, 'system');
    for i := 0 to list.Count - 1 do
    begin
      p1 := pos('<' + elementname, list[i]);
      if p1 > 0 then
      begin //Element gefunden
        s1 := copy(list[i], 1, p1 - 1); //String vor '<element'
        s2 := copy(list[i], p1, Length(list[i])); //String ab '<element'
        p2 := pos('>', s2);
        s3 := copy(s2, 1, p2); //String von '<element' bis '>'
        s4 := copy(s2, p2 + 1, Length(list[i])); //Alles hinter '>'

        p2 := pos(namespace, s3);
        if p2 > 0 then //sonst nicht vorhanden
        begin
          System.Delete(s3, p2, Length(namespace));
          if s3[p2 - 1] = ' ' then //wir haben bei Add ein Leerzeichen hinzugefï¿½gt
            System.Delete(s3, p2 - 1, 1);

          list[i] := s1 + s3 + s4;
          bChanged := True;
          Result := True;
          break; //wir suchen nur nach dem ersten Vorkommen
        end;
      end;
    end;
    if bChanged then
      list.SaveToFile(filename);
  finally
    list.Free;
  end;
end;

function TuibInstScript.EvaluateBoolean(Input: string; var Remaining: string;
  var BooleanResult: boolean; NestingLevel: integer;
  var InfoSyntaxError: string): boolean;
begin
  script.FEvalBoolBaseNestLevel := NestingLevel;
  Result := EvaluateBoolean_(Input, Remaining, BooleanResult,
    NestingLevel, InfoSyntaxError);
end;

function TuibInstScript.EvaluateBoolean_
  (Input: string; var Remaining: string; var BooleanResult: boolean;
  NestingLevel: integer; var InfoSyntaxError: string): boolean;
var
  sx: string = '';
  r: string = '';
  s1: string = '';
  s2: string = '';
  s3: string = '';
  syntaxcheck: boolean;
  FileRecord: TUnicodeSearchRec;
  RunTimeInfo: string = '';
  BooleanResult0: boolean;
  Textfile: TPatchList;
  freebytes: int64 = 0;
  requiredbytes: int64 = 0;
  einheit: int64 = 0;
  j: integer = 0;
  drivenumber: integer = 0;
  errnumber: integer = 0;
  relationSymbol: string = '';
  intresult: integer = 0;
  n1: integer = 0;
  n2: integer = 0;
  comparesign: integer = 0;
  erroroccured: boolean;
  oldDisableWow64FsRedirectionStatus: pointer = nil;
  dummybool: boolean;
  OldWinapiErrorMode: cardinal;
  list1: TXStringList;
  InputBakup: string;
  i: integer;
  tmpint: integer;
  tmpbool, tmpbool1, tmpbool2: boolean;
  tmpstr, tmpstr1, tmpstr2, tmpstr3: string;
  FindResultcode: integer = 0;
  flushhandle: Thandle;
  int64result: int64;
  funcname: string;
  funcindex: integer;

begin
  syntaxCheck := False;
  InfoSyntaxError := '';
  RunTimeInfo := '';
  BooleanResult := False;
  list1 := TXStringlist.Create;
  InputBakup := Input;

  LogDatei.log_prog('EvaluateBoolean: Parsing: ' + Input + ' ', LLDebug);


  // geklammerter Boolescher Ausdruck
  if Skip('(', Input, r, sx) then
  begin
    if EvaluateBoolean_(r, r, BooleanResult, NestingLevel + 1, InfoSyntaxError) then
      if Skip(')', r, r, InfoSyntaxError) then
        syntaxCheck := True;
  end

  // Boolean function
  else if Skip('not', Input, r, sx) then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateBoolean_(r, r, BooleanResult, NestingLevel + 1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          BooleanResult := not BooleanResult;
          syntaxCheck := True;
        end;
  end

 {$IFDEF WIN32}
  else if Skip('FileExists64', Input, r, sx) then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          LogDatei.log('  Starting query if file exist (64 Bit mode)...', LLInfo);
          s2 := s1;
          if (length(s1) > 0) and (s1[length(s1)] = PATHSEPARATOR) then
            s2 := copy(s1, 1, length(s1) - 1);
          try
            if DSiDisableWow64FsRedirection(oldDisableWow64FsRedirectionStatus) then
            begin
              LogDatei.log('DisableWow64FsRedirection succeeded', LLinfo);
              //BooleanResult := GetFileInfo (s2, FileRecord, RunTimeInfo);
              // disable  critical-error-handler message box. (Drive not ready)
              OldWinapiErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
              try
                try
                  BooleanResult := FileExists(s2) or DirectoryExists(s2);
                  if (not BooleanResult) and (not (trim(s2) = '')) then
                  begin
                    LogDatei.log('File: ' + s2 + ' not found via FileExists', LLDebug3);
                  end;
                except
                  BooleanResult := False;
                end;
              finally
                setErrorMode(OldWinapiErrorMode);
              end;
              dummybool := DSiRevertWow64FsRedirection(
                oldDisableWow64FsRedirectionStatus);
              LogDatei.log('RevertWow64FsRedirection succeeded', LLinfo);
            end
            else
            begin
              LogDatei.log('Error: DisableWow64FsRedirection failed', LLError);
              BooleanResult := False;
            end;
          except
            on ex: Exception do
            begin
              LogDatei.log('Error: ' + ex.message, LLError);
            end;
          end;
          if not BooleanResult then
          begin
            RunTimeInfo := 'Not found: "' + s1 + '": ' + RunTimeInfo;
            LogDatei.log(RunTimeInfo, LLinfo);
          end;
          syntaxCheck := True;
        end;
  end

  else if Skip('FileExistsSysNative', Input, r, sx) then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          if Is64BitSystem then
            LogDatei.log('  Starting query if file exist (SysNative 64 Bit mode)...',
              LLInfo)
          else
            LogDatei.log('  Starting query if file exist (SysNative 32 Bit mode)...',
              LLInfo);
          s2 := s1;
          if (length(s1) > 0) and (s1[length(s1)] = PATHSEPARATOR) then
            s2 := copy(s1, 1, length(s1) - 1);
          if Is64BitSystem then
          begin
            try
              if DSiDisableWow64FsRedirection(oldDisableWow64FsRedirectionStatus) then
              begin
                LogDatei.log('DisableWow64FsRedirection succeeded', LLinfo);
                OldWinapiErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
                try
                  try
                    BooleanResult := FileExists(s2) or DirectoryExists(s2);
                    if (not BooleanResult) and (not (trim(s2) = '')) then
                    begin
                      LogDatei.log('File: ' + s2 +
                        ' not found via FileExists', LLDebug3);
                    end;
                  except
                    BooleanResult := False;
                  end;
                finally
                  setErrorMode(OldWinapiErrorMode);
                end;
                dummybool := DSiRevertWow64FsRedirection(
                  oldDisableWow64FsRedirectionStatus);
                LogDatei.log('RevertWow64FsRedirection succeeded', LLinfo);
              end
              else
              begin
                LogDatei.log('Error: DisableWow64FsRedirection failed', LLError);
                BooleanResult := False;
              end;
            except
              on ex: Exception do
              begin
                LogDatei.log('Error: ' + ex.message, LLError);
              end;
            end;
          end
          else
          begin
            BooleanResult := FileExists(s2) or DirectoryExists(s2);
            if (not BooleanResult) and (not (trim(s2) = '')) then
            begin
              LogDatei.log('File: ' + s2 + ' not found via FileExists', LLDebug3);
            end;
          end;
          if not BooleanResult then
          begin
            RunTimeInfo := 'Not found: "' + s1 + '": ' + RunTimeInfo;
            LogDatei.log(RunTimeInfo, LLinfo);
          end;
          syntaxCheck := True;
        end;
  end

  else if Skip('FileExists32', Input, r, sx) then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          LogDatei.log('  Starting query if file exist ...', LLInfo);
          s2 := s1;
          if (length(s1) > 0) and (s1[length(s1)] = PATHSEPARATOR) then
            s2 := copy(s1, 1, length(s1) - 1);
          OldWinapiErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
          try
            try
              BooleanResult := FileExists(s2) or DirectoryExists(s2);
              if (not BooleanResult) and (not (trim(s2) = '')) then
              begin
                LogDatei.log('File: ' + s2 + ' not found via FileExists', LLDebug3);
              end;
            except
              BooleanResult := False;
            end;
          finally
            setErrorMode(OldWinapiErrorMode);
          end;
          if not BooleanResult then
          begin
            RunTimeInfo := 'Not found: "' + s1 + '": ' + RunTimeInfo;
            LogDatei.log(RunTimeInfo, LLinfo);
          end;
          syntaxCheck := True;
        end;
  end
 {$ENDIF WIN32}

  else if Skip('FileExists', Input, r, sx) then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          s1 := ExpandFileName(s1);
          LogDatei.log('Starting query if file exist ...', LLInfo);
          s2 := s1;
          if (length(s1) > 0) and (s1[length(s1)] = PATHSEPARATOR) then
            s2 := copy(s1, 1, length(s1) - 1);
      {$IFDEF WINDOWS}
          { disable  critical-error-handler message box. (Drive not ready) }
          OldWinapiErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
          try
            try
              s2 := trim(s2);
              BooleanResult := FileExists(s2) or DirectoryExists(s2);
              if (not BooleanResult) and (not (trim(s2) = '')) then
              begin
                LogDatei.log('File: ' + s2 + ' not found via FileExists', LLDebug3);
              end;
            except
              BooleanResult := False;
            end;
          finally
            setErrorMode(OldWinapiErrorMode);
          end;
      {$ELSE WINDOWS}
          BooleanResult := FileExists(s2) or DirectoryExists(s2);
          if (not BooleanResult) and (not (trim(s2) = '')) then
          begin
            list1.Clear;
            list1.Text := execShellCall('ls ' + s2, 'sysnative', 4, False, False).Text;
            //calling shellCall with FetchExitCodePublic=false result is on FLastPrivateExitCode
            if (0 = FLastPrivateExitCode) then
              BooleanResult := True;
          end;
      {$ENDIF WINDOWS}
          if not BooleanResult then
          begin
            RunTimeInfo := 'Not found: "' + s1 + '": ' + RunTimeInfo;
            LogDatei.log(RunTimeInfo, LLinfo);
          end;
          syntaxCheck := True;
        end;
  end

  else if Skip('DirectoryExists', Input, r, sx) then
  begin
    tmpbool := True; // default to sysnative
    LogDatei.log_prog('DirectoryExists from: ' + r, LLdebug3);
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, tmpstr, s1, InfoSyntaxError)
      // next after ',' or ')'
      then
        if Skip(',', tmpstr, tmpstr1, tmpstr3) then
          if EvaluateString(tmpstr1, tmpstr2, s2, tmpstr3) then;
    if s2 = '' then
    begin
      // only one parameter
      if Skip(')', tmpstr, r, InfoSyntaxError) then
      begin
        syntaxCheck := True;
      end;
    end
    else
    begin
      // two parameter
      if Skip(')', tmpstr2, r, InfoSyntaxError) then
      begin
        syntaxCheck := True;
        try
          tmpbool := True;
          if lowercase(s2) = '32bit' then
            tmpbool := False
          else if lowercase(s2) = '64bit' then
            tmpbool := True
          else if lowercase(s2) = 'sysnative' then
            tmpbool := True
          else
          begin
            InfoSyntaxError :=
              'Error: unknown parameter: ' + s2 +
              ' expected one of 32bit,64bit,sysnative - fall back to sysnative';
            syntaxCheck := False;
          end;
        except
          Logdatei.log('Error: Exception in GetRegistryValue: ', LLError);
          syntaxCheck := False;
        end;
      end;
    end;
    if syntaxcheck then
    begin
      s1 := ExpandFileName(s1);
      LogDatei.log('Starting query if directory exist ...', LLInfo);
      tmpstr := s1;
      if (length(s1) > 0) and (s1[length(s1)] = PATHSEPARATOR) then
        tmpstr := copy(s1, 1, length(s1) - 1);
      {$IFDEF WIN32}
      try
        tmpbool1 := True;
        if Is64BitSystem and tmpbool then
        begin
          if not DSiDisableWow64FsRedirection(oldDisableWow64FsRedirectionStatus) then
          begin
            LogDatei.log('Error: DisableWow64FsRedirection failed', LLError);
            BooleanResult := False;
            tmpbool1 := False;
          end
          else
          begin
            tmpbool1 := True;
            LogDatei.log('DisableWow64FsRedirection succeeded', LLinfo);
          end;
        end;
        if tmpbool1 then
          // disable  critical-error-handler message box. (Drive not ready)
          OldWinapiErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
        try
          try
            tmpstr := trim(tmpstr);
            BooleanResult := DirectoryExists(tmpstr);
            if (not BooleanResult) and (not (trim(tmpstr) = '')) then
            begin
              LogDatei.log('Directory: ' + tmpstr + ' not found via DirectoryExists',
                LLDebug3);
            end;
          except
            BooleanResult := False;
          end;
        finally
          setErrorMode(OldWinapiErrorMode);
        end;
        if Is64BitSystem and tmpbool then
        begin
          dummybool := DSiRevertWow64FsRedirection(oldDisableWow64FsRedirectionStatus);
          LogDatei.log('RevertWow64FsRedirection succeeded', LLinfo);
        end;
      except
        on ex: Exception do
        begin
          LogDatei.log('Error: ' + ex.message, LLError);
        end;
      end;
      {$ENDIF WINDOWS}
      {$IFDEF UNIX}
      BooleanResult := DirectoryExists(tmpstr);
      {$ENDIF UNIX}
      syntaxCheck := True;
    end;
  end


 {$IFDEF WINDOWS}

  // XMLAddNamespace(Datei:string,ElementName:string,Namespace:string):Boolean
  // True, if Namespace was not present and have to be inserted
  else if Skip('XMLAddNamespace', Input, r, sx) then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(',', r, r, InfoSyntaxError) then
          if EvaluateString(r, r, s2, InfoSyntaxError) then
            if Skip(',', r, r, InfoSyntaxError) then
              if EvaluateString(r, r, s3, InfoSyntaxError) then
                if Skip(')', r, r, InfoSyntaxError) then
                begin
                  syntaxCheck := True;
                  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

                  LogDatei.log(' Adding NameSpace (if not existing) "' +
                    s3 + '" for element "' + s2 + '" in file "' + s1 +
                    '"', LevelComplete);

                  BooleanResult := False;
                  try
                    BooleanResult := doXMLAddNamespace(s1, s2, s3);
                  except
                    on ex: Exception do
                    begin
                      LogDatei.log('Error: ' + ex.message, LLError);
                    end;
                  end;

                  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
                end;
  end

  //XMLRemoveNamespace(Datei:string,ElementName:string,Namespace:string):Boolean
  //  True, if the  Namespace was successful removed
  else if Skip('XMLRemoveNamespace', Input, r, sx) then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(',', r, r, InfoSyntaxError) then
          if EvaluateString(r, r, s2, InfoSyntaxError) then
            if Skip(',', r, r, InfoSyntaxError) then
              if EvaluateString(r, r, s3, InfoSyntaxError) then
                if Skip(')', r, r, InfoSyntaxError) then
                begin
                  syntaxCheck := True;
                  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
                  LogDatei.log
                  (' Removing NameSpace "' + s3 + '" for element "' +
                    s2 + '" in file "' + s1 + '"',
                    LevelComplete);

                  BooleanResult := False;
                  try
                    BooleanResult := doXMLRemoveNamespace(s1, s2, s3);
                  except
                    on ex: Exception do
                    begin
                      LogDatei.log('Error: ' + ex.message, LLError);
                    end;
                  end;
                  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
                end;
  end
 {$ENDIF WINDOWS}

  else if Skip('xml2NodeExistsByPathInXMLFile', Input, r, sx) then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(',', r, r, InfoSyntaxError) then
          if EvaluateString(r, r, s2, InfoSyntaxError) then
            if Skip(',', r, r, InfoSyntaxError) then
              if EvaluateString(r, r, s3, InfoSyntaxError) then
                if Skip(')', r, r, InfoSyntaxError) then
                begin
                  syntaxCheck := True;
                  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
                  LogDatei.log('xml2NodeExistsByPathInXMLFile in File "' +
                    s1 + '" path "' + s2 + '"  strict mode: "' + s3 + '"', LLInfo);
                  try
                    s1 := ExpandFileNameUTF8(s1);
                    BooleanResult :=
                      nodeExistsByPathInXMLFile(s1, s2, StrToBool(s3), InfoSyntaxError);
                  except
                    on ex: Exception do
                    begin
                      LogDatei.log(
                        'Error: Exception around xml2NodeExistsByPathInXMLFile' +
                        ex.message, LLError);
                    end;
                  end;
                end;
  end

  else if Skip('LineBeginning_ExistsIn', Input, r, sx) then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(',', r, r, InfoSyntaxError) then
          if EvaluateString(r, r, s2, InfoSyntaxError) then
            if Skip(')', r, r, InfoSyntaxError) then
            begin
              syntaxCheck := True;
              LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
              LogDatei.log('Searching "' + s1 + '" in File "' + s2 + '"', LevelComplete);
              try
                Textfile := TPatchList.Create;
                Textfile.Clear;
                Textfile.ItemPointer := -1;
                s2 := ExpandFileName(s2);
                Textfile.LoadFromFile(s2);
                Textfile.Text := reencode(Textfile.Text, 'system');
                BooleanResult := (Textfile.FindFirstItemStartingWith(s1, False, -1) >= 0)
              except
                on ex: Exception do
                begin
                  LogDatei.log('Error: ' + ex.message, LLError);
                end;
              end;

              LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
            end;
  end

  else if Skip('LineExistsIn', Input, r, sx) then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(',', r, r, InfoSyntaxError) then
          if EvaluateString(r, r, s2, InfoSyntaxError) then
            if Skip(')', r, r, InfoSyntaxError) then
            begin
              syntaxCheck := True;

              LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
              LogDatei.log('Searching "' + s1 + '" in File "' + s2 + '"', LevelComplete);
              try
                Textfile := TPatchList.Create;
                Textfile.Clear;
                Textfile.ItemPointer := -1;
                s2 := ExpandFileName(s2);
                Textfile.LoadFromFile(s2);
                Textfile.Text := reencode(Textfile.Text, 'system');

                BooleanResult :=
                  (Textfile.FindFirstItem(s1, False, -1, BooleanResult0) >= 0)
              except
                on ex: Exception do
                begin
                  LogDatei.log('Error: ' + ex.message, LLError);
                end;
              end;

              LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
            end;
  end

  else if Skip('LineContaining_ExistsIn', Input, r, sx) then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(',', r, r, InfoSyntaxError) then
          if EvaluateString(r, r, s2, InfoSyntaxError) then
            if Skip(')', r, r, InfoSyntaxError) then
            begin
              syntaxCheck := True;

              LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
              LogDatei.log('Searching "' + s1 + '" in File "' + s2 + '"', LevelComplete);
              try
                Textfile := TPatchList.Create;
                Textfile.Clear;
                Textfile.ItemPointer := -1;
                s2 := ExpandFileName(s2);
                Textfile.LoadFromFile(s2);
                Textfile.Text := reencode(Textfile.Text, 'system');

                BooleanResult := (Textfile.FindFirstItemWith(s1, False, -1) >= 0)
              except
                on ex: Exception do
                begin
                  LogDatei.log('Error: ' + ex.message, LLError);
                end;
              end;

              LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
            end;
  end

  else if Skip('contains', Input, r, sx) then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(',', r, r, InfoSyntaxError) then
          if EvaluateString(r, r, s2, InfoSyntaxError) then
            if Skip(')', r, r, InfoSyntaxError) then
            begin
              syntaxCheck := True;
              BooleanResult := strContains(s1, s2);
            end;
  end

  else if Skip('isRegexMatch', Input, r, sx) then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(',', r, r, InfoSyntaxError) then
          if EvaluateString(r, r, s2, InfoSyntaxError) then
            if Skip(')', r, r, InfoSyntaxError) then
            begin
              syntaxCheck := True;
              BooleanResult := isRegexMatch(s1, s2);
            end;
  end

  else if Skip('isValidIP4Network', Input, r, sx) then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(',', r, r, InfoSyntaxError) then
          if EvaluateString(r, r, s2, InfoSyntaxError) then
            if Skip(')', r, r, InfoSyntaxError) then
            begin
              syntaxCheck := True;
              BooleanResult := isValidIP4Network(s1, s2);
            end;
  end


  else if Skip('isValidIP4Host', Input, r, sx) then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(',', r, r, InfoSyntaxError) then
          if EvaluateString(r, r, s2, InfoSyntaxError) then
            if Skip(')', r, r, InfoSyntaxError) then
            begin
              syntaxCheck := True;
              BooleanResult := isValidIP4Host(s1, s2);
            end;
  end

  else if Skip('waitForPackageLock', Input, r, sx) then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(',', r, r, InfoSyntaxError) then
          if EvaluateString(r, r, s2, InfoSyntaxError) then
            if Skip(')', r, r, InfoSyntaxError) then
            begin
              syntaxCheck := True;
     {$IFDEF UNIX}
              BooleanResult := False;
              try
                tmpint := StrToInt(s1);
                tmpbool := StrToBool(s2);
                BooleanResult := getPackageLock(tmpint, tmpbool);
              except
                on ex: Exception do
                begin
                  LogDatei.log('Error osparser at waitForPackageLock: with: ' +
                    s1 + ',' + s2, LLError);
                  LogDatei.log('Exception in waitForPackageLock: Error: ' +
                    ex.message, LLError);
                end;
              end;
     {$ELSE LINUX}
              LogDatei.log(
                'Error waitForPackageLock: not implemented for Windows; result=true: ',
                LLError);
              BooleanResult := True;
     {$ENDIF LINUX}
            end;
  end


  else if Skip('sendlog', Input, r, sx) then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(',', r, r, InfoSyntaxError) then
          if EvaluateString(r, r, s2, InfoSyntaxError) then
            if Skip(',', r, r, InfoSyntaxError) then
              if EvaluateString(r, r, s3, InfoSyntaxError) then
                if Skip(')', r, r, InfoSyntaxError) then
                begin
                  syntaxCheck := True;
                  if local_opsidata <> nil then
                  begin
                    logdatei.Appendmode := True;
                    // switch append mode on in order to avoid that a final log removes this one
                    local_opsidata.setActualClient(s1);
                    if 'true' = LowerCase(s3) then
                      BooleanResult := local_opsidata.sendLog(s2, True)
                    else
                      BooleanResult := local_opsidata.sendlog(s2, False);
                  end
                  else
                  begin
                    LogDatei.log('Service not initialized', LLError);
                  end;
                end;
  end

  else if Skip('savetextfilewithencoding', Input, r, sx) then
  begin
    try
      BooleanResult := False;
      list1 := TXStringList.Create;
      if Skip('(', r, r, InfoSyntaxError) then
        if produceStringList(script, r, r, list1, InfoSyntaxError) then
          if Skip(',', r, r, InfoSyntaxError) then
            if EvaluateString(r, r, s1, InfoSyntaxError) then
              if Skip(',', r, r, InfoSyntaxError) then
                if EvaluateString(r, r, s2, InfoSyntaxError) then
                  if Skip(')', r, r, InfoSyntaxError) then
                  begin
                    syntaxCheck := True;
                    try
                      s1 := ExpandFileName(s1);
                      BooleanResult := list1.FuncSaveToFile(s1, s2);
                    except
                      logdatei.log('Error: Could not write list to filename: ' +
                        s1, LLError);
                    end;
                  end;
    finally
      list1.Free;
      list1 := nil;
    end;
  end



  else if Skip('savetextfile', Input, r, sx) then
  begin
    try
      BooleanResult := False;
      list1 := TXStringList.Create;
      if Skip('(', r, r, InfoSyntaxError) then
        if produceStringList(script, r, r, list1, InfoSyntaxError) then
          if Skip(',', r, r, InfoSyntaxError) then
            if EvaluateString(r, r, s1, InfoSyntaxError) then
              if Skip(')', r, r, InfoSyntaxError) then
              begin
                syntaxCheck := True;
                try
                  s1 := ExpandFileName(s1);
                  BooleanResult := list1.FuncSaveToFile(s1);
                except
                  logdatei.log('Error: Could not write list to filename: ' +
                    s1, LLError);
                end;
              end;
    finally
      list1.Free;
      list1 := nil;
    end;
  end


  else if Skip('isNumber', Input, r, InfoSyntaxError) then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          syntaxCheck := True;
          try
            BooleanResult := TryStrToInt64(s1, int64result);
          except
            BooleanResult := False;
          end;
        end;
  end

  else if Skip('isPingReachable', Input, r, InfoSyntaxError) then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          syntaxCheck := True;
          try
            if pinghost(s1) <> -1 then
              BooleanResult := True
            else
              BooleanResult := False;
          except
            BooleanResult := False;
          end;
        end;
  end



  else if Skip('isValidIP4', Input, r, InfoSyntaxError) then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          syntaxCheck := True;
          try
            BooleanResult := isValidIP4(s1);
          except
            BooleanResult := False;
          end;
        end;
  end


  else if Skip('isConfidential', Input, r, InfoSyntaxError) then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          syntaxCheck := True;
          try
            BooleanResult := logdatei.isConfidential(s1);
          except
            BooleanResult := False;
          end;
        end;
  end

 (*
 else if Skip ('isValidUtf8String', Input, r, InfoSyntaxError)
 then
 begin
    if Skip ('(', r, r, InfoSyntaxError)
    then if EvaluateString (r, r, s1, InfoSyntaxError)
    then if Skip (')', r, r, InfoSyntaxError)
    then
    Begin
      syntaxCheck := true;
      try
        BooleanResult := isValidUtf8String(s1);
      except
        BooleanResult := false;
      end
    end;
 end
 *)

  else if Skip('processIsRunning', Input, r, InfoSyntaxError) then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          syntaxCheck := True;
          BooleanResult := False;
          try
            list1.Text := getProcesslist.Text;
            for i := 0 to list1.Count - 1 do
            begin
              if strContains(list1.Strings[i], s1) then
                BooleanResult := True;
            end;
          except
            logdatei.log('Error: Exception in processIsRunning:  ' + s1, LLError);
            BooleanResult := False;
          end;
        end;
  end


  else if Skip('jsonIsValid', Input, r, InfoSyntaxError) then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          syntaxCheck := True;
          try
            BooleanResult := jsonIsValid(s1);
          except
            BooleanResult := False;
            logdatei.log('Error: Exception at jsonIsValid with : "' + s1 + '"', LLError);
          end;
        end;
  end


  else if Skip('jsonIsArray', Input, r, InfoSyntaxError) then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          syntaxCheck := True;
          try
            BooleanResult := jsonIsArray(s1);
          except
            BooleanResult := False;
            logdatei.log('Error: Exception at jsonIsArray with : "' + s1 + '"', LLError);
          end;
        end;
  end

  else if Skip('jsonIsObject', Input, r, InfoSyntaxError) then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          syntaxCheck := True;
          try
            BooleanResult := jsonIsObject(s1);
          except
            BooleanResult := False;
            logdatei.log('Error: Exception at jsonIsObject with : "' +
              s1 + '"', LLError);
          end;
        end;
  end

  else if Skip('jsonAsObjectHasKey', Input, r, sx) then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(',', r, r, InfoSyntaxError) then
          if EvaluateString(r, r, s2, InfoSyntaxError) then
            if Skip(')', r, r, InfoSyntaxError) then
            begin
              try
                syntaxCheck := True;
                BooleanResult := jsonAsObjectHasKey(s1, s2);
              except
                BooleanResult := False;
                logdatei.log('Error: Exception at jsonAsObjectHasKey with : "' +
                  s1 + '","' + s2 + '"', LLError);
              end;
            end;
  end

  else if Skip('stringToBool', Input, r, InfoSyntaxError) then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          syntaxCheck := True;
          try
            BooleanResult := StrToBool(s1);
          except
            BooleanResult := False;
            logdatei.log(
              'Error: stringToBool: given string expression is not a boolean value: "' +
              s1 + ' Defaulting to false', LLError);
          end;
        end;
  end
(*
 else  if AnsiStartsText('CompareDotSeparatedNumbers', trim(Input))
       and AnsiContainsText(Input,'(')
       and (NPos(',',Input,2) > 0)
       and (NPos(',',Input,2) < AnsiPos(')',Input))
       and AnsiContainsText(Input,')')
*)
  else if Skip('CompareDotSeparatedNumbers', InputBakup, r, InfoSyntaxError) and
    Skip('(', r, r, InfoSyntaxError) and EvaluateString(r, r, s1, InfoSyntaxError) and
    Skip(',', r, r, InfoSyntaxError) and EvaluateString(r, r, s1, InfoSyntaxError) and
    Skip(',', r, r, InfoSyntaxError) and EvaluateString(r, r, s1, InfoSyntaxError) and
    Skip(')', r, r, InfoSyntaxError) then
  begin
    LogDatei.log('The String: ' + Input +
      ' looks like the boolean CompareDotSeparatedNumbers function', LLDebug3);
    if Skip('CompareDotSeparatedNumbers', Input, r, InfoSyntaxError) then
    begin
      try
        if Skip('(', r, r, InfoSyntaxError) then
          if EvaluateString(r, r, s1, InfoSyntaxError) then
            if Skip(',', r, r, InfoSyntaxError) then
              if EvaluateString(r, r, s2, InfoSyntaxError) then
                if Skip(',', r, r, InfoSyntaxError) then
                  if EvaluateString(r, r, s3, InfoSyntaxError) then
                    if Skip(')', r, r, InfoSyntaxError) then
                    begin
                      syntaxCheck := True;
                      s2 := trim(s2);
                      LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
                      LogDatei.log('Checking if "' + s1 + '" is "' +
                        s2 + '" than / as "' + s3 + '"', LLDebug);
                      BooleanResult := False;
                      if getDecimalCompareSign(s1, s3, intresult, sx, False) then
                      begin
                        if s2 = '<' then
                        begin
                          case intresult of
                            -1: BooleanResult := True;   // s1 < s3
                            0: BooleanResult := False;   // s1 = s3
                            1: BooleanResult := False;   // s1 > s3
                          end;
                        end
                        else if (s2 = '<=') or (s2 = '=<') then
                        begin
                          case intresult of
                            -1: BooleanResult := True;   // s1 < s3
                            0: BooleanResult := True;   // s1 = s3
                            1: BooleanResult := False;   // s1 > s3
                          end;
                        end
                        else if s2 = '=' then
                        begin
                          case intresult of
                            -1: BooleanResult := False;   // s1 < s3
                            0: BooleanResult := True;   // s1 = s3
                            1: BooleanResult := False;   // s1 > s3
                          end;
                        end
                        else if (s2 = '>=') or (s2 = '=>') then
                        begin
                          case intresult of
                            -1: BooleanResult := False;   // s1 < s3
                            0: BooleanResult := True;   // s1 = s3
                            1: BooleanResult := True;   // s1 > s3
                          end;
                        end
                        else if s2 = '>' then
                        begin
                          case intresult of
                            -1: BooleanResult := False;   // s1 < s3
                            0: BooleanResult := False;   // s1 = s3
                            1: BooleanResult := True;   // s1 > s3
                          end;
                        end
                        else
                        begin
                          syntaxCheck := False;
                          LogDatei.log(
                            'Error: relation operator [<,<=,=,>=,>] expected but we got :'
                            + s2, LLError);
                          InfoSyntaxError :=
                            'Error: relation operator [<,<=,=,>=,>] expected but we got :'
                            + s2;
                        end;
                      end
                      else
                      begin
                        //syntaxCheck := false;
                        LogDatei.log('Error: ' + sx, LLError);
                        LogDatei.log(
                          'Error: CompareDotSeparatedNumbers: using default result=false',
                          LLError);
                        BooleanResult := False;
                      end;
                    end;
      except
        on ex: Exception do
        begin
          LogDatei.log('Error: Exception in CompareDotSeparatedNumbers (bool) ' +
            ex.message, LLError);
        end;
      end;
      LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
    end;
  end

  else if Skip('CompareDotSeparatedStrings', InputBakup, r, InfoSyntaxError) and
    Skip('(', r, r, InfoSyntaxError) and EvaluateString(r, r, s1, InfoSyntaxError) and
    Skip(',', r, r, InfoSyntaxError) and EvaluateString(r, r, s1, InfoSyntaxError) and
    Skip(',', r, r, InfoSyntaxError) and EvaluateString(r, r, s1, InfoSyntaxError) and
    Skip(')', r, r, InfoSyntaxError) then
  begin
    LogDatei.log('The String: ' + Input +
      ' looks like the boolean CompareDotSeparatedStrings function', LLDebug3);
    if Skip('CompareDotSeparatedStrings', Input, r, InfoSyntaxError) then
    begin
      try
        if Skip('(', r, r, InfoSyntaxError) then
          if EvaluateString(r, r, s1, InfoSyntaxError) then
            if Skip(',', r, r, InfoSyntaxError) then
              if EvaluateString(r, r, s2, InfoSyntaxError) then
                if Skip(',', r, r, InfoSyntaxError) then
                  if EvaluateString(r, r, s3, InfoSyntaxError) then
                    if Skip(')', r, r, InfoSyntaxError) then
                    begin
                      syntaxCheck := True;
                      s2 := trim(s2);
                      LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
                      LogDatei.log('Checking if "' + s1 + '" is "' +
                        s2 + '" than / as "' + s3 + '"', LLDebug);
                      BooleanResult := False;
                      if getDecimalCompareSign(s1, s3, intresult, sx, True) then
                      begin
                        if s2 = '<' then
                        begin
                          case intresult of
                            -1: BooleanResult := True;   // s1 < s3
                            0: BooleanResult := False;   // s1 = s3
                            1: BooleanResult := False;   // s1 > s3
                          end;
                        end
                        else if (s2 = '<=') or (s2 = '=<') then
                        begin
                          case intresult of
                            -1: BooleanResult := True;   // s1 < s3
                            0: BooleanResult := True;   // s1 = s3
                            1: BooleanResult := False;   // s1 > s3
                          end;
                        end
                        else if s2 = '=' then
                        begin
                          case intresult of
                            -1: BooleanResult := False;   // s1 < s3
                            0: BooleanResult := True;   // s1 = s3
                            1: BooleanResult := False;   // s1 > s3
                          end;
                        end
                        else if (s2 = '>=') or (s2 = '=>') then
                        begin
                          case intresult of
                            -1: BooleanResult := False;   // s1 < s3
                            0: BooleanResult := True;   // s1 = s3
                            1: BooleanResult := True;   // s1 > s3
                          end;
                        end
                        else if s2 = '>' then
                        begin
                          case intresult of
                            -1: BooleanResult := False;   // s1 < s3
                            0: BooleanResult := False;   // s1 = s3
                            1: BooleanResult := True;   // s1 > s3
                          end;
                        end
                        else
                        begin
                          syntaxCheck := False;
                          LogDatei.log(
                            'Error: relation operator [<,<=,=,>=,>] expected but we got :'
                            + s2, LLError);
                          InfoSyntaxError :=
                            'Error: relation operator [<,<=,=,>=,>] expected but we got :'
                            + s2;
                        end;
                      end
                      else
                      begin
                        syntaxCheck := False;
                        LogDatei.log('Error: ' + sx, LLError);
                      end;
                    end;
      except
        on ex: Exception do
        begin
          LogDatei.log('Error: Exception in CompareDotSeparatedStrings (bool) ' +
            ex.message, LLError);
        end;
      end;
      LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
    end;
  end


 {$IFDEF WINDOWS}
  else if Skip('IsDriveReady', Input, r, InfoSyntaxError) then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(')', r, r, InfoSyntaxError) then
        begin
          syntaxCheck := True;
          try
            BooleanResult := IsDriveReady(s1);
          except
            BooleanResult := False;
          end;
        end;
  end

  else if Skip('RegKeyExists', Input, r, InfoSyntaxError) then
  begin
    s2 := '';
    tmpstr2 := '';
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, tmpstr, s1, InfoSyntaxError)
      // next after , or )
      //then GetWord(tmpstr,s2,r,WordDelimiterSet6);
      then
        if Skip(',', tmpstr, tmpstr1, tmpstr3) then
          if EvaluateString(tmpstr1, tmpstr2, s2, tmpstr3) then;
    if s2 = '' then
    begin
      // only one parameter
      if Skip(')', tmpstr, r, InfoSyntaxError) then
      begin
        syntaxCheck := True;
        try
          BooleanResult := RegKeyExists(s1, True);
        except
          BooleanResult := False;
        end;
      end;
    end
    else
    begin
      if Skip(')', tmpstr2, r, InfoSyntaxError) then
      begin
        syntaxCheck := True;
        try
          tmpbool := True;
          if lowercase(s2) = '32bit' then
            tmpbool := False
          else if lowercase(s2) = '64bit' then
            tmpbool := True
          else if lowercase(s2) = 'sysnative' then
            tmpbool := True
          else
            Logdatei.log('Error: unknown modifier: ' + s2 +
              ' expected one of 32bit,64bit,sysnative - fall back to sysnative',
              LLError);
          BooleanResult := RegKeyExists(s1, tmpbool);
        except
          BooleanResult := False;
        end;
      end;
    end;
  end

  else if Skip('RegVarExists', Input, r, InfoSyntaxError) then
  begin
    s2 := '';
    tmpstr2 := '';
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(',', r, r, InfoSyntaxError) then
          if EvaluateString(r, tmpstr, s2, InfoSyntaxError)
          // next after , or )
          //then GetWord(tmpstr,s2,r,WordDelimiterSet6);
          then
            if Skip(',', tmpstr, tmpstr1, tmpstr3) then
              if EvaluateString(tmpstr1, tmpstr2, s3, tmpstr3) then;
    if s3 = '' then
    begin
      // only one parameter
      if Skip(')', tmpstr, r, InfoSyntaxError) then
      begin
        syntaxCheck := True;
        try
          BooleanResult := RegVarExists(s1, s2, True);
        except
          BooleanResult := False;
        end;
      end;
    end
    else
    begin
      if Skip(')', tmpstr2, r, InfoSyntaxError) then
      begin
        syntaxCheck := True;
        try
          tmpbool := True;
          if lowercase(s3) = '32bit' then
            tmpbool := False
          else if lowercase(s3) = '64bit' then
            tmpbool := True
          else if lowercase(s3) = 'sysnative' then
            tmpbool := True
          else
            Logdatei.log('Error: unknown modifier: ' + s3 +
              ' expected one of 32bit,64bit,sysnative - fall back to sysnative',
              LLError);
          BooleanResult := RegVarExists(s1, s2, tmpbool);
        except
          BooleanResult := False;
        end;
      end;
    end;
  end


 {$ENDIF WINDOWS}

  else if (Skip('ErrorsOccuredSinceMark ', Input, r, sx) or
    Skip('ErrorsOccurredSinceMark ', Input, r, sx)) then
  begin
    getword(r, relationSymbol, r, WordDelimiterWhiteSpace);
    try
      errNumber := StrToInt(r);
      syntaxcheck := True;
      r := '';
    except
      InfoSyntaxError := r + ' is not a number'
    end;

    syntaxCheck :=
      syntaxcheck and hasRelation(relationSymbol,
      getCompareSign(Logdatei.NumberOfErrors - Logdatei.ErrorNumberMarked,
      errNumber), BooleanResult, InfoSyntaxError);
    LogDatei.log('(TotalErrors: ' + IntToStr(Logdatei.NumberOfErrors) +
      ' - ErrorMark: ' + IntToStr(Logdatei.ErrorNumberMarked) + ') ' +
      relationSymbol + ' ' + IntToStr(errNumber) + ' -> ' +
      BoolToStr(BooleanResult), LLDebug2);
  end

  else if Skip('opsiLicenseManagementEnabled', Input, r, InfoSyntaxError) then
  begin
    booleanResult := False;
    Syntaxcheck := True;
    errorOccured := False;
    if opsidata = nil then
    begin
      errorOccured := True;
    end
    else
    begin
      try
        local_opsidata := opsidata;

        //LogDatei.log('Calling opsi service at ' + local_opsidata.serviceUrl, LevelComplete);
      except
        errorOccured := True;
      end;
    end;

    if not errorOccured then
    begin
      booleanresult := local_opsidata.withLicenceManagement;
    end;
  end

  else if Skip('runningAsAdmin', Input, r, InfoSyntaxError) then
  begin
    Syntaxcheck := True;
    errorOccured := False;
    booleanresult := runningasadmin;
  end

  else if Skip('isLoginScript', Input, r, InfoSyntaxError) then
  begin
    Syntaxcheck := True;
    errorOccured := False;
    booleanresult := runloginscripts;
  end

  else if Skip('runningOnUefi', Input, r, InfoSyntaxError) then
  begin
    Syntaxcheck := True;
    errorOccured := False;
    booleanresult := isUefi;
  end

  else if Skip('runningInPE', Input, r, InfoSyntaxError) then
  begin
    Syntaxcheck := True;
    errorOccured := False;
    booleanresult := isWinPE;
  end

  else if Skip('runningWithGui', Input, r, InfoSyntaxError) then
  begin
    Syntaxcheck := True;
    errorOccured := False;
    booleanresult := isGUI;
  end


  else if Skip('scriptWasExecutedBefore', Input, r, InfoSyntaxError) then
  begin
    Syntaxcheck := True;
    errorOccured := False;
    booleanresult := scriptWasExecutedBefore;
  end



  else if Skip('HasMinimumSpace', Input, r, sx) then
  begin
    if Skip('(', r, r, InfoSyntaxError) then
      if EvaluateString(r, r, s1, InfoSyntaxError) then
        if Skip(',', r, r, InfoSyntaxError) then
          if EvaluateString(r, r, s2, InfoSyntaxError) then
            if Skip(')', r, r, InfoSyntaxError) then
            begin
              syntaxCheck := True;

              LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

              einheit := 1;
              j := pos('kb', lowercase(s2));
              if j > 0 then
                einheit := 1000
              else
              begin
                j := pos('mb', lowercase(s2));
                if j > 0 then
                  einheit := 1000 * 1000
                else
                begin
                  j := pos('gb', lowercase(s2));
                  if j > 0 then
                    einheit := 1000 * 1000 * 1000;
                end;
              end;


              if j = 0 then
                try
                  requiredbytes := strtoint64(s2);
                except
                  RunTimeInfo := '"' + s2 + '" is not a valid number"';
                  syntaxCheck := False;
                end
              else
                try
                  sx := cutRightBlanks(copy(s2, 1, j - 1));
                  requiredbytes := strtoint64(sx);
                  requiredbytes := requiredbytes * einheit;
                except
                  RunTimeInfo := '"' + s2 + '" is not a valid number"';
                  syntaxCheck := False;
                end;

              if syntaxCheck then // parse s1 in order to get the drive char
              begin
                if (length(s1) = 2) and (s1[2] = ':') then
                  drivenumber := Ord(uppercase(s1)[1]) - Ord('A') + 1
                else
                begin
                  syntaxcheck := False;
                  RunTimeInfo := '"' + s1 + '" is not a valid drive"';
                end;
              end;

              if syntaxCheck then
                freebytes := diskfree(drivenumber);

              if syntaxCheck and (freebytes = -1) then
              begin
                syntaxCheck := False;
                RunTimeInfo := '"' + s1 + '" is not a valid drive"';
              end;

              if syntaxCheck then
              begin
                BooleanResult := (freebytes >= requiredBytes);
                RunTimeInfo :=
                  'Free on Disk ' + s1 + ': ' + formatInt(freebytes) + ' bytes';
                if BooleanResult then
                  RunTimeInfo := RunTimeInfo + '  This is more '
                else
                  RunTimeInfo := RunTimeInfo + '  This is less ';

                RunTimeInfo :=
                  RunTimeInfo + 'than the required amount of ' +
                  formatInt(requiredbytes) + ' bytes';
              end;

              LogDatei.log(RunTimeInfo, LLInfo);

              LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
            end;
  end

  (* Boolescher Ausdruck  s1 = s2 *)
  else if EvaluateString(Input, r, s1, InfoSyntaxError) then
  begin
    if skip('int', r, r, InfoSyntaxError) then
    begin
      if getOrdersymbol(r, relationsymbol, r, InfoSyntaxError) then
        if EvaluateString(r, r, s2, InfoSyntaxError) then
        begin
          syntaxCheck := True;
          OldNumberOfErrors := LogDatei.NumberOfErrors;

          try
            n1 := StrToInt(s1);
            try
              n2 := StrToInt(s2);
              comparesign := getCompareSign(n1, n2);
              syntaxCheck := hasRelation(relationsymbol,
                comparesign, BooleanResult, InfoSyntaxError);
            except
              LogDatei.log('Error: ' + s2 + ' has no Integer format', LLError);
            end;
          except
            LogDatei.log('Error: ' + s1 + ' has no Integer format', LLError)
          end;

          DiffNumberOfErrors := LogDatei.NumberOfErrors - OldNumberOfErrors;
          FNumberOfErrors := NumberOfErrors + DiffNumberOfErrors;

        end;
    end
    else
    begin
      if getOrdersymbol(r, relationsymbol, r, InfoSyntaxError) then
        if EvaluateString(r, r, s2, InfoSyntaxError) then
        begin
          comparesign := getCompareSignStrings(s1, s2);
          syntaxCheck := hasRelation(relationsymbol, comparesign,
            BooleanResult, InfoSyntaxError);
          syntaxCheck := True;
        end;
    end;

   {
   if Skip ('=', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s2, InfoSyntaxError)
   then
   Begin
     BooleanResult := (AnsiUpperCase (s1) = AnsiUpperCase (s2));
     syntaxCheck := true;
   End
   }
  end

  else
  if InfoSyntaxError = '' then
    InfoSyntaxError := 'not a well formed boolean Expressionstr';


  (*  concatenate  AND / OR  *)
  if syntaxCheck then
  begin
    if Skip('and', r, r, sx) then
    begin
      if EvaluateBoolean_(r, r, BooleanResult0, NestingLevel + 1, InfoSyntaxError) then
        BooleanResult := BooleanResult and BooleanResult0
      else
        syntaxCheck := False;
    end
    else
    if Skip('or', r, r, sx) then
    begin
      if EvaluateBoolean_(r, r, BooleanResult0, NestingLevel + 1, InfoSyntaxError) then
        BooleanResult := BooleanResult or BooleanResult0
      else
        syntaxCheck := False;
    end;
  end;

  Result := syntaxCheck;
  Remaining := r;

  RunTimeInfo := copy(Input, 1, length(Input) - length(Remaining));

  if Result then
  begin
    if BooleanResult then
      RunTimeInfo := RunTimeInfo + '   <<< result true'
    else
      RunTimeInfo := RunTimeInfo + '   <<< result false';
  end
  else
    RunTimeInfo := RunTimeInfo + '    <<< syntax error, no result!! - set to false';
  //LogDatei.log ('NestingLevel: '+IntToStr(NestingLevel), LLInfo);
  //LogDatei.log ('script.FEvalBoolBaseNestLevel: '+IntToStr(script.FEvalBoolBaseNestLevel), LLInfo);
  if NestingLevel = script.FEvalBoolBaseNestLevel then
  begin
    // This is the complete boolean expression
    LogDatei.log(RunTimeInfo, LLInfo);
  end
  else // These are the parts in the recursion
    LogDatei.log(RunTimeInfo, LLDebug);
  list1.Free;

end;

function TuibInstScript.doSetVar(const section: TuibIniScript;
  const Expressionstr: string; var Remaining: string;
  var InfoSyntaxError: string): boolean;
  //var
  // NestLevel : integer = 0;
begin
  Result := doSetVar(section, Expressionstr, Remaining, InfoSyntaxError, NestingLevel);
end;

function TuibInstScript.doSetVar(const section: TuibIniScript;
  const Expressionstr: string; var Remaining: string; var InfoSyntaxError: string;
  var NestLevel: integer): boolean;
var
  VarName: string = '';
  VarValue: string = '';
  r: string = '';
  VarIndex: integer = 0;
  list: TXStringList;
  funcindex: integer = 0;

  function isStringlistVar(varname: string): boolean;
  begin
    Result := False;
    if isVisibleLocalVar(VarName, funcindex) then
      if definedFunctionArray[FuncIndex].getLocalVarDatatype(varname) =
        dfpStringlist then
        Result := True;
    if listOfStringLists.IndexOf(LowerCase(VarName)) >= 0 then
      Result := True;
  end;

  function isStringVar(varname: string): boolean;
  begin
    Result := False;
    if isVisibleLocalVar(VarName, funcindex) then
      if definedFunctionArray[FuncIndex].getLocalVarDatatype(varname) = dfpString then
        Result := True;
    if Result = False then
      if VarList.IndexOf(LowerCase(VarName)) >= 0 then
        Result := True;
  end;

begin
  LogDatei.log_prog('Start doSetVar with expr: ' + Expressionstr, LLdebug);
  Result := False;
  GetWord(Expressionstr, VarName, r, WordDelimiterSet1);
  if VarName = '' then
    InfoSyntaxError := 'variable expected'
  else
  begin
    //VarIndex := listOfStringLists.IndexOf(LowerCase(VarName));
    if isStringlistVar(Varname) // we should get a StringList
    then
    begin
      if Skip('=', r, r, InfoSyntaxError) and
        produceStringList(section, r, Remaining, list, InfoSyntaxError,
        Nestlevel, inDefFuncIndex) then
        if isVisibleLocalVar(VarName, funcindex) then
        begin
          // local var
          Result := definedFunctionArray[FuncIndex].setLocalVarValueList(varname, list);
        end
        else
        begin
          try
            VarIndex := listOfStringLists.IndexOf(LowerCase(VarName));
            ContentOfStringLists.Items[VarIndex] := list;
            Result := True;
          except
          end;
        end;
      if Result then
      begin
        LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
        LogDatei.log('The value of the variable "' + Varname + '" is now:', LLDebug);
        LogDatei.log_list(list, LLDebug);
        LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
      end;
    end
    else
    begin // it must be a string value
      if not isStringVar(varname) then
        InfoSyntaxError := 'Unknown variable name: ' + VarName
      else
      begin
        if Skip('=', r, r, InfoSyntaxError) and
          EvaluateString(r, Remaining, VarValue, InfoSyntaxError,
          Nestlevel, inDefFuncIndex) then
        begin
          if isVisibleLocalVar(VarName, funcindex) then
          begin
            // local var
            Result := definedFunctionArray[FuncIndex].setLocalVarValueString(
              varname, VarValue);
          end
          else
          begin
            try
              VarIndex := VarList.IndexOf(LowerCase(VarName));
              ValuesList[VarIndex] := VarValue;
              Result := True;
            except
            end;
          end;
        end;
      end;

      if Result then
      begin
        LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
        LogDatei.log
        ('The value of the variable "' + Varname + '" is now: "' +
          VarValue + '"', LLInfo);
        LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
      end;
    end;
  end;
end;


{$IFDEF WINDOWS}
(* experimentell *)
function TuibInstScript.StartConnection(LocalName, NetResourceName: string;
  WaitTime: longint): TSectionResult;
var
  NetResource: TNetResourceA;
  waitedTime: longint = 0;
  showTime: longint = 0;
  waitTimeMilsec: longint = 0;
  netresult: DWord = 0;
  lastErrorCode: DWord = 0;
  ErrorInfo: string = '';
  ReportedUser: string = '';



  function netErrorMessage(errorcode: integer): string;

  var
    pErrorBuff, pNameBuff: PChar;
    nErrorBuffSize: DWord = 0;
    nNamebuffSize: DWord = 0;
    Error: string = '';
    Name: string = '';

  begin

    case errorcode of
      no_error: Result := 'no_error';
      ERROR_ACCESS_DENIED: Result :=
          'Access to the network resource was denied.';
      ERROR_ALREADY_ASSIGNED: Result :=
          'The local device specified by lpLocalName is already connected to a network resource.';
      ERROR_BAD_DEV_TYPE: Result :=
          'The type of local device and the type of network resource do not match.';
      ERROR_BAD_DEVICE: Result :=
          'The value specified by lpLocalName is invalid.';
      ERROR_BAD_NET_NAME: Result :=
          'The value specified by lpRemoteName is not acceptable to any network resource provider. '
          + 'The resource name is invalid, or the named resource cannot be located.';
      ERROR_BAD_PROFILE: Result :=
          'The user profile is in an incorrect format.';
      ERROR_BAD_PROVIDER: Result :=
          'The value specified by lpProvider does not match any provider.';
      ERROR_BUSY: Result :=
          'The router or provider is busy, possibly initializing. The caller should retry.';
      ERROR_CANCELLED: Result :=
          'The attempt to make the connection was cancelled by the user through a dialog box from one '
          + 'of the network resource providers, or by a called resource.';
      ERROR_CANNOT_OPEN_PROFILE: Result :=
          'The system is unable to open the user profile to process persistent connections.';
      ERROR_DEVICE_ALREADY_REMEMBERED: Result :=
          'An entry for the device specified in lpLocalName is already in the user profile.';
      ERROR_EXTENDED_ERROR:
      begin
        GetMem(pErrorBuff, 300);
        GetMem(pNameBuff, 300);
        WNetGetLastError(DWord(errorcode), pErrorBuff, nErrorBuffSize,
          pNameBuff, nNameBuffSize);
        Error := pErrorBuff;
        Result := Error;
        FreeMem(pErrorBuff);
        FreeMem(pNameBuff);
      end;
      ERROR_INVALID_PASSWORD: Result := 'The specified password is invalid.';
      ERROR_NO_NET_OR_BAD_PATH: Result :=
          'A network component has not started, or the specified name could not be handled.';
      ERROR_NO_NETWORK: Result := 'There is no network present.';
      else
        Result := ''
    end;
  end;

begin
  Result := tsrPositive;

  NetResource.dwType := RESOURCETYPE_DISK;
  NetResource.lpRemoteName := PAnsiChar(NetResourceName);
  NetResource.lpLocalName := PAnsiChar(LocalName);
  NetResource.lpProvider := '';

  LogDatei.log('Add Connection '  (* + 'User ' + Username *) + ' to ' +
    NetResourceName, LLinfo);
  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

  netresult := wNetAddConnection2(NetResource, nil (* PAnsiChar (Password) *),
    nil (* PAnsiChar (Username) *), 0);
  lastErrorCode := 0;

  if (netresult = no_error) then
  begin
    GetNetUser('', ReportedUser, ErrorInfo);
    ReportedUser := 'User ' + ReportedUser;
    LogDatei.log('Sec 0 ' + ReportedUser + ' Connected ' + ErrorInfo, LLinfo);
  end
  else
  begin
    lastErrorCode := GetLastError;
    GetNetUser('', ReportedUser, ErrorInfo);
    LogDatei.log('User ' + ReportedUser + ' ' + ErrorInfo, LLinfo);
    LogDatei.log('Sec 0  Connect error ' + IntToStr(lastErrorCode) +
      ': "' + RemoveLineBreaks(SysErrorMessage(lastErrorCode)) + '"', LLinfo);
    CentralForm.TimerWaitSet(100);
    // alle Zehntelsec Timer-Ereignis anschauen
  end;
  WaitedTime := 1000;
  ShowTime := 1000;
  WaitTimeMilsec := WaitTime * 1000;
  while (netresult <> no_error) and (WaitedTime < WaitTimeMilSec) do
  begin
    ProcessMess;
    if CentralForm.TimerWaitReady(10) then
      (* nach 10 Zehntelsec erneuten Versuch unternehmen *)
    begin
      netresult := wNetAddConnection2(NetResource, nil (* PAnsiChar (Password)*),
        nil (* PAnsiChar (Username)*), 0);
      if netresult = no_error then
        LogDatei.log('Sec ' + IntToStr(WaitedTime div 1000) + '  Connected', LLinfo)
      else
      begin
        if (lastErrorCode <> GetLastError) or (ShowTime > 2000) then
        begin
          lastErrorCode := GetLastError;
          ShowTime := 1000;
          LogDatei.log('Sec ' + IntToStr(WaitedTime div 1000) +
            '  Connect error ' + IntToStr(lastErrorCode) + ': "' +
            RemoveLineBreaks(SysErrorMessage(lastErrorCode)) + ' ' +
            netErrorMessage(lastErrorCode) + '"', LLinfo);
        end
        else
          ShowTime := ShowTime + 1000;

        CentralForm.TimerWaitSet(100);
        WaitedTime := WaitedTime + 1000;
      end;
    end;
  end;

  if WaitedTime >= WaitTimeMilSec then
  begin
    LogDatei.log('tried for ' + IntToStr(WaitedTime div 1000) + ' sec', LLinfo);
    LogDatei.log('Error: No Connect to ' + NetResourceName, LLError);

    DiffNumberOfErrors := 1;
  end
  else
    DiffNumberOfErrors := 0;

  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;

  if ExitOnError and (DiffNumberOfErrors > 0) then
    Result := tsrExitProcess;
end;

{$ENDIF WINDOWS}


procedure TuibInstScript.ApplyTextConstants(var Sektion: TXStringList;
  CStringEscaping: boolean);
var
  i: integer;
begin
  for i := 1 to ConstList.Count do
    if CStringEscaping then
      Sektion.GlobalReplace(1, Constlist.Strings[i - 1],
        CEscaping(ConstValuesList.Strings[i - 1]), False)
    else
      Sektion.GlobalReplace(1, Constlist.Strings[i - 1],
        ConstValuesList.Strings[i - 1], False);
end;


procedure TuibInstScript.ApplyTextVariables(var Sektion: TXStringList;
  CStringEscaping: boolean);
var
  i: integer;
  locallist: TStringList;
begin
  if inDefinedFuncNestCounter > 0 then
  begin
    // first replace local function vars
    locallist := TStringList.Create;
    locallist.Text := getVisibleLocalStringVarNameValueList.Text;
    for i := 1 to locallist.Count do
    begin
      if CStringEscaping then
        Sektion.GlobalReplace(1, locallist.Names[i - 1],
          CEscaping(locallist.ValueFromIndex[i - 1]), False)
      else
        Sektion.GlobalReplace(1, locallist.Names[i - 1],
          locallist.ValueFromIndex[i - 1], False);
    end;
    locallist.Free;
  end;

  // now the global vars
  for i := 1 to VarList.Count do
  begin
    if CStringEscaping then
      Sektion.GlobalReplace(1, Varlist.Strings[i - 1],
        CEscaping(ValuesList.Strings[i - 1]), False)
    else
      Sektion.GlobalReplace(1, Varlist.Strings[i - 1], ValuesList.Strings[i - 1], False);
  end;
end;

procedure TuibInstScript.ApplyTextVariablesToString(var mystr: string;
  CStringEscaping: boolean);
var
  i: integer;
  NewLine: string;
  locallist: TStringList;
begin
  if inDefinedFuncNestCounter > 0 then
  begin
    // first replace local function vars
    locallist := TStringList.Create;
    locallist.Text := getVisibleLocalStringVarNameValueList.Text;
    for i := 1 to locallist.Count do
    begin
      if CStringEscaping then
      begin
        if ReplaceInLine(mystr, locallist.Names[i - 1],
          CEscaping(locallist.ValueFromIndex[i - 1]), False, NewLine) then
        begin
          mystr := NewLine;
        end;
      end
      else
      begin
        if ReplaceInLine(mystr, locallist.Names[i - 1],
          locallist.ValueFromIndex[i - 1], False, NewLine) then
        begin
          mystr := NewLine;
        end;
      end;
    end;
    locallist.Free;
  end;

  // now the global vars
  for i := 1 to VarList.Count do
  begin
    if CStringEscaping then
    begin
      if ReplaceInLine(mystr, Varlist.Strings[i - 1],
        CEscaping(ValuesList.Strings[i - 1]), False, NewLine) then
      begin
        mystr := NewLine;
      end;
    end
    else
    begin
      if ReplaceInLine(mystr, Varlist.Strings[i - 1], ValuesList.Strings[i - 1],
        False, NewLine) then
      begin
        mystr := NewLine;
      end;
    end;
  end;
end;

procedure TuibInstScript.ApplyTextConstantsToString(var mystr: string;
  CStringEscaping: boolean);
var
  i: integer;
  NewLine: string = '';
begin
  //LogDatei.log_prog('ApplyTextConstantsToString - base: '+mystr,LLDebug2);
  for i := 1 to ConstList.Count do
  begin
    //LogDatei.log_prog('ApplyTextConstantsToString - const: '+Constlist.Strings [i-1],LLDebug2);
    if CStringEscaping then
    begin
      if ReplaceInLine(mystr, Constlist.Strings[i - 1],
        CEscaping(ConstValuesList.Strings[i - 1]), False, NewLine) then
      begin
        mystr := NewLine;
        //LogDatei.log_prog('ApplyTextConstantsToString - new: '+mystr,LLDebug2);
      end;
    end
    else
    begin
      if ReplaceInLine(mystr, Constlist.Strings[i - 1],
        ConstValuesList.Strings[i - 1], False, NewLine) then
      begin
        mystr := NewLine;
        //LogDatei.log_prog('ApplyTextConstantsToString - new: '+mystr,LLDebug2);
      end;
    end;
  end;
end;

function TuibInstScript.doAktionen(Sektion: TWorkSection;
  const CallingSektion: TWorkSection): TSectionResult;
var
  //i : integer=0;
  StartlineOfSection: integer = 0;
  Parameter: string = '';
  Filename: string = '';
  SectionSpecifier: TSectionSpecifier;
  posSlash: integer = 0;
  imageNo: integer = 0;
  ArbeitsSektion: TWorkSection;
  localSection: TWorkSection;
  WaitConditions: TSetWaitConditions;
  expr: string = '';
  ident: string = '';
  WaitSecs: word = 0;
  numberString: string = '';
  ActionResult: TSectionResult = 0;
  ActLevel: integer = 0;
  NestLevel: integer = 0;
  StatKind: TStatement;
  Expressionstr: string = '';
  remaining: string = '';
  registryformat: TRegistryFormat;
  reg_specified_basekey: string = '';
  flag_force64: boolean;
  BooleanResult: boolean;
  Info: string = '';
  ErrorInfo: string = '';
  InfoSyntaxError: string = '';
  SyntaxCheck: boolean;
  sign: integer = 0;
  evaluated: boolean;
  FName: string = '';
  Labeltext: string = '';
  localname: string = '';
  remotename: string = '';
  username: string = '';
  password: string = '';
  trytime: string = '';
  timeout: integer = 0;
  Fehlertext: string = '';
  output: TXStringlist;
  inputlist: TXStringList;
  inputname: string = '';
  looplist: TXStringList;
  loopvar: string = '';
  loopvalue: string = '';
  loopindex: integer = 0;
  inloop: boolean;
  continue: boolean;
  stayOnStatement: string = '';
  Startindentlevel: integer = 0;
  goon: boolean;
  sleepsecs: integer = 0;
  diffTime: TDateTime;
  Encoding2use, usedEncoding: string;
  ntuserpath: string = '';
  fullfilename: string;
  //filefound : boolean;

  //outlist : TXStringList;
  //versionI  : TVersionInfo; //from versioninfoX

  j: integer = 0;
  includelogLinecount: integer = 0;
  //includelogStrList : TStringList;
  logtailLinecount: integer = 0;
  logtailLinecountstr: string = '';
  logtailEncodingstr: string = 'system';
  includeLogLineStart: integer = 0;
  aktline: integer = 0;
  call: string = '';
  p1: string = '';
  p2: string = '';
  p3: string = '';
  p4: string = '';
  s1: string = '';
  s2: string = '';
  s3: string = '';
  s4: string = '';
  seconds: string = '';
  runAs: TRunAs;
  linecount, k, constcounter: integer;
  incfilename, fullincfilename, testincfilename, incline, myline, replacedline: string;
  //remaining, errorinfo, expr , numberString : string;
  incfile: TextFile;
  inclist: TStringList;
  found: boolean;
  //, doincludes, evaluated : boolean;
  loopstart, loopstop: integer;
  dummybool: boolean;
  switchCondition: string = '';
  switchExpressionstr: string = '';
  InSwitch: boolean = False;
  InCase: boolean = False;
  ValidCase: boolean = False;
  SwitchResolved: boolean = False;
  LppClientId: string = '';
  LppProductId: string = '';
  newDefinedfunction: TOsDefinedFunction;
  dummylist: TStringList;
  //endofDefFuncFound : boolean;
  inDefFunc: integer = 0;
  inDefFunc2: integer = 0;
  //inDefFunc3 : integer = 0;  // we are between deffunc and endfunc line (even in a not active code)
  funcindex, secindex: integer;
  importFunctionName: string;
  inSearchedFunc: boolean;
  alllines, inclines: integer;
  processline: boolean = True;
  // are we on a active code branch (needed handling of section ends '['
  tmplist: TXStringlist;
  secname: string;
  tmpint: integer;
  tmpstr, tmpstr1, tmpstr2, tmpstr3: string;
  tmpbool, tmpbool1: boolean;
  localKindOfStatement: Tstatement;
  linecounter: integer;
  numberOfSectionLines: integer;

{$IFDEF WINDOWS}
  function parseAndCallRegistry(ArbeitsSektion: TWorkSection;
    Remaining: string): TSectionResult;
  begin
    logdatei.log('Execution of: ' + ArbeitsSektion.Name + ' ' + Remaining, LLNotice);
    syntaxcheck := True;

    registryformat := trfWinst;
    flag_all_ntuser := False;
    flag_ntuser := False;
    reg_specified_basekey := '';
    flag_force64 := False;
    flag_all_usrclass := False;

    // if this is a 'ProfileActions' which is called as sub in Machine mode
    // so run registry sections implicit as /Allntuserdats
    if runProfileActions then
      flag_all_ntuser := True;

    remaining := CutRightBlanks(Remaining);

    if length(remaining) > 0 then
      goon := True;
    while goon do
    begin

      if skip(Parameter_AllNTUserDats, Remaining, Remaining, ErrorInfo) then
        flag_all_ntuser := True

      else if skip(Parameter_AllUsrClassDats, Remaining, Remaining, ErrorInfo) then
        flag_all_usrclass := True

      else if skip(Parameter_RegistryNTUserDat, Remaining, Remaining, ErrorInfo) then
      begin
        flag_ntuser := True;
        if not EvaluateString(Remaining, Remaining, ntuserpath, ErrorInfo) then
        begin
          syntaxcheck := False;
          //ActionResult := reportError (ErrorInfo);
        end;
      end
      else if skip(Parameter_Registry64Bit, Remaining, Remaining, ErrorInfo) then
      begin
        if (GetNTVersionMajor = 5) and (GetNTVersionMinor = 0) then
        begin
          // we are on win 2000 which can't handle redirections flags
          flag_force64 := False;
        end
        else // we are on xp or higher
        begin
          flag_force64 := True;
        end;
      end


      else if skip(Parameter_Registry32Bit, Remaining, Remaining, ErrorInfo) then
      begin
        if flag_all_ntuser then
        begin
          // no redirection in user profile registry
          // so always we are on sysnative
          LogDatei.log(
            'Ignoring parameter /32bit for /AllNTUserdats. This is always /sysnative',
            LLInfo);
          flag_force64 := True;
        end
        else
          flag_force64 := False;
      end

      else if skip(Parameter_RegistrySysNative, Remaining, Remaining, ErrorInfo) then
      begin
        if (GetNTVersionMajor = 5) and (GetNTVersionMinor = 0) then
        begin
          // we are on win 2000 which can't handle redirections flags
          flag_force64 := False;
        end
        else // we are on xp or higher
        begin
          flag_force64 := True;
        end;
      end


      else if skip(Parameter_RegistryUsercontext, Remaining, Remaining, ErrorInfo) then
        reg_specified_basekey := 'HKEY_USERS\' + usercontextSID

      else if skip(Parameter_RegistryBaseKey, Remaining, Remaining, ErrorInfo) and
        skip('=', Remaining, Remaining, ErrorInfo) and
        GetString(Remaining, s1, Remaining, ErrorInfo, False) then
        reg_specified_basekey := s1

      else if skip(Parameter_SysDiffAddReg, Remaining, Remaining, ErrorInfo) then
        registryformat := trfSysdiff

      else if skip(Parameter_RegeditFormat, Remaining, Remaining, ErrorInfo) then
        registryformat := trfRegedit

      else
      begin
        goon := False;
        if length(remaining) > 0 then
        begin
          syntaxcheck := False;
          ActionResult := reportError(Sektion, linecounter,
            Sektion.strings[linecounter - 1], '"' + remaining +
            '" is no valid parameter ');
        end;
      end;

    end;


    if (flag_all_ntuser or flag_all_usrclass) and (reg_specified_basekey <> '') then
    begin
      syntaxcheck := False;
      ErrorInfo := 'this combination of options is impossible';
    end;

    if inUsercontext then
      reg_specified_basekey := 'HKEY_USERS\' + usercontextSID;


    if syntaxcheck then
    begin
      if flag_all_ntuser then
      begin
        if registryformat = trfSysdiff then
          ActionResult := reportError(Sektion, linecounter,
            Sektion.strings[linecounter - 1], '"' + Remaining +
            '": sysdiff format not possible with option "for all nt user"')
        else
          ActionResult := doRegistryAllNTUserDats(ArbeitsSektion,
            registryformat, flag_force64);
      end
      else if flag_ntuser then
      begin
        if registryformat = trfSysdiff then
          ActionResult := reportError(Sektion, linecounter,
            Sektion.strings[linecounter - 1], '"' + Remaining +
            '": sysdiff format not possible with option "ntuser"')
        else
          ActionResult := doRegistryNTUserDat(ArbeitsSektion,
            registryformat, flag_force64, ntuserpath);
      end
      else if flag_all_usrclass then
      begin
        if registryformat = trfSysdiff then
          ActionResult := reportError(Sektion, linecounter,
            Sektion.strings[linecounter - 1], '"' + Remaining +
            '": sysdiff format not possible with option "for all usr classes"')
        else
          ActionResult := doRegistryAllUsrClassDats(ArbeitsSektion,
            registryformat, flag_force64);
      end
      else
        case registryformat of
          trfWinst:
            ActionResult := doRegistryHack(ArbeitsSektion, reg_specified_basekey,
              flag_force64);

          trfSysdiff:
            ActionResult := doRegistryHackInfSource(ArbeitsSektion,
              reg_specified_basekey, flag_force64);

          trfRegedit:
            ActionResult := doRegistryHackRegeditFormat(ArbeitsSektion,
              reg_specified_basekey, flag_force64);

        end;
    end;
    parseAndCallRegistry := ActionResult;
  end;

{$ELSE WINDOWS}
  function parseAndCallRegistry(ArbeitsSektion: TWorkSection;
    Remaining: string): TSectionResult;
  begin
    logdatei.log('Registry sections are not implemented for Linux / Mac.', LLWarning);
  end;

{$ENDIF WINDOWS}


  function parseAndCallWinbatch(ArbeitsSektion: TWorkSection;
    Remaining: string): TSectionResult;
  begin
    logdatei.log('Execution of: ' + ArbeitsSektion.Name + ' ' + Remaining, LLNotice);
    runAs := traInvoker;
 {$IFDEF WIN32}
    opsiSetupAdmin_runElevated := False;
 {$ENDIF WIN32}
    WaitSecs := 0;
    flag_force64 := False;
    GetWord(Remaining, expr, Remaining, WordDelimiterSet0);
    SyntaxCheck := True;

    ident := '';
    WaitConditions := [ttpWaitOnTerminate];

    while SyntaxCheck and (length(expr) > 0) do
    begin
   {$IFDEF WIN32}
      if LowerCase(expr) = LowerCase(ParameterRunAsAdmin) then
      begin
        runAs := traAdmin;
      end

      else if LowerCase(expr) = LowerCase(ParameterRunAsAdmin1) then
      begin
        runAs := traAdmin;
      end

      else if LowerCase(expr) = LowerCase(ParameterRunAsAdmin2) then
      begin
        runAs := traAdminProfile;
      end

      else if LowerCase(expr) = LowerCase(ParameterRunAsAdmin3) then
      begin
        runAs := traAdminProfileImpersonate;
      end

      else if LowerCase(expr) = LowerCase(ParameterRunAsAdmin4) then
      begin
        runAs := traAdminProfileImpersonateExplorer;
      end

      else if LowerCase(expr) = LowerCase(ParameterRunAsInvoker) then
      begin
        runAs := traInvoker;
      end

      else if LowerCase(expr) = LowerCase(ParameterRunElevated) then
      begin
        opsiSetupAdmin_runElevated := True;
      end

      else if LowerCase(expr) = LowerCase(ParameterRunAsLoggedOnUser) then
      begin
        if runLoginScripts then
          runAs := traLoggedOnUser
        else
          LogDatei.log('Warning: Not in UserLoginScript mode: /RunAsLoggedinUser ignored',
            LLWarning);
      end

      else if (LowerCase(expr) = LowerCase(Parameter_64Bit)) and Is64BitSystem then
      begin
        flag_force64 := True;
      end

      else if LowerCase(expr) = LowerCase(Parameter_32Bit) then
      begin
        flag_force64 := False;
      end

      else if (LowerCase(expr) = LowerCase(Parameter_SysNative)) and Is64BitSystem then
      begin
        flag_force64 := True;
      end

      else if (LowerCase(expr) = LowerCase(Parameter_SysNative)) and
        (not Is64BitSystem) then
      begin
        flag_force64 := False;
      end

      else
{$ENDIF WIN32}
      if LowerCase(expr) = LowerCase(ParameterWaitSecs) then
      begin
        WaitConditions := WaitConditions + [ttpWaitTime];
        // WaitConditions := WaitConditions - [ttpWaitOnTerminate];

        GetWord(Remaining, expr, Remaining, WordDelimiterSet0);
        try
          WaitSecs := StrToInt64(expr);
        except
          on EConvertError do
          begin
            InfoSyntaxError := 'Integer number expected';
            SyntaxCheck := False;
          end
        end;
      end

   {$IFDEF WIN32}
      else if LowerCase(expr) = LowerCase(ParameterCloseOnWindow) then
      begin
        runAs := traInvoker;
        WaitConditions := WaitConditions + [ttpWaitForWindowAppearing];

        if EvaluateString(Remaining, Remaining, ident, InfoSyntaxError) then
        begin
          if Remaining <> '' then
          begin
            SyntaxCheck := False;
            InfoSyntaxError := 'not expected chars after "';
          end;
        end
        else
          SyntaxCheck := False;
      end


      else if LowerCase(expr) = LowerCase(ParameterCloseBehindWindow) then
      begin
        runAs := traInvoker;
        WaitConditions := WaitConditions + [ttpWaitForWindowVanished];

        if EvaluateString(Remaining, Remaining, ident, InfoSyntaxError) then
        begin
          if Remaining <> '' then
          begin
            SyntaxCheck := False;
            InfoSyntaxError := 'unexpected characters after "';
          end;
        end
        else
          SyntaxCheck := False;
      end

   {$ENDIF WIN32}

      else if LowerCase(expr) = LowerCase(ParameterWaitProcessTimeoutSecs) then
      begin
        WaitConditions := WaitConditions - [ttpWaitTime];
        WaitConditions := WaitConditions + [ttpWaitTimeout];

        GetWord(Remaining, expr, Remaining, WordDelimiterSet0);
        try
          WaitSecs := StrToInt64(expr);
        except
          on EConvertError do
          begin
            try
              EvaluateString(expr, expr, seconds, InfoSyntaxError);
              WaitSecs := StrToInt64(seconds);
            except
              on EConvertError do
              begin
                InfoSyntaxError := 'Integer number expected ' + InfoSyntaxError;
                SyntaxCheck := False;
              end;
            end;
          end
        end;
      end

      else if LowerCase(expr) = LowerCase(ParameterWaitForProcessEnding) then
      begin
        WaitConditions := WaitConditions + [ttpWaitForProcessEnding];
        if not EvaluateString(Remaining, Remaining, ident, InfoSyntaxError) then
          SyntaxCheck := False;
      end

      else if UpperCase(expr) = UpperCase(ParameterDontWait) then
      begin
        WaitConditions := WaitConditions - [ttpWaitOnTerminate];
        WaitConditions := WaitConditions - [ttpWaitTimeout];
      end

      else if UpperCase(expr) = UpperCase(ParameterWaitOnTerminate) then
      begin
        WaitConditions := WaitConditions + [ttpWaitOnTerminate];
      end



      else
      begin
        SyntaxCheck := False;
        InfoSyntaxError := expr + ' not legal WinBatch parameter';
      end;

      GetWord(Remaining, expr, Remaining, WordDelimiterSet0);
    end;

    if SyntaxCheck then
      parseAndCallWinbatch := execWinBatch(ArbeitsSektion, Remaining,
        WaitConditions, Ident, WaitSecs, runAs, flag_force64)
    else
      parseAndCallWinbatch := reportError(Sektion, linecounter,
        'Expressionstr', InfoSyntaxError);
  end;

begin
  logdatei.log_prog('Starting doAktionen: ', LLDebug2);
  Script.FLastSection := Script.FActiveSection;
  Script.ActiveSection := sektion;
  Result := tsrPositive;
  ActionResult := tsrPositive;

  NestLevel := Sektion.NestingLevel;
  ActLevel := NestLevel;
  conditions[ActLevel] := True;

  StartIndentLevel := LogDatei.LogSIndentLevel;
  //LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

  //FBatchOberflaeche.setPicture (3, '', '');

  ArbeitsSektion := TWorkSection.Create(NestLevel, Sektion);
  //ApplyTextConstants (TXStringList (ArbeitsSektion), false);
  //ApplyTextConstants (TXStringList (Sektion), false);
  output := TXStringList.Create;
  {$IFDEF GUI}
  FBatchOberflaeche.setWindowState(batchWindowMode);
  {$ENDIF GUI}

  linecounter := 1;
  continue := True;

  looplist := TXStringList.Create;
  inloop := False;
  logdatei.log_prog('Working doAktionen: Vars initialized.', LLDebug2);
  while (linecounter <= Sektion.Count) and continue and
    (actionresult > tsrFatalError) and (FExtremeErrorLevel > levelfatal) and
    not scriptstopped do
  begin
    //writeln(actionresult);
    Remaining := trim(Sektion.strings[linecounter - 1]);
    // Replace constants on every line in primary section:
    ApplyTextConstantsToString(Remaining, False);
    logdatei.log_prog('Working doAktionen: Remaining:' + remaining, LLDebug2);
    //logdatei.log_prog('Actlevel: '+IntToStr(Actlevel)+' NestLevel: '+IntToStr(NestLevel)+' Sektion.NestingLevel: '+IntToStr(Sektion.NestingLevel)+' condition: '+BoolToStr(conditions [ActLevel],true),LLDebug3);
    if (inDefFuncLevel = 0)          // count only lines on base level
      and (lowercase(Sektion.Name) = 'actions')   // count only lines in actions
    then
      Inc(FAktScriptLineNumber);
    logdatei.log_prog('Script line: ' + IntToStr(linecounter) + ' / ' +
      IntToStr(FAktScriptLineNumber) + ' : ' + Remaining, LLDebug);
    //writeln(remaining);
    //readln;

    repeat // looping if inLoop, otherwise going through the statements just once

      if inLoop then
      begin
        Remaining := stayOnStatement; //looping on this statement
        loopvalue := looplist[0];
        Remaining := StringReplace(Remaining, loopvar, loopvalue);
        looplist.Delete(0);
      end;

      // if remaining starts with DefFunc
      if pos(lowercase(PStatNames^ [tsDefineFunction]), lowercase(
        trim(Remaining))) = 1 then
        Inc(inDefFunc3);
      // if remaining starts with EndFunc
      if pos(lowercase(PStatNames^ [tsEndFunction]), lowercase(trim(Remaining))) = 1 then
        Dec(inDefFunc3);
      //if (lowercase(Remaining) = lowercase(PStatNames^ [tsEndFunction])) then dec(inDefFunc2);
      logdatei.log_prog('Parsingprogress: inDefFunc: ' + IntToStr(inDefFunc), LLDebug3);
      logdatei.log_prog('Parsingprogress: inDefFuncIndex: ' + IntToStr(
        inDefFuncIndex), LLDebug3);
      logdatei.log_prog('Parsingprogress: inDefFunc3: ' + IntToStr(inDefFunc3), LLDebug);

      if (Remaining = '') or (Remaining[1] = LineIsCommentChar) then
      // continue
      else if (Remaining[1] = '[') then
      begin
        // subsection beginning
        logdatei.log_prog('line is starting with "[": inDefFunc3: ' +
          IntToStr(inDefFunc3), LLInfo);

        //else if (Remaining [1] = '[')  then
        // subsection beginning
        //if (inDefFunc3 = 0) and (inDefFuncIndex = -1) then
        //if (inDefFunc3 = 0) or processline then
        if (inDefFunc3 = 0) or processline then
        begin
          // (inDefFunc3 = 0) : we are not between deffunc and enfunc
          // ((inDefFuncIndex = -1)) : we are on base level (no local functions active)
          continue := False;
          LogDatei.log('Section ending since next line is starting with "["', LLInfo);
        end;
      end
      else
      begin
        call := remaining;
        Expressionstr := '';
        logdatei.log('Parsingprogress: r: ' + Remaining + ' exp: ' +
          Expressionstr, LLDebug3);
        GetWord(Remaining, Expressionstr, Remaining, WordDelimiterSet4);
        logdatei.log('Parsingprogress: r: ' + Remaining + ' exp: ' +
          Expressionstr, LLDebug3);
        StatKind := FindKindOfStatement(Expressionstr, SectionSpecifier, call);
        ArbeitsSektion.Name := Expressionstr;
        ArbeitsSektion.SectionKind := StatKind;
        ArbeitsSektion.NestingLevel := Nestlevel;
        logdatei.log_prog('Actlevel: ' + IntToStr(Actlevel) +
          ' NestLevel: ' + IntToStr(NestLevel) + ' ArbeitsSektion.NestingLevel: ' +
          IntToStr(ArbeitsSektion.NestingLevel) + ' condition: ' +
          BoolToStr(conditions[ActLevel], True), LLDebug2);
        logdatei.log_prog('StatKind: ' + TypInfo.GetEnumName(
          TypeInfo(TStatement), integer(StatKind)), LLDebug2);


        // start switch statement
        // syntax:
        // switch <string expr ; normally variable>
        // case <string const>
        // endcase
        // defaultcase
        // endswitch
        if StatKind = tsSwitch then
        begin
          LogDatei.log_prog('Entering Switch statement', LLDebug2);
          if inswitch then
          begin
            reportError(Sektion, linecounter, '',
              'Nested Switch Statement is not allowed.');
            exit;
          end
          else
          begin
            Expressionstr := Remaining;
            if EvaluateString(Expressionstr, Remaining, switchCondition,
              InfoSyntaxError) then
            begin
              InSwitch := True;
              InCase := False;
              ValidCase := False;
              SwitchResolved := False;
              doLogEntries(PStatNames^ [tsSwitch] + ' : ' + switchCondition, LLinfo);
              LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
            end
            else
              reportError(Sektion, linecounter, Expressionstr, InfoSyntaxError);
          end;
        end

        else if StatKind = tsSwitchCaseOpen then
        begin
          LogDatei.log_prog('Entering Case statement', LLDebug2);
          if not inswitch then
          begin
            reportError(Sektion, linecounter, '',
              'Case Statement only allowed inside switch statement.');
            exit;
          end
          else
          if InCase then
          begin
            reportError(Sektion, linecounter, '',
              'Nested Case Statement is not allowed.');
            exit;
          end
          else
          begin
            Expressionstr := Remaining;
            if EvaluateString(Expressionstr, Remaining,
              switchExpressionstr, InfoSyntaxError) then
            begin
              doLogEntries(PStatNames^ [tsSwitchCaseOpen] + ' : ' +
                switchExpressionstr, LLinfo);
              InSwitch := True;
              InCase := True;
              if LowerCase(switchExpressionstr) = LowerCase(switchCondition) then
              begin
                ValidCase := True;
                SwitchResolved := True;
                LogDatei.log('Case match: ' + switchExpressionstr +
                  ' = ' + switchCondition, LLInfo);
                LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
              end
              else
              begin
                ValidCase := False;
                LogDatei.log('Case mismatch: ' + switchExpressionstr +
                  ' != ' + switchCondition, LLDebug2);
              end;
            end
            else
              reportError(Sektion, linecounter, Expressionstr, InfoSyntaxError);
          end;
        end

        else if StatKind = tsSwitchCaseClose then
        begin
          LogDatei.log_prog('Entering EndCase statement', LLDebug2);
          if not inswitch then
          begin
            reportError(Sektion, linecounter, '',
              'EndCase Statement only allowed inside switch statement.');
            exit;
          end
          else
          if not InCase then
          begin
            reportError(Sektion, linecounter, '',
              'There is no open Case statement to close.');
            exit;
          end
          else
          begin
            doLogEntries(PStatNames^ [tsSwitchCaseClose], LLinfo);
            if ValidCase then
              LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
            InSwitch := True;
            InCase := False;
            ValidCase := False;
          end;
        end

        else if StatKind = tsSwitchDefaultOpen then
        begin
          LogDatei.log_prog('Entering DefaultCase statement', LLDebug2);
          if not inswitch then
          begin
            reportError(Sektion, linecounter, '',
              'DefaultCase Statement only allowed inside switch statement.');
            exit;
          end
          else
          if InCase then
          begin
            reportError(Sektion, linecounter, '',
              'Nested Case Statement is not allowed.');
            exit;
          end
          else
          begin
            InSwitch := True;
            InCase := True;
            if not SwitchResolved then
            begin
              doLogEntries(PStatNames^ [tsSwitchDefaultOpen], LLinfo);
              ValidCase := True;
              SwitchResolved := True;
              LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
            end;
          end;
        end

        else if (StatKind = tsSwitchClose) then
        begin
          LogDatei.log_prog('Entering EndSwitch statement', LLDebug2);
          if not inswitch then
          begin
            reportError(Sektion, linecounter, '',
              'EndSwitch Statement only allowed inside switch statement.');
            exit;
          end
          else
          if InCase then
          begin
            reportError(Sektion, linecounter, '',
              'There is a open Case which have to be closed by EndCase before the use of EndSwitch');
            exit;
          end
          else
          begin
            doLogEntries(PStatNames^ [tsSwitchClose], LLinfo);
            InSwitch := False;
            InCase := False;
            ValidCase := False;
            LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
          end;
        end

        // treat statements for if-else first

        else if (StatKind = tsCondOpen) and (not (InSwitch) or ValidCase) then
        begin
          //if FExtremeErrorLevel > levelfatal then
          begin
            Inc(NestLevel);
            Ifelseendiflevel := Nestlevel;
            ThenBranch[NestLevel] := True;
            logdatei.log_prog('IF: Actlevel: ' + IntToStr(Actlevel) +
              ' NestLevel: ' + IntToStr(NestLevel) + ' sektion.NestingLevel: ' +
              IntToStr(sektion.NestingLevel) + ' ThenBranch: ' +
              BoolToStr(ThenBranch[NestLevel], True), LLDebug);
            doLogEntries(PStatNames^ [tsCondOpen], LLinfo);
            if NestLevel > High(TConditions) then
            begin
              reportError(Sektion, linecounter, '', 'Too many nested conditions');
              exit;
            end;

            LogDatei.LogSIndentLevel := NestLevel;

            if (NestLevel = ActLevel + 1) and Conditions[ActLevel] then
            begin
              // eine neue aktive Ebene wird erzeugt, falls
              // die if-Anweisung auf einer aktiven Ebene UND im positiven Zweig steht

              Expressionstr := Remaining;
              if EvaluateBoolean(Expressionstr, Remaining, BooleanResult,
                NestLevel, InfoSyntaxError) then
              begin
                Inc(ActLevel);
                Conditions[NestLevel] := BooleanResult;
              end
              else
                reportError(Sektion, linecounter, Expressionstr, InfoSyntaxError);
              if Remaining <> '' then
                reportError(Sektion, linecounter, Remaining, 'erroneous characters ');
            end;

            LogDatei.LogSIndentLevel := NestLevel - 1;
            doLogEntries(PStatNames^ [tsCondThen], LLInfo);
            LogDatei.LogSIndentLevel := NestLevel;
          end;
          //ArbeitsSektion.NestingLevel:=Nestlevel;
          //Sektion.NestingLevel:=Nestlevel;
        end

        else if (StatKind = tsCondElse) and (not (InSwitch) or ValidCase) then
        begin
          //if FExtremeErrorLevel > levelfatal then
          begin
            logdatei.log_prog('ELSE: Actlevel: ' + IntToStr(Actlevel) +
              ' NestLevel: ' + IntToStr(NestLevel) + ' sektion.NestingLevel: ' +
              IntToStr(sektion.NestingLevel) + ' ThenBranch: ' +
              BoolToStr(ThenBranch[NestLevel], True), LLDebug);
            if NestLevel <= Sektion.NestingLevel then
              reportError(Sektion, linecounter, '', PStatNames^
                [tsCondElse] + '  without  ' + PStatNames^ [tsCondOpen])
            else
            begin
              if not ThenBranch[NestLevel] then
              begin
                logdatei.log_prog('ELSE: Actlevel: ' + IntToStr(Actlevel) +
                  ' NestLevel: ' + IntToStr(NestLevel) +
                  ' sektion.NestingLevel: ' + IntToStr(sektion.NestingLevel) +
                  ' ThenBranch: ' + BoolToStr(ThenBranch[NestLevel], True), LLWarning);
                reportError(Sektion, linecounter, '', 'double ' +
                  PStatNames^ [tsCondElse]);
              end
              else
              begin
                ThenBranch[NestLevel] := False;

                // innerhalb einer aktiven Ebene Wechsel zwischen positivem/negativen Zweig
                LogDatei.LogSIndentLevel := NestLevel - 1;
                doLogEntries(PStatNames^ [tsCondElse], LLinfo);
                LogDatei.LogSIndentLevel := NestLevel;

                if NestLevel = ActLevel then
                  Conditions[ActLevel] := not Conditions[ActLevel];
              end;
            end;
          end;
        end

        else if (StatKind = tsCondClose) and (not (InSwitch) or ValidCase) then
        begin
          // if FExtremeErrorLevel > levelfatal then
          begin
            //Herabstufen einer formalen Ebene, innerhalb einer aktiven Ebene Beenden von dieser
            // End active level and go down one level

            if NestLevel = ActLevel then
              Dec(ActLevel);
            Dec(NestLevel);
            Ifelseendiflevel := Nestlevel;
            try
              logdatei.log_prog('ENDIF: Actlevel: ' + IntToStr(Actlevel) +
                ' NestLevel: ' + IntToStr(NestLevel) + ' sektion.NestingLevel: ' +
                IntToStr(sektion.NestingLevel) + ' ThenBranch: ' +
                BoolToStr(ThenBranch[NestLevel], True), LLDebug);
            except
              logdatei.log_prog('ENDIF: Actlevel: ' + IntToStr(Actlevel) +
                ' NestLevel: ' + IntToStr(NestLevel) + ' sektion.NestingLevel: ' +
                IntToStr(sektion.NestingLevel) + ' ThenBranch: unknown', LLDebug);
            end;
            //ArbeitsSektion.NestingLevel:=Nestlevel;
            //Sektion.NestingLevel:=Nestlevel;

            LogDatei.LogSIndentLevel := NestLevel;
            doLogEntries(PStatNames^ [tsCondClose], LLinfo);


            if NestLevel < Sektion.NestingLevel then
              reportError(Sektion, linecounter, '', PStatNames^
                [tsCondClose] + '  without  ' + PStatNames^ [tsCondOpen]);
          end;
        end


        else
        // Ausfuehrung alles folgenden nur falls in einem aktuellen true-Zweig
        // further line execution only if:
        // we are in a valid if or else branch
        if ((NestLevel = ActLevel) and conditions[ActLevel])
          // and if we are inside a Switch and a valid Case
          // in other words: and ( not(InSwitch) or ValidCase)
          and (not (InSwitch) or ValidCase)
          // sowie falls weitere Bearbeitung gewuenscht
          // and line processing not stoped now
          and (ActionResult > 0) then
        begin
          processline := True;
          case SectionSpecifier of
            tsecIncluded:
            begin

              ArbeitsSektion.Clear;
              SearchForSectionLines(self, Sektion, CallingSektion, Expressionstr,
                TXStringList(ArbeitsSektion), StartlineOfSection, True, True, False);


              if inLoop then
                ArbeitsSektion.GlobalReplace(1, loopvar, loopvalue, False);

              if ArbeitsSektion.Count = 0 then
              begin
                LogDatei.log('Warning: The section "' + Expressionstr +
                  '" (called in line ' + IntToStr(linecounter) +
                  ' of the calling section) does not exist or is empty', LLWarning);
                FNumberOfWarnings := FNumberOfWarnings + 1;
              end

              else
                ArbeitsSektion.StartLineNo := StartlineOfSection;
            end;




            tsecExpected:
            begin
              ArbeitsSektion.Clear;

              inputlist := TXStringList.Create;
              inputname := Remaining;

              if produceStringList(sektion, Remaining, Remaining,
                inputlist, InfoSyntaxError) then
                // do we get the section as a string list? then ready.
              begin
                Arbeitssektion.Assign(inputlist);
                if Arbeitssektion.Count > 0 then
                begin
                  ArbeitsSektion.Name := ArbeitsSektion.Name + ' {' + inputname + '} ';
                  ArbeitsSektion.StartLineNo := 1;
                end;
              end

              // Otherwise, we should have a file name and try to get our command lines from it
              else if EvaluateString(Remaining, Remaining, FName, InfoSyntaxError)
              then
              begin
                fullfilename := '';
                ArbeitsSektion.Name := ArbeitsSektion.Name + ' "' + FName + '"';
                Expressionstr := Expressionstr + ' ["' + FName + '"]';
                found := False;
                // full file path given
                testincfilename := ExpandFilename(FName);
                LogDatei.log('Looking for: ' + testincfilename, LLDebug2);
                if FileExistsUTF8(testincfilename) then
                begin
                  found := True;
                  fullfilename := testincfilename;
                end;
                if (not found) then
                begin
                  // search in %ScriptPath%
                  testincfilename := ExtractFileDir(FFilename) + PathDelim + FName;
                  LogDatei.log('Looking for: ' + testincfilename, LLDebug2);
                  if FileExistsUTF8(testincfilename) then
                  begin
                    found := True;
                    fullfilename := testincfilename;
                  end;
                end;
                {$IFDEF WINDOWS}
                if (not found) then
                begin
                  // search in %opsiScriptHelperPath%\lib
                  testincfilename :=
                    getSpecialFolder(CSIDL_PROGRAM_FILES) +
                    '\opsi.org\opsiScriptHelper\lib' + PathDelim + FName;
                  if FileExistsUTF8(testincfilename) then
                  begin
                    found := True;
                    fullfilename := testincfilename;
                  end;
                end;
                {$ENDIF WINDOWS}
                if (not found) then
                begin
                  // search in %ScriptDrive%\lib  aka %ScriptPath%/../lib
                  testincfilename :=
                    ExtractFileDir(FFilename) + PathDelim +
                    '..' + PathDelim + 'lib' + PathDelim + FName;
                  LogDatei.log('Looking for: ' + testincfilename, LLDebug2);
                  if FileExistsUTF8(testincfilename) then
                  begin
                    found := True;
                    fullfilename := testincfilename;
                  end;
                end;
                {$IFDEF WINDOWS}
                if (not found) then
                begin
                  // search in %WinstDir%\lib
                  testincfilename :=
                    ExtractFileDir(reencode(ParamStr(0), 'system')) +
                    PathDelim + 'lib' + PathDelim + FName;
                  LogDatei.log('Looking for: ' + testincfilename, LLDebug2);
                  if FileExistsUTF8(testincfilename) then
                  begin
                    found := True;
                    fullfilename := testincfilename;
                  end;
                end;
                {$ENDIF WINDOWS}
                if fullfilename = '' then
                begin
                  LogDatei.log('Error: No file found for called: ' +
                    FName + ' - Could not load external sub section', LLError);
                end
                else
                begin
                  if CheckFileExists(fullfilename, ErrorInfo) then
                  begin
                    try
                      LoadValidLinesFromFile(fullfilename, ArbeitsSektion);

                      ArbeitsSektion.StartLineNo := 1;
                    except
                      Logdatei.log('File "' + fullfilename +
                        '" cannot be read', LLError);
                    end;
                  end
                  else
                  begin
                    Logdatei.log('Error: File "' + fullfilename +
                      '" not found', LLError);
                  end;
                end;
              end
              else
                reportError(Sektion, linecounter, Expressionstr +
                  '^' + Remaining, InfoSyntaxError);

            end
          end;


          // treating first the special cases of sub sections and loops
          if StatKind in [tsActions, tsProfileActions] then
          begin
            if (ArbeitsSektion.Count > 0) then
            begin
              if StatKind = tsProfileActions then
                runProfileActions := True;
              LogDatei.log('', LLinfo);
              LogDatei.log('~~~~~~~ Start Sub ~~~~~~~  ' + ArbeitsSektion.Name, LLinfo);
              ArbeitsSektion.NestingLevel := NestLevel;
              // ArbeitsSektion.NestingLevel + 1;
              //ApplyTextConstants (TXStringList (ArbeitsSektion), false);

              ActionResult := doAktionen(ArbeitsSektion, Sektion);
              LogDatei.log('', LLinfo);
              LogDatei.log('~~~~~~~ End Sub   ~~~~~~~  ' + ArbeitsSektion.Name, LLinfo);
              LogDatei.log('', LLinfo);
              runProfileActions := False;
            end
            else
              Logdatei.log('Error: Work section is empty', LLError);
          end

          else if (StatKind = tsLoopStringList) or (StatKind = tsLoopForTo)
          // we have to fetch the ArbeitsSektion after checking the loop syntax
          then
          begin
            if inloop then
            begin
              syntaxCheck := False;
              InfoSyntaxError := 'loop in loop not yet implemented';
            end
            else
            begin
              // syntax: for varname in list
              syntaxCheck := False;
              GetWord(Remaining, loopvar, Remaining, WordDelimiterWhiteSpace);
              LogDatei.log_prog('loopvar is: ' + loopvar, LLDebug);
              if not addLoopvarToVarList(loopvar, InfoSyntaxError) then
                LogDatei.log(InfoSyntaxError, LLError)
              else
              begin

                // loop through stringlist
                if Skip('in', Remaining, Remaining, InfoSyntaxError) and
                  produceStringList(sektion, Remaining, Remaining,
                  looplist, InfoSyntaxError) and Skip('do', Remaining,
                  Remaining, InfoSyntaxError) then
                begin
                  if looplist.Count = 0 then
                  begin
                    LogDatei.log('Warning: list to loop through is empty - no loop ...',
                      LLWarning);
                    // clearing the loop variable from the list of variables, first the value
                    if not delLoopvarFromVarList(loopvar, InfoSyntaxError) then
                      syntaxCheck := False
                    else
                      syntaxCheck := True;
                  end
                  else
                  begin
                    syntaxCheck := True;
                    inloop := True;
                    //GetWord (Remaining, Expressionstr, Remaining, WordDelimiterSet1);
                    LogDatei.log('', LLinfo);
                    s1 := '';
                    for loopindex := 0 to looplist.Count - 2 do
                    begin
                      s1 := s1 + '''' + looplist[loopindex] + ''', ';
                    end;
                    if looplist.Count > 0 then
                      s1 := s1 + '''' + looplist[looplist.Count - 1] + '''';

                    LogDatei.log('~~~~~~ Looping through:  ' + s1, LLinfo);
                    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

                    stayOnStatement := remaining;
                  end;
                end
                else
                  // loop through loopstart to loopstop
                begin
                  if Skip('=', Remaining, Remaining, InfoSyntaxError) then
                  begin
                    syntaxCheck := True;
                    if not evaluatestring(Remaining, Remaining,
                      Expressionstr, InfoSyntaxError) then
                      syntaxCheck := False
                    else
                    begin
                      try
                        loopstart := StrToInt(Expressionstr);
                      except
                        syntaxCheck := False;
                        InfoSyntaxError := Expressionstr + 'is no valid integer';
                      end;
                    end;
                  end;
                  if syntaxCheck then
                  begin
                    if Skip('to', Remaining, Remaining, InfoSyntaxError) then
                    begin
                      if not evaluatestring(Remaining,
                        Remaining, Expressionstr, InfoSyntaxError) then
                        syntaxCheck := False
                      else
                        try
                          loopstop := StrToInt(Expressionstr);
                        except
                          syntaxCheck := False;
                          InfoSyntaxError := Expressionstr + 'is no valid integer';
                        end;
                    end;
                  end;
                  if syntaxCheck then
                  begin
                    if not Skip('do', Remaining, Remaining, InfoSyntaxError) then
                    begin
                      syntaxCheck := False;
                      InfoSyntaxError := '"do" expected';
                    end;
                  end;
                  if syntaxCheck then
                  begin
                    if loopstart > loopstop then
                    begin
                      LogDatei.log(
                        'Warning: list to loop through is empty - no loop ...',
                        LLWarning);
                      // clearing the loop variable from the list of variables, first the value
                      if not delLoopvarFromVarList(loopvar, InfoSyntaxError) then
                        syntaxCheck := False
                      else
                        syntaxCheck := True;
                    end
                    else
                    begin
                      syntaxCheck := True;
                      inloop := True;
                      looplist.Clear;
                      for loopindex := loopstart to loopstop do
                        looplist.Append(IntToStr(loopindex));
                      LogDatei.log('', LLinfo);
                      s1 := '';
                      for loopindex := 0 to looplist.Count - 2 do
                      begin
                        s1 := s1 + '''' + looplist[loopindex] + ''', ';
                      end;
                      if looplist.Count > 0 then
                        s1 := s1 + '''' + looplist[looplist.Count - 1] + '''';
                      LogDatei.log('~~~~~~ Looping through:  ' + s1, LLinfo);
                      LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
                      stayOnStatement := remaining;
                    end;
                  end;
                end; // loop
              end; // loopvar valid
            end;  // get loop var
            if not syntaxCheck then
              reportError(Sektion, linecounter, Expressionstr, InfoSyntaxError);
          end  //tsLoopStringList

          else
          begin
            if StatKind in [tsFileActions] then
            begin
              // do ApplyTextConstants later while working
            end
            else
            begin
              if StatKind in [tsExecutePython, tsExecuteWith_escapingStrings]
              then
              begin
                ApplyTextConstants(TXStringList(ArbeitsSektion), True);
                ApplyTextVariables(TXStringList(ArbeitsSektion), True);
              end
              else
              begin
                ApplyTextConstants(TXStringList(ArbeitsSektion), False);
                ApplyTextVariables(TXStringList(ArbeitsSektion), False);
              end;
            end;


            if inloop then
              ArbeitsSektion.GlobalReplace(1, loopvar, loopvalue, False);

            case StatKind of
              tsMessage:
                if skip('=', remaining, remaining, InfoSyntaxError) then
                  ActionResult := doInfo(Remaining)
                else if EvaluateString(Remaining, Remaining, Parameter, InfoSyntaxError)
                then
                begin
                  ActionResult := doInfo(Parameter);
                  LogDatei.log('message ' + Parameter, LLnotice);
                end
                else
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    InfoSyntaxError);

              tsMessageFile:
                if EvaluateString(Remaining, Remaining, FName, InfoSyntaxError)
                then
                begin
                  {$IFDEF GUI}
                  CreateSystemInfo;
                  SystemInfo.Memo1.Lines.Clear;
                  try
                    Fname := ExpandFileName(Fname);
                    SystemInfo.Memo1.Lines.LoadFromFile(FName);
                    //SystemInfo.Memo1.Lines.Text := reencode(SystemInfo.Memo1.Lines.Text, 'system');
                    Logdatei.log('', LevelComplete);
                    Logdatei.log('ShowMessagefile "' + Fname + '"', LevelComplete);
                    SystemInfo.Showmodal;
                  except
                    on E: Exception do
                    begin
                      Logdatei.log('ShowMessagefile "' + Fname + '"', LLwarning);
                      Logdatei.log(
                        '  File does not exist or cannot be accessed, system message: "'
                        + E.Message + '"',
                        LLwarning);
                    end
                  end;

                  SystemInfo.Free;
                  SystemInfo := nil;
                  FBatchOberflaeche.BringToFront;
                  {$ENDIF GUI}

                end
                else
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    InfoSyntaxError);

              tsDefinedVoidFunction:
              begin
                // defined local function ?
                GetWord(call, p1, p2, WordDelimiterSet5);
                FuncIndex := definedFunctionNames.IndexOf(LowerCase(p1));
                if FuncIndex >= 0 then
                begin
                  if not (definedFunctionArray[FuncIndex].datatype = dfpVoid) then
                  begin
                    // error
                    syntaxCheck := False;
                    LogDatei.log('Syntax Error: defined function: ' +
                      p1 + ' is not from type string.', LLError);
                  end
                  else
                  begin
                    if definedFunctionArray[FuncIndex].call(p2, p2, NestLevel) then
                    begin
                      syntaxCheck := True;
                      //logdatei.log('We leave the defined function: inDefFunc3: '+IntToStr(inDefFunc3),LLInfo);
                    end
                    else
                    begin
                      // defined function call failed
                      LogDatei.log('Call of defined function: ' +
                        p1 + ' failed', LLError);
                      syntaxCheck := False;
                    end;
                  end;
                end;
              end;

              tsImportLib:
              begin
                syntaxCheck :=
                  EvaluateString(Remaining, Remaining, FName, InfoSyntaxError);
                if syntaxCheck then
                begin
                  try
                    importFunctionName := '';
                    fullincfilename := '';
                    LogDatei.log_prog('Found ImportLib statement for: ' +
                      FName, LLInfo);
                    if pos('::', FName) > 0 then
                    begin
                      incfilename := copy(FName, 0, pos('::', FName) - 1);
                      //incfilename:= ExpandFileName(incfilename);
                      importFunctionName :=
                        copy(FName, pos('::', FName) + 2, length(FName));
                    end
                    else
                      incfilename := FName;
                    if ExtractFileExt(incfilename) = '' then
                      incfilename := incfilename + '.opsiscript';
                    LogDatei.log('Found ImportLib statement for file: ' +
                      incfilename + ' and function: ' + importFunctionName, LLDebug);
                    found := False;
                    // full file path given ?
                    testincfilename := ExpandFilename(incfilename);
                    LogDatei.log_prog('Looking for: ' + testincfilename, LLNotice);
                    if FileExistsUTF8(testincfilename) then
                    begin
                      found := True;
                      fullincfilename := testincfilename;
                    end;
                    if (not found) then
                    begin
                      // search in %ScriptPath%
                      testincfilename :=
                        ExtractFileDir(FFilename) + PathDelim + incfilename;
                      testincfilename := ExpandFilename(testincfilename);
                      LogDatei.log_prog('Looking for: ' + testincfilename, LLNotice);
                      if FileExistsUTF8(testincfilename) then
                      begin
                        found := True;
                        fullincfilename := testincfilename;
                      end;
                    end;
                        {$IFDEF WINDOWS}
                    if (not found) then
                    begin
                      // search in %opsiScriptHelperPath%\lib
                      testincfilename :=
                        getSpecialFolder(CSIDL_PROGRAM_FILES) +
                        '\opsi.org\opsiScriptHelper\lib' + PathDelim + incfilename;
                      testincfilename := ExpandFilename(testincfilename);
                      LogDatei.log_prog('Looking for: ' + testincfilename, LLNotice);
                      if FileExistsUTF8(testincfilename) then
                      begin
                        found := True;
                        fullincfilename := testincfilename;
                      end;
                    end;
                        {$ENDIF WINDOWS}
                    if (not found) then
                    begin
                      // search in %ScriptDrive%\lib  aka %ScriptPath%/../lib
                      testincfilename :=
                        ExtractFileDir(FFilename) + PathDelim +
                        '..' + PathDelim + 'lib' + PathDelim + incfilename;
                      testincfilename := ExpandFilename(testincfilename);
                      LogDatei.log_prog('Looking for: ' + testincfilename, LLNotice);
                      if FileExistsUTF8(testincfilename) then
                      begin
                        found := True;
                        fullincfilename := testincfilename;
                      end;
                    end;
                    if (not found) then
                    begin
                          {$IFDEF WINDOWS}
                      // search in %OpsiscriptDir%\lib
                      testincfilename :=
                        ExtractFileDir(reencode(ParamStr(0),
                        'system')) + PathDelim + 'lib' + PathDelim + incfilename;
                          {$ENDIF WINDOWS}
                          {$IFDEF LINUX}
                      // search in /usr/share/opsi-script/lib
                      testincfilename :=
                        '/usr/share/opsi-script' + PathDelim +
                        'lib' + PathDelim + incfilename;
                          {$ENDIF LINUX}
                          {$IFDEF DARWIN}
                      // search in %OpsiscriptDir%\lib
                      testincfilename :=
                        ExtractFileDir(reencode(ParamStr(0),
                        'system')) + PathDelim + '../Resources/lib' +
                        PathDelim + incfilename;
                      testincfilename := ExpandFilename(testincfilename);
                      LogDatei.log_prog('Looking for: ' + testincfilename, LLNotice);
                      if FileExistsUTF8(testincfilename) then
                      begin
                        found := True;
                        fullincfilename := testincfilename;
                      end;
                      if (not found) then
                      begin
                        // search in /usr/local/share/opsi-script/lib
                        testincfilename :=
                          '/usr/local/share/opsi-script' +
                          PathDelim + 'lib' + PathDelim + incfilename;
                      end;
                          {$ENDIF DARWIN}
                    end;
                    if (not found) then
                    begin
                      testincfilename := ExpandFilename(testincfilename);
                      LogDatei.log_prog('Looking for: ' + testincfilename, LLNotice);
                      if FileExistsUTF8(testincfilename) then
                      begin
                        found := True;
                        fullincfilename := testincfilename;
                      end;
                    end;
                    if found then
                    begin
                      inSearchedFunc := False;
                      LogDatei.log('Found File: ' + fullincfilename, LLDebug2);
                      inclist := TStringList.Create;
                      inclist.LoadFromFile(ExpandFileName(fullincfilename));
                      Encoding2use := searchencoding(inclist.Text);
                      //Encoding2use := inclist.Values['encoding'];
                      inclist.Free;
                      if Encoding2use = '' then
                        Encoding2use := 'system';
                      LogDatei.log_prog('Will Include : ' +
                        incfilename + ' with encoding: ' + Encoding2use, LLDebug);
                      assignfile(incfile, fullincfilename);
                      reset(incfile);
                      //script.Strings[i] := '';
                      alllines := 0;
                      inclines := 0;
                      inDefFunc2 := 0;
                      while not EOF(incfile) do
                      begin
                        Inc(alllines);
                        readln(incfile, incline);
                        LogDatei.log_prog(
                          'Found line in lib file (raw): ' + incline, LLDebug3);
                        incline := reencode(incline, Encoding2use, usedEncoding);
                        LogDatei.log_prog(
                          'Found line in lib file (reencoded): ' + incline, LLDebug2);
                        for constcounter := 1 to ConstList.Count do
                          if Sektion.replaceInLine(incline,
                            Constlist.Strings[constcounter - 1],
                            ConstValuesList.Strings[constcounter - 1],
                            False, replacedline) then
                            incline := replacedline;


                        if importFunctionName <> '' then
                        begin
                          // we import only one function
                          GetWord(incline, Expressionstr, Remaining,
                            WordDelimiterSet4);
                          // does the line starts with deffunc ?
                          if 'deffunc' = trim(lowercase(Expressionstr)) then
                          begin
                            Inc(inDefFunc2);
                            // is it the searched function name ? ?
                            if (not inSearchedFunc) and (inDefFunc2 = 1) then
                            begin
                              GetWord(Remaining, Expressionstr,
                                Remaining, WordDelimiterSet4);
                              if lowercase(Expressionstr) =
                                LowerCase(importFunctionName) then
                              begin
                                inSearchedFunc := True;
                                secindex :=
                                  Script.FSectionNameList.Add(importFunctionName);
                                tmpint := length(script.FSectionInfoArray);
                                if secindex <> tmpint then
                                  LogDatei.log(
                                    'Error: internal: secindex <> length(script.FSectionInfoArray)',
                                    LLCritical);
                                setlength(script.FSectionInfoArray, secindex + 1);
                                script.FSectionInfoArray[secindex].SectionName :=
                                  importFunctionName;
                                script.FSectionInfoArray[secindex].SectionFile :=
                                  ExtractFileName(fullincfilename);
                                script.FSectionInfoArray[secindex].StartLineNo :=
                                  alllines;
                              end;
                            end;
                          end;
                          if inSearchedFunc then
                          begin
                            Inc(inclines);
                            LogDatei.log_prog(
                              'Will Include line : ' + incline, LLDebug);
                            Sektion.Insert(linecounter - 1 + inclines, incline);
                            LogDatei.log_prog(
                              'Line included at pos: ' +
                              IntToStr(linecounter - 1 + inclines) +
                              ' to Sektion with ' + IntToStr(Sektion.Count) +
                              ' lines.', LLDebug2);
                            //LogDatei.log_prog('Will Include add at pos '+inttostr(Sektion.StartLineNo + i-1+k)+'to FLinesOriginList with count: '+inttostr(script.FLinesOriginList.Count),LLDebug2);
                            script.FLinesOriginList.Insert(
                              linecounter - 1 + inclines, incfilename +
                              ' Line: ' + IntToStr(alllines));
                            script.FLibList.Insert(linecounter - 1 + inclines, 'true');
                            LogDatei.log_prog(
                              'Include added to FLinesOriginList.', LLDebug2);
                          end;
                          // do we have an endfunc ?
                          //GetWord (incline, Expressionstr, Remaining, WordDelimiterSet4);
                          if 'endfunc' = trim(lowercase(Expressionstr)) then
                            Dec(inDefFunc2);
                          if inSearchedFunc and (inDefFunc2 = 0) then
                            inSearchedFunc := False;
                        end  // import only one func
                        else
                        begin  // import all func
                          Inc(inclines);
                          LogDatei.log_prog('Will Include line : ' + incline, LLDebug);
                          Sektion.Insert(linecounter - 1 + inclines, incline);
                          LogDatei.log_prog(
                            'Line included at pos: ' +
                            IntToStr(linecounter - 1 + inclines) +
                            ' to Sektion with ' + IntToStr(Sektion.Count) +
                            ' lines.', LLDebug2);
                          //LogDatei.log_prog('Will Include add at pos '+inttostr(Sektion.StartLineNo + i-1+k)+'to FLinesOriginList with count: '+inttostr(script.FLinesOriginList.Count),LLDebug2);
                          script.FLinesOriginList.Insert(
                            linecounter - 1 + inclines, incfilename +
                            ' Line: ' + IntToStr(alllines));
                          script.FLibList.Insert(linecounter - 1 + inclines, 'true');
                          LogDatei.log_prog(
                            'Include added to FLinesOriginList.', LLDebug2);
                        end;
                      end;
                      closeFile(incfile);
                      linecount := Count;

                      for tmpint := 0 to 3 do
                      begin
                        { add a empty line to avoid last line problem }
                        incline := '';
                        Inc(inclines);
                        LogDatei.log_prog(
                          'Will Include empty last line : ' + incline, LLDebug);
                        Sektion.Insert(linecounter - 1 + inclines, incline);
                        LogDatei.log_prog(
                          'Line included at pos: ' +
                          IntToStr(linecounter - 1 + inclines) +
                          ' to Sektion with ' + IntToStr(Sektion.Count) +
                          ' lines.', LLDebug2);
                        //LogDatei.log_prog('Will Include add at pos '+inttostr(Sektion.StartLineNo + i-1+k)+'to FLinesOriginList with count: '+inttostr(script.FLinesOriginList.Count),LLDebug2);
                        script.FLinesOriginList.Insert(
                          linecounter - 1 + inclines, incfilename +
                          ' Line: ' + IntToStr(alllines));
                        script.FLibList.Insert(linecounter - 1 + inclines, 'true');
                        LogDatei.log_prog(
                          'Include added to FLinesOriginList.', LLDebug2);
                        { finished add a empty line to avoid last line problem }
                      end;

                      if importFunctionName = '' then
                      begin
                        LogDatei.log('Imported all functions from file: ' +
                          fullincfilename, LLNotice);
                        tmplist := TXstringlist.Create;
                        tmplist.LoadFromFile(fullincfilename);
                        script.registerSectionOrigins(tmplist, fullincfilename);
                        tmplist.Free;
                      end
                      else
                        LogDatei.log('Imported function : ' +
                          importFunctionName + ' from file: ' +
                          fullincfilename, LLNotice);
                      //for j:= 0 to sektion.Count -1 do
                      //  logdatei.log_prog('script: '+sektion.Strings[j],LLDebug);
                    end
                    else
                    begin
                      LogDatei.log('Error: Could not find import file :' +
                        incfilename, LLCritical);
                      FExtremeErrorLevel := levelFatal;
                    end;
                  except
                    on E: Exception do
                    begin
                      Logdatei.log('importLib "' + Fname + '"', LLCritical);
                      Logdatei.log(
                        ' Failed to import (insert) file, system message: "' +
                        E.Message + '"',
                        LLCritical);
                      FExtremeErrorLevel := levelFatal;
                    end
                  end;
                end
                else
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    InfoSyntaxError);
              end; // tsImportLib



              tsIncludeInsert:
              begin
                syntaxCheck :=
                  EvaluateString(Remaining, Remaining, FName, InfoSyntaxError);
                if syntaxCheck then
                begin
                  try
                    fullincfilename := '';
                    incfilename := FName;
                    LogDatei.log('Found Include_insert statement for: ' +
                      incfilename, LLDebug);
                    found := False;
                    // full file path given
                    testincfilename := ExpandFilename(incfilename);
                    LogDatei.log_prog('Looking for: ' + testincfilename, LLDebug2);
                    if FileExistsUTF8(testincfilename) then
                    begin
                      found := True;
                      fullincfilename := testincfilename;
                    end;
                    if (not found) then
                    begin
                      // search in %ScriptPath%
                      testincfilename :=
                        ExtractFileDir(FFilename) + PathDelim + incfilename;
                      testincfilename := ExpandFilename(testincfilename);
                      LogDatei.log_prog('Looking for: ' + testincfilename, LLDebug2);
                      if FileExistsUTF8(testincfilename) then
                      begin
                        found := True;
                        fullincfilename := testincfilename;
                      end;
                    end;
                        {$IFDEF WINDOWS}
                    if (not found) then
                    begin
                      // search in %opsiScriptHelperPath%\lib
                      testincfilename :=
                        getSpecialFolder(CSIDL_PROGRAM_FILES) +
                        '\opsi.org\opsiScriptHelper\lib' + PathDelim + incfilename;
                      testincfilename := ExpandFilename(testincfilename);
                      LogDatei.log_prog('Looking for: ' + testincfilename, LLDebug2);
                      if FileExistsUTF8(testincfilename) then
                      begin
                        found := True;
                        fullincfilename := testincfilename;
                      end;
                    end;
                        {$ENDIF WINDOWS}
                    if (not found) then
                    begin
                      // search in %ScriptDrive%\lib  aka %ScriptPath%/../lib
                      testincfilename :=
                        ExtractFileDir(FFilename) + PathDelim +
                        '..' + PathDelim + 'lib' + PathDelim + incfilename;
                      testincfilename := ExpandFilename(testincfilename);
                      LogDatei.log_prog('Looking for: ' + testincfilename, LLDebug2);
                      if FileExistsUTF8(testincfilename) then
                      begin
                        found := True;
                        fullincfilename := testincfilename;
                      end;
                    end;
                        {$IFDEF WINDOWS}
                    if (not found) then
                    begin
                      // search in %WinstDir%\lib
                      testincfilename :=
                        ExtractFileDir(reencode(ParamStr(0),
                        'system')) + PathDelim + 'lib' + PathDelim + incfilename;
                      testincfilename := ExpandFilename(testincfilename);
                      LogDatei.log_prog('Looking for: ' + testincfilename, LLDebug2);
                      if FileExistsUTF8(testincfilename) then
                      begin
                        found := True;
                        fullincfilename := testincfilename;
                      end;
                    end;
                        {$ENDIF WINDOWS}
                    if found then
                    begin
                      LogDatei.log('Found File: ' + fullincfilename, LLDebug2);
                      inclist := TStringList.Create;
                      inclist.LoadFromFile(ExpandFileName(fullincfilename));
                      Encoding2use := searchencoding(inclist.Text);
                      //Encoding2use := inclist.Values['encoding'];
                      Script.registerSectionOrigins(inclist, fullincfilename);
                      inclist.Free;
                      if Encoding2use = '' then
                        Encoding2use := 'system';
                      LogDatei.log('Will Include : ' + incfilename +
                        ' with encoding: ' + Encoding2use, LLDebug2);
                      assignfile(incfile, fullincfilename);
                      reset(incfile);
                      //script.Strings[i] := '';
                      k := 0;
                      while not EOF(incfile) do
                      begin
                        Inc(k);
                        readln(incfile, incline);
                        LogDatei.log_prog(
                          'Will Include line (raw): ' + incline, LLDebug3);
                        incline := reencode(incline, Encoding2use, usedEncoding);
                        LogDatei.log_prog(
                          'Will Include line (reencoded): ' + incline, LLDebug3);
                        for constcounter := 1 to ConstList.Count do
                          if Sektion.replaceInLine(incline,
                            Constlist.Strings[constcounter - 1],
                            ConstValuesList.Strings[constcounter - 1],
                            False, replacedline) then
                            incline := replacedline;
                        LogDatei.log_prog(
                          'Will Include line (constants replaced): ' +
                          incline, LLDebug3);
                        Sektion.Insert(linecounter - 1 + k, incline);
                        LogDatei.log_prog(
                          'Line included at pos: ' + IntToStr(linecounter - 1 + k) +
                          ' to Sektion with ' + IntToStr(Sektion.Count) +
                          ' lines.', LLDebug3);
                        //LogDatei.log('Will Include add at pos '+inttostr(Sektion.StartLineNo + i-1+k)+'to FLinesOriginList with count: '+inttostr(script.FLinesOriginList.Count),LLDebug3);
                        //script.FLinesOriginList.Insert(Sektion.StartLineNo + i-1+k,incfilename+ ' Line: '+inttostr(k));
                        //script.FLibList.Insert(Sektion.StartLineNo + i-1+k,'false');
                        script.FLinesOriginList.Insert(
                          linecounter - 1 + k, incfilename + ' Line: ' + IntToStr(k));
                        script.FLibList.Insert(linecounter - 1 + k, 'false');
                        LogDatei.log_prog(
                          'Include added to FLinesOriginList.', LLDebug3);
                      end;
                      closeFile(incfile);
                      linecount := Count;
                      LogDatei.log('Included (insert) file: ' +
                        fullincfilename + ' with encoding: ' + usedEncoding, LLInfo);
                    end
                    else
                    begin
                      LogDatei.log('Error: Could not find include file :' +
                        incfilename, LLCritical);
                      FExtremeErrorLevel := levelFatal;
                    end;
                  except
                    on E: Exception do
                    begin
                      Logdatei.log('Include_Insert "' +
                        Fname + '"', LLCritical);
                      Logdatei.log(
                        ' Failed to include (insert) file, system message: "' +
                        E.Message + '"',
                        LLCritical);
                      FExtremeErrorLevel := levelFatal;
                    end
                  end;
                end
                else
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    InfoSyntaxError);
              end;

              tsIncludeAppend:
              begin
                syntaxCheck :=
                  EvaluateString(Remaining, Remaining, FName, InfoSyntaxError);
                if syntaxCheck then
                begin
                  try
                    incfilename := FName;
                    LogDatei.log('Found Include_append statement for: ' +
                      incfilename, LLDebug);
                    found := False;
                    // full file path given
                    testincfilename := ExpandFilename(incfilename);
                    LogDatei.log('Looking for: ' + testincfilename, LLDebug2);
                    if FileExistsUTF8(testincfilename) then
                    begin
                      found := True;
                      fullincfilename := testincfilename;
                    end;
                    if (not found) then
                    begin
                      // search in %ScriptPath%
                      testincfilename :=
                        ExtractFileDir(FFilename) + PathDelim + incfilename;
                      testincfilename := ExpandFilename(testincfilename);
                      LogDatei.log('Looking for: ' + testincfilename, LLDebug2);
                      if FileExistsUTF8(testincfilename) then
                      begin
                        found := True;
                        fullincfilename := testincfilename;
                      end;
                    end;
                        {$IFDEF WINDOWS}
                    if (not found) then
                    begin
                      // search in %opsiScriptHelperPath%\lib
                      testincfilename :=
                        getSpecialFolder(CSIDL_PROGRAM_FILES) +
                        '\opsi.org\opsiScriptHelper\lib' + PathDelim + incfilename;
                      testincfilename := ExpandFilename(testincfilename);
                      if FileExistsUTF8(testincfilename) then
                      begin
                        found := True;
                        fullincfilename := testincfilename;
                      end;
                    end;
                        {$ENDIF WINDOWS}
                    if (not found) then
                    begin
                      // search in %ScriptDrive%\lib  aka %ScriptPath%/../lib
                      testincfilename :=
                        ExtractFileDir(FFilename) + PathDelim +
                        '..' + PathDelim + 'lib' + PathDelim + incfilename;
                      testincfilename := ExpandFilename(testincfilename);
                      LogDatei.log('Looking for: ' + testincfilename, LLDebug2);
                      if FileExistsUTF8(testincfilename) then
                      begin
                        found := True;
                        fullincfilename := testincfilename;
                      end;
                    end;
                        {$IFDEF WINDOWS}
                    if (not found) then
                    begin
                      // search in %WinstDir%\lib
                      testincfilename :=
                        ExtractFileDir(reencode(ParamStr(0),
                        'system')) + PathDelim + 'lib' + PathDelim + incfilename;
                      testincfilename := ExpandFilename(testincfilename);
                      LogDatei.log('Looking for: ' + testincfilename, LLDebug2);
                      if FileExistsUTF8(testincfilename) then
                      begin
                        found := True;
                        fullincfilename := testincfilename;
                      end;
                    end;
                        {$ENDIF WINDOWS}
                    if found then
                    begin
                      LogDatei.log('Found File: ' + fullincfilename, LLDebug2);
                      inclist := TStringList.Create;
                      inclist.LoadFromFile(ExpandFileName(fullincfilename));
                      Encoding2use := searchencoding(inclist.Text);
                      //Encoding2use := inclist.Values['encoding'];
                      Script.registerSectionOrigins(inclist, fullincfilename);
                      inclist.Free;
                      if Encoding2use = '' then
                        Encoding2use := 'system';
                      linecount := Count;
                      assignfile(incfile, fullincfilename);
                      reset(incfile);
                      //script.Strings[i] := '';
                      k := 0;
                      while not EOF(incfile) do
                      begin
                        Inc(k);
                        readln(incfile, incline);
                        incline := reencode(incline, Encoding2use, usedEncoding);
                        for constcounter := 1 to ConstList.Count do
                          if Sektion.replaceInLine(incline,
                            Constlist.Strings[constcounter - 1],
                            ConstValuesList.Strings[constcounter - 1],
                            False, replacedline) then
                            incline := replacedline;
                        LogDatei.log_prog('Include_append line: ' + incline, LLDebug);
                        append(incline);
                        linecount := Count;
                        script.FLinesOriginList.Append(incfilename +
                          ' Line: ' + IntToStr(k));
                        script.FLibList.Append('false');
                      end;
                      closeFile(incfile);
                      //linecount := Count;
                      LogDatei.log('Included (append) file: ' +
                        fullincfilename + ' with encoding: ' + usedEncoding, LLInfo);
                    end
                    else
                    begin
                      LogDatei.log('Error: Could not find include file :' +
                        incfilename, LLCritical);
                      FExtremeErrorLevel := levelFatal;
                    end;
                  except
                    on E: Exception do
                    begin
                      Logdatei.log('Include_Append "' +
                        Fname + '"', LLCritical);
                      Logdatei.log(
                        ' Failed to include (append) file, system message: "' +
                        E.Message + '"',
                        LLCritical);
                      FExtremeErrorLevel := levelFatal;
                    end
                  end;
                end
                else
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    InfoSyntaxError);
              end;


              tsIncludeLog:
              begin
                logtailEncodingstr := 'system';
                syntaxCheck :=
                  EvaluateString(Remaining, Remaining, FName, InfoSyntaxError);
                if syntaxCheck and (Remaining <> '') then
                  syntaxCheck :=
                    EvaluateString(Remaining, Remaining, logtailLinecountstr,
                    InfoSyntaxError);
                if syntaxCheck and (Remaining <> '') then
                  syntaxCheck :=
                    EvaluateString(Remaining, Remaining, logtailEncodingstr,
                    InfoSyntaxError);
                if syntaxCheck then
                begin
                  try
                    logtailLinecount := StrToInt(logtailLinecountstr);
                    logdatei.includelogtail(
                      fname, logtailLinecount, logtailEncodingstr);
                  except
                    on E: Exception do
                    begin
                      Logdatei.log('IncludeLogFile "' + Fname + '"', LLwarning);
                      Logdatei.log(
                        ' Failed to include log file, system message: "' +
                        E.Message + '"',
                        LLwarning);
                    end
                  end;
                end
                else
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    InfoSyntaxError);
              end;

              tsShrinkFileToMB:
              begin
                syntaxCheck :=
                  EvaluateString(Remaining, Remaining, FName, InfoSyntaxError);
                if syntaxCheck and (Remaining <> '') then
                  syntaxCheck :=
                    EvaluateString(Remaining, Remaining, logtailLinecountstr,
                    InfoSyntaxError);
                if syntaxCheck then
                begin
                  try
                    logtailLinecount := StrToInt(logtailLinecountstr);
                    shrinkFileToMB(fname, logtailLinecount);
                  except
                    on E: Exception do
                    begin
                      Logdatei.log('shrinkFileToMB "' + Fname + '"', LLwarning);
                      Logdatei.log(' Failed to shrink file, system message: "' +
                        E.Message + '"',
                        LLwarning);
                    end
                  end;
                end
                else
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    InfoSyntaxError);
              end;


              tsShowBitmap:
              begin
                SyntaxCheck := True;

                if PathDelim <> '/' then
                begin
                  posSlash := pos('/', Remaining);
                  if posSlash > 0 then
                  begin
                    if (length(Remaining) >= posSlash + 1) and
                      (Remaining[posSlash + 1] in ['1'..'9']) then
                    begin
                      imageNo := Ord(Remaining[posSlash + 1]) - Ord('0');

                      if (imageNo > 4) or
                        (length(Remaining) >= posSlash + 2) and
                        (Remaining[posSlash + 2] in ['0'..'9']) then
                      begin
                        SyntaxCheck := False;
                        InfoSyntaxError := 'only supported 1 .. 4 ';
                      end;

                      system.Delete(Remaining, posSlash, 2);
                    end
                    else
                    begin
                      SyntaxCheck := False;
                      InfoSyntaxError :=
                        'after "/", a number between 1 and 9 is expected';
                    end;
                  end
                  else
                       {$IFDEF GUI}
                    imageNo := centralImageNo;
                       {$ENDIF GUI}
                end
                else
                       {$IFDEF GUI}
                  imageNo := centralImageNo;
                      {$ENDIF GUI}

                if syntaxCheck then
                begin
                  Remaining := cutLeftBlanks(Remaining);

                  Labeltext := '';
                  FName := '';

                  if Remaining = '' then
                  begin
                     (*
                      // set back to default
                       if imageNo <= wibtch.maxBitmaps
                       then
                       Begin
                         Labeltext := wibtch.labelTexts [imageNo];
                         FName := wibtch.bitmapFnames [imageNo];
                       End
                     *)
                  end

                  else
                  begin
                    syntaxCheck :=
                      EvaluateString(Remaining, Remaining, FName, InfoSyntaxError);

                    if syntaxCheck and (Remaining <> '') then
                      syntaxCheck :=
                        EvaluateString(Remaining, Remaining, Labeltext, InfoSyntaxError);
                  end;
                end;

                if syntaxCheck then
                begin
                  if FName <> '' then
                    FName := ExpandFileName(FName);
                     {$IFDEF GUI}
                  FBatchOberflaeche.setPicture(FName, Labeltext);
                     {$ENDIF GUI}
                end
                else
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    InfoSyntaxError);

              end;

              tsChangeDirectory:
              begin
                syntaxCheck := False;
                if skip('=', remaining, Parameter, InfoSyntaxError) then
                  syntaxCheck := True
                else if EvaluateString(Remaining, Remaining,
                  Parameter, InfoSyntaxError) then
                  syntaxCheck := True;

                if syntaxCheck then
                begin
                  Parameter := ExpandFileName(Parameter);
                  if DirectoryExists(Parameter) then
                    ChangeDirectory(parameter)
                  else
                    LogDatei.log('Error at ChangeDirectory: ' + Parameter +
                      ' is not a directory', LLError);
                end;
              end;


              tsPause:
              begin
                syntaxCheck := False;
                if skip('=', remaining, Parameter, InfoSyntaxError) then
                  syntaxCheck := True
                else if EvaluateString(Remaining, Remaining,
                  Parameter, InfoSyntaxError) then
                  syntaxCheck := True;

                if syntaxCheck then
                begin
                    {$IFDEF GUI}
                  if messagedlg(Parameter + LineEnding + rsReadyToContinue,
                    mtConfirmation, [mbYes], 0) = mrNo then
                    ActionResult := tsrExitProcess;
                    {$ENDIF GUI}
                end
                else
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    InfoSyntaxError);
              end;

              tsSleep:
              begin
                syntaxCheck := False;
                processmess;

                try
                  try
                    // may be just a number (without quotes)
                    sleepSecs := StrToInt(Remaining);
                    syntaxCheck := True;
                  except
                    // it could be also a string Expressionstr
                    if EvaluateString(Remaining, Remaining, Parameter,
                      InfoSyntaxError) then
                      syntaxCheck := True;
                    // which should contain a number
                    try
                      if syntaxCheck then
                        sleepSecs := StrToInt(Parameter);
                    except;
                      syntaxCheck := False;
                      InfoSyntaxError :=
                        remaining + ' -- expected an integer (number of secs) ';
                    end;
                  end;
                except
                  syntaxCheck := False;
                  InfoSyntaxError :=
                    remaining + ' -- expected an integer (number of secs) ';
                end;

                if syntaxCheck then
                begin
                  LogDatei.log('sleep ' + IntToStr(sleepSecs) + ' seconds...', LLDebug2);
                  Sleep(1000 * sleepSecs);
                end
                else
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    InfoSyntaxError);

                processmess;

              end;

              tsStop:
              begin
                syntaxCheck := False;
                if skip('=', remaining, Parameter, InfoSyntaxError) then
                  syntaxCheck := True
                else if EvaluateString(Remaining, Remaining,
                  Parameter, InfoSyntaxError) then
                  syntaxCheck := True;

                if syntaxCheck then
                begin
                    {$IFDEF GUI}
                  if messagedlg(Parameter + LineEnding + rsAbortProgram,
                    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
                    ActionResult := tsrExitProcess;
                    {$ELSE GUI}
                  ActionResult := tsrExitProcess;
                    {$ENDIF GUI}
                end
                else
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    InfoSyntaxError);
              end;


              tsComment:
              begin
                syntaxCheck := False;
                if skip('=', remaining, Parameter, InfoSyntaxError) then
                  syntaxCheck := True
                else if EvaluateString(Remaining, Remaining,
                  Parameter, InfoSyntaxError) then
                  syntaxCheck := True;

                if syntaxCheck then
                  LogDatei.log('comment: ' + Parameter, LLnotice);
              end;

              tsActionProgress:
              begin
                syntaxCheck := False;
                if skip('=', remaining, Parameter, InfoSyntaxError) then
                  syntaxCheck := True
                else if EvaluateString(Remaining, Remaining,
                  Parameter, InfoSyntaxError) then
                  syntaxCheck := True;
                if syntaxCheck then
                  LogDatei.log('set ActionProgress to: ' + Parameter, LLInfo);
                opsidata.setActionProgress(Parameter);
              end;


              tsLogWarning:
              begin
                syntaxCheck := False;
                if skip('=', remaining, Parameter, InfoSyntaxError) then
                  syntaxCheck := True
                else if EvaluateString(Remaining, Remaining,
                  Parameter, InfoSyntaxError) then
                  syntaxCheck := True
                else
                  InfoSyntaxError := 'Info string expected';

                if syntaxCheck then
                begin
                  LogDatei.log('Warning: ' + Parameter, LLwarning);
                  FNumberOfWarnings := FNumberOfWarnings + 1;
                end
                else
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    InfoSyntaxError);

              end;

              tsLogError:
              begin
                syntaxCheck := False;
                if skip('=', remaining, Parameter, InfoSyntaxError) then
                  syntaxCheck := True
                else if EvaluateString(Remaining, Remaining,
                  Parameter, InfoSyntaxError) then
                  syntaxCheck := True
                else
                  InfoSyntaxError := 'Info string expected';

                if syntaxCheck then
                begin
                  LogDatei.log('Error: ' + Parameter, LLerror);
                  FNumberOfErrors := FNumberOfErrors + 1;
                end
                else
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    InfoSyntaxError);
              end;

              tsSetSkinDir:
              begin
                syntaxCheck := False;
                if EvaluateString(Remaining, Remaining, Parameter, InfoSyntaxError)
                then
                  syntaxCheck := True
                else
                  InfoSyntaxError := 'Expected a string (directory path)';

                if syntaxCheck then
                begin
                  if Parameter <> '' then
                    FName := ExpandFileName(Parameter);
                    {$IFDEF GUI}
                  FBatchOberflaeche.loadSkin(Parameter);
                  processMess;
                    {$ENDIF GUI}
                end
                else
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    InfoSyntaxError);
              end;


              tsKillTask:
              begin
                syntaxCheck := False;
                if EvaluateString(Remaining, Remaining, Fname, InfoSyntaxError)
                then
                begin
                  syntaxCheck := True;
                     {$IFNDEF WIN64}
                  if KillTask(Fname, Info) then
                  begin
                    LogDatei.Log(info, LLInfo);
                  end
                  else
                  begin
                    LogDatei.log('"' + Fname + '" could not be killed, ' +
                      info, LLWarning);
                  end;
                     {$ELSE WIN64}
                  LogDatei.Log('Not implemnted on win64', LLWarning);
                     {$ENDIF WIN64}
                end
                else
                  ActionResult :=
                    reportError(Sektion, linecounter, Expressionstr, InfoSyntaxError);
              end;

              tsShellcall:
              begin
                if Skip('(', Remaining, Remaining, InfoSyntaxError) then
                  if EvaluateString(Remaining, Remaining, s1, InfoSyntaxError)
                  then
                    if Skip(')', Remaining, Remaining, InfoSyntaxError)
                    then
                    begin
                      syntaxCheck := True;
                      try
                        //LogDatei.log ('Executing0 ' + s1, LLInfo);
                        dummylist := execShellCall(s1, 'sysnative', 0, True);
                      except
                        on e: Exception do
                        begin
                          LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
                          LogDatei.log('Error executing :' + s1 + ' : ' + e.message,
                            LLError);
                          FNumberOfErrors := FNumberOfErrors + 1;
                          LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
                        end;
                      end;
                    end;
              end;


              tsPowershellcall:
              begin
                 {$IFDEF UNIX}
                LogDatei.log('Error powershellcall not implemented on Linux ',
                  LLError);
                  {$ENDIF Linux}
                  {$IFDEF WINDOWS}
                s2 := '';
                s3 := '';
                tmpstr2 := '';
                tmpbool := True; // sysnative
                tmpbool1 := True; // handle execution policy
                syntaxCheck := False;
                if Skip('(', Remaining, Remaining, InfoSyntaxError) then
                  if EvaluateString(Remaining, tmpstr, s1, InfoSyntaxError)
                  // next after ',' or ')'
                  then
                    if Skip(',', tmpstr, tmpstr1, tmpstr3) then
                      if EvaluateString(tmpstr1, tmpstr2, s2, tmpstr3) then;
                if s2 = '' then
                begin
                  // only one parameter
                  if Skip(')', tmpstr, Remaining, InfoSyntaxError) then
                  begin
                    syntaxCheck := True;
                    s2 := 'sysnative';
                  end;
                end
                else
                begin
                  // got second parameter
                  tmpbool := True;
                  if lowercase(s2) = '32bit' then
                    tmpbool := False
                  else if lowercase(s2) = '64bit' then
                    tmpbool := True
                  else if lowercase(s2) = 'sysnative' then
                    tmpbool := True
                  else
                  begin
                    InfoSyntaxError :=
                      'Error: unknown parameter: ' + s2 +
                      ' expected one of 32bit,64bit,sysnative - fall back to sysnative';
                    syntaxCheck := False;
                  end;
                  // three parameter ?
                  if Skip(',', tmpstr2, tmpstr1, tmpstr3) then
                  begin
                    if EvaluateString(tmpstr1, tmpstr2, s3, tmpstr3) then
                    begin
                      // got third parameter
                      if Skip(')', tmpstr2, Remaining, InfoSyntaxError) then
                      begin
                        if TryStrToBool(s3, tmpbool1) then
                          syntaxCheck := True
                        else
                        begin
                          syntaxCheck := False;
                          InfoSyntaxError :=
                            'Error: boolean string (true/false) expected but got: ' + s3;
                        end;
                      end;
                    end;
                  end
                  else
                  if Skip(')', tmpstr2, Remaining, InfoSyntaxError) then
                  begin
                    // two parameter
                    syntaxCheck := True;
                  end;
                end;
                if syntaxCheck then
                begin
                  try
                    execPowershellCall(s1, s2, 0, True, False, tmpbool1);
                  except
                    on e: Exception do
                    begin
                      LogDatei.log('Error executing :' + s1 +
                        ' : with powershell: ' + e.message,
                        LLError);
                    end
                  end;
                end;
                {$ENDIF WINDOWS}
              end;

              tsExecuteSection:
              begin
                logdatei.log('Execution of: ' + ArbeitsSektion.Name +
                  ' ' + Remaining, LLNotice);
                syntaxCheck := False;
                tmplist := TXStringlist.Create;
                if Skip('(', Remaining, Remaining, InfoSyntaxError) then
                  if EvaluateString(Remaining, Remaining, call, InfoSyntaxError) then
                  begin
                    //savelogsindentlevel := LogDatei.LogSIndentLevel;
                    localSection :=
                      TWorkSection.Create(LogDatei.LogSIndentLevel + 1, Sektion);
                    GetWord(call, Expressionstr, tmpstr, WordDelimiterSet1);
                    localSection.Name := Expressionstr;

                    //localKindOfStatement := findKindOfStatement (s2, SecSpec, s1);
                    localKindOfStatement :=
                      FindKindOfStatement(Expressionstr, SectionSpecifier, call);
                    logdatei.log('Execution of: ' + localSection.Name +
                      ' ' + tmpstr, LLNotice);

                    if not (localKindOfStatement in
                      [tsDOSBatchFile, tsDOSInAnIcon,
                      tsShellBatchFile, tsShellInAnIcon,
                      tsExecutePython, tsExecuteWith, tsExecuteWith_escapingStrings,
                      tsWinBatch, tsRegistryHack, tsFileActions]) then
                    begin
                      logdatei.log('not implemented for this kind of section', LLerror);
                      InfoSyntaxError := 'not implemented for this kind of section';
                      syntaxCheck := False;
                    end
                    else
                    begin
                      if Skip(')', Remaining, Remaining, InfoSyntaxError) then
                      begin
                        syntaxCheck := True;
                        if not SearchForSectionLines(
                          self, sektion, callingsektion, Expressionstr,
                          TXStringList(localSection), startlineofsection,
                          True, True, False) then
                        begin
                          InfoSyntaxError := 'Section "' + Expressionstr + '" not found';
                          syntaxCheck := False;
                          logdatei.log('Section "' + Expressionstr +
                            '" not found', LLerror);
                        end
                        else
                        begin
                          if localKindOfStatement in
                            [tsExecutePython, tsExecuteWith_escapingStrings] then
                          begin
                            ApplyTextConstants(TXStringList(localSection), True);
                            ApplyTextVariables(TXStringList(localSection), True);
                          end
                          else
                          begin
                            ApplyTextConstants(TXStringList(localSection), False);
                            ApplyTextVariables(TXStringList(localSection), False);
                          end;
                          if not syntaxCheck then
                          begin
                            ActionResult :=
                              reportError(Sektion, linecounter,
                              Sektion.strings[linecounter - 1], InfoSyntaxError);
                          end
                          else
                            case localKindOfStatement of

                              tsExecutePython:
                                execPython(localSection, tmpstr,
                                  False {no catchout}, 1,
                                  [ttpWaitOnTerminate], tmplist);

                              tsExecuteWith_escapingStrings, tsExecuteWith:
                                executeWith(localSection, tmpstr,
                                  False {no catchout}, 1, tmplist);

                              tsDOSBatchFile, tsDOSInAnIcon, tsShellBatchFile,
                              tsShellInAnIcon:
                                execDOSBatch(localSection, tmpstr,
                                  SW_HIDE, False {no catchout}, 1,
                                  [ttpWaitOnTerminate], tmplist);

                              tsWinBatch:
                                ActionResult :=
                                  parseAndCallWinbatch(localSection, tmpstr);

                              tsRegistryHack:
                                ActionResult :=
                                  parseAndCallRegistry(localSection, tmpstr);

                              tsFileActions:
                              begin
                                if localSection.Count > 0 then
                                  for tmpint := 0 to localSection.Count - 1 do
                                    logdatei.log(localSection.Strings[tmpint], LLDebug2);
                                flag_all_ntuser := False;
                                // if this is a 'ProfileActions' which is called as sub in Machine mode
                                // so run registry sections implicit as /Allntuserdats
                                if runProfileActions then
                                  flag_all_ntuser := True;
                                ActionResult := doFileActions(localSection, tmpstr);
                                logdatei.log('Finished of: ' +
                                  localSection.Name + ' ' + tmpstr, LLNotice);
                              end;
                            end;
                        end;
                      end;

                    end;

                    if localSection <> nil then
                      localSection.Free;

                    // LogDatei.LogSIndentLevel := saveLogSIndentLevel;
                  end;
                if tmplist <> nil then
                  FreeAndNil(tmplist);
              end;


              {$IFDEF WIN32}
              tsBlockInput:
              begin
                syntaxCheck := False;
                if EvaluateString(Remaining, Remaining, Parameter,
                  InfoSyntaxError) then
                begin
                  if UpperCase(Parameter) = UpperCase('True') then
                  begin
                    syntaxCheck := True;
                    ActionResult := tsrPositive;
                    if winBlockInput(True) then
                      LogDatei.log('Blocking Input', LLInfo)
                    else
                      LogDatei.log('Failed Blocking Input ...', LLWarning);
                  end

                  else if UpperCase(Parameter) = UpperCase('False') then
                  begin
                    syntaxCheck := True;
                    ActionResult := tsrPositive;
                    if winBlockInput(False) then
                      LogDatei.log('Unblocking Input', LLInfo)
                    else
                      LogDatei.log('Failed Unblocking Input ...', LLWarning);
                  end;
                end;
                if not syntaxCheck then
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    'Expected a boolean string (true/false)');
                if not (Remaining = '') then
                  LogDatei.log('Error at Blockinput: trailing parameter: ' +
                    Parameter + ' ignored', LLWarning);
              end;


              tsLocalAdmin:
              begin
                if UpperCase(Remaining) = UpperCase('/Create1') then
                begin
                  LogDatei.log('Creating temporary local admin ...', LLNotice);
                  ActionResult := tsrPositive;
                  if CreateTemporaryLocalAdmin(traAdmin) then
                  begin
                    LogDatei.log('Created temporary local admin ...', LLInfo);

                    FConstValuesList.Strings[FConstList.IndexOf('%AppDataDir%')] :=
                      GetAppDataPath;
                    FConstValuesList.Strings[FConstList.IndexOf('%CurrentAppDataDir%')]
                    := GetAppDataPath;
                    FConstValuesList.Strings[FConstList.IndexOf(
                      '%CurrentStartmenuDir%')] := GetStartmenuPath;
                    FConstValuesList.Strings[FConstList.IndexOf('%CurrentDesktopDir%')]
                    := GetDesktopPath;
                    FConstValuesList.Strings[FConstList.IndexOf('%CurrentStartupDir%')]
                    := GetStartupPath;
                    FConstValuesList.Strings[FConstList.IndexOf(
                      '%CurrentProgramsDir%')] := GetProgramsPath;
                    FConstValuesList.Strings[FConstList.IndexOf(
                      '%CurrentSendToDir%')] :=
                      GetSendToPath;
                    FConstValuesList.Strings[FConstList.IndexOf('%CurrentProfileDir%')]
                    := getUserProfilePath;
                    FConstValuesList.Strings[FConstList.IndexOf('%AppDataDir%')] :=
                      GetAppDataPath;
                    FConstValuesList.Strings[FConstList.IndexOf('%AppDataDir%')] :=
                      GetAppDataPath;

                    ApplyTextConstants(TXStringList(ArbeitsSektion), False);
                  end
                  else
                    LogDatei.log('Failed creating temporary local admin ...', LLWarning);
                end

                else if UpperCase(Remaining) = UpperCase('/Create2') then
                begin
                  LogDatei.log('Creating temporary local admin ...', LLInfo);
                  ActionResult := tsrPositive;
                  if CreateTemporaryLocalAdmin(traAdminProfile) then
                  begin
                    LogDatei.log('Created temporary local admin ...', LLNotice);

                    FConstValuesList.Strings[FConstList.IndexOf('%AppDataDir%')] :=
                      GetAppDataPath;
                    FConstValuesList.Strings[FConstList.IndexOf('%CurrentAppDataDir%')]
                    := GetAppDataPath;
                    FConstValuesList.Strings[FConstList.IndexOf(
                      '%CurrentStartmenuDir%')] := GetStartmenuPath;
                    FConstValuesList.Strings[FConstList.IndexOf('%CurrentDesktopDir%')]
                    := GetDesktopPath;
                    FConstValuesList.Strings[FConstList.IndexOf('%CurrentStartupDir%')]
                    := GetStartupPath;
                    FConstValuesList.Strings[FConstList.IndexOf(
                      '%CurrentProgramsDir%')] := GetProgramsPath;
                    FConstValuesList.Strings[FConstList.IndexOf(
                      '%CurrentSendToDir%')] :=
                      GetSendToPath;
                    FConstValuesList.Strings[FConstList.IndexOf('%CurrentProfileDir%')]
                    := getUserProfilePath;
                    FConstValuesList.Strings[FConstList.IndexOf('%AppDataDir%')] :=
                      GetAppDataPath;
                    FConstValuesList.Strings[FConstList.IndexOf('%AppDataDir%')] :=
                      GetAppDataPath;

                    ApplyTextConstants(TXStringList(ArbeitsSektion), False);
                  end
                  else
                    LogDatei.log('Failed creating temporary local admin ...', LLWarning);
                end

                else if UpperCase(Remaining) = UpperCase('/Create3') then
                begin
                  LogDatei.log('Creating temporary local admin ...', LLInfo);
                  ActionResult := tsrPositive;
                  if CreateTemporaryLocalAdmin(traAdminProfileImpersonate) then
                  begin
                    LogDatei.log('Created temporary local admin ...', LLNotice);

                    FConstValuesList.Strings[FConstList.IndexOf('%AppDataDir%')] :=
                      GetAppDataPath;
                    FConstValuesList.Strings[FConstList.IndexOf('%CurrentAppDataDir%')]
                    := GetAppDataPath;
                    FConstValuesList.Strings[FConstList.IndexOf(
                      '%CurrentStartmenuDir%')] := GetStartmenuPath;
                    FConstValuesList.Strings[FConstList.IndexOf('%CurrentDesktopDir%')]
                    := GetDesktopPath;
                    FConstValuesList.Strings[FConstList.IndexOf('%CurrentStartupDir%')]
                    := GetStartupPath;
                    FConstValuesList.Strings[FConstList.IndexOf(
                      '%CurrentProgramsDir%')] := GetProgramsPath;
                    FConstValuesList.Strings[FConstList.IndexOf(
                      '%CurrentSendToDir%')] :=
                      GetSendToPath;
                    FConstValuesList.Strings[FConstList.IndexOf('%CurrentProfileDir%')]
                    := getUserProfilePath;
                    FConstValuesList.Strings[FConstList.IndexOf('%AppDataDir%')] :=
                      GetAppDataPath;
                    FConstValuesList.Strings[FConstList.IndexOf('%AppDataDir%')] :=
                      GetAppDataPath;

                    ApplyTextConstants(TXStringList(ArbeitsSektion), False);
                  end
                  else
                    LogDatei.log('Failed creating temporary local admin ...', LLWarning);
                end

                else if UpperCase(Remaining) = UpperCase('/Create4') then
                begin
                  LogDatei.log('Creating temporary local admin ...', LLInfo);
                  ActionResult := tsrPositive;
                  if CreateTemporaryLocalAdmin(traAdminProfileImpersonateExplorer) then
                  begin
                    LogDatei.log('Created temporary local admin ...', LLNotice);

                    FConstValuesList.Strings[FConstList.IndexOf('%AppDataDir%')] :=
                      GetAppDataPath;
                    FConstValuesList.Strings[FConstList.IndexOf('%CurrentAppDataDir%')]
                    := GetAppDataPath;
                    FConstValuesList.Strings[FConstList.IndexOf(
                      '%CurrentStartmenuDir%')] := GetStartmenuPath;
                    FConstValuesList.Strings[FConstList.IndexOf('%CurrentDesktopDir%')]
                    := GetDesktopPath;
                    FConstValuesList.Strings[FConstList.IndexOf('%CurrentStartupDir%')]
                    := GetStartupPath;
                    FConstValuesList.Strings[FConstList.IndexOf(
                      '%CurrentProgramsDir%')] := GetProgramsPath;
                    FConstValuesList.Strings[FConstList.IndexOf(
                      '%CurrentSendToDir%')] :=
                      GetSendToPath;
                    FConstValuesList.Strings[FConstList.IndexOf('%CurrentProfileDir%')]
                    := getUserProfilePath;
                    FConstValuesList.Strings[FConstList.IndexOf('%AppDataDir%')] :=
                      GetAppDataPath;
                    FConstValuesList.Strings[FConstList.IndexOf('%AppDataDir%')] :=
                      GetAppDataPath;

                    ApplyTextConstants(TXStringList(ArbeitsSektion), False);
                  end
                  else
                    LogDatei.log('Failed creating temporary local admin ...', LLWarning);
                end

                else if UpperCase(Remaining) = UpperCase('/Delete') then
                begin
                  LogDatei.log('Deleting temporary local admin ...', LLInfo);
                  ActionResult := tsrPositive;
                  if opsiSetupAdmin_created then
                    if DeleteTemporaryLocalAdmin then
                    begin
                      LogDatei.log('Deleted temporary local admin ...', LLNotice);

                      FConstValuesList.Strings[FConstList.IndexOf('%AppDataDir%')] :=
                        GetAppDataPath;
                      FConstValuesList.Strings[FConstList.IndexOf(
                        '%CurrentAppDataDir%')] := GetAppDataPath;
                      FConstValuesList.Strings[FConstList.IndexOf(
                        '%CurrentStartmenuDir%')] := GetStartmenuPath;
                      FConstValuesList.Strings[FConstList.IndexOf(
                        '%CurrentDesktopDir%')] := GetDesktopPath;
                      FConstValuesList.Strings[FConstList.IndexOf(
                        '%CurrentStartupDir%')] := GetStartupPath;
                      FConstValuesList.Strings[FConstList.IndexOf(
                        '%CurrentProgramsDir%')] := GetProgramsPath;
                      FConstValuesList.Strings[FConstList.IndexOf(
                        '%CurrentSendToDir%')] := GetSendToPath;
                      FConstValuesList.Strings[FConstList.IndexOf(
                        '%CurrentProfileDir%')] := getUserProfilePath;
                      FConstValuesList.Strings[FConstList.IndexOf('%AppDataDir%')] :=
                        GetAppDataPath;
                      FConstValuesList.Strings[FConstList.IndexOf('%AppDataDir%')] :=
                        GetAppDataPath;

                      ApplyTextConstants(TXStringList(ArbeitsSektion), False);
                    end
                    else
                      LogDatei.log('Failed deleting temporary local admin ...',
                        LLWarning);
                end
                else if Remaining = '' then
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    'Parameter needed')
                else
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    'not an allowed Parameter');
              end;
                {$ENDIF WIN32}


              tsExitWindows:
              begin
                if runLoginScripts then
                begin
                  LogDatei.log(
                    'ExitWindows is ignored while running in login script mode',
                    LLError);
                end
                else
                begin
                  if UpperCase(Remaining) = UpperCase('/ImmediateReboot')
                  then
                  begin
                    PerformExitWindows := txrImmediateReboot;
                    ActionResult := tsrExitWindows;
                    scriptstopped := True;
                    LogDatei.log('ExitWindows set to Immediate Reboot', BaseLevel);
                  end
                  else if UpperCase(Remaining) = UpperCase('/ImmediateLogout')
                  then
                  begin
                    PerformExitWindows := txrImmediateLogout;
                    LogDatei.log('', BaseLevel);
                    ActionResult := tsrExitWindows;
                    scriptstopped := True;
                    LogDatei.log('ExitWindows set to Immediate Logout', BaseLevel);
                  end
                  else if UpperCase(Remaining) = UpperCase('/Reboot')
                  then
                  begin
                    PerformExitWindows := txrReboot;
                    LogDatei.log('', BaseLevel);
                    LogDatei.log('ExitWindows set to Reboot', BaseLevel);
                  end
                  else if UpperCase(Remaining) = UpperCase('/RebootWanted')
                  then
                  begin
                    if PerformExitWindows < txrRegisterForReboot then
                    begin
                      PerformExitWindows := txrRegisterForReboot;
                      LogDatei.log('', BaseLevel);
                      LogDatei.log('ExitWindows set to RegisterReboot', BaseLevel);
                    end
                    else
                      LogDatei.log('ExitWindows already set to Reboot', BaseLevel);
                  end
                  else if UpperCase(Remaining) = UpperCase('/LogoutWanted')
                  then
                  begin
                    if PerformExitWindows < txrRegisterForLogout then
                    begin
                      PerformExitWindows := txrRegisterForLogout;
                      LogDatei.log('', BaseLevel);
                      LogDatei.log('ExitWindows set to RegisterForLogout', BaseLevel);
                    end
                    else
                      LogDatei.log('ExitWindows already set to (Register)Reboot',
                        BaseLevel);
                  end

                  else if UpperCase(Remaining) = UpperCase('/ShutdownWanted')
                  then
                  begin
                    PerformShutdown := tsrRegisterForShutdown;
                    LogDatei.log('', BaseLevel);
                    LogDatei.log('PerformShutdown set to RegisterForShutdown',
                      BaseLevel);
                  end

                  else if Remaining = '' then
                    ActionResult :=
                      reportError(Sektion, linecounter,
                      Sektion.strings[linecounter - 1], 'Parameter needed')
                  else
                    ActionResult :=
                      reportError(Sektion, linecounter,
                      Sektion.strings[linecounter - 1], 'not an allowed Parameter');
                end; // not loginscripts
              end;

              tsAutoActivityDisplay:
                if skip('=', remaining, remaining, InfoSyntaxError) then
                begin
                  if UpperCase(Remaining) = 'TRUE' then
                  begin
                    LogDatei.log('AutoActivityDisplay was ' +
                      BoolToStr(AutoActivityDisplay, True) + ' is set to true', LLInfo);
                    AutoActivityDisplay := True;
                  end
                  else
                  begin
                    LogDatei.log('AutoActivityDisplay was ' +
                      BoolToStr(AutoActivityDisplay, True) + ' is set to false', LLInfo);
                    AutoActivityDisplay := False;
                  end;
                end
                else
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    InfoSyntaxError);

              tsforceLogInAppendMode:
                if skip('=', remaining, remaining, InfoSyntaxError) then
                begin
                  if UpperCase(Remaining) = 'TRUE' then
                  begin
                    LogDatei.log('forceLogInAppendMode was ' +
                      BoolToStr(forceLogInAppendMode, True) + ' is set to true', LLInfo);
                    forceLogInAppendMode := True;
                    if Assigned(LogDatei) then
                      LogDatei.Appendmode := True;
                  end
                  else
                  begin
                    LogDatei.log('forceLogInAppendMode was ' +
                      BoolToStr(forceLogInAppendMode, True) +
                      ' is set to false', LLInfo);
                    forceLogInAppendMode := False;
                    if Assigned(LogDatei) then
                      LogDatei.Appendmode := False;
                  end;
                end
                else
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    InfoSyntaxError);

              tsSetDebug_Prog:
                if skip('=', remaining, remaining, InfoSyntaxError) then
                begin
                  if UpperCase(Remaining) = 'TRUE' then
                  begin
                    LogDatei.log('debug_prog was ' + BoolToStr(
                      logdatei.debug_prog, True) + ' is set to true', LLInfo);
                    logdatei.debug_prog := True;
                  end
                  else
                  begin
                    LogDatei.log('debug_prog was ' + BoolToStr(
                      logdatei.debug_prog, True) + ' is set to false', LLInfo);
                    logdatei.debug_prog := False;
                  end;
                end
                else
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    InfoSyntaxError);


              tsFatalOnSyntaxError:
                if skip('=', remaining, remaining, InfoSyntaxError) then
                begin
                  if UpperCase(Remaining) = 'TRUE' then
                  begin
                    LogDatei.log('FatalOnSyntaxError was ' + BoolToStr(
                      FatalOnSyntaxError, True) + ' is set to true', LLInfo);
                    FatalOnSyntaxError := True;
                  end
                  else
                  begin
                    LogDatei.log('FatalOnSyntaxError was ' + BoolToStr(
                      FatalOnSyntaxError, True) + ' is set to false', LLInfo);
                    FatalOnSyntaxError := False;
                    if (FExtremeErrorLevel = LevelFatal) and
                      (LogDatei.ActionProgress = 'Syntax Error') then
                      FExtremeErrorLevel := Level_not_initialized;
                  end;
                end
                else
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    InfoSyntaxError);

              tsFatalOnRuntimeError:
                if skip('=', remaining, remaining, InfoSyntaxError) then
                begin
                  if UpperCase(Remaining) = 'TRUE' then
                  begin
                    LogDatei.log('FatalOnRuntimeError was ' + BoolToStr(
                      FatalOnRuntimeError, True) + ' is set to true', LLInfo);
                    FatalOnRuntimeError := True;
                  end
                  else
                  begin
                    LogDatei.log('FatalOnRuntimeError was ' + BoolToStr(
                      FatalOnRuntimeError, True) + ' is set to false', LLInfo);
                    FatalOnRuntimeError := False;
                  end;
                end
                else
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    InfoSyntaxError);

              tsSetExitOnError:
                if skip('=', remaining, remaining, InfoSyntaxError) then
                begin
                  if UpperCase(Remaining) = 'TRUE' then
                  begin
                    LogDatei.log('ExitOnError was ' + BoolToStr(
                      ExitOnError, True) + ' is set to true', LLInfo);
                    ExitOnError := True;
                  end
                  else
                  begin
                    LogDatei.log('ExitOnError was ' + BoolToStr(
                      ExitOnError, True) + ' is set to false', LLInfo);
                    ExitOnError := False;
                  end;
                end
                else
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    InfoSyntaxError);

              tsSetFatalError:
                if remaining = '' then
                begin
                  FExtremeErrorLevel := LevelFatal;
                  LogDatei.log('Error level set to fatal', LLCritical);
                  ActionResult := tsrFatalError;
                  scriptstopped := True;
                  NestLevel := NestingLevel;
                  ActLevel := NestLevel;
                end
                else
                begin
                  EvaluateString(remaining, remaining, Parameter, infosyntaxerror);
                  if remaining = '' then
                  begin
                    FExtremeErrorLevel := LevelFatal;
                    LogDatei.log('Error level set to fatal', LLCritical);
                    ActionResult := tsrFatalError;
                    LogDatei.ActionProgress := Parameter;
                    scriptstopped := True;
                    //NestLevel:= NestingLevel;
                    //ActLevel:= NestLevel;
                  end
                  else
                    ActionResult :=
                      reportError(Sektion, linecounter,
                      Sektion.strings[linecounter - 1],
                      ' none or one parameter expected');
                end;

              tsSetSuccess:
                if remaining = '' then
                begin
                  LogDatei.log('Set: Exit Script as successful', LLnotice);
                  ActionResult := tsrExitProcess;
                  scriptstopped := True;
                  //NestLevel:= NestingLevel;
                  //ActLevel:= NestLevel;
                end
                else
                begin
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    ' no parameter expected');
                end;

              tsSetNoUpdate:
                if remaining = '' then
                begin
                  LogDatei.log('Set: Do not run Update script', LLnotice);
                  runUpdate := False;
                end
                else
                begin
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    ' no parameter expected');
                end;

              tsSetSuspended:
                if remaining = '' then
                begin
                  LogDatei.log('Set: Stop script and leave switches unchanged',
                    LLnotice);
                  runUpdate := False;
                  script.suspended := True;
                  scriptstopped := True;
                  ActionResult := tsrExitProcess;
                  //NestLevel:= NestingLevel;
                  //ActLevel:= NestLevel;
                end
                else
                begin
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    ' no parameter expected');
                end;

              tsSetMarkerErrorNumber:
                if remaining = '' then
                begin
                  LogDatei.ErrorNumberMarked := Logdatei.NumberOfErrors;
                  LogDatei.log('Marked error number ' +
                    IntToStr(LogDatei.ErrorNumberMarked),
                    levelcomplete);
                end
                else
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    ' end of line expected');

              tsSaveVersionToProfile:
                if remaining = '' then
                begin
                  saveVersionToProfile;
                  LogDatei.log(
                    'Saved productversion-packageversion to local profile', LLNotice);
                end
                else
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    ' end of line expected');


              tsSetReportMessages:
                if skip('=', remaining, remaining, InfoSyntaxError) then
                begin
                  if (UpperCase(Remaining) = 'OFF') or
                    (UpperCase(Remaining) = 'FALSE') then
                  begin
                    LogDatei.log('ScriptErrorMessages was ' + BoolToStr(
                      ReportMessages, True) + ' is set to false', LLinfo);
                    ReportMessages := False;
                  end
                  else
                  begin
                    LogDatei.log('ScriptErrorMessages was ' + BoolToStr(
                      ReportMessages, True) + ' is set to true', LLinfo);
                    ReportMessages := True;
                  end;
                end
                else
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    InfoSyntaxError);

              tsSetTimeMark:
              begin
                markedTime := Time;

                LogDatei.log(FormatDateTime('hh:mm:ss:zzz',
                  markedTime) + '  ============  Time marked ', baselevel);
              end;

              tsLogDiffTime:
              begin
                diffTime := Time - markedTime;

                LogDatei.log(FormatDateTime('hh:mm:ss:zzz', diffTime) +
                  '  ============  Time passed since marked time  ',
                  baselevel);
              end;


              tsSetTraceMode:
                if skip('=', remaining, remaining, InfoSyntaxError) then
                begin
                  if (UpperCase(Remaining) = 'ON') or
                    (UpperCase(Remaining) = 'TRUE') then
                  begin
                    TraceMode := True;
                    Logdatei.TraceMode := True;
                  end
                  else
                  begin
                    TraceMode := False;
                    Logdatei.TraceMode := False;
                  end;
                end
                else
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    InfoSyntaxError);

              tsSetOldLogLevel:
                if skip('=', remaining, remaining, InfoSyntaxError) then
                begin
                  try
                    LogLevel := StrToInt(Remaining) + 4;
                    LogDatei.log(
                      'Warning: depricated: <LogLevel=> please use <setLogLevel=> ',
                      LLwarning);
                    LogDatei.log('LogLevel was ' + IntToStr(LogDatei.LogLevel),
                      LLessential);
                    LogDatei.LogLevel := Loglevel;
                    LogDatei.log('LogLevel set to ' + IntToStr(LogLevel), LLessential);
                  except
                    try
                      EvaluateString(remaining, remaining, Parameter, infosyntaxerror);
                      LogLevel := StrToInt(Parameter) + 4;
                      LogDatei.log(
                        'Warning: depricated: <LogLevel=> please use <setLogLevel=> ',
                        LLwarning);
                      LogDatei.log('LogLevel was ' + IntToStr(LogDatei.LogLevel),
                        LLessential);
                      LogDatei.LogLevel := Loglevel;
                      LogDatei.log('LogLevel set to ' + IntToStr(LogLevel),
                        LLessential);
                    except
                      LogDatei.log(
                        'Integer conversion error, LogLevel could not be set, it is ' +
                        IntToStr(LogLevel), Logdatei.loglevel);
                      LogLevel := LLinfo;
                    end;
                  end;
                end
                else
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    InfoSyntaxError);


              tsSetLogLevel:
                if skip('=', remaining, remaining, InfoSyntaxError) then
                begin
                  try
                    LogLevel := StrToInt(Remaining);
                    LogDatei.log('LogLevel was ' + IntToStr(LogDatei.LogLevel), LLinfo);
                    LogDatei.LogLevel := Loglevel;
                    LogDatei.log('LogLevel set to ' + IntToStr(LogLevel), LLinfo);
                  except
                    try
                      EvaluateString(remaining, remaining, Parameter, infosyntaxerror);
                      LogLevel := StrToInt(Parameter);
                      LogDatei.log('LogLevel was ' +
                        IntToStr(LogDatei.LogLevel), LLinfo);
                      LogDatei.LogLevel := Loglevel;
                      LogDatei.log('LogLevel set to ' + IntToStr(LogLevel), LLinfo);
                    except
                      LogDatei.log(
                        'Integer conversion error, LogLevel could not be set, it is ' +
                        IntToStr(LogLevel), Logdatei.loglevel);
                      LogLevel := LLinfo;
                    end;
                  end;
                end
                else
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    InfoSyntaxError);

              tsSetConfidential:
              begin
                syntaxCheck := False;
                if EvaluateString(Remaining, Remaining, Parameter, InfoSyntaxError)
                then
                  syntaxCheck := True
                else
                  InfoSyntaxError := 'Expected a string (confidential)';

                if syntaxCheck then
                begin
                  LogDatei.AddToConfidentials(Parameter);
                end
                else
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    InfoSyntaxError);
              end;

              tsSetUsercontext:
                if skip('=', remaining, remaining, InfoSyntaxError) then
                begin
                  EvaluateString(remaining, remaining, Parameter, infosyntaxerror);
                  LogDatei.log('Usercontext was ' + usercontext, LLessential);
                  usercontext := Parameter;
                  LogDatei.log('Usercontext set to ' + usercontext, LLessential);
                end
                else
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    InfoSyntaxError);


              tsSetOutputLevel:
                if skip('=', remaining, remaining, InfoSyntaxError) then
                begin
                  try
                    OutputLevel := StrToInt(Remaining);
                    //LogDatei.log ('OutputLevel was ' + inttoStr(LogDatei.LogLevel), LLessential);
                    {$IFDEF GUI}
                    FBatchOberflaeche.setOutputLevel(OutputLevel);
                    {$ENDIF GUI}
                    LogDatei.log('OutputLevel set to ' + IntToStr(OutputLevel),
                      LLessential);
                  except
                    try
                      EvaluateString(remaining, remaining, Parameter, infosyntaxerror);
                      OutputLevel := StrToInt(Parameter);
                      //LogDatei.log ('OutputLevel was ' + inttoStr(LogDatei.LogLevel), LLessential);
                      {$IFDEF GUI}
                      FBatchOberflaeche.setOutputLevel(OutputLevel);
                      {$ENDIF GUI}
                      LogDatei.log('OutputLevel set to ' +
                        IntToStr(OutputLevel), LLessential);
                    except
                      LogDatei.log(
                        'Integer conversion error, OutputLevel could not be set, it is '
                        + IntToStr(LogLevel), Logdatei.loglevel);
                      {$IFDEF GUI}
                      FBatchOberflaeche.setOutputLevel(3);
                      {$ENDIF GUI}
                    end;
                  end;
                end
                else
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    InfoSyntaxError);

              tsSetStayOnTop:
                if skip('=', remaining, remaining, InfoSyntaxError) then
                begin
                   {$IFDEF GUI}
                  if (UpperCase(Remaining) = 'ON') or
                    (UpperCase(Remaining) = 'TRUE') then
                    FBatchOberflaeche.ForceStayOnTop(True)
                  else if (UpperCase(Remaining) = 'OFF') or
                    (UpperCase(Remaining) = 'FALSE') then
                    FBatchOberflaeche.ForceStayOnTop(False)
                  else
                    ActionResult :=
                      reportError(Sektion, linecounter,
                      Sektion.strings[linecounter - 1], Remaining +
                      ' is no valid value');
                   {$ENDIF GUI}
                end
                else
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    InfoSyntaxError);

              tsIconizeWinst:
                 {$IFDEF GUI}
              begin
                SavedBatchWindowMode := BatchWindowMode;
                BatchWindowMode := bwmIcon;
                FBatchOberflaeche.setWindowState(BatchWindowMode);
              end
                 {$ENDIF GUI}
                ;


              tsNormalizeWinst:
                 {$IFDEF GUI}
              begin
                SavedBatchWindowMode := BatchWindowMode;
                BatchWindowMode := bwmNormalWindow;
                FBatchOberflaeche.setWindowState(BatchWindowMode);
              end
                 {$ENDIF GUI}
                ;

              tsRestoreWinst:
                 {$IFDEF GUI}
              begin
                BatchWindowMode := SavedBatchWindowMode;
                FBatchOberflaeche.setWindowState(BatchWindowMode);
              end
                 {$ENDIF GUI}
                ;

              tsMaximizeWinst:
                  {$IFDEF GUI}
              begin
                SavedBatchWindowMode := BatchWindowMode;
                BatchWindowMode := bwmMaximized;
                FBatchOberflaeche.setWindowState(BatchWindowMode);
              end
                  {$ENDIF GUI}
                ;

              tsEncoding:
                ;

              tsUpdateEnvironment:
              {$IFDEF WIN32}
              begin
                dummybool := updateEnvironment;
              end
                 {$ENDIF WIN32}
                ;

              tsLoadProductProperties:
              begin
                // get clientId and productId and use it to reload Propertylist
                GetWordOrStringExpressionstr(Remaining, LppClientId,
                  Remaining, InfoSyntaxError);
                GetWordOrStringExpressionstr(Remaining, LppProductId,
                  Remaining, InfoSyntaxError);
                if ProductvarsForPC.Count = 0 then
                  LogDatei.log('Empty Property lookup table - try to load...', LLDebug)
                else
                  LogDatei.log('Existing Property lookup table - try to reload...',
                    LLDebug);
                if opsidata <> nil then
                begin
                  //LogDatei.log('Empty lookup table - try to get from opsidata',LLDebug);
                  opsidata.setActualClient(LppClientId);
                  opsidata.setActualProductName(LppProductId);
                  ProductvarsForPC := opsidata.getProductproperties;
                  LogDatei.log('Loaded lookup table from opsidata', LLDebug);
                end
                else if local_opsidata <> nil then
                begin
                  //LogDatei.log('Empty lookup table - try to get from local_opsidata',LLDebug);
                  local_opsidata.setActualClient(LppClientId);
                  local_opsidata.setActualProductName(LppProductId);
                  ProductvarsForPC := local_opsidata.getProductproperties;
                  LogDatei.log('Loaded lookup table from local_opsidata', LLDebug);
                end
                else
                begin
                  LogDatei.log(
                    'No service connection in LoadProductProperties - failed',
                    LLWarning);
                end;
              end;

              tsOpsiscriptVersionRequired, tsWinstVersionRequired:
              begin

                GetWord(Remaining, expr, Remaining, WordDelimiterWhiteSpace);
                GetWordOrStringExpressionstr(Remaining, numberString,
                  Remaining, InfoSyntaxError);
                // expr should be a comparison sign

                if getDecimalCompareSign(
                  osconf.OpsiscriptVersion, numberString, sign, InfoSyntaxError, False)
                then
                begin

                  hasRelation(expr, sign, evaluated, InfoSyntaxError);

                  if evaluated then
                  begin
                    LogDatei.log
                    (osconf.selfProductName + ' has version  ' +
                      osconf.OpsiscriptVersion + ', required is : ' +
                      expr + ' ' + numberString,
                      LLinfo);
                    LogDatei.OpsiscriptVersionRequired := numberString;
                  end
                  else
                  begin

                    LogDatei.log
                    ('Error: wrong opsi-script version,  requested :     ' +
                      expr + '  ' + numberString,
                      LLcritical);
                    FNumberOfErrors := FNumberOfErrors + 1;
                    FExtremeErrorLevel := LevelFatal;
                    LogDatei.log('Error level set to fatal', LLcritical);
                    ActionResult := tsrFatalError;
                  end;

                end
                else
                begin
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    InfoSyntaxError);
                end;
              end;

              tsPatchAnyTextFile:
              begin
                logdatei.log('Execution of: ' + ArbeitsSektion.Name +
                  ' ' + Remaining, LLNotice);
                flag_all_ntuser := False;
                // if this is a 'ProfileActions' which is called as sub in Machine mode
                // so run patches sections implicit as /Allntuserprofiles
                if runProfileActions then
                  flag_all_ntuser := True;
                if Remaining = '' then
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    'File parameter missing')
                else
                begin
                  GetWordOrStringExpressionstr(Remaining, Filename,
                    Remaining, ErrorInfo);
                  if Remaining = '' then
                    ActionResult := doTextpatch(ArbeitsSektion, Filename, '')
                  else
                    ActionResult := doTextpatch(ArbeitsSektion, Filename, Remaining);
                end;
              end;

              tsTests:
                ActionResult := doTests(ArbeitsSektion, Remaining);


              tsPatchIniFile:
              begin
                logdatei.log('Execution of: ' + ArbeitsSektion.Name +
                  ' ' + Remaining, LLNotice);
                flag_all_ntuser := False;
                flag_encoding := 'system';
                // if this is a 'ProfileActions' which is called as sub in Machine mode
                // so run patches sections implicit as /Allntuserprofiles
                if runProfileActions then
                  flag_all_ntuser := True;
                if Remaining = '' then
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    'File parameter missing')
                else
                begin
                  GetWordOrStringExpressionstr(Remaining, Filename,
                    Remaining, ErrorInfo);
                end;
                remaining := CutRightBlanks(Remaining);

                if length(remaining) > 0 then
                  goon := True;
                while goon do
                begin

                  if skip(Parameter_AllNTUserProfiles, Remaining,
                    Remaining, ErrorInfo) then
                    flag_all_ntuser := True

                  else
                  if skip('/encoding', Remaining, Remaining, ErrorInfo)
                  then
                  begin
                    if not EvaluateString(Remaining, Remaining,
                      flag_encoding, ErrorInfo) then
                    begin
                      syntaxcheck := False;
                      //ActionResult := reportError (ErrorInfo);
                    end;
                    flag_encoding := LowerCase(flag_encoding);
                    if not isSupportedEncoding(flag_encoding) then
                    begin
                      logdatei.log('Given Encoding: ' + flag_encoding +
                        ' is not supported - fall back to system encoding.',
                        LLWarning);
                      flag_encoding := 'system';
                    end;
                  end
                  else
                  begin
                    goon := False;
                    if length(remaining) > 0 then
                    begin
                      syntaxcheck := False;
                      ActionResult :=
                        reportError(Sektion, linecounter,
                        Sektion.strings[linecounter - 1], '"' + remaining +
                        '" is no valid parameter ');
                    end;
                  end;
                end;
                ActionResult := doInifilePatches(ArbeitsSektion, Filename, '');
              end;

              tsHostsPatch:
              begin
                logdatei.log('Execution of: ' + ArbeitsSektion.Name +
                  ' ' + Remaining, LLNotice);
                GetWordOrStringExpressionstr(Remaining, Filename,
                  Remaining, ErrorInfo);
                ActionResult := doHostsPatch(ArbeitsSektion, Filename);
              end;



              tsRegistryHack:
              begin
                ActionResult := parseAndCallRegistry(ArbeitsSektion, Remaining);
              end;


              tsXMLPatch:
              begin
                  {$IFDEF WINDOWS}
                logdatei.log('Execution of: ' + ArbeitsSektion.Name +
                  ' ' + Remaining, LLNotice);
                EvaluateString(Remaining, Remaining, Parameter, InfoSyntaxError);
                if Remaining = '' then
                  ActionResult := doXMLPatch(ArbeitsSektion, Parameter, output)
                else
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    ' end of line expected');
                  {$ELSE WINDOWS}
                logdatei.log(
                  'XMLPatch sections are not implemented for Linux right now',
                  LLWarning);
                  {$ENDIF WINDOWS}
              end;

              tsXML2:
              begin
                EvaluateString(Remaining, Remaining, Parameter, InfoSyntaxError);
                if Remaining = '' then
                  ActionResult := doXMLPatch2(ArbeitsSektion, Parameter, output)
                else
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    ' end of line expected');
              end;


              tsOpsiServiceCall, tsOpsiServiceCallStat:
              begin
                logdatei.log('Execution of: ' + ArbeitsSektion.Name +
                  ' ' + Remaining, LLNotice);
                Parameter := Remaining;
                if (uppercase(PStatNames^ [tsOpsiServiceCall]) =
                  uppercase(Expressionstr)) and
                  skip('/preloginservice', Remaining, Remaining, errorInfo) then
                begin
                  logdatei.log(
                    'Execution of OpsiServiceCall /preloginservce', LLNotice);
                  if (local_opsidata <> nil) and (local_opsidata <> opsidata) then
                  begin
                    local_opsidata.Free;
                    local_opsidata := opsidata;
                  end;
                end
                else
                  ActionResult := doOpsiServiceCall(ArbeitsSektion, Parameter, output);
              end;

              tsOpsiServiceHashList:
              begin
                logdatei.log('Execution of: ' + ArbeitsSektion.Name +
                  ' ' + Remaining, LLNotice);
                Parameter := Remaining;
                ActionResult :=
                  doOpsiServiceHashList(ArbeitsSektion, Parameter, output);
              end;

              {$IFDEF WINDOWS}
              tsIdapiConfig:
              begin
                GetWordOrStringExpressionstr(Remaining, Filename,
                  Remaining, ErrorInfo);
                ActionResult := doIdapiConfig(ArbeitsSektion, Filename);
                //ActionResult := doIdapiConfig (ArbeitsSektion, Remaining);
              end;
              {$ENDIF WINDOWS}

              tsLDAPsearch:
              begin
                logdatei.log('Execution of: ' + ArbeitsSektion.Name +
                  ' ' + Remaining, LLNotice);
                EvaluateString(Remaining, Remaining, Parameter, InfoSyntaxError);
                if produceLDAPsearchParameters(Remaining, cacheRequest,
                  outputRequest, InfoSyntaxError) then
                  ActionResult :=
                    doLDAPSearch(ArbeitsSektion, cacheRequest, outputRequest, output)
                else
                  ActionResult :=
                    reportError(Sektion, linecounter, Sektion.strings[linecounter - 1],
                    InfoSyntaxError);
              end;

              tsFileActions:
              begin
                logdatei.log('Execution of: ' + ArbeitsSektion.Name +
                  ' ' + Remaining, LLNotice);
                if ArbeitsSektion.Count > 0 then
                  for tmpint := 0 to ArbeitsSektion.Count - 1 do
                    logdatei.log(ArbeitsSektion.Strings[tmpint], LLDebug2);
                flag_all_ntuser := False;
                // if this is a 'ProfileActions' which is called as sub in Machine mode
                // so run registry sections implicit as /Allntuserdats
                if runProfileActions then
                  flag_all_ntuser := True;
                ActionResult := doFileActions(ArbeitsSektion, Remaining);
                logdatei.log('Finished of: ' + ArbeitsSektion.Name +
                  ' ' + Remaining, LLNotice);
              end;

              tsLinkFolder:
              begin
                //{$IFDEF WIN32}
                logdatei.log('Execution of: ' + ArbeitsSektion.Name +
                  ' ' + Remaining, LLNotice);
                if Remaining = '' then
                  doLinkFolderActions(ArbeitsSektion, True)
                else
                begin
                  GetWord(remaining, Expressionstr, remaining, WorddelimiterSet0);
                  if not (lowercase(Expressionstr) =
                    lowercase(Parameter_AllNTUser)) then
                    ActionResult :=
                      reportError(Sektion, linecounter,
                      Sektion.strings[linecounter - 1], 'No valid parameter')
                  else
                    doLinkFolderActions(ArbeitsSektion, False);
                end;
                //{$ELSE WIN32}
                //logdatei.log('Linkfolder sections are not implemented for Linux right now', LLWarning);
                //{$ENDIF WIN32}
              end;


              tsWinBatch:
              begin
                ActionResult := parseAndCallWinbatch(ArbeitsSektion, Remaining);
                //parseAndCallWinbatch(ArbeitsSektion,Remaining);
              end;

              {$IFDEF WIN32}
              tsStayWhileWindowOpen:
              begin
                SyntaxCheck := False;
                if EvaluateString(Remaining, Remaining, ident, InfoSyntaxError)
                then
                  if Remaining = '' then
                    SyntaxCheck := True
                  else
                    InfoSyntaxError := 'unexpected chars after "';
                if SyntaxCheck then
                  while FindWindowEx(0, 0, nil, PChar(ident)) <> 0 do
                    ProcessMess
                else
                  ActionResult :=
                    reportError(Sektion, linecounter, Expressionstr, InfoSyntaxError);
              end;
               {$ENDIF WIN32}

              tsDosBatchFile:
              begin
                logdatei.log('Execution of: ' + ArbeitsSektion.Name +
                  ' ' + Remaining, LLNotice);
                ActionResult :=
                  execDOSBatch(ArbeitsSektion, Remaining, SW_ShowNormal,
                  False {dont catch out}, 0, [ttpWaitOnTerminate], output);
              end;

              tsDosInAnIcon:
              begin
                logdatei.log('Execution of: ' + ArbeitsSektion.Name +
                  ' ' + Remaining, LLNotice);
                ActionResult :=
                  execDOSBatch(ArbeitsSektion, Remaining, SW_HIDE,
                  True {catch out}, 0, [ttpWaitOnTerminate], output);
              end;

              tsShellBatchFile:
              begin
                logdatei.log('Execution of: ' + ArbeitsSektion.Name +
                  ' ' + Remaining, LLNotice);
                ActionResult :=
                  execDOSBatch(ArbeitsSektion, Remaining, SW_ShowNormal,
                  False {dont catch out}, 0, [ttpWaitOnTerminate], output);
              end;

              tsShellInAnIcon:
              begin
                logdatei.log('Execution of: ' + ArbeitsSektion.Name +
                  ' ' + Remaining, LLNotice);
                ActionResult :=
                  execDOSBatch(ArbeitsSektion, Remaining, SW_HIDE,
                  True {catch out}, 0, [ttpWaitOnTerminate], output);
              end;

              tsExecutePython:
              begin
                logdatei.log('Execution of: ' + ArbeitsSektion.Name +
                  ' ' + Remaining, LLNotice);
                ActionResult :=
                  execPython(ArbeitsSektion, Remaining, True {catch out},
                  0, [ttpWaitOnTerminate], output);
              end;

              tsExecuteWith, tsExecuteWith_escapingStrings:
              begin
                logdatei.log('Execution of: ' + ArbeitsSektion.Name +
                  ' ' + Remaining, LLNotice);
                if produceExecLine(remaining, p1, p2,
                  p3, p4, InfoSyntaxError) then
                  ActionResult :=
                    executeWith(ArbeitsSektion, Remaining, True {catch out},
                    0, output)
                else
                  ActionResult :=
                    reportError(Sektion, linecounter, Expressionstr, InfoSyntaxError);
              end;

              tsWorkOnStringList:
              begin
                ActionResult :=
                  reportError(Sektion, linecounter, Expressionstr,
                  'not yet implemented');
              end;

              tsDDEwithProgman:
                //ActionResult := doDDEwithProgman (ArbeitsSektion, Remaining, SaveddeWithProgman);
                ActionResult := doDDEwithProgman(ArbeitsSektion, Remaining, False);

              {$IFDEF WIN32}
              tsAddConnection:
              begin
                syntaxCheck := True;
                GetWord(Remaining, localname, Remaining, WordDelimiterWhiteSpace);
                GetWord(Remaining, remotename, Remaining, WordDelimiterWhiteSpace);
                (* GetWord (Remaining, username, Remaining, WordDelimiterWhiteSpace); *)
                GetWord(Remaining, trytime, Remaining, WordDelimiterWhiteSpace);

                try
                  timeout := StrToInt(trytime);
                except
                  syntaxCheck := False;
                  InfoSyntaxError := trytime + ' is not an integer value';
                end;

                if syntaxCheck then
                  ActionResult := startConnection(localname, remotename, timeout)
                else
                  ActionResult :=
                    reportError(Sektion, linecounter, Expressionstr, InfoSyntaxError);
              end;
               {$ENDIF WIN32}

              tsDefineVar:
              begin
                call := Remaining;
                GetWord(Remaining, Expressionstr, Remaining, WordDelimiterSet1);
                if Remaining <> '' then
                  reportError(Sektion, linecounter, Expressionstr,
                    'not allowed char following variable name')
                else if findKindOfStatement(Expressionstr, SectionSpecifier, call) <>
                  tsNotDefined then
                  reportError(Sektion, linecounter, Expressionstr,
                    'Reserved name, must not be used in a variable definition')
                // in local function ?
                else if inDefinedFuncNestCounter > 0 then
                begin
                  // get the function we are in
                  funcindex :=
                    StrToInt(definedFunctionsCallStack.Strings[
                    definedFunctionsCallStack.Count - 1]);
                  if definedFunctionArray[funcindex].addLocalVar(
                    lowercase(Expressionstr), dfpString, False) then
                    LogDatei.log('Defined local string var: ' +
                      lowercase(Expressionstr) + ' in local function: ' +
                      definedFunctionArray[funcindex].Name, LLDebug2)
                  else
                    reportError(Sektion, linecounter, Expressionstr,
                      'name is already in use');
                end
                // not in local function - make it global
                else if VarList.IndexOf(lowercase(Expressionstr)) >= 0 then
                  reportError(Sektion, linecounter, Expressionstr,
                    'name is already in use')
                else
                begin
                  VarList.Add(lowercase(Expressionstr));
                  ValuesList.Add('');
                end;
              end;

              tsDefineStringList:
              begin
                call := Remaining;
                GetWord(Remaining, Expressionstr, Remaining, WordDelimiterSet1);
                if Remaining <> '' then
                  reportError(Sektion, linecounter, Expressionstr,
                    'char not allowed following variable name')
                else if findKindOfStatement(Expressionstr, SectionSpecifier, call) <>
                  tsNotDefined then
                  reportError(Sektion, linecounter, Expressionstr,
                    'Reserved name, must not be used in a variable definition')
                // in local function ?
                else if inDefinedFuncNestCounter > 0 then
                begin
                  // get the function we are in
                  funcindex :=
                    StrToInt(definedFunctionsCallStack.Strings[
                    definedFunctionsCallStack.Count - 1]);
                  if definedFunctionArray[funcindex].addLocalVar(
                    lowercase(Expressionstr), dfpStringlist, False) then
                    LogDatei.log('Defined local stringlist var: ' +
                      lowercase(Expressionstr) + ' in local function: ' +
                      definedFunctionArray[funcindex].Name, LLDebug2)
                  else
                    reportError(Sektion, linecounter, Expressionstr,
                      'name is already in use');
                end
                // not in local function - make it global

                else if VarList.IndexOf(lowercase(Expressionstr)) or
                  listOfStringLists.IndexOf(lowercase(Expressionstr)) >= 0 then
                  reportError(Sektion, linecounter, Expressionstr,
                    'Name already in use')
                else

                begin
                  listOfStringLists.Add(lowercase(Expressionstr));
                  ContentOfStringLists.Add(TStringList.Create);
                  LogDatei.log('', leveldebug);
                  LogDatei.log('(created string list ' + Expressionstr +
                    ')', leveldebug);
                end;
              end;

              tsDefineFunction:
              begin
                call := Remaining;
                GetWord(Remaining, Expressionstr, Remaining, WordDelimiterSet5);
                FuncIndex := definedFunctionNames.IndexOf(LowerCase(Expressionstr));
                if FuncIndex >= 0 then
                begin
                  LogDatei.log('tsDefineFunction: Passing well known localfunction: ' +
                    Expressionstr, LLInfo);
                  try
                    // get all lines until 'endfunction'
                    //endofDefFuncFound := false;
                    inDefFunc := 1;
                    repeat
                      // get next line of section
                      Inc(linecounter); // inc line counter
                      Inc(FaktScriptLineNumber);
                      // inc line counter that ignores the execution of defined functions
                      if (linecounter <= Sektion.Count - 1) then
                      begin
                        Remaining := trim(Sektion.strings[linecounter - 1]);
                        myline := remaining;
                        GetWord(Remaining, Expressionstr, Remaining,
                          WordDelimiterSet4);
                        StatKind :=
                          FindKindOfStatement(Expressionstr, SectionSpecifier, call);
                        if StatKind = tsDefineFunction then
                          Inc(inDefFunc);
                        if StatKind = tsEndFunction then
                          Dec(inDefFunc);
                      end;
                    until (inDefFunc <= 0) or (linecounter >= Sektion.Count - 2);
                  except
                    on e: Exception do
                    begin
                      LogDatei.log(
                        'Exception in doAktionen: tsDefineFunction: endfunction: ' +
                        e.message, LLError);
                      //raise e;
                    end;
                  end;
                  Inc(linecounter); // inc line counter
                  Inc(FaktScriptLineNumber);
                  // inc line counter that ignores the execution of defined functions
                  LogDatei.log('tsDefineFunction: passed well known localfunction: ' +
                    Expressionstr, LLInfo);
                  Dec(inDefFunc3);
                end
                else
                begin
                  Remaining := call;
                  try
                    newDefinedfunction := TOsDefinedFunction.Create;
                    BooleanResult :=
                      newDefinedfunction.parseDefinition(Remaining, ErrorInfo);
                    if not BooleanResult then
                    begin
                      reportError(Sektion, linecounter, Expressionstr, ErrorInfo);
                    end
                    else
                    begin
                      try
                        s1 := newDefinedfunction.Name;
                        tmpint := script.FSectionNameList.IndexOf(s1);
                        if (tmpint >= 0) and
                          (tmpint <= length(Script.FSectionInfoArray)) then
                        begin
                          newDefinedfunction.OriginFile :=
                            Script.FSectionInfoArray[tmpint].SectionFile;
                          newDefinedfunction.OriginFileStartLineNumber :=
                            Script.FSectionInfoArray[tmpint].StartLineNo;
                        end
                        else
                          logdatei.log('Warning: Origin of function: ' +
                            s1 + ' not found.', LLwarning);
                         (*
                         tmplist := TXStringlist.Create;
                         if FLinesOriginList.Count < script.aktScriptLineNumber then
                         begin
                           s1 := FLinesOriginList.Strings[FLinesOriginList.Count-1];
                           LogDatei.log('Error in doAktionen: tsDefineFunction: ' +
                              ' OriginList: '+inttostr(FLinesOriginList.Count)+
                              ' aktScriptLineNumber: '+inttostr(script.aktScriptLineNumber), LLError);
                         end
                         else
                           s1 := FLinesOriginList.Strings[script.aktScriptLineNumber-1];
                         stringsplitByWhiteSpace(s1,tmplist);
                         //newDefinedfunction.OriginFile := ExtractFileName(tmplist[0]);
                         newDefinedfunction.OriginFile := tmplist[0];
                         try
                           if tmplist[0] = script.FFilename then
                             // not imported : add section header
                             newDefinedfunction.OriginFileStartLineNumber:=strtoint(tmplist[2])+sektion.StartLineNo-1
                           else
                             // imported : no section header
                             newDefinedfunction.OriginFileStartLineNumber:=strtoint(tmplist[2]);
                         finally
                           tmplist.Free;
                         end;
                         *)
                      except
                        on e: Exception do
                        begin
                          LogDatei.log(
                            'Exception in doAktionen: tsDefineFunction: tmplist: ' +
                            e.message, LLError);
                          //raise e;
                        end;
                      end;
                      try
                        // get all lines until 'endfunction'
                        //endofDefFuncFound := false;
                        inDefFunc := 1;
                        numberOfSectionLines := Sektion.Count;
                        repeat
                          // get next line of section
                          Inc(linecounter); // inc line counter
                          Inc(FaktScriptLineNumber);
                          // inc line counter that ignores the execution of defined functions
                          if (linecounter <= numberOfSectionLines - 1) then
                          begin
                            Remaining := trim(Sektion.strings[linecounter - 1]);
                            myline := remaining;
                            GetWord(Remaining, Expressionstr,
                              Remaining, WordDelimiterSet4);
                            StatKind :=
                              FindKindOfStatement(Expressionstr, SectionSpecifier, call);
                            if StatKind = tsDefineFunction then
                              Inc(inDefFunc);
                            if StatKind = tsEndFunction then
                              Dec(inDefFunc);
                            // Line with tsEndFunction should not be part of the content
                            if inDefFunc > 0 then
                            begin
                              newDefinedfunction.addContent(myline);
                              LogDatei.log_prog(
                                'inDefFunc: ' + IntToStr(inDefFunc) +
                                ' add line: ' + myline, LLDebug3);
                            end;
                          end;
                        until (inDefFunc <= 0) or
                          (linecounter >= numberOfSectionLines - 2);
                      except
                        on e: Exception do
                        begin
                          LogDatei.log(
                            'Exception in doAktionen: tsDefineFunction: endfunction: ' +
                            e.message, LLError);
                          //raise e;
                        end;
                      end;
                      try
                        if inDefFunc > 0 then
                        begin
                          LogDatei.log('Found DefFunc without EndFunc', LLCritical);
                          reportError(Sektion, linecounter,
                            Expressionstr, 'Found DefFunc without EndFunc');
                        end
                        else
                        begin
                          // endfunction found
                          // append new defined function to the stored (known) functions
                          Inc(definedFunctioncounter);
                          newDefinedfunction.Index := definedFunctioncounter - 1;
                          SetLength(definedFunctionArray, definedFunctioncounter);
                          definedFunctionArray[definedFunctioncounter - 1] :=
                            newDefinedfunction;
                          definedFunctionNames.Append(newDefinedfunction.Name);
                          Dec(inDefFunc3);
                          LogDatei.log('Added defined function: ' +
                            newDefinedfunction.Name + ' to the known functions', LLInfo);
                          logdatei.log_prog(
                            'After adding a defined function: inDefFunc3: ' +
                            IntToStr(inDefFunc3), LLDebug3);
                        end;
                      except
                        on e: Exception do
                        begin
                          LogDatei.log(
                            'Exception in doAktionen: tsDefineFunction: append: ' +
                            e.message, LLError);
                          //raise e;
                        end;
                      end;
                    end;
                    LogDatei.log_prog('After reading ' + newDefinedfunction.Name +
                      ' we are on line: ' + IntToStr(linecounter - 1) +
                      ' -> ' + trim(Sektion.strings[linecounter - 1]), LLInfo);
                  except
                    on e: Exception do
                    begin
                      LogDatei.log('Exception in doAktionen: tsDefineFunction: ' +
                        e.message, LLError);
                      //raise e;
                    end;
                  end;
                end;
              end;

              tsEndFunction:
              begin
                // nothing to do
              end;


              tsSetVar:
              begin
                LogDatei.log_prog('Start tsSetVar with expr: ' + Remaining, LLdebug);
                //writeln('set');
                Expressionstr := Remaining;
                doLogEntries(PStatNames^ [tsSetVar] + '  ' + Expressionstr, LLInfo);
                if doSetVar(sektion, Expressionstr, Remaining,
                  InfoSyntaxError, NestLevel) then
                begin
                  if Remaining <> '' then
                    reportError(Sektion, linecounter, Remaining,
                      'Remaining char(s) not allowed here');
                end
                else

                  reportError(Sektion, linecounter, Expressionstr, InfoSyntaxError);
              end

              else
                ActionResult :=
                  reportError(Sektion, linecounter, Expressionstr, 'undefined');

            end (* case *);
          end;
        end
        else
          processline := False;
        ProcessMess;
      end;


      if inloop then
      begin
        if looplist.Count = 0 //loop will finish
        then
        begin
          inloop := False;
          // clearing the loop variable from the list of variables, first the value
          if not delLoopvarFromVarList(loopvar, InfoSyntaxError) then
            syntaxCheck := False
          else
            syntaxCheck := True;
          LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
          LogDatei.log('', LLinfo);
          LogDatei.log('~~~~~~ End Loop', LLinfo);

        end;

      end;

    until not inloop;
    LogDatei.log_prog('Finished with linenr: ' + IntToStr(linecounter) +
      ' -> ' + trim(Sektion.strings[linecounter - 1]), LLinfo);
    logdatei.log_prog('Actlevel: ' + IntToStr(Actlevel) + ' NestLevel: ' +
      IntToStr(NestLevel) + ' ArbeitsSektion.NestingLevel: ' + IntToStr(
      ArbeitsSektion.NestingLevel) + ' Sektion.NestingLevel: ' +
      IntToStr(Sektion.NestingLevel), LLDebug2);
    Inc(linecounter);
  end;

  if not scriptstopped then
  begin
    if (ActLevel > NestLevel) and (actionresult > tsrFatalError) then
      reportError(Sektion, linecounter, PStatNames^ [tsCondClose], 'expected');

    if (NestLevel > Sektion.NestingLevel) and (actionresult > tsrFatalError) then
      reportError(Sektion, linecounter, PStatNames^ [tsCondClose], 'expected');
  end;


  if ActionResult <= tsrFatalError then
    doLogEntries(AbortedInfo, BaseLevel)
  else if ActionResult <= tsrExitProcess then
    doLogEntries(StopInfo, BaseLevel);

  ArbeitsSektion.Free;

  LogDatei.LogSIndentLevel := StartIndentLevel;

  Result := ActionResult;
  output.Free;
  // restore last section
  Script.ActiveSection := Script.FLastSection;
end;


procedure TuibInstScript.registerSectionOrigins(mylist: TStringList; filename: string);
begin
  registerSectionOrigins(mylist, filename, '');
end;

procedure TuibInstScript.registerSectionOrigins(mylist: TStringList;
  filename: string; secname: string);
var
  i, secindex, tmpint: integer;
  str: string;
begin
  for i := 0 to mylist.Count - 1 do
  begin
    str := trim(mylist.Strings[i]);
    if pos('sub "', secname) = 1 then
    begin
      // we have a new external sub section
      secindex := Script.FSectionNameList.Add(secname);
      tmpint := length(script.FSectionInfoArray);
      if secindex <> tmpint then
        LogDatei.log('Error: internal: secindex <> length(script.FSectionInfoArray)',
          LLCritical);
      setlength(script.FSectionInfoArray, secindex + 1);
      script.FSectionInfoArray[secindex].SectionName := secname;
      script.FSectionInfoArray[secindex].SectionFile := ExtractFileName(filename);
      script.FSectionInfoArray[secindex].StartLineNo := i;
    end
    else
    begin
      secname := opsiunquotestr2(str, '[]');
      if secname <> str then
      begin
        // we have a new real section
        secindex := Script.FSectionNameList.Add(secname);
        tmpint := length(script.FSectionInfoArray);
        if secindex <> tmpint then
          LogDatei.log('Error: internal: secindex <> length(script.FSectionInfoArray)',
            LLCritical);
        setlength(script.FSectionInfoArray, secindex + 1);
        script.FSectionInfoArray[secindex].SectionName := secname;
        script.FSectionInfoArray[secindex].SectionFile := ExtractFileName(filename);
        script.FSectionInfoArray[secindex].StartLineNo := i;
      end;
    end;
    if pos('deffunc', lowercase(str)) = 1 then
    begin
      // we have a new function
      secname := trim(Copy(str, length('deffunc') + 1, length(str)));
      GetWord(secname, secname, remaining, WordDelimiterSet5);
      secindex := Script.FSectionNameList.Add(secname);
      tmpint := length(script.FSectionInfoArray);
      if secindex <> tmpint then
        LogDatei.log('Error: internal: secindex <> length(script.FSectionInfoArray)',
          LLCritical);
      setlength(script.FSectionInfoArray, secindex + 1);
      script.FSectionInfoArray[secindex].SectionName := secname;
      script.FSectionInfoArray[secindex].SectionFile := ExtractFileName(filename);
      script.FSectionInfoArray[secindex].StartLineNo := i;
    end;
  end;
end;


procedure CreateAndProcessScript(const Scriptdatei: string;
  NestingLevel: integer; SaveddeWithProgman: boolean;
  var extremeErrorLevel: TErrorLevel);

var
  StartlineOfSection: integer = 0;
  Aktionsliste: TWorkSection;
  //Script : TuibInstScript;
  nrErrors: integer = 0;
  i: integer = 0;
  nrWarnings: integer = 0;
  oldNumberOfErrors: integer = 0;
  oldNumberOfWarnings: integer = 0;
  weiter: integer = 0;
  ValueToTake: string = '';
  s: string = '';
  ErrorInfo: string = '';
  saveIndent: integer = 0;
  hostnaming: string = '';
  computernaming: string = '';
  ipName: string = '';
  ipAddress: string = '';
  usedEncoding: string = '';
  Encoding2use: string = '';
  tmpstr: string = '';
  str: string;
  depotdrive_bak, depotdir_bak: string;
  {$IFDEF WINDOWS}
  Regist: TuibRegistry;
  {$ENDIF WINDOWS}

begin
  try
    // reset Local_opsidata after product
    if (local_opsidata <> nil) and (local_opsidata <> opsidata) then
    begin
      local_opsidata.Free;
      //local_opsidata := nil;
      local_opsidata := opsidata;
    end;
    //Script := TuibInstScript.Create;
    //// Backup existing depotdrive, depotdir
    //depotdrive_bak := depotdrive;
    //depotdir_bak :=  depotdir;


    if not readconfig_done then
    begin
      if not readconfig then
      begin
        logdatei.log('readconfig failed: Perhaps got defaults ', LLError);
        LogDatei.log('Depot path from readconfig:  ' + depotdrive + depotdir, LLinfo);
        LogDatei.log('Restoring depot path from backup', LLinfo);
        // restoring old values
        depotdrive := depotdrive_bak;
        depotdir := depotdir_bak;
      end;
    end;
    LogDatei.force_min_loglevel := osconf.force_min_loglevel;
    LogDatei.debug_prog := osconf.debug_prog;
    LogDatei.LogLevel := osconf.default_loglevel;
    LogDatei.debug_lib := osconf.debug_lib;
    logDatei.log_prog('force_min_loglevel: ' + IntToStr(osconf.force_min_loglevel),
      LLinfo);
    logDatei.log_prog('default_loglevel: ' + IntToStr(osconf.default_loglevel), LLinfo);
    logDatei.log_prog('debug_prog: ' + BoolToStr(osconf.debug_prog, True), LLinfo);
    logDatei.log_prog('debug_lib: ' + booltostr(osconf.debug_lib, True), LLinfo);
    logDatei.log_prog('ScriptErrorMessages: ' + BoolToStr(
      osconf.ScriptErrorMessages, True), LLinfo);
    logDatei.log_prog('AutoActivityDisplay: ' + booltostr(
      osconf.AutoActivityDisplay, True), LLinfo);
    LogDatei.log('Using new Depot path:  ' + depotdrive + depotdir, LLinfo);

  {$IFDEF DARWIN}
    if not checkForMacosDependencies(tmpstr) then
    begin
      logdatei.log('Could not get the Dependencies for macos ', LLError);
      logdatei.log(tmpstr, LLError);
    end;
  {$ENDIF DARWIN}

    // init vars
    inDefFuncLevel := 0;
    inDefFuncIndex := -1;
    Ifelseendiflevel := 0;
    inDefinedFuncNestCounter := 0;
    definedFunctioncounter := 0;
    inDefFunc3 := 0;


    Script := TuibInstScript.Create;
    script.aktScriptLineNumber := 0;
    // Backup existing depotdrive, depotdir
    depotdrive_bak := depotdrive;
    depotdir_bak := depotdir;
  {$IFDEF UNIX}
    computername := getHostnameLin;
    logdatei.log('computername: ' + computername, LLDebug);
  {$ENDIF LINUX}
  {$IFDEF GUI}
    CentralForm.Label1.Caption := '';
    FBatchOberflaeche.setInfoLabel('');

    CentralForm.Label2.Caption := '';
    FBatchOberflaeche.setDetailLabel(CentralForm.Label2.Caption);
  {$ENDIF GUI}
  {$IFDEF UNIX}
    lispecfolder.retrieveFolders4Linux;
  {$ENDIF LINUX}
    if Scriptdatei <> '' then
    begin
      //s := 'Drives found: ' + GetDrives;
      if not CheckFileExists(Scriptdatei, ErrorInfo) then
      begin
        LogDatei.log('Script  ' + Scriptdatei + '  not found ' +
          ErrorInfo + ' - retrying', LLWarning);
        Sleep(1000);
        if not CheckFileExists(Scriptdatei, ErrorInfo) then
        begin
          LogDatei.log('Script  ' + Scriptdatei + '  not found ' +
            ErrorInfo + ' - retrying', LLWarning);
        {$IFDEF LINUX}
          if ProgramMode = pmBuildPC_service then
          begin
            logdatei.log('check opsi depot mount', LLDebug);
            if not isMounted(depotdir) then
            begin
              logdatei.log('Try remount ...', LLWarning);
              mount_depotshare(depotDir, opsiservicePassword, osconf.computername);
            end;
          end;
        {$ENDIF LINUX}
          Sleep(1000);
          if not CheckFileExists(Scriptdatei, ErrorInfo) then
          begin
            LogDatei.log('Script  ' + Scriptdatei + '  not found ' +
              ErrorInfo + ' - giving up', LLError);
            ExtremeErrorLevel := LevelFatal;
            LogDatei.log('Error level set to fatal', LLCritical);
            exit;
          end;
        end;
      end;
      //Scriptdatei := ExpandFileName(Scriptdatei);
      Script.LoadFromFile(Scriptdatei);
      Encoding2use := searchencoding(Script.Text);
      if Encoding2use = '' then
        Encoding2use := 'system';
      Script.Text := reencode(Script.Text, Encoding2use, usedEncoding);
      Script.FFilename := Scriptdatei;
      for i := 0 to script.Count - 1 do
      begin
        str := Script.Strings[i];
        script.FLinesOriginList.Append(script.FFilename + ' line: ' + IntToStr(i + 1));
        script.FLibList.Append('false');
        //writeln('i='+inttostr(i)+' = '+Script.FLinesOriginList.Strings[i-1]);
      end;
      Script.registerSectionOrigins(TStringList(Script), Scriptdatei);
    end
    else
    begin
    {$IFDEF GUI}
      // Run Viewlist as script
      Script.Assign(CentralForm.Memo1.Lines);
      for i := 1 to script.Count do
      begin
        script.FLinesOriginList.Append('Viewlist line: ' + IntToStr(i));
        script.FLibList.Append('false');
        //writeln('i='+inttostr(i)+' = '+Script.FLinesOriginList.Strings[i-1]);
        //writeln('i='+inttostr(i));
      end;
    {$ENDIF GUI}
    end;


    LogDatei.TraceMode := Script.TraceMode;
    //LogDatei.LogLevel := Script.LogLevel;

    LogDatei.log('', LLessential);


    ps := '============ ' + OpsiscriptVersionName + ' script "' + Scriptdatei + '"';
    LogDatei.log(ps, LLessential);
    ps := '             used script encoding: ' + usedEncoding;
    LogDatei.log(ps, LLessential);
    ps := '             used system encoding: ' + mysystemEncoding;
    LogDatei.log(ps, LLessential);

    ps := FormatDateTime('yyyy-mm-dd  hh:mm:ss ', now);

    computernaming := osconf.computername;

    if computernaming = '' then
      computernaming := oslog.getComputerName;


    ps := '             start: ' + ps;
    LogDatei.log(ps, LLessential);

    if opsidata <> nil then
    begin
      try
        try
          ps := '             installing product: ';
          ps := ps + Topsi4data(opsidata).getActualProductId;
          ps := ps + '_' + opsidata.getActualProductVersion;
        except
          ps := ps + '_' + ' Unknown'
        end;
      finally
        LogDatei.log(ps, LLessential);
      end;
    end;

    ps := '             on client named    "' + computernaming + '"';
    LogDatei.log(ps, LLessential);

    ps := '             loggedin user    "' + getLoggedInUser + '"';
    LogDatei.log(ps, LLessential);

    GetNetUser('', ps, ErrorInfo);
    ps := '             opsi-script running as    "' + ps + '"';
    LogDatei.log(ps, LLessential);

    if runningAsAdmin then
      LogDatei.log('             opsi-script running with admin privileges', LLessential)
    else
      LogDatei.log('             opsi-script not running with admin privileges',
        LLWarning);

    if runloginscripts then
    begin
      LogDatei.log('             opsi-script running in loginscript mode', LLessential);
      LogDatei.log('             using context: ' + usercontext +
        ' with SID ' + usercontextsid, LLessential);
    end
    else
    if runproductlist then
      LogDatei.log('             opsi-script running in productlist script mode',
        LLessential)
    else if runprocessproducts then
      LogDatei.log('             opsi-script running in processproducts script mode',
        LLessential)
    else
      LogDatei.log('             opsi-script running in standard script mode',
        LLessential);


    ps := 'executing: "' + reencode(ParamStr(0), 'system') + '"';
    LogDatei.log(ps, LLessential);

    //LogDatei.log ('PC MAC address, method1 ' + getMACAddress1, BaseLevel);

    LogDatei.log('system infos:', LLessential);

    FindLocalIPData(ipName, ipAddress);
    LogDatei.log(GetMACAddress2 + '  -  PC hardware address', LLessential);
    LogDatei.log(ipName + '  -  IP name ', LLessential);
    LogDatei.log(ipAddress + '  -  IP address', LLessential);
  {$IFDEF WINDOWS}
    LogDatei.log(GetSystemDefaultLocale(LOCALE_SABBREVLANGNAME) +
      '  -  System default locale ',
      LLessential);
    tmpstr := 'MS Windows ' + GetSystemOSVersionInfoEx('major_version') +
      '.' + GetSystemOSVersionInfoEx('minor_version');
    if Is64BitSystem then
      tmpstr := tmpstr + ' 64 Bit'
    else
      tmpstr := tmpstr + ' 32 Bit';
    { we have no ReleaseId before Win10  }
    if (StrToInt(GetSystemOSVersionInfoEx('major_version')) >= 10) and
      RegVarExists('HKLM\SOFTWARE\Microsoft\Windows NT\CurrentVersion',
      'ReleaseID', True) then
    begin
      tmpstr := tmpstr + ', Release: ' + GetRegistrystringvalue(
        'HKLM\SOFTWARE\Microsoft\Windows NT\CurrentVersion', 'ReleaseID', True);
    end;
    tmpstr := tmpstr + ', Edition: ' + getProductInfoStrByNum(OSGetProductInfoNum);
    LogDatei.log(tmpstr, LLessential);
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
    tmpstr := getLinuxDistroName + ' ' + getLinuxDistroDescription;
    if Is64BitSystem then
      tmpstr := tmpstr + ' 64 Bit'
    else
      tmpstr := tmpstr + ' 32 Bit';
    LogDatei.log(tmpstr, LLessential);
  {$ENDIF LINUX}
  {$IFDEF DARWIN}
    tmpstr := 'macOS: ' + GetMacosVersionInfo;
    LogDatei.log(tmpstr, LLessential);
  {$ENDIF DARWIN}

    if opsidata <> nil then
    begin
      LogDatei.log('opsi service version : ' + opsidata.getOpsiServiceVersion,
        LLessential);
      //Logdatei.log('Setup script name: '+opsidata.getProductScriptPath(tacSetup), LLessential);
    end;

    { backup the start working dir  }
    opsiWinstStartdir := GetCurrentDir;

    LogDatei.log('', LLessential);


    { Definition of global system variables }
    LogDatei.log_prog('Start: Definition of global system variables', LLinfo);
    with Script do
    begin
      { Attention: every key need an value entry !
        If value is missing AppyTextConstants will get an index error. }

      {  System directories:  }
    {$IFDEF WINDOWS}
      FConstList.add('%Systemroot%');
      ValueToTake := GetWinDirectory;
      { delete closing back slash }
      System.Delete(ValueToTake, length(ValueToTake), 1);
      FConstValuesList.add(ValueToTake);

      FConstList.add('%System%');
      ValueToTake := GetWinSystemDirectory;
      { delete closing back slash }
      System.Delete(ValueToTake, length(ValueToTake), 1);
      FConstValuesList.add(ValueToTake);

      FConstList.add('%SystemDrive%');
      ValueToTake := extractfiledrive(GetWinDirectory);
      FConstValuesList.add(ValueToTake);
    {$ENDIF WINDOWS}
      FConstList.add('%ProfileDir%');
      ValueToTake := getProfilesPath;
      FConstValuesList.add(ValueToTake);

      FConstList.add('%DefaultUserProfileDir%');
      ValueToTake := getDefaultUsersProfilesPath;
      FConstValuesList.add(ValueToTake);


      saveIndent := Logdatei.LogSIndentLevel;
    {$IFDEF WINDOWS}
      ValueToTake := getSpecialFolder(CSIDL_PROGRAM_FILES);
      LogDatei.LogSIndentLevel := saveIndent;

      FConstList.add('%ProgramFilesDir%');
      FConstValuesList.add(ValueToTake);
      FConstList.add('%ProgramFiles32Dir%');
      FConstValuesList.add(ValueToTake);
      try
        if (GetNTVersionMajor = 5) and (GetNTVersionMinor = 0) then
        begin
          { we are on win 2000 which can't handle redirections flags  }
          Regist := Tuibregistry.Create;
        end
        else { we are on xp or higher  }
        begin
          Regist := Tuibregistry.Create(True, True);
        end;
        regist.OpenExistingKey('HKLM', 'SOFTWARE\Microsoft\Windows\CurrentVersion');
        ValueToTake := regist.ReadString('ProgramFilesDir');
        Regist.Free;
        Regist := nil;
      except
        ValueToTake := 'C:\Program Files'
      end;
      FConstList.add('%ProgramFiles64Dir%');
      FConstValuesList.add(ValueToTake);
      FConstList.add('%ProgramFilesSysnativeDir%');
      FConstValuesList.add(ValueToTake);



      {  Usercontext data   }

      FConstList.add('%Usercontext%');
      FConstValuesList.add(usercontext);

      FConstList.add('%UsercontextSID%');
      FConstValuesList.add(usercontextSID);
    {$ENDIF WINDOWS}

      {  Common (AllUsers) directories: }
      FConstList.add('%CommonAppDataDir%');
      ValueToTake := GetCommonAppDataPath;
      FConstValuesList.add(ValueToTake);

      FConstList.add('%AllUsersProfileDir%');
      ValueToTake := guessCommonProfilesPath;
      FConstValuesList.add(ValueToTake);
      FConstList.add('%CommonProfileDir%');
      FConstValuesList.add(ValueToTake);

      FConstList.add('%CommonStartmenuDir%');
      ValueToTake := GetCommonStartmenuPath;
      FConstValuesList.add(ValueToTake);
      FConstList.add('%CommonStartmenuPath%');
      FConstValuesList.add(ValueToTake);

      FConstList.add('%CommonDesktopDir%');
      ValueToTake := GetCommonDesktopPath;
      FConstValuesList.add(ValueToTake);

      FConstList.add('%CommonStartupDir%');
      ValueToTake := GetCommonStartupPath;
      FConstValuesList.add(ValueToTake);

      FConstList.add('%CommonProgramsDir%');
      ValueToTake := GetCommonProgramsPath;
      FConstValuesList.add(ValueToTake);

      FConstList.add('%AppDataDir%');
      ValueToTake := GetAppDataPath;
      FConstValuesList.add(ValueToTake);
      FConstList.add('%CurrentAppDataDir%');
      FConstValuesList.add(ValueToTake);

      FConstList.add('%CurrentStartmenuDir%');
      ValueToTake := GetStartmenuPath;
      FConstValuesList.add(ValueToTake);

      FConstList.add('%CurrentDesktopDir%');
      ValueToTake := GetDesktopPath;
      FConstValuesList.add(ValueToTake);

      FConstList.add('%CurrentStartupDir%');
      ValueToTake := GetStartupPath;
      FConstValuesList.add(ValueToTake);

      FConstList.add('%CurrentProgramsDir%');
      ValueToTake := GetProgramsPath;
      FConstValuesList.add(ValueToTake);

      FConstList.add('%CurrentSendToDir%');
      ValueToTake := GetSendToPath;
      FConstValuesList.add(ValueToTake);

      FConstList.add('%CurrentProfileDir%');
      ValueToTake := getUserProfilePath;
      FConstValuesList.add(ValueToTake);

      { /AllNtUserProfiles directory constants:
       they are not defined here and will be replaced
       at the working section code }


      { opsi-script-Path and Directories }
      FConstList.add('%ScriptDrive%');
      ValueToTake := extractfiledrive(ExpandFilename(Scriptdatei));
      FConstValuesList.add(ValueToTake);

      FConstList.add('%Scriptpath%');
      ValueToTake := ExtractFileDir(Scriptdatei);
      FConstValuesList.add(ValueToTake);
      FConstList.add('%ScriptDir%');
      FConstValuesList.add(ValueToTake);

      FConstList.add('%WinstDir%');
      ValueToTake := ExtractFileDir(reencode(ParamStr(0), 'system'));
      FConstValuesList.add(ValueToTake);
      FConstList.add('%OpsiscriptDir%');
      ValueToTake := ExtractFileDir(reencode(ParamStr(0), 'system'));
      FConstValuesList.add(ValueToTake);

      FConstList.add('%WinstVersion%');
      ValueToTake := osconf.OpsiscriptVersion;
      FConstValuesList.add(ValueToTake);
      FConstList.add('%OpsiscriptVersion%');
      ValueToTake := osconf.OpsiscriptVersion;
      FConstValuesList.add(ValueToTake);

      FConstList.add('%LogFile%');
      FConstValuesList.add(LogDatei.FileName);

      FConstList.add('%opsiTmpDir%');
    {$IFDEF WINDOWS}
      FConstValuesList.add('c:\opsi.org\tmp');
    {$ENDIF WINDOWS}
    {$IFDEF UNIX}
      FConstValuesList.add('/tmp');
    {$ENDIF UNIX}

      FConstList.add('%opsiLogDir%');
      FConstValuesList.add(copy(oslog.defaultStandardMainLogPath, 1,
        Length(oslog.defaultStandardMainLogPath) - 1));

      FConstList.add('%opsiapplog%');
    {$IFDEF WINDOWS}
      FConstValuesList.add('c:\opsi.org\applog');
    {$ENDIF WINDOWS}
    {$IFDEF UNIX}
      FConstValuesList.add('~/opsi.org/applog');
    {$ENDIF UNIX}

      FConstList.add('%opsidata%');
    {$IFDEF WINDOWS}
      FConstValuesList.add('c:\opsi.org\data');
    {$ENDIF WINDOWS}
    {$IFDEF UNIX}
      FConstValuesList.add('/var/lib/opsi-client-agent');
    {$ENDIF UNIX}

    {$IFDEF WINDOWS}
      FConstList.add('%opsiScriptHelperPath%');
      ValueToTake := getSpecialFolder(CSIDL_PROGRAM_FILES) +
        '\opsi.org\opsiScriptHelper';
      FConstValuesList.add(ValueToTake);



      { Network informations  }
      FConstList.add('%PCNAME%');
      ValueToTake := ValueOfEnvVar('PCNAME');
      if valueToTake = valueEnvVarNotFound then
        valueToTake := ValueOfEnvVar('computername');
      FConstValuesList.add(ValueToTake);

      FConstList.add('%HOST%');
      ValueToTake := ValueOfEnvVar('HOST');
      FConstValuesList.add(ValueToTake);
    {$ENDIF WINDOWS}

    {$IFDEF UNIX}
      FConstList.add('%PCNAME%');
      ValueToTake := GetHostName;
      if valueToTake = valueEnvVarNotFound then
        valueToTake := getCommandResult('hostname');
      FConstValuesList.add(ValueToTake);

      FConstList.add('%HOST%');
      ValueToTake := getCommandResult('hostname');
      FConstValuesList.add(ValueToTake);
    {$ENDIF LINUX}

      FConstList.add('%IPAddress%');
      FConstValuesList.add(ipAddress);

      FConstList.add('%IPName%');
      FConstValuesList.add(ipName);


      FConstList.add('%Username%');
      GetNetUser('', ValueToTake, ErrorInfo);
      FConstValuesList.add(ValueToTake);

    {$IFDEF WINDOWS}
      FConstList.add('%TempUser%');
      FConstValuesList.add(TempUserRegKey);
    {$ENDIF WINDOWS}

      { opsi service values }

      FConstList.add('%opsiserviceURL%');
      FConstValuesList.add(opsiserviceURL);

      FConstList.add('%opsiserviceUser%');
      FConstValuesList.add(opsiserviceUser);

      FConstList.add('%opsiservicePassword%');
      FConstValuesList.add(opsiservicePassword);

      FConstList.add('%hostID%');
      FConstValuesList.add(computernaming);

      FConstList.add('%opsiServer%');

      valuetotake := copy(opsiserviceUrl, pos('//', opsiserviceUrl) +
        2, length(opsiserviceurl));
      valuetotake := copy(valuetotake, 1, pos(':', valuetotake) - 1);
      FConstValuesList.add(valuetotake);

      FConstList.add('%opsiDepotId%');
      if opsidata = nil then
        FConstValuesList.add('')
      else
        FConstValuesList.add(trim(Topsi4data(opsidata).depotId));

      FConstList.add('%installingProdName%');
      if opsidata = nil then
        FConstValuesList.add('')
      else
        FConstValuesList.add(Topsi4data(opsidata).getActualProductId);

      FConstList.add('%installingProdVersion%');
      if opsidata = nil then
        FConstValuesList.add('')
      else
        try
          FConstValuesList.add(opsidata.getActualProductVersion);
        except
          FConstValuesList.add('')
        end;

      FConstList.add('%installingProduct%');
      if opsidata = nil then
        FConstValuesList.add('')
      else
        try
          FConstValuesList.add(Topsi4data(opsidata).getActualProductId);
        except
          FConstValuesList.add('')
        end;
    end;
    LogDatei.log_prog('End: Definition of global system variables', LLinfo);



    Aktionsliste := TWorkSection.Create(NestingLevel, nil);
  {$IFDEF GUI}
    FBatchOberflaeche.LoadSkin('');
    FBatchOberflaeche.setPicture('', '');
  {$ENDIF GUI}
    { initial section  }
    AktionsListe.Name := NameInitSektion;
    Script.GetSectionLines(NameInitSektion, TXStringList(Aktionsliste),
      StartlineOfSection, True, True, True);
    LogDatei.log_prog('CreateAndProcessScript: ' + Aktionsliste.Name +
      ': After GetSectionLiness', LLinfo);
    //Script.ApplyTextConstants (TXStringList (Aktionsliste));

    try
      { inital section  }
      if Aktionsliste.Count > 0 then
        weiter := Script.doAktionen(Aktionsliste, Aktionsliste)
      else
        weiter := tsrPositive;

      { profile actions section : run it only on loginscript  }
      if (weiter > 0) and runloginscripts then
      begin
        Aktionsliste.Clear;
        AktionsListe.Name := NameProfileActionsSection;
        Aktionsliste.StartLineNo := StartlineOfSection;
        Script.GetSectionLines(NameProfileActionsSection, TXStringList(Aktionsliste),
          StartlineOfSection, True, True, True);
        LogDatei.log_prog('CreateAndProcessScript: ' + Aktionsliste.Name +
          ': After GetSectionLiness', LLinfo);
        Script.ApplyTextConstants(TXStringList(Aktionsliste), False);
        LogDatei.log_prog('CreateAndProcessScript: ' + Aktionsliste.Name +
          ': After ApplyTextConstants', LLinfo);
        if Aktionsliste.Count > 0 then
        begin
          Aktionsliste.StartLineNo := StartlineOfSection;
          weiter := Script.doAktionen(Aktionsliste, Aktionsliste);
          { do not run actions after profileActions  }
          weiter := 0;
        end
        else
          weiter := tsrPositive;
      end;

      { actions section  }
      if weiter > 0 then
      begin
        Aktionsliste.Clear;
        Aktionsliste.Name := NameAktionenSektion;
        Aktionsliste.StartLineNo := StartlineOfSection;

        Script.GetSectionLines(NameAktionenSektion, TXStringList(Aktionsliste),
          StartlineOfSection, True, True, True);
        LogDatei.log_prog('CreateAndProcessScript: ' + Aktionsliste.Name +
          ': After GetSectionLiness', LLinfo);
        Script.ApplyTextConstants(TXStringList(Aktionsliste), False);
        LogDatei.log_prog('CreateAndProcessScript: ' + Aktionsliste.Name +
          ': After ApplyTextConstants', LLinfo);

        if Aktionsliste.Count > 0 then
        begin
          Aktionsliste.StartLineNo := StartlineOfSection;
          { Here do we run the script }
          LogDatei.log('Starting with script...', LLDebug);
          weiter := Script.doAktionen(Aktionsliste, Aktionsliste);
        end;
      end;
      try
        Aktionsliste.Free;
        Aktionsliste := nil;
      except
        on e: Exception do
     {$IFDEF GUI}
          MyMessageDlg.wiMessage('CreateAndProcessScript : free Aktionsliste: ' +
            e.Message, [mrOk]);
     {$ELSE GUI}
        writeln('CreateAndProcessScript : free Aktionsliste: ' + e.Message);
     {$ENDIF GUI}
      end;
    except
      on e: Exception do
      begin
        LogDatei.log('Exception in CreateAndProcessScript: Handling Aktionsliste: ' +
          e.message, LLError);
        extremeErrorLevel := levelFatal;
      end;
    end;

    freeDefinedFunctions;


  {$IFDEF GUI}
    CentralForm.Memo1.SelectAll;
    CentralForm.Memo1.SelStart := CentralForm.Memo1.SelLength;
  {$ENDIF GUI}

    // write final messages to log


    LogDatei.LogSIndentLevel := 0;

    if Script.FExtremeErrorLevel < extremeErrorLevel then
      extremeErrorLevel := Script.FExtremeErrorLevel;


    ps := ('___________________');
    LogDatei.log(ps, LLessential);
    ps := ('script finished: ');
    if extremeErrorLevel = LevelFatal then
      ps := ps + 'failed'
    else
      ps := ps + 'success';
    LogDatei.log(ps, LLessential);
    ps := IntToStr(Script.NumberOfErrors) + ' error';
    if Script.NumberOfErrors <> 1 then
      ps := ps + 's';
    LogDatei.log(ps, LLessential);

    ps := IntToStr(Script.NumberOfWarnings) + ' warning';
    if Script.NumberOfWarnings <> 1 then
      ps := ps + 's';
    LogDatei.log(ps, LLessential);
    ps := '';
    LogDatei.log(ps, LLessential);
    if opsidata <> nil then
    begin
      if runLoginScripts then
        LogDatei.log('Executed login script for : ' + Topsi4data(
          opsidata).getActualProductId + ' Version: ' +
          opsidata.getActualProductVersion, LLessential)
      else
      if runproductlist then
        LogDatei.log('Executed via runproductlist setup script for : ' +
          Topsi4data(opsidata).getActualProductId + ' Version: ' +
          opsidata.getActualProductVersion,
          LLessential)
      else
      begin
        LogDatei.log('installed product: ' + Topsi4data(opsidata).getActualProductId +
          ' Version: ' + opsidata.getActualProductVersion, LLessential);
        LogDatei.log2history('installed : ' + Topsi4data(opsidata).getActualProductId +
          ' Version: ' + opsidata.getActualProductVersion);
      end;

      //LogDatei.log ('opsi service version: '+opsidata.getOpsiServiceVersion, LLessential);
    end;
    LogDatei.log(ps, LLessential);

    if LogDatei.ReportErrorsToUser and (Script.NumberOfErrors > 0) then
    begin
      //if Script.NumberOfErrors = 1
      //then ps := 'is '
      //else ps := 'are ';
     {$IFDEF GUI}
      MyMessageDlg.WiMessage('On installing ' + ps + #10 +
        IntToStr(Script.NumberOfErrors) + '  ' + 'error(s) occurred ' +
        #10 + 'Please inform your administrator',
        [mrOk]);
     {$ENDIF GUI}
    end;
  {$IFDEF WIN32}
    if opsiSetupAdmin_created then
      if DeleteTemporaryLocalAdmin then
        LogDatei.log('Temporary Local Admin deleted', LLDebug2);
  {$ENDIF WIN32}
    if Logdatei.LogLevel < LLconfidential then
    begin
      deleteTempBatFiles('');
      LogDatei.log('Temp cmd files deleted, next: free script ', LLDebug2);
    end;
    if Script.forceLogInAppendMode then
      LogDatei.Appendmode := True;

    { moved before final output}
    //if Script.FExtremeErrorLevel < extremeErrorLevel
    //then extremeErrorLevel := Script.FExtremeErrorLevel;

    // reset current dir to the start value
    SetCurrentDir(opsiWinstStartdir);
    scriptsuspendstate := script.Suspended;

  {$IFDEF WIN32}
    // Unblock Input
    winBlockInput(False);
  {$ENDIF WIN32}

    try
      Script.Free;
      Script := nil;
    except
      on e: Exception do
   {$IFDEF GUI}
        MyMessageDlg.wiMessage('CreateAndProcessScript : free Aktionsliste: ' +
          e.Message, [mrOk]);
   {$ELSE GUI}
      writeln('CreateAndProcessScript : free Aktionsliste: ' + e.Message);
   {$ENDIF GUI}
    end;
    LogDatei.log('End of CreateAndProcessScript', LLDebug2);
    //LogDatei.Close;
  except
    on e: Exception do
    begin
      LogDatei.log('Exception in CreateAndProcessScript: General' + e.message, LLError);
      LogDatei.Close;
    end;
  end;
end;



begin
  randomize;

  //PreDefinedVariableSkinDirectory  := '$skindirectory$';
  //PreDefinedVariableSkinDirectoryValue := ExtractFileDir (Paramstr(0));

  GetMem(PStatNames, sizeof(TStatementNames));
  PStatNames^ [tsNotDefined] := '';
  PStatNames^ [tsActions] := 'Sub';
  PStatNames^ [tsProfileActions] := 'ProfileAction';
  PStatNames^ [tsPatchAnyTextfile] := 'PatchTextFile';
  PStatNames^ [tsTests] := 'Tests';
  //PStatNames^ [tsTestCommand]         := 'Testcommand';
  PStatNames^ [tsPatchIniFile] := 'Patches';
  PStatNames^ [tsHostsPatch] := 'PatchHosts';

  PStatNames^ [tsRegistryHack] := 'Registry';
  PStatNames^ [tsXMLPatch] := 'XMLPatch';
  PStatNames^ [tsXML2] := 'XML2';
  PStatNames^ [tsIdapiConfig] := 'IdapiConfig';
  PStatNames^ [tsLDAPsearch] := 'LDAPsearch';
  PStatNames^ [tsFileActions] := 'Files';
  PStatNames^ [tsLinkFolder] := 'LinkFolder';
  PStatNames^ [tsWinBatch] := 'WinBatch';
  PStatNames^ [tsDOSBatchFile] := 'DOSBatch';
  PStatNames^ [tsDOSInAnIcon] := 'DOSInAnIcon';
  PStatNames^ [tsShellBatchFile] := 'ShellBatch';
  PStatNames^ [tsShellInAnIcon] := 'ShellInAnIcon';
  PStatNames^ [tsExecutePython] := 'ExecPython';
  PStatNames^ [tsExecuteWith] := 'ExecWith';
  PStatNames^ [tsExecuteWith_escapingStrings] := 'ExecWith_escapingStrings';
  // is implemented via parameterEscapeStrings
  PStatNames^ [tsKillTask] := 'KillTask';
  PStatNames^ [tsOpsiServiceCall] := 'OpsiServiceCall';
  PStatNames^ [tsOpsiServiceCallStat] := 'OpsiServiceCall';
  PStatNames^ [tsOpsiServiceHashList] := 'OpsiServiceHashList';
  PStatNames^ [tsDDEwithProgman] := 'ProgmanGroups';

  PStatNames^ [tsWorkOnStringList] := 'WorkOnStringList';

  PStatNames^ [tsStayWhileWindowOpen] := 'StayWhileWindowOpen';
  (* fuer Testzwecke, nicht dokumentiert *)

  PStatNames^ [tsCondOpen] := 'If';
  PStatNames^ [tsCondThen] := 'Then';
  PStatNames^ [tsCondElse] := 'Else';
  PStatNames^ [tsCondClose] := 'EndIf';

  // switch
  PStatNames^ [tsSwitch] := 'Switch';
  PStatNames^ [tsSwitchCaseOpen] := 'Case';
  PStatNames^ [tsSwitchCaseClose] := 'EndCase';
  PStatNames^ [tsSwitchDefaultOpen] := 'DefaultCase';
  PStatNames^ [tsSwitchClose] := 'EndSwitch';


  PStatNames^ [tsLoopStringList] := 'for';
  PStatNames^ [tsLoopForTo] := 'for';

  PStatNames^ [tsMessage] := 'Message';
  PStatNames^ [tsMessageFile] := 'ShowMessageFile';
  PStatNames^ [tsShowBitmap] := 'ShowBitmap';
  PStatNames^ [tsPause] := 'Pause';
  PStatNames^ [tsSleep] := 'SleepSeconds';
  PStatNames^ [tsStop] := 'Stop';
  PStatNames^ [tsComment] := 'Comment';
  PStatNames^ [tsActionProgress] := 'setActionProgress';
  PStatNames^ [tsLogError] := 'LogError';
  PStatNames^ [tsLogWarning] := 'LogWarning';
  PStatNames^ [tsSetSkinDir] := 'SetSkinDirectory';

  PStatNames^ [tsImportLib] := 'ImportLib';
  PStatNames^ [tsIncludeInsert] := 'Include_Insert';
  PStatNames^ [tsIncludeAppend] := 'Include_Append';
  PStatNames^ [tsIncludeLog] := 'IncludeLog';
  PStatNames^ [tsShrinkFileToMB] := 'ShrinkFileToMB';
  PStatNames^ [tsChangeDirectory] := 'ChangeDirectory';



  PStatNames^ [tsExitWindows] := 'ExitWindows';
  PStatNames^ [tsLocalAdmin] := 'LocalAdmin';
  PStatNames^ [tsBlockInput] := 'BlockInput';
  PStatNames^ [tsSetDebug_prog] := 'SetDebug_prog';


  PStatNames^ [tsAddConnection] := 'AddConnection';
  (* nicht dokumentiert *)

  PStatNames^ [tsSetOldLogLevel] := 'LogLevel';
  PStatNames^ [tsSetLogLevel] := 'SetLogLevel';
  PStatNames^ [tsSetOutputLevel] := 'SetOutputLevel';
  PStatNames^ [tsSetExitOnError] := 'ExitOnError';
  PStatNames^ [tsFatalOnSyntaxError] := 'FatalOnSyntaxError';
  PStatNames^ [tsFatalOnRuntimeError] := 'FatalOnRuntimeError';
  PStatNames^ [tsAutoActivityDisplay] := 'AutoActivityDisplay';
  PStatNames^ [tsforceLogInAppendMode] := 'forceLogInAppendMode';

  PStatNames^ [tsSetConfidential] := 'SetConfidential';

  PStatNames^ [tsSetFatalError] := 'IsFatalError';
  PStatNames^ [tsSetSuccess] := 'IsSuccess';
  PStatNames^ [tsSetSuspended] := 'IsSuspended';
  PStatNames^ [tsSetNoUpdate] := 'noUpdateScript';
  PStatNames^ [tsSetMarkerErrorNumber] := 'MarkErrornumber';
  PStatNames^ [tsSetReportMessages] := 'ScriptErrorMessages';
  PStatNames^ [tsSetTimeMark] := 'MarkTime';
  PStatNames^ [tsLogDiffTime] := 'DiffTime';
  PStatNames^ [tsSetTraceMode] := 'TraceMode';
  PStatNames^ [tsSetStayOnTop] := 'StayOnTop';
  PStatNames^ [tsIconizeWinst] := 'IconizeWinst';
  PStatNames^ [tsRestoreWinst] := 'RestoreWinst';
  PStatNames^ [tsNormalizeWinst] := 'NormalizeWinst';
  PStatNames^ [tsMaximizeWinst] := 'MaximizeWinst';
  PStatNames^ [tsEncoding] := 'encoding';
  PStatNames^ [tsUpdateEnvironment] := 'UpdateEnvironment';
  PStatNames^ [tsLoadProductProperties] := 'LoadProductProperties';
  PStatNames^ [tsWinstVersionRequired] := 'RequiredWinstVersion';
  PStatNames^ [tsOpsiscriptVersionRequired] := 'RequiredOpsiscriptVersion';
  PStatNames^ [tsSetUsercontext] := 'SetUserContext';
  PStatNames^ [tsSaveVersionToProfile] := 'saveVersionToProfile';



  PStatNames^ [tsDefineVar] := 'DefVar';
  PStatNames^ [tsDefineStringList] := 'DefStringList';
  PStatNames^ [tsSetVar] := 'Set';
  PStatNames^ [tsShellCall] := 'shellCall';
  PStatNames^ [tsPowershellCall] := 'powershellCall';
  PStatNames^ [tsExecuteSection] := 'executeSection';
  PStatNames^ [tsDefineFunction] := 'DefFunc';
  PStatNames^ [tsEndFunction] := 'EndFunc';


  runProfileActions := False;
  runLoginScripts := False;
  allLoginScripts := True;
  inUsercontext := False;
  runSilent := False;
  flag_all_ntuser := False;
  runproductlist := False;
  runprocessproducts := False;
  scriptMode := tsmMachine;

end.
