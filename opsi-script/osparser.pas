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
osswaudit,
DOM,
{$ENDIF}
{$IFDEF LINUX}
lispecfolder,
osfunclin,
oslindesktopfiles,
baseunix,
unix,
{$ENDIF}
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
{$ENDIF GUI}
osencoding,
osconf,
//DOM,
//wixml,
//Process,
fileutil,
LazFileUtils,
  sysutils,
   Types,
  oslog,
  osfunc,
  classes,
  contnrs,
  IniFiles,
  //windatamodul,
  ldapsend,
  strutils,
  oslocale,
  osxbase64,
  dateutils,
  synautil,
  synaip,
  synsock,
  oscalc,
  osdefinedfunctions,
  opsihwbiosinfo,
  osjson,
  oscrypt,
  osparserhelper,
  osnetworkcalculator,
  osregex,
  osurlparser,
  LAZUTF8;


type
  TStatement = (tsNotDefined,
                tsDefinedVoidFunction,
                // start of sectionnames
                tsActions,
                tsProfileActions,
                tsPatchAnyTextFile,
                tsTests, tsPatchIniFile,
                tsHostsPatch, tsRegistryHack, tsXMLPatch, tsIdapiConfig, tsLDAPsearch,
                tsFileActions, tsLinkFolder,
                tsWinBatch, tsDOSBatchFile, tsDOSInAnIcon,
                tsShellBatchFile, tsShellInAnIcon, tsExecutePython,
                tsExecuteWith, tsExecuteWith_escapingStrings,
                tsOpsiServiceCall,
                tsOpsiServiceHashList,
                tsDDEwithProgman,
                // end of section names
                // start of other commands  after tsWorkOnStringList
                tsWorkOnStringList,
                tsOpsiServiceCallStat,
                //tsTestCommand,
                tsStayWhileWindowOpen,
                tsCondOpen, tsCondThen, tsCondElse, tsCondClose,
                tsSwitch, tsSwitchCaseOpen, tsSwitchCaseClose, tsSwitchDefaultOpen, tsSwitchClose,
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
                tsFatalOnSyntaxError,
                tsFatalOnRuntimeError,
                tsAutoActivityDisplay,
                tsSetTraceMode, tsSetStayOnTop,
                tsIconizeWinst, tsRestoreWinst, tsNormalizeWinst, tsMaximizeWinst,
                tsWinstVersionRequired,
                tsEncoding,
                tsUpdateEnvironment,
                tsLoadProductProperties,
                tsDefineVar, tsDefineStringList,
                tsDefineFunction,
                tsEndFunction,
                tsShellcall,
                tsPowershellcall,
                // tsSetVar should be the last here for loop in FindKindOfStatement
                tsSetVar);




  TStatementNames = Array [TStatement] of String [50];
  TPStatementNames = ^TStatementNames;

  TSectionSpecifier = (tsecNoSection, tsecIncluded, tsecExpected);

  TWaitConditions   = (ttpWaitOnTerminate, ttpWaitForWindowVanished,
                       ttpWaitForWindowAppearing, ttpWaitForProcessEnding,
                       ttpWaitTime,ttpWaitTimeout);

  TSetWaitConditions = Set of TWaitConditions;

  TLDAPsearchCacheRequest  =  (tlcrNone, tlcrCache, tlcrCached, tlcrClear);
  TLDAPsearchOutputRequest =  (tlorNone, tlorObjects, tlorAttributes, tlorValues);

  TSectionResult        = Integer;

  TRegistryFormat = (trnf,trfWinst, trfRegedit, trfSysdiff);

  TExitRequest = (txrNoExit,
                  txrRegisterforLogout,
                  txrRegisterforReboot,
                  txrReboot,
                  txrImmediateLogout,
                  txrImmediateReboot);

  TShutdownRequest = (tsrNoShutdown, tsrRegisterForShutdown);

  TScriptMode = (tsmMachine, tsmLogin);





Const
    tsrFatalError   = -2;
    tsrExitWindows  = -1;
    tsrExitProcess  =  0;
    tsrPositive     =  1;


    TempUserRegKey  = 'PatchNTUserdatTempUser';


    XMLDOMVendor = 'Open XML'; // 'Xerces XML'


type

TSectionInfo = record
    Sectionkind : TStatement;
    StartLineNo    : Integer;
    SectionName    : String;
    SectionFile    : String;
end;


TWorkSection = class (TuibIniScript) // class (TXStringList)
  private
    FSectionkind : TStatement;
    FStartLineNo    : Integer;
    FSectionName    : String;
    FNestingLevel   : Integer;
    FInSwitch       : boolean;
    FInCase         : boolean;
    FParentSection  : TWorkSection;

  public
    constructor create (const NestLevel : Integer; const ParentSection : TWorkSection);
    destructor destroy; override;

    property StartLineNo : Integer read FStartLineNo write FStartLineNo;
    property Name : String read FSectionName write FSectionName;
    property NestingLevel : Integer read FNestingLevel write FNestingLevel;
    property SectionKind : TStatement read FSectionKind write FSectionKind;
    property InSwitch : boolean read FInSwitch write FInSwitch;
    property InCase : boolean read FInCase write FInCase;
    property ParentSection : TWorkSection read FParentSection write FParentSection;

  end;


{$IFDEF FPC}
{$ELSE}
TuibXMLNodeDescription = class (TObject)
  private
    Fxmldoc : TuibXMLDocument;
    elementname : WideString;
    attributes : TStringList;
    text : WideString;
    positioning : boolean;
    positionOfElement : Integer;
    position : Integer;
  public
    constructor Create (xmldoc : TuibXMLDocument);
    destructor Destroy; override;

    function evaluate (s : String; var Remaining: String; var InfoSyntaxError : String) : Boolean;

    procedure evaluateElementname;
    procedure evaluateAttribute;
    procedure evaluateText;
 end;
{$ENDIF}




TuibInstScript = class (TuibIniScript)
private
  FLogLevel : Integer;
  FOutputLevel : Integer;
  FNumberOfWarnings : Integer;
  FNumberOfErrors : Integer;
  FTraceMode : Boolean;
  FExitOnError : Boolean;
  FReportMessages : Boolean;
  FExtremeErrorLevel : Integer;
  FFatalOnSyntaxError : boolean;
  FFatalOnRuntimeError : boolean;
  FSuspended : boolean;
  FAutoActivityDisplay : boolean;



  FVarList : TStringList;
  FlistOfStringLists : TStringList;
  FValuesList : TStringList;
  FContentOfStringLists : TObjectList;
  FConstList : TStringList;
  FConstValuesList : TStringList;
  FLastExitCodeOfExe : LongInt;
  FLastPrivateExitCode : LongInt; // not seen by getLastExitcode
  FFilename : String;
  FLinesOriginList : TStringList;
  FaktScriptLineNumber : int64;
  FEvalBoolBaseNestLevel : int64;
  FSectionNameList : Tstringlist;   // hold section and function names with index of FSectionInfoArray
  FSectionInfoArray : array of TSectionInfo; // holds for each section file and startline infos
  FActiveSection  : TWorkSection;
  FLastSection  : TWorkSection;


  
protected
  function getVarValues : TStringList; //nicht verwendet

public
  FLibList : TStringList;
  constructor create;
  destructor destroy; override;

  (* Properties *)
  property LogLevel : Integer read FLogLevel write FLogLevel;
  property OutputLevel : Integer read FOutputLevel write FOutputLevel;
  property NumberOfWarnings : Integer read FNumberOfWarnings;
  property NumberOfErrors : Integer read FNumberOfErrors;
  property TraceMode : Boolean read FTraceMode write FTraceMode;
  property ExitOnError : Boolean read FExitOnError write FExitOnError;
  property FatalOnSyntaxError : Boolean read FFatalOnSyntaxError write FFatalOnSyntaxError;
  property FatalOnRuntimeError : Boolean read FFatalOnRuntimeError write FFatalOnRuntimeError;
  property Suspended : Boolean read FSuspended write FSuspended;
  property AutoActivityDisplay : Boolean read FAutoActivityDisplay write FAutoActivityDisplay;


  property ReportMessages : Boolean read FReportMessages write FReportMessages;

  property varList : TStringList read FvarList write FvarList;
  property valuesList : TStringList read Fvalueslist write FvaluesList;
  property constList : TStringList read FconstList write FConstList;
  property constValuesList : TStringList read FconstValuesList write FconstValuesList;
  property listOfStringLists : TStringList read FlistOfStringLists write FlistOfStringLists;
  property ContentOfStringLists : TObjectList read FContentOfStringLists write FContentOfStringLists;
  property aktScriptLineNumber : int64 read FaktScriptLineNumber write FaktScriptLineNumber;
  property Filename : string read FFilename write FFilename;
  property ActiveSection  : TWorkSection read FActiveSection write FActiveSection;
  property LastSection  : TWorkSection read FLastSection write FLastSection;


  (* Infofunktionen *)
  function doInfo (Meldung : String)                          : TSectionResult;
  function doLogEntries (const Lines : String; EntryLevel : Integer)
                                                              : TSectionResult;
  function reportError (const Sektion: TWorkSection; LineNo : Integer;
                        const Content : String; Comment: String) : TSectionResult;


  (* Skriptvariable setzen, Ausdruecke analysieren und auswerten *)
  function doSetVar (const section: TuibIniScript; const Expressionstr : String;
                     var Remaining : String; var InfoSyntaxError : String) : Boolean; overload;

  function doSetVar (const section: TuibIniScript; const Expressionstr : String;
                   var Remaining : String; var InfoSyntaxError : String;
                   var NestLevel : integer) : Boolean; overload;

  function produceStringList
    (const section: TuibIniScript; const s0 : String; var Remaining: String;
     var list : TXStringlist; var InfoSyntaxError : String ) : Boolean; overload;

  function produceStringList
    (const section: TuibIniScript; const s0 : String; var Remaining: String;
     var list : TXStringlist; var InfoSyntaxError : String;
     var NestLevel : integer; const inDefFuncIndex : integer) : Boolean; overload;

  function EvaluateString
    (const s0 : String; var Remaining: String;
     var StringResult : String; var InfoSyntaxError : String ) : Boolean; overload;

  function EvaluateString
    (const s0 : String; var Remaining: String;
     var StringResult : String; var InfoSyntaxError : String;
     var NestLevel : integer; const inDefFuncIndex : integer ) : Boolean; overload;

  function EvaluateBoolean (Input : String; var Remaining : String;
     var BooleanResult : Boolean; NestingLevel : Integer; var InfoSyntaxError : String) : Boolean;

  function EvaluateBoolean_ (Input : String; var Remaining : String;
     var BooleanResult : Boolean; NestingLevel : Integer; var InfoSyntaxError : String) : Boolean;

  procedure GetWordOrStringExpressionstr (const s: String;
                           var resultString, Remaining, errorinfo : String);

  function produceExecLine(const s : String;
                        var programfilename, programparas, passparas, winstoption: String;
                        var InfoSyntaxError : String): boolean;


  function doXMLAddNamespace(filename:string;
    const elementname:string;
    const namespace:string):boolean;

  function doXMLRemoveNamespace(filename:string;
    const elementname:string;
    const namespace:string):boolean;



  (* Spezielle Methoden *)
  {$IFDEF WINDOWS}
  function StartConnection (LocalName, NetResourceName: String; WaitTime : LongInt) : TSectionResult;
  {$ENDIF WINDOWS}

  (* Sektion erstellen *)
  procedure loadValidLinesFromFile (FName : String; var Section : TWorkSection);
  //procedure getLinesFromUnicodeFile (Const FName : String; var Section : TWorkSection);
  procedure ApplyTextVariables (var Sektion : TXStringList; CStringEscaping : Boolean);
  procedure ApplyTextConstants (var Sektion : TXStringList; CStringEscaping : Boolean);
  procedure ApplyTextVariablesToString (var mystr : String; CStringEscaping : Boolean);

  // handle file and line origins
  procedure registerSectionOrigins(mylist : Tstringlist;filename : string);  overload;
  procedure registerSectionOrigins(mylist : Tstringlist;filename : string; secname : string); overload;

  (* Sektionsbearbeitungsmethoden *)
  (* Hilfsmethoden *)
  function initSection (const Sektion: TWorkSection; var SaveErrorNumber, SaveWarningNumber: Integer) : Boolean;
  procedure finishSection (const Sektion: TWorkSection; const SaveErrorNumber, SaveWarningNumber: Integer;
                            var DiffNumberOfErrors, DiffNumberOfWarnings : Integer);

  (* fuer primaere Sektionen *)
  function doAktionen (const Sektion: TWorkSection; const CallingSektion: TWorkSection)
                                                               : TSectionResult;
  (* fuer andere Sektionen *)

  function doTextpatch (const Sektion: TWorkSection; Filename : String; PatchParameter : String)
                                                               : TSectionResult;


  function doTests (const Sektion: TWorkSection; TestParameter : String)
                                                               : TSectionResult;

  function doInifilePatches (const Sektion: TWorkSection; Filename : string;  PatchParameter : String)
                                                                 : TSectionResult;

  function doHostsPatch (const Sektion: TWorkSection; HostsFilename : String)
                                                                 : TSectionResult;
  function doIdapiConfig (const Sektion : TWorkSection; IdapiParameter : String)
                                                               : TSectionResult;

  function doLDAPsearch
          (const Sektion : TWorkSection;
          cacheRequest   : TLDAPsearchCacheRequest;
          outputRequest  : TLDAPsearchOutputRequest;
          var output: TXStringList)
                                                               : TSectionResult;
{$IFDEF WINDOWS}
  function doRegistryHack (const Sektion: TWorkSection; Const RegParameter : String; const flag_force64 : boolean)
                                                               : TSectionResult;
  function doRegistryHackInfSource (const Sektion: TWorkSection; Const RegParameter : String; const flag_force64 : boolean)
                                                               : TSectionResult;
  function doRegistryHackRegeditFormat (const Sektion: TWorkSection;
                                        Const RegParameter : String; const flag_force64 : boolean)
                                                               : TSectionResult;
  function doRegistryAllNTUserDats (const Sektion: TWorkSection;  rfSelected: TRegistryFormat; const flag_force64 : boolean)
                                                               : TSectionResult;
  function doRegistryAllUsrClassDats (const Sektion: TWorkSection;  rfSelected: TRegistryFormat; const flag_force64 : boolean)
                                                               : TSectionResult;
  function doRegistryNTUserDat (const Sektion: TWorkSection;
         rfSelected: TRegistryFormat; const flag_force64 : boolean; const UserPath : string): TSectionResult;


{$ENDIF WINDOWS}

  function doXMLPatch (const Sektion: TWorkSection; Const XMLFilename : String;
           var output: TXStringList)
                                                                 : TSectionResult;

  function doOpsiServiceHashList (const Sektion: TWorkSection;
  Const parameter : String; var output: TXStringList)
                                                                 : TSectionResult;

  function doOpsiServiceCall (const Sektion: TWorkSection; Const parameter : String;
           var output: TXStringList)
                                                                 : TSectionResult;


  function doFileActions (const Sektion: TWorkSection; CopyParameter : String)
                                                                 : TSectionResult;


  function doLinkFolderActions (const Sektion: TWorkSection; common : boolean)
                                                                 : TSectionResult;

  function doDDEwithProgman (const Sektion: TWorkSection; DDEParameter : String;
                              SaveddeWithProgman : Boolean)      : TSectionResult;
  function execWinBatch (const Sektion: TWorkSection; WinBatchParameter : String;
                 WaitConditions : TSetWaitConditions; ident : String; WaitSecs : Word;
                 runAs : TRunAs;flag_force64:boolean) : TSectionResult;
  function execDOSBatch (const Sektion: TWorkSection; BatchParameter : String;
                 ShowCmd : Integer; catchOut: Boolean; logleveloffset : integer;
                 WaitConditions : TSetWaitConditions;
                 var output: TXStringList)                       : TSectionResult;


  function execPython (
        const Sektion: TWorkSection; PythonParameter : String;
        catchOut : Boolean; logleveloffset : integer;WaitConditions : TSetWaitConditions;
        var output: TXStringList)                       : TSectionResult;


  function executeWith (
      const Sektion: TWorkSection; ExecParameter : String;
      catchOut : Boolean; logleveloffset : integer;
      var output: TXStringList)                       : TSectionResult;

  function execShellCall (command : String; archparam:string;
          logleveloffset : integer; FetchExitCodePublic : boolean): TStringlist; overload;
  function execShellCall (command : String; archparam:string;
          logleveloffset : integer;
          FetchExitCodePublic, FatalOnFail : boolean): TStringlist;  overload;
{$IFDEF WINDOWS}
  function execPowershellCall (command : String; archparam:string;
          logleveloffset : integer;
          FetchExitCodePublic, FatalOnFail : boolean; handle_policy : boolean): TStringlist;
 {$ENDIF WINDOWS}
end;


procedure CreateAndProcessScript (Const Scriptdatei : String;
                         NestingLevel : Integer; SaveddeWithProgman : Boolean;
                         var extremeErrorLevel : TErrorLevel);

function DefaultHosts : String;

function GetMACAddress2: string;

function FindKindOfStatement (const Statement: String;
       var SectionSpecifier :  TSectionSpecifier;
            const completeCall : String) : TStatement;
          

function GetNetUser (Host : String; Var UserName : String; var ErrorInfo : String) : Boolean;
  (* for Host = '' Username will become the name of the current user of the process *)


function getDecimalCompareSign
   (const decimalString1, decimalString2 : String;
    var sign : Integer;
    var InfoSyntaxError : String;
    stringcomparison: boolean) : Boolean;

function SearchForSectionLines
  (const selfsection : TUIBInstScript; localsection, callingsection : TWorkSection; const Sectionname: string; var Resultlist: TXStringList;
  var StartlineNo: integer; takeCommentLines, takeEmptyLines, trimmed:
  boolean): boolean;

const
  NameInitSektion                   = 'Initial';
  NameAktionenSektion               = 'Actions';
  NameProfileActionsSection         = 'ProfileActions';
  //NameAktionenSektion2              = 'Actions';

  (* Registry call parameters *)
  Parameter_SysDiffAddReg           = '/AddReg';
  Parameter_RegeditFormat           = '/Regedit';
  Parameter_AllNTUserDats           = '/AllNTUserDats';
  Parameter_AllUsrClassDats         = '/AllUsrClassDats';
  Parameter_RegistryBaseKey         = '/BaseKey';
  Parameter_RegistryUsercontext     = '/UserContext';
  Parameter_Registry64Bit           = '/64Bit';
  Parameter_Registry32Bit           = '/32Bit';
  Parameter_RegistrySysNative       = '/SysNative';
  Parameter_RegistryNTUserDat       = '/NTUserDat:';

  (* File section parameters *)
  Parameter_AllNTUserSendTo         = '/AllNTUserSendTo';
  Parameter_AllNTUserProfiles       = '/AllNTUserProfiles';
  Parameter_AllNTUser               = '/AllNTUser';
  Parameter_64Bit                     = '/64Bit';
  Parameter_32Bit                     = '/32Bit';
  Parameter_SysNative                = '/SysNative';

  (* 'WinBatch' *)
   ParameterDontWait                = '/LetThemGo';
   ParameterWaitOnTerminate         = '/WaitOnClose';
   ParameterCloseBehindWindow       = '/WaitForWindowVanish';
   ParameterCloseOnWindow           = '/WaitForWindowAppearing';
   ParameterWaitSecs                = '/WaitSeconds';
   ParameterWaitForProcessEnding    = '/WaitForProcessEnding';
   ParameterWaitProcessTimeoutSecs  = '/TimeOutSeconds';
   ParameterRunAsAdmin              = '/RunAsLoggedOnAdmin';
   ParameterRunAsAdmin1             = '/RunAsLoggedOnAdmin1';
   ParameterRunAsAdmin2             = '/RunAsLoggedOnAdmin2';
   ParameterRunAsAdmin3             = '/RunAsLoggedOnAdmin3';
   ParameterRunAsAdmin4             = '/RunAsLoggedOnAdmin4';
   ParameterRunAsInvoker            = '/RunAsInvoker';
   ParameterRunElevated             = '/RunElevated';
   ParameterRunAsLoggedOnUser       = '/RunAsLoggedOnUser';
   ParameterShowWindowHide          = '/WindowHide';


  DefaultWaitProcessTimeoutSecs = 1200; //20 min

  winst_emptyregkeydat = 'winst_emptyregkey.dat';
  TempBatchfilename           = '_opsiscript_';
  TempBatchdatei     = TempBatchfilename + '.cmd';
  TempBatchDatei_UniqueCount : Integer = 0;

  StopInfo             = 'Process stopped';
  AbortedInfo          = 'Process aborted';
  SpaceForEnvVarAsString = '2048';

  RegNoCallLogout = 0;
  RegCallLogout   = 2;
  RegCallReboot   = 4;


  (* Synt-Errors *)
  ErrorRemaining  = 'Char(s) at end of line not interpreted';
  
  (* Keys for ExecWith *)
  optionsSplitter = 'WINST';
  passSplitter = 'PASS';
  parameterEscapeStrings = '/EscapeStrings';


var
  PStatNames : TPStatementNames;
  flag_all_ntuser, flag_ntuser, flag_all_usrclass : boolean;
  flag_encoding : string = 'system';
  runLoginScripts: boolean;
  allLoginScripts : boolean;
  inUsercontext: boolean;
  runSilent: boolean;
  scriptMode : TScriptMode;
  runProfileActions : boolean;
  runproductlist : boolean;
  opsiWinstStartdir : string;
  Script : TuibInstScript;
  scriptsuspendstate : boolean;
  scriptstopped : boolean;
  inDefFuncLevel : integer = 0;
  inDefFuncIndex : integer = -1; // index of the active defined function
  Ifelseendiflevel : longint = 0; // global nestlevel store (do 18.1.2018)
  inDefFunc3 : integer = 0;  // we are between deffunc and endfunc line (even in a not active code)



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
  {NB30,} {for getmacadress2}
  osmain;




var
 ps : String;
 markedTime : TDateTime;

 // Expressionstr, Remaining : String;

 OldNumberOfErrors, DiffNumberOfErrors,
 OldNumberOfWarnings, DiffNumberOfWarnings : Integer;

 local_opsidata : TOpsi4Data;

 cacheRequest   : TLDAPsearchCacheRequest;
 outputRequest  : TLDAPsearchOutputRequest;
 ldapResult : TStringList;

 //labelinfoAlignmentSave : TAlignment;

type
 TConditions = Array [0..100] of Boolean;
var
 Conditions : TConditions;
 ThenBranch : TConditions;

//const
 //zaehler  : Integer = 0;



function GetString
  (const s : String; var ResultString, Remaining, errorinfo : String;
   StringInStringAllowed : Boolean) : Boolean;   overload;  forward;
function GetString
  (const s : String; var ResultString, Remaining, errorinfo : String;
   StringInStringAllowed, requote : boolean) : Boolean;   overload;  forward;

function SearchForSectionLines
  (const selfsection : TUIBInstScript; localsection, callingsection : TWorkSection; const Sectionname: string; var Resultlist: TXStringList;
  var StartlineNo: integer; takeCommentLines, takeEmptyLines, trimmed:
  boolean): boolean;
var
  myworksection : TWorkSection;
begin
  result := false;
  Resultlist.clear;

  // look if we are in a subprogram
  // that may have its own sections in it

  if (Resultlist.count = 0) and (inDefFuncLevel > 0)
  then
  begin
    // local function
    Logdatei.log('Looking for section: '+ Sectionname +' in local function .',LLDebug3);
    localsection.GetSectionLines (Sectionname, Resultlist,
                      StartlineNo, true, true, false);
  end;

  if (Resultlist.count = 0) and (inDefFuncLevel > 0)
  then
  begin
    // local function2
    Logdatei.log('Looking for section: '+ Sectionname +' in local function: '+definedFunctionArray[inDefFuncIndex].Name,LLDebug3);
    myworksection := TWorkSection.create(NestingLevel,nil);
    myworksection.AddText(definedFunctionArray[inDefFuncIndex].Content.Text);
    myworksection.GetSectionLines (Sectionname,
         Resultlist,StartlineNo, true, true, false);
    myworksection.Free;
  end;

  if Resultlist.count = 0
  then
  begin
    // normal case
    Logdatei.log('Looking for section: '+ Sectionname +' in standard section.',LLDebug3);
    selfsection.GetSectionLines (Sectionname, Resultlist,
                      StartlineNo, true, true, false);
  end;(*
  else
  begin
    if 0 <= selfsection.FindSectionheaderIndex(Sectionname) then
      Logdatei.log('Multiple sections with same name: '+ Sectionname +'also found in standard section.',LLWarning);
  end;*)


  if Assigned(callingsection) and (callingsection <> nil) then
  begin
    // subsub case
    if Resultlist.count = 0 then
    begin
      Logdatei.log('Looking for section: '+ Sectionname +' in calling section.',LLDebug3);
      callingsection.GetSectionLines (Sectionname, Resultlist,
                        StartlineNo, true, true, false);
    end;(*
    else
    begin
      if 0 <= callingsection.FindSectionheaderIndex(Sectionname) then
        Logdatei.log('Multiple sections with same name: '+ Sectionname +'also found in calling section.',LLWarning);
    end;*)
  end;



  if Resultlist.count = 0
  then
  begin
    // subsub case
    Logdatei.log('Looking for section: '+ Sectionname +' in global section.',LLDebug3);
    localsection.GetSectionLines (Sectionname, Resultlist,
                      StartlineNo, true, true, false);
  end;(*
  else
  begin
    if 0 <= localsection.FindSectionheaderIndex(Sectionname) then
      Logdatei.log('Multiple sections with same name: '+ Sectionname +'also found in global section.',LLWarning);
  end;*)

  if Assigned(callingsection) and (callingsection <> nil)
    and Assigned(callingsection.ParentSection)
    and (callingsection.ParentSection <> nil) then
  begin
    // subsubsub case
    if Resultlist.count = 0  then
    begin
      Logdatei.log('Looking for section: '+ Sectionname +' in callingsection.ParentSection section.',LLDebug3);
      callingsection.ParentSection.GetSectionLines(Sectionname, Resultlist,
                      StartlineNo, true, true, false);
    end;(*
    else
    begin
      if 0 <= callingsection.ParentSection.FindSectionheaderIndex(Sectionname) then
        Logdatei.log('Multiple sections with same name: '+ Sectionname +'also found in callingsection.ParentSection section.',LLWarning);
    end;*)
  end;

  if Assigned(callingsection) and (callingsection <> nil)
       and Assigned(callingsection.ParentSection)
       and (callingsection.ParentSection <> nil)
       and Assigned(callingsection.ParentSection.ParentSection)
       and (callingsection.ParentSection.ParentSection <> nil) then
  begin
    // subsubsubsub case
    if Resultlist.count = 0 then
    begin
      Logdatei.log('Looking for section: '+ Sectionname +' in callingsection.FParentSection.FParentSectio section.',LLDebug3);
      callingsection.FParentSection.FParentSection.GetSectionLines(Sectionname, Resultlist,
                      StartlineNo, true, true, false);
    end;(*
    else
    begin
      if 0 <= callingsection.FParentSection.FParentSection.FindSectionheaderIndex(Sectionname) then
        Logdatei.log('Multiple sections with same name: '+ Sectionname +'also found in callingsection.FParentSection.FParentSection section.',LLWarning);
    end;*)
  end;

  if Resultlist.count > 0 then result := true;
end;

function getCompareSignStrings (s1 : String; s2 : String) : Integer;
 var s1A, s2A : String;
begin
  s1A := AnsiUpperCase(s1);
  s2A := AnsiUpperCase(s2);
  result := 0;
  if s1A > s2A then result := 1
  else if s1A < s2A then result := -1
end;

function getCompareSign (number1 : Integer; number2 : Integer) : Integer;
begin
  result := 0;
  if number1 > number2 then result := 1
  else if number1 < number2 then result := -1
end;


function getCompareSignDouble (number1 : double; number2 : double) : Integer;
begin
  result := 0;
  if number1 > number2 then result := 1
  else if number1 < number2 then result := -1
end;


function getDecimalCompareSign
   (const decimalString1, decimalString2 : String;
    var sign : Integer;
    var InfoSyntaxError : String;
    stringcomparison: boolean) : Boolean;

var
 decimals1 : TXStringList;
 decimals2 : TXStringList;
 i : Integer;
 comparing : Boolean;
 number1 : Integer;
 number2 : Integer;

 ///partCompareResult : Integer;
 ///isEqual : Boolean;


begin

  result := true;
  if (trim (decimalString1) = '')
    or (trim (decimalString2) = '')

  then
  begin
    result := false;
    InfoSyntaxError := 'Missing value for comparing';
    exit;
  end;


  decimals1 := TXStringlist.create;
  decimals2 := TXStringlist.create;
  stringsplit(trim(decimalString1), '.', decimals1);
  stringsplit(trim(decimalString2), '.', decimals2);


  sign := 0; // we start with assumption equality
  comparing := true;
  i := 1;

  while result and comparing
  do
  Begin

     if (decimals1.count >= i) and (decimals2.count < i)
     then
     Begin
      comparing := false;
      if sign = 0 // components were equal up to now
      then
        sign := 1
     End
     else if (decimals1.count < i) and (decimals2.count >= i)
     then
     Begin
       comparing := false;
       if sign = 0
       then sign := -1
     End
     else if (decimals1.count < i) and (decimals2.count < i)
     then
     Begin
      comparing := false;
     End;


     // we continue comparing
     if comparing
     then
     begin
        if stringcomparison
        then
        begin
          sign := getCompareSignStrings(decimals1[i-1], decimals2[i-1]);
        end

        else
        begin

          try
            number1 := strtoInt (decimals1[i-1]);
            number2 := strtoInt (decimals2[i-1]);
          except
            InfoSyntaxError := 'Expecting a sequence of "." and numbers';
            result := false;
          end;

          if result
          then
            sign := getCompareSign(number1, number2);
        end;

        if sign <> 0 then comparing := false;
     End;

     inc (i);
  end;

end;



function hasRelation (orderrelationSymbol : String; orderSign : Integer; var BooleanResult : boolean; var InfoSyntaxError : String) : Boolean;
begin
   result := true;

   if orderRelationSymbol = '='       then  BooleanResult :=  (orderSign = 0)
   else if orderRelationSymbol = '>'  then  BooleanResult :=  (orderSign > 0)
   else if orderRelationSymbol = '>=' then  BooleanResult :=  (orderSign >= 0)
   else if orderRelationSymbol = '<'  then  BooleanResult :=  (orderSign < 0)
   else if orderRelationSymbol = '<=' then  BooleanResult :=  (orderSign <= 0)

   else
   Begin
     result := false;
     infoSyntaxError := '"=", ">", ">=" or "<", "<=" expected';
   end
end;



procedure adjustBounds (var a1, a2 : Integer; const list1 : TXStringList);
// adjust the lower and upper indices to the limits of the real list
begin
  //a1
   if a1 < 0 then a1 := 0;
  //a2

   if list1.count = 0
   then
     a2 := -1
   else
   begin

     if a2 >= list1.count
     then a2 := list1.count - 1
     else

       while (a2 < 0)
       do
         a2 := a2 + list1.count; // move modulo list1.count

   end;

end;

function getOrderSymbol (const s : String; var ResultString, Remaining, errorinfo : String) : Boolean;
begin
   result := true;
   if Skip ('>=', s, remaining, errorinfo) then resultstring := '>='
   else if Skip ('<=', s, remaining, errorinfo) then resultstring := '<='
   else if Skip ('>', s, remaining, errorinfo) then resultstring := '>'
   else if Skip ('<', s, remaining, errorinfo) then resultstring := '<'
   else if Skip ('=', s, remaining, errorinfo) then resultstring := '='
   else
   Begin
      result := false;
      errorinfo := 'relation symbol ("<", "=", ">" ...) expected';
   end;
end;

function GetString
  (const s : String; var ResultString, Remaining, errorinfo : String;
   StringInStringAllowed : Boolean) : Boolean;
begin
  result := GetString(s, ResultString, Remaining, errorinfo, StringInStringAllowed, false);
end;

function GetString
  (const s : String; var ResultString, Remaining, errorinfo : String;
   StringInStringAllowed, requote : boolean) : Boolean;
(* reads a string that is delimited either by ' or by "  (citation mark);
   if StringInStringallowed then the function interprets a double mark
   of the appropriate kind as being contained in the string.
   resultstring takes the correctly interpreted chars
   remaining gets the original value s if result is false *)

 var
   TheMark: Char;
   r, Info, DoubleMark : String;
   continue : Boolean;
   PartValue : String='';

   //i : Integer;
   //foundDelimiter : Boolean;

   rorg, rnew : String;

begin
  result := false;
  resultString := '';
  info :='';
  r := s;
  SetLength (r, length (s));


  if (length (r) < 2)
  then
    ErrorInfo := 'String marker expected'
  else
  Begin
    logdatei.log_prog('r:'+r,llDebug3);
    TheMark := r[1];
    if (TheMark <> '''') and (TheMark <> '"')
    then
        ErrorInfo := '" or '' expected'
    else
    Begin
      DoubleMark := TheMark + TheMark;
      if requote then
      begin
        if pos(DoubleMark,r) = 1 then
        begin
          logdatei.log_prog('may be double quotet: r:'+r,llDebug2);
          rorg := r;
          //GetWord (rorg, rnew, rorg, WordDelimiterWhiteSpace);
          rnew := r;
          logdatei.log_prog('rnew:'+rnew,llDebug2);

          if AnsiEndsStr(DoubleMark, rnew) then
          begin
            logdatei.log_prog('rnew is doublequoted:'+rnew,llDebug2);
            //unquote the wrong mark
            rnew := opsiunquotestr2(rnew,TheMark);
            // quote with the alternative mark
            if TheMark = '"' then
            begin
              r := '''' + rnew + '''';
              TheMark := '''';
            end
            else
            begin
              r:= '"' + rnew + '"';
              TheMark := '"';
            end;
          end;
        end;
      end;
      //skipA(TheMark, r, r, errorinfo);
      logdatei.log_prog('r:'+r,llDebug3);
      skipA(TheMark, r, r, errorinfo);
      logdatei.log_prog('r:'+r,llDebug3);
      Continue := true;
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
        logdatei.log_prog('r:'+r,llDebug3);
        GetWord (r, PartValue, r, [TheMark]);
        ResultString := ResultString + PartValue;
        logdatei.log_prog('r:'+r+' ResultString:'+ResultString,llDebug3);

        if StringInStringAllowed then
        Begin
          if SkipA (DoubleMark, r, r, Info)  //SkipA (DoubleMark, r, r, Info)
          then
            ResultString := ResultString + TheMark
          else
            Continue := false;
        End
        else
          Continue := false;
        logdatei.log_prog('r:'+r+' ResultString:'+ResultString,llDebug3);
      End;
      if SkipA (TheMark, r, r, ErrorInfo)//SkipA (TheMark, r, r, ErrorInfo)
      then result := true;
    End;
  End;

  if result
  //then Remaining := cutLeftBlanks (r)
  then Remaining := r
  else Remaining := s;
end;


function winstGetTempFileNameWithExt(myext:string): string;
begin
  Randomize;
  result := TempPath + TempBatchfilename + randomstr(False) +myext;
  //inc(TempBatchDatei_UniqueCount);
  //result := TempPath + TempBatchfilename + inttoStr(TempBatchDatei_UniqueCount) +myext;
end;

function winstGetTempFileName: string;
begin
  result := winstGetTempFileNameWithExt('.cmd');
end;


procedure deleteTempBatFiles(const tempfilename : string);
Var
  files : TuibFileInstall;
begin
  if tempfilename <> '' then
  begin
    // delete the actual file
    if SysUtils.DeleteFile(tempfilename) then
      Logdatei.log('The file: '+tempfilename+' has been deleted', LLDebug)
    else
      Logdatei.log('The file: '+tempfilename+' was not deleted', LLDebug);
  end;

  // delete files that are older than 2 days
  files := TuibFileInstall.create;
  try
    if tempfilename <> '' then
      files.alldelete(TempPath + TempBatchfilename + '*', false, true, 2);
  except
    LogDatei.log('not all files "' + TempPath + TempBatchdatei + '*"  could be deleted', LLInfo);
  end;
  files.Free;
end;




function GetStringA
  (const s : String; var ResultString, Remaining, errorinfo : String;
   StringInStringAllowed : Boolean) : Boolean;
   // do not delete the heading spaces
   // reads a string that is delimited either by ' or by "  (citation mark);
   // if StringInStringallowed then the function interprets a double mark
   // of the appropriate kind as being contained in the string.
   // resultstring takes the correctly interpreted chars
   // remaining gets the original value s if result is false

 var
   TheMark: Char;
   r, Info, DoubleMark : String;
   continue : Boolean;
   PartValue : String='';

   //i : Integer;
   //foundDelimiter : Boolean;

begin
  info:='';
  result := false;
  resultString := '';
  r := s;
  SetLength (r, length (s));


  if (length (r) < 2)
  then
    ErrorInfo := 'string delimiter expected'
  else
  Begin
    TheMark := r[1];
    logdatei.log('r:'+r,llDebug2);
    if (TheMark <> '''') and (TheMark <> '"')
    then
        ErrorInfo := '" or '' expected '
    else
    Begin
      DoubleMark := TheMark + TheMark;
      //skipA(TheMark, r, r, errorinfo);
      skipA(TheMark, r, r, errorinfo);
      Continue := true;
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
        GetWord (r, PartValue, r, [TheMark]);
        ResultString := ResultString + PartValue;

        if StringInStringAllowed then
        Begin
          if SkipA (DoubleMark, r, r, Info)  //SkipA (DoubleMark, r, r, Info)
          then
            ResultString := ResultString + TheMark
          else
            Continue := false;
        End
        else
          Continue := false;
      End;
      if SkipA (TheMark, r, r, ErrorInfo)//SkipA (TheMark, r, r, ErrorInfo)
      then result := true;
    End;
  End;

  if result
  then Remaining := r // cutLeftBlanks (r)
  else Remaining := s;
end;

function SeekString
  (const s : String; var ResultString, Remaining, errorinfo : String) : Boolean;
  {looks for next string in s, delimited by ",
   escaping of " by \ possible}

  var
    i : Integer;
    state : Integer;
    goOn : Boolean;
begin
   result := false;
   ResultString := '';
   errorinfo := '';
   i := 1;
   goOn := true;
   state := 0;
   while goOn
   do
   Begin
     if i > length(s)
     then goOn := false
     else

     case state of
          0: // string not started
          Begin
             if s[i] = '"' then Begin state := 1;  End; //string started
          End;

          1 :
          Begin
             if s[i] = '\' then state := -1 // escape char
             else if s[i] = '"' then Begin state := 2; result := true; End //string finished
             else
               ResultString := ResultString + s[i]
          End;

         -1 :
         Begin
            ResultString := ResultString + s[i]; state := 1;
         End;

         2 : // stripping white space
         Begin
           if not ( (s[i] = ' ') or (s[i] = #9))
             then goon := false;
         End
     End;

     inc (i);
   End;



   if not result
   then
     errorinfo := 'string end mark missing';

   Remaining := copy (s, i-1, length(s));
end;


function FindKindOfStatement (const Statement: String;
                              var SectionSpecifier :  TSectionSpecifier;
                              const CompleteCall : String) : TStatement;
  var
   s : TStatement;
   test : String;
   i1, i2, FuncIndex : Integer;
   funcname, r : string;
   deffuncFound : boolean;
begin
  result := tsNotDefined;
  SectionSpecifier := tsecNoSection;
  for s := tsWorkOnStringList to tsSetVar
  do
    if  UpperCase (Statement) = UpperCase (PStatNames^ [s])
    then
    Begin
      result := s;
      exit;
    End;
  for s := tsActions to tsDDEwithProgman
  do
  Begin
    if copy (UpperCase (Statement), 1, length (PStatNames^[s]))
       = UpperCase(PStatNames^[s])
    then
    Begin
      result := s;
      test := copy (UpperCase (Statement), 1, length (PStatNames^[s]));
      test := UpperCase(PStatNames^[s]);
      if length (PStatNames^[s]) < length (Statement)
      then SectionSpecifier := tsecIncluded
      else SectionSpecifier := tsecExpected;

      //special hack for tsExecuteWith
      if result = tsExecuteWith
      then
      Begin
        i1 := pos (lowercase(OPTIONSSPLITTER) + ' ', lowercase(completecall));
        if (i1 > 0)
        then
        Begin
            i2 :=  pos ( lowercase(parameterEscapeStrings) ,
                         lowercase( copy(completecall, i1, length(completecall) ) ) );
            if i2 > 0
            then result := tsExecuteWith_escapingStrings
        end;
      end;
      exit;
    End;
  End;
  // look if we have defined function with result type void
  deffuncFound := false;
  GetWord (CompleteCall, funcname, r, WordDelimiterSet5);
  FuncIndex := definedFunctionNames.IndexOf (LowerCase (funcname));
  if FuncIndex >= 0 then
    if definedFunctionArray[FuncIndex].datatype = dfpVoid then
       deffuncFound := true;
 if deffuncFound then
  Begin
    result := tsDefinedVoidFunction;
    exit;
  End;
end;


function DefaultHosts : String;
begin
  {$IFDEF WINDOWS}
  if UpperCase (ValueOfEnvVar ('OS')) = 'WINDOWS_NT'
  then
    result :=   GetWinDirectory + 'system32\drivers\etc\'
               (* DefaultHostsDir0 *)
  else
    result  :=  GetWinDirectory;
               (* DefaultHostsDir1; *)
  result := result + 'hosts';
  {$ELSE WINDOWS}
  result := '/etc/hosts';
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

function readFirstLineFromFile(filename : string) : string;
var
  myfile : text;
  myresult : string;
begin
  try
    assignfile(myfile,filename);
    reset(myfile);
    readln(myfile,myresult);
    closefile(myfile);
    result := myresult;
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
  TCreateGUIDFunction = function(AGUID: PGUID): LongInt; stdcall;
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
  VDevice := 'eth0';
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
      VCreateGUIDFunction := TCreateGUIDFunction(GetProcedureAddress(VLibHandle,
        'UuidCreateSequential'));
      if Assigned(VCreateGUIDFunction) then
        if (VCreateGUIDFunction(@VGUID1) = 0) and
          (VCreateGUIDFunction(@VGUID2) = 0) and (VGUID1.D4[2] = VGUID2.D4[2]) and
          (VGUID1.D4[3] = VGUID2.D4[3]) and (VGUID1.D4[4] = VGUID2.D4[4]) and
          (VGUID1.D4[5] = VGUID2.D4[5]) and (VGUID1.D4[6] = VGUID2.D4[6]) and
          (VGUID1.D4[7] = VGUID2.D4[7]) then
            Result := Format(CLSFormatMACMask, [VGUID1.D4[2], VGUID1.D4[3],
                        VGUID1.D4[4], VGUID1.D4[5], VGUID1.D4[6], VGUID1.D4[7]]);
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
  Result    := '';
  _SystemID := '';
  Getmem(NCB, SizeOf(TNCB));
  Fillchar(NCB^, SizeOf(TNCB), 0);

  Getmem(Lenum, SizeOf(TLanaEnum));
  Fillchar(Lenum^, SizeOf(TLanaEnum), 0);

  Getmem(Adapter, SizeOf(TAdapterStatus));
  Fillchar(Adapter^, SizeOf(TAdapterStatus), 0);

  Lenum.Length    := chr(0);
  NCB.ncb_command := chr(NCBENUM);
  NCB.ncb_buffer  := Pointer(Lenum);
  NCB.ncb_length  := SizeOf(Lenum);
  RetCode         := Netbios(NCB);

  i := 0;
  repeat
    Fillchar(NCB^, SizeOf(TNCB), 0);
    Ncb.ncb_command  := chr(NCBRESET);
    Ncb.ncb_lana_num := lenum.lana[I];
    RetCode          := Netbios(Ncb);

    Fillchar(NCB^, SizeOf(TNCB), 0);
    Ncb.ncb_command  := chr(NCBASTAT);
    Ncb.ncb_lana_num := lenum.lana[I];
    // Must be 16
    Ncb.ncb_callname := '*               ';

    Ncb.ncb_buffer := Pointer(Adapter);

    Ncb.ncb_length := SizeOf(TAdapterStatus);
    RetCode        := Netbios(Ncb);
    //---- calc _systemId from mac-address[2-5] XOR mac-address[1]...
    if (RetCode = chr(0)) or (RetCode = chr(6)) then
    begin
      _SystemId := IntToHex(Ord(Adapter.adapter_address[0]), 2) + ':' +
        IntToHex(Ord(Adapter.adapter_address[1]), 2) + ':' +
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


{
Type
 TGUID=record
  A,B:word;
  D,M,S:word;
  MAC:array[1..6] of byte;
 end;

function GetMACAddress1:String;
var
 UuidCreateFunc : function (var guid: TGUID):HResult;stdcall;
 handle : THandle;
 g:TGUID;
 WinVer:_OSVersionInfoA;
 i:integer;
 ErrCode: HResult;
begin
 WinVer.dwOSVersionInfoSize := sizeof(WinVer);
 getversionex(WinVer);

 handle := LoadLibrary('RPCRT4.DLL');
 if WinVer.dwMajorVersion >= 5 then (* Windows 2000 *)
  @UuidCreateFunc := GetProcAddress(Handle, 'UuidCreateSequential')
 else
  @UuidCreateFunc := GetProcAddress(Handle, 'UuidCreate') ;

 UuidCreateFunc(g);
 result:='';
 for i:=1 to 6 do
   result:=result+IntToHex(g.MAC[i],2);
end;

}


{
procedure DeleteRegKey(aRoot : HKey; aPath : String);
var
  SL : TStringList;
  X : Integer;
begin
  SL := TStringList.Create;
  with TRegistry.Create do
  try
    RootKey := aRoot;
    if OpenKey(aPath,true) then begin
      GetKeyNames(SL);
      For X:=0 to SL.Count-1 do DeleteRegKey(aRoot,aPath + '\' + SL[X]);
      CloseKey;
      DeleteKey(aPath);
    end;
  finally
    Free;
    SL.Free;
  end;
end;

}

function GetNetUser (Host : String; Var UserName : String; var ErrorInfo : String) : Boolean;
{$IFDEF WINDOWS}
  (* for Host = '' Username will become the name of the current user of the process *)

var
  pLocalName : PChar;
  pUserName  : PChar;


  function ApiCall (var Username, ErrorInfo : String; BuffSize : DWord) : Boolean;
   var
   errorcode : DWord;
   nBuffSize  : DWord;
   pErrorBuff, pNameBuff : PChar;
   nErrorBuffSize : DWord=0;
   nNameBuffSize : DWord=0;


  begin
    result := false;
    GetMem (pUserName, BuffSize);
    nBuffSize := Buffsize;

    UserName := '';
    errorCode := WNetGetUser(nil, pUserName, nBuffSize);


    case errorCode of
      no_error                  :
        Begin
         ErrorInfo := '';
         SetLength (UserName, StrLen (pUserName));
         UserName := pUserName;
         result := true;
        End;
      ERROR_NOT_CONNECTED      : ErrorInfo :=
      'The device specified by lpszLocalName is not a redirected device or a connected network name.';
      ERROR_MORE_DATA      : ApiCall (UserName, ErrorInfo, nBuffSize + 1);
      ERROR_NO_NETWORK      : ErrorInfo := 'No network is present.';
      ERROR_EXTENDED_ERROR      :
         Begin
           GetMem (pErrorBuff, 300);
           GetMem (pNameBuff, 300);
           WNetGetLastError (errorcode, pErrorBuff, nErrorBuffSize, pNameBuff, nNameBuffSize);
           ErrorInfo := pErrorBuff;
           FreeMem (pErrorBuff);
           FreeMem (pNameBuff);
         End;
      ERROR_NO_NET_OR_BAD_PATH   : ErrorInfo :=
         'None of the providers recognized this local name as having a connection. '
       + 'However, the network is not available for at least one provider to whom the connection may belong';
      else
      errorInfo := 'NT-Error ' + RemoveLineBreaks(SysErrorMessage (errorCode));
    end;

    if errorCode <> no_error then errorInfo := IntToStr (errorCode) + ' ' + errorInfo;

    FreeMem (pUserName);
  end;


Begin
  if Host <> ''
  then pLocalName := PChar (Host)
  else pLocalName := nil;

  if ApiCall (Username, ErrorInfo, 100)
  then result := true
  else result := false;
End;
{$ELSE WINDOWS}
begin
  //###LINUX
  result := true;
  Username := getCommandResult('/bin/bash -c whoami');
end;
{$ENDIF WINDOWS}

constructor TWorkSection.create (const NestLevel : Integer; const ParentSection : TWorkSection);
begin
  inherited create;
  FStartLineNo  := 0;
  FSectionName  := '';
  FNestingLevel := NestLevel;
  FInSwitch := false;
  FInCase := false;
  FParentSection:= ParentSection;
end;

destructor TWorkSection.destroy;
begin
  inherited destroy;
end;

{$IFDEF FPC}
{$ELSE}
// *** TuibXMLNodeDescription ****
//

constructor TuibXMLNodeDescription.Create (xmldoc : TuibXMLDocument);
begin
 inherited create;
 fxmldoc := xmldoc;
end;

destructor TuibXMLNodeDescription.Destroy;
begin

 inherited destroy;
end;


function TuibXMLNodeDescription.evaluate (s : String; var Remaining: String;
                                           var  InfoSyntaxError : String) : Boolean;

 var
  r : String;
  syntaxCheck : Boolean;
  syntaxInfo : String;
  elementname : String;

begin
  r := lowercase (s);

  syntaxCheck := false;
  InfoSyntaxError := '';

  if Skip ('(', s, r, syntaxInfo)  // geklammerter Ausdruck
  then
  begin
   if evaluate (r, r, syntaxInfo)
   and Skip (')', r, r, syntaxInfo)
   then
      syntaxCheck := true;
  end


  else if Skip ('elementname', r, r, syntaxInfo)
  then
  Begin

    if GetString (r, elementname, r, InfoSyntaxError, false)
    then
    begin
      syntaxCheck := true;
      //Fxmldoc.filterByChildelement(elementname)
    end;

  end

  else if Skip ('attribute', r, r, syntaxInfo)
  then
  Begin

  end

  else if Skip ('position', r, r, syntaxInfo)
  then
  Begin

  end

  else
  begin
     InfoSyntaxError := 'XML Path description not accepted'
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



constructor TuibInstScript.create;

Begin
  inherited create;
  FNumberOfErrors := 0;
  FNumberOfWarnings := 0;

  FExtremeErrorLevel := level_not_initialized;

  FLogLevel := LLinfo;
  FExitOnError := false;
  //FReportMessages := true;
  FReportMessages := osconf.ScriptErrorMessages;
  FTraceMode := false;
  FFatalOnSyntaxError := true;
  FFatalOnRuntimeError := false;
  FSuspended := false;
  //FAutoActivityDisplay := false;
  FAutoActivityDisplay := osconf.AutoActivityDisplay;
  scriptstopped := false;

  FVarList := TStringList.create;
  //VarList.add(PreDefinedVariableSkinDirectory);
  FValuesList := TStringList.create;
  //ValuesList.add(PreDefinedVariableSkinDirectoryValue);

  FlistOfStringLists := TStringList.create;
  FContentOfStringLists := TObjectList.create;
  FConstList := TStringList.create;
  FConstValuesList := TStringList.create;
  FLinesOriginList := TStringList.create;
  FLibList := TStringList.create;
  FsectionNameList := TStringList.create;
  //FSectionInfoArray := Length(0);
  FActiveSection := nil;
  FLastSection := nil;
End;

destructor TuibInstScript.destroy;
var
  counter,i : integer;
begin
  FVarList.free; VarList := nil;
  FValuesList.free; ValuesList := nil;
  FlistOfStringLists.free; listOfStringLists := nil;
  FContentOfStringLists.free; ContentOfStringLists := nil;
  FLinesOriginList.free; FLinesOriginList := nil;
  FLibList.Free; FLibList := nil;
  FsectionNameList.Free; FsectionNameList := nil;
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



procedure TuibInstScript.LoadValidLinesFromFile (FName : String; var Section : TWorkSection);
 var
  OriginalList : TXStringList;
  s : String;
  i : Integer;
  Encoding2use,usedEncoding : string;
  statkind : TStatement;
  //secname, remaining : string;
  //secindex : integer;
begin
  Section.Clear;
  OriginalList := TXStringList.create;
  OriginalList.LoadFromFile (ExpandFileName(FName));
  Encoding2use := searchencoding(OriginalList.Text);
  if Encoding2use = '' then Encoding2use := 'system';
  OriginalList.Text:=  reencode(OriginalList.Text, Encoding2use,usedEncoding);
  logdatei.log('Loaded sub from: '+FName+' with encoding: '+usedEncoding,LLDebug);
  for i := 1 to OriginalList.count do
  Begin
    s := trim(OriginalList.Strings [i-1]);
    Section.Add (s);
    script.FLinesOriginList.Append(FName+' line: '+inttostr(i));
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
  End;
  registerSectionOrigins(OriginalList,FName, Section.Name);
  OriginalList.free;
End;


function TuibInstScript.reportError (const Sektion: TWorkSection; LineNo : Integer;
             Const Content : String; Comment: String) : TSectionResult;
var
  funcname, funcfile, funcmesseage : string;
  originmessage : string;
  funcline : integer;
  i,index : integer;
begin
  try
    result := tsrPositive;
    if inDefFuncIndex >= 0 then
    begin
      funcname := definedFunctionArray[inDefFuncIndex].Name;
      funcfile := definedFunctionArray[inDefFuncIndex].OriginFile;
      funcline := definedFunctionArray[inDefFuncIndex].OriginFileStartLineNumber;
      funcmesseage := ' in defined function: '+funcname+' file: '+ExtractFileName(funcfile)
                      +' function start at line: '+inttostr(funcline+1);
      originmessage := '; origin: '+funcfile+' line: '+inttostr(funcline+LineNo+1)+'): ';
    end
    else
    begin
      funcname := Sektion.Name;
      index := script.FSectionNameList.IndexOf(funcname);
      if index = -1 then
      begin
        funcmesseage := ' in section: '+Sektion.Name+'; file: unknown';
        originmessage := '; origin: not found'+'): ';
      end
      else
      begin
        funcfile := Script.FSectionInfoArray[index].SectionFile;
        funcline := Script.FSectionInfoArray[index].StartLineNo;
        funcmesseage := ' in section: '+funcname+'; file: '+ExtractFileName(funcfile)
                        +'; section start at line: '+inttostr(funcline+1);
        //originmessage := '; origin: '+FLinesOriginList.Strings[Sektion.StartLineNo + LineNo]+'): ';
        originmessage := '; origin: '+funcfile+'; line: '+inttostr(funcline+LineNo+1)+'): ';
      end;
    end;
  except
    originmessage := '; origin: not found'+'): ';
    funcmesseage := '; section or function not found';
  end;
  //for i:= 0 to FLinesOriginList.Count -1 do
  //  logdatei.log_prog('FLinesOriginList: '+FLinesOriginList.Strings[i],LLDebug);
  ps := 'Syntax Error in Section: ' + Sektion.Name +
    ' (Command in line ' + IntToStr (Sektion.StartLineNo + LineNo)
   //   + ' Command in line ' + IntToStr (script.aktScriptLineNumber)
      + funcmesseage + originmessage
    //  + ' origin: '+FLinesOriginList.Strings[Sektion.StartLineNo + LineNo-1]+'): '
    //  + ' origin: '+FLinesOriginList.Strings[script.aktScriptLineNumber]+'): '
      + Content+' -> '+Comment;
  LogDatei.log(ps,LLCritical);
  if FatalOnSyntaxError then
  begin
    FExtremeErrorLevel:=LevelFatal;
    LogDatei.ActionProgress:='Syntax Error';
  end;
  inc(FNumberOfErrors);
  //Topsi4data(opsidata).ActionProgress:= 'Script Syntax Error';
  if ReportMessages then
  Begin
    ps := 'Section   ' + Sektion.Name +
                ' (Command in line ' + IntToStr (Sektion.StartLineNo + LineNo) + '):'
          + LineEnding + Content
          + LineEnding + Comment;
    {$IFDEF GUI}
    if MyMessageDlg.WiMessage (ps, [mrOK, mrAbort]) = mrAbort
      then result := tsrExitProcess;
    {$ELSE GUI}
    writeln(ps);
    //result := tsrExitProcess;
    {$ENDIF GUI}
  End;

end;


function TuibInstScript.getVarValues : TStringList;
var
  k : Integer;
begin
  if FVarList <> nil
  then
  begin
    k := FVarList.IndexOf('%loglevel%');
    if k > -1
    then
      FValuesList.strings[k] := IntToStr (Logdatei.LogLevel);
  end;
  result := FValuesList;
end;


function TuibInstScript.doInfo (Meldung: String) : TSectionResult;
begin
  {$IFDEF GUI}
  result := tsrPositive;
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
    FBatchOberflaeche.LabelInfo.Font.Size := LableInfoDefaultFontSize;;
  end;


  FBatchOberflaeche.setInfoLabel(Meldung);
  {$ELSE GUI}
  result := tsrPositive;
  writeln(Meldung);
  {$ENDIF GUI}
end;

function TuibInstScript.doLogEntries (const Lines : String; EntryLevel : Integer)
   : TSectionResult;
var
  ManyLines, line : String;
  i : Byte;
begin
  result := 0;
  ManyLines := Lines;
  repeat
    i := pos (LineEnding, ManyLines);
    if i > 0 then
    Begin
      line := copy (ManyLines, 1, i - 1);
      ManyLines := copy (ManyLines, i+2, length (ManyLines))
    End
    else
      line := ManyLines;
    LogDatei.log (Line, EntryLevel);

  until i = 0;
  {$IFDEF GUI}
  if logdatei.LogLevel >= EntryLevel then
  begin
    if Line = PStatNames^ [tsCondOpen] then Line := '|';
    if Line = PStatNames^ [tsCondThen] then Line := '/';
    if Line = PStatNames^ [tsCondElse] then Line := '-';
    if Line = PStatNames^ [tsCondClose] then Line := '\';
    CentralForm.Label2.caption := Line;
    FBatchOberflaeche.setDetailLabel(Line);
  end;

  ProcessMess;
  {$ENDIF GUI}

  result := tsrPositive;
end;


function TuibInstScript.initSection (const Sektion: TWorkSection;
   var SaveErrorNumber, SaveWarningNumber : Integer) : Boolean;
begin
  result := true;
  if Sektion.count = 0 then result := false;

  SaveErrorNumber := LogDatei.NumberOfErrors;
  SaveWarningNumber := LogDatei.NumberOfWarnings;
  //LogDatei.ErrorNumberMarked := SaveErrorNumber;

  (* LogDatei.LogSIndentLevel := Sektion.NestingLevel; *)
  ps := '';
  LogDatei.log (ps, LLNotice);
  if Sektion.count > 0 then
  begin
    //ps := 'Execution of ' + Sektion.Name;
    //LogDatei.log (ps, LLNotice);
  end
  else
  // this case should be captured beforehand
    LogDatei.log ('Warning: Section  "' + Sektion.Name + '"  does not exist or is empty', LLWarning);

  //ps := (*  'Ausfuehrung von ' +  *) copy (Sektion.Name, length (PStatNames^ [Sektion.fSectionKind]) + 1,
  //                                length (Sektion.Name));
  ps := Sektion.Name;
  {$IFDEF GUI}
  CentralForm.Label2.caption := ps;
  FBatchOberflaeche.setDetailLabel(CentralForm.Label2.caption);
  {$ENDIF GUI}
end;

procedure TuibInstScript.finishSection (const Sektion: TWorkSection;
   const SaveErrorNumber, SaveWarningNumber : Integer;
   var DiffNumberOfErrors, DiffNumberOfWarnings : Integer);
begin
  DiffNumberOfErrors := LogDatei.NumberOfErrors - SaveErrorNumber;
  DiffNumberOfWarnings := LogDatei.NumberOfWarnings - SaveWarningNumber;
  FNumberOfErrors := SaveErrorNumber + DiffNumberOfErrors;
  FNumberOfWarnings := SaveWarningNumber + DiffNumberOfWarnings;
  {$IFDEF GUI}
  CentralForm.Label2.caption := CentralForm.Label2.caption + ' finished';
  FBatchOberflaeche.setDetailLabel(CentralForm.Label2.caption);

  (* LogDatei.LogSIndentLevel := Sektion.NestingLevel; *)
  ProcessMess;
  {$ENDIF GUI}
end;

function TuibInstScript.doTextpatch (const Sektion: TWorkSection; Filename : String; PatchParameter : String)
                                                               : TSectionResult;

var
  i, j, insertLineIndx: Integer;
  methodname : String='';
  s : String='';
  r : String='';
  s0 : String='';
  s1 : String='';
  s2 : String='';
  startofline : String='';
  oldLine : String='';
  old_s2 : String='';
  x : String='';
  found : Boolean;
  lastfind : Boolean;
  PatchListe : TpatchList;
  ErrorInfo : String='';
  FileError : String='';
  syntaxCheck : Boolean;
  indx : Integer;
  d, sum : Integer;
  saveToOriginalFile : Boolean;
  working : Boolean;
  goOn : Boolean;
  secondStringList : TStringList;
  PatchFilename : String='';
  Patchdatei : TuibPatchIniFile;
  ProfileList : TStringlist;
  pc: Integer=0;

  procedure CheckRemainder (Var SyntaxCheck : Boolean);
  begin
    if r <> ''
    then
      errorinfo := ErrorRemaining
    else
      syntaxCheck := true;
  end;



  procedure doTextpatchMain(const Section : TXStringList; const presetDir : string);
  var
  i: Integer=0;
  index: Integer=0;
  patchlistcounter : Integer=0;
  workingSection : TXStringList;
  NameValueSeparator : char;

  begin
    //ps := LogDatei.LogSIndentPlus (+3) + 'FILE ' +  PatchdateiName;
    //LogDatei.log (ps, LevelWarnings);
    Logdatei.log('', LLInfo);
    Logdatei.log('Patching: '+PatchFilename, LLInfo);
    ps := LogDatei.LogSIndentPlus (+3) + 'FILE ' +  PatchFilename;
    LogDatei.log (ps, LevelWarnings);

    workingSection := TXStringList.Create;
    workingSection.Assign(Section);
    workingSection.GlobalReplace(1, '%userprofiledir%', copy (presetDir, 1, length(presetDir) - 1), false);
    workingSection.GlobalReplace(1, '%currentprofiledir%', copy (presetDir, 1, length(presetDir) - 1), false);


    if not FileExists (ExpandFileName(PatchFilename))
    then
    Begin
      ps := LogDatei.LogSIndentPlus (+3) + 'Info: This file does not exist and will be created ';
      LogDatei.log (ps, LLInfo);
      LogDatei.NumberOfHints := Logdatei.NumberOfHints + 1;

      if CreateTextfile (ExpandFileName(PatchFilename), ErrorInfo)
      then
      Begin
       if ErrorInfo <> ''
       then
       Begin
         ps := LogDatei.LogSIndentPlus (+3) + 'Warning: ' + ErrorInfo;
         LogDatei.log (ps, LLWarning);
       End;
      End
      else
      Begin
         ps := LogDatei.LogSIndentPlus (+3) + 'Error: ' + ErrorInfo;
         LogDatei.log (ps, LLError);
         exit; // ------------------------------  exit
       End;
    End;


    ProcessMess;
    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

    // create the list we work on
    PatchListe := TPatchList.Create;
    PatchListe.clear;
    PatchListe.ItemPointer := -1;
    PatchListe.LoadFromFile (ExpandFileName(PatchFilename));
    PatchListe.Text:= reencode(PatchListe.Text,'system');
    saveToOriginalFile := true;
    lastfind := false;

    i := 1;
    syntaxcheck := true;
    while (i <= workingSection.count) and syntaxcheck
    do
    Begin
      ErrorInfo := '';
      syntaxCheck := false;
      r := cutLeftBlanks (workingSection.strings [i-1]);
      if (r = '') or (r [1] = LineIsCommentChar)
      then
        syntaxCheck := true
       // continue
      else
      Begin
        logdatei.log('Patchtextfile: command: '+r,LLDebug3);
        GetWord (r, methodname, r, WordDelimiterSet1);

        if LowerCase (methodname) = 'sorted'
        then
        Begin
          CheckRemainder (syntaxCheck);

          if SyntaxCheck
          then
          Begin
            PatchListe.Sort;
            PatchListe.Sorted := true;
          End;
        End

        else if LowerCase (methodname) = 'findline'
        then
        begin
          if GetString (r, s, r, errorinfo, true)
          then CheckRemainder (syntaxCheck);

          if syntaxCheck
          then
            PatchListe.ItemPointer
             := PatchListe.FindFirstItem (s, false, PatchListe.ItemPointer, lastfind);
        end

        else if LowerCase (methodname) = 'findline_containing'
        then
        Begin
          if GetString (r, s, r, errorinfo, true)
          then CheckRemainder (syntaxCheck);

          if syntaxCheck then
            PatchListe.ItemPointer
              := PatchListe.FindFirstItemWith (s, false, PatchListe.ItemPointer);
        End

        else if LowerCase (methodname) = 'findline_startingwith'
        then
        Begin
          LogDatei.log('r:'+r+' s:'+s,LLDebug3);
          if GetString (r, s, r, errorinfo, true)
          then CheckRemainder (syntaxCheck);
          LogDatei.log('r:'+r+' s:'+s,LLDebug3);
          if syntaxCheck then
           PatchListe.ItemPointer
           := PatchListe.FindFirstItemStartingWith (s, false, PatchListe.ItemPointer);
        End

        else if LowerCase (methodname) = lowerCase ('DeleteAllLines_StartingWith')
        then
        Begin
          if GetString (r, s, r, errorinfo, true)
          then CheckRemainder (syntaxCheck);

          if syntaxCheck
          then
          Begin
            indx := PatchListe.FindFirstItemWith (s, false, -1);
            while indx > -1
            do
            Begin
              PatchListe.delete (indx);
              indx := PatchListe.FindFirstItemStartingWith (s, false, -1);
            End;
          End;
        End

        else if LowerCase (methodname) = 'advanceline'
        then
        Begin
          syntaxCheck := true;
          if r = ''
          then
            d := 1
          else
            try
              d := StrToInt (r);
            except
              syntaxCheck := false;
              errorInfo := r + ' is not an integer value ';
            end;

          if SyntaxCheck
          then
          Begin
            sum := PatchListe.ItemPointer + d;
            if sum < 0
              then PatchListe.ItemPointer := 0
            else if sum > PatchListe.Count - 1
              then PatchListe.ItemPointer := PatchListe.Count - 1
            else PatchListe.ItemPointer := sum;
          End;
        End

        else if LowerCase (methodname) = lowercase ('GoToTop')
        then
        Begin
          CheckRemainder (syntaxCheck);

          //if PatchListe.count > 0
          //then
          PatchListe.SetItemPointer (-1);
        End

        else if LowerCase (methodname) = LowerCase ('GoToBottom')
        then
        Begin
          CheckRemainder (syntaxCheck);

          if PatchListe.count > 0
          then
            PatchListe.ItemPointer := PatchListe.count - 1
        End

        else if (LowerCase (methodname) = 'insert_line')
           or   (LowerCase (methodname) = 'insertline')
        then
        Begin
          if GetStringA (r, s, r, errorinfo, true)
          then CheckRemainder (syntaxCheck);

          if syntaxCheck
          then
          if (PatchListe.ItemPointer > -1)
          then
            PatchListe.insert (PatchListe.ItemPointer, s)
          else if PatchListe.count = 0
          then
            PatchListe.add (s)
          else
            PatchListe.insert (0, s);

        End

        else if (LowerCase (methodname) = 'append_line')
             or (LowerCase (methodname) = 'appendline')
        then
        Begin
          if GetStringA (r, s, r, errorinfo, true)
          then CheckRemainder (syntaxCheck);

          if syntaxCheck
          then
          if (PatchListe.ItemPointer > -1) and (PatchListe.ItemPointer < PatchListe.count - 1)
          then
          Begin
            PatchListe.ItemPointer := PatchListe.ItemPointer + 1;
            PatchListe.insert (PatchListe.ItemPointer , s)
          End
          else
            PatchListe.add (s);
        End

        else if  (LowerCase (methodname) = 'append_file')
              or (LowerCase (methodname) = 'appendfile')
        then
        Begin
          if GetString (r, s1, r, errorinfo, true)
          then CheckRemainder (syntaxCheck);

          if syntaxCheck
          then
          Begin
            s1 := ExpandFileName(s1);
            working := CheckFileExists (s1, FileError);
            if working
            then
            begin
              try
                 secondStringList := TStringList.create;
                 secondStringList.LoadFromFile (ExpandFileName(s1));
                 secondStringList.Text:= reencode(secondStringList.Text, 'system');
                 PatchListe.addStrings (secondStringList);
                 secondStringList.Free;
              except
                   on ex: Exception
                   do
                   begin
                     FileError := ex.message;
                     working := false;
                   end;
              end;
            End;

            if working
            then
              LogDatei.log ('Appended "' + s1 + '"', LevelComplete)
            else
            Begin
              LogDatei.log('Error: Could not append "' + s1 + '" , ' + FileError,
                                      LLError);
            End;
          End;
        End

        else if LowerCase (methodname) = 'deletetheline'
        then
        Begin
          CheckRemainder (syntaxCheck);

          if syntaxCheck
          then
            PatchListe.delete (PatchListe.ItemPointer)
        End

        else if   (LowerCase (methodname) = 'deleteline')
               or (LowerCase (methodname) = 'delete_line')
        then
        Begin
          if GetString (r, s1, r, errorinfo, true)
            then CheckRemainder (syntaxCheck);

            if syntaxCheck
            then
            Begin
              indx := PatchListe.FindFirstItem (s1, false, PatchListe.ItemPointer, lastfind);
              if lastfind then PatchListe.delete (indx);
            End
        End

        else if (LowerCase (methodname) = 'addline')
             or (LowerCase (methodname) = 'add_line')
        then
        Begin
          if GetString (r, s, r, errorinfo, true)
          then CheckRemainder (syntaxCheck);

          if syntaxCheck then
            PatchListe.Add (s)
        End

        else if (LowerCase (methodname) = LowerCase('setKeyValueSeparator'))
        then
        Begin
          if GetString (r, s, r, errorinfo, true)
          then CheckRemainder (syntaxCheck);
          if Length(s) = 1 then
          begin
            NameValueSeparator :=   char(s[1]);
            LogDatei.log('KeyValueSeparator changed from: >'+ PatchListe.NameValueSeparator +
                         '< to : >'+ NameValueSeparator +'<',LLInfo);
          end
          else
          begin
            syntaxCheck := false;
            ErrorInfo := 'KeyValueSeparator has to be a char';
          end;
          if syntaxCheck then
            PatchListe.NameValueSeparator := NameValueSeparator;
        End

        else if (LowerCase (methodname) = LowerCase('setValueByKey'))
        then
        Begin
          if GetStringA (trim(r), s1, r, errorinfo, true) then
          begin
            logdatei.log('key= '+s1,LLDebug2);
            if GetStringA (trim(r), s2, r, errorinfo, true) then
            begin
              logdatei.log('value= '+s2,LLDebug2);
              syntaxCheck := true;
            end
            else  syntaxCheck := false
          end
          else  syntaxCheck := false;
          if syntaxCheck then
          begin
            // the fast way:
            index := PatchListe.IndexOfName(s1);
            if index = -1 then
            begin
              // let us retry with trimed keys
             for patchlistcounter := 0 to PatchListe.Count -1 do
             begin
               if PatchListe.Names[patchlistcounter] <> '' then
                 if lowerCase(trim(PatchListe.Names[patchlistcounter])) = lowerCase(trim(s1)) then
                   index := PatchListe.IndexOfName(PatchListe.Names[patchlistcounter]);
             end;
            end;
            if index = -1 then
            begin
              logdatei.log('Key: '+s1+' not found - creating',LLInfo);
              if (PatchListe.ItemPointer > -1) and (PatchListe.ItemPointer < PatchListe.count - 1)
              then
              Begin
                PatchListe.ItemPointer := PatchListe.ItemPointer + 1;
                PatchListe.insert (PatchListe.ItemPointer , s1+PatchListe.NameValueSeparator+s2);
              End
              else
                PatchListe.add (s1+PatchListe.NameValueSeparator+s2);
            end
            else
            begin
              logdatei.log('Key: '+s1+' found - Value was: >'+PatchListe.ValueFromIndex[index]+
                           '< setting to: >'+s2+'<' ,LLInfo);
              PatchListe.ValueFromIndex[index] := s2;
            end;
          end;
        End

        else if (LowerCase (methodname) = LowerCase('searchAndReplace'))
        then
        Begin
          if GetStringA (trim(r), s1, r, errorinfo, true) then
          begin
            logdatei.log('search string= '+s1,LLDebug2);
            if GetStringA (trim(r), s2, r, errorinfo, true) then
            begin
              logdatei.log('replace string= '+s2,LLDebug2);
              syntaxCheck := true;
            end
            else  syntaxCheck := false
          end
          else  syntaxCheck := false;
          if syntaxCheck then
          begin
           if patchliste.GlobalReplace(1,s1,s2,false) then
             logdatei.log('Replaced all occurrences of  "'+s1+'" by "'+s2+'".',LLInfo)
           else
             logdatei.log('No occurrences of  "'+s1+'" found - nothing replaced.',LLInfo)
          end;
        End

        else if (LowerCase (methodname) = lowerCase ('SaveToFile'))
        then
        Begin
          saveToOriginalFile := false;
          LogDatei.log ('saveToOriginalFile = false', LevelComplete);
          if GetString (r, s1, r, ErrorInfo, false)
          then CheckRemainder (syntaxCheck);

          if SyntaxCheck then
          Begin
             s1 := ExpandFileName(s1);
             working := FileExists (s1) or CreateTextFile (s1, FileError);
             if working then
               try
                  PatchListe.SaveToFile (s1);
               except
                  on ex: Exception do
                  begin
                     FileError := ex.message;
                     working := false;
                  end;
               end;

             if working
             then
               LogDatei.log ('The lines are saved to "' + s1 + '"', LevelComplete)
             else
             Begin
               LogDatei.log ('Error: Could not save lines to "' + s1 + '" , ' + FileError,
                                       LLError);
             End;
          End;
        End

        else if (LowerCase (methodname) = lowerCase ('Subtract_File'))
        then
        Begin
          if GetString (r, s1, r, ErrorInfo, false)
          then CheckRemainder (syntaxCheck);

          if SyntaxCheck then
          Begin
            s1 := ExpandFileName(s1);
            working := CheckFileExists (s1, FileError);
            if working
            then
            Begin
              try
                 secondStringList := TStringList.create;
                 secondStringList.LoadFromFile (ExpandFileName(s1));
                 secondStringList.Text:= reencode(secondStringList.Text, 'system');
                 patchliste.SetItemPointer(0);

                 j := 0;
                 goOn := true;
                 while (patchliste.count > 0) and (j + 1 <= secondStringList.count) and goOn
                 do
                 Begin
                    if patchliste.strings [0] = secondStringList.strings [j]
                    then
                    Begin
                       patchliste.delete(0);
                       inc (j);
                    end
                    else
                       goOn := false
                 End;
                 secondStringList.free;
              except
                   on ex: Exception
                   do
                   begin
                     FileError := ex.message;
                     working := false;
                   end;
              end;
            End;

            if working
            then
              LogDatei.log ('Subtracted "' + s1 + '"', LevelComplete)
            else
              LogDatei.log ('Error: Could not substract "' + s1 + '" , ' + FileError,
                                      LLError);
          End;
        End

        else if LowerCase (methodname) = lowercase ('Set_Mozilla_Pref')
        then
        Begin

         //purify layout
         while PatchListe.GlobalReplace(1, ' (', '(', true) do;
         while PatchListe.GlobalReplace(1, '( ', '(', true) do;

         if Skip ('(', r, r, ErrorInfo)
            and GetStringA(r, s0, r, ErrorInfo, false)
            and Skip(',', r, r, ErrorInfo)
            and GetStringA(r, s1, r, ErrorInfo, false)
            and Skip(',', r, r, ErrorInfo)
         then
         begin
            if (length(r) > 0) and (r[length(r)] = ')')
            then
            Begin
                syntaxcheck := true;
                s2 := copy(r, 1, length(r) - 1);
            end
            else
                ErrorInfo := ErrorRemaining;

           if syntaxCheck then
           Begin
              insertLineIndx := -1;
              PatchListe.ItemPointer := PatchListe.FindFirstItemStartingWith (s0 + '("' + s1 + '"', true, -1);
              while PatchListe.ItemPointer  > -1
              do
              begin
                insertLineIndx := PatchListe.ItemPointer;
                PatchListe.Delete (insertLineIndx);
                PatchListe.ItemPointer := PatchListe.FindFirstItemStartingWith (s0 + '("' + s1 + '"', true, -1);
              end;
              if insertLineIndx > -1
              then
                PatchListe.Insert(insertLineIndx, s0 + '("' + s1  + '", ' + s2 + ');')
              else
                PatchListe.add(s0 + '("' + s1  + '", ' + s2 + ');');
           End;
         End
        End

        else if LowerCase (methodname) = lowercase ('Set_Netscape_User_Pref')
        then
        Begin

         if Skip ('(', r, r, ErrorInfo)
            and GetStringA(r, s1, r, ErrorInfo, false)
            and Skip(',', r, r, ErrorInfo)
         then
            if (length(r) > 0) and (r[length(r)] = ')')
            then
            Begin
              syntaxcheck := true;
              s2 := copy(r, 1, length(r) - 1);
            end
            else
              ErrorInfo := ErrorRemaining;

         if syntaxCheck then
          Begin
            //PatchListe.Sort;   --- entfernt wegen Mozilla-Kommentaren in prefs.js
            //PatchListe.Sorted := true;   --- entfernt wegen Mozilla-Kommentaren in prefs.js
            PatchListe.ItemPointer := PatchListe.FindFirstItemWith (s1, false, -1);
            PatchListe.Delete (PatchListe.ItemPointer);
            PatchListe.Add ('user_pref("' + s1  + '", ' + s2 + ');');
          End;
        End

      else if
        (LowerCase (methodname) = lowercase ('AddStringListElement_To_Mozilla_Pref'))
        then
        Begin

      //purify layout
         while PatchListe.GlobalReplace(1, ' (', '(', true) do;
         while PatchListe.GlobalReplace(1, '( ', '(', true) do;

         if Skip ('(', r, r, ErrorInfo)
          and GetStringA(r, s0, r, ErrorInfo, false)
          and Skip(',', r, r, ErrorInfo)
          and GetStringA(r, s1, r, ErrorInfo, false)
          and Skip(',', r, r, ErrorInfo)
          and GetStringA(r, s2, r, ErrorInfo, false)
          and Skip (')', r, r, ErrorInfo)
          then
            if r = ''
            then
                syntaxcheck := true
            else
                ErrorInfo := ErrorRemaining;


         if syntaxCheck then
         Begin
            insertLineIndx := -1;
            startofline := s0 + '("' + s1 + '"';

            PatchListe.ItemPointer := PatchListe.FindFirstItemStartingWith (startofline, true, -1);
            while PatchListe.ItemPointer  > -1
            // we treat only the last line of this type, eliminating all others
            do
            begin
              insertLineIndx := PatchListe.ItemPointer;
              oldline := PatchListe.Strings[insertLineIndx];
              PatchListe.Delete(insertLineIndx);
              PatchListe.ItemPointer := PatchListe.FindFirstItemStartingWith (startofline, true, -1);
            end;

           if insertLineIndx > -1
           then
           Begin
             syntaxCheck := false;

         if
           skip (startofLine, oldLine, oldLine, ErrorInfo)
          and Skip (',', oldLine, oldLine, ErrorInfo)
          and GetStringA(oldLine, old_s2, oldline, ErrorInfo, false)
          and Skip (')', oldLine, oldLine, ErrorInfo)
             then
              syntaxCheck := true;
           End;
         End;


         if syntaxCheck then
         Begin
           if insertLineIndx > -1
           then
           Begin
             //search if s2 already is an element of existing list

             oldLine := old_s2;

             //get first list element
             GetWord (oldLine, x, oldLine, [',']);
             if (oldLine <> '')
             then // list not finished, delete ','
             System.delete (oldLine, 1,1);

             //first element equal to added string?
             found := (lowerCase (x) = lowerCase (s2));

             while (oldLine <> '') and not found
             do
             Begin
               GetWord (oldLine, x, oldLine, [',']);
               if (oldLine <> '') then  System.delete (oldLine, 1,1);
               if lowerCase (x) = lowerCase (s2)
               then found := true;
             End;

             if found
             then
             //reinsert the old line
               PatchListe.Insert(insertLineIndx,
                  startOfLine + ', "' + old_s2 +'");')
             else
             //append s2
               PatchListe.Insert(insertLineIndx,
                  startOfLine + ', "' + old_s2 + ', ' + s2 + '");');
           End
           else
             PatchListe.Add(
                  startOfLine + ', "' +  s2 + '");');

         End;

        end


        else if
        (LowerCase (methodname) = lowercase ('AddStringListElement_To_Netscape_User_Pref'))
        then
        Begin

         if Skip ('("', r, r, ErrorInfo)
         then
         Begin
           GetWord (r, s1, r, ['"']);
           if Skip ('"', r, r, ErrorInfo)
           then if Skip (',', r, r, ErrorInfo)
           then if Skip ('"', r, r, ErrorInfo)
           then
           Begin
             GetWord (r, s2, r, ['"']);
             if Skip ('"', r, r, ErrorInfo)
             then if Skip (')', r, r, ErrorInfo)
             then
               syntaxCheck := true;
           End;
         End;

         if syntaxCheck then
         Begin
           PatchListe.Sort;
           PatchListe.Sorted := true;
           PatchListe.ItemPointer := PatchListe.FindFirstItemWith (s1, false, -1);

           if PatchListe.ItemPointer > -1
           then
           Begin
             oldLine := PatchListe.Strings[PatchListe.ItemPointer];
             syntaxCheck := false;

             if Skip ('user_pref', oldLine, oldLine, ErrorInfo)
             then if Skip ('("', oldLine, oldLine, ErrorInfo)
             then if Skip (s1, oldLine, oldLine, ErrorInfo)
             then if Skip ('"', oldLine, oldLine, ErrorInfo)
             then if Skip (',', oldLine, oldLine, ErrorInfo)
             then if Skip ('"', oldLine, oldLine, ErrorInfo)
             then
             Begin
               GetWord (oldLine, old_s2, oldLine, ['"']);
               if Skip ('"', oldLine, oldLine, ErrorInfo)
               then if Skip (')', oldLine, oldLine, ErrorInfo)
               then
                 syntaxCheck := true;
             End;
           End;
         End;


         if syntaxCheck then
         Begin
           if PatchListe.ItemPointer > -1
           then
           Begin
             oldLine := old_s2;
             GetWord (oldLine, x, oldLine, [',', '"']);
             if (oldLine <> '') then  System.delete (oldLine, 1,1);

             found := false;
             while (oldLine <> '') and not found
             do
             Begin
               GetWord (oldLine, x, oldLine, [',', '"']);
               if (oldLine <> '') then  System.delete (oldLine, 1,1);
               if lowerCase (x) = lowerCase (s2)
               then found := true;
             End;
             if not found
             then
             Begin
               PatchListe.Delete (PatchListe.ItemPointer);
               PatchListe.Add ('user_pref("' + s1  + '", "' + old_s2 + ',' + s2 + '");');
             End;
           End
           else
             PatchListe.Add ('user_pref("' + s1  + '", "' +  s2 + '");');

         End;

        end

        else
          ErrorInfo := methodname + ' not defined';

      End;

      if syntaxcheck then inc(i);

      if not syntaxCheck
      then
         reportError (Sektion, i, Sektion.strings [i-1], errorinfo);
    End;

    if saveToOriginalFile then PatchListe.SaveToFile (PatchFilename);
    PatchListe.free; PatchListe := nil;

    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
    workingSection.Free;
  end;   //doTextpatchMain

begin
  result := tsrPositive;
  //Filename := ExpandFileName(Filename);

  if not initSection (Sektion, OldNumberOfErrors, OldNumberOfWarnings) then exit;

  if (lowercase (PatchParameter) = lowercase (Parameter_AllNTUserProfiles))
  or flag_all_ntuser
  then
  Begin
    flag_all_ntuser := false;
    ProfileList := getProfilesDirList;
    for pc:= 0 to ProfileList.Count -1 do
    begin
      PatchFilename := sysutils.StringReplace(Filename, '%userprofiledir%', ProfileList.Strings[pc],[rfReplaceAll,rfIgnoreCase]);
      PatchFilename := ExpandFileName(PatchFilename);
      doTextpatchMain(Sektion,ProfileList.Strings[pc]+PathDelim);
    end;
  End
  else
  begin
    if runLoginScripts then
      PatchFilename := sysutils.StringReplace(Filename, '%userprofiledir%', GetUserProfilePath,[rfReplaceAll,rfIgnoreCase])
    else PatchFilename := Filename;
    PatchFilename := ExpandFileName(PatchFilename);
    doTextpatchMain(Sektion,GetUserProfilePath+PathDelim);
  end;

  finishSection (Sektion, OldNumberOfErrors, OldNumberOfWarnings,
                 DiffNumberOfErrors, DiffNumberOfWarnings);

  if ExitOnError and (DiffNumberOfErrors> 0)
  then result := tsrExitProcess;

End;



  function TuibInstScript.doTests (const Sektion: TWorkSection; TestParameter : String)
                       : TSectionResult;
   var
   i : Integer;
   FindResultCode : Integer;
   FileRecord : TSearchRec;
   Command : String='';
   Remaining : String='';
   Filename : String='';
   Comment : String='';

  begin
    result := tsrPositive;

    if not initSection (Sektion, OldNumberOfErrors, OldNumberOfWarnings) then exit;

    i := 1;
    while (i <= Sektion.count) and (result > 0)
    do
    Begin
      if (Sektion.Strings [i-1] = '') or (Sektion.Strings [i-1] [1] = LineIsCommentChar)
      then
       (* continue *)
      else
      Begin
        GetWord (Sektion.Strings[i-1], Command, Remaining, WordDelimiterSet0);
        GetWord (Remaining, Filename, Remaining, WordDelimiterSet0);
        Comment := CutRightBlanks (Remaining);

        if length (Comment)
        > 0
        then
          if   ((Comment [1] = '"') and (Comment [length (Comment)] = '"'))
            or ((Comment [1] = '''') and (Comment [length (Comment)] = ''''))
          then
          Begin
            System.delete (Comment, 1, 1);
            if length (Comment) > 0
            then
            System.delete (Comment, length (Comment), 1)
          End;


        FindResultcode := FindFirst (FileName, faAnyFile, FileRecord);


        if UpperCase (Command) = UpperCase ('ExitOnFileNotExist')
        then Begin

          if FindResultCode < 0 (* not FileExists (FileName) *)
          then
          Begin
            ps := LogDatei.LogSIndent  +
                  'Error: File ' + FileName + ' does not exist';
            LogDatei.log (ps, LLError);
            if (length (Comment) > 0) then
               LogDatei.log (LogDatei.LogSIndentPlus (1) + Comment, BaseLevel);

            result := tsrExitProcess;
          End
          else if FileRecord.Attr and faDirectory = faDirectory
          then
          Begin
            ps := LogDatei.LogSIndent +
                  'File ' + FileName + ' (directory) exists ';
            LogDatei.log (ps, LevelComplete);
          End
          else
          Begin
            ps := LogDatei.LogSIndent +
                  'File ' + FileName + ' exists';
            LogDatei.log (ps, LevelComplete);
          End
        end

        else if UpperCase (Command) = UpperCase ('ExitOnFileZero')
        then Begin
          if FindResultCode < 0 (* not FileExists (FileName) *)
          then
          Begin
            ps :=  LogDatei.LogSIndent +
                   'Error: File ' + FileName + ' does not exist';
            LogDatei.log (ps, LLError);

            if (length (Comment) > 0) then
               LogDatei.log (LogDatei.LogSIndentPlus (1) + Comment,
                BaseLevel);

            result := tsrExitProcess;
          End
          else if FileRecord.Attr and faDirectory = faDirectory
          then
          Begin
            ps := LogDatei.LogSIndent +
                  'File ' + FileName + ' (directory) exists ';
            LogDatei.log (ps, LevelComplete);
          End
          else if FileRecord.Size = 0
          then
          Begin
            ps := LogDatei.LogSIndent +
                  'Error: File ' + FileName + ' has size 0';
            LogDatei.log (ps, LLError);
            if (length (Comment) > 0) then
               LogDatei.log (LogDatei.LogSIndentPlus (1) + Comment, BaseLevel);
            result := tsrExitProcess;
          End
          else
          Begin
            ps := LogDatei.LogSIndent +
                  'File ' + FileName + ' exists and has not size 0';
            LogDatei.log (ps, LevelComplete);
          End;
        end

        else if UpperCase (Command) = UpperCase ('WarningOnFileNotExist')
        then Begin
          if FindResultCode < 0 (* not FileExists (FileName) *)
          then
          Begin
            ps := LogDatei.LogSIndent +
                  'Warning: File ' + FileName + ' does not exist';
            LogDatei.log (ps, LevelWarnings);
            if (length (Comment) > 0) then
               LogDatei.log (LogDatei.LogSIndentPlus (1) + Comment, LLWarning);
          End
          else if FileRecord.Attr and faDirectory = faDirectory
          then
          Begin
            ps := LogDatei.LogSIndent +
                  'File ' + FileName + ' (directory) exists ';
            LogDatei.log (ps, LevelComplete);
          End
          else
          Begin
            ps := LogDatei.LogSIndent +
                  'File ' + FileName + ' exists';
            LogDatei.log (ps, LevelComplete);
          End;
        end

        else if UpperCase (Command) = UpperCase ('WarningOnFileZero')
        then Begin
          if FindResultCode < 0  (* not FileExists (FileName) *)
          then
          Begin
            ps := LogDatei.LogSIndent +
                  'Warning: File ' + FileName + ' does not exist';
            LogDatei.log (ps, LLWarning);
            if (length (Comment) > 0) then
            Begin
               ps := LogDatei.LogSIndentPlus (1) + Comment;
               LogDatei.log (ps, LevelWarnings);
            End;
          End
          else if FileRecord.Attr and faDirectory = faDirectory
          then
          Begin
            ps := LogDatei.LogSIndent +
                  'File ' + FileName + ' (directory) exists ';
            LogDatei.log (ps, LevelComplete);
          End

          else if FileRecord.Size = 0
          then
          Begin
            ps := LogDatei.LogSIndent +
                  'Warning: File ' + FileName + ' has size 0';
            LogDatei.log (ps, LLWarning);
            if (length (Comment) > 0) then
            Begin
               ps := LogDatei.LogSIndentPlus (1) + Comment;
               LogDatei.log (ps, LevelWarnings);
            End;
          End
          else
          Begin
            ps := LogDatei.LogSIndent +
                  'File ' + FileName + ' exists and has not size 0';
            LogDatei.log (ps, LevelComplete);
          End;
        end

        else
          reportError (Sektion, i, Command, 'nicht definiert');;

        sysutils.findclose (FileRecord);
      end;

      inc (i);
    End;

    finishSection (Sektion, OldNumberOfErrors, OldNumberOfWarnings,
                   DiffNumberOfErrors, DiffNumberOfWarnings);

    if ExitOnError and (DiffNumberOfErrors > 0)
    then result := tsrExitProcess;
  end;



function TuibInstScript.doInifilePatches (const Sektion: TWorkSection; Filename : string; PatchParameter : String)
                  : TSectionResult;
var
  pc: Integer=0;
  Befehlswort : String='';
  Rest : String='';
  Bereich : String='';
  Eintrag : String='';
  AlterEintrag : String='';
  /// Value : String;
  Patchdateiname : String='';
  Patchdatei : TuibPatchIniFile;
  ErrorInfo : String='';
  ProfileList : TStringlist;

  procedure doInifilePatchesMain;
  var
  i: Integer=0;
  dummy : string;
  mytxtfile : Tstringlist;
  begin
    //ps := LogDatei.LogSIndentPlus (+3) + 'FILE ' +  PatchdateiName;
    //LogDatei.log (ps, LevelWarnings);
    Logdatei.log('', LLInfo);
    Logdatei.log('Patching: '+PatchdateiName, LLInfo);


    if not FileExists (PatchdateiName)
    then
    Begin
      ps := LogDatei.LogSIndentPlus (+3) + 'Info: This file does not exist and will be created ';
      LogDatei.log (ps, LLInfo);
      LogDatei.NumberOfHints := Logdatei.NumberOfHints + 1;

      if CreateTextfile (PatchdateiName, ErrorInfo)
      then
      Begin
       if ErrorInfo <> ''
       then
       Begin
         ps := LogDatei.LogSIndentPlus (+3) + 'Warning: ' + ErrorInfo;
         LogDatei.log (ps, LLWarning);
       End;
      End
      else
      Begin
         ps := LogDatei.LogSIndentPlus (+3) + 'Error: ' + ErrorInfo;
         LogDatei.log (ps, LLError);
         exit; // ------------------------------  exit
       End;
    End;


    ProcessMess;

    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

    // jetzt Arbeitsstruktur erzeugen

    Patchdatei := TuibPatchIniFile.Create;
    //mytxtfile := TStringlist.Create;


    Patchdatei.clear;
    if FileExists (PatchdateiName)
    then
      mytxtfile := LoadFromFileWithEncoding(ExpandFileName(PatchdateiName),flag_encoding);
      //Patchdatei.LoadFromFile  (ExpandFileName(PatchdateiName));
      Patchdatei.text :=  mytxtfile.Text;
    //Patchdatei.text := reencode(Patchdatei.Text, flag_encoding,dummy,'system');
      //Patchdatei.text := reencode(Patchdatei.Text, flag_encoding,dummy,system);
      for i:= 0 to Patchdatei.Count -1 do
        logdatei.log_prog('Loaded: '+Patchdatei.Strings[i],LLDebug);

    for i:=1 to Sektion.count
    do
    Begin
      if (Sektion.strings [i-1] = '') or (Sektion.strings [i-1] [1] = LineIsCommentChar)
      then
       (* continue *)
      else
      Begin
        WortAbspalten (cutLeftBlanks(Sektion.strings [i-1]), Befehlswort, Rest);

        if UpperCase (Befehlswort) = 'ADD'
        then Begin
          SectionnameAbspalten (Rest, Bereich, Eintrag);
          Patchdatei.addEntry (Bereich, Eintrag);
        End
        else if UpperCase (Befehlswort) = 'DEL'
        then Begin
          SectionnameAbspalten (Rest, Bereich, Eintrag);
          Patchdatei.delEntry (Bereich, Eintrag);
        End
        else if UpperCase (Befehlswort) = 'SET'
        then Begin
          SectionnameAbspalten (Rest, Bereich, Eintrag);
          Patchdatei.setEntry  (Bereich, Eintrag);
        End
        else if UpperCase (Befehlswort) = 'ADDNEW'
        then Begin
          SectionnameAbspalten (Rest, Bereich, Eintrag);
          Patchdatei.addNewEntry (Bereich, Eintrag);
        End
        else if UpperCase (Befehlswort) = 'DELSEC'
        then begin
          SectionnameAbspalten (Rest, Bereich, Eintrag);
          if Eintrag <> '' then
            reportError (Sektion, i, Sektion.strings [i-1], 'syntax error')
          else
            Patchdatei.delSec (Bereich);
        end
        else if UpperCase (Befehlswort) = 'CHANGE'
        then Begin
          SectionnameAbspalten (Rest, Bereich, Eintrag);
          Patchdatei.changeEntry (Bereich, Eintrag);
        End
        else if UpperCase (Befehlswort) = 'REPLACE'
        then Begin
          WortAbspalten (Rest, AlterEintrag, Eintrag);
          if pos (' ', eintrag) > 0
          then
            reportError (Sektion, i, Sektion.strings [i-1], 'replace string not identified ')
          else
            Patchdatei.replaceEntry (AlterEintrag, Eintrag);
        End
        else
          reportError (Sektion, i, Befehlswort, ' Operation not defined');
      End;
    End;
    Logdatei.log('--- ', LLInfo);
    //Patchdatei.Text:= reencode(Patchdatei.Text, 'system',dummy,flag_encoding);
    if not ((flag_encoding = 'utf8')  or (flag_encoding = 'UTF-8')) then
    begin
      //mytxtfile.Text := reencode(Patchdatei.Text, 'utf8',dummy,flag_encoding);
      //mytxtfile.SaveToFile(PatchdateiName);
      Patchdatei.SaveToFile (PatchdateiName,flag_encoding);
    end
    else
    begin
      Patchdatei.SaveToFile (PatchdateiName,'utf8');

    end;
    for i:= 0 to Patchdatei.Count -1 do
        logdatei.log_prog('Saved: '+Patchdatei.Strings[i],LLDebug);
    Patchdatei.free; Patchdatei := nil;
  end;

begin
  result := tsrPositive;
  //Filename := ExpandFileName(Filename);

  if not initSection (Sektion, OldNumberOfErrors, OldNumberOfWarnings) then exit;

  if flag_all_ntuser then
  Begin
    flag_all_ntuser := false;
    ProfileList := getProfilesDirList;
    for pc:= 0 to ProfileList.Count -1 do
    begin
      PatchdateiName := sysutils.StringReplace(Filename, '%userprofiledir%', ProfileList.Strings[pc],[rfReplaceAll,rfIgnoreCase]);
      PatchdateiName := ExpandFileName(PatchdateiName);
      doInifilePatchesMain;
    end;
  End
  else
  begin
    if runLoginScripts then
      PatchdateiName := sysutils.StringReplace(Filename, '%userprofiledir%', GetUserProfilePath,[rfReplaceAll,rfIgnoreCase])
    else PatchdateiName := Filename;
    PatchdateiName := ExpandFileName(PatchdateiName);
    doInifilePatchesMain;
  end;

  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;

  finishSection (Sektion, OldNumberOfErrors, OldNumberOfWarnings,
                 DiffNumberOfErrors, DiffNumberOfWarnings);


  if ExitOnError and (DiffNumberOfErrors> 0)
  then result := tsrExitProcess;
end;


function TuibInstScript.doHostsPatch (const Sektion: TWorkSection; HostsFilename : String) : TSectionResult;

  var
  HostsLocation : String;
  Expressionstr, remaining : String;
  oldDisableWow64FsRedirectionStatus: pointer=nil;
  Wow64FsRedirectionDisabled, boolresult : boolean;


  procedure patchHosts (HostsLocation : String; var SectionResult : TSectionResult);

   var
   i: Integer=0;
   HostsImage : TuibPatchHostsFile;
   ///HostsFound : Boolean;
   Ident : String='';
   Hostname : String='';
   ipAdresse : String='';
   Alias : String='';
   ErrorInfo : String='';

  Begin
    HostsLocation := ExpandFileName(HostsLocation);
    ps := LogDatei.LogSIndentPlus (+2)+ 'FILE ' + HostsLocation ;
    LogDatei.log (ps, LevelWarnings);

    ProcessMess;

    HostsImage := TuibPatchHostsFile.Create;

    if FileExists (HostsLocation)
    then HostsImage.LoadFromFile (HostsLocation)
    else
    Begin
      ps := LogDatei.LogSIndentPlus (+3) + 'Info: This file does not exist and will be created ';
      LogDatei.log (ps, LLinfo);
      LogDatei.NumberOfHints := Logdatei.NumberOfHints + 1;

      if CreateTextfile (HostsLocation, ErrorInfo)
      then
      Begin
       if ErrorInfo <> ''
       then
       Begin
         ps := LogDatei.LogSIndentPlus (+3) + 'Warning: ' + ErrorInfo;
         LogDatei.log (ps, LLWarning);
       End;
      End
      else
      Begin
         ps := LogDatei.LogSIndentPlus (+3) + 'Error: ' + ErrorInfo;
         LogDatei.log (ps, LLError);
         exit; (* ------------------------------  exit *)
       End;
    End;
    HostsImage.Text:= reencode(HostsImage.Text, 'system');
    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

    for i:=1 to Sektion.count
    do
    Begin
      Remaining := cutLeftBlanks(Sektion.strings [i-1]);

      if (Remaining = '') or (Remaining [1] = LineIsCommentChar)
         or (Remaining [1] = '#')
          (* Zeilen, die mit # beginnen, werden in einer HostsPatch-Sektion ebenfalls
             als Kommentar gewertet *)
      then
       (* continue *)
      else
      Begin
        GetWord (Remaining, Expressionstr, Remaining, WordDelimiterSetHosts);

        Hostname := '';
        ipAdresse := '';
        Alias := '';
        Ident := '';

        if UpperCase (Expressionstr) =  UpperCase ('SetAddr') then
        Begin
          GetWord (Remaining, Hostname, ipAdresse, WordDelimiterSetHosts);
          HostsImage.SetAddress (Hostname, ipAdresse);
        End
        else if UpperCase (Expressionstr) =  UpperCase ('SetName') then
        Begin
          GetWord (Remaining, ipAdresse, Hostname, WordDelimiterSetHosts);
          HostsImage.SetName (ipAdresse, Hostname);
        End
        else if UpperCase (Expressionstr) =  UpperCase ('SetAlias') then
        Begin
          GetWord (Remaining, Ident, Alias, WordDelimiterSetHosts);
          HostsImage.SetAlias (Ident, Alias);
        End
        else if UpperCase (Expressionstr) =  UpperCase ('DelAlias') then
        Begin
          GetWord (Remaining, Ident, Alias, WordDelimiterSetHosts);
          HostsImage.DelAlias (Ident, Alias);
        End
        else if UpperCase (Expressionstr) =  UpperCase ('DelHost') then
        Begin
          GetWord (Remaining, Ident, Remaining, WordDelimiterSetHosts);
          HostsImage.DelHost (Ident);
        End
        else if UpperCase (Expressionstr) =  UpperCase ('SetComment') then
        Begin
          GetWord (Remaining, Ident, Remaining, WordDelimiterSetHosts);
          HostsImage.SetComment (Ident, Remaining);
        End

        else
          reportError (Sektion, i, Expressionstr, ' Operation not defined');
      end;
    End;

    try
      HostsImage.SaveToFile (HostsLocation);
    except
      ps  := LogDatei.LogSIndent
             + 'Error: file ' + HostsLocation + ' could not be saved to disk';
      Logdatei.log (ps, LLError);
    end;
    HostsImage.free; HostsImage := nil;

    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
  End;

Begin
  Wow64FsRedirectionDisabled := false;
  result := tsrPositive;

  if not initSection (Sektion, OldNumberOfErrors, OldNumberOfWarnings) then exit;

  if HostsFilename <> ''
  then
    patchHosts (HostsFilename, result)
  else
  Begin
    {$IFDEF WIN32}
    if Is64BitSystem then
    Begin
      boolresult := DSiDisableWow64FsRedirection(oldDisableWow64FsRedirectionStatus);
      Wow64FsRedirectionDisabled := true;
    End
    else Wow64FsRedirectionDisabled := false;
    {$ENDIF}
    HostsLocation := DefaultHosts;
    if FileExists (HostsLocation) then patchHosts (HostsLocation, result);
    {$IFDEF WIN32}
    if Wow64FsRedirectionDisabled then
    Begin
      boolresult := DSiRevertWow64FsRedirection(oldDisableWow64FsRedirectionStatus);
      Wow64FsRedirectionDisabled := false;
    End;
    {$ENDIF}

  End;

  finishSection (Sektion, OldNumberOfErrors, OldNumberOfWarnings,
                 DiffNumberOfErrors, DiffNumberOfWarnings);

  if ExitOnError and (DiffNumberOfErrors> 0)
  then result := tsrExitProcess;
end;


function TuibInstScript.doIdapiConfig (const Sektion : TWorkSection; IdapiParameter : String)
                                                                 : TSectionResult;

  begin
    // is not supported any more
    result :=  -1;
  end;


function tlcrToStr(x : TLDAPsearchCacheRequest) : String;
begin
  case x of
    tlcrNone    : result := '';
    tlcrCache   : result := 'Cache';
    tlcrCached  : result := 'Cached';
    tlcrClear   : result := 'Free';
  end;
end;

function tlorToStr(x : TLDAPsearchOutputRequest) : String;
begin
  case x of
    tlorNone        : result := '';
    tlorObjects     : result := 'Objects';
    tlorAttributes  : result := 'Attributes';
    tlorValues      : result := 'Values';
  end;
end;

function produceLDAPsearchParameters
    (Const LDAPparameter : String;
    var cacheRequest   : TLDAPsearchCacheRequest;
    var outputRequest  : TLDAPsearchOutputRequest;
    var errorinfo : String)
    : boolean;

Var
  remainingParameter : String='';
  checkParameter : String='';
  syntaxcheck : Boolean;

begin
  cacheRequest := tlcrNone;
  outputRequest := tlorNone;
  syntaxcheck := true;

  remainingParameter := LDAPParameter;
  while syntaxcheck and (length(remainingParameter) > 0)
  do
  begin
    getWord(remainingParameter, checkParameter, remainingParameter, worddelimiterWhiteSpace);

    if skip('/', checkParameter, checkParameter, errorinfo)
    then
    begin

      if lowercase(checkParameter) = lowercase(tlcrToStr(tlcrCache))
      then
        cacheRequest := tlcrCache
      else if lowercase(checkParameter) = lowercase(tlcrToStr(tlcrCached))
      then
        cacheRequest := tlcrCached
      else if lowercase(checkParameter) = lowercase(tlcrToStr(tlcrClear))
      then
        cacheRequest := tlcrClear

      else if lowercase(checkParameter) = lowercase(tlorToStr(tlorObjects))
      then
        outputRequest := tlorObjects
      else if lowercase(checkParameter) = lowercase(tlorToStr(tlorAttributes))
      then
        outputRequest := tlorAttributes
      else if lowercase(checkParameter) = lowercase(tlorToStr(tlorValues))
      then
        outputRequest := tlorValues

      else
      Begin
        syntaxcheck := false;
        errorinfo := 'no valid parameter';
      end;

    end
    else
      syntaxcheck := false;

  end;

  result := syntaxcheck;
end;


function TuibInstScript.doLDAPsearch
        (const Sektion : TWorkSection;
        cacheRequest   : TLDAPsearchCacheRequest;
        outputRequest  : TLDAPsearchOutputRequest;
        var output: TXStringList)
        : TSectionResult;

// uses ldapsend
Var
  targethost : String;
  targetport : String;
  ldapsearch_user: String;
  ldapsearch_credentials: String;
  ldapsearch_dn : String;
  ldapsearch_typesOnly : boolean;
  ldapsearch_filter : String;
  ldapsearch_attributes : TStringList;


  i : Integer;
  savelogsindentlevel : Integer;
  syntaxcheck : boolean;
  errorinfo : String;
  Expressionstr : String;
  remaining : String;
  itemS : String;

  errorOccurred : boolean;

  function analyzeSection : boolean;
  var
    ldapsearch_typesOnlyString : String;
    find : boolean;
    findInfo : String;
  begin
    syntaxCheck := true;

    targethost := '';
    targetport := '';
    ldapsearch_dn := '';
    ldapsearch_typesOnly := false;
    ldapsearch_typesOnlyString := 'false';
    ldapsearch_filter := '(objectclass=*)';
    ldapsearch_attributes := TStringList.Create;

    i := 1;

    while (i <= Sektion.count) and syntaxcheck
    do
    Begin
      Remaining := trim (Sektion.strings [i-1]);
      if (Remaining = '') or (Remaining [1] = LineIsCommentChar)
      then
        inc (i)
      else
      Begin //real line
        logdatei.log('  analyze: ' + Remaining,  LevelComplete);
        GetWord (Remaining, Expressionstr, Remaining, WordDelimiterSet0);

        if (length(Expressionstr) < 2) or (Expressionstr[length(Expressionstr)] <> ':')
        then
        begin
          syntaxcheck := false;
          errorInfo := 'keyname plus ":" expected"';
        end

        else
        begin
          Expressionstr := copy(Expressionstr, 0, length(Expressionstr) -1);
          if LowerCase (Expressionstr) = 'targethost'
          then
          begin
            if (length (Remaining) = 0)
            then
            Begin
              syntaxCheck := false;
              errorinfo := 'expected targethost name';
            End
            else
            begin
              getWordOrStringExpressionstr(remaining, targethost, remaining, errorinfo);
              if length(remaining) > 0
              then
                reportError(Sektion, i, Sektion.Strings[i-1], errorRemaining);

            end;
          End

          else if LowerCase (Expressionstr) = 'targetport'
          then
          begin
            if (length (Remaining) = 0)
            then
            Begin
              //stay to default port
            End
            else
            begin
              getWordOrStringExpressionstr(remaining, targetport, remaining, errorinfo);

              try
                strtoint(targetPort)
              except
                reportError(Sektion, i, Sektion.Strings[i-1], '"' + targetport + '" is no number');
                syntaxcheck := false;
              end;

              if length(remaining) > 0
              then
                reportError(Sektion, i, Sektion.Strings[i-1], errorRemaining);
            end;
          End
          
          else if LowerCase (Expressionstr) = 'user'
          then
          begin
            if (length (Remaining) = 0)
            then
            Begin
              //anonymous logon
            End
            else
            begin
              getWordOrStringExpressionstr(remaining, ldapsearch_user, remaining, errorinfo);

              if length(remaining) > 0
              then
                reportError(Sektion, i, Sektion.Strings[i-1], errorRemaining);
            end;
          End
          
          else if LowerCase (Expressionstr) = 'password'
          then
          begin
            if (length (Remaining) = 0)
            then
            Begin
              //anonymous logon
            End
            else
            begin
              getWordOrStringExpressionstr(remaining, ldapsearch_credentials, remaining, errorinfo);

              if length(remaining) > 0
              then
                reportError(Sektion, i, Sektion.Strings[i-1], errorRemaining);
            end;
          End


          else if LowerCase (Expressionstr) = 'dn'
          then
          begin
            if (length (Remaining) = 0)
            then
            Begin
              //stay to default port
            End
            else
            begin
              getWordOrStringExpressionstr(remaining, ldapsearch_dn, remaining, errorinfo);

              if length(remaining) > 0
              then
                reportError(Sektion, i, Sektion.Strings[i-1], errorRemaining);
            end;
          End

          else if LowerCase (Expressionstr) = 'typesonly'
          then
          begin
            if (length (Remaining) = 0)
            then
            Begin
              //stay to default
            End
            else
            begin
              getWordOrStringExpressionstr(remaining, ldapsearch_typesonlyString, remaining, errorinfo);
              if  lowercase(ldapsearch_typesonlyString) = 'false'
              then
                ldapsearch_typesOnly := false
              else if lowercase(ldapsearch_typesonlyString) = 'true'
              then
                ldapsearch_typesOnly := true
              else
              Begin
                syntaxcheck := false;
                errorinfo := 'only values "true" or "false" possible'
              end;

              if syntaxcheck and (length(remaining) > 0)
              then
                reportError(Sektion, i, Sektion.Strings[i-1], errorRemaining);

            end
          End

          else if LowerCase (Expressionstr) = 'filter'
          then
          begin
            if (length (Remaining) = 0)
            then
            Begin
              //stay to default
            End
            else
            begin
              getWordOrStringExpressionstr(remaining, ldapsearch_filter, remaining, errorinfo);

              if length(remaining) > 0
              then
                reportError(Sektion, i, Sektion.Strings[i-1], errorRemaining);

            end;
          End

          else if LowerCase (Expressionstr) = 'attributes'
          then
          begin
            if (length (Remaining) = 0)
            then
            Begin
              //stay to default
            End
            else
            begin
              // construct string list!
              find := true;
              while find do
              begin
                getWord(Remaining, itemS, Remaining, [',']);
                if (length(itemS) > 0)
                then
                	ldapsearch_attributes.add(itemS);
                Skip(',', Remaining,  Remaining,  findInfo);
                find := length(Remaining) > 0;
                logdatei.log('  attribute:'  + itemS +  ' remaining >' + Remaining +  '<',  LevelComplete);
              end;

            end;
          End

          else
            reportError(Sektion, i, Sektion.Strings[i-1], 'Unknown LDAP search specification');


        end;

        if syntaxcheck then inc(i);

      end; //real line

    end; //while

    if not syntaxcheck then
    begin
      reportError(Sektion, i, Sektion.Strings[i-1], errorinfo)
    end
    else
    begin
      logdatei.log('LDAP search starting for:', LevelComplete);
      logdatei.log('  targethost: ' + targethost, LevelComplete);
      logdatei.log('  targetport: ' + targetport, LevelComplete);
      logdatei.log('  dn: ' + ldapsearch_dn, LevelComplete);
      logdatei.log('  typesOnly: ' + ldapsearch_typesOnlyString, LevelComplete);
      logdatei.log('  filter: ' + ldapsearch_filter,  LevelComplete);
      logdatei.log('  attribute:' + ldapsearch_attributes.text,  LevelComplete);
    end;

    result := syntaxcheck;

  end;

  function callLDAP : boolean;
  var
    ldap: TLDAPsend;
    goOn : boolean;
    l : Integer;
  begin
    ldap:= TLDAPsend.Create;

    goOn := true;
    if (ldapResult <> nil)
    then
    begin
      ldapResult.clear;
      ldapResult.free;
    end;
    ldapResult := TStringList.create;

    try

      try //for catching an exception
        ldap.TargetHost := targethost;
        if targetPort <> ''
        then
          ldap.TargetPort := targetPort;

         ldap.UserName := ldapsearch_user;
         ldap.Password :=  ldapsearch_credentials;

        if not ldap.Login
        then
        Begin
            goOn := false;
            logdatei.log('Error in LDAP login: ' + ldap.ResultString, LLError);
        End;

        if goOn and not ldap.Bind
        then
        Begin
            goOn := false;
            logdatei.log('Error in LDAP login: ' + ldap.ResultString, LLError);
        End;

        if goOn
        then
        Begin
          ldap.Search(ldapsearch_dn,ldapsearch_typesonly,ldapsearch_filter,ldapsearch_attributes);
          ldapResult.setText(PAnsiChar(LDAPResultdump(ldap.SearchResult)));

        (* // check result: *)
        logdatei.log('LDAP search result: ', LevelComplete);
        logdatei.LogSIndentLevel := logDatei.logSIndentLevel + 4;
        for l := 0 to ldapResult.Count-1
        do
        Begin
          logdatei.log(ldapResult.Strings[l], LevelComplete);
        End;
        logdatei.LogSIndentLevel := logDatei.logSIndentLevel - 4;
        logdatei.log('', LevelComplete);
      End;


      ldap.Logout;

      except
        logdatei.log('Error (LDAP): ' + ldap.ResultString, LLError);
        goOn := false;
      end;

    finally

      ldap.Free;
    end;

    result := goOn;

  end; //callLDAP

  function buildOutput: boolean;

  var
    iLines : Integer=0;
    ///iObjects : Integer;
    ///iAttributes : Integer;

    found : boolean;

    errorOccurred : boolean;

    line : String='';
    n : Integer=0;
    rem : String='';
    err : String='';

  begin

    errorOccurred := false;

    if (outputRequest = tlorNone)
    then
    begin
      for iLines := 0 to ldapResult.count - 1
      do
        output.add(ldapResult.strings[iLines])
    end
    else
    begin

      iLines := 0;

      found := false;

      n := ldapresult.Count;
      while not found and (iLines < n)  do
      begin
        line := trim( ldapResult.strings[iLines] ) ;
        if pos('results:', lowercase(line)) > -1
        then found := true;
        inc(iLines);
      end;

      if not found then errorOccurred := true;

      while iLines < n do
      begin
        line := trim( ldapResult.strings[iLines] ) ;
        if (pos('result:', lowercase(line)) = 1) or (trim(line) = '')
        then
        begin
          //ignore
        end
        else
        begin
          rem := line;
          if skip('object:',line, rem, err)
          then
          begin
            if outputRequest = tlorObjects
            then
              output.add(rem);
          end

          else if skip('attribute:',line, rem, err)
          then
          begin
            if outputRequest = tlorAttributes
            then
              output.add(rem);
          end
          else
          begin
             if outputRequest = tlorValues
             then
                output.add(line);

          end;


        end;

        inc(iLines);

      end;

    end;

    result := errorOccurred;

  end;


begin
  result := tsrPositive;

  if Sektion.count = 0 then exit;

  LogDatei.LogSIndentLevel := Sektion.NestingLevel;
  savelogsindentlevel := LogDatei.LogSIndentLevel;

  OldNumberOfErrors := LogDatei.NumberOfErrors;
  OldNumberOfWarnings := LogDatei.NumberOfWarnings;

  ps := '';
  LogDatei.log (ps, LLNotice);
  ps := 'Execution of ' + Sektion.Name;

  if  cacheRequest <> tlcrNone
  then
    ps := ps + '  (CacheOption: ' + tlcrToStr(cacheRequest) + ')';

  if  outputRequest <> tlorNone
  then
    ps := ps + '  (OutputOption: ' + tlorToStr(outputRequest) + ')';


  LogDatei.log (ps, LLNotice);

  if pos (uppercase (PStatNames^ [tsLDAPsearch]), uppercase (Sektion.Name)) > 0 then
    //ps := (* 'Ausfuehrung von ' +  *) copy (Sektion.Name, length (PStatNames^ [tsLDAPsearch]) + 1, length (Sektion.Name));
    ps := Sektion.Name;

  {$IFDEF GUI}
  CentralForm.Label2.caption := ps;
  FBatchOberflaeche.setDetailLabel(ps);
  ProcessMess;
  {$ENDIF GUI}
  LogDatei.LogSIndentLevel := Sektion.NestingLevel + 1;
  //Sektion.eliminateLinesStartingWith (';', false);


  errorOccurred := false;
  syntaxCheck := true;

  output := TXStringList.create;


  if cacheRequest = tlcrClear
  then
  begin
    logdatei.log('Clearing cached LDAP search result', LevelComplete);
    if (ldapResult <> nil)
    then
    begin
      ldapResult.clear;
      ldapResult.free;
      ldapResult := nil;
    end;

  end
  else if cacheRequest = tlcrCached
  then
  begin
    if (ldapResult = nil)
    then
    begin
      reportError(Sektion,0, '(parameter /cached)', 'There was probably no (successful) LDAP search');
      logDatei.log('No cached LDAP search result found', LLError);
      syntaxCheck := false;
      errorOccurred := true;
    end
  end
  else //tlcrNone, tlcrCache
  begin
    syntaxCheck := analyzeSection;
    if syntaxCheck
    then
      errorOccurred := not callLDAP;
      //testLDAP;
  end;

  if syntaxCheck and not errorOccurred and not (cacheRequest = tlcrClear)
  then
    buildOutput;

  if (cacheRequest = tlcrNone) and (ldapResult <> nil)
  then
  begin
    ldapResult.clear;
    ldapResult.free;
    ldapResult := nil;
  end;

  // if no error occurred we have a ldapResult stringlist
  // and may evaluate it

  finishSection (Sektion, OldNumberOfErrors, OldNumberOfWarnings,
                 DiffNumberOfErrors, DiffNumberOfWarnings);

  LogDatei.LogSIndentLevel := savelogsindentlevel;

  if ExitOnError and (DiffNumberOfErrors > 0)
  then result := tsrExitProcess;

end;

{$IFDEF WINDOWS}
function TuibInstScript.doRegistryHackInfSource (const Sektion: TWorkSection; Const RegParameter : String; const flag_force64 : boolean)
                                                               : TSectionResult;
var
  Regist : TuibRegistry;
  i, Startline_of_Command : Integer;

  r : String;
  LineWasComplete : Boolean;
  error : String;
  syntaxCheck : Boolean;

  function InterpretLine (var r  : String) : Boolean;
  var
    key0 : String='';
    key : String='';
    name : String='';
    value : String='';
    regTStr : String='';
    regTInt : LongInt;
    regType :  TuibRegDataType;

  begin
    result := false;
    syntaxCheck := true;

    GetWord (r, key0, r, [',']);
    if not Skip (',"', r, r, error) then Begin Syntaxcheck := false; exit End;
    GetWord (r, key, r, ['"']);
    if not Regist.openkey (key0, key)
    then
    Begin
     error := 'Registry key ' + key0 + '\' + key + ' could not be opened';
     exit;
    End;

    if not Skip ('","', r, r, error) then Begin Syntaxcheck := false; exit End;
    GetWord (r, name, r, ['"']);
    if not Skip ('",', r, r, error) then Begin Syntaxcheck := false; exit End;
    GetWord (r, regTStr, r, [',']);
    if not Skip (',', r, r, error) then Begin Syntaxcheck := false; exit End;

    try
      regTInt :=  StrToInt64 (regTStr);
    except
      reportError (Sektion, i, Sektion.strings [i-1], regTStr + ' is not a number');
      Begin Syntaxcheck := false; exit End;
    end;

    value := '';

    if regTInt = 0 then
    Begin
      regtype := trdString;
      if not Skip ('"', r, r, error) then Begin Syntaxcheck := false; exit End;
      GetWord (r, value, r, ['"']);
    End
    else if regTInt = 65537 then
    Begin
      regtype := trdInteger;
      value := r;
    End
    else if regTInt = 131072
    then
    Begin
      regtype := trdExpandString;
      if not Skip ('"', r, r, error) then Begin Syntaxcheck := false; exit End;
      GetWord (r, value, r, ['"']);
      (* Beispiel: 131072,"C:\Programme\wiz\wiz.exe %1" *)
    End
    else if regTInt = 196609 then
    Begin
      regtype := trdBinary;
      value := r;
    End

    else
    Begin
      Syntaxcheck := false;
      Error := regTStr + ' kein erkannter Registrytyp';
      exit;
    End;

    Regist.writeEntry (name, regtype, value);
    Regist.closeKey;

    result := true;
  end;

begin
  result := tsrPositive;

  if not initSection (Sektion, OldNumberOfErrors, OldNumberOfWarnings) then exit;
  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

  if flag_force64 Then
    Regist := TuibRegistry.create(true)
  else
    Regist := TuibRegistry.create;

  i := 1;
  Startline_of_Command := 1;
  LineWasComplete := true;

  while i <= Sektion.count
  do
  Begin
    if LineWasComplete
    then
    Begin
      r := cutLeftBlanks(Sektion.strings [i-1]);
      Startline_of_Command := i;
    End
    else
      r := r + cutLeftBlanks (Sektion.strings [i-1]);

    if (length (r) > 1) and (r [length (r)] = '\')
    then
    Begin
      system.delete (r, length (r), 1);
      LineWasComplete := false;
    End
    else
      LineWasComplete := true;

    if (r = '') or (r [1] = LineIsCommentChar)
    then
     (* continue *)
    else
    Begin
      if LineWasComplete
      then
        if not InterpretLine (r)
        then
          if SyntaxCheck = false then
           reportError (Sektion, Startline_of_Command, Sektion.strings [i-1], error);
    End;

    inc (i);
  End;

  Regist.free; Regist := nil;

  finishSection (Sektion, OldNumberOfErrors, OldNumberOfWarnings,
                 DiffNumberOfErrors, DiffNumberOfWarnings);


  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;

  if ExitOnError and (DiffNumberOfErrors> 0)
  then result := tsrExitProcess;
end;

function TuibInstScript.doRegistryHackRegeditFormat
                                       (const Sektion: TWorkSection;
                                        Const RegParameter : String;
                                        const flag_force64 : boolean)
                                                               : TSectionResult;
var
  Regist : TuibRegistry;
  keyOpenCommandExists : Boolean;
  keyOpened : Boolean;
  regtype : TuibRegDataType;
  ///oldregtype : TuibRegDataType;
  ///Separator : Char;
  ErrorInfo : String='';
  SyntaxCheck : Boolean;
  Expressionstr : String='';
  (* wenn die globale Variable Expressionstr hier nicht lokal neu angelegt wird,
     so verliert der Parameter Sectionname, der ein Zeiger auf die globale Variable Expressionstr ist,
     zusammen mit dieser seinen Wert *)
    r : String='';
  i : integer=0;
  k : Integer=0;
  StartIndentLevel : Integer;

  registrykeys : TXStringList;
  entries : TXStringList;

  basekey : String='';
  sectionname : String='';
  key : String='';
  key0 : String='';
  key_completepath : String='';
  startlineno : Integer=0;

  field : String='';
  typeinfo : String='';
  value : String='';
  ///test : Integer;
  outkey: HKEY;

  function GetTypedValue_RegEditSyntax
    (s, regtypeinfo : String; var regtype : TuibRegDataType; var value, r, ErrorInfo : String) : Boolean;

   var
     ///PartValue,Info : String;
     ///Continue : Boolean;

     binValue : String='';
     lpdata : Pointer=nil;
     cbdata : DWord=0;

     j : Integer=0;
     startvalue : String='';

  Begin
    result := true;
    value := '';

    if Skip (':', s, r, ErrorInfo)
    then
    Begin
      if (upperCase (regtypeinfo) = 'REG_SZ') or (upperCase (regtypeinfo) = uppercase ('hex(0)'))
       // hex(0) stands for REG_NONE 'no defined value type'
      then regtype := trdString
      else
      if upperCase (regtypeinfo) = 'REG_EXPAND_SZ'
      then regtype := trdExpandString
      else
      //if upperCase (regtypeinfo) = 'REG_MULTI_SZ'
      //then regtype := trdMultiString
      //else
      if     (upperCase (regtypeinfo) = uppercase ('hex'))
          or (upperCase (regtypeinfo) = uppercase ('hex(7)'))
      then regtype := trdBinary
      else
      if uppercase (regtypeinfo) =  uppercase ('dword')
      then regtype := trdInteger
      else
      Begin
        result := false;
        ErrorInfo := regtypeinfo + ' is a not supported type of a registry entry';
        exit;
      End
    End
    else
    Begin
      if regtypeinfo = ''
      then
      Begin
        ErrorInfo := ''; (* zuruecksetzen, da noch Hoffnung auf Korrektheit *)
        regtype := trdString;
      End
      else
      begin
        result := false;
        exit;
      end;
    End;


    result := false;
    case regtype of
      trdString :
      begin

          if skip ('"', r, r, errorInfo)
          then
          Begin

            value := '';
            startvalue := r;
            j := 1;
            while j < length (startvalue)
            do
            Begin
             if (startvalue[j] = '\')  and (j < length(startvalue))
             then
               // backquoted char
               inc (j);
             value := value + startvalue[j];
             inc (j);

            end;
            r := '';

            if startvalue[j] = '"'
            then
              result := true
            else
              errorInfo := '""" expected';

          end;
      end;
      trdExpandString :
          if GetString (r, value, r, errorinfo, true) then result := true;
      trdMultiString :
          if GetString (r, value, r, errorinfo, true)
          then
          Begin
             result := true;
             value := value + MultiszVisualDelimiter + MultiszVisualDelimiter;
             value := StringReplace (value, MultiszVisualDelimiter, #10);
          End;
      trdInteger:
          try
            r := '$'+r;
            StrToInt64 (r);
            value := r;
            r := '';
            result := true;
          except
            on EConvertError do
               ErrorInfo := r + ' is no valid number';
            on E: Exception do
               ErrorInfo := E.Message;
          end;
      trdBinary :
          Begin
             binValue := StringReplace(r, ',', ' ');

             SetLength (binValue, length (r) + 1);
             if StrToBinary (PChar (binValue), lpdata, cbdata)
             then
             Begin
               result := true;
               Value := r;
               r := '';
             End
             else
               ErrorInfo :=  '"' + r + '" is not a binary or hex value ';
          End;
    end;

  End;


begin
  try
    basekey := RegParameter;
    result := tsrPositive;

    if not initSection (Sektion, OldNumberOfErrors, OldNumberOfWarnings) then exit;
    StartIndentLevel := LogDatei.LogSIndentLevel;


    if flag_force64 Then
      Regist := TuibRegistry.create(true)
    else
      Regist := TuibRegistry.create;
    keyOpened := false;
    keyOpenCommandExists := false;



    // Sektion has ini file format, therefore we may do

    registrykeys := TXStringlist.create;
    Sektion.GetSectionTitles(registrykeys);

    for k := 0 to registrykeys.Count - 1
    do
    Begin
      sectionname := registrykeys.strings[k];
      sectionname := copy (sectionname, 2, length (sectionname) -2);
      key := sectionname;

      LogDatei.LogSIndentLevel  := startindentlevel + 1;

      LogDatei.log('key ' + key, LLDebug);

      key_completepath := key;
      LogDatei.log('Key is: '+key, LLdebug2);
      GetWord (key, key0, key, ['\']);
      LogDatei.log('Key0 is: '+key0, LLdebug2);
      if Is64BitSystem and ('software' = lowerCase(key0)) then
      begin
        LogDatei.log('key starting with software on 64 bit. Key is: '+key, LLdebug);
        if not flag_force64 then
        begin
          key := key0 + '\wow6432node' + key;
          LogDatei.log('key starting with software on 64 bit has to be redirected. So we work with: '+key, LLdebug);
        end
        else
        begin
          key := key_completepath;
          LogDatei.log('key starting with software on 64 bit. Redirection is disabled. So we work with: '+key, LLdebug);
        end
      end
      else
      if (flag_all_ntuser or flag_ntuser or flag_all_usrclass or runLoginScripts) and (('HKEY_CURRENT_USER' = UpperCase(key0)) or ('HKCU' = UpperCase(key0))) then
      begin
        // remove HKCU from the beginning
        key := key;
        LogDatei.log ('Running loginscripts: ignoring key0 : '+key0 + ', using only key : '+key, LLdebug2);
      end
      else  key := key_completepath;
      GetWord (key, key0, key, ['\']);
      if GetHKEY(key0, outkey) then
      begin
         LogDatei.log ('key0 : '+key0+' is a valid base key and we will use it',LLDebug2);
         key := key_completepath;
      end
      else
      begin
        LogDatei.log('Key0 is: '+key0+ ' Key is: '+key, LLdebug2);
        LogDatei.log ('key0 : '+key0+' is not a valid base key and we will praefix it with: '+basekey,LLDebug2);
        // insert RegParameter before key
        if basekey <> '' then
        Begin
          if basekey [length (basekey)] = '\' then basekey := copy(basekey,1,length(basekey)-1);
          key_completepath := basekey;
          LogDatei.log('key_completepath1 : '+key_completepath, LLdebug3);
          if (trim(key0) <> '') then
          begin
            if key0[1] = '\' then key0 := copy(key0,2,length(key0));
            if key0 [length (key0)] = '\' then key0 := copy(key0,1,length(key0)-1);
            key_completepath := key_completepath + '\' + key0;
            LogDatei.log('key_completepath2 : '+key_completepath, LLdebug3);
          end;
          if (trim(key) <> '') then
          begin
            if key[1] = '\' then key := copy(key,2,length(key));
            key_completepath := key_completepath + '\' + key;
            LogDatei.log('key_completepath3 : '+key_completepath, LLdebug3);
          end;
        End;
      end;

       // extract HKEY
       GetWord (key_completepath, key0, key, ['\']);
       system.delete(key,1,1);
       LogDatei.log('We will open Key : '+key_completepath, LLdebug2);


      LogDatei.LogSIndentLevel  := startindentlevel + 2;
      Regist.BaseIndentLevel := startindentlevel + 2;

      // close key before opening the next -> regunload will fail if keys are open
      if keyOpened then keyOpened := not Regist.CloseKey;
      keyOpened := Regist.OpenKey (key0, key);


      entries := TXStringList.Create;

      sektion.GetSectionLines (sectionname, entries, startlineno, false, false, true);

      i := 0;
      while i < entries.count do
      begin

        r := cutLeftBlanks(entries.strings[i]);
        while (r[length(r)] = '\') and (i < entries.count - 1)
        do // join lines
        begin
           inc (i);
           r := copy (r, 1, length(r) - 1) + cutLeftBlanks (entries.strings[i]);
        end;

        LogDatei.log(r, baselevel);

        if (r = '') or (r [1] = LineIsCommentChar)
        then
         (* continue *)
        else
        Begin
          syntaxcheck := true;

          if skip ('@', r, r, ErrorInfo)
          //in Regedit-Syntax, a '@' stands for the default variable ("") of the key
          then
          begin
            field := '';
          end

           // the following is the set command in the registry section according to the winst syntax
          else if Skip ('"', r, r, ErrorInfo)
          then
          Begin
            GetWord (r, field, r, ['"']);
            if not Skip ('"', r, r, ErrorInfo)
            then syntaxcheck := false;
          end

          else
            syntaxcheck := false;

          if syntaxcheck
          then
          Begin
            syntaxcheck := Skip ('=', r, r, ErrorInfo);
            if syntaxcheck
            then
            Begin
              GetWord (r, typeinfo, r, [':', '"']);

              syntaxcheck := GetTypedValue_RegEditSyntax (r, typeinfo, regtype, value, r, ErrorInfo);
              if syntaxcheck and (r <> '')
              then begin SyntaxCheck := false; ErrorInfo := ErrorRemaining end;
            end;

          End;


          if SyntaxCheck
          then
            Regist.WriteEntry (field, regtype, value)
          else
             reportError (Sektion, i, entries.strings [i], ErrorInfo);

          inc (i);
        end;
      end;

    End;


    Regist.free; Regist := nil;

    finishSection (Sektion, OldNumberOfErrors, OldNumberOfWarnings,
                   DiffNumberOfErrors, DiffNumberOfWarnings);

    LogDatei.LogSIndentLevel := StartIndentLevel;
    //LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;

    if ExitOnError and (DiffNumberOfErrors> 0)
    then result := tsrExitProcess;
  except
    on e: Exception do
    begin
      LogDatei.log('Error in doRegistryHackRegeditFormat: '+ e.message,LLError);
    end;
  end;
end;


function TuibInstScript.doRegistryHack (const Sektion: TWorkSection; Const RegParameter : String; const flag_force64 : boolean)
                 : TSectionResult;
var
  Regist : TuibRegistry;
  keyOpenCommandExists : Boolean;
  keyOpened : Boolean;
  basekey : String='';
  key0 : String='';
  key : String='';
  key_completepath : String='';
  templatekey : String='';
  field : String='';
  value : String='';
  oldvalue : String='';
  typeinfo : String='';
  path : String='';
  textfile : TStringList;
  filename : String='';
  regtype, oldregtype : TuibRegDataType;
  Separator : Char;
  ErrorInfo : String='';
  citmark : Char;
  SyntaxCheck : Boolean;
  Expressionstr : String='';
  (* wenn die globale Variable Expressionstr hier nicht lokal neu angelegt wird,
     so verliert der Parameter Sectionname, der ein Zeiger auf die globale Variable Expressionstr ist,
     zusammen mit dieser seinen Wert *)
  r : String='';
  i : Integer=0;
  varno : Integer=0;
  varlist : TStringList;

  StartIndentLevel : Integer=0;

  subkeys : TXStringList;
  regpath : String='';
  j : Integer;
  commandline : String='';
  report : String='';
  output: TXStringList;
  outputlines : integer=0;
  outkey: HKEY;
  p1,p2,p3,p4 : integer;



  function GetTypedValue
    (s, regtypeinfo : String; var regtype : TuibRegDataType; var value, r, ErrorInfo : String) : Boolean;

   var
     ///PartValue, Info : String;
     ///Continue : Boolean;

     binValue : String='';
     lpdata : Pointer=nil;
     cbdata : DWord=0;


  Begin
    result := true;
    value := '';

    if Skip (':', s, r, ErrorInfo)
    then
    Begin
      if upperCase (regtypeinfo) = 'REG_SZ'
      then regtype := trdString
      else if upperCase (regtypeinfo) = 'REG_EXPAND_SZ'
      then regtype := trdExpandString
      else if upperCase (regtypeinfo) = 'REG_MULTI_SZ'
      then regtype := trdMultiString
      else if upperCase (regtypeinfo) = uppercase ('REG_Binary')
      then regtype := trdBinary
      else if uppercase (regtypeinfo) =  uppercase ('REG_DWord')
      then regtype := trdInteger
      //else if uppercase (regtypeinfo) =  uppercase ('REG_NONE')
      //then regtype := trdUnknown
      else
      Begin
        result := false;
        ErrorInfo := regtypeinfo + ' is a unknown or not implemented registry type';
        exit;
      End
    End
    else
    Begin
      if regtypeinfo = ''
      then
      Begin
        ErrorInfo := ''; (* zuruecksetzen, da noch Hoffnung auf Korrektheit *)
        regtype := trdDefaultString;
      End
      else
      begin
        result := false;
        exit;
      end;
    End;


    result := false;
    case regtype of
      trdDefaultString  :
          if GetString (r, value, r, errorinfo, true, true) then result := true;
      trdString :
          if GetString (r, value, r, errorinfo, true, true) then result := true;
      trdExpandString :
          if GetString (r, value, r, errorinfo, true, true) then result := true;
      trdMultiString :
          if GetString (r, value, r, errorinfo, true, true)
          then
          Begin
             result := true;
             value := value + MultiszVisualDelimiter + MultiszVisualDelimiter;
             value := StringReplace (value, MultiszVisualDelimiter, #10);
          End;
      trdInteger:
          try
            StrToInt64 (r);
            Value := r;
            r := '';
            result := true;
          except
            on EConvertError do
               ErrorInfo := r + ' is no valid number';
            on E: Exception do
               ErrorInfo := E.Message;
          end;
      trdBinary :
          Begin
             binValue := r;
             SetLength (binValue, length (r));
             if StrToBinary (PChar (binValue), lpdata, cbdata)
             then
             Begin
               result := true;
               Value := r;
               r := '';
             End
             else
               ErrorInfo :=  '"' + r + '" is no valid binary or hex value';
          End;
      //trdUnknown :
      //    begin
      //      result := true;
      //    end;

    end;

  End;

begin
  basekey := RegParameter;
  result := tsrPositive;

  if not initSection (Sektion, OldNumberOfErrors, OldNumberOfWarnings) then exit;
  StartIndentLevel := LogDatei.LogSIndentLevel;
  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
  if flag_force64 Then
    Regist := TuibRegistry.create(true)
  else
    Regist := TuibRegistry.create;
  keyOpened := false;
  keyOpenCommandExists := false;

  Regist.BaseIndentLevel := LogDatei.LogSIndentLevel;

  for i:=1 to Sektion.count
  do
  Begin
    r := cutLeftBlanks(Sektion.strings [i-1]);
    if (r = '') or (r [1] = LineIsCommentChar)
    then
     // continue
    else
    Begin
      GetWord (r, Expressionstr, r, WordDelimiterSet1);

      if LowerCase (Expressionstr) = LowerCase ('OpenKey')
      then
      Begin
        if keyOpenCommandExists // i.e., existed already
        then
           //LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1
        else
           keyOpenCommandExists := true;

        SyntaxCheck := false;
        GetWord (r, key, r, [']'], true);
        key := trim(key)+']';
        p1 := pos('[',key);
        p2 := posFromEnd(']',key);
        p3 := length(key);
        p4 := length(trim(key));
        if not((pos('[',key) = 1) and (posFromEnd(']',key) = length(key))) then
        begin
          SyntaxCheck := false;
          ErrorInfo := 'Wrong Key Format: Key must be given inside [] - but we got: '+key
        end
        else
        begin
          key := opsiUnquotestr2(trim(key),'[]');
          if (pos('[',key) = 1) and (posFromEnd(']',key) = length(key)) then
          begin
            SyntaxCheck := false;
            ErrorInfo := 'Wrong Key Format: Have still brackets after removing them: '+key
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
            SyntaxCheck := true;
            key_completepath := key;
            LogDatei.log('Key is: '+key, LLdebug);
            GetWord (key, key0, key, ['\']);
            LogDatei.log('Key0 is: '+key0, LLdebug2);
            if Is64BitSystem and ('software' = lowerCase(key0)) then
            begin
              LogDatei.log('key starting with software on 64 bit. Key is: '+key, LLdebug);
              if not flag_force64 then
              begin
                key := key0 + '\wow6432node' + key;
                LogDatei.log('key starting with software on 64 bit has to be redirected. So we work with: '+key, LLdebug);
              end
              else
              begin
                key := key_completepath;
                LogDatei.log('key starting with software on 64 bit. Redirection is disabled. So we work with: '+key, LLdebug);
              end
            end
            else
            if (flag_all_ntuser or flag_ntuser or flag_all_usrclass or runLoginScripts) and (('HKEY_CURRENT_USER' = UpperCase(key0)) or ('HKCU' = UpperCase(key0))) then
            begin
              // remove HKCU from the beginning
              key := key;
              LogDatei.log ('Running loginscripts: ignoring key0 : '+key0 + ', using only key : '+key, LLdebug2);
            end
            else  key := key_completepath;
            GetWord (key, key0, key, ['\']);
            if GetHKEY(key0, outkey) then
            begin
               LogDatei.log ('key0 : '+key0+' is a valid base key and we will use it',LLDebug2);
               key := key_completepath;
            end
            else
            begin
              LogDatei.log('Key0 is: '+key0+ ' Key is: '+key, LLdebug2);
              LogDatei.log ('key0 : '+key0+' is not a valid base key and we will praefix it with: '+basekey,LLDebug2);
              // insert RegParameter before key
              if basekey <> '' then
              Begin
                if basekey [length (basekey)] = '\' then basekey := copy(basekey,1,length(basekey)-1);
                key_completepath := basekey;
                LogDatei.log('key_completepath1 : '+key_completepath, LLdebug3);
                if (trim(key0) <> '') then
                begin
                  if key0[1] = '\' then key0 := copy(key0,2,length(key0));
                  if key0 [length (key0)] = '\' then key0 := copy(key0,1,length(key0)-1);
                  key_completepath := key_completepath + '\' + key0;
                  LogDatei.log('key_completepath2 : '+key_completepath, LLdebug3);
                end;
                if (trim(key) <> '') then
                begin
                  if key[1] = '\' then key := copy(key,2,length(key));
                  key_completepath := key_completepath + '\' + key;
                  LogDatei.log('key_completepath3 : '+key_completepath, LLdebug3);
                end;
              End;
            end;


           // extract HKEY
           GetWord (key_completepath, key0, key, ['\']);
           system.delete(key,1,1);
           LogDatei.log('We will open Key : '+key_completepath, LLdebug2);
          End;
        End;

        if SyntaxCheck
        then
        Begin
          if keyOpened then keyOpened := not Regist.CloseKey;
          Regist.BaseIndentLevel := StartIndentLevel + 1;
          keyOpened := Regist.OpenKey (key0, key);
          //LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
        End
        else
          reportError (Sektion, i, Sektion.strings [i-1], ErrorInfo);

      End   // openkey

      else if LowerCase (Expressionstr) = LowerCase ('DeleteKey')
      then
      Begin
        SyntaxCheck := false;
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
        GetWord (r, key, r, [']'], true);
        key := trim(key)+']';
        p1 := pos('[',key);
        p2 := posFromEnd(']',key);
        p3 := length(key);
        p4 := length(trim(key));
        if not((pos('[',key) = 1) and (posFromEnd(']',key) = length(key))) then
        begin
          SyntaxCheck := false;
          ErrorInfo := 'Wrong Key Format: Key must be given inside [] - but we got: '+key
        end
        else
        begin
          key := opsiUnquotestr2(trim(key),'[]');
          if (pos('[',key) = 1) and (posFromEnd(']',key) = length(key)) then
          begin
            SyntaxCheck := false;
            ErrorInfo := 'Wrong Key Format: Have still brackets after removing them: '+key
          end
          else
          begin
            SyntaxCheck := true;
            key_completepath := key;
            // extract HKEY
            GetWord (key_completepath, key0, key, ['\']);
            system.delete(key,1,1);

            if (flag_all_ntuser or flag_ntuser or flag_all_usrclass or runLoginScripts) and (('HKEY_CURRENT_USER' = UpperCase(key0)) or ('HKCU' = UpperCase(key0))) then
            begin
              // remove HKCU from the beginning
              key := key;
              LogDatei.log ('Running loginscripts: ignoring key0 : '+key0 + ', using only key : '+key, LLdebug2);
            end
            else  key := key_completepath;
            GetWord (key, key0, key, ['\']);
            if GetHKEY(key0, outkey) then
            begin
               LogDatei.log ('key0 : '+key0+' is a valid base key and we will use it',LLDebug2);
               key := key_completepath;
            end
            else
            begin
              LogDatei.log('Key0 is: '+key0+ ' Key is: '+key, LLdebug2);
              LogDatei.log ('key0 : '+key0+' is not a valid base key and we will praefix it with: '+basekey,LLDebug2);
              // insert RegParameter before key
              if basekey <> '' then
              Begin
                if basekey [length (basekey)] = '\' then basekey := copy(basekey,1,length(basekey)-1);
                key_completepath := basekey;
                LogDatei.log('key_completepath1 : '+key_completepath, LLdebug3);
                if (trim(key0) <> '') then
                begin
                  if key0[1] = '\' then key0 := copy(key0,2,length(key0));
                  if key0 [length (key0)] = '\' then key0 := copy(key0,1,length(key0)-1);
                  key_completepath := key_completepath + '\' + key0;
                  LogDatei.log('key_completepath2 : '+key_completepath, LLdebug3);
                end;
                if (trim(key) <> '') then
                begin
                  if key[1] = '\' then key := copy(key,2,length(key));
                  key_completepath := key_completepath + '\' + key;
                  LogDatei.log('key_completepath3 : '+key_completepath, LLdebug3);
                end;
              End;
            end;

            // extract HKEY
            GetWord (key_completepath, key0, key, ['\']);
            system.delete(key,1,1);
            LogDatei.log('We will open Key : '+key_completepath, LLdebug2);
          end;
        end;

        if Is64BitSystem and (GetNTVersionMajor < 6) and regist.myforce64 then
        begin
          // Workaround for RegDeleteKey 64 Bit problem
         //errorcode := RegDeleteKeyEx(mykey, PChar(keytoopen), KeyOpenMode,0);
          output := TXStringList.create;
          commandline := 'cmd64.exe /c "reg delete ""'+key_completepath+'"" /f"';
          LogDatei.log ('Executing ' + commandline, LLDebug);
          if not RunCommandAndCaptureOut(commandline,true,output, report, SW_SHOWMINIMIZED, FLastExitCodeOfExe) then
          Begin
            ps := 'Error: ' + Report;
            LogDatei.log(ps, LLcritical);
            FExtremeErrorLevel := LevelFatal;
            scriptstopped := true;
          End
          else
          Begin
            LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 6;
            LogDatei.log ('', LLDebug);
            LogDatei.log ('output:', LLDebug);
            LogDatei.log ('--------------', LLDebug);
            for outputlines := 0 to output.count-1 do
            begin
              LogDatei.log (output.strings[outputlines], LLDebug);
            end;
            LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 6;
            LogDatei.log ('', LLDebug);
            output.Free;
          end
        end
        else // workaround
        begin
          if SyntaxCheck then
          Begin
            if keyOpened then keyOpened := not Regist.CloseKey;
            subkeys := TXStringList.create;
            stringsplit ( key_completepath, '\', subkeys);
            if subkeys.count <= 1
            then
            Begin
              SyntaxCheck := false;
              ErrorInfo := 'No deleting on root level of registry'
            end
          End;

          if Syntaxcheck
          then
          Begin
              regpath := subkeys[1];
              for j := 2 to subkeys.count - 1
              do regpath := regpath + '\' + subkeys[j];
              Regist.BaseIndentLevel := StartIndentLevel + 1;
              Regist.DeleteKey (subkeys[0], regpath);
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
          End;
        end; // workaround


        if not syntaxcheck
        then
          reportError (Sektion, i, Sektion.strings [i-1], ErrorInfo);

      End

      else
      if not keyOpenCommandExists
      then
        reportError (Sektion, i, Sektion.strings [i-1], 'OpenKey expected')
      else
      Begin
        SyntaxCheck := false;
        if LowerCase (Expressionstr) = 'set'
        then
        Begin
          CitMark := '#';
          if   (Skip ('"', r, r, ErrorInfo))
          then
            CitMark := '"'
          else if   (Skip ('''', r, r, ErrorInfo))
          then
            CitMark := ''''
          else
            ErrorInfo := 'Expected "' + 'or ''';

          if CitMark <> '#'
          (* syntax o.k.*)
          then
          Begin
            GetWord (r, field, r, [citMark]);
            if Skip (CitMark, r, r, ErrorInfo)

            then if Skip ('=', r, r, ErrorInfo)
            then
            Begin
              if (length(r) > 0) and (r[1] in ['"',''''])
              then
               typeinfo := ''
              else
               GetWord (r, typeinfo, r, [':']);
              if GetTypedValue (r, typeinfo, regtype, value, r, ErrorInfo)
              then
                if r = '' then SyntaxCheck := true else ErrorInfo := ErrorRemaining;
            End;
          End;
          if SyntaxCheck
          then
            Regist.WriteEntry (field, regtype, value)
          else
            reportError (Sektion, i, Sektion.strings [i-1], ErrorInfo);
        End

        else if LowerCase (Expressionstr) = 'add'
        then
        Begin
          SyntaxCheck := false;

          if Skip ('"', r, r, ErrorInfo)
          then
          Begin
            GetWord (r, field, r, ['"']);
            if Skip ('"', r, r, ErrorInfo)
            then if Skip ('=', r, r, ErrorInfo)
            then
            Begin
              GetWord (r, typeinfo, r, [':', '"']);
              if GetTypedValue (r, typeinfo, regtype, value, r, ErrorInfo)
              then
                if r = '' then SyntaxCheck := true else ErrorInfo := ErrorRemaining;
            End;
          End;
          if SyntaxCheck
          then
          Begin
            if Regist.ReadEntry (field, oldRegType, oldValue)
            then
            Begin
              LogDatei.log ('Info: Variable ' + field + ' exists, '
                         + ' no overwrite. (The value is "' + oldValue + '")', LLinfo );
              LogDatei.NumberOfHints := LogDatei.NumberOfHints + 1;
            End
            else
              Regist.WriteEntry (field, regtype, value)
          End
          else
            reportError (Sektion, i, Sektion.strings [i-1], ErrorInfo);
        End

        else if LowerCase (Expressionstr) = 'supp'
        then
        Begin
          SyntaxCheck := false;
          if Skip ('"', r, r, ErrorInfo)
          then
          Begin
            GetWord (r, field, r, ['"']);
            if Skip ('"', r, r, ErrorInfo)
            then
            Begin
              if (length (r) = 0)
              then ErrorInfo := 'Separator expected'
              else
              Begin
                if r[1] = '"'
                then
                  Separator := #0
                else
                Begin
                  Separator := r[1];
                  Skip (Separator, r, r, ErrorInfo);
                End;
                if Skip ('"', r, r, ErrorInfo)
                then
                Begin
                  GetWord (r, value, r, ['"']);
                  if Skip ('"', r, r, ErrorInfo)
                  then
                    SyntaxCheck := true;
                End;
              End;
            End;
          End;
          if SyntaxCheck
          then
            Regist.SupplementItems (Separator, field, value)
          else
            reportError (Sektion, i, Sektion.strings [i-1], ErrorInfo);
        End

        else if LowerCase (Expressionstr) = LowerCase ('GetMultiSZFromFile')
        then
        Begin
          Syntaxcheck := false;
          if GetString (trim(r), field, r, ErrorInfo, false)
             and
             GetString (trim(r), filename, r, ErrorInfo, false)
          then
            if r = '' then syntaxCheck := true else ErrorInfo := ErrorRemaining;

          if SyntaxCheck
          then
          Begin
            LogDatei.log ('Get value of variable "' + field + '" from file "' + filename + '"',
                                    LevelComplete);
            TextFile := TStringList.create;
            try
               filename := ExpandFileName(filename);
               TextFile.LoadFromFile (filename);
               TextFile.Text:= reencode(TextFile.Text, 'system');
            except
               on e: exception do
               Begin
                  LogDatei.log ('Error: ' + e.message, LLError);
               End;
            End;
            value := TextFile.Text;
            value := StringReplace (value, #13#10, #10);
            Regist.WriteEntry (field, trdMultiString, value);
            TextFile.free;
          End
          else
            reportError (Sektion, i, Sektion.strings [i-1], ErrorInfo);
        End


        else if LowerCase (Expressionstr) = LowerCase ('SaveValueToFile')
          // undocumented
        then
        Begin
          Syntaxcheck := false;
          if GetString (trim(r), field, r, ErrorInfo, false)
             and
             GetString (trim(r), filename, r, ErrorInfo, false)
          then
            if r = '' then syntaxCheck := true else ErrorInfo := ErrorRemaining;


          if SyntaxCheck
          then
          Begin
            LogDatei.log ('Save value of variable "' + field + '" to file "' + filename + '"',
                                    LevelComplete);
            TextFile := TStringList.create;
            TextFile.Text := Regist.ReadString (field);

            try
               TextFile.SaveToFile (filename);
            except
               on e: exception do
               Begin
                  LogDatei.log ('Error: ' + e.message, LLError);
               End;
            End;
            Textfile.free;
          End
          else
            reportError (Sektion, i, Sektion.strings [i-1], ErrorInfo);
        End


        else if LowerCase (Expressionstr) = 'flushkey'
        then
        Begin
           Regist.FlushKey
        End

        else if LowerCase (Expressionstr) = LowerCase ('ReconstructFrom')
        then
        Begin
          SyntaxCheck := false;
          if Skip ('"', r, r, ErrorInfo)
          then
          Begin
            GetWord (r, path, r, ['"']);
            if Skip ('"', r, r, ErrorInfo)
            then
            Begin
              if r = ''
              then SyntaxCheck := true
              else ErrorInfo := 'not interpreted characters after "';
              if SyntaxCheck then Regist.RestoreFromFile (path)
              else reportError (Sektion, i, Sektion.strings [i-1], ErrorInfo);
            End;
          End;
        End

        else if LowerCase (Expressionstr) = LowerCase ('CopySecurityFrom')
        // experimentell
        then
        Begin
          SyntaxCheck := false;
          if Skip ('[', r, r, ErrorInfo)
          then
          Begin
             GetWord (r, TemplateKey, r, [']']);
             if Skip (']', r, r, ErrorInfo)
             then
             Begin
               if r = ''
               then SyntaxCheck := true
               else ErrorInfo := 'not interpreted characters after ]';

             (* fuege RegParameter vor key ein *)
                TemplateKey := basekey + TemplateKey;

             (* hole HKEY aus key heraus *)
               GetWord (TemplateKey, key0, TemplateKey, ['\']);
               System.Delete (TemplateKey, 1,1);
             end;

             if SyntaxCheck
             then Regist.SetSecurity (TemplateKey, key)
             else reportError (Sektion, i, Sektion.strings [i-1], ErrorInfo);
          End;
        End

        else if LowerCase (Expressionstr) = LowerCase ('DeleteVar')
        then
        Begin
           SyntaxCheck := false;
           if Skip ('"', r, r, ErrorInfo)
           then
           Begin
              GetWord (r, field, r, ['"']);
              if Skip ('"', r, r, ErrorInfo)
              then
                if r = ''
                then syntaxCheck :=true
                else ErrorInfo := ErrorRemaining;
           End;

           if SyntaxCheck
           then Regist.DeleteEntry (Field)
           else reportError (Sektion, i, Sektion.strings [i-1], ErrorInfo);
        End

        else if LowerCase (Expressionstr) = LowerCase ('DeleteAllVars')
        then
        Begin

           syntaxcheck := false;

           if r <> ''
           then ErrorInfo := ErrorRemaining

           else
           Begin
             syntaxcheck := true;
             varlist := Regist.GetAllVars;
             for varno := 0 to varlist.count - 1
             do
              Regist.DeleteEntry (varlist.Strings[varno])
           end;
        End

        else
        reportError (Sektion, i, Expressionstr, ' Operation nicht definiert');
      end;
    End;

  End;

  {
  if keyOpenCommandExists (* i.e., existed already *)
  then
    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
  }

  if keyOpened then keyOpened := not Regist.CloseKey;

  Regist.free; Regist := nil;

  finishSection (Sektion, OldNumberOfErrors, OldNumberOfWarnings,
                 DiffNumberOfErrors, DiffNumberOfWarnings);

  LogDatei.LogSIndentLevel := StartIndentLevel;
  //LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;

  if ExitOnError and (DiffNumberOfErrors> 0)
  then result := tsrExitProcess;

end;


function TuibInstScript.doRegistryAllNTUserDats (const Sektion: TWorkSection;
         rfSelected: TRegistryFormat; const flag_force64 : boolean): TSectionResult;

 var
   //SearchPath : String='';
   //SearchRec  : TSearchRec;
   //findresult : Integer=0;

   profilename, profilepath : string;
   ProfileList : TStringList;
   hkulist  : TStringList;
   i : integer;
   pc : integer;
   domain : string = '';
   UserPath : String='';
   aktsidStr : String = '';

   Errorcode  : Integer=0;
   Info : String='';

   StartWithErrorNumbers : integer=0;
   StartWithWarningsNumber: Integer=0;
{$IFDEF WIN32}

 function LoadNTUserDat (const path : String) : Boolean;
  var
   Errorcode                                     : Integer;
 begin
   if not SetProcessPrivilege ('SeSecurityPrivilege') then
     logdatei.log('Error: Could not set process privilege SeSecurityPrivilege in LoadNTUserDat',LLError);
   if not SetProcessPrivilege ('SeRestorePrivilege') then
     logdatei.log('Error: Could not set process privilege SeRestorePrivilege in LoadNTUserDat',LLError);
   if not SetProcessPrivilege ('SeBackupPrivilege') then
     logdatei.log('Error: Could not set process privilege SeBackupPrivilege in LoadNTUserDat',LLError);

   {load the selected ntuser.dat in HKEY_USERS, subkey TempUserRegKey}

   Errorcode:= RegLoadKeyW (HKEY_USERS, PWChar(UnicodeString(TempUserRegKey)), PWChar (UnicodeString(path)));
   if Errorcode = Error_success
   then
   Begin
     LogDatei.log ('"' + path + '" loaded.', LevelComplete);
     result := true;
   End
   else
   Begin
        (* check if NTUser.dat is in use, since the specific user is logged in *)
     LogDatei.log ('Warning: NTUser.dat could not be loaded from path "' + path + '". '
        + 'Code ' + IntToStr(Errorcode) + ': ' +  RemoveLineBreaks(SysErrorMessage(Errorcode)), LLWarning);
     result := false;
   End;
 end;

 procedure workOnNtUserDat;
 begin
     LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;


     case rfSelected of
       trfRegedit: doRegistryHackRegeditFormat (Sektion, 'HKEY_USERS\' + TempUserRegKey, flag_force64);
       trfWinst  : doRegistryHack (Sektion, 'HKEY_USERS\' + TempUserRegKey, flag_force64);
     end;

     LogDatei.log ('', LevelDebug);
     LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;

     ErrorCode := RegFlushKey(HKEY_Users);

     if Errorcode = Error_success
     then
       LogDatei.log ('Flushed', LevelDebug)
     else
     Begin
       LogDatei.log ('Warning: Could not be flushed. Code '
                              + IntToStr(Errorcode) +': ' + RemoveLineBreaks(SysErrorMessage(Errorcode)), LLWarning);
     End;

     Errorcode := RegUnloadKey (HKEY_Users, PChar(TempUserRegKey));
     if Errorcode = Error_success
     then
       LogDatei.log ('Unloaded', LevelDebug)
     else
     Begin
       LogDatei.log ('Warning: Could not be unloaded. Code '
                              + IntToStr(Errorcode) +': ' + RemoveLineBreaks(SysErrorMessage(Errorcode)), LLWarning);
     End;
 end;

 procedure workOnHkuserSid (const name : String);
 var
  sidStr : String='';
 begin
  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

  //sidStr := GetLocalUserSidStr(GetUserNameEx_);
  //LogDatei.log('sidStr :'+sidStr,LLDebug);
  sidStr := GetLocalUserSidStr(name);
  LogDatei.log('sidStr :'+sidStr,LLDebug2);
  sidStr := copy(sidStr,2,length(sidStr)-2);
  LogDatei.log('sidStr :'+sidStr,LLDebug);


   case rfSelected of
     trfRegedit: doRegistryHackRegeditFormat (Sektion, 'HKEY_USERS\' + sidStr, flag_force64);
     trfWinst  : doRegistryHack (Sektion, 'HKEY_USERS\' + sidStr, flag_force64);
   end;

   LogDatei.log ('', LLDebug);
   LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;

   ErrorCode := RegFlushKey(HKEY_Users);

   if Errorcode = Error_success
   then
     LogDatei.log ('Flushed', LevelDebug)
   else
   Begin
     LogDatei.log ('Warning: Could not be flushed. Code '
                            + IntToStr(Errorcode) +': ' + RemoveLineBreaks(SysErrorMessage(Errorcode)), LLWarning);
   End;
 end;

begin
   result := tsrPositive;

   if not initSection (Sektion, StartWithErrorNumbers, StartWithWarningsNumber) then exit;
   LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

   // mount the NTUser.dat in the users profile path , patch them and write them back
   ProfileList := getProfilesDirList;
   for pc:= 0 to ProfileList.Count -1 do
   begin
     profilepath := ProfileList.Strings[pc];
     profilename := ExtractFileName(profilepath);

     LogDatei.log ('', LLInfo);
     LogDatei.log ('Branch: ' + profilename, LLInfo);
     UserPath := profilepath + '\NTUser.dat';
     if FileExists (UserPath)
     then
     Begin
       if not FileGetWriteAccess (UserPath, Info)
       then
       Begin
         LogDatei.log ('Error: ' + Info, LLError);
       End
       else
       Begin
         if Info <> ''
         then
         Begin
           LogDatei.log ('Warning: ' + Info, LLWarning);
         End;
          if LoadNTUserdat (UserPath) then
          begin
              workOnNtUserDat;
          end
          else
          begin
            if (GetUserNameEx_ <> '') then
            begin
              hkulist := Tstringlist.Create;
              hkulist.AddStrings(GetRegistryKeyList('HKU\',false));
              for i := 0 to hkulist.Count-1 do
                LogDatei.log('found in hku  '+ hkulist.Strings[i],LLDebug2);
              hkulist.Free;
              aktsidStr := GetDomainUserSidS('',GetUserNameEx_,domain);
              aktsidStr := copy(aktsidStr,2,length(aktsidStr)-2);
              LogDatei.log('sid is: '+aktsidStr,LLDebug2);
              LogDatei.log('index is: '+inttostr(GetRegistryKeyList('HKU\',false).IndexOf(aktsidStr)),LLDebug2);
              if (profilename = GetUserNameEx_)
                 or (profilepath = getProfileImagePathfromSid(GetLocalUserSidStr(GetUserNameEx_)))
                 or (GetRegistryKeyList('HKU\',false).IndexOf(aktsidStr) > -1)
                 then
              begin
                LogDatei.log('The Branch for :'+profilename+' seems to be the logged in user,',LLDebug);
                LogDatei.log('so let us try to patch it via HKUsers\SID',LLDebug);
                workOnHkuserSid (GetUserNameEx_);
              end;
            end
            else
            begin
              // at XP we have problems to get the username while pcpatch is logged in
              if GetSystemOSVersionInfoEx('major_version') = '5' then
              begin
                LogDatei.log('The Branch for :'+profilename+' may be the logged in user,',LLDebug);
                LogDatei.log('so let us try to patch it via HKUsers\SID',LLDebug);
                workOnHkuserSid (profilename);
              end;
            end;
          end;
       End;
     End;
   End;

   (*
     findresult := findnext (SearchRec);
   End;

   sysutils.findclose (SearchRec);
   *)


   // Patch HKEY_Current_User

   LogDatei.log ('', LevelWarnings);
   LogDatei.log ('Make it for user .DEFAULT', LevelWarnings);
   LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
   case rfSelected of
     trfRegedit: doRegistryHackRegeditFormat (Sektion, 'HKEY_USERS\.DEFAULT', flag_force64);
     trfWinst  : doRegistryHack (Sektion, 'HKEY_USERS\.DEFAULT', flag_force64);
   end;

   // do not to try the temporary pcpatch account or SYSTEM - sense less and may fail
   if not ((GetUserName_ = 'pcpatch') or (GetUserName_ = 'SYSTEM')) then
   begin
     LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
     LogDatei.log ('', LevelWarnings);
     LogDatei.log ('And finally: The current user: '+GetUserName_+' : '+GetLocalUserSidStr(GetUserName_), LLInfo);
     LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

     case rfSelected of
       trfRegedit: doRegistryHackRegeditFormat (Sektion, 'HKEY_CURRENT_USER', flag_force64);
       trfWinst  : doRegistryHack (Sektion, 'HKEY_CURRENT_USER', flag_force64);
     end;
   end;

   LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;

   LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;

   finishSection (Sektion, OldNumberOfErrors, OldNumberOfWarnings,
                 DiffNumberOfErrors, DiffNumberOfWarnings);

   if ExitOnError and (DiffNumberOfErrors > 0)
   then result := tsrExitProcess;
end;
{$ELSE WIN32}
begin
  // not implemented
  result := tsrExitProcess;
end;

{$ENDIF WIN32}

function TuibInstScript.doRegistryAllUsrClassDats (const Sektion: TWorkSection;
         rfSelected: TRegistryFormat; const flag_force64 : boolean): TSectionResult;

 var
   //SearchPath : String='';
   //SearchRec  : TSearchRec;
   //findresult : Integer=0;

   profilename, profilepath : string;
   ProfileList : TStringList;
   hkulist  : TStringList;
   i : integer;
   pc : integer;
   domain : string = '';
   UserPath : String='';
   aktsidStr : String = '';

   Errorcode  : Integer=0;
   Info : String='';

   StartWithErrorNumbers : integer=0;
   StartWithWarningsNumber: Integer=0;
{$IFDEF WIN32}

 function LoadUsrClassDat (const path : String) : Boolean;
  var
   Errorcode                                     : Integer;
 begin
   if not SetProcessPrivilege ('SeSecurityPrivilege') then
     logdatei.log('Error: Could not set process privilege SeSecurityPrivilege in LoadUsrClassDat',LLError);
   if not SetProcessPrivilege ('SeRestorePrivilege') then
     logdatei.log('Error: Could not set process privilege SeRestorePrivilege in LoadUsrClassDat',LLError);
   if not SetProcessPrivilege ('SeBackupPrivilege') then
     logdatei.log('Error: Could not set process privilege SeBackupPrivilege in LoadUsrClassDat',LLError);

   {load the selected UsrClass.dat in HKEY_USERS, subkey TempUserRegKey}

   Errorcode:= RegLoadKeyW (HKEY_USERS, PWChar(UnicodeString(TempUserRegKey)), PWChar (UnicodeString(path)));
   if Errorcode = Error_success
   then
   Begin
     LogDatei.log ('"' + path + '" loaded.', LevelComplete);
     result := true;
   End
   else
   Begin
        (* check if UsrClass.dat is in use, since the specific user is logged in *)
     LogDatei.log ('Warning: UsrClass.dat could not be loaded from path "' + path + '". '
        + 'Code ' + IntToStr(Errorcode) + ': ' +  RemoveLineBreaks(SysErrorMessage(Errorcode)), LLWarning);
     result := false;
   End;
 end;

 procedure workOnUsrClassDat;
 begin
     LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;


     case rfSelected of
       trfRegedit: doRegistryHackRegeditFormat (Sektion, 'HKEY_USERS\' + TempUserRegKey, flag_force64);
       trfWinst  : doRegistryHack (Sektion, 'HKEY_USERS\' + TempUserRegKey, flag_force64);
     end;

     LogDatei.log ('', LevelDebug);
     LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;

     ErrorCode := RegFlushKey(HKEY_Users);

     if Errorcode = Error_success
     then
       LogDatei.log ('Flushed', LevelDebug)
     else
     Begin
       LogDatei.log ('Warning: Could not be flushed. Code '
                              + IntToStr(Errorcode) +': ' + RemoveLineBreaks(SysErrorMessage(Errorcode)), LLWarning);
     End;

     Errorcode := RegUnloadKey (HKEY_Users, PChar(TempUserRegKey));
     if Errorcode = Error_success
     then
       LogDatei.log ('Unloaded', LevelDebug)
     else
     Begin
       LogDatei.log ('Warning: Could not be unloaded. Code '
                              + IntToStr(Errorcode) +': ' + RemoveLineBreaks(SysErrorMessage(Errorcode)), LLWarning);
     End;
 end;

 procedure workOnHkuserSid (const name : String);
 var
  sidStr : String='';
 begin
  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

  //sidStr := GetLocalUserSidStr(GetUserNameEx_);
  //LogDatei.log('sidStr :'+sidStr,LLDebug);
  sidStr := GetLocalUserSidStr(name);
  LogDatei.log('sidStr :'+sidStr,LLDebug2);
  sidStr := copy(sidStr,2,length(sidStr)-2);
  LogDatei.log('sidStr :'+sidStr,LLDebug);


   case rfSelected of
     trfRegedit: doRegistryHackRegeditFormat (Sektion, 'HKEY_USERS\' + sidStr, flag_force64);
     trfWinst  : doRegistryHack (Sektion, 'HKEY_USERS\' + sidStr, flag_force64);
   end;

   LogDatei.log ('', LLDebug);
   LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;

   ErrorCode := RegFlushKey(HKEY_Users);

   if Errorcode = Error_success
   then
     LogDatei.log ('Flushed', LevelDebug)
   else
   Begin
     LogDatei.log ('Warning: Could not be flushed. Code '
                            + IntToStr(Errorcode) +': ' + RemoveLineBreaks(SysErrorMessage(Errorcode)), LLWarning);
   End;
 end;

begin
   result := tsrPositive;

   if not initSection (Sektion, StartWithErrorNumbers, StartWithWarningsNumber) then exit;
   LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

   // mount the UsrClass.dat in the users profile path , patch them and write them back
   ProfileList := getProfilesDirList;
   for pc:= 0 to ProfileList.Count -1 do
   begin
     profilepath := ProfileList.Strings[pc];
     profilename := ExtractFileName(profilepath);

     LogDatei.log ('', LLInfo);
     LogDatei.log ('Branch: ' + profilename, LLInfo);
     UserPath := profilepath + '\AppData\Local\Microsoft\Windows\UsrClass.dat';
     if GetSystemOSVersionInfoEx('major_version') = '5' then
     begin
         UserPath := profilepath + '\Local Settings\Microsoft\Windows\UsrClass.dat';
     end;
     if FileExists (UserPath)
     then
     Begin
       if not FileGetWriteAccess (UserPath, Info)
       then
       Begin
         LogDatei.log ('Error: ' + Info, LLError);
       End
       else
       Begin
         if Info <> ''
         then
         Begin
           LogDatei.log ('Warning: ' + Info, LLWarning);
         End;
          if LoadUsrClassDat (UserPath) then
          begin
              workOnUsrClassDat;
          end
          else
          begin
            if (GetUserNameEx_ <> '') then
            begin
              hkulist := Tstringlist.Create;
              hkulist.AddStrings(GetRegistryKeyList('HKU\',false));
              for i := 0 to hkulist.Count-1 do
                LogDatei.log('found in hku  '+ hkulist.Strings[i],LLDebug2);
              hkulist.Free;
              aktsidStr := GetDomainUserSidS('',GetUserNameEx_,domain);
              aktsidStr := copy(aktsidStr,2,length(aktsidStr)-2);
              LogDatei.log('sid is: '+aktsidStr,LLDebug2);
              LogDatei.log('index is: '+inttostr(GetRegistryKeyList('HKU\',false).IndexOf(aktsidStr)),LLDebug2);
              if (profilename = GetUserNameEx_)
                 or (profilepath = getProfileImagePathfromSid(GetLocalUserSidStr(GetUserNameEx_)))
                 or (GetRegistryKeyList('HKU\',false).IndexOf(aktsidStr) > -1)
                 then
              begin
                LogDatei.log('The Branch for :'+profilename+' seems to be the logged in user,',LLDebug);
                LogDatei.log('so let us try to patch it via HKUsers\SID',LLDebug);
                workOnHkuserSid (GetUserNameEx_);
              end;
            end
            else
            begin
              // at XP we have problems to get the username while pcpatch is logged in
              if GetSystemOSVersionInfoEx('major_version') = '5' then
              begin
                LogDatei.log('The Branch for :'+profilename+' may be the logged in user,',LLDebug);
                LogDatei.log('so let us try to patch it via HKUsers\SID',LLDebug);
                workOnHkuserSid (profilename);
              end;
            end;
          end;
       End;
     End;
   End;

   (*
     findresult := findnext (SearchRec);
   End;

   sysutils.findclose (SearchRec);
   *)


   // Patch HKEY_Current_User

   LogDatei.log ('', LevelWarnings);
   LogDatei.log ('Make it for user .DEFAULT', LevelWarnings);
   LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
   case rfSelected of
     trfRegedit: doRegistryHackRegeditFormat (Sektion, 'HKEY_USERS\.DEFAULT', flag_force64);
     trfWinst  : doRegistryHack (Sektion, 'HKEY_USERS\.DEFAULT', flag_force64);
   end;

   // do not to try the temporary pcpatch account or SYSTEM - sense less and may fail
   if not ((GetUserName_ = 'pcpatch') or (GetUserName_ = 'SYSTEM')) then
   begin
     LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
     LogDatei.log ('', LevelWarnings);
     LogDatei.log ('And finally: The current user: '+GetUserName_+' : '+GetLocalUserSidStr(GetUserName_), LLInfo);
     LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

     case rfSelected of
       trfRegedit: doRegistryHackRegeditFormat (Sektion, 'HKEY_CURRENT_USER', flag_force64);
       trfWinst  : doRegistryHack (Sektion, 'HKEY_CURRENT_USER', flag_force64);
     end;
   end;

   LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;

   LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;

   finishSection (Sektion, OldNumberOfErrors, OldNumberOfWarnings,
                 DiffNumberOfErrors, DiffNumberOfWarnings);

   if ExitOnError and (DiffNumberOfErrors > 0)
   then result := tsrExitProcess;
end;
{$ELSE WIN32}
begin
  // not implemented
  result := tsrExitProcess;
end;

{$ENDIF WIN32}


function TuibInstScript.doRegistryNTUserDat (const Sektion: TWorkSection;
         rfSelected: TRegistryFormat; const flag_force64 : boolean; const UserPath : string): TSectionResult;

 var
   //SearchPath : String='';
   //SearchRec  : TSearchRec;
   //findresult : Integer=0;

   //profilename, profilepath : string;
   //ProfileList : TStringList;
   //pc : integer;

   //UserPath : String='';

   Errorcode  : Integer=0;
   Info : String='';

   StartWithErrorNumbers : integer=0;
   StartWithWarningsNumber: Integer=0;

   function LoadNTUserDat (const path : String) : Boolean;
    var
     Errorcode                                     : Integer;
   begin
     if not SetProcessPrivilege ('SeSecurityPrivilege') then
       logdatei.log('Error: Could not set process privilege SeSecurityPrivilege in LoadNTUserDat',LLError);
     if not SetProcessPrivilege ('SeRestorePrivilege') then
       logdatei.log('Error: Could not set process privilege SeRestorePrivilege in LoadNTUserDat',LLError);
     if not SetProcessPrivilege ('SeBackupPrivilege') then
       logdatei.log('Error: Could not set process privilege SeBackupPrivilege in LoadNTUserDat',LLError);

     {load the selected ntuser.dat in HKEY_USERS, subkey TempUserRegKey}

     Errorcode:= RegLoadKeyW (HKEY_USERS, PWChar(UnicodeString(TempUserRegKey)), PWChar (UnicodeString(path)));
     if Errorcode = Error_success
     then
     Begin
       LogDatei.log ('"' + path + '" loaded.', LevelComplete);
       result := true;
     End
     else
     Begin
          (* check if NTUser.dat is in use, since the specific user is logged in *)
       LogDatei.log ('Warning: NTUser.dat could not be loaded from path "' + path + '". '
          + 'Code ' + IntToStr(Errorcode) + ': ' +  RemoveLineBreaks(SysErrorMessage(Errorcode)), LLWarning);
       result := false;
     End;
   end;

 procedure workOnNtUserDat;
 begin
     LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;


     case rfSelected of
       trfRegedit: doRegistryHackRegeditFormat (Sektion, 'HKEY_USERS\' + TempUserRegKey, flag_force64);
       trfWinst  : doRegistryHack (Sektion, 'HKEY_USERS\' + TempUserRegKey, flag_force64);
     end;

     LogDatei.log ('', LevelDebug);
     LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;

     ErrorCode := RegFlushKey(HKEY_Users);

     if Errorcode = Error_success
     then
       LogDatei.log ('Flushed', LevelDebug)
     else
     Begin
       LogDatei.log ('Warning: Could not be flushed. Code '
                              + IntToStr(Errorcode) +': ' + RemoveLineBreaks(SysErrorMessage(Errorcode)), LLWarning);
     End;

     Errorcode := RegUnloadKey (HKEY_Users, PChar(TempUserRegKey));
     if Errorcode = Error_success
     then
       LogDatei.log ('Unloaded', LevelDebug)
     else
     Begin
       LogDatei.log ('Warning: Could not be unloaded. Code '
                              + IntToStr(Errorcode) +': ' + RemoveLineBreaks(SysErrorMessage(Errorcode)), LLWarning);
     End;
 end;


begin
   result := tsrPositive;

   if not initSection (Sektion, StartWithErrorNumbers, StartWithWarningsNumber) then exit;
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
     if FileExists (UserPath)
     then
     Begin
       if not FileGetWriteAccess (UserPath, Info)
       then
       Begin
         LogDatei.log ('Error: ' + Info, LLError);
       End
       else
       Begin
         if Info <> ''
         then
         Begin
           LogDatei.log ('Warning: ' + Info, LLWarning);
         End;
          if LoadNTUserdat (UserPath) then
          begin
              workOnNtUserDat;
          end
          else
            LogDatei.log('Error: could not patch ntuserdat: '+UserPath,LLError);
          (*
          begin
            if (GetUserNameEx_ <> '') then
            begin
              if (profilename = GetUserNameEx_)
                 or (profilepath = getProfileImagePathfromSid(GetLocalUserSidStr(GetUserNameEx_)))
                 then
              begin
                LogDatei.log('The Branch for :'+profilename+' seems to be the logged in user,',LLDebug);
                LogDatei.log('so let us try to patch it via HKUsers\SID',LLDebug);
                workOnHkuserSid (profilename);
              end;
            end
            else
            begin
              // at XP we have problems to get the username while pcpatch is logged in
              if GetSystemOSVersionInfoEx('major_version') = '5' then
              begin
                LogDatei.log('The Branch for :'+profilename+' may be the logged in user,',LLDebug);
                LogDatei.log('so let us try to patch it via HKUsers\SID',LLDebug);
                workOnHkuserSid (profilename);
              end;
            end;
          end;
          *)
       End;
     End;
   //End;
   LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;

   LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;

   finishSection (Sektion, OldNumberOfErrors, OldNumberOfWarnings,
                 DiffNumberOfErrors, DiffNumberOfWarnings);

   if ExitOnError and (DiffNumberOfErrors > 0)
   then result := tsrExitProcess;
end;
{$ENDIF WINDOWS}


function TuibInstScript.doOpsiServiceCall
 (const Sektion: TWorkSection; Const parameter : String;
           var output: TXStringList)
                                                    : TSectionResult;

  Type
    TServiceChoice = (tscGlobal, tscReuse, tscLogin, tscInteractiveLogin,
                      tscOpsiclientd, tscOpsiclientdOnce);

  Var
   startSIndentLevel : Integer=0;
   serviceChoice : TServicechoice;
   errorOccured : Boolean;
   syntaxcheck : Boolean;
   errorInfo : String='';
   i : integer=0;
   paramstartI : Integer;
   j : Integer=0;

   methodname : String='';
   paramList : TStringList;
   parameters : Array of String;

   omc: TOpsiMethodCall;
   //local_opsidata : TOpsiDataJSONService;
   serviceURL : String='';
   username : String='';
   password : String='';
   sessionid : String='';
   r : String='';
   Expressionstr : String='';
   param : String='';
   inParams, paramsFound, paramsValueListFound : Boolean;
   stopIt : boolean;
   testresult : String='';
   {$IFDEF GUI}
   passwordDialog : TDialogServicePassword;
   {$ENDIF GUI}
   opsiServiceVersion : String='';
   myconf : TIniFile;
   opsiclientd_conf : String='';
   tmplist : Tstringlist;

   procedure getNextValidLine (var line : String; var lineno : Integer; const lines : TStrings);
   begin
    line := trim(lines [lineno - 1]);
    while (lineno <= lines.count) and (line = '') or (line [1] = LineIsCommentChar)
    do
    begin
      inc (lineno);
      if lineno <= lines.count
      then
        line := trim(lines [lineno - 1]);
      logdatei.log_prog('Script line(gNVL): '+intToStr(lineno)+' : '+line,LLDebug3);
    end;
    logdatei.log_prog('Script line(gNVL): '+intToStr(lineno)+' : '+line,LLDebug2);
   end;


begin
 try
    // initializing
    tmplist := Tstringlist.create;
    result := tsrPositive;
    startSIndentLevel :=  LogDatei.LogSIndentLevel;
    if not initSection (Sektion, OldNumberOfErrors, OldNumberOfWarnings) then exit;
    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
    try
      //convert String to catch not allowed chars
      r := trim(Utf8ToAnsi(AnsiToUtf8(parameter)));
    except
      on ex: Exception
      do
      begin
          LogDatei.log ('Exception in doOpsiServiceCall: convert parameter' + ex.message ,LLError);
      end;
    end;
    syntaxcheck := true;


    serviceChoice := tscGlobal;

    if local_opsidata <> nil
    then serviceChoice := tscReuse;
    logdatei.log('Parsingprogress: r: '+r+' exp: '+Expressionstr,LLDebug3);
    if r <> ''
    then
    Begin
      serviceChoice := tscLogin;
      while r <> ''
      do
      Begin
        if skip ('/opsiclientd-once', r, r, errorInfo)
        then
           serviceChoice := tscOpsiclientdOnce // we call the local opsiclientd and switch back

        else if skip ('/opsiclientd', r, r, errorInfo)
        then
           serviceChoice := tscOpsiclientd // we call the local opsiclientd

        else if skip ('/preloginservice', r, r, errorInfo)
        then
           serviceChoice := tscGlobal // we return to the default service from the installation server

        else if skip ('/interactive', r, r, errorInfo)
        then
           serviceChoice := tscInteractiveLogin

        else if skip ('/serviceurl', r, r, errorInfo)
           or skip ('/opsiservice', r, r, errorInfo)
           or skip ('/s', r, r, errorInfo)
        then
          EvaluateString(r, r, serviceurl, errorInfo)

        else if skip ('/username', r, r, errorInfo)
           or skip ('/user', r, r, errorInfo)
           or skip ('/u', r, r, errorInfo)
        then
          EvaluateString(r, r, username, errorInfo)

        else if skip ('/password', r, r, errorInfo)
           or skip ('/pass', r, r, errorInfo)
           or skip ('/p', r, r, errorInfo)
        then
          EvaluateString(r, r, password, errorInfo)

        else
        Begin
          r := '';
          errorInfo := 'Not valid parameter for section ' + Sektion.Name;
          syntaxcheck := false;
        End;
      end;

    end;

    if (serviceChoice in [tscGlobal, tscReuse, tscOpsiclientd])
      and
      ((username <> '') or (password <> '') or (serviceurl<>''))
    then
    Begin
      syntaxcheck := false;
      errorInfo := 'Connection parameters must not be specified if existing connection shall be used.'
    end;
    if (serviceChoice in [tscLogin, tscInteractiveLogin]) and (local_opsidata <> nil) then
      Logdatei.log('Warning: Existing service connection will be reopend. This may cause critical errors. Do not do this, if you are not really sure what you doing.', LLwarning );

    // lets do something

    i := 1;
    if syntaxcheck
    then
    Begin
      errorOccured := false;
      methodname := '';
      paramList := TStringList.create;
      inParams := false;
      paramsFound := false;
      paramsValueListFound := false;

    end;


    while (i <= Sektion.count) and syntaxcheck
    do
    Begin
        getNextValidLine (r, i, Sektion);

      if i <= Sektion.count
      then
      Begin
        SyntaxCheck := false;
        GetWord (r, Expressionstr, r, [':']);

        if Skip (':', r, r, errorInfo)
        then
        Begin
          logdatei.log('Parsingprogress: r: '+r+' exp: '+Expressionstr,LLDebug3);
          if LowerCase (Expressionstr) = LowerCase ('"method"')
          then
          Begin
            if GetString (r, methodname, r, errorInfo, true)
            then  syntaxcheck := true;
            logdatei.log_prog('Parsingprogress: r: '+r+' exp: '+Expressionstr,LLDebug3);
          end

          else if LowerCase (Expressionstr) = LowerCase ('"params"')
          then
          Begin
             syntaxcheck := true;
             paramsFound := true;

             if r = ''
             then
               getNextValidLine (r, i, Sektion);
             logdatei.log_prog('Parsingprogress: r: '+r+' exp: '+Expressionstr,LLDebug3);
             if (i <= Sektion.count) and skip ('[', r, r, errorInfo)
             then
             Begin
               logdatei.log_prog('Parsingprogress: r: '+r+' exp: '+Expressionstr,LLDebug3);
               inParams := true;
               paramsValueListFound := true;
               paramStartI := i;
               if r = ']'
               then  // empty param list
               Begin
                 inParams := false;
               end
               else if r <> ''
               then
               Begin
                 syntaxcheck := false;
                 errorInfo := 'Parameters must be on their own lines';
               end
               else
               Begin
                 inc (i);
               End;


               while
                 syntaxcheck and (i <= Sektion.count) and inParams
               do
               Begin
                  getNextValidLine(r, i, Sektion);
                  logdatei.log_prog('Parsingprogress: r: '+r+' exp: '+Expressionstr,LLDebug3);
                  if i <= Sektion.count
                  then
                  Begin
                    if r = ']'
                    then inParams := false
                    else
                    Begin
                      if length(r) > 0
                      then
                      Begin
                        if r[length(r)] = ','
                        then r := trim(copy (r, 1, length(r) -1));
                        if length(r) > 0
                        then
                        Begin

                          if GetString (r, param, r, errorInfo, true)
                          then
                          begin
                            LogDatei.log_prog ('Parsing: getparam: ' + param ,LLdebug2);
                            paramList.Add(param);
                          end
                          else
                            syntaxcheck := false;
                        end;
                      end;
                      inc(i);
                    End;
                  End

               end;
             End;

             if syntaxcheck and (i > Sektion.count) and inParams
             then
             Begin
               SyntaxCheck := false;
               errorInfo := 'ParamList starting on line ' + inttoStr(paramStartI) + ' not closed '
                 + ' (must be a pure "]" on a line)'
             End;

          End

          else
          Begin
             SyntaxCheck := false;
             ErrorInfo := Expressionstr + ' is not a valid parameter ';
          End
        End


      end;


      inc(i);

    End; // loop through lines


    if syntaxCheck
    then
    Begin
      if (methodname = '')
      then
      Begin
        syntaxCheck := false;
        errorInfo := errorInfo + ' Method name missing';
      End;

      if paramsFound
      then
      Begin
        if not paramsValueListFound
        then
        Begin
        errorInfo := errorInfo + ' Params missing';
        syntaxCheck := false;
        End
      end
      else
      Begin
        errorInfo := errorInfo + ' Params missing';
        syntaxCheck := false;
      End;
    end;


    if syntaxCheck then
    Begin
        LogDatei.log('   "method": "' + methodname + '"', LLInfo);
      //LogDatei.log_prog('   "params" : "' + jsonParams, LLInfo);

      testresult := 'service not initialized';
      case serviceChoice of
        tscGlobal:
          Begin

            if opsidata = nil
            then
            Begin
              errorOccured := true;
            End
            else
             try
              if opsidata.getOpsiServiceVersion = '4' then
                local_opsidata := TOpsi4Data(opsidata);
                 LogDatei.log_prog('Calling opsi service at ' + local_opsidata.serviceUrl, LLDebug);
             except
               errorOccured := true;
               testresult := 'not in service mode';
             end;

          end;

        tscReuse:
          begin
            if (local_opsidata = nil) or  not(local_opsidata is TOpsi4Data)
            then
            Begin
              errorOccured := true;
              testresult := 'not connected to service';
            End
            else
            Begin
              LogDatei.log_prog('Calling opsi service at ' + local_opsidata.serviceUrl, LLDebug);
            End
          end;

        tscLogin:
          begin
              try
                opsiServiceVersion := getOpsiServiceVersion(serviceurl, username, password, sessionid);
                if opsiServiceVersion = '4' then
                begin
                  local_opsidata := TOpsi4Data.Create;
                  local_opsidata.initOpsiConf(serviceurl, username, password, sessionid);
                  //Topsi4data(local_opsidata).initOpsiConf(serviceurl, username, password);
                  //OpsiData.setOptions (opsiclientd_serviceoptions);
                  omc := TOpsiMethodCall.create ('backend_info',[]);
                  //omc := TOpsiMethodCall.create ('authenticated',[]);
                  testresult := local_opsidata.CheckAndRetrieve (omc, errorOccured);
                  omc.free;
                end
                else if opsiServiceVersion = '' then LogDatei.log ('opsi service version could not retrieved, perhaps no connection', LLwarning)
                    else LogDatei.log ('Internal Error: Unkown Opsi Service Version:>'+opsiServiceVersion+'<', LLerror);
              except
                on e: exception do
                Begin
                  LogDatei.log('Exception in doOpsiServicecall: tscLogin: ' + e.message, LLError);
                  errorOccured := true;
                  testresult := 'not connected to service';
                End;
              end;
          end;

        tscInteractiveLogin:
          Begin
            {$IFDEF GUI}
            stopIt := false;
            passwordDialog := TDialogServicePassword.create(nil);


            while not stopIt
            do
            Begin
              passwordDialog.Visible := false;
              passwordDialog.EditServiceURL.Text := serviceurl;
              passwordDialog.EditUsername.Text := username;
              passwordDialog.EditPassword.Text := '';

              if passwordDialog.showModal = mrOK
              then
              Begin
                serviceUrl := passwordDialog.EditServiceURL.Text;
                username := passwordDialog.EditUsername.Text;
                password := passwordDialog.EditPassword.Text;
                LogDatei.log ('serviceUrl: '+serviceUrl, LLconfidential);
                LogDatei.log ('username: '+username, LLconfidential);
                LogDatei.log ('password: '+password, LLconfidential);
                opsiServiceVersion := getOpsiServiceVersion(serviceurl, username, password,sessionid);
                if opsiServiceVersion = '4' then
                begin
                  local_opsidata := TOpsi4Data.Create;
                  local_opsidata.initOpsiConf(serviceurl, username, password, sessionid);
                  //Topsi4data(local_opsidata).initOpsiConf(serviceurl, username, password);
                  //OpsiData.setOptions (opsiclientd_serviceoptions);
                  omc := TOpsiMethodCall.create ('backend_info',[]);
                  //omc := TOpsiMethodCall.create ('authenticated',[]);
                  testresult := local_opsidata.CheckAndRetrieve (omc, errorOccured);
                  omc.free;
                end
                else errorOccured := true;
                //LogDatei.log ('Internal Error: Unkown Opsi Service Version:>'+opsiServiceVersion+'<', LLerror);

                stopIt := not errorOccured;
              End

              else
              Begin
                stopIt := true;
                errorOccured := true;
                testresult := 'Cancelled by user';
              end;

            end;

            passwordDialog.Free;

            {$ELSE GUI}
            stopIt := false;
            //passwordDialog := TDialogServicePassword.create(nil);


            while not stopIt
            do
            Begin

                if cmdLineInputDialog(serviceUrl, rsGetServiceUrl, serviceurl, false) and
                  cmdLineInputDialog(username, rsGetUserName, '', false) and
                  cmdLineInputDialog(password, rsGetPassword, '', true) then
              Begin
                LogDatei.log ('serviceUrl: '+serviceUrl, LLconfidential);
                LogDatei.log ('username: '+username, LLconfidential);
                LogDatei.log ('password: '+password, LLconfidential);
                opsiServiceVersion := getOpsiServiceVersion(serviceurl, username, password,sessionid);
                if opsiServiceVersion = '4' then
                begin
                  local_opsidata := TOpsi4Data.Create;
                  local_opsidata.initOpsiConf(serviceurl, username, password, sessionid);
                  omc := TOpsiMethodCall.create ('backend_info',[]);
                  testresult := local_opsidata.CheckAndRetrieve (omc, errorOccured);
                  omc.free;
                end
                else errorOccured := true;
                stopIt := not errorOccured;
              End

              else
              Begin
                stopIt := true;
                errorOccured := true;
                testresult := 'Cancelled by user';
              end;
            end;
            {$ENDIF GUI}
          end;

        tscOpsiclientd, tscOpsiclientdOnce:
           begin
            serviceurl := 'https://localhost:4441/opsiclientd';
            {$IFDEF WINDOWS}
            opsiclientd_conf := getSpecialFolder(CSIDL_PROGRAM_FILES)
                                +'\opsi.org\opsi-client-agent\opsiclientd\opsiclientd.conf';
            {$ENDIF WINDOWS}
            {$IFDEF LINUX}
            opsiclientd_conf := '/etc/opsi/opsiclientd.conf';
            {$ENDIF LINUX}
            if FileExists(opsiclientd_conf) then
            begin
              myconf := TInifile.Create(opsiclientd_conf);
              password := myconf.ReadString('global','opsi_host_key','');
              username := myconf.ReadString('global','host_id','');
              myconf.Free;
            end;
            if password = '' then
            begin
              stopIt := true;
              errorOccured := true;
              testresult := 'Can not get opsi_host_key';
            end
            else
            begin
              local_opsidata := TOpsi4Data.Create;
              local_opsidata.initOpsiConf(serviceurl, username, password);
              omc := TOpsiMethodCall.create ('backend_info',[]);
              testresult := local_opsidata.CheckAndRetrieve (omc, errorOccured);
              omc.free;
            end;
          end;

      end;

      if errorOccured
        then
        Begin
          local_opsidata := nil;
          LogDatei.log('Error: ' + testresult, LLError);
        End;

      if local_opsidata = nil
        then
        Begin
          errorOccured := true;
          LogDatei.log('Error: no connection to service', LLError);
        End;

    end;

    if not errorOccured and syntaxcheck
      then
      Begin
        try
          setlength(parameters, paramList.count);
          for j := 0 to paramList.count - 1 do
          begin
            parameters[j] := paramlist.Strings[j];
            logdatei.log_prog('param['+inttostr(j)+']: '+paramlist.Strings[j],LLDebug2);
          end;

          omc := TOpsiMethodCall.create (methodname, parameters);
          testresult := '';

          if copy(methodname, length(methodname) - length('_hash') + 1, length(methodname))
             = '_hash'
          then
          Begin
            if output = nil then output := TXStringList.create;
              tmplist := local_opsidata.checkAndRetrieveMap(omc, errorOccured);
              if tmplist <> nil then output.Assign(tmplist);
              //output.Text := tmplist.Text;
             if (not errorOccured) and (output <> nil) then
             begin
               try
                 if output.count > 0 then
                    testresult :=    output.strings[0];

                  for j:= 0 to output.count - 1 do
                     testresult := testresult + ', ' + output.strings[j]
                except
                LogDatei.log('Error: exeption after checkAndRetrieveMap in reading the stringlist' + testresult, LLError);
               end
             end
             else if output = nil then output := TXStringList.create;
          end
          else if copy(methodname, length(methodname) - length('_list') + 1, length(methodname))
             = '_list'
          then
          Begin
            if output = nil then output := TXStringList.create;
            tmplist := local_opsidata.checkAndRetrieveStringList(omc, errorOccured);
            if tmplist <> nil then output.Assign(tmplist);
            //output.Text := tmplist.Text;
            if (not errorOccured) and (output <> nil) then
            begin
              try
                if output.count > 0 then
                  testresult :=    output.strings[0];

                for j:= 1 to output.count - 1 do
                  testresult := testresult + ', ' + output.strings[j]
              except
                 LogDatei.log('Error: exeption after checkAndRetrieveStringList in reading the stringlist' + testresult, LLError);
              end
            end
            else if output = nil then output := TXStringList.create;
          end
          else

          Begin
             testresult := local_opsidata.CheckAndRetrieveString (omc, errorOccured);
             output := TXStringList.create;
             output.add (testresult);
          End;

          omc.free;

          if errorOccured
          then
          begin
            LogDatei.log('Error: ' + testresult, LLError);
          end
          else
          begin
            LogDatei.log('JSON result: '+ testresult, LLinfo);
            //Logdatei.log('Setup script name: '+opsidata.getProductScriptPath(tacSetup), LLessential);
          end;
        except
          on e: exception do
          Begin
            LogDatei.log('Exception in doOpsiServiceCall: do the call: ' + e.message, LLError);
          End;
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
    finishSection (Sektion, OldNumberOfErrors, OldNumberOfWarnings,
                   DiffNumberOfErrors, DiffNumberOfWarnings);

    LogDatei.LogSIndentLevel := startSIndentLevel;

    if ExitOnError and (DiffNumberOfErrors > 0)
    then result := tsrExitProcess;

    if not SyntaxCheck then
    Begin
      if i >= Sektion.count
      then
        reportError (Sektion, Sektion.count, Sektion.strings [Sektion.count - 1], ErrorInfo)
      else
         reportError (Sektion, i, Sektion.strings [i-1], ErrorInfo);
    End
  except
    on ex: Exception
    do
    begin
      LogDatei.log ('Exception in doOpsiServiceCall: general' + ex.message ,LLError);
    end;
  end;

end;


//#############################################################################

function TuibInstScript.doOpsiServiceHashList
 (const Sektion: TWorkSection; Const parameter : String;
           var output: TXStringList)
                                                    : TSectionResult;

  Type
    TServiceChoice = (tscGlobal, tscReuse, tscLogin, tscInteractiveLogin);

  Var
   startSIndentLevel : Integer=0;
   serviceChoice : TServicechoice;
   errorOccured : Boolean;
   syntaxcheck : Boolean;
   errorInfo : String='';
   i : integer=0;
   paramstartI : Integer=0;
   j : Integer=0;
   methodname : String='';
   paramList : TStringList;
   parameters : Array of String;

   omc: TOpsiMethodCall;
   //local_opsidata : TOpsiDataJSONService;
   serviceURL : String='';
   username : String='';
   password : String='';
   sessionid : String='';
   r : String='';
   Expressionstr : String='';
   param : String='';
   inParams, paramsFound, paramsValueListFound : Boolean;
   stopIt : boolean;
   testresult : String='';
   {$IFDEF GUI}
   passwordDialog : TDialogServicePassword;
   {$ENDIF GUI}
  listname : String='';
  list : TStringList;
  varindex : integer=0;
  opsiServiceVersion : String='';

   procedure getNextValidLine (var line : String; var lineno : Integer; const lines : TStrings);
   begin
    line := trim(lines [lineno - 1]);
    while (lineno <= lines.count) and (line = '') or (line [1] = LineIsCommentChar)
    do
    begin
      inc (lineno);
      if lineno <= lines.count
      then
        line := trim(lines [lineno - 1]);
    end;
   end;


begin
  // initializing
  result := tsrPositive;
  sessionid := '';
  startSIndentLevel :=  LogDatei.LogSIndentLevel;
  if not initSection (Sektion, OldNumberOfErrors, OldNumberOfWarnings) then exit;
  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
  try
    //convert String to catch not allowed chars
    r := trim(Utf8ToAnsi(AnsiToUtf8(parameter)));
  except
    on ex: Exception
    do
    begin
      LogDatei.log ('Exception in doOpsiServiceCall: ' + ex.message ,LLError);
    end;
  end;
  syntaxcheck := true;


  serviceChoice := tscGlobal;

  if local_opsidata <> nil
  then serviceChoice := tscReuse;

  if r <> ''
  then
  Begin
    serviceChoice := tscLogin;
    while r <> ''
    do
    Begin
      if skip ('/preloginservice', r, r, errorInfo)
      then
         serviceChoice := tscGlobal // we return to the default service from the installation server

      else if skip ('/interactive', r, r, errorInfo)
      then
         serviceChoice := tscInteractiveLogin

      else if skip ('/serviceurl', r, r, errorInfo)
         or skip ('/opsiservice', r, r, errorInfo)
         or skip ('/s', r, r, errorInfo)
      then
        EvaluateString(r, r, serviceurl, errorInfo)

      else if skip ('/username', r, r, errorInfo)
         or skip ('/user', r, r, errorInfo)
         or skip ('/u', r, r, errorInfo)
      then
        EvaluateString(r, r, username, errorInfo)

      else if skip ('/password', r, r, errorInfo)
         or skip ('/pass', r, r, errorInfo)
         or skip ('/p', r, r, errorInfo)
      then
        EvaluateString(r, r, password, errorInfo)

      else
      Begin
        r := '';
        errorInfo := 'Not valid parameter for section ' + Sektion.Name;
        syntaxcheck := false;
      End;
    end;

  end;

  if (serviceChoice in [tscGlobal, tscReuse])
    and
    ((username <> '') or (password <> '') or (serviceurl<>''))
  then
  Begin
    syntaxcheck := false;
    errorInfo := 'Connection parameters must not be specified if existing connection shall be used.'
  end;

  // lets do something

  i := 1;
  if syntaxcheck
  then
  Begin
    errorOccured := false;
    methodname := '';
    paramList := TStringList.create;
    inParams := false;
    paramsFound := false;
    paramsValueListFound := false;
  end;


  while (i <= Sektion.count) and syntaxcheck
  do
  Begin
      getNextValidLine (r, i, Sektion);
    if i <= Sektion.count
    then
    Begin
      SyntaxCheck := false;
      GetWord (r, Expressionstr, r, [':']);

      if Skip (':', r, r, errorInfo)
      then
      Begin
        if LowerCase (Expressionstr) = LowerCase ('"method"')
        then
        Begin
          if GetString (r, methodname, r, errorInfo, false)
          then
            syntaxcheck := true;
        end

        else if LowerCase (Expressionstr) = LowerCase ('"HashList"')
          then
          Begin
            if GetString (r, listname, r, errorInfo, false)
            then
            begin
              syntaxcheck := true;
              list := TXStringList.create;
              VarIndex := listOfStringLists.IndexOf (LowerCase (listname));
              if VarIndex >= 0 then
              begin
                list.Assign(TStringList(contentOfStringLists[VarIndex]));
                syntaxCheck := true;
              end
            end
          end

        else if LowerCase (Expressionstr) = LowerCase ('"params"')
        then
        Begin
           syntaxcheck := true;
           paramsFound := true;

           if r = ''
           then
             getNextValidLine (r, i, Sektion);

           if (i <= Sektion.count) and skip ('[', r, r, errorInfo)
           then
           Begin
             inParams := true;
             paramsValueListFound := true;
             paramStartI := i;
             if r = ']'
             then  // empty param list
             Begin
               inParams := false;
             end
             else if r <> ''
             then
             Begin
               syntaxcheck := false;
               errorInfo := 'Parameters must be on their own lines';
             end
             else
             Begin
               inc (i);
             End;


             while
               syntaxcheck and (i <= Sektion.count) and inParams
             do
             Begin
                getNextValidLine(r, i, Sektion);
                if i <= Sektion.count
                then
                Begin
                  if r = ']'
                  then inParams := false
                  else
                  Begin
                    if length(r) > 0
                    then
                    Begin
                      if r[length(r)] = ','
                      then r := trim(copy (r, 1, length(r) -1));
                      if length(r) > 0
                      then
                      Begin

                        if GetString (r, param, r, errorInfo, true)
                        then
                        begin
                          LogDatei.log_prog ('Parsing: getparam: ' + param ,LLdebug2);
                          paramList.Add(param);
                        end
                        else
                          syntaxcheck := false;
                      end;
                    end;
                    inc(i);
                  End;
                End

             end;
           End;

           if syntaxcheck and (i > Sektion.count) and inParams
           then
           Begin
             SyntaxCheck := false;
             errorInfo := 'ParamList starting on line ' + inttoStr(paramStartI) + ' not closed '
               + ' (must be a pure "]" on a line)'
           End;

        End

        else
        Begin
           SyntaxCheck := false;
           ErrorInfo := Expressionstr + ' is not a valid parameter ';
        End
      End

    end;
    inc(i);
  End; // loop through lines


  if syntaxCheck
  then
  Begin
    if (methodname = '')
    then
    Begin
      syntaxCheck := false;
      errorInfo := errorInfo + ' Method name missing';
    End;

    if paramsFound
    then
    Begin
      if not paramsValueListFound
      then
      Begin
      errorInfo := errorInfo + ' Params missing';
      syntaxCheck := false;
      End
    end
    else
    Begin
      errorInfo := errorInfo + ' Params missing';
      syntaxCheck := false;
    End;
  end;


  if syntaxCheck then
  Begin
    LogDatei.log('   "method": "' + methodname + '"', LevelComplete);
    //LogDatei.log('   "params" : "' + jsonParams, LevelComplete);

    testresult := 'service not initialized';
    case serviceChoice of
      tscGlobal:
        Begin

          if opsidata = nil
          then
          Begin
            errorOccured := true;
          End
          else
           try
              if opsidata.getOpsiServiceVersion = '4' then
              local_opsidata := opsidata;
             LogDatei.log('Calling opsi service on existing connection at ' + local_opsidata.serviceUrl, LevelComplete);
           except
             errorOccured := true;
             testresult := 'not in service mode';
           end;

        end;

      tscReuse:
        begin
          if (local_opsidata = nil) or  not(local_opsidata is TOpsi4Data)
          then
          Begin
            errorOccured := true;
            testresult := 'not connected to service';
          End
          else
          Begin
            LogDatei.log('Calling opsi service on existing connection at ' + local_opsidata.serviceUrl, LevelComplete);
          End
        end;

      tscLogin:
        begin
          opsiServiceVersion := getOpsiServiceVersion(serviceurl, username, password, sessionid);
          if opsiServiceVersion = '4' then
          begin
            Local_opsidata := TOpsi4Data.Create;
            local_opsidata.initOpsiConf(serviceurl, username, password, sessionid);
            //Topsi4data(local_opsidata).initOpsiConf(serviceurl, username, password);
            //OpsiData.setOptions (opsiclientd_serviceoptions);
            omc := TOpsiMethodCall.create ('backend_info',[]);
            //omc := TOpsiMethodCall.create ('authenticated',[]);
            testresult := local_opsidata.CheckAndRetrieve (omc, errorOccured);
            omc.free;
          end
          else LogDatei.log ('Internal Error: Unkown Opsi Service Version:>'+opsiServiceVersion+'<', LLerror);
        end;

      tscInteractiveLogin:
        Begin
          {$IFDEF GUI}
          stopIt := false;
          passwordDialog := TDialogServicePassword.create(nil);


          while not stopIt
          do
          Begin
            passwordDialog.Visible := false;
            passwordDialog.EditServiceURL.Text := serviceurl;
            passwordDialog.EditUsername.Text := username;
            passwordDialog.EditPassword.Text := '';

            if passwordDialog.showModal = mrOK
            then
            Begin
              serviceUrl := passwordDialog.EditServiceURL.Text;
              username := passwordDialog.EditUsername.Text;
              password := passwordDialog.EditPassword.Text;
              opsiServiceVersion := getOpsiServiceVersion(serviceurl, username, password, sessionid);
              if opsiServiceVersion = '4' then
              begin
                local_opsidata := TOpsi4Data.Create;
                local_opsidata.initOpsiConf(serviceurl, username, password, sessionid);
                //Topsi4data(local_opsidata).initOpsiConf(serviceurl, username, password, sessionid);
                //OpsiData.setOptions (opsiclientd_serviceoptions);
                omc := TOpsiMethodCall.create ('backend_info',[]);
                //omc := TOpsiMethodCall.create ('authenticated',[]);
                testresult := local_opsidata.CheckAndRetrieve (omc, errorOccured);
                omc.free;
              end
              else LogDatei.log ('Internal Error: Unkown Opsi Service Version:>'+opsiServiceVersion+'<', LLerror);

              stopIt := not errorOccured;
            End

            else
            Begin
              stopIt := true;
              errorOccured := true;
              testresult := 'Cancelled by user';
            end;

          end;

          passwordDialog.Free;

          {$ENDIF GUI}
        end;
    end;

    if errorOccured
      then
      Begin
        local_opsidata := nil;
        LogDatei.log('Error: ' + testresult, LLError);
      End;

    if local_opsidata = nil
      then
      Begin
        errorOccured := true;
        LogDatei.log('Error: no connection to service', LLError);
      End;

  end;

  if not errorOccured and syntaxcheck
    then
    Begin
      setlength(parameters, paramList.count);
      for j := 0 to paramList.count - 1 do
      begin
        parameters[j] := paramlist.Strings[j]
      end;

      omc := TOpsiMethodCall.create (methodname, list, parameters);
      testresult := '';

      TStringList(output) := local_opsidata.checkAndRetrieveHashList(omc, errorOccured);
      if output.count > 0 then
        testresult :=    output.strings[0];

      for j:= 0 to output.count - 1
      do
        testresult := testresult + ', ' + output.strings[j];

      omc.free;
      //Logdatei.log('Setup script name: '+opsidata.getProductScriptPath(tacSetup), LLessential);

      if errorOccured
      then
      begin
        LogDatei.log('Error: ' + testresult, LLError);
      end
      else
      begin
        LogDatei.log('JSON result: '+ testresult, LLinfo);
      end;
    end;

  // finishing our section
  finishSection (Sektion, OldNumberOfErrors, OldNumberOfWarnings,
                 DiffNumberOfErrors, DiffNumberOfWarnings);

  LogDatei.LogSIndentLevel := startSIndentLevel;

  if ExitOnError and (DiffNumberOfErrors > 0)
  then result := tsrExitProcess;

  if not SyntaxCheck then
  Begin
    if i >= Sektion.count
    then
      reportError (Sektion, Sektion.count, Sektion.strings [Sektion.count - 1], ErrorInfo)
    else
       reportError (Sektion, i, Sektion.strings [i-1], ErrorInfo);
  End
end;
//#############################################################################

function TuibInstScript.doXMLPatch (const Sektion: TWorkSection; Const XMLFilename : String;
                var output: TXStringList) : TSectionResult;
{$IFDEF WINDOWS}
begin
  //DataModuleLogServer.IdTCPServer1.Active:=true;
  result := executeWith (Sektion,  '"'+ ExtractFileDir(paramstr(0))+PathDelim+'opsiwinstxmlplugin.exe" --xmlfile="'+trim(XMLFilename)+'" --scriptfile=' , true, 0, output);
  LogDatei.includelogtail(StandardLogPath+'opsiwinstxmlplugin.log',1000,'auto');
  DeleteFile(StandardLogPath+'opsiwinstxmlplugin.log');
    //DataModuleLogServer.IdTCPServer1.Active:=false;
end;

{$ELSE WINDOWS}
begin
  LogDatei.log('Not implemented for Linux', LLError);
end;

{$ENDIF WINDOWS}




function TuibInstScript.doFileActions (const Sektion: TWorkSection; CopyParameter : String) : TSectionResult;
 var
  Source : String='';
  Target : String='';
  SourceDirectory : String='';
  targetDirectory : String='';
  startDirectory : String='';
  presetDirectory : String='';
  cpSpecify : TcpSpecify;
  NoCounted : Integer=0;
  recursive, ignoreReadOnly : Boolean;
  finished : Boolean;
  daysback : Integer=0;
  SyntaxCheck : Boolean;
  RebootWanted : Boolean=false;

  errorinfo : String='';
  Expressionstr : String='';
  remaining : String='';

  findresultcode : Integer=0;
  //searchresult : TSearchRec;
  ch : Char;
  oldDisableWow64FsRedirectionStatus: pointer=nil;
  Wow64FsRedirectionDisabled : boolean;
  boolresult : boolean;
  ProfileList : TStringList;
  pc : integer;
  //OldWinapiErrorMode: Cardinal;

  Install : TuibFileInstall;

  procedure fileActionsMain (const Section : TXStringList; const presetDir : String);
  var
    i : integer=0;
    j: Integer=0;
    dummyint : integer=0;
    workingSection : TXStringList;
    ///info : string;
    remaining_with_leading_blanks : String='';
    search4file : boolean;
    mode : string;  // used on linux
    list1 : TStringlist;
    shellcallArchParam : String;
    go_on : boolean;


  begin
    targetDirectory := presetDir;
    targetDirectory := ExpandFileName(targetDirectory);

    workingSection := TXStringList.Create;
    workingSection.Assign(Section);
    workingSection.GlobalReplace(1, '%userprofiledir%', copy (presetDir, 1, length(presetDir) - 1), false);
    workingSection.GlobalReplace(1, '%currentprofiledir%', copy (presetDir, 1, length(presetDir) - 1), false);
    ApplyTextConstants (TXStringList (workingSection), false);
    ApplyTextVariables (TXStringList (workingSection), false);

    for i:= 1 to Sektion.count
    do
    Begin
      Remaining := cutLeftBlanks(workingSection.strings [i-1]);
      logdatei.log(Remaining, LevelDebug);
      SyntaxCheck := true;

      if (Remaining = '') or (Remaining [1] = LineIsCommentChar)
      then
       // continue
      else
      Begin
        GetWord (Remaining, Expressionstr, Remaining, WordDelimiterSet0);

        if UpperCase (Expressionstr) = 'SOURCEPATH'
        then
        begin
          if (length (Remaining) = 0) or (Remaining [1] <> '=')
          then
            reportError (Sektion, i, Sektion.strings [i-1], '=  expected')
          else
          Begin
            Remaining := copy (Remaining, 2, length (Remaining) - 1);
            Remaining := cutLeftBlanks (Remaining);

            if not GetString (Remaining, SourceDirectory, Remaining, errorinfo, false)
            then
              GetWord (Remaining, SourceDirectory, Remaining, WordDelimiterSet2);

            if length (Remaining) > 0
            then
              reportError (Sektion, i, Sektion.Strings [i-1], 'end of line expected');

            if     (length (SourceDirectory) > 0)
               and (SourceDirectory [length(SourceDirectory)] <> PathDelim)
            then  SourceDirectory := SourceDirectory + PathDelim;

          End;


        end

        else if UpperCase (Expressionstr) = 'CHECKTARGETPATH'
        then
        begin
          if (length (Remaining) = 0) or (Remaining [1] <> '=')
          then
            reportError (Sektion, i, Sektion.strings [i-1], '=  expected')
          else
          Begin
            Remaining := copy (Remaining, 2, length (Remaining) - 1);
            Remaining := cutLeftBlanks (Remaining);

            if not GetString (Remaining, TargetDirectory, Remaining, errorinfo, false)
            then
              GetWord (Remaining, TargetDirectory, Remaining, WordDelimiterSet2);

            if length (Remaining) > 0
            then
              reportError (Sektion, i, Sektion.strings [i-1], 'end of line expected')
            else
              Install.MakePath (TargetDirectory);

            if     (length (TargetDirectory) > 0)
             and (TargetDirectory [length(TargetDirectory)] <> PathDelim)
            then  TargetDirectory := TargetDirectory + PathDelim;
          End;
        end

        else if UpperCase (Expressionstr) = 'COPY'
        then
        begin
          if not GetString (Remaining, Expressionstr, Remaining, errorinfo, false)
          then
            GetWord (Remaining, Expressionstr, Remaining, WordDelimiterSet0);

          cpSpecify := 0;

            // which copy options

          if Expressionstr [1] = '-'
          then
          Begin
            Expressionstr := copy (Expressionstr, 2, length (Expressionstr) - 1);
            for j:= 1 to length (Expressionstr) do
            Begin
              if Expressionstr[j] = 's'
              then
                cpSpecify := cpSpecify or cpRecursive
              else if Expressionstr[j] = 'e'
              then
                cpSpecify := cpSpecify or cpCreateEmptySubdirectories
              else if Expressionstr[j] = 'v' then
              begin
                {$IFDEF WINDOWS}
                cpSpecify := cpSpecify or cpVersionControl;
                LogDatei.log('The copy -v Option is deprecated. Use -V',LLWarning);
                {$ELSE}
                LogDatei.log('The copy -v Option is Windows only',LLWarning);
                {$ENDIF WINDOWS}
              end
              else if Expressionstr[j] = 'V' then
              begin
                {$IFDEF WINDOWS}
                cpSpecify := cpSpecify or cpVersionControlRestricted;
                {$ELSE}
                LogDatei.log('The copy -V Option is Windows only',LLWarning);
                {$ENDIF WINDOWS}
              end
              else if Expressionstr[j] = 'd' then
              begin
                {$IFDEF WINDOWS}
                cpSpecify := cpSpecify or cpDateControl;
                {$ELSE}
                LogDatei.log('The copy -d Option is Windows only',LLWarning);
                {$ENDIF WINDOWS}
              end
              else if Expressionstr[j] = 'u' then
              begin
                {$IFDEF WINDOWS}
                cpSpecify := cpSpecify or cpUpdate;
                {$ELSE}
                LogDatei.log('The copy -u Option is Windows only',LLWarning);
                {$ENDIF WINDOWS}
              end
              else if Expressionstr[j] = 'w'
              then
                cpSpecify := cpSpecify or cpDontForceOverwrite
              else if Expressionstr[j] = 'n'
              then
                cpSpecify := cpSpecify or cpNoOverwrite
              else if Expressionstr[j] = 'h'
              then
                cpSpecify := cpSpecify or cpFollowSymlinks
              else if Expressionstr[j] = 'x'  then
              begin
                {$IFDEF WINDOWS}
                cpSpecify := cpSpecify or cpExtract;
                {$ELSE}
                LogDatei.log('The copy -x Option is Windows only',LLWarning);
                {$ENDIF WINDOWS}
              end
              else if Expressionstr[j] = 'r' then
              begin
                {$IFDEF WINDOWS}
                cpSpecify := cpSpecify or cpLeaveReadonly;
                {$ELSE}
                LogDatei.log('The copy -r Option is Windows only',LLWarning);
                {$ENDIF WINDOWS}
              end
              else if Expressionstr[j] = 'c' then
              begin
                {$IFDEF WINDOWS}
                cpSpecify := cpSpecify or cpNoExtraReboot;
                {$ELSE}
                LogDatei.log('The copy -c Option is Windows only',LLWarning);
                {$ENDIF WINDOWS}
              end
              else
              Begin
                SyntaxCheck := false;
                reportError (Sektion, i, Sektion.strings [i-1],
                             Expressionstr[j] + ' is not a valid copy option');
              End;
            End;

            if (cpSpecify and cpRecursive) = cpRecursive
            then
              cpSpecify := cpSpecify or cpCreateEmptySubdirectories;
             // creat also empty directories

           if not GetString (Remaining, Expressionstr, Remaining, errorinfo, false)
           then
              GetWord (Remaining, Expressionstr, Remaining, WordDelimiterSet0);

          End;

          if isAbsoluteFileName (Expressionstr)
          then
            Source := Expressionstr
          else
            Source := SourceDirectory + Expressionstr;
          source := ExpandFileName(source);

          LogDatei.log('source: '+source+' - target: '+target,LLDebug2);
          if not GetString (Remaining, Target, Remaining, errorinfo, false)
          then Target := Remaining;
          LogDatei.log('source: '+source+' - target: '+target,LLDebug3);
          target := trim(target);
          Target := opsiUnquoteStr(target,'"');
          Target := opsiUnquoteStr(target,'''');
          LogDatei.log('source: '+source+' - target: '+target,LLDebug3);
          if not isAbsoluteFileName (Target)
          then
          Begin
            Target := TargetDirectory + Remaining;
          End;
          LogDatei.log('source: '+source+' - target: '+target,LLDebug3);
          target := trim(target);
          LogDatei.log('source: '+source+' - target: '+target,LLDebug3);
          target := ExpandFileName(target);
          LogDatei.log('source: '+source+' - target: '+target,LLDebug3);
          if Target = ''
          then
          Begin
            SyntaxCheck := false;
            reportError (Sektion, i, Sektion.strings [i-1], 'No target directory defined');
          End;

          if SyntaxCheck
          then
          Begin
            if Install.MakePath (Target)
            then
            Begin
              Target := ExpandFileName (Target);
              Install.AllCopy  (Source, Target, cpSpecify, tccmCounting, NoCounted, RebootWanted);
              Install.AllCopy  (Source, Target, cpSpecify, tccmCounted,  NoCounted, RebootWanted);
            End;
            if RebootWanted and not( cpSpecify and cpNoExtraReboot = cpNoExtraReboot) then
            Begin
              PerformExitWindows := txrReboot; // txrRegisterForReboot; bis Version 3.03
              LogDatei.log ('', LLInfo);
              LogDatei.log ('ExitWindows set to Reboot', LLnotice);
            End;
          End;

        end

        else if (UpperCase (Expressionstr) = 'DELETE') or (UpperCase (Expressionstr) = 'DEL')
        then
        begin
          search4file := true;
          cpSpecify := 0;
          if UpperCase (Expressionstr) = 'DEL' then search4file := false;
          if not GetString (Remaining, Expressionstr, Remaining, errorinfo, false)
          then
          Begin
            LogDatei.log ('No Getstring: '+errorinfo, LLDebug3);
            LogDatei.log ('after Getstring: rem : '+remaining+' expr: '+Expressionstr, LLDebug3);
             // instead of using (as up to version 4.1)
             // GetWord (Remaining, Expressionstr, Remaining, WordDelimiterWhiteSpace);
             // we want to be exact with the number of blanks
             DivideAtFirst (' ', remaining, Expressionstr, remaining_with_leading_blanks);
             LogDatei.log ('after DivideAtFirst: rem : '+remaining_with_leading_blanks+' expr: '+Expressionstr, LLDebug3);
             remaining := cutLeftBlanks(remaining_with_leading_blanks);
          End;
          LogDatei.log ('Remaining before Options: '+remaining, LLDebug2);
          LogDatei.log ('Expressionstr before Options: '+Expressionstr, LLDebug2);
          recursive := false;
          ignoreReadOnly := false;
          daysback := 0;
          while (length(Expressionstr) > 0) and (Expressionstr [1] = '-')
          do
          Begin
            Expressionstr := lowerCase (copy (Expressionstr, 2, length (Expressionstr) - 1));
            LogDatei.log ('Expressionstr while Options: '+Expressionstr, LLDebug3);
            j := 1;
            while j <= length (Expressionstr)
            do
            Begin
              if Expressionstr[j] = 's'
              then
              begin
                recursive := true;
                LogDatei.log ('Option -s detected: ', LLDebug2);
              end
              else if Expressionstr [j] = 'f' then
              begin
                ignoreReadOnly := true;
                LogDatei.log ('Option -f detected: ', LLDebug2);
              end
              else if Expressionstr [j] = 'c' then
              begin
                {$IFDEF WINDOWS}
                cpSpecify := cpSpecify or cpNoExtraReboot;
                LogDatei.log ('Option -c detected: ', LLDebug2);
                {$ELSE}
                LogDatei.log('The del -c Option is Windows only',LLWarning);
                {$ENDIF WINDOWS}
              end
              else if Expressionstr [j] = 'd'
              then
              Begin
                LogDatei.log ('Option -d detected: ', LLDebug2);
                 finished := false;
                 while (j+1 <= length (Expressionstr)) and not finished
                 do
                 Begin
                    ch := Expressionstr [j+1];
                    if (ord ('0') <= ord (ch)) and (ord (ch) <= ord('9'))
                    then
                    Begin
                      inc (j);
                      daysback := daysback * 10 + ord (Expressionstr[j]) -ord ('0');
                    End
                    else
                      finished := true;
                 End;
                 //default
                 if daysback = 0 then daysback := 1;
              End
              else
              Begin
                SyntaxCheck := false;
                reportError (Sektion, i, Sektion.strings [i-1], Expressionstr[j] + ' is no copy option');
              End;
              inc (j);
            End;

            if not GetString (Remaining, Expressionstr, Remaining, errorinfo, false)
            then
            Begin
             DivideAtFirst (' ', remaining, Expressionstr, remaining_with_leading_blanks);
             remaining := cutLeftBlanks(remaining_with_leading_blanks);
             //GetWord (Remaining, Expressionstr, Remaining, WordDelimiterWhiteSpace);
            End;

          End;
          LogDatei.log ('Remaining: '+Remaining, LLDebug2);
          LogDatei.log ('Expressionstr: '+Expressionstr, LLDebug2);
          //remaining := cutLeftBlanks(remaining_with_leading_blanks);
          LogDatei.log ('we try to delete: '+Expressionstr, LLDebug2);
          if Remaining <> ''
          then
            Expressionstr := trim(Expressionstr + ' ' + Remaining_with_leading_blanks);
            // if file names have e.g. blanks in it, don't let them be split
            // (with the consequence of deleting files that have the first part as name)

          if isAbsoluteFileName (Expressionstr)
          then
            Source := Expressionstr
          else
            Source := SourceDirectory + Expressionstr;
          source := ExpandFileName(source);

          if SyntaxCheck
          then
          begin
           LogDatei.log ('we try to delete: '+source, LLDebug2);
           Install.AllDelete (Source, recursive, ignoreReadOnly, daysback,search4file, RebootWanted);
          end;
          if RebootWanted and not( cpSpecify and cpNoExtraReboot = cpNoExtraReboot) then
          Begin
            PerformExitWindows := txrReboot;
            LogDatei.log ('', LLInfo);
            LogDatei.log ('ExitWindows set to Reboot', LLnotice);
          End;
        end

        {$IFDEF LINUX}
        else if (UpperCase (Expressionstr) = 'CHMOD')
        then
        begin
          if not GetString (Remaining, Expressionstr, Remaining, errorinfo, false)
          then
          Begin
             // instead of using (as up to version 4.1)
             // GetWord (Remaining, Expressionstr, Remaining, WordDelimiterWhiteSpace);
             // we want to be exact with the number of blanks
             DivideAtFirst (' ', remaining, Expressionstr, remaining_with_leading_blanks);
             remaining := cutLeftBlanks(remaining_with_leading_blanks);
          End;

          mode := Expressionstr;
          mode := opsiUnquoteStr(mode,'"');
          mode := opsiUnquoteStr(mode,'''');
          if mode = '' then
          begin
            SyntaxCheck := false;
            errorinfo := errorinfo + ' Invalid empty string for mode in chmod';
          end;
          try
            dummyint := strToInt(mode);
          except
            SyntaxCheck := false;
            errorinfo := errorinfo + ' Invalid string for mode in chmod. Expected something like 755 found: '+mode;
          end;

            if not GetString (Remaining, Expressionstr, Remaining, errorinfo, false)
            then
            Begin
             DivideAtFirst (' ', remaining, Expressionstr, remaining_with_leading_blanks);
             remaining := cutLeftBlanks(remaining_with_leading_blanks);
             //GetWord (Remaining, Expressionstr, Remaining, WordDelimiterWhiteSpace);
            End;

          if Remaining <> ''
          then
            Expressionstr := trim(Expressionstr + ' ' + Remaining_with_leading_blanks);
            // if file names have e.g. blanks in it, don't let them be split
            // (with the consequence of deleting files that have the first part as name)

          if isAbsoluteFileName (Expressionstr)
          then
            Source := Expressionstr
          else
            Source := SourceDirectory + Expressionstr;
          source := ExpandFileName(source);

          if SyntaxCheck
          then
          begin
           LogDatei.log ('we try to chmod: '+source+' to mode: '+mode, LLDebug2);
           Install.chmod(mode,Source);
          end;
        end
        {$ENDIF LINUX}

        else if (UpperCase (Expressionstr) = 'HARDLINK')
        then
        begin
          go_on := true;
          if not GetString (Remaining, Expressionstr, Remaining, errorinfo, false)
          then
          Begin
             // instead of using (as up to version 4.1)
             // GetWord (Remaining, Expressionstr, Remaining, WordDelimiterWhiteSpace);
             // we want to be exact with the number of blanks
             DivideAtFirst (' ', remaining, Expressionstr, remaining_with_leading_blanks);
             remaining := cutLeftBlanks(remaining_with_leading_blanks);
          End;

          Source := Expressionstr;
          LogDatei.log('hardlink source: '+source,lldebug3);
          Source := ExpandFileNameUTF8(Source);
          if not (isAbsoluteFileName (Source) and FileExists(Source)) then
          begin
            //syntaxcheck := false;
            //reportError (Sektion, i, Sektion.strings [i-1], source + ' is no existing file or directory');
            go_on := false;
            logDatei.log('Error: hardlink: source: '+source + ' is no existing file or directory',LLError);
          end;

          if syntaxcheck and go_on  then
          begin
            if not GetString (Remaining, Target, Remaining, errorinfo, false)
            then Target := Remaining;
            LogDatei.log('source: '+source+' - target: '+target,LLDebug3);
            target := trim(target);
            Target := opsiUnquoteStr(target,'"');
            Target := opsiUnquoteStr(target,'''');
            LogDatei.log('source: '+source+' - target: '+target,LLDebug3);
            //LogDatei.log('hardlink target: '+Expressionstr,lldebug3);
            if not isAbsoluteFileName (target)
            then  target := targetDirectory + target;
            target := ExpandFileNameUTF8(target);

            if not (isAbsoluteFileName (target)) then
            begin
              //syntaxcheck := false;
              //reportError (Sektion, i, Sektion.strings [i-1], target + ' is not a valid file name');
              go_on := false;
              logDatei.log('Error: hardlink: target: '+target + ' is not a valid file name',LLError);
            end;


            if SyntaxCheck and go_on
            then
            begin
             LogDatei.log ('we try to hardlink: '+source+' to '+target, LLDebug2);
             if Install.hardlink(Source,target) then
               LogDatei.log ('hardlinked: '+source+' to '+target, LLInfo);
            end;
          end;
        end


        else if (UpperCase (Expressionstr) = 'SYMLINK')
        then
        begin
         go_on := true;
          if not GetString (Remaining, Expressionstr, Remaining, errorinfo, false)
          then
          Begin
             // instead of using (as up to version 4.1)
             // GetWord (Remaining, Expressionstr, Remaining, WordDelimiterWhiteSpace);
             // we want to be exact with the number of blanks
             DivideAtFirst (' ', remaining, Expressionstr, remaining_with_leading_blanks);
             remaining := cutLeftBlanks(remaining_with_leading_blanks);
          End;

          Source := Expressionstr;
          Source := ExpandFileNameUTF8(Source);
          if not (isAbsoluteFileName (Source) and FileExists(Source)) then
          begin
            //syntaxcheck := false;
            //reportError (Sektion, i, Sektion.strings [i-1], source + ' is no existing file or directory');
            go_on := false;
            logDatei.log('Error: symlink: source: '+source + ' is no existing file or directory',LLError);
          end;
          {$IFDEF WINDOWS}
          if GetNTVersionMajor < 6 then
          begin
            //syntaxcheck := false;
            //reportError (Sektion, i, Sektion.strings [i-1], 'Symlink on Windows is not avaible on this Release - needs NT6 and up');
            go_on := false;
            logDatei.log('Warning: symlink: Symlink on Windows is not avaible on this Release - needs NT6 and up',LLWarning);
          end;
          {$ENDIF WINDOWS}

          if syntaxcheck and go_on  then
          begin
            if not GetString (Remaining, Target, Remaining, errorinfo, false)
            then Target := Remaining;
            LogDatei.log('source: '+source+' - target: '+target,LLDebug3);
            target := trim(target);
            Target := opsiUnquoteStr(target,'"');
            Target := opsiUnquoteStr(target,'''');
            LogDatei.log('source: '+source+' - target: '+target,LLDebug3);
            if not isAbsoluteFileName (target)
            then  target := targetDirectory + target;
            target := ExpandFileNameUTF8(target);

            if not (isAbsoluteFileName (target)) then
            begin
              //syntaxcheck := false;
              //reportError (Sektion, i, Sektion.strings [i-1], target + ' is not a valid file name');
              go_on := false;
              logDatei.log('Error: symlink: target: '+target + ' is not a valid file name',LLError);
            end;


            if SyntaxCheck and go_on
            then
            begin
             LogDatei.log ('we try to symlink: '+source+' to '+target, LLDebug2);
             if Install.symlink(Source,target) then
               LogDatei.log ('symlinked: '+source+' to '+target, LLInfo);
             {$IFDEF WINDOWS}
             LogDatei.log('Reading symlink via dir to reread the cache', LLInfo);
             if Wow64FsRedirectionDisabled then shellcallArchParam := '64'
             else shellcallArchParam := '32';
             list1 := Tstringlist.create;
             list1.Clear;
             list1.Text := execShellCall('dir '+target, shellcallArchParam, 4, false,false).Text;
             //calling shellCall with FetchExitCodePublic=false result is on FLastPrivateExitCode
             list1.Free;
             {$ENDIF WINDOWS}
            end;
          end;
        end

        else if ((UpperCase (Expressionstr) = 'RENAME') or (UpperCase (Expressionstr) = 'MOVE'))
        then
        begin
          if not GetString (Remaining, Expressionstr, Remaining, errorinfo, false)
          then
            GetWord (Remaining, Expressionstr, Remaining, WordDelimiterSet0);

          cpSpecify := 0;
          go_on := true;

            // which move options

          if Expressionstr [1] = '-'
          then
          Begin
            Expressionstr := copy (Expressionstr, 2, length (Expressionstr) - 1);
            for j:= 1 to length (Expressionstr) do
            Begin
              if Expressionstr[j] = 'c' then
              begin
                {$IFDEF WINDOWS}
                cpSpecify := cpSpecify or cpNoExtraReboot;
                {$ELSE}
                LogDatei.log('The move / rename -c Option is Windows only',LLWarning);
                {$ENDIF WINDOWS}
              end
              else
              Begin
                SyntaxCheck := false;
                reportError(Sektion, i, Sektion.strings [i-1],
                             Expressionstr[j] + ' is not a valid rename / move option');
              End;
            End;
            if not GetString (Remaining, Expressionstr, Remaining, errorinfo, false) then
            begin
              //GetWord (Remaining, Expressionstr, Remaining, WordDelimiterSet0);
              // instead of using (as up to version 4.1)
              // GetWord (Remaining, Expressionstr, Remaining, WordDelimiterWhiteSpace);
              // we want to be exact with the number of blanks
             DivideAtFirst (' ', remaining, Expressionstr, remaining_with_leading_blanks);
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
          LogDatei.log('source: '+Expressionstr+' - target: '+Remaining,LLDebug3);
          Source := Expressionstr;
          Source := ExpandFileNameUTF8(Source);
          if not (isAbsoluteFileName (Source) and FileExists(Source)) then
          begin
            //syntaxcheck := false;
            //reportError (Sektion, i, Sektion.strings [i-1], source + ' is no existing file or directory');
            go_on := false;
            logDatei.log('Error: move/rename: source: '+source + ' is no existing file or directory',LLError);
          end;

          if syntaxcheck and go_on then
          begin
            remaining := cutLeftBlanks(remaining);
            if not GetString (Remaining, Target, Remaining, errorinfo, false)
            then Target := Remaining;
            LogDatei.log('source: '+source+' - target: '+Remaining,LLDebug3);
            LogDatei.log('source: '+source+' - target: '+target,LLDebug3);
            target := trim(target);
            Target := opsiUnquoteStr(target,'"');
            Target := opsiUnquoteStr(target,'''');
            LogDatei.log('source: '+source+' - target: '+target,LLDebug3);
            if not isAbsoluteFileName (target)
            then  target := targetDirectory + target;
            target := ExpandFileNameUTF8(target);

            if not (isAbsoluteFileName (target)) then
            begin
              //syntaxcheck := false;
              //reportError (Sektion, i, Sektion.strings [i-1], target + ' is not a valid file name');
              go_on := false;
              logDatei.log('Error: move/rename: target: '+target + ' is not a valid file name',LLError);
            end;

            if SyntaxCheck  and go_on
            then
            begin
             LogDatei.log ('we try to rename: '+source+' to '+target, LLDebug);
             if Install.rename(Source,target,RebootWanted) then
             begin
               LogDatei.log ('renamed: '+source+' to '+target, LLInfo);
               if RebootWanted and not( cpSpecify and cpNoExtraReboot = cpNoExtraReboot) then
               Begin
                 PerformExitWindows := txrReboot;
                 LogDatei.log ('', LLInfo);
                 LogDatei.log ('ExitWindows set to Reboot', LLnotice);
               End;
             end;
            end;
          end;
        end

        {$IFDEF WINDOWS}
        else if UpperCase (Expressionstr) = 'ZIP'
        then
        begin
          if not GetString (Remaining, Expressionstr, Remaining, errorinfo, false)
          then
              GetWord (Remaining, Expressionstr, Remaining, WordDelimiterSet0);

          recursive := false;
          if Expressionstr [1] = '-'
          then
          Begin
            Expressionstr := copy (Expressionstr, 2, length (Expressionstr) - 1);
            for j:= 1 to length (Expressionstr) do
            Begin
              if Expressionstr[j] = 's'
              then
                recursive := true
              else
              Begin
                SyntaxCheck := false;
                reportError (Sektion, i, Sektion.strings [i-1], Expressionstr[j] + ' is not a valid zip option');
              End;
            End;

            if not GetString (Remaining, Expressionstr, Remaining, errorinfo, false)
            then
              GetWord (Remaining, Expressionstr, Remaining, WordDelimiterSet0);
          End;

          if isAbsoluteFileName (Expressionstr)
          then
            Target := Expressionstr
          else
            Target := TargetDirectory + Expressionstr;

          if isAbsoluteFileName (Remaining)
          then
            Source := Remaining
          else
            Source := SourceDirectory + Remaining;

          if SyntaxCheck
          then
          Begin
            Install.MakePath (Target);
            Install.AllCompress (Source, Target, recursive, true);
          End;
        End
        {$ENDIF WINDOWS}
        else
          reportError (Sektion, i, Expressionstr, 'is not a valid command');
        end;

    end;

    workingSection.Free;

  end;

begin
  Wow64FsRedirectionDisabled := false;
  result := tsrPositive;

  if not initSection (Sektion, OldNumberOfErrors, OldNumberOfWarnings) then exit;
  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

  Install := TuibFileInstall.create;

  {$IFDEF WIN32}
  // disable  critical-error-handler message box. (Drive not ready)
  //OldWinapiErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
   if (0 < pos(lowercase (Parameter_64Bit), lowercase (CopyParameter)))
     or (0 < pos(lowercase (Parameter_SysNative), lowercase (CopyParameter))) then
   Begin
     boolresult := DSiDisableWow64FsRedirection(oldDisableWow64FsRedirectionStatus);
     Wow64FsRedirectionDisabled := true;
   End
   else Wow64FsRedirectionDisabled := false;
   {$ENDIF WIN32}

  if (0 < pos(lowercase (Parameter_AllNTUserProfiles),lowercase (CopyParameter)))
    or flag_all_ntuser
  then
  Begin
    flag_all_ntuser := false;
    ProfileList := getProfilesDirList;
    for pc:= 0 to ProfileList.Count -1 do
    begin
      presetdirectory := ProfileList.Strings[pc]+PathDelim;
      if DirectoryExistsUTF8(presetdirectory) then
      begin
        logdatei.log(' Make it for user directory: '+presetdirectory, LLInfo);
        fileActionsMain (Sektion, presetDirectory);
      end;
    end;
  End
  else if 0 < pos(lowercase(Parameter_AllNTUserSendTo), lowercase (CopyParameter))
  then
  Begin
    flag_all_ntuser := false;
    ProfileList := getProfilesDirList;
    for pc:= 0 to ProfileList.Count -1 do
    begin
      presetdirectory := ProfileList.Strings[pc] + '\SendTo\';
      fileActionsMain (Sektion, presetDirectory);
    end;
  End
  else
  Begin
    // no special preset directory
    SourceDirectory := '';
    PresetDirectory := '';
    if runLoginScripts then
       PresetDirectory := GetUserProfilePath + PathDelim;
    fileActionsMain (Sektion, presetdirectory);
    {$IFDEF WIN32}
    //setErrorMode(OldWinapiErrorMode);
    if Wow64FsRedirectionDisabled then
    Begin
      boolresult := DSiRevertWow64FsRedirection(oldDisableWow64FsRedirectionStatus);
      Wow64FsRedirectionDisabled := false;
    End;
    {$ENDIF WIN32}
  End;


  Install.free; Install := nil;

  finishSection (Sektion, OldNumberOfErrors, OldNumberOfWarnings,
                 DiffNumberOfErrors, DiffNumberOfWarnings);


  if ExitOnError and (DiffNumberOfErrors > 0)
  then result := tsrExitProcess;


  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;

end;


function TuibInstScript.doLinkFolderActions (const Sektion: TWorkSection; common : boolean)
                                                                 : TSectionResult;
 var
   {$IFDEF WINDOWS}ShellLinks : TuibShellLinks;{$ENDIF WINDOWS}
   {$IFDEF LINUX}ShellLinks : TuibLinuxDesktopFiles;{$ENDIF LINUX}
   stack : TStringList;
   startindentlevel : Integer;
   Expressionstr : String='';
   ///remaining : String;

 procedure linkActionsMain ();
  Var
   i : Integer=0;
   SyntaxCheck : boolean;
   Remaining : String='';
   errorInfo : String='';

   csidl : Integer=0;
   csidl_set : Boolean;
   folder_opened : Boolean;

   basefolder : String='';
   subfoldername : String='';
   deletefoldername : String='';
   s : String='';

   in_link_features : Boolean;
   regular_end : boolean;
   link_name : String='';
   link_target : String='';
   link_paramstr : String='';
   link_working_dir : String='';
   link_icon_file : String='';
   link_icon_index: Integer=0;
   link_categories : String='';
   link_shortcut : word=0;

 begin

    i := 1;
    if Sektion.count = 0 then exit;

    stack := TStringList.create;

    csidl := 0;
    csidl_set := false;
    folder_opened := false;
    subfoldername := '';

    while i <= Sektion.count
    do
    Begin
      Remaining := trim (Sektion.strings [i-1]);
      if (Remaining = '') or (Remaining [1] = LineIsCommentChar)
      then
        inc (i)
      else
      Begin
        GetWord (Remaining, Expressionstr, Remaining, WordDelimiterSet0);


        if LowerCase (Expressionstr) = 'set_basefolder'
        then
        begin

          syntaxcheck := true;
          if (length (Remaining) = 0)
          then
          Begin
            syntaxCheck := false;
            reportError (Sektion, i, Sektion.strings [i-1], 'folder name  expected');
          End
          else
          Begin
            if not GetString (Remaining, basefolder, Remaining, errorinfo, false)
            then
            Begin
              basefolder := Remaining;
              Remaining := '';
            End;

            if length (Remaining) > 0
            then
            Begin
              reportError (Sektion, i, Sektion.Strings [i-1], 'end of line expected');
            End;

            if producecsidlfromName (basefolder, csidl, errorinfo)
              then
              Begin
                csidl_set := true;
                LogDatei.log ('Base folder is '
                + ShellLinks.Tell_Systemfolder(csidl) , levelComplete);
              End
              else
              Begin
                LogDatei.log ('Error: ' + errorinfo, LLError);
              End;

          End;
        End

        {
        else if LowerCase (Expressionstr) = 'push_location'
        then
        begin
           if length (Remaining) > 0
           then
              reportError (Sektion, i, Sektion.Strings [i-1], 'end of line expected');
           stack.Insert(0, startdirectory);

           LogDatei.log ('Folder ' + Startdirectory + ' put on stack', levelcomplete);
        End

        else if LowerCase (Expressionstr) = 'pop_location'
        then
        begin
           if length (Remaining) > 0
           then
              reportError (Sektion, i, Sektion.Strings [i-1], 'end of line expected');

           if stack.count > 0
           then
           Begin
             startdirectory := stack.Strings[0];
             LogDatei.log ('New base folder is ' + Startdirectory, levelComplete);
             stack.Delete (0);
           End
           else
           Begin
             LogDatei.log ('No folder left on stack', levelInfo);
           End

        End
        }
        //{$IFDEF WIN32}
        else if LowerCase (Expressionstr) = 'set_subfolder'
        then
        begin
          if not getString (Remaining, subfoldername, Remaining, errorinfo, false)
          then
          Begin
             subfoldername  := Remaining;
             Remaining := '';
          End;

          if length (Remaining) > 0
           then
           Begin
              reportError (Sektion, i, Sektion.Strings [i-1], 'end of line expected');
           End;

          if csidl_set then
             Begin
                {LogDatei.log ('Subfolder is '  + subfoldername + ' in '
                   +  shellLinks.tell_systemfolder(csidl), levelComplete);
                }
                ShellLinks.OpenShellFolderPath (csidl, subfoldername);
                folder_opened := true;
             End
             else
               LogDatei.log ('No base folder set, therefore subfolder not set',
                 LLWarning);

          //startdirectory := startdirectory + '\' + subfoldername;
          //LogDatei.log ('New base folder is ' + Startdirectory, levelComplete);

        End
        {$IFDEF WIN32}
        else if LowerCase (Expressionstr) = 'delete_subfolder'
        then
        begin

          if not getString (Remaining, deletefoldername, Remaining, errorinfo, false)
          then
          Begin
             deletefoldername  := Remaining;
             Remaining := '';
          End;

          if length (Remaining) > 0
          then
          Begin
              reportError (Sektion, i, Sektion.Strings [i-1], 'end of line expected');
              syntaxCheck := false;
          End;

          if csidl_set
          then
          Begin
             LogDatei.LogSIndentLevel := startindentlevel;
             ShellLinks.DeleteShellFolder (csidl, deletefoldername);
             //csidl_set := false;
          End
          else
            LogDatei.log ('No base folder set, therefore no deletion of subfolder', LLWarning);

        End
        {$ENDIF WIN32}

        else
        if LowerCase (Expressionstr) = 'delete_element'
        then
        begin
          syntaxcheck := true;
          if not getString (Remaining, s, Remaining, errorinfo, false)
          then
          Begin
             s  := Remaining;
             Remaining := '';
          End;

          if length (Remaining) > 0
          then
          begin
              reportError (Sektion, i, Sektion.Strings [i-1], 'end of line expected');
              syntaxcheck := false;
          end;
          {$IFDEF WINDOWS}
          if folder_opened
          then
          Begin
             if syntaxcheck
             then
               ShellLinks.DeleteShellLink (s);

          End
          else
            LogDatei.log ('No folder selected, therefore no deletion of "' + s + '"', LLWarning);
          {$ENDIF WINDOWS}
          {$IFDEF LINUX}
          ShellLinks.DeleteShellLink (s);
          {$ENDIF LINUX}
        End

        else if LowerCase (Expressionstr) = 'set_link'
        then
        begin
          syntaxcheck := true;

          if length (Remaining) > 0
          then
          Begin
              reportError (Sektion, i, Sektion.Strings [i-1], 'end of line expected');
              syntaxCheck := false;
          End;
          inc (i);
          in_link_features := true;
          regular_end := false;
          link_name := '';
          link_target := '';
          link_paramstr := '';
          link_working_dir := '';
          link_icon_file := '';
          link_icon_index := 0;
          link_shortcut :=0;

          while (i <= Sektion.count) and in_link_features do
          Begin
            Remaining :=  trim (Sektion.strings [i-1]);
            if (Remaining = '') or (Remaining [1] = LineIsCommentChar)
            then
              inc (i)
            else
            Begin
              GetWord (Remaining, Expressionstr, Remaining, [':']);
              Skip (':', Remaining, Remaining, errorInfo);

              if LowerCase (Expressionstr) = 'name'
              then
              Begin
                if not getString (Remaining, link_name, Remaining, errorinfo, false)
                then
                Begin
                   link_name  := Remaining;
                   Remaining := '';
                End;
                LogDatei.log_prog('Link_name: '+link_name,LLDebug);
              End

              else if LowerCase (Expressionstr) = 'target'
              then
              Begin
                if not getString (Remaining, link_target, Remaining, errorinfo, true)
                then
                Begin
                 link_target  := Remaining;
                 Remaining := '';
                End;
                LogDatei.log_prog('link_target: '+link_target,LLDebug);
              End

              else if LowerCase (Expressionstr) = 'parameters'
              then
              Begin
                if not getString (Remaining, link_paramstr, Remaining, errorinfo, true)
                then
                Begin
                 link_paramstr  := Remaining;
                 Remaining := '';
                End;
                LogDatei.log_prog('link_paramstr: '+link_paramstr,LLDebug);
              End

              else if LowerCase (Expressionstr) = 'working_dir'
              then
              Begin
                if not getString (Remaining, link_working_dir, Remaining, errorinfo, false)
                then
                Begin
                 link_working_dir  := Remaining;
                 Remaining := '';
                End;
                LogDatei.log_prog('link_working_dir: '+link_working_dir,LLDebug);
              End


              else if LowerCase (Expressionstr) = 'working_directory'
              then
              Begin
                if not getString (Remaining, link_working_dir, Remaining, errorinfo, false)
                then
                Begin
                 link_working_dir  := Remaining;
                 Remaining := '';
                End;
                LogDatei.log_prog('link_working_dir: '+link_working_dir,LLDebug);
              End

              else if LowerCase (Expressionstr) = 'icon_file'
              then
              Begin
                if not getString (Remaining, link_icon_file, Remaining, errorinfo, false)
                then
                Begin
                 link_icon_file  := Remaining;
                 Remaining := '';
                End;
                LogDatei.log_prog('link_icon_file: '+link_icon_file,LLDebug);
              End


              else if LowerCase (Expressionstr) = 'icon_index'
              then
              Begin
                if not getString (Remaining, s, Remaining, errorinfo, false)
                then
                  s := Remaining;
                {$IFDEF LINUX}
                logdatei.log('Option icon_index is ignored at Linux',LLWarning);
                {$ENDIF LINUX}
                {$IFDEF WIN32}
                if s = ''
                then
                  link_icon_index := 0
                else
                 try
                   link_icon_index  := strtoint (s);
                   Remaining := s;
                 except
                   reportError (Sektion, i, Sektion.Strings [i-1],
                     '"' + s + '" could not converted to an integer.');
                 end;
                LogDatei.log_prog('link_icon_index: '+s,LLDebug);
                {$ENDIF WIN32}
              End

              else if LowerCase (Expressionstr) = 'shortcut'
              then
              Begin
                if not getString (Remaining, s, Remaining, errorinfo, false)
                then
                  s := Remaining;
                {$IFDEF LINUX}
                logdatei.log('Option shurtcut is ignored at Linux',LLWarning);
                {$ENDIF LINUX}
                {$IFDEF WIN32}
                if s = ''
                then
                  link_shortcut := 0
                else
                 try
                   link_shortcut  := ShortCutStringToWinApiWord (s);
                   Remaining := s;
                 except
                   reportError (Sektion, i, Sektion.Strings [i-1],
                     '"' + s + '" could not converted to a shortcut key.');
                 end;
                LogDatei.log_prog('link_shortcut: '+s,LLDebug);
                {$ENDIF WIN32}
              End

              else if LowerCase (Expressionstr) = 'link_categories'
              then
              Begin
                if not getString (Remaining, s, Remaining, errorinfo, false)
                then  s := Remaining;
                link_categories := s;
                {$IFDEF WIN32}
                logdatei.log('Option link_categories is ignored at WIN32',LLWarning);
                {$ENDIF WIN32}
              End

              else if LowerCase (Expressionstr) = 'end_link'
              then
              Begin in_link_features := false; regular_end := true End

              else
              Begin
                reportError (Sektion, i, Sektion.Strings [i-1], 'unknown Option');
                syntaxCheck := false;
              End;


              if in_link_features then inc (i);
            end;
          End;



          if not regular_end
          then
          Begin
            reportError (Sektion, i, Sektion.Strings[i-1], 'end_link missing');
            syntaxCheck := false;
          End;

          //InstallItem (Const CommandLine, ItemName, Workdir, IconPath, IconIndex : String);

          {$IFDEF WIN32}
          if csidl_set
          then
          begin
            if syntaxCheck
            then
            Begin

              if not folder_opened
              then
              Begin
                if ShellLinks.OpenShellFolderPath (csidl, subfoldername)
                then folder_opened := true
              End;

              ShellLinks.MakeShellLink(link_name,  link_target, link_paramstr,
                      link_working_dir, link_icon_file,
                      link_icon_index, link_shortcut);

            End
          end
          {$ENDIF WIN32}
          {$IFDEF LINUX}
          ShellLinks.MakeShellLink(link_name,  link_target, link_paramstr,
                      link_working_dir, link_icon_file,
                      link_categories,'','');
          {$ENDIF LINUX}
        End

        else
          reportError (Sektion, i, Expressionstr, 'is not a defined command');

        inc (i);
      End;

    end;
 end;


begin
  result := tsrPositive;

  if not initSection (Sektion, OldNumberOfErrors, OldNumberOfWarnings) then exit;

  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
  startindentlevel := LogDatei.LogSIndentLevel;

  begin  // doLinkFolder main
    {$IFDEF WIN32}
    //Install := TuibFileInstall.create;
    if runLoginScripts then
    begin
      if Impersonate2User(usercontextDomain, usercontextUser,usercontextsid) then
      begin
        try
          ShellLinks := TuibShellLinks.create;
          linkActionsMain;
          ShellLinks.free;
        finally
          RevertToSelf;
        end;
      end
      else
        LogDatei.log('Error: could not impersonate for user:' +usercontextDomain+'\'+ usercontextUser, LLError);
    end
    else
    begin
      ShellLinks := TuibShellLinks.create;
      linkActionsMain;
      ShellLinks.free;
    end;
    {$ENDIF WIN32}
    {$IFDEF LINUX}
    begin
      ShellLinks := TuibLinuxDesktopFiles.create;
      linkActionsMain;
      ShellLinks.free;
    end;
    {$ENDIF LINUX}

  end;  // eigentliche Verarbeitung

  finishSection (Sektion, OldNumberOfErrors, OldNumberOfWarnings,
                 DiffNumberOfErrors, DiffNumberOfWarnings);

  if ExitOnError and (DiffNumberOfErrors > 0)
  then result := tsrExitProcess;

  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
end;


function TuibInstScript.doDDEwithProgman (const Sektion: TWorkSection; DDEParameter : String;
                           SaveddeWithProgman : Boolean)
                           : TSectionResult;
// not implemented

begin
// not implemented
end;

function TuibInstScript.execWinBatch (const Sektion: TWorkSection; WinBatchParameter : String;
                                     WaitConditions : TSetWaitConditions;
                                     ident : String;
                                     WaitSecs : Word;
                                     runAs : TRunAs;
                                     flag_force64:boolean)
                     : TSectionResult;

Var
 i : Integer=0;
 commandline : String='';
 report : String='';
 waitForReturn : Boolean;
 StartIndentLevel : Integer=0;
 targetfilename : String='';
 ///exefilename : string;
 retrycount : integer=0;
 waitsecsAsTimeout : boolean = false;
 oldDisableWow64FsRedirectionStatus: pointer=nil;
 Wow64FsRedirectionDisabled, boolresult: boolean;

begin
  try
    result := tsrPositive;
    retrycount := 0;

    if not initSection (Sektion, OldNumberOfErrors, OldNumberOfWarnings) then exit;



    StartIndentLevel := LogDatei.LogSIndentLevel;
    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
    {$IFDEF WIN32}
    Wow64FsRedirectionDisabled := false;
    if flag_force64 then
    Begin
      boolresult := DSiDisableWow64FsRedirection(oldDisableWow64FsRedirectionStatus);
      Wow64FsRedirectionDisabled := true;
    End
    else Wow64FsRedirectionDisabled := false;
    {$ENDIF WIN32}
    {$IFDEF GUI}
    if AutoActivityDisplay then FBatchOberflaeche.showAcitvityBar(true);
    {$ENDIF GUI}


    for i:= 1 to Sektion.count
    do
    Begin
      if (Sektion.strings[i-1] = '') or (Sektion.strings[i-1] [1] = LineIsCommentChar)
      then
         // continue
      else
      Begin

        {GetWordOrStringExpressionstr (Sektion.strings[i-1], Expressionstr, Remaining, report);}

        CommandLine := trim(Sektion.strings[i-1]);
        If 1 = pos('"',CommandLine) then
        begin
          //we have a heading '"'
          // we assume, that all until the next '"' is the target file
          targetfilename := copy(CommandLine,2,posex('"',CommandLine,2)-2);
        end
        else
        begin
          //we have no heading '"'
          // we assume, that all until the next ' ' is the target file
          targetfilename := leftstr(CommandLine,pos(' ',CommandLine));
        end;
        // targetfile has no path separators we assume it is a command in the path
        // so we dont try to find it
        if (0 = pos(PathDelim,targetfilename)) and (0 = pos(PathDelim,targetfilename)) then retrycount := 99;
        // if there was no second '"' or ' ' we use the command line
        if targetfilename = '' then targetfilename := CommandLine;
        targetfilename := trim(targetfilename);

        while (not FileExists(targetfilename)) and (retrycount < 10) do
        begin
          LogDatei.log('Warning: file not found :'+targetfilename+' -retrying', LLwarning);
          FileGetAttr(targetfilename);
          ProcessMess;
          sleep(200);
          FileGetAttr(targetfilename);
          ProcessMess;
          sleep(200);
          inc(retrycount);
        end;
        if retrycount = 10 then
          LogDatei.log('Warning: file not found :'+targetfilename+' - giving up', LLwarning);

        LogDatei.log ('Call "' + CommandLine + '"', LevelComplete);

        if ttpWaitTimeout in WaitConditions then waitsecsAsTimeout := true;
        if WaitSecs > 0
        then
          if waitsecsAsTimeout then
            LogDatei.log ('   Timeout ' + IntToStr (WaitSecs) + ' seconds ', LevelComplete)
          else
            LogDatei.log ('   Waiting ' + IntToStr (WaitSecs) + ' seconds ', LevelComplete);



        if ttpWaitForWindowVanished in WaitConditions
        then
        Begin
          LogDatei.log ('   Waiting until window "' + ident + '" has vanished' , LevelComplete);
          if not StartProcess (Commandline, sw_hide, true, true, false, false, waitsecsAsTimeout, runAs, ident, WaitSecs, Report, FLastExitCodeOfExe)

          then
          Begin
            ps := 'Error: ' + Report;
            LogDatei.log (ps, LLError);
          End
          else
            LogDatei.log (Report, LevelComplete);
        End
        else if ttpWaitForWindowAppearing in WaitConditions
        then
        Begin
          LogDatei.log ('   Waiting until window "' + ident + '" is coming up' , LevelComplete);

          if not StartProcess (Commandline, sw_hide, true, false, true, false, waitsecsAsTimeout, runAs, ident, WaitSecs, Report, FLastExitCodeOfExe)
          then
          Begin
            ps := 'Error: ' + Report;
            LogDatei.log (ps, LLError);
          End
          else
            LogDatei.log (Report, LevelComplete);
        End
        else if ttpWaitForProcessEnding in WaitConditions
        then
        Begin
          LogDatei.log ('   Waiting until process "' + ident + '" started and has ended' , LevelComplete);

          if not StartProcess (Commandline, sw_hide, true, false, false, true, waitsecsAsTimeout, runAs, ident, WaitSecs, Report, FLastExitCodeOfExe)
          then
          Begin
            ps := 'Error: ' + Report;
            LogDatei.log (ps, LLError);
          End
          else
            LogDatei.log (Report, LevelComplete);
        End
        else
        Begin

          if ttpWaitOnTerminate in WaitConditions
          then
          Begin
            WaitForReturn := true;
            LogDatei.log ('   Waiting until the called process is finished', LevelComplete);
          End

          else
            WaitForReturn := false;
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

          if not StartProcess (Commandline, sw_hide, WaitForReturn, false, false, false, waitsecsAsTimeout, runAs, '', WaitSecs, Report, FLastExitCodeOfExe)
          then
          Begin
              ps := 'Error: ' + Report;
              LogDatei.log(ps, LLError);
          End
          else
              LogDatei.log (Report, LLInfo);
        End;
      End;
    end;
    {$IFDEF WIN32}
    if Wow64FsRedirectionDisabled then
    Begin
      boolresult := DSiRevertWow64FsRedirection(oldDisableWow64FsRedirectionStatus);
      Wow64FsRedirectionDisabled := false;
    End;
    {$ENDIF WIN32}
    finishSection (Sektion, OldNumberOfErrors, OldNumberOfWarnings,
                   DiffNumberOfErrors, DiffNumberOfWarnings);

    LogDatei.LogSIndentLevel := StartIndentLevel;
    if ExitOnError and (DiffNumberOfErrors > 0)
    then result := tsrExitProcess;

  finally
    {$IFDEF GUI}
    FBatchOberflaeche.showAcitvityBar(false);
    {$ENDIF GUI}
  end;
end;

{$IFDEF WINDOWS}
function TuibInstScript.execPowershellCall (command : String; archparam:string;
          logleveloffset : integer;
          FetchExitCodePublic, FatalOnFail : boolean; handle_policy : boolean): TStringlist;
Var
 commandline : String='';
 //fullps : string;
 runas : TRunAs;
 //force64: boolean;
 oldDisableWow64FsRedirectionStatus: pointer=nil;
 output: TXStringList;
 tmplist : TStringlist;
 //dummybool : boolean;
 filename : String='';
 parameters : String='';
 report : String='';
 errorinfo : String='';
 i : integer=0;
 localExitCode : LongInt = 0;
 org_execution_policy :string;
 mySektion: TWorkSection;
 ActionResult : TSectionResult;
 shortarch : string;  // for execShellCall
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
    if AutoActivityDisplay then FBatchOberflaeche.showAcitvityBar(true);
    {$ENDIF GUI}

    If (lowercase(archparam) = '64bit') and Is64BitSystem then
       shortarch := '64';

    If (lowercase(archparam) = 'sysnative') and Is64BitSystem then
         shortarch := 'sysnative';

    If (lowercase(archparam) = '32bit') then
         shortarch := '32';

    LogDatei.log ('PowerhellCall Executing: ' + command+ ' ; mode: ' +shortarch, LLNotice+logleveloffset);


    if handle_policy then // backup and set execution policy
    begin
      // backup
      commandline := 'powershell.exe get-executionpolicy';
      tmplist := execShellCall(commandline, shortarch, 1, false, true);
      org_execution_policy := trim(tmplist[0]);
      // set (open)
      commandline := 'powershell.exe set-executionpolicy RemoteSigned';
      tmplist := execShellCall(commandline, shortarch, 1, false, true);
    end;

    mySektion := TWorkSection.create(NestingLevel,ActiveSection);
    mySektion.Add('trap { write-output $_ ; exit 1 }');
    mySektion.Add(command);
    mySektion.Add('exit $LASTEXITCODE');
    mySektion.Name:='tmp-internal';
    parameters := 'powershell.exe winst /'+archparam;
    if not FetchExitCodePublic then // backup last extcode
      localExitCode := FLastExitCodeOfExe;
    ActionResult := executeWith(mySektion,parameters,true,logleveloffset+1,output);
    if not FetchExitCodePublic then  // restore last extcode
    begin
      FLastPrivateExitCode := FLastExitCodeOfExe;
      FLastExitCodeOfExe := localExitCode;
    end;
    LogDatei.log ('output:', LLDebug+logleveloffset);
    LogDatei.log ('--------------', LLDebug+logleveloffset);
    for i := 0 to output.count-1 do
    begin
     LogDatei.log (output.strings[i], LLDebug+logleveloffset);
    end;
    //LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 6;
    LogDatei.log ('', LLDebug+logleveloffset);
    result.Text := output.text;
    output.Free;

    if handle_policy then // restore execution policy
    begin
      // set (close)
      commandline := 'powershell.exe set-executionpolicy '+org_execution_policy;
      tmplist := execShellCall(commandline, shortarch, 1, false, true);
    end;
  finally
    {$IFDEF GUI}
    FBatchOberflaeche.showAcitvityBar(false);
    {$ENDIF GUI}
  end;
end;
{$ENDIF WINDOWS}

function TuibInstScript.execShellCall (command : String; archparam:string;
          logleveloffset : integer; FetchExitCodePublic : boolean): TStringlist;
begin
  result := execShellCall(command, archparam, logleveloffset, FetchExitCodePublic, true);
end;

function TuibInstScript.execShellCall (command : String; archparam:string;
          logleveloffset : integer; FetchExitCodePublic, FatalOnFail : boolean): TStringlist;
Var
 commandline : String='';
 runas : TRunAs;
 force64: boolean;
 oldDisableWow64FsRedirectionStatus: pointer=nil;
 output: TXStringList;
 dummybool : boolean;
 filename : String='';
 parameters : String='';
 report : String='';
 errorinfo : String='';
 i : integer=0;
 localExitCode : LongInt = 0;
begin
  try
    Result := TStringList.Create;
    output := TXStringList.Create;
    runAs := traInvoker;
    OldNumberOfErrors := LogDatei.NumberOfErrors;
    OldNumberOfWarnings := LogDatei.NumberOfWarnings;
    force64 := false;

    If (lowercase(archparam) = '64') and Is64BitSystem then
       force64 := true;

    If (lowercase(archparam) = 'sysnative') and Is64BitSystem then
         force64 := true;

    If (lowercase(archparam) = '32') then
         force64 := false;

    {$IFDEF WIN32}
    if force64 then
    begin
      if not FileExists(GetWinDirectory+'\cmd64.exe') then
      begin
        Logdatei.log(GetWinDirectory+'\cmd64.exe not found - try to get it', LLDebug2+logleveloffset);
        try
          if DSiDisableWow64FsRedirection(oldDisableWow64FsRedirectionStatus) then
          begin
            LogDatei.log('DisableWow64FsRedirection succeeded', LLinfo+logleveloffset);
            if FileExists(GetWinSystemDirectory+'\cmd.exe') then
            begin
             if fileutil.CopyFile(GetWinSystemDirectory+'\cmd.exe',GetWinDirectory+'\cmd64.exe') then
               LogDatei.log('cmd64.exe created in '+GetWinDirectory, LLinfo+logleveloffset)
             else
               LogDatei.log('could not get cmd64.exe', LLError);
            end
            else
              LogDatei.log('could see: '+GetWinSystemDirectory+'\cmd.exe', LLError);
            dummybool := DSiRevertWow64FsRedirection(oldDisableWow64FsRedirectionStatus);
            LogDatei.log('RevertWow64FsRedirection succeeded', LLinfo+logleveloffset);
          end
          else
          begin
            LogDatei.log('Error: DisableWow64FsRedirection failed', LLError);
          end;
        except
          on ex: Exception
          do
          Begin
            LogDatei.log ('Error: ' + ex.message, LLError);
          End;
        end;
      end;
      if not FileExists(GetWinDirectory+'\cmd64.exe') then
        LogDatei.log('no cmd64.exe - will use cmd.exe instead', LLError);
    end;
    {$ENDIF WIN32}

    if GetUibOsType (errorinfo) = tovLinux then
    begin
      FileName := '/bin/bash';
      Parameters := Parameters+' -c "' + command + ' || exit $?"';
    end
    else
    begin
     {$IFDEF WINDOWS}
       If force64 and FileExists(GetWinDirectory+'\cmd64.exe') then
         FileName := '"'+GetWinDirectory+'\cmd64.exe"'
       else
         FileName := '"cmd.exe"';
       Parameters := ' /C "' + command + '" ';
     {$ENDIF WINDOWS}
    end;
    {$IFDEF GUI}
    if AutoActivityDisplay then FBatchOberflaeche.showAcitvityBar(true);
    {$ENDIF GUI}

    commandline := FileName + ' ' +trim(Parameters);

    LogDatei.log ('ShellCall Executing: ' + commandline, LLNotice+logleveloffset);
    if not RunCommandAndCaptureOut
       (commandline,
        true,
        output, report, SW_HIDE, localExitCode, false, logleveloffset)
    then
    Begin
      if FetchExitCodePublic then FLastExitCodeOfExe := localExitCode
      else FLastPrivateExitCode := localExitcode;
      ps := 'Error: ' + Report;
      if FatalOnFail then
      begin
        LogDatei.log(ps, LLcritical);
        FExtremeErrorLevel := LevelFatal;
        scriptstopped := true;
      end
      else
      begin
        LogDatei.log(ps, LLError);
      end;
    End
    else
    begin
      if FetchExitCodePublic then FLastExitCodeOfExe := localExitCode
      else FLastPrivateExitCode := localExitcode;
      LogDatei.log ('output:', LLDebug+logleveloffset);
      LogDatei.log ('--------------', LLDebug+logleveloffset);
      for i := 0 to output.count-1 do
      begin
       LogDatei.log (output.strings[i], LLDebug+logleveloffset);
      end;
      //LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 6;
      LogDatei.log ('', LLDebug+logleveloffset);
      result.Text := output.text;
    end;
   output.Free;
  finally
    {$IFDEF GUI}
    FBatchOberflaeche.showAcitvityBar(false);
    {$ENDIF GUI}
  end;
end;


function TuibInstScript.execDOSBatch (
      const Sektion: TWorkSection; BatchParameter : String;
      ShowCmd : Integer; catchOut: Boolean; logleveloffset : integer;
      WaitConditions : TSetWaitConditions;
      var output: TXStringList)                       : TSectionResult;


Var
 SaveStayOnTop : Boolean;
 commandline : String='';
 filename : String='';
 parameters : String='';
 report : String='';
 errorinfo : String='';
 i : integer=0;
 force64, dummybool : boolean;
 winstparam : String='';
 oldDisableWow64FsRedirectionStatus: pointer=nil;
 tempfilename : String='';
 runas : TRunAs;
 showoutput : boolean = false;
 remainingstr, evaluatedstr, newbatchparastr, errorstr : string;

begin
 try
    {$IFDEF GUI}
    SaveStayOnTop := BatchScreenOnTop;
    if BatchScreenOnTop then FBatchOberflaeche.ForceStayOnTop (false);
    if AutoActivityDisplay then FBatchOberflaeche.showAcitvityBar(true);
    FBatchOberflaeche.TimerProcessMess.Enabled:= true;
    {$ENDIF GUI}

    result := tsrPositive;

    if Sektion.count = 0 then exit;

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
    CentralForm.Label2.caption := ps;
    FBatchOberflaeche.setDetailLabel(ps);
    ProcessMess;
    {$ENDIF GUI}

    LogDatei.LogSIndentLevel := Sektion.NestingLevel + 1;

    Sektion.eliminateLinesStartingWith (';', false);

    //Sektion.SaveToFile (TempPath + TempBatchdatei);
    //inc(TempBatchDatei_UniqueCount);
    //tempfilename := TempPath + TempBatchfilename + inttoStr(TempBatchDatei_UniqueCount) + '.bat';
    tempfilename := winstGetTempFileName;
    //Sektion.SaveToFile (tempfilename);
    if not Sektion.FuncSaveToFile (tempfilename) then
    begin
      LogDatei.log ('Error: Sektion could not be saved - so we switch to failed', LLcritical);
      FExtremeErrorLevel:=LevelFatal;
    end
    else
    begin
    {$IFDEF LINUX}
    fpchmod(tempfilename, &700);
    {$ENDIF LINUX}
    LogDatei.log('Content of saved file: '+tempfilename,LLDebug2);
    LogDatei.log('-----------------------',LLDebug2);
    for i := 0 to Sektion.Count-1 do
      LogDatei.log(Sektion.Strings[i],LLDebug2);
    LogDatei.log('-----------------------',LLDebug2);
    if pos('winst ', lowercase(BatchParameter)) > 0 then
    begin
      winstparam := trim(copy(BatchParameter,pos('winst ', lowercase(BatchParameter))+5,length(BatchParameter)));
      BatchParameter := trim(copy(BatchParameter,0,pos('winst ', lowercase(BatchParameter))-1));
    end;
    force64 := false;
    runAs := traInvoker;

    If (pos(lowercase('/64bit'),lowercase(winstparam)) > 0 ) and Is64BitSystem then
       force64 := true;

    If (pos(lowercase('/sysnative'),lowercase(winstparam)) > 0 ) and Is64BitSystem then
         force64 := true;

    If (pos(lowercase('/32bit'),lowercase(winstparam)) > 0 ) then
         force64 := false;

    If (pos(lowercase('/showoutput'),lowercase(winstparam)) > 0 ) then
    begin
         showoutput := true;
         LogDatei.log('Set Showoutput true', LLDebug3);
    end;

    If (pos(lowercase(ParameterRunAsLoggedOnUser),lowercase(winstparam)) > 0 ) then
    Begin
      if runLoginScripts then
        runAs := traLoggedOnUser
      else
        LogDatei.log('Warning: Not in UserLoginScript mode: /RunAsLoggedinUser ignored', LLWarning);
    End;

    {$IFDEF WIN32}
    if force64 then
    begin
      if not FileExists(GetWinDirectory+'\cmd64.exe') then
      begin
        Logdatei.log(GetWinDirectory+'\cmd64.exe not found - try to get it', LLDebug2);
        try
          if DSiDisableWow64FsRedirection(oldDisableWow64FsRedirectionStatus) then
          begin
            LogDatei.log('DisableWow64FsRedirection succeeded', LLinfo);
            if FileExists(GetWinSystemDirectory+'\cmd.exe') then
            begin
             if fileutil.CopyFile(GetWinSystemDirectory+'\cmd.exe',GetWinDirectory+'\cmd64.exe') then
               LogDatei.log('cmd64.exe created in '+GetWinDirectory, LLinfo)
             else
               LogDatei.log('could not get cmd64.exe', LLError);
            end
            else
              LogDatei.log('could see: '+GetWinSystemDirectory+'\cmd.exe', LLError);
            dummybool := DSiRevertWow64FsRedirection(oldDisableWow64FsRedirectionStatus);
            LogDatei.log('RevertWow64FsRedirection succeeded', LLinfo);
          end
          else
          begin
            LogDatei.log('Error: DisableWow64FsRedirection failed', LLError);
          end;
        except
          on ex: Exception
          do
          Begin
            LogDatei.log ('Error: ' + ex.message, LLError);
          End;
        end;
      end;
      if not FileExists(GetWinDirectory+'\cmd64.exe') then
        LogDatei.log('no cmd64.exe - will use cmd.exe instead', LLError);
    end;
    {$ENDIF WIN32}

    if BatchParameter <> '' then
    begin
      ApplyTextVariablesToString(BatchParameter,false);
    end;

      if GetUibOsType (errorinfo) = tovLinux then
      begin
       {$IFDEF GUI}
       if ShowCmd = SW_HIDE then
       begin
        FileName := '/bin/bash';
        Parameters := Parameters+' ' + tempfilename + ' ' + BatchParameter;
       end
       else
       begin
       FileName := '/usr/bin/xterm';
       Parameters := Parameters+' -e script /tmp/opsi-script-out.txt -c "/bin/bash ' + tempfilename
                     + ' ' + BatchParameter + '"';
       end
       {$ELSE GUI}
        FileName := '/bin/bash';
        Parameters := Parameters+' ' + tempfilename + ' ' + BatchParameter;
       {$ENDIF GUI}
      end
      else
      begin
       {$IFDEF WINDOWS}
       if GetUibOsType (errorinfo) = tovWinNT
       then
       Begin
         If force64 and FileExists(GetWinDirectory+'\cmd64.exe') then
           FileName := '"'+GetWinDirectory+'\cmd64.exe"'
         else
           FileName := '"cmd.exe"';
         // Quote tempfile only if contains spaces, only if not quoted parameters may be quoted
         if 0 = pos(' ',tempfilename) then
           Parameters := ' /C ' + tempfilename + ' ' + BatchParameter
         else
           Parameters := ' /C "' + tempfilename + '" ' + BatchParameter;
       End
       else  // win16
       Begin
         FileName := 'command.com';
         Parameters := ' /E:' + SpaceForEnvVarAsString +
                              ' /C ' + tempfilename + ' ' + BatchParameter;
       End;
       {$ENDIF WINDOWS}
      end;

     commandline := FileName + ' ' +trim(Parameters);

     begin
      output := TXStringlist.create;
      LogDatei.log ('Executing ' + commandline, LLDebug+logleveloffset);
      if not RunCommandAndCaptureOut
         (commandline,
          catchout,
          output, report, showcmd, FLastExitCodeOfExe,showoutput)
      then
      Begin
        // is failed
        ps := 'Error: ' + IntToStr(FLastExitCodeOfExe) + ' : '+Report;
        // retry on error 19
        if FLastExitCodeOfExe = 19 then
        begin
          ps := 'We got a Write protect error (19) - we retry';
          LogDatei.log(ps, LLError);
          ProcessMess;
          Sleep(100);
          if not RunCommandAndCaptureOut
             (commandline,
              catchout,
              output, report, showcmd, FLastExitCodeOfExe,showoutput)
          then
          Begin
            LogDatei.log(ps, LLcritical);
            FExtremeErrorLevel := LevelFatal;
            scriptstopped := true;
          end;
        // is failed
        ps := 'Error: ' + IntToStr(FLastExitCodeOfExe) + ' : '+Report;
        end
        else
        begin
          LogDatei.log(ps, LLcritical);
          FExtremeErrorLevel := LevelFatal;
          scriptstopped := true;
        end;
      End
          //
      else if catchout or (filename = '/usr/bin/xterm') then
      Begin
        if filename = '/usr/bin/xterm' then
        begin
          output.LoadFromFile('/tmp/opsi-script-out.txt');
          sysutils.DeleteFile('/tmp/opsi-script-out.txt');
        end;
        //xoutput.AddStrings(output);
        LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 4;
        LogDatei.log ('', LLDebug+logleveloffset);
        LogDatei.log ('output:', LLDebug+logleveloffset);
        LogDatei.log ('--------------', LLDebug+logleveloffset);
        for i := 0 to output.count-1 do
        begin
         LogDatei.log (output.strings[i], LLDebug+logleveloffset);
        end;
        LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 4;
        LogDatei.log ('', LLDebug+logleveloffset);
      End
     End;
    end;


    finishSection (Sektion, OldNumberOfErrors, OldNumberOfWarnings,
                   DiffNumberOfErrors, DiffNumberOfWarnings);

    if ExitOnError and (DiffNumberOfErrors > 0)
    then result := tsrExitProcess;
    {$IFDEF GUI}
    if SaveStayOnTop then FBatchOberflaeche.ForceStayOnTop (true);
    {$ENDIF GUI}
    if Logdatei.LogLevel < LLconfidential then deleteTempBatFiles(tempfilename);
  finally
    {$IFDEF GUI}
    FBatchOberflaeche.showAcitvityBar(false);
    {$ENDIF GUI}
  end;
end;


function TuibInstScript.execPython (
      const Sektion: TWorkSection; PythonParameter : String;
      catchOut : Boolean; logleveloffset : integer; WaitConditions : TSetWaitConditions;
      var output: TXStringList)                       : TSectionResult;


Var
 ///commandline, filename, parameters,
 report : String='';
 ///errorinfo: String;
 i : integer=0;
 showcmd : Integer=0;
 outlines : TXStringList;
 pythonVersionInfo : TXStringList;
 versionTestResult : String='';
 savelogsindentlevel : Integer=0;
 fatalError : Boolean;
 tempfilename : String='';


 function pythonCall (const parameters : String; var outlines : TXStringList; showcmd : Integer) : Boolean;

 begin
    result := true;
    LogDatei.log ('python ' + parameters, LLInfo+logleveloffset);

    if not RunCommandAndCaptureOut
       ('python ' + parameters,
        true,
        outlines, report, showcmd, FLastExitCodeOfExe)
    then
    Begin
      ps := 'Error: ' + Report;
      LogDatei.log(ps, LLcritical);
      FExtremeErrorLevel := LevelFatal;
      scriptstopped := true;
    End;
 end;

begin

  result := tsrPositive;
  showcmd := SW_SHOWMINIMIZED; // SW_SHOWNORMAL;
  fatalError := false;

  if Sektion.count = 0 then exit;

  LogDatei.LogSIndentLevel := Sektion.NestingLevel;
  savelogsindentlevel := LogDatei.LogSIndentLevel;

  OldNumberOfErrors := LogDatei.NumberOfErrors;
  OldNumberOfWarnings := LogDatei.NumberOfWarnings;

  ps := '';
  LogDatei.log (ps, LLNotice);
  ps := 'Execution of ' + Sektion.Name + ' ' + PythonParameter;
  LogDatei.log (ps, LLNotice);

  if pos (uppercase (PStatNames^ [tsExecutePython]), uppercase (Sektion.Name)) > 0 then
     ps := Sektion.Name;
    //ps := (* 'Ausfuehrung von ' +  *) copy (Sektion.Name, length (PStatNames^ [tsExecutePython]) + 1, length (Sektion.Name));

  {$IFDEF GUI}
  CentralForm.Label2.caption := ps;
  FBatchOberflaeche.setDetailLabel(ps);
  ProcessMess;
  {$ENDIF GUI}

  LogDatei.LogSIndentLevel := Sektion.NestingLevel + 1;

  Sektion.eliminateLinesStartingWith (';', false);

  outlines := TXStringList.create;

  if pythonCall ('--version', outlines, showcmd)
  then
  Begin
    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 6;
    if outlines.count > 0
    then
      versionTestResult := outlines.Strings[0];

    for i := 0 to outlines.count-1 do
    begin
     LogDatei.log ('::: ' + outlines.strings[i], LevelComplete);
    end;

    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 6;
    LogDatei.log ('', LLInfo);
  End;

  outlines.Clear;


  pythonVersionInfo := TXStringList.Create;
  stringsplitByWhiteSpace (versionTestResult, pythonVersionInfo);

  if (pythonVersionInfo.Count = 0)
    or (lowerCase (pythonVersionInfo.Strings[0]) <> 'python')
  then
  Begin
   LogDatei.log ('Error: python command not found', LLCritical);
   fatalError := true;
  end;

  if not fatalError then
  begin
   //Sektion.SaveToFile (TempPath + TempBatchdatei);
     //inc(TempBatchDatei_UniqueCount);
     //tempfilename := TempPath + TempBatchfilename + inttoStr(TempBatchDatei_UniqueCount) + '.bat';
     //Sektion.SaveToFile (tempfilename);
     tempfilename := winstGetTempFileName;
     if not Sektion.FuncSaveToFile (tempfilename) then
     begin
       LogDatei.log ('Error: Sektion could not be saved - so we switch to failed', LLcritical);
       FExtremeErrorLevel:=LevelFatal;
     end
     else
     begin
       output := TXStringList.create;
       if pythonCall (tempfilename +
        (* '--winstvars ' + [loglevel:loglevel, url:serviceurl, user, client, password] *) ' '
         + pythonparameter, output, showcmd)
       then
       Begin
          LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 4;
          LogDatei.log ('', LLDebug+logleveloffset);
          LogDatei.log ('output:', LLDebug+logleveloffset);
          LogDatei.log ('--------------', LLDebug+logleveloffset);

          for i := 0 to output.count-1 do
          begin
           LogDatei.log (output.strings[i], LLDebug+logleveloffset);
          end;

          LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 4;
          LogDatei.log ('', LLDebug+logleveloffset);
        End;
     end;

  end;

  finishSection (Sektion, OldNumberOfErrors, OldNumberOfWarnings,
                 DiffNumberOfErrors, DiffNumberOfWarnings);


  LogDatei.LogSIndentLevel := savelogsindentlevel;

  if ExitOnError and (DiffNumberOfErrors > 0)
  then result := tsrExitProcess;
  if Logdatei.LogLevel < LLconfidential then deleteTempBatFiles(tempfilename);
end;


function TuibInstScript.produceExecLine(const s : String;
                        var programfilename, programparas, passparas, winstoption: String;
                        var InfoSyntaxError : String): boolean;

 var
    parts : TXStringList;
    remaining : String='';
    r : String='';
    part : String='';
    partA : String='';
    partB : String='';
    continue : Boolean;

begin

  result := false;
  programfilename := '';
  programparas := '';
  passparas := '';
  winstoption := '';

  remaining := s;
  parts := TXStringList.create;

  //determine part1 until PASS
  continue := true;
  partA := '';

  while (remaining <> '') and continue
  do
  Begin
    GetWord(remaining, part, remaining, WordDelimiterWhiteSpace);
    if (lowercase(part) = lowercase(passSplitter)) or (lowercase(part) = lowercase(optionsSplitter))
    then
      continue := false
    else
      partA:= partA + ' ' + part;
  end;

  parts.add(trim(partA));


  //determine parts[1] until WINST
  continue := true;
  partB := '';

  if lowercase(part) <> lowercase(optionsSplitter)
  then
    while (remaining <> '') and continue
    do
    Begin
      GetWord(remaining, part, remaining, WordDelimiterWhiteSpace);
      if lowercase(part) = lowercase(optionsSplitter)
      then
        continue := false
      else
        partB := partB + ' ' + part;
    end;

  parts.add(trim(partB));

  parts.add(trim(remaining));

  if (parts[2]<> '')
  then
  Begin
    if not GetString(parts[2], remaining, r, InfoSyntaxError, true)
    then
      remaining := parts[2];

    continue := true;

    while (remaining <> '') and continue
    do
    Begin
      if
      not Skip(ParameterDontWait, remaining, remaining, InfoSyntaxError)
      and
      not Skip(ParameterEscapeStrings, remaining, remaining, InfoSyntaxError)
      and
      not Skip(Parameter_64Bit, remaining, remaining, InfoSyntaxError)
      and
      not Skip(Parameter_32Bit, remaining, remaining, InfoSyntaxError)
      and
      not Skip(Parameter_SysNative, remaining, remaining, InfoSyntaxError)
      then continue := false;
    end;


    if (remaining <> '') and not continue
    then
    Begin
      InfoSyntaxError := 'winst option not recognized';
      exit;
    End;

  end;

  if parts[0] = ''
  then
  Begin
    InfoSyntaxError := 'Program file name missing';
    exit;
  end;

  result := true;

  if not GetString(parts[0], programFileName, remaining, infosyntaxError, true)
  then GetWord(parts[0], programFileName, remaining, WordDelimiterWhiteSpace);

  if not EvaluateString(remaining, remaining, programParas, infoSyntaxError)
  then
    programParas := remaining;

  if not EvaluateString(parts[1], remaining, passParas, infoSyntaxError)
  then
      passParas := parts[1];

  winstOption := parts[2];

  parts.free;
end;

function TuibInstScript.executeWith (
      const Sektion: TWorkSection; ExecParameter : String;
      catchOut : Boolean; logleveloffset : integer;
      var output: TXStringList)                       : TSectionResult;


Var
 commandline : String='';
 programfilename : String='';
 programparas : String='';
 passparas : String='';
 winstoption : String='';
 report : String='';
 threaded : boolean;
 errorinfo : String='';
 i : integer=0;
 showcmd : Integer=0;
 waitSecs : Integer=0;
 tempfilename : String='';
 savelogsindentlevel : Integer=0;
 runAs : TRunAs;
 useext : string;
 force64 : boolean;
 oldDisableWow64FsRedirectionStatus: pointer=nil;
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
    result := tsrPositive;
    showcmd := SW_SHOWMINIMIZED; // SW_SHOWNORMAL;
    waitSecs := 0;

    if Sektion.count = 0 then exit;

    LogDatei.LogSIndentLevel := Sektion.NestingLevel;
    savelogsindentlevel := LogDatei.LogSIndentLevel;

    OldNumberOfErrors := LogDatei.NumberOfErrors;
    OldNumberOfWarnings := LogDatei.NumberOfWarnings;

    ps := '';
    LogDatei.log (ps, LLNotice+logleveloffset);
    ps := 'Execution of ' + Sektion.Name + ' ' +ExecParameter;
    LogDatei.log (ps, LLNotice+logleveloffset);

    if pos (uppercase (PStatNames^ [tsExecuteWith]), uppercase (Sektion.Name)) > 0 then
       ps := Sektion.Name;
      //ps := (* 'Ausfuehrung von ' +  *) copy (Sektion.Name, length (PStatNames^ [tsExecuteWith]) + 1, length (Sektion.Name));

    {$IFDEF GUI}
    CentralForm.Label2.caption := ps;
    FBatchOberflaeche.setDetailLabel(ps);
    ProcessMess;
    {$ENDIF GUI}

    LogDatei.LogSIndentLevel := Sektion.NestingLevel + 1;

    Sektion.eliminateLinesStartingWith (';', false);

    output := TXStringList.create;

    //inc(TempBatchDatei_UniqueCount);
    //tempfilename := TempPath + TempBatchfilename + inttoStr(TempBatchDatei_UniqueCount) + '.bat';
    //Sektion.SaveToFile (tempfilename);

    if ExecParameter <> '' then
    begin
      ApplyTextVariablesToString(ExecParameter,false);
    end;
    // syntax should been checked
    if not produceExecLine(
      ExecParameter,
      programfilename, programparas, passparas, winstoption,
      errorInfo)
    then
    begin
      LogDatei.log ('Error: Illegal Parameter Syntax  - so we switch to failed', LLcritical);
      FExtremeErrorLevel:=LevelFatal;
      exit;
    end;

    useext := '.cmd';
    if pos('powershell.exe',LowerCase(programfilename)) > 0  then useext := '.ps1';
    if LowerCase(programfilename) = 'powershell' then useext := '.ps1';
    tempfilename := winstGetTempFileNameWithExt(useext);

    if not Sektion.FuncSaveToFile (tempfilename) then
    begin
      LogDatei.log ('Error: Sektion could not be saved - so we switch to failed', LLcritical);
      FExtremeErrorLevel:=LevelFatal;
    end
    else
    begin
      {$IFDEF LINUX}
      fpchmod(tempfilename, &700);
      {$ENDIF LINUX}
      LogDatei.log('Content of saved file: '+tempfilename,LLDebug2);
      LogDatei.log('-----------------------',LLDebug2);
      for i := 0 to Sektion.Count-1 do
        LogDatei.log(Sektion.Strings[i],LLDebug);
      LogDatei.log('-----------------------',LLDebug2);
      // if parameters end with '=' we concat tempfile without space
      if copy(programparas,length(programparas),1) = '=' then
          commandline :=
             '"' + programfilename +  '" ' + programparas
          + '"' + tempfilename + '"  '
          + passparas
      else
        commandline :=
             '"' + programfilename +  '" ' + programparas
          + ' "' + tempfilename + '"  '
          + passparas;

      force64 := false;
      If (pos (lowercase('/64bit'), lowercase (winstoption)) > 0) and Is64BitSystem then
         force64 := true;

      If (pos (lowercase('/sysnative'), lowercase (winstoption)) > 0) and Is64BitSystem then
         force64 := true;

      If (pos (lowercase('/32bit'), lowercase (winstoption)) > 0) then
         force64 := false;

      {$IFDEF WIN32}
      Wow64FsRedirectionDisabled := false;
      if force64 then
      Begin
        boolresult := DSiDisableWow64FsRedirection(oldDisableWow64FsRedirectionStatus);
        Wow64FsRedirectionDisabled := true;
      End
      else Wow64FsRedirectionDisabled := false;
      {$ENDIF WIN32}
      {$IFDEF GUI}
      if AutoActivityDisplay then FBatchOberflaeche.showAcitvityBar(true);
      {$ENDIF GUI}


      threaded :=
        (pos (lowercase(ParameterDontWait), lowercase (winstoption)) > 0);

      if threaded
      then
      begin
        if not StartProcess (Commandline, sw_hide, false, false, false, false, runas, '', WaitSecs, Report, FLastExitCodeOfExe)
        then
        Begin
            ps := 'Error: ' + Report;
            LogDatei.log(ps, LLcritical);
            FExtremeErrorLevel := LevelFatal;
        End
        else
            LogDatei.log (Report, LevelComplete);
      end
      else
      begin
        LogDatei.log_prog ('Executing ' + commandline, LLDebug);
        if not RunCommandAndCaptureOut
           (commandline,
            true,
            output, report, showcmd, FLastExitCodeOfExe)
        then
        Begin
          ps := 'Error: ' + Report;
          LogDatei.log(ps, LLcritical);
          FExtremeErrorLevel := LevelFatal;
          scriptstopped := true;
        End
        else
        Begin
          LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 4;
          LogDatei.log ('', LLDebug+logleveloffset);
          LogDatei.log ('output:', LLDebug+logleveloffset);
          LogDatei.log ('--------------', LLDebug+logleveloffset);

          for i := 0 to output.count-1 do
          begin
           LogDatei.log (output.strings[i], LLDebug+logleveloffset);
          end;

          LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 4;
          LogDatei.log ('', LLDebug+logleveloffset);
        End;
      end;

      {$IFDEF WIN32}
      if Wow64FsRedirectionDisabled then
      Begin
        boolresult := DSiRevertWow64FsRedirection(oldDisableWow64FsRedirectionStatus);
        Wow64FsRedirectionDisabled := false;
      End;
      {$ENDIF WIN32}
    end;
    finishSection (Sektion, OldNumberOfErrors, OldNumberOfWarnings,
                   DiffNumberOfErrors, DiffNumberOfWarnings);


    LogDatei.LogSIndentLevel := savelogsindentlevel;

    if ExitOnError and (DiffNumberOfErrors > 0)
    then result := tsrExitProcess;
    if Logdatei.LogLevel < LLconfidential then
      if not threaded then deleteTempBatFiles(tempfilename);
  finally
    {$IFDEF GUI}
    FBatchOberflaeche.showAcitvityBar(false);
    {$ENDIF GUI}
  end;
end;

function TuibInstScript.produceStringList
   (const section: TuibIniScript;
   const s0 : String;
   var Remaining: String;
   var list : TXStringList;
   var InfoSyntaxError : String ) : Boolean;
//var
// NestLevel : integer;
begin
  result := produceStringList(section,s0,Remaining,list,InfoSyntaxError,Ifelseendiflevel,inDefFuncIndex);
end;

function TuibInstScript.produceStringList
   (const section: TuibIniScript;
   const s0 : String;
   var Remaining: String;
   var list : TXStringList;
   var InfoSyntaxError : String;
   var NestLevel : integer;
   const inDefFuncIndex : integer) : Boolean;

var
  VarIndex : integer=0;
  i: Integer=0;
  k: Integer=0;
  startlineofsection : Integer=0;
  syntaxCheck : boolean;
  localsyntaxcheck : boolean;
  goOn : boolean;
  a2_to_default : boolean;
  s : String='';
  r : String='';
  s1 : String='';
  s2 : String='';
  s3 : String='';
  s4 : String='';
  r1 : String='';
  ///sx,
  tmpstr, tmpstr1, tmpstr2, tmpstr3 : String;
  tmpint : integer;
  tmpbool, tmpbool1 : boolean;
  a1 : integer=0;
  a2 : Integer=0;
  list1 : TXStringList;
  list2 : TXStringList;
  list3 : TXStringList;
  slist : TStringList;
  inifile: TuibIniScript;
  localSection : TWorkSection;
  secSpec : TSectionSpecifier;
  localKindOfStatement  : TStatement;
  logstring : String='';
  savelogsindentlevel : Integer=0;
  {$IFDEF WINDOWS}
  versionInfo : TVersionInfo; //from versionInfoX
  {$ENDIF WINDOWS}
  // is needed with $OBJECTCHECKS ON
  dummyActionresult : TSectionResult=0;
  oldDisableWow64FsRedirectionStatus: pointer=nil;
  dummybool : boolean;
  Wow64FsRedirectionDisabled, boolresult : boolean;
  funcname : string;
  funcindex, funcindexvar : integer;

begin

 syntaxcheck := false;

 savelogsindentlevel := LogDatei.LogSIndentLevel;

 if Skip ('(', s0, s1, InfoSyntaxError)
 then
 begin
    if produceStringList (section, s1, r, list, InfoSyntaxError) //Recursion
    and Skip (')', r,r, InfoSyntaxError)
       then
       Begin
         syntaxCheck := true;
       End
 end

 else
 Begin
    // defined local function ?
   GetWord (s0, funcname, r, WordDelimiterSet5);
   FuncIndex := definedFunctionNames.IndexOf (LowerCase (funcname));
   GetWord (s0, s, r, WordDelimiterSet1);  // getting word s
   list := TXStringList.create; //list to return
   slist := TStringList.Create;  // if we need a real TStringlist
   VarIndex := listOfStringLists.IndexOf (LowerCase (s));
   logstring := s;

   // local variable
   if isVisibleLocalVar(s,funcindexvar)  then
   begin
     if not (definedFunctionArray[funcindexvar].getLocalVarDatatype(s) = dfpStringlist) then
     begin
       // type error
       InfoSyntaxError := 'Syntax Error: Type mismatch: Stringlist expected but the visible local variable: '
          +s+' is from type: '+osdfParameterTypesNames[definedFunctionArray[funcindexvar].getLocalVarDatatype(s)];
     end
     else
     begin
       list.Text := definedFunctionArray[funcindexvar].getLocalVarValueList(s).Text;
       syntaxCheck := true;
     end;
   end

   // global var : clone of an existing list?
   else if VarIndex >= 0 then
   begin
      list.Assign(TStringList(contentOfStringLists[VarIndex]));
      syntaxCheck := true;
   end

   // defined functions
   else if FuncIndex >= 0 then
   begin
     if not (definedFunctionArray[FuncIndex].datatype = dfpStringlist) then
     begin
       // error
       syntaxCheck := false;
       LogDatei.log('Syntax Error: defined function: '+funcname+' is not from type stringlist.',LLError);
     end
     else
     begin
       if definedFunctionArray[FuncIndex].call(r,r,NestLevel) then
       begin
         //r := ''; we may have closing brackets here
         list.Text := definedFunctionArray[FuncIndex].ResultList.Text;
         syntaxCheck := true;
         //logdatei.log('We leave the defined function: inDefFunc3: '+IntToStr(inDefFunc3),LLInfo);
       end
       else
       begin
         // defined function call failed
         LogDatei.log('Call of defined function: '+funcname+' failed',LLError);
         syntaxCheck := false;
       end;
     end;
   end

    else if LowerCase (s) = LowerCase ('shellcall')
   then
   begin
    if Skip ('(', r, r, InfoSyntaxError)
    then
     if EvaluateString (r, r, s1, InfoSyntaxError)
     then
       if Skip (')', r,r, InfoSyntaxError)
       then
       Begin
         syntaxCheck := true;
         try
           //LogDatei.log ('Executing0 ' + s1, LLInfo);
           list.Text := execShellCall(s1, 'sysnative',1, true).Text;
         except
           on e: exception do
           begin
             LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
             LogDatei.log('Error executing :'+s1+' : '+ e.message,
               LLError);
             FNumberOfErrors := FNumberOfErrors + 1;
             LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
           end
         end;
       End
   end

    else if LowerCase (s) = LowerCase ('powershellcall')
    then
    begin
      {$IFDEF Linux}
      LogDatei.log('Error powershellcall not implemented on Linux ', LLError);
      {$ENDIF Linux}
      {$IFDEF WINDOWS}
       s2 := '';
       s3 := '';
       tmpstr2 := '';
       tmpbool := true; // sysnative
       tmpbool1 := true; // handle execution policy
       syntaxCheck := false;
      if Skip ('(', r, r, InfoSyntaxError)
      then if EvaluateString (r, tmpstr, s1, InfoSyntaxError)
      // next after ',' or ')'
      then if Skip (',', tmpstr, tmpstr1, tmpstr3) then
       if EvaluateString (tmpstr1, tmpstr2, s2, tmpstr3) then;
      if s2 = '' then
      begin
       // only one parameter
       if Skip (')', tmpstr, r, InfoSyntaxError) then
       Begin
         syntaxCheck := true;
         s2 := 'sysnative';
       end;
      end
      else
      begin
       // got second parameter
        tmpbool := true;
       if lowercase(s2) = '32bit' then tmpbool := false
       else if lowercase(s2) = '64bit' then tmpbool := true
       else if lowercase(s2) = 'sysnative' then tmpbool := true
       else
       begin
         InfoSyntaxError := 'Error: unknown parameter: '+s2+' expected one of 32bit,64bit,sysnative - fall back to sysnative';
         syntaxCheck := false;
       end;
       // three parameter ?
       if Skip (',', tmpstr2, tmpstr1, tmpstr3) then
       begin
         if EvaluateString (tmpstr1, tmpstr2, s3, tmpstr3) then
         begin
          // got third parameter
           if Skip (')', tmpstr2, r, InfoSyntaxError) then
           Begin
             if TryStrToBool(s3,tmpbool1) then
               syntaxCheck := true
             else
             begin
               syntaxCheck := false;
               InfoSyntaxError := 'Error: boolean string (true/false) expected but got: '+s3;
             end;
           end;
         end;
       end
       else
       if Skip (')', tmpstr2, r, InfoSyntaxError) then
       Begin
        // two parameter
         syntaxCheck := true;
       end;
      end;
      if syntaxCheck then
       begin
         try
           list.Text := execPowershellCall(s1, s2,1, true,false,tmpbool1).Text;
         except
           on e: exception do
           begin
             LogDatei.log('Error executing :'+s1+' : with powershell: '+ e.message,
               LLError);
           end
         end;
       end;
      {$ENDIF WINDOWS}
     end


   else if LowerCase (s) = LowerCase ('LoadTextFile')
   then
   begin
    if Skip ('(', r, r, InfoSyntaxError)
    then
     if EvaluateString (r, r, s1, InfoSyntaxError)
     then
     Begin
       try
         s1 := ExpandFileName(s1);
         list.loadfromfile (s1);
         list.Text:= reencode(list.Text, 'system');
       except
         on e: exception do
         begin
           LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
           LogDatei.log('Error on loading file: ' + e.message, LLError);
           FNumberOfErrors := FNumberOfErrors + 1;
           LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
         end
       end;
       if Skip (')', r,r, InfoSyntaxError)
       then
       Begin
         syntaxCheck := true;
       End
     End
   end

   else if LowerCase (s) = LowerCase ('LoadTextFileWithEncoding')
   then
   begin
    if Skip ('(', r, r, InfoSyntaxError)
    then
     if EvaluateString (r, r, s1, InfoSyntaxError)
     then
       if Skip (',', r,r, InfoSyntaxError)
       then
        if EvaluateString (r, r, s2, InfoSyntaxError)
        then
             if Skip (')', r,r, InfoSyntaxError)
             then
             Begin
               syntaxCheck := true;
               try
                 s1 := ExpandFileName(s1);
                 //list.AddText(LoadFromFileWithEncoding(s1, s2).Text);
                 list.loadfromfile (s1);
                 list.Text:= reencode(list.Text, s2);
               except
                 on e: exception do
                 begin
                   LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
                   LogDatei.log('Error on loading file: ' + e.message, LLError);
                   FNumberOfErrors := FNumberOfErrors + 1;
                   LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
                 end
               end;
             End
   end


   else if LowerCase (s) = LowerCase ('LoadUnicodeTextFile')
   then
   begin
    if Skip ('(', r, r, InfoSyntaxError)
    then
     if EvaluateString (r, r, s1, InfoSyntaxError)
     then
     Begin
       try
         s1 := ExpandFileName(s1);
         //list.loadfromfile (s1);
         //list.Text:= reencode(list.Text, 'ucs2le');
         TStringList(list).Assign(stringListLoadUtf8FromFile(s1));
         //wsloadfromfile (s1, TStringList (list));
       except
         on e: exception do
         begin
           LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
           LogDatei.log('Error on loading file: ' + e.message,
             LLError);
           FNumberOfErrors := FNumberOfErrors + 1;
           LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
         end
       end;
       if Skip (')', r,r, InfoSyntaxError)
       then
       Begin
         syntaxCheck := true;
       End
     End
   end

   else if LowerCase (s) = LowerCase ('GetSectionNames')
   then
   begin
      if Skip ('(', r, r, InfoSyntaxError)
      and EvaluateString (r, r, s1, InfoSyntaxError)
      and Skip (')', r,r, InfoSyntaxError)
      then
      Begin
        syntaxCheck := true;
        inifile := TuibIniScript.create;
        try
          s1 := ExpandFileName(s1);
          inifile.loadfromfile (s1);
          inifile.Text:= reencode(inifile.Text, 'system');
        except
           on e: exception do
           begin
             LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
             LogDatei.log('Error on loading file: ' + e.message,
               LLError);
             FNumberOfErrors := FNumberOfErrors + 1;
             LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
           end
        end;

        inifile.GetSectionNames(list);
        inifile.free;
      end
   end


   else if LowerCase (s) = LowerCase ('retrieveSection')
   then
   begin
    if Skip ('(', r, r, InfoSyntaxError)
    then
     if EvaluateString (r, r, s1, InfoSyntaxError)
     then
     Begin

       //list.assign(TXStringList(self));  // the complete script

       (*(Tuibiniscript(self)).*)
       GetSectionLines (s1, list, startlineofsection, true, true, true);

       if Skip (')', r,r, InfoSyntaxError)
       then
       Begin
         syntaxCheck := true;
       End
     End
   end

   else if LowerCase (s) = LowerCase ('getOutstreamFromSection')
   then
   begin
    if Skip ('(', r, r, InfoSyntaxError)
    then
     if EvaluateString (r, r, s1, InfoSyntaxError)
     then
     Begin
       savelogsindentlevel := LogDatei.LogSIndentLevel;
       localSection := TWorkSection.create(LogDatei.LogSIndentLevel  + 1,ActiveSection);
       GetWord (s1, s2, r1, WordDelimiterSet1);
       localSection.Name := s2;

       localKindOfStatement := findKindOfStatement (s2, SecSpec, s1);

       if
         not
         (
         localKindOfStatement
         in
         [tsDOSBatchFile, tsDOSInAnIcon,
         tsShellBatchFile, tsShellInAnIcon,
         tsExecutePython, tsExecuteWith, tsExecuteWith_escapingStrings]
         )
       then
         InfoSyntaxError := 'not implemented for this kind of section'
       else
       Begin
         //if not (section.GetSectionLines (s2, TXStringList(localSection), startlineofsection, true, true, false)
         //  or GetSectionLines (s2, TXStringList(localSection), startlineofsection, true, true, false))
         if not SearchForSectionLines(self,TWorkSection(section),nil,s2,
                 TXStringList (localSection),startlineofsection, true, true, false)
         then
             InfoSyntaxError := 'Section "' + s2 + '" not found'
         else
         Begin
           if localKindOfStatement in [tsExecutePython, tsExecuteWith_escapingStrings]
           then
           Begin
            ApplyTextConstants (TXStringList (localSection), true);
            ApplyTextVariables (TXStringList (localSection), true);
           End
           else
           Begin
            ApplyTextConstants (TXStringList (localSection), false);
            ApplyTextVariables (TXStringList (localSection), false);
           End;

           case localKindOfStatement of

            tsExecutePython :
                  execPython(localSection, r1,
                      true {catchout}, 1,
                      [ttpWaitOnTerminate], list);

            tsExecuteWith_escapingStrings, tsExecuteWith:
                  executeWith(localSection, r1,
                      true {catchout}, 1, list);

            tsDOSBatchFile, tsDOSInAnIcon, tsShellBatchFile, tsShellInAnIcon:
                  execDOSBatch (localSection, r1,
                      SW_HIDE, true {catchout}, 1,
                      [ttpWaitOnTerminate], list);

           end;

           if Skip (')', r,r, InfoSyntaxError)
           then
           Begin
             syntaxCheck := true;
           End
         End;

       End;

       if localSection <> nil then localSection.Free;

       LogDatei.LogSIndentLevel := saveLogSIndentLevel;
     End
   end

   else if LowerCase (s) = LowerCase ('getReturnlistFromSection')
   then
   begin
    if Skip ('(', r, r, InfoSyntaxError)
    then
     if EvaluateString (r, r, s1, InfoSyntaxError)
     then
     Begin
       savelogsindentlevel := LogDatei.LogSIndentLevel;
       localSection := TWorkSection.create(LogDatei.LogSIndentLevel  + 1,ActiveSection);
       GetWord (s1, s2, r1, WordDelimiterSet1);
       localKindOfStatement := findKindOfStatement (s2, SecSpec, s1);
       if
         not (localKindOfStatement in
         [tsXMLPatch, tsOpsiServiceCall, tsLDAPsearch, tsOpsiServiceHashList]
         )
       then
         InfoSyntaxError := 'not implemented for this kind of section'
       else
       Begin
         //if not (section.GetSectionLines (s2, TXStringList(localSection), startlineofsection, true, true, true)
         //  or GetSectionLines (s2, TXStringList(localSection), startlineofsection, true, true, true))
         if not SearchForSectionLines(self,TWorkSection(section),nil,s2,
                 TXStringList (localSection),startlineofsection, true, true, false)
         then
           InfoSyntaxError := 'Section "' + s2 + '" not found'
         else
         Begin
           localsyntaxcheck := true;
           localSection.Name := s2;
           ApplyTextVariables (TXStringList (localSection), false);
           ApplyTextConstants (TXStringList (localSection), false);

           case localKindOfStatement of
             tsXMLPatch : dummyActionresult := doxmlpatch (localSection, r1, list);
             tsOpsiServiceCall : dummyActionresult := doOpsiServiceCall (localSection, r1, list);
             tsOpsiServiceHashList : dummyActionresult := doOpsiServiceHashList (localSection, r1, list);
             tsLDAPsearch :
               begin
                  if produceLDAPsearchParameters(r1, cacheRequest, outputRequest, InfoSyntaxError)
                  then
                    dummyActionresult := doLDAPSearch (localSection, cacheRequest, outputRequest, list)
                  else
                    localsyntaxcheck := false;
               end;
           end;
           localSection.Free;

           if localsyntaxcheck and (Skip (')', r,r, InfoSyntaxError))
           then
           Begin
             syntaxCheck := true;
           End
         End;
       End;
       LogDatei.LogSIndentLevel := saveLogSIndentLevel;
     End
   end


   else if LowerCase (s) = LowerCase ('editStringList')
   then
   begin
    if Skip ('(', r, r, InfoSyntaxError)
    then
     if EvaluateString (r, r, s1, InfoSyntaxError)
     then
     Begin
       if Skip (')', r,r, InfoSyntaxError)
         then
         Begin
           syntaxCheck := true;
         End
     End
   end

   else if LowerCase (s) = LowerCase ('splitStringOnWhiteSpace')
   then
   begin
    if Skip ('(', r, r, InfoSyntaxError)
    then
     if EvaluateString (r, r, s1, InfoSyntaxError)
     then
       if Skip (')', r,r, InfoSyntaxError)
       then
       Begin
          syntaxCheck := true;
          stringsplitByWhiteSpace (s1, list);
          // if s1 is confidential all parts are confidential as well
           if logdatei.isConfidential(s1) then
           begin
             for i := 0 to list.Count -1 do
             begin
               tmpstr := list.Strings[i];
               if tmpstr <> '' then
                 logdatei.AddToConfidentials(tmpstr);
             end;
           end;
       End;
   End


   else if LowerCase (s) = LowerCase ('parseUrl')
   then
   begin
    if Skip ('(', r, r, InfoSyntaxError) then
     if EvaluateString (r, r, s1, InfoSyntaxError) then
       if Skip (')', r,r, InfoSyntaxError) then
       Begin
          syntaxCheck := true;
          list.clear;
          list.AddStrings(parseUrl(s1));
       End;
   End

   else if LowerCase (s) = LowerCase ('jsonAsArrayToStringList')
   then
   begin
    if Skip ('(', r, r, InfoSyntaxError)
    then
     if EvaluateString (r, r, s1, InfoSyntaxError)
     then
     if Skip (')', r,r, InfoSyntaxError)
     then
     Begin
       syntaxCheck := true;
       try
         list.Text :=  '';
         if jsonAsArrayToStringList(s1,slist) then
           list.Text := slist.Text
        else
          LogDatei.log('Error at jsonAsArrayToStringList with: "' + s1+'"', LLerror);
       except
        list.Text :=  '';
        LogDatei.log('Error: Exception at jsonAsArrayToStringList with: "' + s1+'"', LLerror);
       end;
     End
   End

   else if LowerCase (s) = LowerCase ('jsonAsObjectGetKeyList')
   then
   begin
    if Skip ('(', r, r, InfoSyntaxError)
    then
     if EvaluateString (r, r, s1, InfoSyntaxError)
     then
     if Skip (')', r,r, InfoSyntaxError)
     then
     Begin
       syntaxCheck := true;
       try
         list.Text :=  '';
         if jsonAsObjectGetKeyList(s1,slist) then
           list.Text := slist.Text
        else
          LogDatei.log('Error at jsonAsObjectGetKeyList with: "' + s1+'"', LLerror);
       except
        list.Text :=  '';
        LogDatei.log('Error: Exception at jsonAsObjectGetKeyList with: "' + s1+'"', LLerror);
       end;
     End
   End


   else if LowerCase (s) = LowerCase ('splitString')
   then
   begin
    if Skip ('(', r, r, InfoSyntaxError)
    then
     if EvaluateString (r, r, s1, InfoSyntaxError)
     then
       if Skip (',', r,r, InfoSyntaxError)
       then
        if EvaluateString (r, r, s2, InfoSyntaxError)
        then
             if Skip (')', r,r, InfoSyntaxError)
             then
             Begin
               syntaxCheck := true;

               stringsplit (s1, s2, list);
               // if s1 is confidential all parts are confidential as well
               if logdatei.isConfidential(s1) then
               begin
                 for i := 0 to list.Count -1 do
                 begin
                   tmpstr := list.Strings[i];
                   if tmpstr <> '' then
                     logdatei.AddToConfidentials(tmpstr);
                 end;
               end;
             End
   End

   else if LowerCase (s) = LowerCase ('GetProductPropertyList')
   then
   begin
     if Skip ('(', r, r, InfoSyntaxError)
     then
       if EvaluateString (r, r, s1, InfoSyntaxError) then
       Begin
         if Skip (',', r,r1, InfoSyntaxError) then
         begin
          list1 := TXStringList.create;
          // is the second argument a valid string list ?
          if not produceStringList (section,r1, r, list1, InfoSyntaxError) then
          begin
            // if not -  is it a string Expressionstr ?
            if not EvaluateString (r1, r, s2, InfoSyntaxError) then
            begin
              LogDatei.log('Error: String Expressionstr or Stringlist expected ', LLerror);
              syntaxcheck := false;
            end
            else
            begin
              // if it is a string: make a string list with one element
              list1.Clear;
              list1.Add(s2);
              syntaxcheck := true;
            end;
          end
          else syntaxcheck := true;   // it was a string list
          if syntaxcheck then
          begin
            tmpstr := r;
            // is it the 2 argument call ?
            if Skip (')', r,r, InfoSyntaxError)
            then
            Begin
             syntaxCheck := true;
             if opsidata <> nil then
             begin
               list.AddStrings(opsidata.getProductPropertyList(s1,list1));
             end
             else if local_opsidata <> nil then
             begin
               list.AddStrings(local_opsidata.getProductPropertyList(s1,list1));
             end
             else
             begin
               LogDatei.log('No service connection in GetProductPropertyList - using default',LLDebug);
               list.Text:= list1.Text;
             end;
            End
            else
            begin
              syntaxCheck := false;
              // is it the 4 argument call ?
              if Skip (',', tmpstr,r, InfoSyntaxError) then
                if EvaluateString (r, r, s3, InfoSyntaxError) then
                  if Skip (',', r, r, InfoSyntaxError) then
                    if EvaluateString (r, r, s4, InfoSyntaxError) then
                      if Skip (')', r,r, InfoSyntaxError) then
                      begin
                         syntaxCheck := true;
                         InfoSyntaxError := '';
                         LogDatei.log('Calling with 4 arguments',LLDebug);
                         if opsidata <> nil then
                         begin
                           list.AddStrings(opsidata.getProductPropertyList(s1,list1,s3,s4));
                         end
                         else if local_opsidata <> nil then
                         begin
                           list.AddStrings(local_opsidata.getProductPropertyList(s1,list1,s3,s4));
                         end
                         else
                         begin
                           LogDatei.log('No service connection in GetProductPropertyList - using default',LLDebug);
                           list.Text:= list1.Text;
                         end;
                      end
                      else syntaxCheck := false;
            end;
         end;
       end
     end
   End


   else if LowerCase (s) = LowerCase ('getlistcontaining')
   then
   begin
    if Skip ('(', r, r, InfoSyntaxError)
    then
    Begin
       list1 := TXStringList.create;
      if produceStringList (section,r, r, list1, InfoSyntaxError) //Recursion
        and skip (',', r, r, InfoSyntaxError)
      then
      Begin
        if EvaluateString (r, r, s1, InfoSyntaxError)
        and
          skip (')', r, r, InfoSyntaxError)
        then
        Begin
          syntaxcheck := true;
           list.clear;
           for i := 0 to list1.count - 1
           do
           try
            if AnsiContainsText(list1[i], s1) then
              list.add (list1[i]);
           except
             on e: exception do
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
         End
       End
    End
   End


   else if LowerCase (s) = LowerCase ('removeFromListByMatch')
   then
   begin
    if Skip ('(', r, r1, InfoSyntaxError)
    then
    Begin
      if EvaluateString (r1, r, s1, InfoSyntaxError)
        and skip (',', r, r, InfoSyntaxError) then
      Begin
        list1 := TXStringList.create;
        if produceStringList (section,r, r, list1, InfoSyntaxError) //Recursion
           and skip (')', r, r, InfoSyntaxError) then
        Begin
          syntaxcheck := true;
           list.clear;
           for i := 0 to list1.count - 1
           do
           try
            if not (LowerCase(list1[i]) = LowerCase(s1)) then
              list.add (list1[i]);
           except
             on e: exception do
             begin
               LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
               LogDatei.log('Error on producing sublist: ' + e.message, LLerror);
               FNumberOfErrors := FNumberOfErrors + 1;
               LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
             end
           end;
           list1.Free;
           list1 := nil;
         End
       End
    End
   End


   else if LowerCase (s) = LowerCase ('removeFromListByContaining')
   then
   begin
    if Skip ('(', r, r1, InfoSyntaxError)
    then
    Begin
      list1 := TXStringList.create;
      // is the first argument a valid string list ?
      if not produceStringList (section,r1, r, list1, InfoSyntaxError) then
      begin
        // if not is it a string Expressionstr
        if not EvaluateString (r1, r, s1, InfoSyntaxError) then
        begin
          LogDatei.log('Error: String Expressionstr or Stringlist expected ', LLerror);
          syntaxcheck := false;
        end
        else
        begin
          // if it is a string: make a string list with one element
          list1.Clear;
          list1.Add(s1);
          syntaxcheck := true;
        end;
      end
      else syntaxcheck := true;   // it was a string list
      if skip (',', r, r, InfoSyntaxError) and syntaxcheck
      then
      Begin
        list2 := TXStringList.create;
        if produceStringList (section,r, r, list2, InfoSyntaxError) //Recursion
          and skip (')', r, r, InfoSyntaxError)
        then
        Begin
          syntaxcheck := true;
          list.clear;
          try
            // fill list
            for k := 0 to list2.count - 1 do list.add(list2[k]);
           // remove unwanted entries
           for i := 0 to list1.count - 1 do
             for k := 0 to list2.count - 1 do
               if AnsiContainsText(list2[k], list1[i]) then list.Delete(list.IndexOf(list2[k]));
          except
            on e: exception do
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
         End
       End
    End
   End

   else if LowerCase (s) = LowerCase ('getSubListByContainingRegex')
   then
   begin
    if Skip ('(', r, r1, InfoSyntaxError)
    then
    Begin
      if EvaluateString (r1, r, s1, InfoSyntaxError)
        and skip (',', r, r, InfoSyntaxError) then
      Begin
        list1 := TXStringList.create;
        if produceStringList (section,r, r, list1, InfoSyntaxError)
           and skip (')', r, r, InfoSyntaxError) then
        Begin
          syntaxcheck := true;
           list.clear;
           list.AddStrings(getSubListByContainingRegex(s1,list1));
           list1.Free;
           list1 := nil;
        End
      End
      else
      Begin
        list2 := TXStringList.create;
        if produceStringList(section,r1, r, list2, InfoSyntaxError)
           and skip (',', r, r, InfoSyntaxError) then
           Begin
             list3 := TXStringList.create;
             if produceStringList (section,r, r, list3, InfoSyntaxError)
                and skip (')', r, r, InfoSyntaxError) then
             Begin
               syntaxcheck := true;
               list.clear;
               list.AddStrings(getSubListByContainingRegex(list2,list3));
               list2.Free;
               list2 := nil;
               list3.Free;
               list3 := nil;
             End
           End
      End
    End
   End

   else if LowerCase (s) = LowerCase ('getRegexMatchList')
   then
   begin
    if Skip ('(', r, r1, InfoSyntaxError)
    then
    Begin
      if EvaluateString (r1, r, s1, InfoSyntaxError)
        and skip (',', r, r, InfoSyntaxError) then
      Begin
        list1 := TXStringList.create;
        if produceStringList (section,r, r, list1, InfoSyntaxError)
           and skip (')', r, r, InfoSyntaxError) then
        Begin
          syntaxcheck := true;
           list.clear;
           list.AddStrings(getRegexMatchList(s1,list1));
           list1.Free;
           list1 := nil;
        End
      End
      else
      Begin
        list2 := TXStringList.create;
        if produceStringList(section,r1, r, list2, InfoSyntaxError)
           and skip (',', r, r, InfoSyntaxError) then
           Begin
             list3 := TXStringList.create;
             if produceStringList (section,r, r, list3, InfoSyntaxError)
                and skip (')', r, r, InfoSyntaxError) then
             Begin
               syntaxcheck := true;
               list.clear;
               list.AddStrings(getRegexMatchList(list2,list3));
               list2.Free;
               list2 := nil;
               list3.Free;
               list3 := nil;
             End
           End
      End
    End
   End


   else if LowerCase (s) = LowerCase ('removeFromListByContainingRegex')
   then
   begin
    if Skip ('(', r, r1, InfoSyntaxError)
    then
    Begin
      if EvaluateString (r1, r, s1, InfoSyntaxError)
        and skip (',', r, r, InfoSyntaxError) then
      Begin
        list1 := TXStringList.create;
        if produceStringList (section,r, r, list1, InfoSyntaxError)
           and skip (')', r, r, InfoSyntaxError) then
        Begin
          syntaxcheck := true;
           list.clear;
           list.AddStrings(removeFromListByContainingRegex(s1,list1));
           list1.Free;
           list1 := nil;
        End
      End
      else
      Begin
        list2 := TXStringList.create;
        if produceStringList(section,r1, r, list2, InfoSyntaxError)
           and skip (',', r, r, InfoSyntaxError) then
           Begin
             list3 := TXStringList.create;
             if produceStringList (section,r, r, list3, InfoSyntaxError)
                and skip (')', r, r, InfoSyntaxError) then
             Begin
               syntaxcheck := true;
               list.clear;
               list.AddStrings(removeFromListByContainingRegex(list2,list3));
               list2.Free;
               list2 := nil;
               list3.Free;
               list3 := nil;
             End
           End
      End
    End
   End


   else if LowerCase (s) = LowerCase ('getSubListByContaining')
   then
   begin
    if Skip ('(', r, r1, InfoSyntaxError)
    then
    Begin
      list1 := TXStringList.create;
      // is the first argument a valid string list ?
      if not produceStringList (section,r1, r, list1, InfoSyntaxError) then
      begin
        // if not is it a string Expressionstr
        if not EvaluateString (r1, r, s1, InfoSyntaxError) then
        begin
          LogDatei.log('Error: String Expressionstr or Stringlist expected ', LLerror);
          syntaxcheck := false;
        end
        else
        begin
          // if it is a string: make a string list with one element
          list1.Clear;
          list1.Add(s1);
          syntaxcheck := true;
        end;
      end
      else syntaxcheck := true;   // it was a string list
      if skip (',', r, r, InfoSyntaxError) and syntaxcheck
      then
      Begin
        list2 := TXStringList.create;
        if produceStringList (section,r, r, list2, InfoSyntaxError) //Recursion
          and skip (')', r, r, InfoSyntaxError)
        then
        Begin
          syntaxcheck := true;
          list.clear;
          try
            // fill list
           for i := 0 to list1.count - 1 do
             for k := 0 to list2.count - 1 do
               if AnsiContainsText(list2[k], list1[i]) then
                 list.Add(list2.Strings[k]);
          except
            on e: exception do
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
         End
       End
    End
   End

   else if LowerCase (s) = LowerCase ('getSubListByKey')
   then
   begin
    if Skip ('(', r, r1, InfoSyntaxError)
    then
    Begin
      list1 := TXStringList.create;
      // is the first argument a valid string list ?
      if not produceStringList (section,r1, r, list1, InfoSyntaxError) then
      begin
        // if not is it a string Expressionstr
        if not EvaluateString (r1, r, s1, InfoSyntaxError) then
        begin
          LogDatei.log('Error: String Expressionstr or Stringlist expected ', LLerror);
          syntaxcheck := false;
        end
        else
        begin
          // if it is a string: make a string list with one element
          list1.Clear;
          list1.Add(s1);
          syntaxcheck := true;
        end;
      end
      else syntaxcheck := true;   // it was a string list
      if skip (',', r, r, InfoSyntaxError) and syntaxcheck
      then
      Begin
        list2 := TXStringList.create;
        if produceStringList (section,r, r, list2, InfoSyntaxError) //Recursion
          and skip (')', r, r, InfoSyntaxError)
        then
        Begin
          syntaxcheck := true;
          list.clear;
          try
            // fill list
           for i := 0 to list1.count - 1 do
             for k := 0 to list2.count - 1 do
               if AnsiStartsText(list1.Strings[i]+'=', list2.Strings[k]) then
                 list.Add(list2.Strings[k]);
          except
            on e: exception do
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
         End
       End
    End
   End


   else if LowerCase (s) = LowerCase ('getSubListByMatch')
   then
   begin
    if Skip ('(', r, r1, InfoSyntaxError)
    then
    Begin
      list1 := TXStringList.create;
      // is the first argument a valid string list ?
      if not produceStringList (section,r1, r, list1, InfoSyntaxError) then
      begin
        // if not is it a string Expressionstr
        if not EvaluateString (r1, r, s1, InfoSyntaxError) then
        begin
          LogDatei.log('Error: String Expressionstr or Stringlist expected ', LLerror);
          syntaxcheck := false;
        end
        else
        begin
          // if it is a string: make a string list with one element
          list1.Clear;
          list1.Add(s1);
          syntaxcheck := true;
        end;
      end
      else syntaxcheck := true;   // it was a string list
      if skip (',', r, r, InfoSyntaxError) and syntaxcheck
      then
      Begin
        list2 := TXStringList.create;
        if produceStringList (section,r, r, list2, InfoSyntaxError) //Recursion
          and skip (')', r, r, InfoSyntaxError)
        then
        Begin
          syntaxcheck := true;
          list.clear;
          try
            // fill list
           for i := 0 to list1.count - 1 do
             for k := 0 to list2.count - 1 do
               if lowercase(list1.Strings[i]) = lowercase(list2.Strings[k]) then
                 list.Add(list1.Strings[i]);
          except
            on e: exception do
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
         End
       End
    End
   End



   else if LowerCase (s) = LowerCase ('reencodestrlist')
   then
   begin
    if Skip ('(', r, r, InfoSyntaxError)
    then
    Begin
      list1 := TXStringList.create;
      if produceStringList (section,r, r, list1, InfoSyntaxError) //Recursion
      then
       if Skip (',', r,r, InfoSyntaxError)
         then
          if EvaluateString (r,r, s1, InfoSyntaxError)
          then
            if Skip (',', r,r, InfoSyntaxError)
            then
             if EvaluateString (r,r, s3, InfoSyntaxError)
             then
              if Skip (')', r,r, InfoSyntaxError)
              then
              Begin
                syntaxCheck := true;
                 list.clear;
                 try
                    // s2 is here used source encoding
                    list.Text:= reencode(list1.Text,s1,s2,s3);
                 except
                   on e: exception do
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
               End;
      list1.Free;
      list1 := nil;
    End
   End

   else if LowerCase (s) = LowerCase ('stringReplaceRegexInList')
   then
   begin
    if Skip ('(', r, r, InfoSyntaxError) then
    Begin
      list1 := TXStringList.create;
      if produceStringList (section,r, r, list1, InfoSyntaxError) then
       if Skip (',', r,r, InfoSyntaxError) then
         if EvaluateString (r,r, s1, InfoSyntaxError) then
            if Skip (',', r,r, InfoSyntaxError) then
             if EvaluateString (r,r, s2, InfoSyntaxError) then
              if Skip (')', r,r, InfoSyntaxError) then
              Begin
                syntaxCheck := true;
                 list.clear;
                 list.AddStrings(stringReplaceRegexInList(list1, s1, s2));
              End;
      list1.Free;
      list1 := nil;
    End
   End

   else if LowerCase (s) = LowerCase ('setStringInListAtIndex')
   then
   begin
    if Skip ('(', r, r, InfoSyntaxError)
    then
    Begin
      if EvaluateString (r,r, s1, InfoSyntaxError)
      then
       if Skip (',', r,r, InfoSyntaxError)
       then
         list1 := TXStringList.create;
         if produceStringList (section,r, r, list1, InfoSyntaxError) //Recursion
         then
            if Skip (',', r,r, InfoSyntaxError)
            then
             if EvaluateString (r,r, s2, InfoSyntaxError)
             then
              if Skip (')', r,r, InfoSyntaxError)
              then
              Begin
                syntaxCheck := true;
                 list.clear;
                 try
                    if strtoint(s2) < list1.Count then
                      list1.Strings[strtoint(s2)] := s1
                    else list1.Add(s1);
                    list.Text:= list1.Text;
                 except
                   on e: exception do
                   begin
                     LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
                     LogDatei.log('Exception: Error on setStringInListAtIndex: ' + e.message,
                       LLerror);
                     list.Text:= '';
                     FNumberOfErrors := FNumberOfErrors + 1;
                     LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
                     list1.Free;
                     list1 := nil;
                   end
                 end;
               End;
      list1.Free;
      list1 := nil;
    End
   End

   else if LowerCase (s) = LowerCase ('addtolist')
   then
   begin
    if Skip ('(', r, r, InfoSyntaxError)
    then
    Begin
       list1 := TXStringList.create;
      if produceStringList (section,r, r, list1, InfoSyntaxError) //Recursion
        and skip (',', r, r, InfoSyntaxError)
      then
      Begin
        if EvaluateString (r, r, s1, InfoSyntaxError)
        and
          skip (')', r, r, InfoSyntaxError)
        then
        Begin
          syntaxcheck := true;
           list.clear;
           //list.AddStrings(list1.lines);
           try
            //for i := 0 to list1.count - 1
              //do list.add (list1[i]);
              list.AddStrings(list1);
            list.Add(s1);
           except
             on e: exception do
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
         End
       End
    End
   End

   else if LowerCase (s) = LowerCase ('addlisttolist')
   then
   begin
    if Skip ('(', r, r, InfoSyntaxError)
    then
    Begin
       list1 := TXStringList.create;
      if produceStringList (section,r, r, list1, InfoSyntaxError) //Recursion
        and skip (',', r, r, InfoSyntaxError)
      then
      Begin
        list2 := TXStringList.create;
        if produceStringList (section,r, r, list2, InfoSyntaxError) //Recursion
          and skip (')', r, r, InfoSyntaxError)
        then
        Begin
          syntaxcheck := true;
          list.clear;
          try
            list.AddStrings(list1);
            list.AddStrings(list2);
          except
            on e: exception do
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
         End
       End
    End
   End

   else if LowerCase (s) = LowerCase('getListContainingList')
   then
   begin
    if Skip ('(', r, r, InfoSyntaxError)
    then
    Begin
       list1 := TXStringList.create;
      if produceStringList (section,r, r, list1, InfoSyntaxError) //Recursion
        and skip (',', r, r, InfoSyntaxError)
      then
      Begin
        list2 := TXStringList.create;
        if produceStringList (section,r, r, list2, InfoSyntaxError) //Recursion
          and skip (')', r, r, InfoSyntaxError)
        then
        Begin
          syntaxcheck := true;
          list.clear;
          try
           for i := 0 to list1.count - 1 do
             for k := 0 to list2.count - 1 do
               if list1.Strings[i] = list2.Strings[k] then
                list.Add(list1.Strings[i]);
          except
            on e: exception do
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
         End
       End
    End
   End



   else if LowerCase (s) = LowerCase ('getsublist')
   then
   begin
    if Skip ('(', r, r, InfoSyntaxError)
    then
    Begin
       syntaxcheck := true;
       // syntax: "{d}:[-]{d}, listvalue , that is any number of digits followed by a ':'
       // followed by again any number of digits, followed by a comma, then the listvalue
       // if the first series of digits is empty, the start index is zero
       // if the second series of digits is empty, the last index is count - 1

       try
         GetWord (r, s1, r, [':']);
         if length (s1) = 0
         then
           a1 := 0
         else
           a1 := strtoint(trim(s1));

         syntaxCheck := Skip(':', r, r, InfoSyntaxError);
         GetWord (r, s1, r, [',']);

         a2_to_default := false;
         if length (s1) = 0
         then
         begin
           a2 := 0;
           a2_to_default := true;
         end
         else
           a2 := strtoint(trim(s1));

         syntaxCheck := Skip(',', r, r, InfoSyntaxError);

       except
         syntaxcheck := false;
         InfoSyntaxError := ' No valid sublist selection ';
       end;

       if syntaxCheck then
       Begin
         list1 := TXStringList.create;

         if not produceStringList (section,r, r, list1, InfoSyntaxError) //Recursion
         or not Skip (')', r,r, InfoSyntaxError)
         then
            syntaxCheck := false
         else
         Begin
           list.clear;

           if a2_to_default
           then a2 := list1.count - 1;
           adjustBounds (a1, a2, list1);

           for i := a1 to a2
           do
           try
             list.add (list1[i]);
           except
             on e: exception do
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
         End
       End
    End
   End

   else if LowerCase (s) = LowerCase ('createStringList')
   then
   begin
    if Skip ('(', r, r, InfoSyntaxError)
    then
    Begin
      if length (r) = 0
      then
        InfoSyntaxError := ' ")" missing '
      else
      Begin
        goOn := true;
        syntaxCheck := true;
      End;

      while syntaxCheck and goon
      do
      Begin
          evaluateString (r, r, s1, InfoSyntaxError);
          if length(s1) > 0
          then
            list.add (s1);

          if length(r) = 0
          then
          Begin
            syntaxCheck := false;
            InfoSyntaxError := ' Expressionstr not terminated ';
          End
          else
          Begin
           if r[1] = ','
           then
             Skip (',', r, r, InfoSyntaxError) //here is no syntax error possible
           else if r[1] = ')'
           then
           Begin
             goOn := false;
             Skip (')', r, r, InfoSyntaxError) //consume the ')'
           End
           else
           Begin
             syntaxCheck := false;
             InfoSyntaxError := ' "," or ")" expected ';
           End;
          End;
      End
    end
   end

   else if LowerCase (s) = LowerCase ('reverse')
   then
   begin
    if Skip ('(', r, r, InfoSyntaxError)
    then
    begin
      list1 := TXStringList.create;
      if produceStringList (section,r, r, list1, InfoSyntaxError) //Recursion
      then
      Begin
        list.clear;
        for i := list1.count downto 1
        do
          list.add (list1[i-1]);
        if Skip (')', r, r, InfoSyntaxError)
        then
          syntaxCheck := true;
      End;
      list1.free;
    end;
   end

   else if LowerCase (s) = LowerCase ('getKeyList')
   then
   begin
    if Skip ('(', r, r, InfoSyntaxError)
    then
    begin
      list1 := TXStringList.create;
      if produceStringList (section,r, r, list1, InfoSyntaxError) //Recursion
      then
      Begin
        if Skip (')', r, r, InfoSyntaxError) then
        begin
          syntaxCheck := true;
          list.clear;
          for i := 0 to list1.count - 1 do
          begin
           k := pos('=',list1[i]);
           if k = 0 then k:= length(list1[i]);
           list.add (copy(list1[i],1,k-1));
          end;
        end;
      end;
      list1.free;
    end;
   end

   else if LowerCase (s) = LowerCase ('emptyList')
   then
   begin
    if Skip ('(', r, r, InfoSyntaxError)
    then
    begin
      // we do not need it - but for syntax check reasons it has to be done
      list1 := TXStringList.create;
      if produceStringList (section,r, r, list1, InfoSyntaxError) //Recursion
      then
      Begin
        list.clear;
        if Skip (')', r, r, InfoSyntaxError) then
          syntaxCheck := true;
      end;
      list1.free;
    end;
   end


   else if LowerCase (s) = LowerCase ('editMap')
   then
   begin
    if Skip ('(', r, r, InfoSyntaxError)
    then
    begin
      // we do not need it - but for syntax check reasons it has to be done
      list1 := TXStringList.create;
      if produceStringList (section,r, r, list1, InfoSyntaxError) //Recursion
      then
      Begin
        list.Clear;
        {$IFDEF GUI}
        list.AddStrings(checkMapGUI(list1));
        {$ELSE GUI}
        for i:= 0 to list1.Count-1 do
        begin
          s1 := list1.Strings[i];
          s2 := Copy(s1,1,pos('=',s1)-1);
          s3 := Copy(s1,pos('=',s1)+1,length(s1));
          if not cmdLineInputDialog(s3, s2, s3, false) then
            Logdatei.log('Error editMap (noGUI) for: '+s1,LLError);
          list.Add(s2+'='+s3);
        end;
        {$ENDIF GUI}
        if Skip (')', r, r, InfoSyntaxError) then
          syntaxCheck := true;
      end;
      list1.free;
    end;
   end


   else if LowerCase(s) = LowerCase ('getProductMap')
   then
   Begin
     //if (r = '') or (r=')') then
     Begin
      syntaxcheck := true;
      if not (opsidata = nil) then
      begin
        list.add ('id='+Topsi4data(opsidata).getActualProductId);
        list.add ('name='+Topsi4data(opsidata).getActualProductName);
        list.add ('description='+Topsi4data(opsidata).getActualProductDescription);
        list.add ('advice='+Topsi4data(opsidata).getActualProductAdvice);
        list.add ('productversion='+Topsi4data(opsidata).getActualProductProductVersion);
        list.add ('packageversion='+Topsi4data(opsidata).getActualProductPackageVersion);
        list.add ('priority='+Topsi4data(opsidata).getActualProductPriority);
        list.add ('installationstate='+Topsi4data(opsidata).getActualProductInstallationState);
        list.add ('lastactionrequest='+Topsi4data(opsidata).getActualProductLastActionRequest);
        list.add ('lastactionresult='+Topsi4data(opsidata).getActualProductProductLastActionReport);
        list.add ('installedversion='+Topsi4data(opsidata).getActualProductInstalledVersion);
        list.add ('installedpackage='+Topsi4data(opsidata).getActualProductInstalledPackage);
        list.add ('installedmodificationtime='+Topsi4data(opsidata).getActualProductInstalledModificationTime);
        list.add ('actionrequest='+ Topsi4data(opsidata).getActualProductActionRequest);
       end;
     end;
   end

   {$IFDEF WIN32}
   else if LowerCase(s) = LowerCase ('getIpMacMap')
   //does not work (do 15.6.2012)
   then
   Begin
     Begin
      syntaxcheck := true;
      list.AddStrings(getIpMacHash);
     end;
   end
   {$ENDIF WIN32}

   else if LowerCase(s) = LowerCase ('getLocaleInfo')
   then
   Begin
     //if (r = '') or (r=')') then
     Begin
      syntaxcheck := true;
      {$IFDEF WINDOWS}
      list.add (copy (GetSystemDefaultLocale(LOCALE_SABBREVLANGNAME), 1, 2));
      list.add (GetSystemDefaultLocale(LOCALE_SABBREVLANGNAME));
      list.add (GetSystemDefaultLocale(LOCALE_SENGLANGUAGE));
      list.add (GetSystemDefaultLocale(LOCALE_SENGCOUNTRY));
      list.add (GetSystemDefaultLocale(LOCALE_ILANGUAGE));
      list.add (GetSystemDefaultLocale(LOCALE_SNATIVELANGNAME));
      list.add (GetSystemDefaultLocale(LOCALE_IDEFAULTLANGUAGE));
      //list.add (GetSystemDefaultLocale(LOCALE_NOUSEROVERRIDE));
      {$ENDIF WINDOWS}
     end;
   end

   else if LowerCase(s) = LowerCase ('getLocaleInfoMap')
   then
   Begin
     //if (r = '') or (r=')') then
     Begin
      syntaxcheck := true;
      {$IFDEF WINDOWS}
      list.add ('language_id_2chars=' + copy (GetSystemDefaultLocale(LOCALE_SABBREVLANGNAME), 1, 2));
      list.add ('language_id=' + GetSystemDefaultLocale(LOCALE_SABBREVLANGNAME));
      list.add ('localized_name_of_language=' + GetSystemDefaultLocale(LOCALE_SLANGUAGE));
      list.add ('English_name_of_language=' + GetSystemDefaultLocale(LOCALE_SENGLANGUAGE));
      list.add ('abbreviated_language_name=' + GetSystemDefaultLocale(LOCALE_SABBREVLANGNAME));
      list.add ('native_name_of_language=' + GetSystemDefaultLocale(LOCALE_SNATIVELANGNAME));

      list.add ('country_code=' + GetSystemDefaultLocale(LOCALE_ICOUNTRY));
      list.add ('localized_name_of_country=' + GetSystemDefaultLocale(LOCALE_SCOUNTRY));
      list.add ('English_name_of_country=' + GetSystemDefaultLocale(LOCALE_SENGCOUNTRY));
      list.add ('abbreviated_country_name=' + GetSystemDefaultLocale(LOCALE_SABBREVCTRYNAME));
      list.add ('native_name_of_country=' + GetSystemDefaultLocale(LOCALE_SNATIVECTRYNAME));
      list.add ('default_language_id=' + GetSystemDefaultLocale(LOCALE_IDEFAULTLANGUAGE));
      list.add ('default_language_id_decimal=' + IntToStr(StrToInt('$'+GetSystemDefaultLocale(LOCALE_IDEFAULTLANGUAGE))));
      list.add ('default_country_code=' + GetSystemDefaultLocale(LOCALE_IDEFAULTCOUNTRY));
      list.add ('default_oem_code_page=' + GetSystemDefaultLocale(LOCALE_IDEFAULTCODEPAGE));
      list.add ('default_ansi_code_page=' + GetSystemDefaultLocale(LOCALE_IDEFAULTANSICODEPAGE));
      tmpstr := GetRegistrystringvalue('HKEY_LOCAL_MACHINE\System\CurrentControlset\Control\Nls\Language','InstallLanguage',false);
      list.add ('system_default_language_id=' + tmpstr);
      //list.add ('system_default_UI_language=' + GetSystemDefaultUILanguage );
      list.add ('system_default_posix=' +  StringReplace(getlangcodeByHexvalueStr('0x'+tmpstr),'-','_'));
      list.add ('system_default_lang_region=' + getlangcodeByHexvalueStr('0x'+tmpstr));
      {$ENDIF WINDOWS}
     end;
   end

  {$IFDEF WINDOWS}
  else if LowerCase(s) = LowerCase ('getMSVersionMap')
   then
   Begin
     //if (r = '') or (r=')') then
     Begin
      syntaxcheck := true;
      list.add ('major_version=' + GetSystemOSVersionInfoEx('major_version'));
      list.add ('minor_version=' + GetSystemOSVersionInfoEx('minor_version'));
      list.add ('build_number=' + GetSystemOSVersionInfoEx('build_number'));
      list.add ('platform_id=' + GetSystemOSVersionInfoEx('platform_id'));
      list.add ('csd_version=' + GetSystemOSVersionInfoEx('csd_version'));
      list.add ('service_pack_major=' + GetSystemOSVersionInfoEx('service_pack_major'));
      list.add ('service_pack_minor=' + GetSystemOSVersionInfoEx('service_pack_minor'));
      list.add ('suite_mask=' + GetSystemOSVersionInfoEx('suite_mask'));
      list.add ('product_type_nr=' + GetSystemOSVersionInfoEx('product_type_nr'));
      list.add ('2003r2=' + GetSystemOSVersionInfoEx('2003r2'));
      if  GetNTVersionMajor < 6 then
      begin
        list.add ('ReleaseID=');
        list.add ('prodInfoNumber=');
        list.add ('prodInfoText=');
      end
      else
      begin
        if  GetNTVersionMajor >= 10 then
          if RegVarExists('HKLM\SOFTWARE\Microsoft\Windows NT\CurrentVersion','ReleaseID',true) then
            list.add ('ReleaseID=' + GetRegistrystringvalue('HKLM\SOFTWARE\Microsoft\Windows NT\CurrentVersion','ReleaseID',true))
          else list.add ('ReleaseID=1507')
        else list.add ('ReleaseID=');
        tmpint := OSGetProductInfoNum;
        list.add ('prodInfoNumber=' + IntToStr(tmpInt));
        list.add ('prodInfoText=' + getProductInfoStrByNum(tmpInt));
      end;
     end;
   end
   {$ENDIF WINDOWS}

   {$IFDEF LINUX}
   else if LowerCase(s) = LowerCase ('getLinuxVersionMap')
    then
    Begin
      Begin
       syntaxcheck := true;
       list.AddStrings(getLinuxVersionMap);
      end;
    end
    {$ENDIF LINUX}


   {$IFDEF WINDOWS}
   else if (LowerCase(s) = LowerCase ('getFileInfoMap'))
        or (LowerCase(s) = LowerCase ('getFileInfoMap32'))
   {$ELSE WINDOWS}
    //Linux
   else if LowerCase(s) = LowerCase ('getFileInfoMap')
   {$ENDIF WINDOWS}
   then
   Begin
    if Skip ('(', r, r, InfoSyntaxError)
    then
      if EvaluateString (r, r, s1, InfoSyntaxError)
      then
      Begin
        if not fileExists (s1) then
        begin
          LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
          LogDatei.log('File "' + s1 + '" does not exist', LLWarning);
          FNumberOfWarnings := FNumberOfWarnings + 1;
          LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
        end
        else
        Begin
         {$IFDEF WINDOWS}
          versionInfo := TVersionInfo.create (s1);

          for i := 0 to versionInfo.TranslationCount-1 do
          Begin
            list.add('Language name ' + IntToStr(i) + '=' + versionInfo.LanguageNames[i]);
            list.add('Language ID ' + IntToStr(i) + '=' + IntToStr(versionInfo.LanguageID[i]));
          End;

          list.add('file version=' + IntToStr(versionInfo.FileVersion));
          list.add('file version with dots=' + versionInfo.GetFileVersionWithDots);
          list.add('product version=' + IntToStr(versionInfo.ProductVersion));


          for i := Low(versionInfoX.PredefinedStrings) to High(versionInfoX.PredefinedStrings) do
          list.add (versionInfoX.PredefinedStrings[i] + '=' + versionInfo.getString(PredefinedStrings[i]) );


          versionInfo.free;
         {$ENDIF WINDOWS}
        end;
        if Skip (')', r,r, InfoSyntaxError)
        then
        Begin
          syntaxCheck := true;
        End
      End;
   end

   {$IFDEF WINDOWS}
   else if LowerCase(s) = LowerCase ('getFileInfoMap64')
   then
   Begin
    if Skip ('(', r, r, InfoSyntaxError)
    then
      if EvaluateString (r, r, s1, InfoSyntaxError)
      then
      Begin
        {$IFDEF WIN32}
        Wow64FsRedirectionDisabled := false;
        if DSiDisableWow64FsRedirection(oldDisableWow64FsRedirectionStatus) then
        begin
          LogDatei.log('DisableWow64FsRedirection succeeded', LLinfo);
          Wow64FsRedirectionDisabled := true;
        End
        else
        begin
          Wow64FsRedirectionDisabled := false;
          LogDatei.log('Error: DisableWow64FsRedirection failed', LLError);
        end;
       {$ENDIF WIN32}
    (*
        if DSiDisableWow64FsRedirection(oldDisableWow64FsRedirectionStatus) then
        begin
          LogDatei.log('DisableWow64FsRedirection succeeded', LLinfo);
    *)
          if not fileExists (s1) then
          begin
            LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
            LogDatei.log('File "' + s1 + '" does not exist', LLWarning);
            FNumberOfWarnings := FNumberOfWarnings + 1;
            LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
          end
          else
          Begin
            versionInfo := TVersionInfo.create (s1);

            for i := 0 to versionInfo.TranslationCount-1 do
            Begin
              list.add('Language name ' + IntToStr(i) + '=' + versionInfo.LanguageNames[i]);
              list.add('Language ID ' + IntToStr(i) + '=' + IntToStr(versionInfo.LanguageID[i]));
            End;

            list.add('file version=' + IntToStr(versionInfo.FileVersion));
            list.add('file version with dots=' + versionInfo.GetFileVersionWithDots);
            list.add('product version=' + IntToStr(versionInfo.ProductVersion));


            for i := Low(versionInfoX.PredefinedStrings) to High(versionInfoX.PredefinedStrings) do
            list.add (versionInfoX.PredefinedStrings[i] + '=' + versionInfo.getString(PredefinedStrings[i]) );


            versionInfo.free;
          end;
          {$IFDEF WIN32}
          if Wow64FsRedirectionDisabled then
          Begin
            boolresult := DSiRevertWow64FsRedirection(oldDisableWow64FsRedirectionStatus);
            Wow64FsRedirectionDisabled := false;
            LogDatei.log('RevertWow64FsRedirection succeeded', LLinfo);
          End;
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
        if Skip (')', r,r, InfoSyntaxError)
        then
        Begin
          syntaxCheck := true;
        End
      End;
   end


   else if LowerCase(s) = LowerCase ('getFileInfoMapSysnative')
   then
   Begin
    if Skip ('(', r, r, InfoSyntaxError)
    then
      if EvaluateString (r, r, s1, InfoSyntaxError)
      then
      Begin
        if Is64BitSystem then
        begin
           LogDatei.log ('  Starting getFileInfoMap (SysNative 64 Bit mode)...', LLInfo);
            {$IFDEF WIN32}
            Wow64FsRedirectionDisabled := false;
            if DSiDisableWow64FsRedirection(oldDisableWow64FsRedirectionStatus) then
            begin
              LogDatei.log('DisableWow64FsRedirection succeeded', LLinfo);
              Wow64FsRedirectionDisabled := true;
            End
            else
            begin
              Wow64FsRedirectionDisabled := false;
              LogDatei.log('Error: DisableWow64FsRedirection failed', LLError);
            end;
           {$ENDIF WIN32}
           (*
          if DSiDisableWow64FsRedirection(oldDisableWow64FsRedirectionStatus) then
          begin
            LogDatei.log('DisableWow64FsRedirection succeeded', LLinfo);
           *)
            if not fileExists (s1) then
            begin
              LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
              LogDatei.log('File "' + s1 + '" does not exist', LLWarning);
              FNumberOfWarnings := FNumberOfWarnings + 1;
              LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
            end
            else
            Begin
              versionInfo := TVersionInfo.create (s1);

              for i := 0 to versionInfo.TranslationCount-1 do
              Begin
                list.add('Language name ' + IntToStr(i) + '=' + versionInfo.LanguageNames[i]);
                list.add('Language ID ' + IntToStr(i) + '=' + IntToStr(versionInfo.LanguageID[i]));
              End;

              list.add('file version=' + IntToStr(versionInfo.FileVersion));
              list.add('file version with dots=' + versionInfo.GetFileVersionWithDots);
              list.add('product version=' + IntToStr(versionInfo.ProductVersion));


              for i := Low(versionInfoX.PredefinedStrings) to High(versionInfoX.PredefinedStrings) do
              list.add (versionInfoX.PredefinedStrings[i] + '=' + versionInfo.getString(PredefinedStrings[i]) );


              versionInfo.free;
            end;
            {$IFDEF WIN32}
            if Wow64FsRedirectionDisabled then
            Begin
              boolresult := DSiRevertWow64FsRedirection(oldDisableWow64FsRedirectionStatus);
              Wow64FsRedirectionDisabled := false;
              LogDatei.log('RevertWow64FsRedirection succeeded', LLinfo);
            End;
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
          LogDatei.log ('  Starting getFileInfoMap (SysNative 32 Bit mode)...', LLInfo);
          if not fileExists (s1) then
          begin
            LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
            LogDatei.log('File "' + s1 + '" does not exist', LLWarning);
            FNumberOfWarnings := FNumberOfWarnings + 1;
            LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
          end
          else
          Begin
            versionInfo := TVersionInfo.create (s1);

            for i := 0 to versionInfo.TranslationCount-1 do
            Begin
              list.add('Language name ' + IntToStr(i) + '=' + versionInfo.LanguageNames[i]);
              list.add('Language ID ' + IntToStr(i) + '=' + IntToStr(versionInfo.LanguageID[i]));
            End;

            list.add('file version=' + IntToStr(versionInfo.FileVersion));
            list.add('file version with dots=' + versionInfo.GetFileVersionWithDots);
            list.add('product version=' + IntToStr(versionInfo.ProductVersion));

            for i := Low(versionInfoX.PredefinedStrings) to High(versionInfoX.PredefinedStrings) do
              list.add (versionInfoX.PredefinedStrings[i] + '=' + versionInfo.getString(PredefinedStrings[i]) );
            versionInfo.free;
          end;
        end;
        if Skip (')', r,r, InfoSyntaxError)
        then
        Begin
          syntaxCheck := true;
        End
      End;
   end


   {$ENDIF WINDOWS}

   else if LowerCase(s) = LowerCase ('getRegistryKeyList32')
   then
   Begin
    {$IFDEF WINDOWS}
    if Skip ('(', r, r, InfoSyntaxError) then
      if EvaluateString (r, r, s1, InfoSyntaxError) then
      Begin
        //for i :=
        list.AddStrings(GetRegistryKeyList(s1,false));
        if Skip (')', r,r, InfoSyntaxError)  then
        Begin
          syntaxCheck := true;
        End
      End;
    {$ELSE WINDOWS}
    SyntaxCheck := false;
    InfoSyntaxError := 'Only implemented for Windows';
    LogDatei.log(s +' is only implemented for Windows',LLError);
    {$ENDIF WINDOWS}
   end

   else if (LowerCase(s) = LowerCase ('getRegistryKeyList64'))
        or (LowerCase(s) = LowerCase ('getRegistryKeyListSysnative'))
   then
   Begin
    {$IFDEF WINDOWS}
    if Skip ('(', r, r, InfoSyntaxError) then
      if EvaluateString (r, r, s1, InfoSyntaxError) then
      Begin
        //for i :=
        list.AddStrings(GetRegistryKeyList(s1,true));
        if Skip (')', r,r, InfoSyntaxError)  then
        Begin
          syntaxCheck := true;
        End
      End;
    {$ELSE WINDOWS}
    SyntaxCheck := false;
    InfoSyntaxError := 'Only implemented for Windows';
    LogDatei.log(s +' is only implemented for Windows',LLError);
    {$ENDIF WINDOWS}
   end

   else if LowerCase(s) = LowerCase ('getRegistryVarList32')
   then
   Begin
    {$IFDEF WINDOWS}
    if Skip ('(', r, r, InfoSyntaxError) then
      if EvaluateString (r, r, s1, InfoSyntaxError) then
      Begin
        //for i :=
        list.AddStrings(GetRegistryVarList(s1,false));
        if Skip (')', r,r, InfoSyntaxError)  then
        Begin
          syntaxCheck := true;
        End
      End;
    {$ELSE WINDOWS}
    SyntaxCheck := false;
    InfoSyntaxError := 'Only implemented for Windows';
    LogDatei.log(s +' is only implemented for Windows',LLError);
    {$ENDIF WINDOWS}
   end

   else if (LowerCase(s) = LowerCase ('getRegistryVarList64'))
        or (LowerCase(s) = LowerCase ('getRegistryVarListSysnative'))
   then
   Begin
    {$IFDEF WINDOWS}
    if Skip ('(', r, r, InfoSyntaxError) then
      if EvaluateString (r, r, s1, InfoSyntaxError) then
      Begin
        //for i :=
        list.AddStrings(GetRegistryVarList(s1,true));
        if Skip (')', r,r, InfoSyntaxError)  then
        Begin
          syntaxCheck := true;
        End
      End;
    {$ELSE WINDOWS}
    SyntaxCheck := false;
    InfoSyntaxError := 'Only implemented for Windows';
    LogDatei.log(s +' is only implemented for Windows',LLError);
    {$ENDIF WINDOWS}
   end

   else if LowerCase(s) = LowerCase ('getRegistryVarMap32')
   then
   Begin
    {$IFDEF WINDOWS}
    if Skip ('(', r, r, InfoSyntaxError) then
      if EvaluateString (r, r, s1, InfoSyntaxError) then
      Begin
        //for i :=
        list.AddStrings(GetRegistryVarMap(s1,false));
        if Skip (')', r,r, InfoSyntaxError)  then
        Begin
          syntaxCheck := true;
        End
      End;
    {$ELSE WINDOWS}
    SyntaxCheck := false;
    InfoSyntaxError := 'Only implemented for Windows';
    LogDatei.log(s +' is only implemented for Windows',LLError);
    {$ENDIF WINDOWS}
   end

   else if (LowerCase(s) = LowerCase ('getRegistryVarMap64'))
        or (LowerCase(s) = LowerCase ('getRegistryVarMapSysnative'))
   then
   Begin
    {$IFDEF WINDOWS}
    if Skip ('(', r, r, InfoSyntaxError) then
      if EvaluateString (r, r, s1, InfoSyntaxError) then
      Begin
        //for i :=
        list.AddStrings(GetRegistryVarMap(s1,true));
        if Skip (')', r,r, InfoSyntaxError)  then
        Begin
          syntaxCheck := true;
        End
      End;
    {$ELSE WINDOWS}
    SyntaxCheck := false;
    InfoSyntaxError := 'Only implemented for Windows';
    LogDatei.log(s +' is only implemented for Windows',LLError);
    {$ENDIF WINDOWS}
   end


else if LowerCase (s) = LowerCase ('getSlowInfoMap')
  // reads slowinfocache
  // productkey is the key below ARPCache
  //hash key may be: hasname, installsize, lastused, usagefrequency, binaryname
   then
   begin
    {$IFDEF WINDOWS}
    if Skip ('(', r, r, InfoSyntaxError)
    then
     if EvaluateString (r, r, s1, InfoSyntaxError)
     then
       if Skip (',', r,r, InfoSyntaxError)
       then
        if EvaluateString (r, r, s2, InfoSyntaxError)
        then
            if Skip (')', r,r, InfoSyntaxError)
            then
            Begin
              syntaxCheck := true;
              tmpstr := GetSlowInfoCache(s1, 'hasname', s2);
              if tmpstr <> '' then list.add('hasname=' + tmpstr);
              tmpstr := GetSlowInfoCache(s1, 'installsize', s2);
              if tmpstr <> '' then list.add('installsize=' + tmpstr);
              tmpstr := GetSlowInfoCache(s1, 'lastused', s2);
              if tmpstr <> '' then list.add('lastused=' + tmpstr);
              tmpstr := GetSlowInfoCache(s1, 'usagefrequency', s2);
              if tmpstr <> '' then list.add('usagefrequency=' + tmpstr);
              tmpstr := GetSlowInfoCache(s1, 'binaryname', s2);
              if tmpstr <> '' then list.add('binaryname=' + tmpstr);
              //list.add('installsize=' + GetSlowInfoCache(s1, 'installsize', s2));
              //list.add('lastused=' + GetSlowInfoCache(s1, 'lastused', s2));
              //list.add('usagefrequency=' + GetSlowInfoCache(s1, 'usagefrequency', s2));
              //list.add('binaryname=' + GetSlowInfoCache(s1, 'binaryname', s2));
            End
    {$ELSE WINDOWS}
    SyntaxCheck := false;
    InfoSyntaxError := 'Only implemented for Windows';
    LogDatei.log(s +' is only implemented for Windows',LLError);
    {$ENDIF WINDOWS}
   End

else if LowerCase (s) = LowerCase ('getSwauditInfoList')
  // reads slowinfocache
  // productkey is the key below ARPCache
  //hash key may be: hasname, installsize, lastused, usagefrequency, binaryname
   then
   begin
    {$IFDEF WINDOWS}
    if Skip ('(', r, r, InfoSyntaxError)
    then
     if EvaluateString (r, r, s1, InfoSyntaxError)
     then
       if Skip (',', r,r, InfoSyntaxError)
       then
        if EvaluateString (r, r, s2, InfoSyntaxError)
        then
          if Skip (',', r,r, InfoSyntaxError)
          then
            if EvaluateString (r, r, s3, InfoSyntaxError)
            then
              if Skip (')', r,r, InfoSyntaxError)
              then
              Begin
                syntaxCheck := true;
                list1 := TXStringList.Create;
                getSwauditInfoList(s1,s2,s3,list1);
                list.AddStrings(list1);
                list1.Free;
              End
    {$ELSE WINDOWS}
    SyntaxCheck := false;
    InfoSyntaxError := 'Only implemented for Windows';
    LogDatei.log(s +' is only implemented for Windows',LLError);
    {$ENDIF WINDOWS}
   End


   else if LowerCase(s) = LowerCase ('getProcesslist')
   then
   Begin
   //   if r = '' then
        Begin
        syntaxcheck := true;
        list.AddStrings(getProcessList);
       end;
    end

   else if LowerCase(s) = LowerCase ('getProfilesDirList')
   then
   Begin
   //   if r = '' then
        Begin
        syntaxcheck := true;
        list.AddStrings(getProfilesDirList);
       end;
    end

   else if LowerCase(s) = LowerCase ('getHWBiosInfoMap')
   then
   Begin
   //   if r = '' then
        Begin
        syntaxcheck := true;
        list.AddStrings(getHwBiosShortlist);
       end;
    end

   else
    InfoSyntaxError := s0 + ' no valid Expressionstr for a string list';



   if syntaxcheck
   then
   Begin
     LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
     LogDatei.log ('retrieving strings from ' + logstring , LLDebug2);
     LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
     LogDatei.log_list(list, LLDebug2);
     LogDatei.log('', LLDebug2);
     LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 4;
   end;

 End;

 if syntaxcheck
 then
   Remaining := r
 else
   Remaining := s0;

 result := syntaxCheck;

end;

// string functions
function TuibInstScript.EvaluateString
   (const s0 : String;
   var Remaining: String;
   var StringResult : String;
   var InfoSyntaxError : String ) : Boolean;
//var
// NestLevel : integer;
begin
  // nesting level from TuibInstScript
  result := EvaluateString(s0,Remaining,StringResult,InfoSyntaxError,Ifelseendiflevel,inDefFuncIndex);
end;

function TuibInstScript.EvaluateString
   (const s0 : String;
   var Remaining: String;
   var StringResult : String;
   var InfoSyntaxError : String;
   var NestLevel : integer;
   const inDefFuncIndex : integer) : Boolean;


var
 VarIndex : Integer=0;
 a1 : Integer=0;
 syntaxCheck : boolean;
 s : String='';
 sx : String='';
 r : String='';
 s1 : String='';
 s2 : String='';
 r1 : String='';
 s3 : String='';
 s4 : String='';
 n1 : integer=0;
 n2 : Integer=0;
 s1enc : String='';
 s2enc : String='';
 s3enc : String='';
 s4enc : String='';
 listindex : Integer=0;
 randomInt : Integer=0;
 errorinfo : String='';
 intresult : Integer=0;
 list1 : TXStringlist;
 slist : TStringlist; // if we need a real one
 HostsImage : TuibPatchHostsFile;
 HostsLocation : String='';
 i : Integer=0;
 ///m, n, rest : Integer;
 itemlist : TXStringList;
 {$IFDEF WINDOWS}
   Regist : TuibRegistry;
   RegType : TuibRegDataType;
 {$ENDIF WINDOWS}
   majorver : TuibNTVersion;
   minorver : DWORD=0;
   ///programfilesdir, diffx86 : String;
   key0 : String='';
   key : String='';
   valuename : String='';
   StartIndentLevel : Integer=0;
   errorOccured : Boolean;
   continue : Boolean;
   j : Integer=0;
   parameters : Array of String;
   omc : TOpsiMethodCall;
 IniFile : TIniFile;
 //tempchar : char;
 //tempansistr : ansistring;
 //tempansistrp : Pansichar;
   runAs : TRunAs;
   WaitSecs : Word=0;
   flag_force64 : boolean;
   expr : String='';
   ident : String='';
   WaitConditions : TSetWaitConditions;
   ArbeitsSektion : TWorkSection;
   seconds : String='';
   ActionResult : TSectionResult=0;
   mydouble : Double;
   funcindex : integer = 0;
   funcname : string;
   boolresult : boolean;
   p1,p2,p3,p4 : integer;
   tmpstr, tmpstr1, tmpstr2, tmpstr3: string;
   tmpbool, tmpbool1 : boolean;




begin
 LogDatei.log_prog ('EvaluateBoolean: Parsing: '+s0+' ', LLDebug);
 syntaxCheck := false;
 InfoSyntaxError := '';
 StringResult := '';
 slist := TStringlist.create;
 StartIndentLevel := LogDatei.LogSIndentLevel;

 // defined local function ?
 GetWord (s0, funcname, r, WordDelimiterSet5);
 FuncIndex := definedFunctionNames.IndexOf (LowerCase (funcname));
 // string variable?
 GetWord (s0, s, r, WordDelimiterSet3);
 VarIndex := VarList.IndexOf (LowerCase (s));


 // defined local function ?
 if FuncIndex >= 0 then
 begin
   if not (definedFunctionArray[FuncIndex].datatype = dfpString) then
   begin
     // error
     syntaxCheck := false;
     LogDatei.log('Syntax Error: defined function: '+funcname+' is not from type string.',LLError);
   end
   else
   begin
     if definedFunctionArray[FuncIndex].call(r,r,NestLevel) then
     begin
       StringResult := definedFunctionArray[FuncIndex].Resultstring;
       syntaxCheck := true;
       //logdatei.log('We leave the defined function: inDefFunc3: '+IntToStr(inDefFunc3),LLInfo);
     end
     else
     begin
       // defined function call failed
       LogDatei.log('Call of defined function: '+funcname+' failed',LLError);
       syntaxCheck := false;
     end;
   end;
 end

 // local variable
 else if isVisibleLocalVar(s,FuncIndex)  then
 begin
   if not (definedFunctionArray[FuncIndex].getLocalVarDatatype(s) = dfpString) then
   begin
     // type error
     InfoSyntaxError := 'Syntax Error: Type mismatch: String expected but the visible local variable: '
        +s+' is from type: '+osdfParameterTypesNames[definedFunctionArray[FuncIndex].getLocalVarDatatype(s)];
   end
   else
   begin
     StringResult := definedFunctionArray[FuncIndex].getLocalVarValueString(s);
     syntaxCheck := true;
   end;
 end

 // string variable?
 else if VarIndex >= 0 then
 begin
   if ValuesList.count - 1 < VarIndex
   then
   Begin
    InfoSyntaxError := 'The variable: '+s+' has no string value';
   End
   else
   Begin
     StringResult := ValuesList [VarIndex];
     syntaxCheck := true;
   End
 end

 // string constant?
 else if (length (s0) > 0) and (s0[1] = '"') then
 Begin
   r := copy (s0, 2, length (s0)-1);
   GetWord (r, StringResult, r, ['"']);
   if skip ('"', r, r, InfoSyntaxError)
   then syntaxCheck := true;

 End

 // string constant delimited by "'" ?
 else if (length (s0) > 0) and (s0[1] = '''') then
 Begin
   r := copy (s0, 2, length (s0)-1);
   GetWord (r, StringResult, r, ['''']);
   if skip ('''', r, r, InfoSyntaxError)
   then syntaxCheck := true;
 End

 // checking our pseudo function name for retrieving a string avoiding any escape problems of citations marks
 else if LowerCase (s) = LowerCase ('EscapeString') then
 Begin
   if Skip (':', r, s1, InfoSyntaxError)
   then
   Begin
     StringResult := s1;
     r := '';
     syntaxCheck := true;
   End
 End

 // string functions ?

 else if (LowerCase (s) = LowerCase ('LogLevel')) or ( LowerCase (s) = LowerCase('getLogLevel') ) then
 Begin
    StringResult := IntToStr (Logdatei.LogLevel) ;
    syntaxCheck := true;
 End

 else if (LowerCase (s) = LowerCase('getLastExitCode') ) then
 Begin
    StringResult := IntToStr (FLastExitCodeOfExe) ;
    syntaxCheck := true;
 End

 else if (LowerCase (s) = LowerCase('getDiffTimeSec') ) then
 Begin
    StringResult := IntToStr (SecondsBetween(markedTime,Time)) ;
    syntaxCheck := true;
 End


 else if LowerCase (s) = LowerCase ('EnvVar') then
 Begin
   if Skip ('(', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s1, InfoSyntaxError)
   then if Skip (')', r, r, InfoSyntaxError)
   then
   Begin
     StringResult := ValueOfEnvVar (s1);
     syntaxCheck := true;
   End
 End

 else if LowerCase(s) = LowerCase ('GetOS')
 then
 Begin
    syntaxCheck := true;
    OldNumberOfErrors := LogDatei.NumberOfErrors;
    case GetuibOsType (ErrorInfo) of
     tovNotKnown :
       Begin
         LogDatei.log (ErrorInfo, LLError);
         StringResult := 'OS not identified';
       End;
     tovWin16 : StringResult := 'Windows_16';
     tovWin95 : StringResult := 'Windows_95';
     tovWinNT : StringResult := 'Windows_NT';
     tovLinux : StringResult := 'Linux';
    end;

    DiffNumberOfErrors := LogDatei.NumberOfErrors - OldNumberOfErrors;
    FNumberOfErrors := NumberOfErrors + DiffNumberOfErrors;

 End

 else if LowerCase(s) = LowerCase ('GetNTVersion')
 then
 Begin
   {$IFDEF WINDOWS}
    syntaxCheck := true;


    OldNumberOfErrors := LogDatei.NumberOfErrors;

    if GetUibOsType (errorinfo) <> tovWinNT
    then
      StringResult := 'No OS of type Windows NT'
    else
    Begin
      OldNumberOfErrors := LogDatei.NumberOfErrors;
      case GetuibNTversion (ErrorInfo) of
       tntverNONE:
         Begin
           LogDatei.log (ErrorInfo, LLError);
           StringResult := ErrorInfo;
         End;
       tntverNT3 : StringResult := 'NT3';
       tntverNT4 : StringResult := 'NT4';
       tntverWIN2K :
         Begin
          minorver := GetNTVersionMinor;
          if minorver = 0
          then StringResult := 'Win2k'
          else if minorver = 1
          then StringResult := 'WinXP'
          else StringResult := 'Win NT 5.' + inttostr(minorver);
         End;
       tntverWINVISTA :
          Begin
              StringResult := 'Windows Vista'
          end;
       tntverWINX: StringResult := 'Win NT ' + inttostr(GetNtVersionMajor) + '.' + inttostr(minorver);
      end;
      DiffNumberOfErrors := LogDatei.NumberOfErrors - OldNumberOfErrors;
      FNumberOfErrors := NumberOfErrors + DiffNumberOfErrors;
    End
    {$ELSE}
    StringResult := 'No Windows';
    {$ENDIF WINDOWS}
 End

 else if LowerCase(s) = LowerCase('GetLinuxDistroType')
 then
 Begin
    syntaxcheck := true;
    StringResult := 'no_linux';
    {$IFDEF LINUX}
    StringResult := getLinuxDistroType;
    {$ENDIF LINUX}
 End

 else if LowerCase(s) = LowerCase('GetSystemtype')
 then
 Begin
    syntaxcheck := true;
    if Is64BitSystem
      then
        StringResult := '64 Bit System'
      else
        StringResult := 'x86 System';
 End

 else if LowerCase(s) = LowerCase('GetUsercontext')
 then
 Begin
    syntaxcheck := true;
    StringResult := usercontext;
 End

 else if LowerCase(s) = LowerCase('GetLoggedInUser')
 then
 Begin
    syntaxcheck := true;
    StringResult := getLoggedInUser;
 End

 else if LowerCase(s) = LowerCase('readVersionFromProfile')
 then
 Begin
    syntaxcheck := true;
    StringResult := readVersionFromProfile;
 End


 else if LowerCase(s) = LowerCase('GetScriptMode')
 then
 Begin
    syntaxcheck := true;
    case  scriptMode of
      tsmMachine: StringResult := 'Machine';
      tsmLogin: StringResult := 'Login';
    end;
 End


 else if LowerCase(s) = LowerCase ('GetMSVersionInfo')
 then
 Begin
    syntaxCheck := true;

    OldNumberOfErrors := LogDatei.NumberOfErrors;

    if GetUibOsType (errorinfo) <> tovWinNT
    then
      StringResult := 'Not an OS of type Windows NT'
    else
    Begin
      {$IFDEF WINDOWS}
      OldNumberOfErrors := LogDatei.NumberOfErrors;

      majorVer := GetuibNTversion (ErrorInfo);
      if majorVer = tntverNONE
      then
      Begin
        LogDatei.log (ErrorInfo, LLError);
        StringResult := ErrorInfo;
      End
      else
      Begin
        StringResult := intToStr (GetNTVersionMajor) + '.' + intToStr(GetNTVersionMinor);
      end;

      DiffNumberOfErrors := LogDatei.NumberOfErrors - OldNumberOfErrors;
      FNumberOfErrors := NumberOfErrors + DiffNumberOfErrors;
      {$ENDIF WINDOWS}
    End
 End

 else if LowerCase (s) = LowerCase ('IniVar')  then
 Begin
   if Skip ('(', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s1, InfoSyntaxError)
   then if Skip (')', r, r, InfoSyntaxError)
   then
   Begin
     LogDatei.log ('The Function IniVar is deprecated ! use GetProductProperty', LLWarning);
     StringResult := ProductvarsForPC.Values [s1];
     syntaxCheck := true;
   End
 End

 else if (LowerCase (s) = LowerCase ('GetProductProperty'))
      or (LowerCase (s) = LowerCase ('GetConfidentialProductProperty')) then
 Begin
   try
     if Skip ('(', r, r, InfoSyntaxError)
     then if EvaluateString (r, r, s1, InfoSyntaxError)
     then if Skip(',', r, r, InfoSyntaxError)
     then if EvaluateString (r, r, s2, InfoSyntaxError)
     then if Skip (')', r, r, InfoSyntaxError)
     then
     begin
       syntaxCheck := true;

       // try to get from lookup table
       if ProductvarsForPC.indexOfName(s1) = -1 then
       begin
         LogDatei.log('Property not existing in GetProductProperty - using default',LLWarning);
         StringResult := s2;
       end
       else
       begin
         // get the property value from the looup table
         StringResult := ProductvarsForPC.Values [s1];
       end;
     End;
     if (LowerCase (s) = LowerCase ('GetConfidentialProductProperty')) then
       LogDatei.AddToConfidentials(StringResult);
   except
     on E: Exception do
      begin
       Logdatei.log('Exception in GetProductProperty: '+ E.Message+ ' - using default',LLError);
       StringResult := s2;
      end;
   end;
 End


 else if LowerCase (s) = LowerCase ('GetHostsName') then
 Begin
   if Skip ('(', r, r, InfoSyntaxError)
   then
   if Skip ('"', r, r, InfoSyntaxError)
   then
   Begin
     GetWord (r, s1, r, ['"']);

     if Skip ('"', r, r, InfoSyntaxError) then
     if Skip (')', r, r, InfoSyntaxError) then
     Begin
       syntaxCheck := true;
       //StringResult := GetHostByName(s1);

       HostsImage := TuibPatchHostsFile.Create;
       Logdatei.LogLevel := LogLevel;
       Logdatei.LogSIndentLevel := 0;

       OldNumberOfErrors := LogDatei.NumberOfErrors;
       OldNumberOfWarnings := LogDatei.NumberOfWarnings;

       HostsLocation := DefaultHosts;

       if not FileExists (HostsLocation)
       then
       begin
         LogDatei.log ('Error: Hosts ' + HostsLocation + ' not found', LLError);
       end
       else
       begin
         HostsImage.LoadFromFile (HostsLocation);
         HostsImage.GetHostname (s1, StringResult)
       end;
       HostsImage.free; HostsImage := nil;

       DiffNumberOfErrors := LogDatei.NumberOfErrors - OldNumberOfErrors;
       DiffNumberOfWarnings := LogDatei.NumberOfWarnings - OldNumberOfWarnings;
       FNumberOfErrors := NumberOfErrors + DiffNumberOfErrors;
       FNumberOfWarnings := NumberOfWarnings + DiffNumberOfWarnings;
     end;
   End;
 End

 else if LowerCase (s) =  LowerCase ('GetHostsAddr') then
 Begin
   if Skip ('(', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s1, InfoSyntaxError)
   then if Skip (')', r, r, InfoSyntaxError)
   then
   Begin
     syntaxCheck := true;

     HostsImage := TuibPatchHostsFile.Create;
     Logdatei.LogLevel := LogLevel;
     Logdatei.LogSIndentLevel := 0;

     OldNumberOfErrors := LogDatei.NumberOfErrors;
     OldNumberOfWarnings := LogDatei.NumberOfWarnings;

     HostsLocation := DefaultHosts;

     if not FileExists (HostsLocation)
     then
     begin
       Logdatei.log('Hosts ' + HostsLocation + ' not found', LLerror);
     end
     else
     begin
       HostsImage.LoadFromFile (HostsLocation);
       HostsImage.GetAddress (s1, StringResult)
     end;
     HostsImage.free; HostsImage := nil;

     DiffNumberOfErrors := LogDatei.NumberOfErrors - OldNumberOfErrors;
     DiffNumberOfWarnings := LogDatei.NumberOfWarnings - OldNumberOfWarnings;
     FNumberOfErrors := NumberOfErrors + DiffNumberOfErrors;
     FNumberOfWarnings := NumberOfWarnings + DiffNumberOfWarnings;
   End;
 End


 else if LowerCase (s) = LowerCase ('GetMyIpByTarget') then
 Begin
   if Skip ('(', r, r, InfoSyntaxError)
    then if EvaluateString (r, r, s1, InfoSyntaxError)
   then if Skip (')', r, r, InfoSyntaxError)
   then
   Begin
     syntaxCheck := true;
     {$IFDEF WIN64}
     StringResult :=  '';
     Logdatei.log('Error: Not implemented for winst64', LLerror);
     {$ELSE}
     if isip(s1) then
       StringResult := getMyIpByTarget(s1)
     else
     begin
       StringResult :=  '';
       Logdatei.log('Error: '+s1+' is not a valid IPv4 Address', LLerror);
     end;
     {$ENDIF}
   End;
 End



 else if LowerCase (s) = LowerCase ('GetIpByName') then
 Begin
   if Skip ('(', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s1, InfoSyntaxError)
   then if Skip (')', r, r, InfoSyntaxError)
   then
   Begin
     syntaxCheck := true;
     if GetIPFromHost(s1,s2,s3) then StringResult := s2
     else
     begin
       {$IFDEF LINUX}
       StringResult :=  getCommandResult('resolveip -s '+s1);
       {$ENDIF LINUX}
       {$IFDEF WINDOWS}
       StringResult :=  '';
       Logdatei.log('Error: '+s3, LLerror);
       {$ENDIF WINDOWS}
     end;
   End;
 End

 else if LowerCase (s) = LowerCase ('getDefaultNetmaskByIP4adr') then
 Begin
   if Skip ('(', r, r, InfoSyntaxError)
    then if EvaluateString (r, r, s1, InfoSyntaxError)
   then if Skip (')', r, r, InfoSyntaxError)
   then
   Begin
     syntaxCheck := true;
     if isip(s1) then
       StringResult := getDefaultNetmaskByIP4adr(s1)
     else
     begin
       StringResult :=  '';
       Logdatei.log('Error: '+s1+' is not a valid IPv4 Address', LLerror);
     end;
   End;
 End

 else if LowerCase (s) =  LowerCase ('GetIni') then
 Begin
   if Skip ('(', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s1, InfoSyntaxError)
   then if Skip ('[', r, r, InfoSyntaxError)
   then
   Begin
     s1 := ExpandFileName(s1);
     Inifile := TInifile.Create (s1);
     GetWord (r, s1, r, WordDelimiterSet0);
     if Skip (']', r, r, InfoSyntaxError)
     then
     Begin
       GetWord (r, sx, r, WordDelimiterSet1);
       if Skip (')', r, r, InfoSyntaxError)
       then
       Begin
         StringResult := Inifile.ReadString (s1, sx, '' (* 'ERROR' *));
         syntaxCheck := true;
       end;
     end;
     Inifile.free; Inifile := nil;
   End;
 End

 else if LowerCase (s) =  LowerCase ('GetValueFromInifile') then
 Begin
   if Skip ('(', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s1, InfoSyntaxError)
   then if Skip (',', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s2, InfoSyntaxError)
   then if Skip (',', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s3, InfoSyntaxError)
   then if Skip (',', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s4, InfoSyntaxError)
   then if Skip (')', r, r, InfoSyntaxError)
   then
   Begin
     syntaxCheck := true;
     try
       s1 := ExpandFileName(s1);
       Inifile := TInifile.Create (s1);

       LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
       LogDatei.log
       ('    reading the value to the key "' +  s3 + '" in section "' +  s2 + '"  from inifile  "' + s1 + '", default value  "' + s4 +'"',
       LevelComplete);
       s2enc := UTF8ToWinCP(s2);
       s3enc := UTF8ToWinCP(s3);
       s4enc := UTF8ToWinCP(s4);
       StringResult := Inifile.ReadString( s2enc, s3enc, s4enc);
       StringResult := WinCPToUTF8(StringResult);
       LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;

       Inifile.free; Inifile := nil;
     except
       on e: Exception
          do
          Begin
              LogDatei.log ('Error in creating inifile "' + s1 + '", message: "' + e.Message + '"', LevelWarnings);
              StringResult := s4;
          End;
     end;

   End;
 End


 else if LowerCase (s) = LowerCase ('Lower') then
 begin
  if Skip ('(', r, r, InfoSyntaxError)
  then
   if EvaluateString (r, r, s1, InfoSyntaxError)
   then
     if Skip (')', r,r, InfoSyntaxError)
     then
     Begin
         syntaxCheck := true;
         StringResult := lowercase(s1);
     End;
 end

 else if LowerCase (s) = LowerCase ('Upper') then
 begin
  if Skip ('(', r, r, InfoSyntaxError)
  then
   if EvaluateString (r, r, s1, InfoSyntaxError)
   then
     if Skip (')', r,r, InfoSyntaxError)
     then
     Begin
         syntaxCheck := true;
         StringResult := uppercase(s1);
     End;
 end

 else if LowerCase (s) = LowerCase ('Trim') then
 begin
  if Skip ('(', r, r, InfoSyntaxError)
  then
   if EvaluateString (r, r, s1, InfoSyntaxError)
   then
     if Skip (')', r,r, InfoSyntaxError)
     then
     Begin
         syntaxCheck := true;
         StringResult := trim(s1);
     End;
 end

 else if LowerCase (s) = LowerCase ('asConfidential') then
 begin
  // backup and set loglevel to warning
  // get the input
  // make it confidential
  // give it to output
  // restore loglevel
  if Skip ('(', r, r, InfoSyntaxError)
  then
  begin
   p1 := logdatei.LogLevel;
   try
     logdatei.LogLevel:= LLWarning;
     if EvaluateString (r, r, s1, InfoSyntaxError)
     then
     begin
       if Skip (')', r,r, InfoSyntaxError)
       then
       Begin
           syntaxCheck := true;
           logdatei.AddToConfidentials(s1);
           StringResult := s1;
       End;
     end;
   finally
     logdatei.LogLevel:= p1;
   end;
  end;
 end


 else if LowerCase (s) = LowerCase ('calculate') then
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
         if not opsicalc(s1,StringResult) then
         begin
           LogDatei.log ('Error in calculate : could not calculate : ' + s1 + ' ; '+StringResult, LLError);
           StringResult := '';
         end;
     End;
  if  not syntaxCheck then
  begin
   LogDatei.log ('Error in calculate : could not calculate : ' + s1 + ' ; '+InfoSyntaxError, LLError);
  end;
 end


 else if LowerCase (s) = LowerCase ('strLength') then
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
         StringResult := IntToStr(length(s1));
     End;
  if  not syntaxCheck then
  begin
   LogDatei.log ('Error in strLength : could not get length of : ' + s1 + ' ; '+InfoSyntaxError, LLError);
  end;
 end

 else if LowerCase (s) = LowerCase ('strLoadTextFile')
 then
 begin
  if Skip ('(', r, r, InfoSyntaxError)
  then
   if EvaluateString (r, r, s1, InfoSyntaxError)
   then
   Begin
     try
       list1 := TXStringList.create;
       s1 := ExpandFileName(s1);
       list1.loadfromfile (s1);
       if list1.Count > 0 then
         StringResult:= reencode(list1.Strings[0], 'system')
       else StringResult:= '';
       list1.Free;
     except
       on e: exception do
       begin
         LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
         LogDatei.log('Error on loading file: ' + e.message, LLError);
         FNumberOfErrors := FNumberOfErrors + 1;
         LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
       end
     end;
     if Skip (')', r,r, InfoSyntaxError)
     then
     Begin
       syntaxCheck := true;
     End
   End
 end

 else if LowerCase (s) = LowerCase ('strLoadTextFileWithEncoding')
 then
 begin
  if Skip ('(', r, r, InfoSyntaxError)
  then
   if EvaluateString (r, r, s1, InfoSyntaxError)
   then
     if Skip (',', r,r, InfoSyntaxError)
     then
      if EvaluateString (r, r, s2, InfoSyntaxError)
      then
           if Skip (')', r,r, InfoSyntaxError)
           then
           Begin
             syntaxCheck := true;
             try
               list1 := TXStringList.create;
               s1 := ExpandFileName(s1);
               list1.loadfromfile (s1);
               if list1.Count > 0 then
                 StringResult:= reencode(list1.Strings[0], s2)
               else StringResult:= '';
               list1.Free;
             except
               on e: exception do
               begin
                 LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
                 LogDatei.log('Error on loading file: ' + e.message, LLError);
                 FNumberOfErrors := FNumberOfErrors + 1;
                 LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
               end
             end;
           End
 end


 else if LowerCase (s) = LowerCase ('strPos') then
 begin
  syntaxCheck := false;
  if Skip ('(', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s1, InfoSyntaxError)
     and Skip(',', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s2, InfoSyntaxError)
     and Skip (')', r, r, InfoSyntaxError)
  then
    Begin
      syntaxCheck := true;
      StringResult := IntToStr(pos(s2,s1));
    End;
  if  not syntaxCheck then
  begin
   LogDatei.log ('Error in strPos : could not get pos of : ' + s2 + 'in : '+s1+' ; '+InfoSyntaxError, LLError);
  end;
 end

 else if LowerCase (s) = LowerCase ('strPart') then
 begin
  syntaxCheck := false;
  if Skip ('(', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s1, InfoSyntaxError)
     and Skip(',', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s2, InfoSyntaxError)
     and Skip(',', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s3, InfoSyntaxError)
     and Skip (')', r, r, InfoSyntaxError)
  then
    Begin
      syntaxCheck := true;
      try
        n1 := StrToInt(LowerCase(trim(s2)));
        try
          n2 := StrToInt(LowerCase(trim(s3)));
          StringResult := copy(s1,n1,n2);
          LogDatei.log('strPart from: ' + s1 + ' start: '+inttostr(n1)+' number: '+inttostr(n2)+' gives: >'+stringresult+'<', LLDebug2);
        except
          LogDatei.log('Error: ' + s2 + ' has no Integer format', LLerror)
        end;
      except
        LogDatei.log('Error: ' + s3 + ' has no Integer format', LLerror)
      end;
    End;
   if  not syntaxCheck then
  begin
   LogDatei.log ('Error in strPart : could not get part of : ' + s1 + ' ; '+InfoSyntaxError, LLError);
  end;
 end

 else if LowerCase (s) = LowerCase ('randomIntStr') then
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
         //LogDatei.log ('incStr for : ' + s1 , LLDebug2);
         try
           intresult := StrToInt(s1);
           //LogDatei.log ('incStr for : ' + IntToStr(intresult) , LLDebug2);
           //Randomize;
           StringResult := IntToStr(random(intresult));
           //LogDatei.log ('incStr for : ' + IntToStr(intresult) , LLDebug2);
           //StringResult := IntToStr(intresult);
         except
           on E:Exception do
           LogDatei.log ('Error in randomIntStr : perhaps could not convert to int : ' + s1 + ' Error : '+E.Message, LLError);
         end;
     End;
  if  not syntaxCheck then
  begin
   LogDatei.log ('Error in randomIntStr : could not make random for : ' + s1 + ' ; '+InfoSyntaxError, LLError);
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

 else if LowerCase (s) = LowerCase ('LangCodeByHex') then
 begin
  if Skip ('(', r, r, InfoSyntaxError)
  then
   if EvaluateString (r, r, s1, InfoSyntaxError)
   then
     if Skip (')', r,r, InfoSyntaxError)
     then
     Begin
      syntaxCheck := true;
      s1 := LowerCase(trim(s1));
      if pos('$',s1) = 1 then
        s1 := copy(s1,2,length(s1));
      if pos('0x',s1) = 1 then
        s1 := copy(s1,3,length(s1));
      try
        StringResult := getlangcodeByHexvalueStr('0x'+s1);
      except
        LogDatei.log('Error: ' + s1 + ' has no Integer format', LLError)
      end;
     End;
 end

 else if LowerCase (s) = LowerCase ('md5sumFromFile') then
 begin
  if Skip ('(', r, r, InfoSyntaxError)
  then
   if EvaluateString (r, r, s1, InfoSyntaxError)
   then
     if Skip (')', r,r, InfoSyntaxError)
     then
     Begin
      syntaxCheck := true;
      s1 := LowerCase(trim(s1));
      StringResult := '';
      if not FileExistsUTF8(ExpandFileName(s1)) then
      begin
       LogDatei.log('Error: md5sumFromFile: ' + ExpandFileName(s1) + ' is no valid file', LLError)
      end
      else
      try
        StringResult := Copy(lowerCase(md5fromFile(ExpandFileName(s1))),0,32);
      except
        LogDatei.log('Error: Exception at md5sumFromFile: ' + ExpandFileName(s1), LLError)
      end;
     End;
 end

 else if LowerCase (s) = LowerCase ('shellcall')
 then
 begin
  if Skip ('(', r, r, InfoSyntaxError)
  then
   if EvaluateString (r, r, s1, InfoSyntaxError)
   then
     if Skip (')', r,r, InfoSyntaxError)
     then
     Begin
       syntaxCheck := true;
       StringResult := '';
       try
         //LogDatei.log ('Executing0 ' + s1, LLInfo);
         execShellCall(s1, 'sysnative',0, true);
         StringResult := IntToStr (FLastExitCodeOfExe) ;
       except
         on e: exception do
         begin
           LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
           LogDatei.log('Error executing :'+s1+' : '+ e.message,
             LLError);
           FNumberOfErrors := FNumberOfErrors + 1;
           LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
         end
       end;
     End
 end

 else if LowerCase (s) = LowerCase ('powershellcall')
 then
 begin
  {$IFDEF Linux}
  LogDatei.log('Error powershellcall not implemented on Linux ', LLError);
  {$ENDIF Linux}
  {$IFDEF WINDOWS}
   s2 := '';
   s3 := '';
   tmpstr2 := '';
   tmpbool := true; // sysnative
   tmpbool1 := true; // handle execution policy
   syntaxCheck := false;
   StringResult := '';
  if Skip ('(', r, r, InfoSyntaxError)
  then if EvaluateString (r, tmpstr, s1, InfoSyntaxError)
  // next after ',' or ')'
  then if Skip (',', tmpstr, tmpstr1, tmpstr3) then
   if EvaluateString (tmpstr1, tmpstr2, s2, tmpstr3) then;
  if s2 = '' then
  begin
   // only one parameter
   if Skip (')', tmpstr, r, InfoSyntaxError) then
   Begin
     syntaxCheck := true;
     s2 := 'sysnative';
   end;
  end
  else
  begin
   // got second parameter
    tmpbool := true;
   if lowercase(s2) = '32bit' then tmpbool := false
   else if lowercase(s2) = '64bit' then tmpbool := true
   else if lowercase(s2) = 'sysnative' then tmpbool := true
   else
   begin
     InfoSyntaxError := 'Error: unknown parameter: '+s2+' expected one of 32bit,64bit,sysnative - fall back to sysnative';
     syntaxCheck := false;
   end;
   // three parameter ?
   if Skip (',', tmpstr2, tmpstr1, tmpstr3) then
   begin
     if EvaluateString (tmpstr1, tmpstr2, s3, tmpstr3) then
     begin
      // got third parameter
       if Skip (')', tmpstr2, r, InfoSyntaxError) then
       Begin
         if TryStrToBool(s3,tmpbool1) then
           syntaxCheck := true
         else
         begin
           syntaxCheck := false;
           InfoSyntaxError := 'Error: boolean string (true/false) expected but got: '+s3;
         end;
       end;
     end;
   end
   else
   if Skip (')', tmpstr2, r, InfoSyntaxError) then
   Begin
    // two parameter
     syntaxCheck := true;
   end;
  end;
  if syntaxCheck then
   begin
   try
         execPowershellCall(s1, s2,0, true,false,tmpbool1);
         StringResult := IntToStr (FLastExitCodeOfExe) ;
       except
         on e: exception do
         begin
           LogDatei.log('Error executing :'+s1+' : with powershell: '+ e.message,
             LLError);
         end
       end;
   end;
   {$ENDIF WINDOWS}
 end



 else if LowerCase (s) = LowerCase ('processCall')
 then
 begin
  if Skip ('(', r, r, InfoSyntaxError)
  then
   if EvaluateString (r, r, s1, InfoSyntaxError)
   then
     if Skip (')', r,r, InfoSyntaxError)
     then
     Begin
       syntaxCheck := true;
       StringResult := '';
       ArbeitsSektion := TWorkSection.create(0,Nil);
       ArbeitsSektion.Text:= s1;
       tmpstr := r;
//////////////////
begin
              //{$IFDEF WINDOWS}
               runAs := traInvoker;
               {$IFDEF WIN32}
               opsiSetupAdmin_runElevated := false;
               {$ENDIF WIN32}
               WaitSecs := 0;
               flag_force64 := false;
               GetWord (r, expr, r, WordDelimiterSet0);
               SyntaxCheck := true;

               ident := '';
               WaitConditions := [ttpWaitOnTerminate];

               while SyntaxCheck and (length (expr) > 0)
               do
               Begin
                 {$IFDEF WIN32}
                 if LowerCase (expr) = LowerCase (ParameterRunAsAdmin)
                 then
                 Begin
                     runAs := traAdmin;
                 End

                 else if LowerCase (expr) = LowerCase (ParameterRunAsAdmin1)
                 then
                 Begin
                     runAs := traAdmin;
                 End

                 else if LowerCase (expr) = LowerCase (ParameterRunAsAdmin2)
                 then
                 Begin
                     runAs := traAdminProfile;
                 End

                 else if LowerCase (expr) = LowerCase (ParameterRunAsAdmin3)
                 then
                 Begin
                     runAs := traAdminProfileImpersonate;
                 End

                 else if LowerCase (expr) = LowerCase (ParameterRunAsAdmin4)
                 then
                 Begin
                     runAs := traAdminProfileImpersonateExplorer;
                 End

                 else if LowerCase (expr) = LowerCase (ParameterRunAsInvoker)
                 then
                 Begin
                     runAs := traInvoker;
                 End

                 else if LowerCase (expr) = LowerCase (ParameterRunElevated)
                 then
                 Begin
                     opsiSetupAdmin_runElevated := true;
                 End

                 else if LowerCase (expr) = LowerCase (ParameterRunAsLoggedOnUser)
                 then
                 Begin
                   if runLoginScripts then
                     runAs := traLoggedOnUser
                   else
                     LogDatei.log('Warning: Not in UserLoginScript mode: /RunAsLoggedinUser ignored', LLWarning);
                 End

                 else if (LowerCase (expr) = LowerCase (Parameter_64Bit)) and Is64BitSystem
                 then
                 Begin
                     flag_force64 := true;
                 End

                 else if LowerCase (expr) = LowerCase (Parameter_32Bit)
                 then
                 Begin
                     flag_force64 := false;
                 End

                 else if (LowerCase (expr) = LowerCase (Parameter_SysNative)) and Is64BitSystem
                 then
                 Begin
                     flag_force64 := true;
                 End

                 else if (LowerCase (expr) = LowerCase (Parameter_SysNative)) and (not Is64BitSystem)
                 then
                 Begin
                     flag_force64 := false;
                 End

                 else {$ENDIF WIN32}  if LowerCase (expr) = LowerCase (ParameterWaitSecs)
                 then
                 Begin
                   WaitConditions := WaitConditions + [ttpWaitTime];
                   // WaitConditions := WaitConditions - [ttpWaitOnTerminate];

                   GetWord (r, expr, r, WordDelimiterSet0);
                   try
                     WaitSecs := StrToInt64 (expr);
                   except
                     on EConvertError do
                     Begin
                        InfoSyntaxError := 'Integer number expected';
                        SyntaxCheck := false;
                     End
                   end;
                 End

                 {$IFDEF WIN32}
                 else if LowerCase (expr) = LowerCase (ParameterCloseOnWindow)
                 then
                 Begin
                   runAs := traInvoker;
                   WaitConditions := WaitConditions + [ttpWaitForWindowAppearing];

                   if EvaluateString (r, r, ident, InfoSyntaxError)
                   then
                   Begin
                     if r <> ''
                     then
                     Begin
                       SyntaxCheck := false;
                       InfoSyntaxError := 'not expected chars after "';
                     End;
                   End
                   else
                      SyntaxCheck := false;
                 End


                 else if LowerCase (expr) = LowerCase (ParameterCloseBehindWindow)
                 then
                 Begin
                   runAs := traInvoker;
                   WaitConditions := WaitConditions + [ttpWaitForWindowVanished];

                   if EvaluateString (r, r, ident, InfoSyntaxError)
                   then
                   Begin
                     if r <> ''
                     then
                     Begin
                       SyntaxCheck := false;
                       InfoSyntaxError := 'unexpected characters after "';
                     End;
                   End
                   else
                     SyntaxCheck := false;
                 End

                 {$ENDIF WIN32}

                 else if LowerCase (expr) = LowerCase (ParameterWaitProcessTimeoutSecs)
                 then
                 Begin
                   WaitConditions := WaitConditions - [ttpWaitTime];
                   WaitConditions := WaitConditions + [ttpWaitTimeout];

                   GetWord (r, expr, r, WordDelimiterSet0);
                   try
                     WaitSecs := StrToInt64 (expr);
                   except
                     on EConvertError do
                     Begin
                       try
                         EvaluateString (expr, expr, seconds, InfoSyntaxError);
                         WaitSecs := StrToInt64 (seconds);
                       except
                         on EConvertError do
                         Begin
                           InfoSyntaxError := 'Integer number expected '+InfoSyntaxError;
                          SyntaxCheck := false;
                        end;
                       end
                     End
                   end
                 End

                 else if LowerCase (expr) = LowerCase (ParameterWaitForProcessEnding)
                 then
                 Begin
                   WaitConditions := WaitConditions + [ttpWaitForProcessEnding];
(*
                   GetWord (Remaining, ident, Remaining, WordDelimiterSet0);
                   if ident = '' then
                   begin
                     SyntaxCheck := false;
                     InfoSyntaxError := 'process name expected';
                   end;
*)
                   if not EvaluateString (r, r, ident, InfoSyntaxError)  then
                      SyntaxCheck := false;
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
                 End

                 else if UpperCase (expr) = UpperCase (ParameterDontWait)
                 then
                 Begin
                     WaitConditions := WaitConditions - [ttpWaitOnTerminate];
                     WaitConditions := WaitConditions - [ttpWaitTimeout];
                 End

                 else if UpperCase (expr) = UpperCase (ParameterWaitOnTerminate)
                 then
                 Begin
                     WaitConditions := WaitConditions + [ttpWaitOnTerminate];
                 End



                 else
                 Begin
                       SyntaxCheck := false;
                       InfoSyntaxError := expr + ' not legal WinBatch parameter';
                 End;

                 GetWord (r, expr, r, WordDelimiterSet0);
               end;

               if SyntaxCheck
               then
               begin
                 LogDatei.log ('Executing: ' + s +'('+ s1+') '+tmpstr, LLNotice);
                 ActionResult := execWinBatch (ArbeitsSektion, r, WaitConditions, Ident, WaitSecs, runAs,flag_force64);
                 StringResult := IntToStr (FLastExitCodeOfExe) ;
               end
               else
                 ActionResult := reportError (ArbeitsSektion, i, 'Expressionstr', InfoSyntaxError);
               (*
               {$ELSE WINDOWS}
                logdatei.log('Winbatch sections are not implemented for Linux right now', LLWarning);
                {$ENDIF WINDOWS}
                *)
             end;
             ArbeitsSektion.Free;
///////////////////
     End
 end

 else if LowerCase (s) = LowerCase ('boolToString') then
 begin
  if Skip ('(', r, r, InfoSyntaxError)
  then
  begin
   StringResult := '';
   // backup given expr
   r1 := r;
   if EvaluateString (r1, r, s1, InfoSyntaxError) then
   begin
    try
     boolresult := strtobool(s1);
     if Skip (')', r,r, InfoSyntaxError)
     then
     Begin
      syntaxCheck := true;
      try
        StringResult := BoolToStr(boolresult,true);
      except
        LogDatei.log('Error: boolToString: string expression' + s1 + ' has no boolean value', LLError);
        StringResult := '';
      end;
     End;
     except
       LogDatei.log('Error: boolToString: string expression' + s1 + ' has no boolean value', LLDebug2);
       StringResult := '';
     end;
   end;
   if StringResult = '' then
   begin
     // r1 is not a standard boolean string representation
     // let us try boolean string expression
     intresult := 0;
     if EvaluateBoolean (r1, r, boolresult, intresult, InfoSyntaxError)
     then
     begin
       if Skip (')', r,r, InfoSyntaxError)
       then
       Begin
        syntaxCheck := true;
        try
          StringResult := BoolToStr(boolresult,true);
        except
          LogDatei.log('Error: boolToString: string expression' + r + ' has no boolean value', LLError);
          StringResult := '';
        end;
       End;
     end
     else
     begin
       // EvaluateBoolean = false
       LogDatei.log('Error: boolToString: string expression' + r + ' has no boolean value', LLError);
       StringResult := '';
     end;
   end;
  end;
 end


 else if LowerCase (s) = LowerCase ('HexStrToDecStr') then
 begin
  if Skip ('(', r, r, InfoSyntaxError)
  then
   if EvaluateString (r, r, s1, InfoSyntaxError)
   then
     if Skip (')', r,r, InfoSyntaxError)
     then
     Begin
      syntaxCheck := true;
      s1 := LowerCase(trim(s1));
      if pos('$',s1) = 1 then
        s1 := copy(s1,2,length(s1));
      if pos('0x',s1) = 1 then
        s1 := copy(s1,3,length(s1));
      try
        StringResult := IntToStr(StrToInt('$'+s1));
      except
        LogDatei.log('Error: ' + s1 + ' has no Integer format', LLError)
      end;
     End;
 end

 else if LowerCase (s) = LowerCase ('DecStrToHexStr') then
 begin
  if Skip ('(', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s1, InfoSyntaxError)
     and Skip(',', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s2, InfoSyntaxError)
     and Skip (')', r, r, InfoSyntaxError)
  then
    Begin
      syntaxCheck := true;
      s1 := LowerCase(trim(s1));
      try
        n2 := StrToInt(LowerCase(trim(s2)));
        try
          n1 := StrToInt(s1);
          StringResult := IntToHex(n1,n2);
        except
          LogDatei.log('Error: ' + s1 + ' has no Integer format', LLerror)
        end;
      except
        LogDatei.log('Error: ' + s2 + ' has no Integer format', LLerror)
      end;
    End;
 end

 else if LowerCase (s) = LowerCase ('encryptStringBlow') then
 begin
  if Skip ('(', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s1, InfoSyntaxError)
     and Skip(',', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s2, InfoSyntaxError)
     and Skip (')', r, r, InfoSyntaxError)
  then
    Begin
      syntaxCheck := true;
      try
        StringResult := encryptStringBlow(s1,s2);
      except
         LogDatei.log('Error: Exception in encrypt_hex_blow: "' + s1 + '","'+s2+'"', LLerror)
      end;
    End;
 end

 else if LowerCase (s) = LowerCase ('decryptStringBlow') then
 begin
  if Skip ('(', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s1, InfoSyntaxError)
     and Skip(',', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s2, InfoSyntaxError)
     and Skip (')', r, r, InfoSyntaxError)
  then
    Begin
      syntaxCheck := true;
      try
        StringResult := decryptStringBlow(s1,s2);
      except
         LogDatei.log('Error: Exception in decrypt_hex_blow: "' + s1 + '","'+s2+'"', LLerror)
      end;
    End;
 end


 else if LowerCase (s) = LowerCase ('base64EncodeStr') then
 begin
  if Skip ('(', r, r, InfoSyntaxError)
  then
   if EvaluateString (r, r, s1, InfoSyntaxError)
   then
     if Skip (')', r,r, InfoSyntaxError)
     then
     Begin
      syntaxCheck := true;
      StringResult := B64Encode(s1);
    End;
 end

 else if LowerCase (s) = LowerCase ('base64DecodeStr') then
 begin
  if Skip ('(', r, r, InfoSyntaxError)
  then
   if EvaluateString (r, r, s1, InfoSyntaxError)
   then
     if Skip (')', r,r, InfoSyntaxError)
     then
     Begin
      syntaxCheck := true;
      StringResult := B64Decode(s1);
    End;
 end

 else if LowerCase (s) = LowerCase ('GetShortWinPathName') then
 begin
  {$IFDEF WINDOWS}
  if Skip ('(', r, r, InfoSyntaxError)
  then
   if EvaluateString (r, r, s1, InfoSyntaxError)
   then
     if Skip (')', r,r, InfoSyntaxError)
     then
     Begin
       syntaxCheck := true;
       StringResult := GetShortWinPathName(s1);
     End;
  {$ELSE WINDOWS}
  StringResult := '';
  LogDatei.log('Error: GetShortWinPathName only impemented for Windows.', LLerror);
  {$ENDIF WINDOWS}
 end

 else if LowerCase (s) = LowerCase ('Convert2JsonStr') then
 begin
  if Skip ('(', r, r, InfoSyntaxError)
  then
   if EvaluateString (r, r, s1, InfoSyntaxError)
   then
     if Skip (')', r,r, InfoSyntaxError)
     then
     Begin
      syntaxCheck := true;
      str2jsonstr(s1, s2);
      if s2 = 'ok' then
        StringResult := s1
      else
      begin
        StringResult := '';
        LogDatei.log('Error: ' + s2, LLerror);
      end
    End;
 end

 else if LowerCase (s) = LowerCase ('jsonAsArrayCountElements') then
 begin
  if Skip ('(', r, r, InfoSyntaxError)
  then
   if EvaluateString (r, r, s1, InfoSyntaxError)
   then
     if Skip (')', r,r, InfoSyntaxError)
     then
     Begin
      syntaxCheck := true;
      try
        StringResult := IntToStr(jsonAsArrayCountElements(s1));
      except
        StringResult := '';
        LogDatei.log('Error: Exception at jsonAsArrayCountElements with: "' + s1+'"', LLerror);
      end;
    End;
 end

 else if LowerCase (s) = LowerCase ('jsonAsObjectCountElements') then
 begin
  if Skip ('(', r, r, InfoSyntaxError)
  then
   if EvaluateString (r, r, s1, InfoSyntaxError)
   then
     if Skip (')', r,r, InfoSyntaxError)
     then
     Begin
      syntaxCheck := true;
      try
        StringResult := IntToStr(jsonAsObjectCountElements(s1));
      except
        StringResult := '';
        LogDatei.log('Error: Exception at jsonAsObjectCountElements with: "' + s1+'"', LLerror);
      end;
    End;
 end

 else if LowerCase (s) = LowerCase ('jsonAsArrayGetElementByIndex') then
 begin
   if Skip ('(', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s1, InfoSyntaxError)
     and Skip(',', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s2, InfoSyntaxError)
     and Skip (')', r, r, InfoSyntaxError)
   then
   Begin
      syntaxCheck := true;
      try
       StringResult := '';
         if jsonAsArrayGetElementByIndex(s1,StrToInt(s2),s3) then
           StringResult := s3
        else
          LogDatei.log('Error at jsonAsArrayGetElementByIndex with: "' + s1+'","'+s2+'"', LLerror);
      except
        StringResult := '';
        LogDatei.log('Error: Exception at jsonAsArrayGetElementByIndex with: "' + s1+'","'+s2+'"', LLerror);
      end;
   end;
 end

 else if LowerCase (s) = LowerCase ('jsonAsArrayDeleteObjectByIndex') then
 begin
   if Skip ('(', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s1, InfoSyntaxError)
     and Skip(',', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s2, InfoSyntaxError)
     and Skip (')', r, r, InfoSyntaxError)
   then
   Begin
      syntaxCheck := true;
      try
       StringResult := '';
         if jsonAsArrayDeleteObjectByIndex(s1,StrToInt(s2)) then
           StringResult := s1
        else
          LogDatei.log('Error at jsonAsArrayDeleteObjectByIndex with: "' + s1+'","'+s2+'"', LLerror);
      except
        StringResult := '';
        LogDatei.log('Error: Exception at jsonAsArrayDeleteObjectByIndex with: "' + s1+'","'+s2+'"', LLerror);
      end;
   end;
 end

 else if LowerCase (s) = LowerCase ('jsonAsObjectDeleteByKey') then
 begin
   if Skip ('(', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s1, InfoSyntaxError)
     and Skip(',', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s2, InfoSyntaxError)
     and Skip (')', r, r, InfoSyntaxError)
   then
   Begin
      syntaxCheck := true;
      try
       StringResult := '';
         if jsonAsObjectDeleteByKey(s1,s2) then
           StringResult := s1
        else
          LogDatei.log('Error at jsonAsObjectDeleteByKey with: "' + s1+'","'+s2+'"', LLerror);
      except
        StringResult := '';
        LogDatei.log('Error: Exception at jsonAsObjectDeleteByKey with: "' + s1+'","'+s2+'"', LLerror);
      end;
   end;
 end


 else if LowerCase (s) = LowerCase ('jsonAsObjectGetValueByKey') then
 begin
   if Skip ('(', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s1, InfoSyntaxError)
     and Skip(',', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s2, InfoSyntaxError)
     and Skip (')', r, r, InfoSyntaxError)
   then
   Begin
      syntaxCheck := true;
      try
        StringResult := '';
        s3 :='';
        if jsonAsObjectGetValueByKey(s1,s2,s3) then
           StringResult := s3
        else
          LogDatei.log('Error at jsonAsObjectGetValueByKey with: "' + s1+'","'+s2+'"', LLerror);
      except
        StringResult := '';
        LogDatei.log('Error: Exception at jsonAsObjectGetValueByKey with: "' + s1+'","'+s2+'"', LLerror);
      end;
   end;
 end


 else if LowerCase (s) = LowerCase ('jsonAsArrayPutObjectByIndex') then
 begin
   if Skip ('(', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s1, InfoSyntaxError)
     and Skip(',', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s2, InfoSyntaxError)
     and Skip(',', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s3, InfoSyntaxError)
     and Skip (')', r, r, InfoSyntaxError)
   then
   Begin
      syntaxCheck := true;
      try
        StringResult := '';
        if jsonAsArrayPutObjectByIndex(s3,s1,StrToInt(s2)) then
           StringResult := s1
        else
          LogDatei.log('Error at jsonAsArrayPutObjectByIndex with: "' + s1+'","'+s2+'","'+s3+'"', LLerror);
      except
        StringResult := '';
        LogDatei.log('Error: Exception at jsonAsArrayPutObjectByIndex with: "' + s1+'","'+s2+'","'+s3+'"', LLerror);
      end;
   end;
 end

 else if LowerCase (s) = LowerCase ('jsonAsObjectSetValueByKey') then
 begin
   if Skip ('(', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s1, InfoSyntaxError)
     and Skip(',', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s2, InfoSyntaxError)
     and Skip(',', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s3, InfoSyntaxError)
     and Skip (')', r, r, InfoSyntaxError)
   then
   Begin
      syntaxCheck := true;
      try
        StringResult := '';
        s4:='';
        if jsonAsObjectSetValueByKey(s1,s2,s3,s4) then
           StringResult := s4
        else
          LogDatei.log('Error at jsonAsObjectSetValueByKey with: "' + s1+'","'+s2+'","'+s3+'"', LLerror);
      except
        StringResult := '';
        LogDatei.log('Error: Exception at jsonAsObjectSetValueByKey with: "' + s1+'","'+s2+'","'+s3+'"', LLerror);
      end;
   end;
 end

 else if LowerCase (s) = LowerCase ('jsonAsObjectSetStringtypeValueByKey') then
 begin
   if Skip ('(', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s1, InfoSyntaxError)
     and Skip(',', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s2, InfoSyntaxError)
     and Skip(',', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s3, InfoSyntaxError)
     and Skip (')', r, r, InfoSyntaxError)
   then
   Begin
      syntaxCheck := true;
      try
        StringResult := '';
        s4:='';
        if jsonAsObjectSetStringtypeValueByKey(s1,s2,s3,s4) then
           StringResult := s4
        else
          LogDatei.log('Error at jsonAsObjectSetStringtypeValueByKey with: "' + s1+'","'+s2+'","'+s3+'"', LLerror);
      except
        StringResult := '';
        LogDatei.log('Error: Exception at jsonAsObjectSetStringtypeValueByKey with: "' + s1+'","'+s2+'","'+s3+'"', LLerror);
      end;
   end;
 end

 else if LowerCase (s) = LowerCase ('jsonAsObjectAddKeyAndValue') then
 begin
   if Skip ('(', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s1, InfoSyntaxError)
     and Skip(',', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s2, InfoSyntaxError)
     and Skip(',', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s3, InfoSyntaxError)
     and Skip (')', r, r, InfoSyntaxError)
   then
   Begin
      syntaxCheck := true;
      try
        StringResult := '';
        s4:='';
        if jsonAsObjectAddKeyAndValue(s1,s2,s3,s4) then
           StringResult := s4
        else
          LogDatei.log('Error at jsonAsObjectAddKeyAndValue with: "' + s1+'","'+s2+'","'+s3+'"', LLerror);
      except
        StringResult := '';
        LogDatei.log('Error: Exception at jsonAsObjectAddKeyAndValue with: "' + s1+'","'+s2+'","'+s3+'"', LLerror);
      end;
   end;
 end

  else if LowerCase (s) = LowerCase ('jsonStringListToJsonArray')
 then
 begin
    if Skip ('(', r, r, InfoSyntaxError)
    then
    Begin
       syntaxcheck := true;
       stringresult := '';
       list1 := TXStringList.create;
       slist.clear;

       if not produceStringList (script, r, r, list1, InfoSyntaxError)
         or not Skip (')', r,r, InfoSyntaxError)
       then
            syntaxCheck := false
       else
       Begin
         try
           slist.text := list1.text;
         if not stringListToJsonArray(slist,stringresult) then
         begin
           LogDatei.log('Error at jsonStringListToJsonArray ', LLerror);
           stringresult := '';
         end;
         except
           LogDatei.log('Error Exception at jsonStringListToJsonArray ', LLerror);
           stringresult := '';
         end
       End;
                list1.Free;
         list1 := nil;

    End
 End

 else if LowerCase (s) = LowerCase ('createUrl')
 then
 begin
    if Skip ('(', r, r, InfoSyntaxError)
    then
    Begin
       syntaxcheck := true;
       stringresult := '';
       list1 := TXStringList.create;
       if not produceStringList (script, r, r, list1, InfoSyntaxError)
         or not Skip (')', r,r, InfoSyntaxError)
       then
            syntaxCheck := false
       else
       Begin
        syntaxCheck := true;
        StringResult := createUrl(list1);
       End;
       list1.Free;
       list1 := nil;
    End
 End

  else if LowerCase (s) = LowerCase ('getStringFromListAtIndex') then
  begin
    if Skip ('(', r, r, InfoSyntaxError)
      and  produceStringList (script, r, r, list1, InfoSyntaxError)
      and Skip(',', r, r, InfoSyntaxError)
      and EvaluateString(r,r,s1, InfoSyntaxError)
      and Skip (')', r, r, InfoSyntaxError)
    then
    Begin
       syntaxCheck := true;
       try
         StringResult := '';
         StringResult := list1.Strings[StrToInt(s1)];
       except
         StringResult := '';
         LogDatei.log('Error: Exception at getStringFromListAtIndex with: "' + s1+'"', LLerror);
       end;
    end;
  end

 else if LowerCase (s) = LowerCase ('RandomStr') then
 begin
   StringResult := randomstr(true);
   syntaxCheck := true;
 end

 else if LowerCase (s) = LowerCase ('createNewOpsiHostKey') then
 begin
   StringResult := createNewOpsiHostKey;
   syntaxCheck := true;
 end

 else if LowerCase (s) = LowerCase ('timeStampAsFloatStr') then
 begin
   DecimalSeparator := '.';
   StringResult := floattostrF(now,ffFixed,15,3);
   syntaxCheck := true;
 end


 (* comparison Expressionstrs *)
 else if LowerCase (s) = LowerCase ('CompareStrings') then
 begin
   if Skip ('(', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s1, InfoSyntaxError)
     and Skip(',', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s2, InfoSyntaxError)
     and Skip (')', r, r, InfoSyntaxError)
   then
   begin
    syntaxCheck := true;
    StringResult := inttostr (getCompareSignStrings(s1, s2));
   end;
 end

 else if LowerCase (s) = LowerCase ('CompareNumbers') then
 begin
   if Skip ('(', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s1, InfoSyntaxError)
     and Skip(',', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s2, InfoSyntaxError)
     and Skip (')', r, r, InfoSyntaxError)
   then
   begin
    syntaxCheck := true;
    OldNumberOfErrors := LogDatei.NumberOfErrors;

    try
      n1 := StrToInt(s1);
      try
        n2 := StrToInt(s2);
        StringResult := inttostr (getCompareSign(n1, n2));
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

 else if LowerCase (s) = LowerCase ('getIP4NetworkByAdrAndMask') then
 begin
   if Skip ('(', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s1, InfoSyntaxError)
     and Skip(',', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s2, InfoSyntaxError)
     and Skip (')', r, r, InfoSyntaxError)
   then
   begin
    syntaxCheck := true;
    if isip(s1) then
      StringResult := getIP4NetworkByAdrAndMask(s1,s2)
    else
    begin
      StringResult :=  '';
      Logdatei.log('Error: '+s1+' is not a valid IPv4 Address', LLerror);
    end;
   end;
 end

 else if LowerCase (s) = LowerCase ('CompareDotSeparatedNumbers') then
 begin
   if Skip ('(', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s1, InfoSyntaxError)
     and Skip(',', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s2, InfoSyntaxError)
     and Skip (')', r, r, InfoSyntaxError)
   then
   begin
    syntaxCheck := true;
    OldNumberOfErrors := LogDatei.NumberOfErrors;
    StringResult := '0';
    if getDecimalCompareSign(s1, s2, intresult, errorinfo, false)
    then
      StringResult := inttostr (intresult)
    else
    begin
      LogDatei.log('Error: ' + errorinfo, LLError);
      LogDatei.log('Error: CompareDotSeparatedNumbers: using default result = 0 ', LLError);
    end;

    DiffNumberOfErrors := LogDatei.NumberOfErrors - OldNumberOfErrors;
    FNumberOfErrors := NumberOfErrors + DiffNumberOfErrors;
   end;
 end

 else if LowerCase (s) = LowerCase ('CompareDotSeparatedStrings') then
 begin
   if Skip ('(', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s1, InfoSyntaxError)
     and Skip(',', r, r, InfoSyntaxError)
     and EvaluateString(r,r,s2, InfoSyntaxError)
     and Skip (')', r, r, InfoSyntaxError)
   then
   begin
    syntaxCheck := true;

    OldNumberOfErrors := LogDatei.NumberOfErrors;

    if getDecimalCompareSign(s1, s2, intresult, errorinfo, true)
    then
      StringResult := inttostr (intresult)
    else
      LogDatei.log('Error: ' + errorinfo, LLError);

    DiffNumberOfErrors := LogDatei.NumberOfErrors - OldNumberOfErrors;
    FNumberOfErrors := NumberOfErrors + DiffNumberOfErrors;
   end;
 end


 else if LowerCase (s) = LowerCase ('ParamStr') then
 begin
   StringResult := ExtraParameter;
   syntaxCheck := true;
 end

 else if LowerCase (s) = LowerCase ('SubstringBefore')
 then
 begin
  if Skip ('(', r, r, InfoSyntaxError)
  then
   if EvaluateString (r, r, s1, InfoSyntaxError)
   then
     if Skip (',', r,r, InfoSyntaxError)
     then
      if EvaluateString (r,r, s2, InfoSyntaxError)
      then
       if Skip (')', r,r, InfoSyntaxError)
       then
       Begin
         syntaxCheck := true;

         StringResult := '';
         if  (length (s1) >= length (s2))
            and (copy (s1, length(s1) - length(s2) + 1, length (s2)) = s2)

         then
           StringResult := copy (s1, 1, length(s1) - length(s2))
       End
 End


 else if LowerCase (s) = LowerCase ('unquote')
 then
 begin
  if Skip ('(', r, r, InfoSyntaxError)
  then
   if EvaluateString (r, r, s1, InfoSyntaxError)
   then
     if Skip (',', r,r, InfoSyntaxError)
     then
      if EvaluateString (r,r, s2, InfoSyntaxError)
      then
       if Skip (')', r,r, InfoSyntaxError)
       then
       Begin
         syntaxCheck := true;
         StringResult := opsiUnquotestr(s1,s2);
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
       End
 End

 else if LowerCase (s) = LowerCase ('unquote2')
 then
 begin
  if Skip ('(', r, r, InfoSyntaxError)
  then
   if EvaluateString (r, r, s1, InfoSyntaxError)
   then
     if Skip (',', r,r, InfoSyntaxError)
     then
      if EvaluateString (r,r, s2, InfoSyntaxError)
      then
       if Skip (')', r,r, InfoSyntaxError)
       then
       Begin
         syntaxCheck := true;
         StringResult := opsiUnquotestr2(s1,s2);
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
       End
 End



 else if LowerCase (s) = LowerCase ('stringreplace')
 then
 begin
  if Skip ('(', r, r, InfoSyntaxError)
  then
   if EvaluateString (r, r, s1, InfoSyntaxError)
   then
     if Skip (',', r,r, InfoSyntaxError)
     then
      if EvaluateString (r,r, s2, InfoSyntaxError)
      then
        if Skip (',', r,r, InfoSyntaxError)
        then
         if EvaluateString (r,r, s3, InfoSyntaxError)
         then
          if Skip (')', r,r, InfoSyntaxError)
          then
          Begin
            syntaxCheck := true;
            StringResult := StringReplace1(s1,s2,s3);
            if (StringResult = NULL_STRING_VALUE)  then  StringResult := s1;
          End
 End

 else if LowerCase (s) = LowerCase ('stringReplaceRegex')
 then
 begin
  if Skip ('(', r, r, InfoSyntaxError) then
   if EvaluateString (r, r, s1, InfoSyntaxError) then
     if Skip (',', r,r, InfoSyntaxError) then
      if EvaluateString (r,r, s2, InfoSyntaxError) then
        if Skip (',', r,r, InfoSyntaxError) then
         if EvaluateString (r,r, s3, InfoSyntaxError) then
          if Skip (')', r,r, InfoSyntaxError) then
          Begin
            syntaxCheck := true;
            StringResult := stringReplaceRegex(s1,s2,s3);
          End
 End

 else if LowerCase (s) = LowerCase ('reencodestr')
 then
 begin
  if Skip ('(', r, r, InfoSyntaxError)
  then
   if EvaluateString (r, r, s1, InfoSyntaxError)
   then
     if Skip (',', r,r, InfoSyntaxError)
     then
      if EvaluateString (r,r, s2, InfoSyntaxError)
      then
        if Skip (',', r,r, InfoSyntaxError)
        then
         if EvaluateString (r,r, s4, InfoSyntaxError)
         then
          if Skip (')', r,r, InfoSyntaxError)
          then
          Begin
            syntaxCheck := true;
            // s3 is the used encoding
            StringResult := reencode(s1,s2,s3,s4);
          End
 End

 else if LowerCase (s) = LowerCase ('StringSplit')
   // deprecated, please replace by splitting the string by splitstring and
   // returning a specific part by takestring
 then
 begin
  if Skip ('(', r, r, InfoSyntaxError)
  then
   if EvaluateString (r, r, s1, InfoSyntaxError)
   then
     if Skip (',', r,r, InfoSyntaxError)
     then
      if EvaluateString (r, r, s2, InfoSyntaxError)
      then
       if Skip (',', r,r, InfoSyntaxError)
       then
        if EvaluateString (r, r, sx, InfoSyntaxError)
        then
           if Skip (')', r,r, InfoSyntaxError)
           then
           Begin
             syntaxCheck := true;

             itemlist.free;
             itemlist := TXStringList.create;
             stringsplit (s1, s2, itemlist);

             try
                ListIndex := strtoint (sx);
             except
                syntaxCheck := false;
                InfoSyntaxError := sx + ' keine ganze Zahl';
                ListIndex := 0;
             end;

             if ListIndex >= itemList.count
             then
             Begin
                StringResult := '';
                OldNumberOfWarnings := LogDatei.NumberOfWarnings;
                LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
                LogDatei.log  ('Warning: String "' + s1 + '" using delimiter "'
                                              + s2 + '" was not splitted in ('
                                              + inttostr (ListIndex) + ' + 1)  parts', LLWarning);
                DiffNumberOfWarnings := LogDatei.NumberOfWarnings - OldNumberOfWarnings;
                FNumberOfWarnings := NumberOfWarnings + DiffNumberOfWarnings;
                LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
            End
             else
                StringResult := itemlist[ListIndex];
           End
 End


 else if LowerCase (s) = LowerCase ('takeString')
 then
 begin
    if Skip ('(', r, r, InfoSyntaxError)
    then
    Begin
       syntaxcheck := true;
       stringresult := '';
       try
         GetWord (r, s1, r, [',']);

         a1 := strtoint(s1);

         syntaxCheck := Skip(',', r, r, InfoSyntaxError);
       except
          try
            sx := s1;
            if EvaluateString (s1, s1, s2, InfoSyntaxError) then
            begin
              a1 := strtoint(s2);
              syntaxCheck := Skip(',', r, r, InfoSyntaxError);
            end
            else  a1 := strtoint(sx);
          except
            syntaxcheck := false;
            InfoSyntaxError := ' No valid index for list ';
          end;
       end;

       if syntaxCheck then
       Begin
         list1 := TXStringList.create;
         r1 := r;
         if not produceStringList (script,r, r, list1, InfoSyntaxError)
         or not Skip (')', r,r, InfoSyntaxError)
         then
            syntaxCheck := false
         else
         Begin
           if list1.count = 0 then // list is empty
           begin
             Logdatei.log('Stringlist '+ r1 + ' is empty in takeString function ! Use count() before takestring() to avoid this problem.',LLWarning);
             if FatalOnRuntimeError then
             begin
               Logdatei.log('Set to FatalError because FatalOnRuntimeError is set',LLCritical);
               FExtremeErrorLevel := LevelFatal;
               LogDatei.ActionProgress:='Runtime Error';
             end;
           end
           else
           begin
             if (a1 < 0)
             then
                a1 := list1.count + a1; //we count downward

             if (a1 < 0) or (a1 > list1.count - 1)
             then
             begin
               Logdatei.log ('Stringlist list ' + r1 + ' has '+ intToStr(list1.count) +' elements. And the effective list index ' + inttostr (a1)
                          + ' is out of bounds in takeString function ! Use count() before takestring() to avoid this problem.',LLWarning);
               if FatalOnRuntimeError then
               begin
                 Logdatei.log('Set to FatalError because FatalOnRuntimeError is set',LLCritical);
                 FExtremeErrorLevel := LevelFatal;
                 LogDatei.ActionProgress:='Runtime Error';
               end;
             end
             else
               stringresult := list1[a1];
           end;
         End;

         list1.Free;
         list1 := nil;
       End
    End
 End

 else if LowerCase (s) = LowerCase ('count')
 then
 begin
    if Skip ('(', r, r, InfoSyntaxError)
    then
    Begin
       syntaxcheck := true;
       stringresult := '';
       list1 := TXStringList.create;

       if not produceStringList (script, r, r, list1, InfoSyntaxError)
         or not Skip (')', r,r, InfoSyntaxError)
       then
            syntaxCheck := false
       else
       Begin
         stringresult := intToStr(list1.count);
         list1.Free;
         list1 := nil;
       End
    End
 End

 else if LowerCase (s) = LowerCase ('getValue')
 then
 begin
    if Skip ('(', r, r, InfoSyntaxError)
    then
    Begin
       syntaxcheck := true;
       stringresult := '';

       if not evaluateString (r, r, s1, InfoSyntaxError)
       or
       not Skip (',', r,r, InfoSyntaxError)
       then syntaxCheck := false;

       if syntaxCheck then
       Begin
         list1 := TXStringList.create;

         if not produceStringList (script, r, r, list1, InfoSyntaxError)
         or not Skip (')', r,r, InfoSyntaxError)
         then
            syntaxCheck := false
         else
         Begin
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
         End;

         list1.Free;
         list1 := nil;
       End
    End
 End

 else if LowerCase (s) = LowerCase ('getValueBySeparator')
 then
 begin
   syntaxCheck := false;
   stringresult := '';
  if Skip ('(', r, r, InfoSyntaxError)
  then
   if EvaluateString (r, r, s1, InfoSyntaxError)
   then
     if Skip (',', r,r, InfoSyntaxError)
     then
      if EvaluateString (r, r, s2, InfoSyntaxError)
      then
       if Skip (',', r,r, InfoSyntaxError)
       then
         syntaxCheck := true;


       if syntaxCheck then
       Begin
         list1 := TXStringList.create;

         if not produceStringList (script, r, r, list1, InfoSyntaxError)
         or not Skip (')', r,r, InfoSyntaxError)
         then
            syntaxCheck := false
         else
         Begin
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
         End;

         list1.Free;
         list1 := nil;
       End
 End

 else if LowerCase (s) = LowerCase ('getValueFromFile')
 then
 begin
    if Skip ('(', r, r, InfoSyntaxError)
    then
    Begin
       syntaxcheck := true;
       stringresult := '';

       if not evaluateString (r, r, s1, InfoSyntaxError)
       or
       not Skip (',', r,r, InfoSyntaxError)
       then syntaxCheck := false;

       if syntaxCheck then
       Begin
         list1 := TXStringList.create;
         if EvaluateString (r, r, s3, InfoSyntaxError)
         then
         Begin
           try
             s3 := ExpandFileName(s3);
             list1.loadfromfile (s3);
             list1.Text:= reencode(list1.Text, 'system');
           except
             on e: exception do
             begin
               LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
               LogDatei.log('Error on loading file: ' + e.message, LLError);
               LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
             end
           end;
           if not Skip (')', r,r, InfoSyntaxError)
           then
              syntaxCheck := false
           else
           Begin
             syntaxCheck := true;
             stringresult := list1.getStringValue(s1);
             if stringresult = 'NULL' then stringresult := '';

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
           End;
         end;
         list1.Free;
         list1 := nil;
       End
    End
 End


 else if LowerCase (s) = LowerCase ('getValueFromFileBySeparator')
 then
 begin
   syntaxCheck := false;
   stringresult := '';
  if Skip ('(', r, r, InfoSyntaxError)
  then
   if EvaluateString (r, r, s1, InfoSyntaxError)
   then
     if Skip (',', r,r, InfoSyntaxError)
     then
      if EvaluateString (r, r, s2, InfoSyntaxError)
      then
       if Skip (',', r,r, InfoSyntaxError)
       then
         syntaxCheck := true;


       if syntaxCheck then
       Begin
         list1 := TXStringList.create;
         if EvaluateString (r, r, s3, InfoSyntaxError)
         then
         Begin
           try
             s3 := ExpandFileName(s3);
             list1.loadfromfile (s3);
             list1.Text:= reencode(list1.Text, 'system');
           except
             on e: exception do
             begin
               LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
               LogDatei.log('Error on loading file: ' + e.message, LLError);
               LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
             end
           end;
           if not Skip (')', r,r, InfoSyntaxError)
           then
              syntaxCheck := false
           else
           Begin
             syntaxCheck := true;
             // use the first char of the second argument as separator char
             list1.NameValueSeparator := trim(s2)[1];
             stringresult := list1.getStringValue(s1);
             if stringresult = 'NULL' then stringresult := '';
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
           End;
         end;
         list1.Free;
         list1 := nil;
       End
 End

  else if LowerCase (s) = LowerCase ('takeFirstStringContaining')
 then
 begin
   if Skip ('(', r, r, InfoSyntaxError)
   then
   Begin

     list1 := TXStringList.create;

     if produceStringList (script, r, r, list1, InfoSyntaxError)
        and skip (',', r, r, InfoSyntaxError)
     then
     Begin
       if EvaluateString (r, r, s1, InfoSyntaxError)
       and
       skip (')', r, r, InfoSyntaxError)
       then
       Begin
         SyntaxCheck := true;
         stringResult := '';
         i := 0;
         while (stringResult = '') and (i < list1.Count) do
         begin
           if AnsiContainsText(list1[i], s1) then
            stringResult := list1[i]
           else
             inc(i);
         end;
       End
     End
   End;
 End

 else if LowerCase (s) = LowerCase ('getIndexFromListByContaining')
 then
 begin
   if Skip ('(', r, r, InfoSyntaxError)
   then
   Begin

     list1 := TXStringList.create;

     if produceStringList (script, r, r, list1, InfoSyntaxError)
        and skip (',', r, r, InfoSyntaxError)
     then
     Begin
       if EvaluateString (r, r, s1, InfoSyntaxError)
       and
       skip (')', r, r, InfoSyntaxError)
       then
       Begin
         SyntaxCheck := true;
         stringResult := '';
         i := 0;
         while (stringResult = '') and (i < list1.Count) do
         begin
           if AnsiContainsText(list1[i], s1) then
            stringResult := intToStr(i)
           else
             inc(i);
         end;
       End
     End
   End;
 End


 else if LowerCase (s) = LowerCase ('composeString')
 then
 begin
   if Skip ('(', r, r, InfoSyntaxError)
   then
   Begin

     list1 := TXStringList.create;

     if produceStringList (script, r, r, list1, InfoSyntaxError)
        and skip (',', r, r, InfoSyntaxError)
     then
     Begin
       if EvaluateString (r, r, s1, InfoSyntaxError)
       and
       skip (')', r, r, InfoSyntaxError)
       then
       Begin
         SyntaxCheck := true;
         stringResult := '';
         for i := 0 to list1.Count - 2
         do
           stringResult := stringResult + list1.strings[i] + s1;

         stringResult := stringResult + list1[list1.count-1];
       End
     End
   End;
 End

 else if LowerCase (s) = LowerCase ('ExtractFilePath')
 then
 begin
   if Skip ('(', r, r, InfoSyntaxError)
   then
    if EvaluateString (r, r, s1, InfoSyntaxError)
    then
      if Skip (')', r, r, InfoSyntaxError)
      then
      Begin
        try
         StringResult := ExtractFilePath (s1);
         syntaxCheck := true;
        except
          InfoSyntaxError := '"' + s1 + '" is not a valid file path';
        end;
      End
 end

  else if LowerCase (s) = LowerCase ('RegString')
 then
 begin
   if Skip ('(', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s1, InfoSyntaxError)
   then if Skip (')', r, r, InfoSyntaxError)
   then
   Begin
     StringResult := CEscaping (s1);
     syntaxCheck := true;
   End
 end

 {$IFDEF WINDOWS}
 {$IFDEF WIN32}
 else if LowerCase (s) = LowerCase ('SidToName')
 then
 begin
   if Skip ('(', r, r, InfoSyntaxError)
   then
    if EvaluateString (r, r, s1, InfoSyntaxError)
    then
      if Skip (')', r, r, InfoSyntaxError)
      then
      Begin
        try
         StringResult := StrSIDToName (s1);
         syntaxCheck := true;
        except
          on e: exception do
          Begin
            InfoSyntaxError := '"' + s1 + '" is not a valid sid string';
          end
        end;
      End
 end

 else if LowerCase (s) = LowerCase ('NameToSID')
 then
 begin
   if Skip ('(', r, r, InfoSyntaxError)
   then
    if EvaluateString (r, r, s1, InfoSyntaxError)
    then
      if Skip (')', r, r, InfoSyntaxError)
      then
      Begin
        try
         StringResult := GetLocalUserSidStr(s1);
         syntaxCheck := true;
        except
          on e: exception do
          Begin
            InfoSyntaxError := '"' + s1 + '" is not a valid sid string';
          end;
        end;
      End
 end
{$ENDIF WIN32}


else if (LowerCase (s) = LowerCase ('GetRegistryValue')) then
  begin
   s3 := '';
   tmpstr2 := '';
   tmpbool := true;
   syntaxCheck := false;
   StringResult := '';
   LogDatei.log_prog ('GetRegistryValue from: '+r, LLdebug3);
   if Skip ('(', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s1, InfoSyntaxError)
   then if Skip (',', r, r, InfoSyntaxError)
   then if EvaluateString (r, tmpstr, s2, InfoSyntaxError) then
   // next after ',' or ')'
   if Skip (',', tmpstr, tmpstr1, tmpstr3) then
     if EvaluateString (tmpstr1, tmpstr2, s3, tmpstr3) then;
   if s3 = '' then
   begin
     // only two parameter
     if Skip (')', tmpstr, r, InfoSyntaxError) then
     Begin
       syntaxCheck := true;
     end;
   end
   else
   begin
     // three parameter
     if Skip (')', tmpstr2, r, InfoSyntaxError) then
     Begin
       syntaxCheck := true;
       try
         tmpbool := true;
         if lowercase(s3) = '32bit' then tmpbool := false
         else if lowercase(s3) = '64bit' then tmpbool := true
         else if lowercase(s3) = 'sysnative' then tmpbool := true
         else
         begin
           InfoSyntaxError := 'Error: unknown parameter: '+s3+' expected one of 32bit,64bit,sysnative - fall back to sysnative';
           syntaxCheck := false;
         end;
       except
         Logdatei.log('Error: Exception in GetRegistryValue: ',LLError);
       end
     end;
   end;
   if syntaxCheck then
   begin
     GetWord (s1, key0, key, ['\']);
     LogDatei.log_prog ('GetRegistryValue from: '+key0+key+' ValueName: '+s2, LLdebug);
     StringResult := '';
     LogDatei.log ('key0 = '+key0, LLdebug2);
     if runLoginScripts and (('HKEY_CURRENT_USER' = UpperCase(key0)) or ('HKCU' = UpperCase(key0))) then
     begin
       // remove HKCU from the beginning
       // switch to HKEY_USERS
       key0 := 'HKEY_USERS';
       key := '\'+usercontextSID+key;
       LogDatei.log ('Running loginscripts: key0 is now: '+key0 + ', key is now: '+key, LLdebug);
     end;
     StringResult := GetRegistrystringvalue(key0+key,s2,tmpbool);
   end;
  end



 else if (LowerCase (s) = LowerCase ('GetRegistryStringValue'))
        or (LowerCase (s) = LowerCase ('GetRegistryStringValue32'))
  then
  begin
    LogDatei.log_prog ('GetRegistryStringValue from: '+r, LLdebug3);
    if Skip ('(', r, r, InfoSyntaxError)
    then if EvaluateString (r, r, s1, InfoSyntaxError) then
    begin
      LogDatei.log_prog ('GetRegistryStringValue from: '+s1+' Remaining: '+r, LLdebug3);
      if Skip (')', r, r, InfoSyntaxError) then
      Begin
        //GetWord (r1, key, r1, [']']);
        LogDatei.log_prog ('GetRegistryStringValue from: '+s1+' Remaining: '+r, LLdebug2);
        GetWord (s1, key, r1, [']'], true);
        key := trim(key)+']';
        p1 := pos('[',key);
        p2 := posFromEnd(']',key);
        p3 := length(key);
        p4 := length(trim(key));
        if not((pos('[',key) = 1) and (posFromEnd(']',key) = length(key))) then
        begin
          SyntaxCheck := false;
          ErrorInfo := 'Wrong Key Format: Key must be given inside [] - but we got: '+key
        end
        else
        begin
          key := opsiUnquotestr2(trim(key),'[]');
          if (pos('[',key) = 1) and (posFromEnd(']',key) = length(key)) then
          begin
            SyntaxCheck := false;
            ErrorInfo := 'Wrong Key Format: Have still brackets after removing them: '+key
          end
          else
          begin
            SyntaxCheck := true;
            GetWord (key, key0, key, ['\']);
            System.delete (key, 1, 1);
            if Skip (']', r1, r1, InfoSyntaxError) then
              ValueName := r1;
              //GetWord (r1, ValueName, r1, [''], true);
              //GetWord (r1, ValueName, r1, WordDelimiterSet1);
            ValueName := trim(ValueName);
            LogDatei.log_prog ('GetRegistryStringValue from: '+key0+'\'+key+' ValueName: '+ValueName, LLdebug);
            StringResult := '';
            LogDatei.log ('key0 = '+key0, LLdebug2);
            if runLoginScripts and (('HKEY_CURRENT_USER' = UpperCase(key0)) or ('HKCU' = UpperCase(key0))) then
            begin
              // remove HKCU from the beginning
              // switch to HKEY_USERS
              key0 := 'HKEY_USERS';
              key := usercontextSID+'\'+key;
              LogDatei.log ('Running loginscripts: key0 is now: '+key0 + ', key is now: '+key, LLdebug);
            end;
            StringResult := GetRegistrystringvalue(key0+'\'+key,ValueName,false);
          End
        end;
      end;
    End;
  end

  else if (LowerCase (s) = LowerCase ('GetRegistryStringValue64'))
        or (LowerCase (s) = LowerCase ('GetRegistryStringValueSysNative'))
  then
  begin
    LogDatei.log_prog ('GetRegistryStringValue from: '+r, LLdebug3);
    if Skip ('(', r, r, InfoSyntaxError)
    then if EvaluateString (r, r, s1, InfoSyntaxError) then
    begin
      LogDatei.log_prog ('GetRegistryStringValue from: '+s1+' Remaining: '+r, LLdebug3);
      if Skip (')', r, r, InfoSyntaxError) then
      Begin
        //GetWord (r1, key, r1, [']']);
        LogDatei.log_prog ('GetRegistryStringValue from: '+s1+' Remaining: '+r, LLdebug2);
        GetWord (s1, key, r1, [']'], true);
        key := trim(key)+']';
        p1 := pos('[',key);
        p2 := posFromEnd(']',key);
        p3 := length(key);
        p4 := length(trim(key));
        if not((pos('[',key) = 1) and (posFromEnd(']',key) = length(key))) then
        begin
          SyntaxCheck := false;
          ErrorInfo := 'Wrong Key Format: Key must be given inside [] - but we got: '+key
        end
        else
        begin
          key := opsiUnquotestr2(trim(key),'[]');
          if (pos('[',key) = 1) and (posFromEnd(']',key) = length(key)) then
          begin
            SyntaxCheck := false;
            ErrorInfo := 'Wrong Key Format: Have still brackets after removing them: '+key
          end
          else
          begin
            SyntaxCheck := true;
            GetWord (key, key0, key, ['\']);
            System.delete (key, 1, 1);
            if Skip (']', r1, r1, InfoSyntaxError) then
              ValueName := r1;
              //GetWord (r1, ValueName, r1, [''], true);
              //GetWord (r1, ValueName, r1, WordDelimiterSet1);
            ValueName := trim(ValueName);
            LogDatei.log_prog ('GetRegistryStringValue from: '+key0+'\'+key+' ValueName: '+ValueName, LLdebug);
            StringResult := '';
            if runLoginScripts and (('HKEY_CURRENT_USER' = UpperCase(key0)) or ('HKCU' = UpperCase(key0))) then
            begin
              // remove HKCU from the beginning
              // switch to HKEY_USERS
              key0 := 'HKEY_USERS';
              key := usercontextSID+'\'+key;
              LogDatei.log ('Running loginscripts: key0 is now: '+key0 + ', key is now: '+key, LLdebug);
            end;
            if (GetNTVersionMajor = 5) and (GetNTVersionMinor = 0) then
              StringResult := GetRegistrystringvalue(key0+'\'+key,ValueName,false)
            else
              StringResult := GetRegistrystringvalue(key0+'\'+key,ValueName,true);
          End
        end;
      end;
    End;
  end

  {$IFDEF WIN32}
  else if LowerCase(s) = LowerCase('GetUserSID')
  then
  begin
    if Skip('(', r, r, InfoSyntaxError)
    then if EvaluateString(r, r, s1, InfoSyntaxError)
    then if Skip(')', r, r, InfoSyntaxError)
    then
    Begin
      if r <> ''
      then InfoSyntaxError := ErrorRemaining
      else
      Begin
        SyntaxCheck := true;
        itemlist := TXStringlist.create;
        stringsplit(s1, '\', itemlist);
        if itemlist.count > 1
        then
        Begin
          s2 := itemlist.Strings[0];
          s3 := itemlist.strings[itemlist.count - 1]
        end
        else
        Begin
          s2 := '';
          s3 := s1;
        End;
        logdatei.log('search user: ' + s3 + ' in domain: ' + s2 , LevelComplete);
        s4 := ''; //founddomain
        StringResult := GetDomainUserSidS(s2, s3, s4);

        logdatei.log('found in domain ' + s4 + ', SID: ' + StringResult, LevelComplete);

        if length(StringResult) > 1
        then
        Begin
          Skip('[', StringResult, StringResult, InfoSyntaxError);
          if StringResult[length(StringResult)] = ']'
          then StringResult := copy(StringResult, 1, length(StringResult) - 1);
        End;

        if StringResult = ''
        then
        Begin
          OldNumberOfErrors := LogDatei.NumberOfErrors;
          LogDatei.log  ('Error: SID not found for "' + s1 + '"', LLError);
          DiffNumberOfErrors := LogDatei.NumberOfErrors - OldNumberOfErrors;
          FNumberOfErrors := NumberOfErrors + DiffNumberOfErrors;
        End
      end
    End
  End
  {$ENDIF WIN32}
  {$ELSE WINDOWS}
  else if LowerCase (s) = LowerCase ('SidToName')
  then
  begin
    SyntaxCheck := false;
    InfoSyntaxError := 'Not implemented for Linux';
    StringResult := 'Error';
    LogDatei.log('SyntaxError: SidToName not implemented for Linux',LLError );
  end

  else if LowerCase (s) = LowerCase ('NameToSID')
  then
  begin
    SyntaxCheck := false;
    InfoSyntaxError := 'Not implemented for Linux';
    StringResult := 'Error';
    LogDatei.log('SyntaxError: NameToSID not implemented for Linux',LLError );
  end



  else if (LowerCase (s) = LowerCase ('GetRegistryStringValue'))
        or (LowerCase (s) = LowerCase ('GetRegistryStringValue32'))
        or (LowerCase (s) = LowerCase ('GetRegistryStringValue64'))
        or (LowerCase (s) = LowerCase ('GetRegistryStringValueSysNative'))
  then
  begin
    SyntaxCheck := false;
    InfoSyntaxError := 'Not implemented for Linux';
    StringResult := 'Error';
    LogDatei.log('SyntaxError: GetRegistryStringValue not implemented for Linux',LLError );
  end


  else if LowerCase(s) = LowerCase('GetUserSID')
  then
  begin
    SyntaxCheck := false;
    InfoSyntaxError := 'Not implemented for Linux';
    StringResult := 'Error';
    LogDatei.log('SyntaxError: GetUserSID not implemented for Linux',LLError );
  End
  {$ENDIF WINDOWS}

  else if LowerCase(s) = LowerCase('getLastServiceErrorClass')
  then
  begin
    SyntaxCheck := true;
    testresult := '';
    errorOccured := false;
    if opsidata = nil
    then
    Begin
      errorOccured := true;
      testresult := '!!! no opsidata !!!';
    End
    else
      try
        local_opsidata := opsidata;

        //LogDatei.log('Calling opsi service at ' + local_opsidata.serviceUrl, LevelComplete);
      except
        errorOccured := true;
        testresult := '!!! not in service mode !!!';
      end;


      if errorOccured
      then
        stringresult := testresult
      else
      begin
        if local_opsidata.ServiceLastErrorInfo.indexOfName('class') < 0
        then
          stringresult := 'None' //'!!! error key "class" not found !!!'
        else
          stringresult := local_opsidata.ServiceLastErrorInfo.values['class'];
      end
  end


  else if LowerCase(s) = LowerCase('getLastServiceErrorMessage')
  then
  begin
    testresult := '';
    SyntaxCheck := true;
    errorOccured := false;
    if opsidata = nil
    then
    Begin
      errorOccured := true;
      testresult := '!!! no opsidata !!!';
    End
    else
      try
        local_opsidata := opsidata;

        //LogDatei.log('Calling opsi service at ' + local_opsidata.serviceUrl, LevelComplete);
      except
        errorOccured := true;
        testresult := '!!! not in service mode !!!';
      end;

     if errorOccured
     then
      stringresult := testresult
     else
     begin
      if local_opsidata.ServiceLastErrorInfo.indexOfName('message') < 0
      then
        stringresult := 'None' //'!!! error key "message" not found !!!'
      else
        stringresult := local_opsidata.ServiceLastErrorInfo.values['message'];
     end;
  end

  else if LowerCase(s) = LowerCase('demandLicenseKey')
  then
   begin
      errorOccured := false;
      syntaxCheck := true;
      testresult := '';

      r := trim(r);
      setLength(parameters, 4);

      parameters[0] := osconf.computername;
      parameters[1] := '';
      parameters[2] := '';
      parameters[3] := '';

      syntaxcheck := skip ('(', r, r, InfoSyntaxError);

      j := 1;
      continue := true;

      while syntaxCheck and continue do
      Begin
        if skip (')', r, r, InfoSyntaxError)
          and (r = '')
        then
          continue :=  false
        else
        Begin
          if j > 3 then syntaxcheck := false
          else
          begin
            if j > 1
            then
               syntaxcheck := skip (',', r, r, InfoSyntaxError);

             if syntaxCheck
             then
               syntaxCheck := EvaluateString(r, r, parameters[j], InfoSyntaxError);
          end
        end;
        inc(j);
      end;

      if syntaxcheck
      then
      begin
        testresult := 'service request possible';
        if opsidata = nil
        then
        Begin
          errorOccured := true;
          testresult := '!!! no opsidata !!!';
        End
        else
         try
            local_opsidata := opsidata;

           LogDatei.log_prog('Calling opsi service at ' + local_opsidata.serviceUrl, LLDebug);
         except
           errorOccured := true;
           testresult := '!!! not in service mode !!!';
         end;

         if not errorOccured
         then
         Begin
            omc := TOpsiMethodCall.create ('getAndAssignSoftwareLicenseKey', parameters);

            testresult := local_opsidata.CheckAndRetrieveString (omc, errorOccured);
         end
      end;

      if errorOccured
      then
      Begin
          OldNumberOfErrors := LogDatei.NumberOfErrors;
          LogDatei.log  ('Error: Got no license key: ' + testresult, LLError);
          DiffNumberOfErrors := LogDatei.NumberOfErrors - OldNumberOfErrors;
          FNumberOfErrors := NumberOfErrors + DiffNumberOfErrors;
          stringresult := '';
      end
      else
        stringresult := testresult;
   end


  else if LowerCase(s) = LowerCase('freeLicense')
  then
   begin
      errorOccured := false;
      syntaxCheck := true;
      testresult := '';

      r := trim(r);
      setLength(parameters, 5);

      parameters[0] := osconf.computername;
      parameters[1] := '';
      parameters[2] := '';
      parameters[3] := '';
      parameters[4] := '';

      syntaxcheck := skip ('(', r, r, InfoSyntaxError);

      j := 2;
      continue := true;

      while syntaxCheck and continue do
      Begin
        if skip (')', r, r, InfoSyntaxError)
          and (r = '')
        then
          continue :=  false
        else
        Begin
          if j > 4 then syntaxcheck := false
          else
          begin
            if j > 2
            then
               syntaxcheck := skip (',', r, r, InfoSyntaxError);

             if syntaxCheck
             then
               syntaxCheck := EvaluateString(r, r, parameters[j], InfoSyntaxError);
          end
        end;
        inc(j);
      end;

      if syntaxcheck
      then
      begin
        testresult := 'service request possible';
        if opsidata = nil
        then
        Begin
          errorOccured := true;
          testresult := '!!! no opsidata !!!';
        End
        else
         try
            local_opsidata := opsidata;

           LogDatei.log_prog('Calling opsi service at ' + local_opsidata.serviceUrl, LLDebug);
         except
           errorOccured := true;
           testresult := '!!! not in service mode !!!';
         end;

         if not errorOccured
         then
         Begin
            omc := TOpsiMethodCall.create ('deleteSoftwareLicenseUsage', parameters);
            testresult := local_opsidata.CheckAndRetrieveString (omc, errorOccured);
         end
      end;

      if errorOccured
      then
      Begin
          OldNumberOfErrors := LogDatei.NumberOfErrors;
          LogDatei.log  ('Error: Could not delete software license usage ' + testresult, LLError);
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
 if syntaxCheck and skip ('+', r, r, sx)
 then
 Begin
    syntaxCheck := EvaluateString (r, r, s1, InfoSyntaxError);
    StringResult := StringResult + s1;
 End;

 if syntaxcheck
 then
    Remaining := r;

 result := syntaxCheck;

       (*  if Script.ExitOnError then
        Begin
          tsr := tsrExitProcess;
          .log (StopInfo, BaseLevel);
        End; *)

  LogDatei.LogSIndentLevel := StartIndentLevel;
  slist.Free;
End;


procedure TuibInstScript.GetWordOrStringExpressionstr (const s: String;
                           var resultString, Remaining, errorinfo : String);
 { nur fuer AktionenSektion-Syntax 0 }
 var s0 : String='';

begin
  Resultstring := '';
  s0 := s;
  setLength (s0, length (s)); //should create a new instance, but does not always work
  if not EvaluateString (s0, Remaining, resultString, errorInfo)
  then
    GetWord (s0, resultString, Remaining, WordDelimiterWhiteSpace);
end;



function TuibInstScript.doXMLAddNamespace(filename:string;
  const elementname:string;
  const namespace:string):boolean;

 var list:TStringList;
  s1,s2,s3,s4:string;
  i,p1,p2:integer;
  bChanged:Boolean;
  ///done : Boolean;
 begin
  Result:=false;
  bChanged:=False;
  list:=TStringList.Create;
  try
    filename := ExpandFileName(filename);
    list.LoadFromFile(filename);
    list.Text:= reencode(list.Text, 'system');
    for i:=0 to list.Count-1 do
    begin
      p1:=pos('<'+elementname,list[i]);
      if p1>0 then
      begin //Element found
        s1:=copy(list[i],1,p1-1); //String before '<element'
        s2:=copy(list[i],p1,Length(list[i])); //String after '<element'
        p2:=pos('>',s2);
        s3:=copy(s2,1,p2); //String from '<element' until '>'
        s4:=copy(s2,p2+1,Length(list[i])); //All behind '>'

        p2:=pos(namespace,s3);
        if p2=0 then // still existing anyway
        begin
          s3:=copy(s3,1,Length(s3)-1); //'>' cut
          s3:=s3+' '+namespace+'>'; //and append again

          list[i]:=s1+s3+s4;
          bChanged:=True;
          Result:=True;
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

 function TuibInstScript.doXMLRemoveNamespace(filename:string;
  const elementname:string;
  const namespace:string):boolean;
  var
    list:TStringList;
    s1,s2,s3,s4:string;
    i,p1,p2:integer;
    bChanged:Boolean;
  begin
    Result:=false;
    bChanged:=False;
    list:=TStringList.Create;
    try
      filename := ExpandFileName(filename);
      list.LoadFromFile(filename);
      list.Text:= reencode(list.Text, 'system');
      for i:=0 to list.Count-1 do
      begin
        p1:=pos('<'+elementname,list[i]);
        if p1>0 then
        begin //Element gefunden
          s1:=copy(list[i],1,p1-1); //String vor '<element'
          s2:=copy(list[i],p1,Length(list[i])); //String ab '<element'
          p2:=pos('>',s2);
          s3:=copy(s2,1,p2); //String von '<element' bis '>'
          s4:=copy(s2,p2+1,Length(list[i])); //Alles hinter '>'

          p2:=pos(namespace,s3);
          if p2>0 then //sonst nicht vorhanden
          begin
            System.delete(s3,p2,Length(namespace));
          if s3[p2-1]=' ' then //wir haben bei Add ein Leerzeichen hinzugefï¿½gt
          System.delete(s3,p2-1,1);

          list[i]:=s1+s3+s4;
          bChanged:=True;
          Result:=True;
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

function TuibInstScript.EvaluateBoolean (Input : String; var Remaining : String;
     var BooleanResult : Boolean; NestingLevel : Integer; var InfoSyntaxError : String) : Boolean;
begin
  script.FEvalBoolBaseNestLevel:=NestingLevel;
  result := EvaluateBoolean_ (Input, Remaining, BooleanResult, NestingLevel, InfoSyntaxError);
end;

function TuibInstScript.EvaluateBoolean_
     (Input : String;
     var Remaining : String;
     var BooleanResult : Boolean;
     NestingLevel : Integer;
     var InfoSyntaxError : String) : Boolean;
var
  sx : String='';
  r : String='';
  s1 : String='';
  s2 : String='';
  s3 : String='';
  syntaxcheck : Boolean;
  FileRecord : TUnicodeSearchRec;
  RunTimeInfo : String='';
  BooleanResult0 : Boolean;
  Textfile : TPatchList;
  freebytes : Int64=0;
  requiredbytes : Int64=0;
  einheit  :  Int64=0;
  j : integer=0;
  drivenumber : integer=0;
  errnumber : Integer=0;
  relationSymbol : String='';
  intresult : integer=0;
  n1 : integer=0;
  n2 : integer=0;
  comparesign : Integer=0;
  erroroccured : boolean;
  oldDisableWow64FsRedirectionStatus: pointer=nil;
  dummybool : boolean;
  OldWinapiErrorMode: Cardinal;
  list1 : TXStringList;
  InputBakup : string;
  i : integer;
  tmpint : integer;
  tmpbool, tmpbool1, tmpbool2 : boolean;
  tmpstr,tmpstr1,tmpstr2,tmpstr3 : string;
  FindResultcode: integer = 0;
  flushhandle : Thandle;
  int64result : Int64;
  funcname : string;
  funcindex : integer;




begin
 syntaxCheck := false;
 InfoSyntaxError := '';
 RunTimeInfo := '';
 BooleanResult := false;
 list1 := TXStringlist.Create;
 InputBakup := Input;

 LogDatei.log_prog ('EvaluateBoolean: Parsing: '+Input+' ', LLDebug);


 (*
 // defined local function ?
 GetWord (Input, funcname, r, WordDelimiterSet5);
 FuncIndex := definedFunctionNames.IndexOf (LowerCase (funcname));
  if FuncIndex >= 0 then
  begin
    if not (definedFunctionArray[FuncIndex].datatype = dfpBoolean) then
    begin
      // error
      syntaxCheck := false;
      LogDatei.log('Syntax Error: defined function: '+funcname+' is not from type boolean.',LLError);
    end
    else
    begin
      if definedFunctionArray[FuncIndex].call(r) then
      begin
        r := '';
        BooleanResult := definedFunctionArray[FuncIndex].ResultBool;
        syntaxCheck := true;
      end
      else
      begin
        // defined function call failed
        LogDatei.log('Call of defined function: '+funcname+' failed',LLError);
        syntaxCheck := false;
      end;
    end;
  end
  *)


 // geklammerter Boolescher Ausdruck
 if Skip ('(', Input, r, sx)
 then
 begin
   if EvaluateBoolean_ (r, r, BooleanResult, NestingLevel+1, InfoSyntaxError)
   then if Skip (')', r, r, InfoSyntaxError)
   then
      syntaxCheck := true;
 end

 // Boolean function
 else if Skip ('not', Input, r, sx)
 then
 begin
   if Skip ('(', r, r, InfoSyntaxError)
   then if EvaluateBoolean_ (r, r, BooleanResult, NestingLevel+1, InfoSyntaxError)
   then if Skip (')', r, r, InfoSyntaxError)
   then
   Begin
      BooleanResult := not BooleanResult;
      syntaxCheck := true;
   End;
 end

 {$IFDEF WIN32}
 else if Skip ('FileExists64', Input, r, sx)
 then
 begin
   if Skip ('(', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s1, InfoSyntaxError)
   then if Skip (')', r, r, InfoSyntaxError)
   then
   Begin
      LogDatei.log ('  Starting query if file exist (64 Bit mode)...', LLInfo);
      // BooleanResult := CheckFileExists (s1, RunTimeInfo)  or IsDirectory (s1);

      s2 := s1;
      if (length(s1) > 0) and (s1[length(s1)] = PATHSEPARATOR)
        then s2 := copy(s1,1,length(s1)-1);
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
                LogDatei.log ('File: '+s2+' not found via FileExists', LLDebug3);
                (*
                list1.Clear;
                list1.Text := execShellCall('dir '+s2, '64',4, false,false).Text;
                //calling shellCall with FetchExitCodePublic=false result is on FLastPrivateExitCode
                if (0 = FLastPrivateExitCode) then BooleanResult := true;
                *)
              end;
            except
              BooleanResult := false;
            end;
          finally
            setErrorMode(OldWinapiErrorMode);
          end;
          dummybool := DSiRevertWow64FsRedirection(oldDisableWow64FsRedirectionStatus);
          LogDatei.log('RevertWow64FsRedirection succeeded', LLinfo);
        end
        else
        begin
          LogDatei.log('Error: DisableWow64FsRedirection failed', LLError);
          BooleanResult := false;
        end;
      except
        on ex: Exception
        do
        Begin
          LogDatei.log ('Error: ' + ex.message, LLError);
        End;
      end;
      if not BooleanResult then
      Begin
        RunTimeInfo := '  "' + s1 + '": ' + RunTimeInfo;
        LogDatei.log (RunTimeInfo, LLwarning);
      End;
      syntaxCheck := true;
   End;
 end

  else if Skip ('FileExistsSysNative', Input, r, sx)
 then
 begin
   if Skip ('(', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s1, InfoSyntaxError)
   then if Skip (')', r, r, InfoSyntaxError)
   then
   Begin
      if Is64BitSystem then
        LogDatei.log ('  Starting query if file exist (SysNative 64 Bit mode)...', LLInfo)
      else
        LogDatei.log ('  Starting query if file exist (SysNative 32 Bit mode)...', LLInfo);
      s2 := s1;
      if (length(s1) > 0) and (s1[length(s1)] = PATHSEPARATOR)
        then s2 := copy(s1,1,length(s1)-1);
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
                  LogDatei.log ('File: '+s2+' not found via FileExists', LLDebug3);
                  (*
                  list1.Clear;
                  list1.Text := execShellCall('dir '+s2, 'sysnative',4, false,false).Text;
                  //calling shellCall with FetchExitCodePublic=false result is on FLastPrivateExitCode
                  if (0 = FLastPrivateExitCode) then BooleanResult := true;
                  *)
                end;
              except
                BooleanResult := false;
              end;
            finally
              setErrorMode(OldWinapiErrorMode);
            end;
            dummybool := DSiRevertWow64FsRedirection(oldDisableWow64FsRedirectionStatus);
            LogDatei.log('RevertWow64FsRedirection succeeded', LLinfo);
          end
          else
          begin
            LogDatei.log('Error: DisableWow64FsRedirection failed', LLError);
            BooleanResult := false;
          end;
        except
          on ex: Exception
          do
          Begin
            LogDatei.log ('Error: ' + ex.message, LLError);
          End;
        end
      end
      else
      begin
        //BooleanResult := GetFileInfo (s2, FileRecord, RunTimeInfo);
       BooleanResult := FileExists(s2) or DirectoryExists(s2);
        if (not BooleanResult) and (not (trim(s2) = '')) then
        begin
          LogDatei.log ('File: '+s2+' not found via FileExists', LLDebug3);
          (*
          list1.Clear;
          list1.Text := execShellCall('dir '+s2, 'sysnative',4, false,false).Text;
          //calling shellCall with FetchExitCodePublic=false result is on FLastPrivateExitCode
          if (0 = FLastPrivateExitCode) then BooleanResult := true;
          *)
        end;
      end;
      if not BooleanResult then
      Begin
        RunTimeInfo := '  "' + s1 + '": ' + RunTimeInfo;
        LogDatei.log (RunTimeInfo, LLwarning);
      End;
      syntaxCheck := true;
   End;
 end

  else if Skip ('FileExists32', Input, r, sx)
 then
 begin
   if Skip ('(', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s1, InfoSyntaxError)
   then if Skip (')', r, r, InfoSyntaxError)
   then
   Begin
      LogDatei.log ('  Starting query if file exist ...', LLInfo);
      s2 := s1;
      if (length(s1) > 0) and (s1[length(s1)] = PATHSEPARATOR)
       then s2 := copy(s1,1,length(s1)-1);
      OldWinapiErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
      try
        try
          BooleanResult := FileExists(s2) or DirectoryExists(s2);
          if (not BooleanResult) and (not (trim(s2) = '')) then
          begin
            LogDatei.log ('File: '+s2+' not found via FileExists', LLDebug3);
            //FindResultcode := SysUtils.FindFirst(s2,faAnyFile - faDirectory, FileRecord);
            //if FindResultcode = 0 then BooleanResult := true;
            //list1.Clear;
            //list1.Text := execShellCall('dir '+s2, '32',4, false,false).Text;
            ////calling shellCall with FetchExitCodePublic=false result is on FLastPrivateExitCode
            //if (0 = FLastPrivateExitCode) then BooleanResult := true;
          end;
        except
          BooleanResult := false;
        end;
      finally
        setErrorMode(OldWinapiErrorMode);
      end;
      if not BooleanResult then
      Begin
        RunTimeInfo := '  "' + s1 + '": ' + RunTimeInfo;
        LogDatei.log (RunTimeInfo, LevelWarnings);
      End;
      syntaxCheck := true;
   End;
 end
 {$ENDIF WIN32}

 else if Skip ('FileExists', Input, r, sx)
 then
 begin
   if Skip ('(', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s1, InfoSyntaxError)
   then if Skip (')', r, r, InfoSyntaxError)
   then
   Begin
     s1 := ExpandFileName(s1);
      LogDatei.log ('Starting query if file exist ...', LLInfo);
      s2 := s1;
      if (length(s1) > 0) and (s1[length(s1)] = PATHSEPARATOR)
       then s2 := copy(s1,1,length(s1)-1);
      {$IFDEF WINDOWS}
      // disable  critical-error-handler message box. (Drive not ready)
      OldWinapiErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
      try
        try
          (*
          try
            flushhandle := FileOpen(ExtractFileDrive(s1),fmOpenRead);
            FlushFileBuffers(flushhandle);
          except
            LogDatei.log ('Exception: while flushfilebuffers', LLDebug3);
          end;
          *)
          (*
          // wait for getting the cache ready
          ProcessMess;
          *)
          s2 := trim(s2);
          BooleanResult := FileExists(s2) or DirectoryExists(s2);
          if (not BooleanResult) and (not (trim(s2) = '')) then
          begin
            LogDatei.log ('File: '+s2+' not found via FileExists', LLDebug3);
            (*
            // search for s2*
            LogDatei.log ('Looking for: '+ExtractFilePath(s2)+'*'+' via FindFirst', LLDebug3);
            FindResultcode := FindFirst(ExtractFilePath(s2)+'*',faAnyFile or faSymlink, FileRecord);
            while FindResultcode = 0 do
            begin
              LogDatei.log ('found: '+ExtractFilePath(s2)+ FileRecord.Name+' via FindFirst/Next', LLDebug3);
              if ExtractFilePath(s2)+ FileRecord.Name = s2 then
              begin
                BooleanResult := true;
                LogDatei.log ('File: '+s2+' found via FindFirst/Next', LLDebug3);
              end;
              FindResultcode := sysutils.FindNext(FileRecord);
            end;
            SysUtils.findclose(FileRecord);
            if not BooleanResult then LogDatei.log ('File: '+s2+' not found via FindFirst/next', LLDebug3);
            list1.Clear;
            list1.Text := execShellCall('dir '+s2, '32',4, false,false).Text;
            //calling shellCall with FetchExitCodePublic=false result is on FLastPrivateExitCode
            if (0 = FLastPrivateExitCode) then BooleanResult := true;
            LogDatei.log ('Looking for: '+ExtractFilePath(s2)+'*'+' via FindFirst', LLDebug3);
            FindResultcode := FindFirst(ExtractFilePath(s2)+'*',faAnyFile or faSymlink, FileRecord);
            while FindResultcode = 0 do
            begin
              LogDatei.log ('found: '+ExtractFilePath(s2)+ FileRecord.Name+' via FindFirst/Next', LLDebug3);
              if ExtractFilePath(s2)+ FileRecord.Name = s2 then
              begin
                BooleanResult := true;
                LogDatei.log ('File: '+s2+' found via FindFirst/Next', LLDebug3);
              end;
              FindResultcode := sysutils.FindNext(FileRecord);
            end;
            SysUtils.findclose(FileRecord);
            if not BooleanResult then LogDatei.log ('File: '+s2+' not found via FindFirst/next', LLDebug3);
            *)
            (*
            if FindResultcode = 0 then
            begin
              BooleanResult := true;
              LogDatei.log ('File: '+s2+' found via FindFirst', LLDebug3);
            end
            else LogDatei.log ('File: '+s2+' not found via FindFirst', LLDebug3);
            list1.Clear;
            list1.Text := execShellCall('dir '+s2, '32',4, false,false).Text;
            //calling shellCall with FetchExitCodePublic=false result is on FLastPrivateExitCode
            if (0 = FLastPrivateExitCode) then BooleanResult := true;
            *)
          end;
        except
          BooleanResult := false;
        end;
      finally
        setErrorMode(OldWinapiErrorMode);
      end;
      {$ELSE WINDOWS}
      BooleanResult := FileExists(s2) or DirectoryExists(s2);
      if (not BooleanResult) and (not (trim(s2) = '')) then
      begin
        list1.Clear;
        list1.Text := execShellCall('ls '+s2, 'sysnative',4, false,false).Text;
        //calling shellCall with FetchExitCodePublic=false result is on FLastPrivateExitCode
        if (0 = FLastPrivateExitCode) then BooleanResult := true;
      end;
      {$ENDIF WINDOWS}
      if not BooleanResult then
      Begin
        RunTimeInfo := '  "' + s1 + '": ' + RunTimeInfo;
        LogDatei.log (RunTimeInfo, LevelWarnings);
      End;
      syntaxCheck := true;
   End;
 end


 {$IFDEF WINDOWS}

 // XMLAddNamespace(Datei:string,ElementName:string,Namespace:string):Boolean
 // True, if Namespace was not present and have to be inserted
 else if Skip ('XMLAddNamespace', Input, r, sx)
 then
 begin
    if Skip ('(', r, r, InfoSyntaxError)
    then if EvaluateString (r, r, s1, InfoSyntaxError)
    then if Skip (',', r, r, InfoSyntaxError)
    then if EvaluateString (r, r, s2, InfoSyntaxError)
    then if Skip (',', r, r, InfoSyntaxError)
    then if EvaluateString (r, r, s3, InfoSyntaxError)
    then if Skip (')', r, r, InfoSyntaxError)
    then
    Begin
      syntaxCheck := true;
      LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

      LogDatei.log (' Adding NameSpace (if not existing) "' + s3 + '" for element "' + s2 + '" in file "' + s1 + '"', LevelComplete);

      BooleanResult:=false;
      try
        BooleanResult:=doXMLAddNamespace(s1,s2,s3);
      except
        on ex: Exception
        do
        Begin
          LogDatei.log ('Error: ' + ex.message, LLError);
        End;
      end;

      LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
    End;
 end

  //XMLRemoveNamespace(Datei:string,ElementName:string,Namespace:string):Boolean
  //  True, if the  Namespace was successful removed
 else if Skip ('XMLRemoveNamespace', Input, r, sx)
 then
 begin
    if Skip ('(', r, r, InfoSyntaxError)
    then if EvaluateString (r, r, s1, InfoSyntaxError)
    then if Skip (',', r, r, InfoSyntaxError)
    then if EvaluateString (r, r, s2, InfoSyntaxError)
    then if Skip (',', r, r, InfoSyntaxError)
    then if EvaluateString (r, r, s3, InfoSyntaxError)
    then if Skip (')', r, r, InfoSyntaxError)
    then
    Begin
      syntaxCheck := true;
      LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
      LogDatei.log
       (' Removing NameSpace "' + s3 + '" for element "' + s2 + '" in file "' + s1 + '"',
       LevelComplete);

      BooleanResult:=false;
      try
        BooleanResult:=doXMLRemoveNamespace(s1,s2,s3);
      except
        on ex: Exception
        do
        Begin
          LogDatei.log ('Error: ' + ex.message, LLError);
        End;
      end;
      LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
    End;
 end
 {$ENDIF WINDOWS}

 else if Skip ('LineBeginning_ExistsIn', Input, r, sx)
 then
 begin
   if Skip ('(', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s1, InfoSyntaxError)
   then if Skip (',', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s2, InfoSyntaxError)
   then if Skip (')', r, r, InfoSyntaxError)
   then
   Begin
     syntaxCheck := true;
     LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
     LogDatei.log ('Searching "' + s1 + '" in File "' + s2 + '"', LevelComplete);
     try
       Textfile := TPatchList.Create;
       Textfile.clear;
       Textfile.ItemPointer := -1;
       s2 := ExpandFileName(s2);
       Textfile.LoadFromFile (s2);
       Textfile.Text:= reencode(Textfile.Text, 'system');
       BooleanResult := ( Textfile.FindFirstItemStartingWith(s1, false, -1) >= 0 )
     except
       on ex: Exception
       do
       Begin
         LogDatei.log ('Error: ' + ex.message, LLError);
       End;
     end;

     LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
   End
 End

 else if Skip ('LineExistsIn', Input, r, sx)
 then
 begin
   if Skip ('(', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s1, InfoSyntaxError)
   then if Skip (',', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s2, InfoSyntaxError)
   then if Skip (')', r, r, InfoSyntaxError)
   then
   Begin
     syntaxCheck := true;

     LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
     LogDatei.log ('Searching "' + s1 + '" in File "' + s2 + '"', LevelComplete);
     try
       Textfile := TPatchList.Create;
       Textfile.clear;
       Textfile.ItemPointer := -1;
       s2 := ExpandFileName(s2);
       Textfile.LoadFromFile (s2);
       Textfile.Text:= reencode(Textfile.Text, 'system');

       BooleanResult := (Textfile.FindFirstItem(s1, false, -1, BooleanResult0) >= 0 )
     except
       on ex: Exception
       do
       Begin
         LogDatei.log ('Error: ' + ex.message, LLError);
       End;
     end;

     LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
   End
 End

  else if Skip ('LineContaining_ExistsIn', Input, r, sx)
 then
 begin
   if Skip ('(', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s1, InfoSyntaxError)
   then if Skip (',', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s2, InfoSyntaxError)
   then if Skip (')', r, r, InfoSyntaxError)
   then
   Begin
     syntaxCheck := true;

     LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
     LogDatei.log ('Searching "' + s1 + '" in File "' + s2 + '"', LevelComplete);
     try
       Textfile := TPatchList.Create;
       Textfile.clear;
       Textfile.ItemPointer := -1;
       s2 := ExpandFileName(s2);
       Textfile.LoadFromFile (s2);
       Textfile.Text:= reencode(Textfile.Text, 'system');

       BooleanResult := (Textfile.FindFirstItemWith(s1, false, -1) >= 0 )
     except
       on ex: Exception
       do
       Begin
         LogDatei.log ('Error: ' + ex.message, LLError);
       End;
     end;

     LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
   End
 End

 else if Skip ('contains', Input, r, sx)
 then
 begin
   if Skip ('(', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s1, InfoSyntaxError)
   then if Skip (',', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s2, InfoSyntaxError)
   then if Skip (')', r, r, InfoSyntaxError)
   then
   Begin
     syntaxCheck := true;
     BooleanResult := strContains(s1,s2);
   end;
 End

 else if Skip ('isRegexMatch', Input, r, sx)
 then
 begin
   if Skip ('(', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s1, InfoSyntaxError)
   then if Skip (',', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s2, InfoSyntaxError)
   then if Skip (')', r, r, InfoSyntaxError)
   then
   Begin
     syntaxCheck := true;
     BooleanResult := isRegexMatch(s1,s2);
   end;
 End

 else if Skip ('isValidIP4Network', Input, r, sx)
 then
 begin
   if Skip ('(', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s1, InfoSyntaxError)
   then if Skip (',', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s2, InfoSyntaxError)
   then if Skip (')', r, r, InfoSyntaxError)
   then
   Begin
     syntaxCheck := true;
     if isip(s1) then
       BooleanResult := isValidIP4Network(s1,s2)
     else
     begin
       BooleanResult := false;
       Logdatei.log('Error: '+s1+' is not a valid IPv4 Address', LLerror);
     end;
   end;
 End


 else if Skip ('isValidIP4Host', Input, r, sx)
 then
 begin
   if Skip ('(', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s1, InfoSyntaxError)
   then if Skip (',', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s2, InfoSyntaxError)
   then if Skip (')', r, r, InfoSyntaxError)
   then
   Begin
     syntaxCheck := true;
     if isip(s1) then
       BooleanResult := isValidIP4Host(s1,s2)
     else
     begin
       BooleanResult := false;
       Logdatei.log('Error: '+s1+' or '+s2+' is not a valid IPv4 Address', LLerror);
     end;
   end;
 End

 else if Skip ('waitForPackageLock', Input, r, sx)
 then
 begin
   if Skip ('(', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s1, InfoSyntaxError)
   then if Skip (',', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s2, InfoSyntaxError)
   then if Skip (')', r, r, InfoSyntaxError)
   then
   Begin
     syntaxCheck := true;
     {$IFDEF LINUX}
     BooleanResult := false;
     try
       tmpint := StrToInt(s1);
       tmpbool := StrToBool(s2);
       BooleanResult := getPackageLock(tmpint, tmpbool);
     except
       on ex: Exception
       do
       Begin
         LogDatei.log('Error osparser at waitForPackageLock: with: ' + s1+','+s2, LLError);
         LogDatei.log('Exception in waitForPackageLock: Error: ' + ex.message, LLError);
       end;
     end;
     {$ELSE LINUX}
     LogDatei.log('Error waitForPackageLock: not implemented for Windows; result=true: ', LLError);
     BooleanResult := true;
     {$ENDIF LINUX}
   end;
 End


 else if Skip ('sendlog', Input, r, sx)
 then
 begin
   if Skip ('(', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s1, InfoSyntaxError)
   then if Skip (',', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s2, InfoSyntaxError)
   then if Skip (',', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s3, InfoSyntaxError)
   then if Skip (')', r, r, InfoSyntaxError)
   then
   Begin
     syntaxCheck := true;
     if local_opsidata <> nil then
     begin
       logdatei.Appendmode := True; // switch append mode on in order to avoid that a final log removes this one
       local_opsidata.setActualClient(s1);
       if 'true' = LowerCase(s3) then
         BooleanResult := local_opsidata.sendLog(s2,true)
       else
         BooleanResult := local_opsidata.sendlog(s2,false);
     end
     else
     begin
       LogDatei.log('Service not initialized',LLError);
     end;
   end;
 End

 else if Skip ('savetextfilewithencoding', Input, r, sx)
 then
 begin
   try
     BooleanResult := false;
     list1 := TXStringList.create;
     if Skip ('(', r, r, InfoSyntaxError)
     then if produceStringList (script, r, r, list1, InfoSyntaxError)
     then if Skip (',', r, r, InfoSyntaxError)
     then if EvaluateString (r, r, s1, InfoSyntaxError)
     then if Skip (',', r, r, InfoSyntaxError)
     then if EvaluateString (r, r, s2, InfoSyntaxError)
     then if Skip (')', r, r, InfoSyntaxError)
     then
     Begin
       syntaxCheck := true;
       try
          s1 := ExpandFileName(s1);
          BooleanResult := list1.FuncSaveToFile(s1,s2);
       except
         logdatei.log('Error: Could not write list to filename: '+s1,LLError);
       end;
     end;
   finally
     list1.free;
     list1 := nil;
   end
 End



 else if Skip ('savetextfile', Input, r, sx)
 then
 begin
   try
     BooleanResult := false;
     list1 := TXStringList.create;
     if Skip ('(', r, r, InfoSyntaxError)
     then if produceStringList (script, r, r, list1, InfoSyntaxError)
     then if Skip (',', r, r, InfoSyntaxError)
     then if EvaluateString (r, r, s1, InfoSyntaxError)
     then if Skip (')', r, r, InfoSyntaxError)
     then
     Begin
       syntaxCheck := true;
       try
          s1 := ExpandFileName(s1);
          BooleanResult := list1.FuncSaveToFile(s1);
       except
         logdatei.log('Error: Could not write list to filename: '+s1,LLError);
       end;
     end;
   finally
     list1.free;
     list1 := nil;
   end
 End


 else if Skip ('isNumber', Input, r, InfoSyntaxError)
 then
 begin
    if Skip ('(', r, r, InfoSyntaxError)
    then if EvaluateString (r, r, s1, InfoSyntaxError)
    then if Skip (')', r, r, InfoSyntaxError)
    then
    Begin
      syntaxCheck := true;
      try
        BooleanResult := TryStrToInt64(s1,int64result);
      except
        BooleanResult := false;
      end
    end;
 end


 else if Skip ('isValidIP4', Input, r, InfoSyntaxError)
 then
 begin
    if Skip ('(', r, r, InfoSyntaxError)
    then if EvaluateString (r, r, s1, InfoSyntaxError)
    then if Skip (')', r, r, InfoSyntaxError)
    then
    Begin
      syntaxCheck := true;
      try
        BooleanResult := isValidIP4(s1);
      except
        BooleanResult := false;
      end
    end;
 end


 else if Skip ('isConfidential', Input, r, InfoSyntaxError)
 then
 begin
    if Skip ('(', r, r, InfoSyntaxError)
    then if EvaluateString (r, r, s1, InfoSyntaxError)
    then if Skip (')', r, r, InfoSyntaxError)
    then
    Begin
      syntaxCheck := true;
      try
        BooleanResult := logdatei.isConfidential(s1);
      except
        BooleanResult := false;
      end
    end;
 end

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


 else if Skip ('processIsRunning', Input, r, InfoSyntaxError)
 then
 begin
    if Skip ('(', r, r, InfoSyntaxError)
    then if EvaluateString (r, r, s1, InfoSyntaxError)
    then if Skip (')', r, r, InfoSyntaxError)
    then
    Begin
      syntaxCheck := true;
      BooleanResult := false;
      try
        list1.Text := getProcesslist.Text;
        for i := 0 to list1.Count-1 do
        begin
         if strContains(list1.Strings[i],s1) then BooleanResult := true;
        end;
      except
        logdatei.log('Error: Exception in processIsRunning:  '+s1,LLError);
        BooleanResult := false;
      end
    end;
 end


 else if Skip ('jsonIsValid', Input, r, InfoSyntaxError)
 then
 begin
    if Skip ('(', r, r, InfoSyntaxError)
    then if EvaluateString (r, r, s1, InfoSyntaxError)
    then if Skip (')', r, r, InfoSyntaxError)
    then
    Begin
      syntaxCheck := true;
      try
        BooleanResult := jsonIsValid (s1);
      except
        BooleanResult := false;
        logdatei.log('Error: Exception at jsonIsValid with : "'+s1+'"',LLError);
      end
    end;
 end


 else if Skip ('jsonIsArray', Input, r, InfoSyntaxError)
 then
 begin
    if Skip ('(', r, r, InfoSyntaxError)
    then if EvaluateString (r, r, s1, InfoSyntaxError)
    then if Skip (')', r, r, InfoSyntaxError)
    then
    Begin
      syntaxCheck := true;
      try
        BooleanResult := jsonIsArray (s1);
      except
        BooleanResult := false;
        logdatei.log('Error: Exception at jsonIsArray with : "'+s1+'"',LLError);
      end
    end;
 end

 else if Skip ('jsonIsObject', Input, r, InfoSyntaxError)
 then
 begin
    if Skip ('(', r, r, InfoSyntaxError)
    then if EvaluateString (r, r, s1, InfoSyntaxError)
    then if Skip (')', r, r, InfoSyntaxError)
    then
    Begin
      syntaxCheck := true;
      try
        BooleanResult := jsonIsObject (s1);
      except
        BooleanResult := false;
        logdatei.log('Error: Exception at jsonIsObject with : "'+s1+'"',LLError);
      end
    end;
 end

 else if Skip ('jsonAsObjectHasKey', Input, r, sx)
 then
 begin
   if Skip ('(', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s1, InfoSyntaxError)
   then if Skip (',', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s2, InfoSyntaxError)
   then if Skip (')', r, r, InfoSyntaxError)
   then
   Begin
     try
       syntaxCheck := true;
       BooleanResult := jsonAsObjectHasKey(s1,s2);
     except
        BooleanResult := false;
        logdatei.log('Error: Exception at jsonAsObjectHasKey with : "'+s1+'","'+s2+'"',LLError);
      end
   end;
 End

 else if Skip ('stringToBool', Input, r, InfoSyntaxError)
 then
 begin
    if Skip ('(', r, r, InfoSyntaxError)
    then if EvaluateString (r, r, s1, InfoSyntaxError)
    then if Skip (')', r, r, InfoSyntaxError)
    then
    Begin
      syntaxCheck := true;
      try
        BooleanResult := StrToBool (s1);
      except
        BooleanResult := false;
        logdatei.log('Error: stringToBool: given string expression is not a boolean value: "'+s1+' Defaulting to false',LLError);
      end
    end;
 end
(*
 else  if AnsiStartsText('CompareDotSeparatedNumbers', trim(Input))
       and AnsiContainsText(Input,'(')
       and (NPos(',',Input,2) > 0)
       and (NPos(',',Input,2) < AnsiPos(')',Input))
       and AnsiContainsText(Input,')')
*)
else if Skip ('CompareDotSeparatedNumbers', InputBakup, r, InfoSyntaxError)
     and Skip ('(', r, r, InfoSyntaxError)
     and EvaluateString (r, r, s1, InfoSyntaxError)
     and Skip (',', r, r, InfoSyntaxError)
     and EvaluateString (r, r, s1, InfoSyntaxError)
     and Skip (',', r, r, InfoSyntaxError)
     and EvaluateString (r, r, s1, InfoSyntaxError)
     and Skip (')', r, r, InfoSyntaxError)
 then
 begin
   LogDatei.log ('The String: '+Input+' looks like the boolean CompareDotSeparatedNumbers function', LLDebug3);
   if Skip ('CompareDotSeparatedNumbers', Input, r, InfoSyntaxError) then
   begin
     try
      if Skip ('(', r, r, InfoSyntaxError)
      then if EvaluateString (r, r, s1, InfoSyntaxError)
      then if Skip (',', r, r, InfoSyntaxError)
      then if EvaluateString (r, r, s2, InfoSyntaxError)
      then if Skip (',', r, r, InfoSyntaxError)
      then if EvaluateString (r, r, s3, InfoSyntaxError)
      then if Skip (')', r, r, InfoSyntaxError)
      then
      Begin
        syntaxCheck := true;
        s2 := trim(s2);
        LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
        LogDatei.log ('Checking if "' + s1 + '" is "' + s2 + '" than / as "' + s3 + '"', LLDebug);
        BooleanResult:=false;
        if getDecimalCompareSign(s1, s3, intresult, sx, false) then
        begin
          if s2 = '<' then
          begin
            case intresult of
              -1 : BooleanResult:=true;   // s1 < s3
               0 : BooleanResult:=false;   // s1 = s3
               1 : BooleanResult:=false;   // s1 > s3
            end;
          end
          else if (s2 = '<=') or (s2 = '=<') then
          begin
            case intresult of
             -1 : BooleanResult:=true;   // s1 < s3
              0 : BooleanResult:=true;   // s1 = s3
              1 : BooleanResult:=false;   // s1 > s3
            end;
          end
          else if s2 = '=' then
          begin
            case intresult of
             -1 : BooleanResult:=false;   // s1 < s3
              0 : BooleanResult:=true;   // s1 = s3
              1 : BooleanResult:=false;   // s1 > s3
            end;
          end
          else if (s2 = '>=') or (s2 = '=>') then
          begin
            case intresult of
             -1 : BooleanResult:=false;   // s1 < s3
              0 : BooleanResult:=true;   // s1 = s3
              1 : BooleanResult:=true;   // s1 > s3
            end;
          end
          else if s2 = '>' then
          begin
            case intresult of
             -1 : BooleanResult:=false;   // s1 < s3
              0 : BooleanResult:=false;   // s1 = s3
              1 : BooleanResult:=true;   // s1 > s3
            end;
          end
          else
          begin
            syntaxCheck := false;
            LogDatei.log('Error: relation operator [<,<=,=,>=,>] expected but we got :'+s2, LLError);
            InfoSyntaxError := 'Error: relation operator [<,<=,=,>=,>] expected but we got :'+s2;
          end
        end
        else
        begin
          //syntaxCheck := false;
          LogDatei.log('Error: ' + sx, LLError);
          LogDatei.log('Error: CompareDotSeparatedNumbers: using default result=false', LLError);
          BooleanResult:=false;
        end;
       end;
      except
        on ex: Exception
        do
        Begin
          LogDatei.log ('Error: Exception in CompareDotSeparatedNumbers (bool) ' + ex.message, LLError);
        End;
      end;
      LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
   end
end

else if Skip ('CompareDotSeparatedStrings', InputBakup, r, InfoSyntaxError)
     and Skip ('(', r, r, InfoSyntaxError)
     and EvaluateString (r, r, s1, InfoSyntaxError)
     and Skip (',', r, r, InfoSyntaxError)
     and EvaluateString (r, r, s1, InfoSyntaxError)
     and Skip (',', r, r, InfoSyntaxError)
     and EvaluateString (r, r, s1, InfoSyntaxError)
     and Skip (')', r, r, InfoSyntaxError)
 then
 begin
   LogDatei.log ('The String: '+Input+' looks like the boolean CompareDotSeparatedStrings function', LLDebug3);
   if Skip ('CompareDotSeparatedStrings', Input, r, InfoSyntaxError) then
   begin
     try
      if Skip ('(', r, r, InfoSyntaxError)
      then if EvaluateString (r, r, s1, InfoSyntaxError)
      then if Skip (',', r, r, InfoSyntaxError)
      then if EvaluateString (r, r, s2, InfoSyntaxError)
      then if Skip (',', r, r, InfoSyntaxError)
      then if EvaluateString (r, r, s3, InfoSyntaxError)
      then if Skip (')', r, r, InfoSyntaxError)
      then
      Begin
        syntaxCheck := true;
        s2 := trim(s2);
        LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
        LogDatei.log ('Checking if "' + s1 + '" is "' + s2 + '" than / as "' + s3 + '"', LLDebug);
        BooleanResult:=false;
        if getDecimalCompareSign(s1, s3, intresult, sx, true) then
        begin
          if s2 = '<' then
          begin
            case intresult of
              -1 : BooleanResult:=true;   // s1 < s3
               0 : BooleanResult:=false;   // s1 = s3
               1 : BooleanResult:=false;   // s1 > s3
            end;
          end
          else if (s2 = '<=') or (s2 = '=<') then
          begin
            case intresult of
             -1 : BooleanResult:=true;   // s1 < s3
              0 : BooleanResult:=true;   // s1 = s3
              1 : BooleanResult:=false;   // s1 > s3
            end;
          end
          else if s2 = '=' then
          begin
            case intresult of
             -1 : BooleanResult:=false;   // s1 < s3
              0 : BooleanResult:=true;   // s1 = s3
              1 : BooleanResult:=false;   // s1 > s3
            end;
          end
          else if (s2 = '>=') or (s2 = '=>') then
          begin
            case intresult of
             -1 : BooleanResult:=false;   // s1 < s3
              0 : BooleanResult:=true;   // s1 = s3
              1 : BooleanResult:=true;   // s1 > s3
            end;
          end
          else if s2 = '>' then
          begin
            case intresult of
             -1 : BooleanResult:=false;   // s1 < s3
              0 : BooleanResult:=false;   // s1 = s3
              1 : BooleanResult:=true;   // s1 > s3
            end;
          end
          else
          begin
            syntaxCheck := false;
            LogDatei.log('Error: relation operator [<,<=,=,>=,>] expected but we got :'+s2, LLError);
            InfoSyntaxError := 'Error: relation operator [<,<=,=,>=,>] expected but we got :'+s2;
          end
        end
        else
        begin
          syntaxCheck := false;
          LogDatei.log('Error: ' + sx, LLError);
        end;
       end;
      except
        on ex: Exception
        do
        Begin
          LogDatei.log ('Error: Exception in CompareDotSeparatedStrings (bool) ' + ex.message, LLError);
        End;
      end;
      LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
   end
end


 {$IFDEF WINDOWS}
 else if Skip ('IsDriveReady', Input, r, InfoSyntaxError)
 then
 begin
    if Skip ('(', r, r, InfoSyntaxError)
    then if EvaluateString (r, r, s1, InfoSyntaxError)
    then if Skip (')', r, r, InfoSyntaxError)
    then
    Begin
      syntaxCheck := true;
      try
        BooleanResult := IsDriveReady (s1);
      except
        BooleanResult := false;
      end
    end;
 end

 else if Skip ('RegKeyExists', Input, r, InfoSyntaxError)
 then
 begin
    s2 := '';
    tmpstr2 := '';
    if Skip ('(', r, r, InfoSyntaxError)
    then if EvaluateString (r, tmpstr, s1, InfoSyntaxError)
    // next after , or )
    //then GetWord(tmpstr,s2,r,WordDelimiterSet6);
    then if Skip (',', tmpstr, tmpstr1, tmpstr3) then
      if EvaluateString (tmpstr1, tmpstr2, s2, tmpstr3) then;
    if s2 = '' then
    begin
      // only one parameter
      if Skip (')', tmpstr, r, InfoSyntaxError) then
      Begin
        syntaxCheck := true;
        try
          BooleanResult := RegKeyExists(s1,true);
        except
          BooleanResult := false;
        end
      end;
    end
    else
    begin
      if Skip (')', tmpstr2, r, InfoSyntaxError) then
      Begin
        syntaxCheck := true;
        try
          tmpbool := true;
          if lowercase(s2) = '32bit' then tmpbool := false
          else if lowercase(s2) = '64bit' then tmpbool := true
          else if lowercase(s2) = 'sysnative' then tmpbool := true
          else Logdatei.log('Error: unknown modifier: '+s2+' expected one of 32bit,64bit,sysnative - fall back to sysnative',LLError);
          BooleanResult := RegKeyExists(s1,tmpbool);
        except
          BooleanResult := false;
        end
      end;
    end;
 end

 else if Skip ('RegVarExists', Input, r, InfoSyntaxError)
 then
 begin
    s2 := '';
    tmpstr2 := '';
    if Skip ('(', r, r, InfoSyntaxError)
    then if EvaluateString (r, r, s1, InfoSyntaxError)
    then if Skip (',', r, r, InfoSyntaxError)
    then if EvaluateString (r, tmpstr, s2, InfoSyntaxError)
    // next after , or )
    //then GetWord(tmpstr,s2,r,WordDelimiterSet6);
    then if Skip (',', tmpstr, tmpstr1, tmpstr3) then
      if EvaluateString (tmpstr1, tmpstr2, s3, tmpstr3) then;
    if s3 = '' then
    begin
      // only one parameter
      if Skip (')', tmpstr, r, InfoSyntaxError) then
      Begin
        syntaxCheck := true;
        try
          BooleanResult := RegVarExists(s1,s2,true);
        except
          BooleanResult := false;
        end
      end;
    end
    else
    begin
      if Skip (')', tmpstr2, r, InfoSyntaxError) then
      Begin
        syntaxCheck := true;
        try
          tmpbool := true;
          if lowercase(s3) = '32bit' then tmpbool := false
          else if lowercase(s3) = '64bit' then tmpbool := true
          else if lowercase(s3) = 'sysnative' then tmpbool := true
          else Logdatei.log('Error: unknown modifier: '+s3+' expected one of 32bit,64bit,sysnative - fall back to sysnative',LLError);
          BooleanResult := RegVarExists(s1,s2,tmpbool);
        except
          BooleanResult := false;
        end
      end;
    end;
 end


 {$ENDIF WINDOWS}

 else if (Skip ('ErrorsOccuredSinceMark ', Input, r, sx) or Skip ('ErrorsOccurredSinceMark ', Input, r, sx))
 then
 begin
   getword (r, relationSymbol, r, WordDelimiterWhiteSpace);
   try
     errNumber := strtoint (r);
     syntaxcheck := true;
     r := '';
   except
     InfoSyntaxError := r + ' is not a number'
   end;

   syntaxCheck :=
     syntaxcheck and
     hasRelation ( relationSymbol ,
                   getCompareSign (Logdatei.NumberOfErrors - Logdatei.ErrorNumberMarked, errNumber),
                   BooleanResult, InfoSyntaxError );
   LogDatei.log('(TotalErrors: '
                  +intToStr(Logdatei.NumberOfErrors)
                  +' - ErrorMark: '+intToStr(Logdatei.ErrorNumberMarked)
                  +') '+relationSymbol+' '
                  +intToStr(errNumber)+' -> '+BoolToStr(BooleanResult),LLDebug2);
 End

 else if Skip ('opsiLicenseManagementEnabled', Input, r, InfoSyntaxError)
 then
 begin
    booleanResult := false;
    Syntaxcheck := true;
    errorOccured := false;
    if opsidata = nil
    then
    Begin
      errorOccured := true;
    End
    else
    begin
      try
        local_opsidata := opsidata;

        //LogDatei.log('Calling opsi service at ' + local_opsidata.serviceUrl, LevelComplete);
      except
        errorOccured := true;
      end;
    end;

    if not errorOccured
    then
    begin
      booleanresult := local_opsidata.withLicenceManagement;
    end;
 end

 else if Skip ('runningAsAdmin', Input, r, InfoSyntaxError)
 then
 begin
    Syntaxcheck := true;
    errorOccured := false;
    booleanresult := runningasadmin;
 end

 else if Skip ('isLoginScript', Input, r, InfoSyntaxError)
 then
 begin
    Syntaxcheck := true;
    errorOccured := false;
    booleanresult := runloginscripts;
 end

 else if Skip ('runningOnUefi', Input, r, InfoSyntaxError)
 then
 begin
    Syntaxcheck := true;
    errorOccured := false;
    booleanresult := isUefi;
 end

  else if Skip ('runningInPE', Input, r, InfoSyntaxError)
 then
 begin
    Syntaxcheck := true;
    errorOccured := false;
    booleanresult := isWinPE;
 end


 else if Skip ('scriptWasExecutedBefore', Input, r, InfoSyntaxError)
 then
 begin
    Syntaxcheck := true;
    errorOccured := false;
    booleanresult := scriptWasExecutedBefore;
 end



 else if Skip ('HasMinimumSpace', Input, r, sx)
 then
 begin
   if Skip ('(', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s1, InfoSyntaxError)
   then if Skip (',', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s2, InfoSyntaxError)
   then if Skip (')', r, r, InfoSyntaxError)
   then
   Begin
     syntaxCheck := true;

     LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

     einheit := 1;
     j := pos ('kb', lowercase(s2));
     if j > 0
     then
       einheit := 1000
     else
     Begin
        j := pos ('mb', lowercase(s2));
        if j > 0
        then
           einheit := 1000 * 1000
        else
        Begin
          j := pos ('gb', lowercase(s2));
          if j > 0
          then
             einheit := 1000 * 1000 * 1000;
        End
     End;


     if j = 0
     then
       try
         freebytes := strtoint64 (s2);
       except
         RunTimeInfo := '"' + s2 + '" is not a valid number"';
         syntaxCheck := false;
       end
     else
       try
         sx := cutRightBlanks(copy (s2, 1, j - 1));
         requiredbytes := strtoint64 (sx);
         requiredbytes := requiredbytes * einheit;
       except
         RunTimeInfo := '"' + s2 + '" is not a valid number"';
         syntaxCheck := false;
       end;

     if syntaxCheck
     then // parse s1 in order to get the drive char
     Begin
       if (length (s1) = 2) and (s1[2] = ':')
       then
         drivenumber := ord (uppercase(s1)[1]) - ord ('A') + 1
       else
       begin
         syntaxcheck := false;
         RunTimeInfo := '"' + s1 + '" is not a valid drive"';
       end;
     end;

     if syntaxCheck
     then
       freebytes := diskfree (drivenumber);

     if syntaxCheck and (freebytes = -1)
     then
     Begin
       syntaxCheck := false;
       RunTimeInfo := '"' + s1 + '" is not a valid drive"';
     End;

     if syntaxCheck
     then
     Begin
       BooleanResult := (freebytes >= requiredBytes);
       RunTimeInfo := 'Free on Disk ' + s1 + ': ' + formatInt (freebytes) + ' bytes';
       if BooleanResult
       then
         RunTimeInfo := RunTimeInfo + '  This is more '
       else
         RunTimeInfo := RunTimeInfo + '  This is less ';

       RunTimeInfo := RunTimeInfo + 'than the required amount of ' + formatInt (requiredbytes) + ' bytes';
     End;

     LogDatei.log (RunTimeInfo, LevelComplete);

     LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
   End
 End

 (* Boolescher Ausdruck  s1 = s2 *)
 else if EvaluateString (Input, r, s1, InfoSyntaxError)
 then
 Begin
   if skip('int', r, r, InfoSyntaxError)
   then
   Begin
     if getOrdersymbol (r, relationsymbol, r, InfoSyntaxError)
     then if EvaluateString (r, r, s2, InfoSyntaxError)
     then
     Begin
       syntaxCheck := true;
       OldNumberOfErrors := LogDatei.NumberOfErrors;

       try
          n1 := StrToInt(s1);
          try
            n2 := StrToInt(s2);
            comparesign := getCompareSign(n1, n2);
            syntaxCheck := hasRelation(
              relationsymbol,
              comparesign,
              BooleanResult, InfoSyntaxError);
          except
            LogDatei.log('Error: ' + s2 + ' has no Integer format', LLError);
          end;
       except
          LogDatei.log('Error: ' + s1 + ' has no Integer format', LLError)
       end;

       DiffNumberOfErrors := LogDatei.NumberOfErrors - OldNumberOfErrors;
       FNumberOfErrors := NumberOfErrors + DiffNumberOfErrors;

     End
   end
   else
   Begin
     if getOrdersymbol (r, relationsymbol, r, InfoSyntaxError)
     then if EvaluateString (r, r, s2, InfoSyntaxError)
     then
     Begin
       comparesign := getCompareSignStrings(s1, s2);
       syntaxCheck := hasRelation(
          relationsymbol,
          comparesign,
          BooleanResult, InfoSyntaxError);
       syntaxCheck := true;
     End
   End

   {
   if Skip ('=', r, r, InfoSyntaxError)
   then if EvaluateString (r, r, s2, InfoSyntaxError)
   then
   Begin
     BooleanResult := (AnsiUpperCase (s1) = AnsiUpperCase (s2));
     syntaxCheck := true;
   End
   }
 End

 else
   if InfoSyntaxError = '' then InfoSyntaxError := 'not a well formed boolean Expressionstr';


 (*  concatenate  AND / OR  *)
 if syntaxCheck then
 Begin
   if Skip ('and', r, r, sx)
   then
   Begin
     if EvaluateBoolean_ (r, r, BooleanResult0, NestingLevel+1, InfoSyntaxError)
     then
       BooleanResult := BooleanResult and BooleanResult0
     else
       syntaxCheck := false;
   End
   else
   if Skip ('or', r, r, sx)
   then
   Begin
     if EvaluateBoolean_ (r, r, BooleanResult0, NestingLevel+1, InfoSyntaxError)
     then
       BooleanResult := BooleanResult or BooleanResult0
     else
       syntaxCheck := false;
   End
 End;

 result := syntaxCheck;
 Remaining := r;

 RunTimeInfo := copy (Input, 1, length (Input) - length (Remaining));

 if result
 then
 Begin
   if BooleanResult
   then
     RunTimeInfo := RunTimeInfo + '   <<< result true'
   else
     RunTimeInfo := RunTimeInfo + '   <<< result false'
 End
 else
   RunTimeInfo := RunTimeInfo +   '    <<< syntax error, no result!! - set to false';
 //LogDatei.log ('NestingLevel: '+IntToStr(NestingLevel), LLInfo);
 //LogDatei.log ('script.FEvalBoolBaseNestLevel: '+IntToStr(script.FEvalBoolBaseNestLevel), LLInfo);
 if NestingLevel = script.FEvalBoolBaseNestLevel then
 begin
   // This is the complete boolean expression
   LogDatei.log (RunTimeInfo, LLInfo);
 end
 else // These are the parts in the recursion
   LogDatei.log (RunTimeInfo, LLDebug);
 list1.Free;

End;

function TuibInstScript.doSetVar (const section: TuibIniScript; const Expressionstr : String;
                   var Remaining : String; var InfoSyntaxError : String) : Boolean;
 //var
 // NestLevel : integer = 0;
begin
  result := doSetVar(section,Expressionstr, Remaining, InfoSyntaxError, NestingLevel);
end;

function TuibInstScript.doSetVar (const section: TuibIniScript; const Expressionstr : String;
                   var Remaining : String; var InfoSyntaxError : String;
                   var NestLevel : integer) : Boolean;
 var
   VarName : String='';
   VarValue : String='';
   r : String='';
   VarIndex : Integer=0;
   list : TXStringList;
   funcindex : integer = 0;

   function isStringlistVar(varname : string) : boolean;
   begin
     result := false;
     if isVisibleLocalVar(VarName,funcindex)  then
       if definedFunctionArray[FuncIndex].getLocalVarDatatype(varname) = dfpStringlist then
         result := true;
     if listOfStringLists.IndexOf(LowerCase(VarName)) >= 0 then
         result := true;
   end;

   function isStringVar(varname : string) : boolean;
   begin
     result := false;
     if isVisibleLocalVar(VarName,funcindex)  then
       if definedFunctionArray[FuncIndex].getLocalVarDatatype(varname) = dfpString then
         result := true;
     if VarList.IndexOf (LowerCase (VarName)) >= 0 then
         result := true;
   end;

begin
  result := false;
  GetWord (Expressionstr, VarName, r, WordDelimiterSet1);
  if VarName = ''
  then
     InfoSyntaxError := 'variable expected'
  else
  Begin
    //VarIndex := listOfStringLists.IndexOf(LowerCase(VarName));
    if isStringlistVar(Varname) // we should get a StringList
    then
    Begin
      if    Skip ('=', r, r, InfoSyntaxError)
        and produceStringList (section, r, Remaining, list,  InfoSyntaxError, Nestlevel,inDefFuncIndex)
      then
      if isVisibleLocalVar(VarName,funcindex)  then
      begin
        // local var
        result := definedFunctionArray[FuncIndex].setLocalVarValueList(varname,list);
      end
      else
      begin
      try
          VarIndex := listOfStringLists.IndexOf(LowerCase(VarName));
        ContentOfStringLists.Items[VarIndex] := list;
        result := true;
      except
      end;
      end;
      if result
      then
      Begin
        LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
        LogDatei.log('The value of the variable "'+Varname+'" is now:', LLDebug);
        LogDatei.log_list(list,LLDebug);
        LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
      End;
    End
    else
    Begin // it must be a string value
      if not isStringVar(varname)
      then
        InfoSyntaxError := 'Unknown variable name: '+VarName
      else
      Begin
        if    Skip ('=', r, r, InfoSyntaxError)
          and EvaluateString (r, Remaining, VarValue, InfoSyntaxError, Nestlevel,inDefFuncIndex)
        then
        Begin
          if isVisibleLocalVar(VarName,funcindex)  then
          begin
            // local var
            result := definedFunctionArray[FuncIndex].setLocalVarValueString(varname,VarValue);
          end
          else
          begin
          try
              VarIndex := VarList.IndexOf (LowerCase (VarName));
            ValuesList [VarIndex] := VarValue;
            result := true;
          except
          end;
          end;
        End
      End;

      if result
      then
      Begin
        LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
        LogDatei.log
        ('The value of the variable "'+Varname+'" is now: "' + VarValue + '"', LLInfo);
        LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
      End;
    End;
  End;
end;


{$IFDEF WINDOWS}
(* experimentell *)
function TuibInstScript.StartConnection (LocalName, NetResourceName: String; WaitTime : LongInt) : TSectionResult;
  Var
    NetResource : TNetResourceA;
    waitedTime : LongInt=0;
    showTime  : LongInt=0;
    waitTimeMilsec : LongInt=0;
    netresult : DWord=0;
    lastErrorCode : DWord=0;
    ErrorInfo : String='';
    ReportedUser : String='';



    function netErrorMessage (errorcode : Integer) : String;

    var
      pErrorBuff, pNameBuff : PChar;
      nErrorBuffSize : DWord=0;
      nNamebuffSize : DWord=0;
      Error : String='';
      Name : String='';

    begin

      case  errorcode of
       no_error                         : result := 'no_error';
       ERROR_ACCESS_DENIED          : result := 'Access to the network resource was denied.';
       ERROR_ALREADY_ASSIGNED           : result :=
          'The local device specified by lpLocalName is already connected to a network resource.';
       ERROR_BAD_DEV_TYPE               : result :=
          'The type of local device and the type of network resource do not match.';
       ERROR_BAD_DEVICE                 : result :=
          'The value specified by lpLocalName is invalid.';
       ERROR_BAD_NET_NAME               : result :=
          'The value specified by lpRemoteName is not acceptable to any network resource provider. ' +
          'The resource name is invalid, or the named resource cannot be located.';
       ERROR_BAD_PROFILE               : result :=
          'The user profile is in an incorrect format.';
       ERROR_BAD_PROVIDER               : result :=
          'The value specified by lpProvider does not match any provider.';
       ERROR_BUSY                       : result :=
          'The router or provider is busy, possibly initializing. The caller should retry.';
       ERROR_CANCELLED               : result :=
          'The attempt to make the connection was cancelled by the user through a dialog box from one ' +
          'of the network resource providers, or by a called resource.';
       ERROR_CANNOT_OPEN_PROFILE       : result :=
          'The system is unable to open the user profile to process persistent connections.';
       ERROR_DEVICE_ALREADY_REMEMBERED  : result :=
          'An entry for the device specified in lpLocalName is already in the user profile.';
       ERROR_EXTENDED_ERROR       :
          Begin
            GetMem (pErrorBuff, 300);
            GetMem (pNameBuff, 300);
            WNetGetLastError (DWord (errorcode), pErrorBuff, nErrorBuffSize, pNameBuff, nNameBuffSize);
            Error := pErrorBuff;
            result := Error;
            FreeMem (pErrorBuff);
            FreeMem (pNameBuff);
          End;
       ERROR_INVALID_PASSWORD      : result := 'The specified password is invalid.';
       ERROR_NO_NET_OR_BAD_PATH      : result :=
       'A network component has not started, or the specified name could not be handled.';
       ERROR_NO_NETWORK              : result := 'There is no network present.';
       else
        result := ''
      end;
    end;



begin
  result := tsrPositive;

  NetResource.dwType := RESOURCETYPE_DISK;
  NetResource.lpRemoteName :=  PAnsiChar (NetResourceName);
  NetResource.lpLocalName  :=  PAnsiChar (LocalName) ;
  NetResource.lpProvider := '';

  LogDatei.log ('Add Connection '  (* + 'User ' + Username *) +  ' to ' + NetResourceName, LLinfo);
  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

  netresult := wNetAddConnection2 (NetResource, nil (* PAnsiChar (Password) *), nil (* PAnsiChar (Username) *), 0);
  lastErrorCode := 0;

  if (netresult = no_error)
  then
  Begin
    GetNetUser ('', ReportedUser, ErrorInfo);
    ReportedUser := 'User ' + ReportedUser;
    LogDatei.log ('Sec 0 ' + ReportedUser + ' Connected ' + ErrorInfo, LLinfo)
  End
  else
  Begin
    lastErrorCode := GetLastError;
    GetNetUser ('', ReportedUser, ErrorInfo);
    LogDatei.log ( 'User ' + ReportedUser + ' '  + ErrorInfo, LLinfo);
    LogDatei.log ('Sec 0  Connect error '
                            + IntToStr (lastErrorCode) + ': "'
                            + RemoveLineBreaks(SysErrorMessage(lastErrorCode)) + '"', LLinfo);
    CentralForm.TimerWaitSet (100);
    // alle Zehntelsec Timer-Ereignis anschauen
  End;
  WaitedTime := 1000;
  ShowTime := 1000;
  WaitTimeMilsec := WaitTime * 1000;
  while (netresult <> no_error) and (WaitedTime < WaitTimeMilSec)
  do
  Begin
    ProcessMess;
    if CentralForm.TimerWaitReady (10)
    then
    (* nach 10 Zehntelsec erneuten Versuch unternehmen *)
    Begin
      netresult := wNetAddConnection2 (NetResource, nil (* PAnsiChar (Password)*), nil (* PAnsiChar (Username)*), 0);
      if netresult = no_error
      then
          LogDatei.log ('Sec ' + IntToStr (WaitedTime DIV 1000) + '  Connected', LLinfo)
      else
      Begin
        if (lastErrorCode <> GetLastError) or (ShowTime > 2000)
        then
        Begin
          lastErrorCode := GetLastError;
          ShowTime := 1000;
          LogDatei.log ('Sec ' + IntToStr (WaitedTime DIV 1000) + '  Connect error '
                   + IntToStr (lastErrorCode) + ': "'
                   + RemoveLineBreaks(SysErrorMessage (lastErrorCode))  + ' '
                   + netErrorMessage (lastErrorCode) + '"', LLinfo);
        End
        else
          ShowTime := ShowTime + 1000;

        CentralForm.TimerWaitSet (100);
        WaitedTime := WaitedTime + 1000;
      End
    End;
  End;

  if WaitedTime >= WaitTimeMilSec
  then
  Begin
    LogDatei.log ('tried for ' + IntToStr (WaitedTime DIV 1000) + ' sec', LLinfo);
    LogDatei.log ('Error: No Connect to ' + NetResourceName, LLError);

    DiffNumberOfErrors := 1;
  End
  else
   DiffNumberOfErrors := 0;

  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;

  if ExitOnError and (DiffNumberOfErrors > 0)
  then result := tsrExitProcess;
End;
{$ENDIF WINDOWS}


procedure TuibInstScript.ApplyTextConstants (var Sektion : TXStringList; CStringEscaping : Boolean);
 var
  i: Integer;
begin
 for i := 1 to ConstList.Count
 do
   if CStringEscaping
   then
    Sektion.GlobalReplace (1, Constlist.Strings [i-1], CEscaping (ConstValuesList.Strings [i-1]), false)
   else
    Sektion.GlobalReplace (1, Constlist.Strings [i-1], ConstValuesList.Strings [i-1], false)
end;


procedure TuibInstScript.ApplyTextVariables (var Sektion : TXStringList; CStringEscaping : Boolean);
 var
  i: Integer;
  locallist : Tstringlist;
begin
  if inDefinedFuncNestCounter > 0 then
  begin
     // first replace local function vars
     locallist := Tstringlist.Create;
     locallist.Text:= getVisibleLocalStringVarNameValueList.Text;
     for i := 1 to locallist.Count
     do
     Begin
       if CStringEscaping
       then
          Sektion.GlobalReplace (1, locallist.Names[i-1], CEscaping (locallist.ValueFromIndex[i-1]), false)
       else
          Sektion.GlobalReplace (1, locallist.Names[i-1], locallist.ValueFromIndex[i-1], false)
     end;
     locallist.Free;
  end;

  // now the global vars
 for i := 1 to VarList.Count
 do
 Begin
   if CStringEscaping
   then
      Sektion.GlobalReplace (1, Varlist.Strings [i-1], CEscaping (ValuesList.Strings [i-1]), false)
   else
      Sektion.GlobalReplace (1, Varlist.Strings [i-1], ValuesList.Strings [i-1], false)
 end;
end;

procedure TuibInstScript.ApplyTextVariablesToString (var mystr : String; CStringEscaping : Boolean);
 var
  i: Integer;
  NewLine : String;
  locallist : Tstringlist;
begin
  if inDefinedFuncNestCounter > 0 then
  begin
     // first replace local function vars
     locallist := Tstringlist.Create;
     locallist.Text:= getVisibleLocalStringVarNameValueList.Text;
     for i := 1 to locallist.Count
     do
     Begin
       if CStringEscaping then
       begin
         if ReplaceInLine(mystr, locallist.Names[i-1], CEscaping (locallist.ValueFromIndex[i-1]), false, NewLine) then
         begin
           mystr := NewLine;
         end;
       end
       else
       begin
         if ReplaceInLine(mystr, locallist.Names[i-1], locallist.ValueFromIndex[i-1], false, NewLine) then
         begin
           mystr := NewLine;
         end;
       end;
     end;
     locallist.Free;
  end;

  // now the global vars
  for i := 1 to VarList.Count
  do
  Begin
   if CStringEscaping then
   begin
     if ReplaceInLine(mystr, Varlist.Strings [i-1], CEscaping (ValuesList.Strings [i-1]), false, NewLine) then
     begin
       mystr := NewLine;
     end;
   end
   else
   begin
     if ReplaceInLine(mystr, Varlist.Strings [i-1], ValuesList.Strings [i-1], false, NewLine) then
     begin
       mystr := NewLine;
     end;
   end;
  end;
end;

function TuibInstScript.doAktionen (const Sektion: TWorkSection; const CallingSektion: TWorkSection)
                       : TSectionResult;
 var
  i : integer=0;
  StartlineOfSection: Integer=0;
  Parameter : String='';
  Filename : String='';
  SectionSpecifier : TSectionSpecifier;
  posSlash : integer=0;
  imageNo : Integer=0;
  ArbeitsSektion : TWorkSection;
  WaitConditions : TSetWaitConditions;
  expr : String='';
  ident : String='';
  WaitSecs : Word=0;
  numberString : String='';
  ActionResult : TSectionResult=0;
  ActLevel : integer=0;
  NestLevel : Integer=0;
  StatKind : TStatement;
  Expressionstr : String='';
  remaining : String='';
  registryformat : TRegistryFormat;
  reg_specified_basekey : String='';
  flag_force64 : boolean;
  BooleanResult : Boolean;
  Info : String='';
  ErrorInfo : String='';
  InfoSyntaxError : String='';
  SyntaxCheck : Boolean;
  sign : Integer=0;
  evaluated : Boolean;
  FName : String='';
  Labeltext : String='';
  localname : String='';
  remotename : String='';
  username : String='';
  password : String='';
  trytime : String='';
  timeout : Integer=0;
  Fehlertext : String='';
  output : TXStringlist;
  inputlist : TXStringList;
  inputname : String='';
  looplist : TXStringList;
  loopvar : String='';
  loopvalue : String='';
  loopindex : Integer=0;
  inloop : Boolean;
  continue : Boolean;
  stayOnStatement : String='';
  Startindentlevel : Integer=0;
  goon : boolean;
  sleepsecs : Integer=0;
  diffTime : TDateTime;
  Encoding2use,usedEncoding : String;
  ntuserpath : String = '';
  fullfilename : string;
  //filefound : boolean;

  //outlist : TXStringList;
  //versionI  : TVersionInfo; //from versioninfoX

  j : Integer=0;
  includelogLinecount : integer=0;
  //includelogStrList : TStringList;
  logtailLinecount : integer=0;
  logtailLinecountstr : String='';
  logtailEncodingstr : String='system';
  includeLogLineStart : integer=0;
  aktline : integer=0;
  call : String='';
  p1 : String='';
  p2 : String='';
  p3 : String='';
  p4 : String='';
  s1 : String='';
  s2 : String='';
  s3 : String='';
  s4 : String='';
  seconds : String='';
  runAs : TRunAs;
  linecount ,k, constcounter : integer;
  incfilename, fullincfilename, testincfilename, incline, myline,replacedline : string;
  //remaining, errorinfo, expr , numberString : string;
  incfile : TextFile;
  inclist : TStringlist;
  found   : boolean;
  //, doincludes, evaluated : boolean;
  loopstart, loopstop : integer;
  dummybool : boolean;
  switchCondition : string='';
  switchExpressionstr : string='';
  InSwitch : boolean=false;
  InCase : boolean=false;
  ValidCase : boolean=false;
  SwitchResolved : boolean=false;
  LppClientId : string='';
  LppProductId : string='';
  newDefinedfunction : TOsDefinedFunction;
  dummylist : TStringList;
  //endofDefFuncFound : boolean;
  inDefFunc : integer = 0;
  inDefFunc2 : integer = 0;
  //inDefFunc3 : integer = 0;  // we are between deffunc and endfunc line (even in a not active code)
  funcindex,secindex : integer;
  importFunctionName : String;
  inSearchedFunc : boolean;
  alllines, inclines : integer;
  processline : boolean = true; // are we on a active code branch (needed handling of section ends '['
  tmplist : TXStringlist;
  secname : string;
  tmpint : integer;
  tmpstr, tmpstr1, tmpstr2, tmpstr3 : string;
  tmpbool, tmpbool1 : boolean;

begin
  Script.FLastSection := Script.FActiveSection;
  Script.ActiveSection := sektion;
  result := tsrPositive;
  ActionResult := tsrPositive;

  NestLevel := Sektion.NestingLevel;
  ActLevel := NestLevel;
  conditions [ActLevel] := true;

  StartIndentLevel := LogDatei.LogSIndentLevel;
  //LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

  //FBatchOberflaeche.setPicture (3, '', '');

  ArbeitsSektion := TWorkSection.create (NestLevel,Sektion);
  output := TXStringList.Create;
  {$IFDEF GUI}
  FBatchOberflaeche.setWindowState(batchWindowMode);
  {$ENDIF GUI}

  i := 1;
  continue := true;

  looplist := TXStringList.create;
  inloop := false;

  while (i <= Sektion.count) and continue
  and (actionresult > tsrFatalError)
  and (FExtremeErrorLevel > levelfatal)
  and not scriptstopped
  do
  begin
   //writeln(actionresult);
    Remaining := trim (Sektion.strings [i-1]);
    //logdatei.log_prog('Actlevel: '+IntToStr(Actlevel)+' NestLevel: '+IntToStr(NestLevel)+' Sektion.NestingLevel: '+IntToStr(Sektion.NestingLevel)+' condition: '+BoolToStr(conditions [ActLevel],true),LLDebug3);
    if (inDefFuncLevel = 0)          // count only lines on base level
      and (lowercase(Sektion.Name) = 'actions')   // count only lines in actions
      then inc(FAktScriptLineNumber);
    logdatei.log_prog('Script line: '+intToStr(i)+' / '+intToStr(FAktScriptLineNumber)+' : '+Remaining,LLDebug2);
    //writeln(remaining);
    //readln;

    repeat // looping if inLoop, otherwise going through the statements just once

      if inLoop
      then
      Begin
        Remaining := stayOnStatement; //looping on this statement
        loopvalue := looplist[0];
        Remaining := StringReplace(Remaining, loopvar, loopvalue);
        looplist.delete (0);
      End;

      // if remaining starts with DefFunc
      if pos(lowercase(PStatNames^ [tsDefineFunction]),lowercase(trim(Remaining))) = 1 then
        inc(inDefFunc3);
      // if remaining starts with EndFunc
      if pos(lowercase(PStatNames^ [tsEndFunction]),lowercase(trim(Remaining))) = 1 then dec(inDefFunc3);
      //if (lowercase(Remaining) = lowercase(PStatNames^ [tsEndFunction])) then dec(inDefFunc2);
      logdatei.log_prog('Parsingprogress: inDefFunc: '+IntToStr(inDefFunc),LLDebug3);
      logdatei.log_prog('Parsingprogress: inDefFuncIndex: '+IntToStr(inDefFuncIndex),LLDebug3);
      logdatei.log_prog('Parsingprogress: inDefFunc3: '+IntToStr(inDefFunc3),LLDebug);

      if (Remaining = '') or (Remaining [1] = LineIsCommentChar)
      then
         // continue
      else if (Remaining [1] = '[') then
      begin
        // subsection beginning
        logdatei.log_prog('line is starting with "[": inDefFunc3: '+IntToStr(inDefFunc3),LLInfo);

      //else if (Remaining [1] = '[')  then
         // subsection beginning
        //if (inDefFunc3 = 0) and (inDefFuncIndex = -1) then
        //if (inDefFunc3 = 0) or processline then
        if (inDefFunc3 = 0) or processline then
        begin
          // (inDefFunc3 = 0) : we are not between deffunc and enfunc
          // ((inDefFuncIndex = -1)) : we are on base level (no local functions active)
           continue := false;
           LogDatei.log ('Section ending since next line is starting with "["', LLInfo);
        end
      end
      else
      Begin
        call := remaining;
        Expressionstr := '';
        logdatei.log('Parsingprogress: r: '+Remaining+' exp: '+Expressionstr,LLDebug3);
        GetWord (Remaining, Expressionstr, Remaining, WordDelimiterSet4);
        logdatei.log('Parsingprogress: r: '+Remaining+' exp: '+Expressionstr,LLDebug3);
        StatKind := FindKindOfStatement (Expressionstr, SectionSpecifier, call);
        ArbeitsSektion.Name := Expressionstr;
        ArbeitsSektion.SectionKind := StatKind;
        ArbeitsSektion.NestingLevel:=Nestlevel;
        logdatei.log_prog('Actlevel: '+IntToStr(Actlevel)+' NestLevel: '+IntToStr(NestLevel)+' ArbeitsSektion.NestingLevel: '+IntToStr(ArbeitsSektion.NestingLevel)+' condition: '+BoolToStr(conditions [ActLevel],true),LLDebug2);


        // start switch statement
        // syntax:
        // switch <string expr ; normally variable>
        // case <string const>
        // endcase
        // defaultcase
        // endswitch
        if StatKind = tsSwitch then
        Begin
          LogDatei.log_prog('Entering Switch statement',LLDebug2);
          if inswitch then
          begin
            reportError (Sektion, i, '', 'Nested Switch Statement is not allowed.');
            exit;
          end
          else
          begin
            Expressionstr := Remaining;
            if EvaluateString(Expressionstr, Remaining, switchCondition,InfoSyntaxError) then
            begin
              InSwitch := true;
              InCase := false;
              ValidCase := false;
              SwitchResolved := false;
              doLogEntries (PStatNames^ [tsSwitch]+' : '+switchCondition, LLinfo);
              LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
            end
            else reportError (Sektion, i, Expressionstr, InfoSyntaxError);
          end
        end

        else if StatKind = tsSwitchCaseOpen then
        Begin
          LogDatei.log_prog('Entering Case statement',LLDebug2);
          if not inswitch then
          begin
            reportError (Sektion, i, '', 'Case Statement only allowed inside switch statement.');
            exit;
          end
          else
          if InCase then
          begin
            reportError (Sektion, i, '', 'Nested Case Statement is not allowed.');
            exit;
          end
          else
          begin
            Expressionstr := Remaining;
            if EvaluateString(Expressionstr, Remaining, switchExpressionstr,InfoSyntaxError) then
            begin
              doLogEntries (PStatNames^ [tsSwitchCaseOpen]+' : '+switchExpressionstr, LLinfo);
              InSwitch := true;
              InCase := true;
              if LowerCase(switchExpressionstr) = LowerCase(switchCondition) then
              begin
                ValidCase := true;
                SwitchResolved := true;
                LogDatei.log('Case match: '+switchExpressionstr+' = '+switchCondition,LLInfo);
                LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
              end
              else
              begin
                ValidCase := false;
                LogDatei.log('Case mismatch: '+switchExpressionstr+' != '+switchCondition,LLDebug2);
              end;
            end
            else reportError (Sektion, i, Expressionstr, InfoSyntaxError);
          end
        end

        else if StatKind = tsSwitchCaseClose then
        Begin
          LogDatei.log_prog('Entering EndCase statement',LLDebug2);
          if not inswitch then
          begin
            reportError (Sektion, i, '', 'EndCase Statement only allowed inside switch statement.');
            exit;
          end
          else
          if not InCase then
          begin
            reportError (Sektion, i, '', 'There is no open Case statement to close.');
            exit;
          end
          else
          begin
            doLogEntries (PStatNames^ [tsSwitchCaseClose], LLinfo);
            if ValidCase then LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
            InSwitch := true;
            InCase := false;
            ValidCase := false;
          end
        end

        else if StatKind = tsSwitchDefaultOpen then
        Begin
          LogDatei.log_prog('Entering DefaultCase statement',LLDebug2);
          if not inswitch then
          begin
            reportError (Sektion, i, '', 'DefaultCase Statement only allowed inside switch statement.');
            exit;
          end
          else
          if InCase then
          begin
            reportError (Sektion, i, '', 'Nested Case Statement is not allowed.');
            exit;
          end
          else
          begin
            InSwitch := true;
            InCase := true;
            if not SwitchResolved then
            begin
              doLogEntries (PStatNames^ [tsSwitchDefaultOpen], LLinfo);
              ValidCase := true;
              SwitchResolved := true;
              LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
            end
          end
        end

        else if (StatKind = tsSwitchClose) then
        Begin
          LogDatei.log_prog('Entering EndSwitch statement',LLDebug2);
          if not inswitch then
          begin
            reportError (Sektion, i, '', 'EndSwitch Statement only allowed inside switch statement.');
            exit;
          end
          else
          if InCase then
          begin
            reportError (Sektion, i, '', 'There is a open Case which have to be closed by EndCase before the use of EndSwitch');
            exit;
          end
          else
          begin
            doLogEntries (PStatNames^ [tsSwitchClose], LLinfo);
            InSwitch := false;
            InCase := false;
            ValidCase := false;
            LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
          end
        end

        // treat statements for if-else first

        else if (StatKind = tsCondOpen) and (not(InSwitch) or ValidCase) then
        Begin
          //if FExtremeErrorLevel > levelfatal then
          begin
            inc (NestLevel);
            Ifelseendiflevel:=Nestlevel;
            ThenBranch [NestLevel] := true;
            logdatei.log_prog('IF: Actlevel: '+IntToStr(Actlevel)+' NestLevel: '+IntToStr(NestLevel)+' sektion.NestingLevel: '+IntToStr(sektion.NestingLevel)+' ThenBranch: '+BoolToStr(ThenBranch [NestLevel],true),LLDebug);
            doLogEntries (PStatNames^ [tsCondOpen], LLinfo);
            if NestLevel > High (TConditions)
            then
            Begin
              reportError (Sektion, i, '', 'Too many nested conditions');
              exit;
            End;

            LogDatei.LogSIndentLevel := NestLevel;

            if (NestLevel = ActLevel + 1) and Conditions [ActLevel] then
            begin
              // eine neue aktive Ebene wird erzeugt, falls
              // die if-Anweisung auf einer aktiven Ebene UND im positiven Zweig steht

              Expressionstr := Remaining;
              if EvaluateBoolean (Expressionstr, Remaining, BooleanResult,
                                  NestLevel, InfoSyntaxError)
              then
              begin
                 inc (ActLevel);
                 Conditions [NestLevel] := BooleanResult;
              end
              else
                reportError (Sektion, i, Expressionstr, InfoSyntaxError);
              if Remaining <> ''
              then reportError (Sektion, i, Remaining, 'erroneous characters ');
            end;

            LogDatei.LogSIndentLevel := NestLevel - 1;
            doLogEntries (PStatNames^ [tsCondThen], LLInfo);
            LogDatei.LogSIndentLevel := NestLevel;
          end;
          //ArbeitsSektion.NestingLevel:=Nestlevel;
          //Sektion.NestingLevel:=Nestlevel;
        End

        else if (StatKind = tsCondElse) and (not(InSwitch) or ValidCase) then
        Begin
          //if FExtremeErrorLevel > levelfatal then
          begin
            logdatei.log_prog('ELSE: Actlevel: '+IntToStr(Actlevel)+' NestLevel: '+IntToStr(NestLevel)+' sektion.NestingLevel: '+IntToStr(sektion.NestingLevel)+' ThenBranch: '+BoolToStr(ThenBranch [NestLevel],true),LLDebug);
            if NestLevel <= Sektion.NestingLevel
            then
              reportError (Sektion, i, '',PStatNames^ [tsCondElse] + '  without  ' + PStatNames^ [tsCondOpen])
            else
            Begin
              if not ThenBranch [NestLevel]
              then
              begin
                logdatei.log_prog('ELSE: Actlevel: '+IntToStr(Actlevel)+' NestLevel: '+IntToStr(NestLevel)+' sektion.NestingLevel: '+IntToStr(sektion.NestingLevel)+' ThenBranch: '+BoolToStr(ThenBranch [NestLevel],true),LLWarning);
                reportError (Sektion, i, '', 'double ' + PStatNames^ [tsCondElse]);
              end
              else
              Begin
                ThenBranch [NestLevel] := false;

                    // innerhalb einer aktiven Ebene Wechsel zwischen positivem/negativen Zweig
                LogDatei.LogSIndentLevel := NestLevel - 1;
                doLogEntries (PStatNames^ [tsCondElse], LLinfo);
                LogDatei.LogSIndentLevel := NestLevel;

                if NestLevel = ActLevel then Conditions [ActLevel] := not Conditions [ActLevel];
              End;
            End;
          end;
        End

        else if (StatKind = tsCondClose) and (not(InSwitch) or ValidCase) then
        begin
         // if FExtremeErrorLevel > levelfatal then
          begin
            //Herabstufen einer formalen Ebene, innerhalb einer aktiven Ebene Beenden von dieser
            // End active level and go down one level

            if NestLevel = ActLevel then dec (ActLevel);
            dec (NestLevel);
            Ifelseendiflevel:=Nestlevel;
            try
              logdatei.log_prog('ENDIF: Actlevel: '+IntToStr(Actlevel)+' NestLevel: '+IntToStr(NestLevel)+' sektion.NestingLevel: '+IntToStr(sektion.NestingLevel)+' ThenBranch: '+BoolToStr(ThenBranch [NestLevel],true),LLDebug);
            except
              logdatei.log_prog('ENDIF: Actlevel: '+IntToStr(Actlevel)+' NestLevel: '+IntToStr(NestLevel)+' sektion.NestingLevel: '+IntToStr(sektion.NestingLevel)+' ThenBranch: unknown',LLDebug);
            end;
            //ArbeitsSektion.NestingLevel:=Nestlevel;
            //Sektion.NestingLevel:=Nestlevel;

            LogDatei.LogSIndentLevel := NestLevel ;
            doLogEntries (PStatNames^ [tsCondClose] , LLinfo);


            if NestLevel < Sektion.NestingLevel
            then
              reportError (Sektion, i, '', PStatNames^ [tsCondClose] + '  without  ' + PStatNames^ [tsCondOpen]);
          end;
        End


        else
           // Ausfuehrung alles folgenden nur falls in einem aktuellen true-Zweig
           // further line execution only if:
           // we are in a valid if or else branch
        if ((NestLevel = ActLevel) and conditions [ActLevel])
           // and if we are inside a Switch and a valid Case
           // in other words: and ( not(InSwitch) or ValidCase)
           and (not(InSwitch) or ValidCase)
           // sowie falls weitere Bearbeitung gewuenscht
           // and line processing not stoped now
            and (ActionResult > 0)
        then
        Begin
          processline := true;
          case SectionSpecifier of
           tsecIncluded:
            Begin

              ArbeitsSektion.clear;
              SearchForSectionLines(self,Sektion,CallingSektion,Expressionstr,
                 TXStringList (ArbeitsSektion),StartlineOfSection, true, true, false);

              (*
              ArbeitsSektion.clear;

              // look if we are in a subprogram
              // that may have its own sections in it

              if (ArbeitsSektion.count = 0) and (inDefFuncLevel > 0)
              then
              begin
                // local function
                Logdatei.log('Looking for section: '+ Expressionstr +' in local function .',LLDebug3);
                Sektion.GetSectionLines (Expressionstr, TXStringList (ArbeitsSektion),
                                  StartlineOfSection, true, true, false);
              end;

              if ArbeitsSektion.count = 0
              then
              begin
                // normal case
                Logdatei.log('Looking for section: '+ Expressionstr +' in standard section.',LLDebug3);
                GetSectionLines (Expressionstr, TXStringList (ArbeitsSektion),
                                  StartlineOfSection, true, true, false);
              end;

              if ArbeitsSektion.count = 0
              then
              begin
                // subsub case
                if Assigned(CallingSektion) and (CallingSektion <> nil) then
                begin
                  Logdatei.log('Looking for section: '+ Expressionstr +' in calling section.',LLDebug3);
                  CallingSektion.GetSectionLines (Expressionstr, TXStringList (ArbeitsSektion),
                                    StartlineOfSection, true, true, false);
                end;
              end;


              if ArbeitsSektion.count = 0
              then
              begin
                // subsub case
                Logdatei.log('Looking for section: '+ Expressionstr +' in global section.',LLDebug3);
                Sektion.GetSectionLines (Expressionstr, TXStringList (ArbeitsSektion),
                                  StartlineOfSection, true, true, false);
              end;

              if ArbeitsSektion.count = 0
              then
              begin
                // subsubsub case
                if Assigned(CallingSektion.ParentSection) and (CallingSektion.ParentSection <> nil) then
                begin
                  Logdatei.log('Looking for section: '+ Expressionstr +' in CallingSektion.ParentSection section.',LLDebug3);
                  CallingSektion.ParentSection.GetSectionLines(Expressionstr, TXStringList (ArbeitsSektion),
                                  StartlineOfSection, true, true, false);
                end;
              end;

              if ArbeitsSektion.count = 0
              then
              begin
                // subsubsubsub case
                if Assigned(CallingSektion.ParentSection) and (CallingSektion.ParentSection <> nil)
                   and Assigned(CallingSektion.ParentSection.ParentSection)
                   and (CallingSektion.ParentSection.ParentSection <> nil) then
                begin
                  Logdatei.log('Looking for section: '+ Expressionstr +' in CallingSektion.FParentSection.FParentSectio section.',LLDebug3);
                  CallingSektion.FParentSection.FParentSection.GetSectionLines(Expressionstr, TXStringList (ArbeitsSektion),
                                  StartlineOfSection, true, true, false);
                end;
              end;
              *)


              if inLoop then ArbeitsSektion.GlobalReplace(1, loopvar, loopvalue, false);

              if ArbeitsSektion.count = 0
              then
              Begin
                LogDatei.log('Warning: The section "' + Expressionstr + '" (called in line ' +
                  inttostr(i) +
                  ' of the calling section) does not exist or is empty', LLWarning);
                FNumberOfWarnings := FNumberOfWarnings + 1;
              End

              else
                 ArbeitsSektion.StartLineNo := StartlineOfSection;
            End;




           tsecExpected:
            Begin
              ArbeitsSektion.clear;

              inputlist := TXStringList.create;
              inputname := Remaining;

              if produceStringList (sektion ,Remaining, Remaining, inputlist, InfoSyntaxError)
              then // do we get the section as a string list? then ready.
              begin
                Arbeitssektion.Assign(inputlist);
                if Arbeitssektion.Count > 0 then
                begin
                 ArbeitsSektion.Name := ArbeitsSektion.Name +  ' {' + inputname + '} ';
                 ArbeitsSektion.StartLineNo := 1;
                end
              end

               // Otherwise, we should have a file name and try to get our command lines from it
              else if EvaluateString (Remaining, Remaining, FName, InfoSyntaxError)
              then
              Begin
                fullfilename := '';
                ArbeitsSektion.Name := ArbeitsSektion.Name + ' "' + FName + '"';
                Expressionstr := Expressionstr + ' ["' + FName + '"]';
                found := false;
                // full file path given
                testincfilename := ExpandFilename(FName);
                LogDatei.log('Looking for: '+testincfilename,LLDebug2);
                if FileExistsUTF8(testincfilename) then
                begin
                  found := true;
                  fullfilename := testincfilename;
                end;
                if (not found) then
                begin
                  // search in %ScriptPath%
                  testincfilename := ExtractFileDir(FFilename)+PathDelim+FName;
                  LogDatei.log('Looking for: '+testincfilename,LLDebug2);
                  if FileExistsUTF8(testincfilename) then
                  begin
                    found := true;
                    fullfilename := testincfilename;
                  end;
                end;
                {$IFDEF WINDOWS}
                if (not found) then
                begin
                  // search in %opsiScriptHelperPath%\lib
                  testincfilename := getSpecialFolder(CSIDL_PROGRAM_FILES)+'\opsi.org\opsiScriptHelper'
                                       +PathDelim+FName;
                  if FileExistsUTF8(testincfilename) then
                  begin
                    found := true;
                    fullfilename := testincfilename;
                  end;
                end;
                {$ENDIF WINDOWS}
                if (not found) then
                begin
                  // search in %ScriptDrive%\lib  aka %ScriptPath%/../lib
                  testincfilename := ExtractFileDir(FFilename)
                                      +PathDelim+'..'
                                      +PathDelim+'lib'+PathDelim+FName;
                  LogDatei.log('Looking for: '+testincfilename,LLDebug2);
                  if FileExistsUTF8(testincfilename) then
                  begin
                    found := true;
                    fullfilename := testincfilename;
                  end;
                end;
                {$IFDEF WINDOWS}
                if (not found) then
                begin
                  // search in %WinstDir%\lib
                  testincfilename := ExtractFileDir(Paramstr(0))
                                       +PathDelim+'lib'+PathDelim+FName;
                  LogDatei.log('Looking for: '+testincfilename,LLDebug2);
                  if FileExistsUTF8(testincfilename) then
                  begin
                    found := true;
                    fullfilename := testincfilename;
                  end;
                end;
                {$ENDIF WINDOWS}
                if fullfilename = '' then
                begin
                  LogDatei.log('Error: No file found for called: '+FName+ ' - Could not load external sub section',LLError);
                end
                else
                begin
                  if CheckFileExists (fullfilename, ErrorInfo)
                  then
                  Begin
                    try
                      LoadValidLinesFromFile (fullfilename, ArbeitsSektion);

                      ArbeitsSektion.StartLineNo := 1;
                    except
                      Logdatei.log ('File "' + fullfilename + '" cannot be read', LLError);
                    end;
                  End
                  else
                  Begin
                    Logdatei.log ('Error: File "' + fullfilename + '" not found', LLError);
                  End;
                end;
              End
              else
                reportError (Sektion, i, Expressionstr + '^' + Remaining, InfoSyntaxError);

            End
          End;


          // treating first the special cases of sub sections and loops
          if StatKind in [tsActions,tsProfileActions]
          then
          Begin
            if (ArbeitsSektion.Count > 0) then
            begin
              if StatKind = tsProfileActions then
                runProfileActions := true;
              LogDatei.log ('', LLinfo);
              LogDatei.log ('~~~~~~~ Start Sub ~~~~~~~  ' + ArbeitsSektion.Name, LLinfo);
              ArbeitsSektion.NestingLevel := NestLevel; // ArbeitsSektion.NestingLevel + 1;
              ApplyTextConstants (TXStringList (ArbeitsSektion), false);

              ActionResult := doAktionen (ArbeitsSektion, Sektion);
              LogDatei.log ('', LLinfo);
              LogDatei.log ('~~~~~~~ End Sub   ~~~~~~~  ' + ArbeitsSektion.Name, LLinfo);
              LogDatei.log ('', LLinfo);
              runProfileActions := false;
            end
            else Logdatei.log ('Error: Work section is empty', LLError);
          End

          else if (StatKind = tsLoopStringList) or (StatKind = tsLoopForTo)
          // we have to fetch the ArbeitsSektion after checking the loop syntax
          then
          Begin
            if inloop
            then
            begin
              syntaxCheck := false;
              InfoSyntaxError := 'loop in loop not yet implemented';
            End
            else
            begin
              // syntax: for varname in list
              syntaxCheck := false;
              GetWord (Remaining, loopvar, Remaining, WordDelimiterWhiteSpace);
              if     ( VarList.IndexOf (LowerCase (loopvar)) >= 0 )
                  or  ( listOfStringLists.IndexOf (LowerCase (loopvar)) >= 0 )
              then
                 InfoSyntaxError := 'Existing variable must not be used als loop variable'
              else
              Begin
                VarList.add (loopvar);
                ValuesList.add ('');
                // loop through stringlist
                if Skip ('in', Remaining, Remaining, InfoSyntaxError)
                    and produceStringList (sektion, Remaining, Remaining, looplist, InfoSyntaxError)
                    and Skip ('do', Remaining, Remaining, InfoSyntaxError)
                then
                begin
                  if looplist.count = 0 then
                  begin
                     LogDatei.log('Warning: list to loop through is empty - no loop ...', LLWarning);
                     // clearing the loop variable from the list of variables, first the value
                     ValuesList.Delete( varlist.indexOf (loopvar));
                     varlist.Delete( varlist.indexOf (loopvar) );
                     syntaxCheck := true;
                  end
                  else
                  begin
                    syntaxCheck := true;
                    inloop := true;
                    //GetWord (Remaining, Expressionstr, Remaining, WordDelimiterSet1);
                    LogDatei.log ('', LLinfo);
                    s1 := '';
                    for loopindex := 0 to looplist.count - 2 do
                    Begin s1 := s1 + '''' + looplist[loopindex] + ''', '; End;
                    if looplist.count > 0
                    then
                      s1 := s1 + '''' + looplist[looplist.count - 1]+ '''';

                    LogDatei.log('~~~~~~ Looping through:  ' + s1, LLinfo);
                    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

                    stayOnStatement := remaining;
                  end;
                 end
                 else
                 // loop through loopstart to loopstop
                 begin
                   if Skip ('=', Remaining, Remaining, InfoSyntaxError) then
                   begin
                     syntaxCheck := true;
                     if not evaluatestring(Remaining, Remaining,Expressionstr,InfoSyntaxError) then
                        syntaxCheck := false
                     else
                     begin
                       try
                         loopstart := StrToInt(Expressionstr);
                       except
                         syntaxCheck := false;
                         InfoSyntaxError :=  Expressionstr+ 'is no valid integer';
                       end;
                     end;
                   end;
                   if syntaxCheck then
                   begin
                     if Skip ('to', Remaining, Remaining, InfoSyntaxError) then
                     begin
                       if not evaluatestring(Remaining, Remaining,Expressionstr,InfoSyntaxError) then
                          syntaxCheck := false
                       else
                         try
                           loopstop := StrToInt(Expressionstr);
                         except
                           syntaxCheck := false;
                           InfoSyntaxError :=  Expressionstr+ 'is no valid integer';
                         end;
                     end;
                   end;
                   if syntaxCheck then
                   begin
                     if not Skip ('do', Remaining, Remaining, InfoSyntaxError) then
                     begin
                       syntaxCheck := false;
                       InfoSyntaxError :=  '"do" expected';
                     end;
                   end;
                   if syntaxCheck then
                   begin
                      if loopstart > loopstop then
                      begin
                         LogDatei.log('Warning: list to loop through is empty - no loop ...', LLWarning);
                         // clearing the loop variable from the list of variables, first the value
                         ValuesList.Delete( varlist.indexOf (loopvar));
                         varlist.Delete( varlist.indexOf (loopvar) );
                         syntaxCheck := true;
                      end
                      else
                      begin
                        syntaxCheck := true;
                        inloop := true;
                        looplist.Clear;
                        for loopindex := loopstart to loopstop do
                        looplist.Append(inttostr(loopindex));
                        LogDatei.log ('', LLinfo);
                        s1 := '';
                        for loopindex := 0 to looplist.count - 2 do
                        Begin s1 := s1 + '''' + looplist[loopindex] + ''', '; End;
                        if looplist.count > 0
                        then
                          s1 := s1 + '''' + looplist[looplist.count - 1]+ '''';
                        LogDatei.log('~~~~~~ Looping through:  ' + s1, LLinfo);
                        LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
                        stayOnStatement := remaining;
                      end;
                   end;
                 end; // loop
              end; // loopvar valid
            End;  // get loop var
            if not syntaxCheck then reportError (Sektion, i, Expressionstr, InfoSyntaxError);
          End  //tsLoopStringList

          else //
          Begin
            if StatKind in [tsFileActions]
            then
            Begin
              // do ApplyTextConstants later while working
            End
            else
            Begin
              if StatKind in [tsExecutePython, tsExecuteWith_escapingStrings]
              then
              Begin
                ApplyTextConstants (TXStringList (ArbeitsSektion), true);
                ApplyTextVariables (TXStringList (ArbeitsSektion), true);
              End
              else
              Begin
                ApplyTextConstants (TXStringList (ArbeitsSektion), false);
                ApplyTextVariables (TXStringList (ArbeitsSektion), false);
              End;
            End;


            if inloop then ArbeitsSektion.GlobalReplace(1, loopvar, loopvalue, false);

            case StatKind of
              tsMessage:
                if skip ('=', remaining, remaining, InfoSyntaxError)
                then
                  ActionResult := doInfo (Remaining)
                else if EvaluateString (Remaining, Remaining, Parameter, InfoSyntaxError)
                then
                begin
                  ActionResult := doInfo (Parameter);
                  LogDatei.log('message '+Parameter, LLnotice)
                end
                else
                  ActionResult
                  := reportError (Sektion, i, Sektion.strings [i-1], InfoSyntaxError);

              tsMessageFile:
                if EvaluateString (Remaining, Remaining, FName, InfoSyntaxError)
                then
                Begin
                  {$IFDEF GUI}
                  CreateSystemInfo;
                  SystemInfo.Memo1.Lines.clear;
                  try
                    Fname := ExpandFileName(Fname);
                    SystemInfo.Memo1.Lines.LoadFromFile (FName);
                    //SystemInfo.Memo1.Lines.Text := reencode(SystemInfo.Memo1.Lines.Text, 'system');
                    Logdatei.log ('', LevelComplete);
                    Logdatei.log ('ShowMessagefile "' + Fname + '"', LevelComplete);
                    SystemInfo.Showmodal;
                  except
                      on E: exception do
                      Begin
                        Logdatei.log ('ShowMessagefile "' + Fname + '"', LLwarning);
                        Logdatei.log ('  File does not exist or cannot be accessed, system message: "' + E.Message +'"',
                                               LLwarning);
                      End
                  end;

                  SystemInfo.free; SystemInfo := nil;
                  FBatchOberflaeche.BringToFront;
                  {$ENDIF GUI}

                End
                else
                  ActionResult
                  := reportError (Sektion, i, Sektion.strings [i-1], InfoSyntaxError);

                tsDefinedVoidFunction:
                  begin
                     // defined local function ?
                     GetWord (call, p1, p2, WordDelimiterSet5);
                     FuncIndex := definedFunctionNames.IndexOf (LowerCase (p1));
                     if FuncIndex >= 0 then
                     begin
                       if not (definedFunctionArray[FuncIndex].datatype = dfpVoid) then
                       begin
                         // error
                         syntaxCheck := false;
                         LogDatei.log('Syntax Error: defined function: '+p1+' is not from type string.',LLError);
                       end
                       else
                       begin
                         if definedFunctionArray[FuncIndex].call(p2,p2,NestLevel) then
                         begin
                           syntaxCheck := true;
                           //logdatei.log('We leave the defined function: inDefFunc3: '+IntToStr(inDefFunc3),LLInfo);
                         end
                         else
                         begin
                           // defined function call failed
                           LogDatei.log('Call of defined function: '+p1+' failed',LLError);
                           syntaxCheck := false;
                         end;
                       end;
                     end
                  end;

                tsImportLib:
                  begin
                    syntaxCheck := EvaluateString (Remaining, Remaining, FName, InfoSyntaxError);
                    if syntaxCheck then
                    Begin
                      try
                       importFunctionName := '';
                        fullincfilename := '';
                        LogDatei.log_prog('Found ImportLib statement for: '+FName,LLInfo);
                        if pos('::',FName) > 0 then
                        begin
                          incfilename:= copy(FName,0,pos('::',FName)-1);
                          //incfilename:= ExpandFileName(incfilename);
                          importFunctionName := copy(FName,pos('::',FName)+2,length(FName));
                        end
                        else incfilename := FName;
                        if ExtractFileExt(incfilename) = '' then
                            incfilename:= incfilename + '.opsiscript';
                        LogDatei.log('Found ImportLib statement for file: '+incfilename +' and function: '+importFunctionName,LLDebug);
                        found := false;
                        // full file path given ?
                        testincfilename := ExpandFilename(incfilename);
                        LogDatei.log_prog('Looking for: '+testincfilename,LLNotice);
                        if FileExistsUTF8(testincfilename) then
                        begin
                          found := true;
                          fullincfilename := testincfilename;
                        end;
                        if (not found) then
                        begin
                          // search in %ScriptPath%
                          testincfilename := ExtractFileDir(FFilename)+PathDelim+incfilename;
                          testincfilename := ExpandFilename(testincfilename);
                          LogDatei.log_prog('Looking for: '+testincfilename,LLNotice);
                          if FileExistsUTF8(testincfilename) then
                          begin
                            found := true;
                            fullincfilename := testincfilename;
                          end;
                        end;
                        {$IFDEF WINDOWS}
                        if (not found) then
                        begin
                          // search in %opsiScriptHelperPath%\lib
                          testincfilename := getSpecialFolder(CSIDL_PROGRAM_FILES)+'\opsi.org\opsiScriptHelper'
                                               +PathDelim+incfilename;
                          testincfilename := ExpandFilename(testincfilename);
                          LogDatei.log_prog('Looking for: '+testincfilename,LLNotice);
                          if FileExistsUTF8(testincfilename) then
                          begin
                            found := true;
                            fullincfilename := testincfilename;
                          end;
                        end;
                        {$ENDIF WINDOWS}
                        if (not found) then
                        begin
                          // search in %ScriptDrive%\lib  aka %ScriptPath%/../lib
                          testincfilename := ExtractFileDir(FFilename)
                                              +PathDelim+'..'
                                              +PathDelim+'lib'+PathDelim+incfilename;
                          testincfilename := ExpandFilename(testincfilename);
                          LogDatei.log_prog('Looking for: '+testincfilename,LLNotice);
                          if FileExistsUTF8(testincfilename) then
                          begin
                            found := true;
                            fullincfilename := testincfilename;
                          end;
                        end;
                        if (not found) then
                        begin
                          {$IFDEF WINDOWS}
                          // search in %WinstDir%\lib
                          testincfilename := ExtractFileDir(Paramstr(0))
                                               +PathDelim+'lib'+PathDelim+incfilename;
                          {$ENDIF WINDOWS}
                          {$IFDEF LINUX}
                          // search in /usr/share/opsi-script/lib
                          testincfilename := '/usr/share/opsi-script'
                                               +PathDelim+'lib'+PathDelim+incfilename;
                          {$ENDIF LINUX}
                          testincfilename := ExpandFilename(testincfilename);
                          LogDatei.log_prog('Looking for: '+testincfilename,LLNotice);
                          if FileExistsUTF8(testincfilename) then
                          begin
                            found := true;
                            fullincfilename := testincfilename;
                          end;
                        end;
                        if found then
                        begin
                          inSearchedFunc := false;
                          LogDatei.log('Found File: '+fullincfilename,LLDebug2);
                          inclist := TStringList.Create;
                          inclist.LoadFromFile(ExpandFileName(fullincfilename));
                          Encoding2use := searchencoding(inclist.Text);
                          //Encoding2use := inclist.Values['encoding'];
                          inclist.Free;
                          if Encoding2use = '' then Encoding2use := 'system';
                          LogDatei.log_prog('Will Include : '+incfilename+' with encoding: '+Encoding2use,LLDebug);
                          assignfile(incfile,fullincfilename);
                          reset(incfile);
                          //script.Strings[i] := '';
                          alllines := 0;
                          inclines := 0;
                          inDefFunc2 := 0;
                          while not eof(incfile) do
                          begin
                            inc(alllines);
                            readln(incfile, incline);
                            LogDatei.log_prog('Found line in lib file (raw): '+incline,LLDebug3);
                            incline := reencode(incline, Encoding2use,usedEncoding);
                            LogDatei.log_prog('Found line in lib file (reencoded): '+incline,LLDebug2);
                            for constcounter := 1 to ConstList.Count   do
                              if Sektion.replaceInLine(incline, Constlist.Strings [constcounter-1], ConstValuesList.Strings [constcounter-1], false,replacedline)
                              then incline := replacedline;


                            if importFunctionName <> '' then
                            begin
                              // we import only one function
                              GetWord (incline, Expressionstr, Remaining, WordDelimiterSet4);
                              // does the line starts with deffunc ?
                              if 'deffunc' = trim(lowercase(Expressionstr)) then
                              begin
                                inc(inDefFunc2);
                                // is it the searched function name ? ?
                                if (not inSearchedFunc)  and (inDefFunc2 = 1) then
                                begin
                                  GetWord (Remaining, Expressionstr, Remaining, WordDelimiterSet4);
                                  if lowercase(Expressionstr) = LowerCase(importFunctionName) then
                                  begin
                                    inSearchedFunc := true;
                                    secindex := Script.FSectionNameList.Add(importFunctionName);
                                    tmpint := length(script.FSectionInfoArray);
                                    if secindex <> tmpint then
                                      LogDatei.log('Error: internal: secindex <> length(script.FSectionInfoArray)',LLCritical);
                                    setlength(script.FSectionInfoArray, secindex+1);
                                    script.FSectionInfoArray[secindex].SectionName:=importFunctionName;
                                    script.FSectionInfoArray[secindex].SectionFile:=ExtractFileName(fullincfilename);
                                    script.FSectionInfoArray[secindex].StartLineNo:=alllines;
                                  end;
                                end
                              end;
                              if inSearchedFunc  then
                              begin
                                inc(inclines);
                                LogDatei.log_prog('Will Include line : '+incline,LLDebug);
                                Sektion.Insert(i-1+inclines,incline);
                                LogDatei.log_prog('Line included at pos: '+inttostr(i-1+inclines)+' to Sektion with '+inttostr(Sektion.Count)+' lines.',LLDebug2);
                                //LogDatei.log_prog('Will Include add at pos '+inttostr(Sektion.StartLineNo + i-1+k)+'to FLinesOriginList with count: '+inttostr(script.FLinesOriginList.Count),LLDebug2);
                                script.FLinesOriginList.Insert( i-1+inclines,incfilename+ ' Line: '+inttostr(alllines));
                                script.FLibList.Insert( i-1+inclines,'true');
                                LogDatei.log_prog('Include added to FLinesOriginList.',LLDebug2);
                              end;
                                // do we have an endfunc ?
                                //GetWord (incline, Expressionstr, Remaining, WordDelimiterSet4);
                              if 'endfunc' = trim(lowercase(Expressionstr)) then
                                  dec(inDefFunc2);
                              if inSearchedFunc and (inDefFunc2 = 0) then
                                  inSearchedFunc := false;
                            end  // import only one func
                            else
                            begin  // import all func
                              inc(inclines);
                              LogDatei.log_prog('Will Include line : '+incline,LLDebug);
                              Sektion.Insert(i-1+inclines,incline);
                              LogDatei.log_prog('Line included at pos: '+inttostr(i-1+inclines)+' to Sektion with '+inttostr(Sektion.Count)+' lines.',LLDebug2);
                              //LogDatei.log_prog('Will Include add at pos '+inttostr(Sektion.StartLineNo + i-1+k)+'to FLinesOriginList with count: '+inttostr(script.FLinesOriginList.Count),LLDebug2);
                              script.FLinesOriginList.Insert(i-1+inclines,incfilename+ ' Line: '+inttostr(alllines));
                              script.FLibList.Insert(i-1+inclines,'true');
                              LogDatei.log_prog('Include added to FLinesOriginList.',LLDebug2);
                            end
                          end;
                          closeFile(incfile);
                          linecount := Count;
                          if importFunctionName = '' then
                          begin
                            LogDatei.log('Imported all functions from file: '+fullincfilename,LLNotice);
                            tmplist := TXstringlist.Create;
                            tmplist.LoadFromFile(fullincfilename);
                            script.registerSectionOrigins(tmplist,fullincfilename);
                            tmplist.free;
                          end
                          else
                            LogDatei.log('Imported function : '+importFunctionName+' from file: '+fullincfilename ,LLNotice);
                          //for j:= 0 to sektion.Count -1 do
                          //  logdatei.log_prog('script: '+sektion.Strings[j],LLDebug);
                        end
                        else
                        begin
                          LogDatei.log('Error: Could not find import file :'+incfilename,LLCritical);
                          FExtremeErrorLevel:= levelFatal;
                        end;
                       except
                            on E: exception do
                           Begin
                              Logdatei.log ('importLib "' + Fname + '"', LLCritical);
                              Logdatei.log (' Failed to import (insert) file, system message: "' + E.Message +'"',
                                                     LLCritical);
                              FExtremeErrorLevel:= levelFatal;
                            End
                        end;
                    End
                    else
                      ActionResult
                      := reportError (Sektion, i, Sektion.strings [i-1], InfoSyntaxError);
                  end; // tsImportLib



                tsIncludeInsert:
                  begin
                    syntaxCheck := EvaluateString (Remaining, Remaining, FName, InfoSyntaxError);
                    if syntaxCheck then
                    Begin
                      try
                        fullincfilename := '';
                        incfilename := FName;
                        LogDatei.log('Found Include_insert statement for: '+incfilename,LLDebug);
                        found := false;
                        // full file path given
                        testincfilename := ExpandFilename(incfilename);
                        LogDatei.log_prog('Looking for: '+testincfilename,LLDebug2);
                        if FileExistsUTF8(testincfilename) then
                        begin
                          found := true;
                          fullincfilename := testincfilename;
                        end;
                        if (not found) then
                        begin
                          // search in %ScriptPath%
                          testincfilename := ExtractFileDir(FFilename)+PathDelim+incfilename;
                          testincfilename := ExpandFilename(testincfilename);
                          LogDatei.log_prog('Looking for: '+testincfilename,LLDebug2);
                          if FileExistsUTF8(testincfilename) then
                          begin
                            found := true;
                            fullincfilename := testincfilename;
                          end;
                        end;
                        {$IFDEF WINDOWS}
                        if (not found) then
                        begin
                          // search in %opsiScriptHelperPath%\lib
                          testincfilename := getSpecialFolder(CSIDL_PROGRAM_FILES)+'\opsi.org\opsiScriptHelper'
                                               +PathDelim+incfilename;
                          testincfilename := ExpandFilename(testincfilename);
                          LogDatei.log_prog('Looking for: '+testincfilename,LLDebug2);
                          if FileExistsUTF8(testincfilename) then
                          begin
                            found := true;
                            fullincfilename := testincfilename;
                          end;
                        end;
                        {$ENDIF WINDOWS}
                        if (not found) then
                        begin
                          // search in %ScriptDrive%\lib  aka %ScriptPath%/../lib
                          testincfilename := ExtractFileDir(FFilename)
                                              +PathDelim+'..'
                                              +PathDelim+'lib'+PathDelim+incfilename;
                          testincfilename := ExpandFilename(testincfilename);
                          LogDatei.log_prog('Looking for: '+testincfilename,LLDebug2);
                          if FileExistsUTF8(testincfilename) then
                          begin
                            found := true;
                            fullincfilename := testincfilename;
                          end;
                        end;
                        {$IFDEF WINDOWS}
                        if (not found) then
                        begin
                          // search in %WinstDir%\lib
                          testincfilename := ExtractFileDir(Paramstr(0))
                                               +PathDelim+'lib'+PathDelim+incfilename;
                          testincfilename := ExpandFilename(testincfilename);
                          LogDatei.log_prog('Looking for: '+testincfilename,LLDebug2);
                          if FileExistsUTF8(testincfilename) then
                          begin
                            found := true;
                            fullincfilename := testincfilename;
                          end;
                        end;
                        {$ENDIF WINDOWS}
                        if found then
                        begin
                          LogDatei.log('Found File: '+fullincfilename,LLDebug2);
                          inclist := TStringList.Create;
                          inclist.LoadFromFile(ExpandFileName(fullincfilename));
                          Encoding2use := searchencoding(inclist.Text);
                          //Encoding2use := inclist.Values['encoding'];
                          Script.registerSectionOrigins(inclist,fullincfilename);
                          inclist.Free;
                          if Encoding2use = '' then Encoding2use := 'system';
                          LogDatei.log('Will Include : '+incfilename+' with encoding: '+Encoding2use,LLDebug2);
                          assignfile(incfile,fullincfilename);
                          reset(incfile);
                          //script.Strings[i] := '';
                          k := 0;
                          while not eof(incfile) do
                          begin
                            inc(k);
                            readln(incfile, incline);
                            LogDatei.log_prog('Will Include line (raw): '+incline,LLDebug3);
                            incline := reencode(incline, Encoding2use,usedEncoding);
                            LogDatei.log_prog('Will Include line (reencoded): '+incline,LLDebug3);
                            for constcounter := 1 to ConstList.Count   do
                              if Sektion.replaceInLine(incline, Constlist.Strings [constcounter-1], ConstValuesList.Strings [constcounter-1], false,replacedline)
                              then incline := replacedline;
                            LogDatei.log_prog('Will Include line (constants replaced): '+incline,LLDebug3);
                            Sektion.Insert(i-1+k,incline);
                            LogDatei.log_prog('Line included at pos: '+inttostr(i-1+k)+' to Sektion with '+inttostr(Sektion.Count)+' lines.',LLDebug3);
                            //LogDatei.log('Will Include add at pos '+inttostr(Sektion.StartLineNo + i-1+k)+'to FLinesOriginList with count: '+inttostr(script.FLinesOriginList.Count),LLDebug3);
                            //script.FLinesOriginList.Insert(Sektion.StartLineNo + i-1+k,incfilename+ ' Line: '+inttostr(k));
                            //script.FLibList.Insert(Sektion.StartLineNo + i-1+k,'false');
                            script.FLinesOriginList.Insert(i-1+k,incfilename+ ' Line: '+inttostr(k));
                            script.FLibList.Insert(i-1+k,'false');
                            LogDatei.log_prog('Include added to FLinesOriginList.',LLDebug3);
                          end;
                          closeFile(incfile);
                          linecount := Count;
                          LogDatei.log('Included (insert) file: '+fullincfilename+' with encoding: '+usedEncoding,LLInfo);
                        end
                        else
                        begin
                          LogDatei.log('Error: Could not find include file :'+incfilename,LLCritical);
                          FExtremeErrorLevel:= levelFatal;
                        end;
                       except
                            on E: exception do
                           Begin
                              Logdatei.log ('Include_Insert "' + Fname + '"', LLCritical);
                              Logdatei.log (' Failed to include (insert) file, system message: "' + E.Message +'"',
                                                     LLCritical);
                              FExtremeErrorLevel:= levelFatal;
                            End
                        end;
                    End
                    else
                      ActionResult
                      := reportError (Sektion, i, Sektion.strings [i-1], InfoSyntaxError);
                  end;

                tsIncludeAppend:
                  begin
                    syntaxCheck := EvaluateString (Remaining, Remaining, FName, InfoSyntaxError);
                    if syntaxCheck then
                    Begin
                      try
                        incfilename := FName;
                        LogDatei.log('Found Include_append statement for: '+incfilename,LLDebug);
                        found := false;
                        // full file path given
                        testincfilename := ExpandFilename(incfilename);
                        LogDatei.log('Looking for: '+testincfilename,LLDebug2);
                        if FileExistsUTF8(testincfilename) then
                        begin
                          found := true;
                          fullincfilename := testincfilename;
                        end;
                        if (not found) then
                        begin
                          // search in %ScriptPath%
                          testincfilename := ExtractFileDir(FFilename)+PathDelim+incfilename;
                          testincfilename := ExpandFilename(testincfilename);
                          LogDatei.log('Looking for: '+testincfilename,LLDebug2);
                          if FileExistsUTF8(testincfilename) then
                          begin
                            found := true;
                            fullincfilename := testincfilename;
                          end;
                        end;
                        {$IFDEF WINDOWS}
                        if (not found) then
                        begin
                          // search in %opsiScriptHelperPath%\lib
                          testincfilename := getSpecialFolder(CSIDL_PROGRAM_FILES)+'\opsi.org\opsiScriptHelper'
                                               +PathDelim+incfilename;
                          testincfilename := ExpandFilename(testincfilename);
                          if FileExistsUTF8(testincfilename) then
                          begin
                            found := true;
                            fullincfilename := testincfilename;
                          end;
                        end;
                        {$ENDIF WINDOWS}
                        if (not found) then
                        begin
                          // search in %ScriptDrive%\lib  aka %ScriptPath%/../lib
                          testincfilename := ExtractFileDir(FFilename)
                                              +PathDelim+'..'
                                              +PathDelim+'lib'+PathDelim+incfilename;
                          testincfilename := ExpandFilename(testincfilename);
                          LogDatei.log('Looking for: '+testincfilename,LLDebug2);
                          if FileExistsUTF8(testincfilename) then
                          begin
                            found := true;
                            fullincfilename := testincfilename;
                          end;
                        end;
                        {$IFDEF WINDOWS}
                        if (not found) then
                        begin
                          // search in %WinstDir%\lib
                          testincfilename := ExtractFileDir(Paramstr(0))
                                               +PathDelim+'lib'+PathDelim+incfilename;
                          testincfilename := ExpandFilename(testincfilename);
                          LogDatei.log('Looking for: '+testincfilename,LLDebug2);
                          if FileExistsUTF8(testincfilename) then
                          begin
                            found := true;
                            fullincfilename := testincfilename;
                          end;
                        end;
                        {$ENDIF WINDOWS}
                        if found then
                        begin
                          LogDatei.log('Found File: '+fullincfilename,LLDebug2);
                          inclist := TStringList.Create;
                          inclist.LoadFromFile(ExpandFileName(fullincfilename));
                          Encoding2use := searchencoding(inclist.Text);
                          //Encoding2use := inclist.Values['encoding'];
                          Script.registerSectionOrigins(inclist,fullincfilename);
                          inclist.Free;
                          if Encoding2use = '' then Encoding2use := 'system';
                          linecount := count;
                          assignfile(incfile,fullincfilename);
                          reset(incfile);
                          //script.Strings[i] := '';
                          k:=0;
                          while not eof(incfile) do
                          begin
                            inc(k);
                            readln(incfile, incline);
                            incline := reencode(incline, Encoding2use,usedEncoding);
                            for constcounter := 1 to ConstList.Count   do
                              if Sektion.replaceInLine(incline, Constlist.Strings [constcounter-1], ConstValuesList.Strings [constcounter-1], false,replacedline)
                              then incline := replacedline;
                            LogDatei.log_prog('Include_append line: '+incline,LLDebug);
                            append(incline);
                            linecount := Count;
                            script.FLinesOriginList.Append(incfilename+ ' Line: '+inttostr(k));
                            script.FLibList.Append('false');
                          end;
                          closeFile(incfile);
                          //linecount := Count;
                          LogDatei.log('Included (append) file: '+fullincfilename+' with encoding: '+usedEncoding,LLInfo);
                        end
                        else
                        begin
                          LogDatei.log('Error: Could not find include file :'+incfilename,LLCritical);
                          FExtremeErrorLevel:= levelFatal;
                        end;
                       except
                            on E: exception do
                           Begin
                              Logdatei.log ('Include_Append "' + Fname + '"', LLCritical);
                              Logdatei.log (' Failed to include (append) file, system message: "' + E.Message +'"',
                                                     LLCritical);
                              FExtremeErrorLevel:= levelFatal;
                            End
                        end;
                    End
                    else
                      ActionResult
                      := reportError (Sektion, i, Sektion.strings [i-1], InfoSyntaxError);
                  end;


                tsIncludeLog:
                  begin
                    logtailEncodingstr := 'system';
                    syntaxCheck := EvaluateString (Remaining, Remaining, FName, InfoSyntaxError);
                    if syntaxCheck and (Remaining <> '') then
                             syntaxCheck := EvaluateString (Remaining, Remaining, logtailLinecountstr, InfoSyntaxError);
                    if syntaxCheck and (Remaining <> '') then
                             syntaxCheck := EvaluateString (Remaining, Remaining, logtailEncodingstr, InfoSyntaxError);
                    if syntaxCheck then
                    Begin
                      try
                        logtailLinecount := StrToInt(logtailLinecountstr);
                        logdatei.includelogtail(fname,logtailLinecount,logtailEncodingstr);
                       except
                            on E: exception do
                           Begin
                              Logdatei.log ('IncludeLogFile "' + Fname + '"', LLwarning);
                              Logdatei.log (' Failed to include log file, system message: "' + E.Message +'"',
                                                     LLwarning);
                            End
                        end;
                    End
                    else
                      ActionResult
                      := reportError (Sektion, i, Sektion.strings [i-1], InfoSyntaxError);
                  end;

                tsShrinkFileToMB:
                  begin
                    syntaxCheck := EvaluateString (Remaining, Remaining, FName, InfoSyntaxError);
                    if syntaxCheck and (Remaining <> '') then
                             syntaxCheck := EvaluateString (Remaining, Remaining, logtailLinecountstr, InfoSyntaxError);
                    if syntaxCheck then
                    Begin
                      try
                        logtailLinecount := StrToInt(logtailLinecountstr);
                        shrinkFileToMB(fname,logtailLinecount);
                       except
                            on E: exception do
                           Begin
                              Logdatei.log ('shrinkFileToMB "' + Fname + '"', LLwarning);
                              Logdatei.log (' Failed to shrink file, system message: "' + E.Message +'"',
                                                     LLwarning);
                            End
                        end;
                    End
                    else
                      ActionResult
                      := reportError (Sektion, i, Sektion.strings [i-1], InfoSyntaxError);
                  end;


              tsShowBitmap:
                 Begin
                   SyntaxCheck := true;

                   if PathDelim <> '/' then
                   begin
                     posSlash := pos ('/', Remaining);
                     if posSlash > 0
                     then
                     Begin
                        if    (length (Remaining) >= posSlash  + 1)
                          and (Remaining[posSlash + 1] in ['1'..'9'])
                        then
                        Begin
                          imageNo := ord (Remaining[posSlash + 1]) - ord('0');

                          if (imageNo > 4)
                          or  (length (Remaining) >= posSlash  + 2)
                           and (Remaining[posSlash + 2] in ['0'..'9'])
                          then
                          Begin
                            SyntaxCheck := false;
                            InfoSyntaxError := 'only supported 1 .. 4 ';
                          End;

                          system.delete (Remaining, posSlash, 2);
                        End
                        else
                        Begin
                          SyntaxCheck := false;
                          InfoSyntaxError := 'after "/", a number between 1 and 9 is expected';
                        End
                     End
                     else
                       {$IFDEF GUI}
                       imageNo := centralImageNo;
                       {$ENDIF GUI}
                   end
                   else
                       {$IFDEF GUI}
                       imageNo := centralImageNo;
                      {$ENDIF GUI}

                   if syntaxCheck
                   then
                   Begin
                     Remaining := cutLeftBlanks (Remaining);

                     Labeltext := '';
                     FName := '';

                     if Remaining = ''
                     then
                     Begin
                     (*
                      // set back to default
                       if imageNo <= wibtch.maxBitmaps
                       then
                       Begin
                         Labeltext := wibtch.labelTexts [imageNo];
                         FName := wibtch.bitmapFnames [imageNo];
                       End
                     *)
                     End

                     else
                     Begin
                        syntaxCheck := EvaluateString (Remaining, Remaining, FName, InfoSyntaxError);

                        if syntaxCheck and (Remaining <> '')
                        then
                           syntaxCheck := EvaluateString (Remaining, Remaining, Labeltext, InfoSyntaxError)
                     End;
                   End;

                   if syntaxCheck
                   then
                   begin
                     if FName <> '' then FName := ExpandFileName(FName);
                     {$IFDEF GUI}
                     FBatchOberflaeche.setPicture (FName, Labeltext);
                     {$ENDIF GUI}
                   end
                   else
                     ActionResult
                     := reportError (Sektion, i, Sektion.strings [i-1], InfoSyntaxError);

                end;

              tsChangeDirectory:
                Begin
                  syntaxCheck := false;
                  if skip ('=', remaining, Parameter, InfoSyntaxError)
                  then syntaxCheck := true
                  else if EvaluateString (Remaining, Remaining, Parameter, InfoSyntaxError)
                  then syntaxCheck := true;

                  if syntaxCheck then
                  begin
                     Parameter := ExpandFileName(Parameter);
                     if DirectoryExists(Parameter) then
                       ChangeDirectory(parameter)
                     else
                       LogDatei.log ('Error at ChangeDirectory: '+Parameter+ ' is not a directory', LLError);
                  end;
                End;


              tsPause:
                 Begin
                  syntaxCheck := false;
                  if skip ('=', remaining, Parameter, InfoSyntaxError)
                  then syntaxCheck := true
                  else if EvaluateString (Remaining, Remaining, Parameter, InfoSyntaxError)
                  then syntaxCheck := true;

                  if syntaxCheck
                  then
                  Begin
                    {$IFDEF GUI}
                    if messagedlg (Parameter + LineEnding + rsReadyToContinue, mtConfirmation, [mbYes], 0)
                     = mrNo
                    then
                      ActionResult := tsrExitProcess;
                    {$ENDIF GUI}
                  End
                  else
                    ActionResult
                    := reportError (Sektion, i, Sektion.strings [i-1], InfoSyntaxError);
                End;

                tsSleep:
                 Begin
                  syntaxCheck := false;
                  processmess;

                  try
                    try
                      // may be just a number (without quotes)
                      sleepSecs := StrToInt (Remaining);
                      syntaxCheck := true;
                    except
                       // it could be also a string Expressionstr
                      if EvaluateString (Remaining, Remaining, Parameter, InfoSyntaxError)
                      then syntaxCheck := true;
                      // which should contain a number
                      try
                        if syntaxCheck then sleepSecs := StrToInt (Parameter);
                      except ;
                        syntaxCheck := false;
                        InfoSyntaxError := remaining + ' -- expected an integer (number of secs) ';
                      end;
                    end;
                  except
                    syntaxCheck := false;
                    InfoSyntaxError := remaining + ' -- expected an integer (number of secs) ';
                  end;

                  if syntaxCheck then
                  begin
                    LogDatei.log('sleep '+IntToStr(sleepSecs)+' seconds...',LLDebug2);
                    Sleep(1000 * sleepSecs);
                  end
                  else
                    ActionResult
                    := reportError (Sektion, i, Sektion.strings [i-1], InfoSyntaxError);

                  processmess;

                End;

             tsStop:
                 Begin
                  syntaxCheck := false;
                  if skip ('=', remaining, Parameter, InfoSyntaxError)
                  then syntaxCheck := true
                  else if EvaluateString (Remaining, Remaining, Parameter, InfoSyntaxError)
                  then syntaxCheck := true;

                  if syntaxCheck
                  then
                  Begin
                    {$IFDEF GUI}
                    if messagedlg (Parameter + LineEnding + rsAbortProgram, mtConfirmation, [mbYes, mbNo], 0)
                     = mrYes
                    then
                      ActionResult := tsrExitProcess;
                    {$ELSE GUI}
                      ActionResult := tsrExitProcess;
                    {$ENDIF GUI}
                  End
                  else
                    ActionResult
                    := reportError (Sektion, i, Sektion.strings [i-1], InfoSyntaxError);
                End;


              tsComment:
                Begin
                  syntaxCheck := false;
                  if skip ('=', remaining, Parameter, InfoSyntaxError)
                  then syntaxCheck := true
                  else if EvaluateString (Remaining, Remaining, Parameter, InfoSyntaxError)
                  then syntaxCheck := true;

                  if syntaxCheck
                  then
                     LogDatei.log ('comment: ' + Parameter, LLnotice);
                End;

              tsActionProgress:
                Begin
                  syntaxCheck := false;
                  if skip ('=', remaining, Parameter, InfoSyntaxError)
                  then syntaxCheck := true
                  else if EvaluateString (Remaining, Remaining, Parameter, InfoSyntaxError)
                  then syntaxCheck := true;
                  if syntaxCheck
                  then
                     LogDatei.log ('set ActionProgress to: ' + Parameter, LLInfo);
                     opsidata.setActionProgress(Parameter);
                End;


              tsLogWarning:
                Begin
                  syntaxCheck := false;
                  if skip ('=', remaining, Parameter, InfoSyntaxError)
                  then syntaxCheck := true
                  else if EvaluateString (Remaining, Remaining, Parameter, InfoSyntaxError)
                  then syntaxCheck := true
                  else InfoSyntaxError := 'Info string expected';

                  if syntaxCheck
                  then
                  Begin
                     LogDatei.log ('Warning: ' + Parameter, LLwarning);
                     FNumberOfWarnings := FNumberOfWarnings + 1;
                  end
                  else
                   ActionResult
                    := reportError (Sektion, i, Sektion.strings [i-1], InfoSyntaxError);

                End;

              tsLogError:
                Begin
                  syntaxCheck := false;
                  if skip ('=', remaining, Parameter, InfoSyntaxError)
                  then syntaxCheck := true
                  else if EvaluateString (Remaining, Remaining, Parameter, InfoSyntaxError)
                  then syntaxCheck := true
                  else InfoSyntaxError := 'Info string expected';

                  if syntaxCheck
                  then
                  Begin
                     LogDatei.log ('Error: ' + Parameter, LLerror);
                     FNumberOfErrors := FNumberOfErrors + 1;
                  End
                  else
                   ActionResult
                    := reportError (Sektion, i, Sektion.strings [i-1], InfoSyntaxError);
                End;

               tsSetSkinDir:
                 Begin
                  syntaxCheck := false;
                  if EvaluateString (Remaining, Remaining, Parameter, InfoSyntaxError)
                  then syntaxCheck := true
                  else InfoSyntaxError := 'Expected a string (directory path)';

                  if syntaxCheck
                  then
                  Begin
                    if Parameter <> '' then FName := ExpandFileName(Parameter);
                    {$IFDEF GUI}
                    FBatchOberflaeche.loadSkin(Parameter);
                    processMess;
                    {$ENDIF GUI}
                  End
                  else
                   ActionResult
                    := reportError (Sektion, i, Sektion.strings [i-1], InfoSyntaxError);
                End;


               tsKillTask:
                 Begin
                   syntaxCheck := false;
                   if EvaluateString (Remaining, Remaining, Fname, InfoSyntaxError)
                   then
                   Begin
                     syntaxCheck := true;
                     {$IFNDEF WIN64}
                     if KillTask (Fname, Info)
                     then
                     Begin
                       LogDatei.Log (info, LLInfo);
                     End
                     else
                     Begin
                       LogDatei.log('"'+ Fname + '" could not be killed, ' + info, LLWarning );
                     End
                     {$ELSE WIN64}
                     LogDatei.Log ('Not implemnted on win64', LLWarning);
                     {$ENDIF WIN64}
                  End
                  else
                     ActionResult := reportError (Sektion, i, Expressionstr, InfoSyntaxError);
                 End;

                 tsShellcall:
                 begin
                  if Skip ('(', Remaining, Remaining, InfoSyntaxError)
                  then
                   if EvaluateString (Remaining, Remaining, s1, InfoSyntaxError)
                   then
                     if Skip (')', Remaining,Remaining, InfoSyntaxError)
                     then
                     Begin
                       syntaxCheck := true;
                       try
                         //LogDatei.log ('Executing0 ' + s1, LLInfo);
                         dummylist := execShellCall(s1, 'sysnative',0, true);
                       except
                         on e: exception do
                         begin
                           LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 2;
                           LogDatei.log('Error executing :'+s1+' : '+ e.message,
                             LLError);
                           FNumberOfErrors := FNumberOfErrors + 1;
                           LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 2;
                         end;
                       end;
                     End;
                 end;


               tsPowershellcall:
               begin
                 {$IFDEF Linux}
                  LogDatei.log('Error powershellcall not implemented on Linux ', LLError);
                  {$ENDIF Linux}
                  {$IFDEF WINDOWS}
                 s2 := '';
                 s3 := '';
                 tmpstr2 := '';
                 tmpbool := true; // sysnative
                 tmpbool1 := true; // handle execution policy
                 syntaxCheck := false;
                if Skip ('(', Remaining, Remaining, InfoSyntaxError)
                then if EvaluateString (Remaining, tmpstr, s1, InfoSyntaxError)
                // next after ',' or ')'
                then if Skip (',', tmpstr, tmpstr1, tmpstr3) then
                 if EvaluateString (tmpstr1, tmpstr2, s2, tmpstr3) then;
                if s2 = '' then
                begin
                 // only one parameter
                 if Skip (')', tmpstr, Remaining, InfoSyntaxError) then
                 Begin
                   syntaxCheck := true;
                   s2 := 'sysnative';
                 end;
                end
                else
                begin
                 // got second parameter
                  tmpbool := true;
                 if lowercase(s2) = '32bit' then tmpbool := false
                 else if lowercase(s2) = '64bit' then tmpbool := true
                 else if lowercase(s2) = 'sysnative' then tmpbool := true
                 else
                 begin
                   InfoSyntaxError := 'Error: unknown parameter: '+s2+' expected one of 32bit,64bit,sysnative - fall back to sysnative';
                   syntaxCheck := false;
                 end;
                 // three parameter ?
                 if Skip (',', tmpstr2, tmpstr1, tmpstr3) then
                 begin
                   if EvaluateString (tmpstr1, tmpstr2, s3, tmpstr3) then
                   begin
                    // got third parameter
                     if Skip (')', tmpstr2, Remaining, InfoSyntaxError) then
                     Begin
                       if TryStrToBool(s3,tmpbool1) then
                         syntaxCheck := true
                       else
                       begin
                         syntaxCheck := false;
                         InfoSyntaxError := 'Error: boolean string (true/false) expected but got: '+s3;
                       end;
                     end;
                   end;
                 end
                 else
                 if Skip (')', tmpstr2, Remaining, InfoSyntaxError) then
                 Begin
                  // two parameter
                   syntaxCheck := true;
                 end;
                end;
                if syntaxCheck then
                 begin
                   try
                     execPowershellCall(s1, s2,0, true,false,tmpbool1);
                   except
                     on e: exception do
                     begin
                       LogDatei.log('Error executing :'+s1+' : with powershell: '+ e.message,
                         LLError);
                     end
                   end;
                 end;
                {$ENDIF WINDOWS}
               end;




              {$IFDEF WIN32}
              tsBlockInput:
                Begin
                  syntaxCheck := false;
                  if EvaluateString (Remaining, Remaining, Parameter, InfoSyntaxError) then
                  Begin
                    if UpperCase (Parameter) = UpperCase ('True')
                    then
                    Begin
                      syntaxCheck := true;
                      ActionResult := tsrPositive;
                      if winBlockInput(true) then LogDatei.log ('Blocking Input', LLInfo)
                      else LogDatei.log ('Failed Blocking Input ...', LLWarning);
                    End

                    else if UpperCase (Parameter) = UpperCase ('False')
                    then
                    Begin
                      syntaxCheck := true;
                      ActionResult := tsrPositive;
                      if winBlockInput(false) then LogDatei.log ('Unblocking Input', LLInfo)
                      else LogDatei.log ('Failed Unblocking Input ...', LLWarning);
                    End;
                  end;
                  if not syntaxCheck then
                  ActionResult
                      := reportError (Sektion, i, Sektion.strings [i-1], 'Expected a boolean string (true/false)');
                  if not (Remaining = '')  then  LogDatei.log ('Error at Blockinput: trailing parameter: '+Parameter+ ' ignored', LLWarning);
                End;


              tsLocalAdmin:
                Begin
                  if UpperCase (Remaining) = UpperCase ('/Create1')
                  then
                  Begin
                    LogDatei.log ('Creating temporary local admin ...', LLNotice);
                    ActionResult := tsrPositive;
                    if CreateTemporaryLocalAdmin(traAdmin) then
                    begin
                      LogDatei.log ('Created temporary local admin ...', LLInfo);

                      FConstValuesList.Strings[FConstList.IndexOf('%AppDataDir%')] := GetAppDataPath;
                      FConstValuesList.Strings[FConstList.IndexOf('%CurrentAppDataDir%')] := GetAppDataPath;
                      FConstValuesList.Strings[FConstList.IndexOf('%CurrentStartmenuDir%')] := GetStartmenuPath;
                      FConstValuesList.Strings[FConstList.IndexOf('%CurrentDesktopDir%')] := GetDesktopPath;
                      FConstValuesList.Strings[FConstList.IndexOf('%CurrentStartupDir%')] := GetStartupPath;
                      FConstValuesList.Strings[FConstList.IndexOf('%CurrentProgramsDir%')] := GetProgramsPath;
                      FConstValuesList.Strings[FConstList.IndexOf('%CurrentSendToDir%')] := GetSendToPath;
                      FConstValuesList.Strings[FConstList.IndexOf('%CurrentProfileDir%')] := getUserProfilePath;
                      FConstValuesList.Strings[FConstList.IndexOf('%AppDataDir%')] := GetAppDataPath;
                      FConstValuesList.Strings[FConstList.IndexOf('%AppDataDir%')] := GetAppDataPath;

                      ApplyTextConstants (TXStringList (ArbeitsSektion), false);
                    end
                    else LogDatei.log ('Failed creating temporary local admin ...', LLWarning);
                  End

                  else if UpperCase (Remaining) = UpperCase ('/Create2')
                  then
                  Begin
                    LogDatei.log ('Creating temporary local admin ...', LLInfo);
                    ActionResult := tsrPositive;
                    if CreateTemporaryLocalAdmin(traAdminProfile) then
                    begin
                      LogDatei.log ('Created temporary local admin ...', LLNotice);

                      FConstValuesList.Strings[FConstList.IndexOf('%AppDataDir%')] := GetAppDataPath;
                      FConstValuesList.Strings[FConstList.IndexOf('%CurrentAppDataDir%')] := GetAppDataPath;
                      FConstValuesList.Strings[FConstList.IndexOf('%CurrentStartmenuDir%')] := GetStartmenuPath;
                      FConstValuesList.Strings[FConstList.IndexOf('%CurrentDesktopDir%')] := GetDesktopPath;
                      FConstValuesList.Strings[FConstList.IndexOf('%CurrentStartupDir%')] := GetStartupPath;
                      FConstValuesList.Strings[FConstList.IndexOf('%CurrentProgramsDir%')] := GetProgramsPath;
                      FConstValuesList.Strings[FConstList.IndexOf('%CurrentSendToDir%')] := GetSendToPath;
                      FConstValuesList.Strings[FConstList.IndexOf('%CurrentProfileDir%')] := getUserProfilePath;
                      FConstValuesList.Strings[FConstList.IndexOf('%AppDataDir%')] := GetAppDataPath;
                      FConstValuesList.Strings[FConstList.IndexOf('%AppDataDir%')] := GetAppDataPath;

                      ApplyTextConstants (TXStringList (ArbeitsSektion), false);
                    end
                    else LogDatei.log ('Failed creating temporary local admin ...', LLWarning);
                  End

                  else if UpperCase (Remaining) = UpperCase ('/Create3')
                  then
                  Begin
                    LogDatei.log ('Creating temporary local admin ...', LLInfo);
                    ActionResult := tsrPositive;
                    if CreateTemporaryLocalAdmin(traAdminProfileImpersonate) then
                    begin
                      LogDatei.log ('Created temporary local admin ...', LLNotice);

                      FConstValuesList.Strings[FConstList.IndexOf('%AppDataDir%')] := GetAppDataPath;
                      FConstValuesList.Strings[FConstList.IndexOf('%CurrentAppDataDir%')] := GetAppDataPath;
                      FConstValuesList.Strings[FConstList.IndexOf('%CurrentStartmenuDir%')] := GetStartmenuPath;
                      FConstValuesList.Strings[FConstList.IndexOf('%CurrentDesktopDir%')] := GetDesktopPath;
                      FConstValuesList.Strings[FConstList.IndexOf('%CurrentStartupDir%')] := GetStartupPath;
                      FConstValuesList.Strings[FConstList.IndexOf('%CurrentProgramsDir%')] := GetProgramsPath;
                      FConstValuesList.Strings[FConstList.IndexOf('%CurrentSendToDir%')] := GetSendToPath;
                      FConstValuesList.Strings[FConstList.IndexOf('%CurrentProfileDir%')] := getUserProfilePath;
                      FConstValuesList.Strings[FConstList.IndexOf('%AppDataDir%')] := GetAppDataPath;
                      FConstValuesList.Strings[FConstList.IndexOf('%AppDataDir%')] := GetAppDataPath;

                      ApplyTextConstants (TXStringList (ArbeitsSektion), false);
                    end
                    else LogDatei.log ('Failed creating temporary local admin ...', LLWarning);
                  End

                  else if UpperCase (Remaining) = UpperCase ('/Create4')
                  then
                  Begin
                    LogDatei.log ('Creating temporary local admin ...', LLInfo);
                    ActionResult := tsrPositive;
                    if CreateTemporaryLocalAdmin(traAdminProfileImpersonateExplorer) then
                    begin
                      LogDatei.log ('Created temporary local admin ...', LLNotice);

                      FConstValuesList.Strings[FConstList.IndexOf('%AppDataDir%')] := GetAppDataPath;
                      FConstValuesList.Strings[FConstList.IndexOf('%CurrentAppDataDir%')] := GetAppDataPath;
                      FConstValuesList.Strings[FConstList.IndexOf('%CurrentStartmenuDir%')] := GetStartmenuPath;
                      FConstValuesList.Strings[FConstList.IndexOf('%CurrentDesktopDir%')] := GetDesktopPath;
                      FConstValuesList.Strings[FConstList.IndexOf('%CurrentStartupDir%')] := GetStartupPath;
                      FConstValuesList.Strings[FConstList.IndexOf('%CurrentProgramsDir%')] := GetProgramsPath;
                      FConstValuesList.Strings[FConstList.IndexOf('%CurrentSendToDir%')] := GetSendToPath;
                      FConstValuesList.Strings[FConstList.IndexOf('%CurrentProfileDir%')] := getUserProfilePath;
                      FConstValuesList.Strings[FConstList.IndexOf('%AppDataDir%')] := GetAppDataPath;
                      FConstValuesList.Strings[FConstList.IndexOf('%AppDataDir%')] := GetAppDataPath;

                      ApplyTextConstants (TXStringList (ArbeitsSektion), false);
                    end
                    else LogDatei.log ('Failed creating temporary local admin ...', LLWarning);
        End

        else if UpperCase (Remaining) = UpperCase ('/Delete')
        then
        Begin
          LogDatei.log ('Deleting temporary local admin ...', LLInfo);
                    ActionResult := tsrPositive;
                    if opsiSetupAdmin_created then
                      if DeleteTemporaryLocalAdmin then
                      begin
                        LogDatei.log ('Deleted temporary local admin ...', LLNotice);

                        FConstValuesList.Strings[FConstList.IndexOf('%AppDataDir%')] := GetAppDataPath;
                        FConstValuesList.Strings[FConstList.IndexOf('%CurrentAppDataDir%')] := GetAppDataPath;
                        FConstValuesList.Strings[FConstList.IndexOf('%CurrentStartmenuDir%')] := GetStartmenuPath;
                        FConstValuesList.Strings[FConstList.IndexOf('%CurrentDesktopDir%')] := GetDesktopPath;
                        FConstValuesList.Strings[FConstList.IndexOf('%CurrentStartupDir%')] := GetStartupPath;
                        FConstValuesList.Strings[FConstList.IndexOf('%CurrentProgramsDir%')] := GetProgramsPath;
                        FConstValuesList.Strings[FConstList.IndexOf('%CurrentSendToDir%')] := GetSendToPath;
                        FConstValuesList.Strings[FConstList.IndexOf('%CurrentProfileDir%')] := getUserProfilePath;
                        FConstValuesList.Strings[FConstList.IndexOf('%AppDataDir%')] := GetAppDataPath;
                        FConstValuesList.Strings[FConstList.IndexOf('%AppDataDir%')] := GetAppDataPath;

                        ApplyTextConstants (TXStringList (ArbeitsSektion), false);
                      end
                      else LogDatei.log ('Failed deleting temporary local admin ...', LLWarning);
                  End
                  else if Remaining = ''
                  then
                    ActionResult
                      := reportError (Sektion, i, Sektion.strings [i-1], 'Parameter needed')
                  else
                    ActionResult
                      := reportError (Sektion, i, Sektion.strings [i-1], 'not an allowed Parameter');
                End;
                {$ENDIF WIN32}


              tsExitWindows:
                Begin
                  if UpperCase (Remaining) = UpperCase ('/ImmediateReboot')
                  then
                  Begin
                    PerformExitWindows := txrImmediateReboot;
                    ActionResult := tsrExitWindows;
                    scriptstopped := true;
                    LogDatei.log ('ExitWindows set to Immediate Reboot', BaseLevel);
                  End
                  else if UpperCase (Remaining) = UpperCase ('/ImmediateLogout')
                  then
                  Begin
                    PerformExitWindows := txrImmediateLogout;
          LogDatei.log ('', BaseLevel);
                    ActionResult := tsrExitWindows;
                    scriptstopped := true;
                    LogDatei.log ('ExitWindows set to Immediate Logout', BaseLevel);
                  End
                  else if UpperCase (Remaining) = UpperCase ('/Reboot')
                  then
                  Begin
                    PerformExitWindows := txrReboot;
          LogDatei.log ('', BaseLevel);
                    LogDatei.log ('ExitWindows set to Reboot', BaseLevel);
                  End
                  else if UpperCase (Remaining) = UpperCase ('/RebootWanted')
                  then
                  Begin
                    if PerformExitWindows < txrRegisterForReboot
                    then
                    Begin
                      PerformExitWindows := txrRegisterForReboot;
            LogDatei.log ('', BaseLevel);
                      LogDatei.log ('ExitWindows set to RegisterReboot', BaseLevel);
                    End
                    else
                      LogDatei.log ('ExitWindows already set to Reboot', BaseLevel);
                  End
                  else if UpperCase (Remaining) = UpperCase ('/LogoutWanted')
                  then
                  Begin
                    if PerformExitWindows < txrRegisterForLogout
                    then
                    Begin
                      PerformExitWindows := txrRegisterForLogout;
                      LogDatei.log ('', BaseLevel);
                      LogDatei.log ('ExitWindows set to RegisterForLogout', BaseLevel);
                    End
                    else
                      LogDatei.log ('ExitWindows already set to (Register)Reboot', BaseLevel);
                  End

          else if UpperCase (Remaining) = UpperCase ('/ShutdownWanted')
                  then
                  Begin
                    PerformShutdown := tsrRegisterForShutdown;
                    LogDatei.log ('', BaseLevel);
                    LogDatei.log ('PerformShutdown set to RegisterForShutdown', BaseLevel);
                  End

                  else if Remaining = ''
                  then
                    ActionResult
                      := reportError (Sektion, i, Sektion.strings [i-1], 'Parameter needed')
                  else
                    ActionResult
                      := reportError (Sektion, i, Sektion.strings [i-1], 'not an allowed Parameter');
                End;

                tsAutoActivityDisplay:
                  if skip ('=', remaining, remaining, InfoSyntaxError)
                  then
                  Begin
                     if   UpperCase (Remaining) = 'TRUE' then
                     begin
                       LogDatei.log ('AutoActivityDisplay was '+BoolToStr(AutoActivityDisplay,true)+' is set to true', LLInfo);
                       AutoActivityDisplay := true;
                     end
                     else
                     begin
                       LogDatei.log ('AutoActivityDisplay was '+BoolToStr(AutoActivityDisplay,true)+' is set to false', LLInfo);
                       AutoActivityDisplay := false;
                     end;
                  End
                  else
                    ActionResult
                    := reportError (Sektion, i, Sektion.strings [i-1], InfoSyntaxError);


            tsFatalOnSyntaxError:
              if skip ('=', remaining, remaining, InfoSyntaxError)
              then
              Begin
                 if   UpperCase (Remaining) = 'TRUE' then
                 begin
                   LogDatei.log ('FatalOnSyntaxError was '+BoolToStr(FatalOnSyntaxError,true)+' is set to true', LLInfo);
                   FatalOnSyntaxError := true;
                 end
                 else
                 begin
                   LogDatei.log ('FatalOnSyntaxError was '+BoolToStr(FatalOnSyntaxError,true)+' is set to false', LLInfo);
                   FatalOnSyntaxError := false;
                   if (FExtremeErrorLevel=LevelFatal) and (LogDatei.ActionProgress='Syntax Error') then
                     FExtremeErrorLevel := Level_not_initialized;
                 end;
              End
              else
                ActionResult
                := reportError (Sektion, i, Sektion.strings [i-1], InfoSyntaxError);

            tsFatalOnRuntimeError:
              if skip ('=', remaining, remaining, InfoSyntaxError)
              then
              Begin
                 if   UpperCase (Remaining) = 'TRUE' then
                 begin
                   LogDatei.log('FatalOnRuntimeError was '+BoolToStr(FatalOnRuntimeError,true)+' is set to true', LLInfo);
                   FatalOnRuntimeError := true;
                 end
                 else
                 begin
                   LogDatei.log('FatalOnRuntimeError was '+BoolToStr(FatalOnRuntimeError,true)+' is set to false', LLInfo);
                   FatalOnRuntimeError := false;
                 end;
              End
              else
                ActionResult
                := reportError (Sektion, i, Sektion.strings [i-1], InfoSyntaxError);

              tsSetExitOnError:
                if skip ('=', remaining, remaining, InfoSyntaxError)
                then
                Begin
                   if   UpperCase (Remaining) = 'TRUE' then
                   begin
                    LogDatei.log ('ExitOnError was '+BoolToStr(ExitOnError,true)+' is set to true', LLInfo);
                    ExitOnError := true;
                   end
                   else
                   begin
                     LogDatei.log ('ExitOnError was '+BoolToStr(ExitOnError,true)+' is set to false', LLInfo);
                     ExitOnError := false;
                   end;
                End
                else
                  ActionResult
                  := reportError (Sektion, i, Sektion.strings [i-1], InfoSyntaxError);

              tsSetFatalError:
                if remaining = ''
                then
                Begin
                   FExtremeErrorLevel := LevelFatal;
                   LogDatei.log('Error level set to fatal', LLCritical);
                   ActionResult := tsrFatalError;
                   scriptstopped := true;
                   NestLevel:= NestingLevel;
                   ActLevel:= NestLevel;
                End
                else
                begin
                  EvaluateString (remaining, remaining, Parameter, infosyntaxerror);
                  if remaining = '' then
                  begin
                    FExtremeErrorLevel := LevelFatal;
                   LogDatei.log('Error level set to fatal', LLCritical);
                    ActionResult := tsrFatalError;
                    LogDatei.ActionProgress:=Parameter;
                    scriptstopped := true;
                    //NestLevel:= NestingLevel;
                    //ActLevel:= NestLevel;
                  end
                  else
                      ActionResult
                      := reportError (Sektion, i, Sektion.strings [i-1], ' none or one parameter expected');
                end;

                tsSetSuccess:
                  if remaining = ''
                  then
                  Begin
                     LogDatei.log('Set: Exit Script as successful', LLnotice);
                     ActionResult := tsrExitProcess;
                     scriptstopped := true;
                     //NestLevel:= NestingLevel;
                     //ActLevel:= NestLevel;
                  End
                  else
                  begin
                        ActionResult
                        := reportError (Sektion, i, Sektion.strings [i-1], ' no parameter expected');
                  end;

                tsSetNoUpdate:
                  if remaining = ''
                  then
                  Begin
                     LogDatei.log('Set: Do not run Update script', LLnotice);
                     runUpdate := false;
                  End
                  else
                  begin
                        ActionResult
                        := reportError (Sektion, i, Sektion.strings [i-1], ' no parameter expected');
                  end;

              tsSetSuspended:
                if remaining = ''
                  then
                  Begin
                     LogDatei.log('Set: Stop script and leave switches unchanged', LLnotice);
                     runUpdate := false;
                     script.suspended:= true;
                     scriptstopped := true;
                     ActionResult := tsrExitProcess;
                     //NestLevel:= NestingLevel;
                     //ActLevel:= NestLevel;
                  End
                  else
                  begin
                        ActionResult
                        := reportError (Sektion, i, Sektion.strings [i-1], ' no parameter expected');
                  end;

              tsSetMarkerErrorNumber:
                if remaining = ''
                then
                Begin
                  LogDatei.ErrorNumberMarked := Logdatei.NumberOfErrors;
                  LogDatei.log('Marked error number '
                      + inttostr(LogDatei.ErrorNumberMarked),
                      levelcomplete);
                End
                else
                  ActionResult
                  := reportError (Sektion, i, Sektion.strings [i-1], ' end of line expected');

              tsSaveVersionToProfile:
                if remaining = ''
                then
                Begin
                  saveVersionToProfile;
                  LogDatei.log('Saved productversion-packageversion to local profile',LLNotice);
                End
                else
                  ActionResult
                  := reportError (Sektion, i, Sektion.strings [i-1], ' end of line expected');


              tsSetReportMessages:
                if skip ('=', remaining, remaining, InfoSyntaxError)
                then
                Begin
                   if    (UpperCase (Remaining) = 'OFF')
                      or (UpperCase (Remaining) = 'FALSE')
                   then
                   begin
                     LogDatei.log ('ScriptErrorMessages was '+BoolToStr(ReportMessages,true)+' is set to false', LLinfo);
                     ReportMessages := false;
                   end
                   else
                   begin
                     LogDatei.log ('ScriptErrorMessages was '+BoolToStr(ReportMessages,true)+' is set to true', LLinfo);
                     ReportMessages := true;
                   end;
                End
                else
                  ActionResult
                  := reportError (Sektion, i, Sektion.strings [i-1], InfoSyntaxError);

              tsSetTimeMark:
                Begin
                  markedTime :=  Time;

                  LogDatei.log(FormatDateTime('hh:mm:ss:zzz', markedTime)
                  + '  ============  Time marked ', baselevel);
                End;

              tsLogDiffTime:
                Begin
                  diffTime := Time - markedTime;

                  LogDatei.log(FormatDateTime('hh:mm:ss:zzz', diffTime)
                  + '  ============  Time passed since marked time  ', baselevel);
                End;


              tsSetTraceMode:
                if skip ('=', remaining, remaining, InfoSyntaxError)
                then
                Begin
                   if    (UpperCase (Remaining) = 'ON')
                      or (UpperCase (Remaining) = 'TRUE')
           then
                   Begin
                      TraceMode := true;
                      Logdatei.TraceMode := true;
                   end
                   else
                   begin
                      TraceMode := false;
                      Logdatei.TraceMode := false;
                   end
                End
                else
                  ActionResult
                  := reportError (Sektion, i, Sektion.strings [i-1], InfoSyntaxError);

              tsSetOldLogLevel:
                if skip ('=', remaining, remaining, InfoSyntaxError)
                then
                Begin
                  try
                    LogLevel := StrToInt (Remaining) + 4;
                    LogDatei.log ('Warning: depricated: <LogLevel=> please use <setLogLevel=> ', LLwarning);
                    LogDatei.log ('LogLevel was ' + inttoStr(LogDatei.LogLevel), LLessential);
                    LogDatei.LogLevel := Loglevel;
                    LogDatei.log ('LogLevel set to ' + inttoStr(LogLevel), LLessential);
                  except
                    try
                      EvaluateString (remaining, remaining, Parameter, infosyntaxerror);
                      LogLevel := StrToInt (Parameter) + 4;
                      LogDatei.log ('Warning: depricated: <LogLevel=> please use <setLogLevel=> ', LLwarning);
                      LogDatei.log ('LogLevel was ' + inttoStr(LogDatei.LogLevel), LLessential);
                      LogDatei.LogLevel := Loglevel;
                      LogDatei.log ('LogLevel set to ' + inttoStr(LogLevel), LLessential);
                    except
                      LogDatei.log ('Integer conversion error, LogLevel could not be set, it is ' + inttoStr(LogLevel), Logdatei.loglevel);
                      LogLevel := LLinfo;
                    end;
                  end
                End
                else
                  ActionResult := reportError (Sektion, i, Sektion.strings [i-1], InfoSyntaxError);


              tsSetLogLevel:
                if skip ('=', remaining, remaining, InfoSyntaxError)
                then
                Begin
                  try
                    LogLevel := StrToInt (Remaining);
                    LogDatei.log ('LogLevel was ' + inttoStr(LogDatei.LogLevel), LLinfo);
                    LogDatei.LogLevel := Loglevel;
                    LogDatei.log ('LogLevel set to ' + inttoStr(LogLevel), LLinfo);
                  except
                    try
                      EvaluateString (remaining, remaining, Parameter, infosyntaxerror);
                      LogLevel := StrToInt (Parameter);
                      LogDatei.log ('LogLevel was ' + inttoStr(LogDatei.LogLevel), LLinfo);
                      LogDatei.LogLevel := Loglevel;
                      LogDatei.log ('LogLevel set to ' + inttoStr(LogLevel), LLinfo);
                    except
                      LogDatei.log ('Integer conversion error, LogLevel could not be set, it is ' + inttoStr(LogLevel), Logdatei.loglevel);
                      LogLevel := LLinfo;
                    end;
                  end
                End
                else
                  ActionResult := reportError (Sektion, i, Sektion.strings [i-1], InfoSyntaxError);

              tsSetConfidential:
                 Begin
                  syntaxCheck := false;
                  if EvaluateString (Remaining, Remaining, Parameter, InfoSyntaxError)
                  then syntaxCheck := true
                  else InfoSyntaxError := 'Expected a string (confidential)';

                  if syntaxCheck
                  then
                  Begin
                    LogDatei.AddToConfidentials(Parameter);
                  End
                  else
                   ActionResult
                    := reportError (Sektion, i, Sektion.strings [i-1], InfoSyntaxError);
                End;

              tsSetUsercontext:
                if skip ('=', remaining, remaining, InfoSyntaxError)
                then
                Begin
                  EvaluateString (remaining, remaining, Parameter, infosyntaxerror);
                  LogDatei.log ('Usercontext was ' + usercontext, LLessential);
                  usercontext := Parameter;
                  LogDatei.log ('Usercontext set to ' + usercontext, LLessential);
                End
                else
                  ActionResult := reportError (Sektion, i, Sektion.strings [i-1], InfoSyntaxError);


              tsSetOutputLevel:
                if skip ('=', remaining, remaining, InfoSyntaxError)
                then
                Begin
                  try
                    OutputLevel := StrToInt (Remaining);
                    //LogDatei.log ('OutputLevel was ' + inttoStr(LogDatei.LogLevel), LLessential);
                    {$IFDEF GUI}
                    FBatchOberflaeche.setOutputLevel(OutputLevel);
                    {$ENDIF GUI}
                    LogDatei.log ('OutputLevel set to ' + inttoStr(OutputLevel), LLessential);
                  except
                    try
                      EvaluateString (remaining, remaining, Parameter, infosyntaxerror);
                      OutputLevel := StrToInt (Parameter);
                      //LogDatei.log ('OutputLevel was ' + inttoStr(LogDatei.LogLevel), LLessential);
                      {$IFDEF GUI}
                      FBatchOberflaeche.setOutputLevel(OutputLevel);
                      {$ENDIF GUI}
                      LogDatei.log ('OutputLevel set to ' + inttoStr(OutputLevel), LLessential);
                    except
                      LogDatei.log ('Integer conversion error, OutputLevel could not be set, it is ' + inttoStr(LogLevel), Logdatei.loglevel);
                      {$IFDEF GUI}
                      FBatchOberflaeche.setOutputLevel(3);
                      {$ENDIF GUI}
                    end;
                  end
                End
                else
                  ActionResult := reportError (Sektion, i, Sektion.strings [i-1], InfoSyntaxError);

              tsSetStayOnTop:
                 if skip ('=', remaining, remaining, InfoSyntaxError)
                 then
                 Begin
                   {$IFDEF GUI}
                    if    (UpperCase (Remaining) = 'ON')
                       or (UpperCase (Remaining) = 'TRUE')
                    then
                       FBatchOberflaeche.ForceStayOnTop (true)
                    else if
                          (UpperCase (Remaining) = 'OFF')
                       or (UpperCase (Remaining) = 'FALSE')
                    then
                       FBatchOberflaeche.ForceStayOnTop (false)
                    else
                      ActionResult := reportError (Sektion, i, Sektion.strings [i-1],
                                                   Remaining + ' is no valid value');
                   {$ENDIF GUI}
                 End
                 else
                   ActionResult
                   := reportError (Sektion, i, Sektion.strings [i-1], InfoSyntaxError);

              tsIconizeWinst:
                 {$IFDEF GUI}
                 begin
                   SavedBatchWindowMode := BatchWindowMode;
                   BatchWindowMode := bwmIcon;
                   FBatchOberflaeche.setWindowState (BatchWindowMode);
                 end
                 {$ENDIF GUI}
                 ;


              tsNormalizeWinst:
                 {$IFDEF GUI}
                 begin
                   SavedBatchWindowMode := BatchWindowMode;
                   BatchWindowMode := bwmNormalWindow;
                   FBatchOberflaeche.setWindowState (BatchWindowMode);
                 end
                 {$ENDIF GUI}
                 ;

              tsRestoreWinst:
                 {$IFDEF GUI}
                 begin
                   BatchWindowMode := SavedBatchWindowMode;
                   FBatchOberflaeche.setWindowState (BatchWindowMode);
                 end
                 {$ENDIF GUI}
                 ;

               tsMaximizeWinst:
                  {$IFDEF GUI}
                  begin
                    SavedBatchWindowMode := BatchWindowMode;
                    BatchWindowMode := bwmMaximized;
                    FBatchOberflaeche.setWindowState (BatchWindowMode);
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
                GetWordOrStringExpressionstr (Remaining, LppClientId, Remaining, InfoSyntaxError);
                GetWordOrStringExpressionstr (Remaining, LppProductId, Remaining, InfoSyntaxError);
                if ProductvarsForPC.Count = 0 then
                  LogDatei.log('Empty Property lookup table - try to load...',LLDebug)
                else LogDatei.log('Existing Property lookup table - try to reload...',LLDebug);
                if opsidata <> nil then
                begin
                 //LogDatei.log('Empty lookup table - try to get from opsidata',LLDebug);
                 opsidata.setActualClient(LppClientId);
                 opsidata.setActualProductName(LppProductId);
                 ProductvarsForPC := opsidata.getProductproperties;
                 LogDatei.log('Loaded lookup table from opsidata',LLDebug);
                end
                else if local_opsidata <> nil then
                begin
                 //LogDatei.log('Empty lookup table - try to get from local_opsidata',LLDebug);
                 local_opsidata.setActualClient(LppClientId);
                 local_opsidata.setActualProductName(LppProductId);
                 ProductvarsForPC := local_opsidata.getProductproperties;
                 LogDatei.log('Loaded lookup table from local_opsidata',LLDebug);
                end
                else
                begin
                 LogDatei.log('No service connection in LoadProductProperties - failed',LLWarning);
                end;
              end;

              tsWinstVersionRequired:
                 begin

                   GetWord (Remaining, expr, Remaining, WordDelimiterWhiteSpace);
                   GetWordOrStringExpressionstr (Remaining, numberString, Remaining, InfoSyntaxError);
                   // expr should be a comparison sign

                   if
                    getDecimalCompareSign (osconf.WinstVersion, numberString, sign, InfoSyntaxError, false)
                   then
                   Begin

                     hasRelation (expr, sign, evaluated, InfoSyntaxError);

                     if evaluated then
                     Begin
                       LogDatei.log
                        (osconf.selfProductName+' has version  ' + osconf.WinstVersion + ', required is : ' + expr + ' ' + numberString,
                         LLinfo);
                      LogDatei.WinstVersionRequired := numberString;
                     end
                     else
                     Begin

                       LogDatei.log
                        ('Error: wrong opsi-script version,  requested :     ' + expr + '  ' + numberString,
                         LLcritical);
                       FNumberOfErrors := FNumberOfErrors + 1;
                       FExtremeErrorLevel := LevelFatal;
                       LogDatei.log('Error level set to fatal', LLcritical);
                       ActionResult := tsrFatalError;
                     End

                   End
                   else
                   Begin
                     ActionResult
                     := reportError (Sektion, i, Sektion.strings [i-1], InfoSyntaxError);
                   end;
                 end;

              tsPatchAnyTextFile:
                begin
                   logdatei.log('Execution of: '+ArbeitsSektion.Name+' '+ Remaining,LLNotice);
                   flag_all_ntuser := false;
                   // if this is a 'ProfileActions' which is called as sub in Machine mode
                   // so run patches sections implicit as /Allntuserprofiles
                   if runProfileActions then flag_all_ntuser := true;
                   if Remaining = ''
                   then
                      ActionResult := reportError (Sektion, i, Sektion.strings [i-1], 'File parameter missing')
                   else
                   Begin
                     GetWordOrStringExpressionstr (Remaining, Filename, Remaining, ErrorInfo);
                     if Remaining = '' then
                       ActionResult := doTextpatch (ArbeitsSektion, Filename,'')
                     else ActionResult := doTextpatch (ArbeitsSektion, Filename, Remaining);
                   End;
                end;

              tsTests:
              ActionResult := doTests (ArbeitsSektion, Remaining);


              tsPatchIniFile:
                begin
                  logdatei.log('Execution of: '+ArbeitsSektion.Name+' '+ Remaining,LLNotice);
                  flag_all_ntuser := false;
                  flag_encoding := 'system';
                  // if this is a 'ProfileActions' which is called as sub in Machine mode
                  // so run patches sections implicit as /Allntuserprofiles
                  if runProfileActions then flag_all_ntuser := true;
                  if Remaining = ''
                  then
                    ActionResult := reportError (Sektion, i, Sektion.strings [i-1], 'File parameter missing')
                  else
                  Begin
                    GetWordOrStringExpressionstr (Remaining, Filename, Remaining, ErrorInfo);
                  End;
                  remaining := CutRightBlanks (Remaining);

                   if length (remaining) > 0 then goon := true;
                   while goon
                   do
                   begin

                      if skip(Parameter_AllNTUserProfiles, Remaining, Remaining, ErrorInfo)
                      then
                        flag_all_ntuser := true

                      else
                      if skip('/encoding', Remaining, Remaining, ErrorInfo)
                      then
                      begin
                        if not EvaluateString (Remaining, Remaining, flag_encoding, ErrorInfo) then
                        begin
                         syntaxcheck := false;
                          //ActionResult := reportError (ErrorInfo);
                        end;
                        flag_encoding := LowerCase(flag_encoding);
                        if not isSupportedEncoding(flag_encoding) then
                        begin
                          logdatei.log('Given Encoding: ' + flag_encoding +
                             ' is not supported - fall back to system encoding.', LLWarning);
                          flag_encoding := 'system';
                        end
                      end
                      else
                      Begin
                        goon := false;
                        if length (remaining) > 0
                        then
                        begin
                          syntaxcheck := false;
                          ActionResult := reportError (Sektion, i, Sektion.strings [i-1],
                                                   '"' + remaining + '" is no valid parameter ');
                        end;
                      end;
                   end;
                   ActionResult := doInifilePatches (ArbeitsSektion, Filename,'')
                end;

              tsHostsPatch:
                Begin
                  logdatei.log('Execution of: '+ArbeitsSektion.Name+' '+ Remaining,LLNotice);
                   GetWordOrStringExpressionstr (Remaining, Filename, Remaining, ErrorInfo);
                   ActionResult := doHostsPatch (ArbeitsSektion, Filename);
                End;



              tsRegistryHack:
                 begin
                   {$IFDEF WINDOWS}
                   logdatei.log('Execution of: '+ArbeitsSektion.Name+' '+ Remaining,LLNotice);
                   syntaxcheck := true;

                   registryformat := trfWinst;
                   flag_all_ntuser := false;
                   flag_ntuser := false;
                   reg_specified_basekey := '';
                   flag_force64 := false;
                   flag_all_usrclass := false;

                   // if this is a 'ProfileActions' which is called as sub in Machine mode
                   // so run registry sections implicit as /Allntuserdats
                   if runProfileActions then
                      flag_all_ntuser := true;

                   remaining := CutRightBlanks (Remaining);

                   if length (remaining) > 0 then goon := true;
                   while goon
                   do
                   begin

                      if skip(Parameter_AllNTUserDats, Remaining, Remaining, ErrorInfo)
                      then
                        flag_all_ntuser := true

                      else if skip(Parameter_AllUsrClassDats, Remaining, Remaining, ErrorInfo)
                      then
                        flag_all_usrclass := true

                      else if skip(Parameter_RegistryNTUserDat, Remaining, Remaining, ErrorInfo)
                      then
                      begin
                        flag_ntuser := true;
                        if not EvaluateString (Remaining, Remaining, ntuserpath, ErrorInfo) then
                        begin
                         syntaxcheck := false;
                          //ActionResult := reportError (ErrorInfo);
                        end;
                      end
                      else if skip(Parameter_Registry64Bit, Remaining, Remaining, ErrorInfo)
                      then
                      begin
                        if (GetNTVersionMajor = 5) and (GetNTVersionMinor = 0) then
                        begin
                          // we are on win 2000 which can't handle redirections flags
                          flag_force64 := false;
                        end
                        else // we are on xp or higher
                        begin
                          flag_force64 := true;
                        end;
                      end


                      else if skip(Parameter_Registry32Bit, Remaining, Remaining, ErrorInfo)
                      then
                      begin
                        if flag_all_ntuser then
                        begin
                          // no redirection in user profile registry
                          // so always we are on sysnative
                          LogDatei.log('Ignoring parameter /32bit for /AllNTUserdats. This is always /sysnative',LLInfo);
                          flag_force64 := true;
                        end
                        else flag_force64 := false;
                      end

                      else if skip(Parameter_RegistrySysNative, Remaining, Remaining, ErrorInfo)
                      then
                      begin
                        if (GetNTVersionMajor = 5) and (GetNTVersionMinor = 0) then
                        begin
                          // we are on win 2000 which can't handle redirections flags
                          flag_force64 := false;
                        end
                        else // we are on xp or higher
                        begin
                          flag_force64 := true;
                        end;
                      end


                      else if skip (Parameter_RegistryUsercontext, Remaining, Remaining, ErrorInfo)
                      then
                        reg_specified_basekey := 'HKEY_USERS\' + usercontextSID

                      else if
                        skip (Parameter_RegistryBaseKey, Remaining, Remaining, ErrorInfo)
                        and skip ('=', Remaining, Remaining, ErrorInfo)
                        and GetString(Remaining, s1, Remaining, ErrorInfo, false)
                      then
                        reg_specified_basekey := s1

                      else if skip (Parameter_SysDiffAddReg, Remaining, Remaining, ErrorInfo)
                      then
                        registryformat := trfSysdiff

                      else if skip (Parameter_RegeditFormat, Remaining, Remaining, ErrorInfo)
                      then
                        registryformat := trfRegedit

                      else
                      Begin
                        goon := false;
                        if length (remaining) > 0
                        then
                        begin
                          syntaxcheck := false;
                          ActionResult := reportError (Sektion, i, Sektion.strings [i-1],
                                                   '"' + remaining + '" is no valid parameter ');
                        end;
                      end;

                   end;


                   if (flag_all_ntuser or flag_all_usrclass)and (reg_specified_basekey <> '')
                   then
                   begin
                     syntaxcheck := false;
                     ErrorInfo := 'this combination of options is impossible';
                   end;

                   if inUsercontext then
                        reg_specified_basekey := 'HKEY_USERS\' + usercontextSID;


                   if syntaxcheck then
                   begin
                     if flag_all_ntuser
                     then
                     begin
                       if registryformat = trfSysdiff
                       then
                           ActionResult := reportError (Sektion, i, Sektion.strings [i-1],
                                       '"' + Remaining + '": sysdiff format not possible with option "for all nt user"')
                       else
                          ActionResult := doRegistryAllNTUserDats (ArbeitsSektion, registryformat, flag_force64);
                     end
                     else if flag_ntuser
                     then
                     begin
                       if registryformat = trfSysdiff
                       then
                           ActionResult := reportError (Sektion, i, Sektion.strings [i-1],
                                       '"' + Remaining + '": sysdiff format not possible with option "ntuser"')
                       else
                          ActionResult := doRegistryNTUserDat (ArbeitsSektion, registryformat, flag_force64,ntuserpath);
                     end
                     else if flag_all_usrclass
                     then
                     begin
                       if registryformat = trfSysdiff
                       then
                           ActionResult := reportError (Sektion, i, Sektion.strings [i-1],
                                       '"' + Remaining + '": sysdiff format not possible with option "for all usr classes"')
                       else
                          ActionResult := doRegistryAllUsrClassDats (ArbeitsSektion, registryformat, flag_force64);
                     end
                     else
                       case registryformat of
                        trfWinst   :
                           ActionResult := doRegistryHack (ArbeitsSektion, reg_specified_basekey, flag_force64);

                        trfSysdiff :
                           ActionResult := doRegistryHackInfSource (ArbeitsSektion, reg_specified_basekey, flag_force64);

                        trfRegedit :
                           ActionResult := doRegistryHackRegeditFormat (ArbeitsSektion, reg_specified_basekey, flag_force64);

                       end;


                   end;
                   {$ELSE WINDOWS}
                  logdatei.log('Registry sections are not implemented for Linux.', LLWarning);
                  {$ENDIF WINDOWS}
                 end;


              tsXMLPatch:
                begin
                  {$IFDEF WINDOWS}
                   logdatei.log('Execution of: '+ArbeitsSektion.Name+' '+ Remaining,LLNotice);
                   EvaluateString (Remaining, Remaining, Parameter, InfoSyntaxError);
                   if Remaining = ''
                   then
                      ActionResult := doXMLPatch (ArbeitsSektion, Parameter, output)
                   else
                      ActionResult := reportError (Sektion, i, Sektion.strings [i-1],
                                       ' end of line expected');
                  {$ELSE WINDOWS}
                  logdatei.log('XMLPatch sections are not implemented for Linux right now', LLWarning);
                  {$ENDIF WINDOWS}
                end;

              tsOpsiServiceCall, tsOpsiServiceCallStat:
                begin
                   logdatei.log('Execution of: '+ArbeitsSektion.Name+' '+ Remaining,LLNotice);
                   Parameter := Remaining;
                   if (uppercase(PStatNames^ [tsOpsiServiceCall]) = uppercase(Expressionstr))
                      and skip ('/preloginservice', Remaining, Remaining, errorInfo)
                     then
                   begin
                     logdatei.log('Execution of OpsiServiceCall /preloginservce',LLNotice);
                     if (local_opsidata <> nil) and (local_opsidata <> opsidata) then
                     begin
                       local_opsidata.Free;
                       local_opsidata := opsidata;
                     end;
                   end
                   else ActionResult := doOpsiServiceCall (ArbeitsSektion, Parameter, output)
                end;

                tsOpsiServiceHashList:
                begin
                   logdatei.log('Execution of: '+ArbeitsSektion.Name+' '+ Remaining,LLNotice);
                   Parameter := Remaining;
                   ActionResult := doOpsiServiceHashList (ArbeitsSektion, Parameter, output)
                end;

              {$IFDEF WINDOWS}
              tsIdapiConfig:
                 begin
                      GetWordOrStringExpressionstr (Remaining, Filename, Remaining, ErrorInfo);
                      ActionResult := doIdapiConfig (ArbeitsSektion, Filename);
                      //ActionResult := doIdapiConfig (ArbeitsSektion, Remaining);
                  end;
              {$ENDIF WINDOWS}

              tsLDAPsearch:
                begin
                 logdatei.log('Execution of: '+ArbeitsSektion.Name+' '+ Remaining,LLNotice);
                  EvaluateString (Remaining, Remaining, Parameter, InfoSyntaxError);
                  if produceLDAPsearchParameters(Remaining, cacheRequest, outputRequest, InfoSyntaxError)
                  then
                    ActionResult := doLDAPSearch (ArbeitsSektion, cacheRequest, outputRequest, output)
                  else
                    ActionResult := reportError (Sektion, i, Sektion.strings [i-1], InfoSyntaxError);
                end;

              tsFileActions:
                begin
                 logdatei.log('Execution of: '+ArbeitsSektion.Name+' '+ Remaining,LLNotice);
                 flag_all_ntuser := false;
                 // if this is a 'ProfileActions' which is called as sub in Machine mode
                 // so run registry sections implicit as /Allntuserdats
                 if runProfileActions then
                      flag_all_ntuser := true;
                 ActionResult := doFileActions (ArbeitsSektion, Remaining);
                end;

              tsLinkFolder:
                begin
                  //{$IFDEF WIN32}
                 logdatei.log('Execution of: '+ArbeitsSektion.Name+' '+ Remaining,LLNotice);
                  if Remaining = ''
                  then
                      doLinkFolderActions (ArbeitsSektion, true)
                  else
                  Begin
                      GetWord (remaining, Expressionstr, remaining, WorddelimiterSet0);
                      if not (lowercase (Expressionstr) = lowercase (Parameter_AllNTUser))
                      then
                        ActionResult := reportError (Sektion, i, Sektion.strings [i-1],
                                        'No valid parameter')
                      else
                         doLinkFolderActions (ArbeitsSektion, false);
                  End
                  //{$ELSE WIN32}
                  //logdatei.log('Linkfolder sections are not implemented for Linux right now', LLWarning);
                  //{$ENDIF WIN32}
                end;


              tsWinBatch:
               begin
                //{$IFDEF WINDOWS}
                logdatei.log('Execution of: '+ArbeitsSektion.Name+' '+ Remaining,LLNotice);
                 runAs := traInvoker;
                 {$IFDEF WIN32}
                 opsiSetupAdmin_runElevated := false;
                 {$ENDIF WIN32}
                 WaitSecs := 0;
                 flag_force64 := false;
                 GetWord (Remaining, expr, Remaining, WordDelimiterSet0);
                 SyntaxCheck := true;

                 ident := '';
                 WaitConditions := [ttpWaitOnTerminate];

                 while SyntaxCheck and (length (expr) > 0)
                 do
                 Begin
                   {$IFDEF WIN32}
                   if LowerCase (expr) = LowerCase (ParameterRunAsAdmin)
                   then
                   Begin
                       runAs := traAdmin;
                   End

                   else if LowerCase (expr) = LowerCase (ParameterRunAsAdmin1)
                   then
                   Begin
                       runAs := traAdmin;
                   End

                   else if LowerCase (expr) = LowerCase (ParameterRunAsAdmin2)
                   then
                   Begin
                       runAs := traAdminProfile;
                   End

                   else if LowerCase (expr) = LowerCase (ParameterRunAsAdmin3)
                   then
                   Begin
                       runAs := traAdminProfileImpersonate;
                   End

                   else if LowerCase (expr) = LowerCase (ParameterRunAsAdmin4)
                   then
                   Begin
                       runAs := traAdminProfileImpersonateExplorer;
                   End

                   else if LowerCase (expr) = LowerCase (ParameterRunAsInvoker)
                   then
                   Begin
                       runAs := traInvoker;
                   End

                   else if LowerCase (expr) = LowerCase (ParameterRunElevated)
                   then
                   Begin
                       opsiSetupAdmin_runElevated := true;
                   End

                   else if LowerCase (expr) = LowerCase (ParameterRunAsLoggedOnUser)
                   then
                   Begin
                     if runLoginScripts then
                       runAs := traLoggedOnUser
                     else
                       LogDatei.log('Warning: Not in UserLoginScript mode: /RunAsLoggedinUser ignored', LLWarning);
                   End

                   else if (LowerCase (expr) = LowerCase (Parameter_64Bit)) and Is64BitSystem
                   then
                   Begin
                       flag_force64 := true;
                   End

                   else if LowerCase (expr) = LowerCase (Parameter_32Bit)
                   then
                   Begin
                       flag_force64 := false;
                   End

                   else if (LowerCase (expr) = LowerCase (Parameter_SysNative)) and Is64BitSystem
                   then
                   Begin
                       flag_force64 := true;
                   End

                   else if (LowerCase (expr) = LowerCase (Parameter_SysNative)) and (not Is64BitSystem)
                   then
                   Begin
                       flag_force64 := false;
                   End

                   else {$ENDIF WIN32}  if LowerCase (expr) = LowerCase (ParameterWaitSecs)
                   then
                   Begin
                     WaitConditions := WaitConditions + [ttpWaitTime];
                     // WaitConditions := WaitConditions - [ttpWaitOnTerminate];

                     GetWord (Remaining, expr, Remaining, WordDelimiterSet0);
                     try
                       WaitSecs := StrToInt64 (expr);
                     except
                       on EConvertError do
                       Begin
                          InfoSyntaxError := 'Integer number expected';
                          SyntaxCheck := false;
                       End
                     end;
                   End

                   {$IFDEF WIN32}
                   else if LowerCase (expr) = LowerCase (ParameterCloseOnWindow)
                   then
                   Begin
                     runAs := traInvoker;
                     WaitConditions := WaitConditions + [ttpWaitForWindowAppearing];

                     if EvaluateString (Remaining, Remaining, ident, InfoSyntaxError)
                     then
                     Begin
                       if Remaining <> ''
                       then
                       Begin
                         SyntaxCheck := false;
                         InfoSyntaxError := 'not expected chars after "';
                       End;
                     End
                     else
                        SyntaxCheck := false;
                   End


                   else if LowerCase (expr) = LowerCase (ParameterCloseBehindWindow)
                   then
                   Begin
                     runAs := traInvoker;
                     WaitConditions := WaitConditions + [ttpWaitForWindowVanished];

                     if EvaluateString (Remaining, Remaining, ident, InfoSyntaxError)
                     then
                     Begin
                       if Remaining <> ''
                       then
                       Begin
                         SyntaxCheck := false;
                         InfoSyntaxError := 'unexpected characters after "';
                       End;
                     End
                     else
                       SyntaxCheck := false;
                   End

                   {$ENDIF WIN32}

                   else if LowerCase (expr) = LowerCase (ParameterWaitProcessTimeoutSecs)
                   then
                   Begin
                     WaitConditions := WaitConditions - [ttpWaitTime];
                     WaitConditions := WaitConditions + [ttpWaitTimeout];

                     GetWord (Remaining, expr, Remaining, WordDelimiterSet0);
                     try
                       WaitSecs := StrToInt64 (expr);
                     except
                       on EConvertError do
                       Begin
                         try
                           EvaluateString (expr, expr, seconds, InfoSyntaxError);
                           WaitSecs := StrToInt64 (seconds);
                         except
                           on EConvertError do
                           Begin
                             InfoSyntaxError := 'Integer number expected '+InfoSyntaxError;
                            SyntaxCheck := false;
                          end;
                         end
                       End
                     end
                   End

                   else if LowerCase (expr) = LowerCase (ParameterWaitForProcessEnding)
                   then
                   Begin
                     WaitConditions := WaitConditions + [ttpWaitForProcessEnding];
(*
                     GetWord (Remaining, ident, Remaining, WordDelimiterSet0);
                     if ident = '' then
                     begin
                       SyntaxCheck := false;
                       InfoSyntaxError := 'process name expected';
                     end;
*)
                     if not EvaluateString (Remaining, Remaining, ident, InfoSyntaxError)  then
                        SyntaxCheck := false;
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
                   End

                   else if UpperCase (expr) = UpperCase (ParameterDontWait)
                   then
                   Begin
                       WaitConditions := WaitConditions - [ttpWaitOnTerminate];
                       WaitConditions := WaitConditions - [ttpWaitTimeout];
                   End

                   else if UpperCase (expr) = UpperCase (ParameterWaitOnTerminate)
                   then
                   Begin
                       WaitConditions := WaitConditions + [ttpWaitOnTerminate];
                   End



                   else
                   Begin
                         SyntaxCheck := false;
                         InfoSyntaxError := expr + ' not legal WinBatch parameter';
                   End;

                   GetWord (Remaining, expr, Remaining, WordDelimiterSet0);
                 end;

                 if SyntaxCheck
                 then
                   ActionResult := execWinBatch (ArbeitsSektion, Remaining, WaitConditions, Ident, WaitSecs, runAs,flag_force64)
                 else
                   ActionResult := reportError (Sektion, i, 'Expressionstr', InfoSyntaxError);
                 (*
                 {$ELSE WINDOWS}
                  logdatei.log('Winbatch sections are not implemented for Linux right now', LLWarning);
                  {$ENDIF WINDOWS}
                  *)
               end;

              {$IFDEF WIN32}
              tsStayWhileWindowOpen:
               begin
                 SyntaxCheck := false;
                 if EvaluateString (Remaining, Remaining, ident, InfoSyntaxError)
                 then
                 if Remaining = ''
                 then SyntaxCheck := true
                 else InfoSyntaxError := 'unexpected chars after "';
                 if SyntaxCheck
                 then
                   while FindWindowEx (0, 0, nil, PChar (ident) ) <> 0
                   do ProcessMess
                 else
                   ActionResult := reportError (Sektion, i, Expressionstr, InfoSyntaxError);
               end;
               {$ENDIF WIN32}

              tsDosBatchFile:
                begin
                  logdatei.log('Execution of: '+ArbeitsSektion.Name+' '+ Remaining,LLNotice);
                  ActionResult := execDOSBatch (ArbeitsSektion, Remaining,
                           SW_ShowNormal, false {dont catch out}, 0,
                           [ttpWaitOnTerminate], output);
                end;

              tsDosInAnIcon:
                begin
                  logdatei.log('Execution of: '+ArbeitsSektion.Name+' '+ Remaining,LLNotice);
                   ActionResult := execDOSBatch (ArbeitsSektion, Remaining,
                           SW_HIDE, true {catch out}, 0,
                           [ttpWaitOnTerminate], output);
                 end;

              tsShellBatchFile:
                begin
                  logdatei.log('Execution of: '+ArbeitsSektion.Name+' '+ Remaining,LLNotice);
                  ActionResult := execDOSBatch (ArbeitsSektion, Remaining,
                           SW_ShowNormal, false {dont catch out}, 0,
                           [ttpWaitOnTerminate], output);
                end;

              tsShellInAnIcon:
                begin
                  logdatei.log('Execution of: '+ArbeitsSektion.Name+' '+ Remaining,LLNotice);
                  ActionResult := execDOSBatch (ArbeitsSektion, Remaining,
                           SW_HIDE, true {catch out},  0,
                           [ttpWaitOnTerminate], output);
                end;

              tsExecutePython :
                begin
                  logdatei.log('Execution of: '+ArbeitsSektion.Name+' '+ Remaining,LLNotice);
                  ActionResult := execPython (ArbeitsSektion, Remaining,
                           true {catch out},  0,
                           [ttpWaitOnTerminate], output);
                end;

              tsExecuteWith, tsExecuteWith_escapingStrings:
                begin
                  logdatei.log('Execution of: '+ArbeitsSektion.Name+' '+ Remaining,LLNotice);
                  if
                    produceExecLine(remaining, p1, p2, p3, p4,
                      InfoSyntaxError)
                  then
                    ActionResult := executeWith (ArbeitsSektion, Remaining,
                             true {catch out}, 0,
                             output)
                  else
                    ActionResult := reportError (Sektion, i, Expressionstr, InfoSyntaxError);
                end;

              tsWorkOnStringList:
                 Begin
                   ActionResult := reportError (Sektion, i, Expressionstr, 'not yet implemented');
                 end;

                tsDDEwithProgman:
                  //ActionResult := doDDEwithProgman (ArbeitsSektion, Remaining, SaveddeWithProgman);
                  ActionResult := doDDEwithProgman (ArbeitsSektion, Remaining, false);

              {$IFDEF WIN32}
              tsAddConnection:
               Begin
                                     syntaxCheck := true;
                   GetWord (Remaining, localname, Remaining, WordDelimiterWhiteSpace);
                   GetWord (Remaining, remotename, Remaining, WordDelimiterWhiteSpace);
                   (* GetWord (Remaining, username, Remaining, WordDelimiterWhiteSpace); *)
                   GetWord (Remaining, trytime, Remaining, WordDelimiterWhiteSpace);

                   try
                     timeout := StrToInt (trytime);
                   except
                     syntaxCheck := false;
                     InfoSyntaxError := trytime + ' is not an integer value';
                   end;

                   if syntaxCheck
                   then
                     ActionResult := startConnection (localname, remotename, timeout)
                   else
                     ActionResult := reportError (Sektion, i, Expressionstr, InfoSyntaxError);
               End;
               {$ENDIF WIN32}

              tsDefineVar:
               Begin
                 call := Remaining;
                 GetWord (Remaining, Expressionstr, Remaining, WordDelimiterSet1);
                 if Remaining <> ''
                 then
                   reportError (Sektion, i, Expressionstr, 'not allowed char following variable name')
                 else if findKindOfStatement (Expressionstr, SectionSpecifier, call) <> tsNotDefined then
                   reportError (Sektion, i, Expressionstr,
                     'Reserved name, must not be used in a variable definition')
                 // in local function ?
                 else if inDefinedFuncNestCounter > 0 then
                 begin
                   // get the function we are in
                   funcindex := strToInt(definedFunctionsCallStack.Strings[definedFunctionsCallStack.Count-1]);
                   if definedFunctionArray[funcindex].addLocalVar(lowercase(Expressionstr),dfpString,false) then
                     LogDatei.log('Defined local string var: '+lowercase(Expressionstr)+' in local function: '+definedFunctionArray[funcindex].Name,LLDebug2)
                   else reportError (Sektion, i, Expressionstr, 'name is already in use')
                 end
                 // not in local function - make it global
                 else if VarList.IndexOf (lowercase(Expressionstr)) >= 0 then
                   reportError (Sektion, i, Expressionstr, 'name is already in use')
                 else
                 Begin
                   VarList.Add (lowercase(Expressionstr));
                   ValuesList.Add ('');
                 End;
               End;

               tsDefineStringList:
               Begin
                 call := Remaining;
                 GetWord (Remaining, Expressionstr, Remaining, WordDelimiterSet1);
                 if Remaining <> ''
                 then
                   reportError (Sektion, i, Expressionstr, 'char not allowed following variable name')
                 else if findKindOfStatement (Expressionstr, SectionSpecifier, call) <> tsNotDefined then
                   reportError (Sektion, i, Expressionstr,
                     'Reserved name, must not be used in a variable definition')
                 // in local function ?
                 else if inDefinedFuncNestCounter > 0 then
                 begin
                   // get the function we are in
                   funcindex := strToInt(definedFunctionsCallStack.Strings[definedFunctionsCallStack.Count-1]);
                   if definedFunctionArray[funcindex].addLocalVar(lowercase(Expressionstr),dfpStringlist,false) then
                     LogDatei.log('Defined local stringlist var: '+lowercase(Expressionstr)+' in local function: '+definedFunctionArray[funcindex].Name,LLDebug2)
                   else reportError (Sektion, i, Expressionstr, 'name is already in use')
                 end
                 // not in local function - make it global

                 else if VarList.IndexOf (lowercase(Expressionstr))  or listOfStringLists.IndexOf(lowercase(Expressionstr)) >= 0 then
                   reportError (Sektion, i, Expressionstr, 'Name already in use')
                 else

                 Begin
                   listOfStringLists.Add (lowercase(Expressionstr));
                   ContentOfStringLists.Add (TStringList.create);
                   LogDatei.log( '', leveldebug);
                   LogDatei.log( '(created string list ' + Expressionstr + ')', leveldebug );
                 End;
               End;

               tsDefineFunction:
               Begin
                 try
                   newDefinedfunction := TOsDefinedFunction.create;
                   if not newDefinedfunction.parseDefinition(Remaining,ErrorInfo) then
                   begin
                     reportError (Sektion, i, Expressionstr, ErrorInfo);
                   end
                   else
                   begin
                     try
                       s1 := newDefinedfunction.Name;
                       tmpint := script.FSectionNameList.IndexOf(s1);
                       if (tmpint >= 0) and (tmpint <= length(Script.FSectionInfoArray)) then
                       begin
                         newDefinedfunction.OriginFile:=Script.FSectionInfoArray[tmpint].SectionFile;
                         newDefinedfunction.OriginFileStartLineNumber:=Script.FSectionInfoArray[tmpint].StartLineNo;
                       end
                       else logdatei.log('Warning: Origin of function: '+s1+' not found.',LLwarning);
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
                          LogDatei.log('Exception in doAktionen: tsDefineFunction: tmplist: ' +
                            e.message, LLError);
                          //raise e;
                        end;
                     end;
                     try
                       // get all lines until 'endfunction'
                       //endofDefFuncFound := false;
                       inDefFunc := 1;
                       repeat
                         // get next line of section
                         inc(i); // inc line counter
                         inc(FaktScriptLineNumber); // inc line counter that ignores the execution of defined functions
                         if (i <= Sektion.Count-1) then
                         begin
                           Remaining := trim (Sektion.strings [i-1]);
                           myline := remaining;
                           GetWord (Remaining, Expressionstr, Remaining, WordDelimiterSet4);
                           StatKind := FindKindOfStatement (Expressionstr, SectionSpecifier, call);
                           if StatKind = tsDefineFunction then inc(inDefFunc);
                           if StatKind = tsEndFunction then dec(inDefFunc);
                           // Line with tsEndFunction should not be part of the content
                           if inDefFunc > 0 then
                           begin
                             newDefinedfunction.addContent(myline);
                             LogDatei.log_prog('inDefFunc: '+inttostr(inDefFunc)+' add line: '+myline,LLDebug3);
                           end;
                         end;
                       until (inDefFunc <= 0) or (i >= Sektion.Count - 2);
                     except
                        on e: Exception do
                        begin
                          LogDatei.log('Exception in doAktionen: tsDefineFunction: endfunction: ' +
                            e.message, LLError);
                          //raise e;
                        end;
                     end;
                     try
                       if inDefFunc > 0 then
                       begin
                         LogDatei.log('Found DefFunc without EndFunc',LLCritical);
                         reportError (Sektion, i, Expressionstr, 'Found DefFunc without EndFunc');
                       end
                       else
                       begin
                         // endfunction found
                         // append new defined function to the stored (known) functions
                         inc(definedFunctioncounter);
                         newDefinedfunction.Index:= definedFunctioncounter-1;
                         SetLength(definedFunctionArray, definedFunctioncounter);
                         definedFunctionArray[definedFunctioncounter-1] := newDefinedfunction;
                         definedFunctionNames.Append(newDefinedfunction.Name);
                         dec(inDefFunc3);
                         LogDatei.log('Added defined function: '+newDefinedfunction.Name+' to the known functions',LLInfo);
                         logdatei.log_prog('After adding a defined function: inDefFunc3: '+IntToStr(inDefFunc3),LLDebug3);
                       end;
                     except
                        on e: Exception do
                        begin
                          LogDatei.log('Exception in doAktionen: tsDefineFunction: append: ' +
                            e.message, LLError);
                          //raise e;
                        end;
                     end;
                   end;
                   LogDatei.log_prog('After reading '+newDefinedfunction.Name+' we are on line: '+inttostr(i-1)+' -> '+trim (Sektion.strings [i-1]),LLInfo);
                 except
                    on e: Exception do
                    begin
                      LogDatei.log('Exception in doAktionen: tsDefineFunction: ' +
                        e.message, LLError);
                      //raise e;
                    end;
                 end;
               End;

               tsEndFunction:
               Begin
                 // nothing to do
               end;


              tsSetVar:
               Begin
                 //writeln('set');
                 Expressionstr := Remaining;
                 doLogEntries (PStatNames^ [tsSetVar] + '  '  + Expressionstr, LLInfo);
                 if doSetVar (sektion, Expressionstr, Remaining, InfoSyntaxError,NestLevel)
                 then
                 Begin
                   if Remaining <> ''
                   then
                     reportError (Sektion, i, Remaining, 'Remaining char(s) not allowed here');
                 End
                 else

                    reportError (Sektion, i, Expressionstr, InfoSyntaxError);
               End

              else
               ActionResult := reportError (Sektion, i, Expressionstr, 'undefined');

            End (* case *);
          End;
        End
        else processline := false;
        ProcessMess;
      end;


      if inloop
      then
      begin
        if looplist.count = 0 //loop will finish
        then
        begin
          inloop := false;
          // clearing the loop variable from the list of variables, first the value
          ValuesList.Delete( varlist.indexOf (loopvar));
          varlist.Delete( varlist.indexOf (loopvar) );
          LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
          LogDatei.log ('', LLinfo);
          LogDatei.log ('~~~~~~ End Loop', LLinfo);

        end;

      end;

    until not inloop;
    LogDatei.log_prog ('Finished with linenr: '+inttostr(i)+' -> '+trim (Sektion.strings [i-1]), LLinfo);
    logdatei.log_prog('Actlevel: '+IntToStr(Actlevel)+' NestLevel: '+IntToStr(NestLevel)+' ArbeitsSektion.NestingLevel: '+IntToStr(ArbeitsSektion.NestingLevel)+' Sektion.NestingLevel: '+IntToStr(Sektion.NestingLevel),LLDebug2);
    inc (i);
  end;

  if not scriptstopped then
  begin
    if (ActLevel > NestLevel) and (actionresult > tsrFatalError)
    then
      reportError (Sektion, i, PStatNames^ [tsCondClose], 'expected');

    if (NestLevel > Sektion.NestingLevel) and (actionresult > tsrFatalError)
    then
      reportError (Sektion, i, PStatNames^ [tsCondClose], 'expected');
  end;


  if ActionResult <= tsrFatalError
  then
    doLogEntries (AbortedInfo, BaseLevel)
  else if ActionResult <= tsrExitProcess
  then
    doLogEntries (StopInfo, BaseLevel);

  ArbeitsSektion.free;

  LogDatei.LogSIndentLevel := StartIndentLevel;

  result := ActionResult;
  output.Free;
  // restore last section
  Script.ActiveSection := Script.FLastSection;
end;


procedure TuibInstScript.registerSectionOrigins(mylist : Tstringlist;filename : string);
begin
  registerSectionOrigins(mylist, filename, '');
end;

procedure TuibInstScript.registerSectionOrigins(mylist : Tstringlist;filename : string; secname : string);
var
  i,secindex, tmpint : integer;
  str : string;
begin
  for i := 0 to mylist.count-1 do
  Begin
    str := trim(mylist.Strings [i]);
    if pos('sub "', secname) = 1 then
    begin
      // we have a new external sub section
      secindex := Script.FSectionNameList.Add(secname);
      tmpint := length(script.FSectionInfoArray);
      if secindex <> tmpint then
        LogDatei.log('Error: internal: secindex <> length(script.FSectionInfoArray)',LLCritical);
      setlength(script.FSectionInfoArray, secindex+1);
      script.FSectionInfoArray[secindex].SectionName:=secname;
      script.FSectionInfoArray[secindex].SectionFile:=ExtractFileName(filename);
      script.FSectionInfoArray[secindex].StartLineNo:=i;
    end
    else
    begin
      secname := opsiunquotestr2(str,'[]');
      if secname <> str then
      begin
        // we have a new real section
        secindex := Script.FSectionNameList.Add(secname);
        tmpint := length(script.FSectionInfoArray);
        if secindex <> tmpint then
          LogDatei.log('Error: internal: secindex <> length(script.FSectionInfoArray)',LLCritical);
        setlength(script.FSectionInfoArray, secindex+1);
        script.FSectionInfoArray[secindex].SectionName:=secname;
        script.FSectionInfoArray[secindex].SectionFile:=ExtractFileName(filename);
        script.FSectionInfoArray[secindex].StartLineNo:=i;
      end;
    end;
    if pos('deffunc',lowercase(str)) = 1  then
    begin
      // we have a new function
      secname := trim(Copy(str,length('deffunc')+1,length(str)));
      GetWord(secname, secname, remaining,WordDelimiterSet5);
      secindex := Script.FSectionNameList.Add(secname);
      tmpint := length(script.FSectionInfoArray);
      if secindex <> tmpint then
        LogDatei.log('Error: internal: secindex <> length(script.FSectionInfoArray)',LLCritical);
      setlength(script.FSectionInfoArray, secindex+1);
      script.FSectionInfoArray[secindex].SectionName:=secname;
      script.FSectionInfoArray[secindex].SectionFile:=ExtractFileName(filename);
      script.FSectionInfoArray[secindex].StartLineNo:=i;
    end;
  End;
end;


procedure CreateAndProcessScript (Const Scriptdatei : String;
                         NestingLevel : Integer; SaveddeWithProgman : Boolean;
                         var extremeErrorLevel : TErrorLevel);


 var
  StartlineOfSection: Integer=0;
  Aktionsliste : TWorkSection;
  //Script : TuibInstScript;
  nrErrors : Integer=0;
  i : Integer=0;
  nrWarnings : Integer=0;
  oldNumberOfErrors : Integer=0;
  oldNumberOfWarnings : Integer=0;
  weiter : Integer=0;
  ValueToTake : String='';
  s : String='';
  ErrorInfo : String='';
  saveIndent : Integer=0;
  hostnaming : String='';
  computernaming : String='';
  ipName : String='';
  ipAddress : String='';
  usedEncoding : String='';
  Encoding2use : String='';
  tmpstr : string='';
  str : string;
  depotdrive_bak, depotdir_bak : string;
  {$IFDEF WINDOWS}
  Regist : TuibRegistry;
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
      logdatei.log('readconfig failed: Perhaps got defaults ',LLError);
      LogDatei.log('Depot path from readconfig:  ' + depotdrive + depotdir, LLinfo);
      LogDatei.log('Restoring depot path from backup', LLinfo);
      // restoring old values
      depotdrive := depotdrive_bak;
      depotdir :=  depotdir_bak;
    end;
  end;
  LogDatei.force_min_loglevel:=osconf.force_min_loglevel;
  LogDatei.debug_prog:=osconf.debug_prog;
  LogDatei.LogLevel:=osconf.default_loglevel;
  LogDatei.debug_lib:= osconf.debug_lib;
  logDatei.log_prog('force_min_loglevel: '+inttostr(osconf.force_min_loglevel),LLessential);
  logDatei.log_prog('default_loglevel: '+inttostr(osconf.default_loglevel),LLessential);
  logDatei.log_prog('debug_prog: '+BoolToStr(osconf.debug_prog,true),LLessential);
  logDatei.log_prog('debug_lib: '+booltostr(osconf.debug_lib,true),LLessential);
  logDatei.log_prog('ScriptErrorMessages: '+BoolToStr(osconf.ScriptErrorMessages,true),LLessential);
  logDatei.log_prog('AutoActivityDisplay: '+booltostr(osconf.AutoActivityDisplay,true),LLessential);
  LogDatei.log('Using new Depot path:  ' + depotdrive + depotdir, LLinfo);
  // init vars
  inDefFuncLevel := 0;
  inDefFuncIndex := -1;
  Ifelseendiflevel := 0;
  inDefinedFuncNestCounter := 0;
  definedFunctioncounter := 0;
  inDefFunc3 := 0;


  Script := TuibInstScript.Create;
  script.aktScriptLineNumber:=0;
  // Backup existing depotdrive, depotdir
  depotdrive_bak := depotdrive;
  depotdir_bak :=  depotdir;
  {$IFDEF LINUX} computername := getHostnameLin; {$ENDIF LINUX}
  {$IFDEF GUI}
  CentralForm.Label1.Caption := '';
  FBatchOberflaeche.setInfoLabel('');

  CentralForm.Label2.caption := '';
  FBatchOberflaeche.setDetailLabel(CentralForm.Label2.caption);
  {$ENDIF GUI}
  {$IFDEF LINUX}
  lispecfolder.retrieveFolders4Linux;
  {$ENDIF LINUX}
  if Scriptdatei <> ''
  then
  Begin
    //s := 'Drives found: ' + GetDrives;
    if not CheckFileExists (Scriptdatei, ErrorInfo)
    then
    Begin
      LogDatei.log ('Script  ' + Scriptdatei + '  not found ' + ErrorInfo+' - retrying',LLWarning);
      Sleep(1000);
      if not CheckFileExists (Scriptdatei, ErrorInfo) then
      begin
        LogDatei.log ('Script  ' + Scriptdatei + '  not found ' + ErrorInfo+' - retrying',LLWarning);
        Sleep(1000);
        if not CheckFileExists (Scriptdatei, ErrorInfo) then
        begin
          LogDatei.log ('Script  ' + Scriptdatei + '  not found ' + ErrorInfo+' - giving up',LLError);
          ExtremeErrorLevel := LevelFatal;
          LogDatei.log('Error level set to fatal', LLCritical);
          exit;
        end;
      end;
    end;
    //Scriptdatei := ExpandFileName(Scriptdatei);
    Script.LoadFromFile(Scriptdatei);
    Encoding2use := searchencoding(Script.Text);
    if Encoding2use = '' then Encoding2use := 'system';
    Script.Text:= reencode(Script.Text, Encoding2use,usedEncoding);
    Script.FFilename:=Scriptdatei;
    for i := 0 to script.Count-1 do
    begin
      str := Script.Strings[i];
      script.FLinesOriginList.Append(script.FFilename+' line: '+inttostr(i+1));
      script.FLibList.Append('false');
      //writeln('i='+inttostr(i)+' = '+Script.FLinesOriginList.Strings[i-1]);
    end;
    Script.registerSectionOrigins(Tstringlist(Script),Scriptdatei);
  End
  else
  Begin
    {$IFDEF GUI}
    // Run Viewlist as script
    Script.Assign (CentralForm.Memo1.lines);
    for i := 1 to script.Count do
    begin
      script.FLinesOriginList.Append('Viewlist line: '+inttostr(i));
      script.FLibList.Append('false');
      //writeln('i='+inttostr(i)+' = '+Script.FLinesOriginList.Strings[i-1]);
      //writeln('i='+inttostr(i));
    end;
    {$ENDIF GUI}
  End;


  LogDatei.TraceMode := Script.TraceMode;
  //LogDatei.LogLevel := Script.LogLevel;

  LogDatei.log ('', LLessential);


  ps := '============ ' + WinstVersionName + ' script "' + Scriptdatei   + '"';
  LogDatei.log (ps, LLessential);
  ps :=  '             used script encoding: '  +  usedEncoding;
  LogDatei.log (ps, LLessential);
  ps :=  '             used system encoding: '  +  mysystemEncoding;
  LogDatei.log (ps, LLessential);

  ps := FormatDateTime('yyyy-mm-dd  hh:mm:ss ', now);

  computernaming := osconf.computername;

  if computernaming = '' then computernaming := oslog.getComputerName;


  ps :=  '             start: '  +  ps;
  LogDatei.log (ps, LLessential);

  if opsidata <> nil then
  begin
    ps :=  '             installing product: '+Topsi4data(opsidata).getActualProductId+'_'+opsidata.getActualProductVersion;
    LogDatei.log (ps, LLessential);
  end;

  ps :=  '             on client named    "' + computernaming + '"';
  LogDatei.log (ps, LLessential);

  ps :=  '             loggedin user    "' + getLoggedInUser + '"';
  LogDatei.log (ps, LLessential);

  GetNetUser ('', ps, ErrorInfo);
  ps :=  '             opsi-script running as    "' + ps + '"';
  LogDatei.log (ps, LLessential);

   If runningAsAdmin then
     LogDatei.log ('             opsi-script running with admin privileges', LLessential)
  else
     LogDatei.log ('             opsi-script not running with admin privileges', LLWarning);

   If runloginscripts then
  begin
     LogDatei.log ('             opsi-script running in loginscript mode', LLessential);
     LogDatei.log ('             using context: '+usercontext+' with SID '+usercontextsid, LLessential);
  end
  else
    if runproductlist then LogDatei.log ('             opsi-script running in productlist script mode', LLessential)
     else LogDatei.log ('             opsi-script running in standard script mode', LLessential);


  ps := 'executing: "' + ParamStr(0) + '"';
  LogDatei.log (ps, LLessential);

  //LogDatei.log ('PC MAC address, method1 ' + getMACAddress1, BaseLevel);

  LogDatei.log ('system infos:', LLessential);

  FindLocalIPData (ipName, ipAddress);
  LogDatei.log (GetMACAddress2  + '  -  PC hardware address', LLessential);
  LogDatei.log (ipName +  '  -  IP name ', LLessential);
  LogDatei.log (ipAddress + '  -  IP address', LLessential);
  {$IFDEF WINDOWS}
  LogDatei.log (GetSystemDefaultLocale(LOCALE_SABBREVLANGNAME)
                        + '  -  System default locale ',
                          LLessential);
  tmpstr :=  'MS Windows ' +GetSystemOSVersionInfoEx('major_version')
             +'.'+GetSystemOSVersionInfoEx('minor_version');
  if Is64BitSystem then
    tmpstr :=  tmpstr+' 64 Bit'
  else tmpstr :=  tmpstr+' 32 Bit';
  // we have no ReleaseId before Win10
  if (StrToInt(GetSystemOSVersionInfoEx('major_version')) >= 10)
    and RegVarExists('HKLM\SOFTWARE\Microsoft\Windows NT\CurrentVersion','ReleaseID',true) then
  begin
    tmpstr :=  tmpstr+', Release: '+GetRegistrystringvalue('HKLM\SOFTWARE\Microsoft\Windows NT\CurrentVersion','ReleaseID',true);
  end;
  tmpstr :=  tmpstr+', Edition: '+getProductInfoStrByNum(OSGetProductInfoNum);
  LogDatei.log (tmpstr, LLessential);
  {$ENDIF WINDOWS}

  if opsidata <> nil then
  begin
    LogDatei.log ('opsi service version : '+opsidata.getOpsiServiceVersion, LLessential);
    //Logdatei.log('Setup script name: '+opsidata.getProductScriptPath(tacSetup), LLessential);
  end;

  // backup the start working dir
  opsiWinstStartdir := GetCurrentDir;

  LogDatei.log ('', LLessential);


  {ps := '        (Assuming the shell is "';
  if ShellIsExplorer
  then ps := ps + 'explorer.exe'
  else ps := ps + 'progman.exe';
  ps := ps + '")';

  LogDatei.log (ps, BaseLevel);
  }

  //ProcessIncludes;


  (* Definieren der globalen Systemvariablen *)
  with Script do
  Begin
    //System directories:
    {$IFDEF WINDOWS}
    FConstList.add ('%Systemroot%');
    ValueToTake := GetWinDirectory;
       (* delete closing back slash *)
    System.delete (ValueToTake, length (ValueToTake), 1);
    FConstValuesList.add (ValueToTake);

    FConstList.add ('%System%');
    ValueToTake := GetWinSystemDirectory;
       (* delete closing back slash *)
    System.delete (ValueToTake, length (ValueToTake), 1);
    FConstValuesList.add (ValueToTake);

    FConstList.add ('%SystemDrive%');
    ValueToTake := extractfiledrive (GetWinDirectory);
    FConstValuesList.add (ValueToTake);
    {$ENDIF WINDOWS}
    FConstList.add('%ProfileDir%');
    ValueToTake := getProfilesPath;
    FConstValuesList.add (ValueToTake);

     FConstList.add('%DefaultUserProfileDir%');
    ValueToTake := getDefaultUsersProfilesPath;
    FConstValuesList.add (ValueToTake);


    saveIndent := Logdatei.LogSIndentLevel;
    {$IFDEF WINDOWS}
    ValueToTake := getSpecialFolder(CSIDL_PROGRAM_FILES);
    LogDatei.LogSIndentLevel := saveIndent;

    FConstList.add('%ProgramFilesDir%');
    FConstValuesList.add (ValueToTake);
    FConstList.add('%ProgramFiles32Dir%');
    FConstValuesList.add (ValueToTake);
    try
      if (GetNTVersionMajor = 5) and (GetNTVersionMinor = 0) then
      begin
        // we are on win 2000 which can't handle redirections flags
        Regist := Tuibregistry.Create;
      end
      else // we are on xp or higher
      begin
        Regist := Tuibregistry.Create(true,true);
      end;
      regist.OpenExistingKey ('HKLM', 'SOFTWARE\Microsoft\Windows\CurrentVersion');
      ValueToTake := regist.ReadString ('ProgramFilesDir');
      Regist.free; Regist := nil;
    except
      ValueToTake := 'C:\Program Files'
    end;
    FConstList.add('%ProgramFiles64Dir%');
    FConstValuesList.add (ValueToTake);
    FConstList.add('%ProgramFilesSysnativeDir%');
    FConstValuesList.add (ValueToTake);



    //Usercontext data

    FConstList.add('%Usercontext%');
    FConstValuesList.add (usercontext);

    FConstList.add('%UsercontextSID%');
    FConstValuesList.add (usercontextSID);
    {$ENDIF WINDOWS}
 (*
    FConstList.add('%UsercontextAppdataDir%');
    ValueToTake := '';
    if wispecfolder.specialfolders.indexOf('UsercontextAppdata') > 0
    then valueToTake := wispecfolder.specialfolders.values['UsercontextAppdata'];
    FConstValuesList.add (ValueToTake);
 *)
//  Common (AllUsers) directories:
    FConstList.add('%CommonAppDataDir%');
    ValueToTake := GetCommonAppDataPath;
    FConstValuesList.add (ValueToTake);

    FConstList.add('%AllUsersProfileDir%');
    ValueToTake := guessCommonProfilesPath;
    FConstValuesList.add (ValueToTake);
    FConstList.add('%CommonProfileDir%');
    FConstValuesList.add (ValueToTake);

    FConstList.add('%CommonStartmenuDir%');
    ValueToTake := GetCommonStartmenuPath;
    FConstValuesList.add (ValueToTake);
    FConstList.add ('%CommonStartmenuPath%');
    FConstValuesList.add (ValueToTake);

    FConstList.add('%CommonDesktopDir%');
    ValueToTake := GetCommonDesktopPath;
    FConstValuesList.add (ValueToTake);

    FConstList.add('%CommonStartupDir%');
    ValueToTake := GetCommonStartupPath;
    FConstValuesList.add (ValueToTake);

    FConstList.add('%CommonProgramsDir%');
    ValueToTake := GetCommonProgramsPath;
    FConstValuesList.add (ValueToTake);

    FConstList.add('%AppDataDir%');
    ValueToTake := GetAppDataPath;
    FConstValuesList.add (ValueToTake);
    FConstList.add('%CurrentAppDataDir%');
    FConstValuesList.add (ValueToTake);

    FConstList.add('%CurrentStartmenuDir%');
    ValueToTake := GetStartmenuPath;
    FConstValuesList.add (ValueToTake);

    FConstList.add('%CurrentDesktopDir%');
    ValueToTake := GetDesktopPath;
    FConstValuesList.add (ValueToTake);

    FConstList.add('%CurrentStartupDir%');
    ValueToTake := GetStartupPath;
    FConstValuesList.add (ValueToTake);

    FConstList.add('%CurrentProgramsDir%');
    ValueToTake := GetProgramsPath;
    FConstValuesList.add (ValueToTake);

    FConstList.add('%CurrentSendToDir%');
    ValueToTake := GetSendToPath;
    FConstValuesList.add (ValueToTake);

    FConstList.add('%CurrentProfileDir%');
    ValueToTake := getUserProfilePath;
    FConstValuesList.add (ValueToTake);

    ///AllNtUserProfiles directory constants:
    // they are not defined here and will be replaced
    // at the working section code


    //opsi-script-Path and Directories
    FConstList.add ('%ScriptDrive%');
    ValueToTake := extractfiledrive ( ExpandFilename(Scriptdatei));
    FConstValuesList.add (ValueToTake);

    FConstList.add ('%Scriptpath%');
    ValueToTake := ExtractFileDir (Scriptdatei);
    FConstValuesList.add (ValueToTake);
    FConstList.add ('%ScriptDir%');
    FConstValuesList.add (ValueToTake);

    FConstList.add('%WinstDir%');
    ValueToTake := ExtractFileDir (Paramstr(0));
    FConstValuesList.add (ValueToTake);

    FConstList.add('%WinstVersion%');
    ValueToTake := osconf.WinstVersion;
    FConstValuesList.add (ValueToTake);

    FConstList.add ('%LogFile%');
    FConstValuesList.add ( LogDatei.FileName );

    FConstList.add ('%opsiTmpDir%');
    {$IFDEF WINDOWS}FConstValuesList.add ( 'c:\opsi.org\tmp' ); {$ENDIF WINDOWS}
    {$IFDEF LINUX}FConstValuesList.add ( '/tmp' ); {$ENDIF LINUX}

    FConstList.add ('%opsiLogDir%');
    {$IFDEF WINDOWS}FConstValuesList.add ( 'c:\opsi.org\log' ); {$ENDIF WINDOWS}
    {$IFDEF LINUX}FConstValuesList.add ( '/var/log/opsi-script' ); {$ENDIF LINUX}

    FConstList.add ('%opsiapplog%');
    {$IFDEF WINDOWS}FConstValuesList.add ( 'c:\opsi.org\applog' ); {$ENDIF WINDOWS}
    {$IFDEF LINUX}FConstValuesList.add ( '~/opsi.org/applog' ); {$ENDIF LINUX}

    FConstList.add ('%opsidata%');
    {$IFDEF WINDOWS}FConstValuesList.add ( 'c:\opsi.org\data' ); {$ENDIF WINDOWS}
    {$IFDEF LINUX}FConstValuesList.add ( '/var/lib/opsi-client-agent' ); {$ENDIF LINUX}

    {$IFDEF WINDOWS}
    FConstList.add('%opsiScriptHelperPath%');
    ValueToTake := getSpecialFolder(CSIDL_PROGRAM_FILES)+'\opsi.org\opsiScriptHelper';
    FConstValuesList.add (ValueToTake);



    //Network informations
    FConstList.add ('%PCNAME%');
    ValueToTake := ValueOfEnvVar ('PCNAME');
    if valueToTake =  valueEnvVarNotFound
      then
          valueToTake := ValueOfEnvVar ('computername');
    FConstValuesList.add (ValueToTake);

    FConstList.add ('%HOST%');
    ValueToTake := ValueOfEnvVar ('HOST');
    FConstValuesList.add (ValueToTake);
    {$ENDIF WINDOWS}

    {$IFDEF LINUX}
    FConstList.add ('%PCNAME%');
    ValueToTake := GetHostName;
    if valueToTake =  valueEnvVarNotFound
      then
          valueToTake := getCommandResult('hostname');
    FConstValuesList.add (ValueToTake);

    FConstList.add ('%HOST%');
    ValueToTake := getCommandResult('hostname');
    FConstValuesList.add (ValueToTake);
    {$ENDIF LINUX}



    (*
    FConstList.add ('%MacAddress%');
    FConstValuesList.add (GetMACAddress2);
  *)

    FConstList.add ('%IPAddress%');
    FConstValuesList.add (ipAddress);

    FConstList.add ('%IPName%');
    FConstValuesList.add (ipName);


    FConstList.add ('%Username%');
    GetNetUser ('', ValueToTake, ErrorInfo);
    FConstValuesList.add (ValueToTake);

    {$IFDEF WINDOWS}
    FConstList.add ('%TempUser%');
    FConstValuesList.add (TempUserRegKey);
    {$ENDIF WINDOWS}

    //FConstList.add ('%Wow64%');
    //FConstValuesList.add (IntToStr( iswow64));


    //opsi service values

    FConstList.add ('%opsiserviceURL%');
    FConstValuesList.add (opsiserviceURL);

    FConstList.add ('%opsiserviceUser%');
    FConstValuesList.add (opsiserviceUser);

    FConstList.add ('%opsiservicePassword%');
    FConstValuesList.add (opsiservicePassword);

    FConstList.add ('%hostID%');
    FConstValuesList.add (computernaming);

    FConstList.add ('%opsiServer%');    //

    valuetotake := copy(opsiserviceUrl, pos('//', opsiserviceUrl)+2, length(opsiserviceurl));
    valuetotake := copy(valuetotake, 1, pos(':', valuetotake) - 1);
    FConstValuesList.add (valuetotake);

    FConstList.add ('%opsiDepotId%');
    if opsidata = nil
    then
        FConstValuesList.add ('')
    else
        FConstValuesList.add (trim(Topsi4data(opsidata).depotId));

    FConstList.add ('%installingProdName%');
    if opsidata = nil
    then
        FConstValuesList.add ('')
    else
        FConstValuesList.add (Topsi4data(opsidata).getActualProductId);

    FConstList.add ('%installingProdVersion%');
    if opsidata = nil
    then
        FConstValuesList.add ('')
    else FConstValuesList.add (opsidata.getActualProductVersion);

    FConstList.add ('%installingProduct%');
    if opsidata = nil
    then
        FConstValuesList.add ('')
    else
        FConstValuesList.add (Topsi4data(opsidata).getActualProductId);
  End;




  Aktionsliste := TWorkSection.Create (NestingLevel,nil);
  {$IFDEF GUI}
  FBatchOberflaeche.LoadSkin('');
  FBatchOberflaeche.setPicture ('', '');
  {$ENDIF GUI}
  // initial section
  AktionsListe.Name := NameInitSektion ;
  Script.GetSectionLines(NameInitSektion, TXStringList (Aktionsliste),
                         StartlineOfSection, true, true, true);
  //Script.ApplyTextConstants (TXStringList (Aktionsliste));

  try
    // inital section
    if Aktionsliste.count > 0
    then
       weiter := Script.doAktionen (Aktionsliste, Aktionsliste)
    else
       weiter := tsrPositive;

    // profile actions section : run it only on loginscript
    if (weiter > 0) and runloginscripts then
    begin
      Aktionsliste.clear;
      AktionsListe.Name := NameProfileActionsSection ;
      Aktionsliste.StartLineNo := StartlineOfSection;
      Script.GetSectionLines(NameProfileActionsSection, TXStringList (Aktionsliste),
                             StartlineOfSection, true, true, true);
      Script.ApplyTextConstants (TXStringList (Aktionsliste), false);
      if Aktionsliste.count > 0  then
      begin
        Aktionsliste.StartLineNo := StartlineOfSection;
         weiter := Script.doAktionen (Aktionsliste, Aktionsliste);
         // do not run actions after profileActions
         weiter := 0;
      end
      else
         weiter := tsrPositive;
    end;

    // actions section
    if weiter > 0 then
    Begin
      Aktionsliste.clear;
      Aktionsliste.Name := NameAktionenSektion;
      Aktionsliste.StartLineNo := StartlineOfSection;

      Script.GetSectionLines(NameAktionenSektion, TXStringList (Aktionsliste),
                             StartlineOfSection, true, true, true);
      Script.ApplyTextConstants (TXStringList (Aktionsliste), false);

      if Aktionsliste.count > 0
      then
      Begin
        Aktionsliste.StartLineNo := StartlineOfSection;
        weiter := Script.doAktionen (Aktionsliste, Aktionsliste);
      End;
    End;
    (*
    if opsidata <> nil then
    begin
      LogDatei.log ('opsi service version: '+opsidata.getOpsiServiceVersion, LLessential);
      Logdatei.log('Setup script name: '+opsidata.getProductScriptPath(tacSetup), LLessential);
    end;
    *)
    try
      Aktionsliste.free; Aktionsliste := nil;
    except
     on e: exception do
     {$IFDEF GUI}
     MyMessageDlg.wiMessage ('CreateAndProcessScript : free Aktionsliste: '+e.Message, [mrOK]);
     {$ELSE GUI}
     writeln('CreateAndProcessScript : free Aktionsliste: '+e.Message);
     {$ENDIF GUI}
    end;
  except
    on e: exception do
    Begin
      LogDatei.log('Exception in CreateAndProcessScript: Handling Aktionsliste: ' + e.message, LLError);
      extremeErrorLevel := levelFatal;
    End;
  end;

  freeDefinedFunctions;


  {$IFDEF GUI}
  CentralForm.Memo1.SelectAll;
  CentralForm.Memo1.SelStart := CentralForm.Memo1.SelLength;
  {$ENDIF GUI}

  // write final messages to log


  LogDatei.LogSIndentLevel := 0;

  if Script.FExtremeErrorLevel < extremeErrorLevel
  then extremeErrorLevel := Script.FExtremeErrorLevel;


  ps := ('___________________');
  LogDatei.log (ps, LLessential);
  ps := ('script finished: ');
  if extremeErrorLevel = LevelFatal then ps := ps +'failed'
  else ps := ps +'success';
  LogDatei.log (ps, LLessential);
  ps := IntToStr (Script.NumberOfErrors) + ' error';
  if Script.NumberOfErrors <> 1 then ps := ps + 's';
  LogDatei.log (ps, LLessential);

  ps := IntToStr (Script.NumberOfWarnings) + ' warning';
  if Script.NumberOfWarnings <> 1 then ps := ps + 's';
  LogDatei.log (ps, LLessential);
  ps := '';
  LogDatei.log (ps, LLessential);
  if opsidata <> nil then
  begin
    if runLoginScripts then
      LogDatei.log ('Executed login script for : '+Topsi4data(opsidata).getActualProductId+' Version: '+opsidata.getActualProductVersion , LLessential)
    else
    if runproductlist then
      LogDatei.log ('Executed via runproductlist setup script for : '+Topsi4data(opsidata).getActualProductId+' Version: '+opsidata.getActualProductVersion , LLessential)
    else
    begin
      LogDatei.log ('installed product: '+Topsi4data(opsidata).getActualProductId+' Version: '+opsidata.getActualProductVersion , LLessential);
      LogDatei.log2history ('installed : '+Topsi4data(opsidata).getActualProductId+' Version: '+opsidata.getActualProductVersion );
    end;

    //LogDatei.log ('opsi service version: '+opsidata.getOpsiServiceVersion, LLessential);
  end;
  LogDatei.log (ps, LLessential);

  if LogDatei.ReportErrorsToUser and (Script.NumberOfErrors > 0)
  then
  Begin
   //if Script.NumberOfErrors = 1
     //then ps := 'is '
     //else ps := 'are ';
     {$IFDEF GUI}
     MyMessageDlg.WiMessage ('On installing ' + ps + #10 +
                 IntToStr (Script.NumberOfErrors) +  '  '  +
                 'error(s) occurred ' + #10 +
                 'Please inform your administrator',
                             [mrOK]);
     {$ENDIF GUI}
  End;
  {$IFDEF WIN32}
  if opsiSetupAdmin_created then
    if DeleteTemporaryLocalAdmin then
      LogDatei.log ('Temporary Local Admin deleted', LLDebug2);
  {$ENDIF WIN32}
  if Logdatei.LogLevel < LLconfidential then
  begin
    deleteTempBatFiles('');
    LogDatei.log ('Temp cmd files deleted, next: free script ', LLDebug2);
  end;

  (*
  moved before final output
  if Script.FExtremeErrorLevel < extremeErrorLevel
  then extremeErrorLevel := Script.FExtremeErrorLevel;
  *)

  // reset current dir to the start value
  SetCurrentDir(opsiWinstStartdir);
  scriptsuspendstate := script.Suspended;

  {$IFDEF WIN32}
  // Unblock Input
  winBlockInput(false);
  {$ENDIF WIN32}

  try
    Script.free; Script := nil;
  except
   on e: exception do
   {$IFDEF GUI}
   MyMessageDlg.wiMessage ('CreateAndProcessScript : free Aktionsliste: '+e.Message, [mrOK]);
   {$ELSE GUI}
   writeln('CreateAndProcessScript : free Aktionsliste: '+e.Message);
   {$ENDIF GUI}
  end;
    LogDatei.log ('End of CreateAndProcessScript', LLDebug2);
    //LogDatei.Close;
  except
    on e: exception do
    Begin
      LogDatei.log('Exception in CreateAndProcessScript: General' + e.message, LLError);
      LogDatei.Close;
    End;
  end;
end;



begin
  randomize;

  //PreDefinedVariableSkinDirectory  := '$skindirectory$';
  //PreDefinedVariableSkinDirectoryValue := ExtractFileDir (Paramstr(0));

  GetMem (PStatNames, sizeof (TStatementNames));
  PStatNames^ [tsNotDefined]          := '';
  PStatNames^ [tsActions]             := 'Sub';
   PStatNames^ [tsProfileActions]      := 'ProfileAction';
  PStatNames^ [tsPatchAnyTextfile]    := 'PatchTextFile';
  PStatNames^ [tsTests]               := 'Tests';
  //PStatNames^ [tsTestCommand]         := 'Testcommand';
  PStatNames^ [tsPatchIniFile]        := 'Patches';
  PStatNames^ [tsHostsPatch]          := 'PatchHosts';

  PStatNames^ [tsRegistryHack]        := 'Registry';
  PStatNames^ [tsXMLPatch]            := 'XMLPatch';
  PStatNames^ [tsIdapiConfig]         := 'IdapiConfig';
  PStatNames^ [tsLDAPsearch]          := 'LDAPsearch';
  PStatNames^ [tsFileActions]         := 'Files';
  PStatNames^ [tsLinkFolder]          := 'LinkFolder';
  PStatNames^ [tsWinBatch]            := 'WinBatch';
  PStatNames^ [tsDOSBatchFile]        := 'DOSBatch';
  PStatNames^ [tsDOSInAnIcon]         := 'DOSInAnIcon';
  PStatNames^ [tsShellBatchFile]      := 'ShellBatch';
  PStatNames^ [tsShellInAnIcon]       := 'ShellInAnIcon';
  PStatNames^ [tsExecutePython]       := 'ExecPython';
  PStatNames^ [tsExecuteWith]         := 'ExecWith';
  PStatNames^ [tsExecuteWith_escapingStrings] := 'ExecWith_escapingStrings'; // is implemented via parameterEscapeStrings
  PStatNames^ [tsKillTask]            := 'KillTask';
  PStatNames^ [tsOpsiServiceCall]     := 'OpsiServiceCall';
  PStatNames^ [tsOpsiServiceCallStat] := 'OpsiServiceCall';
  PStatNames^ [tsOpsiServiceHashList] := 'OpsiServiceHashList';
  PStatNames^ [tsDDEwithProgman]      := 'ProgmanGroups';

  PStatNames^ [tsWorkOnStringList]    := 'WorkOnStringList';

  PStatNames^ [tsStayWhileWindowOpen] := 'StayWhileWindowOpen';
                   (* fuer Testzwecke, nicht dokumentiert *)

  PStatNames^ [tsCondOpen]            := 'If';
  PStatNames^ [tsCondThen]            := 'Then';
  PStatNames^ [tsCondElse]            := 'Else';
  PStatNames^ [tsCondClose]           := 'EndIf';

  // switch
  PStatNames^ [tsSwitch]              := 'Switch';
  PStatNames^ [tsSwitchCaseOpen]      := 'Case';
  PStatNames^ [tsSwitchCaseClose]     := 'EndCase';
  PStatNames^ [tsSwitchDefaultOpen]   := 'DefaultCase';
  PStatNames^ [tsSwitchClose]         := 'EndSwitch';


  PStatNames^ [tsLoopStringList]      := 'for';
  PStatNames^ [tsLoopForTo]           := 'for';

  PStatNames^ [tsMessage]             := 'Message';
  PStatNames^ [tsMessageFile]         := 'ShowMessageFile';
  PStatNames^ [tsShowBitmap]          := 'ShowBitmap';
  PStatNames^ [tsPause]               := 'Pause';
  PStatNames^ [tsSleep]               := 'SleepSeconds';
  PStatNames^ [tsStop]                := 'Stop';
  PStatNames^ [tsComment]             := 'Comment';
  PStatNames^ [tsActionProgress]      := 'setActionProgress';
  PStatNames^ [tsLogError]            := 'LogError';
  PStatNames^ [tsLogWarning]          := 'LogWarning';
  PStatNames^ [tsSetSkinDir]          := 'SetSkinDirectory';

  PStatNames^ [tsImportLib]           := 'ImportLib';
  PStatNames^ [tsIncludeInsert]       := 'Include_Insert';
  PStatNames^ [tsIncludeAppend]       := 'Include_Append';
  PStatNames^ [tsIncludeLog]          := 'IncludeLog';
  PStatNames^ [tsShrinkFileToMB]      := 'ShrinkFileToMB';
  PStatNames^ [tsChangeDirectory]     := 'ChangeDirectory';



  PStatNames^ [tsExitWindows]         := 'ExitWindows';
  PStatNames^ [tsLocalAdmin]          := 'LocalAdmin';
  PStatNames^ [tsBlockInput]          := 'BlockInput';


  PStatNames^ [tsAddConnection]       := 'AddConnection';
                 (* nicht dokumentiert *)

  PStatNames^ [tsSetOldLogLevel]         := 'LogLevel';
  PStatNames^ [tsSetLogLevel]            := 'SetLogLevel';
  PStatNames^ [tsSetOutputLevel]         := 'SetOutputLevel';
  PStatNames^ [tsSetExitOnError]         := 'ExitOnError';
  PStatNames^ [tsFatalOnSyntaxError]     := 'FatalOnSyntaxError';
  PStatNames^ [tsFatalOnRuntimeError]    := 'FatalOnRuntimeError';
  PStatNames^ [tsAutoActivityDisplay]    := 'AutoActivityDisplay';

  PStatNames^ [tsSetConfidential]        := 'SetConfidential';

  PStatNames^ [tsSetFatalError]          := 'IsFatalError';
  PStatNames^ [tsSetSuccess]             := 'IsSuccess';
  PStatNames^ [tsSetSuspended]           := 'IsSuspended';
  PStatNames^ [tsSetNoUpdate]            := 'noUpdateScript';
  PStatNames^ [tsSetMarkerErrorNumber]   := 'MarkErrornumber';
  PStatNames^ [tsSetReportMessages]      := 'ScriptErrorMessages';
  PStatNames^ [tsSetTimeMark]            := 'MarkTime';
  PStatNames^ [tsLogDiffTime]            := 'DiffTime';
  PStatNames^ [tsSetTraceMode]        := 'TraceMode';
  PStatNames^ [tsSetStayOnTop]        := 'StayOnTop';
  PStatNames^ [tsIconizeWinst]        := 'IconizeWinst';
  PStatNames^ [tsRestoreWinst]        := 'RestoreWinst';
  PStatNames^ [tsNormalizeWinst]      := 'NormalizeWinst';
  PStatNames^ [tsMaximizeWinst]       := 'MaximizeWinst';
  PStatNames^ [tsEncoding]            := 'encoding';
  PStatNames^ [tsUpdateEnvironment]   := 'UpdateEnvironment';
  PStatNames^ [tsLoadProductProperties] := 'LoadProductProperties';
  PStatNames^ [tsWinstVersionRequired] := 'RequiredWinstVersion';
  PStatNames^ [tsSetUsercontext]       := 'SetUserContext';
  PStatNames^ [tsSaveVersionToProfile]       := 'saveVersionToProfile';



  PStatNames^ [tsDefineVar]           := 'DefVar';
  PStatNames^ [tsDefineStringList]    := 'DefStringList';
  PStatNames^ [tsSetVar]              := 'Set';
  PStatNames^ [tsShellCall]           := 'shellCall';
  PStatNames^ [tsPowershellCall]      := 'powershellCall';
  PStatNames^ [tsDefineFunction]      := 'DefFunc';
  PStatNames^ [tsEndFunction]         := 'EndFunc';


  runProfileActions := false;
  runLoginScripts := False;
  allLoginScripts := True;
  inUsercontext := False;
  runSilent := False;
  flag_all_ntuser := False;
  runproductlist := False;
  scriptMode := tsmMachine;


end.
