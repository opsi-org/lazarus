unit osfunc;

{$MODE Delphi}
//{$RANGECHECKS ON}
{$OVERFLOWCHECKS ON}
{$IOCHECKS ON}
{$OBJECTCHECKS ON}
{$VARSTRINGCHECKS ON}
{$LONGSTRINGS ON}


(* contains base functions *)

// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: Rupert Roeder, detlef oertel



interface

uses
{$IFDEF WINDOWS}
  oswinacl,
  JwaWindows,
  JwaWinType,
  jwatlhelp32,
  jwawinbase,
  JwaSddl,
  JwaWinNT,
  jwaAclApi,
  JwaAccCtrl,
  JwaProfInfo,
  JwaUserEnv,
  //JclMiscel,
  //JclBase,
  //JclSecurity,
  Windows,
  ActiveX, (* zur Laufzeit von CoCreateInstance benoetigt *)
  winsock,
  //registry,
  VersionInfoX,
  //unitImpersonator,
  osfuncwin2,
  osfuncwin3,
{$IFDEF WIN32}
  osregistry,
  DSiWin32,
  oslocaladmin,
  packdefs,
  zipinter,
  //uCpuUsage,
{$ENDIF WIN32}
  //unitImpersonator,
  (* for infozip  *)

  shellapi,
  Shlobj,
{$ENDIF}
{$IFDEF GUI}
  osGUIControl,
  Forms,
  Graphics,
  LResources,
  LCLIntf,
  LCLProc,
{$ENDIF GUI}
{$IFDEF UNIX}

  baseunix,
  unix,
  types,
  dateutils,
  //initc,
  osfuncunix,
{$ENDIF UNIX}
  //LConvEncoding,
  blcksock,
  synautil,
  synaip,
  osencoding,
  LazUTF8,
  FileUtil,
  LazFileUtils,
  process,
  osprocesses,
  SysUtils,
  Classes,
  charencstreams,
  osconf,
  oslog,
  osparserhelper,
  osurlparser,
  crt,
  strutils,
  lconvencoding,
  lcltype,
  ostxstringlist,
  osstartproc_cp,
  pipes,
  oszip,
  osfilehelper,
  osnetutil,
  math;

const
  BytesarrayLength = 5000;
  PATHSEPARATOR = PathDelim;
  READ_BYTES = 2048;
//KEY_WOW64_64KEY = $0100;
//KEY_WOW64_32KEY = $0200;



type
  //  TCharset = set of char;
  TcpSpecify = integer;
  TcpCountModus = (tccmNoCounter, tccmCounting, tccmCounted);
  TExitMode = (txmNoExit, txmLogout, txmReboot);
  TuibOSVersion = (tovNotKnown, tovWin16, tovWin95, tovWinNT, tovLinux, tovMacOS);
  TuibNTVersion = (tntverNONE, tntverNT3, tntverNT4, tntverWIN2K,
    tntverWINVISTA, tntverWINX);
  Str20 = string

    [20];

  tbytearray = array [0..BytesarrayLength] of byte;

  FileNameStr = string;
{$IFNDEF  WIN32}
  TRunAs = (traPcpatch, traInvoker, traAdmin, traAdminProfile, traAdminProfileExplorer,
    traAdminProfileImpersonate, traAdminProfileImpersonateExplorer, traLoggedOnUser);
{$ENDIF}

  TShowOutputFlag = (tsofHideOutput, tsofShowOutput, tsofShowOutputNoSystemInfo);



(*
{ gone to ostxstringlist }
  TXStringList = class(TStringList)
  public
    function ReplaceInLine(const OldLine, SearchItem, Replaceitem: string;
      MatchCase: boolean; var NewLine: string): boolean;
    function GlobalReplace(Startline: integer; const SearchItem, Replaceitem: string;
      MatchCase: boolean): boolean;
    procedure EliminateLinesStartingWith(const startS: string; MatchCase: boolean);
    procedure SaveToFile(const FileName: string; encodingtype: string); overload;
    procedure SaveToFile(const FileName: string); override; overload;
    procedure SaveToFile(const FileName: string; encodingtype: string;
      raise_on_error: boolean); overload;
    function FuncSaveToFile(const FileName: string; encodingtype: string): boolean;
      overload;
    function FuncSaveToFile(const FileName: string): boolean; overload;
    procedure loadFromUnicodeFile(const Filename: string; codepage: word);
    function getStringValue(const keyname: string): string;
    // returns the string value of a handmade properties list with separator either '=' or ':'
    // we return values[keyname], until further notice
  end;
*)

  TPatchList = class(TXStringList)
  private
    ip: integer;
  public
    procedure Delete(StrNo: integer);
    procedure SetItemPointer(const newvalue: integer);
    function FindFirstItem
      (const SearchString: string; MatchCase: boolean; startIndex: integer;
      var found: boolean): integer;
    function FindFirstItemWith
      (const SearchString: string; MatchCase: boolean;
      startIndex: integer): integer;
    function FindFirstItemStartingWith
      (const SearchString: string; MatchCase: boolean;
      startIndex: integer): integer;
    property ItemPointer: integer read ip write SetItemPointer;
    procedure Add(const s: string);
    procedure Insert(i: integer; const s: string);

  end;


  TEditScript = class(TXStringList)
  protected
    procedure SetString(const i: integer; const s: string);
    (* sets the string with number i, if i < count
       else add the string to the string list *)
  public
  end;


  TuibIniScript = class(TEditScript)
  protected
    function FindEndOfSectionIndex(const OffLine: integer): integer;
    function FindSectionheaderIndex(const Sectionname: string): integer;
  public
    procedure GetSectionTitles(var resultlist: TXStringlist);
    procedure GetSectionNames(var resultlist: TXStringlist);
    function GetSectionLines(const Sectionname: string;
      var Resultlist: TXStringList; var StartlineNo: integer;
      takeCommentLines, takeEmptyLines, trimmed: boolean): boolean;
       (*
          reads alle lines of a sektion including the title into a txstringlist
          returns true if there is a non-empty section

        *)

    (*    Procedure ReadSection (const Sectionname: string; var Resultlist : TStringList);
          Liest alle Idents einer Sektion mit der Zeilenstruktur Ident=Value in Strings ein *)
  end;


  TuibPatchIniFile = class(TuibIniScript)
  protected
    function FindIdentIndex(const Sectionname, Ident: string): integer;
    function FindEntryIndex(const Sectionname, Ident, Value: string): integer;
    function FindValueIndex(const Sectionname, Value: string): integer;


  public
    constructor Create;
    destructor Destroy; override;

    procedure SaveToFile(const Filename: string);

    procedure addSection(const Sektion: string);
    (* fuegt eine Sektion hinzu,, sofern sie nicht bereits existiert *)


    procedure addEntry(const Sektion, Eintrag: string);
      (* Fuegt einen Eintrag
      Ident = Value
      in eine Sektion ein, falls der vordere Teil
      Ident =
      noch nicht existiert, sonst wird nichts geschrieben
      Falls die Sektion noch nicht da ist wird er erstellt. *)
    procedure delEntry(const Sektion, Eintrag: string);
      (* Loescht einen Eintrag,
      Wenn kompletter Eintrag
      Ident = Value
      in Sektion nicht gefunden wird,
      wird angenommen (und ueberprueft), dass Eintrag die Form
      Ident
      hat.
      Der erste Eintrag mit dem Schluesselnamen Ident in Sektion wird
      dann entfernt *)
    procedure setEntry(const Sektion, Eintrag: string);
      (* Setzt Eintrag Ident = Value in Sektion
      Falls Sektion oder Eintrag nicht existiert wird er erstellt
      Falls der Eintrag mit anderem Wert nach dem Gleichheitszeichen
      vorhanden ist,
      Ident = x
      wird er
      - beim ersten Vorkommen von Ident! -
      umgesetzt auf
      Ident = Value *)
    procedure addNewEntry(const Sektion, Eintrag: string);
      (*  Fuegt einen Eintrag in einen Sektion ein, falls er noch nicht existiert
      Falls die Sektion noch nicht da ist wird er erstellt. *)
    procedure delSec(const Sektion: string);
    (*  Loescht Sektion *)
    procedure changeEntry(const Sektion, Eintrag: string);
    (* Aendert einen Eintrag nur, wenn er bis zum = vorhanden ist *)
    procedure replaceEntry(const AlterEintrag, Eintrag: string);
    (* Ersetzt AlterEintrag in allen Sektionen durch Eintrag *)

    //historisch:
    //procedure addProgManGroup (const NeueGruppe: String);
    //procedure delProgManGroup (const AlteGruppe: String);

  end;

  TuibIniFile = class(TuibPatchIniFile)
  private
    filename: string;
    saved: boolean;
  public
    constructor Create(inifilename: string);
    destructor Destroy; override;
    procedure ReadSection(const Sectionname: string; var SectionVars: TStringList);
    procedure ReadSectionValues(const Sectionname: string;
      var SectionValues: TStringList);
    procedure ReadRawSection(const SectionName: string; var RawSection: TXStringList);
    function ReadString(const Section, Ident, defaultvalue: string): string;
    procedure WriteString(const Section, Ident, Value: string);
    procedure SaveBack;
  end;


  TuibPatchHostsFile = class(TEditScript)
  private
    LineIdents, UpperLineIdents: TStringList;
    function FindAndSplitLine
      (const SearchIdent: string;
      var ipAddress, Hostname, Alias, comment: string): integer;

     (* Looks if SearchIdent exists in a hostsline before the commentarea.

        If it does not exist the function  returns -1; the result variables remain empty.
        If it is found the function returns the corresponding line index;
        the result variables contain the 4 components of the line;
        the member variables LineIdents and UpperLineIdents contain (in original writing/
        converted to uppercase) ipNo, hostname and
        the different aliases of the line in question *)

    procedure MakeLine(i: integer; s: string);

  public
    constructor Create;
    destructor Destroy; override;


    procedure SetAddress(const Hostname, ipAddress: string);
      (* setzt die IP-Adresse fuer den Host  <hostname> auf  <ipadresse>.
         Falls noch kein Eintrag fuer den Host <hostname> besteht,
         wird er neu eingetragen *)

    procedure SetName(const ipAddress, Hostname: string);
      (* setzt den Namen des Hosts mit der angegebenen IP-Adresse  <ipadresse> auf <hostname>.
         Falls noch kein Eintrag mit der IP-Adresse <ipadresse> existiert,
         wird er neu erzeugt. *)

    procedure SetAlias(const Ident, Alias: string);
      (* fuegt fuer den Host, identifiziert durch <Ident>, wobei
         <Ident> entweder als Name (oder Alias) oder als ipAdresse interpretiert
         werden kann, den ALIAS-Namen  <Alias> ein. *)


    procedure DelAlias(const Ident, Alias: string);
      (* loescht aus dem Eintrag fuer den Host, gegeben durch IP-Adresse, Hostname oder Alias
         den Alias <alias> *)


    procedure DelHost(const Ident: string);
    (* loescht den Eintrag des mit IP-Namen, IP-Adresse oder Alias gegebenen Hosts *)

    procedure SetComment(const Ident, Comment: string);
    (* setzt den Kommentar zum Eintrag des mit IP-Namen, IP-Adresse oder Alias gegebenen Hosts neu *)

    function GetAddress(const Ident: string; var ipAddress: string): boolean;
    (* get address for Hostname or Alias <Ident> *)

    function GetHostname(const ipAddress: string; var Hostname: string): boolean;
    (* get Hostname for ipAddress (or Alias) *)
  end;




  TuibFileInstall = class(TObject)
  private
    copyDenyList: TStringList;
    function GetReadOnlyAttribute
      (const FileName: string; eliminate: boolean): boolean;

  public
    constructor Create;
    destructor Destroy; override;

    function FileCheckDate
      (const Sourcefilename, Targetfilename: string;
      OverwriteIfEqual: boolean): boolean;
    procedure AllCopy(const SourceMask, Target: string; cpSpecify: TcpSpecify;
      CountModus: TcpCountModus; var NumberCounted: integer;
      var RebootWanted: boolean);

    function MakePath(const Dirname: string): boolean;

    function AllDelete
      (const Filename: string; recursive, ignoreReadOnly: boolean;
      daysback: integer; logleveloffset: integer = 0): boolean; overload;

    function AllDelete
      (const Filename: string; recursive, ignoreReadOnly: boolean;
      daysback: integer; search4file: boolean; var RebootWanted: boolean;
      logleveloffset: integer = 0): boolean;
      overload;

    function AllDelete
      (const Filename: string; recursive, ignoreReadOnly: boolean;
      daysBack: integer; search4file: boolean; var RebootWanted: boolean;
      retryOnReboot: boolean; logleveloffset: integer = 0): boolean; overload;

    function HardLink(existingfilename, newfilename: string): boolean;
    function SymLink(existingfilename, newfilename: string): boolean;
    function Rename(existingfilename, newfilename: string;
      var RebootWanted: boolean): boolean;
    {$IFDEF WINDOWS}
    function FileCheckVersions(const SourceF, TargetF: string;
      restrictedToTargetDir: boolean): boolean;

    procedure AllCompress(const SourceMask, TargetDir: string;
      Recursive: boolean; MaintainEmptySubdirs: boolean);
    {$ENDIF WINDOWS}
    {$IFDEF UNIX}
    function chmod(mode: string; const FileName: string): boolean;
    function calcMode(mode: string; const FileName: string): string;
    {$ENDIF UNIX}
  end;

  {$IFDEF WINDOWS}
  (* Shell-Links *)

  TuibShellLinks = class(TInterfacedObject)
  private
    MySystemfolder: integer;
    MyFoldername: string;
    MyFolderpath: WideString;
    MyFiles: TuibFileInstall;
    FolderOpened: boolean;
    StartIndentLevel: integer;
  public
    constructor Create;
    destructor Destroy; override;
    function Tell_SystemFolder(const Systemfolder: integer): string;
    function ShowShellFolderWindow: boolean;
    function OpenShellFolderPath(const Systemfolder: integer;
      const foldername: string): boolean;
    function MakeShellLink
      (const description, thePath, commandline_arguments, working_directory,
      iconPath: string; const icon_index: integer): boolean; overload;
    function MakeShellLink
      (const description, thePath, commandline_arguments, working_directory,
      iconPath: string; const icon_index: integer; shortcut: word): boolean; overload;
    function MakeShellLink
      (const description, thePath, commandline_arguments, working_directory,
      iconPath: string; const icon_index: integer; shortcut: word;
      showwindow: integer): boolean; overload;
    function DeleteShellLink(const description: string): boolean;
    function DeleteShellFolder(const SystemFolder: integer;
      const foldername: string): boolean;

  end;

{$ENDIF WINDOWS}

{$IFDEF WINDOWS}
function GetOSId: DWord;
function SetProcessPrivilege(PrivilegeName: string): boolean;

(*
function GetWinDirectory: string;
function GetWinSystemDirectory: string;
function GetSystemDefaultLocale(const typeOfValue: DWord): string;
function GetOSVersionEx(var lpVersionInformation: TOSVersionInfoEx): LPBOOL;
  stdcall; external kernel32 Name 'GetVersionExA';
function GetSystemOSVersionInfoEx(const typeOfValue: string): string;
*)
function getExecutableName(FileName: string): string;


function GetDrives: string;
function CheckDriveSpaceForTarget
  (const TargetFilename: string (* directory name is sufficient *);
  const SourceSize: int64; var ProblemInfo: string): boolean;

(*
function CreateHardLink(lpFileName: pchar; lpExistingFileName :pchar;
      lpSecurityAttributes: pointer): LPBOOL; external 'CreateHardLink@kernel32.dll stdcall';
function CreateSymbolicLink(lpSymlinkFileName: pchar; lpExistingFileName :pchar;
      dwFlags: DWORD): LPBOOL; stdcall; external kernel32 Name 'CreateSymbolicLink';
 *)

{$ENDIF WINDOWS}

procedure FindLocalIPData(var ipName: string; var address: string);

function ExitSession(exitmode: TExitMode; var Fehler: string): boolean;
function GetNetDrive(const pathname: string): string;



{$IFNDEF WIN64}
function KillTask(ExeFileName: string; var info: string): boolean;
{$ENDIF WIN64}

{$IFDEF WIN32}
function SetFilePermissionForRunAs(filename: string; runas: TRunAs;
  var errorCode: DWORD): boolean;
{$ENDIF WIN32}

function StartProcess(CmdLinePasStr: string; ShowWindowFlag: integer;
  WaitForReturn: boolean; WaitForWindowVanished: boolean;
  WaitForWindowAppearing: boolean; WaitForProcessEnding: boolean;
  Ident: string; WaitSecs: word; var Report: string; var ExitCode: longint): boolean;
  overload;

function StartProcess(CmdLinePasStr: string; ShowWindowFlag: integer;
  WaitForReturn: boolean; WaitForWindowVanished: boolean;
  WaitForWindowAppearing: boolean; WaitForProcessEnding: boolean;
  RunAs: TRunas; Ident: string; WaitSecs: word; var Report: string;
  var ExitCode: longint): boolean; overload;

function StartProcess(CmdLinePasStr: string; ShowWindowFlag: integer;
  WaitForReturn: boolean; WaitForWindowVanished: boolean;
  WaitForWindowAppearing: boolean; WaitForProcessEnding: boolean;
  waitsecsAsTimeout: boolean; RunAs: TRunas; Ident: string; WaitSecs: word;
  var Report: string; var ExitCode: longint): boolean; overload;

function StartProcess(CmdLinePasStr: string; ShowWindowFlag: integer;
  showoutputflag: TShowOutputFlag; WaitForReturn: boolean;
  WaitForWindowVanished: boolean; WaitForWindowAppearing: boolean;
  WaitForProcessEnding: boolean; waitsecsAsTimeout: boolean; RunAs: TRunas;
  Ident: string; WaitSecs: word; var Report: string; var ExitCode: longint;
  catchout: boolean; var output: TXStringList): boolean; overload;

function StartProcess(CmdLinePasStr: string; ShowWindowFlag: integer;
  showoutputflag: TShowOutputFlag; WaitForReturn: boolean;
  WaitForWindowVanished: boolean; WaitForWindowAppearing: boolean;
  WaitForProcessEnding: boolean; waitsecsAsTimeout: boolean; RunAs: TRunAs;
  Ident: string; WaitSecs: word; var Report: string; var ExitCode: longint;
  catchout: boolean; var output: TXStringList; showtitle: string): boolean; overload;



function GetUibNTVersion(var ErrorInfo: string): TuibNTVersion;
function GetUibOSType(var errorinfo: string): TuibOSVersion;


function FormatInt(const Value: int64): string;

function IsDirectory(const FName: string): boolean;
function SizeOfFile(FName: string): int64;
function ExtractFileDir(const FileName: string): string;
function ExpandFileName(const FileName: string): string;


function FileGetWriteAccess(const Filename: string; var ActionInfo: string): boolean;


procedure MakeBakFile(const FName: string);

// moved to osfilehelper (do 4.6.2021)
// procedure MakeBakFiles(const FName: string; maxbaks: integer);

function FileCopy
  (const sourcefilename, targetfilename: string; var problem: string;
  DelayUntilRebootIfNeeded: boolean; var RebootWanted: boolean): boolean; overload;
function FileCopy
  (const sourcefilename, targetfilename: string; var problem: string;
  DelayUntilRebootIfNeeded: boolean; var RebootWanted: boolean;
  followSymlink: boolean): boolean; overload;
function FileCopy
  (const sourcefilename, targetfilename: string; var problem: string;
  DelayUntilRebootIfNeeded: boolean; var RebootWanted: boolean;
  followSymlink: boolean; doNotChangeAccessRights: boolean): boolean; overload;


function Is64BitSystem: boolean;
function getOSArchitecture: string;
function runningAsAdmin: boolean;
function isUefi: boolean;
function isWinPE: boolean;
function isGUI: boolean;
function runningInWAnMode: boolean;


function CheckFileExists(const FName: string; var ErrorInfo: string): boolean;
function CreateTextfile(const FName: string; var ErrorInfo: string): boolean;

function FindInSubDirs(const dir: string; const filename: string): TStringList;

function PointerAufString(Alpha: string): PChar;
//procedure DisposeString(p: PChar);

function removeLineBreaks(const s0: string): string;

function StrIsIn(const s: string; const A: array of Str20): integer;
function isAbsoluteFileName(const s: string): boolean;
function GetFileInfo(const CompleteName: string; var fRec: TSearchRec;
  var ErrorInfo: string): boolean;

procedure stringsplit(const s, delimiter: string; var Result: TXStringList);
// produziert eine Stringliste aus den Teilstrings, die zwischen den Delimiter-Strings stehen


function concatPathParts(const part1, part2: string): string;
(* concatenates parts of a path, e.g. the main path part and the file name, observing that just path separator is set *)


function StringReplace(const S, OldPattern, NewPattern: string): string;
(* replaces every occurence of OldPattern by NewPattern ; CaseSensitive *)
function StringReplace1(const S, OldPattern, NewPattern: string): string;
(* replaces every occurence of OldPattern by NewPattern ; not CaseSensitive *)

function CEscaping(const s: string): string;

procedure WortAbspalten(const s: string; var Wortlinks, Rest: string);
procedure IdentAbspalten(const s: string; var Ident, Value: string);
procedure SectionnameAbspalten(const s: string; var Sektion, Rest: string);

procedure str2jsonstr(var str: string; var errorstr: string);
function getProcessList: TStringList;
function getProcessListWithPath: TStringList;
function getLoggedInUser: string;
function randomstr(usespecialchars: boolean): string;
function randomstrWithParameters(minLength, nLowerCases, nUpperCases,
  nDigits, nSpecialChars: integer): string;
procedure ShrinkFileToMB(filename: string; newsize: integer);
procedure ChangeDirectory(newdir: string);
function strContains(const str: string; const substr: string): boolean;
function createNewOpsiHostKey: string;
function getProfilesDirList: TStringList;
function opsiunquotestr(s1, s2: string): string;

function cmdLineInputDialog(var inputstr: string; const message, default: string;
  confidential: boolean): boolean;
//function isValidUtf8String(str:string) : boolean;
//function getFixedUtf8String(str:string) : string;
function posFromEnd(const substr: string; const s: string): integer;
function isSymLink(filepath: string): boolean;
function resolveSymlink(const filepath: string; recursive: boolean = True): string;

function isNumeric(s: string): boolean;
function isBoolean(s: string): boolean;

function GetFQDN: string;
procedure noLockSleep(const Milliseconds: DWord);
{$IFDEF WINDOWS}
function DeleteFileWithRetries(filename: string): boolean;
{$ENDIF WINDOWS}

const

  // NULL_STRING_VALUE = 'NULL';

  LineIsCommentChar = ';';
  MultiszVisualDelimiter = '|';

  // additive (mask) sonstanst for TcpSpecify
  cpFlat = 0;  // ro recursion, no subdirectories, over write
  cpCreateEmptySubdirectories = 1;
  cpRecursive = 2;
  cpVersionControl = 4;  // overwrite controled by internal version (Windows)
  cpDateControl = 8;  // no overwrite of younger exe files
  cpUpdate = 16; // no overwrite if target younger or equal old
  cpDontForceOverwrite = 32; // overwrite if write protected
  cpNoOverwrite = 64; // no overwrite
  cpExtract = 128; // extract on copy
  cpLeaveReadonly = 256;

  cpNoExtraReboot = 512; (* if on copying a file being in use cannot be overwritten, then
                            the overwrite process is postponed after the next reboot. By default, in this case
                            the winst copy command sets the reboot flag. This Option turns this special reboot off*)

  cpVersionControlRestricted = 1024;
                                  (* Overwrite bei Dateien mit Extension in ExtensionsForVersionsControl
                                    nur nach vorheriger Versionskontrolle, beschraenkt auf
                                    Dateien im gleichen Verzeichnis wie die Zieldatai *)

  cpFollowSymlinks = 2048; // resolve symlinks while copy


  ExtensionsForVersionControl: array [1..13] of Str20 =
    ('EXE', 'DLL', 'FON', 'VBX', 'DRV',
    'CPL', 'ACM', '386', 'TSK', 'D32',
    'OLB', 'MOD', '3GR');
  // nicht mehr verwendet seit 3.07

  LangPreferenceOrder: array [1..4] of Str20 =
    ('German', 'SwissGerman', 'USEnglish', 'UKEnglish');

  {$IFDEF WINDOWS}
  StandardTempPath = 'c:\opsi.org\tmp\';
  StandardUsercontextSubPath = 'opsi\tmp\';
  {$ELSE WINDOWS}
  StandardTempPath = '/tmp/';
  UniventionTempPath = '/var/tmp/';
  StandardUsercontextSubPath = 'opsi/tmp/';
  {$ENDIF WINDOWS}
  IPPatchLogFilename =  (* TempPath + *) 'iplog.txt';

  emptyregfileName = 'winst_emptyregkey.dat';

  CitMark = '"';

  (*
  ddeTimerInterval = 100;
  ddeTimerWaitIntervals = 20;
    // give DDE with Program 2 seconds, since DDE response seems not to be
    //   correctly transferred by Delphi
  *)
  CharsForRandomStrings = ['A'..'Z', 'a'..'z', '0'..'9'];

  //CharsSpecialForRandomStrings = ['!','$','(',')','*','+','/',';','=','?','[',']','{','}','ß','~','§','°'];
  CharsSpecialForRandomStrings =
    ['!', '$', '(', ')', '*', '+', '/', ';', '=', '?', '[', ']', '{', '}', '~'];

  CharsUpperForRandomStrings = ['A'..'Z'];
  CharsLowerForRandomStrings = ['a'..'z'];
  CharsNumericForRandomStrings = ['0'..'9'];




implementation



uses
{$IFDEF UNIX}
  osfunclin,
  osprocessux,
{$ENDIF UNIX}
{$IFDEF DARWIN}

  osfuncmac,
{$ENDIF DARWIN}
{$IFDEF WINDOWS}
  osfuncwin,
{$ENDIF WINDOWS}
  {$IFDEF GUI}
  osbatchgui,
  osinteractivegui,
  osshowsysinfo,
  {$ENDIF GUI}
  osmain,
  osdefinedfunctions,
  //windatamodul,
  superobject;

type
  TcompressMethod = (tcmNull, tcmZIP, tcmLZH, tcmLHA);

  TCopyCount = class(TObject)
  private
    FActCount: integer;
    FTotal: integer;
    FReady: boolean;
    FCountMode: TcpCountModus;

  public
    constructor Create(CountMode: TcpCountModus; NumberCounted: integer);

    procedure Init(NumberCounted: integer);
    function CheckReady: boolean;
    function TellRatio: real;

    property ActCount: integer read FActCount;
    property ratio: real read TellRatio;
    property ready: boolean read CheckReady write FReady;
  end;



var
  Ident: string = '';
  Value: string = '';
  LogSCommand: string = '';
  LogS: string = '';
  Logged: boolean;

//ps: string;
//FpcProcess: TProcess;




const

  ExtractTempSubDirectory = 'decompr' + PathDelim;
  UsedFilesTempSubDirectory = 'usedfs' + PathDelim;

(* Hilfsfunktionen *)

(* ---- allgemeine Systemaufrufe --- *)

function posFromEnd(const substr: string; const s: string): integer;
var
  revstr: string;
  len, posi: integer;
begin
  Result := 0;
  revstr := ReverseString(s);
  len := length(s);
  posi := Pos(ReverseString(substr), revstr);
  if posi > 0 then
    Result := len - (posi - 1);
end;

(*
function isValidUtf8String(str:string) : boolean;
begin
  if FindInvalidUTF8Codepoint(PChar(str), Length(str)) <> -1 then result := false
  else result := true;
end;
*)

(*
function getFixedUtf8String(str:string) : string;
begin
  result := ValidUTF8String(str);
end;
*)
function cmdLineInputDialog(var inputstr: string; const message, default: string;
  confidential: boolean): boolean;
var
  c: char;
begin
  inputstr := '';
  Result := True;
  writeln;
  writeln(message);
  Write('[' + default + ']  ');
  repeat
    c := readkey;
    if (c = char(27)) then
      Result := False;
    if not confidential then
      Write(c)
    else
      Write('*');
    if (c >= char(32)) then
      inputstr := inputstr + c;
  until (c = LineEnding) or (c = char(13)) or (c = char(27));
  writeln;
  if (inputstr = '') then
    inputstr := default;
end;

function getProfilesDirList: TStringList;
begin
  {$IFDEF WINDOWS}
  {$IFDEF WIN32}
  Result := getProfilesDirListWin;
  {$ELSE WIN32}
  Result := TStringList.Create;
  {$ENDIF WIN32}
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
  Result := getProfilesDirListLin;
  {$ENDIF}
  {$IFDEF DARWIN}
  Result := getProfilesDirListMac;
  {$ENDIF}
end;

function isUefi: boolean;
begin
  {$IFDEF WINDOWS}
  Result := winIsUefi;
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
  Result := linIsUefi;
  {$ENDIF}
  {$IFDEF DARWIN}
  Result := True;
  {$ENDIF}
end;

function isWinPE: boolean;
begin
  {$IFDEF WINDOWS}
  Result := WinIsPE;
  {$ENDIF WINDOWS}
  {$IFDEF UNIX}
  Result := False;
  {$ENDIF LINUX}
end;

function isGUI: boolean;
begin
  Result := True;
  {$IFDEF UNIX}
  {$IFDEF GUI}
  Result := True;
  {$ELSE GUI}
  Result := False;
  {$ENDIF GUI}
  {$ENDIF UNIX}
end;

function strContains(const str: string; const substr: string): boolean;
begin
  if pos(substr, str) = 0 then
    Result := False
  else
    Result := True;
end;

function createNewOpsiHostKey: string;
var
  mykey: string;
begin
  Randomize;
  mykey := IntToHex(random(MAXINT), 8);
  mykey := mykey + IntToHex(random(MAXINT), 8);
  mykey := mykey + IntToHex(random(MAXINT), 8);
  mykey := mykey + IntToHex(random(MAXINT), 8);
  Result := lowercase(mykey);
end;

procedure ChangeDirectory(newdir: string);
begin
  if SetCurrentDir(newdir) then
    Logdatei.Log('Changed current directory to ' + newdir, LLinfo)
  else
    Logdatei.log('Failed to change directory ', LLError);
end;

procedure ShrinkFileToMB(filename: string; newsize: integer);
var
  mylist: TStringList;
  fs, sStream: TFileStream;
  SplitFileNameTail, SplitFileNameBegin: string;
  SizeofFile, newPosition: integer;

begin
  fileName := ExpandFileName(fileName);
  SizeofFile := newsize * 1024 * 1024;

  try
    if FileExists(filename) then
    begin
      fs := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);

      // file size ok -> exit
      if (fs.Size <= SizeofFile) then
      begin
        fs.Free;
        exit;
      end;

      newPosition := fs.Size - SizeofFile;
      try
        // copy first part
        SplitFileNameBegin := ChangeFileExt(FileName, '.begin');
        sStream := TFileStream.Create(SplitFileNameBegin, fmCreate or fmShareExclusive);
        try
          sStream.CopyFrom(fs, newPosition);
        finally
          sStream.Free;
        end;
        // copy last part
        SplitFileNameTail := ChangeFileExt(FileName, '.tail');
        sStream := TFileStream.Create(SplitFileNameTail, fmCreate or fmShareExclusive);
        try
          sStream.CopyFrom(fs, SizeofFile);
        finally
          sStream.Free;
        end;
      finally
        fs.Free;
      end;
      // remove first line from xxx.tail
      mylist := TStringList.Create;
      try
        mylist.LoadFromFile(SplitFileNameTail);
        mylist.Delete(0);
        mylist.SaveToFile(SplitFileNameTail);
      finally
        mylist.Free;
      end;
      // remove from filesystem original file and SplitFileNameBegin
      // rename xxx.tail to xxx.log
      try
        DeleteFile(SplitFileNameBegin);
        DeleteFile(filename);
        RenameFile(SplitFileNameTail, filename)
      except
        on E: Exception do
          Logdatei.Log(
            'Error in osfunc.ShrinkFileToMB renaming and deleting files: ' +
            E.Message, LLError);
      end;
    end;
  except
    on E: Exception do
      Logdatei.Log('Error in osfunc.ShrinkFileToMB: ' + E.Message, LLError);
  end;
end;

(*
procedure ShrinkFileToMB(filename: string; newsize: integer);
var
  mylist: TStringList;
begin
  try
    if FileExists(filename) then
    begin
      mylist := TStringList.Create;
      try
        fileName := ExpandFileName(fileName);
        mylist.LoadFromFile(filename);
        while length(mylist.Text) > newsize * 1024 * 1024 do
        begin
          mylist.Delete(0);
        end;
        mylist.SaveToFile(filename);
      finally
        mylist.Free;
      end;
    end;
  except
    on E: Exception do
      Logdatei.Log('Error in osfunc.ShrinkFileToMB: ' + E.Message, LLError);
  end;
end;
*)

procedure str2jsonstr(var str: string; var errorstr: string);
var
  //str : SOString;
  new_obj: ISuperObject;
begin
  //new_obj := TSuperobject.ParseString(PSOChar(str));
  new_obj := SO(str);
  if new_obj <> nil then
  begin
    str := new_obj.asJson;
    errorstr := 'ok';
  end
  else
  begin
    errorstr := 'Could not convert: ' + str + ' to a json str';
  end;
end;



function randomstr(usespecialchars: boolean): string;
var
  //i, randomInt: integer;
  stringresult: string = '';
begin
  (*StringResult := '';
  for i := 1 to 2 do
  begin
    repeat
      randomInt := random(126)
    until
      char(randomInt) in CharsUpperForRandomStrings;
    StringResult := StringResult + char(randomInt);
    if usespecialchars then
    begin
      repeat
        randomInt := random(126)
      until
        char(randomInt) in CharsSpecialForRandomStrings;
      StringResult := StringResult + char(randomInt);
    end;
    repeat
      randomInt := random(126)
    until
      char(randomInt) in CharsLowerForRandomStrings;
    StringResult := StringResult + char(randomInt);
    repeat
      randomInt := random(126)
    until
      char(randomInt) in CharsNumericForRandomStrings;
    StringResult := StringResult + char(randomInt);
    repeat
      randomInt := random(126)
    until
      char(randomInt) in CharsNumericForRandomStrings;
    StringResult := StringResult + char(randomInt);
  end;*)
  if usespecialchars = True then
    StringResult := randomstrWithParameters(10, 2, 2, 4, 2)
  else
    StringResult := randomstrWithParameters(10, 3, 3, 4, 0);
  Result := StringResult;
end;

// randomstrWithParameters returns a random string with predefined preferences
function randomstrWithParameters(minLength, nLowerCases, nUpperCases,
  nDigits, nSpecialChars: integer): string;
var
  StringResult: string = '';
  i, randomInt, n, m: integer;
  aux: char;
begin
  { do not call randomize here.
    calling randomize here results that two calls  one after another
    will get the same result.
    randomize is called at the initialization of osparser unit }
  // Randomize;
  for i := 1 to nLowerCases do
  begin
    repeat
      randomInt := random(122) + 1
    until char(randomInt) in CharsLowerForRandomStrings;
    StringResult := StringResult + char(randomInt);
  end;
  for i := 1 to nUpperCases do
  begin
    repeat
      randomInt := random(90) + 1
    until char(randomInt) in CharsUpperForRandomStrings;
    StringResult := StringResult + char(randomInt);
  end;
  for i := 1 to nDigits do
  begin
    repeat
      randomInt := random(57) + 1
    until char(randomInt) in CharsNumericForRandomStrings;
    StringResult := StringResult + char(randomInt);
  end;
  for i := 1 to nSpecialChars do
  begin
    repeat
      randomInt := random(126) + 1
    until char(randomInt) in CharsSpecialForRandomStrings;
    StringResult := StringResult + char(randomInt);
  end;
  if Length(StringResult) < minLength then
  begin
    repeat
      repeat
        randomInt := random(122) + 1
      until char(randomInt) in CharsLowerForRandomStrings;
      StringResult := StringResult + char(randomInt);
    until Length(StringResult) >= minLength;
  end;

  //randomly reorganizing the string
  n := Length(StringResult);
  repeat
    m := random(n) + 1;
    aux := StringResult[n];
    StringResult[n] := StringResult[m];
    StringResult[m] := aux;
    n := n - 1;
  until n = 1;

  Result := StringResult;
end;


function GetUibNTVersion(var ErrorInfo: string): TuibNTVersion;
var
  version: dword = 0;
begin

  errorInfo := '';
  Result := tntverNONE;
  {$IFDEF WINDOWS}
  try
    version := GetNTVersionMajor;
    case version of
      3: Result := tntverNT3;
      4: Result := tntverNT4;
      5: Result := tntverWIN2K;
      6: Result := tntverWINVISTA;
      //7: Result := tntverWINLater;
      10: Result := tntverWINX;
    end
  except
    on e: Exception do
      ErrorInfo := 'Error: ' + e.message;
  end;
  {$ENDIF WINDOWS}
  if Result = tntverNONE then
    errorInfo := 'NT-OS not identified';
end;


function GetUibOSType(var ErrorInfo: string): TuibOSVersion;
begin
  errorInfo := '';
  Result := tovNotKnown;
  {$IFDEF WINDOWS}
  try
    case GetOSID
      of
      VER_PLATFORM_WIN32s: ;
      VER_PLATFORM_WIN32_WINDOWS: Result := tovWin95;
      VER_PLATFORM_WIN32_NT: Result := tovWinNT;
    end;
    if Result = tovNotKnown then
      errorInfo := 'OS version is not identified';
  except
    on e: Exception do
      ErrorInfo := 'Error: ' + e.message;
  end;
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
  Result := tovLinux;
  {$ENDIF LINUX}
  {$IFDEF DARWIN}
  Result := tovMacOS;
  {$ENDIF DARWIN}
end;

{$RANGECHECKS OFF}
procedure FindLocalIPData(var ipName: string; var address: string);
type
  bytearray = array of byte;
{$IFDEF WINDOWS}
var
  myHostEnt: THostEnt;
{$ENDIF WINDOWS}
begin
  ipName := '';
  address := '';
  {$IFDEF UNIX}
  ipName := getHostnameLin;

  address := getMyIpByDefaultRoute;
  {$ENDIF LINUX}
  {$IFDEF WINDOWS}
  if getMyHostEnt(myHostEnt) then
    with myHostEnt do
    begin
      ipName := h_name;
      address := Format('%d.%d.%d.%d',
        //[Byte((h_addr^)[0]), Byte(h_addr^[1]), Byte(h_addr^[2]), Byte(h_addr^[3])]);
        [Bytearray(h_addr^)[0], Bytearray(h_addr^)[1], Bytearray(h_addr^)[2],
        Bytearray(h_addr^)[3]]);
    end;
  {$ENDIF WINDOWS}
end;

{$RANGECHECKS ON}


{$IFDEF UNIX}
(*
function getMyHostEnt: THostEnt;

var
  buffer: PChar;
  len: integer = 0;
  errorcode: integer = 0;
begin
  result := synsock.
  try
    try
      buffer := StrAlloc(301);
      len := 300;
      errorcode := winsock.gethostname(buffer, len);
      if errorcode = 0 then
      begin
        Result := gethostbyname(buffer)^;
      end
      else
        Logdatei.Log('gethostname error ', LLError);
    finally
      StrDispose(buffer);
    end
  except
    Logdatei.Log('gethostname error ' +
      IntToStr(wsagetlasterror), LLError);
  end;
end;
(*
procedure FindLocalIPData(var ipName: string; var address: string);
begin
    ipName := IdIPWatch1.CurrentIP;
    address := IdIPWatch1.LocalIP;
end;
*)
{$ENDIF LINUX}


function GetNetDrive(const pathname: string): string;
var
  thePath: string = '';
  theDrive: string = '';
  posColon: integer = 0;
  posSlash: integer = 0;
begin
  thePath := ExtractFilePath(ExpandFileName(pathname));
  posColon := pos(':', thePath);
  if posColon > 0 then
    theDrive := copy(thePath, 1, posColon)
  else
  begin
    // Netzlaufwerk, Syntax: \\server\netzlaufwerksname\lokalerPfad
    posSlash := pos('\', copy(thePath, 3, length(thePath))) + 2;
    posSlash := posSlash + pos('\', copy(thePath, posSlash + 1, length(thePath)));

    theDrive := copy(thePath, 1, posSlash - 1);
  end;
  Result := theDrive;
end;

{$IFDEF WINDOWS}
function retrieveFiletime(const filename: string; var filetime: integer;
  var problem: string): boolean;
var
  PLastWriteTime: PFileTime;
  handle: THandle;
begin
  try
    Result := True;
    getMem(PLastWriteTime, sizeof(PFileTime));
    handle := CreateFile(PChar(filename), GENERIC_READ, FILE_SHARE_READ,
      nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    if handle = Invalid_Handle_Value then
    begin
      Problem := 'CreateFile for reading, File Error No. ' +
        IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) + ')';
      Result := False;
    end;

    if Result then
    begin
      Result := GetFileTime(handle, nil, nil, PLastWriteTime);
      if not Result then
        Problem := 'GetFileTime Error ' + IntToStr(GetLastError) +
          ' (' + SysErrorMessage(GetLastError) + ')';
    end;

    if Result then
      filetime := PlastWriteTime^.dwLowDateTime + PlastWriteTime^.dwHighDateTime shl 32;
  finally
    freemem(PlastWriteTime);
    closeHandle(handle);
  end;
end;

{$ENDIF WINDOWS}

{$IFNDEF WIN64}
function KillTask(ExeFileName: string; var info: string): boolean;
var
  stopped, found: integer;
begin
  info := '';
  found := 0;
  Result := True;
  stopped := KillProcessbyname(ExeFileName, found);
  {$IFDEF WINDOWS}
  if found = 0 then
  begin
    Result := True;
    info := 'Nothing to stop, no instances of "' + ExeFilename + '" found';
  end
  else
  begin
    if found = stopped then
    begin
      Result := True;
      info := IntToStr(found) + ' instance(s) of "' + ExeFilename + '" stopped';
    end
    else
    begin
      Result := False;
      info := IntToStr(found - stopped) + ' instance(s) of "' +
        ExeFilename + '" could not be stopped, problem(s) ' + info;
    end;
  end;
  {$ELSE WINDOWS}
  if stopped = 0 then
  begin
    if found > 0 then
    begin
      Result := False;
      info := IntToStr(found) + ' instance(s) of "' + ExeFilename +
        '" could not be stopped after kill success, problem(s) ' + info;
    end;

  end
  else
  begin
    if found > 0 then
    begin
      Result := False;
      info := IntToStr(found) + ' instance(s) of "' + ExeFilename +
        '" could not be stopped: kill failed, problem(s) ' + info;
    end;
  end;
  {$ENDIF WINDOWS}
end;

{$ENDIF WIN64}

function getProcessList: TStringList;
begin
  {$IFDEF WINDOWS}
  {$IFDEF WIN32}
  Result := getWinProcessList;
  {$ENDIF WIN32}
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
  Result := getLinProcessList;
  {$ENDIF LINUX}
  {$IFDEF DARWIN}
  Result := getMacosProcessList;
  {$ENDIF DARWIN}
end;


function getProcessListWithPath: TStringList;
begin
  {$IFDEF WINDOWS}
  {$IFDEF WIN32}
  Result := getWinProcessListWithPath;
  {$ENDIF WIN32}
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
  // Result := getLinProcessListWithPath;
 {$ENDIF LINUX}
  {$IFDEF DARWIN}
  // Result := getMacosProcessListWithPath;
  {$ENDIF DARWIN}
end;

{$IFDEF WINDOWS}
function findChildren(parentProcessID: DWord; var allChildrenIDs: TStringList;
  var info: string): integer;
  //  search all tasks
  //  and add IDs of processes which have the given parentProcessID to the list allChildrenIDs
var
  ContinueLoop: boolean;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
  processID: DWORD = 0;
  found: integer = 0;
  IDstring: string = '';
begin
  info := '';
  Result := 0;
  found := 0;
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := Sizeof(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
  while ContinueLoop do
  begin
    if (FProcessEntry32.th32ParentProcessID = parentProcessID) then
    begin
      Inc(found);
      processID := FProcessEntry32.th32ProcessID;
      IDString := IntToStr(processID);
      if allChildrenIDs.indexOf(IDString) = -1 then
      begin
        Logdatei.Log('yet a child process, ID ' +
          IntToStr(processID) + ', found. ' + ' length of allChildrenIDs: ' +
          IntToStr(allChildrenIDs.Count),
          LLinfo);
        allChildrenIDs.add(IDString);
      end;
    end;
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);

  Result := found;
end;


function FindFirstTask(ExeFileName: string; var processID: Dword;
  var parentProcessID: DWord; var info: string): boolean;
  // finds a task that executes ExeFilename
  // thereby retrieving its processID
var
  ContinueLoop: boolean;
  FSnapshotHandle: THandle = 0;
  FProcessEntry32: TProcessEntry32;
  found: integer = 0;
  teststring: string = '';
begin
  //Logdatei.Log ('FindTask for ' + ExeFilename, LLinfo);
  info := '';
  Result := False;
  found := 0;
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := Sizeof(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
  while ContinueLoop do
  begin

    teststring := UpperCase(ExtractFileName(FProcessEntry32.szExeFile));
    if (teststring = UpperCase(ExtractFileName(ExeFileName))) //'IRSETUP.EXE'
    //or (UpperCase(FProcessEntry32.szExeFile) = UpperCase(ExeFileName))
    then
    begin
      Inc(found);
      processID := FProcessEntry32.th32ProcessID;
      parentProcessID := FProcessEntry32.th32ParentProcessID;
      //Logdatei.Log (ExtractFileName(FProcessEntry32.szExeFile) + ', processId ' + #9 +
      //  IntToStr (FProcessEntry32.th32ProcessID) + #9 + 'parent process ' + #9 +
      //  IntToStr (FProcessEntry32.th32ParentProcessID), LLinfo);
    end;
    ContinueLoop := (found = 0) and Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);

  Result := (found > 0);
end;

{$ENDIF WINDOWS}

{$IFDEF WINDOWS}
function getExecutableName(FileName: string): string;
var
  FName, ExeName: PChar;
begin
  ExeName := StrAlloc(255);
  FName := StrAlloc(255);
  StrPCopy(FName, FileName);
  if FindExecutable(FName, nil, ExeName) < 32 then
  begin
    FileName := StrPas(ExeName) + ' ' + StrPas(FName);
    //StrPCopy(ExeName, FileName);
    Result := FileName;
  end
  else
    Result := '';
  StrDispose(FName);
  StrDispose(ExeName);
end;

{$ENDIF WINDOWS}

{$IFDEF WIN32}//ToDo: Ask Detlef why this works not for winst64 e.g.IFDEF WINDOWS
function SetFilePermissionForRunAs(filename: string; runas: TRunAs;
  var errorCode: DWORD): boolean;

const
  CHANGED_SECURITY_INFO = jwawinnt.DACL_SECURITY_INFORMATION;
  EA_COUNT = 1;

  // workaround for a FPC bug, see:
  // https://stackoverflow.com/a/59172446/12403540
  // https://bugs.freepascal.org/view.php?id=36368
type
  TRUSTEE_FIX = packed record
    pMultipleTrustee: Pointer;
    MultipleTrusteeOperation: DWORD;
    TrusteeForm: DWORD;
    TrusteeType: DWORD;
    ptstrName: LPSTR;
  end;

  EXPLICIT_ACCESS_FIX = packed record
    grfAccessPermissions: DWORD;
    grfAccessMode: DWORD;
    grfInheritance: DWORD;
    Trustee: TRUSTEE_FIX;
  end;

var
  sid: JwaWinNT.PSID;
  sidSize: DWORD;
  nameUse: JwaWinNT.SID_NAME_USE;
  refDomain: LpStr;
  refDomainSize: DWORD;
  dolocalfree: boolean;
  procToken: HANDLE;
  ea: array[0..(EA_COUNT - 1)] of EXPLICIT_ACCESS_FIX;
  acl: jwawinnt.PACL;
  status: jwawintype.DWORD;

  function SetPrivilege(token: HANDLE; privilege: PChar; state: boolean): boolean;
  var
    luid: jwawintype._LUID;
    tp: jwawinnt.TOKEN_PRIVILEGES;
  begin
    if not jwawinbase.LookupPrivilegeValue(nil, privilege, luid) then
      Result := False
    else
    begin
      FillChar(tp, sizeOf(tp), #0);
      tp.privilegeCount := 1;
      tp.Privileges[0].Luid := luid;
      if state then
        tp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED
      else
        tp.Privileges[0].Attributes := SE_PRIVILEGE_REMOVED;

      Result := AdjustTokenPrivileges(token, longbool(False), @tp,
        sizeOf(tp), nil, nil);
    end;
  end;

begin
  if runas = traInvoker then
    Result := True
  else
    try
      begin
        status := NO_ERROR;
        sidSize := 0;
        refDomainSize := 0;
        doLocalFree := False;

        procToken := INVALID_HANDLE_VALUE;

        sid := nil;

        // Determine the SID of the user that the file will be run as

        if runas = traLoggedOnUser then
        begin
          // use usercontextSID variable to determine SID of logged on user
          Result := jwasddl.ConvertStringSidToSidA(PChar(usercontextSID), sid);
          dolocalfree := True;
        end
        else if runas = traPcpatch then
        begin
          {
          jwawinbase.LookupAccountNameA(nil, 'pcpatch', nil,
            sidSize, nil, refDomainSize, nameUse);

          GetMem(sid, sidSize);
          GetMem(refDomain, refDomainSize);

          Result := jwawinbase.LookupAccountNameA(nil, 'pcpatch',
            sid, sidSize, refDomain, refDomainSize, nameUse);
          }

          // just generate an error, as it is currently unused anyways
          LogDatei.log('SetFilePermissionForRunAs: traPcpatch is not supported',
            LLError);
          Result := False;
        end
        else if runas in [traAdmin, traAdminProfile, traAdminProfileExplorer,
          traAdminProfileImpersonate, traAdminProfileImpersonateExplorer] then
        begin
          // to get the user's SID we need to create the local admin already here
          Result := CreateTemporaryLocalAdmin(runas);

          if Result then
          begin
            jwawinbase.LookupAccountNameA(nil, 'opsiSetupAdmin', nil,
              sidSize, nil, refDomainSize, nameUse);
            GetMem(sid, sidSize);
            GetMem(refDomain, refDomainSize);

            Result := jwawinbase.LookupAccountNameA(nil, 'opsiSetupAdmin',
              sid, sidSize, refDomain, refDomainSize, nameUse);
          end;
        end
        else
          Result := False;

        // Create an ACL allowing the determined SID
        // Read and Execute Rights, then apply it to
        // the given file.

        if Result then
        begin
          jwawinbase.ZeroMemory(@ea, EA_COUNT * sizeOf(EXPLICIT_ACCESS_FIX));

          ea[0].grfAccessPermissions := FILE_GENERIC_READ or FILE_GENERIC_EXECUTE;
          ea[0].grfAccessMode := DWORD(SET_ACCESS);
          ea[0].grfInheritance := NO_INHERITANCE;
          ea[0].Trustee.MultipleTrusteeOperation := DWORD(NO_MULTIPLE_TRUSTEE);
          ea[0].Trustee.pMultipleTrustee := nil;
          ea[0].Trustee.TrusteeForm := DWORD(TRUSTEE_IS_SID);
          ea[0].Trustee.TrusteeType := DWORD(TRUSTEE_IS_USER);
          ea[0].Trustee.ptstrName := pointer(sid);

          status := jwaaclapi.SetEntriesInAclA(EA_COUNT, @ea, nil, acl);

          if status <> ERROR_SUCCESS then
          begin
            Result := False;
          end
          else
          begin
            status := jwaaclapi.SetNamedSecurityInfoA(PChar(filename),
              JwaAccCtrl.SE_FILE_OBJECT, CHANGED_SECURITY_INFO, nil, nil, acl, nil);

            if status <> ERROR_SUCCESS then
            begin
              // if it fails we are probably missing the SE_RESTORE_NAME privilege, so we retry
              status := NO_ERROR;

              if (not OpenProcessToken(GetCurrentProcess(),
                TOKEN_ADJUST_PRIVILEGES, procToken)) or
                (not SetPrivilege(procToken, SE_RESTORE_NAME, True)) then
                Result := False
              else
              begin
                status := jwaaclapi.SetNamedSecurityInfoA(PChar(filename),
                  JwaAccCtrl.SE_FILE_OBJECT, CHANGED_SECURITY_INFO, nil, nil, nil, nil);
                Result := (status = ERROR_SUCCESS);

                if not SetPrivilege(procToken, SE_RESTORE_NAME, False) then
                  LogDatei.log('Could not disable SE_RESTORE_NAME privilege: ' +
                    IntToStr(getLastError()), LLDebug);
              end;
            end;
          end;
        end;
      end
    finally
      begin
        if not Result and (status = NO_ERROR) then
          status := getLastError();

        errorCode := status;

        if Assigned(sid) then
          if doLocalFree then
            LocalFree(DWORD(sid))
          else
            FreeMem(sid, sidSize);

        if Assigned(refDomain) then
          FreeMem(refDomain, refDomainSize);

        if Assigned(acl) then
          LocalFree(DWORD(acl));
      end;
    end;
end;

//Function modified by J. Werner (probaly does not work correct at the moment)
{function SetFilePermissionForRunAs(filename: string; runas: TRunAs;
  var errorCode: DWORD): boolean;

const
  EA_COUNT = 1;

  // workaround for a FPC bug, see:
  // https://stackoverflow.com/a/59172446/12403540
  // https://bugs.freepascal.org/view.php?id=36368
type
  TRUSTEE_FIX = packed record
    pMultipleTrustee: Pointer;
    MultipleTrusteeOperation: DWORD;
    TrusteeForm: DWORD;
    TrusteeType: DWORD;
    ptstrName: LPSTR;
  end;

  EXPLICIT_ACCESS_FIX = packed record
    grfAccessPermissions: DWORD;
    grfAccessMode: DWORD;
    grfInheritance: DWORD;
    Trustee: TRUSTEE_FIX;
  end;

var
  sid: JwaWinNT.PSID;
  sidSize: DWORD;
  nameUse: JwaWinNT.SID_NAME_USE;
  refDomain: LpStr;
  refDomainSize: DWORD;
  dolocalfree: boolean;
  procToken: HANDLE;
  ea: array[0..(EA_COUNT - 1)] of EXPLICIT_ACCESS_FIX;
  acl: jwawinnt.PACL;
  status: jwawintype.DWORD;

  function SetPrivilege(token: HANDLE; privilege: PChar; state: boolean): boolean;
  var
    luid: jwawintype._LUID;
    tp: jwawinnt.TOKEN_PRIVILEGES;
  begin
    if jwawinbase.LookupPrivilegeValue(nil, privilege, luid) then
    begin
      FillChar(tp, sizeOf(tp), #0);
      tp.privilegeCount := 1;
      tp.Privileges[0].Luid := luid;
      if state then
        tp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED
      else
        tp.Privileges[0].Attributes := SE_PRIVILEGE_REMOVED;
      Result := AdjustTokenPrivileges(token, longbool(False), @tp, sizeOf(tp), nil, nil);
    end
    else
      Result := False;
  end;

begin
  status := NO_ERROR;
  sidSize := 0;
  refDomainSize := 0;
  doLocalFree := False;
  procToken := INVALID_HANDLE_VALUE;
  sid := nil;
  try
    case runas of
      traInvoker: Result := True;
      traLoggedOnUser:
      begin
        // use usercontextSID variable to determine SID of logged on user
        Result := jwasddl.ConvertStringSidToSidA(PChar(usercontextSID), sid);
        dolocalfree := True;
      end;
      traPCpatch:
      begin
        (*
        jwawinbase.LookupAccountNameA(nil, 'pcpatch', nil,
          sidSize, nil, refDomainSize, nameUse);

        GetMem(sid, sidSize);
        GetMem(refDomain, refDomainSize);

        Result := jwawinbase.LookupAccountNameA(nil, 'pcpatch',
          sid, sidSize, refDomain, refDomainSize, nameUse);
        *)
        // just generate an error, as it is currently unused anyways
        LogDatei.log('SetFilePermissionForRunAs: traPcpatch is not supported', LLError);
        Result := False;
      end;
      traAdmin, traAdminProfile, traAdminProfileExplorer,
      traAdminProfileImpersonate, traAdminProfileImpersonateExplorer:
      begin
        // to get the user's SID we need to create the local admin already here
        Result := CreateTemporaryLocalAdmin(runas);
        if Result then
        begin
          jwawinbase.LookupAccountNameA(nil, 'opsiSetupAdmin', nil,
            sidSize, nil, refDomainSize, nameUse);
          GetMem(sid, sidSize);
          GetMem(refDomain, refDomainSize);
          Result := jwawinbase.LookupAccountNameA(nil, 'opsiSetupAdmin',
            sid, sidSize, refDomain, refDomainSize, nameUse);
        end;
      end;
      else Result := False;
    end;//case

    // Create an ACL allowing the determined SID
    // Read and Execute Rights, then apply it to
    // the given file.
    if Result then
    begin
      jwawinbase.ZeroMemory(@ea, EA_COUNT * sizeOf(EXPLICIT_ACCESS_FIX));
      ea[0].grfAccessPermissions := FILE_GENERIC_READ or FILE_GENERIC_EXECUTE;
      ea[0].grfAccessMode := DWORD(SET_ACCESS);
      ea[0].grfInheritance := NO_INHERITANCE;
      ea[0].Trustee.MultipleTrusteeOperation := DWORD(NO_MULTIPLE_TRUSTEE);
      ea[0].Trustee.pMultipleTrustee := nil;
      ea[0].Trustee.TrusteeForm := DWORD(TRUSTEE_IS_SID);
      ea[0].Trustee.TrusteeType := DWORD(TRUSTEE_IS_USER);
      ea[0].Trustee.ptstrName := pointer(sid);
      status := jwaaclapi.SetEntriesInAclA(EA_COUNT, @ea, nil, acl);
      if status <> ERROR_SUCCESS then
      begin
        Result := False;
      end
      else
      begin
        status := jwaaclapi.SetNamedSecurityInfoA(PChar(filename),
          JwaAccCtrl.SE_FILE_OBJECT, jwawinnt.DACL_SECURITY_INFORMATION, nil, nil, acl, nil);
        if status <> ERROR_SUCCESS then
        begin
          // if it fails we are probably missing the SE_RESTORE_NAME privilege, so we retry
          status := NO_ERROR;
          if OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES, procToken)
            or SetPrivilege(procToken, SE_RESTORE_NAME, True) then
          begin
            status := jwaaclapi.SetNamedSecurityInfoA(PChar(filename),
              JwaAccCtrl.SE_FILE_OBJECT, jwawinnt.DACL_SECURITY_INFORMATION, nil, nil, nil, nil);
            Result := (status = ERROR_SUCCESS);

            if SetPrivilege(procToken, SE_RESTORE_NAME, False) then
              LogDatei.log('Disable SE_RESTORE_NAME privilege: ' +
                IntToStr(getLastError()), LLDebug)
            else
              LogDatei.log('Could not disable SE_RESTORE_NAME privilege: ' +
                IntToStr(getLastError()), LLDebug);
          end
          else
            Result := False;
        end;
      end;
    end;
    //clean up and free memory
    finally
      begin
        if not Result and (status = NO_ERROR) then
          status := getLastError();
        errorCode := status;
        if Assigned(sid) then
          if doLocalFree then
            LocalFree(DWORD(sid))
          else
            FreeMem(sid, sidSize);
        if Assigned(refDomain) then
          FreeMem(refDomain, refDomainSize);
        if Assigned(acl) then
          LocalFree(DWORD(acl));
      end;
    end;
end;}

{$ENDIF WIN32}

{$IFDEF WINDOWS}
function StartProcess_se(CmdLinePasStr: string; ShowWindowFlag: integer;
  WaitForReturn: boolean; WaitForWindowVanished: boolean;
  WaitForWindowAppearing: boolean; WaitForProcessEnding: boolean;
  waitsecsAsTimeout: boolean; Ident: string; WaitSecs: word;
  var Report: string; var ExitCode: longint): boolean;

type
  PShellExecuteInfo = ^TShellExecuteInfoA;

var
  // StartupInfo : TStartupInfo;
  // ProcessInformation : TProcessInformation;
  // tokenhandle: thandle;

  executeInfo: TShellExecuteInfoA;
  pExecuteInfo: PShellExecuteInfo;

  running: boolean;
  lpExitCode: DWORD = 0;

  //FSnapshotHandle: THandle;
  //FProcessEntry32: TProcessEntry32;

  WaitWindowStarted: boolean;
  desiredProcessStarted: boolean;
  WaitForProcessEndingLogflag: boolean;
  starttime, nowtime: TDateTime;

  line: string = '';
  filename: string = '';
  params: string = '';
  //dir: string;
  firstsplit: integer = 0;
  //len: DWord;
  //functionresult: hinst;

  resultfilename: string = '';
  presultfilename: PChar;
  processID: Dword = 0;
  parentProcessID: DWord = 0;
  //allChildrenIDs : TStringList;
  //k : Integer;
  info: string = '';
  mypid: Dword = 0;

const
  secsPerDay = 86400;

begin
   (*
    with StartupInfo do
     Begin
       cb := SizeOf (TStartupInfo);
       lpReserved  := nil;
       lpDesktop   := nil;
       lpTitle     := nil;
       dwFlags     := STARTF_USESHOWWINDOW;
       wShowWindow := ShowWindowFlag;
       lpReserved2 := nil;
     end;
    *)

  ExitCode := -1;

  if length(CmdLinePasStr) = 0 then
  begin
    Result := False;
    Report := 'No file to execute';
    exit;
  end;

  params := '';

  if CmdLinePasStr[1] = '"' then
  begin
    line := copy(CmdLinePasStr, 2, length(CmdLinePasStr));
    firstsplit := pos('"', line);
    if firstsplit <= 0 then
    begin
      Result := False;
      Report := 'No valid filename';
      exit;
    end;
    filename := copy(line, 1, firstsplit - 1);
    params := copy(line, firstsplit + 1, length(line));
  end
  else
  begin
    firstsplit := pos(' ', CmdLinePasStr);
    if firstsplit > 0 then
    begin
      filename := copy(CmdLinePasStr, 1, firstsplit - 1);
      params := copy(CmdLinePasStr, firstsplit + 1, length(CmdLinePasStr));
    end
    else
    begin
      filename := CmdLinePasStr;
    end;
  end;

  with executeInfo do
  begin
    cbSize := sizeOf(TShellExecuteInfoA);

    fMask := SEE_MASK_NOCLOSEPROCESS; //leave the process running

    //Wnd := 0;
    Wnd := GetActiveWindow();
    lpVerb := 'open';
    //lpFile := PWideChar(Widestring(filename));
    //lpParameters := PWideChar(Widestring(params));
    lpFile := PChar(filename);
    lpParameters := PChar(params);
    lpDirectory := nil;
    nShow := ShowWindowFlag;
    // optional :
    hInstApp := 0;
    lpIDList := nil;
    lpClass := nil;
    hkeyClass := 0;
    dwHotKey := 0;
    //hIcon:= 0;
    hProcess := 0;
    //  hInstApp: HINST;
    (* Optional fields
       lpIDList: Pointer;
       lpClass: PWideChar;
       hkeyClass: HKEY;
       dwHotKey: DWORD;
       hIcon: THandle;
       hProcess: THandle;
       *)

  end;

  //pExecuteInfo := addr(executeInfo);


  //functionresult := ShellExecute (0, 'open', PChar (Filename), PChar(params), nil, showWindowFlag);
  //if functionresult <= 32
  try
    try
      CoInitialize(nil);
      //if not ShellExecuteEx(pExecuteInfo)
      if not ShellExecuteExA(@executeInfo)
        (*
        if not CreateProcess (nil, PChar (CmdLinePasStr),
                         nil, nil, true, 0 , nil, nil,
                         StartupInfo, ProcessInformation)
        *) then
      begin
        Result := False;
        Report := CmdLinePasStr + ' .... ShellExecute Error ' +
          IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) + ')';
        CloseHandle(executeinfo.hProcess);
      end
      else

      begin
        Result := True;

        desiredProcessStarted := False;
        WaitForProcessEndingLogflag := True;
        setLength(resultfilename, 400);
        presultfilename := PChar(resultfilename);
        //mypid := GetProcessId(executeinfo.hProcess);

        //FindExecutable(PChar(filename), nil, presultfilename);
        //FindFirstTask ( ExtractFilename(presultfilename), processID, parentProcessID, info);
        //allChildrenIDs := TStringList.create;
        //allChildrenIDs.add ( inttoStr(processID));

        if not WaitForReturn and (WaitSecs = 0) then
          Report := 'Process started:    ' + CmdLinePasStr
        else
        begin
             (*  waitForSingleObject (ProcessInformation.hProcess, INFINITE);
             CloseHandle (ProcessInformation.hProcess);
              // das funktioniert nicht
             *)

          running := True;
          starttime := now;
          WaitWindowStarted := False;

          while running do
          begin
            nowtime := now;

            running := False;

            (* wait for task vanished *)

            if WaitForWindowAppearing
              (* waiting condition 0:
                 wait until a window is appearing
              *) then
            begin
              if FindWindowEx(0, 0, nil, PChar(Ident)) = 0 then
                running := True;
            end

            else if WaitForWindowVanished and not WaitWindowStarted
              (* waiting condition 1:
                 we are waiting for a window that will later vanish
                 but this window did not appear yet *) then
            begin
              if FindWindowEx(0, 0, nil, PChar(Ident)) <> 0 then
                WaitWindowStarted := True;

              if not WaitWindowStarted or WaitForWindowVanished then
                running := True;
              // in case WaitForWindowVanished we are not yet ready
              // but have to check waiting condition 3
            end

            else if not waitsecsAsTimeout and (WaitSecs > 0) and
              ((nowtime - starttime) < waitSecs / secsPerDay)
               (* waiting condition 2 : we shall observe a waiting time
                                      and it has not finished )
                *) then
            begin
              running := True;
            end

            else if WaitForProcessEnding and not desiredProcessStarted
                (* waiting condition 3a : we wait that some other process will come into existence
                *) then
            begin
              if WaitForProcessEndingLogflag then
              begin
                Logdatei.Log('Waiting for start of "' +
                  ident + '"', LLinfo);
                WaitForProcessEndingLogflag := False;
              end;

              //FindExecutable(PChar(Ident),nil, presultfilename);
              desiredProcessStarted :=
                FindFirstTask(PChar(Ident) (*presultfilename *),
                processID, parentProcessID, info);

              if WaitSecs = 0 then
                running := True
              else
              begin //time out given
                if ((nowtime - starttime) < waitSecs / secsPerDay) then
                  running := True
                else
                  Logdatei.Log('Waiting for "' + ident +
                    '" stopped - time out ' + IntToStr(waitSecs) + ' sec', LLinfo);
              end;

            end

            else if WaitForProcessEnding and desiredProcessStarted
                (* waiting condition 3b : now we continue waiting until the observed other process will stop
                *) then
            begin
              running :=
                FindFirstTask(PChar(Ident) (*ExtractFilename(presultfilename)*),
                processID, parentProcessID, info);

              if not WaitForProcessEndingLogflag and running then
              begin
                Logdatei.Log('Waiting for process "' + ident +
                  '" ending', LLinfo);
                WaitForProcessEndingLogflag := True;
              end;

              if not running then
              begin
                Logdatei.Log('Process "' + ident + '" ended', LLinfo);
              end
              else
              begin

                if (waitSecs > 0) // we look for time out
                  and  //time out occured
                  ((nowtime - starttime) >= waitSecs / secsPerDay) then
                begin
                  running := False;
                  logdatei.log('Waiting for ending of "' +
                    ident + '" stopped - time out ' + IntToStr(waitSecs) +
                    ' sec', LLinfo);
                end;

              end;
            end


            else if GetExitCodeProcess(executeinfo.hProcess, lpExitCode) and
              (lpExitCode <> still_active)
                (* waiting condition 4 :  Process has finished;
                   we still have to look if WindowToVanish did vanish if this is necessary

                   *) then
            begin
              if WaitForWindowVanished then
              begin
                if not (FindWindowEx(0, 0, nil, PChar(Ident)) = 0) then
                begin
                  running := True;
                end;
              end;

            end

            else if waitForReturn
            (* waiting condition 4 : Process is still active *) then
            begin
              if waitsecsAsTimeout and (waitSecs > 0) // we look for time out
                and  //time out occured
                ((nowtime - starttime) >= waitSecs / secsPerDay) then
              begin
                running := False;
                Logdatei.Log('Waited for the end of started process"' +
                  ' - but time out reached after ' + IntToStr(waitSecs) +
                  ' sec.', LLInfo);
                // try to close process
                //if KillProcessbypid(mypid) then
                //        Logdatei.Log('Killed process with pid: '+ IntToStr(mypid), LLInfo)
                //else Logdatei.Log('Coud not kill process with pid: '+ IntToStr(mypid), LLWarning);
              end
              else
              begin
                running := True;
              end;
            end;

            if running then
            begin
              ProcessMess;
              sleep(50);
            end;
          end;

          ProcessMess;

          GetExitCodeProcess(executeinfo.hProcess, lpExitCode);
          //Report := 'Process executed  + CmdLinePasStr
          Report := 'ExitCode ' + IntToStr(lpExitCode) + '    Executed process "' +
            CmdLinePasStr + '"';
          exitCode := longint(lpExitCode);
        end;
      end;
    except
      on e: Exception do
      begin
        LogDatei.log_prog('Exception in StartProcess_se: ' +
          e.message, LLDebug);
        Report := 'Could not execute process "' + CmdLinePasStr + '"';
        exitcode := -1;
        Result := False;
      end;
    end;
  finally
    CloseHandle(executeinfo.hProcess);
    CoUninitialize;
  end;
end;

function StartProcess_se_as(CmdLinePasStr: string; ShowWindowFlag: integer;
  WaitForReturn: boolean; WaitForWindowVanished: boolean;
  WaitForWindowAppearing: boolean; WaitForProcessEnding: boolean;
  waitsecsAsTimeout: boolean; Ident: string; WaitSecs: word;
  var Report: string; var ExitCode: longint): boolean;

type
  PShellExecuteInfo = ^TShellExecuteInfoA;

var
  // StartupInfo : TStartupInfo;
  // ProcessInformation : TProcessInformation;
  // tokenhandle: thandle;

  executeInfo: TShellExecuteInfoA;
  pExecuteInfo: PShellExecuteInfo;

  running: boolean;
  lpExitCode: DWORD = 0;

  //FSnapshotHandle: THandle;
  //FProcessEntry32: TProcessEntry32;

  WaitWindowStarted: boolean;
  desiredProcessStarted: boolean;
  WaitForProcessEndingLogflag: boolean;
  starttime, nowtime: TDateTime;

  line: string = '';
  filename: string = '';
  params: string = '';
  //dir: string;
  firstsplit: integer = 0;
  //len: DWord;
  //functionresult: hinst;

  resultfilename: string = '';
  presultfilename: PChar;
  processID: Dword = 0;
  parentProcessID: DWord = 0;
  //allChildrenIDs : TStringList;
  //k : Integer;
  info: string = '';
  mypid: dword = 0;

const
  secsPerDay = 86400;

begin
   (*
    with StartupInfo do
     Begin
       cb := SizeOf (TStartupInfo);
       lpReserved  := nil;
       lpDesktop   := nil;
       lpTitle     := nil;
       dwFlags     := STARTF_USESHOWWINDOW;
       wShowWindow := ShowWindowFlag;
       lpReserved2 := nil;
     end;
    *)

  ExitCode := -1;

  if length(CmdLinePasStr) = 0 then
  begin
    Result := False;
    Report := 'No file to execute';
    exit;
  end;

  params := '';

  if CmdLinePasStr[1] = '"' then
  begin
    line := copy(CmdLinePasStr, 2, length(CmdLinePasStr));
    firstsplit := pos('"', line);
    if firstsplit <= 0 then
    begin
      Result := False;
      Report := 'No valid filename';
      exit;
    end;
    filename := copy(line, 1, firstsplit - 1);
    params := copy(line, firstsplit + 1, length(line));
  end
  else
  begin
    firstsplit := pos(' ', CmdLinePasStr);
    if firstsplit > 0 then
    begin
      filename := copy(CmdLinePasStr, 1, firstsplit - 1);
      params := copy(CmdLinePasStr, firstsplit + 1, length(CmdLinePasStr));
    end
    else
    begin
      filename := CmdLinePasStr;
    end;
  end;

  FillChar(executeInfo, SizeOf(executeInfo), #0);
  with executeInfo do
  begin
    cbSize := sizeOf(TShellExecuteInfoA);

    fMask := SEE_MASK_NOCLOSEPROCESS; //leave the process running

    //Wnd := 0;
    Wnd := GetActiveWindow();
    //lpVerb := 'open';
    lpVerb := 'runas';
    //lpFile := PWideChar(Widestring(filename));
    //lpParameters := PWideChar(Widestring(params));
    lpFile := PChar(filename);
    lpParameters := PChar(params);
    lpDirectory := PChar(GetCurrentDir);
    nShow := ShowWindowFlag;
    // optional :
    hInstApp := 0;
    lpIDList := nil;
    lpClass := nil;
    hkeyClass := 0;
    dwHotKey := 0;
    //hIcon:= 0;
    hProcess := 0;
    //  hInstApp: HINST;
    (* Optional fields
       lpIDList: Pointer;
       lpClass: PWideChar;
       hkeyClass: HKEY;
       dwHotKey: DWORD;
       hIcon: THandle;
       hProcess: THandle;
       *)

  end;

  //pExecuteInfo := addr(executeInfo);


  //functionresult := ShellExecute (0, 'open', PChar (Filename), PChar(params), nil, showWindowFlag);
  //if functionresult <= 32
  try
    try
      CoInitialize(nil);
      //if not ShellExecuteEx(pExecuteInfo)
      if not ShellExecuteExA(@executeInfo)
        (*
        if not CreateProcess (nil, PChar (CmdLinePasStr),
                         nil, nil, true, 0 , nil, nil,
                         StartupInfo, ProcessInformation)
        *) then
      begin
        Result := False;
        Report := CmdLinePasStr + ' .... ShellExecute Error ' +
          IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) + ')';
        CloseHandle(executeinfo.hProcess);
      end
      else

      begin
        Result := True;

        desiredProcessStarted := False;
        WaitForProcessEndingLogflag := True;
        setLength(resultfilename, 400);
        presultfilename := PChar(resultfilename);
        //mypid := GetProcessId(executeInfo.hProcess);

        //FindExecutable(PChar(filename), nil, presultfilename);
        //FindFirstTask ( ExtractFilename(presultfilename), processID, parentProcessID, info);
        //allChildrenIDs := TStringList.create;
        //allChildrenIDs.add ( inttoStr(processID));

        if not WaitForReturn and (WaitSecs = 0) then
          Report := 'Process started:    ' + CmdLinePasStr
        else
        begin
             (*  waitForSingleObject (ProcessInformation.hProcess, INFINITE);
             CloseHandle (ProcessInformation.hProcess);
              // das funktioniert nicht
             *)

          running := True;
          starttime := now;
          WaitWindowStarted := False;

          while running do
          begin
            nowtime := now;

            running := False;

            (* wait for task vanished *)

            if WaitForWindowAppearing
              (* waiting condition 0:
                 wait until a window is appearing
              *) then
            begin
              if FindWindowEx(0, 0, nil, PChar(Ident)) = 0 then
                running := True;
            end

            else if WaitForWindowVanished and not WaitWindowStarted
              (* waiting condition 1:
                 we are waiting for a window that will later vanish
                 but this window did not appear yet *) then
            begin
              if FindWindowEx(0, 0, nil, PChar(Ident)) <> 0 then
                WaitWindowStarted := True;

              if not WaitWindowStarted or WaitForWindowVanished then
                running := True;
              // in case WaitForWindowVanished we are not yet ready
              // but have to check waiting condition 3
            end

            else if not waitsecsAsTimeout and (WaitSecs > 0) and
              ((nowtime - starttime) < waitSecs / secsPerDay)
               (* waiting condition 2 : we shall observe a waiting time
                                      and it has not finished )
                *) then
            begin
              running := True;
            end

            else if WaitForProcessEnding and not desiredProcessStarted
                (* waiting condition 3a : we wait that some other process will come into existence
                *) then
            begin
              if WaitForProcessEndingLogflag then
              begin
                Logdatei.Log('Waiting for start of "' +
                  ident + '"', LLinfo);
                WaitForProcessEndingLogflag := False;
              end;

              //FindExecutable(PChar(Ident),nil, presultfilename);
              desiredProcessStarted :=
                FindFirstTask(PChar(Ident) (*presultfilename *),
                processID, parentProcessID, info);

              if WaitSecs = 0 then
                running := True
              else
              begin //time out given
                if ((nowtime - starttime) < waitSecs / secsPerDay) then
                  running := True
                else
                begin
                  Logdatei.Log('Waiting for "' + ident +
                    '" stopped - time out ' + IntToStr(waitSecs) + ' sec', LLinfo);
                end;
              end;

            end

            else if WaitForProcessEnding and desiredProcessStarted
                (* waiting condition 3b : now we continue waiting until the observed other process will stop
                *) then
            begin
              running :=
                FindFirstTask(PChar(Ident) (*ExtractFilename(presultfilename)*),
                processID, parentProcessID, info);

              if not WaitForProcessEndingLogflag and running then
              begin
                Logdatei.Log('Waiting for process "' + ident +
                  '" ending', LLinfo);
                WaitForProcessEndingLogflag := True;
              end;

              if not running then
              begin
                Logdatei.Log('Process "' + ident + '" ended', LLinfo);
              end
              else
              begin

                if (waitSecs > 0) // we look for time out
                  and  //time out occured
                  ((nowtime - starttime) >= waitSecs / secsPerDay) then
                begin
                  running := False;
                  logdatei.log('Waiting for ending of "' +
                    ident + '" stopped - time out ' + IntToStr(waitSecs) +
                    ' sec', LLinfo);
                end;

              end;

            end


            else if GetExitCodeProcess(executeinfo.hProcess, lpExitCode) and
              (lpExitCode <> still_active)
                (* waiting condition 4 :  Process has finished;
                   we still have to look if WindowToVanish did vanish if this is necessary

                   *) then
            begin
              if WaitForWindowVanished then
              begin
                if not (FindWindowEx(0, 0, nil, PChar(Ident)) = 0) then
                begin
                  running := True;
                end;
              end;

            end

            else if waitForReturn
            (* waiting condition 4 : Process is still active *) then
            begin
              if waitsecsAsTimeout and (waitSecs > 0) // we look for time out
                and  //time out occured
                ((nowtime - starttime) >= waitSecs / secsPerDay) then
              begin
                running := False;
                Logdatei.Log('Waited for the end of started process"' +
                  ' - but time out reached after ' + IntToStr(waitSecs) +
                  ' sec.', LLInfo);
                // try to close process
                //if KillProcessbypid(mypid) then
                //        Logdatei.Log('Killed process with pid: '+ IntToStr(mypid), LLInfo)
                //else Logdatei.Log('Coud not kill process with pid: '+ IntToStr(mypid), LLWarning);
              end
              else
              begin
                running := True;
              end;
            end;

            if running then
            begin
              ProcessMess;
              sleep(50);
            end;
          end;

          ProcessMess;

          GetExitCodeProcess(executeinfo.hProcess, lpExitCode);
          //Report := 'Process executed  + CmdLinePasStr
          Report := 'ExitCode ' + IntToStr(lpExitCode) + '    Executed process "' +
            CmdLinePasStr + '"';
          exitCode := longint(lpExitCode);
        end;
      end;
    except
      on e: Exception do
      begin
        Logdatei.Log('Exception in StartProcess_se_as: ' +
          e.message, LLDebug);
        Report := 'Could not execute process "' + CmdLinePasStr + '"';
        exitcode := -1;
        Result := False;
      end;
    end;
  finally
    CloseHandle(executeinfo.hProcess);
    CoUninitialize;
  end;
end;

{$ENDIF WINDOWS}


{$IFDEF WIN32}
function ReadPipe(var Buffer: string; var hReadPipe: THandle;
  var BytesRead: longword; var output: TXStringList; showoutput: boolean): boolean;
var
  output_line: string = '';
  lpBuffer: array[0..READ_BYTES - 1] of char;//array of 2048 char
  LineBreakPos: longword;
  BytesAvail: longword;
  BytesLeft: longword;
begin
  Result := PeekNamedPipe(hReadPipe, nil, READ_BYTES, @BytesRead,
    @BytesAvail, @BytesLeft);
  if BytesAvail > 0 then
  begin
    lpBuffer := '';
    Result := ReadFile(hReadPipe, lpBuffer, READ_BYTES, BytesRead, nil);

    OemToAnsiBuff(lpBuffer, lpBuffer, BytesRead);
    Buffer := Buffer + lpBuffer;

    LineBreakPos := Pos(#13, Buffer);
    while not (LineBreakPos = 0) do
    begin
      output_line := Copy(Buffer, 1, LineBreakPos - 1);
      output_line := WinCPToUTF8(output_line);
      output.Add(output_line);
      {$IFDEF GUI}
      if showoutput then
      begin
        SystemInfo.Memo1.Lines.Add(output_line);
        ProcessMess;
      end;
      {$ENDIF GUI}

      // skip carriage return if present
      if (length(Buffer) > LineBreakPos) and (Buffer[LineBreakPos + 1] = #10) then
        Inc(LineBreakPos, 1);

      Buffer := Copy(Buffer, LineBreakPos + 1, READ_BYTES);

      LineBreakPos := Pos(#13, Buffer);
    end;
  end;
end;

function StartProcess_cp_lu(CmdLinePasStr: string; ShowWindowFlag: integer;
  showoutput: boolean; WaitForReturn: boolean; WaitForWindowVanished: boolean;
  WaitForWindowAppearing: boolean; WaitForProcessEnding: boolean;
  waitsecsAsTimeout: boolean; Ident: string; WaitSecs: word;
  var Report: string; var ExitCode: longint; catchout: boolean;
  var output: TXStringList): boolean;

var
  WaitWindowStarted: boolean;
  desiredProcessStarted: boolean;
  WaitForProcessEndingLogflag: boolean;
  starttime, nowtime: TDateTime;

  line: string;
  filename: string;
  params: string;
  //dir: string;
  firstsplit: integer;
  //len: DWord;
  //functionresult: hinst;

  resultfilename: string;
  presultfilename: PChar;
  running: boolean;
  processID: Dword;
  parentProcessID: DWord;
  info: string;
  lpExitCode: DWORD = 0;
  gottoken: boolean;

  BytesRead: longword;
  Buffer: string = '';
  hReadPipe: THandle = 0;
  hWritePipe: THandle = 0;
  readResult: boolean;
  sa: TSecurityAttributes;
var
  ProcessInfo: jwawinbase.TProcessInformation;
  StartupInfo: jwawinbase.TStartupInfoW;
  CreateEnvironmentBlock: function(lpEnvironment: PPointer; hToken: THandle;
    bInherit: BOOL): BOOL; stdcall;
  DestroyEnvironmentBlock: function(lpEnvironment: PPointer): BOOL; stdcall;
  Environment: Pointer;
  mypid: dword = 0;
  lpProfileInfo: PROFILEINFOW;
  lpEnvironment: LPLPVOID;
  wstr: WideString;

const
  secsPerDay = 86400;

begin
  params := '';

  if CmdLinePasStr[1] = '"' then
  begin
    line := copy(CmdLinePasStr, 2, length(CmdLinePasStr));
    firstsplit := pos('"', line);
    if firstsplit <= 0 then
    begin
      Result := False;
      Report := 'No valid filename';
      exit;
    end;
    filename := copy(line, 1, firstsplit - 1);
    params := copy(line, firstsplit + 1, length(line));
  end
  else
  begin
    firstsplit := pos(' ', CmdLinePasStr);
    if firstsplit > 0 then
    begin
      filename := copy(CmdLinePasStr, 1, firstsplit - 1);
      params := copy(CmdLinePasStr, firstsplit + 1, length(CmdLinePasStr));
    end
    else
    begin
      filename := CmdLinePasStr;
    end;
  end;
  //Logdatei.Log('>->->'+filename+'='+getExecutableName(filename),LLEssential);
  try
    try
      FillChar(sa, SizeOf(sa), 0);
      sa.nLength := sizeof(sa);
      sa.lpSecurityDescriptor := nil;
      sa.bInheritHandle := True;

      FillChar(processInfo, SizeOf(processInfo), 0);

(*
      @CreateEnvironmentBlock := GetProcAddress(LoadLibrary('userenv.dll'),
        'CreateEnvironmentBlock');
      @DestroyEnvironmentBlock := GetProcAddress(LoadLibrary('userenv.dll'),
        'DestroyEnvironmentBlock');
      if (not Assigned(CreateEnvironmentBlock)) or
        (not Assigned(DestroyEnvironmentBlock)) then
        begin
          Logdatei.Log('Could not assign CreateEnvironmentBlock', LLError);
          exit;
        end;

      if not CreateEnvironmentBlock(lpEnvironment,
        opsiSetupAdmin_logonHandle, False) then
      begin
        Logdatei.Log('Could not CreateEnvironmentBlock', LLError);
        opsiSetupAdmin_lpEnvironment := nil;
      end
      else
        opsiSetupAdmin_lpEnvironment := lpEnvironment^;
 *)
      Logdatei.Log(
        'we will work with the logged on user with profile', LLInfo);
      FillChar(lpProfileInfo, SizeOf(lpProfileInfo), 0);
      lpProfileInfo.dwSize := SizeOf(lpProfileInfo);
      lpProfileInfo.lpUserName := pwidechar(usercontext);
      lpProfileInfo.dwFlags := 0;
      if not LoadUserProfileW(opsiSetupAdmin_logonHandle, lpProfileInfo) then
      begin
        //Result := False;
        Logdatei.Log(
          'Load profile for loggedon user failed: ' + IntToStr(GetLastError) +
          ' (' + SysErrorMessage(GetLastError) + ')', LLError);
      end
      else
      begin
        Logdatei.Log('Loaded profile for logged on user ',
          LLDebug);
        opsiSetupAdmin_ProfileHandle := lpProfileInfo.hProfile;
        lpEnvironment := nil;
      end;

      wstr := CmdLinePasStr;

      if not WaitForReturn then
        catchout := False;

      if not CreatePipe(hReadPipe, hWritePipe, @sa, 0) then
      begin
        Report := 'Error creating Pipe';
        Result := False;
        exit;
      end;

      FillChar(StartupInfo, SizeOf(StartupInfo), #0);
      StartupInfo.cb := SizeOf(StartupInfo);
      if catchout then
      begin
        StartupInfo.dwFlags := STARTF_USESTDHANDLES;
        StartupInfo.hStdInput := 0;
        StartupInfo.hStdOutput := hWritePipe;
        StartupInfo.hStdError := hWritePipe;
      end;

      if not jwawinbase.CreateProcessAsUserW(opsiSetupAdmin_logonHandle,
        nil, pwidechar(wstr), nil, nil,
        //opsiSetupAdmin_pSecAttrib, opsiSetupAdmin_pSecAttrib,
        catchout,  // inherit handles if we catch output
        //CREATE_NEW_CONSOLE or CREATE_NEW_PROCESS_GROUP or CREATE_UNICODE_ENVIRONMENT,
        //CREATE_NO_WINDOW or CREATE_UNICODE_ENVIRONMENT or
        //CREATE_DEFAULT_ERROR_MODE,
        0,
        //opsiSetupAdmin_lpEnvironment,
        nil,
        //PChar(GetCurrentDir),
        nil, startupInfo, processInfo) then
      begin
        Result := False;
        Report := CmdLinePasStr + ' .... CreateProcessAsUser Error ' +
          IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) + ')';
        Logdatei.Log(Report, LLError);
        CloseHandle(processInfo.hProcess);
        CloseHandle(processInfo.hThread);
        CloseHandle(hReadPipe);
        CloseHandle(hWritePipe);
      end

(*
      if not CreateProcessElevated(nil, CmdLinePasStr, PChar(GetCurrentDir), 0,
        ProcessInfo) then
      begin
        Result := False;
        Logdatei.Log('Could not start process ', LLError);
      end
*)
      else
      begin
        SetProcessAffinityMask(ProcessInfo.hProcess, 1);
        Result := True;

        desiredProcessStarted := False;
        WaitForProcessEndingLogflag := True;
        setLength(resultfilename, 400);
        presultfilename := PChar(resultfilename);

        BytesRead := 0;

        //mypid := ProcessInfo.dwProcessId;

        //FindExecutable(PChar(filename), nil, presultfilename);
        //FindFirstTask ( ExtractFilename(presultfilename), processID, parentProcessID, info);
        //allChildrenIDs := TStringList.create;
        //allChildrenIDs.add ( inttoStr(processID));

        if not WaitForReturn and (WaitSecs = 0) then
          Report := 'Process started:    ' + CmdLinePasStr
        else
        begin
          running := True;
          starttime := now;
          WaitWindowStarted := False;

          while running do
          begin
            nowtime := now;

            running := False;

            if catchout and (not
              (ReadPipe(Buffer, hReadPipe, BytesRead, output, showoutput))) then
              LogDatei.log('Read Error: ' + SysErrorMessage(getLastError()), LLError);

            //wait for task vanished

            if WaitForWindowAppearing then
            begin
              //waiting condition 0:
              //wait until a window is appearing
              if FindWindowEx(0, 0, nil, PChar(Ident)) = 0 then
                running := True;
            end

            else if WaitForWindowVanished and not WaitWindowStarted then
            begin
              //waiting condition 1:
              //we are waiting for a window that will later vanish
              //but this window did not appear yet
              if FindWindowEx(0, 0, nil, PChar(Ident)) <> 0 then
                WaitWindowStarted := True;

              if not WaitWindowStarted or WaitForWindowVanished then
                running := True;
              // in case WaitForWindowVanished we are not yet ready
              // but have to check waiting condition 3
            end

            else if not waitsecsAsTimeout and (WaitSecs > 0) and
              ((nowtime - starttime) < waitSecs / secsPerDay) then
            begin
              // waiting condition 2 : we shall observe a waiting time
              // and it has not finished
              running := True;
            end

            else if WaitForProcessEnding and not desiredProcessStarted then
            begin
              //waiting condition 3a : we wait that some other process will come into existence
              if WaitForProcessEndingLogflag then
              begin
                Logdatei.Log('Waiting for start of "' +
                  ident + '"', LLinfo);
                WaitForProcessEndingLogflag := False;
              end;

              desiredProcessStarted :=
                FindFirstTask(PChar(Ident), processID, parentProcessID, info);

              if WaitSecs = 0 then
                running := True
              else
              begin //time out given
                if ((nowtime - starttime) < waitSecs / secsPerDay) then
                  running := True
                else
                  Logdatei.Log('Waiting for "' + ident +
                    '" stopped - time out ' + IntToStr(waitSecs) + ' sec', LLinfo);
              end;

            end

            else if WaitForProcessEnding and desiredProcessStarted then
            begin
              //waiting condition 3b : now we continue waiting until the observed other process will stop
              running :=
                FindFirstTask(PChar(Ident), processID, parentProcessID, info);

              if not WaitForProcessEndingLogflag and running then
              begin
                Logdatei.Log('Waiting for process "' +
                  ident + '" ending', LLinfo);
                WaitForProcessEndingLogflag := True;
              end;

              if not running then
              begin
                Logdatei.Log('Process "' + ident + '" ended', LLinfo);
              end
              else
              begin

                if (waitSecs > 0) // we look for time out
                  and  //time out occured
                  ((nowtime - starttime) >= waitSecs / secsPerDay) then
                begin
                  running := False;
                  Logdatei.Log('Waiting for ending of "' +
                    ident + '" stopped - waitSecs out ' + IntToStr(waitSecs) +
                    ' sec', LLinfo);
                end;

              end;

            end

            //else if not FpcProcess.Running
            else if GetExitCodeProcess(processInfo.hProcess, lpExitCode) and
              (lpExitCode <> still_active) then
              //else if FpcProcess.ExitStatus  <> still_active then
            begin
              // waiting condition 4 :  Process has finished;
              //   we still have to look if WindowToVanish did vanish if this is necessary
              Logdatei.Log(
                'Process terminated at: ' + DateTimeToStr(now) +
                ' exitcode is: ' + IntToStr(lpExitCode), LLDebug2);
 (*
            end
            else if GetExitCodeProcess(FpcProcess.ProcessHandle, lpExitCode) and
              (lpExitCode <> still_active)
            then
            begin
              Logdatei.Log('Process terminated at: ' +
                DateTimeToStr(now) + ' exitcode is: ' + IntToStr(lpExitCode), LLDebug2);
*)
              if WaitForWindowVanished then
              begin
                if not (FindWindowEx(0, 0, nil, PChar(Ident)) = 0) then
                begin
                  running := True;
                end;
              end;

            end

            else if waitForReturn then
            begin
              //waiting condition 4 : Process is still active
              if waitsecsAsTimeout and (waitSecs >
                0) // we look for time out
                and  //time out occured
                ((nowtime - starttime) >= waitSecs / secsPerDay) then
              begin
                running := False;
                Logdatei.Log('Waited for the end of started process"' +
                  ' - but time out reached after ' + IntToStr(waitSecs) +
                  ' sec.', LLinfo);
                // try to close process
                //if KillProcessbypid(mypid) then
                //    Logdatei.Log('Killed process with pid: '+ IntToStr(mypid), LLInfo)
                //else Logdatei.Log('Coud not kill process with pid: '+ IntToStr(mypid), LLWarning);
              end
              else
              begin
                running := True;
              end;
            end;

            if running then
            begin
              ProcessMess;
              sleep(1000);
              GetExitCodeProcess(ProcessInfo.hProcess, lpExitCode);
              ProcessMess;
              Logdatei.Log('Waiting for ending at ' +
                DateTimeToStr(now) + ' exitcode is: ' + IntToStr(lpExitCode), LLDebug2);
              ProcessMess;
            end;
          end;

          ProcessMess;

          if catchout then
          begin
            repeat
              readResult := ReadPipe(Buffer, hReadPipe, BytesRead, output, showoutput);
              if not readResult then
                LogDatei.log('Read Error: ' + SysErrorMessage(getLastError()), LLError);
            until (BytesRead <= 0) or (not readResult);

            // add remainder of buffer as last line
            if Buffer <> '' then
              output.Add(Buffer);
          end;

          ProcessMess;

          GetExitCodeProcess(ProcessInfo.hProcess, lpExitCode);
          exitCode := longint(lpExitCode);
          Report := 'ExitCode ' + IntToStr(exitCode) + '    Executed process "' +
            CmdLinePasStr + '"';
        end;

      end;

    except
      on e: Exception do
      begin
        Logdatei.Log('Exception in StartProcess_cp_lu: ' +
          e.message, LLDebug);
        Report := 'Could not execute process "' + CmdLinePasStr + '"';
        exitcode := -1;
        Result := False;
      end;
    end;
  finally
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(processInfo.hThread);
    CloseHandle(hReadPipe);
    CloseHandle(hWritePipe);
    (*
    if not (opsiSetupAdmin_lpEnvironment = nil) then
      if not DestroyEnvironmentBlock(opsiSetupAdmin_lpEnvironment) then
        Logdatei.Log('Could not destroy Environment : ' +
          IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) +
          ')', LLWarning);
    *)
    if not UnloadUserProfile(opsiSetupAdmin_logonHandle,
      opsiSetupAdmin_ProfileHandle) then
      Logdatei.Log('Could not unload userprofile : ' + IntToStr(
        GetLastError) + ' (' + SysErrorMessage(GetLastError) + ')', LLNotice);
    CloseHandle(opsiSetupAdmin_logonHandle);
    ///S.Free;
    //FpcProcess.Free;
    ///M.Free;
  end;
end;

function StartProcess_cp_el(CmdLinePasStr: string; ShowWindowFlag: integer;
  showoutput: boolean; WaitForReturn: boolean; WaitForWindowVanished: boolean;
  WaitForWindowAppearing: boolean; WaitForProcessEnding: boolean;
  waitsecsAsTimeout: boolean; Ident: string; WaitSecs: word;
  var Report: string; var ExitCode: longint; catchout: boolean;
  var output: TXStringList): boolean;

var
  WaitWindowStarted: boolean;
  desiredProcessStarted: boolean;
  WaitForProcessEndingLogflag: boolean;
  starttime, nowtime: TDateTime;

  line: string;
  filename: string;
  params: string;
  //dir: string;
  firstsplit: integer;
  //len: DWord;
  //functionresult: hinst;

  resultfilename: string;
  presultfilename: PChar;
  running: boolean;
  processID: Dword;
  parentProcessID: DWord;
  info: string;
  lpExitCode: DWORD = 0;
  gottoken: boolean;

  // output catching
  hReadPipe: THandle = 0;
  hWritePipe: THandle = 0;
  BytesRead: longword;
  Buffer: string = '';
  readResult: boolean;
  sa: TSecurityAttributes;
var
  ProcessInfo: jwawinbase.TProcessInformation;
  StartupInfo: jwawinbase.TStartupInfo;
  CreateEnvironmentBlock: function(lpEnvironment: PPointer; hToken: THandle;
    bInherit: BOOL): BOOL; stdcall;
  DestroyEnvironmentBlock: function(lpEnvironment: PPointer): BOOL; stdcall;
  Environment: Pointer;
  mypid: dword = 0;

const
  secsPerDay = 86400;

begin
  params := '';

  if CmdLinePasStr[1] = '"' then
  begin
    line := copy(CmdLinePasStr, 2, length(CmdLinePasStr));
    firstsplit := pos('"', line);
    if firstsplit <= 0 then
    begin
      Result := False;
      Report := 'No valid filename';
      exit;
    end;
    filename := copy(line, 1, firstsplit - 1);
    params := copy(line, firstsplit + 1, length(line));
  end
  else
  begin
    firstsplit := pos(' ', CmdLinePasStr);
    if firstsplit > 0 then
    begin
      filename := copy(CmdLinePasStr, 1, firstsplit - 1);
      params := copy(CmdLinePasStr, firstsplit + 1, length(CmdLinePasStr));
    end
    else
    begin
      filename := CmdLinePasStr;
    end;
  end;
  //Logdatei.Log('>->->'+filename+'='+getExecutableName(filename),LLEssential);
  try
    try
      FillChar(sa, SizeOf(sa), 0);
      sa.nLength := sizeof(sa);
      sa.lpSecurityDescriptor := nil;
      sa.bInheritHandle := True;

      FillChar(processInfo, SizeOf(processInfo), 0);



      //CreateProcessElevated(lpApplicationName: PChar; lpCommandLine: String;
      //lpCurrentDirectory: PChar;Counter: Integer; var ProcessInfo: TProcessInformation): Boolean;
      //gottoken := OpenShellProcessToken('action_processor_starter.exe', opsiSetupAdmin_logonHandle);
      //if not gottoken then gottoken := OpenShellProcessToken('opsiclientd.exe', opsiSetupAdmin_logonHandle);

      gottoken := OpenShellProcessToken('opsiclientd.exe', opsiSetupAdmin_logonHandle);
      if not gottoken then
        gottoken := OpenShellProcessToken('explorer.exe', opsiSetupAdmin_logonHandle);
      if not gottoken then
        LogDatei.log('Could not get token from opsiclientd or explorer',
          LLError);
      @CreateEnvironmentBlock := GetProcAddress(LoadLibrary('userenv.dll'),
        'CreateEnvironmentBlock');
      @DestroyEnvironmentBlock := GetProcAddress(LoadLibrary('userenv.dll'),
        'DestroyEnvironmentBlock');
      if (not Assigned(CreateEnvironmentBlock)) or
        (not Assigned(DestroyEnvironmentBlock)) then
        Exit;

      if not CreateEnvironmentBlock(@opsiSetupAdmin_lpEnvironment,
        opsiSetupAdmin_logonHandle, False) then
      begin
        LogDatei.log('Could not CreateEnvironmentBlock', LLError);
        opsiSetupAdmin_lpEnvironment := nil;
      end;

      if not WaitForReturn then
        catchout := False;

      if not CreatePipe(hReadPipe, hWritePipe, @sa, 0) then
      begin
        Report := 'Error creating Pipe';
        Result := False;
        exit;
      end;

      FillChar(StartupInfo, SizeOf(StartupInfo), #0);
      StartupInfo.cb := SizeOf(StartupInfo);
      if catchout then
      begin
        StartupInfo.dwFlags := STARTF_USESTDHANDLES;
        StartupInfo.hStdInput := 0;
        StartupInfo.hStdOutput := hWritePipe;
        StartupInfo.hStdError := hWritePipe;
      end;

      if not jwawinbase.CreateProcessAsUser(opsiSetupAdmin_logonHandle,
        nil, PChar(CmdLinePasStr),
        //nil, nil,
        opsiSetupAdmin_pSecAttrib, opsiSetupAdmin_pSecAttrib,
        catchout, // inherit handles only if we catch output
        //CREATE_NEW_CONSOLE or CREATE_NEW_PROCESS_GROUP or CREATE_UNICODE_ENVIRONMENT,
        CREATE_NO_WINDOW or CREATE_UNICODE_ENVIRONMENT or
        CREATE_DEFAULT_ERROR_MODE, opsiSetupAdmin_lpEnvironment,
        //nil,
        PChar(GetCurrentDir), startupInfo, processInfo) then
      begin
        Result := False;
        Report := CmdLinePasStr + ' .... CreateProcessAsUser Error ' +
          IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) + ')';
        LogDatei.log(Report, LLError);
        CloseHandle(processInfo.hProcess);
        CloseHandle(processInfo.hThread);
        CloseHandle(hWritePipe);
        CloseHandle(hReadPipe);
      end
      else
      begin
        SetProcessAffinityMask(ProcessInfo.hProcess, 1);
        Result := True;
        logdatei.log('Started process "' + CmdLinePasStr, LLInfo);
        desiredProcessStarted := False;
        WaitForProcessEndingLogflag := True;
        setLength(resultfilename, 400);
        presultfilename := PChar(resultfilename);
        //mypid := ProcessInfo.dwProcessId;

        //FindExecutable(PChar(filename), nil, presultfilename);
        //FindFirstTask ( ExtractFilename(presultfilename), processID, parentProcessID, info);
        //allChildrenIDs := TStringList.create;
        //allChildrenIDs.add ( inttoStr(processID));

        BytesRead := 0;

        if not WaitForReturn and (WaitSecs = 0) then
          Report := 'Process started:    ' + CmdLinePasStr
        else
        begin
          running := True;
          starttime := now;
          WaitWindowStarted := False;
          {$IFDEF GUI}
          if waitsecsAsTimeout and (WaitSecs > 5) then
          begin
            FBatchOberflaeche.SetElementVisible(True, eProgressBar);
            //showProgressBar(True);
            //FBatchOberflaeche.setProgress(0);
          end;
          {$ENDIF GUI}

          while running do
          begin
            nowtime := now;

            running := False;

            if catchout and not
              (ReadPipe(Buffer, hReadPipe, BytesRead, output, showoutput)) then
              LogDatei.log('Read Error: ' + SysErrorMessage(getLastError()), LLError);

            //wait for task vanished

            if WaitForWindowAppearing then
            begin
              //waiting condition 0:
              //wait until a window is appearing
              if FindWindowEx(0, 0, nil, PChar(Ident)) = 0 then
                running := True;
            end

            else if WaitForWindowVanished and not WaitWindowStarted then
            begin
              //waiting condition 1:
              //we are waiting for a window that will later vanish
              //but this window did not appear yet
              if FindWindowEx(0, 0, nil, PChar(Ident)) <> 0 then
              begin
                WaitWindowStarted := True;
                logdatei.log('Wait for vanish Window: "' + Ident + '" found.', LLDebug);
              end;

              if not WaitWindowStarted or WaitForWindowVanished then
                // in case WaitForWindowVanished we are not yet ready
                // but have to check waiting condition 3
                if WaitSecs = 0 then
                  running := True
                else
                begin //time out given
                  if ((nowtime - starttime) < waitSecs / secsPerDay) then
                  begin
                    running := True;
                  end
                  else
                  begin
                    logdatei.log('Wait for vanish Window "' + ident +
                      '" stopped - time out ' + IntToStr(waitSecs) + ' sec', LLInfo);
                  end;
                end;
            end

            else if not waitsecsAsTimeout and (WaitSecs > 0) and
              ((nowtime - starttime) < waitSecs / secsPerDay) then
            begin
              // waiting condition 2 : we shall observe a waiting time
              // and it has not finished
              running := True;
            end

            else if WaitForProcessEnding and not desiredProcessStarted then
            begin
              //waiting condition 3a : we wait that some other process will come into existence
              if WaitForProcessEndingLogflag then
              begin
                logdatei.log('Waiting for start of "' + ident +
                  '"', LLInfo);
                WaitForProcessEndingLogflag := False;
              end;

              desiredProcessStarted :=
                FindFirstTask(PChar(Ident), processID, parentProcessID, info);

              if WaitSecs = 0 then
                running := True
              else
              begin //time out given
                if ((nowtime - starttime) < waitSecs / secsPerDay) then
                  running := True
                else
                  logdatei.log('Waiting for "' + ident +
                    '" stopped - time out ' + IntToStr(waitSecs) + ' sec', LLInfo);
              end;

            end

            else if WaitForProcessEnding and desiredProcessStarted then
            begin
              //waiting condition 3b : now we continue waiting until the observed other process will stop
              running :=
                FindFirstTask(PChar(Ident), processID, parentProcessID, info);

              if not WaitForProcessEndingLogflag and running then
              begin
                logdatei.log('Waiting for process "' + ident +
                  '" ending', LLDebug2);
                WaitForProcessEndingLogflag := True;
              end;

              if not running then
              begin
                logdatei.log('Process "' + ident + '" ended', LLinfo);
                // After the process we waited for has ended, the Parent may be still alive
                // in this case we have to wait for the end of the parent
                {$IFDEF WINDOWS}
                if GetExitCodeProcess(processInfo.hProcess, longword(lpExitCode)) and
                  (lpExitCode = still_active) then
                begin
                  running := True;
                  WaitForProcessEnding := False;
                end;
                {$ENDIF WINDOWS}
                (*
                {$IFDEF UNIX}
                lpExitCode := FpcProcess.ExitCode;
                if FpcProcess.Running then
                begin
                  running := True;
                  WaitForProcessEnding := False;
                end
                else
                begin
                  lpExitCode := FpcProcess.ExitCode;
                  logdatei.log(
                    'Process : ' + FpcProcess.Executable + ' terminated at: ' +
                    DateTimeToStr(now) + ' exitcode is: ' +
                    IntToStr(lpExitCode), LLInfo);
                end;
                {$ENDIF LINUX}
                *)
              end
              else
              begin

                if (waitSecs > 0) // we look for time out
                  and  //time out occured
                  ((nowtime - starttime) >= waitSecs / secsPerDay) then
                begin
                  running := False;
                  logdatei.log('Waiting for ending of "' +
                    ident + '" stopped - waitSecs out ' + IntToStr(waitSecs) +
                    ' sec', LLinfo);
                end;

              end;

            end

            //else if not FpcProcess.Running
            else if GetExitCodeProcess(processInfo.hProcess, lpExitCode) and
              (lpExitCode <> still_active) then
              //else if FpcProcess.ExitStatus  <> still_active then
            begin
              // waiting condition 4 :  Process has finished;
              //   we still have to look if WindowToVanish did vanish if this is necessary
              logdatei.log(
                'Process terminated at: ' + DateTimeToStr(now) +
                ' exitcode is: ' + IntToStr(lpExitCode), LLDebug2);
              if WaitForWindowVanished then
              begin
                if not (FindWindowEx(0, 0, nil, PChar(Ident)) = 0) then
                begin
                  running := True;
                end;
              end;
            end
            else if waitForReturn then
            begin
              //waiting condition 4 : Process is still active
              if waitsecsAsTimeout and (waitSecs >
                0) // we look for time out
                and  //time out occured
                ((nowtime - starttime) >= waitSecs / secsPerDay) then
              begin
                running := False;
                logdatei.log('Waited for the end of started process"' +
                  ' - but time out reached after ' + IntToStr(waitSecs) +
                  ' sec.', LLDebug2);
                // try to close process
                //if KillProcessbypid(mypid) then
                //    Logdatei.Log('Killed process with pid: '+ IntToStr(mypid), LLInfo)
                //else Logdatei.Log('Coud not kill process with pid: '+ IntToStr(mypid), LLWarning);
              end
              else
              begin
                running := True;
              end;
            end;

            if running then
            begin
              ProcessMess;
              sleep(1000);
              GetExitCodeProcess(ProcessInfo.hProcess, lpExitCode);
              ProcessMess;
              logdatei.log('Waiting for ending at ' +
                DateTimeToStr(now) + ' exitcode is: ' + IntToStr(lpExitCode), LLDebug2);
              ProcessMess;
            end;
          end;

          ProcessMess;

          if catchout then
          begin
            repeat
              readResult := ReadPipe(Buffer, hReadPipe, BytesRead, output, showoutput);
              if not readResult then
                LogDatei.log('Read Error: ' + SysErrorMessage(getLastError()), LLError);
            until (BytesRead <= 0) or (not readResult);

            // add remainder of buffer as last line
            if Buffer <> '' then
              output.Add(Buffer);
          end;

          ProcessMess;

          //exitCode := FpcProcess.ExitStatus;
          GetExitCodeProcess(ProcessInfo.hProcess, lpExitCode);
          exitCode := longint(lpExitCode);
          //Report := 'Process executed  + CmdLinePasStr
          Report := 'ExitCode ' + IntToStr(exitCode) + '    Executed process "' +
            CmdLinePasStr + '"';
        end;

      end;

    except
      on e: Exception do
      begin
        LogDatei.log('Exception in StartProcess_cp_el: ' + e.message, LLDebug);
        Report := 'Could not execute process "' + CmdLinePasStr + '"';
        exitcode := -1;
        Result := False;
      end;
    end;
  finally
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(processInfo.hThread);
    CloseHandle(hReadPipe);
    CloseHandle(hWritePipe);
  end;
end;

function StartProcess_as(CmdLinePasStr: string; ShowWindowFlag: integer;
  showoutput: boolean; WaitForReturn: boolean; WaitForWindowVanished: boolean;
  WaitForWindowAppearing: boolean; WaitForProcessEnding: boolean;
  waitsecsAsTimeout: boolean; Ident: string; WaitSecs: word;
  var Report: string; var ExitCode: longint; catchout: boolean;
  var output: TXStringList): boolean;

const
  // default values for window stations and desktops
  CreateProcDEFWINSTATION = 'WinSta0';
  CreateProcDEFDESKTOP = 'Default';
  CreateProcDOMUSERSEP = '\';

var
  WaitWindowStarted: boolean;
  desiredProcessStarted: boolean;
  WaitForProcessEndingLogflag: boolean;
  starttime, nowtime: TDateTime;

  line: string;
  tempstr: string;
  filename: string;
  params: string;
  //dir: string;
  firstsplit: integer;
  //len: DWord;
  //functionresult: hinst;

  resultfilename: string;
  presultfilename: PChar;
  running: boolean;
  processID: Dword;
  parentProcessID: DWord;
  info: string;
  lpExitCode: DWORD = 0;
  lastError: DWORD;
  logonHandle: THandle;
  //processInfo, processInfoShell: jwawinbase.TProcessInformation;
  //startupInfo,startupInfoShell: jwawinbase.TStartupInfo;
  processInfo: jwawinbase.PROCESS_INFORMATION;
  startupInfo: jwawinbase.STARTUPINFO;

  useWorkDir: string;
  mypass: string;
  lpProfileInfo: jwaprofinfo.PROFILEINFO;
  lpEnvironment: LPLPVOID;
  //myimperson : TImpersonator;
  ConsoleTitle: string;
  Help: string;
  WinStaName: string;
  DesktopName: string;
  hUserToken: THandle;
  hWindowStation: HWINSTA;
  hDesktop: HDESK;
  mypid: dword = 0;

  // output catching
  hReadPipe: THandle = 0;
  hWritePipe: THandle = 0;
  BytesRead: longword;
  Buffer: string = '';
  readResult: boolean;
  sa: TSecurityAttributes;
const
  secsPerDay = 86400;
  //ReadBufferSize = 2048;

begin
  params := '';

  if CmdLinePasStr[1] = '"' then
  begin
    line := copy(CmdLinePasStr, 2, length(CmdLinePasStr));
    firstsplit := pos('"', line);
    if firstsplit <= 0 then
    begin
      Result := False;
      Report := 'No valid filename';
      exit;
    end;
    filename := copy(line, 1, firstsplit - 1);
    params := copy(line, firstsplit + 1, length(line));
  end
  else
  begin
    firstsplit := pos(' ', CmdLinePasStr);
    if firstsplit > 0 then
    begin
      filename := copy(CmdLinePasStr, 1, firstsplit - 1);
      params := copy(CmdLinePasStr, firstsplit + 1, length(CmdLinePasStr));
    end
    else
    begin
      filename := CmdLinePasStr;
    end;
  end;
  //Logdatei.Log('>->->'+filename+'='+getExecutableName(filename),LLEssential);
  try
    try
      //mypass := opsiSetupAdmin_Password;
      (*
      myimperson := TImpersonator.create(DSiGetComputerName,'opsiSetupAdmin',mypass);
      if not myimperson.Impersonating then
      *)
(*
      if not LogonUser(PChar('opsiSetupAdmin'), PChar(DSiGetComputerName), PChar(mypass),
           LOGON32_LOGON_INTERACTIVE, LOGON32_PROVIDER_DEFAULT, opsiSetupAdmin_logonHandle) then
      begin
        result := false;
        Logdatei.Log('Logged in as temporary Admin opsiSetupAdmin failed: '
              +  IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) + ')', LLError);
      end
      else
      begin
        Logdatei.Log('Logged in as temporary Admin opsiSetupAdmin ', LLDebug);
        FillChar(lpProfileInfo, SizeOf(lpProfileInfo), 0);
        lpProfileInfo.dwSize := SizeOf(lpProfileInfo);
        lpProfileInfo.lpUserName:=PChar('opsiSetupAdmin');
        lpProfileInfo.dwFlags:=0;
        //CopyMemory(@lpProfileInfo, @opsiSetupAdmin_lpProfileInfo, SizeOf(lpProfileInfo));
        //CopyMemory(@opsiSetupAdmin_lpProfileInfo, @lpProfileInfo, SizeOf(lpProfileInfo));
        //move(@lpProfileInfo, @opsiSetupAdmin_lpProfileInfo, SizeOf(lpProfileInfo));
        //move(@opsiSetupAdmin_lpProfileInfo, @lpProfileInfo, SizeOf(lpProfileInfo));
        //lpProfileInfo.lpProfilePath:='';
        if not LoadUserProfile(opsiSetupAdmin_logonHandle,lpProfileInfo) then
        begin
          result := false;
          Logdatei.Log('Load profile for temporary Admin opsiSetupAdmin failed: '
           + IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) + ')', LLError);
        end
        else
        begin
          Logdatei.Log('Loaded profile for temporary Admin opsiSetupAdmin ', LLDebug);
          opsiSetupAdmin_ProfileHandle :=  lpProfileInfo.hProfile;
          //lpEnvironment := nil;

          if not CreateEnvironmentBlock(lpEnvironment,opsiSetupAdmin_logonHandle,true) then
          begin
            result := false;
            Logdatei.Log('Create EnvironmentBlock for temporary Admin opsiSetupAdmin failed: '
             + IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) + ')', LLError);
          end
          else

          begin
            Logdatei.Log('Created EnvironmentBlock for temporary Admin opsiSetupAdmin ', LLDebug);
            opsiSetupAdmin_lpEnvironment := lpEnvironment^;
            if not DSiEnablePrivilege('SE_TCB_NAME') then
            begin
              Logdatei.Log('EnablePrivilege Error: '
               + IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) + ')', LLError);
            end;
            if not DSiEnablePrivilege('SE_PRIVILEGE_ENABLED') then
            begin
               Logdatei.Log('EnablePrivilege Error: '
               + IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) + ')', LLError);
            end;

             hWindowStation:= GetProcessWindowStation;
             WinStaName := GetUserObjectName(hWindowStation);

             if WinStaName = '' then
               WinStaName := CreateProcDEFWINSTATION;

             if not SetUserObjectFullAccess(hWindowStation) then
             begin
               result := false;
               Logdatei.Log('SetUserObjectFullAccess(hWindowStation) failed: '
                 + IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) + ')', LLError);
             end;

             hDesktop := GetThreadDesktop(GetCurrentThreadId);
             DesktopName := GetUserObjectName(hDesktop);
             if DesktopName = '' then
               DesktopName := CreateProcDEFDESKTOP;

             if not SetUserObjectFullAccess(hDesktop) then
             begin
               result := false;
               Logdatei.Log('SetUserObjectFullAccess(hDesktop) failed: '
                 + IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) + ')', LLError);
             end;

             if result then
             begin
               // Step 4: set the startup info for the new process
               //ConsoleTitle := UserDomain + UserName;
               ConsoleTitle := 'opsi-winst-as-admin';
               FillChar(StartUpInfo, SizeOf(StartUpInfo), #0);
               with StartUpInfo do
               begin
                 cb:= SizeOf(StartUpInfo);
                 lpTitle:= PChar('opsi-winst-as-admin');
                 Help := opsiSetupAdmin_startupinfo_help;
                 lpDesktop:= PChar(Help);
                 dwFlags := STARTF_USESHOWWINDOW;
                 wShowWindow := ShowWindowFlag;
               end;
               FillChar(StartUpInfoShell, SizeOf(StartUpInfoShell), #0);
               with StartUpInfoShell do
               begin
                 cb:= SizeOf(StartUpInfo);
                 lpTitle:= PChar('shell-4-opsi-admin');
                 Help := WinStaName + '\' + DeskTopName;
                 lpDesktop:= PChar(Help);
                 dwFlags := STARTF_USESHOWWINDOW;
                 wShowWindow := ShowWindowFlag;
               end;

              //else
              //if workDir = '' then
                GetDir(0, useWorkDir);
              //else
              //  useWorkDir := workDir;
              FillChar(processInfo, SizeOf(processInfo), 0);
              FillChar(processInfoShell, SizeOf(processInfoShell), 0);
              //FillChar(startupInfo, SizeOf(startupInfo), 0);
              //startupInfo.cb := SizeOf(startupInfo);
              //startupInfo.dwFlags := STARTF_USESHOWWINDOW;
              //startupInfo.wShowWindow := ShowWindowFlag;
              //if not DSiCreateProcessAsUser(logonHandle, nil, PChar(CmdLinePasStr), nil,

              // startup explorer as shell for the environment
              tempstr :=  'explorer.exe';
              if not CreateProcessAsUser(opsiSetupAdmin_logonHandle, nil, PChar(tempstr), nil,
                   nil, false,
                   CREATE_NEW_CONSOLE or CREATE_NEW_PROCESS_GROUP or CREATE_UNICODE_ENVIRONMENT,
                   opsiSetupAdmin_lpEnvironment,
                   //nil,
                   nil, @startupInfoShell, @processInfoShell)
              then
              begin
                Result := False;
                Report := CmdLinePasStr + ' .... CreateProcessAsUser Error ' +
                  IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) + ')';
                Logdatei.Log(Report, LLError);
                CloseHandle(processInfoShell.hProcess);
                CloseHandle(processInfoShell.hThread);
              end
              else

              begin
*)
      // Step 4: set the startup info for the new process

      if not WaitForReturn then
        catchout := False;

      FillChar(sa, SizeOf(sa), 0);
      sa.nLength := sizeof(sa);
      sa.lpSecurityDescriptor := nil;
      sa.bInheritHandle := True;

      if not CreatePipe(hReadPipe, hWritePipe, @sa, 0) then
      begin
        Report := 'Error creating Pipe';
        Result := False;
        exit;
      end;

      ConsoleTitle := 'opsi-winst-as-admin';
      FillChar(StartUpInfo, SizeOf(StartUpInfo), #0);
      with StartUpInfo do
      begin
        cb := SizeOf(StartUpInfo);
        lpTitle := PChar('opsi-winst-as-admin');
        Help := opsiSetupAdmin_startupinfo_help;
        lpDesktop := PChar(Help);
        if catchout then
          dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES
        else
          dwFlags := STARTF_USESHOWWINDOW;

        wShowWindow := ShowWindowFlag;
        hStdInput := 0;
        hStdOutput := hWritePipe;
        hStdError := hWritePipe;
      end;
(*
      if not CreateEnvironmentBlock(lpEnvironment,0,false) then
      begin
        lpEnvironment := nil;
        Logdatei.Log('Create EnvironmentBlock for temporary Admin opsiSetupAdmin failed: '
         + IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) + ')', LLError);
      end;
      opsiSetupAdmin_lpEnvironment := lpEnvironment^;
*)
      GetDir(0, useWorkDir);
      FillChar(processInfo, SizeOf(processInfo), 0);
      // start work process
      if not jwawinbase.CreateProcessAsUser(opsiSetupAdmin_logonHandle,
        nil, PChar(CmdLinePasStr),
        //nil, nil,
        opsiSetupAdmin_pSecAttrib, opsiSetupAdmin_pSecAttrib,
        catchout, // inherit handes only if we catch output
        //CREATE_NEW_CONSOLE or CREATE_NEW_PROCESS_GROUP or CREATE_UNICODE_ENVIRONMENT,
        CREATE_NO_WINDOW or CREATE_UNICODE_ENVIRONMENT or NORMAL_PRIORITY_CLASS,
        opsiSetupAdmin_lpEnvironment,
        //nil,
        nil, startupInfo, processInfo) then
      begin
        Result := False;
        Report := CmdLinePasStr + ' .... CreateProcessAsUser Error ' +
          IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) + ')';
        Logdatei.Log(Report, LLError);
        CloseHandle(processInfo.hProcess);
        CloseHandle(processInfo.hThread);
      end
      else
      begin
        Logdatei.Log(
          'Created process as temporary Admin opsiSetupAdmin ', LLDebug);
        Result := True;

        desiredProcessStarted := False;
        WaitForProcessEndingLogflag := True;
        setLength(resultfilename, 400);
        presultfilename := PChar(resultfilename);
        //mypid := processinfo.dwProcessId;

        //FindExecutable(PChar(filename), nil, presultfilename);
        //FindFirstTask ( ExtractFilename(presultfilename), processID, parentProcessID, info);
        //allChildrenIDs := TStringList.create;
        //allChildrenIDs.add ( inttoStr(processID));

        if not WaitForReturn and (WaitSecs = 0) then
          Report := 'Process started:    ' + CmdLinePasStr
        else
        begin
                   (*  waitForSingleObject (ProcessInformation.hProcess, INFINITE);
                   CloseHandle (ProcessInformation.hProcess);
                    (( das funktioniert nicht ))
                   *)

          running := True;
          starttime := now;
          WaitWindowStarted := False;

          BytesRead := 0;

          while running do
          begin
            nowtime := now;

            running := False;

            if catchout and (not
              (ReadPipe(Buffer, hReadPipe, BytesRead, output, showoutput))) then
              LogDatei.log('Read Error: ' + SysErrorMessage(getLastError()), LLError);

            (* wait for task vanished *)

            if WaitForWindowAppearing
                    (* waiting condition 0:
                       wait until a window is appearing
                    *) then
            begin
              if FindWindowEx(0, 0, nil, PChar(Ident)) = 0 then
                running := True;
            end

            else if WaitForWindowVanished and not WaitWindowStarted
                    (* waiting condition 1:
                       we are waiting for a window that will later vanish
                       but this window did not appear yet *) then
            begin
              if FindWindowEx(0, 0, nil, PChar(Ident)) <> 0 then
                WaitWindowStarted := True;

              if not WaitWindowStarted or WaitForWindowVanished then
                running := True;
              // in case WaitForWindowVanished we are not yet ready
              // but have to check waiting condition 3
            end

            else if not waitsecsAsTimeout and (WaitSecs > 0) and
              ((nowtime - starttime) < waitSecs / secsPerDay)
                     (* waiting condition 2 : we shall observe a waiting time
                                            and it has not finished )
                      *) then
            begin
              running := True;
            end

            else if WaitForProcessEnding and not desiredProcessStarted
                      (* waiting condition 3a : we wait that some other process will come into existence
                      *) then
            begin
              if WaitForProcessEndingLogflag then
              begin
                Logdatei.Log('Waiting for start of "' +
                  ident + '"', LLinfo);
                WaitForProcessEndingLogflag := False;
              end;

              //FindExecutable(PChar(Ident),nil, presultfilename);
              desiredProcessStarted :=
                FindFirstTask(PChar(Ident) (*presultfilename *),
                processID, parentProcessID, info);

              if WaitSecs = 0 then
                running := True
              else
              begin //time out given
                if ((nowtime - starttime) < waitSecs / secsPerDay) then
                  running := True
                else
                  Logdatei.Log('Waiting for "' +
                    ident + '" stopped - time out ' +
                    IntToStr(waitSecs) + ' sec', LLinfo);
              end;

            end

            else if WaitForProcessEnding and desiredProcessStarted
                      (* waiting condition 3b : now we continue waiting until the observed other process will stop
                      *) then
            begin
              running :=
                FindFirstTask(PChar(Ident)
                (*ExtractFilename(presultfilename)*), processID,
                parentProcessID, info);

              if not WaitForProcessEndingLogflag and running then
              begin
                Logdatei.Log('Waiting for process "' +
                  ident + '" ending', LLinfo);
                WaitForProcessEndingLogflag := True;
              end;

              if not running then
              begin
                Logdatei.Log('Process "' + ident +
                  '" ended', LLinfo);
              end
              else
              begin

                if (waitSecs > 0) // we look for time out
                  and  //time out occured
                  ((nowtime - starttime) >= waitSecs / secsPerDay) then
                begin
                  running := False;
                  logdatei.log('Waiting for ending of "' +
                    ident + '" stopped - waitSecs out ' +
                    IntToStr(waitSecs) + ' sec', LLinfo);
                end;

              end;

            end

            //else if not FpcProcess.Running

            else if GetExitCodeProcess(processInfo.hProcess, lpExitCode) and
              (lpExitCode <> still_active)
            //else if FpcProcess.ExitStatus  <> still_active
                      (* waiting condition 4 :  Process has finished;
                         we still have to look if WindowToVanish did vanish if this is necessary

                         *) then
            begin
              Logdatei.Log(
                'Process terminated at: ' + DateTimeToStr(now) +
                ' exitcode is: ' + IntToStr(lpExitCode), LLDebug2);
              if WaitForWindowVanished then
              begin
                if not (FindWindowEx(0, 0, nil, PChar(Ident)) = 0) then
                begin
                  running := True;
                end;
              end;

            end

            else if waitForReturn
            (* waiting condition 4 : Process is still active *) then
            begin
              if waitsecsAsTimeout and (waitSecs > 0) // we look for time out
                and  //time out occured
                ((nowtime - starttime) >= waitSecs / secsPerDay) then
              begin
                running := False;
                Logdatei.Log(
                  'Waited for the end of started process"' +
                  ' - but time out reached after ' +
                  IntToStr(waitSecs) + ' sec.', LLInfo);
                // try to close process
                //if KillProcessbypid(mypid) then
                //    Logdatei.Log('Killed process with pid: '+ IntToStr(mypid), LLInfo)
                //else Logdatei.Log('Coud not kill process with pid: '+ IntToStr(mypid), LLWarning);

              end
              else
              begin
                running := True;
              end;
            end;

            if running then
            begin
              ProcessMess;
              //sleep(50);
              sleep(1000);
              GetExitCodeProcess(processInfo.hProcess, lpExitCode);
              ProcessMess;
              logdatei.log('Waiting for ending at ' +
                DateTimeToStr(now) + ' exitcode is: ' + IntToStr(lpExitCode), LLDebug2);
              ProcessMess;
            end;
          end;

          ProcessMess;

          if catchout then
          begin
            repeat
              readResult := ReadPipe(Buffer, hReadPipe, BytesRead, output, showoutput);
              if not readResult then
                LogDatei.log('Read Error: ' + SysErrorMessage(getLastError()), LLError);
            until (BytesRead <= 0) or (not readResult);

            // add remainder of buffer as last line
            if Buffer <> '' then
              output.Add(Buffer);
          end;

          ProcessMess;
          GetExitCodeProcess(processInfo.hProcess, lpExitCode);
          exitCode := longint(lpExitCode);
          //Report := 'Process executed  + CmdLinePasStr
          Report := 'ExitCode ' + IntToStr(exitCode) +
            '    Executed process "' + CmdLinePasStr + '"';
        end;
        CloseHandle(processInfo.hProcess);
        CloseHandle(processInfo.hThread);
      end;   //CreateProcessAsUser   (work)
    except
      on e: Exception do
      begin
        Logdatei.Log('Exception in StartProcess_as: ' +
          e.message, LLDebug);
        Report := 'Could not execute process "' + CmdLinePasStr + '"';
        exitcode := -1;
        Result := False;
      end;
    end;
  finally
    //if not (processInfo.hProcess = nil) then
    //CloseHandle(processInfo.hProcess);
    //if not (processInfo.hThread = nil) then
    //CloseHandle(processInfo.hThread);
    lastError := GetLastError;
    //if not (logonHandle = nil) then
    //CloseHandle(logonHandle);
  end;
end;

{$ENDIF WIN32}


function StartProcess(CmdLinePasStr: string; ShowWindowFlag: integer;
  WaitForReturn: boolean; WaitForWindowVanished: boolean;
  WaitForWindowAppearing: boolean; WaitForProcessEnding: boolean;
  Ident: string; WaitSecs: word; var Report: string; var ExitCode: longint): boolean;
var
  runAs: TRunAs;
begin
  // set runas
  // provide a temp stringlist
  runAs := traInvoker;

  Result := StartProcess(CmdLinePasStr, ShowWindowFlag, False,
    WaitForReturn, WaitForWindowVanished, WaitForWindowAppearing,
    WaitForProcessEnding, runas, Ident, WaitSecs, Report, ExitCode);
end;

function StartProcess(CmdLinePasStr: string; ShowWindowFlag: integer;
  WaitForReturn: boolean; WaitForWindowVanished: boolean;
  WaitForWindowAppearing: boolean; WaitForProcessEnding: boolean;
  RunAs: TRunAs; Ident: string; WaitSecs: word; var Report: string;
  var ExitCode: longint): boolean;
var
  output: TXStringList;
begin
  // set waitsecsAsTimeout = false
  // provide a temp stringlist
  output := TXStringList.Create;

  Result := StartProcess(CmdLinePasStr, ShowWindowFlag, tsofHideOutput,
    WaitForReturn, WaitForWindowVanished, WaitForWindowAppearing,
    WaitForProcessEnding, False, runAs, Ident, WaitSecs, Report,
    ExitCode, False, output);

  output.Free;
end;

function StartProcess(CmdLinePasStr: string; ShowWindowFlag: integer;
  WaitForReturn: boolean; WaitForWindowVanished: boolean;
  WaitForWindowAppearing: boolean; WaitForProcessEnding: boolean;
  waitsecsAsTimeout: boolean; RunAs: TRunAs; Ident: string; WaitSecs: word;
  var Report: string; var ExitCode: longint): boolean;
var
  output: TXStringList;
begin
  // compatibility with old version that had no output capturing
  output := TXStringList.Create;

  Result := StartProcess(CmdLinePasStr, ShowWindowFlag, tsofHideOutput,
    WaitForReturn, WaitForWindowVanished, WaitForWindowAppearing,
    WaitForProcessEnding, waitsecsAsTimeout, RunAs, Ident, WaitSecs,
    Report, Exitcode, False, output);

  output.Free;
end;

function StartProcess(CmdLinePasStr: string; ShowWindowFlag: integer;
  showoutputflag: TShowOutputFlag; WaitForReturn: boolean;
  WaitForWindowVanished: boolean; WaitForWindowAppearing: boolean;
  WaitForProcessEnding: boolean; waitsecsAsTimeout: boolean; RunAs: TRunAs;
  Ident: string; WaitSecs: word; var Report: string; var ExitCode: longint;
  catchout: boolean; var output: TXStringList): boolean;
var
  showtitle: string = '';
begin
  Result := StartProcess(CmdLinePasStr, ShowWindowFlag, tsofHideOutput,
    WaitForReturn, WaitForWindowVanished, WaitForWindowAppearing,
    WaitForProcessEnding, waitsecsAsTimeout, RunAs, Ident, WaitSecs,
    Report, Exitcode, False, output, showtitle);
end;

function StartProcess(CmdLinePasStr: string; ShowWindowFlag: integer;
  showoutputflag: TShowOutputFlag; WaitForReturn: boolean;
  WaitForWindowVanished: boolean; WaitForWindowAppearing: boolean;
  WaitForProcessEnding: boolean; waitsecsAsTimeout: boolean; RunAs: TRunAs;
  Ident: string; WaitSecs: word; var Report: string; var ExitCode: longint;
  catchout: boolean; var output: TXStringList; showtitle: string): boolean;

var
  //myStringlist : TStringlist;
  //S: TStringList;
  //M: TMemoryStream;
  //FpcProcess: TProcess;
  //n: longint;
  //BytesRead: longint;
  //WaitWindowStarted: boolean;
  //desiredProcessStarted: boolean;
  //WaitForProcessEndingLogflag: boolean;
  //starttime, nowtime: TDateTime;

  line: string = '';
  filename: string = '';
  params: string = '';
  ext: string = '';
  //dir: string;
  firstsplit: integer;
  //len: DWord;
  //functionresult: hinst;

  //resultfilename: string;
  //presultfilename: PChar;
  //running: boolean;
  //processID: Dword;
  //parentProcessID: DWord;
  //info: string;

  dwThreadId: dword;
  sessionId: dword;
  {$IFDEF WIN32}
  adminmode: Tadminmode;
  WTSGetActiveConsoleSessionId: function: DWord; stdcall;
  WTSQueryUserToken: function(SessionId: ULONG; var phToken: THandle): BOOL; stdcall;
  myduptoken, mytoken: THandle;
  {$ENDIF WIN32}

  showoutput: boolean;
const
  secsPerDay = 86400;
  ReadBufferSize = 2048;

begin
  params := '';

  if CmdLinePasStr[1] = '"' then
  begin
    line := copy(CmdLinePasStr, 2, length(CmdLinePasStr));
    firstsplit := pos('"', line);
    if firstsplit <= 0 then
    begin
      Result := False;
      Report := 'No valid filename';
      exit;
    end;
    filename := copy(line, 1, firstsplit - 1);
    params := copy(line, firstsplit + 1, length(line));
  end
  else
  begin
    firstsplit := pos(' ', CmdLinePasStr);
    if firstsplit > 0 then
    begin
      filename := copy(CmdLinePasStr, 1, firstsplit - 1);
      params := copy(CmdLinePasStr, firstsplit + 1, length(CmdLinePasStr));
    end
    else
    begin
      filename := CmdLinePasStr;
    end;
  end;

  showoutput := False;

  if not WaitForReturn then
    showoutputflag := tsofHideOutput;

  if showoutputFlag in [tsofShowOutput, tsofShowOutputNoSystemInfo] then
    showoutput := True;

  {$IFDEF GUI}


  if showoutput then
  begin
    FBatchOberflaeche.SetElementLeft(5, eMainForm);//Left := 5;
    FBatchOberflaeche.SetElementTop(5, eMainForm);//Top := 5;

    // In the normal case of tsofShowOutput
    // we call CreateSystemInfo and show the
    // output of the called process

    // if tsofShowOutputNoSystemInfo is used
    // we assume that the caller already has
    // created a SystemInfo and just append
    // to that one. We also won't free it and
    // assume the caller does it.
    if showoutputflag = tsofShowOutput then
    begin
      LogDatei.log('Start Showoutput', LLInfo);
      CreateSystemInfo;
      SystemInfo.Memo1.Lines.Clear;
    end;

    SystemInfo.Memo1.Color := clBlack;
    SystemInfo.Memo1.Font.Color := clWhite;
    //systeminfo.BitBtn1.Enabled := False;
    systeminfo.BitBtn1.Visible := False;
    //systeminfo.Label1.Caption := 'Executing: ' + CmdLinePasStr;
    systeminfo.Label1.Caption := 'Executing: ' + showtitle;
    systeminfo.ShowOnTop;
    ProcessMess;
  end;
  {$ENDIF GUI}

  {$IFDEF UNIX}
  // we start as Invoker
  // we assume that this is a executable
  // we try it via createprocess (Tprocess)
  Logdatei.Log(
    'Start process as invoker: ' + getCommandResult('/bin/bash -c whoami'), LLInfo);
  Result := StartProcess_cp(CmdLinePasStr, ShowWindowFlag, showoutput,
    WaitForReturn, WaitForWindowVanished, WaitForWindowAppearing,
    WaitForProcessEnding, waitsecsAsTimeout, Ident, WaitSecs, Report,
    ExitCode, catchout, output);
  {$ENDIF UNIX}
  {$IFDEF WIN64}
  // we start as Invoker
  // we assume that this is a executable
  // we try it via createprocess (Tprocess)
  LogDatei.log('Start process as invoker.', LLInfo);
  Result := StartProcess_cp(CmdLinePasStr, ShowWindowFlag, showoutput,
    WaitForReturn, WaitForWindowVanished, WaitForWindowAppearing,
    WaitForProcessEnding, waitsecsAsTimeout, Ident, WaitSecs, Report,
    ExitCode, catchout, output);
  {$ENDIF WIN64}
  {$IFDEF WIN32}
  ext := ExtractFileExt(filename);
  if FileExists(filename) and not ((LowerCase(ext) = '.exe') or
    (LowerCase(ext) = '.com')) then
  begin
    // we assume that this is a call of a not executable file
    // this is deprecated so we warn and still try it via shellexecute
    Logdatei.Log('winbatch call of not executable file: ' +
      filename + ' is deprecated and output capturing not supported', LLWarning);
    Result := StartProcess_se(CmdLinePasStr, ShowWindowFlag,
      WaitForReturn, WaitForWindowVanished, WaitForWindowAppearing,
      WaitForProcessEnding, waitsecsAsTimeout, Ident, WaitSecs, Report, ExitCode);
  end
  else
  begin
    if ((RunAs in [traAdmin, traAdminProfile, traAdminProfileExplorer,
      traAdminProfileImpersonateExplorer, traAdminProfileImpersonate,
      traLoggedOnUser]) or opsiSetupAdmin_created) then
      if not (('system' = LowerCase(DSiGetUserName)) or opsiSetupAdmin_created) then
      begin
        RunAs := traInvoker;
        Logdatei.Log(
          'Could not create temporary admin because not running as SYSTEM.', LLWarning);
      end;
    if ('pcpatch' = LowerCase(DSiGetUserName)) or
      //assume: nt5 and started as old mode pcpatch
      ((RunAs = traInvoker)  // explicit started as invoker
      and not (opsiSetupAdmin_runElevated or opsiSetupAdmin_created)) then
    begin
      // we start as Invoker
      // we assume that this is a executable
      // we try it via createprocess (Tprocess)
      LogDatei.log(
        'Start process as invoker: ' + DSiGetUserName, LLInfo);
      Result := StartProcess_cp(CmdLinePasStr, ShowWindowFlag,
        showoutput, WaitForReturn, WaitForWindowVanished, WaitForWindowAppearing,
        WaitForProcessEnding, waitsecsAsTimeout, Ident, WaitSecs,
        Report, ExitCode, catchout, output);
    end
    else if RunAs = traLoggedOnUser then
    begin
      try
        @WTSGetActiveConsoleSessionId :=
          GetProcAddress(LoadLibrary('kernel32.dll'), 'WTSGetActiveConsoleSessionId');
        @WTSQueryUserToken := GetProcAddress(LoadLibrary('wtsapi32.dll'), 'WTSQueryUserToken');
        if (not Assigned(WTSGetActiveConsoleSessionId)) or
          (not Assigned(WTSQueryUserToken)) then
          Logdatei.Log(
            'Error: osfunc: StartProcess: Could not assign WTSGetActiveConsoleSessionId / WTSQueryUserToken',
            LLError);
        sessionId := WTSGetActiveConsoleSessionId;
        if not WTSQueryUserToken(sessionId, opsiSetupAdmin_logonHandle) then
          Logdatei.Log(
            'Error: osfunc: StartProcess: Could not WTSQueryUserToken for sessionid: ' +
            IntToStr(sessionid), LLError);
         (*
         if not DuplicateTokenEx(mytoken, MAXIMUM_ALLOWED, 0,
                      jwawinnt.SecurityIdentification,
                      jwawinnt.TokenPrimary , myduptoken) then
           Logdatei.Log('Error: wifunc: StartProcess: Could not DuplicateTokenEx handle for logged on user',LLError)
         else
           opsiSetupAdmin_logonHandle := myduptoken;
         //DuplicateTokenEx(hToken,MAXIMUM_ALLOWED,NULL,SecurityIdentification,TokenPrimary, &hTokenDup) then
         *)

        Logdatei.Log(
          'Start process as LoggedOnUser: ' + usercontextDomain +
          '\' + usercontextUser, LLInfo);
        Result := StartProcess_cp_lu(CmdLinePasStr, ShowWindowFlag,
          showoutput, WaitForReturn, WaitForWindowVanished,
          WaitForWindowAppearing, WaitForProcessEnding, waitsecsAsTimeout,
          Ident, WaitSecs, Report, ExitCode, catchout, output);
        if not Result then
          Logdatei.Log('Failed to start process as logged on user.', LLError);
      except
        on e: Exception do
        begin
          Logdatei.Log('Error in osfunc: StartProcess: ' +
            e.message, LLError);
        end;
      end;
    end
    else if opsiSetupAdmin_runElevated then
    begin
      // we start as Invoker
      // we assume that this is a executable
      // we try it via createprocess (Tprocess)
      LogDatei.log('Start process elevated ....', LLInfo);
      Result := StartProcess_cp_el(CmdLinePasStr, ShowWindowFlag,
        showoutput, WaitForReturn, WaitForWindowVanished, WaitForWindowAppearing,
        WaitForProcessEnding, waitsecsAsTimeout, Ident, WaitSecs,
        Report, ExitCode, catchout, output);

    end
    else
    begin
      //we should create an admin
      if opsiSetupAdmin_created then
      begin
        //it is still created
        Result := StartProcess_as(CmdLinePasStr, ShowWindowFlag,
          showoutput, WaitForReturn, WaitForWindowVanished,
          WaitForWindowAppearing, WaitForProcessEnding, waitsecsAsTimeout,
          Ident, WaitSecs, Report, ExitCode, catchout, output);
        dwThreadId := GetCurrentThreadId;
        //if SetThreadToken(nil,opsiSetupAdmin_logonHandle) then
        //  Logdatei.Log('SetThreadToken success', LLDebug2)
        //else Logdatei.Log('SetThreadToken failed: ' +
        //  IntToStr(GetLastError) + ' (' +
        //  SysErrorMessage(GetLastError) + ')', LLDebug2);
        // do not remove temporary admin, it has to be removed by script
        (*
        if opsiSetupAdmin_created then
            if not DeleteTemporaryLocalAdmin then
              Logdatei.Log('Could not delete Temporary Admin ', LLWarning);
        *)
         (*
          Logdatei.Log('winbatch started using shellexecute with runas',LLDebug);
          result := StartProcess_se_as(CmdLinePasStr,ShowWindowFlag,
                     WaitForReturn, WaitForWindowVanished,
                     WaitForWindowAppearing, WaitForProcessEnding,
                     waitsecsAsTimeout, Ident, WaitSecs, Report, ExitCode);
           *)
      end
      else
      begin
        try
          if not CreateTemporaryLocalAdmin(runas) then
          begin
            Logdatei.Log('Could not create Temporary Admin ', LLError);
            Logdatei.Log('Try to run without temporary Admin ', LLWarning);
            Result := StartProcess_cp(CmdLinePasStr, ShowWindowFlag,
              showoutput, WaitForReturn, WaitForWindowVanished,
              WaitForWindowAppearing, WaitForProcessEnding, waitsecsAsTimeout,
              Ident, WaitSecs, Report, ExitCode, catchout, output);
          end
          else
          begin
            Result := StartProcess_as(CmdLinePasStr, ShowWindowFlag,
              showoutput, WaitForReturn, WaitForWindowVanished,
              WaitForWindowAppearing, WaitForProcessEnding, waitsecsAsTimeout,
              Ident, WaitSecs, Report, ExitCode, catchout, output);


          (*
          Logdatei.Log('winbatch started using shellexecute with runas',LLDebug);
          result := StartProcess_se_as(CmdLinePasStr,ShowWindowFlag,
                     WaitForReturn, WaitForWindowVanished,
                     WaitForWindowAppearing, WaitForProcessEnding,
                     waitsecsAsTimeout, Ident, WaitSecs, Report, ExitCode);
           *)

          end
        finally
          if opsiSetupAdmin_created then
            if not DeleteTemporaryLocalAdmin then
              Logdatei.Log('Could not delete Temporary Admin ', LLWarning);
        end;
      end;
    end;
  end;
  {$ENDIF WIN32}

  {$IFDEF GUI}
  if showoutput then
  begin
    // if tsofShowOutputNoSystemInfo we assume
    // the caller will free the SystemInfo for us
    if showoutputflag = tsofShowOutput then
    begin
      LogDatei.log('Stop Showoutput', LLInfo);
      SystemInfo.Free;
      SystemInfo := nil;
    end;
    FBatchOberflaeche.BringElementToFront(eMainForm);//BringToFront;
    FBatchOberflaeche.SetWindowPosition(poScreenCenter); //centerWindow;
    ProcessMess;
  end;
  {$ENDIF GUI}
end;



{$IFDEF WINDOWS}

function GetOSId: DWord;
 (*
     VER_PLATFORM_WIN32s  Win32s on Windows 3.1
     VER_PLATFORM_WIN32_WINDOWS  Win32 on Windows 95
     VER_PLATFORM_WIN32_NT  Windows NT
    *)

var
  vi: TOSVersionInfo;

begin
  vi.dwOSVersionInfoSize := SizeOf(vi);
  if not GetVersionEx(vi) then
    raise Exception.Create('Fehler ' + IntToStr(GetLastError) +
      ' ("' + syserrormessage(GetLastError) + '")')
  else
    Result := vi.dwPlatformID;
end;

function SetProcessPrivilege(PrivilegeName: string): boolean;
  (* possible values e.g.
     'SeSecurityPrivilege'
     'SeRestorePrivilege'
     'SeShutdownPrivilege'
  *)

var
  hToken: THandle = 0;
  tp: TTokenPrivileges;
  h: DWord = 0;

begin
  OpenProcessToken(GetCurrentProcess, token_adjust_privileges, hToken);
  LookupPrivilegeValue(nil, PChar(PrivilegeName), tp.Privileges[0].Luid);
  tp.PrivilegeCount := 1;
  tp.Privileges[0].Attributes := se_privilege_enabled;
  h := 0;
  Result := AdjustTokenPrivileges(hToken, False, tp, 0, PTokenPrivileges(nil)^, h);
  CloseHandle(hToken);
end;

function ExitSession(ExitMode: TExitMode; var Fehler: string): boolean;

var
  flag: DWord;
begin
  if ExitMode = txmNoExit then
    exit;

  flag := 0;
  case exitmode of
    txmLogout: flag := ewx_logoff;
    txmReboot: flag := ewx_reboot + ewx_force;
  end;

  Result := False;
  Fehler := '';

  if GetOSId = VER_PLATFORM_WIN32_NT then
    (* adjust privileges *)
    SetProcessPrivilege('SeShutdownPrivilege');

  Result := ExitWindowsEx(flag, 0);
  if not Result then
    Fehler := 'Error ' + IntToStr(GetLastError) + ' "' +
      SysErrorMessage(GetLastError) + '"';
end;

{$ENDIF WINDOWS}
{$IFDEF UNIX}
function ExitSession(ExitMode: TExitMode; var Fehler: string): boolean;
var
  exitcode: integer;
  exitcmd: string;
  outstr: string;

  procedure cleanuplog;
  begin
    if LogDatei <> nil then
    begin
      LogDatei.Free;
      LogDatei := nil;
    end;
    Result := True;
    Fehler := '';
  end;

begin
  if ExitMode = txmNoExit then
  begin
    Result := True;
    exit;
  end;

  //flag := 0;
  case exitmode of
    txmLogout: TerminateApp;
    txmReboot:
    begin
      if LogDatei <> nil then
      begin
        LogDatei.LogSIndentLevel := 0;
        Logdatei.Log('============  opsi-script ' +
          OpsiscriptVersionname + ' is regularly and direct rebooting. Time ' +
          FormatDateTime('yyyy-mm-dd  hh:mm:ss ', now) + '.', LLessential);

        sleep(1000);
        //LogDatei.Free;
        //LogDatei := nil;
      end;
      exitcmd := FindDefaultExecutablePath('shutdown');
      if exitcmd = '' then
        exitcmd := '/sbin/shutdown';
      if not FileExistsUTF8(exitcmd) then
        exitcmd := '/usr/sbin/shutdown';
      if not FileExistsUTF8(exitcmd) then
        exitcmd := '/usr/bin/shutdown';
      exitcmd := exitcmd + ' -r +1 opsi-reboot';
      LogDatei.log('Exit command is: ' + exitcmd, LLinfo);
      exitcode := fpSystem(exitcmd);
      if exitcode = 0 then
        cleanuplog
      else
      begin
        LogDatei.log('Got exitcode: ' + IntToStr(exitcode) + ' for command: ' + exitcmd,
          LLWarning);
        exitcmd := '/sbin/reboot';
        LogDatei.log('Now try: ' + exitcmd, LLinfo);
        exitcode := fpSystem(exitcmd);
        if exitcode = 0 then
          cleanuplog
        else
        begin
          LogDatei.log('Got exitcode: ' + IntToStr(exitcode) +
            ' for command: ' + exitcmd,
            LLWarning);
          exitcmd := '/sbin/shutdown -r now';
          LogDatei.log('Now try: ' + exitcmd, LLinfo);
          exitcode := fpSystem(exitcmd);
          if exitcode = 0 then
            cleanuplog
          else
          begin
            Result := False;
            LogDatei.log('Got exitcode: ' + IntToStr(fpgetErrno) +
              ' for command: ' + exitcmd,
              LLWarning);
            exitcmd := 'systemctl reboot';
            LogDatei.log('Now try: ' + exitcmd, LLinfo);
            exitcode := fpSystem(exitcmd);
            if exitcode = 0 then
              cleanuplog
            else
            begin
              LogDatei.log('Got exitcode: ' + IntToStr(exitcode) +
                ' for command: ' + exitcmd,
                LLWarning);
              exitcmd := 'systemctl reboot';
              LogDatei.log('Now try: ' + exitcmd, LLinfo);
              outstr := getCommandResult(exitcmd, exitcode);
              LogDatei.log('Output: ' + outstr, LLinfo);
              if exitcode = 0 then
                cleanuplog
              else
              begin
                LogDatei.log('Got exitcode: ' + IntToStr(exitcode) +
                  ' for command: ' + exitcmd + ' - Giving up',
                  LLerror);
                Fehler := 'Error no.: ' + IntToStr(fpgetErrno);
                if LogDatei <> nil then
                begin
                  LogDatei.Free;
                  LogDatei := nil;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

{$ENDIF UNIX}




function FormatInt(const Value: int64): string;
var
  i: longint = 0;
  reducedvalue: int64 = 0;
  remainder: integer = 0;
begin
  if Value = 0 then
    Result := '0'
  else
  begin
    Result := '';
    reducedValue := Value;

    remainder := reducedValue mod 10;
    i := 0;

    while (remainder <> 0) or (reducedValue <> 0) do
    begin
      if (i > 0) and (i mod 3 = 0) then
        Result := DefaultFormatSettings.ThousandSeparator + Result;
      Result := chr(Ord('0') + remainder) + Result;
      reducedValue := reducedValue div 10;
      remainder := reducedValue mod 10;
      Inc(i);
    end;
  end;
end;


(* ---- Dateihilfsfunktionen *)

function isAbsoluteFileName(const s: string): boolean;
begin
  if (s <> '') and
    // Backslash durch pathdelim und : durch Drivedelim ersetzt
    ((s[1] = PathDelim) or (pos(DriveDelim + Pathdelim, s) > 0)) then
    Result := True
  else
    Result := False;
end;

function GetFileInfo(const CompleteName: string; var fRec: TSearchRec;
  var ErrorInfo: string): boolean;
var
  //Attr: integer;
  FileResult: integer = 0;
begin
  FileResult := FindFirstUTF8(CompleteName, faAnyFile or faSymlink, fRec);
  if FileResult = 0 then
  begin
    Result := True;
    ErrorInfo := '';
  end
  else
  begin
    Result := False;
    ErrorInfo := 'File Error ' + IntToStr(FileResult) + ' (' +
      removeLineBreaks(SysErrorMessage(FileResult)) + ')';
  end;
  FindCloseUTF8(fRec);
end;

function IsDirectory(const FName: string): boolean;
var
  Complete: string = '';
  pureName: string = '';
begin
  Complete := ExpandFileName(Fname);
  pureName := ExtractFileName(Complete);
  if (pos('*', pureName) = 0) and (pos('?', pureName) = 0) and
    (directoryExists(Complete) or (copy(Complete, length(Complete) - 1, 2) =
    Pathdelim + drivedelim)) then
    Result := True
  else
    Result := False;
end;

function SizeOfFile(FName: string): int64;
var
  SearchRec: TSearchRec;
begin
  FName := ExpandFileName(FName);
  if findfirstUTF8(FName, faAnyFile or faSymlink, SearchRec) = 0 then
    Result := SearchRec.Size
  else
    Result := -1;
  FindCloseUTF8(SearchRec);
end;

function ExtractFileDir(const FileName: string): string;
var
  completeFilename: string = '';
begin
  completeFilename := ExpandFileName(Filename);
  Result := SysUtils.ExtractFileDir(completeFileName);
(* 16 Bit code
     result := ExtractFilePath (completeFilename);
     if (length (result) > 0) and (result [length (result)] = Pathdelim)
     then system.delete (result, length (result), 1);
*)
end;

function opsiunquotestr(s1, s2: string): string;
  // old version
  // removes found quotes even if they not at start and end
  // takes only the first char of s2 as mark (no brackets)
  // used by unquote
var
  tempchar: char;
  tempansistr: ansistring;
  tempansistrp: pansichar;
begin
  Result := '';
  if (length(s1) >= 1) and (length(s2) >= 1) then
  begin
    tempchar := trim(s2)[1];
    tempansistr := s1;
    tempansistrp := pansichar(tempansistr);
    Result := AnsiExtractQuotedStr(tempansistrp, tempchar);
    // s1 was not quoted then give back s1
    if Result = '' then
      Result := s1;
    // s1 was not quoted by s2 then give back s1
    // and (ansipos('NULL',s1)=0)
    if (Result = NULL_STRING_VALUE) then
      Result := s1;
  end;
end;


function ExpandFileName(const FileName: string): string;
var
  tmp: string;
begin
  if FileName <> '' then
  begin
    //Result := Filename
    tmp := opsiUnquoteStr(FileName, '"');
    tmp := opsiUnquoteStr(tmp, '''');
    {$IFDEF WINDOWS}
    Result := ExpandFileNameUTF8(tmp);
    {$ENDIF WINDOWS}
    {$IFDEF UNIX}
    Result := CleanAndExpandFilename(tmp);
    {$ENDIF LINUX}
  end
  else
    Result := '';
end;


{$IFDEF WINDOWS}

{$ENDIF WINDOWS}

function Is64BitSystem: boolean;
begin
  Result := False;
  {$IFDEF WIN32}
  Result := DSiIsWow64;
  {$ENDIF WIN32}
  {$IFDEF WIN64}
  Result := True;
  {$ENDIF WIN64}
  {$IFDEF UNIX}
  Result := Is64BitSystemLin;
  {$ENDIF LINUX}
end;

function getOSArchitecture: string;
begin
  Result := 'unknown';
  {$IFDEF WIN32}
  if DSiIsWow64 then Result := 'x86_64'
  else
    Result := 'x86_32';
  {$ENDIF WIN32}
  {$IFDEF WIN64}
  Result := 'x86_64';
  {$ENDIF WIN64}
  {$IFDEF LINUX}
  // At the moment the only supported architecture at Linux
  Result := 'x86_64';
  {$ENDIF LINUX}
  {$IFDEF DARWIN}
  Result := trim(getCommandResult('uname -m'));
  if Result = 'arm64' then Result := 'arm_64';
  if Result = 'x86_64' then
  begin
    if '1' = trim(getCommandResult('sysctl -in sysctl.proc_translated')) then
      Result := 'arm_64';
  end;
  {$ENDIF DARWIN}
end;

function runningInWAnMode: boolean;
var
  mylist: TStringList;
  myserver: string;
begin
  Result := False;
  mylist := parseUrl(opsiserviceURL);
  myserver := trim(LowerCase(mylist.Values['Host']));
  if myserver = 'localhost' then Result := True;
  if myserver = '127.0.0.1' then Result := True;
  if myserver = '::1' then Result := True;
  FreeAndNil(mylist);
end;


function runningAsAdmin: boolean;
begin
  {$IFDEF WIN32}
  Result := DSiIsAdmin;
  {$ENDIF WIN32}
  {$IFDEF WIN64}
  // not implemented
  Result := False;
  {$ENDIF WIN64}
  {$IFDEF UNIX}
  Result := False;
  if FpGeteuid = 0 then
    Result := True;
  {$ENDIF LINUX}
end;

function getLoggedInUser: string;
var
  itemlist: TXStringlist;
  user: string = '';
  dom: string = '';
begin
  {$IFDEF WINDOWS}
  {$IFDEF WIN32}
  if usercontext = '' then
  begin
    // we have no usercontext so let us try to find out
    Result := GetUserNameEx_;
    (*
    if result = '' then
    begin
      result := GetRegistrystringvalue('HKEY_LOCAL_MACHINE\SOFTWARE\Wow6432Node\Microsoft\Windows NT\CurrentVersion\Winlogon','DefaultUserName',false);
    end;
    HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Authentication\LogonUI
    LastLoggedOnUser
    HKEY_LOCAL_MACHINE\SOFTWARE\Wow6432Node\Microsoft\Windows NT\CurrentVersion\Winlogon
    DefaultUserName
      *)
  end
  else
  begin
    itemlist := TXStringlist.Create;
    stringsplit(usercontext, '\', itemlist);
    if itemlist.Count > 1 then
    begin
      dom := itemlist.Strings[0];
      user := itemlist.strings[itemlist.Count - 1];
    end
    else
      user := usercontext;
    itemlist.Free;

    Result := user;
  end;
  {$ELSE WIN32}
  Result := user;
  {$ENDIF WIN32}
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
  //Result := execShellCall('who | awk ''{print $1}'' | uniq','sysnative');
  //Result := getCommandResult('who | awk ''{print $1}'' | uniq');
  //Result := getCommandResult('/bin/bash -c who | awk ''''''{print $1}'''''' | uniq');
  //Result := getCommandResult('/bin/bash -c "who -q | grep `whoami`"');
  Result := getCommandResult('id -un');
  {$ENDIF LINUX}
  {$IFDEF DARWIN}
  // https://scriptingosx.com/2020/02/getting-the-current-user-in-macos-update/
  //Result := getCommandResult('who | awk ''{print $1}'' | uniq');
  //Result := getCommandResult('/bin/bash -c who | awk ''''''{print $1}'''''' | uniq');
  //Result := getCommandResult('/bin/bash -c "who -q | grep `whoami`"');
  Result := getCommandResult('/bin/bash -c "stat -f %Su /dev/console"');
  if Result = 'root' then
    Result := '';
  //echo "show State:/Users/ConsoleUser" | scutil | awk '/Name :/ && ! /loginwindow/ { print $3 }'
  {$ENDIF DARWIN}
end;


procedure MakeBakFile(const FName: string);
var
  BakFName: string = '';
  problem: string = '';
  rebootWanted: boolean;

begin

  if FileExists(FName) then
  begin
    BakFName := changefileext(FName, '.bak');
    FileCopy(FName, BakFName, problem, False, rebootWanted);
  end;

end;


function FileGetWriteAccess(const Filename: string; var ActionInfo: string): boolean;
var
  Attr: integer = 0;
  ErrorNo: integer = 0;
  userPos: integer;
  user: ansistring;
  runninguser: string;
begin
  LogDatei.log_prog('start FileGetWriteAccess', LLInfo);
  Result := True;
  ActionInfo := '';
  if not FileExists(Filename) then
    exit;
  {$IFDEF WINDOWS}
  // Adding file access rights to running user (e.g. system)
  GetNetUser('', runninguser, ActionInfo);
  LogDatei.log_prog('FileGetWriteAccess is calling AddAccessRightsToACL for :' +
    runninguser, LLInfo);
  if AddAccessRightsToACL(Filename, runninguser, JwaWindows.GENERIC_ALL,
    JwaWindows.SET_ACCESS, JwaWindows.SUB_CONTAINERS_AND_OBJECTS_INHERIT) = True then
    LogDatei.log('Access Rights (file) modified and granted to :' +
      runninguser, LLDebug);
  if AddAccessRightsToACL(ExtractFileDir(Filename), runninguser,
    JwaWindows.GENERIC_ALL, JwaWindows.SET_ACCESS,
    JwaWindows.NO_INHERITANCE) = True then
    LogDatei.log('Access Rights (dir) modified and granted to :' + runninguser, LLDebug);

  // does the file path points to a user profile ?
  userPos := Pos(':\Users\', Filename) + 8;
  if userPos > 8 then
  begin
    // we are do it for a user profile - so we give the access rights also for this user
    user := copy(Filename, userPos,
      Pos('\', copy(Filename, userPos, Length(Filename) - 9)) - 1);
    // Adding file access rights to <user>
    LogDatei.log_prog('FileGetWriteAccess is calling AddAccessRightsToACL for :' +
      user, LLInfo);
    if AddAccessRightsToACL(Filename, user, JwaWindows.GENERIC_ALL,
      JwaWindows.SET_ACCESS, JwaWindows.SUB_CONTAINERS_AND_OBJECTS_INHERIT) =
      True then
      LogDatei.log_prog('Access Rights modified and granted to ' + user, LLInfo);
  end;
  // else: we assume that the file is not in a user profile
  //else
  //   LogDatei.log_prog('FileGetWriteAccess: Retrieving userProfile from filePath for modifying Access Rights failed', LLError);

  // manipulating file attributes
  LogDatei.log_prog('FileGetWriteAccess is calling FileGetAttr', LLDebug);
  Attr := SysUtils.FileGetAttr(Filename);
  if Attr and faReadOnly = faReadOnly then
  begin
    Attr := Attr and (not faReadOnly);

    ErrorNo := SysUtils.FileSetAttr(Filename, Attr);
    if ErrorNo = 0 then
      ActionInfo := 'Readonly attribute of file "' + Filename + '" eliminated'
    else
    begin
      Result := False;
      ActionInfo := 'Readonly attribute of file "' + Filename +
        '" could not be eliminated, Code ' + IntToStr(ErrorNo) +
        ' (' + SysErrorMessage(ErrorNo) + ')';
    end;
  end;
  (* I think we do not need this anymore (do 13.8.21)
  if Result = False then
     begin
       LogDatei.log('---FileGetWriteAccess is calling AddAccessRightsToACL for user after FileSetAttr', LLInfo);
       userPos := Pos(':\Users\',Filename)+8;
       if userPos > 8 then
       begin
         user := copy(Filename, userPos, Pos('\',
                               copy(Filename, userPos, Length(Filename)- 9))-1);
         if AddAccessRightsToACL(Filename, user, JwaWindows.GENERIC_ALL, JwaWindows.SET_ACCESS,
                                 JwaWindows.SUB_CONTAINERS_AND_OBJECTS_INHERIT) = True then
         LogDatei.log('---Access Rights granted for user '+ user, LLInfo);
         Result := True;
       end
       else
         LogDatei.log('---Retrieving userProfile from filePath for modifying Access Rights failed', LLError);
     end;
     *)
  {$ENDIF WINDOWS}
  {$IFDEF UNIX}
  if fpAccess(Filename, W_OK) <> 0 then
    if 0 = FpChmod(Filename, S_IWUSR or S_IWGRP or S_IWOTH) then
      ActionInfo := 'Readonly attribute of file "' + Filename + '" eliminated'
    else
    begin
      Result := False;
      ActionInfo := 'Readonly attribute of file "' + Filename +
        '" could not be eliminated, Code ' + IntToStr(fpgeterrno);
    end;
  {$ENDIF UNIX}
end;


{$IFDEF WINDOWS}
function FileCopyWin
  (const sourcefilename, targetfilename: string; var problem: string;
  DelayUntilRebootIfNeeded: boolean; var RebootWanted: boolean;
  doNotChangeAccessRights: boolean): boolean;
var
  //Date: longint;
  //fileresult: integer;
  NewTargetfilename: string = '';
  psourcefilename, ptargetfilename, pnewtargetfilename: PWChar;
  handle: THandle = 0;
  LastError: DWord = 0;
  i: integer = 0;
  retries: integer = 0;
  cmdstr, report: string;
  outlines: TXStringlist;
  ExitCode: integer;

  PLastWriteTime: PFileTime;


  function setTimeForFile(ptargetfilename: PWChar; plastwritetime: PFiletime): boolean;
  var
    handle: THandle = 0;
    attr: integer = 0;
    problem1: string = '';
  begin
    Result := True;

    if not doNotChangeAccessRights then
    begin
      // make file writable
      Result := FileGetWriteAccess(UTF16ToUTF8(ptargetfilename), problem);
      problem1 := problem;
    end;

    handle := CreateFileW(ptargetfilename, GENERIC_WRITE, FILE_SHARE_WRITE,
      nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    if handle = Invalid_Handle_Value then
    begin
      Problem := 'File Err. No. ' + IntToStr(GetLastError) + ' (' +
        SysErrorMessage(GetLastError) + ')';
      Result := False;
      LastError := GetLastError;
    end;

    retries := 0;
    while not Result and (LastError = 32) and (retries < 3)
        {it's now highly probable that the problem file is the source
         and not the target, therefore give the host some time to manage things} do
    begin
      Inc(retries);
      CentralForm.TimerWaitSet(100); // interval of 1/10 seconds
      while not CentralForm.TimerWaitReady(10) // 10 intervals over ?
        do
        ProcessMess;

      handle := CreateFileW(ptargetfilename, GENERIC_WRITE, FILE_SHARE_WRITE,
        nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
      if handle = Invalid_Handle_Value then
      begin
        Result := False;
      end
      else
      begin
        Result := True;
        Problem := '';
      end;

      LastError := GetLastError;
      if not Result then
      begin
        problem := problem + '. While retrying: Errorcode ' + IntToStr(LastError);
        if LastError <> 32 then
          problem := problem + ' ("' + SysErrorMessage(GetLastError) + '")';
      end;
    end;

    if Result then
    begin
      Result := SetFileTime(handle, nil, nil, PLastWriteTime);
      if not Result then
        Problem := 'SetFileTime Error ' + IntToStr(GetLastError) +
          ' (' + RemoveLineBreaks(SysErrorMessage(GetLastError)) + ')';
      if Result and (Attr and faReadOnly = faReadOnly) then
      begin
        SysUtils.FilesetAttr(ptargetfilename, attr);
        if problem = problem1 then
          problem := '';
        // reset write attributes
      end;

    end;

    closeHandle(handle);
  end;

begin
  try
    try
      Result := True;
      problem := '';
      pSourceFilename := PWchar(unicodestring(sourcefilename));
      pTargetFilename := PWchar(unicodestring(targetfilename));


      CheckDriveSpaceForTarget(targetfilename, SizeOfFile(sourcefilename), problem);

      //get filetime

      getMem(PLastWriteTime, sizeof(PFileTime));
      //handle := CreateFile(PChar(UTF8ToWinCP(sourcefilename)), GENERIC_READ,
      //  FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
      handle := CreateFileW(pSourceFilename, GENERIC_READ, FILE_SHARE_READ,
        nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
      if handle = Invalid_Handle_Value then
      begin
        Problem := 'CreateFile for reading, File Error No. ' +
          IntToStr(GetLastError) + ' (' + RemoveLineBreaks(
          SysErrorMessage(GetLastError)) + ')';
        Result := False;
      end;

      if Result then
      begin
        Result := GetFileTime(handle, nil, nil, PLastWriteTime);
        if not Result then
          Problem := 'GetFileTime Error ' + IntToStr(GetLastError) +
            ' (' + RemoveLineBreaks(SysErrorMessage(GetLastError)) + ')';
      end;



      if not Result then
        exit;

      //Logdatei.Log('Before copy: '+SourceFilename, LLDebug);

      (* I think we do not need this anymore (do 13.8.21)
      if AddAccessRightsToACL(pTargetFilename,'SYSTEM', JwaWindows.GENERIC_ALL,
        JwaWindows.SET_ACCESS, JwaWindows.SUB_CONTAINERS_AND_OBJECTS_INHERIT ) = True then
      logdatei.log('---AccessRights of '+ pTargetFilename+' granted to SYSTEM' ,LLInfo)
      else
      logdatei.log('---AccessRights of '+ pTargetFilename+' not modified for SYSTEM',LLInfo);
      *)
      if fileExists(pTargetFilename) and (not doNotChangeAccessRights) then
      begin
        //logdatei.log_prog('---TargetFile exists, calling FileGetWriteAcces', LLInfo);
        Result := FileGetWriteAccess(pTargetFilename, problem);
      end
      else
        //logdatei.log('---TargetFile does not exist',LLInfo)
      ;

      Result := Windows.copyFileW(pSourceFilename, pTargetFilename, False);
      LastError := GetLastError;
      logdatei.log('After copy: ' + SourceFilename + ' LastError: ' +
        IntToStr(LastError) + ' Success: ' + BoolToStr(Result, True), LLDebug2);


      if Result then
      begin
        // set timestamp of target to the value of source
        Result := setTimeForFile(pTargetFilename, plastwritetime);
      end;

      if (not Result) and (LastError = 32)
        { file is being used by another process } and DelayUntilRebootIfNeeded then
      begin
        if GetOSId = VER_PLATFORM_WIN32_NT then
        begin
          logdatei.log('File was in use - try to fix ...', LLDebug2);
          i := 0;
          NewTargetFilename := TargetFilename + '.variant-' + IntToStr(i);
          while FileExists(NewTargetFilename) do
          begin
            i := i + 1;
            NewTargetFilename := TargetFilename + '.variant-' + IntToStr(i);
          end;


          pNewTargetFileName := PWchar(unicodestring(NewTargetFilename));
          Result := Windows.copyFileW(pSourceFilename, pNewTargetFilename, False);

          if not Result then
          begin
            logdatei.log('copy of: ' + SourceFilename + ' to ' +
              NewTargetFilename + ' failed.',
              LLDebug2);
            LastError := GetLastError;
            problem := problem + ' Errorcode ' + IntToStr(LastError) +
              ' ("' + RemoveLineBreaks(SysErrorMessage(GetLastError)) + '")';
            retries := 0;
            while not Result and (LastError = 32) and (retries < 3)
                {it's now highly probable that the problem file is the source
                 and not the target, therefore give the host some time to manage things} do
            begin
              Inc(retries);
              CentralForm.TimerWaitSet(100); // interval of 1/10 seconds
              while not CentralForm.TimerWaitReady(10)
                // 10 intervals over ?
                do
                ProcessMess;

              Result := fileutil.copyFile(SourceFilename, NewTargetFilename, False);
              LastError := GetLastError;
              if not Result then
              begin
                problem := problem + '. While retrying: Errorcode ' +
                  IntToStr(LastError);
                if LastError <> 32 then
                  problem := problem + ' ("' +
                    RemoveLineBreaks(SysErrorMessage(GetLastError)) + '")';
              end;
            end;
          end;

          if Result then
          begin
            logdatei.log('copy of: ' + SourceFilename + ' to ' +
              NewTargetFilename + ' done.',
              LLDebug2);
            // set timestamp of target to the value of source

            Result := setTimeForFile(pNewTargetFilename, plastwritetime);

            problem := problem +
              ' Target file was in use, copying should be completed after reboot.';
            Result := MoveFileExW(pNewTargetFilename, pTargetFilename,
              movefile_delay_until_reboot or movefile_replace_existing);

            if Result then
              RebootWanted := True
            else
              problem := problem + ' Errorcode ' + IntToStr(GetLastError) +
                ' ("' + RemoveLineBreaks(SysErrorMessage(GetLastError)) + '")';
          end;

          //freemem(pNewTargetFilename);

        end;
      end;
      //  $ENDIF

      if not Result then
      begin
        problem := problem + ' Errorcode ' + IntToStr(LastError) +
          ' ("' + RemoveLineBreaks(SysErrorMessage(LastError)) + '")';
        if LastError = 5 then // permission denied
        begin
          {$IFDEF WINDOWS}
          if FileExists(ExtractFilePath(ParamStr(0)) + 'handle.exe') then
          begin
            try
              //RunCommand();
              LogDatei.log('Search for open handles on source and target:', LLinfo);
              outlines := TXStringList.Create;
              cmdstr := ExtractFilePath(ParamStr(0)) + 'handle.exe' +
                ' /u ' + SourceFilename;
              RunCommandAndCaptureOut(cmdstr, True, outlines, report, SW_HIDE, Exitcode);
              for i := outlines.Count - 1 downto 0 do
                if outlines[i] = '' then
                  outlines.Delete(i);
              outlines.EliminateLinesStartingWith('Handle v', False);
              outlines.EliminateLinesStartingWith('Copyright (c)', False);
              outlines.EliminateLinesStartingWith('Sysinternals', False);
              LogDatei.log_list(TStrings(outlines), LLinfo);
              outlines.Clear;
              cmdstr := ExtractFilePath(ParamStr(0)) + 'handle.exe' +
                ' /u ' + TargetFilename;
              RunCommandAndCaptureOut(cmdstr, True, outlines, report, SW_HIDE, Exitcode);
              for i := outlines.Count - 1 downto 0 do
                if outlines[i] = '' then
                  outlines.Delete(i);
              outlines.EliminateLinesStartingWith('Handle v', False);
              outlines.EliminateLinesStartingWith('Copyright (c)', False);
              outlines.EliminateLinesStartingWith('Sysinternals', False);

              LogDatei.log_list(TStrings(outlines), LLinfo);
            finally
              FreeAndNil(outlines);
            end;
          end;
          {$ENDIF WINDOWS}
        end;
      end;

    except
      on E: Exception do
      begin
        Logdatei.Log('File copy Error: source: ' + SourceFilename +
          ' target: ' + TargetFilename + ' error: ' + e.message, LLError);
        //Logdatei.Log('Problem: '+problem, LLError);
        Result := False;
      end;
    end;

  finally
    //freemem(pSourceFilename);
    //freemem(pTargetFilename);
    freemem(PLastWriteTime);
    closeHandle(handle);
  end;

  if not Result then
    exit;
end;

{$ENDIF WINDOWS}


function FileCopy
  (const sourcefilename, targetfilename: string; var problem: string;
  DelayUntilRebootIfNeeded: boolean; var RebootWanted: boolean): boolean;
begin
  Result := FileCopy(sourcefilename, targetfilename, problem,
    DelayUntilRebootIfNeeded, RebootWanted, False);
end;

function FileCopy
  (const sourcefilename, targetfilename: string; var problem: string;
  DelayUntilRebootIfNeeded: boolean; var RebootWanted: boolean;
  followSymlink: boolean): boolean;
begin
  Result := FileCopy(sourcefilename, targetfilename, problem,
    DelayUntilRebootIfNeeded, RebootWanted, False, False);
end;

function FileCopy
  (const sourcefilename, targetfilename: string; var problem: string;
  DelayUntilRebootIfNeeded: boolean; var RebootWanted: boolean;
  followSymlink: boolean; doNotChangeAccessRights: boolean): boolean; overload;
var
  myerrorcode: integer;
  linktarget: string;
begin
  {$IFDEF WINDOWS}
  Result := FileCopyWin(sourcefilename, targetfilename, problem,
    DelayUntilRebootIfNeeded, RebootWanted, doNotChangeAccessRights);
  {$ENDIF WINDOWS}
  {$IFDEF UNIX}
  problem := '';
  try
    // remove existing files to avoid problems like: Error: 26 : Text (code segment) file busy
    if FileExistsUTF8(targetfilename) then
      DeleteFileUTF8(targetfilename);
    if (not FileIsSymlink(sourcefilename)) or followSymlink then
    begin
      if not copyFile(PChar(sourcefilename), PChar(targetfilename), True) then
      begin
        myerrorcode := fpgeterrno;
        problem := 'Could not copy. Error: ' + IntToStr(myerrorcode) +
          ' : ' + SysErrorMessageUTF8(myerrorcode);
        Result := False;
      end
      else
        Result := True;
    end
    else // symlink and (not followSymlink)
    begin
      linktarget := fpReadLink(sourcefilename);
      linktarget := CreateRelativePath(linktarget, ExtractFileDir(linktarget));
      if 0 <> fpsymlink(PChar(linktarget), PChar(targetfilename)) then
        problem := 'Could not create symlink: from ' + targetfilename +
          ' to ' + linktarget;
    end;
  except
    myerrorcode := fpgeterrno;
    Result := False;
    problem := 'Could not copy. Error: ' + IntToStr(myerrorcode) +
      ' : ' + SysErrorMessageUTF8(myerrorcode);
    LogDatei.log(problem, LLNotice);
  end;
  {$ENDIF LINUX}
end;


{$IFDEF WINDOWS}
function GetDrives: string;
var
  buffer, nextS: PWChar;
begin
  buffer := AllocMem(101);
  nextS := buffer;
  GetLogicalDriveStringsW(100, buffer);
  Result := StrPas(nextS);
  while not (StrPas(nextS) = '') do
  begin
    nextS := PWChar(StrEnd(nextS) + 1);
    Result := Result + StrPas(nextS);
  end;
  FreeMem(buffer);
  buffer := nil;
end;

{$ENDIF WINDOWS}

{$IFDEF WINDOWS}
function CheckDriveSpaceForTarget
  (const TargetFilename: string (* directory name is sufficient *);
  const SourceSize: int64; var ProblemInfo: string): boolean;

var
  TargetDriveNo: byte = 0;
  FreeOnDisk: int64 = 0;
  Fullname: string = '';

begin
  Result := True;
  Fullname := Expandfilename(targetfilename);
  TargetDriveNo := Ord(Fullname[1]) - Ord('A') + 1;
  FreeOnDisk := diskfree(TargetDriveNo);

  if SourceSize > FreeOnDisk - 10000 then
  begin
    Result := False;
    problemInfo :=
      'Sourcefile has ' + formatInt  (* IntToStr *)(SourceSize) +
      ' bytes, free on target disk ' + Fullname[1] + ':  ' +
      formatInt (* IntToStr *)(FreeOnDisk) + ' bytes';
  end;
end;

{$ENDIF WINDOWS}

function CheckFileExists(const FName: string; var ErrorInfo: string): boolean;
var
  hFile: THandle = 0;
  retrycount: integer = 0;
  widename: WideString;

begin
  while (not FileExists(FName)) and (retrycount < 10) do
  begin
    Logdatei.Log('Warning: file not found :' + FName + ' -retrying', LLwarning);
    FileGetAttr(FName);
    ProcessMess;
    sleep(200);
    FileGetAttr(FName);
    ProcessMess;
    sleep(200);
    Inc(retrycount);
  end;
  if retrycount = 10 then
  begin
    Logdatei.Log('Warning: file not found :' + FName +
      ' - giving up', LLwarning);
    {$IFDEF UNIX}
    ErrorInfo := 'File Err. No. ' + IntToStr(fpgeterrno);
    Result := False;
    {$ENDIF LINUX}
  end
  else
  begin
    ErrorInfo := '';
    Result := True;
  end;
  {$IFDEF WINDOWS}
  //hFile := CreateFile(PChar(FName), GENERIC_READ, FILE_SHARE_READ,
  //  nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  widename := UTF8ToUTF16(FName);
  hFile := CreateFileW(PWChar(widename), GENERIC_READ, FILE_SHARE_READ,
    nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if hFile = Invalid_Handle_Value then
  begin
    ErrorInfo := 'File Err. No. ' + IntToStr(GetLastError) + ' (' +
      RemoveLineBreaks(SysErrorMessage(GetLastError)) + ')';
    Result := False;
  end
  else
  begin
    ErrorInfo := '';
    Result := True;
  end;
  closeHandle(hFile);
  {$ENDIF WINDOWS}
end;


function CreateTextfile(const FName: string; var ErrorInfo: string): boolean;

var
  dirname: string = '';
  NullFile: Textfile;
begin
  Result := True;
  ErrorInfo := '';
  if not SysUtils.FileExists(FName) then
  begin
    dirname := ExtractFilePath(FName);
    if not (dirname = '') and not // dirname is drivename
      ((dirname[2] = ':') and (length(dirname) <= 3)) and not
      DirectoryExists(dirname) then
    begin
      try
        ForceDirectories(Dirname);
        //ErrorInfo := 'Directory ' + dirname + ' did not exist und was created';
        LogDatei.log('Directory ' + dirname + ' did not exist und was created', LLInfo);
      except
        on e: Exception do
        begin
          ErrorInfo := 'Directory ' + dirname + ' could not be created. ("' +
            e.message + '")';
          Result := False;
        end;
      end;
    end;

    if Result (* Directory exists *) then
    begin
      try
        AssignFile(NullFile, Fname);
        rewrite(NullFile);
        CloseFile(NullFile);
      except
        on e: Exception do
        begin
          ErrorInfo := 'File ' + ExtractFileName(Fname) +
            ' could not be created in Directory ' + dirname + '. ("' +
            e.message + '")';
          Result := False;
        end;
      end;
    end;
  end;
end;

function FindInSubDirs(const dir: string; const filename: string): TStringList;
var
  subdirs: TStringList;
  i: integer;
  fullpath: string;
begin
  Result := TStringList.Create;
  subdirs := FindAllDirectories(dir, False);
  for i := 0 to (subdirs.Count - 1) do
  begin
    fullpath := ConcatPaths([subdirs[i], filename]);
    if FileExists(fullpath) then
      Result.Add(fullpath);
  end;
end;

procedure TCopyCount.Init(NumberCounted: integer);
begin
  FActCount := 0;
  case FCountMode of
    tccmCounting:
    begin
      FReady := False;
      FTotal := 0;
    end;
    tccmCounted:
    begin
      FReady := True;
      FTotal := NumberCounted;
    end;
    tccmNoCounter: FReady := True;
  end;
end;

constructor TCopyCount.Create(CountMode: TcpCountModus; NumberCounted: integer);
begin
  inherited Create;
  FCountMode := CountMode;
  Init(NumberCounted);
end;

function TCopyCount.CheckReady: boolean;
begin
  Result := FReady;
  FActCount := FActCount + 1;
end;


function TCopyCount.TellRatio: real;
begin
  if FTotal > 0 then
    TellRatio := FActCount / FTotal
  else
    TellRatio := 1;
end;


(* --- Strings *)

function PointerAufString(Alpha: string): PChar;
begin
  Result := PChar(Alpha);
end;

(*
procedure DisposeString(p: PChar);
begin
  // was only 16 bit code
end;
*)

function removeLineBreaks(const s0: string): string;
var
  i: integer = 0;
begin
  Result := s0;
  i := 1;
  while i < length(Result) do
  begin
    if (Result[i] = #13) then //and (Result[i + 1] = #10) then
    begin
      Result[i] := ' ';
      system.Delete(Result, i, 1);
    end;
    if (Result[i] = #10) then
    begin
      Result[i] := ' ';
      system.Delete(Result, i, 1);
    end;
    Inc(i);
  end;
  Result := trim(Result);
  Result := Result + '<';
end;


function StrIsIn(const s: string; const A: array of Str20): integer;
var
  i: integer = 0;
begin
  i := Low(A);
  Result := 0;
  while (i <= High(A)) and (Result = 0) do
  begin
    if s = A[i] then
      Result := i - Low(A) + 1
    else
      i := i + 1;
  end;
end;


procedure stringsplit(const s, delimiter: string; var Result: TXStringList);
// calls stringsplit from osparserhelper
begin
  osparserhelper.stringsplit(s, delimiter, TStringList(Result));
end;

function concatPathParts(const part1, part2: string): string;
  (* concatenates parts of a path, e.g. the main path part and the file name, observing that just path separator is set *)
begin
  Result := part1;
  if (length(part1) > 0) and (part1[length(part1)] <> PATHSEPARATOR) then
    Result := Result + PATHSEPARATOR;

  if (length(part2) > 0) and (part2[1] <> PATHSEPARATOR) then
    Result := Result + part2
  else
    Result := Result + copy(part2, 2, length(part2) - 1);
end;

//http://www.lazarusforum.de/viewtopic.php?f=10&t=2821
function WideStringReplace(const S, OldPattern, NewPattern: WideString;
  Flags: TReplaceFlags): WideString;
var
  Srch, OldP, RemS: WideString;
  // Srch and Oldp can contain uppercase versions of S,OldPattern
  P: integer;
begin
  Srch := S;
  OldP := OldPattern;
  if rfIgnoreCase in Flags then
  begin
    Srch := WideUpperCase(Srch);
    OldP := WideUpperCase(OldP);
  end;
  RemS := S;
  Result := '';
  while (Length(Srch) <> 0) do
  begin
    P := Pos(OldP, Srch);
    if P = 0 then
    begin
      Result := Result + RemS;
      Srch := '';
    end
    else
    begin
      Result := Result + Copy(RemS, 1, P - 1) + NewPattern;
      P := P + Length(OldP);
      RemS := Copy(RemS, P, Length(RemS) - P + 1);
      if not (rfReplaceAll in Flags) then
      begin
        Result := Result + RemS;
        Srch := '';
      end
      else
        Srch := Copy(Srch, P, Length(Srch) - P + 1);
    end;
  end;
end;

function StringReplace1(const S, OldPattern, NewPattern: string): string;
begin
  //Result := WideStringReplace(WideString(s), WideString(OldPattern),
  //  WideString(NewPattern), [rfReplaceAll, rfIgnoreCase]);
  Result := SysUtils.StringReplace(s, OldPattern, NewPattern,
    [rfReplaceAll, rfIgnoreCase]);
end;

function StringReplace(const S, OldPattern, NewPattern: string): string;
  (* ersetzt jedes Vorkommen von OldPattern durch NewPattern *)

  //SysUtils.StringReplace(s, OldPattern, NewPattern, [rfReplaceAll, rfIgnoreCase])
  // does not work properly

var
  idx: integer = 0;
  parseString: string = '';
begin
  Result := '';
  parseString := S;
  idx := pos(OldPattern, parseString);
  while (idx > 0) do
  begin
    Result := Result + copy(parseString, 1, idx - 1) + NewPattern;
    parseString := copy(parseString, idx + length(OldPattern), length(parseString));
    idx := pos(OldPattern, parseString);
  end;
  Result := Result + parseString;
end;

function CEscaping(const s: string): string;
begin
  Result := StringReplace(s, '\', '\\');
end;

function CharReplace(p: Pointer; oldC, newC: char; len: DWord): Pointer;
var
  i: DWord = 0;
  p0, p1: Pointer;
  theNewC: char;
begin
  i := 0;
  p1 := p;
  theNewC := newC;
  p0 := @theNewC;
  while i <= len do
  begin
    if PChar(p1)[0] = oldC then
      move(p0, p1, 1);
    i := i + 1;
    p1 := Pointer(DWord(p1) + 1);
  end;
  Result := p1;
end;


function GetIdentComponent(const s: string; var Remaining: string): string;
    (* Liefert Teilstring bis zu einem Punkt; die auf den Punkt folgenden Zeichen
       werden - bei Elimination fuehrender Leerzeichen - auf Remaining zurueckgegeben *)

var
  i: integer = 0;

begin
  i := 1;
  while (i <= length(s)) and (s[i] <> '.') do
    Inc(i);

  GetIdentComponent := copy(s, 1, i - 1);
  Remaining := copy(s, i + 1, length(s) - i);
  Remaining := CutLeftBlanks(Remaining);
end;


function KappeBlanks(const s: string): string;
begin
  Result := trim(s);
    (* result := CutLeftBlanks (s);
    result := CutRightBlanks (result); *)
end;


procedure WortAbspalten(const s: string; var Wortlinks, Rest: string);
var
  i: integer = 0;
  sGekappt: string = '';
begin
  sGekappt := KappeBlanks(s);
  i := pos(' ', sGekappt);
  if i = 0 then
    i := Length(sGekappt) + 1;
  Wortlinks := copy(sGekappt, 1, i - 1);
  Rest := copy(s, i + 1, length(sGekappt) - i);
  Rest := CutLeftBlanks(Rest);
end;


procedure SectionnameAbspalten(const s: string; var Sektion, Rest: string);
(* vorausgesetzt ist, dass vor Sectionname nichts mehr steht und s mit [ beginnt *)
var
  i: integer = 0;
begin
  i := pos(']', s);
  Sektion := copy(s, 2, i - 2);
  Rest := copy(s, i + 1, length(s) - i);
  Rest := KappeBlanks(Rest);
end;


procedure IdentAbspalten(const s: string; var Ident, Value: string);
var
  i: integer = 0;
begin
  i := pos('=', s);
  if i = 0 then
    i := length(s) + 1;

  Ident := copy(s, 1, i - 1);
  Value := copy(s, i + 1, length(s) - i);
  Ident := KappeBlanks(Ident);
  Value := KappeBlanks(Value);
end;



(*
{ TXStringList }


{:Converts Unicode string to Ansi string using specified code page.
  @param   ws       Unicode string.
  @param   codePage Code page to be used in conversion.
  @returns Converted ansi string.
  Author: Primoz Gabrijelcic
}

(*
function WideStringToString(const ws: WideString; codePage: word): ansistring;
var
  l: integer;
begin
  if ws = '' then
    Result := ''
  else
  begin
    l := WideCharToMultiByte(codePage, WC_COMPOSITECHECK or WC_DISCARDNS or
      WC_SEPCHARS or WC_DEFAULTCHAR, @ws[1], -1, nil, 0, nil, nil);
    SetLength(Result, l - 1);
    if l > 1 then
      WideCharToMultiByte(codePage,
        WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR,
        @ws[1], -1, @Result[1], l - 1, nil, nil);
  end;
end; { WideStringToString }


{:Converts Ansi string to Unicode string using specified code page.
  @param   s        Ansi string.
  @param   codePage Code page to be used in conversion.
  @returns Converted wide string.
  Author: Primoz Gabrijelcic
}
function StringToWideString(const s: ansistring; codePage: word): WideString;
var
  l: integer;
begin
  if s = '' then
    Result := ''
  else
  begin
    l := MultiByteToWideChar(codePage, MB_PRECOMPOSED, PChar(@s[1]), -1, nil, 0);
    SetLength(Result, l - 1);
    if l > 1 then
      MultiByteToWideChar(CodePage, MB_PRECOMPOSED, PChar(@s[1]),
        -1, PWideChar(@Result[1]), l - 1);
  end;
end; { StringToWideString }

*)

(* TPatchList *)

procedure TPatchList.SetItemPointer(const newvalue: integer);
var
  ValueText: string;
begin
  if ip <> newvalue then
  begin
    ip := newvalue;
    if newValue < 0 then
    begin
      ValueText := 'Top (before first line)';
      ip := -1;
    end
    else
      ValueText := IntToStr(newValue);

    Logdatei.Log('Item Pointer set to ' + Valuetext, LLinfo);
  end;
end;

procedure TPatchList.Delete(StrNo: integer);
var
  deleteS: string;
begin
  if (StrNo <= -1) or (StrNo >= Count) then
    Logdatei.Log('Item pointer shows not to valid item, nothing to delete',
      LLinfo)
  else
    try
      deleteS := Strings[StrNo];
      inherited Delete(StrNo);
      LogS := 'Deleted item no. ' + IntToStr(StrNo) + ': ' + deleteS;
      Logdatei.Log(LogS, LLinfo);
    except
      LogS := 'Error: ' + 'Item ' + IntToStr(StrNo) + ' could not be deleted';
      LogDatei.log(LogS, LLError);
    end;
end;

procedure TPatchList.Add(const s: string);
begin
  try
    inherited add(s);
    LogS := 'Added item ''' + s + '''';
    Logdatei.Log(LogS, LLinfo);
  except
    LogS := 'Error: ' + 'Item  could not be added';
    LogDatei.log(LogS, LLError);
  end;
end;

procedure TPatchList.insert(i: integer; const s: string);
begin
  try
    inherited insert(i, s);
    LogS := 'Inserted item ''' + s + '''' + ' at position ' + IntToStr(i);
    Logdatei.Log(LogS, LLinfo);
  except
    LogS := 'Error: ' + 'Item  could not be inserted';
    LogDatei.log(LogS, LLError);
  end;
end;


{
function TPatchList.FindFirstItemUsing
             (Const SearchString : String; MatchCase : Boolean; startIndex : Integer) : Integer;
var
  found : Boolean;
  SearchItem, SearchUString : String;
  i : Integer;
}

function TPatchList.FindFirstItem
  (const SearchString: string; MatchCase: boolean; startIndex: integer;
  var found: boolean): integer;
var
  SearchItem, SearchUString: string;
  i: integer;
begin
  if MatchCase then
    SearchUString := SearchString
  else
    SearchUString := UTF8UpperString(SearchString);

  found := False;
  Result := startIndex;
  i := startIndex + 1;
  if i <= 0 then
    i := 1;

  while not found and (i <= Count) do
  begin
    if MatchCase then
      SearchItem := Strings[i - 1]
    else
      SearchItem := UTF8UpperString(Strings[i - 1]);

    if SearchUString = SearchItem then
    begin
      Result := i - 1;
      found := True;
    end
    else
      Inc(i);
  end;

  if found then
  begin
    LogS := 'Item no. ' + IntToStr(Result) + ' is containing ''' +
      SearchUString + '''';
    LogDatei.log(LogS, LLInfo);
  end
  else
  begin
    LogS := 'No item found containing ''' + SearchUString + '''';
    LogDatei.log(LogS, LLInfo);
  end;
end;



function TPatchList.FindFirstItemWith(const SearchString: string;
  MatchCase: boolean; startIndex: integer): integer;
var
  found: boolean;
  SearchItem: string = '';
  SearchUString: string = '';
  i: integer = 0;
begin
  if MatchCase then
    SearchUString := SearchString
  else
    SearchUString := AnsiUpperCase(SearchString);

  found := False;
  Result := startIndex;
  i := startIndex + 1;
  if i <= 0 then
    i := 1;

  while not found and (i <= Count) do
  begin
    if MatchCase then
      SearchItem := Strings[i - 1]
    else
      SearchItem := AnsiUpperCase(Strings[i - 1]);

    if pos(SearchUString, SearchItem) > 0 then
    begin
      Result := i - 1;
      found := True;
    end
    else
      Inc(i);
     {
     LogS := 'Debug: Item no. ' + IntToStr (i-1)  + ' is ''' + SearchItem + '''';
     Logdatei.Log (LogS, LLdebug);
     }
    if (i / 1000) = 0 then
      LogDatei.log('Searching in Item no: ' + IntToStr(i), LLDebug2);
  end;

  if found then
  begin
    LogS := 'Item no. ' + IntToStr(Result) + ' is containing ''' +
      SearchUString + '''';
    Logdatei.Log(LogS, LLinfo);
  end
  else
  begin
    LogS := 'No item found containing ''' + SearchUString + '''';
    Logdatei.Log(LogS, LLinfo);
  end;
end;

function TPatchList.FindFirstItemStartingWith
  (const SearchString: string; MatchCase: boolean; startIndex: integer): integer;
var
  found: boolean;
  SearchItem: string = '';
  SearchUString: string = '';
  i: integer = 0;
begin
  if MatchCase then
    SearchUString := SearchString
  else
    SearchUString := AnsiLowerCase(SearchString);

  found := False;
  Result := startIndex;
  i := startIndex + 1;
  if i <= 0 then
    i := 1;

  while not found and (i <= Count) do
  begin
    SearchItem := Strings[i - 1];
    if not MatchCase then
      SearchItem := AnsiLowerCase(SearchItem);

    if (length(SearchItem) >= length(SearchUString)) and
      (SearchUString = copy(SearchItem, 1, length(SearchUString))) then
    begin
      Result := i - 1;
      found := True;
    end
    else
      Inc(i);
  end;


  if found then
  begin
    LogS := 'Item no. ' + IntToStr(Result) + ' is starting with ''' +
      SearchUString + '''';
    Logdatei.Log(LogS, LLinfo);
  end
  else
  begin
    LogS := 'No item found starting with ''' + SearchUString + '''';
    Logdatei.Log(LogS, LLinfo);
  end;
end;

(* TEditScript *)
procedure TEditScript.SetString(const i: integer; const s: string);
begin
  if (i >= 0) and (i + 1 <= Count) then
    Strings[i] := s
  else
    Add(s);
end;



(* TuibIniScript *)

function IsHeaderLine(const s: string): boolean;
var
  TestS: string = '';
begin
  TestS := KappeBlanks(s);
  if (length(TestS) > 2) and (TestS[1] = '[') and (TestS[length(TestS)] = ']') then
    IsHeaderLine := True
  else
    IsHeaderLine := False;
end;

function TuibIniScript.FindEndOfSectionIndex(const OffLine: integer): integer;
  // we assume that the section end below the line withe the index = offline
var
  i: integer = 0;
  s: string = '';
  weitersuchen: boolean;
  inBlock: boolean;
begin
  i := OffLine;
  Result := OffLine;
  Inc(i);
  weitersuchen := True;
  inBlock := False;
  while weitersuchen and (i + 1 <= Count) do
  begin
    s := Kappeblanks(Strings[i]);

       (*     versuch mit zuordnung eines Blocks zu einer Sektion
              funktioniert, aber nur auf einer Ebene
       if (s<>'') and (s[1] = '{')
       then
       Begin
         inBlock := true;
         s := copy(s,2,length(s)-1);
       End;

       if inBlock and (s<>'') and (s[length(s)] = '}')
       then
       Begin
         inBlock := false;
         s := copy(s, 1, length(s)-1);
       End;

       if inBlock
       then
       Begin
         inc(i);
         result := i; // the next line is candidate for being the last line of section + block
       End

       else *)
    begin
      if IsHeaderLine(s) then
      begin
        weitersuchen := False;
      end
      else if IsEndOfLocalFunction(s) then
        weitersuchen := False
      else
      begin
        if (s <> '') and (s[1] <> LineIsCommentChar) then
          (* i ist neuer Kandidat fuer Sektionsende *)
          Result := i;
        Inc(i);
      end;
    end;
  end;
end;

function TuibIniScript.FindSectionheaderIndex(const Sectionname: string): integer;
var
  found: boolean;
  i: integer = 0;
  s : string = '';
begin
  if Count = 0 then
    Result := -1
  else
  begin
    found := False;
    i := 1;
    while not found and (i <= Count) do
    begin
      s := KappeBlanks(Strings[i - 1]);
      //if AnsiUpperCase(s) = '[' + AnsiUpperCase(Sectionname) + ']' then  //unuseful
      if LowerCase(s) = '[' + LowerCase(Sectionname) + ']' then
        found := True
      else
      // look for old german Aktionen sections
      if (AnsiUpperCase(Sectionname) = 'ACTIONS') and
        (AnsiUpperCase(s) = '[AKTIONEN]') then
        found := True
      else
        Inc(i);
    end;

    if found then
      Result := i - 1
    else
    begin
      Result := -1;
      LogDatei.log_prog('FindSectionheaderIndex: Section not found: '+Sectionname,LLinfo);
    end;
  end;
end;

procedure TuibIniScript.GetSectionTitles(var Resultlist: TXStringList);
var
  i: integer = 0;
  s: string = '';
begin
  for i := 0 to Count - 1 do
  begin
    s := trim(Strings[i]);
    if (length(s) > 1) and (s[1] = '[') and (s[length(s)] = ']') then
      resultlist.add(s);
  end;
end;

procedure TuibIniScript.GetSectionNames(var Resultlist: TXStringList);
var
  i: integer = 0;
  s: string = '';
begin
  for i := 0 to Count - 1 do
  begin
    s := trim(Strings[i]);
    if (length(s) > 1) and (s[1] = '[') and (s[length(s)] = ']') then
      resultlist.add(copy(s, 2, length(s) - 2));
  end;
end;

function TuibIniScript.GetSectionLines
  (const Sectionname: string; var Resultlist: TXStringList;
  var StartlineNo: integer; takeCommentLines, takeEmptyLines, trimmed:
  boolean): boolean;

var
  i: integer = 0;
  j: integer = 0;
  n: integer = 0;
  searchstartindex: integer = 0;
  s: string = '';
  ersteZeileSuchen: boolean;
  DefFuncFound: boolean = False;
  EndoFuncFound: boolean = False;

begin
  try
    Result := False;
    i := FindSectionheaderIndex(Sectionname);
    if i >= 0 then
      StartlineNo := i + 1
    else
      StartlineNo := i;
    if (i >= 0)      // section header existing
      and (i + 1 <= Count - 1)
    //the i+1-line exists - the line below the section header
    then
    begin
      Inc(i);
      // the line below the section header
      // do the next section start - so this section is empty

      ersteZeileSuchen := True;
      while (i <= Count - 1) and ersteZeileSuchen do
      begin
        s := trim(Strings[i]);
        if (length(s) > 0) and (s[1] <> LineIsCommentChar) then
        begin
          ersteZeileSuchen := False;
          if isHeaderLine(s) then
            Result := False
          else //i ist Index der ersten Sektionszeile
            Result := True;
        end
        else
          Inc(i);
      end;

      if Result then
      begin // Sektionsinhalt existiert
        searchstartindex := StartlineNo;
        // if we have defFunc section headers before EndFunc should be ignored
        searchstartindex := getFirstLineAfterEndFunc(self, searchstartindex);
        n := FindEndOfSectionIndex(searchstartindex);
        for j := StartlineNo to n do
        begin
          s := KappeBlanks(Strings[j]);
          if s = '' then
          begin
            if takeEmptyLines then
              Resultlist.Add(s);
          end
          else (* s <> '' *)
          begin
            if takeCommentLines or (s[1] <> LineIsCommentChar) then
            begin
              (*
              if (s<> '') and (s[1] = '{')
              then
                s := copy (s,2, length(s) -1);

              if (s<> '') and (s[length(s)] = '}')
              then
                s := copy (s,1, length(s) -1);
              *)
              if trimmed then
                Resultlist.Add(s)
              else
                Resultlist.Add(cutRightBlanks(Strings[j]));

            end;
          end;
        end;
      end; //Sektionsinhalt existiert
    end;
  except
    on E: Exception do
    begin
      Logdatei.log('Exception in GetSectionLines: ' + Sectionname, LLCritical);
      Logdatei.log(e.ClassName + ' system message: "' +
        E.Message + '" - giving up',
        LLCritical);
    end;
  end;
end;


(* TuibPatchIniFile *)

constructor TuibPatchIniFile.Create;
begin
  inherited Create;
end;

destructor TuibPatchIniFile.Destroy;
begin
  inherited Destroy;
end;

procedure TuibPatchIniFile.SaveToFile(const Filename: string);
var
  ActionInfo: string = '';
  myfilename: string;
begin
  myfilename := ExpandFileName(FileName);
  if FileGetWriteAccess(myfilename, ActionInfo) then
  begin
    if ActionInfo <> '' then
    begin
      LogS := 'Info: ' + ActionInfo;
      LogDatei.log(LogS, LLinfo);
      LogDatei.NumberOfHints := LogDatei.NumberOfHints + 1;
    end;
    inherited SaveToFile(myfilename, 'utf8');
  end
  else
  begin
    LogS := 'Error: ' + ActionInfo;
    LogDatei.log(LogS, LLError);
  end;
end;

function TuibPatchIniFile.FindIdentIndex(const Sectionname, Ident: string): integer;
var
  found: boolean;
  i, n: integer;
  s: string = '';
  Ident0: string = '';
  Value0: string = '';
begin
  i := FindsectionheaderIndex(Sectionname);
  if i = -1 then
    Result := -1
  else
  begin
    Inc(i);
    n := FindEndOfSectionIndex(i);
    found := False;
    while not found and (i <= n) and (i + 1 <= Count) do
    begin
      s := KappeBlanks(Strings[i]);
      IdentAbspalten(s, Ident0, Value0);
      if UpperCase(Ident0) = UpperCase(Ident) then
        found := True
      else
        Inc(i);
    end;

    if found then
      Result := i
    else
      Result := -1;
  end;
end;

function TuibPatchIniFile.FindEntryIndex(
  const Sectionname, Ident, Value: string): integer;
var
  i, n: integer;
  found: boolean;
  s: string = '';
  Ident0: string = '';
  Value0: string = '';

begin
  i := FindsectionheaderIndex(Sectionname);
  if i = -1 then
    Result := -1
  else
  begin
    Inc(i);
    n := FindEndOfSectionIndex(i);
    found := False;
    while not found and (i <= n) and (i + 1 <= Count) do
    begin
      s := KappeBlanks(Strings[i]);
      IdentAbspalten(s, Ident0, Value0);
      found := (UpperCase(Ident0 + '=' + Value0) =
        UpperCase(Ident + '=' + Value));
      if not found then
        Inc(i);
    end;

    if found then
      Result := i
    else
      Result := -1;
  end;

end;

function TuibPatchIniFile.FindValueIndex(const Sectionname, Value: string): integer;
var
  i, n: integer;
  found: boolean;
  s: string = '';
  Ident0: string = '';
  Value0: string = '';

begin
  i := FindsectionheaderIndex(Sectionname);
  if i = -1 then
    Result := -1
  else
  begin
    Inc(i);
    n := FindEndOfSectionIndex(i);
    found := False;
    while not found and (i <= n) and (i + 1 <= Count) do
    begin
      s := KappeBlanks(Strings[i]);
      IdentAbspalten(s, Ident0, Value0);
      found := (UpperCase(Value0) = UpperCase(Value));
      if not found then
        Inc(i);
    end;

    if found then
      Result := i
    else
      Result := -1;
  end;

end;


procedure TuibPatchIniFile.addSection(const Sektion: string);
(* fuegt eine Sektion hinzu, sofern sie nicht bereits existiert *)
var
  s: string = '';
begin
  LogSCommand := 'addSection ' + '[' + Sektion + ']';
  Logged := Logdatei.Log(LogSCommand, LLinfo);

  if FindSectionheaderIndex(Sektion) = -1 then
  begin
    if (Count >= 1) and (length(Strings[Count - 1]) > 0) then
      (* wenn nicht vorhanden, Leerzeile anhaengen *)
    begin
      s := '';
      Add(s);
    end;

    s := '[' + Sektion + ']';
    Add(s);

    LogS := '  done';
    Logdatei.Log(LogS, LLinfo);
  end
  else
  begin
    if not Logged then
      Logdatei.Log(LogSCommand, LLinfo);

    LogS := 'Section ' + Sektion + ' existed already - nothing to do';
    Logdatei.Log(LogS, LLwarning);
  end;

end;

procedure TuibPatchIniFile.addEntry(const Sektion, Eintrag: string);
 (* Fuegt einen Eintrag
    Ident = Value
    in eine Sektion ein, falls der vordere Teil
    Ident =
    noch nicht existiert, sonst wird nichts geschrieben
    Falls die Sektion noch nicht da ist wird er erstellt. *)
var
  OldId: string = '';
  OldVal: string = '';
  i: integer = 0;
begin
  LogSCommand := 'addEntry ' + '[' + Sektion + '] ' + Eintrag;
  Logged := Logdatei.Log(LogSCommand, LLinfo);
  IdentAbspalten(Eintrag, Ident, Value);
  if FindSectionheaderIndex(Sektion) = -1 then
    addSection(Sektion);

  i := FindIdentIndex(Sektion, Ident);
  if i = -1 then
  begin
    i := FindSectionheaderIndex(Sektion);
    i := FindEndOfSectionIndex(i);
    Insert(i + 1, Eintrag);

    LogS := '  done';
    Logdatei.Log(LogS, LLinfo);
  end
  else
  begin
    IdentAbspalten(Strings[i], OldId, OldVal);
    if upperCase(OldVal) = upperCase(Value) then
    begin
      LogS := 'Entry existed already';
      Logdatei.Log(LogS, LLinfo);
    end
    else
    begin
      if not Logged then
        Logged := Logdatei.Log(LogSCommand, LLinfo);
      LogS := 'Warning:  ident  ' + Ident + '  existed already with value ' +
        OldVal + '   - nothing done ';
      Logdatei.Log(LogS, LLwarning);
    end;
  end;
end;

procedure TuibPatchIniFile.delEntry(const Sektion, Eintrag: string);
 (* Loescht einen Eintrag,
    Wenn kompletter Eintrag
    Ident = Value
    in Sektion nicht gefunden wird,
    wird angenommen (und ueberprueft), dass Eintrag die Form
    Ident
    hat.
    Der erste Eintrag mit dem Schluesselnamen Ident in Sektion wird
    dann entfernt
     *)
var
  i: integer = 0;
begin
  LogSCommand := 'delEntry ' + '[' + Sektion + '] ' + Eintrag;
  Logged := Logdatei.Log(LogSCommand, LLinfo);

  IdentAbspalten(Eintrag, Ident, Value);
  if FindSectionheaderIndex(Sektion) = -1 then
  begin
    if not Logged then
      Logged := Logdatei.Log(LogSCommand, LLinfo);

    LogS := 'Warning:  section ' + Sektion + ' not found - nothing to do';
    Logdatei.Log(LogS, LLwarning);
  end
  else
  begin
    i := FindEntryIndex(Sektion, Ident, Value);
    if i >= 0 then
    begin
      Delete(i);

      LogS := '  done';
      Logdatei.Log(LogS, LLinfo);
    end
    else if Eintrag = Ident then
    begin
      i := FindIdentIndex(Sektion, Ident);
      if i >= 0 then
      begin
        LogS := Strings[i];
        Delete(i);

        LogS := '  in section ' + Sektion + ' deleted  ' + LogS;
        Logdatei.Log(LogS, LLinfo);
      end;
    end
    else
    begin
      if not Logged then
        Logged := Logdatei.Log(LogS, LLinfo);

      LogS := 'found neither entry ' + Eintrag + 'in section ' +
        Sektion + ' not found';
      Logdatei.Log(LogS, LLinfo);

      LogS := '           nor  ident ' + Eintrag + 'in section ' +
        Sektion + ' not found';
      Logdatei.Log(LogS, LLwarning);
    end;
  end;

end;

procedure TuibPatchIniFile.setEntry(const Sektion, Eintrag: string);
(* Setzt Eintrag Ident = Value in Sektion
  Falls Sektion oder Eintrag nicht existiert wird er erstellt
  Falls der Eintrag mit anderem Wert nach dem Gleichheitszeichen
  vorhanden ist,
  Ident = x
  wird er
  - beim ersten Vorkommen von Ident! -
  umgesetzt auf
  Ident = Value
*)
var
  i: integer = 0;
begin
  LogSCommand := 'setEntry ' + '[' + Sektion + '] ' + Eintrag;
  Logged := Logdatei.Log(LogSCommand, LLinfo);

  IdentAbspalten(Eintrag, Ident, Value);
  if FindSectionheaderIndex(Sektion) = -1 then
  begin
    addSection(Sektion);
  end;

  i := FindIdentIndex(Sektion, Ident);
  if i = -1 then
  begin
    i := FindSectionheaderIndex(Sektion);
    i := FindEndOfSectionIndex(i);
    Insert(i + 1, Eintrag);

    LogS := '  in section ' + Sektion + ' appended entry ' + Eintrag;
    Logdatei.Log(LogS, LLinfo);
  end
  else
  begin
    LogS := Strings[i];

    Delete(i);
    insert(i, Eintrag);

    if LogS = Eintrag then
    begin
      LogS := '  Entry not changed';
      Logdatei.Log(LogS, LLinfo);
    end
    else
    begin
      LogS := '  Entry      ' + LogS;
      Logdatei.Log(LogS, LLinfo);
      LogS := '  changed to ' + Eintrag;
      Logdatei.Log(LogS, LLinfo);
    end;
  end;

end;


procedure TuibPatchIniFile.addNewEntry(const Sektion, Eintrag: string);
(*  Fuegt einen Eintrag in eine Sektion ein, falls er noch nicht existiert
    Falls der Sektion noch nicht da ist wird er erstellt.
*)

var
  i: integer = 0;
begin
  LogSCommand := 'addNewEntry ' + '[' + Sektion + '] ' + Eintrag;
  Logged := Logdatei.Log(LogSCommand, LLinfo);

  IdentAbspalten(Eintrag, Ident, Value);

  if FindSectionheaderIndex(Sektion) = -1 then
  begin
    addSection(Sektion);
  end;

  i := FindEntryIndex(Sektion, Ident, Value);

  if i = -1 then
  begin
    i := FindSectionheaderIndex(Sektion);
    i := FindEndOfSectionIndex(i);
    Insert(i + 1, Eintrag);

    LogS := '  appended entry';
    Logdatei.Log(LogS, LLinfo);
  end
  else
  begin
    if not Logged then
      Logged := Logdatei.Log(LogSCommand, LLinfo);

    LogS := 'Warning:  entry ' + Eintrag + ' existed already in ' +
      Sektion + ' - nothing to do';
    Logdatei.Log(LogS, LLwarning);
  end;

end;

procedure TuibPatchIniFile.delSec(const Sektion: string);
var
  i, j, n: integer;
begin
  LogSCommand := 'delSec' + '[' + Sektion + '] ';
  Logged := Logdatei.Log(LogSCommand, LLinfo);

  i := FindSectionheaderIndex(Sektion);
  if i >= 0 then
  begin
    n := FindEndOfSectionIndex(i);
    for  j := i to n do
    begin
      LogS := Strings[i];
      Delete(i);

      LogS := '  deleted ' + LogS;
      Logdatei.Log(LogS, LLinfo);
    end;

    (* anschliessende Leerzeilen auch loeschen *)

    while (i + 1 <= Count) and (Strings[i] = '') do
      Delete(i);

  end
  else
  begin
    if not Logged then
      Logged := Logdatei.Log(LogSCommand, LLinfo);

    LogS := 'Warning:  section ' + Sektion + ' not found - nothing to do';
    Logdatei.Log(LogS, LLwarning);

  end;
end;

procedure TuibPatchIniFile.changeEntry(const Sektion, Eintrag: string);
(* Aendert einen Eintrag nur, wenn er bis zum = vorhanden ist *)
var
  Ident: string = '';
  Value: string = '';
  i: integer = 0;
begin
  LogSCommand := 'changeEntry ' + '[' + Sektion + '] ' + Eintrag;
  Logged := Logdatei.Log(LogSCommand, LLinfo);

  IdentAbspalten(Eintrag, Ident, Value);
  i := FindIdentIndex(Sektion, Ident);

  if i >= 0 then
  begin
    if Strings[i] <> Eintrag then
    begin
      LogS := Strings[i];

      Delete(i);
      insert(i, Eintrag);

      LogS := '  entry      ' + LogS;
      Logdatei.Log(LogS, LLinfo);
      LogS := '  changed to ' + Eintrag;
      Logdatei.Log(LogS, LLinfo);
    end
    else
    begin
      if not Logged then
        Logged := Logdatei.Log(LogSCommand, LLinfo);

      LogS := 'Warning:  entry ' + Eintrag + ' exists - nothing to do';
      Logdatei.Log(LogS, LLwarning);

    end;

  end
  else
  begin
    if not Logged then
      Logged := Logdatei.Log(LogSCommand, LLinfo);

    LogS := 'Warning:  ident ' + Ident + ' in section ' + Sektion +
      ' not found - nothing to do';
    Logdatei.Log(LogS, LLwarning);
  end;
end;

procedure TuibPatchIniFile.replaceEntry(const AlterEintrag, Eintrag: string);
var
  Ident0: string = '';
  Value0: string = '';
  i: integer = 0;
begin
  LogSCommand := 'replaceEntry' + AlterEintrag + ' ' + Eintrag;
  Logged := Logdatei.Log(LogSCommand, LLinfo);

  IdentAbspalten(AlterEintrag, Ident, Value);
  for i := 1 to Count do
  begin
    IdentAbspalten(Strings[i - 1], Ident0, Value0);
    if (UpperCase(Ident0) = UpperCase(Ident)) and
      (UpperCase(Value0) = UpperCase(Value)) then
    begin
      Delete(i - 1);
      insert(i - 1, Eintrag);

      LogS := '  replaced in line ' + IntToStr(i + 1);
      Logdatei.Log(LogS, LLinfo);
    end;
  end;

end;

{
procedure TuibPatchIniFile.addProgManGroup (const NeueGruppe: String);
var
  i, GruppenNr, LetzteGruppeLine : Integer;
  GroupFound : Boolean;
  s, Gruppe, Section : String;
begin
  section := 'Groups';

  LogSCommand := 'addProgManGroup' + '[' + section + '] ' + NeueGruppe;
  Logged :=  Logdatei.Log (LogSCommand, LLinfo);

  i := FindSectionheaderIndex (section);
  LetzteGruppeLine := FindEndOfSectionIndex (i);

  GroupFound := false;
  Gruppennr := 1;

  inc (i); (* zur ersten Zeile der Sektion *)

  while not GroupFound and (i <= LetzteGruppeLine)
  do
  Begin
    IdentAbspalten (Strings [i], Ident, Gruppe);

    if upperCase (Gruppe) = upperCase (KappeBlanks (NeueGruppe))
    then
      GroupFound := true
    else
    Begin
      inc (i);
      inc (Gruppennr);
    end;

  end;

  if GroupFound
  then
  begin
    if not Logged then
      Logged := Logdatei.Log (LogSCommand, LLinfo);

    LogS :=
            'Warning:  groupfile ' + NeueGruppe + ' already contained as ' + 'group' + IntToStr (Gruppennr);
    Logdatei.Log (LogS, LLinfo);

    LogDatei.NumberOfWarnings := LogDatei.NumberOfWarnings + 1;
  end
  else
  Begin
    s:= 'Group' + IntToStr (Gruppennr) + '=' + NeueGruppe;
    addEntry (section, s);

    (* neue Gruppennr als letzte einfuegen in Settings-Section *)
    section := 'Settings';
    Ident := 'Order';
    i := FindIdentIndex (section, Ident);
    s := Strings [i] + ' ' + IntToStr (Gruppennr);
    delete (i);
    insert (i, s);

    LogS := '  done';
    Logdatei.Log (LogS, LLinfo);
  end;
end;



procedure TuibPatchIniFile.delProgManGroup (const AlteGruppe: String);

var
  i, k, GruppenNr, LetzteGruppeLine : Integer;
  GroupFound : Boolean;
  s, Gruppe, Zahl, Rest : String;
  ZahlNumerisch : Integer;
  section : String;

begin
  section := 'Groups';

  LogSCommand := 'delProgManGroup' + '[' + section + '] ' + AlteGruppe;
  Logged :=  Logdatei.Log (LogSCommand, LLinfo);


  i := FindSectionheaderIndex (section);
  LetzteGruppeLine := FindEndOfSectionIndex (i);

  GroupFound := false;
  Gruppennr := 1;

  inc (i); (* zur ersten Zeile der Sektion *)

  while not GroupFound and (i <= LetzteGruppeLine)
  do
  Begin
    IdentAbspalten (Strings [i], Ident, Gruppe);

    if upperCase (Gruppe) = upperCase (KappeBlanks (AlteGruppe))
    then
      GroupFound := true
    else
    Begin
      inc (i);
      inc (Gruppennr);
    end;

  end;

  if not GroupFound
  then
  begin
    if not Logged then
      Logged := Logdatei.Log (LogSCommand, LLinfo);

    LogS := 'Warning:  groupfile ' + AlteGruppe + ' not found';;
    Logdatei.Log (LogS, LLinfo);

    LogDatei.NumberOfWarnings := LogDatei.NumberOfWarnings + 1;
  end
  else
    (* i ist der Index des Eintrags von AlteGruppe,
       Gruppennr enthaelt die entsprechende Nummer *)

  Begin

    (* Gruppeneintrag loeschen; die Eintraege mit hoeherer
       Nummer um 1 zurueckstufen  *)


    delete (i);
    dec (LetzteGruppeLine);

    k := Gruppennr;

    while i <= LetzteGruppeLine
    do
    Begin
      IdentAbspalten (Strings [i], Ident, Value);
      s := 'Group' + IntToStr (k) + '=' + Value;
      delete (i);
      insert (i, s);
      inc (i);
      inc (k);
    end;

    (* Gruppennr elimieren aus Orderliste in Setting-Section *)

    section := 'Settings';
    Ident := 'Order';
    i := FindIdentIndex (section, Ident);
    IdentAbspalten (Strings [i], Ident, Value);

    s := Ident + '=';

    WortAbspalten (Value, Zahl, Rest);

    while length (Zahl) > 0
    do
    Begin
      ZahlNumerisch := StrToInt (Zahl);
      if ZahlNumerisch <> Gruppennr
      then
      Begin
        if ZahlNumerisch > Gruppennr
        then ZahlNumerisch := ZahlNumerisch - 1;
        s := s + ' ' + IntToStr (ZahlNumerisch);
      End;
      WortAbspalten (Rest, Zahl, Rest);
    End;

    delete (i);
    insert (i, s);


    LogS := '  done';
    Logdatei.Log (LogS, LLinfo);
  end;
End;
}



(* TuibIniFile *)
constructor TuibIniFile.Create(inifilename: string);
begin
  inherited Create;
  saved := True;

  try
    inifilename := ExpandFileName(inifilename);
    LoadFromFile(inifilename);
    //Text := reencode(Text, 'system');
    filename := inifilename;
  except
  end;

end;

destructor TuibIniFile.Destroy;
begin
  inherited Destroy;
end;

procedure TuibIniFile.ReadSection(const Sectionname: string;
  var SectionVars: TStringList);
var
  i: integer = 0;
  startlineno: integer = 0;
  resultList: TXStringList;

begin
  resultList := TXStringList.Create;
  resultList.Clear;
  GetSectionLines(Sectionname, resultList, startlineno, False, False, True);
  for i := 0 to resultList.Count - 1 do
    resultlist[i] := trim(copy(resultlist[i], 1, pos('=', resultlist[i]) - 1));
  SectionVars.Assign(resultlist);
  resultList.Free;
end;

procedure TuibIniFile.ReadSectionValues(const Sectionname: string;
  var SectionValues: TStringList);
var
  startlineno: integer = 0;
  resultList: TXStringList;
  line: string = '';
  posSeparator: integer = 0;
  i: integer = 0;

begin
  resultList := TXStringList.Create;
  resultList.Clear;

  GetSectionLines(Sectionname, resultlist, startlineno, False, False, True);
  for i := 0 to resultList.Count - 1 do
  begin
    line := resultlist[i];
    setLength(line, length(line));
    posSeparator := pos('=', line);

    resultlist[i] := trim(copy(line, 1, posSeparator - 1)) + '=' +
      trim(copy(line, posSeparator + 1, length(line)));
  end;
  SectionValues.Assign(resultList);

  // test:
  //centralform.Memo1.Lines.AddStrings  (SectionValues);

  resultList.Free;
end;

procedure TuibIniFile.ReadRawSection(const SectionName: string;
  var RawSection: TXStringList);
var
  (*
  i: integer = 0;
  s: string = '';
  sectionFound : string ='';
  nextLine : string = '';
  comp: integer =2;
  *)
  startlineno: integer = 0;
  resultList: TXStringList;
begin
  resultList := TXStringList.Create;
  resultList.Clear;
  (*
  i:= 0;
  repeat
     s := trim(Strings[i]);
     i:=i+1;
     if (length(s) > 1) and (s[1] = '[') and (s[length(s)] = ']') then
        sectionFound := copy(s, 2, length(s) - 2);
  until ((CompareStr(SectionName,sectionFound)=0) OR (i=Count));

  repeat
     s := Strings[i];
     if CompareStr(s,'')<>0 then
        resultList.Add(s);
     i:=i+1;
     if CompareStr(Strings[i],'')<>0 then
     begin
       nextLine:= Strings[i];
       comp := CompareStr(nextLine[1],'[');
     end;
  until ((comp=0) OR (i=Count)) ;
  *)
  GetSectionLines(Sectionname, resultlist, startlineno, False, False, False);
  RawSection.Assign(resultList);
  resultList.Free;
end;

function TuibIniFile.ReadString(const Section, Ident, defaultvalue: string): string;
var
  LogSCommand: string = '';
  found: boolean;
  theSection: TXStringList;
  i, startlineno: integer;
begin
  //if LogDatei <> nil
  //then
  begin
    LogSCommand := 'ReadString ' + '[' + Section + '] ' + Ident;
    Logged := Logdatei.Log(LogSCommand, LLinfo);
  end;

  found := True;

  if FindSectionheaderIndex(Section) = -1 then
  begin
    //if LogDatei <> nil
    //then
    begin

      Logdatei.Log('Section not found', LLinfo);

    end;
    found := False;
  end
  else
  begin
    startlineno := FindIdentIndex(Section, Ident);
    if startlineno = -1 then
    begin
      //if LogDatei <> nil
      //then
      begin
        Logdatei.Log('Variable ' + ident + ' not found', LLinfo);
      end;
      found := False;
    end
    else
    begin
      TheSection := txStringlist.Create;
      GetSectionLines(Section, TheSection, i, False, False, True);
      Result := TheSection.Values[ident];
      //if LogDatei <> nil
      //then
      begin
        Logdatei.Log('resulting ' + Result, LLinfo);
      end;
    end;
  end;

  if not found then
  begin
    //if LogDatei <> nil
    //then
    begin
      Logdatei.Log('taking default ' + defaultvalue, LLinfo);
    end;
    Result := DefaultValue;
  end;

end;

procedure TuibIniFile.WriteString(const Section, Ident, Value: string);
begin
  setEntry(section, Ident + '=' + Value);
  saved := False;
end;

procedure TuibIniFile.saveBack;
var
  //i: integer;
  //outfile: TextFile;
  errormessage: string = '';
  erroroccured: boolean;
  handle: thandle = 0;
begin

  // test of permissions of file filename
  Handle := fileopen(filename, fmOpenWrite + fmShareDenyNone);
  if Handle <= 0 then
  begin
    errorOccured := True;
    Logdatei.Log('IO-Error ---- ', LLinfo);
    errormessage := 'file  could not be opened for writing, error code ' +
      IntToStr(-handle) + ' ' + RemoveLineBreaks(SysErrorMessage(-handle));
    Logdatei.Log('', LLinfo);
    LogDatei.log
    (filename + ' could not be saved back ' + errormessage, LLError);
    Logdatei.Log('-------------- ', LLinfo);
    Logdatei.Log('', LLinfo);
  end
  else
    Logdatei.Log('File ' + filename +
      ' can be opened for writing, save it back', LLinfo);

  fileclose(handle);
  saveToFile(filename);

end;


(* TuibPatchHostsFile  *)

constructor TuibPatchHostsFile.Create;
begin
  inherited Create;
  LineIdents := TStringList.Create;
  UpperLineIdents := TStringList.Create;
end;

destructor TuibPatchHostsFile.Destroy;
begin
  LineIdents.Free;
  UpperLineIdents.Free;
  inherited Destroy;
end;


function TuibPatchHostsFile.FindAndSplitLine
  (const SearchIdent: string; var ipAddress, Hostname, Alias, comment: string): integer;

 (* Looks if Searchstr exists in a hostline before the commentarea.
    If it does not exist the function  returns -1; the result variables remain empty.
    If it is found the function returns the corresponding line index; the result variables
    contain the 4 components of the line *)

var
  j: integer = 0;
  i: integer = 0;
  found: boolean;
  Remaining: string = '';
  ident: string = '';

begin
  i := 1;
  Comment := '';
  LineIdents.Clear;
  UpperLineIdents.Clear;

  found := False;

  (* Zeilen durchgehen *)
  while (i <= Count) and not found do
  begin
    Remaining := CutLeftBlanks(Strings[i - 1]);
    LineIdents.Clear;
    UpperLineIdents.Clear;

    j := pos('#', Remaining);
    if j > 0 then
    begin
      Comment := copy(Remaining, j, length(Remaining) - j + 1);
      Remaining := copy(Remaining, 1, j - 1);
    end;

    GetWord(Remaining, ipAddress, Remaining, WordDelimiterWhiteSpace);
    GetWord(Remaining, Hostname, Remaining, WordDelimiterWhiteSpace);
    Alias := Remaining;

    LineIdents.Add(ipAddress);
    LineIdents.Add(Hostname);
    UpperLineIdents.Add(upperCase(ipAddress));
    UpperLineIdents.Add(upperCase(Hostname));
    while length(Remaining) > 0 do
    begin
      GetWord(Remaining, Ident, Remaining, WordDelimiterWhiteSpace);
      UpperLineIdents.Add(upperCase(Ident));
      LineIdents.Add(Ident);
    end;

    if UpperLineIdents.IndexOf(upperCase(SearchIdent)) >= 0 then
    begin
      found := True;
      Result := i - 1;
    end
    else
    begin
      Comment := '';
    end;

    Inc(i);
  end;

  if not found then
  begin
    Result := -1;
    Alias := '';
    ipAddress := '';
    Hostname := '';
  end;
end;


procedure TuibPatchHostsFile.MakeLine(i: integer; s: string);
var
  lastLine: string = '';
  llBeforeComment: string = '';
  llComment: string = '';
  SimplyAddString: boolean;
begin
  if i = -1 then
    (* a new line has to be created *)
  begin
    SimplyAddString := False;

    if Count = 0 then
      SimplyAddString := True
    else
    begin
      (* is the file finishing with a comment line? *)
      if (CutLeftBlanks(Strings[Count - 1]) = '') then
        Delete(Count - 1);
      (* delete empty last line and look at the last line now being there *)

      if Count = 0 then
        SimplyAddString := True
      else
      begin
        LastLine := Strings[Count - 1];
        GetWord(LastLine, llBeforeComment, llComment, ['#']);
        if CutLeftBlanks(LastLine) = llComment then
          (* let it stay at the end *)
          Insert(Count - 1, CutRightBlanks(s))
        else
          SimplyAddString := True;
      end;
    end;

    if SimplyAddString then
      Add(CutRightBlanks(s));
  end

  else
    Strings[i] := CutRightBlanks(s);

end;


procedure TuibPatchHostsFile.SetAddress(const Hostname, ipAddress: string);
  (* setzt die IP-Adresse fuer den Host  <hostname> auf  <ipadresse>.
     Falls noch kein Eintrag fuer den Host <hostname> besteht,
     wird er neu eingetragen *)
var
  hn: string = '';
  addr: string = '';
  ali: string = '';
  comment: string = '';
  i: integer = 0;
begin
  i := FindAndSplitLine(Hostname, addr, hn, ali, comment);

  MakeLine(i, ipAddress + #9 + Hostname + #9 + ali + #9 + comment);

  if i >= 0 then
    LogS := 'Set ipAddress for Hostname "' + Hostname + '" from ' +
      addr + ' to ' + ipAddress
  else
    LogS := 'Set ipAddress ' + ipAddress + ' Hostname "' + Hostname + '"';

  Logdatei.Log(LogS, LLinfo);
end;

procedure TuibPatchHostsFile.SetName(const ipAddress, Hostname: string);
  (* setzt den Namen des Hosts mit der angegebenen IP-Adresse  <ipadresse> auf <hostname>.
     Falls noch kein Eintrag mit der IP-Adresse <ipadresse> existiert,
     wird er neu erzeugt. *)
var
  hn: string = '';
  addr: string = '';
  ali: string = '';
  comment: string = '';
  i: integer = 0;
begin
  i := FindAndSplitLine(ipAddress, addr, hn, ali, comment);
  MakeLine(i, ipAddress + #9 + Hostname + #9 + ali + #9 + comment);

  LogS := '';
  if i >= 0 then
    LogS := LogS + 'Set Hostname of ' + ipAddress + ' from "' + hn +
      '" to "' + Hostname + '"'
  else
    LogS := LogS + 'Set Hostname "' + Hostname + '" for ipAddress ' + ipAddress;

  Logdatei.Log(LogS, LLinfo);
end;

procedure TuibPatchHostsFile.SetAlias(const Ident, Alias: string);
   (* fuegt fuer den Host, identifiziert durch <Ident>, wobei
      <Ident> entweder als Name (oder Alias) oder als ipAdresse interpretiert
      werden kann, den ALIAS-Namen  <Alias> ein. *)
var
  hn: string = '';
  addr: string = '';
  ali: string = '';
  comment: string = '';
  i: integer = 0;
begin
  i := FindAndSplitLine(Ident, addr, hn, ali, comment);
  if i = -1 then
  begin
    LogS := 'Error:  Alias "' + Alias + '" not inserted  -  entry "' +
      Ident + '" does not exist';
    LogDatei.log(LogS, LLError);
    exit;
  end;

  if ali = '' then
    ali := Alias
  else
  begin
    (* kein zweites Einfuegen eines Alias *)
    if UpperLineIdents.IndexOf(UpperCase(Alias)) >= 0 then
      (* Alias schon enthalten in ali *)
    begin
      LogS := 'Alias "' + Alias + '" for entry "' + Ident + '" exists already';
      Logdatei.Log(LogS, LLinfo);
      exit;
    end
    else
      ali := ali + #9 + Alias;
  end;

  MakeLine(i, addr + #9 + hn + #9 + ali + #9 + comment);

  LogS := 'Alias "' + Alias + '" set for entry "' + Ident + '"';
  Logdatei.Log(LogS, LLinfo);
end;

procedure TuibPatchHostsFile.DelAlias(const Ident, Alias: string);
    (*  loescht aus dem Eintrag fuer den Host,
    gegeben durch IP-Adresse, Hostname oder Alias <Ident>
    den Alias <Alias> *)
var
  hn: string = '';
  addr: string = '';
  ali: string = '';
  comment: string = '';
  i: integer = 0;
  j: integer = 0;
  AliasIndex: integer = 0;
begin
  i := FindAndSplitLine(Ident, addr, hn, ali, comment);
  if i = -1 then
  begin
    LogS := 'Info: Host "' + Ident + '" does not exist';
    Logdatei.Log(LogS, LLinfo);
  end
  else
  begin
    AliasIndex := UpperLineIdents.IndexOf(upperCase(Alias));

    if AliasIndex < 0 then
    begin
      LogS := 'Info: Alias "' + Alias + '" for host "' + Ident + '" does not exist';
      Logdatei.Log(LogS, LLinfo);
    end
    else if AliasIndex < 2 then
    begin
      LogS := 'Warning: "' + Alias +
        '" is not an alias, but the hostname and will not be deleted';
      Logdatei.Log(LogS, LLwarning);
    end
    else
    begin
      ali := '';
      for j := 3 to LineIdents.Count do
        if j - 1 <> AliasIndex then
          ali := ali + #9 + LineIdents.Strings[j - 1];

      MakeLine(i, addr + #9 + hn + #9 + ali + #9 + comment);

      LogS := 'Alias "' + Alias + '" for entry "' + Ident + '" deleted ';
      Logdatei.Log(LogS, LLinfo);
    end;
  end;
end;

procedure TuibPatchHostsFile.DelHost(const Ident: string);
(*  loescht den Eintrag des mit IP-Namen, IP-Adresse oder Alias gegebenen Hosts *)
var
  hn: string = '';
  addr: string = '';
  ali: string = '';
  comment: string = '';
  i: integer = 0;
begin
  i := FindAndSplitLine(Ident, addr, hn, ali, comment);
  if i = -1 then
  begin
    LogS := 'Info: Host "' + Ident + '" not found ';
    Logdatei.Log(LogS, LLinfo);
    exit;
  end;

  Delete(i);
  LogS := 'Entry "' + Ident + '" deleted';
  Logdatei.Log(LogS, LLinfo);
end;

procedure TuibPatchHostsFile.SetComment(const Ident, Comment: string);
(*  setzt den Kommentar zum Eintrag des mit IP-Namen, IP-Adresse oder Alias gegebenen Hosts neu *)
var
  hn: string = '';
  addr: string = '';
  ali: string = '';
  comm: string = '';
  i: integer = 0;
begin
  i := FindAndSplitLine(Ident, addr, hn, ali, comm);
  if i = -1 then
  begin
    LogS := 'Info: Host "' + Ident + '" not found ';
    Logdatei.Log(LogS, LLinfo);
    exit;
  end;

  MakeLine(i, addr + #9 + hn + #9 + CutRightBlanks(ali) + #9 + '# ' + Comment);
  LogS := 'SetComment of Host "' + Ident + '" to "' + Comment + '"';
  Logdatei.Log(LogS, LLinfo);

end;

function TuibPatchHostsFile.GetAddress(const Ident: string;
  var ipAddress: string): boolean;
  (* get address for Hostname or Alias <Ident> *)
var
  hn: string = '';
  addr: string = '';
  ali: string = '';
  comment: string = '';
  i: integer = 0;
begin
  i := FindAndSplitLine(Ident, ipAddress, hn, ali, comment);
  if i >= 0 then
  begin
    Result := True;
    LogS := 'Found ipAddress ' + ipAddress + ' for Host "' + Ident + '"';
    Logdatei.Log(LogS, LLinfo);
  end
  else
  begin
    Result := False;
    LogS := 'Warning: ipAddress for Host "' + Ident + '"  not found';
    Logdatei.Log(LogS, LLwarning);
  end;
end;

function TuibPatchHostsFile.GetHostname(const ipAddress: string;
  var Hostname: string): boolean;
  (* get Hostname for ipAddress (or Alias) *)

var
  hn: string = '';
  addr: string = '';
  ali: string = '';
  comment: string = '';
  i: integer = 0;
begin
  i := FindAndSplitLine(ipAddress, addr, Hostname, ali, comment);
  if i >= 0 then
  begin
    Result := True;
    LogS := 'Found Hostname "' + Hostname + '" for ipAddress ' + ipAddress;
    Logdatei.Log(LogS, LLinfo);
  end
  else
  begin
    Result := False;
    LogS := 'Warning: Name for ipAddress ' + ipAddress + ' not found';
    Logdatei.Log(LogS, LLwarning);
  end;

end;




(* TuibFileInstall *)

constructor TuibFileInstall.Create;
begin
  copyDenyList := TStringList.Create;
  copyDenyList.CaseSensitive := False;
  copyDenyList.Add('.ds_store');
  inherited Create;
end;

destructor TuibFileInstall.Destroy;
begin
  FreeAndNil(copyDenyList);
  inherited Destroy;
end;


function TuibFileInstall.HardLink(existingfilename, newfilename: string): boolean;
var
  exitcode: integer;
  exist, new: PChar;
  {$IFDEF WINDOWS}
  exitbool: winbool;
{$ENDIF WINDOWS}
  deleted: boolean;
  maxretry, retries: integer;
  ActionInfo: string;
begin
  Result := False;

  exist := StrAlloc(length(existingfilename) + 1);
  new := StrAlloc(length(newfilename) + 1);
  exist := strpcopy(exist, existingfilename);
  new := strpcopy(new, newfilename);
  if FileExistsUTF8(newfilename) then
  begin
    if FileGetWriteAccess(newfilename, ActionInfo) then
      if ActionInfo <> '' then
        LogDatei.log(ActionInfo, LLInfo);
    maxretry := 3;
    retries := 0;
    repeat
      deleted := SysUtils.DeleteFile(newfilename);
      if deleted then
        LogDatei.log('Existing file ' + new + ' was deleted', LLInfo)
      else
        LogDatei.log('Existing file ' + new + ' could not be  deleted,  Error: ' +
          removeLineBreaks(SysErrorMessage(GetLastOSError)), LLError);
      Inc(retries);
      Sleep(100);
      ProcessMess;
    until deleted or (retries > maxretry);
  end;
  {$IFDEF UNIX}
  exitcode := fplink(exist, new);
  if 0 = exitcode then
    Result := True
  else
  begin
    Result := False;
    LogDatei.log('Could not create hard link from ' + exist + ' to ' +
      new + ' Error: ' + SysErrorMessage(fpgeterrno), LLerror);
  end;
  {$ENDIF LINUX}
  {$IFDEF WIN32}
  exitbool := winCreateHardLink(new, exist, nil);
  if exitbool then
    Result := True
  else
  begin
    Result := False;
    LogDatei.log('Could not create hard link from ' + exist + ' to ' +
      new + ' Error: ' + removeLineBreaks(SysErrorMessage(GetLastError)) +
      ' (' + SysErrorMessage(GetLastError) + ')', LLerror);
  end;
  {$ENDIF WIN32}
  StrDispose(exist);
  StrDispose(new);

end;

function TuibFileInstall.SymLink(existingfilename, newfilename: string): boolean;
var
  exitcode: integer;
  exist, new: PChar;
  existisdir: dword;
  {$IFDEF WINDOWS}
  exitbool: winbool;
{$ENDIF WINDOWS}
  ActionInfo: string;
  list1: TStringList;
begin
  Result := False;
  //exist:=StrAlloc (length(existingfilename)+1);
  //new:=StrAlloc (length(newfilename)+1);
  exist := PChar(existingfilename);
  new := PChar(newfilename);
  if FileExists(newfilename) then
  begin
    if FileGetWriteAccess(newfilename, ActionInfo) then
      if ActionInfo <> '' then
        LogDatei.log(ActionInfo, LLInfo);
    if SysUtils.DeleteFile(newfilename) then
      LogDatei.log('Existing file ' + new + ' was deleted', LLInfo)
    else
      LogDatei.log('Existing file ' + new + ' could not be  deleted,  Error: ' +
        removeLineBreaks(SysErrorMessage(GetLastOSError)), LLError);
  end;
  {$IFDEF UNIX}
  exitcode := fpsymlink(exist, new);
  if 0 = exitcode then
    Result := True
  else
  begin
    Result := False;
    LogDatei.log('Could not create sym link from ' + exist + ' to ' +
      new + ' Error: ' + SysErrorMessage(fpgeterrno), LLerror);
  end;
  {$ENDIF LINUX}
  {$IFDEF WIN32}
  if DirectoryExistsUTF8(existingfilename) then
    existisdir := 1
  else
    existisdir := 0;
  exitbool := winCreateSymbolicLink(new, exist, existisdir);
  if exitbool then
    Result := True
  else
  begin
    Result := False;
    LogDatei.log('Could not create sym link from ' + exist + ' to ' +
      new + ' Error: ' + removeLineBreaks(SysErrorMessage(GetLastError)) +
      ' (' + SysErrorMessage(GetLastError) + ')', LLerror);
  end;
  if Result then
  begin
    LogDatei.log('Reading symlink via dir to reread the cache', LLInfo);
    (*
  list1 := Tstringlist.create;
  list1.Clear;
  list1.Text := execShellCall('dir '+newfilename, '32',4, false,false).Text;
  //calling shellCall with FetchExitCodePublic=false result is on FLastPrivateExitCode
  if (0 = FLastPrivateExitCode) then Result := true
  else result := false;
  list1.Free;
  end;
  *)
  end;
  {$ENDIF WIN32}
  //StrDispose(exist);
  //StrDispose(new);
end;

function TuibFileInstall.Rename(existingfilename, newfilename: string;
  var RebootWanted: boolean): boolean;
var
  exitcode: integer;
  {$IFDEF WINDOWS}
  exist, new: PWchar;
  {$ELSE WINDOWS}
  exist, new: PChar;
  {$ENDIF WINDOWS}
  moveflags: DWORD;
  {$IFDEF WINDOWS}
  exitbool: winbool;
{$ENDIF WINDOWS}
begin
  Result := False;
  //exist:=StrAlloc (length(existingfilename)+1);
  //new:=StrAlloc (length(newfilename)+1);

  {$IFDEF UNIX}
  exist := PChar(existingfilename);
  new := PChar(newfilename);
  exitcode := fprename(exist, new);
  if 0 = exitcode then
    Result := True
  else
  begin
    Result := False;
    LogDatei.log('Could not move / rename from ' + exist + ' to ' +
      new + ' Error: ' + SysErrorMessage(fpgeterrno), LLerror);
  end;
  {$ENDIF LINUX}
  {$IFDEF WINDOWS}
  exist := PWChar(unicodestring(existingfilename));
  new := PWChar(unicodestring(newfilename));
  // perhaps also MOVEFILE_WRITE_THROUGH
  moveflags := MOVEFILE_COPY_ALLOWED or MOVEFILE_REPLACE_EXISTING;
  exitbool := MoveFileExW(exist, new, moveflags);
  if exitbool then
    Result := True
  else
  begin
    if GetLastError = 32 then
    begin
      LogDatei.log('Target file was in use, retry with DELAY_UNTIL_REBOOT.', LLDebug2);
      moveflags := MOVEFILE_DELAY_UNTIL_REBOOT or MOVEFILE_REPLACE_EXISTING;
      exitbool := MoveFileExW(exist, new, moveflags);
      if exitbool then
      begin
        Result := True;
        RebootWanted := True;
        LogDatei.log(
          'Target file was in use, move / rename should be completed after reboot.',
          LLInfo);
      end
      else
        LogDatei.log('Target file was in use, retry with DELAY_UNTIL_REBOOT failed.',
          LLError);
    end;
    if not Result then
    begin
      LogDatei.log('Could not rename / move from ' + exist + ' to ' +
        new + ' Error: ' + IntToStr(GetLastError) + ' (' +
        removeLineBreaks(SysErrorMessage(GetLastError)) + ')', LLerror);
    end;
  end;
  {$ENDIF WINDOWS}
  //StrDispose(exist);
  //StrDispose(new);
end;

(*
function TuibFileInstall.RenameDir(existingdirname, newdirname: string;
  var RebootWanted: boolean): boolean;
var
  exitcode: integer;
  {$IFDEF WINDOWS}
  exist, new: PWchar;
  {$ELSE WINDOWS}
  exist, new: PChar;
  {$ENDIF WINDOWS}
  moveflags: DWORD;
  {$IFDEF WINDOWS}
  exitbool: winbool;
{$ENDIF WINDOWS}
begin
  Result := False;
  //exist:=StrAlloc (length(existingfilename)+1);
  //new:=StrAlloc (length(newfilename)+1);
      function RenameDir(OldName, NewName: string): boolean;
    begin
      // DirectorySeparator at the end of directories avoids some troubles
      OldName:= IncludeTrailingPathDelimiter(OldName);
      NewName:= IncludeTrailingPathDelimiter(NewName);
      // Let's go
      Result := RenameFile(OldName, NewName);
    end;

  {$IFDEF UNIX}
  exist := PChar(existingfilename);
  new := PChar(newfilename);
  exitcode := fprename(exist, new);
  if 0 = exitcode then
    Result := True
  else
  begin
    Result := False;
    LogDatei.log('Could not move / rename from ' + exist + ' to ' +
      new + ' Error: ' + SysErrorMessage(fpgeterrno), LLerror);
  end;
  {$ENDIF LINUX}
  {$IFDEF WINDOWS}
  exist := PWChar(unicodestring(existingfilename));
  new := PWChar(unicodestring(newfilename));
  // perhaps also MOVEFILE_WRITE_THROUGH
  moveflags := MOVEFILE_COPY_ALLOWED or MOVEFILE_REPLACE_EXISTING;
  exitbool := MoveFileExW(exist, new, moveflags);
  if exitbool then
    Result := True
  else
  begin
    if GetLastError = 32 then
    begin
      LogDatei.log('Target file was in use, retry with DELAY_UNTIL_REBOOT.', LLDebug2);
      moveflags := MOVEFILE_DELAY_UNTIL_REBOOT or MOVEFILE_REPLACE_EXISTING;
      exitbool := MoveFileExW(exist, new, moveflags);
      if exitbool then
      begin
        Result := True;
        RebootWanted := True;
        LogDatei.log(
          'Target file was in use, move / rename should be completed after reboot.',
          LLInfo);
      end
      else
        LogDatei.log('Target file was in use, retry with DELAY_UNTIL_REBOOT failed.',
          LLError);
    end;
    if not Result then
    begin
      LogDatei.log('Could not rename / move from ' + exist + ' to ' +
        new + ' Error: ' + IntToStr(GetLastError) + ' (' +
        removeLineBreaks(SysErrorMessage(GetLastError)) + ')', LLerror);
    end;
  end;
  {$ENDIF WINDOWS}
  //StrDispose(exist);
  //StrDispose(new);
end;
*)

{$IFDEF UNIX}
function TuibFileInstall.chmod(mode: string; const FileName: string): boolean;
begin
  mode := opsiUnquoteStr(mode, '"');
  mode := opsiUnquoteStr(mode, '''');
  if 0 = fpchmod(FileName, StrToInt('&' + mode)) then
    Result := True
  else
    Result := False;
end;

function TuibFileInstall.calcMode(mode: string; const FileName: string): string;
var
  info : Stat;
  stMode, aux: Cardinal;
  i, posSym : integer;
  inputMode, rwxPart, existingMode, existingOctalMode, resultMode : string;
  octalPart : char ;
begin
  try
  // 1- Extracting the existing Mode in the input File
  existingMode :='';
  //existingOctalMode := '-';

  // fpStat gets information about the file specified with its path,
  // and stores it in Info, of type Stat
  // Docu : https://www.freepascal.org/docs-html/rtl/baseunix/fpstat.html
  if fpStat (FileName, info) <> 0 then
     begin
       LogDatei.log('fpStat failed : ', LLcritical);
       //halt (1);
     end
  else
     begin
      // Info is a Stat record of a given file
      // info.st_mode returns the existing mode within a file
      stMode := info.st_mode;
      aux := stMode;
      // converting info.st_mode to octal format
      for i:=0 to 2 do
      begin
        existingMode :=chr((aux and $7)+$30)+existingMode;
        aux:=aux shr 3;
      end;
     end;
  (* // converting the octal-mode format to the rwx-mode format
  i := 1;
  repeat
    if existingOctalMode[i] = '7' then existingOctalMode := existingOctalMode+ 'rwx' ;
    if existingOctalMode[i] = '6' then existingOctalMode := existingOctalMode+ 'rw-' ;
    if existingOctalMode[i] = '5' then existingOctalMode := existingOctalMode+ 'r-x' ;
    if existingOctalMode[i] = '4' then existingOctalMode := existingOctalMode+ 'r--' ;
    if existingOctalMode[i] = '3' then existingOctalMode := existingOctalMode+ '-wx' ;
    if existingOctalMode[i] = '2' then existingOctalMode := existingOctalMode+ '-w-';
    if existingOctalMode[i] = '1' then existingOctalMode := existingOctalMode+ '--x';
    if existingOctalMode[i] = '0' then existingOctalMode := existingOctalMode+ '---' ;
    i:= i+1;
  until j> length(existingMode);
  *)

  //Writeln('existingMode : ' + existingMode);

  // 2- Formatting the input Mode
  inputMode := '   ';
  //SetLength(inputMode, 3) ;

  posSym := max(pos('=',mode), pos('+',mode)) ;
  posSym := max(posSym, pos('-',mode)) ;

  rwxPart := copy(mode, posSym+1, length(mode));

  if rwxPart = 'rwx' then octalPart := '7' ;
  if rwxPart = 'rw'  then octalPart := '6' ;
  if rwxPart = 'rx'  then octalPart := '5' ;
  if rwxPart = 'r'   then octalPart := '4' ;
  if rwxPart = 'wx'  then octalPart := '3' ;
  if rwxPart = 'w'   then octalPart := '2' ;
  if rwxPart = 'x'   then octalPart := '1' ;
  if rwxPart = ''    then octalPart := '0' ;

 if pos('u',mode) = 0 then
    inputMode[1] := '0'
 else
    inputMode[1] := octalPart;

 if pos('g',mode) = 0 then
    inputMode[2] := '0'
 else
    inputMode[2] := octalPart;

 if pos('o',mode) = 0 then
    inputMode[3] := '0'
 else
    inputMode[3] := octalPart;

 //Writeln('inputMode : ' + inputMode);

  // 3- Calculating the resulting Mode
  resultMode := '   ';
  //SetLength(resultMode, 3) ;

  if pos('=',mode) > 0 then
       resultMode := inputMode;

  if pos('+',mode) > 0 then
    for i:= 1 to 3 do
        // "+" operation is equivalent to a bitwise OR
        resultMode[i] := IntToSTr( (StrToInt(existingMode[i])) or (StrToInt(inputMode[i])) )[1];

  if pos('-',mode) > 0 then
    for i:= 1 to 3 do
        // "-" operation is equivalent to a bitwise XAND
        resultMode[i] := IntToSTr( (StrToInt(existingMode[i])) and (not(StrToInt(inputMode[i]))) )[1];

  //Writeln('resultMode : ' +resultMode);

  Result := trim(resultMode);

  except
    on E: Exception do
    begin
      LogDatei.log('Exception: Failed to caculate mode : ' + mode
         + ' for: ' + FileName + ': ' + E.message, LLError);
    end;
  end;
end;

{$ENDIF LINUX}


function TuibFileInstall.GetReadOnlyAttribute
  (const FileName: string; eliminate: boolean): boolean;
var
  //ErrorNo: integer;
  Attr: integer = 0;
  ActionI: string = '';
begin
  Result := False;
  Attr := SysUtils.FileGetAttr(FileName);
  if Attr and faReadOnly = faReadOnly then
  begin
    Result := True;
    if not eliminate then
    begin
      LogS := 'Warning: "' + FileName + '" is readonly';
      LogDatei.log(LogS, LLinfo);
    end
    else
    begin
      if FileGetWriteAccess(Filename, ActionI) then
      begin
        Result := False;
        LogS := 'Info: ' + ActionI;
        Logdatei.Log(LogS, LLinfo);
        LogDatei.NumberOfHints := LogDatei.NumberOfHints + 1;
      end
      else
      begin
        LogS := 'Error: ' + ActionI;
        LogDatei.log(LogS, LLError);
      end;
    end;
  end;
end;

{$IFDEF WINDOWS}
function Prefer
  (const File1, File2: string; PreferIfEqual: boolean;
  var File1Hint, File2Hint: string; var WeHaveAProblem, OtherWarning: boolean): boolean;

type
  TComparisonResult = (tcrOpen, tcrEqual, tcrPositive, tcrNegative,
    tcrNotDecided, tcrNotComparable);

var
  Expression1: string = '';
  Expression2: string = '';
  Info1: string = '';
  Info2: string = '';
  i: integer = 0;
  //lang1pos, lang2pos: integer;
  firstCriterion, secondCriterion: TComparisonResult;
  FilesDiffer: boolean;
  Size1, Size2: longint;
  //File1VersionInfoRec, File2VersionInfoRec : TVersionInfoRec;
  versionFile1: TVersionInfo;
  versionFile2: TVersionInfo;

begin
(* ersetzt durch unit VersionInfo ; do 18.12.04
 CentralForm.VersionInfoResource1.FileName := File1;
 CentralForm.VersionInfoResource2.FileName := File2;
*)


  versionFile1 := TVersionInfo.Create(File1);
  versionFile2 := TVersionInfo.Create(File2);
  //File1VersionInfoRec := GetVersionInfoRec(File1);
  //File2VersionInfoRec := GetVersionInfoRec(File2);

  Result := False;


  WeHaveAProblem := False;
  OtherWarning := False;

  FirstCriterion := tcrOpen;
  SecondCriterion := tcrOpen;


  FilesDiffer := True;

     (* if CentralForm.VersionResource1.VersionInfo = CentralForm.VersionResource2.VersionInfo
     then
     Begin
       Assign (f, File1);
       Reset (f);
       Size1 := Filesize (f);
       Close (f);
       Assign (f, File2);
       Reset (f);
       Size2 := Filesize (f);
       close (f);
       if Size1 = Size2
       then
       FilesDiffer := false;
     End;
         *)


  // with CentralForm.VersionInfoResource1  do
  //   File1Hint :=
  //      'FileVersion '  + FileVersion.AsString + '  '
  //    + 'ProductVersion ' + ProductVersion.AsString (* FileVersion2 *)
  //   (* + '  Language ' + Language *);

  // with CentralForm.VersionInfoResource2  do
  //   File2Hint :=
  //      'FileVersion '  + FileVersion.AsString + '  '
  //    + 'ProductVersion ' + ProductVersion.AsString (* FileVersion2 *)
  //   (* + '  Language ' + Language *);

  File1Hint := 'FileVersion ' + versionFile1.getString('FileVersion') +
    '  ' + 'ProductVersion ' + versionFile1.getString('ProductVersion');

  File2Hint := 'FileVersion ' + versionFile2.getString('FileVersion') +
    '  ' + 'ProductVersion ' + versionFile2.getString('ProductVersion');

 (*
 with File1VersionInfoRec  do
   File1Hint :=
      'FileVersion '  + FileVersion + '  '
    + 'ProductVersion ' + ProductVersion;

 with File2VersionInfoRec  do
   File2Hint :=
      'FileVersion '  + FileVersion + '  '
    + 'ProductVersion ' + ProductVersion;
 *)

  Size1 := SizeOfFile(File1);
  Size2 := SizeOfFile(File2);


  if File1Hint = File2Hint then
  begin
    if PreferIfEqual then
      Result := True;

    if Size1 = Size2 then
   (* alle verfuegbaren Informationen deuten auf Gleichheit hin,
      daher wahrscheinlich unproblematisch *)
      FilesDiffer := False
    else
    begin
      File1Hint := ' ... identical information here, but files differ ... ';
      WeHaveAProblem := True;
    end;

    exit; (* machen kann man aber nichts *)
  end;


  if Size1 = 0 then
  begin
    File1Hint := ' Size of File is 0';
    OtherWarning := True;
    exit;
  end
  else if Size2 = 0 then
  begin
    File2Hint := ' Size of File is 0';
    Result := True;
    OtherWarning := True;
    exit;
  end;


  (* FileVersion  inspizieren *)

  // Info1 := CentralForm.VersionInfoResource1.FileVersion.AsString;
  // Info2 := CentralForm.VersionInfoResource2.FileVersion.AsString;

  Info1 := versionFile1.getString('FileVersion'); //File1VersionInfoRec.FileVersion;
  Info2 := versionFile2.getString('FileVersion'); //File2VersionInfoRec.FileVersion;


  Info1 := CutLeftBlanks(Info1);
  Info2 := CutLeftBlanks(Info2);

  if Info1 = Info2 then

    firstCriterion := tcrEqual
  else
  begin
    if (Info1 = '') or (Info2 = '') or (Info1 = '0.0.0.0') or (Info2 = '0.0.0.0') then
    begin
      firstCriterion := tcrNotComparable;
      WeHaveAProblem := True; (* weil sie nicht gleich sein koennen *)
    end;

    i := 1;
    while (firstCriterion = tcrOpen) and (i <= 4) do
     (* Numerischer Vergleich der vier durch Punkte abgetrennten Komponenten
        der beiden FileVersion-Inhalte *)
    begin
      Expression1 := GetIdentComponent(Info1, Info1);
      Expression2 := GetIdentComponent(Info2, Info2);
      if StrToIntDef(Expression1, 0) > StrToIntDef(Expression2, 0) then
        firstCriterion := tcrPositive
      else if StrToIntDef(Expression1, 0) < StrToIntDef(Expression2, 0) then
        firstCriterion := tcrNegative;

      (* wenn Expression leer, ist der zugeordnete Wert 0 und die Datei wird bzw. bleibt nachrangig *)
      Inc(i);
    end;

    if firstCriterion = tcrOpen then
    begin
      firstCriterion := tcrNotDecided; (* kann eigentlich nicht vorkommen *)
      WeHaveAProblem := True;
    end;
  end;

  (* FileVersion2 inspizieren *)

  // Info1 := CentralForm.VersionInfoResource1.Productversion.AsString;
  // Info2 := CentralForm.VersionInfoResource2.Productversion.AsString;

  //Info1 := File1VersionInfoRec.Productversion;
  //Info2 := File2VersionInfoRec.Productversion;
  Info1 := versionFile1.getString('ProductVersion');
  Info2 := versionFile2.getString('ProductVersion');


  Info1 := CutLeftBlanks(Info1);
  Info2 := CutLeftBlanks(Info2);

  if Info1 = Info2 then
    SecondCriterion := tcrEqual
  else
  begin
    if (Info1 = '') or (Info2 = '') or (Info1 = '0.0.0.0') or (Info2 = '0.0.0.0') then
    begin
      secondCriterion := tcrNotComparable;
      WeHaveAProblem := True; (* weil sie nicht gleich sein koennen *)
    end;

    i := 1;
    while (secondCriterion = tcrOpen) and (i <= 4) do
     (* Numerischer Vergleich der vier durch Punkte abgetrennten Komponenten
        der beiden FileVersion-Inhalte *)
    begin
      Expression1 := GetIdentComponent(Info1, Info1);
      Expression2 := GetIdentComponent(Info2, Info2);
      if StrToIntDef(Expression1, 0) > StrToIntDef(Expression2, 0) then
        secondCriterion := tcrPositive
      else if StrToIntDef(Expression1, 0) < StrToIntDef(Expression2, 0) then
        secondCriterion := tcrNegative;

      (* wenn Expression leer, ist der zugeordnete Wert 0 und die Datei wird bzw. bleibt nachrangig *)
      Inc(i);
    end;

    if secondCriterion = tcrOpen then
    begin
      secondCriterion := tcrNotDecided; (* kann eigentlich nicht vorkommen *)
      WeHaveAProblem := True;
    end;
  end;

  (* Resuemee ziehen *)

  if ((FirstCriterion = tcrPositive) and (SecondCriterion = tcrPositive)) or
    ((FirstCriterion = tcrEqual) and (SecondCriterion = tcrPositive)) or
    ((FirstCriterion = tcrPositive) and (SecondCriterion = tcrEqual)) then
    (* klarer Fall *)
    Result := True

  else if (FirstCriterion = tcrEqual) and (SecondCriterion = tcrEqual) then
  begin
   {lang1pos  :=
       StrIsIn (CentralForm.VersionResource1.VersionInfo.Language, LangPreferenceOrder);
   lang2pos  :=
       StrIsIn (CentralForm.VersionResource2.VersionInfo.Language, LangPreferenceOrder);
   if (0 < lang1pos) and (0 < lang2pos)
   then
   Begin
     if lang1pos < lang2pos
     then
       Result := true
     else if lang1pos = lang2pos
     then
       if PreferIfEqual then Result := true;
   End
   else }
    (* Language-Info nicht verwenden *)
    if PreferIfEqual then
      Result := True;

    if {(CentralForm.VersionResource1.VersionInfo.Language
   = CentralForm.VersionResource2.VersionInfo.Language)
   and}
    FilesDiffer then
      WeHaveAProblem := True;
  end

  else if (FirstCriterion = tcrPositive) and (SecondCriterion in
    [tcrNotDecided, tcrNotComparable]) then
    Result := True
  else if (SecondCriterion = tcrPositive) and (FirstCriterion in
    [tcrNotDecided, tcrNotComparable]) then
    Result := True
  else
  (* Result bleibt false; der bisher nicht erfasste Fall fuer WeHaveAProblem = false: *)

  if ((FirstCriterion = tcrPositive) and (SecondCriterion = tcrNegative)) or
    ((FirstCriterion = tcrNegative) and (SecondCriterion = tcrPositive)) then
    WeHaveAProblem := True;

end;


function TuibFileInstall.FileCheckVersions(const SourceF, TargetF: string;
  restrictedToTargetDir: boolean): boolean;
var
  InstallOK: boolean;
  delicate, warning: boolean;

  File1Info: string = '';
  File2Info: string = '';
  CopyOfTargetF: string;

  Directory: string = '';

  function WeHaveAFileProblem(FName: string): boolean;
  var
    ModuleHandle: THandle;
    FnameS1: PChar;
    Size: integer;
  begin
    Result := False;
    FnameS1 := PointerAufString(FName);
    Size := length(FName) + 1;
    ModuleHandle := GetModuleHandle(FnameS1);

    //freemem(FnameS1);

    if SizeOfFile(FName) = 0 then
    begin
      Result := True;
      LogS := FName + ' has size 0';
      Logdatei.Log(LogS, LLinfo);
    end;
  end;

  function UnusedCopyOfFile(FName: string): string;
  var
    problem: string = '';
    rebootWanted: boolean;
  begin
    if WeHaveAFileProblem(FName) then
    begin
      MakePath(TempPath + UsedFilesTempSubDirectory);
      Result := TempPath + UsedFilesTempSubDirectory + ExtractFileName(FName);
      FileCopy(Fname, Result, problem, False, rebootWanted);
    end
    else
      Result := FName;
  end;

  procedure CheckAlternativeF(const Directory: string; var InstallOK: boolean);

  var
    AlternativeF: string = '';
    CopyOfAlternativeF: string = '';

  begin
    AlternativeF := Directory + ExtractFileName(SourceF);
    if fileexists(AlternativeF) then
    begin
      CopyOfAlternativeF := UnusedCopyOfFile(AlternativeF);

      if Prefer(CopyOfAlternativeF, SourceF, False, File1Info,
        File2Info, delicate, warning) then
      begin
        LogS :=
          'Warning: File not copied - existing ' + AlternativeF +
          ' seems to be newer than source ' + Sourcef;
        Logdatei.Log(LogS, LLwarning);

        InstallOK := False;
      end
      else
      begin
        LogS :=
          'Warning: Older or equally old Version of ' + SourceF +
          ' exists in ' + Directory;
        Logdatei.Log(LogS, LLwarning);
      end;

      if delicate then
      begin
        LogS :=
          'Warning: Ordering of files is dubious: ';
        Logdatei.Log(LogS, LLwarning);
      end;

      if warning then
      begin
        LogS := 'Warning: File cannot be read';
        Logdatei.Log(LogS, LLwarning);
      end;



      LogS := LogDatei.LogSIndentPlus(1) + 'Source     ' + SourceF +
        '  ===  ' + File2Info;
      Logdatei.Log(LogS, LLinfo);
      LogS := LogDatei.LogSIndentPlus(1) + 'Compare to ' + AlternativeF +
        '  ===  ' + File1Info;
      Logdatei.Log(LogS, LLinfo);
    end;
  end;

begin
  InstallOK := True;

  if FileExists(TargetF) then
  begin

    CopyOfTargetF := UnusedCopyOfFile(TargetF);

    if Prefer(CopyOfTargetF, SourceF, False, File1Info, File2Info,
      delicate, warning) then
    begin
      LogS :=
        'Warning: Existing ' + TargetF + ' seems to be newer than Source ' +
        SourceF + ' - no overwrite ';
      Logdatei.Log(LogS, LLwarning);

      InstallOK := False;
    end
    else
    begin
      LogS :=
        'Target ' + TargetF +
        ' exists, but seems to be older or equally old than source ' + SourceF;
      Logdatei.Log(LogS, LLinfo);
    end;

    if delicate then
    begin
      LogS :=
        'Warning: Ordering of files is dubious: ';
      Logdatei.Log(LogS, LLwarning);
    end;

    if warning then
    begin
      LogS :=
        'Warning: File cannot be read';
      Logdatei.Log(LogS, LLwarning);
    end;


    LogS := LogDatei.LogSIndentPlus(1) + 'Source ' + SourceF +
      '  ===  ' + File2Info;
    Logdatei.Log(LogS, LLinfo);
    LogS := LogDatei.LogSIndentPlus(1) + 'Target ' + TargetF +
      '  ===  ' + File1Info;
    Logdatei.Log(LogS, LLinfo);

  end;

  if not restrictedToTargetDir then
  begin
    (* Vergleich mit potentiell existierender Datei gleichen Namens in Windows\ *)
    Directory := GetWinDirectory;
    if Directory <> ExtractFilePath(TargetF) then
      CheckAlternativeF(Directory, InstallOK);

    (* ditto in Windows\system *)
    Directory := GetWinSystemDirectory;
    if Directory <> ExtractFilePath(TargetF) then
      CheckAlternativeF(Directory, InstallOK);
  end;

  Result := InstallOK;
end;

{$ENDIF WINDOWS}

{$IFDEF WINDOWS}
function CompareFileTime_WithTimeInterval
  (filetime1, filetime2: TFiletime; seconds: real): integer;

var
  delayed_ft2: TFiletime;
  diffresult1, diffresult2: integer;
begin

  delayed_ft2.dwHighDateTime := filetime2.dwHighDateTime;
  delayed_ft2.dwLowDateTime :=
    filetime2.dwLowDateTime + round((2 * seconds) * 10000000);

  (* filetime gibt Nanosekunden an *)
  diffresult1 := CompareFileTime(@filetime1, @filetime2);
  diffresult2 := CompareFileTime(@filetime1, @delayed_ft2);


      (*    comparefiletime yields
      -1     if:  First file time is less than second file time.
      0      if:  First file time is equal to second file time.
      +1     if:  First file time is greater than second file time.
      *)

  if (diffresult1 <> diffresult2) then
    Result := 0 // "fast gleiche" Zeiten, genaueres weiss man nicht
  else
    Result := diffresult1;
end;

{$ENDIF WINDOWS}

function TuibFileInstall.FileCheckDate
  (const Sourcefilename, Targetfilename: string; OverwriteIfEqual: boolean): boolean;
(*
{$IFDEF UNIX}
begin
  result := FileCheckDate(Sourcefilename, Targetfilename,OverwriteIfEqual);
end;
{$ENDIF LINUX}
*)

var
  {$IFDEF UNIX}
  fstatRecordSource, fstatRecordTarget: stat;
  uxtime1, uxtime2: cardinal;
  {$ENDIF LINUX}
  fRecordSource, fRecordTarget: TSearchRec;
  diffresult: longint;
  ErrorInfo: string;
  filetime1, filetime2: TFiletime;
  systime1, systime2: TSystemTime;
  datetime1, datetime2: TDateTime;

begin
  try
    if not GetFileInfo(Sourcefilename, fRecordSource, ErrorInfo) then
    begin
      Result := False;
      LogS := 'Error:  ' + SourceFileName + ' does not exist';
      ;
      LogDatei.log(LogS, LLError);
    end
    else if not GetFileInfo(Targetfilename, fRecordTarget, ErrorInfo) then
    begin
      Result := False;
      LogS := 'Info:  ' + TargetFileName + ' does not exist ' +
        SourceFileName + ' will be copied ';
      ;
      LogDatei.log(LogS, LLinfo);
      LogDatei.NumberOfHints := LogDatei.NumberOfHints + 1;
    end
    else
    begin
      {$IFDEF LINUX}
      if 0 <> fpstat(Sourcefilename, fstatRecordSource) then
      begin
        Result := False;
        LogS := 'Error: Could not stat ' + SourceFileName + ' : ' +
          SysErrorMessage(fpgeterrno);
        LogDatei.log(LogS, LLError);
      end
      else
        uxtime1 := fstatRecordSource.st_mtime;

      if 0 <> fpstat(Targetfilename, fstatRecordTarget) then
      begin
        Result := False;
        LogS := 'Error: Could not stat ' + TargetFileName + ' : ' +
          SysErrorMessage(fpgeterrno);
        LogDatei.log(LogS, LLError);
      end
      else
        uxtime2 := fstatRecordTarget.st_mtime;
      dateTime1 := UnixToDateTime(uxtime1);
      dateTime2 := UnixToDateTime(uxtime2);
      diffresult := abs(uxtime1 - uxtime2);
      if diffresult < 2 then
        diffresult := 0;
      {$ENDIF LINUX}
      {$IFDEF DARWIN}
      LogDatei.log('not implemented for macos', LLError);
      {$ENDIF DARWIN}

  {$IFDEF WINDOWS}
      //LogDatei.log('FileCheckDate 1', LLInfo);
      filetime1 := fRecordSource.FindData.ftLastWriteTime;
      //LogDatei.log('FileCheckDate 2', LLInfo);
      filetime2 := fRecordTarget.FindData.ftLastWriteTime;
      //LogDatei.log('FileCheckDate 3', LLInfo);
      LogDatei.log_prog('FileCheckDate Source:' + UIntToStr(
        filetime1.dwHighDateTime) + ' / ' + UIntToStr(filetime1.dwHighDateTime) +
        ' Target: ' + UIntToStr(filetime2.dwHighDateTime) + ' / ' +
        UIntToStr(filetime2.dwHighDateTime), LLInfo);

      diffresult := CompareFileTime_WithTimeInterval(filetime1, filetime2, 2);
      //LogDatei.log('FileCheckDate 4', LLInfo);

        (*
        -1  First file time is less than second file time.
        0     First file time is similar to second file time.
        +1  First file time is greater than second file time.
        *)

       (*
      filetimetosystemtime(fRecordSource.FindData.ftLastWriteTime, systime1);
      filetimetosystemtime(fRecordTarget.FindData.ftLastWriteTime, systime2);
      *)

      filetimetosystemtime(filetime1, systime1);
      //LogDatei.log('FileCheckDate 5', LLInfo);
      filetimetosystemtime(filetime2, systime2);
      //LogDatei.log('FileCheckDate 6', LLInfo);

      dateTime1 := SystemTimeToDateTime(sysTime1);
      //LogDatei.log('FileCheckDate 7', LLInfo);
      dateTime2 := SystemTimeToDateTime(sysTime2);
      //LogDatei.log('FileCheckDate 8', LLInfo);
      {$ENDIF WINDOWS}
      LogS := '"' + TargetFileName + '" has LastWriteTime ' + DateTimeToStr(dateTime2);
      LogDatei.log(LogS, LLInfo);
      LogS := '"' + SourceFileName + '" has LastWriteTime ' + DateTimeToStr(dateTime1);
      LogDatei.log(LogS, LLInfo);


      //(Source-Date1 > Target-Date2) or ((Date2 = Date1) and OverwriteIfEqual)


      //Logdatei.Log (inttostr (diffresult), LLinfo);
      if (diffresult = 1) or ((diffresult = 0) and OverwriteIfEqual) then
      begin
        Result := True;
        LogS := 'Info:  Target "' + TargetFileName + '"  shall be overwritten: ';
        if diffresult = 0 then
          LogS := LogS + 'Its age is about the age of source  "'
        else
          LogS := LogS + 'It is older than ';
        LogS := LogS + 'source  "' + SourceFileName + '"';
        LogDatei.log(LogS, LLInfo);
        LogDatei.NumberOfHints := LogDatei.NumberOfHints + 1;
      end
      else
      begin
        Result := False;
        LogS := 'Target "' + TargetFileName + '"  will not be replaced: ';
        if diffresult = 0 then
          LogS := LogS + 'It has about the same age as '
        else
          LogS := LogS + 'It is younger than ';
        LogS := LogS + 'source  "' + SourceFileName + '"';
        LogDatei.log(LogS, LLInfo);
      end;
    end;
  except
    on e: Exception do
    begin
      LogDatei.log('Exception: Error on FileCheckDate: ' + e.message,
        LLerror);
      LogDatei.log(' Source:' + UIntToStr(filetime1.dwHighDateTime) +
        ' / ' + UIntToStr(filetime1.dwHighDateTime) + ' Target: ' +
        UIntToStr(filetime2.dwHighDateTime) + ' / ' + UIntToStr(
        filetime2.dwHighDateTime) + ' Diff: ' + IntToStr(diffresult), LLerror);
      Result := False;
    end
  end;
end;


procedure TuibFileInstall.AllCopy(const SourceMask, Target: string;
  cpSpecify: TcpSpecify; CountModus: TcpCountModus; var NumberCounted: integer;
  var RebootWanted: boolean);

var
  FileFound: boolean;
  Recursion_Level: integer = 0;
  problem: string = '';
  Copycount: TCopyCount;


  procedure AllCopyRecursive
    (var Recursion_Level: integer; SourceMask, Target: string;
  var FileFound: boolean; var cpSpecify: TcpSpecify; var RebootWanted: boolean);

  var
    {$IFDEF WIN32}
    ZipFileName: tdirtype;
    {$ENDIF WIN32}
    CompleteName, Extension, SourcePath, SourceFilemask, SourceName,
    TargetPath, TargetName: FileNameStr;
    PSourceName: PChar;
    FindResultcode: integer = 0;
    FileAttr: integer = 0;
    SearchResult: TSearchRec;

    FileFoundOnThisLevel, FileFoundOnADeeperLevel: boolean;
    DirectoryExisted: boolean;
    DirectoryError: integer = 0;

    DecompressedSource: string = '';
    FName: string = '';
    SaveLogLevel: integer = 0;
    followsymlinks: boolean = False;
    //doNotChangeAccessRights: boolean = False;
    doNotChangeAccessRights: boolean = True;

    procedure ToCopyOrNotToCopy(const SourceName, TargetName: string);
    var
      CopyShallTakePlace: boolean;

    begin
      CopyShallTakePlace := True;
      if SourceName = TargetName then
      begin
        LogS := 'Warning: ' + ' Target "' + TargetName + '" = Source ';
        LogDatei.log(LogS, LLwarning);
        CopyShallTakePlace := False;
      end;

      if CopyShallTakePlace then
      begin
        if copyDenyList.IndexOf(ExtractFileName(SourceName)) > -1 then
        begin
          CopyShallTakePlace := False;
          LogDatei.log('Copy of source: ' + ExtractFileName(
            SourceName) + ' denied by policy.', LLinfo);
        end;
      end;

      if CopyShallTakePlace and FileExists(TargetName) then
      begin
        CopyShallTakePlace := True;

        {$IFDEF WINDOWS}
        if cpSpecify and cpVersionControl = cpVersionControl then
        begin
         {
         Extension := UpperCase (ExtractFileExt (TargetName));
         delete (Extension, 1, 1);
         if StrIsIn (Extension, ExtensionsForVersionControl) > 0
         then }
          CopyShallTakePlace := FileCheckVersions(SourceName, TargetName, False);
        end;

        if cpSpecify and cpVersionControlRestricted = cpVersionControlRestricted then
        begin
         {
         Extension := UpperCase (ExtractFileExt (TargetName));
         delete (Extension, 1, 1);
         if StrIsIn (Extension, ExtensionsForVersionControl) > 0
         then }
          CopyShallTakePlace := FileCheckVersions(SourceName, TargetName, True);
        end;
        {$ENDIF WINDOWS}
      end;

      if CopyShallTakePlace and FileExists(TargetName) then
      begin
        if cpSpecify and cpNoOverwrite = cpNoOverwrite then
        begin
          CopyShallTakePlace := False;
          LogS := 'Target ' + TargetName + ' exists - no copy';
          LogDatei.log(LogS, LLinfo);
        end
        else if cpSpecify and cpUpdate = cpUpdate then
        begin
          CopyShallTakePlace := FileCheckDate(SourceName, TargetName, False);
        end
        else if cpSpecify and cpFollowSymlinks = cpFollowSymlinks then
        begin
          followsymlinks := True;
        end
        else if cpSpecify and cpDateControl = cpDateControl then
        begin
          Extension := UpperCase(ExtractFileExt(TargetName));
          system.Delete(Extension, 1, 1);
          if (Extension = 'EXE') then
            CopyShallTakePlace := FileCheckDate(SourceName, TargetName, True);
        end
        else
        begin
          LogS := 'Info: Target ' + TargetName +
            ' exists and shall be overwritten';
          LogDatei.log(LogS, LLDebug);
          LogDatei.NumberOfHints := LogDatei.NumberOfHints + 1;
        end;

        if CopyShallTakePlace then

          if cpDontForceOverwrite = cpSpecify and cpDontForceOverwrite then
          begin
            if GetReadonlyAttribute(TargetName, False) then
            begin
              CopyShallTakePlace := False;
              LogS := '"' + TargetName + '" will not be overwritten';
              LogDatei.log(LogS, LLinfo);
            end;
          end
          else
            GetReadOnlyAttribute(TargetName, True);

      end;

      if CopyShallTakePlace then
      begin
        if cpLeaveReadonly = cpSpecify and cpLeaveReadonly then
          doNotChangeAccessRights := True;

        if FileCopy(SourceName, TargetName, problem, True, rebootWanted,
          followsymlinks, doNotChangeAccessRights) then
        begin
          LogS := 'copy: ' + SourceName + ' copied to ' + TargetPath;
          LogDatei.log(LogS, LLInfo);
          if problem <> '' then
          begin
            LogS := 'Warning: ' + problem;
            LogDatei.log(LogS, LLWarning);
          end;
        end
        else
        begin
          LogS := 'Error: copy of ' + SourceName + ' to ' +
            TargetName + ' not possible. ' + problem;
          LogDatei.log(LogS, LLError);
        end;


        if not (cpLeaveReadonly = cpSpecify and cpLeaveReadonly) then
          GetReadOnlyAttribute(TargetName, True);
        // a readonly attribute will be eliminated

      end;
    end;

    {$IFDEF WIN32}
    procedure HandleArchive(CompressMethod: TCompressMethod);
    var
      NumberOfExtractedFiles, fileI: integer;
      TempCopyOfSource: string = '';
      zipresult: boolean;
      //i: integer;

      rc: integer = 0;
      packfileinfo: tpackrec;
      buf: tdirtype;
      ZipError: string = '';
      ExtractTempDir: string = '';
      PExtractTempDir: PChar;

      problem: string = '';

    begin
      ExtractTempDir := TempPath + ExtractTempSubDirectory;
      PExtractTempDir := PointerAufString(ExtractTempDir);

      case CompressMethod of
        tcmLZH:
          LogS := SourceName + ' identified as LZH archive';
        tcmLHA:
          (* LogS :=  SourceName + ' identified as LHA archive'*);
        tcmZIP:
          LogS := SourceName + ' identified as ZIP archive';
      end;


      Logdatei.Log(LogS, LLinfo);

      MakePath(ExtractTempDir);
      SaveLogLevel := LogDatei.LogLevel;
      LogDatei.LogLevel := LLinfo;
      AllDelete(ExtractTempDir + '*.*', True, True, 0);
      LogDatei.LogLevel := SaveLogLevel;


      CentralForm.zipfiles.items.Clear;

      case CompressMethod of
        tcmLHA: ;

        tcmLZH:
        begin
        end;

        tcmZIP:
        begin
          rc := getfirstinzip(ZipFileName, packfileinfo);

          problem := '';
          if not CheckDriveSpaceForTarget(ExtractTempDir, packfileinfo.Size, problem)
          then
          begin
            LogS := 'Warning: ' + problem;
            Logdatei.Log(LogS, LLwarning);
          end;

          zipresult := True;
          zipError := '';
          while rc = zip_ok do
          begin
            strcopy(buf, PExtractTempDir);
            strcat(buf, packfileinfo.filename);
            rc := zipinter.unzipfile(ZipFileName, buf,
              packfileinfo.attr, packfileinfo.headeroffset, 0, 0);
            // no keyboard interrupts
            if rc = unzip_ok then
              CentralForm.zipfiles.items.Add(buf)
            else
            begin
              zipresult := False;
              ZipError := '';
              case rc of
                unzip_CRCErr: ZipError := ZipFileName + ' CRC-Error!';
                unzip_WriteErr: ZipError := ZipFileName + ' Write error!';
                unzip_ReadErr: ZipError := ZipFileName + ' Read error!';
                unzip_ZipFileErr: ZipError :=
                    ZipFileName + ' Error in Zip file structure!';
                unzip_UserAbort: ZipError := ZipFileName + ' Aborted by user!';
                unzip_NotSupported:
                  if packfileinfo.packmethod <= 8 then
                    ZipError :=
                      ZipFileName + ' Format ' +
                      StrPas(methods[packfileinfo.packmethod]) + ' not supported!'
                  else
                    ZipError :=
                      ZipFileName + ' Unknown pack method ' +
                      IntToStr(packfileinfo.packmethod) + '!';
                unzip_Encrypted: ZipError := ZipFileName + ' File encrypted, skipped!';
                unzip_InUse: ZipError :=
                    ZipFileName + ' DLL already in use, try later or use pkunzip!';
              end;
              LogS := 'Error: ' + ZipError;
              LogDatei.log(LogS, LLError);
            end;

            if (rc = unzip_ReadErr) or (rc = unzip_Userabort) or
              (rc = unzip_InUse) or (rc = unzip_ZipFileErr) then
              rc := -100    // Serious error, force abort
            else
            begin
              rc := getnextinzip(packfileinfo);
              problem := '';
              if not CheckDriveSpaceForTarget(ExtractTempDir,
                packfileinfo.Size, problem) then
              begin
                LogS := 'Warning: ' + problem;
                Logdatei.Log(LogS, LLwarning);
              end;
            end;
          end;
          closezipfile(packfileinfo);
          // Free memory used for central directory info
        end;
      end;

      if zipresult then
      begin
        NumberOfExtractedFiles := CentralForm.zipfiles.items.Count;
        for FileI := 0 to NumberOfExtractedFiles - 1 do
        begin
          FName := CutRightBlanks(CentralForm.zipfiles.items[FileI]);
          LogS := FName + ' extracted to ' + ExtractTempDir;
          Logdatei.Log(LogS, LLinfo);
        end;
        for FileI := 0 to NumberOfExtractedFiles - 1 do
        begin
          FName := CutRightBlanks(CentralForm.zipfiles.items[FileI]);

          DecompressedSource := FName;
          TargetName := TargetPath + Extractfilename(FName);
          ToCopyOrNotToCopy(DecompressedSource, TargetName);
        end;
      end;
      // clean up
      SaveLogLevel := LogDatei.LogLevel;
      LogDatei.LogLevel := LLinfo;
      AllDelete(ExtractTempDir + '*.*', True, False, 0);
      LogDatei.LogLevel := SaveLogLevel;
      //freemem(PExtractTempDir);
    end;

   {$ENDIF WIN32}

  begin
    Recursion_Level := Recursion_Level + 1;
    FileFoundOnThisLevel := False;
    if cpSpecify and cpFollowSymlinks = cpFollowSymlinks then
      followsymlinks := True
    else
      followsymlinks := False;

    // bring Source Mask to standard form
    //CompleteName := UTF8ToWinCP(SourceMask);
    CompleteName := SourceMask;
    CompleteName := ExpandFileName(CompleteName);
    SourcePath := ExtractFilePath(CompleteName);
    if SourcePath[length(SourcePath)] <> PathDelim then
      SourcePath := SourcePath + PathDelim;
    SourceFilemask := ExtractFileName(CompleteName);

    {$IFDEF WINDOWS}
    if SourceFilemask = '' then
      SourceFilemask := '*.*';
    {$ELSE}
    if (SourceFilemask = '') or (SourceFilemask = '*.*') then
      SourceFilemask := '*';
    {$ENDIF WINDOWS}

    // bring Target Mask to standard form
    CompleteName := ExpandFileName(Target);
    TargetPath := CompleteName;
    if TargetPath[length(TargetPath)] <> PathDelim then
      TargetPath := TargetPath + PathDelim;


    if (length(TargetPath) > 1) and (TargetPath[length(TargetPath) - 1] = ':') then
    // TargetPath is e.g. c:\, that is, not a file but a volume identifier
    else
    begin
      CompleteName := copy(TargetPath, 1, length(TargetPath) - 1);

      if fileGetAttr(CompleteName) and faDirectory = 0 then
      begin
        LogS := 'Error:  ' + CompleteName +
          ' is no directory and cannot function as target';
        ;
        Logdatei.Log(LogS, LLError);
        exit;
      end;
    end;

    // starting search
    Logdatei.Log('Search: ' + SourcePath + SourceFilemask, LLDebug2);
    //FindResultcode := SysUtils.FindFirst(SourcePath + SourceFilemask,
    //  faAnyfile - faDirectory - faVolumeId, SearchResult);
    {$IFDEF WINDOWS}
    FindResultcode := FindFirstUTF8(SourcePath + SourceFilemask,
      (faSymlink or faAnyfile) - faDirectory, SearchResult);
    {$ELSE}
    // at Linux we have to search here also for directories because
    // we need to find symlinks to directories (bug in returned attr)
    if followsymlinks then
      FindResultcode := FindFirstUTF8(SourcePath + SourceFilemask,
        (faSymlink or faAnyfile) - faDirectory, SearchResult)
    else
      FindResultcode := FindFirstUTF8(SourcePath + SourceFilemask,
        (faSymlink or faAnyfile), SearchResult);
    {$ENDIF WINDOWS}

    if FindResultcode = 0 then
      FileFoundOnThisLevel := True
    else
    begin
      if CountModus <> tccmCounting then
      begin
        LogS := 'No (non-directory) file with mask  ' + SourcePath +
          SourceFilemask + '  found';
        Logdatei.Log(LogS, LLInfo);
      end;
    end;

    while FindResultcode = 0 do
    begin
      SourceName := SourcePath + SearchResult.Name;
      TargetName := TargetPath + SearchResult.Name;
      LogDatei.log('Found: ' + SourceName, LLDebug2);
      LogDatei.log_prog('Found: ' + SearchResult.Name + ' with attr:' +
        IntToStr(SearchResult.Attr), LLDebug2);

      if (SearchResult.Attr and faDirectory <> faDirectory)
        // and (SearchResult.Name <> '.') and (SearchResult.Name <> '..'))
        // copy symlinks (even dirs) allways here
        //or (SearchResult.Attr and faSymlink <> faSymlink)  //seems to to work
        or FileIsSymlink(SourcePath + SearchResult.Name) then
        // work only in Linux
        if CopyCount.Ready then
        begin
          {$IFDEF GUI}
          if CountModus <> tccmNoCounter then
            FBatchOberflaeche.SetProgress(round(CopyCount.Ratio * 100), pPercent);
          //SetProgress(round(CopyCount.Ratio * 100));
          {$ENDIF GUI}
          LogS := 'Source ' + SourceName;
          LogDatei.log(LogS, LLDebug);
          //{$IFDEF WIN32}
          if cpSpecify and cpExtract = cpExtract then
          begin
            try
              if UnzipWithDirStruct(SourceName, TargetPath) then
                LogDatei.log('unzipped: ' + SourceName + ' to ' + TargetPath, LLInfo)
              else
                LogDatei.log('Failed to unzip: ' + SourceName + ' to ' +
                  TargetPath, LLError)
            except
              on E: Exception do
              begin
                LogDatei.log('Exception: Failed to unzip: ' +
                  SourceName + ' to ' + TargetPath + ' : ' + e.message, LLError);
              end;
            end;
            (*
            PSourceName := PointerAufString(SourceName);
            strLcopy(@ZipFileName, PSourceName, DirLength + 1);

            if ZipInter.IsZip(ZipFileName) then
              HandleArchive(tcmZIP)
            else
            begin
              begin
                ToCopyOrNotToCopy(SourceName, TargetName);
              end;
            end;
            //freemem(PSourceName);
            *)
          end
          else
            //{$ENDIF WIN32}
            LogDatei.log_prog('copy candidate: ' + SourceName +
              ' to: ' + TargetName, LLDebug2);
          ToCopyOrNotToCopy(SourceName, TargetName);
        end;

      FindResultcode := FindNextUTF8(SearchResult);
      ProcessMess;
    end;

    FindCloseUTF8(SearchResult);
    LogDatei.log_prog('Finished Search: ' + SourcePath + SourceFilemask, LLDebug);

    if (cpSpecify and cpRecursive = cpRecursive) or
      (cpSpecify and cpCreateEmptySubdirectories = cpCreateEmptySubdirectories) then
    begin
   (* Subdirectories im Source-Verzeichnis suchen
     und gegebenenfalls im Targetverzeichnis neu anlegen *)
      {$IFDEF WINDOWS}
      FindResultcode := FindFirstUTF8(SourcePath + '*.*', faAnyfile, SearchResult);
      {$ELSE}
      FindResultcode := FindFirstUTF8(SourcePath + '*', faAnyfile or
        faSymlink, SearchResult);
      {$ENDIF WINDOWS}

      while FindResultcode = 0 do
      begin
        LogDatei.log_prog('Found: ' + SearchResult.Name + ' with attr:' +
          IntToStr(SearchResult.Attr), LLDebug);
        LogDatei.log_prog('Found: ' + SearchResult.Name +
          ' is SymLink by Attr: ' + BoolToStr(
          (SearchResult.Attr and faSymlink = faSymlink), True),
          LLDebug);
        LogDatei.log_prog('Found: ' + SearchResult.Name +
          ' is SymLink by func: ' +
          BoolToStr(FileIsSymlink(SourcePath + SearchResult.Name), True),
          LLDebug);
        {$IFDEF UNIX}
        LogDatei.log_prog('Found: ' + SearchResult.Name +
          ' is SymLink by fpReadLink: ' + BoolToStr(
          (fpReadLink(SourcePath + SearchResult.Name) <> ''), True), LLDebug);
        {$ENDIF LINUX}

        if (SearchResult.Attr and faDirectory = faDirectory) and
          // do not follow symlinks to directories
          //(SearchResult.Attr and faSymlink <> faSymlink) and // seems not work
          (not (FileIsSymlink(SourcePath + SearchResult.Name)) or followsymlinks) and
          // work only in Linux
          //{$IFDEF UNIX}
          //(fpReadLink(SourcePath+SearchResult.Name) = '') and
          //{$ENDIF LINUX}
          (SearchResult.Name <> '.') and (SearchResult.Name <> '..') then
        begin
          DirectoryError := 0;

          TargetName := TargetPath + SearchResult.Name;

          FileAttr := FileGetAttr(TargetName);

          if (FileAttr >= 0) then
          begin

            if FileAttr and faDirectory = faDirectory then
            begin
              DirectoryExisted := True;
            end
            else
            begin
              DirectoryError := 1;

              LogS := 'Error:  ' + TargetName +
                ' exists als filename and cannot be created as directory';

              LogDatei.log(LogS, LLError);
            end;
          end
          else
          begin
            DirectoryExisted := False;
            try
              mkdir(TargetName);
              FileSetAttr(TargetName, SearchResult.Attr and (not faReadOnly));
              LogDatei.log_prog('Created: ' + TargetName +
                ' with attr:' + IntToStr(SearchResult.Attr and
                (not faReadOnly)), LLDebug);
            except
              LogS :=
                'Error: ' + 'missing directory ' + TargetName +
                ' cannot be created';
              LogDatei.log_prog(LogS, LLError);
              DirectoryError := 2;
            end;

          end;

          FileFoundOnADeeperLevel := False;

          if ((cpSpecify and cpRecursive) = cpRecursive) and (DirectoryError = 0) then
          begin
            // Rekursion
            Logdatei.Log('Recursion: ' + SourcePath +
              SearchResult.Name + PathDelim + SourceFileMask, LLDebug2);
            AllCopyRecursive
            (Recursion_Level, SourcePath + SearchResult.Name +
              PathDelim + SourceFileMask,
              TargetName, FileFoundOnADeeperLevel, cpSpecify, RebootWanted);
            if FileFoundOnADeeperLevel then
              FileFoundOnThisLevel := True;
          end;

          if ((cpSpecify and cpCreateEmptySubdirectories) = 0) and
            not FileFoundOnADeeperLevel and not DirectoryExisted and
            (DirectoryError = 0) then
            // angelegtes directory wieder entfernen
            rmdir(TargetName);
        end;
        FindResultcode := FindNextUTF8(SearchResult);
      end;
      FindCloseUTF8(SearchResult);
    end;
    FileFound := FileFoundOnThisLevel; // resp. on a deeper level
    Recursion_Level := Recursion_Level - 1;
  end;

begin
  RebootWanted := False;

  if CountModus <> tccmCounting then
  begin
    LogS := 'Copying  ' + SourceMask + ' -----> ' + Target;
    LogDatei.log(LogS, LLInfo);
    {$IFDEF GUI}
    FBatchOberflaeche.SetMessageText(LogS, mCommand);//setCommandLabel(LogS);
    ProcessMess;
    {$ENDIF GUI}
  end;

  if CountModus = tccmCounted then
  begin
    LogS := IntToStr(NumberCounted) + ' File(s) found';
    LogDatei.log(LogS, LLInfo);
  end;

  CopyCount := TCopyCount.Create(CountModus, NumberCounted);
  {$IFDEF GUI}
  if CountModus = tccmCounted then
    FBatchOberflaeche.SetElementVisible(True, eProgressBar);//showProgressBar(True);
  {$ENDIF GUI}
  FileFound := False;
  Recursion_Level := -1;
  AllCopyRecursive
  (Recursion_Level, SourceMask, Target, FileFound, cpSpecify, RebootWanted);

  if CountModus = tccmCounting then
    NumberCounted := CopyCount.ActCount
  else
  begin
    LogS := IntToStr(CopyCount.ActCount) + ' File(s) treated';
    LogDatei.log(LogS, LLInfo);
    if (CountModus = tccmCounted) and (NumberCounted > CopyCount.ActCount) then
    begin
      LogS := 'Error: Some previously found files were not found now';
      LogDatei.log_prog(LogS, LLError);
    end;
  end;

  CopyCount.Free;
  CopyCount := nil;
  {$IFDEF GUI}
  FBatchOberflaeche.SetElementVisible(False, eProgressBar);//showProgressBar(False);
  FBatchOberflaeche.SetMessageText('', mCommand); //setCommandLabel('');
  ProcessMess;
  {$ENDIF GUI}

end;

function TuibFileInstall.MakePath(const Dirname: string): boolean;

var
  exdirname: string = '';

begin
  Result := True;
  try
    exdirname := ExpandFileName(Dirname);
  except
    on e: Exception do
    begin
      LogDatei.log('Error: "' + exdirname + '" is not a valid Directoryname; "' +
        e.Message + '"', LLError);
    end;
  end;

  if not DirectoryExists(exdirname) then
  begin

    ForceDirectories(exdirname);

    if not DirectoryExists(exdirname) then
    begin
      Result := False;
      LogS := 'Error:  ' + 'Directory ' + exdirname +
        ' does not exist and cannot be created';
      LogDatei.log(LogS, LLerror);
    end
    else
    if not (copy(exdirName, length(exdirName) - 1, 2) = DriveDelim + PathDelim) then
    begin
      LogS := LogDatei.LogSIndent + 'path ' + exdirname + ' created';
      Logdatei.Log(LogS, LLInfo);
    end;

  end;
end;

function TuibFileInstall.AllDelete
  (const Filename: string; recursive, ignoreReadOnly: boolean;
  daysBack: integer; logleveloffset: integer = 0): boolean;
var
  RebootWanted: boolean;
begin
  Result := AllDelete(Filename, recursive, ignoreReadOnly, daysBack,
    True, RebootWanted, logleveloffset);
end;

function TuibFileInstall.AllDelete
  (const Filename: string; recursive, ignoreReadOnly: boolean;
  daysBack: integer; search4file: boolean; var RebootWanted: boolean;
  logleveloffset: integer = 0): boolean;
var
  retryOnReboot: boolean = False;
begin
  Result := AllDelete(Filename, recursive, ignoreReadOnly, daysBack,
    True, RebootWanted, retryOnReboot, logleveloffset);
end;

function TuibFileInstall.AllDelete
  (const Filename: string; recursive, ignoreReadOnly: boolean;
  daysBack: integer; search4file: boolean; var RebootWanted: boolean;
  retryOnReboot: boolean; logleveloffset: integer = 0): boolean;

var
  FileFinder: TSearchRec;
  CompleteName: string = '';
  PathName: string = '';
  testName: string = '';
  FileMask: string = '';
  DeleteDeeperDir, DeleteStartDir: boolean;
  {$IFDEF WINDOWS}
  exist: PWchar;
  exitbool: winbool;
  errorNo: integer;
  {$ELSE WINDOWS}
  exist: PChar;
  {$ENDIF WINDOWS}
  moveflags: DWORD;

  procedure ExecDelete
    (const CompleteName: string; DeleteDir: boolean);
  var
    Filename: string = '';
    testname: string = '';
    FileMask: string = '';
    OrigPath: string = '';
    FindResultcode: integer = 0;
    SearchResult: TSearchRec;

    FileIsReadOnly: boolean;
    shallDelete: boolean;
    ddiff: integer = 0;
    errorno: integer = 0;
    isdeleted: boolean;

  begin

    { analyze Completename }
    OrigPath := ExtractFilePath(CompleteName);
    { should end always with pathdelimiter }
    Filemask := ExtractFileName(CompleteName);

    LogS := 'Search "' + OrigPath + '"';
    LogDatei.log_prog(LogS, LLInfo + logleveloffset);

    { start with the sub directories,
      and go with the recursion deeper and delete the lowest level first }
    if recursive then
    begin
      {$IFDEF WINDOWS}
      FindResultcode := FindFirstUTF8(OrigPath + '*.*', faAnyfile, SearchResult);
      {$ELSE WINDOWS}
      FindResultcode := FindFirstUTF8(OrigPath + '*', faAnyfile or
        faSymlink, SearchResult);
      {$ENDIF WINDOWS}
      while FindResultcode = 0 do
      begin
        LogDatei.log_prog('Found: ' + SearchResult.Name + ' with attr: ' +
          IntToStr(SearchResult.Attr), LLDebug2);
        if (SearchResult.Attr and faDirectory = faDirectory) and
          (SearchResult.Name <> '.') and (SearchResult.Name <> '..') then
          ExecDelete
          (OrigPath + SearchResult.Name + PathDelim + FileMask, DeleteDeeperDir);
        FindResultcode := FindNextUTF8(SearchResult);
      end;
      FindCloseUTF8(SearchResult);
    end;

    { now delete in the base directory }
    {$IFDEF WINDOWS}
    if Filemask = '' then
      Filemask := '*.*';
    {$ELSE}
    if (Filemask = '') or (Filemask = '*.*') then
      Filemask := '*';
    {$ENDIF WINDOWS}
    LogS := 'Search "' + OrigPath + Filemask + '"';
    LogDatei.log(LogS, LLInfo + logleveloffset);

    FindResultcode := FindFirstUTF8(OrigPath + Filemask, faAnyFile or
      faSymlink - faDirectory, SearchResult);

    while FindResultcode = 0 do
    begin
      Filename := OrigPath + SearchResult.Name;
      LogS := 'File "' + Filename + '"';
      LogDatei.log(LogS, LLdebug2);


      FileIsReadOnly := False;
      if ignoreReadonly then
      begin
        if GetReadOnlyAttribute(Filename, True) then
        begin
          { Elimination of readonly attribut did not work }
          FileIsReadonly := True;
          Result := False;
        end;
      end
      else
      begin
        if GetReadOnlyAttribute(Filename, False) then
        begin
          { Elimination of readonly attribut was not intended  }
          FileIsReadonly := True;
          Result := False;
          LogS := 'No deletion, file is readonly';
          LogDatei.log(LogS, LLNotice);
        end;
      end;

      shallDelete := not FileIsReadOnly;

      if shallDelete and (daysBack > 0) then
      begin
        ddiff :=
          DateTimeToTimeStamp(now).date - DateTimeToTimeStamp(
          FileDateToDateTime(searchResult.time)).date;

        shallDelete := (ddiff >= daysBack);

        LogS := 'The file is ' + IntToStr(ddiff) + ' day(s) old';
        if not shallDelete then
          LogS := LogS + ', no deletion';
        LogDatei.log(LogS, LLDebug2);
      end;

      if shallDelete then
      begin
        {$IFDEF WINDOWS}
        isdeleted := DeleteFileWithRetries(Filename);
        {$ELSE}
        isdeleted := SysUtils.DeleteFile(filename);
        {$ENDIF WINDOWS}

        if isdeleted then
        begin
          LogS := 'The file ' + Filename + ' has been deleted';
          LogDatei.log(LogS, LLInfo + logleveloffset);
        end
        else
        begin
          {$IFDEF WINDOWS}
          if retryOnReboot then
          begin
            errorNo := GetLastError;
            if (errorNo = 32) or (errorNo = 5) then
            begin
              LogDatei.log('Target file: ' + Filename +
                ' was in use, retry with DELAY_UNTIL_REBOOT.',
                LLDebug2);
              exist := PWChar(unicodestring(Filename));
              moveflags := MOVEFILE_DELAY_UNTIL_REBOOT;
              exitbool := MoveFileExW(exist, nil, moveflags);
              if exitbool then
              begin
                Result := True;
                RebootWanted := True;
                LogDatei.log(
                  'Target file was in use, move / rename should be completed after reboot.',
                  LLWarning);
              end
              else
                LogDatei.log(
                  'Target file was in use, retry with DELAY_UNTIL_REBOOT failed.',
                  LLError);
            end;
          end
          else
          {$ENDIF WINDOWS}
          begin
            //LogDatei.log('Warning: The file could not be deleted', LLWarning);
           {$IFDEF UNIX}
            LogS := 'Warning: "' + 'file ' + Filename +
              '" cannot be deleted, error ' + SysErrorMessage(fpgeterrno);
           {$ENDIF UNIX}
           {$IFDEF WINDOWS}
            errorNo := GetLastError;
            LogS := 'Warning: "' + 'file ' + Filename +
              '" cannot be deleted, error ' + IntToStr(errorNo) +
              ' ("' + RemoveLineBreaks(SysErrorMessage(errorNo)) + '")';
           {$ENDIF WINDOWS}
            LogDatei.log(LogS, LLWarning);
          end;
        end;
      end;


      FindResultcode := FindNextUTF8(SearchResult);
    end;

    FindCloseUTF8(SearchResult);

    if DeleteDir then
      // zum Schluss der Behandlung einer Verzeichnisebene
      //   gegebenfalls das Verzeichnis selbst verschwinden lassen
    begin
      system.Delete(OrigPath, length(OrigPath), 1);

      if removedir(OrigPath) then
      begin
        LogS := 'Directory "' + OrigPath + '" deleted';
        LogDatei.log(LogS, LLInfo + logleveloffset);
      end
      else
      begin
        {$IFDEF UNIX}
        LogS := 'Warning: "' + 'Directory ' + OrigPath +
          '" cannot be deleted, error ' + SysErrorMessage(fpgeterrno);
        {$ENDIF UNIX}
        {$IFDEF WINDOWS}
        errorNo := GetLastError;
        LogS := 'Warning: "' + 'Directory ' + OrigPath +
          '" cannot be deleted, error ' + IntToStr(errorNo) + ' ("' +
          RemoveLineBreaks(SysErrorMessage(GetLastError)) + '")';
        {$ENDIF WINDOWS}
        LogDatei.log(LogS, LLWarning);
        {$IFDEF WINDOWS}
        if retryOnReboot then
        begin
          errorNo := GetLastError;
          if (errorNo = 32) or (errorNo = 145) then
          begin
            LogDatei.log('Target dir: ' + Filename +
              ' was in use, retry with DELAY_UNTIL_REBOOT.',
              LLDebug2);
            exist := PWChar(unicodestring(OrigPath));
            moveflags := MOVEFILE_DELAY_UNTIL_REBOOT;
            exitbool := MoveFileExW(exist, nil, moveflags);
            if exitbool then
            begin
              Result := True;
              RebootWanted := True;
              LogDatei.log(
                'Target dir was in use, move / rename should be completed after reboot.',
                LLWarning);
            end
            else
              LogDatei.log(
                'Target dir was in use, retry with DELAY_UNTIL_REBOOT failed.',
                LLError);
          end;
        end;
          {$ENDIF WINDOWS}
        Result := False;
      end;
    end;



  end; // ExecDelete

begin
  Result := True;
  { Filename processing }
  PathName := ExtractFilePath(CompleteName);
  //CompleteName := ExpandFileName (Filename);
  { changed because it is dangerous }
  testname := IncludeTrailingPathDelimiter(lowercase(Filename));
  if (testname = 'c:\') or (testname = 'c:\windows\') or
    (testname = 'c:\windows\system32\') or (testname = '/Applications/') or
    (testname = 'c:\program files\') or (testname = 'c:\program files (x86)\') or
    (Filename = PathDelim) or (Filename = '\\') or (1 = pos('\', Filename)) then
  begin
    LogDatei.log('By policy we will not delete: ' + Filename, LLError);
    CompleteName := '';
  end
  else
    CompleteName := Filename;


       {  if (FileGetAttr (CompleteName) and faDirectory = faDirectory)
       does not work!! }
  if isDirectory(CompleteName) and (CompleteName[length(CompleteName)] <>
    PathDelim) then
    CompleteName := CompleteName + PathDelim;

  if recursive then
    LogS := 'Delete recursively'
  else
    LogS := 'Delete';
  LogS := LogS + ' "' + CompleteName + '"';
  LogDatei.log_prog(LogS, LLDebug);

  Filemask := ExtractFileName(CompleteName);

  DeleteStartDir := False;
  DeleteDeeperDir := False;
  if (Filemask = '') or (Filemask = '*.*') or (Filemask = '*') then
    DeleteDeeperDir := True;
  if Filemask = '' then
    DeleteStartDir := True;
  if (Filemask = '*.*') or (Filemask = '*') or
    (ExtractFileNameOnly(Filemask) = '*') or (ExtractFileExt(Filemask) = '*') then
    testname := ExtractFilePath(CompleteName)
  else
    testname := CompleteName;
  LogDatei.log_prog('testname: ' + testname + ' ' + {$INCLUDE %LINE%}, LLDebug);
  { Start }
  if not search4file then
  begin
    // FindFirst only finds directories without PathDelim at the end
    if testname <> '' then
    begin
      if (testname[length(testname)] = PathDelim) then
      begin
        testname := ExtractFileDir(testname);
      end;
    end
    else
      LogDatei.Log('Files section "del" command. No file or directory path given in the script. Please check your script', LLWarning);
    { new del syntax: "del -s c:\not-existing" will do nothing (if not existing) }
    if not (FindFirst(testname, faAnyFile, FileFinder) = 0) then
    begin
      { does not exist }
      LogS := 'Notice: ' + 'File or Directory ' + CompleteName +
        ' does not exist, nothing deleted';
      LogDatei.log(LogS, LLInfo + logleveloffset);
    end
    else
    begin
      { does exist as dir or file }
      if isDirectory(ExtractFilePath(CompleteName)) then
      begin
        ExecDelete(CompleteName, DeleteStartDir);
      end
      else
      begin
        { is a existing file }
        if SysUtils.DeleteFile(CompleteName) then
        begin
          LogS := 'The file has been deleted';
          LogDatei.log(LogS, LLInfo + logleveloffset);
        end
        else
        begin
          {$IFDEF WINDOWS}
          if retryOnReboot then
          begin
            errorNo := GetLastError;
            if (errorNo = 32) or (errorNo = 5) then
            begin
              LogDatei.log('Target file: ' + Filename +
                ' was in use, retry with DELAY_UNTIL_REBOOT.',
                LLDebug2);
              exist := PWChar(unicodestring(Filename));
              moveflags := MOVEFILE_DELAY_UNTIL_REBOOT;
              exitbool := MoveFileExW(exist, nil, moveflags);
              if exitbool then
              begin
                Result := True;
                RebootWanted := True;
                LogDatei.log(
                  'Target file was in use, move / rename should be completed after reboot.',
                  LLWarning);
              end
              else
                LogDatei.log(
                  'Target file was in use, retry with DELAY_UNTIL_REBOOT failed.',
                  LLError);
            end;
          end
          else
          {$ENDIF WINDOWS}
          LogS := 'Warning: The file could not be deleted';
          LogDatei.log(LogS, LLWarning);
        end;
      end;
    end;
    FindClose(FileFinder);
  end
  else { old delete syntax: "delete -s c:\not-existing" will scan the harddisk for "not-existing" }
  begin
    if isDirectory(ExtractFilePath(CompleteName)) then
      ExecDelete(CompleteName, DeleteStartDir)
    else
    begin
      LogS := 'Notice: ' + 'Directory ' + ExtractFilePath(CompleteName) +
        ' does not exist, nothing deleted';
      LogDatei.log(LogS, LLInfo + logleveloffset);
    end;
  end;
end;


function ProduceArchiveName(const fname: string): string;
var
  Ext: string[4];
begin
  Ext := ExtractFileExt(fname);
  if Ext = '' then
    Ext := '.';
  Ext := copy(Ext, 1, 3) + '#';
  Result := ChangeFileExt(fname, Ext);
end;

function ProduceSourceMask(const fname: string): string;
var
  Ext: string = '';
begin
  Ext := ExtractFileExt(fname);
  if length(Ext) >= 4 then
    Ext := copy(Ext, 1, 3) + '*';
  Result := ChangeFileExt(fname, Ext);
end;

{$IFDEF WINDOWS}
procedure TuibFileInstall.AllCompress
  (const SourceMask, TargetDir: string; Recursive: boolean;
  MaintainEmptySubdirs: boolean);
type
  TCompressModus = (tcmZip, tcmLZH, tcmCount, tcmNull);
var
  FileFound: boolean;
  TotalNoOfFiles, NoOfFiles: integer;
  CompressModus: TCompressModus;


  procedure CompressRecursive
    (SourceMask, TargetDir: string; var FileFound: boolean; Recursive: boolean;
  var CompressModus: TCompressModus);

  var
    CompleteName, Extension, SourcePath, SourceFilemask, SourceName,
    TargetPath, TargetName: string;
    FindResultcode: integer;
    FileAttr: integer;
    SearchResult: TSearchRec;
    FileFoundOnADeeperLevel, FileFoundOnThisLevel: boolean;
    DirectoryExisted: boolean;
    DirectoryError: integer;
    CmdLinePasStr: string;
    ReturnCode: longint = 0;

  begin
    FileFoundOnThisLevel := False;


    // SourceMask auf Standardform bringen
    CompleteName := ExpandFileName(SourceMask);
    SourcePath := ExtractFilePath(CompleteName);
    if SourcePath[length(SourcePath)] <> PathDelim then
      SourcePath := SourcePath + PathDelim;
    SourceFilemask := ExtractFileName(CompleteName);
    {$IFDEF WINDOWS}
    if SourceFilemask = '' then
      SourceFilemask := '*.*';
    {$ELSE}
    if (SourceFilemask = '') or (SourceFilemask = '*.*') then
      SourceFilemask := '*';
    {$ENDIF WINDOWS}




    (* TargetMask auf Standardform bringen *)
    CompleteName := ExpandFileName(TargetDir);
    TargetPath := CompleteName;
    if TargetPath[length(TargetPath)] <> PathDelim then
      TargetPath := TargetPath + PathDelim;

    if (length(TargetPath) > 1) and (TargetPath[length(TargetPath) - 1] = ':') then
    (* TargetPath is e.g. c:\, that is, not a file but a volume identifier *)
    else
    begin
      CompleteName := copy(TargetPath, 1, length(TargetPath) - 1);


      if fileGetAttr(CompleteName) and faDirectory = 0 then
      begin
        LogS := 'Error:  ' + CompleteName +
          ' is no directory and cannot function as target';
        ;
        LogDatei.log(LogS, LLError);
        exit;
      end;
    end;



    FindResultcode := FindFirstUTF8(SourcePath + SourceFilemask,
      faAnyfile - faDirectory - faVolumeId, SearchResult);

    if FindResultcode = 0 then
      FileFoundOnThisLevel := True;


    while FindResultcode = 0 do
    begin
      SourceName := ProduceSourceMask(SourcePath + SearchResult.Name);
      (* SourceName kann mehrfach vorgenommen,
         Kontrolle ueber eine Liste der vorgefundenen Namen scheint mir
         aufwendiger als die seltene mehrfach durchgefuehrte Kompression *)

      TargetName := ProduceArchiveName(TargetPath + SearchResult.Name);

      if SourceName = TargetName then
      begin
        LogS := 'Warning: ' + ' Target (' + TargetName + ') = Source ';
        Logdatei.Log(LogS, LLwarning);
      end
      else
      begin

        case CompressModus of
          tcmZip:
          begin
            LogS := 'compressing ' + Sourcename + ' -----> ' + Targetname;
            Logdatei.Log(LogS, LLinfo);


            CmdLinePasStr := (* 'cmd.exe /C echo'; *)  ExtractFilePath(ParamStr(0)) +
              'zip32.exe';
            if not FileExists(CmdLinePasStr) then

              CmdLinePasStr :=
                (* 'cmd.exe /C echo'; *)  ExtractFilePath(ParamStr(0)) + 'zip.exe';



            if not FileExists(CmdLinePasStr) then
            begin
              LogS := 'Error: zip.exe bzw. zip32.exe im Programmverzeichnis nicht gefunden';
              LogDatei.log(LogS, LLError);
              exit;
            end;

            CmdLinePasStr := CmdLinePasStr + ' -j  "' + TargetName +
              '"  "' + Sourcename + '"';
            (* -j : Speichern ohne Pfad *)

            if StartProcess(CmdLinePasStr, sw_minimize, True,
              False, False, False, '', 0, LogS, ReturnCode) then
            begin

              Logdatei.Log(LogS, LLinfo);

            end
            else
            begin
              LogS := 'Error: ' + LogS;
              LogDatei.log(LogS, LLError);
            end;
          end;
          tcmLZH:
          begin
          end;
          tcmCount:
            Inc(TotalNoOfFiles);
        end;

        Inc(NoOfFiles);
        if compressModus <> tcmCount then
        begin
          FBatchOberflaeche.SetProgress(round(NoOfFiles / TotalNoOfFiles * 100),
            pPercent);
          ProcessMess;
        end;
      end;

      FindResultcode := FindNextUTF8(SearchResult);
    end;

    FindCloseUTF8(SearchResult);


   (* Subdirectories im Source-Verzeichnis suchen
     und gegebenfalls im Targetverzeichnis neu anlegen *)

    FindResultcode := FindFirstUTF8(SourcePath + '*.*', faAnyFile, SearchResult);


    while FindResultcode = 0 do
    begin
      if (SearchResult.Attr = faDirectory) and (SearchResult.Name <> '.') and
        (SearchResult.Name <> '..') then
      begin
        DirectoryError := 0;

        TargetName := TargetPath + SearchResult.Name;

        FileAttr := FileGetAttr(TargetName);

        if (FileAttr >= 0) then
        begin
          if FileAttr and faDirectory = faDirectory then
          begin
            DirectoryExisted := True;
          end
          else
          begin
            DirectoryError := 1;
            LogS := 'Error:  ' + TargetName +
              ' exists als filename and cannot be created as directory';
            LogDatei.log(LogS, LLError);
          end;
        end
        else
        begin
          DirectoryExisted := False;
          try
            mkdir(TargetName);
          except
            LogS :=
              'Error: ' + 'missing directory ' + TargetName +
              ' cannot be created';
            LogDatei.log(LogS, LLError);
            DirectoryError := 2;
          end;
        end;


        FileFoundOnADeeperLevel := False;

        if Recursive and (DirectoryError = 0) then
          (* Rekursion *)
        begin
          CompressRecursive
          (SourcePath + SearchResult.Name + PathDelim + SourceFileMask,
            TargetName, FileFoundOnADeeperLevel, Recursive, CompressModus);
          if FileFoundOnADeeperLevel then
            FileFoundOnThisLevel := True;
        end;

        if not FileFoundOnADeeperLevel and not DirectoryExisted and
          (DirectoryError = 0) and not MaintainEmptySubdirs then
          (* angelegtes directory wieder entfernen *)
          rmdir(TargetName);
      end;

      FindResultcode := FindNextUTF8(SearchResult);
    end;

    FindCloseUTF8(SearchResult);

    FileFound := FileFoundOnthisLevel (* resp. on a deeper level *);
  end;

begin
  FileFound := False;

  LogS := 'Zipping  ' + SourceMask + ' -----> ' + TargetDir;
  Logdatei.Log(LogS, LLinfo);



  (* FBatchOberflaeche.Gauge1.progress := 0; *)
  TotalNoOfFiles := 0;
  NoOfFiles := 0;
  CompressModus := tcmCount;

  CompressRecursive
  (SourceMask, TargetDir, FileFound, Recursive, CompressModus);

  NoOfFiles := 0;
  CompressModus := tcmZip;

  if (TotalNoOfFiles > 0) and (CompressModus <> tcmNull) then
    CompressRecursive
    (SourceMask, TargetDir, FileFound, Recursive, CompressModus);



end;

{$ENDIF WINDOWS}

(* Hilfsfunktionen *)

function GetStrCitation(s: PChar; var Citation: string; var r: PChar): boolean;
var
  i, n: cardinal;
  s0: PChar;
  s1: array [0..255] of char;

begin

  s0 := StrScan(s, CitMark);
  if s0 = nil then
  begin
    Result := False;
    r := nil;
    Citation := '';
    exit;
  end;

  n := StrLen(s0);

  if n < 2 then
  begin
    Result := False;
    r := nil;
    Citation := '';
    exit;
  end;

  i := 1;
      (* CitMark steht an Stelle 0, da s0 Ergebnis eines StrScan ist
         Suche des korrespondierenden CitMark *)

  while (i <= n - 1) and (s0[i] <> CitMark) do
    Inc(i);


  if i = n then
    (* Suche wurde nicht fuendig *)
  begin
    Result := False;
    r := nil;
    Citation := '';
  end
  else
  begin
    Result := True;
    StrMove(s1, s0, i + 1);
    s1[i + 1] := #0;
    Citation := StrPas(s1);
    r := @(s0[i + 1]);
  end;

end;

function GetPLine(s: PChar; var Line: PChar; var s1: PChar): boolean;
var
  i, n: cardinal;
  s0: array [0..255] of char;

begin

  n := StrLen(s);

  if n = 0 then
  begin
    Result := False;
    Line := nil;
    s1 := nil;
  end
  else
  begin
    i := 0;
    while (i <= n - 1) and (s[i] + s[i + 1] <> #13#10) do
      Inc(i);

    StrMove(s0, s, i);
    s0[i] := #0;
    Line := @(s0[0]);

    if i >= n - 1 then
    begin
      Result := False;
      s1 := nil;
    end
    else
    begin
      Result := True;
      s1 := @(s[i + 2]);
    end;
  end;
end;

function DeleteCitationmarks(var s: string): string;
begin
  if (s[1] = '"') and (s[length(s)] = '"') then
    Result := copy(s, 2, length(s) - 2);
end;

{$IFDEF WINDOWS}
{ TuibShellLinks }
constructor TuibShellLinks.Create;
begin
  inherited Create;
  CoInitialize(nil);
  FolderOpened := False;
  MySystemFolder := CSIDL_COMMON_PROGRAMS;
  MyFoldername := 'default';
  MyFolderPath := '';
  MyFiles := TuibFileInstall.Create;
  StartIndentLevel := LogDatei.LogSIndentLevel;


  (*
      MySystemFolder := CSIDL_STARTMENU;
      MySystemFolder := CSIDL_PROGRAMS;
      MySystemFolder := CSIDL_DESKTOPDIRECTORY;
      MySystemFolder := CSIDL_STARTUP;
      MySystemFolder := CSIDL_FAVORITES;
      MySystemFolder := CSIDL_COMMON_STARTMENU;
      MySystemFolder := CSIDL_COMMON_PROGRAMS;
      MySystemFolder := CSIDL_COMMON_DESKTOPDIRECTORY;
      MySystemFolder := CSIDL_COMMON_STARTUP;
      MySystemFolder := CSIDL_COMMON_FAVORITES;
  *)
end;


destructor TuibShellLinks.Destroy;
begin
  CoUnInitialize;
  ;
  MyFiles.Free;
  // MyMessageDlg.WiMessage ('destroy uibShellLinks ', [mrOK]);
  inherited Destroy;
end;

function TuibShellLinks.Tell_SystemFolder(const Systemfolder: integer): string;
begin
  Result := 'the ';

  case Systemfolder of
    CSIDL_DESKTOP: Result := Result + 'DESKTOP folder';
    CSIDL_SENDTO: Result := Result + 'SENDTO folder';
    CSIDL_STARTMENU: Result := Result + 'STARTMENU folder';
    CSIDL_PROGRAMS: Result := Result + 'PROGRAMS folder';
    CSIDL_DESKTOPDIRECTORY: Result := Result + 'DESKTOPDIRECTORY folder';
    CSIDL_STARTUP: Result := Result + 'STARTUP folder';
    CSIDL_FAVORITES: Result := Result + 'FAVORITES folder';
    CSIDL_COMMON_STARTMENU: Result := Result + 'COMMON STARTMENU folder';
    CSIDL_COMMON_PROGRAMS: Result := Result + 'COMMON PROGRAMS folder';
    CSIDL_COMMON_DESKTOPDIRECTORY: Result := Result + 'COMMON DESKTOPDIRECTORY folder';
    CSIDL_COMMON_STARTUP: Result := Result + 'COMMON STARTUP folder';
    CSIDL_COMMON_FAVORITES: Result := Result + 'COMMON FAVORITES folder';
    else
      Result := 'no systemfolder'
  end;
end;


function TuibShellLinks.OpenShellFolderPath(const Systemfolder: integer;
  const foldername: string): boolean;
var
  oldFolderPath: string = '';
  pidl: PItemIdList;

begin
  Result := True;
  oldFolderPath := myFolderPath;
  if FolderOpened then
    (* some folder was already been referred *)


  if not SUCCEEDED(SHGetSpecialFolderLocation(0, SystemFolder, pidl)) then
  begin
    Result := False;
    LogS := 'Error ' + Tell_SystemFolder(SystemFolder) + ' could not be located.';
    LogDatei.log(LogS, LLError);
  end
  else
  begin
    // Umformung: pidl -> FolderPath:
    SetLength(MyFolderPath, max_path);
    SHGetPathFromIDListW(pidl, pwidechar(MyFolderPath));
    SetLength(myFolderPath, strlen(pwidechar(MyFolderPath)));

    // Eventuell einen neuen Ordner erzeugen:

    MyFolderPath := MyFolderPath + PathDelim + foldername;
    if DirectoryExists(MyFolderPath) then
    begin
      LogS := 'Opened "' + foldername + '" in ' + Tell_SystemFolder(SystemFolder);
      Logdatei.Log(LogS, LLinfo);
    end

    else
      try
        ForceDirectories(MyFolderPath);
        LogS := 'Created "' + foldername + '" in ' + Tell_SystemFolder(SystemFolder);
        Logdatei.Log(LogS, LLinfo);
      except
        Result := False;
        LogS := 'Error: "' + foldername + '"  could not be created in ' +
          Tell_SystemFolder(SystemFolder);
        LogDatei.log(LogS, LLError);
      end;
  end;

  if Result then
  begin
    MyFoldername := Foldername;
    MySystemFolder := Systemfolder;
    FolderOpened := True;
  end
  else
    myFolderPath := oldFolderPath;


end;


function TuibShellLinks.MakeShellLink
  (const description, thePath, commandline_arguments, working_directory,
  iconPath: string;
  const icon_index: integer): boolean;
begin
  Result := MakeShellLink(description, thePath, commandline_arguments,
    working_directory, iconPath, icon_index, 0);
end;

function TuibShellLinks.MakeShellLink
  (const description, thePath, commandline_arguments, working_directory,
  iconPath: string;
  const icon_index: integer; shortcut: word): boolean;
begin
  Result := MakeShellLink(description, thePath, commandline_arguments,
    working_directory, iconPath, icon_index, 0, 0);
end;

function TuibShellLinks.MakeShellLink
  (const description, thePath, commandline_arguments, working_directory,
  iconPath: string;
  const icon_index: integer; shortcut: word; showwindow: integer): boolean;

const
  IID_IPersistFile: TGUID = (D1: $10B; D2: 0; D3: 0;
    D4: ($C0, $00, $00, $00, $00, $00, $00, $46));

var
  ShellLink: IShellLinkW;
  LinkFile: IPersistFile;
  widestr: pwidechar;
  widefilename: WideString;
  unicodefilename: unicodestring;
  Filename: string = '';
  Filename_system: string = '';
  winresult: HRESULT;
  Linkname: string = '';
  LinkExisted: boolean;
begin
  Linkname := '"' + description + '"';
  Result := False;

  winresult := CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER or
    CLSCTX_INPROC_HANDLER or CLSCTX_LOCAL_SERVER, IID_IShellLinkW, ShellLink);

  if not (winresult = s_ok) then
    LogS := 'Error: ShellLink Interface could not be created.'
  else
  begin
    LogDatei.log('try to set folderlink: ' + thePath + ' - ' + Description, LLDebug2);
    ShellLink.SetPath(PWchar(unicodestring(thePath)));
    ShellLink.SetDescription(PWchar(unicodestring(Description)));
    ShellLink.SetArguments(PWchar(unicodestring(commandline_arguments)));
    ShellLink.SetWorkingDirectory(PWchar(unicodestring(working_directory)));
    ShellLink.SetIconLocation(PWchar(unicodestring(iconPath)), icon_index);
    if shortcut > 0 then
    begin
      ShellLink.SetHotkey(shortcut);
      LogDatei.log('try to set shortcut: ' + IntToStr(shortcut), LLDebug2);
    end;
    if showwindow > 0 then
    begin
      ShellLink.SetShowCmd(showwindow);
      LogDatei.log('try to set showwindow: ' + IntToStr(showwindow), LLDebug2);
    end;

    (*
    ShellLink.SetPath(Pchar(ConvertEncoding(thePath,'utf8','system')));
    ShellLink.SetDescription(Pchar(ConvertEncoding(Description,'utf8','system')));
    ShellLink.SetArguments(Pchar(ConvertEncoding(commandline_arguments,'utf8','system')));
    ShellLink.SetWorkingDirectory(Pchar(ConvertEncoding(working_directory,'utf8','system')));
    ShellLink.SetIconLocation(Pchar(ConvertEncoding(iconPath,'utf8','system')), icon_index);
    *)
    // Setzen: LinkFile (Interface-Variable):
    winresult := ShellLink.QueryInterface(IID_IPersistFile, LinkFile);
    if not (winresult = s_ok) then
      LogS := 'Error: QueryInterface failed.'
    else
    begin
      GetMem(widestr, max_path * 2);
      try
        FileName := Description + '.LNK';
        FileName := MyFolderPath + PathDelim + FileName;
        LogDatei.log('Check for folderlink: ' + Filename, LLDebug2);
        if FileExists(Filename) then
          LinkExisted := True
        else
          LinkExisted := False;
        // Umformung: FileName -> widestr
        //Filename_system := ConvertEncoding(filename,'utf8','system');
        Filename_system := UTF8ToWinCP(filename);
        MultiByteToWideChar(CP_ACP, 0, PChar(Filename_system), -1, widestr, max_path);
        //widefilename := ConvertEncoding(filename,'utf8','ucs2be');
        //winresult := LinkFile.Save(widestr, True);
        unicodefilename := unicodestring(FileName);
        winresult := LinkFile.Save(widestr, True);
        if not (winresult = s_ok) then
        begin
          LogS := 'Error: LinkFile.Save failed.';
          logdatei.log('Error: Creation of folderlink: ' + FileName +
            ' failed.', LLError);
          logdatei.log('Error: Creation of folderlink: ' + widestr +
            ' failed.', LLError);
          logdatei.log('Error: Creation of folderlink: ' + unicodefilename +
            ' failed.', LLError);
        end
        else
          Result := True;
      finally
        FreeMem(widestr);
      end;
    end;
  end;

  if Result then
  begin
    if LinkExisted then
      LogS := ' overwritten'
    else
      LogS := ' created';
    LogS := 'ShellLink ' + Linkname + LogS;
    Logdatei.Log(LogS, LLinfo);
  end
  else
  begin
    LogS := LogS + ' System message "' + RemoveLineBreaks(
      SysErrorMessage(winresult)) + '"';
    Logdatei.Log(LogS, LLinfo);
    LogS := 'ShellLink ' + Linkname + ' could not be made';
    LogDatei.log(LogS, LLError);
  end;

end;


function TuibShellLinks.ShowShellFolderWindow: boolean;
var
  report: string = '';
  returncode: longint = 0;
begin
  Result := False;
  if not FolderOpened then
  begin
    LogS := 'Error: no folder opened ';
    LogDatei.log(LogS, LLError);
  end
  else
  begin
    if not StartProcess('explorer.exe "' + MyFolderPath + '"', sw_Show,
      False, False, False, False, '', 0, Report, ReturnCode) then
    begin
      LogS := 'Error: ' + Report;
      LogDatei.log(LogS, LLError);
    end
    else
    begin
      LogS := 'Show FolderWindow "' + MyFolderPath + '"';
      Logdatei.Log(LogS, LLinfo);
      Result := True;
    end;
  end;
end;


function TuibShellLinks.DeleteShellLink(const description: string): boolean;
var
  Filename: string = '';
  Linkname: string = '';
begin
  Linkname := '"' + description + '"';
  if not FolderOpened then
  begin
    LogS := 'Error: no folder opened ';
    LogDatei.log(LogS, LLError);
    Result := False;
  end
  else
  begin
    Result := True;
    Filename := MyFolderPath + PathDelim + description;
    if Copy(Filename, Length(Filename)-3) <> '.url' then
      Filename := Filename + '.lnk';
    if not SysUtils.FileExists(Filename) then
    begin
      LogS := 'Info: Link ' + Linkname + ' does not exist';
      Logdatei.Log(LogS, LLinfo);
      LogDatei.NumberOfHints := LogDatei.NumberOfHints + 1;
      Result := False;
    end;

    if Result then
    begin
      if DeleteFile(Filename) then
      begin
        LogS := 'Link ' + Linkname + ' deleted';
        Logdatei.Log(LogS, LLinfo);
      end
      else
      begin
        LogS := 'Error: ' + Linkname + ' could not be deleted, Errorno. ' +
          IntToStr(GetLastError) + ' ("' + RemoveLineBreaks(
          SysErrorMessage(GetLastError)) + '")';
        LogDatei.log(LogS, LLError);
        Result := False;
      end;
    end;
  end;
end;

function TuibShellLinks.DeleteShellFolder
  (const SystemFolder: integer; const foldername: string): boolean;

var
  FolderPath0: WideString = '';
  FolderPath: string = '';
  pidl: PItemIdList;

begin
  Result := False;

  if not SUCCEEDED(SHGetSpecialFolderLocation(0, SystemFolder, pidl)) then
  begin
    LogS := 'Error: Systemfolder ' + Tell_SystemFolder(SystemFolder) + ' not found';
    LogDatei.log(LogS, LLError);
  end
  else
  begin
    // Umformung: pidl -> FolderPath:
    SetLength(FolderPath0, max_path);
    SHGetPathFromIDListW(pidl, pwidechar(FolderPath0));
    SetLength(FolderPath0, strlen(pwidechar(FolderPath0)));

    FolderPath := FolderPath0 + '\' + foldername;
    if not DirectoryExists(FolderPath) then
    begin
      LogS := 'Folder "' + foldername + '" in ' + Tell_SystemFolder(SystemFolder) +
        ' does not exist, nothing to delete';
      Logdatei.Log(LogS, LLinfo);
      Result := True;
    end
    else
    begin
      LogS := 'Delete folder "' + foldername + '" in ' +
        Tell_SystemFolder(SystemFolder);
      Logdatei.Log(LogS, LLinfo);



      if MyFiles.AllDelete(FolderPath, True, True, 0) then
      begin
        Result := True;
        if FolderOpened and (FolderPath = MyFolderPath) then
          FolderOpened := False;
      end;

    end;
  end;
end;

{$ENDIF WINDOWS}

function isSymLink(filepath: string): boolean;
var
  fileinfo: TSearchRec;
  errorinfo: string;
begin
  Result := False;
  filepath := GetForcedPathDelims(filepath);
  LogDatei.log('resolving symlink: ' + filepath, LLinfo);
  if GetFileInfo(filepath, fileinfo, errorinfo) then
  begin
    if (fileinfo.Attr and fasymlink) = fasymlink then
      Result := True;
  end;
  //result := FileIsSymlink(filepath) ;
end;


function resolveSymlink(const filepath: string; recursive: boolean = True): string;
var
  fileinfo: TSearchRec;
  outpath: string;
begin
  Result := filepath;
  {$IFDEF WINDOWS}
  Result := resolveWinSymlink(filepath);
  //result := execPowershellCall(filepath, '', 1, True, False, tmpbool1).Text;
  {$ENDIF WINDOWS}
  {$IFDEF UNIX}
  Result := resolveUnixSymlink(filepath);
  {$ENDIF UNIX}
end;

function isNumeric(s: string): boolean;
var
  i: real;
begin
  Result := TryStrToFloat(s, i);
end;

function isBoolean(s: string): boolean;
var
  i: boolean;
begin
  Result := TryStrToBool(s, i);
end;

function GetFQDN: string;
begin
  {$IFDEF WINDOWS}
  Result := GetFQDNfromWMI;
  {$ENDIF WINDOWS}
  {$IFDEF UNIX}
  Result := GetFQDNUnix;
  {$ENDIF UNIX}
  if not isValidFQDN(Result) then
    LogDatei.log('"' + Result + '"' + ' is no valid fqdn', LLNotice);
end;

procedure noLockSleep(const Milliseconds: DWord);
// from
// https://www.delphi-treff.de/tipps-tricks/system/prozesse/anwendung-fuer-eine-bestimmte-zeit-pausieren/
var
  FirstTickCount: DWord;
begin
  FirstTickCount := GetTickCount;
  while ((GetTickCount - FirstTickCount) < Milliseconds) do
  begin
    ProcessMess;
    Sleep(0);
  end;
end;

(*
 function GetFileInfo(const CompleteName: string; var fRec: TSearchRec;
  var ErrorInfo: string): boolean;
  GetFinalPathNameByHandle()
*)

{$IFDEF WINDOWS}
function DeleteFileWithRetries(filename: string): boolean;

const
  maxretries: integer = 10;     // maximum number of (re)tries
  sleepms:    integer = 2;      // milliseconds sleep time
  retryErrorNumbers   = [5,32]; // retry if one of these errors

var
  countdown: integer;           // for retries
  errorNo:   integer;           // from GetLastError
  logtext:   string = '';       // for debug logging
begin
  Result := False;
  countdown := maxretries;
  // delete with retries
  while ((not SysUtils.DeleteFile(filename)) and (countdown > 0)) do
  begin
    // delete failed
    errorNo := GetLastError;  // get error code
    if errorNo in retryErrorNumbers then
    begin
      countdown := countdown-1;
      if (countdown > 0) then Sleep(sleepms); // sleep before next retry
    end
    else
    begin
       // other error code => stop retries
       countdown := -1;
    end;
  end;
  if (countdown > 0) then Result := True;
  //generate log
  if LogDatei.debug_prog then
  begin
    logtext   := 'delete file ' + filename;
    case countdown of
      -1: logtext += ' - failed with error code ' + IntToStr(errorNo);
       0: logtext += ' - still failed after doing retries for '
                       + IntToStr((maxretries-1) * sleepms) +' ms';
      else
      begin
        logtext += ' - was successful';
        // logging if retries were needed
        if (countdown < maxretries) then
        begin
          logtext += ', retries: '+IntToStr(maxretries - countdown)+' with '
                     + IntToStr(sleepms) +' ms sleep, total: '
                     + IntToStr((maxretries - countdown) * sleepms) +' ms';
        end;
      end;
    end;
    Logdatei.log_prog(logtext, LLDebug);
  end;
end;
{$ENDIF WINDOWS}

end.
