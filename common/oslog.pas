unit oslog;

// Defines global logging

// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: Rupert Roeder, detlef oertel


{$MODE Delphi}
{$RANGECHECKS ON}
{$OVERFLOWCHECKS ON}
{$IOCHECKS ON}
{$OBJECTCHECKS ON}
{$VARSTRINGCHECKS ON}

{$IFDEF OPSISCRIPT} {$DEFINE PARTLOG} {$ENDIF}
{$IFDEF OPSISCRIPT} {$DEFINE OPSI} {$ENDIF}
{$IFDEF OPSISCRIPTSTARTER} {$DEFINE PARTLOG} {$ENDIF}
{$IFDEF OPSISCRIPTSTARTER} {$DEFINE OPSI} {$ENDIF}


interface

uses
{$IFDEF GUI}
  Controls,
  LCLIntf,
  Forms,
  Dialogs,
  //osGUIControl,
{$ENDIF GUI}
{$IFDEF OPSISCRIPT}
  osconf,
  //osdefinedfunctions,
  {$IFDEF GUI}
  osGUIControl,
  {$ENDIF GUI}
{$ENDIF OPSISCRIPT}
{$IFDEF WINDOWS}
  registry,
  shlobj,
{$ENDIF WINDOWS}
{$IFDEF UNIX}
  baseUnix,
  cwstring,
  clocale,
{$ENDIF LINUX}
  //lconvencoding,
  osencoding,
  osfilehelper,
  Classes,
  //idsyslogMessage,
  SysUtils,
  fileutil,
  lazfileutils,
  lconvencoding,
  LazUTF8;

type
  //TRemoteErrorLogging = (trel_none, trel_filesystem, trel_syslog);
  // in registry values 0, 1, 2

  TLogInfo = class(TObject)
  private
    FLogLevel: integer;
    FLogSIndentLevel: integer;
    FNumberOfWarnings: integer;
    FNumberOfErrors: integer;
    FNumberOfHints: integer;
    FErrorNumberMarked: integer;
    FFileName: string;
    FMainFileName: string;
    FPartFileName: string;
    FPartReadFileName: string;
    FErrorsFilename: string;
    FErrorsFileExists: boolean;
    FHistoryFilename: string;
    FHistoryFileExists: boolean;
    FWriteComponentFile: boolean;
    FComponentFileExists: boolean;
    FComponentFilename: string;
    FRemoteErrorsPath: string;
    FRemoteErrorsFilename: string;
    FRemoteErrorsFileExists: boolean;
    FReportErrorsToUser: boolean;
    FTraceMode: boolean;
    FLogFileExists: boolean;
    FPartLogFileExists: boolean;
    FRemoteFileLogging: string;
    FOpsiscriptVersionRequired: string;
    FLogcallcount: integer;
    FAppendmode: boolean;
    FActionProgress: string;
    FAktProduktId: string;
    FConfidentialStrings: TStringList;
    FLogProduktId: boolean;
    FStandardPartLogFilename: string;
    FStandardLogFilename: string;
    FStandardLogFileext: string;
    FWritePartLog: boolean;
    FWriteHistFile: boolean;
    FWriteErrFile: boolean;
    Fdebug_prog: boolean;
    Fdebug_lib: boolean;
    Fforce_min_loglevel: integer;
    Fdefault_loglevel: integer;
    FStandardLogPath: string;
    FStandardMainLogPath: string;
    FStandardPartLogPath: string;
    FUsedLogLevel: integer;
    FNoLogFiles: TStringList;


  protected
    LogMainFile: Text;
    LogPartFile: Text;
    LogPartReadFile: Text;
    ErrorFile: Text;
    RemoteErrorFile: Text;
    LogMainFileF: THandle;
    LogPartFileF: THandle;
    LogPartReadFileF: THandle;
    ErrorFileF: THandle;
    RemoteErrorFileF: THandle;
    HistroyFile: Text;
    HistroryFileF: THandle;
    ComponentFile: Text;
    ComponentFileF: THandle;


  public
    constructor Create;
    destructor Destroy; override;
    procedure initiate(const Fname: string; Trace: boolean);
    procedure Empty;
    procedure Reopen;
    procedure PartReopen;
    procedure Close;
    procedure DeletePartLog;
    procedure WriteLogLine(myfile: THandle; line: string);
    //class procedure DumpToLog(const S: string);
    procedure OpenForReading;
    procedure PartOpenForReading;
    procedure PartCloseFromReading;
    procedure PartCopyToRead;
    function getLine(var S: string): boolean;
    function getPartLine(var S: string): boolean;
    function log(const S: string; LevelOfLine: integer): boolean;
    function log_prog(const S: string; LevelOfLine: integer): boolean;
    function log_list(const list: TStrings; LevelOfLine: integer): boolean;
    function log_exception(E: Exception; LevelOfLine: integer): boolean;
    procedure setLogSIndentLevel(const Value: integer);
    function PartbiggerthanMB(maxsize: integer): boolean;
    procedure PartShrinkToMB(newsize: integer);
    procedure AddToConfidentials(newsecret: string);
    function isConfidential(teststring: string): boolean;
    procedure log2history(line: string);
    procedure logComponent(comp: string; line: string);
    procedure CreateTheLogfile(LogDateiname: string); overload;
    procedure CreateTheLogfile(LogDateiname: string; check4append: boolean); overload;


    procedure SetNumberOfErrors(Number: integer);
    procedure addToNoLogFiles(filename: string);
    property LogLevel: integer read FLogLevel write FLogLevel;
    property NumberOfWarnings: integer read FNumberOfWarnings write FNumberOfWarnings;
    property NumberOfErrors: integer read FNumberOfErrors write SetNumberOfErrors;
    property NumberOfHints: integer read FNumberOfHints write FNumberOfHints;
    property ErrorNumberMarked: integer read FErrorNumberMarked
      write FErrorNumberMarked;

    property ReportErrorsToUser: boolean read FReportErrorsToUser
      write FReportErrorsToUser;
    property FileName: string read FFileName write FFileName;
    property MainFileName: string read FMainFileName write FMainFileName;
    property PartFileName: string read FPartFileName write FPartFileName;
    property PartReadFileName: string read FPartReadFileName write FPartReadFileName;
    property TraceMode: boolean read FTraceMode write FTraceMode;
    property RemoteFileLogging: string read FRemoteFileLogging;
    property LogFileExists: boolean read FLogFileExists write FLogFileExists;
    property PartLogFileExists: boolean read FPartLogFileExists write FPartLogFileExists;
    property StandardLogFileext: string read FStandardLogFileext
      write FStandardLogFileext;


    property LogSIndentLevel: integer read FLogSIndentLevel write setLogSIndentLevel;
    function LogSIndent: string;
    function LogSIndentPlus(const n: integer): string;
    property OpsiscriptVersionRequired: string
      read FOpsiscriptVersionRequired write FOpsiscriptVersionRequired;
    procedure includelogtail(fname: string; logtailLinecount: integer;
      sourceEncoding: string);
    property Appendmode: boolean read FAppendmode write FAppendmode;
    property ActionProgress: string read FActionProgress write FActionProgress;
    property AktProduktId: string read FAktProduktId write FAktProduktId;
    property LogProduktId: boolean read FLogProduktId write FLogProduktId;
    property StandardPartLogFilename: string
      read FStandardPartLogFilename write FStandardPartLogFilename;
    property StandardLogFilename: string read FStandardLogFilename
      write FStandardLogFilename;
    property WritePartLog: boolean read FWritePartLog write FWritePartLog;
    property WriteHistFile: boolean read FWriteHistFile write FWriteHistFile;
    property WriteErrFile: boolean read FWriteErrFile write FWriteErrFile;
    property WriteComponentFile: boolean read FWriteComponentFile
      write FWriteComponentFile;
    property debug_prog: boolean read Fdebug_prog write Fdebug_prog;
    property debug_lib: boolean read Fdebug_lib write Fdebug_lib;
    property force_min_loglevel: integer read Fforce_min_loglevel
      write Fforce_min_loglevel;
    property default_loglevel: integer read Fdefault_loglevel write Fdefault_loglevel;
    property StandardLogPath: string read FStandardLogPath write FStandardLogPath;
    property StandardMainLogPath: string read FStandardMainLogPath
      write FStandardMainLogPath;
    property StandardPartLogPath: string read FStandardPartLogPath
      write FStandardPartLogPath;
    property UsedLogLevel: integer read FUsedLogLevel write FUsedLogLevel;
    //function copyPartLogToFullLog: boolean;
  end;



type
  TErrorLevel = integer;

const
  StandardPartLogFileext = '.log';
  StandardPartReadFileext = '.read';
  ComponentLogSubDir = 'lastprodlogs';
  //FStandardLogFileext = '.log';
  //  FStandardLogFilename =  'opsi-winst';
  //  FStandardLogFileext =   '.log';


(* old Loglevels before opsi 4
  LevelFatal    = -2; //not yet used;
  BaseLevel     = -1;
  LevelWarnings     =  0;
  LevelInfo  = 1;
  LevelComplete = 2;
  LevelDebug    = 3;
  Level_not_initialized = 9;
*)

(* new Loglevels since opsi 4:
0 = nothing (absolute nothing)
1 = essential ("unverzichtbares")
2 = critical (unexpected errors that my cause a program abort)
3 = error (Errors that don't will abort the running program)
4 = warning (you should have a look at this)
5 = notice (Important statements to the program flow)
6 = info (Additional Infos)
7 = debug (important debug messages)
8 = debug2 (a lot more debug informations and data)
9 = confidential (passwords and other security relevant data)
*)
  LLnothing = 0;
  LLessential = 1;
  LLcritical = 2;
  LLerror = 3;
  LLwarning = 4;
  LLnotice = 5;
  LLinfo = 6;
  LLdebug = 7;
  LLdebug2 = 8;
  LLdebug3 = 9;
  LLconfidential = LLdebug3;


  // ExtremeErrorLevels:
  LevelFatal = 1;
  Level_not_initialized = 9;

  passwordFiller = '***(confidential)***';

  peakIndicator: string = '';
  peakIndicatorLength: integer = 100;
  lastPeakLen: integer = 0;
  peakIndicatorChar: char = '|';


var
  LogDatei: TLogInfo;
  defaultStandardLogPath: string;
  defaultStandardMainLogPath: string;
  defaultStandardPartLogPath: string;
  AppDataPath: array[0..MaxPathLen] of char; //Allocate memory


//const
//  remoteErrorLogging: TRemoteErrorLogging = trel_none;


function getComputerName: string;
//procedure CreateTheLogfile(var LogDateiname: string); overload;
//procedure CreateTheLogfile(var LogDateiname: string; check4append: boolean); overload;
function GetContinueLogFile(var LogFilename: string): boolean;
function getCallAddrStr: string;

implementation

//uses

{$IFDEF OPSISCRIPT}
uses
{$IFDEF WINDOWS}
  Windows,
  osregistry,
{$ENDIF WINDOWS}
{$IFDEF GUI}
  osmessagedialog,
  osbatchgui,
  osinteractivegui,
{$ENDIF GUI}
  osparser,
  osmain,
  osdefinedfunctions,
  osfunc;

{$ENDIF}


type
  TLogCallbackProcedure = procedure(level: integer; logmsg: string) of object;


var
  syslogmessagetag: string;
  externLogProc: TLogCallbackProcedure;
  j: integer;


{$IFNDEF OPSISCRIPT}
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


(*
moved to osfilehelper (do 4.6.2021)

procedure MakeBakFiles(const FName: string; maxbaks: integer);
var
  bakcounter: integer;
  extension: string;
  basename: string;
  path: string;
  newfilename, newbakname: string;

begin
  path := ExtractFilePath(FName);
  basename := ExtractFileNameOnly(FName);
  extension := ExtractFileExt(FName);
  if FileExists(FName) then
  begin
    // this is old style (name.ext.num) and is here only for clean up old logs
    for bakcounter := maxbaks - 1 downto 0 do
    begin
      if FileExists(FName + '.' + IntToStr(bakcounter)) then
      begin
        newfilename := path + PathDelim + basename + '_' +
          IntToStr(bakcounter) + extension;
        CopyFile(FName + '.' + IntToStr(bakcounter), newfilename, True);
        DeleteFileUTF8(FName + '.' + IntToStr(bakcounter));
      end;
    end;
    // this is new style (name_num.ext)
    for bakcounter := maxbaks - 1 downto 0 do
    begin
      newfilename := path + PathDelim + basename + '_' +
        IntToStr(bakcounter) + extension;
      if FileExists(newfilename) then
      begin
        newbakname := path + PathDelim + basename + '_' +
          IntToStr(bakcounter + 1) + extension;
        CopyFile(newfilename, newbakname, True);
      end;
    end;
    newfilename := path + PathDelim + basename + '_' + IntToStr(0) + extension;
    CopyFile(FName, newfilename, True);
    DeleteFileUTF8(FName);
  end;
end;

*)

{$ENDIF}


function escapeControlChars(t: string): string;
begin
  //t := stringreplace(t, char($5c), '\\'); //  \
  //t := stringreplace(t, char($22), '\"'); //  "
  //t := stringreplace(t, char($2f), '\/'); //  /
  t := stringreplace(t, #0, '\u0000'); //  null
  t := stringreplace(t, #2, '\c'); //  #10
  t := stringreplace(t, #3, '\c'); //  #10
  t := stringreplace(t, #4, '\c'); //  #10
  t := stringreplace(t, #5, '\c'); //  #10
  t := stringreplace(t, #6, '\c'); //  #10
  t := stringreplace(t, #7, '\a'); //  #10
  //t := stringreplace(t, #8, '\b'); //  backspace
  //t := stringreplace(t, #9, '\t'); // tab
  t := stringreplace(t, #10, '\n'); //  newline
  t := stringreplace(t, #11, '\v'); //  vertical tab
  t := stringreplace(t, #12, '\f'); //  form feed
  t := stringreplace(t, #13, '\r'); //  carriage return
  t := stringreplace(t, #14, '\c'); //  SO
  t := stringreplace(t, #15, '\c'); //  SI
  t := stringreplace(t, #16, '\c'); //  DLE
  t := stringreplace(t, #17, '\c'); //  DC1
  t := stringreplace(t, #18, '\c'); //  DC2
  t := stringreplace(t, #19, '\c'); //  DC3
  t := stringreplace(t, #20, '\c'); //  DC4
  t := stringreplace(t, #21, '\c'); //  NAK
  t := stringreplace(t, #22, '\c'); //  SYN
  t := stringreplace(t, #23, '\c'); //  ETB
  t := stringreplace(t, #24, '\c'); //  Cancel
  t := stringreplace(t, #25, '\c'); //  EM
  t := stringreplace(t, #26, '\c'); //  SUB
  t := stringreplace(t, #27, '\c'); //  ESC
  t := stringreplace(t, #28, '\c'); //  FS
  t := stringreplace(t, #29, '\c'); //  GS
  t := stringreplace(t, #30, '\c'); //  RS
  t := stringreplace(t, #31, '\c'); //  US
  t := stringreplace(t, #127, '\c'); //  DEL
  Result := t;
end;


procedure setLogProcedure(procName: TLogCallbackProcedure);
begin
  externLogProc := procName;
end;



function GetContinueLogFile(var LogFilename: string): boolean;

  //  Reads the continue flag from the registry.
  //  Only if this flag will be 1 and a file with the filename stored in the Registry
  //  will exist, LogFilename will take this value

var
  ContinueFilename: string;

  {$if defined(WINDOWS) and defined(OPSISCRIPT)}
  procedure readValues(const RegHive: string);
  var
    rkey: HKEY;
    RegLogFileOptions: TRegistry;
  begin
    RegLogfileOptions := TRegistry.Create;
    with RegLogfileOptions do
    begin
      //get rkey from RegHive
      GetHKey(RegHive, rkey);
      Rootkey := rkey;
      OpenKey(WinstRegKey, True);
      if ReadInteger(WinstRegContinueVar) = 1 then
      begin
        Result := True;
        try
          ContinueFilename := ReadString(WinstRegLastLogfile);
        except
          Result := False;
        end;
        if Result and not fileExists(LogFileName) then
          Result := False;
      end
      else
        Result := False;
      Free;
    end;
  end;

  {$ENDIF}

begin
  try
    {$if defined(WINDOWS) and defined(OPSISCRIPT)}
    readValues(WinstRegHive);
    {$ELSE}
    Result := False;
    {$ENDIF}
  except
    Result := False;
  end;
  {if not result then
  try
    readValues ('HKCU')
  except
  end;}

  if Result then
    LogFilename := ContinueFilename;
end;

procedure TLogInfo.CreateTheLogfile(LogDateiname: string);
begin
  CreateTheLogfile(LogDateiname, True);
end;

procedure TLogInfo.CreateTheLogfile(LogDateiname: string; check4append: boolean);
var
  i: integer;
  filelist: TStringList;
  filename: string;
  {$IFDEF OPSISCRIPT}
  files: TuibFileInstall;
  {$ENDIF}
  maxbaks: integer = 8;
begin
  if (FStandardPartLogFilename = 'noname-part-') and (LogDateiname <> '') then
    FStandardPartLogFilename := LogDateiname + '-part-';
  {$IFDEF OPSISCRIPT}
  // remove old partlog files
  startupmessages.Add('Cleanup old part files at ' + DateTimeToStr(Now));
  files := TuibFileInstall.Create;
  try
    files.alldelete(FStandardPartLogPath + Pathdelim + FStandardPartLogFilename +
      '*', False, True, 7);
  except
  end;
  files.Free;
  // get maxbaks from osconf:
  maxbaks := log_rotation_count;
  if maxbaks > 999 then maxbaks := 999;
  if maxbaks < 0 then maxbaks := 0;
  {$ENDIF}
  {$IFNDEF OPSISCRIPT}
  if not DirectoryExistsUTF8(FStandardPartLogPath) then
    ForceDirectoriesUTF8(FStandardPartLogPath);
  // remove old partlog files
  try
    filelist := FindAllFiles(FStandardPartLogPath, FStandardPartLogFilename +
      '*', False);
    for i := 0 to filelist.Count - 1 do
    begin
      // only delete files older than a week
      if FileAge(filelist.Strings[i]) < DateTimeToFileDate(now - 7) then
      begin
        filename := filelist.Strings[i];
        DeleteFile(filename);
      end;
    end;
  except
  end;
  filelist.Free;
  LogDateiName := FStandardMainLogPath + PathDelim + LogDateiName;
  {$ENDIF}

  if not check4append then
  begin
    // normally called at login scripts
    // do not look for appends in registry from machine runs
    // just create the log
    // create new Log File
    LogDatei.Appendmode := False;
    MakeBakFiles(LogDateiName, maxbaks);
    LogDatei.initiate(LogDateiName, False);
    LogDatei.Empty;
  end
  else
  begin
    // at machine mode look if we should append (and to which file)
    if GetContinueLogfile(LogDateiname) then
    begin
      LogDatei.Appendmode := True;
      LogDatei.initiate(LogDateiName, False);
      Logdatei.Log('', LLessential);
      Logdatei.Log('', LLessential);
      Logdatei.Log('======= APPEND   ' + DateTimeToStr(Now), LLessential);
    end
    else
    begin
      // create new Log File
      LogDatei.Appendmode := False;
      {$IFDEF OPSISCRIPT}
      if assigned(startupmessages) then
        startupmessages.Add('Backup old log files at ' + DateTimeToStr(Now));
      {$ENDIF OPSISCRIPT}
      MakeBakFiles(LogDateiName, maxbaks);
      {$IFDEF OPSISCRIPT}
      if assigned(startupmessages) then
        startupmessages.Add('Initiate new log file at ' + DateTimeToStr(Now));
      {$ENDIF OPSISCRIPT}
      LogDatei.initiate(LogDateiName, False);
      LogDatei.Empty;
    end;
  end;
  {$IFDEF OPSISCRIPT}
  //LogDatei.FWriteHistFile:=true;
  Logdatei.log('opsi-script ' + OpsiscriptVersion + ' started at >> ' + starttimestr,
    LLessential);
  Logdatei.log('opsi-script log file with encoding ' + DefaultEncoding, LLessential);
  for i := 0 to startupmessages.Count - 1 do
    Logdatei.log(startupmessages.Strings[i], LLessential);
  {$ENDIF}
end;


(* TLogInfo *)

constructor TLogInfo.Create;
begin
  inherited Create;
  LogFileExists := False;
  FAppendmode := False;
  FConfidentialStrings := TStringList.Create;
  FLogProduktId := False;
  FStandardLogFileext := '.log';
  FWritePartLog := True;
  FStandardPartLogFilename := 'noname-part-';
  {$IFDEF OPSISCRIPT}
  FStandardPartLogFilename := 'opsi-script-part-';
  FStandardLogFilename := 'opsi-script';
  FStandardLogFileext := '.log';
  {$ENDIF OPSISCRIPT}
  {$IFDEF OCASIMP}
  FStandardPartLogFilename := 'ocasimp-part-';
  FStandardLogFilename := 'ocasimp';
  {$ENDIF OCASIMP}
  {$IFDEF OPSISCRIPTSTARTER}
  FStandardPartLogFilename := 'opsiscriptstarter-part-';
  FStandardLogFilename := 'opsiscriptstarter';
  {$ENDIF OPSISCRIPTSTARTER}
  {$IFDEF OPSICLIENTKIOSK}
  FStandardPartLogFilename := 'opsiclientkiosk-part-';
  FStandardLogFilename := 'opsiclientkiosk';
  {$ENDIF OPSICLIENTKIOSK}
  (*
  {$IFDEF OPSICLIENTD_SHUTDOWN_STARTER}
  FStandardPartLogFilename := 'opsiclientd_shutdown_starter-part-';
  FStandardLogFilename := 'opsiclientd_shutdown_starter';
  {$ENDIF OPSICLIENTD_SHUTDOWN_STARTER}
  *)
  FStandardLogPath := defaultStandardLogPath;
  FStandardMainLogPath := defaultStandardMainLogPath;
  FStandardPartLogPath := defaultStandardPartLogPath;
  FNoLogFiles := TStringList.Create;
  { we want no duplicates in this list - so we have not to check at add }
  FNoLogFiles.Sorted := True;
  FNoLogFiles.Duplicates := dupIgnore;
  FWriteComponentFile := False;
end;


procedure informTheAdministrator(info: string);
var
  ps: string;
begin
  ps := info + '! ' + LineEnding + 'Please inform the Administrator!';
  {$IFDEF GUI}
  {$IFDEF OPSISCRIPT}
  MyMessageDlg.WiMessage(ps, [mrOk]);
  {$ELSE}
  ShowMessage(ps);
  {$ENDIF}
  {$ENDIF}
end;


function isopenText(var FileVariable: Text): boolean;
begin
  case TTextRec(FileVariable).Mode of
    fmInput, fmOutput:
      Result := True;
    else
      Result := False;
  end; {case}
end;


function isopen(var FileVariable: Thandle): boolean;
begin
  if FileSeek(FileVariable, 0, fsFromEnd) = -1 then
    Result := False
  else
    Result := True;
end;


//constructor TLogInfo.create (const Fname : String; Trace: Boolean);
procedure TLogInfo.initiate(const Fname: string; Trace: boolean);
var
  ps: string;
  YetATrial: boolean;
  LogFileNo: integer;
  Ext: string;
  YetATrialCounter: integer = 0;
  maxTrials: integer = 0;
  myrandomstr: string;

begin
  NumberOfErrors := 0;
  NumberOfWarnings := 0;
  NumberOfHints := 0;
  ErrorNumberMarked := 0;
  LogLevel := LLinfo;
  FLogcallcount := 0;

  LogSIndentLevel := 0;

  LogFileExists := False;

  TraceMode := Trace;
  FFilename := Fname;
  FConfidentialStrings.Clear;
  FErrorsFilename := ChangeFileExt(Fname, '.err');
  FHistoryFilename := ChangeFileExt(Fname, '.history');

  FErrorsFileExists := False;
  FRemoteErrorsFileExists := False;
  LogFileExists := False;
  FHistoryFileExists := False;
  FComponentFileExists := False;
  FComponentFilename := '';
  ComponentFileF := 0;


  YetATrial := True;
  maxTrials := 10;

  while YetATrial do
  begin
    //assignfile(LogMainFile, FileName);
    //if not FErrorsFileExists then
    if FWriteErrFile then
    begin
      if isopen(ErrorfileF) then
      begin
        try
          //append(Errorfile);
          //Closefile(Errorfile);
          FileClose(ErrorfileF);
        except
          //assignfile(Errorfile, FErrorsFilename);
        end;
      end
      else
      begin
        if not isOpen(ErrorfileF) then
        begin
          if not fileexists(FErrorsFilename) then
          begin
            ErrorfileF := FileCreate(FErrorsFilename);
            FileClose(ErrorfileF);
          end;
          //ErrorfileF := FileOpen(FErrorsFilename, fmOpenReadWrite or fmShareDenyNone);
          //FileSeek(FileVariable,0,fsFromEnd);
        end;
        (*
        {$I-}
        append(Errorfile);
        {$I+}
        if IOResult <> 0 then
          assignfile(Errorfile, FErrorsFilename)
        else
          Closefile(Errorfile);
          *)
      end;
    end;

    if FWriteHistFile then
    begin
      if isopen(HistroryFileF) then
      begin
        try
          FileClose(HistroryFileF);
        except
        end;
      end
      else
      begin
        if not isOpen(HistroryFileF) then
        begin
          if not fileexists(FHistoryFilename) then
          begin
            HistroryFileF := FileCreate(FHistoryFilename);
            FileClose(HistroryFileF);
          end;
        end;
      end;
    end;


    if not isOpen(LogMainFileF) then
    begin
      if not fileexists(FFilename) then
      begin
        LogMainFileF := FileCreate(FFilename);
        FileClose(LogMainFileF);
      end;
    end;

    ps := '';
    if fileExists(FFilename) then
      try
        //append(LogMainFile);
        LogMainFileF := FileOpen(FFilename, fmOpenReadWrite or fmShareDenyNone);
        FileSeek(LogMainFileF, 0, fsFromEnd);
        LogFileExists := True;
      except
        on E: Exception do
          ps := 'Existing file "' + FFilename +
            '" could not be opened as logfile. Exception "' + E.Message + '"';
      end
    else
    begin
      if FileName = '' then
        ps := 'An empty string does not work as logfilename.'
      else
        try
          LogMainFileF := FileCreate(FFilename);
          FileClose(LogMainFileF);
          LogMainFileF := FileOpen(FFilename, fmOpenReadWrite or fmShareDenyNone);
          //FileSeek(LogMainFileF,0,fsFromEnd);
          LogFileExists := True;
        except
          on E: Exception do
            ps := '"' + FFilename +
              '" could not be created as logfile. Exception "' + E.Message + '"';
        end;
    end;

    if FWriteErrFile then
    begin
      ps := '';
      // error file
      if fileExists(FErrorsFileName) then
        try
          //append(ErrorFile);
          ErrorFileF := FileOpen(FErrorsFileName, fmOpenReadWrite or fmShareDenyNone);
          FileSeek(ErrorFileF, 0, fsFromEnd);
          FErrorsFileExists := False;
        except
          on E: Exception do
          begin
            ps := 'Existing file "' + FErrorsFilename +
              '" could not be opened as error-logfile. Exception "' +
              E.Message + '"';
            FErrorsFileExists := False;
          end
        end
      else
      begin
        if FErrorsFileName = '' then
        begin
          ps := 'An empty string does not work as logfilename.';
          FErrorsFileExists := False;
        end
        else
          try
            ErrorFileF := FileCreate(FErrorsFileName);
            FileClose(ErrorFileF);
            ErrorFileF := FileOpen(FErrorsFileName, fmOpenReadWrite or
              fmShareDenyNone);
          except
            on E: Exception do
            begin
              ps := '"' + Filename +
                '" could not be created as logfile. Exception "' + E.Message + '"';
              FErrorsFileExists := False;
            end;
          end;
      end;
    end;

    // History file
    if FWriteHistFile then
    begin
      if fileExists(FHistoryFilename) then
        try
          //append(ErrorFile);
          HistroryFileF := FileOpen(FHistoryFilename, fmOpenReadWrite or
            fmShareDenyNone);
          FileSeek(HistroryFileF, 0, fsFromEnd);
          FHistoryFileExists := False;
        except
          on E: Exception do
          begin
            ps := 'Existing file "' + FHistoryFilename +
              '" could not be opened as error-logfile. Exception "' +
              E.Message + '"';
            FHistoryFileExists := False;
          end
        end
      else
      begin
        if FHistoryFilename = '' then
        begin
          ps := 'An empty string does not work as logfilename.';
          FHistoryFileExists := False;
        end
        else
          try
            HistroryFileF := FileCreate(FHistoryFilename);
            FileClose(HistroryFileF);
            HistroryFileF :=
              FileOpen(FHistoryFilename, fmOpenReadWrite or fmShareDenyNone);
          except
            on E: Exception do
            begin
              ps := '"' + Filename +
                '" could not be created as logfile. Exception "' + E.Message + '"';
              FHistoryFileExists := False;
            end;
          end;
      end;
    end;

    if LogFileExists then
      YetATrial := False
    else
    begin
      Ext := ExtractFileExt(FFilename);
      System.Delete(Ext, 1, 1);
      try
        LogfileNo := StrToInt(Ext);
        Inc(LogFileNo);
      except
        LogFileNo := 0;
      end;

      Filename := copy(Filename, 1, pos('.', FFilename)) + IntToStr(LogFileNo);
      ps := ps + LineEnding + 'use instead "' + FFilename + '" ?' +
        LineEnding + 'OK | Cancel (install without logging) | Exit program ';

      Inc(YetATrialCounter);
      if YetATrialCounter > maxTrials then
        YetATrial := False; // give up - no logging
      (*
      case MyMessageDlg.WiMessage(ps, [mrOk, mrCancel, mrAbort]) of
        mrOk: ;
        mrCancel:
        begin
          YetATrial := False;
          informTheAdministrator('No logging');
        end;
        mrAbort:
        begin
          informTheAdministrator('Exit program');
          Halt;
        end;
      end;
      *)
    end;
  end;

  FReportErrorsToUser := False;
  if FWritePartLog then
  begin
    PartLogFileExists := False;
    ForceDirectories(FStandardPartLogPath);
    Randomize;
    {$IFDEF OPSISCRIPT}
    myrandomstr := randomstr(False);
    {$ELSE}
    //inttostr(Random(MAXLONGINT))+ExtractFileNameWithoutExt(ExtractFileName(FFilename))
    myrandomstr := IntToStr(Random(MAXLONGINT));
    {$ENDIF}
    if (FStandardPartLogFilename = 'noname-part-') and (Fname <> '') then
      FStandardPartLogFilename := Fname + '-part-';
    PartFileName := FStandardPartLogPath + PathDelim + FStandardPartLogFilename +
      myrandomstr + StandardPartLogFileext;
    //assignfile(LogPartFile, PartFileName);
    //writeln(PartFileName);
    try
      //rewrite(LogPartFile);
      LogPartFileF := FileCreate(PartFileName);
      FileClose(LogPartFileF);
      LogPartFileF := FileOpen(PartFileName, fmOpenReadWrite or fmShareDenyNone);
      PartLogFileExists := True;
    except
      on E: Exception do
        ps := '"' + PartFileName +
          '" could not be created as logfile. Exception "' + E.Message + '"';
    end;

    Log('--', LLessential);
    Log('--', LLessential);
    Log(PartFileName, LLessential);
  end;
end;

destructor TLogInfo.Destroy;
begin
  if LogFileExists then
    if isOpen(LogMainFileF) then
      FileClose(LogMainFileF);

  if FErrorsFileExists then
    try
      if isOpen(ErrorFileF) then
        FileClose(ErrorFileF);

    except
    end;

  if FHistoryFileExists then
    try
      if isOpen(HistroryFileF) then
        FileClose(HistroryFileF);
    except
    end;

  if PartLogFileExists then
    if isOpen(LogPartFileF) then
      FileClose(LogPartFileF);

  FreeAndNil(FNoLogFiles);
  FreeAndNil(FConfidentialStrings);
  inherited Destroy;
end;




procedure TLogInfo.Reopen;
begin
  if LogFileExists then
    try
      LogMainFileF := FileOpen(FFileName, fmOpenReadWrite or fmShareDenyNone);
      FileSeek(LogMainFileF, 0, fsFromEnd);
    except
      LogFileExists := False;
    end;
  if FErrorsFileExists then
    try
      ErrorFileF := FileOpen(FErrorsFileName, fmOpenReadWrite or fmShareDenyNone);
      FileSeek(ErrorFileF, 0, fsFromEnd);
    except
      FErrorsFileExists := False;
    end;

  if FHistoryFileExists then
    try
      HistroryFileF := FileOpen(FHistoryFilename, fmOpenReadWrite or fmShareDenyNone);
      FileSeek(HistroryFileF, 0, fsFromEnd);
    except
      FHistoryFileExists := False;
    end;

end;

procedure TLogInfo.PartReopen;
begin
  if FWritePartLog then
  begin
    if PartLogFileExists then
      try
        LogPartFileF := FileOpen(PartFileName, fmOpenReadWrite or fmShareDenyNone);
        FileSeek(LogPartFileF, 0, fsFromEnd);
      except
        PartLogFileExists := False;
      end;
  end;
end;


procedure TLogInfo.OpenForReading;
begin
  if LogFileExists then
    try
      if isOpen(LogMainFileF) then
        FileSeek(LogMainFileF, 0, fsFromBeginning)
      else
      begin
        LogMainFileF := FileOpen(FFileName, fmOpenReadWrite or fmShareDenyNone);
        FileSeek(LogMainFileF, 0, fsFromBeginning);
      end;
    except
      LogFileExists := False;
    end;
end;

procedure TLogInfo.PartOpenForReading;
begin
  PartCopyToRead;
  //Log('->1',LLNotice);
  assignfile(LogPartReadFile, FPartReadFileName);
  //Log('->2',LLNotice);
  try
    reset(LogPartReadFile);
  except
    on E: Exception do
    begin
      Log('wilog: PartOpenForReading: "' + E.Message + '" ==> retry',
        LLError);
      try
        Sleep(500);
        reset(LogPartReadFile);
      except
        on E: Exception do
        begin
          Log('wilog: PartOpenForReading: "' + E.Message +
            '" --> giving up',
            LLError);
        end;
      end;
    end;
  end;
  Log('read file opend', LLInfo);
end;

procedure TLogInfo.PartCloseFromReading;
begin
  //if isOpenText(LogPartReadFile) then
  try
    closeFile(LogPartReadFile);
  except
  end;
  SysUtils.DeleteFile(FPartReadFileName);
end;

procedure TLogInfo.DeletePartLog;
var
{$IFDEF OPSISCRIPT}
  files: TuibFileInstall;
{$ENDIF}
  i: integer;
  filelist: TStringList;
begin
  {$IFDEF OPSISCRIPT}
  files := TuibFileInstall.Create;
  if PartLogFileExists then
  begin
    try
      system.Erase(LogPartFile);
      PartLogFileExists := False;
    except
      PartLogFileExists := True;
    end;
  end;
  try
    files.alldelete(FStandardPartLogPath + Pathdelim + FStandardPartLogFilename +
      '*', False, True, 7);
  except
    //Logdatei.Log('not all files "' + TempPath + TempBatchdatei + '*"  could be deleted', LLInfo);
  end;
  files.Free;
  {$ENDIF}

  {$IFNDEF OPSISCRIPT}
  // remove old partlog files
  try
    filelist := FindAllFiles(FStandardPartLogPath, FStandardPartLogFilename +
      '*', False);
    for i := 0 to filelist.Count - 1 do
    begin
      DeleteFile(filelist.Strings[i]);
    end;
  except
  end;
  filelist.Free;
  {$ENDIF}

end;

procedure TLogInfo.Close;
begin
  if LogFileExists then
    if isOpen(LogMainFileF) then
      FileClose(LogMainFileF);

  if FErrorsFileExists then
    if isOpen(ErrorFileF) then
      FileClose(ErrorFileF);

  if FHistoryFileExists then
    if isOpen(HistroryFileF) then
      FileClose(HistroryFileF);

  if PartLogFileExists then
    if isOpen(LogPartFileF) then
      FileClose(LogPartFileF);

  if FComponentFileExists then
    if isOpen(ComponentFileF) then
      FileClose(ComponentFileF);
end;


procedure TLogInfo.Empty;
begin
  if LogFileExists then
  begin
    if isOpen(LogMainFileF) then
      FileClose(LogMainFileF);
    //DeleteFileUTF8(FFileName);
    LogMainFileF := FileCreate(FFileName);
    FileClose(LogMainFileF);
    LogMainFileF := FileOpen(FFileName, fmOpenReadWrite or fmShareDenyNone);
  end;
end;

procedure TLogInfo.logComponent(comp: string; line: string);
var
  mycompfilename: string;
  mylogdir: string;
begin
  mylogdir := FStandardPartLogPath + PathDelim + ComponentLogSubDir;
  mycompfilename := mylogdir + PathDelim + comp + '.log';
  ForceDirectory(mylogdir);
  if FComponentFilename = mycompfilename then
  begin
    // we work on an existing log
    if not isOpen(ComponentFileF) then
    begin
      // open new
      ComponentFileF := FileCreate(FComponentFilename);
    end;
  end
  else
  begin
    // we work on an new log
    FComponentFilename := mycompfilename;
    if isOpen(ComponentFileF) then // close old
    begin
      // close old
      FileClose(ComponentFileF);
    end;
    ComponentFileF := FileCreate(FComponentFilename);
  end;
  WriteLogLine(ComponentFileF, line);
end;

function TLogInfo.LogSIndentPlus(const n: integer): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to n do
    Result := Result + '  ';
end;

function TLogInfo.LogSIndent;
begin
  Result := LogSIndentPlus(LogSIndentLevel);
end;

procedure TLogInfo.WriteLogLine(myfile: THandle; line: string);
begin
  line := line + LineEnding;
  FileWrite(myfile, line[1], Length(line));
end;


function TLogInfo.getLine(var S: string): boolean;
var
  charbuf, pstr: PChar;
  Count: integer;
begin
  s := '';
  //if logfileExists and not EOF(LogMainFile) then
  if logfileExists then
  begin
    //readln(LogMainFile, s);
    charbuf := StrAlloc(250);
    pstr := StrAlloc(260);
    Count := FileRead(LogMainFileF, charbuf, 250);
    if Count > 0 then
    begin
      Result := True;
      strlcopy(pstr, charbuf, Count);
      s := strpas(pstr);
    end
    else
      Result := False;
    strdispose(pstr);
    strdispose(charbuf);
  end
  else
    Result := False;
end;


function TLogInfo.getPartLine(var S: string): boolean;
begin
  if not EOF(LogPartReadFile) then
  begin
    readln(LogPartReadFile, s);
    Result := True;
  end
  else
    Result := False;
end;

function TLogInfo.log_prog(const S: string; LevelOfLine: integer): boolean;
begin
  Result := False;
  if (LevelOfLine <= LLwarning) or Fdebug_prog then
    Result := log('Prog: ' + S, LevelOfLine);
end;


function TLogInfo.log(const S: string; LevelOfLine: integer): boolean;
var
  PasS: string;
  st: string;
  peaklen: integer = 0;
  //usedloglevel: integer = 0;
  i: integer;
  dummybool: boolean;
  orgfilename, mainfilename, sectionFilename: string;
  {$IFDEF GUI}
  dlgresult: TModalresult;
  {$ENDIF}
begin
  try
    try
      if LevelOfLine < LLnothing then
        LevelOfLine := LLnothing;
      if LevelOfLine > LLdebug3 then
        LevelOfLine := LLdebug3;

      if LevelOfLine = LLWarning then
        NumberOfWarnings := NumberOfWarnings + 1;
      if LevelOfLine = LLError then
        NumberOfErrors := NumberOfErrors + 1;
      if LevelOfLine = LLCritical then
        NumberOfErrors := NumberOfErrors + 1;

      FUsedLogLevel := loglevel;
      if FUsedLogLevel < Fforce_min_loglevel then
        FUsedLogLevel := Fforce_min_loglevel;

      {$IFDEF OPSISCRIPT}
      // running defined function ?
      if inDefFuncIndex > -1 then
      begin
        orgfilename := definedFunctionArray[inDefFuncIndex].OriginFile;
        mainfilename := ExtractFileName(script.Filename);
        if FNoLogFiles.IndexOf(orgfilename) <> -1 then
        begin
          // defined function imported from lib
          // do we want to debug libraries ?
          if (not debug_lib) then
          begin
            // only Warnings and less
            FUsedLogLevel := LLWarning;
          end;
        end;

       (*
       if Assigned(script) then
       if Assigned(script.FLibList) then
         if script.FLibList.Count > script.aktScriptLineNumber then
         begin
           // does this log line come from a library ?
           if StrToBool(script.FLibList.Strings[script.aktScriptLineNumber])
              // do we want to debug libraries ?
              and (not debug_lib) then
                // only Warnings and less
                usedloglevel :=  LLWarning;
         end;
         *)
      end;
       {$ENDIF}



      st := s;
      st := TrimRight(st);
      //UTF8FixBroken(st);

      // now some things we do not want to log:
      // thing we do not log below loglevel 9
      if FUsedLogLevel < LLconfidential then
      begin
       {$IFDEF OPSISCRIPT}
        // hide opsi service password
        st := SysUtils.StringReplace(st, opsiservicePassword,
          passwordFiller, [rfReplaceAll]);
       {$ENDIF}
        // hide all confidential strings
        for i := 0 to FConfidentialStrings.Count - 1 do
          st := SysUtils.StringReplace(st, FConfidentialStrings[i],
            passwordFiller, [rfReplaceAll]);
      end;
      // replace ' -->' by ' ==>' because the opsi web service get in trouble because it is interpreted as xml comment
      st := SysUtils.StringReplace(st, ' -->', ' ==>', [rfReplaceAll]);
      // check if line starts with '-->'
      if pos('-->', st) = 1 then
        st := SysUtils.StringReplace(st, '-->', '==>', []);
      // no line breaks
      st := escapeControlChars(st);
    except
      on E: Exception do
      begin
        Log('oslog: Log: process message and Loglevel:"' +
          E.Message + '"',
          LLError);
      end
    end;



    try
      //show activity
      //FBatchOberflaeche.LabelProgress.Caption := st;

      begin
        if length(st) > peakindicatorlength then
          peaklen := peakindicatorlength
        else
          peaklen := length(st);

        if (lastpeaklen > 1) and (peaklen > 1) then
          peaklen := (lastpeaklen + peaklen) div 2;
        lastpeaklen := peaklen;

        {$IFDEF GUI}
        {$IFDEF OPSISCRIPT}
        if FBatchOberflaeche <> nil //dont log before creating FBatchOberflaeche
        then
        begin
          // commented out while 4.11.2 debugging (do)
          if (FUsedLogLevel >= LevelOfLine) then
            FBatchOberflaeche.SetMessageText(copy(peakindicator, 1, peaklen), mActivity);
          //setActivityLabel(copy(peakindicator, 1, peaklen));
        end;
        {$ENDIF}
        {$ENDIF}
      end;
    except
      on E: Exception do
      begin
        Log('oslog: Log: Activity "' + E.Message + '"',
          LLError);
      end
    end;




    if (FUsedLogLevel >= LevelOfLine) and LogFileExists then
    begin
      try
        if FLogProduktId then
        begin
         (*
          PasS := '[' + IntToStr(LevelOfLine) + '] [' +
            FormatDateTime('mmm dd hh:nn:ss:zzz', Now) + '] [' +
            FAktProduktId + '] ' + LogSIndent + st;
            *)
          PasS := '[' + IntToStr(LevelOfLine) + '] [' +
            FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now) + '] [' +
            FAktProduktId + '] ' + LogSIndent + st;

        end
        else
        begin
          (*
          PasS := '[' + IntToStr(LevelOfLine) + '] [' +
            FormatDateTime('mmm dd hh:nn:ss:zzz', Now) + '] ' + LogSIndent + st;
            *)
          PasS := '[' + IntToStr(LevelOfLine) + '] [' +
            FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now) + '] [] ' + LogSIndent + st;
        end;
      except
        on E: Exception do
        begin
          Log('oslog: Log: Format : got exception: "' +
            E.Message + '" while preparing log message: ' + PasS,
            LLError);
        end
      end;
      try
        {$IFDEF OPSISCRIPT}
        (*
        // reencode log line to system encoding
        PasS  := ConvertEncodingFromUTF8(PasS,GetDefaultTextEncoding,dummybool);
        *)
        {$ENDIF}

        {$IFDEF GUI}
        {$IFDEF OPSISCRIPT}
        try
          CentralForm.Memo1Add(PasS);
        except
          try
            //Writeln(LogMainFile,
            WriteLogLine(LogMainFileF,
              '!!!Warning: memory problem, the following line could not be inserted into the memo field');
          except
          end;
          NumberOfWarnings := NumberOfWarnings + 1;
        end;
        {$ENDIF}
        {$ENDIF GUI}

        try
          //WriteLn(LogMainFile, PasS);
          //flush(LogMainFile);
          WriteLogLine(LogMainFileF, PasS);
        except
          try
            LogDatei.Reopen;
            //WriteLn(LogMainFile, PasS);
            WriteLogLine(LogMainFileF, PasS);
            //flush(LogMainFile);
          except
            {$IFDEF GUI}
            {$IFDEF OPSISCRIPT}
            if MyMessageDlg.WiMessage('Logfile ' + Filename +
              ' not available.  Continue without logging? ', [mrYes, mrNo]) = mrNo then
              halt
            else
              LogFileExists := False;
              {$ELSE}
            if MessageDlg('Question', 'Logfile ' + Filename +
              ' not available.  Continue without logging? ', mtConfirmation,
              [mbYes, mbNo], 0) = mrNo then
              halt
            else
              LogFileExists := False;
            {$ENDIF}
            {$ENDIF}
          end;
        end;

        if FWritePartLog then
          if PartLogFileExists then
          begin
            try
              WriteLogLine(LogPartFileF, PasS);
              //WriteLn(LogPartFile, PasS);
              //flush(LogPartFile);
            except
              try
                LogDatei.PartReopen;
                WriteLogLine(LogPartFileF, PasS);
                //WriteLn(LogPartFile, PasS);
                //flush(LogPartFile);
              except
              {$IFDEF GUI}
              {$IFDEF OPSISCRIPT}
                if MyMessageDlg.WiMessage('Logfile ' + FPartFileName +
                  ' not available.  Continue without logging? ',
                  [mrYes, mrNo]) = mrNo then
                  halt
                else
                  PartLogFileExists := False;
              {$ELSE}
                if MessageDlg('Question', 'Logfile ' + FPartFileName +
                  ' not available.  Continue without logging? ',
                  mtConfirmation, [mbYes, mbNo], 0) = mrNo then
                  halt
                else
                  PartLogFileExists := False;
            {$ENDIF}
              {$ENDIF}
              end;
            end;
          end;


        if FWriteErrFile then
          if FErrorsFileExists and (LevelOfLine <= LLwarning) then
            try
              WriteLogLine(ErrorFileF, PasS);
            except
              FErrorsFileExists := False
            end;


        if FWriteComponentFile and FLogProduktId then
          if trim(FAktProduktId) <> '' then
            logComponent(trim(FAktProduktId), PasS);

        Result := True;
      except
        on E: Exception do
        begin
          Log('oslog: Log: write: got exception: "' +
            E.Message + '" while writeing log message: ' + PasS,
            LLError);
        end
      end;
    end
    else
      Result := False;


    {$IFDEF GUI}
    if (TraceMode) and (LogLevel >= LevelOfLine)
    //if (not LogFileExists or TraceMode) and (LogLevel >= LevelOfLine)
    then
    begin
      {$IFDEF OPSISCRIPT}
      dlgresult := MyMessageDlg.WiMessage('TRACE:' + LineEnding +
        St, [mrOk, mrAbort]);
      {$ELSE}
      dlgresult := MessageDlg('Question', 'TRACE:' + LineEnding +
        St, mtConfirmation, [mbOK, mbAbort], 0);
     {$ENDIF}
      if dlgresult = mrAbort then
        TraceMode := False;
      Result := True;
    end;
    {$ENDIF}
  except
    on E: Exception do
    begin
      Log('oslog: Log: master:"' + E.Message + '"',
        LLError);
    end
  end;
end;


function TLogInfo.log_list(const list: TStrings;
  LevelOfLine: integer): boolean;
var
  i: integer;
begin
  i := 0;
  Result := True;
  if list <> nil then
  begin
    while (i < list.Count) and Result do
    begin
      Result := Log(format('(string %3d)', [i]) + list[i], levelOfLine);
      Inc(i);
    end;
  end
  else
  begin
    Log('Error writing string list to log: stringlist = nil', LLError);
    Result := False;
  end;
end;

procedure TLogInfo.SetLogSIndentLevel(const Value: integer);
begin
  FLogSIndentLevel := Value;
end;

procedure TLogInfo.SetNumberOfErrors(Number: integer);
begin
  FNumberOfErrors := Number;
end;

procedure TLogInfo.addToNoLogFiles(filename: string);
begin
  FNoLogFiles.add(filename);
end;

procedure TLogInfo.includelogtail(fname: string; logtailLinecount: integer;
  sourceEncoding: string);
var
  includelogStrList: TStringList;
  aktline, includeLogLineStart, includelogLinecount: integer;
  bool: boolean;
  str: string;
begin
  try
    Fname := ExpandFileName(Fname);
    if lowercase(sourceEncoding) = 'unicode' then
    begin
      includelogStrList := loadUnicodeTextFile(Fname, bool, str);
    end
    else
    begin
    {$IFDEF UNIX}
      includelogStrList := TStringList.Create;
      includelogStrList.LoadFromFile(FName);
      includelogStrList.Text :=
        reencode(includelogStrList.Text, sourceEncoding, sourceEncoding);
    {$ELSE}
      includelogStrList := loadTextFileWithEncoding(Fname, sourceEncoding);
    {$ENDIF}
    end;
    try
      includelogLinecount := includelogStrList.Count;
      if logtailLinecount > 0 then
      begin
        // working in tail mode
        if includelogLinecount > logtailLinecount then
          includeLogLineStart := includelogLinecount - logtailLinecount - 1
        else
          includeLogLineStart := 0;
        Log('Start including tail of LogFile "' + Fname +
          ' with encoding: ' + sourceEncoding + '"', LLDebug);
        Log('################################################################',
          LLDebug);
        for aktline := includeLogLineStart to includelogLinecount - 1 do
          Log('-->: ' + includelogStrList.Strings[aktline], LLDebug);
      end
      else
      begin
        // working in head mode
        // remove negative sign
        logtailLinecount := abs(logtailLinecount);
        // we can not include more then the file has
        if includelogLinecount < logtailLinecount then
          logtailLinecount := includelogLinecount;
        includeLogLineStart := 0;
        Log('Start including head of LogFile "' + Fname +
          ' with encoding: ' + sourceEncoding + '"', LLDebug);
        Log('################################################################',
          LLDebug);
        for aktline := includeLogLineStart to logtailLinecount - 1 do
          Log('-->: ' + includelogStrList.Strings[aktline], LLDebug);
      end;
      Log('################################################################',
        LLDebug);
      Log('End including LogFile "' + Fname + '"', LLDebug);
      finally
        if assigned(includelogStrList) then FreeAndNil(includelogStrList);
      end;
    except
      on E: Exception do
      begin
        Log('IncludeLogFile "' + Fname + '"', LLwarning);
        Log(' Failed to include log file, system message: "' + E.Message + '"',
          LLwarning);
      end
    end;
end;

function TLogInfo.PartbiggerthanMB(maxsize: integer): boolean;
var
  bytesize, mbsize: int64;
begin
  bytesize := lazfileutils.FileSizeUtf8(PartFileName);
  mbsize := bytesize div (1024 * 1024);
  Logdatei.log('Checking if partlog: ' + PartFileName + ' is bigger than ' +
    IntToStr(maxsize) + ' MB - found: ' + IntToStr(mbsize) + ' MB', LLInfo);
  Result := False;
  if PartLogFileExists then
  begin
    if mbsize > maxsize then
      Result := True
    else
      Result := False;
  end;
end;

procedure TLogInfo.AddToConfidentials(newsecret: string);
begin
  FConfidentialStrings.Append(newsecret);
end;

function TLogInfo.isConfidential(teststring: string): boolean;
var
  index: integer;
begin
  Result := False;
  index := FConfidentialStrings.IndexOf(teststring);
  if index > -1 then
    Result := True;
end;

procedure TLogInfo.PartShrinkToMB(newsize: integer);
begin
  if PartLogFileExists then
  begin
    FileClose(LogPartFileF);
    try
      {$IFDEF OPSISCRIPT}
      ShrinkFileToMB(PartFileName, newsize);
      {$ENDIF}
    finally
      PartReopen;
    end;
  end;
end;

procedure TLogInfo.PartCopyToRead;
var
  len: integer;
  //ct,units,
  size: longint;
  buffer: packed array [0..2047] of byte;

begin
  if PartLogFileExists then
    try
      if isOpen(LogPartFileF) then
        FileSeek(LogPartFileF, 0, fsFromBeginning)
      else
      begin
        LogPartFileF := FileOpen(PartFileName, fmOpenReadWrite or fmShareDenyNone);
        FileSeek(LogPartFileF, 0, fsFromBeginning);
      end;
    except
      PartLogFileExists := False;
    end;
  if PartLogFileExists then
  begin
    FPartReadFileName := ChangeFileExt(PartFileName, StandardPartReadFileext);
    if isOpen(LogPartReadFileF) then
      FileClose(LogPartReadFileF);
    LogPartReadFileF := FileCreate(FPartReadFileName);
    try
      try
        //ct:=0;
        if LogPartFileF >= 0 then
        begin
          size := FileSeek(LogPartFileF, 0, fsFromEnd);
          //units:=size div 2048;
          FileSeek(LogPartFileF, 0, fsFromBeginning);
          if LogPartReadFileF >= 0 then
          begin
            while size > 0 do
            begin
              len := FileRead(LogPartFileF, buffer, sizeof(buffer));
              FileWrite(LogPartReadFileF, buffer, len);
              size := size - len;
              //if units > 0 then
              //  ct:=ct+1;
            end;
          end;
        end;
      except
        on E: Exception do
        begin
          Log('wilog: PartCopyToRead: "' + E.Message + '"',
            LLError);
        end
      end;
    finally
      FileClose(LogPartReadFileF);
      PartReopen;
      Log('read file created', LLInfo);
    end;
  end;
end;

procedure TLogInfo.log2history(line: string);
begin
  if FWriteHistFile then
  begin
    //line := FormatDateTime('yyyy mmm dd hh:nn', Now) + '  ' + line;
    line := FormatDateTime('yyyy-mm-dd hh:nn', Now) + '  ' + line;
    //UTF8FixBroken(line);
    try
      WriteLogLine(HistroryFileF, line);
    except
      try
        LogDatei.Reopen;
        WriteLogLine(HistroryFileF, line);
      except
      end;
    end;
  end;
end;

function getComputerName: string;
{$IFDEF WINDOWS}
var
  regist: TRegistry;
{$ENDIF}
begin
{$IFDEF WINDOWS}
  Result := '';
  try
    regist := TRegistry.Create;
    regist.RootKey := HKEY_LOCAL_MACHINE;

    if regist.openKey('\SYSTEM\CurrentControlSet\Control\ComputerName\ComputerName',
      False) then
      Result := regist.ReadString('ComputerName');

    regist.CloseKey;
    regist.Free;

  except
  end;

  if Result = '' then
    Result := SysUtils.GetEnvironmentVariable('Computername');
{$ELSE}
  Result := 'unknown'; //########################################
    {$IFDEF OPSISCRIPT}
    {$IFDEF UNIX}
  Result := osconf.computername;
{$ENDIF LINUX}
    {$ENDIF OPSISCRIPT}
{$ENDIF}
end;



function TLogInfo.log_exception(E: Exception; LevelOfLine: integer): boolean;
var
  I: integer;
  Frames: PPointer;
  Report: string;
begin
  //Report := 'Program exception! ' + LineEnding +'Stacktrace:' + LineEnding + LineEnding;
  if E <> nil then
  begin
    Report := 'Exception class: ' + E.ClassName + LineEnding +
      'Message: ' + E.Message + LineEnding;
    log(Report, LevelOfLine);
  end;
  Report := BackTraceStrFunc(ExceptAddr);
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    Report := Report + LineEnding + BackTraceStrFunc(Frames[I]);
  log(Report, LevelOfLine);
end;

function getCallAddrStr: string;
  // derivated from DumpCallStack
  // http://wiki.freepascal.org/Logging_exceptions
var
  CallerAddress, bp: Pointer;
begin
  Result := '';
  bp := get_frame;
  // This trick skip SendCallstack item
  // bp:= get_caller_frame(get_frame);
  try
    CallerAddress := get_caller_addr(bp);
    if (CallerAddress <> nil) then
      Result := BackTraceStrFunc(CallerAddress) + LineEnding;
  except
    { prevent endless dump if an exception occured }
  end;
end;


begin
  syslogmessagetag := ' --- opsi-script on ' + getcomputername + ' -- ';

  for j := 1 to peakIndicatorLength do
    peakIndicator := peakIndicator + peakIndicatorChar;

  {$IFDEF WINDOWS}
  {$IFDEF OPSI}
  defaultStandardLogPath := 'c:\opsi.org\log\';
  defaultStandardMainLogPath := defaultStandardLogPath;
  defaultStandardPartLogPath := 'c:\opsi.org\log\';
  {$IFDEF OPSI_AS_USER}
  defaultStandardLogPath := 'c:\opsi.org\applog\';
  defaultStandardMainLogPath := defaultStandardLogPath;
  defaultStandardPartLogPath := defaultStandardLogPath;
  (*
  AppDataPath:='';
  SHGetFolderPath(0,CSIDL_APPDATA,0,SHGFP_TYPE_CURRENT,AppDataPath);
  //SHGetSpecialFolderPath(0,AppDataPath,CSIDL_LOCAL_APPDATA,false)
  if ForceDirectories(AppDataPath+'\opsi.org\log\') then
  begin
    defaultStandardLogPath := AppDataPath+'\opsi.org\log\';
    defaultStandardMainLogPath := defaultStandardLogPath;
    defaultStandardPartLogPath := defaultStandardLogPath;
  end;
  *)
  {$ENDIF OPSI_AS_USER}
  {$ELSE OPSI}
  defaultStandardLogPath := GetTempDir(False);
  defaultStandardMainLogPath := GetTempDir(False);
  defaultStandardPartLogPath := GetTempDir(False);
  {$ENDIF OPSI}
  {$ELSE WINDOWS}
  if 0 = fpGetEUid then
  begin
    {$IFDEF OPSI}
    defaultStandardLogPath := '/var/log/opsi-script/';
    defaultStandardMainLogPath := '/var/log/opsi-script/';
    defaultStandardPartLogPath := '/var/log/opsi-script/';
    {$IFDEF OPSI_AS_USER}
    defaultStandardLogPath := GetUserDir + '/.opsi.org/applog/';
    defaultStandardMainLogPath := defaultStandardLogPath;
    defaultStandardPartLogPath := defaultStandardLogPath;
    {$ENDIF OPSI_AS_USER}
    {$IFDEF OPSISCRIPTSTARTER}
    defaultStandardLogPath := '/var/log/opsi-client-agent/opsiclientd/';
    defaultStandardMainLogPath := '/var/log/opsi-client-agent/opsiclientd/';
    defaultStandardPartLogPath := '/var/log/opsi-client-agent/opsiclientd/';
    {$ENDIF OPSISCRIPTSTARTER}
    {$IFDEF OPSICLIENTAGENT}
    defaultStandardLogPath := '/var/log/opsi-client-agent/';
    defaultStandardMainLogPath := '/var/log/opsi-client-agent/';
    defaultStandardPartLogPath := '/var/log/opsi-client-agent/';
    {$ENDIF OPSICLIENTAGENT}
    {$ELSE OPSI}
    defaultStandardLogPath := '/tmp/';
    defaultStandardMainLogPath := '/tmp/';
    defaultStandardPartLogPath := '/tmp/';
    {$ENDIF OPSI}
  end
  else
  begin
    defaultStandardLogPath := '/tmp/';
    defaultStandardMainLogPath := '/tmp/';
    defaultStandardPartLogPath := '/tmp/';
    {$IFDEF OPSI_AS_USER}
    defaultStandardLogPath := GetUserDir + '/.opsi.org/applog/';
    defaultStandardMainLogPath := defaultStandardLogPath;
    defaultStandardPartLogPath := defaultStandardLogPath;
    {$ENDIF OPSI_AS_USER}
  end;
  {$ENDIF}

end.
