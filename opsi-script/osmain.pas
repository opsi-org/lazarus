
unit osmain;

{$MODE DELPHI}
//{$RANGECHECKS ON}
{$OVERFLOWCHECKS ON}
{$IOCHECKS ON}
{$OBJECTCHECKS ON}
{$VARSTRINGCHECKS ON}
{$LONGSTRINGS ON}



(* contains the main program code *)


(*

   ***********************************
  Copyright OpenSSL libraries for Indy

  Copyright 2000-2002,
  Gregor Ibic, gregor.ibic@intelicom.si
  Intelicom d.o.o.
  www.intelicom.si


  This library is tested on Indy 9.0 and higher.
  Support forum is avaiable at http://www.intelicom.si/forum

  Please note, that you have to specify the usage of Indy and IndySSL with a copyright notice
  in your applications.

  Comments and suggestions are welcome to my email.

  ***********************************

*)

// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: Rupert Roeder, detlef oertel




{$IFDEF WINDOWS} {$DEFINE GUI} {$ENDIF WINDOWS}

interface

uses
  //osfunc,
{$IFDEF WINDOWS}
  Windows,
  VersionInfoX,
  registry,
  osregistry,
  systemcriticalu,

{$IFDEF WIN32}
  osfuncwin2,
  osbackgroundinstall,
{$ENDIF WIN32}
  shellapi,
  wispecfolder,
  osfuncwin3,
{$ENDIF WINDOWS}
{$IFDEF UNIX}
  osprocessux,
  osprocesses,
  baseunix,
  oscrypt,
{$ENDIF UNIX}
{$IFDEF LINUX}
  osfunclin,
  lispecfolder,
{$ENDIF LINUX}
{$IFDEF DARWIN}
  osfuncmac,
  lispecfolder,

{$ENDIF DARWIN}
{$IFDEF GUI}
  osmessagedialog,
  osGUIControl,
  osbatchgui,
  Forms, Dialogs,
  ExtCtrls,
  StdCtrls,
  Controls,
  LCLIntf,
  Menus, Buttons, ComCtrls,
  LResources,
  //lcltranslator,
{$ENDIF GUI}
  osencoding,
  osconf,
  FileUtil,
  LazFileUtils,
  LazUTF8,
  SysUtils,
  Classes,
  synautil,
  oswebservice,
  //wirequlist,
  oslog,
  ostxstringlist,
  osparser,
  osparserhelper,
  osfunc,
  osnetutil,
  //IdSysLog,
  strutils,
  inifiles,
  osmeta;

type
  TProgramMode = (pmNotSet, pmInfo, pmStandard, pmHistoDialog, pmBuildPC_classic,
    pmBuildPC_service, pmBatch, pmSilent);

//type
(*
  { TCentralForm }

  TCentralForm = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Panel2: TPanel;
    Label3: TLabel;
    zipfiles: TListBox;
    TimerWait: TTimer;
    OpenDialog1: TOpenDialog;
    //IdSysLog1: TIdSysLog;
    MainMenu1: TMainMenu;
    Datei1: TMenuItem;
    Hilfe1: TMenuItem;
    Info1: TMenuItem;
    Skript1: TMenuItem;
    Beenden1: TMenuItem;
    ToolBar1: TToolBar;
    Panel3: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Edit1: TEdit;
    Label5: TLabel;
    LabelWinstVersion: TLabel;
    Label6: TLabel;
    Panel4: TPanel;
    Edit2: TEdit;
    Label4: TLabel;
    SpeedButton3: TSpeedButton;
    Button_show_gui: TSpeedButton;
    RadioGroup1: TRadioGroup;
    RadioButtonNewLogfile: TRadioButton;
    RadioButtonAppendLogfile: TRadioButton;
    SpeedButton5: TSpeedButton;
    ButtonProcessMemo: TButton;
    ButtonGetClipboardText: TButton;
    ComboBox1: TComboBox;
    SpeedButton4: TSpeedButton;
    LabelActivity: TLabel;
    procedure Label3Click(Sender: TObject);
    procedure Memo1Add(const s: string);
    procedure FormCreate(Sender: TObject);
        procedure Button_TerminateClick(Sender: TObject);
    procedure Button_ExecuteClick(Sender: TObject);
    procedure TerminateInteractive;
    procedure getScriptName;

    procedure TakeToSaveList(FName: string);

    procedure LZH1Progress(Sender: TObject; const FileName: string;
         const FileBytesWritten, FileBytes, TotalBytesWritten, TotalBytes: longint;
      var Cancel: boolean);
 *)
procedure ProcessProdukt(var extremeErrorLevel: TErrorLevel);


procedure BuildPC;
procedure LoginScripts;
procedure Productlist;
procedure StartProgramModes;
procedure GetParameter;

    {$IFNDEF GUI}
procedure main;
    {$ENDIF GUI}



    (*
    procedure TimerWaitTimer(Sender: TObject);
    procedure TimerWaitset(Interval: word);
    function TimerWaitready(WaitIntervals: word): boolean;
    procedure ButtonProcessMemoClick(Sender: TObject);
    procedure ButtonGetClipboardTextClick(Sender: TObject);
    procedure Button_BrowseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Beenden1Click(Sender: TObject);
    procedure Info1Click(Sender: TObject);
    procedure Skript1Click(Sender: TObject);
    procedure BitBtnViewLogfileClick(Sender: TObject);
    procedure Button_show_guiClick(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure Memo1Change(Sender: TObject);

  private
    { Private-Deklarationen }

  public
    { Public-Deklarationen }
  end;
 *)
procedure ProcessMess;
procedure TerminateApp;
procedure CreateSystemInfo;
function ValueOfEnvVar(const VarName: string): string;
procedure saveVersionToProfile;
function readVersionFromProfile: string;
function scriptWasExecutedBefore: boolean;
procedure SetAndSendProductProgress(const Verfahren: TActionRequest);
function makeAbsoluteScriptPath(const Pfad: string; const ScriptPath: string): string;
function GetPathToScript: string;




var

  // CentralForm: TCentralForm;
  // CentralFormVisible: boolean;
  Skriptdatei: string;
  scriptlist: TXStringList;
  usercontext: string;
  batchproductid: string = '';  //id of product we are running in batch mode
  logproductid: string = '';
  //id of product used for logging in batch mode without service
  usercontextSID: string;
  usercontextUser: string;
  usercontextDomain: string;
  NestingLevel: integer;
  ExtraParameter: string;

  ProductvarsForPC: TStringList;


  ShellIsExplorer: boolean;

  ParamFileLocalization: string;

  testresult: string;
  teststringlist: TStringList;
  //{$IFNDEF GUI}
  startupmessages: TStringList;
  //{$ENDIF GUI}

  {If the command "reloadProductList" is called within a script, the variable FlagReloadProductList
   is set to true. If FlagReloadProductList is true, the product list is reloaded from the server
   after the end of the script and the list is processed again, i.e. the procedure BuildPC is called again.
   As default this flag must be set to false.}
  FlagReloadProductList: boolean = False;

  {The list ProductsRunnedUsingReloadProductList is used for avoiding an endless loop
   if different products which use "reloadProducList" set each other to "setup"}
  ProductsRunnedUsingReloadProductList: TStringlist;

  runUpdate: boolean;
  DontUpdateMemo: boolean = False;
  PerformExitProgram: boolean = False;
  PerformExitWindows: TExitRequest = txrNoExit;
  PerformShutdown: TShutdownRequest = tsrNoShutdown;
  ProgramMode: TProgramMode;
  batchUpdatePOC: boolean = False;


  toggle: boolean;
  starttimestr: string;



resourcestring
  rsProductCheck = 'product check';

implementation

//{$R manifest.rc}

{$IFDEF GUI}
uses osshowsysinfo
{$IFDEF WIN32}
  //, osshowlog
{$ENDIF}
  , osinteractivegui;

{$ENDIF GUI}

var
  ///SaveddeText :   Textfile;
  //TimerWait_waitedIntervals: word;

  ///connected :   Boolean;
  LogDateiName: string;
  logfileFromCommandLine: boolean;
  ///ContinueLog  :   Integer;
  {$IFDEF WINDOWS}
  RegLogOutOptions: TuibRegistry;
  regist: TRegistry;
  {$ENDIF}


var
  IniFileLocalization: string;

  //Profildateiname: string;

  Produkt: string;

  //Verfahren  : TActionRequest;



///NumberOfErrors   :   Integer;
///NumberOfWarnings :   Integer;

///LogLevel :   Integer;
///ExitOnError :   Boolean;
///ReportMessages :   Boolean;


const
  //MaxSavedScriptFiles = 20;
  StandardIniFile = 'opsi-script.ini';


function ProgramModeToString(ProgramMode:TProgramMode):string;
begin
  case ProgramMode of
    pmNotSet: Result := 'pmNotSet';
    pmInfo: Result := 'pmInfo';
    pmStandard: Result := 'pmStandard';
    pmHistoDialog: Result := 'pmHistoDialog';
    pmBuildPC_classic: Result := 'pmBuildPC_classic';
    pmBuildPC_service: Result := 'pmBuildPC_service';
    pmBatch:Result := 'pmBatch';
    pmSilent: Result := 'pmSilent';
    else Result := 'Unknown ProgramMode';
  end;
end;

procedure saveVersionToProfile;
var
  myinifile: TIniFile;
  mypath: string;
begin
  mypath := GetAppDataPath + PathDelim + '.opsi.org';
  if not SysUtils.ForceDirectories(mypath) then
  begin
    mypath := GetUserProfilePath + PathDelim + '.opsi.org';
    SysUtils.ForceDirectories(mypath);
  end;
  myinifile := TIniFile.Create(mypath + PathDelim + 'userLoginScripts.ini');
  if opsidata <> nil then
    myinifile.WriteString('runstamps', Topsi4data(opsidata).getActualProductId,
      Topsi4data(opsidata).getActualProductVersion);
  myinifile.updatefile;
  myinifile.Free;
end;

function readVersionFromProfile: string;
var
  myinifile: TIniFile;
  mypath: string;
begin
  Result := '';
  mypath := GetAppDataPath + PathDelim + '.opsi.org';
  if not FileExists(mypath + PathDelim + 'userLoginScripts.ini') then
    mypath := GetUserProfilePath + PathDelim + '.opsi.org';
  if FileExists(mypath + PathDelim + 'userLoginScripts.ini') then
  begin
    myinifile := TIniFile.Create(mypath + PathDelim + 'userLoginScripts.ini');
    if opsidata <> nil then
      Result := myinifile.ReadString('runstamps', Topsi4data(
        opsidata).getActualProductId, '');
    myinifile.updatefile;
    myinifile.Free;
  end;
end;


function scriptWasExecutedBefore: boolean;
begin
  Result := False;
  // check old stamp
  if opsidata <> nil then
    if Topsi4data(opsidata).getActualProductVersion = readVersionFromProfile then
      Result := True;
  // now save actual stamp
  saveVersionToProfile;
end;


function booleanToStr(b: boolean): string;
begin
  if b then
    Result := 'true'
  else
    Result := 'false';
end;

function fillString(const s: string; fulllength: integer): string;

var
  i: integer;

begin

  Result := s;

  for i := 1 to length(s) - fulllength do
    Result := Result + ' ';

end;

function setBootmode(const bootmode: string; var problem: string): boolean;
{$IFDEF UNIX}
var
  myconf: TIniFile;
{$ENDIF UNIX}
begin
  {$IFDEF WINDOWS}
  Result := True;
  try
    regist := TRegistry.Create;
    regist.RootKey := HKEY_LOCAL_MACHINE;

    if regist.openKey(opsiorggeneralkey, False) then
    begin
      regist.WriteString('bootmode', bootmode);
    end
    else
    begin
      problem := 'registry key ' + opsiorggeneralkey + ' could not be openend.';
      Result := False;
    end;

    regist.closeKey;
    regist.Free;


  except
    on E: Exception do
    begin
      problem := 'exception while working in registry key ' +
        opsiorggeneralkey + ' error ' + e.message;
      LogDatei.log(problem, LLError);
      Result := False;
    end;
  end;
  {$ENDIF}
  {$IFDEF UNIX}
  try
    if FileExists(opsiclientagentconf) then
    begin
      myconf := TIniFile.Create(opsiclientagentconf);
      myconf.WriteString('opsi-script', 'bootmode', bootmode);
      myconf.Free;
      Result := True;
    end
    else
      LogDatei.log('File not found ' + opsiclientagentconf, LLwarning);
  except
    on E: Exception do
    begin
      problem := 'exception while writing ' + opsiscriptconf +
        ' error ' + e.message;
      Result := False;
    end;
  end;
  {$ENDIF UNIX}
end;


procedure getBootmode(var bootmode: string; var fromRegistry: boolean);
{$IFDEF UNIX}
var
  myconf: TIniFile;
{$ENDIF UNIX}
begin
{$IFDEF WINDOWS}
  fromRegistry := True;

  try
    regist := TRegistry.Create;
    regist.RootKey := HKEY_LOCAL_MACHINE;

    if regist.openKey(opsiorggeneralkey, False) then
    begin
      bootmode := regist.ReadString('bootmode');
      if bootmode = '' // Eintrag (noch) nicht vorhanden
      then
        fromRegistry := False;
    end
    else
      fromRegistry := False;

    regist.Free;
  except
    fromRegistry := False
  end;

  if fromRegistry then
  begin
    LogDatei.log('bootmode from registry: ' + bootmode, LLDebug);
  end
  else
  begin
    bootmode := ValueOfEnvVar('BOOTMODE');
    LogDatei.log('bootmode from environment: ' + bootmode, LLDebug);
  end;
{$ENDIF}
{$IFDEF UNIX}
  try
    if FileExists(opsiclientagentconf) then
    begin
      myconf := TIniFile.Create(opsiclientagentconf);
      bootmode := myconf.ReadString('opsi-script', 'bootmode', 'BKSTD');
      myconf.Free;
    end
    else
      LogDatei.log('File not found ' + opsiclientagentconf, LLwarning);
  except
    on E: Exception do
    begin
      LogDatei.log('exception while reading ' + opsiclientagentconf +
        ' error ' + e.message, LLERROR);
    end;
  end;
  {$ENDIF UNIX}
end;

function determinateRemoteLogging: boolean;

//var
  //registval: longint;
  ///properties  :   TStringList;
begin
  Result := True;
{$IFDEF WINDOWS}
  try
    regist := TRegistry.Create;
    regist.RootKey := HKEY_LOCAL_MACHINE;

    Result := regist.openKey(opsiorgSyslogDKey, False);
  except
    Result := False;
  end;

  regist.CloseKey;
  regist.Free;
{$ENDIF}
end;




function GetTempPath: string;
var
  testpassed: boolean;
  teststr: string;
  mytemppath: string;
begin
  mytemppath := StandardTempPath;
  {$IFDEF LINUX}
  if getLinuxDistroName = 'Univention' then
  begin
    if logdatei <> nil then
      logdatei.log('Univention detected: switching to temp path: ' +
        mytemppath, LLdebug);
    mytemppath := UniventionTempPath;
  end;
  {$ENDIF}
  testpassed := False;
  if (mytemppath <> '') and SysUtils.ForceDirectories(mytemppath) then
  begin
    if logdatei <> nil then
      logdatei.log('Testing as temp path: ' + mytemppath, LLdebug);
    testpassed := True;
    teststr := mytemppath + 'testing_write_privilege_for_opsiscript';
    if not fileexists(teststr) then
    begin
      if not SysUtils.ForceDirectories(teststr) or not RemoveDir(teststr) then
      begin
        testpassed := False;
        if logdatei <> nil then
          logdatei.log('Failed:Testing as temp path: ' + mytemppath, LLwarning);
      end
      else
      if logdatei <> nil then
        logdatei.log('Succeeded: Testing as temp path: ' + mytemppath +
          ' (' + DateTimeToStr(Now) + ')', LLdebug);
    end;
    teststr := '';
  end;

  if testpassed then
    TempPath := mytemppath
  else
    TempPath := ValueOfEnvVar('TEMP') + PATHSEPARATOR;
  SysUtils.ForceDirectories(TempPath);
  if not DirectoryExists(TempPath) then
  begin
    {$IFDEF WINDOWS}
    TempPath := 'c:\tmp\';
    {$ELSE}
    TempPath := '/tmp/opsiscript/';
    {$ENDIF WINDOWS}
    SysUtils.ForceDirectories(TempPath);
  end;
  if logdatei <> nil then
    logdatei.log('Final: Using as temp path: ' + TempPath, LLdebug);
  Result := TempPath;
end;

procedure ProcessMess;
begin
  {$IFDEF GUI}
  Application.ProcessMessages;
  //Sleep(10);
  {$ENDIF GUI}
end;

procedure CreateSystemInfo;
begin
  {$IFDEF GUI}
  SystemInfo := TSystemInfo.Create(Application);
  {$ENDIF GUI}
end;

procedure TerminateApp;
begin
  try
    if Assigned(ProductsRunnedUsingReloadProductList) then FreeAndNil(ProductsRunnedUsingReloadProductList);
    if LogDatei <> nil then
    begin
      LogDatei.LogSIndentLevel := 0;
      LogDatei.log('============  opsi-script ' + OpsiscriptVersionname +
        ' is regularly exiting. Time ' + FormatDateTime(
        'yyyy-mm-dd  hh:mm:ss ', now) + 'Exitcode: '+inttostr(system.ExitCode)+' .', LLessential);

      LogDatei.Close;
      sleep(500);
      FreeAndNil(LogDatei);
      {$IFDEF WINDOWS}
        SystemCritical.IsCritical := False;
      {$ENDIF WINDOWS}
    end;
    try
      {$IFDEF GUI}
      // Application is visible in GUI mode and
      // Application.terminate provides the exitcode
      // exit uses system.exitcode as exitcode but
      // exit does not work with Application
      Application.Terminate;
      halt(system.ExitCode);
      {$ELSE GUI}
      // Application is not visible in console mode and exit works
      Exit;
      {$ENDIF GUI}
      // fallback if nothing else works:
      halt(system.ExitCode);
    except
      halt(system.ExitCode);
    end;
  except
    try
      halt(system.ExitCode);
    except
      halt(system.ExitCode);
    end;
  end;
end;


function ValueOfEnvVar(const VarName: string): string;

(*
Var
  requiredLength :   Integer;

Function GetEnVar (Const VarName : String; Var Value : String; MaxLength : Integer; Var
           RequiredLength : Integer) :   Integer;



Var
  lpName :   PChar;
  lpBuffer :   PChar;
  nSize :   DWord;

  ReturnSize :   DWord;

Begin
  lpName := PChar (Varname);
  nSize := MaxLength + 1;
  GetMem (lpBuffer, nSize);
{$IFDEF FPC}
  ReturnSize := GetEnvironmentVariable(lpName, lpBuffer, nSize);
{$ELSE}
  ReturnSize := winprocs.GetEnvironmentVariable(lpName, lpBuffer, nSize);
{$ENDIF}


  If ReturnSize = 0
    Then
    Begin
      Value := valueEnvVarNotFound;
      requiredLength := 0;
      result := -1;
    End
  Else
    If ReturnSize < nSize
      Then
      Begin
        Value := StrPas (lpBuffer);
        RequiredLength := ReturnSize;
        result := 1;
      End
  Else
    Begin
      Value :=
           IntToStr (nSize) + ' /* required ' + IntToStr (ReturnSize) + ' */';
      RequiredLength := ReturnSize - 1;
      result := 0;
    End;


  FreeMem (lpBuffer);
  lpBuffer := Nil;

End;
 *)
begin
  {$IFDEF WINDOWS}
  Result := GetEnvironmentVariable(VarName);

  {$ENDIF WINDOWS}
  {$IFDEF UNIX}
  Result := strpas(fpGetEnv(VarName));
  {$ENDIF LINUX}

  if Result = '' then
    Result := valueEnvVarNotFound;
  (*
  If GetEnVar (VarName, result, 255, RequiredLength) = 0
    Then
    GetEnVar (VarName, result, RequiredLength + 1, RequiredLength )
  *)
end;




procedure extractTmpPathFromLogdatei(var Logdateiname: string);
begin
  TmpPathFromLogdatei := ExtractFilePath(ExpandFilename(LogDateiname));
end;

(*

procedure TCentralForm.TimerWaitset(Interval: word);
begin
  TimerWait_waitedIntervals := 0;
  TimerWait.Enabled := True;
  TimerWait.Interval := Interval;
end;

function TCentralForm.TimerWaitready(WaitIntervals: word): boolean;
begin
  if TimerWait_waitedIntervals < WaitIntervals then
    Result := False
  else
  begin
    Result := True;
    TimerWait.Enabled := False;
  end;
end;

procedure TCentralForm.TimerWaitTimer(Sender: TObject);
begin
  Inc(TimerWait_waitedIntervals);
end;

*)
function makeAbsoluteScriptPath(const Pfad: string; const ScriptPath: string): string;
  // take ScriptPath if it is absolute, else  pfad + scriptPath
  // return '' if scriptpath is ''

  // it is assumed that pfad ends with PathDelim

var
  expandedScriptname: string;
  partsExpanded, partsScriptpath: TXStringList;
  i: integer;
  equal: boolean;
  ///delim :   String;
begin
  Result := '';
  if ScriptPath <> '' then
  begin
    // is scriptpath already an absolute file path?

    expandedScriptname := expandFilename(ScriptPath);

    partsExpanded := TXStringList.Create;
    partsScriptpath := TXStringList.Create;


    Stringsplit(expandedScriptname, pathdelim, partsExpanded);
    Stringsplit(scriptPath, pathdelim, partsScriptPath);


    i := 0;
    equal := True;
    if partsScriptpath.Count <> partsExpanded.Count then
      equal := False;


    while equal and (i < partsScriptpath.Count) do
    begin
      if uppercase(partsScriptPath.Strings[i]) <> uppercase(partsExpanded.Strings[i])
      then
        equal := False;
      Inc(i);
    end;

    partsExpanded.Free;
    partsScriptpath.Free;

    if equal
    //if UpperCase (expandedScriptname) = ScriptPath
    // ScriptPath absolute path?? then take it
    then
      Result := ExpandFileName(ScriptPath)
    else
      Result := ExpandFileName(Pfad + ScriptPath);

  end;
end;

procedure SetAndSendProductProgress(const Verfahren: TActionRequest);
// set the progress according to the kind of request
// and calls to send a update of the productOnClient object to the server
begin
  case Verfahren of
    tacSetup: opsidata.setProductProgress(tppInstalling);
    tacDeinstall: opsidata.setProductProgress(tppUninstalling);
    tacOnce: opsidata.setProductProgress(tppInstalling);
    tacAlways: opsidata.setProductProgress(tppInstalling);
    tacCustom: opsidata.setProductProgress(tppInstalling);
    tacLogin: opsidata.setProductProgress(tppInstalling);
    tacUpdate: opsidata.setProductProgress(tppInstalling);
  end;
end;

function GetPathToScript: string;
var
  Pfad: string;
begin
    //only for backward compatibility and for special circumstances
  {$IFDEF UNIX}
    if not DirectoryExists(depotdrive) then
    begin
      LogDatei.log('Setting depotdrive from: ' + depotdrive + ' to: ' + depotdrive_old,
        LLdebug2);
      depotdrive := depotdrive_old;
    end;
  {$ENDIF LINUX}

    Pfad := opsidata.getSpecialScriptPath;
    if Pfad = '' //this should be the normal case since winst 4.2
    then
      // take pfad from depotdrive + depotdir (coming from registry)
    begin
      Pfad := depotdrive + depotdir;

      if Pfad[length(Pfad)] <> DirectorySeparator then
        Pfad := Pfad + PathDelim;
      Pfad := Pfad + Produkt;
      if Pfad[length(Pfad)] <> PathDelim then
        Pfad := Pfad + PathDelim;
    end;
  Result:=Pfad;
end;

function CheckForProcessProduct:boolean;
begin
  Result := True; //Process product?
{$IFDEF WIN32}
  // start with background handling:
  // are we in a background situation (users are logged in) ?
  if userAreLoggedIn then
  begin
    LogDatei.log(
      'logged in users detected - so we check about background install',
      LLnotice);
    // is background-install enabled an licensed ?
    if isBackgroundinstall_enabled then
    begin
      LogDatei.log(
      'background install enabled',
      LLnotice);
      // check license module for background_install
      if opsidata.backgroundInstallActivated then
      begin
         LogDatei.log(
      'background install licensed',
      LLnotice);
        // do we have the meta data ?
        if metaDataFound(ExtractFileDir(makeAbsoluteScriptPath(GetPathToScript,opsidata.getProductScriptPath(opsidata.getProductActionRequest)))) then
        begin
          LogDatei.log(
            'meta data found: use it for background install',
            LLnotice);
          // handle background install
          checkAndHandleRunningProductProcesses(osmeta.CheckDirs,
            osmeta.processes, Result);
           LogDatei.log(
            'background install check finished - install: ' + BoolToStr(Result,true),
            LLnotice);
        end
        else
        begin
          LogDatei.log(
            'Background install situation, but no opsi-meta-data file found.',
            LLwarning);
          LogDatei.log(
            'So we defer the installation.', LLwarning);
          scriptdeferstate:= True;
          Result:=False;
        end;
      end
      else
      begin
        LogDatei.log(
          'background install enabled but no license - so nothing to do about background install',
          LLError);
        LogDatei.log(
          'Either set config backroundinstall_enabled=false or import license for background_install.',
          LLError);
        Result := False;
      end;
    end
    else
    begin
       LogDatei.log(
        'background install not enabled - so nothing to do about background install',
        LLdebug);
       // The result has to be true here
    end;
  end
  else
  begin
    LogDatei.log(
      'No useres are logged in - so nothing to do about background install',
      LLdebug);
  // end with background handling:
  end;
  {$ENDIF WIN32}
  LogDatei.log('CheckForProcessProduct: '+ BoolToStr(Result, True), LLInfo);
end;

procedure ProcessProdukt(var extremeErrorLevel: TErrorLevel);

var
  Verfahren: TActionRequest;
  Pfad: string;
  scriptname, absscriptname: string;



  function ProcessNonZeroScript(Skriptdateiname: string;
  var extremeErrorLevel: TErrorLevel): boolean;

  var
    Scriptname: string;
  begin
    Logdatei.log_prog('Start ProcessNonZeroScript', LLdebug2);
    if Skriptdateiname = '' then
    begin
      Result := False;
      Logdatei.log('no script file name given', LLnotice);
    end
    else
    begin
      Result := True;
      Scriptname := Skriptdateiname;
      NestingLevel := 0;
      ScriptConstants.Init(Scriptname);
      CreateAndProcessScript(Scriptname, NestingLevel, False, extremeErrorLevel);
      Logdatei.log_prog('After CreateAndProcessScript', LLdebug2);
    end;
    Logdatei.log_prog('End ProcessNonZeroScript', LLdebug2);
  end;

begin
  //updateAfterSetup := false;
  Logdatei.log_prog('Entering ProcessNonZeroScript', LLdebug2);

  runUpdate := True;

  if runloginscripts then
    Verfahren := tacLogin
  else
  if runproductlist then
    Verfahren := tacSetup
  else
    Verfahren := opsidata.getProductActionRequest;
  Logdatei.log('Actionrequest for Product: ' + Produkt + ' is: ' +
    opsidata.actionRequestToString(Verfahren), LLInfo);
  if Verfahren = tacNone then
    exit;

  ProductvarsForPC := opsidata.getProductproperties;
  if not opsidata.initProduct then
  begin
    extremeErrorLevel := levelFatal;
    LogDatei.log(
        'failed opsidata.initProduct', LLcritical);
  end;
  LogDatei.log('ProcessNonZeroScript opsidata initialized', LLdebug2);

  Pfad:=GetPathToScript;


  if Pfad <> '' then
  begin


    if runloginscripts then
      Verfahren := tacLogin
    else
    if runproductlist then
      Verfahren := tacSetup
    else
      Verfahren := opsidata.getProductActionRequest;


    // this switches state and progress
    if Verfahren in [tacDeinstall, tacSetup, tacAlways] then
      opsidata.SetProductProgressByActionrequest(Verfahren);
      SetAndSendProductProgress(Verfahren);


    if Verfahren in [tacDeinstall, tacSetup, tacOnce, tacAlways,
      tacCustom, tacLogin] then
    begin
      scriptname := opsidata.getProductScriptPath(Verfahren);
      Logdatei.log('scriptname: "' + scriptname + '", special path: "' +
        pfad + '"', LLInfo);
      absscriptname := makeAbsoluteScriptPath(Pfad, scriptname);
      if (Verfahren in [tacDeinstall, tacSetup, tacOnce, tacAlways, tacCustom]) or
        ((Verfahren = tacLogin) and (scriptname <> '')) then
        if not ProcessNonZeroScript(absscriptname, extremeErrorLevel)
        //meaning in the moment that scriptname is empty
        then
        begin
          extremeErrorLevel := LevelFatal;
          LogDatei.log('Error level set to fatal, action type ' +
            opsidata.actionRequestToString(Verfahren), LLCritical);
        end;
    end;
    LogDatei.log('First ProcessNonZeroScript finished', LLDebug2);
    //LogDatei.log('Setup script name: '+opsidata.getProductScriptPath(tacSetup), LLessential);


    if (Verfahren = tacUpdate) or ((Verfahren = tacSetup) and
      (PerformExitWindows <= txrReboot)) then
      //the setup script does not require a direct Reboot or Logout, and seems to be finished right now
      if extremeErrorLevel = LevelFatal then
      begin
        LogDatei.log(
          'We do not look for a update script, because the setup script is failed',
          LLnotice);
      end
      else
      if not runUpdate then
        LogDatei.log('We do not look for a update script, because noUpdateScript is set',
          LLnotice)
      else
      begin
        opsidata.setProductActionRequest(tacUpdate);
        LogDatei.log('product ' + Produkt + ' set to update', LLessential);
        LogDatei.log('get Update script name ...', LLdebug2);
        scriptname := opsidata.getProductScriptPath(tacUpdate);
        LogDatei.log('Update script name: ' + opsidata.getProductScriptPath(
          tacUpdate), LLdebug2);
        absscriptname := makeAbsoluteScriptPath(Pfad, scriptname);
        LogDatei.log('Update script name: ' + absscriptname, LLessential);

        if absscriptname <> '' then
        begin
          // get new productOnClient with update switch
          opsidata.initProduct;
          //updateAfterSetup := true;
          if not ProcessNonZeroScript(absscriptname, extremeErrorLevel) then
            Logdatei.log('start of update script failed.', LLWarning);
          // if the original action request was setup set it back to setup (instead of update)
          // so that lastAction will be setup
          if (Verfahren = tacSetup) then
          begin
            opsidata.setActualProductActionRequest('setup');
            Logdatei.log('Set lastAction to setup', LLessential);
          end;
        end
        else
        begin
          Logdatei.log('we have no update script', LLessential);
          //updateAfterSetup := false;
        end;
      end;
  end;
end;


procedure BuildPC;

var
  i: integer;
  Bootmode: string;
  bootmodeFromRegistry: boolean;
  Fehler: string;
  numberValue: string;
  errorNumber: integer;
  extremeErrorLevel: TErrorLevel;
  showErrorMessages: boolean;
  tmpstr, cmdstr: string;
  buildpcscript: TuibInstScript;
  tmplist: TStringList;
  Produkte: TStringList = nil;
  initialProductState: string;
  goOn: boolean;
  problemString: string;
  aktActionRequestStr: string;
  aktAction, orgAction: TActionRequest;
  processProduct: boolean;
  list: TStringList;
  excludedProducts: TStringList;
  productscopy: TStringList;
  opsiclientd: boolean;
  ScriptDatei : string;
  {$IFDEF WINDOWS}
  regDataType: tuibRegDataType;
  {$ENDIF WINDOWS}
  {$IFDEF UNIX}
  filehandle: cint;
  TheExitMode: TExitMode;
  {$ENDIF UNIX}



  procedure LogProductSequence(const produkte: TStringList;
    errorfound: boolean; numberOfCorrectItems: integer);

  var
    i: integer;
    Zeile: string;
    SaveProductname: string;
    requestedAction: TActionRequest;
  begin
    LogDatei.LogProduktId := False;
    SaveProductname := Topsi4data(opsidata).getActualProductId;
    LogDatei.log('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~',
      LLinfo);
    if errorfound then
      LogDatei.log('Error in the conditions for the sequence of products, ' +
        ' the sorted list of maximum length is:',
        LLinfo)
    else
      LogDatei.log('Resolved sequence of products (' + DateTimeToStr(Now) + '): ',
        LLinfo);

    i := 0;
    while i < numberOfCorrectItems do
    begin
      LogDatei.log_prog(IntToStr(i)+' productId ' + produkte.strings[i],
        LLinfo);
      // set productname to get action via opsidata
      opsidata.setActualProductName(produkte.strings[i]);

      requestedAction := opsidata.getProductActionRequest;
      LogDatei.log_prog(IntToStr(i)+' productId ' + produkte.strings[i]+
      ' : ' + opsidata.actionRequestToString(requestedAction),
        LLinfo);
      if errorfound or (requestedAction <> tacNone) then
      begin
        Zeile := 'Product ' + IntToStr(i) + ' ' + #9 + Produkte.Strings[i] +
          ' : ' + opsidata.actionRequestToString(requestedAction);
        LogDatei.log(Zeile, LLinfo);
      end;

      Inc(i);
    end;
    LogDatei.log('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~', LLinfo);

    opsidata.setActualProductName(SaveProductname);
  end;

  function ChangeProductstatusOnReinst(const Bootmode: string): boolean;

  var
    i: integer;
    productState: TProductState;
    productActionRequest: TActionRequest;
    excludedProducts: TStringList;
    productscopy: TStringList;
  begin
    Result := False;
    LogDatei.LogProduktId := False;
    //LogDatei.log('bootmode ' + bootmode, LLInfo);
    if Bootmode = 'REINS' then
    begin
      {$IFDEF GUI}
      FBatchOberflaeche.SetMessageText('Reloading product list after reinstallation...',
        mInfo);
      //FBatchOberflaeche.setInfoLabel('Reloading product list after reinstallation...');
      {$ENDIF GUI}
      LogDatei.log('setting all on-products to setup', LLessential);
      for i := 1 to Produkte.Count do
      begin
        Produkt := Produkte.Strings[i - 1];
        LogDatei.log('inspecting product "' + Produkt, LLessential);

        opsidata.setActualProductName(Produkt);

        productState := opsidata.getProductState;
        productActionRequest := opsidata.getProductActionRequest;

        LogDatei.log('product "' + Produkt + '" has state ' +
          opsidata.stateToString(productState) + ', action request is ' +
          opsidata.actionRequestToString(productActionRequest)
          , LLessential);

        if (produkt <> selfProductName) and (produkt <> 'opsi-winst') then
        begin
          if (productState in [tpsInstalled, tpsUnkown]) then
          begin
            opsidata.setProductState(tpsNotInstalled);
            if productActionRequest in [tacNone, tacUpdate] then
            begin
              opsidata.setProductActionRequestWithDependencies(tacSetup);
              LogDatei.log('product "' + Produkt + '" set to setup',
                LLessential);
            end;
          end;
        end;
      end;

      opsidata.saveOpsiConf;
      // reload the new productlist
      Produkte := OpsiData.getListOfProductIDs;
      if runprocessproducts then
      begin
        scriptlist.Delimiter := ',';
        //LogDatei.log('Processing is limited to the following products: ' + scriptlist.DelimitedText, LLessential);
        excludedProducts := TStringList.Create;
        productscopy := TStringList.Create;
        productscopy.Text := Produkte.Text;
        excludedProducts.Delimiter := ',';
        //LogDatei.log('Found productOnClients: ' + Produkte.DelimitedText, LLDebug2);
        stringlistintersection(productscopy, scriptlist, excludedProducts, Produkte);
        //LogDatei.log('We will not process: ' + excludedProducts.DelimitedText, LLDebug2);
        //LogDatei.log('Process possible: ' + Produkte.DelimitedText, LLessential);
        excludedProducts.Free;
        productscopy.Free;
      end;
    end;
    Result := True;
    // no errors
  end;// End of function ChangeProductstatusOnReinst


begin
  try
    {$IFDEF WINDOWS}
    SystemCritical.IsCritical := True;
    {$ENDIF WINDOWS}
    goOn := True;
    {$IFDEF GUI}
    // in build pc mode we want stay on top
    FBatchOberflaeche.SetForceStayOnTop(True);
    {$ENDIF GUI}
    DontUpdateMemo := True;
    FlagReloadProductList := False;
    OpsiData.setActualClient(computername);
    Produkte := OpsiData.getListOfProductIDs;
    if runprocessproducts then
    begin
      scriptlist.Delimiter := ',';
      LogDatei.log('Processing is limited to the following products: ' +
        scriptlist.DelimitedText, LLessential);
      excludedProducts := TStringList.Create;
      productscopy := TStringList.Create;
      productscopy.Text := Produkte.Text;
      excludedProducts.Delimiter := ',';
      LogDatei.log('Found productOnClients: ' + Produkte.DelimitedText, LLDebug2);
      stringlistintersection(productscopy, scriptlist, excludedProducts, Produkte);
      LogDatei.log('We will not process: ' + excludedProducts.DelimitedText, LLDebug2);
      LogDatei.log('Process possible: ' + Produkte.DelimitedText, LLessential);
      excludedProducts.Free;
      productscopy.Free;
    end;
    LogDatei.log('Computername:' + computername, LLessential);
    if computername <> ValueOfEnvVar('computername') then
      LogDatei.log('Computername according to Environment Variable :' +
        ValueOfEnvVar('computername'),
        LLessential);
    if opsiserviceURL <> '' then
      LogDatei.log('opsi service URL ' + opsiserviceurl,
        LLessential);
    LogDatei.log('Depot path:  ' + depotdrive + depotdir, LLinfo);
    LogDatei.log('', LLinfo);
    {$IFDEF GUI}
    FBatchOberflaeche.SetMessageText(rsProductCheck, mInfo);
    {$ENDIF GUI}
    ProcessMess;
    getBootmode(bootmode, bootmodeFromRegistry);
    LogDatei.log('Bootmode: ' + bootmode, LLinfo);
  except
    on e: Exception do
    begin
      LogDatei.log('exception in BuildPC: starting ' + e.message, LLError);
      {$IFDEF WINDOWS}
      SystemCritical.IsCritical := False;
      {$ENDIF WINDOWS}
    end;
  end;

  try
    //walk through all products
    if goOn then
    begin
      //If Produkte <> Nil Then Produkte.free;
      //Produkte := OpsiData.getListOfProducts;

      if ChangeProductstatusOnReinst(Bootmode) then

        //If Bootmode = 'REINS' then change Productstatus to 'setup' if it was 'on'.
        //The purpose is that the request that the product shall be installed will be saved
        //even if the bootmode has become 'BKSTD' following a reboot

      begin
        if (Bootmode = 'REINS')
        {$IFDEF WINDOWS}
          and bootmodeFromRegistry
        {$ENDIF WINDOWS}
        then
          if not setBootmode('BKSTD', Fehler) then
          begin
            LogDatei.log('Bootmode could not be set, ' + Fehler, LLError);
          end;
      end;



      if Produkte.Count > 0 then
      begin

        if OpsiData.sortByServer then
        begin
          LogProductSequence(Produkte, False, Produkte.Count);
        end
        else
        begin
          LogDatei.log('Got no product sorting by server.', LLError);
        end;
      end;

      i := 1;
      while (i <= Produkte.Count) and (PerformExitWindows < txrReboot) and
        (not PerformExitProgram) do
      begin
        processProduct := False;
        Produkt := Produkte.Strings[i - 1];
        opsidata.setActualProductName(Produkt);
        // get the actionrequest from the original productlist created at startup
        orgAction := opsidata.getProductActionRequest;
        if (orgAction <> tacNone) then
        begin

          if trim(Produkt) = '' then
            LogDatei.log('product ' + IntToStr(i - 1) + ' is "" ', LLWarning);
          opsidata.InitProduct;
          // check if there is still an action request if we had one at startup
          // get the actual (live) actionrequest
          aktActionRequestStr := opsidata.getActualProductActionRequest;
          aktAction := opsidata.actionRequestStringToActionRequest(aktActionRequestStr);
          Logdatei.log('Actionrequest for product: ' + Produkt +
            ' is (original/actual): (' + opsidata.actionRequestToString(
            orgAction) + ' / ' + aktActionRequestStr + ')', LLInfo);
          ScriptDatei := ExtractFileDir(makeAbsoluteScriptPath(GetPathToScript,
          opsidata.getProductScriptPath(opsidata.getProductActionRequest)));
          ScriptConstants.Init(ScriptDatei);
          // process product only if we have a original action request which is still set
          if (aktAction <> tacNone) and (orgAction <> tacNone) and CheckForProcessProduct() then
            processProduct := True
          else
            processProduct := False;
        end;
        initialProductState := opsidata.getActualProductInstallationState;
        LogDatei.log('initialProductState: ' + initialProductState, LLInfo);
        if processProduct then
        begin
          LogDatei.log('BuildPC: process product .....', LLDebug3);
          extremeErrorLevel := Level_not_initialized;
          logdatei.ActionProgress := '';
          {$IFDEF GUI}
          // maximize window
          BatchWindowMode := bwmMaximized;
          SavedBatchWindowMode := BatchWindowMode;
          FBatchOberflaeche.SetBatchWindowMode(BatchWindowMode);
          ProcessMess;
          {$ENDIF GUI}
          LogDatei.LogProduktId := True;
          LogDatei.log('opsidata.getProductState: '
             + opsidata.stateToString(opsidata.getProductState), LLInfo);

          ProcessProdukt(extremeErrorLevel);


          if (PerformExitWindows < txrImmediateLogout)
          then
          begin
            LogDatei.log_prog('BuildPC: standard update switches .....', LLDebug);
            opsidata.UpdateSwitches(extremeErrorLevel, logdatei.actionprogress);
          end
          else LogDatei.log('BuildPC: will not update switches .....', LLDebug);

          LogDatei.log_prog('BuildPC: finishProduct .....', LLDebug);
          opsidata.finishProduct;
          LogDatei.LogProduktId := False;
        end;
         // store results to webservice
          if (scriptfailed) then
          begin
            LogDatei.log('installation failed: set product state: unknown'
             + initialProductState, LLInfo);
            opsidata.ProductOnClient_update(opsidata.getActualProductActionRequest,  // progress
                                            tarFailed,    // result
                                            opsidata.actionRequestStringToActionRequest('None'),
                                            opsidata.actionRequestStringToActionRequest(opsidata.getActualProductLastActionRequest),
                                            opsidata.stateStringToState('unknown'));  // state
          end
          else if (scriptdeferstate) then
          begin
            LogDatei.log('installation deferred: set progress and restore the initial product state: '
             + initialProductState, LLInfo);
            opsidata.ProductOnClient_update('Deferred',  // progress
                                            tarNone,    // result
                                            opsidata.actionRequestStringToActionRequest(opsidata.getActualProductActionRequest),
                                            opsidata.actionRequestStringToActionRequest(opsidata.getActualProductLastActionRequest),
                                            opsidata.stateStringToState(initialProductState));  // state
          end
          else if (scriptsuspendstate) then
          begin
            LogDatei.log('installation suspended: set progress and restore the initial product state: '
             + initialProductState, LLInfo);
            opsidata.ProductOnClient_update('Suspended',
                                            tarNone,
                                            opsidata.actionRequestStringToActionRequest(opsidata.getActualProductActionRequest),
                                            opsidata.actionRequestStringToActionRequest(opsidata.getActualProductLastActionRequest),
                                            opsidata.stateStringToState('unknown'));
          end;

        // At the recursive call to BuildPC we have lost the Produkte list
        // as a dirty hack we always reload here and do not increment the counter
        //Produkte := OpsiData.getListOfProductIDs;
        Inc(i);
      end;


      LogDatei.log('BuildPC: saveOpsiConf .....', LLDebug3);
      opsidata.saveOpsiConf;

      Produkte.Free;
      Produkte := nil;
      //Produktestatus.free; Produktestatus := nil;
    end;
  except
    on e: Exception do
    begin
      LogDatei.log('exception in BuildPC: walk through all products  ' +
        e.message, LLError);
      {$IFDEF WINDOWS}
      SystemCritical.IsCritical := False;
      {$ENDIF WINDOWS}
    end;
  end;

  try

    LogDatei.log('BuildPC: saveOpsiConf .....', LLDebug3);
    opsidata.saveOpsiConf;
    //SaveProfildatei;

    errorNumber := 0;

    // LINUX see below
  {$IFDEF WINDOWS}
    LogDatei.log('BuildPC: handle reboot options: write to registry .....', LLDebug3);
    RegLogOutOptions := TuibRegistry.Create;
    with RegLogOutOptions do
    begin
      OpenKey(WinstRegHive, WinstRegKey);

      case PerformExitWindows of
        txrNoExit:
          WriteEntry(WinstRegRebootVar, trdInteger, IntToStr(RegNoCallLogout));

        txrRegisterforLogout, txrImmediateLogout:
          WriteEntry(WinstRegRebootVar,
            trdInteger, IntToStr(RegCallLogout));

        txrRegisterforReboot, txrReboot, txrImmediateReboot:
          WriteEntry(WinstRegRebootVar
            , trdInteger,
            IntToStr(RegCallReboot));
      end;

      if PerformShutdown = tsrRegisterForShutdown then
        WriteEntry(WinstRegFinalShutdownVar, trdInteger, IntToStr(RegCallShutdown));

      LogDatei.log('BuildPC: handle reboot options: registry log continue .....',
        LLDebug3);
      WriteEntry(WinstRegLastLogfile, trdString, LogDatei.FileName);

      if PerformExitWindows >= txrRegisterForReboot then
      begin
        LogDatei.log('We will >= Reboot', LLDebug);
        WriteEntry(WinstRegContinueVar, trdInteger, '1');

        try
          if ReadEntry(WinstRegNumberOfErrors, regDataType, numberValue) and
            (numberValue <> '') then
            errorNumber := StrToInt(numberValue);
        except
        end;

        errorNumber := errorNumber + LogDatei.NumberOfErrors;

        // Backup Errorcount for use after reboot
        WriteEntry(WinstRegNumberOfErrors, trdInteger, IntToStr(errorNumber));

        // Reset Errorcount to avoid an errormessage
        errorNumber := 0;
        // >=win 10 and  w10BitlockerSuspendOnReboot then suspend bitlocker for 1 reboot
        if GetNTVersionMajor >= 10 then
        begin
          LogDatei.log('We are on Win >= 10', LLDebug);
          if ((PerformExitWindows = txrRegisterForReboot) or
            (PerformExitWindows = txrImmediateReboot) or
            (PerformExitWindows = txrReboot)) then
          begin
            LogDatei.log('We have to reboot', LLDebug);
            if w10BitlockerSuspendOnReboot then
            begin
              LogDatei.log('Suspend Bitlocker', LLInfo);
              try
                try
                  buildpcscript := TuibInstScript.Create;
                  tmplist := TStringList.Create;
                  tmpstr := extractfiledrive(GetWinDirectory);
                  cmdstr := '(Get-BitLockerVolume -MountPoint ' +
                    tmpstr + ').EncryptionPercentage';
                  tmplist.Text :=
                    buildpcscript.execPowershellCall(cmdstr, 'sysnative',
                    0, True, False).Text;
                  if buildpcscript.LastExitCodeOfExe = 0 then
                  begin
                    LogDatei.log('Succesful asked for Bitlocker', LLInfo);
                    if StrToInt(trim(tmplist.Strings[0])) = 0 then
                    begin
                      LogDatei.log('Bitlocker not active', LLInfo);
                    end
                    else
                    begin
                      LogDatei.log('Bitlocker active - suspend', LLInfo);
                      cmdstr :=
                        'Suspend-BitLocker -MountPoint "' +
                        tmpstr + '" -RebootCount 1';
                      buildpcscript.execPowershellCall(cmdstr,
                        'sysnative', 0, True, False);
                    end;
                  end
                  else
                    LogDatei.log('Problem Suspending Bitlocker: Error: ' +
                      IntToStr(buildpcscript.LastExitCodeOfExe), LLWarning)
                except
                  on e: Exception do
                  begin
                    LogDatei.log('Error executing :' + cmdstr +
                      ' : with powershell: ' + e.message,
                      LLError);
                  end
                end;
              finally
                buildpcscript.Free;
                tmplist.Free;
              end;
            end;
          end;
        end;
      end
      else
      begin
        WriteEntry(WinstRegContinueVar, trdInteger, '0');

        try
          if ReadEntry(WinstRegNumberOfErrors, regDataType, numberValue) and
            (numberValue <> '') then
          begin
            WriteEntry(WinstRegNumberOfErrors, trdInteger, '0');
            //Zuruecksetzen, wird jetzt ausgewertet
            errorNumber := StrToInt(numberValue);
          end;
        except
        end;

      end;

      FlushKey;
      Free;
    end;

    {$ENDIF WINDOWS}

    showErrorMessages := False;

    if (ErrorNumber > 0) and (showErrorMessages or LogDatei.ReportErrorsToUser) then
    begin
      {$IFDEF GUI}
      MyMessageDlg.WiMessage('There are ' + #10 + IntToStr(ErrorNumber) +
        '  ' + 'Error(s) occured while installation.' + #10 +
        'Please contact your admin.',
        [mrOk]);
      {$ENDIF GUI}
    end;


    LogDatei.log('BuildPC: finishOpsiconf .....', LLDebug2);
    if not FlagReloadProductList then OpsiData.finishOpsiconf;
    LogDatei.log('BuildPC: after finishOpsiconf .....', LLDebug2);

    {$IFDEF UNIX}
    if not FlagReloadProductList then
    begin
      opsiclientd := False;
      if ProcessIsRunning('opsiclientd') then
        opsiclientd := True;
      if PerformExitWindows <> txrNoExit then
      begin
        case PerformExitWindows of
          txrRegisterForReboot,
          txrReboot, txrImmediateReboot:
          begin
            if opsiclientd then
            begin
              LogDatei.log('Reboot via opsiclientd', LLinfo);
              TheExitMode := txmNoExit;
              filehandle :=
                fpOpen('/var/run/opsiclientd/reboot', O_WrOnly or O_Creat);
              fpClose(filehandle);
            end
            else
            begin
              LogDatei.log('Reboot direct (not via opsiclientd)', LLinfo);
              if PerformExitWindows <> txrImmediateReboot then
              begin
                LogDatei.log('BuildPC: update switches 2.....', LLDebug3);
                opsidata.UpdateSwitches(extremeErrorLevel, logdatei.actionprogress);
              end;
              TheExitMode := txmReboot;
              if not ExitSession(TheExitMode, Fehler) then
                {$IFDEF GUI}
                MyMessageDlg.WiMessage('ExitWindows Error ' + LineEnding + Fehler, [mrOk]);
                {$ELSE GUI}
              writeln('ExitWindows Error ' + LineEnding + Fehler);
                {$ENDIF GUI}
            end;
          end;

          txrRegisterforLogout, txrImmediateLogout: TheExitMode := txmLogout;
        end;
      end;
      if PerformShutdown = tsrRegisterForShutdown then
      begin
        if opsiclientd then
        begin
          LogDatei.log('Shutdown via opsiclientd', LLinfo);
          filehandle := fpOpen('/var/run/opsiclientd/shutdown', O_WrOnly or O_Creat);
          fpClose(filehandle);
        end
        else
        begin
          LogDatei.log('Shutdown direct (not via opsiclientd)', LLinfo);
          os_shutdown();
        end;
      end;
    end;
    {$ENDIF UNIX}
    LogDatei.log('BuildPC: Terminating .....', LLDebug3);
    {$IFDEF WINDOWS}
    SystemCritical.IsCritical := False;
    {$ENDIF WINDOWS}
    if FlagReloadProductList then BuildPC; //if true reload product list and process list
    TerminateApp;
  except
    on e: Exception do
    begin
      LogDatei.log('exception in BuildPC: final ' + e.message, LLError);
      LogDatei.log('BuildPC: Terminating .....', LLDebug3);
      {$IFDEF WINDOWS}
      SystemCritical.IsCritical := False;
{$ENDIF WINDOWS}
      TerminateApp;
    end;
  end;
end;


procedure Loginscripts;

var
  i: integer;
  //KorrektSortierte: integer;
  //Bootmode: string;
  //bootmodeFromRegistry: boolean;
  //Fehler: string;
  //numberValue: string;
  //errorNumber: integer;
  extremeErrorLevel: TErrorLevel;
  showErrorMessages: boolean;
  //ps: string;
  itemlist: TXStringlist;
  user, dom, sid: string;
  //s4: string;
  //ErrorInfo: string;
  goOn: boolean;
  //problemString: string;
  Produkte: TStringList = nil;
  ///val :   Integer;
  trycounter, maxtries: integer;
  {$IFDEF WINDOWS}
  //regDataType: tuibRegDataType;
  {$ENDIF WINDOWS}

begin
  LogDatei.LogProduktId := False;
  goOn := True;
  {$IFDEF GUI}
  FBatchOberflaeche.SetForceStayOnTop(False);
  {$ENDIF GUI}
  maxtries := 10;
  trycounter := 0;
  if (usercontext = '') or (usercontext = '\') then
    repeat
      begin
        if trycounter > 0 then
          Sleep(1000);
        Inc(trycounter);
        //we got no usercontext yet (by param string) - so let us try to detect
        //##LINUX
        {$IFDEF WIN32}
        usercontext := getloggedonDomUser;
{$ENDIF}
      end;
    until (trycounter > maxtries) or (not (usercontext = '\'));
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
  {$IFDEF WIN32}
  initializeFoldersForUser(dom, user);
  sid := GetLocalUserSidStr(user);
  //LogDatei.log('sidStr :'+sid,LLDebug);
  sid := copy(sid, 2, length(sid) - 2);
  LogDatei.log('sidStr :' + sid, LLDebug);
  (*
  sid := GetDomainUserSidS(s2, s3, s4);

  If length(sid) > 1 Then
  Begin
    Skip('[', sid, sid, Errorinfo);
    If sid[length(sid)] = ']' Then
       sid := copy(sid, 1, length(sid) - 1);
  End;
  *)
  usercontextSID := sid;
  usercontextUser := user;
  usercontextDomain := dom;
  {$ENDIF WIN32}
  DontUpdateMemo := True;

  OpsiData.setActualClient(computername);
  //Produkte := OpsiData.getListOfProducts;
  Produkte := TOpsi4data(OpsiData).getMapOfLoginscripts2Run(allLoginScripts);


  LogDatei.log('Computername:' + computername, LLinfo);

  if computername <> ValueOfEnvVar('computername') then
    LogDatei.log('Computername according to Environment Variable :' +
      ValueOfEnvVar('computername'),
      LLinfo);

  if opsiserviceURL <> '' then
    LogDatei.log('opsi service URL ' + opsiserviceurl,
      LLinfo);

  LogDatei.log('Depot path:  ' + depotdrive + depotdir, LLinfo);
  LogDatei.log('', LLinfo);
  {$IFDEF GUI}
  FBatchOberflaeche.SetMessageText(rsProductCheck, mInfo);
  //setInfoLabel(rsProductCheck);
  ProcessMess;
  {$ENDIF GUI}
  (* walk through all products *)
  if goOn then
  begin
    LogDatei.LogProduktId := True;
    i := 1;
    while (i <= Produkte.Count) do
    begin
      Produkt := Produkte.Strings[i - 1];
      scriptMode := tsmLogin;

      opsidata.setActualProductName(Produkt);
      if trim(Produkt) = '' then
        LogDatei.log('product ' + IntToStr(i - 1) + ' is "" ', LLinfo);

      extremeErrorLevel := Level_not_initialized;

      //if opsidata.getProductState = tpsInstalled then
      begin
        ProcessProdukt(extremeErrorLevel);
        opsidata.finishProduct;
      end;
      Inc(i);
    end;
    LogDatei.LogProduktId := False;
    opsidata.saveOpsiConf;
    Produkte.Free;
    Produkte := nil;
  end;


  opsidata.saveOpsiConf;

  showErrorMessages := False;

  OpsiData.finishOpsiconf;

  TerminateApp;
end;

procedure Productlist;

var
  i: integer;
  extremeErrorLevel: TErrorLevel;
  showErrorMessages: boolean;
  goOn: boolean;
  Produkte: TStringList = nil;

begin
  LogDatei.LogProduktId := False;
  goOn := True;
  {$IFDEF GUI}
  FBatchOberflaeche.SetForceStayOnTop(True);
  {$ENDIF GUI}
  DontUpdateMemo := True;

  OpsiData.setActualClient(computername);
  Produkte := TStringList.Create;
  Produkte.Assign(TStringList(scriptlist));


  LogDatei.log('Computername:' + computername, LLinfo);

  if computername <> ValueOfEnvVar('computername') then
    LogDatei.log('Computername according to Environment Variable :' +
      ValueOfEnvVar('computername'),
      LLinfo);

  if opsiserviceURL <> '' then
    LogDatei.log('opsi service URL ' + opsiserviceurl,
      LLinfo);

  LogDatei.log('Depot path:  ' + depotdrive + depotdir, LLinfo);
  LogDatei.log('', LLinfo);
  {$IFDEF GUI}
  FBatchOberflaeche.SetMessageText(rsProductCheck, mInfo);
  //setInfoLabel(rsProductCheck);
  ProcessMess;
  {$ENDIF GUI}
  (* walk through all products *)
  if goOn then
  begin
    LogDatei.LogProduktId := True;
    i := 1;
    while (i <= Produkte.Count) and (PerformExitWindows < txrReboot) and
      not PerformExitProgram do
    begin
      Produkt := Produkte.Strings[i - 1];
      scriptMode := tsmMachine;

      opsidata.setActualProductName(Produkt);
      if trim(Produkt) = '' then
        LogDatei.log('product ' + IntToStr(i - 1) + ' is "" ', LLinfo);

      extremeErrorLevel := Level_not_initialized;

      //if opsidata.getProductState = tpsInstalled then
      begin
        ProcessProdukt(extremeErrorLevel);

        // update switches
        if extremeErrorLevel > levelfatal then
        begin
          //successful after setup
          opsidata.ProductOnClient_update(LogDatei.ActionProgress,
            tarSuccessful,
            tacNone,
           // ttc4Installed,
            tacSetup,
            tpsInstalled);
        end
        else //failed
        begin
          //failed after setup
          opsidata.ProductOnClient_update(LogDatei.ActionProgress,
            tarFailed,
            tacNone,
            //ttc4Installed,
            tacSetup,
            tpsUnkown);
        end; // failed
        opsidata.finishProduct;
      end;
      Inc(i);
    end;
    LogDatei.LogProduktId := False;
    opsidata.saveOpsiConf;
    Produkte.Free;
    Produkte := nil;
  end;


  opsidata.saveOpsiConf;

  showErrorMessages := False;

  OpsiData.finishOpsiconf;

  TerminateApp;
end;



{$IFDEF WINDOWS}
procedure writeLogFileOptions(const RegHive: string; const info: string);
var
  rkey: HKEY;
  RegLogfileOptions: TRegistry;
begin
  RegLogfileOptions := TRegistry.Create;
  with RegLogfileOptions do
  begin
    GetHKey(WinstRegHive, rkey);
    RootKey := rkey;
    OpenKey(WinstRegKey, True);
    WriteString(WinstRegLastLogfile, info);

    if PerformExitWindows >= txrRegisterForReboot then
      WriteInteger(WinstRegContinueVar, 1)
    else
      WriteInteger(WinstRegContinueVar, 0);
    Free;
  end;
end;

{$ELSE WINDOWS}

procedure writeLogFileOptions(const RegHive: string; const info: string);
begin

end;

{$ENDIF WINDOWS}

{$IFDEF UNIX}
function freeLinuxAgentStart: boolean;
var
  startkey: string;
  startcounter: integer;
  conffile: TIniFile;
  list: TStringList;
begin
  try
    Result := False;
    list := TStringList.Create;
    conffile := nil;
    if FileExists(opsiclientagentconf) then
    begin
      conffile := TIniFile.Create(opsiclientagentconf);
      startkey := conffile.ReadString('opsi-script', 'start', 'void');
      if startkey <> 'void' then
      begin
        startkey := decryptStringBlow(opsiservicepassword, startkey);
        LogDatei.log('startkey from opsi-client-agent.conf: ' + startkey, LLNotice);
        try
          stringsplitByWhiteSpace(startkey, list);
          if trim(list.Strings[0]) = opsiservicepassword then
          begin
            if TryStrToInt(trim(list.Strings[1]), startcounter) then
            begin
              if startcounter > 0 then
              begin
                Result := True;
                Dec(startcounter);
                logdatei.log('Using free Linux Agent start. ' + IntToStr(
                  startcounter) + ' remaining', LLNotice);
                startkey := opsiservicepassword + ' ' + IntToStr(startcounter);
                startkey := encryptStringBlow(opsiservicepassword, startkey);
                conffile.WriteString('opsi-script', 'start', startkey);
              end
              else
                logdatei.log('No free Linux Agent start', LLDebug2);
            end
            else
              logdatei.log('Error: Startkey has no valid integer: ' +
                trim(list.Strings[1]), LLError);
          end
          else
            logdatei.log('Error: Startkey has wrong signature.', LLError);
        except
          on ex: Exception do
          begin
            logdatei.log('Error: Exception in osmain:freeLinuxAgentStart : ' +
              ex.message, LLError);
            Result := False;
          end;
        end;
      end
      else
        logdatei.log('No valid start enty in ' + opsiclientagentconf +
          ' found ', LLWarning);
    end
    else
      logdatei.log('No ' + opsiclientagentconf + ' found ', LLWarning);
  finally
    list.Free;
    if conffile <> nil then
      conffile.Free;
  end;
end;

{$ENDIF UNIX}

procedure StartProgramModes;

var
  ///  i :   Integer;
  Fehler: string;
  TheExitMode: TExitMode;
  extremeErrorLevel: TErrorLevel;
  //omc: TOpsiMethodCall;
  errorOccured: boolean;
  ipAddress, ipName: string;
  scriptindex: integer;
  //path: string;
  opsiServiceVersion, sessionid, opsiclientdconf: string;
  myconf: TIniFile;
  {$IFDEF WINDOWS}
  rkey: HKEY;
  Reg: TRegistry;
  {$ENDIF WINDOWS}
  startTime: TDateTime;

begin
  try
    //writeln('StartProgramModes');
    startupmessages.Append('startmessage StartProgramModes and create log: ' +
      DateTimeToStr(Now));
    //writeln('StartProgramModes1');
    if Logdatei <> nil then
      Logdatei.Free;
    //writeln('StartProgramModes2');
    Logdatei := TLogInfo.Create;
    if LogDateiName = '' then
      LogDateiName := LogPath + logdatei.StandardLogFilename +
        logdatei.StandardLogFileext;
    //writeln('StartProgramModes3');
    Logdatei.log('opsi-script ' + OpsiscriptVersion + ' started at ' +
      starttimestr, LLessential);
    Logdatei.log('opsi-script log file with encoding ' + DefaultEncoding, LLessential);
    {$MACRO ON}
    //(*  comment this block for code formatting
    startupmessages.Append(
      'Compiled with FPC ' + {$i %FPCVERSION%} + ' for ' +
      {$i %FPCTARGETOS%} + '-' + {$i %FPCTARGETCPU%} + ' at ' + {$i %TIME%} + ' on ' + {$i %DATE%});
    //*)
    {$MACRO OFF}
    //writeln('StartProgramModes4');
    {$IFDEF GUI}
    //FBatchOberflaeche.setVisible(false);
    FBatchOberflaeche.SetBatchWindowMode(BatchWindowMode);
    //writeln('StartProgramModes5');
    {$ENDIF GUI}
    LogDatei.log('program mode: ' + ProgramModeToString(ProgramMode), LLessential);
    startupmessages.Append('program mode: ' + ProgramModeToString(ProgramMode));
    sessionid := '';

    if ProgramMode = pmInfo then
    begin
      {$IFDEF GUI}
      FBatchOberflaeche.SetBatchWindowMode(bwmIcon);
      FBatchOberflaeche.SetElementVisible(True, eMainForm);
      CentralFormVisible := True;
      CentralForm.Visible := CentralFormVisible;



      MyMessageDlg.WiMessageSized('command line options are' + LineEnding +
        '	 ' + ParamDelim + '?' + LineEnding + '	 ' + ParamDelim +
        'h[elp]' + LineEnding +
        //'   ' + ParamDelim + 'pcprofil  [PCProfileFile  [[' + ParamDelim + 'logfile] Logfile ] ] [' + ParamDelim + 'parameter ParameterString]' + LineEnding +
        '	 ' + ParamDelim + 'opsiservice <OpsiServiceUrl> ' +
        ParamDelim + 'clientid <clientid> ' + ParamDelim +
        'credentialfile <filename>' + ParamDelim + 'username <username> ' +
        ParamDelim + 'password <password> [' + ParamDelim +
        'sessionid <sessionid> ] [' + ParamDelim + 'usercontext <usercontext> ] [' +
        ParamDelim + 'productlist <productlist> | ' + ParamDelim +
        'processproducts <productlist> | ' + ParamDelim +
        'loginscripts | ' + ParamDelim + 'allloginscripts ] [' +
        ParamDelim + 'silent ]' + '	 [<Scriptfile>  [' + ParamDelim +
        'logfile <LogFile>] [' + ParamDelim + 'productid <productid> ] [' +
        ParamDelim + 'logproductid <productid> ] [' + ParamDelim +
        '[batch|silent] |histolist Inifilepath ] [' + ParamDelim +
        'parameter ParameterString]' + LineEnding +
        '	 Scriptfile[;Scriptfile]*  [' + ParamDelim + 'logfile LogFile] [' +
        ParamDelim + 'lang langcode] [' + ParamDelim + '[batch|silent]] [' +
        ParamDelim + 'productid] [' + ParamDelim + 'productid <productid> ] [' +
        ParamDelim + 'parameter ParameterString] ['+ ParamDelim +
        'testsyntax ]',
        [mrOk],
        650, 250);
      {$ELSE GUI}
      //nogui
      writeln(ExtractFileName(reencode(ParamStr(0), 'system')) +
        ' Version: ' + OpsiscriptVersion);
      writeln('command line options are' + LineEnding + '' +
        ParamDelim + '? |' + ParamDelim + 'h[elp]' + LineEnding +
        //'   ' + ParamDelim + 'pcprofil  [PCProfileFile  [[' + ParamDelim + 'logfile] Logfile ] ] [' + ParamDelim + 'parameter ParameterString]' + LineEnding +
        '' + ParamDelim + 'opsiservice <OpsiServiceUrl> ' + ParamDelim +
        'clientid <clientid> ' + ParamDelim + 'credentialfile <filename>' +
        ParamDelim + 'username <username> ' + ParamDelim +
        'password <password> [' + ParamDelim + 'sessionid <sessionid> ] [' +
        ParamDelim + 'usercontext <usercontext> ] [' + ParamDelim +
        'productlist <productlist> | ' + ParamDelim +
        'processproducts <productlist> | ' + ParamDelim +
        'loginscripts | ' + ParamDelim + 'allloginscripts ] [' +
        ParamDelim + 'silent ]' + LineEnding + ' Scriptfile  [' +
        ParamDelim + 'logfile <LogFile>] [' + ParamDelim +
        'productid <productid> ] [' + ParamDelim + 'productid <productid> ] [' +
        ParamDelim + '[batch|silent] | histolist Inifilepath ] [' +
        ParamDelim + 'parameter ParameterString]' + LineEnding +
        ' Scriptfile[;Scriptfile]*  [' + ParamDelim + 'logfile LogFile] [' +
        ParamDelim + '[batch|silent]] [' + ParamDelim + 'productid] [' +
        ParamDelim + 'productid <productid> ] [' + ParamDelim +
        'parameter ParameterString] ['+ ParamDelim +
        'testsyntax ]');
      {$ENDIF GUI}

      TerminateApp;
    end

    else
    begin

      if ProgramMode = pmBuildPC_classic then
      begin
        //not implemented any more
      end

      else if ProgramMode = pmBuildPC_service then
      begin
        try
          errorOccured := False;
          {$IFDEF WINDOWS}
          SystemCritical.IsCritical := True;
          {$ENDIF WINDOWS}
          { if necessary do product installations  }
          {$IFDEF GUI}
          if runSilent then
            FBatchOberflaeche.SetElementVisible(False, eMainForm)
          else
            FBatchOberflaeche.SetElementVisible(True, eMainForm);
          {$ENDIF GUI}
          startupmessages.Append('startmessage start opsi service connection: ' +
            DateTimeToStr(Now));
          //opsiServiceVersion := getOpsiServiceVersion(opsiserviceurl, opsiserviceUser, opsiservicePassword, sessionid);
          //startupmessages.Append('startmessage opsi service version is : '+opsiServiceVersion+' at '+DateTimeToStr(Now));
          opsiServiceVersion := '4';
          if opsiServiceVersion = '4' then
          begin
            //LogDatei.log ('Initializing opsi service version: '+opsiServiceVersion, LLessential);
            opsidata := TOpsi4Data.Create;
            opsidata.initOpsiConf(opsiserviceurl,
              opsiserviceUser,
              opsiservicePassword,
              opsiserviceSessionId);
            startupmessages.Append('startmessage: opsidata initialized: ' +
              DateTimeToStr(Now));
            //OpsiData.setOptions (opsiclientd_serviceoptions);
            opsidata.setActualClient(computername);
            startupmessages.Append(computername);
            startupmessages.Append(readConfigsFromService);
            startTime := now;
            (*
            omc := TOpsiMethodCall.Create('backend_info', []);
            //omc := TOpsiMethodCall.Create('authenticated', []);
            testresult := opsidata.CheckAndRetrieve(omc, errorOccured);
            startupmessages.Append('JSON Bench for ' + omc.opsimethodname + ' '
                + copy(omc.getJsonUrlString,pos(',',omc.getJsonUrlString)+1,50)
                +' Start: '+FormatDateTime('hh:nn:ss:zzz',startTime)
                +' Time: '+FormatDateTime('hh:nn:ss:zzz',now-startTime));
            omc.Free;
            *)
          end
          else
            startupmessages.Append('Internal Error: Unkown Opsi Service Version:>' +
              opsiServiceVersion + '<');
        except
          on e: Exception do
          begin
            //LogDatei.log('Exception in StartProgramModes: pmBuildPC_service' + e.message, LLError);
            startupmessages.Append('Exception in StartProgramModes: pmBuildPC_service'
              + e.message + ' ' + DateTimeToStr(Now));
            errorOccured := True;
          end;
        end;


        if errorOccured then
        begin
          FindLocalIPData(ipName, ipAddress);
          LogPath := logdatei.StandardLogPath;
          startupmessages.Append('startmessage create log: ' + DateTimeToStr(Now));
          LogDatei.CreateTheLogfile(LogDateiName);
          LogDatei.log('opsi-script cannot connect to service with URL: ' +
            opsiserviceurl + ' with user ' + opsiserviceUser +
            '  The message is: >' + testresult + '< - Aborting ', LLcritical);

          terminateApp;
        end

        else
        begin
          if runloginscripts then
          begin
            //ForceDirectories('c:\opsi.org\log');
            LogPath := logdatei.StandardLogPath;
            LogDateiName := LogPath + getLoggedInUser + '_login.log';
            startupmessages.Append('startmessage create log: ' + DateTimeToStr(Now));
            LogDatei.CreateTheLogfile(LogDateiName, False);
          end
          else
          begin
            LogDateiName := OpsiData.getLogFileName(LogDateiName);
            startupmessages.Append('startmessage create log: ' + DateTimeToStr(Now));
            LogDatei.WriteHistFile := True;
            LogDatei.CreateTheLogfile(LogDateiName);
          end;
          LogDatei.force_min_loglevel := osconf.force_min_loglevel;
          LogDatei.debug_prog := osconf.debug_prog;
          LogDatei.LogLevel := osconf.default_loglevel;
          LogDatei.debug_lib := osconf.debug_lib;
          LogDatei.WriteComponentFile := osconf.configWriteProductLogFile;
          logDatei.log('force_min_loglevel: ' + IntToStr(osconf.force_min_loglevel),
            LLessential);
          logDatei.log('default_loglevel: ' + IntToStr(osconf.default_loglevel),
            LLessential);
          logDatei.log('debug_prog: ' + booleantostr(osconf.debug_prog), LLessential);
          logDatei.log('debug_lib: ' + booleantostr(osconf.debug_lib), LLessential);

          extractTmpPathFromLogdatei(LogDateiName);
          TempPath := GetTempPath;

          logSupportedEncodings;

          {$IFDEF LINUX}
          if not opsidata.linuxAgentActivated then
          begin
            if freeLinuxAgentStart then
            begin
              LogDatei.log('Using free LinuxAgent start', LLNotice);
              writeln('Using free LinuxAgent start');
            end
            else
            begin
              LogDatei.log('No more free LinuxAgent starts', LLError);
              writeln('No more free LinuxAgent starts');
              LogDatei.log(
                'Use of opsi Linux Client Agent Extension is not activated', LLerror);
              writeln('Use of opsi Linux Client Agent Extension is not activated');
              {$IFDEF GUI}
              MyMessageDlg.WiMessage(
                'Use of opsi Linux Client Agent Extension is not activated ' +
                LineEnding + 'Terminating Program', [mrOk]);
              {$ENDIF GUI}
              LogDatei.log('Terminating Program', LLerror);
              TerminateApp;
            end;
          end
          else
          begin
            LogDatei.log(
              'Use of opsi Linux Client Agent Extension is activated', LLInfo);
          end;
          {$ENDIF LINUX}

          {$IFDEF DARWIN}
          if not opsidata.macosAgentActivated then
          begin
            LogDatei.log(
              'Use of opsi MacOS Client Agent Extension is not activated', LLerror);
            writeln('Use of opsi MacOS Client Agent Extension is not activated');
              {$IFDEF GUI}
            MyMessageDlg.WiMessage(
              'Use of opsi MacOS Client Agent Extension is not activated ' +
              LineEnding + 'Terminating Program', [mrOk]);
              {$ENDIF GUI}
            LogDatei.log('Terminating Program', LLerror);
            TerminateApp;
          end
          else
          begin
            LogDatei.log(
              'Use of opsi MacOS Client Agent Extension is activated', LLInfo);
          end;
          {$ENDIF DARWIN}


          if runloginscripts then
          begin
            if pos('opsisetupadmin', LowerCase(usercontext)) = 0 then
              LoginScripts;
          end
          else
          if runproductlist then
            Productlist
          else
            BuildPC;
        end;
      end

      else if ProgramMode in [pmBatch, pmSilent] then
      begin
        try
          {$IFDEF WINDOWS}
          SystemCritical.IsCritical := True;
          {$ENDIF WINDOWS}
          //FBatchOberflaeche.BorderIcons := [];
          {$IFDEF GUI}
          (*
          if ProgramMode = pmBatch then
            FBatchOberflaeche.setVisible(True)

          else if ProgramMode = pmSilent then
            FBatchOberflaeche.setVisible(False);
           *)

          if runSilent then
            FBatchOberflaeche.SetElementVisible(False, eMainForm)
          else
            FBatchOberflaeche.SetElementVisible(True, eMainForm);
          if scriptlist.Count <= 0 then
            centralform.Edit1.Text := scriptlist.strings[0];
          centralform.Edit_LogFile.Text := LogDateiName;

          DontUpdateMemo := True;
          {$ENDIF GUI}
          logdatei.StandardPartLogPath := ExtractFileDir(Logdateiname);
          LogDatei.CreateTheLogFile(Logdateiname, False);
          extractTmpPathFromLogdatei(LogDateiName);
          TempPath := GetTempPath;
          extremeErrorLevel := Level_not_initialized;
          LogDatei.force_min_loglevel := osconf.force_min_loglevel;
          LogDatei.debug_prog := osconf.debug_prog;
          LogDatei.LogLevel := osconf.default_loglevel;
          LogDatei.debug_lib := osconf.debug_lib;
          logDatei.log_prog('force_min_loglevel: ' + IntToStr(
            osconf.force_min_loglevel), LLessential);
          logDatei.log_prog('default_loglevel: ' + IntToStr(
            osconf.default_loglevel), LLessential);
          logDatei.log_prog('debug_prog: ' + booleantostr(osconf.debug_prog),
            LLessential);
          logDatei.log_prog('debug_lib: ' + booleantostr(osconf.debug_lib), LLessential);

          if not (batchproductid = '') then
          begin
            LogDatei.LogProduktId := True;
            LogDatei.AktProduktId := batchproductid;
          end;

          if not (logproductid = '') then
          begin
            LogDatei.LogProduktId := True;
            LogDatei.AktProduktId := logproductid;
          end;

          logDatei.log_prog('AktProduktId: ' + LogDatei.AktProduktId +
            ' = ' + booleantostr(LogDatei.LogProduktId), LLessential);

          if scriptlist.Count <= 0 then
          begin
            logDatei.log('Error we have no script in batch mode. ', LLcritical);
            extremeErrorLevel := levelFatal;
          end;


          { Are we in batch with /productid (opsi-template-with-admin) ?
             open service connection if possible  }
          if not (batchproductid = '') then
          begin
            try
              if (opsiserviceurl = '') or (opsiserviceUser = '') or
                (opsiservicePassword = '') then
              begin
                logDatei.log(
                  'Param: /productid: Get service credentials from opsicliend.conf',
                  LLinfo);
              {$IFDEF WINDOWS}
                opsiclientdconf :=
                  getSpecialFolder(CSIDL_PROGRAM_FILES) +
                  '\opsi.org\opsi-client-agent\opsiclientd\opsiclientd.conf';
              {$ENDIF WINDOWS}
              {$IFDEF UNIX}
                opsiclientdconf := '/etc/opsi-client-agent/opsiclientd/opsiclientd.conf';
              {$ENDIF UNIX}
                if FileExists(opsiclientdconf) then
                begin
                  myconf := TInifile.Create(opsiclientdconf);
                  opsiservicePassword :=
                    myconf.ReadString('global', 'opsi_host_key', '');
                  opsiserviceUser := myconf.ReadString('global', 'host_id', '');
                  opsiserviceurl := myconf.ReadString('config_service', 'url', '');
                  myconf.Free;
                end

                else
                begin
                  LogDatei.log('No opsiclientd.conf found: No Service connection',
                    LLCritical);
                  extremeErrorLevel := levelFatal;
                end;
              end
              else
                logDatei.log(
                  'Param: /productid: Got service credentials from parameters',
                  LLinfo);
              if not ((opsiserviceurl = '') or (opsiserviceUser = '') or
                (opsiservicePassword = '')) then
              begin
                try
                  opsidata := TOpsi4Data.Create;
                  opsidata.initOpsiConf(opsiserviceurl,
                    opsiserviceUser,
                    opsiservicePassword,
                    opsiserviceSessionId);
                  if opsidata.isConnected then
                  begin
                    { isConnected calls backendInfo so no need to call it again }
                      (*
                      startTime := now;
                      omc := TOpsiMethodCall.Create('backend_info', []);
                      testresult := opsidata.CheckAndRetrieve(omc, errorOccured);
                      LogDatei.log_prog('JSON Bench for ' + omc.OpsiMethodName +
                        ' ' + copy(omc.getJsonUrlString,
                        pos(',', omc.getJsonUrlString) + 1, 50) +
                        ' Start: ' + FormatDateTime('hh:nn:ss:zzz', startTime) +
                        ' Time: ' + FormatDateTime('hh:nn:ss:zzz',
                        now - startTime), LLinfo);
                      omc.Free;
                      *)
                    opsidata.setActualProductName(batchproductid);
                    if isValidFQDN(opsiserviceClientId) then
                       opsidata.setActualClient(opsiserviceClientId)
                    else
                       opsidata.setActualClient(opsiserviceUser);
                    ProductvarsForPC := opsidata.getProductproperties;
                    if not opsidata.initProduct then
                    begin
                      extremeErrorLevel := levelFatal;
                      LogDatei.log('Could not connect to Service !', LLCritical);
                    end;
                  end
                  else
                  begin
                    //extremeErrorLevel := levelFatal;
                    LogDatei.log('Could not connect to Service :' +
                      opsiserviceurl + ' - retry with localhost', LLerror);
                    opsiserviceurl := 'https://localhost:4441/rpc';
                    if Assigned(opsidata) then
                      FreeAndNil(opsidata);
                    opsidata := TOpsi4Data.Create;
                    opsidata.initOpsiConf(opsiserviceurl,
                      opsiserviceUser,
                      opsiservicePassword,
                      opsiserviceSessionId);
                    if opsidata.isConnected then
                    begin
                      { isConnected calls backendInfo so no need to call it again }
                        (*
                        startTime := now;
                        omc := TOpsiMethodCall.Create('backend_info', []);
                        testresult := opsidata.CheckAndRetrieve(omc, errorOccured);
                        LogDatei.log_prog('JSON Bench for ' + omc.OpsiMethodName +
                          ' ' + copy(omc.getJsonUrlString,
                          pos(',', omc.getJsonUrlString) + 1, 50) +
                          ' Start: ' + FormatDateTime('hh:nn:ss:zzz', startTime) +
                          ' Time: ' + FormatDateTime('hh:nn:ss:zzz',
                          now - startTime), LLinfo);
                        omc.Free;
                        *)
                      opsidata.setActualProductName(batchproductid);
                      if isValidFQDN(opsiserviceClientId) then
                         opsidata.setActualClient(opsiserviceClientId)
                      else
                         opsidata.setActualClient(opsiserviceUser);
                      ProductvarsForPC := opsidata.getProductproperties;
                      if not opsidata.initProduct then
                      begin
                        extremeErrorLevel := levelFatal;
                        LogDatei.log('Could not connect to Service !', LLCritical);
                      end;
                    end
                    else
                    begin
                      //extremeErrorLevel := levelFatal;
                      LogDatei.log('Could not connect to Service :' +
                        opsiserviceurl + ' - giving up', LLerror);
                      LogDatei.log('Any opsiservicecall will probably fail.', LLerror);
                    end;
                  end;
                except
                  on e: Exception do
                  begin
                    LogDatei.log(
                      'Exception in StartProgramModes: [pmBatch, pmSilent]: opsidata.create'
                      + e.message, LLCritical);
                      {$IFDEF WINDOWS}
                    SystemCritical.IsCritical := False;
                      {$ENDIF WINDOWS}
                    extremeErrorLevel := levelFatal;
                  end;
                end;
              end
              else
              begin
                LogDatei.log(
                  'Data missing in opsiclientd.conf or parameters: No Service connection',
                  LLCritical);
                extremeErrorLevel := levelFatal;
              end;
            except
              on e: Exception do
              begin
                LogDatei.log(
                  'Exception in StartProgramModes: [pmBatch, pmSilent]: read opsiclientd.conf'
                  + e.message, LLCritical);
                {$IFDEF WINDOWS}
                SystemCritical.IsCritical := False;
                {$ENDIF WINDOWS}
                extremeErrorLevel := levelFatal;
              end;
            end;
          end;


          //*****************************************************************************************
          // Are we in batch with /productid (opsi-template-with-admin) ?  or /ServiceBatch (part 1)
          //*****************************************************************************************
          // we have to read the logging configuration from the service now and
          // configure logging, before the opsi-script is being processed.

          if (runningAsAdmin and (not (batchproductid = ''))) or
            ((not (batchproductid = '')) and batchUpdatePOC and not
            (opsidata = nil)) then
          begin
            try
              if batchUpdatePOC and not (opsidata = nil) then
              begin
                // we are in /serviceBatch mode and get logging parameters from service
                LogDatei.log(readConfigsFromService, LLessential);

                LogDatei.force_min_loglevel := osconf.force_min_loglevel;
                LogDatei.debug_prog := osconf.debug_prog;
                LogDatei.LogLevel := osconf.default_loglevel;
                LogDatei.debug_lib := osconf.debug_lib;
                LogDatei.WriteComponentFile := osconf.configWriteProductLogFile;
                LogDatei.log('serviceBatch force_min_loglevel: ' + IntToStr(osconf.force_min_loglevel),LLessential);
                LogDatei.log('serviceBatch default_loglevel: ' + IntToStr(osconf.default_loglevel),LLessential);
                LogDatei.log('serviceBatch debug_prog: ' + booleantostr(osconf.debug_prog), LLessential);
                LogDatei.log('serviceBatch debug_lib: ' + booleantostr(osconf.debug_lib), LLessential);
              end
            except
              on e: Exception do
              begin
                LogDatei.log(
                  'exception in StartProgramModes: [pmBatch, pmSilent]: reg.Write: with-admin-fatal '
                  + e.message, LLError);

                {$IFDEF WINDOWS}
                SystemCritical.IsCritical := False;
                {$ENDIF WINDOWS}
              end;
            end;
          end;

          //***************************************************************
          //** processing the opsi-script ...
          //***************************************************************
          if extremeErrorLevel <> levelFatal then
          begin
            for scriptindex := 0 to scriptlist.Count - 1 do
            begin
              NestingLevel := 0;
              if batchUpdatePOC and not (opsidata = nil) then
              begin
                opsidata.setProductProgress(tppInstalling);
                opsidata.setProductState(tpsUnkown);
              end;
              ScriptConstants.Init(scriptlist.Strings[scriptindex]);
              CreateAndProcessScript(scriptlist.Strings[scriptindex],
                NestingLevel, False, extremeErrorLevel);
            end;

            if not (opsidata = nil) then
            begin
              // send log
              logdatei.Appendmode := True;
              opsidata.finishOpsiConf;
            end;
          end;

          //*****************************************************************************************
          // Are we in batch with /productid (opsi-template-with-admin) ?  or /ServiceBatch  (part 2)
          //*****************************************************************************************
          // At this point it would be too late to get and configure the logging,
          // because the opsi-script has already been processed above in CreateAndProcessScript.
          // So the logging has been configured in  "/ServiceBatch (part 1)" above.
          // This part has to be done AFTER processing the opsi-script file:

          if (runningAsAdmin and (not (batchproductid = ''))) or
            ((not (batchproductid = '')) and batchUpdatePOC and not
            (opsidata = nil)) then
          begin
            try
              if batchUpdatePOC and not (opsidata = nil) then
              begin
                // we are in /serviceBatch mode and so we have to update the productOnClient via service

                LogDatei.log('serviceBatch: update switches .....', LLDebug);
                if (PerformExitWindows < txrImmediateLogout)
                   and (not scriptsuspendstate)
                   and (not scriptdeferstate)
                then
                begin
                  LogDatei.log_prog('serviceBatch: update switches 2.....', LLDebug);
                  // we are in /serviceBatch mode and so we act like having a actionrequest=setup
                  opsidata.setActualProductActionRequest('setup');
                  opsidata.UpdateSwitches(extremeErrorLevel, Logdatei.actionprogress);
                end;
                LogDatei.log_prog('serviceBatch: finishProduct .....', LLDebug);
                // free productVars
                opsidata.finishProduct;
                LogDatei.LogProduktId := False;
              end
              else
              begin
                // We are in batch with /productid (opsi-template-with-admin)
                // write isFatal (or not) to registry
                //LogDatei.log('extremeErrorLevel is : '+IntToStr(extremeErrorLevel), LLDebug2);
                //##LINUX
              {$IFDEF WINDOWS}
                reg := TRegistry.Create;
                GetHKey(WinstRegHive, rkey);
                reg.RootKey := rkey;
                reg.OpenKey(WinstRegKey, True);
                if extremeErrorLevel <= levelfatal then
                begin
                  reg.WriteString('with-admin-fatal', 'true');
                end
                else
                begin
                  reg.WriteString('with-admin-fatal', 'false');
                end;
                reg.CloseKey;
                reg.Free;
              {$ENDIF WINDOWS}
              end;
            except
              on e: Exception do
              begin
                LogDatei.log(
                  'exception in StartProgramModes: [pmBatch, pmSilent]: reg.Write: with-admin-fatal '
                  + e.message, LLError);

                {$IFDEF WINDOWS}
                SystemCritical.IsCritical := False;
                {$ENDIF WINDOWS}
              end;
            end;
          end;


          try
            {$IFDEF WINDOWS}
            writeLogFileOptions(WinstRegHive, Logdateiname);
            {$ENDIF}
          except
          end;

          if PerformExitWindows <> txrNoExit then
          begin
            case PerformExitWindows of
              txrRegisterForReboot,
              txrReboot, txrImmediateReboot: TheExitMode := txmReboot;
              txrRegisterforLogout, txrImmediateLogout: TheExitMode := txmLogout;
            end;

            if not ExitSession(TheExitMode, Fehler) then
              {$IFDEF GUI}
              MyMessageDlg.WiMessage('ExitWindows Fehler ' + LineEnding +
                Fehler, [mrOk]);
              {$ELSE GUI}
            writeln('ExitWindows Fehler ' + LineEnding + Fehler);
              {$ENDIF GUI}
          end;

          TerminateApp;
        except
          on e: Exception do
          begin
            LogDatei.log('exception in StartProgramModes: [pmBatch, pmSilent]: ' +
              e.message, LLError);
            {$IFDEF WINDOWS}
            SystemCritical.IsCritical := False;
            {$ENDIF WINDOWS}
          end;
        end;
      end
      else
      begin
        //writeln('interactive mode');
        {$IFDEF GUI}
        with centralform do
        begin
          // we are in interactive mode
          FBatchOberflaeche.SetElementVisible(True, eMainForm);
          CentralFormVisible := True;
          CentralForm.Visible := CentralFormVisible;

            {$IFDEF WINDOWS}
          ShowWindow(HWND(FBatchOberflaeche.Handle), SW_HIDe);
          ShowWindow(HWND(Handle), SW_SHOW);
            {$ENDIF WINDOWS}
          if ProgramMode = pmHistoDialog then
          begin
            ComboBox1.Enabled := True;
            ComboBox1.Visible := True;
            Edit1.Enabled := False;
            Edit1.Visible := False;
            IniFileLocalization := ExpandFileName(IniFileLocalization);
            if FileExists(IniFileLocalization) then
              ComboBox1.Items.LoadFromFile(IniFileLocalization)
            else
              ComboBox1.Items.Clear;

            if (Skriptdatei = '') and (ComboBox1.Items.Count > 0) then
              ComboBox1.Text := ComboBox1.Items[0]
            else
              ComboBox1.Text := Skriptdatei;
          end
          else
            Edit1.Text := Skriptdatei;

          if GetContinueLogFile(LogDateiName) then
            RadioButtonAppendLogFile.Checked := True;

          Edit_LogFile.Text := LogDateiName;
          extractTmpPathFromLogdatei(LogDateiName);
          TempPath := GetTempPath;
        end;
        {$ELSE GUI}
        writeln('no interactive mode at nogui version');
        TerminateApp;
        {$ENDIF GUI}
      end;
    end;
  except
    on e: Exception do
    begin
      LogDatei.log('Exception in StartProgramModes: General' + e.message, LLError);
    end;
  end;

end;


procedure GetParameter;

var
  i, NumberOfTrueParameters: integer;
  Parameter, Next: string;
  weiter: boolean;
  testpassed: boolean;
  lookForSpecificParameters: boolean;
  ParamListe: TStringList;
  list1: TXStringList;

  r: string;

  part1, part2: string;

  teststr: string;

begin
  try
    //{$IFDEF GUI}
    ProgramMode := pmNotSet;
    //{$ELSE GUI}
    //ProgramMode := pmInfo;
    //{$ENDIF GUI}
    scriptMode := tsmMachine;
    IniFileLocalization := StandardIniFile;
    list1 := TXStringList.Create;

    testpassed := False;
    if (StandardTempPath <> '') and SysUtils.ForceDirectories(StandardTempPath) then
    begin
      testpassed := True;
      teststr := StandardTempPath + 'testing_write_privilege_for_winst';
      if not fileexists(teststr) then
      begin
        if not SysUtils.ForceDirectories(teststr) or not RemoveDir(teststr) then
          testpassed := False;
      end;
      teststr := '';
    end;

    if testpassed then
      TempPath := StandardTempPath
    else
      TempPath := ValueOfEnvVar('TEMP') + PATHSEPARATOR;

    testpassed := False;
    if not Assigned(logdatei) then
      LogDatei := TLogInfo.Create;
    if (logdatei.StandardMainLogPath <> '') and
      SysUtils.ForceDirectories(logdatei.StandardMainLogPath) then
    begin
      testpassed := True;
      teststr := logdatei.StandardMainLogPath + 'testing_write_privilege_for_winst';
      if not fileexists(teststr) then
      begin
        if not SysUtils.ForceDirectories(teststr) or not RemoveDir(teststr) then
          testpassed := False;
      end;
      teststr := '';
    end;

    if testpassed then
      LogPath := logdatei.StandardMainLogPath
    else
      LogPath := TempPath;


    //LogDateiName := LogPath + StandardLogFilename + StandardLogFileext;
    LogDateiName := '';
    logfileFromCommandLine := False;

    //LogdateiName can be reset via a command line option.
    //afterwards TempPath is redefined from its path;

    SkriptDatei := '';
    scriptlist := TXStringList.Create;
    //BatchWindowMode := bwmNotActivated;
    ExtraParameter := '';

    //determinateRemoteLogging;
    //if (remoteErrorLogging <> trel_syslog) or (sysloghost = '') then
    //  idsyslog1.active := False
    //else
    //  idsyslog1.Host := sysloghost;


    NumberOfTrueParameters := 0;

    ParamListe := TStringList.Create;
    ParamListe.Clear;

    for i := 1 to ParamCount do
    begin
      teststr := SysToUTF8(ParamStr(i));
      ParamListe.Add(teststr);
    end;

    // if paramcount > 0
    if ParamListe.Count > 0 then
    begin
      startupmessages.Append('Called with parameter: ' + ParamListe.Text);
      Parameter := ParamListe.Strings[0];
      if Parameter[1] = '?' then
      begin
        ProgramMode := pmInfo;
        exit;
      end;
    end;

    i := 1;
    (* while i <= paramcount do *)
    while i <= ParamListe.Count do
    begin
      (* Parameter := paramStr (i); *)
      Parameter := ParamListe.Strings[i - 1];


      if Parameter[1] = ParamDelim then
      begin
        Delete(Parameter, 1, 1);
        if (Parameter = '?') or (lowerCase(Parameter) = 'h') or
          (lowerCase(Parameter) = 'help') then
        begin
          ProgramMode := pmInfo;
          exit;
        end

        else if LowerCase(parameter) = 'histolist' then
        begin
          Inc(i);
          if i <= (* paramcount *)ParamListe.Count then
          begin
            ProgramMode := pmHistoDialog;
            (* IniFileLocalization := ParamStr (i); *)
            IniFileLocalization := ParamListe.Strings[i - 1];
            case IniFileLocalization[length(IniFileLocalization)] of
              ':': IniFileLocalization :=
                  IniFileLocalization + PathDelim + StandardIniFile;
              PathDelim: IniFileLocalization :=
                  IniFileLocalization + StandardIniFile
            end;
            IniFileLocalization := ExpandFileName(IniFileLocalization);
            Inc(i);
          end;
        end

        else if LowerCase(parameter) = 'parameter' then
        begin
          Inc(i);
          weiter := True;
          while weiter and (i <= (* paramCount*) ParamListe.Count) do
          begin
            (* Next := ParamStr (i); *)
            Next := ParamListe.Strings[i - 1];
            if (Next[1] <> ParamDelim) then
            begin
              ExtraParameter := ExtraParameter + Next;
              Inc(i);
            end
            else
              weiter := False;
          end;
        end

        else if Lowercase(Parameter) = 'opsiservice' then
        begin
          try
            if not ((ProgramMode = pmNotSet) or (ProgramMode = pmBatch)) then
            begin
              ProgramMode := pmInfo;
              exit;
            end
            else if (NumberOfTrueParameters >= 1) and (ProgramMode = pmNotSet) then
              (* pmBuildPC geht nicht mehr *)
            begin
              ProgramMode := pmInfo;
              exit;
            end
            else
            begin
              try
                if not (ProgramMode = pmBatch) then
                  ProgramMode := pmBuildPC_service;

                computername := '';
                if not readconfig then
                  startupmessages.Append(
                    'Error: readconfig failed: Working with defaults ' +
                    DateTimeToStr(Now));
                startupmessages.Append('Depot path from readconfig:  ' +
                  depotdrive + depotdir + ' ' + DateTimeToStr(Now));
                Inc(i);
                if i <= ParamListe.Count then
                begin
                  opsiserviceURL := ParamListe.Strings[i - 1];
                  NumberOfTrueParameters := 1;
                  Inc(i);
                  if '/rpc' = lowercase(rightstr(opsiserviceURL, 4)) then
                    opsiserviceURL :=
                      LeftStr(opsiserviceURL, length(opsiserviceURL) - 4);
                end
                else
                begin
                  // take depotserver as default
                  divideAtFirst('\', depoturl, part1, part2);
                  divideAtFirst('\', part2, part1, part2);
                  divideAtFirst('\', part2, part1, part2);
                  opsiserviceURL :=
                    'https://' + part1 + ':' + IntToStr(opsiservicePORThttps);
                end;
              except
                on e: Exception do
                begin
                  ProgramMode := pmInfo;
                  startupmessages.Append(
                    'Exception in GetParameter: opsiserviceURL' +
                    e.message + ' ' + DateTimeToStr(Now));
                  //LogDatei.log('Exception in GetParameter: opsiserviceURL' + e.message, LLError);
                end;
              end;


              lookForSpecificParameters := True;
              while (i <= ParamListe.Count) and lookForSpecificParameters do
              begin
                r := ParamListe.Strings[i - 1] (* ParamStr (i) *);

                if (lowercase(r) = paramDelim + 'clientid') then
                begin
                  Inc(i);
                  if (i <= ParamListe.Count) then
                  begin
                    opsiserviceClientId := ParamListe.Strings[i - 1];
                    if opsiserviceUser = '' then opsiserviceUser := opsiserviceClientId;
                    computername := opsiserviceClientId;
                    if (length(computername) = 0) or
                      (computername[1] = ParamDelim) then
                    begin
                      ProgramMode := pmInfo;
                      exit;
                    end;
                    Inc(i);
                  end
                  else
                  begin
                    ProgramMode := pmInfo;
                    exit;
                  end;
                end


                else if (lowercase(r) = paramDelim + 'username') then
                begin
                  Inc(i);
                  if i <= ParamListe.Count then
                  begin
                    opsiserviceUser := ParamListe.Strings[i - 1];
                    //  opsiserviceUser might be the clientid (with hostkey as pw)
                    if opsiserviceClientId = '' then opsiserviceClientId := opsiserviceUser;
                    if (length(opsiserviceUser) = 0) or
                      (opsiserviceUser[1] = ParamDelim) then
                    begin
                      ProgramMode := pmInfo;
                      exit;
                    end;
                    Inc(i);
                  end
                  else
                  begin
                    ProgramMode := pmInfo;
                    exit;
                  end;
                end
                else if (lowercase(r) = paramDelim + 'password') then
                begin
                  Inc(i);
                  if i <= ParamListe.Count then
                  begin
                    opsiservicePassword :=
                      ParamListe.Strings[i - 1];
                    if (length(opsiservicePassword) = 0) or
                      (opsiservicePassword[1] = ParamDelim) then
                    begin
                      ProgramMode := pmInfo;
                      exit;
                    end;
                    Inc(i);
                  end;
                end
                else if (lowercase(r) = paramDelim + 'sessionid') then
                begin
                  Inc(i);
                  if i <= ParamListe.Count then
                  begin
                    opsiserviceSessionId := ParamListe.Strings[i - 1];
                    if (length(opsiserviceSessionId) = 0) or
                      (opsiserviceSessionId[1] = ParamDelim) then
                    begin
                      opsiserviceSessionId := '';
                    end
                    else
                      Inc(i);
                  end;
                end
                else if (lowercase(r) = paramDelim + 'credentialfile') then
                begin
                  Inc(i);
                  if i <= ParamListe.Count then
                  begin
                    list1.LoadFromFile(ExpandFileNameUTF8(ParamListe.Strings[i - 1]));
                    opsiserviceUser := list1.getStringValue('username');
                    opsiservicePassword := list1.getStringValue('password');
                    opsiserviceSessionId := list1.getStringValue('opsiserviceSessionId');
                    if opsiserviceSessionId = 'NULL' then
                      opsiserviceSessionId := '';
                    startupmessages.Add('found as credentials:' +
                      opsiserviceUser + ' ; ' + opsiservicePassword +
                      ' ; ' + opsiserviceSessionId);
                    if (length(ParamListe.Strings[i - 1]) = 0) or
                      (ParamListe.Strings[i - 1][1] = ParamDelim) then
                    begin
                      ProgramMode := pmInfo;
                      exit;
                    end;
                    Inc(i);
                  end
                  else
                  begin
                    ProgramMode := pmInfo;
                    exit;
                  end;
                end

                else if (lowercase(r) = paramDelim + 'depotpath') then
                begin
                  Inc(i);
                  if (i <= ParamListe.Count) then
                  begin
                    depotpath := ParamListe.Strings[i - 1];
                    if (length(depotpath) = 0) or
                      (depotpath[1] = ParamDelim) then
                    begin
                      ProgramMode := pmInfo;
                      exit;
                    end
                    else
                    begin
                      startupmessages.Append(
                        'Depot path from parameters:  ' + depotpath);
                      depotdrive := depotpath;
                     {$ifdef windows}
                      depotdrive := extractfiledrive(depotpath);
                      depotdir := extractfiledir(depotpath);
                     {$endif}
                      startupmessages.Append(
                        'Depot drive+dir from parameters:  ' + depotdrive + depotdir);
                    end;
                    Inc(i);
                  end
                  else
                  begin
                    ProgramMode := pmInfo;
                    exit;
                  end;
                end

                else if (lowercase(r) = paramDelim + 'usercontext') then
                begin
                  Inc(i);
                  if i <= ParamListe.Count then
                  begin
                    r := ParamListe.Strings[i - 1];
                    if r[1] = ParamDelim then
                    begin
                      ProgramMode := pmInfo;
                      exit;
                    end;
                    usercontext := opsiunQuotestr(trim(r), '"');
                    if (usercontext = '') or (usercontext = '\') then
                    begin
                      usercontext := '';
                      inUsercontext := False;
                    end
                    else
                      inUsercontext := True;
                    Inc(i);
                  end
                  else
                  begin
                    ProgramMode := pmInfo;
                    exit;
                  end;
                end

                else if (lowercase(r) = paramDelim + 'productlist') then
                begin
                  Inc(i);
                  if i <= ParamListe.Count then
                  begin
                    r := ParamListe.Strings[i - 1];
                    if r[1] = ParamDelim then
                    begin
                      ProgramMode := pmInfo;
                      exit;
                    end;
                    r := opsiunQuotestr(trim(r), '"');
                    if (r = '') then
                    begin
                      runproductlist := False;
                      ProgramMode := pmInfo;
                      exit;
                    end
                    else
                      stringsplit(r, ',', scriptlist);
                    // list of productIds now in scriptlist
                    runproductlist := True;
                    Inc(i);
                  end
                  else
                  begin
                    ProgramMode := pmInfo;
                    exit;
                  end;
                end

                else if (lowercase(r) = paramDelim + 'processproducts') then
                begin
                  Inc(i);
                  if i <= ParamListe.Count then
                  begin
                    r := ParamListe.Strings[i - 1];
                    if r[1] = ParamDelim then
                    begin
                      ProgramMode := pmInfo;
                      exit;
                    end;
                    r := opsiunQuotestr(trim(r), '"');
                    if (r = '') then
                    begin
                      runprocessproducts := False;
                      ProgramMode := pmInfo;
                      exit;
                    end
                    else
                      stringsplit(r, ',', scriptlist);
                    // list of productIds now in scriptlist
                    runprocessproducts := True;
                    Inc(i);
                  end
                  else
                  begin
                    ProgramMode := pmInfo;
                    exit;
                  end;
                end

                else if (lowercase(r) = paramDelim + 'loginscripts') then
                begin
                  if runproductlist then
                  begin
                    ProgramMode := pmInfo;
                    exit;
                  end;
                  runloginscripts := True;
                  allLoginScripts := False;
                  inUsercontext := True;
                  //usercontext := getloggedonDomUser;
                  Inc(i);
                end

                else if (lowercase(r) = paramDelim + 'allloginscripts') then
                begin
                  if runproductlist then
                  begin
                    ProgramMode := pmInfo;
                    exit;
                  end;
                  runloginscripts := True;
                  allLoginScripts := True;
                  inUsercontext := True;
                  //usercontext := getloggedonDomUser;
                  Inc(i);
                end

                else if (lowercase(r) = paramDelim + 'silent') then
                begin
                  runSilent := True;
                  Inc(i);
                end

                else
                  LookforSpecificParameters := False;

              end;

              if computername = '' then
                computername := getComputerName;

            end;
          except
            on e: Exception do
            begin
              ProgramMode := pmInfo;
              //LogDatei.log('Exception in GetParameter: opsiservice' + e.message, LLError);
              startupmessages.Append('Exception in GetParameter: opsiservice'
                + e.message + ' ' + DateTimeToStr(Now));
            end;
          end;
        end

        (* not supported anymore (4.12.4.16 do 27.7.21)
        else if Lowercase(Parameter) = 'pcprofil' then
        begin
          if ProgramMode <> pmNotSet then
          begin
            ProgramMode := pmInfo;
            exit;
          end
          else if NumberOfTrueParameters >= 1 then
            // pmBuildPC geht nicht mehr
          begin
            ProgramMode := pmInfo;
            exit;
          end
          else
          begin
            ProgramMode := pmInfo;
            exit;
          end;
        end
        *)

        else if Lowercase(Parameter) = 'logfile' then
        begin
          Inc(i);
          if i <= ParamListe.Count then
          begin
            r := ParamListe.Strings[i - 1];
            if r[1] = ParamDelim then
            begin
              ProgramMode := pmInfo;
              exit;
            end;

            LogDateiName := r;
            logfileFromCommandLine := True;
            Inc(i);
          end

          else
          begin
            ProgramMode := pmInfo;
            exit;
          end;
        end
       {$IFDEF GUI}
        else if Lowercase(Parameter) = 'lang' then
        begin
          Inc(i);
          if i <= ParamListe.Count then
          begin
            r := ParamListe.Strings[i - 1];
            if r[1] = ParamDelim then
            begin
              ProgramMode := pmInfo;
              exit;
            end;
            //            SetDefaultLang(r);
            Inc(i);
          end
          else
          begin
            ProgramMode := pmInfo;
            exit;
          end;
        end
       {$ENDIF GUI}
        else if Lowercase(Parameter) = 'usercontext' then
        begin
          Inc(i);
          if i <= ParamListe.Count then
          begin
            r := ParamListe.Strings[i - 1];
            if r[1] = ParamDelim then
            begin
              ProgramMode := pmInfo;
              exit;
            end;
            usercontext := r;
            Inc(i);
          end
          else
          begin
            ProgramMode := pmInfo;
            exit;
          end;
        end

        else if Lowercase(Parameter) = 'productid' then
        begin
          Inc(i);
          if i <= ParamListe.Count then
          begin
            r := ParamListe.Strings[i - 1];
            if r[1] = ParamDelim then
            begin
              ProgramMode := pmInfo;
              exit;
            end;
            batchproductid := r;
            Inc(i);
          end
          else
          begin
            ProgramMode := pmInfo;
            exit;
          end;
        end

        else if Lowercase(Parameter) = 'logproductid' then
        begin
          Inc(i);
          if i <= ParamListe.Count then
          begin
            r := ParamListe.Strings[i - 1];
            if r[1] = ParamDelim then
            begin
              ProgramMode := pmInfo;
              exit;
            end;
            logproductid := r;
            Inc(i);
          end
          else
          begin
            ProgramMode := pmInfo;
            exit;
          end;
        end


        else if Lowercase(Parameter) = 'batch' then
        begin
          ProgramMode := pmBatch;
          {$IFDEF GUI}
          BatchWindowMode := bwmNormalWindow;
          SavedBatchWindowMode := BatchWindowMode;
          {$ENDIF GUI}
          Inc(i);
        end

        else if Lowercase(Parameter) = 'servicebatch' then
        begin
          // if /serviceBatch then also needed:
          //  /ClientId /opsiservice /username /password /productId  and <script path>
          ProgramMode := pmBatch;
          batchUpdatePOC := True;
          {$IFDEF GUI}
          BatchWindowMode := bwmNormalWindow;
          SavedBatchWindowMode := BatchWindowMode;
          {$ENDIF GUI}
          Inc(i);
        end

        else if Lowercase(Parameter) = 'silent' then
        begin
          ProgramMode := pmSilent;
          runSilent := True;
          Inc(i);
        end

        else if LowerCase(Parameter) = 'normalwindow' then
        begin
          {$IFDEF GUI}
          BatchWindowMode := bwmNormalWindow;
          SavedBatchWindowMode := BatchWindowMode;
          {$ENDIF GUI}
          Inc(i);
        end

        else if Lowercase(Parameter) = 'testsyntax' then
        begin
          configTestSyntax := True;
          Inc(i);
        end

        else
        begin
          ProgramMode := pmInfo;
          exit;
        end;
      end

      else
      begin
        Inc(NumberOfTrueParameters);

        case NumberOfTrueParameters of
          1:
          begin
            if length(parameter) > 0 then
            begin
              stringsplit(parameter, ';', scriptlist);
              Skriptdatei := scriptlist.Strings[0];
            end;
          end;
          2: LogDateiName := Parameter;
          3:
          begin
            ProgramMode := pmInfo;
            exit;
          end
        end;
        Inc(i);
      end;
    end;

    if ProgramMode = pmNotSet then
    {$IFDEF GUI}
      ProgramMode := pmStandard;
    {$ELSE GUI}
    ProgramMode := pmInfo;
    {$ENDIF GUI}

    ParamListe.Free;
    ParamListe := nil;
    list1.Free;
  except
    on e: Exception do
    begin
      ProgramMode := pmInfo;
      //LogDatei.log('Exception in GetParameter: General' + e.message, LLError);
      startupmessages.Append('Exception in GetParameter: General' +
        e.message + ' ' + DateTimeToStr(Now));
    end;
  end;
end;

{$IFNDEF GUI}
procedure main;

var
  ErrorInfo: string;
  str: string;
  s2, s3, s4, sid: string;

begin
  starttimestr := DateTimeToStr(Now);
  if not Assigned(startupmessages) then
  begin
    startupmessages := TStringList.Create;
    startupmessages.Clear;
  end;
  startupmessages.Append('startmessage opsi-script created at main: ' +
    DateTimeToStr(Now));
  toggle := True;
  initEncoding;
  ProductvarsForPC := TStringList.Create;
  ProductvarsForPC.Clear;
  if not Assigned(ProductsRunnedUsingReloadProductList) then ProductsRunnedUsingReloadProductList := TStringList.Create;

  {$IFDEF WINDOWS}
  if FileExists(GetWinSystemDirectory + 'w32hupsi.dll') then
    // very old version
  begin
    WinstRegKey := veryOldWinstRegKey;
  end
  else
  begin
    regist := TRegistry.Create;
    regist.RootKey := HKEY_LOCAL_MACHINE;
    if Regist.OpenKey('\SOFTWARE\opsi.org\preloginloader', False) then
      try
        str := Regist.ReadString('WinstRegKey');
        if str <> '' then
          WinstRegKey := str;
      except
      end;
    regist.Free;
  end;


  ShellIsExplorer := True;
  regist := TRegistry.Create;
  regist.RootKey := HKEY_LOCAL_MACHINE;
  if Regist.OpenKey('\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Winlogon', False) then
    if not (LowerCase(Regist.ReadString('Shell')) = 'explorer.exe') then
      ShellIsExplorer := False;


  regist.Free;

  if GetuibOsType(ErrorInfo) = tovWin95 then
    ShellIsExplorer := True;
  {$ENDIF WINDOWS}
  try
    GetParameter;
    StartProgramModes;
  except
    on E: Exception do
    begin
      writeln('osmain.main : ' + E.Message);
    end;
  end;
end;

{$ENDIF GUI}


initialization

  opsiserviceSessionId := '';
  {$IFDEF GUI}
  try
    //    GetDefaultLang;
  except
  end;
  {$ENDIF GUI}
end.
