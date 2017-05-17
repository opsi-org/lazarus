
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
// credits: http://www.opsi.org/credits/

//***************************************************************************
// Subversion:
// $Revision: 508 $
// $Author: oertel $
// $Date: 2016-10-20 16:20:16 +0200 (Do, 20 Okt 2016) $
//***************************************************************************



{$IFDEF WINDOWS} {$DEFINE GUI} {$ENDIF WINDOWS}
interface

uses
{$IFDEF WINDOWS}
  Windows,
  VersionInfoX,
  registry,
  osregistry,
  systemcriticalu,
{$IFDEF WIN32}
osfuncwin2,
{$ENDIF WIN32}
  shellapi,
  wispecfolder,
{$ENDIF WINDOWS}
{$IFDEF LINUX}
  osfunclin,
  lispecfolder,
  baseunix,
  oscrypt,
{$ENDIF LINUX}
{$IFDEF GUI}
  osmessagedialog,
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
  SysUtils,
  Classes,
  synautil,
  oswebservice,
  //wirequlist,
  oslog, osparser, osfunc,
  //IdSysLog,
  strutils,
  inifiles;


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




var

 // CentralForm: TCentralForm;
 // CentralFormVisible: boolean;
  Skriptdatei: string;
  scriptlist: TXStringList;
  usercontext: string;
  batchproductid: string = '';  //id of product we are running in batch mode
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

  runUpdate : boolean;
  DontUpdateMemo: boolean = False;
  PerformExitProgram: boolean = False;
  PerformExitWindows: TExitRequest = txrNoExit;
  PerformShutdown: TShutdownRequest = tsrNoShutdown;


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
  TimerWait_waitedIntervals: word;

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

  ProgramMode: TProgramMode;

  Profildateiname: string;

  Produkt: string;

  //Verfahren  : TActionRequest;

  Produkte: TStringList;

///NumberOfErrors   :   Integer;
///NumberOfWarnings :   Integer;

///LogLevel :   Integer;
///ExitOnError :   Boolean;
///ReportMessages :   Boolean;


const
  MaxSavedScriptFiles = 20;
  StandardIniFile = 'opsi-script.ini';


procedure saveVersionToProfile;
var
  myinifile: TIniFile;
  mypath: string;
begin
  mypath := GetAppDataPath + PathDelim+'.opsi.org';
  if not SysUtils.ForceDirectories(mypath) then
  begin
    mypath := GetUserProfilePath + PathDelim+'.opsi.org';
    SysUtils.ForceDirectories(mypath);
  end;
  myinifile := TIniFile.Create(mypath + PathDelim+'userLoginScripts.ini');
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
  mypath := GetAppDataPath + PathDelim+'.opsi.org';
  if not FileExists(mypath + PathDelim+'userLoginScripts.ini') then
    mypath := GetUserProfilePath + PathDelim+'.opsi.org';
  if FileExists(mypath + PathDelim+'userLoginScripts.ini') then
  begin
    myinifile := TIniFile.Create(mypath + PathDelim+'userLoginScripts.ini');
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
{$IFDEF LINUX}
var
  myconf : TIniFile;
{$ENDIF LINUX}
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
      LogDatei.DependentAdd(problem, LLError);
      Result := False;
    end;
  end;
  {$ENDIF}
  {$IFDEF LINUX}
  try
    myconf := TIniFile.Create(opsiscriptconf);
    myconf.WriteString('general','bootmode',bootmode);
    myconf.Free;
    Result := True;
  except
    on E: Exception do
    begin
      problem := 'exception while writing ' +
        opsiscriptconf + ' error ' + e.message;
      Result := False;
    end;
  end;
  {$ENDIF LINUX}
end;


procedure getBootmode(var bootmode: string; var fromRegistry: boolean);
{$IFDEF LINUX}
var
  myconf : TIniFile;
{$ENDIF LINUX}
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
    LogDatei.DependentAdd('bootmode from registry: ' + bootmode, LLDebug);
  end
  else
  begin
    bootmode := ValueOfEnvVar('BOOTMODE');
    LogDatei.DependentAdd('bootmode from environment: ' + bootmode, LLDebug);
  end;
{$ENDIF}
{$IFDEF LINUX}
  try
    myconf := TIniFile.Create(opsiscriptconf);
    bootmode := myconf.ReadString('general','bootmode','BKSTD');
    myconf.Free;
  except
    on E: Exception do
    begin
      LogDatei.DependentAdd('exception while reading ' +
        opsiscriptconf + ' error ' + e.message, LLERROR);
    end;
  end;
  {$ENDIF LINUX}
end;

function determinateRemoteLogging: boolean;

var
  registval: longint;
  ///properties  :   TStringList;
begin
  Result := true;
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
begin
  Result := StandardTempPath;
  //result := TmpPathFromLogdatei;
  SysUtils.ForceDirectories(Result);
  if not DirectoryExists(Result) then
  begin
    Result := ValueOfEnvVar('TEMP');
    if not DirectoryExists(Result) then
    begin
      {$IFDEF WINDOWS}
      Result := 'c:\tmp';
      {$ELSE}
      Result := '/tmp/opsiscript';
      {$ENDIF WINDOWS}
      mkdir(Result);
    end;
    Result := Result + PathDelim;
  end;
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
    if LogDatei <> nil then
    begin
      LogDatei.LogSIndentLevel := 0;
      LogDatei.log('============  opsi-script ' + winstversionname +
        ' is regularly exiting. Time ' + FormatDateTime(
        'yyyy-mm-dd  hh:mm:ss ', now) + '.', LLessential);

      sleep(1000);
      LogDatei.Free;
      LogDatei := nil;
      {$IFDEF WINDOWS} SystemCritical.IsCritical := false; {$ENDIF WINDOWS}
    end;
    try
      //TerminateApp;
      halt(0);
      //Application.Terminate;
    except
      // test
      Halt(0);
    end;
  except
    try
      halt(0);
      //TerminateApp;
      //Application.Terminate;
    except
      // test
      Halt;
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
  {$IFDEF LINUX}
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


    osfunc.Stringsplit(expandedScriptname, pathdelim, partsExpanded);
    osfunc.Stringsplit(scriptPath, pathdelim, partsScriptPath);


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


procedure ProcessProdukt(var extremeErrorLevel: TErrorLevel);

var
  Verfahren: TAction;
  Pfad: string;
  scriptname, absscriptname: string;



  function ProcessNonZeroScript(Skriptdateiname: string;
  var extremeErrorLevel: TErrorLevel): boolean;

  var
    Scriptname: string;
  begin
    Logdatei.log('Start ProcessNonZeroScript', LLdebug2);
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
      CreateAndProcessScript(Scriptname, NestingLevel, False, extremeErrorLevel);
      Logdatei.log('After CreateAndProcessScript', LLdebug2);
    end;
    Logdatei.log('End ProcessNonZeroScript', LLdebug2);
  end;

begin
  //updateAfterSetup := false;
  Logdatei.log('Entering ProcessNonZeroScript', LLdebug2);

  runUpdate := true;

  if runloginscripts then
    Verfahren := tacLogin
  else
  if runproductlist then
    Verfahren := tacSetup
  else
    Verfahren := opsidata.getProductAction;
  Logdatei.log('Actionrequest for Product: '+Produkt+' is: '+opsidata.actionToString(Verfahren), LLInfo);
  if Verfahren = tacNull then
    exit;

  ProductvarsForPC := opsidata.getProductproperties;
  if runproductlist then
    if not opsidata.setAddProductOnClientDefaults(true) then
      LogDatei.DependentAdd('failed telling server to look for productOnClient defaults', LLerror);
  if not opsidata.initProduct then extremeErrorLevel := levelFatal;
  if runproductlist then
    if not opsidata.setAddProductOnClientDefaults(false) then
          LogDatei.DependentAdd('failed telling server to stop looking for productOnClient defaults',
            LLerror);
  Logdatei.DependentAdd('ProcessNonZeroScript opsidata initialized', LLdebug2);
  Pfad := opsidata.getSpecialScriptPath;
  //only for backward compatibility and for special circumstances
{$IFDEF LINUX}
  if not fileexists(depotdrive) then depotdrive := depotdrive_old;
{$ENDIF LINUX}
  if Pfad = '' //this should be the normal case since winst 4.2
  then
    // take pfad from depotdrive + depotdir (coming from registry)
  begin
    Pfad := depotdrive + depotdir;

    if Pfad[length(Pfad)] <> DirectorySeparator then
      Pfad := Pfad + PathDelim;
    Pfad := Pfad + Produkt;
  end;


  if Pfad <> '' then
  begin
    if Pfad[length(Pfad)] <> PathDelim then
      Pfad := Pfad + PathDelim;

  if runloginscripts then
    Verfahren := tacLogin
  else
  if runproductlist then
    Verfahren := tacSetup
  else
    Verfahren := opsidata.getProductAction;

    if Verfahren in [tacDeinstall, tacSetup, tacOnce, tacAlways,
      tacCustom, tacLogin] then
    begin
      scriptname := opsidata.getProductScriptPath(Verfahren);
      Logdatei.log('scriptname: "' + scriptname + '", special path: "' +
        pfad + '"', LLNotice);
      absscriptname := makeAbsoluteScriptPath(Pfad, scriptname);
      if Verfahren = tacSetup then
        opsidata.setProductState(tpsInstalling);
      if (Verfahren in [tacDeinstall, tacSetup, tacOnce, tacAlways, tacCustom]) or
        ((Verfahren = tacLogin) and (scriptname <> '')) then
        if not ProcessNonZeroScript(absscriptname, extremeErrorLevel)
        //meaning in the moment that scriptname is empty
        then
        begin
          extremeErrorLevel := LevelFatal;
          Logdatei.DependentAdd('Error level set to fatal, action type ' +
            sayActionType(Verfahren), LLCritical);
        end;
    end;
    Logdatei.DependentAdd('First ProcessNonZeroScript finished', LLDebug2);
    //Logdatei.DependentAdd('Setup script name: '+opsidata.getProductScriptPath(tacSetup), LLessential);


      if (Verfahren = tacUpdate) or ((Verfahren = tacSetup) and
        (PerformExitWindows <= txrReboot)) then
          //the setup script does not require a direct Reboot or Logout, and seems to be finished right now
      if extremeErrorLevel = LevelFatal then
      begin
        Logdatei.DependentAdd('We do not look for a update script, because the setup script is failed', LLnotice);
      end
      else
      if not runUpdate then
        Logdatei.DependentAdd('We do not look for a update script, because noUpdateScript is set', LLnotice)
      else
      begin
        opsidata.setProductActionRequest(tapUpdate);
        LogDatei.DependentAdd('product ' + Produkt + ' set to update', LLessential);
        Logdatei.DependentAdd('get Update script name ...', LLdebug2);
        scriptname := opsidata.getProductScriptPath(tacUpdate);
        Logdatei.DependentAdd('Update script name: ' + opsidata.getProductScriptPath(
          tacUpdate), LLdebug2);
        absscriptname := makeAbsoluteScriptPath(Pfad, scriptname);
        Logdatei.DependentAdd('Update script name: ' + absscriptname, LLessential);

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
  KorrektSortierte: integer;
  Bootmode: string;
  bootmodeFromRegistry: boolean;
  Fehler: string;
  numberValue: string;
  errorNumber: integer;
  extremeErrorLevel: TErrorLevel;
  showErrorMessages: boolean;
  ps: string;
  TheExitMode: TExitMode;
  {$IFDEF WINDOWS}
  regDataType: tuibRegDataType;
  {$ENDIF WINDOWS}


  procedure LogProductSequence(const produkte: TStringList;
    errorfound: boolean; numberOfCorrectItems: integer);

  var
    i: integer;
    Zeile: string;
    SaveProductname: string;
    requestedAction: TAction;
  begin
    LogDatei.LogProduktId:=False;
    SaveProductname := Topsi4data(opsidata).getActualProductId;
    LogDatei.DependentAdd('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~',
      BaseLevel);
    if errorfound then
      LogDatei.DependentAdd('Error in the conditions for the sequence of products, '
        + ' the sorted list of maximum length is:',
        BaseLevel)
    else
      LogDatei.DependentAdd('Resolved sequence of products (' +
        DateTimeToStr(Now) + '): ',
        BaseLevel);

    i := 0;
    while i < numberOfCorrectItems do
    begin
      // set productname to get action via opsidata
      opsidata.setActualProductName(produkte.strings[i]);

      requestedAction := opsidata.getProductAction;

      if errorfound or (requestedAction <> tacNull) then
      begin
        Zeile := 'Product ' + IntToStr(i) + ' ' + #9 + Produkte.Strings[i] +
          ' : ' + opsidata.actionToString(requestedAction);
        LogDatei.DependentAdd(Zeile, BaseLevel);
      end;

      Inc(i);
    end;
    LogDatei.DependentAdd('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~', BaseLevel);

    opsidata.setActualProductName(SaveProductname);
  end;

  function ChangeProductstatusOnReinst(const Bootmode: string): boolean;

  var
    i: integer;
    productState: TProductState;
    productActionRequest: TActionRequest;
  begin
    Result := False;
    LogDatei.LogProduktId:= False;
    LogDatei.DependentAdd('bootmode ' + bootmode, LLNotice);
    if Bootmode = 'REINS' then
    begin
      {$IFDEF GUI}
      FBatchOberflaeche.setInfoLabel('Reloading product list after reinstallation...');
      {$ENDIF GUI}
      if opsidata.getOpsiServiceVersion = '4' then
      begin
        LogDatei.DependentAdd('telling server to look for dependent products',
          LLessential);
        if not TOpsi4Data(opsidata).setAddDependentProductOnClients(True) then
          LogDatei.DependentAdd('failed telling server to look for dependent products',
            LLerror);
      end;
      LogDatei.DependentAdd('setting all on-products to setup', LLessential);
      for i := 1 to Produkte.Count do
      begin
        Produkt := Produkte.Strings[i - 1];
        LogDatei.DependentAdd('inspecting product "' + Produkt, LLessential);

        opsidata.setActualProductName(Produkt);

        productState := opsidata.getProductState;
        productActionRequest := opsidata.getProductActionRequest;

        LogDatei.DependentAdd('product "' + Produkt + '" has state ' +
          opsidata.stateToString(productState) + ', action request is ' +
          opsidata.actionRequestToString(productActionRequest)
          , LLessential);

        if (produkt <> selfProductName) and (produkt <> 'opsi-winst') then
        begin
          if (productState in [tpsInstalled, tpsInstalling, tpsFailed]) then
          begin
            opsidata.setProductState(tpsNotInstalled);
            if productActionRequest in [tapNull, tapUpdate] then
            begin
              opsidata.setProductActionRequest(tapSetup);
              LogDatei.DependentAdd('product "' + Produkt + '" set to setup',
                LLessential);
            end;
          end;
        end;
      end;
      if opsidata.getOpsiServiceVersion = '4' then
        if not TOpsi4Data(opsidata).setAddDependentProductOnClients(False) then
          LogDatei.DependentAdd(
            'failed telling server to look not for dependent products',
            LLerror);

      opsidata.saveOpsiConf;
      // reload the new productlist
      Produkte := OpsiData.getListOfProducts;
    end;
    Result := True;
    // no errors
  end;

var
  goOn: boolean;
  problemString: string;
  aktActionRequestStr : string;
  aktAction, orgAction : TAction;
  processProduct : boolean;
  ///val :   Integer;
  {$IFDEF LINUX}
  filehandle : cint;
  {$ENDIF LINUX}
  list : Tstringlist;
  opsiclientd : boolean;

begin
  try
    {$IFDEF WINDOWS} SystemCritical.IsCritical := true; {$ENDIF WINDOWS}
    goOn := True;
    {$IFDEF GUI}
    FBatchOberflaeche.ForceStayOnTop(False);
    {$ENDIF GUI}
    DontUpdateMemo := True;


    //OpsiData.initOpsiConf(pathnamsInfoFilename, profildateiname, ProdukteInfoFilename);
    OpsiData.setActualClient(computername);
    if Produkte <> nil then
      Produkte.Free;
    Produkte := OpsiData.getListOfProducts;

    LogDatei.DependentAdd('Computername:' + computername, baselevel);

    if computername <> ValueOfEnvVar('computername') then
      LogDatei.DependentAdd('Computername according to Environment Variable :' +
        ValueOfEnvVar('computername'),
        baseLevel);

    if opsiserviceURL <> '' then
      LogDatei.DependentAdd('opsi service URL ' + opsiserviceurl,
        baseLevel);

    LogDatei.log('Depot path:  ' + depotdrive + depotdir, LLinfo);
    LogDatei.log('', LLinfo);
    {$IFDEF GUI}
    FBatchOberflaeche.setInfoLabel(rsProductCheck);
    {$ENDIF GUI}
    ProcessMess;


    getBootmode(bootmode, bootmodeFromRegistry);
  except
    on e: exception do
    Begin
      LogDatei.DependentAddError ('exception in BuildPC: starting ' + e.message, LLError);
      {$IFDEF WINDOWS} SystemCritical.IsCritical := false; {$ENDIF WINDOWS}
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
        {$IFDEF WINDOWS} and bootmodeFromRegistry {$ENDIF WINDOWS}
        then
          if not setBootmode('BKSTD', Fehler) then
          begin
            LogDatei.DependentAdd('Bootmode could not be set, ' + Fehler, LLError);
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
          LogDatei.DependentAddError('Got no product sorting by server.',LLError);
        end;
      end;

      i := 1;
      while (i <= Produkte.Count) and (PerformExitWindows < txrReboot) and
        not PerformExitProgram do
      begin
        processProduct := false;
        Produkt := Produkte.Strings[i - 1];
        opsidata.setActualProductName(Produkt);
        // get the actionrequest from the original productlist created at startup
        orgAction := opsidata.getProductAction;
        if (orgAction <> tacNull) then
        begin

          if trim(Produkt) = '' then
            LogDatei.DependentAdd('product ' + IntToStr(i - 1) + ' is "" ', LLWarning);

          if opsidata.initProduct then ;


          // check if there is still an action request if we had one at startup
          // get the actual (live) actionrequest
          aktActionRequestStr :=opsidata.getActualProductActionRequest;
          aktAction := opsidata.actionRequestStringToAction(aktActionRequestStr);
          Logdatei.log('Actionrequest for product: '+Produkt+' is (original/actual): ('
                        +opsidata.actionToString(orgAction)+' / '+aktActionRequestStr+')', LLInfo);
          // process product only if we have a original action request which is still set
          if (aktAction <> tacNull) and (orgAction <> tacNull) then processProduct := true
          else processProduct := false;
        end;
        if processProduct then
        begin
          LogDatei.DependentAdd ('BuildPC: process product .....', LLDebug3);
          extremeErrorLevel := Level_not_initialized;
          logdatei.ActionProgress := '';
          {$IFDEF GUI}
          // maximize window
          BatchWindowMode := bwmMaximized;
          SavedBatchWindowMode := BatchWindowMode;
          FBatchOberflaeche.setWindowState(BatchWindowMode);
          ProcessMess;
          //FBatchOberflaeche.setWindowState(bwmMaximized);
          {$ENDIF GUI}
          LogDatei.LogProduktId:=true;

          ProcessProdukt(extremeErrorLevel);

          //FBatchOberflaeche.setWindowState(bwmNormalWindow);
          LogDatei.DependentAdd ('BuildPC: update switches .....', LLDebug);
          if (PerformExitWindows < txrImmediateLogout) and (not scriptsuspendstate) then
          begin
            LogDatei.DependentAdd ('BuildPC: update switches 2.....', LLDebug3);
            opsidata.UpdateSwitches(extremeErrorLevel, logdatei.actionprogress);
          end;
          LogDatei.DependentAdd ('BuildPC: finishProduct .....', LLDebug3);
          opsidata.finishProduct;
          LogDatei.LogProduktId:=false;
        end;

        Inc(i);
      end;
      LogDatei.DependentAdd ('BuildPC: saveOpsiConf .....', LLDebug3);
      opsidata.saveOpsiConf;

      Produkte.Free;
      Produkte := nil;
      //Produktestatus.free; Produktestatus := nil;
    end;
  except
    on e: exception do
    Begin
      LogDatei.DependentAddError ('exception in BuildPC: walk through all products  ' + e.message, LLError);
      {$IFDEF WINDOWS} SystemCritical.IsCritical := false; {$ENDIF WINDOWS}
    end;
  end;

  try

    LogDatei.DependentAdd ('BuildPC: saveOpsiConf .....', LLDebug3);
    opsidata.saveOpsiConf;
    //SaveProfildatei;

    errorNumber := 0;

  // LINUX see below
  {$IFDEF WINDOWS}
    LogDatei.DependentAdd ('BuildPC: handle reboot options: write to registry .....', LLDebug3);
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
      begin
        WriteEntry(WinstRegFinalShutdownVar, trdInteger, IntToStr(RegCallShutdown));
        //LogDatei.DependentAdd('Written RegCallShutdown', BaseLevel)
      end
      else
      begin
        // we tried to ensure that we have a correct registry entry:
        // if it does not exist it is created with value 0
        // but it seems not to work
        //try
        //  val := ReadInteger (WinstRegFinalShutdownVar);
        //  LogDatei.DependentAdd('Read RegCallShutdown: ' + Inttostr(val), BaseLevel)
        //except
        // LogDatei.DependentAdd('Not read RegCallShutdown', BaseLevel);

        // WriteEntry(WinstRegFinalShutdownVar, trdInteger, IntToStr (RegNoCallShutdown));
        //end
      end;

      LogDatei.DependentAdd ('BuildPC: handle reboot options: registry log continue .....', LLDebug3);
      WriteEntry(WinstRegLastLogfile, trdString, LogDatei.FileName);

      if PerformExitWindows >= txrRegisterForReboot then
      begin
        WriteEntry(WinstRegContinueVar, trdInteger, '1');

        try
          if ReadEntry(WinstRegNumberOfErrors, regDataType, numberValue) and
            (numberValue <> '') then
            errorNumber := StrToInt(numberValue);
        except
        end;

        errorNumber := errorNumber + LogDatei.NumberOfErrors;

        // Aufheben der Fehlerzahl fuer spaetere Verwendung
        WriteEntry(WinstRegNumberOfErrors, trdInteger, IntToStr(errorNumber));

        // Zuruecksetzen, da sonst Fehlermeldung erscheint
        errorNumber := 0;
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
        '  ' + 'Error(s) occured while installation.' + #10 + 'Please contact your admin.',
        [mrOk]);
      {$ENDIF GUI}
    end;


    LogDatei.DependentAdd ('BuildPC: finishOpsiconf .....', LLDebug3);
    OpsiData.finishOpsiconf;


    {$IFDEF LINUX}
    opsiclientd := true;
    if '' = getcommandresult('ps --no-headers -C opsiclientd') then opsiclientd := false;
       if PerformExitWindows <> txrNoExit then
          begin
            case PerformExitWindows of
              txrRegisterForReboot,
              txrReboot,txrImmediateReboot:
                begin
                  if opsiclientd then
                  begin
                    TheExitMode := txmNoExit;
                    filehandle := fpOpen('/var/run/opsiclientd/reboot',O_WrOnly or O_Creat);
                    fpClose(filehandle);
                  end
                  else TheExitMode := txmReboot;
                end;

              txrRegisterforLogout, txrImmediateLogout: TheExitMode := txmLogout;
            end;


            if not ExitSession(TheExitMode, Fehler) then
              {$IFDEF GUI}
              MyMessageDlg.WiMessage('ExitWindows Error ' + LineEnding + Fehler, [mrOk]);
              {$ELSE GUI}
              writeln('ExitWindows Error ' + LineEnding + Fehler);
              {$ENDIF GUI}
          end;
    if PerformShutdown = tsrRegisterForShutdown then
    begin
      filehandle := fpOpen('/var/run/opsiclientd/shutdown',O_WrOnly or O_Creat);
      fpClose(filehandle);
    end;
    {$ENDIF LINUX}
    LogDatei.log ('BuildPC: Terminating .....', LLDebug3);
    {$IFDEF WINDOWS} SystemCritical.IsCritical := false; {$ENDIF WINDOWS}
    TerminateApp;
  except
    on e: exception do
    Begin
      LogDatei.log ('exception in BuildPC: final ' + e.message, LLError);
      LogDatei.log ('BuildPC: Terminating .....', LLDebug3);
      {$IFDEF WINDOWS} SystemCritical.IsCritical := false; {$ENDIF WINDOWS}
      TerminateApp;
    end;
  end;
end;


procedure Loginscripts;

var
  i: integer;
  KorrektSortierte: integer;
  Bootmode: string;
  bootmodeFromRegistry: boolean;
  Fehler: string;
  numberValue: string;
  errorNumber: integer;
  extremeErrorLevel: TErrorLevel;
  showErrorMessages: boolean;
  ps: string;
  itemlist: TXStringlist;
  user, dom, s4, sid: string;
  ErrorInfo: string;
  goOn: boolean;
  problemString: string;
  ///val :   Integer;
  trycounter, maxtries: integer;
  {$IFDEF WINDOWS}
  regDataType: tuibRegDataType;
  {$ENDIF WINDOWS}


begin
  LogDatei.LogProduktId:=false;
  goOn := True;
  {$IFDEF GUI}
  FBatchOberflaeche.ForceStayOnTop(False);
  //BatchWindowMode := bwmNormalWindow;
  //FBatchOberflaeche.setWindowState(BatchWindowMode);
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
        {$IFDEF WIN32}usercontext := getloggedonDomUser;{$ENDIF}
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
  //LogDatei.DependentAdd('sidStr :'+sid,LLDebug);
  sid := copy(sid, 2, length(sid) - 2);
  LogDatei.DependentAdd('sidStr :' + sid, LLDebug);
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
  if Produkte <> nil then
    Produkte.Free;
  //Produkte := OpsiData.getListOfProducts;
  Produkte := TOpsi4data(OpsiData).getMapOfLoginscripts2Run(allLoginScripts);


  LogDatei.DependentAdd('Computername:' + computername, baselevel);

  if computername <> ValueOfEnvVar('computername') then
    LogDatei.DependentAdd('Computername according to Environment Variable :' +
      ValueOfEnvVar('computername'),
      baseLevel);

  if opsiserviceURL <> '' then
    LogDatei.DependentAdd('opsi service URL ' + opsiserviceurl,
      baseLevel);

  LogDatei.log('Depot path:  ' + depotdrive + depotdir, LLinfo);
  LogDatei.DependentAdd('', BaseLevel);
  {$IFDEF GUI}
  FBatchOberflaeche.setInfoLabel(rsProductCheck);
  ProcessMess;
  {$ENDIF GUI}
  (* walk through all products *)
  if goOn then
  begin
    LogDatei.LogProduktId:=true;
    i := 1;
    while (i <= Produkte.Count) and (PerformExitWindows < txrReboot) and
      not PerformExitProgram do
    begin
      Produkt := Produkte.Strings[i - 1];
      scriptMode := tsmLogin;

      opsidata.setActualProductName(Produkt);
      if trim(Produkt) = '' then
        LogDatei.DependentAdd('product ' + IntToStr(i - 1) + ' is "" ', BaseLevel);

      extremeErrorLevel := Level_not_initialized;

      //if opsidata.getProductState = tpsInstalled then
      begin
        ProcessProdukt(extremeErrorLevel);
        opsidata.finishProduct;
      end;
      Inc(i);
    end;
    LogDatei.LogProduktId:=false;
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
  KorrektSortierte: integer;
  Bootmode: string;
  bootmodeFromRegistry: boolean;
  Fehler: string;
  numberValue: string;
  errorNumber: integer;
  extremeErrorLevel: TErrorLevel;
  showErrorMessages: boolean;
  ps: string;
  itemlist: TXStringlist;
  user, dom, s4, sid: string;
  ErrorInfo: string;
  goOn: boolean;
  problemString: string;
  ///val :   Integer;
  trycounter, maxtries: integer;
  {$IFDEF WINDOWS}
  regDataType: tuibRegDataType;
  {$ENDIF WINDOWS}


begin
  LogDatei.LogProduktId:=false;
  goOn := True;
  {$IFDEF GUI}
  FBatchOberflaeche.ForceStayOnTop(False);
  //BatchWindowMode := bwmNormalWindow;
  //FBatchOberflaeche.setWindowState(BatchWindowMode);
  {$ENDIF GUI}
  DontUpdateMemo := True;

  OpsiData.setActualClient(computername);
  if Produkte <> nil then
    Produkte.Free;
  //Produkte := OpsiData.getListOfProducts;
  Produkte := scriptlist;


  LogDatei.DependentAdd('Computername:' + computername, baselevel);

  if computername <> ValueOfEnvVar('computername') then
    LogDatei.DependentAdd('Computername according to Environment Variable :' +
      ValueOfEnvVar('computername'),
      baseLevel);

  if opsiserviceURL <> '' then
    LogDatei.DependentAdd('opsi service URL ' + opsiserviceurl,
      baseLevel);

  LogDatei.log('Depot path:  ' + depotdrive + depotdir, LLinfo);
  LogDatei.DependentAdd('', BaseLevel);
  {$IFDEF GUI}
  FBatchOberflaeche.setInfoLabel(rsProductCheck);
  ProcessMess;
  {$ENDIF GUI}
  (* walk through all products *)
  if goOn then
  begin
    LogDatei.LogProduktId:=true;
    i := 1;
    while (i <= Produkte.Count) and (PerformExitWindows < txrReboot) and
      not PerformExitProgram do
    begin
      Produkt := Produkte.Strings[i - 1];
      scriptMode := tsmMachine;

      opsidata.setActualProductName(Produkt);
      if trim(Produkt) = '' then
        LogDatei.DependentAdd('product ' + IntToStr(i - 1) + ' is "" ', BaseLevel);

      extremeErrorLevel := Level_not_initialized;

      //if opsidata.getProductState = tpsInstalled then
      begin
        ProcessProdukt(extremeErrorLevel);

        // update switches
        if extremeErrorLevel > levelfatal then
        begin
            //successful after setup
            opsidata.ProductOnClient_update('',
              tar4Successful,
              tac4None,
              ttc4Installed,
              tac4Setup,
              tps4Installed);
        end
        else //failed
        begin
            //failed after setup
            opsidata.ProductOnClient_update('',
              tar4Failed,
              tac4None,
              ttc4Installed,
              tac4Setup,
              tps4Unkown);
        end; // failed
        opsidata.finishProduct;
      end;
      Inc(i);
    end;
    LogDatei.LogProduktId:=false;
    opsidata.saveOpsiConf;
    Produkte.Free;
    Produkte := nil;
  end;


  opsidata.saveOpsiConf;

  showErrorMessages := False;

  OpsiData.finishOpsiconf;

  TerminateApp;
end;


procedure writeLogFileOptions(const RegHive: string; const info: string);
{$IFDEF WINDOWS}
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
begin

end;
{$ENDIF WINDOWS}

{$IFDEF LINUX}
function freeLinuxAgentStart : boolean;
var
  startkey : string;
  startcounter : integer;
  conffile : TIniFile;
  list : TXStringlist;
begin
  result := false;
  list := TXStringlist.Create;
  if FileExists('/etc/opsi-client-agent/opsi-script.conf') then
  begin
    conffile := TIniFile.Create('/etc/opsi-client-agent/opsi-script.conf');
    startkey := conffile.ReadString('general','start','void');
    if startkey <> 'void' then
    begin
      startkey := decryptStringBlow(opsiservicepassword,startkey);
      try
        stringsplitByWhiteSpace(startkey,list);
        if trim(list.Strings[0]) = opsiservicepassword then
        begin
          if TryStrToInt(trim(list.Strings[1]),startcounter) then
          begin
            if startcounter > 0 then
            begin
              result := true;
              dec(startcounter);
              logdatei.log('Using free Linux Agent start. '+IntTostr(startcounter)+' remaining',LLNotice);
              startkey :=  opsiservicepassword+' '+IntTostr(startcounter);
              startkey :=  encryptStringBlow(opsiservicepassword,startkey);
              conffile.WriteString('general','start',startkey);
            end
            else logdatei.log('No free Linux Agent start',LLDebug2);
          end
          else logdatei.log('Error: Startkey has no valid integer: '+trim(list.Strings[1]),LLError);
        end
        else logdatei.log('Error: Startkey has wrong signature.',LLError);
      except
       on ex: Exception
       do
       begin
         logdatei.log('Error: Exception in osmain:freeLinuxAgentStart : '+ex.message,LLError);
         result := false;
       end;
      end;
    end
    else logdatei.log('No valid start enty in /etc/opsi-client-agent/opsi-script.conf found ',LLWarning);
    conffile.Free;
  end
  else logdatei.log('No /etc/opsi-client-agent/opsi-script.conf found ',LLWarning);
  list.Free;
end;

{$ENDIF LINUX}

procedure StartProgramModes;

var
  ///  i :   Integer;
  Fehler: string;
  TheExitMode: TExitMode;
  extremeErrorLevel: TErrorLevel;
  omc: TOpsiMethodCall;
  errorOccured: boolean;
  ipAddress, ipName: string;
  scriptindex: integer;
  path: string;
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
      LogDateiName := LogPath + logdatei.StandardLogFilename + logdatei.StandardLogFileext;
    //writeln('StartProgramModes3');
    Logdatei.log('opsi-script ' + winstversion + ' started at ' + starttimestr, LLessential);
    Logdatei.log('opsi-script log file with encoding ' + DefaultEncoding, LLessential);
    //writeln('StartProgramModes4');
    {$IFDEF GUI}
    //FBatchOberflaeche.setVisible(false);
    FBatchOberflaeche.setWindowState(BatchWindowMode);
    //writeln('StartProgramModes5');
    {$ENDIF GUI}
    LogDatei.log('pm: ' + IntToStr(Ord(ProgramMode)), LLessential);
    startupmessages.Append('pm: ' + IntToStr(Ord(ProgramMode))+' '+ DateTimeToStr(Now));
    sessionid := '';

    if ProgramMode = pmInfo then
    begin
      {$IFDEF GUI}
      FBatchOberflaeche.setWindowState(bwmIcon);
      FBatchOberflaeche.setVisible(True);
      CentralFormVisible := True;
      CentralForm.Visible := CentralFormVisible;



      MyMessageDlg.WiMessageSized('command line options are' + LineEnding +
        '	 ' + ParamDelim + '?' + LineEnding +
        '	 ' + ParamDelim + 'h[elp]' + LineEnding +
        //'	 ' + ParamDelim + 'pcprofil  [PCProfileFile  [[' + ParamDelim + 'logfile] Logfile ] ] [' + ParamDelim + 'parameter ParameterString]' + LineEnding +
        '	 ' + ParamDelim + 'opsiservice <OpsiServiceUrl> '
             + ParamDelim + 'clientid <clientid> '
             + ParamDelim + 'credentialfile <filename>'
             + ParamDelim + 'username <username> '
             + ParamDelim + 'password <password> ['
             + ParamDelim + 'sessionid <sessionid> ] ['
             + ParamDelim + 'usercontext <usercontext> ] ['
             + ParamDelim + 'productlist <productlist> | '
             + ParamDelim + 'loginscripts | '
             + ParamDelim + 'allloginscripts ] ['
             + ParamDelim + 'silent ]' +
        '	 [<Scriptfile>  ['
             + ParamDelim + 'logfile <LogFile>] ['
             + ParamDelim + 'productid <productid> ] ['
             + ParamDelim + '[batch|silent] |histolist Inifilepath ] ['
             + ParamDelim + 'parameter ParameterString]' + LineEnding +
        '	 Scriptfile[;Scriptfile]*  ['
             + ParamDelim + 'logfile LogFile] ['
             + ParamDelim + 'lang langcode] ['
             + ParamDelim + '[batch|silent]] ['
             + ParamDelim + 'productid] ['
             + ParamDelim + 'parameter ParameterString]',
        [mrOk],
        650, 250);
      {$ELSE GUI}
      //nogui
      writeln(ExtractFileName(ParamStr(0)) + ' Version: '+WinstVersion);
      writeln('command line options are' + LineEnding +
        '' + ParamDelim + '? |' + ParamDelim + 'h[elp]' + LineEnding +
        //'	 ' + ParamDelim + 'pcprofil  [PCProfileFile  [[' + ParamDelim + 'logfile] Logfile ] ] [' + ParamDelim + 'parameter ParameterString]' + LineEnding +
        '' + ParamDelim + 'opsiservice <OpsiServiceUrl> '
             + ParamDelim + 'clientid <clientid> '
             + ParamDelim + 'credentialfile <filename>'
             + ParamDelim + 'username <username> '
             + ParamDelim + 'password <password> ['
             + ParamDelim + 'sessionid <sessionid> ] ['
             + ParamDelim + 'usercontext <usercontext> ] ['
             + ParamDelim + 'productlist <productlist> | '
             + ParamDelim + 'loginscripts | '
             + ParamDelim + 'allloginscripts ] ['
             + ParamDelim + 'silent ]'  + LineEnding+
        ' Scriptfile  ['
             + ParamDelim + 'logfile <LogFile>] ['
             + ParamDelim + 'productid <productid> ] ['
             + ParamDelim + '[batch|silent] | histolist Inifilepath ] ['
             + ParamDelim + 'parameter ParameterString]' + LineEnding +
        ' Scriptfile[;Scriptfile]*  ['
             + ParamDelim + 'logfile LogFile] ['
             + ParamDelim + '[batch|silent]] ['
             + ParamDelim + 'productid] ['
             + ParamDelim + 'parameter ParameterString]');
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
          errorOccured := false;
          {$IFDEF WINDOWS} SystemCritical.IsCritical := true; {$ENDIF WINDOWS}
          // if necessary do product installations
          {$IFDEF GUI}
          if runSilent then
            FBatchOberflaeche.setVisible(False)
          else
            FBatchOberflaeche.setVisible(True);
          {$ENDIF GUI}
          startupmessages.Append('startmessage start opsi service connection: ' +
            DateTimeToStr(Now));
          //opsiServiceVersion := getOpsiServiceVersion(opsiserviceurl, opsiserviceUser, opsiservicePassword, sessionid);
          //startupmessages.Append('startmessage opsi service version is : '+opsiServiceVersion+' at '+DateTimeToStr(Now));
          opsiServiceVersion := '4';
          if opsiServiceVersion = '4' then
          begin
            //LogDatei.DependentAdd ('Initializing opsi service version: '+opsiServiceVersion, LLessential);
            opsidata := TOpsi4Data.Create;
            opsidata.initOpsiConf(opsiserviceurl,
              opsiserviceUser,
              opsiservicePassword,
              opsiserviceSessionId);
            startupmessages.Append('startmessage: opsidata initialized: ' + DateTimeToStr(Now));
            //OpsiData.setOptions (opsiclientd_serviceoptions);
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
          on e: exception do
          Begin
            //LogDatei.log('Exception in StartProgramModes: pmBuildPC_service' + e.message, LLError);
            startupmessages.Append('Exception in StartProgramModes: pmBuildPC_service'
                           + e.message +' '+ DateTimeToStr(Now));
            errorOccured := true;
          End;
        end;


        if errorOccured then
        begin
          FindLocalIPData(ipName, ipAddress);
          LogPath := StandardLogPath;
          startupmessages.Append('startmessage create log: ' + DateTimeToStr(Now));
          LogDatei.CreateTheLogfile(LogDateiName);
          LogDatei.log('opsi-script cannot connect to service with URL: ' +
            opsiserviceurl +' with user ' + opsiserviceUser +
            '  The message is: >' + testresult + '< - Aborting ', LLcritical);

          terminateApp;
        end

        else
        begin
          if runloginscripts then
          begin
            //ForceDirectories('c:\opsi.org\log');
            LogPath := StandardLogPath;
            LogDateiName := LogPath + getLoggedInUser + '_login.log';
            startupmessages.Append('startmessage create log: ' + DateTimeToStr(Now));
            LogDatei.CreateTheLogfile(LogDateiName, False);
          end
          else
          begin
            LogDateiName := OpsiData.getLogFileName(LogDateiName);
            startupmessages.Append('startmessage create log: ' + DateTimeToStr(Now));
            LogDatei.CreateTheLogfile(LogDateiName);
          end;
          extractTmpPathFromLogdatei(LogDateiName);
          TempPath := GetTempPath;

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
              MyMessageDlg.WiMessage('Use of opsi Linux Client Agent Extension is not activated ' + LineEnding + 'Terminating Program', [mrOk]);
              {$ENDIF GUI}
              LogDatei.DependentAdd('Terminating Program', LLerror);
              TerminateApp;
            end;
          end;
          {$ENDIF LINUX}


          if runloginscripts then
          begin
            if pos('opsisetupadmin',LowerCase(usercontext)) = 0 then LoginScripts
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
          {$IFDEF WINDOWS} SystemCritical.IsCritical := True; {$ENDIF WINDOWS}
          //FBatchOberflaeche.BorderIcons := [];
          {$IFDEF GUI}
          if ProgramMode = pmBatch then
            FBatchOberflaeche.setVisible(True)

          else if ProgramMode = pmSilent then
            FBatchOberflaeche.setVisible(False);

          centralform.Edit1.Text := scriptlist.strings[0];
          centralform.Edit2.Text := LogDateiName;

          DontUpdateMemo := True;
          {$ENDIF GUI}
          oslog.StandardPartLogPath:= ExtractFileDir(Logdateiname);
          LogDatei.CreateTheLogFile(Logdateiname, False);
          extractTmpPathFromLogdatei(LogDateiName);
          TempPath := GetTempPath;
          extremeErrorLevel := Level_not_initialized;
          LogDatei.LogProduktId:=false;

          // Are we in batch with /productid (opsi-template-with-admin) ?
          // open service connection if possible
          if not (batchproductid = '') then
          begin
            try
              {$IFDEF WINDOWS}
              opsiclientdconf := getSpecialFolder(CSIDL_PROGRAM_FILES) +
                '\opsi.org\opsi-client-agent\opsiclientd\opsiclientd.conf';
              {$ENDIF WINDOWS}
              {$IFDEF LINUX}
              opsiclientdconf := '/etc/opsi-client-agent/opsiclientd/opsiclientd.conf';
              {$ENDIF LINUX}
              if FileExists(opsiclientdconf) then
              begin
                myconf := TInifile.Create(opsiclientdconf);
                opsiservicePassword := myconf.ReadString('global', 'opsi_host_key', '');
                opsiserviceUser := myconf.ReadString('global', 'host_id', '');
                opsiserviceurl := myconf.ReadString('config_service', 'url', '');
                myconf.Free;
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
                      startTime := now;
                      omc := TOpsiMethodCall.Create('backend_info', []);
                      testresult := opsidata.CheckAndRetrieve(omc, errorOccured);
                      LogDatei.DependentAdd('JSON Bench for ' + omc.OpsiMethodName +
                        ' ' + copy(omc.getJsonUrlString, pos(',', omc.getJsonUrlString) + 1,
                        50) + ' Start: ' + FormatDateTime('hh:nn:ss:zzz', startTime) +
                        ' Time: ' + FormatDateTime('hh:nn:ss:zzz', now - startTime), LLinfo);
                      omc.Free;
                      opsidata.setActualProductName(batchproductid);
                      opsidata.setActualClient(opsiserviceUser);
                      ProductvarsForPC := opsidata.getProductproperties;
                      if not opsidata.initProduct then
                      begin
                        extremeErrorLevel := levelFatal;
                        LogDatei.log('Could not connect to Service !',LLCritical);
                      end;
                    end
                    else
                    begin
                      extremeErrorLevel := levelFatal;
                      LogDatei.log('Could not connect to Service !',LLCritical);
                    end;
                  except
                    on e: exception do
                    Begin
                      LogDatei.log('Exception in StartProgramModes: [pmBatch, pmSilent]: opsidata.create' + e.message, LLCritical);
                      {$IFDEF WINDOWS} SystemCritical.IsCritical := false; {$ENDIF WINDOWS}
                      extremeErrorLevel := levelFatal;
                    End;
                  end
                end
                else
                begin
                  LogDatei.log('Data missing in opsiclientd.conf: No Service connection', LLCritical);
                  extremeErrorLevel := levelFatal;
                end;
              end
              else
              begin
                LogDatei.DependentAdd('No opsiclientd.conf found: No Service connection', LLCritical);
                extremeErrorLevel := levelFatal;
              end;
            except
              on e: exception do
              Begin
                LogDatei.log('Exception in StartProgramModes: [pmBatch, pmSilent]: read opsiclientd.conf' + e.message, LLCritical);
                {$IFDEF WINDOWS} SystemCritical.IsCritical := false; {$ENDIF WINDOWS}
                extremeErrorLevel := levelFatal;
              End;
            end;
          end;


        if  extremeErrorLevel <> levelFatal then
        begin
          for scriptindex := 0 to scriptlist.Count - 1 do
          begin
            NestingLevel := 0;
            CreateAndProcessScript(scriptlist.Strings[scriptindex],
              NestingLevel, False, extremeErrorLevel);
          end;

          if not (opsidata = nil) then
          begin
            logdatei.Appendmode := True;
            opsidata.finishOpsiConf;
          end;
        end;

          // Are we in batch with /productid (opsi-template-with-admin) ?
          if runningAsAdmin and (not (batchproductid = '')) then
          begin
            try
              // write isFatal (or not) to registry
              //LogDatei.DependentAdd('extremeErrorLevel is : '+IntToStr(extremeErrorLevel), LLDebug2);
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
            except
              on e: exception do
              Begin
                LogDatei.DependentAddError ('exception in StartProgramModes: [pmBatch, pmSilent]: reg.Write: with-admin-fatal ' + e.message, LLError);
                {$IFDEF WINDOWS} SystemCritical.IsCritical := false; {$ENDIF WINDOWS}
              End;
            end;
          end;


          try
            {$IFDEF WINDOWS}writeLogFileOptions(WinstRegHive, Logdateiname);{$ENDIF}
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
              MyMessageDlg.WiMessage('ExitWindows Fehler ' + LineEnding + Fehler, [mrOk]);
              {$ELSE GUI}
              writeln('ExitWindows Fehler ' + LineEnding + Fehler);
              {$ENDIF GUI}
          end;

          TerminateApp;
        except
          on e: exception do
          Begin
            LogDatei.DependentAddError ('exception in StartProgramModes: [pmBatch, pmSilent]: ' + e.message, LLError);
            {$IFDEF WINDOWS} SystemCritical.IsCritical := false; {$ENDIF WINDOWS}
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
            FBatchOberflaeche.setVisible(True);
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

            Edit2.Text := LogDateiName;
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
    on e: exception do
    Begin
      LogDatei.log('Exception in StartProgramModes: General' + e.message, LLError);
    End;
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
      TempPath := ValueOfEnvVar('TEMP');

    testpassed := False;
    if (StandardMainLogPath <> '') and SysUtils.ForceDirectories(StandardMainLogPath) then
    begin
      testpassed := True;
      teststr := StandardMainLogPath + 'testing_write_privilege_for_winst';
      if not fileexists(teststr) then
      begin
        if not SysUtils.ForceDirectories(teststr) or not RemoveDir(teststr) then
          testpassed := False;
      end;
      teststr := '';
    end;

    if testpassed then
      LogPath := StandardMainLogPath
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

  (*
   r := CutLeftBlanks (CmdLine);
   while r <> ''
   do
   Begin
     GetWord (r, Parameter, r, [' ']);
     ParamListe.Add (Parameter);
   End;
   *)


    for i := 1 to ParamCount do
    begin
      teststr := ParamStr(i);
      ParamListe.Add(ParamStr(i));
    end;

    (* if paramcount > 0  *)
    if ParamListe.Count > 0 then
    begin
      (* Parameter := ParamStr (1); *)
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
            if ProgramMode <> pmNotSet then
            begin
              ProgramMode := pmInfo;
              exit;
            end
            else if NumberOfTrueParameters >= 1 then
              (* pmBuildPC geht nicht mehr *)
            begin
              ProgramMode := pmInfo;
              exit;
            end
            else
            begin
              try
                ProgramMode := pmBuildPC_service;

                computername := '';
                if not readconfig then
                  startupmessages.Append('Error: readconfig failed: Working with defaults '+ DateTimeToStr(Now));
                startupmessages.Append('Depot path from readconfig:  ' + depotdrive + depotdir+' '+ DateTimeToStr(Now));
                Inc(i);
                if i <= ParamListe.Count then
                begin
                  opsiserviceURL := ParamListe.Strings[i - 1];
                  NumberOfTrueParameters := 1;
                  Inc(i);
                  if '/rpc' = lowercase(rightstr(opsiserviceURL, 4)) then
                    opsiserviceURL := LeftStr(opsiserviceURL, length(opsiserviceURL) - 4);
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
                on e: exception do
                Begin
                  ProgramMode := pmInfo;
                  startupmessages.Append('Exception in GetParameter: opsiserviceURL'
                           + e.message +' '+ DateTimeToStr(Now));
                  //LogDatei.log('Exception in GetParameter: opsiserviceURL' + e.message, LLError);
                End;
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
                    computername := ParamListe.Strings[i - 1];
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
                    opsiserviceUser :=
                      ParamListe.Strings[i - 1];
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
                  //stringsplit(parameter, ';', scriptlist);
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
                      runproductlist := True;
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
            on e: exception do
            Begin
              ProgramMode := pmInfo;
              //LogDatei.log('Exception in GetParameter: opsiservice' + e.message, LLError);
              startupmessages.Append('Exception in GetParameter: opsiservice'
                           + e.message +' '+ DateTimeToStr(Now));
            End;
          end;
        end


        else if Lowercase(Parameter) = 'pcprofil' then
        begin
          if ProgramMode <> pmNotSet then
          begin
            ProgramMode := pmInfo;
            exit;
          end
          else if NumberOfTrueParameters >= 1 then
            (* pmBuildPC geht nicht mehr *)
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

        else if Lowercase(Parameter) = 'batch' then
        begin
          ProgramMode := pmBatch;
          {$IFDEF GUI}
          BatchWindowMode := bwmNormalWindow;
          SavedBatchWindowMode := BatchWindowMode;
          (* Test  FBatchOberflaeche.ForceStayOnTop (true); *)
          {$ENDIF GUI}
          Inc(i);
        end

        else if Lowercase(Parameter) = 'silent' then
        begin
          ProgramMode := pmSilent;
          (* Test  FBatchOberflaeche.ForceStayOnTop (true); *)
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
    on e: exception do
    Begin
      ProgramMode := pmInfo;
      //LogDatei.log('Exception in GetParameter: General' + e.message, LLError);
      startupmessages.Append('Exception in GetParameter: General'
                           + e.message +' '+ DateTimeToStr(Now));
    End;
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
  startupmessages := TStringList.Create;
  startupmessages.Append('startmessage opsi-script created at main: ' +
    DateTimeToStr(Now));
  toggle := True;
  initEncoding;
  ProductvarsForPC := TStringList.Create;
  ProductvarsForPC.Clear;

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
      writeln('TCentralForm.FormCreate : ' + E.Message);
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

