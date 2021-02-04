
unit osinteractivegui;

{$MODE DELPHI}
//{$RANGECHECKS ON}
{$OVERFLOWCHECKS ON}
{$IOCHECKS ON}
{$OBJECTCHECKS ON}
{$VARSTRINGCHECKS ON}
{$LONGSTRINGS ON}



{$IFDEF DARWIN}
{$modeswitch objectivec1}
{$ENDIF DARWIN}



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




interface

uses
{$IFDEF WINDOWS}
  Windows,
  VersionInfoX,
  registry,
  osregistry,
{$IFDEF WIN32}
  osfuncwin2,
  osfuncwin3,
//osshowlog {ShowTextFile},
{$ENDIF WIN32}
  shellapi,
  wispecfolder,
{$ENDIF WINDOWS}
{$IFDEF LINUX}
  osfunclin,
  lispecfolder,
  baseunix,
{$ENDIF LINUX}
{$IFDEF DARWIN}
  macosall,
  CocoaAll,   // Needed for NSBundle
  CocoaUtils, // Needed for NSStringToString
{$ENDIF DARWIN}
  osencoding,
  osconf,
  osmain,
  FileUtil,
  LCLIntf,
  LResources,
  SysUtils,
  Classes,
  synautil,
  osbatchgui,
  osmessagedialog,
  oswebservice,
  Forms, Dialogs,
  ExtCtrls,
  StdCtrls,
  Controls,
  //wirequlist,
  oslog, osparser, osfunc,
ostxstringlist,
  Menus, Buttons, ComCtrls,
  //IdSysLog,
  lcltranslator,
  strutils,
  inifiles,
  osGUIControl,
  osSimpleWinBatchGUI;

type

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
    procedure FormWindowStateChange(Sender: TObject);
    procedure Label3Click(Sender: TObject);
    procedure Memo1Add(const s: string);
    procedure FormCreate(Sender: TObject);
    procedure Button_TerminateClick(Sender: TObject);
    procedure Button_ExecuteClick(Sender: TObject);


(*
    procedure ProcessProdukt(var extremeErrorLevel: TErrorLevel);


    procedure BuildPC;
    procedure LoginScripts;
    procedure Productlist;
    procedure StartProgramModes;
    procedure GetParameter;

    procedure LZH1Progress(Sender: TObject; const FileName: string;
      const FileBytesWritten, FileBytes, TotalBytesWritten, TotalBytes: longint;
      var Cancel: boolean);
*)
    procedure getScriptName;
    procedure TerminateInteractive;
    procedure TakeToSaveList(FName: string);
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

procedure CreateSystemInfo;
procedure ProcessMess;
(*
procedure ProcessMess;
procedure TerminateApp;
procedure CreateSystemInfo;
function ValueOfEnvVar(const VarName: string): string;
procedure saveVersionToProfile;
function readVersionFromProfile: string;
function scriptWasExecutedBefore: boolean;

*)


var

  CentralForm: TCentralForm;
  CentralFormVisible: boolean;
  Skriptdatei: string;
  scriptlist: TXStringList;
  usercontext: string;
  batchproductid: string = '';  //id of product we are running in batch mode
  logproductid: string = '';  //id of product used for logging in batch mode without service
  usercontextSID: string;
  usercontextUser: string;
  usercontextDomain: string;
  NestingLevel: integer;
  ExtraParameter: string;

  //ProductvarsForPC: TStringList;


  ShellIsExplorer: boolean;

  ParamFileLocalization: string;

  testresult: string;
  teststringlist: TStringList;
  //startupmessages: TStringList;


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

uses osshowsysinfo
{$IFDEF WIN32}
//, osshowlog
{$ENDIF}
;

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

(*
type
  TProgramMode = (pmNotSet, pmInfo, pmStandard, pmHistoDialog, pmBuildPC_classic,
    pmBuildPC_service, pmBatch, pmSilent);
 *)
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

(*
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
      Result := False;
    end;
  end;
{$ENDIF}
end;


procedure getBootmode(var bootmode: string; var fromRegistry: boolean);
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
  Application.ProcessMessages;
  //Sleep(10);
end;

procedure CreateSystemInfo;
begin
  SystemInfo := TSystemInfo.Create(Application);
end;

procedure TerminateApp;
begin
  try
    if LogDatei <> nil then
    begin
      LogDatei.LogSIndentLevel := 0;
      LogDatei.DependentAdd('============  opsi-script ' + winstversionname +
        ' is regularly exiting. Time ' + FormatDateTime(
        'yyyy-mm-dd  hh:mm:ss ', now) + '.', LLessential);

      sleep(1000);
      LogDatei.Free;
      LogDatei := nil;
    end;
    try
      Application.Terminate;
    except
      // test
      Halt;
    end;
  except
    try
      Application.Terminate;
    except
      // test
      Halt;
    end;
  end;
end;


function ValueOfEnvVar(const VarName: string): string;
begin
  //{$IFDEF WINDOWS}
  Result := GetEnvironmentVariable(VarName);

  if Result = '' then
    Result := valueEnvVarNotFound;
end;




procedure extractTmpPathFromLogdatei(var Logdateiname: string);
begin
  TmpPathFromLogdatei := ExtractFilePath(ExpandFilename(LogDateiname));
end;

*)

procedure ProcessMess;
begin
  Application.ProcessMessages;
  //Sleep(10);
end;


procedure CreateSystemInfo;
begin
  SystemInfo := TSystemInfo.Create(Application);
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

function GetGUITheme(PathToSkinFile: string): string;
var
  ThemeFile: TIniFile;
begin
  //PathToThemeFile := path + PathDelim + 'theme.ini';
  ThemeFile := TIniFile.Create(PathToSkinFile);
  Result := ThemeFile.ReadString('Window', 'Theme' , 'Default');
  ThemeFile.Free;
end;

{ TCentralForm }

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


procedure TCentralForm.TakeToSaveList(FName: string);

var
  i: integer;
begin
  FName := ExpandFileName(FName);
  i := ComboBox1.Items.IndexOf(FName);
  if i > 0 then
  begin
    ComboBox1.Items.Delete(i);
    ComboBox1.Items.Insert(0, FName);
    (* ComboBox1.Items.Exchange (0, i) *)
  end
  else if i < 0 (* nicht vorhanden *) then
  begin
    if ComboBox1.Items.Count = MaxSavedScriptFiles then
      ComboBox1.Items.Delete(MaxSavedScriptFiles - 1);
    ComboBox1.Items.Insert(0, FName);
  end;
  ComboBox1.Text := FName;

  //unerklaerlich, aber sonst wird der String leer, wenn ein Exchange stattgefunden hat
  ComboBox1.Items.SaveToFile(IniFileLocalization);
end;


procedure TCentralForm.Memo1Add(const s: string);

var
  i: integer;
  memo1count: integer;
begin
  if CentralFormVisible then
  begin

    memo1count := Memo1.Lines.Count;

    try
      if memo1Count > maxvisualmemostrings then
      begin
        for i := 2 to maxvisualmemostrings do
          Memo1.Lines.Delete(3);

        Memo1.Lines.insert(3, '  ...   ');
        Memo1.Lines.insert(3, '');
      end;

      Memo1.Lines.Add(s);

    except
    end;
  end;
end;

procedure TCentralForm.Label3Click(Sender: TObject);
begin

end;

procedure TCentralForm.FormWindowStateChange(Sender: TObject);
begin

end;


procedure TCentralForm.TerminateInteractive;
//{$IFDEF MSWINDOWS}

var
  Hinweis, Fehler: string;
  TheExitMode: TExitMode;
  //  rkey : HKEY;

begin
  if ProgramMode = pmHistoDialog then
    (* Abspeichern der Skriptdatei in ComboBox-Liste *)
    TakeToSaveList(ComboBox1.Text);

  try
    {$IFDEF WINDOWS}writeLogFileOptions(WinstRegHive, Edit2.Text);{$ENDIF}
  except
     { try
      writeLogFileOptions ('HKCU', Edit2.Text)
    except
    end; }
  end;

  if PerformExitWindows <> txrNoExit then
  begin
    case PerformExitWindows of
      txrRegisterForLogout, txrImmediateLogout: Hinweis :=
          'Windows verlassen und User abmelden?';
      txrRegisterForReboot,
      txrReboot: Hinweis := 'Windows verlassen und System rebooten?';
      txrImmediateReboot:
        Hinweis := 'Windows jetzt verlassen und rebooten?' + LineEnding +
           '(Im Anschluss bitte DASSELBE Skript nochmal starten!)';
    end;

    if MyMessageDlg.WiMessage(Hinweis, [mrYes, mrNo]) = mrYes then
    begin
      case PerformExitWindows of
        txrRegisterForReboot,
        txrReboot, txrImmediateReboot: TheExitMode := txmReboot;
        txrRegisterforLogout, txrImmediateLogout: TheExitMode := txmLogout;
      end;

      if not ExitSession(TheExitMode, Fehler) then
        MyMessageDlg.WiMessage('Exit Windows Error ' + LineEnding + Fehler, [mrOk]);
    end
    else
      PerformExitWindows := txrNoExit;
  end;
  //Logdatei.DeletePartLog;
  TerminateApp;
end;

procedure TCentralForm.Button_TerminateClick(Sender: TObject);
begin
  TerminateInteractive;
end;

procedure TCentralForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TerminateInteractive;
end;


procedure TCentralForm.Button_ExecuteClick(Sender: TObject);

var
  dummyErrorLevel: TErrorLevel;
begin
  //FBatchOberflaeche.Show;
  //ShowWindow(HWND(FBatchOberflaeche.Handle), SW_SHOW);
  //ProcessMess;
  if ProgramMode = pmHistoDialog then
    SkriptDatei := ComboBox1.Text
  else
    Skriptdatei := Edit1.Text;


  LogDateiName := Edit2.Text;
  Memo1.Lines.Clear;
  CentralForm.Refresh;

  if ProgramMode = pmHistoDialog then
    (* Abspeichern der Skriptdatei in ComboBox-Liste *)
    TakeToSaveList(ComboBox1.Text);

  //oslog.StandardPartLogPath:= ExtractFileDir(Logdateiname);
  if RadioButtonNewLogFile.Checked then
  begin
    //MakeBakFile(LogDateiName,8);
    //if Logdatei <> nil then
    //Logdatei.Free;
    if Logdatei = nil then
      Logdatei := TLogInfo.Create;
    Logdatei.StandardPartLogPath:= ExtractFileDir(Logdateiname);
    if LogDateiName = '' then
      LogDateiName := LogPath + logdatei.StandardLogFilename + logdatei.StandardLogFileext;
    LogDatei.CreateTheLogfile(LogDateiName, False);
  end
  else
  begin
    Logdatei.StandardPartLogPath:= ExtractFileDir(Logdateiname);
    LogDatei.initiate(LogDateiName, False);
    LogDatei.DependentAdd('', LLessential);
    LogDatei.DependentAdd('', LLessential);
    Logdatei.DependentAdd('opsi-script ' + OpsiscriptVersion + ' started at ' + starttimestr,
      LLessential);
    Logdatei.log('opsi-script log file with encoding ' + DefaultEncoding, LLessential);
    LogDatei.DependentAdd('======= APPEND   ' + DateTimeToStr(Now), LLessential);
  end;

  NestingLevel := 0;
  CreateAndProcessScript(SkriptDatei, NestingLevel, False, dummyErrorLevel);
  LogDatei.Close;
end;

procedure TCentralForm.ButtonProcessMemoClick(Sender: TObject);

var
  dummyErrorLevel: TErrorLevel;
begin
  Skriptdatei := '';

  LogDateiName := Edit2.Text;
  LogDatei.StandardPartLogPath:= ExtractFileDir(Logdateiname);
  if RadioButtonNewLogFile.Checked then
  begin
    LogDatei.CreateTheLogfile(LogDateiName, False);
  end
  else
    LogDatei.initiate(LogDateiName, False);

  NestingLevel := 0;
  CreateAndProcessScript(SkriptDatei, NestingLevel, False, dummyErrorLevel);

end;

procedure TCentralForm.ButtonGetClipboardTextClick(Sender: TObject);
begin
  Memo1.Clear;
  Memo1.PasteFromClipboard;
end;

(*
procedure TCentralForm.LZH1Progress(Sender: TObject; const FileName: string;
  const FileBytesWritten, FileBytes, TotalBytesWritten, TotalBytes: longint;
  var Cancel: boolean);
begin
  if (zipfiles.items.Count > 0) and (zipfiles.items[zipfiles.items.Count - 1] =
    fileName) then
  begin
    // nothing
  end
  else
    zipfiles.items.add(fileName);
end;
 *)

procedure TCentralForm.FormCreate(Sender: TObject);

var
  ErrorInfo: string;
  str: string;
  s2, s3, s4, sid: string;
  lang,localedir : string;
  Theme: string;

begin
  //writeln('TCentralForm.FormCreate');
  starttimestr := DateTimeToStr(Now);
  startupmessages := TStringList.Create;
  startupmessages.Append('startmessage opsi-script created at CentralForm.FormCreate: ' +
    DateTimeToStr(Now));
  toggle := True;
  Memo1.Clear;
  Memo1.Lines.add('');
  Memo1.Lines.add('--------------- ' + OpsiscriptVersionName +
    ' ----------------------');
  initEncoding;
  ProductvarsForPC := TStringList.Create;
  ProductvarsForPC.Clear;

  {$IFDEF WINDOWS}
  (*
  if FileExists(GetWinSystemDirectory + 'w32hupsi.dll') then
    // very old version
  begin
    WinstRegKey := veryOldWinstRegKey;
  end
  else
  *)
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
  {$IFDEF DARWIN}
  // set locale path to the resource/locale dir of the .app bundle
  localedir := NSStringToString(NSBundle.mainBundle.resourcePath)
    + PathDelim+ 'locale' + PathDelim;
  //lang := GetDefaultLang;
  lang := '';
  SetDefaultLang(lang,localedir);
  {$ENDIF DARWIN}


  CentralFormVisible := False;
  CentralForm.Visible := CentralFormVisible;
  CentralForm.Label1.Caption := '';
  CentralForm.Label2.Caption := '';
  LabelWinstVersion.Caption := OpsiscriptVersionName;

  Theme := LowerCase(TGUIControl.GetGUITheme(''));
  { Include new themes in the if .. then block if available }
  if  (Theme = LowerCase('Default')) or (Theme = LowerCase('BatchOberflaeche')) then
  begin
    FBatchOberflaeche := TFBatchOberflaeche.Create(Application);
  end
  else
  if (Theme = LowerCase('WindowsSimple')) then
  begin
    FBatchOberflaeche := TSimpleWinBatchGUI.Create(Application);
  end
  else FBatchOberflaeche := TFBatchOberflaeche.Create(Application);

  ProcessMess;
  MyMessageDLG := TMyMessageDLG.Create(Application);


  FBatchOberflaeche.SetForceStayOnTop(False);

  FBatchOberflaeche.SetMessageText(OpsiscriptVersionName, mVersion);//FBatchOberflaeche.setVersionLabel(OpsiscriptVersionName);

  try

    osmain.GetParameter;
    osmain.StartProgramModes;
  except
    on E: Exception do
    begin
      FBatchOberflaeche.SetForceStayOnTop(False);
      MyMessageDlg.WiMessage('TCentralForm.FormCreate : ' + E.Message, [mrOk]);
    end;
  end;
end;


procedure TCentralForm.getScriptName;
begin
  if OpenDialog1.Execute then
    if ProgramMode = pmHistoDialog then
      ComboBox1.Text := OpenDialog1.FileName
    else
      Edit1.Text := OpenDialog1.FileName;
end;


procedure TCentralForm.Button_BrowseClick(Sender: TObject);
begin
  getScriptName;
end;

procedure TCentralForm.Beenden1Click(Sender: TObject);
begin
  TerminateInteractive;
end;

procedure TCentralForm.Info1Click(Sender: TObject);
begin

  MyMessageDlg.WiMessage('opsi-script  ' + OpsiscriptVersionName + LineEnding +
    'Copyright (c) uib 1995 - 2016' + LineEnding +
    'opsi-script is AGPLv3 licensed Open Source software' + LineEnding +
    'Detected Language: '+ GetDefaultLang + LineEnding +
    'Internet homes: ' + LineEnding + ' www.uib.de  ' + ' www.opsi.org  ',
    [mrOk]);
end;

procedure TCentralForm.Skript1Click(Sender: TObject);
begin
  getScriptName;
end;

procedure TCentralForm.BitBtnViewLogfileClick(Sender: TObject);
begin
  ShowMessage('Logview is temporary not working. Please use the opsi-logviewer product.');
  {$IFDEF WIN32}
  (*
  ShowTextFile.lzRichEdit1.Clear;
  //ShowTextFile.RichEdit1.Clear;
  ShowTextFile.Visible := True;
  if FileExists(LogDateiName) then
  begin
    ShowTextFile.reloadfromfile;
    ShowTextFile.setcolor;
  end;
  {$IFDEF WINDOWS}ShowWindow(ShowTextFile.handle, SW_RESTORE);{$ENDIF}
  *)
  {$ENDIF WIN32}
end;


procedure TCentralForm.Button_show_guiClick(Sender: TObject);
begin
  {$IFDEF WINDOWS}ShowWindow(HWND(FBatchOberflaeche.Handle), SW_SHOW);{$ENDIF}
  FBatchOberflaeche.Visible:= true;
  toggle := False;
end;

procedure TCentralForm.SpeedButton4Click(Sender: TObject);
begin
  {$IFDEF WINDOWS}ShowWindow(HWND(FBatchOberflaeche.Handle), SW_Hide);{$ENDIF}
  FBatchOberflaeche.Visible:= false;
end;

procedure TCentralForm.Memo1Change(Sender: TObject);
begin
  if LabelActivity.Caption = '-' then
    LabelActivity.Caption := '|'
  else
    LabelActivity.Caption := '-';

end;


initialization
{$IFDEF FPC}
 {$i osinteractivegui.lrs}
{$ELSE}
{$ENDIF}

  opsiserviceSessionId := '';
end.

