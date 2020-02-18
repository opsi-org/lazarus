unit uibdata;

//{$mode objfpc}
{$mode delphi}
{$H+}

interface

uses
  fileinfo,
{$IFDEF LINUX}
  //, winpeimagereader {need this for reading exe info}
  elfreader, {needed for reading ELF executables}
  libnotify,
{$ENDIF LINUX}
  {$IFDEF WINDOWS}
  winpeimagereader, {need this for reading exe info}
  Windows,
  pingsend,
  {$ENDIF WINDOWS}
  Classes, SysUtils, IBConnection, sqldb, DB,
  //FileUtil,
  LazFileUtils,
  Menus, ExtCtrls,
  ///registry,
  Forms,
  Graphics,
  typinfo,
  Controls,
  Dialogs,
  IniFiles, process, DateUtils,
  runprocess,
  httpservice,
  uibtWorkRepChooser,
  uib2erp,
  Variants,
  maskedit, UniqueInstance,
  oslog;

type

  { TDataModule1 }

  TDataModule1 = class(TDataModule)
    DataSourceTmp: TDataSource;
    DSQueryAktEvents: TDataSource;
    DSuiballevent: TDataSource;
    DSuibevent: TDataSource;
    DSuibuserevent: TDataSource;
    DSuibloggedin: TDataSource;
    DSuibtimeout: TDataSource;
    DSuibcalls: TDataSource;
    DSuibsoll: TDataSource;
    DSquerytimeout: TDataSource;
    DSuibdefproj: TDataSource;
    DSuibaktuser: TDataSource;
    DSuibaktevent: TDataSource;
    DS_day_report: TDataSource;
    DS_work_description: TDataSource;
    DS_holydays: TDataSource;
    IBConnection1: TIBConnection;
    Dateneditieren1: TMenuItem;
    IBConnection2: TIBConnection;
    Arbeitsberichte: TMenuItem;
    ProcessTrayNotify: TProcess;
    SQholydays: TSQLQuery;
    SQLQueryTmp: TSQLQuery;
    TimerCheckNet: TTimer;
    trayconfig: TMenuItem;
    Query_day_report: TSQLQuery;
    Query4Result: TSQLQuery;
    TrayQuery4: TSQLQuery;
    TrayQuery3: TSQLQuery;
    TrayQuery2: TSQLQuery;
    TrayQuery1: TSQLQuery;
    SQLTransaction2: TSQLTransaction;
    SQqueryloggedin: TSQLQuery;
    SQwork_description: TSQLQuery;
    TimerTrayIcon: TTimer;
    TrayIcon1: TTrayIcon;
    UniqueInstance1: TUniqueInstance;
    ZeigenurmeineProjekte1: TMenuItem;
    schmaleLeiste: TMenuItem;
    LeisteNeuAufbauen1: TMenuItem;
    MenuItem13: TMenuItem;
    Hilfe1: TMenuItem;
    Info1: TMenuItem;
    ShowDebugWindow1: TMenuItem;
    uibtime2erp: TMenuItem;
    Weristda1: TMenuItem;
    Statistik1: TMenuItem;
    Import1: TMenuItem;
    Export1: TMenuItem;
    Datenweitergabe1: TMenuItem;
    MenuItem8: TMenuItem;
    Autologin1: TMenuItem;
    PopupMenu1: TPopupMenu;
    SQQueryAktEvents: TSQLQuery;
    SQuiballevent: TSQLQuery;
    SQuibevent: TSQLQuery;
    SQuibuserevent: TSQLQuery;
    SQuibloggedin: TSQLQuery;
    SQuibtimeout: TSQLQuery;
    SQQueryuibevent: TSQLQuery;
    SQuibsoll: TSQLQuery;
    SQquerytimeout: TSQLQuery;
    SQuibdefproj: TSQLQuery;
    SQuibaktuser: TSQLQuery;
    SQuibaktevent: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    TimerQueryLoggedIn: TTimer;
    Timerloggedin: TTimer;
    TimerOnTop: TTimer;
    TimerLogoffOnTop: TTimer;
    procedure ArbeitsberichteClick(Sender: TObject);
    procedure Autologin1Click(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure Dateneditieren1Click(Sender: TObject);
    procedure Dateneditieren1Cancel;
    procedure Datenweitergabe1Click(Sender: TObject);
    procedure Export1Click(Sender: TObject);
    procedure Hilfe1Click(Sender: TObject);
    procedure IBConnection1Log(Sender: TSQLConnection; EventType: TDBEventType;
      const Msg: string);
    procedure Import1Click(Sender: TObject);
    procedure Info1Click(Sender: TObject);
    procedure LeisteNeuAufbauen1Click(Sender: TObject);
    procedure PopupMenu1Close(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure queryAfterHelper(myevent: string; myquery: TSQLQuery; DataSet: TDataSet);
    procedure Query4ResultAfterDelete(DataSet: TDataSet);
    procedure Query4ResultAfterPost(DataSet: TDataSet);
    procedure schmaleLeisteClick(Sender: TObject);
    procedure ShowDebugWindow1Click(Sender: TObject);
    procedure SQholydaysAfterDelete(DataSet: TDataSet);
    procedure SQholydaysAfterInsert(DataSet: TDataSet);
    procedure SQholydaysAfterPost(DataSet: TDataSet);
    procedure SQuibakteventAfterDelete(DataSet: TDataSet);
    procedure SQuibakteventAfterEdit(DataSet: TDataSet);
    procedure SQuibakteventAfterInsert(DataSet: TDataSet);
    procedure SQuibakteventAfterPost(DataSet: TDataSet);
    procedure SQuibakteventBeforePost(DataSet: TDataSet);
    procedure SQuibaktuserAfterDelete(DataSet: TDataSet);
    procedure SQuibaktuserAfterPost(DataSet: TDataSet);
    procedure SQuiballeventAfterPost(DataSet: TDataSet);
    procedure SQuibdefprojAfterDelete(DataSet: TDataSet);
    procedure SQuibdefprojAfterPost(DataSet: TDataSet);
    procedure SQuibeventAfterDelete(DataSet: TDataSet);
    procedure SQuibeventAfterInsert(DataSet: TDataSet);
    procedure SQuibeventAfterPost(DataSet: TDataSet);
    procedure SQuibeventBeforeClose(DataSet: TDataSet);
    procedure SQuibeventBeforeDelete(DataSet: TDataSet);
    procedure SQuibeventBeforePost(DataSet: TDataSet);
    procedure SQuibeventPostError(DataSet: TDataSet; E: EDatabaseError;
      var DataAction: TDataAction);
    procedure SQuibloggedinAfterDelete(DataSet: TDataSet);
    procedure SQuibloggedinAfterPost(DataSet: TDataSet);
    procedure SQuibsollAfterDelete(DataSet: TDataSet);
    procedure SQuibsollAfterPost(DataSet: TDataSet);
    procedure SQuibtimeoutAfterDelete(DataSet: TDataSet);
    procedure SQuibtimeoutAfterPost(DataSet: TDataSet);
    procedure SQuibusereventAfterDelete(DataSet: TDataSet);
    procedure SQuibusereventAfterPost(DataSet: TDataSet);
    procedure SQwork_descriptionAfterDelete(DataSet: TDataSet);
    procedure SQwork_descriptionAfterInsert(DataSet: TDataSet);
    procedure SQwork_descriptionAfterPost(DataSet: TDataSet);
    procedure SQwork_descriptionPostError(DataSet: TDataSet;
      E: EDatabaseError; var DataAction: TDataAction);
    procedure Statistik1Click(Sender: TObject);
    procedure Statistik1Cancel;
    procedure TimerCheckNetTimer(Sender: TObject);
    procedure TimerloggedinTimer(Sender: TObject);
    procedure TimerLogoffOnTopTimer(Sender: TObject);
    procedure TimerOnTopTimer(Sender: TObject);
    procedure TimerQueryLoggedInTimer(Sender: TObject);
    procedure TimerTrayIconTimer(Sender: TObject);
    procedure trayconfigClick(Sender: TObject);
    procedure uibtime2erpClick(Sender: TObject);
    procedure Weristda1Click(Sender: TObject);
    procedure ZeigenurmeineProjekte1Click(Sender: TObject);

    procedure setTimerQueryLoggedIn(millis: integer; run: boolean);
    procedure getLoggedInList(userlist: TStrings; dobeep: boolean);

    function getVersion(): string;
    function getVerDatum(): string;
    function getLeftint(): integer;
    procedure setontopwidth(newvalue: integer);
    procedure setOntopheight(newvalue: integer);
    function getOntopHeight(): integer;
    procedure seteditonly(mode: boolean);
    function geteditonly: boolean;
    procedure setloggedin(ishere: boolean);
    function getloggedin: boolean;
    function getfloggedin_created(): boolean;
    procedure setfloggedin_created(newvalue: boolean);
    procedure debugOut(level: integer; meldung: string); overload;
    procedure debugOut(level: integer; Source: string; meldung: string); overload;
    procedure debugOutList(level: integer; Source: string; meldungen: TStrings);
    function getshowallprojects(): boolean;
    function getdebuglevel(): integer;
    procedure setdebuglevel(newlevel: integer);
    //procedure writeVerinfoToLog(var feil: Textfile);
    procedure TerminateApplication;
    function isValidEvent(event: string): boolean;
    function dateIsHolyday(mydate: TdateTime): boolean;
    procedure CustomExceptionHandler(Sender: TObject; E: Exception);
    procedure DumpExceptionCallStack(E: Exception);
    procedure OnEndSession(Sender: TObject);
    procedure SetFontName(Control: TControl; Name: string);
  private
    { private declarations }
  public
    { public declarations }
  end;


const
  //verDatum = '14.12.2015';
  ontopSchmal = 17;
  ontopHoch = 21;

var
  DataModule1: TDataModule1;
  uid: string;
  user_h_per_day: double;
  user_work_days: set of byte;
  leftint: integer;
  screenx, screeny: integer;
  ontopwidth: integer;
  ontopheight: integer;
  onlyEdit: boolean;
  loggedin: boolean;
  ///Reg: TRegistry;
  logfeil: TextFile;
  ontop_showallprojects: boolean;
  floggedin_created: boolean;
  version: string;
  debuglevel: integer;
  FileVerInfo: TFileVersionInfo;
  verDatum: string;
  Trayshow: boolean;
  TrayInterval: cardinal;
  scalefactor: double = 1.0;
  myFont: string;
  myscreen : TScreen;



implementation

{$R *.lfm}


uses ontop, login, debug, logoff, dataedit,
  loggedin_, statistik, work_description, nachf,
  treescrolldown,
  notificationdlg;

///uses (*expo, impo, *)(*, *){,} , , , { mexpo, shell_func,}
///  , ; ///, versioninfo;

//var
//  hostconnected, hostresolved: boolean;
///vi: TVersionInfoRec;



{ TDataModule1 }
function TDataModule1.getdebuglevel(): integer;
begin
  // wiil be called at start before the logfile is initialized
  if Assigned(logdatei) then
    getdebuglevel := logdatei.LogLevel
  else
    getdebuglevel := debuglevel;
end;

procedure TDataModule1.setdebuglevel(newlevel: integer);
var
  logdir, logfeilname: string;
  myini: TIniFile;
begin
  if Assigned(logdatei) then
    logdatei.LogLevel := newlevel;
  debuglevel := newlevel;
  // write new loglevel to conf
  // we will use logdir for logging and for configuration
  logdir := SysUtils.GetAppConfigDir(False);
  if logdir = '' then
  begin
    logdir := SysUtils.GetUserDir;
    logdir := logdir + '\uibtime';
  end;
  logdir := ExpandFileNameUTF8(logdir);
  ForceDirectories(logdir);
  logfeilname := ExpandFileNameUTF8(logdir + 'uibtime.conf');
  myini := TIniFile.Create(logfeilname);
  debugOut(5, 'Will use conf file: ' + logfeilname);
  DataModule1.debugOut(6, 'setdebuglevel', 'Will use conf file: ' + logfeilname);
  if myini = nil then
  begin
    DataModule1.debugOut(2, 'setdebuglevel',
      'myini = nil: coud not open :' + logfeilname);
    ShowMessage('Fehler in Konfigurations Datei. Bitte Log sichern. Programm wird beendet');
    Application.Terminate;
  end;
  myini.WriteInteger('general', 'debuglevel', newlevel);
  myini.UpdateFile;
  myini.Free;
  debugOut(6, 'debuglevel now: ' + IntToStr(newlevel));
end;

procedure TDataModule1.setOntopwidth(newvalue: integer);
begin
  ontopwidth := newvalue;
  leftint := (screenx - ontopwidth) div 2;
end;

procedure TDataModule1.setOntopheight(newvalue: integer);
begin
  ontopheight := newvalue;
end;

function TDataModule1.getOntopHeight(): integer;
begin
  getOntopHeight := ontopHeight;
end;

function TDataModule1.getLeftint(): integer;
begin
  getLeftint := leftint;
end;

function TDataModule1.getVersion(): string;
begin
  getVersion := version;
end;

function TDataModule1.getVerDatum(): string;
begin
  getVerDatum := verDatum;
end;

function TDataModule1.getshowallprojects(): boolean;
begin
  getshowallprojects := ontop_showallprojects;
  debugOut(6, 'ontop_showallprojects: ' + BoolToStr(ontop_showallprojects, True));
end;


function TDataModule1.getfloggedin_created(): boolean;
begin
  getfloggedin_created := floggedin_created;
end;

procedure TDataModule1.setfloggedin_created(newvalue: boolean);
begin
  floggedin_created := newvalue;
end;


procedure TDataModule1.seteditonly(mode: boolean);
begin
  if mode then
  begin
    onlyedit := True;
    TimerLogoffOnTop.Enabled := False;
    TimerOnTop.Enabled := False;
  end
  else //false
  begin
    onlyedit := False;
  end;
end;

function TDataModule1.geteditonly: boolean;
begin
  geteditonly := onlyedit;
end;


procedure TDataModule1.debugOut(level: integer; meldung: string);
begin
  debugOut(level, '', meldung);
end;

procedure TDataModule1.debugOut(level: integer; Source: string; meldung: string);
var
  sourcestr, logstr, logdir, logfeilname: string;
  aktlevel: integer;
begin
  try
    aktlevel := LLdebug;
    if Assigned(logdatei) then
    begin
      logdatei.AktProduktId := Source;
      logdatei.log(meldung, level);
      aktlevel := logdatei.LogLevel;
    end;
    if level <= aktlevel then
    begin
      if FDebug <> nil then
      begin
        if Source <> '' then
          sourcestr := '[' + Source + '] '
        else
          sourcestr := '';
        logstr := '[' + IntToStr(level) + '] [' + DateTimeToStr(Now) +
          '] ' + sourcestr + meldung;
        FDebug.memo1.Lines.add(logstr);
      end;
    end;
  except
    on e: Exception do
    begin
      if Assigned(logdatei) then
      begin
        logdatei.log('exception in DataModule1.debugOut', LLcritical);
        logdatei.log(e.Message, LLcritical);
      end;
      raise;
    end;
  end;
end;

procedure TDataModule1.debugOutList(level: integer; Source: string; meldungen: TStrings);
var
  sourcestr, logstr, logdir, logfeilname: string;
  aktlevel, i: integer;
begin
  try
    aktlevel := LLdebug;
    if Assigned(logdatei) then
    begin
      logdatei.AktProduktId := Source;
      LogDatei.log_list(meldungen, level);
      //logdatei.log(meldung, level);
      aktlevel := logdatei.LogLevel;
    end;
    if level <= aktlevel then
    begin
      for i := 0 to meldungen.Count - 1 do
        if FDebug <> nil then
        begin
          if Source <> '' then
            sourcestr := '[' + Source + '] '
          else
            sourcestr := '';
          logstr := '[' + IntToStr(level) + '] [' + DateTimeToStr(Now) +
            '] ' + sourcestr + meldungen[i];
          FDebug.memo1.Lines.add(logstr);
        end;
    end;
  except
    on e: Exception do
    begin
      if Assigned(logdatei) then
      begin
        logdatei.log('exception in DataModule1.debugOut', LLcritical);
        logdatei.log(e.Message, LLcritical);
      end;
      raise;
    end;
  end;
end;

(*
procedure TDataModule1.debugOut(level: integer; Source: string; meldung: string);
var
  sourcestr, logstr, logdir, logfeilname: string;
begin
  try
    if Source <> '' then
      sourcestr := '[' + Source + '] '
    else
      sourcestr := '';
    if level <= debuglevel then
    begin
      logstr := '[' + IntToStr(level) + '] [' + DateTimeToStr(Now) + '] ' + sourcestr + meldung;
      logdir := SysUtils.GetAppConfigDir(False);
      if logdir = '' then
      begin
        logdir := SysUtils.GetUserDir;
        logdir := logdir + '\uibtime';
      end;
      logfeilname := ExpandFileNameUTF8(logdir + '\uibtime.log');
      AssignFile(logfeil, logfeilname);
      append(logfeil);
      writeln(logfeil, logstr);
      flush(logfeil);
      Close(logfeil);
      if FDebug <> nil then
        FDebug.memo1.Lines.add(logstr);
    end;
  except
  end;
end;
*)

procedure TDataModule1.Dateneditieren1Click(Sender: TObject);
begin
  FDataedit := TFDataedit.Create(self);
  FOntop.ineditmode := True;
  FDataedit.showmodal;
  FDataedit.FormStyle := fsNormal;
  FDataedit.PopupParent := FDataedit;
  //FOntop.Enabled := False;
  //FDataedit.Show;
  FreeAndNil(FDataedit);
  DataModule1.SQuibevent.last;
  ontop.lastevent := DataModule1.SQuibevent.FieldByName('event').AsString;
  FOntop.ineditmode := False;
  FOntop.eventhandler(ontop.lastevent);
end;

procedure TDataModule1.Dateneditieren1Cancel;
begin
  //FStatistik.free;
  DataModule1.SQuibevent.last;
  ontop.lastevent := DataModule1.SQuibevent.FieldByName('event').AsString;
  FOntop.Enabled := True;
  FOntop.ineditmode := False;
  FOntop.eventhandler(ontop.lastevent);
  TimerOntop.Enabled := True;
end;


procedure TDataModule1.Autologin1Click(Sender: TObject);
var
  logdir, logfeilname: string;
  myini: TIniFile;
begin
  // we will use logdir for logging and for configuration
  logdir := SysUtils.GetAppConfigDir(False);
  if logdir = '' then
  begin
    logdir := SysUtils.GetUserDir;
    logdir := logdir + '\uibtime';
  end;
  logdir := ExpandFileNameUTF8(logdir);
  ForceDirectories(logdir);
  logfeilname := ExpandFileNameUTF8(logdir + 'uibtime.conf');
  myini := TIniFile.Create(logfeilname);
  debugOut(5, 'Will use conf file: ' + logfeilname);
  DataModule1.debugOut(6, 'Autologin1Click', 'Will use conf file: ' + logfeilname);
  if myini = nil then
  begin
    DataModule1.debugOut(2, 'Autologin1Click',
      'myini = nil: coud not open :' + logfeilname);
    ShowMessage('Fehler in Konfigurations Datei. Bitte Log sichern. Programm wird beendet');
    Application.Terminate;
  end;
  if Autologin1.Checked then
  begin
    Autologin1.Checked := False;
    myini.WriteBool('general', 'autologin', False);
  end
  else
  begin
    Autologin1.Checked := True;
    myini.WriteBool('general', 'autologin', True);
  end;
  myini.UpdateFile;
  myini.Free;
end;

procedure TDataModule1.ArbeitsberichteClick(Sender: TObject);
begin
  TimerOntop.Enabled := False;
  FuibtWorkRepChooser := TFuibtWorkRepChooser.Create(self);
  FuibtWorkRepChooser.Show;
end;



procedure TDataModule1.Datenweitergabe1Click(Sender: TObject);
begin

end;

procedure TDataModule1.Export1Click(Sender: TObject);
begin

end;

procedure TDataModule1.Hilfe1Click(Sender: TObject);
begin

end;

procedure TDataModule1.IBConnection1Log(Sender: TSQLConnection;
  EventType: TDBEventType; const Msg: string);
var
  Source: string;
begin
  //TDBEventType = (detCustom, detPrepare, detExecute, detFetch, detCommit, detRollBack, detParamValue, detActualSQL);
  case EventType of
    detCustom: Source := 'Custom:  ';
    detPrepare: Source := 'Prepare: ';
    detExecute: Source := 'Execute: ';
    detFetch: Source := 'Fetch:   ';
    detCommit: Source := 'Commit:  ';
    detRollBack: Source := 'Rollback:';
    detParamValue: Source := 'ParamValue:';
    detActualSQL: Source := 'ActualSQL:';
    else
      Source := 'Unknown event: ';
  end;
  debugOut(9, 'Database', Source + Msg);
end;

procedure TDataModule1.Import1Click(Sender: TObject);
begin

end;

procedure TDataModule1.Info1Click(Sender: TObject);
begin
  ShowMessage('uibtime Version ' + getVersion + ' vom ' + getVerDatum +
    ' (c) www.uib.de');
end;

procedure TDataModule1.LeisteNeuAufbauen1Click(Sender: TObject);
begin
  FOnTop.ReBuildForm;
end;

procedure TDataModule1.PopupMenu1Close(Sender: TObject);
begin
  TimerOnTop.Enabled := True;
end;

procedure TDataModule1.PopupMenu1Popup(Sender: TObject);
begin
  TimerOnTop.Enabled := False;
end;

procedure TDataModule1.queryAfterHelper(myevent: string; myquery: TSQLQuery;
  DataSet: TDataSet);
begin
  try
    if (DataSet.UpdateStatus in [usModified, usInserted, usDeleted]) or
      (lowercase(myevent) = 'afterdelete') then
    begin
      try
        myquery.ApplyUpdates;
        debugOut(5, DataSet.Name + myevent, 'start  ' + DataSet.Name +
          myevent + ' (apply)');
      except
        debugOut(2, DataSet.Name + myevent, 'exception in ' +
          DataSet.Name + myevent + ' (ApplyUpdates)');
        raise
      end;
    end;
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      debugOut(5, DataSet.Name + myevent, 'start  ' + DataSet.Name +
        myevent + ' (commit/start)');
    end
    else
      debugOut(3, DataSet.Name + myevent, 'Error: Missing Transaction ' +
        DataSet.Name + myevent + ' ');
  except
    debugOut(2, DataSet.Name + myevent, 'exception in ' + DataSet.Name +
      myevent + ' (commit)');
    raise
  end;
end;



procedure TDataModule1.Query4ResultAfterDelete(DataSet: TDataSet);
var
  myevent: string;
  myquery: TSQLQuery;
begin
  myquery := DataModule1.Query4Result;
  myevent := 'AfterDelete';
  queryAfterHelper(myevent, myquery, DataSet);
  (*
  debugOut(5, 'start  '+ DataSet.Name+' AfterDelete');
  try
    // after delete the state is not edit or insert but we have to apply
    //if DataModule1.SQuibevent.State in [dsInsert, dsEdit] then
      DataModule1.Query4Result.ApplyUpdates;
      debugOut(5, DataSet.Name+'AfterDelete', 'start  '+ DataSet.Name+' AfterDelete (apply)');
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      debugOut(5,DataSet.Name+'AfterDelete',  'start  '+ DataSet.Name+' AfterDelete (commit/start)');
    end
    else debugOut(3,DataSet.Name+'AfterDelete',  'Error: Missing Transaction '+ DataSet.Name+' AfterDelete');
  except
    debugOut(2,DataSet.Name+'AfterDelete',  'exception in '+ DataSet.Name+' AfterDelete (commit)');
  end;
  *)
end;

procedure TDataModule1.Query4ResultAfterPost(DataSet: TDataSet);
var
  myevent: string;
  myquery: TSQLQuery;
begin
  myquery := DataModule1.Query4Result;
  myevent := 'AfterPost';
  queryAfterHelper(myevent, myquery, DataSet);
  (*
  try
    if DataSet.UpdateStatus in [usModified,usInserted,usDeleted] then
    begin
      myquery.ApplyUpdates;
      debugOut(5,DataSet.Name+myeqevent,'start  '+ DataSet.Name+myeqevent+' (apply)');
    end;
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      debugOut(5,DataSet.Name+myeqevent,  'start  '+ DataSet.Name+myeqevent+' (commit/start)');
    end
    else debugOut(3,DataSet.Name+myeqevent,  'Error: Missing Transaction '+ DataSet.Name+myeqevent+' ');
  except
    debugOut(2,DataSet.Name+myeqevent,  'exception in '+ DataSet.Name+myeqevent+' (commit)');
  end;
  *)
end;


procedure TDataModule1.schmaleLeisteClick(Sender: TObject);
begin
end;

procedure TDataModule1.ShowDebugWindow1Click(Sender: TObject);
var
  logdir, logfeilname: string;
  myini: TIniFile;
begin
  // we will use logdir for logging and for configuration
  logdir := SysUtils.GetAppConfigDir(False);
  if logdir = '' then
  begin
    logdir := SysUtils.GetUserDir;
    logdir := logdir + '\uibtime';
  end;
  logdir := ExpandFileNameUTF8(logdir);
  ForceDirectories(logdir);
  logfeilname := ExpandFileNameUTF8(logdir + 'uibtime.conf');
  myini := TIniFile.Create(logfeilname);
  debugOut(5, 'Will use conf file: ' + logfeilname);
  DataModule1.debugOut(6, 'ShowDebugWindow1Click', 'Will use conf file: ' + logfeilname);
  if myini = nil then
  begin
    DataModule1.debugOut(2, 'ShowDebugWindow1Click',
      'myini = nil: coud not open :' + logfeilname);
    ShowMessage('Fehler in Konfigurations Datei. Bitte Log sichern. Programm wird beendet');
    Application.Terminate;
  end;
  if ShowDebugWindow1.Checked then
  begin
    ShowDebugWindow1.Checked := False;
    FDebug.Visible := False;
  end
  else
  begin
    ShowDebugWindow1.Checked := True;
    FDebug.Visible := True;
  end;
  myini.WriteBool('general', 'ShowDebugWindow', ShowDebugWindow1.Checked);
  debugOut(6, 'ShowDebugWindow: ' + BoolToStr(ShowDebugWindow1.Checked, True));
  myini.UpdateFile;
  myini.Free;
end;

procedure TDataModule1.SQholydaysAfterDelete(DataSet: TDataSet);
begin
  debugOut(5, 'start  ' + DataSet.Name + ' AfterDelete');
  try
    // after delete the state is not edit or insert but we have to apply
    //if DataModule1.SQuibevent.State in [dsInsert, dsEdit] then
    DataModule1.SQuibaktevent.ApplyUpdates;
    debugOut(5, 'start  ' + DataSet.Name + ' AfterDelete (apply)');
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      debugOut(5, 'start  ' + DataSet.Name + ' AfterDelete (commit/start)');
    end
    else
      debugOut(3, 'SQholydaysAfterDelete', 'Error: Missing Transaction ' +
        DataSet.Name + ' AfterDelete');
  except
    debugOut(2, 'SQholydaysAfterDelete', 'exception in ' + DataSet.Name +
      ' AfterDelete (commit)');
  end;
end;

procedure TDataModule1.SQholydaysAfterInsert(DataSet: TDataSet);
begin
  try
    //DataModule1.SQuibaktevent.ApplyUpdates;
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      //SQLTransaction1.Commit;
      //SQLTransaction1.StartTransaction;
    end
    else
      debugOut(3, 'SQholydaysAfterInsert',
        'Error: Missing Transaction SQuibakteventAfterInsert');
    debugOut(5, 'Commit in SQholydaysAfterInsert');
  except
    debugOut(2, 'SQholydaysAfterInsert',
      'exception in SQuibakteventAfterInsert (commit)');
  end;
end;

procedure TDataModule1.SQholydaysAfterPost(DataSet: TDataSet);
begin
  try
    //if DataModule1.SQuibaktevent.State in [dsInsert, dsEdit] then
    if DataSet.UpdateStatus in [usModified, usInserted, usDeleted] then
    begin
      DataModule1.SQholydays.ApplyUpdates;
      debugOut(5, 'SQholydaysAfterPost', 'start  SQholydaysAfterPost (apply)');
    end;
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      //SQLTransaction1.Commit;
      //SQLTransaction1.StartTransaction;
    end
    else
      debugOut(3, 'SQholydaysAfterPost',
        'Error: Missing Transaction SQuibakteventAfterPost');
    debugOut(5, 'Commit in SQholydaysAfterPost');
  except
    debugOut(2, 'SQholydaysAfterPost', 'exception in SQholydaysAfterPost (commit)');
  end;
end;

procedure TDataModule1.SQuibakteventAfterDelete(DataSet: TDataSet);
begin
  debugOut(5, 'start  ' + DataSet.Name + ' AfterDelete');
  try
    // after delete the state is not edit or insert but we have to apply
    //if DataModule1.SQuibevent.State in [dsInsert, dsEdit] then
    DataModule1.SQuibaktevent.ApplyUpdates;
    debugOut(5, 'start  ' + DataSet.Name + ' AfterDelete (apply)');
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      debugOut(5, 'start  ' + DataSet.Name + ' AfterDelete (commit/start)');
    end
    else
      debugOut(3, 'SQuibakteventAfterDelete', 'Error: Missing Transaction ' +
        DataSet.Name + ' AfterDelete');
  except
    debugOut(2, 'SQuibakteventAfterDelete', 'exception in ' +
      DataSet.Name + ' AfterDelete (commit)');
  end;
end;

procedure TDataModule1.SQuibakteventAfterEdit(DataSet: TDataSet);
begin
  debugOut(5, 'start  ' + DataSet.Name + ' BeforePost');
end;

procedure TDataModule1.SQuibakteventAfterInsert(DataSet: TDataSet);
begin
  try
    //DataModule1.SQuibaktevent.ApplyUpdates;
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      //SQLTransaction1.Commit;
      //SQLTransaction1.StartTransaction;
    end
    else
      debugOut(3, 'SQuibakteventAfterInsert',
        'Error: Missing Transaction SQuibakteventAfterInsert');
    debugOut(5, 'Commit in SQuibakteventAfterInsert');
  except
    debugOut(2, 'SQuibakteventAfterInsert',
      'exception in SQuibakteventAfterInsert (commit)');
  end;
end;

procedure TDataModule1.SQuibakteventAfterPost(DataSet: TDataSet);
begin
  try
    //if DataModule1.SQuibaktevent.State in [dsInsert, dsEdit] then
    if DataSet.UpdateStatus in [usModified, usInserted, usDeleted] then
    begin
      DataModule1.SQuibaktevent.ApplyUpdates;
      debugOut(5, 'SQuibakteventAfterPost', 'start  SQuibakteventAfterPost (apply)');
    end;
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      //SQLTransaction1.Commit;
      //SQLTransaction1.StartTransaction;
    end
    else
      debugOut(3, 'SQuibakteventAfterPost',
        'Error: Missing Transaction SQuibakteventAfterPost');
    debugOut(5, 'Commit in SQuibakteventAfterPost');
  except
    debugOut(2, 'SQuibakteventAfterPost',
      'exception in SQuibakteventAfterPost (commit)');
  end;
end;

procedure TDataModule1.SQuibakteventBeforePost(DataSet: TDataSet);
begin
  debugOut(5, 'start  ' + DataSet.Name + ' BeforePost');
  debugOut(5, 'end  ' + DataSet.Name + ' BeforePost');
end;

procedure TDataModule1.SQuibaktuserAfterDelete(DataSet: TDataSet);
begin
  debugOut(5, 'start  ' + DataSet.Name + ' AfterDelete');
  try
    // after delete the state is not edit or insert but we have to apply
    //if DataModule1.SQuibevent.State in [dsInsert, dsEdit] then
    DataModule1.SQuibaktuser.ApplyUpdates;
    debugOut(5, 'SQuibaktuserAfterDelete', 'start  ' + DataSet.Name +
      ' AfterDelete (apply)');
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      debugOut(5, 'SQuibaktuserAfterDelete', 'start  ' + DataSet.Name +
        ' AfterDelete (commit/start)');
    end
    else
      debugOut(3, 'SQuibaktuserAfterDelete', 'Error: Missing Transaction ' +
        DataSet.Name + ' AfterDelete');
  except
    debugOut(2, 'SQuibaktuserAfterDelete', 'exception in ' +
      DataSet.Name + ' AfterDelete (commit)');
  end;
end;

procedure TDataModule1.SQuibaktuserAfterPost(DataSet: TDataSet);
begin
  try
    //if DataModule1.SQuibaktuser.State in [dsInsert, dsEdit] then
    if DataSet.UpdateStatus in [usModified, usInserted, usDeleted] then
    begin
      DataModule1.SQuibaktuser.ApplyUpdates;
      debugOut(5, 'SQuibaktuserAfterPost', 'start  SQuibaktuserAfterPost (apply)');
    end;
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      //SQLTransaction1.Commit;
      //SQLTransaction1.StartTransaction;
    end
    else
      debugOut(3, 'Error: Missing Transaction SQuibaktuserAfterPost');
    debugOut(5, 'SQuibaktuserAfterPost', 'Commit in SQuibaktuserAfterPost');
  except
    debugOut(2, 'SQuibaktuserAfterPost', 'exception in SQuibaktuserAfterPost (commit)');
  end;
end;

procedure TDataModule1.SQuiballeventAfterPost(DataSet: TDataSet);
begin
  try
    //if DataModule1.SQuiballevent.State in [dsInsert, dsEdit] then
    if DataSet.UpdateStatus in [usModified, usInserted, usDeleted] then
    begin
      DataModule1.SQuiballevent.ApplyUpdates;
      debugOut(5, 'SQuiballeventAfterPost', 'start  SQuiballeventAfterPost (apply)');
    end;
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      //SQLTransaction1.Commit;
      //SQLTransaction1.StartTransaction;
    end
    else
      debugOut(3, 'Error: Missing Transaction SQuiballeventAfterPost');
    debugOut(5, 'SQuiballeventAfterPost', 'Commit in SQuiballeventAfterPost');
  except
    debugOut(2, 'SQuiballeventAfterPost',
      'exception in SQuiballeventAfterPost (commit)');
  end;
end;

procedure TDataModule1.SQuibdefprojAfterDelete(DataSet: TDataSet);
begin
  debugOut(5, 'start  ' + DataSet.Name + ' AfterDelete');
  try
    // after delete the state is not edit or insert but we have to apply
    DataModule1.SQuibdefproj.ApplyUpdates;
    debugOut(5, 'SQuibdefprojAfterDelete', 'start  ' + DataSet.Name +
      ' AfterDelete (apply)');
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      debugOut(5, 'SQuibdefprojAfterDelete', 'start  ' + DataSet.Name +
        ' AfterDelete (commit/start)');
    end
    else
      debugOut(3, 'SQuibdefprojAfterDelete', 'Error: Missing Transaction ' +
        DataSet.Name + ' AfterDelete');
  except
    debugOut(2, 'SQuibdefprojAfterDelete', 'exception in ' +
      DataSet.Name + ' AfterDelete (commit)');
  end;
end;

procedure TDataModule1.SQuibdefprojAfterPost(DataSet: TDataSet);
begin
  try
    //if DataModule1.SQuibdefproj.State in [dsInsert, dsEdit] then
    if DataSet.UpdateStatus in [usModified, usInserted, usDeleted] then
    begin
      DataModule1.SQuibdefproj.ApplyUpdates;
      debugOut(5, 'SQuibdefprojAfterPost', 'start  SQuibdefprojAfterPost (apply)');
    end;
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      //SQLTransaction1.Commit;
      //SQLTransaction1.StartTransaction;
    end
    else
      debugOut(3, 'SQuibdefprojAfterPost',
        'Error: Missing Transaction SQuibdefprojAfterPost');
    debugOut(5, 'Commit in SQuibdefprojAfterPost');
  except
    debugOut(2, 'SQuibdefprojAfterPost', 'exception in SQuibdefprojAfterPost (commit)');
  end;
end;

procedure TDataModule1.SQuibeventAfterDelete(DataSet: TDataSet);
begin
  debugOut(5, 'SQuibeventAfterDelete', 'start  ' + DataSet.Name + ' AfterDelete');
  try
    // after delete the state is not edit or insert but we have to apply
    //if DataModule1.SQuibevent.State in [dsInsert, dsEdit] then
    DataModule1.SQuibevent.ApplyUpdates;
    debugOut(5, 'start  ' + DataSet.Name + ' AfterDelete (apply)');
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      debugOut(5, 'SQuibeventAfterDelete', 'start  ' + DataSet.Name +
        ' AfterDelete (commit/start)');
    end
    else
      debugOut(3, 'SQuibeventAfterDelete', 'Error: Missing Transaction ' +
        DataSet.Name + ' AfterDelete');
  except
    debugOut(2, 'SQuibeventAfterDelete', 'exception in ' + DataSet.Name +
      ' AfterDelete (commit)');
  end;
end;


procedure TDataModule1.SQuibeventAfterInsert(DataSet: TDataSet);
begin
  SQuibevent.FieldByName('pkuserid').AsString := uid;
  //Tableevent.Fields[0].AsString := uid;
  SQuibevent.FieldByName('pkinittime').AsDateTime := date + time;
  //Tableevent.Fields[1].asdatetime := now;
  SQuibevent.FieldByName('userid').AsString := uid;
  SQuibevent.FieldByName('starttime').AsDateTime := now;
  SQuibevent.FieldByName('stoptime').AsDateTime := now;
end;

procedure TDataModule1.SQuibeventAfterPost(DataSet: TDataSet);
begin
  debugOut(5, 'SQuibeventAfterPost', 'start  SQuibeventAfterPost');
  try
    //if DataModule1.SQuibevent.State in [dsInsert, dsEdit] then
    if DataSet.UpdateStatus in [usModified, usInserted, usDeleted] then
    begin
      DataModule1.SQuibevent.ApplyUpdates;
      debugOut(5, 'SQuibeventAfterPost', 'start  SQuibeventAfterPost (apply)');
    end;
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      //SQLTransaction1.Commit;
      //SQLTransaction1.StartTransaction;
      debugOut(5, 'SQuibeventAfterPost', 'start  SQuibeventAfterPost (commit/start)');
    end
    else
      debugOut(3, 'SQuibeventAfterPost',
        'Error: Missing Transaction SQuibeventAfterPost');
  except
    debugOut(2, 'SQuibeventAfterPost', 'exception in SQuibeventAfterPost (commit)');
  end;

end;

procedure TDataModule1.SQuibeventBeforeClose(DataSet: TDataSet);
begin
  if login.automatic then
  begin
    DataModule1.SQuibevent.last;
    DataModule1.SQuibevent.edit;
    DataModule1.SQuibevent.FieldByName('stoptime').AsDateTime := now;
    try
      DataModule1.SQuibevent.post;
      DataModule1.SQuibevent.ApplyUpdates;
    except
      DataModule1.SQuibevent.post;
      DataModule1.SQuibevent.ApplyUpdates;
    end;
  end;
end;

procedure TDataModule1.SQuibeventBeforeDelete(DataSet: TDataSet);
begin
  (*
  try
    DataModule1.SQuibevent.ApplyUpdates;
    if SQLTransaction1.Active then
      SQLTransaction1.CommitRetaining;
    debugOut(5, 'StartTransaction in SQuibeventBeforeDelete: ');
  except
    debugOut(3,'', 'exception in SQuibeventBeforeDelete (starttransaction)');
  end;
  *)
end;

procedure TDataModule1.SQuibeventBeforePost(DataSet: TDataSet);
begin
 (*
     try
  if not SQLTransaction1.Active then SQLTransaction1.StartTransaction;
  debugOut(5,'StartTransaction in SQuibeventBeforePost: ');
 except
  debugOut(5,'exception in SQuibeventBeforePost (starttransaction)');
 end;
 *)
end;

procedure TDataModule1.SQuibeventPostError(DataSet: TDataSet;
  E: EDatabaseError; var DataAction: TDataAction);
begin
  ShowMessage('Beim Schreiben des Datensatzes in uibevent ist ein Fehler aufgetreten: ' +
    LineEnding + E.Message + LineEnding + 'Führe jetzt Cancel auf Datensatz aus.' +
    LineEnding + 'Bitte Datensatz kontrollieren.');
  DataSet.Cancel;
  DataAction := daAbort;
  debugOut(3, 'SQuibeventPostError', E.Message);
end;

procedure TDataModule1.SQuibloggedinAfterDelete(DataSet: TDataSet);
begin
  debugOut(5, 'SQuibloggedinAfterDelete', 'start  ' + DataSet.Name + ' AfterDelete');
  try
    // after delete the state is not edit or insert but we have to apply
    //if DataModule1.SQuibevent.State in [dsInsert, dsEdit] then
    DataModule1.SQuibloggedin.ApplyUpdates;
    debugOut(5, 'start  ' + DataSet.Name + ' AfterDelete (apply)');
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      debugOut(5, 'SQuibloggedinAfterDelete', 'start  ' + DataSet.Name +
        ' AfterDelete (commit/start)');
    end
    else
      debugOut(3, 'SQuibloggedinAfterDelete', 'Error: Missing Transaction ' +
        DataSet.Name + ' AfterDelete');
  except
    debugOut(2, 'SQuibloggedinAfterDelete', 'exception in ' +
      DataSet.Name + ' AfterDelete (commit)');
  end;
end;

procedure TDataModule1.SQuibloggedinAfterPost(DataSet: TDataSet);
begin
  debugOut(5, 'SQuibloggedinAfterPost', 'start ');
  try
    if DataSet.UpdateStatus in [usModified, usInserted, usDeleted] then
    begin
      debugOut(7, 'SQuibloggedinAfterPost', ' ApplyUpdates ....');
      DataModule1.SQuibloggedin.ApplyUpdates;
      debugOut(6, 'SQuibloggedinAfterPost', ' ApplyUpdates done');
    end;
  except
    debugOut(2, 'SQuibloggedinAfterPost', 'exception in ApplyUpdates');
  end;
  try
    if SQLTransaction1.Active then
    begin
      debugOut(7, 'SQuibloggedinAfterPost', ' CommitRetaining ....');
      SQLTransaction1.CommitRetaining;
      debugOut(6, 'SQuibloggedinAfterPost', ' CommitRetaining done');
    end
    else
      debugOut(3, 'SQuibloggedinAfterPost', 'Error: Missing Transaction');
  except
    debugOut(2, 'SQuibloggedinAfterPost', 'exception in CommitRetaining');
  end;
  debugOut(5, 'SQuibloggedinAfterPost', 'end ');
end;

procedure TDataModule1.SQuibsollAfterDelete(DataSet: TDataSet);
begin
  debugOut(5, 'SQuibsollAfterDelete', 'start  ' + DataSet.Name + ' AfterDelete');
  try
    // after delete the state is not edit or insert but we have to apply
    DataModule1.SQuibsoll.ApplyUpdates;
    debugOut(5, 'SQuibsollAfterDelete', 'start  ' + DataSet.Name +
      ' AfterDelete (apply)');
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      debugOut(5, 'SQuibsollAfterDelete', 'start  ' + DataSet.Name +
        ' AfterDelete (commit/start)');
    end
    else
      debugOut(3, 'SQuibsollAfterDelete', 'Error: Missing Transaction ' +
        DataSet.Name + ' AfterDelete');
  except
    debugOut(2, 'SQuibsollAfterDelete', 'exception in ' + DataSet.Name +
      ' AfterDelete (commit)');
  end;
end;

procedure TDataModule1.SQuibsollAfterPost(DataSet: TDataSet);
begin
  try
    //if DataModule1.SQuibsoll.State in [dsInsert, dsEdit] then
    if DataSet.UpdateStatus in [usModified, usInserted, usDeleted] then
    begin
      DataModule1.SQuibsoll.ApplyUpdates;
      debugOut(5, 'SQuibsollAfterPost', 'start  SQuibsollAfterPost (apply)');
    end;
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      //SQLTransaction1.Commit;
      //SQLTransaction1.StartTransaction;
    end
    else
      debugOut(3, 'Error: Missing Transaction SQuibsollAfterPost');
    debugOut(5, 'SQuibsollAfterPost', 'StartTransaction in SQuibsollAfterPost: ');
  except
    debugOut(2, 'SQuibsollAfterPost',
      'exception in SQuibsollAfterPost (starttransaction)');
  end;
end;

procedure TDataModule1.SQuibtimeoutAfterDelete(DataSet: TDataSet);
begin
  debugOut(5, 'SQuibtimeoutAfterDelete', 'start  ' + DataSet.Name + ' AfterDelete');
  try
    // after delete the state is not edit or insert but we have to apply
    DataModule1.SQuibtimeout.ApplyUpdates;
    debugOut(5, 'SQuibtimeoutAfterDelete', 'start  ' + DataSet.Name +
      ' AfterDelete (apply)');
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      debugOut(5, 'SQuibtimeoutAfterDelete', 'start  ' + DataSet.Name +
        ' AfterDelete (commit/start)');
    end
    else
      debugOut(3, 'SQuibtimeoutAfterDelete', 'Error: Missing Transaction ' +
        DataSet.Name + ' AfterDelete');
  except
    debugOut(2, 'SQuibtimeoutAfterDelete', 'exception in ' +
      DataSet.Name + ' AfterDelete (commit)');
  end;
end;

procedure TDataModule1.SQuibtimeoutAfterPost(DataSet: TDataSet);
begin
  try
    //if DataModule1.SQuibtimeout.State in [dsInsert, dsEdit] then
    if DataSet.UpdateStatus in [usModified, usInserted, usDeleted] then
    begin
      DataModule1.SQuibtimeout.ApplyUpdates;
      debugOut(5, 'SQuibtimeoutAfterPost', 'start  SQuibtimeoutAfterPost (apply)');
    end;
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      //SQLTransaction1.Commit;
      //SQLTransaction1.StartTransaction;
    end
    else
      debugOut(3, 'Error: Missing Transaction SQuibtimeoutAfterPost');
    debugOut(5, 'SQuibtimeoutAfterPost', 'StartTransaction in SQuibtimeoutAfterPost: ');
  except
    debugOut(2, 'SQuibtimeoutAfterPost',
      'exception in SQuibtimeoutAfterPost (starttransaction)');
  end;
end;

procedure TDataModule1.SQuibusereventAfterDelete(DataSet: TDataSet);
begin
  debugOut(5, 'SQuibusereventAfterDelete', 'start  ' + DataSet.Name + ' AfterDelete');
  try
    // after delete the state is not edit or insert but we have to apply
    //if DataModule1.SQuibevent.State in [dsInsert, dsEdit] then
    DataModule1.SQuibuserevent.ApplyUpdates;
    debugOut(5, 'SQuibusereventAfterDelete', 'start  ' + DataSet.Name +
      ' AfterDelete (apply)');
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      debugOut(5, 'SQuibusereventAfterDelete', 'start  ' + DataSet.Name +
        ' AfterDelete (commit/start)');
    end
    else
      debugOut(3, 'SQuibusereventAfterDelete', 'Error: Missing Transaction ' +
        DataSet.Name + ' AfterDelete');
  except
    debugOut(2, 'SQuibusereventAfterDelete', 'exception in ' +
      DataSet.Name + ' AfterDelete (commit)');
  end;
end;

procedure TDataModule1.SQuibusereventAfterPost(DataSet: TDataSet);
begin
  try
    //if DataModule1.SQuibuserevent.State in [dsInsert, dsEdit] then
    if DataSet.UpdateStatus in [usModified, usInserted, usDeleted] then
    begin
      DataModule1.SQuibuserevent.ApplyUpdates;
      debugOut(5, 'SQuibusereventAfterPost', 'start  SQuibusereventAfterPost (apply)');
    end;
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
    end
    else
      debugOut(3, 'SQuibusereventAfterPost',
        'Error: Missing Transaction SQuibusereventAfterPost');
    debugOut(5, 'SQuibusereventAfterPost',
      'StartTransaction in SQuibusereventAfterPost: ');
  except
    debugOut(2, 'SQuibusereventAfterPost',
      'exception in SQuibusereventAfterPost (starttransaction)');
  end;
end;

procedure TDataModule1.SQwork_descriptionAfterDelete(DataSet: TDataSet);
begin
  debugOut(5, 'SQwork_descriptionAfterDelete', 'start  ' + DataSet.Name +
    ' AfterDelete');
  try
    // after delete the state is not edit or insert but we have to apply
    DataModule1.SQwork_description.ApplyUpdates;
    debugOut(5, 'SQwork_descriptionAfterDelete', 'start  ' +
      DataSet.Name + ' AfterDelete (apply)');
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      debugOut(5, 'SQwork_descriptionAfterDelete', 'start  ' +
        DataSet.Name + ' AfterDelete (commit/start)');
    end
    else
      debugOut(3, 'SQwork_descriptionAfterDelete', 'Error: Missing Transaction ' +
        DataSet.Name + ' AfterDelete');
  except
    debugOut(2, 'SQwork_descriptionAfterDelete', 'exception in ' +
      DataSet.Name + ' AfterDelete (commit)');
  end;
end;

procedure TDataModule1.SQwork_descriptionAfterInsert(DataSet: TDataSet);
begin
  SQwork_description.FieldByName('userid').AsString := uid;
  SQwork_description.FieldByName('jahr').AsInteger :=
    YearOf(ScanDateTime('dd.mm.yyyy', fwork_description.EditButtonDate.Text));
  SQwork_description.FieldByName('monat').AsInteger :=
    MonthOf(ScanDateTime('dd.mm.yyyy', fwork_description.EditButtonDate.Text));
  SQwork_description.FieldByName('tag').AsInteger :=
    DayOf(ScanDateTime('dd.mm.yyyy', fwork_description.EditButtonDate.Text));
  SQwork_description.FieldByName('event').AsString :=
    Query_day_report.FieldByName('event').AsString;
end;

procedure TDataModule1.SQwork_descriptionAfterPost(DataSet: TDataSet);
var
  myevent: string;
  myquery: TSQLQuery;
begin
  myquery := DataModule1.SQwork_description;
  myevent := 'AfterPost';
  queryAfterHelper(myevent, myquery, DataSet);
  (*
  debugOut(5, 'SQwork_descriptionAfterPost', 'start  ' + DataSet.Name + ' AfterPost');
  try
    //if (DataModule1.SQwork_description.State in [dsInsert, dsEdit])
    //   or DataModule1.SQwork_description.Modified then
    if DataSet.UpdateStatus in [usModified, usInserted, usDeleted] then
    begin
      DataModule1.SQwork_description.ApplyUpdates;
      debugOut(5, 'start  SQwork_descriptionAfterPost (apply)');
    end;
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      debugOut(5, 'SQwork_descriptionAfterPost', 'start  ' +
        DataSet.Name + ' AfterPost (CommitRetaining)');
    end
    else
      debugOut(3, 'Error: Missing Transaction SQwork_descriptionAfterPost');
    debugOut(5, 'SQwork_descriptionAfterPost', 'end SQwork_descriptionAfterPost: ');
  except
    debugOut(2, 'SQwork_descriptionAfterPost',
      'exception in SQwork_descriptionAfterPost (CommitRetaining)');
    raise;
  end;
  *)
end;

procedure TDataModule1.SQwork_descriptionPostError(DataSet: TDataSet;
  E: EDatabaseError; var DataAction: TDataAction);
begin
  ShowMessage('Beim Schreiben des Datensatzes in work_description ist ein Fehler aufgetreten: '
    + LineEnding + E.Message + LineEnding + 'Führe jetzt Cancel auf Datensatz aus.' +
    LineEnding + 'Bitte Datensatz kontrollieren.');
  DataSet.Cancel;
  DataAction := daAbort;
  debugOut(3, 'SQwork_descriptionPostError', E.Message);
end;


procedure TDataModule1.Statistik1Click(Sender: TObject);
begin
  TimerOntop.Enabled := False;
  FStatistik := TFStatistik.Create(self);
  FStatistik.Show;
end;


procedure TDataModule1.Statistik1Cancel;
begin
  TimerOntop.Enabled := True;
end;

procedure TDataModule1.TimerCheckNetTimer(Sender: TObject);
var
  cinfo: TConnInfoType;
  servername: string;
  retries: integer;
begin

  servername := 'groupware';
  ;
  retries := 0;
  {$IFDEF WIN32}
  while (pinghost(servername) = -1) and (retries < 10) do
  begin
    debugOut(3, 'CheckNetTimer', 'Could not reach ' + servername + ' retry ...');
    Inc(retries);
    Sleep(1000);
  end;
  if retries >= 10 then
    debugOut(2, 'CheckNetTimer', 'Could not reach ' + servername + ' no retry.');
 {$ENDIF WIN32}
  (*
     if mrAbort = MessageDlg('uibtime: Warnung','Die Netzwerkverbindung zum DB-Server '+servername,mtError,[mbAbort,mbIgnore],0)
     then
     begin
       debugOut(3, 'CheckNetTimer', 'Could not reach '+servername+' terminate.');
       Application.Terminate;
       halt;
     end;
  {$ENDIF WIN32}
  if not IBConnection1.Connected  then
  begin
    debugOut(3, 'CheckNetTimer', 'connection error');
  end;
  *)
end;


procedure TDataModule1.TimerloggedinTimer(Sender: TObject);
begin
  SQuibloggedin.Edit;
  SQuibloggedin.FieldByName('loggedin').AsDateTime := now;
  SQuibloggedin.Post;
end;

procedure TDataModule1.TimerLogoffOnTopTimer(Sender: TObject);
begin
  debugOut(8, 'TimerLogoffOnTopTimer', 'start ');
  TimerLogoffOnTop.interval := 1500;
  //SetWindowPos(FLogoff.handle, HWND_TOPMOST, 0, 0, screenx - 1, screeny - 1, SWP_NOACTIVATE);
end;

procedure TDataModule1.TimerOnTopTimer(Sender: TObject);
begin
  debugOut(8, 'TimerOnTopTimer', 'start ');
  TimerOntop.interval := 500;
  {$IFDEF WINDOWS}
  SetWindowPos(FOnTop.handle, HWND_TOPMOST, leftint, 0, ontopwidth,
    ontopheight, SWP_NOACTIVATE);
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
  FOntop.Left := leftint;
  FOnTop.Top := 0;
  FOnTop.Height := ontopheight;
  FOnTop.Width := ontopwidth;
  FOnTop.FormStyle := fsSystemStayOnTop;
  //FOnTop.ReBuildForm;
  //FOnTop.Repaint;
  try
     //myscreen.MoveFormToFocusFront(FOnTop);
    //myscreen.MoveFormToZFront(FOnTop);
    Application.BringToFront;
     debugOut(8, 'TimerOnTopTimer', 'movefront ');
  except
    debugOut(8, 'TimerOnTopTimer', 'failed: movefront ');
  end;

  Application.ProcessMessages;
  //  SetWindowPos(FOnTop.handle, HWND_TOPMOST, leftint, 0, ontopwidth,
  //    ontopheight, SWP_NOACTIVATE);
  {$ENDIF LINUX}

  // show last event in DBLCB_topten
  //if not FOnTop.ineditmode then SQuibevent.Last;
end;

procedure TDataModule1.TimerQueryLoggedInTimer(Sender: TObject);
begin
  debugOut(8, 'TimerQueryLoggedInTimer', 'start ');
  getLoggedInList(Floggedin.ListBox1.Items, False);
end;

function reportmissing(startt, stopt: TDateTime; var missinglist: TStringList;
  addNotMissing: boolean): boolean;
var
  laststartt, laststopt: TDateTime;
  //startt, stopt : TDateTime;
  // sumtime,
  //firststartt : TDatetime;
  uname, event: string;
  //starttime, stoptime : String;
  year, month, day: word;

begin
  Result := False;
  //missinglist := Tstringlist.create;
  // start looking for missing reports
  //sumtime := 0;
  //starttime :=
  //startt := now;
  //stoptime := now+1;
  //stopt := now+1;
  missinglist.Clear;
  //missinglist.Add('uibtime Missing Reports:');
  if DataModule1.TrayQuery3.Active then
    DataModule1.TrayQuery3.Close;
  DataModule1.TrayQuery3.sql.Clear;
  DataModule1.TrayQuery3.sql.Add('select * from uibevent ');
  DataModule1.TrayQuery3.sql.Add('where ');
  DataModule1.TrayQuery3.sql.Add('(userid = :uid) and ');
  DataModule1.TrayQuery3.sql.Add('(starttime >= :start) and ');
  DataModule1.TrayQuery3.sql.Add('(starttime < :stop) ');
  DataModule1.TrayQuery3.sql.Add('order by starttime ');
  DataModule1.TrayQuery3.ParamByName('uid').AsString := uid;
  DataModule1.TrayQuery3.ParamByName('start').AsDateTime := startt;
  DataModule1.TrayQuery3.ParamByName('stop').AsDateTime := stopt;
  DataModule1.TrayQuery3.Open;
  DataModule1.TrayQuery3.First;
  laststartt := DataModule1.TrayQuery3.FieldByName('starttime').AsDateTime;
  //firststartt := laststartt;
  //laststopt := DataModule1.TrayQuery3.fieldbyname('stoptime').asdatetime;
  //if not (combobox1.Text = 'Summe Alle') then
  //  DataModule1.SQuibevent.Locate('starttime;stoptime', VarArrayOf([laststartt,laststopt]), [loCaseInsensitive,loPartialKey])
  //else
  //if not DataModule1.TrayQuery4.Active then DataModule1.TrayQuery4.Open;
  //  DataModule1.TrayQuery4.Locate('userid;starttime;stoptime', VarArrayOf([uid,laststartt,laststopt]), [loCaseInsensitive,loPartialKey]);
  //DataModule1.TrayQuery3.close;
  //sumtime := laststopt - laststartt;
  //DataModule1.SQuibevent.next;
  // query for event that need a report
  DataModule1.TrayQuery1.sql.Clear;
  DataModule1.TrayQuery1.sql.Add('select event from uibaktevent where');
  DataModule1.TrayQuery1.sql.Add(' reportrequired = 1');
  DataModule1.TrayQuery1.Open;
  decodeDate(laststartt, year, month, day);
  DataModule1.TrayQuery2.sql.Clear;
  DataModule1.TrayQuery2.sql.Add('select * from UIB_WORK_DESCRIPTION where ');
  DataModule1.TrayQuery2.sql.Add('(jahr >= :year) and ');
  DataModule1.TrayQuery2.sql.Add('(monat >= :month) and ');
  DataModule1.TrayQuery2.sql.Add('(tag >= :day) and ');
  DataModule1.TrayQuery2.sql.Add('(userid = :uid) and ');
  DataModule1.TrayQuery2.sql.Add('(not (description = ""))  ');
  DataModule1.TrayQuery2.ParamByName('year').AsInteger := year;
  DataModule1.TrayQuery2.ParamByName('month').AsInteger := month;
  DataModule1.TrayQuery2.ParamByName('day').AsInteger := day;
  DataModule1.TrayQuery2.ParamByName('uid').AsString := uid;
  DataModule1.TrayQuery2.Open;

  while not DataModule1.TrayQuery3.EOF do
  begin
    uname := DataModule1.TrayQuery3.FieldByName('userid').AsString;
    startt := DataModule1.TrayQuery3.FieldByName('starttime').AsDateTime;
    event := DataModule1.TrayQuery3.FieldByName('event').AsString;
    decodeDate(startt, year, month, day);
    if DataModule1.TrayQuery1.Locate('event', event, [loCaseInsensitive]) then
    begin
      if missinglist.IndexOf(event) < 0 then
      begin
        if not DataModule1.TrayQuery2.Locate('userid;jahr;monat;tag;event',
          VarArrayOf([uname, year, month, day, event]),
          [loCaseInsensitive]) then
        begin
          missinglist.Add(event);
          Result := True;
        end;
      end;
    end;
    DataModule1.TrayQuery3.Next;
  end;
  DataModule1.TrayQuery1.Close;
  DataModule1.TrayQuery2.Close;
  DataModule1.TrayQuery3.Close;
  // end looking for missing reports
end;

procedure TDataModule1.TimerTrayIconTimer(Sender: TObject);
var
  missinglist: TStringList;
  {$IFDEF LINUX}
  hello: PNotifyNotification;
  {$ENDIF LINUX}
  exitcode: integer;
begin
  debugOut(6, 'trayicon', 'start trytimer ');
  missinglist := TStringList.Create;
  if reportmissing(date, now, missinglist, False) then
  begin
    debugOut(6, 'trayicon', 'Report missing: ' + missinglist.Text);
    {$IFDEF WINDOWS}
    TrayIcon1.BalloonHint := 'missing reports: ' + LineEnding + missinglist.Text;
    TrayIcon1.ShowBalloonHint;
    {$ENDIF WINDOWS}
    {$IFDEF LINUX}

    notify_init(argv[0]);
    hello := notify_notification_new(
      // Title
      PChar('uibtime missing reports'),
      // Content
      PChar(missinglist.Text),
      // Icon
      'dialog-information');
    // Lets display it, but we will not handle any errors ...
    notify_notification_show(hello, nil);
    // That's all folks :)
    notify_uninit;

    (*
    try
      ProcessTrayNotify.Parameters.Add('--icon="dialog-information"  "uibtime report missing:" '
                                             + missinglist.Text);
      ProcessTrayNotify.Execute;
      exitcode := ProcessTrayNotify.ExitCode;
      if exitcode <> 0 then
        debugOut(3,'trayicon', 'Exitcode starting notify-send: '+inttostr(exitcode));
    except
      debugOut(3,'trayicon', 'Exception starting notify-send ');
    end;
    *)
    {$ENDIF LINUX}
  end;
  missinglist.Free;
  debugOut(6, 'trayicon', 'stop trytimer ');
end;

procedure TDataModule1.trayconfigClick(Sender: TObject);
begin
  Fnotificationdlg.Show;
end;

procedure TDataModule1.uibtime2erpClick(Sender: TObject);
begin
  //Fuibtime2erp.Visible:=true;
  TimerOntop.Enabled := False;
  Fuibtime2erp := TFuibtime2erp.Create(self);
  Fuibtime2erp.Show;
end;

function TDataModule1.getloggedin: boolean;
begin
  getloggedin := loggedin;
end;

procedure TDataModule1.setloggedin(ishere: boolean);
var
  logtime: Tdatetime;
begin
  if ishere then
  begin
    loggedin := True;
    logtime := now;
    debugOut(6, 'setloggedin', 'true fuer ' + uid);
  end
  else
  begin
    loggedin := False;
    logtime := encodedate(1999, 1, 1);
    debugOut(6, 'setloggedin', 'false fuer ' + uid);
  end;
  if not SQuibloggedin.Active then
    DataModule1.SQuibloggedin.Open;
  debugOut(7, 'setloggedin', 'SQuibloggedin.FieldCount: ' +
    IntToStr(DataModule1.SQuibloggedin.FieldCount));
  debugOut(7, 'setloggedin', 'SQuibloggedin.active: ' +
    BoolToStr(DataModule1.SQuibloggedin.Active, True));
  debugOut(7, 'setloggedin', 'SQuibloggedin.filterd: ' +
    BoolToStr(DataModule1.SQuibloggedin.Filtered, True));
  debugOut(7, 'setloggedin', 'SQuibloggedin.filter: ' +
    DataModule1.SQuibloggedin.Filter);
  if not DataModule1.SQuibloggedin.Locate('userid', uid, [loCaseInsensitive]) then
    //if DataModule1.SQuibloggedin.RecordCount < 1 then
  begin
    try
      DataModule1.SQuibloggedin.Append;
      DataModule1.SQuibloggedin.FieldByName('userid').AsString := uid;
      DataModule1.SQuibloggedin.FieldByName('loggedin').AsDateTime := logTime;
      DataModule1.SQuibloggedin.Post;
      //DataModule1.SQuibloggedin.ApplyUpdates;
      //DataModule1.SQLTransaction1.CommitRetaining;
    except
      DataModule1.SQuibloggedin.Post;
      //DataModule1.SQuibloggedin.ApplyUpdates;
      //DataModule1.SQLTransaction1.CommitRetaining;
    end;
  end
  else
  begin
    //debugOut(5, 'SQuibloggedin field 0: ' + SQuibloggedin.Fields[0].AsString);
    //if DataModule1.SQuibloggedin.FieldByName('userid').AsString = uid then
    //begin
    DataModule1.SQuibloggedin.Edit;
    DataModule1.SQuibloggedin.FieldByName('loggedin').AsDateTime := logtime;
    DataModule1.SQuibloggedin.Post;
    //DataModule1.SQuibloggedin.ApplyUpdates;
    //DataModule1.SQLTransaction1.CommitRetaining;
    //end
    //else
    //  debugOut(5, 'setloggedin not done: ' +
    //    DataModule1.SQuibloggedin.FieldByName('userid').AsString);
  end;
  if ishere then
  begin
    Timerloggedin.Enabled := True;
  end
  else
  begin
    Timerloggedin.Enabled := False;
  end;
  //Timerloggedin.Enabled := true;
  if floggedin_created then
    FLoggedin.BtnAktualisierenClick(nil);
end;


procedure TDataModule1.Weristda1Click(Sender: TObject);
var
  logdir, logfeilname: string;
  myini: TIniFile;
begin
  try
    DataModule1.debugOut(5, 'start von DataModule1.Weristda1Click');
    // we will use logdir for logging and for configuration
    logdir := SysUtils.GetAppConfigDir(False);
    if logdir = '' then
    begin
      logdir := SysUtils.GetUserDir;
      logdir := logdir + '\uibtime';
    end;
    logdir := ExpandFileNameUTF8(logdir);
    ForceDirectories(logdir);
    logfeilname := ExpandFileNameUTF8(logdir + 'uibtime.conf');
    myini := TIniFile.Create(logfeilname);
    debugOut(5, 'Will use conf file: ' + logfeilname);
    DataModule1.debugOut(6, 'Weristda1Click', 'Will use conf file: ' + logfeilname);
    if myini = nil then
    begin
      DataModule1.debugOut(2, 'Weristda1Click',
        'myini = nil: coud not open :' + logfeilname);
      ShowMessage('Fehler in Konfigurations Datei. Bitte Log sichern. Programm wird beendet');
      Application.Terminate;
    end;
    //  Versuch zu verhindern, dass hier in 'nichts' gegriffen wird
    while not floggedin_created do
      Sleep(100);

    if Floggedin.Visible then
    begin
      FLoggedin.hide;
      Weristda1.Checked := False;
      myini.WriteBool('general', 'weristda', False);
    end
    else
    begin
      FLoggedin.Show;
      if not setwindowtoalldesktops('Presenz') then
        DataModule1.debugOut(2, 'ontop', 'failed presenz to all desktops');
      Weristda1.Checked := True;
      myini.WriteBool('general', 'weristda', True);
    end;
    myini.UpdateFile;
    myini.Free;
    DataModule1.debugOut(5, 'ende von DataModule1.Weristda1Click');
  except
    on e: Exception do
    begin
      debugOut(3, '', 'exception in DataModule1.Weristda1Click');
      debugOut(5, e.Message);
      raise;
    end;
  end;
end;

procedure TDataModule1.ZeigenurmeineProjekte1Click(Sender: TObject);
var
  logdir, logfeilname: string;
  myini: TIniFile;
begin
  // we will use logdir for logging and for configuration
  logdir := SysUtils.GetAppConfigDir(False);
  if logdir = '' then
  begin
    logdir := SysUtils.GetUserDir;
    logdir := logdir + '\uibtime';
  end;
  logdir := ExpandFileNameUTF8(logdir);
  ForceDirectories(logdir);
  logfeilname := ExpandFileNameUTF8(logdir + 'uibtime.conf');
  myini := TIniFile.Create(logfeilname);
  debugOut(5, 'Will use conf file: ' + logfeilname);
  DataModule1.debugOut(6, 'ZeigenurmeineProjekte1Click', 'Will use conf file: ' +
    logfeilname);
  if myini = nil then
  begin
    DataModule1.debugOut(2, 'ZeigenurmeineProjekte1Click',
      'myini = nil: coud not open :' + logfeilname);
    ShowMessage('Fehler in Konfigurations Datei. Bitte Log sichern. Programm wird beendet');
    Application.Terminate;
  end;
  if ZeigenurmeineProjekte1.Checked then
  begin
    ZeigenurmeineProjekte1.Checked := False;
    ontop_showallprojects := True;
    //FOntop.showallprojects := True;
    myini.WriteBool('general', 'zeigenurmeineprojekte', False);
    debugOut(6, 'showallprojects: True');
  end
  else
  begin
    ZeigenurmeineProjekte1.Checked := True;
    //FOntop.showallprojects := False;
    ontop_showallprojects := False;
    myini.WriteBool('general', 'zeigenurmeineprojekte', True);
    debugOut(6, 'showallprojects: False');
  end;
  myini.UpdateFile;
  myini.Free;
end;


procedure TDataModule1.setTimerQueryLoggedIn(millis: integer; run: boolean);
begin
  if run then
  begin
    TimerQueryLoggedIn.interval := millis;
    TimerQueryLoggedIn.Enabled := run;
  end
  else
  begin
    TimerQueryLoggedIn.Enabled := run;
  end;
end;


procedure TDataModule1.getLoggedInList(userlist: TStrings; dobeep: boolean);
var
  i: integer;
  //lserver,
  userlistcount: integer;
  //pingserver, fehlerstr: string;
  //lets_try_to_reach: boolean;
begin
  if IBConnection1.Connected then
  begin
    try
      userlistcount := userlist.Count;

      if SQLTransaction2.Active then
      begin
        SQLTransaction2.Commit;
        SQLTransaction2.StartTransaction;
        debugOut(5, 'DataModule1.getLoggedInList', 'Transaction restarted ');
      end;

      if SQqueryloggedin.active then
        SQqueryloggedin.Close;
      ///SQqueryloggedin.databasename := 'uibtime';
      SQqueryloggedin.sql.Clear;
      SQqueryloggedin.sql.add(
        'select userid from uibloggedin where loggedin > :ltime');
      SQqueryloggedin.ParamByName('ltime').AsDateTime := (now - encodetime(0, 15, 0, 0));
      debugOut(6, 'DataModule1.getLoggedInList', 'ltime: ' + DateTimeToStr(
        (now - encodetime(0, 15, 0, 0))));
      SQqueryloggedin.Open;
      userlist.Clear;
    (*
    pingserver := database1.Params.Values['SERVER NAME'];
    pingserver := copy(pingserver,1,pos(':',pingserver)-1);
    if AnsiLowerCase(pingserver) = 'c' then
     pingserver := 'local';
     *)
      debugOut(6, 'DataModule1.getLoggedInList', 'count: ' + IntToStr(
        SQqueryloggedin.RecordCount));
      for i := 1 to SQqueryloggedin.RecordCount do
      begin
        //userlist.Add(pingserver+':'+queryloggedin.fieldbyname('userid').asstring);
        userlist.Add(SQqueryloggedin.FieldByName('userid').AsString);
        debugOut(7, 'DataModule1.getLoggedInList', 'found: ' +
          SQqueryloggedin.FieldByName('userid').AsString);
        SQqueryloggedin.Next;
      end;
    (*
    for lserver:=1 to loggedinserver.count do
    begin
     if databaseLoggedin.Connected then databaseLoggedin.Close;
     databaseLoggedin.params.Clear;
     databaseLoggedin.params.add('TYPE=INTERBASE');
     databaseLoggedin.params.add('USER NAME=uibtime');
     databaseLoggedin.params.add('password=uibtime');
     databaseLoggedin.params.add('LANGDRIVER=BLLT1DE0');
     databaseLoggedin.params.add('SERVER NAME='+loggedinserver[lserver-1]);
     pingserver := loggedinserver[lserver-1];
     pingserver := copy(pingserver,1,pos(':',pingserver)-1);
     if AnsiLowerCase(pingserver) = 'c' then
      pingserver := 'local';
     if not (pingserver = 'local') then
     begin
      //TcpClient1.RemoteHost
      tcpsocke.Host := pingserver;
      hostresolved := false;
      hostconnected := false;
      try
       tcpsocke.Connect(1000);
       tcpsocke.disconnect;
       hostconnected := true;
       hostresolved := true;
      except
       on e : exception do
       begin
        fehlerstr := E.message;
        debugOut(5,'DataModule1.getLoggedInList - tcp-connect: '+fehlerstr);
        hostconnected := false;
        tcpsocke.disconnect;
       end;
      end;
      if hostconnected then
      begin
       lets_try_to_reach := true;
       //tcpsocke.disconnect;
      end
      else
      begin
       lets_try_to_reach := false;
       userlist.Add(fehlerstr);
       userlist.Add('Server '+pingserver+' nicht erreicht');
       if not hostresolved then
        userlist.Add('Servername '+pingserver+' konnte nich aufgöste werden.');
      end;
     end
     else lets_try_to_reach := true;
     if lets_try_to_reach then
     begin
      databaseloggedin.open;
      if queryloggedin.active then queryloggedin.close;
      queryloggedin.databasename := 'uibtimeloggedin';
      //queryloggedin.sql.clear;
      //queryloggedin.sql.add('select userid from uibloggedin where loggedin=1');
      queryloggedin.open;
      //userlist.Clear;
      for i:=1 to queryloggedin.recordcount do
      begin
       userlist.Add(pingserver+':'+queryloggedin.fieldbyname('userid').asstring);
       queryloggedin.next;
      end;
     end;
    end;
    *)
      /// if (userlistcount <> userlist.Count) and dobeep then
      ///   beep;
      debugOut(5, 'DataModule1.getLoggedInList', 'ende von DataModule1.getLoggedInList');
    except
      on e: Exception do
      begin
        debugOut(3, 'DataModule1.getLoggedInList',
          'exception in DataModule1.getLoggedInList');
        debugOut(3, 'Exception', e.Message);
        //raise;
      end;
    end;

  end;
end;


(*
procedure TDataModule1.writeVerinfoToLog(var feil: Textfile);
//var
//  S: string;
begin
  //version := '4.0.12';
  writeln(feil, '-----Angfang Version-Info-----');
  writeln(feil, 'uibtime Version: ' + version + ' vom ' + verDatum);
  writeln(feil, '-----Ende Version-Info-----');
end;
*)

function IncludeTrailingPathDelimiter(path: string): string;
begin
  Result := IncludeTrailingBackslash(path);
end;

function GetSpecialFolderLocation(csidl: integer): string;
begin

end;



procedure TDataModule1.DataModuleCreate(Sender: TObject);
var
  logdir, logfeilname: string;
  myini: TIniFile;
begin

  // we will use logdir for logging and for configuration
  logdir := SysUtils.GetAppConfigDir(False);
  if logdir = '' then
  begin
    logdir := SysUtils.GetUserDir;
    logdir := logdir + '\uibtime';
  end;
  logdir := ExpandFileName(logdir);
  ForceDirectories(logdir);
  logfeilname := ExpandFileNameUTF8(logdir + 'uibtime.conf');
  myini := TIniFile.Create(logfeilname);
  Fdebug.Memo1.Append('Will use conf file: ' + logfeilname);
  //DataModule1.debugOut(6, 'DataModuleCreate', 'Will use conf file: ' + logfeilname);
  if myini = nil then
  begin
    Fdebug.Memo1.Append('DataModuleCreate myini = nil: coud not open :' +
      logfeilname);
    //DataModule1.debugOut(2, 'DataModuleCreate', 'myini = nil: coud not open :'+logfeilname);
    ShowMessage('Fehler in Konfigurations Datei. Bitte Log sichern. Programm wird beendet');
    Application.Terminate;
  end;
  Autologin1.Checked := myini.ReadBool('general', 'autologin', False);
  Fdebug.Memo1.Append('autologin: ' + BoolToStr(Autologin1.Checked, True));
  zeigenurmeineprojekte1.Checked :=
    myini.ReadBool('general', 'zeigenurmeineprojekte', False);
  Fdebug.Memo1.Append('zeigenurmeineprojekte: ' + BoolToStr(
    zeigenurmeineprojekte1.Checked, True));
  if ZeigenurmeineProjekte1.Checked then
    ontop_showallprojects := False
  else
    ontop_showallprojects := True;
  ShowDebugwindow1.Checked := myini.ReadBool('general', 'showdebugwindow', False);
  Fdebug.Memo1.Append('showdebugwindow: ' + BoolToStr(ShowDebugwindow1.Checked, True));
  if ShowDebugwindow1.Checked then
  begin
    Fdebug.Visible := True;
    Fdebug.Memo1.Append('DataModuleCreate: show');
  end
  else
  begin
    Fdebug.Visible := False;
    Fdebug.Memo1.Append('DataModuleCreate: hide');
  end;
  Weristda1.Checked := myini.ReadBool('general', 'weristda', False);
  Fdebug.Memo1.Append('weristda: ' + BoolToStr(Weristda1.Checked, True));
  /// is not created right now and waiting does not work
  /// while not floggedin_created do Sleep(100);
  ///if Weristda1.Checked then FLoggedin.Visible:= true
  ///else FLoggedin.Visible:= false;

  debuglevel := myini.ReadInteger('general', 'debuglevel', 5);
  Fdebug.Memo1.Append('debuglevel: ' + IntToStr(debuglevel));
  TrayInterval := myini.ReadInteger('general', 'TrayInterval', 5);
  TimerTrayIcon.Interval := TrayInterval * 60 * 1000;
  Trayshow := myini.ReadBool('general', 'showTray', True);
  TrayIcon1.Visible := Trayshow;
  TimerTrayIcon.Enabled := Trayshow;
  myini.UpdateFile;
  myini.Free;

  // Initialize logging
  LogDatei := TLogInfo.Create;
  logfeilname := ExtractFileNameOnly(Application.ExeName);
  LogDatei.FileName := logfeilname;
  LogDatei.StandardLogFileext := '.log';
  LogDatei.StandardLogFilename := logfeilname;
  LogDatei.WritePartLog := False;
  LogDatei.WriteErrFile := False;
  LogDatei.WriteHistFile := False;
  LogDatei.LogProduktId := True;
  LogDatei.LogLevel := debuglevel;
  LogDatei.StandardLogPath := logdir;
  LogDatei.StandardMainLogPath := logdir;

  //LogDatei.StandardPartLogFilename := lfilename+ '-part';
  LogDatei.CreateTheLogfile(logfeilname + '.log', True);
  LogDatei.log(DateTimeToStr(Now) + ': uibtime started', LLessential);
  LogDatei.log('uibtime version: ' + version, LLessential);

  (*
  logfeilname := ExpandFileNameUTF8(logdir + '\uibtime.log');
  AssignFile(logfeil, logfeilname);
  Rewrite(logfeil);
  writeln(logfeil, DateTimeToStr(Now) + ': uibtime started');
  writeVerinfoToLog(logfeil);
  writeln(logfeil, Fdebug.Memo1.Lines.Text);
  closeFile(logfeil);
  *)
  Application.OnException := CustomExceptionHandler;
  Application.OnEndSession := OnEndsession;
  // Initialize Font
  myFont := 'Arial';
  {$IFDEF LINUX}
  //myFont := 'Liberation Sans Narrow';
  myFont := 'Liberation Sans';
  {$ENDIF LINUX}

end;

procedure TDataModule1.TerminateApplication;
var
  logdir, logfeilname: string;
  myini: TIniFile;
begin
  logdir := SysUtils.GetAppConfigDir(False);
  if logdir = '' then
  begin
    logdir := SysUtils.GetUserDir;
    logdir := logdir + '\uibtime';
  end;
  logdir := ExpandFileNameUTF8(logdir);
  ForceDirectories(logdir);
  logfeilname := ExpandFileNameUTF8(logdir + '\uibtime.conf');
  myini := TIniFile.Create(logfeilname);
  debugOut(5, 'Will use conf file: ' + logfeilname);
  DataModule1.debugOut(6, 'TerminateApplication', 'Will use conf file: ' + logfeilname);
  if myini = nil then
  begin
    DataModule1.debugOut(2, 'TerminateApplication',
      'myini = nil: coud not open :' + logfeilname);
    ShowMessage('Fehler in Konfigurations Datei. Bitte Log sichern. Programm wird beendet');
    //Application.Terminate;
  end;
  myini.WriteBool('general', 'autologin', Autologin1.Checked);
  myini.WriteBool('general', 'zeigenurmeineprojekte', zeigenurmeineprojekte1.Checked);
  myini.WriteBool('general', 'showdebugwindow', ShowDebugwindow1.Checked);
  myini.WriteInteger('general', 'debuglevel', LogDatei.LogLevel);
  myini.UpdateFile;
  myini.Free;
  try
    debugOut(5, 'Will free components ');
    if Assigned(FLoggedin) then
      FLoggedin.Free;
    //if Assigned(FOnTop) then FOnTop.Free;
    if Assigned(FLogoff) then
      FLogoff.Free;
    if Assigned(FNachfrage) then
      FNachfrage.Free;
    if Assigned(fwork_description) then
      fwork_description.Free;
    if Assigned(FTreeview) then
      FTreeview.Free;
    if Assigned(FDataedit) then
      FDataedit.Free;
    if Assigned(FStatistik) then
      FStatistik.Free;
    {$IFDEF WINDOWS}
    debugOut(5, 'Will stop httpserver .... ');
    if Assigned(httpservice.FServerThread) then
      stophttpserver;
    {$ENDIF WINDOWS}
    //if Assigned(FOnTop) then FOnTop.Close;
    //FOnTop.Destroy;
    //if Assigned(FOnTop) then FOnTop.Close;
    debugOut(5, 'Finished free components ');
  finally
    debugOut(5, 'Will terminate .... ');
    LogDatei.Close;
    Application.Terminate;
    //debugOut(5, 'Will halt .... ');

    halt;
  end;
end;

function TDataModule1.isValidEvent(event: string): boolean;
begin
  Result := SQuibaktevent.Locate('event', event, [loCaseInsensitive]);
end;

function TDataModule1.dateIsHolyday(mydate: TdateTime): boolean;
var
  rcount: integer;
  myeof: boolean;
  filterstr: string;
begin
  Result := False;
  filterstr := 'DTOS(holydate) = "' + formatdatetime('yyyymmdd', mydate) + '"';
  SQholydays.Filtered := False;
  SQholydays.Filter := filterstr;
  SQholydays.Filtered := True;
  rcount := SQholydays.RecordCount;
  myeof := SQholydays.EOF;
  if not myeof then
    Result := True;
  SQholydays.Filtered := False;
end;

procedure CatchUnhandledException(Obj: TObject; Addr: Pointer;
  FrameCount: longint; Frames: PPointer);
var
  Message: string;
  i: longint;
  hstdout: ^Text;
begin
  hstdout := @stdout;
  Writeln(hstdout^, 'An unhandled exception occurred at $',
    HexStr(PtrUInt(Addr), SizeOf(PtrUInt) * 2), ' :');
  if Obj is Exception then
  begin
    Message := Exception(Obj).ClassName + ' : ' + Exception(Obj).Message;
    Writeln(hstdout^, Message);
  end
  else
    Writeln(hstdout^, 'Exception object ', Obj.ClassName, ' is not of class Exception.');
  Writeln(hstdout^, BackTraceStrFunc(Addr));
  if (FrameCount > 0) then
  begin
    for i := 0 to FrameCount - 1 do
      Writeln(hstdout^, BackTraceStrFunc(Frames[i]));
  end;
  Writeln(hstdout^, '');
end;

procedure TDataModule1.DumpExceptionCallStack(E: Exception);
var
  I: integer;
  Frames: PPointer;
  Report, Rep2: string;
  Replist: TStringList;
begin
  //debugOut(2, 'Exception', 'Message: ' + E.Message);
  Replist := TStringList.Create;
  Replist.Add('Program exception! ');
  Replist.Add('Stacktrace:');
  if E <> nil then
  begin
    Replist.Add('Exception class: ' + E.ClassName);
    Replist.Add('Message: ' + E.Message);
  end;
  Replist.Add(BackTraceStrFunc(ExceptAddr));
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    Replist.Add(BackTraceStrFunc(Frames[I]));
  debugOutList(2, 'Exception', Replist);
  //ShowMessage(Report);
  (*
  Rep2 := 'uibtime Exception class: ' + E.ClassName + ' , ' +
    'Message: ' + E.Message + ' , ' + ' Bitte Logdatei sichern: ' +
    LogDatei.FileName + ' - Eventuell Programmabbruch.';
  MessageDlg('uibtime: Fehler', Rep2, mtError, [mbClose], 0);
  *)
  //    'Keine Netzwerkverbindung zum DB-Server. Programmabbruch ', mtError, [mbAbort], 0);
  //Application.Terminate;
  //Halt; // End of program execution
end;

procedure TDataModule1.CustomExceptionHandler(Sender: TObject; E: Exception);
var
  retries: integer;
  outstr: string;
begin
  (*
  if E.ClassType = EIBDatabaseError then
  begin
    retries := 0;
    if pos('Unable to complete network request',E.Message) > 0 then
      begin
        while retries < 10 do
        begin
          debugOut(2,'Exception', 'Unable to complete network request: retry: '+inttostr(retries));
          inc(retries);
          sleep(1000);
          Flogin.BitBtnOkClick(sender);
          Application.ProcessMessages;
        end;
      end
    end;
  if not IBConnection1.Connected then
    begin
    *)
  DumpExceptionCallStack(E);
  if E.ClassType = EConvertError then
  begin
    outstr := E.Message;
    DataModule1.debugOut(1, 'Exception', outstr);
    MessageDlg('uibtime: Fehler', outstr, mtError, [mbClose], 0);
  end
  else if (E.ClassType = EDatabaseError) and
    (pos('Must apply updates before refreshing data', E.Message) > 0) then
  begin
    outstr := E.Message;
    DataModule1.debugOut(1, 'Exception', outstr);
    MessageDlg('uibtime: Fehler', outstr, mtError, [mbClose], 0);
  end
  else if (E.ClassType = EDBEditError) then
  begin
    outstr := 'Ein Eingabefehler in einer Maske: ' + E.Message;
    DataModule1.debugOut(1, 'Exception', outstr);
    MessageDlg('uibtime: Fehler', outstr, mtError, [mbClose], 0);
  end
  else
  begin
    outstr := 'uibtime Exception class: ' + E.ClassName + ' , ' +
      'Message: ' + E.Message + ' , ' + LineEnding +
      ' Sollte das Programm sich beenden, so bitte Logdatei sichern: ' +
      LogDatei.FileName;
    MessageDlg('uibtime: Fehler', outstr, mtError, [mbClose], 0);
    //Application.ShowException(E);
    if not IBConnection1.Connected then
      TerminateApplication;
  end;
end;

procedure TDataModule1.OnEndSession(Sender: TObject);
begin
  if Assigned(logdatei) then
    LogDatei.log('Terminating: onendsession', LLessential);
end;

{ from https://stackoverflow.com/questions/10588660/font-consistency-throughout-project
  modified for font.name only }
procedure TDataModule1.SetFontName(Control: TControl; Name: string);
// Set font properties
var
  Index: integer;
  Font: TFont;
  AnObject: TObject;
  ChildControl: TControl;
begin
  // Set font properties
  try
    AnObject := GetObjectProp(Control, 'Font', TControl);
    if AnObject is TFont then
    begin
      // Set properties
      Font := TFont(AnObject);
      Font.Name := Name;
    end;

  except
  end;

  // Set child font properties
  if Control is TWinControl then
  begin
    // Set
    for Index := 0 to TWinControl(Control).ControlCount - 1 do
    begin
      // Child control
      ChildControl := TWinControl(Control).Controls[Index];

      // Set font properties
      SetFontName(ChildControl, Name);
    end;
  end;
end;


initialization
  { initialization-Abschnitt }
  DefaultFormatSettings.ShortDateFormat := 'dd.mm.yyyy';
  ontopwidth := 850;//730;
  ontopheight := 28;
  {$ifdef LINUX}
  ontopheight := round(32 * (96 / screen.PixelsPerInch));
  {$ENDIF LINUX}
  //ontopheight := 50;
  scalefactor := 96 / screen.PixelsPerInch;
  screenx := Screen.Width;
  screeny := Screen.Height;
  leftint := (screenx - ontopwidth) div 2;
  loggedin := False;
  floggedin_created := False;
  // DataModule1.loggedinserver := TStringList.Create;
  ///vi := GetVersionInfoRec(Application.ExeName);
  ///version := vi.FileVersion;
  //version := '4.0.12';
  //debuglevel := 5;

  verDatum := DateToStr(FileDateToDateTime(FileAge(ParamStr(0))));
  //from http://wiki.freepascal.org/Show_Application_Title,_Version,_and_Company
  FileVerInfo := TFileVersionInfo.Create(nil);
  try
    FileVerInfo.FileName := ParamStr(0);
    FileVerInfo.ReadFileInfo;
    version := FileVerInfo.VersionStrings.Values['FileVersion'];
    (*
    writeln('Company: ',FileVerInfo.VersionStrings.Values['CompanyName']);
    writeln('File description: ',FileVerInfo.VersionStrings.Values['FileDescription']);
    writeln('File version: ',FileVerInfo.VersionStrings.Values['FileVersion']);
    writeln('Internal name: ',FileVerInfo.VersionStrings.Values['InternalName']);
    writeln('Legal copyright: ',FileVerInfo.VersionStrings.Values['LegalCopyright']);
    writeln('Original filename: ',FileVerInfo.VersionStrings.Values['OriginalFilename']);
    writeln('Product name: ',FileVerInfo.VersionStrings.Values['ProductName']);
    writeln('Product version: ',FileVerInfo.VersionStrings.Values['ProductVersion']);
    *)
    myscreen := TScreen.Create(Application.Owner);
  finally
    FileVerInfo.Free;
  end;
  //WinstVersion := '4.11.6.1';

finalization
  { finalization-Abschnitt }
  begin
    myscreen.Free;
  end;

end.
