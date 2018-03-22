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
{$ENDIF LINUX}
  {$IFDEF WINDOWS}
  winpeimagereader, {need this for reading exe info}
  Windows,
  {$ENDIF WINDOWS}
  Classes, SysUtils, IBConnection, sqldb, DB,
  //FileUtil,
  LazFileUtils,
  Menus, ExtCtrls,
  ///registry,
  Forms, Controls, Dialogs, IniFiles, process, DateUtils,
  runprocess,
  httpservice,
  uibtWorkRepChooser,
  uib2erp,
  Variants;

type

  { TDataModule1 }

  TDataModule1 = class(TDataModule)
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
    procedure Import1Click(Sender: TObject);
    procedure Info1Click(Sender: TObject);
    procedure LeisteNeuAufbauen1Click(Sender: TObject);
    procedure PopupMenu1Close(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure queryAfterHelper(myevent:string; myquery : TSQLQuery; DataSet: TDataSet);
    procedure Query4ResultAfterDelete(DataSet: TDataSet);
    procedure Query4ResultAfterPost(DataSet: TDataSet);
    procedure schmaleLeisteClick(Sender: TObject);
    procedure ShowDebugWindow1Click(Sender: TObject);
    procedure SQholydaysAfterDelete(DataSet: TDataSet);
    procedure SQholydaysAfterInsert(DataSet: TDataSet);
    procedure SQholydaysAfterPost(DataSet: TDataSet);
    procedure SQuibakteventAfterDelete(DataSet: TDataSet);
    procedure SQuibakteventAfterInsert(DataSet: TDataSet);
    procedure SQuibakteventAfterPost(DataSet: TDataSet);
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
    procedure SQwork_descriptionPostError(DataSet: TDataSet; E: EDatabaseError;
      var DataAction: TDataAction);
    procedure Statistik1Click(Sender: TObject);
    procedure Statistik1Cancel;
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
    procedure debugOut(level: integer; meldung: string); overload ;
    procedure debugOut(level: integer; source: string; meldung: string); overload ;
    function getshowallprojects(): boolean;
    function getdebuglevel(): integer;
    procedure setdebuglevel(newlevel: integer);
    procedure writeVerinfoToLog(var feil: Textfile);
    procedure TerminateApplication;
    function isValidEvent(event:string): boolean;

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
  user_h_per_day : double;
  user_work_days : set of byte;
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
  verDatum : string;
  Trayshow : boolean;
  TrayInterval : cardinal;


implementation

{$R *.lfm}


uses ontop, login, debug, logoff, dataedit,
  loggedin_, statistik,work_description, nachf,
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
  getdebuglevel := debuglevel;
end;

procedure TDataModule1.setdebuglevel(newlevel: integer);
var
  logdir, logfeilname: string;
  myini: TIniFile;
begin
  debuglevel := newlevel;
  // we will use logdir for logging and for configuration
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
  myini.WriteInteger('general', 'debuglevel', debuglevel);
  myini.UpdateFile;
  myini.Destroy;
  debugOut(6, 'debuglevel now: '+IntToStr(debuglevel));
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
  debugOut(6,'ontop_showallprojects: '+BoolToStr(ontop_showallprojects,true));
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

procedure TDataModule1.debugOut(level: integer; source: string; meldung: string);
var
  sourcestr, logstr, logdir,logfeilname : string;
begin
  try
    if source <> '' then sourcestr := '['+source+'] '
    else sourcestr := '';
    if level <= debuglevel then
    begin
      logstr := '['+IntToStr(level)+'] ['+DateTimeToStr(Now) + '] '+ sourcestr + meldung;
      logdir := SysUtils.GetAppConfigDir(False);
      if logdir = '' then
      begin
        logdir := SysUtils.GetUserDir;
        logdir := logdir + '\uibtime';
      end;
      logfeilname := ExpandFileNameUTF8(logdir + '\uibtime.log');
      AssignFile(logfeil, logfeilname);
      append(logfeil);
      writeln(logfeil,logstr);
      flush(logfeil);
      Close(logfeil);
      if FDebug <> nil then
        FDebug.memo1.Lines.add(logstr);
    end;
  except
  end;
end;

procedure TDataModule1.Dateneditieren1Click(Sender: TObject);
begin
  FDataedit := TFDataedit.Create(self);
  FOntop.ineditmode := True;
  //FDataedit.showmodal;
  //FDataedit.FormStyle:=fsNormal;
  //FOntop.Enabled := False;
  FDataedit.Show;
  //FDataedit.free;
  //Datamodule1.SQuibevent.last;
  //ontop.lastevent := Datamodule1.SQuibevent.fieldbyname('event').asstring;
  //FOntop.ineditmode := false;
  //FOntop.eventhandler(ontop.lastevent);
end;

procedure TDataModule1.Dateneditieren1Cancel;
begin
  //FStatistik.free;
  Datamodule1.SQuibevent.last;
  ontop.lastevent := Datamodule1.SQuibevent.FieldByName('event').AsString;
  FOntop.Enabled := True;
  FOntop.ineditmode := False;
  FOntop.eventhandler(ontop.lastevent);
  TimerOntop.Enabled := True;
end;


procedure TDataModule1.Autologin1Click(Sender: TObject);
var
  logdir,logfeilname: string;
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
  logfeilname := ExpandFileNameUTF8(logdir + '\uibtime.conf');
  myini := TIniFile.Create(logfeilname);
  debugOut(5, 'Will use conf file: ' + logfeilname);
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
  myini.Destroy;
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
  TimerOnTop.Enabled:=true;
end;

procedure TDataModule1.PopupMenu1Popup(Sender: TObject);
begin
  TimerOnTop.Enabled:=false;
end;

procedure TDataModule1.queryAfterHelper(myevent:string; myquery : TSQLQuery; DataSet: TDataSet);
begin
  try
    if (DataSet.UpdateStatus in [usModified,usInserted,usDeleted])
       or (lowercase(myevent) = 'afterdelete') then
    begin
      myquery.ApplyUpdates;
      debugOut(5,DataSet.Name+myevent,'start  '+ DataSet.Name+myevent+' (apply)');
    end;
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      debugOut(5,DataSet.Name+myevent,  'start  '+ DataSet.Name+myevent+' (commit/start)');
    end
    else debugOut(3,DataSet.Name+myevent,  'Error: Missing Transaction '+ DataSet.Name+myevent+' ');
  except
    debugOut(2,DataSet.Name+myevent,  'exception in '+ DataSet.Name+myevent+' (commit)');
  end;
end;



procedure TDataModule1.Query4ResultAfterDelete(DataSet: TDataSet);
var
  myevent : string;
  myquery : TSQLQuery;
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
  myevent : string;
  myquery : TSQLQuery;
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
  logdir,logfeilname: string;
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
  logfeilname := ExpandFileNameUTF8(logdir + '\uibtime.conf');
  myini := TIniFile.Create(logfeilname);
  debugOut(5, 'Will use conf file: ' + logfeilname);
  if ShowDebugWindow1.Checked then
  begin
    ShowDebugWindow1.Checked := false;
    FDebug.Visible:= false;
  end
  else
  begin
    ShowDebugWindow1.Checked := true;
    FDebug.Visible:= true;
  end;
  myini.WriteBool('general', 'ShowDebugWindow', ShowDebugWindow1.Checked);
  debugOut(6, 'ShowDebugWindow: '+BoolToStr(ShowDebugWindow1.Checked,true));
  myini.UpdateFile;
  myini.Destroy;
end;

procedure TDataModule1.SQholydaysAfterDelete(DataSet: TDataSet);
begin
    debugOut(5, 'start  '+ DataSet.Name+' AfterDelete');
  try
    // after delete the state is not edit or insert but we have to apply
    //if DataModule1.SQuibevent.State in [dsInsert, dsEdit] then
      DataModule1.SQuibaktevent.ApplyUpdates;
      debugOut(5, 'start  '+ DataSet.Name+' AfterDelete (apply)');
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      debugOut(5, 'start  '+ DataSet.Name+' AfterDelete (commit/start)');
    end
    else debugOut(3,'SQholydaysAfterDelete', 'Error: Missing Transaction '+ DataSet.Name+' AfterDelete');
  except
    debugOut(2,'SQholydaysAfterDelete', 'exception in '+ DataSet.Name+' AfterDelete (commit)');
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
    else debugOut(3,'SQholydaysAfterInsert', 'Error: Missing Transaction SQuibakteventAfterInsert');
    debugOut(5, 'Commit in SQholydaysAfterInsert');
  except
    debugOut(2,'SQholydaysAfterInsert', 'exception in SQuibakteventAfterInsert (commit)');
  end;
end;

procedure TDataModule1.SQholydaysAfterPost(DataSet: TDataSet);
begin
  try
    //if DataModule1.SQuibaktevent.State in [dsInsert, dsEdit] then
    if DataSet.UpdateStatus in [usModified,usInserted,usDeleted] then
    begin
      DataModule1.SQholydays.ApplyUpdates;
      debugOut(5,'SQholydaysAfterPost', 'start  SQholydaysAfterPost (apply)');
    end;
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      //SQLTransaction1.Commit;
      //SQLTransaction1.StartTransaction;
    end
    else debugOut(3,'SQholydaysAfterPost', 'Error: Missing Transaction SQuibakteventAfterPost');
    debugOut(5, 'Commit in SQholydaysAfterPost');
  except
    debugOut(2,'SQholydaysAfterPost', 'exception in SQholydaysAfterPost (commit)');
  end;
end;

procedure TDataModule1.SQuibakteventAfterDelete(DataSet: TDataSet);
begin
  debugOut(5, 'start  '+ DataSet.Name+' AfterDelete');
  try
    // after delete the state is not edit or insert but we have to apply
    //if DataModule1.SQuibevent.State in [dsInsert, dsEdit] then
      DataModule1.SQuibaktevent.ApplyUpdates;
      debugOut(5, 'start  '+ DataSet.Name+' AfterDelete (apply)');
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      debugOut(5, 'start  '+ DataSet.Name+' AfterDelete (commit/start)');
    end
    else debugOut(3,'SQuibakteventAfterDelete', 'Error: Missing Transaction '+ DataSet.Name+' AfterDelete');
  except
    debugOut(2,'SQuibakteventAfterDelete', 'exception in '+ DataSet.Name+' AfterDelete (commit)');
  end;
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
    else debugOut(3,'SQuibakteventAfterInsert', 'Error: Missing Transaction SQuibakteventAfterInsert');
    debugOut(5, 'Commit in SQuibakteventAfterInsert');
  except
    debugOut(2,'SQuibakteventAfterInsert', 'exception in SQuibakteventAfterInsert (commit)');
  end;
end;

procedure TDataModule1.SQuibakteventAfterPost(DataSet: TDataSet);
begin
  try
    //if DataModule1.SQuibaktevent.State in [dsInsert, dsEdit] then
    if DataSet.UpdateStatus in [usModified,usInserted,usDeleted] then
    begin
      DataModule1.SQuibaktevent.ApplyUpdates;
      debugOut(5,'SQuibakteventAfterPost', 'start  SQuibakteventAfterPost (apply)');
    end;
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      //SQLTransaction1.Commit;
      //SQLTransaction1.StartTransaction;
    end
    else debugOut(3,'SQuibakteventAfterPost', 'Error: Missing Transaction SQuibakteventAfterPost');
    debugOut(5, 'Commit in SQuibakteventAfterPost');
  except
    debugOut(2,'SQuibakteventAfterPost', 'exception in SQuibakteventAfterPost (commit)');
  end;
end;

procedure TDataModule1.SQuibaktuserAfterDelete(DataSet: TDataSet);
begin
  debugOut(5, 'start  '+ DataSet.Name+' AfterDelete');
  try
    // after delete the state is not edit or insert but we have to apply
    //if DataModule1.SQuibevent.State in [dsInsert, dsEdit] then
      DataModule1.SQuibaktuser.ApplyUpdates;
      debugOut(5,'SQuibaktuserAfterDelete', 'start  '+ DataSet.Name+' AfterDelete (apply)');
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      debugOut(5,'SQuibaktuserAfterDelete',  'start  '+ DataSet.Name+' AfterDelete (commit/start)');
    end
    else debugOut(3,'SQuibaktuserAfterDelete',  'Error: Missing Transaction '+ DataSet.Name+' AfterDelete');
  except
    debugOut(2,'SQuibaktuserAfterDelete',  'exception in '+ DataSet.Name+' AfterDelete (commit)');
  end;
end;

procedure TDataModule1.SQuibaktuserAfterPost(DataSet: TDataSet);
begin
  try
    //if DataModule1.SQuibaktuser.State in [dsInsert, dsEdit] then
    if DataSet.UpdateStatus in [usModified,usInserted,usDeleted] then
    begin
      DataModule1.SQuibaktuser.ApplyUpdates;
      debugOut(5,'SQuibaktuserAfterPost', 'start  SQuibaktuserAfterPost (apply)');
    end;
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      //SQLTransaction1.Commit;
      //SQLTransaction1.StartTransaction;
    end
    else debugOut(3, 'Error: Missing Transaction SQuibaktuserAfterPost');
    debugOut(5,'SQuibaktuserAfterPost', 'Commit in SQuibaktuserAfterPost');
  except
    debugOut(2,'SQuibaktuserAfterPost', 'exception in SQuibaktuserAfterPost (commit)');
  end;
end;

procedure TDataModule1.SQuiballeventAfterPost(DataSet: TDataSet);
begin
  try
    //if DataModule1.SQuiballevent.State in [dsInsert, dsEdit] then
    if DataSet.UpdateStatus in [usModified,usInserted,usDeleted] then
    begin
      DataModule1.SQuiballevent.ApplyUpdates;
      debugOut(5,'SQuiballeventAfterPost', 'start  SQuiballeventAfterPost (apply)');
    end;
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      //SQLTransaction1.Commit;
      //SQLTransaction1.StartTransaction;
    end
    else debugOut(3, 'Error: Missing Transaction SQuiballeventAfterPost');
    debugOut(5,'SQuiballeventAfterPost',  'Commit in SQuiballeventAfterPost');
  except
    debugOut(2,'SQuiballeventAfterPost',  'exception in SQuiballeventAfterPost (commit)');
  end;
end;

procedure TDataModule1.SQuibdefprojAfterDelete(DataSet: TDataSet);
begin
  debugOut(5, 'start  '+ DataSet.Name+' AfterDelete');
  try
    // after delete the state is not edit or insert but we have to apply
      DataModule1.SQuibdefproj.ApplyUpdates;
      debugOut(5,'SQuibdefprojAfterDelete', 'start  '+ DataSet.Name+' AfterDelete (apply)');
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      debugOut(5,'SQuibdefprojAfterDelete',  'start  '+ DataSet.Name+' AfterDelete (commit/start)');
    end
    else debugOut(3,'SQuibdefprojAfterDelete',  'Error: Missing Transaction '+ DataSet.Name+' AfterDelete');
  except
    debugOut(2,'SQuibdefprojAfterDelete',  'exception in '+ DataSet.Name+' AfterDelete (commit)');
  end;
end;

procedure TDataModule1.SQuibdefprojAfterPost(DataSet: TDataSet);
begin
  try
    //if DataModule1.SQuibdefproj.State in [dsInsert, dsEdit] then
    if DataSet.UpdateStatus in [usModified,usInserted,usDeleted] then
    begin
      DataModule1.SQuibdefproj.ApplyUpdates;
      debugOut(5,'SQuibdefprojAfterPost', 'start  SQuibdefprojAfterPost (apply)');
    end;
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      //SQLTransaction1.Commit;
      //SQLTransaction1.StartTransaction;
    end
    else debugOut(3,'SQuibdefprojAfterPost',  'Error: Missing Transaction SQuibdefprojAfterPost');
    debugOut(5, 'Commit in SQuibdefprojAfterPost');
  except
    debugOut(2,'SQuibdefprojAfterPost', 'exception in SQuibdefprojAfterPost (commit)');
  end;
end;

procedure TDataModule1.SQuibeventAfterDelete(DataSet: TDataSet);
begin
  debugOut(5,'SQuibeventAfterDelete', 'start  '+ DataSet.Name+' AfterDelete');
  try
    // after delete the state is not edit or insert but we have to apply
    //if DataModule1.SQuibevent.State in [dsInsert, dsEdit] then
      DataModule1.SQuibevent.ApplyUpdates;
      debugOut(5, 'start  '+ DataSet.Name+' AfterDelete (apply)');
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      debugOut(5,'SQuibeventAfterDelete',  'start  '+ DataSet.Name+' AfterDelete (commit/start)');
    end
    else debugOut(3,'SQuibeventAfterDelete',  'Error: Missing Transaction '+ DataSet.Name+' AfterDelete');
  except
    debugOut(2,'SQuibeventAfterDelete',  'exception in '+ DataSet.Name+' AfterDelete (commit)');
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
  debugOut(5,'SQuibeventAfterPost', 'start  SQuibeventAfterPost');
  try
    //if DataModule1.SQuibevent.State in [dsInsert, dsEdit] then
    if DataSet.UpdateStatus in [usModified,usInserted,usDeleted] then
    begin
      DataModule1.SQuibevent.ApplyUpdates;
      debugOut(5,'SQuibeventAfterPost',  'start  SQuibeventAfterPost (apply)');
    end;
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      //SQLTransaction1.Commit;
      //SQLTransaction1.StartTransaction;
      debugOut(5,'SQuibeventAfterPost',  'start  SQuibeventAfterPost (commit/start)');
    end
    else debugOut(3,'SQuibeventAfterPost',  'Error: Missing Transaction SQuibeventAfterPost');
  except
    debugOut(2,'SQuibeventAfterPost', 'exception in SQuibeventAfterPost (commit)');
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
  ShowMessage('Beim Schreiben des Datensatzes in uibevent ist ein Fehler aufgetreten: '+ LineEnding
               + E.Message + LineEnding
               +'Führe jetzt Cancel auf Datensatz aus.'+ LineEnding
               +'Bitte Datensatz kontrollieren.');
  DataSet.Cancel;
  DataAction:=daAbort;
  debugOut(3,'SQuibeventPostError', E.Message);
end;

procedure TDataModule1.SQuibloggedinAfterDelete(DataSet: TDataSet);
begin
  debugOut(5,'SQuibloggedinAfterDelete', 'start  '+ DataSet.Name+' AfterDelete');
  try
    // after delete the state is not edit or insert but we have to apply
    //if DataModule1.SQuibevent.State in [dsInsert, dsEdit] then
      DataModule1.SQuibloggedin.ApplyUpdates;
      debugOut(5, 'start  '+ DataSet.Name+' AfterDelete (apply)');
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      debugOut(5,'SQuibloggedinAfterDelete',  'start  '+ DataSet.Name+' AfterDelete (commit/start)');
    end
    else debugOut(3,'SQuibloggedinAfterDelete',  'Error: Missing Transaction '+ DataSet.Name+' AfterDelete');
  except
    debugOut(2,'SQuibloggedinAfterDelete', 'exception in '+ DataSet.Name+' AfterDelete (commit)');
  end;
end;

procedure TDataModule1.SQuibloggedinAfterPost(DataSet: TDataSet);
begin
  debugOut(5,'SQuibloggedinAfterPost', 'start ');
  try
    if DataSet.UpdateStatus in [usModified,usInserted,usDeleted] then
    begin
      debugOut(7,'SQuibloggedinAfterPost', ' ApplyUpdates ....');
      DataModule1.SQuibloggedin.ApplyUpdates;
      debugOut(6,'SQuibloggedinAfterPost', ' ApplyUpdates done');
    end;
  except
    debugOut(2,'SQuibloggedinAfterPost', 'exception in ApplyUpdates');
  end;
  try
    if SQLTransaction1.Active then
    begin
      debugOut(7,'SQuibloggedinAfterPost', ' CommitRetaining ....');
      SQLTransaction1.CommitRetaining;
      debugOut(6,'SQuibloggedinAfterPost', ' CommitRetaining done');
    end
    else debugOut(3,'SQuibloggedinAfterPost',  'Error: Missing Transaction');
  except
    debugOut(2,'SQuibloggedinAfterPost', 'exception in CommitRetaining');
  end;
  debugOut(5,'SQuibloggedinAfterPost',  'end ');
end;

procedure TDataModule1.SQuibsollAfterDelete(DataSet: TDataSet);
begin
  debugOut(5,'SQuibsollAfterDelete', 'start  '+ DataSet.Name+' AfterDelete');
  try
    // after delete the state is not edit or insert but we have to apply
      DataModule1.SQuibsoll.ApplyUpdates;
      debugOut(5,'SQuibsollAfterDelete',  'start  '+ DataSet.Name+' AfterDelete (apply)');
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      debugOut(5,'SQuibsollAfterDelete',  'start  '+ DataSet.Name+' AfterDelete (commit/start)');
    end
    else debugOut(3,'SQuibsollAfterDelete',  'Error: Missing Transaction '+ DataSet.Name+' AfterDelete');
  except
    debugOut(2,'SQuibsollAfterDelete', 'exception in '+ DataSet.Name+' AfterDelete (commit)');
  end;
end;

procedure TDataModule1.SQuibsollAfterPost(DataSet: TDataSet);
begin
  try
    //if DataModule1.SQuibsoll.State in [dsInsert, dsEdit] then
    if DataSet.UpdateStatus in [usModified,usInserted,usDeleted] then
    begin
      DataModule1.SQuibsoll.ApplyUpdates;
      debugOut(5,'SQuibsollAfterPost', 'start  SQuibsollAfterPost (apply)');
    end;
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      //SQLTransaction1.Commit;
      //SQLTransaction1.StartTransaction;
    end
    else debugOut(3, 'Error: Missing Transaction SQuibsollAfterPost');
    debugOut(5,'SQuibsollAfterPost',  'StartTransaction in SQuibsollAfterPost: ');
  except
    debugOut(2,'SQuibsollAfterPost', 'exception in SQuibsollAfterPost (starttransaction)');
  end;
end;

procedure TDataModule1.SQuibtimeoutAfterDelete(DataSet: TDataSet);
begin
  debugOut(5,'SQuibtimeoutAfterDelete', 'start  '+ DataSet.Name+' AfterDelete');
  try
    // after delete the state is not edit or insert but we have to apply
      DataModule1.SQuibtimeout.ApplyUpdates;
      debugOut(5,'SQuibtimeoutAfterDelete',  'start  '+ DataSet.Name+' AfterDelete (apply)');
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      debugOut(5,'SQuibtimeoutAfterDelete',  'start  '+ DataSet.Name+' AfterDelete (commit/start)');
    end
    else debugOut(3,'SQuibtimeoutAfterDelete',  'Error: Missing Transaction '+ DataSet.Name+' AfterDelete');
  except
    debugOut(2,'SQuibtimeoutAfterDelete', 'exception in '+ DataSet.Name+' AfterDelete (commit)');
  end;
end;

procedure TDataModule1.SQuibtimeoutAfterPost(DataSet: TDataSet);
begin
  try
    //if DataModule1.SQuibtimeout.State in [dsInsert, dsEdit] then
    if DataSet.UpdateStatus in [usModified,usInserted,usDeleted] then
    begin
      DataModule1.SQuibtimeout.ApplyUpdates;
      debugOut(5,'SQuibtimeoutAfterPost', 'start  SQuibtimeoutAfterPost (apply)');
    end;
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      //SQLTransaction1.Commit;
      //SQLTransaction1.StartTransaction;
    end
    else debugOut(3, 'Error: Missing Transaction SQuibtimeoutAfterPost');
    debugOut(5,'SQuibtimeoutAfterPost',  'StartTransaction in SQuibtimeoutAfterPost: ');
  except
    debugOut(2,'SQuibtimeoutAfterPost', 'exception in SQuibtimeoutAfterPost (starttransaction)');
  end;
end;

procedure TDataModule1.SQuibusereventAfterDelete(DataSet: TDataSet);
begin
  debugOut(5,'SQuibusereventAfterDelete', 'start  '+ DataSet.Name+' AfterDelete');
  try
    // after delete the state is not edit or insert but we have to apply
    //if DataModule1.SQuibevent.State in [dsInsert, dsEdit] then
      DataModule1.SQuibuserevent.ApplyUpdates;
      debugOut(5,'SQuibusereventAfterDelete',  'start  '+ DataSet.Name+' AfterDelete (apply)');
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      debugOut(5,'SQuibusereventAfterDelete',  'start  '+ DataSet.Name+' AfterDelete (commit/start)');
    end
    else debugOut(3,'SQuibusereventAfterDelete',  'Error: Missing Transaction '+ DataSet.Name+' AfterDelete');
  except
    debugOut(2,'SQuibusereventAfterDelete', 'exception in '+ DataSet.Name+' AfterDelete (commit)');
  end;
end;

procedure TDataModule1.SQuibusereventAfterPost(DataSet: TDataSet);
begin
  try
    //if DataModule1.SQuibuserevent.State in [dsInsert, dsEdit] then
    if DataSet.UpdateStatus in [usModified,usInserted,usDeleted] then
    begin
      DataModule1.SQuibuserevent.ApplyUpdates;
      debugOut(5,'SQuibusereventAfterPost', 'start  SQuibusereventAfterPost (apply)');
    end;
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
    end
    else debugOut(3,'SQuibusereventAfterPost',  'Error: Missing Transaction SQuibusereventAfterPost');
    debugOut(5,'SQuibusereventAfterPost',  'StartTransaction in SQuibusereventAfterPost: ');
  except
    debugOut(2,'SQuibusereventAfterPost', 'exception in SQuibusereventAfterPost (starttransaction)');
  end;
end;

procedure TDataModule1.SQwork_descriptionAfterDelete(DataSet: TDataSet);
begin
  debugOut(5,'SQwork_descriptionAfterDelete', 'start  '+ DataSet.Name+' AfterDelete');
  try
    // after delete the state is not edit or insert but we have to apply
      DataModule1.SQwork_description.ApplyUpdates;
      debugOut(5,'SQwork_descriptionAfterDelete',  'start  '+ DataSet.Name+' AfterDelete (apply)');
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      debugOut(5,'SQwork_descriptionAfterDelete',  'start  '+ DataSet.Name+' AfterDelete (commit/start)');
    end
    else debugOut(3,'SQwork_descriptionAfterDelete',  'Error: Missing Transaction '+ DataSet.Name+' AfterDelete');
  except
    debugOut(2,'SQwork_descriptionAfterDelete', 'exception in '+ DataSet.Name+' AfterDelete (commit)');
  end;
end;

procedure TDataModule1.SQwork_descriptionAfterInsert(DataSet: TDataSet);
begin
  SQwork_description.FieldByName('userid').AsString := uid;
  SQwork_description.FieldByName('jahr').AsInteger :=
    YearOf(StrToDate(fwork_description.EditButtonDate.text));
  SQwork_description.FieldByName('monat').AsInteger :=
    MonthOf(StrToDate(fwork_description.EditButtonDate.text));
  SQwork_description.FieldByName('tag').AsInteger :=
    DayOf(StrToDate(fwork_description.EditButtonDate.text));
  SQwork_description.FieldByName('event').AsString :=
    Query_day_report.FieldByName('event').AsString;
end;

procedure TDataModule1.SQwork_descriptionAfterPost(DataSet: TDataSet);
begin
  debugOut(5,'SQwork_descriptionAfterPost', 'start  '+ DataSet.Name+' AfterPost');
  try
    //if (DataModule1.SQwork_description.State in [dsInsert, dsEdit])
    //   or DataModule1.SQwork_description.Modified then
    if DataSet.UpdateStatus in [usModified,usInserted,usDeleted] then
    begin
      DataModule1.SQwork_description.ApplyUpdates;
      debugOut(5, 'start  SQwork_descriptionAfterPost (apply)');
    end;
    if SQLTransaction1.Active then
    begin
      SQLTransaction1.CommitRetaining;
      debugOut(5,'SQwork_descriptionAfterPost',  'start  '+ DataSet.Name+' AfterPost (commit/start)');
    end
    else debugOut(3, 'Error: Missing Transaction SQwork_descriptionAfterPost');
    debugOut(5,'SQwork_descriptionAfterPost',  'end SQwork_descriptionAfterPost: ');
  except
    debugOut(2,'SQwork_descriptionAfterPost', 'exception in SQwork_descriptionAfterPost (starttransaction)');
  end;
end;

procedure TDataModule1.SQwork_descriptionPostError(DataSet: TDataSet;
  E: EDatabaseError; var DataAction: TDataAction);
begin
  ShowMessage('Beim Schreiben des Datensatzes in work_description ist ein Fehler aufgetreten: '+ LineEnding
               + E.Message + LineEnding
               +'Führe jetzt Cancel auf Datensatz aus.'+ LineEnding
               +'Bitte Datensatz kontrollieren.');
  DataSet.Cancel;
  DataAction:=daAbort;
  debugOut(3,'SQwork_descriptionPostError', E.Message);
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


procedure TDataModule1.TimerloggedinTimer(Sender: TObject);
begin
  SQuibloggedin.Edit;
  SQuibloggedin.FieldByName('loggedin').AsDateTime := now;
  SQuibloggedin.Post;
end;

procedure TDataModule1.TimerLogoffOnTopTimer(Sender: TObject);
begin
  debugOut(8,'TimerLogoffOnTopTimer', 'start ');
  TimerLogoffOnTop.interval := 1500;
  //SetWindowPos(FLogoff.handle, HWND_TOPMOST, 0, 0, screenx - 1, screeny - 1, SWP_NOACTIVATE);
end;

procedure TDataModule1.TimerOnTopTimer(Sender: TObject);
begin
  debugOut(8,'TimerOnTopTimer', 'start ');
  TimerOntop.interval := 500;
  {$IFDEF WINDOWS}
  SetWindowPos(FOnTop.handle, HWND_TOPMOST, leftint, 0, ontopwidth,
    ontopheight, SWP_NOACTIVATE);
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
  FOntop.Left := leftint;
  FOnTop.Top:= 0;
  FOnTop.Height:=ontopheight;
  FOnTop.Width:=ontopwidth;
  FOnTop.FormStyle := fsSystemStayOnTop;
//  SetWindowPos(FOnTop.handle, HWND_TOPMOST, leftint, 0, ontopwidth,
//    ontopheight, SWP_NOACTIVATE);
  {$ENDIF LINUX}

  // show last event in DBLCB_topten
  //if not FOnTop.ineditmode then SQuibevent.Last;
end;

procedure TDataModule1.TimerQueryLoggedInTimer(Sender: TObject);
begin
  debugOut(8,'TimerQueryLoggedInTimer', 'start ');
  getLoggedInList(Floggedin.ListBox1.Items, False);
end;

function reportmissing(startt, stopt : TDateTime; var missinglist : Tstringlist; addNotMissing: boolean) : boolean;
var
 laststartt, laststopt : TDateTime;
 //startt, stopt : TDateTime;
// sumtime,
//firststartt : TDatetime;
 uname, event  : String;
 //starttime, stoptime : String;
 year, month, day: word;

begin
  result := false;
  //missinglist := Tstringlist.create;
  // start looking for missing reports
  //sumtime := 0;
  //starttime :=
  //startt := now;
  //stoptime := now+1;
  //stopt := now+1;
  if Datamodule1.TrayQuery3.Active then Datamodule1.TrayQuery3.Close;
  Datamodule1.TrayQuery3.sql.Clear;
  Datamodule1.TrayQuery3.sql.Add('select * from uibevent ');
  Datamodule1.TrayQuery3.sql.Add('where ');
  Datamodule1.TrayQuery3.sql.Add('(userid = :uid) and ');
  Datamodule1.TrayQuery3.sql.Add('(starttime >= :start) and ');
  Datamodule1.TrayQuery3.sql.Add('(starttime < :stop) ');
  Datamodule1.TrayQuery3.sql.Add('order by starttime ');
  Datamodule1.TrayQuery3.ParamByName('uid').AsString := uid;
  Datamodule1.TrayQuery3.ParamByName('start').AsDateTime := startt;
  Datamodule1.TrayQuery3.ParamByName('stop').AsDateTime := stopt;
  Datamodule1.TrayQuery3.open;
  Datamodule1.TrayQuery3.first;
  laststartt := Datamodule1.TrayQuery3.fieldbyname('starttime').asdatetime;
  //firststartt := laststartt;
  //laststopt := Datamodule1.TrayQuery3.fieldbyname('stoptime').asdatetime;
  //if not (combobox1.Text = 'Summe Alle') then
  //  Datamodule1.SQuibevent.Locate('starttime;stoptime', VarArrayOf([laststartt,laststopt]), [loCaseInsensitive,loPartialKey])
  //else
  //if not Datamodule1.TrayQuery4.Active then Datamodule1.TrayQuery4.Open;
  //  Datamodule1.TrayQuery4.Locate('userid;starttime;stoptime', VarArrayOf([uid,laststartt,laststopt]), [loCaseInsensitive,loPartialKey]);
  //Datamodule1.TrayQuery3.close;
  //sumtime := laststopt - laststartt;
  //Datamodule1.SQuibevent.next;
  // query for event that need a report
  Datamodule1.TrayQuery1.sql.Clear;
  Datamodule1.TrayQuery1.sql.Add('select event from uibaktevent where');
  Datamodule1.TrayQuery1.sql.Add(' reportrequired = 1');
  Datamodule1.TrayQuery1.open;
  decodeDate(laststartt, year, month, day);
  Datamodule1.TrayQuery2.sql.Clear;
  Datamodule1.TrayQuery2.sql.Add('select * from UIB_WORK_DESCRIPTION where ');
  Datamodule1.TrayQuery2.sql.Add('(jahr >= :year) and ');
  Datamodule1.TrayQuery2.sql.Add('(monat >= :month) and ');
  Datamodule1.TrayQuery2.sql.Add('(tag >= :day) and ');
  Datamodule1.TrayQuery2.sql.Add('(userid = :uid) and ');
  Datamodule1.TrayQuery2.sql.Add('(not (description = ""))  ');
  Datamodule1.TrayQuery2.ParamByName('year').AsInteger := year;
  Datamodule1.TrayQuery2.ParamByName('month').AsInteger := month;
  Datamodule1.TrayQuery2.ParamByName('day').AsInteger := day;
  Datamodule1.TrayQuery2.ParamByName('uid').AsString := uid;
  Datamodule1.TrayQuery2.open;

  while not Datamodule1.TrayQuery3.eof do
  begin
   uname := Datamodule1.TrayQuery3.fieldbyname('userid').asString;
   startt := Datamodule1.TrayQuery3.fieldbyname('starttime').asdatetime;
   event := Datamodule1.TrayQuery3.fieldbyname('event').asString;
   decodeDate(startt, year, month, day);
   if  Datamodule1.TrayQuery1.Locate('event',event,[loCaseInsensitive]) then
   begin
     if missinglist.IndexOf(event) < 0 then
     begin
       if not Datamodule1.TrayQuery2.Locate('userid;jahr;monat;tag;event',
                          VarArrayOf([uname, year, month, day, event]),[loCaseInsensitive]) then
       begin
          missinglist.Add(event);
          result := true;
       end;
     end;
   end;
   Datamodule1.TrayQuery3.next;
  end;
  Datamodule1.TrayQuery1.close;
  Datamodule1.TrayQuery2.close;
  Datamodule1.TrayQuery3.close;
  // end looking for missing reports
end;

procedure TDataModule1.TimerTrayIconTimer(Sender: TObject);
var missinglist : Tstringlist;
begin
  debugOut(6,'trayicon', 'start trytimer ');
  missinglist := Tstringlist.Create;
  if reportmissing(date, now,missinglist,false) then
  begin
    debugOut(6,'trayicon', 'Report missing: '+ missinglist.Text);
    {$IFDEF WINDOWS}
    TrayIcon1.BalloonHint:='Report missing: '+LineEnding + missinglist.Text;
    TrayIcon1.ShowBalloonHint;
    {$ENDIF WINDOWS}
    {$IFDEF LINUX}
    try
      ProcessTrayNotify.Parameters.AddStrings(missinglist);
      ProcessTrayNotify.Execute;
    except
      debugOut(3,'trayicon', 'Exception starting notify-send ');
    end;
    {$ENDIF LINUX}
  end;
  missinglist.Free;
  debugOut(6,'trayicon', 'stop trytimer ');
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
    debugOut(6,'setloggedin', 'true fuer ' + uid);
  end
  else
  begin
    loggedin := False;
    logtime := encodedate(1999, 1, 1);
    debugOut(6,'setloggedin', 'false fuer ' + uid);
  end;
  if not SQuibloggedin.Active then
    Datamodule1.SQuibloggedin.Open;
  debugOut(7,'setloggedin', 'SQuibloggedin.FieldCount: ' +
    IntToStr(Datamodule1.SQuibloggedin.FieldCount));
  debugOut(7,'setloggedin', 'SQuibloggedin.active: ' +
    BoolToStr(Datamodule1.SQuibloggedin.Active, True));
  debugOut(7,'setloggedin', 'SQuibloggedin.filterd: ' +
    BoolToStr(Datamodule1.SQuibloggedin.Filtered, True));
  debugOut(7,'setloggedin', 'SQuibloggedin.filter: ' + Datamodule1.SQuibloggedin.Filter);
  if not DataModule1.SQuibloggedin.Locate('userid',uid,[loCaseInsensitive]) then
  //if Datamodule1.SQuibloggedin.RecordCount < 1 then
  begin
    try
      Datamodule1.SQuibloggedin.Append;
      Datamodule1.SQuibloggedin.FieldByName('userid').AsString := uid;
      Datamodule1.SQuibloggedin.FieldByName('loggedin').AsDateTime := logTime;
      Datamodule1.SQuibloggedin.Post;
      //Datamodule1.SQuibloggedin.ApplyUpdates;
      //Datamodule1.SQLTransaction1.CommitRetaining;
    except
      Datamodule1.SQuibloggedin.Post;
      //Datamodule1.SQuibloggedin.ApplyUpdates;
      //Datamodule1.SQLTransaction1.CommitRetaining;
    end;
  end
  else
  begin
    //debugOut(5, 'SQuibloggedin field 0: ' + SQuibloggedin.Fields[0].AsString);
    //if Datamodule1.SQuibloggedin.FieldByName('userid').AsString = uid then
    //begin
      Datamodule1.SQuibloggedin.Edit;
      Datamodule1.SQuibloggedin.FieldByName('loggedin').AsDateTime := logtime;
      Datamodule1.SQuibloggedin.Post;
      //Datamodule1.SQuibloggedin.ApplyUpdates;
      //Datamodule1.SQLTransaction1.CommitRetaining;
    //end
    //else
    //  debugOut(5, 'setloggedin not done: ' +
    //    Datamodule1.SQuibloggedin.FieldByName('userid').AsString);
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
  if floggedin_created then FLoggedin.BtnAktualisierenClick(nil);
end;


procedure TDataModule1.Weristda1Click(Sender: TObject);
var
  logdir,logfeilname: string;
  myini: TIniFile;
begin
  try
    datamodule1.debugOut(5, 'start von DataModule1.Weristda1Click');
    // we will use logdir for logging and for configuration
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
        datamodule1.debugOut(2,'ontop', 'failed presenz to all desktops');
      Weristda1.Checked := True;
      myini.WriteBool('general', 'weristda', True);
    end;
    myini.UpdateFile;
    myini.Destroy;
    datamodule1.debugOut(5, 'ende von DataModule1.Weristda1Click');
  except
    on e: Exception do
    begin
      debugOut(3,'', 'exception in DataModule1.Weristda1Click');
      debugOut(5, e.Message);
      raise;
    end;
  end;
end;

procedure TDataModule1.ZeigenurmeineProjekte1Click(Sender: TObject);
var
  logdir,logfeilname: string;
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
  logfeilname := ExpandFileNameUTF8(logdir + '\uibtime.conf');
  myini := TIniFile.Create(logfeilname);
  debugOut(5, 'Will use conf file: ' + logfeilname);
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
  myini.Destroy;
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
  i : integer;
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
        debugOut(5, 'DataModule1.getLoggedInList Transaction restarted ');
      end;

      if SQqueryloggedin.active then
        SQqueryloggedin.Close;
      ///SQqueryloggedin.databasename := 'uibtime';
      SQqueryloggedin.sql.Clear;
      SQqueryloggedin.sql.add(
        'select userid from uibloggedin where loggedin > :ltime');
      SQqueryloggedin.ParamByName('ltime').AsDateTime := (now - encodetime(0, 15, 0, 0));
      debugOut(5, 'DataModule1.getLoggedInList ltime: '+DateTimeToStr((now - encodetime(0, 15, 0, 0))));
      SQqueryloggedin.Open;
      userlist.Clear;
    (*
    pingserver := database1.Params.Values['SERVER NAME'];
    pingserver := copy(pingserver,1,pos(':',pingserver)-1);
    if AnsiLowerCase(pingserver) = 'c' then
     pingserver := 'local';
     *)
     debugOut(5, 'DataModule1.getLoggedInList count: '+IntToStr(SQqueryloggedin.RecordCount));
      for i := 1 to SQqueryloggedin.RecordCount do
      begin
        //userlist.Add(pingserver+':'+queryloggedin.fieldbyname('userid').asstring);
        userlist.Add(SQqueryloggedin.FieldByName('userid').AsString);
        debugOut(5, 'DataModule1.getLoggedInList found: '+SQqueryloggedin.FieldByName('userid').AsString);
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
      debugOut(5, 'ende von DataModule1.getLoggedInList');
    except
      on e: Exception do
      begin
        debugOut(3,'', 'exception in DataModule1.getLoggedInList');
        debugOut(5, e.Message);
        raise;
      end;
    end;

  end;
end;



procedure TDataModule1.writeVerinfoToLog(var feil: Textfile);
//var
//  S: string;
begin
  //version := '4.0.12';
  writeln(feil,'-----Angfang Version-Info-----');
  writeln(feil,'uibtime Version: '+version+' vom '+verDatum);
  writeln(feil,'-----Ende Version-Info-----');
end;

function IncludeTrailingPathDelimiter(path: string): string;
begin
  Result := IncludeTrailingBackslash(path);
end;

function GetSpecialFolderLocation(csidl: integer): string;
begin

end;



procedure TDataModule1.DataModuleCreate(Sender: TObject);
var
  logdir,logfeilname: string;
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
  logfeilname := ExpandFileNameUTF8(logdir + '\uibtime.conf');
  myini := TIniFile.Create(logfeilname);
  ////debugOut(5, 'Will use conf file: ' + logfeilname);
  ////debugOut(5,'Will use conf file: '+logdir+'\uibtime.conf');
    Fdebug.Memo1.Append('Will use conf file: '+ logfeilname);
  Autologin1.Checked := myini.ReadBool('general', 'autologin', False);
  Fdebug.Memo1.Append('autologin: '+BoolToStr(Autologin1.Checked,true));
  zeigenurmeineprojekte1.Checked :=
    myini.ReadBool('general', 'zeigenurmeineprojekte', False);
  Fdebug.Memo1.Append('zeigenurmeineprojekte: '+BoolToStr(zeigenurmeineprojekte1.Checked,true));
  if ZeigenurmeineProjekte1.Checked then
    ontop_showallprojects := False
  else
    ontop_showallprojects := True;
  ShowDebugwindow1.Checked := myini.ReadBool('general', 'showdebugwindow', False);
  Fdebug.Memo1.Append('showdebugwindow: '+BoolToStr(ShowDebugwindow1.Checked,true));
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
  Fdebug.Memo1.Append('weristda: '+BoolToStr(Weristda1.Checked,true));
  /// is not created right now and waiting does not work
  /// while not floggedin_created do Sleep(100);
  ///if Weristda1.Checked then FLoggedin.Visible:= true
  ///else FLoggedin.Visible:= false;

  debuglevel := myini.ReadInteger('general', 'debuglevel', 5);
  Fdebug.Memo1.Append('debuglevel: '+IntToStr(debuglevel));
  TrayInterval := myini.ReadInteger('general', 'TrayInterval', 5);
  TimerTrayIcon.Interval:=TrayInterval* 60 * 1000;
  Trayshow := myini.ReadBool('general', 'showTray', True);
  TrayIcon1.Visible:=Trayshow;
  TimerTrayIcon.Enabled:=Trayshow;
  myini.UpdateFile;
  myini.Destroy;
  logfeilname := ExpandFileNameUTF8(logdir + '\uibtime.log');
  AssignFile(logfeil, logfeilname);
  Rewrite(logfeil);
  writeln(logfeil, DateTimeToStr(Now) + ': uibtime started');
  writeVerinfoToLog(logfeil);
  writeln(logfeil,Fdebug.Memo1.Lines.Text);
  closeFile(logfeil);
end;

procedure TDataModule1.TerminateApplication;
var
  logdir,logfeilname: string;
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
  myini.WriteBool('general', 'autologin', Autologin1.Checked);
  myini.WriteBool('general', 'zeigenurmeineprojekte', zeigenurmeineprojekte1.Checked);
  myini.WriteBool('general', 'showdebugwindow', ShowDebugwindow1.Checked);
  myini.WriteInteger('general', 'debuglevel', debuglevel);
  myini.UpdateFile;
  myini.Destroy;
  try
    debugOut(5, 'Will free components ');
    if Assigned(FLoggedin) then FLoggedin.Free;
    //if Assigned(FOnTop) then FOnTop.Free;
    if Assigned(FLogoff) then FLogoff.Free;
    if Assigned(FNachfrage) then FNachfrage.Free;
    if Assigned(fwork_description) then fwork_description.Free;
    if Assigned(FTreeview) then FTreeview.Free;
    if Assigned(FDataedit) then FDataedit.Free;
    if Assigned(FStatistik) then FStatistik.Free;
    {$IFDEF WINDOWS}
    debugOut(5, 'Will stop httpserver .... ');
    if Assigned(httpservice.FServerThread) then stophttpserver;
    {$ENDIF WINDOWS}
    //if Assigned(FOnTop) then FOnTop.Close;
    //FOnTop.Destroy;
    //if Assigned(FOnTop) then FOnTop.Close;
    debugOut(5, 'Finished free components ');
  finally
    debugOut(5, 'Will terminate .... ');
    Application.Terminate;
    debugOut(5, 'Will halt .... ');
    halt;
  end;
end;

function TDataModule1.isValidEvent(event:string): boolean;
begin
   result:= SQuibaktevent.Locate('event',event,[loCaseInsensitive]);
end;

initialization
  { initialization-Abschnitt }
  ontopwidth := 850;//730;
  ontopheight := 25;
  screenx := Screen.Width;
  screeny := Screen.Height;
  leftint := (screenx - ontopwidth) div 2;
  loggedin := False;
  floggedin_created := False;
  // datamodule1.loggedinserver := TStringList.Create;
  ///vi := GetVersionInfoRec(Application.ExeName);
  ///version := vi.FileVersion;
  //version := '4.0.12';
  //debuglevel := 5;

  verDatum := DateToStr(FileDateToDateTime(FileAge(paramstr(0))));
  //from http://wiki.freepascal.org/Show_Application_Title,_Version,_and_Company
  FileVerInfo:=TFileVersionInfo.Create(nil);
  try
    FileVerInfo.FileName:=paramstr(0);
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
  finally
    FileVerInfo.Free;
  end;
//WinstVersion := '4.11.6.1';

finalization
  { finalization-Abschnitt }
  begin

  end;

end.
