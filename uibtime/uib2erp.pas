unit uib2erp;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  sqldb,
  FileUtil,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  PairSplitter,
  DBGrids,
  Grids,
  Buttons,
  LR_Class,
  LR_DBSet,
  //LR_View, LR_E_CSV,
  //lrEmailExportFilter,
  lr_e_pdf,
  //Spin,
  EditBtn,
  ExtDlgs,
  DBCtrls,
  MaskEdit,
  ComCtrls,
  DB,
  Variants,
  DateUtils,
  //Math, debug,
  strutils,
  Result,
  uibdatetime;

type

  { TFuibtime2erp }

  TFuibtime2erp = class(TForm)
    BitBtn1: TBitBtn;
    BitBtnShowexportTable: TBitBtn;
    BitBtnExp2Erp: TBitBtn;
    BitBtnEditExportErrorTable: TBitBtn;
    BtnLockReportData: TButton;
    BtnCreateReport: TButton;
    BtnCreateInterimReport: TButton;
    BtnLastMonth: TButton;
    BtnLastWeek: TButton;
    BtnLoadRequiredReports: TButton;
    BtnExportZeroAccounting: TButton;
    BtnThisMonth: TButton;
    BtnThisWeek: TButton;
    CalendarDialog1: TCalendarDialog;
    CheckBox1: TCheckBox;
    CheckBoxAccountexport: TCheckBox;
    CheckGroup1: TCheckGroup;
    DataSource1: TDataSource;
    DataSource3: TDataSource;
    DBNavigator1: TDBNavigator;
    DBText1: TDBText;
    DBText2: TDBText;
    Edit1: TDateEdit;
    Edit2: TDateEdit;
    DBGrid1: TDBGrid;
    EditButtonExportDir: TEditButton;
    //frDBDataSet1: TfrDBDataSet;
    //frReport1: TfrReport;
    //frDBDataSet1: TfrDBDataSet;
    //frReport1: TfrReport;
    //frTNPDFExport1: TfrTNPDFExport;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    MaskEditDh: TMaskEdit;
    MaskEditHm: TMaskEdit;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    PanelExport: TPanel;
    query1: TSQLQuery;
    query2: TSQLQuery;
    QueryProjektzeit: TSQLQuery;
    query3: TSQLQuery;
    QueryUibeventaccountreport: TSQLQuery;
    QueryUEARhelper: TSQLQuery;
    BtnHm2Dh: TSpeedButton;
    BtnDh2HM: TSpeedButton;
    queryUAE: TSQLQuery;
    queryAccEv: TSQLQuery;
    StatusBar1: TStatusBar;
    StringGrid1: TStringGrid;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtnEditExportErrorTableClick(Sender: TObject);
    procedure BitBtnExp2ErpClick(Sender: TObject);
    procedure BitBtnShowexportTableClick(Sender: TObject);
    procedure BtnCreateInterimReportClick(Sender: TObject);
    procedure BtnCreateReportClick(Sender: TObject);
    procedure BtnDh2HMClick(Sender: TObject);
    procedure BtnHm2DhClick(Sender: TObject);
    procedure BtnLastMonthClick(Sender: TObject);
    procedure BtnLastWeekClick(Sender: TObject);
    procedure BtnExportZeroAccountingClick(Sender: TObject);
    procedure BtnLoadRequiredReportsClick(Sender: TObject);
    procedure BtnLockReportDataClick(Sender: TObject);
    procedure BtnThisMonthClick(Sender: TObject);
    procedure BtnThisWeekClick(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure EditButtonExportDirChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MaskEditDhChange(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure Panel2Click(Sender: TObject);
    procedure Panel3Click(Sender: TObject);
    procedure query1AfterPost(DataSet: TDataSet);
    procedure StringGrid1Click(Sender: TObject);
    procedure StringGrid1DblClick(Sender: TObject);
    procedure StringGrid1DrawCell(Sender: TObject; aCol, aRow: integer;
      aRect: TRect; aState: TGridDrawState);
    function filter(flt: string): boolean;
    procedure createReport(output, comment: string);
    procedure StringGrid1GetCellHint(Sender: TObject; ACol, ARow: integer;
      var HintText: string);
    procedure StringGrid1ShowHint(Sender: TObject; HintInfo: PHintInfo);
  private
    { private declarations }
    function getLastIntervalInfo(const event: string;
      const querystartdate, queryenddate: string;
      var lastIntervalStart, lastIntervalEnd: TDateTime;
      var intervalStartFound, intervalEndFound, reportrequired,
      accountingrequired: boolean; var basemonth: integer;
      var projektstart: TDateTime; var totaltime, usedtime, availabletime: TDateTime;
      var avaiablesign: char): boolean;
  public
    { public declarations }
  end;

var
  Fuibtime2erp: TFuibtime2erp;
  frDBDataSet1: TfrDBDataSet;
  frReport1: TfrReport;
  frTNPDFExport1: TfrTNPDFExport;

implementation

uses
  uibdata;


{$R *.lfm}
var
  rowcolor: array [0 .. 1024] of integer;
  rowcounter: integer;
  //suchevent, vonstr, bisstr, bisstrincl: string;
  suchevent, vonstr, bisstr: string;
  repvonstr, repbisstr, repfilevonstr, repfilebisstr: string;

{ TFuibtime2erp }

function TFuibtime2erp.getLastIntervalInfo(const event: string;
  const querystartdate, queryenddate: string;
  var lastIntervalStart, lastIntervalEnd: TDateTime;
  var intervalStartFound, intervalEndFound, reportrequired, accountingrequired: boolean;
  var basemonth: integer; var projektstart: TDateTime;
  var totaltime, usedtime, availabletime: TDateTime; var avaiablesign: char): boolean;
var
  //available_min, total_min, used_min,
  total_min, years, months, days, hours, minutes: integer;
  total, used, available: double;
  //base: string;
  endmonth, year, endyear, month, day, startyear, startmonth, startday,
  aktstartyear, aktstartmonth: word;
  //endmonthi, endyeari, aktstartyeari, aktstartmonthi: integer;
  lastIntervalStart_, lastIntervalEnd_: TDateTime;

  helperint: integer;
  suchevent: string;
  monthsmod, monthdiv, acc_per_monthnum_int: integer;
  acc_per_monthnum: double;
  querystartdt, queryenddt, vondate, bisdate: TDateTime;
begin
  try
    // some initial retrun values
    Result := False;
    intervalStartFound := False;
    intervalEndFound := False;
    reportrequired := False;
    accountingrequired := False;
    totaltime := 0;
    total := 0;
    usedtime := 0;
    availabletime := 0;
    lastIntervalStart := 0;
    lastIntervalEnd := 0;
    projektstart := 0;
    basemonth := 0;
    avaiablesign := '+';
    // other inits
    aktstartyear := 2001;
    aktstartmonth := 1;
    DataModule1.debugOut(6, 'getLastIntervalInfo', 'enter for :' + event);
    suchevent := event;
    querystartdt := StrToDate(querystartdate);
    queryenddt := StrToDate(queryenddate);
    // get project info for event....
    if QueryProjektzeit.Active then
      QueryProjektzeit.Close;
    //QueryProjektzeit.databasename :='uibtime';
    QueryProjektzeit.SQL.Clear;
    QueryProjektzeit.sql.Add(' select time_h, acc_per_monthnum, projectstart, ');
    QueryProjektzeit.sql.Add('    reportrequired, accountingrequired ');
    QueryProjektzeit.sql.Add('    from uiballevent');
    QueryProjektzeit.sql.Add('    where (event = :suchevent)');
    QueryProjektzeit.parambyname('suchevent').AsString := suchevent;
    QueryProjektzeit.Open;
    // get requires
    if QueryProjektzeit.FieldByName('reportrequired').AsInteger = 1 then
      reportrequired := True
    else
      reportrequired := False;
    if QueryProjektzeit.FieldByName('accountingrequired').AsInteger = 1 then
      accountingrequired := True
    else
      accountingrequired := False;
    // get last interval
    if (not QueryProjektzeit.FieldByName('time_h').isNull) and
      (not QueryProjektzeit.FieldByName('projectstart').isNull) and
      (QueryProjektzeit.FieldByName('acc_per_monthnum').AsFloat > 0) then
    begin
      // get total free hours
      total := QueryProjektzeit.FieldByName('time_h').AsFloat;
      (*
      total_min := round(abs((total - trunc(total)) * 60));
      hours := trunc(Total);
      minutes := Round(frac(Total) * 60);
      if minutes >= 60 then
      begin
        minutes := minutes - 60;
        hours := hours + 1;
      end;
      //years := hours div 8760;
      //hours := hours mod 8760;
      //month :=
      *)
      totaltime := total / 24;
      //totaltime := EncodeTimeInterval(hours, minutes, 0, 0);
      // basemonth and projektstart
      acc_per_monthnum := QueryProjektzeit.FieldByName('acc_per_monthnum').AsFloat;
      basemonth := trunc(acc_per_monthnum);
      projektstart := QueryProjektzeit.FieldByName('projectstart').AsDateTime;
      DataModule1.debugOut(6, 'getLastIntervalInfo projektstart :' +
        DateToStr(projektstart));
      // if projektstart is in future we will not find anything
      if projektstart >= queryenddt then
      begin
        Result := False;
        DataModule1.debugOut(3, 'getLastIntervalInfo',
          'Projektstart of: ' + suchevent +
          ' is in "future": later or equal then end of searchinterval:' + DateToStr(queryenddt));
        Exit;
      end;
      //// startday of projekt end
      //projektstart := projektstart -1;
      // look for accounting boundaries (intervals)
      //todo (or not todo): work with fraktal month
      acc_per_monthnum_int := trunc(acc_per_monthnum);
      monthsmod := acc_per_monthnum_int mod 12;
      monthdiv := acc_per_monthnum_int div 12;
      // calculate last interval boundaries
      (*
      // look first for end version
      decodeDate(querystartdt, year, month, day);
      decodeDate(projektstart, startyear, startmonth, startday);
      helperint := month - ((12 * (year - startyear) + month - startmonth) mod
        monthsmod);
      if helperint < 1 then
      begin
        endyeari := year - 1;
        endmonthi := 12 + helperint;
      end
      else
      begin
        endmonthi := helperint;
        endyeari := year;
      end;
      if endmonthi > 12 then
      begin
        endmonthi := endmonthi - 12;
        endyeari := endyeari + 1;
      end;
      // now start
      aktstartmonthi := endmonthi - monthsmod;
      aktstartyeari := endyeari;
      if aktstartmonthi < 1 then
      begin
        aktstartmonthi := aktstartmonthi + 12;
        aktstartyeari := aktstartyeari - 1;
      end;
      if aktstartmonthi < 1 then
      begin
        aktstartmonthi := aktstartmonthi + 12;
        aktstartyeari := aktstartyeari - 1;
      end;
      aktstartmonth := aktstartmonthi;
      aktstartyear := aktstartyeari;
      endmonth := endmonthi;
      endyear := endyeari;
      *)

      (*
      // look first for start version
      decodeDate(querystartdt, year, month, day);
      decodeDate(projektstart, startyear, startmonth, startday);
      helperint := month - ((12 * (year - startyear) + month - startmonth));
      if  acc_per_monthnum_int > 0 then
        helperint := month - ((12 * (year - startyear) + month - startmonth) mod
        acc_per_monthnum_int);
      if helperint < 1 then
      begin
        aktstartyear := year - 1;
        aktstartmonth := 12 + helperint;
      end
      else
      begin
        aktstartmonth := helperint;
        aktstartyear := year;
      end;
      if aktstartmonth > 12 then
      begin
        aktstartmonth := aktstartmonth - 12;
        aktstartyear := aktstartyear + 1;
      end;
      endmonth := aktstartmonth + monthsmod;
      endyear := aktstartyear + monthdiv;
      if endmonth > 12 then
      begin
        endmonth := endmonth - 12;
        endyear := endyear + 1;
      end;
      if endmonth > 12 then
      begin
        endmonth := endmonth - 12;
        endyear := endyear + 1;
      end;
      *)
      // here is the result for the last Interval
      //lastIntervalStart := EncodeDate(aktstartyear, aktstartmonth, startday);
      lastIntervalStart := getLastIntervalStart(
        projektstart, queryenddt, acc_per_monthnum_int);
      //decodeDate(lastIntervalStart, aktstartyear, aktstartmonth, aktstartday);
      DataModule1.debugOut(6, 'getLastIntervalInfo',
        'lastIntervalStart :' + DateToStr(lastIntervalStart));
      //lastIntervalEnd := EncodeDate(endyear, endmonth, startday);
      lastIntervalEnd := getLastIntervalEnd(projektstart, queryenddt,
        acc_per_monthnum_int);
      DataModule1.debugOut(6, 'getLastIntervalInfo',
        'lastIntervalEnd :' + DateToStr(lastIntervalEnd));
      // are the interval boundaries in search intervall
      if (lastIntervalStart >= querystartdt) and (lastIntervalStart <= queryenddt) then
        intervalStartFound := True
      else
        intervalStartFound := False;

      if (lastIntervalEnd >= querystartdt) and (lastIntervalEnd <= queryenddt) then
        intervalEndFound := True
      else
        intervalEndFound := False;
      // finding end is mor impotant then start
      if intervalStartFound and (lastIntervalStart > querystartdt) and
        not intervalEndFound then
      begin
        // step back one interval
        lastIntervalStart_ := IncMonth(lastIntervalStart, monthsmod * -1);
        lastIntervalEnd_ := IncMonth(lastIntervalEnd, monthsmod * -1);
        if (lastIntervalEnd_ >= querystartdt) and
          (lastIntervalEnd_ <= queryenddt) and
          (lastIntervalStart_ >= projektstart) then
        begin
          lastIntervalEnd := lastIntervalEnd_;
          lastIntervalStart := lastIntervalStart_;
        end;
      end;
      // use changed data
      if (lastIntervalStart >= querystartdt) and (lastIntervalStart <= queryenddt) then
        intervalStartFound := True
      else
        intervalStartFound := False;

      if (lastIntervalEnd >= querystartdt) and (lastIntervalEnd <= queryenddt) then
        intervalEndFound := True
      else
        intervalEndFound := False;
    end;

    if QueryProjektzeit.Active then
      QueryProjektzeit.Close;
    QueryProjektzeit.SQL.Clear;
    QueryProjektzeit.sql.Add(' select sum(stunden) as stunden from uibevent');
    QueryProjektzeit.sql.Add('    where (event = :suchevent)');
    QueryProjektzeit.sql.Add('    and  (starttime >= :von)');
    QueryProjektzeit.sql.Add('    and (stoptime < :bis)');
    QueryProjektzeit.parambyname('suchevent').AsString := suchevent;
    // init query interval with default
    vondate := querystartdt;
    bisdate := queryenddt;
    if intervalEndFound then
    begin
      // we have to query the whole accounting interval
      vondate := lastIntervalStart;
      bisdate := lastIntervalEnd;
    end;
    if intervalStartFound then
    begin
      // we will start at the beginning of the accounting interval
      vondate := lastIntervalStart;
    end;
    QueryProjektzeit.parambyname('von').AsString :=
      datetostr(vondate);
    QueryProjektzeit.parambyname('bis').AsString :=
      datetostr(bisdate);
    QueryProjektzeit.Open;
    used := QueryProjektzeit.FieldByName('stunden').AsFloat;
    hours := trunc(used);
    minutes := Round(frac(used) * 60);
    if minutes >= 60 then
    begin
      minutes := minutes - 60;
      hours := hours + 1;
    end;
    //usedtime := EncodeTimeInterval(hours, minutes, 0, 0);
    usedtime := used / 24;
    available := total - used;
    avaiablesign := '+';
    if available < 0 then
    begin
      available := abs(available);
      avaiablesign := '-';
    end;
    hours := trunc(available);
    minutes := Round(frac(available) * 60);
    if minutes >= 60 then
    begin
      minutes := minutes - 60;
      hours := hours + 1;
    end;
    availabletime := available / 24;



    //availabletime := EncodeTimeInterval(hours, minutes, 0, 0);
    // datetime is day.time  so it is also hour.frac-hour *24
    //availabletime := available * 24;
    (*
    EditProjektzeit.Hint := IntToStr(total_min) + '-' + IntToStr(
      used_min) + ' since 1.' + IntToStr(aktstartmonth) + '.' +
      IntToStr(aktstartyear);
    *)
    DataModule1.debugOut(6, 'getLastIntervalInfo', 'ProjektzeitTimer: ' +
      FloatToStr(total) + '-' + FloatToStr(used) + '=' + floattostr(available));
    (*
    EditProjektzeit.Text := IntToStr(trunc(available)) + minute2str(available_min);
    EditProjektzeit.Hint := IntToStr(trunc(total)) + minute2str(
      total_min) + '-' + IntToStr(trunc(used)) + minute2str(used_min) +
      ' since 1.' + IntToStr(aktstartmonth) + '.' + IntToStr(aktstartyear);

  end
  else
    EditProjektzeit.Text := '';
    *)

    if QueryProjektzeit.Active then
      QueryProjektzeit.Close;

    DataModule1.debugOut(6, 'getLastIntervalInfo', 'leave');
    Result := True;

  except
    on e: Exception do
    begin
      DataModule1.debugOut(3, '', 'exception in uibtime2erp.getLastIntervalInfo');
      DataModule1.debugOut(3, e.Message);
      raise;
    end;
  end;
end;

procedure TFuibtime2erp.Panel1Click(Sender: TObject);
begin

end;

procedure TFuibtime2erp.Panel2Click(Sender: TObject);
begin

end;

procedure TFuibtime2erp.Panel3Click(Sender: TObject);
begin

end;

procedure TFuibtime2erp.query1AfterPost(DataSet: TDataSet);
begin
  try
    if DataSet.UpdateStatus in [usModified, usInserted, usDeleted] then
    begin
      query1.ApplyUpdates;
      query3.Refresh;
      DataModule1.debugOut(5, 'query1AfterPost', 'start  query1AfterPost (apply)');
    end;
    if DataModule1.SQLTransaction1.Active then
    begin
      DataModule1.SQLTransaction1.CommitRetaining;
    end
    else
      DataModule1.debugOut(3, 'query1AfterPost',
        'Error: Missing Transaction SQuibakteventAfterPost');
    DataModule1.debugOut(5, 'Commit in query1AfterPost');
  except
    DataModule1.debugOut(2, 'query1AfterPost',
      'exception in query1AfterPost (commit)');
  end;
end;

procedure TFuibtime2erp.StringGrid1Click(Sender: TObject);
begin

end;

procedure TFuibtime2erp.StringGrid1DblClick(Sender: TObject);
var
  selindex: integer;
  tmpdate: TDate;
  year, month, day: word;
  summe_h, freed, monthnum, total: double;
begin
  DataModule1.debugOut(6, 'start StringGrid1DblClick');
  //Cursor:=crHourGlass;
  //StringGrid1.Cursor:=crHourGlass;
  try
    screen.Cursor := crHourGlass;
    Application.ProcessMessages;
    selindex := stringgrid1.Row;
    suchevent := stringgrid1.Rows[selindex][0];
    vonstr := stringgrid1.Rows[selindex][4];
    bisstr := stringgrid1.Rows[selindex][5];
    tmpdate := StrToDate(vonstr);
    DecodeDate(tmpdate, year, month, day);
    repfilevonstr := IntToStr(month) + '-' + IntToStr(year);
    repvonstr := vonstr;
    tmpdate := StrToDate(bisstr);
    DecodeDate(tmpdate - 1, year, month, day);
    repfilebisstr := IntToStr(month) + '-' + IntToStr(year);
    repbisstr := dateToStr(tmpdate - 1);
    // Allow export if intervalEndFound
    if (stringgrid1.Rows[selindex][3] = '+') or
      (stringgrid1.Rows[selindex][8] = '00:00') then
      PanelExport.Enabled := True
    else
      PanelExport.Enabled := False;

    // clear buffer

    if DataSource1.Enabled then
      DataSource1.Enabled := False;
    if query1.Active then
      query1.Close;
    query1.SQL.Clear;
    query1.sql.Add('delete ');
    query1.sql.Add('FROM uibeventaccountreport a ');
    query1.sql.Add('where ');
    query1.sql.Add('(a.event = :event) ');
    query1.sql.Add('and(a.dateday >= :start) ');
    query1.sql.Add('and (a.dateday <= :stop) ');
    query1.sql.Add('and (a.locked = 0) ');
    query1.ParamByName('event').AsString := suchevent;
    query1.parambyname('start').AsString := vonstr;
    query1.parambyname('stop').AsString := bisstr;
    query1.ExecSQL;

    // crear temp buffer
    if query1.Active then
      query1.Close;
    query1.SQL.Clear;
    query1.sql.Add('delete from uibevrtemp ');
    query1.ExecSQL;

    // fill temp buffer
    if query1.Active then
      query1.Close;
    query1.SQL.Clear;
    query1.sql.Add('insert into uibevrtemp ');
    query1.sql.Add('(userid, dateday, event, stunden, description) ');
    query1.sql.Add('select ');
    query1.sql.Add('a.USERID, ');
    query1.sql.Add(
      'cast( extract(day from a.starttime)||''.''|| extract(month from a.starttime)||''.''|| extract(year from a.starttime) as timestamp) as datum, ');
    query1.sql.Add('a.event, ');
    query1.sql.Add('sum(a.STUNDEN) as stunden, ');
    query1.sql.Add('b.DESCRIPTION ');
    query1.sql.Add('FROM uibevent a ');
    query1.sql.Add('FULL JOIN UIB_WORK_DESCRIPTION b on ');
    query1.sql.Add('(b.event=a.event) ');
    query1.sql.Add('and (b.JAHR = extract(year from a.STARTTIME)) ');
    query1.sql.Add('and (b.MONAT = extract(month from a.STARTTIME)) ');
    query1.sql.Add('and (b.TAG = extract(day from a.STARTTIME)) ');
    query1.sql.Add('and (a.USERID = b.USERID) ');
    query1.sql.Add('where ');
    query1.sql.Add('(a.event = :event) ');
    query1.sql.Add('and(a.starttime >= :start) ');
    query1.sql.Add('and (a.stoptime <= :stop) ');
    query1.sql.Add('group by 1,2,3,5  ');
    query1.ParamByName('event').AsString := suchevent;
    query1.parambyname('start').AsString := vonstr;
    query1.parambyname('stop').AsString := bisstr;
    query1.ExecSQL;

    // fill temp buffer
    if query1.Active then
      query1.Close;
    query1.SQL.Clear;
    query1.sql.Add('select * from uibevrtemp ');
    query1.Open;

    if query2.Active then
      query2.Close;
    query2.SQL.Clear;
    query2.sql.Add('select * from uibeventaccountreport a ');
    query2.sql.Add('where  ');
    query2.sql.Add('(a.event = :event) ');
    query2.sql.Add('and(a.dateday >= :start) ');
    query2.sql.Add('and (a.dateday <= :stop) ');
    query2.sql.Add('and (a.locked = 1) ');
    query2.ParamByName('event').AsString := suchevent;
    query2.parambyname('start').AsString := vonstr;
    query2.parambyname('stop').AsString := bisstr;
    query2.Open;
    while not query1.EOF do
    begin
      if query2.Locate('userid;dateday;event;locked',
        VarArrayOf([query1.FieldByName('userid').AsString,
        query1.FieldByName('dateday').AsDateTime,
        query1.FieldByName('event').AsString, 1]), [loCaseInsensitive]) then
      begin
        // record exists and is locked
      end
      else
      begin
        // record does not exists and will be appended
        query2.Append;
        // acc_rep_id will be reset by trigger
        query2.FieldByName('acc_rep_id').AsInteger := 0;
        query2.FieldByName('userid').AsString := query1.FieldByName('userid').AsString;
        query2.FieldByName('dateday').AsDateTime :=
          query1.FieldByName('dateday').AsDateTime;
        query2.FieldByName('event').AsString := query1.FieldByName('event').AsString;
        query2.FieldByName('stunden').AsFloat :=
          round(query1.FieldByName('stunden').AsFloat * 60) / 60;
        query2.FieldByName('description').AsString :=
          query1.FieldByName('description').AsString;
        query2.FieldByName('locked').AsInteger := 0;
        query2.Post;
      end;
      query1.Next;
    end;
    query2.ApplyUpdates;
    DataModule1.SQLTransaction1.CommitRetaining;
    //query2.Close;
    query1.Close;

    // query edit buffer
    if query1.Active then
      query1.Close;
    query1.SQL.Clear;
    query1.sql.Add('select ');
    query1.sql.Add('* ');
    query1.sql.Add('FROM uibeventaccountreport a ');
    query1.sql.Add('where ');
    query1.sql.Add('(a.event = :event) ');
    query1.sql.Add('and(a.dateday >= :start) ');
    query1.sql.Add('and (a.dateday <= :stop) ');
    query1.sql.Add('order by 3 ');
    query1.ParamByName('event').AsString := suchevent;
    query1.parambyname('start').AsString := vonstr;
    query1.parambyname('stop').AsString := bisstr;
    query1.Open;
    DataSource1.Enabled := True;

    // query summe
    if query3.Active then
      query3.Close;
    query3.SQL.Clear;
    query3.sql.Add('select ');
    query3.sql.Add('event, sum(stunden) as summe ');
    query3.sql.Add('FROM uibeventaccountreport a ');
    query3.sql.Add('where ');
    query3.sql.Add('(a.event = :event) ');
    query3.sql.Add('and(a.dateday >= :start) ');
    query3.sql.Add('and (a.dateday <= :stop) ');
    query3.sql.Add('group by 1  ');
    query3.ParamByName('event').AsString := suchevent;
    query3.parambyname('start').AsString := vonstr;
    query3.parambyname('stop').AsString := bisstr;
    query3.Open;
    DataSource3.Enabled := True;
    summe_h := query3.FieldByName('summe').AsFloat;

    //query free
    if QueryUEARhelper.Active then
      QueryUEARhelper.Close;
    QueryUEARhelper.SQL.Clear;
    QueryUEARhelper.sql.Add(' select time_h, acc_per_monthnum ');
    QueryUEARhelper.sql.Add('    from uiballevent');
    QueryUEARhelper.sql.Add('    where (event = :suchevent)');
    QueryUEARhelper.parambyname('suchevent').AsString := suchevent;
    QueryUEARhelper.Open;
    monthnum := 0;
    total := 0;
    if not QueryUEARhelper.FieldByName('time_h').IsNull then
    begin
      total := QueryUEARhelper.FieldByName('time_h').AsFloat;
      monthnum := round(QueryUEARhelper.FieldByName('acc_per_monthnum').AsFloat);
    end;
    QueryUEARhelper.Close;

    //calculate ex gratia  (Kulanz)
    if (monthnum > 0) and (total > 0.9) then
    begin
      // there are real free minutes
      freed := total - summe_h;
      // we give about 5,4 Minutes (= 0.09 h) ex gratia if
      // this make possible to fit to the free hours
      if (freed < 0) and (freed > -0.09) then // give ex gratia
      begin
        query1.Append;
        // acc_rep_id will be reset by trigger
        query1.FieldByName('acc_rep_id').AsInteger := 0;
        query1.FieldByName('USERID').AsString := 'accounting';
        query1.FieldByName('DATEDAY').AsDateTime := StrToDate(repbisstr);
        query1.FieldByName('EVENT').AsString := suchevent;
        query1.FieldByName('STUNDEN').AsFloat := freed;
        query1.FieldByName('DESCRIPTION').AsString := 'Kulanz / Ex gratia';
        query1.FieldByName('locked').AsInteger := 0;
        query1.Post;
        query1.ApplyUpdates;
        DataModule1.SQLTransaction1.CommitRetaining;
      end;
    end;


  finally
    screen.Cursor := crDefault;
    Application.ProcessMessages;
    DataModule1.debugOut(6, 'end StringGrid1DblClick');
  end;
end;

procedure TFuibtime2erp.StringGrid1DrawCell(Sender: TObject;
  aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
var
  CellText: string;
  TextStyle: TTextStyle;
begin
  with TStringGrid(Sender) do
  begin
    if (gdFixed in aState) or (gdSelected in aState) then
    //Canvas.Brush.Color := clBtnFace
    else
    begin
      Canvas.Brush.Color := clWindow;
      //if (Rows[aRow][2] = '+') and (Rows[aRow][3] = '+') and
      if (Rows[aRow][3] = '+') and filter('markiere ganzes Interval') then
        Canvas.Brush.Color := clMoneyGreen;
      //Canvas.FillRect(aRect);
      //Canvas.TextOut(aRect.Left + 2, aRect.Top + 2, Cells[ACol, ARow]);
    end;
    CellText := Cells[ACol, ARow];
    Canvas.FillRect(ARect);
    TextStyle := Canvas.TextStyle;
    TextStyle.Alignment := taRightJustify;
    Canvas.TextRect(ARect, 0, 0, CellText + '  ', TextStyle);
  end;
end;

procedure TFuibtime2erp.BtnLastMonthClick(Sender: TObject);
var
  startmonth, year, startyear, month, day: word;
begin
  decodeDate(now, year, month, day);
  startmonth := month - 1;
  startyear := year;
  if startmonth = 0 then
  begin
    startmonth := 12;
    startyear := year - 1;
  end;
  edit1.Text := datetostr(encodedate(startyear, startmonth, 1));
  edit2.Text := datetostr(encodedate(year, month, 1));
end;

procedure TFuibtime2erp.BitBtn1Click(Sender: TObject);
begin
  hide;
end;

procedure TFuibtime2erp.BitBtnEditExportErrorTableClick(Sender: TObject);
begin
  if DataModule1.Query4Result.Active then
    DataModule1.Query4Result.Close;
  DataModule1.Query4Result.ReadOnly := False;
  DataModule1.Query4Result.SQL.Clear;
  DataModule1.Query4Result.sql.Add('select * from uibaccountexport ');
  DataModule1.Query4Result.sql.Add('where erperror = 1 ');
  DataModule1.Query4Result.Open;
  Fresult := TFResult.Create(self);
  Fresult.DataSource1.Enabled := False;
  //Fresult.ListBox1.Clear;
  //Fresult.DataSource1.DataSet := DataModule1.Query4Result;
  FResult.QueryResult := DataModule1.Query4Result;
  Fresult.DataSource1.AutoEdit := True;
  Fresult.DataSource1.Enabled := True;
  FResult.Edit1.Text := 'uibaccountexport';
  //FResult.Label2.Caption:=inttostr(DataModule1.Query4Result.RecordCount);
  Fresult.showmodal();
  Fresult.Free;
  DataModule1.Query4Result.Close;
end;

procedure TFuibtime2erp.BitBtnExp2ErpClick(Sender: TObject);
var
  fromday, untilday: Tdate;
  stunden: double;
begin
  try
    query1.First;
    while not query1.EOF do
    begin
      query1.Edit;
      query1.FieldByName('locked').AsInteger := 1;
      query1.Next;
    end;
    if query1.State in [dsEdit, dsInsert] then
      query1.Post;

    createReport('show', 'Final');
    createReport('pdf', 'Final');
    if QueryUEARhelper.Active then
      QueryUEARhelper.Close;
    QueryUEARhelper.SQL.Clear;
    QueryUEARhelper.sql.Add('insert into uibaccountexport ');
    QueryUEARhelper.sql.Add(
      '(event, fromday, untilday, stunden, inerp, erperror, erp_errorstr) ');
    QueryUEARhelper.sql.Add('values(:event, :fromday, :untilday, :stunden, 0, 0, "") ');
    QueryUEARhelper.ParamByName('event').AsString := suchevent;
    fromday := StrToDate(repvonstr);
    untilday := StrToDate(repbisstr);
    stunden := query3.FieldByName('summe').AsFloat;
    QueryUEARhelper.ParamByName('fromday').AsDate := fromday;
    QueryUEARhelper.ParamByName('untilday').AsDate := untilday;
    QueryUEARhelper.ParamByName('stunden').AsFloat := stunden;
    QueryUEARhelper.ExecSQL;
    checkbox1.Checked := False;
    stringgrid1.DeleteRow(StringGrid1.Row);
  except
    on e: Exception do
    begin
      DataModule1.debugOut(3, '', 'exception in uib2erp.BtnExp2Erp');
      DataModule1.debugOut(3, e.Message);
      raise;
    end;
  end;
end;

procedure TFuibtime2erp.BitBtnShowexportTableClick(Sender: TObject);
begin
  if DataModule1.Query4Result.Active then
    DataModule1.Query4Result.Close;
  if CheckBoxAccountexport.Checked then
    DataModule1.Query4Result.ReadOnly := True;
  DataModule1.Query4Result.SQL.Clear;
  DataModule1.Query4Result.sql.Add('select * from uibaccountexport ');
  DataModule1.Query4Result.SQL.Add('order by acc_exp_id desc ');
  DataModule1.Query4Result.Open;
  Fresult := TFResult.Create(self);
  Fresult.DataSource1.Enabled := False;
  //Fresult.ListBox1.Clear;
  Fresult.DataSource1.DataSet := DataModule1.Query4Result;
  Fresult.DataSource1.AutoEdit := False;
  Fresult.DataSource1.Enabled := True;
  FResult.Edit1.Text := 'uibaccountexport';
  //FResult.Label2.Caption:=inttostr(DataModule1.Query4Result.RecordCount);
  Fresult.showmodal();
  Fresult.Free;
  DataModule1.Query4Result.Close;
  DataModule1.Query4Result.ReadOnly := False;
  CheckBoxAccountexport.Checked := True;
end;

procedure TFuibtime2erp.BtnCreateInterimReportClick(Sender: TObject);
begin
  createReport('show', 'Zwischenbericht');
  query1.First;
  while not query1.EOF do
  begin
    query1.Edit;
    query1.FieldByName('locked').AsInteger := 1;
    query1.Next;
  end;
  if query1.State in [dsEdit, dsInsert] then
    query1.Post;
end;


procedure TFuibtime2erp.createReport(output, comment: string);
var
  title: string;
  mypath: string;
  summe_h: double;
  presumme_htd, summe_htd, free_htd: TDateTime;
  months: integer;
  //base: string;
  total, freed: double;
  hours, minutes: cardinal;
  sign: string;
  filename, uibname, dirname: string;
  len, posi: integer;
  reversestr: string;
  isQuotaReport: boolean;
  projectstart, projectend: TDateTime;
  presumme_h: double;
  quota_lifetime_month: integer;
begin
  try
    isQuotaReport := False;
    // query isQuotaReport
    QueryUEARhelper.Close;
    if QueryUEARhelper.Active then
      QueryUEARhelper.Close;
    QueryUEARhelper.SQL.Clear;
    QueryUEARhelper.sql.Add(' select projectstart, quota_lifetime_month ');
    QueryUEARhelper.sql.Add('    from uiballevent');
    QueryUEARhelper.sql.Add('    where (event = :suchevent)');
    QueryUEARhelper.parambyname('suchevent').AsString := suchevent;
    QueryUEARhelper.Open;
    if not QueryUEARhelper.FieldByName('quota_lifetime_month').IsNull then
    begin
      quota_lifetime_month := round(QueryUEARhelper.FieldByName(
        'quota_lifetime_month').AsFloat);
      if quota_lifetime_month > 0 then
        isQuotaReport := True;
      projectstart := QueryUEARhelper.FieldByName('projectstart').AsDateTime;
    end;
    QueryUEARhelper.Close;

    if isQuotaReport then
    begin
      if QueryUEARhelper.Active then
        QueryUEARhelper.Close;
      QueryUEARhelper.SQL.Clear;
      QueryUEARhelper.sql.Add('select ');
      QueryUEARhelper.sql.Add('event, sum(stunden) as summe ');
      QueryUEARhelper.sql.Add('FROM uibeventaccountreport a ');
      QueryUEARhelper.sql.Add('where ');
      QueryUEARhelper.sql.Add('(a.event = :event) ');
      QueryUEARhelper.sql.Add('and(a.dateday >= :start) ');
      QueryUEARhelper.sql.Add('and (a.dateday <= :stop) ');
      QueryUEARhelper.sql.Add('group by 1  ');
      QueryUEARhelper.ParamByName('event').AsString := suchevent;
      QueryUEARhelper.parambyname('start').AsDateTime := projectstart;
      QueryUEARhelper.parambyname('stop').AsString := vonstr;
      QueryUEARhelper.Open;
      presumme_h := QueryUEARhelper.FieldByName('summe').AsFloat;
      QueryUEARhelper.Close;
    end;

    // query edit buffer
    if QueryUibeventaccountreport.Active then
      QueryUibeventaccountreport.Close;
    QueryUibeventaccountreport.SQL.Clear;
    QueryUibeventaccountreport.sql.Add('select ');
    QueryUibeventaccountreport.sql.Add(
      'userid, dateday as datum , stunden, description, ');
    QueryUibeventaccountreport.sql.Add('((round(a.STUNDEN*60) /60) / 24)  as sumtime ');
    QueryUibeventaccountreport.sql.Add('FROM uibeventaccountreport a ');
    QueryUibeventaccountreport.sql.Add('where ');
    QueryUibeventaccountreport.sql.Add('(a.event = :event) ');
    QueryUibeventaccountreport.sql.Add('and(a.dateday >= :start) ');
    QueryUibeventaccountreport.sql.Add('and (a.dateday <= :stop) ');
    QueryUibeventaccountreport.sql.Add('order by datum ');
    QueryUibeventaccountreport.ParamByName('event').AsString := suchevent;
    QueryUibeventaccountreport.parambyname('start').AsString := vonstr;
    QueryUibeventaccountreport.parambyname('stop').AsString := bisstr;
    QueryUibeventaccountreport.Open;
    //DataSource1.Enabled := True;

    // query summe
    if QueryUEARhelper.Active then
      QueryUEARhelper.Close;
    QueryUEARhelper.SQL.Clear;
    QueryUEARhelper.sql.Add('select ');
    QueryUEARhelper.sql.Add('event, sum(stunden) as summe ');
    QueryUEARhelper.sql.Add('FROM uibeventaccountreport a ');
    QueryUEARhelper.sql.Add('where ');
    QueryUEARhelper.sql.Add('(a.event = :event) ');
    QueryUEARhelper.sql.Add('and(a.dateday >= :start) ');
    QueryUEARhelper.sql.Add('and (a.dateday <= :stop) ');
    QueryUEARhelper.sql.Add('group by 1  ');
    QueryUEARhelper.ParamByName('event').AsString := suchevent;
    QueryUEARhelper.parambyname('start').AsString := vonstr;
    QueryUEARhelper.parambyname('stop').AsString := bisstr;
    QueryUEARhelper.Open;
    summe_h := QueryUEARhelper.FieldByName('summe').AsFloat;
  (*
  hours := trunc(summe_h);
  minutes := Round(frac(summe_h) * 60);
  if minutes >= 60 then
  begin
    minutes := minutes - 60;
    hours := hours + 1;
  end;
  summe_htd := EncodeTimeInterval(hours, minutes, 0, 0);
  *)
    QueryUEARhelper.Close;

    if QueryUEARhelper.Active then
      QueryUEARhelper.Close;
    QueryUEARhelper.SQL.Clear;
    QueryUEARhelper.sql.Add(' select time_h, acc_per_monthnum ');
    QueryUEARhelper.sql.Add('    from uiballevent');
    QueryUEARhelper.sql.Add('    where (event = :suchevent)');
    QueryUEARhelper.parambyname('suchevent').AsString := suchevent;
    QueryUEARhelper.Open;
    months := 0;
    total := 0;
    if not QueryUEARhelper.FieldByName('time_h').IsNull then
    begin
      total := QueryUEARhelper.FieldByName('time_h').AsFloat;
      months := round(QueryUEARhelper.FieldByName('acc_per_monthnum').AsFloat);
    end;
    QueryUEARhelper.Close;
  {$IFDEF Linux}
    mypath := '/usr/share/uibtime/';
    if not FileExists(mypath + 'workrep.lrf') then
      // path for development:
      mypath := ExtractFilePath(ParamStr(0));
  {$ELSE}
    mypath := ExtractFilePath(ParamStr(0));
  {$ENDIF Linux}
    frDBDataSet1.DataSet := QueryUibeventaccountreport;
    frReport1.Clear;
    if isQuotaReport then
    begin
      frReport1.LoadFromFile(mypath + 'uib2erp_quota_workrep.lrf');
      projectend := IncMonth(projectstart, quota_lifetime_month);
      frReport1.FindObject('memoQuota').Memo.Text :=
        'Projektstart: ' + DateToStr(projectstart) + ' bis: ' + DateToStr(projectend) +
        ' mit Stunden: ' + timeFloatTohourminutesStr(total);
      //+IntToStr(trunc(Total)) + ':' + Format('%.*d', [2, round(frac(Total) * 60)]);
    end
    else
      frReport1.LoadFromFile(mypath + 'uib2erp_workrep.lrf');
    //frReport1.Dataset := QueryUibeventaccountreport;
    title := 'opsi Tätigkeitsbericht für: ' + suchevent + ' ';
    //if not (combobox1.Text = 'Summe Alle') then
    //  title := title + 'und user: ' + uid;
    title := title + ' ' + comment;
    frReport1.FindObject('memoTitle').Memo.Text := title;
    frReport1.FindObject('memoStartEnd').Memo.Text :=
      'Von ' + vonstr + ' bis (incl.) ' + repbisstr;
    frReport1.FindObject('memosumhm').Memo.Text :=
      IntToStr(trunc(summe_h)) + ':' + Format('%.*d', [2, round(frac(summe_h) * 60)]);
    //FormatDateTime('hh:nn', summe_htd);
    //frReport1.FindObject('memofreistunden').Memo.Text :=
    //  IntToStr(round(Total)) + ' pro ' + IntToStr(months) + ' Monat(e)';
    if isQuotaReport then
    begin
      frReport1.FindObject('memo_hfromstart').Memo.Text :=
        IntToStr(trunc(presumme_h)) + ':' +
        Format('%.*d', [2, round(frac(presumme_h) * 60)]);

      frReport1.FindObject('memo_GS').Memo.Text :=
        IntToStr(trunc(presumme_h + summe_h)) + ':' +
        Format('%.*d', [2, round(frac(presumme_h + summe_h) * 60)]);

      frReport1.FindObject('memofreistunden').Memo.Text :=
        IntToStr(trunc(Total)) + ':' + Format('%.*d', [2, round(frac(Total) * 60)]);

      // we need freed without sign
      freed := abs(total - (presumme_h + summe_h));
      sign := '+ ';
      if (presumme_h + summe_h) > total then
        sign := ' -';

      frReport1.FindObject('memoDiff').Memo.Text :=
        sign + IntToStr(trunc(freed)) + ':' + Format('%.*d', [2, round(frac(freed) * 60)]);
    end
    else
    begin
      frReport1.FindObject('memofreistunden').Memo.Text :=
        IntToStr(trunc(Total)) + ':' + Format('%.*d', [2, round(frac(Total) * 60)]) +
        ' pro ' + IntToStr(months) + ' Monat(e)';
      // we need freed without sign
      freed := abs(total - summe_h);
      sign := '+ ';
      if summe_h > total then
        sign := ' -';

      frReport1.FindObject('memoDiff').Memo.Text :=
        sign + IntToStr(trunc(freed)) + ':' + Format('%.*d', [2, round(frac(freed) * 60)]);
    end;
    //sign + FormatDateTime('hh:nn', free_htd - summe_htd);
    frReport1.DefExportFileName := suchevent + '_' + vonstr + '_' + bisstr + '.pdf';
    //SelectDirectoryDialog1.FileName:=suchevent+'_'+vonstr+'_'+bisstr+ '.pdf';
    if output = 'show' then
    begin
      try
        frReport1.ShowReport;
      except
      end;
    end;
    if output = 'pdf' then
    begin
      try
        // we create the filename by cutting the 'extension' eg. .suport , .psusp , ...
        len := length(suchevent);
        reversestr := ReverseString(suchevent);
        posi := Pos('.', reversestr);
        if posi > 0 then
          len := len - posi;
        uibname := Copy(suchevent, 1, len);
        // use uibtime name as filename (for multiple events at one uibname)
        //filename := uibname+'_'+Copy(suchevent, len+2, Length(suchevent));
        filename := suchevent + '_' + repfilevonstr + '_' + repfilebisstr + '.pdf';
        dirname := EditButtonExportDir.Text + PathDelim + uibname;
        if not DirectoryExists(dirname) then
          dirname := EditButtonExportDir.Text + PathDelim + 'report';
        if frReport1.PrepareReport then
          frReport1.ExportTo(TFrTNPDFExportFilter, dirname + PathDelim + filename);
        //EditButtonExportDir.Text + PathDelim + suchevent+'_'+vonstr+'_'+bisstr+ '.pdf');
      except
      end;
    end;
    QueryUibeventaccountreport.Close;
  except
    on e: Exception do
    begin
      DataModule1.debugOut(3, '', 'exception in uib2erp.createreport');
      DataModule1.debugOut(3, e.Message);
      raise;
    end;
  end;
end;

procedure TFuibtime2erp.StringGrid1GetCellHint(Sender: TObject;
  ACol, ARow: integer; var HintText: string);
var
  CellText: string;
begin
  with TStringGrid(Sender) do
  begin
    HintText := '';
    CellText := Cells[ACol, ARow];
    if CellText = 'event' then
      HintText := '';
    if CellText = 'report' then
      HintText := 'reportrequired ist gesetzt';
    if CellText = 'startf' then
      HintText :=
        'Innerhalb des Abfragezeitraums wurde der Start eines Freistunden-Intervalls gefunden.';
    if CellText = 'endf' then
      HintText :=
        'Innerhalb des Abfragezeitraums wurde das Ende eines Freistunden-Intervalls gefunden.';
    if CellText = 'start' then
      HintText := 'Start des Abfragezeitraums oder Start des Freistunden-Intervalls.';
    if CellText = 'end' then
      HintText := 'Ende des Abfragezeitraums.';
    if CellText = 'prjstart' then
      HintText := 'Projektstart so vorhanden';
    if CellText = 'basem' then
      HintText := 'Dauer des Freistunden-Intervalls in Monaten.';
    if CellText = 'frei' then
      HintText := 'Anzahl der Freistunden (pro Intervall)';
    if CellText = 'used' then
      HintText := 'Angefallenen Stunden im Zeitraum start - end.';
    if CellText = 'rest' then
      HintText := 'Verbleibende Stunden (frei - used)';
  end;
end;

procedure TFuibtime2erp.StringGrid1ShowHint(Sender: TObject; HintInfo: PHintInfo);
begin

end;


procedure TFuibtime2erp.BtnCreateReportClick(Sender: TObject);
begin
  createReport('show', 'Vorläufig');
end;

procedure TFuibtime2erp.BtnDh2HMClick(Sender: TObject);
var
  mytime: TDateTime;
  dechours: double;
  HH, MM, SS, MS: word;
begin
  dechours := StrToFloat(MaskEditDh.Text);
  mytime := dechours / 24;
  DecodeTime(mytime, HH, MM, SS, MS);
  MaskEditHm.Text := format('%d:%d', [HH, MM]);
end;

procedure TFuibtime2erp.BtnHm2DhClick(Sender: TObject);
var
  mytime: TDateTime;
  dechours: double;
begin
  mytime := StrToTime(MaskEditHm.Text);
  dechours := mytime * 24;
  MaskEditDh.Text := format('%n', [dechours]);
end;

procedure TFuibtime2erp.BtnLastWeekClick(Sender: TObject);
var
  startmonth, year, startyear, month, startday, day, weekday, endday,
  endmonth, endyear: word;
begin
  decodeDate(now, year, month, day);
  weekday := DayOfTheWeek(now);
  decodeDate(now - 7 - weekday + 1, startyear, startmonth, startday);
  decodeDate(now - 7 + (7 - weekday) + 1, endyear, endmonth, endday);
  edit1.Text := datetostr(encodedate(startyear, startmonth, startday));
  edit2.Text := datetostr(encodedate(endyear, endmonth, endday));
end;

procedure TFuibtime2erp.BtnExportZeroAccountingClick(Sender: TObject);
var
  event, starttime, stoptime: string;
  (*
  laststartt, laststopt, startt, stopt: TDateTime;
  sumtime, firststartt: TDatetime;
  uname, event, starttime, stoptime: string;
  year, month, day: word;
  querystartdt, queryenddt, vondate, bisdate,
  *)
  startt, stopt: TDateTime;
  sumtime: TDatetime;
  lastIntervalStart, lastIntervalEnd: TDateTime;
  totaltime, usedtime, availabletime: TDateTime;
  intervalStartFound, intervalEndFound: boolean;
  reportrequired, accountingrequired: boolean;
  avaiablesign: char;
  totaltimestr, usedtimestr, availabletimestr, querystart, queryend: string;
  basemonth: integer;
  projektstart: TDateTime;
  //i: integer;
  Show: boolean;
  fromday, untilday: Tdate;
  numberexported: integer = 0;
  numberselected: integer = 0;

begin
  try
    try
      screen.Cursor := crHourGlass;
      sumtime := 0;
      starttime := edit1.Text;
      startt := StrToDate(starttime);
      stoptime := edit2.Text;
      stopt := StrToDate(stoptime);
      // query for event that need accounting
      if queryAccEv.Active then
        queryAccEv.Close;
      queryAccEv.sql.Clear;
      queryAccEv.sql.Add('select event from uiballevent where');
      queryAccEv.sql.Add(' ((accountingrequired = 1)');
      queryAccEv.sql.Add(' or (reportrequired = 1))');
      queryAccEv.sql.Add(' and ((event in ');
      queryAccEv.sql.Add(
        ' (select distinct event from UIBEVENT where starttime > :start)) ');
      queryAccEv.sql.Add(' or (event in (select distinct event from uibaktevent))) ');
      queryAccEv.sql.Add(' order by 1');
      // start 3 Month (90 days) before querystart
      queryAccEv.ParamByName('start').AsDate := startt - 90;
      queryAccEv.Open;
      while not queryAccEv.EOF do
      begin
        event := queryAccEv.FieldByName('event').AsString;
        if event <> '' then
        begin
          if getLastIntervalInfo(event, starttime, stoptime,
            lastIntervalStart, lastIntervalEnd, intervalStartFound,
            intervalEndFound, reportrequired, accountingrequired,
            basemonth, projektstart, totaltime, usedtime, availabletime,
            avaiablesign) then
          begin
            Inc(numberselected);

            // Alles was kleiner als 0.001 (1,5 Minuten) ist, wird ignoriert
            if usedtime < 0.001 then
              usedtime := 0;

            DateTimeToString(totaltimestr, 'hh:nn', totaltime);
            DateTimeToString(usedtimestr, 'hh:nn', usedtime);
            DateTimeToString(availabletimestr, avaiablesign + 'hh:nn', availabletime);
            querystart := starttime;
            queryend := stoptime;


            Show := True;
            if (usedtime <> 0) then
              Show := False;

            if Show then
            begin
              if intervalEndFound then
              begin
                if True then
                begin
                  // we have to query the whole accounting interval
                  querystart := DateToStr(lastIntervalStart);
                end;
                queryend := DateToStr(lastIntervalEnd);
              end
              else
                Show := False;
            (*
            if intervalStartFound then
            begin
              // we will start at the beginning of the accounting interval
              querystart := DateToStr(lastIntervalStart);
            end
            else
              Show := False;
              *)
              if queryUAE.Active then
                queryUAE.Close;
              QueryUAE.SQL.Clear;
              queryUAE.SQL.Add('select * from uibaccountexport ');
              queryUAE.SQL.Add('where ');
              queryUAE.SQL.Add('(event = :event) and ');
              queryUAE.SQL.Add('(fromday = :start) ');
              //queryUAE.SQL.Add(' and (untilday = :stop)');
              queryUAE.ParamByName('event').AsString := event;
              queryUAE.ParamByName('start').AsDate := StrToDate(querystart);
              //queryUAE.ParamByName('stop').AsDate := StrToDate(queryend) - 1;
              queryUAE.ReadOnly := True;
              queryUAE.Open;
            end;
            if filter('keine Abgerechneten') then
              if queryUAE.RecordCount >= 1 then
                Show := False;
            queryUAE.Close;
            if Show then
            begin
              if queryUAE.Active then
                queryUAE.Close;
              QueryUAE.SQL.Clear;
              queryUAE.SQL.Add('insert into uibaccountexport ');
              QueryUAE.sql.Add(
                '(event, fromday, untilday, stunden, inerp, erperror, erp_errorstr) ');
              QueryUAE.sql.Add(
                'values(:event, :fromday, :untilday, :stunden, 0, 0, "") ');
              QueryUAE.ParamByName('event').AsString := event;
              fromday := StrToDate(querystart);
              untilday := StrToDate(queryend) - 1;
              QueryUAE.ParamByName('fromday').AsDate := fromday;
              QueryUAE.ParamByName('untilday').AsDate := untilday;
              QueryUAE.ParamByName('stunden').AsFloat := usedtime;
              try
                QueryUAE.ExecSQL;
              except
                ShowMessage('Kann nicht einfügen: ' + event + ' vom: ' +
                  querystart + ' bis: ' + DateToStr(untilday));
              end;
              Inc(numberexported);
              DataModule1.debugOut(5, 'ExportZero', 'exp: ' + event +
                ' ' + DateToStr(fromday) + ' - ' + DateToStr(untilday));
            end;
          end;
        end;
        queryAccEv.Next;
      end;
      queryAccEv.Close;
      StatusBar1.SimpleText :=
        'Null Stunden Datensätze exportiert: ' + IntToStr(numberexported) +
        ' von ' + IntToStr(numberselected);
    except
      on e: Exception do
      begin
        DataModule1.debugOut(3, '',
          'exception in uibtime2erp.BtnExportZeroAccountingClick');
        DataModule1.debugOut(3, e.Message);
        raise;
      end;
    end;
  finally
    screen.Cursor := crDefault;
  end;
end;

function TFuibtime2erp.filter(flt: string): boolean;
var
  i: integer = 0;
begin
  Result := False;
  i := CheckGroup1.Items.IndexOf(flt);
  if i > -1 then
    Result := CheckGroup1.Checked[i];
end;


procedure TFuibtime2erp.BtnLoadRequiredReportsClick(Sender: TObject);
var
  event, starttime, stoptime: string;

  (*
  laststartt, laststopt, startt, stopt: TDateTime;
  sumtime, firststartt: TDatetime;
  uname, event, starttime, stoptime: string;
  year, month, day: word;
  querystartdt, queryenddt, vondate, bisdate,
  *)
  sumtime, startt, stopt: TDatetime;
  lastIntervalStart, lastIntervalEnd: TDateTime;
  totaltime, usedtime, availabletime: TDateTime;
  intervalStartFound, intervalEndFound: boolean;
  reportrequired, accountingrequired: boolean;
  avaiablesign: char;
  totaltimestr, usedtimestr, availabletimestr, querystart, queryend: string;
  basemonth: integer;
  projektstart: TDateTime;
  i: integer;
  Show: boolean;
  numberexported: integer = 0;
  numberselected: integer = 0;
  numcols: integer = 0;
  projectStartStr: string;
begin
  try
    try
      screen.Cursor := crHourGlass;
      for i := 0 to 1024 do
        rowcolor[i] := clWindow;
      StringGrid1.Clear;
      //rowcolor[StringGrid1.RowCount] := clBtnFace;
      StringGrid1.InsertRowWithValues(StringGrid1.RowCount,
        ['event', 'report', 'startf', 'endf', 'start', 'end', 'prjstart',
        'basem', 'frei', 'used', 'rest']);
      StringGrid1.FixedRows := 1;
      StringGrid1.Repaint;
      Application.ProcessMessages;
      // start looking for missing reports
      sumtime := 0;
      starttime := edit1.Text;
      startt := StrToDate(starttime);
      stoptime := edit2.Text;
      stopt := StrToDate(stoptime);
      // query for event that need accounting
      if queryAccEv.Active then
        queryAccEv.Close;
      queryAccEv.sql.Clear;
      queryAccEv.sql.Add('select event from uiballevent where');
      queryAccEv.sql.Add(' ((accountingrequired = 1)');
      queryAccEv.sql.Add(' or (reportrequired = 1))');
      queryAccEv.sql.Add(' and ((event in ');
      queryAccEv.sql.Add(
        ' (select distinct event from UIBEVENT where starttime > :start)) ');
      queryAccEv.sql.Add(' or (event in (select distinct event from uibaktevent))) ');
      queryAccEv.sql.Add(' order by 1');
      // start 3 Month (90 days) before querystart
      queryAccEv.ParamByName('start').AsDate := startt - 90;
      queryAccEv.Open;
      while not queryAccEv.EOF do
      begin
        event := queryAccEv.FieldByName('event').AsString;
        if event <> '' then
        begin
          if getLastIntervalInfo(event, starttime, stoptime,
            lastIntervalStart, lastIntervalEnd, intervalStartFound,
            intervalEndFound, reportrequired, accountingrequired,
            basemonth, projektstart, totaltime, usedtime, availabletime,
            avaiablesign) then
          begin
            Inc(numberselected);

            // Alles was kleiner als 0.001 (1,5 Minuten) ist, wird ignoriert
            if usedtime < 0.001 then
              usedtime := 0;

            totaltimestr := IntToStr(trunc(totaltime * 24)) + ':' +
              Format('%.*d', [2, round(frac(totaltime * 24) * 60)]);
            usedtimestr := IntToStr(trunc(usedtime * 24)) + ':' +
              Format('%.*d', [2, round(frac(usedtime * 24) * 60)]);
            availabletimestr :=
              avaiablesign + IntToStr(trunc(availabletime * 24)) +
              ':' + Format('%.*d', [2, round(frac(availabletime * 24) * 60)]);
            //totaltimestr := FormatDateTime('hh:nn', totaltime, [fdoInterval]);
            //usedtimestr := FormatDateTime('hh:nn', usedtime, [fdoInterval]);
            //availabletimestr := avaiablesign +FormatDateTime('hh:nn', availabletime, [fdoInterval]);
            //DateTimeToString(totaltimestr, 'hh:nn', totaltime);
            //DateTimeToString(usedtimestr, 'hh:nn', usedtime);
            //DateTimeToString(availabletimestr, avaiablesign + 'hh:nn', availabletime);
            querystart := starttime;
            queryend := stoptime;
            projectStartStr := '';
            if projektstart > 0 then
              projectStartStr := DateToStr(projektstart);

            Show := True;
            if (usedtime <= 0) and filter('used > 0') then
              Show := False;

            if Show then
            begin
              if intervalEndFound then
              begin
                if filter('ganzes Interval bei Ende') then
                begin
                  // we have to query the whole accounting interval
                  querystart := DateToStr(lastIntervalStart);
                end;
                queryend := DateToStr(lastIntervalEnd);
                if filter('markiere ganzes Interval') then
                  rowcolor[StringGrid1.RowCount] := clMoneyGreen;
              end
              else
              if intervalStartFound then
              begin
                // we will start at the beginning of the accounting interval
                querystart := DateToStr(lastIntervalStart);
              end;
              if queryUAE.Active then
                queryUAE.Close;
              QueryUAE.SQL.Clear;
              queryUAE.SQL.Add('select * from uibaccountexport ');
              queryUAE.SQL.Add('where ');
              queryUAE.SQL.Add('(event = :event) and ');
              queryUAE.SQL.Add('(fromday = :start) ');
              //queryUAE.SQL.Add(' and (untilday = :stop) ');
              queryUAE.ParamByName('event').AsString := event;
              queryUAE.ParamByName('start').AsDate := StrToDate(querystart);
              //queryUAE.ParamByName('stop').AsDate := StrToDate(queryend) - 1;
              queryUAE.ReadOnly := True;
              queryUAE.Open;
            end;
            if filter('keine Abgerechneten') then
              if queryUAE.RecordCount >= 1 then
                Show := False;
            queryUAE.Close;
            if Show then
            begin
              StringGrid1.InsertRowWithValues(StringGrid1.RowCount,
                [event, BoolToStr(reportrequired, '+', '-'),
                BoolToStr(intervalStartFound, '+', '-'),
                BoolToStr(intervalEndFound, '+', '-'), querystart,
                queryend, projectStartStr, IntToStr(basemonth),
                totaltimestr, usedtimestr, availabletimestr]);
              Inc(numberexported);
              DataModule1.debugOut(5, 'Load NonZero', 'NonZero: ' +
                event + ' ' + querystart + ' - ' + DateToStr(StrToDate(queryend) - 1));
            end;
          end;
        end;
        queryAccEv.Next;
      end;
      queryAccEv.Close;
      StatusBar1.SimpleText :=
        'NichtNull Stunden Datensätze geladen: ' + IntToStr(numberexported) +
        ' von ' + IntToStr(numberselected);
      stringgrid1.AutoSizeColumn(0);
      stringgrid1.ColWidths[0] := 250;
      StringGrid1.Repaint;
      Application.ProcessMessages;
    except
      on e: Exception do
      begin
        DataModule1.debugOut(3, '',
          'exception in uibtime2erp.BtnLoadRequiredReportsClick');
        DataModule1.debugOut(3, e.Message);
        raise;
      end;
    end;
  finally
    screen.Cursor := crDefault;
  end;
end;

procedure TFuibtime2erp.BtnLockReportDataClick(Sender: TObject);
begin
  query1.First;
  query1.Edit;
  while not query1.EOF do
  begin
    query1.FieldByName('locked').AsInteger := 1;
    query1.Next;
  end;
  if query1.State in [dsEdit, dsInsert] then
    query1.Post;
end;

procedure TFuibtime2erp.BtnThisMonthClick(Sender: TObject);
var
  endmonth, year, endyear, month, day: word;
begin
  decodeDate(now, year, month, day);
  endmonth := month + 1;
  endyear := year;
  if endmonth = 13 then
  begin
    endmonth := 1;
    endyear := year + 1;
  end;
  edit1.Text := datetostr(encodedate(year, month, 1));
  edit2.Text := datetostr(encodedate(endyear, endmonth, 1));
end;

procedure TFuibtime2erp.BtnThisWeekClick(Sender: TObject);
var
  startmonth, year, startyear, month, startday, day, weekday, endday,
  endmonth, endyear: word;
begin
  decodeDate(now, year, month, day);
  weekday := DayOfTheWeek(now);
  decodeDate(now - weekday + 1, startyear, startmonth, startday);
  decodeDate(now + (7 - weekday) + 1, endyear, endmonth, endday);
  edit1.Text := datetostr(encodedate(startyear, startmonth, startday));
  edit2.Text := datetostr(encodedate(endyear, endmonth, endday));
end;

procedure TFuibtime2erp.CheckBox1Change(Sender: TObject);
begin
  if checkbox1.Checked then
    BitBtnExp2Erp.Enabled := True
  else
    BitBtnExp2Erp.Enabled := False;
end;

procedure TFuibtime2erp.EditButtonExportDirChange(Sender: TObject);
begin

end;

procedure TFuibtime2erp.FormCreate(Sender: TObject);
var
  //startmonth, year, startyear, month, day: word;
  year, month, day: word;
begin
  decodeDate(now, year, month, day);
  (*
  combobox1.items.Clear;
  combobox1.items.add(uid);
  combobox1.items.add('Summe Alle');
  combobox1.Text := 'Summe Alle';
  *)
  edit1.Text := '1.1.2016';
  edit2.Text := datetostr(date);
  BtnLastMonthClick(Sender);
end;

procedure TFuibtime2erp.FormShow(Sender: TObject);
var
  //startmonth, year, startyear, month, day: word;
  year, month, day: word;
begin
  decodeDate(now, year, month, day);
end;

procedure TFuibtime2erp.MaskEditDhChange(Sender: TObject);
begin

end;

begin
  for rowcounter := 0 to 1024 do
    rowcolor[rowcounter] := clWindow;
  //(*
  frDBDataSet1 := TfrDBDataSet.Create(Fuibtime2erp);
  frReport1 := TfrReport.Create(Fuibtime2erp);
  frReport1.Dataset := frDBDataSet1;
  //*)
  frTNPDFExport1 := TfrTNPDFExport.Create(Fuibtime2erp);

end.
