unit uibtWorkRepChooser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LR_Class, LR_DBSet,
  //LR_View,
  LR_E_CSV, LR_Desgn,
  lrEmailExportFilter, lr_e_pdf, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls,
  //Spin,
  Buttons, EditBtn, db, sqldb, Variants, DateUtils;


type

  { TFuibtWorkRepChooser }

  TFuibtWorkRepChooser = class(TForm)
    BitBtn1: TBitBtn;
    BtnLastMonth: TButton;
    BtnLastWeek: TButton;
    BtnLoadRequiredReports: TButton;
    BtnLoadRequiredReportsMissing: TButton;
    BtnThisMonth: TButton;
    BtnThisWeek: TButton;
    BtnCreateReport: TButton;
    ButtonExportRep2pdf: TButton;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    EditButtonExportDir: TEditButton;
    frCSVExport1: TfrCSVExport;
    frDBDataSet1: TfrDBDataSet;
    frReport1: TfrReport;
    frTNPDFExport1: TfrTNPDFExport;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ListBox1: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    query1: TSQLQuery;
    query2: TSQLQuery;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    procedure BitBtn1Click(Sender: TObject);
    procedure BtnCreateReportClick(Sender: TObject);
    procedure BtnLastMonthClick(Sender: TObject);
    procedure BtnLastWeekClick(Sender: TObject);
    procedure BtnLoadRequiredReportsClick(Sender: TObject);
    procedure BtnLoadRequiredReportsMissingClick(Sender: TObject);
    procedure BtnThisMonthClick(Sender: TObject);
    procedure BtnThisWeekClick(Sender: TObject);
    procedure ButtonExportRep2pdfClick(Sender: TObject);
    procedure EditButtonExportDirButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure createReport(output : string);
    procedure frDesigner1LoadReport(Report: TfrReport; var ReportName: String);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FuibtWorkRepChooser: TFuibtWorkRepChooser;

implementation

uses
  uibdata;

{$R *.lfm}


{ TFuibtWorkRepChooser }

procedure TFuibtWorkRepChooser.BtnLoadRequiredReportsClick(Sender: TObject);
var
 laststartt, laststopt, startt, stopt : TDateTime;
 sumtime, firststartt : TDatetime;
 uname, event, starttime, stoptime : String;
 year, month, day: word;
begin
  listBox1.Clear;
  // start looking for all reports
  sumtime := 0;
  starttime := edit1.Text;
  startt := StrToDate(starttime);
  stoptime := edit2.Text;
  stopt := StrToDate(stoptime);
  if Datamodule1.SQQueryuibevent.Active then Datamodule1.SQQueryuibevent.Close;
  Datamodule1.SQQueryuibevent.sql.Clear;
  Datamodule1.SQQueryuibevent.sql.Add('select * from uibevent ');
  Datamodule1.SQQueryuibevent.sql.Add('where ');
  if not (combobox1.Text = 'Summe Alle') then
    Datamodule1.SQQueryuibevent.sql.Add('(userid = :uid) and ');
  Datamodule1.SQQueryuibevent.sql.Add('(starttime >= :start) and ');
  Datamodule1.SQQueryuibevent.sql.Add('(starttime < :stop) ');
  Datamodule1.SQQueryuibevent.sql.Add('order by starttime ');
  if not (combobox1.Text = 'Summe Alle') then
    Datamodule1.SQQueryuibevent.ParamByName('uid').AsString := uid;
  Datamodule1.SQQueryuibevent.ParamByName('start').AsDateTime := startt;
  Datamodule1.SQQueryuibevent.ParamByName('stop').AsDateTime := stopt;
  Datamodule1.SQQueryuibevent.open;
  Datamodule1.SQQueryuibevent.first;
  laststartt := Datamodule1.SQQueryuibevent.fieldbyname('starttime').asdatetime;
  firststartt := laststartt;
  laststopt := Datamodule1.SQQueryuibevent.fieldbyname('stoptime').asdatetime;
  if not (combobox1.Text = 'Summe Alle') then
    Datamodule1.SQuibevent.Locate('starttime;stoptime', VarArrayOf([laststartt,laststopt]), [loCaseInsensitive,loPartialKey])
  else
    Datamodule1.SQuibevent.Locate('userid;starttime;stoptime', VarArrayOf([uid,laststartt,laststopt]), [loCaseInsensitive,loPartialKey]);
  //Datamodule1.SQQueryuibevent.close;
  sumtime := laststopt - laststartt;
  //Datamodule1.SQuibevent.next;
  // query for event that need a report
  query1.sql.Clear;
  query1.sql.Add('select event from uibaktevent where');
  query1.sql.Add(' reportrequired = 1');
  query1.open;
  decodeDate(laststartt, year, month, day);
  query2.sql.Clear;
  query2.sql.Add('select * from UIB_WORK_DESCRIPTION where ');
  query2.sql.Add('(jahr >= :year) and ');
  query2.sql.Add('(monat >= :month) and ');
  query2.sql.Add('(tag >= :day) and ');
  if not (combobox1.Text = 'Summe Alle') then
    query2.sql.Add('(userid = :uid) and ');
  query2.sql.Add('(not (description = ""))  ');
  query2.ParamByName('year').AsInteger := year;
  query2.ParamByName('month').AsInteger := month;
  query2.ParamByName('day').AsInteger := day;
  if not (combobox1.Text = 'Summe Alle') then
    query2.ParamByName('uid').AsString := uid;
  query2.open;

  while not Datamodule1.SQQueryuibevent.eof do
  begin
   uname := Datamodule1.SQQueryuibevent.fieldbyname('userid').asString;
   startt := Datamodule1.SQQueryuibevent.fieldbyname('starttime').asdatetime;
   event := Datamodule1.SQQueryuibevent.fieldbyname('event').asString;
   decodeDate(startt, year, month, day);
   if  query1.Locate('event',event,[loCaseInsensitive]) then
   begin
     if ListBox1.Items.IndexOf(event) < 0 then
       ListBox1.Items.Add(event);
    (*
    // needs report
     if query2.Locate('userid;jahr;monat;tag;event',
                          VarArrayOf([uid, year, month, day, event]),[loCaseInsensitive]) then
     begin
       // report found
       memo1.lines.add('..............Report gefunden : ' + uname + ' -> '+DateTimeToStr(startt)+' : '+event);
     end
     else
     begin
       // no report found
       memo1.lines.add('#*#*#*#*#  -> Report fehlt : ' + uname + ' -> '+DateTimeToStr(startt)+' : '+event);
     end;
     *)
   end;
   Datamodule1.SQQueryuibevent.next;
  end;
  query1.close;
  query2.close;
  Datamodule1.SQQueryuibevent.close;

  // end looking for missing reports

end;

procedure TFuibtWorkRepChooser.BtnLoadRequiredReportsMissingClick(
  Sender: TObject);
var
 laststartt, laststopt, startt, stopt : TDateTime;
 sumtime, firststartt : TDatetime;
 uname, event, starttime, stoptime : String;
 year, month, day: word;
begin
  listBox1.Clear;
  // start looking for missing reports
  sumtime := 0;
  starttime := edit1.Text;
  startt := StrToDate(starttime);
  stoptime := edit2.Text;
  stopt := StrToDate(stoptime);
  if Datamodule1.SQQueryuibevent.Active then Datamodule1.SQQueryuibevent.Close;
  Datamodule1.SQQueryuibevent.sql.Clear;
  Datamodule1.SQQueryuibevent.sql.Add('select * from uibevent ');
  Datamodule1.SQQueryuibevent.sql.Add('where ');
  if not (combobox1.Text = 'Summe Alle') then
    Datamodule1.SQQueryuibevent.sql.Add('(userid = :uid) and ');
  Datamodule1.SQQueryuibevent.sql.Add('(starttime >= :start) and ');
  Datamodule1.SQQueryuibevent.sql.Add('(starttime < :stop) ');
  Datamodule1.SQQueryuibevent.sql.Add('order by starttime ');
  if not (combobox1.Text = 'Summe Alle') then
    Datamodule1.SQQueryuibevent.ParamByName('uid').AsString := uid;
  Datamodule1.SQQueryuibevent.ParamByName('start').AsDateTime := startt;
  Datamodule1.SQQueryuibevent.ParamByName('stop').AsDateTime := stopt;
  Datamodule1.SQQueryuibevent.open;
  Datamodule1.SQQueryuibevent.first;
  laststartt := Datamodule1.SQQueryuibevent.fieldbyname('starttime').asdatetime;
  firststartt := laststartt;
  laststopt := Datamodule1.SQQueryuibevent.fieldbyname('stoptime').asdatetime;
  if not (combobox1.Text = 'Summe Alle') then
    Datamodule1.SQuibevent.Locate('starttime;stoptime', VarArrayOf([laststartt,laststopt]), [loCaseInsensitive,loPartialKey])
  else
    Datamodule1.SQuibevent.Locate('userid;starttime;stoptime', VarArrayOf([uid,laststartt,laststopt]), [loCaseInsensitive,loPartialKey]);
  //Datamodule1.SQQueryuibevent.close;
  sumtime := laststopt - laststartt;
  //Datamodule1.SQuibevent.next;
  // query for event that need a report
  query1.sql.Clear;
  query1.sql.Add('select event from uibaktevent where');
  query1.sql.Add(' reportrequired = 1');
  query1.open;
  decodeDate(laststartt, year, month, day);
  query2.sql.Clear;
  query2.sql.Add('select * from UIB_WORK_DESCRIPTION where ');
  query2.sql.Add('(jahr >= :year) and ');
  query2.sql.Add('(monat >= :month) and ');
  query2.sql.Add('(tag >= :day) and ');
  if not (combobox1.Text = 'Summe Alle') then
    query2.sql.Add('(userid = :uid) and ');
  query2.sql.Add('(not (description = ""))  ');
  query2.ParamByName('year').AsInteger := year;
  query2.ParamByName('month').AsInteger := month;
  query2.ParamByName('day').AsInteger := day;
  if not (combobox1.Text = 'Summe Alle') then
    query2.ParamByName('uid').AsString := uid;
  query2.open;

  while not Datamodule1.SQQueryuibevent.eof do
  begin
   uname := Datamodule1.SQQueryuibevent.fieldbyname('userid').asString;
   startt := Datamodule1.SQQueryuibevent.fieldbyname('starttime').asdatetime;
   event := Datamodule1.SQQueryuibevent.fieldbyname('event').asString;
   decodeDate(startt, year, month, day);
   if  query1.Locate('event',event,[loCaseInsensitive]) then
   begin
     if ListBox1.Items.IndexOf(event) < 0 then
     begin
       if not query2.Locate('userid;jahr;monat;tag;event',
                          VarArrayOf([uname, year, month, day, event]),[loCaseInsensitive]) then
       begin
          ListBox1.Items.Add(event);
       end;
     end;
   end;
   Datamodule1.SQQueryuibevent.next;
  end;
  query1.close;
  query2.close;
  Datamodule1.SQQueryuibevent.close;

  // end looking for missing reports

end;

procedure TFuibtWorkRepChooser.BtnThisMonthClick(Sender: TObject);
var
 endmonth, year, endyear, month, day : word;
begin
 decodeDate(now,year,month,day);
 endmonth := month+1;
 endyear := year;
 if endmonth = 13 then
 begin
  endmonth := 1;
  endyear := year+1;
 end;
 edit1.text := datetostr(encodedate(year,month,1));
 edit2.text := datetostr(encodedate(endyear,endmonth,1));
end;

procedure TFuibtWorkRepChooser.BtnThisWeekClick(Sender: TObject);
var
 startmonth, year, startyear, month, startday, day, weekday,
 endday, endmonth, endyear : word;
begin
 decodeDate(now,year,month,day);
 weekday := DayOfTheWeek(now);
 decodeDate(now-weekday+1,startyear,startmonth,startday);
 decodeDate(now+(7-weekday)+1,endyear,endmonth,endday);
 edit1.text := datetostr(encodedate(startyear,startmonth,startday));
 edit2.text := datetostr(encodedate(endyear,endmonth,endday));
end;

procedure TFuibtWorkRepChooser.ButtonExportRep2pdfClick(Sender: TObject);
begin
  createReport('pdf');
end;


procedure TFuibtWorkRepChooser.EditButtonExportDirButtonClick(Sender: TObject);
begin
  SelectDirectoryDialog1.Execute;
  EditButtonExportDir.Text:= SelectDirectoryDialog1.FileName;
end;


procedure TFuibtWorkRepChooser.BitBtn1Click(Sender: TObject);
begin
 hide;
 close;
end;

procedure TFuibtWorkRepChooser.createReport(output : string);
var
 i : Integer;
 title : string;
 mypath : string;
 summe_h : double;
 summe_htd, free_htd : TDateTime;
 months : integer;
 //base : string;
 total : double;
 hours, minutes : cardinal;
 sign : string;
begin
  for i := 0 to Listbox1.Count -1 do
  begin
    if listbox1.Selected[i] then
    begin
     if query1.Active then query1.Close;
       query1.sql.Clear;
       query1.sql.Add('select ');
       query1.sql.Add('cast( extract(day from a.starttime)||''.''|| extract(month from a.starttime)||''.''|| extract(year from a.starttime) as date) as datum, ');
       query1.sql.Add('a.USERID, ');
       query1.sql.Add('sum(a.STUNDEN) as stunden, ');
       query1.sql.Add('((sum(a.STUNDEN)+0.01) / 24)  as sumtime , ');
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
       if not (combobox1.Text = 'Summe Alle') then
         query1.sql.Add('and (a.userid = :uid) ');
       query1.sql.Add('group by 1,2,5  ');
       query1.ParamByName('event').AsString := listbox1.items[i];
       query1.parambyname('start').asstring := edit1.text;
       query1.parambyname('stop').asstring := edit2.text;
       if not (combobox1.Text = 'Summe Alle') then
         query1.ParamByName('uid').AsString := uid;
       query1.open;
       // Results
       query2.sql.Clear;
       query2.sql.Add('select ');
       query2.sql.Add('sum(UIBEVENT.STUNDEN) as stunden ');
       query2.sql.Add('FROM  uibevent where ');
       query2.sql.Add('(event = :event) ');
       query2.sql.Add('and(starttime >= :start) ');
       query2.sql.Add('and (stoptime <= :stop) ');
       if not (combobox1.Text = 'Summe Alle') then
         query2.sql.Add('and (userid = :uid) ');
       query2.ParamByName('event').AsString := listbox1.items[i];
       query2.parambyname('start').asstring := edit1.text;
       query2.parambyname('stop').asstring := edit2.text;
       if not (combobox1.Text = 'Summe Alle') then
         query2.ParamByName('uid').AsString := uid;
       query2.open;
       summe_h := query2.FieldByName('stunden').AsFloat;
       hours := trunc(summe_h);
       minutes := Round(frac(summe_h)*60);
       if minutes >= 60 then
       begin
         minutes := minutes - 60;
         hours := hours +1;
       end;
       summe_htd := EncodeTimeInterval(hours,minutes,0,0);
       query2.close;
       //
       if query2.Active then query2.Close;
       query2.SQL.Clear;
        query2.sql.Add(' select time_h, acc_per_monthnum ');
        query2.sql.Add('    from uibaktevent');
        query2.sql.Add('    where (event = :suchevent)');
        query2.parambyname('suchevent').AsString := listbox1.items[i];
        query2.Open;
        months := 0;
        total := 0;
        if not query2.FieldByName('time_h').IsNull then
        begin
          total := query2.FieldByName('time_h').AsFloat;
          months := round(query2.FieldByName('acc_per_monthnum').AsFloat);
        end;
        query2.close;

       {$IFDEF Linux}
       mypath := '/usr/share/uibtime/';
       if not FileExists(mypath+'workrep.lrf') then
       // path for development:
       mypath := ExtractFilePath(paramstr(0));
       {$ELSE}
       mypath := ExtractFilePath(paramstr(0));
       {$ENDIF Linux}
       frReport1.Clear;
       frReport1.LoadFromFile(mypath+'workrep.lrf');
       title := 'opsi Tätigkeitsbericht für: '+listbox1.items[i]+ ' ';
       if not (combobox1.Text = 'Summe Alle') then
         title := title +'und user: '+ uid;
       frReport1.FindObject('memoTitle').Memo.Text:= title;
       frReport1.FindObject('memoStartEnd').Memo.Text := 'Von '+edit1.Text+
         ' bis (excl.) '+edit2.text;
       frReport1.FindObject('memosumhm').Memo.Text:= FormatDateTime('hh:nn',summe_htd);
       frReport1.FindObject('memofreistunden').Memo.Text:= IntToStr(round(Total))+' pro '+IntTostr(months)+' Monat(e)';
       hours := trunc(Total);
       minutes := Round(frac(Total)*60);
       if minutes >= 60 then
       begin
         minutes := minutes - 60;
         hours := hours +1;
       end;
       free_htd := EncodeTimeInterval(hours,minutes,0,0);
       sign := '+ ';
       if summe_htd > free_htd then
       begin
         sign := ' -';
         // todo: make font red
         //frReport1.FindObject('memoDiff').
       end;
       frReport1.FindObject('memoDiff').Memo.Text:= sign+FormatDateTime('hh:nn',free_htd - summe_htd);
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
           if frReport1.PrepareReport then
             frReport1.ExportTo(TFrTNPDFExportFilter,
                  EditButtonExportDir.Text+PathDelim+listbox1.items[i]+'.pdf' );
         except
         end;
       end;
       query1.close;
    end;
  end;
end;

procedure TFuibtWorkRepChooser.frDesigner1LoadReport(Report: TfrReport;
  var ReportName: String);
begin

end;


procedure TFuibtWorkRepChooser.BtnCreateReportClick(Sender: TObject);
begin
  createReport('show');
end;


procedure TFuibtWorkRepChooser.BtnLastMonthClick(Sender: TObject);
var
 startmonth, year, startyear, month, day : word;
begin
 decodeDate(now,year,month,day);
 startmonth := month -1;
 startyear := year;
 if startmonth = 0 then
 begin
  startmonth := 12;
  startyear := year-1;
 end;
 edit1.text := datetostr(encodedate(startyear,startmonth,1));
 edit2.text := datetostr(encodedate(year,month,1));
end;

procedure TFuibtWorkRepChooser.BtnLastWeekClick(Sender: TObject);
var
 startmonth, year, startyear, month, startday, day, weekday,
 endday, endmonth, endyear : word;
begin
 decodeDate(now,year,month,day);
 weekday := DayOfTheWeek(now);
 decodeDate(now-7-weekday+1,startyear,startmonth,startday);
 decodeDate(now-7+(7-weekday)+1,endyear,endmonth,endday);
 edit1.text := datetostr(encodedate(startyear,startmonth,startday));
 edit2.text := datetostr(encodedate(endyear,endmonth,endday));
end;

procedure TFuibtWorkRepChooser.FormCreate(Sender: TObject);
var
 //startmonth, year, startyear, month, day : word;
 year, month, day : word;
begin
 decodeDate(now,year,month,day);

 combobox1.items.clear;
 combobox1.items.add(uid);
 combobox1.items.add('Summe Alle');
 combobox1.text := uid;
 edit1.text := '1.1.2016';
 edit2.text := datetostr(date);
end;



end.

