unit logoff;

{$MODE Delphi}

interface

uses
  //LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, DBCtrls, EditBtn, ExtDlgs, ExtCtrls, Db, sqldb, Variants{, IBQuery},
  dateutils,
  {$IFDEF LINUX}
    libnotify,
  {$ENDIF LINUX}
  uibdatetime, RichMemo;

type

  { TFlogoff }

  TFlogoff = class(TForm)
    ButtonLastWeek: TButton;
    ButtonLastMonth: TButton;
    CalendarDialog1: TCalendarDialog;
    EditButton1: TEditButton;
    Label1: TLabel;
    DBText1: TDBText;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    Label3: TLabel;
    orgMemo1: TMemo;
    Label2: TLabel;
    Query1: TSQLQuery;
    Btn_work_description: TBitBtn;
    Query2: TSQLQuery;
    RichMemo1: TRichMemo;
    FlogOffTimer: TTimer;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure ButtonLastMonthClick(Sender: TObject);
    procedure ButtonLastWeekClick(Sender: TObject);
    procedure checkDB(begintime : TDateTime);
    procedure EditButton1ButtonClick(Sender: TObject);
    procedure EditButton1Change(Sender: TObject);
    procedure FlogOffTimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Btn_work_descriptionClick(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
  private
    { Private-Deklarationen}
    Fmymodresult: TModalResult;
  public
    { Public-Deklarationen}
    property mymodresult: TModalResult read Fmymodresult;
  end;

var
  Flogoff: TFlogoff;

implementation

//uses uibdata, debug, login, ontop, work_description;
uses uibdata, ontop;

{$R *.lfm}

procedure TFlogoff.checkDB (begintime : TDateTime);
var
 //laststartt, laststopt, startt, stopt : TDateTime;
  laststartt, laststopt, startt : TDateTime;
 sumtime, firststartt, aktdate : TDatetime;
 uname, event : String;
 year, month, day: word;
 filterstr, filterdatestr : string;
 rcount : integer;
 datefound : boolean;
 foundstarttime  : TDatetime;
 weekday : byte;
 mynewline : string;
begin
 try
 FOnTop.ineditmode:=true;
  // start looking for gaps
 richmemo1.lines.add('Suche ab: '+ DateTimeToStr(begintime));
 datamodule1.debugOut(5,'logoff.checkDB','Suche ab: '+ DateTimeToStr(begintime));
 sumtime := 0;
 if Datamodule1.SQQueryuibevent.Active then Datamodule1.SQQueryuibevent.Close;
 Datamodule1.SQQueryuibevent.sql.Clear;
 Datamodule1.SQQueryuibevent.sql.Add('select * from uibevent ');
 Datamodule1.SQQueryuibevent.sql.Add('where userid = :uid ');
 Datamodule1.SQQueryuibevent.sql.Add('and starttime >= :start');
 Datamodule1.SQQueryuibevent.sql.Add('order by starttime ');
 Datamodule1.SQQueryuibevent.ParamByName('uid').AsString := uid;
 Datamodule1.SQQueryuibevent.ParamByName('start').AsDateTime := begintime;
 Datamodule1.SQQueryuibevent.open;
 Datamodule1.SQQueryuibevent.first;
 laststartt := Datamodule1.SQQueryuibevent.fieldbyname('starttime').asdatetime;
 richmemo1.lines.add('Start ab: '+ DateTimeToStr(laststartt));
 datamodule1.debugOut(5,'logoff.checkDB','Start ab: '+ DateTimeToStr(laststartt));
 firststartt := laststartt;
 laststopt := Datamodule1.SQQueryuibevent.fieldbyname('stoptime').asdatetime;
 if not Datamodule1.SQuibevent.Locate('userid;starttime;stoptime',
          VarArrayOf([uid, laststartt,laststopt]), [loCaseInsensitive,loPartialKey]) then
 begin
    richmemo1.lines.add('Record not found ');
     datamodule1.debugOut(5,'logoff.checkDB','Record not found ');
 end;
 Datamodule1.SQQueryuibevent.close;
 sumtime := laststopt - laststartt;
 Datamodule1.SQuibevent.next;
 while not Datamodule1.SQuibevent.eof do
 begin
  uname := Datamodule1.SQuibevent.fieldbyname('userid').asString;
  startt := Datamodule1.SQuibevent.fieldbyname('starttime').asdatetime;
  if startt <> laststopt then
  begin
   if startt < laststopt then
   begin
    // Überlappung   > 1 min
    if ((laststopt - startt) >  (1/(24*60))) then
    begin
     mynewline := 'Achtung Überlappung zw. : '
       +DateTimeToStr(laststartt)+' und '+DateTimeToStr(startt)+' bei '+uname;
     richmemo1.lines.add(mynewline);
     richmemo1.SetRangeColor(Length(richmemo1.Lines.Text) - Length(mynewline)-2,Length(mynewline)+1,  clRed);
     datamodule1.debugOut(5,'logoff.checkDB','Achtung Überlappung zw. : '
       +DateTimeToStr(laststartt)+' und '+DateTimeToStr(startt)+' bei '+uname);
    end;
   end
   else // >
   begin
    //Pause > 1 min  und am selben Tag
    if ((startt - laststopt) >  (1/(24*60)))
        and (trunc(startt) = trunc(laststopt)) then
    begin
     mynewline := 'Loch zw. : '
      +DateTimeToStr(laststopt)+' und '+DateTimeToStr(startt)
      +' Dauer: '+TimeToStr(startt - laststopt)+' bei '+uname;
     richmemo1.lines.add(mynewline);
     richmemo1.SetRangeColor(Length(richmemo1.Lines.Text) - Length(mynewline)-2,Length(mynewline)+1,  clRed);
     datamodule1.debugOut(5,'logoff.checkDB','Loch zw. : '
      +DateTimeToStr(laststopt)+' und '+DateTimeToStr(startt)
      +' Dauer: '+TimeToStr(startt - laststopt)+' bei '+uname);
    end;
   end;
  end;
  laststartt := Datamodule1.SQuibevent.fieldbyname('starttime').asdatetime;
  laststopt := Datamodule1.SQuibevent.fieldbyname('stoptime').asdatetime;
  sumtime := sumtime + (laststopt - laststartt);
  Datamodule1.SQuibevent.next;
 end;   // eof
 richmemo1.lines.add('Zeit von: '+DateTimeToStr(firststartt)+' bis '+DateTimeToStr(laststopt));
 richmemo1.lines.add('Stunden in diesem Zeitraum (Anwesenheit ohne Löcher): '+TimeToStr(sumtime)+
                 ' ('+floattostrF(sumtime*24,ffFixed,5,2)+')');
 query1.sql.Clear;
 query1.sql.Add('select sum(stunden) as summe_h from uibevent where');
 query1.sql.Add('(userid = :uid) and');
 query1.sql.Add('(starttime >= :start) and');
 query1.sql.Add('(stoptime <= :stop) and');
 query1.sql.Add('not (event = :pause)');
 query1.parambyname('uid').asstring := uid;
 query1.parambyname('start').asDateTime := firststartt;
 query1.parambyname('stop').asDateTime := laststopt;
 query1.parambyname('pause').asString := 'Pause';
 query1.open;
 richmemo1.lines.add('davon Arbeitszeit (ohne Pausen): '
                  +TimeToStr(query1.fieldbyname('summe_h').asfloat/24)+
                  ' ('+floattostrF(query1.fieldbyname('summe_h').asfloat,ffFixed,5,2)+')');
 query1.close;
 // end looking for gaps
 // start looking for missing reports
 richmemo1.lines.add('-------------------------------');
 richmemo1.lines.add('Der Report-Report:.....');
 sumtime := 0;
 if Datamodule1.SQQueryuibevent.Active then Datamodule1.SQQueryuibevent.Close;
 Datamodule1.SQQueryuibevent.sql.Clear;
 Datamodule1.SQQueryuibevent.sql.Add('select * from uibevent ');
 Datamodule1.SQQueryuibevent.sql.Add('where userid = :uid ');
 Datamodule1.SQQueryuibevent.sql.Add('and starttime >= :start');
 Datamodule1.SQQueryuibevent.sql.Add('order by starttime ');
 Datamodule1.SQQueryuibevent.ParamByName('uid').AsString := uid;
 Datamodule1.SQQueryuibevent.ParamByName('start').AsDateTime := begintime;
 Datamodule1.SQQueryuibevent.open;
 Datamodule1.SQQueryuibevent.first;
 laststartt := Datamodule1.SQQueryuibevent.fieldbyname('starttime').asdatetime;
 firststartt := laststartt;
 laststopt := Datamodule1.SQQueryuibevent.fieldbyname('stoptime').asdatetime;
 Datamodule1.SQuibevent.Locate('userid;starttime;stoptime', VarArrayOf([uid, laststartt,laststopt]), [loCaseInsensitive,loPartialKey]);
 Datamodule1.SQQueryuibevent.close;
 sumtime := laststopt - laststartt;
 //Datamodule1.SQuibevent.next;
 // query for event that need a report
 query1.sql.Clear;
 query1.sql.Add('select event from uibaktevent where');
 query1.sql.Add(' (reportrequired = 1)');
 //query1.sql.Add(' (not (time_h = null))');
 query1.open;
 //decodeDate(begintime, year, month, day);
 query2.sql.Clear;
 query2.sql.Add('select * from UIB_WORK_DESCRIPTION where ');
 query2.sql.Add('(jahr >= :year) and ');
 query2.sql.Add('(monat >= :month) and ');
 query2.sql.Add('(tag >= :day) and ');
 query2.sql.Add('(userid = :uid) and ');
 query2.sql.Add('(not (description = ""))  ');
 (*
 query2.ParamByName('year').AsInteger := year;
 query2.ParamByName('month').AsInteger := month;
 query2.ParamByName('day').AsInteger := day;
 query2.ParamByName('uid').AsString := uid;
 query2.open;
 *)

 while not Datamodule1.SQuibevent.eof do
 begin
  uname := Datamodule1.SQuibevent.fieldbyname('userid').asString;
  startt := Datamodule1.SQuibevent.fieldbyname('starttime').asdatetime;
  event := Datamodule1.SQuibevent.fieldbyname('event').asString;
  decodeDate(startt, year, month, day);
  if query2.Active then query2.Close;
   query2.ParamByName('year').AsInteger := year;
   query2.ParamByName('month').AsInteger := month;
   query2.ParamByName('day').AsInteger := day;
   query2.ParamByName('uid').AsString := uid;
   query2.open;
  if  query1.Locate('event',event,[loCaseInsensitive]) then
  begin
   // needs report
    if query2.Locate('userid;jahr;monat;tag;event',
                         VarArrayOf([uid, year, month, day, event]),[loCaseInsensitive]) then
    begin
      // report found
      with RichMemo1 do
      begin
        mynewline := '..............Report gefunden : ' + uname + ' -> '+DateTimeToStr(startt)+' : '+event;
        lines.add(mynewline);
        SetRangeColor(Length(Lines.Text) - Length(mynewline)-2,Length(mynewline)+1,  clGreen);
        //SetRangeColor(Length(Lines.Text) - Length(Lines[Lines.Count - 1])
        //                 - Lines.Count - 1, Length(Lines[Lines.Count - 1]), clGreen);
      end;
    end
    else
    begin
      // no report found
      with RichMemo1 do
      begin
        mynewline := '#*#*#*#*#  -> Report fehlt : ' + uname + ' -> '+DateTimeToStr(startt)+' : '+event;
        lines.add(mynewline);
        SetRangeColor(Length(Lines.Text) - Length(mynewline)-2,Length(mynewline)+1,  clRed);
        (*
        lines.add();
        SetRangeColor(Length(Lines.Text) - Length(Lines[Lines.Count - 1])
                         - Lines.Count - 1, Length(Lines[Lines.Count - 1]), clRed);
        *)
      end;
    end;
  end;
  Datamodule1.SQuibevent.next;
 end;
 query1.close;
 query2.close;

 // end looking for missing reports
 // start looking for missing workdays or holydays
 richmemo1.lines.add('-------------------------------');
 richmemo1.lines.add('Der Missing Days-Report:.....');
 if Datamodule1.SQQueryuibevent.Active then Datamodule1.SQQueryuibevent.Close;
 Datamodule1.SQQueryuibevent.sql.Clear;
 Datamodule1.SQQueryuibevent.sql.Add('select * from uibevent ');
 Datamodule1.SQQueryuibevent.sql.Add('where userid = :uid ');
 Datamodule1.SQQueryuibevent.sql.Add('and starttime >= :start');
 Datamodule1.SQQueryuibevent.sql.Add('order by starttime ');
 Datamodule1.SQQueryuibevent.ParamByName('uid').AsString := uid;
 Datamodule1.SQQueryuibevent.ParamByName('start').AsDateTime := begintime;
 Datamodule1.SQQueryuibevent.open;
 Datamodule1.SQQueryuibevent.first;
 aktdate := begintime;
 repeat
   // is it a work day
   weekday := DayOfTheWeek(aktdate);
   if weekday in user_work_days then
   begin
     // is a entry missing
     datefound := false;
     foundstarttime := Datamodule1.SQQueryuibevent.FieldByName('starttime').AsDateTime;
     while (foundstarttime < aktdate) and (not Datamodule1.SQQueryuibevent.EOF) do
     begin
       Datamodule1.SQQueryuibevent.Next;
       foundstarttime := Datamodule1.SQQueryuibevent.FieldByName('starttime').AsDateTime;
     end;
     if (foundstarttime < aktdate+1) then datefound := true;
     if not datefound then
     begin
       with RichMemo1 do
       begin
         if Datamodule1.dateIsHolyday(aktdate) then
          mynewline := 'Kein Eintrag für Feiertag :   ' +lazDayofWeekbyteToDayOfWeekGerStr(weekday) + ' - '+DateTimeToStr(aktdate)
         else
           mynewline := 'Kein Eintrag für Arbeitstag : ' +lazDayofWeekbyteToDayOfWeekGerStr(weekday) + ' - '+DateTimeToStr(aktdate);
         lines.add(mynewline);
         SetRangeColor(Length(Lines.Text) - Length(mynewline),Length(mynewline)+1,  clRed);

         //SetRangeColor(Length(Lines.Text) - Length(Lines[Lines.Count - 1])
         //                - Lines.Count - 1, Length(Lines[Lines.Count - 1]), clRed);
       end;
     end;
   end;
   aktdate := IncDay(aktdate);
 until aktdate > now;
 richmemo1.lines.add('Finished Missing Days-Report.');
 // end looking for missing workdays or holydays
 FOnTop.ineditmode:=false;

 except
  Formstyle := fsNormal;
  Datamodule1.TimerLogoffOnTop.Enabled := false;
  WindowState := wsNormal;
  application.processmessages;
 end;
end;

procedure TFlogoff.BitBtn2Click(Sender: TObject);
begin
  {$IFDEF LINUX}
  Fmymodresult:=mrAbort;
  Visible:=false;
  {$ENDIF LINUX}

  //DataModule1.TerminateApplication;mrAbort
  //Application.terminate;
end;

procedure TFlogoff.BitBtn1Click(Sender: TObject);
begin
  {$IFDEF LINUX}
  Fmymodresult:=mrOk;
  Visible:=false;
  {$ENDIF LINUX}
end;

procedure TFlogoff.BitBtn3Click(Sender: TObject);
begin
  {$IFDEF LINUX}
  Fmymodresult:= mrCancel;
  Visible:=false;
  {$ENDIF LINUX}
end;

procedure TFlogoff.ButtonLastMonthClick(Sender: TObject);
begin
  richmemo1.lines.clear;
  EditButton1.text := DateToStr(incmonth(date,-1));
end;

procedure TFlogoff.ButtonLastWeekClick(Sender: TObject);
begin
  richmemo1.lines.clear;
  EditButton1.text := DateToStr(incweek(date,-1));
end;



procedure TFlogoff.EditButton1ButtonClick(Sender: TObject);
begin
  CalendarDialog1.Date:=now;
  Formstyle := fsNormal;
  // start fix for modal form always on top
  //Datamodule1.TimerLogoffOnTop.Enabled := false;
  //WindowState := wsMinimized;
  // stop fix for modal form always on top
  application.processmessages;
  CalendarDialog1.Execute;
  application.processmessages;
  Formstyle := fsSystemStayOnTop;
  WindowState := wsMaximized;
  Datamodule1.TimerLogoffOnTop.Enabled := true;
  application.processmessages;
  EditButton1.text := DateToStr(CalendarDialog1.Date);
  //richmemo1.lines.clear;
  //checkDB(StrToDate(EditButton1.text));
end;

procedure TFlogoff.EditButton1Change(Sender: TObject);
begin
  richmemo1.lines.clear;
  checkDB(ScanDateTime('dd.mm.yyyy',EditButton1.text));
end;

procedure TFlogoff.FlogOffTimerTimer(Sender: TObject);
var
  {$IFDEF LINUX}
  hello : PNotifyNotification;
  {$ENDIF LINUX}
  exitcode : integer;
begin
  Datamodule1.debugOut(6, 'trayicon', 'start FlogOffTimer ');
  {$IFDEF LINUX}
  notify_init(argv[0]);
  hello := notify_notification_new(
    // Title
    pchar('uibtime logoff notify'),
    // Content
    pchar('You logged off from uibtime'),
    // Icon
    'dialog-information');
  // Lets display it, but we will not handle any errors ...
  notify_notification_show(hello, nil);
  // That's all folks :)
  notify_uninit;
  {$ENDIF LINUX}
  Datamodule1.debugOut(6, 'trayicon', 'stop trytimer ');
end;

procedure TFlogoff.FormCreate(Sender: TObject);
begin
 Flogoff.Caption:= 'uibtime - logged off -runtime';
  DataModule1.SetFontName(TControl(sender),myFont);
end;

procedure TFlogoff.FormHide(Sender: TObject);
begin
 Datamodule1.TimerLogoffOnTop.Enabled := false;
end;

procedure TFlogoff.FormPaint(Sender: TObject);
begin
  datamodule1.debugOut(5,'Paint FLogoff');
end;

procedure TFlogoff.FormShow(Sender: TObject);
begin
 datamodule1.debugOut(5,'Show FLogoff');
 richmemo1.lines.clear;
 Formstyle := fsSystemStayOnTop;
 WindowState := wsMaximized;
 application.processmessages;
 Datamodule1.TimerLogoffOnTop.Enabled := true;
 if EditButton1.text = DateToStr(date) then
 begin
   if FOnTop.visible then checkDB(date);
 end
 else EditButton1.text := DateToStr(date);
 //if FOnTop.visible then checkDB(date);
end;

procedure TFlogoff.Btn_work_descriptionClick(Sender: TObject);
begin
  {$IFDEF LINUX}
  Fmymodresult:=mrYes;
  Visible:=false;
  {$ENDIF LINUX}
 (*
 Flogoff.Hide;
 if Fwork_description = nil then
    Fwork_description := TFwork_description.Create(self);
  //Fwork_description.showmodal();
  Fwork_description.show;
  //Fwork_description.Free;
 //Flogoff.Show;
 *)
end;

procedure TFlogoff.FormWindowStateChange(Sender: TObject);
begin
  datamodule1.debugOut(5,'StateChange FLogoff');
end;

end.
