unit statistik;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, {LMessages, Messages,} SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, Db, sqldb, {Grids,} DBGrids, StdCtrls, {MaskEdit,} ExtCtrls,
  Buttons, result, sqltext, Spin, LR_Class, LR_DBSet, DateUtils;

type

  { TFStatistik }

  TFStatistik = class(TForm)
    frDBDataSet1: TfrDBDataSet;
    frReport1: TfrReport;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ComboBox1: TComboBox;
    Panel2: TPanel;
    BtnStunden: TButton;
    BtnSollStunden: TButton;
    BtnFehl: TButton;
    BtnAufteilung: TButton;
    query1: TSQLQuery;
    Edit1: TEdit;
    Edit2: TEdit;
    BitBtn1: TBitBtn;
    BtnTage: TButton;
    ///BatchMove1: TBatchMove;
    Table1: TSQLQuery;
    BtnMonate: TButton;
    BtnSQLtext: TButton;
    BtnLastMonth: TButton;
    BtnMonthRep: TBitBtn;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    QuerySollStunden: TSQLQuery;
    Label4: TLabel;
    Label5: TLabel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    BtnTreeSum: TBitBtn;
    ComboBoxAktevent: TComboBox;
    BtnBSZreport: TBitBtn;
    BtnThisMonth: TButton;
    BtnLastWeek: TButton;
    BtnThisWeek: TButton;
    BitBtnKnotenByUser: TBitBtn;
    BitBtnControlRep2: TBitBtn;
    BtnWorkDescription: TButton;
    BitBtnworkreport: TBitBtn;
    RadioGroupReference: TRadioGroup;
    Bevel3: TBevel;
    BitBtnArbeitsberichtOutput: TBitBtn;
    BitBtnMonthreportStartStop: TBitBtn;
    procedure BtnStundenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Edit1Exit(Sender: TObject);
    procedure Edit2Exit(Sender: TObject);
    procedure BtnSollStundenClick(Sender: TObject);
    procedure BtnFehlClick(Sender: TObject);
    procedure BtnAufteilungClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure BtnTageClick(Sender: TObject);
    procedure BtnMonateClick(Sender: TObject);
    procedure BtnSQLtextClick(Sender: TObject);
    procedure BtnLastMonthClick(Sender: TObject);
    procedure BtnMonthRepClick(Sender: TObject);
    procedure BtnTreeSumClick(Sender: TObject);
    procedure BtnBSZreportClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BtnThisMonthClick(Sender: TObject);
    procedure BtnThisWeekClick(Sender: TObject);
    procedure BtnLastWeekClick(Sender: TObject);
    procedure BitBtnKnotenByUserClick(Sender: TObject);
    procedure BitBtnControlRep2Click(Sender: TObject);
    procedure BtnWorkDescriptionClick(Sender: TObject);
    procedure BitBtnworkreportClick(Sender: TObject);
    procedure BitBtnArbeitsberichtOutputClick(Sender: TObject);
    procedure BitBtnMonthreportStartStopClick(Sender: TObject);
    procedure Panel2Click(Sender: TObject);
  private
    { Private-Deklarationen}
  public
    { Public-Deklarationen}
  end;

var
  FStatistik: TFStatistik;

implementation

uses uibdata, work_description;
//uses uibdata, debug,rmonat, work_description;

{$R *.lfm}

procedure TFStatistik.BtnStundenClick(Sender: TObject);
begin
 Fresult := TFResult.create(self);
 if DataModule1.Query4Result.active then DataModule1.Query4Result.close;
 ///DataModule1.Query4Result.databasename :='uibtime';
 DataModule1.Query4Result.SQL.clear;
 DataModule1.Query4Result.sql.Add('select');
 if (combobox1.Text = 'Summe Alle') then
 else
  DataModule1.Query4Result.sql.Add(' userid,');
 DataModule1.Query4Result.sql.Add('sum(stunden) as summe_h');
 DataModule1.Query4Result.sql.Add('from uibevent where');
 if (pos('Alle',combobox1.Text) =0) then
  DataModule1.Query4Result.sql.Add('(userid = :uid) and');
 DataModule1.Query4Result.sql.Add('(starttime >= :start) and');
 DataModule1.Query4Result.sql.Add('(stoptime <= :stop)');
 DataModule1.Query4Result.sql.Add('and (not (event = ''Pause''))');
 if not (combobox1.Text = 'Summe Alle') then
  DataModule1.Query4Result.sql.Add('group by userid');
 if (pos('Alle',combobox1.Text) =0) then
  DataModule1.Query4Result.parambyname('uid').asstring := uid;
 DataModule1.Query4Result.parambyname('start').asstring := edit1.text;
 DataModule1.Query4Result.parambyname('stop').asstring := edit2.text;
 DataModule1.Query4Result.open;
 //Fresult.DataSource1.DataSet := DataModule1.Query4Result;
 FResult.Edit1.text := 'Summe h von '+Edit1.text+' bis (excl.)'
                           +Edit2.text+' für '+ComboBox1.text;
 Fresult.showmodal();
 Fresult.free;
end;

procedure TFStatistik.FormCreate(Sender: TObject);
var
 //startmonth, year, startyear, month, day : word;
  year, month, day : word;
begin
 decodeDate(now,year,month,day);
 SpinEdit1.Value := month;
 SpinEdit2.Value := year;
 combobox1.items.clear;
 combobox1.items.add(uid);
 combobox1.items.add('Summe Alle');
 combobox1.text := uid;
 edit1.text := '1.1.2003';
 edit2.text := datetostr(date);
end;

procedure TFStatistik.Edit1Exit(Sender: TObject);
begin
 edit1.text := datetostr(strtodate(edit1.text));
end;

procedure TFStatistik.Edit2Exit(Sender: TObject);
begin
 edit2.text := datetostr(strtodate(edit2.text));
end;

procedure TFStatistik.BtnSollStundenClick(Sender: TObject);
var
 year, month, day: Word;
begin
 Fresult := TFResult.create(self);
 if DataModule1.Query4Result.active then DataModule1.Query4Result.close;
 ///DataModule1.Query4Result.databasename :='uibtime';
 DataModule1.Query4Result.SQL.clear;
 DataModule1.Query4Result.sql.Add('select');
 if (combobox1.Text = 'Summe Alle') then
 else
  DataModule1.Query4Result.sql.Add(' userid,');
 DataModule1.Query4Result.sql.Add('sum(stunden) as summe_h');
 DataModule1.Query4Result.sql.Add('from uibsoll where');
 if (pos('Alle',combobox1.Text) =0) then
  DataModule1.Query4Result.sql.Add('(userid = :uid) and');
 DataModule1.Query4Result.sql.Add('(jahr >= :starty) and');
 DataModule1.Query4Result.sql.Add('(monat >= :startm) and');
 DataModule1.Query4Result.sql.Add('(jahr <= :stopy) and');
 DataModule1.Query4Result.sql.Add('(monat <= :stopm)');
 if not (combobox1.Text = 'Summe Alle') then
  DataModule1.Query4Result.sql.Add('group by userid');
 if (pos('Alle',combobox1.Text) =0) then
  DataModule1.Query4Result.parambyname('uid').asstring := combobox1.text;
 decodedate(strtodate(edit1.text),year,month,day);
 DataModule1.Query4Result.parambyname('starty').asinteger := year;
 DataModule1.Query4Result.parambyname('startm').asinteger := month;
 decodedate(strtodate(edit2.text),year,month,day);
 DataModule1.Query4Result.parambyname('stopy').asinteger := year;
 DataModule1.Query4Result.parambyname('stopm').asinteger := month;
 DataModule1.Query4Result.ReadOnly:=true;
 DataModule1.Query4Result.open;
 //Fresult.DataSource1.DataSet := DataModule1.Query4Result;
 FResult.Edit1.text := 'Sum Soll-h von '+Edit1.text+' bis (excl.)'
                           +Edit2.text+' für '+ComboBox1.text;
 Fresult.showmodal();
 Fresult.free;
end;

procedure TFStatistik.BtnFehlClick(Sender: TObject);
var
 year, month, day: Word;
begin
 Fresult := TFResult.create(self);
 if DataModule1.Query4Result.active then DataModule1.Query4Result.close;
 ///DataModule1.Query4Result.databasename :='uibtime';
 DataModule1.Query4Result.SQL.clear;
 DataModule1.Query4Result.sql.Add('select');
(* if not (pos('Alle',combobox1.Text) <>0) then*)
  DataModule1.Query4Result.sql.Add(' userid,');
 DataModule1.Query4Result.sql.Add('(sum(stunden) - (select sum(stunden) from uibevent ');
(* if not (pos('Alle',combobox1.Text) <>0) then*)
  DataModule1.Query4Result.sql.Add('where (userid = :uid) and');
 DataModule1.Query4Result.sql.Add('(starttime >= :start) and');
 DataModule1.Query4Result.sql.Add('(stoptime <= :stop)');
 DataModule1.Query4Result.sql.Add(')) as summe_h');
 DataModule1.Query4Result.sql.Add('from uibsoll where');
 (*if not (pos('Alle',combobox1.Text) <>0) then*)
  DataModule1.Query4Result.sql.Add('(userid = :uid) and');
(* DataModule1.Query4Result.sql.Add('(e.userid = s.userid) and');*)
 DataModule1.Query4Result.sql.Add('(jahr >= :starty) and');
 DataModule1.Query4Result.sql.Add('(monat >= :startm) and');
 DataModule1.Query4Result.sql.Add('(jahr <= :stopy) and');
 DataModule1.Query4Result.sql.Add('(monat <= :stopm)');
 DataModule1.Query4Result.sql.Add('and (not (event = ''Pause''))');
(* if not (combobox1.Text = 'Summe Alle') then
  DataModule1.Query4Result.sql.Add('group by userid');*)
(* if not (pos('Alle',combobox1.Text) <>0) then*)
  DataModule1.Query4Result.parambyname('uid').asstring := uid;
 decodedate(strtodate(edit1.text),year,month,day);
 DataModule1.Query4Result.parambyname('starty').asinteger := year;
 DataModule1.Query4Result.parambyname('startm').asinteger := month;
 decodedate(strtodate(edit2.text),year,month,day);
 DataModule1.Query4Result.parambyname('stopy').asinteger := year;
 DataModule1.Query4Result.parambyname('stopm').asinteger := month;
 DataModule1.Query4Result.parambyname('start').asstring := edit1.text;
 DataModule1.Query4Result.parambyname('stop').asstring := edit2.text;
 DataModule1.Query4Result.ReadOnly:=true;
 DataModule1.Query4Result.open;
 //Fresult.DataSource1.DataSet := DataModule1.Query4Result;
 FResult.Edit1.text := 'Fehlstunden h von '+Edit1.text+' bis (excl.)'
                           +Edit2.text+' für '+ComboBox1.text;
 Fresult.showmodal();
 Fresult.free;
end;

procedure TFStatistik.BtnAufteilungClick(Sender: TObject);
var
 fc : integer;
begin
 Fresult := TFResult.create(self);
 if DataModule1.Query4Result.active then DataModule1.Query4Result.close;
 ///DataModule1.Query4Result.databasename :='uibtime';
 DataModule1.Query4Result.SQL.clear;
 DataModule1.Query4Result.sql.Add('select');
 DataModule1.Query4Result.sql.Add(' event, ');
 if (combobox1.Text = 'Summe Alle') then
 else
  DataModule1.Query4Result.sql.Add(' userid,');
 DataModule1.Query4Result.sql.Add(' sum(stunden) as summe_h');
 DataModule1.Query4Result.sql.Add(' from uibevent where');
 if (pos('Alle',combobox1.Text) =0) then
  DataModule1.Query4Result.sql.Add('(userid = :uid) and');
 DataModule1.Query4Result.sql.Add('(starttime >= :start) and');
 DataModule1.Query4Result.sql.Add('(stoptime <= :stop)');
 DataModule1.Query4Result.sql.Add('and (not (event = ''Pause''))');
 DataModule1.Query4Result.sql.Add('group by event ');
 if not (combobox1.Text = 'Summe Alle') then
  DataModule1.Query4Result.sql.Add(',userid');
 if (pos('Alle',combobox1.Text) =0) then
  DataModule1.Query4Result.parambyname('uid').asstring := combobox1.text;
 DataModule1.Query4Result.parambyname('start').asstring := edit1.text;
 DataModule1.Query4Result.parambyname('stop').asstring := edit2.text;
 DataModule1.Query4Result.ReadOnly:=true;
 DataModule1.Query4Result.open;
 for fc := 0 to DataModule1.Query4Result.Fieldcount-1 do
  DataModule1.Query4Result.Fields[fc].visible := true;
 //Fresult.DataSource1.DataSet := DataModule1.Query4Result;
 FResult.Edit1.text := 'Art h von '+Edit1.text+' bis (excl.)'
                           +Edit2.text+' für '+ComboBox1.text;
 Fresult.showmodal();
 Fresult.free;
end;

procedure TFStatistik.FormActivate(Sender: TObject);
//var
// i : integer;
begin
 datamodule1.debugOut(5,'Activate FStatistik');
 if DataModule1.Query4Result.active then DataModule1.Query4Result.close;
 ///DataModule1.Query4Result.databasename :='uibtime';
 DataModule1.Query4Result.SQL.clear;
 DataModule1.Query4Result.sql.Add('select event from uibaktevent');
 DataModule1.Query4Result.ReadOnly:=true;
 DataModule1.Query4Result.open;
 ComboBoxAktevent.Items.Clear;
 while not DataModule1.Query4Result.Eof do
 begin
  ComboBoxAktevent.Items.Add(DataModule1.Query4Result.fieldbyname('event').AsString);
  DataModule1.Query4Result.Next;
 end;
end;

procedure TFStatistik.BtnTageClick(Sender: TObject);
//var
// fc : integer;
begin
 Fresult := TFResult.create(self);
 if DataModule1.Query4Result.active then DataModule1.Query4Result.close;
 ///DataModule1.Query4Result.databasename :='uibtime';
 DataModule1.Query4Result.SQL.clear;
 DataModule1.Query4Result.sql.Add('select');
 DataModule1.Query4Result.sql.Add(' Tag,Monat,Jahr, sum(stunden) as Summe ');
 DataModule1.Query4Result.sql.Add('from uibeventtimesplit where');
 if (pos('Alle',combobox1.Text) =0) then
  DataModule1.Query4Result.sql.Add('(userid = :uid) and');
 DataModule1.Query4Result.sql.Add('(starttime >= :start) and');
 DataModule1.Query4Result.sql.Add('(stoptime <= :stop)');
 DataModule1.Query4Result.sql.Add('and (not (event = ''Pause''))');
 if (pos('Alle',combobox1.Text) =0) then
  DataModule1.Query4Result.parambyname('uid').asstring := combobox1.text;
 DataModule1.Query4Result.parambyname('start').asstring := edit1.text;
 DataModule1.Query4Result.parambyname('stop').asstring := edit2.text;
 DataModule1.Query4Result.sql.Add(' group by Tag, monat, jahr');
 DataModule1.Query4Result.ReadOnly:=true;
 DataModule1.Query4Result.open;

 //Fresult.DataSource1.DataSet := DataModule1.Query4Result;
 FResult.Edit1.text := 'Sum. h / Monatstag von '+Edit1.text+' bis (excl.)'
                           +Edit2.text+' für '+ComboBox1.text;
 Fresult.showmodal();
 Fresult.free;
end;

procedure TFStatistik.BtnMonateClick(Sender: TObject);
begin
 Fresult := TFResult.create(self);
 if DataModule1.Query4Result.active then DataModule1.Query4Result.close;
 ///DataModule1.Query4Result.databasename :='uibtime';
 DataModule1.Query4Result.SQL.clear;
 DataModule1.Query4Result.sql.Add('select');
 DataModule1.Query4Result.sql.Add(' Monat,Jahr, sum(stunden) as Summe ');
 DataModule1.Query4Result.sql.Add('from uibeventtimesplit where');
 if (pos('Alle',combobox1.Text) =0) then
  DataModule1.Query4Result.sql.Add('(userid = :uid) and');
 DataModule1.Query4Result.sql.Add('(starttime >= :start) and');
 DataModule1.Query4Result.sql.Add('(stoptime <= :stop)');
 DataModule1.Query4Result.sql.Add('and (not (event = ''Pause''))');
 if (pos('Alle',combobox1.Text) =0) then
  DataModule1.Query4Result.parambyname('uid').asstring := combobox1.text;
 DataModule1.Query4Result.parambyname('start').asstring := edit1.text;
 DataModule1.Query4Result.parambyname('stop').asstring := edit2.text;
 DataModule1.Query4Result.sql.Add(' group by Monat, jahr');
 DataModule1.Query4Result.ReadOnly:=true;
 DataModule1.Query4Result.open;
 //Fresult.DataSource1.DataSet := DataModule1.Query4Result;
 FResult.Edit1.text := 'Sum h/Mon. von '+Edit1.text+' bis (excl.)'
                           +Edit2.text+' für '+ComboBox1.text;
 Fresult.showmodal();
 Fresult.free;

end;

procedure TFStatistik.BtnSQLtextClick(Sender: TObject);
begin
 FSQL := TFSQL.create(self);
 if DataModule1.Query4Result.active then DataModule1.Query4Result.close;
 ///DataModule1.Query4Result.databasename :='uibtime';
 DataModule1.Query4Result.SQL.clear;
 FSQL.showmodal();
end;

procedure TFStatistik.BtnLastMonthClick(Sender: TObject);
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

procedure TFStatistik.BtnMonthRepClick(Sender: TObject);
var
 nextmonth, nextmonthyear : integer;
 mypath : string;
begin
 ///FRmonat := TFRmonat.create(self);

 if query1.active then query1.close;
 ///query1.databasename :='uibtime';
 query1.SQL.clear;
 query1.sql.Add('select');
 query1.sql.Add(' a.Tag, b.daystr,  min(a.starttime), max(a.stoptime), sum(a.stunden) as Summe ');
 query1.sql.Add('from uibeventtimesplit as a, uibtwoche as b where');
 query1.sql.Add('(a.wochentag = b.daynum) and ');
 query1.sql.Add('(a.userid = :uid) and ');
 query1.sql.Add('(a.starttime >= :start) and ');
 query1.sql.Add('(a.stoptime <= :stop) ');
 query1.sql.Add('and (not (event = ''Pause''))');
 query1.parambyname('uid').asstring := combobox1.text;
 query1.parambyname('start').asstring
  := '1.'+spinedit1.Text+'.'+spinedit2.Text;
 nextmonth := strtoint(spinedit1.Text)+1;
 nextmonthyear := strtoint(spinedit2.Text);
 if nextmonth > 12 then
 begin
  nextmonth := 1;
  inc(nextmonthyear);
 end;
 query1.parambyname('stop').asstring
  := '1.'+inttostr(nextmonth)+'.'+inttostr(nextmonthyear);
 query1.sql.Add(' group by Tag, daystr');
 query1.ReadOnly:=true;
 query1.open;

 // sollstunden holen....
 ///Datamodule1.Tableuser.FindKey([uid]);
 if querysollstunden.active then querysollstunden.close;
 querysollstunden.SQL.Clear;
 querysollstunden.SQL.Add('select stunden from uibsoll where');
 querysollstunden.sql.Add('(userid = :uid) ');
 querysollstunden.sql.Add('and (jahr = '+spinedit2.Text+')');
 querysollstunden.sql.Add('and (monat = '+spinedit1.Text+')');
 querysollstunden.parambyname('uid').asstring := combobox1.text;
 ///  := Datamodule1.Tableuser.fieldbyname('userid').asstring;
 querysollstunden.open;
 (*
 FRmonat.QRLabel1.Caption := 'Monatsreport für '+
 Datamodule1.Tableuser.fieldbyname('name').asstring;
 FRmonat.QRLabel2.Caption := 'Von 1.'+spinedit1.Text+'.'+spinedit2.Text+
   ' bis (excl.) 1.'+inttostr(nextmonth)+'.'+inttostr(nextmonthyear);
 FRmonat.QuickRep1.Preview;
 FRmonat.free;
 *)
 {$IFDEF Linux}
 mypath := '/usr/share/uibtime/';
 if not FileExists(mypath+'monthrep.lrf') then
 // development:
 mypath := ExtractFilePath(paramstr(0));
 {$ELSE}
 mypath := ExtractFilePath(paramstr(0));
 {$ENDIF Linux}
 frReport1.LoadFromFile(mypath+'monthrep.lrf');

 frReport1.FindObject('memoTitle').Memo.Text:= 'Monatsreport für '+
   Datamodule1.SQuibaktuser.fieldbyname('name').asstring;
 frReport1.FindObject('memoStartEnd').Memo.Text := 'Von 1.'+spinedit1.Text+'.'+spinedit2.Text+
   ' bis (excl.) 1.'+inttostr(nextmonth)+'.'+inttostr(nextmonthyear);
 frReport1.ShowReport;
end;

procedure TFStatistik.BtnTreeSumClick(Sender: TObject);
var
 searchuserstr : String;
  procedurename : string;
begin
 if RadioGroupReference.ItemIndex = 0 then
  procedurename := 'uibbuild_tmp_treesum_aktevent'
 else
  procedurename := 'uibbuild_tmp_treesum';
 Fresult := TFResult.create(self);
 if DataModule1.Query4Result.active then DataModule1.Query4Result.close;
 ///DataModule1.Query4Result.databasename :='uibtime';
 DataModule1.Query4Result.SQL.clear;
 DataModule1.Query4Result.sql.Add('execute procedure '+procedurename+'(');
 DataModule1.Query4Result.sql.Add(':below,:userid, :searchuser,:start, :stop);');
 DataModule1.Query4Result.parambyname('userid').asstring := uid;
 DataModule1.Query4Result.parambyname('below').asstring := ComboBoxAktevent.Text;
 DataModule1.Query4Result.parambyname('start').asstring := edit1.text;
 DataModule1.Query4Result.parambyname('stop').asstring := edit2.text;
 if (combobox1.Text = 'Summe Alle') then
 begin
  DataModule1.Query4Result.parambyname('searchuser').asstring := '%';
  searchuserstr := 'für alle.';
 end
 else
 begin
  DataModule1.Query4Result.parambyname('searchuser').asstring := uid;
  searchuserstr := 'für '+uid;
 end;
 DataModule1.Query4Result.ExecSQL;
 if DataModule1.Query4Result.active then DataModule1.Query4Result.close;
 DataModule1.Query4Result.SQL.clear;
 DataModule1.Query4Result.sql.Add('select event, stunden from uibtmptreesum ');
 DataModule1.Query4Result.sql.Add('where userid = :userid');
 DataModule1.Query4Result.parambyname('userid').asstring := uid;
 DataModule1.Query4Result.ReadOnly:=true;
 DataModule1.Query4Result.open;
 //Fresult.DataSource1.DataSet := DataModule1.Query4Result;
 FResult.Edit1.text := 'Stundensummen von '
                        +Edit1.text+'bis (excl.)'
                        +Edit2.text+' unterhalb (+incl.) von '
                        +ComboBoxAktevent.text+' '
                        +searchuserstr;
 Fresult.showmodal();
 Fresult.free;
end;

procedure TFStatistik.BtnBSZreportClick(Sender: TObject);
var
 searchuserstr : String;
 procedurename : string;
begin
 if RadioGroupReference.ItemIndex = 0 then
  procedurename := 'buildbsz_tmp_monthrep_aktevent'
 else
  procedurename := 'buildbsz_tmp_monthrep';
 Fresult := TFResult.create(self);
 if DataModule1.Query4Result.active then DataModule1.Query4Result.close;
 ///DataModule1.Query4Result.databasename :='uibtime';
 DataModule1.Query4Result.SQL.clear;
 DataModule1.Query4Result.sql.Add('execute procedure '+procedurename+'(');
 DataModule1.Query4Result.sql.Add(':userid, :searchuser,:start, :stop);');
 DataModule1.Query4Result.parambyname('userid').asstring := uid;
 DataModule1.Query4Result.parambyname('start').asstring := edit1.text;
 DataModule1.Query4Result.parambyname('stop').asstring := edit2.text;
 if (combobox1.Text = 'Summe Alle') then
 begin
  DataModule1.Query4Result.parambyname('searchuser').asstring := '%';
  searchuserstr := ' für alle.';
 end
 else
 begin
  DataModule1.Query4Result.parambyname('searchuser').asstring := uid;
  searchuserstr := ' für '+uid;
 end;
 DataModule1.Query4Result.ExecSQL;
 if DataModule1.Query4Result.active then DataModule1.Query4Result.close;
 DataModule1.Query4Result.SQL.clear;
 DataModule1.Query4Result.sql.Add('select event, id_string, stunden from uibtmptreesum ');
 DataModule1.Query4Result.sql.Add('where userid = :userid');
 DataModule1.Query4Result.parambyname('userid').asstring := uid;
 DataModule1.Query4Result.ReadOnly:=true;
 DataModule1.Query4Result.open;
 //Fresult.DataSource1.DataSet := DataModule1.Query4Result;
 FResult.Edit1.text := 'BSZ-UR Controllingreport von '
                        +Edit1.text+'bis (excl.)'
                        +Edit2.text
                        +searchuserstr;
 Fresult.showmodal();
 Fresult.free;
end;

procedure TFStatistik.BitBtn1Click(Sender: TObject);
begin
 hide;
 close;
end;

procedure TFStatistik.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Datamodule1.Statistik1Cancel;
end;

procedure TFStatistik.BtnThisMonthClick(Sender: TObject);
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

procedure TFStatistik.BtnThisWeekClick(Sender: TObject);
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

procedure TFStatistik.BtnLastWeekClick(Sender: TObject);
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


procedure TFStatistik.BitBtnKnotenByUserClick(Sender: TObject);
var
 referenztabelle : string;
begin
 if RadioGroupReference.ItemIndex = 0 then
  referenztabelle := 'uibaktevent'
 else
  referenztabelle := 'uiballevent';
 Fresult := TFResult.create(self);
 if DataModule1.Query4Result.active then DataModule1.Query4Result.close;
 ///DataModule1.Query4Result.databasename :='uibtime';
 DataModule1.Query4Result.SQL.clear;

 DataModule1.Query4Result.sql.Add(' select userid , sum(stunden) as stunden from uibevent');
 DataModule1.Query4Result.sql.Add('    where (');
 DataModule1.Query4Result.sql.Add('    (event in (select event from '+referenztabelle+' where');
 DataModule1.Query4Result.sql.Add('    (parentevent in (select event from '+referenztabelle+' where');
 DataModule1.Query4Result.sql.Add('    (parentevent in (select event from '+referenztabelle+' where');
 DataModule1.Query4Result.sql.Add('    (parentevent in (select event from '+referenztabelle+' where');
 DataModule1.Query4Result.sql.Add('    (parentevent in (select event from '+referenztabelle+' where');
 DataModule1.Query4Result.sql.Add('    (parentevent in (select event from '+referenztabelle+' where');
 DataModule1.Query4Result.sql.Add('    (parentevent = :below_event)))))))))))))');
 DataModule1.Query4Result.sql.Add('    or');
 DataModule1.Query4Result.sql.Add('    (event in (select event from '+referenztabelle+' where');
 DataModule1.Query4Result.sql.Add('    (parentevent in (select event from '+referenztabelle+' where');
 DataModule1.Query4Result.sql.Add('    (parentevent in (select event from '+referenztabelle+' where');
 DataModule1.Query4Result.sql.Add('    (parentevent in (select event from '+referenztabelle+' where');
 DataModule1.Query4Result.sql.Add('    (parentevent in (select event from '+referenztabelle+' where');
 DataModule1.Query4Result.sql.Add('    (parentevent = :below_event)))))))))))');
 DataModule1.Query4Result.sql.Add('    or');
 DataModule1.Query4Result.sql.Add('    (event in (select event from '+referenztabelle+' where');
 DataModule1.Query4Result.sql.Add('    (parentevent in (select event from '+referenztabelle+' where');
 DataModule1.Query4Result.sql.Add('    (parentevent in (select event from '+referenztabelle+' where');
 DataModule1.Query4Result.sql.Add('    (parentevent in (select event from '+referenztabelle+' where');
 DataModule1.Query4Result.sql.Add('    (parentevent = :below_event)))))))))');
 DataModule1.Query4Result.sql.Add('    or');
 DataModule1.Query4Result.sql.Add('    (event in (select event from '+referenztabelle+' where');
 DataModule1.Query4Result.sql.Add('    (parentevent in (select event from '+referenztabelle+' where');
 DataModule1.Query4Result.sql.Add('    (parentevent in (select event from '+referenztabelle+' where');
 DataModule1.Query4Result.sql.Add('    (parentevent = :below_event)))))))');
 DataModule1.Query4Result.sql.Add('    or');
 DataModule1.Query4Result.sql.Add('    (event in (select event from '+referenztabelle+' where');
 DataModule1.Query4Result.sql.Add('    (parentevent in (select event from '+referenztabelle+' where');
 DataModule1.Query4Result.sql.Add('    (parentevent = :below_event)))))');
 DataModule1.Query4Result.sql.Add('    or');
 DataModule1.Query4Result.sql.Add('    (event in (select event from uiballevent');
 DataModule1.Query4Result.sql.Add('    where (parentevent like :below_event)))');
 DataModule1.Query4Result.sql.Add('    or');
 DataModule1.Query4Result.sql.Add('    (event = :below_event)');
 DataModule1.Query4Result.sql.Add('    )');
 DataModule1.Query4Result.sql.Add('    and (starttime >= :von)');
 DataModule1.Query4Result.sql.Add('    and (stoptime < :bis)');
 DataModule1.Query4Result.sql.Add('    group by userid;');
 DataModule1.Query4Result.parambyname('below_event').asstring := ComboBoxAktevent.text;
 DataModule1.Query4Result.parambyname('von').asstring := edit1.text;
 DataModule1.Query4Result.parambyname('bis').asstring := edit2.text;
 DataModule1.Query4Result.ReadOnly:=true;
 DataModule1.Query4Result.open;
 //Fresult.DataSource1.DataSet := DataModule1.Query4Result;
 FResult.Edit1.text := 'Stunden pro User ab Knoten: '
                        +ComboBoxAktevent.text
                        +' von '+Edit1.text+' bis (excl.)'+Edit2.text;
 Fresult.showmodal();
 Fresult.free;
end;



procedure TFStatistik.BitBtnControlRep2Click(Sender: TObject);
var
 searchuserstr : String;
 procedurename : string;
begin
 if RadioGroupReference.ItemIndex = 0 then
  procedurename := 'buildbsz2_tmp_monthrep_aktevent'
 else
  procedurename := 'buildbsz2_tmp_monthrep';
 Fresult := TFResult.create(self);
 if DataModule1.Query4Result.active then DataModule1.Query4Result.close;
 ///DataModule1.Query4Result.databasename :='uibtime';
 DataModule1.Query4Result.SQL.clear;
 DataModule1.Query4Result.sql.Add('execute procedure '+procedurename+'(');
 DataModule1.Query4Result.sql.Add(':userid, :searchuser,:start, :stop);');
 DataModule1.Query4Result.parambyname('userid').asstring := uid;
 DataModule1.Query4Result.parambyname('start').asstring := edit1.text;
 DataModule1.Query4Result.parambyname('stop').asstring := edit2.text;
 if (combobox1.Text = 'Summe Alle') then
 begin
  DataModule1.Query4Result.parambyname('searchuser').asstring := '%';
  searchuserstr := ' für alle.';
 end
 else
 begin
  DataModule1.Query4Result.parambyname('searchuser').asstring := uid;
  searchuserstr := ' für '+uid;
 end;
 DataModule1.Query4Result.ExecSQL;
 if DataModule1.Query4Result.active then DataModule1.Query4Result.close;
 DataModule1.Query4Result.SQL.clear;
 DataModule1.Query4Result.sql.Add('select event, id_string, uibuser, stunden from uibtmptreesum2 ');
 DataModule1.Query4Result.sql.Add('where userid = :userid');
 DataModule1.Query4Result.parambyname('userid').asstring := uid;
 DataModule1.Query4Result.ReadOnly:=true;
 DataModule1.Query4Result.open;
 //Fresult.DataSource1.DataSet := DataModule1.Query4Result;
 FResult.Edit1.text := 'BSZ-UR Controllingreport von '
                        +Edit1.text+'bis (excl.)'
                        +Edit2.text
                        +searchuserstr;
 Fresult.showmodal();
 Fresult.free;
end;

procedure TFStatistik.BtnWorkDescriptionClick(Sender: TObject);
begin
 if Fwork_description = nil then
    Fwork_description := TFwork_description.Create(self);
  //Fwork_description.showmodal();
  Fwork_description.show;
end;

procedure TFStatistik.BitBtnworkreportClick(Sender: TObject);
var
 searchuserstr : String;
 procedurename : string;
begin
 if RadioGroupReference.ItemIndex = 0 then
  procedurename := 'buildbsz_tmp_sapevent_aktevent'
 else
  procedurename := 'buildbsz_tmp_sapevent';
 Fresult := TFResult.create(self);
 if DataModule1.Query4Result.active then DataModule1.Query4Result.close;
 ///DataModule1.Query4Result.databasename :='uibtime';
 DataModule1.Query4Result.SQL.clear;
 DataModule1.Query4Result.sql.Add('execute procedure '+procedurename+';');
 DataModule1.Query4Result.ExecSQL;
 if DataModule1.Query4Result.active then DataModule1.Query4Result.close;
 DataModule1.Query4Result.SQL.clear;
 DataModule1.Query4Result.sql.Add('select');
 DataModule1.Query4Result.sql.Add(' UIBEVENTTIMESPLIT.TAG, ');
 DataModule1.Query4Result.sql.Add(' UIBEVENTTIMESPLIT.USERID, ');
 DataModule1.Query4Result.sql.Add(' UIBEVENTTIMESPLIT.EVENT, ');
 DataModule1.Query4Result.sql.Add(' uibtmp_sapevent.sapevent, ');
 DataModule1.Query4Result.sql.Add(' uibtmp_sapevent.id_string, ');
 DataModule1.Query4Result.sql.Add(' sum(UIBEVENTTIMESPLIT.STUNDEN), ');
 DataModule1.Query4Result.sql.Add(' UIB_WORK_DESCRIPTION.DESCRIPTION ');
 DataModule1.Query4Result.sql.Add(' from ');
 DataModule1.Query4Result.sql.Add(' UIBEVENTTIMESPLIT LEFT OUTER JOIN UIB_WORK_DESCRIPTION ON ');
 DataModule1.Query4Result.sql.Add(' (UIBEVENTTIMESPLIT.USERID = UIB_WORK_DESCRIPTION.USERID) and ');
 DataModule1.Query4Result.sql.Add(' (UIBEVENTTIMESPLIT.TAG = UIB_WORK_DESCRIPTION.TAG) and ');
 DataModule1.Query4Result.sql.Add(' (UIBEVENTTIMESPLIT.MONAT = UIB_WORK_DESCRIPTION.MONAT) and ');
 DataModule1.Query4Result.sql.Add(' (UIBEVENTTIMESPLIT.JAHR = UIB_WORK_DESCRIPTION.JAHR) and ');
 DataModule1.Query4Result.sql.Add(' (UIBEVENTTIMESPLIT.EVENT = UIB_WORK_DESCRIPTION.EVENT), ');
 DataModule1.Query4Result.sql.Add(' uibtmp_sapevent ');
 DataModule1.Query4Result.sql.Add(' where ( ');
 DataModule1.Query4Result.sql.Add(' (UIBEVENTTIMESPLIT.EVENT = uibtmp_sapevent.event) and ');
 DataModule1.Query4Result.sql.Add(' (UIBEVENTTIMESPLIT.EVENT in (select event from uibtmp_sapevent)) and ');
 DataModule1.Query4Result.sql.Add(' (UIBEVENTTIMESPLIT.userid like :searchuser) and');
 DataModule1.Query4Result.sql.Add(' (UIBEVENTTIMESPLIT.MONAT = :month) and ');
 DataModule1.Query4Result.sql.Add(' (UIBEVENTTIMESPLIT.JAHR = :year)) ');
 DataModule1.Query4Result.sql.Add(' GROUP BY ');
 DataModule1.Query4Result.sql.Add(' UIBEVENTTIMESPLIT.TAG, ');
 DataModule1.Query4Result.sql.Add(' UIBEVENTTIMESPLIT.EVENT, ');
 DataModule1.Query4Result.sql.Add(' uibtmp_sapevent.sapevent, ');
 DataModule1.Query4Result.sql.Add(' uibtmp_sapevent.id_string, ');
 DataModule1.Query4Result.sql.Add(' UIBEVENTTIMESPLIT.USERID, ');
 DataModule1.Query4Result.sql.Add(' UIB_WORK_DESCRIPTION.DESCRIPTION ');
 DataModule1.Query4Result.sql.Add(' ORDER BY ');
 DataModule1.Query4Result.sql.Add(' UIBEVENTTIMESPLIT.TAG, UIBEVENTTIMESPLIT.USERID, UIBEVENTTIMESPLIT.EVENT');

 if (combobox1.Text = 'Summe Alle') then
 begin
  DataModule1.Query4Result.parambyname('searchuser').asstring := '%';
  searchuserstr := ' für alle.';
 end
 else
 begin
  DataModule1.Query4Result.parambyname('searchuser').asstring := uid;
  searchuserstr := ' für '+uid;
 end;
 DataModule1.Query4Result.parambyname('month').asstring := spinedit1.Text;
 DataModule1.Query4Result.parambyname('year').asstring := spinedit2.Text;
 DataModule1.Query4Result.ReadOnly:=true;
 DataModule1.Query4Result.open;
 //Fresult.DataSource1.DataSet := DataModule1.Query4Result;
 FResult.Edit1.text := 'BSZ-UR Tätigkeitsbericht für '
                        +spinedit1.Text+'.'
                        +spinedit2.Text
                        +searchuserstr;
 Fresult.showmodal();
 Fresult.free;
end;


procedure TFStatistik.BitBtnArbeitsberichtOutputClick(Sender: TObject);
var
 searchuserstr, searchuser, below : String;
 referenztabelle : string;
 //year, month, day : word;
begin
 if RadioGroupReference.ItemIndex = 0 then
  referenztabelle := 'uibaktevent'
 else
  referenztabelle := 'uiballevent';
 if (combobox1.Text = 'Summe Alle') then
 begin
  searchuser := '%';
  searchuserstr := 'für alle.';
 end
 else
 begin
  searchuser := uid;
  searchuserstr := 'für '+uid;
 end;
 below := ComboBoxAktevent.Text;
 Fresult := TFResult.create(self);
 if DataModule1.Query4Result.active then DataModule1.Query4Result.close;
 DataModule1.Query4Result.SQL.clear;
 DataModule1.Query4Result.sql.Add(' select UIBEVENTTIMESPLIT.JAHR,UIBEVENTTIMESPLIT.Monat,UIBEVENTTIMESPLIT.TAG,');
 DataModule1.Query4Result.sql.Add('UIBEVENTTIMESPLIT.EVENT,UIBEVENTTIMESPLIT.USERID,sum(UIBEVENTTIMESPLIT.STUNDEN),UIB_WORK_DESCRIPTION.DESCRIPTION ');
 DataModule1.Query4Result.sql.Add('from ');
 DataModule1.Query4Result.sql.Add('UIBEVENTTIMESPLIT LEFT OUTER JOIN UIB_WORK_DESCRIPTION ');
 DataModule1.Query4Result.sql.Add('ON ');
 DataModule1.Query4Result.sql.Add('(UIBEVENTTIMESPLIT.USERID = UIB_WORK_DESCRIPTION.USERID) ');
 DataModule1.Query4Result.sql.Add('and ');
 DataModule1.Query4Result.sql.Add('(UIBEVENTTIMESPLIT.TAG = UIB_WORK_DESCRIPTION.TAG) ');
 DataModule1.Query4Result.sql.Add('and ');
 DataModule1.Query4Result.sql.Add('(UIBEVENTTIMESPLIT.MONAT = UIB_WORK_DESCRIPTION.MONAT) ');
 DataModule1.Query4Result.sql.Add('and ');
 DataModule1.Query4Result.sql.Add('(UIBEVENTTIMESPLIT.JAHR = UIB_WORK_DESCRIPTION.JAHR) ');
 DataModule1.Query4Result.sql.Add('and ');
 DataModule1.Query4Result.sql.Add('(UIBEVENTTIMESPLIT.EVENT = UIB_WORK_DESCRIPTION.EVENT) ');
 DataModule1.Query4Result.sql.Add('where ');
 DataModule1.Query4Result.sql.Add('(UIBEVENTTIMESPLIT.EVENT ');
 DataModule1.Query4Result.sql.Add('in (select event from '+referenztabelle+' where ');
 DataModule1.Query4Result.sql.Add('    (parentevent in (select event from '+referenztabelle+' where ');
 DataModule1.Query4Result.sql.Add('    (parentevent in (select event from '+referenztabelle+' where ');
 DataModule1.Query4Result.sql.Add('    (parentevent in (select event from '+referenztabelle+' where ');
 DataModule1.Query4Result.sql.Add('    (parentevent in (select event from '+referenztabelle+' where ');
 DataModule1.Query4Result.sql.Add('    (parentevent in (select event from '+referenztabelle+' where ');
 DataModule1.Query4Result.sql.Add('    (parentevent = '''+below+'''))))))))))) ');
 DataModule1.Query4Result.sql.Add('    or ');
 DataModule1.Query4Result.sql.Add('    (event in (select event from '+referenztabelle+' where ');
 DataModule1.Query4Result.sql.Add('    (parentevent in (select event from '+referenztabelle+' where ');
 DataModule1.Query4Result.sql.Add('    (parentevent in (select event from '+referenztabelle+' where ');
 DataModule1.Query4Result.sql.Add('    (parentevent in (select event from '+referenztabelle+' where ');
 DataModule1.Query4Result.sql.Add('    (parentevent in (select event from '+referenztabelle+' where ');
 DataModule1.Query4Result.sql.Add('    (parentevent = '''+below+'''))))))))))) ');
 DataModule1.Query4Result.sql.Add('    or ');
 DataModule1.Query4Result.sql.Add('    (event in (select event from '+referenztabelle+' where ');
 DataModule1.Query4Result.sql.Add('    (parentevent in (select event from '+referenztabelle+' where ');
 DataModule1.Query4Result.sql.Add('    (parentevent in (select event from '+referenztabelle+' where ');
 DataModule1.Query4Result.sql.Add('    (parentevent in (select event from '+referenztabelle+' where ');
 DataModule1.Query4Result.sql.Add('    (parentevent = '''+below+'''))))))))) ');
 DataModule1.Query4Result.sql.Add('    or ');
 DataModule1.Query4Result.sql.Add('    (event in (select event from '+referenztabelle+' where ');
 DataModule1.Query4Result.sql.Add('    (parentevent in (select event from '+referenztabelle+' where ');
 DataModule1.Query4Result.sql.Add('    (parentevent in (select event from '+referenztabelle+' where ');
 DataModule1.Query4Result.sql.Add('    (parentevent = '''+below+'''))))))) ');
 DataModule1.Query4Result.sql.Add('    or ');
 DataModule1.Query4Result.sql.Add('    (event in (select event from '+referenztabelle+' where ');
 DataModule1.Query4Result.sql.Add('    (parentevent in (select event from '+referenztabelle+' where ');
 DataModule1.Query4Result.sql.Add('    (parentevent = '''+below+'''))))) ');
 DataModule1.Query4Result.sql.Add('    or ');
 DataModule1.Query4Result.sql.Add('    (event in (select event from '+referenztabelle+' where ');
 DataModule1.Query4Result.sql.Add('    (parentevent = '''+below+'''))) ');
 DataModule1.Query4Result.sql.Add('    or ');
 DataModule1.Query4Result.sql.Add('    (event = '''+below+''') ');
 DataModule1.Query4Result.sql.Add('    ) ');
 DataModule1.Query4Result.sql.Add('    ) ');
 DataModule1.Query4Result.sql.Add('and (UIBEVENTTIMESPLIT.starttime >= '''+edit1.Text+''') ');
 DataModule1.Query4Result.sql.Add('and (UIBEVENTTIMESPLIT.stoptime < '''+edit2.Text+''') ');
 DataModule1.Query4Result.sql.Add('and (UIBEVENTTIMESPLIT.userid like '''+searchuser+''') ');
 DataModule1.Query4Result.sql.Add('GROUP BY ');
 DataModule1.Query4Result.sql.Add('UIBEVENTTIMESPLIT.Jahr,UIBEVENTTIMESPLIT.Monat,UIBEVENTTIMESPLIT.TAG,UIBEVENTTIMESPLIT.EVENT ');
 DataModule1.Query4Result.sql.Add(',UIBEVENTTIMESPLIT.USERID,UIB_WORK_DESCRIPTION.DESCRIPTION ');
 DataModule1.Query4Result.sql.Add('ORDER BY UIBEVENTTIMESPLIT.Jahr,UIBEVENTTIMESPLIT.Monat,UIBEVENTTIMESPLIT.TAG,UIBEVENTTIMESPLIT.EVENT ');

 DataModule1.debugOut(6,DataModule1.Query4Result.SQL.Text);
 DataModule1.Query4Result.ReadOnly:=true;
 DataModule1.Query4Result.open;
 //Fresult.DataSource1.DataSet := DataModule1.Query4Result;
 FResult.Edit1.text := 'Arbeitsbericht von '
                        +Edit1.text+'bis (excl.)'
                        +Edit2.text+' unterhalb (+incl.) von '
                        +ComboBoxAktevent.text+' '
                        +searchuserstr;
 Fresult.showmodal();
 Fresult.free;
end;

procedure TFStatistik.BitBtnMonthreportStartStopClick(Sender: TObject);
var
 nextmonth, nextmonthyear : integer;
begin
 Fresult := TFResult.create(self);

 if DataModule1.Query4Result.active then DataModule1.Query4Result.close;
 ///DataModule1.Query4Result.databasename :='uibtime';
 DataModule1.Query4Result.SQL.clear;
 DataModule1.Query4Result.sql.Add('select');
 DataModule1.Query4Result.sql.Add(' Tag,wochentag,  extract(hour from min(starttime)) as Beginn_h, ');
 DataModule1.Query4Result.sql.Add('extract(minute from min(starttime)) as Beginn_min, ');
 DataModule1.Query4Result.sql.Add('extract(hour from max(stoptime)) as Ende_h, ');
 DataModule1.Query4Result.sql.Add('extract(minute from max(stoptime)) as Ende_min, ');
 DataModule1.Query4Result.sql.Add('sum(stunden) as Arbeitszeit ');
 DataModule1.Query4Result.sql.Add('from uibeventtimesplit where');
 DataModule1.Query4Result.sql.Add('(userid = :uid) and ');
 DataModule1.Query4Result.sql.Add('(starttime >= :start) and ');
 DataModule1.Query4Result.sql.Add('(stoptime <= :stop) ');
 DataModule1.Query4Result.sql.Add('and (not (event = ''Pause''))');
  DataModule1.Query4Result.parambyname('uid').asstring := combobox1.text;
 DataModule1.Query4Result.parambyname('start').asstring
  := '1.'+spinedit1.Text+'.'+spinedit2.Text;
 nextmonth := strtoint(spinedit1.Text)+1;
 nextmonthyear := strtoint(spinedit2.Text);
 if nextmonth > 12 then
 begin
  nextmonth := 1;
  inc(nextmonthyear);
 end;
 DataModule1.Query4Result.parambyname('stop').asstring
  := '1.'+inttostr(nextmonth)+'.'+inttostr(nextmonthyear);
 DataModule1.Query4Result.sql.Add(' group by Tag, wochentag');
 DataModule1.Query4Result.ReadOnly:=true;
 DataModule1.Query4Result.open;
 //Fresult.DataSource1.DataSet := DataModule1.Query4Result;
 FResult.Edit1.text := 'Arbeitsbericht für Monat '
                        +spinedit1.Text+'.'
                        +spinedit2.Text
                        +' mit Beginn und Ende sowie Arbeitzeiten ohne Pausen für '
                        +combobox1.text;

 Fresult.showmodal();
 Fresult.free;
end;

procedure TFStatistik.Panel2Click(Sender: TObject);
begin

end;

end.
