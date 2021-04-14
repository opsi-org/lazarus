unit statistik;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, {LMessages, Messages,} SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, DB, sqldb, {Grids,} DBGrids, StdCtrls, {MaskEdit,} ExtCtrls,
  Buttons, Result, sqltext, Spin, LR_Class, LR_DBSet, DateUtils;

type

  { TFStatistik }

  TFStatistik = class(TForm)
    frDBDataSet1: TfrDBDataSet;
    frReport1: TfrReport;
    LabelMonthURL: TLabel;
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
    procedure LabelMonthURLClick(Sender: TObject);
    procedure LabelMonthURLMouseEnter(Sender: TObject);
    procedure LabelMonthURLMouseLeave(Sender: TObject);
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
  Fresult := TFResult.Create(self);
  FResult.setResultDataset(DataModule1.Query4Result);
  if DataModule1.Query4Result.active then
    DataModule1.Query4Result.Close;
  ///DataModule1.Query4Result.databasename :='uibtime';
  DataModule1.Query4Result.SQL.Clear;
  DataModule1.Query4Result.sql.Add('select');
  if (combobox1.Text = 'Summe Alle') then
  else
    DataModule1.Query4Result.sql.Add(' userid,');
  DataModule1.Query4Result.sql.Add('sum(stunden) as summe_h');
  DataModule1.Query4Result.sql.Add('from uibevent where');
  if (pos('Alle', combobox1.Text) = 0) then
    DataModule1.Query4Result.sql.Add('(userid = :uid) and');
  DataModule1.Query4Result.sql.Add('(starttime >= :start) and');
  DataModule1.Query4Result.sql.Add('(starttime <= :stop)');
  DataModule1.Query4Result.sql.Add('and (not (event = ''Pause''))');
  if not (combobox1.Text = 'Summe Alle') then
    DataModule1.Query4Result.sql.Add('group by userid');
  if (pos('Alle', combobox1.Text) = 0) then
    DataModule1.Query4Result.parambyname('uid').AsString := uid;
  DataModule1.Query4Result.parambyname('start').AsDateTime :=
    ScanDateTime('dd.mm.yyyy', edit1.Text);
  DataModule1.Query4Result.parambyname('stop').AsDateTime :=
    ScanDateTime('dd.mm.yyyy', edit2.Text);
  //DataModule1.Query4Result.parambyname('start').AsString := edit1.Text;
  //DataModule1.Query4Result.parambyname('stop').asstring := edit2.text;
  DataModule1.Query4Result.Open;
  //Fresult.DataSource1.DataSet := DataModule1.Query4Result;
  FResult.Edit1.Text := 'Summe h von ' + Edit1.Text + ' bis (excl.)' +
    Edit2.Text + ' für ' + ComboBox1.Text;
  Fresult.showmodal();
  Fresult.Free;
end;

procedure TFStatistik.FormCreate(Sender: TObject);
var
  //startmonth, year, startyear, month, day : word;
  year, month, day: word;
begin
  decodeDate(now, year, month, day);
  SpinEdit1.Value := month;
  SpinEdit2.Value := year;
  combobox1.items.Clear;
  combobox1.items.add(uid);
  combobox1.items.add('Summe Alle');
  combobox1.Text := uid;
  edit1.Text := '01.01.2003';
  edit2.Text := datetostr(date);
  {$IFDEF WINDOWS}
  ComboBoxAktevent.Style := csDropDown;
  {$ElSE WINDOWS}
  ComboBoxAktevent.Style := csSimple;
  {$ENDIF WINDOWS}
  DataModule1.SetFontName(TControl(Sender), myFont);
end;

procedure TFStatistik.Edit1Exit(Sender: TObject);
begin
  edit1.Text := datetostr(ScanDateTime('dd.mm.yyyy', edit1.Text));
end;

procedure TFStatistik.Edit2Exit(Sender: TObject);
begin
  edit2.Text := datetostr(ScanDateTime('dd.mm.yyyy', edit2.Text));
end;

procedure TFStatistik.BtnSollStundenClick(Sender: TObject);
var
  year, month, day: word;
begin
  Fresult := TFResult.Create(self);
  FResult.setResultDataset(DataModule1.Query4Result);
  if DataModule1.Query4Result.active then
    DataModule1.Query4Result.Close;
  ///DataModule1.Query4Result.databasename :='uibtime';
  DataModule1.Query4Result.SQL.Clear;
  DataModule1.Query4Result.sql.Add('select');
  if (combobox1.Text = 'Summe Alle') then
  else
    DataModule1.Query4Result.sql.Add(' userid,');
  DataModule1.Query4Result.sql.Add('sum(stunden) as summe_h');
  DataModule1.Query4Result.sql.Add('from uibsoll where');
  if (pos('Alle', combobox1.Text) = 0) then
    DataModule1.Query4Result.sql.Add('(userid = :uid) and');
  DataModule1.Query4Result.sql.Add('(jahr >= :starty) and');
  DataModule1.Query4Result.sql.Add('(monat >= :startm) and');
  DataModule1.Query4Result.sql.Add('(jahr <= :stopy) and');
  DataModule1.Query4Result.sql.Add('(monat <= :stopm)');
  if not (combobox1.Text = 'Summe Alle') then
    DataModule1.Query4Result.sql.Add('group by userid');
  if (pos('Alle', combobox1.Text) = 0) then
    DataModule1.Query4Result.parambyname('uid').AsString := combobox1.Text;
  decodedate(ScanDateTime('dd.mm.yyyy', edit1.Text), year, month, day);
  DataModule1.Query4Result.parambyname('starty').AsInteger := year;
  DataModule1.Query4Result.parambyname('startm').AsInteger := month;
  decodedate(ScanDateTime('dd.mm.yyyy', edit2.Text), year, month, day);
  DataModule1.Query4Result.parambyname('stopy').AsInteger := year;
  DataModule1.Query4Result.parambyname('stopm').AsInteger := month;
  DataModule1.Query4Result.ReadOnly := True;
  DataModule1.Query4Result.Open;
  //Fresult.DataSource1.DataSet := DataModule1.Query4Result;
  FResult.Edit1.Text := 'Sum Soll-h von ' + Edit1.Text + ' bis (excl.)' +
    Edit2.Text + ' für ' + ComboBox1.Text;
  Fresult.showmodal();
  Fresult.Free;
end;

procedure TFStatistik.BtnFehlClick(Sender: TObject);
var
  year, month, day: word;
begin
  Fresult := TFResult.Create(self);
  FResult.setResultDataset(DataModule1.Query4Result);
  if DataModule1.Query4Result.active then
    DataModule1.Query4Result.Close;
  ///DataModule1.Query4Result.databasename :='uibtime';
  DataModule1.Query4Result.SQL.Clear;
  DataModule1.Query4Result.sql.Add('select');
  (* if not (pos('Alle',combobox1.Text) <>0) then*)
  DataModule1.Query4Result.sql.Add(' userid,');
  DataModule1.Query4Result.sql.Add(
    '(sum(stunden) - (select sum(stunden) from uibevent ');
  (* if not (pos('Alle',combobox1.Text) <>0) then*)
  DataModule1.Query4Result.sql.Add('where (userid = :uid) and');
  DataModule1.Query4Result.sql.Add('(starttime >= :start) and');
  DataModule1.Query4Result.sql.Add('(starttime <= :stop)');
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
  DataModule1.Query4Result.parambyname('uid').AsString := uid;
  decodedate(ScanDateTime('dd.mm.yyyy', edit1.Text), year, month, day);
  DataModule1.Query4Result.parambyname('starty').AsInteger := year;
  DataModule1.Query4Result.parambyname('startm').AsInteger := month;
  decodedate(ScanDateTime('dd.mm.yyyy', edit2.Text), year, month, day);
  DataModule1.Query4Result.parambyname('stopy').AsInteger := year;
  DataModule1.Query4Result.parambyname('stopm').AsInteger := month;
  DataModule1.Query4Result.parambyname('start').AsDateTime :=
    ScanDateTime('dd.mm.yyyy', edit1.Text);
  DataModule1.Query4Result.parambyname('stop').AsDateTime :=
    ScanDateTime('dd.mm.yyyy', edit2.Text);
  //DataModule1.Query4Result.parambyname('start').AsString := edit1.Text;
  //DataModule1.Query4Result.parambyname('stop').AsString := edit2.Text;
  DataModule1.Query4Result.ReadOnly := True;
  DataModule1.Query4Result.Open;
  //Fresult.DataSource1.DataSet := DataModule1.Query4Result;
  FResult.Edit1.Text := 'Fehlstunden h von ' + Edit1.Text + ' bis (excl.)' +
    Edit2.Text + ' für ' + ComboBox1.Text;
  Fresult.showmodal();
  Fresult.Free;
end;

procedure TFStatistik.BtnAufteilungClick(Sender: TObject);
var
  fc: integer;
begin
  Fresult := TFResult.Create(self);
  FResult.setResultDataset(DataModule1.Query4Result);
  if DataModule1.Query4Result.active then
    DataModule1.Query4Result.Close;
  ///DataModule1.Query4Result.databasename :='uibtime';
  DataModule1.Query4Result.SQL.Clear;
  DataModule1.Query4Result.sql.Add('select');
  DataModule1.Query4Result.sql.Add(' event, ');
  if (combobox1.Text = 'Summe Alle') then
  else
    DataModule1.Query4Result.sql.Add(' userid,');
  DataModule1.Query4Result.sql.Add(' sum(stunden) as summe_h');
  DataModule1.Query4Result.sql.Add(' from uibevent where');
  if (pos('Alle', combobox1.Text) = 0) then
    DataModule1.Query4Result.sql.Add('(userid = :uid) and');
  DataModule1.Query4Result.sql.Add('(starttime >= :start) and');
  DataModule1.Query4Result.sql.Add('(starttime <= :stop)');
  DataModule1.Query4Result.sql.Add('and (not (event = ''Pause''))');
  DataModule1.Query4Result.sql.Add('group by event ');
  if not (combobox1.Text = 'Summe Alle') then
    DataModule1.Query4Result.sql.Add(',userid');
  if (pos('Alle', combobox1.Text) = 0) then
    DataModule1.Query4Result.parambyname('uid').AsString := combobox1.Text;
  DataModule1.Query4Result.parambyname('start').AsDateTime :=
    ScanDateTime('dd.mm.yyyy', edit1.Text);
  DataModule1.Query4Result.parambyname('stop').AsDateTime :=
    ScanDateTime('dd.mm.yyyy', edit2.Text);
  //DataModule1.Query4Result.parambyname('start').AsString := edit1.Text;
  //DataModule1.Query4Result.parambyname('stop').AsString := edit2.Text;
  DataModule1.Query4Result.ReadOnly := True;
  DataModule1.Query4Result.Open;
  for fc := 0 to DataModule1.Query4Result.Fieldcount - 1 do
    DataModule1.Query4Result.Fields[fc].Visible := True;
  //Fresult.DataSource1.DataSet := DataModule1.Query4Result;
  FResult.Edit1.Text := 'Art h von ' + Edit1.Text + ' bis (excl.)' +
    Edit2.Text + ' für ' + ComboBox1.Text;
  Fresult.showmodal();
  Fresult.Free;
end;

procedure TFStatistik.FormActivate(Sender: TObject);
//var
// i : integer;
begin
  datamodule1.debugOut(5, 'Activate FStatistik');
  if DataModule1.Query4Result.active then
    DataModule1.Query4Result.Close;
  ///DataModule1.Query4Result.databasename :='uibtime';
  DataModule1.Query4Result.SQL.Clear;
  DataModule1.Query4Result.sql.Add('select event from uibaktevent order by event');
  DataModule1.Query4Result.ReadOnly := True;
  DataModule1.Query4Result.Open;
  ComboBoxAktevent.Items.Clear;
  while not DataModule1.Query4Result.EOF do
  begin
    ComboBoxAktevent.Items.Add(DataModule1.Query4Result.FieldByName('event').AsString);
    DataModule1.Query4Result.Next;
  end;
  BtnLastMonthClick(Sender);
end;

procedure TFStatistik.BtnTageClick(Sender: TObject);
//var
// fc : integer;
begin
  Fresult := TFResult.Create(self);
  FResult.setResultDataset(DataModule1.Query4Result);
  if DataModule1.Query4Result.active then
    DataModule1.Query4Result.Close;
  ///DataModule1.Query4Result.databasename :='uibtime';
  DataModule1.Query4Result.SQL.Clear;
  DataModule1.Query4Result.sql.Add('select');
  DataModule1.Query4Result.sql.Add(' Tag,Monat,Jahr, sum(stunden) as Summe ');
  DataModule1.Query4Result.sql.Add('from uibeventtimesplit where');
  if (pos('Alle', combobox1.Text) = 0) then
    DataModule1.Query4Result.sql.Add('(userid = :uid) and');
  DataModule1.Query4Result.sql.Add('(starttime >= :start) and');
  DataModule1.Query4Result.sql.Add('(starttime <= :stop)');
  DataModule1.Query4Result.sql.Add('and (not (event = ''Pause''))');
  if (pos('Alle', combobox1.Text) = 0) then
    DataModule1.Query4Result.parambyname('uid').AsString := combobox1.Text;
  DataModule1.Query4Result.parambyname('start').AsDateTime :=
    ScanDateTime('dd.mm.yyyy', edit1.Text);
  DataModule1.Query4Result.parambyname('stop').AsDateTime :=
    ScanDateTime('dd.mm.yyyy', edit2.Text);
  //DataModule1.Query4Result.parambyname('start').AsString := edit1.Text;
  //DataModule1.Query4Result.parambyname('stop').AsString := edit2.Text;
  DataModule1.Query4Result.sql.Add(' group by Tag, monat, jahr');
  DataModule1.Query4Result.ReadOnly := True;
  DataModule1.Query4Result.Open;

  //Fresult.DataSource1.DataSet := DataModule1.Query4Result;
  FResult.Edit1.Text := 'Sum. h / Monatstag von ' + Edit1.Text +
    ' bis (excl.)' + Edit2.Text + ' für ' + ComboBox1.Text;
  Fresult.showmodal();
  Fresult.Free;
end;

procedure TFStatistik.BtnMonateClick(Sender: TObject);
begin
  Fresult := TFResult.Create(self);
  FResult.setResultDataset(DataModule1.Query4Result);
  if DataModule1.Query4Result.active then
    DataModule1.Query4Result.Close;
  ///DataModule1.Query4Result.databasename :='uibtime';
  DataModule1.Query4Result.SQL.Clear;
  DataModule1.Query4Result.sql.Add('select');
  DataModule1.Query4Result.sql.Add(' Monat,Jahr, sum(stunden) as Summe ');
  DataModule1.Query4Result.sql.Add('from uibeventtimesplit where');
  if (pos('Alle', combobox1.Text) = 0) then
    DataModule1.Query4Result.sql.Add('(userid = :uid) and');
  DataModule1.Query4Result.sql.Add('(starttime >= :start) and');
  DataModule1.Query4Result.sql.Add('(starttime <= :stop)');
  DataModule1.Query4Result.sql.Add('and (not (event = ''Pause''))');
  if (pos('Alle', combobox1.Text) = 0) then
    DataModule1.Query4Result.parambyname('uid').AsString := combobox1.Text;
  DataModule1.Query4Result.parambyname('start').AsDateTime :=
    ScanDateTime('dd.mm.yyyy', edit1.Text);
  DataModule1.Query4Result.parambyname('stop').AsDateTime :=
    ScanDateTime('dd.mm.yyyy', edit2.Text);
  //DataModule1.Query4Result.parambyname('start').AsString := edit1.Text;
  //DataModule1.Query4Result.parambyname('stop').AsString := edit2.Text;
  DataModule1.Query4Result.sql.Add(' group by Monat, jahr');
  DataModule1.Query4Result.ReadOnly := True;
  DataModule1.Query4Result.Open;
  //Fresult.DataSource1.DataSet := DataModule1.Query4Result;
  FResult.Edit1.Text := 'Sum h/Mon. von ' + Edit1.Text + ' bis (excl.)' +
    Edit2.Text + ' für ' + ComboBox1.Text;
  Fresult.showmodal();
  Fresult.Free;

end;

procedure TFStatistik.BtnSQLtextClick(Sender: TObject);
begin
  FSQL := TFSQL.Create(self);
  if DataModule1.Query4Result.active then
    DataModule1.Query4Result.Close;
  ///DataModule1.Query4Result.databasename :='uibtime';
  DataModule1.Query4Result.SQL.Clear;
  FSQL.showmodal();
end;

procedure TFStatistik.BtnLastMonthClick(Sender: TObject);
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

procedure TFStatistik.BtnMonthRepClick(Sender: TObject);
var
  nextmonth, nextmonthyear: integer;
  mypath: string;
  startdt, stopdt: TDateTime;
  sollbrutto: double; // contract
  sollnetto: double; // abzgl. zeitgutschriten (feiertag)
  feierstunden: double; // zeitgutschriten (feiertag)
  summework: double;  // summe work
  ueberstunden: double;  // summe work - soll
  tmptime: TDateTime;
  hours, minutes: integer;
  outstr: string;
begin
  ///FRmonat := TFRmonat.create(self);
  DefaultFormatSettings.ShortDateFormat := 'dd.mm.yyyy';
  //startdt := StrToDateTime('1.'+spinedit1.Text+'.'+spinedit2.Text);
  startdt := encodedate(StrToInt(spinedit2.Text), StrToInt(spinedit1.Text), 1);
  if query1.active then
    query1.Close;
  ///query1.databasename :='uibtime';
  query1.SQL.Clear;
  query1.sql.Add('select');
  query1.sql.Add(
    ' a.Tag, b.daystr,  min(a.starttime), max(a.stoptime), sum(a.stunden) as Summe ');
  query1.sql.Add('from uibeventtimesplit as a, uibtwoche as b where');
  query1.sql.Add('(a.wochentag = b.daynum) and ');
  query1.sql.Add('(a.userid = :uid) and ');
  query1.sql.Add('(a.starttime >= :start) and ');
  query1.sql.Add('(a.starttime <= :stop) ');
  query1.sql.Add('and (not (event = ''Pause''))');
  query1.parambyname('uid').AsString := combobox1.Text;
  query1.parambyname('start').AsDateTime := startdt;
  //  := '1.'+spinedit1.Text+'.'+spinedit2.Text;
  nextmonth := StrToInt(spinedit1.Text) + 1;
  nextmonthyear := StrToInt(spinedit2.Text);
  if nextmonth > 12 then
  begin
    nextmonth := 1;
    Inc(nextmonthyear);
  end;
  stopdt := encodedate(nextmonthyear, nextmonth, 1);
  //stopdt := StrToDateTime('1.'+inttostr(nextmonth)+'.'+inttostr(nextmonthyear));
  query1.parambyname('stop').AsDateTime := stopdt;
  //  := '1.'+inttostr(nextmonth)+'.'+inttostr(nextmonthyear);
  query1.sql.Add(' group by Tag, daystr');
  query1.ReadOnly := True;
  query1.Open;
  summework := 0;
  query1.First;
  while not query1.EOF do
  begin
    summework := summework + query1.FieldByName('Summe').AsFloat;
    query1.Next;
  end;
  query1.First;

  // sollstunden brutto (gemäß vertrag) holen....
  // sollstunden netto (nach abzug zeitgutschriften) holen....
  ///Datamodule1.Tableuser.FindKey([uid]);
  if querysollstunden.active then
    querysollstunden.Close;
  querysollstunden.SQL.Clear;
  querysollstunden.SQL.Add('select UIB_MONTH_SOLL_CONTRACT, ');
  querysollstunden.SQL.Add('UIB_MONTH_SOLL  ');
  querysollstunden.SQL.Add('from EVAL_MONTH where');
  querysollstunden.sql.Add('(UIB_USER = :uid) ');
  querysollstunden.sql.Add('and (UIB_YEAR = ' + spinedit2.Text + ')');
  querysollstunden.sql.Add('and (UIB_MONTH = ' + spinedit1.Text + ')');
  querysollstunden.parambyname('uid').AsString := combobox1.Text;
  ///  := Datamodule1.Tableuser.fieldbyname('userid').asstring;
  querysollstunden.Open;
  sollbrutto := querysollstunden.FieldByName('UIB_MONTH_SOLL_CONTRACT').AsFloat;
  sollnetto := querysollstunden.FieldByName('UIB_MONTH_SOLL').AsFloat;
  feierstunden := sollbrutto - sollnetto;
  ueberstunden := summework - sollnetto;

  (*
  // sollstunden netto (nach abzug zeitgutschriften) holen....
  ///Datamodule1.Tableuser.FindKey([uid]);
  if query2.active then
    query2.Close;
  query2.SQL.Clear;    // EVAL_MONTH.UIB_MONTH_SOLL_CONTRACT
  //UIB_USER,UIB_YEAR,UIB_MONTH
  query2.SQL.Add('select UIB_MONTH_SOLL_CONTRACT from EVAL_MONTH where');
  query2.sql.Add('(UIB_USER = :uid) ');
  query2.sql.Add('and (UIB_YEAR = ' + spinedit2.Text + ')');
  query2.sql.Add('and (UIB_MONTH = ' + spinedit1.Text + ')');
  query2.parambyname('uid').AsString := combobox1.Text;
  ///  := Datamodule1.Tableuser.fieldbyname('userid').asstring;
  query2.Open;
  *)
 {$IFDEF Linux}
  // development:
  mypath := ExtractFilePath(ParamStr(0));
  if not FileExists(mypath + 'monthrep.lrf') then
    mypath := '/usr/share/uibtime/';
 {$ELSE}
  mypath := ExtractFilePath(ParamStr(0));
 {$ENDIF Linux}
  frReport1.LoadFromFile(mypath + 'monthrep.lrf');

  frReport1.FindObject('memoTitle').Memo.Text :=
    'Monatsreport für ' + Datamodule1.SQuibaktuser.FieldByName('name').AsString;
  frReport1.FindObject('memoStartEnd').Memo.Text :=
    'Von 1.' + spinedit1.Text + '.' + spinedit2.Text + ' bis (excl.) 1.' +
    IntToStr(nextmonth) + '.' + IntToStr(nextmonthyear);

  hours := trunc(feierstunden);
  minutes := trunc((feierstunden - hours) * 60);
  outstr := '(' + IntToStr(hours) + ':' + format('%.2d', [minutes]) + ' h)       ' +
    FormatFloat('###0.00', feierstunden);
  frReport1.FindObject('gutschrift').Memo.Text := outstr;

  //FormatDateTime('hh:nn',feierstunden);
  hours := trunc(summework + feierstunden);
  minutes := trunc((summework + feierstunden - hours) * 60);
  outstr := '(' + IntToStr(hours) + ':' + format('%.2d', [minutes]) + ' h)       ' +
    FormatFloat('###0.00', summework + feierstunden);
  frReport1.FindObject('zwsumme').Memo.Text := outstr;

  // FormatDateTime('hh:nn',TDateTime(summework + feierstunden))+'*';
  hours := trunc(sollbrutto);
  minutes := trunc((sollbrutto - hours) * 60);
  outstr := '(' + IntToStr(hours) + ':' + format('%.2d', [minutes]) + ' h)       ' +
    FormatFloat('###0.00', sollbrutto);
  frReport1.FindObject('sollstunden').Memo.Text := outstr;

  //FormatDateTime('hh:nn',TDateTime(sollbrutto))+'+';
  hours := trunc(ueberstunden);
  minutes := trunc((ueberstunden - hours) * 60);
  outstr := '(' + IntToStr(hours) + ':' + format('%.2d', [minutes]) + ' h)       ' +
    FormatFloat('###0.00', ueberstunden);
  frReport1.FindObject('ueberstunden').Memo.Text := outstr;
  Application.ProcessMessages;

  //FormatDateTime('hh:nn',tmptime)

  frReport1.ShowReport;
  Application.ProcessMessages;
end;

procedure TFStatistik.BtnTreeSumClick(Sender: TObject);
var
  searchuserstr: string;
  procedurename: string;
begin
  if RadioGroupReference.ItemIndex = 0 then
    procedurename := 'uibbuild_tmp_treesum_aktevent'
  else
    procedurename := 'uibbuild_tmp_treesum';
  Fresult := TFResult.Create(self);
  FResult.setResultDataset(DataModule1.Query4Result);
  if DataModule1.Query4Result.active then
    DataModule1.Query4Result.Close;
  ///DataModule1.Query4Result.databasename :='uibtime';
  DataModule1.Query4Result.SQL.Clear;
  DataModule1.Query4Result.sql.Add('execute procedure ' + procedurename + '(');
  DataModule1.Query4Result.sql.Add(':below,:userid, :searchuser,:start, :stop);');
  DataModule1.Query4Result.parambyname('userid').AsString := uid;
  DataModule1.Query4Result.parambyname('below').AsString := ComboBoxAktevent.Text;
  DataModule1.Query4Result.parambyname('start').AsDateTime :=
    ScanDateTime('dd.mm.yyyy', edit1.Text);
  DataModule1.Query4Result.parambyname('stop').AsDateTime :=
    ScanDateTime('dd.mm.yyyy', edit2.Text);
  //DataModule1.Query4Result.parambyname('start').AsString := edit1.Text;
  //DataModule1.Query4Result.parambyname('stop').AsString := edit2.Text;
  if (combobox1.Text = 'Summe Alle') then
  begin
    DataModule1.Query4Result.parambyname('searchuser').AsString := '%';
    searchuserstr := 'für alle.';
  end
  else
  begin
    DataModule1.Query4Result.parambyname('searchuser').AsString := uid;
    searchuserstr := 'für ' + uid;
  end;
  try
    DataModule1.debugOut(6, 'Statistik.BtnTreeSumClick', 'Will execute sql: ' +
      DataModule1.Query4Result.SQL.Text);
    screen.Cursor := crSQLWait;
    Application.ProcessMessages;
    DataModule1.Query4Result.ExecSQL;
  finally
    screen.Cursor := crDefault;
    Application.ProcessMessages;
  end;
  DataModule1.debugOut(6, 'Statistik.BtnTreeSumClick', 'Procedure call finished');
  if DataModule1.Query4Result.active then
    DataModule1.Query4Result.Close;
  DataModule1.Query4Result.SQL.Clear;
  DataModule1.Query4Result.sql.Add('select event, stunden from uibtmptreesum ');
  DataModule1.Query4Result.sql.Add('where userid = :userid');
  DataModule1.Query4Result.parambyname('userid').AsString := uid;
  DataModule1.Query4Result.ReadOnly := True;
  DataModule1.Query4Result.Open;
  //Fresult.DataSource1.DataSet := DataModule1.Query4Result;
  FResult.Edit1.Text := 'Stundensummen von ' + Edit1.Text + 'bis (excl.)' +
    Edit2.Text + ' unterhalb (+incl.) von ' + ComboBoxAktevent.Text +
    ' ' + searchuserstr;
  Fresult.showmodal();
  Fresult.Free;
end;

procedure TFStatistik.BtnBSZreportClick(Sender: TObject);
var
  searchuserstr: string;
  procedurename: string;
begin
  if RadioGroupReference.ItemIndex = 0 then
    procedurename := 'buildbsz_tmp_monthrep_aktevent'
  else
    procedurename := 'buildbsz_tmp_monthrep';
  Fresult := TFResult.Create(self);
  FResult.setResultDataset(DataModule1.Query4Result);
  if DataModule1.Query4Result.active then
    DataModule1.Query4Result.Close;
  ///DataModule1.Query4Result.databasename :='uibtime';
  DataModule1.Query4Result.SQL.Clear;
  DataModule1.Query4Result.sql.Add('execute procedure ' + procedurename + '(');
  DataModule1.Query4Result.sql.Add(':userid, :searchuser,:start, :stop);');
  DataModule1.Query4Result.parambyname('userid').AsString := uid;
  DataModule1.Query4Result.parambyname('start').AsDateTime :=
    ScanDateTime('dd.mm.yyyy', edit1.Text);
  DataModule1.Query4Result.parambyname('stop').AsDateTime :=
    ScanDateTime('dd.mm.yyyy', edit2.Text);
  //DataModule1.Query4Result.parambyname('start').AsString := edit1.Text;
  //DataModule1.Query4Result.parambyname('stop').AsString := edit2.Text;
  if (combobox1.Text = 'Summe Alle') then
  begin
    DataModule1.Query4Result.parambyname('searchuser').AsString := '%';
    searchuserstr := ' für alle.';
  end
  else
  begin
    DataModule1.Query4Result.parambyname('searchuser').AsString := uid;
    searchuserstr := ' für ' + uid;
  end;
  DataModule1.Query4Result.ExecSQL;
  if DataModule1.Query4Result.active then
    DataModule1.Query4Result.Close;
  DataModule1.Query4Result.SQL.Clear;
  DataModule1.Query4Result.sql.Add(
    'select event, id_string, stunden from uibtmptreesum ');
  DataModule1.Query4Result.sql.Add('where userid = :userid');
  DataModule1.Query4Result.parambyname('userid').AsString := uid;
  DataModule1.Query4Result.ReadOnly := True;
  DataModule1.Query4Result.Open;
  //Fresult.DataSource1.DataSet := DataModule1.Query4Result;
  FResult.Edit1.Text := 'BSZ-UR Controllingreport von ' + Edit1.Text +
    'bis (excl.)' + Edit2.Text + searchuserstr;
  Fresult.showmodal();
  Fresult.Free;
end;

procedure TFStatistik.BitBtn1Click(Sender: TObject);
begin
  hide;
  Close;
end;

procedure TFStatistik.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Datamodule1.Statistik1Cancel;
end;

procedure TFStatistik.BtnThisMonthClick(Sender: TObject);
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

procedure TFStatistik.BtnThisWeekClick(Sender: TObject);
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

procedure TFStatistik.BtnLastWeekClick(Sender: TObject);
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


procedure TFStatistik.BitBtnKnotenByUserClick(Sender: TObject);
var
  referenztabelle: string;
begin
  if RadioGroupReference.ItemIndex = 0 then
    referenztabelle := 'uibaktevent'
  else
    referenztabelle := 'uiballevent';
  Fresult := TFResult.Create(self);
  FResult.setResultDataset(DataModule1.Query4Result);
  if DataModule1.Query4Result.active then
    DataModule1.Query4Result.Close;
  ///DataModule1.Query4Result.databasename :='uibtime';
  DataModule1.Query4Result.SQL.Clear;

  DataModule1.Query4Result.sql.Add(
    ' select userid , sum(stunden) as stunden from uibevent');
  DataModule1.Query4Result.sql.Add('    where (');
  DataModule1.Query4Result.sql.Add('    (event in (select event from ' +
    referenztabelle + ' where');
  DataModule1.Query4Result.sql.Add('    (parentevent in (select event from ' +
    referenztabelle + ' where');
  DataModule1.Query4Result.sql.Add('    (parentevent in (select event from ' +
    referenztabelle + ' where');
  DataModule1.Query4Result.sql.Add('    (parentevent in (select event from ' +
    referenztabelle + ' where');
  DataModule1.Query4Result.sql.Add('    (parentevent in (select event from ' +
    referenztabelle + ' where');
  DataModule1.Query4Result.sql.Add('    (parentevent in (select event from ' +
    referenztabelle + ' where');
  DataModule1.Query4Result.sql.Add('    (parentevent = :below_event)))))))))))))');
  DataModule1.Query4Result.sql.Add('    or');
  DataModule1.Query4Result.sql.Add('    (event in (select event from ' +
    referenztabelle + ' where');
  DataModule1.Query4Result.sql.Add('    (parentevent in (select event from ' +
    referenztabelle + ' where');
  DataModule1.Query4Result.sql.Add('    (parentevent in (select event from ' +
    referenztabelle + ' where');
  DataModule1.Query4Result.sql.Add('    (parentevent in (select event from ' +
    referenztabelle + ' where');
  DataModule1.Query4Result.sql.Add('    (parentevent in (select event from ' +
    referenztabelle + ' where');
  DataModule1.Query4Result.sql.Add('    (parentevent = :below_event)))))))))))');
  DataModule1.Query4Result.sql.Add('    or');
  DataModule1.Query4Result.sql.Add('    (event in (select event from ' +
    referenztabelle + ' where');
  DataModule1.Query4Result.sql.Add('    (parentevent in (select event from ' +
    referenztabelle + ' where');
  DataModule1.Query4Result.sql.Add('    (parentevent in (select event from ' +
    referenztabelle + ' where');
  DataModule1.Query4Result.sql.Add('    (parentevent in (select event from ' +
    referenztabelle + ' where');
  DataModule1.Query4Result.sql.Add('    (parentevent = :below_event)))))))))');
  DataModule1.Query4Result.sql.Add('    or');
  DataModule1.Query4Result.sql.Add('    (event in (select event from ' +
    referenztabelle + ' where');
  DataModule1.Query4Result.sql.Add('    (parentevent in (select event from ' +
    referenztabelle + ' where');
  DataModule1.Query4Result.sql.Add('    (parentevent in (select event from ' +
    referenztabelle + ' where');
  DataModule1.Query4Result.sql.Add('    (parentevent = :below_event)))))))');
  DataModule1.Query4Result.sql.Add('    or');
  DataModule1.Query4Result.sql.Add('    (event in (select event from ' +
    referenztabelle + ' where');
  DataModule1.Query4Result.sql.Add('    (parentevent in (select event from ' +
    referenztabelle + ' where');
  DataModule1.Query4Result.sql.Add('    (parentevent = :below_event)))))');
  DataModule1.Query4Result.sql.Add('    or');
  DataModule1.Query4Result.sql.Add('    (event in (select event from uiballevent');
  DataModule1.Query4Result.sql.Add('    where (parentevent like :below_event)))');
  DataModule1.Query4Result.sql.Add('    or');
  DataModule1.Query4Result.sql.Add('    (event = :below_event)');
  DataModule1.Query4Result.sql.Add('    )');
  DataModule1.Query4Result.sql.Add('    and (starttime >= :von)');
  DataModule1.Query4Result.sql.Add('    and (starttime < :bis)');
  DataModule1.Query4Result.sql.Add('    group by userid;');
  DataModule1.Query4Result.parambyname('below_event').AsString := ComboBoxAktevent.Text;
  DataModule1.Query4Result.parambyname('von').AsDateTime :=
    ScanDateTime('dd.mm.yyyy', edit1.Text);
  DataModule1.Query4Result.parambyname('bis').AsDateTime :=
    ScanDateTime('dd.mm.yyyy', edit2.Text);
  //DataModule1.Query4Result.parambyname('von').AsString := edit1.Text;
  //DataModule1.Query4Result.parambyname('bis').AsString := edit2.Text;
  DataModule1.Query4Result.ReadOnly := True;
  DataModule1.Query4Result.Open;
  //Fresult.DataSource1.DataSet := DataModule1.Query4Result;
  FResult.Edit1.Text := 'Stunden pro User ab Knoten: ' + ComboBoxAktevent.Text +
    ' von ' + Edit1.Text + ' bis (excl.)' + Edit2.Text;
  Fresult.showmodal();
  Fresult.Free;
end;



procedure TFStatistik.BitBtnControlRep2Click(Sender: TObject);
var
  searchuserstr: string;
  procedurename: string;
begin
  if RadioGroupReference.ItemIndex = 0 then
    procedurename := 'buildbsz2_tmp_monthrep_aktevent'
  else
    procedurename := 'buildbsz2_tmp_monthrep';
  Fresult := TFResult.Create(self);
  FResult.setResultDataset(DataModule1.Query4Result);
  if DataModule1.Query4Result.active then
    DataModule1.Query4Result.Close;
  ///DataModule1.Query4Result.databasename :='uibtime';
  DataModule1.Query4Result.SQL.Clear;
  DataModule1.Query4Result.sql.Add('execute procedure ' + procedurename + '(');
  DataModule1.Query4Result.sql.Add(':userid, :searchuser,:start, :stop);');
  DataModule1.Query4Result.parambyname('userid').AsString := uid;
  DataModule1.Query4Result.parambyname('start').AsDateTime :=
    ScanDateTime('dd.mm.yyyy', edit1.Text);
  DataModule1.Query4Result.parambyname('stop').AsDateTime :=
    ScanDateTime('dd.mm.yyyy', edit2.Text);
  //DataModule1.Query4Result.parambyname('start').AsString := edit1.Text;
  //DataModule1.Query4Result.parambyname('stop').AsString := edit2.Text;
  if (combobox1.Text = 'Summe Alle') then
  begin
    DataModule1.Query4Result.parambyname('searchuser').AsString := '%';
    searchuserstr := ' für alle.';
  end
  else
  begin
    DataModule1.Query4Result.parambyname('searchuser').AsString := uid;
    searchuserstr := ' für ' + uid;
  end;
  DataModule1.Query4Result.ExecSQL;
  if DataModule1.Query4Result.active then
    DataModule1.Query4Result.Close;
  DataModule1.Query4Result.SQL.Clear;
  DataModule1.Query4Result.sql.Add(
    'select event, id_string, uibuser, stunden from uibtmptreesum2 ');
  DataModule1.Query4Result.sql.Add('where userid = :userid');
  DataModule1.Query4Result.parambyname('userid').AsString := uid;
  DataModule1.Query4Result.ReadOnly := True;
  DataModule1.Query4Result.Open;
  //Fresult.DataSource1.DataSet := DataModule1.Query4Result;
  FResult.Edit1.Text := 'BSZ-UR Controllingreport von ' + Edit1.Text +
    'bis (excl.)' + Edit2.Text + searchuserstr;
  Fresult.showmodal();
  Fresult.Free;
end;

procedure TFStatistik.BtnWorkDescriptionClick(Sender: TObject);
begin
  if Fwork_description = nil then
    Fwork_description := TFwork_description.Create(self);
  Fwork_description.PopupParent := Fwork_description;
  Fwork_description.showmodal();
  FreeAndNil(Fwork_description);
  //Fwork_description.Show;
end;

procedure TFStatistik.BitBtnworkreportClick(Sender: TObject);
var
  searchuserstr: string;
  procedurename: string;
begin
  if RadioGroupReference.ItemIndex = 0 then
    procedurename := 'buildbsz_tmp_sapevent_aktevent'
  else
    procedurename := 'buildbsz_tmp_sapevent';
  Fresult := TFResult.Create(self);
  FResult.setResultDataset(DataModule1.Query4Result);
  if DataModule1.Query4Result.active then
    DataModule1.Query4Result.Close;
  ///DataModule1.Query4Result.databasename :='uibtime';
  DataModule1.Query4Result.SQL.Clear;
  DataModule1.Query4Result.sql.Add('execute procedure ' + procedurename + ';');
  DataModule1.Query4Result.ExecSQL;
  if DataModule1.Query4Result.active then
    DataModule1.Query4Result.Close;
  DataModule1.Query4Result.SQL.Clear;
  DataModule1.Query4Result.sql.Add('select');
  DataModule1.Query4Result.sql.Add(' UIBEVENTTIMESPLIT.TAG, ');
  DataModule1.Query4Result.sql.Add(' UIBEVENTTIMESPLIT.USERID, ');
  DataModule1.Query4Result.sql.Add(' UIBEVENTTIMESPLIT.EVENT, ');
  DataModule1.Query4Result.sql.Add(' uibtmp_sapevent.sapevent, ');
  DataModule1.Query4Result.sql.Add(' uibtmp_sapevent.id_string, ');
  DataModule1.Query4Result.sql.Add(' sum(UIBEVENTTIMESPLIT.STUNDEN), ');
  DataModule1.Query4Result.sql.Add(' UIB_WORK_DESCRIPTION.DESCRIPTION ');
  DataModule1.Query4Result.sql.Add(' from ');
  DataModule1.Query4Result.sql.Add(
    ' UIBEVENTTIMESPLIT LEFT OUTER JOIN UIB_WORK_DESCRIPTION ON ');
  DataModule1.Query4Result.sql.Add(
    ' (UIBEVENTTIMESPLIT.USERID = UIB_WORK_DESCRIPTION.USERID) and ');
  DataModule1.Query4Result.sql.Add(
    ' (UIBEVENTTIMESPLIT.TAG = UIB_WORK_DESCRIPTION.TAG) and ');
  DataModule1.Query4Result.sql.Add(
    ' (UIBEVENTTIMESPLIT.MONAT = UIB_WORK_DESCRIPTION.MONAT) and ');
  DataModule1.Query4Result.sql.Add(
    ' (UIBEVENTTIMESPLIT.JAHR = UIB_WORK_DESCRIPTION.JAHR) and ');
  DataModule1.Query4Result.sql.Add(
    ' (UIBEVENTTIMESPLIT.EVENT = UIB_WORK_DESCRIPTION.EVENT), ');
  DataModule1.Query4Result.sql.Add(' uibtmp_sapevent ');
  DataModule1.Query4Result.sql.Add(' where ( ');
  DataModule1.Query4Result.sql.Add(
    ' (UIBEVENTTIMESPLIT.EVENT = uibtmp_sapevent.event) and ');
  DataModule1.Query4Result.sql.Add(
    ' (UIBEVENTTIMESPLIT.EVENT in (select event from uibtmp_sapevent)) and ');
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
  DataModule1.Query4Result.sql.Add(
    ' UIBEVENTTIMESPLIT.TAG, UIBEVENTTIMESPLIT.USERID, UIBEVENTTIMESPLIT.EVENT');

  if (combobox1.Text = 'Summe Alle') then
  begin
    DataModule1.Query4Result.parambyname('searchuser').AsString := '%';
    searchuserstr := ' für alle.';
  end
  else
  begin
    DataModule1.Query4Result.parambyname('searchuser').AsString := uid;
    searchuserstr := ' für ' + uid;
  end;
  DataModule1.Query4Result.parambyname('month').AsString := spinedit1.Text;
  DataModule1.Query4Result.parambyname('year').AsString := spinedit2.Text;
  DataModule1.Query4Result.ReadOnly := True;
  DataModule1.Query4Result.Open;
  //Fresult.DataSource1.DataSet := DataModule1.Query4Result;
  FResult.Edit1.Text := 'BSZ-UR Tätigkeitsbericht für ' + spinedit1.Text +
    '.' + spinedit2.Text + searchuserstr;
  Fresult.showmodal();
  Fresult.Free;
end;


procedure TFStatistik.BitBtnArbeitsberichtOutputClick(Sender: TObject);
var
  searchuserstr, searchuser, below: string;
  referenztabelle: string;
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
    searchuserstr := 'für ' + uid;
  end;
  below := ComboBoxAktevent.Text;
  Fresult := TFResult.Create(self);
  FResult.setResultDataset(DataModule1.Query4Result);
  if DataModule1.Query4Result.active then
    DataModule1.Query4Result.Close;
  DataModule1.Query4Result.SQL.Clear;
  DataModule1.Query4Result.sql.Add(
    ' select UIBEVENTTIMESPLIT.JAHR,UIBEVENTTIMESPLIT.Monat,UIBEVENTTIMESPLIT.TAG,');
  DataModule1.Query4Result.sql.Add(
    'UIBEVENTTIMESPLIT.EVENT,UIBEVENTTIMESPLIT.USERID,sum(UIBEVENTTIMESPLIT.STUNDEN),UIB_WORK_DESCRIPTION.DESCRIPTION ');
  DataModule1.Query4Result.sql.Add('from ');
  DataModule1.Query4Result.sql.Add(
    'UIBEVENTTIMESPLIT LEFT OUTER JOIN UIB_WORK_DESCRIPTION ');
  DataModule1.Query4Result.sql.Add('ON ');
  DataModule1.Query4Result.sql.Add(
    '(UIBEVENTTIMESPLIT.USERID = UIB_WORK_DESCRIPTION.USERID) ');
  DataModule1.Query4Result.sql.Add('and ');
  DataModule1.Query4Result.sql.Add(
    '(UIBEVENTTIMESPLIT.TAG = UIB_WORK_DESCRIPTION.TAG) ');
  DataModule1.Query4Result.sql.Add('and ');
  DataModule1.Query4Result.sql.Add(
    '(UIBEVENTTIMESPLIT.MONAT = UIB_WORK_DESCRIPTION.MONAT) ');
  DataModule1.Query4Result.sql.Add('and ');
  DataModule1.Query4Result.sql.Add(
    '(UIBEVENTTIMESPLIT.JAHR = UIB_WORK_DESCRIPTION.JAHR) ');
  DataModule1.Query4Result.sql.Add('and ');
  DataModule1.Query4Result.sql.Add(
    '(UIBEVENTTIMESPLIT.EVENT = UIB_WORK_DESCRIPTION.EVENT) ');
  DataModule1.Query4Result.sql.Add('where ');
  DataModule1.Query4Result.sql.Add('(UIBEVENTTIMESPLIT.EVENT ');
  DataModule1.Query4Result.sql.Add('in (select event from ' +
    referenztabelle + ' where ');
  DataModule1.Query4Result.sql.Add('    (parentevent in (select event from ' +
    referenztabelle + ' where ');
  DataModule1.Query4Result.sql.Add('    (parentevent in (select event from ' +
    referenztabelle + ' where ');
  DataModule1.Query4Result.sql.Add('    (parentevent in (select event from ' +
    referenztabelle + ' where ');
  DataModule1.Query4Result.sql.Add('    (parentevent in (select event from ' +
    referenztabelle + ' where ');
  DataModule1.Query4Result.sql.Add('    (parentevent in (select event from ' +
    referenztabelle + ' where ');
  DataModule1.Query4Result.sql.Add('    (parentevent = ''' + below + '''))))))))))) ');
  DataModule1.Query4Result.sql.Add('    or ');
  DataModule1.Query4Result.sql.Add('    (event in (select event from ' +
    referenztabelle + ' where ');
  DataModule1.Query4Result.sql.Add('    (parentevent in (select event from ' +
    referenztabelle + ' where ');
  DataModule1.Query4Result.sql.Add('    (parentevent in (select event from ' +
    referenztabelle + ' where ');
  DataModule1.Query4Result.sql.Add('    (parentevent in (select event from ' +
    referenztabelle + ' where ');
  DataModule1.Query4Result.sql.Add('    (parentevent in (select event from ' +
    referenztabelle + ' where ');
  DataModule1.Query4Result.sql.Add('    (parentevent = ''' + below + '''))))))))))) ');
  DataModule1.Query4Result.sql.Add('    or ');
  DataModule1.Query4Result.sql.Add('    (event in (select event from ' +
    referenztabelle + ' where ');
  DataModule1.Query4Result.sql.Add('    (parentevent in (select event from ' +
    referenztabelle + ' where ');
  DataModule1.Query4Result.sql.Add('    (parentevent in (select event from ' +
    referenztabelle + ' where ');
  DataModule1.Query4Result.sql.Add('    (parentevent in (select event from ' +
    referenztabelle + ' where ');
  DataModule1.Query4Result.sql.Add('    (parentevent = ''' + below + '''))))))))) ');
  DataModule1.Query4Result.sql.Add('    or ');
  DataModule1.Query4Result.sql.Add('    (event in (select event from ' +
    referenztabelle + ' where ');
  DataModule1.Query4Result.sql.Add('    (parentevent in (select event from ' +
    referenztabelle + ' where ');
  DataModule1.Query4Result.sql.Add('    (parentevent in (select event from ' +
    referenztabelle + ' where ');
  DataModule1.Query4Result.sql.Add('    (parentevent = ''' + below + '''))))))) ');
  DataModule1.Query4Result.sql.Add('    or ');
  DataModule1.Query4Result.sql.Add('    (event in (select event from ' +
    referenztabelle + ' where ');
  DataModule1.Query4Result.sql.Add('    (parentevent in (select event from ' +
    referenztabelle + ' where ');
  DataModule1.Query4Result.sql.Add('    (parentevent = ''' + below + '''))))) ');
  DataModule1.Query4Result.sql.Add('    or ');
  DataModule1.Query4Result.sql.Add('    (event in (select event from ' +
    referenztabelle + ' where ');
  DataModule1.Query4Result.sql.Add('    (parentevent = ''' + below + '''))) ');
  DataModule1.Query4Result.sql.Add('    or ');
  DataModule1.Query4Result.sql.Add('    (event = ''' + below + ''') ');
  DataModule1.Query4Result.sql.Add('    ) ');
  DataModule1.Query4Result.sql.Add('    ) ');
  DataModule1.Query4Result.sql.Add('and (UIBEVENTTIMESPLIT.starttime >= ''' +
    edit1.Text + ''') ');
  DataModule1.Query4Result.sql.Add('and (UIBEVENTTIMESPLIT.starttime < ''' +
    edit2.Text + ''') ');
  DataModule1.Query4Result.sql.Add('and (UIBEVENTTIMESPLIT.userid like ''' +
    searchuser + ''') ');
  DataModule1.Query4Result.sql.Add('GROUP BY ');
  DataModule1.Query4Result.sql.Add(
    'UIBEVENTTIMESPLIT.Jahr,UIBEVENTTIMESPLIT.Monat,UIBEVENTTIMESPLIT.TAG,UIBEVENTTIMESPLIT.EVENT ');
  DataModule1.Query4Result.sql.Add(
    ',UIBEVENTTIMESPLIT.USERID,UIB_WORK_DESCRIPTION.DESCRIPTION ');
  DataModule1.Query4Result.sql.Add(
    'ORDER BY UIBEVENTTIMESPLIT.Jahr,UIBEVENTTIMESPLIT.Monat,UIBEVENTTIMESPLIT.TAG,UIBEVENTTIMESPLIT.EVENT ');

  DataModule1.debugOut(6, DataModule1.Query4Result.SQL.Text);
  DataModule1.Query4Result.ReadOnly := True;
  DataModule1.Query4Result.Open;
  //Fresult.DataSource1.DataSet := DataModule1.Query4Result;
  FResult.Edit1.Text := 'Arbeitsbericht von ' + Edit1.Text +
    'bis (excl.)' + Edit2.Text + ' unterhalb (+incl.) von ' +
    ComboBoxAktevent.Text + ' ' + searchuserstr;
  Fresult.showmodal();
  Fresult.Free;
end;

procedure TFStatistik.BitBtnMonthreportStartStopClick(Sender: TObject);
var
  nextmonth, nextmonthyear: integer;
  startdt, stopdt: TDateTime;
begin
  Fresult := TFResult.Create(self);
  FResult.setResultDataset(DataModule1.Query4Result);
  startdt := encodedate(StrToInt(spinedit2.Text), StrToInt(spinedit1.Text), 1);
  if DataModule1.Query4Result.active then
    DataModule1.Query4Result.Close;
  ///DataModule1.Query4Result.databasename :='uibtime';
  DataModule1.Query4Result.SQL.Clear;
  DataModule1.Query4Result.sql.Add('select');
  DataModule1.Query4Result.sql.Add(
    ' Tag,wochentag,  extract(hour from min(starttime)) as Beginn_h, ');
  DataModule1.Query4Result.sql.Add(
    'extract(minute from min(starttime)) as Beginn_min, ');
  DataModule1.Query4Result.sql.Add('extract(hour from max(stoptime)) as Ende_h, ');
  DataModule1.Query4Result.sql.Add('extract(minute from max(stoptime)) as Ende_min, ');
  DataModule1.Query4Result.sql.Add('sum(stunden) as Arbeitszeit ');
  DataModule1.Query4Result.sql.Add('from uibeventtimesplit where');
  DataModule1.Query4Result.sql.Add('(userid = :uid) and ');
  DataModule1.Query4Result.sql.Add('(starttime >= :start) and ');
  DataModule1.Query4Result.sql.Add('(starttime <= :stop) ');
  DataModule1.Query4Result.sql.Add('and (not (event = ''Pause''))');
  DataModule1.Query4Result.parambyname('uid').AsString := combobox1.Text;
  DataModule1.Query4Result.parambyname('start').AsDateTime := startdt;
  // := '1.'+spinedit1.Text+'.'+spinedit2.Text;
  nextmonth := StrToInt(spinedit1.Text) + 1;
  nextmonthyear := StrToInt(spinedit2.Text);
  if nextmonth > 12 then
  begin
    nextmonth := 1;
    Inc(nextmonthyear);
  end;
  stopdt := encodedate(nextmonthyear, nextmonth, 1);
  DataModule1.Query4Result.parambyname('stop').AsDateTime := stopdt;
  //DataModule1.Query4Result.parambyname('stop').asstring
  // := '1.'+inttostr(nextmonth)+'.'+inttostr(nextmonthyear);
  DataModule1.Query4Result.sql.Add(' group by Tag, wochentag');
  DataModule1.Query4Result.ReadOnly := True;
  DataModule1.Query4Result.Open;
  //Fresult.DataSource1.DataSet := DataModule1.Query4Result;
  FResult.Edit1.Text := 'Arbeitsbericht für Monat ' + spinedit1.Text +
    '.' + spinedit2.Text + ' mit Beginn und Ende sowie Arbeitzeiten ohne Pausen für ' +
    combobox1.Text;

  Fresult.showmodal();
  Fresult.Free;
end;

procedure TFStatistik.LabelMonthURLClick(Sender: TObject);
var
  link: string;
begin
  link := 'https://uibtime.uib.gmbh/uibtime/stdkonto/uibtime_info.php';
  link := link + '?monat=' + spinedit2.Text + '_';
  link := link + format('%.2d', [StrToInt(spinedit1.Text)]);
  OpenURL(link);
end;

procedure TFStatistik.LabelMonthURLMouseEnter(Sender: TObject);
var
  link: string;
begin
  link := 'https://uibtime.uib.gmbh/uibtime/stdkonto/uibtime_info.php';
  link := link + '?monat=' + spinedit2.Text + '_';
  link := link + format('%.2d', [StrToInt(spinedit1.Text)]);
  LabelMonthURL.Hint := link;
  Screen.Cursor := crHandPoint;
end;

procedure TFStatistik.LabelMonthURLMouseLeave(Sender: TObject);
begin
  Screen.Cursor := crDefault;
end;

end.
