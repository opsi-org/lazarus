unit login;

{$MODE Delphi}

interface

uses
  SysUtils, LCLIntf, LCLType, {LMessages, Messages,} Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, StdCtrls, Buttons, ComCtrls, inifiles,
  ///registry,
  lazfileutils,
  fileutil,
  DB,
  runprocess;

type

  { TFlogin }

  TFlogin = class(TForm)
    Label2: TLabel;
    Edit1: TEdit;
    Label3: TLabel;
    BitBtnOk: TBitBtn;
    BitBtnAbort: TBitBtn;
    ProgressBar1: TProgressBar;
    Timer1: TTimer;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label1: TLabel;
    ComboBox1: TComboBox;
    CheckBoxEditOnly: TCheckBox;
    TimerAutologin: TTimer;
    procedure BitBtnOkClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure BitBtnAbortClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBoxEditOnlyClick(Sender: TObject);
    procedure TimerAutologinTimer(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Flogin: TFlogin;
  automatic : boolean;

implementation
uses ontop, uibdata{, debug, loggedin_};

{$R *.lfm}
var
 wait : boolean;
 loggedinServer_ : TStringList;

procedure TFlogin.BitBtnOkClick(Sender: TObject);
var
 found : boolean;
 gaugefak : integer;
 str : string;
begin
 try
   DataModule1.debugOut(5,'Login: '+edit1.Text+'@'+Combobox1.Text);
   Datamodule1.SQLTransaction1.Params.Clear;
   Datamodule1.SQLTransaction1.Params.Add('isc_tpb_read_committed');
   Datamodule1.SQLTransaction1.Params.Add('isc_tpb_rec_version');
   ///datamodule1.setLoggedInServerList(loggedinServer_);
  // loggedinServer_.free;
   //***Datamodule1.IBConnection1.params.add('SERVER NAME='+Combobox1.Text);
   if Datamodule1.IBConnection1.Connected then Datamodule1.IBConnection1.Close;
   Datamodule1.IBConnection1.DatabaseName :=  Combobox1.Text;
   if Datamodule1.IBConnection2.Connected then Datamodule1.IBConnection2.Close;
   Datamodule1.IBConnection2.DatabaseName :=  Combobox1.Text;
   gaugefak := (100 div 13) +1;
   Label4.caption := 'Connecting..';
   application.processmessages;
   ProgressBar1.visible := true;
   ProgressBar1.Position := gaugefak * 1;
   Datamodule1.IBConnection1.open;
   Datamodule1.IBConnection2.open;
   application.processmessages;
   Datamodule1.SQLTransaction1.StartTransaction;
   ProgressBar1.Position := gaugefak * 2;
   application.processmessages;
   Datamodule1.SQuibaktuser.open;
   Datamodule1.SQuibaktuser.first;
   found := false;
   if edit1.text = 'admin' then found := true;
   while not (Datamodule1.SQuibaktuser.eof or found) do
   begin
    str := Datamodule1.SQuibaktuser.fieldbyname('name').asstring;
    if pos(lowercase(edit1.text),lowercase(str)) <> 0 then
     found := true
    else
     Datamodule1.SQuibaktuser.next;
   end;
   if not found then
   begin
    label2.caption := 'Unbekannt !!!';
    wait := true;
    timer1.Interval := 2000;
    timer1.enabled := true;
    while wait do application.processmessages;
    label2.caption := 'Wer bist Du ?';
    DataModule1.IBConnection1.Close;
    ProgressBar1.Position := 0;
    ///DataModule1.loggedinserver.Clear;
    DataModule1.debugOut(5,'Login gescheitert: '+edit1.Text+'@'+Combobox1.Text);
   end
   else
   begin
    if edit1.text = 'admin' then uid := 'admin'
    else uid := Datamodule1.SQuibaktuser.fieldbyname('userid').asstring;
    user_h_per_day := Datamodule1.SQuibaktuser.fieldbyname('h_per_day').AsFloat;
    DataModule1.debugOut(8,'Login: uid = '+uid);
    ProgressBar1.Position := gaugefak * 3;
    //with Datamodule1.SQuibevent.Params.CreateParam(ftString, 'uid', ptInput) do
    //   AsString := uid;
    //Datamodule1.SQuibevent.ParamByName('uid').AsString := uid;
    Datamodule1.SQuibevent.open;
    DataModule1.debugOut(8,'Login: opend 3');
    ProgressBar1.Position := gaugefak * 4;
    application.processmessages;
    Datamodule1.SQuibaktevent.open;
    DataModule1.debugOut(8,'Login: opend 4');
    ProgressBar1.Position := gaugefak * 5;
    application.processmessages;
    Datamodule1.SQuiballevent.open;
    DataModule1.debugOut(8,'Login: opend 5');
    ProgressBar1.Position := gaugefak * 6;
    application.processmessages;
    Datamodule1.SQuibuserEvent.open;
    DataModule1.debugOut(8,'Login: opend 6');
    ProgressBar1.Position := gaugefak * 7;
    application.processmessages;
    Datamodule1.SQuibtimeout.open;
    DataModule1.debugOut(8,'Login: opend 7');
    ProgressBar1.Position := gaugefak * 8;
    ///application.processmessages;
    ///Datamodule1.SQuibcalls.open;
    DataModule1.debugOut(8,'Login: opend 8');
    ProgressBar1.Position := gaugefak * 9;
    application.processmessages;
    Datamodule1.SQuibsoll.open;
    DataModule1.debugOut(8,'Login: opend 9');
    ProgressBar1.Position := gaugefak * 10;
    application.processmessages;
    Datamodule1.SQuibdefproj.open;
    DataModule1.debugOut(8,'Login: opend 10');
    ProgressBar1.Position := gaugefak * 11;
    application.processmessages;
    Datamodule1.SQuibloggedin.open;
    DataModule1.debugOut(8,'Login: opend 11');
    ProgressBar1.Position := gaugefak * 12;
    application.processmessages;
    DataModule1.SQQueryAktEvents.Active := true;
    DataModule1.debugOut(8,'Login: opend 12');
    ProgressBar1.Position := gaugefak * 13;
    application.processmessages;
    DataModule1.SQholydays.Active := true;
    DataModule1.debugOut(8,'Login: opend 13');
    Application.CreateForm(TFOnTop, FOnTop);
    DataModule1.debugOut(8,'Login: FonTop created');
    application.processmessages;
    Sleep(500);
    application.processmessages;
    if not (edit1.text = 'admin')  then
    begin
     //***Datamodule1.SQuibevent.setrange([uid],[uid]);
     //Datamodule1.SQuibevent.setrange([uid],[uid]);
     Datamodule1.SQuibevent.Filter := 'userid = '+QuotedStr(uid);
     DataModule1.debugOut(8,'Login: filter setto '+Datamodule1.SQuibevent.Filter);
     Datamodule1.SQuibevent.Filtered := true;
     Datamodule1.SQuibuserEvent.Filter := 'userid = '+QuotedStr(uid);
     Datamodule1.SQuibuserEvent.Filtered := true;
     ///Datamodule1.SQuibcalls.Filter := 'userid = '+QuotedStr(uid);
     ///Datamodule1.SQuibcalls.Filtered := true;
     Datamodule1.SQuibsoll.Filter := 'userid = '+QuotedStr(uid);
     Datamodule1.SQuibsoll.Filtered := true;
     Datamodule1.SQuibtimeout.Filter := 'userid = '+QuotedStr(uid);
     Datamodule1.SQuibtimeout.Filtered := true;
     Datamodule1.SQuibdefproj.Filter := 'userid = '+QuotedStr(uid);
     Datamodule1.SQuibdefproj.Filtered := true;
     Datamodule1.setloggedin(true);
     //Datamodule1.SQuibloggedin.Filter := 'userid = '+QuotedStr(uid);
     //Datamodule1.SQuibloggedin.Filtered := true;
     //***Datamodule1.SQuibaktuserEvent.setrange([uid],[uid]);
     //***Datamodule1.SQuibcalls.setrange([uid],[uid]);
     //***Datamodule1.SQuibsoll.setrange([uid],[uid]);
     //***Datamodule1.SQuibtimeout.setrange([uid],[uid]);
     //***Datamodule1.SQuibdefproj.setrange([uid],[uid]);
     //***Datamodule1.SQuibloggedin.setrange([uid],[uid]);
     DataModule1.debugOut(8,'Login: filter set');
     DataModule1.debugOut(5, 'SQuibloggedin.active: ' + BoolToStr(Datamodule1.SQuibloggedin.Active,true) );
    end;
    DataModule1.debugOut(5,'Login erfolgreich: '+edit1.Text+'@'+Combobox1.Text);
    fontop.labeluid.caption := uid;
    application.processmessages;
    Sleep(500);
    application.processmessages;
    hide;
    ///fontop.TimerCallCount.Enabled := true;
    fontop.show;
    application.processmessages;
    if not setwindowtoalldesktops('fontop') then
        datamodule1.debugOut(2,'login', 'failed fontop to all desktops');
    ///FLoggedin.Show;
   end;

  except
    datamodule1.debugOut(3,'', 'exception in setDefaultEvent');
    raise;
  end;
end;

procedure TFlogin.Timer1Timer(Sender: TObject);
begin
 wait := false;
 timer1.enabled := false;
end;

procedure TFlogin.BitBtnAbortClick(Sender: TObject);
begin
 automatic := false;
 DataModule1.debugOut(5,'In Login durch Anwender abgebrochen: Ende');
 DataModule1.TerminateApplication;
 //application.terminate;
end;

procedure TFlogin.FormCreate(Sender: TObject);
var
 myini: TIniFile;
 serverstr : string;
 mypath : string;
 i : integer;
 lserver : String;
 //PNAME : array[0..30] of Char;

begin
 {$IFDEF Linux}
 mypath := '/usr/share/uibtime/';
 if not FileExists(mypath+'uibtime.ini') then
 // development:
 mypath := ExtractFilePath(paramstr(0));
 {$ELSE}
 mypath := ExtractFilePath(paramstr(0));
 {$ENDIF Linux}
 myini := TIniFile.Create(mypath+'uibtime.ini');
 DataModule1.debugOut(5,'login-formcreate', 'Will use uibtime.ini file from: ' + mypath);
 serverstr := myini.ReadString('database', 'SERVER NAME', 'ERROR');
 loggedinServer_ := TStringList.Create;
 loggedinServer_.clear;
 for i:=1 to 10 do
 begin
  lserver := 'server'+inttostr(i);
  lserver := myini.ReadString('loggedin', lserver, 'NONE');
  if not (lserver = 'NONE') then
   loggedinServer_.Add(lserver);
 end;
 myini.Free;
 //datamodule1.setLoggedInServerList(loggedinServer_);
 //loggedinServer_.free;
 if serverstr = 'ERROR' then
 begin
  messagebox(0,pchar('Servername in uibtime.ini nicht gefunden'),
               pchar('uibtime: Fehler'),mb_ok);
 end
 else
  Combobox1.Text := serverstr;
 label6.caption := Datamodule1.getVersion();
 label7.caption := Datamodule1.getVerDatum();
 automatic := true;
 //CheckBoxEditOnly.Checked := false;
 DataModule1.seteditonly(false);
 Edit1.text :=  GetEnvironmentVariable('UNAME');
end;

procedure TFlogin.CheckBoxEditOnlyClick(Sender: TObject);
begin
 if CheckBoxEditOnly.Checked then
  DataModule1.seteditonly(true)
 else
  DataModule1.seteditonly(false);
end;

procedure TFlogin.TimerAutologinTimer(Sender: TObject);
begin
 TimerAutologin.Enabled := false;
 BitBtnOk.Click;  // login starten
end;

procedure TFlogin.Edit1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 // Autologin verhindern - jemand tippt was
 TimerAutologin.Enabled := false;
end;

procedure TFlogin.FormShow(Sender: TObject);
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
  logfeilname := ExpandFileName(logdir + '\uibtime.conf');
  myini := TIniFile.Create(logfeilname);
  DataModule1.debugOut(6,'login-formshow','Will use conf file: '+logfeilname);
  TimerAutologin.Enabled := myini.ReadBool('general','autologin',false);
  myini.UpdateFile;
  myini.Destroy;
end;

procedure TFlogin.FormDestroy(Sender: TObject);
begin
 //DataModule1.debugOut(5,'Ende: formdestroy von flogin');
 loggedinServer_.Free;
end;

initialization
  	{ initialization-Abschnitt }


finalization
  	{ finalization-Abschnitt }
begin
end;


end.
