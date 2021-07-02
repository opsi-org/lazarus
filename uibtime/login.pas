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
  linhandlewin,
  uibdatetime,
  osurlparser;

type

  { TFlogin }

  TFlogin = class(TForm)
    CheckBoxRememberLogin: TCheckBox;
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
    procedure Edit1KeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Flogin: TFlogin;
  automatic: boolean;

implementation

uses ontop, uibdata{, debug, loggedin_};

{$R *.lfm}
var
  wait: boolean;
  loggedinServer_: TStringList;

procedure TFlogin.BitBtnOkClick(Sender: TObject);
var
  found: boolean;
  gaugefak: integer;
  str: string;
  logdir, logfeilname: string;
  myini: TIniFile;
  urllist : TStringlist;
  i : integer;

begin
  try
    DataModule1.debugOut(5, 'login-bitbtnokclick', 'Login: ' + edit1.Text +
      '@' + Combobox1.Text);
    urllist := parseUrl('db://'+edit1.Text +'@' +Combobox1.Text);
    for i := 0 to urllist.Count -1 do
       DataModule1.debugOut(5, 'login-bitbtnOKclick','url: '+urllist.Strings[i]);
    myDbServer:= urllist.Values['Host'] ;
    urllist.Free;
    Datamodule1.SQLTransaction1.Params.Clear;

    Datamodule1.SQLTransaction1.Params.Add('isc_tpb_version3');
    Datamodule1.SQLTransaction1.Params.Add('isc_tpb_write');
    Datamodule1.SQLTransaction1.Params.Add('isc_tpb_read_committed');
    Datamodule1.SQLTransaction1.Params.Add('isc_tpb_nowait');
    Datamodule1.SQLTransaction1.Params.Add('isc_tpb_rec_version');


    ///datamodule1.setLoggedInServerList(loggedinServer_);
    // loggedinServer_.free;
    //***Datamodule1.IBConnection1.params.add('SERVER NAME='+Combobox1.Text);
    if Datamodule1.IBConnection1.Connected then
      Datamodule1.IBConnection1.Close;
    Datamodule1.IBConnection1.DatabaseName := Combobox1.Text;
    if Datamodule1.IBConnection2.Connected then
      Datamodule1.IBConnection2.Close;
    Datamodule1.IBConnection2.DatabaseName := Combobox1.Text;
    gaugefak := (100 div 19) + 1;
    Label4.Caption := 'Connecting..';
    application.ProcessMessages;
    ProgressBar1.Visible := True;
    ProgressBar1.Position := gaugefak * 1;
    Datamodule1.IBConnection1.Open;
    Datamodule1.IBConnection2.Open;
    DataModule1.debugOut(6, 'login-bitbtnokclick', 'Connected ');
    // ping does not work (why ever)
    //DataModule1.TimerCheckNet.Enabled := True;
    application.ProcessMessages;
    Datamodule1.SQLTransaction1.StartTransaction;
    ProgressBar1.Position := gaugefak * 2;
    application.ProcessMessages;
    Datamodule1.SQuibaktuser.Open;
    Datamodule1.SQuibaktuser.First;
    found := False;
    if edit1.Text = 'admin' then
      found := True;
    while not (Datamodule1.SQuibaktuser.EOF or found) do
    begin
      str := Datamodule1.SQuibaktuser.FieldByName('name').AsString;
      if pos(lowercase(edit1.Text), lowercase(str)) <> 0 then
        found := True
      else
        Datamodule1.SQuibaktuser.Next;
    end;
    if not found then
    begin
      label2.Caption := 'Unbekannt !!!';
      wait := True;
      timer1.Interval := 2000;
      timer1.Enabled := True;
      while wait do
        application.ProcessMessages;
      label2.Caption := 'Wer bist Du ?';
      DataModule1.IBConnection1.Close;
      ProgressBar1.Position := 0;
      ///DataModule1.loggedinserver.Clear;
      DataModule1.debugOut(5, 'login-bitbtnokclick', 'Login gescheitert: ' +
        edit1.Text + '@' + Combobox1.Text);
    end
    else
    begin
      if edit1.Text = 'admin' then
        uid := 'admin'
      else
        uid := Datamodule1.SQuibaktuser.FieldByName('userid').AsString;
      DataModule1.debugOut(6, 'login-bitbtnokclick', 'Connect: User is valid: '+ uid);
      ProgressBar1.Position := gaugefak * 3;
      user_h_per_day := Datamodule1.SQuibaktuser.FieldByName('h_per_day').AsFloat;
      ProgressBar1.Position := gaugefak * 4;
      if Datamodule1.SQuibaktuser.FieldByName('mo_is_work').AsBoolean then
        include(user_work_days, dayOfWeekGerStrToLazDayofWeekbyte('mo'));
      ProgressBar1.Position := gaugefak * 5;
      if Datamodule1.SQuibaktuser.FieldByName('mo_is_work').AsBoolean then
        include(user_work_days, dayOfWeekGerStrToLazDayofWeekbyte('di'));
      ProgressBar1.Position := gaugefak * 6;
      if Datamodule1.SQuibaktuser.FieldByName('mo_is_work').AsBoolean then
        include(user_work_days, dayOfWeekGerStrToLazDayofWeekbyte('mi'));
      ProgressBar1.Position := gaugefak * 7;
      if Datamodule1.SQuibaktuser.FieldByName('mo_is_work').AsBoolean then
        include(user_work_days, dayOfWeekGerStrToLazDayofWeekbyte('do'));
      ProgressBar1.Position := gaugefak * 8;
      if Datamodule1.SQuibaktuser.FieldByName('mo_is_work').AsBoolean then
        include(user_work_days, dayOfWeekGerStrToLazDayofWeekbyte('fr'));
      DataModule1.debugOut(6, 'login-bitbtnokclick', 'Workdays done ');
      ProgressBar1.Position := gaugefak * 9;
      //with Datamodule1.SQuibevent.Params.CreateParam(ftString, 'uid', ptInput) do
      //   AsString := uid;
      //Datamodule1.SQuibevent.ParamByName('uid').AsString := uid;
      Datamodule1.SQuibevent.Open;
      DataModule1.debugOut(7, 'login-bitbtnokclick', 'Login: opend 3');
      ProgressBar1.Position := gaugefak * 10;
      application.ProcessMessages;
      Datamodule1.SQuibaktevent.Open;
      DataModule1.debugOut(7, 'login-bitbtnokclick', 'Login: opend 4');
      ProgressBar1.Position := gaugefak * 11;
      application.ProcessMessages;
      Datamodule1.SQuiballevent.Open;
      DataModule1.debugOut(7, 'login-bitbtnokclick', 'Login: opend 5');
      ProgressBar1.Position := gaugefak * 12;
      application.ProcessMessages;
      Datamodule1.SQuibuserEvent.Open;
      DataModule1.debugOut(7, 'login-bitbtnokclick', 'Login: opend 6');
      ProgressBar1.Position := gaugefak * 13;
      application.ProcessMessages;
      Datamodule1.SQuibtimeout.Open;
      DataModule1.debugOut(7, 'login-bitbtnokclick', 'Login: opend 7');
      ProgressBar1.Position := gaugefak * 14;
      ///application.processmessages;
      ///Datamodule1.SQuibcalls.open;
      DataModule1.debugOut(7, 'login-bitbtnokclick', 'Login: opend 8');
      ProgressBar1.Position := gaugefak * 15;
      application.ProcessMessages;
      Datamodule1.SQuibsoll.Open;
      DataModule1.debugOut(7, 'login-bitbtnokclick', 'Login: opend 9');
      ProgressBar1.Position := gaugefak * 16;
      application.ProcessMessages;
      Datamodule1.SQuibdefproj.Open;
      DataModule1.debugOut(7, 'login-bitbtnokclick', 'Login: opend 10');
      ProgressBar1.Position := gaugefak * 17;
      application.ProcessMessages;
      Datamodule1.SQuibloggedin.Open;
      DataModule1.debugOut(7, 'login-bitbtnokclick', 'Login: opend 11');
      ProgressBar1.Position := gaugefak * 18;
      application.ProcessMessages;
      DataModule1.SQQueryAktEvents.Open;
      DataModule1.debugOut(7, 'login-bitbtnokclick', 'Login: opend 12');
      ProgressBar1.Position := gaugefak * 19;
      application.ProcessMessages;
      DataModule1.SQholydays.Open;
      DataModule1.debugOut(7, 'login-bitbtnokclick', 'Login: opend 13');
      Application.CreateForm(TFOnTop, FOnTop);
      DataModule1.debugOut(7, 'login-bitbtnokclick', 'Login: FonTop created');
      application.ProcessMessages;
      Sleep(500);
      application.ProcessMessages;
      if not (edit1.Text = 'admin') then
      begin
        //***Datamodule1.SQuibevent.setrange([uid],[uid]);
        //Datamodule1.SQuibevent.setrange([uid],[uid]);
        Datamodule1.SQuibevent.Filter := 'userid = ' + QuotedStr(uid);
        DataModule1.debugOut(8, 'login-bitbtnokclick', 'Login: filter set to ' +
          Datamodule1.SQuibevent.Filter);
        Datamodule1.SQuibevent.Filtered := True;
        Datamodule1.SQuibuserEvent.Filter := 'userid = ' + QuotedStr(uid);
        Datamodule1.SQuibuserEvent.Filtered := True;
        ///Datamodule1.SQuibcalls.Filter := 'userid = '+QuotedStr(uid);
        ///Datamodule1.SQuibcalls.Filtered := true;
        Datamodule1.SQuibsoll.Filter := 'userid = ' + QuotedStr(uid);
        Datamodule1.SQuibsoll.Filtered := True;
        Datamodule1.SQuibtimeout.Filter := 'userid = ' + QuotedStr(uid);
        Datamodule1.SQuibtimeout.Filtered := True;
        Datamodule1.SQuibdefproj.Filter := 'userid = ' + QuotedStr(uid);
        Datamodule1.SQuibdefproj.Filtered := True;
        Datamodule1.setloggedin(True);
        //Datamodule1.SQuibloggedin.Filter := 'userid = '+QuotedStr(uid);
        //Datamodule1.SQuibloggedin.Filtered := true;
        //***Datamodule1.SQuibaktuserEvent.setrange([uid],[uid]);
        //***Datamodule1.SQuibcalls.setrange([uid],[uid]);
        //***Datamodule1.SQuibsoll.setrange([uid],[uid]);
        //***Datamodule1.SQuibtimeout.setrange([uid],[uid]);
        //***Datamodule1.SQuibdefproj.setrange([uid],[uid]);
        //***Datamodule1.SQuibloggedin.setrange([uid],[uid]);
        DataModule1.debugOut(8, 'login-bitbtnokclick', 'Login: filter set');
        DataModule1.debugOut(5, 'login-bitbtnokclick', 'SQuibloggedin.active: ' +
          BoolToStr(Datamodule1.SQuibloggedin.Active, True));
      end;
      DataModule1.debugOut(5, 'login-bitbtnokclick', 'Login erfolgreich: ' +
        edit1.Text + '@' + Combobox1.Text);
      fontop.labeluid.Caption := uid;
      application.ProcessMessages;
      Sleep(500);
      application.ProcessMessages;
      hide;
      fontop.Show;
      application.ProcessMessages;
      (*
      if not setwindowtoalldesktops(fontop.Caption) then
        datamodule1.debugOut(2, 'login-bitbtnokclick', 'failed fontop to all desktops');
        *)
      ///FLoggedin.Show;
      // write login to uibtime.conf
      logdir := SysUtils.GetAppConfigDir(False);
      if logdir = '' then
      begin
        logdir := SysUtils.GetUserDir;
        logdir := logdir + '\uibtime';
      end;
      logdir := ExpandFileName(logdir);
      ForceDirectories(logdir);
      logfeilname := ExpandFileName(logdir + 'uibtime.conf');
      myini := TIniFile.Create(logfeilname);
      DataModule1.debugOut(6, 'login-bitbtnokclick', 'Will use conf file: ' +
        logfeilname);
      if myini = nil then
      begin
        datamodule1.debugOut(2, 'login-bitbtnokclick',
          'myini = nil: coud not open :' + logfeilname);
        ShowMessage('Fehler in Konfigurations Datei. Bitte Log sichern. Programm wird beendet');
        Application.Terminate;
      end;
      myini.WriteBool('general', 'rememberlogin', CheckBoxRememberLogin.Checked);
      if CheckBoxRememberLogin.Checked then
        myini.WriteString('general', 'loginname', edit1.Text)
      else
        myini.WriteString('general', 'loginname', '');
      myini.UpdateFile;
      myini.Free;
      hide;
      Visible:=false;
      Application.ProcessMessages;
    end;
  except
    datamodule1.debugOut(2, 'login-bitbtnokclick', 'exception in login: BtnOK');
    raise;
  end;
end;

procedure TFlogin.Timer1Timer(Sender: TObject);
begin
  wait := False;
  timer1.Enabled := False;
end;

procedure TFlogin.BitBtnAbortClick(Sender: TObject);
begin
  automatic := False;
  DataModule1.debugOut(5, 'In Login durch Anwender abgebrochen: Ende');
  DataModule1.TerminateApplication;
  //application.terminate;
end;

procedure TFlogin.FormCreate(Sender: TObject);
var
  myini: TIniFile;
  serverstr: string;
  mypath: string;
  i: integer;
  lserver: string;
  uname: string;
  //PNAME : array[0..30] of Char;

begin
  mypath := ExtractFilePath(ParamStr(0));
   {$IFDEF Linux}
  mypath := '/usr/share/uibtime/';
  if not FileExists(mypath + 'uibtime.ini') then
    // development:
    mypath := ExtractFilePath(ParamStr(0));

 {$ENDIF Linux}
 {$IFDEF Darwin}
  mypath := ExtractFilePath(ParamStr(0));
  mypath := mypath +'../Resources/';
  if not FileExists(mypath + 'uibtime.ini') then
  begin
    // development:
    mypath := ExtractFilePath(ParamStr(0));
     mypath := mypath +'/../../../';
  end;
 {$ENDIF Darwin}
  myini := TIniFile.Create(mypath + 'uibtime.ini');
  DataModule1.debugOut(5, 'login-formcreate',
    'Will use uibtime.ini file from: ' + mypath);
  serverstr := myini.ReadString('database', 'SERVER NAME', 'ERROR');
  loggedinServer_ := TStringList.Create;
  loggedinServer_.Clear;
  for i := 1 to 10 do
  begin
    lserver := 'server' + IntToStr(i);
    lserver := myini.ReadString('loggedin', lserver, 'NONE');
    if not (lserver = 'NONE') then
      loggedinServer_.Add(lserver);
  end;
  myini.Free;
  //datamodule1.setLoggedInServerList(loggedinServer_);
  //loggedinServer_.free;
  if serverstr = 'ERROR' then
  begin
    messagebox(0, PChar('Servername in uibtime.ini nicht gefunden'),
      PChar('uibtime: Fehler'), mb_ok);
  end
  else
    Combobox1.Text := serverstr;
  label6.Caption := Datamodule1.getVersion();
  label7.Caption := Datamodule1.getVerDatum();
  automatic := True;
  //CheckBoxEditOnly.Checked := false;
  DataModule1.seteditonly(False);
  Uname := GetEnvironmentVariable('UNAME');
  if uname = '' then
    GetEnvironmentVariable('UIBTIMEUSER');
  Edit1.Text := uname;
  DataModule1.SetFontName(TControl(sender),myFont);
end;

procedure TFlogin.CheckBoxEditOnlyClick(Sender: TObject);
begin
  if CheckBoxEditOnly.Checked then
    DataModule1.seteditonly(True)
  else
    DataModule1.seteditonly(False);
end;

procedure TFlogin.TimerAutologinTimer(Sender: TObject);
begin
  TimerAutologin.Enabled := False;
  BitBtnOk.Click;  // login starten
end;

procedure TFlogin.Edit1KeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  // Autologin verhindern - jemand tippt was
  TimerAutologin.Enabled := False;
end;

procedure TFlogin.FormShow(Sender: TObject);
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
  logfeilname := ExpandFileName(logdir + 'uibtime.conf');
  myini := TIniFile.Create(logfeilname);
  DataModule1.debugOut(6, 'login-formshow', 'Will use conf file: ' + logfeilname);
  if myini = nil then
  begin
    //Fdebug.Memo1.Append('myini = nil: coud not open :' + logfeilname);
    datamodule1.debugOut(2, 'Flogin.FormShow',
      'myini = nil: coud not open :' + logfeilname);
    ShowMessage('Fehler in Konfigurations Datei. Bitte Log sichern. Programm wird beendet');
    Application.Terminate;
  end;
  TimerAutologin.Enabled := myini.ReadBool('general', 'autologin', False);
  CheckBoxRememberLogin.Checked := myini.ReadBool('general', 'rememberlogin', False);
  edit1.Text := myini.ReadString('general', 'loginname', '');
  myini.UpdateFile;
  myini.Free;
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
