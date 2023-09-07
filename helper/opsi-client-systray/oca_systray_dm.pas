unit oca_systray_dm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, Menus, ExtCtrls,
  Forms,
  Dialogs,
  oslog,
  oswebservice,
  fileinfo,
  superobject,
  lcltranslator,
  //osparserhelper,
  {$IFDEF WINDOWS}
  Windows,
  jwawinbase,
  winpeimagereader,
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
  elfreader,
  OSProcessux,
  libnotify,
  {$ENDIF LINUX}
  {$IFDEF Darwin}
  elfreader,
  OSProcessux,
  MacOSAll,
  osfuncmac,
  {$ENDIF}
  Graphics,
  //PopupNotifier,
  osprocesses,
  uniqueinstanceraw;

type

  { TDataModule1 }

  TDataModule1 = class(TDataModule)
    MI_exit: TMenuItem;
    MI_startevent: TMenuItem;
    MI_pull_for_action_request: TMenuItem;
    PopupMenu1: TPopupMenu;
    //PopupNotifier1: TPopupNotifier;
    Timer1: TTimer;
    TrayIcon1: TTrayIcon;
    //uniqueinstance1 : Tuniqueinstance;
    procedure DataModuleCreate(Sender: TObject);
    procedure MI_exitClick(Sender: TObject);
    procedure MI_pull_for_action_requestClick(Sender: TObject);
    procedure MI_starteventClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure WriteHelp;
    procedure startNotify(notifyempty: boolean);
  private
    { private declarations }
    pathMedia: string;
  public
    { public declarations }
  end;

var
  DataModule1: TDataModule1;
  checkIntervall: integer;
  myservice_url, myclientid, myVersion: string;
  myDepotId, myNotifyFormat: string;

resourcestring
  rsActionsWaiting = 'opsi: Pending installations:';
  rsNoActionsWaiting = 'opsi: No pending installations';
  rsNone = 'None';

implementation

{$R *.lfm}

{$IFDEF WINDOWS}
{:Returns user name of the current thread.
  @author  Miha-R, Lee_Nover
  @since   2002-11-25
}
function GetUserName_: string;
var
  buffer: PChar;
  bufferSize: DWORD;
begin
  bufferSize := 256; //UNLEN from lmcons.h
  buffer := AllocMem(bufferSize * SizeOf(char));
  try
    GetUserName(buffer, bufferSize);
    Result := string(buffer);
  finally
    FreeMem(buffer, bufferSize);
  end;
end; { DSiGetUserName }

function GetSystemDefaultLocale(const typeOfValue: DWord): string;
  // possible values: cf. "Locale Types" in windows.pas
var
  buffer: PChar;
  size: word = 0;
  usedsize: word = 0;

begin
  Result := '';
  size := 101;
  Buffer := StrAlloc(101);
  try
    usedsize := GetLocaleInfo(LOCALE_SYSTEM_DEFAULT, typeOfValue, buffer, size);
    if usedsize <> 0 then
      Result := StrPas(Buffer);
  finally
    StrDispose(Buffer);
  end;
end;

{$ENDIF WINDOWS}


function MyOpsiMethodCall(const method: string; parameters: array of string): string;
var
  omc: TOpsiMethodCall;
  errorOccured: boolean;
  resultstring: string;
begin
  Result := '';
  try
    omc := TOpsiMethodCall.Create(method, parameters);
    resultstring := opsidata.checkAndRetrieve(omc, errorOccured);
    Result := resultstring;
  except
    LogDatei.log('Exception calling method: ' + method, LLerror);
  end;
end;

function getActionrequests: TStringList;
var
  resultstring: string;
  new_obj, detail_obj, pod_obj, prod_obj: ISuperObject;
  i: integer;
  //execp : double;
  productid, prodver, packver, prodname, request: string;
begin
  try
    Result := TStringList.Create;
    // det poc for all action requests
    resultstring := MyOpsiMethodCall('productOnClient_getObjects',
      ['[]', '{"clientId":"' + myclientid +
      '","actionRequest":["setup","uninstall", "update"]}']);
    LogDatei.log('resultstring: ' + resultstring, LLDebug2);
    new_obj := SO(resultstring).O['result'];

    for i := 0 to new_obj.AsArray.Length - 1 do
    begin
      try
        detail_obj := new_obj.AsArray.O[i];
        productid := detail_obj.S['productId'];
        request := detail_obj.S['actionRequest'];
        if myNotifyFormat <> 'productid : request' then
        begin
          // get product and package version from productOnDepot
          resultstring := MyOpsiMethodCall('productOnDepot_getObjects',
            ['[]', '{"depotId":"' + mydepotid + '","productId":"' + productid + '"}']);
          LogDatei.log('resultstring: ' + resultstring, LLDebug2);
          pod_obj := SO(resultstring).O['result'];

          prodver := pod_obj.AsArray.O[0].S['productVersion'];
          packver := pod_obj.AsArray.O[0].S['packageVersion'];
          // get productName from product
          resultstring := MyOpsiMethodCall('product_getObjects',
            ['[]', '{"id":"' + productid + '","productVersion":"' +
            prodver + '","packageVersion":"' + packver + '"}']);
          LogDatei.log('resultstring: ' + resultstring, LLDebug2);
          prod_obj := SO(resultstring).O['result'];
          prodname := prod_obj.AsArray.O[0].S['name'];
          LogDatei.log('productid: ' + productid + ' prodver: ' +
            prodver + ' packver: ' + packver + ' prodname: ' + prodname +
            ' request: ' + request, LLInfo);
          //Result.Add(detail_obj.S['productId'] + ' : ' + detail_obj.S['actionRequest"']);
        end
        else
          LogDatei.log('productid: ' + productid + ' request: ' + request, LLInfo);
        if myNotifyFormat = 'productname productversion : request' then
          Result.Add(prodname + ' ' + prodver + ' : ' + request);
        if myNotifyFormat = 'productname : request' then
          Result.Add(prodname + ' : ' + request);
        if myNotifyFormat = 'productid : request' then
          Result.Add(productid + ' : ' + request);
      except
        on e: Exception do
        begin
          logdatei.log('Exception in oca_systray_dm: getActionrequests:inner loop.',
            LLError);
          logdatei.log('Exception: ' + E.message, LLError);
          logdatei.log_exception(e, LLError);
        end;
      end;
    end;
  except
    on e: Exception do
    begin
      logdatei.log('Exception in oca_systray_dm: getActionrequests:inner loop.',
        LLError);
      logdatei.log('Exception: ' + E.message, LLError);
    end;
  end;
end;



function initConnection(const seconds: integer): boolean;
var
  networkup, timeout: boolean;
  myseconds: integer;
  resultstring: string;
  new_obj: ISuperObject;
begin
  //FopsiClientKiosk.Cursor := crHourGlass;
  Result := False;
  networkup := False;
  timeout := False;
  myseconds := seconds;
  LogDatei.log('service_url=' + myservice_url, LLDebug2);
  LogDatei.log('myclientid=' + myclientid, LLDebug2);
  opsidata := TOpsi4Data.Create;
  LogDatei.log('opsidata created', LLDebug2);
  opsidata.setActualClient(myclientid);
  opsidata.initOpsiConf(myservice_url, myclientid,
    '', '', '', '', 'opsi-client-systray-' + myVersion);
  {opciclientd 4.2 recieve identity}
  //opsidata.CommunicationMode:=2;
  LogDatei.log('opsidata initialized', LLDebug2);
  repeat
    try
      if myseconds > 0 then
      begin
        resultstring := MyOpsiMethodCall('getDepotId', [myclientid]);
        new_obj := SO(resultstring).O['result'];
        myDepotId := new_obj.AsString;
        networkup := True;
      end
      else
        timeout := True;
    except
      LogDatei.log('opsidata not connected - retry', LLInfo);
      myseconds := myseconds - 1;
      Sleep(1000);
    end;
  until networkup or timeout;
  if networkup then
  begin
    LogDatei.log('opsidata connected', LLInfo);
    Result := True;
    //Fopsiclientkiosk.StatusBar1.Panels[0].Text :=
    //  'Connected to ' + myservice_url + ' as ' + myclientid;
  end
  else
  begin
    LogDatei.log('init connection failed (timeout after ' + IntToStr(seconds) +
      ' seconds/retries.', LLError);
    //Fopsiclientkiosk.StatusBar1.Panels[0].Text := 'Connection failed';
  end;
  //FopsiClientKiosk.Cursor := crArrow;
end;




procedure firePushInstallation;
var
  resultstring, str: string;
begin
  FreeAndNil(opsidata);
  initConnection(30);
  resultstring := MyOpsiMethodCall('processActionRequests', []);
end;


{ TDataModule1 }
procedure TDataModule1.WriteHelp;
var
  filename: string;
  msg: string;
begin
  { add your help code here }
  filename := ExtractFileName(Application.ExeName);
  msg := 'This is ' + filename + ' version: ' + myVersion +
    ' (c) uib gmbh, AGPLv3' + LineEnding;
  msg := msg + 'Usage: ' + filename + ' Options' + LineEnding;
  msg := msg + 'Options:' + LineEnding;
  msg := msg + ' --help -> write this help and exit' + LineEnding;
  msg := msg + ' -h -> write this help and exit' + LineEnding;
  msg := msg +
    ' --checkintervall=<interval minutes> -> minutes between check for action request; required'
    + LineEnding;
  msg := msg + ' --fqdn=<fqdn of the client> -> ; required' + LineEnding;
  msg := msg + ' --lang=<lang of the client>' + LineEnding;
  msg := msg + ' --notifyformat=<notifyformat>' + LineEnding;
  msg := msg + ' --service_url_port=<service_url_port> (default=4441' + LineEnding;
  ShowMessage(msg);
end;



procedure TDataModule1.DataModuleCreate(Sender: TObject);

var
  ErrorMsg: string;
  optionlist: TStringList;
  FileVerInfo: TFileVersionInfo;
  preloglist: TStringList;
  i: integer;
  lfilename: string;
  logAndTerminate: boolean = False;
  service_url_port: string;
  mylang: string;
  tmpimage: TPicture;
  icofilename: string;
  report : string;
begin
  if InstanceRunning then
    Application.Terminate;
  checkIntervall := 0;
  myNotifyFormat := 'productid : request';
  myservice_url := 'https://localhost:4441/kiosk';
  service_url_port := '4441';
  preloglist := TStringList.Create;
  preloglist.Add('PreLog for: ' + Application.exename + ' opend at : ' +
    DateTimeToStr(now));
  FileVerInfo := TFileVersionInfo.Create(nil);
  try
    FileVerInfo.FileName := ParamStr(0);
    FileVerInfo.ReadFileInfo;
    myVersion := FileVerInfo.VersionStrings.Values['FileVersion'];
  finally
    FileVerInfo.Free;
  end;
  preloglist.Add('Application version: ' + myVersion);


  // quick check parameters
  optionlist := TStringList.Create;
  optionlist.Add('help');
  optionlist.Add('lang:');
  optionlist.Add('checkintervall:');
  optionlist.Add('fqdn::');
  optionlist.Add('notifyformat:');
  optionlist.Add('service_url_port:');
  //optionlist.Add('idevent:');
  ErrorMsg := Application.CheckOptions('hc:f:', optionlist);
  if ErrorMsg <> '' then
  begin
    preloglist.Add(ErrorMsg);
    //logdatei.Close;
    logAndTerminate := True;
    Application.ShowException(Exception.Create(ErrorMsg));
    //Application.Terminate;
    //Exit;
  end;

  // parse parameters
  if Application.HasOption('h', 'help') then
  begin
    preloglist.Add('Found Parameter help: show and exit');
    WriteHelp;
    logAndTerminate := True;
    //Application.Terminate;
    //Exit;
  end;

  if Application.HasOption('c', 'checkintervall') then
  begin
    preloglist.Add('Found Parameter checkintervall');
    checkIntervall := StrToInt(Application.GetOptionValue('c', 'checkintervall'));
    preloglist.Add('Found Parameter checkintervall: ' + IntToStr(checkIntervall));
  end;

  if Application.HasOption('f', 'fqdn') then
  begin
    preloglist.Add('Found Parameter fqdn');
    myclientid := Application.GetOptionValue('f', 'fqdn');
    preloglist.Add('Found Parameter fqdn: ' + myclientid);
  end;

  if Application.HasOption('notifyformat') then
  begin
    preloglist.Add('Found Parameter notifyformat');
    myNotifyFormat := Application.GetOptionValue('notifyformat');
    preloglist.Add('Found Parameter notifyformat: ' + myNotifyFormat);
  end;


  mylang := GetDefaultLang;
  {$IFDEF WINDOWS}
  if Mylang = '' then
    mylang := LowerCase(copy(GetSystemDefaultLocale(LOCALE_SABBREVLANGNAME), 1, 2));
  {$ENDIF WINDOWS}
  {$IFDEF DARWIN}
  getMacLang(mylang,report);
  preloglist.Add(report);
  {$ENDIF DARWIN}
  SetDefaultLang(mylang);
  preloglist.Add('Detected default lang: ' + mylang);
  preloglist.Add('Detected default lang by laz: ' + GetDefaultLang);

  if Application.HasOption('lang') then
  begin
    preloglist.Add('Found Parameter lang');
    SetDefaultLang(Application.GetOptionValue('lang'));
    preloglist.Add('Found Parameter lang: ' + Application.GetOptionValue('lang'));
    preloglist.Add('Active lang: ' + GetDefaultLang);
  end;

  if Application.HasOption('service_url_port') then
  begin
    preloglist.Add('Found Parameter service_url_port');
    service_url_port := Application.GetOptionValue('service_url_port');
    preloglist.Add('Found Parameter service_url_port: ' + service_url_port);
  end;

  myservice_url := 'https://localhost:' + service_url_port + '/kiosk';

  // Initialize logging
  LogDatei := TLogInfo.Create;
  //lfilename := ExtractFileNameOnly(Application.ExeName);
  {$IFDEF UNIX}
  lfilename := 'systray-' + getCommandResult('id -un');
  {$ENDIF UNIX}
  {$IFDEF WINDOWS}
  lfilename := 'systray-' + GetUserName_;
  {$ENDIF WINDOWS}
  //LogDatei.FileName := lfilename;
  LogDatei.StandardLogFileext := '.log';
  LogDatei.StandardLogFilename := lfilename;
  LogDatei.WritePartLog := False;
  LogDatei.WriteErrFile := False;
  LogDatei.WriteHistFile := False;
  //LogDatei.StandardPartLogFilename := lfilename+ '-part';
  LogDatei.CreateTheLogfile(lfilename + '.log', False);
  // push prelog buffer to logfile
  if preloglist.Count > 0 then
    for i := 0 to preloglist.Count - 1 do
      LogDatei.log(preloglist.Strings[i], LLEssential);
  if logAndTerminate then
  begin
    LogDatei.log('Closing log and terminating due to previous errors.', LLCritical);
    logdatei.Close;
    Application.Terminate;
    Exit;
  end;
  LogDatei.log('Log for: ' + Application.exename + ' opend at : ' +
    DateTimeToStr(now), LLinfo);

  LogDatei.LogLevel := 8;
  LogDatei.debug_prog := True;
  // is opsiclientd running ?
  if numberOfProcessInstances('opsiclientd') < 1 then
  begin
    LogDatei.log('opsiclientd is not running - so we abort', LLCritical);
    LogDatei.Close;
    ShowMessage('opsiclientd is not running - so we abort');
    halt(1);
  end;
  LogDatei.log('opsiclientd is running - ok.', LLnotice);

  TrayIcon1.PopUpMenu := PopupMenu1;
  //icofilename := '/usr/local/share/opsi-client-systray/opsi-client-systray_co32.ico';
  //if fileexists(icofilename) then tmpimage.LoadFromFile(icofilename);
  {$IFDEF DARWIN}
  tmpimage := TPicture.Create;
  icofilename := '/usr/local/share/opsi-client-systray/opsi_client_systray_bw16.ico';
  if fileexists(icofilename) then
    tmpimage.LoadFromFile(icofilename);
  icofilename := ExtractFilePath(ParamStr(0)) + '/opsi_client_systray_bw16.ico';
  if fileexists(icofilename) then
    tmpimage.LoadFromFile(icofilename);
  icofilename := ExtractFilePath(ParamStr(0)) + '../../..//opsi_client_systray_bw16.ico';
  if fileexists(icofilename) then
    tmpimage.LoadFromFile(icofilename);
  icofilename := '/opt/opsi-client-systray/opsi_client_systray_bw16.ico';
  if fileexists(icofilename) then
    tmpimage.LoadFromFile(icofilename);
  TrayIcon1.Icon.Assign(tmpimage.Bitmap);
  FreeAndNil(tmpimage);
  {$ENDIF DARWIN}


  trayIcon1.Show;
  if checkIntervall = 0 then
    Timer1.Enabled := False
  else
  begin
    // checkIntervall is minutes ; intervall is millis
    Timer1.Interval := checkIntervall * 60 * 1000;
    Timer1.Enabled := True;
  end;
end;


procedure TDataModule1.MI_exitClick(Sender: TObject);
begin
  logDatei.log('Exit button clicked - will terminate.', LLnotice);
  Application.Terminate;
  halt(0);
end;

procedure TDataModule1.startNotify(notifyempty: boolean);
var
  list: TStringList;
  actionstring: string;
  i: integer;
  {$IFDEF LINUX}
  hello: PNotifyNotification;
  {$ENDIF LINUX}
begin
  initConnection(30);
  list := getActionrequests;
  actionstring := '';
  if list.Count = 0 then
  begin
    LogDatei.log('No action requests found.', LLNotice);
    if notifyempty then
    begin
      LogDatei.log('Notifying no requests', LLNotice);
      {$IFDEF WINDOWS}
      Trayicon1.BalloonFlags := bfInfo;
      TrayIcon1.BalloonHint := rsNone;
      TrayIcon1.BalloonTitle := rsActionsWaiting;
      TrayIcon1.ShowBalloonHint;
     {$ENDIF WINDOWS}
     {$IFDEF LINUX}
      notify_init(argv[0]);
      hello := notify_notification_new(PChar(rsNone), // Title
        PChar(actionstring), // Content
        'dialog-information'); // icon
      // Lets display it, but we will not handle any errors ...
      notify_notification_show(hello, nil);
      notify_uninit;
      {$ENDIF LINUX}
      {$IFDEF DARWIN}
      CFUserNotificationDisplayNotice(1000,kCFUserNotificationPlainAlertLevel
             ,nil,nil,nil,CFStr(Pchar(rsActionsWaiting))
             ,CFStr(Pchar(rsNone)),CFStr(Pchar('ok')));
      (*
      PopupNotifier1.Title := rsActionsWaiting;
      PopupNotifier1.Text := actionstring;
      PopupNotifier1.ShowAtPos(screen.Width - screen.Width div 4, 20);
      //PopupNotifier1.Icon;
      PopupNotifier1.Show;
      *)
      {$ENDIF DARWIN}
      LogDatei.log('Notifying no requests finished.', LLinfo);
    end;
  end
  else
  begin
    for i := 0 to list.Count - 1 do
      actionstring := actionstring + list[i] + LineEnding;
    LogDatei.log('Notifying Action requests found: ' + actionstring, LLNotice);
    {$IFDEF WINDOWS}
    Trayicon1.BalloonFlags := bfInfo;
    TrayIcon1.BalloonHint := actionstring;
    TrayIcon1.BalloonTitle := rsActionsWaiting;
    TrayIcon1.ShowBalloonHint;
    {$ENDIF WINDOWS}
    {$IFDEF LINUX}
    notify_init(argv[0]);
    hello := notify_notification_new(
      { Title   } PChar(rsActionsWaiting),
      { Content } PChar(actionstring),
      { Icon    } 'dialog-information');
    // Lets display it, but we will not handle any errors ...
    notify_notification_show(hello, nil);
    notify_uninit;
    {$ENDIF LINUX}
    {$IFDEF DARWIN}
    CFUserNotificationDisplayNotice(1000,kCFUserNotificationPlainAlertLevel
             ,nil,nil,nil,CFStr(Pchar(rsActionsWaiting))
             ,CFStr(Pchar(actionstring)),CFStr(Pchar('ok')));
    //CFUserNotificationCreate();
    //Application.MessageBox(PChar(rsActionsWaiting), PChar(actionstring), 0);
    (*
    PopupNotifier1.Title := rsActionsWaiting;
    PopupNotifier1.Text := actionstring;
    PopupNotifier1.ShowAtPos(screen.Width - screen.Width div 4, 20);
    //PopupNotifier1.Icon;
    PopupNotifier1.Show;

    Trayicon1.BalloonFlags := bfInfo;
    TrayIcon1.BalloonHint := actionstring;
    TrayIcon1.BalloonTitle := rsActionsWaiting;
    TrayIcon1.ShowBalloonHint;
    *)
      {$ENDIF DARWIN}
    LogDatei.log('Notifying requests finished.', LLinfo);
  end;
end;


procedure TDataModule1.MI_pull_for_action_requestClick(Sender: TObject);
begin
  logDatei.log('Check menu clicked - start check and notify.', LLnotice);
  startNotify(True);
end;

procedure TDataModule1.MI_starteventClick(Sender: TObject);
begin
  logDatei.log('Install now menu clicked - start firePushInstallation.', LLnotice);
  firePushInstallation;
end;

procedure TDataModule1.Timer1Timer(Sender: TObject);
begin
  logDatei.log('Timer fired - start check and notify.', LLnotice);
  startNotify(False);
end;

end.
