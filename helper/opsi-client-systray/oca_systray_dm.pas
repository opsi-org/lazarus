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
  winpeimagereader,
  superobject,
  lcltranslator;

type

  { TDataModule1 }

  TDataModule1 = class(TDataModule)
    MI_exit: TMenuItem;
    MI_startevent: TMenuItem;
    MI_pull_for_action_request: TMenuItem;
    PopupMenu1: TPopupMenu;
    Timer1: TTimer;
    TrayIcon1: TTrayIcon;
    procedure DataModuleCreate(Sender: TObject);
    procedure MI_exitClick(Sender: TObject);
    procedure MI_pull_for_action_requestClick(Sender: TObject);
    procedure MI_starteventClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure WriteHelp;
  private
    { private declarations }
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

implementation

{$R *.lfm}

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
  i : integer;
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
            ['[]', '{"id":"' + productid + '","productVersion":"' + prodver +
            '","packageVersion":"' + packver + '"}']);
          LogDatei.log('resultstring: ' + resultstring, LLDebug2);
          prod_obj := SO(resultstring).O['result'];
          prodname := prod_obj.AsArray.O[0].S['name'];
          LogDatei.log('productid: ' + productid + ' prodver: ' + prodver
            + ' packver: ' + packver + ' prodname: ' + prodname + ' request: ' + request, LLInfo);
          //Result.Add(detail_obj.S['productId'] + ' : ' + detail_obj.S['actionRequest"']);
        end
        else LogDatei.log('productid: ' + productid +  ' request: ' + request, LLInfo);
        if myNotifyFormat = 'productname productversion : request' then
          Result.Add(prodname + ' '+prodver+' : ' + request);
        if myNotifyFormat = 'productname : request' then
          Result.Add(prodname +' : ' + request);
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



procedure closeConnection;
var
  resultstring: string;
  new_obj: ISuperObject;
begin
  try
    resultstring := MyOpsiMethodCall('backend_exit', []);
    new_obj := SO(resultstring).O['result'];
  except
    on e: Exception do
    begin
      logdatei.log('Exception closeConnection', LLError);
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
  //myservice_url := 'https://localhost:4441/
  //myclientid := '';
  FreeAndNil(opsidata);
  initConnection(30);
  resultstring := MyOpsiMethodCall('fireEvent_software_on_demand', []);
  closeConnection;
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
    +
    LineEnding;
  msg := msg + ' --fqdn=<fqdn of the client> -> ; required' + LineEnding;
  msg := msg + ' --lang=<lang of the client>' + LineEnding;
  msg := msg + ' --notifyformat=<notifyformat>' + LineEnding;
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
begin
  checkIntervall := 0;
  myNotifyFormat := 'productid : request';
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
  optionlist.Add('fqdn:');
  optionlist.Add('notifyformat:');
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


  GetDefaultLang;
  preloglist.Add('Detected default lang: ' + GetDefaultLang);
  if Application.HasOption('lang') then
  begin
    preloglist.Add('Found Parameter lang');
    SetDefaultLang(Application.GetOptionValue('lang'));
    preloglist.Add('Found Parameter lang: ' + Application.GetOptionValue('lang'));
    preloglist.Add('Active lang: ' + GetDefaultLang);
  end;

  myservice_url := 'https://localhost:4441/kiosk';

  // Initialize logging
  LogDatei := TLogInfo.Create;
  lfilename := ExtractFileNameOnly(Application.ExeName);
  LogDatei.FileName := lfilename;
  LogDatei.StandardLogFileext := '.log';
  LogDatei.StandardLogFilename := lfilename;
  LogDatei.WritePartLog := False;
  //LogDatei.StandardPartLogFilename := lfilename+ '-part';
  LogDatei.CreateTheLogfile(lfilename + '.log', True);
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

  TrayIcon1.PopUpMenu := PopupMenu1;
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
  Application.Terminate;
end;

procedure TDataModule1.MI_pull_for_action_requestClick(Sender: TObject);
begin
  Timer1Timer(Sender);
end;

procedure TDataModule1.MI_starteventClick(Sender: TObject);
begin
  firePushInstallation;
end;

procedure TDataModule1.Timer1Timer(Sender: TObject);
var
  list: TStringList;
  actionstring: string;
  i: integer;
begin
  initConnection(30);
  list := getActionrequests;
  closeConnection;
  actionstring := '';
  if list.Count = 0 then
    LogDatei.log('No action requests found.', LLNotice)
  else
  begin
    for i := 0 to list.Count - 1 do
      actionstring := actionstring + list[i] + LineEnding;
    LogDatei.log('Action requests found: ' + actionstring, LLNotice);
    Trayicon1.BalloonFlags := bfInfo;
    TrayIcon1.BalloonHint := actionstring;
    TrayIcon1.BalloonTitle := rsActionsWaiting;
    TrayIcon1.ShowBalloonHint;
  end;
end;

end.
