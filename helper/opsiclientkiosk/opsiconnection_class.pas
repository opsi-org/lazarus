unit opsiconnection_class;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  //Controls,
  oslog,
  oswebservice,
  superobject,
  //oscrypt,
  lazfileutils,
  //sqlite3conn, sqldb,
  db,
  inifiles,
  Variants,
 // fileinfo,
  proginfo,
  //winpeimagereader,
  //lcltranslator,
  //datadb,
  //osprocesses,
  //jwawinbase,
  //dialogs,
  progresswindow;

type
  TMyOpsiConf = record
    myclientid, myhostkey, myerror, myservice_url: string;
  end;

  { TOpsiConnection }

  TOpsiConnection = class(TObject)
    myclientid,
    myhostkey,
    myerror,
    myservice_url: string;
    myexitcode,
    myloglevel: integer;
    opsidata: TOpsi4Data;
    opsiclientdmode : boolean;
    procedure readconf;
    procedure readconf2;
    function MyOpsiMethodCall(const method: string; parameters: array of string): string;
    function MyOpsiMethodCall2(const method: string; parameters: array of string): string;
    function initConnection(const seconds: integer; var ConnectionInfo:string): boolean;
    procedure closeConnection;
    procedure fetchProductData(Data:TDataSet; const OpsiMethod:string);
    constructor Create(clientdmode:boolean);overload;
    destructor Destroy;override;
  end;

 var
   OckOpsiConnection:TOpsiConnection;

implementation

const
  opsiclientdconf =
    'C:\Program Files (x86)\opsi.org\opsi-client-agent\opsiclientd\opsiclientd.conf';


procedure TOpsiConnection.readconf;
var
  myini: TInifile;
begin
  //opsiconfd mode
  myini := TIniFile.Create(opsiclientdconf);
  myservice_url := myini.ReadString('config_service', 'url', '');
  myclientid := myini.ReadString('global', 'host_id', '');
  myhostkey := myini.ReadString('global', 'opsi_host_key', '');
  //myloglevel := myini.ReadInteger('global', 'log_level', 5);
  myloglevel := 7;
  myini.Free;
end;

procedure TOpsiConnection.readconf2;
begin
  // opsiclientd mode
  myservice_url := 'https://localhost:4441/kiosk';
  //myclientid := 'localhost';
  //myclientid := oslog.getComputerName;
  myhostkey := '';
  myloglevel := 7;
end;


function TOpsiConnection.MyOpsiMethodCall(const method: string; parameters: array of string): string;
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
    on e: Exception do
    begin
      LogDatei.log('Exception calling method: ' + method, LLerror);
      logdatei.log('Exception: ' + E.message, LLError);
      logdatei.log('Exception handled at: ' + getCallAddrStr, LLError);
      logdatei.log_exception(E,LLError);
    end;
  end;
end;

function TOpsiConnection.MyOpsiMethodCall2(const method: string; parameters: array of string): string;
var
  omc: TOpsiMethodCall;
  errorOccured: boolean;
  //resultstring: string;
  resultstringlist: TStringList;
  i: integer;
begin
  try
    omc := TOpsiMethodCall.Create(method, parameters);
    resultstringlist := TStringList.Create;
    resultstringlist := opsidata.checkAndRetrieveList(omc, errorOccured);
    for i := 0 to resultstringlist.Count - 1 do
      Result := resultstringlist[i];

  except
    on e: Exception do
    begin
      LogDatei.log('Exception calling method: ' + method, LLerror);
      logdatei.log('Exception: ' + E.message, LLError);
      logdatei.log('Exception handled at: ' + getCallAddrStr, LLError);
      logdatei.log_exception(E,LLError);
    end;
  end;
end;

procedure TOpsiConnection.closeConnection;
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
      logdatei.log('Exception handled at: ' + getCallAddrStr, LLError);
      logdatei.log_exception(E,LLError);
    end;
  end;
end;

function TOpsiConnection.initConnection(const seconds: integer; var ConnectionInfo:string): boolean;
var
  networkup, timeout: boolean;
  myseconds: integer;
  resultstring: string;
begin
  FormProgressWindow.LabelDataLoad.Caption := 'Connecting to OPSI web service';
  //FormProgressWindow.ProcessMess;
  //FormOpsiClientKiosk.Cursor := crHourGlass;
  Result := False;
  networkup := False;
  timeout := False;
  myseconds := seconds;

  FormProgressWindow.ProgressBar1.StepIt;
  //FormProgressWindow.ProcessMess;
  repeat
    try
      FormProgressWindow.ProcessMess;
      if myseconds > 0 then
      begin
        resultstring := MyOpsiMethodCall('getDepotId', [myclientid]);
        networkup := True;
      end
      else
        timeout := True;
    except
      LogDatei.log('opsidata not connected - retry', LLInfo);
      myseconds := myseconds - 1;
      FormProgressWindow.ProgressBar1.StepIt;
      Sleep(1000);
    end;
    FormProgressWindow.ProgressBar1.StepIt;
    //FormProgressWindow.ProcessMess;
  until networkup or timeout;

  if networkup then
  begin
    LogDatei.log('opsidata connected', LLInfo);
    //FormOpsiClientKiosk.StatusBar1.Panels[0].Text :=
      //'Connected to ' + myservice_url + ' as ' + myclientid;
    ConnectionInfo := 'Connected to ' + myservice_url + ' as ' + myclientid;
    Result := True;
  end
  else
  begin
    LogDatei.log('init connection failed (timeout after ' + IntToStr(seconds) +
      ' seconds/retries.', LLError);
    //FormOpsiClientKiosk.StatusBar1.Panels[0].Text := 'Connection failed';
    ConnectionInfo := 'Connection failed';
  end;
  FormProgressWindow.ProgressBar1.StepIt;
  FormProgressWindow.ProcessMess;
  //FormOpsiClientKiosk.Cursor := crArrow;
end;

procedure TOpsiConnection.fetchProductData(Data:TDataSet; const OpsiMethod:string);
var
  resultstring, groupstring, method, testresult: string;
  jOResult, new_obj, detail_obj: ISuperObject;
  i: integer;
  str, pid, depotid, pidliststr, reqtype: string;
  //productdatarecord: TProductData;
begin
  logdatei.log('starting fetchProductData ....', LLInfo);
  FormProgressWindow.ProgressBar1.StepIt;
  FormProgressWindow.ProcessMess;
  FormProgressWindow.LabelInfo.Caption := 'Loading data ...';

  resultstring := MyOpsiMethodCall('getKioskProductInfosForClient', [myclientid]);
  new_obj := SO(resultstring).O['result'];
  LogDatei.log('Get products done', LLNotice);

  FormProgressWindow.LabelDataload.Caption := 'Fill Database';
  FormProgressWindow.ProgressbarDetail.Max := new_obj.AsArray.Length;
  FormProgressWindow.ProgressbarDetail.Min := 0;
  FormProgressWindow.ProgressbarDetail.Position := 0;
  FormProgressWindow.ProcessMess;

  // product data to database
  Data.Open;
  for i := 0 to new_obj.AsArray.Length - 1 do
  begin
    detail_obj := new_obj.AsArray.O[i];
    //str := detail_obj.AsString;
    //str := detail_obj.S['productId'];
    //FormProgressWindow.LabelDataLoadDetail.Caption := str;
    //FormProgressWindow.ProgressBarDetail.StepIt;
    //FormProgressWindow.ProcessMess;
    //productdatarecord.id := str;
    logdatei.log('read: ' + detail_obj.S['productId'], LLInfo);
    Data.Append;
    Data.FieldByName('ProductId').AsString := detail_obj.S['productId'];
    Data.FieldByName('productVersion').AsString :=
      detail_obj.S['productVersion'];
    Data.FieldByName('packageVersion').AsString :=
      detail_obj.S['packageVersion'];
    Data.FieldByName('versionstr').AsString :=
      detail_obj.S['productVersion'] + '-' + detail_obj.S['packageVersion'];
    Data.FieldByName('ProductName').AsString := detail_obj.S['productName'];
    Data.FieldByName('description').AsString := detail_obj.S['description'];
    Data.FieldByName('advice').AsString := detail_obj.S['advice'];
    Data.FieldByName('priority').AsString := detail_obj.S['priority'];
    Data.FieldByName('producttype').AsString := detail_obj.S['productType'];
    Data.FieldByName('hasSetup').AsString := detail_obj.S['hasSetup'];
    Data.FieldByName('hasUninstall').AsString :=
      detail_obj.S['hasUninstall'];
    if detail_obj.S['installationStatus'] = 'not_installed' then
      Data.FieldByName('installationStatus').AsString := ''
    else
      Data.FieldByName('installationStatus').AsString :=
        detail_obj.S['installationStatus'];
    Data.FieldByName('installedprodver').AsString :=
      detail_obj.S['installedProdVer'];
    Data.FieldByName('installedpackver').AsString :=
      detail_obj.S['installedPackVer'];
    Data.FieldByName('installedverstr').AsString :=
      detail_obj.S['installedVerStr'];
    if detail_obj.S['actionRequest'] = 'none' then
      Data.FieldByName('actionrequest').AsString := ''
    else
      Data.FieldByName('actionrequest').AsString :=
        detail_obj.S['actionRequest'];
    Data.FieldByName('actionresult').AsString := detail_obj.S['actionResult'];
    Data.FieldByName('updatePossible').AsString :=
      detail_obj.S['updatePossible'];
    Data.FieldByName('possibleAction').AsString :=
      detail_obj.S['possibleAction'];
    Data.Post;

    // productDependencies
  end;

  Data.Close;
  //Data.Open;
  //Data.First;
end;

constructor TOpsiConnection.Create(clientdmode:boolean);overload;
begin
  inherited Create;
  opsiclientdmode := clientdmode;
  if opsiclientdmode then readconf2
    else readconf;
  OpsiData := TOpsi4Data.Create;
  LogDatei.log('service_url=' + myservice_url, LLDebug2);
  LogDatei.log('service_pass=' + myhostkey, LLDebug2);
  LogDatei.log('clientid=' + myclientid, LLDebug2);
  LogDatei.log('opsidata created', LLDebug2);
  OpsiData.SetActualClient(myclientid);
  OpsiData.InitOpsiConf(myservice_url, myclientid,
    myhostkey, '', '', '', 'opsi-client-kiosk-' + ProgramInfo.Version);
  LogDatei.log('opsidata initialized', LLDebug2);
end;

destructor TOpsiConnection.Destroy;
begin
  if OpsiData <> nil then FreeAndNil(OpsiData);
  inherited Destroy;
end;

//initialization
 //OckOpsiConnection := TOpsiConnection.Create(True);
end.



