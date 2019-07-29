unit opsiconnection;

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
  //db,
  inifiles,
  Variants,
 // fileinfo,
  proginfo,
  fpjson,
  jsonParser,
  //winpeimagereader,
  //lcltranslator,
  //datadb,
  //osprocesses,
  //jwawinbase,
  dialogs;
  //progresswindow;

type
  //TMyOpsiConf = record
   // myclientid, myhostkey, myerror, myservice_url: string;
  //end;


  { TOpsiConnection }

  TOpsiConnection = class(TObject)
    myclientid,
    myhostkey,
    myerror,
    myservice_url: string;
    myexitcode,
    myloglevel: integer;
    OpsiData: TOpsi4Data;
    opsiclientdmode : boolean;
    opsiProducts:ISuperObject;
    JSONData : TJSONData;
    JSONObjectConfigStates,
    JSONObjectProducts:TJSONObject;
    //opsiProduct:ISuperObject;
    procedure readconf;
    procedure readconf2;
    function MyOpsiMethodCall(const method: string; parameters: array of string): string;
    function MyOpsiMethodCall2(const method: string; parameters: array of string): string;
    function initConnection(const seconds: integer; var ConnectionInfo:string): boolean;
    procedure closeConnection;
    procedure SetActionRequest(pid: string; request: string);
    function GetActionRequests: TStringList;
    procedure FirePushInstallation;
    function GetConfigState(ConfigProperty:String):TStringList;
    procedure GetJSONFromServer;
    constructor Create(clientdmode:boolean; ClientID:string);overload;
    destructor Destroy;override;
  end;

 var
   OCKOpsiConnection:TOpsiConnection;

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
  myclientid := 'pcjan.uib.local';
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
    resultstring := OpsiData.checkAndRetrieve(omc, errorOccured);
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
    resultstringlist := OpsiData.checkAndRetrieveList(omc, errorOccured);
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

procedure TOpsiConnection.GetJSONFromServer;
var
  StringJSON,StringResult:string;
  //JSONData,JSONDataResult:TJSONData;
  JSONObject1:TJSONObject;
  count :integer;
  SoftwareOnDemand: boolean;
begin
  StringJSON := MyOpsiMethodCall('getKioskProductInfosForClient', [myclientid]);
  JSONData := GetJSON(StringJSON).FindPath('result');
  //count := JSONData.Count;
  //StringResult := JSONData.AsJSON;
  JSONObjectProducts := TJSONObject(JSONData.Items[0].FindPath('products'));
  count:= JSONObjectProducts.Count;
  StringResult := JSONObjectProducts.AsJSON;
  //StringResult := JSONData.Items[0].FindPath('configStates').AsJSON;
  //count := JSONData.Items[0].FindPath('configStates').Count;
  JSONObjectConfigStates := TJSONObject(JSONData.Items[0].FindPath('configStates'));
  //SoftwareOnDemand := JSONObjectConfigStates.Arrays['software-on-demand.kiosk.allowed'].Items[0].AsBoolean;
  opsiProducts := SO(StringResult);
end;

function TOpsiConnection.initConnection(const seconds: integer; var ConnectionInfo:string): boolean;
var
  networkup, timeout: boolean;
  myseconds: integer;
  resultstring: string;
begin
  //FormProgressWindow.LabelDataLoad.Caption := 'Connecting to OPSI web service';
  //FormProgressWindow.ProgressBar1.StepIt;
  //FormOpsiClientKiosk.Cursor := crHourGlass;
  //FormProgressWindow.ProcessMess;
  Result := False;
  networkup := False;
  timeout := False;
  myseconds := seconds;
  repeat
    try
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
      //FormProgressWindow.ProgressBar1.StepIt;
      Sleep(1000);
    end;
    //FormProgressWindow.ProgressBar1.StepIt;
    //FormProgressWindow.ProcessMess;
  until networkup or timeout;

  if networkup then
  begin
    LogDatei.log('opsidata connected', LLInfo);
    //FormProgressWindow.ProgressBar1.StepIt;
    //FormProgressWindow.ProcessMess;
    //FormProgressWindow.LabelDataLoad.Caption := 'Connection  done';
    ConnectionInfo := 'Connected to ' + myservice_url + ' as ' + myclientid;
    Result := True;
  end
  else
  begin
    LogDatei.log('init connection failed (timeout after ' + IntToStr(seconds) +
      ' seconds/retries.', LLError);
    ConnectionInfo := 'Connection failed';
  end;
  //FormOpsiClientKiosk.Cursor := crArrow;
end;


constructor TOpsiConnection.Create(clientdmode:boolean; ClientID:string);overload;
begin
  inherited Create;
  opsiclientdmode := clientdmode;
  myexitcode := 0;
  myerror := '';
  myclientid := ClientID;
  if opsiclientdmode then readconf2
    else readconf;
  LogDatei.log('service_url=' + myservice_url, LLDebug2);
  //LogDatei.log('service_pass=' + myhostkey, LLDebug2);
  LogDatei.log('clientid=' + myclientid, LLDebug2);
  LogDatei.log('service_user=' + myclientid, LLNotice);
  LogDatei.AddToConfidentials(myhostkey);
  LogDatei.log('host_key=' + myhostkey, LLdebug3);
  OpsiData := TOpsi4Data.Create;
  LogDatei.log('opsidata created', LLDebug2);
  OpsiData.SetActualClient(myclientid);
  OpsiData.InitOpsiConf(myservice_url, myclientid,
    myhostkey, '', '', '', 'opsi-client-kiosk-' + ProgramInfo.Version);
  LogDatei.log('opsidata initialized', LLDebug2);
end;

destructor TOpsiConnection.Destroy;
begin
  JSONData.Free;
  if OpsiData <> nil then FreeAndNil(OpsiData);
  inherited Destroy;
end;

procedure TOpsiConnection.SetActionRequest(pid: string; request: string);
var
  resultstring: string;
begin
  resultstring := MyOpsiMethodCall('setProductActionRequestWithDependencies',
    [pid, MyClientID, request]);
end;

function TOpsiConnection.getActionRequests: TStringList;
var
  resultstring, str: string;
  new_obj, opsiProduct: ISuperObject;
  i: integer;
begin
  Result := TStringList.Create;
  resultstring := MyOpsiMethodCall('productOnClient_getObjects',
    ['[]', '{"clientId":"' + myclientid + '","actionRequest":["setup","uninstall"]}']);
  new_obj := SO(resultstring).O['result'];
  str := new_obj.AsString;
  for i := 0 to new_obj.AsArray.Length - 1 do
  begin
    opsiProduct := new_obj.AsArray.O[i];
    Result.Add(opsiProduct.S['productId'] + ' : ' + opsiProduct.S['actionRequest"']);
  end;
end;

procedure TOpsiConnection.firePushInstallation;
var
  resultstring, str: string;
  proginfo: string;
begin
  // switch to opsiclientd mode if we on opsiconfd
  if opsiclientdmode then readconf2;
  FreeAndNil(opsidata);
  initConnection(30,proginfo);
  // opsiclientd mode
  resultstring := MyOpsiMethodCall('fireEvent_software_on_demand', []);
  //closeConnection;
  // switch back to opsiconfd mode
  if not opsiclientdmode then readconf;
  //FreeAndNil(opsidata);
  //initConnection(30);
  // opsiconfd mode
  // may not work if acl.conf is restricted
  //resultstring := MyOpsiMethodCall('hostControlSafe_fireEvent',  ['on_demand', '[' + myclientid + ']']);
end;


function TOpsiConnection.GetConfigState(ConfigProperty:String): TStringList;
var
  i : integer;
begin
  Result := TStringList.Create;
  for i := 0 to JSONObjectConfigStates.Arrays[ConfigProperty].Count -1 do
   Result.Add(JSONObjectConfigStates.Arrays[ConfigProperty].Items[i].AsString);
end;

//initialization
 //OckOpsiConnection := TOpsiConnection.Create(True);
end.



