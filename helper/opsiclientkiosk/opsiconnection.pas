unit opsiconnection;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  oslog,
  oswebservice,
  //superobject,

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
  private
    //opsiProducts:ISuperObject;
    JSONDataForClient : TJSONData;
    JSONObjectProducts:TJSONObject;
    JSONObjectProduct:TJSONObject;
    JSONObjectConfigStates:TJSONObject;
    function MyOpsiMethodCall(const method: string; parameters: array of string): string;
    function MyOpsiMethodCall2(const method: string; parameters: array of string): string;
    //opsiProduct:ISuperObject;
    function GetDataFromNewDataStructure:boolean;
    function GetDataFromOldDataStructure:boolean;
    procedure ReadConfigdConf; //configd mode
    procedure ReadClientdConf(ClientID:string); //clientd mode

    function initConnection(const seconds: integer; var ConnectionInfo:string): boolean;
    procedure closeConnection;
  public
    MyClientID,
    MyHostkey,
    MyError,
    MyService_URL: string;
    MyExitcode,
    MyLoglevel: integer;
    ClientdMode : boolean;
    OpsiData: TOpsi4Data;
    procedure SetActionRequest(pid: string; request: string);
    function GetActionRequests: TStringList;
    procedure DoActionsOnDemand;
    procedure DoSingleActionOnDemand(ProductID:String);
    function GetConfigState(ConfigProperty:String):TStringList;
    procedure GetProductInfosFromServer;
    procedure SelectProduct(Index:integer);
    function GetProductValueAsString(key:string):string;
    function GetProductValueAsInteger(key:string):integer;
    function GetProductValueAsBoolean(key:string):boolean;
    function ProductCount:integer;
    constructor Create(fClientdMode:boolean; const ClientID:string = '' );overload;
    destructor Destroy;override;
  end;

 var
   OCKOpsiConnection:TOpsiConnection;

implementation

const
  opsiclientdconf =
    'C:\Program Files (x86)\opsi.org\opsi-client-agent\opsiclientd\opsiclientd.conf';


procedure TOpsiConnection.ReadConfigdConf;
var
  MyInifile: TInifile;
begin
  //opsiconfd mode
  try
    MyInifile := TIniFile.Create(opsiclientdconf);
    MyService_URL := MyIniFile.ReadString('config_service', 'url', '');
    MyClientID := MyIniFile.ReadString('global', 'host_id', '');
    MyHostkey := MyIniFile.ReadString('global', 'opsi_host_key', '');
    MyLoglevel := MyIniFile.ReadInteger('global', 'log_level', 5);
    if MyService_URL = '' then LogDatei.log('Error while reading file: ' + opsiclientdconf,LLDebug);
    //myloglevel := 7;
  finally
    MyInifile.Free;
  end;
end;

procedure TOpsiConnection.ReadClientdConf(ClientID: string);
begin
  // opsiclientd mode
  MyClientID := ClientID;//'pcjan.uib.local';//'jan-client01.uib.local';
  if MyClientID = '' then  LogDatei.log('Error could not get ClientID.' ,LLDebug);
  //MyClientID := oslog.getComputerName;
  MyService_URL := 'https://localhost:4441/kiosk';
  MyHostkey := '';
  MyLoglevel := 7;
end;


function TOpsiConnection.MyOpsiMethodCall(const method: string; parameters: array of string): string;
var
  omc: TOpsiMethodCall;
  ErrorOccured: boolean;
  resultstring: string;
begin
  Result := '';
  try
    omc := TOpsiMethodCall.Create(method, parameters);
    resultstring := OpsiData.checkAndRetrieve(omc, ErrorOccured);
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
  StringJSON: string;
  //new_obj: ISuperObject;
begin
  try
    StringJSON := MyOpsiMethodCall('backend_exit', []);
    //new_obj := SO(resultstring).O['result'];
    //JSONData := GetJSON(StringJSON).FindPath('result');
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

function TOpsiConnection.GetDataFromNewDataStructure:boolean;
var
  StringJSON,StringResult:string;
  count :integer;
  SoftwareOnDemand: boolean;
begin
  try
  { new data structur }
    StringJSON := MyOpsiMethodCall('getKioskProductInfosForClient', [myclientid, 'True']);
    JSONDataForClient := GetJSON(StringJSON).FindPath('result');
    //count := JSONDataProductInfos.Count;
    //StringResult := JSONDataForClient.AsJSON;
    JSONObjectProducts := TJSONObject(JSONDataForClient.FindPath('products'));
    //count:= JSONObjectProducts.Count;
    //StringResult := JSONObjectProducts.AsJSON;
    JSONObjectConfigStates := TJSONObject(JSONDataForClient.FindPath('configStates'));
    if (JSONObjectProducts <> nil) and (JSONObjectConfigStates <> nil) then
    begin
      Result := True;
      LogDatei.log('Kiosk mode: new', LLInfo);
    end
    else
    begin
      Result := False;
      LogDatei.log('Could not get data from new data structur.', LLInfo)
    end;
  except
    LogDatei.log('Error using Kiosk mode: new', LLDebug);
  end;
end;

function TOpsiConnection.GetDataFromOldDataStructure:boolean;
var
  StringJSON,StringResult:string;
  count :integer;
  SoftwareOnDemand: boolean;
begin
  try
    { old data structur }
    StringJSON := MyOpsiMethodCall('getKioskProductInfosForClient', [myclientid]);
    JSONDataForClient := GetJSON(StringJSON).FindPath('result');
    //count := JSONDataProductInfos.Count;
    //StringResult := JSONDataForClient.AsJSON;
    JSONObjectProducts := TJSONObject(JSONDataForClient);
    //count:= JSONObjectProducts.Count;
    //StringResult := JSONObjectProducts.AsJSON;
    //StringResult := JSONData.Items[0].FindPath('configStates').AsJSON;
    //count := JSONData.Items[0].FindPath('configStates').Count;
    JSONObjectConfigStates := nil;
    if (JSONObjectProducts) <> nil then
    begin
      Result := True;
      ShowMessage('Kiosk mode: old. Update to newer opsi version to get full functionality (e.g. disable software on demand) of Opsi Kiosk.');
      LogDatei.log('Old kiosk mode', LLInfo);
    end
    else
    begin
      Result := False;
      LogDatei.log('Could not get data from old data structur.', LLInfo)
    end;
  except
    LogDatei.log('Error using Kiosk mode: old', LLDebug);
    ShowMessage('Could not get data from server.')
  end;
end;

procedure TOpsiConnection.GetProductInfosFromServer;
begin
  try
    if not GetDataFromNewDataStructure then GetDataFromOldDataStructure;
  except
    on EExternalException do
    begin
      //GetDataFromOldDataStructure;
    end;
  end;
end;

procedure TOpsiConnection.SelectProduct(Index: integer);
begin
  JSONObjectProduct := TJSONObject(OCKOpsiConnection.JSONObjectProducts.Items[index]);
  logdatei.log('Selected Product: ' + JSONObjectProduct.Strings['productId'], LLInfo);
end;

function TOpsiConnection.GetProductValueAsString(key: string): string;
begin
  if not JSONObjectProduct.Nulls[key] then
        Result := JSONObjectProduct.Strings[key]
  else Result := '';
  //logdatei.log('read ' + key + ': ' + Result, LLDebug3);
end;


function TOpsiConnection.GetProductValueAsInteger(key: string): integer;
begin
  if not JSONObjectProduct.Nulls[key] then
    Result := JSONObjectProduct.Integers[key]
  else
    begin
      ShowMessage('No value found! Set' + key +  'to 0.');
      Result := 0;
    end;
end;

function TOpsiConnection.GetProductValueAsBoolean(key: string): boolean;
begin
  if not JSONObjectProduct.Nulls[key] then
    Result := JSONObjectProduct.Booleans[key]
  else
    begin
      ShowMessage('No value found! Set' + key +  'to false.');
      Result := False;
    end;
end;



function TOpsiConnection.ProductCount: integer;
begin
  Result := JSONObjectProducts.Count;
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


constructor TOpsiConnection.Create(fClientdMode:boolean; const ClientID:string = '');overload;
begin
  inherited Create;
  ClientdMode := fClientdMode;
  MyExitcode := 0;
  MyError := '';
  try
    if ClientdMode then ReadClientdConf(ClientID)
      else ReadConfigdConf;
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
  except
    LogDatei.log('Error while initializing opsiconnection.', LLDebug);
    ShowMessage('Error while initializing opsiconnection.');
  end;
end;

destructor TOpsiConnection.Destroy;
begin
  JSONDataForClient.Free;
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

function TOpsiConnection.GetActionRequests: TStringList;
var
  StringJSON, str: string;
  //new_obj, opsiProduct: ISuperObject;
  opsiProducts :TJSONData;
  opsiProduct: TJSONObject;
  i: integer;
begin
  Result := TStringList.Create;
  StringJSON := MyOpsiMethodCall('productOnClient_getObjects',
    ['[]', '{"clientId":"' + myclientid + '","actionRequest":["setup","uninstall"]}']);
  //new_obj := SO(resultstring).O['result'];
  opsiProducts := GetJSON(StringJSON).FindPath('result');
  str := opsiProducts.AsJSON;
  for i := 0 to opsiProducts.Count - 1 do
  begin
    opsiProduct := opsiProducts.Items[i] as TJSONObject;
    Result.Add(opsiProduct.Strings['productId'] + ' : ' + opsiProduct.Strings['actionRequest']);//['actionRequest"'] ?
  end;
  opsiProducts.Free;
end;


procedure TOpsiConnection.DoSingleActionOnDemand(ProductID: String);
var
  resultstring:String;
begin
   resultstring := MyOpsiMethodCall('fireEvent_software_on_demand', [ProductID]);
end;

procedure TOpsiConnection.DoActionsOnDemand;
var
  resultstring:String;
begin
   resultstring := MyOpsiMethodCall('fireEvent_software_on_demand', []);
end;


function TOpsiConnection.GetConfigState(ConfigProperty:String): TStringList;
var
  i : integer;
begin
  Result := TStringList.Create;
  if JSONObjectConfigStates <> nil then
    for i := 0 to JSONObjectConfigStates.Arrays[ConfigProperty].Count -1 do
     Result.Add(JSONObjectConfigStates.Arrays[ConfigProperty].Items[i].AsString)
  else Result.Add('True');
end;

//initialization
 //OckOpsiConnection := TOpsiConnection.Create(True);
end.



