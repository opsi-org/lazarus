unit opsiconnection;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  oslog,
  oswebservice,
  superobject,
  //oscrypt,
  proginfo,
  //lazfileutils,
  inifiles,
  progresswindow;

procedure readconf;
procedure readconf2;
function MyOpsiMethodCall(const method: string; parameters: array of string): string;
function MyOpsiMethodCall2(const method: string; parameters: array of string): string;
function initConnection(const seconds: integer; var ConnectionInfo:string): boolean;
procedure closeConnection;



implementation

const
  opsiclientdconf =
    'C:\Program Files (x86)\opsi.org\opsi-client-agent\opsiclientd\opsiclientd.conf';
var
  myexitcode, myloglevel: integer;
  myclientid, myhostkey, myerror, myservice_url: string;
  //INI: TINIFile;
  //logfilename: string;

procedure readconf;
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

procedure readconf2;
begin
  // opsiclientd mode
  myservice_url := 'https://localhost:4441/kiosk';
  //myclientid := 'localhost';
  //myclientid := oslog.getComputerName;
  myhostkey := '';
  myloglevel := 7;
end;


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
    on e: Exception do
    begin
      LogDatei.log('Exception calling method: ' + method, LLerror);
      logdatei.log('Exception: ' + E.message, LLError);
      logdatei.log('Exception handled at: ' + getCallAddrStr, LLError);
      logdatei.log_exception(E,LLError);
    end;
  end;
end;

function MyOpsiMethodCall2(const method: string; parameters: array of string): string;
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
      logdatei.log('Exception handled at: ' + getCallAddrStr, LLError);
      logdatei.log_exception(E,LLError);
    end;
  end;
end;

function initConnection(const seconds: integer; var ConnectionInfo:string): boolean;
var
  networkup, timeout: boolean;
  myseconds: integer;
  resultstring: string;
begin
  FormProgressWindow.LabelDataLoad.Caption := 'Connecting to OPSI web service';
  FormProgressWindow.ProcessMess;
  //FormOpsiClientKiosk.Cursor := crHourGlass;
  Result := False;
  networkup := False;
  timeout := False;
  myseconds := seconds;
  LogDatei.log('service_url=' + myservice_url, LLDebug2);
  LogDatei.log('service_pass=' + myhostkey, LLDebug2);
  LogDatei.log('clientid=' + myclientid, LLDebug2);
  opsidata := TOpsi4Data.Create;
  LogDatei.log('opsidata created', LLDebug2);
  opsidata.setActualClient(myclientid);
  opsidata.initOpsiConf(myservice_url, myclientid,
    myhostkey, '', '', '', 'opsi-client-kiosk-' + ProgramInfo.Version);
  LogDatei.log('opsidata initialized', LLDebug2);
  FormProgressWindow.ProgressBar1.StepIt;
  FormProgressWindow.ProcessMess;
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
    FormProgressWindow.ProcessMess;
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

end.

