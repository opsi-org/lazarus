// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: Rupert Roeder, detlef oertel
// credits: http://www.opsi.org/credits/


unit ockdata;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  Controls,
  oslog,
  oswebservice,
  superobject,
  oscrypt,
  lazfileutils,
  sqlite3conn, sqldb,
  DB,
  inifiles,
  Variants,
 // fileinfo,
  winpeimagereader,
  lcltranslator,
  datadb,
  osprocesses,
  jwawinbase,
  dialogs, progresswindow;

const
  opsiclientdconf =
    'C:\Program Files (x86)\opsi.org\opsi-client-agent\opsiclientd\opsiclientd.conf';

type
//  Tmythread = class(TThread)
//  public
//    procedure Execute; override;
//  end;


  TProductData = record
    id: string;
    Name: string;
    description: string;
    advice: string;
    productversion: string;
    packageversion: string;
    versionstr: string;
    priority: integer;
    producttype: string;
    installationStatus: string;
    installedprodver: string;
    installedpackver: string;
    installedverstr: string;
    actionrequest: string;
    actionresult: string;
    updatePossible: boolean;
    hasSetup: boolean;
    hasUninstall: boolean;
    possibleAction: string;
  end;

  TPProductData = ^TProductData;


var
  optionlist: TStringList;
  myexitcode, myloglevel: integer;
  myclientid, myhostkey, myerror, myservice_url: string;
  INI: TINIFile;
  logfilename: string;
  myuser, myencryptedpass, mypass, myshare, mydepot, mymountpoint,
  mydepotuser, mydomain: string;
  mountresult: dword;
  nogui: boolean;
  productgrouplist: TStringList;
  opsidata: Topsi4data;
  ZMQuerydataset1: TSQLQuery;
  ZMQuerydataset2: TSQLQuery;
  productIdsList: TStringList;
  opsiclientdmode : boolean = true;


procedure initdb;
procedure main;
procedure setActionrequest(pid: string; request: string);
function getActionrequests: TStringList;
procedure firePushInstallation;
procedure fetchProductData_by_getKioskProductInfosForClient;

resourcestring
  rsNoGroups = 'No Product Groups found ...';
  rsAllGroups = 'All groups';

implementation

uses
  opsiclientkioskgui;

{*var
  mythread: Tmythread;

procedure Tmythread.Execute;
var
  countms, intervalms: integer;
begin
  countms := 0;
  intervalms := 100;
  repeat
    sleep(intervalms);
    countms := countms + intervalms;
  until Terminated or (countms > 31000);
  // if we called the terminate method we do nothing
  if not Terminated then
  begin
    LogDatei.log('network timeout by thread - aborting program', LLInfo);
    halt(0);
  end;
end;*}

// from osfunc
function divideAtFirst(const partialS, S: string; var part1, part2: string): boolean;
  // teilt den String S beim ersten Vorkommen des Teilstrings partialS;
  //   liefert true, wenn partialS vorkommt,
  //   andernfalls false;
  //   wenn partialS nicht vorkommt, enthaelt part1 den Gesamtstring, part2 ist leer

var
  i: integer = 0;
begin
  i := pos(lowercase(partialS), lowercase(s));
  if i > 0 then
  begin
    part1 := copy(S, 1, i - 1);
    part2 := copy(S, i + length(partialS), length(S));
    Result := True;
  end
  else
  begin
    part1 := s;
    part2 := '';
    Result := False;
  end;
end;

// from osfunc
procedure stringsplit(const s, delimiter: string; var Result: TStringList);
// produziert eine Stringliste aus den Teilstrings, die zwischen den Delimiter-Strings stehen
var
  remainder: string = '';
  item: string = '';
  found: boolean;
begin
  found := divideAtFirst(delimiter, s, item, remainder);
  while found do
  begin
    Result.add(item);
    found := divideAtFirst(delimiter, remainder, item, remainder);
  end;
  Result.add(item);
end;


procedure initdb;
var
  newFile: boolean;
begin
  logdatei.log('startinitdb ', LLInfo);
  with FormProgressWindow do
  begin
    LabelDataload.Caption := 'Create database';
    ProgressBar1.StepIt;
  end;
  Datamodule1 := TDataModule1.Create(nil);
  Datamodule1.SQLite3Connection1.Close; // Ensure the connection is closed when we start
  try
    // Since we're making this database for the first time,
    // check whether the file already exists
    Datamodule1.SQLite3Connection1.DatabaseName := GetTempDir + 'opsikiosk.db';
    logdatei.log('db is : ' + Datamodule1.SQLite3Connection1.DatabaseName, LLInfo);
    if FileExists(Datamodule1.SQLite3Connection1.DatabaseName) then
      DeleteFileUTF8(Datamodule1.SQLite3Connection1.DatabaseName);
    newFile := not FileExists(Datamodule1.SQLite3Connection1.DatabaseName);

    if newFile then
    begin
      // Create the database and the tables
      try
        logdatei.log('Creating new database ', LLInfo);
        Datamodule1.SQLite3Connection1.Open;
        Datamodule1.SQLTransaction1.Active := True;
        ZMQuerydataset1 := Datamodule1.SQLQuery1;
        ZMQuerydataset2 := Datamodule1.SQLQuery2;

        try
          Datamodule1.SQLite3Connection1.ExecuteDirect(
            'CREATE TABLE products (' + 'ProductId String not null primary key, ' +
            'ProductName String, ' + 'description String, ' +
            'advice String, ' + 'productversion String, ' +
            'packageversion String, ' + 'versionstr String, ' +
            'priority Integer, ' + 'producttype String, ' +
            'installationStatus String, ' + 'installedprodver String, ' +
            'installedpackver String, ' + 'installedverstr String, ' +
            'actionrequest String, ' + 'actionresult String, ' +
            'updatePossible String,' + 'hasSetup String, ' +
            'hasUninstall String, ' + 'possibleAction String);');
          logdatei.log('Finished products ', LLInfo);
        except
          on e: Exception do
          begin
            logdatei.log('Exception CREATE TABLE products', LLError);
            logdatei.log('Exception: ' + E.message, LLError);
            logdatei.log('Exception handled at: ' + getCallAddrStr, LLError);
            logdatei.log_exception(E,LLError);
          end;
        end;


        //Datamodule1.SQLTransaction1.Commit;
        try
          Datamodule1.SQLite3Connection1.ExecuteDirect(
            'CREATE TABLE dependencies (ProductId String not null, ' +
            'requiredProductId String, required String, ' +
            'prerequired String, postrequired String, ' +
            'PRIMARY KEY(ProductId,requiredProductId));');

          logdatei.log('Finished dependencies ', LLInfo);
        except
          on e: Exception do
          begin
            logdatei.log('Exception CREATE TABLE dependencies', LLError);
            logdatei.log('Exception: ' + E.message, LLError);
            logdatei.log('Exception handled at: ' + getCallAddrStr, LLError);
            logdatei.log_exception(E,LLError);
          end;
        end;

        try
          Datamodule1.SQLTransaction1.Commit;
        except
          on e: Exception do
          begin
            logdatei.log('Exception commit', LLError);
            logdatei.log('Exception: ' + E.message, LLError);
            logdatei.log('Exception handled at: ' + getCallAddrStr, LLError);
            logdatei.log_exception(E,LLError);
          end;
        end;

        if ZMQueryDataSet1.Active then
          ZMQueryDataSet1.Close;
        ZMQuerydataset1.SQL.Clear;
        ZMQuerydataset1.SQL.Add('select * from products order by Upper(ProductName)');
        ZMQuerydataset1.Open;
        if ZMQueryDataSet2.Active then
          ZMQueryDataSet2.Close;
        ZMQuerydataset2.SQL.Clear;
        ZMQuerydataset2.SQL.Add('select * from dependencies order by ProductId');
        ZMQuerydataset2.Open;
        logdatei.log('Finished initdb', LLInfo);

        //ShowMessage('Succesfully created database.');
      except
        //ShowMessage('Unable to Create new Database');
        on e: Exception do
        begin
          logdatei.log('Exception Unable to Create new Database', LLError);
          logdatei.log('Exception: ' + E.message, LLError);
          logdatei.log('Exception handled at: ' + getCallAddrStr, LLError);
          logdatei.log_exception(E,LLError);
        end;
      end;
    end;
  except
    //ShowMessage('Unable to check if database file exists');
    on e: Exception do
    begin
      logdatei.log('Exception check if database file exists', LLError);
      logdatei.log('Exception: ' + E.message, LLError);
      logdatei.log('Exception handled at: ' + getCallAddrStr, LLError);
      logdatei.log_exception(E,LLError);
    end;
  end;
  DataModule1.DataSource1.DataSet := ZMQuerydataset1;
  DataModule1.DataSource2.DataSet := ZMQuerydataset2;
  ZMQUerydataset2.DataSource := DataModule1.DataSource1;
  //FormOpsiClientKiosk.DBGrid1.DataSource := DataModule1.DataSource1;
  //FormOpsiClientKiosk.DBGrid2.DataSource := DataModule1.DataSource2;
  //FormOpsiClientKiosk.PanelProductDetail.Height := 0;
  with FormProgressWindow do
  begin
    LabelDataLoad.Caption := 'Initialize Database';
    //LabelDataLoadDetail.Caption := 'initdb';
    //Progressbar1.Position := 15;
    ProgressBar1.StepIt;
    ProgressbarDetail.Position := 0;
    ProcessMess;
    //ProgressBarDetail.Visible := False;
    //ProgressBar1.Visible := False;
    //LabelDataLoadDetail.Visible := False;
    //LabelDataLoad.Visible := False;
  end;
end;



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
  resultstring: string;
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


function initLogging(const clientname: string): boolean;
var
  i : integer;
begin
  Result := True;
  logdatei := TLogInfo.Create;
  logfilename := 'kiosk-' + GetUserName_ +'.log';
  LogDatei.WritePartLog := False;
  LogDatei.WriteErrFile:= False;
  LogDatei.WriteHistFile:= False;
  logdatei.CreateTheLogfile(logfilename, False);
  logdatei.LogLevel := myloglevel;
  for i := 0 to preLogfileLogList.Count-1 do
    logdatei.log(preLogfileLogList.Strings[i], LLessential);
  preLogfileLogList.Free;
  logdatei.log('opsi-client-kiosk: version: ' + myVersion, LLessential);
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

function initConnection(const seconds: integer): boolean;
var
  networkup, timeout: boolean;
  myseconds: integer;
  resultstring: string;
begin
  FormProgressWindow.LabelDataLoad.Caption := 'Connecting to OPSI web service';
  FormProgressWindow.ProcessMess;
  FormOpsiClientKiosk.Cursor := crHourGlass;
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
    myhostkey, '', '', '', 'opsi-client-kiosk-' + myVersion);
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
    FormOpsiClientKiosk.StatusBar1.Panels[0].Text :=
      'Connected to ' + myservice_url + ' as ' + myclientid;
    Result := True;
  end
  else
  begin
    LogDatei.log('init connection failed (timeout after ' + IntToStr(seconds) +
      ' seconds/retries.', LLError);
    FormOpsiClientKiosk.StatusBar1.Panels[0].Text := 'Connection failed';
  end;
  FormProgressWindow.ProgressBar1.StepIt;
  FormProgressWindow.ProcessMess;
  FormOpsiClientKiosk.Cursor := crArrow;
end;

procedure fetchProductData_by_getKioskProductInfosForClient;
var
  resultstring, groupstring, method, testresult: string;
  jOResult, new_obj, detail_obj: ISuperObject;
  i: integer;
  str, pid, depotid, pidliststr, reqtype: string;
  productdatarecord: TProductData;
begin
  logdatei.log('starting fetchProductData ....', LLInfo);
  try
    if ZMQueryDataSet1.Active then
      ZMQueryDataSet1.Close;
    ZMQuerydataset1.SQL.Clear;
    ZMQueryDataSet1.SQL.Add('delete from products');
    ZMQueryDataSet1.ExecSQL;
  except
    on e: Exception do
    begin
      logdatei.log('Exception delete from products', LLError);
      logdatei.log('Exception: ' + E.message, LLError);
      logdatei.log('Exception handled at: ' + getCallAddrStr, LLError);
      logdatei.log_exception(E,LLError);
    end;
  end;
  ZMQueryDataSet1.Filtered := False;
  //FormProgressWindow.ProgressBar1.StepIt;
  //FormProgressWindow.ProcessMess;
  if ZMQueryDataSet2.Active then
    ZMQueryDataSet2.Close;
  ZMQUerydataset2.SQL.Clear;
  ZMQueryDataSet2.SQL.Add('delete from dependencies');
  ZMQueryDataSet2.ExecSQL;
  //FormProgressWindow.ProgressBar1.StepIt;
  //FormProgressWindow.ProcessMess;

  if ZMQueryDataSet1.Active then
    ZMQueryDataSet1.Close;
  ZMQUerydataset1.SQL.Clear;
  ZMQUerydataset1.SQL.Add('select * from products order by Upper(PRODUCTName)');
  ZMQUerydataset1.Open;
  //FormProgressWindow.ProgressBar1.StepIt;
  //FormProgressWindow.ProcessMess;

  if ZMQueryDataSet2.Active then
    ZMQueryDataSet2.Close;
  ZMQUerydataset2.SQL.Clear;
  ZMQUerydataset2.SQL.Add('select * from dependencies order by ProductId');
  ZMQUerydataset2.Open;

  FormProgressWindow.ProgressBar1.StepIt;
  //FormOpsiClientKiosk.ProgressbarDetail.Max := productIdsList.Count;
  //FormOpsiClientKiosk.LabelDataLoad.Caption := 'Products';
  //FormOpsiClientKiosk.Progressbar1.Position := 5;
  FormProgressWindow.ProcessMess;
  //initConnection(30);
  //FormProgressWindow.ProgressBar1.StepIt;
  FormProgressWindow.LabelInfo.Caption := 'Loading data ...';

  resultstring := MyOpsiMethodCall('getKioskProductInfosForClient', [myclientid]);
  //closeConnection;
  new_obj := SO(resultstring).O['result'];
  //str := new_obj.AsString;
  LogDatei.log('Get products done', LLNotice);

  FormProgressWindow.LabelDataload.Caption := 'Fill Database';
  FormProgressWindow.ProgressbarDetail.Max := new_obj.AsArray.Length;
  FormProgressWindow.ProgressbarDetail.Min := 0;
  FormProgressWindow.ProgressbarDetail.Position := 0;
  FormProgressWindow.ProcessMess;

  // product data to database
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
    ZMQueryDataSet1.Append;
    ZMQueryDataSet1.FieldByName('ProductId').AsString := detail_obj.S['productId'];
    ZMQueryDataSet1.FieldByName('productVersion').AsString :=
      detail_obj.S['productVersion'];
    ZMQueryDataSet1.FieldByName('packageVersion').AsString :=
      detail_obj.S['packageVersion'];
    ZMQueryDataSet1.FieldByName('versionstr').AsString :=
      detail_obj.S['productVersion'] + '-' + detail_obj.S['packageVersion'];
    ZMQueryDataSet1.FieldByName('ProductName').AsString := detail_obj.S['productName'];
    ZMQueryDataSet1.FieldByName('description').AsString := detail_obj.S['description'];
    ZMQueryDataSet1.FieldByName('advice').AsString := detail_obj.S['advice'];
    ZMQueryDataSet1.FieldByName('priority').AsString := detail_obj.S['priority'];
    ZMQueryDataSet1.FieldByName('producttype').AsString := detail_obj.S['productType'];
    ZMQueryDataSet1.FieldByName('hasSetup').AsString := detail_obj.S['hasSetup'];
    ZMQueryDataSet1.FieldByName('hasUninstall').AsString :=
      detail_obj.S['hasUninstall'];
    if detail_obj.S['installationStatus'] = 'not_installed' then
      ZMQueryDataSet1.FieldByName('installationStatus').AsString := ''
    else
      ZMQueryDataSet1.FieldByName('installationStatus').AsString :=
        detail_obj.S['installationStatus'];
    ZMQueryDataSet1.FieldByName('installedprodver').AsString :=
      detail_obj.S['installedProdVer'];
    ZMQueryDataSet1.FieldByName('installedpackver').AsString :=
      detail_obj.S['installedPackVer'];
    ZMQueryDataSet1.FieldByName('installedverstr').AsString :=
      detail_obj.S['installedVerStr'];
    if detail_obj.S['actionRequest'] = 'none' then
      ZMQueryDataSet1.FieldByName('actionrequest').AsString := ''
    else
      ZMQueryDataSet1.FieldByName('actionrequest').AsString :=
        detail_obj.S['actionRequest'];
    ZMQueryDataSet1.FieldByName('actionresult').AsString := detail_obj.S['actionResult'];
    ZMQueryDataSet1.FieldByName('updatePossible').AsString :=
      detail_obj.S['updatePossible'];
    ZMQueryDataSet1.FieldByName('possibleAction').AsString :=
      detail_obj.S['possibleAction'];
    ZMQueryDataSet1.Post;


    // productDependencies
  end;

  ZMQueryDataSet1.Close;
  ZMQueryDataSet1.Open;
  ZMQueryDataSet1.First;
end;


procedure setActionrequest(pid: string; request: string);
var
  resultstring: string;
begin
  resultstring := MyOpsiMethodCall('setProductActionRequestWithDependencies',
    [pid, myclientid, request]);
end;

function getActionrequests: TStringList;
var
  resultstring, str: string;
  new_obj, detail_obj: ISuperObject;
  i: integer;
begin
  Result := TStringList.Create;
  resultstring := MyOpsiMethodCall('productOnClient_getObjects',
    ['[]', '{"clientId":"' + myclientid + '","actionRequest":["setup","uninstall"]}']);
  new_obj := SO(resultstring).O['result'];
  str := new_obj.AsString;
  for i := 0 to new_obj.AsArray.Length - 1 do
  begin
    detail_obj := new_obj.AsArray.O[i];
    Result.Add(detail_obj.S['productId'] + ' : ' + detail_obj.S['actionRequest"']);
  end;
end;

procedure firePushInstallation;
var
  resultstring, str: string;
begin
  // switch to opsiclientd mode if we on opsiconfd
  if not opsiclientdmode then readconf2;
  FreeAndNil(opsidata);
  initConnection(30);
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

procedure main;
var
  ErrorMsg: string;
  parameters: array of string;
  resultstring: string;
  i: integer;
  grouplist: TStringList;
begin
{*  FileVerInfo := TFileVersionInfo.Create(nil);
  try
    FileVerInfo.FileName := ParamStr(0);
    FileVerInfo.ReadFileInfo;
    myVersion := FileVerInfo.VersionStrings.Values['FileVersion'];
  finally
    FileVerInfo.Free;
  end;*}
  FormProgressWindow.LabelInfo.Caption := 'Please wait while connecting to service';
  FormProgressWindow.LabelDataload.Caption := 'Connect opsi Web Service';
  FormProgressWindow.ProcessMess;
  myexitcode := 0;
  myerror := '';
  if opsiclientdmode then readconf2
  else readconf;
  // opsiconfd mode
  //readconf;
  // opsiclientd mode
  //readconf2;
  // do not forget to check firePushInstallation
  initlogging(myclientid);
  LogDatei.log('clientid=' + myclientid, LLNotice);
  LogDatei.log('service_url=' + myservice_url, LLNotice);
  LogDatei.log('service_user=' + myclientid, LLNotice);
  logdatei.AddToConfidentials(myhostkey);
  LogDatei.log('host_key=' + myhostkey, LLdebug3);
  // is an other instance running ?
  if numberOfProcessInstances(ExtractFileName(ParamStr(0))) > 1 then
  begin
    LogDatei.log('A other instance of this program is running - so we abort', LLCritical);
    LogDatei.Close;
    halt(1);
  end;
  // is opsiclientd running ?
  if numberOfProcessInstances('opsiclientd') < 1 then
  begin
    LogDatei.log('opsiclientd is not running - so we abort', LLCritical);
    LogDatei.Close;
    ShowMessage('opsiclientd is not running - so we abort');
    halt(1);
  end;
  FormProgressWindow.ProgressBar1.StepIt;
  FormProgressWindow.ProcessMess;

  if initConnection(30) then
  begin
    LogDatei.log('init Connection done', LLNotice);
    initdb;
    FormProgressWindow.LabelInfo.Caption := 'Please wait while gettting products';
    LogDatei.log('start fetchProductData_by_getKioskProductInfosForClient', LLNotice);
    fetchProductData_by_getKioskProductInfosForClient;
    LogDatei.log('Handle products done', LLNotice);
    FormProgressWindow.LabelDataload.Caption := 'Handle Products';
    FormProgressWindow.ProcessMess;
  end
  else
  begin
    LogDatei.log('init Connection failed - Aborting', LLError);
    if opsidata <> nil then
      opsidata.Free;
    FormOpsiClientKiosk.Terminate;
    halt(1);
  end;
  LogDatei.log('main done', LLDebug2);
  FormProgressWindow.ProgressBar1.StepIt;
  FormProgressWindow.ProcessMess;
end;

end.
