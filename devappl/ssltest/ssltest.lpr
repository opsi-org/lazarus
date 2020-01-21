program ssltest;

{$mode objfpc}{$H+}



uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  ssl_openssl_lib,
  oswebservice,
  //httpsend,
  //ssl_openssl,
  Classes,
  SysUtils,
  //fphttpclient,
  //fpopenssl,
  openssl,
  oslog,
  lazfileutils,
  {$IFDEF UNIX}
  baseunix,
  {$ENDIF UNIX}
  fpjson { you can add units after this };



procedure closelog;
begin
  LogDatei.log('Close the log', LLNotice);
  LogDatei.Close;
end;

procedure initlog;
var
  lfilename: string;
begin
  // Initialize logging
  // create the object
  LogDatei := TLogInfo.Create;
  // we do not need part logs
  //LogDatei.WritePartLog := False;
  // set the loglevel
  LogDatei.LogLevel := 8;
  // get the name for the log file
  lfilename := ExtractFileName(ParamStr(0));
  lfilename := ExtractFileNameWithoutExt(lfilename);
  // set the name for the log file
  // yes that is confusing and has to be cleaned up
  LogDatei.FileName := lfilename;
  LogDatei.StandardLogFileext := '.log';
  LogDatei.StandardLogFilename := lfilename;
  LogDatei.CreateTheLogfile(lfilename + '.log', True);
  // now we can start with the inital logentry
  LogDatei.log('Log for: ' + paramstr(0) + ' opend at : ' +
    DateTimeToStr(now), LLNotice);
  // now you can use logdatei everywhere if oslog is in the uses list
  // do not forget to close the log at the end of the program

  // The following part is not needed and only for your information:

  // some pre compiler definitions that influence the logpath
  // have a look at the end of the oslog.pas for mor information
  // have a look at project settings / compiler settings / custom settings
  // to create cutom defines ( WINDOWS or LINUX are defined automatically)
  {$IFDEF WINDOWS}
  LogDatei.log('WINDOWS is defined', LLNotice);
  {$IFDEF OPSI}
  LogDatei.log('OPSI is defined', LLNotice);
  {$IFDEF OPSI_AS_USER}
  LogDatei.log('OPSI_AS_USER is defined', LLNotice);
  {$ENDIF OPSI_AS_USER}
  {$ELSE OPSI}
  LogDatei.log('OPSI is not defined', LLNotice);
  {$ENDIF OPSI}
  {$ELSE WINDOWS}
  LogDatei.log('WINDOWS is not defined', LLNotice);
  if 0 = fpGetEUid then
  begin
    {$IFDEF OPSI}
    LogDatei.log('OPSI is defined and we running as root', LLNotice);
    {$ELSE OPSI}
    LogDatei.log('OPSI is not defined and we running as root', LLNotice);
    {$ENDIF OPSI}
  end
  else
  begin
    LogDatei.log('We are not running as root', LLNotice);
  end;
  {$ENDIF}
  LogDatei.log('Log path is: ' + LogDatei.FileName, LLNotice);
  LogDatei.LogLevel := 8;

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

function initConnection(const seconds: integer): boolean;
var
  networkup, timeout: boolean;
  myseconds: integer;
  resultstring: string;
  myservice_url, myclientid, myhostkey : string;
begin
  //FopsiClientKiosk.Cursor := crHourGlass;
  Result := False;
  networkup := False;
  timeout := False;
  myseconds := seconds;
  //myservice_url := 'https://192.168.1.14:4447/rpc';
  myservice_url := 'https://172.16.166.1:4447/rpc';
  //myclientid := 'pcbon4.uib.local';
  myclientid := 'win10x64.vmnat.local';
  //myhostkey := '379187d370b6aa9a7babe0b6c5f0bc64';
  myhostkey := 'b6d17719e8653437f3ec2e5276b9747f';

  LogDatei.log('service_url=' + myservice_url, LLDebug2);
  LogDatei.log('service_pass=' + myhostkey, LLDebug2);
  LogDatei.log('clientid=' + myclientid, LLDebug2);
  opsidata := TOpsi4Data.Create;
  LogDatei.log('opsidata created', LLDebug2);
  opsidata.setActualClient('pcbon4.uib.local');
  opsidata.initOpsiConf(myservice_url, myclientid,
    myhostkey, '', '', '', 'huh');
  LogDatei.log('opsidata initialized', LLDebug2);
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
      Sleep(1000);
    end;
  until networkup or timeout;
  if networkup then
  begin
    LogDatei.log('opsidata connected', LLInfo);
    Result := True;
    writeln('Connected to ' + myservice_url + ' as ' + myclientid);
  end
  else
  begin
    LogDatei.log('init connection failed (timeout after ' + IntToStr(seconds) +
      ' seconds/retries.', LLError);
    writeln('Connection failed');
  end;
  //FopsiClientKiosk.Cursor := crArrow;
end;


var
  t: TStringList;
  URL, errmsg: string;
  JSON: TJSonStringType;
  session, result : string;
begin

  initlog;
  //result := getOpsiServerVersion('https://192.168.0.3:4447/rpc','adminuser','linux123',session);
  result := getOpsiServerVersion('https://172.16.166.1:4447/rpc','adminuser','linux123',session);
  writeln('result: '+result);
  initConnection(30);
  if opsidata.sendLog then writeln('send done')
  else writeln('send failed');
  closelog;

end.
