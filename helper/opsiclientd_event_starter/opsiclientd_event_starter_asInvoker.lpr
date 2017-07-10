program opsiclientd_event_starter_asInvoker;

{$mode objfpc}{$H+}


uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  fileinfo,
  winpeimagereader,
  inifiles,
  shlobj,
  Windows,
  activex,
  oswebservice,
  osjson,
  oslog;

const
  opsiclientdconf = '\opsi.org\opsi-client-agent\opsiclientd\opsiclientd.conf';
  opsilog = 'opsiclientd_event_starter.log';
  SECONDS_TO_SLEEP_AFTER_ACTION = 5;
  //TIMEOUTSECONDS = 60;


type

  { Tstarter }

  Tstarter = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

var
  myexitcode, myloglevel: integer;
  myclientid, myhostkey, myerror, myservice_url: string;
  INI: TINIFile;
  logfilename: string;
  myuser, myencryptedpass, mypass, myshare, mydepot, mymountpoint,
  mydepotuser, mydomain: string;
  opsidata: Topsi4data;
  FileVerInfo : TfileVersionInfo;
  myVersion, myevent : String;
  second_counter : integer = 0;

procedure Freecsidl(csidl: PItemIDList);
var
  alloc: IMalloc;
begin
  if Succeeded(SHGetMalloc(alloc)) then
  begin
    alloc.Free(csidl);
  end;
end;


  function getSpecialFolder(csidlValue: integer): string;
  var
    csidl: pItemIDList;
    nameBuf: array [0..MAX_PATH] of widechar;
    //nameBuf: string ='';
  begin
    Result := '';
    if SUCCEEDED(SHGetSpecialFolderLocation(0, csidlValue, csidl)) then
    begin
      if csidl <> nil then
      begin
        if SHGetPathFromIDListW(csidl, namebuf) then
          Result := StrPas(namebuf);
        Freecsidl(csidl);
      end;
    end
    else
      //Fix:
      if assigned(@SHGetFolderPath) and ((csidlvalue = CSIDL_APPDATA) or
        (csidlvalue = CSIDL_PERSONAL)) then
      begin
        if SUCCEEDED(SHGetFolderPathW(0, csidlValue, 0, 0, (namebuf))) then
          Result := StrPas(namebuf);
      end;
  end;


procedure readconf;
var
  myini: TInifile;
  confname : string;
begin
  //confname := getSpecialFolder(CSIDL_PROGRAM_FILES) +opsiclientdconf;
  //myini := TIniFile.Create(confname);
  //myservice_url := myini.ReadString('config_service', 'url', '');
  //myservice_url := 'https://localhost:4441/interface';
  //myservice_url := 'https://localhost:4441';
  //myservice_url := 'https://localhost:4441/opsiclientd';
  myservice_url := 'https://localhost:4441/kiosk';
  myclientid := '';
  myhostkey := '';
  //myclientid := myini.ReadString('global', 'host_id', '');
  //myhostkey := myini.ReadString('global', 'opsi_host_key', '');
  //myloglevel := myini.ReadInteger('global', 'log_level', 5);
  myloglevel := 8;
  //myini.Free;
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
    Result := trim(resultstring);
  except
    LogDatei.log('Exception calling method: ' + method, LLerror);
    raise exception.create('opsiserviceall: '+method+' failed') at
      get_caller_addr(get_frame),
      get_caller_frame(get_frame);
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
    LogDatei.log('Exception calling method: ' + method, LLerror);
  end;
end;


function initLogging : boolean;
begin
  Result := True;
  logdatei := TLogInfo.Create;
  logfilename := opsilog;
  logdatei.CreateTheLogfile(logfilename, False);
  logdatei.LogLevel := myloglevel;
  logdatei.log('opsiclientd_event_starter_asInvoker: version: '+myVersion,LLessential);
end;


function initConnection(const seconds: integer): boolean;
var
  networkup, timeout: boolean;
  myseconds: integer;
  resultstring : string;
begin
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
  opsidata.initOpsiConf(myservice_url, myclientid, myhostkey);
  LogDatei.log('opsidata initialized', LLDebug2);
  repeat
    try
      if myseconds > 0 then
      begin
       //resultstring := MyOpsiMethodCall('backend_info',[]);
       resultstring := MyOpsiMethodCall('getDepotId', [myclientid]);
       networkup := True;
      end
      else timeout := True;
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
  end
  else
  begin
    LogDatei.log('init connection failed (timeout after ' + IntToStr(seconds) +
      ' seconds/retries.', LLError);
  end;
end;


{ Tstarter }

procedure Tstarter.DoRun;

var
  ErrorMsg: String;
  optionlist : TStringlist;
  resultstring, myBoolStr : string;

begin
  myevent := '';

  FileVerInfo:=TFileVersionInfo.Create(nil);
  try
    FileVerInfo.FileName:=paramstr(0);
    FileVerInfo.ReadFileInfo;
    myVersion := FileVerInfo.VersionStrings.Values['FileVersion'];
  finally
    FileVerInfo.Free;
  end;

  optionlist := TStringlist.Create;
  optionlist.Append('help');
  //optionlist.Append('event:');
  optionlist.Append('version');
  optionlist.Append('fqdn::');
  // quick check parameters
  ErrorMsg:= CheckOptions('h',optionlist);
  if ErrorMsg<>'' then begin
    writeln('Error: '+ErrorMsg);
    //ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('version') then begin
    writeln(myVersion);
    Terminate;
    Exit;
  end;
  readconf;
  if HasOption('fqdn') then
  begin
    myClientId := GetOptionValue('fqdn');
  end;



  if HasOption('event') then
  begin
    myevent := GetOptionValue('event');
  end;
(*
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;


  if myevent ='' then
  begin
    writeln('Error: No event given');
    WriteHelp;
    Terminate;
    Exit;
  end;
  *)

  { add your program here }
  myexitcode := 0;
  myerror := '';
  //readconf;
  initlogging;
  LogDatei.log('clientid=' + myclientid, LLNotice);
  LogDatei.log('service_url=' + myservice_url, LLNotice);
  LogDatei.log('service_user=' + myclientid, LLNotice);
  logdatei.AddToConfidentials(myhostkey);
  LogDatei.log('host_key=' + myhostkey, LLNotice);
  LogDatei.log('myevent=' + myevent, LLNotice);


  if initConnection(30) then
  begin
    LogDatei.log('init Connection done', LLNotice);
    try
      (*
      if trim(LowerCase(myEvent)) = 'on_shutdown' then
      begin
        resultstring := MyOpsiMethodCall('isInstallationPending',[]);
        LogDatei.log('resultstring=' + resultstring, LLNotice);
        if jsonIsObject(resultstring) then
        begin
          if not jsonAsObjectGetValueByKey(resultstring,'result',myBoolStr) then
            LogDatei.log('Error in jsonAsObjectGetValueByKey',LLCritical);
          if myBoolStr = 'true' then
          begin
            LogDatei.log('State installation pending detected, do not starting shutdown event.',LLCritical);
            Terminate;
            LogDatei.log('Terminate called.',LLCritical);
            Exit;
          end
          else LogDatei.log('No installation pending - fine', LLNotice);
        end
        else
        begin
          LogDatei.log('Error: Got no JSON object from service',LLCritical);
          Terminate;
        end;
      end;
      *)

      //resultstring := MyOpsiMethodCall('fireEvent',[myEvent]);
      resultstring := MyOpsiMethodCall('fireEvent_software_on_demand', []);
      LogDatei.log('resultstring=' + resultstring, LLNotice);
      if jsonIsObject(resultstring) then
      begin
        if not jsonAsObjectGetValueByKey(resultstring,'error',myBoolStr) then
            LogDatei.log('Error in jsonAsObjectGetValueByKey',LLCritical);
        if myBoolStr = 'null' then
        begin
          LogDatei.log('Succesfull fired fireEvent_software_on_demand ',LLNotice)
        end
        else
        begin
          LogDatei.log('Failed fired fireEvent_software_on_demand: ', LLCritical);
        end;
      end
      else
      begin
        LogDatei.log('Error: Got no JSON object from service: '+resultstring,LLCritical);
        Terminate;
      end;

      (*
      // event fired
      Sleep(SECONDS_TO_SLEEP_AFTER_ACTION * 1000);
      myBoolStr := 'false';
      repeat
        LogDatei.log('calling: ' + 'isEventRunning,['+myEvent+']', LLNotice);
        resultstring := MyOpsiMethodCall('isEventRunning',[myEvent]);
        LogDatei.log('resultstring=' + resultstring, LLNotice);
        if not jsonAsObjectGetValueByKey(resultstring,'result',myBoolStr) then
            LogDatei.log('Error in jsonAsObjectGetValueByKey',LLCritical);
        if myBoolStr = 'false' then
        begin
          LogDatei.log('calling: ' + 'isEventRunning,['+myEvent+'{user_logged_in}]', LLNotice);
          resultstring := MyOpsiMethodCall('isEventRunning',[myEvent+'{user_logged_in}']);
          LogDatei.log('resultstring=' + resultstring, LLNotice);
          if not jsonAsObjectGetValueByKey(resultstring,'result',myBoolStr) then
              LogDatei.log('Error in jsonAsObjectGetValueByKey',LLCritical);
        end;
        // check and sleep if one of the events running
        if myBoolStr = 'true' then
        begin
          Sleep(SECONDS_TO_SLEEP_AFTER_ACTION * 1000);
          second_counter := second_counter + SECONDS_TO_SLEEP_AFTER_ACTION;
        end;

      until (myBoolStr = 'false');
      //until (myBoolStr = 'false') or (second_counter > TIMEOUTSECONDS);
      //if (second_counter > TIMEOUTSECONDS) then
      //     LogDatei.log('Task aborted by timeout',LLWarning)
      //else
      LogDatei.log('Task completed',LLNotice)
      *)
    except
      on E: Exception do
      begin
        LogDatei.log('Exception in DoRun: '+E.Message,LLCritical);
        Terminate;
      end;
    end;
  end
  else
  begin
    // no cnnection
    Terminate;
  end;


  // stop program loop
  Terminate;
end;

constructor Tstarter.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor Tstarter.Destroy;
begin
  inherited Destroy;
end;

procedure Tstarter.WriteHelp;
begin
  { add your help code here }
  writeln(ExtractFileName(paramstr(0)) + ' Version: '+myVersion);
  writeln('Usage: ', ExtractFileName(ExeName), ' Option');
  writeln('Valid Options:');
  writeln(' -h / --help  : write this help');
  writeln(' --version    : write version string');
  //writeln(' --event=<event>    : starts the event <event>');
  //writeln('      <event> has to be given');
  //writeln('      Example --event=on_shutdown');
  writeln(' --fqdn=<fqdn>    : fqdn of the client');
  writeln('      May run without admin rights');
end;

var
  Application: Tstarter;

{$R *.res}
{$R manifest_asInvoker.rc}

begin
  Application:=Tstarter.Create(nil);
  Application.Run;
  Application.Free;
end.

