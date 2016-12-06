program opsiclientd_shutdown_starter;

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
  oswebservice,
  oslog,
  shlobj,
  Windows,
  activex

  { you can add units after this };

const
  opsiclientdconf = '\opsi.org\opsi-client-agent\opsiclientd\opsiclientd.conf';
  opsilog = 'opsiclientd_shutdown_starter.log';


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
  myVersion : String;

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
  confname := getSpecialFolder(CSIDL_PROGRAM_FILES) +opsiclientdconf;
  myini := TIniFile.Create(confname);
  //myservice_url := myini.ReadString('config_service', 'url', '');
  //myservice_url := 'https://localhost:4441/interface';
  myservice_url := 'https://localhost:4441';
  myclientid := myini.ReadString('global', 'host_id', '');
  myhostkey := myini.ReadString('global', 'opsi_host_key', '');
  //myloglevel := myini.ReadInteger('global', 'log_level', 5);
  myloglevel := 7;
  myini.Free;
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
    LogDatei.log('Exception calling method: ' + method, LLerror);
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
  oslog.CreateTheLogfile(logfilename, False);
  logdatei.LogLevel := myloglevel;
  logdatei.log('opsiclientd_shutdown_starter: version: '+myVersion,LLessential);
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
       resultstring := MyOpsiMethodCall('backend_info',[]);
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


begin
  FileVerInfo:=TFileVersionInfo.Create(nil);
  try
    FileVerInfo.FileName:=paramstr(0);
    FileVerInfo.ReadFileInfo;
    myVersion := FileVerInfo.VersionStrings.Values['FileVersion'];
  finally
    FileVerInfo.Free;
  end;

  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }
  myexitcode := 0;
  myerror := '';
  readconf;
  initlogging;
  LogDatei.log('clientid=' + myclientid, LLNotice);
  LogDatei.log('service_url=' + myservice_url, LLNotice);
  LogDatei.log('service_user=' + myclientid, LLNotice);
  logdatei.AddToConfidentials(myhostkey);
  LogDatei.log('host_key=' + myhostkey, LLNotice);


  if initConnection(30) then
  begin
    LogDatei.log('init Connection done', LLNotice);
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
  writeln('Usage: ', ExtractFileName(ExeName), ' -h');

end;

var
  Application: Tstarter;

{$R *.res}

begin
  Application:=Tstarter.Create(nil);
  Application.Run;
  Application.Free;
end.

