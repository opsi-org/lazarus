program opsiscriptstarter;

{$mode objfpc}{$H+}
//{$mode delphi}{$H+}
//{$apptype console}

uses {$IFDEF UNIX} //{$IFDEF UseCThreads}
  cthreads, {$ENDIF}//{$ENDIF}
  //Interfaces, // this includes the LCL widgetset
 {$IFDEF LINUX}
  osfunclin, {$ENDIF} {$IFDEF DARWIN}
  osfuncmac, {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  process,
  blowfish,
  DCPblowfish,
  DCPcrypt2,
  DCPblockciphers,
  baseunix,
  fileinfo,
  elfreader,
  inifiles,
  oslog,
  oswebservice,
  superobject,
  OSProcessux,
  oslinmount;

const
{$IFDEF LINUX}
  // SW_HIDE = 0;
  opsiclientdconf = '/etc/opsi-client-agent/opsiclientd.conf';
  opsiscriptbin = '/usr/bin/opsi-script';
  opsiscriptnoguibin = '/usr/bin/opsi-script-nogui';
  opsiscriptstarterlog = 'opsiscriptstarter.log';
{$ENDIF}
{$IFDEF DARWIN}
  SW_HIDE = 0;
  opsiclientdconf = '/etc/opsi-client-agent/opsiclientd.conf';
  opsiscriptbin = '/Applications/opsi-script.app/Contents/MacOS/opsi-script';
  opsiscriptnoguibin = '/usr/local/bin/opsi-script-nogui';
  opsiscriptstarterlog = 'opsiscriptstarter.log';
{$ENDIF}

type

  Tmythread = class(TThread)
  public
    procedure Execute; override;
  end;

  { Topsiscriptstarter }

  Topsiscriptstarter = class(TCustomApplication)
  protected
    mythread: Tmythread;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { Topsiscriptstarter }
  //bytearray = array[0..255] of byte;

var
  optionlist: TStringList;
  myexitcode, myloglevel: integer;
  myclientid, myhostkey, myerror, myservice_url: string;
  INI: TINIFile;
  //opsidata : TOpsi4Data;
  logfilename: string;
  myuser, myencryptedpass, mypass, myshare, mydepot, mymountpoint,
  mydepotuser, mydomain: string;
  mountresult: dword;
  nogui: boolean;
  myVersion: string;

  procedure Tmythread.Execute;
  begin
    sleep(31000);
    // if we called the terminate method we do nothing
    if not Terminated then
    begin
      ;
      LogDatei.DependentAdd('network timeout by thread - aborting program', LLInfo);
      writeln(FormatDateTime('mmm dd hh:nn:ss:zzz', Now) +
        ' network timeout by thread - aborting program');
      halt(0);
    end;
  end;

  function startopsiscript: integer;
  var
    cmd, report: string;
    outlines: TStringList;
    credentials: TStringList;
    ExitCode: longint;
    i: integer;
    credfilename: string;
    redirection: string;
  begin
    outlines := TStringList.Create;
    credfilename := '/tmp/opsicredentials';
    credentials := TStringList.Create;
    credentials.Add('username=' + myclientid);
    credentials.Add('password=' + myhostkey);
    credentials.SaveToFile(credfilename);
    fpchmod(credfilename, &600);
    Result := 0;
{$IFDEF LINUX}
    redirection := ' &> /dev/tty1"';
{$ENDIF}
{$IFDEF DARWIN}
    redirection := '"';
{$ENDIF}
    if nogui then
    begin
      cmd := '/bin/bash -c " ' + opsiscriptnoguibin + ' -opsiservice ' +
        myservice_url + ' -clientid ' + myclientid +
        ' -credentialfile ' + credfilename + redirection;
      //                         +' -username '+ myclientid
      //                         +' -password ' + myhostkey+' &> /dev/tty1"';
    end
    else
    begin
      cmd := '/bin/bash -c " export DISPLAY=:0 ; ' + opsiscriptbin +
        ' -opsiservice ' + myservice_url + ' -clientid ' + myclientid +
        ' -credentialfile ' + credfilename + '"';
      //                         +' -username '+ myclientid
      //                         +' -password ' + myhostkey+'"';
    end;
    LogDatei.DependentAdd('calling: ' + cmd, LLNotice);
    if not RunCommandAndCaptureOut(cmd, True, outlines, report, SW_HIDE, ExitCode) then
    begin
      LogDatei.log('Error: ' + Report + 'Exitcode: ' + IntToStr(ExitCode), LLError);
      Result := -1;
    end
    else
    begin
      LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 6;
      LogDatei.log('', LLDebug);
      LogDatei.log('output:', LLDebug);
      LogDatei.log('--------------', LLDebug);
      for i := 0 to outlines.Count - 1 do
      begin
        LogDatei.log(outlines.strings[i], LLDebug);
      end;
      LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 6;
      LogDatei.log('', LLDebug);
    end;
    fpunlink(credfilename);
    outlines.Free;
    credentials.Free;
  end;


  procedure readconf;
  var
    myini: TInifile;
  begin
    myini := TIniFile.Create(opsiclientdconf);
    myservice_url := myini.ReadString('config_service', 'url', '');
    myclientid := myini.ReadString('global', 'host_id', '');
    myhostkey := myini.ReadString('global', 'opsi_host_key', '');
    myloglevel := myini.ReadInteger('global', 'log_level', 5);
    myini.Free;
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
      //if param = '' then
      omc := TOpsiMethodCall.Create(method, parameters);
      //else
      //  omc := TOpsiMethodCall.Create(method,[param]);
      //resultstring :=  opsidata.checkAndRetrieveString(omc,errorOccured);
      resultstringlist := TStringList.Create;
      resultstringlist := opsidata.checkAndRetrieveList(omc, errorOccured);
      for i := 0 to resultstringlist.Count - 1 do
        Result := resultstringlist[i];

    except
      //LogDatei.DependentAdd('Exception calling method: '+method+'  - with param: '+param, LLerror);
      LogDatei.DependentAdd('Exception calling method: ' + method, LLerror);
    end;
  end;

  function initLogging(const clientname: string): boolean;
  begin
    Result := True;
    logdatei := TLogInfo.Create;
    logfilename := opsiscriptstarterlog;
    logdatei.CreateTheLogfile(logfilename, False);
    logdatei.LogLevel := myloglevel;
  end;


  function initConnection(const seconds: integer): boolean;
  var
    networkup, timeout: boolean;
    myseconds: integer;
  begin
    Result := False;
    networkup := False;
    timeout := False;
    myseconds := seconds;
    LogDatei.DependentAdd('service_url=' + myservice_url, LLDebug2);
    LogDatei.DependentAdd('service_pass=' + myhostkey, LLDebug2);
    LogDatei.DependentAdd('clientid=' + myclientid, LLDebug2);
    opsidata := TOpsi4Data.Create;
    LogDatei.DependentAdd('opsidata created', LLDebug2);
    opsidata.setActualClient(myclientid);
    opsidata.initOpsiConf(myservice_url, myclientid, myhostkey);
    LogDatei.DependentAdd('opsidata initialized', LLDebug2);
    repeat
      try
        if opsidata.isConnected then
          networkup := True
        else
        begin
          LogDatei.DependentAdd('opsidata not connected - retry', LLInfo);
          writeln(FormatDateTime('mmm dd hh:nn:ss:zzz', Now) +
            ' opsidata not connected - retry');
          myseconds := myseconds - 1;
          Sleep(1000);
        end;
      except
        LogDatei.DependentAdd('opsidata not connected - retry', LLInfo);
        writeln(FormatDateTime('mmm dd hh:nn:ss:zzz', Now) +
          ' opsidata not connected - retry');
        myseconds := myseconds - 1;
        Sleep(1000);
      end;
      if myseconds = 0 then
        timeout := True;
    until networkup or timeout;
    if networkup then
    begin
      LogDatei.DependentAdd('opsidata connected', LLInfo);
      writeln(FormatDateTime('mmm dd hh:nn:ss:zzz', Now) + ' opsidata connected');
      Result := True;
    end
    else
    begin
      LogDatei.DependentAdd('init connection failed (timeout after ' +
        IntToStr(seconds) + ' seconds/retries.', LLError);
      writeln(FormatDateTime('mmm dd hh:nn:ss:zzz', Now) +
        ' init connection failed (timeout after ' + IntToStr(seconds) + ' seconds/retries.');
    end;
  end;


  procedure Topsiscriptstarter.DoRun;
  var
    ErrorMsg: string;
    parameters: array of string;
    //parameterlist : TStringlist;
    resultstring: string;
    actionlist: TStringList;
    foundActionRequest: boolean;
    i: integer;
    FileVerInfo: TFileVersionInfo;
    mounttry: integer;
    mountoption: string;
  begin
    myexitcode := 0;
    myerror := '';
    myDepot := '';
    myuser := 'pcpatch';
    mydomain := '';
    myshare := 'opsi_depot';
  {$IFDEF LINUX}
    mymountpoint := '/media/opsi_depot';
  {$ENDIF}
  {$IFDEF DARWIN}
    //mymountpoint := '/Network/opsi_depot';
    mymountpoint := '/Volumes/opsi_depot';
  {$ENDIF}
    nogui := False;
    FileVerInfo := TFileVersionInfo.Create(nil);
    try
      FileVerInfo.FileName := ParamStr(0);
      FileVerInfo.ReadFileInfo;
      myVersion := FileVerInfo.VersionStrings.Values['FileVersion'];
    finally
      FileVerInfo.Free;
    end;


    optionlist := TStringList.Create;
    optionlist.Append('help');
    optionlist.Append('nogui');
    // quick check parameters
    ErrorMsg := CheckOptions('', optionlist);
    if ErrorMsg <> '' then
    begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;



    // parse parameters
    if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;
    if HasOption('nogui') then
    begin
      nogui := True;
    end;

    readconf;
    initlogging(myclientid);
    logdatei.log('Starting opsiscriptstarter version: ' + myVersion, LLNotice);
    if nogui then
    begin
      logdatei.log('Running in nogui mode', LLNotice);
      writeln(FormatDateTime('mmm dd hh:nn:ss:zzz', Now) +
        ' opsiscriptstarter (nogui) version: ' + myVersion);
    end
    else
    begin
      logdatei.log('Running in gui mode', LLNotice);
      writeln(FormatDateTime('mmm dd hh:nn:ss:zzz', Now) +
        ' opsiscriptstarter (gui) version: ' + myVersion);
    end;


    writeln(FormatDateTime('mmm dd hh:nn:ss:zzz', Now) + ' clientid=' + myclientid);
    writeln(FormatDateTime('mmm dd hh:nn:ss:zzz', Now) + ' service_url=' + myservice_url);
    writeln(FormatDateTime('mmm dd hh:nn:ss:zzz', Now) + ' service_user=' + myclientid);
    //writeln('host_key=',myhostkey);
    logdatei.AddToConfidentials(myhostkey);
    writeln(FormatDateTime('mmm dd hh:nn:ss:zzz', Now) + ' log_level=', myloglevel);
    mythread := Tmythread.Create(False);
    if initConnection(30) then
    begin
      mythread.Terminate;
      writeln(FormatDateTime('mmm dd hh:nn:ss:zzz', Now) + ' init done');
      LogDatei.log('init done', LLNotice);
      LogDatei.log('Starting opsiclientd part:', LLNotice);
      opsidata.setActualClient(myclientid);
      foundActionRequest := False;
      actionlist := opsidata.getMapOfProductActionRequests;
      LogDatei.log('Number of POC entries: ' + IntToStr(actionlist.Count), LLNotice);
      if actionlist.Count > 0 then
        for i := 0 to actionlist.Count - 1 do
        begin
          LogDatei.DependentAdd('action' + IntToStr(i) + ': ' +
            actionlist.Strings[i], LLNotice);
          if not (actionlist.ValueFromIndex[i] = 'none') then
            foundActionRequest := True;
        end;
      if not foundActionRequest then
      begin
        LogDatei.DependentAdd('No action requests - nothing to do', LLNotice);
        writeln(FormatDateTime('mmm dd hh:nn:ss:zzz', Now) +
          ' No action requests - nothing to do');
      end
      else
      begin
        LogDatei.DependentAdd('Action requests found', LLNotice);
        writeln(FormatDateTime('mmm dd hh:nn:ss:zzz', Now) + ' Action requests found');
        opsidata.setActualClient(myclientid);
        mount_depotshare(mymountpoint, myhostkey, myclientId);
        if not isMounted(mymountpoint) then
          LogDatei.log('Failed to mount ' + myshare + ' to ' +
            mymountpoint + ' - abort!', LLCritical)
        else
        begin
          writeln(FormatDateTime('mmm dd hh:nn:ss:zzz', Now) +
            ' share mounted - starting action processor...');
          startopsiscript;
          writeln(FormatDateTime('mmm dd hh:nn:ss:zzz', Now) + ' action processor finished');
          umount(mymountpoint);
          writeln(FormatDateTime('mmm dd hh:nn:ss:zzz', Now) + ' share unmounted');
        end;
      end;
      opsidata.sendLog('clientconnect');
    end
    else
    begin
      //writeln('init failed');
      LogDatei.log('init failed', LLError);
    end;
    // stop program loop
    Terminate;
  end;

  constructor Topsiscriptstarter.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor Topsiscriptstarter.Destroy;
  begin
    inherited Destroy;
  end;

  procedure Topsiscriptstarter.WriteHelp;
  var
    filename: string;
  begin
    filename := ExtractFileName(ParamStr(0));
    writeln(ParamStr(0));
    writeln(filename);
    writeln('Version: ' + myVersion);
    writeln(myerror);
    writeln(Title);
    writeln('Usage:');
    writeln(filename + ' --help');
    writeln('Options:');
    writeln(' --help -> write this help and exit');
    writeln(' --nogui -> starts opsi-script-nogui instead of opsi-script');
    Terminate;
    halt(0);
    Exit;
  end;

(*
procedure Topsiscriptstarter.terminateByTimer(Sender:TObject);
begin
  LogDatei.DependentAdd('Terminated by timeout Timer',LLInfo);
  LogDatei.Close;
  writeln('Terminated by timeout Timer');
  Terminate;
  halt(1);
  exit;
end;
*)

var
  Application: Topsiscriptstarter;

{$R *.res}

begin
  Application := Topsiscriptstarter.Create(nil);
  Application.Title:='opsi-script-starter';
  Application.Run;
  Application.Free;
end.
