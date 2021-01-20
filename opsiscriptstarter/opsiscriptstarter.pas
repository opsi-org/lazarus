unit opsiscriptstarter;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF UNIX}//{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}//{$ENDIF}
  //Interfaces, // this includes the LCL widgetset
  {$IFDEF LINUX}
  osfunclin,
  {$ENDIF}
  {$IFDEF DARWIN}
  osfuncmac,
  {$ENDIF}
  superobject,
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
  OSProcessux;

const
  SW_HIDE = 0;
  opsiclientdconf = '/etc/opsi-client-agent/opsiclientd.conf';
  opsiscriptbin = '/Applications/opsi-script.app/Contents/MacOS/opsi-script';
  opsiscriptnoguibin = '/usr/local/bin/opsi-script-nogui';
  opsiscriptstarterlog = 'opsiscriptstarter.log';


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
  bytearray = array[0..255] of byte;

implementation

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

function divideAtFirst(const partialS, S: string; var part1, part2: string): boolean;
  (* teilt den String S beim ersten Vorkommen des Teilstrings partialS;
     liefert true, wenn partialS vorkommt,
     andernfalls false;

     wenn partialS nicht vorkommt, enthaelt part1 den Gesamtstring, part2 ist leer *)

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


function startopsiscript: integer;
var
  cmd, report: string;
  outlines: TStringList;
  credentials: TStringList;
  ExitCode: longint;
  i: integer;
  credfilename: string;
begin
  outlines := TStringList.Create;
  credfilename := '/tmp/opsicredentials';
  credentials := TStringList.Create;
  credentials.Add('username=' + myclientid);
  credentials.Add('password=' + myhostkey);
  credentials.SaveToFile(credfilename);
  fpchmod(credfilename, &600);
  Result := 0;
  if nogui then
  begin
    cmd := '/bin/bash -c " ' + opsiscriptnoguibin + ' -opsiservice ' +
      myservice_url + ' -clientid ' +
      myclientid + ' -credentialfile ' + credfilename + '"';
    //                         +' -username '+ myclientid
    //                         +' -password ' + myhostkey+' &> /dev/tty1"';
  end
  else
  begin
    cmd := '/bin/bash -c " export DISPLAY=:0 ; ' + opsiscriptbin +
      ' -opsiservice ' + myservice_url + ' -clientid ' +
      myclientid + ' -credentialfile ' + credfilename + '"';
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
  //fpunlink(credfilename);
  outlines.Free;
  credentials.Free;
end;


procedure transformHex
  (const hexstring: string; var hexarray: bytearray);
var
  section: string;
  thesize: integer;
  onehex: string;
  i: integer;
begin
  theSize := length(hexstring) div 2;
  section := hexstring;
  for i := 0 to theSize - 1 do
  begin
    onehex := copy(section, 1, 2);
    HexToBin(PChar(onehex), @hexarray[i], 1);
    section := copy(section, 3, length(section));
  end;
end;

function decrypt(hexkey, hexpass: string): string;
var
  passByte, keyByte: bytearray;
  passByteDecry: bytearray;
  passSize: integer;
  keySize: integer;
  i: integer;
  Cipher: TDCP_blowfish;
begin
  transformHex(hexkey, keyByte);
  transformHex(hexpass, passByte);

  passSize := length(hexpass) div 2;
  keySize := length(hexkey) div 2;

  //DCP_blowfish1.Init(keyByte,keySize*8,PChar('OPSI1234'));
  //DCP_blowfish1.Decrypt(passByte,passByteDecry,passSize);

  Cipher := TDCP_blowfish.Create(nil);
  Cipher.Init(keyByte, keySize * 8, PChar('OPSI1234')); // remember key size is in BITS

  Cipher.DecryptCBC(passByte, passByteDecry, passSize);
  Cipher.Burn;
  Cipher.Free;

  Result := '';
  for i := 0 to passSize - 1 do
    Result := Result + char(passByteDecry[i]);
end;


function DecryptBlowfish(const myencrypted, mykey: string): string;
var
  Cipher: TDCP_blowfish;
  Key: array[0..15] of byte; // key can be any size upto 448bits with blowfish (56bytes)
  //Buffer: array[0..127] of byte; // buffer can be any size
  s: string;
  i: integer;
begin
  //Cipher.Reset;
  //Cipher.DecryptCBC(Buffer,Buffer,Sizeof(Buffer));
  s := myencrypted;
  for i := 0 to 15 do
  begin
    key[i] := StrToInt('$' + mykey[i * 2 + 1] + mykey[i * 2 + 2]);
  end;
  Cipher := TDCP_blowfish.Create(nil);
  Cipher.Init(Key, Sizeof(Key) * 8, nil); // remember key size is in BITS

  Cipher.DecryptCBC(s[1], s[1], Length(s));
  Cipher.Burn;
  Cipher.Free;
  Result := s;
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

function MyOpsiMethodCall(const method: string; parameters: array of string): string;
var
  omc: TOpsiMethodCall;
  errorOccured: boolean;
  resultstring: string;
begin
  Result := '';
  try
    //if param = '' then
    omc := TOpsiMethodCall.Create(method, parameters);
    //else
    //  omc := TOpsiMethodCall.Create(method,[param]);
    resultstring := opsidata.checkAndRetrieve(omc, errorOccured);
    Result := resultstring;
  except
    //LogDatei.DependentAdd('Exception calling method: '+method+'  - with param: '+param, LLerror);
    LogDatei.DependentAdd('Exception calling method: ' + method +
      ' - wait and retry', LLerror);
    sleep(2000);
    try
      resultstring := opsidata.checkAndRetrieve(omc, errorOccured);
      Result := resultstring;
    except
      LogDatei.DependentAdd('Exception calling method: ' + method +
        ' - wait and retry', LLerror);
      sleep(2000);
      try
        resultstring := opsidata.checkAndRetrieve(omc, errorOccured);
        Result := resultstring;
      except
        LogDatei.DependentAdd('Exception calling method: ' + method +
          ' - wait and retry', LLerror);
        sleep(2000);
        try
          resultstring := opsidata.checkAndRetrieve(omc, errorOccured);
          Result := resultstring;
        except
          LogDatei.DependentAdd('Exception calling method: ' + method +
            ' - wait and retry', LLerror);
          sleep(2000);
          try
            resultstring := opsidata.checkAndRetrieve(omc, errorOccured);
            Result := resultstring;
          except
            LogDatei.DependentAdd('Exception calling method: ' +
              method + ' - giving up', LLerror);
          end;
        end;
      end;
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
  outstr: string;
  opsiclientd_conf: string;
  myconf : TIniFile;
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
  //mymountpoint := '/Volumes/opsi_depot';
  mymountpoint := '/var/opsisetupadmin/opsi_depot';

  opsiclientd_conf := '/etc/opsi-client-agent/opsiclientd.conf';
  if FileExists(opsiclientd_conf) then
  begin
    myconf := TIniFile.Create(opsiclientd_conf);
    outstr := myconf.ReadString('depot_server', 'drive', mymountpoint);
    if DirectoryExists(outstr) then
      mymountpoint := outstr;
    myconf.Free;
  end;
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
    logdatei.log('Running in nogui mode', LLNotice);

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
        LogDatei.DependentAdd('action' + IntToStr(i) + ': ' + actionlist.Strings[i], LLNotice);
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
      mydepot := opsidata.depotId;
      writeln(FormatDateTime('mmm dd hh:nn:ss:zzz', Now) + ' depotId=', mydepot);
      //resultstring := MyOpsiMethodCall2('configState_getClientToDepotserver', ['[]','['+myclientid+']','True','["acroread", "config-win-base"]']);
      //myDepot := SO(resultstring).S['depotId'];
      resultstring := MyOpsiMethodCall('getGeneralConfigValue',
        ['clientconfig.depot.user', myclientid]);
      mydepotuser := SO(resultstring).S['result'];
      LogDatei.log('Got depot user from service: ' + mydepotuser, LLNotice);
      if mydepotuser <> '' then
        if divideAtFirst('\', mydepotuser, mydomain, myuser) then
      ;
      LogDatei.log('Will use as domain: ' + mydomain + ' as user: ' + myuser, LLNotice);
      resultstring := MyOpsiMethodCall('user_getCredentials', ['pcpatch', myclientid]);
      myencryptedpass := SO(resultstring).O['result'].S['password'];
      mypass := decrypt(myhostkey, myencryptedpass);
      //writeln('mypass=',mypass);
      resultstring := MyOpsiMethodCall('host_getObjects',
        ['', '{"type":"OpsiDepotserver","id":["' + myDepot + '"]}']);
      myshare := SO(resultstring).S['result'];
      myshare := SO(resultstring).O['result'].AsArray.O[0].S['depotRemoteUrl'];
      myshare := copy(myshare, 5, length(myshare));
      writeln(FormatDateTime('mmm dd hh:nn:ss:zzz', Now) + ' myshare=', myshare);
      umount(mymountpoint);
      logdatei.AddToConfidentials(mypass);
      mounttry := 0;
      repeat
        if (mounttry div 3) = 0 then
          mountoption := ' vers=3.0,';
        if (mounttry div 3) = 1 then
          mountoption := ' vers=2.0,';
        if (mounttry div 6) = 1 then
          mountoption := ' vers=1.0,';
        if (mounttry div 9) = 1 then
          mountoption := '';
        errorcode := mountSmbShare(mymountpoint, myshare, mydomain,
          myuser, mypass, mountoption);
        if (errorcode <> 0) or (not isMounted(mymountpoint)) then
        begin
          Inc(mounttry);
          LogDatei.log('Failed to mount ' + myshare + ' with option: ' +
            mountoption + ' to ' + mymountpoint + ' Error code: ' + IntToStr(errorcode) +
            ' - retry ...', LLWarning);
          sleep(2000);
        end;
      until isMounted(mymountpoint) or (mounttry > 12);
      if not isMounted(mymountpoint) then
        LogDatei.log('Failed to mount ' + myshare + ' to ' + mymountpoint +
          ' - abort!', LLCritical)
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



end.
