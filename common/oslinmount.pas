unit oslinmount;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF OPSISCRIPT}
  ostxstringlist,
  {$ENDIF OPSISCRIPT}
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
  osprocessux,
  osjson,
  superobject;

const
  SW_HIDE = 0;

type
  bytearray = array[0..255] of byte;

function isMounted(mountpoint: string): boolean;
function mountSmbShare(mymountpoint, myshare, mydomain, myuser, mypass, myoption: string)
  : integer;
function umount(mymountpoint: string): integer;
procedure transformHex(const hexstring: string; var hexarray: bytearray);
function decrypt(hexkey, hexpass: string): string;
function DecryptBlowfish(const myencrypted, mykey: string): string;
procedure mount_depotshare(mymountpoint: string; myhostkey: string; myclientId: string);

implementation

function divideAtFirst(const partialS, S: string; var part1, part2: string): boolean;
  { teilt den String S beim ersten Vorkommen des Teilstrings partialS;
     liefert true, wenn partialS vorkommt,
     andernfalls false;

     wenn partialS nicht vorkommt, enthaelt part1 den Gesamtstring, part2 ist leer }

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


function isMounted(mountpoint: string): boolean;
var
  output: string;
  exename: string;
  commands: array of string;
begin
  Result := False;
  if not RunCommand('/usr/bin/which', ['findmnt'], output) then
    writeln('Could not find mount binary')
  else
  begin
    exename := output;
    exename := exename.Replace(#10, '');
    exename := exename.Replace('''', '');
  end;
  if RunCommand(exename, [mountpoint], output) then
  begin
    Result := True;
  end;
end;

function mountSmbShare(mymountpoint, myshare, mydomain, myuser, mypass,
  myoption: string): integer;
var
  cmd, report: string;
  {$IFDEF OPSISCRIPT}
  outlines: TXStringList;
  {$ELSE OPSISCRIPT}
  outlines: TStringList;
  {$ENDIF OPSISCRIPT}
  ExitCode: longint;
  i: integer;
begin
  try
    {$IFDEF OPSISCRIPT}
    outlines :=
      TXStringList.Create;
    {$ELSE OPSISCRIPT}
    outlines :=
      TStringList.Create;
    {$ENDIF OPSISCRIPT}
    Result := -1;
    try
      if not directoryexists(mymountpoint) then
        mkdir(mymountpoint);
    except
      LogDatei.log('Error: could not create moutpoint: ' + mymountpoint, LLError);
    end;
    if mydomain = '' then
      cmd := '/bin/bash -c "/sbin/mount.cifs ' + myshare + ' ' +
        mymountpoint + ' -o ' + myoption + 'ro,noperm,user=' + myuser +
        ',pass=' + mypass + '"'
    else
      cmd := '/bin/bash -c "/sbin/mount.cifs ' + myshare + ' ' +
        mymountpoint + ' -o ' + myoption + 'ro,noperm,user=' + myuser +
        ',dom=' + mydomain + ',pass=' + mypass + '"';
    Logdatei.Log('calling: ' + cmd, LLNotice);
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
      Result := ExitCode;
    end;
    outlines.Free;
  except
    on e: Exception do
    begin
      LogDatei.log('Exception in oslinmount: mountSmbShare: ' +
        e.message, LLError);
    end;
  end;
end;


function umount(mymountpoint: string): integer;
var
  cmd, report: string;
  {$IFDEF OPSISCRIPT}
  outlines: TXStringList;
  {$ELSE OPSISCRIPT}
  outlines: TStringList;
  {$ENDIF OPSISCRIPT}
  ExitCode: longint;
  i: integer;
begin
  {$IFDEF OPSISCRIPT}
  outlines :=
    TXStringList.Create;
    {$ELSE OPSISCRIPT}
  outlines :=
    TStringList.Create;
    {$ENDIF OPSISCRIPT}
  Result := -1;
  cmd := '/bin/bash -c "/bin/umount ' + mymountpoint + '"';
  LogDatei.log('calling: ' + cmd, LLNotice);
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
    Result := ExitCode;
  end;
  outlines.Free;
end;


procedure transformHex(const hexstring: string; var hexarray: bytearray);
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
    //Logdatei.Log('Exception calling method: '+method+'  - with param: '+param, LLerror);
    Logdatei.Log('Exception calling method: ' + method +
      ' - wait and retry', LLerror);
    sleep(2000);
    try
      resultstring := opsidata.checkAndRetrieve(omc, errorOccured);
      Result := resultstring;
    except
      Logdatei.Log('Exception calling method: ' + method +
        ' - wait and retry', LLerror);
      sleep(2000);
      try
        resultstring := opsidata.checkAndRetrieve(omc, errorOccured);
        Result := resultstring;
      except
        Logdatei.Log('Exception calling method: ' + method +
          ' - wait and retry', LLerror);
        sleep(2000);
        try
          resultstring := opsidata.checkAndRetrieve(omc, errorOccured);
          Result := resultstring;
        except
          Logdatei.Log('Exception calling method: ' + method +
            ' - wait and retry', LLerror);
          sleep(2000);
          try
            resultstring := opsidata.checkAndRetrieve(omc, errorOccured);
            Result := resultstring;
          except
            Logdatei.Log('Exception calling method: ' +
              method + ' - giving up', LLerror);
          end;
        end;
      end;
    end;
  end;
end;


procedure mount_depotshare(mymountpoint: string; myhostkey: string; myclientId: string);
var
  resultstring, mydepotuser, mydomain, mydepot: string;
  myuser, myencryptedpass, myshare, mypass, mountoption: string;
  mounttry, i: integer;
  configlist: TStringList;
  parastr , configid, values: string;
begin
  try
    configlist := TStringList.Create;
    mydepot := opsidata.depotId;
    myuser := 'pcpatch';
    mydomain := '';
    //writeln('depotId=',mydepot);
    if opsidata.setAddConfigStateDefaults(True) then
    begin
      parastr := '{"objectId": "' + myclientid + '","configId":"clientconfig.depot.user"}';
      resultstring := MyOpsiMethodCall('configState_getObjects', ['', parastr]);
      if jsonIsValid(resultstring) then
      begin
        //'got valid json object from getOpsiServiceConfigs'
        if jsonAsObjectGetValueByKey(resultstring, 'result', resultstring) then
          if jsonIsArray(resultstring) then
          begin
            //'got json Array from result'
            if jsonAsArrayToStringList(resultstring, configlist) then
            begin
              for i := 0 to configlist.Count - 1 do
              begin
                if jsonIsObject(configlist.Strings[i]) then
                begin
                  if jsonAsObjectGetValueByKey(configlist.Strings[i],
                    'configId', configid) then
                  begin
                    //'got configid: ' + configid

                    if LowerCase(configid) = 'clientconfig.depot.user' then
                    begin
                      if jsonAsObjectGetValueByKey(configlist.Strings[i],
                        'values', values) then
                        if jsonAsArrayGetElementByIndex(values, 0, mydepotuser) then
                        begin
                          LogDatei.log('Got depot user from service: ' +
                            mydepotuser, LLNotice);
                          if mydepotuser <> '' then
                          begin
                            if not divideAtFirst('\', mydepotuser, mydomain, myuser) then
                            begin
                              myuser := mydepotuser;
                              mydomain := '';
                            end;
                          end;
                        end;
                    end;
                  end;
                end;
              end;
            end;
          end;
      end;
      opsidata.setAddConfigStateDefaults(False);
    end;
    (*
      deprecated: getGeneralConfigValue    (do 04.01.2022)
    resultstring := MyOpsiMethodCall('getGeneralConfigValue',
      ['clientconfig.depot.user', myclientid]);
    mydepotuser := trim(SO(resultstring).S['result']);
    LogDatei.log('Got depot user from service: ' + mydepotuser, LLNotice);
    if mydepotuser <> '' then
    begin
      if not divideAtFirst('\', mydepotuser, mydomain, myuser) then
      begin
        myuser := mydepotuser;
        mydomain := '';
      end
    end
    else { we got no clientconfig.depot.user }
      myuser := 'pcpatch';
      *)
    LogDatei.log('Will use as domain: ' + mydomain + ' as user: ' + myuser, LLNotice);
    resultstring := MyOpsiMethodCall('user_getCredentials', ['pcpatch', myclientid]);
    myencryptedpass := SO(resultstring).O['result'].S['password'];
    LogDatei.log('Will use as encryptedpass: ' + myencryptedpass +
      ' clear pass: ' + mypass, LLInfo);
    mypass := decrypt(myhostkey, myencryptedpass);
    logdatei.AddToConfidentials(mypass);
    LogDatei.log('Will use as encryptedpass: ' + myencryptedpass +
      ' clear pass: ' + mypass, LLInfo);
    //writeln('mypass=',mypass);
    resultstring := MyOpsiMethodCall('host_getObjects',
      ['', '{"type":"OpsiDepotserver","id":["' + myDepot + '"]}']);
    myshare := SO(resultstring).S['result'];
    myshare := SO(resultstring).O['result'].AsArray.O[0].S['depotRemoteUrl'];
    myshare := copy(myshare, 5, length(myshare));
    //writeln('myshare=',myshare);
    umount(mymountpoint);

    mounttry := 0;
    repeat
      if (mounttry div 2) = 0 then
        mountoption := ' vers=3.11,';
      if (mounttry div 2) = 1 then
        mountoption := ' vers=3.0,';
      if (mounttry div 4) = 1 then
        mountoption := ' vers=2.0,';
      if (mounttry div 6) = 1 then
        mountoption := ' vers=1.0,';
      if (mounttry div 8) = 1 then
        mountoption := '';
      errorcode := mountSmbShare(mymountpoint, myshare, mydomain,
        myuser, mypass, mountoption);
      if (errorcode <> 0) or (not isMounted(mymountpoint)) then
      begin
        Inc(mounttry);
        LogDatei.log('Failed to mount ' + myshare + ' with option: ' +
          mountoption + ' to ' + mymountpoint + ' Error code: ' +
          IntToStr(errorcode) + ' - retry ...', LLWarning);

        sleep(2000);
      end;
    until isMounted(mymountpoint) or (mounttry > 9);

  except
    on e: Exception do
    begin
      LogDatei.log('Exception in oslinmount: mount_depotshare: ' +
        e.ClassName + ' Message: ' + e.message, LLError);
    end;
  end;
end;

end.
