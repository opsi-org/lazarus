program opsinetmount;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF} {$IFDEF WINDOWS}
  Windows,
  registry,
  osurlparser, {$ENDIF WINDOWS} {$IFDEF LINUX}
  process, {$ENDIF LINUX}
  Classes,
  SysUtils,
  CustApp,
  inifiles,
  blowfish,
  DCPblowfish,
  DCPcrypt2,
  DCPblockciphers,
  LazUTF8,
  oscertificates { you can add units after this };

const
  SW_HIDE = 0;

type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { TMyApplication }
  bytearray = array[0..255] of byte;


var
  optionlist: TStringList;
  myexitcode: integer;
  myuser, myencryptedpass, mypass, mykeyfile, myhostkey, mydrive,
  myshare, myerror, mymountpoint: string;
  myclientid: string;
  INI: TINIFile;
  mountresult: dword;
  network_ok, network_timeout: boolean;
  network_timeout_counter, network_timeout_counter_max: integer;
  pingtarget: string;


  procedure writevalues;
  begin
    writeln('myuser: ' + myuser);
    //writeln('mypass: '+mypass);
    writeln('myencryptedpass: ' + myencryptedpass);
    writeln('myhostkey: ' + myhostkey);
    writeln('mydrive: ' + mydrive);
    writeln('myshare: ' + myshare);
    writeln('mykeyfile: ' + mykeyfile);
  end;

{$IFDEF WINDOWS}
(*
function GetRegistryVarList(aktkey: string; noredirect: boolean): TStringList;
var
  basekey, relkey, winencodekey: string;
  Regist: Tregistry;
  KeyOpenMode: longword;
  rkey: HKey;
  resultlist: TStringList;
  i: integer;
begin
  winencodekey := UTF8ToWinCP(aktkey);
  Result := TStringList.Create;
  resultlist := TStringList.Create;
  basekey := copy(winencodekey, 0, Pos('\', winencodekey) - 1);
  LogDatei.log('basekey : ' + basekey, LLdebug3);
  relkey := copy(winencodekey, Pos('\', winencodekey) + 1, length(winencodekey));
  LogDatei.log('relkey : ' + relkey, LLdebug3);
  if noredirect then
  begin
    KeyOpenMode := KEY_READ or KEY_WOW64_64KEY;
    LogDatei.log('Registry started without redirection (64 Bit)', LLdebug);
  end
  else
  begin
    KeyOpenMode := KEY_READ or KEY_WOW64_32KEY;
    LogDatei.log('Registry started with redirection (32 Bit)', LLdebug);
  end;
  try
    try
      Regist := Tregistry.Create(KeyOpenMode);
      if GetHKey(basekey, rkey) then
      begin
        regist.RootKey := rkey;
        if regist.KeyExists(copy(winencodekey, Pos('\', winencodekey) +
          1, length(winencodekey))) then
        begin
          regist.OpenKey(relkey, False);
          regist.GetValueNames(resultlist);
          for i := 0 to resultlist.Count - 1 do
            Result.Add(WinCPToUTF8(resultlist.Strings[i]));
        end;
        regist.CloseKey;
      end
      else
        LogDatei.log('Could not get root key from : ' + basekey, LLError);
    except
      on e: Exception do
      begin
        LogDatei.log('Error: ' + e.message, LLError);
      end;
    end
  finally
    regist.Free;
    resultlist.Free;
    //LogDatei.log('finished6', LLdebug);
  end;
end;
function TuibRegistry.OpenKey(const Key0, Key: string): boolean;
var
  rkey: HKEY;
  dwdisposition: dword = 0;
  regresult: integer = 0;
  enckey: string;

begin
  Result := False;
  enckey := UTF8ToWinCP(key);
  if not GetHKEY(Key0, rkey) then
  begin
    LogS := 'Error: ' + Key0 + ' not accepted as registry root key';
    LogDatei.log(LogS, LLNotice);
  end
  else
  begin
    if keyopened then
      CloseKey;
    regresult :=
      RegCreateKeyEx(rkey, PChar(enckey), 0, nil, reg_option_non_volatile,
      KeyOpenMode, nil, mykey, @dwDisposition);

    if regresult <> ERROR_SUCCESS then
    begin
      LogS := 'Error: Registry key ' + '[' + key0 + '\' + key + '] ' +
        '  could not be opened by RegCreateKeyEx, ' + ' Errorno: ' +
        IntToStr(regresult) + ' "' + RemoveLineBreaks(SysErrorMessage(regresult)) + '"';
      LogDatei.log(LogS, LLNotice);
    end
    else
    begin
      keyopened := True;

      if dwDisposition = reg_created_new_key then
        LogS := 'created'
      else
        LogS := 'opened';

      LogS := 'Registry key ' + '[' + key0 + '\' + key + ']  ' + LogS;
      LogDatei.log(LogS, LLInfo);

      Result := True;
    end;
  end;

  LogDatei.LogSIndentLevel := baseindentlevel + countChar('\', key);

end;
*)

  function addWebdavRegKeys(fqdn: unicodestring): boolean;
  var
    unikey: unicodestring;
    Regist: Tregistry;
    KeyOpenMode: longword;
  begin
    // WebDAV Size Limit: HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Services\WebClient\Parameters FileSizeLimitInBytes = 0xffffffff
    // Share Trust: HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Internet Settings\ZoneMap\Domains\<fqdn>@SSL@4447 file = 1
    try
      try
        Result := True;
        unikey := 'SOFTWARE\Microsoft\Windows\CurrentVersion\Internet Settings\ZoneMap\Domains\'
          +
          fqdn + '@SSL@4447';
        Regist := Tregistry.Create;
        Regist.RootKey := HKEY_LOCAL_MACHINE;
        if not Regist.OpenKey('SYSTEM\CurrentControlSet\Services\WebClient\Parameters',
          True) then
        begin
          // failed
          Result := False;
        end
        else
        begin
          regist.WriteInt64('FileSizeLimitInBytes', $ffffffff);
        end;
        if not Regist.OpenKey(unikey, True) then
        begin
          // failed
          Result := False;
        end
        else
        begin
          regist.WriteInt64('file', 1);
        end;
      except
        Result := False;
      end;
    finally
      regist.Free;
    end;
  end;

  function IsValidDriveLetter(Key: char): boolean;
  var
    aVal: integer;
  begin
    aVal := Ord(key);
    case aVal of
      //65..90, 97..122, 192..255 : Result:=true; //65..90:A..Z, 97..122=a..z, 192..255: Umlaute, Akzente
      65..90: Result := True; //65..90:A..Z
      else
        Result := False;
    end;
  end;

  function mountWinShare(drive: string; uncPath: string; Username: string;
    Password: string; RestoreAtLogon: boolean): DWORD;
  var
    NetResource: TNetResource;
    dwFlags: DWORD;
    chPassword, chUsername: PChar;
  begin
    ZeroMemory(@NetResource, sizeof(TNetResource));
    with NetResource do
    begin
      dwType := RESOURCETYPE_DISK;
      lpLocalName := PChar(drive);
      lpRemoteName := PChar(uncPath);
      lpProvider := nil;
    end;

    if (RestoreAtLogon) then
      dwFlags := CONNECT_UPDATE_PROFILE
    else
      dwFlags := 0;

    if Username <> '' then
      chUsername := PChar(Username)
    else
      chUsername := nil;

    if Password <> '' then
      chPassword := PChar(Password)
    else
      chPassword := nil;

    Result := WNetAddConnection2(NetResource, chPassword, chUsername, dwFlags);
  end;

{$ENDIF WINDOWS}

{$IFDEF LINUX}
  function RunCommandAndCaptureOut
    (cmd: string; catchOut: boolean; var outlines: TStringList;
  var report: string; showcmd: integer; var ExitCode: longint): boolean;
  const
    ReadBufferSize = 2048;

  var
    //myStringlist : TStringlist;
    S: TStringList;
    M: TMemoryStream;
    FpcProcess: TProcess;
    n: longint;
    BytesRead: longint;

  begin
    Result := True;
    try
      try
        M := TMemoryStream.Create;
        BytesRead := 0;
        FpcProcess := process.TProcess.Create(nil);
        FpcProcess.CommandLine := cmd;
        FpcProcess.Options := [poUsePipes, poStderrToOutput];
        FpcProcess.ShowWindow := swoMinimize;
        FpcProcess.Execute;
        //Logdatei.DependentAdd('RunCommandAndCaptureOut: started: ' + cmd, LLdebug2);
        while FpcProcess.Running do
        begin
          // stellt sicher, dass wir Platz haben
          M.SetSize(BytesRead + ReadBufferSize);

          // versuche, es zu lesen
          if FpcProcess.Output.NumBytesAvailable > 0 then
          begin
            n := FpcProcess.Output.Read((M.Memory + BytesRead)^, ReadBufferSize);
            //Logdatei.DependentAdd('RunCommandAndCaptureOut: read: ' +
            //  IntToStr(n) + ' bytes', LLdebug2);
            if n > 0 then
            begin
              Inc(BytesRead, n);
              //Logdatei.DependentAdd('RunCommandAndCaptureOut: read: ' +
              // IntToStr(n) + ' bytes', LLdebug2);
              //Write('.')
            end;
          end
          else
          begin
            // keine Daten, warte 100 ms
            //Logdatei.DependentAdd('RunCommandAndCaptureOut: no data - waiting....',
            // LLdebug2);
            Sleep(100);
          end;
        end;
        exitCode := FpcProcess.ExitStatus;
        // lese den letzten Teil
        repeat
          // stellt sicher, dass wir Platz haben
          M.SetSize(BytesRead + ReadBufferSize);
          if FpcProcess.Output.NumBytesAvailable > 0 then
          begin
            // versuche es zu lesen
            n := FpcProcess.Output.Read((M.Memory + BytesRead)^, ReadBufferSize);
            if n > 0 then
            begin
              Inc(BytesRead, n);
              //Logdatei.DependentAdd('RunCommandAndCaptureOut: read: ' +
              //IntToStr(n) + ' bytes', LLdebug2);
              //Write('.');
            end;
          end
          else
            n := 0;
        until n <= 0;
        //if BytesRead > 0 then WriteLn;
        M.SetSize(BytesRead);
        //Logdatei.DependentAdd('RunCommandAndCaptureOut: -- executed --', LLdebug2);
        //WriteLn('-- executed --');

        S := TStringList.Create;
        S.LoadFromStream(M);
        //Logdatei.DependentAdd('RunCommandAndCaptureOut: -- linecount = ' +
        //  IntToStr(S.Count), LLdebug2);
        //WriteLn('-- linecount = ', S.Count, ' --');
        for n := 0 to S.Count - 1 do
        begin
          //WriteLn('| ', S[n]);
          outlines.Add(S[n]);
        end;
        //WriteLn('-- end --');
        //LogDatei.DependentAdd('ExitCode ' + IntToStr(exitCode), LLInfo);
      except
        on e: Exception do
        begin
          //LogDatei.DependentAdd('Exception in RunCommandAndCaptureOut: ' +
          //  e.message, LLError);
          Result := False;
        end;
      end;
    finally
      S.Free;
      FpcProcess.Free;
      M.Free;
    end;
  end;

  function mountSmbShare(mymountpoint, myshare, myuser, mypass: string): integer;
  var
    cmd, report: string;
    outlines: TStringList;
    ExitCode: longint;
    i: integer;
  begin
    outlines := TStringList.Create;
    Result := 0;
    //{$IFDEF CPU64}
    cmd := '/sbin/mount.cifs ' + myshare + ' ' + mymountpoint + ' -o ro,user=' +
      myuser + ',pass=' + mypass;
    //{$ELSE}
    //cmd := '/bin/bash -c "/sbin/mount.smbfs ' + myshare+' '+mymountpoint+' -o ro,user='+myuser+',pass='+mypass+'"';
    //{$ENDIF}
    //LogDatei.DependentAdd('calling: '+cmd,LLNotice);
    if not RunCommandAndCaptureOut(cmd, True, outlines, report, SW_HIDE, ExitCode) then
    begin
      //LogDatei.log('Error: ' + Report + 'Exitcode: ' + IntToStr(ExitCode), LLError);
      writeln('Error: ' + Report + 'Exitcode: ' + IntToStr(ExitCode));
      Result := ExitCode;
    end
    else
    begin
      Result := ExitCode;
    (*
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
    *)
    end;
    outlines.Free;
  end;

(*
function umount(mymountpoint : string) : integer;
var
  cmd, report: string;
  outlines: TStringlist;
  ExitCode: longint;
  i: integer;
begin
  outlines := TStringList.Create;
  Result := 0;
  cmd := '/bin/bash -c "/bin/umount ' +mymountpoint+'"';
  LogDatei.DependentAdd('calling: '+cmd,LLNotice);
  if not RunCommandAndCaptureOut(cmd, True, outlines, report,
    SW_HIDE, ExitCode) then
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
  outlines.Free;
end;
*)

  function ping(pingtarget: string): boolean;
  var
    cmd, report: string;
    outlines: TStringList;
    ExitCode: longint;
    i: integer;
  begin
    outlines := TStringList.Create;
    Result := False;
    cmd := '/bin/ping -c 1 ' + pingtarget;
    //LogDatei.DependentAdd('calling: '+cmd,LLNotice);
    if not RunCommandAndCaptureOut(cmd, True, outlines, report, SW_HIDE, ExitCode) then
    begin
      //LogDatei.log('Error: ' + Report + 'Exitcode: ' + IntToStr(ExitCode), LLError);
      writeln('Error: ' + Report + 'Exitcode: ' + IntToStr(ExitCode));
    end
    else
    begin
      if ExitCode = 0 then Result := True;
    (*
    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 6;
    LogDatei.log('', LLDebug);
    LogDatei.log('output:', LLDebug);
    LogDatei.log('--------------', LLDebug);
    *)
      for i := 0 to outlines.Count - 1 do
      begin
        //LogDatei.log(outlines.strings[i], LLDebug);
        writeln(outlines.strings[i]);
      end;
    (*
    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 6;
    LogDatei.log('', LLDebug);
    *)
    end;
    outlines.Free;
  end;


{$ENDIF LINUX}


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
    (*
  s:= '!!!Hello World!!!';
  Cipher:= TDCP_blowfish.Create(nil);
  Cipher.Init(Key,Sizeof(Key)*8,nil); // remember key size is in BITS
  Cipher.EncryptCBC(Buffer,Buffer,Sizeof(Buffer));
  Cipher.EncryptCBC(s[1],s[1],Length(s)); // when encrypting strings you must point the algorithm to the first character

  TO DECRYPT
  *)

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



  procedure TMyApplication.DoRun;
  var
    ErrorMsg: string;
    ufqdn: unicodestring;
    urllist: TStringList;
    regpatch: boolean;
    filename: string;
  begin
    //0c76f3e7c5ca30bc
    myexitcode := 0;
    myerror := '';
    myuser := 'pcpatch';
    mydrive := 'P';
    mypass := 'linux123';
    mymountpoint := '/mnt';
    pingtarget := 'sepiolina';
    network_timeout_counter_max := 20;
    optionlist := TStringList.Create;
    optionlist.Append('help');
    optionlist.Append('keyfile::');
    optionlist.Append('user:');
    optionlist.Append('encrypted-pass::');
  {$IFDEF WINDOWS}
    optionlist.Append('drive:');
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
    optionlist.Append('mountpoint:');
    optionlist.Append('pingtarget:');
    optionlist.Append('network-timeout::');
  {$ENDIF LINUX}
    optionlist.Append('share::');
    optionlist.Append('mount-protocol:');

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

    if HasOption('keyfile') then
    begin
      mykeyfile := trim(GetOptionValue('keyfile'));
      if not FileExists(mykeyfile) then
      begin
        myerror := 'Error: Given keyfile: ' + mykeyfile + ' does not exist.';
        WriteHelp;
      end
      else
        writeln('Given keyfile: ' + mykeyfile);
      // try to read it as config.ini
      INI := TINIFile.Create(mykeyfile);
      myhostkey := trim(INI.ReadString('shareinfo', 'pckey', ''));
      myuser := trim(INI.ReadString('shareinfo', 'user', myuser));
      myclientid := trim(INI.ReadString('install', 'client_hostname', ''));
      if myclientid <> '' then
        myclientid := myclientid + '.' + trim(INI.ReadString('install', 'client_domain', ''));
      if myhostkey = '' then
      begin
        // try to read it as opsiclientd.conf
        myhostkey := trim(INI.ReadString('global', 'opsi_host_key', ''));
      end;
      if myclientid = '' then
      begin
        // try to read it as opsiclientd.conf
        myclientid := trim(INI.ReadString('global', 'host_id', ''));
      end;
      ini.Free;
      if myhostkey = '' then
      begin
        myerror := 'Error: No host key in keyfile: ' + mykeyfile;
        WriteHelp;
      end;
      if myhostkey = '#@PCKEY#' then
      begin
        myerror := 'Error: No valid host key in keyfile: ' + mykeyfile + ' , found: #@PCKEY#.';
        WriteHelp;
      end;
    end
    else
    begin
      myerror := 'Error: No keyfile given.';
      WriteHelp;
    end;


    if HasOption('encrypted-pass') then
    begin
      myencryptedpass := trim(GetOptionValue('encrypted-pass'));
      if myencryptedpass = '' then
      begin
        myerror := 'Error: No valid encrypted password given.';
        WriteHelp;
      end
      else
        writeln('Given encrypted password: ' + myencryptedpass);
    end
    else
    begin
      myerror := 'Error: No encrypted password given.';
      WriteHelp;
    end;

    if HasOption('user') then
    begin
      myuser := trim(GetOptionValue('user'));
      if myuser = '' then
      begin
        myerror := 'Error: No valid user given.';
        WriteHelp;
      end
      else
        writeln('Given user: ' + myuser);
    end;

    if HasOption('share') then
    begin
      myshare := trim(GetOptionValue('share'));
      if myshare = '' then
      begin
        myerror := 'Error: No valid share given.';
        WriteHelp;
      end
      else
        writeln('Given share: ' + myshare);
    end
    else
    begin
      myerror := 'Error: No share given.';
      WriteHelp;
    end;

  {$IFDEF WINDOWS}
    if HasOption('drive') then
    begin
      mydrive := trim(GetOptionValue('drive'));
      mydrive := UpperCase(mydrive);
      if not IsValidDriveLetter(mydrive[1]) then
      begin
        myerror := 'Error: No valid drive letter given.';
        WriteHelp;
      end
      else
        writeln('Given drive: ' + mydrive);
    end;
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
    if HasOption('mountpoint') then
    begin
      mymountpoint := trim(GetOptionValue('mountpoint'));
      if not FileExists(mymountpoint) then
      begin
        myerror := 'Error: Given mountpoint: ' + mymountpoint + ' does not exist.';
        WriteHelp;
      end
      else
        writeln('Given mountpoint: ' + mymountpoint);
    end;
    if HasOption('pingtarget') then
    begin
      pingtarget := trim(GetOptionValue('pingtarget'));
      writeln('Given pingtarget: ' + pingtarget);
    end;
    if HasOption('network-timeout') then
    begin
      network_timeout_counter_max := StrToInt(trim(GetOptionValue('network-timeout')));
      writeln('Given network-timeout: ' + IntToStr(network_timeout_counter_max));
    end;
  {$ENDIF LINUX}



    { add your program here }
    mypass := decrypt(myhostkey, myencryptedpass);
    writevalues;

  {$IFDEF WINDOWS}
    filename := ExtractFilePath(ParamStr(0)) + '\opsi-ca-cert.pem';
    urllist := parseUrl(myshare);
    if urllist.Values['Protocol'] = 'https' then
    begin
      ufqdn := UTF8ToUTF16(urllist.Values['Host']);
      writeln('Webdav detected with https: and host: ' + urllist.Values['Host']);
      regpatch := True;
      if not addWebdavRegKeys(ufqdn) then
      begin
        //failed
        regpatch := False;
        writeln('Webdav detected - Registry patch failed');
      end
      else
        writeln('Webdav detected - Registry patch success');

      if not pemfileToSystemStore(filename) then
      begin
        //failed
        writeln('Webdav detected - Add CA to system store failed');
      end
      else
        writeln('Webdav detected - Add CA to system store success');
      myuser := myclientid;
      mypass := myhostkey;
      writeln('Webdav detected - using host_id / host_key as user and password');
      writeln('using host_id / host_key: '+myclientid+' / '+myhostkey);
    end;
    FreeAndNil(urllist);
  (*
  if not FileExists(filename) then writeln('Not found: '+filename)
  else
  if not pemfileToSystemStore(filename) then
    begin
      //failed
       writeln('No Webdav detected - Add CA to system store failed');
    end
    else writeln('No Webdav detected - Add CA to system store success');
    *)
    mountresult := mountwinShare(mydrive + ':', myshare, myuser, mypass, False);
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
    if not (pingtarget = '') then
    begin
      network_ok := False;
      network_timeout := False;
      network_timeout_counter := 0;
      repeat
        if ping(pingtarget) then
          network_ok := True
        else
        begin
          if network_timeout_counter = network_timeout_counter_max then
            network_timeout := True
          else
          begin
            Inc(network_timeout_counter);
            sleep(1000);
            writeln(DateTimeToStr(now) + ' - Network not reachable ...');
          end;
        end;
      until network_ok or network_timeout;
      if network_ok then writeln('Network seems reachable ...');
    end;
    mountresult := mountSmbShare(mymountpoint, myshare, myuser, mypass);
  {$ENDIF LINUX}
    writeln('mountresult: ' + IntToStr(mountresult) + ' : ' + SysErrorMessage(mountresult));
    myexitcode := mountresult;

    // stop program loop
    Terminate;
    halt(myexitcode);
  end;

  constructor TMyApplication.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor TMyApplication.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TMyApplication.WriteHelp;
  var
    filename: string;
  begin
    filename := ExtractFileName(ParamStr(0));
    writeln(ParamStr(0));
    writeln(filename);
    writeln(myerror);
    writeln('Usage:');
    writeln(filename +
      ' --keyfile=<path\filename> --encrypted-pass=<pass> --drive=n --share=<\\server\share name> [--user=<login name>]');
    writeln('Options:');
    writeln(' --help -> write this help and exit');
    writeln(' --encrypted-pass=<pass> -> with opsi host key encrypted password');
    writeln(' --keyfile=<path\filename> -> file with opsi host key (config.ini or opsiclientd format)');
    writeln(' --user=<login name> -> user to use (optional: default = pcpatch.');
  {$IFDEF WINDOWS}
    writeln(' --drive=n -> drive letter to which the share should be mounted (e.g. "P")');
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
    writeln(' --mountpoint=path> --> directory to which the share should be mounted (e.g. "/mnt")');
  {$ENDIF LINUX}
    writeln(' --share=<\\server\share name> -> the smb share which should be mounted. or');
    writeln(' --share=<https://<fqdn>:4447/depot> -> the webdav share which should be mounted.');
  {$IFDEF LINUX}
    writeln(' --pingtarget=<IP> --> IP-Adress we will ping to check the network');
    writeln(' --network-timeout=<seconds> --> Seconds we will wait for the network to come up');
  {$ENDIF LINUX}

    Terminate;
    halt(-1);
    Exit;
  end;

var
  Application: TMyApplication;


{$R *.res}

begin
  Application := TMyApplication.Create(nil);
  Application.Run;
  Application.Free;
end.
