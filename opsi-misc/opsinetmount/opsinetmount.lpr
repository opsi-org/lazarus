program opsinetmount;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  {$IFDEF WINDOWS}
  windows,
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
  process,
  {$ENDIF LINUX}
  Classes, SysUtils, CustApp, inifiles, blowfish, DCPblowfish, DCPcrypt2,
  DCPblockciphers

  { you can add units after this };

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
   optionlist : TStringlist;
	myexitcode : integer;
  myuser, myencryptedpass, mypass, mykeyfile, myhostkey, mydrive, myshare, myerror, mymountpoint : string;
  INI:TINIFile;
  mountresult : dword;
  network_ok, network_timeout :boolean;
  network_timeout_counter, network_timeout_counter_max : integer;
  pingtarget : string;


procedure writevalues;
begin
  writeln('myuser: '+myuser);
  //writeln('mypass: '+mypass);
  writeln('myencryptedpass: '+myencryptedpass);
  writeln('myhostkey: '+myhostkey);
  writeln('mydrive: '+mydrive);
  writeln('myshare: '+myshare);
  writeln('mykeyfile: '+mykeyfile);
end;

{$IFDEF WINDOWS}
function IsValidDriveLetter(Key: char):Boolean;
var aVal: integer;
begin
  aVal:=Ord(key);
  case aVal of
    //65..90, 97..122, 192..255 : Result:=true; //65..90:A..Z, 97..122=a..z, 192..255: Umlaute, Akzente
    65..90 : Result:=true; //65..90:A..Z
    else Result:=false;
  end;
end;

function mountSmbShare(drive: String; uncPath: String;
  Username: String; Password: String; RestoreAtLogon: Boolean): DWORD;
var
  NetResource: TNetResource;
  dwFlags: DWORD;
  chPassword, chUsername : PChar;
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

function mountSmbShare(mymountpoint, myshare, myuser, mypass: string) : integer;
var
  cmd, report: string;
  outlines: TStringlist;
  ExitCode: longint;
  i: integer;
begin
  outlines := TStringList.Create;
  Result := 0;
  //{$IFDEF CPU64}
  cmd := '/sbin/mount.cifs ' + myshare+' '+mymountpoint+' -o ro,user='+myuser+',pass='+mypass;
  //{$ELSE}
  //cmd := '/bin/bash -c "/sbin/mount.smbfs ' + myshare+' '+mymountpoint+' -o ro,user='+myuser+',pass='+mypass+'"';
  //{$ENDIF}
  //LogDatei.DependentAdd('calling: '+cmd,LLNotice);
  if not RunCommandAndCaptureOut(cmd, True, outlines, report,
    SW_HIDE, ExitCode) then
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

function ping(pingtarget : string) : boolean;
var
  cmd, report: string;
  outlines: TStringlist;
  ExitCode: longint;
  i: integer;
begin
  outlines := TStringList.Create;
  Result := false;
  cmd := '/bin/ping -c 1 ' +pingtarget;
  //LogDatei.DependentAdd('calling: '+cmd,LLNotice);
  if not RunCommandAndCaptureOut(cmd, True, outlines, report,
    SW_HIDE, ExitCode) then
  begin
    //LogDatei.log('Error: ' + Report + 'Exitcode: ' + IntToStr(ExitCode), LLError);
    writeln('Error: ' + Report + 'Exitcode: ' + IntToStr(ExitCode));
  end
  else
  begin
    if  ExitCode = 0 then Result := true;
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
   (const hexstring : String;
    var hexarray : bytearray);
var
   section : String;
   thesize : integer;
   onehex : string;
   i: integer;
 begin
   theSize := length (hexstring) div 2;
   section := hexstring;
   for i:=0 to theSize -1 do
   begin
    onehex := copy(section,1,2);
    HexToBin(Pchar(onehex),@hexarray[i],1);
    section := copy(section,3,length(section));
   end;
 end;

function decrypt(hexkey, hexpass : String) : String;
Var
 passByte, keyByte : bytearray;
 passByteDecry : bytearray;
 passSize : Integer;
 keySize : Integer;
 i: integer;
 Cipher: TDCP_blowfish;
begin
 transformHex (hexkey, keyByte);
 transformHex (hexpass, passByte);

 passSize := length (hexpass) div 2;
 keySize := length (hexkey) div 2;

 //DCP_blowfish1.Init(keyByte,keySize*8,PChar('OPSI1234'));
 //DCP_blowfish1.Decrypt(passByte,passByteDecry,passSize);

  Cipher:= TDCP_blowfish.Create(nil);
  Cipher.Init(keyByte,keySize*8,PChar('OPSI1234')); // remember key size is in BITS

  Cipher.DecryptCBC(passByte,passByteDecry,passSize);
  Cipher.Burn;
  Cipher.Free;

 result := '';
 for i :=0 to passSize - 1 do
  result := result + char (passByteDecry [i])
end;


function DecryptBlowfish(const myencrypted,mykey: string): string;
var
Cipher: TDCP_blowfish;
Key: array[0..15] of byte; // key can be any size upto 448bits with blowfish (56bytes)
//Buffer: array[0..127] of byte; // buffer can be any size
s: string;
i : integer;
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
  s:= myencrypted;
  for i := 0 to 15 do
  begin
    key[i] := strtoint('$'+mykey[i*2+1]+mykey[i*2+2]);
  end;
  Cipher:= TDCP_blowfish.Create(nil);
  Cipher.Init(Key,Sizeof(Key)*8,nil); // remember key size is in BITS

  Cipher.DecryptCBC(s[1],s[1],Length(s));
  Cipher.Burn;
  Cipher.Free;
  result := s;
end;

(*
function EncryptBlowfish(const a,key: string): string;
var Inp, Outp, EC: TStream;
begin
   Inp := TStringStream.Create(a);
   try
      Outp := TStringStream.Create('');
      try
         EC := TBlowFishEncryptStream.Create(Key, Outp);
         try
            EC.CopyFrom(Inp, Inp.Size);
         finally
            EC.Free; // Destroy automatically calls Flush
         end;
         result := TStringStream(Outp).DataString;
      finally
         Outp.Free;
      end;
   finally
      Inp.Free;
   end;
end;


function DecryptBlowfish(const a,key: string): string;
var Inp, Outp, EC: TStream;
  mypass : array [0..1023] of char;
  mybin : array [0..1023] of char;
  bytesread : integer;
  myhexpass : string;
begin
  // HexToBin(pchar(a),pchar(mybin),sizeof(mybin));
   Inp := TStringStream.Create(a);
   try
      Outp := TStringStream.Create('');
      try
         EC := TBlowFishDecryptStream.Create(Key, inp);
         try
             repeat
                bytesread:=EC.Read(mypass[0],Sizeof(mypass[0]));
                if bytesread<>0 then
                  //processbuffer(mypass,bytesread);
                  BinToHex(mypass,pchar(myhexpass),sizeof(mypass));
                  result := result +  myhexpass;
              until (bytesread=0);
            //bytesread := EC.Read(mypass[0],1024);
         finally
            EC.Free; // Destroy automatically calls Flush
         end;
         //result := string(mypass);
         //result := TStringStream(Outp).DataString;
      finally
         Outp.Free;
      end;
   finally
      Inp.Free;
   end;
end;
 *)



procedure TMyApplication.DoRun;
var
  ErrorMsg: String;
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
  optionlist := TStringlist.Create;
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
  // quick check parameters
  ErrorMsg:= CheckOptions('',optionlist);
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;



  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('keyfile') then
  begin
    mykeyfile := trim(GetOptionValue('keyfile'));
    if not FileExists(mykeyfile) then
    begin
      myerror := 'Error: Given keyfile: '+mykeyfile+' does not exist.';
      WriteHelp;
    end;
    // try to read it as config.ini
    INI := TINIFile.Create(mykeyfile);
    myhostkey := trim(INI.ReadString('shareinfo','pckey',''));
    myuser := trim(INI.ReadString('shareinfo','user',myuser));
    if myhostkey = '' then
    begin
      // try to read it as opsiclientd.conf
      myhostkey := trim(INI.ReadString('global','opsi_host_key',''));
    end;
    ini.free;
    if myhostkey = '' then
    begin
       myerror := 'Error: No host key in keyfile: '+mykeyfile;
       WriteHelp;
    end;
    if myhostkey = '#@PCKEY#' then
    begin
      myerror := 'Error: No valid host key in keyfile: '+mykeyfile+' , found: #@PCKEY#.';
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
    end;
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
    end;
  end;

  if HasOption('share') then
  begin
    myshare := trim(GetOptionValue('share'));
    if myshare = '' then
    begin
      myerror := 'Error: No valid share given.';
      WriteHelp;
    end;
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
    end;
  end;
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
  if HasOption('mountpoint') then
  begin
    mymountpoint := trim(GetOptionValue('mountpoint'));
    if not FileExists(mymountpoint) then
    begin
      myerror := 'Error: Given mountpoint: '+mymountpoint+' does not exist.';
      WriteHelp;
    end;
  end;
  if HasOption('pingtarget') then
  begin
    pingtarget := trim(GetOptionValue('pingtarget'));
  end;
  if HasOption('network-timeout') then
  begin
    network_timeout_counter_max := StrToInt(trim(GetOptionValue('network-timeout')));
  end;
  {$ENDIF LINUX}



  { add your program here }
  mypass := decrypt(myhostkey, myencryptedpass);
  writevalues;

  {$IFDEF WINDOWS}
  mountresult := mountSmbShare(mydrive+':', myshare, myuser, mypass, false);
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
  if not (pingtarget = '') then
  begin
    network_ok := false;
    network_timeout := false;
    network_timeout_counter := 0;
    repeat
      if ping(pingtarget) then
        network_ok := true
      else
      begin
        if network_timeout_counter = network_timeout_counter_max then  network_timeout := true
        else
        begin
          inc(network_timeout_counter);
          sleep(1000);
          writeln(DateTimeToStr(now) + ' - Network not reachable ...');
        end;
      end;
    until network_ok or network_timeout;
    if network_ok then writeln('Network seems reachable ...');
  end;
  mountresult := mountSmbShare(mymountpoint, myshare, myuser, mypass);
  {$ENDIF LINUX}
  writeln('mountresult: '+inttostr(mountresult)+' : '+SysErrorMessage(mountresult));
  myexitcode := mountresult;

  // stop program loop
  Terminate;
  halt(myexitcode);
end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMyApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TMyApplication.WriteHelp;
var
  filename : string;
begin
  filename := ExtractFileName(paramstr(0));
  writeln(paramstr(0));
  writeln(filename);
  writeln(myerror);
  writeln('Usage:');
  writeln(filename+ ' --keyfile=<path\filename> --encrypted-pass=<pass> --drive=n --share=<\\server\share name> [--user=<login name>]');
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
  writeln(' --share=<\\server\share name> -> the share which should be mounted.');
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
  Application:=TMyApplication.Create(nil);
  Application.Run;
  Application.Free;
end.
