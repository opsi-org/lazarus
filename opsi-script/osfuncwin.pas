unit osfuncwin;

// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: Rupert Roeder, detlef oertel


{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  osfunc,
  ostxstringlist,
  Windows,
{$IFNDEF WIN64}
  DSiWin32,
{$ENDIF WIN64}
{$IFDEF GUI}
  graphics,
{$ENDIF GUI}
  registry,
  //JwaWinnt,
  //jwawinbase,
  //JwaWindows,
  //JwaUserEnv,
  winsock,
  oslog,
  LAZUTF8;

(*
type
  Treadthread = class(TThread)
  public
    procedure execute; override;
  end;
*)

function RunCommandAndCaptureOut
  (cmd: string; catchOut: boolean; var outlines: TXStringList;
  var report: string; showcmd: integer; var ExitCode: longint): boolean; overload;

function RunCommandAndCaptureOut
  (cmd: string; catchOut: boolean; var outlines: TXStringList;
  var report: string; showcmd: integer; var ExitCode: longint; showoutput:boolean): boolean;   overload;

function RunCommandAndCaptureOut
  (cmd: string; catchOut: boolean; var outlines: TXStringList;
  var report: string; showcmd: integer; var ExitCode: longint;
  showoutput:boolean; logleveloffset : integer): boolean;   overload;


function getMyHostEnt(var myHostEnt : THostEnt) :boolean;

function WinIsUefi: boolean;
function WinIsPE: boolean;

function IsDriveReady(Drive: string): Boolean;
function GetIPFromHost(var HostName, IPaddr, WSAErr: string): Boolean;



implementation
uses
  {$IFDEF GUI}
  osbatchgui,
  osinteractivegui,
  osshowsysinfo,
  {$ENDIF GUI}
    osmain;
(*
const
  ReadBufferSize = 2048;

var
  hReadPipe: THandle = 0;
  lpBuffer: array[0..ReadBufferSize] of char;
  BytesRead: dword = 0;
  BytesToRead: dword = 0;
  rb: boolean = False;


procedure  Treadthread.execute;
  begin
    lpBuffer := '';
    rb := ReadFile(hReadPipe, lpBuffer, BytesToRead, BytesRead, nil);
  end;
*)


function RunCommandAndCaptureOut
  (cmd: string; catchOut: boolean; var outlines: TXStringList;
  var report: string; showcmd: integer; var ExitCode: longint): boolean;
begin
  result := RunCommandAndCaptureOut(cmd, catchOut, outlines, report, showcmd, ExitCode, false);
end;

function RunCommandAndCaptureOut
  (cmd: string; catchOut: boolean; var outlines: TXStringList;
  var report: string; showcmd: integer; var ExitCode: longint;showoutput:boolean): boolean;
begin
  result := RunCommandAndCaptureOut(cmd, catchOut, outlines, report, showcmd, ExitCode, showoutput,0);
end;

function RunCommandAndCaptureOut
  (cmd: string; catchOut: boolean; var outlines: TXStringList;
  var report: string; showcmd: integer; var ExitCode: longint;
  showoutput:boolean; logleveloffset : integer): boolean;

const
  ReadBufferSize = 2048;

var
  //tmp: string;
  hReadPipe: THandle = 0;
  hWritePipe: THandle = 0;
  sa: TSecurityAttributes;
  si: TStartupInfo;
  pi: TProcessInformation;
  lpBuffer: array[0..ReadBufferSize] of char;
  BytesRead: dword = 0;
  BytesToRead: dword = 0;
  BytesAvail: dword = 0;
  BytesLeft: dword = 0;
  Buffer: string = '';
  rb: boolean = False;
  BufPos: integer = 0;
  startpos: integer = 0;
  FoundNewLine: boolean = False;
  output_line: string = '';
  Apprunning: DWord = 0;
  line_no: integer = 0;
  functionresult: longbool;
  splittingOffLines: boolean = False;
  lpExitCode: DWORD = 0;


begin
  Result := True;
  //lines.add ('start');
  sa.nLength := sizeof(sa);
  sa.lpSecurityDescriptor := nil;
  sa.bInheritHandle := True;

  if not CreatePipe(hReadPipe, hWritePipe, @sa, 0) then
  begin
    Report := 'Error creation Pipe';
    Result := False;
    exit;
  end;

  FillChar(si, sizeof(si), 0);
  si.cb := sizeof(si);
  si.dwFlags := STARTF_USESHOWWINDOW;
  if catchOut then
    si.dwFlags := si.dwFlags or STARTF_USESTDHANDLES;
  si.wShowWindow := showcmd; // SW_SHOW; SW_HIDE
  si.hStdInput := 0;
  si.hStdOutput := hWritePipe;
  si.hStdError := hWritePipe;
  {$IFDEF WINDOWS}
  cmd := utf8towincp(cmd);
  {$ENDIF WINDOWS}

  functionresult := CreateProcess(nil, PChar(cmd), nil, nil, True,
    0, nil, nil, si, pi);

  if not functionresult then
  begin
    report := 'Could not execute ' + cmd + ', error ' + ' (' +
      SysErrorMessage(getLastError) + ')';
    ExitCode := getLastError;
    CloseHandle(hReadPipe);
    CloseHandle(hWritePipe);
    Result := False;
    exit;
  end;

  CloseHandle(hWritePipe);
  BytesToRead := ReadBufferSize;
  BytesRead := 0;
  line_no := 0;

  {$IFDEF GUI}
  if showoutput then
  begin
    FBatchOberflaeche.Left:= 5;
    FBatchOberflaeche.Top:= 5;
    CreateSystemInfo;
    SystemInfo.Memo1.Color:= clBlack;
    SystemInfo.Memo1.Font.Color := clWhite;
    SystemInfo.Memo1.Lines.clear;
    systeminfo.BitBtn1.Enabled:= false;
    systeminfo.Label1.Caption:='Executing: '+cmd;
    systeminfo.ShowOnTop;
    ProcessMess;
    LogDatei.log('Start Showoutput', LLInfo+logleveloffset);
  end;
  {$ENDIF GUI}

  while (True) do
  begin
    lpBuffer := '';
    while PeekNamedPipe(hReadPipe, @lpBuffer, BytesToRead, @BytesRead,@BytesAvail,@BytesLeft) and (BytesAvail = 0) do
    begin
      Processmess;
      Sleep(100);
    end;
    lpBuffer := '';
    rb := ReadFile(hReadPipe, lpBuffer, BytesToRead, BytesRead, nil);
    //readthread := Treadthread.Create(false);
    //readthread.Priority:= tpLower;
    //ProcessMess;
    //readthread.WaitFor;
    //readthread.Free;

    if (not rb) then
      if (length(Buffer) = 0) then
        break;

    // exit from loop

    OemToAnsiBuff(lpBuffer, lpBuffer, BytesRead);
    Buffer := Buffer + lpBuffer;
    BufPos := AnsiPos(#13, Buffer);

    splittingOffLines := (length(Buffer) > 0) and (bufPos > 0);

    while splittingOffLines do
    begin
      Inc(line_no);

      foundNewLine := True;
      output_line :=
        //inttostr (line_no) + ': ' +
        Copy(Buffer, 1, BufPos - 1);



      // Here you have in output_line the console output line by line.
      output_line := WinCPToUTF8(output_line);
      outlines.Add(output_line);

      {$IFDEF GUI}
      if showoutput then
      begin
        SystemInfo.Memo1.Lines.Add(output_line);
        ProcessMess;
      end;
      //ProcessMess;
      {$ENDIF GUI}

      //LogDatei.DependentAdd(copy (cmd, 1, 5) + ' ... :' + output_line, LevelInfo);

      // shift remainder of buffer down

      if (Length(buffer) > bufpos) and (buffer[BufPos + 1] = #10)
      //  we have
      //  - carriage return at position bufpos
      //  - and an additional line feed at position bufpos + 1
      then
        startpos := BufPos + 2
      else
        startpos := BufPos + 1;

      Buffer := Copy(Buffer, startpos, ReadBufferSize);

      BufPos := AnsiPos(#13, Buffer);

      if (length(Buffer) = 0) or (bufPos = 0) then
        SplittingOffLines := False;
    end;


    //BytesToRead   := ReadBufferSize - Length( Buffer );

    if length(Buffer) > 0 then
      //bufPos = 0 since otherwise the loop would continue
    begin
      if rb then
        // no end of line found but we expect more data to fill the line
      begin
      end
      else
      begin
        // we do not expect more data and yield the rest of the buffer
        Buffer := WinCPToUTF8(Buffer);
        outlines.Add(
          //inttostr (line_no) + ': ' +
          Buffer);
        {$IFDEF GUI}
        if showoutput then
        begin
          SystemInfo.Memo1.Lines.Add(output_line);
          ProcessMess;
        end;
        //ProcessMess;
        {$ENDIF GUI}
        Buffer := '';

      end;
    end;
    GetExitCodeProcess(pi.hProcess, lpExitCode);
    exitCode := longint(lpExitCode);
    //LogDatei.DependentAdd('ExitCode ' + IntToStr(exitCode), LLInfo);
  end;
  //lines.add('waiting 0');
     {
     repeat
      Apprunning := WaitForSingleObject
                   (pi.hProcess,100) ;
      Application.ProcessMessages;
      lines.add('waiting');
     until (Apprunning <> WAIT_TIMEOUT) ;
     }
  if GetExitCodeProcess(pi.hProcess, lpExitCode) and (lpExitCode = still_active) then
    repeat
      //sleep(500);
      ProcessMess;
    until GetExitCodeProcess(pi.hProcess, lpExitCode) and
      (lpExitCode <> still_active);
  //WaitForSingleObject(pi.hProcess, INFINITE);
  GetExitCodeProcess(pi.hProcess, lpExitCode);
  exitCode := longint(lpExitCode);
  LogDatei.log('ExitCode ' + IntToStr(exitCode), LLInfo+logleveloffset);
  CloseHandle(pi.hProcess);
  CloseHandle(hReadPipe);
  {$IFDEF GUI}
  if showoutput then
  begin
    SystemInfo.free; SystemInfo := nil;
    FBatchOberflaeche.BringToFront;
    FBatchOberflaeche.centerWindow;
    ProcessMess;
    LogDatei.log('Stop Showoutput', LLInfo+logleveloffset);
  end;
  {$ENDIF GUI}
end;


function getMyHostEnt(var myHostEnt : THostEnt) :boolean;
var
  buffer: PChar;
  len: integer = 0;
  errorcode: integer = 0;
  WSAData: TWSAData;
begin
  result := false;
  try
    try
      (* was a try - did not work (do 31.7.2017)
      if WSAStartup($0101, WSAData) = 0 then
      begin
        Logdatei.log_prog('gethostname: Winsock is not responding.', LLError);
      end
      else
      *)
      begin
        buffer := StrAlloc(301);
        len := 300;
        errorcode := winsock.gethostname(buffer, len);
        if errorcode = 0 then
        begin
          myHostEnt := gethostbyname(buffer)^;
          result := true;
        end
        else
        begin
          case WSAGetLastError of
            WSANOTINITIALISED:Logdatei.log_prog('gethostname error WSANotInitialised', LLError);
            WSAENETDOWN      :Logdatei.log_prog('gethostname error WSAENetDown', LLError);
            WSAEINPROGRESS   : Logdatei.log_prog('gethostname error WSAEInProgress', LLError);
            else Logdatei.log_prog('unknown gethostname error ', LLError);
           end;
        end;
      end;
    finally
      StrDispose(buffer);
      //WSACleanup;
    end
  except
    Logdatei.DependentAddError('gethostname error ' +
      IntToStr(wsagetlasterror), LLError);
  end;
end;

function GetFirmwareEnvironmentVariableA(lpName, lpGuid: LPCSTR; pBuffer: Pointer;
  nSize: DWORD): DWORD; stdcall; external kernel32 name 'GetFirmwareEnvironmentVariableA';

function WinIsUefi: boolean;
begin
  Result := false;
  try
    GetFirmwareEnvironmentVariableA('','{00000000-0000-0000-0000-000000000000}', nil,0);
    if (GetLastError = ERROR_INVALID_FUNCTION) then
      //Writeln('Legacy BIOS')
      Result := false
    else
      //Writeln('UEFI Boot Mode');
      Result := true;
  except
    on E: Exception do
      Logdatei.log('Exception in WinIsUefi: '+E.ClassName+ ': '+ E.Message,LLError);
  end;
end;

function WinIsPE: boolean;
var
  myreg: Tregistry;
begin
  Result := false;
  try
    if Is64BitSystem then
    begin
      myreg := TRegistry.Create(KEY_ALL_ACCESS or KEY_WOW64_64KEY);
      LogDatei.log_prog('Registry started without redirection (64 Bit)', LLdebug3);
    end
    else
    begin
      myreg := TRegistry.Create(KEY_ALL_ACCESS or KEY_WOW64_32KEY);
      LogDatei.log_prog('Registry started with redirection (32 Bit)', LLdebug3);
    end;
    myreg.RootKey := HKEY_LOCAL_MACHINE;
    if myreg.KeyExists('SYSTEM\ControlSet001\Control\MiniNT') then
      //'PE Indicator');
      // http://techgenix.com/howtodetectwhetheryouareinwindowspe/
      // https://groups.google.com/forum/#!topic/microsoft.public.win32.programmer.kernel/jam056kRtBA
      Result := true;
    myreg.Free;
  except
    on E: Exception do
      Logdatei.log('Exception in WinIsPE: '+E.ClassName+ ': '+ E.Message,LLError);
  end;
end;


//http://www.swissdelphicenter.ch/torry/showcode.php?id=109
//http://stackoverflow.com/questions/738856/test-if-disk-has-write-access
function IsDriveReady(Drive: string): Boolean;
var
  OldWinapiErrorMode:      Cardinal;
  Dw1, Dw2: DWORD;
begin
  OldWinapiErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  if Length(Drive) = 1 then Drive := Drive + ':\';
  if Length(Drive) = 2 then Drive := Drive + '\';
  try
    try
      Result := GetVolumeInformation(PChar(Drive), nil, 0, nil, Dw1, Dw2, nil, 0);
    except
      Result := false;
    end;
  finally
    SetErrorMode(OldWinapiErrorMode);
  end;
end;

function GetIPFromHost(var HostName, IPaddr, WSAErr: string): Boolean;
type
  Name = array[0..100] of Char;
  PName = ^Name;
var
  HEnt: pHostEnt;
  HName: PName;
  WSAData: TWSAData;
  i: Integer;
begin
  try
    Result := False;
    if WSAStartup($0101, WSAData) <> 0 then begin
      WSAErr := 'Winsock is not responding.';
      Exit;
    end;
    IPaddr := '';
    New(HName);
    //if GetHostName(HName^, SizeOf(Name)) = 0 then
    StrPCopy(HName^,Hostname);
    begin
      HostName := StrPas(HName^);
      HEnt := GetHostByName(HName^);
      for i := 0 to HEnt^.h_length - 1 do
       IPaddr :=
        Concat(IPaddr,
        IntToStr(Ord(HEnt^.h_addr_list^[i])) + '.');
      SetLength(IPaddr, Length(IPaddr) - 1);
      Result := True;
    end;
    (*
    else begin
     case WSAGetLastError of
      WSANOTINITIALISED:WSAErr:='WSANotInitialised';
      WSAENETDOWN      :WSAErr:='WSAENetDown';
      WSAEINPROGRESS   :WSAErr:='WSAEInProgress';
     end;
    end;
    *)
    Dispose(HName);
    WSACleanup;
  except
    WSAErr := 'Error resolving Host';
  end;
end;


end.

