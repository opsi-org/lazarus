unit osfuncwin2;

// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: Rupert Roeder, detlef oertel




{$mode delphi}
{$RANGECHECKS ON}
{$OVERFLOWCHECKS ON}
{$IOCHECKS ON}
{$OBJECTCHECKS ON}
{$VARSTRINGCHECKS ON}
{$LONGSTRINGS ON}


interface

uses
  Classes, SysUtils,
  Windows,
  osregistry,
  osfuncwin3,
{$IFNDEF WIN64}
  DSiWin32,
  uCpuUsage,
{$ENDIF WIN64}
  registry,
  oslog,
  JwaWinnt,
  jwawinbase,
  JwaWindows,
  //jwawinbase,
  JwaUserEnv,
  LCLIntf,
  charencstreams,
  //shellapi,
  Shlobj,
  Variants,
  ActiveX,
  JwaWbemCli,
  ostxstringlist,
  LAZUTF8,
  lazfileutils,
  Process;

//JclSecurity,
//JclWin32;

const
  SID_REVISION = 1;
  FILENAME_ADVAPI32 = 'ADVAPI32.DLL';
  PROC_CONVERTSIDTOSTRINGSIDA = 'ConvertSidToStringSidA';

type
  TConvertSidToStringSidA = function(Sid: PSID; var StringSid: LPSTR): BOOL; stdcall;
  TProfilesType = (tDirectory, tSID);



function GetDomainUserSidS(const domainName: string; const userName: string;
  var foundDomain: string): string;
function GetLocalUserSidStr(const UserName: string): string;
function getPid4user(const domain: string; const user: string; var pid: dword): boolean;
  overload;
function getPid4user(const domain: string; const user: string;
  var pid: dword; const searchexe: string): boolean; overload;
function Impersonate2User(const domain: string; const user: string): boolean; overload;
function Impersonate2User(const domain: string; const user: string;
  const usercontextsid: string): boolean; overload;
function Impersonate2User(const domain: string; const user: string;
  const usercontextsid: string; var usertok: THandle): boolean; overload;
function GetProcessUserBypid(pid: DWORD; var UserName, Domain: ansistring): boolean;
function KillProcessbyname(const exename: string; var found: integer): integer;
function KillProcessbypid(pid: DWORD): boolean;
function getWinProcessList: TStringList;
function getWinProcessListWithPath: TStringList;
//procedure myimpersontest;
function getloggedonDomUser: string;
//function GetLoggedInUserSID: string;
function GetUserName_: string;
function GetUserNameEx_: string;
{$IFDEF WIN32}
function GetProcessActivityByPid(pid: DWORD): real;
function GetProcessActivityByName(const exename: string; var found: boolean): real;
function DeleteTemporaryLocalAdmin: boolean;
function getWinlogonHandleForUser(const myuser: string; var myhandle: THandle): boolean;
{$ENDIF WIN32}



function getProfileImagePathfromSid(sid: string): string;
function StrSIDToName(const StrSID: string): string;
function CreateWinUser(const wServer, wUsername, wPassword, wGroup: WideString): boolean;
function CreateProcessElevated(lpApplicationName: PChar; lpCommandLine: string;
  lpCurrentDirectory: PChar; Counter: integer;
  var ProcessInfo: jwawinbase.TProcessInformation): boolean;
function OpenShellProcessToken(ProcessName: string; var hToken: THandle): boolean;
function OpenShellProcessInSessionToken(ProcessName: string;
  Sessionid: DWORD; Username: ansistring; horgToken: THandle;
  var hToken: THandle): boolean;

function winBlockInput(block: boolean): boolean;
function getMyIpByTarget(const myserver: ansistring): string;
function getIpMacHash: TStringList;
//function GetIPFromHost(var HostName, IPaddr, WSAErr: string): Boolean;
function mountSmbShare(drive: string; uncPath: string; Username: string;
  Password: string; RestoreAtLogon: boolean): DWORD;
function unmountSmbShare(driveOrPath: string; force: boolean): DWORD;
function getProfilesDirListWin: TStringList;
function getProfilesListWin(ProfilesType:TProfilesType):TStringList;


function winCreateHardLink(lpFileName: PChar; lpExistingFileName: PChar;
  lpSecurityAttributes: pointer): winbool;
function winCreateSymbolicLink(lpSymlinkFileName: PChar; lpExistingFileName: PChar;
  dwFlags: DWORD): winbool;
function updateEnvironment(): boolean;
function resolveWinSymlink(filepath: string; recursive: boolean = True): string;

implementation

uses
  osfunc, osfuncwin, wispecfolder
{$IFNDEF WIN64}
  , oslocaladmin
  {$ENDIF WIN64};

(*
type
   // Don't forget stdcall for API functions.
  TCreateHardLink = function: LPBOOL; stdcall;
  TCreateSymbolicLink = function: LPBOOL; stdcall;

var
  _CreateHardLink: TCreateHardLink;
  _CreateSymbolicLink: TCreateSymbolicLink;
*)



(*
function getProfileImagePathfromSid(sid: string): string;
var
  myreg: Tregistry;
  profilepath: string;
  files: TuibFileInstall;
  output: TXStringlist;
  commandline: string;
  report: string = '';
  errorinfo: string = '';
  i: integer;
begin
  Result := '';
  if Is64BitSystem then
  begin
    myreg := TRegistry.Create(KEY_ALL_ACCESS or KEY_WOW64_64KEY);
    Logdatei.Log('Registry started without redirection (64 Bit)', LLdebug3);
  end
  else
  begin
    myreg := TRegistry.Create(KEY_ALL_ACCESS or KEY_WOW64_32KEY);
    Logdatei.Log('Registry started with redirection (32 Bit)', LLdebug3);
  end;
  myreg.RootKey := HKEY_LOCAL_MACHINE;
  if myreg.KeyExists('SOFTWARE\Microsoft\Windows NT\CurrentVersion\ProfileList\' +
    sid) then
  begin
    myreg.OpenKey('SOFTWARE\Microsoft\Windows NT\CurrentVersion\ProfileList\' +
      sid, False);
    profilepath := myreg.ReadString('ProfileImagePath');
    myreg.CloseKey();
    profilepath := StringReplace(profilepath, '%SystemDrive%',
      extractfiledrive(GetWinDirectory));
    Logdatei.Log('found profile reg entry for sid: ' + sid +
      ' and path: ' + profilepath, LLdebug3);
    Result := profilepath;
  end;
  myreg.Free;
end;
*)

function getProfileImagePathfromSid(sid: string): string;
var
  StringResult: string = '';
  profilepath: string;
  files: TuibFileInstall;
  noredirect: boolean;
  aktkey: string;
begin
  Result := '';
  aktkey := 'HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows NT\CurrentVersion\ProfileList\'
    + sid;
  if Is64BitSystem then
  begin
    noredirect := True;
    Logdatei.Log('Registry started without redirection (64 Bit)', LLdebug3);
  end
  else
  begin
    noredirect := False;
    Logdatei.Log('Registry started with redirection (32 Bit)', LLdebug3);
  end;
  StringResult := GetRegistrystringvalue(aktkey, 'ProfileImagePath', noredirect);
  profilepath := StringReplace(StringResult, '%SystemDrive%',
    extractfiledrive(GetWinDirectory));
  LogDatei.log('found profile reg entry for sid: ' + sid + ' and path: ' +
    profilepath, LLdebug3);
  Result := profilepath;
end;


(*
function delUserProfile(sid: string): boolean;
var
  //sid : string;
  myreg: Tregistry;
  profilepath: string;
  files: TuibFileInstall;
  output: TXStringlist;
  commandline: string;
  report: string = '';
  errorinfo: string = '';
  i: integer;
  longintdummy: longint;

begin
  Result := False;

  //myreg := TRegistry.Create;
  if Is64BitSystem then
  begin
    myreg := TRegistry.Create(KEY_ALL_ACCESS or KEY_WOW64_64KEY);
    //myreg.KeyOpenMode := KEY_ALL_ACCESS or KEY_WOW64_64KEY;
    Logdatei.Log('Registry started without redirection (64 Bit)', LLdebug);
  end
  else
  begin
    myreg := TRegistry.Create(KEY_ALL_ACCESS or KEY_WOW64_32KEY);
    //myreg.KeyOpenMode := KEY_ALL_ACCESS or KEY_WOW64_32KEY;
    Logdatei.Log('Registry started with redirection (32 Bit)', LLdebug);
  end;
  myreg.RootKey := HKEY_LOCAL_MACHINE;
  if myreg.KeyExists('SOFTWARE\Microsoft\Windows NT\CurrentVersion\ProfileList\' +
    sid) then
  begin
    myreg.OpenKey('SOFTWARE\Microsoft\Windows NT\CurrentVersion\ProfileList\' +
      sid, False);
    profilepath := myreg.ReadString('ProfileImagePath');
    myreg.CloseKey();
    if DeleteProfile(PChar(sid), nil, nil) then
    begin
      Logdatei.Log('Deleted profile of temporary Admin opsiSetupAdmin ',
        LLDebug);
      Result := True;
    end
    else
    begin
      Logdatei.Log('Could not delete profile for sid : ' +
        sid + ' - ' + IntToStr(GetLastError) + ' (' +
        SysErrorMessage(GetLastError) + ')', LLNotice);

      myreg.DeleteKey('SOFTWARE\Microsoft\Windows NT\CurrentVersion\ProfileList\' + sid);
      //myreg.free;
      profilepath := StringReplace(profilepath, '%SystemDrive%',
        extractfiledrive(GetWinDirectory));
      Logdatei.Log('deleted profile reg entry for sid: ' +
        sid + ' and path: ' + profilepath, LLdebug);
      if DirectoryExists(profilepath) then
      begin
        Logdatei.Log('profilepath exists: ' + profilepath, LLdebug);
        output := TXStringlist.Create;
        commandline := 'cmd.exe /c  FOR /D %A IN ("' + GetProfilesPath +
          '\opsiSetupAdmin.*") DO rmdir /s /q "%A"';
        Logdatei.Log('Executing ' + commandline, LLinfo);
        if not RunCommandAndCaptureOut(commandline, True, output,
          report, SW_HIDE, longintdummy) then
        begin
          Logdatei.Log('Error: ' + Report, LLError);
        end
        else
        begin
          LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 6;
          Logdatei.Log('', LLinfo);
          Logdatei.Log('output:', LLinfo);
          Logdatei.Log('--------------', LLinfo);
          for i := 0 to output.Count - 1 do
          begin
            Logdatei.Log(output.strings[i], LLinfo);
          end;
          LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 6;
          Logdatei.Log('', LLinfo);
        end;
        output.Free;
        if not DirectoryExists(profilepath) then
        begin
          Result := True;
          Logdatei.Log('deleted profile: ' + profilepath, LLdebug);
        end;
      end;
    end;
  end;
end;
*)

function delUserProfile(sid: string): boolean;
var
  //sid : string;
  myreg: Tregistry;
  profilepath: string;
  files: TuibFileInstall;
  output: TXStringlist;
  commandline: string;
  report: string = '';
  errorinfo: string = '';
  i: integer;
  longintdummy: longint;

begin
  Result := False;
  if DeleteProfile(PChar(sid), nil, nil) then
  begin
    LogDatei.log('Deleted profile of: ' + sid,
      LLDebug);
    Result := True;
  end
  else
  begin
    logdatei.log('Could not delete profile for sid : ' + sid + ' - ' +
      IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) + ')', LLNotice);
    profilepath := getProfileImagePathfromSid(sid);
    if DirectoryExists(profilepath) then
    begin
      Logdatei.Log('profilepath exists: ' + profilepath, LLdebug);
      output := TXStringlist.Create;
      commandline := 'cmd.exe /c  FOR /D %A IN ("' + GetProfilesPath +
        '\opsiSetupAdmin.*") DO rmdir /s /q "%A"';
      Logdatei.Log('Executing ' + commandline, LLinfo);
      if not RunCommandAndCaptureOut(commandline, True, output,
        report, SW_HIDE, longintdummy) then
      begin
        Logdatei.Log('Error: ' + Report, LLError);
      end
      else
      begin
        LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 6;
        Logdatei.Log('', LLinfo);
        Logdatei.Log('output:', LLinfo);
        Logdatei.Log('--------------', LLinfo);
        for i := 0 to output.Count - 1 do
        begin
          Logdatei.Log(output.strings[i], LLinfo);
        end;
        LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 6;
        Logdatei.Log('', LLinfo);
      end;
      output.Free;
      if not DirectoryExists(profilepath) then
      begin
        Result := True;
        Logdatei.Log('deleted profile: ' + profilepath, LLdebug);
      end;
    end;
  end;
end;

function GetLoggedInUserSID: string;
var
  Output: string;
  Command: string;
begin
  Command := '([System.Security.Principal.NTAccount](Get-WMIObject -class Win32_ComputerSystem ' +
    '| Select-Object -Property username).username).Translate([System.Security.Principal.SecurityIdentifier]).Value';
  if RunCommand('powershell',['-c', Command], Output,[poUsePipes], swoHide) then
    Result := trim(Output)
  else
    Result := 'Unkown_SID';
end;

{:Returns user name of the current thread.
  @author  Miha-R, Lee_Nover
  @since   2002-11-25
}
function GetUserName_: string;
var
  buffer: LPWSTR;
  bufferSize: DWORD;
begin
  bufferSize := 256; //UNLEN from lmcons.h
  buffer := AllocMem(bufferSize * SizeOf(char));
  try
    GetUserNameW(buffer, bufferSize);
    Result := UTF16ToUTF8(unicodestring(buffer));
  finally
    // bufferSize may be changed by GetUserNameW
    // so we free 256
    FreeMem(buffer, 256);
  end;
end; { DSiGetUserName }

{:Returns name of the user owning the desktop (currently logged user).
  @author  Lee_Nover
  @since   2003-09-03
}
function GetUserNameEx_: string;
var
  dwProcessId: DWORD;
  h: HWND;
  hProcess: THandle;
  hToken: THandle;
begin
  Result := '';
  h := FindWindow('Progman', nil);// maybe use GetDesktopWindow
  if h = 0 then
    Exit;
  if GetWindowThreadProcessId(h, @dwProcessId) = 0 then
    Exit;
  hProcess := OpenProcess(PROCESS_ALL_ACCESS, False, dwProcessId);
  if hProcess = 0 then
    Exit;
  try
    if OpenProcessToken(hProcess, TOKEN_ALL_ACCESS, hToken) then
      try
        ImpersonateLoggedOnUser(hToken);
        try
          Result := GetUserName_;
        finally
          RevertToSelf;
        end;
      finally
        CloseHandle(hToken);
      end;
  finally
    CloseHandle(hProcess);
  end;
end; { TDSiRegistry.DSiGetUserNameEx }

// -------------------------------------------
// functions to retrieve the SID for a user




function WinGetSidStr(Sid: PSid): string;
var
  SidToStr: TConvertSidToStringSidA;
  h: longword;
  Buf: array [0..MAX_PATH - 1] of char;
  p: pansichar;
begin
  h := LoadLibrary(FILENAME_ADVAPI32);
  if h <> 0 then
    try
      @SidToStr := GetProcAddress(h, PROC_CONVERTSIDTOSTRINGSIDA);
      if @SidToStr <> nil then
      begin
        FillChar(Buf, SizeOf(Buf), 0);

        if SidToStr(Sid, p) then
          Result := '[' + string(p) + ']';

        LocalFree(longword(p));
      end;
    finally
      FreeLibrary(h);
    end;
end;

function GetSidStr(Sid: PSid): string;
var
  Psia: PSIDIdentifierAuthority;
  SubAuthCount: longword;
  i: longword;
begin
  if IsValidSid(Sid) then
  begin

    // Win 2000+ contains ConvertSidToStringSidA() in advapi32.dll so we just
    // use it if we can

    if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 5) then
      Result := WinGetSidStr(Sid)
    else
    begin
      Psia := GetSidIdentifierAuthority(Sid);
      SubAuthCount := GetSidSubAuthorityCount(Sid)^;
      Result := Format('[S-%u-', [SID_REVISION]);
      if ((Psia.Value[0] <> 0) or (Psia.Value[1] <> 0)) then
        Result := Result + Format('%.2x%.2x%.2x%.2x%.2x%.2x',
          [Psia.Value[0], Psia.Value[1], Psia.Value[2], Psia.Value[3],
          Psia.Value[4], Psia.Value[5]])
      else
        Result := Result + Format('%u', [longword(Psia.Value[5]) +
          longword(Psia.Value[4] shl 8) + longword(Psia.Value[3] shl 16) +
          longword(Psia.Value[2] shl 24)]);

      for i := 0 to SubAuthCount - 1 do
        Result := Result + Format('-%u', [GetSidSubAuthority(Sid, i)^]);

      Result := Result + ']';
    end;
  end;
end;

function GetDomainUserSidS(const DomainName: string; const UserName: string;
  var foundDomain: string): string;
var
  RefDomain: array [0..MAX_PATH - 1] of char;      // enough
  RefDomainSize: longword;
  Snu: SID_NAME_USE;
  Sid: PSid;
  SidSize: longword;

begin
  SidSize := 0;
  RefDomainSize := SizeOf(RefDomain);
  Sid := nil;
  FillChar(RefDomain, SizeOf(RefDomain), 0);
  if DomainName = '' then
    LookupAccountName(nil, PChar(UserName), Sid, SidSize, RefDomain, RefDomainSize, Snu)
  else
    LookupAccountName(PChar(DomainName), PChar(UserName), Sid, SidSize, RefDomain,
      RefDomainSize, Snu);
  Sid := AllocMem(SidSize);
  try
    RefDomainSize := SizeOf(RefDomain);
    if DomainName = '' then
    begin
      if LookupAccountName(nil, PChar(UserName), Sid, SidSize,
        RefDomain, RefDomainSize, Snu) then
        Result := GetSidStr(Sid);
    end
    else
    begin
      if LookupAccountName(PChar(DomainName), PChar(UserName), Sid,
        SidSize, RefDomain, RefDomainSize, Snu) then
        Result := GetSidStr(Sid);
    end;

    foundDomain := string(RefDomain);

  finally
    FreeMem(Sid, SidSize);
  end;
end;



function GetLocalUserSidStr(const UserName: string): string;
var
  RefDomain: array [0..MAX_PATH - 1] of char;      // enough
  RefDomainSize: longword;
  Snu: SID_NAME_USE;
  Sid: PSid;
  SidSize: longword;
begin
  SidSize := 0;
  RefDomainSize := SizeOf(RefDomain);
  Sid := nil;
  FillChar(RefDomain, SizeOf(RefDomain), 0);
  LookupAccountName(nil, PChar(UserName), Sid, SidSize, RefDomain,
    RefDomainSize, Snu);
  Sid := AllocMem(SidSize);
  try
    RefDomainSize := SizeOf(RefDomain);
    if LookupAccountName(nil, PChar(UserName), Sid, SidSize, RefDomain,
      RefDomainSize, Snu) then
      Result := GetSidStr(Sid);
  finally
    FreeMem(Sid, SidSize);
  end;
end;

function getloggedonDomUser: string;
var
  pid: DWORD;
  h: HWND;
  myuser: ansistring = '';
  mydomain: ansistring = '';
begin
  Result := '';
  h := FindWindow('Progman', nil);// maybe use GetDesktopWindow
  if h <> 0 then
  begin
    if GetWindowThreadProcessId(h, @pid) <> 0 then
    begin
      if not GetProcessUserBypid(pid, myuser, mydomain) then
        Logdatei.Log('Could not get user for pid: ' + IntToStr(pid), LLWarning);
    end
    else
      Logdatei.Log('Could not get pid for current session', LLWarning);
  end
  else
    Logdatei.Log('Could not get handle for current session: no user',
      LLWarning);
  if not (myuser = GetUserNameEx_) then
    Logdatei.Log('Strange: different users found: ' + myuser +
      ' + ' + GetUserNameEx_, LLWarning);
  Logdatei.Log('Session owner found: ' + mydomain + '\' + myuser, LLInfo);
  Result := mydomain + '\' + myuser;
end;



function Impersonate2loggedonUser: boolean;
var
  dwProcessId: DWORD;
  h: HWND;
  hProcess: THandle;
  hToken: THandle = 0;
begin
  Result := False;
  h := FindWindow('Progman', nil);// maybe use GetDesktopWindow
  if h = 0 then
    Exit;
  if GetWindowThreadProcessId(h, @dwProcessId) = 0 then
    Exit;
  hProcess := OpenProcess(PROCESS_ALL_ACCESS, False, dwProcessId);
  if hProcess = 0 then
    Exit;
  try
    if OpenProcessToken(hProcess, TOKEN_ALL_ACCESS, hToken) then
      try
        if ImpersonateLoggedOnUser(hToken) then
          Result := True;
      finally
        CloseHandle(hToken);
      end;
  finally
    CloseHandle(hProcess);
  end;
end; { TDSiRegistry.DSiGetUserNameEx }

function Impersonate2User(const domain: string; const user: string): boolean;
var
  usercontextsid: string;
begin
  Result := Impersonate2User(domain, user, usercontextsid);
end;

function Impersonate2User(const domain: string; const user: string;
  const usercontextsid: string): boolean;
var
  usertok: THandle;
begin
  Result := Impersonate2User(domain, user, usercontextsid, usertok);
end;

function Impersonate2User(const domain: string; const user: string;
  const usercontextsid: string; var usertok: THandle): boolean;
var
  dwProcessId: DWORD = 0;
  //h: HWND;
  hProcess: THandle;
  hToken: THandle = 0;
  pidfound: boolean;
  retrycounter: integer;
  proclist: TStringList;
  str: string;
  i: integer;
begin
  Result := False;
  pidfound := False;
  retrycounter := 0;
  repeat
    pidfound := getPid4user(domain, user, dwProcessId, 'explorer.exe');
    if not pidfound then
    begin
      LogDatei.log('Retry waiting for explorer.exe', LLDebug2);
      Sleep(500);
    end;
    Inc(retrycounter);
  until pidfound or (retrycounter > 5);
  if pidfound then
  begin
    hProcess := OpenProcess(PROCESS_ALL_ACCESS, False, dwProcessId);
    if hProcess = 0 then
      Exit;
    try
      if OpenProcessToken(hProcess, TOKEN_ALL_ACCESS, hToken) then
        try
          if ImpersonateLoggedOnUser(hToken) then
            Result := True;
        finally
          CloseHandle(hToken);
        end;
    finally
      CloseHandle(hProcess);
    end;
  end
  else
  begin
    LogDatei.log(
      'Error: got no pid at Impersonate2User/getPid4user for user:' + user, LLError);
    LogDatei.log('Warning: Here comes the process list:', LLWarning);
    proclist := getWinProcessList;
    for i := 0 to proclist.Count - 1 do
      LogDatei.log(proclist.Strings[i], LLWarning);
    if usercontextsid <> '' then
    begin
      LogDatei.log('usercontextsid: ' + usercontextsid, LLWarning);
      str := GetRegistrystringvalue('HKEY_USERS\' + usercontextsid +
        '\Volatile Environment', 'USERDOMAIN', True);
      LogDatei.log('HKEY_USERS\...\Volatile Environment: USERDOMAIN : ' + str,
        LLWarning);
      str := GetRegistrystringvalue('HKEY_USERS\' + usercontextsid +
        '\Volatile Environment', 'USERNAME', True);
      LogDatei.log('HKEY_USERS\...\Volatile Environment: USERNAME : ' + str, LLWarning);
    end;

  end;

end; { TDSiRegistry.DSiGetUserNameEx }

(*
procedure myimpersontest;
var
  regist: Tregistry;
  str: string;
  impersonator: TImpersonator;
begin
  regist := Tregistry.Create;
  impersonator := TImpersonator.CreateLoggedOn;
  try
    //DSiImpersonateUser(edit1.Caption,edit2.Caption,edit3.Caption);
    //label5.Text := DSiReadRegistry('Software\opsi.org\test','myname','not-found').asstring;
    regist.RootKey := HKEY_CURRENT_USER;
    if Regist.OpenKey('\Software\opsi.org\test', False) then
    begin
      str := regist.ReadString('myname');
      Logdatei.Log(str, LLEssential);
      //memo1.Append(str);
    end
    else
      Logdatei.Log('failed', LLEssential);
    //memo1.Append('DSiGetUserNameEx :'+DSiGetUserNameEx);
    //RetrieveSIDInfo(
    //('Software\opsi.org\test','myname','not-found').asstring;
    //DSiExecute(edit4.Caption);
    //DSiStopImpersonatingUser;
  finally
    impersonator.Free  // Revert to the SYSTEM account
  end;
  Logdatei.Log('DSiGetUserNameEx :' + DSiGetUserNameEx, LLEssential);
  Logdatei.Log('DSiGetDomain :' + DSiGetDomain, LLEssential);
  Logdatei.Log('GetDomainUserSidS :' + GetDomainUserSidS(
    DSiGetDomain, DSiGetUserNameEx, str), LLEssential);
  Logdatei.Log('GetLocalUserSidStr :' + GetLocalUserSidStr(
    DSiGetUserNameEx), LLEssential);
  //DSiGetDomain
  //GetDomainUserSidS
  //GetLocalUserSidStr
  regist.RootKey := HKEY_USERS;
  str := GetLocalUserSidStr(DSiGetUserNameEx);
  str := copy(str, 2, length(str) - 2);
  Logdatei.Log('sidStr :' + str, LLEssential);
  if Regist.OpenKey('\' + str + '\Software\opsi.org\test', False) then
  begin
    str := regist.ReadString('myname');
    Logdatei.Log('read from hkusers: ' + str, LLEssential);
    //memo1.Append(str);
  end
  else
    Logdatei.Log('openkey failed', LLEssential);
  if Impersonate2loggedonUser then
    Logdatei.Log('Desktop :' + DSiGetFolderLocation(
      CSIDL_DESKTOPDIRECTORY), LLEssential)
  else
    Logdatei.Log('impersonate failed :' + DSiGetFolderLocation(
      CSIDL_DESKTOPDIRECTORY), LLEssential);
  RevertToSelf;
  regist.Free;
end;
*)



// functions to retrieve the SID for a user
// -------------------------------------------

// Process list
//http://www.lazarus.freepascal.org/index.php?topic=3543.0

//http://www.delphigeist.com/2010/03/process-list.html

//http://www.delphi-library.de/topic_Prozessliste+erstellen_41114,0.html
//http://www.delphi-forum.de/viewtopic.php?p=563144&sid=db0be57e7b505dc7e7d0f4700653674f
//http://support.microsoft.com/kb/175030/de
function getWinProcessList: TStringList;
var
  ContinueLoop: boolean;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
  resultstring: string;
  UserName, Domain: ansistring;
begin
  Result := TStringList.Create;
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := Sizeof(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
  while ContinueLoop do
  begin
    resultstring := '';
    UserName := '';
    domain := '';

    resultstring := FProcessEntry32.szExeFile + ';' + IntToStr(
      FProcessEntry32.th32ProcessID) + ';';
    if GetProcessUserBypid(FProcessEntry32.th32ProcessID, UserName, Domain) then
      resultstring := resultstring + Domain + '\' + UserName;
    //if FProcessEntry32.th32ProcessID > 0 then
    //  resultstring := resultstring + ';' + FormatFloat('##0.00',GetProcessActivityByPid(FProcessEntry32.th32ProcessID))
    //else resultstring := resultstring + ';';
    resultstring := WinCPToUTF8(resultstring);
    Result.add(resultstring);
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;

//**********************************************************************************************************************
// https://wiki.lazarus.freepascal.org/Windows_Programming_Tips#Showing.2Ffinding_processes
// https://learn.microsoft.com/de-de/windows/win32/api/winbase/nf-winbase-queryfullprocessimagenamew
// https://github.com/alrieckert/lazarus/blob/master/components/fpdebug/fpdbgwinclasses.pas
// an external reference nust be declared as a global function (cannot be declared as a local function):
function QueryFullProcessImageName(hProcess:HANDLE; dwFlags: DWord; lpExeName:LPWSTR;
  var lpdwSize:DWORD):BOOL; stdcall; external 'kernel32' name 'QueryFullProcessImageNameW';

function getWinProcessListWithPath: TStringList;
var
  ContinueLoop: boolean;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
  resultstring: string;
  UserName, Domain: ansistring;

  //**************************************************************************
  //** alternative 1 with GetModuleFileNameEx
  //**
  //** from https://swissdelphicenter.ch/en/showcode.php?id=2010

  function GetFullProcessFileNameV1(PID: DWORD): string;
  var
    Handle: THandle;
  begin
    Result := '';
    Handle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, PID);
    if Handle <> 0 then
      try
        SetLength(Result, MAX_PATH);
        if GetModuleFileNameEx(Handle, 0, PChar(Result), MAX_PATH) > 0 then
          SetLength(Result, StrLen(PChar(Result)))
        else
          Result := '';
      finally
        CloseHandle(Handle);
      end
    else
      begin
        Result := '!!! NO HANDLE !!!';
      end;
  end;

  //**************************************************************************
  //** alternative 2 with QueryFullProcessImageName
  //**
  //** from https://github.com/alrieckert/lazarus/blob/master/components/fpdebug/fpdbgwinclasses.pas

  function GetFullProcessImageName(AProcessHandle: THandle): string;
  var
    u: UnicodeString;
    len: DWORD;
  begin
    len := MAX_PATH;
    SetLength(u, len);
    if QueryFullProcessImageName(AProcessHandle, 0, @u[1], len)
    then begin
      SetLength(u, len);
      Result:=UTF8Encode(u);
    end else begin
      Result := '';
    end;
  end;

  function GetFullProcessFileNameV2(PID: DWORD): string;
  var
    Handle: THandle;
  begin
    Result := '';
    Handle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, PID);
    if Handle <> 0 then
      try
        Result := GetFullProcessImageName(Handle);
      finally
        CloseHandle(Handle);
      end
    else
      begin
        Result := '!!! NO HANDLE !!!';
      end;
  end;
  //**************************************************************************

begin
  Result := TStringList.Create;
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := Sizeof(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
  while ContinueLoop do
  begin
    resultstring := '';
    UserName := '';
    domain := '';

    resultstring := FProcessEntry32.szExeFile + ';' + IntToStr(
      FProcessEntry32.th32ProcessID) + ';';
    if GetProcessUserBypid(FProcessEntry32.th32ProcessID, UserName, Domain) then
      resultstring := resultstring + Domain + '\' + UserName;
    resultstring := resultstring + ';' + GetFullProcessFileNameV2(FProcessEntry32.th32ProcessID);
    resultstring := WinCPToUTF8(resultstring);
    Result.add(resultstring);
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;


(*
function getWinProcessList:TStringList;
var PrIDs : Array [0..1000] of DWORD;
      bia : DWORD;
  PrCount : Integer;
  ProzessHandle : HWND;
  Modulhandle : HWND;
  i : Integer;
  PrName : Array [0..255] of Char;
  resultstring : string;
  var UserName, Domain: AnsiString;
begin
Result:=TStringList.Create;
if EnumProcesses(@PrIDs, SizeOf(PrIDs), bia) then
 if bia < sizeof(PrIDs) then
  begin
   PrCount:=Bia div SizeOf(DWORD);
   for i:=0 to PrCount do
    begin
     resultstring := '';
     UserName := '';
     domain :='';
     ProzessHandle := OpenProcess(PROCESS_QUERY_INFORMATION or
                                  PROCESS_VM_READ, false, PrIDS[i]);
     if ProzessHandle<>0 then
      begin
       EnumProcessModules(ProzessHandle, @ModulHandle, SizeOf(modulhandle), bia);
       //GetModuleFilenameEx(Prozesshandle, ModulHandle, PrName, SizeOf(PrName));
       GetModuleBaseName(Prozesshandle, ModulHandle, PrName, SizeOf(PrName));
       CloseHandle(ProzessHandle);
      end;
      resultstring := PrName+';'+IntToStr(PrIDS[i])+';';
      if  GetProcessUserBypid(PrIDS[i],UserName, Domain) then
          resultstring :=  resultstring + Domain+'\'+UserName;
      Result.add(resultstring);
    end;
  end else Logdatei.Log('Error: getting process list',LLError);
 //else //RaiseLastOSError(); //if enumprocesses...
end;
*)
function GetProcessUserBypid(pid: DWORD; var UserName, Domain: ansistring): boolean;
  // from http://www.delphigeist.com/2010/03/process-list.html
var
  hToken: THandle = 0;
  cbBuf: cardinal = 0;
  tokUser: PTOKEN_USER;
  sidNameUse: SID_NAME_USE;
  hProcess: THandle;
  UserSize, DomainSize: DWORD;
  bSuccess: boolean;
begin
  Result := False;
  hProcess := OpenProcess(PROCESS_QUERY_INFORMATION, False, pid);
  if hProcess <> 0 then
  begin
    if OpenProcessToken(hProcess, TOKEN_QUERY, hToken) then
    begin
      bSuccess := GetTokenInformation(hToken, TokenUser, nil, 0, cbBuf);
      tokUser := nil;
      while (not bSuccess) and (GetLastError = ERROR_INSUFFICIENT_BUFFER) do
      begin
        ReallocMem(tokUser, cbBuf);
        bSuccess := GetTokenInformation(hToken, TokenUser, tokUser, cbBuf, cbBuf);
      end;// while (not bSuccess) and...
      CloseHandle(hToken);
      if not bSuccess then
        Exit;
      UserSize := 0;
      DomainSize := 0;
      LookupAccountSid(nil, tokUser.User.Sid, nil, UserSize, nil,
        DomainSize, sidNameUse);
      if (UserSize <> 0) and (DomainSize <> 0) then
      begin
        SetLength(UserName, UserSize);
        SetLength(Domain, DomainSize);
        if LookupAccountSid(nil, tokUser.User.Sid, pansichar(UserName),
          UserSize, pansichar(Domain), DomainSize, sidNameUse) then
        begin
          Result := True;
          UserName := StrPas(pansichar(UserName));
          Domain := StrPas(pansichar(Domain));
        end;// if LookupAccountSid(nil, tokUser.User.Sid, PAnsiChar(UserName), UserSize,
      end;// if (UserSize <> 0) and (DomainSize <> 0) then begin
      if bSuccess then
        FreeMem(tokUser);
    end;// if OpenProcessToken(hProcess, TOKEN_QUERY, hToken) then begin
    CloseHandle(hProcess);
  end;// if hProcess <> 0 then begin
end;// function TDGProcessList.GetProcessUserAndDomain(dwProcessID: DWORD;

function GetUserByToken(hToken: THandle; var UserName, Domain: ansistring): boolean;
  // from http://www.delphigeist.com/2010/03/process-list.html
var
  cbBuf: cardinal = 0;
  tokUser: PTOKEN_USER;
  sidNameUse: SID_NAME_USE;
  hProcess: THandle;
  UserSize, DomainSize: DWORD;
  bSuccess: boolean;
begin
  Result := False;
  bSuccess := GetTokenInformation(hToken, TokenUser, nil, 0, cbBuf);
  tokUser := nil;
  while (not bSuccess) and (GetLastError = ERROR_INSUFFICIENT_BUFFER) do
  begin
    ReallocMem(tokUser, cbBuf);
    bSuccess := GetTokenInformation(hToken, TokenUser, tokUser, cbBuf, cbBuf);
  end;// while (not bSuccess) and...
  CloseHandle(hToken);
  if not bSuccess then
    Exit;
  UserSize := 0;
  DomainSize := 0;
  LookupAccountSid(nil, tokUser.User.Sid, nil, UserSize, nil,
    DomainSize, sidNameUse);
  if (UserSize <> 0) and (DomainSize <> 0) then
  begin
    SetLength(UserName, UserSize);
    SetLength(Domain, DomainSize);
    if LookupAccountSid(nil, tokUser.User.Sid, pansichar(UserName),
      UserSize, pansichar(Domain), DomainSize, sidNameUse) then
    begin
      Result := True;
      UserName := StrPas(pansichar(UserName));
      Domain := StrPas(pansichar(Domain));
    end;// if LookupAccountSid(nil, tokUser.User.Sid, PAnsiChar(UserName), UserSize,
  end;// if (UserSize <> 0) and (DomainSize <> 0) then begin
  if bSuccess then
    FreeMem(tokUser);
end;

function GetSessionByToken(hToken: THandle; var SessionId: DWORD): boolean;
var
  cbBuf: cardinal = 0;
  TokenInfo: Dword;
  bSuccess: boolean;
begin
  Result := False;
  bSuccess := GetTokenInformation(hToken, TokenUser, @TokenInfo,
    sizeof(TokenInfo), cbBuf);
  CloseHandle(hToken);
  if not bSuccess then
    Exit;
  SessionId := TokenInfo;
  Result := True;
end;

(*
function getPid4user(const domain: string; const user: string; var pid: dword): boolean;
var
  PrIDs: array [0..1000] of DWORD;
  bia: DWORD=0;
  PrCount, i: integer;
  founduser: string='';
  founddomain: string='';
begin
  Result := False;
  if EnumProcesses(@PrIDs, SizeOf(PrIDs), bia) then
    if bia < sizeof(PrIDs) then
    begin
      PrCount := Bia div SizeOf(DWORD);
      for i := 0 to PrCount do
      begin
        if GetProcessUserByPid(PrIDS[i], founduser, founddomain) then
        begin
          if founduser = user then
          begin
            Result := True;
            pid := PrIDS[i];
            exit;
          end;
        end;
      end;
    end
    else
      Logdatei.Log('Error: getting process list', LLError)
  else
    RaiseLastOSError(); //if enumprocesses...
end;
*)
function getPid4user(const domain: string; const user: string; var pid: dword): boolean;
begin
  Result := getPid4user(domain, user, pid, '');
end;

function getPid4user(const domain: string; const user: string;
  var pid: dword; const searchexe: string): boolean;
var
  //pid: dword;
  //user, domain,
  //myuser, mydomain,
  foundexe, domuser: string;
  //winstuser, winstdom: ansistring;
  h: HWND;
  proclist: TStringList;
  procdetails: TXStringlist;
  i: integer;
  found: boolean;
begin
  Result := False;
  found := False;
  i := 0;
  procdetails := TXStringlist.Create;
  proclist := getWinProcessList;
  while (not found) and (i < proclist.Count) do
  begin
    procdetails.Clear;
    stringsplit(proclist.Strings[i], ';', procdetails);
    //Logdatei.Log(proclist.Strings[i],LLDebug);
    //Logdatei.Log('analyze: exe: ' + procdetails.Strings[0] +
    //  ' pid: ' + procdetails.Strings[1] + ' from user: ' + procdetails.Strings[2], LLEssential);
    if procdetails.Strings[1] <> '' then
    begin
      try
        pid := StrToInt(procdetails.Strings[1]);
      except
        pid := 0;
      end;
      foundexe := procdetails.Strings[0];
      domuser := procdetails.Strings[2];
      if (searchexe = '') or (LowerCase(searchexe) = LowerCase(foundexe)) then
        if (domuser = domain + '\' + user) then
        begin
          found := True;
          LogDatei.log('Found pid: ' + IntToStr(pid) + ' for user: ' +
            domuser + ' and exe: ' + foundexe, LLDebug);
          Result := True;
        end
        else
          LogDatei.log('Found exe: ' + foundexe + ' but for wrong user: ' +
            domuser, LLDebug2);
    end;
    Inc(i);
  end;
  if not found then
    LogDatei.log('Found no pid for user: ' + domain + '\' + user +
      ' and exe: ' + searchexe, LLError);
end;



//http://www.delphipraxis.net/156073-klasse-zum-beenden-eines-prozesses-4.html
(*
function EnablePrivilege(const Privilege: string;
  fEnable: Boolean; out PreviousState: Boolean): DWORD;
var
  Token: THandle;
  NewState: TTokenPrivileges;
  Luid: TLargeInteger;
  PrevState: TTokenPrivileges;
  Return: DWORD;
begin
  PreviousState := True;
  if (GetVersion() > $80000000) then
    // Win9x
    Result := ERROR_SUCCESS
  else
  begin
    // WinNT
    if not OpenProcessToken(GetCurrentProcess(), MAXIMUM_ALLOWED, Token) then
      Result := GetLastError()
    else
    try
      if not LookupPrivilegeValue(nil, PChar(Privilege), Luid) then
        Result := GetLastError()
      else
      begin
        NewState.PrivilegeCount := 1;
        NewState.Privileges[0].Luid := Luid;
        if fEnable then
          NewState.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED
        else
          NewState.Privileges[0].Attributes := 0;
        if not AdjustTokenPrivileges(Token, False, NewState,
          SizeOf(TTokenPrivileges), PrevState, Return) then
          Result := GetLastError()
        else
        begin
          Result := ERROR_SUCCESS;
          PreviousState :=
            (PrevState.Privileges[0].Attributes and SE_PRIVILEGE_ENABLED <> 0);
        end;
      end;
    finally
      CloseHandle(Token);
    end;
  end;
end;
*)

function KillProcessbypid(pid: DWORD): boolean;
var
  hProcess: THandle;
begin
  Result := False;
  {$IFDEF WIN32}
  if not DSiEnablePrivilege('SE_DEBUG_NAME') then
    Logdatei.Log(
      'Warning: Could not get SE_DEBUG_NAME Privelege to kill process -> will try with out',
      LLWarning);
  hProcess := OpenProcess(PROCESS_ALL_ACCESS, False, pid);
  if hProcess <> 0 then
  begin
    Logdatei.Log('Try to kill process with pid: ' + IntToStr(pid), LLDebug);
    Result := TerminateProcess(hProcess, 0);
    Logdatei.Log('killed process with pid: ' + IntToStr(pid), LLDebug);
    CloseHandle(hProcess);
  end;// if hProcess <> 0 then begin
  {$ENDIF WIN32}
end;// KillProcessbypid(pid: DWORD): Boolean;


function getpid4exe(const exename: string; var pid: dword): boolean;
var
  //PrIDs: array [0..1000] of DWORD;
  //bia: DWORD;
  //PrCount, i: integer;
  //ProzessHandle, ModulHandle: HWND;
  //PrName: array [0..255] of char;
  ContinueLoop: boolean;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  Result := False;
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := Sizeof(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
  while ContinueLoop do
  begin
    if AnsiSameText(FProcessEntry32.szExeFile, exename) then
    begin
      pid := FProcessEntry32.th32ProcessID;
      ContinueLoop := False;
      Result := True;
    end
    else
      ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;

function KillProcessbyname(const exename: string; var found: integer): integer;
var
  pid: dword;
  //user, domain,
  myuser, mydomain, foundexe, domuser, selfuser, selfdom: ansistring;
  h: HWND;
  proclist: TStringList;
  procdetails: TXStringlist;
  i: integer;
  selfprocess: string;
begin
  Result := 0;
  found := 0;
  myuser := '';
  mydomain := '';
  selfuser := '';
  selfdom := '';
  procdetails := TXStringlist.Create;
  h := FindWindow('explorer', nil);
  if h <> 0 then
  begin
    if GetWindowThreadProcessId(h, @pid) <> 0 then
    begin
      if not GetProcessUserBypid(pid, myuser, mydomain) then
        LogDatei.log('Could not get user for pid: ' + IntToStr(pid), LLDebug2);
    end
    else
      LogDatei.log('Could not get pid for current session', LLDebug2);
  end
  else
    LogDatei.log('Could not get handle for current session: no user', LLDebug2);
  if not (myuser = GetUserNameEx_) then
    LogDatei.log('Strange: different users found: ' + myuser + ' + ' +
      GetUserNameEx_, LLDebug);
  LogDatei.log('Session owner found: ' + mydomain + '\' + myuser, LLDebug);
  selfprocess := ExtractFileName(ParamStr(0));
  if getpid4exe(selfprocess, pid) then
  begin
    if GetProcessUserBypid(pid, selfuser, selfdom) then
    begin
      LogDatei.log_prog('opsi-script owner found: ' + selfdom + '\' + selfuser, LLDebug);
    end
    else
      LogDatei.log('Could not get owner for current ' + selfprocess, LLDebug);
  end
  else
    LogDatei.log('Could not get pid for current ' + selfprocess, LLDebug);

  //while getpid4exe(exename,pid) do
  proclist := getWinProcessList;
  for i := 0 to proclist.Count - 1 do
  begin
    procdetails.Clear;
    stringsplit(proclist.Strings[i], ';', procdetails);
    //Logdatei.Log(proclist.Strings[i],LLDebug);
    LogDatei.log_prog('analyze: exe: ' + procdetails.Strings[0] +
      ' pid: ' + procdetails.Strings[1] + ' from user: ' +
      procdetails.Strings[2], LLDebug);
    if procdetails.Strings[1] <> '' then
    begin
      try
        pid := StrToInt(procdetails.Strings[1]);
      except
        pid := 0;
      end;
      foundexe := procdetails.Strings[0];
      domuser := procdetails.Strings[2];
      if UpperCase(exename) = UpperCase(foundexe) then
      begin
        if (domuser = mydomain + '\' + myuser) or
          (domuser = selfdom + '\' + selfuser) or (domuser = '') or
          (selfuser = 'SYSTEM') then
        begin
          LogDatei.log('Will kill exe: ' + foundexe + ' pid: ' +
            IntToStr(pid) + ' from user: ' + domuser, LLDebug);
          Inc(found);
          if KillProcessbypid(pid) then
            Result := Result + 1;
        end
        else
          LogDatei.log('Will not kill exe: ' + foundexe + ' pid: ' +
            IntToStr(pid) + ' from user: ' + domuser, LLDebug);
      end;
      //else Logdatei.Log('No user found for exe: '+exename+' and pid: '+IntToStr(pid),LLDebug);
    end;
  end;
end;// KillProcessbyname(const exename : string;): Boolean;



//http://stackoverflow.com/questions/4482296/translate-sid-to-name
(*
function GetNameFromSid(ASID: String): String;
var
  lSidLen: DWORD;
  lSid: PSID;
  lName, lDomain: WideString;
begin
  lSidLen := SECURITY_MAX_SID_SIZE;

  lSid := AllocMem(lSidLen);
  try
    StringToSID(ASID, lSid, lSidLen);
    LookupAccountBySid(lSid, lName, lDomain);
    Result := lName;
  finally
    FreeMem(lSid);
  end;
end;
*)
function GetNameFromSid(ASID: string): string;
var
  Sid: PSID;
  peUse: DWORD;
  cchDomain: DWORD;
  cchName: DWORD;
  //Name: array of Char;
  //Domain: array of Char;
  lName, lDomain: string;
begin
  Result := '';
  Sid := nil;
  // First convert String SID to SID
  Win32Check(ConvertStringSidToSid(PChar(ASID), Sid));

  cchName := 0;
  cchDomain := 0;
  // Get Length
  if (not LookupAccountSid(nil, Sid, nil, cchName, nil, cchDomain, peUse)) and
    (GetLastError = ERROR_INSUFFICIENT_BUFFER) then
  begin
    SetLength(lName, cchName);
    SetLength(lDomain, cchDomain);
    if LookupAccountSid(nil, Sid, PChar(lName), cchName, PChar(lName),
      cchDomain, peUse) then
    begin
      // note: cast to PChar because LookupAccountSid returns zero terminated string
      //ShowMessageFmt('%s\%s', [PChar(Domain), PChar(Name)]);
      Result := lDomain + '\' + lName;
    end;
  end;

  if Assigned(Sid) then
    LocalFree(DWORD(Sid));
end;

//http://www.delphi-forum.de/topic_Name+der+Benutzergruppe+Administratoren+ermiltteln_56660,0.html
function StrSIDToName(const StrSID: string): string;
var
  SID: PSID;
  Buffer: PChar;
  NameLen, TempLen: cardinal;
  err: boolean;
  peUse: DWORD = 0;
  Name: string;
begin
  Result := '';
  if StrSID <> '' then
  begin
    err := ConvertStringSIDToSID(PChar(StrSID), SID);
    if err then
    begin
      NameLen := 0;
      TempLen := 0;
      LookupAccountSidA(nil, SID, nil, NameLen, nil, TempLen, peUse);
      GetMem(Buffer, NameLen);
      try
        err := LookupAccountSidA(nil, SID, Buffer, NameLen, nil, TempLen, peUse);
        if err then
        begin
          SetString(Name, Buffer, Namelen);
          Result := Name;
        end;
      finally
        FreeMem(Buffer);
      end;
    end;
    if Assigned(SID) then
      LocalFree(DWORD(SID));
    //result := err;
  end;
end;

//http://stackoverflow.com/questions/2444541/create-windows-user-using-delphi
//http://www.delphipraxis.net/9931-netgroupadduser-2.html
function CreateWinUser(const wServer, wUsername, wPassword, wGroup: WideString): boolean;
var
  Buf: USER_INFO_2;//Buf for the new user info
  Err: NET_API_STATUS;
  ParmErr: DWORD;
  GrpUsrInfo: USER_INFO_0;//Buf for the group
  lgmiWork: LOCALGROUP_MEMBERS_INFO_3;
  wDummyStr: WideString;
begin
  wDummyStr := '';
  FillChar(Buf, SizeOf(USER_INFO_2), 0);
  with Buf do
  begin
    usri2_name := pwidechar(wUsername);
    usri2_full_name := pwidechar(wUsername);//You can add a more descriptive name here
    usri2_password := pwidechar(wPassword);
    usri2_comment := pwidechar(wDummyStr);
    usri2_priv := USER_PRIV_USER;
    usri2_flags := UF_SCRIPT or UF_DONT_EXPIRE_PASSWD;
    usri2_script_path := pwidechar(wDummyStr);
    usri2_home_dir := pwidechar(wDummyStr);
    usri2_acct_expires := TIMEQ_FOREVER;
  end;

  GrpUsrInfo.usri0_name := pwidechar(wGroup);

  Err := NetUserAdd(pwidechar(wServer), 1, @Buf, @ParmErr);
  Result := (Err = NERR_SUCCESS);

  if Result then //NOw you must set the group for the new user
  begin
    //Err := NetUserSetGroups(PWideChar(wServer),PWideChar(wGroup),0,@GrpUsrInfo,1);
    lgmiWork.lgrmi3_domainandname := pwidechar(wUsername);
    Err := NetLocalGroupAddMembers(nil, pwidechar(wGroup), 3, @lgmiWork, 1);
    Result := (Err = NERR_SUCCESS);
  end;
end;

{$IFDEF WIN32}

function DeleteTemporaryLocalAdmin: boolean;
var
  wServer, wUser: WideString;
  Err: NET_API_STATUS;
  strsid: string;
  sid: PSID;
  profiledeleted: boolean = False;
  wnetresult: dword;
begin
  Result := True;
  wServer := DSiGetComputerName;
  wUser := 'opsiSetupAdmin';

  //if not (opsiSetupAdmin_processInfoShell_hProcess = 0) then
  begin
    // now stop the shell
    //if runas in [traAdminProfileImpersonate,traAdminProfileImpersonateExplorer] then
(*
       wnetresult :=  unmountSmbShare('p:', true);
                if NO_ERROR <>  wnetresult then
                  Logdatei.Log('Error unmounting p: '+ SysErrorMessage(wnetresult), LLWarning);
*)
    Logdatei.Log('Try ro remove temporary admin ....', LLDebug2);
    if opsiSetupAdmin_processInfoShell_hProcess <> 0 then
      if not TerminateProcess(opsiSetupAdmin_processInfoShell_hProcess, 0) then
        Logdatei.Log('Could not stop started explorer : ' +
          IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) +
          ')', LLWarning)
      else
        Logdatei.Log(
          'Stopped explorer shell for temporary Admin opsiSetupAdmin',
          LLDebug2);
    if RevertToSelf then
    begin
      Logdatei.Log('Reverted to self.', LLDebug2);
    end
    else
      Logdatei.Log('Revert to self failed', LLDebug2);
(*
    wnetresult :=  mountSmbShare('p:','\\sepiolina\opsi_depot','pcpatch','linux123',false);
      if NO_ERROR <>  wnetresult then
       Logdatei.Log('Error remounting p: '+ SysErrorMessage(wnetresult), LLWarning);
*)
    retrieveFoldersFromWinApi;
    Logdatei.Log('current appdata is now: ' + GetAppDataPath, LLDebug2);
    //Impersonate2loggedonUser;
    //retrieveFoldersFromWinApi;
    //Logdatei.Log('current appdata is now: ' + GetAppDataPath, LLDebug2);
    CloseHandle(opsiSetupAdmin_processInfoShell_hProcess);
    CloseHandle(opsiSetupAdmin_processInfoShell_hThread);
    opsiSetupAdmin_processInfoShell_hProcess := 0;
    opsiSetupAdmin_processInfoShell_hThread := 0;
    if not (opsiSetupAdmin_lpEnvironment = nil) then
      if not DestroyEnvironmentBlock(opsiSetupAdmin_lpEnvironment) then
        Logdatei.Log('Could not destroy Environment : ' +
          IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) +
          ')', LLWarning);

    if not UnloadUserProfile(opsiSetupAdmin_org_logonHandle,
      opsiSetupAdmin_ProfileHandle) then
      Logdatei.Log('Could not unload userprofile : ' + IntToStr(
        GetLastError) + ' (' + SysErrorMessage(GetLastError) + ')', LLNotice);
    CloseHandle(opsiSetupAdmin_logonHandle);
  end;

  strsid := GetLocalUserSidStr('opsiSetupAdmin');
  strsid := copy(strsid, 2, length(strsid) - 2);
  sid := nil;
  Win32Check(ConvertStringSidToSid(PChar(strsid), Sid));
  if DeleteProfile(PChar(strsid), nil, nil) then
    //if delUserProfile(strsid) then
  begin
    Logdatei.Log('Deleted profile of temporary Admin opsiSetupAdmin ',
      LLDebug);
    profiledeleted := True;
  end
  else
    Logdatei.Log('Could not delete profile for sid : ' +
      strsid + ' - ' + IntToStr(GetLastError) + ' (' +
      SysErrorMessage(GetLastError) + ')', LLNotice);

  Err := NetUserDel(PWideChar(wServer), PWideChar(wUser));
  Result := (Err = NERR_SUCCESS);
  if Result then
  begin
    Logdatei.Log('Deleted temporary Admin opsiSetupAdmin with sid: ' +
      strsid, LLDebug);
    opsiSetupAdmin_created := False;
    if not profiledeleted then
    begin
      //if delUserProfile(strsid) then
      if DeleteProfile(PChar(strsid), nil, nil) then
      begin
        Logdatei.Log('Deleted profile of temporary Admin opsiSetupAdmin ',
          LLDebug);
      end
      else
      begin
        Logdatei.Log('Could not delete profile for sid : ' +
          strsid + ' - ' + IntToStr(GetLastError) + ' (' +
          SysErrorMessage(GetLastError) + ')', LLNotice);
        if delUserProfile(strsid) then
        ;
      end;
    end;
    retrieveFoldersFromWinApi;
    //Logdatei.Log('current appdata is now: '+GetAppDataPath, LLDebug);
  end;
  Logdatei.Log('current appdata is now: ' + GetAppDataPath, LLDebug2);
end;

{$ENDIF WIN32}

function OpenShellProcessInSessionToken(ProcessName: string;
  Sessionid: DWORD; Username: ansistring; horgToken: THandle;
  var hToken: THandle): boolean;
type
  TSessionData = record
    UserLuid: TLUID;
    UserName,
    Domain: string;
    LogonType: TSecurityLogonType;
    LogonTime: TDateTime;
  end;
var
  hSnapshot, hProcess, hdupToken: THandle;
  Process: TProcessEntry32;
  FoundSessionid: DWORD = 0;
  FoundUserName, FoundDomain: ansistring;
var
  Count: ULONG;
  Luid: PLUID;
  myLuid: TLuid;
  dupUser, dupDomain: ansistring;
  Return: DWORD;
  PSesDat: PSecurityLogonSessionData;
  i: integer;
  sid: cardinal;
  SizeNeeded, SizeNeeded2: DWORD;
  OwnerName, DomainName: PChar;
  OwnerType: SID_NAME_USE;
  pBuffer: Pointer;
  pBytesreturned: DWord;
  LocalFileTime: TFileTime;
  Secur32Handle, Wtsapi32Handle: THandle;
  SessionData: TSessionData;
  NewState, PrevState: TTokenPrivileges;
  //FWTSQuerySessionInformation : TWTSQuerySessionInformation; // ab Win XP
  //FWTSGetActiveConsoleSessionId : TWTSGetActiveConsoleSessionId; // ab Win XP
  //FLsaEnumerateLogonSessions : TLsaEnumerateLogonSessions; // ab Win XP
  //FLsaGetLogonSessionData : TLsaGetLogonSessionData; // ab Win 2000
  //FLsaFreeReturnBuffer : TLsaFreeReturnBuffer; // ab Win 2000
begin
  try
    if (LsaNtStatusToWinError(LsaEnumerateLogonSessions(@Count, Luid)) = 0) then
    begin
      i := -1;
      if Count > 0 then
        repeat
          Inc(i);
          // Prüfe auf mögliche Fehler (z.B. Access denied)
          if LsaNtStatusToWinError(LsaGetLogonSessionData(Luid, PSesDat)) = 0 then
          begin
            // Prüfe, ob es sich um eine Konsolen- oder Remote-Anmeldung handelt
            if (PSesDat^.LogonType = Interactive) or
              (PSesDat^.LogonType = RemoteInteractive) then
            begin
              SizeNeeded := MAX_PATH;
              SizeNeeded2 := MAX_PATH;
              GetMem(OwnerName, MAX_PATH);
              GetMem(DomainName, MAX_PATH);
              try
                if LookupAccountSID(nil, PSesDat^.SID, OwnerName,
                  SizeNeeded, DomainName, SizeNeeded2, OwnerType) then
                begin
                  // Prüfen ob es sich um einen Benutzer handelt und ob es die
                  // SessionId des aufrufenden Prozesses ist
                  Logdatei.Log('Found winlogon for owner:' +
                    OwnerName, LLDebug2);
                  if (OwnerType = 1) and
                    ProcessIdToSessionId(GetCurrentProcessId, sid) and
                    (PSesDat^.Session = sid) then
                  begin
                    Logdatei.Log('Found winlogon for owner:' +
                      OwnerName + ' which is user and in session: ' +
                      IntToStr(sid), LLDebug2);
                    if AnsiLowerCase(OwnerName) = AnsiLowerCase(Username) then
                    begin
                      Sessionid := sid;
                      Logdatei.Log('owner:' + OwnerName +
                        ' matches searched user name', LLDebug2);
                    end;
(*
                  if WTSQuerySessionInformation(WTS_CURRENT_SERVER_HANDLE,
                      PSesDat^.Session, WTSConnectState,pBuffer,pBytesreturned) then
                  begin
                    if WTS_CONNECTSTATE_CLASS(pBuffer^) = WTSActive then
                    begin
                      with SessionData do
                      begin
                        UserLuid:=Luid^;
                        UserName:=PSesDat^.UserName.Buffer;
                        DOmain:=PSesDat^.LogonDomain.Buffer;
                        LogonType:=PSesDat^.LogonType;
                        LogonTime:=Now;
                        //if FileTimeToLocalFileTime(TFileTime(PSesDat^.LogonTime),LocalFileTime) then
                        //  LogonTime:=FileTimeToDateTime(LocalFileTime);
                        //  result:=true;
                        //end;
                        Logdatei.Log('Found winlogon for owner:'+UserName+' logon type: ', LLDebug2);
                      end;
                    end;
                    LSAFreeReturnBuffer(pBuffer);
                  end;
*)
                  end;
                end;
              finally
                FreeMem(OwnerName);
                FreeMem(DomainName);
              end;
            end;
          end;
          Inc(Luid);
          try
            LSAFreeReturnBuffer(PSesDat);
          except
          end;
        until (i = Count - 1) or Result;
    end;
  finally
    LSAFreeReturnBuffer(Luid);
    //FreeLibrary(Wtsapi32Handle); FreeLibrary(Secur32Handle);
  end;
 (*
  if GetSessionByToken(horgToken,FoundSessionid) then
    Logdatei.Log('Org token in session: '+intToStr(FoundSessionid), LLDebug2);
  if not SetTokenInformation(horgToken, TokenSessionId, @Sessionid, sizeof(Sessionid)) then
    Logdatei.Log('Could not add logon handle to session. Error: '+intToStr(GetLastError), LLError);
  if not WTSQueryUserToken(SessionId,hdupToken) then
    Logdatei.Log('Could not get primary user Token. Error: '+intToStr(GetLastError), LLError)
  else
  begin
    hToken := hdupToken;
    GetUserByToken(hdupToken, dupUser,dupDomain);
    Logdatei.Log('Got Token from process:'+ProcessName+' in session: '+inttostr(Sessionid)+' for user: '+dupUser, LLDebug2);
  end;
 *)

  ////////////////////////////////////
  Result := False;
  hSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if (hSnapshot <> 0) and (hSnapshot <> INVALID_HANDLE_VALUE) then
    try
      FillChar(Process, SizeOf(Process), #0);
      Process.dwSize := SizeOf(Process);
      if Process32First(hSnapshot, Process) then
        repeat
          //Logdatei.Log('Found process:'+Process.szExeFile, LLDebug2);
          //if not GetProcessUserBypid(Process.th32ProcessID, FoundUserName, FoundDomain) then
          //Logdatei.Log('Did not found user for process:'+Process.szExeFile, LLDebug2)
          //else
          begin
            //Logdatei.Log('Found process:'+Process.szExeFile+' for user: '+FoundUserName, LLDebug2);
            if not ProcessIdToSessionId(Process.th32ProcessID, FoundSessionid) then
            //Logdatei.Log('Could not get Sessionid.', LLDebug2)
            else
            if (AnsiLowerCase(Process.szExeFile) = AnsiLowerCase(ProcessName)) and
              (FoundSessionid = Sessionid)
            //and (AnsiLowerCase(FoundUserName) = AnsiLowerCase(UserName))
            then
            begin
              Logdatei.Log('Found process:' + ProcessName +
                ' in session: ' + IntToStr(Sessionid), LLDebug2);
              hProcess :=
                //OpenProcess(PROCESS_ALL_ACCESS, false, Process.th32ProcessID);
                OpenProcess(MAXIMUM_ALLOWED, False, Process.th32ProcessID);
              if (hProcess <> 0) and (hProcess <> INVALID_HANDLE_VALUE) then
                try
                  Logdatei.Log('Try to get token ....', LLDebug2);
                  //Result := OpenProcessToken(hProcess, TOKEN_ALL_ACCESS, hToken);
                  Result := OpenProcessToken(hProcess,
                    TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY or
                    TOKEN_DUPLICATE or TOKEN_ASSIGN_PRIMARY or
                    TOKEN_READ or TOKEN_WRITE, hToken);
                  Result := jwawinbase.DuplicateTokenEx(hToken,
                    MAXIMUM_ALLOWED, nil, jwawinnt.SecurityIdentification,
                    jwawinnt.TokenPrimary, hdupToken);
                  Result := SetTokenInformation(hdupToken, TokenSessionId,
                    @Sessionid, sizeof(Sessionid));
                  if not LookupPrivilegeValue(nil, SE_DEBUG_NAME, myLuid) then
                    Logdatei.Log('Could not LookupPrivilegeValue.', LLError)
                  else
                  begin
                    NewState.PrivilegeCount := 1;
                    NewState.Privileges[0].Luid := myLuid;
                    NewState.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
                    if not AdjustTokenPrivileges(hdupToken, False,
                      @NewState, SizeOf(TTokenPrivileges), @PrevState, @Return) then
                      Logdatei.Log('Could not AdjustTokenPrivileges.', LLError);
                    //else
                    begin
                      hToken := hdupToken;
                      GetUserByToken(hdupToken, dupUser, dupDomain);
                      Logdatei.Log('Got Token from process:' +
                        ProcessName + ' in session: ' + IntToStr(Sessionid) +
                        ' for user: ' + dupUser, LLDebug2);
                    end;
                  end;
                finally
                  CloseHandle(hProcess);
                end;
              Break;
            end;
          end;
        until (not Process32Next(hSnapshot, Process));
    finally
      CloseHandle(hSnapshot);
    end;
end;


//http://www.delphipraxis.net/164555-dienst-windows-xp-windows-server-2003-r2.html


function OpenShellProcessToken(ProcessName: string; var hToken: THandle): boolean;
var
  hSnapshot, hProcess, hdupToken: THandle;
  Process: TProcessEntry32;
  (*
  SessionID : Dword;
  myLuid: TLuid;
  dupUser,dupDomain : ansistring;
  Return : DWORD;
  NewState, PrevState: TTokenPrivileges;
  *)
begin
  //SessionID := WTSGetActiveConsoleSessionId;
  Result := False;
  hSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if (hSnapshot <> 0) and (hSnapshot <> INVALID_HANDLE_VALUE) then
    try
      FillChar(Process, SizeOf(Process), #0);
      Process.dwSize := SizeOf(Process);
      if Process32First(hSnapshot, Process) then
        repeat
          if (AnsiLowerCase(Process.szExeFile) = AnsiLowerCase(ProcessName)) then
          begin
            hProcess :=
              OpenProcess(PROCESS_ALL_ACCESS, False, Process.th32ProcessID);
            if (hProcess <> 0) and (hProcess <> INVALID_HANDLE_VALUE) then
        (*
                  try
            Logdatei.Log('Try to get token ....', LLDebug2);
            //Result := OpenProcessToken(hProcess, TOKEN_ALL_ACCESS, hToken);
            Result := OpenProcessToken(hProcess, TOKEN_ADJUST_PRIVILEGES
                                              or TOKEN_QUERY
                                              or TOKEN_DUPLICATE
                                              or TOKEN_ASSIGN_PRIMARY
                                              or TOKEN_READ
                                              or TOKEN_WRITE, hToken);
            Result := jwawinbase.DuplicateTokenEx(hToken, MAXIMUM_ALLOWED, nil,
                        jwawinnt.SecurityIdentification,
                        jwawinnt.TokenPrimary, hdupToken);
            Result := SetTokenInformation(hdupToken, TokenSessionId, @Sessionid, sizeof(Sessionid));
            if not LookupPrivilegeValue(nil, SE_DEBUG_NAME, myLuid) then
              Logdatei.Log('Could not LookupPrivilegeValue.', LLError)
            else
            begin
              NewState.PrivilegeCount := 1;
              NewState.Privileges[0].Luid := myLuid;
              NewState.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
              if not AdjustTokenPrivileges(hdupToken, False, @NewState,
                SizeOf(TTokenPrivileges), @PrevState, @Return) then
                Logdatei.Log('Could not AdjustTokenPrivileges.', LLError)
              else
              begin
                hToken := hdupToken;
                GetUserByToken(hdupToken, dupUser,dupDomain);
                Logdatei.Log('Got Token from process:'+ProcessName+' in session: '+inttostr(Sessionid)+' for user: '+dupUser, LLDebug2);
              end;
            end;
          finally
            CloseHandle(hProcess);
          end;
*)

              try
                Result := OpenProcessToken(hProcess, TOKEN_ALL_ACCESS, hToken);
                //GetUserByToken(hToken, dupUser,dupDomain);
                //Logdatei.Log('Got Token from process:'+ProcessName+' in session: '+inttostr(Sessionid)+' for user: '+dupUser, LLDebug2);
              finally
                CloseHandle(hProcess);
              end;

            Break;
          end;
        until (not Process32Next(hSnapshot, Process));
    finally
      CloseHandle(hSnapshot);
    end;
end;



function CreateProcessElevated(lpApplicationName: PChar; lpCommandLine: string;
  lpCurrentDirectory: PChar; Counter: integer;
  var ProcessInfo: jwawinbase.TProcessInformation): boolean;
var
  WTSGetActiveConsoleSessionId: function: DWord; stdcall;
  WTSQueryUserToken: function(SessionId: ULONG; var phToken: THandle): BOOL; stdcall;
  CreateEnvironmentBlock: function(lpEnvironment: PPointer; hToken: THandle;
    bInherit: BOOL): BOOL; stdcall;
  DestroyEnvironmentBlock: function(lpEnvironment: PPointer): BOOL; stdcall;
var
  hUserToken, hLinkedToken, hElevatedToken: THandle;
  ReturnLength, ElevationType: DWord;
  Environment: Pointer;
  StartupInfo: jwawinbase.TStartupInfo;
begin
  Logdatei.Log('CreateProcessElevated 1', LLDebug2);
  Result := False;
  @CreateEnvironmentBlock := GetProcAddress(LoadLibrary('userenv.dll'),
    'CreateEnvironmentBlock');
  @DestroyEnvironmentBlock := GetProcAddress(LoadLibrary('userenv.dll'),
    'DestroyEnvironmentBlock');
  if (not Assigned(CreateEnvironmentBlock)) or
    (not Assigned(DestroyEnvironmentBlock)) then
    Exit;
  //@WTSGetActiveConsoleSessionId := GetProcAddress(LoadLibrary('kernel32.dll'), 'WTSGetActiveConsoleSessionId');
  //@WTSQueryUserToken := GetProcAddress(LoadLibrary('wtsapi32.dll'), 'WTSQueryUserToken');
  begin
    Logdatei.Log('CreateProcessElevated 2', LLDebug2);
    Result := OpenShellProcessToken('opsiclientd.exe', hUserToken);
    if not Result then
      Result := OpenShellProcessToken('opsiclientd.exe', hUserToken);
    if not Result then
      Result := OpenShellProcessToken('explorer.exe', hUserToken);
    //Result := OpenShellProcessToken('explorer.exe', hUserToken);
    //Result := OpenShellProcessToken('winlogon.exe', hUserToken);
    if Result then
      try
        begin
          begin
            hElevatedToken := hUserToken;
          end;
          try
            Logdatei.Log('CreateProcessElevated 3', LLDebug2);
            if CreateEnvironmentBlock(@Environment, hElevatedToken, False) then
              try
                FillChar(StartupInfo, SizeOf(StartupInfo), #0);
                StartupInfo.cb := SizeOf(StartupInfo);
                uniqueString(lpcommandline);
                Result := jwawinbase.CreateProcessAsUser(hElevatedToken,
                  lpApplicationName, PChar(lpCommandLine), nil, nil,
                  False, CREATE_NEW_CONSOLE or CREATE_DEFAULT_ERROR_MODE or
                  CREATE_UNICODE_ENVIRONMENT, Environment, lpCurrentDirectory,
                  StartupInfo, ProcessInfo);
                Logdatei.Log('CreateProcessElevated 4', LLDebug);
                //Kernzuweisung
                SetProcessAffinityMask(ProcessInfo.hProcess, counter + 1);
              finally
                Logdatei.Log('CreateProcessElevated 5', LLDebug2);
                DestroyEnvironmentBlock(Environment);
              end;
          finally
            Logdatei.Log('CreateProcessElevated 6', LLDebug2);
            //CloseHandle(hElevatedToken);
          end;
        end;
      finally
        Logdatei.Log('CreateProcessElevated 7', LLDebug2);
        //CloseHandle(hUserToken);
      end;
  end;
end;

function winBlockInput(block: boolean): boolean;
var
  BlockInput: function(fBlockIt: Windows.BOOL): Windows.BOOL; stdcall;
  res: Windows.BOOL;
begin
  @BlockInput := GetProcAddress(LoadLibrary('user32.dll'), 'BlockInput');
  if block then
    Res := BlockInput(longbool(1))
  else
    Res := BlockInput(longbool(0));
  if res = longbool(0) then
    Result := False
  else
    Result := True;
end;

(*
function GetAdaptersInfo(const pAdapterInfo: PIP_ADAPTER_INFO;
const pOutBufLen: PULONG): DWORD; stdcall;
external 'IPHLPAPI.DLL' Name 'GetAdaptersInfo';

function GetBestInterface(const dwDestAddr: cardinal;
var pdwBestIfIndex: DWORD): DWORD; stdcall;
external 'IPHLPAPI.DLL' Name 'GetBestInterface';
*)

function getMyIpByTarget(const myserver: ansistring): string;
(*
const
  MAX_ADAPTER_DESCRIPTION_LENGTH = 128; // arb.
  MAX_ADAPTER_NAME_LENGTH = 256; // arb.
  MAX_ADAPTER_ADDRESS_LENGTH = 8;  // arb.


type

  PIP_ADDRESS_STRING = ^IP_ADDRESS_STRING;

  IP_ADDRESS_STRING =
    packed record
    acString: array [1..16] of char;
  end;

  PIP_MASK_STRING = ^IP_MASK_STRING;
  IP_MASK_STRING = IP_ADDRESS_STRING;

  PIP_ADDR_STRING = ^IP_ADDR_STRING;

  IP_ADDR_STRING =
    packed record
    Next: PIP_ADDR_STRING;
    IpAddress: IP_ADDRESS_STRING;
    IpMask: IP_MASK_STRING;
    Context: DWORD;
  end;

  time_t = int64;

  PIP_ADAPTER_INFO = ^IP_ADAPTER_INFO;

  IP_ADAPTER_INFO =
    packed record
    Next: PIP_ADAPTER_INFO;
    ComboIndex: DWORD;
    AdapterName: array [1..MAX_ADAPTER_NAME_LENGTH + 4] of char;
    Description: array [1..MAX_ADAPTER_DESCRIPTION_LENGTH + 4] of char;
    AddressLength: UINT;
    Address: array [1..MAX_ADAPTER_ADDRESS_LENGTH] of byte;
    Index: DWORD;
    dwType: UINT;
    DhcpEnabled: UINT;
    CurrentIpAddress: PIP_ADDR_STRING;
    IpAddressList: IP_ADDR_STRING;
    GatewayList: IP_ADDR_STRING;
    DhcpServer: IP_ADDR_STRING;
    HaveWins: boolean;
    PrimaryWinsServer: IP_ADDR_STRING;
    SecondaryWinsServer: IP_ADDR_STRING;
    LeaseObtained: time_t;
    LeaseExpires: time_t;
  end;
 *)

var
  ErrorMsg: string;
  dwResult: DWORD;
  dwLen: DWORD;
  pAdapterWork: PIP_ADAPTER_INFO;
  pAdapterList: PIP_ADAPTER_INFO;
  iasWork: IP_ADDR_STRING;
  adapterindex: DWORD;
  optionlist: TStringList;

begin

  adapterindex := 0;
  GetBestinterface(inet_addr(pansichar(myserver)), adapterindex);
  pAdapterList := nil;
  dwLen := 0;
  dwResult := GetAdaptersInfo(pAdapterList, dwLen);
  if dwResult = ERROR_BUFFER_OVERFLOW then
  begin
    pAdapterList := AllocMem(dwLen);
    try
      dwResult := GetAdaptersInfo(pAdapterList, dwLen);
      if dwResult = ERROR_SUCCESS then
      begin
        pAdapterWork := pAdapterList;
        repeat
          if pAdapterWork.Index = adapterindex then
          begin
            //writeln('found:'+trim(pAdapterWork.IpAddressList.IpAddress.acString));
            Result := trim(pAdapterWork.IpAddressList.IpAddress.S);
          end;
          pAdapterWork := pAdapterWork.Next;
        until pAdapterWork = nil;
      end;
    finally
      FreeMem(pAdapterList, dwLen);
    end;
  end;
end;



function getIpMacHash: TStringList;
  //does not work (do 15.6.2012)
  //code derivated from:
  //http://theroadtodelphi.wordpress.com/2011/04/21/accesing-the-wmi-from-delphi-and-fpc-via-com-without-late-binding-or-wbemscripting_tlb/

const
  RPC_C_AUTHN_LEVEL_DEFAULT = 0;
  RPC_C_IMP_LEVEL_IMPERSONATE = 3;
  RPC_C_AUTHN_WINNT = 10;
  RPC_C_AUTHZ_NONE = 0;
  RPC_C_AUTHN_LEVEL_CALL = 3;
  EOAC_NONE = 0;
  strLocale = '';
  strUser = '';
  strPassword = '';
  strNetworkResource = 'root\cimv2';
  strAuthority = '';
  WQL =
    'Select Description, IPAddress, MacAddress from Win32_NetworkAdapterConfiguration';
var
  FWbemLocator: IWbemLocator;
  FWbemServices: IWbemServices;
  FUnsecuredApartment: IUnsecuredApartment;
  ppEnum: IEnumWbemClassObject;
  apObjects: IWbemClassObject;
  puReturned: ULONG;
  pDesc, pIP, pMac, pVal: olevariant;
  pType: integer;
  plFlavor: integer;
  Succeed: HRESULT;
  I: integer;

begin
  Result := TStringList.Create;
  Logdatei.Log('getIpMacHash start', LLDebug2);
  try
    // Initialize COM. ------------------------------------------
    if Succeeded(CoInitializeEx(nil, COINIT_MULTITHREADED)) then
      try
        Logdatei.Log('getIpMacHash CoInitializeEx', LLDebug2);
        // Set general COM security levels --------------------------
        // Note: If you are using Windows 2000, you need to specify -
        // the default authentication credentials for a user by using
        // a SOLE_AUTHENTICATION_LIST structure in the pAuthList ----
        // parameter of CoInitializeSecurity ------------------------
        if Failed(CoInitializeSecurity(nil, -1, nil, nil,
          RPC_C_AUTHN_LEVEL_DEFAULT, RPC_C_IMP_LEVEL_IMPERSONATE, nil,
          EOAC_NONE, nil)) then
          Logdatei.Log('Error: getIpMacHash: Failed:  CoInitializeSecurity',
            LLError);
        // Obtain the initial locator to WMI -------------------------
        if Succeeded(CoCreateInstance(CLSID_WbemLocator, nil,
          CLSCTX_INPROC_SERVER, IID_IWbemLocator, FWbemLocator)) then
          try
            // Connect to WMI through the IWbemLocator::ConnectServer method
            if Succeeded(FWbemLocator.ConnectServer(strNetworkResource,
              strUser, strPassword, strLocale, WBEM_FLAG_CONNECT_USE_MAX_WAIT,
              strAuthority, nil, FWbemServices)) then
              try
                // Set security levels on the proxy -------------------------
                if Failed(CoSetProxyBlanket(FWbemServices, RPC_C_AUTHN_WINNT,
                  RPC_C_AUTHZ_NONE, nil, RPC_C_AUTHN_LEVEL_CALL,
                  RPC_C_IMP_LEVEL_IMPERSONATE, nil, EOAC_NONE)) then
                  Exit;
                if Succeeded(CoCreateInstance(CLSID_UnsecuredApartment,
                  nil, CLSCTX_LOCAL_SERVER, IID_IUnsecuredApartment,
                  FUnsecuredApartment)) then
                  try
                    // Use the IWbemServices pointer to make requests of WMI
                    //Succeed := FWbemServices.ExecQuery('WQL', WQL, WBEM_FLAG_FORWARD_ONLY OR WBEM_FLAG_RETURN_IMMEDIATELY, nil, ppEnum);
                    Succeed :=
                      FWbemServices.ExecQuery('WQL', WQL, WBEM_FLAG_FORWARD_ONLY,
                      nil, ppEnum);
                    if Succeeded(Succeed) then
                    begin
                      // Get the data from the query
                      //while (ppEnum.Next(WBEM_INFINITE, 1, apObjects, puReturned)=0) do
                      while (ppEnum.Next(MAXLONGINT, 1, apObjects, puReturned) = 0) do
                      begin
                        apObjects.Get('description', 0, pDesc, pType, plFlavor);
                        apObjects.Get('IPAddress', 0, pIP, pType, plFlavor);
                        apObjects.Get('MacAddress', 0, pMac, pType, plFlavor);

                        if VarIsArray(pIp) then
                        begin
                          I := VarArrayLowBound(pIp, 1);
                          if VarIsStr(VarArrayGet(pIp, [I])) then
                          begin
                            Result.add(VarToStr(VarArrayGet(pIp, [I])) +
                              '=' + VarToStr(pMac));
                            Logdatei.Log(VarToStr(VarArrayGet(pIp, [I])) +
                              '=' + VarToStr(pMac), LLDebug2);
                            //writeln(VarToStr(VarArrayGet(pIp,[I]))+' '+VarToStr(pMac));
                    (*
                    writeln('IPAddress: '+ VarToStr(VarArrayGet(pIp,[I])));
                    writeln('MacAddress: '+  VarToStr(pMac));
                    writeln('description: '+  VarToStr(pDesc));
                    *)
                          end;
                        end;
                        VarClear(pDesc);
                        VarClear(pIp);
                        VarClear(pMac);
                      end;
                    end
                    else
                      //Writeln(Format('Error executing WQL sentence %x',[Succeed]));
                      Logdatei.Log('Error executing WQL sentence ' +
                        IntToStr(Succeed), LLError);
                  finally
                    FUnsecuredApartment := nil;
                  end;
              finally
                FWbemServices := nil;
              end;
          finally
            FWbemLocator := nil;
          end;
      finally
        CoUninitialize();
      end;
  except
    on E: Exception do
      Logdatei.Log('Error in wiwin32.getIpMacHash: ' + E.Message, LLError);
    //writeln('Error in ip2mac: '+E.Message);
  end;
end;

function mountSmbShare(drive: string; uncPath: string; Username: string;
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

function unmountSmbShare(driveOrPath: string; force: boolean): DWORD;
begin
  Result := WNetCancelConnection2(PChar(driveOrPath), 0, force);
end;


function getProfilesListWin(ProfilesType:TProfilesType):TStringList;

var
  profileList: TStringList;
  noredirection: boolean;
  i: integer;
  myregkey: string;
  mypath: string;
begin
  Result := TStringList.Create;
  profileList := TStringList.Create;
  myregkey := 'HKLM\SOFTWARE\Microsoft\Windows NT\CurrentVersion\ProfileList';
  if Is64BitSystem then
    noredirection := True
  else
    noredirection := False;
  profileList.AddStrings(GetRegistryKeyList(myregkey, noredirection));
  for i := 0 to profileList.Count - 1 do
  begin
    mypath := getProfileImagePathfromSid(profileList.Strings[i]);
    // let us exclude profiles that normaly not should be patched
    if (0 = pos('localservice', LowerCase(mypath))) and
      (0 = pos('networkservice', LowerCase(mypath))) and
      (0 = pos('systemprofile', LowerCase(mypath))) and
      // avoid empty path
      (mypath <> '') then
        case ProfilesType of
          tDirectory: Result.Add(mypath);
          tSID: Result.Add(profileList.Strings[i]);
        end;
  end;
  case ProfilesType of
    tDirectory: Result.Add(GetDefaultUsersProfilesPath);
    tSID: Result.Add('defaultprofile');
  end;
  profileList.Free;
end;


function getProfilesDirListWin: TStringList;
begin
  Result := getProfilesListWin(tDirectory);
end;

(*
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
    Dispose(HName);
    WSACleanup;
  except
    WSAErr := 'Error resolving Host';
  end;
end;
*)

{$IFDEF WIN32}
function getWinlogonHandleForUser(const myuser: string; var myhandle: THandle): boolean;
var
  myorgtoken: cardinal = 0;
  myduptoken: cardinal = 0;
begin
  Result := False;
  if OpenShellProcessInSessionToken('explorer.exe', WTSGetActiveConsoleSessionId,
    myuser, myorgtoken, myduptoken) then
  begin
    myhandle := myduptoken;
    Result := True;
  end
  else
    Logdatei.Log('Could not find winlogon token for user: ' + myuser, LLError);
  myhandle := myduptoken;
end;

function GetProcessActivityByPid(pid: DWORD): real;
var
  cnt: PCPUUsageData;
  usage: real;
begin
  Result := 0;
  try
    if not DSiEnablePrivilege('SE_DEBUG_NAME') then
      Logdatei.Log(
        'Warning: Could not get SE_DEBUG_NAME Privilege  -> will try with out',
        LLWarning);
    //Initialize the counter
    cnt := wsCreateUsageCounter(pid);
    if cnt <> nil then
    begin
      //Allow for some time to elapse
      Sleep(500);
      //Get the CPU usage
      usage := wsGetCpuUsage(cnt);
      //The returned value is a real number between 0 and 100 (representint %).
      //result := round(usage*10);
      Result := usage;
    end;
  finally
    if cnt <> nil then
    begin
      //Destroy the counter and free memory
      wsDestroyUsageCounter(cnt);
    end;
  end;
end;

function GetProcessActivityByName(const exename: string; var found: boolean): real;
var
  pid: dword;
begin
  found := False;
  Result := 0;
  if getpid4exe(exename, pid) then
  begin
    Result := GetProcessActivityByPid(pid);
    found := True;
  end;
end;

{$ENDIF WIN32}


function CreateHardLinkA(lpFileName: PChar; lpExistingFileName: PChar;
  lpSecurityAttributes: pointer): winbool; stdcall;
  external kernel32 Name 'CreateHardLinkA';

function winCreateHardLink(lpFileName: PChar; lpExistingFileName: PChar;
  lpSecurityAttributes: pointer): winbool;
  //var
  //kernel32: HModule;
  //_CreateHardLink: function(lpFileName: pchar; lpExistingFileName :pchar;
  //    lpSecurityAttributes: pointer): LPBOOL; stdcall;
begin
  try
    (*
    if not Assigned(_CreateHardLink) then
    begin
      // Kernel32 is always loaded already, so use GetModuleHandle
      // instead of LoadLibrary
      kernel32 := GetModuleHandle('kernel32');
      if kernel32 = 0 then
        RaiseLastOSError;
      @_CreateHardLink := GetProcAddress(kernel32, 'CreateHardLink');
      if not Assigned(_CreateHardLink) then
        RaiseLastOSError;
    end;
    *)
    Result := CreateHardLinkA(lpFileName, lpExistingFileName, lpSecurityAttributes);
  except
    on ex: Exception do
    begin
      Result := False;
      LogDatei.log('Exception in winCreateHardLink: ' + ex.message, LLError);
    end;
  end;
end;


//function CreateSymbolicLinkA(lpSymlinkFileName: pchar; lpExistingFileName :pchar;
//      dwFlags: DWORD): winbool; stdcall; external kernel32 name 'CreateSymbolicLinkA';

function winCreateSymbolicLink(lpSymlinkFileName: PChar; lpExistingFileName: PChar;
  dwFlags: DWORD): winbool;
var
  ExitCode: longint;
  Report, CmdLinePasStr: string;
  kernel32: HModule;
  _CreateSymbolicLinkA: function(lpSymlinkFileName: PChar;
    lpExistingFileName: PChar; dwFlags: DWORD): winbool; stdcall;
  list1: TStringList;
begin
  try
    // try api
    // get needed privilege
    {$IFDEF WIN32}
    if not DSiEnablePrivilege('SeCreateSymbolicLinkPrivilege') then
    begin
      LogDatei.log('EnablePrivilege Error: ' + IntToStr(GetLastError) +
        ' (' + SysErrorMessage(GetLastError) + ')', LLError);
    end;
    {$ENDIF WIN32}
    //next try
    if not SetProcessPrivilege('SeCreateSymbolicLinkPrivilege') then
    begin
      LogDatei.log('EnablePrivilege Error: ' + IntToStr(GetLastError) +
        ' (' + SysErrorMessage(GetLastError) + ')', LLError);
    end;
    _CreateSymbolicLinkA := nil;
    if not Assigned(_CreateSymbolicLinkA) then
    begin
      // Kernel32 is always loaded already, so use GetModuleHandle
      // instead of LoadLibrary
      kernel32 := GetModuleHandle('kernel32');
      if kernel32 = 0 then
        RaiseLastOSError;
      @_CreateSymbolicLinkA := GetProcAddress(kernel32, 'CreateSymbolicLinkA');
      if not Assigned(_CreateSymbolicLinkA) then
        RaiseLastOSError;
    end;
    // This may fail because UAC will strip the privilege
    Result := _CreateSymbolicLinkA(lpSymlinkFileName, lpExistingFileName, dwFlags);
    if not Result then
    begin
      // plan B: try elevated process
      {$IFDEF WIN32}
      opsiSetupAdmin_runElevated := True;
      {$ENDIF WIN32}
      CmdLinePasStr := 'cmd.exe /c mklink "' + lpSymlinkFileName +
        '" "' + lpExistingFileName + '"';
      LogDatei.log('Missing Priviliges for API Call - start process elevated: ' +
        CmdLinePasStr, LLInfo);
      Result := StartProcess(CmdLinePasStr, SW_HIDE, False, False,
        False, False, True, traInvoker, '', 10, Report, ExitCode);
      {$IFDEF WIN32}
      opsiSetupAdmin_runElevated := False;
      {$ENDIF WIN32}
      (*
      LogDatei.log('Reading symlink via dir to reread the cache', LLInfo);
      Result := StartProcess('cmd.exe /c dir "' + lpSymlinkFileName+'"', SW_HIDE, False, False,
        False, False, True, traInvoker, '', 10, Report, ExitCode);
      *)
    end;
  except
    on ex: Exception do
    begin
      Result := False;
      LogDatei.log('Exception in winCreateSymbolicLink: ' + ex.message, LLError);
    end;
  end;
end;

function updateEnvironment(): boolean;
var
  //mylong: lresult;
  mylong: DWORD_PTR;
  mylparam: lparam;
begin
  if GetNTVersionMajor < 6 then
  begin
    LogDatei.log('No updateEnvironment for Windows < 6', LLDebug);
    Result := False;
  end
  else
    try
      //if SendMessageTimeoutA() ;
      mylparam := lparam(PChar('Environment'));
      if 0 <> SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE,
        0, mylparam, SMTO_ABORTIFHUNG, 3000, mylong) then
      begin
        //mylong := jwawindows.SendMessage(HWND_BROADCAST, WM_SETTINGCHANGE, 0, mylparam);
        //mylong := SendMessage(HWND_BROADCAST, WM_SETTINGCHANGE, 0, mylparam);
        LogDatei.log('Sended Message to Reload Environment  ', LLDebug);
        Result := True;
      end
      else
      begin
        LogDatei.log('Timeout Message to Reload Environment  ', LLError);
        Result := False;
      end;
    except
      on ex: Exception do
      begin
        Result := False;
        LogDatei.log('Exception in updateEnvironment (SendMessage): ' +
          ex.message, LLError);
      end;
    end;
end;

//https://support.microsoft.com/en-us/help/157234/how-to-deal-with-localized-and-renamed-user-and-group-names
//https://social.technet.microsoft.com/wiki/contents/articles/13813.localized-names-for-administrator-account-in-windows.aspx


// https://stackoverflow.com/questions/49252038/how-to-get-the-full-file-name-from-a-textfile-variable

const
  FILE_NAME_NORMALIZED = $00000000;

function GetFinalPathNameByHandleUndefined(hFile: THandle; lpszFilePath: PChar;
  cchFilePath, dwFlags: DWORD): DWORD; stdcall;
begin
  StrPCopy(lpszFilePath, '');
  Result := 0;
end;

function FileHandleToFileName(Handle: THandle): string;
type
  TGetFinalPathNameByHandle = function(hFile: THandle; lpszFilePath: PChar;
      cchFilePath, dwFlags: DWORD): DWORD; stdcall;

const
  GetFinalPathNameByHandle: TGetFinalPathNameByHandle = nil;

var
  Err: cardinal;
  errorstr: string;
  mylength: cardinal;

begin
  Result := '';
  if not Assigned(GetFinalPathNameByHandle) then
  begin
    GetFinalPathNameByHandle :=
      GetProcAddress(GetModuleHandle('kernel32'), 'GetFinalPathNameByHandleA');
    if not Assigned(GetFinalPathNameByHandle) then
      GetFinalPathNameByHandle := GetFinalPathNameByHandleUndefined;
  end;
  SetLength(Result, MAX_PATH + 1);
  mylength := GetFinalPathNameByHandle(Handle, @Result[1], LENGTH(
    Result), FILE_NAME_NORMALIZED);
  if mylength > 0 then
  begin
    SetLength(Result, mylength);
    if COPY(Result, 1, 4) = '\\?\' then
      system.Delete(Result, 1, 4);
  end
  else
  begin
    errorstr := 'GetFinalPathNameByHandle : ' + IntToStr(GetLastError) +
      ' (' + SysErrorMessage(GetLastError) + ')';
    Result := '';
  end;

end;


// https://stackoverflow.com/questions/49252038/how-to-get-the-full-file-name-from-a-textfile-variable
function resolveWinSymlink(filepath: string; recursive: boolean = True): string;
var
  //fileinfo: TSearchRec;
  errorinfo: string;
  myhandle: THandle;
begin
  Result := filepath;
  filepath := GetForcedPathDelims(filepath);
  if FileExists(filepath, False) then
  begin
    try
      myhandle := CreateFile(PChar(filepath), 0, FILE_SHARE_READ, nil,
        OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);
      if Win32Check(myhandle <> INVALID_HANDLE_VALUE) then
        Result := FileHandleToFileName(myhandle)
      else
        errorinfo := 'Could not get handle for: ' + filepath;
    except
      on e: Exception do
      begin
        LogDatei.log_prog('Exception in resolveWinSymlink: ' +
          e.message, LLError);
        LogDatei.log_prog('Returning input: ' + filepath, LLError);
        Result := filepath;
      end;
    end;
  end
  else // return filepath also if filepath does not exists
    Result := filepath;
end;


(*
// https://stackoverflow.com/questions/23409775/using-filegetsymlinktarget-in-delphi-xe5-does-not-return-the-network-address-to
// https://sad-notes.ru/en/poluchit-put-k-obektu-na-kotoryj-ukazyvaet-simvolicheskoj-ssylki-ili-tochki-soedineniya-2/
function SymLinkTarget(path: string): string;
var
  LinkHandle: THandle;
  TargetName: array [0..512] of Char;
begin
  LinkHandle := CreateFile(PChar(path), 0, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);
  Win32Check(LinkHandle <> INVALID_HANDLE_VALUE);
  try
    if GetFinalPathNameByHandle(LinkHandle, TargetName, 512, FILE_NAME_NORMALIZED) > 0 then
      Result: Copy (TargetName, 5, maxint) / TargetName receives in the form of UNC ways (q??C:'Users'...)
    else
      RaiseLastOSError;
  finally
    CloseHandle(LinkHandle);
  end;
end;
*)
end.
