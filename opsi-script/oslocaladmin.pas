unit oslocaladmin;


// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: Rupert Roeder, detlef oertel
// credits: http://www.opsi.org/credits/

//***************************************************************************
// Subversion:
// $Revision: 453 $
// $Author: oertel $
// $Date: 2016-05-27 17:56:01 +0200 (Fr, 27 Mai 2016) $
//***************************************************************************


{$mode delphi}

{$RANGECHECKS ON}
{$OVERFLOWCHECKS ON}
{$IOCHECKS ON}
{$OBJECTCHECKS ON}
{$VARSTRINGCHECKS ON}
{$LONGSTRINGS ON}


interface

uses
{$IFDEF WINDOWS}
//  JwaWindows,
  JwaWinType,
  jwatlhelp32,
  jwawinbase,
  jwawinnt,
  JwaProfInfo,
  JwaUserEnv,
  JwaWtsApi32,
  jclexcerpt,
  //JclMiscel,
  //JclBase,
  //JclSecurity,
  Windows,
  ActiveX,
  ComObj, (* zur Laufzeit von CoCreateInstance benoetigt *)
  winsock,
  registry,
  VersionInfoX,
  //unitImpersonator,
  DSiWin32,
  osfuncwin2,
  osfuncwin3,
  //unitImpersonator,
{$ENDIF}
  LCLIntf,
  LCLProc,
  process,
  SysUtils,
  Classes,
  shellapi,
  Shlobj,
  (* for infozip  *)
  packdefs,
  {$IFDEF WINDOWS}zipinter, {$ENDIF}
  wispecfolder,
  oslog;

const
  SID_REVISION = 1;
  FILENAME_ADVAPI32 = 'ADVAPI32.DLL';
  PROC_CONVERTSIDTOSTRINGSIDA = 'ConvertSidToStringSidA';
  ADMIN_SIDSTRING = 'S-1-5-32-544';

type
  Tadminmode = (useronly, full);
  TRunAs = (traPcpatch, traInvoker, traAdmin, traAdminProfile, traAdminProfileExplorer,
    traAdminProfileImpersonate, traAdminProfileImpersonateExplorer, traLoggedOnUser);

  TConvertSidToStringSidA = function(Sid: PSID; var StringSid: LPSTR): BOOL; stdcall;
(*
type
  PLuid = ^LUID;

  _LUID = record
    LowPart: DWORD;
    HighPart: longint;
  end;
  LUID = _LUID;

  USHORT = word;

  _LSA_UNICODE_STRING = record
    Length: USHORT;
    MaximumLength: USHORT;
    Buffer: LPWSTR;
  end;
  LSA_UNICODE_STRING = _LSA_UNICODE_STRING;

  _SECURITY_LOGON_TYPE = (
    Interactive = 2,
    Network,
    Batch,
    Service,
    Proxy,
    Unlock,
    NetworkCleartext,
    NewCredentials,
    RemoteInteractive,
    CachedInteractive,
    CachedRemoteInteractive);
  SECURITY_LOGON_TYPE = _SECURITY_LOGON_TYPE;
  TSecurityLogonType = SECURITY_LOGON_TYPE;

  PSECURITY_LOGON_SESSION_DATA = ^SECURITY_LOGON_SESSION_DATA;

  _SECURITY_LOGON_SESSION_DATA = record
    Size: ULONG;
    LogonId: LUID;
    UserName: LSA_UNICODE_STRING;
    LogonDomain: LSA_UNICODE_STRING;
    AuthenticationPackage: LSA_UNICODE_STRING;
    LogonType: SECURITY_LOGON_TYPE;
    Session: ULONG;
    Sid: PSID;
    LogonTime: LARGE_INTEGER;
    LogonServer: LSA_UNICODE_STRING;
    DnsDomainName: LSA_UNICODE_STRING;
    Upn: LSA_UNICODE_STRING;
  end;
  SECURITY_LOGON_SESSION_DATA = _SECURITY_LOGON_SESSION_DATA;
  TSecurityLogonSessionData = SECURITY_LOGON_SESSION_DATA;
 *)


var
  opsiSetupAdminPassword: string;
  opsiSetupAdmin_Password: string;
  opsiSetupAdmin_LogonHandle: THandle;
  opsiSetupAdmin_org_LogonHandle: THandle;
  opsiSetupAdmin_ProfileHandle: THandle;
  opsiSetupAdmin_lpEnvironment: LPVOID;
  opsiSetupAdmin_startupinfo_help: string;
  opsiSetupAdmin_processInfoShell_hProcess: THandle;
  opsiSetupAdmin_processInfoShell_hThread: THandle;
  opsiSetupAdmin_created: boolean = False;
  opsiSetupAdmin_pSecAttrib: jwawinbase.LPSECURITY_ATTRIBUTES;
  opsiSetupAdmin_runElevated : boolean = false;
  //opsiSetupAdmin_pSecAttrib: jwawindows.LPSECURITY_ATTRIBUTES;
//opsiSetupAdmin_StartUpInfo : jwawinbase.TStartupInfo;

function CreateTemporaryLocalAdmin(RunAs: TRunAs): boolean;
function LsaNtStatusToWinError(Status: cardinal): ULONG; stdcall;
  external 'Advapi32.dll';
//function LsaEnumerateLogonSessions(LogonSessionCount: PULONG;
  //LogonSessionList: PLUID): longint; stdcall; external 'Secur32.dll';
//function LsaGetLogonSessionData(LogonId: PLUID;
  //var ppLogonSessionData: PSECURITY_LOGON_SESSION_DATA): ULONG; stdcall;
  //external 'Secur32.dll';
//function LsaFreeReturnBuffer(Buffer: Pointer): ULONG; stdcall; external 'Secur32.dll';


implementation

uses
  osfunc;

function CreateTemporaryLocalAdmin(RunAs: TRunAs): boolean;
type
  TWtsProcessInfoArray = array [1..$4000] of TWtsProcessInfo;
  PWtsprocessInfoArray = ^TWtsProcessInfoArray;

var
  wGroup: WideString;
  tempstr: string;

  lastError: DWORD;
  logonHandle: THandle;
  processInfo: jwawinbase.TProcessInformation;
  processInfoShell: jwawinbase.PROCESS_INFORMATION;
  startupInfo: jwawinbase.TStartupInfo;
  startupInfoShell: jwawinbase.STARTUPINFO;
  useWorkDir: string;
  mypass: string;
  //lpProfileInfo: jwaprofinfo.PROFILEINFO;
  lpProfileInfo: PROFILEINFO;
  lpEnvironment: LPLPVOID;
  //myimperson : TImpersonator;
  ConsoleTitle: string;
  Help: string;
  WinStaName: string;
  DesktopName: string;
  hUserToken: THandle;
  hWindowStation: HWINSTA;
  hDesktop: HDESK;
  dwSessionId: DWORD;
  //pInfoArray : PWtsprocessInfoArray;
  pInfo: PWtsProcessInfo;
  nCount: DWord;
  notfound: boolean;
  counter: integer;
  winlogonpid: DWORD;
  winlogonProcess: THandle;
  myToken, myduptoken: cardinal;
  //pSecAttrib: jwawinbase.LPSECURITY_ATTRIBUTES;
  toktype: TOKEN_TYPE;
  dwProcessId, dwThreadId,dwLogonType: DWORD;
  aktprocesshandle: HWND;
  wnetresult : dword;

const
  // default values for window stations and desktops
  CreateProcDEFWINSTATION = 'WinSta0';
  CreateProcDEFDESKTOP = 'Default';
  CreateProcDOMUSERSEP = '\';

begin
  if not ('system' = LowerCase(DSiGetUserName)) then
  begin
    LogDatei.DependentAdd(
      'Could not create temporary admin because not running as SYSTEM.',
      LLWarning);
    Result := False;
  end
  else
  begin
    // remove if existing
    if opsiSetupAdmin_created then
      DeleteTemporaryLocalAdmin;
    Result := False;

    if getLoggedInUser = '' then
    begin
      dwThreadId := GetCurrentThreadId;
      LogDatei.DependentAdd('Using CurrentThread for user: ' +
        getLoggedInUser, LLDebug);
    end
    else
    begin
      aktprocesshandle := FindWindow('Progman', nil);
      // maybe use GetDesktopWindow
      if aktprocesshandle = 0 then
        dwThreadId := GetCurrentThreadId  //failed
      else
      begin
        dwThreadId := GetWindowThreadProcessId(aktprocesshandle, @dwProcessId);
        if dwThreadId = 0 then
          dwThreadId := GetCurrentThreadId // failed
        else
          LogDatei.DependentAdd('Found ThreadId :' + IntToStr(
            dwThreadId) + ' for logged on user: ' + getLoggedInUser, LLDebug);
      end;
    end;

    if runas in [traAdmin, traAdminProfile, traAdminProfileExplorer,
      traAdminProfileImpersonateExplorer, traAdminProfileImpersonate] then
    begin
      // get localized name of administrators
      wGroup := StrSIDToName(ADMIN_SIDSTRING);
      opsiSetupAdmin_Password := randomstr(True);
      mypass := opsiSetupAdmin_Password;
      Result := CreateWinUser(DSiGetComputerName, 'opsiSetupAdmin',
        opsiSetupAdmin_Password, wGroup);
    end;
    if not Result then
    begin
      LogDatei.DependentAdd('Failed to create temporary Admin opsiSetupAdmin - retry',
        LLWarning);
      DeleteTemporaryLocalAdmin;
      Result := CreateWinUser(DSiGetComputerName, 'opsiSetupAdmin',
        opsiSetupAdmin_Password, wGroup);
    end;
    if not Result then
      LogDatei.DependentAdd('Failed to create temporary Admin opsiSetupAdmin ', LLError)
    else
    begin
      LogDatei.DependentAdd(
        'Created temporary Admin opsiSetupAdmin as member of group: ' +
        wgroup, LLDebug);
      opsiSetupAdmin_created := True;
       //if (GetNTVersionMajor >= 6) and opsiSetupAdmin_runElevated then
       //if (GetNTVersionMajor >= 6) then
       //begin
       //  dwLogonType := LOGON32_LOGON_NETWORK_CLEARTEXT;
       //  LogDatei.DependentAdd('runElevated=true + nt6 --> LOGON_NETWORK_CLEARTEXT', LLDebug2);
       //end
       //else dwLogonType := LOGON32_LOGON_INTERACTIVE;
       dwLogonType := LOGON32_LOGON_INTERACTIVE;
      if not LogonUser(PChar('opsiSetupAdmin'), PChar(DSiGetComputerName),
        PChar(mypass), dwLogonType, LOGON32_PROVIDER_DEFAULT,
        logonHandle) then
      begin
        Result := False;
        LogDatei.DependentAdd('Logged in as temporary Admin opsiSetupAdmin failed: '
          + IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) +
          ')', LLError);
      end
      else
      begin
        opsiSetupAdmin_org_logonHandle := logonHandle;
        opsiSetupAdmin_logonHandle := logonHandle;
        LogDatei.DependentAdd('Logged in as temporary Admin opsiSetupAdmin ', LLDebug);

        if (GetNTVersionMajor >= 6) and opsiSetupAdmin_runElevated then
        //if OpenShellProcessToken('winlogon.exe', hUserToken) then
        if OpenShellProcessInSessionToken('winlogon.exe',1,'opsiSetupAdmin',
                       opsiSetupAdmin_org_logonHandle, opsiSetupAdmin_logonHandle) then
           LogDatei.DependentAdd('Switched to winlogon token',LLDebug2)
        else LogDatei.DependentAdd('Failed to Switch to winlogon token',LLDebug2);

        if runas in [traAdminProfile, traAdminProfileExplorer,
          traAdminProfileImpersonate, traAdminProfileImpersonateExplorer] then
        begin
          LogDatei.DependentAdd(
            'we will work with the logged on user with profile', LLInfo);
          FillChar(lpProfileInfo, SizeOf(lpProfileInfo), 0);
          lpProfileInfo.dwSize := SizeOf(lpProfileInfo);
          lpProfileInfo.lpUserName := PChar('opsiSetupAdmin');
          lpProfileInfo.dwFlags := 0;
          if not LoadUserProfile(logonHandle, lpProfileInfo) then
          begin
            Result := False;
            LogDatei.DependentAdd(
              'Load profile for temporary Admin opsiSetupAdmin failed: ' +
              IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) +
              ')', LLError);
          end
          else
          begin
            LogDatei.DependentAdd('Loaded profile for temporary Admin opsiSetupAdmin ',
              LLDebug);
            opsiSetupAdmin_ProfileHandle := lpProfileInfo.hProfile;
            lpEnvironment := nil;
(*
            if not CreateEnvironmentBlock(lpEnvironment,logonHandle,false) then
            begin
              result := false;
              LogDatei.DependentAdd('Create EnvironmentBlock for temporary Admin opsiSetupAdmin failed: '
               + IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) + ')', LLError);
            end
            else
*)
            begin
              //           LogDatei.DependentAdd(
              //             'Created EnvironmentBlock for temporary Admin opsiSetupAdmin ', LLDebug);

              //opsiSetupAdmin_lpEnvironment := lpEnvironment^;
              opsiSetupAdmin_lpEnvironment := nil;
              if not DSiEnablePrivilege('SE_TCB_NAME') then
              begin
                LogDatei.DependentAdd('EnablePrivilege Error: ' +
                  IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) +
                  ')', LLError);
              end;
              if not DSiEnablePrivilege('SE_PRIVILEGE_ENABLED') then
              begin
                LogDatei.DependentAdd('EnablePrivilege Error: ' +
                  IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) +
                  ')', LLError);
              end;
              if not DSiEnablePrivilege('SE_ASSIGNPRIMARYTOKEN_NAME') then
              begin
                LogDatei.DependentAdd('EnablePrivilege Error: ' +
                  IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) +
                  ')', LLError);
              end;
              if not DSiEnablePrivilege('SE_INCREASE_QUOTA_NAME') then
              begin
                LogDatei.DependentAdd('EnablePrivilege Error: ' +
                  IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) +
                  ')', LLError);
              end;
              if not DSiEnablePrivilege('SE_RESTORE_NAME') then
              begin
                LogDatei.DependentAdd('EnablePrivilege Error: ' +
                  IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) +
                  ')', LLError);
              end;
              if not DSiEnablePrivilege('SE_BACKUP_NAME') then
              begin
                LogDatei.DependentAdd('EnablePrivilege Error: ' +
                  IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) +
                  ')', LLError);
              end;

              hWindowStation := GetProcessWindowStation;
              WinStaName := GetUserObjectName(hWindowStation);

              if WinStaName = '' then
                WinStaName := CreateProcDEFWINSTATION;

              if not SetUserObjectFullAccess(hWindowStation) then
              begin
                Result := False;
                LogDatei.DependentAdd(
                  'SetUserObjectFullAccess(hWindowStation) failed: ' +
                  IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) +
                  ')', LLError);
              end;


              hDesktop := GetThreadDesktop(dwThreadId);
              DesktopName := GetUserObjectName(hDesktop);
              if DesktopName = '' then
                DesktopName := CreateProcDEFDESKTOP;

              if not SetUserObjectFullAccess(hDesktop) then
              begin
                Result := False;
                LogDatei.DependentAdd('SetUserObjectFullAccess(hDesktop) failed: '
                  + IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) +
                  ')', LLError);
              end;
              opsiSetupAdmin_startupinfo_help := WinStaName + '\' + DeskTopName;
              LogDatei.DependentAdd('Using Desktop : ' + opsiSetupAdmin_startupinfo_help,
                LLDebug);
              // get folders for opsiSetupAdmin
              initializeFoldersForToken(opsiSetupAdmin_org_logonHandle);
              //retrieveFoldersFromWinApi;
              // create late used security attribute

              opsiSetupAdmin_pSecAttrib := nil;

(*
              // elevate logon handle if we are on nt6
              if GetNTVersionMajor >= 6 then
              begin

                if OpenShellProcessInSessionToken('winlogon.exe',
                WTSGetActiveConsoleSessionId, 'opsiSetupAdmin',logonhandle, myduptoken) then
                    opsiSetupAdmin_logonHandle := myduptoken
                else
                    LogDatei.DependentAdd('Could not find winlogon token of created user.', LLError);
              end;
*)
(*
                New(opsiSetupAdmin_pSecAttrib);
                opsiSetupAdmin_pSecAttrib^.nLength := SizeOf(opsiSetupAdmin_pSecAttrib^);
                opsiSetupAdmin_pSecAttrib^.lpSecurityDescriptor := nil;
                opsiSetupAdmin_pSecAttrib^.bInheritHandle := False;
                dwSessionId := WTSGetActiveConsoleSessionId;

                if not WTSEnumerateProcesses(WTS_CURRENT_SERVER_HANDLE,
                  0, 1, pInfo, nCount) then
                begin
                  //error
                  LogDatei.DependentAdd('WTSEnumerateProcesses failed: ' +
                    IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) +
                    ')', LLError);
                end
                else
                begin
                  if ncount = 0 then
                    LogDatei.DependentAdd('No processes found.', LLError);
                  notfound := True;
                  counter := 0;
                  while notfound and (counter < ncount) do
                  begin
                    if (pInfo.SessionId = dwSessionId) and
                      (pInfo.pProcessName = PChar('winlogon.exe')) then
                      //(pInfo.pProcessName = PChar('opsiclientd')) then
                    begin
                      notfound := False;
                      winlogonpid := pInfo.ProcessId;
                    end;
                    Inc(pInfo);
                    inc(counter);
                  end;
                  // try winlogon if no opsiclientd was found
                  counter := 0;
                  while notfound and (counter > ncount) do
                  begin
                    if (pInfo.SessionId = dwSessionId) and
                      //(pInfo.pProcessName = PChar('winlogon')) then
                      (pInfo.pProcessName = PChar('opsiclientd')) then
                    begin
                      notfound := False;
                      winlogonpid := pInfo.ProcessId;
                    end;
                    Inc(pInfo);
                  end;
                  if notfound then
                    LogDatei.DependentAdd('Could not find winlogon token of created user.', LLError)
                  else
                  begin
                    winlogonProcess := OpenProcess(MAXIMUM_ALLOWED, False, winlogonpid);
                    OpenProcessToken(winlogonProcess, TOKEN_DUPLICATE, mytoken);
                    toktype := TokenPrimary;
                    DuplicateTokenEx(mytoken, MAXIMUM_ALLOWED, opsiSetupAdmin_pSecAttrib,
                      jwawinnt.SecurityIdentification,
                      jwawinnt.TokenPrimary, myduptoken);
                    opsiSetupAdmin_logonHandle := myduptoken;
                    LogDatei.DependentAdd('Using winlogon token of created user.', LLDebug)
                  end
                end;
*)


              if runas in [traAdminProfileImpersonate,
                traAdminProfileImpersonateExplorer] then
              begin
(*
                wnetresult :=  unmountSmbShare('p:', true);
                if NO_ERROR <>  wnetresult then
                  LogDatei.DependentAdd('Error unmounting p: '+ SysErrorMessage(wnetresult), LLWarning);
*)
                if ImpersonateLoggedOnUser(opsiSetupAdmin_logonHandle) then
                begin
                  LogDatei.DependentAdd('Impersonated to temporary admin.', LLDebug);
                  LogDatei.DependentAdd('current appdata is now: ' + GetAppDataPath, LLDebug);
(*
                  wnetresult :=  mountSmbShare('p:','\\sepiolina\opsi_depot','pcpatch','linux123',false);
                  if NO_ERROR <>  wnetresult then
                    LogDatei.DependentAdd('Error remounting p: '+ SysErrorMessage(wnetresult), LLWarning);
*)
                end
                else
                  LogDatei.DependentAdd('Failed to impersonate to temporary admin.', LLError);
              end;

    (*
               // obtain the process id of the winlogon process that
               // is running within the currently active session
               //Process[] processes = Process.GetProcessesByName("winlogon");
               GetProcessesBy
               end;
               end;
               end;
               end;
                end;
    (*
               foreach (Process p in processes)
               {
                   if ((uint)p.SessionId == dwSessionId)
                   {
                       winlogonPid = (uint)p.Id;
                   }
               }
               // obtain a handle to the winlogon process
               hProcess = OpenProcess(MAXIMUM_ALLOWED, false, winlogonPid);
               // obtain a handle to the access token of the winlogon process
               if (!OpenProcessToken(hProcess, TOKEN_DUPLICATE, ref hPToken))
               {
                   CloseHandle(hProcess);
                   return false;
               }
               // Security attibute structure used in DuplicateTokenEx and CreateProcessAsUser
               // I would prefer to not have to use a security attribute variable and to just
               // simply pass null and inherit (by default) the security attributes
               // of the existing token. However, in C# structures are value types and therefore
               // cannot be assigned the null value.
               SECURITY_ATTRIBUTES sa = new SECURITY_ATTRIBUTES();
               sa.Length = Marshal.SizeOf(sa);

               // copy the access token of the winlogon process;
               // the newly created token will be a primary token
               if (!DuplicateTokenEx(hPToken, MAXIMUM_ALLOWED, ref sa,
                       (int)SECURITY_IMPERSONATION_LEVEL.SecurityIdentification,
                       (int)TOKEN_TYPE.TokenPrimary, ref hUserTokenDup))
               {
                   CloseHandle(hProcess);
                   CloseHandle(hPToken);
                   return false;
               }
               STARTUPINFO si = new STARTUPINFO();
               si.cb = (int)Marshal.SizeOf(si);

               // interactive window station parameter; basically this indicates
               // that the process created can display a GUI on the desktop
               si.lpDesktop = @"winsta0\default";

               // flags that specify the priority and creation method of the process
               int dwCreationFlags = NORMAL_PRIORITY_CLASS | CREATE_NEW_CONSOLE;

               // create a new process in the current User's logon session
               bool result = CreateProcessAsUser(hUserTokenDup,  // client's access token
                                               null,             // file to execute
                                               applicationName,  // command line
                                               ref sa,           // pointer to process SECURITY_ATTRIBUTES
                                               ref sa,           // pointer to thread SECURITY_ATTRIBUTES
                                               false,            // handles are not inheritable
                                               dwCreationFlags,  // creation flags
                                               IntPtr.Zero,      // pointer to new environment block
                                               null,             // name of current directory
                                               ref si,           // pointer to STARTUPINFO structure
                                               out procInfo      // receives information about new process
                                               );
    *)
            end;


            if Result and (runas in [traAdminProfileExplorer,
              traAdminProfileImpersonateExplorer]) then
            begin
              // Step 4: set the startup info for the new process
              //ConsoleTitle := UserDomain + UserName;
              ConsoleTitle := 'opsi-winst-as-admin';
              opsiSetupAdmin_startupinfo_help := WinStaName + '\' + DeskTopName;
              FillChar(StartUpInfoShell, SizeOf(StartUpInfoShell), #0);
              with StartUpInfoShell do
              begin
                cb := SizeOf(StartUpInfo);
                lpTitle := PChar('shell-4-opsi-admin');
                Help := opsiSetupAdmin_startupinfo_help;
                lpDesktop := PChar(Help);
                dwFlags := STARTF_USESHOWWINDOW;
                wShowWindow := SW_SHOWMINIMIZED;
              end;

              //else
              //if workDir = '' then
              GetDir(0, useWorkDir);
              //else
              //  useWorkDir := workDir;
              //FillChar(processInfo, SizeOf(processInfo), 0);
              FillChar(processInfoShell, SizeOf(processInfoShell), 0);
              //FillChar(startupInfo, SizeOf(startupInfo), 0);
              //startupInfo.cb := SizeOf(startupInfo);
              //startupInfo.dwFlags := STARTF_USESHOWWINDOW;
              //startupInfo.wShowWindow := ShowWindowFlag;
              //if not DSiCreateProcessAsUser(logonHandle, nil, PChar(CmdLinePasStr), nil,

              // startup explorer as shell for the environment
              tempstr := 'explorer.exe';
              if not jwawinbase.CreateProcessAsUser(opsiSetupAdmin_logonHandle,
                nil, PChar(tempstr),
                //nil, nil,
                opsiSetupAdmin_pSecAttrib, opsiSetupAdmin_pSecAttrib,
                False, CREATE_NO_WINDOW or CREATE_NEW_PROCESS_GROUP or
                CREATE_UNICODE_ENVIRONMENT, opsiSetupAdmin_lpEnvironment,
                //nil,
                nil, startupInfoShell, processInfoShell) then
              begin
                // we want to go on even if we could not start the explorer
                //Result := False;
                tempstr := tempstr + ' .... CreateProcessAsUser Error ' +
                  IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) + ')';
                LogDatei.DependentAdd(tempstr, LLError);

                //http://stackoverflow.com/questions/3072997/get-window-station-for-a-non-interactive-user-per-process-user-or-session
                //http://www.codeproject.com/Articles/35773/Subverting-Vista-UAC-in-Both-32-and-64-bit-Archite
                // obtain the currently active session id; every logged on
                // User in the system has a unique session id
                CloseHandle(processInfoShell.hProcess);
                CloseHandle(processInfoShell.hThread);
              end
              else
              begin
                LogDatei.DependentAdd(
                  'Created explorer shell for temporary Admin opsiSetupAdmin ', LLDebug);
                opsiSetupAdmin_processInfoShell_hProcess := processInfoShell.hProcess;
                opsiSetupAdmin_processInfoShell_hThread := processInfoShell.hThread;
              end;
              //initializeFoldersForUser(DSiGetComputerName, 'opsiSetupAdmin');
              LogDatei.DependentAdd('current appdata is now: ' +
                GetAppDataPath, LLDebug);

              // elevate logon handle if we are on nt6
              if GetNTVersionMajor >= 6 then
              begin

                if OpenShellProcessToken('explorer.exe', myduptoken) then
                    opsiSetupAdmin_logonHandle := myduptoken
                else
                    LogDatei.DependentAdd('Could not find explorer token of created user.', LLWarning);
              end;

            end;

          end;     // set priv
        end

        else    // adminmode=useronly

        begin
          LogDatei.DependentAdd(
            'we will work with the logged on user without profile', LLInfo);

          opsiSetupAdmin_ProfileHandle := longword(nil);
          opsiSetupAdmin_lpEnvironment := nil;
          opsiSetupAdmin_pSecAttrib := nil;
          if not DSiEnablePrivilege('SE_TCB_NAME') then
          begin
            LogDatei.DependentAdd('EnablePrivilege Error: ' +
              IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) +
              ')', LLError);
          end;
          if not DSiEnablePrivilege('SE_PRIVILEGE_ENABLED') then
          begin
            LogDatei.DependentAdd('EnablePrivilege Error: ' +
              IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) +
              ')', LLError);
          end;
          if not DSiEnablePrivilege('SE_ASSIGNPRIMARYTOKEN_NAME') then
          begin
            LogDatei.DependentAdd('EnablePrivilege Error: ' +
              IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) +
              ')', LLError);
          end;
          if not DSiEnablePrivilege('SE_INCREASE_QUOTA_NAME') then
          begin
            LogDatei.DependentAdd('EnablePrivilege Error: ' +
              IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) +
              ')', LLError);
          end;
          if not DSiEnablePrivilege('SE_RESTORE_NAME') then
          begin
            LogDatei.DependentAdd('EnablePrivilege Error: ' +
              IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) +
              ')', LLError);
          end;
          if not DSiEnablePrivilege('SE_BACKUP_NAME') then
          begin
            LogDatei.DependentAdd('EnablePrivilege Error: ' +
              IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) +
              ')', LLError);
          end;

          hWindowStation := GetProcessWindowStation;
          WinStaName := GetUserObjectName(hWindowStation);

          if WinStaName = '' then
            WinStaName := CreateProcDEFWINSTATION;

          if not SetUserObjectFullAccess(hWindowStation) then
          begin
            Result := False;
            LogDatei.DependentAdd(
              'SetUserObjectFullAccess(hWindowStation) failed: ' +
              IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) +
              ')', LLError);
          end;

          hDesktop := GetThreadDesktop(dwThreadId);
          DesktopName := GetUserObjectName(hDesktop);
          if DesktopName = '' then
            DesktopName := CreateProcDEFDESKTOP;

          if not SetUserObjectFullAccess(hDesktop) then
          begin
            Result := False;
            LogDatei.DependentAdd('SetUserObjectFullAccess(hDesktop) failed: '
              + IntToStr(GetLastError) + ' (' + SysErrorMessage(GetLastError) +
              ')', LLError);
          end;
          opsiSetupAdmin_startupinfo_help := WinStaName + '\' + DeskTopName;
        end;
        //end;
      end;   // load profile
    end;    // logon user
  end;    // running as system
end;



end.

