unit osbglistwinuser;
(******************************************************************
This unit is part of opsi-script background-install extension
Winows only

used to get the list of logged on users
******************************************************************)


{$mode Delphi}

interface

uses
  Classes, SysUtils,
  Windows,
  JwaWtsApi32,
  oslog;

  // WTSEnumerateSessionsW  WTSQuerySessionInformationW  from:
  // https://learn.microsoft.com/en-us/windows/win32/api/wtsapi32/nf-wtsapi32-wtsenumeratesessionsw
  // https://learn.microsoft.com/de-de/windows/win32/api/wtsapi32/nf-wtsapi32-wtsquerysessioninformationw
  // https://forum.lazarus.freepascal.org/index.php?topic=51125.0
  // https://www.delphipraxis.net/192001-wie-findet-man-den-richtigen-angemeldeten-windows-benutzer.html

function getLoggedInUserList(activeonly : boolean = false): TStringList;

implementation

function getUsernameBySession(sessionId: DWord): string;
var
  pBuffer: LPWSTR = nil;
  Bytes: DWORD = 0;
  Username, Domainname: string;
begin
  Domainname := '';
  if WTSQuerySessionInformationW(WTS_CURRENT_SERVER_HANDLE, sessionId,
    WTSUserName, pBuffer, Bytes) then
  begin
    try
      SetString(Username, pBuffer, lstrlenW(pBuffer)); //copy content of pBuffer to Username (not including the terminating null character)
      // get also domain name
      pBuffer := nil;
      Bytes := 0;
      if WTSQuerySessionInformationW(WTS_CURRENT_SERVER_HANDLE, sessionId,
        WTSDomainName, pBuffer, Bytes) then
        begin
         SetString(Domainname, pBuffer, lstrlenW(pBuffer));
        end;
      Result := UTF8Encode(Domainname)+'\'+UTF8Encode(Username);
      LogDatei.log_prog('getUsernameBySession: ' + Result, LLnotice);
    finally
      WTSFreeMemory(pBuffer);
    end;
  end
  else
    LogDatei.log('getUsernameBySession: WTSQuerySessionInformationW failed', LLWarning);
end;

function ConStateToStr(const conState: TWTSConnectStateClass): string;
begin
  case conState of
    WTSActive: Result := 'WTSActive';
    WTSConnected: Result := 'WTSConnected';
    WTSConnectQuery: Result := 'WTSConnectQuery';
    WTSShadow: Result := 'WTSShadow';
    WTSDisconnected: Result := 'WTSDisconnected';
    WTSIdle: Result := 'WTSIdle';
    WTSListen: Result := 'WTSListen';
    WTSReset: Result := 'WTSReset';
    WTSDown: Result := 'WTSDown';
    WTSInit: Result := 'WTSInit';
    else
      Result := 'Unknown';
  end;
end;

function getLoggedInUserList(activeonly : boolean = false): TStringList;
var
  pSessionInfo: PWtsSessionInfoW; //A pointer to an array of WTS_SESSION_INFO structures that represent the retrieved sessions. To free the returned buffer, call the WTSFreeMemory function.
  Count: DWord; //number of WTS_SESSION_INFO structures returned in pSessionInfo
  WinStationName: string; //The WinStation name is a name that Windows associates with the session, for example, "services", "console", or "RDP-Tcp#0".
  State : TWTSConnectStateClass; //A value from the WTS_CONNECTSTATE_CLASS enumeration type that indicates the session's current connection state.
  SessionID: DWord; //Session identifier of the session.
  i: integer;
  Username: string;
begin
  Result := TStringList.Create;
  try
    try
      if Win32Check(WtsEnumerateSessionsW(WTS_CURRENT_SERVER_HANDLE,
        0, 1, pSessionInfo, Count)) then
      begin
        LogDatei.log_prog('WtsEnum: WtsEnumerateSessions success', LLnotice);
        LogDatei.log_prog('WtsEnum: Count=' + IntToStr(Count), LLnotice);
        for i := 0 to Count - 1 do //loop through array of WTS_SESSION_INFO structures
        begin
          SetString(WinStationName, pSessionInfo^.pWinStationName, lstrlenW(pSessionInfo^.pWinStationName));
          SessionID := pSessionInfo^.SessionId;
          State := pSessionInfo^.State;
          LogDatei.log_prog('WtsEnum: SessionID=' + IntToStr(SessionID), LLnotice);
          LogDatei.log_prog('WtsEnum: WinStationName=' + WinStationName, LLnotice);
          LogDatei.log_prog('WtsEnum: State=' + ConStateToStr(State), LLnotice);
          if (not activeonly) or (activeonly and (State = WTSActive)) then // WTSActive: A user is logged on to the WinStation. This state occurs when a user is signed in and actively connected to the device.
          begin
            Username := getUsernameBySession(SessionID);
            if Username <> '' then
            begin
              Result.Add(Username);
            end;
            LogDatei.log('Found logged in user: ' + Username, LLDebug);
          end;
          Inc(pSessionInfo); // next structure
        end;
      end
      else
        LogDatei.log('WtsEnum: WtsEnumerateSessions failed', LLWarning);
    except
      On E: Exception do
      begin
        LogDatei.log('Exception in WtsEnum: ' + e.message, LLError);
      end;
    end;
  finally
    WtsFreeMemory(pSessionInfo);
  end;
end;

end.
