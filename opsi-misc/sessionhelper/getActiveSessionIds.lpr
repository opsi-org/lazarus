(* getActiveSessionIds is part of the desktop management solution opsi
   (open pc server integration) http://www.opsi.org

   Copyright (C) 2013 uib GmbH

   http://www.uib.de/

   All rights reserved.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2 as
   published by the Free Software Foundation.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

   @copyright:  uib GmbH <info@uib.de>
   @author: Erol Ueluekmen <e.ueluekmen@uib.de>
   @license: GNU General Public License version 3
*)

program getActiveSessionIds;

//{$mode objfpc}{$H+}

{$mode delphi}
{$RANGECHECKS ON}
{$OVERFLOWCHECKS ON}
{$IOCHECKS ON}
{$OBJECTCHECKS ON}
{$VARSTRINGCHECKS ON}
{$LONGSTRINGS ON}


uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  Windows,
  //DSiWin32,
  registry,
  //unitImpersonator,
  //wilog,
  JwaWinnt,
  jwawinbase,
  JwaWindows,
  //jwawinbase,
  JwaUserEnv,
  //LCLIntf,
  //charencstreams,
  //shellapi,
  Shlobj,
  Variants,
  ActiveX,
  JwaWbemCli;

const
  SID_REVISION = 1;
  FILENAME_ADVAPI32 = 'ADVAPI32.DLL';
  PROC_CONVERTSIDTOSTRINGSIDA = 'ConvertSidToStringSidA';

type

  { TgetActiveSessionIds }

  TgetActiveSessionIds = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { TgetActiveSessionIds }

  TSessionData = record
    UserLuid: TLUID;
    UserName,
    Domain: string;
    LogonType: TSecurityLogonType;
    LogonTime: TDateTime;
    sid: PSID;
  end;

   TConvertSidToStringSidA = function(Sid: PSID; var StringSid: LPSTR): BOOL; stdcall;

var

  hSnapshot, hProcess, hdupToken: THandle;
  Process: TProcessEntry32;
  FoundSessionid: DWORD = 0;
  FoundUserName, FoundDomain: ansistring;
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




function WinGetSidStr(Sid: PSid): string;
var
  SidToStr: TConvertSidToStringSidA;
  h: longword;
  Buf: array [0..MAX_PATH - 1] of char;
  p: PAnsiChar;
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

  procedure TgetActiveSessionIds.DoRun;
  var
    ErrorMsg: string;
    sessionid: cardinal;
    lsauser, wtsuser: string;
    helplongint : longint;
    outstring : string;
    mypointer : pointer;
    sessionlist : TStrings;
  begin
    // quick check parameters
    ErrorMsg := CheckOptions('h', 'help');
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

    { add your program here }
    if ParamCount > 0 then
    begin
      try
        sessionid := cardinal(abs(StrToInt(Params[1])));
      except
        writeln('Exception while converting ' + Params[1] + ' to integer');
      end;
      try
        if WTSQuerySessionInformationA(WTS_CURRENT_SERVER_HANDLE,
          sessionid, WTSUsername, pBuffer, pBytesreturned) then
        begin
          if pBytesreturned > 1 then
          begin
            //SetString(wtsuser,pBuffer^,length(pBuffer^));
            mypointer := pointer(pBuffer^);
            wtsuser := Pchar(mypointer^);
            //wtsuser := PAnsichar(pBuffer^);
            writeln(wtsuser);
          end;
          LSAFreeReturnBuffer(pBuffer);
        end;
      except
        writeln('Exception while WTSQuerySessionInformation');
      end;
      if (LsaNtStatusToWinError(LsaEnumerateLogonSessions(@Count, Luid)) = 0) then
      begin
        i := -1;
        if Count > 0 then
          repeat
            Inc(i);
            // Prüfe auf mögliche Fehler (z.B. Access denied)
            if LsaNtStatusToWinError(LsaGetLogonSessionData(Luid, PSesDat)) = 0 then
            begin
              if (PSesDat^.Session = sessionid) then
              begin
                if LowerCase(PSesDat^.UserName.Buffer) = lowerCase(wtsuser) then
                begin
                  with SessionData do
                  begin
                    UserLuid := Luid^;
                    UserName := PSesDat^.UserName.Buffer;
                    DOmain := PSesDat^.LogonDomain.Buffer;
                    LogonType := PSesDat^.LogonType;
                    sid := PSesDat^.SID;
                    //LogonTime := Now;
                    if FileTimeToLocalFileTime(TFileTime(PSesDat^.LogonTime),LocalFileTime) then
                    begin
                      helplongint :=  LARGE_INTEGER(LocalFileTime).QuadPart;
                      LogonTime:=FileDateToDateTime(helplongint);
                      //result:=true;
                    end;
                    //LogDatei.DependentAdd('Found winlogon for owner:'+UserName+' logon type: ', LLDebug2);
                  end;
                end;
              end;
            end;
          until (i = Count - 1);
          with SessionData do
         begin
          outstring := '{''UserName'':u'''+ UserName+'''';

         end;
        writeln(outstring);
      end
      else writeln('LsaEnumerateLogonSessions failed');
    end
    else // no params
    begin
      sessionlist := TStringlist.create;
      if (LsaNtStatusToWinError(LsaEnumerateLogonSessions(@Count, Luid)) = 0) then
      begin
        i := -1;
        if Count > 0 then
          repeat
            Inc(i);
            // Prüfe auf mögliche Fehler (z.B. Access denied)
            if LsaNtStatusToWinError(LsaGetLogonSessionData(Luid, PSesDat)) = 0 then
            begin
              //if (int64(PSesDat^.LogonType) = 2) or (int64(PSesDat^.LogonType) = 10)then
              begin
                sessionid := PSesDat^.session;
                if sessionlist.IndexOf(inttostr(sessionid)) = 0 then
                   sessionlist.Append(inttostr(sessionid));
              end;
            end;
          until (i = Count - 1);
          outstring := '[';
          for i:=0 to sessionlist.Count -1 do
          begin
            if i > 1 then  outstring := outstring + ',';
            outstring := outstring + sessionlist[i];
          end;
          outstring := outstring + ']';
        writeln(outstring);
      end
      else writeln('LsaEnumerateLogonSessions failed');
    end;
    // stop program loop
    Terminate;
  end;

  constructor TgetActiveSessionIds.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor TgetActiveSessionIds.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TgetActiveSessionIds.WriteHelp;
  begin
    { add your help code here }
    writeln('Usage: ', ExeName, ' -h');
  end;

var
  Application: TgetActiveSessionIds;
begin
  Application := TgetActiveSessionIds.Create(nil);
  Application.Title := 'getActiveSessionIds';
  Application.Run;
  Application.Free;
end.
