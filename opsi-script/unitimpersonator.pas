unit unitImpersonator;

{$mode delphi}{$H+}

//http://groups.google.com/group/borland.public.delphi.nativeapi.win32/msg/fd1d050a0f8bb8df?dmode=source

interface

uses Windows, Classes, SysUtils,
      jwatlhelp32,
    jwawinbase,
    JwaWinnt,
    jwadde,
      JwaWindows,
    lclstrconsts, StdCtrls;

type

TProfileInfo = record
  dwSize : DWORD;
  dwFlags : DWORD;
  lpUserName : PChar;
  lpProfilePath : PChar;
  lpDefaultPath : PChar;
  lpServerName : PChar;
  lpPolicyPath : PChar;
  hProfile : HKEY;
end;


TImpersonator = class
private
  fTokenHandle : THandle;
  fImpersonating: boolean;
  fProfileLoaded : boolean;
  fProfileInfo : TProfileInfo;
  procedure Impersonate;
  function GetImpersonating: boolean;
  function GetHKCURootKey: HKEY;
public
  constructor Create (const domain, user, password : string);
  constructor CreateLoggedOn;   // Impersonate the currently logged on user.

  destructor Destroy; override;

  property Impersonating : boolean read GetImpersonating;
  property HKCURootKey : HKEY read GetHKCURootKey;
end;

const
  PI_NOUI        = 1;     // Prevents displaying of messages
  PI_APPLYPOLICY = 2;     // Apply NT4 style policy

function LoadUserProfile (hToken : THandle; var profileInfo :
TProfileInfo) : BOOL; stdcall;
function UnloadUserProfile (hToken, HKEY : THandle) : BOOL; stdcall;
function GetCurrentUserName : string;
function OpenProcessHandle (const process : string) : THandle;

implementation

//uses psapi;

function LoadUserProfile (hToken : THandle; var profileInfo :
TProfileInfo) : BOOL; external 'userenv.dll' name 'LoadUserProfileA';
function UnLoadUserProfile (hToken, HKEY : THandle) : BOOL; external
'userenv.dll';

function OpenProcessHandle (const process : string) : THandle;
var
  buffer, pid : PDWORD;
  bufLen, cbNeeded : DWORD;
  hp : THandle;
  fileName : array [0..256] of char;
  i : Integer;
begin
  result := 0;
  bufLen := 65536;
  GetMem (buffer, bufLen);
  try
    if EnumProcesses (buffer, bufLen, cbNeeded) then
    begin
      pid := buffer;
      for i := 0 to cbNeeded div sizeof (DWORD) - 1 do
      begin
        hp := OpenProcess (PROCESS_VM_READ or
PROCESS_QUERY_INFORMATION, False, pid^);
        if hp <> 0 then
        try
          if (GetModuleBaseName (hp, 0, fileName, sizeof (fileName)) >
0) and
             (CompareText (fileName, process) = 0) then
          begin
            result := hp;
            break
          end
        finally
          if result = 0 then
            CloseHandle (hp)
        end;

        Inc (pid)
      end
    end
  finally
    FreeMem (buffer)
  end
end;

function GetExplorerProcessToken : THandle;
var
  explorerProcessHandle : THandle;
begin
  explorerProcessHandle := OpenProcessHandle ('explorer.exe');
  if explorerProcesshandle <> 0 then
  try
    if not OpenProcessToken (explorerProcessHandle, TOKEN_QUERY or
TOKEN_IMPERSONATE or TOKEN_DUPLICATE, result) then
      RaiseLastOSError;
  finally
    CloseHandle (explorerProcessHandle)
  end
  else
    result := INVALID_HANDLE_VALUE;
end;

function GetCurrentUserName : string;
var
  unLen : DWORD;
begin
  unLen := 512;
  SetLength (result, unLen);
  GetUserName (PChar (result), unLen);
  result := PChar (result);
end;

{ TImpersonator }

constructor TImpersonator.Create(const domain, user, password: string);
begin
  if LogonUser (PChar (user), PChar (domain), PChar (password),
LOGON32_LOGON_INTERACTIVE, LOGON32_PROVIDER_DEFAULT, fTokenHandle) then
    Impersonate;
end;

procedure TImpersonator.Impersonate;
var
  userName : string;
begin
  fImpersonating := ImpersonateLoggedOnUser (fTokenHandle);
  if fImpersonating then
  begin
    userName := GetCurrentUserName;

    ZeroMemory (@fProfileInfo, sizeof (fProfileInfo));
    fProfileInfo.dwSize := sizeof (fProfileInfo);
    fProfileInfo.lpUserName := PChar (userName);
    fProfileInfo.dwFlags := PI_APPLYPOLICY;
    fprofileLoaded := LoadUserProfile (fTokenHandle, fProfileInfo);
  end
end;

constructor TImpersonator.CreateLoggedOn;
begin
  fTokenHandle := GetExplorerProcessToken;

  if fTokenHandle <> INVALID_HANDLE_VALUE then
    Impersonate;
end;

destructor TImpersonator.Destroy;
begin
  if fProfileLoaded then
    UnloadUserProfile (fTokenHandle, fProfileInfo.hProfile);

  if fImpersonating then
    RevertToSelf;

  CloseHandle (fTokenHandle);
end;

function TImpersonator.GetImpersonating: boolean;
begin
  result := fImpersonating and fProfileLoaded;
end;

function TImpersonator.GetHKCURootKey: HKEY;
begin
  result := fProfileInfo.hProfile
end;

end.
