unit wispecfolder;

{$MODE Delphi}
{$RANGECHECKS ON}
{$OVERFLOWCHECKS ON}
{$IOCHECKS ON}
{$OBJECTCHECKS ON}
{$VARSTRINGCHECKS ON}
{$LONGSTRINGS ON}


// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the General Public License.

// Text of the GPL: http://www.gnu.org/licenses/gpl.html
// Unofficial GPL Translations: http://www.gnu.org/licenses/translations.html

// author: Rupert Roeder, detlef oertel
// credits: http://www.opsi.org/credits/


interface

uses
{$IFDEF WINDOWS}
  JwaWindows,
  Windows,
  activex,
  comobj,
  registry,
  DSiWin32,
  //wiwin32,
  //wilog,
  //shellapi,
  //JwaWindows,
  //JwaUserEnv,
  shlobj,
{$ENDIF}
  LCLIntf,
  Classes,
  SysUtils;

var
  specialFolders: TStringList;

//procedure initializeFoldersForUserWith(const SID : String);
//procedure initializeFoldersForUser(const domain: string; const user: string);
//procedure initializeFoldersForToken(hToken: THandle);
function getSpecialFolder(csidlValue: integer): string;
procedure retrieveFoldersFromWinApi;

function produceCsidlFromName(const Name: string; var csidl: integer;
  var errormessage: string): boolean;

function produceNameFromCsidl(var Name: string; const csidl: integer;
  var errormessage: string): boolean;

function GetCommonStartmenuPath: string;

function GetCommonDesktopPath: string;

function GuessCommonProfilesPath: string;

function GetCommonAppDataPath: string;

function GetCommonStartupPath: string;
function GetCommonProgramsPath: string;
function GetStartmenuPath: string;
function GetDesktopPath: string;
function GetAppDataPath: string;
function GetStartupPath: string;
function GetProgramsPath: string;
function GetSendToPath: string;

function GetUserProfilePath: string;
function GetProfilesPath: string;

   { beruht darauf, dass das Profiles-Verzeichnis des aktuell angemeldeten Users ausgelesen
     und dessen Name entfernt wird;
     ab NT 5 kann das Ergebnis auch direkt ermittelt werden durch Aufruf von
     CSIDL_PROFILE }

function GetDefaultUsersProfilesPath: string;
function GetNTVersionMajor: Dword;

var

  SHGetFolderPath: function(hwndOwner: HWND; nFolder: integer;
  hToken: THandle; dwFlags: DWORD;
  lpszPath: PChar): HRESULT;
  stdcall = nil;

implementation

//uses wifunc;

var
  //uibNTVersion: TUibNTVersion; // we must distinguish between vista and pre-vista
  errorinfo: string;


const
  csidlstrings: array [0..9] of string = (
    'desktop',
    'sendto',
    'startmenu',
    'startup',
    'programs',
    'desktopdirectory',
    'common_startmenu',
    'common_programs',
    'common_startup',
    'common_desktopdirectory'
    );

function GetNTVersionMajor: Dword;
var
  vi: TOSVersionInfo;

begin
  Result := 0;
  vi.dwOSVersionInfoSize := SizeOf(vi);
  if not GetVersionEx(vi) then
    raise Exception.Create('Fehler ' + IntToStr(GetLastError) +
      ' ("' + syserrormessage(GetLastError) + '")')
  else
    Result := vi.dwMajorVersion;
end;



function produceCsidlFromName(const Name: string; var csidl: integer;
  var errormessage: string): boolean;

var
  i: integer;
begin

  Result := True;
  errormessage := '';
  csidl := 0;

  if lowercase(Name) = csidlstrings[0] then
    csidl := CSIDL_DESKTOP
  else if lowercase(Name) = csidlstrings[1] then
    csidl := CSIDL_SENDTO
  else if lowercase(Name) = csidlstrings[2] then
    csidl := CSIDL_STARTMENU
  else if lowercase(Name) = csidlstrings[3] then
    csidl := CSIDL_STARTUP
  else if lowercase(Name) = csidlstrings[4] then
    csidl := CSIDL_PROGRAMS
  else if lowercase(Name) = csidlstrings[5] then
    csidl := CSIDL_DESKTOPDIRECTORY
  else if lowercase(Name) = csidlstrings[6] then
    csidl := CSIDL_COMMON_STARTMENU
  else if lowercase(Name) = csidlstrings[7] then
    csidl := CSIDL_COMMON_PROGRAMS
  else if lowercase(Name) = csidlstrings[8] then
    csidl := CSIDL_COMMON_STARTUP
  else if lowercase(Name) = csidlstrings[9] then
    csidl := CSIDL_COMMON_DESKTOPDIRECTORY
  else
  begin
    Result := False;
    errormessage := 'shell folder name "' + Name + '" not recognized. Valid names are ';
    for i := 0 to length(csidlstrings) - 1 do
      errormessage := errormessage + csidlstrings[i] + ', ';
  end;
end;


function produceNameFromCsidl(var Name: string; const csidl: integer;
  var errormessage: string): boolean;

var
  i: integer;
begin

  Result := True;
  errormessage := '';
  Name := '';

  if csidl = CSIDL_DESKTOP then
    Name := csidlstrings[0]
  else if csidl = CSIDL_SENDTO then
    Name := csidlstrings[1]
  else if csidl = CSIDL_STARTMENU then
    Name := csidlstrings[2]
  else if csidl = CSIDL_STARTUP then
    Name := csidlstrings[3]
  else if csidl = CSIDL_PROGRAMS then
    Name := csidlstrings[4]
  else if csidl = CSIDL_DESKTOPDIRECTORY then
    Name := csidlstrings[5]
  else if csidl = CSIDL_COMMON_STARTMENU then
    Name := csidlstrings[6]
  else if csidl = CSIDL_COMMON_PROGRAMS then
    Name := csidlstrings[7]
  else if csidl = CSIDL_COMMON_STARTUP then
    Name := csidlstrings[8]
  else if csidl = CSIDL_COMMON_DESKTOPDIRECTORY then
    Name := csidlstrings[9]
  else
  begin
    Result := False;
    errormessage := 'shell folder name not recognized. Valid names are ';
    for i := 0 to length(csidlstrings) - 1 do
      errormessage := errormessage + csidlstrings[i] + ', ';
  end;
end;




{******************************************************
For internal use
******************************************************}
procedure Freecsidl(csidl: PItemIDList);
var
  alloc: IMalloc;
begin
  if Succeeded(SHGetMalloc(alloc)) then
  begin
    alloc.Free(csidl);
  end;
end;

{******************************************************}




function getSpecialFolder(csidlValue: integer): string;
var
  csidl: pItemIDList;
  nameBuf: array [0..MAX_PATH] of char;
begin
  @SHGetFolderPath := GetProcAddress(LoadLibrary('shell32.dll'),
    'SHGetFolderPathA');
  Result := '';
  if assigned(SHGetFolderPath) then
  //if SUCCEEDED(SHGetSpecialFolderLocation (0, csidlValue, csidl)) then
  //if SUCCEEDED(SHGetFolderLocation(0, csidlValue, nil,0, csidl)) then
  if SUCCEEDED(SHGetFolderPath(0, csidlValue, 0, 0, namebuf)) then
  begin
    Result := StrPas(namebuf);
    (*
    if csidl <> nil then
    begin
      if SHGetPathFromIDList(csidl, namebuf) then
        Result := StrPas(namebuf);
      Freecsidl(csidl);
    end;
    *)
  end
  else
  //Fix:
  if assigned(SHGetFolderPath) and ((csidlvalue = CSIDL_APPDATA) or
    (csidlvalue = CSIDL_PERSONAL)) then
  begin
    if SUCCEEDED(SHGetFolderPath(0, csidlValue, 0, 0, namebuf)) then
      Result := StrPas(namebuf);
  end;
end;

(*
procedure initializeFoldersForUser(const domain: string; const user: string);
begin
  if Impersonate2User(domain, user) then
  begin
    try
      retrieveFoldersFromWinApi;
    finally
      RevertToSelf;
    end;
  end
  else
    LogDatei.DependentAddError('Error: could not impersonate for user:' + user, LLError);
end;

procedure initializeFoldersForToken(hToken: THandle);
begin
  if ImpersonateLoggedOnUser(hToken) then
  begin
    try
      retrieveFoldersFromWinApi;
    finally
      RevertToSelf;
    end;
  end
  else
    LogDatei.DependentAddError('Error: could not impersonate for user by token.', LLError);
end;

 *)
(*
procedure initializeFoldersForUserWith(const SID : String);
var
  Reg: TRegistry;
  ///r,s,t,x : String;
  ///i : Integer;
  value, profile, stdvalue,commonvalue : String;

  function getFuncPath(const stdpath: string) : string;
  var
    i,k : integer;
    tmpstr : string;
  begin
    i := pos(specialFolders.Values['DefaultUserProfile'],stdpath);
    k := length(stdpath);
    tmpstr := copy(stdpath,i,k);
    result := copy(tmpstr, pos('\',tmpstr)+1,length(tmpstr));
  end;

begin
  Reg := TRegistry.Create(KEY_READ);
  try
    //Reg.RootKey := HKEY_USERS;
    //Reg.OpenKey(SID+'\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders', False);
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.OpenKey('SOFTWARE\Microsoft\Windows NT\CurrentVersion\ProfileList\'+SID, False);
    profile := Reg.ReadString('ProfileImagePath');
    commonvalue := specialFolders.Values['Common AppData'];

    specialFolders.Add ('UsercontextAppData');
    //value := Reg.ReadString('AppData');
    specialFolders.Values['UsercontextAppData'] := profile+'\'+getFuncPath(commonvalue);
    specialFolders.Values['AppData'] := profile+'\'+getFuncPath(commonvalue);

    commonvalue := specialFolders.Values['Common Desktop'];
    specialFolders.Values['Desktop'] := profile+'\'+getFuncPath(commonvalue);

    commonvalue := specialFolders.Values['Common Start Menu'];
    specialFolders.Values ['Start Menu'] := profile+'\'+getFuncPath(commonvalue);

    commonvalue := specialFolders.Values['Common Startup'];
    specialFolders.Values ['Startup'] := profile+'\'+getFuncPath(commonvalue);

    commonvalue := specialFolders.Values['Common Programs'];
    specialFolders.Values ['Programs'] := profile+'\'+getFuncPath(commonvalue);

    commonvalue := specialFolders.Values['Common SendTo'];
    specialFolders.Values ['SendTo'] := profile+'\'+getFuncPath(commonvalue);


    //WinXP standard   C:\Dokumente und Einstellungen\USER\Anwendungsdaten
    //Windows Vista    C:\users\Anwendungdaten


    specialFolders.add('Common Desktop');
    specialFolders.Values['Common Desktop'] := Reg.ReadString('Common Desktop');
    //WinXP standard   C:\Dokumente und Einstellungen\All Users\Desktop
    //Windows Vista    C:\Users\Public\Desktop

    specialFolders.add('Common Start Menu');
    specialFolders.Values ['Common Start Menu'] := Reg.ReadString('Common Start Menu');
    //WinXP standard   C:\Dokumente und Einstellungen\All Users\Startmenü
    //Windows Vista    C:\ProgrammData\Microsoft\Windows\Start Menu


    r := specialFolders.Values['Common Desktop'];
    s := ExtractFileDrive(r);
    t := copy (r, length(s)+2,length(r));
    i := pos ('\', t);
    x := s + '\' + copy (t, 1, i-1);
    specialFolders.add ('Users home');
    specialFolders.Values ['Users home'] := x;
    //WinXP  C:\Dokumente und Einstellungen
    //Vista  C:\Users


  finally
    Reg.Free;
  end;

end;
*)


procedure retrieveFoldersFromHKLM;
var
  Reg: TRegistry;
  r, s, t, x: string;
  i: integer;

begin
  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.OpenKey('Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders', False);
    specialFolders.Add('Common AppData');
    specialFolders.Values['Common AppData'] := Reg.ReadString('Common AppData');
    //WinXP standard   C:\Dokumente und Einstellungen\All Users\Anwendungsdaten
    //Windows Vista    C:\ProgramData

    specialFolders.add('Common Desktop');
    specialFolders.Values['Common Desktop'] := Reg.ReadString('Common Desktop');
    //WinXP standard   C:\Dokumente und Einstellungen\All Users\Desktop
    //Windows Vista    C:\Users\Public\Desktop

    specialFolders.add('Common Start Menu');
    specialFolders.Values['Common Start Menu'] := Reg.ReadString('Common Start Menu');
    //WinXP standard   C:\Dokumente und Einstellungen\All Users\Startmenü
    //Windows Vista    C:\ProgrammData\Microsoft\Windows\Start Menu


    r := specialFolders.Values['Common Desktop'];
    s := ExtractFileDrive(r);
    t := copy(r, length(s) + 2, length(r));
    i := pos('\', t);
    x := s + '\' + copy(t, 1, i - 1);
    specialFolders.add('Users home');
    specialFolders.Values['Users home'] := x;
    //WinXP  C:\Dokumente und Einstellungen
    //Vista  C:\Users

  finally
    Reg.Free;
  end;

end;

procedure retrieveFoldersFromWinApi;
var
  r, s, t, x, profiledir: string;
  charbuf , pstr : pchar;
  i: integer;
  profdirlength : dword;
  Reg: TRegistry;

begin
  try
    specialFolders.Add('Common AppData');
    specialFolders.Values['Common AppData'] := getSpecialFolder(CSIDL_COMMON_APPDATA);

    specialFolders.add('Common Desktop');
    specialFolders.Values['Common Desktop'] :=
      getSpecialFolder(CSIDL_COMMON_DESKTOPDIRECTORY);

    specialFolders.add('Common Start Menu');
    specialFolders.Values['Common Start Menu'] :=
      getSpecialFolder(CSIDL_COMMON_STARTMENU);

    specialFolders.add('Common Startup');
    specialFolders.Values['Common Startup'] := getSpecialFolder(CSIDL_COMMON_STARTUP);

    specialFolders.add('Common Programs');
    specialFolders.Values['Common Programs'] :=
      getSpecialFolder(CSIDL_COMMON_PROGRAMS);

    specialFolders.Add('AppData');
    specialFolders.Values['AppData'] := getSpecialFolder(CSIDL_APPDATA);

    specialFolders.add('Desktop');
    specialFolders.Values['Desktop'] := getSpecialFolder(CSIDL_DESKTOPDIRECTORY);

    specialFolders.add('Start Menu');
    specialFolders.Values['Start Menu'] := getSpecialFolder(CSIDL_STARTMENU);

    specialFolders.add('Startup');
    specialFolders.Values['Startup'] := getSpecialFolder(CSIDL_STARTUP);

    specialFolders.add('Programs');
    specialFolders.Values['Programs'] := getSpecialFolder(CSIDL_PROGRAMS);

    specialFolders.add('SendTo');
    specialFolders.Values['SendTo'] := getSpecialFolder(CSIDL_SENDTO);

    specialFolders.add('current profile');
    specialFolders.Values['current profile'] := getSpecialFolder(CSIDL_PROFILE);

    (*
    r := specialFolders.Values['Common Desktop'];
    s := ExtractFileDrive(r);
    t := copy(r, length(s) + 2, length(r));
    i := pos('\', t);
    x := s + '\' + copy(t, 1, i - 1);
    specialFolders.add('Users home');
    specialFolders.Values['Users home'] := x;
    *)
    // this is the better way via winapi (d.oertel 2.7.2012) :
    profdirlength := MAX_STRING_LENGTH;
    charbuf := StrAlloc(profdirlength+5);
    GetProfilesDirectory(charbuf, profdirlength);
    pstr := StrAlloc(profdirlength+1);
    strlcopy(pstr,charbuf,profdirlength);
    profiledir := pstr;
    specialFolders.Values['Users home'] := profiledir;
    strdispose(pstr);
    strdispose(charbuf);
    (*
    if profiledir = '' then
    begin
      //LogDatei.DependentAdd('Api Call GetProfilesDirectory failed: '+removeLineBreaks(SysErrorMessage(GetLastError)),LLWarning);
      Reg := TRegistry.Create(KEY_READ);
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      Reg.OpenKey('SOFTWARE\Microsoft\Windows NT\CurrentVersion\ProfileList', False);
      profiledir :=  Reg.ReadString('ProfilesDirectory');
      profiledir := StringReplace1(profiledir,'%SystemDrive%',extractfiledrive(GetWinDirectory));
      specialFolders.Values['Users home'] := profiledir;
      Reg.Free;
    end;
    *)

    profdirlength := MAX_STRING_LENGTH;
    specialFolders.add('Common Profile');
    charbuf := StrAlloc(profdirlength+5);
    GetAllUsersProfileDirectory(charbuf, profdirlength);
    pstr := StrAlloc(profdirlength+1);
    strlcopy(pstr,charbuf,profdirlength);
    profiledir := pstr;
    specialFolders.Values['Common Profile'] := profiledir;
    strdispose(pstr);
    strdispose(charbuf);

    profdirlength := MAX_STRING_LENGTH;
    specialFolders.add('DefaultUserProfile');
    charbuf := StrAlloc(profdirlength+5);
    GetDefaultUserProfileDirectory(charbuf, profdirlength);
    pstr := StrAlloc(profdirlength+1);
    strlcopy(pstr,charbuf,profdirlength);
    profiledir := pstr;
    specialFolders.Values['DefaultUserProfile'] := profiledir;
    strdispose(pstr);
    strdispose(charbuf);



    (*
    // At NT5 CSIDL_PROFILE is not impersonated so we have to guess it
    // it is a hack but it seems that no one have a better idea
    // patches are welcome  (d.oertel 15.5.2012)
    // perhaps is GetUserProfileDirectory a good idea  (d.oertel 3.7.2012)
    if (GetNTVersionMajor < 6)
       and  (0=pos(specialFolders.Values['Desktop'], specialFolders.Values['current profile']))
       and runLoginScripts
    then
      specialFolders.Values['current profile'] :=
        specialFolders.Values['Users home'] + '\'+getLoggedInUser;
    *)
  finally
  end;
end;


function GetCommonStartmenuPath: string;
begin
  Result := specialFolders.Values['Common Start Menu'];
end;

function GetCommonDesktopPath: string;
begin
  Result := specialFolders.Values['Common Desktop'];
end;

function GetCommonAppDataPath: string;
begin
  Result := specialFolders.Values['Common AppData'];
end;

function GetCommonStartupPath: string;
begin
  Result := specialFolders.Values['Common Startup'];
end;

function GetCommonProgramsPath: string;
begin
  Result := specialFolders.Values['Common Programs'];
end;

function GetStartmenuPath: string;
begin
  Result := specialFolders.Values['Start Menu'];
end;

function GetDesktopPath: string;
begin
  Result := specialFolders.Values['Desktop'];
end;

function GetAppDataPath: string;
begin
  Result := specialFolders.Values['AppData'];
end;

function GetStartupPath: string;
begin
  Result := specialFolders.Values['Startup'];
end;

function GetProgramsPath: string;
begin
  Result := specialFolders.Values['Programs'];
end;

function GetSendToPath: string;
begin
  Result := specialFolders.Values['SendTo'];
end;

function GetUserProfilePath: string;
begin
  Result := specialFolders.Values['current profile'];
end;


{
function GetProgramDir : String;
begin
  result := getSpecialFolder ($002A);
end
}

function guessCommonProfilesPath: string;
begin
  Result := specialFolders.Values['Users home'];
  if GetNTVersionMajor < 6 then
    Result := Result + '\All Users'
  else
    Result := Result + '\Public';
end;

function getCommonProfilesPath: string;
begin
  Result := specialFolders.Values['Common Profile'];
end;


function GetProfilesPath: string;
begin
  Result := specialFolders.Values['Users home'];
end;

function GetDefaultUsersProfilesPath: string;
begin
  Result := specialFolders.Values['DefaultUserProfile'];
end;

(*
var i : Integer;
   numberOfSlashes : Integer;
begin
  result := getSpecialFolder (CSIDL_personal);
  i := length(result) - 1;
  numberOfSlashes := 0;
  while (numberOfSlashes < 2) and (i >= 0)
  do
  Begin
    if result[i] = '\' then
      inc(numberOfSlashes);
    dec(i);
  End;
  result := copy (result, 0 , i);
end;
*)



{****************************************************** }

begin
  //uibNTVersion := GetUibNTVersion(errorinfo);
  specialFolders := TStringList.Create;
  //retrieveFoldersFromHKLM;
  retrieveFoldersFromWinApi;
end.

