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
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: Rupert Roeder, detlef oertel



interface

uses
{$IFDEF WINDOWS}
  Windows,
  activex,
  comobj,
  registry,
  osfuncwin2,
osfuncwin3,
{$IFNDEF WIN64}
  DSiWin32,
{$ENDIF WIN64}
  oslog,
  shellapi,
  JwaWindows,
  JwaUserEnv,
{$ENDIF}
  LCLIntf,
  Classes,
  shlobj,
  SysUtils;

var
  specialFolders, debugmessages: TStringList;

//procedure initializeFoldersForUserWith(const SID : String);
{$IFDEF WIN32}
procedure initializeFoldersForUser(const domain: string; const user: string);
{$ENDIF WIN32}
procedure initializeFoldersForToken(hToken: THandle);
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

//http://msdn.microsoft.com/en-us/library/windows/desktop/bb762494%28v=vs.85%29.aspx
//http://msdn.microsoft.com/en-us/library/windows/desktop/dd378457%28v=vs.85%29.aspx
//http://blogs.msdn.com/b/patricka/archive/2010/03/18/where-should-i-store-my-data-and-configuration-files-if-i-target-multiple-os-versions.aspx
//http://windowsitpro.com/powershell/easily-finding-special-paths-powershell-scripts

function GetShortWinPathName(const longpath:string):string;

(* aus shlobj.pas
   {$EXTERNALSYM REGSTR_PATH_SPECIAL_FOLDERS}
  REGSTR_PATH_SPECIAL_FOLDERS   = REGSTR_PATH_EXPLORER + '\Shell Folders';
  {$EXTERNALSYM CSIDL_DESKTOP}
  CSIDL_DESKTOP                       = $0000;
  {$EXTERNALSYM CSIDL_INTERNET}
  CSIDL_INTERNET                      = $0001;
  {$EXTERNALSYM CSIDL_PROGRAMS}
  CSIDL_PROGRAMS                      = $0002;
  {$EXTERNALSYM CSIDL_CONTROLS}
  CSIDL_CONTROLS                      = $0003;
  {$EXTERNALSYM CSIDL_PRINTERS}
  CSIDL_PRINTERS                      = $0004;
  {$EXTERNALSYM CSIDL_PERSONAL}
  CSIDL_PERSONAL                      = $0005;
  {$EXTERNALSYM CSIDL_FAVORITES}
  CSIDL_FAVORITES                     = $0006;
  {$EXTERNALSYM CSIDL_STARTUP}
  CSIDL_STARTUP                       = $0007;
  {$EXTERNALSYM CSIDL_RECENT}
  CSIDL_RECENT                        = $0008;
  {$EXTERNALSYM CSIDL_SENDTO}
  CSIDL_SENDTO                        = $0009;
  {$EXTERNALSYM CSIDL_BITBUCKET}
  CSIDL_BITBUCKET                     = $000a;
  {$EXTERNALSYM CSIDL_STARTMENU}
  CSIDL_STARTMENU                     = $000b;
  {$EXTERNALSYM CSIDL_DESKTOPDIRECTORY}
  CSIDL_DESKTOPDIRECTORY              = $0010;
  {$EXTERNALSYM CSIDL_DRIVES}
  CSIDL_DRIVES                        = $0011;
  {$EXTERNALSYM CSIDL_NETWORK}
  CSIDL_NETWORK                       = $0012;
  {$EXTERNALSYM CSIDL_NETHOOD}
  CSIDL_NETHOOD                       = $0013;
  {$EXTERNALSYM CSIDL_FONTS}
  CSIDL_FONTS                         = $0014;
  {$EXTERNALSYM CSIDL_TEMPLATES}
  CSIDL_TEMPLATES                     = $0015;
  {$EXTERNALSYM CSIDL_COMMON_STARTMENU}
  CSIDL_COMMON_STARTMENU              = $0016;
  {$EXTERNALSYM CSIDL_COMMON_PROGRAMS}
  CSIDL_COMMON_PROGRAMS               = $0017;
  {$EXTERNALSYM CSIDL_COMMON_STARTUP}
  CSIDL_COMMON_STARTUP                = $0018;
  {$EXTERNALSYM CSIDL_COMMON_DESKTOPDIRECTORY}
  CSIDL_COMMON_DESKTOPDIRECTORY       = $0019;
  {$EXTERNALSYM CSIDL_APPDATA}
  CSIDL_APPDATA                       = $001a;
  {$EXTERNALSYM CSIDL_PRINTHOOD}
  CSIDL_PRINTHOOD                     = $001b;
  {$EXTERNALSYM CSIDL_ALTSTARTUP}
  CSIDL_ALTSTARTUP                = $001d;         // DBCS
  {$EXTERNALSYM CSIDL_COMMON_ALTSTARTUP}
  CSIDL_COMMON_ALTSTARTUP         = $001e;         // DBCS
  {$EXTERNALSYM CSIDL_COMMON_FAVORITES}
  CSIDL_COMMON_FAVORITES          = $001f;
  {$EXTERNALSYM CSIDL_INTERNET_CACHE}
  CSIDL_INTERNET_CACHE            = $0020;
  {$EXTERNALSYM CSIDL_COOKIES}
  CSIDL_COOKIES                   = $0021;
  {$EXTERNALSYM CSIDL_HISTORY}
  CSIDL_HISTORY                   = $0022;
*)
{
   ********** Code Start **********
'This code was originally written by Dev Ashish.
'It is not to be altered or distributed,
'except as part of an application.
'You are free to use it in any application,
'provided the copyright notice is left unchanged.
'
'Code Courtesy of
'Dev Ashish
'
'   The following table outlines the different DLL versions,
'   and how they were distributed.
'
'   Version     DLL             Distribution Platform
'   4.00          All               Microsoft® Windows® 95/Windows NT® 4.0.
'   4.70          All               Microsoft® Internet Explorer 3.x.
'   4.71          All               Microsoft® Internet Explorer 4.0
'   4.72          All               Microsoft® Internet Explorer 4.01 and Windows® 98
'   5.00          Shlwapi.dll  Microsoft® Internet Explorer 5
'   5.00          Shell32.dll   Microsoft® Windows® 2000.
'   5.80          Comctl32.dll Microsoft® Internet Explorer 5
'   5.81          Comctl32.dll Microsoft® Windows 2000
'
'

'   © Microsoft. Information copied from Microsoft's
'   Platform SDK Documentation in MSDN
'   (http://msdn.microsoft.com)
'
'   If a special folder does not exist, you can force it to be
'   created by using the following special CSIDL:
'   (Version 5.0)
Public Const CSIDL_FLAG_CREATE = &H8000

'   Combine this CSIDL with any of the CSIDLs listed below
'   to force the creation of the associated folder.

'   The remaining CSIDLs correspond to either file system or virtual folders.
'   Where the CSIDL identifies a file system folder, a commonly used path
'   is given as an example. Other paths may be used. Some CSIDLs can be
'   mapped to an equivalent %VariableName% environment variable.
'   CSIDLs are much more reliable, however, and should be used if at all possible.

'   File system directory that is used to store administrative tools for an individual user.
'   The Microsoft Management Console will save customized consoles to
'   this directory and it will roam with the user.
'   (Version 5.0)
Public Const CSIDL_ADMINTOOLS = &H30

'   File system directory that corresponds to the user's
'   nonlocalized Startup program group.
Public Const CSIDL_ALTSTARTUP = &H1D

'   File system directory that serves as a common repository for application-specific
'   data. A typical path is C:\Documents and Settings\username\Application Data.
'   This CSIDL is supported by the redistributable ShFolder.dll for systems that do
'   not have the Internet Explorer 4.0 integrated shell installed.
'   (Version 4.71)
Public Const CSIDL_APPDATA = &H1A

'   Virtual folder containing the objects in the user's Recycle Bin.
Public Const CSIDL_BITBUCKET = &HA

'   File system directory containing containing administrative tools
'   for all users of the computer.
'   Version 5
Public Const CSIDL_COMMON_ADMINTOOLS = &H2F

'   File system directory that corresponds to the nonlocalized Startup program
'   group for all users. Valid only for Windows NT® systems.
Public Const CSIDL_COMMON_ALTSTARTUP = &H1E

'   Application data for all users. A typical path is
'   C:\Documents and Settings\All Users\Application Data.
'   Version 5
Public Const CSIDL_COMMON_APPDATA = &H23

'   File system directory that contains files and folders that appear on the
'   desktop for all users. A typical path is C:\Documents and Settings\All Users\Desktop.
'   Valid only for Windows NT® systems.
Public Const CSIDL_COMMON_DESKTOPDIRECTORY = &H19

'   File system directory that contains documents that are common to all users.
'   A typical path is C:\Documents and Settings\All Users\Documents.
'   Valid for Windows NT® systems and Windows 95 and Windows 98
'   systems with Shfolder.dll installed.
Public Const CSIDL_COMMON_DOCUMENTS = &H2E

'   File system directory that serves as a common repository for all users' favorite items.
'   Valid only for Windows NT® systems.
Public Const CSIDL_COMMON_FAVORITES = &H1F

'   File system directory that contains the directories for the common program
'   groups that appear on the Start menu for all users. A typical path is
'   C:\Documents and Settings\All Users\Start Menu\Programs.
'   Valid only for Windows NT® systems.
Public Const CSIDL_COMMON_PROGRAMS = &H17

'   File system directory that contains the programs and folders that appear on
'   the Start menu for all users. A typical path is
'   C:\Documents and Settings\All Users\Start Menu.
'   Valid only for Windows NT® systems.
Public Const CSIDL_COMMON_STARTMENU = &H16

'   File system directory that contains the programs that appear in the
'   Startup folder for all users. A typical path is
'   C:\Documents and Settings\All Users\Start Menu\Programs\Startup.
'   Valid only for Windows NT® systems.
Public Const CSIDL_COMMON_STARTUP = &H18

'   File system directory that contains the templates that are available to all users.
'   A typical path is C:\Documents and Settings\All Users\Templates.
'   Valid only for Windows NT® systems.
Public Const CSIDL_COMMON_TEMPLATES = &H2D

'   Virtual folder containing icons for the Control Panel applications.
Public Const CSIDL_CONTROLS = &H3

'   File system directory that serves as a common repository for Internet cookies.
'   A typical path is C:\Documents and Settings\username\Cookies.
Public Const CSIDL_COOKIES = &H21

'   Windows Desktop—virtual folder that is the root of the namespace..
Public Const CSIDL_DESKTOP = &H0

'   File system directory used to physically store file objects on the desktop
'   (not to be confused with the desktop folder itself).
'   A typical path is C:\Documents and Settings\username\Desktop
Public Const CSIDL_DESKTOPDIRECTORY = &H10

'   My Computer—virtual folder containing everything on the local computer:
'   storage devices, printers, and Control Panel. The folder may
'   also contain mapped network drives.
Public Const CSIDL_DRIVES = &H11

'   File system directory that serves as a common repository for the user's
'   favorite items. A typical path is C:\Documents and Settings\username\Favorites.
Public Const CSIDL_FAVORITES = &H6

'   Virtual folder containing fonts. A typical path is C:\WINNT\Fonts.
Public Const CSIDL_FONTS = &H14

'   File system directory that serves as a common repository for
'   Internet history items.
Public Const CSIDL_HISTORY = &H22

'   Virtual folder representing the Internet.
Public Const CSIDL_INTERNET = &H1

'   File system directory that serves as a common repository for
'   temporary Internet files. A typical path is
'   C:\Documents and Settings\username\Temporary Internet Files.
Public Const CSIDL_INTERNET_CACHE = &H20

'   File system directory that serves as a data repository for local
'   (non-roaming) applications. A typical path is
'   C:\Documents and Settings\username\Local Settings\Application Data.
'   Version 5
Public Const CSIDL_LOCAL_APPDATA = &H1C

'   My Pictures folder. A typical path is
'   C:\Documents and Settings\username\My Documents\My Pictures.
'   Version 5
Public Const CSIDL_MYPICTURES = &H27

'   A file system folder containing the link objects that may exist in the
'   My Network Places virtual folder. It is not the same as CSIDL_NETWORK,
'   which represents the network namespace root. A typical path is
'   C:\Documents and Settings\username\NetHood.
Public Const CSIDL_NETHOOD = &H13

'   Network Neighborhood—virtual folder representing the
'   root of the network namespace hierarchy.
Public Const CSIDL_NETWORK = &H12

'   File system directory that serves as a common repository for documents.
'   A typical path is C:\Documents and Settings\username\My Documents.
Public Const CSIDL_PERSONAL = &H5

'   Virtual folder containing installed printers.
Public Const CSIDL_PRINTERS = &H4

'   File system directory that contains the link objects that may exist in the
'   Printers virtual folder. A typical path is
'   C:\Documents and Settings\username\PrintHood.
Public Const CSIDL_PRINTHOOD = &H1B

'   User's profile folder.
'   Version 5
Public Const CSIDL_PROFILE = &H28

'   Program Files folder. A typical path is C:\Program Files.
'   Version 5
Public Const CSIDL_PROGRAM_FILES = &H2A

'   A folder for components that are shared across applications. A typical path
'   is C:\Program Files\Common.
'   Valid only for Windows NT® and Windows® 2000 systems.
'   Version 5
Public Const CSIDL_PROGRAM_FILES_COMMON = &H2B

'   Program Files folder that is common to all users for x86 applications
'   on RISC systems. A typical path is C:\Program Files (x86)\Common.
'   Version 5
Public Const CSIDL_PROGRAM_FILES_COMMONX86 = &H2C

'   Program Files folder for x86 applications on RISC systems. Corresponds
'   to the %PROGRAMFILES(X86)% environment variable.
'   A typical path is C:\Program Files (x86).
'   Version 5
Public Const CSIDL_PROGRAM_FILESX86 = &H2A

'   File system directory that contains the user's program groups (which are
'   also file system directories). A typical path is
'   C:\Documents and Settings\username\Start Menu\Programs.
Public Const CSIDL_PROGRAMS = &H2

'   File system directory that contains the user's most recently used documents.
'   A typical path is C:\Documents and Settings\username\Recent.
'   To create a shortcut in this folder, use SHAddToRecentDocs. In addition to
'   creating the shortcut, this function updates the shell's list of recent documents
'   and adds the shortcut to the Documents submenu of the Start menu.
Public Const CSIDL_RECENT = &H8

'   File system directory that contains Send To menu items. A typical path is
'   C:\Documents and Settings\username\SendTo.
Public Const CSIDL_SENDTO = &H9

'   File system directory containing Start menu items.
'   A typical path is C:\Documents and Settings\username\Start Menu.
Public Const CSIDL_STARTMENU = &HB

'   File system directory that corresponds to the user's Startup program group.
'   The system starts these programs whenever any user logs onto Windows NT® or
'   starts Windows® 95. A typical path is
'   C:\Documents and Settings\username\Start Menu\Programs\Startup.
Public Const CSIDL_STARTUP = &H7

'   System folder. A typical path is C:\WINNT\SYSTEM32.
'   Version 5
Public Const CSIDL_SYSTEM = &H25

'   System folder for x86 applications on RISC systems.
'   A typical path is C:\WINNT\SYS32X86.
'   Version 5
Public Const CSIDL_SYSTEMX86 = &H29

'   File system directory that serves as a common repository
'   for document templates.
Public Const CSIDL_TEMPLATES = &H15

'   Version 5.0. Windows directory or SYSROOT. This corresponds to the %windir%
'   or %SYSTEMROOT% environment variables. A typical path is C:\WINNT.
Public Const CSIDL_WINDOWS = &H24

Public Const NOERROR = 0

'   Retrieves a pointer to the ITEMIDLIST structure of a special folder.
Private Declare Function apiSHGetSpecialFolderLocation Lib "shell32" _
    Alias "SHGetSpecialFolderLocation" _
    (ByVal hwndOwner As Long, _
    ByVal nFolder As Long, _
    pcsidl As Long) _
    As Long

'   Converts an item identifier list to a file system path.
Private Declare Function apiSHGetPathFromIDList Lib "shell32" _
    Alias "SHGetPathFromIDList" _
    (csidl As Long, _
    ByVal pszPath As String) _
    As Long

'   Frees a block of task memory previously allocated through a call to
'   the CoTaskMemAlloc or CoTaskMemRealloc function.
Private Declare Sub sapiCoTaskMemFree Lib "ole32" _
    Alias "CoTaskMemFree" _
    (ByVal pv As Long)

Private Const MAX_PATH = 260

Function fGetSpecialFolderLocation(ByVal lngCSIDL As Long) As String
'
'   Returns path to a special folder on the machine
'   without a trailing backslash.
'
'   Refer to the comments in declarations for OS and
'   IE dependent CSIDL values.
'
Dim lngRet As Long
Dim strLocation As String
Dim csidl As Long

    '   retrieve a csidl for the specified location
    lngRet = apiSHGetSpecialFolderLocation(hWndAccessApp, lngCSIDL, csidl)
    If lngRet = NOERROR Then
        strLocation = Space$(MAX_PATH)
        '  convert the csidl to a physical path
        lngRet = apiSHGetPathFromIDList(ByVal csidl, strLocation)
        If Not lngRet = 0 Then
            '   if successful, return the location
            fGetSpecialFolderLocation = Left$(strLocation, _
                                InStr(strLocation, vbNullChar) - 1)
        End If
        '   calling application is responsible for freeing the allocated memory
        '   for csidl when calling SHGetSpecialFolderLocation. We have to
        '   call IMalloc::Release, but to get to IMalloc, a tlb is required.
        '
        '   According to Kraig Brockschmidt in Inside OLE,   CoTaskMemAlloc,
        '   CoTaskMemFree, and CoTaskMemRealloc take the same parameters
        '   as the interface functions and internally call CoGetMalloc, the
        '   appropriate IMalloc function, and then IMalloc::Release.
        Call sapiCoTaskMemFree(csidl)
    End If
End Function
'   ********** Code End **********
}

//var

  //SHGetFolderPath: function(hwndOwner: HWND; nFolder: integer; hToken: THandle; dwFlags: DWORD; lpszPath: PChar): HRESULT; stdcall = nil;

implementation

uses osfunc, osparser;

var
  uibNTVersion: TUibNTVersion; // we must distinguish between vista and pre-vista
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
  nameBuf: array [0..MAX_PATH] of widechar;
  //nameBuf: string ='';
begin
  Result := '';
  if SUCCEEDED(SHGetSpecialFolderLocation(0, csidlValue, csidl)) then
  begin
    if csidl <> nil then
    begin
      if SHGetPathFromIDListW(csidl, namebuf) then
        Result := StrPas(namebuf);
      Freecsidl(csidl);
    end;
  end
  else
    //Fix:
    if assigned(@SHGetFolderPath) and ((csidlvalue = CSIDL_APPDATA) or
      (csidlvalue = CSIDL_PERSONAL)) then
    begin
      if SUCCEEDED(SHGetFolderPathW(0, csidlValue, 0, 0, (namebuf))) then
        Result := StrPas(namebuf);
    end;
  //debugmessages.Add('getSpecialFolder: '+inttostr(csidlValue)+' -> ' + result);
  //if Assigned(LogDatei) then
  //LogDatei.DependentAdd('getSpecialFolder: '+inttostr(csidlValue)+' -> ' + result, LLDebug2);
end;

{$IFDEF WIN32}
procedure initializeFoldersForUser(const domain: string; const user: string);
begin
  if Impersonate2User(domain, user) then
  begin
    try
      //debugmessages.Clear;
      retrieveFoldersFromWinApi;
      //LogDatei.DependentAddStringList(debugmessages,LLEssential);
    finally
      RevertToSelf;
    end;
  end
  else
    LogDatei.DependentAddError('Error: could not impersonate for user:' + user, LLError);
end;
{$ENDIF WIN32}

procedure initializeFoldersForToken(hToken: THandle);
begin
  if ImpersonateLoggedOnUser(hToken) then
  begin
    try
      //debugmessages.Clear;
      retrieveFoldersFromWinApi;
      //LogDatei.DependentAddStringList(debugmessages,LLEssential);
    finally
      RevertToSelf;
    end;
  end
  else
    LogDatei.DependentAddError('Error: could not impersonate for user by token.', LLError);
end;


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
    if profiledir = '' then
    begin
      LogDatei.DependentAdd('Api Call GetProfilesDirectory failed: '+removeLineBreaks(SysErrorMessage(GetLastError)),LLWarning);
      Reg := TRegistry.Create(KEY_READ);
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      Reg.OpenKey('SOFTWARE\Microsoft\Windows NT\CurrentVersion\ProfileList', False);
      profiledir :=  Reg.ReadString('ProfilesDirectory');
      profiledir := StringReplace1(profiledir,'%SystemDrive%',extractfiledrive(GetWinDirectory));
      specialFolders.Values['Users home'] := profiledir;
      Reg.Free;
    end;

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


function GetShortWinPathName(const longpath:string):string;
var shortpath : PChar;
begin
  shortpath:=StrAlloc(Max_Path);
  jwawindows.GetShortPathName(PChar(longpath), shortpath, Max_Path);
  Result:=string(shortpath);
  StrDispose(shortpath);
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
  uibNTVersion := GetUibNTVersion(errorinfo);
  specialFolders := TStringList.Create;
  debugmessages := TStringList.Create;
  //retrieveFoldersFromHKLM;
  retrieveFoldersFromWinApi;
end.

