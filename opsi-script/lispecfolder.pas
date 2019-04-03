unit lispecfolder;

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
// credits: http://www.opsi.org/credits/


interface

uses
  //LCLIntf,
  Classes,
  SysUtils,
  osfunclin,
  osprocessux;

var
  specialFolders, debugmessages: TStringList;

procedure retrieveFolders4Linux;


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
function GetDefaultUsersProfilesPath: string;
function GetSpecialFolderPath(const Systemfolder : Integer) : String;
function produceCsidlFromName(const Name: string; var csidl: integer;
  var errormessage: string): boolean;
function produceNameFromCsidl(var Name: string; const csidl: integer;
  var errormessage: string): boolean;
implementation

uses osfunc, osparser;

var
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

  //In this function you can use the constants from shlobj.pas:
    CSIDL_DESKTOP                       = $0000;
    CSIDL_PROGRAMS                      = $0002;
    CSIDL_CONTROLS                      = $0003;
    CSIDL_PRINTERS                      = $0004;
    CSIDL_PERSONAL                      = $0005;
    CSIDL_FAVORITES                     = $0006;
    CSIDL_STARTUP                       = $0007;
    CSIDL_RECENT                        = $0008;
    CSIDL_SENDTO                        = $0009;
    CSIDL_BITBUCKET                     = $000a;
    CSIDL_STARTMENU                     = $000b;
    CSIDL_DESKTOPDIRECTORY              = $0010;
    CSIDL_DRIVES                        = $0011;
    CSIDL_NETWORK                       = $0012;
    CSIDL_NETHOOD                       = $0013;
    CSIDL_FONTS                         = $0014;
    CSIDL_TEMPLATES                     = $0015;
    CSIDL_COMMON_STARTMENU              = $0016;
    CSIDL_COMMON_PROGRAMS               = $0017;
    CSIDL_COMMON_STARTUP                = $0018;
    CSIDL_COMMON_DESKTOPDIRECTORY       = $0019;
    CSIDL_APPDATA                       = $001A;
    CSIDL_PRINTHOOD                     = $001B;
    XDG_COMMON_DIRECTORYFILES_DIR       = $001C;
    XDG_CONFIG_DIRS                     = $001D;
    XDG_CONFIG_DIRS_MERGED              = $001E;



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




procedure retrieveFolders4Linux;
var
  r, s, t, x, profiledir: string;
  charbuf , pstr : pchar;
  i: integer;
  profdirlength : dword;

begin
  try
    // not available at Linux
    specialFolders.Add('Common AppData');
    specialFolders.Values['Common AppData'] := '';

    // not available at Linux
    specialFolders.add('Common Desktop');
    specialFolders.Values['Common Desktop'] :=   '';

    // not available at Linux
    specialFolders.add('Common Start Menu');
    specialFolders.Values['Common Start Menu'] :=  '';

    specialFolders.add('Common Startup');
    specialFolders.Values['Common Startup'] := '/etc/xdg/autostart';

    // not needed : managed by xdg and deskop-file utils
    specialFolders.add('Common Programs');
    specialFolders.Values['Common Programs'] := '/usr/share/applications';

    // not available at Linux
    specialFolders.Add('AppData');
    specialFolders.Values['AppData'] := GetEnvironmentVariable('HOME');

    specialFolders.add('Desktop');
    specialFolders.Values['Desktop'] := RunCommandCaptureOutGetOutlist('xdg-user-dir DESKTOP')[0];

    // not available at Linux
    specialFolders.add('Start Menu');
    specialFolders.Values['Start Menu'] := '';

    specialFolders.add('Startup');
    specialFolders.Values['Startup'] := GetEnvironmentVariable('HOME')+'/.config/autostart/';

    // not needed : managed by xdg and deskop-file utils
    specialFolders.add('Programs');
    specialFolders.Values['Programs'] := '';

    // not available at Linux
    specialFolders.add('SendTo');
    specialFolders.Values['SendTo'] := '';

    specialFolders.add('current profile');
    specialFolders.Values['current profile'] := GetEnvironmentVariable('HOME');

    specialFolders.add('Users home');
    specialFolders.Values['Users home'] := '/home';

    // not available at Linux
    specialFolders.add('Common Profile');
    specialFolders.Values['Common Profile'] := '';

    // not available at Linux
    specialFolders.add('DefaultUserProfile');
    specialFolders.Values['DefaultUserProfile'] := '';

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


function guessCommonProfilesPath: string;
begin
  // Linux ?
  (*
  Result := specialFolders.Values['Users home'];
  if uibNTVersion < tntverWINVISTA then
    Result := Result + '\All Users'
  else
    Result := Result + '\Public';
  *)
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



function GetSpecialFolderPath(const Systemfolder : Integer) : String;
begin
  case Systemfolder of
    CSIDL_DESKTOP: result := GetDesktopPath;
    CSIDL_SENDTO: result := '';
    CSIDL_STARTMENU: result :=  GetStartmenuPath;
    CSIDL_PROGRAMS: result := GetProgramsPath;
    CSIDL_DESKTOPDIRECTORY: result := GetDesktopPath;
    CSIDL_COMMON_STARTMENU: result := GetCommonStartmenuPath;
    CSIDL_COMMON_PROGRAMS: result := GetCommonProgramsPath;
    CSIDL_COMMON_STARTUP: result :=  GetCommonStartupPath;
    CSIDL_COMMON_DESKTOPDIRECTORY: result := GetCommonDesktopPath;
    else result := ''
  end;
end;


{****************************************************** }

begin
  //uibNTVersion := GetUibNTVersion(errorinfo);
  specialFolders := TStringList.Create;
  debugmessages := TStringList.Create;
  //retrieveFoldersFromHKLM;
  //retrieveFoldersFromWinApi;
  // If we do it here, there is no log file !
  //retrieveFolders4Linux;
end.

