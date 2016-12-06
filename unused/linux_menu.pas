unit linux_menu;

interface
{$IFDEF LINUX}

//   Copyright of new material (C) 1997-2005 uib
//   author: Rupert Roeder, detlef oertel
//   The source is underlying the GNU General Public License (GPL):
//
//    This program is free software; you can redistribute it and/or
//    modify it under the terms of the GNU General Public License as
//    published by the Free Software Foundation; either version 2 of
//    the License, or (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
//    General Public License for more details.
//    Text of the GPL: www.uib.de/hupsi/gpl.html
//    German Version: www.uib.de/hupsi/gpl-ger.html
//
//    ************************************************
//
//   This unit implements the tools to handle the menues on linux desktops.
//   There are a lot of differences between the linux and the windows world
//   and there also a lot of differences between the differen linux desktops
//   We tryed to implement here the the standards that are defined by
//   freedesktop.org . The newer Versions of KDE, Gnome, ICEFwm,...
//   are known to be freedesktop.org compliant.
//   For more information look to the winst manuals and to www.freedeskop.org
//
//   The main concept by implementing this standard is not to do it complete
//   but to make the things sysadmins have often to do, so simple as possible
//   and (second) so similar as possible between the windows and the linux world.

uses
SysUtils, Classes, IniFiles, wifunc, StrUtils, xmlpatch;

Const
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

  MAXDESKTOPS=2;  //maximum desktop number
  DESK_FREE=0;    // freedesktop.org
  DESK_KDE =1;
  DESK_GNOME=2;


Type

TDesktopList = array [0..MAXDESKTOPS] of integer;

TuibLinuxDesktopFiles = class (TObject)
  private
    MyFolderpath, MyFoldername, mySubfolder, mySystemfolderName : String;
    MySystemfolder : Integer;
    MyFiles : TuibFileInstall;
    FolderOpened : Boolean;
    StartIndentLevel : Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function GetSpecialFolderPath(const desktop: integer;
                                  const Systemfolder : Integer) : String;
    function Tell_SystemFolder (const Systemfolder : Integer) : String;
//    function ShowShellFolderWindow : Boolean;
    function OpenShellFolderPath (const Systemfolder: Integer; const foldername, iconPath : String) : Boolean;
    function MakeShellLink
          (const linkname,                // name of the desktop file (with ext)
                 typeOf,
                 categories,              // (opt.) ; separated list of categories
                 name,                    // name showed in menu
                 genericName,             // (opt.) description (name=mozilla->generic=browser)
                 thePath,                 // path to executable
                 commandline_arguments,
                 working_directory,
                 iconPath: String)
        : Boolean;

      function MakeDesktopFile
          (const dirpath,                 //where to put the file
                 linkname,                // name of the desktop file (with ext)
                 typeOf,
                 categories,              // (opt.) ; separated list of categories
                 name,                    // name showed in menu
                 genericName,             // (opt.) description (name=mozilla->generic=browser)
                 thePath,                 // path to executable
                 commandline_arguments,
                 working_directory,
                 iconPath: String;
                 showOnlyInOld : boolean) // if true it will not apear on a desktop that is freedesktop compliant
          : Boolean;

    function MakeDesktopDir
        (const systemfolderName,
         dirName, description, iconPath: String) : boolean;
    function getRunningDesktop : integer;
    procedure getInstalledDesktops
                 (var desktopList : TDesktopList;
                  var maxDesktopFound : integer);
    function getDesktopName(deskNumber : integer) : string;
    function DeleteShellLink (const linkname : String) : Boolean;
    function DeleteShellFolder (const SystemFolder : Integer; const foldername: String)
        : Boolean;
end;




{$ENDIF}
implementation
{$IFDEF LINUX}
uses
wimain, wiglob,linux_func, wispecfolder, linux_system;

var
LogS : String;




{ TuibShellLinks }
constructor TuibLinuxDesktopFiles.Create;
begin
  inherited create;
//  CoInitialize (nil);
  FolderOpened := false;
  MySystemFolder := CSIDL_COMMON_STARTMENU;
  MySystemfolderName := '';
  MyFoldername := 'default';
  MyFolderPath := '/';
  mySubfolder := '';
  MyFiles := TuibFileInstall.create;
  StartIndentLevel := LogDatei.LogSIndentLevel;
end;

destructor TuibLinuxDesktopFiles.Destroy;
begin
//  CoUnInitialize;
  LogDatei.LogSIndentLevel :=  StartIndentLevel;
  MyFiles.free;
   // MyMessageDlg.WiMessage ('destroy uibShellLinks ', [mrOK]);
  inherited destroy;
end;

function TuibLinuxDesktopFiles.getRunningDesktop : integer;
var
 winman : string;
begin
 winman := GetEnvironmentVariable('WINDOWMANAGER');
 winman := linDereferenceLink(winman);
 if winman <> '' then
 begin
  if pos('kde',winman)>0 then result := DESK_KDE;
  if pos('gnome',winman)>0 then result := DESK_GNOME;
 end
 else
 begin
  // can't find WINDOWMANAGER by environment
  LogS := 'Error :' +'cant find WINDOWMANAGER by environment';
  LogDatei.DependentAddError (LogS, BaseLevel);
 end;
end;

function TuibLinuxDesktopFiles.getDesktopName(deskNumber : integer) : string;
begin
 result := 'unknown (internal Error)';
 case deskNumber of
  DESK_FREE : result := 'freedesktop.org';
  DESK_KDE : result := 'KDE';
  DESK_GNOME : result := 'GNOME';
 end;
end;

procedure TuibLinuxDesktopFiles.getInstalledDesktops
                 (var desktopList : TDesktopList;
                  var maxDesktopFound : integer);
var
 fetchoutput : TStringList;
begin
 // ever ty the new standard
 desktopList[0] := DESK_FREE;
 maxDesktopFound :=0;
 // looking for kde
 fetchoutput := TStringList.Create;
 linPopen('which kde-config',fetchoutput);
 if FileExists(fetchoutput.Strings[0]) then
 begin
  inc(maxDesktopFound);
  desktopList[maxDesktopFound] := DESK_KDE;
  LogS := 'Info : KDE found.';
  LogDatei.DependentAdd (LogS, BaseLevel);
 end;
 fetchoutput.clear;
 // looking for gnome
 linPopen('which gnomevfs-info',fetchoutput);

 if (fetchoutput.Count > 0) and (FileExists(fetchoutput.Strings[0])) then
 begin
  inc(maxDesktopFound);
  desktopList[maxDesktopFound] := DESK_GNOME;
  LogS := 'Info : GNOME found.';
  LogDatei.DependentAdd (LogS, BaseLevel);
 end;
 fetchoutput.clear;
 fetchoutput.free;
end;

function TuibLinuxDesktopFiles.GetSpecialFolderPath(const desktop: integer;
                                                    const Systemfolder : Integer) : String;
var
 myhome, myBaseUserDir, myBaseAllDir, common_startmenu,
 common_desktopdirectory, myConfigDir : string;
 fetchoutput : TStringList;
begin
 result := '';
 fetchoutput := TStringList.Create;
 myhome := GetEnvironmentVariable('HOME');
 case desktop of
  DESK_FREE:
   begin
    myBaseUserDir := GetEnvironmentVariable('XDG_DATA_HOME');
    if myBaseUserDir = '' then myBaseUserDir := myhome+'/.local/share';
    myBaseAllDir := GetEnvironmentVariable('XDG_DATA_DIRS');
    if myBaseAllDir = '' then myBaseAllDir := '/usr/share';
    myConfigDir := GetEnvironmentVariable('XDG_CONFIG_DIRS');
    if myConfigDir = '' then myConfigDir := '/etc/xdg';
    case Systemfolder of
     CSIDL_STARTUP:                   result := '';
     CSIDL_STARTMENU:                 result := myBaseUserDir+'/applications';
     CSIDL_COMMON_STARTMENU:          result := myBaseAllDir+'/applications';
     XDG_COMMON_DIRECTORYFILES_DIR:   result := myBaseAllDir+'/desktop-directories';
     XDG_CONFIG_DIRS:                 result := myConfigDir;
     XDG_CONFIG_DIRS_MERGED:          result := myConfigDir+'/menus/applications-merged';
    else
     result := '';
    end;
   end;
  DESK_KDE:
   begin
    myBaseUserDir := GetEnvironmentVariable('KDEHOME');
    if myBaseUserDir = '' then myBaseUserDir := myhome+'/.kde';
    fetchoutput.clear;
    linPopen('kde-config --prefix',fetchoutput);
    myBaseAllDir := fetchoutput.Strings[0];
    if not DirectoryExists(myBaseAllDir) then
    begin
     myBaseAllDir := GetEnvironmentVariable('WINDOWMANAGER');
     myBaseAllDir := linDereferenceLink(myBaseAllDir);
     if myBaseAllDir <> '' then myBaseAllDir := copy(myBaseAllDir,1,pos('/bin',myBaseAllDir)-1);
     if myBaseAllDir = '' then myBaseAllDir := '/opt/kde3';
    end;
    fetchoutput.clear;
    linpopen('kde-config --install apps',fetchoutput);
    common_startmenu := fetchoutput.Strings[0];
    common_startmenu := AnsiReplaceStr(common_startmenu,'${prefix}',myBaseAllDir);
    if not DirectoryExists(common_startmenu) then
    begin
     common_startmenu := myBaseAllDir+'/share/applnk';
    end;
    // works and is the rigight place according to FHS
    //common_desktopdirectory := '/etc'+myBaseAllDir+'/share/apps/kdesktop/Desktop';
    // But KDE say: http://people.fruitsalad.org/phil/kde/userguide-tng/customizing-kde.html
    common_desktopdirectory := myBaseAllDir+'/share/apps/kdesktop/Desktop';

    case Systemfolder of
     CSIDL_DESKTOP:                   result := myhome+'/Desktop';
     CSIDL_STARTUP:                   result := myBaseUserDir+'/Autostart';
     CSIDL_STARTMENU:                 result := myBaseUserDir+'/share/applnk';
     CSIDL_DESKTOPDIRECTORY:          result := myhome+'/Desktop';
     CSIDL_COMMON_STARTMENU:          result := common_startmenu;
     CSIDL_COMMON_STARTUP:            result := myBaseAllDir+'/share/autostart';
     CSIDL_COMMON_DESKTOPDIRECTORY:   result := common_desktopdirectory;
    else
     result := '';
    end;
   end;
  DESK_GNOME:
    //applications-all-users:///
    //preferences-all-users:///
    //
    case Systemfolder of
     CSIDL_DESKTOP:                   result := myhome+'/Desktop';
     // CSIDL_STARTUP:                   result := myhome+'.kde/Autostart';
     //CSIDL_STARTMENU:                 result := myhome+'.kde/share/applnk';
     CSIDL_DESKTOPDIRECTORY:          result := myhome+'/Desktop';
     //CSIDL_COMMON_STARTMENU:          result := '/opt/kde/share/applnk';
     //CSIDL_COMMON_PROGRAMS:           result := '/usr/local/bin';
     //CSIDL_COMMON_STARTUP:            result := '/opt/kde3/share/autostart';
     //CSIDL_COMMON_DESKTOPDIRECTORY:   result := '/etc/opt/kde3/share/apps/kdesktop/Desktop';
    else
     result := '';
    end;
 end;
 fetchoutput.free;
end;

function TuibLinuxDesktopFiles.Tell_SystemFolder (const Systemfolder : Integer) : String;
begin
  result := 'the ';

  case Systemfolder of
   CSIDL_DESKTOP                   : result := result + 'DESKTOP folder';
   CSIDL_SENDTO                    : result := result + 'SENDTO folder';
   CSIDL_STARTMENU                 : result := result + 'STARTMENU folder';
   CSIDL_PROGRAMS                  : result := result + 'PROGRAMS folder';
   CSIDL_DESKTOPDIRECTORY          : result := result + 'DESKTOPDIRECTORY folder';
   CSIDL_STARTUP                   : result := result + 'STARTUP folder';
   CSIDL_FAVORITES                 : result := result + 'FAVORITES folder';
   CSIDL_COMMON_STARTMENU          : result := result + 'COMMON STARTMENU folder';
   CSIDL_COMMON_PROGRAMS           : result := result + 'COMMON PROGRAMS folder';
   CSIDL_COMMON_DESKTOPDIRECTORY   : result := result + 'COMMON DESKTOPDIRECTORY folder';
   CSIDL_COMMON_STARTUP            : result := result + 'COMMON STARTUP folder';
   XDG_COMMON_DIRECTORYFILES_DIR   : result := result + 'XDG COMMON DIRECTORYFILES folder';
   XDG_CONFIG_DIRS                 : result := result + 'XDG CONFIGURATION folder';
   XDG_CONFIG_DIRS_MERGED          : result := result + 'XDG MERGED MENU folder';

//   CSIDL_COMMON_FAVORITES          : result := result + 'COMMON FAVORITES folder';
   else
     result := 'no systemfolder'
  end;
end;



function TuibLinuxDesktopFiles.OpenShellFolderPath (const Systemfolder: Integer;
   const foldername, IconPath: String): Boolean;
var
  oldFolderPath, msg, systemfolderName, myIconPath : String;
begin
  result := true;
  oldFolderPath := myFolderPath;
  if FolderOpened
  then
    (* some folder was already been referred *)
    LogDatei.LogSIndentLevel :=  LogDatei.LogSIndentLevel - 1;

  if produceNameFromCsidl(systemfolderName,systemfolder,msg) then
  begin
   MakeDesktopDir(systemfolderName,folderName,'', iconPath);
   mySystemFolder := Systemfolder;
   mySystemFolderName := SystemfolderName;
   mySubfolder := foldername;
  end
  else
  begin
   result := false;
   LogS := 'Error ' +msg;
   LogDatei.DependentAddError (LogS, BaseLevel);
  end;


  if result
  then
  Begin
    MyFoldername := Foldername;
    MySystemFolder := Systemfolder;
    FolderOpened := true;
  End
  else
    myFolderPath := oldFolderPath;

  LogDatei.LogSIndentLevel :=  LogDatei.LogSIndentLevel + 1;
end;


function TuibLinuxDesktopFiles.MakeDesktopDir
        (const systemfolderName,
         dirName, description, iconPath: String) :boolean;
var
 newDir, msg, name, myIconPath : String;
 showOnlyInOld : boolean;
 systemfolder : integer;
 desktopList : TDesktopList;
 maxDesktopFound, i : integer;
 stringlist : TStringlist;

begin
 maxDesktopFound := -1;
 stringlist := TStringlist.create;
 if name = '' then
  name := copy(dirname,pos('-',dirname)+1,length(dirname));
 if iconPath = '' then myIconPath := 'folder'
 else myIconPath := iconPath;
 if produceCsidlFromName(systemfoldername,systemfolder,msg) then
 begin
  getInstalledDesktops(desktopList,maxDesktopFound);
  // loop over all found desktops
  for i :=0 to maxDesktopFound do
  begin
   if desktopList[i] = DESK_FREE then
    showOnlyInOld := false
   else
    showOnlyInOld := true;
   case desktopList[i] of
    DESK_FREE:  begin
                 newDir := GetSpecialFolderPath(desktopList[i],XDG_COMMON_DIRECTORYFILES_DIR);
                 if newDir <> '' then
                 begin
                  if DirectoryExists(newDir) then
                  begin
                   LogS := 'Direcory '+newDir+' existed';
                   LogDatei.DependentAdd (LogS, LevelDebug);
                  end
                  else
                  begin
                    try
                       ForceDirectories(newDir);
                       LogS := 'Created '+ Tell_SystemFolder (XDG_COMMON_DIRECTORYFILES_DIR);
                       LogDatei.DependentAdd (LogS, LevelComplete);
                     except
                       result := false;
                       LogS := 'Error: "' + dirname + '"  could not be created in ' + Tell_SystemFolder (XDG_COMMON_DIRECTORYFILES_DIR);
                       LogDatei.DependentAddError (LogS, BaseLevel);
                     end;
                  end;
                   try
                    // creating the directory file
                    result := MakeDesktopFile(newDir,
                               dirname+'.directory','Directory','',name,'','','','',
                               myIconPath,showOnlyInOld);
                    if result then
                    begin
                     LogS := 'Info: Directoryfile:"' + dirname + '.directory" created in ' + Tell_SystemFolder (XDG_COMMON_DIRECTORYFILES_DIR);
                     LogDatei.DependentAddError (LogS, LevelInfo);
                    end
                    else
                    begin
                     LogS := 'Error: Directoryfile:"' + dirname + '.directory"  could not be created in ' + Tell_SystemFolder (XDG_COMMON_DIRECTORYFILES_DIR);
                     LogDatei.DependentAddError (LogS, BaseLevel);
                    end;
                   except
                    result := false;
                    LogS := 'Error: Directoryfile:"' + dirname + '.directory"  could not be created in ' + Tell_SystemFolder (XDG_COMMON_DIRECTORYFILES_DIR);
                    LogDatei.DependentAddError (LogS, BaseLevel);
                   end;
                   try
                    // where to create merged menu file
                    newDir := GetSpecialFolderPath(desktopList[i],XDG_CONFIG_DIRS_MERGED);
                    if newDir <> '' then
                    begin
                     if DirectoryExists(newDir) then
                     begin
                      LogS := 'Info: Direcory '+newDir+' existed';
                      LogDatei.DependentAdd (LogS, LevelDebug);
                     end
                     else
                     begin
                       try
                          ForceDirectories(newDir);
                          LogS := 'Info: Created '+ Tell_SystemFolder (XDG_CONFIG_DIRS_MERGED);
                          LogDatei.DependentAdd (LogS, LevelComplete);
                        except
                          result := false;
                          LogS := 'Error: "' + newDir + '"  could not be created';
                          LogDatei.DependentAddError (LogS, BaseLevel);
                        end;
                     end;
                     // now creating merged menu file
                     if not FileExists(newDir+'/'+dirname+'.menu') then
                     begin
                      try
                       stringlist.add('<!DOCTYPE Menu PUBLIC "-//freedesktop//DTD Menu 0.8//EN"');
                       stringlist.add('   "http://www.freedesktop.org/standards/menu-spec/menu-0.8.dtd">');
                       stringlist.add('  <Menu/>');
                       createXMLFile(newDir+'/'+dirname+'.menu',stringlist);
                       LogS := 'Info: merged menu file:' + newDir+'/'+dirname+'.menu created in ' + Tell_SystemFolder (XDG_CONFIG_DIRS_MERGED);
                       LogDatei.DependentAddError (LogS, BaseLevel);
                      except
                       result := false;
                       LogS := 'Error: merged menu file:' + newDir+'/'+dirname+'.menu  could not be created in ' + Tell_SystemFolder (XDG_CONFIG_DIRS_MERGED);
                       LogDatei.DependentAddError (LogS, BaseLevel);
                      end;
                     end
                     else openXMLFile(newDir+'/'+dirname+'.menu');
                     openNode('Menu =""/Name ');
                     setNodeText(name);
                     saveXMLFile(newDir+'/'+dirname+'.menu');
                     openNode('Menu =""/Directory ');
                     setNodeText(dirname+'.directory');
                     saveXMLFile(newDir+'/'+dirname+'.menu');
                    end
                    else
                    begin
                     LogS := 'Warning: '+Tell_SystemFolder(XDG_CONFIG_DIRS_MERGED)+' is not defined by the '
                            + getDesktopName(desktopList[i])+' standard';
                     result := false;
                     LogDatei.DependentAdd(LogS, BaseLevel);
                    end;
                   except
                    LogS := 'Error: Creating the merged menu file failed';
                    result := false;
                    LogDatei.DependentAdd(LogS, BaseLevel);
                   end;
                 end
                 else
                 begin
                  LogS := 'Warning: '+Tell_SystemFolder(XDG_COMMON_DIRECTORYFILES_DIR)+' is not defined by the '
                          + getDesktopName(desktopList[i])+' standard';
                  result := false;
                  LogDatei.DependentAdd(LogS, BaseLevel);
                 end;
                 ProcessMess;
                end;
    DESK_KDE:   begin
                 newDir := GetSpecialFolderPath(desktopList[i],systemfolder);
                 if newDir <> '' then
                 begin
                  newDir := newDir+'/'+dirname;
                  if DirectoryExists(newDir) then
                   LogS := 'Direcory '+newDir+' existed'
                  else
                  begin
                    try
                       ForceDirectories(newDir);
                       LogS := 'Created "' + dirname + '" in ' + Tell_SystemFolder (SystemFolder);
                       LogDatei.DependentAdd (LogS, LevelComplete);
                       result := MakeDesktopFile(newDir,
                               '.directory','Directory','',name,'','','','',
                               myIconPath,showOnlyInOld);
                     except
                       result := false;
                       LogS := 'Error: "' + dirname + '"  could not be created in ' + Tell_SystemFolder (SystemFolder);
                       LogDatei.DependentAddError (LogS, BaseLevel);
                     end;
                  end;
                 end
                 else
                 begin
                  LogS := 'Warning: '+SystemfolderName+' is not defined by the '
                          + getDesktopName(desktopList[i])+' standard';
                  result := false;
                  LogDatei.DependentAdd(LogS, BaseLevel);
                 end;
                 ProcessMess;
                end;
    DESK_GNOME: begin
                end;
   end;
  end; //for loop over desktops
 end
 else
 begin // no systemfolder recognized
  LogS := msg;
  LogDatei.DependentAddError (LogS, BaseLevel);
 end;
end;


function TuibLinuxDesktopFiles.MakeDesktopFile
    (const dirpath,                 //where to put the file
           linkname,                // name of the desktop file (with ext)
           typeOf,
           categories,              // (opt.) ; separated list of categories
           name,                    // name showed in menu
           genericName,             // (opt.) description (name=mozilla->generic=browser)
           thePath,                 // path to executable
           commandline_arguments,
           working_directory,
           iconPath: String;
           showOnlyInOld : boolean) // if true it will not apear on a desktop that is freedesktop compliant
    : Boolean;

var
  Filename, myLinkname : String;
  LinkExisted : Boolean;
  desktopFile : TMemIniFile;
begin
  if linkname = '' then
   myLinkname :=  ChangeFileExt(ExtractFileName(thePath),'.desktop')
  else myLinkname := linkname;
  Filename := dirpath+'/'+myLinkname;
  if FileExists(Filename) then LinkExisted := true
  else LinkExisted := false;
  result := false;

  desktopFile := TMemIniFile.create(filename);
  desktopFile.WriteString('Desktop Entry','Name',name);
  if thePath <> '' then
   desktopFile.WriteString('Desktop Entry','Exec',thePath+' '+commandline_arguments);
  if genericName <> '' then
   desktopFile.WriteString('Desktop Entry','GenericName',genericName);
  desktopFile.WriteString('Desktop Entry','Icon',iconPath);
  if working_directory <> '' then
   desktopFile.WriteString('Desktop Entry','Path',working_directory);
  desktopFile.WriteString('Desktop Entry','Type',typeOf);
// possible Types are: Application, Link, FSDevice, MimeType,...
  if showOnlyInOld then
   desktopFile.WriteString('Desktop Entry','ShowOnlyIn','Old');
  if categories <> '' then
   desktopFile.WriteString('Desktop Entry','Categories',categories);
//  desktopFile.WriteString('Desktop Entry','',);
//  desktopFile.WriteString('Desktop Entry','',);
//  desktopFile.WriteString('Desktop Entry','',);
//  desktopFile.WriteString('Desktop Entry','',);
  try
   desktopFile.UpdateFile;
   result := true;
  except   on e : Exception do
   begin
    result := false;
    LogS := 'Error: '+e.Message;
    LogDatei.DependentAddError (LogS, BaseLevel);
   end;
  end;
  desktopFile.Free;



  if result
  then
  begin
    if LinkExisted
    then LogS := ' overwritten'
    else LogS := ' created';
    LogS := 'Deskopfile ' + filename + LogS;
    LogDatei.DependentAdd (LogS, LevelComplete);
  end
  else
  begin
//    LogS := LogS + ' System message "' + syserrormessage(winresult) + '"';
//    LogDatei.DependentAdd (LogS, BaseLevel);
    LogS := 'Deskopfile ' + filename + ' could not be made';
    LogDatei.DependentAddError (LogS, BaseLevel);
  end;

end;

function TuibLinuxDesktopFiles.MakeShellLink
          (const linkname,                // name of the desktop file (with ext)
                 typeOf,
                 categories,              // (opt.) ; separated list of categories
                 name,                    // name showed in menu
                 genericName,             // (opt.) description (name=mozilla->generic=browser)
                 thePath,                 // path to executable
                 commandline_arguments,
                 working_directory,
                 iconPath: String)
        : Boolean;
var
 newDir : String;
 showOnlyInOld : boolean;
 desktopList : TDesktopList;
 maxDesktopFound, i : integer;

begin
 maxDesktopFound := -1;
 getInstalledDesktops(desktopList,maxDesktopFound);
 // loop over all found desktops
 for i :=0 to maxDesktopFound do
 begin
  if desktopList[i] = DESK_FREE then
   showOnlyInOld := false
  else
   showOnlyInOld := true;
  case desktopList[i] of
    DESK_FREE:  begin
                 newDir := GetSpecialFolderPath(desktopList[i],mySystemfolder);
                 if newDir <> '' then
                 begin
                  if DirectoryExists(newDir) then
                  begin
                   result := MakeDesktopFile(newDir,
                                             linkname,
                                             typeOf,
                                             categories,
                                             name,
                                             genericName,
                                             thePath,
                                             commandline_arguments,
                                             working_directory,
                                             iconPath,
                                             showOnlyInOld);
                   try
                    // where to create merged menu file
                    newDir := GetSpecialFolderPath(desktopList[i],XDG_CONFIG_DIRS_MERGED);
                    if newDir <> '' then
                    begin
                     if FileExists(newDir+'/'+mySubfolder+'.menu') then
                     begin
                      openXMLFile(newDir+'/'+mySubfolder+'.menu');
                      openNode('Menu =""/Include ');
                      setNodeAtPosition('Filename','','',-1);
                      setNodeText(linkname);
                      saveXMLFile(newDir+'/'+mySubfolder+'.menu');
                      LogS := 'Info: '+linkname+' added to merged menu file: '
                               +newDir+'/'+mySubfolder+'.menu';
                      LogDatei.DependentAdd(LogS, LevelComplete);
                     end
                     else
                     begin
                      LogS := 'Error: merged menu file: '
                               +newDir+'/'+mySubfolder+'.menu'
                               +' not found';
                      result := false;
                      LogDatei.DependentAdd(LogS, BaseLevel);
                     end;
                    end
                    else
                    begin
                     LogS := 'Warning: '+mySystemfolderName+' is not defined by the '
                            + getDesktopName(desktopList[i])+' standard';
                     result := false;
                     LogDatei.DependentAdd(LogS, BaseLevel);
                    end;
                   except
                    LogS := 'Error: Editing the merged menu file failed';
                    result := false;
                    LogDatei.DependentAdd(LogS, BaseLevel);
                   end;
                  end
                  else
                  begin
                   result := false;
                   LogS := 'Error: Directory "' + newDir + '"  doesnt exist (internal Error)';
                   LogDatei.DependentAddError (LogS, BaseLevel);
                  end;
                 end
                 else
                 begin
                  LogS := 'Warning: '+mySystemfolderName+' is not defined by the '
                          + getDesktopName(desktopList[i])+' standard';
                  result := false;
                  LogDatei.DependentAdd(LogS, BaseLevel);
                 end;
                end;
    DESK_KDE:   begin
                 newDir := GetSpecialFolderPath(desktopList[i],mySystemfolder);
                 if newDir <> '' then
                 begin
                  if DirectoryExists(newDir) then
                  begin
                   result := MakeDesktopFile(newDir,
                                             linkname,
                                             typeOf,
                                             categories,
                                             name,
                                             genericName,
                                             thePath,
                                             commandline_arguments,
                                             working_directory,
                                             iconPath,
                                             showOnlyInOld);
                  end
                  else
                  begin
                   result := false;
                   LogS := 'Error: Directory "' + newDir + '"  doesnt exist (internal Error)';
                   LogDatei.DependentAddError (LogS, BaseLevel);
                  end;
                 end
                 else
                 begin
                  LogS := 'Warning: '+mySystemfolderName+' is not defined by the '
                          + getDesktopName(desktopList[i])+' standard';
                  result := false;
                  LogDatei.DependentAdd(LogS, BaseLevel);
                 end;
                end;
    DESK_GNOME: begin
                end;
  end;
 end; // loop over desktops
end;


{$IFDEF notready}
function TuibLinuxDesktopFiles.ShowShellFolderWindow: Boolean;
 var
 report : String;
begin
  result := false;
  if not FolderOpened
  then
  Begin
    LogS := 'Error: no folder opened ';
    LogDatei.DependentAddError (LogS, BaseLevel);
  End
  else
  Begin
    if not StartProcess ('explorer.exe "' + MyFolderPath + '"', sw_Show, false, false, false,'', 0, Report)
    then
    Begin
      LogS := 'Error: ' + Report;
      LogDatei.DependentAddError (LogS, BaseLevel);
    End
    else
    Begin
      LogS := 'Show FolderWindow "' + MyFolderPath + '"';
      LogDatei.DependentAdd (LogS, LevelComplete);
      result := true;
    End
  End;
end;
{$ENDIF }

function TuibLinuxDesktopFiles.DeleteShellLink (const linkname: String)
   : Boolean;
 var
 Filename, newDir : String;
 runningDesktop : integer;
begin
  if not FolderOpened
  then
  Begin
    LogS := 'Error: no folder opened ';
    LogDatei.DependentAddError (LogS, BaseLevel);
    result := false;
  End
  else
  Begin
   result := true;
   //first: doit for the freedesktop standard (if possible)
   newDir := GetSpecialFolderPath(DESK_FREE,mySystemfolder);
   if newDir <> '' then
   begin
    newDir := newDir+'/'+mySubfolder;
    if DirectoryExists(newDir) then
    begin
     filename := newDir+'/'+linkname;
     if not sysutils.FileExists (Filename)
     then
     Begin
      LogS := 'Info: Link ' + Linkname + ' does not exist';
      LogDatei.DependentAdd (LogS, LevelInfo);
      LogDatei.NumberOfHints := LogDatei.NumberOfHints + 1;
      result := false;
     End;

     if result then
     Begin
       if DeleteFile (Filename)
       then
       Begin
         LogS := 'Link ' + Linkname +  ' deleted';
         LogDatei.DependentAdd (LogS, LevelComplete);
       End
       else
       Begin
         LogS := 'Error: ' + Linkname +  ' could not be deleted, Errorno. '
                 + IntToStr (GetLastError) + ' ("' + SysErrorMessage (GetLastError) + '")';
         LogDatei.DependentAddError (LogS, BaseLevel);
         result := false;
       End;
     End;
    end
    else
    begin
     result := false;
     LogS := 'Error: Directory "' + newDir + '"  doesnt exist (internal Error)';
     LogDatei.DependentAddError (LogS, BaseLevel);
    end;
   end
   else
   begin
    LogS := 'Warning: '+mySystemfolderName+' is not defined by the freedesktop.org standard';
    result := false;
    LogDatei.DependentAdd(LogS, BaseLevel);
   end;

    //second: doit in oldstyle for the running desktop
   runningDesktop := getRunningDesktop;
   newDir := GetSpecialFolderPath(runningDesktop,mySystemfolder);
   if newDir <> '' then
   begin
    newDir := newDir+'/'+mySubfolder;
    if DirectoryExists(newDir) then
    begin
     filename := newDir+'/'+linkname;
     if not sysutils.FileExists (Filename)
     then
     Begin
      LogS := 'Info: Link ' + Linkname + ' does not exist';
      LogDatei.DependentAdd (LogS, LevelInfo);
      LogDatei.NumberOfHints := LogDatei.NumberOfHints + 1;
      result := false;
     End;

     if result then
     Begin
       if DeleteFile (Filename)
       then
       Begin
         LogS := 'Link ' + Linkname +  ' deleted';
         LogDatei.DependentAdd (LogS, LevelComplete);
       End
       else
       Begin
         LogS := 'Error: ' + Linkname +  ' could not be deleted, Errorno. '
                 + IntToStr (GetLastError) + ' ("' + SysErrorMessage (GetLastError) + '")';
         LogDatei.DependentAddError (LogS, BaseLevel);
         result := false;
       End;
     End;
    end
    else
    begin
     result := false;
     LogS := 'Error: Directory "' + newDir + '"  doesnt exist (internal Error)';
     LogDatei.DependentAddError (LogS, BaseLevel);
    end;
   end
   else
   begin
    LogS := 'Warning: '+mySystemfolderName+' is not defined by the running desktop standard';
    result := false;
    LogDatei.DependentAdd(LogS, BaseLevel);
   end;
  End;
end;

function TuibLinuxDesktopFiles.DeleteShellFolder
   (const SystemFolder: Integer; const foldername: String)
   : Boolean;

  var
  newDir, FolderPath : String;
  runningDesktop : integer;

begin
  result := false;

  //first: doit for the freedesktop standard (if possible)
  newDir := GetSpecialFolderPath(DESK_FREE,mySystemfolder);
  if newDir = '' then
  begin
   LogS := 'Warning: '+mySystemfolderName+' is not defined by the freedesktop.org standard';
   result := false;
   LogDatei.DependentAdd(LogS, BaseLevel);
  end
  else
  begin
    FolderPath := newDir + '/' + foldername;
    if not DirectoryExists (FolderPath)
    then
    Begin
      LogS := 'Folder "' + foldername + '" in ' + Tell_SystemFolder (SystemFolder) + ' does not exist, nothing to delete';
      LogDatei.DependentAdd (LogS, LevelComplete);
      result := true;
    End
    else
    Begin
      LogS := 'Delete folder "' + foldername + '" in ' + Tell_SystemFolder (SystemFolder);
      LogDatei.DependentAdd (LogS, LevelComplete);

      LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

      if MyFiles.AllDelete (FolderPath, true, true, 0)
      then
      Begin
         result := true;
         if FolderOpened and (FolderPath = MyFolderPath)
         then FolderOpened := false;
      End;
      LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
    End;
  end;

  //second: doit in oldstyle for the running desktop
  runningDesktop := getRunningDesktop;
  newDir := GetSpecialFolderPath(runningDesktop,mySystemfolder);
  if newDir = '' then
  begin
   LogS := 'Warning: '+mySystemfolderName+' is not defined by the running desktop standard';
   result := false;
   LogDatei.DependentAdd(LogS, BaseLevel);
  end
  else
  begin
    FolderPath := newDir + '/' + foldername;
    if not DirectoryExists (FolderPath)
    then
    Begin
      LogS := 'Folder "' + foldername + '" in ' + Tell_SystemFolder (SystemFolder) + ' does not exist, nothing to delete';
      LogDatei.DependentAdd (LogS, LevelComplete);
      result := true;
    End
    else
    Begin
      LogS := 'Delete folder "' + foldername + '" in ' + Tell_SystemFolder (SystemFolder);
      LogDatei.DependentAdd (LogS, LevelComplete);

      LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

      if MyFiles.AllDelete (FolderPath, true, true, 0)
      then
      Begin
         result := true;
         if FolderOpened and (FolderPath = MyFolderPath)
         then FolderOpened := false;
      End;
      LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
    End;
  end;
end;



{$ENDIF}
end.
