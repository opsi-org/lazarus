unit oslindesktopfiles;

{$MODE Delphi}
{$RANGECHECKS ON}
{$OVERFLOWCHECKS ON}
{$IOCHECKS ON}
//{$OBJECTCHECKS ON}
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
  Classes,
  SysUtils,
  strutils,
  IniFiles,
  oslog,
  osfunclin,
  osprocessux,
  osparserhelper,
  ostxstringlist,
  lispecfolder;

const
  // from http://standards.freedesktop.org/menu-spec/menu-spec-1.0.html#category-registry
  categorystrings: array [0..11] of string = (
  'AudioVideo',
  'Audio',
  'Video',
  'Development',
  'Education',
  'Game',
  'Graphics',
  'Network',
  'Office',
  'Settings',
  'System',
  'Utility'
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


type

TuibLinuxDesktopFiles = class (TObject)
  private
    MyFolderpath, MyFoldername, mySubfolder, mySystemfolderName : String;
    MySystemfolder : Integer;
    //MyFiles : TuibFileInstall;
    FolderOpened : Boolean;
    StartIndentLevel : Integer;
  public
    constructor Create;
    destructor Destroy; override;
    (*
    function GetSpecialFolderPath(const desktop: integer;
                                  const Systemfolder : Integer) : String;
//    function ShowShellFolderWindow : Boolean;
    function OpenShellFolderPath (const Systemfolder: Integer; const foldername, iconPath : String) : Boolean;
    function MakeShellLink
      (const description, thePath, commandline_arguments, working_directory,
      iconPath: string; const icon_index: integer): boolean;
      *)
    function OpenShellFolderPath (const Systemfolder: Integer; const subfoldername): Boolean;
    function Tell_SystemFolder (const Systemfolder : Integer) : String;
    function MakeShellLink
          (const name,                    // name showed in menu
                 thePath,                 // path to executable
                 commandline_arguments,   // (opt) Arguments for the executable
                 working_directory,       // (opt) Path in desktop file
                 iconPath,                // (opt) the icon file
                 categories,              // (opt) ; separated list of categories
                 typeOf,                  // default:Application; possible Types are: Application, Link, FSDevice, MimeType,.
                 genericName: String)     // (opt.) description (name=mozilla->generic=browser)
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
                 showOnlyInOld,           // if true it will not apear on a desktop that is freedesktop compliant
                 hidden: boolean)         // if true it will be deleted
          : String;
      function ValidateDesktopFile (const desktopfilename: String) : Boolean;
      function InstallDesktopFile (const desktopfilename: String) : Boolean;
      function getIconSize (const iconfilename: String) : integer;
      function installIcon(iconPath : String) :boolean;
        (*
    function MakeDesktopDir
        (const systemfolderName,
         dirName, description, iconPath: String) : boolean;
    function getRunningDesktop : integer;
    procedure getInstalledDesktops
                 (var desktopList : TDesktopList;
                  var maxDesktopFound : integer);
    function getDesktopName(deskNumber : integer) : string;
    *)
    function DeleteShellLink (const linkname : String) : Boolean;
    function DeleteShellFolder (const SystemFolder : Integer; const foldername: String)
        : Boolean;
end;




implementation

uses
  osfunc;

var
LogS : String;

(*
installDesktopFile()
makeDesktopEntryList()
makeDesktopFile()
  *)

constructor TuibLinuxDesktopFiles.Create;
begin
  inherited create;
  FolderOpened := false;
  MySystemFolder := CSIDL_COMMON_STARTMENU;
  MySystemfolderName := '';
  MyFoldername := 'default';
  MyFolderPath := '/';
  mySubfolder := '';
end;

destructor TuibLinuxDesktopFiles.Destroy;
begin
  inherited destroy;
end;

function TuibLinuxDesktopFiles.Tell_SystemFolder (const Systemfolder : Integer) : String;
begin
  result := 'the ';

  case Systemfolder of
   CSIDL_DESKTOP                   : result := result + 'DESKTOP folder';
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
     result := 'no well known and supported systemfolder'
  end;
end;



function TuibLinuxDesktopFiles.OpenShellFolderPath (const Systemfolder: Integer;
   const subfoldername): Boolean;
var
  oldFolderPath, msg, systemfolderName : String;
begin
  // subfoldername is ignored at Linux
  result := false;
  oldFolderPath := myFolderPath;
  if FolderOpened
  then
    // some folder was already been referred
    LogDatei.LogSIndentLevel :=  LogDatei.LogSIndentLevel - 1;

  if produceNameFromCsidl(systemfolderName,systemfolder,msg) then
  begin
   mySystemFolder := Systemfolder;
   mySystemFolderName := SystemfolderName;
   FolderOpened := true;
   result := true;
  end
  else
  begin
   result := false;
   myFolderPath := oldFolderPath;
   LogS := 'Error ' +msg;
   LogDatei.log (LogS, LLError);
  end;
  LogDatei.LogSIndentLevel :=  LogDatei.LogSIndentLevel + 1;
end;

function TuibLinuxDesktopFiles.getIconSize (const iconfilename: String) : integer;
var
  outlist : TXStringlist;
begin
  result :=  0;
  outlist := RunCommandCaptureOutGetOutlist('file '+ iconfilename);
  if outlist.Count > 0 then
  begin
    if Pos('16 x 16',outlist.Text) > 0 then result := 16;
    if Pos('22 x 22',outlist.Text) > 0 then result := 22;
    if Pos('32 x 32',outlist.Text) > 0 then result := 32;
    if Pos('48 x 48',outlist.Text) > 0 then result := 48;
    if Pos('64 x 64',outlist.Text) > 0 then result := 64;
    if Pos('128 x 128',outlist.Text) > 0 then result := 128;
  end;
end;

function TuibLinuxDesktopFiles.installIcon(iconPath : String) :boolean;
var
  cmd, myicon : string;
  iconsize : integer;
begin
  result := false;
  iconsize := getIconSize(iconPath);
  myicon :=  ChangeFileExt(ExtractFileName(iconPath),'');
  cmd := 'xdg-icon-resource install --novendor --size '+intToStr(iconsize)
         +' '+iconPath+ ' '+myicon;
  if 0 <> RunCommandCaptureOutGetExitcode(cmd) then  LogDatei.log ('Could not install icon file', LLError)
  else result := true;
end;

function TuibLinuxDesktopFiles.InstallDesktopFile (const desktopfilename: String) : Boolean;
begin
  result :=  false;
  if 0 = RunCommandCaptureOutGetExitcode('desktop-file-install '+ desktopfilename) then result :=  true;
  //if 0 = RunCommandCaptureOutGetExitcode('xdg-desktop-menu install --novendor '+ desktopfilename) then result :=  true;
end;


function TuibLinuxDesktopFiles.ValidateDesktopFile (const desktopfilename: String) : Boolean;
begin
  result :=  false;
  if 0 = RunCommandCaptureOutGetExitcode('desktop-file-validate '+ desktopfilename) then result :=  true;
end;


function TuibLinuxDesktopFiles.MakeDesktopFile
    (const dirpath,                 //where to put the file
           linkname,                // name of the desktop file (with ext)
           typeOf,                  // default:Application; possible Types are: Application, Link, FSDevice, MimeType,.
           categories,              // (opt.) ; separated list of categories
           name,                    // name showed in menu
           genericName,             // (opt.) description (name=mozilla->generic=browser)
           thePath,                 // path to executable
           commandline_arguments,
           working_directory,
           iconPath: String;
           showOnlyInOld,           // if true it will not apear on a desktop that is freedesktop compliant
           hidden: boolean)         // if true it will be deleted
    : string;

var
  Filename, myLinkname : String;
  LinkExisted : Boolean;
  desktopFile : TMemIniFile;
  orgcatlist,proofedcatlist : TXstringList;
  category, mydirpath, proofedcategories, mytypeof, myiconfile : string;
  i : integer;
begin
  result := '';
  if linkname = '' then
   //myLinkname :=  ChangeFileExt(ExtractFileName(thePath),'.desktop')
    myLinkname :=  LowerCase(trim(name))+'.desktop'
  else myLinkname := LowerCase(linkname);
  myLinkname := DelChars(myLinkname,#10);// delete any line feed chars
  myLinkname := DelChars(myLinkname,#13); // delete any carriage return (enter) chars
  myLinkname := ReplaceText(myLinkname,'/',' '); //converts slashes to space char
  myLinkname := Tab2Space(myLinkname, 1);// converts tabulator char to one space char
  myLinkname := DelSpace1(myLinkname);// Reduces sequences of space characters to 1 space character
  myLinkname := trim(myLinkname);
  myLinkname := ReplaceText(myLinkname,' ','-'); // filename must not contain whitespaces
  if dirpath = '' then mydirpath := '/tmp'
  else mydirpath := dirpath;
  Filename := mydirpath+'/'+myLinkname;
  if FileExists(Filename) then LinkExisted := true
  else LinkExisted := false;

  orgcatlist := TXStringList.Create;
  proofedcatlist := TXStringList.Create;
  stringsplit(categories, ';', orgcatlist);
  for i := 0 to orgcatlist.Count -1 do
  begin
    category := trim(orgcatlist[i]);
    if strutils.AnsiMatchStr(category, categorystrings) then
      proofedcatlist.Append(category)
    else
      if category <> '' then
        LogDatei.log('Given category: '+category+' is not allowed and will be ignored',LLWarning);
  end;
  if proofedcatlist.Count > 0 then
    begin
      for i := 0 to proofedcatlist.Count - 1 do
        proofedcategories := proofedcategories + proofedcatlist.strings[i] + ';';
      // now the final element without ;
      //proofedcategories := proofedcategories + proofedcatlist.Strings[proofedcatlist.count-1];
    end
  else
    proofedcategories :='';
  if trim(proofedcategories) = '' then proofedcategories :='Utility;';

  desktopFile := TMemIniFile.create(filename);
  desktopFile.WriteString('Desktop Entry','Name',name);
  if thePath <> '' then
   desktopFile.WriteString('Desktop Entry','Exec',thePath+' '+commandline_arguments);
  if genericName <> '' then
   desktopFile.WriteString('Desktop Entry','GenericName',genericName);
  desktopFile.WriteString('Desktop Entry','Icon',iconPath);
  if working_directory <> '' then
   desktopFile.WriteString('Desktop Entry','Path',working_directory);
  mytypeof := typeOf;
  if mytypeOf = '' then mytypeOf := 'Application';
  desktopFile.WriteString('Desktop Entry','Type',mytypeOf);
// possible Types are: Application, Link, FSDevice, MimeType,...
  if showOnlyInOld then
   desktopFile.WriteString('Desktop Entry','ShowOnlyIn','Old');
  if proofedcategories <> '' then
   desktopFile.WriteString('Desktop Entry','Categories',proofedcategories);
  if hidden then
   desktopFile.WriteString('Desktop Entry','Hidden','true')
  else desktopFile.WriteString('Desktop Entry','Hidden','false');
  try
   desktopFile.UpdateFile;
   result := filename;
  except   on e : Exception do
   begin
    result := '';
    LogS := 'Error: '+e.Message;
    LogDatei.log (LogS, LLError);
   end;
  end;
  desktopFile.Free;

  if result <> ''
  then
  begin
    if LinkExisted
    then LogS := ' overwritten'
    else LogS := ' created';
    LogS := 'Deskopfile ' + filename + LogS;
    LogDatei.log (LogS, LLInfo);
  end
  else
  begin
    LogS := 'Deskopfile ' + filename + ' could not be made';
    LogDatei.log (LogS, LLError);
  end;
  orgcatlist.Free;
  proofedcatlist.Free;
end;

function TuibLinuxDesktopFiles.MakeShellLink
          (const name,                    // name showed in menu
                 thePath,                 // path to executable
                 commandline_arguments,   // (opt) Arguments for the executable
                 working_directory,       // (opt) Path in desktop file
                 iconPath,                // (opt) the icon file
                 categories,              // (opt) ; separated list of categories
                 typeOf,                  // default:Application; possible Types are: Application, Link, FSDevice, MimeType,.
                 genericName: String)     // (opt.) description (name=mozilla->generic=browser)
        : Boolean;
var
  dfilename, cmd, createdir : string;
  iconsize : integer;
  myicon : string;
begin
  result :=  false;
  createdir := '';
  if FolderOpened then
  begin
    if MySystemfolder = CSIDL_COMMON_PROGRAMS then createdir := '/tmp'
    else createdir := GetSpecialFolderPath(MySystemfolder);
    LogDatei.log ('Given base directory is: '+Tell_SystemFolder(MySystemfolder), LLNotice);
  end
  else
  begin
    MySystemfolder := CSIDL_COMMON_PROGRAMS;
    createdir := '/tmp';
    LogDatei.log ('No base directory given - using default : COMMON_PROGRAMS', LLNotice);
  end;
  if createdir = '' then LogDatei.log ('The given basedirectory: '+Tell_SystemFolder(MySystemfolder)+' is not supported', LLError);
  myicon := iconPath;
  if FileExists(iconPath) then
  begin
    if installIcon(iconpath) then
      myicon :=  ChangeFileExt(ExtractFileName(iconPath),'');
  end;
  dfilename := MakeDesktopFile(createdir,'', typeOf, categories, name, genericName,
                     thePath, commandline_arguments, working_directory, myicon, false, false);

  if dfilename = '' then LogDatei.log ('Could not create desktop file', LLError)
  else if not ValidateDesktopFile(dfilename) then
  begin
    LogDatei.log ('Could not validate desktop file', LLError);
    if not sysutils.DeleteFile(dfilename) then
      LogDatei.log ('Could not delete invalid desktop file: '+dfilename, LLError)
  end
  else if MySystemfolder = CSIDL_COMMON_PROGRAMS then
    if not InstallDesktopFile(dfilename) then  LogDatei.log ('Could not install desktop file', LLError)
    else result := true;
end;

function TuibLinuxDesktopFiles.DeleteShellLink (const linkname: String)
   : Boolean;
 var
 dfilename, newDir : String;
 runningDesktop : integer;
begin
  result :=  false;
  //if 0 = RunCommandCaptureOutGetExitcode('xdg-desktop-menu uninstall '+ linkname) then result :=  true;
  dfilename := MakeDesktopFile('/tmp','', '', '', linkname, '', '', '', '', '', false,true);

  if dfilename = '' then LogDatei.log ('Could not create desktop file', LLError)
  else if not ValidateDesktopFile(dfilename) then
  begin
    LogDatei.log ('Could not validate desktop file', LLError);
    if not sysutils.DeleteFile(dfilename) then
      LogDatei.log ('Could not delete invalid desktop file: '+dfilename, LLError)
  end
  else if MySystemfolder = CSIDL_COMMON_PROGRAMS then
    if not InstallDesktopFile(dfilename) then  LogDatei.log ('Could not uninstall desktop file', LLError)
    else result := true;
  // try hard
  dfilename := '/usr/share/applications/'+linkname+'.desktop';
  LogDatei.log ('Try to delet desktop file: '+dfilename, LLDebug);
  if not sysutils.DeleteFile(dfilename) then
  begin
    LogDatei.log ('Could not delete desktop file: '+dfilename, LLError);
    result := false;
  end
  else result := true;
  (*
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
  *)
end;

function TuibLinuxDesktopFiles.DeleteShellFolder
   (const SystemFolder: Integer; const foldername: String)
   : Boolean;

  var
  newDir, FolderPath : String;
  runningDesktop : integer;

begin
  result := false;
  (*

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
  *)
end;


end.

