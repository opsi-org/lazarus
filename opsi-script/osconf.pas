unit osconf;

// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: Rupert Roeder, detlef oertel
// credits: http://www.opsi.org/credits/

//***************************************************************************
// Subversion:
// $Revision: 512 $
// $Author: oertel $
// $Date: 2016-10-28 12:56:36 +0200 (Fr, 28 Okt 2016) $
//***************************************************************************


{$MODE DELPHI}
//{$RANGECHECKS ON}
{$OVERFLOWCHECKS ON}
{$IOCHECKS ON}
{$OBJECTCHECKS ON}
{$VARSTRINGCHECKS ON}
{$LONGSTRINGS ON}


interface
uses
  Classes
  , SysUtils
{$IFDEF GUI}
  , Forms
{$ENDIF GUI}
{$IFDEF LINUX}
  , fileinfo
  //, winpeimagereader {need this for reading exe info}
  , elfreader, // {needed for reading ELF executables}
{$ENDIF LINUX}
{$IFDEF WINDOWS}
 , VersionInfoX
  , Windows
  , registry,
{$ENDIF WINDOWS}
  inifiles,
  lazfileutils;


function readConfig: boolean;
function writeConfig: boolean;
procedure readConfigFromService;

const
{$IFDEF WINDOWS}
selfProductName = 'opsi-script';
{$ELSE}
selfProductName = 'opsi-script';
{$ENDIF WINDOWS}
ParamFilename = 'winst_p.ini';

valueEnvVarNotFound = '/-!!- not found -!!-/';


opsiorgKey = '\SOFTWARE\opsi.org';
opsiorgShareInfoKey = opsiorgKey + '\shareinfo';
opsiorgGeneralKey = opsiorgKey + '\general';


//deprecated stuff start:
opsiorgSyslogDKey = opsiorgKey + '\syslogd';
Profildateienverzeichnis: string = 'p:\pcpatch';
PathNamsInfoFilename: string = 'p:\pcpatch\pathnams.ini';
ProdukteInfoFilename: string = 'p:\utils\produkte.txt';
SaveddeWithProgmanFilename = 'ProgGrps.ins';
veryOldWinstRegKey = 'SOFTWARE\Hupsi\winst';

//deprecated stuff end


opsiservicePORThttps = 4447;


  debug: boolean = False;
  maxvisualmemostrings = 1000;
  {$IFDEF WINDOWS}
  ParamDelim = '/';
  opsiscriptconf = '';
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
  ParamDelim = '-';
  opsiscriptconf = '/etc/opsi-client-agent/opsi-script.conf';
  {$ENDIF LINUX}

  WinstRegHive = 'HKLM';
  WinstRegKey: string = 'SOFTWARE\opsi.org\winst';

  WinstRegFinalShutdownVar = 'ShutdownRequested';
  WinstRegRebootVar = 'RebootRequested';
  WinstRegContinueVar = 'ContinueLogFile';
  WinstRegLastLogfile = 'LastLogFilename';
  WinstRegNumberOfErrors = 'NumberOfErrors';

  WinstSendToLog = 'SendLogToService';

  RegNoCallLogout = 0;
  RegCallLogout = 2;
  RegCallReboot = 4;
  RegNoCallShutdown = 0;
  RegCallShutdown = 1;

  var
  computername: string;
  opsiserviceURL: string;
  opsiserviceUser: string;
  opsiservicePassword: string;
  opsiserviceSessionId: string;
  depoturl: string;
  depotshare, depotdir: string;
  depotdrive, depotdrive_old: string;
  TempPath: string;
  LogPath: string;
  TmpPathFromLogdatei: string;
  {$IFDEF WINDOWS}
  vi: TVersionInfo;
  regist : TRegistry;
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
  FileVerInfo: TFileVersionInfo;
  {$ENDIF LINUX}
  WinstVersion: string;
  WinstVersionName: string;
  readconfig_done : boolean = false;

  //deprecated stuff start:
  configurl: string;
  configshare, configdir: string;
  utilsurl: string;
  utilsshare, utilsdir: string;
  utilsdrive: string;
  configdrive: string;
  //deprecated stuff end
  debug_prog : boolean = false;
  debug_lib: boolean = false;


implementation
uses
{$IFDEF WINDOWS}osfunc {$ENDIF}
{$IFDEF LINUX}osfunclin {$ENDIF};

function writeConfig: boolean;
var
  myconf : TIniFile;
begin
  if not FileExists(opsiscriptconf) then
  begin
    // prepare to create it
    ForceDirectory(ExtractFilePath(opsiscriptconf));
  end;
  myconf := TIniFile.Create(opsiscriptconf);
  myconf.WriteString('global','debug_prog',BoolToStr(debug_prog,true));
  myconf.WriteString('global','debug_lib',BoolToStr(debug_lib,true));
  myconf.Free;
end;

function readConfig: boolean;

const
  shareDelim = '\\';

var
  part1, part2: string;
  myconf : TIniFile;

begin
  if not FileExists(opsiscriptconf) then
  begin
    // prepare to create it
    ForceDirectory(ExtractFilePath(opsiscriptconf));
  end;
  myconf := TIniFile.Create(opsiscriptconf);
  debug_prog := strToBool(myconf.ReadString('global','debug_prog',boolToStr(debug_prog,true)));
  debug_lib := strToBool(myconf.ReadString('global','debug_lib',boolToStr(debug_lib,true)));
  myconf.Free;


{$IFDEF LINUX} computername := getHostnameLin; {$ENDIF LINUX}
//{$IFDEF LINUX} computername := ''; {$ENDIF LINUX}//
  Result := True;
  depoturl := '';
  //configurl := '';
  //utilsurl := '';

  depotshare := '';
  //configshare := '';
  //utilsshare := '';

  //depotdir := PathDelim + 'install';
  depotdir := PathDelim;
  //configdir := PathDelim + 'pcpatch';
  //utilsdir := PathDelim + 'utils';

  //utilsdrive := 'p:';
  //configdrive := 'p:';

  {$IFDEF WINDOWS} depotdrive := 'p:'; {$ENDIF WINDOWS}
  {$IFDEF LINUX} depotdrive_old := '/mnt'; {$ENDIF LINUX}
  {$IFDEF LINUX} depotdrive := '/media/opsi_depot'; {$ENDIF LINUX}

{$IFDEF WINDOWS}
  try
    regist := TRegistry.Create;
    regist.RootKey := HKEY_LOCAL_MACHINE;

    Result := regist.openKey(opsiorgshareinfokey, False);

    if Result then
    begin
      depoturl := regist.ReadString('depoturl');
      if (depoturl = '') then
        Result := False
      else
      begin
        depoturl := StringReplace(depoturl, '/', '\');
        depotdrive := regist.ReadString('depotdrive');
        DivideAtFirst('\\', depoturl, part1, part2);
        DivideAtFirst('\', part2, part1, part2);
        depotshare := '\\' + part1;
        DivideAtFirst('\', part2, part1, part2);
        depotshare := depotshare + '\' + part1;
        depotdir := '\' + part2;
        readconfig_done := true;
      end;
    end;

    regist.CloseKey;
    regist.Free;

  except
    Result := False;
    readconfig_done := false;
  end;
{$ENDIF}
    readconfig_done := true;
end;

procedure readConfigFromService;
begin
  // not implemented yet
end;



initialization
{$IFDEF WINDOWS}
vi := TVersionInfo.Create(Application.ExeName);
WinstVersion := vi.getString('FileVersion');
vi.Free;
{$ELSE}
//from http://wiki.freepascal.org/Show_Application_Title,_Version,_and_Company
FileVerInfo:=TFileVersionInfo.Create(nil);
try
  FileVerInfo.FileName:=paramstr(0);
  FileVerInfo.ReadFileInfo;
  WinstVersion := FileVerInfo.VersionStrings.Values['FileVersion'];
  (*
  writeln('Company: ',FileVerInfo.VersionStrings.Values['CompanyName']);
  writeln('File description: ',FileVerInfo.VersionStrings.Values['FileDescription']);
  writeln('File version: ',FileVerInfo.VersionStrings.Values['FileVersion']);
  writeln('Internal name: ',FileVerInfo.VersionStrings.Values['InternalName']);
  writeln('Legal copyright: ',FileVerInfo.VersionStrings.Values['LegalCopyright']);
  writeln('Original filename: ',FileVerInfo.VersionStrings.Values['OriginalFilename']);
  writeln('Product name: ',FileVerInfo.VersionStrings.Values['ProductName']);
  writeln('Product version: ',FileVerInfo.VersionStrings.Values['ProductVersion']);
  *)
finally
  FileVerInfo.Free;
end;
//WinstVersion := '4.11.6.1';
{$ENDIF WINDOWS}

WinstVersionName := 'Version ' + winstVersion ;



end.


