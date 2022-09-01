unit osconf;

// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: Rupert Roeder, detlef oertel


{$MODE DELPHI}
//{$RANGECHECKS ON}
{$OVERFLOWCHECKS ON}
{$IOCHECKS ON}
{$OBJECTCHECKS ON}
{$VARSTRINGCHECKS ON}
{$LONGSTRINGS ON}


interface

uses
  Classes,
  SysUtils,
  fileinfo,
{$IFDEF GUI}
  Forms,
{$ENDIF GUI}
{$IFDEF UNIX}
  //fileinfo,
  //, winpeimagereader {need this for reading exe info}
  elfreader, // {needed for reading ELF executables}
{$ENDIF UNIX}
  {$IFDEF DARWIN}
  machoreader, // {needed for reading mac executables}
  {$ENDIF DARWIN}
{$IFDEF WINDOWS}
  winpeimagereader, {need this for reading exe info}
  //VersionInfoX,
  Windows,
  registry,
{$ENDIF WINDOWS}
  inifiles,
  lazfileutils,
  osparserhelper,
  process,
  osencoding;

function readConfig: boolean;
function writeConfig: boolean;
function readConfigFromService: string;

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
  opsiscriptconfinit = 'opsi-script.conf';
  {$ENDIF }
  {$IFDEF UNIX}
  ParamDelim = '-';
  opsiscriptconfinit = '/etc/opsi-script/opsi-script.conf';
  opsiclientagentconf = '/etc/opsi-client-agent/opsi-client-agent.conf';
  {$ENDIF }

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
  opsiserviceClientId: string;
  opsiscriptProcName: string;
  depoturl: string;
  depotshare, depotdir: string;
  depotdrive, depotdrive_old: string;
  depotpath: string;
  TempPath: string;
  LogPath: string;
  TmpPathFromLogdatei: string;
  {$IFDEF WINDOWS}
  //vi: TVersionInfo;
  regist: TRegistry;
  {$ENDIF WINDOWS}
  {$IFDEF UNIX}
  //FileVerInfo: TFileVersionInfo;
  {$ENDIF LINUX}
  FileVerInfo: TFileVersionInfo;
  OpsiscriptVersion: string;
  OpsiscriptVersionName: string;
  OsFileName: string;
  readconfig_done: boolean = False;

  //deprecated stuff start:
  configurl: string;
  configshare, configdir: string;
  utilsurl: string;
  utilsshare, utilsdir: string;
  utilsdrive: string;
  configdrive: string;
  //deprecated stuff end

  {$IFDEF OSDEBUG}
  debug_prog: boolean = True;
  default_loglevel: integer = 8;
  debug_lib: boolean = True;
  force_min_loglevel: integer = 8;
  {$ELSE}
  debug_prog: boolean = False;
  default_loglevel: integer = 7;
  debug_lib: boolean = True;
  force_min_loglevel: integer = 4;
  {$ENDIF}
  //debug_prog: boolean = True;
  //debug_lib: boolean = False;
  //debug_lib: boolean = True;
  opsiscriptconf: string;
  ScriptErrorMessages: boolean = False;
  AutoActivityDisplay: boolean = False;
  w10BitlockerSuspendOnReboot: boolean = False;
  configReverseProductOrderByUninstall: boolean = False;
  configSupressSystemEncodingWarning: boolean = False;
  log_rotation_count: integer = 8;
  configWriteProductLogFile: boolean = False;
  configTestSyntax: boolean = False;


implementation

uses
  {$IFDEF WINDOWS}osfunc,{$ENDIF}
  {$IFDEF UNIX}osfunclin,{$ENDIF}
  oswebservice,
  osjson,
  osmain;

function writeConfig: boolean;
var
  myconf: TIniFile;
begin
  try
    Result := True;
    if not FileExists(opsiscriptconf) then
    begin
      // prepare to create it
      ForceDirectory(ExtractFilePath(opsiscriptconf));
    end;
    myconf := TIniFile.Create(opsiscriptconf);
    myconf.WriteString('global', 'debug_prog', BoolToStr(debug_prog, False));
    myconf.WriteString('global', 'debug_lib', BoolToStr(debug_lib, False));
    myconf.WriteString('global', 'default_loglevel', IntToStr(default_loglevel));
    myconf.WriteString('global', 'force_min_loglevel', IntToStr(force_min_loglevel));
    myconf.WriteString('global', 'ScriptErrorMessages',
      BoolToStr(ScriptErrorMessages, True));
    myconf.WriteString('global', 'AutoActivityDisplay',
      BoolToStr(AutoActivityDisplay, False));
    myconf.WriteString('global', 'ReverseProductOrderByUninstall',
      BoolToStr(configReverseProductOrderByUninstall, False));
    myconf.WriteString('global', 'ReverseProductOrderByUninstall',
      BoolToStr(configSupressSystemEncodingWarning, False));
    myconf.WriteString('global', 'log_rotation_count', IntToStr(log_rotation_count));
    myconf.WriteString('global', 'writeProductLogFile',
      BoolToStr(configWriteProductLogFile, False));
    myconf.WriteString('global', 'testSyntax',
      BoolToStr(configTestSyntax, False));
    myconf.Free;
  except
    Result := False;
    raise;
  end;
end;


function readConfig: boolean;
const
  shareDelim = '\\';

var
  part1, part2, outstr: string;
  myconf: TIniFile;
  opsiclientd_conf: string;

begin
  try
    Result := True;
    if not FileExists(opsiscriptconf) then
    begin
      // prepare to create it
      ForceDirectory(ExtractFilePath(opsiscriptconf));
    end;
    myconf := TIniFile.Create(opsiscriptconf);
    debug_prog := strToBool(myconf.ReadString('global', 'debug_prog',
      boolToStr(debug_prog, True)));
    debug_lib := strToBool(myconf.ReadString('global', 'debug_lib',
      boolToStr(debug_lib, True)));
    default_loglevel := myconf.ReadInteger('global', 'default_loglevel',
      default_loglevel);
    force_min_loglevel := myconf.ReadInteger('global', 'force_min_loglevel',
      force_min_loglevel);
    ScriptErrorMessages := strToBool(myconf.ReadString('global',
      'ScriptErrorMessages', boolToStr(ScriptErrorMessages, True)));
    AutoActivityDisplay := strToBool(myconf.ReadString('global',
      'AutoActivityDisplay', boolToStr(AutoActivityDisplay, False)));
    configReverseProductOrderByUninstall :=
      strToBool(myconf.ReadString('global', 'ReverseProductOrderByUninstall',
      boolToStr(configReverseProductOrderByUninstall, False)));
    configSupressSystemEncodingWarning :=
      strToBool(myconf.ReadString('global', 'supressSystemEncodingWarning',
      boolToStr(configSupressSystemEncodingWarning, False)));
    log_rotation_count := myconf.ReadInteger('global', 'log_rotation_count',
      log_rotation_count);
    configWriteProductLogFile :=
      strToBool(myconf.ReadString('global', 'writeProductLogFile',
      boolToStr(configWriteProductLogFile, False)));
    configTestSyntax :=
      strToBool(myconf.ReadString('global', 'testSyntax',
      boolToStr(configTestSyntax, False)));
    myconf.Free;


    {$IFDEF UNIX}
    computername := getHostnameLin;
    {$ENDIF UNIX}
    Result := True;
    depoturl := '';
    depotshare := '';
    depotdir := PathDelim;
    {$IFDEF WINDOWS}
    depotdrive := 'p:';
    {$ENDIF WINDOWS}
    {$IFDEF LINUX}
    depotdrive_old := '/mnt';
    depotdrive := '/media/opsi_depot';
    {$ENDIF LINUX}
    {$IFDEF DARWIN}
    //depotdrive := '/Network/opsi_depot';
    //depotdrive := '/Volumes/opsi_depot';
    depotdrive := '/var/opsisetupadmin/opsi_depot';
    opsiclientd_conf := '/etc/opsi-client-agent/opsiclientd.conf';
    if FileExists(opsiclientd_conf) then
    begin
      myconf := TIniFile.Create(opsiclientd_conf);
      outstr := myconf.ReadString('depot_server', 'drive', depotdrive);
      if DirectoryExistsUTF8(outstr) then
        depotdrive := outstr;
      myconf.Free;
    end;


    {$ENDIF DARWIN}

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
          readconfig_done := True;
        end;
      end;

      regist.CloseKey;
      regist.Free;

    except
      Result := False;
      readconfig_done := False;
    end;
   {$ENDIF}

    if depotpath <> '' then
    begin
      depotdrive := depotpath;
     {$IFDEF WINDOWS}
      depotdrive := ExtractFileDrive(depotpath);
      depotdir := ExtractFileDir(depotpath);
     {$ENDIF}
    end;

    readconfig_done := True;
  except
    Result := False;
    raise;
  end;
end;

function readConfigFromService: string;
var
  serviceresult: string;
  configid, values, tmpstr: string;
  configlist: TStringList;
  i: integer;
begin
  try
    configlist := TStringList.Create;
    if opsidata.isConnected2(startupmessages) then
    begin
      try
        serviceresult := opsidata.getOpsiServiceConfigs;
      except
        on e: Exception do
        begin
          startupmessages.Append('Exception in readConfigFromService: ' +
            'opsidata.getOpsiServiceConfigs: ' + e.message + ' ' + DateTimeToStr(Now));
          serviceresult := '';
        end;
      end;
      //osmain.startupmessages.Add('OpsiServiceConfigs: ' + copy(serviceresult,1,100);
      Result := serviceresult;
      if jsonIsValid(serviceresult) then
      begin
        //osmain.startupmessages.Add('got valid json object from getOpsiServiceConfigs');
        if jsonAsObjectGetValueByKey(serviceresult, 'result',
          serviceresult) then
          if jsonIsArray(serviceresult) then
          begin
            //osmain.startupmessages.Add('got json Array from result');
            if jsonAsArrayToStringList(serviceresult, configlist) then
            begin
              for i := 0 to configlist.Count - 1 do
              begin
                if jsonIsObject(configlist.Strings[i]) then
                begin
                  if jsonAsObjectGetValueByKey(configlist.Strings[i],
                    'configId', configid) then
                  begin
                    //osmain.startupmessages.Add('got configid: ' + configid);
                    if pos('opsi-script.', configid) = 1 then
                    begin
                      // we got a opsi-script config

                      if LowerCase(configid) = 'opsi-script.global.debug_prog' then
                      begin
                        if jsonAsObjectGetValueByKey(configlist.Strings[i],
                          'values', values) then
                          if jsonAsArrayGetElementByIndex(values, 0, tmpstr) then
                          begin
                            osmain.startupmessages.Add('got debug_prog: ' + tmpstr);
                            if not TryStrToBool(tmpstr, debug_prog) then
                              osmain.startupmessages.Add(
                                'Error: Not a Boolean:  debug_prog: ' + tmpstr);
                            Result := 'readConfigFromService: ok';
                          end;
                      end;


                      if LowerCase(configid) = 'opsi-script.global.debug_lib' then
                      begin
                        if jsonAsObjectGetValueByKey(configlist.Strings[i],
                          'values', values) then
                          if jsonAsArrayGetElementByIndex(values, 0, tmpstr) then
                          begin
                            osmain.startupmessages.Add('got debug_lib: ' + tmpstr);
                            if not TryStrToBool(tmpstr, debug_lib) then
                              osmain.startupmessages.Add(
                                'Error: Not a Boolean:  debug_lib: ' +
                                tmpstr + ' (' + DateTimeToStr(Now) + ')');
                            Result := 'readConfigFromService: ok';
                          end;
                      end;


                      if LowerCase(configid) = 'opsi-script.global.default_loglevel' then
                      begin
                        if jsonAsObjectGetValueByKey(configlist.Strings[i],
                          'values', values) then
                          if jsonAsArrayGetElementByIndex(values, 0, tmpstr) then
                          begin
                            osmain.startupmessages.Add(
                              'got default_loglevel: ' + tmpstr);
                            if not TryStrToInt(tmpstr, default_loglevel) then
                              osmain.startupmessages.Add(
                                'Error: Not an Integer:  default_loglevel: ' + tmpstr);
                            Result := 'readConfigFromService: ok';
                          end;
                      end;

                      if LowerCase(configid) =
                        'opsi-script.global.force_min_loglevel' then
                      begin
                        osmain.startupmessages.Add(
                          'got config: opsi-script.global.force_min_loglevel');
                        if jsonAsObjectGetValueByKey(configlist.Strings[i],
                          'values', values) then
                          if jsonAsArrayGetElementByIndex(values, 0, tmpstr) then
                          begin
                            osmain.startupmessages.Add(
                              'got force_min_loglevel: ' + tmpstr);
                            if not TryStrToInt(tmpstr, force_min_loglevel) then
                              osmain.startupmessages.Add(
                                'Error: Not an Integer:  force_min_loglevel: ' + tmpstr);
                            Result := 'readConfigFromService: ok';
                          end;
                      end;

                      if LowerCase(configid) =
                        LowerCase('opsi-script.global.ScriptErrorMessages') then
                      begin
                        if jsonAsObjectGetValueByKey(configlist.Strings[i],
                          'values', values) then
                          if jsonAsArrayGetElementByIndex(values, 0, tmpstr) then
                          begin
                            osmain.startupmessages.Add(
                              'got ScriptErrorMessages: ' + tmpstr);
                            if not TryStrToBool(tmpstr, ScriptErrorMessages) then
                              osmain.startupmessages.Add(
                                'Error: Not a Boolean:  ScriptErrorMessages: ' + tmpstr);
                            Result := 'readConfigFromService: ok';
                          end;
                      end;

                      if LowerCase(configid) =
                        LowerCase('opsi-script.global.AutoActivityDisplay') then
                      begin
                        if jsonAsObjectGetValueByKey(configlist.Strings[i],
                          'values', values) then
                          if jsonAsArrayGetElementByIndex(values, 0, tmpstr) then
                          begin
                            osmain.startupmessages.Add(
                              'got AutoActivityDisplay: ' + tmpstr);
                            if not TryStrToBool(tmpstr, AutoActivityDisplay) then
                              osmain.startupmessages.Add(
                                'Error: Not a Boolean:  AutoActivityDisplay: ' + tmpstr);
                            Result := 'readConfigFromService: ok';
                          end;
                      end;

                      if LowerCase(configid) =
                        LowerCase('opsi-script.global.w10BitlockerSuspendOnReboot') then
                      begin
                        if jsonAsObjectGetValueByKey(configlist.Strings[i],
                          'values', values) then
                          if jsonAsArrayGetElementByIndex(values, 0, tmpstr) then
                          begin
                            osmain.startupmessages.Add(
                              'got w10BitlockerSuspendOnReboot: ' + tmpstr);
                            if not TryStrToBool(tmpstr, w10BitlockerSuspendOnReboot) then
                              osmain.startupmessages.Add(
                                'Error: Not a Boolean:  w10BitlockerSuspendOnReboot: '
                                + tmpstr);
                            Result := 'readConfigFromService: ok';
                          end;
                      end;

                      if LowerCase(configid) = LowerCase(
                        'opsi-script.global.ReverseProductOrderByUninstall') then
                      begin
                        if jsonAsObjectGetValueByKey(configlist.Strings[i],
                          'values', values) then
                          if jsonAsArrayGetElementByIndex(values, 0, tmpstr) then
                          begin
                            osmain.startupmessages.Add(
                              'got ReverseProductOrderByUninstall: ' + tmpstr);
                            if not TryStrToBool(tmpstr,
                              configReverseProductOrderByUninstall) then
                              osmain.startupmessages.Add(
                                'Error: Not a Boolean:  ReverseProductOrderByUninstall: '
                                + tmpstr);
                            Result := 'readConfigFromService: ok';
                          end;
                      end;

                      if LowerCase(configid) = LowerCase(
                        'opsi-script.global.supressSystemEncodingWarning') then
                      begin
                        if jsonAsObjectGetValueByKey(configlist.Strings[i],
                          'values', values) then
                          if jsonAsArrayGetElementByIndex(values, 0, tmpstr) then
                          begin
                            osmain.startupmessages.Add(
                              'got supressSystemEncodingWarning: ' + tmpstr);
                            if not TryStrToBool(tmpstr,
                              configSupressSystemEncodingWarning) then
                              osmain.startupmessages.Add(
                                'Error: Not a Boolean:  supressSystemEncodingWarning: '
                                + tmpstr);
                            Result := 'readConfigFromService: ok';
                          end;
                      end;

                      if LowerCase(configid) =
                        'opsi-script.global.log_rotation_count' then
                      begin
                        osmain.startupmessages.Add(
                          'got config: opsi-script.global.log_rotation_count');
                        if jsonAsObjectGetValueByKey(configlist.Strings[i],
                          'values', values) then
                          if jsonAsArrayGetElementByIndex(values, 0, tmpstr) then
                          begin
                            osmain.startupmessages.Add(
                              'got log_rotation_count: ' + tmpstr);
                            if not TryStrToInt(tmpstr, log_rotation_count) then
                              osmain.startupmessages.Add(
                                'Error: Not an Integer:  log_rotation_count: ' + tmpstr);
                            Result := 'readConfigFromService: ok';
                          end;
                      end;

                      if LowerCase(configid) = LowerCase(
                        'opsi-script.global.writeProductLogFile') then
                      begin
                        if jsonAsObjectGetValueByKey(configlist.Strings[i],
                          'values', values) then
                          if jsonAsArrayGetElementByIndex(values, 0, tmpstr) then
                          begin
                            osmain.startupmessages.Add(
                              'got writeProductLogFile: ' + tmpstr);
                            if not TryStrToBool(tmpstr,
                              configWriteProductLogFile) then
                              osmain.startupmessages.Add(
                                'Error: Not a Boolean:  writeProductLogFile: '
                                + tmpstr);
                            Result := 'readConfigFromService: ok';
                          end;
                      end;

                      if LowerCase(configid) = LowerCase(
                        'opsi-script.global.testSyntax') then
                      begin
                        if jsonAsObjectGetValueByKey(configlist.Strings[i],
                          'values', values) then
                          if jsonAsArrayGetElementByIndex(values, 0, tmpstr) then
                          begin
                            osmain.startupmessages.Add(
                              'got testSyntax: ' + tmpstr);
                            // do not overwrite cli parameter /testsyntax
                            if not configTestSyntax then
                              if not TryStrToBool(tmpstr,
                                configTestSyntax) then
                                osmain.startupmessages.Add(
                                  'Error: Not a Boolean:  testSyntax: '
                                  + tmpstr);
                            Result := 'readConfigFromService: ok';
                          end;
                      end;

                    end;
                  end;
                end;
              end;
            end;
          end;
      end;
    end;
    configlist.Free;
  except
    on e: Exception do
    begin
      startupmessages.Append('Exception in readConfigFromService: ' +
        e.message + ' ' + DateTimeToStr(Now));
    end;
  end;
end;



initialization
  opsiscriptconf := opsiscriptconfinit;
  {$IFDEF DARWIN}
  //force_min_loglevel := 8;
  //debug_prog:= True;
  {$ENDIF DARWIN}
  initEncoding;
  //from http://wiki.freepascal.org/Show_Application_Title,_Version,_and_Company
  FileVerInfo := TFileVersionInfo.Create(nil);
  try
    OsFileName := reencode(ParamStr(0), 'system');
    FileVerInfo.FileName := OsFileName;
    FileVerInfo.ReadFileInfo;
    OpsiscriptVersion := FileVerInfo.VersionStrings.Values['FileVersion'];
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

  OpsiscriptVersionName := 'Version ' + OpsiscriptVersion;
  opsiscriptProcName := ExtractFileName(reencode(ParamStr(0), 'system'));

end.
