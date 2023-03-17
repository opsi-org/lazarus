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
  osencoding,
  fpjson,
  jsonparser;

function readConfig: boolean;
function writeConfig: boolean;
function readConfigsFromService: string;


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
  DepotUser: string;
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
  osmain,
  oslog;

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

function StringListAsJsonArray(const StringList: TStringList): string;
(* function StringListAsJsonArray converts a StringList into an JSON array:
   'String1', 'String2' -> '["String1,"String2"]' *)
var
  i: integer;
  JsonArray: string;
begin
  JsonArray := '['; //start of JSON array
  for i:= 0 to StringList.Count - 2 do //all entries but without the last one
    JsonArray := JsonArray + '"' + StringList[i] + '",';
  JsonArray := JsonArray + '"' + StringList[StringList.Count-1] + '"]';//the last one
  Result:=JsonArray;
end;

function ConfigValueToBool(const Value: string; const ConfigName: string; Default:boolean): boolean;
begin
  if TryStrToBool(Value, Result) then
    osmain.startupmessages.Add('got ' + ConfigName + ': ' + BoolToStr(Result))
  else
  begin
    Result := Default;
    osmain.startupmessages.Add('Error: Not a Boolean: ' + ConfigName +
      ': ' + Value + ' using default value: ' + BoolToStr(Result));
  end;
end;

function ConfigValueToInt(const Value: string; const ConfigName: string; Default:integer): integer;
begin
  if TryStrToInt(Value, Result) then
    osmain.startupmessages.Add('got ' + ConfigName + ': ' + IntToStr(Result))
  else
  begin
    Result := Default;
    osmain.startupmessages.Add('Error: Not an Integer: ' + ConfigName +
      ': ' + Value + ' using default value: ' + IntToStr(Result));
  end;
end;


procedure SetSingleConfig(const JsonObject: TJSONObject; const KeyConfigID:string; const KeyConfigValue:string);
(* Procedure SetsingleConfig sets the value for an opsi-script config (Host-Parameter).
   The value is taken from a json object which contains the value and
   the corresponding config ID.
   The KeyConfigID is "id" or "configId" and the KeyConfigValue is "defaultValues" or "values"
   depending on if it is a Config or ConfigState object.

   Include here any new opsi-script Config *)
var
  ConfigID : string;
  Value: string;
begin
  ConfigID := LowerCase(JsonObject.FindPath(KeyConfigID).AsString);
  Value := JsonObject.FindPath(KeyConfigValue).AsString;
  if ConfigID = LowerCase('opsi-script.global.debug_prog') then
    debug_prog := ConfigValueToBool(Value, 'debug_prog', False)
  else if ConfigID = LowerCase('opsi-script.global.debug_lib') then
    debug_lib := ConfigValueToBool(Value, 'debug_lib', False)
  else if ConfigID = LowerCase('opsi-script.global.default_loglevel') then
    default_loglevel := ConfigValueToInt(Value, 'default_loglevel', 7)
  else if ConfigID = LowerCase('opsi-script.global.force_min_loglevel') then
    force_min_loglevel := ConfigValueToInt(Value, 'force_min_loglevel', 0)
  else if ConfigID = LowerCase('opsi-script.global.ScriptErrorMessages') then
    ScriptErrorMessages := ConfigValueToBool(Value, 'ScriptErrorMessages', False)
  else if ConfigID = LowerCase('opsi-script.global.AutoActivityDisplay') then
    AutoActivityDisplay := ConfigValueToBool(Value, 'AutoActivityDisplay', True)
  else if ConfigID = LowerCase('opsi-script.global.w10BitlockerSuspendOnReboot') then
    w10BitlockerSuspendOnReboot := ConfigValueToBool(Value, 'w10BitlockerSuspendOnReboot', True)
  else if ConfigID = LowerCase('opsi-script.global.ReverseProductOrderByUninstall') then
    configReverseProductOrderByUninstall := ConfigValueToBool(Value, 'ReverseProductOrderByUninstall', True)
  else if ConfigID = LowerCase('opsi-script.global.supressSystemEncodingWarning') then
    configsupressSystemEncodingWarning := ConfigValueToBool(Value, 'supressSystemEncodingWarning', False)
  else if ConfigID = LowerCase('opsi-script.global.log_rotation_count') then
    log_rotation_count := ConfigValueToInt(Value, 'log_rotation_count', 32)
  else if ConfigID = LowerCase('opsi-script.global.writeProductLogFile') then
    configwriteProductLogFile := ConfigValueToBool(Value, 'writeProductLogFile', False)
  else if ConfigID = LowerCase('opsi-script.global.testSyntax') then
    configtestSyntax := ConfigValueToBool(Value, 'testSyntax', False)
  else if ConfigID = LowerCase('clientconfig.depot.user') then
    DepotUser := Value
  else if KeyConfigValue = 'defaultValues' then
    LogDatei.log('Unknown config: ' + ConfigID + ' (osconf.pas: procedure SetSingleConfig)', LLWarning);
end;

procedure SetConfigs(const JsonRpcResponse: string; KeyConfigID:string; KeyConfigValue:string);
(* Procedure SetConfigs extracts the values of opsi-script configs from a JSON-RPC response.
   Therefore it loops through the result of the response. The expected result is a json array
   containing different Config or ConfigState objects in json object format.
   The KeyConfigID is "id" or "configId" and the KeyConfigValue is "defaultValues"
   or "values" depending on if it is a Config or ConfigState object *)

var
  ConfigEnum: TJSONEnum;
  SingleConfig: TJSONObject;
  Configs: TJSONArray;
begin
  if JsonRpcResponse <> '' then
  begin
    Configs := GetJSON(JsonRpcResponse).FindPath('result') as TJSONArray;
    if Configs <> nil then
      begin
      // Loop using the TJSONEnumerator
      for ConfigEnum in Configs do
      begin
        // Cast the enum value to ConfigObject
        SingleConfig := ConfigEnum.Value as TJSONObject;
        SetSingleConfig(SingleConfig, KeyConfigID, KeyConfigValue);
      end;
    end;
  end;
end;

function readConfigsFromService: string;
var
  JsonRpcResponse: string;
  ConfigIDs: TStringList;
  ConfigIDsAsJsonArray: string;
begin
  ConfigIDs := TStringList.Create;
  try
    ConfigIDs.Clear;
    //Include here any new opsi-script configs in the list.
    //Do not forget to do this for the procedure SetConfig, too.
    ConfigIDs.Append(LowerCase('opsi-script.global.debug_prog'));
    ConfigIDs.Append(LowerCase('opsi-script.global.debug_lib'));
    ConfigIDs.Append(LowerCase('opsi-script.global.default_loglevel'));
    ConfigIDs.Append(LowerCase('opsi-script.global.force_min_loglevel'));
    ConfigIDs.Append(LowerCase('opsi-script.global.ScriptErrorMessages'));
    ConfigIDs.Append(LowerCase('opsi-script.global.AutoActivityDisplay'));
    ConfigIDs.Append(LowerCase('opsi-script.global.w10BitlockerSuspendOnReboot'));
    ConfigIDs.Append(LowerCase('opsi-script.global.ReverseProductOrderByUninstall'));
    ConfigIDs.Append(LowerCase('opsi-script.global.supressSystemEncodingWarning'));
    ConfigIDs.Append(LowerCase('opsi-script.global.log_rotation_count'));
    ConfigIDs.Append(LowerCase('opsi-script.global.writeProductLogFile'));
    ConfigIDs.Append(LowerCase('opsi-script.global.testSyntax'));
    ConfigIDs.Append(LowerCase('clientconfig.depot.user'));

    if opsidata.isConnected2(startupmessages) then
    begin
      try
        ConfigIDsAsJsonArray:=StringListAsJsonArray(ConfigIDs);
        //Get defaults from service and set config default values
        JsonRpcResponse := OpsiData.getConfigObjectsFromService(ConfigIDsAsJsonArray);
        SetConfigs(JsonRpcResponse, 'id', 'defaultValues');
        //Get actual values from service and set actual config values
        JsonRpcResponse := OpsiData.getConfigStateObjectsFromService(ConfigIDsAsJsonArray);
        SetConfigs(JsonRpcResponse, 'configId', 'values');
        Result := 'readConfigFromService: ok';
      except
        on e: Exception do
        begin
          startupmessages.Append('Exception in readConfigFromService: ' +
            'opsidata.getOpsiServiceConfigs: ' + e.message + ' ' + DateTimeToStr(Now));
          Result := '';
        end;
      end;
    end;
    finally
      FreeAndNil(ConfigIDs);
    end;
 end;


initialization
  opsiscriptconf := opsiscriptconfinit;
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
