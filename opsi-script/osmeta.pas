unit osmeta;

{$mode Delphi}

interface

uses
  Classes, SysUtils,
  oslog,
  osjson,
  ostoml,
  osfunc;

const
  META_FILE_NAME = 'opsi-meta-data.toml';

var
  checkdirs: TStringList = nil;
  processes: TStringList = nil;

// are meta data about the processes we have to look for available
function metaDataFound(pathToMeta: string): boolean;
//function getMetacheckdirs: TStringList;
//function getMetaCheckProcesses: TStringList;
procedure getMetaCheckProcessData(var checkdirs: TStringList;
  var processes: TStringList);
function getInstallInBackgroundFlag: boolean;

implementation

uses
  osparser;

var
  meta_toml, meta_json: string;
  install_in_background: boolean = True;

function getInstallInBackgroundFlag: boolean;
begin
  Result := install_in_background;
end;

function getMetaData(pathToMeta: string; var meta_toml: string;
  var meta_json: string): boolean;
var
  metaFileFullName: string;
begin
  logdatei.log_prog('start getMetaData', LLinfo);
  Result := False;
  metaFileFullName := pathToMeta + PathDelim + META_FILE_NAME;
  if FileExists(metaFileFullName) then
  begin
    logdatei.log('Meta data file found: ' + metaFileFullName, LLnotice);
    meta_toml := ReadTOMLFile(metaFileFullName);
    logdatei.log_prog('meta_toml: ' + meta_toml, LLinfo);
    logdatei.log_prog('replace Script constants in MetaData: ', LLinfo);
    meta_toml := ScriptConstants.ReplaceInString(meta_toml);
    logdatei.log_prog('ConvertTOMLtoJSON MetaData: ', LLinfo);
    meta_json := ConvertTOMLtoJSON(meta_toml);
    meta_json := SysUtils.StringReplace(meta_json, #13#10, '', [rfReplaceAll]);
    //meta_json := sysutils.StringReplace(meta_json,' ','',[rfReplaceAll]);
    logdatei.log_prog('meta_json: ' + meta_json, LLinfo);
    Result := True;
    logdatei.log_prog('getMetaData Result: ' + BoolToStr(Result, True), LLinfo);
  end
  else
    logdatei.log_prog(' no MetaData file: ' + metaFileFullName, LLinfo);
  logdatei.log_prog('finished getMetaData', LLinfo);
end;

procedure AddListFromJsonObjectToStringList(const JsonObject:string; const Key: string; var List: TStringList);
var
  Value: string;
  HelperList: TStringList;
  i : integer;

begin
  HelperList := TStringList.Create;
  try
    if jsonAsObjectHasKey(JsonObject, Key) then
    begin
      jsonAsObjectGetValueByKey(JsonObject, Key, Value);
      if jsonIsArray(Value) then
      begin
        jsonAsArrayToStringList(Value, HelperList);
        if HelperList.Count > 0 then
        begin
          for i := 0 to HelperList.Count - 1 do
            //  avoid double entries
            if List.IndexOf(HelperList[i]) = -1 then
              List.Add(HelperList[i]);
        end
        else
          logdatei.log('No entries found for key: ' + Key + '. Nothing added.', LLinfo);
      end;
    end;
  finally
    FreeAndNil(HelperList);
  end;
end;


procedure getMetaCheckProcessData(var checkdirs: TStringList;
  var processes: TStringList);
var

  json_installer_backround: string = '';
  installers: string = '';
  installer_list: TStringList;
  installer_list_to_use: TStringList;
  i, installers_count: integer;
  json_requirement: string = '';
  found_os: string = '';
  my_os: string = '';
  found_os_arch: string = '';
  my_os_arch: string;
  json_processes: string = '';


begin
  logdatei.log_prog('start getMetaCheckProcessData', LLinfo);
  {$IFDEF WINDOWS}
  my_os := 'windows';
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
  my_os := 'linux';
  {$ENDIF LINUX}
  {$IFDEF DARWIN}
  my_os := 'macos';
  {$ENDIF DARWIN}
  my_os_arch := getOSArchitecture;
  // set to strings according OTW 13 / 18
  if my_os_arch = 'x86_32' then my_os_arch := 'x86';
  if my_os_arch = 'x86_64' then my_os_arch := 'x64';
  if my_os_arch = 'arm_64' then my_os_arch := 'arm64';

  installer_list := TStringList.Create;
  installer_list_to_use := TStringList.Create;

  // product table
  // nothing to read here

  // installers table array
  if not jsonAsObjectGetValueByKey(meta_json, 'installers', installers) then
    LogDatei.log('No installers found. :' + installers, LLwarning)
  else
  begin
    LogDatei.log_prog('Installers found. :' + installers, LLinfo);
    if jsonIsArray(installers) then
    begin
      installers_count := jsonAsArrayCountElements(installers);
      if installers_count > 0 then
      begin
        jsonAsArrayToStringList(installers, installer_list);
        for i := 0 to installer_list.Count - 1 do
        begin
          begin
            if jsonAsObjectHasKey(installer_list[i], 'requirement') then
            begin
              jsonAsObjectGetValueByKey(installer_list[i], 'requirement',
                json_requirement);
              jsonAsObjectGetValueByKey(json_requirement, 'os', found_os);
              logdatei.log('Installer ' + IntToStr(i) + ': Meta data: my os: ' +
                my_os + ' / required os: ' + found_os, LLinfo);
              // do we have the required OS ?
              if LowerCase(found_os) = LowerCase(my_os) then
              begin
                jsonAsObjectGetValueByKey(json_requirement, 'os_arch', found_os_arch);
                logdatei.log('Installer ' + IntToStr(i) + ': Meta data: my os_arch: ' +
                  my_os_arch + ' / required os_arch: ' + found_os_arch, LLinfo);
                // do we have the required OS_arch ?
                if LowerCase(found_os_arch) = LowerCase(my_os_arch) then
                  installer_list_to_use.Add(installer_list[i])
                else if (found_os = 'windows') and (my_os_arch = 'x64') and
                  (found_os_arch = 'x86') then
                  // windows: 32bit x86 requirement on 64bit x86 OS allowed
                  installer_list_to_use.Add(installer_list[i])
                else if LowerCase(found_os_arch) = 'arch_unknown' then
                  installer_list_to_use.Add(installer_list[i])
                else
                  logdatei.log('Installer ' + IntToStr(i) +
                    ': OS_Arch do not match', LLinfo);
              end
              else
                logdatei.log('Installer ' + IntToStr(i) + ': OS do not match', LLinfo);
            end;
          end;
        end;
      end
      else
        logdatei.log('No Installers found', LLwarning);
    end
    else
      logdatei.log('Installers found not a Array', LLwarning);
  end;
  if installer_list_to_use.Count > 0 then
  begin
    for i := 0 to installer_list_to_use.Count - 1 do
    begin
      // check if installer is enabled for background install
      jsonAsObjectGetValueByKey(installer_list[i], 'install_in_background',
        json_installer_backround);
      if (json_installer_backround = 'true') or
        // json_installer_backround = '' is ok, because true is default
        ((json_installer_backround = '')) then
      begin
        // Add check_processes_from_dirs
        AddListFromJsonObjectToStringList(installer_list_to_use[i],'check_processes_from_dirs', checkdirs);
        // Add for processes
        AddListFromJsonObjectToStringList(installer_list_to_use[i],'processes', processes);
      end
      else
      begin
        logdatei.log(
          'install_in_background=false: Installer is not allowed for background_install.',
          LLnotice);
        logdatei.log('We are in Background mode. Setup script will be defered.',
          LLnotice);
        install_in_background := False;
      end;
    end;

    if checkdirs.Count > 0 then
    begin
      logdatei.log('checkdirs found:', LLnotice);
      logdatei.log_list(checkdirs, LLdebug);
    end
    else
      logdatei.log('No checkdirs found', LLnotice);

    if processes.Count > 0 then
    begin
      logdatei.log('processes found:', LLnotice);
      logdatei.log_list(processes, LLdebug);
    end
    else
      logdatei.log('No processes to check found', LLnotice);
  end
  else
  begin
    logdatei.log('No Installers to use found. Setup script will not run.', LLcritical);
    scriptfailed := True;
    scriptstopped := True;
  end;


  FreeAndNil(installer_list);
  FreeAndNil(installer_list_to_use);
  logdatei.log_prog('finished getMetaCheckProcessData', LLinfo);
end;



function getMetacheckdirs: TStringList;
begin
  Result := TStringList.Create;
  Result.Assign(checkdirs);
end;

function getMetaCheckProcesses: TStringList;
begin
  Result := TStringList.Create;
  Result.Assign(processes);
end;


function metaDataFound(pathToMeta: string): boolean;
var
  tmpbool: boolean;
begin
  logdatei.log_prog('start metaDataFound', LLinfo);
  if not Assigned(checkdirs) then checkdirs := TStringList.Create
  else
    checkdirs.Clear;
  if not Assigned(processes) then processes := TStringList.Create
  else
    processes.Clear;

  tmpbool := getMetaData(pathToMeta, meta_toml, meta_json);
  if tmpbool then getMetaCheckProcessData(checkdirs, processes);
  Result := tmpbool;
  logdatei.log_prog('finished metaDataFound - result: ' +
    BoolToStr(tmpbool, True), LLinfo);
end;

end.
