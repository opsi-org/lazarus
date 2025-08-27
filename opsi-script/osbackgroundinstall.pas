unit osbackgroundinstall;

{$mode Delphi}

interface

uses
  Classes, SysUtils,
  strutils,
  fileutil,
  oslog,
  osregistry,
  osfunc,
  oswmi,
  osprocesses,
  oswebservice,
  fpjson,
  osjson,
  IniFiles,
  wispecfolder,
  shlobj,
  osmeta,
  osbglistwinuser;

procedure checkAndHandleRunningProductProcesses(const checkDirList: TStringList;
  const procList: TStringList; var ContinueWithInstallation: boolean);
function userAreLoggedIn: boolean;
function isBackgroundinstall_enabled: boolean;

resourcestring
  rsBgDialog_Title = 'Quit programs';
  rsBgDialog_StopFor =
    'The following programs must be stopped for the installation of \"**#@ProgramName@#**\" :';
  rsBgDialog_timeoutMessage =
    'The default method: \"**#@defaultMethod@#**\" is automatically selected in : **{timeout}** minutes.';
  rsBgDialog_deferMessage =
    'You can defer **#@maxDefer@#** times and have already done so **#@deferLoops@#** times.';
  rsBgDialog_BtnDefer = 'Defer Installation';
  rsBgDialog_BtnRecheck = 'Try again';
  rsBgDialog_BtnKill = 'Kill the program(s)';



implementation

uses
  osparser;

var
  configsLoaded: boolean = False;
  // the configs:
  backgroundinstall_enabledDefault: boolean = False;
  defaultActionDefault: string = 'defer';
  maxdeferLoopsDefault: integer = 5;
  maxrecheckLoopsDefault: integer = 3;
  dialogTimeoutsSecondsDefault: integer = 60;
  maxdeferActionDefault: string = 'kill';
  maxrecheckActionDefault: string = 'defer';
  //// ## here all background configs

function userAreLoggedIn: boolean;
var
  userlist: TStringList;
begin
  Result := False;
  userlist := getLoggedInUserList;
  if userlist.Count > 0 then Result := True;
  FreeAndNil(userlist);
end;


(*
Es gibt jetzt einen neuen RPC-Call showDialog.
Wir der ausgeführt, starten auf allen Desktops notifier.
Dabei kann man einen Titel, eine Nachricht (Markdown),
einen Timeout und eine Liste von Buttons übergeben.
Ein Button benötigt die Attribute id (str) und label (str),
optional noch order (int) und default (bool).
Drückt der User einen Button, liefert der RPC die ID des Buttons als action zurück.
Nach dem Timeout wird als action die id des Default-Buttons zurückgegeben
und zusätzlich timed_out: true.
Ist bereits ein anderer Dialog-RPC am Laufen, wird der alte RPC abgebrochen.
Das Ergebnis des abgebrochenen RPC ist dann action: null, canceled: true.
In der Nachricht kann der Platzhalter {timeout} verwendet werden.
Der wird dann durch den mm:ss ersetzt.

 opsi-cli jsonrpc execute --timeout 3600
 hostControl_opsiclientdRpc
 "showDialog" '["Programme beenden", "Folgende Prozesses müssen **beendet** werden:\n\n ... \n\n{timeout}", 30, [{"id":"retry","label":"Nochmal versuchen"},{"id":"not_now","label":"Nicht jetzt","default":true},{"id":"stop_proc","label":"Prozesse beenden"}]]' '["js-client1.uib.local"]' 3600
 *)

function opsiclientdRpcShowdialog(title: string; messageList: TStringList;
  ButtonList: TStringList; MethodList: TStringList; defaultMethod: string;
  dialogTimeoutsString: string): string;
var
  Messages: TStringList;
  omc: TOpsiMethodCall;
  JsonButtonArrayStr, JsonPartStr, JsonPartStr2: string;
  i, j: integer;
  rpcButonlistList: TStringList;
  buttonStr: string;
  defaultArg: string;
  parameters: array of string;
  timeout: string = '';
  timeoutint: integer = 0;
  testresult: string = '';
  errorOccured: boolean;
  myconf: TIniFile;
  opsiclientd_conf: string = '';
  serviceURL: string = '';
  username: string = '';
  password: string = '';
  sessionid: string = '';
  UserAgent: string = 'opsiServiceCall';
  local_opsidata: TOpsi4Data = nil;
  messagestr: string;
  resultTimeout: boolean;
  methodIndex: integer;
begin

  Messages := TStringList.Create;
  rpcButonlistList := TStringList.Create;
  // default timeout
  Result := '-1';
  // other possible results: index of methodlist

  // get opsiclientd credentials - copied from osparser: function TuibInstScript.doOpsiServiceCall: entry  tscOpsiclientd
  serviceurl := 'https://localhost:4441/opsiclientd';
  {$IFDEF WINDOWS}
  opsiclientd_conf :=
    getSpecialFolder(CSIDL_PROGRAM_FILES) +
    '\opsi.org\opsi-client-agent\opsiclientd\opsiclientd.conf';
  {$ENDIF WINDOWS}
  {$IFDEF UNIX}
  opsiclientd_conf := '/etc/opsi-client-agent/opsiclientd.conf';
  {$ENDIF UNIX}
  if FileExists(opsiclientd_conf) then
  begin
    LogDatei.log('Found opsiclientd.conf: ' + opsiclientd_conf, LLDebug2);
    myconf := TInifile.Create(opsiclientd_conf);
    password := myconf.ReadString('global', 'opsi_host_key', '');
    username := myconf.ReadString('global', 'host_id', '');
    myconf.Free;
  end
  else
    LogDatei.log('opsiclientd.conf does not exist or could not accessed at ' +
      opsiclientd_conf, LLError);

  if password = '' then
  begin
    errorOccured := True;
    LogDatei.log('Error: Can not get opsi_host_key', LLError);
  end
  else
  begin
    LogDatei.log_prog('We start a new service connection to the local opsiclientd.',LLinfo);
    LogDatei.log_prog('This will lead to some messages you should ignore:',LLinfo);
    LogDatei.log_prog('Ignore: "No method backend_getLicensingInfo at service - perhaps old server or opsiclientd, we fallback to backend_info"',LLinfo);
    LogDatei.log_prog('Ignore: "No modules data in backend_info"',LLinfo);
    local_opsidata := TOpsi4Data.Create;
    UserAgent := 'opsi-script (RpcShowDialog:opsiclientd)';
    local_opsidata.initOpsiConf(serviceurl, username, password,
      '', '', '', UserAgent);
    omc := TOpsiMethodCall.Create('backend_info', []);
    testresult := local_opsidata.CheckAndRetrieve(omc, errorOccured);
    omc.Free;
    if errorOccured then
    begin
      LogDatei.log('Error: ' + testresult, LLError);
    end;
  end;


  try
    if not errorOccured then
    begin
      // we have a connection to the opsiclientd
      try
        // do this opsiclientd provide the new showDialog method ?
        if local_opsidata.isMethodProvided('showDialog') then
        begin
          // opsiclientd >= 4.3.9.0
          // create Button definition for showdialog
          for i := 0 to Buttonlist.Count - 1 do
          begin
            if MethodList[i] = defaultMethod then
              defaultArg := ',"default":true'
            else
              defaultArg := '';
            buttonStr := '{"id":"' + MethodList[i] + '","label":"' +
              ButtonList[i] + '"' + defaultArg + '}';
            logdatei.log('buttonStr: ' + buttonStr, LLDebug);
            rpcButonlistList.Add(buttonStr);
          end;
          if not stringListToJsonArray(rpcButonlistList, JsonButtonArrayStr) then
          begin
            LogDatei.log('Error at opsiclientdRpcShowdialog:jsonStringListToJsonArray ',
              LLerror);
            JsonButtonArrayStr := '[]';
          end;

          // create Message definition for showdialog
          messagestr := messageList.Text;
          messagestr := messagestr.Replace(#13, #10, [rfReplaceAll]);
          logdatei.log('messagestr: ' + messagestr, LLDebug);


          try
            // create parameter definition for showdialog
            setlength(parameters, 4);
            parameters[0] := '"' + title + '"';   // title
            parameters[1] := '"' + messagestr + ' "';
            // message
            parameters[2] := dialogTimeoutsString;        // timeout seconds
            parameters[3] := JsonButtonArrayStr;  // buttons
            for j := 0 to 3 do
            begin
              logdatei.log('param[' + IntToStr(j) + ']: ' + parameters[j], LLDebug);
            end;

            // call the dialog
            omc := TOpsiMethodCall.Create('showDialog', parameters);
            omc.JSONValueSyntaxInParameterList := True;
            testresult := '';
            testresult := local_opsidata.CheckAndRetrieveString(omc, errorOccured);
            logdatei.log('testresult: ' + testresult, LLDebug);

            if Assigned(omc) then
              FreeAndNil(omc);

            if errorOccured then
            begin
              LogDatei.log('Error: ' + testresult, LLError);
            end
            else
            begin
              LogDatei.log('JSON result: ' + testresult, LLinfo);
            end;
          except
            on e: Exception do
            begin
              LogDatei.log('Exception in opsiclientdRpcShowdialog: doOpsiServiceCall' +
                e.message, LLError);
            end;
          end;

          // process result
          // expect something like that:
          // {"canceled":false,"action":"defer","button_pressed":{"order":0,"label":"Jetzt nicht","default":true,"id":"defer"},"timed_out":false}
          if not (jsonAsObjectGetValueByKey(testresult, 'timed_out', JsonPartStr)) then
          begin
            logdatei.log('Problem parsing result: timed_out: ' + testresult, LLError);
          end
          else
          begin
            resultTimeout := StrToBool(JsonPartStr);
            if resultTimeout then
            begin
              Result := '-1';
              logdatei.log('Backgound Install Dialog timed out : Result is -1 ',
                LLnotice);
            end
            else
            if not (jsonAsObjectGetValueByKey(testresult,
              'button_pressed', JsonPartStr)) then
            begin
              logdatei.log('Problem parsing result: button_pressed: ' +
                testresult, LLError);
            end
            else
            begin
              if not (jsonAsObjectGetValueByKey(JsonPartStr, 'id', JsonPartStr2)) then
              begin
                logdatei.log('Problem parsing result: button_pressed: ' +
                  JsonPartStr, LLError);
              end
              else
              begin
                methodIndex := MethodList.IndexOf(JsonPartStr2);
                if methodIndex = -1 then
                begin
                  logdatei.log(
                    'Problem parsing result: button_pressed:id unknown method ' +
                    JsonPartStr2, LLError);
                end
                else
                begin
                  Result := IntToStr(methodIndex);
                  logdatei.log('Backgound Install Dialog Button pressed: "' +
                    MethodList[methodIndex] + '" Result is: ' +
                    IntToStr(methodIndex), LLnotice);
                end;
              end;
            end;
          end;
        end
        else  // opsiclientd < 4.3.9.0
          LogDatei.log(
            'Error: opsicliend do not provide showdialog method - update opsi-client-agent',
            LLerror);
      except
        on e: Exception do
        begin
          Logdatei.log('Exception in showServerRpcDialog: ' +
            e.message + ' ' + DateTimeToStr(Now), LLerror);
          Result := '';
        end;
      end;
      // close the connection to opsiclientd
      omc := TOpsiMethodCall.Create('backend_exit', []);
      testresult := local_opsidata.CheckAndRetrieve(omc, errorOccured);
      omc.Free;
    end;
  finally
    FreeAndNil(rpcButonlistList);
    FreeAndNil(Messages);
    FreeAndNil(local_opsidata);
  end;
end;

function productLoopCounter(productID: string; loopType: string;
  loopcommand: string): integer;
(*
generic loop control
here used for checking deferloops, recheckloops
the loop data is stored in the registry
stored is: productID / loopType / counter
allowed commands: "ADD", "DEL", "READ", "INC"
*)
var
  readloop, writeloop: integer;
  ProductSettingsKey: string;
  myreg: TuibRegistry;
  tmpstr: string = '';
  regType: TuibRegDataType;
begin
  Result := -1;

  // make it case insensitive
  loopcommand := UpperCase(loopcommand);
  productID := LowerCase(ProductID);

  //set productID to default if empty
  if productID = '' then
    Logdatei.log('No productId given', LLError);

  ProductSettingsKey := 'SOFTWARE\opsi.org\opsi-script\checkRunningProc\' +
    productID + loopType;
  myreg := TuibRegistry.Create(False);
  myreg.OpenKey('HKLM', ProductSettingsKey);
  myreg.ReadEntry('loop', regType, tmpstr);
  if not TryStrToInt(tmpstr, readloop) then
  begin
    readloop := 0;
    Logdatei.log('No Integer found for readloop - default to 0', LLInfo);
  end
  else
    Logdatei.log('Found readloop: ' + IntToStr(readloop), LLInfo);
  if loopcommand = 'READ' then
  // nothing to do
  else if loopcommand = 'INC' then
  begin
    writeloop := readloop + 1;
    myreg.WriteEntry('loop', trdInteger, IntToStr(writeloop));
    ;
    Logdatei.log('loop counter written to registry: ' + IntToStr(writeloop), LLInfo);
  end
  else if loopcommand = 'DEL' then
  begin
    myreg.DeleteKey('HKLM', ProductSettingsKey);
    Logdatei.log('loop counter registry key deleted: HKLM\' +
      ProductSettingsKey, LLInfo);
    readloop := 0;
  end
  else
  begin
    LogDatei.log('undefined command: ' + loopcommand + ' in productLoopCounter' +
      LineEnding + 'allowed command: "ADD", "DEL", "READ"' + LineEnding +
      'result defaults to -1', LLerror);
    readloop := -1;
  end;
  Result := readloop;
end;


function getRunningProcesses(searchProcessList: TStringList): TStringList;
(*
;@author    Detlef Oertel
;@date      28.4.2025
;@Description  Returns from $searchProcessList$ only the running once
;@Returns    Returns the list of running processes out of $searchProcessList$
;@OnError    Returns empty list
;@SpecialCase
;@References
;@Links
;@Requires
;@ParamDesc_$searchProcessList$  List of processe to look for
;@Example  set $runningProcessList$ = getRunningProcesses($searchProcessList$)
*)
var
  procname: string;
  procshort: string;
  proclong: string;
  i, k: integer;

  fullpathlist: TStringList;
  templist: TStringList;
  proclistPartsList: TStringArray;
  foundList: TStringList;
begin
  fullpathlist := TStringList.Create;
  foundList := TStringList.Create;
  Result := TStringList.Create;
  // we want no duplicates in Result
  // https://www.freepascal.org/docs-html/rtl/classes/tstringlist.duplicates.html
  Result.Sorted:= true;
  Result.Duplicates:= dupIgnore;

  LogDatei.log('Got list of searched processes: ', LLdebug2);
  LogDatei.log_list(searchProcessList, LLdebug2);

  // get process list
  templist := getProcessListWithPath;
  LogDatei.log('Got Processlist: ', LLdebug2);
  LogDatei.log_list(templist, LLdebug2);

  // create list with running processes, where we have the full path entries
  for i := 0 to templist.Count - 1 do
  begin
    procname := templist[i];
    proclistPartsList := splitstring(procname, ';');
    // do we have a long name (with path) ?
    if length(proclistPartsList) = 4 then
    begin
      procshort := proclistPartsList[0];
      proclong := proclistPartsList[3];
      setlength(proclistPartsList,0);
      proclistPartsList := nil;
      //avoid empty entries
      if not (proclong = '') then
      begin
        // avoid double entries
        for k := 0 to fullpathlist.Count - 1 do
        begin
          foundList.Clear;
          if lowercase(fullpathlist.Strings[k]) = lowercase(proclong) then
            foundList.Add(proclong);
        end;
        if foundList.Count = 0 then
        begin
          // add unique element
          fullpathlist.Add(proclong);
        end;
      end;
    end;
  end;

  LogDatei.log('Got Full path processlist: ', LLdebug2);
  LogDatei.log_list(fullpathlist, LLdebug2);

  // get list of running searched processes
  for i := 0 to searchProcessList.Count - 1 do
  begin
    procname := searchProcessList[i];
    LogDatei.log_prog('Search for proc: ' + procname, LLdebug);
    // is it proc with path ?
    if fileexists(procname) then
    begin
      LogDatei.log_prog('Search file exists: ' + procname, LLdebug2);
      foundList.Clear;
      for k := 0 to fullpathlist.Count - 1 do
      begin
        LogDatei.log_prog('Is equal ? : ' + procname + ' =  ' +
          fullpathlist.Strings[k], LLdebug);
        if lowercase(fullpathlist.Strings[k]) = lowercase(procname) then
        begin
          LogDatei.log('Add search proc (long): ' + procname, LLdebug);
          foundList.Add(procname);
        end;
      end;
      if foundList.Count >= 0 then
      begin
        // add found elements
        Result.AddStrings(foundList);
      end;
    end
    else
    begin
      // only short procname
      if processIsRunning(procname) then
      begin
        LogDatei.log('Add search proc (short): ' + procname, LLdebug);
        Result.Add(procname);
      end;
    end;
  end;

  LogDatei.log('Got list of running searched processes: ', LLdebug);
  LogDatei.log_list(Result, LLdebug);

  FreeAndNil(templist);
  FreeAndNil(fullpathlist);
  FreeAndNil(foundList);
end;

function removeServicesBinariesFromList(runningProcessList: TStringList): TStringList;
(*
;@author    Detlef Oertel
;@date      28.4.2025
;@Description  Returns from runningProcessList only the entries that not detected as service binaries
;@Returns    Returns the list runningProcessList with all  service processes removed
;@OnError    Returns empty list
;@SpecialCase
;@References
;@Links
;@Requires
;@ParamDesc_runningProcessList  List of processe to look for
;@Example  runningProcessList = removeServicesBinariesFromList(removeServicesBinariesFromList)
*)
var
  procname: string;
  proclong: string;
  i, k: integer;
  fullpathentry: string;

  fullpathlist: TStringList;
  servicelist: TStringList;
  foundList: TStringList;
  ErrorMsg:string;
  WMIProperties :TStringList;
begin
  fullpathlist := TStringList.Create;
  servicelist:= TStringlist.Create;
  foundList := TStringList.Create;
  Result := TStringList.Create;
  WMIProperties := TStringList.Create;
  WMIProperties.Add('PathName');

  // set default result
  Result.AddStrings(runningProcessList);
  LogDatei.log('Got list of running processes: ', LLdebug);
  LogDatei.log_list(runningProcessList, LLdebug);

  //create list with full service path entries
  try
    if osGetWMI('root\cimv2', 'Win32_Service', WMIProperties,
      '', servicelist, ErrorMsg) then
    begin
      LogDatei.log('Got list of service processes1: ', LLdebug2);
      LogDatei.log_list(servicelist, LLdebug2)
    end;
  except
    on e: Exception do
    begin
      LogDatei.log('Error executing :get service : with powershell: ' + e.message,
        LLError);
    end
  end;
  //  remove all svchost.exe services
  try
    k := 0;
    // using while loop to iterate through List,
    // if an element is deleted the index must not be increased
    while k < servicelist.Count do
    begin
      if AnsiContainsText(servicelist[k], 'svchost.exe') then
        servicelist.Delete(k)
      //after deleting element k, k indexes the next element
      else
        Inc(k); //to index the next element if nothing is deleted
    end;
  except
    on e: Exception do
    begin
      LogDatei.log('Error on producing sublist: ' + e.message, LLerror);
    end
  end;
  LogDatei.log('Got list of service processes2: ', LLdebug2);
  LogDatei.log_list(servicelist, LLdebug2);

  // create list with running processes, where we have the full path entries
  for i := 0 to servicelist.Count - 1 do
  begin
    procname := servicelist[i];

    // we have always a long name (with path)
    proclong := procname;
    //avoid empty entries
    if not (proclong = '') then
    begin
      // avoid double entries
      for k := 0 to fullpathlist.Count - 1 do
      begin
        foundList.Clear;
        if lowercase(fullpathlist.Strings[k]) = lowercase(proclong) then
          foundList.Add(proclong);
      end;
      if foundList.Count = 0 then
      begin
        // add unique element
        // That would be something like this: pathname="c:\program files (x86)\anydesk\anydesk.exe"
        // remove 'pathname='
        proclong := copy(proclong,10,length(proclong));
        fullpathlist.Add(proclong);
      end;
    end;
  end;

  LogDatei.log('Got Full path service list: ', LLdebug);
  LogDatei.log_list(fullpathlist, LLdebug);

  foundList.Clear;
  // get list of running searched processes
  for i := 0 to Result.Count - 1 do
  begin
    procname := Result[i];
    // is it proc with path ?
    for k := 0 to fullpathlist.Count - 1 do
    begin
      fullpathentry := lowercase(fullpathlist.Strings[k]);
      // do we have a quoted entry ?
      if pos('"', fullpathentry) = 1 then
      begin
        // remove first quote
        fullpathentry := copy(fullpathentry, 2, length(fullpathentry));
        // remove second quote and every thing beyond
        fullpathentry := copy(fullpathentry, 1, pos('"', fullpathentry) - 1);
      end
      else
      // do we have a non quoted entry with empty space ?
      if pos(' ', fullpathentry) > 0 then
      begin
        // remove first space and every thing beyond
        fullpathentry := copy(fullpathentry, 0, pos(' ', fullpathentry) - 1);
      end;
      LogDatei.log_prog('fullpathentry: ' + fullpathentry, LLdebug);

      // do we have a match ?
      if fullpathentry = lowercase(procname) then
      begin
        LogDatei.log('Add matched proc (long): ' + procname, LLdebug);
        foundList.Add(procname);
      end;
    end;
  end;
  // have we found an matching service processes ?
  if foundList.Count >= 0 then
  begin
    for k := foundList.Count - 1 downto 0 do
    begin
      // remove found elements from
      LogDatei.log('Remove matched proc (long)2: ' + foundList[k], LLdebug);
      Result.Delete(Result.IndexOf(foundList[k]));
    end;
  end;

  LogDatei.log('Got list of running searched processes: ', LLdebug);
  LogDatei.log_list(Result, LLdebug);

  FreeAndNil(fullpathlist);
  FreeAndNil(foundList);
  FreeAndNil(servicelist);
  FreeAndNil(WMIProperties);
end;

function StringListAsJsonArray(const StringList: TStringList): string;
  // function StringListAsJsonArray converts a StringList into an JSON array:
  // 'String1', 'String2' -> '["String1,"String2"]'
var
  i: integer;
  JsonArray: string;
begin
  JsonArray := '['; //start of JSON array
  for i := 0 to StringList.Count - 2 do //all entries but without the last one
    JsonArray := JsonArray + '"' + StringList[i] + '",';
  JsonArray := JsonArray + '"' + StringList[StringList.Count - 1] + '"]';//the last one
  Result := JsonArray;
end;


function ConfigValueToBool(const Value: string; const ConfigName: string;
  Default: boolean): boolean;
begin
  if TryStrToBool(Value, Result) then
  begin
    LogDatei.log('got ' + ConfigName + ': ' + BoolToStr(Result, True), LLinfo);
  end
  else
  begin
    Result := Default;
    Logdatei.log('Error: Not a Boolean: ' + ConfigName + ': ' +
      Value + ' using default value: ' + BoolToStr(Result, True), LLinfo);
  end;
end;

function ConfigValueToInt(const Value: string; const ConfigName: string;
  Default: integer): integer;
begin
  if TryStrToInt(Value, Result) then
  begin
    Logdatei.log('got ' + ConfigName + ': ' + IntToStr(Result), LLinfo);
  end
  else
  begin
    Result := Default;
    Logdatei.log('Error: Not an Integer: ' + ConfigName + ': ' +
      Value + ' using default value: ' + IntToStr(Result), LLinfo);
  end;
end;

function ConfigValueToStr(const Value: string; const ConfigName: string;
  Default: string): string;
begin
  if Value <> '' then
  begin
    Result := Value;
    Logdatei.log('got ' + ConfigName + ': ' + Result, LLinfo);
  end
  else
  begin
    Result := Default;
    Logdatei.log('Config value empty: ' + ConfigName + ': ' +
      Value + ' using default value: ' + Result, LLinfo);
  end;
end;

function readConfigFromJsonResult(JSONObject: TJSONObject; configname: string): string;
var
  JSONArray: TJSONArray;
begin
  Result := '';
  JSONArray := (JSONObject.Find(LowerCase(configname)) as TJSONArray);
  if JSONArray <> nil then
  begin
    if JSONArray.Count > 0 then
      Result := JSONArray.Items[0].AsString
    else
      Logdatei.log('Config: ' + configname + ' not found', LLinfo);
  end
  else
    Logdatei.log('Config: ' + configname + ' not found', LLinfo);
  Logdatei.log_prog('got ' + configname + ' :' + Result, LLinfo);
end;

procedure SetBackgoundConfigsOpsi43(const JsonRpcResponse: string);
var
  ConfigValue: string;
  JSONObject: TJSONObject;
  JSONResult: TJSONData;
begin
  // This will overwrite the hard coded defaults by the configs
  if JsonRpcResponse <> '' then
  begin
    JSONResult := GetJSON(JsonRpcResponse).FindPath('result');
    if (JSONResult <> nil) and not (JSONResult is TJSONNull) and
      (JSONResult.AsJSON <> '{}') then
    begin
      JSONObject := JSONResult as TJSONObject;
      JSONResult := JSONObject.Find(OpsiData.actualClient);
      if (JSONResult <> nil) and not (JSONResult is TJSONNull) and
        (JSONResult.AsJSON <> '{}') then
      begin
        JSONObject := JSONResult as TJSONObject;
        //Logdatei.log_prog('got JSONResult :'+ JSONResult.AsString, LLinfo);
        ConfigValue := readConfigFromJsonResult(JSONObject,
          'opsi-script.backgroundinstall.active');
        backgroundinstall_enabledDefault :=
          ConfigValueToBool(ConfigValue, 'active',
          backgroundinstall_enabledDefault);

        ConfigValue := readConfigFromJsonResult(JSONObject,
          'opsi-script.backgroundinstall.default_action');
        defaultActionDefault :=
          ConfigValueToStr(ConfigValue, 'default_action', defaultActionDefault);

        ConfigValue := readConfigFromJsonResult(JSONObject,
          'opsi-script.backgroundinstall.max_defer');
        maxdeferLoopsDefault :=
          ConfigValueToInt(ConfigValue, 'max_defer', maxdeferLoopsDefault);

        ConfigValue := readConfigFromJsonResult(JSONObject,
          'opsi-script.backgroundinstall.max_recheck');
        maxrecheckLoopsDefault :=
          ConfigValueToInt(ConfigValue, 'max_recheck', maxrecheckLoopsDefault);

        ConfigValue := readConfigFromJsonResult(JSONObject,
          'opsi-script.backgroundinstall.dialog_timeout');
        dialogTimeoutsSecondsDefault :=
          ConfigValueToInt(ConfigValue, 'dialog_timeout', dialogTimeoutsSecondsDefault);

        ConfigValue := readConfigFromJsonResult(JSONObject,
          'opsi-script.backgroundinstall.max_defer_action');
        maxdeferActionDefault :=
          ConfigValueToStr(ConfigValue, 'max_defer_action', maxdeferActionDefault);

        ConfigValue := readConfigFromJsonResult(JSONObject,
          'opsi-script.backgroundinstall.max_recheck_action');
        maxrecheckActionDefault :=
          ConfigValueToStr(ConfigValue, 'max_recheck_action', maxrecheckActionDefault);
      end;
    end
    else
      Logdatei.log('Got no configs, json result was: ' + JSONResult.AsJSON, LLerror);
  end;
end;


function readBackgoundConfigsFromService: boolean;
var
  JsonRpcResponse: string;
  ConfigIDs: TStringList;
  Messages: TStringList;
  ConfigIDsAsJsonArray: string;
begin
  ConfigIDs := TStringList.Create;
  Messages := TStringList.Create;
  Result := False;
  try
    LogDatei.log('Read opsi-script.backgroundinstall configs', LLnotice);
    ConfigIDs.Clear;
    //Include here any new opsi-script Backgound configs in the list.
    //Do not forget to do this for the procedure SetConfig, too.
    ConfigIDs.Append(LowerCase(
      'opsi-script.backgroundinstall.active'));
    ConfigIDs.Append(LowerCase('opsi-script.backgroundinstall.default_action'));
    ConfigIDs.Append(LowerCase('opsi-script.backgroundinstall.max_defer'));
    ConfigIDs.Append(LowerCase('opsi-script.backgroundinstall.max_recheck'));
    ConfigIDs.Append(LowerCase('opsi-script.backgroundinstall.dialog_timeout'));
    ConfigIDs.Append(LowerCase('opsi-script.backgroundinstall.max_defer_action'));
    ConfigIDs.Append(LowerCase('opsi-script.backgroundinstall.max_recheck_action'));

    if Assigned(opsidata) then
    begin
      if opsidata.isConnected2(Messages) then
      begin
        try
          ConfigIDsAsJsonArray := StringListAsJsonArray(ConfigIDs);
          if OpsiData.isMethodProvided('configState_getValues') then
          begin
            //opsi 4.3
            JsonRpcResponse := OpsiData.getConfigStateValuesFromService(
              ConfigIDsAsJsonArray);
            // This will overwrite the hard coded defaults by the configs:
            SetBackgoundConfigsOpsi43(JsonRpcResponse);
            Result := True;
          end
          else
          begin
            LogDatei.log('opsi 4.3 or higher required for Background install', LLerror);
          end;
        except
          on e: Exception do
          begin
            Logdatei.log('Exception in readBackgoundConfigsFromService: ' +
              e.message + ' ' + DateTimeToStr(Now), LLerror);
            Result := False;
          end;
        end;
      end
      else
        Logdatei.log('No opsi-service in readBackgoundConfigsFromService: opsidata not connected.'
          , LLwarning);
    end
    else
      Logdatei.log('No opsi-service in readBackgoundConfigsFromService: opsidata not assigned.'
        , LLwarning);
  finally
    FreeAndNil(ConfigIDs);
    FreeAndNil(Messages);
  end;
end;

function isBackgroundinstall_enabled: boolean;
begin
  Result := False;
  if not configsLoaded then
    configsLoaded := readBackgoundConfigsFromService;
  Result := backgroundinstall_enabledDefault;
end;

procedure checkAndHandleRunningProcFull(procList: TStringList;
  defaultMethod: string; maxDeferLoops: integer; maxRecheckLoops: integer; var ContinueWithInstallation: boolean);
var
  handle_running_process_method: string;
  deferloops: integer;
  recheckloops: integer;
  procName: string = '';
  runningProcNamesStr: string = '';
  showDialog: boolean = True;
  defaultMethodText: string = '';
  defaultMethodIndex: integer;
  checkProcesses: boolean = False;
  dialogTimeoutsSeconds: integer;
  dialogTimeoutsString: string = '';
  dialogResult: string;
  backgroundinstall_enabled: boolean;
  maxdeferAction: string;
  maxrecheckAction: string;
  buttonindex: integer;
  i: integer;
  recheckCounter: integer;
  infostr: string;
  productID: string;
  tmpstr: string;

  runningProcList: TStringList;
  serviceBinaryList: TStringList;
  messageList: TStringList;
  methodlist: TStringList; // '["defer","recheck","kill"]'
  ButtonList: TStringList; // '["Jetzt nicht","Nochmal probieren","Prozess beenden"]'
begin
  // create lists
  serviceBinaryList := TStringList.Create;
  messageList := TStringList.Create;
  methodlist := TStringList.Create;
  ButtonList := TStringList.Create;
  // set defaults
  if not configsLoaded then
    configsLoaded := readBackgoundConfigsFromService;
  // init Parameter defaults
  if defaultMethod = '' then defaultMethod := defaultActionDefault;
  if maxDeferLoops = 0 then maxDeferLoops := maxDeferLoopsDefault;
  if maxRecheckLoops = 0 then maxRecheckLoops := maxRecheckLoopsDefault;
  dialogTimeoutsSeconds := dialogTimeoutsSecondsDefault;
  checkProcesses := backgroundinstall_enabledDefault;
  backgroundinstall_enabled := backgroundinstall_enabledDefault;
  if maxdeferAction = '' then maxdeferAction := maxdeferActionDefault;
  if maxrecheckAction = '' then maxrecheckAction := maxrecheckActionDefault;
  //if dialogtimeoutAction = '' then dialogtimeoutAction := dialogtimeoutActionDefault;
  productID := opsidata.getActualProductId;

  try
    ContinueWithInstallation := false;
    if backgroundinstall_enabled then
    begin
      LogDatei.log('backgroundinstall is enabled', LLnotice);
      // check license module for background_install
      if opsidata.backgroundInstallActivated then
      begin
        LogDatei.log('Modul background_install is activated', LLnotice);
        LogDatei.log('users are logged in: background install situation', LLnotice);
        // init lists
        methodlist.Add('defer');
        methodlist.Add('recheck');
        methodlist.Add('kill');
        ButtonList.Add(rsBgDialog_BtnDefer);
        ButtonList.Add(rsBgDialog_BtnRecheck);
        ButtonList.Add(rsBgDialog_BtnKill);

        // get loop counter
        deferloops := productLoopCounter(productID, 'defer', 'read');
        if deferloops >= maxdeferLoops then
        begin
          logdatei.log(
            'Max Defer loops reached - do not show defer button - new default: ' +
            maxdeferAction,
            LLnotice);
          methodlist.Clear;
          methodlist.Add('recheck');
          methodlist.Add('kill');
          ButtonList.Clear;
          ButtonList.Add(rsBgDialog_BtnRecheck);
          ButtonList.Add(rsBgDialog_BtnKill);
          defaultMethod := maxdeferAction;
        end;

        // get caption of default method
        defaultMethodIndex := methodlist.IndexOf(defaultMethod);
        defaultMethodText := ButtonList[defaultMethodIndex];

        // check given method
        if -1 = methodlist.IndexOf(defaultMethod) then
        begin
          LogDatei.log('In checkAndHandleRunningProc given default method: ' +
            defaultMethod + ' is not allowed.', LLerror);
          LogDatei.log('Allowed: ' + methodlist.Text, LLerror);
        end;

        logdatei.log('procs to search:',LLdebug);
        LogDatei.log_list(procList,LLdebug);
        if procList.Count > 0 then
        begin
          LogDatei.log('check for running processes before we start', LLnotice);
          // the running processes from the search list
          runningProcList := getRunningProcesses(procList);
          // remove the service binaries from the running list
          runningProcList.Text := removeServicesBinariesFromList(runningProcList).Text;

          // Any critical processes to handle ?
          if runningProcList.Count > 0 then
          begin
            // no dialog if dialog timeout is 0
            if dialogTimeoutsSeconds = 0 then showDialog := False;

            dialogTimeoutsString := IntToStr(dialogTimeoutsSeconds);

            if showDialog then
            begin
              handle_running_process_method := 'recheck';
              for recheckCounter := 1 to maxRecheckLoops do
              begin
                if handle_running_process_method = 'recheck' then
                begin
                  // prepare message
                  runningProcNamesStr := runningProcList.Text;
                  LogDatei.log('processes: ' + runningProcNamesStr +
                    ' are running. This may interfere with the installation',
                    LLnotice);
                  // we have  running processes that may interfere with the installation
                  messageList.Clear;
                  // use resource string
                  tmpstr := rsBgDialog_StopFor;
                  // replace place holder by proc name
                  tmpstr := ReplaceStr(tmpstr, '#@ProgramName@#',
                    opsidata.getActualProductName);
                  messageList.add(tmpstr);
                  for i := 0 to runningProcList.Count - 1 do
                    messageList.Add(' **' +
                      ExtractFileName(runningProcList[i]) + '** ');
                  tmpstr := rsBgDialog_timeoutMessage;
                  // replace place holder by defaultMethod
                  tmpstr := ReplaceStr(tmpstr, '#@defaultMethod@#', defaultMethodText);
                  messageList.Add(tmpstr);
                  tmpstr := rsBgDialog_deferMessage;
                  // replace place holder by default values
                  tmpstr := ReplaceStr(tmpstr, '#@maxDefer@#', IntToStr(maxDeferLoops));
                  tmpstr := ReplaceStr(tmpstr, '#@deferLoops@#', IntToStr(deferloops));
                  messageList.Add(tmpstr);

                  // do not use opsi-script internal dialog any more
                  //dialogResult := showInternalDialog(messageList, ButtonList, timeoutMessage, dialogTimeoutsString);
                  // use opsiclientdRpcShowdialog
                  dialogResult :=
                    opsiclientdRpcShowdialog(rsBgDialog_Title, messageList,
                    ButtonList, MethodList, defaultMethod, dialogTimeoutsString);
                  buttonindex := StrToInt(dialogResult);
                  if buttonindex = -1 then
                  begin
                    LogDatei.log('showmessagebox timeout. We use default: ' +
                      defaultMethod, LLwarning);
                    buttonindex := methodlist.IndexOf(defaultMethod);
                  end;
                  handle_running_process_method := methodlist[buttonindex];
                  if handle_running_process_method = 'recheck' then
                  begin
                    // does the user stopped the process ?
                    runningProcList := getRunningProcesses(runningProcList);
                    if runningProcList.Count = 0 then
                    begin
                      handle_running_process_method := 'solved';
                      recheckloops := productLoopCounter(productID, 'recheck', 'del');
                      // problem solved - do not ask again
                      showDialog := False;
                    end
                    else
                    begin
                      runningProcNamesStr := runningProcList.Text;
                      recheckloops := productLoopCounter(productID, 'recheck', 'inc');
                      if recheckloops + 1 >= maxRecheckLoops then
                      begin
                        LogDatei.log('Recheck loop counter now ' +
                          IntToStr(recheckloops + 1) + ' - maximum reached',
                          LLwarning);
                        LogDatei.log(
                          'Showmessagebox after recheck: process still running. We use default: '
                          + defaultMethod, LLwarning);
                        buttonindex := methodlist.IndexOf(defaultMethod);
                        handle_running_process_method := methodlist[buttonindex];
                        showDialog := False;
                        recheckloops :=
                          productLoopCounter(productID, 'recheck', 'del');
                      end
                      else
                        LogDatei.log('process still running after recheck - retry',
                          LLnotice);
                    end;
                  end
                  else
                  begin
                    showDialog := False;
                    recheckloops := productLoopCounter(productID, 'recheck', 'del');
                  end;
                  LogDatei.log('after showmessagebox: we continue with: ' +
                    handle_running_process_method, LLnotice);
                end;
              end;
            end
            else // no dialog
              handle_running_process_method := defaultMethod;

            // execute the selected method
            if handle_running_process_method = 'kill' then
            // will be handled later
            else if handle_running_process_method = 'defer' then
            begin
              deferloops := productLoopCounter(productID, 'defer', 'inc');
              if deferloops < maxDeferLoops then
              begin
                LogDatei.log(
                  'product will defered due to running processes. It was now defered '
                  + IntToStr(deferloops + 1) + ' times - max: ' +
                  IntToStr(maxDeferLoops), LLnotice);
                opsidata.setActionProgress('defered due to running processes');
                // isDeferred
                LogDatei.log('Set Deferred: Stop script and restore product state',
                  LLnotice);
                productDeferred := True;
                productStopped := True;
              end
              else
              begin
                LogDatei.log('process will be killed because product was defered ' +
                  IntToStr(deferloops + 1) +
                  ' times before due to running processes. - maximum reached',
                  LLError);
                handle_running_process_method := 'kill';
              end;
            end
            else if handle_running_process_method = 'ignore' then
            begin
              LogDatei.log('running process: ' + runningProcNamesStr +
                ' ignored - install anyway', LLwarning);
              // delete the loopcounter if exists
              deferloops := productLoopCounter(productID, 'defer', 'del');
            end
            else if handle_running_process_method = 'solved' then
            begin
              ContinueWithInstallation := True;
              LogDatei.log('running processes: ' + runningProcNamesStr +
                ' were stopped - install: ' + BoolToStr(ContinueWithInstallation, True), LLnotice);
              // no process is running - delete the loopcounter if exists
              deferloops := productLoopCounter(productID, 'defer', 'del');
            end
            else
            begin
              LogDatei.log('undefined method: ' + handle_running_process_method +
                ' in handle running process', LLerror);
              LogDatei.log('allowed methods: ["solved","defer","kill"]', LLerror);
              // delete the loopcounter if exists
              deferloops := productLoopCounter(productID, 'defer', 'del');
            end;
            if handle_running_process_method = 'kill' then
            begin
              for i := 0 to runningProcList.Count - 1 do
                KillTask(ExtractFileName(runningProcList[i]), infostr);
              Sleep(2000);
              // delete the loopcounter if exists
              deferloops := productLoopCounter(productID, 'defer', 'del');
              runningProcList := getRunningProcesses(runningProcList);
              if runningProcList.Count > 0 then
              begin
                LogDatei.log('failed to kill: ' + runningProcNamesStr +
                  ' - will abort', LLerror);
              end
              else
              begin
                ContinueWithInstallation := true;
                LogDatei.log('processes successful killed - install: ' + BoolToStr(ContinueWithInstallation,true), LLnotice);
              end;
            end;
          end
          else
          begin
            ContinueWithInstallation := true;
            LogDatei.log('no process is running', LLnotice);
            //- delete the loopcounter if exists
            deferloops := productLoopCounter(productID, 'defer', 'del');
          end;
        end
        else
        begin
          ContinueWithInstallation := true;
          LogDatei.log('no process to check', LLnotice);
          // delete the loopcounter if exists
          deferloops := productLoopCounter(productID, 'defer', 'del');
        end;
        // clearup recheck counter
        recheckloops := productLoopCounter(productID, 'recheck', 'del');
      end
      else
      begin
        LogDatei.log(
          'Modul background_install is not activated - background install check aborted',
          LLerror);
        LogDatei.log('Check your opsi background_install license', LLerror);
      end;
    end
    else
      LogDatei.log('backgroundinstall_enabled is not enabled via config', LLnotice);

  finally
    FreeAndNil(runningProcList);
    FreeAndNil(serviceBinaryList);
    FreeAndNil(messageList);
    FreeAndNil(methodlist);
    FreeAndNil(ButtonList);
  end;
end;

procedure checkAndHandleRunningProc(procList: TStringList; var ContinueWithInstallation: boolean);
begin
  try
    checkAndHandleRunningProcFull(procList, '', 0, 0, ContinueWithInstallation);
  except
    on E: Exception do
    begin
      LogDatei.log('Exception in checkAndHandleRunningProc ', LLError);
      LogDatei.log('Exception: Message: ' + E.message, LLError);
    end;
  end;
end;

procedure checkAndHandleRunningProductProcesses(const checkDirList: TStringList;
  const procList: TStringList; var ContinueWithInstallation: boolean);
var
  dirList: TStringList;
  allDirList: TStringList;
  resultList: TStringList;
  installdir: string;
  i, k, index: integer;
begin
  if getInstallInBackgroundFlag then
  begin
    LogDatei.log('install_in_background=true: Installer is allowed for background_install',
      LLnotice);
    dirList := TStringList.Create;
    allDirList := TStringList.Create;
    resultList := TStringList.Create;
    try
      try
        // collect from installdirs
        if checkDirList.Count > 0 then
        begin
          for i := 0 to checkDirList.Count - 1 do
          begin
            installdir := checkDirList[i];
            installdir := ScriptConstants.ReplaceInString(installdir);
            if directoryExists(installdir) then
            begin
              FindAllFiles(dirList, installdir, '*.exe', True);
              // avoid double entries: remove allDirList from dirList
              if allDirList.Count > 0 then
              begin
                for k := 0 to allDirList.Count - 1 do
                begin
                  index := dirList.IndexOf(allDirList[k]);
                  if index > -1 then dirList.Delete(index);
                end;
              end;
              allDirList.AddStrings(TStrings(dirList));
            end;
          end;
        end;
        logdatei.log('procs from checkdirs:', LLdebug);
        LogDatei.log_list(allDirList, LLdebug);
        // add proclist
        // avoid double entries: remove proclist from allDirList
        if procList.Count > 0 then
        begin
          for i := 0 to procList.Count - 1 do
          begin
            index := allDirList.IndexOf(procList[i]);
            if index > -1 then allDirList.Delete(index);
          end;
        end;
        resultList.Text := allDirList.Text;
        resultList.AddStrings(TStrings(procList));
        // avoid empty entries
        for i := 0 to resultList.Count - 1 do
        begin
          if resultList[i] = '' then resultList.Delete(i);
        end;
        logdatei.log('procs from dirs + procs:', LLdebug);
        LogDatei.log_list(resultList, LLdebug);
        if resultList.Count > 0 then
        begin
          // call checkAndHandleRunningProcFull with defaults
          checkAndHandleRunningProc(resultList, ContinueWithInstallation);
        end
        else
        begin
          LogDatei.log(
            'checkAndHandleRunningProductProcesses: No meta data matching processes found',
            LLnotice);
          ContinueWithInstallation := True;
        end;
      except
        on E: Exception do
        begin
          LogDatei.log('Exception in checkAndHandleRunningProductProcesses ', LLError);
          LogDatei.log('Exception: Message: ' + E.message, LLError);
          ContinueWithInstallation := False;
        end;
      end;
    finally
      FreeAndNil(dirList);
      FreeAndNil(allDirList);
      FreeAndNil(resultList);
    end;
  end
  else
  begin
    LogDatei.log('install_in_background=false: Set Deferred: Stop script',
      LLnotice);
    productDeferred := True;
    productStopped := True;
    ContinueWithInstallation := False;
  end;
end;

end.
