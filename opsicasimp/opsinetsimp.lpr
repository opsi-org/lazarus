program opsinetsimp;

{$mode objfpc}{$H+}
{$apptype console}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  //Interfaces, // this includes the LCL widgetset
  Classes,
  SysUtils,
  CustApp,
  oswebservice,
  oslog,
  superobject,
 // {$IFDEF LINUX}
  fileinfo,
  winpeimagereader, // {need this for reading exe info}
  elfreader, // {needed for reading ELF executables}
// {$ENDIF LINUX}
  inifiles,
  ocsnetboot,
  ocsglobal;

type

  { Topsicasimp }

  Topsicasimp = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { Topsicasimp }



var
  optionlist: TStringList;
  INI: TINIFile;
  logfilename: string;

  procedure downloadWithWebdav;
  var
    davProductId, myFilesName: utf8string;
    myFiles, myDummyList: TStringList;
    i: integer;
  begin
    myFiles := TStringList.Create;
    myDummyList := TStringList.Create;
    davProductId := opsidata.getActualProductId;
    myFilesName := davProductId + '/' + davProductId + '.files';
    Logdatei.DependentAdd('Loading file: ' + myFilesName, LLDebug2);
    if opsidata.getFileFromDepot(myFilesName, True, myFiles) then
    begin
      if myFiles.Count > 0 then
      begin
        for i := 0 to myFiles.Count - 1 do
        begin
          Logdatei.DependentAdd('Files[' + IntToStr(i) + '] : ' + myFiles.Strings[i], LLDebug2);
          if pos('f', myFiles.Strings[i]) = 1 then
          begin
            myFilesName := copy(myFiles.Strings[i], 4, length(myFiles.Strings[i]));
            myFilesName := copy(myFilesName, 0, pos('''', myFilesName) - 1);
            myFilesName := AnsiToUtf8(davProductId + '/' + myFilesName);
            myFilesName := StringReplace(myFilesName, ' ', '%20',
              [rfIgnoreCase, rfReplaceAll]);
            Logdatei.DependentAdd('downloading file : ' + myFilesName, LLDebug2);
            if opsidata.getFileFromDepot(myFilesName, False, myDummyList) then
              Logdatei.DependentAdd('file download successful : ' + myFilesName, LLDebug2)
            else
              Logdatei.DependentAdd('file download failed : ' + myFilesName, LLWarning);
          end;
        end;
      end;
    end;
    myFiles.Free;
    myDummyList.Free;
  end;

  procedure ProcessProdukt(var extremeErrorLevel: TErrorLevel);

  var
    Verfahren: TAction;
    Pfad: string;
    scriptname, absscriptname: string;
    ProductvarsForPC: TStringList;

  begin
    ProductvarsForPC := TStringList.Create;
    ProductvarsForPC.Clear;

    Verfahren := opsidata.getProductAction;
    if Verfahren = tacNull then
      exit;

    ProductvarsForPC := opsidata.getProductproperties;
    if not opsidata.initProduct then
      extremeErrorLevel := levelFatal;
    Logdatei.DependentAdd('ProcessNonZeroScript opsidata initialized', LLdebug2);
    Verfahren := opsidata.getProductAction;

    if Verfahren in [tacDeinstall, tacSetup, tacOnce, tacAlways,
      tacCustom, tacLogin] then
    begin
      scriptname := opsidata.getProductScriptPath(Verfahren);
      Logdatei.DependentAdd('scriptname: "' + scriptname + '", special path: "' +
        pfad + '"', LLnotice);
      //absscriptname := makeAbsoluteScriptPath(Pfad, scriptname);
      if Verfahren = tacSetup then
        opsidata.setProductState(tpsInstalling);
    end;
    Logdatei.DependentAdd('First ProcessNonZeroScript finished', LLDebug2);
    //Logdatei.DependentAdd('Setup script name: '+opsidata.getProductScriptPath(tacSetup), LLessential);


    if (Verfahren = tacUpdate) or ((Verfahren = tacSetup)) then
      //the setup script does not require a direct Reboot or Logout, and seems to be finished right now
      if extremeErrorLevel = LevelFatal then
      begin
        Logdatei.DependentAdd(
          'We do not look for a update script, because the setup script is failed', LLnotice);
      end
      else
      begin
        opsidata.setProductActionRequest(tapUpdate);
        //LogDatei.DependentAdd('product "' + Produkt + ' set to update', LLessential);
        Logdatei.DependentAdd('get Update script name ...', LLdebug2);
        scriptname := opsidata.getProductScriptPath(tacUpdate);
        Logdatei.DependentAdd('Update script name: ' + absscriptname, LLessential);
      end;

  end;


  procedure BuildPC;

  var
    i: integer;
    KorrektSortierte: integer;
    ///Produktschalter, NeuerSchalter :   String;
    ///Logdateiname1 :   String;
    Bootmode: string;
    bootmodeFromRegistry: boolean;
    Fehler: string;
    numberValue: string;
    //regDataType: tuibRegDataType;
    errorNumber: integer;
    extremeErrorLevel: TErrorLevel;
    showErrorMessages: boolean;
    ps: string;
    Produkt: string;
    Produkte: TStringList;




    procedure LogProductSequence(const produkte: TStringList;
      errorfound: boolean; numberOfCorrectItems: integer);

    var
      i: integer;
      Zeile: string;
      SaveProductname: string;
      requestedAction: TAction;
    begin
      LogDatei.LogProduktId := False;
      SaveProductname := Topsi4data(opsidata).getActualProductId;
      LogDatei.DependentAdd('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~',
        LLnotice);
      if errorfound then
        LogDatei.DependentAdd('Error in the conditions for the sequence of products, '
          + ' the sorted list of maximum length is:',
          LLnotice)
      else
        LogDatei.DependentAdd('Resolved sequence of products (' +
          DateTimeToStr(Now) + '): ',
          LLnotice);

      i := 0;
      while i < numberOfCorrectItems do
      begin
        // set productname to get action via opsidata
        opsidata.setActualProductName(produkte.strings[i]);
        requestedAction := opsidata.getProductAction;
        if errorfound or (requestedAction <> tacNull) then
        begin
          Zeile := 'Product ' + IntToStr(i) + ' ' + #9 + Produkte.Strings[i] +
            ' : ' + opsidata.actionToString(requestedAction);
          LogDatei.DependentAdd(Zeile, LLnotice);
        end;
        Inc(i);
      end;
      LogDatei.DependentAdd('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~', LLnotice);

      opsidata.setActualProductName(SaveProductname);
    end;

  var
    goOn: boolean;
    problemString: string;
    ///val :   Integer;

  begin
    Produkte := TStringList.Create;
    goOn := True;
    //writeln('.');
    OpsiData.setActualClient(myclientid);
    if Produkte <> nil then
      Produkte.Free;
    Produkte := OpsiData.getListOfProducts;


    (* walk through all products *)
    if goOn then
    begin

      if Produkte.Count > 0 then
      begin
        LogProductSequence(Produkte, False, Produkte.Count);
      end;

      i := 1;
      while (i <= Produkte.Count) do
      begin
        Produkt := Produkte.Strings[i - 1];
        opsidata.setActualProductName(Produkt);
        if trim(Produkt) = '' then
          LogDatei.DependentAdd('product ' + IntToStr(i - 1) + ' is "" ', LLnotice);
        extremeErrorLevel := Level_not_initialized;
        logdatei.ActionProgress := '';

        if opsidata.getProductAction <> tacNull then
        begin
          ProcessProdukt(extremeErrorLevel);
          opsidata.UpdateSwitches(extremeErrorLevel, logdatei.actionprogress);
          if mywebdav then
            downloadWithWebdav;
          opsidata.finishProduct;
          LogDatei.LogProduktId := False;
        end;
        Inc(i);
      end;
      opsidata.saveOpsiConf;
      //writeln('.');
      Produkte.Free;
      Produkte := nil;
    end;
    //opsidata.saveOpsiConf;
    //writeln('.');
    errorNumber := 0;
    showErrorMessages := False;
    //OpsiData.finishOpsiconf;
    //writeln('.');
  end;




  procedure MyOpsiMethodCall(const method: string; parameters: array of string);
  var
    omc: TOpsiMethodCall;
    errorOccured: boolean;
    resultstring: string;
  begin
    try
      //if param = '' then
      omc := TOpsiMethodCall.Create(method, parameters);
      //else
      //  omc := TOpsiMethodCall.Create(method,[param]);
      resultstring := opsidata.checkAndRetrieve(omc, errorOccured);
    except
      //LogDatei.DependentAdd('Exception calling method: '+method+'  - with param: '+param, LLerror);
      LogDatei.DependentAdd('Exception calling method: ' + method, LLerror);
    end;
  end;

  function MyOpsiMethodCall2(const method: string; parameters: array of string): string;
  var
    omc: TOpsiMethodCall;
    errorOccured: boolean;
    resultstring: string;
    resultstringlist: TStringList;
    i: integer;
  begin
    try
      //if param = '' then
      omc := TOpsiMethodCall.Create(method, parameters);
      //else
      //  omc := TOpsiMethodCall.Create(method,[param]);
      //resultstring :=  opsidata.checkAndRetrieveString(omc,errorOccured);
      resultstringlist := TStringList.Create;
      resultstringlist := opsidata.checkAndRetrieveList(omc, errorOccured);
      for i := 0 to resultstringlist.Count - 1 do
        Result := resultstringlist[i];

    except
      //LogDatei.DependentAdd('Exception calling method: '+method+'  - with param: '+param, LLerror);
      LogDatei.DependentAdd('Exception calling method: ' + method, LLerror);
    end;
  end;

  function initLogging(const clientname: string): boolean;
  begin
    Result := True;
    logdatei := TLogInfo.Create;
    logfilename := ExtractFilePath(ParamStr(0)) + PathDelim + clientname + '.log';
    logdatei.CreateTheLogfile(logfilename, False);
    logdatei.LogLevel := mydebug_level;
  end;


  function initConnection: boolean;
  begin
    Result := True;
    LogDatei.DependentAdd('service_url=' + myservice_url, LLDebug2);
    LogDatei.DependentAdd('service_user=' + myservice_user, LLDebug2);
    LogDatei.DependentAdd('service_pass=' + myservice_pass, LLDebug2);
    LogDatei.DependentAdd('clientid=' + myclientid, LLDebug2);
    LogDatei.DependentAdd('ip=' + myip, LLDebug2);
    opsidata := TOpsi4Data.Create;
    LogDatei.DependentAdd('opsidata created', LLDebug2);
    opsidata.setActualClient(myclientid);
    opsidata.initOpsiConf(myservice_url, myclientid, myservice_pass, '', myip, myport);
    LogDatei.DependentAdd('opsidata initialized', LLDebug2);
  end;

  procedure Topsicasimp.DoRun;
  var
    ErrorMsg: string;
    parameters: array of string;
    resultstring: string;
    FileVerInfo : TFileVersionInfo;
  begin

    //from http://wiki.freepascal.org/Show_Application_Title,_Version,_and_Company
    FileVerInfo := TFileVersionInfo.Create(nil);
    try
      FileVerInfo.FileName := ParamStr(0);
      FileVerInfo.ReadFileInfo;
      myversion := FileVerInfo.VersionStrings.Values['FileVersion'];
    finally
      FileVerInfo.Free;
    end;


    optionlist := TStringList.Create;
    optionlist.Append('help');
    //optionlist.Append('conffile::');
    optionlist.Append('service_url:');
    optionlist.Append('service_user::');
    optionlist.Append('service_pass:');
    optionlist.Append('clientid:');
    optionlist.Append('ip::');
    optionlist.Append('debug_level::');
    optionlist.Append('mode::');
    optionlist.Append('mac::');
    optionlist.Append('webdav::');
    //optionlist.Append('port::');
    // quick check parameters
    ErrorMsg := CheckOptions('', optionlist);
    if ErrorMsg <> '' then
    begin
      ShowException(Exception.Create(ErrorMsg));
      WriteHelp;
      Terminate;
      Exit;
    end;



    // parse parameters
    if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    if HasOption('mode') then
    begin
      mymode := trim(GetOptionValue('mode'));
    end
    else
    begin
      mymode := 'netboot';
      myerror := 'Notice: No mode given - using default (netboot)';
    end;
    writeln('mode=' + mymode);

    if HasOption('debug_level') then
    begin
      mydebug_level := StrToInt(trim(GetOptionValue('debug_level')));
    end
    else
    begin
      mydebug_level := 8;
      myerror := 'Notice: No debug_level given - using default (8)';
    end;
    writeln('debug_level=' + IntToStr(mydebug_level));


    if HasOption('clientid') then
    begin
      myclientid := trim(GetOptionValue('clientid'));
      initLogging(myclientid);
    end
    else
    begin
      myerror := 'Error: No clientid given.';
      //WriteHelp;
      myclientid := 'detlefvm10.uib.local';
    end;
    writeln('clientid=' + myclientid);

    if HasOption('service_url') then
    begin
      myservice_url := trim(GetOptionValue('service_url'));
    end
    else
    begin
      myerror := 'Error: No service_url given.';
      //WriteHelp;
    end;
    writeln('service_url=' + myservice_url);

    if HasOption('service_user') then
    begin
      myservice_user := trim(GetOptionValue('service_user'));
    end
    else
    begin
      myservice_user := myclientid;
      myerror := 'Notice: No service_user given - using clientid';
    end;
    writeln('service_user=' + myservice_user);

    if HasOption('service_pass') then
    begin
      myservice_pass := trim(GetOptionValue('service_pass'));
    end
    else
    begin
      myerror := 'Error: No service_pass given.';
      //WriteHelp;
    end;
    writeln('service_pass=' + myservice_pass);


  (*
  if HasOption('conffile') then
  begin
    myconffile := trim(GetOptionValue('conffile'));
    if not FileExists(myconffile) then
      myconffile := ExtractFilePath(paramstr(0))+PathDelim+myconffile;
    if not FileExists(myconffile) then
    begin
      myerror := 'Error: Given conffile: '+myconffile+' does not exist.';
      WriteHelp;
    end;
    // try to read it as config.ini
    INI := TINIFile.Create(myconffile);
    myservice_url := trim(INI.ReadString('service','service_url',''));
    myservice_user := trim(INI.ReadString('service','service_user',''));
    myservice_pass := trim(INI.ReadString('service','service_pass',''));
    ini.free;
    if myservice_url = '' then
    begin
       myerror := 'Error: No service_url in conffile: '+myconffile;
       WriteHelp;
    end;
    if myservice_user = '' then
    begin
       myerror := 'Error: No service_user in conffile: '+myconffile;
       WriteHelp;
    end;
    if myservice_pass = '' then
    begin
       myerror := 'Error: No service_pass in conffile: '+myconffile;
       WriteHelp;
    end;
  end
  else
  begin
    myerror := 'Error: No conffile given.';
    WriteHelp;
  end;
  writeln('service_url='+myservice_url);
  writeln('service_user='+myservice_user);
  writeln('service_pass=*******');//+myservice_pass);
  *)


    if HasOption('ip') then
    begin
      myip := trim(GetOptionValue('ip'));
      if myip = '' then
      begin
        myerror := 'Error: No valid IP given.';
       // WriteHelp;
       myip := '192.168.10.10';
      end;
    end;
    writeln('ip=' + myip);



    if HasOption('mac') then
    begin
      mymac := trim(GetOptionValue('mac'));
    end
    else
    begin
      mymac := '00:11:22:33:44:55';
      myerror := 'Notice: No mode given - using default (00:11:22:33:44:55)';
    end;
    writeln('mac=' + mymac);

    if HasOption('webdav') then
    begin
      mywebdav := True;
    end;
    writeln('webdav=' + BoolToStr(mywebdav, True));


    doNetboot(mymac,mynetbootserver);
    (* opsiconfd bootimage log
    (193)     [5] [Apr 25 12:36:13] Authorization request from host pctry4detlef.uib.local@192.168.2.109 (application: opsi linux bootimage 20180208) (workers.py|215)
    (240)     [5] [Apr 25 12:36:13] -----> Executing: backend_getInterface() (JsonRpc.py|128)
    (250)     [5] [Apr 25 12:36:13] -----> Executing: configState_getClientToDepotserver([], u'pctry4detlef.uib.local', True, []) (JsonRpc.py|128)
    (319)     [5] [Apr 25 12:36:14] -----> Executing: licenseOnClient_getOrCreateObject(u'pctry4detlef.uib.local', None, u'win10-x64-1709', None) (JsonRpc.py|128)
    (330)     [5] [Apr 25 12:36:14] -----> Executing: user_getCredentials(u'pcpatch', u'pctry4detlef.uib.local') (JsonRpc.py|128)
    (351)     [5] [Apr 25 12:36:14] -----> Executing: productOnClient_updateObjects(<ProductOnClient(clientId=u'pctry4detlef.uib.local', productId=u'win10-x64-1709', installationStatus=u'unknown', actionRequest=None)>) (JsonRpc.py|128)
    (362)     [5] [Apr 25 12:36:14] -----> Executing: backend_info() (JsonRpc.py|128)
    (372)     [5] [Apr 25 12:36:14] -----> Executing: backend_info() (JsonRpc.py|128)
    (392)     [5] [Apr 25 12:36:15] -----> Executing: auditHardware_getConfig(None) (JsonRpc.py|128)
    (398)     [5] [Apr 25 12:36:17] Creating unix socket u'/var/run/opsipxeconfd/opsipxeconfd.socket' (OpsiPXEConfd.py|74)
    (404)     [5] [Apr 25 12:36:18] -----> Executing: auditHardwareOnHost_setObsolete(u'pctry4detlef.uib.local') (JsonRpc.py|128)
    (415)     [5] [Apr 25 12:36:20] -----> Executing: auditHardwareOnHost_createObjects([<AuditHardwareOnHost(hostId=u'pctry4detlef.uib.local', hardwareClass=u'AUDIO_CONTROLLER', name=u'6 Series/C200 Series Chipset Family High Definition Audio Controller')>, <AuditHardwareOnHost(hostId=u...) (JsonRpc.py|128)
    (555)     [5] [Apr 25 12:36:30] -----> Executing: productOnClient_deleteObjects([]) (JsonRpc.py|128)
    (565)     [5] [Apr 25 12:40:36] Application 'opsi linux bootimage 20180208' on client '192.168.2.109' did not send cookie (workers.py|185)
    (568)     [5] [Apr 25 12:40:36] New session created (session.py|77)
    (579)     [5] [Apr 25 12:40:36] Authorization request from host pctry4detlef.uib.local@192.168.2.109 (application: opsi linux bootimage 20180208) (workers.py|215)
    (598)     [5] [Apr 25 12:40:37] -----> Executing: backend_getInterface() (JsonRpc.py|128)
    (608)     [5] [Apr 25 12:40:37] -----> Executing: configState_getClientToDepotserver([], u'pctry4detlef.uib.local', True, []) (JsonRpc.py|128)
    (672)     [5] [Apr 25 12:40:37] -----> Executing: licenseOnClient_getOrCreateObject(u'pctry4detlef.uib.local', None, u'win7-x64', None) (JsonRpc.py|128)
    (682)     [5] [Apr 25 12:40:38] -----> Executing: user_getCredentials(u'pcpatch', u'pctry4detlef.uib.local') (JsonRpc.py|128)
    (701)     [5] [Apr 25 12:40:38] -----> Executing: productOnClient_updateObjects(<ProductOnClient(clientId=u'pctry4detlef.uib.local', productId=u'win7-x64', installationStatus=u'unknown', actionRequest=None)>) (JsonRpc.py|128)
    (711)     [5] [Apr 25 12:40:38] -----> Executing: backend_info() (JsonRpc.py|128)
    (721)     [5] [Apr 25 12:40:38] -----> Executing: backend_info() (JsonRpc.py|128)
    (740)     [5] [Apr 25 12:40:38] -----> Executing: auditHardware_getConfig(None) (JsonRpc.py|128)
    (752)     [5] [Apr 25 12:40:42] -----> Executing: auditHardwareOnHost_setObsolete(u'pctry4detlef.uib.local') (JsonRpc.py|128)
    (762)     [5] [Apr 25 12:40:43] -----> Executing: auditHardwareOnHost_createObjects([<AuditHardwareOnHost(hostId=u'pctry4detlef.uib.local', hardwareClass=u'AUDIO_CONTROLLER', name=u'6 Series/C200 Series Chipset Family High Definition Audio Controller')>, <AuditHardwareOnHost(hostId=u...) (JsonRpc.py|128)
    (790)     [5] [Apr 25 12:40:53] -----> Executing: productOnClient_deleteObjects([<ProductOnClient(clientId=u'pctry4detlef.uib.local', productId=u'win10-x64-1709', installationStatus=u'unknown', actionRequest=u'setup')>]) (JsonRpc.py|128)
    (800)     [5] [Apr 25 12:43:31] New session created (session.py|77)
    (801)     [5] [Apr 25 12:43:31] Application 'opsi linux bootimage 20180208' on client '192.168.2.109' supplied non existing session id: ZGALX74IQwh7LTB72rzpI5HRzAeb73Av (Worker.py|398)
    (812)     [5] [Apr 25 12:43:31] Authorization request from host pctry4detlef.uib.local@192.168.2.109 (application: opsi linux bootimage 20180208) (workers.py|215)
    (832)     [5] [Apr 25 12:43:32] -----> Executing: getPcpatchPassword(u'pctry4detlef.uib.local') (JsonRpc.py|128)
    (842)     [5] [Apr 25 12:43:33] -----> Executing: productOnClient_updateObjects(<ProductOnClient(clientId=u'pctry4detlef.uib.local', productId=u'win7-x64', installationStatus=u'unknown', actionRequest=u'none')>) (JsonRpc.py|128)
    (852)     [5] [Apr 25 12:43:33] -----> Executing: log_write(u'bootimage', u'Apr 25 12:39:54 [opsiinit] opsi init script started\nApr 25 12:39:54 [opsiinit] opsi linux bootimage version: 20180208 (4.1)\nApr 25 12:39:54 [opsiinit] getBootParam(pwh): cmdline: ini...) (JsonRpc.py|128)

    *)
   (*
    // here come the local boot part
    if initConnection then
    begin
      writeln('init done');
      LogDatei.DependentAdd('init done', LLNotice);
      LogDatei.DependentAdd('Starting opsiclientd part:', LLNotice);
      MyOpsiMethodCall('backend_getInterface', []);
      MyOpsiMethodCall('backend_info', []);
      MyOpsiMethodCall('backend_info', []);
      MyOpsiMethodCall('dispatcher_getConfig', []);
      MyOpsiMethodCall('accessControl_authenticated', []);
      MyOpsiMethodCall('backend_setOptions', ['{"addConfigStateDefaults":True}']);
      MyOpsiMethodCall('configState_getObjects', ['', '{"objectId":"' + myclientid + '"}']);
      //productOnClient_getObjects([u'actionRequest'], {'actionRequest': [u'setup', u'uninstall', u'update', u'always', u'once', u'custom'], 'clientId': u'win7x64.vmnat.local', 'productType': u'LocalbootProduct'}
      MyOpsiMethodCall('productOnClient_getObjects',
        ['actionRequest',
        '{"actionRequest":["setup","uninstall","update","always","once","custom"],"clientId":"' +
        myclientid +
        '","productType":"LocalbootProduct"}']);
      MyOpsiMethodCall('backend_setOptions', ['{"addConfigStateDefaults":True}']);
      MyOpsiMethodCall('configState_getObjects',
        ['', '{"configId":["clientconfig.depot.dynamic","clientconfig.depot.protocol","opsiclientd.depot_server.depot_id","opsiclientd.depot_server.url"],"objectId":"' + myclientid + '"}']);
      //configState_getClientToDepotserver([], [u'win7x64.vmnat.local'], True, [u'acroread', u'config-win-base', u'firefox', u'flashplayer', u'hwaudit', u'javavm', u'jedit', u'mshotfix', u'shutdownwanted', u'swaudit', u'vmwaretools']) (JsonRpc.py|123)
      //MyOpsiMethodCall2('configState_getClientToDepotserver', ['','['+myclientid+']','True','[acroread, config-win-base, firefox, flashplayer, hwaudit, javavm, jedit, mshotfix, shutdownwanted, swaudit, vmwaretools]']);
      resultstring := MyOpsiMethodCall2('configState_getClientToDepotserver',
        ['[]', '[' + myclientid + ']']);
      //resultstring := MyOpsiMethodCall2('configState_getClientToDepotserver', ['[]','['+myclientid+']','True','["acroread", "config-win-base"]']);
      myDepotId := SO(resultstring).S['depotId'];
      LogDatei.DependentAdd('myDepotId: ' + myDepotId, LLDebug2);
      //host_getObjects([], {'type': u'OpsiDepotserver', 'id': [u'sepiolina.vmnat.local']}) (JsonRpc.py|123)
      MyOpsiMethodCall('host_getObjects',
        ['', '{"type":"OpsiDepotserver","id":["' + myDepotId + '"]}']);
      //user_getCredentials(u'pcpatch', u'win7x64.vmnat.local') (JsonRpc.py|123)
      MyOpsiMethodCall('user_getCredentials', ['pcpatch', myclientid]);
      MyOpsiMethodCall('user_getCredentials', ['pcpatch', myclientid]);
      MyOpsiMethodCall('configState_getObjects', ['', '{"objectId":"' + myclientid + '"}']);
      MyOpsiMethodCall('configState_getObjects', ['', '{"objectId":"' + myclientid + '"}']);




      LogDatei.DependentAdd('Starting opsi-winst part:', LLNotice);
      MyOpsiMethodCall('backend_info', []);
      //getDepotId(u'win7x64.vmnat.local') (JsonRpc.py|123)
      MyOpsiMethodCall('getDepotId', [myclientid]);
      BuildPC;
    (*
    opsidata.getListOfProducts;
    //getProductProperties_hash(u'mshotfix', u'win7x64.vmnat.local')
    MyOpsiMethodCall('getProductProperties_hash', ['mshotfix',myclientid]);
    //getProduct_hash(u'mshotfix', u'sepiolina.vmnat.local') (JsonRpc.py|123)
    MyOpsiMethodCall('getProduct_hash', ['mshotfix',myDepotId]);
    //productOnClient_getObjects(u'', {'productType': u'LocalbootProduct', 'clientId': u'win7x64.vmnat.local', 'productId': u'mshotfix'}) (JsonRpc.py|123)
    MyOpsiMethodCall('productOnClient_getObjects', ['','{"productType":"LocalbootProduct","clientId":"'+myclientid+'","productId":"mshotfix"}']);
    //productOnClient_updateObject(<ProductOnClient clientId 'win7x64.vmnat.local', productId 'mshotfix', installationStatus 'unknown', actionRequest 'setup'>) (JsonRpc.py|123)
    MyOpsiMethodCall('productOnClient_updateObject', ['{"type":"ProductOnClient","productType":"LocalbootProduct","clientId":"'+myclientid+'","productId":"mshotfix","installationStatus":"unknown","actionRequest":"setup"}']);
    //setProductActionRequest(u'mshotfix', u'win7x64.vmnat.local', u'update') (JsonRpc.py|123)
    MyOpsiMethodCall('setProductActionRequest', ['mshotfix',myclientid,'update']);
    //productOnClient_updateObject(<ProductOnClient clientId 'win7x64.vmnat.local', productId 'mshotfix', installationStatus 'installed', actionRequest 'none'>) (JsonRpc.py|123)
    MyOpsiMethodCall('productOnClient_updateObject', ['{"type":"ProductOnClient","productType":"LocalbootProduct","clientId":"'+myclientid+'","productId":"mshotfix","installationStatus":"installed","actionRequest":"none"}']);
    //writeLog(u'instlog', u'\n[5] [02.02.2012 15:42:33] --\n[5] [02.02.2012 15:42:33] --\n[1] [02.02.2012 15:42:33] \n[1] [02.02.2012 15:42:33] \n[1] [02.02.2012 15:42:33] ======= APPEND   02.02.2012 15:42:33\n[1] ...) (JsonRpc.py|123)
    opsidata.sendLog;
    //backend_exit() (JsonRpc.py|123)
    MyOpsiMethodCall('backend_exit', []);
    *)
      //opsi-winst part finished
      LogDatei.DependentAdd('Finished opsi-winst part:', LLNotice);
      MyOpsiMethodCall('accessControl_authenticated', []);
      MyOpsiMethodCall('backend_setOptions', ['{"addConfigStateDefaults":True}']);
      MyOpsiMethodCall('configState_getObjects', ['', '{"objectId":"' + myclientid + '"}']);
      // finishOpsiConf contains logwrite and backend_exit
      opsidata.finishOpsiConf;
      // all finished
      LogDatei.DependentAdd('opsidata finished', LLNotice);
    end
    else
    begin
      writeln('init failed');
      LogDatei.DependentAdd('init failed', LLError);
    end;

    *)
    // stop program loop
    Terminate;
  end;

  constructor Topsicasimp.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor Topsicasimp.Destroy;
  begin
    inherited Destroy;
  end;

  procedure Topsicasimp.WriteHelp;
  var
    filename: string;
  begin
    filename := ExtractFileName(ParamStr(0));
    writeln(ParamStr(0));
    writeln(filename);
    writeln(myerror);
    writeln(Title);
    writeln('Version: ' + myversion);
    writeln('Usage:');
    //writeln(filename+ ' --conffile=<path\filename> [--clientid=<clientid to use>] [--ip=<ip to use>]');
    writeln(filename +
      ' --service_url=<service_url> --service_pass=<service_pass>  --clientid=<clientid to use> [--service_user=<service_user>] [--ip=<ip to use>] [--webdav]');
    writeln('Options:');
    writeln(' --help -> write this help and exit');
    //writeln(' --conffile=<path\filename> -> file with configuration');
    writeln(' --service_url=<service_url> -> service_url to use ');
    writeln(' --service_pass=<service_pass> -> service_pass to use ');
    writeln(' --clientid=<client id> -> id to use');
    writeln(' --service_user=<service_user> -> service_user to use (optional: default = clientid)');
    writeln(' --ip=<interface> -> ip to use (optional: default = std ip)');
    writeln(' --debug_level=<debug_level> -> debug_level to use (optional: default = 8)');
    writeln(' --mode=<local/net> -> mode to simulate (optional: default = local)');
    writeln(' --webdav -> load files via https/webdav (optional)');
    writeln(' --mac=<mac-sddress> -> mac address for net mode (optional: default = 00:11:22:33:44:55)');
    Terminate;
    halt(-1);
    Exit;
  end;


var
  Application: Topsicasimp;

{$R *.res}

begin
  Application := Topsicasimp.Create(nil);
  Application.Title:='opsi-netboot-simulator';
  Application.Run;
  Application.Free;
end.
