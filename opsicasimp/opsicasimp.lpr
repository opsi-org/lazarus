program opsicasimp;

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
 // {$IFDEF LINUX}
  fileinfo,
  winpeimagereader, // {need this for reading exe info}
  elfreader, // {needed for reading ELF executables}
// {$ENDIF LINUX}
  superobject,
  inifiles,
  strutils,
  ocsnetboot,
  ocsglobal,
  ocshwaudit;

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
    LogDatei.log('Loading file: ' + myFilesName, LLDebug2);
    if opsidata.getFileFromDepot(myFilesName, True, myFiles) then
    begin
      if myFiles.Count > 0 then
      begin
        for i := 0 to myFiles.Count - 1 do
        begin
          LogDatei.log('Files[' + IntToStr(i) + '] : ' + myFiles.Strings[i], LLDebug2);
          if pos('f', myFiles.Strings[i]) = 1 then
          begin
            myFilesName := copy(myFiles.Strings[i], 4, length(myFiles.Strings[i]));
            myFilesName := copy(myFilesName, 0, pos('''', myFilesName) - 1);
            myFilesName := AnsiToUtf8(davProductId + '/' + myFilesName);
            myFilesName := StringReplace(myFilesName, ' ', '%20',
              [rfIgnoreCase, rfReplaceAll]);
            LogDatei.log('downloading file : ' + myFilesName, LLDebug2);
            if opsidata.getFileFromDepot(myFilesName, False, myDummyList) then
              LogDatei.log('file download successful : ' + myFilesName, LLDebug2)
            else
              LogDatei.log('file download failed : ' + myFilesName, LLWarning);
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
    LogDatei.log('ProcessNonZeroScript opsidata initialized', LLdebug2);
    Verfahren := opsidata.getProductAction;

    if Verfahren in [tacDeinstall, tacSetup, tacOnce, tacAlways,
      tacCustom, tacLogin] then
    begin
      scriptname := opsidata.getProductScriptPath(Verfahren);
      LogDatei.log('scriptname: "' + scriptname + '", special path: "' +
        pfad + '"', LLnotice);
      //absscriptname := makeAbsoluteScriptPath(Pfad, scriptname);
      if Verfahren = tacSetup then
        opsidata.setProductState(tpsInstalling);
    end;
    LogDatei.log('First ProcessNonZeroScript finished', LLDebug2);
    //LogDatei.log('Setup script name: '+opsidata.getProductScriptPath(tacSetup), LLessential);


    if (Verfahren = tacUpdate) or ((Verfahren = tacSetup)) then
      //the setup script does not require a direct Reboot or Logout, and seems to be finished right now
      if extremeErrorLevel = LevelFatal then
      begin
        LogDatei.log(
          'We do not look for a update script, because the setup script is failed', LLnotice);
      end
      else
      begin
        opsidata.setProductActionRequest(tapUpdate);
        //LogDatei.log('product "' + Produkt + ' set to update', LLessential);
        LogDatei.log('get Update script name ...', LLdebug2);
        scriptname := opsidata.getProductScriptPath(tacUpdate);
        LogDatei.log('Update script name: ' + absscriptname, LLessential);
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

  function ChangeProductstatusOnReinst(const Bootmode: string): boolean;
  var
    i: integer;
    productState: TProductState;
    productActionRequest: TActionRequest;
  begin
    Result := False;
    LogDatei.LogProduktId:= False;
    LogDatei.log('bootmode ' + bootmode, LLInfo);
    if Bootmode = 'REINS' then
    begin
      if opsidata.getOpsiServiceVersion = '4' then
      begin
        LogDatei.log('telling server to look for dependent products',
          LLessential);
        if not TOpsi4Data(opsidata).setAddDependentProductOnClients(True) then
          LogDatei.log('failed telling server to look for dependent products',
            LLerror);
      end;
      LogDatei.log('setting all on-products to setup', LLessential);
      for i := 1 to Produkte.Count do
      begin
        Produkt := Produkte.Strings[i - 1];
        LogDatei.log('inspecting product "' + Produkt, LLessential);

        opsidata.setActualProductName(Produkt);

        productState := opsidata.getProductState;
        productActionRequest := opsidata.getProductActionRequest;

        LogDatei.log('product "' + Produkt + '" has state ' +
          opsidata.stateToString(productState) + ', action request is ' +
          opsidata.actionRequestToString(productActionRequest)
          , LLessential);

        //if (produkt <> selfProductName) and (produkt <> 'opsi-winst') then
        if (produkt <> 'opsi-winst') then
        begin
          if (productState in [tpsInstalled, tpsInstalling, tpsFailed]) then
          begin
            opsidata.setProductState(tpsNotInstalled);
            if productActionRequest in [tapNull, tapUpdate] then
            begin
              opsidata.setProductActionRequest(tapSetup);
              LogDatei.log('product "' + Produkt + '" set to setup',
                LLessential);
            end;
          end;
        end;
      end;
      if opsidata.getOpsiServiceVersion = '4' then
        if not TOpsi4Data(opsidata).setAddDependentProductOnClients(False) then
          LogDatei.log(
            'failed telling server to look not for dependent products',
            LLerror);

      opsidata.saveOpsiConf;
      // reload the new productlist
      Produkte := OpsiData.getListOfProducts;
    end;
    Result := True;
    // no errors
  end;




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
      LogDatei.log('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~',
        LLnotice);
      if errorfound then
        LogDatei.log('Error in the conditions for the sequence of products, '
          + ' the sorted list of maximum length is:',
          LLnotice)
      else
        LogDatei.log('Resolved sequence of products (' +
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
          LogDatei.log(Zeile, LLnotice);
        end;
        Inc(i);
      end;
      LogDatei.log('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~', LLnotice);

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
      if ChangeProductstatusOnReinst('REINS') then ;
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
          LogDatei.log('product ' + IntToStr(i - 1) + ' is "" ', LLnotice);
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
      //LogDatei.log('Exception calling method: '+method+'  - with param: '+param, LLerror);
      LogDatei.log('Exception calling method: ' + method, LLerror);
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
      //LogDatei.log('Exception calling method: '+method+'  - with param: '+param, LLerror);
      LogDatei.log('Exception calling method: ' + method, LLerror);
    end;
  end;

  function initLogging(const clientname: string): boolean;
  begin
    Result := True;
    logdatei := TLogInfo.Create;
    //logfilename := ExtractFilePath(ParamStr(0)) + clientname + '.log';
    logfilename := clientname + '.log';
    logdatei.CreateTheLogfile(logfilename, False);
    logdatei.LogLevel := mydebug_level;
  end;


  function initConnection: boolean;
  begin
    Result := True;
    LogDatei.log('service_url=' + myservice_url, LLDebug2);
    LogDatei.log('service_user=' + myservice_user, LLDebug2);
    LogDatei.log('service_pass=' + myservice_pass, LLDebug2);
    LogDatei.log('clientid=' + myclientid, LLDebug2);
    LogDatei.log('ip=' + myip, LLDebug2);
    opsidata := TOpsi4Data.Create;
    LogDatei.log('opsidata created', LLDebug2);
    opsidata.setActualClient(myclientid);
    opsidata.initOpsiConf(myservice_url, myclientid, myservice_pass, '', myip, myport);
    LogDatei.log('opsidata initialized', LLDebug2);
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
    optionlist.Append('bootserver::');
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
    end;
    if not matchstr(mymode, ['net','local','both']) then
    begin
      mymode := 'net';
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
    end;
    if myclientid = '' then
    begin
      myerror := 'Error: No clientid given.';
      WriteHelp;
    end;
    writeln('clientid=' + myclientid);
    initLogging(myclientid);

    if mymode = 'local' then
    begin
      if HasOption('service_url') then
      begin
        myservice_url := trim(GetOptionValue('service_url'));
      end;
      if myservice_url = '' then
      begin
        myerror := 'Error: No service_url given.';
        LogDatei.log(myerror,LLCritical);
        WriteHelp;
      end;
      //writeln('service_url=' + myservice_url);

      if HasOption('service_user') then
      begin
        myservice_user := trim(GetOptionValue('service_user'));
      end;
      if myservice_user = '' then
      begin
        myservice_user := myclientid;
        myerror := 'Notice: No service_user given - using clientid: '+myclientid;
        LogDatei.log(myerror,LLNotice);
      end;
      //writeln('service_user=' + myservice_user);

      if HasOption('service_pass') then
      begin
        myservice_pass := trim(GetOptionValue('service_pass'));
      end;
      if myservice_pass = '' then
      begin
        myerror := 'Error: No service_pass given.';
        LogDatei.log(myerror,LLCritical);
        WriteHelp;
      end;
      //writeln('service_pass=' + myservice_pass);
    end; // mode = local


    if HasOption('ip') then
    begin
      myip := trim(GetOptionValue('ip'));
    end;
    if myip = '' then
    begin
      myerror := 'Error: No valid IP given.';
      LogDatei.log(myerror,LLCritical);
      WriteHelp;
    end;
    //writeln('ip=' + myip);
    LogDatei.log('ip=' + myip,LLNotice);

    if (mymode = 'net') or (mymode = 'both') then
    begin
      if HasOption('mac') then
      begin
        mymac := trim(GetOptionValue('mac'));
      end;
      if mymac = '' then
      begin
        mymac := '00:11:22:33:44:55';
        myerror := 'Notice: No mac given - using default (00:11:22:33:44:55)';
        LogDatei.log(myerror,LLNotice);
      end;
      //writeln('mac=' + mymac);
      LogDatei.log('mac=' + mymac,LLNotice);

      if HasOption('bootserver') then
      begin
        mynetbootserver := trim(GetOptionValue('bootserver'));
      end;
      if mynetbootserver = '' then
      begin
        mynetbootserver := '192.168.1.14';
        myerror := 'Notice: No bootserver given - using default (192.168.1.14)';
        LogDatei.log(myerror,LLNotice);
      end;
      //writeln('bootserver=' + mynetbootserver);
      LogDatei.log('bootserver=' + mynetbootserver,LLNotice);
    end;




    if HasOption('webdav') then
    begin
      mywebdav := True;
    end;
    writeln('webdav=' + BoolToStr(mywebdav, True));


    if (mymode = 'net') or (mymode = 'both') then
    begin
      // here come the net boot part
      doNetboot(mymac,mynetbootserver);
      if initConnection then
      begin
        writeln('init done');
        LogDatei.log('init done', LLNotice);
        LogDatei.log('Starting bootimage part:', LLNotice);
        MyOpsiMethodCall('backend_getInterface', []);
        resultstring := MyOpsiMethodCall2('configState_getClientToDepotserver',
          ['[]', '[' + myclientid + ']']);
        //licenseOnClient_getOrCreateObject(u'pctry4detlef.uib.local', None, u'win10-x64-1709', None)
        MyOpsiMethodCall('licenseOnClient_getOrCreateObject', [myclientid,'','',myproduct]);
        // user_getCredentials(u'pcpatch', u'pctry4detlef.uib.local')
        MyOpsiMethodCall('user_getCredentials', ['pcpatch', myclientid]);
        // productOnClient_updateObjects(<ProductOnClient(clientId=u'pctry4detlef.uib.local', productId=u'win10-x64-1709', installationStatus=u'unknown', actionRequest=None)>)
        MyOpsiMethodCall('productOnClient_updateObjects', ['[{"clientId":"'+myclientid+'","productId":"'+myproduct+'","installationStatus":"unknown","actionRequest":"None","type":"ProductOnClient","productType":"NetbootProduct","actionProgress":"installing","actionResult":""}]']);
        MyOpsiMethodCall('backend_info', []);
        MyOpsiMethodCall('backend_info', []);
        //auditHardware_getConfig(None)
        MyOpsiMethodCall('auditHardware_getConfig', []);
        hwauditstr:= ReplaceStr(hwauditstr,'###FQDN###',myclientid);
        //auditHardwareOnHost_setObsolete
        MyOpsiMethodCall('auditHardwareOnHost_setObsolete', [myclientid]);
        //auditHardwareOnHost_createObjects([<AuditHardwareOnHost(hostId=u'pctry4detlef.uib.local',
        //LogDatei.log('hwauditsr: '+hwauditstr,LLNotice);
        MyOpsiMethodCall('auditHardwareOnHost_createObjects', hwauditstr);
        //productOnClient_deleteObjects([<ProductOnClient(clientId=u'pctry4detlef.uib.local', productId=u'win10-x64-1709', installationStatus=u'unknown', actionRequest=u'setup')>])
        // getPcpatchPassword(u'pctry4detlef.uib.local')
        MyOpsiMethodCall('getPcpatchPassword', [myclientid]);
        // productOnClient_updateObjects(<ProductOnClient(clientId=u'pct
        MyOpsiMethodCall('productOnClient_updateObjects', ['[{"clientId":"'+myclientid+'","productId":"'+myproduct+'","installationStatus":"installed","actionRequest":"None","type":"ProductOnClient","productType":"NetbootProduct","actionProgress":"","actionResult":"successful","lastAction":"setup"}]']);
        if (mymode = 'net') then
        begin
          // finishOpsiConf contains logwrite and backend_exit
          opsidata.finishOpsiConf;
        end;
        // all finished
        LogDatei.log('opsidata finished', LLNotice);
      end
      else
      begin
        writeln('init failed');
        LogDatei.log('init failed', LLError);
      end;
    end;  // end net boot part


    if (mymode = 'local') or (mymode = 'both') then
    begin
      // here come the local boot part
      if initConnection then
      begin
        writeln('init done');
        LogDatei.log('init done', LLNotice);
        LogDatei.log('Starting opsiclientd part:', LLNotice);
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
        LogDatei.log('myDepotId: ' + myDepotId, LLDebug2);
        //host_getObjects([], {'type': u'OpsiDepotserver', 'id': [u'sepiolina.vmnat.local']}) (JsonRpc.py|123)
        MyOpsiMethodCall('host_getObjects',
          ['', '{"type":"OpsiDepotserver","id":["' + myDepotId + '"]}']);
        //user_getCredentials(u'pcpatch', u'win7x64.vmnat.local') (JsonRpc.py|123)
        MyOpsiMethodCall('user_getCredentials', ['pcpatch', myclientid]);
        MyOpsiMethodCall('user_getCredentials', ['pcpatch', myclientid]);
        MyOpsiMethodCall('configState_getObjects', ['', '{"objectId":"' + myclientid + '"}']);
        MyOpsiMethodCall('configState_getObjects', ['', '{"objectId":"' + myclientid + '"}']);
        LogDatei.log('End opsiclientd part:', LLNotice);

        LogDatei.log('Starting opsi-winst part:', LLNotice);
        MyOpsiMethodCall('backend_info', []);
        //getDepotId(u'win7x64.vmnat.local') (JsonRpc.py|123)
        MyOpsiMethodCall('getDepotId', [myclientid]);
        BuildPC;
        //opsi-winst part finished
        LogDatei.log('Finished opsi-winst part:', LLNotice);
        MyOpsiMethodCall('accessControl_authenticated', []);
        MyOpsiMethodCall('backend_setOptions', ['{"addConfigStateDefaults":True}']);
        MyOpsiMethodCall('configState_getObjects', ['', '{"objectId":"' + myclientid + '"}']);
        // finishOpsiConf contains logwrite and backend_exit
        opsidata.finishOpsiConf;
        // all finished
        LogDatei.log('opsidata finished', LLNotice);
      end
      else
      begin
        writeln('init failed');
        LogDatei.log('init failed', LLError);
      end;
    end; // local boot part


    // stop program loop
    LogDatei.Close;
    LogDatei.Free;
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
    writeln(' --bootserver=<ip-sddress> -> ip address for net mode (optional: default = 192.168.1.14)');
    if Assigned(LogDatei) then LogDatei.Close;
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
