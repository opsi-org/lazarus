unit osdanalyze_by_die;

{$mode Delphi}

interface

uses
  Classes, SysUtils,
  strutils,
  osdbasedata,
  osd_detect_it_easy,
  oslog,
  osjson;

procedure analyze_by_die(myfilename: string; var mysetup: TSetupFile);

implementation

uses
  {$IFDEF OSDGUI}
  osdform,
  {$ENDIF OSDGUI}
  osdmain;

function die_installer_name_to_osd(valuestr: string): TKnownInstaller;
begin
  Result := stUnknown;
  if valuestr = 'Inno Setup Module' then Result := stInno
  else if valuestr = 'Install4j Installer' then Result := stInstall4J
  else if valuestr = 'InstallAnywhere' then Result := stInstallAnywhere
  else if valuestr = 'Microsoft dotNET installer' then Result := stDetectedUnknown
  else if valuestr = '7-Zip' then Result := st7zip
  else if valuestr = 'InstallShield' then Result := stInstallShield
  else if valuestr = 'Microsoft Installer(MSI)' then Result := stMsi
  else if valuestr = 'WiX Toolset installer' then Result := stWixToolset
  else if valuestr = 'Nullsoft Scriptable Install System' then Result := stNsis
  else if valuestr = 'QT installer' then Result := stQtInstaller
  else if valuestr = 'Setup Factory' then Result := stSetupFactory
  else if valuestr = 'Squirrel Installer' then Result := stDetectedUnknown
  else if valuestr = 'Windows Installer' then Result := stWixToolset
  else if valuestr = 'Wise Installer' then Result := stWise
  else if valuestr = '' then Result := stUnknown
  else
    Result := stDetectedUnknown;
end;

procedure analyze_by_die(myfilename: string; var mysetup: TSetupFile);
var
  dieDetectList: TStringList;
  dieInfoList: TStringList;
  JSONString, JSONArrayElement, valuestr, filetype: string;
  installerstr: string;
  arraycount, arrayindex: integer;
  previousInstallerId: TKnownInstaller;
begin
  try
    try
      dieDetectList := TStringList.Create;
      dieInfoList := TStringList.Create;

      // get detection
      write_log_and_memo('Detect with "detect it easy": ' + myfilename);
      if not runDieDetect(myfilename, dieDetectList) then
        write_log_and_memo('Failed to detect: ' + myfilename);
      write_memo(dieDetectList.Text);
      // parse result
      JSONString := dieDetectList.Text;
      if not jsonIsValid(JSONString) then
      begin
        LogDatei.log('Got invalid json from die', LLerror);
      end
      else
      begin
        if not jsonAsObjectGetValueByKey(JSONString, 'detects', JSONString) then
          LogDatei.log('Got no detects object from die', LLerror)
        else
        begin
          if not jsonIsArray(JSONString) then
            LogDatei.log('Got no array as expeted from die', LLerror)
          else
          begin
            if not jsonAsArrayGetElementByIndex(JSONString, 0, JSONString) then
              LogDatei.log('Got no array[0] element as expeted from die', LLerror)
            else
            begin
              if not jsonAsObjectGetValueByKey(JSONString, 'filetype', filetype) then
                LogDatei.log('Got no filetype object as expeted from die', LLerror)
              else
              begin
                if (filetype = 'ZIP') then
                begin
                  write_log_and_memo('Detected by die: filetype = ZIP');
                end
                else if (filetype = 'PE32') or (filetype = 'PE64') or
                  (filetype = 'Binary') then
                begin
                  if not jsonAsObjectGetValueByKey(JSONString, 'values', JSONString) then
                    LogDatei.log('Got no values object as expeted from die', LLerror)
                  else
                  begin
                    arraycount := jsonAsArrayCountElements(JSONString);
                    for arrayindex := 0 to arraycount - 1 do
                    begin
                      jsonAsArrayGetElementByIndex(JSONString,
                        arrayindex, JSONArrayElement);
                      jsonAsObjectGetValueByKey(JSONArrayElement, 'type', valuestr);
                      if valuestr = 'Installer' then
                      begin
                        // we found an installer
                        // get the installer name
                        jsonAsObjectGetValueByKey(JSONArrayElement, 'name', valuestr);
                        // do not accept some installer types
                        if not StartsText('SQX Archive Installer', valuestr) then
                        begin
                          previousInstallerId := mysetup.installerId;
                          installerstr := valuestr;
                          mysetup.installerId := die_installer_name_to_osd(installerstr);
                          mysetup.installerName := installerstr;
                          // get the installer version
                          jsonAsObjectGetValueByKey(JSONArrayElement,
                            'version', valuestr);
                          mysetup.installerVersion := valuestr;
                          write_log_and_memo('Detected by die: ' +
                            installerstr + ' version: ' + valuestr);
                          //LogDatei.log('Detected by die: ' + installerstr + ' version: ' + valuestr, LLnotice);

                          // special handlings
                          // installshieldMSI
                          if (previousInstallerId = stInstallShieldMSI) and
                            (installerstr = 'InstallShield') then
                            mysetup.installerId := previousInstallerId;
                          // installaware
                          if (previousInstallerId = stInstallAware) then
                            mysetup.installerId := previousInstallerId;


                          if (previousInstallerId <> stUnknown) and
                            (previousInstallerId <> mysetup.installerId) then
                            if (mysetup.installerId = stUnknown) or
                              (mysetup.installerId = stDetectedUnknown) then
                            begin
                              LogDatei.log('Fall back to previous detection: ', LLInfo);
                              mysetup.installerId := previousInstallerId;
                            end
                            else
                            begin
                              LogDatei.log('We have multiple detections: ', LLwarning);
                              LogDatei.log('Previous: ' + installerToInstallerstr(
                                previousInstallerId), LLwarning);
                              LogDatei.log('Now: ' + installerToInstallerstr(
                                mysetup.installerId), LLwarning);
                            end;
                        end;
                      end
                      else if valuestr = 'Sfx' then
                      begin
                        // we found an 7zip Sfx installer
                        // get the installer name
                        jsonAsObjectGetValueByKey(JSONArrayElement, 'string', valuestr);
                        if valuestr = 'Sfx: 7-Zip' then mysetup.installerId := st7zipsfx;
                        // get the installer version
                        jsonAsObjectGetValueByKey(JSONArrayElement, 'version', valuestr);
                        mysetup.installerVersion := valuestr;
                      end
                      else if valuestr = 'Archive' then
                      begin
                        // we found an Archive
                        // get the installer name
                        jsonAsObjectGetValueByKey(JSONArrayElement, 'name', valuestr);
                        if valuestr = 'Debian Software package (.DEB)' then
                          mysetup.installerId := stLinDeb
                        else if valuestr = 'RPM package' then
                          mysetup.installerId := stLinRPM;
                        // get the installer version
                        jsonAsObjectGetValueByKey(JSONArrayElement, 'version', valuestr);
                        mysetup.installerVersion := valuestr;
                      end;
                      // we have an 64 bit installer
                      if filetype = 'PE64' then mysetup.architecture := a64;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;


      // get info
      //write_log_and_memo('Get info with "detect it easy": ' + myfilename);
      logdatei.log('Get info with "detect it easy": ' + myfilename, LLinfo);
      if not runDieInfo(myfilename, dieInfoList) then
        logdatei.log('Failed to getinfo: ' + myfilename, LLwarning);
      logdatei.log_list(dieInfoList, LLdebug);


    except
      on E: Exception do
      begin
        LogDatei.log('Exception in analyze_by_die', LLcritical);
        LogDatei.log('Error: Message: ' + E.message, LLcritical);
        system.ExitCode := 1;
      end;
    end;
  finally
    FreeAndNil(dieDetectList);
    FreeAndNil(dieInfoList);
  end;
end;


end.
