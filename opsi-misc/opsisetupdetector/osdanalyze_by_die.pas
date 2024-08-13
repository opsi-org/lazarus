unit osdanalyze_by_die;

{$mode Delphi}

interface

uses
  Classes, SysUtils,
  osdbasedata,
  osd_detect_it_easy,
  oslog,
  osjson;

function analyze_by_die(myfilename: string; var mysetup: TSetupFile): TKnownInstaller;

implementation

{$IFDEF OSDGUI}
uses
  osdform,
  osdmain;
{$ELSE OSDGUI}
uses
  osdmain;
{$ENDIF OSDGUI}

function die_installer_name_to_osd(valuestr: string): TKnownInstaller;
begin
  if valuestr = 'Inno Setup Module' then Result := stInno;
  //else if
end;

function analyze_by_die(myfilename: string; var mysetup: TSetupFile): TKnownInstaller;
var
  i: integer;
  dieDetectList: TStringList;
  dieInfoList: TStringList;
  JSONString, JSONArrayElement, valuestr: string;
  arraycount, arrayindex: integer;
begin
  try
    try
      Result := stUnknown;

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
              if not jsonAsObjectGetValueByKey(JSONString, 'values', JSONString) then
                LogDatei.log('Got no values object as expeted from die', LLerror)
              else
              begin
                arraycount := jsonAsArrayCountElements(JSONString);
                for arrayindex := 0 to arraycount - 1 do
                begin
                  jsonAsArrayGetElementByIndex(JSONString, arrayindex, JSONArrayElement);
                  jsonAsObjectGetValueByKey(JSONArrayElement, 'type', valuestr);
                  if valuestr = 'Installer' then
                  begin
                    // we found an installer
                    // get the installer name
                    jsonAsObjectGetValueByKey(JSONArrayElement, 'name', valuestr);
                    mysetup.installerId := die_installer_name_to_osd(valuestr);
                    // get the installer version
                    jsonAsObjectGetValueByKey(JSONArrayElement, 'version', valuestr);
                    mysetup.installerVersion := valuestr;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;


      // get info
      write_log_and_memo('Get info with "detect it easy": ' + myfilename);
      if not runDieInfo(myfilename, dieInfoList) then
        write_log_and_memo('Failed to getinfo: ' + myfilename);
      write_memo(dieInfoList.Text);


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

