unit osd_lessmsi;

{$mode Delphi}

interface

uses
  Classes, SysUtils,
  //osdhelper,
  //osprocesses,
  oslog,
  LCLType,
  OSProcessux,
  Windows,
  lazutf8,
  dsiwin32;

function getInstallDirFromMsi(target: string): string;
function runLessmsiTable(target: string; table: string;
  var Outlist: TStringList): boolean;
//function runDieInfo(target : string; jsonOut : TStringlist) : boolean;


implementation

var
  myLessMsiOutList: TStringList;
  resultStr: string;
  myLessmsi: string;
  myLessmsiParams: string;
  myLessmsiDir: string;


function runLessmsiTable(target: string; table: string;
  var Outlist: TStringList): boolean;
var
  mycommand: string;
  myexitcode: integer = 0;
  myreport: string = '';
begin
  Result := False;
  mycommand := '"' + myLessmsi + '" l -t ' + table + ' "' + target + '"';
  LogDatei.log('Start: ' + mycommand, LLinfo);
  if not FileExists(myLessmsi) then
    LogDatei.log('file not found: ' + myLessmsi, LLerror);
  if not RunCommandAndCaptureOut(mycommand, True, Outlist, myreport,
    SW_SHOWDEFAULT, myexitcode, True, 0) then
  begin
    LogDatei.log('Failed to analyze: ' + myreport, LLerror);
    //write_log_and_memo('Failed to analyze: ' + myreport);
    //mysetup.analyze_progess := 0;
  end
  else
  begin
    Result := True;
    LogDatei.log('lessmsi output by exitcode: ' + IntToStr(myexitcode), LLinfo);
    LogDatei.log_list(Outlist, LLdebug);
  end;
end;

(*
function runDieInfo(target : string; jsonOut : TStringlist) : boolean;
var
  mycommand : string;
  myexitcode : integer = 0;
  myreport : string = '';
  //oldworkdir : string;
begin
  result := false;
  //mycommand := '"C:\WINDOWS\system32\cmd.exe" /C "'+myDiec +' --info '+myDiecParams + ' -b "'+ExtractFileName(target)+'""';
  //mycommand := '"'+myDiec +'" --info '+myDiecParams + ' -b "'+ExtractFileName(target)+'"';
  mycommand := '"'+myDiec +'" --info '+myDiecParams + ' "'+target+'"';
  LogDatei.log('Start: '+mycommand,LLinfo);
  //write_memo(mycommand);
  //oldworkdir := GetCurrentDir;
  //SetCurrentDir(ExtractFileDir(target));

  if not RunCommandAndCaptureOut(mycommand, True, jsonOut, myreport,
    SW_SHOWMINIMIZED, myexitcode) then
  begin
    LogDatei.log('Failed to get info: ' + myreport,LLerror);
    //write_memo('Failed to analyze: ' + myreport);
  end
  else
  begin
    result := true;
    LogDatei.log('Diec output: ',LLinfo);
    LogDatei.log_list(jsonOut,LLdebug);
  end;
  //SetCurrentDir(oldworkdir);
end;
  *)

function getInstallDirFromMsi(target: string): string;
var
  instDirMarker, instDirBase, instDirProd, instDirFull: string;
  instDirMarkerLine: string;
  resultList: TStringList;
  InstDirMarkerList: TStringList;
  instDirMarkerFound: boolean = False;
  myProductName, myManufacturer, myAi_package_type: string;
  i, k: integer;
begin
  Result := 'unknown';
  resultList := TStringList.Create;
  InstDirMarkerList := TStringList.Create;
  try
    // Get some data from 'Property'
    myLessMsiOutList.Clear;
    runLessmsiTable(target, 'Property', myLessMsiOutList);
    resultstr := myLessMsiOutList.Text;
    for i := 0 to myLessMsiOutList.Count - 1 do
    begin
      if myLessMsiOutList[i].StartsWith('ProductName', True) then
      begin
        resultList.Clear;
        resultList.AddStrings(myLessMsiOutList[i].Split(','));
        myProductName := resultlist[1];
      end;
      if myLessMsiOutList[i].StartsWith('Manufacturer', True) then
      begin
        resultList.Clear;
        resultList.AddStrings(myLessMsiOutList[i].Split(','));
        myManufacturer := resultlist[1];
      end;
      if myLessMsiOutList[i].StartsWith('AI_PACKAGE_TYPE', True) then
      begin
        resultList.Clear;
        resultList.AddStrings(myLessMsiOutList[i].Split(','));
        myAi_package_type := resultlist[1];
      end;
    end;


    // Get instdir from 'Directory'
    myLessMsiOutList.Clear;
    runLessmsiTable(target, 'Directory', myLessMsiOutList);
    resultstr := myLessMsiOutList.Text;
    InstDirMarkerList.Add('INSTALLDIR');
    InstDirMarkerList.Add('APPLICATIONROOTDIRECTORY');
    InstDirMarkerList.Add('APPLICATIONFOLDER');
    InstDirMarkerList.Add('INSTALLFOLDER');
    InstDirMarkerList.Add('INSTALLLOCATION');
    i := 0;
    if myLessMsiOutList.Count > 0 then
    begin
      // we expect the scheme:
      // instDirMarker, instDirBase, instDirProd
      // eg.
      // INSTALLDIR, ProgramFiles64Folder, grepWin
      while not instDirMarkerFound and (i < myLessMsiOutList.Count) do
      begin
        k := 0;
        LogDatei.log('Looking for instDirMarker in line: ' +
          myLessMsiOutList[i], LLinfo);
        while not instDirMarkerFound and (k < InstDirMarkerList.Count) do
        begin
          if myLessMsiOutList[i].StartsWith(InstDirMarkerList[k], True) then
          begin
            instDirMarkerFound := True;
            instDirMarkerLine := myLessMsiOutList[i];
          end
          else
            Inc(k);
        end;
        Inc(i);
      end;

      if instDirMarkerFound then
      begin
        // split line at comma
        resultList.Clear;
        resultList.AddStrings(instDirMarkerLine.Split(','));
        resultstr := resultList.Text;
        instDirMarker := resultlist[0];
        instDirBase := resultlist[1];
        instDirProd := resultlist[2];

        if instDirProd.Contains('|') then
        begin
          // we reuse resultlist
          resultList.Clear;
          resultList.AddStrings(instDirProd.Split('|'));
          instDirProd := resultlist[1];
        end;

        // translate msi system folder to opsi system folder
        // https://learn.microsoft.com/de-de/windows/win32/msi/property-reference#system-folder-properties
        if instDirBase = 'ProgramFilesFolder' then Result := '%ProgramFiles32Dir%\'
        else if instDirBase = 'ProgramFiles32Folder' then
          Result := '%ProgramFiles32Dir%\'
        else if instDirBase = 'ProgramFiles64Folder' then
          Result := '%ProgramFiles64Dir%\'
        else
        begin
          i := 0;
          instDirMarkerFound := False;
          if myLessMsiOutList.Count > 0 then
          begin
            // we expect the scheme:
            // SET_APPDIR,307,APPDIR,[AI_UserProgramFiles]\Iridium,-2147483648
            while not instDirMarkerFound and (i < myLessMsiOutList.Count) do
            begin
              if myLessMsiOutList[i].StartsWith(instDirBase, True) then
              begin
                instDirMarkerFound := True;
                instDirMarkerLine := myLessMsiOutList[i];
              end
              else
                Inc(i);
            end;
          end;
          if instDirMarkerFound then
          begin
            // split line at comma
            resultList.Clear;
            resultList.AddStrings(instDirMarkerLine.Split(','));
            resultstr := resultList.Text;
            instDirMarker := resultlist[0];
            instDirBase := resultlist[1];
            //instDirProd := resultlist[2];
            instDirBase := instDirBase.Replace('ProgramFilesFolder',
              '%ProgramFiles32Dir%');
            instDirBase := instDirBase.Replace('ProgramFiles32Folder',
              '%ProgramFiles32Dir%');
            instDirBase := instDirBase.Replace('ProgramFiles64Folder',
              '%ProgramFiles64Dir%');
            Result := instDirBase + '\' + resultlist[2] + '\';
          end;
        end;

        Result := Result + instDirProd;
        LogDatei.log('Got Installdir from msi via lessmsi: ' + Result, LLinfo);
      end
      else
      begin
        // Get instdir from 'CustomAction'  SET_APPDIR
        LogDatei.log('Got no Installdir from msi table directory.', LLWarning);
        myLessMsiOutList.Clear;
        runLessmsiTable(target, 'CustomAction', myLessMsiOutList);
        i := 0;
        if myLessMsiOutList.Count > 0 then
        begin
          // we expect the scheme:
          // SET_APPDIR,307,APPDIR,[AI_UserProgramFiles]\Iridium,-2147483648
          while not instDirMarkerFound and (i < myLessMsiOutList.Count) do
          begin
            if myLessMsiOutList[i].StartsWith('SET_APPDIR', True) then
            begin
              instDirMarkerFound := True;
              instDirMarkerLine := myLessMsiOutList[i];
            end
            else
              Inc(i);
          end;
        end;
        if instDirMarkerFound then
        begin
          LogDatei.log('Looking for instDirMarker in line: ' +
            instDirMarkerLine, LLinfo);
          resultList.Clear;
          resultList.AddStrings(instDirMarkerLine.Split(','));
          instDirFull := resultlist[3];
          instDirFull := instDirFull.Replace('[ProductName]', myProductName);
          instDirFull := instDirFull.Replace('[ProgramFilesFolder]',
            '%ProgramFiles32Dir%\');
          instDirFull := instDirFull.Replace('[ProgramFiles32Folder]',
            '%ProgramFiles32Dir%\');
          instDirFull := instDirFull.Replace('[ProgramFiles64Folder]',
            '%ProgramFiles64Dir%\');
          if myAi_package_type = 'Intel' then
            instDirFull := instDirFull.Replace('[AI_UserProgramFiles]',
              '%ProgramFiles32Dir%\');
          if myAi_package_type = 'x64' then
            instDirFull := instDirFull.Replace('[AI_UserProgramFiles]',
              '%ProgramFiles64Dir%\');
          LogDatei.log('Installdir is now: ' + instDirFull, LLinfo);
          // not completed:
          // instDirMarkerFound := False;
          Result := instDirFull;
          LogDatei.log('Got Installdir from msi via lessmsi: ' + Result, LLinfo);
        end
        else
        begin
          // Get instdir from 'CustomAction'  DIRCA_TARGETDIR
          LogDatei.log('Got no Installdir from msi table CustomAction. / SET_APPDIR',
            LLWarning);
          i := 0;
          // we expect the scheme:
          // DIRCA_TARGETDIR,307,TARGETDIR,[ProgramFilesFolder]\[ProductName]
          while not instDirMarkerFound and (i < myLessMsiOutList.Count) do
          begin
            if myLessMsiOutList[i].StartsWith('DIRCA_TARGETDIR', True) then
            begin
              instDirMarkerFound := True;
              instDirMarkerLine := myLessMsiOutList[i];
            end
            else
              Inc(i);
          end;
          if instDirMarkerFound then
          begin
            LogDatei.log('Looking for instDirMarker in line: ' +
              instDirMarkerLine, LLinfo);
            resultList.Clear;
            resultList.AddStrings(instDirMarkerLine.Split(','));
            instDirFull := resultlist[3];
            instDirFull := instDirFull.Replace('[ProductName]', myProductName);
            instDirFull := instDirFull.Replace('[ProgramFilesFolder]',
              '%ProgramFiles32Dir%');
            instDirFull := instDirFull.Replace('[ProgramFiles32Folder]',
              '%ProgramFiles32Dir%');
            instDirFull := instDirFull.Replace('[ProgramFiles64Folder]',
              '%ProgramFiles64Dir%');
            LogDatei.log('Installdir is now: ' + instDirFull, LLinfo);
            // not completed:
            //instDirMarkerFound := False;
            Result := instDirFull;
            LogDatei.log('Got Installdir from msi via lessmsi: ' + Result, LLinfo);
          end
          else
            LogDatei.log(
              'Got no Installdir from msi table CustomAction. / DIRCA_TARGETDIR',
              LLWarning);
        end;
      end;
    end;

    if not instDirMarkerFound then
      LogDatei.log('Got no Installdir from msi via lessmsi.', LLwarning);

  finally
    FreeAndNil(resultlist);
    FreeAndNil(InstDirMarkerList);
  end;
end;

initialization
  myLessMsiOutList := TStringList.Create;
  {$IFDEF WINDOWS}
  myLessmsiDir := ExtractFileDir(ParamStr(0)) + '\utils\lessmsi';
  myLessmsi := myLessmsiDir + '\lessmsi.exe';
  {$EndIF WINDOWS}

finalization
  FreeAndNil(myLessMsiOutList);

end.
