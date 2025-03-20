unit osd_lessmsi;

{$mode Delphi}

interface

uses
  Classes, SysUtils,
  oslog,
  LCLType,
  OSProcessux,
  {$IFDEF WINDOWS}
  Windows,
  dsiwin32,
  {$ENDIF WINDOWS}
  strutils,
  lazutf8;

{$IFNDEF DARWIN}
function getInstallDirFromMsi(target: string): string;
function readMsiTable(target: string; table: string; var Outlist: TStringList): boolean;
{$ENDIF DARWIN}

implementation

var
  myLessMsiOutList: TStringList;
  resultStr: string;
  myLessmsi: string;
  //myLessmsiParams: string;
  myLessmsiDir: string;


{$IFDEF LINUX}
function readMsiTable(target: string; table: string;
  var Outlist: TStringList): boolean;
var
  mycommand: string;
  myexitcode: integer = 0;
  myreport: string = '';
  i: integer;
begin
  Result := False;
  //mycommand := '"' + myLessmsi + '" l -t ' + table + ' "' + target + '"';
  mycommand := 'bash -c ''msiinfo export "' + target + '" ' + table + '''';
  LogDatei.log('Start: ' + mycommand, LLinfo);
  //if not FileExists(myLessmsi) then
  //  LogDatei.log('file not found: ' + myLessmsi, LLerror);
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
    // replace tabs by comma in list
    for i := 0 to Outlist.Count - 1 do
      Outlist[i] := Outlist[i].Replace(#9, ',');
    LogDatei.log('msiinfo output by exitcode: ' + IntToStr(myexitcode), LLdebug);
    LogDatei.log_list(Outlist, LLdebug);
  end;
end;
{$ENDIF LINUX}

{$IFDEF WINDOWS}
function readMsiTable(target: string; table: string; var Outlist: TStringList): boolean;
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
    LogDatei.log('lessmsi output by exitcode: ' + IntToStr(myexitcode), LLdebug);
    LogDatei.log_list(Outlist, LLdebug);
  end;
end;
{$ENDIF WINDOWS}


function getInstallDirFromMsi(target: string): string;
var
  instDirMarker, instDirBase, instDirProd, instDirFull: string;
  instDirMarkerLine, dirToken: string;
  resultList: TStringList;
  resultstr: string;
  InstDirMarkerList: TStringList;
  dirTokenList: TStringList;
  instDirMarkerFound: boolean = False;
  entryFound: boolean = False;
  myProductName, myManufacturer, myAi_package_type: string;
  i, k: integer;
  recursionCounter: integer = 0;

  function resolveDirectoryToken(var tokenlist: TStringList;
  var recursionCounter: integer): boolean;
  var
    instDirMarkerFound: boolean = False;
    instDirMarkerLine: string;
    debugstr: string;
    dirToken: string;
    i: integer;
    tokenListIndex: integer = 0;
    localTokenListIndex: integer = 0;
    resultList: TStringList;
    localTokenList: TStringList;
  begin
    i := 0;
    resultList := TStringList.Create;
    localTokenList := TStringList.Create;
    try
      localTokenList.Text := tokenlist.Text;
      Result := False;
      for localTokenListIndex := 0 to localTokenList.Count - 1 do
      begin
        dirToken := localTokenList[localTokenListIndex];
        tokenListIndex := tokenlist.IndexOf(dirToken);
        LogDatei.log('resolveDirectoryToken token: ' +
          IntToStr(localTokenListIndex) + ' = ' + dirToken, LLdebug2);

        instDirMarkerFound := False;
        instDirMarkerLine := '';
        if myLessMsiOutList.Count > 0 then
        begin
          // we expect the scheme:
          // instDirMarker, instDirBase, instDirProd
          // eg.
          // INSTALLDIR, ProgramFiles64Folder, grepWin
          while not instDirMarkerFound and (i < myLessMsiOutList.Count) do
          begin
            instDirMarkerLine := myLessMsiOutList[i];
            resultList.Clear;
            resultList.AddStrings(instDirMarkerLine.Split(','));
            //if myLessMsiOutList[i].StartsWith(dirToken, False) then
            if resultlist[0] = dirToken then
            begin
              instDirMarkerFound := True;
              Result := True;
              Inc(recursionCounter);
            end
            else
              Inc(i);
          end;
        end;
        if instDirMarkerFound then
        begin
          LogDatei.log('resolveDirectoryToken line: ' + instDirMarkerLine, LLdebug2);
          debugstr := resultList.Text;
          if resultList.Count > 0 then
            instDirMarker := resultlist[0];
          if resultList.Count > 1 then
            instDirBase := resultlist[1];
          if resultList.Count > 2 then
            instDirProd := resultlist[2];

          if not ((instDirProd = '') or (instDirProd = '.')) then
          begin
            if instDirProd.Contains('|') then
            begin
              // we reuse resultlist
              resultList.Clear;
              resultList.AddStrings(instDirProd.Split('|'));
              instDirProd := resultlist[1];
            end;
            tokenlist[tokenListIndex] := instDirProd;
          end;


          if not ((instDirBase = '') or (instDirBase = '.')) then
          begin
            if instDirBase.Contains('|') then
            begin
              // we reuse resultlist
              resultList.Clear;
              resultList.AddStrings(instDirBase.Split('|'));
              instDirBase := resultlist[1];
            end;
            // translate msi system folder to opsi system folder
            // https://learn.microsoft.com/de-de/windows/win32/msi/property-reference#system-folder-properties
            instDirBase := instDirBase.Replace('ProgramFilesFolder',
              '%ProgramFiles32Dir%');
            instDirBase := instDirBase.Replace('ProgramFiles32Folder',
              '%ProgramFiles32Dir%');
            instDirBase := instDirBase.Replace('ProgramFiles64Folder',
              '%ProgramFiles64Dir%');
            instDirBase := instDirBase.Replace('CommonFilesFolder',
              '%ProgramFiles32Dir%\Common Files');


            if instDirProd = '.' then
              tokenlist[tokenListIndex] := instDirBase
            else
              tokenlist.Insert(tokenListIndex, instDirBase);
          end;
          LogDatei.log('resolveDirectoryToken result: ' + tokenlist.Text, LLdebug2);
        end;
      end;
      LogDatei.log('resolveDirectoryToken result____: ' + tokenlist.Text, LLdebug2);

    finally
      FreeAndNil(resultList);
      FreeAndNil(localTokenList);
    end;
  end;

begin
  LogDatei.log('Try to get Installdir from msi via lessmsi.', LLnotice);
  Result := 'unknown';
  resultList := TStringList.Create;
  InstDirMarkerList := TStringList.Create;
  dirTokenList := TStringList.Create;
  try
    // Get some data from 'Property'
    myLessMsiOutList.Clear;
    readMsiTable(target, 'Property', myLessMsiOutList);
    resultstr := myLessMsiOutList.Text;
    for i := 0 to myLessMsiOutList.Count - 1 do
    begin
      if myLessMsiOutList[i].StartsWith('ProductName', True) then
      begin
        resultList.Clear;
        resultstr := myLessMsiOutList[i];
        resultList.AddStrings(resultstr.Split(','));
        myProductName := resultlist[1];
      end;
      if myLessMsiOutList[i].StartsWith('Manufacturer', True) then
      begin
        resultstr := myLessMsiOutList[i];
        resultList.Clear;
        resultList.AddStrings(resultstr.Split(','));
        myManufacturer := resultlist[1];
      end;
      if myLessMsiOutList[i].StartsWith('AI_PACKAGE_TYPE', True) then
      begin
        resultList.Clear;
        resultstr := myLessMsiOutList[i];
        resultList.AddStrings(resultstr.Split(','));
        myAi_package_type := resultlist[1];
      end;
    end;


    // Get instdir from 'Directory'
    myLessMsiOutList.Clear;
    // get content from directory table
    readMsiTable(target, 'Directory', myLessMsiOutList);
    resultstr := myLessMsiOutList.Text;
    // initialize well known master search strings
    InstDirMarkerList.Add('INSTALLDIR');
    InstDirMarkerList.Add('APPLICATIONROOTDIRECTORY');
    InstDirMarkerList.Add('APPLICATIONFOLDER');
    InstDirMarkerList.Add('INSTALLFOLDER');
    InstDirMarkerList.Add('INSTALLLOCATION');

    k := 0;
    while not instDirMarkerFound and (k < InstDirMarkerList.Count) do
    begin
      dirTokenList.Clear;
      instDirMarker := InstDirMarkerList[k];
      dirTokenList.Add(instDirMarker);
      repeat
        entryFound := resolveDirectoryToken(dirTokenList, recursionCounter);
      until not entryFound;
      if (recursionCounter > 0) then
      begin
        instDirMarkerFound := True;
        Result := '';
        for i := 0 to dirTokenList.Count - 1 do
        begin
          dirToken := dirTokenList[i];
          Result := Result + IncludeTrailingPathDelimiter(dirToken);
        end;
        LogDatei.log('Got Installdir from msi via lessmsi: ' + Result, LLnotice);
      end;
      Inc(k);
    end;

    if not instDirMarkerFound then
    begin
      // Get instdir from 'CustomAction'  SET_APPDIR
      LogDatei.log('Got no Installdir from msi table directory.', LLWarning);
      myLessMsiOutList.Clear;
      readMsiTable(target, 'CustomAction', myLessMsiOutList);
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
            resultstr := myLessMsiOutList[i];
            instDirMarkerLine := resultstr;
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
        instDirFull := instDirFull.Replace('[Manufacturer]', myManufacturer);
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
          instDirFull := instDirFull.Replace('[Manufacturer]', myManufacturer);
          instDirFull := instDirFull.Replace('[ProgramFilesFolder]',
            '%ProgramFiles32Dir%\');
          instDirFull := instDirFull.Replace('[ProgramFiles32Folder]',
            '%ProgramFiles32Dir%\');
          instDirFull := instDirFull.Replace('[ProgramFiles64Folder]',
            '%ProgramFiles64Dir%\');
          LogDatei.log('Installdir is now: ' + instDirFull, LLinfo);
          Result := instDirFull;
          LogDatei.log('Got Installdir from msi via lessmsi: ' + Result, LLnotice);
        end
        else
          LogDatei.log(
            'Got no Installdir from msi table CustomAction. / DIRCA_TARGETDIR',
            LLWarning);
      end;
    end;

    if not instDirMarkerFound then
      LogDatei.log('Got no Installdir from msi via lessmsi.', LLwarning);

  finally
    FreeAndNil(resultlist);
    FreeAndNil(InstDirMarkerList);
    FreeAndNil(dirTokenList);
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
