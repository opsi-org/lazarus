unit osdanalyzewin;

{$mode delphi}

interface

uses
  {$IFDEF WINDOWS}
  verinfo,
  jwawindows,
  {$ENDIF WINDOWS}
  {$IFNDEF DARWIN}
  osd_lessmsi,
  {$ENDIF DARWIN}
  Dialogs,
  LCLType,
  Classes,
  osdhelper,
  fileutil,
  lazfileutils,
  strutils,
  SysUtils,
  fileinfo,
  typinfo,
  winpeimagereader,
  oslog,
  osdbasedata,
  osdanalyzegeneral,
  oscheckbinarybitness,
  osmessagedialog,
  Controls,
  osd_md_html_dlg,
  oszip,
  osxml;

procedure get_aktProduct_general_info_win(installerId: TKnownInstaller;
  myfilename: string; var mysetup: TSetupFile);
procedure get_msi_info(myfilename: string; var mysetup: TSetupFile); overload;
procedure get_msi_info(myfilename: string; var mysetup: TSetupFile;
  uninstall_only: boolean); overload;
procedure get_inno_info(myfilename: string; var mysetup: TSetupFile);
//procedure get_installshield_info(myfilename: string; var mysetup: TSetupFile);
procedure get_installshieldmsi_info(myfilename: string; var mysetup: TSetupFile);
//procedure get_advancedmsi_info(myfilename: string; var mysetup: TSetupFile);
procedure get_null_info(myfilename: string; var mysetup: TSetupFile);
procedure get_installaware_info(myfilename: string; var mysetup: TSetupFile);
//procedure get_genmsinstaller_info(myfilename: string; var mysetup: TSetupFile);
procedure get_wixtoolset_info(myfilename: string; var mysetup: TSetupFile);
procedure get_boxstub_info(myfilename: string; var mysetup: TSetupFile);
procedure get_sfxcab_info(myfilename: string; var mysetup: TSetupFile);
//procedure get_bitrock_info(myfilename: string; var mysetup: TSetupFile);
//procedure get_selfextrackting_info(myfilename: string; var mysetup: TSetupFile);
procedure get_advancedInstaller_info(myfilename: string; var mysetup: TSetupFile);
procedure get_installAnywhere_info(myfilename: string; var mysetup: TSetupFile);
procedure get_QtInstaller_info(myfilename: string; var mysetup: TSetupFile);
procedure get_MsixAppx_info(myfilename: string; var mysetup: TSetupFile);
procedure get_winget_info(var mysetup: TSetupFile);
// marker for add installers
procedure Analyze(FileName: string; var mysetup: TSetupFile; verbose: boolean);
function getProductInfoFromResource(infokey: string; filename: string): string;

resourcestring
  sWarnMultipleMsi =
    'Multiple (more than one) msi files found. Check the log file and directory: ';

implementation

uses
  {$IFDEF OSDGUI}
  osdform,
  ChooseInstallerDlg,
  {$ENDIF OSDGUI}
  osdmain;


{$IFDEF WINDOWS}
procedure KillProcess(Name: string);
var
  h: tHandle;
  pe: tProcessEntry32;
  sPrcName: string;
begin
  LogDatei.log('Try to kill: ' + Name, LLinfo);
  h := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  try
    pe.dwSize := SizeOf(pe);
    if Process32First(h, pe) then
    begin
      while Process32Next(h, pe) do
      begin
        sPrcName := pe.szExeFile;
        LogDatei.log('Found process: ' + sPrcName, LLdebug);
        if pos(LowerCase(Name), LowerCase(sPrcName)) > 0 then
        begin
          LogDatei.log('Will kill: ' + sPrcName, LLinfo);
          if TerminateProcess(OpenProcess(Process_Terminate, False,
            pe.th32ProcessID), 0) then
            LogDatei.log('Successful killed: ' + sPrcName, LLnotice)
          else
            LogDatei.log('Kill failed for: ' + sPrcName, LLwarning);
        end;
      end;
    end
    else
      RaiseLastOSError;
  finally
  end;
end;
{$ENDIF WINDOWS}


function getProductInfoFromResource(infokey: string; filename: string): string;
{ Allowed keys:
  CompanyName
  FileDescription
  FileVersion
  InternalName
  LegalCopyright
  OriginalFilename
  ProductName
  ProductVersion
}
var
  FileVerInfo: TFileVersionInfo;
  {$IFDEF WINDOWS}
  VerInf: verinfo.TVersionInfo;
  {$ENDIF WINDOWS}
  extension: string;
begin
  extension := ExtractFileExt(filename);
  if extension = '.exe' then
  begin
    try
      FileVerInfo := TFileVersionInfo.Create(nil);
      try
        FileVerInfo.FileName := filename;
        FileVerInfo.ReadFileInfo;
        Result := FileVerInfo.VersionStrings.Values[infokey];
      except
        on E: Exception do
        begin
          LogDatei.log('Exception while reading fileversion1: ' +
            infokey + ' Exception: ' + E.ClassName + ': ' + E.Message, LLError);
          Result := '';
        end;
        else // close the 'on else' block here;
      end;
    finally
      FileVerInfo.Free;
    end;
    {$IFDEF WINDOWS}
    if Result = '' then
    begin
      try
        VerInf := verinfo.TVersionInfo.Create(filename);
        if infokey = 'FileVersion' then
          Result := trim(VerInf[CviFileVersion]);
        if infokey = 'ProductName' then
          Result := trim(VerInf[CviProductName]);
        VerInf.Free;
      except
        on E: Exception do
        begin
          LogDatei.log('Exception while reading fileversion2: ' +
            infokey + ' Exception: ' + E.ClassName + ': ' + E.Message, LLError);
          Result := '';
        end;
        else // close the 'on else' block here;
      end;
    end;
    {$ENDIF WINDOWS}
  end;
end;

procedure get_aktProduct_general_info_win(installerId: TKnownInstaller;
  myfilename: string; var mysetup: TSetupFile);
var
  i: integer;
  fsize: int64;
  fsizemb, rsizemb: double;
  sReqSize: string;
  sFileSize: string;
  myArch: string;
  product: string;
  installerstr: string;
  str1, uninstcheckstr: string;
begin
  installerstr := installerToInstallerstr(installerId);
  write_log_and_memo('Analyzing ' + installerstr + ' Setup: ' + myfilename);

  mysetup.installerId := installerId;
  mysetup.link := installerArray[integer(mysetup.installerId)].Link;
  //mysetup.setupFullFileName := myfilename;
  mysetup.SetSetupFullFileName(myfilename);
  mysetup.installerSourceDir := '%scriptpath%\files' + IntToStr(mysetup.ID);
  if mysetup.preferSilent then
    mysetup.installCommandLine :=
      '"$installerSourceDir$\' + mysetup.setupFileName + '" ' +
      installerArray[integer(mysetup.installerId)].silentsetup
  else
    mysetup.installCommandLine :=
      '"$installerSourceDir$\' + mysetup.setupFileName + '" ' +
      installerArray[integer(mysetup.installerId)].unattendedsetup;
  mysetup.isExitcodeFatalFunction :=
    installerArray[integer(mysetup.installerId)].uib_exitcode_function;
  mysetup.uninstallProg := installerArray[integer(mysetup.installerId)].uninstallProg;
  mysetup.uninstall_waitforprocess :=
    installerArray[integer(mysetup.installerId)].uninstall_waitforprocess;
  mysetup.install_waitforprocess :=
    installerArray[integer(mysetup.installerId)].install_waitforprocess;
  if installerID <> stMSI then
  begin
    str1 := getProductInfoFromResource('FileVersion', myfilename);
    mysetup.SoftwareVersion := str1;
    aktProduct.productdata.productversion := trim(mysetup.SoftwareVersion);
    str1 := getProductInfoFromResource('ProductName', myfilename);
  end;
  if str1 <> '' then
  begin
    aktProduct.productdata.productId := getPacketIDShort(str1);
    aktProduct.productdata.productName := str1;
  end
  else
  begin
    product := ExtractFileNameWithoutExt(mysetup.setupFileName);
    aktProduct.productdata.productId := getPacketIDShort(product);
    aktProduct.productdata.productName := product;
  end;

  fsize := fileutil.FileSize(myfilename);
  fsizemb := fsize / (1024 * 1024);
  rsizemb := fsizemb * 6;
  sFileSize := FormatFloat('##0.0', fsizemb) + ' MB';
  sReqSize := FormatFloat('###0', rsizemb) + ' MB';

  write_log_and_memo('Setup file size is: ' + sFileSize);
  write_log_and_memo('Estimated required space is: ' + sReqSize);
  write_log_and_memo('........');

  if fsizemb < 1 then
    fsizemb := 1;

  sFileSize := FormatFloat('#', fsizemb) + ' MB';
  sReqSize := FormatFloat('#', rsizemb) + ' MB';
  mysetup.requiredSpace := round(rsizemb);
  mysetup.setupFileSize := round(fsizemb);

  // uninstallcheck
  if installerId = stMsi then
  begin
    // will be done in  get_msi_info
  end
  else
  begin
    // will be done in analyze after get_*_info
  end;
  if installerArray[integer(mysetup.installerId)].uninstallProg <> '' then
  begin
    uninstcheckstr := mysetup.uninstallProg;
    // the use of the  $installdir$ variable for the primary section function fileexists
    // will for example result to:
    // if fileexists(""+$installdir$+"\uninst.exe")
    uninstcheckstr := StringReplace(uninstcheckstr, '$installdir$',
      '"+$installdir$+"', [rfIgnoreCase]);
    // the use of the  $installerSourceDir$ variable for the primary section function fileexists
    // will for example result to:
    // if fileexists($installerSourceDir$+"\uninst.exe")
    uninstcheckstr := StringReplace(uninstcheckstr, '$installerSourceDir$',
      '"+$installerSourceDir$+"', [rfIgnoreCase]);
    mysetup.uninstallCheck.Add('if fileexists("' + uninstcheckstr + '")');
    mysetup.uninstallCheck.Add('	set $oldProgFound$ = "true"');
    mysetup.uninstallCheck.Add('endif');
    if mysetup.preferSilent then
      mysetup.uninstallCommandLine :=
        '"' + mysetup.uninstallProg + '" ' +
        installerArray[integer(mysetup.installerId)].silentuninstall
    else
      mysetup.uninstallCommandLine :=
        '"' + mysetup.uninstallProg + '" ' +
        installerArray[integer(mysetup.installerId)].unattendeduninstall;
  end
  else
  begin
    // no known uninstall program
    mysetup.uninstallCheck.Add('set $oldProgFound$ = "false"');
  end;

  {$IFDEF WINDOWS}
  (*
  Many setup programs that install 64 bit only software
  are 32 bit.
  So: having a 32 bit installer means nothing
  but having a 64 bit installer means we have a 64 bit architecture for sure
  *)
  myArch := getBinaryArchitecture(myfilename);
  if myArch = '32' then
    mysetup.architecture := aUnknown;
  if myArch = '64' then
    mysetup.architecture := a64;
  if myArch = 'unknown' then
    mysetup.architecture := aUnknown;
  {$ENDIF WINDOWS}
  // check for installErrorHandlingLines and insert
  if installerArray[integer(mysetup.installerId)].installErrorHandlingLines.Count
    > 0 then
  begin
    for i := 0 to installerArray[integer(mysetup.installerId)].installErrorHandlingLines.Count - 1 do
      mysetup.installErrorHandlingLines.Add(
        installerArray[integer(mysetup.installerId)].installErrorHandlingLines[i]);
  end;
  {$IFDEF OSDGUI}
  resetGUI;
  resultForm1.setRunMode;
  procmess;
  {$ENDIF OSDGUI}
end; //get_aktProduct_general_info_win

procedure get_null_info(myfilename: string; var mysetup: TSetupFile);
begin
  // This is a dummy procedure
  write_log_and_memo('No special analyze for: ' +
    installerToInstallerstr(mysetup.installerId));
end;

procedure get_msi_info(myfilename: string; var mysetup: TSetupFile);
begin
  get_msi_info(myfilename, mysetup, False);
end;

procedure get_msi_info(myfilename: string; var mysetup: TSetupFile;
  uninstall_only: boolean);
var
  mycommand: string;
  myoutlines: TStringList;
  myreport: string;
  myexitcode: integer;
  i: integer;
  sSearch: string;
  iPos: integer;
  installdir: string;
  resultList: TStringList;
begin
  write_log_and_memo('Analyzing MSI: ' + myfilename);
  {$IFDEF WINDOWS}
  mycommand := 'cmd.exe /C cscript.exe "' + ExtractFilePath(ParamStr(0)) +
    'utils\msiinfo.js" "' + myfilename + '"';
  write_log_and_memo(mycommand);
  myoutlines := TStringList.Create;
  if not RunCommandAndCaptureOut(mycommand, True, myoutlines, myreport,
    SW_SHOWMINIMIZED, myexitcode) then
  begin
    write_log_and_memo('Failed to analyze: ' + myreport);
    mysetup.analyze_progess := 0;
  end
  else
  begin
    mysetup.analyze_progess := mysetup.analyze_progess + 10;
    write_log_and_memo('........');
    for i := 0 to myoutlines.Count - 1 do
    begin
      write_log_and_memo(myoutlines.Strings[i]);
      mysetup.analyze_progess := mysetup.analyze_progess + 1;
      sSearch := 'ProductName: ';
      iPos := Pos(sSearch, myoutlines.Strings[i]);
      if (iPos <> 0) then
      begin
        mysetup.msiProductName :=
          Copy(myoutlines.Strings[i], Length(sSearch) + 1,
          Length(myoutlines.Strings[i]) - Length(sSearch));
        aktProduct.productdata.productName := mysetup.msiProductName;
      end;

      sSearch := 'ProductVersion: ';
      iPos := Pos(sSearch, myoutlines.Strings[i]);
      if (iPos <> 0) then
        mysetup.softwareversion :=
          Copy(myoutlines.Strings[i], Length(sSearch) + 1,
          Length(myoutlines.Strings[i]) - Length(sSearch));

      sSearch := 'ProductCode: ';
      iPos := Pos(sSearch, myoutlines.Strings[i]);
      if (iPos <> 0) then
        mysetup.msiId :=
          Copy(myoutlines.Strings[i], Length(sSearch) + 1,
          Length(myoutlines.Strings[i]) - Length(sSearch));

      sSearch := 'UpgradeCode: ';
      iPos := Pos(sSearch, myoutlines.Strings[i]);
      if (iPos <> 0) then
        mysetup.msiUpgradeCode :=
          Copy(myoutlines.Strings[i], Length(sSearch) + 1,
          Length(myoutlines.Strings[i]) - Length(sSearch));

    end;
    // we have an msi, so we use msi uninstall mode
    // always if installer = msi
    if mysetup.installerId = stMsi then mysetup.msiUninstallCode := True
    // under conditions if it is a msi wrapper exe
    else if (mysetup.msiId <> '') and ((mysetup.msiProductName <> '') or
      (mysetup.msiUpgradeCode <> '')) and myconfiguration.preferMsiUninstall then
      mysetup.msiUninstallCode := True;
    LogDatei.log('msiUninstallCode: ' + BoolToStr(mysetup.msiUninstallCode, True),
      LLNotice);

    aktproduct.productdata.productversion := trim(mysetup.softwareversion);

  end;
  myoutlines.Free;
  {$ENDIF WINDOWS}

  {$IFDEF LINUX}
  myoutlines := TStringList.Create;
  resultList := TStringList.Create;
  try
    if not readMsiTable(myfilename, 'Property', myoutlines) then
    begin
      write_log_and_memo('Failed to analyze: ' + myfilename);
      mysetup.analyze_progess := 0;
    end
    else
    begin
      mysetup.analyze_progess := mysetup.analyze_progess + 10;
      LogDatei.log('........', LLDebug);
      for i := 0 to myoutlines.Count - 1 do
      begin
        LogDatei.log(myoutlines.Strings[i], LLDebug);
        mysetup.analyze_progess := mysetup.analyze_progess + 1;
        if myoutlines[i].StartsWith('ProductName', True) then
        begin
          resultList.Clear;
          resultList.AddStrings(myoutlines[i].Split(','));
          mysetup.msiProductName := trim(resultlist[1]);
          aktProduct.productdata.productName := mysetup.msiProductName;
        end;
        if myoutlines[i].StartsWith('ProductVersion', True) then
        begin
          resultList.Clear;
          resultList.AddStrings(myoutlines[i].Split(','));
          mysetup.SoftwareVersion := trim(resultlist[1]);
          aktproduct.productdata.productversion := mysetup.softwareversion;
        end;
        if myoutlines[i].StartsWith('ProductCode', True) then
        begin
          resultList.Clear;
          resultList.AddStrings(myoutlines[i].Split(','));
          mysetup.msiId := trim(resultlist[1]);
        end;
        if myoutlines[i].StartsWith('UpgradeCode', True) then
        begin
          resultList.Clear;
          resultList.AddStrings(myoutlines[i].Split(','));
          mysetup.msiUpgradeCode := trim(resultlist[1]);
        end;
      end;

      // we have an msi, so we use msi uninstall mode
      // always if installer = msi
      if mysetup.installerId = stMsi then
      begin
        mysetup.msiUninstallCode := True;
        mysetup.SetSetupFullFileName(myfilename);
      end
      // under conditions if it is a msi wrapper exe
      else if (mysetup.msiId <> '') and (mysetup.msiProductName <> '') or
        (mysetup.msiUpgradeCode <> '') and myconfiguration.preferMsiUninstall then
        mysetup.msiUninstallCode := True;
      LogDatei.log('msiUninstallCode: ' + BoolToStr(mysetup.msiUninstallCode, True),
        LLNotice);
    end;
  finally
    FreeAndNil(myoutlines);
    FreeAndNil(resultList);
  end;
  {$ENDIF LINUX}

  {$IFNDEF DARWIN}
  // use lessmsi / msiinfo
  installdir := getInstallDirFromMsi(myfilename);
  mysetup.installDirectory := installdir;

  // guess architecture from installdir
  if installdir.StartsWith('%ProgramFiles64Dir%') then
    mysetup.architecture := a64
  else if installdir.StartsWith('%ProgramFiles32Dir%') then
    mysetup.architecture := a32;
  {$ENDIF DARWIN}

  if mysetup.msiUninstallCode then
  begin
    if not uninstall_only then
    begin
      if mysetup.preferSilent then
        mysetup.installCommandLine :=
          'msiexec /i "$installerSourceDir$\' + mysetup.setupFileName +
          '" ' + installerArray[integer(mysetup.installerId)].silentsetup
      else
        mysetup.installCommandLine :=
          'msiexec /i "$installerSourceDir$\' + mysetup.setupFileName +
          '" ' + installerArray[integer(mysetup.installerId)].unattendedsetup;

      mysetup.mstAllowed := True;
    end;

    if mysetup.preferSilent then
      mysetup.uninstallCommandLine :=
        'msiexec /x $MsiId$ ' + installerArray[integer(stMsi)].silentuninstall
    else
      mysetup.uninstallCommandLine :=
        'msiexec /x $MsiId$ ' + installerArray[integer(stMsi)].unattendeduninstall;

    // clear non msi uninstall entries
    mysetup.uninstallProg := '';
    mysetup.uninstallCheck.Clear;
  end;


  LogDatei.log('get_MSI_info finished', LLInfo);
  mysetup.analyze_progess := 100;

  LogDatei.log('Finished Analyzing MSI: ' + myfilename, LLInfo);
end;


procedure get_inno_info(myfilename: string; var mysetup: TSetupFile);
var
  myoutlines: TStringList;
  myreport: string;
  myexitcode: integer;
  i: integer;
  destDir: string;
  destfile: string;
  myCommand: string;
  installScriptISS: string;
  issLine: string = '';
  fISS: TextFile;
  // components from install_script.iss[setup]
  AppName: string = '';
  AppVersion: string = '';
  AppVerName: string = '';
  DefaultDirName: string = '';
  ArchitecturesInstallIn64BitMode: string = '';
  ArchitecturesAllowed: string = '';
  UninstallFilesDir: string = '';
  uninstfile: string = '';

  function translateInnoConstants(myline: string; arch: string): string;
  var
    innoToOpsi: TStringList;
    i: integer;
  begin
    // arch has to be '32' or '64'
    innoToOpsi := TStringList.Create;
    // https://jrsoftware.org/ishelp/index.php?topic=consts
    innoToOpsi.Add('{cf}=%ProgramFiles' + arch + 'Dir%\Common Files');
    innoToOpsi.Add('{cf32}=%ProgramFiles32Dir%\Common Files');
    innoToOpsi.Add('{cf64}=%ProgramFiles6Dir%\Common Files');
    innoToOpsi.Add('{pf}=%ProgramFiles' + arch + 'Dir%');
    innoToOpsi.Add('{pf32}=%ProgramFiles32Dir%');
    innoToOpsi.Add('{pf64}=%ProgramFiles64Dir%');
    innoToOpsi.Add('{commoncf}=%ProgramFiles' + arch + 'Dir%\Common Files');
    innoToOpsi.Add('{commoncf32}=%ProgramFiles32Dir%\Common Files');
    innoToOpsi.Add('{commoncf64}=%ProgramFiles64Dir%\Common Files');
    innoToOpsi.Add('{commonpf}=%ProgramFiles' + arch + 'Dir%');
    innoToOpsi.Add('{commonpf32}=%ProgramFiles32Dir%');
    innoToOpsi.Add('{commonpf64}=%ProgramFiles64Dir%');
    innoToOpsi.Add('{autopf}=%ProgramFiles' + arch + 'Dir%');
    innoToOpsi.Add('{code:DefDirRoot}=%ProgramFiles' + arch + 'Dir%');
    innoToOpsi.Add('{code:installDir}=%ProgramFiles' + arch + 'Dir%\<unknown>');
    innoToOpsi.Add('{code:UNKNOWN}=%ProgramFiles' + arch + 'Dir%\<unknown>');
    innoToOpsi.Add('{win}=%Systemroot%');
    innoToOpsi.Add('{sys}=%System%');
    innoToOpsi.Add('{sysnative}=%System%');
    innoToOpsi.Add('{syswow64}=%System%');
    innoToOpsi.Add('{sd}=%Systemdrive%');

    for i := 0 to innoToOpsi.Count - 1 do
    begin
      myline := StringReplace(myline, innoToOpsi.Names[i],
        innoToOpsi.ValueFromIndex[i], [rfIgnoreCase]);
    end;
    Result := myline;
    innoToOpsi.Free;
  end;

begin
  write_log_and_memo('Analyzing Inno-Setup:');
  AppName := '';
  AppVersion := '';
  AppVerName := '';
  DefaultDirName := '';
  ArchitecturesInstallIn64BitMode := '';
  ArchitecturesAllowed := '';
  UninstallFilesDir := '';
  {$IFDEF WINDOWS}

  myoutlines := TStringList.Create;
  destDir := GetTempDir(False);
  destDir := destDir + DirectorySeparator + 'INNO';
  // cleanup destination
  if DirectoryExists(destDir) then
    DeleteDirectory(destDir, True);
  // create destination
  if not DirectoryExists(destDir) then
    ForceDirectories(destDir);
  destfile := ExtractFileName(myfilename);
  destfile := ExtractFileName(destfile);
  installScriptISS := destDir + DirectorySeparator + 'install_script.iss';
  // innounp works only until inno version 6.1  (Supports Inno Setup versions 2.0.7 through 6.1.2)
  // inno version > 6.2 needs innounp-2
  // https://www.rathlev-home.de/index-e.html?tools/prog-e.html#unpack
  // https://www.rathlev-home.de/tools/download/innounp-2.zip

  LogDatei.log('extract install_script.iss from ' + myfilename + ' to', LLInfo);
  LogDatei.log(installScriptISS, LLInfo);
  // myCommand := 'cmd.exe /C "'+ExtractFilePath(paramstr(0))+'innounp.exe" -x -a -y -d"'+destDir+'" ' +myfilename+ ' install_script.iss';
  if FileExists(installScriptISS) then
    DeleteFileUTF8(installScriptISS);
  myCommand := '"' + ExtractFilePath(ParamStr(0)) + 'utils' + PathDelim +
    'innounp.exe" -x -a -y -d"' + destDir + '" "' + myfilename + '" install_script.iss';
  LogDatei.log('Calling: ' + myCommand, LLInfo);

  if not RunCommandAndCaptureOut(myCommand, True, myoutlines, myreport,
    SW_SHOWMINIMIZED, myexitcode) then
  begin
    LogDatei.log('Failed to extract install_script.iss: ' + myreport, LLerror);
  end
  else
  begin
    if exitcode = 0 then
      LogDatei.log('exitcode=' + IntToStr(myexitcode), LLDebug)
    else
      LogDatei.log('exitcode=' + IntToStr(myexitcode), LLerror);
    LogDatei.log('output:', LLDebug);
    for i := 0 to myoutlines.Count - 1 do
      LogDatei.log(myoutlines.Strings[i], LLDebug);
    myoutlines.Free;

    // read install_script.iss
    if not FileExists(installScriptISS) then
    begin
      // no iss file
      LogDatei.log('ISS file not found: ' + installScriptISS, LLError);
    end
    else
    begin
      LogDatei.log('Reading: ' + installScriptISS, LLInfo);
      AssignFile(fISS, installScriptISS);
      Reset(fISS);
      // search [Setup] section
      while (not EOF(fISS)) and (not (0 < pos('[setup]', lowercase(issLine)))) do
      begin
        ReadLn(fISS, issLine);
      end;
      LogDatei.log('Here comes the iss file: ' + installScriptISS, LLInfo);
      LogDatei.log('[Setup]', LLDebug);
      ReadLn(fISS, issLine);

      // read until next section (usually [Files])
      while (not EOF(fISS)) and (Length(issLine) > 0) and (issLine[1] <> '[') do
      begin
        LogDatei.log(issLine, LLDebug);

        if (0 < pos('appname=', lowercase(issLine))) then
          AppName := Copy(issLine, pos('=', issLine) + 1, 100);
        if (0 < pos('appversion=', lowercase(issLine))) then
          AppVersion := Copy(issLine, pos('=', issLine) + 1, 100);
        if (0 < pos('appvername=', lowercase(issLine))) then
          AppVerName := Copy(issLine, pos('=', issLine) + 1, 100);
        if (0 < pos('defaultdirname=', lowercase(issLine))) then
          DefaultDirName := Copy(issLine, pos('=', issLine) + 1, 100);
        if (0 < pos('architecturesinstallin64bitmode=', lowercase(issLine))) then
          ArchitecturesInstallIn64BitMode := Copy(issLine, pos('=', issLine) + 1, 100);
        if (0 < pos('architecturesallowed=', lowercase(issLine))) then
          ArchitecturesAllowed := Copy(issLine, pos('=', issLine) + 1, 100);
        if (0 < pos('uninstallfilesdir=', lowercase(issLine))) then
          UninstallFilesDir := Copy(issLine, pos('=', issLine) + 1, 100);
        ReadLn(fISS, issLine);

      end;

      // get AppVersion from AppVerName (AppVerName = AppName + AppVersion ?)
      if (AppVersion = '') and (AppVerName <> '') then
      begin
        AppVersion := StringReplace(AppVerName, AppName, '', []);
        AppVersion := trim(StringReplace(AppVersion, ' ', '',
          [rfReplaceAll, rfIgnoreCase]));
      end;
      CloseFile(fISS);

      LogDatei.log('........', LLDebug);
      if AppVerName <> '' then
      begin
        aktProduct.productdata.productId := getPacketIDShort(AppName);
        aktProduct.productdata.productName := AppName;
      end;
      aktProduct.productdata.productversion := AppVersion;
      mysetup.SoftwareVersion := AppVersion;
      if AppVerName = '' then
      begin
        if ((AppName <> '') and (AppVersion <> '')) then
          AppVerName := AppName + ' ' + AppVersion
        else
          AppVerName := AppName + AppVersion;
      end;
      // if installdir is resolved at runtime by code then it is unknown
      if (0 < pos(lowercase('{code:'), lowercase(DefaultDirName))) then
        DefaultDirName := '{code:UNKNOWN}';
      // try to translate well known inno constants to opsi
      if (0 < pos('x64', lowercase(ArchitecturesInstallIn64BitMode))) and
        (0 = pos('x86', lowercase(ArchitecturesInstallIn64BitMode))) then
      begin
        DefaultDirName := translateInnoConstants(DefaultDirName, '64');
        mysetup.architecture := a64;
      end
      else
      begin
        DefaultDirName := translateInnoConstants(DefaultDirName, '32');
        mysetup.architecture := a32;
      end;
      // guess architecture from installdir
      if DefaultDirName.StartsWith('%ProgramFiles64Dir%') then
        mysetup.architecture := a64
      else if DefaultDirName.StartsWith('%ProgramFiles32Dir%') then
        mysetup.architecture := a32;
      mysetup.installDirectory := DefaultDirName;
      if UninstallFilesDir <> '' then
      begin
        UninstallFilesDir := StringReplace(UninstallFilesDir, '{app}',
          DefaultDirName, [rfIgnoreCase]);
        mysetup.uninstallDirectory := UninstallFilesDir;
        uninstfile := ExtractFileName(ReplaceText(mysetup.uninstallProg,
          '$installdir$', mysetup.installDirectory));
        // use $installdir$ if possible
        mysetup.uninstallDirectory :=
          ReplaceText(mysetup.uninstallDirectory, mysetup.installDirectory,
          '$installdir$');
        mysetup.uninstallProg := mysetup.uninstallDirectory + '\' + uninstfile;
        resultForm1.updateUninstaller(mysetup);
      end
      else
        mysetup.uninstallDirectory := DefaultDirName;

      aktProduct.productdata.comment := AppVerName;
      with mysetup do
      begin
        LogDatei.log('installDirectory: ' + installDirectory, LLDebug);
        LogDatei.log('SoftwareVersion: ' + SoftwareVersion, LLDebug);
        LogDatei.log('Architecture: ' + GetEnumName(typeInfo(TArchitecture),
          Ord(architecture)), LLDebug);
      end;
      with aktProduct.productdata do
      begin
        LogDatei.log('comment: ' + comment, LLDebug);
        LogDatei.log('productId: ' + productId, LLDebug);
        LogDatei.log('productName: ' + productName, LLDebug);
        LogDatei.log('productversion: ' + productversion, LLDebug);
      end;
    end;
  end;
  {$ENDIF WINDOWS}
  write_log_and_memo('get_inno_info finished');
  write_log_and_memo('Inno Setup detected');
  write_log_and_memo('Finished analyzing Inno-Setup');
end;


function findMsiAndAnalyze(myfilename: string; opsitmp: string;
  var mysetup: TSetupFile): boolean;
var
  smask: string;
  mymsilist: TStringList;
  mymsifilename: string;
begin
  Result := False;
  smask := opsitmp + '*.msi';
  LogDatei.log('Looking for: ' + smask, LLInfo);
  try
    mymsilist := FindAllFiles(opsitmp, '*.msi', True);
    // some wait retry
    if mymsilist.Count = 0 then
    begin
      Sleep(2000);
      mymsilist := FindAllFiles(opsitmp, '*.msi', True);
    end;
    if mymsilist.Count = 0 then
    begin
      Sleep(2000);
      mymsilist := FindAllFiles(opsitmp, '*.msi', True);
    end;
    if mymsilist.Count = 0 then
    begin
      Sleep(2000);
      mymsilist := FindAllFiles(opsitmp, '*.msi', True);
    end;
    if mymsilist.Count = 0 then
    begin
      Sleep(2000);
      mymsilist := FindAllFiles(opsitmp, '*.msi', True);
    end;

    if mymsilist.Count > 0 then
    begin
      Result := True;
      if mymsilist.Count > 1 then
      begin
        LogDatei.log('Found multiple msi files: ' + mymsilist.Text, LLWarning);
        if osdsettings.showgui then
          ShowMessage(sWarnMultipleMsi + opsitmp);
        mymsifilename := mymsilist.Strings[0];
        LogDatei.log('Analyzing msi file: ' + mymsifilename, LLInfo);
      end
      else
      begin
        mymsifilename := mymsilist.Strings[0];
        LogDatei.log('Analyzing found msi file: ' + mymsifilename, LLInfo);
      end;
      get_msi_info(mymsifilename, mysetup, True);
    end
    else
    begin
      LogDatei.log('Could not extract msi from: ' + myfilename, LLNotice);
    end;
  finally
    mymsilist.Free;
  end;

end;

procedure get_installshieldmsi_info(myfilename: string; var mysetup: TSetupFile);
var
  myoutlines: TStringList;
  myreport: string;
  myexitcode: integer;
  myBatch: string;
  msifound: boolean = False;
begin
  write_log_and_memo('Analyzing InstallShield+MSI Setup: ' + myfilename);
  mysetup.installerId := stInstallShieldMSI;
  mysetup.SetSetupFullFileName(myfilename);

  if DirectoryExists(opsitmp) then
    DeleteDirectory(opsitmp, True);
  if not DirectoryExists(opsitmp) then
    ForceDirectories(opsitmp);
  if not DirectoryExists(opsitmp) then
    LogDatei.log('Error: could not create directory: ' + opsitmp, LLError);
  {$IFDEF WINDOWS}
  // extract and analyze MSI from setup

  write_log_and_memo('Analyzing MSI from InstallShield Setup ' + myfilename);
  myoutlines := TStringList.Create;
  try

    (*
    // call setup with extract and try to find msi
    myoutlines.Clear;
    myBatch := '"' + ExtractFilePath(ParamStr(0)) + 'utils' + PathDelim +
        'extractMSI_by_param" "' + myfilename + '"';

    write_log_and_memo(myBatch);
    write_log_and_memo('!! PLEASE WAIT !!');
    write_log_and_memo('!! PLEASE WAIT !! Extracting and analyzing MSI ...');
    write_log_and_memo('!! PLEASE WAIT !!');

    if not RunCommandAndCaptureOut(myBatch, True, myoutlines, myreport,
      SW_SHOWMINIMIZED, myexitcode) then
    begin
      LogDatei.log('Failed to to run "' + myBatch + '": ' + myreport, LLWarning);
    end
    else
    begin
      logdatei.log_list(myoutlines, LLdebug);
      msifound := findMsiAndAnalyze(myfilename, opsitmp, mysetup);
      KillProcess('testsetup.exe');
      if DirectoryExists(opsitmp) then
        DeleteDirectory(opsitmp, True);
    end;
    *)

    if not msifound then
    begin
      // call setup interactive and try to find msi
      myoutlines.Clear;
      myBatch := '"' + ExtractFilePath(ParamStr(0)) + 'utils' + PathDelim +
        'extractMSI.cmd" "' + myfilename + '"';
      write_log_and_memo(myBatch);
      write_log_and_memo('!! PLEASE WAIT !!');
      write_log_and_memo('!! PLEASE WAIT !! Extracting and analyzing MSI ...');
      write_log_and_memo('!! PLEASE WAIT !!');

      if not RunCommandAndCaptureOut(myBatch, True, myoutlines, myreport,
        SW_SHOWMINIMIZED, myexitcode) then
      begin
        LogDatei.log('Failed to to run "' + myBatch + '": ' + myreport, LLWarning);
      end
      else
      begin
        logdatei.log_list(myoutlines, LLdebug);
        msifound := findMsiAndAnalyze(myfilename, opsitmp, mysetup);
        KillProcess('testsetup.exe');
        if DirectoryExists(opsitmp) then
          DeleteDirectory(opsitmp, True);
      end;
    end;
  finally
    FreeAndNil(myoutlines);
  end;
  {$ENDIF WINDOWS}

  write_log_and_memo('get_InstallShield_info finished');
  write_log_and_memo('InstallShield+MSI Setup detected');
end;


procedure get_installaware_info(myfilename: string; var mysetup: TSetupFile);
var
  str1, str2: string;
  pos1, pos2, i: integer;
begin
  write_log_and_memo('Analyzing InstallAware-Setup:');
  for i := 0 to mysetup.infolist.Count - 1 do
  begin
    str1 := mysetup.infolist[i];
    pos1 := pos(lowercase('RunProgram="'), str1);
    if 0 <= pos1 then
    begin
      str2 := copy(str1, pos1 + 12, Length(str1));
      pos2 := pos('"', str2);
      if 0 <= pos2 then
        str2 := copy(str2, 1, pos2 - 1);
      mysetup.uninstallProg := 'C:\ProgramData\{<UNKNOWN GUID>}\' + str2;
      mysetup.uninstall_waitforprocess := str2;
      if mysetup.preferSilent then
        mysetup.uninstallCommandLine :=
          '"' + mysetup.uninstallProg + '" ' +
          installerArray[integer(mysetup.installerId)].silentuninstall
      else
        mysetup.uninstallCommandLine :=
          '"' + mysetup.uninstallProg + '" ' +
          installerArray[integer(mysetup.installerId)].unattendeduninstall;
    end;
  end;
  write_log_and_memo('get_installaware_info finished');
end;

procedure get_wixtoolset_info(myfilename: string; var mysetup: TSetupFile);
var
  i: integer;
  myoutlines, extractedFiles: TStringList;
  destDir: string;
  myCommand: string;
  myreport: string;
  myexitcode: integer;
  msifound: boolean = False;
begin
  write_log_and_memo('Analyzing generic wixtoolset-Setup:');
  mysetup.uninstallProg := 'C:\ProgramData\{<UNKNOWN GUID>}\' +
    ExtractFileName(myfilename);

  write_log_and_memo('Analyzing Wix Bundle:');
  {$IFDEF WINDOWS}

  myoutlines := TStringList.Create;
  destDir := GetTempDir(False);
  destDir := destDir + DirectorySeparator + 'wixbundle';
  // cleanup destination
  if DirectoryExists(destDir) then
    DeleteDirectory(destDir, True);
  // create destination
  if not DirectoryExists(destDir) then
    ForceDirectories(destDir);

  LogDatei.log('extract files from ' + myfilename + ' to' + destDir, LLInfo);
  myCommand := '"' + ExtractFilePath(ParamStr(0)) + 'utils' + PathDelim +
    'wixbin' + PathDelim + 'dark.exe" -x "' + destDir + '" ' + myfilename;
  write_log_and_memo(myCommand);

  if not RunCommandAndCaptureOut(myCommand, True, myoutlines, myreport,
    SW_SHOWMINIMIZED, myexitcode) then
  begin
    LogDatei.log('Failed to extract files from wix bundle: ' + myreport, LLerror);
    osdsettings.myexitcode := myexitcode;
  end
  else
  begin
    osdsettings.myexitcode := myexitcode;
    for i := 0 to myoutlines.Count - 1 do
      write_log_and_memo(myoutlines.Strings[i]);
    myoutlines.Free;

    // read the extracted files
    extractedFiles := TStringList.Create;
    try
      FindAllFiles(extractedFiles, destDir, '*.*', True); //find  all extracted files
      LogDatei.log('Files extracted from wix bundle: ', LLNotice);
      for i := 0 to extractedFiles.Count - 1 do
        LogDatei.log('--extracted: ' + extractedFiles.Strings[i], LLNotice);
    finally
      extractedFiles.Free;
    end;

    msifound := findMsiAndAnalyze(myfilename, destDir, mysetup);
    KillProcess('dark.exe');
    if DirectoryExists(destDir) then
      DeleteDirectory(destDir, True);
  end;
  {$ENDIF WINDOWS}
  write_log_and_memo('get_wixtoolset_info finished');
end;

procedure get_boxstub_info(myfilename: string; var mysetup: TSetupFile);
begin
  write_log_and_memo('Analyzing generic boxstub-Setup:');
  mysetup.uninstallProg := 'C:\ProgramData\{<UNKNOWN GUID>}\' +
    ExtractFileName(myfilename);
  write_log_and_memo('get_boxstub_info finished');
end;

procedure get_sfxcab_info(myfilename: string; var mysetup: TSetupFile);
begin
  write_log_and_memo('Analyzing sfxcabr-Setup:');
  mysetup.uninstallProg := 'C:\ProgramData\{<UNKNOWN GUID>}\' +
    ExtractFileName(myfilename);
  write_log_and_memo('get_sfxcab_info finished');
end;

procedure get_advancedInstaller_info(myfilename: string; var mysetup: TSetupFile);
var
  commandline: string;
  myoutlines: TStringList;
  myreport: string;
  myexitcode: integer;
  temp_exe: string;
  msifound: boolean = False;
begin
  write_log_and_memo('Analyzing advancedInstaller:');
  if DirectoryExists(opsitmp) then
    DeleteDirectory(opsitmp, True);
  if not DirectoryExists(opsitmp) then
    ForceDirectories(opsitmp);
  if not DirectoryExists(opsitmp) then
    LogDatei.log('Error: could not create directory: ' + opsitmp, LLError);
  {$IFDEF WINDOWS}
  // extract and analyze MSI from setup

  write_log_and_memo('Analyzing MSI from advancedInstaller Setup ' + myfilename);
  temp_exe := opsitmp + '\' + ExtractFileName(myfilename);
  CopyFile(myfilename, temp_exe);
  commandline := '"' + temp_exe + '" /extract "' + opsitmp + '"';

  myoutlines := TStringList.Create;
  write_log_and_memo(commandline);
  write_log_and_memo('!! PLEASE WAIT !!');
  write_log_and_memo('!! PLEASE WAIT !! Extracting and analyzing MSI ...');
  write_log_and_memo('!! PLEASE WAIT !!');

  if not RunCommandAndCaptureOut(commandline, True, myoutlines, myreport,
    SW_SHOWMINIMIZED, myexitcode) then
  begin
    LogDatei.log('Failed to to run "' + commandline + '": ' + myreport, LLWarning);
  end
  else
  begin
    logdatei.log_list(myoutlines, LLdebug);
    msifound := findMsiAndAnalyze(myfilename, opsitmp, mysetup);
    KillProcess(temp_exe);
    if DirectoryExists(opsitmp) then
      DeleteDirectory(opsitmp, True);
    FreeAndNil(myoutlines);
    KillProcess(ExtractFileName(temp_exe));
  end;
  {$ENDIF WINDOWS}
  if not mysetup.msiUninstallCode then
  begin
    mysetup.uninstallDirectory := '$installerSourceDir$';
    mysetup.uninstallProg := mysetup.uninstallDirectory + '\' + mysetup.setupFileName;

    if mysetup.preferSilent then
      mysetup.uninstallCommandLine :=
        '"' + mysetup.uninstallDirectory + '\' + mysetup.setupFileName +
        '" ' + installerArray[integer(mysetup.installerId)].silentuninstall
    else
      mysetup.uninstallCommandLine :=
        '"' + mysetup.uninstallDirectory + '\' + mysetup.setupFileName +
        '" ' + installerArray[integer(mysetup.installerId)].unattendeduninstall;

  end;
  resultForm1.updateUninstaller(mysetup);

  write_log_and_memo('get_advancedInstaller_info finished');
end;

procedure get_installAnywhere_info(myfilename: string; var mysetup: TSetupFile);
begin
  write_log_and_memo('Analyzing installAnywhere:');
  write_log_and_memo('get_installAnywhere_info finished');
end;

procedure get_QtInstaller_info(myfilename: string; var mysetup: TSetupFile);
begin
  write_log_and_memo('Analyzing QtInstaller:');
  mysetup.installDirectory := '%Programfiles64Dir%\"+$ProductId$+"';
  mysetup.architecture := a64;
  write_log_and_memo('get_QtInstaller_info finished');
end;

procedure get_MsixAppx_info(myfilename: string; var mysetup: TSetupFile);
var
  packagepath, cmdStr, versionStr, fullNameStr, displayNameStr: string;
  destDir, proptmpstr, architectureStr: string;
  xmlLines, nodeLines: TStringList;
  manifesFileName: string;
  i: integer;
begin
  write_log_and_memo('Analyzing MsixAppx Package:');
  // Analyze
  // unzip package
  xmlLines := TStringList.Create;
  // nodeLines is created by call
  destDir := GetTempDir(False);
  destDir := destDir + DirectorySeparator + 'msixAppx';
  // cleanup destination
  if DirectoryExists(destDir) then
    DeleteDirectory(destDir, True);
  // create destination
  if not DirectoryExists(destDir) then
    ForceDirectories(destDir);
  if UnzipWithDirStruct(myfilename, destDir) then
  begin
    LogDatei.log('Unzipped ' + myfilename + ' to ' + destDir, LLnotice);
    manifesFileName := destDir + DirectorySeparator + 'AppxManifest.xml';
    if not fileexists(manifesFileName) then
      // perhaps  a bundle ?
      manifesFileName := destDir + DirectorySeparator +
        'AppxMetadata\AppxBundleManifest.xml';
    if fileexists(manifesFileName) then
    begin
      xmlLines := getXMLDocumentElementfromFile(manifesFileName);
      if xml2GetFirstChildNodeByName(xmlLines, 'Identity', nodeLines) then
      begin
        if getXml2AttributeValueByKey(nodeLines, 'Version', versionStr) then
        begin
          mysetup.SoftwareVersion := versionStr;
          aktProduct.productdata.productversion := versionStr;
          LogDatei.log('Got Version: ' + versionStr +
            ' from node Identity in AppxManifest.xml', LLnotice);
        end
        else
          LogDatei.log('Could not get Version from AppxManifest.xml.', LLwarning);
        if getXml2AttributeValueByKey(nodeLines, 'Name', fullNameStr) then
        begin
          aktProduct.productdata.productName := fullNameStr;
          LogDatei.log('Got FullName: ' + fullNameStr +
            ' from node Identity in AppxManifest.xml', LLnotice);
        end
        else
          LogDatei.log('Could not get FullName from AppxManifest.xml.', LLwarning);
        if getXml2AttributeValueByKey(nodeLines, 'ProcessorArchitecture',
          architectureStr) then
        begin
          if architectureStr = 'X86' then mysetup.architecture := a32
          else if architectureStr = 'X64' then mysetup.architecture := a64
          // we have no equivalent to 'neutral' right now but perhaps we should have ....(do 25.4.2025)
          else if architectureStr = 'Neutral' then mysetup.architecture := aUnknown
          else
            mysetup.architecture := aUnknown;
          LogDatei.log('Got Architecture: ' + architectureStr +
            ' from node Identity in AppxManifest.xml', LLnotice);
          LogDatei.log('Architecture: ' + GetEnumName(typeInfo(TArchitecture),
            Ord(mysetup.architecture)), LLDebug);
        end
        else
          LogDatei.log('Could not get Architecture from AppxManifest.xml.', LLwarning);
      end
      else
        LogDatei.log('Could not get node Identity from AppxManifest.xml.', LLwarning);
      if xml2GetFirstChildNodeByName(xmlLines, 'DisplayName', nodeLines) then
      begin
        displayNameStr := getXml2NodeText(nodeLines);
        aktProduct.productdata.productId := LowerCase(displayNameStr);
        LogDatei.log('Got productId: ' + productId +
          ' from node DisplayName in AppxManifest.xml', LLnotice);
      end
      else
        LogDatei.log('Could not get node Displayname from AppxManifest.xml.', LLwarning);
      if xml2GetFirstChildNodeByName(xmlLines, 'Description', nodeLines) then
      begin
        aktProduct.productdata.description := getXml2NodeText(nodeLines);
        LogDatei.log('Got description: ' + aktProduct.productdata.description +
          ' from node Description in AppxManifest.xml', LLnotice);
      end
      else
        LogDatei.log('Could not get node Displayname from AppxManifest.xml.', LLwarning);
    end
    else
      LogDatei.log('File AppxManifest.xml not found.', LLwarning);
  end
  else
    write_log_and_memo('Could not unzip: ' + myfilename);

  // install cmd
  packagepath := '"$installerSourceDir$\' + mysetup.setupFileName + '"';
  cmdStr := installerArray[integer(mysetup.installerId)].silentsetup;
  cmdStr := StringReplace(cmdStr, '<#packagePath#>', packagepath, [rfIgnoreCase]);
  mysetup.installCommandLine := cmdStr;

  // uninstall check
  mysetup.uninstallCheck.Clear;
  cmdStr := 'Get-AppxPackage -AllUsers -Name ' + fullNameStr +
    ' | select -expandProperty PackageFullName';
  mysetup.uninstallCheck.Add('set $UninstallList$ = powershellCall("' + cmdStr + '")');
  mysetup.uninstallCheck.Add('; remove empty entries');
  mysetup.uninstallCheck.Add(
    'set $UninstallList$ = removeFromListByMatch("", $UninstallList$)');
  mysetup.uninstallCheck.Add('if count($UninstallList$) int> "0"');
  mysetup.uninstallCheck.Add(
    '	set $MsixAppxPackageName$ = takeString(0,$UninstallList$)');
  mysetup.uninstallCheck.Add('	set $oldProgFound$ = "true"');
  mysetup.uninstallCheck.Add('endif');

  // uninstall command
  if mysetup.preferSilent then
    cmdStr := installerArray[integer(mysetup.installerId)].silentuninstall
  else
    cmdStr := installerArray[integer(mysetup.installerId)].unattendeduninstall;
  cmdStr := StringReplace(cmdStr, '<#packageFullName#>', '$MsixAppxPackageName$',
    [rfIgnoreCase]);
  mysetup.uninstallCommandLine := cmdStr;

  mysetup.optionalUninstallLines.Add(
    'set $cmdStr$ = "Get-AppxProvisionedPackage -Online | Where-Object {$_.PackageName -eq "');
  mysetup.optionalUninstallLines.Add('set $cmdStr$ = $cmdStr$ +''"''');
  mysetup.optionalUninstallLines.Add('set $cmdStr$ = $cmdStr$ + $MsixAppxPackageName$');
  mysetup.optionalUninstallLines.Add('set $cmdStr$ = $cmdStr$ +''"''');
  mysetup.optionalUninstallLines.Add(
    'set $cmdStr$ = $cmdStr$ + "} | Remove-AppxProvisionedPackage -Online"');
  mysetup.optionalUninstallLines.Add('Set $Exitcode$ = powershellcall($cmdStr$)');

  mysetup.optionalUninstallLines.Add(
    'if "true" = isGenericExitcodeFatal($exitcode$, "true", $ErrorString$ )');
  mysetup.optionalUninstallLines.Add('	LogError $ErrorString$');
  mysetup.optionalUninstallLines.Add('	;isfatalerror $ErrorString$');
  mysetup.optionalUninstallLines.Add('else');
  mysetup.optionalUninstallLines.Add('	Comment $ErrorString$');
  mysetup.optionalUninstallLines.Add('endif');

  // add uninstall_before_install with default = false
  aktProduct.productdata.uninstallBeforeInstall := True;
  makeProperties;
  for i := 0 to aktProduct.properties.Count - 1 do
  begin
    proptmpstr := LowerCase(aktProduct.properties.Items[i].Property_Name);
    if (proptmpstr = LowerCase('uninstall_before_install')) then
      aktProduct.properties.Items[i].BoolDefault := False;
  end;

  write_log_and_memo('get_MsixAppx_info finished');
end;

procedure get_winget_info(var mysetup: TSetupFile);
var
  str1, str2, cmdStr: string;
  pos1, pos2, i: integer;
begin
  LogDatei.log('Preparing winget-Setup:', LLnotice);
  mysetup.uninstallProg := '';
  mysetup.uninstall_waitforprocess := '';
  mysetup.uninstallCheck.Clear;
  mysetup.uninstallCheck.Add('set $wingetCommandParam$ = " list --id ' +
    mysetup.wingetId + ' --exact --accept-source-agreements"');
  mysetup.uninstallCheck.Add(
    'set $UninstallList$ = shellCall($wingetBin$ + $wingetCommandParam$)');
  mysetup.uninstallCheck.Add(
    'if not("" = getIndexFromListByContaining($UninstallList$, "' + mysetup.wingetId + '"))');
  mysetup.uninstallCheck.Add('	set $progFound$ = "true"');
  mysetup.uninstallCheck.Add('endif');
  mysetup.uninstallCheck.Add('; alternate check:');
  mysetup.uninstallCheck.Add('if directoryexists($installdir$)');
  mysetup.uninstallCheck.Add('	set $progFound$ = "true"');
  mysetup.uninstallCheck.Add('endif');

  // install command
  if mysetup.preferSilent then
    cmdStr := installerArray[integer(mysetup.installerId)].silentsetup
  else
    cmdStr := installerArray[integer(mysetup.installerId)].unattendedsetup;

  cmdStr := StringReplace(cmdStr, '<#wingetId#>', mysetup.wingetId, [rfIgnoreCase]);
  cmdStr := StringReplace(cmdStr, '<#wingetSource#>', mysetup.wingetSource,
    [rfIgnoreCase]);
  mysetup.installCommandLine := cmdStr;
  mysetup.installCommandStringEx := cmdStr;
  LogDatei.log('winget installCommandLine: ' + cmdStr, LLinfo);

  // uninstall command
  if mysetup.preferSilent then
    cmdStr := installerArray[integer(mysetup.installerId)].silentuninstall
  else
    cmdStr := installerArray[integer(mysetup.installerId)].unattendeduninstall;

  cmdStr := StringReplace(cmdStr, '<#wingetId#>', mysetup.wingetId, [rfIgnoreCase]);
  cmdStr := StringReplace(cmdStr, '<#wingetSource#>', mysetup.wingetSource,
    [rfIgnoreCase]);
  mysetup.uninstallCommandLine := cmdStr;
  mysetup.uninstallCommandStringEx := cmdStr;
  LogDatei.log('winget uninstallCommandLine: ' + cmdStr, LLinfo);

  // akt product
  if (aktProduct.productdata.productId = '') or
    (aktProduct.productdata.productId =
    copy(mysetup.wingetId, 0, mysetup.wingetId.Length - 1)) then
    aktProduct.productdata.productId := mysetup.wingetId;
  if (aktProduct.productdata.productName = '') or
    (aktProduct.productdata.productName =
    copy(mysetup.wingetId, 0, mysetup.wingetId.Length - 1)) then
    aktProduct.productdata.productName := mysetup.wingetId;
  mysetup.isExitcodeFatalFunction :=
    installerArray[integer(mysetup.installerId)].uib_exitcode_function;



  LogDatei.log('get_winget_info finished', LLnotice);
end;



// marker for add installers

procedure Analyze(FileName: string; var mysetup: TSetupFile; verbose: boolean);
var
  setupType: TKnownInstaller;
  tmpstr: string;
  i: integer;
begin
  LogDatei.log('Start Analyze ... ', LLInfo);
  {$IFDEF OSDGUI}
  resultForm1.ProgressBarAnalyze.Position := 0;
  procmess;
  {$ENDIF OSDGUI}
  setupType := stUnknown;
  if '.msi' = lowercase(ExtractFileExt(FileName)) then
  begin
    mysetup.analyze_progess := 10;
    setupType := stMsi;
    get_aktProduct_general_info_win(stMsi, Filename, mysetup);
    get_msi_info(FileName, mysetup);
    write_log_and_memo('Found installer= ' + installerToInstallerstr(setupType));
  end
  else if '.msix' = lowercase(ExtractFileExt(FileName)) then
  begin
    mysetup.analyze_progess := 10;
    setupType := stMsixAppx;
    get_aktProduct_general_info_win(stMsixAppx, Filename, mysetup);
    get_MsixAppx_info(FileName, mysetup);
    write_log_and_memo('Found installer= ' + installerToInstallerstr(setupType));
  end
  else if '.appx' = lowercase(ExtractFileExt(FileName)) then
  begin
    mysetup.analyze_progess := 10;
    setupType := stMsixAppx;
    get_aktProduct_general_info_win(stMsixAppx, Filename, mysetup);
    get_MsixAppx_info(FileName, mysetup);
    write_log_and_memo('Found installer= ' + installerToInstallerstr(setupType));
  end
  else if '.appxbundle' = lowercase(ExtractFileExt(FileName)) then
  begin
    mysetup.analyze_progess := 10;
    setupType := stMsixAppx;
    get_aktProduct_general_info_win(stMsixAppx, Filename, mysetup);
    get_MsixAppx_info(FileName, mysetup);
    write_log_and_memo('Found installer= ' + installerToInstallerstr(setupType));
  end
  else if '.msixbundle' = lowercase(ExtractFileExt(FileName)) then
  begin
    mysetup.analyze_progess := 10;
    setupType := stMsixAppx;
    get_aktProduct_general_info_win(stMsixAppx, Filename, mysetup);
    get_MsixAppx_info(FileName, mysetup);
    write_log_and_memo('Found installer= ' + installerToInstallerstr(setupType));
  end
  else
  begin
    setupType := analyze_binary(FileName, verbose, False, mysetup);

    sleep(2000);

    {$IFDEF OSDGUI}
    if (setupType = stUnknown) or (setupType = stDetectedUnknown) then
    begin
      FChooseInstallerDlg.ComboBoxChooseInstaller.Clear;
      for i := 0 to integer(stUnknown) do
        FChooseInstallerDlg.ComboBoxChooseInstaller.Items.Add(
          installerToInstallerstr(TKnownInstaller(i)));
      FChooseInstallerDlg.ComboBoxChooseInstaller.Text :=
        installerToInstallerstr(stUnknown);
      if setupType = stDetectedUnknown then
      begin
        FChooseInstallerDlg.LabelDetected.Caption :=
          'Detected: ' + mysetup.installerName + ' Version: ' + mysetup.installerVersion;
        FChooseInstallerDlg.SetLabelUnknown(True);
      end
      else
      begin
        FChooseInstallerDlg.LabelDetected.Caption := '';
        FChooseInstallerDlg.SetLabelUnknown(False);
      end;
      if FChooseInstallerDlg.ShowModal = mrOk then
      begin
        setupType := installerstrToInstaller(
          FChooseInstallerDlg.ComboBoxChooseInstaller.Text);
        mysetup.installerId := setupType;
      end;
    end;
    {$ENDIF OSDGUI}

    get_aktProduct_general_info_win(setupType, Filename, mysetup);

    // marker for add installers
    case setupType of
      // get additional infos for some installers:
      stInno: get_inno_info(FileName, mysetup);
      stInstallShieldMSI: get_installshieldmsi_info(FileName, mysetup);
      //stAdvancedMSI: get_advancedmsi_info(FileName, mysetup);
      stMsi: ;// nothing to do here - see above;
      stMsixAppx: ;// nothing to do here - see above;
      stInstallAware: get_installaware_info(FileName, mysetup);
      //stMSGenericInstaller: get_genmsinstaller_info(FileName, mysetup);
      stWixToolset: get_wixtoolset_info(FileName, mysetup);
      stBoxStub: get_boxstub_info(FileName, mysetup);
      stSFXcab: get_sfxcab_info(FileName, mysetup);
      stAdvancedInstaller: get_advancedInstaller_info(FileName, mysetup);
      stInstallAnywhere: get_installAnywhere_info(FileName, mysetup);
      stQtInstaller: get_QtInstaller_info(FileName, mysetup);
      stUnknown: LogDatei.log(
          'Unknown Installer after Analyze.', LLcritical);
      else
        get_null_info(FileName, mysetup);
        {nothing special right now for stBitrock, stSelfExtractingInstaller,
         stPortableApps, stInstall4J, st7zip, stNsis, stInstallShield,
         st7zipsfx,stSetupFactory,stWise, stVisualStudioInstaller}
    end;


    // marker for add installers
    tmpstr := installerToInstallerstr(setupType);
    case setupType of
      stMsi: ;// nothing to do here - see above;
      stMsixAppx: ;// nothing to do here - see above;
(*
      stInno: write_log_and_memo('Found installer= ' + tmpstr);
      stNsis: write_log_and_memo('Found installer= ' + tmpstr);
      stInstallShield: write_log_and_memo('Found installer= ' + tmpstr);
      stInstallShieldMSI: write_log_and_memo('Found installer= ' + tmpstr);
      stAdvancedMSI: write_log_and_memo('Found installer= ' + tmpstr);
      st7zip: write_log_and_memo('Found installer= ' + tmpstr);

      st7zipsfx: write_log_and_memo('Found installer= ' + tmpstr);
      stInstallAware: write_log_and_memo('Found installer= ' + tmpstr);
      stMSGenericInstaller: write_log_and_memo('Found installer= ' + tmpstr);
      stWixToolset: write_log_and_memo('Found installer= ' + tmpstr);
      stBoxStub: write_log_and_memo('Found installer= ' + tmpstr);
      stSFXcab: write_log_and_memo('Found installer= ' + tmpstr);
      stBitrock: write_log_and_memo('Found installer= ' + tmpstr);
      stSelfExtractingInstaller: write_log_and_memo('Found installer= ' + tmpstr);
      stUnknown: write_log_and_memo('Found installer= ' + tmpstr);
      *)
      else
        write_log_and_memo('Found installer= ' + tmpstr);
    end;
    { avoid hyphen char "-" and replace with dot "." in version }
    aktproduct.productdata.productversion :=
      StringReplace(aktproduct.productdata.productversion, '-', '.', [rfReplaceAll]);

  end;
  {$IFDEF OSDGUI}
  resultForm1.ProgressBarAnalyze.Position := 100;
  procmess;
  if osdsettings.DetectCount > 1 then
    MyMessageDlg.ShowMessage('Warning', 'More than one installertype detected:' +
      LineEnding + osdsettings.DetectionSummary.Text + LineEnding +
      'Please check log file.', [mrOk]);
  {$ENDIF OSDGUI}
  //sleep(2000);
  mysetup.installerId := setupType;
  if setupType = stUnknown then
  begin
  end
  else
  begin
    {$IFDEF OSDGUI}
    resultform1.BtAnalyzeNextStepClick(nil);
    tmpstr := installerArray[integer(setupType)].info_message_html.Text;
    if tmpstr <> '' then
    begin
      //MyMessageDlg.wiMessageSized(tmpstr,[mrOk], 950, 740);
      OSD_info.mdContent := tmpstr;
      if osdsettings.showgui then
        OSD_info.ShowModal;
    end;
    {$ENDIF OSDGUI}
  end;
end;

end.
