unit osdanalyzewin;

{$mode delphi}

interface

uses
  {$IFDEF WINDOWS}
  Windows,
  ShlObj,
  Registry,
  verinfo,
  jwatlhelp32,
  jwawindows,
  {$ENDIF WINDOWS}
  Dialogs,
  LCLType,
  Classes,
  osdhelper,
  Process,
  fileutil,
  lazfileutils,
  strutils,
  SysUtils,
  fileinfo,
  winpeimagereader,
  oslog,
  osdbasedata,
  osdanalyzegeneral,
  oscheckbinarybitness,
  osmessagedialog,
  Controls;

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
procedure get_genmsinstaller_info(myfilename: string; var mysetup: TSetupFile);
procedure get_wixtoolset_info(myfilename: string; var mysetup: TSetupFile);
procedure get_boxstub_info(myfilename: string; var mysetup: TSetupFile);
procedure get_sfxcab_info(myfilename: string; var mysetup: TSetupFile);
//procedure get_bitrock_info(myfilename: string; var mysetup: TSetupFile);
//procedure get_selfextrackting_info(myfilename: string; var mysetup: TSetupFile);
procedure get_advancedInstaller_info(myfilename: string; var mysetup: TSetupFile);
procedure get_installAnywhere_info(myfilename: string; var mysetup: TSetupFile);
procedure get_QtInstaller_info(myfilename: string; var mysetup: TSetupFile);
// marker for add installers
procedure Analyze(FileName: string; var mysetup: TSetupFile; verbose: boolean);
function getProductInfoFromResource(infokey: string; filename: string): string;

resourcestring
  sWarnMultipleMsi =
    'Multiple (more than one) msi files found. Check the log file and directory: ';

implementation

{$IFDEF OSDGUI}
uses
  osdform,
  osdmain,
  ChooseInstallerDlg;
{$ELSE OSDGUI}
uses
  osdmain;
{$ENDIF OSDGUI}

{$IFDEF WINDOWS}
procedure KillProcess(name: String);
var h:tHandle;
    pe:tProcessEntry32;
    sPrcName:string;
begin
  h:=CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  try
    pe.dwSize:=SizeOf(pe);
    if Process32First(h,pe) then begin
      while Process32Next(h,pe) do
      begin
        sPrcName:=pe.szExeFile;
        if pos(LowerCase(name),LowerCase(sPrcName))>0 then
        begin
          TerminateProcess(OpenProcess(Process_Terminate, False, pe.th32ProcessID), 0);
        end;
      end;
    end
    else RaiseLastOSError;
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
        LogDatei.log('Exception while reading fileversion2: ' +
          E.ClassName + ': ' + E.Message, LLError);
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
          E.ClassName + ': ' + E.Message, LLError);
        Result := '';
      end;
      else // close the 'on else' block here;
    end;
  end;
  {$ENDIF WINDOWS}
end;

procedure get_aktProduct_general_info_win(installerId: TKnownInstaller;
  myfilename: string; var mysetup: TSetupFile);
var
  myoutlines: TStringList;
  myreport: string;
  myexitcode: integer;
  i: integer;
  fsize: int64;
  fsizemb, rsizemb: double;
  sMsiSize: string;
  sReqSize: string;
  sFileSize: string;
  sSearch: string;
  iPos: integer;
  destDir: string;
  myArch: string;
  product: string;
  installerstr: string;
  str1, uninstcheckstr: string;

begin
  installerstr := installerToInstallerstr(installerId);
  write_log_and_memo('Analyzing ' + installerstr + ' Setup: ' + myfilename);

  mysetup.installerId := installerId;
  mysetup.link := installerArray[integer(mysetup.installerId)].Link;
  mysetup.setupFullFileName := myfilename;
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
    // the use of the  $installdir$ variable for the promary section function fileexists
    // will for example result to:
    // if fileexists(""+$installdir$+"\uninst.exe")
    uninstcheckstr := StringReplace(uninstcheckstr, '$installdir$',
      '"+$installdir$+"', [rfIgnoreCase]);
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
  myArch := getBinaryArchitecture(myfilename);
  if myArch = '32' then
    mysetup.architecture := a32;
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
end; //get_aktProduct_general_info_win

procedure get_null_info(myfilename: string; var mysetup: TSetupFile);
begin
  // This is a dummy procedure
  write_log_and_memo('No special analyze for: '+ installerToInstallerstr(mysetup.installerId));
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
    if not uninstall_only then
    begin
      mysetup.installerId := stMsi;
      mysetup.setupFullFileName := myfilename;
    end;
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

    end;
    aktproduct.productdata.productversion := trim(mysetup.softwareversion);
  end;
  myoutlines.Free;
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
  mycommand := 'bash -c ''msiinfo export "' + myfilename + '" Property''';
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
    if not uninstall_only then
    begin
      mysetup.installerId := stMsi;
      mysetup.setupFullFileName := myfilename;
    end;
    LogDatei.log('........', LLDebug);
    for i := 0 to myoutlines.Count - 1 do
    begin
      LogDatei.log(myoutlines.Strings[i], LLDebug);
      mysetup.analyze_progess := mysetup.analyze_progess + 1;
      sSearch := 'ProductName';
      iPos := Pos(sSearch, myoutlines.Strings[i]);
      if (iPos <> 0) then
      begin
        aktProduct.productdata.productName :=
          trim(Copy(myoutlines.Strings[i], Length(sSearch) + 1,
          Length(myoutlines.Strings[i]) - Length(sSearch)));
        LogDatei.log('Found ProductName: ' +
          aktProduct.productdata.productName, LLNotice);
      end;

      sSearch := 'ProductVersion';
      iPos := Pos(sSearch, myoutlines.Strings[i]);
      if (iPos <> 0) then
      begin
        mysetup.softwareversion :=
          trim(Copy(myoutlines.Strings[i], Length(sSearch) + 1,
          Length(myoutlines.Strings[i]) - Length(sSearch)));
        LogDatei.log('Found ProductVersion: ' + mysetup.softwareversion, LLNotice);
      end;

      sSearch := 'ProductCode';
      iPos := Pos(sSearch, myoutlines.Strings[i]);
      if (iPos <> 0) then
      begin
        mysetup.msiId :=
          trim(Copy(myoutlines.Strings[i], Length(sSearch) + 1,
          Length(myoutlines.Strings[i]) - Length(sSearch)));
        LogDatei.log('Found ProductCode: ' + mysetup.msiId, LLNotice);
      end;

    end;
    //if aktproduct.productdata.productversion = '' then
    aktproduct.productdata.productversion := trim(mysetup.softwareversion);
  end;
  myoutlines.Free;
  {$ENDIF LINUX}
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
  fsize: int64;
  fsizemb: double;
  rsizemb: double;
  sFileSize: string;
  sReqSize: string;
  sSearch: string;
  iPos: integer;
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
  tmpint: integer;
  ArchitecturesAllowed: string = '';
  ArchitecturesInstallIn64BitMode: string = '';
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
    DeleteDirectory(destDir,true);
  // create destination
  if not DirectoryExists(destDir) then
    ForceDirectories(destDir);
  destfile := ExtractFileName(myfilename);
  destfile := ExtractFileName(destfile);
  installScriptISS := destDir + DirectorySeparator + 'install_script.iss';

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
      end
      else
      begin
        DefaultDirName := translateInnoConstants(DefaultDirName, '32');
      end;
      mysetup.installDirectory := DefaultDirName;
      if UninstallFilesDir <> '' then
      begin
        UninstallFilesDir := StringReplace(UninstallFilesDir, '{app}',
          DefaultDirName, [rfIgnoreCase]);
        mysetup.uninstallDirectory:= UninstallFilesDir;
        uninstfile := ExtractFileName(ReplaceText(mysetup.uninstallProg,'$installdir$', mysetup.installDirectory));
        // use $installdir$ if possible
        mysetup.uninstallDirectory := ReplaceText(mysetup.uninstallDirectory, mysetup.installDirectory, '$installdir$');
        mysetup.uninstallProg := mysetup.uninstallDirectory + '\' + uninstfile;
        resultForm1.updateUninstaller(mysetup);
      end
      else mysetup.uninstallDirectory:= DefaultDirName;

      aktProduct.productdata.comment := AppVerName;
      with mysetup do
      begin
        LogDatei.log('installDirectory: ' + installDirectory, LLDebug);
        LogDatei.log('SoftwareVersion: ' + SoftwareVersion, LLDebug);
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

(*
procedure get_installshield_info(myfilename: string; var mysetup: TSetupFile);
var
  product: string;

begin
  write_log_and_memo('Analyzing InstallShield-Setup:');
  write_log_and_memo('get_InstallShield_info finished');
  write_log_and_memo('InstallShield Setup detected');
end;
 *)

procedure get_installshieldmsi_info(myfilename: string; var mysetup: TSetupFile);
var
  myoutlines: TStringList;
  myreport: string;
  myexitcode: integer;
  myBatch: string;
  //FileInfo: TSearchRec;
  smask: string;
  mymsilist: TStringList;
  mymsifilename: string;

begin
  write_log_and_memo('Analyzing InstallShield+MSI Setup: ' + myfilename);
  mysetup.installerId := stInstallShieldMSI;
  mysetup.setupFullFileName := myfilename;

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
  // myBatch := 'cmd.exe /C '+ExtractFilePath(paramstr(0))+'extractMSI.cmd "'+myfilename+'"'; // (does not work with spaces in EXE path)
  myBatch := '"' + ExtractFilePath(ParamStr(0)) + 'utils' + PathDelim +
    'extractMSI.cmd" "' + myfilename + '"';
  // dropped cmd.exe
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
    logdatei.log_list(myoutlines,LLdebug);
    //stopped := KillProcessbyname(ExtractFileName(myfilename), found);
    smask := opsitmp + '*.msi';
    LogDatei.log('Looking for: ' + smask, LLInfo);
    try
      mymsilist := FindAllFiles(opsitmp, '*.msi', True);
      if mymsilist.Count > 0 then
      begin
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
    FreeAndNil(myoutlines);
    KillProcess(ExtractFileName(myfilename));
  end;
  {$ENDIF WINDOWS}
  write_log_and_memo('get_InstallShield_info finished');
  write_log_and_memo('InstallShield+MSI Setup detected');
end;


(*
procedure get_advancedmsi_info(myfilename: string; var mysetup: TSetupFile);
var
  myoutlines: TStringList;
  myreport: string;
  myexitcode: integer;
  myBatch: string;
  FileInfo: TSearchRec;
  //exefile: string;
  smask: string;
  mymsilist: TStringList;
  mymsifilename: string;

begin
  write_log_and_memo('Analyzing AdvancedMSI Setup: ' + myfilename);
  mysetup.installerId := stAdvancedMSI;
  mysetup.setupFileName := ExtractFileName(myfilename);

  {$IFDEF WINDOWS}
  // extract and analyze MSI from setup

  write_log_and_memo('Analyzing MSI from Setup ' + myfilename);

  myoutlines := TStringList.Create;
  myBatch := 'cmd.exe /C "' + myfilename + '" /extract:' + opsitmp;
  write_log_and_memo(myBatch);
  write_log_and_memo('!! PLEASE WAIT !!');
  write_log_and_memo('!! PLEASE WAIT !! Extracting and analyzing MSI ...');
  write_log_and_memo('!! PLEASE WAIT !!');

  if DirectoryExists(opsitmp) then
    DeleteDirectory(opsitmp, True);
  if not DirectoryExists(opsitmp) then
    ForceDirectories(opsitmp);
  if not DirectoryExists(opsitmp) then
    write_log_and_memo('Error: could not create directory: ' + opsitmp);

  if not RunCommandAndCaptureOut(myBatch, True, myoutlines, myreport,
    SW_SHOWMINIMIZED, myexitcode) then
  begin
    LogDatei.log('Failed to to run "' + myBatch + '": ' + myreport, LLWarning);
  end
  else
  begin
    smask := opsitmp + '*.msi';
    LogDatei.log('Looking for: ' + smask, LLInfo);
    try
      mymsilist := FindAllFiles(opsitmp, '*.msi', True);
      if mymsilist.Count > 0 then
      begin
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
  {$ENDIF WINDOWS}

  write_log_and_memo('get_AdvancedMSI_info finished');
  write_log_and_memo('Advanced Installer Setup (with embedded MSI) detected');
end;
*)
(*
procedure get_7zip_info(myfilename: string);
var
  product: string;
begin
  write_log_and_memo('Analyzing 7zip-Setup:');
  write_log_and_memo('get_7zip_info finished');
end;
*)

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

procedure get_genmsinstaller_info(myfilename: string; var mysetup: TSetupFile);
var
  str1, str2: string;
  pos1, pos2, i: integer;
begin
  write_log_and_memo('Analyzing generic MS Installer-Setup:');
  mysetup.uninstallProg := 'C:\ProgramData\{<UNKNOWN GUID>}\' +
    ExtractFileName(myfilename);
  write_log_and_memo('get_genmsinstaller_info finished');
end;

procedure get_wixtoolset_info(myfilename: string; var mysetup: TSetupFile);
var
  str1, str2: string;
  pos1, pos2, i: integer;
  myoutlines, extractedFiles: TStringList;
  destDir: string;
  myCommand: string;
  myreport: string;
  myexitcode : Integer;
  smask: string;
  mymsilist: TStringList;
  mymsifilename: string;
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
    DeleteDirectory(destDir,true);
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
    osdsettings.myexitcode:= myexitcode;
  end
  else
  begin
    osdsettings.myexitcode:= myexitcode;
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

    smask := opsitmp + '*.msi';
    LogDatei.log('Looking for: ' + smask, LLInfo);
    try
      mymsilist := FindAllFiles(destDir, '*.msi', True);
      if mymsilist.Count > 0 then
      begin
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
    {$ENDIF WINDOWS}
  write_log_and_memo('get_wixtoolset_info finished');
end;

procedure get_boxstub_info(myfilename: string; var mysetup: TSetupFile);
var
  str1, str2: string;
  pos1, pos2, i: integer;
begin
  write_log_and_memo('Analyzing generic boxstub-Setup:');
  mysetup.uninstallProg := 'C:\ProgramData\{<UNKNOWN GUID>}\' +
    ExtractFileName(myfilename);
  write_log_and_memo('get_boxstub_info finished');
end;

procedure get_sfxcab_info(myfilename: string; var mysetup: TSetupFile);
var
  str1, str2: string;
  pos1, pos2, i: integer;
begin
  write_log_and_memo('Analyzing sfxcabr-Setup:');
  mysetup.uninstallProg := 'C:\ProgramData\{<UNKNOWN GUID>}\' +
    ExtractFileName(myfilename);
  write_log_and_memo('get_sfxcab_info finished');
end;

procedure get_advancedInstaller_info(myfilename: string; var mysetup: TSetupFile);
var
  str1, str2: string;
  pos1, pos2, i: integer;
begin
  write_log_and_memo('Analyzing advancedInstaller:');
  mysetup.uninstallDirectory:= '"$installerSourceDir$\';
  mysetup.uninstallProg:= mysetup.uninstallDirectory + mysetup.setupFileName;
  if mysetup.preferSilent then
      mysetup.uninstallCommandLine :=
        '"' + mysetup.uninstallProg + '" ' +
        installerArray[integer(mysetup.installerId)].silentuninstall
    else
      mysetup.uninstallCommandLine :=
        '"' + mysetup.uninstallProg + '" ' +
        installerArray[integer(mysetup.installerId)].unattendeduninstall;
  write_log_and_memo('get_advancedInstaller_info finished');
end;

procedure get_installAnywhere_info(myfilename: string; var mysetup: TSetupFile);
var
  str1, str2: string;
  pos1, pos2, i: integer;
begin
  write_log_and_memo('Analyzing installAnywhere:');
  write_log_and_memo('get_installAnywhere_info finished');
end;

procedure get_QtInstaller_info(myfilename: string; var mysetup: TSetupFile);
var
  str1, str2: string;
  pos1, pos2, i: integer;
begin
  write_log_and_memo('Analyzing QtInstaller:');
  mysetup.installDirectory:= '"%Programfiles64Dir%\"+$ProductId$';
  write_log_and_memo('get_QtInstaller_info finished');
end;

// marker for add installers

(*
procedure get_bitrock_info(myfilename: string; var mysetup: TSetupFile);
var
  str1, str2: string;
  pos1, pos2, i: integer;
begin
  write_log_and_memo('Analyzing Windows Bitrock Installer:');
  write_log_and_memo('get_bitrock_info finished');
end;

procedure get_selfextrackting_info(myfilename: string; var mysetup: TSetupFile);
var
  str1, str2: string;
  pos1, pos2, i: integer;
begin
  write_log_and_memo('Analyzing selfextrackting Installer:');
  write_log_and_memo('get_selfextrackting_info finished');
end;
*)


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
  else
  begin
    setupType := analyze_binary(FileName, verbose, False, mysetup);

    sleep(2000);

    {$IFDEF OSDGUI}
    if setupType = stUnknown then
    begin
      FChooseInstallerDlg.ComboBoxChooseInstaller.Clear;
      for i := 0 to integer(stUnknown) do
        FChooseInstallerDlg.ComboBoxChooseInstaller.Items.Add(
          installerToInstallerstr(TKnownInstaller(i)));
      FChooseInstallerDlg.ComboBoxChooseInstaller.Text :=
        installerToInstallerstr(stUnknown);
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
      stInstallAware: get_installaware_info(FileName, mysetup);
      stMSGenericInstaller: get_genmsinstaller_info(FileName, mysetup);
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
         st7zipsfx,stSetupFactory}
    end;


    // marker for add installers
    tmpstr := installerToInstallerstr(setupType);
    case setupType of
      stMsi: ;// nothing to do here - see above;
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
    MyMessageDlg.showMessage('Warning','More than one installertype detected.' +LineEnding+
      'Please check log file.', [mrOK]);
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
      MyMessageDlg.wiMessageSized(tmpstr,[mrOk], 950, 740);
    {$ENDIF OSDGUI}
  end;
end;

end.
