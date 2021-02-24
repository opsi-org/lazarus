unit osdanalyzewin;

{$mode delphi}

interface

uses
  {$IFDEF WINDOWS}
  Windows,
  ShlObj,
  Registry,
  verinfo,
  {$ENDIF WINDOWS}
  Dialogs,
  LCLType,
  Classes,
  osdhelper,
  Process,
  fileutil,
  lazfileutils,
  SysUtils,
  fileinfo,
  winpeimagereader,
  oslog,
  osdbasedata,
  osdanalyzegeneral,
  oscheckbinarybitness;


(*
const

  SetupType_AdvancedMSI = 'AdvancedMSI';
  SetupType_Inno = 'Inno';
  SetupType_InstallShield = 'InstallShield';
  SetupType_InstallShieldMSI = 'InstallShieldMSI';
  SetupType_MSI = 'MSI';
  SetupType_NSIS = 'NSIS';
  SetupType_7zip = '7zip';
*)

procedure get_aktProduct_general_info_win(installerId: TKnownInstaller;
  myfilename: string; var mysetup: TSetupFile);
procedure get_msi_info(myfilename: string; var mysetup: TSetupFile); overload;
procedure get_msi_info(myfilename: string; var mysetup: TSetupFile;
  uninstall_only: boolean); overload;
procedure get_inno_info(myfilename: string; var mysetup: TSetupFile);
procedure get_installshield_info(myfilename: string; var mysetup: TSetupFile);
procedure get_installshieldmsi_info(myfilename: string; var mysetup: TSetupFile);
procedure get_advancedmsi_info(myfilename: string; var mysetup: TSetupFile);
procedure get_nsis_info(myfilename: string; var mysetup: TSetupFile);
procedure get_installaware_info(myfilename: string; var mysetup: TSetupFile);
procedure get_genmsinstaller_info(myfilename: string; var mysetup: TSetupFile);
procedure get_wixtoolset_info(myfilename: string; var mysetup: TSetupFile);
procedure get_boxstub_info(myfilename: string; var mysetup: TSetupFile);
procedure get_sfxcab_info(myfilename: string; var mysetup: TSetupFile);
procedure get_bitrock_info(myfilename: string; var mysetup: TSetupFile);
procedure get_selfextrackting_info(myfilename: string; var mysetup: TSetupFile);
// marker for add installers
//procedure stringsgrep(myfilename: string; verbose,skipzero: boolean);
procedure Analyze(FileName: string; var mysetup: TSetupFile; verbose: boolean);
//procedure grepmsi(instring: string);
//procedure grepmarker(instring: string);
(*
function analyze_binary(myfilename: string; verbose, skipzero: boolean;
  var mysetup: TSetupFile): TKnownInstaller;

function getPacketIDfromFilename(str: string): string;
function getPacketIDShort(str: string): string;
function ExtractVersion(str: string): string;
*)
function getProductInfoFromResource(infokey: string; filename: string): string;

resourcestring
  sWarnMultipleMsi =
    'Multiple (more than one) msi files found. Look to log file and directory: ';

implementation

uses
  osdform;

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

(*
function getPacketIDfromFilename(str: string): string;
var
  strnew: string;
  i: integer;
  myChar: char;
begin
  strnew := '';
  for i := 1 to Length(str) do
  begin
    myChar := str[i];
    if myChar in ['A'..'Z', 'a'..'z', '0'..'9', '_', '-'] then
      strnew := strnew + myChar
    else
    if (myChar <> #195) then
      strnew := strnew + '-';
  end;
  Result := strnew;
end;


function getPacketIDShort(str: string): string;
var
  strnew: string;
  i: integer;
  myChar: char;
  preChar: char = ' ';
begin
  strnew := '';
  str := lowercase(str);
  for i := 1 to Length(str) do
  begin
    myChar := str[i];
    if myChar in ['a'..'z', '0'..'9', '_', '-'] then
    begin
      strnew := strnew + myChar;
      preChar := myChar;
    end
    else
    begin
      if ((myChar <> #195) and (i > 1) and (strnew[Length(strnew) - 1] <> '_') and
        (strnew[Length(strnew) - 1] <> '-') and (i < Length(str))) then
        if (preChar <> '-') then
        begin
          strnew := strnew + '-';
          preChar := '-';
        end;
    end;
  end;
  Result := strnew;
end;


function ExtractVersion(str: string): string;
var
  i: integer;
  outstr: string = '';
begin
  str := StringReplace(str, 'w32', '', [rfReplaceAll, rfIgnoreCase]);
  str := StringReplace(str, 'w64', '', [rfReplaceAll, rfIgnoreCase]);
  str := StringReplace(str, 'win32', '', [rfReplaceAll, rfIgnoreCase]);
  str := StringReplace(str, 'win64', '', [rfReplaceAll, rfIgnoreCase]);
  for i := 1 to Length(str) do
  begin
    if str[i] in ['0'..'9', '.'] then
    begin
      if (Length(outstr) > 0) and (not (str[i - 1] in ['0'..'9', '.'])) then
        outstr := '';
      outstr := outstr + str[i];
    end
    else;
  end;
  Result := outstr;
end;


function grepexe(instring: string): string;
var
  lowerstring: string;
begin
  Result := '';
  lowerstring := lowercase(instring);
  if (0 < pos('installshield', lowerstring)) or (0 < pos('inno', lowerstring)) or
    (0 < pos('wise', lowerstring)) or (0 < pos('nullsoft', lowerstring)) or
    (0 < pos('wixquery', lowerstring)) or
    (0 < pos('product_build_number{', lowerstring)) or
    (0 < pos('productcode{', lowerstring)) or (0 < pos('msiexec', lowerstring)) or
    (0 < pos('extract', lowerstring)) or
    // (0 < pos('setup', lowerstring)) or
    (0 < pos('installer', lowerstring)) then
    Result := instring;
end;

function grepinstr(instring: string; searchstr: string): string;
var
  lowerstring: string;
begin
  Result := '';
  lowerstring := lowercase(instring);
  if (0 < pos(lowercase(searchstr), lowerstring)) then
    Result := instring;
end;
*)
(*
procedure analyze_binstr(instring: string; var mysetup: TSetupFile);
var
  lowerstring: string;
  counter: integer;
  aktId: TKnownInstaller;


  procedure check_line_for_installer(line: string; instId: TKnownInstaller;
  var mysetup: TSetupFile);
  var
    i: integer;
  begin
    for i := 0 to installerArray[integer(instId)].patterns.Count - 1 do
    begin
      //LogDatei.log('check: ' + line + ' for: ' + installerToInstallerstr(instId), LLDebug2);
      if 0 <> pos(LowerCase(installerArray[integer(instId)].patterns[i]), line) then
      begin
        //aktProduct.markerlist.add(installerArray[integer(instId)].Name + IntToStr(i));
        mysetup.markerlist.add(installerArray[integer(instId)].patterns[i]);
        LogDatei.log('For: ' + installerToInstallerstr(instId) +
          ' found: ' + LowerCase(installerArray[integer(instId)].patterns[i]), LLinfo);
      end;
    end;
  end;

  procedure check_line_for_infoline(line: string; instId: TKnownInstaller;
  var mysetup: TSetupFile);
  var
    i: integer;
  begin
    for i := 0 to installerArray[integer(instId)].infopatterns.Count - 1 do
    begin
      //LogDatei.log('check: ' + line + ' for: ' + installerToInstallerstr(instId), LLDebug2);
      if 0 <> pos(LowerCase(installerArray[integer(instId)].infopatterns[i]), line) then
      begin
        //aktProduct.markerlist.add(installerArray[integer(instId)].Name + IntToStr(i));
        mysetup.infolist.add(line);
        LogDatei.log('For: ' + installerToInstallerstr(instId) +
          ' found info: ' + line, LLinfo);
      end;
    end;
  end;

begin
  lowerstring := lowercase(instring);
  for counter := 0 to knownInstallerList.Count - 1 do
  begin
    aktId := installerArray[counter].installerId;
    if aktId <> stUnknown then
    begin
      check_line_for_installer(lowerstring, aktId, mysetup);
      check_line_for_infoline(lowerstring, aktId, mysetup);
    end;
  end;
end;
*)
(*
procedure grepmsi(instring: string);
begin
  if (0 < pos('product_build_number{', lowercase(instring))) or
    (0 < pos('productcode{', lowercase(instring))) then
    mywrite(instring);
end;
*)
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
  str1: string;

begin
  installerstr := installerToInstallerstr(installerId);
  Mywrite('Analyzing ' + installerstr + ' Setup: ' + myfilename);

  mysetup.installerId := installerId;
  mysetup.link := installerArray[integer(mysetup.installerId)].Link;
  mysetup.setupFullFileName := myfilename;
  //mysetup.setupFileNamePath := ExtractFileDir(myfilename);
  mysetup.installCommandLine :=
    '"%scriptpath%\files' + IntToStr(mysetup.ID) + '\' + mysetup.setupFileName +
    '" ' + installerArray[integer(mysetup.installerId)].unattendedsetup;
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

  mywrite('Setup file size is: ' + sFileSize);
  mywrite('Estimated required space is: ' + sReqSize);
  mywrite('........');

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
    mysetup.uninstallCheck.Add('if fileexists($installdir$+"\' +
      mysetup.uninstallProg + '")');
    mysetup.uninstallCheck.Add('	set $oldProgFound$ = "true"');
    mysetup.uninstallCheck.Add('endif');
    mysetup.uninstallCommandLine :=
      '"$Installdir$\' + mysetup.uninstallProg + '" ' +
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
end; //get_aktProduct_general_info_win

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
  Mywrite('Analyzing MSI: ' + myfilename);
  {$IFDEF WINDOWS}
  mycommand := 'cmd.exe /C cscript.exe "' + ExtractFilePath(ParamStr(0)) +
    'utils\msiinfo.js" "' + myfilename + '"';
  mywrite(mycommand);
  myoutlines := TStringList.Create;
  if not RunCommandAndCaptureOut(mycommand, True, myoutlines, myreport,
    SW_SHOWMINIMIZED, myexitcode) then
  begin
    mywrite('Failed to analyze: ' + myreport);
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
    mywrite('........');
    for i := 0 to myoutlines.Count - 1 do
    begin
      mywrite(myoutlines.Strings[i]);
      mysetup.analyze_progess := mysetup.analyze_progess + 1;

      // sSearch := 'Manufacturer: ';
      // iPos := Pos (sSearch, myoutlines.Strings[i]);
      // if (iPos <> 0) then
      //   resultForm1.Edit2.Text := Copy(myoutlines.Strings[i], Length(sSearch)+1, Length(myoutlines.Strings[i])-Length(sSearch));

      sSearch := 'ProductName: ';
      iPos := Pos(sSearch, myoutlines.Strings[i]);
      if (iPos <> 0) then
        aktProduct.productdata.productName :=
          Copy(myoutlines.Strings[i], Length(sSearch) + 1,
          Length(myoutlines.Strings[i]) - Length(sSearch));

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
    if aktproduct.productdata.productversion = '' then
      aktproduct.productdata.productversion := trim(mysetup.softwareversion);
  end;
  myoutlines.Free;
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
  mycommand := 'bash -c ''msiinfo export "' + myfilename + '" Property''';
  mywrite(mycommand);
  myoutlines := TStringList.Create;
  if not RunCommandAndCaptureOut(mycommand, True, myoutlines, myreport,
    SW_SHOWMINIMIZED, myexitcode) then
  begin
    mywrite('Failed to analyze: ' + myreport);
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

      // sSearch := 'Manufacturer: ';
      // iPos := Pos (sSearch, myoutlines.Strings[i]);
      // if (iPos <> 0) then
      //   resultForm1.Edit2.Text := Copy(myoutlines.Strings[i], Length(sSearch)+1, Length(myoutlines.Strings[i])-Length(sSearch));

      sSearch := 'ProductName';
      iPos := Pos(sSearch, myoutlines.Strings[i]);
      if (iPos <> 0) then
      begin
        aktProduct.productdata.productName :=
          Copy(myoutlines.Strings[i], Length(sSearch) + 1,
          Length(myoutlines.Strings[i]) - Length(sSearch));
        LogDatei.log('Found ProductName: ' +
          aktProduct.productdata.productName, LLNotice);
      end;

      sSearch := 'ProductVersion';
      iPos := Pos(sSearch, myoutlines.Strings[i]);
      if (iPos <> 0) then
      begin
        mysetup.softwareversion :=
          Copy(myoutlines.Strings[i], Length(sSearch) + 1,
          Length(myoutlines.Strings[i]) - Length(sSearch));
        LogDatei.log('Found ProductVersion: ' + mysetup.softwareversion, LLNotice);
      end;

      sSearch := 'ProductCode';
      iPos := Pos(sSearch, myoutlines.Strings[i]);
      if (iPos <> 0) then
      begin
        mysetup.msiId :=
          Copy(myoutlines.Strings[i], Length(sSearch) + 1,
          Length(myoutlines.Strings[i]) - Length(sSearch));
        LogDatei.log('Found ProductCode: ' + mysetup.msiId, LLNotice);
      end;

    end;
    if aktproduct.productdata.productversion = '' then
      aktproduct.productdata.productversion := trim(mysetup.softwareversion);
  end;
  myoutlines.Free;
  {$ENDIF LINUX}
  if not uninstall_only then
  begin
    mysetup.installCommandLine :=
      'msiexec /i "%scriptpath%\files' + IntToStr(mysetup.ID) + '\' +
      mysetup.setupFileName + '" ' + installerArray[integer(mysetup.installerId)].unattendedsetup;
    mysetup.mstAllowed := True;
  end;
  mysetup.uninstallCheck.Clear;
  mysetup.uninstallCheck.Add('if stringtobool(checkForMsiProduct("' +
    mysetup.msiId + '"))');
  mysetup.uninstallCheck.Add('	set $oldProgFound$ = "true"');
  mysetup.uninstallCheck.Add('endif');

  mysetup.uninstallCommandLine :=
    'msiexec /x ' + mysetup.msiId + ' ' + installerArray[integer(stMsi)].unattendeduninstall;


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
  fISS: Text;
  // components from install_script.iss[setup]
  AppName: string = '';
  AppVersion: string = '';
  AppVerName: string = '';
  DefaultDirName: string = '';
  tmpint: integer;
  ArchitecturesAllowed : string = '';
  ArchitecturesInstallIn64BitMode : string = '';

begin
  Mywrite('Analyzing Inno-Setup:');
  AppName := '';
  AppVersion := '';
  AppVerName := '';
  DefaultDirName := '';
  ArchitecturesInstallIn64BitMode := '';
  ArchitecturesAllowed := '';
  {$IFDEF WINDOWS}

  myoutlines := TStringList.Create;
  destDir := GetTempDir(False);
  destDir := destDir + DirectorySeparator + 'INNO';
  if not DirectoryExists(destDir) then
    CreateDir(destDir);
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
      if (0 < pos('x64', lowercase(ArchitecturesInstallIn64BitMode)))
         and (0 = pos('x86', lowercase(ArchitecturesInstallIn64BitMode))) then
      begin
        if pos('{pf}', DefaultDirName) > 0 then
        begin
          DefaultDirName := StringReplace(DefaultDirName, '{pf}',
            '%ProgramFiles64Dir%', [rfReplaceAll, rfIgnoreCase]);
        end;
        if pos('{autopf}', DefaultDirName) > 0 then
        begin
          DefaultDirName := StringReplace(DefaultDirName, '{autopf}',
            '%ProgramFiles64Dir%', [rfReplaceAll, rfIgnoreCase]);
        end;
        if pos('{code:DefDirRoot}', DefaultDirName) > 0 then
        begin
          DefaultDirName := StringReplace(DefaultDirName, '{code:DefDirRoot}',
            '%ProgramFiles64Dir%', [rfReplaceAll, rfIgnoreCase]);
        end;
         if pos('{code:installDir}', DefaultDirName) > 0 then
        begin
          DefaultDirName := StringReplace(DefaultDirName, '{code:installDir}',
            '%ProgramFiles64Dir%\<unknown>', [rfReplaceAll, rfIgnoreCase]);
        end;
        if pos('{code:DefaultDirName}', DefaultDirName) > 0 then
        begin
          DefaultDirName := StringReplace(DefaultDirName, '{code:DefaultDirName}',
            '%ProgramFiles64Dir%\<unknown>', [rfReplaceAll, rfIgnoreCase]);
        end;
      end
      else
      begin
        if pos('{pf}', DefaultDirName) > 0 then
        begin
          DefaultDirName := StringReplace(DefaultDirName, '{pf}',
            '%ProgramFiles32Dir%', [rfReplaceAll, rfIgnoreCase]);
        end;
        if pos('{autopf}', DefaultDirName) > 0 then
        begin
          DefaultDirName := StringReplace(DefaultDirName, '{autopf}',
            '%ProgramFiles32Dir%', [rfReplaceAll, rfIgnoreCase]);
        end;
        if pos('{code:DefDirRoot}', DefaultDirName) > 0 then
        begin
          DefaultDirName := StringReplace(DefaultDirName, '{code:DefDirRoot}',
            '%ProgramFiles32Dir%', [rfReplaceAll, rfIgnoreCase]);
        end;
        if pos('{code:installDir}', DefaultDirName) > 0 then
        begin
          DefaultDirName := StringReplace(DefaultDirName, '{code:installDir}',
            '%ProgramFiles32Dir%\<unknown>', [rfReplaceAll, rfIgnoreCase]);
        end;
        if pos('{code:DefaultDirName}', DefaultDirName) > 0 then
        begin
          DefaultDirName := StringReplace(DefaultDirName, '{code:DefaultDirName}',
            '%ProgramFiles32Dir%\<unknown>', [rfReplaceAll, rfIgnoreCase]);
        end;
      end;
      DefaultDirName := StringReplace(DefaultDirName, '{sd}',
        '%Systemdrive%', [rfReplaceAll, rfIgnoreCase]);
      mysetup.installDirectory := DefaultDirName;
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
  mywrite('get_inno_info finished');
  mywrite('Inno Setup detected');
  Mywrite('Finished analyzing Inno-Setup');
end;


procedure get_installshield_info(myfilename: string; var mysetup: TSetupFile);
var
  product: string;

begin
  Mywrite('Analyzing InstallShield-Setup:');
  mywrite('get_InstallShield_info finished');
  mywrite('InstallShield Setup detected');
end;


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
  Mywrite('Analyzing InstallShield+MSI Setup: ' + myfilename);
  mysetup.installerId := stInstallShieldMSI;
  mysetup.setupFullFileName := myfilename;
  //&mysetup.setupFileNamePath := ExtractFileDir(myfilename);

  if DirectoryExists(opsitmp) then
    DeleteDirectory(opsitmp, True);
  if not DirectoryExists(opsitmp) then
    createdir(opsitmp);
  if not DirectoryExists(opsitmp) then
    LogDatei.log('Error: could not create directory: ' + opsitmp, LLError);
  {$IFDEF WINDOWS}
  // extract and analyze MSI from setup

  Mywrite('Analyzing MSI from InstallShield Setup ' + myfilename);

  myoutlines := TStringList.Create;
  // myBatch := 'cmd.exe /C '+ExtractFilePath(paramstr(0))+'extractMSI.cmd "'+myfilename+'"'; // (does not work with spaces in EXE path)
  myBatch := '"' + ExtractFilePath(ParamStr(0)) + 'utils' + PathDelim +
    'extractMSI.cmd" "' + myfilename + '"';
  // dropped cmd.exe
  Mywrite(myBatch);
  Mywrite('!! PLEASE WAIT !!');
  Mywrite('!! PLEASE WAIT !! Extracting and analyzing MSI ...');
  Mywrite('!! PLEASE WAIT !!');

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
  mywrite('get_InstallShield_info finished');
  mywrite('InstallShield+MSI Setup detected');
end;



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
  Mywrite('Analyzing AdvancedMSI Setup: ' + myfilename);
  mysetup.installerId := stAdvancedMSI;
  mysetup.setupFileName := ExtractFileName(myfilename);
  //mysetup.setupFileNamePath := ExtractFileDir(myfilename);

  {$IFDEF WINDOWS}
  // extract and analyze MSI from setup

  Mywrite('Analyzing MSI from Setup ' + myfilename);

  myoutlines := TStringList.Create;
  myBatch := 'cmd.exe /C "' + myfilename + '" /extract:' + opsitmp;
  Mywrite(myBatch);
  Mywrite('!! PLEASE WAIT !!');
  Mywrite('!! PLEASE WAIT !! Extracting and analyzing MSI ...');
  Mywrite('!! PLEASE WAIT !!');

  if DirectoryExists(opsitmp) then
    DeleteDirectory(opsitmp, True);
  if not DirectoryExists(opsitmp) then
    createdir(opsitmp);
  if not DirectoryExists(opsitmp) then
    mywrite('Error: could not create directory: ' + opsitmp);

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

  mywrite('get_AdvancedMSI_info finished');
  mywrite('Advancd Installer Setup (with embedded MSI) detected');
end;


procedure get_nsis_info(myfilename: string; var mysetup: TSetupFile);

begin
  Mywrite('Analyzing NSIS-Setup:');
  mywrite('get_nsis_info finished');
  mywrite('NSIS (Nullsoft Install System) detected');
end;

procedure get_7zip_info(myfilename: string);
var
  product: string;
begin
  Mywrite('Analyzing 7zip-Setup:');
  mywrite('get_7zip_info finished');
end;

procedure get_installaware_info(myfilename: string; var mysetup: TSetupFile);
var
  str1, str2: string;
  pos1, pos2, i: integer;
begin
  Mywrite('Analyzing InstallAware-Setup:');
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
      mysetup.uninstallCommandLine :=
        mysetup.uninstallProg + ' ' +
        installerArray[integer(mysetup.installerId)].unattendeduninstall;
    end;
  end;
  mywrite('get_installaware_info finished');
end;

procedure get_genmsinstaller_info(myfilename: string; var mysetup: TSetupFile);
var
  str1, str2: string;
  pos1, pos2, i: integer;
begin
  Mywrite('Analyzing generic MS Installer-Setup:');
  //mysetup.install_waitforprocess:=ExtractFileName(myfilename);
  //mysetup.SoftwareVersion := getProductInfoFromResource('FileVersion',myfilename);
  mysetup.uninstallProg := 'C:\ProgramData\{<UNKNOWN GUID>}\' +
    ExtractFileName(myfilename);
  mywrite('get_genmsinstaller_info finished');
end;

procedure get_wixtoolset_info(myfilename: string; var mysetup: TSetupFile);
var
  str1, str2: string;
  pos1, pos2, i: integer;
  myoutlines, extractedFiles: TStringList;
  destDir: string;
  myCommand: string;
  myreport: string;
  smask: string;
  mymsilist: TStringList;
  mymsifilename: string;
begin
  Mywrite('Analyzing generic wixtoolset-Setup:');
  mysetup.uninstallProg := 'C:\ProgramData\{<UNKNOWN GUID>}\' +
    ExtractFileName(myfilename);

  Mywrite('Analyzing Wix Bundle:');
  {$IFDEF WINDOWS}

  myoutlines := TStringList.Create;
  destDir := GetTempDir(False);
  destDir := destDir + DirectorySeparator + 'wixbundle';
  if not DirectoryExists(destDir) then
    CreateDir(destDir);

  LogDatei.log('extract files from ' + myfilename + ' to' + destDir, LLInfo);
  myCommand := '"' + ExtractFilePath(ParamStr(0)) + 'utils' + PathDelim +
    'wixbin' + PathDelim + 'dark.exe" -x "' + destDir + '" ' + myfilename;
  Mywrite(myCommand);

  if not RunCommandAndCaptureOut(myCommand, True, myoutlines, myreport,
    SW_SHOWMINIMIZED, myexitcode) then
  begin
    LogDatei.log('Failed to extract files from wix bundle: ' + myreport, LLerror);
  end
  else
  begin
    for i := 0 to myoutlines.Count - 1 do
      mywrite(myoutlines.Strings[i]);
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
  mywrite('get_wixtoolset_info finished');
end;

procedure get_boxstub_info(myfilename: string; var mysetup: TSetupFile);
var
  str1, str2: string;
  pos1, pos2, i: integer;
begin
  Mywrite('Analyzing generic boxstub-Setup:');
  mysetup.uninstallProg := 'C:\ProgramData\{<UNKNOWN GUID>}\' +
    ExtractFileName(myfilename);
  mywrite('get_boxstub_info finished');
end;

procedure get_sfxcab_info(myfilename: string; var mysetup: TSetupFile);
var
  str1, str2: string;
  pos1, pos2, i: integer;
begin
  Mywrite('Analyzing sfxcabr-Setup:');
  mysetup.uninstallProg := 'C:\ProgramData\{<UNKNOWN GUID>}\' +
    ExtractFileName(myfilename);
  mywrite('get_sfxcab_info finished');
end;

procedure get_bitrock_info(myfilename: string; var mysetup: TSetupFile);
var
  str1, str2: string;
  pos1, pos2, i: integer;
begin
  Mywrite('Analyzing Windows Bitrock Installer:');
  mywrite('get_bitrock_info finished');
end;

procedure get_selfextrackting_info(myfilename: string; var mysetup: TSetupFile);
var
  str1, str2: string;
  pos1, pos2, i: integer;
begin
  Mywrite('Analyzing selfextrackting Installer:');
  mywrite('get_selfextrackting_info finished');
end;

// marker for add installers
(*
function analyze_markerlist(var mysetup: TSetupFile): TKnownInstaller;
var
  i: integer;

begin
  try
    Result := stUnknown;
    for i := 0 to mysetup.markerlist.Count - 1 do
      LogDatei.log('marker: ' + mysetup.markerlist[i], LLdebug);
    for i := 0 to integer(stUnknown) - 1 do
    begin
      if not Assigned(installerArray[i].detected) then
        LogDatei.log('No check implemented for: ' +
          installerToInstallerstr(TKnownInstaller(i)), LLWarning)
      else
      begin
        LogDatei.log('Check markerlist for: ' + installerToInstallerstr(
          TKnownInstaller(i)), LLInfo);
        if installerArray[i].detected(TClass(installerArray[i]),
          mysetup.markerlist) then
        begin
          Result := TKnownInstaller(i);
          LogDatei.log('Detected: ' + installerToInstallerstr(Result), LLnotice);
        end;
      end;
    end;

  except
    on E: Exception do
    begin
      LogDatei.log('Exception in analyze_markerlist', LLcritical);
      LogDatei.log('Error: Message: ' + E.message, LLcritical);
    end;
  end;
end;
*)
(*
function analyze_binary(myfilename: string; verbose, skipzero: boolean;
  var mysetup: TSetupFile): TKnownInstaller;
var
  FileStream: TFileStream;
  CharIn: char;
  MinLen, MaxLen: integer;
  CurrValue: string;
  i: integer;
  size, fullsize: int64;
  buffer: array [0 .. 2047] of char;
  charsread: int64;
  msg: string;
  setuptype: TKnownInstaller;
  progress, lastprogress: int64;

begin
  MinLen := 5;
  MaxLen := 512;
  CurrValue := '';
  setupType := stUnknown;
  Result := stUnknown;

  mywrite('------------------------------------');
  Mywrite('Analyzing: ' + myfilename);
  msg := 'stringsgrep started (verbose:';
  if verbose = True then
    msg := msg + 'true'
  else
    msg := msg + 'false';
  msg := msg + ', skipzero:';
  if skipzero = True then
    msg := msg + 'true'
  else
    msg := msg + 'false';
  msg := msg + ')';
  mywrite(msg);
  FileStream := TFileStream.Create(myfilename, fmOpenRead);
  try
    {$IFDEF OSDGUI}
    resultForm1.ProgressBarAnalyze.Position := 0;
    procmess;
        {$ENDIF OSDGUI}
    fullsize := FileStream.Size;
    size := fullsize;
    lastprogress := 0;
    progress := 0;
    while (size > 0) and (setupType = stUnknown) do
    begin
      charsread := FileStream.Read(buffer, sizeof(buffer));
      size := size - charsread;
       {$IFDEF OSDGUI}
      progress := 100 - trunc((size / fullsize) * 100);
      if progress > lastprogress then
      begin
        resultForm1.ProgressBarAnalyze.Position := progress;
        procmess;
        LogDatei.log('AnaProgess: ' + IntToStr(progress), LLDebug);
        lastprogress := progress;
      end;
       {$ENDIF OSDGUI}

      for i := 0 to charsread - 1 do
      begin

        charIn := buffer[i];

        // skipzero: handling of wide strings by ignoring zero byte
        if skipzero and (CharIn = #0) then
          continue;

        // if (CharIn in [' ','A'..'Z','a'..'z','0'..'9','<','>','.','/','_','-']) and (Length(CurrValue) < MaxLen) then
        if (CharIn in [#32..#126]) and (Length(CurrValue) < MaxLen) then
          CurrValue := CurrValue + CharIn;

        if (Length(CurrValue) < MaxLen) and (i < charsread - 1) then
          continue;

        if (Length(CurrValue) >= MinLen) then
        begin
          if '.exe' = lowercase(ExtractFileExt(myfilename)) then
          begin
            if verbose then
            begin
              //grepexe(CurrValue);
              analyze_binstr(CurrValue, mysetup);
              logdatei.log(CurrValue, LLDebug2);
            end
            else
              analyze_binstr(CurrValue, mysetup);
          end
          else if '.msi' = lowercase(ExtractFileExt(myfilename)) then
          begin
            setupType := stMsi;
            if verbose then
            begin
              grepmsi(CurrValue);
              logdatei.log(CurrValue, LLDebug2);
            end;
          end
          else
          begin
            grepexe(CurrValue);
            grepmsi(CurrValue);
            logdatei.log(CurrValue, LLDebug2);
          end;
          CurrValue := '';
        end;
      end;

    end;
    msg := 'stringsgrep completed (verbose:';
    if verbose = True then
      msg := msg + 'true'
    else
      msg := msg + 'false';
    msg := msg + ', skipzero:';
    if skipzero = True then
      msg := msg + 'true'
    else
      msg := msg + 'false';
    msg := msg + ')';
    mywrite(msg);
    mywrite('------------------------------------');
  finally
    FileStream.Free;
  end;
  Result := analyze_markerlist(mysetup);
end;
*)

procedure Analyze(FileName: string; var mysetup: TSetupFile; verbose: boolean);
var
  setupType: TKnownInstaller;
  tmpstr : string;

begin
  LogDatei.log('Start Analyze ... ', LLInfo);
  {$IFDEF OSDGUI}
  resultForm1.ProgressBarAnalyze.Position := 0;
  procmess;
  {$ENDIF OSDGUI}
  //aktProduct.setup32FileNamePath := FileName;
  //resultform1.clearAllTabs;
  setupType := stUnknown;
  if '.msi' = lowercase(ExtractFileExt(FileName)) then
  begin
    mysetup.analyze_progess := 10;
    setupType := stMsi;
    get_aktProduct_general_info_win(stMsi, Filename, mysetup);
    get_msi_info(FileName, mysetup);
    Mywrite('Found well known installer: ' + installerToInstallerstr(setupType));
  end
  else
  begin
    //stringsgrep(FileName, false, false); // filename, verbose, skipzero
    setupType := analyze_binary(FileName, verbose, False, mysetup);
    // filename, verbose, skipzero

    get_aktProduct_general_info_win(setupType, Filename, mysetup);

    // marker for add installers
    case setupType of
      stInno: get_inno_info(FileName, mysetup);
      stNsis: get_nsis_info(FileName, mysetup);
      stInstallShield: get_installshield_info(FileName, mysetup);
      stInstallShieldMSI: get_installshieldmsi_info(FileName, mysetup);
      stAdvancedMSI: get_advancedmsi_info(FileName, mysetup);
      st7zip: get_7zip_info(FileName);
      stMsi: ;// nothing to do here - see above;
      st7zipsfx: logdatei.log('no getinfo implemented for: ' +
          installerToInstallerstr(setupType), LLWarning);
      stInstallAware: get_installaware_info(FileName, mysetup);
      stMSGenericInstaller: get_genmsinstaller_info(FileName, mysetup);
      stWixToolset: get_wixtoolset_info(FileName, mysetup);
      stBoxStub: get_boxstub_info(FileName, mysetup);
      stSFXcab: get_sfxcab_info(FileName, mysetup);
      stBitrock: get_bitrock_info(FileName, mysetup);
      stSelfExtractingInstaller: get_selfextrackting_info(FileName, mysetup);
      stUnknown: LogDatei.log(
          'Unknown Installer after Analyze.', LLcritical);
      else
        LogDatei.log('Unknown Setuptype in Analyze: ' + IntToStr(
          instIdToint(setupType)), LLcritical);
    end;


    // marker for add installers
    tmpstr := installerToInstallerstr(setupType);
    case setupType of
      stInno: Mywrite('Found well known installer: ' + tmpstr);
      stNsis: Mywrite('Found well known installer: ' + tmpstr);
      stInstallShield: Mywrite('Found well known installer: ' + tmpstr);
      stInstallShieldMSI: Mywrite('Found well known installer: ' + tmpstr);
      stAdvancedMSI: Mywrite('Found well known installer: ' + tmpstr);
      st7zip: Mywrite('Found well known installer: ' + tmpstr);
      stMsi: ;// nothing to do here - see above;
      st7zipsfx: Mywrite('Found well known installer: ' + tmpstr);
      stInstallAware: Mywrite('Found well known installer: ' + tmpstr);
      stMSGenericInstaller: Mywrite('Found well known installer: ' + tmpstr);
      stWixToolset: Mywrite('Found well known installer: ' + tmpstr);
      stBoxStub: Mywrite('Found well known installer: ' + tmpstr);
      stSFXcab: Mywrite('Found well known installer: ' + tmpstr);
      stBitrock: Mywrite('Found well known installer: ' + tmpstr);
      stSelfExtractingInstaller: Mywrite('Found well known installer: ' + tmpstr);
      stUnknown: Mywrite('Sorry - unknown installer: ' + tmpstr);
      else
        Mywrite('Sorry - unknown installer: ' + tmpstr);
    end;
    { avoid hyphen char "-" and replace with dot "." in version }
    aktproduct.productdata.productversion := StringReplace(aktproduct.productdata.productversion,'-','.',[rfReplaceAll]);

  end;
  {$IFDEF OSDGUI}
  resultForm1.ProgressBarAnalyze.Position := 100;
  procmess;
  {$ENDIF OSDGUI}
  if not (setupType = stUnknown) then
  begin
    sleep(2000);
    resultform1.BtAnalyzeNextStepClick(nil);
  end;
end;




end.
