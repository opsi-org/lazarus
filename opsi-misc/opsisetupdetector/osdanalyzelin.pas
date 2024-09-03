unit osdanalyzeLin;

{$mode objfpc}
{$H+}

interface

uses
  Dialogs,
  LCLType,
  Classes,
  Process,
  fileutil,
  lazfileutils,
  SysUtils,
  strutils,
  winpeimagereader,
  oslog,
  osdbasedata,
  osdanalyzegeneral,
  osparserhelper;

procedure get_aktProduct_general_info_lin(installerId: TKnownInstaller;
  myfilename: string; var mysetup: TSetupFile);

procedure get_rpm_info(myfilename: string; var mysetup: TSetupFile);
procedure get_deb_info(myfilename: string; var mysetup: TSetupFile);

procedure AnalyzeLin(FileName: string; var mysetup: TSetupFile; verbose: boolean);

implementation

uses
  {$IFDEF OSDGUI}
  osdform,
  {$ENDIF OSDGUI}
  osdmain;

procedure get_aktProduct_general_info_lin(installerId: TKnownInstaller;
  myfilename: string; var mysetup: TSetupFile);
var
  fsize: int64;
  fsizemb, rsizemb: double;
  sReqSize: string;
  sFileSize: string;
  product: string;
  installerstr: string;
  str1: string;
begin
  installerstr := installerToInstallerstr(installerId);
  write_log_and_memo('Analyzing ' + installerstr + ' Setup: ' + myfilename);

  mysetup.installerId := installerId;
  mysetup.link := installerArray[integer(mysetup.installerId)].Link;
  mysetup.SetSetupFullFileName(myfilename);
  mysetup.installerSourceDir := '%scriptpath%/files' + IntToStr(mysetup.ID);
  mysetup.installCommandLine :=
    'set $exitcode$ = shellCall(''' + '$installerSourceDir$ + ' +
    '"/' + mysetup.setupFileName;
  if mysetup.preferSilent then
    mysetup.installCommandLine :=
      mysetup.installCommandLine + '" ' +
      installerArray[integer(mysetup.installerId)].unattendedsetup + ''')'
  else
    mysetup.installCommandLine :=
      mysetup.installCommandLine + '" ' +
      installerArray[integer(mysetup.installerId)].silentsetup + ''')';
  str1 := '';
  // productId and name
  if str1 <> '' then
  begin
    aktProduct.productdata.productId := 'l-' + getPacketIDShort(str1);
    aktProduct.productdata.productName := str1;
  end
  else
  begin
    product := ExtractFileNameWithoutExt(mysetup.setupFileName);
    aktProduct.productdata.productId := 'l-' + getPacketIDShort(product);
    // try to strip version from filename
    str1 := copy(product, 0, pos('-', product) - 1);
    aktProduct.productdata.productName := str1;
  end;
  // installdir: Unknown
  // version
  str1 := ExtractVersionFromFilename(product);
  mysetup.SoftwareVersion := str1;
  aktProduct.productdata.productversion := trim(mysetup.SoftwareVersion);
  // get size
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

  // exitcode
  mysetup.isExitcodeFatalFunction :=
    installerArray[integer(mysetup.installerId)].uib_exitcode_function;

  // uninstall prog
  mysetup.uninstallProg :=
    installerArray[integer(mysetup.installerId)].uninstallProg;
  // uninstall
  mysetup.uninstallCheck.Clear;
  mysetup.uninstallCheck.Add('set $PackageName$ = "<unknown>"');
  mysetup.uninstallCheck.Add('if stringToBool(isOneInstalled(' +
    'CreateStringlist($PackageName$)))');
  mysetup.uninstallCheck.Add('	set $oldProgFound$ = "true"');
  mysetup.uninstallCheck.Add('endif');
  mysetup.uninstallCommandLine :=
    'set $exitcode$ = linuxRemoveOnePackage($PackageName$)';
  LogDatei.log('Finished with get_aktProduct_general_info_lin', LLinfo);
  ;
end; //get_aktProduct_general_info_lin



procedure get_rpm_info(myfilename: string; var mysetup: TSetupFile);
var
  tmpstr, outstr, packageId: string;
  fsize, fsizemb: longword;



  function getFieldInfoFromRpm(filename: string; fieldname: string): string;
  var
    outstr, cmd: string;
    commands: array of TProcessString;
    Options: TProcessOptions = [];
  begin
    Result := '';
    outstr := '';
    cmd := '/usr/bin/rpm';
    commands := ['-qp', '--queryformat', '"%{' + fieldname + '}\n"', filename];
    //-qp --queryformat "%{description}\n"
    Options := [poWaitOnExit, poNoConsole];
    if RunCommand(cmd, commands, outstr, Options, swoHIDE) then
    begin
      LogDatei.log('RPM-Info for: ' + fieldname + ' is: ' + outstr, LLnotice);
      outstr := opsiunquotestr2(outstr, '""');
      outstr := TrimRightSet(outstr, [#10, #13]);
      LogDatei.log('RPM-Info for: ' + fieldname + ' is: ' + outstr, LLnotice);
      Result := outstr;
    end;
  end;

begin
  LogDatei.log('Start with get_rpm_info', LLinfo);
  mysetup.installCommandLine :=
    'set $exitcode$ = linuxInstallOneFile($installerSourceDir$ + ' +
    '"/' + mysetup.setupFileName + '") ';
  {$IFDEF LINUX}
  mysetup.installDirectory := '<none>';
  // product ID
  packageId := getFieldInfoFromRpm(myfilename, 'name');
  aktProduct.productdata.productId := 'l-' + getPacketIDShort(packageId);
  // product name
  aktProduct.productdata.productName := packageId;
  // Softwareversion
  tmpstr := getFieldInfoFromRpm(myfilename, 'version');
  aktProduct.productdata.productversion := tmpstr;
  mysetup.SoftwareVersion := tmpstr;
  // Description
  aktProduct.productdata.description := getFieldInfoFromRpm(myfilename, 'description');
  // Advice
  tmpstr := getFieldInfoFromRpm(myfilename, 'Vendor');
  if tmpstr <> '' then outstr := 'Vendor: ' + tmpstr + LineEnding;
  tmpstr := getFieldInfoFromRpm(myfilename, 'License');
  if tmpstr <> '' then outstr := outstr + 'License: ' + tmpstr + LineEnding;
  tmpstr := getFieldInfoFromRpm(myfilename, 'Architecture');
  if tmpstr <> '' then outstr := outstr + 'Architecture: ' + tmpstr + LineEnding;
  tmpstr := getFieldInfoFromRpm(myfilename, 'Url');
  if tmpstr <> '' then outstr := outstr + 'Homepage: ' + tmpstr + LineEnding;
  aktProduct.productdata.advice := outstr;
  // size;
  tmpstr := getFieldInfoFromRpm(myfilename, 'Size');
  fsize := StrToInt64(tmpstr);
  fsizemb := fsize div (1024 * 1024);
  if fsizemb < 1 then
    fsizemb := 1;
  mysetup.requiredSpace := round(fsizemb);
  // uninstall
  mysetup.uninstallCheck.Clear;
  mysetup.uninstallCheck.Add('if stringToBool(isOneInstalled(' +
    'CreateStringlist("' + packageId + '")))');
  mysetup.uninstallCheck.Add('	set $oldProgFound$ = "true"');
  mysetup.uninstallCheck.Add('endif');
  mysetup.uninstallCommandLine :=
    'set $exitcode$ = linuxRemoveOnePackage("' + packageId + '")';
  {$ELSE LINUX}
  LogDatei.log('Detailed anlyze of rpm files can only be done at linux', LLWarning);
  {$IFDEF OSDGUI}
  MessageDlg(rsRpmAnalyze, rsRPMAnalyzeNotLinux,
    mtInformation, [mbOK], '');
  {$ENDIF OSDGUI}
  {$ENDIF LINUX}
  LogDatei.log('Finished with get_rpm_info', LLinfo);
end;

procedure get_deb_info(myfilename: string; var mysetup: TSetupFile);
var
  tmpstr, outstr, packageId: string;
  fsize, fsizemb: longword;



  function getFieldInfoFromDeb(filename: string; fieldname: string): string;
  var
    outstr, cmd: string;
    commands: array of TProcessString;
    Options: TProcessOptions = [];
  begin
    Result := '';
    outstr := '';
    cmd := '/usr/bin/dpkg-deb';
    commands := ['--field', filename, fieldname];
    Options := [poWaitOnExit, poNoConsole];
    if RunCommand(cmd, commands, outstr, Options, swoHIDE) then
    begin
      outstr := opsiunquotestr2(outstr, '""');
      outstr := TrimRightSet(outstr, [#10, #13]);
      Result := outstr;
    end;
  end;

begin
  LogDatei.log('Start with get_deb_info', LLinfo);
  mysetup.installDirectory := '<none>';
  mysetup.installCommandLine :=
    'set $exitcode$ = linuxInstallOneFile($installerSourceDir$ + ' +
    '"/' + mysetup.setupFileName + '") ';
  {$IFDEF LINUX}
  // product ID
  packageId := getFieldInfoFromDeb(myfilename, 'package');
  aktProduct.productdata.productId := 'l-' + getPacketIDShort(packageId);
  // product name
  aktProduct.productdata.productName := packageId;
  // Softwareversion
  tmpstr := getFieldInfoFromDeb(myfilename, 'version');
  aktProduct.productdata.productversion := tmpstr;
  mysetup.SoftwareVersion := tmpstr;
  // Description
  aktProduct.productdata.description := getFieldInfoFromDeb(myfilename, 'description');
  // Advice
  tmpstr := getFieldInfoFromDeb(myfilename, 'Vendor');
  if tmpstr <> '' then outstr := 'Vendor: ' + tmpstr + LineEnding;
  tmpstr := getFieldInfoFromDeb(myfilename, 'License');
  if tmpstr <> '' then outstr := outstr + 'License: ' + tmpstr + LineEnding;
  tmpstr := getFieldInfoFromDeb(myfilename, 'Architecture');
  if tmpstr <> '' then outstr := outstr + 'Architecture: ' + tmpstr + LineEnding;
  tmpstr := getFieldInfoFromDeb(myfilename, 'Homepage');
  if tmpstr <> '' then outstr := outstr + 'Homepage: ' + tmpstr + LineEnding;
  aktProduct.productdata.advice := outstr;
  // size;
  tmpstr := getFieldInfoFromDeb(myfilename, 'Installed-Size');
  fsize := StrToInt64(tmpstr);
  fsizemb := fsize div (1024 * 1024);
  if fsizemb < 1 then
    fsizemb := 1;
  mysetup.requiredSpace := round(fsizemb);
  // uninstall
  mysetup.uninstallCheck.Clear;
  mysetup.uninstallCheck.Add('if stringToBool(isOneInstalled(' +
    'CreateStringlist("' + packageId + '")))');
  mysetup.uninstallCheck.Add('	set $oldProgFound$ = "true"');
  mysetup.uninstallCheck.Add('endif');
  mysetup.uninstallCommandLine :=
    'set $exitcode$ = linuxRemoveOnePackage("' + packageId + '")';
  {$ELSE LINUX}
  LogDatei.log('Detailed anlyze of deb files can only be done at linux', LLWarning);
  {$IFDEF OSDGUI}
  MessageDlg(rsDebAnalyze, rsDebAnalyzeNotLinux,
    mtInformation, [mbOK], '');
  {$ENDIF OSDGUI}
  {$ENDIF LINUX}
  LogDatei.log('Finished with get_deb_info', LLinfo);
end;


procedure get_bitrock_info(myfilename: string; var mysetup: TSetupFile);
var
  str1, str2: string;
  pos1, pos2, i: integer;
begin
  write_log_and_memo('Analyzing Linux Bitrock Installer:');
  mysetup.installDirectory := '/opt/<product>/';
  if installerArray[integer(mysetup.installerId)].uninstallProg <> '' then
  begin
    // use the uninstall prog without extenstion
    mysetup.uninstallProg := ExtractFileNameOnly(
      installerArray[integer(mysetup.installerId)].uninstallProg);
    mysetup.uninstallCheck.Clear;
    str1 := 'if fileexists(''"' + mysetup.uninstallProg + '"'')';
    mysetup.uninstallCheck.Add(str1);

    mysetup.uninstallCheck.Add('	set $oldProgFound$ = "true"');
    mysetup.uninstallCheck.Add('endif');
    str1 := 'set $exitcode$ = shellCall(''"' + mysetup.uninstallProg;
    if mysetup.preferSilent then
      str1 := str1 + '" ' + installerArray[integer(mysetup.installerId)].unattendeduninstall + ''')'
    else
      str1 := str1 + '" ' + installerArray[integer(mysetup.installerId)].silentuninstall + ''')';
    mysetup.uninstallCommandLine := str1;
  end
  else
  begin
    // no known uninstall program
    mysetup.uninstallCheck.Add('set $oldProgFound$ = "false"');
  end;
  write_log_and_memo('get_bitrock_info finished');
end;

procedure get_linux_generic_info(myfilename: string; var mysetup: TSetupFile);
begin

end;

procedure AnalyzeLin(FileName: string; var mysetup: TSetupFile; verbose: boolean);
var
  setupType: TKnownInstaller;
  extension, tmpstr: string;
begin
  LogDatei.log('Start Analyze ... ', LLInfo);
  {$IFDEF OSDGUI}
  resultForm1.ProgressBarAnalyze.Position := 0;
  procmess;
  {$ENDIF OSDGUI}
  setupType := stUnknown;
  extension := lowercase(ExtractFileExt(FileName));
  case extension of
    '.rpm': setupType := stLinRPM;
    '.deb': setupType := stLinDeb;
    else
      setupType := stUnknown;
  end;
  mysetup.installerId := setupType;

  if setupType = stUnknown then
  begin
    setupType := analyze_binary(FileName, verbose, False, mysetup);
    mysetup.installerId := setupType;
  end;

  get_aktProduct_general_info_lin(setupType, Filename, mysetup);

  // marker for add installers
  case setupType of
    stLinRPM: get_rpm_info(Filename, mysetup);
    stLinDeb: get_deb_info(Filename, mysetup);
    stBitrock: get_bitrock_info(Filename, mysetup);
    stUnknown: LogDatei.log(
        'Unknown Installer after Analyze.', LLcritical);
    else
    begin
      LogDatei.log('Unexpected Setuptype in Analyze: ' +
        installerToInstallerstr(setupType) + '  :  ' +
        IntToStr(instIdToint(setupType)), LLWarning);
      // get_linux_generic_info(Filename, mysetup);
    end;
  end;


  // marker for add installers
  // stLinRPM, stLinDeb
  tmpstr := installerToInstallerstr(setupType);
  case setupType of
    stLinRPM: write_log_and_memo('Found installer= ' + tmpstr);
    stLinDeb: write_log_and_memo('Found installer= ' + tmpstr);
    stBitrock: write_log_and_memo('Found installer= ' + tmpstr);
    stUnknown: write_log_and_memo('Found installer= ' + tmpstr);
    else
      write_log_and_memo('Found installer= ' + tmpstr);
  end;
  { avoid hyphen char "-" and replace with dot "." in version }
  aktproduct.productdata.productversion :=
    StringReplace(aktproduct.productdata.productversion, '-', '.', [rfReplaceAll]);

  {$IFDEF OSDGUI}
  resultForm1.ProgressBarAnalyze.Position := 100;
  procmess;

  if not (setupType = stUnknown) then
  begin
    sleep(2000);
    resultform1.BtAnalyzeNextStepClick(nil);
  end;
  {$ENDIF OSDGUI}
end;


end.
