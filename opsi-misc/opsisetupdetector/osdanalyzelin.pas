unit osdanalyzeLin;

{$mode objfpc}
{$H+}

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
  strutils,
  fileinfo,
  winpeimagereader,
  oslog,
  osdbasedata,
  oscheckbinarybitness,
  osdanalyzegeneral,
  osparserhelper;

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


procedure get_aktProduct_general_info_lin(installerId: TKnownInstaller;
  myfilename: string; var mysetup: TSetupFile);

procedure get_rpm_info(myfilename: string; var mysetup: TSetupFile);
procedure get_deb_info(myfilename: string; var mysetup: TSetupFile);

procedure AnalyzeLin(FileName: string; var mysetup: TSetupFile; verbose: boolean);

//function getPacketIDfromFilename(str: string): string;
//function getPacketIDShort(str: string): string;


(*
// marker for add installers
//procedure stringsgrep(myfilename: string; verbose,skipzero: boolean);
*)

(*
procedure grepmsi(instring: string);
//procedure grepmarker(instring: string);
function analyze_binary(myfilename: string; verbose, skipzero: boolean;
  var mysetup: TSetupFile): TKnownInstaller;
function getPacketIDfromFilename(str: string): string;
function getPacketIDShort(str: string): string;
function ExtractVersion(str: string): string;
function getProductInfoFromResource(infokey: string; filename: string): string;

resourcestring
  sWarnMultipleMsi =
    'Multiple (more than one) msi files found. Look to log file and directory: ';
*)
implementation

uses
  osdform;

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

function ExtractVersionFromFilename(str: string): string;
var
  i: integer;
  outstr: string = '';
begin
  // try to strip productId from filename
  str := copy(str, pos('-', str), length(str));
  str := StringReplace(str, '_', '.', [rfReplaceAll, rfIgnoreCase]);
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

*)


procedure get_aktProduct_general_info_lin(installerId: TKnownInstaller;
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
  mysetup.installCommandLine :=  'set $exitcode$ = shellCall(''' +
    '"%scriptpath%/files' + IntToStr(mysetup.ID) + '/' + mysetup.setupFileName +
    '" ' + installerArray[integer(mysetup.installerId)].unattendedsetup +''')';
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
  // installdir
    // Unknown
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

  mywrite('Setup file size is: ' + sFileSize);
  mywrite('Estimated required space is: ' + sReqSize);
  mywrite('........');

  if fsizemb < 1 then
    fsizemb := 1;

  sFileSize := FormatFloat('#', fsizemb) + ' MB';
  sReqSize := FormatFloat('#', rsizemb) + ' MB';
  mysetup.requiredSpace := round(rsizemb);
  mysetup.setupFileSize := round(fsizemb);

  // exitcode
  mysetup.isExitcodeFatalFunction :=
    installerArray[integer(mysetup.installerId)].uib_exitcode_function;

  // uninstall
  mysetup.uninstallProg:=
    installerArray[integer(mysetup.installerId)].uninstallProg;
  // uninstallcheck
    // nothing yet
  LogDatei.log('Finished with get_aktProduct_general_info_lin',LLinfo);;
end; //get_aktProduct_general_info_lin



procedure get_rpm_info(myfilename: string; var mysetup: TSetupFile);
var
  tmpstr, outstr, packageId : string;
  fsize, fsizemb : Longword;



  function getFieldInfoFromRpm(filename : string; fieldname : string) : string;
  var
    outstr, cmd : string;
    commands:array of TProcessString;
    Options : TProcessOptions = [];
  begin
    result := '';
    outstr := '';
    cmd := '/usr/bin/rpm';
    commands := ['-qp','--queryformat', '"%{'+fieldname+'}\n"', filename];  //-qp --queryformat "%{description}\n"
    Options :=  [poWaitOnExit,poNoConsole];
    (*
    if not FileExists(cmd) then
      // we are on Linux but no dpkg - lets try to install it
      getCommandResult(cmd: string)
      *)
    if RunCommand(cmd,commands,outstr,Options,swoHIDE) then
    begin
      LogDatei.log('RPM-Info for: '+fieldname+' is: '+outstr,LLnotice);
      outstr := opsiunquotestr2(outstr,'""');
      //outstr := StringReplace(outstr,'#10','',[rfReplaceAll]);
      outstr :=  TrimRightSet(outstr, [#10,#13]);
      LogDatei.log('RPM-Info for: '+fieldname+' is: '+outstr,LLnotice);
    result := outstr;
    end;
  end;

begin
  LogDatei.log('Start with get_rpm_info',LLinfo);
  mysetup.installCommandLine :=
    'set $exitcode$ = linuxInstallOneFile(' + '"%scriptpath%/files' +
    IntToStr(mysetup.ID) + '/' + mysetup.setupFileName + '") ';
  try
    {$IFDEF LINUX}
    mysetup.installDirectory:= '<none>';
  // product ID
  packageId := getFieldInfoFromRpm(myfilename,'name');
  aktProduct.productdata.productId:= 'l-'+getPacketIDShort(packageId);
  // product name
  aktProduct.productdata.productName:= packageId;
  // Softwareversion
  tmpstr := getFieldInfoFromRpm(myfilename,'version');
  aktProduct.productdata.productversion:= tmpstr;
  mysetup.SoftwareVersion:= tmpstr;
  // Description
  aktProduct.productdata.description := getFieldInfoFromRpm(myfilename,'description');
  // Advice
  tmpstr := getFieldInfoFromRpm(myfilename,'Vendor');
  if tmpstr <> '' then outstr := 'Vendor: '+tmpstr  + LineEnding;
  tmpstr := getFieldInfoFromRpm(myfilename,'License');
  if tmpstr <> '' then outstr := outstr + 'License: '+tmpstr  + LineEnding;
   tmpstr := getFieldInfoFromRpm(myfilename,'Architecture');
  if tmpstr <> '' then outstr := outstr + 'Architecture: '+tmpstr  + LineEnding;
  tmpstr := getFieldInfoFromRpm(myfilename,'Url');
  if tmpstr <> '' then outstr := outstr + 'Homepage: '+tmpstr  + LineEnding;
  aktProduct.productdata.advice := outstr;
  // size;
  tmpstr := getFieldInfoFromRpm(myfilename,'Size');
  fsize := StrToInt64(tmpstr);
  fsizemb := fsize div (1024 * 1024);
  if fsizemb < 1 then
    fsizemb := 1;
  mysetup.requiredSpace := round(fsizemb);
    // uninstall
  mysetup.uninstallCheck.Add('if stringToBool(isOneInstalled('
    + 'CreateStringlist("'+packageId+'")))');
    mysetup.uninstallCheck.Add('	set $oldProgFound$ = "true"');
    mysetup.uninstallCheck.Add('endif');
  mysetup.uninstallCommandLine:= 'set $exitcode$ = linuxRemoveOnePackage("'
   + packageId +'")';
  {$ELSE LINUX}
  LogDatei.log('Detailed anlyze of rpm files can only be done at linux',LLWarning);
  MessageDlg(rsRpmAnalyze, rsRPMAnalyzeNotLinux,
    mtInformation, [mbOK], '');
  {$ENDIF LINUX}
  finally
    //FreeAndNil(outlist);
  end;
  LogDatei.log('Finished with get_rpm_info',LLinfo);
end;

procedure get_deb_info(myfilename: string; var mysetup: TSetupFile);
var
  tmpstr, outstr, packageId : string;
  fsize, fsizemb : Longword;



  function getFieldInfoFromDeb(filename : string; fieldname : string) : string;
  var
    outstr, cmd : string;
    commands:array of TProcessString;
    Options : TProcessOptions = [];
  begin
    result := '';
    outstr := '';
    cmd := '/usr/bin/dpkg-deb';
    commands := ['--field', filename, fieldname];
    Options :=  [poWaitOnExit,poNoConsole];
    (*
    if not FileExists(cmd) then
      // we are on Linux but no dpkg - lets try to install it
      getCommandResult(cmd: string)
      *)
    if RunCommand(cmd,commands,outstr,Options,swoHIDE) then
    begin
    outstr := opsiunquotestr2(outstr,'""');
      outstr :=  TrimRightSet(outstr, [#10,#13]);
      result := outstr;
    end;
  end;

begin
  LogDatei.log('Start with get_deb_info',LLinfo);
  try
  //outlist := TStringList.Create;
  mysetup.installDirectory:= '<none>';
  mysetup.installCommandLine :=
    'set $exitcode$ = linuxInstallOneFile(' + '"%scriptpath%/files' +
    IntToStr(mysetup.ID) + '/' + mysetup.setupFileName + '") ';
  {$IFDEF LINUX}
  // product ID
  packageId := getFieldInfoFromDeb(myfilename,'package');
  aktProduct.productdata.productId:= 'l-'+getPacketIDShort(packageId);
  // product name
  aktProduct.productdata.productName:= packageId;
  // Softwareversion
  tmpstr := getFieldInfoFromDeb(myfilename,'version');
  aktProduct.productdata.productversion:= tmpstr;
  mysetup.SoftwareVersion:= tmpstr;
  // Description
  aktProduct.productdata.description := getFieldInfoFromDeb(myfilename,'description');
  // Advice
  tmpstr := getFieldInfoFromDeb(myfilename,'Vendor');
  if tmpstr <> '' then outstr := 'Vendor: '+tmpstr  + LineEnding;
  tmpstr := getFieldInfoFromDeb(myfilename,'License');
  if tmpstr <> '' then outstr := outstr + 'License: '+tmpstr  + LineEnding;
   tmpstr := getFieldInfoFromDeb(myfilename,'Architecture');
  if tmpstr <> '' then outstr := outstr + 'Architecture: '+tmpstr  + LineEnding;
  tmpstr := getFieldInfoFromDeb(myfilename,'Homepage');
  if tmpstr <> '' then outstr := outstr + 'Homepage: '+tmpstr  + LineEnding;
  aktProduct.productdata.advice := outstr;
  // size;
  tmpstr := getFieldInfoFromDeb(myfilename,'Installed-Size');
  fsize := StrToInt64(tmpstr);
  fsizemb := fsize div (1024 * 1024);
  if fsizemb < 1 then
    fsizemb := 1;
  mysetup.requiredSpace := round(fsizemb);
  // uninstall
  mysetup.uninstallCheck.Add('if stringToBool(isOneInstalled('
    + 'CreateStringlist("'+packageId+'")))');
    mysetup.uninstallCheck.Add('	set $oldProgFound$ = "true"');
    mysetup.uninstallCheck.Add('endif');
  mysetup.uninstallCommandLine:= 'set $exitcode$ = linuxRemoveOnePackage("'
   + packageId +'")';
  {$ELSE LINUX}
  LogDatei.log('Detailed anlyze of deb files can only be done at linux',LLWarning);
  MessageDlg(rsDebAnalyze, rsDebAnalyzeNotLinux,
    mtInformation, [mbOK], '');
  {$ENDIF LINUX}
  finally
    //FreeAndNil(outlist);
  end;
  LogDatei.log('Finished with get_deb_info',LLinfo);
end;


procedure get_bitrock_info(myfilename: string; var mysetup: TSetupFile);
var
  str1, str2: string;
  pos1, pos2, i: integer;
begin
  Mywrite('Analyzing Linux Bitrock Installer:');
  mysetup.installDirectory:= '/opt/<product>/';
  if installerArray[integer(mysetup.installerId)].uninstallProg <> '' then
  begin
    // use the uninstall prog without extenstion
    mysetup.uninstallProg := ExtractFileNameOnly(installerArray[integer(mysetup.installerId)].uninstallProg);
    str1 := 'if fileexists(''"''+$installdir$+''/' +mysetup.uninstallProg + '"'')';
    mysetup.uninstallCheck.Add(str1);

    mysetup.uninstallCheck.Add('	set $oldProgFound$ = "true"');
    mysetup.uninstallCheck.Add('endif');
    str1 := 'set $exitcode$ = shellCall(''"''+$installdir$+''/'
    + mysetup.uninstallProg + '" '
    + installerArray[integer(mysetup.installerId)].unattendeduninstall+''')';
    mysetup.uninstallCommandLine := str1;
  end
  else
  begin
    // no known uninstall program
    mysetup.uninstallCheck.Add('set $oldProgFound$ = "false"');
  end;
  mywrite('get_bitrock_info finished');
end;

procedure get_linux_generic_info(myfilename: string; var mysetup: TSetupFile);
begin

end;

procedure AnalyzeLin(FileName: string; var mysetup: TSetupFile; verbose: boolean);
var
  setupType: TKnownInstaller;
  extension, tmpstr  : string;

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
        LogDatei.log('Unexpected Setuptype in Analyze: ' + installerToInstallerstr(setupType)
        + '  :  '+ IntToStr(instIdToint(setupType)), LLWarning);
         // get_linux_generic_info(Filename, mysetup);
      end;
  end;


  // marker for add installers
  // stLinRPM, stLinDeb
  tmpstr := installerToInstallerstr(setupType);
  case setupType of
    stLinRPM: Mywrite('Found installer= ' + tmpstr);
    stLinDeb: Mywrite('Found installer= ' + tmpstr);
    stBitrock: Mywrite('Found installer= ' + tmpstr);
    stUnknown: Mywrite('Found installer= ' + tmpstr);
    else
      Mywrite('Found installer= ' + tmpstr);
  end;
  { avoid hyphen char "-" and replace with dot "." in version }
  aktproduct.productdata.productversion :=
    StringReplace(aktproduct.productdata.productversion, '-', '.', [rfReplaceAll]);

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
