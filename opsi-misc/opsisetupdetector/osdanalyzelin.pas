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


procedure get_aktProduct_general_info(installerId: TKnownInstaller;
  myfilename: string; var mysetup: TSetupFile);

procedure get_rpm_info(myfilename: string; var mysetup: TSetupFile);
procedure get_deb_info(myfilename: string; var mysetup: TSetupFile);

procedure AnalyzeLin(FileName: string; var mysetup: TSetupFile; verbose: boolean);

function getPacketIDfromFilename(str: string): string;
function getPacketIDShort(str: string): string;


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


procedure get_aktProduct_general_info(installerId: TKnownInstaller;
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
    'set $installSuccess$ = install_macos_generic(' + '"%scriptpath%\files' +
    IntToStr(mysetup.ID) + '\' + mysetup.setupFileName + '") ';
  str1 := '';
  // productId and name
  if str1 <> '' then
  begin
    aktProduct.productdata.productId := 'm-' + getPacketIDShort(str1);
    aktProduct.productdata.productName := str1;
  end
  else
  begin
    product := ExtractFileNameWithoutExt(mysetup.setupFileName);
    aktProduct.productdata.productId := 'm-' + getPacketIDShort(product);
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


  // uninstallcheck
    // nothing yet
end; //get_aktProduct_general_info



procedure get_rpm_info(myfilename: string; var mysetup: TSetupFile);
var
  tmpstr, outstr : string;
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
      outstr := StringReplace(outstr,'#10','',[rfReplaceAll]);
      outstr :=  TrimRightSet(outstr, [#10,#13]);
    result := outstr;
    end;
  end;

begin
  mysetup.installCommandLine :=
    'set $installSuccess$ = linuxInstallOneFile(' + '"%scriptpath%/files' +
    IntToStr(mysetup.ID) + '/' + mysetup.setupFileName + '") ';
  try
    {$IFDEF LINUX}
  // product ID
  tmpstr := getFieldInfoFromRpm(myfilename,'name');
  aktProduct.productdata.productId:= getPacketIDShort(tmpstr);
  // product name
  aktProduct.productdata.productId:= getFieldInfoFromRpm(myfilename,'name');
  // Softwareversion
  tmpstr := getFieldInfoFromRpm(myfilename,'version');
  aktProduct.productdata.productversion:= tmpstr;
  mysetup.SoftwareVersion:= tmpstr;
  // Description
  aktProduct.productdata.description := getFieldInfoFromRpm(myfilename,'description');
  // Advice
  tmpstr := getFieldInfoFromRpm(myfilename,'Vendor');
  if tmpstr <> '' then outstr := 'Vendor: '+tmpstr  + LineEnding;
  tmpstr := getFieldInfoFromRpm(myfilename,'License') + LineEnding;
  if tmpstr <> '' then outstr := outstr + 'License: '+tmpstr  + LineEnding;
   tmpstr := getFieldInfoFromRpm(myfilename,'Architecture') + LineEnding;
  if tmpstr <> '' then outstr := outstr + 'Architecture: '+tmpstr  + LineEnding;
  tmpstr := getFieldInfoFromRpm(myfilename,'Url') + LineEnding;
  if tmpstr <> '' then outstr := outstr + 'Homepage: '+tmpstr  + LineEnding;
  aktProduct.productdata.advice := outstr;
  // size;
  tmpstr := getFieldInfoFromRpm(myfilename,'Size');
  fsize := StrToInt64(tmpstr);
  fsizemb := fsize div (1024 * 1024);
  if fsizemb < 1 then
    fsizemb := 1;
  mysetup.requiredSpace := round(fsizemb);
  {$ENDIF LINUX}
  finally
    //FreeAndNil(outlist);
  end;
end;

procedure get_deb_info(myfilename: string; var mysetup: TSetupFile);
var
  tmpstr, outstr : string;
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
    result := outstr;
  end;

begin
  try
  //outlist := TStringList.Create;
  mysetup.installCommandLine :=
    'set $installSuccess$ = linuxInstallOneFile(' + '"%scriptpath%/files' +
    IntToStr(mysetup.ID) + '/' + mysetup.setupFileName + '") ';
  {$IFDEF LINUX}
  // product ID
  tmpstr := getFieldInfoFromDeb(myfilename,'package');
  aktProduct.productdata.productId:= getPacketIDShort(tmpstr);
  // product name
  aktProduct.productdata.productId:= getFieldInfoFromDeb(myfilename,'package');
  // Softwareversion
  tmpstr := getFieldInfoFromDeb(myfilename,'version');
  aktProduct.productdata.productversion:= tmpstr;
  mysetup.SoftwareVersion:= tmpstr;
  // Description
  aktProduct.productdata.description := getFieldInfoFromDeb(myfilename,'description');
  // Advice
  tmpstr := getFieldInfoFromDeb(myfilename,'Vendor');
  if tmpstr <> '' then outstr := 'Vendor: '+tmpstr  + LineEnding;
  tmpstr := getFieldInfoFromDeb(myfilename,'License') + LineEnding;
  if tmpstr <> '' then outstr := outstr + 'License: '+tmpstr  + LineEnding;
   tmpstr := getFieldInfoFromDeb(myfilename,'Architecture') + LineEnding;
  if tmpstr <> '' then outstr := outstr + 'Architecture: '+tmpstr  + LineEnding;
  tmpstr := getFieldInfoFromDeb(myfilename,'Homepage') + LineEnding;
  if tmpstr <> '' then outstr := outstr + 'Homepage: '+tmpstr  + LineEnding;
  aktProduct.productdata.advice := outstr;
  // size;
  tmpstr := getFieldInfoFromDeb(myfilename,'Installed-Size');
  fsize := StrToInt64(tmpstr);
  fsizemb := fsize div (1024 * 1024);
  if fsizemb < 1 then
    fsizemb := 1;
  mysetup.requiredSpace := round(fsizemb);
  {$ENDIF LINUX}
  finally
    //FreeAndNil(outlist);
  end;
end;


procedure AnalyzeLin(FileName: string; var mysetup: TSetupFile; verbose: boolean);
var
  setupType: TKnownInstaller;
  extension : string;

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

  get_aktProduct_general_info(setupType, Filename, mysetup);

  // marker for add installers
  case setupType of
    stLinRPM: get_rpm_info(Filename, mysetup);
    stLinDeb: get_deb_info(Filename, mysetup);
    stUnknown: LogDatei.log(
        'Unknown Installer after Analyze.', LLcritical);
    else
      LogDatei.log('Unknown Setuptype in Analyze: ' + IntToStr(
        instIdToint(setupType)), LLcritical);
  end;


  // marker for add installers
  // stLinRPM, stLinDeb
  case setupType of
    stLinRPM: Mywrite('Found well known installer: ' +
        installerToInstallerstr(setupType));
    stLinDeb: Mywrite('Found well known installer: ' +
        installerToInstallerstr(setupType));
    stUnknown: Mywrite('Sorry - unknown installer: ' +
        installerToInstallerstr(setupType));
    else
      Mywrite('Sorry - unknown installer: ' + installerToInstallerstr(setupType));
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
