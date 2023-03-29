unit osdanalyzemac;

{$mode objfpc}

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
  oszip,
  osregex,
  oscheckbinarybitness,
  osdanalyzegeneral;

procedure get_aktProduct_general_info_mac(installerId: TKnownInstaller;
  myfilename: string; var mysetup: TSetupFile);

procedure get_zip_info(myfilename: string; var mysetup: TSetupFile);
procedure get_dmg_info(myfilename: string; var mysetup: TSetupFile);
procedure get_pkg_info(myfilename: string; var mysetup: TSetupFile);
procedure get_app_info(myfilename: string; var mysetup: TSetupFile);

procedure AnalyzeMac(FileName: string; var mysetup: TSetupFile; verbose: boolean);

function getPacketIDfromFilename(str: string): string;
function getPacketIDShort(str: string): string;


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
  // try to strip version from filename
  str := copy(str, 0, pos('-', str) - 1);
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

//https://forum.lazarus.freepascal.org/index.php?topic=8541.0
function GetDirSize(dir: string; subdir: boolean): longint;
var
  rec: TSearchRec;
  found: integer;
begin
  Result := 0;
  dir := IncludeTrailingPathDelimiter(dir);
  found := FindFirst(dir + '*.*', faAnyFile, rec);
  while found = 0 do
  begin
    Inc(Result, rec.Size);
    if (rec.Attr and faDirectory > 0) and (rec.Name[1] <> '.') and
      (subdir = True) then
      Inc(Result, GetDirSize(dir + rec.Name, True));
    found := FindNext(rec);
  end;
  SysUtils.FindClose(rec);
  //THIS IS THE LINE that needs to be/was changed to adapt to Lazarus/FreePascal
end;


procedure get_aktProduct_general_info_mac(installerId: TKnownInstaller;
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
  mysetup.installerSourceDir := '%scriptpath%/files' + IntToStr(mysetup.ID);
  mysetup.installCommandLine :=
    'set $installSuccess$ = install_macos_generic($installerSourceDir$ + ' +
    '"/' + mysetup.setupFileName + '") ';
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
  mysetup.installDirectory := '/Applications/' +
    aktProduct.productdata.productName + '.app';
  // version
  str1 := ExtractVersionFromFilename(product);
  mysetup.SoftwareVersion := str1;
  aktProduct.productdata.productversion := trim(mysetup.SoftwareVersion);
  // get size
  if DirectoryExists(myfilename) then  // app dir
    fsize := GetDirSize(myfilename, True)
  else
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
  mysetup.uninstallCheck.Add('if DirectoryExists($installdir$)');
  mysetup.uninstallCheck.Add('	set $oldProgFound$ = "true"');
  mysetup.uninstallCheck.Add('endif');
end; //get_aktProduct_general_info_mac

procedure get_zip_info(myfilename: string; var mysetup: TSetupFile);
var
  namelist: TStringList;
  exprlist: TStringList;
  i: integer;
begin
  exprlist := TStringList.Create;
  exprlist.Add('(?i)\.dmg$');
  exprlist.Add('(?i)\.pkg$');
  exprlist.Add('(?i)\.app' + PathDelim + '$');
  namelist := getFileListFromZip(myfilename);
  for i := 0 to namelist.Count - 1 do
    LogDatei.log('in zip: ' + namelist.Strings[i], LLinfo);
  ;
  namelist := getRegexMatchList(exprlist, namelist);
  if namelist.Count = 0 then
  begin
    // no well known mac install file found;
    LogDatei.log('no well known mac install file found inside zip', LLerror);
    mywrite('no well known mac install file found inside zip');
    mysetup.installerId := stUnknown;
  end
  else
  begin
    LogDatei.log('found well known mac install ".app" directory', LLnotice);
    mysetup.installCommandLine :=
      'set $installSuccess$ = install_macos_zip($installerSourceDir$ + ' +
      '"/' + mysetup.setupFileName + '") ';
  end;
end;

procedure get_dmg_info(myfilename: string; var mysetup: TSetupFile);
begin
  mysetup.installCommandLine :=
    'set $installSuccess$ = install_macos_dmg($installerSourceDir$ + ' +
    '"/' + mysetup.setupFileName + '") ';
end;

procedure get_pkg_info(myfilename: string; var mysetup: TSetupFile);
begin
  mysetup.installCommandLine :=
    'set $installSuccess$ = install_macos_pkg($installerSourceDir$ + ' +
    '"/' + mysetup.setupFileName + '") ';
end;

procedure get_app_info(myfilename: string; var mysetup: TSetupFile);
begin
  mysetup.installCommandLine :=
    'set $installSuccess$ = install_macos_app($installerSourceDir$ + ' +
    '"/' + mysetup.setupFileName + '") ';
  mysetup.installDirectory := '/Applications/' + ExtractFileName(myfilename);
end;


procedure AnalyzeMac(FileName: string; var mysetup: TSetupFile; verbose: boolean);
var
  setupType: TKnownInstaller;
  extension: string;

begin
  LogDatei.log('Start Analyze for Mac ... ', LLInfo);
  {$IFDEF OSDGUI}
  resultForm1.ProgressBarAnalyze.Position := 0;
  procmess;
  {$ENDIF OSDGUI}
  setupType := stUnknown;
  extension := lowercase(ExtractFileExt(FileName));
  case extension of
    '.zip': setupType := stMacZip;
    '.dmg': setupType := stMacDmg;
    '.pkg': setupType := stMacPKG;
    '.app': setupType := stMacApp;
    else
      setupType := stUnknown;
  end;
  mysetup.installerId := setupType;

  get_aktProduct_general_info_mac(setupType, Filename, mysetup);

  // marker for add installers
  case setupType of
    stMacZip: get_zip_info(Filename, mysetup);
    stMacDmg: get_dmg_info(Filename, mysetup);
    stMacPkg: get_pkg_info(Filename, mysetup);
    stMacApp: get_app_info(Filename, mysetup);
    stUnknown: LogDatei.log(
        'Unknown Installer after Analyze.', LLcritical);
    else
      LogDatei.log('Unknown Setuptype in Analyze: ' + IntToStr(
        instIdToint(setupType)), LLcritical);
  end;


  // marker for add installers
  // stMacZip, stMacDmg, stMacPKG, stMacApp
  case setupType of
    stMacZip: Mywrite('Found installer= ' + installerToInstallerstr(setupType));
    stMacDmg: Mywrite('Found installer= ' + installerToInstallerstr(setupType));
    stMacPKG: Mywrite('Found installer= ' + installerToInstallerstr(setupType));
    stMacApp: Mywrite('Found installer= ' + installerToInstallerstr(setupType));
    stUnknown: Mywrite('Found installer= ' + installerToInstallerstr(setupType));
    else
      Mywrite('Found installer= ' + installerToInstallerstr(setupType));
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
