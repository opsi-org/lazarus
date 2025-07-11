unit osdcreate;

{$mode delphi}

interface

uses
  {$IFDEF WINDOWS}
  Windows,
  ShlObj,
  {$ENDIF WINDOWS}
  Classes,
  SysUtils,
  strutils,
  Forms,
  Controls,
  FileUtil,
  Process,
  osdhelper,
  oslog,
  osdbasedata,
  Dialogs,
  lazfileutils,
  osparserhelper,
  osjson,
  dateutils,
  osfilehelper,
  oswebservice,
  osdmeta;

function createProductStructure: boolean;
function callOpsiPackageBuilder: boolean;
function callServiceOrPackageBuilder: boolean;
function createOpsiFiles(changelogMessage: string): boolean;

function opsiquotestr(s1, s2: string): string;
// returns s1 quoted with s2 (if it is quoted right now, nothing will be changed)
// s2 may be two chars long. Then the first char is the start mark
// and the second char is the end mark
// used by opsiquotelist

procedure opsiquotelist(var list1: TStringList; s2: string);
// returns list1 with every element quoted with s2 (if it is quoted right now, nothing will be changed)
// s2 may be two chars long. Then the first char is the start mark
// and the second char is the end mark


resourcestring
  // new for 4.1.0.2 ******************************************************************
  rsDirectory = 'Directory ';
  rsStillExitsWarningDeleteOverwrite = ' still exits. Should we create backup files ?';
  rsCouldNotCreateDirectoryWarning = 'Could not create directory: ';
  rsConfirmBackupOrRemovalTitle =
    'Should we use the existing directory and create a backup of the old files before creating the new ones ?  Otherwise the exiting directory will be deleted before creating a new one.';
  rsConfirmBackupCaption = 'Yes (Backup)';
  rsConfirmAbortCaption = 'Abort';
  rsConfirmDeleteCaption = 'No';
  rsConfirmAbortHint = 'Leave existing directory unchanged and abort';
  rsConfirmDeleteHint = 'Delete the complete directory before creating the new one.';
  rsConfirmBackupHint =
    'All files that are directly in the directories "CLIENT_DATA" and "OPSI" will be copied to a backup file.';
  rsConfirmExpandButton = 'More Information ...';
  rsConfirmExpandedText =
    'Yes: All files that are located in the directories "CLIENT_DATA" and "OPSI" will be saved to a backup file before the new files are created\nNo: Delete the complete directory before creating the new one.\nAbort: Leave existing directory unchanged and abort';


implementation

uses
  {$IFDEF OSDGUI}
  osdform,
  {$ENDIF OSDGUI}
  osdmain;

var
  patchlist: TStringList;
  myExeDir: string;
  prodpath, clientpath, opsipath: string;

function opsiquotestr(s1, s2: string): string;
  // returns s1 quoted with s2 (if it is quoted right now, nothing will be changed)
  // s2 may be two chars long. Then the first char is the start mark
  // and the second char is the end mark
  // used by opsiquotelist
var
  markstr, startmark, endmark: string;
begin
  Result := '';
  markstr := trim(s2);
  s1 := trim(s1);
  if (length(s1) >= 1) and (length(markstr) >= 1) then
  begin
    startmark := markstr[1];
    if length(markstr) >= 2 then
      endmark := markstr[2] // different marks (brackets) at begin and end
    else
      endmark := startmark; // the same mark (quote) at begin and end
    if not ((pos(startmark, s1) = 1) and AnsiEndsStr(endmark, s1)) then
      Result := startmark + s1 + endmark
    else
      Result := s1;
  end;
end;

procedure opsiquotelist(var list1: TStringList; s2: string);
// returns list1 with every element quoted with s2 (if it is quoted right now, nothing will be changed)
// s2 may be two chars long. Then the first char is the start mark
// and the second char is the end mark
var
  i: integer;
begin
  for i := 0 to list1.Count - 1 do
  begin
    list1.Strings[i] := opsiquotestr(list1.Strings[i], s2);
  end;
end;

procedure initoutputpath;
begin
  prodpath := IncludeTrailingPathDelimiter(myconfiguration.workbench_Path) +
    aktProduct.productdata.productId;
  clientpath := prodpath + PathDelim + 'CLIENT_DATA';
  opsipath := prodpath + PathDelim + 'OPSI';
end;


procedure patchScript(infile, outfile: string);
var
  infileHandle, outfileHandle: Text;
  aktline: string;
  i, k: integer;
  aktindentstr: string;
  aktindentnum: integer;
  aktReplacestr: string;
begin
  if FileExists(infile) then
  begin
    write_log_and_memo('creating: ' + outfile + ' from: ' + infile);
    if not DirectoryExists(ExtractFileDir(outfile)) then
      ForceDirectories(ExtractFileDir(outfile));

    {$I+}//use exceptions
    try
      AssignFile(infileHandle, infile);
      AssignFile(outfileHandle, outfile);
      reset(infileHandle);
      rewrite(outfileHandle);

      while not EOF(infileHandle) do
      begin
        ReadLn(infileHandle, aktline);
        for i := 0 to patchlist.Count - 1 do
        begin
          if pos(lowercase(patchlist.Names[i]), lowercase(aktline)) > 0 then
          begin
            // we have to preserve the indent
            // check actual indent
            aktindentnum := length(aktline) - length(trimleft(aktline));
            aktindentstr := '';
            // indent with tabs
            for k := 1 to aktindentnum do
              aktindentstr := aktindentstr + char(9);
            // p
            aktReplacestr := patchlist.ValueFromIndex[i];
            // patch aktidentstr after each Newline
            aktReplacestr := StringReplace(aktReplacestr, Lineending,
              Lineending + aktindentstr, [rfReplaceAll, rfIgnoreCase]);
            aktline := StringReplace(aktline, patchlist.Names[i],
              aktReplacestr, [rfReplaceAll, rfIgnoreCase]);
          end;
        end;
        writeln(outfileHandle, aktline);
      end;
      CloseFile(infileHandle);
      CloseFile(outfileHandle);
    except
      on E: EInOutError do
        logdatei.log('patchScript file error: ' + E.ClassName + '/' +
          E.Message, LLError);
    end;
    {$I-}//end use exceptions
  end
  else
    logdatei.log('patchScript file warning: infile for: ' +
      ExtractFileName(outfile) + ' not found', LLwarning);
end;

procedure patchThisList(var targetlist: TStringList);
var
  aktline: string;
  i, k, linecounter: integer;
  aktindentstr: string;
  aktindentnum: integer;
  aktReplacestr: string;
begin
  try

    for linecounter := 0 to targetlist.Count - 1 do
    begin
      aktline := targetlist.Strings[linecounter];
      for i := 0 to patchlist.Count - 1 do
      begin
        if pos(lowercase(patchlist.Names[i]), lowercase(aktline)) > 0 then
        begin
          // we have to preserve the indent
          // check actual indent
          aktindentnum := length(aktline) - length(trimleft(aktline));
          aktindentstr := '';
          // indent with tabs
          for k := 1 to aktindentnum do
            aktindentstr := aktindentstr + char(9);
          // p
          aktReplacestr := patchlist.ValueFromIndex[i];
          // patch aktidentstr after each Newline
          aktReplacestr := StringReplace(aktReplacestr, Lineending,
            Lineending + aktindentstr, [rfReplaceAll, rfIgnoreCase]);
          aktline := StringReplace(aktline, patchlist.Names[i],
            aktReplacestr, [rfReplaceAll, rfIgnoreCase]);
        end;
      end;
      targetlist.Strings[linecounter] := aktline;
    end;
  except
    on E: Exception do
      logdatei.log('patchList exception error: ' + E.ClassName + '/' +
        E.Message, LLError);
  end;
end;


procedure fillPatchList;
var
  i: integer;
  str, str2, str3, defaultstr: string;
  strlist: TStringList;
  preinstalllist: TStringList; // preinstall lines
  postinstalllist: TStringList; // postinstall lines
  preUnInstalllist: TStringList; // preUnInstall lines
  postUnInstalllist: TStringList; // postUnInstall lines
  sectionlist: TStringList; // section lines
  finalUnInstallList: TStringList; // Final uinstall lines (unistall.opsiscript)
  templatePath, filepath: string;
  proptmpstr: string;
  neededspacefor2archinstall: integer = 0;
  templateChannelDir, pre_id, genericTemplatePath: string;

  function getFilePath(filename: string): string;
  var
    trypath: string;
  begin
    trypath := templatePath + PathDelim + templateChannelDir +
      Pathdelim + pre_id + PathDelim + filename;
    if not FileExists(trypath) then
    begin
      logdatei.log('File: ' + trypath + ' not found at template channel: ' +
        templateChannelDir + ' - fall back to default', LLinfo);
      trypath := templatePath + PathDelim + 'default' + Pathdelim +
        pre_id + PathDelim + filename;
      if not FileExists(trypath) then
      begin
        logdatei.log('File: ' + trypath +
          ' not found at template channel: default' +
          ' - try to find in generic', LLinfo);
        trypath := genericTemplatePath + PathDelim + filename;
        if not FileExists(trypath) then
        begin
          logdatei.log('File: ' + filename + ' not found in searchpath' +
            ' - giving up', LLerror);
          trypath := '';
        end;
      end;
    end;
    if trypath <> '' then
      logdatei.log('File: ' + filename + ' found at: ' + trypath, LLinfo);

    Result := trypath;
  end;

  procedure readFileToList(myfilename: string; var list: TStringList);
  var
    myfilepath: string;
    mylist: TStringList;
  begin
    mylist := TStringList.Create;
    myfilepath := getFilePath(myfilename);
    if FileExists(myfilepath) then
    begin
      mylist.LoadFromFile(myfilepath);
      list.AddStrings(mylist);
    end
    else
      LogDatei.log('Error: File not found: ' + myfilename, LLerror);
    FreeAndNil(mylist);
  end;

begin
  templateChannelDir := aktProduct.productdata.channelDir;
  {$IFDEF WINDOWS}
  templatePath := ExtractFileDir(Application.ExeName) + PathDelim + 'template-files';
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
  // the first path is in the development environment
  templatePath := ExtractFileDir(Application.ExeName) + PathDelim + 'template-files';
  if not DirectoryExists(templatePath) then
    templatePath := '/usr/share/opsi-setup-detector/template-files';
  {$ENDIF LINUX}
  {$IFDEF DARWIN}
  // the first path is in the development environment
  templatePath := ExtractFileDir(Application.ExeName) + PathDelim + 'template-files';
  if not DirectoryExists(templatePath) then
    templatePath := ExtractFileDir(Application.ExeName) + PathDelim +
      '../Resources/template-files';
  {$ENDIF DARWIN}

  genericTemplatePath := templatePath + Pathdelim + 'default' + Pathdelim + 'generic';
  if osWin in aktProduct.productdata.targetOSset then
    pre_id := 'win'
  else if osLin in aktProduct.productdata.targetOSset then
    pre_id := 'lin'
  else if osMac in aktProduct.productdata.targetOSset then
    pre_id := 'mac';
  if (osMulti in aktProduct.productdata.targetOSset) then
    pre_id := 'multi';
  if (osdsettings.runmode in [analyzeCreateWithUser, createTemplateWithUser]) then
    pre_id := 'with-user';

  try
    strlist := TStringList.Create;
    preinstalllist := TStringList.Create;
    postinstalllist := TStringList.Create;
    sectionlist := TStringList.Create;
    preUnInstalllist := TStringList.Create;
    postUnInstalllist := TStringList.Create;
    finalUnInstallList := TStringList.Create;
    patchlist.Clear;
    // OSD version
    patchlist.add('#@osdVersion*#=' + myVersion);
    str := '';

    // variables
    str := '';
    // have to be allways present:
    str := str + 'DefVar $targetprogram$' + LineEnding;
    // form ProductProperties
    for i := 0 to aktProduct.properties.Count - 1 do
    begin
      proptmpstr := LowerCase(aktProduct.properties.Items[i].Property_Name);
      if (proptmpstr = LowerCase('SecretLicense_or_Pool')) then
      begin
        str := str + 'DefVar $LicenseOrPool$' + LineEnding;
        str := str + 'DefVar $Licensekey$' + LineEnding;
        str := str + 'DefVar $LicenseError$' + LineEnding;
        str := str + 'DefVar $LicenseHandledByScript$ = "true"' + LineEnding;
      end
      else
        str := str + 'DefVar $' + proptmpstr + '$' + LineEnding;
    end;
    // special msix
    if aktProduct.SetupFiles[0].installerId = stMsixAppx then
      str := str + 'DefVar $MsixAppxPackageName$' + LineEnding;

    // special winget
    if aktProduct.SetupFiles[0].installerId = stWinget then
    begin
      //str := str + 'DefVar $wingetId$ = "' + aktProduct.SetupFiles[0].wingetId +'"'+ LineEnding;
      //str := str + 'DefVar $wingetSource$ = ' + aktProduct.SetupFiles[0].wingetSource +'"'+ LineEnding;
      str := str + 'DefVar $wingetBin$' + LineEnding;
      str := str + 'DefVar $wingetCommandParam$' + LineEnding;

      readFileToList('HandleWingetSections.opsiscript', sectionlist);
    end;

    patchlist.add('#@stringVars*#=' + str);

    str := '';
    patchlist.add('#@productId*#=' + aktProduct.productdata.productId);
    patchlist.add('#@productName*#=' + aktProduct.productdata.productName);
    patchlist.add('#@productVersion*#=' + aktProduct.productdata.productversion);
    for i := 0 to myconfiguration.import_libraries.Count - 1 do
      str := str + 'importlib "' + myconfiguration.import_libraries[i] +
        '"' + LineEnding;
    patchlist.add('#@importLibs*#=' + str);
    patchlist.add('#@LicenseRequired*#=' +
      boolToStr(aktProduct.productdata.licenserequired, True));
    str := '';

    // msi special
    if length(aktProduct.SetupFiles) > 0 then
      //if aktProduct.SetupFiles[0].installerId = stMsi then
      if aktProduct.SetupFiles[0].msiUninstallCode then
      begin
        readFileToList('HandleMsiUninstallSections.opsiscript', sectionlist);
      end;

    str := '';
    // msix special
    if aktProduct.SetupFiles[0].installerId = stMsixAppx then
    begin
      if aktProduct.SetupFiles[0].optionalUninstallLines.Count > 0 then
      begin
        str := aktProduct.SetupFiles[0].optionalUninstallLines.Text;
      end;
    end;
    patchlist.add('#@optionalUninstallLines*#=' + str);

    // #@GetProductProperty*#:
    str := '';
    for i := 0 to aktProduct.properties.Count - 1 do
    begin
      proptmpstr := LowerCase(aktProduct.properties.Items[i].Property_Name);
      if (proptmpstr = LowerCase('SecretLicense_or_Pool')) then
      begin
        str2 := '';
        if aktProduct.properties.Items[i].GetDefaultLines.Count > 0 then
          str2 := aktProduct.properties.Items[i].GetDefaultLines[0];
        str := str +
          'set $LicenseOrPool$ = GetConfidentialProductProperty("SecretLicense_or_Pool","'
          +
          str2 + '")' + LineEnding;

        readFileToList('HandleLicense.opsiscript', preinstalllist);
        readFileToList('HandleLicenseUninst.opsiscript', finalUnInstallList);
      end

      else if (proptmpstr = LowerCase('Install_from_local_tmpdir')) then
      begin
        str := str +
          'set $Install_from_local_tmpdir$ = GetProductProperty("Install_from_local_tmpdir","'
          +
          BoolToStr(aktProduct.properties.Items[i].BoolDefault, True) +
          '")' + LineEnding;
        readFileToList('HandleInstallFromLocal.opsiscript', preinstalllist);
        readFileToList('HandlePostInstallFromLocal.opsiscript', postinstalllist);
        readFileToList('HandleInstallFromLocalSections.opsiscript', sectionlist);
      end
      else
      begin
        if aktProduct.properties.Items[i].Property_Type = bool then
          str2 := BoolToStr(aktProduct.properties.Items[i].BoolDefault, True)
        else
        begin
          if aktProduct.properties.Items[i].GetDefaultLines.Count > 0 then
            str2 := aktProduct.properties.Items[i].GetDefaultLines[0]
          else
            str2 := '';
        end;
        { remove brackets [] }
        str2 := opsiunquotestr2(str2, '[]');
        { take first from list }
        GetWordOrStringConstant(str2, str2, str3, WordDelimiterSet6);
        str := str + 'set $' + proptmpstr + '$ = GetProductProperty("' +
          proptmpstr + '", "' + str2 + '")' + LineEnding;
      end;
    end;
    patchlist.add('#@GetProductProperty*#=' + str);

    // from configuration
    str := myconfiguration.preInstallLines.Text;
    if str <> '' then
      preinstalllist.Add(myconfiguration.preInstallLines.Text + LineEnding);

    str := myconfiguration.postInstallLines.Text;
    if str <> '' then
      postinstalllist.Add(myconfiguration.postInstallLines.Text + LineEnding);

    str := myconfiguration.preUninstallLines.Text;
    if str <> '' then
      str := 'comment "Start Pre UnInstall hook :"' + LineEnding +
        myconfiguration.preUninstallLines.Text;
    patchlist.add('#@preUninstallLines*#=' + str);


    str := myconfiguration.postUnInstallLines.Text;
    if str <> '' then
      postUnInstallList.Add(myconfiguration.postUnInstallLines.Text + LineEnding);

    str := '';
    if aktProduct.properties.propExists('DesktopIcon') then
    begin

      readFileToList('HandleDesktopIcon.opsiscript', postinstalllist);

      readFileToList('HandleDesktopIconSections.opsiscript', sectionlist);
      str := 'comment "Start Remove Desktop Icon Handling :"' +
        LineEnding + 'Linkfolder_remove_desktop_icon';
    end;
    patchlist.add('#@DelsubHandleDesktopIcon*#=' + str);

    str := '';
    patchlist.add('#@Profileconfigurationlines*#=' + str);

    str := '';
    patchlist.add('#@LicenseInstallLines*#=' + str);

    str := '';
    patchlist.add('#@LicenseUninstallLines*#=' + str);

    str := '';
    if aktProduct.productdata.useCustomDir then
    begin

      readFileToList('HandleCustomDir.opsiscript', postinstalllist);

      readFileToList('HandleCustomDirSections.opsiscript', sectionlist);
    end;

    if aktProduct.productdata.customizeProfile then
    begin

      readFileToList('HandleCustomizeProfile.opsiscript', postinstalllist);

      readFileToList('HandleCustomizeProfilesSections.opsiscript', sectionlist);
    end;

    // Checkbox uninstallBeforeInstall
    str := '';
    if aktProduct.productdata.uninstallBeforeInstall then
    begin
      str := ' and ($uninstall_before_install$ = "true")';
    end;
    patchlist.add('#@preDelIncCondition*#=' + str);

    // loop over setups
    for i := 0 to 2 do
    begin
      if aktProduct.SetupFiles[i].active then
        //setup 1
        patchlist.add('#@install' + IntToStr(i + 1) + '*#=' +
          BoolToStr(aktProduct.SetupFiles[i].active, True));

      patchlist.add('#@MinimumSpace' + IntToStr(i + 1) + '*#=' +
        IntToStr(aktProduct.SetupFiles[i].requiredSpace) + ' MB');

      neededspacefor2archinstall :=
        neededspacefor2archinstall + aktProduct.SetupFiles[i].requiredSpace;

      patchlist.add('#@InstallDir' + IntToStr(i + 1) + '*#=' +
        aktProduct.SetupFiles[i].installDirectory);

      patchlist.add('#@MsiId' + IntToStr(i + 1) + '*#=' +
        aktProduct.SetupFiles[i].msiId);

      patchlist.add('#@MsiProductName' + IntToStr(i + 1) + '*#=' +
        aktProduct.SetupFiles[i].msiProductName);

      patchlist.add('#@MsiUpgradecode' + IntToStr(i + 1) + '*#=' +
        aktProduct.SetupFiles[i].msiUpgradeCode);

      patchlist.add('#@installCommandLine' + IntToStr(i + 1) + '*#=' +
        aktProduct.SetupFiles[i].installCommandLine);

      patchlist.add('#@installCommandStringEx' + IntToStr(i + 1) +
        '*#=' + aktProduct.SetupFiles[i].installCommandStringEx);

      str := aktProduct.SetupFiles[i].install_waitforprocess;
      if str <> '' then
        str := '/WaitForProcessEnding "' +
          aktProduct.SetupFiles[i].install_waitforprocess + '" /TimeOutSeconds 20';
      patchlist.add('#@installWaitForProc' + IntToStr(i + 1) + '*#=' + str);

      patchlist.add('#@isExitcodeFatalFunction' + IntToStr(i + 1) +
        '*#=' + aktProduct.SetupFiles[i].isExitcodeFatalFunction);

      str := aktProduct.SetupFiles[i].uninstallCheck.Text;
      patchlist.add('#@uninstallCheckLines' + IntToStr(i + 1) + '*#=' + str);

      patchlist.add('#@uninstallCommandLine' + IntToStr(i + 1) +
        '*#=' + aktProduct.SetupFiles[i].uninstallCommandLine);

      patchlist.add('#@uninstallCommandStringEx' + IntToStr(i + 1) +
        '*#=' + aktProduct.SetupFiles[i].uninstallCommandStringEx);

      patchlist.add('#@uninstallProg' + IntToStr(i + 1) + '*#=' +
        aktProduct.SetupFiles[0].uninstallProg);

      str := aktProduct.SetupFiles[i].uninstall_waitforprocess;
      if str <> '' then
        str := '/WaitForProcessEnding "' +
          aktProduct.SetupFiles[i].uninstall_waitforprocess + '" /TimeOutSeconds 20';
      patchlist.add('#@uninstallWaitForProc' + IntToStr(i + 1) + '*#=' + str);

      str := '';
      patchlist.add('#@SetupSectionLines' + IntToStr(i + 1) + '*#=' + str);

      str := '';
      patchlist.add('#@DelsubSectionLines' + IntToStr(i + 1) + '*#=' + str);

      str := aktProduct.SetupFiles[i].installErrorHandlingLines.Text;
      patchlist.add('#@installErrorHandlingLines' + IntToStr(i + 1) + '*#=' + str);

      str := aktProduct.SetupFiles[i].installerSourceDir;
      patchlist.add('#@installerSourceDir' + IntToStr(i + 1) + '*#=' + str);

      str := aktProduct.SetupFiles[i].targetProg;
      patchlist.add('#@targetprogram' + IntToStr(i + 1) + '*#=' + str);

    end;

    patchlist.add('#@MinimumSpace3*#=' +
      IntToStr(neededspacefor2archinstall) + ' MB');

    // Handle lists
    patchThisList(preinstalllist);
    str := preinstalllist.Text;
    patchlist.add('#@preInstallLines*#=' + str);

    patchThisList(postinstalllist);
    str := postinstalllist.Text;
    patchlist.add('#@postInstallLines*#=' + str);

    patchThisList(sectionlist);
    str := sectionlist.Text;
    patchlist.add('#@sectionLines*#=' + str);

    patchThisList(preUnInstalllist);
    str := preUnInstalllist.Text;
    patchlist.add('#@preUnInstallLines*#=' + str);

    patchThisList(postUnInstalllist);
    str := postUnInstalllist.Text;
    patchlist.add('#@postUnInstallLines*#=' + str);

    patchThisList(finalUnInstallList);
    str := finalUnInstallList.Text;
    patchlist.add('#@finalUnInstallLines*#=' + str);

    logdatei.log('Here comes the patchlist: ', LLinfo);
    logdatei.log_list(patchlist, LLdebug);

  finally
    FreeAndNil(strlist);
    FreeAndNil(preinstalllist);
    FreeAndNil(postinstalllist);
    FreeAndNil(sectionlist);
    FreeAndNil(preUnInstalllist);
    FreeAndNil(postUnInstalllist);
    FreeAndNil(finalUnInstallList);

  end;
end;

function createClientFiles: boolean;
var
  infilename, outfilename, tmpname, tmpext, tmppref: string;
  insetup, indelsub, inuninstall: string;
  templatePath, genericTemplatePath, tempstr: string;
  templateChannelDir, pre_id, subdir: string;
  infilelist: TStringList;
  channelLibraryFilelist: TStringList;
  i: integer;
  infilenotfound: boolean = False;

  function getFilePath(filename: string): string;
  var
    trypath: string;
  begin
    trypath := templatePath + PathDelim + templateChannelDir +
      Pathdelim + pre_id + PathDelim + filename;
    if not FileExists(trypath) then
    begin
      logdatei.log('File: ' + trypath + ' not found at template channel: ' +
        templateChannelDir + ' - fall back to default', LLinfo);
      trypath := templatePath + PathDelim + 'default' + Pathdelim +
        pre_id + PathDelim + filename;
      if not FileExists(trypath) then
      begin
        logdatei.log('File: ' + trypath +
          ' not found at template channel: default' +
          ' - try to find in generic', LLinfo);
        trypath := genericTemplatePath + PathDelim + filename;
        if not FileExists(trypath) then
        begin
          logdatei.log('File: ' + filename + ' not found in searchpath' +
            ' - giving up', LLerror);
          trypath := '';
        end;
      end;
    end;
    if trypath <> '' then
      logdatei.log('File: ' + filename + ' found at: ' + trypath, LLinfo);

    Result := trypath;
  end;

begin
  Result := False;
  templateChannelDir := aktProduct.productdata.channelDir;
  {$IFDEF WINDOWS}
  templatePath := ExtractFileDir(Application.ExeName) + PathDelim + 'template-files';
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
  // the first path is in the development environment
  templatePath := ExtractFileDir(Application.ExeName) + PathDelim + 'template-files';
  if not DirectoryExists(templatePath) then
    templatePath := '/usr/share/opsi-setup-detector/template-files';
  {$ENDIF LINUX}
  {$IFDEF DARWIN}
  // the first path is in the development environment
  templatePath := ExtractFileDir(Application.ExeName) + PathDelim + 'template-files';
  if not DirectoryExists(templatePath) then
    templatePath := ExtractFileDir(Application.ExeName) + PathDelim +
      '../Resources/template-files';
  {$ENDIF DARWIN}

  genericTemplatePath := templatePath + Pathdelim + 'default' + Pathdelim + 'generic';
  if osWin in aktProduct.productdata.targetOSset then
    pre_id := 'win'
  else if osLin in aktProduct.productdata.targetOSset then
    pre_id := 'lin'
  else if osMac in aktProduct.productdata.targetOSset then
    pre_id := 'mac';
  if (osMulti in aktProduct.productdata.targetOSset) then
    pre_id := 'multi';
  if (osdsettings.runmode in [analyzeCreateWithUser, createTemplateWithUser]) then
    pre_id := 'with-user';

  try
    try
      patchlist := TStringList.Create;
      fillPatchList;
      infilelist := TStringList.Create;
      case osdsettings.runmode of
        createWingetProd:
        begin
          infilelist.Add('setupwinget.opsiscript');
          infilelist.Add('delincwinget.opsiinc');
          infilelist.Add('uninstallwinget.opsiscript');
          infilelist.Add('declarations.opsiinc');
          infilelist.Add('sections.opsiinc');
          //osd-winget-exitcode.opsiscript
          infilename := genericTemplatePath + Pathdelim +
            'osd-winget-exitcode.opsiscript';
          outfilename := clientpath + PathDelim + 'osd-winget-exitcode.opsiscript';
          copyfile(infilename, outfilename, [cffOverwriteFile,
            cffCreateDestDirectory, cffPreserveTime], True);
        end;
        singleAnalyzeCreate:
        begin
          infilelist.Add('setupsingle.opsiscript');
          infilelist.Add('delincsingle.opsiinc');
          infilelist.Add('uninstallsingle.opsiscript');
          //if aktProduct.SetupFiles[0].installerId = stMsi then
          if aktProduct.SetupFiles[0].msiUninstallCode then
            infilelist.Add('delincmsisingle.opsiinc');
        end;
        twoAnalyzeCreate_1, twoAnalyzeCreate_2:
        begin
          infilelist.Add('setupdouble.opsiscript');
          infilelist.Add('delincdouble.opsiinc');
          infilelist.Add('uninstalldouble.opsiscript');
          //if aktProduct.SetupFiles[0].installerId = stMsi then
          if aktProduct.SetupFiles[0].msiUninstallCode then
            infilelist.Add('delincmsidouble.opsiinc');
        end;
        createTemplate:
        begin
          infilelist.Add('setuptempl.opsiscript');
          infilelist.Add('delinctempl.opsiinc');
          infilelist.Add('uninstalltempl.opsiscript');
        end;
        createMeta:
        begin
          infilelist.Add('setupmeta.opsiscript');
        end;

        threeAnalyzeCreate_1, threeAnalyzeCreate_2, threeAnalyzeCreate_3:
        begin
          infilelist.Add('setupsingle.opsiscript');
          infilelist.Add('uninstallsingle.opsiscript');
          infilelist.Add('win_setupsingle.opsiscript');
          infilelist.Add('win_delincsingle.opsiinc');
          infilelist.Add('win_uninstallsingle.opsiscript');
          infilelist.Add('lin_setupsingle.opsiscript');
          infilelist.Add('lin_delincsingle.opsiinc');
          infilelist.Add('lin_uninstallsingle.opsiscript');
          infilelist.Add('mac_setupsingle.opsiscript');
          infilelist.Add('mac_delincsingle.opsiinc');
          infilelist.Add('mac_uninstallsingle.opsiscript');
        end;
        createMultiTemplate:
        begin
          infilelist.Add('setuptempl.opsiscript');
          infilelist.Add('uninstalltempl.opsiscript');
          infilelist.Add('win_setuptempl.opsiscript');
          infilelist.Add('win_delinctempl.opsiinc');
          infilelist.Add('win_uninstalltempl.opsiscript');
          infilelist.Add('lin_setuptempl.opsiscript');
          infilelist.Add('lin_delinctempl.opsiinc');
          infilelist.Add('lin_uninstalltempl.opsiscript');
          infilelist.Add('mac_setuptempl.opsiscript');
          infilelist.Add('mac_delinctempl.opsiinc');
          infilelist.Add('mac_uninstalltempl.opsiscript');
        end;
        analyzeCreateWithUser, createTemplateWithUser:
        begin
          infilelist.Add('setup.opsiscript');
          infilelist.Add('sections.opsiinc');
          infilelist.Add('declarations.opsiinc');
          infilelist.Add('localsetup\declarations-local.opsiinc');
          //if aktProduct.SetupFiles[0].installerId = stMsi then
          if aktProduct.SetupFiles[0].msiUninstallCode then
            infilelist.Add('localsetup\delsubmsi-local.opsiinc')
          else
            infilelist.Add('localsetup\delsub-local.opsiinc');
          infilelist.Add('localsetup\sections-local.opsiinc');
          infilelist.Add('localsetup\setup-local.opsiinc');
          infilelist.Add('localsetup\setup-local.opsiscript');
          infilelist.Add('localsetup\uninstall-local.opsiscript');
          infilelist.Add('localsetup\update-local.opsiscript');
          for i := 0 to infilelist.Count - 1 do
          begin
            // use GetForcedPathDelims to make it work also at unix
            infilename := getFilePath(GetForcedPathDelims(infilelist.Strings[i]));
            outfilename := clientpath + PathDelim +
              GetForcedPathDelims(infilelist.Strings[i]);
            outfilename := StringReplace(outfilename, 'msi', '', []);
            patchScript(infilename, outfilename);
          end;
        end;
      end;
      // additional files for 'default'
      if (aktProduct.productdata.channelDir = 'default') then
      begin
        case osdsettings.runmode of
          singleAnalyzeCreate:
          begin
            infilelist.Add('declarations.opsiinc');
            infilelist.Add('sections.opsiinc');
          end;
          twoAnalyzeCreate_1, twoAnalyzeCreate_2:
          begin
            infilelist.Add('declarations.opsiinc');
            infilelist.Add('sections.opsiinc');
          end;
          createTemplate:
          begin
            infilelist.Add('declarations.opsiinc');
            infilelist.Add('sections.opsiinc');
          end;
          createMeta:
          begin
          end;
          threeAnalyzeCreate_1, threeAnalyzeCreate_2, threeAnalyzeCreate_3:
          begin
            infilelist.Add('declarations.opsiinc');
          end;
          createMultiTemplate:
          begin
            infilelist.Add('declarations.opsiinc');
          end;
          analyzeCreateWithUser, createTemplateWithUser:
          begin
          end;
        end;
      end;

      // additional files for 'structured'
      if (aktProduct.productdata.channelDir = 'structured') then
      begin
        case osdsettings.runmode of
          singleAnalyzeCreate:
          begin
            infilelist.Add('declarations.opsiinc');
            infilelist.Add('sections.opsiinc');
          end;
          twoAnalyzeCreate_1, twoAnalyzeCreate_2:
          begin
            infilelist.Add('declarations.opsiinc');
            infilelist.Add('sections.opsiinc');
          end;
          createTemplate:
          begin
            infilelist.Add('declarations.opsiinc');
            infilelist.Add('sectionstempl.opsiinc');
          end;
          createMeta:
          begin
          end;
          threeAnalyzeCreate_1, threeAnalyzeCreate_2, threeAnalyzeCreate_3:
          begin
            infilelist.Add('declarations.opsiinc');
            infilelist.Add('sections.opsiinc');
          end;
          createMultiTemplate:
          begin
            infilelist.Add('declarations.opsiinc');
            infilelist.Add('sections.opsiinc');
          end;
          analyzeCreateWithUser, createTemplateWithUser:
          begin
          end;
        end;
      end;

      // additional optional files for 'custom'
      if (aktProduct.productdata.channelDir = 'custom') then
      begin

        // add existing libraries '*.opsilib'
        // create list of existing *.opsilib files in channel
        channelLibraryFilelist :=
          FindAllFiles(templatePath + PathDelim + templateChannelDir +
          Pathdelim + pre_id, '*.opsilib', False);
        // copy library files to client path
        for i := 0 to channelLibraryFilelist.Count - 1 do
        begin
          infilename := channelLibraryFilelist.Strings[i];
          outfilename := ExtractFileName(infilename);
          outfilename := clientpath + PathDelim + outfilename;
          patchScript(infilename, outfilename);
        end;
        // free channelLibraryFilelist
        FreeAndNil(channelLibraryFilelist);

        // add all files from additional_files sub dir
        // create list of files in channel sub dir
        channelLibraryFilelist :=
          FindAllFiles(templatePath + PathDelim + templateChannelDir +
          Pathdelim + 'additional_files', '*.*', True);
        // copy recursive all files to client path
        for i := 0 to channelLibraryFilelist.Count - 1 do
        begin
          infilename := channelLibraryFilelist.Strings[i];
          outfilename := ExtractFileName(infilename);
          // substract the base dir from the complete path
          // in order to get the sub dirs in outfilename
          outfilename := CreateRelativePath(channelLibraryFilelist.Strings[i]
            , templatePath + PathDelim + templateChannelDir + Pathdelim +
            'additional_files');
          // construct the new outfilename full path
          outfilename := clientpath + PathDelim + outfilename;
          patchScript(infilename, outfilename);
        end;
        // free channelLibraryFilelist
        FreeAndNil(channelLibraryFilelist);


        case osdsettings.runmode of
          singleAnalyzeCreate:
          begin
            infilelist.Add('declarations.opsiinc');
            infilelist.Add('sections.opsiinc');
          end;
          twoAnalyzeCreate_1, twoAnalyzeCreate_2:
          begin
            infilelist.Add('declarations.opsiinc');
            infilelist.Add('sections.opsiinc');
          end;
          createTemplate:
          begin
            infilelist.Add('declarations.opsiinc');
            infilelist.Add('sectionstempl.opsiinc');
          end;
          createMeta:
          begin
          end;
          threeAnalyzeCreate_1, threeAnalyzeCreate_2, threeAnalyzeCreate_3:
          begin
            infilelist.Add('declarations.opsiinc');
            infilelist.Add('sections.opsiinc');
          end;
          createMultiTemplate:
          begin
            infilelist.Add('declarations.opsiinc');
            infilelist.Add('sections.opsiinc');
          end;
          analyzeCreateWithUser, createTemplateWithUser:
          begin
          end;
        end;
      end;

      // we did it for analyzeCreateWithUser, createTemplateWithUser right now
      if not (osdsettings.runmode in [analyzeCreateWithUser,
        createTemplateWithUser]) then
        for i := 0 to infilelist.Count - 1 do
        begin
          tmpname := ExtractFileNameOnly(infilelist.Strings[i]);
          tmpext := ExtractFileExt(infilelist.Strings[i]);
          tmppref := copy(infilelist.Strings[i], 1, 3);
          // replace non active setups with template
          if (tmppref = 'win') and (aktProduct.SetupFiles[0].active = False) then
          begin
            infilename := getFilePath(StringReplace(tmpname, 'single',
              'templ', []) + tmpext);
          end
          else if (tmppref = 'lin') and (aktProduct.SetupFiles[1].active = False) then
          begin
            infilename := getFilePath(StringReplace(tmpname, 'single',
              'templ', []) + tmpext);
          end
          else if (tmppref = 'mac') and (aktProduct.SetupFiles[2].active = False) then
          begin
            infilename := getFilePath(StringReplace(tmpname, 'single',
              'templ', []) + tmpext);
          end
          else
          begin
            infilename := getFilePath(infilelist.Strings[i]);
          end;
          tmpname := StringReplace(tmpname, 'winget', '', []);
          tmpname := StringReplace(tmpname, 'single', '', []);
          tmpname := StringReplace(tmpname, 'double', '', []);
          tmpname := StringReplace(tmpname, 'templ', '', []);
          tmpname := StringReplace(tmpname, 'meta', '', []);
          tmpname := StringReplace(tmpname, 'msi', '', []);
          if tmpname = 'setup' then
            outfilename := clientpath + PathDelim + aktProduct.productdata.setupscript
          else if tmpname = 'delsub' then
            outfilename := clientpath + PathDelim + aktProduct.productdata.delsubscript
          else if tmpname = 'uninstall' then
            outfilename := clientpath + PathDelim +
              aktProduct.productdata.uninstallscript
          else
            outfilename := clientpath + PathDelim + tmpname + tmpext;
          patchScript(infilename, outfilename);
        end;

      subdir := '';
      // at 'with user' we need osd-lib also at the 'localsetup' dir
      if osdsettings.runmode in [analyzeCreateWithUser, createTemplateWithUser] then
        subdir := pathdelim + 'localsetup';
      // No need to copy installer for templates or Meta
      if not (osdsettings.runmode in [createTemplate, createMultiTemplate,
        createTemplateWithUser, createMeta]) then
        // loop over setups
        for i := 0 to 2 do
        begin
          infilename := aktProduct.SetupFiles[i].setupFullFileName;
          LogDatei.log('Will copy: ' + infilename + ' to: ' +
            clientpath + subdir + PathDelim + 'files' + IntToStr(i + 1), LLNotice);
          if aktProduct.SetupFiles[i].active then
            // complete dir
            if aktProduct.SetupFiles[i].copyCompleteDir then
            begin
              if not CopyDirTree(ExtractFileDir(infilename),
                clientpath + subdir + PathDelim + 'files' + IntToStr(i + 1),
                [cffOverwriteFile, cffCreateDestDirectory, cffPreserveTime]) then
                LogDatei.log('Failed to copy: ' + infilename, LLError);
            end
            // macos .app dir
            else if DirectoryExistsUTF8(infilename) and
              (LowerCase(ExtractFileExt(infilename)) = '.app') then
            begin
              if not CopyDirTree(infilename, clientpath + subdir +
                PathDelim + 'files' + IntToStr(i + 1) + PathDelim +
                ExtractFileName(infilename), [cffOverwriteFile,
                cffCreateDestDirectory, cffPreserveTime]) then
                LogDatei.log('Failed to copy: ' + infilename, LLError);
            end
            else
            begin
              // setup file
              if FileExists(infilename) then
                if not copyfile(infilename, clientpath + subdir +
                  PathDelim + 'files' + IntToStr(i + 1) + PathDelim +
                  aktProduct.SetupFiles[i].setupFileName,
                  [cffOverwriteFile, cffCreateDestDirectory, cffPreserveTime], True) then
                  LogDatei.log('Failed to copy: ' + infilename, LLError);
              // MST file
              if FileExists(aktProduct.SetupFiles[i].MSTFullFileName) then
                if not copyfile(aktProduct.SetupFiles[i].MSTFullFileName,
                  clientpath + subdir + PathDelim + 'files' +
                  IntToStr(i + 1) + PathDelim + aktProduct.SetupFiles[i].MSTFileName,
                  [cffOverwriteFile, cffCreateDestDirectory, cffPreserveTime], True) then
                  LogDatei.log('Failed to copy: ' +
                    aktProduct.SetupFiles[i].MSTFullFileName, LLError);
            end;
        end;


      // No need to copy libraries and icons for Meta
      if not (osdsettings.runmode in [createMeta]) then
      begin
        //osd-lib.opsiscript
        infilename := genericTemplatePath + Pathdelim + 'osd-lib.opsiscript';
        outfilename := clientpath + subdir + PathDelim + 'osd-lib.opsiscript';
        copyfile(infilename, outfilename, [cffOverwriteFile,
          cffCreateDestDirectory, cffPreserveTime], True);
        // at 'with user' we need osd-lib also at the base dir (without subdir)
        if osdsettings.runmode in [analyzeCreateWithUser, createTemplateWithUser] then
        begin
          outfilename := clientpath + PathDelim + 'osd-lib.opsiscript';
          copyfile(infilename, outfilename, [cffOverwriteFile,
            cffCreateDestDirectory, cffPreserveTime], True);
        end;

        // OS specific install lib

        if osMac in aktProduct.productdata.targetOSset then
        begin
          infilename := genericTemplatePath + Pathdelim +
            'uib_macosinstalllib.opsiscript';
          if fileexists(infilename) then
          begin
            outfilename := clientpath + PathDelim + 'uib_macosinstalllib.opsiscript';
            copyfile(infilename, outfilename, [cffOverwriteFile,
              cffCreateDestDirectory, cffPreserveTime], True);
          end;
        end;
        if (osLin in aktProduct.productdata.targetOSset) or
          (osMulti in aktProduct.productdata.targetOSset) then
        begin
          infilename := genericTemplatePath + Pathdelim + 'uib_lin_install.opsiscript';
          if fileexists(infilename) then
          begin
            outfilename := clientpath + PathDelim + 'uib_lin_install.opsiscript';
            copyfile(infilename, outfilename, [cffOverwriteFile,
              cffCreateDestDirectory, cffPreserveTime], True);
          end;
        end;
        if fileexists(genericTemplatePath + Pathdelim + 'uib_exitcode.opsiscript') then
        begin
          infilename := genericTemplatePath + Pathdelim + 'uib_exitcode.opsiscript';
          outfilename := clientpath + subdir + PathDelim + 'uib_exitcode.opsiscript';
          copyfile(infilename, outfilename, [cffOverwriteFile,
            cffCreateDestDirectory, cffPreserveTime], True);
        end;

        // additional *.opsilib  files from channel



        //product png
        //infilename := templatePath + Pathdelim + 'template.png';
        infilename := aktProduct.productdata.productImageFullFileName;
        outfilename := clientpath + subdir + PathDelim +
          aktProduct.productdata.productId + ExtractFileExt(
          aktProduct.productdata.productImageFullFileName);
        copyfile(infilename, outfilename, [cffOverwriteFile,
          cffCreateDestDirectory, cffPreserveTime], True);

      end; // not meta

      //preinst
      if aktProduct.productdata.useCustomDir then
        infilename := genericTemplatePath + Pathdelim + 'preinst_custom'
      else
        infilename := genericTemplatePath + Pathdelim + 'preinst_empty';
      outfilename := opsipath + pathdelim + 'preinst';
      copyfile(infilename, outfilename, [cffOverwriteFile, cffCreateDestDirectory,
        cffPreserveTime], True);
      //postinst
      if aktProduct.productdata.useCustomDir then
        infilename := genericTemplatePath + Pathdelim + 'postinst_custom'
      else
        infilename := genericTemplatePath + Pathdelim + 'postinst_empty';
      outfilename := opsipath + pathdelim + 'postinst';
      copyfile(infilename, outfilename, [cffOverwriteFile, cffCreateDestDirectory,
        cffPreserveTime], True);

      //customdir
      if aktProduct.productdata.useCustomDir then
      begin
        ForceDirectories(clientpath + subdir + PathDelim + 'custom');
        infilename := genericTemplatePath + Pathdelim + 'customdir_readme.txt';
        outfilename := clientpath + subdir + PathDelim + 'custom' +
          PathDelim + 'readme.txt';
        copyfile(infilename, outfilename, [cffOverwriteFile,
          cffCreateDestDirectory, cffPreserveTime], True);
      end;

      //customdir
      if aktProduct.productdata.customizeProfile then
      begin
        ForceDirectories(clientpath + subdir + PathDelim + 'profile_files');
        infilename := genericTemplatePath + Pathdelim + 'profiledir_readme.txt';
        outfilename := clientpath + subdir + PathDelim + 'profile_files' +
          PathDelim + 'readme.txt';
        copyfile(infilename, outfilename, [cffOverwriteFile,
          cffCreateDestDirectory, cffPreserveTime], True);
      end;

      // write CLIENT_DATA\opsi-meta-data.toml
      if myconfiguration.EnableBackgroundMetaData then
        if osWin in aktProduct.productdata.targetOSset then
        begin
          //LogDatei.log('Collect meta data', LLnotice);
          //osdmeta.aktProdToAktMeta;
          LogDatei.log('Write meta data file to path: ' + clientpath, LLnotice);
          osdmeta.aktMeta.write_product_metadata_ToPath(clientpath);
        end;

      Result := True;
    except
      on E: Exception do
      begin
        logdatei.log('createClientFiles file error: ' + E.ClassName +
          '/' + E.Message, LLError);
      end;
    end;

  finally
    FreeAndNil(patchlist);
    FreeAndNil(infilelist);
  end;
end;

// escape backslashes and quotes for string
function escapeStringForToml(instring: string): string;
begin
  Result := StringReplace(instring, '\', '\\', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '"', '\"', [rfReplaceAll, rfIgnoreCase]);
end;

// escape backslashes and quotes for stringlist
function escapeListForToml(inlist: TStringList): TStringList;
var
  i: integer;
begin
  escapeListForToml := TStringList.Create;
  for i := 0 to inlist.Count - 1 do
    escapeListForToml.Add(escapeStringForToml(inlist[i]));
end;



function createOpsiFiles(changelogMessage: string): boolean;
var
  textlist, helplist, oldtextlist: TStringList;
  i, lineindex: integer;
  mydep: TPDependency;
  myprop: TPProperty;
  tmpstr: string;
  utcoffset: integer;
  utcoffsetstr: string;
  handle : Text;
begin
  Result := False;
  try
    helplist := TStringList.Create;
    textlist := TStringList.Create;
    oldtextlist := TStringList.Create;
    try
      initoutputpath;
      // write project file
      LogDatei.log('Write project file to path: ' + prodpath, LLnotice);
      aktProduct.writeProjectFileToPath(opsipath);
      // remove old projectfile from prodpath
      tmpstr := prodpath + PathDelim + 'opsi-project.osd';
      if fileexists(tmpstr) then
      begin
        LogDatei.log('remove old projectfile: ' + tmpstr, LLnotice);
        DeleteFile(tmpstr);
        // Leave trace
        tmpstr := prodpath+PathDelim+'osd_project_file_moved_to_opsi_dir.txt';
        LogDatei.log('create file: ' + tmpstr, LLnotice);
        FileClose(FileCreateUTF8(tmpstr));
        (*                               S
        AssignFile(handle,opsipath+PathDelim+'osd_project_file_moved_to_opsi_dir.txt');
        Rewrite(handle);
        writeln(handle,'osd_project_file_moved_to_opsi_dir');
        CloseFile(handle);
        *)
      end;

      // create control file (pre 4.3 not_toml style)
      textlist.Add('[Package]');
      textlist.Add('version: ' + IntToStr(aktProduct.productdata.packageversion));
      textlist.Add('depends: ');
      textlist.Add('');
      textlist.Add('[Product]');
      textlist.Add('type: ' + aktProduct.productdata.producttype);
      textlist.Add('id: ' + aktProduct.productdata.productId);
      textlist.Add('name: ' + aktProduct.productdata.productName);
      textlist.Add('description: ' + aktProduct.productdata.description);
      textlist.Add('advice: ' + aktProduct.productdata.advice);
      textlist.Add('version: ' + aktProduct.productdata.productversion);
      textlist.Add('priority: ' + IntToStr(aktProduct.productdata.priority));
      textlist.Add('licenseRequired: False');
      textlist.Add('productClasses: ');
      textlist.Add('setupScript: ' + aktProduct.productdata.setupscript);
      // No uninstall for Meta
      if osdsettings.runmode in [createMeta] then
        textlist.Add('uninstallScript: ')
      else
        textlist.Add('uninstallScript: ' + aktProduct.productdata.uninstallscript);
      textlist.Add('updateScript: ' + aktProduct.productdata.updatescript);
      textlist.Add('alwaysScript: ');
      textlist.Add('onceScript: ');
      textlist.Add('customScript: ');
      if aktProduct.productdata.customizeProfile then
        textlist.Add('userLoginScript: ' + aktProduct.productdata.setupscript)
      else
        textlist.Add('userLoginScript: ');


      //dependencies
      for i := 0 to aktProduct.dependencies.Count - 1 do
      begin
        mydep := TPDependency(aktProduct.dependencies.Items[i]);
        textlist.Add('');
        textlist.Add('[ProductDependency]');
        textlist.Add('action: ' + mydep.Action);
        textlist.Add('requiredProduct: ' + mydep.Required_ProductId);
        case mydep.Required_State of
          noState: ;
          installed: textlist.Add('requiredStatus: installed');
          not_installed: textlist.Add('requiredStatus: not_installed');
          unknown: textlist.Add('requiredStatus: unknown');
        end;
        case mydep.Required_Action of
          noRequest: ;
          setup: textlist.Add('requiredAction: setup');
          uninstall: textlist.Add('requiredAction: uninstall');
          TPActionRequest.update: textlist.Add('requiredAction: update');
        end;
        case mydep.Required_Type of
          doNotMatter: textlist.Add('requirementType: ');
          before: textlist.Add('requirementType: before');
          after: textlist.Add('requirementType: after');
        end;
      end;

      //ProductProperties
      for i := 0 to aktProduct.properties.Count - 1 do
      begin
        myprop := TPProperty(aktProduct.properties.Items[i]);
        textlist.Add('');
        textlist.Add('[ProductProperty]');
        case myprop.Property_Type of
          bool: textlist.Add('type: bool');
          unicode: textlist.Add('type: unicode');
        end;
        textlist.Add('name: ' + myprop.Property_Name);
        textlist.Add('description: ' + myprop.description);
        if myprop.Property_Type = bool then
        begin
          textlist.Add('default: ' + BoolToStr(myprop.boolDefault, True));
        end
        else
        begin
          textlist.Add('multivalue: ' + BoolToStr(myprop.multivalue, True));
          textlist.Add('editable: ' + BoolToStr(myprop.editable, True));
          helplist.Text := myprop.GetValueLines.Text;
          opsiquotelist(helplist, '"');
          if stringListToJsonArray(helplist, tmpstr) then
            textlist.Add('values: ' + tmpstr)
          else
            LogDatei.log('Failed to write property values entry for property: ' +
              myprop.Property_Name, LLerror);
          helplist.Text := myprop.GetDefaultLines.Text;
          opsiquotelist(helplist, '"');
          if stringListToJsonArray(helplist, tmpstr) then
            textlist.Add('default: ' + tmpstr)
          else
            LogDatei.log('Failed to write property default entry for property: ' +
              myprop.Property_Name, LLerror);
        end;
      end;
      textlist.SaveToFile(opsipath + pathdelim + 'control');
      // END: create control file (pre 4.3 not_toml style)

      if myconfiguration.control_in_toml_format then
      begin
        // create control file (4.3 toml style)
        textlist.Clear;
        textlist.Add('[Package]');
        textlist.Add('version = "' +
          IntToStr(aktProduct.productdata.packageversion) + '"');
        textlist.Add('depends = []');
        textlist.Add('');
        textlist.Add('[Product]');
        textlist.Add('type = "' + aktProduct.productdata.producttype + '"');
        textlist.Add('id = "' + aktProduct.productdata.productId + '"');
        textlist.Add('name = "' + escapeStringForToml(
          aktProduct.productdata.productName) + '"');
        textlist.Add('description = """' + escapeStringForToml(
          aktProduct.productdata.description) + '"""');
        textlist.Add('advice = """' + escapeStringForToml(
          aktProduct.productdata.advice) + '"""');
        textlist.Add('version = "' + escapeStringForToml(
          aktProduct.productdata.productversion) + '"');
        textlist.Add('priority = ' + IntToStr(aktProduct.productdata.priority));
        textlist.Add('licenseRequired = false');
        textlist.Add('productClasses = []');
        textlist.Add('setupScript = "' + escapeStringForToml(
          aktProduct.productdata.setupscript) + '"');
        // No uninstall for Meta
        if osdsettings.runmode in [createMeta] then
          textlist.Add('uninstallScript = ""')
        else
          textlist.Add('uninstallScript = "' +
            escapeStringForToml(aktProduct.productdata.uninstallscript) + '"');

        textlist.Add('updateScript = "' + escapeStringForToml(
          aktProduct.productdata.updatescript) + '"');
        textlist.Add('alwaysScript = ""');
        textlist.Add('onceScript = ""');
        textlist.Add('customScript = ""');
        if aktProduct.productdata.customizeProfile then
          textlist.Add('userLoginScript = "' + escapeStringForToml(
            aktProduct.productdata.setupscript) + '"')
        else
          textlist.Add('userLoginScript = ""');
        // the next line avoids a bug in  opsi-makepackage 4.3.0.36 [python-opsi=4.3.0.14]
        textlist.Add('windowsSoftwareIds = []');

        //dependencies
        for i := 0 to aktProduct.dependencies.Count - 1 do
        begin
          mydep := TPDependency(aktProduct.dependencies.Items[i]);
          textlist.Add('');
          textlist.Add('[[ProductDependency]]');
          textlist.Add('action = "' + mydep.Action + '"');
          textlist.Add('requiredProduct = "' + mydep.Required_ProductId + '"');
          case mydep.Required_State of
            noState: ;
            installed: textlist.Add('requiredStatus = "installed"');
            not_installed: textlist.Add('requiredStatus = "not_installed"');
            unknown: textlist.Add('requiredStatus = "unknown"');
          end;
          case mydep.Required_Action of
            noRequest: ;
            setup: textlist.Add('requiredAction = "setup"');
            uninstall: textlist.Add('requiredAction = "uninstall"');
            TPActionRequest.update: textlist.Add('requiredAction = "update"');
          end;
          case mydep.Required_Type of
            doNotMatter: textlist.Add('requirementType = ""');
            before: textlist.Add('requirementType = "before"');
            after: textlist.Add('requirementType = "after"');
          end;
        end;

        //ProductProperties
        for i := 0 to aktProduct.properties.Count - 1 do
        begin
          myprop := TPProperty(aktProduct.properties.Items[i]);
          textlist.Add('');
          textlist.Add('[[ProductProperty]]');
          case myprop.Property_Type of
            bool: textlist.Add('type = "bool"');
            unicode: textlist.Add('type = "unicode"');
          end;
          textlist.Add('name = "' + myprop.Property_Name + '"');
          textlist.Add('description = "' + escapeStringForToml(myprop.description) + '"');
          if myprop.Property_Type = bool then
          begin
            textlist.Add('default = [' + lowercase(
              BoolToStr(myprop.boolDefault, True)) + ']');
          end
          else
          begin
            textlist.Add('multivalue = ' + lowercase(BoolToStr(myprop.multivalue, True)));
            textlist.Add('editable = ' + lowercase(BoolToStr(myprop.editable, True)));
            FreeAndNil(helplist);
            helplist := escapeListForToml(TStringList(myprop.GetValueLines));
            opsiquotelist(helplist, '"');
            if stringListToJsonArray(helplist, tmpstr) then
              textlist.Add('values = ' + tmpstr)
            else
              LogDatei.log('Failed to write property values entry for property: ' +
                myprop.Property_Name, LLerror);
            FreeAndNil(helplist);
            helplist := escapeListForToml(TStringList(myprop.GetDefaultLines));
            opsiquotelist(helplist, '"');
            if stringListToJsonArray(helplist, tmpstr) then
              textlist.Add('default = ' + tmpstr)
            else
              LogDatei.log('Failed to write property default entry for property: ' +
                myprop.Property_Name, LLerror);
          end;
        end;
        textlist.SaveToFile(opsipath + pathdelim + 'control.toml');
        // END: create control file (4.3 toml style)
      end;


      // changelog
      textlist.Clear;
      // new 4.3 style
      textlist.Add('');
      textlist.Add('# Changelog ' + aktProduct.productdata.productId);
      textlist.Add('');
      tmpstr := aktProduct.productdata.productversion + '-' + IntToStr(
        aktProduct.productdata.packageversion);
      textlist.Add('## [' + tmpstr + '] - ' + FormatDateTime('yyyy-mm-dd', now));
      textlist.Add('');
      if changelogMessage <> '' then
      begin
        textlist.Add(changelogMessage);
        LogDatei.log('Added changelog message:' +changelogMessage,LLinfo);
      end
      else
      begin
        textlist.Add('### Added');
        textlist.Add('- created / updated to: ' + aktProduct.productdata.productId +
          ' ' + tmpstr);
      end;
      textlist.Add('  using opsi-setup-detector - Version: ' + myVersion);
      textlist.Add('');
      textlist.Add('(' + myconfiguration.fullName + ' <' +
        myconfiguration.email_address + '>)');
      textlist.Add('');
      if fileexists(opsipath + pathdelim + 'changelog.md') then
      begin
        LogDatei.log('Found old changelog.md and append this to the new entry.',LLinfo);
        oldtextlist.LoadFromFile(opsipath + pathdelim + 'changelog.md');
        // remove: # Changelog <productId>
        lineindex := oldtextlist.IndexOf('# Changelog ' +
          aktProduct.productdata.productId);
        if lineindex > -1 then oldtextlist.Delete(lineindex);
        textlist.Append(oldtextlist.Text);
      end
      else if fileexists(opsipath + pathdelim + 'changelog.txt') then
      begin
        LogDatei.log('Found old changelog.txt and append this to the new entry.',LLinfo);
        oldtextlist.LoadFromFile(opsipath + pathdelim + 'changelog.txt');
        // remove: # Changelog <productId>
        lineindex := oldtextlist.IndexOf('# Changelog ' +
          aktProduct.productdata.productId);
        if lineindex > -1 then oldtextlist.Delete(lineindex);
        textlist.Append(oldtextlist.Text);
      end;
      // save the file
      textlist.SaveToFile(opsipath + pathdelim + 'changelog.md');

      // readme.txt
      if (myconfiguration.Readme_txt_templ <> '') and
        FileExists(myconfiguration.Readme_txt_templ) then
      begin
        textlist.Clear;
        utcoffset := (GetLocalTimeOffset div 60) * 100 * -1;
        if utcoffset >= 0 then
          utcoffsetstr := '+';
        utcoffsetstr := utcoffsetstr + format('%4.4d', [utcoffset]);
        textlist.LoadFromFile(myconfiguration.Readme_txt_templ);
        textlist.Add('');
        tmpstr := aktProduct.productdata.productversion + '-' + IntToStr(
          aktProduct.productdata.packageversion);
        textlist.Add(aktProduct.productdata.productId + ' (' + tmpstr + ')');
        textlist.Add('');
        textlist.Add('-- ' + myconfiguration.fullName + ' <' +
          myconfiguration.email_address + '> ' + FormatDateTime(
          'ddd, dd mmm yyyy hh:nn:ss', LocalTimeToUniversal(now)) + ' ' + utcoffsetstr);
        //mon, 04 Jun 12:00:00 + 0100
        textlist.SaveToFile(opsipath + pathdelim + 'readme.txt');
      end;


      Result := True;
    except
      on E: Exception do
      begin
        LogDatei.log('Error in createOpsiFiles', LLError);
        LogDatei.log('Error: ' + e.Message, LLError);
      end;
    end;
  finally
    FreeAndNil(textlist);
    FreeAndNil(oldtextlist);
  end;
end;

function bakupOldProductDir: boolean;


  function bakupOldInDir(mydir: string): boolean;
  var
    backupfiles: TStringList;
    fname, fbakname, bakpostfix: string;
    i, k: integer;
  begin
    Result := True;
    backupfiles := TStringList.Create;
    // Del really old files
    FindAllFiles(backupfiles, mydir, '*.*', False);
    procmess;
    for i := 0 to backupfiles.Count - 1 do
      MakeBakFiles(backupfiles.Strings[i], 3);
  end;

begin
  Result := True;
  if not bakupOldInDir(clientpath) then
    Result := False;
  if not bakupOldInDir(opsipath) then
    Result := False;
end;

function delOldProductDir: boolean;
begin
  Result := True;
  procmess;
  if not DeleteDirectory(clientpath, False) then
    Result := False;
  procmess;
  if not DeleteDirectory(opsipath, False) then
    Result := False;
  procmess;
end;


function createProductdirectory: boolean;
var
  goon: boolean;
  task: string;
begin
  initoutputpath;
  goon := True;
  if DirectoryExists(prodpath) then
  begin
    if osdsettings.showgui then
    begin
      {$IFDEF OSDGUI}
      LogDatei.log('Target directory exists. Ask what to do.', LLinfo);
      task := '';
      // https://specials.rejbrand.se/TTaskDialog/
      with TTaskDialog.Create(resultForm1) do
      try
        Title := rsDirectory + ' ' + prodpath + ' ' +
          rsStillExitsWarningDeleteOverwrite;
        Caption := 'opsi-setup-detector';
        Text := rsConfirmBackupOrRemovalTitle;
        CommonButtons := [];
        with TTaskDialogButtonItem(Buttons.Add) do
        begin
          Caption := rsConfirmBackupCaption;
          //CommandLinkHint := rsConfirmBackupHint;
          ModalResult := mrYes;
        end;
        with TTaskDialogButtonItem(Buttons.Add) do
        begin
          Caption := rsConfirmDeleteCaption;
          //CommandLinkHint := rsConfirmDeleteHint;
          ModalResult := mrNo;
        end;
        with TTaskDialogButtonItem(Buttons.Add) do
        begin
          Caption := rsConfirmAbortCaption;
          //CommandLinkHint := rsConfirmAbortHint;
          ModalResult := mrAbort;
        end;
        MainIcon := tdiQuestion;
        //include(Flags,[tfExpandFooterArea]);
        Flags := [tfUseCommandLinks, tfAllowDialogCancellation, tfExpandFooterArea];
        ExpandButtonCaption := rsConfirmExpandButton;
        ExpandedText := rsConfirmExpandedText;
        if Execute then
        begin
          if ModalResult = mrYes then
          begin
            task := 'bak';
            LogDatei.log('Choosed to make backups', LLinfo);
          end
          else if ModalResult = mrNo then
          begin
            task := 'del';
            LogDatei.log('Choosed to make no backups', LLinfo);
          end
          else
            //if ModalResult = mrAbort then
          begin
            task := 'abort';
            LogDatei.log('Choosed to abort', LLinfo);
          end;
        end;
      finally
        Free;
      end;
      {$ENDIF OSDGUI}
    end
    else
    begin
      // nogui mode
      if forceProductId <> '' then
      begin
        LogDatei.log('forceProductId is: ' + forceProductId, LLinfo);
        task := 'del';
        LogDatei.log('Choosed to make no backups (overwrite)', LLinfo);
      end;
    end;

    if task = 'abort' then
      goon := False;
    if task = 'del' then
      if not delOldProductDir then
      begin
        LogDatei.log('Could not recursive delete dir: ' + prodpath, LLCritical);
        system.ExitCode := 1;
        goon := False;
      end;
    if task = 'bak' then
      if not bakupOldProductDir then
      begin
        LogDatei.log('Could not inernally backup dir: ' + prodpath, LLCritical);
        system.ExitCode := 1;
        goon := False;
      end;
  end;
  if goon then
  begin
    if not DirectoryExists(prodpath) then
    begin
      if not ForceDirectories(prodpath) then
      begin
        Logdatei.log('Could not create directory: ' + prodpath, LLCritical);
        system.ExitCode := 1;
        if osdsettings.showgui then
          MessageDlg('opsi-setup-detector', rsCouldNotCreateDirectoryWarning +
            prodpath, mtError, [mbOK], '');
        goon := False;
      end;
    end;
    if not DirectoryExists(clientpath) then
    begin
      if not ForceDirectories(clientpath) then
      begin
        Logdatei.log('Could not create directory: ' + clientpath, LLCritical);
        system.ExitCode := 1;
        if osdsettings.showgui then
          MessageDlg('opsi-setup-detector', rsCouldNotCreateDirectoryWarning +
            clientpath, mtError, [mbOK], '');
        goon := False;
      end;
    end;
    if not DirectoryExists(opsipath) then
    begin
      if not ForceDirectories(opsipath) then
      begin
        Logdatei.log('Could not create directory: ' + opsipath, LLCritical);
        system.ExitCode := 1;
        if osdsettings.showgui then
          MessageDlg('opsi-setup-detector', rsCouldNotCreateDirectoryWarning +
            opsipath, mtError, [mbOK], '');
        goon := False;
      end;
    end;
  end;
  Result := goon;
  procmess;
end;


function createProductStructure: boolean;
var
  prodpath, clientpath, opsipath: string;
  goon: boolean;
begin
  goon := True;
  if not createProductdirectory then
  begin
    Logdatei.log('createProductdirectory failed', LLCritical);
    system.ExitCode := 1;
    goon := False;
  end
  else
    Logdatei.log('createProductdirectory done', LLnotice);
  procmess;
  if not (goon and createOpsiFiles('')) then
  begin
    Logdatei.log('createOpsiFiles failed', LLCritical);
    system.ExitCode := 1;
    goon := False;
  end
  else
    Logdatei.log('createOpsiFiles done', LLnotice);
  procmess;
  if not (goon and createClientFiles) then
  begin
    Logdatei.log('createClientFiles failed', LLCritical);
    system.ExitCode := 1;
    goon := False;
  end
  else
    Logdatei.log('createClientFiles done', LLnotice);
  procmess;
  Result := goon;
end;




function ExtractBetween(const Value, A, B: string): string;
var
  aPos, bPos: integer;
begin
  Result := '';
  aPos := pos(A, Value);
  if aPos > 0 then
  begin
    aPos := aPos + Length(A);
    bPos := PosEx(B, Value, aPos);
    if bPos > 0 then
    begin
      Result := Copy(Value, aPos, bPos - aPos);
    end;
  end;
end;


function callOpsiPackageBuilder: boolean;
var
  msg1: string;
  description: string;
  buildCallbinary: string = '';
  buildCallparams: TStringList;
  ParamStr: string = '';
  OpsiBuilderProcess: TProcess;
  packit: boolean = False;
  errorstate: boolean = False;
  notused: string = '(not used)';
  output: string;
  paramlist: TStringList;
begin
  Result := True;
  {$IFDEF OSDGUI}
  logdatei.log('Start callOpsiPackageBuilder', LLDebug2);
  buildCallparams := TStringList.Create;
  OpsiBuilderProcess := process.TProcess.Create(nil);
  buildCallbinary := '"' + myconfiguration.PathToOpsiPackageBuilder + '"';
  paramlist := TStringList.Create;
  //paramlist.Add('-–log=c:\opsi.org\applog\opb-call.log');
  //ParamStr := ' -–log=c:\opsi.org\applog\opb-call.log';

  //ParamStr := ParamStr + ' -p=' + myconfiguration.workbench_Path;
  ParamStr := '-p=' + myconfiguration.workbench_Path;
  if AnsiLastChar(ParamStr) <> DirectorySeparator then
    ParamStr := ParamStr + DirectorySeparator;
  ParamStr := ParamStr + aktProduct.productdata.productId;
  paramlist.Add(ParamStr);
  // call packagebuilder interactive
  if osdsettings.CreateModeIndex = 2 then
  begin
    OpsiBuilderProcess.ShowWindow := swoShowNormal;
  end
  else  // build
  // call packagebuilder silent in commandline build mode
  if osdsettings.CreateModeIndex = 1 then
  begin
    paramlist.Add('--quiet');
    // build mode : build only
    if osdsettings.BuildModeIndex = 0 then
    begin
      paramlist.Add('--build=rebuild');
    end;
    // build mode : build + install
    if osdsettings.BuildModeIndex = 1 then
    begin
      paramlist.Add('--build=rebuild');
      paramlist.Add('--install');
    end;
    OpsiBuilderProcess.ShowWindow := swoMinimize;
  end;

  LogDatei.log('Try to call opsi packagebuilder', LLnotice);
  OpsiBuilderProcess.Executable := buildCallbinary;
  OpsiBuilderProcess.Parameters.Assign(paramlist);
  OpsiBuilderProcess.Options := OpsiBuilderProcess.Options + [poWaitOnExit];
  LogDatei.log('Call: ' + OpsiBuilderProcess.Executable + ' with: ' +
    OpsiBuilderProcess.Parameters.Text, LLInfo);
  LogDatei.log('Call: ' + OpsiBuilderProcess.CommandLine, LLInfo);
  // execute opsiPacketBuilder
  try
    OpsiBuilderProcess.Execute;
    // call packagebuilder silent in commandline build mode
    if osdsettings.CreateModeIndex = 1 then
    begin
      resultForm1.PanelProcess.Visible := True;
      resultForm1.processStatement.Caption := 'invoke opsi package builder ...';
      procmess;
      while OpsiBuilderProcess.Running do
      begin
        procmess;
      end;
    end;
  except
    on E: Exception do
    begin
      errorstate := True;
      Result := False;
      LogDatei.log('Exception while calling ' + buildCallbinary +
        ' Message: ' + E.message, LLerror);
      if osdsettings.showgui then
        ShowMessage(sErrOpsiPackageBuilderStart);
    end;
  end;
  FreeAndNil(paramlist);

  resultForm1.PanelProcess.Visible := False;
  begin
    if (OpsiBuilderProcess.ExitStatus = 0) then
    begin
      LogDatei.log('Finished calling ' + buildCallbinary + ' with exitcode: 0 ', LLinfo);
    end
    else
    begin
      LogDatei.log('Error while calling ' + buildCallbinary +
        ' with exitcode: ' + IntToStr(OpsiBuilderProcess.ExitStatus), LLerror);
      if osdsettings.showgui then
        ShowMessage(sErrOpsiPackageBuilderStart);
    end;
    Result := False;
  end;
  logdatei.log('Here comes the OpsiPackageBuilder log', LLnotice);
  logdatei.includelogtail('c:\opsi.org\applog\opb-call.log', 50, 'utf8');
  logdatei.log('Finished callOpsiPackageBuilder', LLinfo);
  {$ENDIF OSDGUI}
end;   // execute OPSIPackageBuilder

function buildWithOpsiService: boolean;
var
  omc: TOpsiMethodCall;
  params: array of string;
  packagedir, packagefile, serviceresult: string;
  errorOccured: boolean;

  function servicecall(method, param: string): boolean;
  begin
    Result := False;
    setlength(params, 1);
    params[0] := param;
    omc := TOpsiMethodCall.Create(method, params);
    serviceresult := localservicedata.CheckAndRetrieveString(omc, errorOccured);
    if Assigned(omc) then
      FreeAndNil(omc);
    if errorOccured then
    begin
      LogDatei.log('Error: ' + serviceresult, LLError);
    end
    else
    begin
      LogDatei.log('JSON result: ' + serviceresult, LLinfo);
      Result := True;
    end;
  end;

begin
  LogDatei.log('Try to call opsi service', LLnotice);
  Result := False;
  {$IFDEF OSDGUI}
  resultForm1.PanelProcess.Visible := True;
  resultForm1.processStatement.Caption := 'invoke opsi service ...';
  procmess;
  {$ENDIF OSDGUI}
  //packagedir := '/var/lib/opsi/workbench/';
  // packagedir may be relative to workbench
  packagedir := '';
  packagedir := packagedir + aktProduct.productdata.productId;
  packagefile := packagedir + '/' + aktProduct.productdata.productId;
  packagefile := packagefile + '_' + aktProduct.productdata.productversion;
  packagefile := packagefile + '-' + IntToStr(aktProduct.productdata.packageversion);
  packagefile := packagefile + '.opsi';
  //if resultForm1.radioBuildModebuildOnly.Checked = True then
  if osdsettings.BuildModeIndex = 0 then
  begin
    if servicecall('workbench_buildPackage', packagedir) then
    begin
      Result := True;
      LogDatei.log('Package ' + packagefile + ' successful build', LLnotice);
    end
    else
    begin
      LogDatei.log('Package ' + packagefile + ' failed to build', LLerror);
      system.ExitCode := 1;
    end;
  end;

  //if resultForm1.radioBuildModebuildInstall.Checked = True then
  if osdsettings.BuildModeIndex = 1 then
  begin

    if servicecall('workbench_installPackage', packagedir) then
    begin
      Result := True;
      LogDatei.log('Package ' + packagefile + ' successful build + installed', LLnotice);
    end
    else
    begin
      LogDatei.log('Package ' + packagefile + ' failed to build +install', LLerror);
      system.ExitCode := 1;
    end;

  end;
  {$IFDEF OSDGUI}
  resultForm1.PanelProcess.Visible := False;
  {$ENDIF OSDGUI}
end;


function callServiceOrPackageBuilder: boolean;
var
  callOpB: boolean = True;
begin
  if startOpsiServiceConnection then
  begin
    LogDatei.log('opsi-service connected', LLinfo);
    if CompareDotSeparatedNumbers(opsiserviceversion, '>', '4.2.0.311') then
    begin
      LogDatei.log('opsi-service can be used for build & install', LLinfo);
      callOpB := False;
    end;
  end;

  if callOpB then Result := callOpsiPackageBuilder
  else
    Result := buildWithOpsiService;
end;

end.
