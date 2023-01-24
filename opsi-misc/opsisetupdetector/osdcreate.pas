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
  oswebservice;

function createProductStructure: boolean;
procedure callOpsiPackageBuilder;
procedure callServiceOrPackageBuilder;

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
  rsStillExitsWarningDeleteOverwrite = ' still exits. Should we make backup files ?';
  rsCouldNotCreateDirectoryWarning = 'Could not create directory: ';
  rsConfirmBackupOrRemovalTitle =
    'Should we use the existing directory and make backup files of old files, before creating the new ones ?  Otherwise the exiting directory will be deleted before creating a new one.';
  rsConfirmBackupCaption = 'Yes (Backup)';
  rsConfirmAbortCaption = 'Abort';
  rsConfirmDeleteCaption = 'No';
  rsConfirmAbortHint = 'Leave existing directory unchanged and abort';
  rsConfirmDeleteHint = 'Delete the complete directory before creating the new one.';
  rsConfirmBackupHint =
    'All files that are directly in the directories "CLIENT_DATA" and "OPSI" will copy to an backup file.';
  rsConfirmExpandButton = 'More Information ...';
  rsConfirmExpandedText =
    'Yes: All files that are directly in the directories "CLIENT_DATA" and "OPSI" will be saved to an backup file before the new files are created' + LineEnding + 'No: Delete the complete directory before creating the new one.' + LineEnding + 'Abort: Leave existing directory unchanged and abort';


implementation

uses
  osdform;

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
  if (length(s1) >= 1) and (length(markstr) >= 1) then
  begin
    startmark := markstr[1];
    if length(markstr) >= 2 then
      endmark := markstr[2] // different marks (brackets) at begin and end
    else
      endmark := startmark; // the same mark (quote) at begin and end
    if not (pos(startmark, s1) = 1) and AnsiEndsStr(endmark, s1) then
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
    mywrite('creating: ' + outfile + ' from: ' + infile);
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
        logdatei.log('patchScript file error: ' + E.ClassName + '/' + E.Message, LLError);
    end;
  {$I-}//end use exceptions
  end
  else
    logdatei.log('patchScript file warning: infile for: ' + ExtractFileName(outfile) + ' not found', LLwarning);
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
    //myfilepath := templatePath + Pathdelim + 'win' + Pathdelim + myfilename;
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
  if (osdsettings.runmode = analyzeCreateWithUser) then
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
(*
      else if (proptmpstr = LowerCase('DesktopIcon')) then
        str := str + 'DefVar $DesktopIcon$' + LineEnding

      else if (proptmpstr = ('install_architecture')) then
        str := str + 'DefVar $install_architecture$' + LineEnding
      *)
      else
        str := str + 'DefVar $' + proptmpstr + '$' + LineEnding;
    end;
    patchlist.add('#@stringVars*#=' + str);

    str := '';
    patchlist.add('#@productId*#=' + aktProduct.productdata.productId);
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
      if aktProduct.SetupFiles[0].installerId = stMsi then
      begin
        readFileToList('HandleMsiUninstallSections.opsiscript', sectionlist);
      end;

    // #@GetProductProperty*#:
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
    (*
      str := 'comment "Start Pre Install hook :"' + LineEnding +
        myconfiguration.preInstallLines.Text;
    patchlist.add('#@preInstallLines*#=' + str);
    *)

    str := myconfiguration.postInstallLines.Text;
    if str <> '' then
      postinstalllist.Add(myconfiguration.postInstallLines.Text + LineEnding);
    (*
      str := 'comment "Start Post UnInstall hook :"' + LineEnding +
        myconfiguration.postInstallLines.Text;
    patchlist.add('#@postInstallLines*#=' + str);
    *)

    str := myconfiguration.preUninstallLines.Text;
    if str <> '' then
      str := 'comment "Start Pre UnInstall hook :"' + LineEnding +
        myconfiguration.preUninstallLines.Text;
    patchlist.add('#@preUninstallLines*#=' + str);

    str := myconfiguration.postUnInstallLines.Text;
    if str <> '' then
      postUnInstallList.Add(myconfiguration.postUnInstallLines.Text + LineEnding);
    (*
      str := 'comment "Start Post UnInstall hook :"' + LineEnding +
        myconfiguration.postUnInstallLines.Text;
    patchlist.add('#@postUninstallLines*#=' + str);
    *)

    str := '';
    if aktProduct.properties.propExists('DesktopIcon') then
    begin

      readFileToList('HandleDesktopIcon.opsiscript', postinstalllist);

      readFileToList('HandleDesktopIconSections.opsiscript', sectionlist);
      str := 'comment "Start Remove Desktop Icon Handling :"' +
        LineEnding + 'Linkfolder_remove_desktop_icon';
    end;
    patchlist.add('#@DelsubHandleDesktopIcon*#=' + str);

      (*
      filepath := templatePath + Pathdelim + 'win' + Pathdelim +
        'SetupHandleDesktopIcon.opsiscript';
      strlist.Clear;
      if FileExists(filepath) then
        strlist.LoadFromFile(filepath)
      else
        LogDatei.log('Error: File not found: ' + filepath, LLerror);
      ;
      str := 'comment "Start Desktop Icon Handling :"' + LineEnding + strlist.Text;

    end;
    //patchlist.add('#@SetupHandleDesktopIcon*#=' + str);

    str := '';

    if aktProduct.properties.propExists('DesktopIcon') then
    begin
      //strlist.LoadFromFile(templatePath + Pathdelim + 'DelsubHandleDesktopIcon.opsiscript');
      str := 'comment "Start Desktop Icon Handling :"' + LineEnding +
        'Linkfolder_remove_desktop_icon';
    end;

    //patchlist.add('#@DelsubHandleDesktopIcon*#=' + str);

    str := '';
    if aktProduct.properties.propExists('DesktopIcon') then
    begin
      strlist.LoadFromFile(templatePath + Pathdelim + 'win' + Pathdelim +
        'SetupDesktopIconSection.opsiscript');
      str := strlist.Text;
    end;
    patchlist.add('#@SetupDesktopiconSectionLines*#=' + str);

    str := '';
    if aktProduct.properties.propExists('DesktopIcon') then
    begin
      strlist.LoadFromFile(templatePath + Pathdelim + 'win' + Pathdelim +
        'DelsubDesktopIconSection.opsiscript');
      str := strlist.Text;
    end;
    patchlist.add('#@DelsubDesktopiconSectionLines*#=' + str);
    *)


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

      patchlist.add('#@installCommandLine' + IntToStr(i + 1) + '*#=' +
        aktProduct.SetupFiles[i].installCommandLine);

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
  if (osdsettings.runmode = analyzeCreateWithUser) then
    pre_id := 'with-user';

  //templatePath := templatePath + Pathdelim + pre_id;

  try
    try
      patchlist := TStringList.Create;
      fillPatchList;
      infilelist := TStringList.Create;
      case osdsettings.runmode of
        singleAnalyzeCreate:
        begin
          infilelist.Add('setupsingle.opsiscript');
          infilelist.Add('delincsingle.opsiinc');
          infilelist.Add('uninstallsingle.opsiscript');
          if aktProduct.SetupFiles[0].installerId = stMsi then
            infilelist.Add('delincmsisingle.opsiinc');
        end;
        twoAnalyzeCreate_1, twoAnalyzeCreate_2:
        begin
          infilelist.Add('setupdouble.opsiscript');
          infilelist.Add('delincdouble.opsiinc');
          infilelist.Add('uninstalldouble.opsiscript');
          if aktProduct.SetupFiles[0].installerId = stMsi then
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
        analyzeCreateWithUser:
        begin
          infilelist.Add('setup.opsiscript');
          infilelist.Add('sections.opsiinc');
          infilelist.Add('declarations.opsiinc');
          infilelist.Add('localsetup\declarations-local.opsiinc');
          if aktProduct.SetupFiles[0].installerId = stMsi then
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
            infilename := getFilePath(infilelist.Strings[i]);
            (*
            infilename := templatePath + PathDelim + templateChannelDir +
              Pathdelim + pre_id + PathDelim + infilelist.Strings[i];
            if not FileExists(infilename) then
            begin
              infilename := templatePath + PathDelim + 'default' +
                Pathdelim + pre_id + PathDelim + infilelist.Strings[i];
              logdatei.log('File: ' + infilelist.Strings[i] +
                ' not found at template channel: ' + templateChannelDir +
                ' - fall back to default', LLinfo);
            end; *)
            outfilename := clientpath + PathDelim + infilelist.Strings[i];
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
          end;
          twoAnalyzeCreate_1, twoAnalyzeCreate_2:
          begin
            infilelist.Add('declarations.opsiinc');
          end;
          createTemplate:
          begin
            infilelist.Add('declarations.opsiinc');
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
          analyzeCreateWithUser:
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
          analyzeCreateWithUser:
          begin
          end;
        end;
      end;

      // additional optional files for 'custom'
      if (aktProduct.productdata.channelDir = 'custom') then
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
          analyzeCreateWithUser:
          begin
          end;
        end;
      end;

      // we did it for analyzeCreateWithUser right now
      if not (osdsettings.runmode = analyzeCreateWithUser) then
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
              (*
              infilename := templatePath + Pathdelim + templateChannelDir +
                PathDelim + pre_id + PathDelim +
                StringReplace(tmpname, 'single', 'templ', []) + tmpext;
              if not FileExists(infilename) then
              begin
                infilename := templatePath + PathDelim + 'default' +
                  Pathdelim + pre_id + PathDelim +
                  StringReplace(tmpname, 'single', 'templ', []) + tmpext;
                logdatei.log('File: ' + infilename +
                  ' not found at template channel: ' + templateChannelDir +
                  ' - fall back to default', LLinfo);
              end;
              *)
          end
          else if (tmppref = 'lin') and (aktProduct.SetupFiles[1].active = False) then
          begin
            infilename := getFilePath(StringReplace(tmpname, 'single',
              'templ', []) + tmpext);
              (*
              infilename := templatePath + Pathdelim + templateChannelDir +
                PathDelim + pre_id + PathDelim +
                StringReplace(tmpname, 'single', 'templ', []) + tmpext;
              if not FileExists(infilename) then
              begin
                infilename := templatePath + PathDelim + 'default' +
                  PathDelim + pre_id + PathDelim +
                  StringReplace(tmpname, 'single', 'templ', []) + tmpext;
                logdatei.log('File: ' + infilename +
                  ' not found at template channel: ' + templateChannelDir +
                  ' - fall back to default', LLinfo);
              end;
              *)
          end
          else if (tmppref = 'mac') and (aktProduct.SetupFiles[2].active = False) then
          begin
            infilename := getFilePath(StringReplace(tmpname, 'single',
              'templ', []) + tmpext);
              (*
              infilename := templatePath + Pathdelim + templateChannelDir +
                PathDelim + pre_id + PathDelim +
                StringReplace(tmpname, 'single', 'templ', []) + tmpext;
              if not FileExists(infilename) then
              begin
                infilename := templatePath + PathDelim + 'default' +
                  PathDelim + pre_id + PathDelim +
                  StringReplace(tmpname, 'single', 'templ', []) + tmpext;
                logdatei.log('File: ' + infilename +
                  ' not found at template channel: ' + templateChannelDir +
                  ' - fall back to default', LLinfo);
              end;
              *)
          end
          else
          begin
            infilename := getFilePath(infilelist.Strings[i]);
              (*
              infilename := templatePath + PathDelim + templateChannelDir +
                Pathdelim + pre_id + PathDelim + infilelist.Strings[i];
              if not FileExists(infilename) then
              begin
                infilename := templatePath + PathDelim + 'default' +
                  Pathdelim + pre_id + PathDelim + infilelist.Strings[i];
                logdatei.log('File: ' + infilename +
                  ' not found at template channel: ' + templateChannelDir +
                  ' - fall back to default', LLinfo);
              end;
              *)
          end;
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


      (*
      // define_vars_multi
      // none at windows template or createMeta
      if not (((osdsettings.runmode = createTemplate) and
        (osWin in aktProduct.productdata.targetOSset)) or
        (osdsettings.runmode = createMeta) or
        // none at analyzeCreateWithUser
        (osdsettings.runmode = analyzeCreateWithUser) or
        // none at template channel training
        (aktProduct.productdata.channelDir = 'training') or
        // none at template channel structured
        (aktProduct.productdata.channelDir = 'structured')) then
      begin
        //infilename := genericTemplatePath + Pathdelim + 'define_vars_multi.opsiscript';
        infilename := templatePath + PathDelim + templateChannelDir +
              Pathdelim + pre_id + PathDelim + 'declarations.opsiinc';
            if not FileExists(infilename) then
            begin
              infilename := templatePath + PathDelim + 'default' +
                Pathdelim + pre_id + PathDelim + infilelist.Strings[i];
              logdatei.log('File: ' + infilename +
                ' not found at template channel: ' + templateChannelDir +
                ' - fall back to default', LLinfo);
            end;
        outfilename := clientpath + PathDelim + 'define_vars_multi.opsiscript';
        patchScript(infilename, outfilename);
      end;
      *)

    (*
    // setup script
    infilename := templatePath + Pathdelim + insetup;
    outfilename := clientpath + PathDelim + aktProduct.productdata.setupscript;
    patchScript(infilename, outfilename);
    // delsub script
    infilename := templatePath + Pathdelim + indelsub;
    outfilename := clientpath + PathDelim + aktProduct.productdata.delsubscript;
    patchScript(infilename, outfilename);
    // uninstall script
    infilename := templatePath + Pathdelim + inuninstall;
    outfilename := clientpath + PathDelim + aktProduct.productdata.uninstallscript;
    patchScript(infilename, outfilename);    *)

      subdir := '';
      if osdsettings.runmode = analyzeCreateWithUser then
        subdir := pathdelim + 'localsetup';
      // No need to copy installer for templates or Meta
      if not (osdsettings.runmode in [createTemplate, createMultiTemplate,
        createMeta]) then
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

        // install lib

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

      // write project file
      aktProduct.writeProjectFileToPath(prodpath);

      Result := True;
      ;
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

function createOpsiFiles: boolean;
var
  textlist, helplist: TStringList;
  i: integer;
  mydep: TPDependency;
  myprop: TPProperty;
  tmpstr: string;
  utcoffset: integer;
  utcoffsetstr: string;
begin
  Result := False;
  try
    helplist := TStringList.Create;
    textlist := TStringList.Create;
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
    textlist.Add('licenseRequired: ');
    textlist.Add('productClasses: ');
    textlist.Add('setupScript: ' + aktProduct.productdata.setupscript);
    // No uninstall for Meta
    if not (osdsettings.runmode in [createMeta]) then
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
      textlist.Add('action: setup');
      textlist.Add('requiredProduct: ' + mydep.Required_ProductId);
      case mydep.Required_State of
        noState: ;
        installed: textlist.Add('requiredStatus: installed');
        not_installed: textlist.Add('requiredStatus: not installed');
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
          textlist.Add('values: ' + tmpstr);
        helplist.Text := myprop.GetDefaultLines.Text;
        opsiquotelist(helplist, '"');
        if stringListToJsonArray(helplist, tmpstr) then
          textlist.Add('default: ' + tmpstr);
      end;
    end;
    textlist.SaveToFile(opsipath + pathdelim + 'control');
    //FreeAndNil(textlist);

    // changelog
    textlist.Clear;
    utcoffset := (GetLocalTimeOffset div 60) * 100 * -1;
    if utcoffset >= 0 then
      utcoffsetstr := '+';
    utcoffsetstr := utcoffsetstr + format('%4.4d', [utcoffset]);
    textlist.Add('');
    textlist.Add('[Changelog]');
    tmpstr := aktProduct.productdata.productversion + '-' + IntToStr(
      aktProduct.productdata.packageversion);
    textlist.Add(aktProduct.productdata.productId + ' (' + tmpstr +
      ') stable; urgency=medium');
    textlist.Add('');
    textlist.Add('  * initial by opsi-setup-detector - Version: ' + myVersion);
    textlist.Add('');
    textlist.Add('-- ' + myconfiguration.fullName + ' <' +
      myconfiguration.email_address + '> ' + FormatDateTime(
      'ddd, dd mmm yyyy hh:nn:ss', LocalTimeToUniversal(now)) + ' ' + utcoffsetstr);
    //mon, 04 Jun 12:00:00 + 0100
    textlist.SaveToFile(opsipath + pathdelim + 'changelog.txt');

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

    FreeAndNil(textlist);
    Result := True;
  except
    LogDatei.log('Error in createOpsiFiles', LLError);
    FreeAndNil(textlist);
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
    Application.ProcessMessages;
    for i := 0 to backupfiles.Count - 1 do
      MakeBakFiles(backupfiles.Strings[i], 3);
    (*
    Result := True;
    backupfiles := TStringList.Create;
    // Del really old files
    FindAllFiles(backupfiles, mydir, '*.3', False);
    Application.ProcessMessages;
    for i := 0 to backupfiles.Count - 1 do
      if not DeleteFileUTF8(backupfiles.Strings[i]) then
        Result := False;
    Application.ProcessMessages;
    // backup old files
    for k := 2 downto 1 do
    begin
      FindAllFiles(backupfiles, mydir, '*.' + IntToStr(k), False);
      for i := 0 to backupfiles.Count - 1 do
      begin
        fname := backupfiles.Strings[i];
        fbakname := ExtractFileNameWithoutExt(fname) + '.' + IntToStr(k + 1);
        if not CopyFile(fname, fbakname, [cffOverwriteFile, cffPreserveTime]) then
          Result := False;
        Application.ProcessMessages;
      end;
      //Application.ProcessMessages;
    end;
    // backup last files
    FindAllFiles(backupfiles, mydir, '*', False);
    for i := 0 to backupfiles.Count - 1 do
    begin
      fname := backupfiles.Strings[i];
      fbakname := fname + '.1';
      if not CopyFile(fname, fbakname, [cffOverwriteFile, cffPreserveTime]) then
        Result := False;
      Application.ProcessMessages;
    end;
    *)
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
  Application.ProcessMessages;
  if not DeleteDirectory(clientpath, False) then
    Result := False;
  Application.ProcessMessages;
  if not DeleteDirectory(opsipath, False) then
    Result := False;
  Application.ProcessMessages;
end;

function createProductdirectory: boolean;
var
  goon: boolean;
  task: string;
begin
  prodpath := IncludeTrailingPathDelimiter(myconfiguration.workbench_Path) +
    aktProduct.productdata.productId;
  clientpath := prodpath + PathDelim + 'CLIENT_DATA';
  opsipath := prodpath + PathDelim + 'OPSI';
  goon := True;
  if DirectoryExists(prodpath) then
  begin
    if showgui then
    begin
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
        if showgui then
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
        if showgui then
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
        if showgui then
          MessageDlg('opsi-setup-detector', rsCouldNotCreateDirectoryWarning +
            opsipath, mtError, [mbOK], '');
        goon := False;
      end;
    end;
  end;
  Result := goon;
  Application.ProcessMessages;
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
  Application.ProcessMessages;
  if not (goon and createOpsiFiles) then
  begin
    Logdatei.log('createOpsiFiles failed', LLCritical);
    system.ExitCode := 1;
    goon := False;
  end
  else
    Logdatei.log('createOpsiFiles done', LLnotice);
  Application.ProcessMessages;
  if not (goon and createClientFiles) then
  begin
    Logdatei.log('createClientFiles failed', LLCritical);
    system.ExitCode := 1;
    goon := False;
  end
  else
    Logdatei.log('createClientFiles done', LLnotice);
  Application.ProcessMessages;
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


procedure callOpsiPackageBuilder;
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
  if resultForm1.RadioButtonPackageBuilder.Checked = True then
  begin
    OpsiBuilderProcess.ShowWindow := swoShowNormal;
  end
  else  // build
  if resultForm1.RadioButtonBuildPackage.Checked = True then
  begin
    //ParamStr := ParamStr + ' --quiet';
    paramlist.Add('--quiet');
    if resultForm1.radioBuildModebuildOnly.Checked = True then
    begin
      paramlist.Add('--build=rebuild');
      //ParamStr := ParamStr + ' ' + ' --build=rebuild';
    end;
    if resultForm1.radioBuildModebuildInstall.Checked = True then
    begin
      paramlist.Add('--build=rebuild');
      paramlist.Add('--install');
      //ParamStr := ParamStr + ' --build=rebuild --install';
    end;
    OpsiBuilderProcess.ShowWindow := swoMinimize;
  end;
  //buildCallparams.Add(ParamStr);

  LogDatei.log('Try to call opsi packagebuilder', LLnotice);
  //OpsiBuilderProcess.CommandLine := buildCallbinary + ParamStr;
  OpsiBuilderProcess.Executable := buildCallbinary;
  OpsiBuilderProcess.Parameters.Assign(paramlist);
  OpsiBuilderProcess.Options := OpsiBuilderProcess.Options + [poWaitOnExit];
  LogDatei.log('Call: ' + OpsiBuilderProcess.Executable + ' with: ' +
    OpsiBuilderProcess.Parameters.Text, LLInfo);
  LogDatei.log('Call: ' + OpsiBuilderProcess.CommandLine, LLInfo);
  // execute opsiPacketBuilder
  try
    OpsiBuilderProcess.Execute;
    if resultForm1.RadioButtonBuildPackage.Checked = True then
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
      LogDatei.log('Exception while calling ' + buildCallbinary +
        ' Message: ' + E.message, LLerror);
      if showgui then
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
      if showgui then
        ShowMessage(sErrOpsiPackageBuilderStart);
    end;
  end;
  logdatei.log('Here comes the OpsiPackageBuilder log', LLnotice);
  logdatei.includelogtail('c:\opsi.org\applog\opb-call.log', 50, 'utf8');
  logdatei.log('Finished callOpsiPackageBuilder', LLinfo);
end;   // execute OPSIPackageBuilder

procedure buildWithOpsiService;
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
    serviceresult := osdform.localservicedata.CheckAndRetrieveString(omc,
      errorOccured);
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
  resultForm1.PanelProcess.Visible := True;
  resultForm1.processStatement.Caption := 'invoke opsi service ...';
  procmess;
  //packagedir := '/var/lib/opsi/workbench/';
  // packagedir may be relative to workbench
  packagedir := '';
  packagedir := packagedir + aktProduct.productdata.productId;
  packagefile := packagedir + '/' + aktProduct.productdata.productId;
  packagefile := packagefile + '_' + aktProduct.productdata.productversion;
  packagefile := packagefile + '-' + IntToStr(aktProduct.productdata.packageversion);
  packagefile := packagefile + '.opsi';
  if resultForm1.radioBuildModebuildOnly.Checked = True then
  begin
    if servicecall('workbench_buildPackage', packagedir) then
      LogDatei.log('Package ' + packagefile + ' successful build', LLnotice)
    else
    begin
      LogDatei.log('Package ' + packagefile + ' failed to build', LLerror);
      system.ExitCode := 1;
    end;
  end;

  if resultForm1.radioBuildModebuildInstall.Checked = True then
  begin

    if servicecall('workbench_installPackage', packagedir) then
      LogDatei.log('Package ' + packagefile + ' successful build + installed', LLnotice)
    else
    begin
      LogDatei.log('Package ' + packagefile + ' failed to build +install', LLerror);
      system.ExitCode := 1;
    end;

  end;
  resultForm1.PanelProcess.Visible := False;
end;


procedure callServiceOrPackageBuilder;
var
  callOpB: boolean = False;
begin
  if startOpsiServiceConnection then
  begin
    if CompareDotSeparatedNumbers(osdform.opsiserviceversion, '<', '4.2.0.311') then
      callOpB := True;
  end
  else
    callOpB := True;

  if callOpB then callOpsiPackageBuilder
  else
    buildWithOpsiService;
end;

end.
