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
  dateutils;

procedure createProductStructure;
procedure callOpsiPackageBuilder;

resourcestring
  // new for 4.1.0.2 ******************************************************************
  rsDirectory = 'Directory ';
  rsStillExitsWarningDeleteOverwrite = ' still exits. Abort or Backup or Delete ?';
  rsCouldNotCreateDirectoryWarning = 'Could not create directory: ';
  rsConfirmBackupOrRemovalTitle = 'Confirm Delete or Backup of old files.';
  rsConfirmBackupCaption = 'Backup';
  rsConfirmAbortCaption = 'Abort';
  rsConfirmDeleteCaption = 'Delete';
  rsConfirmAbortHint = 'Leave existing directory unchanged and abort';
  rsConfirmDeleteHint = 'Delete the complete directory before creating the new one.';
  rsConfirmBackupHint = 'All files that are directly in the directories "CLIENT_DATA" and "OPSI" will copy to an backup file.';
  rsConfirmExpandButton = 'More Information ...';
  rsConfirmExpandedText = 'Abort: Leave existing directory unchanged and abort' +
    LineEnding + 'Delete: Delete the complete directory before creating the new one.' +
    LineEnding +
    'Backup: All files that are directly in the directories "CLIENT_DATA" and "OPSI" will copy to an backup file.';


implementation

uses
  osdform;

var
  patchlist: TStringList;
  myExeDir: string;
  prodpath, clientpath, opsipath: string;

procedure patchScript(infile, outfile: string);
var
  infileHandle, outfileHandle: Text;
  aktline: string;
  i, k: integer;
  aktindentstr: string;
  aktindentnum: integer;
  aktReplacestr: string;
begin
  mywrite('creating: ' + outfile + ' from: ' + infile);

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
    CloseFile(outfileHandle)
  except
    on E: EInOutError do
      logdatei.log('patchScript file error: ' + E.ClassName + '/' + E.Message, LLError);
  end;
end;


procedure fillPatchList;
var
  i: integer;
  str, str2, str3: string;
  strlist: TStringList;
  templatePath: string;
  proptmpstr: string;
begin
  {$IFDEF WINDOWS}
  templatePath := ExtractFileDir(Application.ExeName) + PathDelim + 'template-files';
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
  // development env
  templatePath := ExtractFileDir(Application.ExeName) + PathDelim + 'template-files';
  if not DirectoryExists(templatePath) then
  begin
    LogDatei.log('Template path: ' + templatePath + ' not found. Fallback to unix path',
      LLInfo);
    templatePath := '/usr/share/opsi-setup-detector/template-files';
  end;
  {$ENDIF LINUX}
  try
    strlist := TStringList.Create;
    patchlist.Clear;
    str := '';
    //ProductProperties
    for i := 0 to aktProduct.properties.Count - 1 do
    begin
      proptmpstr := aktProduct.properties.Items[i].Property_Name;
      if (proptmpstr = 'SecretLicense_or_Pool') and
        aktProduct.productdata.licenserequired then
        str := str + 'DefVar $LicenseOrPool$' + LineEnding
      else
        str := str + 'DefVar $' + proptmpstr + '$' + LineEnding;
    end;
    (*
    if myconfiguration.UsePropDesktopicon then
      str := str + 'DefVar $DesktopIcon$' + LineEnding;
    if myconfiguration.UsePropLicenseOrPool and
      aktProduct.productdata.licenserequired then
      str := str + 'DefVar $LicenseOrPool$' + LineEnding;
      *)
    patchlist.add('#@stringVars*#=' + str);

    str := '';
    patchlist.add('#@productId*#=' + aktProduct.productdata.productId);
    for i := 0 to myconfiguration.import_libraries.Count - 1 do
      str := str + 'importlib "' + myconfiguration.import_libraries[i] +
        '"' + LineEnding;
    patchlist.add('#@importLibs*#=' + str);
    patchlist.add('#@LicenseRequired*#=' +
      boolToStr(aktProduct.productdata.licenserequired, True));
    str := '';
    for i := 0 to aktProduct.properties.Count - 1 do
    begin
      proptmpstr := aktProduct.properties.Items[i].Property_Name;
      if (proptmpstr = 'SecretLicense_or_Pool') and
        aktProduct.productdata.licenserequired then
      begin
        str := str +
          'set $LicenseOrPool$ = GetConfidentialProductProperty("SecretLicense_or_Pool","'
          +
          aktProduct.properties.Items[i - 1].StrDefault[0] + '")' + LineEnding;
        str := str + 'set $LicensePool$ = $LicenseOrPool$' + LineEnding;
      end
      else
      begin
        { remove brackets [] }
        str2 := opsiunquotestr2(aktProduct.properties.Items[i].StrDefault[0], '[]');
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
      str := 'comment "Start Pre Install hook :"' + LineEnding +
        myconfiguration.preInstallLines.Text;
    patchlist.add('#@preInstallLines*#=' + str);

    str := myconfiguration.postInstallLines.Text;
    if str <> '' then
      str := 'comment "Start Post UnInstall hook :"' + LineEnding +
        myconfiguration.postInstallLines.Text;
    patchlist.add('#@postInstallLines*#=' + str);

    str := myconfiguration.preUninstallLines.Text;
    if str <> '' then
      str := 'comment "Start Pre UnInstall hook :"' + LineEnding +
        myconfiguration.preUninstallLines.Text;
    patchlist.add('#@preUninstallLines*#=' + str);

    str := myconfiguration.postUnInstallLines.Text;
    if str <> '' then
      str := 'comment "Start Post UnInstall hook :"' + LineEnding +
        myconfiguration.postUnInstallLines.Text;
    patchlist.add('#@postUninstallLines*#=' + str);

    str := '';
    if myconfiguration.UsePropDesktopicon then
    begin
      strlist.LoadFromFile(templatePath + Pathdelim + 'win' + Pathdelim +
        'SetupHandleDesktopIcon.opsiscript');
      str := 'comment "Start Desktop Icon Handling :"' + LineEnding + strlist.Text;
    end;
    patchlist.add('#@SetupHandleDesktopIcon*#=' + str);

    str := '';
    if myconfiguration.UsePropDesktopicon then
    begin
      //strlist.LoadFromFile(templatePath + Pathdelim + 'DelsubHandleDesktopIcon.opsiscript');
      str := 'comment "Start Desktop Icon Handling :"' + LineEnding +
        'Linkfolder_remove_desktop_icon';
    end;
    patchlist.add('#@DelsubHandleDesktopIcon*#=' + str);

    str := '';
    if myconfiguration.UsePropDesktopicon then
    begin
      strlist.LoadFromFile(templatePath + Pathdelim + 'win' + Pathdelim +
        'SetupDesktopIconSection.opsiscript');
      str := strlist.Text;
    end;
    patchlist.add('#@SetupDesktopiconSectionLines*#=' + str);

    str := '';
    if myconfiguration.UsePropDesktopicon then
    begin
      strlist.LoadFromFile(templatePath + Pathdelim + 'win' + Pathdelim +
        'DelsubDesktopIconSection.opsiscript');
      str := strlist.Text;
    end;
    patchlist.add('#@DelsubDesktopiconSectionLines*#=' + str);



    // loop over setups
    for i := 0 to 2 do
    begin
      if aktProduct.SetupFiles[i].active then
        //setup 1
        patchlist.add('#@install' + IntToStr(i + 1) + '*#=' +
          BoolToStr(aktProduct.SetupFiles[i].active, True));

      patchlist.add('#@MinimumSpace' + IntToStr(i + 1) + '*#=' + IntToStr(
        aktProduct.SetupFiles[i].requiredSpace) + ' MB');

      patchlist.add('#@InstallDir' + IntToStr(i + 1) + '*#=' +
        aktProduct.SetupFiles[i].installDirectory);

      patchlist.add('#@MsiId' + IntToStr(i + 1) + '*#=' + aktProduct.SetupFiles[i].msiId);

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

      patchlist.add('#@uninstallCommandLine' + IntToStr(i + 1) + '*#=' +
        aktProduct.SetupFiles[i].uninstallCommandLine);

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

    end;

  finally
    strlist.Free;
  end;
end;

function createClientFiles: boolean;
var
  infilename, outfilename, tmpname, tmpext, tmppref: string;
  insetup, indelsub, inuninstall: string;
  templatePath, genericTemplatePath, tempstr: string;
  infilelist: TStringList;
  i: integer;
begin
  Result := False;
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
    //templatePath := '/usr/local/share/opsi-setup-detector/template-files';
    templatePath :=  ExtractFileDir(Application.ExeName) + PathDelim
       + '../Resources/template-files';
  {$ENDIF DARWIN}

  genericTemplatePath := templatePath + Pathdelim + 'generic';
  if osWin in aktProduct.productdata.targetOSset then
    tempstr := 'win'
  else if osLin in aktProduct.productdata.targetOSset then
    tempstr := 'lin'
  else if osMac in aktProduct.productdata.targetOSset then
    tempstr := 'mac';
  if (osMulti in aktProduct.productdata.targetOSset) then
    tempstr := 'multi';

  templatePath := templatePath + Pathdelim + tempstr;

  try
    try
      patchlist := TStringList.Create;
      fillPatchList;
      infilelist := TStringList.Create;
      case osdsettings.runmode of
        singleAnalyzeCreate:
        begin
          infilelist.Add('setupsingle.opsiscript');
          infilelist.Add('delsubsingle.opsiscript');
          infilelist.Add('uninstallsingle.opsiscript');
        (*
        insetup := 'setupsingle.opsiscript';
        indelsub := 'delsubsingle.opsiscript';
        inuninstall := 'uninstallsingle.opsiscript';
        *)
        end;
        twoAnalyzeCreate_1, twoAnalyzeCreate_2:
        begin
          infilelist.Add('setupdouble.opsiscript');
          infilelist.Add('delsubdouble.opsiscript');
          infilelist.Add('uninstalldouble.opsiscript');
        (*
        insetup := 'setupdouble.opsiscript';
        indelsub := 'delsubdouble.opsiscript';
        inuninstall := 'uninstalldouble.opsiscript';
        *)
        end;
        createTemplate:
        begin
          infilelist.Add('setuptempl.opsiscript');
          infilelist.Add('delsubtempl.opsiscript');
          infilelist.Add('uninstalltempl.opsiscript');
        (*
        insetup := 'setuptempl.opsiscript';
        indelsub := 'delsubtempl.opsiscript';
        inuninstall := 'uninstalltempl.opsiscript';
        *)
        end;
        threeAnalyzeCreate_1, threeAnalyzeCreate_2, threeAnalyzeCreate_3:
        begin
          infilelist.Add('setupsingle.opsiscript');
          infilelist.Add('uninstallsingle.opsiscript');
          infilelist.Add('win_setupsingle.opsiscript');
          infilelist.Add('win_delsubsingle.opsiscript');
          infilelist.Add('win_uninstallsingle.opsiscript');
          infilelist.Add('lin_setupsingle.opsiscript');
          infilelist.Add('lin_delsubsingle.opsiscript');
          infilelist.Add('lin_uninstallsingle.opsiscript');
          infilelist.Add('mac_setupsingle.opsiscript');
          infilelist.Add('mac_delsubsingle.opsiscript');
          infilelist.Add('mac_uninstallsingle.opsiscript');
        end;
        createMultiTemplate:
        begin
          infilelist.Add('setuptempl.opsiscript');
          infilelist.Add('uninstalltempl.opsiscript');
          infilelist.Add('win_setuptempl.opsiscript');
          infilelist.Add('win_delsubtempl.opsiscript');
          infilelist.Add('win_uninstalltempl.opsiscript');
          infilelist.Add('lin_setuptempl.opsiscript');
          infilelist.Add('lin_delsubtempl.opsiscript');
          infilelist.Add('lin_uninstalltempl.opsiscript');
          infilelist.Add('mac_setuptempl.opsiscript');
          infilelist.Add('mac_delsubtempl.opsiscript');
          infilelist.Add('mac_uninstalltempl.opsiscript');
        end;
      end;
      for i := 0 to infilelist.Count - 1 do
      begin
        tmpname := ExtractFileNameOnly(infilelist.Strings[i]);
        tmpext := ExtractFileExt(infilelist.Strings[i]);
        tmppref := copy(infilelist.Strings[i], 1, 3);
        // replace non active setups with template
        if (tmppref = 'win') and (aktProduct.SetupFiles[0].active = False) then
          infilename := templatePath + Pathdelim +
            StringReplace(tmpname, 'single', 'templ', []) + tmpext
        else if (tmppref = 'lin') and (aktProduct.SetupFiles[1].active = False) then
          infilename := templatePath + Pathdelim +
            StringReplace(tmpname, 'single', 'templ', []) + tmpext
        else if (tmppref = 'mac') and (aktProduct.SetupFiles[2].active = False) then
          infilename := templatePath + Pathdelim +
            StringReplace(tmpname, 'single', 'templ', []) + tmpext
        else
          infilename := templatePath + Pathdelim + infilelist.Strings[i];
        tmpname := StringReplace(tmpname, 'single', '', []);
        tmpname := StringReplace(tmpname, 'double', '', []);
        tmpname := StringReplace(tmpname, 'templ', '', []);
        if tmpname = 'setup' then
          outfilename := clientpath + PathDelim + aktProduct.productdata.setupscript
        else if tmpname = 'delsub' then
          outfilename := clientpath + PathDelim + aktProduct.productdata.delsubscript
        else if tmpname = 'uninstall' then
          outfilename := clientpath + PathDelim + aktProduct.productdata.uninstallscript
        else
          outfilename := clientpath + PathDelim + tmpname + tmpext;
        patchScript(infilename, outfilename);
      end;
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

    // No need to copy installer for templates
    if not(osdsettings.runmode in [createTemplate,createMultiTemplate]) then
      // loop over setups
      for i := 0 to 2 do
      begin
        infilename := aktProduct.SetupFiles[i].setupFullFileName;
        LogDatei.log('Will copy: ' + infilename + ' to: ' + clientpath +
          PathDelim + 'files' + IntToStr(i + 1), LLNotice);
        if aktProduct.SetupFiles[i].active then
          // complete dir
          if aktProduct.SetupFiles[i].copyCompleteDir then
          begin
            if not CopyDirTree(ExtractFileDir(infilename),
              clientpath + PathDelim + 'files' + IntToStr(i + 1),
              [cffOverwriteFile, cffCreateDestDirectory, cffPreserveTime]) then
              LogDatei.log('Failed to copy: ' + infilename, LLError);
          end
          // macos .app dir
          else if DirectoryExistsUTF8(infilename) and
            (LowerCase(ExtractFileExt(infilename)) = '.app') then
          begin
            if not CopyDirTree(infilename, clientpath + PathDelim +
              'files' + IntToStr(i + 1) + PathDelim + ExtractFileName(infilename),
              [cffOverwriteFile, cffCreateDestDirectory, cffPreserveTime]) then
              LogDatei.log('Failed to copy: ' + infilename, LLError);
          end
          else
          begin
            // setup file
            if FileExists(infilename) then
              if copyfile(infilename, clientpath + PathDelim +
                'files' + IntToStr(i + 1) + PathDelim +
                aktProduct.SetupFiles[i].setupFileName,
                [cffOverwriteFile, cffCreateDestDirectory, cffPreserveTime], True) then
                LogDatei.log('Failed to copy: ' + infilename, LLError);
            // MST file
            if FileExists(aktProduct.SetupFiles[i].MSTFullFileName) then
              if copyfile(aktProduct.SetupFiles[i].MSTFullFileName,
                clientpath + PathDelim + 'files' + IntToStr(i + 1) +
                PathDelim + aktProduct.SetupFiles[i].MSTFileName,
                [cffOverwriteFile, cffCreateDestDirectory, cffPreserveTime], True) then
                LogDatei.log('Failed to copy: ' +
                  aktProduct.SetupFiles[i].MSTFullFileName, LLError);
          end;
      end;

      //osd-lib.opsiscript
      infilename := genericTemplatePath + Pathdelim + 'osd-lib.opsiscript';
      outfilename := clientpath + PathDelim + 'osd-lib.opsiscript';
      copyfile(infilename, outfilename, [cffOverwriteFile, cffCreateDestDirectory,
        cffPreserveTime], True);

      // install lib
      if osMac in aktProduct.productdata.targetOSset then
      begin
        infilename := genericTemplatePath + Pathdelim + 'uib_macosinstalllib.opsiscript';
        outfilename := clientpath + PathDelim + 'uib_macosinstalllib.opsiscript';
        copyfile(infilename, outfilename, [cffOverwriteFile,
          cffCreateDestDirectory, cffPreserveTime], True);
      end;
      if (osLin in aktProduct.productdata.targetOSset) or
        (osMulti in aktProduct.productdata.targetOSset) then
      begin
        infilename := genericTemplatePath + Pathdelim + 'uib_lin_install.opsiscript';
        outfilename := clientpath + PathDelim + 'uib_lin_install.opsiscript';
        copyfile(infilename, outfilename, [cffOverwriteFile,
          cffCreateDestDirectory, cffPreserveTime], True);
      end;
      if fileexists(genericTemplatePath + Pathdelim + 'uib_exitcode.opsiscript') then
      begin
        infilename := genericTemplatePath + Pathdelim + 'uib_exitcode.opsiscript';
        outfilename := clientpath + PathDelim + 'uib_exitcode.opsiscript';
        copyfile(infilename, outfilename, [cffOverwriteFile,
          cffCreateDestDirectory, cffPreserveTime], True);
      end;

      //product png
      //infilename := templatePath + Pathdelim + 'template.png';
      infilename := aktProduct.productdata.productImageFullFileName;
      outfilename := clientpath + PathDelim + aktProduct.productdata.productId +
        ExtractFileExt(aktProduct.productdata.productImageFullFileName);
      copyfile(infilename, outfilename, [cffOverwriteFile, cffCreateDestDirectory,
        cffPreserveTime], True);

      //preinst
      infilename := genericTemplatePath + Pathdelim + 'preinst';
      outfilename := opsipath + pathdelim + 'preinst';
      copyfile(infilename, outfilename, [cffOverwriteFile, cffCreateDestDirectory,
        cffPreserveTime], True);
      //postinst
      infilename := genericTemplatePath + Pathdelim + 'postinst';
      outfilename := opsipath + pathdelim + 'postinst';
      copyfile(infilename, outfilename, [cffOverwriteFile, cffCreateDestDirectory,
        cffPreserveTime], True);

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
  textlist: TStringList;
  i: integer;
  mydep: TPDependency;
  myprop: TPProperty;
  tmpstr: string;
  utcoffset: integer;
  utcoffsetstr: string;
begin
  Result := False;
  try
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
    textlist.Add('uninstallScript: ' + aktProduct.productdata.uninstallscript);
    textlist.Add('updateScript: ');
    textlist.Add('alwaysScript: ');
    textlist.Add('onceScript: ');
    textlist.Add('customScript: ');
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
        textlist.Add('values: ' + myprop.Strvalues[0]);
        textlist.Add('default: ' + myprop.StrDefault[0]);
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
  prodpath := myconfiguration.workbench_Path + PathDelim +
    aktProduct.productdata.productId;
  clientpath := prodpath + PathDelim + 'CLIENT_DATA';
  opsipath := prodpath + PathDelim + 'OPSI';
  goon := True;
  if DirectoryExists(prodpath) then
    // https://specials.rejbrand.se/TTaskDialog/
    with TTaskDialog.Create(resultForm1) do
      try
        Title := rsConfirmBackupOrRemovalTitle;
        Caption := 'opsi-setup-detector';
        Text := rsDirectory + prodpath + rsStillExitsWarningDeleteOverwrite;
        CommonButtons := [];
        with TTaskDialogButtonItem(Buttons.Add) do
        begin
          Caption := rsConfirmAbortCaption;
          //CommandLinkHint := rsConfirmAbortHint;
          ModalResult := mrAbort;
        end;
        with TTaskDialogButtonItem(Buttons.Add) do
        begin
          Caption := rsConfirmDeleteCaption;
          //CommandLinkHint := rsConfirmDeleteHint;
          ModalResult := mrYes;
        end;
        with TTaskDialogButtonItem(Buttons.Add) do
        begin
          Caption := rsConfirmBackupCaption;
          //CommandLinkHint := rsConfirmBackupHint;
          ModalResult := mrNo;
        end;
        MainIcon := tdiQuestion;
        //include(Flags,[tfExpandFooterArea]);
        Flags := [tfUseCommandLinks, tfAllowDialogCancellation,tfExpandFooterArea];
        ExpandButtonCaption := rsConfirmExpandButton;
        ExpandedText := rsConfirmExpandedText;
        if Execute then
        begin
          if ModalResult = mrYes then
            task := 'del';
          if ModalResult = mrNo then
            task := 'bak';
          if ModalResult = mrAbort then
            task := 'abort';
        end;
      finally
        Free;
      end;
  if task = 'abort' then
    goon := False;
  if task = 'del' then
    if not delOldProductDir then
    begin
      LogDatei.log('Could not recursive delete dir: ' + prodpath, LLCritical);
      goon := False;
    end;
  if task = 'bak' then
    if not bakupOldProductDir then
    begin
      LogDatei.log('Could not inernally backup dir: ' + prodpath, LLCritical);
      goon := False;
    end;
  if goon then
  begin
    if not DirectoryExists(prodpath) then
    begin
      if not ForceDirectories(prodpath) then
      begin
        Logdatei.log('Could not create directory: ' + prodpath, LLCritical);
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
        MessageDlg('opsi-setup-detector', rsCouldNotCreateDirectoryWarning +
          opsipath, mtError, [mbOK], '');
        goon := False;
      end;
    end;
  end;
  Result := goon;
  Application.ProcessMessages;
end;


procedure createProductStructure;
var
  prodpath, clientpath, opsipath: string;
  goon: boolean;
begin
  goon := True;
  if not createProductdirectory then
  begin
    Logdatei.log('createProductdirectory failed', LLCritical);
    goon := False;
  end
  else
    Logdatei.log('createProductdirectory done', LLnotice);
  Application.ProcessMessages;
  if not (goon and createOpsiFiles) then
  begin
    Logdatei.log('createOpsiFiles failed', LLCritical);
    goon := False;
  end
  else
    Logdatei.log('createOpsiFiles done', LLnotice);
  Application.ProcessMessages;
  if not (goon and createClientFiles) then
  begin
    Logdatei.log('createClientFiles failed', LLCritical);
    goon := False;
  end
  else
    Logdatei.log('createClientFiles done', LLnotice);
  Application.ProcessMessages;
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

begin
  logdatei.log('Start callOpsiPackageBuilder', LLDebug2);
  buildCallparams := TStringList.Create;
  OpsiBuilderProcess := process.TProcess.Create(nil);
  buildCallbinary := '"' + myconfiguration.PathToOpsiPackageBuilder + '"';
  ParamStr := ' -p=' + myconfiguration.workbench_Path;
  if AnsiLastChar(ParamStr) <> DirectorySeparator then
    ParamStr := ParamStr + DirectorySeparator;
  ParamStr := ParamStr + aktProduct.productdata.productId;
  if resultForm1.RadioButtonPackageBuilder.Checked = True then
  begin
    OpsiBuilderProcess.ShowWindow := swoShowNormal;
  end
  else  // build
  if resultForm1.RadioButtonBuildPackage.Checked = True then
  begin
    ParamStr := ParamStr + ' --quiet';
    if resultForm1.radioBuildModebuildOnly.Checked = True then
    begin
      ParamStr := ParamStr + ' ' + '--build=rebuild';
    end;
    if resultForm1.radioBuildModebuildInstall.Checked = True then
    begin
      ParamStr := ParamStr + '--build=rebuild --install';
    end;
    OpsiBuilderProcess.ShowWindow := swoMinimize;
  end;
  buildCallparams.Add(ParamStr);

  LogDatei.log('Try to call opsi packagebuilder', LLnotice);
  OpsiBuilderProcess.CommandLine := buildCallbinary + ParamStr;
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
      ShowMessage(sErrOpsiPackageBuilderStart);
    end;
  end;

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
      ShowMessage(sErrOpsiPackageBuilderStart);
    end;
  end;
  logdatei.log('Finished callOpsiPackageBuilder', LLDebug2);
end;   // execute OPSIPackageBuilder




end.
