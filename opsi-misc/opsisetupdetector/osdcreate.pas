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
  dateutils;

procedure createProductStructure;
procedure callOpsiPackageBuilder;

resourcestring
  // new for 4.1.0.2 ******************************************************************
  rsDirectory = 'Directory ';
  rsStillExitsWarningDeleteOverwrite = ' still exits. Overwrite / Delete ?';
  rsCouldNotCreateDirectoryWarning = 'Could not create directory: ';


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
  aktidentstr: string;
  aktidentnum: integer;
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
        if pos(patchlist.Names[i], aktline) > 0 then
        begin
          // we have to preserve the ident
          // check actual ident
          aktidentnum := length(aktline) - length(trimleft(aktline));
          aktidentstr := '';
          // ident with tabs
          for k := 1 to aktidentnum do
            aktidentstr := aktidentstr + char(9);
          // p
          aktReplacestr := patchlist.ValueFromIndex[i];
          // patch aktidentstr after each Newline
          aktReplacestr := StringReplace(aktReplacestr, Lineending,
            Lineending + aktidentstr, [rfReplaceAll, rfIgnoreCase]);
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
  str: string;
  strlist: TStringList;
  templatePath: string;
begin
     {$IFDEF WINDOWS}
  templatePath := ExtractFileDir(Application.ExeName) + PathDelim + 'template-files';
    {$ENDIF WINDOWS}
    {$IFDEF LINUX}
  templatePath := '/usr/share/opsi-setup-detector/template-files';
    {$ENDIF LINUX}
  try
    strlist := TStringList.Create;
    patchlist.Clear;
    str := '';
    if myconfiguration.UsePropDesktopicon then
      str := str + 'DefVar $DesktopIcon$' + LineEnding;
    if myconfiguration.UsePropLicenseOrPool and aktProduct.productdata.licenserequired then
      str := str + 'DefVar $LicenseOrPool$' + LineEnding;
    patchlist.add('#@stringVars*#=' + str);

    str := '';
    patchlist.add('#@productId*#=' + aktProduct.productdata.productId);
    for i := 0 to myconfiguration.import_libraries.Count - 1 do
      str := str + 'importlib "' + myconfiguration.import_libraries[i] + '"' + LineEnding;
    patchlist.add('#@importLibs*#=' + str);
    patchlist.add('#@LicenseRequired*#=' +
      boolToStr(aktProduct.productdata.licenserequired, True));
    str := '';
    if myconfiguration.UsePropDesktopicon then
      str := str + 'set $DesktopIcon$ = GetProductProperty("DesktopIcon","false")' +
        LineEnding;
    if myconfiguration.UsePropLicenseOrPool and aktProduct.productdata.licenserequired then
    begin
      str := str + 'set $LicenseOrPool$ = GetConfidentialProductProperty("SecretLicense_or_Pool","")' +
        LineEnding;
      str := str + 'set $LicensePool$ = $LicenseOrPool$' +  LineEnding;
    end;
    patchlist.add('#@GetProductProperty*#=' + str);
    //setup 1
    patchlist.add('#@MinimumSpace1*#=' + IntToStr(
      aktProduct.SetupFiles[0].requiredSpace) + ' MB');
     patchlist.add('#@InstallDir1*#=' + aktProduct.SetupFiles[0].installDirectory);
    patchlist.add('#@MsiId1*#=' + aktProduct.SetupFiles[0].msiId);
    str := myconfiguration.preInstallLines.Text;
    if str <> '' then
      str := 'comment "Start Pre Install hook :"' + LineEnding +
        myconfiguration.preInstallLines.Text;
    patchlist.add('#@preInstallLines1*#=' + str);
    patchlist.add('#@installCommandLine1*#=' + aktProduct.SetupFiles[
      0].installCommandLine);
    str := aktProduct.SetupFiles[0].install_waitforprocess;
    if str <> '' then
      str := '/WaitForProcessEnding "' +
        aktProduct.SetupFiles[0].install_waitforprocess + '" /TimeOutSeconds 20';
    patchlist.add('#@installWaitForProc1*#=' + str);
    str := myconfiguration.postInstallLines.Text;
    if str <> '' then
      str := 'comment "Start Post UnInstall hook :"' + LineEnding +
        myconfiguration.postInstallLines.Text;
    patchlist.add('#@postInstallLines1*#=' + str);
    patchlist.add('#@isExitcodeFatalFunction1*#=' +
      aktProduct.SetupFiles[0].isExitcodeFatalFunction);
    str := aktProduct.SetupFiles[0].uninstallCheck.Text;
    patchlist.add('#@uninstallCheckLines1*#=' + str);
    str := myconfiguration.preUninstallLines.Text;
    if str <> '' then
      str := 'comment "Start Pre UnInstall hook :"' + LineEnding +
        myconfiguration.preUninstallLines.Text;
    patchlist.add('#@preUninstallLines1*#=' + str);
    patchlist.add('#@uninstallCommandLine1*#=' +
      aktProduct.SetupFiles[0].uninstallCommandLine);
    patchlist.add('#@uninstallProg1*#=' + aktProduct.SetupFiles[0].uninstallProg);
    str := aktProduct.SetupFiles[0].uninstall_waitforprocess;
    if str <> '' then
      str := '/WaitForProcessEnding "' +
        aktProduct.SetupFiles[0].uninstall_waitforprocess + '" /TimeOutSeconds 20';
    patchlist.add('#@uninstallWaitForProc1*#=' + str);
    str := myconfiguration.postUnInstallLines.Text;
    if str <> '' then
      str := 'comment "Start Post UnInstall hook :"' + LineEnding +
        myconfiguration.postUnInstallLines.Text;
    patchlist.add('#@postUninstallLines1*#=' + str);
    //setup 2
    patchlist.add('#@MinimumSpace2*#=' + IntToStr(
      aktProduct.SetupFiles[1].requiredSpace) + ' MB');
    patchlist.add('#@InstallDir2*#=' + aktProduct.SetupFiles[1].installDirectory);
    patchlist.add('#@MsiId2*#=' + aktProduct.SetupFiles[1].msiId);
    str := myconfiguration.preInstallLines.Text;
    if str <> '' then
      str := 'comment "Start Pre Install hook :"' + LineEnding +
        myconfiguration.preInstallLines.Text;
    patchlist.add('#@preInstallLines2*#=' + str);
    patchlist.add('#@installCommandLine2*#=' + aktProduct.SetupFiles[
      1].installCommandLine);
    str := aktProduct.SetupFiles[1].install_waitforprocess;
    if str <> '' then
      str := '/WaitForProcessEnding "' +
        aktProduct.SetupFiles[1].install_waitforprocess + '" /TimeOutSeconds 20';
    patchlist.add('#@installWaitForProc2*#=' + str);
    str := myconfiguration.postInstallLines.Text;
    if str <> '' then
      str := 'comment "Start Post Install hook :"' + LineEnding +
        myconfiguration.postInstallLines.Text;
    patchlist.add('#@postInstallLines2*#=' + str);
    patchlist.add('#@isExitcodeFatalFunction2*#=' +
      aktProduct.SetupFiles[1].isExitcodeFatalFunction);
    str := aktProduct.SetupFiles[1].uninstallCheck.Text;
    patchlist.add('#@uninstallCheckLines2*#=' + str);
    str := myconfiguration.preUninstallLines.Text;
    if str <> '' then
      str := 'comment "Start Pre Install hook :"' + LineEnding +
        myconfiguration.preUninstallLines.Text;
    patchlist.add('#@preUninstallLines2*#=' + str);
    patchlist.add('#@uninstallCommandLine2*#=' +
      aktProduct.SetupFiles[1].uninstallCommandLine);
    patchlist.add('#@uninstallProg2*#=' + aktProduct.SetupFiles[1].uninstallProg);
    str := aktProduct.SetupFiles[1].uninstall_waitforprocess;
    if str <> '' then
      str := '/WaitForProcessEnding "' +
        aktProduct.SetupFiles[1].uninstall_waitforprocess + '" /TimeOutSeconds 20';
    patchlist.add('#@uninstallWaitForProc2*#=' + str);
    str := myconfiguration.postUninstallLines.Text;
    if str <> '' then
      str := 'comment "Start Post UnInstall hook :"' + LineEnding +
        myconfiguration.postUninstallLines.Text;
    patchlist.add('#@postUninstallLines2*#=' + str);

    str := '';
    if myconfiguration.UsePropDesktopicon then
    begin
      strlist.LoadFromFile(templatePath + Pathdelim + 'SetupHandleDesktopIcon.opsiscript');
      str := 'comment "Start Desktop Icon Handling :"' + LineEnding + strlist.Text;
    end;
    patchlist.add('#@SetupHandleDesktopIcon*#=' + str);

    str := '';
    if myconfiguration.UsePropDesktopicon then
    begin
      //strlist.LoadFromFile(templatePath + Pathdelim + 'DelsubHandleDesktopIcon.opsiscript');
      str := 'comment "Start Desktop Icon Handling :"' + LineEnding + 'Linkfolder_remove_desktop_icon';
    end;
    patchlist.add('#@DelsubHandleDesktopIcon*#=' + str);

    str := '';
    if myconfiguration.UsePropDesktopicon then
    begin
      strlist.LoadFromFile(templatePath + Pathdelim + 'SetupDesktopIconSection.opsiscript');
      str := strlist.Text;
    end;
    patchlist.add('#@SetupSectionLines*#=' + str);
    str := '';
    if myconfiguration.UsePropDesktopicon then
    begin
      strlist.LoadFromFile(templatePath + Pathdelim + 'DelsubDesktopIconSection.opsiscript');
      str := strlist.Text;
    end;
    patchlist.add('#@DelsubSectionLines*#=' + str);
  finally
    strlist.Free;
  end;
end;

function createClientFiles: boolean;
var
  infilename, outfilename: string;
  insetup, indelsub, inuninstall: string;
  templatePath: string;
begin
  Result := False;
    {$IFDEF WINDOWS}
  templatePath := ExtractFileDir(Application.ExeName) + PathDelim + 'template-files';
    {$ENDIF WINDOWS}
    {$IFDEF LINUX}
  templatePath := '/usr/share/opsi-setup-detector/template-files';
    {$ENDIF LINUX}
  try
    patchlist := TStringList.Create;
    fillPatchList;
    case useRunMode of
      singleAnalyzeCreate:
      begin
        insetup := 'setupsingle.opsiscript';
        indelsub := 'delsubsingle.opsiscript';
        inuninstall := 'uninstallsingle.opsiscript';
      end;
      twoAnalyzeCreate_1, twoAnalyzeCreate_2:
      begin
        insetup := 'setupdouble.opsiscript';
        indelsub := 'delsubdouble.opsiscript';
        inuninstall := 'uninstalldouble.opsiscript';
      end;
      createTemplate:
      begin
        insetup := 'setuptempl.opsiscript';
        indelsub := 'delsubtempl.opsiscript';
        inuninstall := 'uninstalltempl.opsiscript';
      end;
    end;
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
    patchScript(infilename, outfilename);
    // setup file 1
    if FileExists(aktProduct.SetupFiles[0].setupFullFileName) then
      copyfile(aktProduct.SetupFiles[0].setupFullFileName,
        clientpath + PathDelim + 'files1' + PathDelim +
        aktProduct.SetupFiles[0].setupFileName,
        [cffOverwriteFile, cffCreateDestDirectory, cffPreserveTime], True);
    // setup file 2
    if FileExists(aktProduct.SetupFiles[1].setupFullFileName) then
      copyfile(aktProduct.SetupFiles[1].setupFullFileName,
        clientpath + PathDelim + 'files2' + PathDelim +
        aktProduct.SetupFiles[1].setupFileName,
        [cffOverwriteFile, cffCreateDestDirectory, cffPreserveTime], True);

    //osd-lib.opsiscript
    infilename := templatePath + Pathdelim + 'osd-lib.opsiscript';
    outfilename := clientpath + PathDelim + 'osd-lib.opsiscript';
    copyfile(infilename, outfilename, [cffOverwriteFile, cffCreateDestDirectory,
      cffPreserveTime], True);
    //product png
    infilename := templatePath + Pathdelim + 'template.png';
    outfilename := clientpath + PathDelim + aktProduct.productdata.productId + '.png';
    copyfile(infilename, outfilename, [cffOverwriteFile, cffCreateDestDirectory,
      cffPreserveTime], True);
    //preinst
    infilename := templatePath + Pathdelim + 'preinst';
    outfilename := opsipath + pathdelim + 'preinst';
    copyfile(infilename, outfilename, [cffOverwriteFile, cffCreateDestDirectory,
      cffPreserveTime], True);
    //postinst
    infilename := templatePath + Pathdelim + 'postinst';
    outfilename := opsipath + pathdelim + 'postinst';
    copyfile(infilename, outfilename, [cffOverwriteFile, cffCreateDestDirectory,
      cffPreserveTime], True);


    FreeAndNil(patchlist);
    Result := True;
    ;
  except
    on E: Exception do
    begin
      logdatei.log('createClientFiles file error: ' + E.ClassName +
        '/' + E.Message, LLError);
    end;
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
      textlist.Add('requiredProduct: ' + mydep.requProductId);
      case mydep.requState of
        noState: ;
        installed: textlist.Add('requiredStatus: installed');
        not_installed: textlist.Add('requiredStatus: not installed');
        unknown: textlist.Add('requiredStatus: unknown');
      end;
      case mydep.requAction of
        noRequest: ;
        setup: textlist.Add('requiredAction: setup');
        uninstall: textlist.Add('requiredAction: uninstall');
        TPActionRequest.update: textlist.Add('requiredAction: update');
      end;
      case mydep.RequType of
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
      case myprop.ptype of
        bool: textlist.Add('type: bool');
        unicode: textlist.Add('type: unicode');
      end;
      textlist.Add('name: ' + myprop.Name);
      textlist.Add('description: ' + myprop.description);
      if myprop.ptype = bool then
      begin
        textlist.Add('default: ' + BoolToStr(myprop.boolDefault, True));
      end
      else
      begin
        textlist.Add('multivalue: ' + BoolToStr(myprop.multivalue, True));
        textlist.Add('editable: ' + BoolToStr(myprop.editable, True));
        textlist.Add('values: ' + myprop.Strvalues.Text);
        textlist.Add('default: ' + myprop.StrDefault.Text);
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
    textlist.Add('  * initial by opsi-setup-detector - Version: '+myVersion);
    textlist.Add('');
    textlist.Add('-- ' + myconfiguration.fullName + ' <' +
      myconfiguration.email_address + '> ' + FormatDateTime(
      'ddd, dd mmm yyyy hh:nn:ss', LocalTimeToUniversal(now)) + ' ' + utcoffsetstr);
    //mon, 04 Jun 12:00:00 + 0100
    textlist.SaveToFile(opsipath + pathdelim + 'changelog.txt');

    // readme.txt
    if (myconfiguration.Readme_txt_templ <> '')
      and FileExists(myconfiguration.Readme_txt_templ) then
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
      textlist.Add(aktProduct.productdata.productId + ' (' + tmpstr +
        ')');
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

function createProductdirectory: boolean;
var
  goon: boolean;
begin
  prodpath := myconfiguration.workbench_Path + PathDelim + aktProduct.productdata.productId;
  clientpath := prodpath + PathDelim + 'CLIENT_DATA';
  opsipath := prodpath + PathDelim + 'OPSI';
  goon := True;
  if DirectoryExists(prodpath) then
    if not MessageDlg('opsi-setup-detector', rsDirectory + prodpath +
      rsStillExitsWarningDeleteOverwrite, mtWarning, mbOKCancel, '') = mrOk then
      goon := False
    else
    if not DeleteDirectory(prodpath, False) then
    begin
      LogDatei.log('Could not recursive delete dir: ' + prodpath, LLCritical);
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
  end;
  if not (goon and createOpsiFiles) then
  begin
    Logdatei.log('createOpsiFiles failed', LLCritical);
    goon := False;
  end;
  if not (goon and createClientFiles) then
  begin
    Logdatei.log('createClientFiles failed', LLCritical);
    goon := False;
  end;
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
