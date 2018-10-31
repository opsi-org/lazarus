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
//procedure removeOtherTypeSpecificSections(setupType, setupFile: string);

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
  i: integer;
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
        aktline := StringReplace(aktline, patchlist.Names[i],
          patchlist.ValueFromIndex[i], [rfReplaceAll, rfIgnoreCase]);
      writeln(outfileHandle, aktline);
    end;
    CloseFile(infileHandle);
    CloseFile(outfileHandle)
  except
    on E: EInOutError do
      logdatei.log('patchScript file error: ' + E.ClassName + '/' + E.Message,LLError);
  end;
end;


  procedure fillPatchList;
  var
      i : integer;
      str : string;
  begin
    patchlist.Clear;
    str := '';
    patchlist.add('#@productId*#='+aktProduct.productdata.productId);
    for i := 0 to myconfiguration.import_libraries.Count-1 do
      str := str + 'importlib "'+myconfiguration.import_libraries[i]+'"'+LineEnding;
    patchlist.add('#@importLibs*#='+str);
    patchlist.add('#@LicenseRequired*#='+ boolToStr(aktProduct.productdata.licenserequired,true));
    patchlist.add('#@MinimumSpace*#='+inttostr(aktProduct.SetupFiles[0].requiredSpace)+' MB');
    //setup 1
    patchlist.add('#@InstallDir1*#='+aktProduct.SetupFiles[0].installDirectory);
    patchlist.add('#@MsiId1*#='+aktProduct.SetupFiles[0].msiId);
    str :='comment "Start Pre Install hook :"'+LineEnding+ myconfiguration.preInstallLines.Text;
    patchlist.add('#@preInstallLines1*#='+str);
    patchlist.add('#@installCommandLine1*#='+aktProduct.SetupFiles[0].installCommandLine);
    str := 'comment "Start Post UnInstall hook :"'+LineEnding+myconfiguration.postInstallLines.Text;
    patchlist.add('#@postInstallLines1*#='+str);
    patchlist.add('#@isExitcodeFatalFunction1*#='+aktProduct.SetupFiles[0].isExitcodeFatalFunction);
    str := aktProduct.SetupFiles[0].uninstallCheck.Text;
    patchlist.add('#@uninstallCheckLines1*#='+str);
    str :='comment "Start Pre UnInstall hook :"'+LineEnding+ myconfiguration.preUninstallLines.Text;
    patchlist.add('#@preUninstallLines1*#='+str);
    patchlist.add('#@uninstallCommandLine1*#='+aktProduct.SetupFiles[0].uninstallCommandLine);
    patchlist.add('#@uninstallProg1*#='+aktProduct.SetupFiles[0].uninstallProg);
    patchlist.add('#@uninstallWaitForProc1*#='+aktProduct.SetupFiles[0].uninstall_waitforprocess);
    str := 'comment "Start Post UnInstall hook :"'+LineEnding+myconfiguration.postUnInstallLines.Text;
    patchlist.add('#@postUninstallLines1*#='+str);
    //setup 2
    patchlist.add('#@InstallDir2*#='+aktProduct.SetupFiles[1].installDirectory);
    patchlist.add('#@MsiId2*#='+aktProduct.SetupFiles[1].msiId);
    str :='comment "Start Pre Install hook :"'+LineEnding+ myconfiguration.preInstallLines.Text;
    patchlist.add('#@preInstallLines2*#='+str);
    patchlist.add('#@installCommandLine2*#='+aktProduct.SetupFiles[1].installCommandLine);
    str := 'comment "Start Post Install hook :"'+LineEnding+myconfiguration.postInstallLines.Text;
    patchlist.add('#@postInstallLines2*#='+str);
    patchlist.add('#@isExitcodeFatalFunction2*#='+aktProduct.SetupFiles[1].isExitcodeFatalFunction);
    str := aktProduct.SetupFiles[1].uninstallCheck.Text;
    patchlist.add('#@uninstallCheckLines2*#='+str);
    str :='comment "Start Pre Install hook :"'+LineEnding+ myconfiguration.preUninstallLines.Text;
    patchlist.add('#@preUninstallLines2*#='+str);
    patchlist.add('#@uninstallCommandLine2*#='+aktProduct.SetupFiles[1].uninstallCommandLine);
    patchlist.add('#@uninstallProg2*#='+aktProduct.SetupFiles[1].uninstallProg);
    patchlist.add('#@uninstallWaitForProc2*#='+aktProduct.SetupFiles[1].uninstall_waitforprocess);
    str := 'comment "Start Post UnInstall hook :"'+LineEnding+myconfiguration.postUninstallLines.Text;
    patchlist.add('#@postUninstallLines2*#='+str);
  end;

  function createClientFiles : boolean;
  var
      infilename, outfilename : string;
      insetup,indelsub,inuninstall : string;
  begin
    result := false;
    try
      patchlist:= TStringList.Create;
      fillPatchList;
      case useRunMode of
        singleAnalyzeCreate :
          begin
            insetup := 'setupsingle.opsiscript';
            indelsub := 'delsubsingle.opsiscript';
            inuninstall := 'uninstallsingle.opsiscript';
          end;
        twoAnalyzeCreate_1, twoAnalyzeCreate_2 :
          begin
            insetup := 'setupdouble.opsiscript';
            indelsub := 'delsubdouble.opsiscript';
            inuninstall := 'uninstalldouble.opsiscript';
          end;
        createTemplate :
          begin
            insetup := 'setuptempl.opsiscript';
            indelsub := 'delsubtempl.opsiscript';
            inuninstall := 'uninstalltempl.opsiscript';
          end;
      end;
      // setup script
      infilename :=ExtractFileDir(Application.ExeName)+PathDelim+'template-files'+Pathdelim+insetup;
      outfilename := clientpath+PathDelim+aktProduct.productdata.setupscript;
      patchScript(infilename,outfilename);
      // delsub script
      infilename :=ExtractFileDir(Application.ExeName)+PathDelim+'template-files'+Pathdelim+indelsub;
      outfilename := clientpath+PathDelim+aktProduct.productdata.delsubscript;
      patchScript(infilename,outfilename);
      // uninstall script
      infilename :=ExtractFileDir(Application.ExeName)+PathDelim+'template-files'+Pathdelim+inuninstall;
      outfilename := clientpath+PathDelim+aktProduct.productdata.uninstallscript;
      patchScript(infilename,outfilename);
      // setup file 1
      if FileExists(aktProduct.SetupFiles[0].setupFullFileName) then
        copyfile(aktProduct.SetupFiles[0].setupFullFileName,
                clientpath+PathDelim+aktProduct.SetupFiles[0].setupFileName,
                [cffOverwriteFile,cffCreateDestDirectory,cffPreserveTime], true);
      // setup file 2
      if FileExists(aktProduct.SetupFiles[1].setupFullFileName) then
        copyfile(aktProduct.SetupFiles[1].setupFullFileName,
                clientpath+PathDelim+aktProduct.SetupFiles[1].setupFileName,
                [cffOverwriteFile,cffCreateDestDirectory,cffPreserveTime], true);

      //osd-lib.opsiscript
      infilename :=ExtractFileDir(Application.ExeName)+PathDelim+'template-files'+Pathdelim+'osd-lib.opsiscript';
      outfilename := clientpath+PathDelim+'osd-lib.opsiscript';
      copyfile(infilename,outfilename,[cffOverwriteFile,cffCreateDestDirectory,cffPreserveTime], true);
      //product png
      infilename :=ExtractFileDir(Application.ExeName)+PathDelim+'template-files'+Pathdelim+'template.png';
      outfilename := clientpath+PathDelim+aktProduct.productdata.productId+'.png';
      copyfile(infilename,outfilename,[cffOverwriteFile,cffCreateDestDirectory,cffPreserveTime], true);
      //preinst
      infilename :=ExtractFileDir(Application.ExeName)+PathDelim+'template-files'+Pathdelim+'preinst';
      outfilename := opsipath+pathdelim+'preinst';
      copyfile(infilename,outfilename,[cffOverwriteFile,cffCreateDestDirectory,cffPreserveTime], true);
      //postinst
      infilename :=ExtractFileDir(Application.ExeName)+PathDelim+'template-files'+Pathdelim+'postinst';
      outfilename := opsipath+pathdelim+'postinst';
      copyfile(infilename,outfilename,[cffOverwriteFile,cffCreateDestDirectory,cffPreserveTime], true);


      FreeAndNil(patchlist);
      result := true;;
    except
      on E: exception do
      begin
        logdatei.log('createClientFiles file error: ' + E.ClassName + '/' + E.Message,LLError);
      end;
    end;
  end;

  function createOpsiFiles : boolean;
  var
    textlist : Tstringlist;
    i : integer;
    mydep : TPDependency;
    myprop : TPProperties;
    tmpstr : string;
    utcoffset : integer;
    utcoffsetstr : string;
  begin
    result := false;
    try
    textlist := Tstringlist.Create;
    textlist.Add('[Package]');
    textlist.Add('version: '+ intToStr(aktProduct.productdata.packageversion));
    textlist.Add('depends: ');
    textlist.Add('');
    textlist.Add('[Product]');
    textlist.Add('type: '+aktProduct.productdata.producttype);
    textlist.Add('id: '+aktProduct.productdata.productId);
    textlist.Add('name: '+aktProduct.productdata.productName);
    textlist.Add('description: '+aktProduct.productdata.description);
    textlist.Add('advice: '+aktProduct.productdata.advice);
    textlist.Add('version: '+ aktProduct.productdata.productversion);
    textlist.Add('priority: '+ intToStr(aktProduct.productdata.priority));
    textlist.Add('licenseRequired: ');
    textlist.Add('productClasses: ');
    textlist.Add('setupScript: '+ aktProduct.productdata.setupscript);
    textlist.Add('uninstallScript: '+ aktProduct.productdata.uninstallscript);
    textlist.Add('updateScript: ');
    textlist.Add('alwaysScript: ');
    textlist.Add('onceScript: ');
    textlist.Add('customScript: ');
    textlist.Add('userLoginScript: ');

    //dependencies
    for i := 0 to aktProduct.dependencies.Count -1 do
    begin
      mydep := TPDependency(aktProduct.dependencies.Items[i]);
      textlist.Add('');
      textlist.Add('[ProductDependency]');
      textlist.Add('action: setup');
      textlist.Add('requiredProduct: '+mydep.requProductId);
      case mydep.requState of
         noState : ;
         installed : textlist.Add('requiredStatus: installed');
         not_installed : textlist.Add('requiredStatus: not installed');
         unknown : textlist.Add('requiredStatus: unknown');
      end;
      case mydep.requAction of
         noRequest : ;
         setup : textlist.Add('requiredAction: setup');
         uninstall : textlist.Add('requiredAction: uninstall');
         TPActionRequest.update : textlist.Add('requiredAction: update');
      end;
      case mydep.RequType of
         doNotMatter : textlist.Add('requirementType: ');
         before : textlist.Add('requirementType: before');
         after : textlist.Add('requirementType: after');
      end;
    end;

    //ProductProperties
    for i := 0 to aktProduct.properties.Count -1 do
    begin
      myprop := TPProperties(aktProduct.properties.Items[i]);
      textlist.Add('');
      textlist.Add('[ProductProperty]');
      case myprop.ptype of
         bool : textlist.Add('type: bool');
         unicode : textlist.Add('type: unicode');
      end;
      textlist.Add('name: '+myprop.name);
      textlist.Add('description: '+myprop.description);
      if myprop.ptype = bool then
      begin
        textlist.Add('default: '+BoolToStr(myprop.boolDefault,true));
      end
      else
      begin
        textlist.Add('multivalue: '+BoolToStr(myprop.multivalue,true));
        textlist.Add('editable: '+BoolToStr(myprop.editable,true));
        textlist.Add('values: '+myprop.Strvalues.Text);
        textlist.Add('default: '+myprop.StrDefault.Text);
      end;
    end;

    // changelog
    utcoffset := (GetLocalTimeOffset div 60)*100*-1;
    if utcoffset >= 0 then utcoffsetstr := '+';
    utcoffsetstr := utcoffsetstr + format('%4.4d',[utcoffset]);
    textlist.Add('');
    textlist.Add('[Changelog]');
    tmpstr := aktProduct.productdata.productversion+'-'+inttostr(aktProduct.productdata.packageversion);
    textlist.Add(aktProduct.productdata.productId+' ('+tmpstr+') stable; urgency=medium');
    textlist.Add('');
    textlist.Add('  * initial by opsi-setup-detector');
    textlist.Add('');
    textlist.Add('-- '+myconfiguration.fullName+' <'+ myconfiguration.email_address+'> '
                 +FormatDateTime('ddd, dd mmm yyyy hh:nn:ss',
                                 LocalTimeToUniversal(now))+' '+utcoffsetstr);
                 //mon, 04 Jun 12:00:00 + 0100

    textlist.SaveToFile(opsipath+pathdelim+'control');
    FreeAndNil(textlist);
    result := true;
    except
      LogDatei.log('Error in createOpsiFiles',LLError);
     FreeAndNil(textlist);
    end;
  end;

function createProductdirectory : boolean;
var
  goon: boolean;
begin
  prodpath := myconfiguration.workbench_Path + aktProduct.productdata.productId;
  clientpath := prodpath + PathDelim + 'CLIENT_DATA';
  opsipath := prodpath + PathDelim + 'OPSI';
  goon := True;
  if DirectoryExists(prodpath) then
    if not MessageDlg('opsi-setup-detector','Directory ' + prodpath + ' still exits. Overwrite ?', mtWarning, mbOKCancel,'') = mrOk then
      goon := False;
  if goon then
  begin
    if not ForceDirectories(prodpath) then
    begin
      Logdatei.log('Could not create directory: ' + prodpath, LLCritical);
      MessageDlg('opsi-setup-detector','Could not create directory: ' + prodpath, mtError, [mbOK],'');
      goon := False;
    end;
    if not ForceDirectories(clientpath) then
    begin
      Logdatei.log('Could not create directory: ' + clientpath, LLCritical);
      MessageDlg('opsi-setup-detector','Could not create directory: ' + clientpath, mtError,[mbOK],'');
      goon := False;
    end;
    if not ForceDirectories(opsipath) then
    begin
      Logdatei.log('Could not create directory: ' + opsipath, LLCritical);
      MessageDlg('opsi-setup-detector','Could not create directory: ' + opsipath, mtError,[mbOK],'');
      goon := False;
    end;
  end;
  result := goon;
end;


procedure createProductStructure;
var
  prodpath, clientpath, opsipath: string;
  goon: boolean;
begin
  goon := True;
  if not createProductdirectory  then
    begin
      Logdatei.log('createProductdirectory failed' , LLCritical);
      goon := False;
    end;
  if not (goon and createOpsiFiles) then
    begin
      Logdatei.log('createOpsiFiles failed' , LLCritical);
      goon := False;
    end;
  if not (goon and createClientFiles) then
    begin
      Logdatei.log('createClientFiles failed' , LLCritical);
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
  buildCallparams: Tstringlist;
  paramstr : string = '';
  OpsiBuilderProcess: TProcess;
  packit: boolean = False;
  errorstate: boolean = False;
  notused: string = '(not used)';
  output : string;

begin
  buildCallparams:= Tstringlist.Create;
  OpsiBuilderProcess := process.TProcess.Create(nil);
  buildCallbinary := '"'+myconfiguration.PathToOpsiPackageBuilder+'"';
  //paramstr := ' --path=' + myconfiguration.workbench_Path;
  paramstr := ' -p=' + myconfiguration.workbench_Path;
  if AnsiLastChar(paramstr) <> DirectorySeparator then
      paramstr := paramstr + DirectorySeparator;
  //paramstr := paramstr + ' --path='+aktProduct.productdata.productId;
  paramstr := paramstr +aktProduct.productdata.productId;
  //paramstr := paramstr + ' --no-netdrv';
  //paramstr := paramstr + ' --set-rights';
  //buildCallparams.Add(paramstr);
      if resultForm1.RadioButtonPackageBuilder.Checked = True then
    begin
      OpsiBuilderProcess.ShowWindow := swoShowNormal;
    end
    else  // build
    if resultForm1.RadioButtonBuildPackage.Checked = True then
    begin
      if resultForm1.CheckboxBuild.Checked = True then
      begin
        paramstr := paramstr + ' '+'--build=rebuild';
        //paramstr := paramstr +' -–no-gui';
      end;
        //buildCallparams.Add('--build=rebuild');
      if resultForm1.CheckboxInstall.Checked = True then
      begin
        paramstr := paramstr +' --install';
      end;

        //buildCallparams.Add('--install');
      if resultForm1.CheckboxQuiet.Checked = True then
        paramstr := paramstr +' --quiet';
        //buildCallparams.Add('--quiet');
      OpsiBuilderProcess.ShowWindow := swoMinimize;
    end;
  buildCallparams.Add(paramstr);

  {$ifdef WINDOWS}
  LogDatei.log('Try to call opsi packagebuilder',LLnotice);
  //OpsiBuilderProcess.Executable := buildCallbinary;
  //OpsiBuilderProcess.Parameters.Assign(buildCallparams);
  OpsiBuilderProcess.CommandLine:=buildCallbinary+paramstr;
  OpsiBuilderProcess.Options := OpsiBuilderProcess.Options + [poWaitOnExit];
  LogDatei.log('Call: '+OpsiBuilderProcess.Executable+' with: '+OpsiBuilderProcess.Parameters.Text,LLInfo);
  LogDatei.log('Call: '+OpsiBuilderProcess.CommandLine,LLInfo);
  // execute opsiPacketBuilder
    try
      OpsiBuilderProcess.Execute;
      if resultForm1.CheckboxQuiet.Checked = True then
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
      LogDatei.log('Exception while calling '+buildCallbinary+' Message: '+E.message,LLerror);
      ShowMessage(sErrOpsiPackageBuilderStart);
      //        PChar(sMBoxHeader), MB_OK);
      end;
    end;

    resultForm1.PanelProcess.Visible := False;
   // if (resultForm1.CheckboxQuiet.Checked = True) then
   // begin
    //  if (errorstate = False) then
      begin
        if (OpsiBuilderProcess.ExitStatus = 0) then
        begin
          LogDatei.log('Finished calling '+buildCallbinary+' with exitcode: 0 ',LLinfo);
        //ShowMessage(sInfoFinished);
        end
        else
        begin
         LogDatei.log('Error while calling '+buildCallbinary+' with exitcode: '+IntToStr(OpsiBuilderProcess.ExitStatus),LLerror);
         ShowMessage(sErrOpsiPackageBuilderStart);
         //ShowMessage('Error while calling '+buildCallbinary+' with exitcode: '+IntToStr(OpsiBuilderProcess.ExitStatus));
         //ShowMessage(format(sErrOpsiPackageBuilderErrCode,[IntToStr(OpsiBuilderProcess.ExitStatus)]));
        end;
      end;
 //   end;
     {$ENDIF WINDOWS}
  end;   // execute OPSIPackageBuilder



(*
procedure removeOtherTypeSpecificSections(setupType, setupFile: string);
var
  infileHandle, outfileHandle: Text;
  Patchmarker: string = '**--** PATCH_';
  filterType: string = '';
  thisType: string = '';
  aktline: string = '';
  tmpfile: string;
  isMarkerLine: boolean;
begin
  mywrite('removing all other type specific sections from: ' + setupfile);
  tmpfile := setupfile + '.tmp';

        {$I+}//use exceptions
  try
    AssignFile(infileHandle, setupfile);
    AssignFile(outfileHandle, tmpfile);
    reset(infileHandle);
    rewrite(outfileHandle);

    filterType := lowercase(setupType);

    // find and handle type specific patch sections
    while (not EOF(infileHandle)) do
    begin
      // find next type specific marker
      repeat
        ReadLn(infileHandle, aktline);
        isMarkerLine := (0 < pos(Patchmarker, aktline));
        if not isMarkerLine then
          writeln(outfileHandle, aktline);
      until isMarkerLine or EOF(infileHandle);
      if not EOF(infileHandle) then
      begin
        thisType := lowercase(ExtractBetween(aktline, Patchmarker, ' '));
        // write lines (or skip lines if other type)
        repeat
          ReadLn(infileHandle, aktline);
          isMarkerLine := (0 < pos(Patchmarker, aktline));
          if (thisType = filterType) and not isMarkerLine then
            writeln(outfileHandle, aktline);
        until isMarkerLine or EOF(infileHandle);
      end;
    end;

    CloseFile(infileHandle);
    CloseFile(outfileHandle);

    // delete setupfile and rename tmpfile to setupfile
    DeleteFile(PChar(setupfile));
    RenameFile(PChar(tmpfile), PChar(setupfile));

  except
    on E: EInOutError do
      mywrite('removeOtherTypeSpecificSections error: ' +
        E.ClassName + '/' + E.Message);
  end;
end;
*)



end.
