unit osdanalyze;

{$mode delphi}

interface

uses
  Classes,
  osdhelper,
  Process,
  Windows,
  fileutil,
  SysUtils,
  oslog,
  osdbasedata;

const

  SetupType_AdvancedMSI      = 'AdvancedMSI';
  SetupType_Inno             = 'Inno';
  SetupType_InstallShield    = 'InstallShield';
  SetupType_InstallShieldMSI = 'InstallShieldMSI';
  SetupType_MSI              = 'MSI';
  SetupType_NSIS             = 'NSIS';
  SetupType_7zip             = '7zip';


procedure get_aktProduct_info(installerId : TKnownInstaller;myfilename: string);
procedure get_msi_info(myfilename: string);
procedure get_inno_info(myfilename: string);
procedure get_installshield_info(myfilename: string);
procedure get_installshieldmsi_info(myfilename: string);
procedure get_advancedmsi_info(myfilename: string);
procedure get_nsis_info(myfilename: string);
//procedure stringsgrep(myfilename: string; verbose,skipzero: boolean);
procedure Analyze(FileName:string);
procedure grepmsi(instring: string);
//procedure grepmarker(instring: string);
function analyze_binary(myfilename: string; verbose,skipzero: boolean) : TKnownInstaller;

implementation
uses
  resultform;

function grepexe(instring: string) : string;
var lowerstring: string;
begin
  result := '';
  lowerstring := lowercase(instring);
  if
    (0 < pos('installshield', lowerstring)) or
    (0 < pos('inno', lowerstring)) or
    (0 < pos('wise', lowerstring)) or
    (0 < pos('nullsoft', lowerstring)) or
    (0 < pos('wixquery', lowerstring)) or
    (0 < pos('product_build_number{', lowerstring)) or
    (0 < pos('productcode{', lowerstring)) or
    (0 < pos('msiexec', lowerstring)) or
    (0 < pos('extract', lowerstring)) or
    // (0 < pos('setup', lowerstring)) or
    (0 < pos('installer', lowerstring))
    then
       result :=instring;
end;

procedure analyze_binstr(instring: string);
var
  lowerstring: string;
  counter : integer;
  aktId, foundId : TKnownInstaller;


  procedure check_line_for_installer(line : string; instId : TKnownInstaller);
  var
    i : integer;
  begin
    for i := 0 to installerArray[integer(instId)].patterns.Count -1 do
    begin
      if 0 <> pos(LowerCase(installerArray[integer(instId)].patterns[i]),line) then
         aktProduct.markerlist.add(installerArray[integer(instId)].Name+inttostr(i));
    end
  end;

begin
  lowerstring := lowercase(instring);
  for counter := 0 to knownInstallerList.Count - 1 do
  begin
    aktId := installerArray[counter].installerId;
    if  aktId <> stUnknown then
    begin
      check_line_for_installer(lowerstring,aktId);
    end;
  end;
end;

(*
procedure grepmarker(instring: string);
var lowerstring: string;
begin
  lowerstring := lowercase(instring);
  if (0 < pos('installer,msi,database', lowerstring))  then begin
     markerEmbeddedMSI := true;
  end;
  if (0 < pos('installshield', lowerstring)) then begin
     markerInstallShield := true;
  end;
  if (markerEmbeddedMSI and markerInstallShield) then begin
     mywrite('found strings "Installer,MSI,Database" and "InstallShield"');
     mywrite('detected InstallShield+MSI Setup (InstallShield with embedded MSI)');
     setupType := SetupType_InstallShieldMSI;
  end;
  if (markerInstallShield and ((0 < pos('<description>InstallShield.Setup</description>', instring))) or ((0 < pos('InstallShield', instring)))) then begin
     mywrite('found string "<description>InstallShield.Setup</description>" or "InstallShield"');
     mywrite('detected InstallShield Setup');
     setupType := SetupType_InstallShield;
  end;
  if (0 < pos('inno', lowerstring)) and ((0 < pos('<description>inno setup</description>', lowerstring)) or (0 < pos('jr.inno.setup', lowerstring))) then begin
     mywrite('found string "<description>Inno Setup</description>" or "JR.Inno.Setup"');
     mywrite('detected Inno Setup');
     setupType := SetupType_Inno;
  end;
  if (0 < pos('nullsoft', lowerstring)) and ((0 < pos('Nullsoft.NSIS.exehead', lowerstring)) or (0 < pos('nullsoft install system', lowerstring))) then begin
      if (0 < pos('nullsoft.nsis.exehead', lowerstring)) then
         mywrite('found string "Nullsoft.NSIS.exehead"')
      else
         mywrite('found string "Nullsoft Install System"');
     mywrite('detected NSIS Setup');
     setupType := SetupType_NSIS;
  end;
  if (0 < pos('name="microsoft.windows.advancedinstallersetup"', lowerstring))  then begin
     mywrite('found string "microsoft.windows.advancedinstallersetup"');
     mywrite('detected Advanced Setup (with embedded MSI)');
     setupType := SetupType_AdvancedMSI;
  end;
end;
 *)

procedure grepmsi(instring: string);
begin
  if (0 < pos('product_build_number{', lowercase(instring))) or
    (0 < pos('productcode{', lowercase(instring))) then
    mywrite(instring);
end;

procedure get_aktProduct_info(installerId : TKnownInstaller;myfilename: string);
var
  myoutlines: TStringList;
  myreport : string;
  myexitcode : integer;
  i: integer;
  fsize : int64;
//  fsizemb,
  sFileSize : string;
  sReqSize : string;
  sSearch : string;
  iPos : integer;
  destDir : string;
  myBatch: string;
  product: string;
  FileInfo : TSearchRec;
  exefile: string;
  smask: string;
  installerstr : string;

begin
  installerstr := installerToInstallerstr(installerId);
  Mywrite('Analyzing '+installerstr+' Setup: ' + myfilename);

  aktProduct.istallerId:=installerId;
  aktProduct.setup32FileName:=ExtractFileName(myfilename);
  aktProduct.setup32FileNamePath:=ExtractFileDir(myfilename);

  if installerId in [stMsi,stInstallShieldMSI, stAdvancedMSI]  then
  begin
   // installerId in [stMsi,stInstallShieldMSI, stAdvancedMSI] then
    //begin
     // extract  MSI from setup for analyze
     // check extract dir
    if DirectoryExists(opsitmp) then
       DeleteDirectory(opsitmp, true);
    if not DirectoryExists(opsitmp) then createdir(opsitmp);
    if not DirectoryExists(opsitmp) then
       mywrite('Error: could not create directory: '+opsitmp);

    // prepare extract

    //Mywrite('Analyzing MSI from InstallShield Setup '+myfilename);

    myoutlines := TStringList.Create;
    // myBatch := 'cmd.exe /C '+ExtractFilePath(paramstr(0))+'extractMSI.cmd "'+myfilename+'"'; // (does not work with spaces in EXE path)
    myBatch := '"'+ExtractFilePath(paramstr(0))+'extractMSI.cmd" "'+myfilename+'"'; // dropped cmd.exe
    Mywrite(myBatch);
    Mywrite('!! PLEASE WAIT !!');
    Mywrite('!! PLEASE WAIT !! Extracting and analyzing MSI ...');
    Mywrite('!! PLEASE WAIT !!');


    product := ExtractFileNameWithoutExt(myfilename);
    // run extract
    if not RunCommandAndCaptureOut(myBatch, true,myoutlines,myreport, SW_SHOWMINIMIZED, myexitcode) then
    begin
       mywrite('Failed to to run "' + myBatch +'": '+myreport);
    end
    else
    begin
       smask := opsitmp + '*.msi';
       Mywrite(smask);
       if SysUtils.FindFirst (smask,faAnyFile,FileInfo)=0 then
       begin
         //resultform1.EditMSI_file.Text := opsitmp + FileInfo.Name;
         aktProduct.msi32FullFileName:=opsitmp + FileInfo.Name;;
       end;
     end;
     get_msi_info(aktProduct.msi32FullFileName);
    end;
end; //get_aktProduct_info



procedure get_msi_info(myfilename: string);
var
  mycommand : string;
  myoutlines : TStringList;
  myreport : string;
  myexitcode : integer;
  i: integer;
  fsize : int64;
  fsizemb, rsizemb : double;
  sMsiSize : string;
  sReqSize : string;
  sSearch : string;
  iPos : integer;

begin
  Mywrite('Analyzing MSI: ' + myfilename);

  mycommand := 'cmd.exe /C cscript.exe "'+ExtractFilePath(paramstr(0))+'msiinfo.js" "'+myfilename+'"';
  mywrite (mycommand);
  myoutlines := TStringList.Create;
  if not RunCommandAndCaptureOut (mycommand, true, myoutlines, myreport, SW_SHOWMINIMIZED, myexitcode) then
  begin
    mywrite('Failed to analyze: '+myreport);
  end
  else
  begin
    aktProduct.istallerId:=stMsi;
    resultForm1.Edit_installer_type.Text := installerToInstallerstr(stMsi);
    //resultForm1.EditMSI_file.Text := myfilename;
    mywrite('........');
    aktProduct.setup32FileName:=ExtractFileName(myfilename);
    aktProduct.setup32FileNamePath:=ExtractFileDir(myfilename);
    resultForm1.setup32NameEdit.Text := myfilename;
    resultForm1.setup32NameEdit.Hint:= myfilename;
    resultForm1.Edit_ProductName.Text := '';
    resultForm1.Edit_ProductVersion.Text := '';
    resultForm1.Edit_msiid.Text := '';
    (*
    resultForm1.EditMSI_ProductName.Text := '';
    resultForm1.EditMSI_ProductVersion.Text := '';
    resultForm1.EditMSI_ProductCode.Text := '';
    *)
    for i := 0 to myoutlines.Count-1 do
    begin
      mywrite(myoutlines.Strings[i]);

      // sSearch := 'Manufacturer: ';
      // iPos := Pos (sSearch, myoutlines.Strings[i]);
      // if (iPos <> 0) then
      //   resultForm1.Edit2.Text := Copy(myoutlines.Strings[i], Length(sSearch)+1, Length(myoutlines.Strings[i])-Length(sSearch));

      sSearch := 'ProductName: ';
      iPos := Pos (sSearch, myoutlines.Strings[i]);
      if (iPos <> 0) then
        resultForm1.Edit_ProductName.Text := Copy(myoutlines.Strings[i], Length(sSearch)+1, Length(myoutlines.Strings[i])-Length(sSearch));

      sSearch := 'ProductVersion: ';
      iPos := Pos (sSearch, myoutlines.Strings[i]);
      if (iPos <> 0) then
        resultForm1.Edit_ProductVersion.Text := Copy(myoutlines.Strings[i], Length(sSearch)+1, Length(myoutlines.Strings[i])-Length(sSearch));

      sSearch := 'ProductCode: ';
      iPos := Pos (sSearch, myoutlines.Strings[i]);
      if (iPos <> 0) then
        resultForm1.Edit_msiid.Text := Copy(myoutlines.Strings[i], Length(sSearch)+1, Length(myoutlines.Strings[i])-Length(sSearch));

    end;
    fsize := fileutil.FileSize(myfilename);
    fsizemb :=  fsize /(1024*1024);
    rsizemb := fsizemb*6;
    sMsiSize := FormatFloat('##0.0',fsizemb)+' MB';
    sReqSize := FormatFloat('###0',rsizemb)+' MB';

    mywrite('MSI file size is: ' + sMsiSize);
    mywrite('Estimated required space is: ' + sReqSize);
    mywrite('........');

    if fsizemb < 1 then
       fsizemb := 1;

    sMsiSize := FormatFloat('#',fsizemb)+' MB';
    sReqSize := FormatFloat('#',rsizemb)+' MB';

    resultForm1.Edit_RequiredSpace.Text := sReqSize;
    resultForm1.Edit_FileSize.Text := sMsiSize;
    aktProduct.requiredSpace:= round(rsizemb);
    aktProduct.setup32FileSize:= round(fsizemb);

    resultForm1.setDefaultParametersMSI;

    mywrite('get_MSI_info finished');
    myoutlines.Free;

  end;
  resultForm1.PageControl1.ActivePage := resultForm1.TabSheetAnalyze;
  if showMSI then
    resultForm1.PageControl1.ActivePage := resultForm1.TabSheetDefault;
    //resultForm1.PageControl1.ActivePage := resultForm1.TabSheetMSI;
end;


procedure get_inno_info(myfilename: string);
var
  myoutlines: TStringList;
  myreport : string;
  myexitcode : integer;
  i: integer;
  fsize : int64;
  fsizemb : double;
  rsizemb : double;
  sFileSize : string;
  sReqSize : string;
  sSearch : string;
  iPos : integer;
  destDir : string;
  myCommand : string;
  installScriptISS : string;
  issLine: string = '';
  fISS: Text;
  // components from install_script.iss[setup]
  AppName: string ='';
  AppVersion: string ='';
  AppVerName: string ='';
  DefaultDirName: string ='';

begin
  Mywrite('Analyzing Inno-Setup:');
  Mywrite('extract install_script.iss from ' + myfilename + ' to');

  myoutlines := TStringList.Create;
  destDir := StringReplace (ExtractFileName(myfilename), ExtractFileExt(myfilename), '', [rfReplaceAll,rfIgnoreCase]); // delete Extension
  destDir := configDir + 'INNO' +DirectorySeparator+ StringReplace (destDir, '.', '-', [rfReplaceAll,rfIgnoreCase]);
  installScriptISS := destDir+DirectorySeparator+'install_script.iss';

  Mywrite(installScriptISS);
  // myCommand := 'cmd.exe /C "'+ExtractFilePath(paramstr(0))+'innounp.exe" -x -a -y -d"'+destDir+'" ' +myfilename+ ' install_script.iss';
  myCommand := '"'+ExtractFilePath(paramstr(0))+'innounp.exe" -x -a -y -d"'+destDir+'" ' +myfilename+ ' install_script.iss';
  Mywrite(myCommand);

  if not RunCommandAndCaptureOut(myCommand, true,myoutlines,myreport, SW_SHOWMINIMIZED, myexitcode) then
  begin
    mywrite('Failed to extract install_script.iss: '+myreport);
  end
  else
  begin
    for i := 0 to myoutlines.Count-1 do
      mywrite(myoutlines.Strings[i]);
    myoutlines.Free;

    // read install_script.iss
    if FileExists(installScriptISS) then
    begin
      Mywrite(installScriptISS);
      AssignFile(fISS, installScriptISS);
      Reset(fISS);
      // search [Setup] section
      while (not EOF(fISS)) and (not(0 < pos('[setup]', lowercase(issLine)))) do begin
        ReadLn(fISS,issLine);
      end;

      AppName := '';
      AppVersion := '';
      AppVerName := '';
      DefaultDirName := '';
      ArchitecturesInstallIn64BitMode := '';

      mywrite('........');
      mywrite('[Setup]');
      ReadLn(fISS,issLine);

      // read until next section (usually [Files])
      while (not EOF(fISS)) and (Length(issLine)>0) and (issLine[1] <>'[') do begin
        mywrite(issLine);
        if (0 < pos('appname=', lowercase(issLine))) then
          AppName := Copy(issLine, pos('=',issLine)+1, 100);
        if (0 < pos('appversion=', lowercase(issLine))) then
          AppVersion := Copy(issLine, pos('=',issLine)+1, 100);
        if (0 < pos('appvername=', lowercase(issLine))) then
          AppVerName := Copy(issLine, pos('=',issLine)+1, 100);
        if (0 < pos('defaultdirname=', lowercase(issLine))) then
          DefaultDirName := Copy(issLine, pos('=',issLine)+1, 100);
        if (0 < pos('architecturesinstallin64bitmode=', lowercase(issLine))) then
          ArchitecturesInstallIn64BitMode := Copy(issLine, pos('=',issLine)+1, 100);
        ReadLn(fISS,issLine);
      end;

      // get AppVersion from AppVerName (AppVerName = AppName + AppVersion ?)
      if (AppVersion = '') and (AppVerName <> '') then begin
         AppVersion := StringReplace (AppVerName, AppName, '', []);
         AppVersion := StringReplace (AppVersion, ' ', '', [rfReplaceAll,rfIgnoreCase]);
      end;

      CloseFile(fISS);
    end;

    aktProduct.setup32FileName:=ExtractFileName(myfilename);
    aktProduct.setup32FileNamePath:=ExtractFileDir(myfilename);
    resultForm1.EditInnoFilename.Text := myfilename;
    mywrite('........');
    resultForm1.Edit_opsiProductID.Text := resultForm1.getPacketIDShort(AppName);
    resultForm1.Edit_ProductName.Text := AppName;
    resultForm1.Edit_ProductVersion.Text := AppVersion;
    if AppVerName = '' then  begin
       if ((AppName <> '') and (AppVersion <> '')) then
          AppVerName := AppName + ' ' +AppVersion
       else
          AppVerName := AppName + AppVersion
    end;
    if (0 < pos('x64', lowercase(ArchitecturesInstallIn64BitMode))) then
       DefaultDirName := StringReplace (DefaultDirName, '{pf}', '%ProgramFilesSysnativeDir%', [rfReplaceAll,rfIgnoreCase])
    else
       DefaultDirName := StringReplace (DefaultDirName, '{pf}', '%ProgramFiles32Dir%', [rfReplaceAll,rfIgnoreCase]);
    DefaultDirName := StringReplace (DefaultDirName, '{sd}', '%Systemdrive%', [rfReplaceAll,rfIgnoreCase]);
    resultForm1.InstallDirEdit.Text := DefaultDirName;  //+'\unins000.exe'

    resultForm1.MemoDefault.Clear;
    resultForm1.MemoDefault.Append(AppVerName);

    fsize := fileutil.FileSize(myfilename);
    fsizemb :=  fsize /(1024*1024);
    rsizemb := fsizemb*6;
    sFileSize := FormatFloat('##0.0',fsizemb)+' MB';
    sReqSize := FormatFloat('###0',rsizemb)+' MB';

    mywrite('Setup file size is: ' + sFileSize);
    mywrite('Estimated required space is: ' + sReqSize);
    mywrite('........');

    if fsizemb < 1 then
       fsizemb := 1;

    sFileSize := FormatFloat('#',fsizemb)+' MB';
    sReqSize := FormatFloat('#',rsizemb)+' MB';

    resultForm1.EditInnoRequiredSpace.Text := sReqSize;
    resultForm1.EditInnoFileSize.Text := sFileSize;

    resultForm1.setDefaultParametersInno;

    aktProduct.requiredSpace:= round(rsizemb);
    aktProduct.setup32FileSize:= round(fsizemb);

    mywrite('get_inno_info finished');
    mywrite('Inno Setup detected');

  end;
  if showInnoSetup then
    resultForm1.PageControl1.ActivePage := resultForm1.TabSheetDefault;
    //resultForm1.PageControl1.ActivePage := resultForm1.TabSheetInnoSetup;
end;


procedure get_installshield_info(myfilename: string);
var
  myoutlines: TStringList;
  myreport : string;
  myexitcode : integer;
  i: integer;
  fsize : int64;
  fsizemb, rsizemb : double;
  sFileSize : string;
  sReqSize : string;
  sSearch : string;
  iPos : integer;
  destDir : string;
  myBatch: string;
  product: string;

begin
  Mywrite('Analyzing InstallShield-Setup:');
    aktProduct.istallerId:=stInstallShield;
  resultForm1.Edit_installer_type.Text := installerToInstallerstr(stInstallShield);
  resultForm1.setup32NameEdit.Text := myfilename;
  aktProduct.setup32FileName:=ExtractFileName(myfilename);
  aktProduct.setup32FileNamePath:=ExtractFileDir(myfilename);


  aktProduct.setup32FileName:=ExtractFileName(myfilename);
  aktProduct.setup32FileNamePath:=ExtractFileDir(myfilename);
  resultForm1.setup32NameEdit.Text := myfilename;

  product := ExtractFileNameWithoutExt(myfilename);
  resultForm1.Edit_opsiProductID.Text := resultForm1.getPacketIDShort(product);
  resultForm1.Edit_ProductName.Text := product;
  resultForm1.Edit_ProductVersion.Text := product;

  aktProduct.productId:=  resultForm1.getPacketIDShort(product);
  aktProduct.productName:=product;
  aktProduct.productversion:=product;
  if (test) then
    resultForm1.InstallDirEdit.Text := '%ProgramFilesSysnativeDir%' +DirectorySeparator+ product;
  resultForm1.MemoDefault.Append(product);

  fsize := fileutil.FileSize(myfilename);
  fsizemb :=  fsize /(1024*1024);
  rsizemb := fsizemb*6;
  sFileSize := FormatFloat('##0.0',fsizemb)+' MB';
  sReqSize  := FormatFloat('###0',rsizemb)+' MB';

  if fsizemb < 1 then
     fsizemb := 1;

  mywrite('Setup file size is: ' + sFileSize);
  mywrite('Estimated required space is: ' + sReqSize);
  mywrite('........');

  resultForm1.Edit_RequiredSpace.Text := sReqSize;
  resultForm1.Edit_FileSize.Text := sFileSize;
  aktProduct.requiredSpace:= round(rsizemb);
  aktProduct.setup32FileSize:= round(fsizemb);

  mywrite('get_InstallShield_info finished');
  mywrite('InstallShield Setup detected');

  if showInstallShield then
    //resultForm1.PageControl1.ActivePage := resultForm1.TabSheetInstallShield;
    resultForm1.PageControl1.ActivePage := resultForm1.TabSheetDefault;
end;


procedure get_installshieldmsi_info(myfilename: string);
var
  myoutlines: TStringList;
  myreport : string;
  myexitcode : integer;
  i: integer;
  fsize : int64;
  fsizemb, rsizemb : double;
  sMsiSize : string;
  sFileSize : string;
  sReqSize : string;
  sSearch : string;
  iPos : integer;
  destDir : string;
  myBatch: string;
  product: string;
  FileInfo : TSearchRec;
  exefile: string;
  smask: string;

begin
  Mywrite('Analyzing InstallShield+MSI Setup: ' + myfilename);
  aktProduct.istallerId:=stInstallShieldMSI;
  resultForm1.Edit_installer_type.Text := installerToInstallerstr(stInstallShieldMSI);
  resultForm1.setup32NameEdit.Text := myfilename;
  aktProduct.setup32FileName:=ExtractFileName(myfilename);
  aktProduct.setup32FileNamePath:=ExtractFileDir(myfilename);

  if DirectoryExists(opsitmp) then
     DeleteDirectory(opsitmp, true);
  if not DirectoryExists(opsitmp) then createdir(opsitmp);
  if not DirectoryExists(opsitmp) then
     mywrite('Error: could not create directory: '+opsitmp);

  // extract and analyze MSI from setup

  Mywrite('Analyzing MSI from InstallShield Setup '+myfilename);

  myoutlines := TStringList.Create;
  // myBatch := 'cmd.exe /C '+ExtractFilePath(paramstr(0))+'extractMSI.cmd "'+myfilename+'"'; // (does not work with spaces in EXE path)
  myBatch := '"'+ExtractFilePath(paramstr(0))+'extractMSI.cmd" "'+myfilename+'"'; // dropped cmd.exe
  Mywrite(myBatch);
  Mywrite('!! PLEASE WAIT !!');
  Mywrite('!! PLEASE WAIT !! Extracting and analyzing MSI ...');
  Mywrite('!! PLEASE WAIT !!');

  product := ExtractFileNameWithoutExt(myfilename);

  resultForm1.EditInstallShieldMSIProductID.Text := product;
  resultForm1.EditInstallShieldMSIProductName.Text := product;
  resultForm1.EditInstallShieldMSIProductVersion.Text := product;
  resultForm1.EditInstallShieldMSIProductCode.Text := 'XXX';

  if not RunCommandAndCaptureOut(myBatch, true,myoutlines,myreport, SW_SHOWMINIMIZED, myexitcode) then
  begin
     mywrite('Failed to to run "' + myBatch +'": '+myreport);
  end
  else
  begin
     smask := opsitmp + '*.msi';
     Mywrite(smask);
     if SysUtils.FindFirst (smask,faAnyFile,FileInfo)=0 then
     begin
       //resultform1.EditMSI_file.Text := opsitmp + FileInfo.Name;
       aktProduct.msi32FullFileName:=opsitmp + FileInfo.Name;;

       // analyze the extracted MSI
       resultform1.OpenDialog1.InitialDir := opsitmp;
       resultform1.OpenDialog1.FileName := FileInfo.Name;
       get_msi_info(aktProduct.msi32FullFileName);

       (*
       // and use the parameters from get_msi_info (MSI analyze)
       product := resultForm1.EditMSI_opsiProductID.Text;
       resultForm1.EditInstallShieldMSIProductID.Text := product;
       resultForm1.EditInstallShieldMSIProductName.Text := resultForm1.EditMSI_ProductName.Text;
       resultForm1.EditInstallShieldMSIProductVersion.Text := resultForm1.EditMSI_ProductVersion.Text;
       resultForm1.EditInstallShieldMSIProductCode.Text := resultForm1.EditMSI_ProductCode.Text;
       resultForm1.MemoInstallShieldMSIDescription.Append(resultForm1.EditMSI_ProductName.Text);
       *)

       // reset OpenDialog parameters
       resultform1.OpenDialog1.InitialDir := myfilename;
       resultform1.OpenDialog1.FileName := ExtractFileNameWithoutExt(myfilename);

       // resultform1.setDefaultParametersMSI;
     end
     else
     begin
      //todo call Dialog here
        //Application.MessageBox(pchar(format(sErrExtractMSIfailed, [myfilename])), pchar(sMBoxHeader), MB_OK);
     end
  end;

  fsize := fileutil.FileSize(myfilename);
  fsizemb :=  fsize /(1024*1024);
  rsizemb := fsizemb*6;
  sMsiSize := FormatFloat('##0.0',fsizemb)+' MB';
  sReqSize := FormatFloat('###0',rsizemb)+' MB';

  if fsizemb < 1 then
     fsizemb := 1;

  mywrite('Setup file size is: ' + sFileSize);
  mywrite('Estimated required space is: ' + sReqSize);
  mywrite('........');

  resultForm1.Edit_RequiredSpace.Text := sReqSize;
  resultForm1.Edit_FileSize.Text := sFileSize;
      aktProduct.requiredSpace:= round(rsizemb);
    aktProduct.setup32FileSize:= round(fsizemb);


  mywrite('get_InstallShield_info finished');
  mywrite('InstallShield+MSI Setup detected');

  if showInstallShieldMSI then
    //resultForm1.PageControl1.ActivePage := resultForm1.TabSheetInstallShieldMSI;
    resultForm1.PageControl1.ActivePage := resultForm1.TabSheetDefault;


end;



procedure get_advancedmsi_info(myfilename: string);
var
  myoutlines: TStringList;
  myreport : string;
  myexitcode : integer;
  i: integer;
  fsize : int64;
  fsizemb, rsizemb : double;
  sFileSize : string;
  sReqSize : string;
  sSearch : string;
  iPos : integer;
  destDir : string;
  myBatch: string;
  product: string;
  FileInfo : TSearchRec;
  exefile: string;
  smask: string;

begin
  Mywrite('Analyzing AdvancedMSI Setup: ' + myfilename);
  resultForm1.EditAdvancedMSIFilename.Text := myfilename;

  // extract and analyze MSI from setup

  Mywrite('Analyzing MSI from Setup '+myfilename);

  myoutlines := TStringList.Create;
  myBatch := 'cmd.exe /C "'+myfilename+'" /extract:'+opsitmp;
  Mywrite(myBatch);
  Mywrite('!! PLEASE WAIT !!');
  Mywrite('!! PLEASE WAIT !! Extracting and analyzing MSI ...');
  Mywrite('!! PLEASE WAIT !!');


  product := ExtractFileNameWithoutExt(myfilename);
  resultForm1.EditAdvancedMSIProductID.Text := product;
  resultForm1.EditAdvancedMSIProductName.Text := product;
  resultForm1.EditAdvancedMSIProductVersion.Text := product;
  resultForm1.EditAdvancedMSIProductCode.Text := 'XXX';

  if DirectoryExists(opsitmp) then
     DeleteDirectory(opsitmp, true);
  if not DirectoryExists(opsitmp) then createdir(opsitmp);
  if not DirectoryExists(opsitmp) then
    mywrite('Error: could not create directory: '+opsitmp);

  if not RunCommandAndCaptureOut(myBatch, true,myoutlines,myreport, SW_SHOWMINIMIZED, myexitcode) then
  begin
    mywrite('Failed to extract MSI: '+myreport);
  end
  else
  begin
    smask := opsitmp+'*.msi';
    Mywrite(smask);
     if SysUtils.FindFirst (smask,faAnyFile,FileInfo)=0 then
     begin
       resultform1.EditMSI_file.Text := opsitmp+FileInfo.Name;

       // analyze the extracted MSI
       resultform1.OpenDialog1.InitialDir := opsitmp;
       resultform1.OpenDialog1.FileName := FileInfo.Name;
       get_msi_info(resultform1.EditMSI_file.Text);

       // and use the parameters from get_msi_info (MSI analyze)
       product := resultForm1.EditMSI_opsiProductID.Text;
       resultForm1.EditAdvancedMSIProductID.Text := product;
       resultForm1.EditAdvancedMSIProductName.Text := resultForm1.EditMSI_ProductName.Text;
       resultForm1.EditAdvancedMSIProductVersion.Text := resultForm1.EditMSI_ProductVersion.Text;
       resultForm1.EditAdvancedMSIProductCode.Text := resultForm1.EditMSI_ProductCode.Text;
       resultForm1.MemoAdvancedMSIDescription.Append(resultForm1.EditMSI_ProductName.Text);

       // reset OpenDialog parameters
       resultform1.OpenDialog1.InitialDir := myfilename;
       resultform1.OpenDialog1.FileName := ExtractFileNameWithoutExt(myfilename);

       // resultform1.setDefaultParametersMSI;
     end;
  end;

  fsize := fileutil.FileSize(myfilename);
  fsizemb :=  fsize /(1024*1024);
  sFileSize := FormatFloat('##0.0',fsizemb)+' MB';
  sReqSize  := FormatFloat('###0',fsizemb*6)+' MB';

  if fsizemb < 1 then
     fsizemb := 1;

  mywrite('Setup file size is: ' + sFileSize);
  mywrite('Estimated required space is: ' + sReqSize);
  mywrite('........');

  resultForm1.EditAdvancedMSIRequiredSpace.Text := sReqSize;
  resultForm1.EditAdvancedMSIFileSize.Text := sFileSize;

  mywrite('get_AdvancedMSI_info finished');
  mywrite('Advancd Installer Setup (with embedded MSI) detected');

  if showAdvancedMSI then
    //resultForm1.PageControl1.ActivePage := resultForm1.TabSheetAdvancedMSI;
    resultForm1.PageControl1.ActivePage := resultForm1.TabSheetDefault;

end;




procedure get_nsis_info(myfilename: string);
var
  myoutlines: TStringList;
  myreport : string;
  myexitcode : integer;
  i: integer;
  fsize : int64;
  fsizemb : double;
  sFileSize : string;
  sReqSize : string;
  sSearch : string;
  iPos : integer;
  destDir : string;
  myBatch: string;
  product: string;

begin
  Mywrite('Analyzing NSIS-Setup:');

  resultForm1.EditNsisFilename.Text := myfilename;

  product := ExtractFileNameWithoutExt(myfilename);
  resultForm1.EditNsisProductID.Text := product;
  resultForm1.EditNsisProductName.Text := product;
  resultForm1.EditNsisProductVersion.Text := resultForm1.ExtractVersion(product);
  if (test) then
    resultForm1.EditNsisInstallDir.Text := '%ProgramFilesSysnativeDir%' +DirectorySeparator+ product;
  resultForm1.MemoNsisDescription.Append(product);

  fsize := fileutil.FileSize(myfilename);
  fsizemb :=  fsize /(1024*1024);
  sFileSize := FormatFloat('##0.0',fsizemb)+' MB';
  sReqSize  := FormatFloat('###0',fsizemb*6)+' MB';

  if fsizemb < 1 then
     fsizemb := 1;

  mywrite('Setup file size is: ' + sFileSize);
  mywrite('Estimated required space is: ' + sReqSize);
  mywrite('........');

  resultForm1.EditNsisRequiredSpace.Text := sReqSize;
  resultForm1.EditNsisFileSize.Text := sFileSize;

  mywrite('get_nsis_info finished');
  mywrite('NSIS (Nullsoft Install System) detected');

  if showNsis then
    //resultForm1.PageControl1.ActivePage := resultForm1.TabSheetNsis;
    resultForm1.PageControl1.ActivePage := resultForm1.TabSheetDefault;
end;

(*
procedure stringsgrep(myfilename: string; verbose,skipzero: boolean);
var
  FileStream: TFileStream;
  CharIn: char;
  MinLen, MaxLen: integer;
  CurrValue: string;
  i: integer;
  size: longint;
  buffer: array [0 .. 2047] of char;
  charsread: longint;
  msg: string;

begin
  MinLen := 5;
  MaxLen := 512;
  CurrValue := '';
  setupType := '';

  mywrite('------------------------------------');
  Mywrite('Analyzing: ' + myfilename);
  msg := 'stringsgrep started (verbose:';
  if verbose=true then msg := msg+'true' else msg:=msg+'false';
  msg := msg + ', skipzero:';
  if skipzero=true then msg := msg+'true' else msg:=msg+'false';
  msg := msg+')';
  mywrite(msg);
  FileStream := TFileStream.Create(myfilename, fmOpenRead);
  markerEmbeddedMSI := false;
  markerInstallShield := false;
  try
    size := FileStream.Size;
    while (size > 0)  and (setupType = '') do
    begin
      charsread := FileStream.Read(buffer, sizeof(buffer));
      size := size - charsread;

      for i := 0 to charsread - 1 do
      begin
        charIn := buffer[i];

        // skipzero: handling of wide strings by ignoring zero byte
        if skipzero and (CharIn = #0) then
           continue;

        // if (CharIn in [' ','A'..'Z','a'..'z','0'..'9','<','>','.','/','_','-']) and (Length(CurrValue) < MaxLen) then
        if (CharIn in [#32..#126]) and (Length(CurrValue) < MaxLen) then
          CurrValue := CurrValue + CharIn;

        if (Length(CurrValue) < MaxLen) and (i < charsread-1) then
           continue;

        if (Length(CurrValue) >= MinLen) then
        begin
          if '.exe' = lowercase(ExtractFileExt(myfilename)) then
          begin
            if verbose then
            begin
               grepexe(CurrValue);
               logdatei.log(CurrValue,LLDebug);
            end
            else
               grepmarker(CurrValue);
          end
          else if '.msi' = lowercase(ExtractFileExt(myfilename)) then
          begin
            setupType := setupType_MSI;
            if verbose then
               grepmsi(CurrValue);
          end
          else
          begin
            grepexe(CurrValue);
            grepmsi(CurrValue);
            logdatei.log(CurrValue,LLDebug);
          end;
          CurrValue := '';
        end;
      end;

    end;
    msg := 'stringsgrep completed (verbose:';
    if verbose=true then msg := msg+'true' else msg:=msg+'false';
    msg := msg + ', skipzero:';
    if skipzero=true then msg := msg+'true' else msg:=msg+'false';
    msg := msg+')';
    mywrite(msg);
    mywrite('------------------------------------');
  finally
    FileStream.Free;
  end;
end;
*)

function analyze_markerlist : TKnownInstaller;
var
  i : integer;

begin
  result := stUnknown;
  for i := 0 to  integer(stUnknown) do
  begin
    if installerArray[i].detected(aktProduct.markerlist) then
      result := TKnownInstaller(i);
  end;
end;

function analyze_binary(myfilename: string; verbose,skipzero: boolean) : TKnownInstaller;
var
  FileStream: TFileStream;
  CharIn: char;
  MinLen, MaxLen: integer;
  CurrValue: string;
  i: integer;
  size: longint;
  buffer: array [0 .. 2047] of char;
  charsread: longint;
  msg: string;
  setuptype : TKnownInstaller;

begin
  MinLen := 5;
  MaxLen := 512;
  CurrValue := '';
  setupType := stUnknown;
  result := stUnknown;

  mywrite('------------------------------------');
  Mywrite('Analyzing: ' + myfilename);
  msg := 'stringsgrep started (verbose:';
  if verbose=true then msg := msg+'true' else msg:=msg+'false';
  msg := msg + ', skipzero:';
  if skipzero=true then msg := msg+'true' else msg:=msg+'false';
  msg := msg+')';
  mywrite(msg);
  FileStream := TFileStream.Create(myfilename, fmOpenRead);
  //markerEmbeddedMSI := false;
  //markerInstallShield := false;
  try
    size := FileStream.Size;
    while (size > 0)  and (setupType = stUnknown) do
    begin
      charsread := FileStream.Read(buffer, sizeof(buffer));
      size := size - charsread;

      for i := 0 to charsread - 1 do
      begin
        charIn := buffer[i];

        // skipzero: handling of wide strings by ignoring zero byte
        if skipzero and (CharIn = #0) then
           continue;

        // if (CharIn in [' ','A'..'Z','a'..'z','0'..'9','<','>','.','/','_','-']) and (Length(CurrValue) < MaxLen) then
        if (CharIn in [#32..#126]) and (Length(CurrValue) < MaxLen) then
          CurrValue := CurrValue + CharIn;

        if (Length(CurrValue) < MaxLen) and (i < charsread-1) then
           continue;

        if (Length(CurrValue) >= MinLen) then
        begin
          if '.exe' = lowercase(ExtractFileExt(myfilename)) then
          begin
            if verbose then
            begin
               //grepexe(CurrValue);
              analyze_binstr(CurrValue);
               logdatei.log(CurrValue,LLDebug);
            end
            else
               analyze_binstr(CurrValue);
          end
          else if '.msi' = lowercase(ExtractFileExt(myfilename)) then
          begin
            setupType := stMsi;
            if verbose then
               grepmsi(CurrValue);
          end
          else
          begin
            grepexe(CurrValue);
            grepmsi(CurrValue);
            logdatei.log(CurrValue,LLDebug);
          end;
          CurrValue := '';
        end;
      end;

    end;
    msg := 'stringsgrep completed (verbose:';
    if verbose=true then msg := msg+'true' else msg:=msg+'false';
    msg := msg + ', skipzero:';
    if skipzero=true then msg := msg+'true' else msg:=msg+'false';
    msg := msg+')';
    mywrite(msg);
    mywrite('------------------------------------');
  finally
    FileStream.Free;
  end;
  Result := analyze_markerlist;
end;


procedure Analyze(FileName:string);
var
  setupType : TKnownInstaller;
begin
  aktProduct.setup32FileNamePath := FileName;
  resultform1.clearAllTabs;
  setupType := stUnknown;
  if '.msi' = lowercase(ExtractFileExt(FileName)) then
    get_msi_info(FileName)
  else begin
    //stringsgrep(FileName, false, false); // filename, verbose, skipzero
    setupType := analyze_binary(FileName, false, false); // filename, verbose, skipzero

    if (setupType = stInno) then
      get_inno_info(FileName)
    else if (setupType = stNsis) then
      get_nsis_info(FileName)
    else if (setupType = stInstallShield) then
      get_installshield_info(FileName)
    else if (setupType =  stInstallShieldMSI) then
      get_installshieldmsi_info(FileName)
    else if (setupType = stAdvancedMSI) then
      get_advancedmsi_info(FileName)
    else begin
      resultForm1.PageControl1.ActivePage := resultForm1.TabSheetAnalyze;
      analyze_binary(FileName, true, false); // filename, verbose, skipzero
      analyze_binary(FileName, true, true); // filename, verbose, skipzero
      mywrite('unknown Setup Type.');   /// XXX Probe-Installation anbieten
    end
  end
end;





end.

