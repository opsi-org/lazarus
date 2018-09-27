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
  Process,
  osdhelper,
  oslog,
  osdbasedata,
  Dialogs;

procedure createProductStructure;
procedure removeOtherTypeSpecificSections(setupType, setupFile: string);

implementation

uses
  resultform;

var
  patchlist: TStringList;
  myExeDir: string;

  function createClientFiles : boolean;
  begin

  end;

  function createOpsiFiles : boolean;
  begin

  end;

function createProductdirectory : boolean;
var
  prodpath, clientpath, opsipath: string;
  goon: boolean;
begin
  prodpath := myconfiguration.workbench_Path + aktProduct.produktpropties.productId;
  clientpath := prodpath + PathDelim + 'CLIENT_DATA';
  opsipath := prodpath + PathDelim + 'OPSI';
  goon := True;
  if DirectoryExists(prodpath) then
    if MessageDlg('opsi-setup-detector','Directory ' + prodpath + ' still exits. Overwrite ?', mtWarning, mbOKCancel,'') = mrOk then
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
      mywrite('patchScript file error: ' + E.ClassName + '/' + E.Message);
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


procedure ButtonCreatePacketClick(Sender: TObject);
var
  msg1: string;
  description: string;
  buildCall: string = '';
  OpsiBuilderProcess: TProcess;
  packit: boolean = False;
  errorstate: boolean = False;
  notused: string = '(not used)';

begin

  if patchlist = nil then
    patchlist := TStringList.Create
  else
    patchlist.Clear;
  myExeDir := ExtractFileDir(ParamStr(0));


  //Panel9.Visible := False;
      {$IFDEF WINDOWS}
  // execute opsiPacketBuilder

  if resultForm1.RadioButtonCreateOnly.Checked = True then
  begin
    //Application.MessageBox(PChar(sInfoFinished), PChar(sMBoxHeader), MB_OK);
  end
  else
  begin  // execute OPSIPackageBuilder
    OpsiBuilderProcess := process.TProcess.Create(nil);
    buildCall := getSpecialFolder(CSIDL_PROGRAM_FILES) + DirectorySeparator +
      'OPSI PackageBuilder' + DirectorySeparator + 'OPSIPackageBuilder.exe' +
      ' -p=' + packetBaseDir;
    if AnsiLastChar(packetBaseDir) <> DirectorySeparator then
      buildCall := buildCall + DirectorySeparator;
    buildCall := buildCall + productId;
    if resultForm1.RadioButtonInteractive.Checked = True then
    begin
      OpsiBuilderProcess.ShowWindow := swoMinimize;
    end
    else  // auto
    if resultForm1.RadioButtonAuto.Checked = True then
    begin
      if resultForm1.CheckboxBuild.Checked = True then
        buildCall := buildCall + ' --build=rebuild';
      if resultForm1.CheckboxInstall.Checked = True then
        buildCall := buildCall + ' --install';
      if resultForm1.CheckboxQuiet.Checked = True then
        buildCall := buildCall + ' --quiet';
      OpsiBuilderProcess.ShowWindow := swoMinimize;
    end;

    //mywrite('invoke opsi package builder');
    //mywrite(buildcall);

    try
      OpsiBuilderProcess.CommandLine := buildCall;
      OpsiBuilderProcess.Execute;
      if resultForm1.CheckboxQuiet.Checked = True then
      begin
        resultForm1.Panel9.Visible := True;
        resultForm1.processStatement.Caption := 'invoke opsi package builder ...';
        //Application.ProcessMessages;
        while OpsiBuilderProcess.Running do
        begin
          //Application.ProcessMessages;
        end;
      end;
    except
      errorstate := True;
      //Application.MessageBox(PChar(sErrOpsiPackageBuilderStart),
      //        PChar(sMBoxHeader), MB_OK);
    end;

    resultForm1.Panel9.Visible := False;
    if (resultForm1.CheckboxQuiet.Checked = True) then
    begin
      if (errorstate = False) then
      begin
        if (OpsiBuilderProcess.ExitStatus = 0) then
        //Application.MessageBox(PChar(sInfoFinished), PChar(sMBoxHeader), MB_OK)
        else;
        //Application.MessageBox(
        //  PChar(format(sErrOpsiPackageBuilderErrCode,
        //  [IntToStr(OpsiBuilderProcess.ExitStatus)])),
        //  PChar(sMBoxHeader), MB_OK);
      end;
    end;
  end;   // execute OPSIPackageBuilder
      {$ENDIF WINDOWS}
end;

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




end.
