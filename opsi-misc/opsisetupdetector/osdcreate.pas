unit osdcreate;

{$mode delphi}

interface

uses
  Classes, SysUtils;

procedure createProductStructure;

implementation

procedure createProductStructure;
begin

end;

procedure TResultform1.BitBtnOpenFileClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    aktSetupFile.setupFullFileName := OpenDialog1.FileName;
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


procedure TResultform1.ButtonCreatePacketClick(Sender: TObject);
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


  Panel9.Visible := False;
      {$IFDEF WINDOWS}
  // execute opsiPacketBuilder

  if RadioButtonCreateOnly.Checked = True then
  begin
    Application.MessageBox(PChar(sInfoFinished), PChar(sMBoxHeader), MB_OK);
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
    if RadioButtonInteractive.Checked = True then
    begin
      OpsiBuilderProcess.ShowWindow := swoMinimize;
    end
    else  // auto
    if RadioButtonAuto.Checked = True then
    begin
      if CheckboxBuild.Checked = True then
        buildCall := buildCall + ' --build=rebuild';
      if CheckboxInstall.Checked = True then
        buildCall := buildCall + ' --install';
      if CheckboxQuiet.Checked = True then
        buildCall := buildCall + ' --quiet';
      OpsiBuilderProcess.ShowWindow := swoMinimize;
    end;

    mywrite('invoke opsi package builder');
    mywrite(buildcall);

    try
      OpsiBuilderProcess.CommandLine := buildCall;
      OpsiBuilderProcess.Execute;
      if CheckboxQuiet.Checked = True then
      begin
        Panel9.Visible := True;
        processStatement.Caption := 'invoke opsi package builder ...';
        Application.ProcessMessages;
        while OpsiBuilderProcess.Running do
        begin
          Application.ProcessMessages;
        end;
      end;
    except
      errorstate := True;
      Application.MessageBox(PChar(sErrOpsiPackageBuilderStart),
        PChar(sMBoxHeader), MB_OK);
    end;

    Panel9.Visible := False;
    if (CheckboxQuiet.Checked = True) then
    begin
      if (errorstate = False) then
      begin
        if (OpsiBuilderProcess.ExitStatus = 0) then
          Application.MessageBox(PChar(sInfoFinished), PChar(sMBoxHeader), MB_OK)
        else
          Application.MessageBox(
            PChar(format(sErrOpsiPackageBuilderErrCode,
            [IntToStr(OpsiBuilderProcess.ExitStatus)])),
            PChar(sMBoxHeader), MB_OK);
      end;
    end;
  end;   // execute OPSIPackageBuilder
      {$ENDIF WINDOWS}

      procedure TResultform1.removeOtherTypeSpecificSections(setupType, setupFile: string);
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
            mywrite('removeOtherTypeSpecificSections error: ' + E.ClassName + '/' + E.Message);
        end;
      end;



  //  end;
end;


end.

