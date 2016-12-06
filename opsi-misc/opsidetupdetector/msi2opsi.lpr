program msi2opsi;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  VersionInfoX,
  CustApp,
  Process,
  FileUtil,
  Windows { you can add units after this };

type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

var
  optionlist: TStringList;
  myexitcode: integer;
  myfilename, myerror, productid: string;
  patchlist: TStringList;
  mybasedir: string;
  myexedir : string;

  procedure mywrite(line: string);
  begin
    writeln(line);
  end;


  procedure patchScript(infile, outfile: string);
  var
    infileHandle, outfileHandle: Text;
    aktline: string;
    i : integer;
  begin
    mywrite('creating: '+outfile+' from: '+infile);
    AssignFile(infileHandle, infile);
    AssignFile(outfileHandle, outfile);
    reset(infileHandle);
    Rewrite(outfileHandle);
    while not EOF(infileHandle) do
    begin
      ReadLn(infileHandle, aktline);
      for i := 0 to patchlist.Count - 1 do
        aktline := StringReplace(aktline, patchlist.Names[i], patchlist.ValueFromIndex[i], [rfReplaceAll,rfIgnoreCase]);
      writeln(outfileHandle,aktline);
    end;
    CloseFile(infileHandle);
    CloseFile(outfileHandle);
  end;


  procedure createProduct;
  var
    i: integer;
    productpath, datapath, opsipath, mydatapath, myopsipath : string;
  begin
    for i := 0 to patchlist.Count - 1 do
      mywrite(patchlist.Strings[i]);
    productpath := mybasedir+DirectorySeparator+productid;
    opsipath := productpath +DirectorySeparator+'OPSI';
    datapath := productpath +DirectorySeparator+'CLIENT_DATA';
    myopsipath := myexedir +DirectorySeparator+'msi2opsi-files'+DirectorySeparator+'OPSI';
    mydatapath := myexedir +DirectorySeparator+'msi2opsi-files'+DirectorySeparator+'CLIENT_DATA';
    if not DirectoryExists(productpath) then createdir(productpath);
    if not DirectoryExists(productpath) then
      mywrite('Error: coud not create directory: '+productpath)
    else
    begin
      mywrite(datapath);
      if not DirectoryExists(datapath) then createdir(datapath);
      mywrite(opsipath);
      if not DirectoryExists(opsipath) then createdir(opsipath);
      patchScript(myopsipath+DirectorySeparator+'control',opsipath+DirectorySeparator+'control');
      patchScript(mydatapath+DirectorySeparator+'setup32.opsiscript',datapath+DirectorySeparator+'setup32.opsiscript');
      patchScript(mydatapath+DirectorySeparator+'delsub32.opsiscript',datapath+DirectorySeparator+'delsub32.opsiscript');
      patchScript(mydatapath+DirectorySeparator+'uninstall32.opsiscript',datapath+DirectorySeparator+'uninstall32.opsiscript');
      copyFile(pchar(myfilename), pchar(datapath+DirectorySeparator+ExtractFileName(myfilename)), False);
      copyFile(pchar(mydatapath+DirectorySeparator+'check_msi_exitcode.opsiscript'), pchar(datapath+DirectorySeparator+'check_msi_exitcode.opsiscript'), False);
      copyFile(pchar(mydatapath+DirectorySeparator+'opsi-template.png'), pchar(datapath+DirectorySeparator+productid+'.png'), False);
    end;
  end;


  procedure grepmsi(instring: string);
  begin
    if (0 < pos('product_build_number{', lowercase(instring))) or
      (0 < pos('productcode{', lowercase(instring))) then
      mywrite(instring);
  end;

  function RunCommandAndCaptureOut
    (cmd: string; catchOut: boolean; var outlines: TStringList;
  var report: string; showcmd: integer; var ExitCode: longint): boolean;
  const
    ReadBufferSize = 2048;

  var
    //myStringlist : TStringlist;
    S: TStringList;
    M: TMemoryStream;
    FpcProcess: TProcess;
    n: longint;
    BytesRead: longint;

  begin

    Result := True;
    try
      try
        M := TMemoryStream.Create;
        BytesRead := 0;
        FpcProcess := process.TProcess.Create(nil);
        FpcProcess.CommandLine := cmd;
        FpcProcess.Options := [poUsePipes, poStderrToOutput];
        FpcProcess.ShowWindow := swoMinimize;
        FpcProcess.Execute;
        //mywrite('RunCommandAndCaptureOut: started: ' + cmd);
        while FpcProcess.Running do
        begin
          // stellt sicher, dass wir Platz haben
          M.SetSize(BytesRead + ReadBufferSize);

          // versuche, es zu lesen
          if FpcProcess.Output.NumBytesAvailable > 0 then
          begin
            n := FpcProcess.Output.Read((M.Memory + BytesRead)^, ReadBufferSize);
            //mywrite('RunCommandAndCaptureOut: read: ' + IntToStr(n) + ' bytes');
            if n > 0 then
            begin
              Inc(BytesRead, n);
              //mywrite('RunCommandAndCaptureOut: read: ' + IntToStr(n) + ' bytes');
              //Write('.')
            end;
          end
          else
          begin
            // keine Daten, warte 100 ms
            //mywrite('RunCommandAndCaptureOut: no data - waiting....');
            Sleep(100);
          end;
        end;
        // lese den letzten Teil
        repeat
          // stellt sicher, dass wir Platz haben
          M.SetSize(BytesRead + ReadBufferSize);
          if FpcProcess.Output.NumBytesAvailable > 0 then
          begin
            // versuche es zu lesen
            n := FpcProcess.Output.Read((M.Memory + BytesRead)^, ReadBufferSize);
            if n > 0 then
            begin
              Inc(BytesRead, n);
              //mywrite('RunCommandAndCaptureOut: read: ' + IntToStr(n) + ' bytes');
              //Write('.');
            end;
          end
          else
            n := 0;
        until n <= 0;
        //if BytesRead > 0 then WriteLn;
        M.SetSize(BytesRead);
        //mywrite('RunCommandAndCaptureOut: -- executed --');
        //WriteLn('-- executed --');

        S := TStringList.Create;
        S.LoadFromStream(M);
        //mywrite('RunCommandAndCaptureOut: -- linecount = ' + IntToStr(S.Count));
        //WriteLn('-- linecount = ', S.Count, ' --');
        for n := 0 to S.Count - 1 do
        begin
          //WriteLn('| ', S[n]);
          outlines.Add(S[n]);
        end;
        //WriteLn('-- end --');
      except
        on e: Exception do
        begin
          Mywrite('Exception in RunCommandAndCaptureOut: ' + e.message);
          Result := False;
        end;
      end;
    finally
      S.Free;
      FpcProcess.Free;
      M.Free;
    end;
  end;


  function getmsidetails(myfilename: string): boolean;
  var
    myoutlines: TStringList;
    myreport: string;
    myexitcode: integer;
    i: integer;
    fsize: int64;
    fsizemb: integer;
    patchstr: string;

  begin
    Result := False;
    myoutlines := TStringList.Create;
    Mywrite('Analyzing: ' + myfilename);

    if not RunCommandAndCaptureOut('cmd.exe /C "cscript.exe ' + ExtractFilePath(
      ParamStr(0)) + '\msiinfo.js ' + myfilename, True, myoutlines, myreport,
      SW_SHOWMINIMIZED, myexitcode) then
    begin
      mywrite('Failed to analyze: ' + myreport);
    end
    else
    begin
      Result := True;
      for i := 0 to myoutlines.Count - 1 do
      begin
        mywrite(myoutlines.Strings[i]);
        if 0 <> pos('Manufacturer', myoutlines.Strings[i]) then
        begin
          patchlist.Add('#@#-PRODUCTDESC-#@#=' + trim(myoutlines.Strings[i]));
        end;
        if 0 <> pos('ProductName', myoutlines.Strings[i]) then
        begin
          patchstr := copy(myoutlines.Strings[i], pos(': ', myoutlines.Strings[i]) + 1,
            length(myoutlines.Strings[i]));
          patchlist.Add('#@#-PRODUCTNAME-#@#=' + trim(patchstr));
        end;
        if 0 <> pos('ProductVersion', myoutlines.Strings[i]) then
        begin
          patchstr := copy(myoutlines.Strings[i], pos(': ', trim(myoutlines.Strings[i])) + 1,
            length(myoutlines.Strings[i]));
          patchlist.Add('#@#-PRODUCTVER-#@#=' + trim(patchstr));
        end;
        if 0 <> pos('ProductCode', myoutlines.Strings[i]) then
        begin
          patchstr := copy(myoutlines.Strings[i], pos(': ', myoutlines.Strings[i]) + 1,
            length(myoutlines.Strings[i]));
          patchlist.Add('#@#-MSIID-#@#=' + trim(patchstr));
        end;
      end;
      fsize := fileutil.FileSize(myfilename);
      fsizemb := round(fsize / (1024 * 1024));
      patchlist.Add('#@#-PRODUCTSIZE-#@#=' + IntToStr(fsizemb * 6));
      patchlist.Add('#@#-INSTALLERFILE-#@#=' + ExtractFileName(myfilename));
    end;
    myoutlines.Free;
  end;

(*
MSI file: C:\temp\Halite.0_3_3_dev1205.x86.msi
Manufacturer: BinaryNotions.com
ProductName: Halite
ProductVersion: 0.3.3
ProductCode: {E0AF5EFE-5971-4A54-A69F-D2D95E9E5363}
UpgradeCode: {6981C8F9-F2FF-49BB-9335-BDFD9B7B635F}
MSI file size is: 1,9 MB
Estimated required space is: 11,3 MB
*)

  { TMyApplication }

  procedure TMyApplication.DoRun;
  var
    ErrorMsg: string;
  begin
    myexedir := ExtractFileDir(Params[0]);
    patchlist := TStringList.Create;
    optionlist := TStringList.Create;
    optionlist.Append('help');
    optionlist.Append('filename::');
    optionlist.Append('productid:');
    optionlist.Append('basedir:');

    // quick check parameters
    ErrorMsg := CheckOptions('', optionlist);
    if ErrorMsg <> '' then
    begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;


    if HasOption('filename') then
    begin
      myfilename := trim(GetOptionValue('filename'));
      // check if we can use the file
      if not FileExists(myfilename) then
      begin
        myerror := 'Error: Given filename: ' + myfilename + ' does not exist.';
        WriteHelp;
      end;
      if not ('.msi' = lowercase(ExtractFileExt(myfilename))) then
      begin
        myerror := 'Error: Given filename: ' + myfilename + ' has no msi extension.';
        WriteHelp;
      end;
    end
    else
    begin
      myerror := 'Error: No filename given';
      WriteHelp;
    end;

    if HasOption('productid') then
    begin
      productid := trim(GetOptionValue('productid'));
      // todo: check is valid
    end
    else
    begin
      // no product id given
      productid := ExtractFileNameWithoutExt(myfilename);
    end;

    if HasOption('basedir') then
    begin
      mybasedir := trim(GetOptionValue('basedir'));
      if not DirectoryExists(mybasedir) then
      begin
        myerror := 'Error: Given basedir: '+mybasedir+' does not exist';
        WriteHelp;
      end;
    end
    else
    begin
      // no product id given
      mybasedir :=  myexedir;
    end;

    patchlist.Add('#@#-PRODUCTID-#@#=' + productid);
    patchlist.Add('#@#-TIMESTAMP-#@#=' + DateTimeToStr(Now));
    if getmsidetails(myfilename) then
      createProduct
    else
      mywrite('Failed to analyze : ' + myfilename);

    // stop program loop
    Terminate;
  end;

  constructor TMyApplication.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor TMyApplication.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TMyApplication.WriteHelp;
  var
    progname: string;
    verinfo: TVersioninfo;
  begin
    verinfo := TVersioninfo.Create(ParamStr(0));
    progname := ExtractFileName(ParamStr(0));
    writeln(ParamStr(0));
    writeln(progname);
    writeln('Version ' + verinfo.getString('FileVersion'));
    writeln(myerror);
    writeln('Usage:');
    writeln(progname + '[Options]');
    writeln('Options:');
    writeln(' --help -> write this help and exit');
    writeln(' --filename=<path\filename> -> msi file)');
    writeln(' --productid=<opsi procutId> -> opsi productid ; if not given we try to use the filename)');
    writeln(' --basedir=<opsi product base directory> -> Path where the product will be created ; actual path if not given)');
    verinfo.Free;
    Terminate;
    halt(-1);
    Exit;
  end;


var
  Application: TMyApplication;

{$R *.res}

begin
  Application := TMyApplication.Create(nil);
  Application.Title := 'msi to opsi';
  Application.Run;
  Application.Free;
end.
