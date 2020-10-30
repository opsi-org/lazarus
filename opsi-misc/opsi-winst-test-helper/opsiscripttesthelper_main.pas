unit opsiscripttesthelper_main;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, //ExtCtrls,
  fptimer,
  {$IFDEF WINDOWS}
  //osfuncwin,
  JwaWinnt,
  jwawinbase,
  JwaWindows,
  JwaUserEnv,
  dsiwin32,
  helperwispecfolder,
  {$ENDIF WINDOWS}
  {$IFDEF UNIX}
  osfunclin,
  lispecfolder,
  {$ENDIF UNIX}

  {$IFDEF GUI}
  Forms,
  {$ELSE}
  custapp,
  {$ENDIF GUI}
  Process,
  osversioninfo,
  oslog,
  lazfileutils;

//type

{ TDataModule1 }

  (*
  TDataModule1 = class(TObject)
    Timer1: TfpTimer;
    Application : TCustomApplication;
    procedure Timer1Timer(Sender: TObject);

  private

  public

  end;
  *)



procedure main();
procedure Timer1Timer(Sender: TObject);

var
  //DataModule1: TDataModule1;
  ErrorMsg: string;
  optionlist: TStringList;
  myexitcode: integer;
  myfile: file;
  mysizemb: integer = 5;
  myfilename: string;
  myblock: array[1..1024] of byte;
  i, k: integer;
  mybyte: byte;
  //mylog: textfile;
  mydefaultlog: string;
  Timer1: TfpTimer;
  {$IFDEF GUI}
  Application: TApplication;
  {$ELSE}
  Application: TCustomApplication;
  {$ENDIF GUI}

  mytimerevent: TMethod;
//   procedure Timer1Timer(Sender: TObject);

implementation

{$IFDEF GUI}
uses
  helperwin;

{$ENDIF GUI}

{$IFDEF WINDOWS}
function IsElevated: boolean;
const
  TokenElevation = TTokenInformationClass(18);
  //TokenElevation = TTokenInformationClass;

type
  TOKEN_ELEVATION = record
    TokenIsElevated: DWORD;
  end;
var
  TokenHandle: THandle;
  ResultLength: cardinal;
  ATokenElevation: TOKEN_ELEVATION;
begin
  //if (IsWinVista or IsWinServer2008 or IsWin7 or IsWinServer2008R2) then
  begin
    Result := False;
    TokenHandle := 0;
    if OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, TokenHandle) then
    begin
      try
        ResultLength := 0;
        if GetTokenInformation(TokenHandle, TTokenInformationClass(18),
          @ATokenElevation, SizeOf(ATokenElevation), ResultLength) then
          Result := ATokenElevation.TokenIsElevated <> 0
        else
          Result := False;
      finally
        CloseHandle(TokenHandle);
      end;
    end
    else
      Result := False;
  end;
  //else
  //  Result := true; //IsAdministrator;
end;


function GetNetUser(Host: string; var UserName: string; var ErrorInfo: string): boolean;
  (* for Host = '' Username will become the name of the current user of the process *)

var
  pLocalName: PChar;
  pUserName: PChar;


  function ApiCall(var Username, ErrorInfo: string; BuffSize: DWord): boolean;
  var
    errorcode: DWord;
    nBuffSize: DWord;
    pErrorBuff, pNameBuff: PChar;
    nErrorBuffSize: DWord = 0;
    nNameBuffSize: DWord = 0;

  begin
    Result := False;
    GetMem(pUserName, BuffSize);
    nBuffSize := Buffsize;

    UserName := '';
    errorCode := WNetGetUser(nil, pUserName, nBuffSize);


    case errorCode of
      no_error:
      begin
        ErrorInfo := '';
        SetLength(UserName, StrLen(pUserName));
        UserName := pUserName;
        Result := True;
      end;
      ERROR_NOT_CONNECTED: ErrorInfo :=
          'The device specified by lpszLocalName is not a redirected device or a connected network name.';
      ERROR_MORE_DATA: ApiCall(UserName, ErrorInfo, nBuffSize + 1);
      ERROR_NO_NETWORK: ErrorInfo := 'No network is present.';
      ERROR_EXTENDED_ERROR:
      begin
        GetMem(pErrorBuff, 300);
        GetMem(pNameBuff, 300);
        WNetGetLastError(errorcode, pErrorBuff, nErrorBuffSize,
          pNameBuff, nNameBuffSize);
        ErrorInfo := pErrorBuff;
        FreeMem(pErrorBuff);
        FreeMem(pNameBuff);
      end;
      ERROR_NO_NET_OR_BAD_PATH: ErrorInfo :=
          'None of the providers recognized this local name as having a connection. '
          +
          'However, the network is not available for at least one provider to whom the connection may belong';
      else
        errorInfo := 'NT-Error ' + SysErrorMessage(errorCode);
    end;

    if errorCode <> no_error then
      errorInfo := IntToStr(errorCode) + ' ' + errorInfo;

    FreeMem(pUserName);
  end;

begin
  if Host <> '' then
    pLocalName := PChar(Host)
  else
    pLocalName := nil;

  if ApiCall(Username, ErrorInfo, 100) then
    Result := True
  else
    Result := False;
end;

{$ENDIF WINDOWS}


procedure WriteHelp;
var
  filename: string;
begin
  filename := ExtractFileName(ParamStr(0));
  writeln(ParamStr(0));
  writeln(filename);
  writeln('Usage:');
  writeln(filename + ' Option [Option]');
  writeln('Options:');
  writeln(' --help -> write this help and exit');
  writeln(' --exit-code=n -> exits with n as exit code (default 42)');
  //writeln(' --log=<path\filename> -> writes log to <path\filename> (c:\opsi.org\tmp\opsiwinsttesthelper.log)');
  writeln(' --time-output -> write timestamp and exit');
  writeln(' --fork-and-stop=n -> starts a helperchild(.exe) and exits.');
  writeln('                     The child process waits 2 seconds, then shows a window for n seconds and stops');
  writeln(' --showwindow=n -> shows a window for n seconds and exit');
  writeln(' --version -> write version info and exit');
  writeln(' --wait=n -> waits n seconds before going on');
  writeln(' --createfile=fname -> creates a file fname');
  writeln(' --filesize=n -> fills the file to the size of n MB');
end;

procedure writeTimestamp;
begin
  writeln(DateTimeToStr(now));
end;


procedure startchild(childsec: integer);
var
  AProcess: TProcess;
  mychildname: string;
begin
  AProcess := TProcess.Create(nil);
  //AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
  { do not try to wait for the child }
  AProcess.Options := [];
  {$IFDEF WINDOWS}
  mychildname := 'helperchild.exe';
  {$ENDIF WINDOWS}
  {$IFDEF UNIX}
  {$IFDEF GUI}
  mychildname := 'helperchild';
  {$ELSE}
  mychildname := 'helperchild_nogui';
  {$ENDIF GUI}
  {$ENDIF UNIX}
  AProcess.Executable := ExtractFilePath(ParamStr(0)) + mychildname;
  LogDatei.log('will start executable ' + AProcess.Executable, LLnotice);
  AProcess.Parameters.Add('--wait=2');
  AProcess.Parameters.Add('--showwindow=' + IntToStr(childsec));
  LogDatei.log('with params :' + AProcess.Parameters.Text, LLnotice);
  try
    AProcess.Execute;
  except
    LogDatei.log('Error starting: ' + AProcess.Executable, LLerror);
  end;
  {$IFDEF GUI}
  Application.ProcessMessages;
  {$ENDIF GUI}
end;

procedure main();
var
  paramvaluestr, mystr, dummystr: string;
  waitsec, childsec: integer;
  myparamstring: string = '';
  myparamcount, i: integer;
  showtimestr: string;
  showtimeint: integer;
  lfilename: string;

begin
  // Initialize logging
  LogDatei := TLogInfo.Create;
  lfilename := ExtractFileName(Application.ExeName);
  lfilename := ExtractFileNameOnly(lfilename);
  LogDatei.FileName := lfilename;
  LogDatei.StandardLogFileext := '.log';
  LogDatei.StandardLogFilename := lfilename;
  LogDatei.WritePartLog := False;
  LogDatei.CreateTheLogfile(lfilename + '.log', True);
  LogDatei.log('Log for: ' + Application.exename + ' opend at : ' +
    DateTimeToStr(now), LLEssential);
  LogDatei.log(lfilename + ' Version: ' + getversioninfo, LLEssential);
  LogDatei.LogLevel := 7;

      {$IFDEF WINDOWS}
  GetNetUser('', mystr, dummystr);
  LogDatei.log('Running as = ' + mystr, LLEssential);
  if DSiIsAdmin then
    LogDatei.log('Running as admin = true', LLEssential)
  else
    LogDatei.log('Running as admin = false', LLEssential);
  if GetNTVersionMajor >= 6 then
  begin
    LogDatei.log('Running on = nt6', LLEssential);
    if IsElevated then
      LogDatei.log('Running elevated on nt6 = true', LLEssential)
    else
      LogDatei.log('Running elevated on nt6 = false', LLEssential);
  end
  else
    LogDatei.log('Running on = nt5', LLEssential);
  retrieveFoldersFromWinApi;
  LogDatei.log('Appdata is = ' + GetAppDataPath, LLEssential);
  LogDatei.log('Desktop is = ' + GetDesktopPath, LLEssential);
      {$ENDIF WINDOWS}
  LogDatei.log('current profile is = ' + GetUserProfilePath, LLEssential);

  myparamcount := ParamCount;
  myparamcount := Application.ParamCount;
  //writeln('paramcount = '+inttostr(myparamcount));
  for i := 1 to myparamcount do
    myparamstring := myparamstring + ' ' + Application.Params[i];
  LogDatei.log('Called as: ' + myparamstring, LLEssential);
  myexitcode := 42;
  system.ExitCode := myexitcode;
  optionlist := TStringList.Create;
  optionlist.Append('help');
  optionlist.Append('log::');
  optionlist.Append('exit-code::');
  optionlist.Append('time-output');
  optionlist.Append('fork-and-stop::');
  optionlist.Append('showwindow::');
  optionlist.Append('version');
  optionlist.Append('wait:');
  optionlist.Append('createfile::');
  optionlist.Append('filesize:');
  // quick check parameters
  // with DataModule1 do
  begin
    ErrorMsg := Application.CheckOptions('', optionlist);
    if ErrorMsg <> '' then
    begin
      ErrorMsg := ErrorMsg + ' with params: ' + myparamstring;
      LogDatei.log(ErrorMsg, LLcritical);
      Application.ShowException(Exception.Create(ErrorMsg));
      LogDatei.log('Terminating with exitcode' + IntToStr(system.ExitCode), LLessential);
      Application.Terminate;
      Exit;
    end;

    // parse parameters
    if Application.HasOption('help') or (paramcount = 0) then
    begin
      //if Application.HasOption('help')  then begin
      WriteHelp;
      LogDatei.log('Terminating with exitcode' + IntToStr(system.ExitCode), LLessential);
      Application.Terminate;
      halt(system.ExitCode);
    end;

    if Application.HasOption('exit-code') then
    begin
      paramvaluestr := Application.GetOptionValue('exit-code');
      try
        myexitcode := StrToInt(paramvaluestr);
        system.ExitCode := myexitcode;
        writeln('Will use ' + paramvaluestr + ' as exit code.');
        LogDatei.log('Will use ' + paramvaluestr + ' as exit code.', LLNotice);
      except
        writeln('>' + paramvaluestr + '< is not a integer. Using default of ' +
          IntToStr(myexitcode) + ' as exit code.');
        LogDatei.log('>' + paramvaluestr + '< is not a integer. Using default of ' +
          IntToStr(myexitcode) + ' as exit code.', LLerror);
      end;
    end;

    if Application.HasOption('log') then
    begin
      LogDatei.log('Option --log is not supportetd any more', LLwarning);
    end;
(*
    if Application.HasOption('log') then
    begin
      mydefaultlog := 'c:\opsi.org\log\opsiscripttesthelper.log';
      paramvaluestr := Application.GetOptionValue('log');
      try
        Assignfile(mylog, paramvaluestr);
        rewrite(mylog);
        writeln('Will use ' + paramvaluestr + ' as log file.');
      except
        writeln('>' + paramvaluestr + '< is not a filename. Using default of ' +
          mydefaultlog + ' as logfile.');
        try
          Assignfile(mylog, mydefaultlog);
          rewrite(mylog);
          writeln('Will use ' + mydefaultlog + ' as log file.');
        except
          writeln('Failed to create log file.');
          halt(myexitcode);
          Application.Terminate;
        end;
      end;
      {$IFDEF WINDOWS}
      GetNetUser('', mystr, dummystr);
      writeln(mylog, 'Running as = ' + mystr);
      if DSiIsAdmin then
        writeln(mylog, 'Running as admin = true')
      else
        writeln(mylog, 'Running as admin = false');
      if GetNTVersionMajor = 6 then
      begin
        writeln(mylog, 'Running on = nt6');
        if IsElevated then
          writeln(mylog, 'Running elevated on nt6 = true')
        else
          writeln(mylog, 'Running elevated on nt6 = false');
      end
      else
        writeln(mylog, 'Running on = nt5');
      retrieveFoldersFromWinApi;
      writeln(mylog, 'Appdata is = ' + GetAppDataPath);
      writeln(mylog, 'Desktop is = ' + GetDesktopPath);
      {$ENDIF WINDOWS}
      writeln(mylog, 'current profile is = ' + GetUserProfilePath);
      closefile(mylog);
    end;
*)


    if Application.HasOption('wait') then
    begin
      paramvaluestr := Application.GetOptionValue('wait');
      //writeln('--wait: waiting ' + paramvaluestr + ' seconds');
      LogDatei.log('--wait: waiting ' + paramvaluestr + ' seconds', LLnotice);
      try
        waitsec := StrToInt(paramvaluestr);
      except
        LogDatei.log('wait >' + paramvaluestr + '< is not a integer.', LLerror);
        //writeln('>' + paramvaluestr + '< is not a integer.');
        waitsec := 1;
      end;
      Sleep(waitsec * 1000);
      LogDatei.log('Finished waiting', LLnotice);
    end;

    if Application.HasOption('fork-and-stop') then
    begin
      paramvaluestr := Application.GetOptionValue('fork-and-stop');
      LogDatei.log('Will use ' + paramvaluestr + ' as fork-and-stop.', LLnotice);
      writeln('Will use ' + paramvaluestr + ' as fork-and-stop.');
      try
        childsec := StrToInt(paramvaluestr);
      except
        LogDatei.log('fork-and-stop >' + paramvaluestr + '< is not a integer.', LLerror);
        writeln('>' + paramvaluestr + '< is not a integer.');
        childsec := 5;
      end;
      try
        startchild(childsec);
      except
        LogDatei.log('Failed to start child.', LLerror);
      end;
      LogDatei.log('Terminating with exitcode' + IntToStr(system.ExitCode), LLessential);
      Application.Terminate;
      halt(system.ExitCode);
    end;


    if Application.HasOption('time-output') then
    begin
      writeTimestamp;
      LogDatei.log('Terminating with exitcode' + IntToStr(system.ExitCode), LLessential);
      Application.Terminate;
      halt(system.ExitCode);
    end;

    if Application.HasOption('version') then
    begin
      writeln('Version: ' + getversioninfo);
      LogDatei.log('Terminating with exitcode' + IntToStr(system.ExitCode), LLessential);
      Application.Terminate;
      halt(system.ExitCode);
    end;

    if Application.HasOption('createfile') then
    begin
      myfilename := Application.GetOptionValue('createfile');
      writeln('Will use ' + myfilename + ' as file name.');
      if Application.HasOption('filesize') then
      begin
        paramvaluestr := Application.GetOptionValue('filesize');
        try
          mysizemb := StrToInt(paramvaluestr);
          writeln('Will use ' + paramvaluestr + ' as file size.');
        except
          writeln('>' + paramvaluestr + '< is not a integer. Using default of ' +
            IntToStr(mysizemb) + ' as file size.');
        end;
        k := 0;
        for i := 1 to 4 do
        begin
          for mybyte := 0 to 255 do
          begin
            k := k + 1;
            myblock[k] := mybyte;
          end;
        end;
        Assignfile(myfile, myfilename);
        rewrite(myfile, 1024);
        for i := 1 to mysizemb * 1024 do
          blockwrite(myfile, myblock, 1);
        closefile(myfile);
      end;
      LogDatei.log('Terminating with exitcode' + IntToStr(system.ExitCode), LLessential);
      Application.Terminate;
      halt(system.ExitCode);
    end;


    if Application.HasOption('showwindow') then
    begin
      {$IFDEF GUI}
      Application.CreateForm(TForm1, Form1);
      Form1.Caption := Application.Title;
      Form1.Show;
     {$ENDIF GUI}
      showtimestr := Application.GetOptionValue('showwindow');
      try
        showtimeint := StrToInt(showtimestr);
      except
        //writeln('<' + showtimestr + '< is not a integer. Using default of 1 second.');
        LogDatei.log('>' + paramvaluestr +
          '< is not a integer. Using default of 1 second.', LLerror);
        showtimeint := 1;
      end;
      //writeln('--showwindow: waiting ' + showtimestr + ' seconds');
      LogDatei.log('--showwindow: waiting ' + showtimestr + ' seconds', LLnotice);
      //Sleep(showtimeint * 1000);


      timer1.Interval := showtimeint * 1000;
      timer1.Enabled := True;

      //Application.ProcessMessages;
      //Form1.Show;
      //Application.ProcessMessages;
    end;


    { add your program here }

    // stop program loop
    //Application.Terminate;
    if not (Application.HasOption('showwindow')) then
    begin
      LogDatei.log('Terminating with exitcode' + IntToStr(system.ExitCode), LLessential);
      Application.Terminate;
      halt(system.ExitCode);
    end;
  end;
end;

{ TDataModule1 }

//procedure(Sender: TObject) of object;
//procedure Timer1Timer(Sender: TObject) of object;
procedure Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
{$IFDEF GUI}
  form1.Visible := False;
{$ENDIF GUI}
  LogDatei.log('Finished waiting (timer)', LLnotice);
  LogDatei.log('Terminating with exitcode' + IntToStr(system.ExitCode), LLessential);
  Application.Terminate;
  halt(system.ExitCode);
end;

initialization



  mytimerevent.Code := @Timer1Timer;
  mytimerevent.Data := nil;
  timer1 := TFPTimer.Create(nil);
  timer1.Interval := 1000;
  timer1.OnTimer := TNotifyEvent(mytimerevent);
  //timer1.StartTimer;

end.
