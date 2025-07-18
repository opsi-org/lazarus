unit OSProcessux;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF OPSISCRIPT}
  ostxstringlist,
  {$IFDEF GUI}
  Graphics,
  Forms,
  {$ENDIF GUI}
  {$ENDIF OPSISCRIPT}
  oslog,
  Process,
  Classes,
  SysUtils;

const
  { ShowWindow  }
  SW_HIDE = 0;
  SW_MAXIMIZE = 3;
  SW_MINIMIZE = 6;
  SW_NORMAL = 1;
  SW_RESTORE = 9;
  SW_SHOW = 5;
  SW_SHOWDEFAULT = 10;
  SW_SHOWMAXIMIZED = 3;
  SW_SHOWMINIMIZED = 2;
  SW_SHOWMINNOACTIVE = 7;
  SW_SHOWNA = 8;
  SW_SHOWNOACTIVATE = 4;
  SW_SHOWNORMAL = 1;


{$IFDEF OPSISCRIPT}
function RunCommandCaptureOutGetOutlist(command: string): TXStringlist;
function RunCommandCaptureOutGetExitcode(command: string): longint;

function RunCommandAndCaptureOut
  (cmd: string; catchOut: boolean; var outlines: TXStringList;
  var report: string; showcmd: integer; var ExitCode: longint;
  showoutput: boolean = False; logleveloffset: integer = 0): boolean;

{$ELSE OPSISCRIPT}
function RunCommandAndCaptureOut
  (cmd: string; catchOut: boolean; var outlines: TStringList;
  var report: string; showcmd: integer; var ExitCode: longint;
  showoutput: boolean = False; logleveloffset: integer = 0): boolean;
{$ENDIF OPSISCRIPT}

function getCommandResult(cmd: string): string; overload;
function getCommandResult(cmd: string; var exitcode: longint): string; overload;

implementation

  {$IFDEF OPSISCRIPT}

uses
   {$IFDEF GUI}
  osshowsysinfo,
{$ENDIF GUI}
  osfunc;

  {$ENDIF OPSISCRIPT}

procedure ProcessMess;
begin
  {$IFDEF OPSISCRIPT}
  {$IFDEF GUI}
  Application.ProcessMessages;
  {$ENDIF GUI}
  {$ENDIF OPSISCRIPT}
end;

procedure InitializeProcess(FpcProcess: TProcess; cmd: string);
begin
  FpcProcess.CommandLine := cmd;
  FpcProcess.Options := [poUsePipes, poStderrToOutput];
  FpcProcess.ShowWindow := swoMinimize;
  FpcProcess.Execute;
end;

procedure ReadLastPartOfProcessOutput(var M: TMemoryStream; var BytesRead: longint;
  const ReadBufferSize: integer; var FpcProcess: TProcess;var n: longint);
begin
  repeat
    // make sure that we have the needed space
    M.SetSize(BytesRead + ReadBufferSize);
    if FpcProcess.Output.NumBytesAvailable > 0 then
    begin
      // try to read
      n := FpcProcess.Output.Read((M.Memory + BytesRead)^, ReadBufferSize);
      if n > 0 then
      begin
        Inc(BytesRead, n);
        //Logdatei.Log('RunCommandAndCaptureOut: read: ' +
        //IntToStr(n) + ' bytes', LLdebug2);
        //Write('.');
      end;
    end
    else
      n := 0;
  until n <= 0;
  //if BytesRead > 0 then WriteLn;
  M.SetSize(BytesRead);
  //Logdatei.Log('RunCommandAndCaptureOut: -- executed --', LLdebug2);
  //WriteLn('-- executed --');
end;

{$IFDEF OPSISCRIPT}

function RunCommandCaptureOutGetOutlist(command: string): TXStringlist;
var
  commandline: string = '';
  //result: TXStringList;
  filename: string = '';
  parameters: string = '';
  report: string = '';
  errorinfo: string = '';
  i: integer = 0;
  exitcode: longint;
begin
  Result := TXStringList.Create;
  //runAs := traInvoker;
  //OldNumberOfErrors := LogDatei.NumberOfErrors;
  //OldNumberOfWarnings := LogDatei.NumberOfWarnings;

  FileName := '/bin/bash';
  Parameters := Parameters + ' -c "' + command + ' || exit $?"';

  commandline := FileName + ' ' + trim(Parameters);

  if Logdatei <> nil then
    LogDatei.log('Executing ' + commandline, LLDebug2);

  if not RunCommandAndCaptureOut(commandline, True, Result, report,
    SW_Minimize, exitcode, False, 2) then
  begin
    if Logdatei <> nil then
      LogDatei.log('Error: ' + Report, LLcritical);
    //FExtremeErrorLevel := LevelFatal;
    //scriptstopped := true;
    Result.Clear;
  end;
end;

function RunCommandCaptureOutGetExitcode(command: string): longint;
var
  commandline: string = '';
  output: TXStringList;
  filename: string = '';
  parameters: string = '';
  report: string = '';
  errorinfo: string = '';
  i: integer = 0;
  exitcode: longint;
begin
  output := TXStringList.Create;
  //runAs := traInvoker;
  //OldNumberOfErrors := LogDatei.NumberOfErrors;
  //OldNumberOfWarnings := LogDatei.NumberOfWarnings;

  FileName := '/bin/bash';
  Parameters := Parameters + ' -c "' + command + ' || exit $?"';

  commandline := FileName + ' ' + trim(Parameters);

  LogDatei.log('Executing ' + commandline, LLDebug2);

  if not RunCommandAndCaptureOut(commandline, True, output, report,
    SW_Minimize, exitcode) then
  begin
    LogDatei.log('Error: ' + Report, LLcritical);
    //FExtremeErrorLevel := LevelFatal;
    //scriptstopped := true;
    Result := exitcode;
  end
  else
  begin
    LogDatei.log('output:', LLDebug2);
    LogDatei.log('--------------', LLDebug2);
    for i := 0 to output.Count - 1 do
    begin
      LogDatei.log(output.strings[i], LLDebug2);
    end;
    LogDatei.log('', LLDebug2);
    Result := exitcode;
  end;
  output.Free;
end;

{$IFDEF GUI}
procedure InitializeSystemInfo(SystemInfo: TSystemInfo; cmd: string; logleveloffset: integer);
begin
  SystemInfo.Memo1.Color := clBlack;
  SystemInfo.Memo1.Font.Color := clWhite;
  SystemInfo.Memo1.Lines.Clear;
  systeminfo.BitBtn1.Enabled := False;
  systeminfo.Label1.Caption := 'Executing: ' + cmd;
  systeminfo.ShowOnTop;
  ProcessMess;
  LogDatei.log('Start Showoutput', LLInfo + logleveloffset);
end;
{$ENDIF GUI}

function RunCommandAndCaptureOut
  (cmd: string; catchOut: boolean; var outlines: TXStringList;
  var report: string; showcmd: integer; var ExitCode: longint;
  showoutput: boolean = False; logleveloffset: integer = 0): boolean;

const
  ReadBufferSize = 2048;

var
  outpart: TStringList;
  M: TMemoryStream;
  FpcProcess: TProcess;
  n: longint;
  BytesRead: longint;

begin
  Result := True;
  M := TMemoryStream.Create;
  outpart := TStringList.Create;
  outlines.Clear;
  BytesRead := 0;
  FpcProcess := process.TProcess.Create(nil);
  try
    try
      InitializeProcess(FpcProcess, cmd);
      if Logdatei <> nil then
        Logdatei.log('RunCommandAndCaptureOut: started: ' + cmd, LLdebug + logleveloffset);
      {$IFDEF GUI}
      if showoutput then
      begin
        SystemInfo := TSystemInfo.Create(nil);
        InitializeSystemInfo(SystemInfo, cmd, logleveloffset);
      end;
      {$ENDIF GUI}

      while FpcProcess.Running do
      begin
        // make sure that we have the needed space
        M.SetSize(BytesRead + ReadBufferSize);

        // try to read
        if FpcProcess.Output.NumBytesAvailable > 0 then
        begin
          n := FpcProcess.Output.Read((M.Memory + BytesRead)^, ReadBufferSize);
          if n > 0 then
          begin
            Inc(BytesRead, n);
            M.SetSize(BytesRead);
            outpart.LoadFromStream(M);
            outlines.Text := outlines.Text + outpart.Text;
            {$IFDEF GUI}
            if showoutput then
            begin
              SystemInfo.Memo1.Lines.Add(outpart.Text);
              ProcessMess;
            end;
            {$ENDIF GUI}
          end;
        end
        else
        begin
          // no data, wait 100 ms
          Sleep(100);
        end;
      end;
      exitCode := FpcProcess.ExitCode;
      ReadLastPartOfProcessOutput(M, BytesRead, ReadBufferSize, FpcProcess, n);

      outpart.LoadFromStream(M);
      outlines.Text := outlines.Text + outpart.Text;
      // Attention: Exitcode is exitcode of bash
      {$IFDEF GUI}
      if showoutput then
      begin
        SystemInfo.Memo1.Lines.Add(outpart.Text);
        ProcessMess;
      end;
      {$ENDIF GUI}
      if Logdatei <> nil then
        LogDatei.log('ExitCode ' + IntToStr(exitCode), LLInfo + logleveloffset);
    except
      on e: Exception do
      begin
        if Logdatei <> nil then
          LogDatei.log('Exception in RunCommandAndCaptureOut: ' + e.message, LLError);
        Result := False;
      end;
    end;
  finally
    outpart.Free;
    FpcProcess.Free;
    M.Free;
    {$IFDEF GUI}
    if showoutput then
    begin
      FreeAndNil(SystemInfo);
      ProcessMess;
      if Logdatei <> nil then
        LogDatei.log('Stop Showoutput', LLInfo + logleveloffset);
    end;
    {$ENDIF GUI}
  end;
end;

{$ELSE OPSISCRIPT}
function RunCommandCaptureOutGetOutlist(command: string): TStringList;
var
  commandline: string = '';
  //result: TXStringList;
  filename: string = '';
  parameters: string = '';
  report: string = '';
  errorinfo: string = '';
  i: integer = 0;
  exitcode: longint;
begin
  Result := TStringList.Create;
  //runAs := traInvoker;
  //OldNumberOfErrors := LogDatei.NumberOfErrors;
  //OldNumberOfWarnings := LogDatei.NumberOfWarnings;

  FileName := '/bin/bash';
  Parameters := Parameters + ' -c "' + command + ' || exit $?"';

  commandline := FileName + ' ' + trim(Parameters);

  LogDatei.log('Executing ' + commandline, LLDebug2);

  if not RunCommandAndCaptureOut(commandline, True, Result, report,
    SW_Minimize, exitcode, False, 2) then
  begin
    LogDatei.log('Error: ' + Report, LLcritical);
    //FExtremeErrorLevel := LevelFatal;
    //scriptstopped := true;
    Result.Clear;
  end;
end;

function RunCommandAndCaptureOut
  (cmd: string; catchOut: boolean; var outlines: TStringList;
  var report: string; showcmd: integer; var ExitCode: longint;
  showoutput: boolean = False; logleveloffset: integer = 0): boolean;

const
  ReadBufferSize = 2048;

var
  S: TStringList;
  M: TMemoryStream;
  FpcProcess: TProcess;
  n: longint;
  BytesRead: longint;

begin
  Result := True;
  M := TMemoryStream.Create;
  S := TStringList.Create;
  BytesRead := 0;
  FpcProcess := process.TProcess.Create(nil);
  try
    try
      InitializeProcess(FpcProcess, cmd);
      if Logdatei <> nil then
        Logdatei.log('RunCommandAndCaptureOut: started: ' + cmd, LLdebug3);

      while FpcProcess.Running do
      begin
        // make sure that we have the needed space
        M.SetSize(BytesRead + ReadBufferSize);

        // try to read
        if FpcProcess.Output.NumBytesAvailable > 0 then
        begin
          n := FpcProcess.Output.Read((M.Memory + BytesRead)^, ReadBufferSize);
          if n > 0 then
          begin
            Inc(BytesRead, n);
          end;
        end
        else
        begin
          // no data, wait 100 ms
          Sleep(100);
        end;
      end;
      exitCode := FpcProcess.ExitCode;
      ReadLastPartOfProcessOutput(M, BytesRead, ReadBufferSize, FpcProcess, n);

      S.LoadFromStream(M);
      for n := 0 to S.Count - 1 do
      begin
        outlines.Add(S[n]);
      end;
      // Attention: Exitcode is exitcode of bash
      if Logdatei <> nil then
        LogDatei.log('ExitCode ' + IntToStr(exitCode), LLInfo + logleveloffset);
    except
      on e: Exception do
      begin
        if Logdatei <> nil then
          LogDatei.log('Exception in RunCommandAndCaptureOut: ' + e.message, LLError);
        Result := False;
      end;
    end;
  finally
    S.Free;
    FpcProcess.Free;
    M.Free;
  end;
end;

{$ENDIF OPSISCRIPT}

function getCommandResult(cmd: string): string;
var
  exitcode: longint;
begin
  Result := getCommandResult(cmd, exitcode);
end;



function getCommandResult(cmd: string; var exitcode: longint): string;
var
  report: string;
  {$IFDEF OPSISCRIPT}
  outlines: TXStringlist;
  {$ELSE OPSISCRIPT}
  outlines: TStringList;
  {$ENDIF OPSISCRIPT}
  //ExitCode: longint;
  i: integer;
begin
  {$IFDEF OPSISCRIPT}
  outlines := TXStringList.Create;
  {$ELSE OPSISCRIPT}
  outlines := TStringList.Create;
  {$ENDIF OPSISCRIPT}
  Result := '';
  if not RunCommandAndCaptureOut(cmd, True, outlines, report, SW_HIDE,
    ExitCode, False, 2) then
  begin
    if Logdatei <> nil then
      LogDatei.log('Error: ' + Report + 'Exitcode: ' + IntToStr(ExitCode), LLError);
  end
  else
  begin
    if Logdatei <> nil then
      LogDatei.log('', LLDebug2);
    if Logdatei <> nil then
      LogDatei.log('output:', LLDebug2);
    if Logdatei <> nil then
      LogDatei.log('--------------', LLDebug2);
    for i := 0 to outlines.Count - 1 do
    begin
      if Logdatei <> nil then
        LogDatei.log(outlines.strings[i], LLDebug2);
      if outlines.Count > 0 then
      begin
        {give always the first string}
        Result := outlines.strings[0];
      end;
    end;
    if Logdatei <> nil then
      LogDatei.log('', LLDebug2);
  end;
  outlines.Free;
end;


end.
