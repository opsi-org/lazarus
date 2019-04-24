unit OSProcessux;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF OPSISCRIPT}
  osfunc,
  {$IFDEF GUI}
  graphics,
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
function RunCommandCaptureOutGetOutlist(command : string) : TXStringlist;
function RunCommandCaptureOutGetExitcode(command : string) : longint;
function RunCommandAndCaptureOut
  (cmd: string; catchOut: boolean; var outlines: TXStringList;
  var report: string; showcmd: integer; var ExitCode: longint): boolean; overload;

function RunCommandAndCaptureOut
  (cmd: string; catchOut: boolean; var outlines: TXStringList;
  var report: string; showcmd: integer; var ExitCode: longint;showoutput:boolean): boolean;   overload;

function RunCommandAndCaptureOut
  (cmd: string; catchOut: boolean; var outlines: TXStringList;
  var report: string; showcmd: integer; var ExitCode: longint;
  showoutput:boolean; logleveloffset : integer): boolean;   overload;

{$ELSE OPSISCRIPT}
function RunCommandAndCaptureOut
  (cmd: string; catchOut: boolean; var outlines: TStringList;
  var report: string; showcmd: integer; var ExitCode: longint): boolean; overload;
function RunCommandAndCaptureOut
  (cmd: string; catchOut: boolean; var outlines: TStringList;
  var report: string; showcmd: integer; var ExitCode: longint;showoutput:boolean): boolean; overload;
function RunCommandAndCaptureOut
  (cmd: string; catchOut: boolean; var outlines: TStringList;
  var report: string; showcmd: integer; var ExitCode: longint;showoutput:boolean;
  logleveloffset : integer): boolean;
{$ENDIF OPSISCRIPT}

function getCommandResult(cmd : string): string;  overload;
function getCommandResult(cmd : string; var exitcode : longint): string;  overload;

implementation

  {$IFDEF OPSISCRIPT}
  {$IFDEF GUI}
uses
  osshowsysinfo;
  {$ENDIF GUI}
  {$ENDIF OPSISCRIPT}

{$IFDEF OPSISCRIPT}

function RunCommandCaptureOutGetOutlist(command : string) : TXStringlist;
Var
 commandline : String='';
 //result: TXStringList;
 filename : String='';
 parameters : String='';
 report : String='';
 errorinfo : String='';
 i : integer=0;
 exitcode : longint;
begin
  result := TXStringList.Create;
  //runAs := traInvoker;
  //OldNumberOfErrors := LogDatei.NumberOfErrors;
  //OldNumberOfWarnings := LogDatei.NumberOfWarnings;

  FileName := '/bin/bash';
  Parameters := Parameters+' -c "' + command + ' || exit $?"';

  commandline := FileName + ' ' +trim(Parameters);

  LogDatei.log ('Executing ' + commandline, LLDebug2);

  if not RunCommandAndCaptureOut
     (commandline,
      true,
      result, report, SW_Minimize, exitcode,false,2)
  then
  Begin
    LogDatei.log('Error: ' + Report, LLcritical);
    //FExtremeErrorLevel := LevelFatal;
    //scriptstopped := true;
    result.Clear;
  End
end;

function RunCommandCaptureOutGetExitcode(command : string) : longint;
Var
 commandline : String='';
 output: TXStringList;
 filename : String='';
 parameters : String='';
 report : String='';
 errorinfo : String='';
 i : integer=0;
 exitcode : longint;
begin
  output := TXStringList.Create;
  //runAs := traInvoker;
  //OldNumberOfErrors := LogDatei.NumberOfErrors;
  //OldNumberOfWarnings := LogDatei.NumberOfWarnings;

  FileName := '/bin/bash';
  Parameters := Parameters+' -c "' + command + ' || exit $?"';

  commandline := FileName + ' ' +trim(Parameters);

  LogDatei.log ('Executing ' + commandline, LLDebug2);

  if not RunCommandAndCaptureOut
     (commandline,
      true,
      output, report, SW_Minimize, exitcode)
  then
  Begin
    LogDatei.log('Error: ' + Report, LLcritical);
    //FExtremeErrorLevel := LevelFatal;
    //scriptstopped := true;
    result := exitcode;
  End
  else
  begin
    LogDatei.log ('output:', LLDebug2);
    LogDatei.log ('--------------', LLDebug2);
    for i := 0 to output.count-1 do
    begin
     LogDatei.log (output.strings[i], LLDebug2);
    end;
    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 6;
    LogDatei.log ('', LLDebug2);
    result := exitcode;
  end;
 output.Free;
end;

function RunCommandAndCaptureOut
  (cmd: string; catchOut: boolean; var outlines: TXStringList;
  var report: string; showcmd: integer; var ExitCode: longint): boolean;
begin
  result := RunCommandAndCaptureOut(cmd, catchOut, outlines, report, showcmd, ExitCode, false);
end;

function RunCommandAndCaptureOut
  (cmd: string; catchOut: boolean; var outlines: TXStringList;
  var report: string; showcmd: integer; var ExitCode: longint;showoutput:boolean): boolean;
begin
  result := RunCommandAndCaptureOut(cmd, catchOut, outlines, report, showcmd, ExitCode, false,0);
end;


function RunCommandAndCaptureOut
  (cmd: string; catchOut: boolean; var outlines: TXStringList;
  var report: string; showcmd: integer; var ExitCode: longint;showoutput:boolean;
  logleveloffset : integer): boolean;

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
      if Logdatei <> nil then Logdatei.log('RunCommandAndCaptureOut: started: ' + cmd, LLdebug3);
      {$IFDEF GUI}
      if showoutput then
      begin
        //CreateSystemInfo;
        SystemInfo := TSystemInfo.Create(nil);
        SystemInfo.Memo1.Color:= clBlack;
        SystemInfo.Memo1.Font.Color := clWhite;
        SystemInfo.Memo1.Lines.clear;
        systeminfo.BitBtn1.Enabled:= false;
        systeminfo.Label1.Caption:='Executing: '+cmd;
        systeminfo.ShowOnTop;
        //ProcessMess;
        LogDatei.log('Start Showoutput', LLInfo+logleveloffset);
        //FBatchOberflaeche.Left:= 5;
        //FBatchOberflaeche.Top:= 5;
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
          //Logdatei.DependentAdd('RunCommandAndCaptureOut: read: ' +
          //  IntToStr(n) + ' bytes', LLdebug2);
          if n > 0 then
          begin
            Inc(BytesRead, n);
            //Logdatei.DependentAdd('RunCommandAndCaptureOut: read: ' +
            // IntToStr(n) + ' bytes', LLdebug2);
            //Write('.')
          end;
        end
        else
        begin
          // no data, wait 100 ms
          //Logdatei.DependentAdd('RunCommandAndCaptureOut: no data - waiting....',
          // LLdebug2);
          Sleep(100);
        end;
      end;
      exitCode := FpcProcess.ExitCode;
      // read the last part
      repeat
        // smake sure that we have the needed space
        M.SetSize(BytesRead + ReadBufferSize);
        if FpcProcess.Output.NumBytesAvailable > 0 then
        begin
          // try to read
          n := FpcProcess.Output.Read((M.Memory + BytesRead)^, ReadBufferSize);
          if n > 0 then
          begin
            Inc(BytesRead, n);
            //Logdatei.DependentAdd('RunCommandAndCaptureOut: read: ' +
            //IntToStr(n) + ' bytes', LLdebug2);
            //Write('.');
          end;
        end
        else
          n := 0;
      until n <= 0;
      //if BytesRead > 0 then WriteLn;
      M.SetSize(BytesRead);
      //Logdatei.DependentAdd('RunCommandAndCaptureOut: -- executed --', LLdebug2);
      //WriteLn('-- executed --');

      S := TStringList.Create;
      S.LoadFromStream(M);
      //Logdatei.DependentAdd('RunCommandAndCaptureOut: -- linecount = ' +
      //  IntToStr(S.Count), LLdebug2);
      //WriteLn('-- linecount = ', S.Count, ' --');
      for n := 0 to S.Count - 1 do
      begin
        //WriteLn('| ', S[n]);
        outlines.Add(S[n]);
        {$IFDEF GUI}
        if showoutput then
        begin
          SystemInfo.Memo1.Lines.Add(S[n]);
          //ProcessMess;
        end;
        //ProcessMess;
        {$ENDIF GUI}
      end;
      //WriteLn('-- end --');
      // Attention: Exitcode is exitcode of bash
          if Logdatei <> nil then LogDatei.log('ExitCode ' + IntToStr(exitCode), LLInfo+logleveloffset);
    except
      on e: Exception do
      begin
            if Logdatei <> nil then LogDatei.log('Exception in RunCommandAndCaptureOut: ' +
          e.message, LLError);
        Result := False;
      end;
    end;
  finally
    S.Free;
    FpcProcess.Free;
    M.Free;
    {$IFDEF GUI}
    if showoutput then
    begin
      SystemInfo.free; SystemInfo := nil;
      //FBatchOberflaeche.BringToFront;
      //FBatchOberflaeche.centerWindow;
      //ProcessMess;
      if Logdatei <> nil then LogDatei.log('Stop Showoutput', LLInfo+logleveloffset);
    end;
    {$ENDIF GUI}
  end;
end;
{$ELSE OPSISCRIPT}
function RunCommandAndCaptureOut
  (cmd: string; catchOut: boolean; var outlines: TStringList;
  var report: string; showcmd: integer; var ExitCode: longint): boolean;
begin
  result := RunCommandAndCaptureOut(cmd, catchOut, outlines, report, showcmd, ExitCode, false);
end;

function RunCommandAndCaptureOut
  (cmd: string; catchOut: boolean; var outlines: TStringList;
  var report: string; showcmd: integer; var ExitCode: longint;showoutput:boolean): boolean;
begin
  result := RunCommandAndCaptureOut(cmd, catchOut, outlines, report, showcmd, ExitCode, showoutput,0);
end;

function RunCommandAndCaptureOut
  (cmd: string; catchOut: boolean; var outlines: TStringList;
  var report: string; showcmd: integer; var ExitCode: longint;showoutput:boolean;
  logleveloffset : integer): boolean;


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
      if Logdatei <> nil then Logdatei.log('RunCommandAndCaptureOut: started: ' + cmd, LLdebug3);
      {$IFDEF GUI}
      if showoutput then
      begin
        CreateSystemInfo;
        SystemInfo.Memo1.Color:= clBlack;
        SystemInfo.Memo1.Font.Color := clWhite;
        SystemInfo.Memo1.Lines.clear;
        systeminfo.BitBtn1.Enabled:= false;
        systeminfo.Label1.Caption:='Executing: '+cmd;
        systeminfo.ShowOnTop;
        ProcessMess;
        LogDatei.log('Start Showoutput', LLInfo+logleveloffset);
        FBatchOberflaeche.Left:= 5;
        FBatchOberflaeche.Top:= 5;
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
          //Logdatei.DependentAdd('RunCommandAndCaptureOut: read: ' +
          //  IntToStr(n) + ' bytes', LLdebug2);
          if n > 0 then
          begin
            Inc(BytesRead, n);
            //Logdatei.DependentAdd('RunCommandAndCaptureOut: read: ' +
            // IntToStr(n) + ' bytes', LLdebug2);
            //Write('.')
          end;
        end
        else
        begin
          // no data, wait 100 ms
          //Logdatei.DependentAdd('RunCommandAndCaptureOut: no data - waiting....',
          // LLdebug2);
          Sleep(100);
        end;
      end;
      exitCode := FpcProcess.ExitCode;
      // read the last part
      repeat
        // smake sure that we have the needed space
        M.SetSize(BytesRead + ReadBufferSize);
        if FpcProcess.Output.NumBytesAvailable > 0 then
        begin
          // try to read
          n := FpcProcess.Output.Read((M.Memory + BytesRead)^, ReadBufferSize);
          if n > 0 then
          begin
            Inc(BytesRead, n);
            //Logdatei.DependentAdd('RunCommandAndCaptureOut: read: ' +
            //IntToStr(n) + ' bytes', LLdebug2);
            //Write('.');
          end;
        end
        else
          n := 0;
      until n <= 0;
      //if BytesRead > 0 then WriteLn;
      M.SetSize(BytesRead);
      //Logdatei.DependentAdd('RunCommandAndCaptureOut: -- executed --', LLdebug2);
      //WriteLn('-- executed --');

      S := TStringList.Create;
      S.LoadFromStream(M);
      //Logdatei.DependentAdd('RunCommandAndCaptureOut: -- linecount = ' +
      //  IntToStr(S.Count), LLdebug2);
      //WriteLn('-- linecount = ', S.Count, ' --');
      for n := 0 to S.Count - 1 do
      begin
        //WriteLn('| ', S[n]);
        outlines.Add(S[n]);
        {$IFDEF GUI}
        if showoutput then
        begin
          SystemInfo.Memo1.Lines.Add(S[n]);
          ProcessMess;
        end;
        //ProcessMess;
        {$ENDIF GUI}
      end;
      //WriteLn('-- end --');
      // Attention: Exitcode is exitcode of bash
          if Logdatei <> nil then LogDatei.log('ExitCode ' + IntToStr(exitCode), LLInfo+logleveloffset);
    except
      on e: Exception do
      begin
            if Logdatei <> nil then LogDatei.log('Exception in RunCommandAndCaptureOut: ' +
          e.message, LLError);
        Result := False;
      end;
    end;
  finally
    S.Free;
    FpcProcess.Free;
    M.Free;
    {$IFDEF GUI}
    if showoutput then
    begin
      SystemInfo.free; SystemInfo := nil;
      FBatchOberflaeche.BringToFront;
      FBatchOberflaeche.centerWindow;
      ProcessMess;
      if Logdatei <> nil then LogDatei.log('Stop Showoutput', LLInfo+logleveloffset);
    end;
    {$ENDIF GUI}
  end;
end;
{$ENDIF OPSISCRIPT}

function getCommandResult(cmd : string): string;
var
  exitcode : longint;
begin
  result := getCommandResult(cmd, exitcode);
end;



function getCommandResult(cmd : string; var exitcode : longint): string;
var
  report: string;
  {$IFDEF OPSISCRIPT}
  outlines: TXStringlist;
  {$ELSE OPSISCRIPT}
  outlines: TStringlist;
  {$ENDIF OPSISCRIPT}
  //ExitCode: longint;
  i: integer;
begin
  {$IFDEF OPSISCRIPT}
  outlines := TXStringList.Create;
  {$ELSE OPSISCRIPT}
  outlines := TStringList.Create;
  {$ENDIF OPSISCRIPT}
  result := '';
  if not RunCommandAndCaptureOut(cmd, True, outlines, report,
    SW_HIDE, ExitCode,false,2) then
  begin
    if Logdatei <> nil then LogDatei.log('Error: ' + Report + 'Exitcode: ' + IntToStr(ExitCode), LLError);
  end
  else
  begin
    if Logdatei <> nil then LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 6;
    if Logdatei <> nil then LogDatei.log('', LLDebug2);
    if Logdatei <> nil then LogDatei.log('output:', LLDebug2);
    if Logdatei <> nil then LogDatei.log('--------------', LLDebug2);
    for i := 0 to outlines.Count - 1 do
    begin
      if Logdatei <> nil then LogDatei.log(outlines.strings[i], LLDebug2);
      result := outlines.strings[i];
    end;
    if Logdatei <> nil then LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 6;
    if Logdatei <> nil then LogDatei.log('', LLDebug2);
  end;
  outlines.Free;
end;


end.

