unit runprocess;

{$mode objfpc}{$H+}

interface

uses
  classes,
  SysUtils,
  process;


function setwindowtoalldesktops(winstr : string) : boolean;

function RunCommandAndCaptureOut
  (cmd: string; catchOut: boolean; var outlines: TStringList;
  var report: string; showcmd: integer; var ExitCode: longint): boolean;


implementation

function setwindowtoalldesktops(winstr : string) : boolean;
{$IFDEF LINUX}
var
  slist : TStringlist;
  report : string;
  exitcode : integer;
{$ENDIF LINUX}
begin
  {$IFDEF LINUX}
  slist := TStringlist.create;
  result := RunCommandAndCaptureOut('wmctrl -r '+winstr+' -t -2',
                                    false , slist , report, 0, exitcode);
  slist.free;
  {$ELSE LINUX}
  Result := true;
  {$ENDIF LINUX}
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
      //Logdatei.DependentAdd('RunCommandAndCaptureOut: started: ' + cmd, LLdebug2);
      while FpcProcess.Running do
      begin
        // stellt sicher, dass wir Platz haben
        M.SetSize(BytesRead + ReadBufferSize);

        // versuche, es zu lesen
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
          // keine Daten, warte 100 ms
          //Logdatei.DependentAdd('RunCommandAndCaptureOut: no data - waiting....',
          // LLdebug2);
          Sleep(100);
        end;
      end;
      exitCode := FpcProcess.ExitCode;
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
      end;
      //WriteLn('-- end --');
      //LogDatei.log('ExitCode ' + IntToStr(exitCode), LLInfo);
    except
      on e: Exception do
      begin
        //LogDatei.log('Exception in RunCommandAndCaptureOut: ' +  e.message, LLError);
        Result := False;
      end;
    end;
  finally
    S.Free;
    FpcProcess.Free;
    M.Free;
  end;
end;


end.

