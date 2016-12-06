unit fpcfunc;

{$mode objfpc}{$H+}

interface

uses
  process,
  Classes,
  SysUtils,
  wiglob;

function fpcRunCommandAndCaptureOut	(cmd : String;	var outlines : TStringList; var report: String) : boolean;

implementation

function fpcRunCommandAndCaptureOut	(cmd : String;	var outlines : TStringList; var report: String) : boolean;
  //http://wiki.lazarus.freepascal.org/Executing_External_Programs/de
var
  S: TStringList;
  M: TMemoryStream;
  FpcProcess: TProcess;
  n: longint;
  BytesRead: longint;
const
ReadBufferSize = 2048;
begin
  try
   try
    M := TMemoryStream.Create;
    BytesRead := 0;
    FpcProcess := process.TProcess.Create(nil);
    FpcProcess.CommandLine := cmd;
    //FpcProcess.CommandLine := 'cmd.exe /c dir';
    FpcProcess.Options := [poUsePipes];
    FpcProcess.ShowWindow := swoMinimize;
    FpcProcess.Execute;
    while FpcProcess.Running do
    begin
      // stellt sicher, dass wir Platz haben
      M.SetSize(BytesRead + ReadBufferSize);

      // versuche, es zu lesen
      n := FpcProcess.Output.Read((M.Memory + BytesRead)^, ReadBufferSize);
      if n > 0
      then begin
        Inc(BytesRead, n);
        //Write('.')
      end
      else begin
        // keine Daten, warte 100 ms
        Sleep(100);
      end;
    end;
    // lese den letzten Teil
    repeat
      // stellt sicher, dass wir Platz haben
      M.SetSize(BytesRead + ReadBufferSize);
      // versuche es zu lesen
      n := FpcProcess.Output.Read((M.Memory + BytesRead)^, ReadBufferSize);
      if n > 0
      then begin
        Inc(BytesRead, n);
        //Write('.');
      end;
    until n <= 0;
    //if BytesRead > 0 then WriteLn;
    M.SetSize(BytesRead);
    //WriteLn('-- executed --');

    S := TStringList.Create;
    S.LoadFromStream(M);
    //WriteLn('-- linecount = ', S.Count, ' --');
    for n := 0 to S.Count - 1 do
    begin
      //WriteLn('| ', S[n]);
      outlines.Add( S[n] );
    end;
    //WriteLn('-- end --');
   except
				on e: exception do
				begin
					LogDatei.DependentAdd('Exception in RunCommandAndCaptureOut: '+e.message, LLdebug2);
				end;
				end;
			finally
   S.Free;
   FpcProcess.Free;
   M.Free;
  end;
end;


end.

