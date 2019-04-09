program genericlaunch;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  osversioninfo,
  osencoding,
  oslog,
  SysUtils,
  strutils,
  lazFileUtils,
  Process;

procedure initLogging;
var
  i : integer;
  logfilename : string;
begin
  logdatei := TLogInfo.Create;
  logfilename := ExtractFileNameOnly(reencode(ParamStr(0), 'system'))+'-laucher.log';
  LogDatei.WritePartLog := False;
  LogDatei.WriteErrFile:= False;
  LogDatei.WriteHistFile:= False;
  logdatei.CreateTheLogfile(logfilename, False);
  logdatei.LogLevel := 7;
  (*
  for i := 0 to preLogfileLogList.Count-1 do
    logdatei.log(preLogfileLogList.Strings[i], LLessential);
  preLogfileLogList.Free;
  *)
  logdatei.log('opsi-generic launcher version: ' + getversioninfo, LLessential);
  logdatei.log('Called as: ' + ExtractFileNameOnly(reencode(ParamStr(0), 'system')), LLessential);
end;

  function launchProgram(strProgram: string): boolean;
    // http://wiki.freepascal.org/Executing_External_Programs/de
  var
    AProcess: TProcess;
    AStringList: TStringList;

  begin
    launchProgram := True;
    try
      try
        // Erzeugen des TProcess Objekts
        AProcess := TProcess.Create(nil);

        AStringList := TStringList.Create;

        // TODO: deprecated ->
        AProcess.CommandLine := strProgram;
        // definieren einer Option, wie das Programm
        // ausgeführt werden soll. Dies stellt sicher, dass
        // unser Programm nicht vor Beendigung des aufgerufenen
        // Programmes fortgesetzt wird. Außerdem wird hier angegeben,
        // dass die Ausgabe gelesen werden soll. Wird aber nciht verwendet
        AProcess.Options := AProcess.Options + [poUsePipes];

        AProcess.Execute;

        // Ausgabe Stringliste lesen
        //AStringList.LoadFromStream(AProcess.Output);
        // Speichern in eine Datei output.txt
        //AStringList.SaveToFile('output.txt');

      except
        on e: Exception do
        begin
          logdatei.log('Exception:: ' + e.message, LLError);
          //writeln('Exception:: ' + e.message);
          launchProgram := False;
        end
      end;
    finally
      AStringList.Free;
      AProcess.Free;
    end;
  end;

  function ReadFileToString(strFilename: string): string;
  var
    stream: TFileStream;
  begin
    // öffnet eine Datei zum Lesen und sperrt sie gleichzeitig gegen Schreibzugriffe
    stream := TFileStream.Create(strFilename, fmOpenRead or fmShareDenyWrite);
    try
      SetLength(Result, stream.Size);
      stream.Read(Result[1], stream.Size);
    finally
      stream.Free;
    end;
  end;

var
  filePath, confFileName, executeStr: string;
  param: string;
  i: integer;

{$R *.res}

begin
  initLogging;
  writeln('opsi-generic launcher version: ' + getversioninfo);
  filePath := ExtractFilePath(ParamStr(0));
  confFileName := ExtractFileNameWithoutExt(ParamStr(0)) + '.conf';
  // hier ev. noch andere Sonderzeichen Abfangen?
  executeStr := StringReplace(ReadFileToString(confFileName), #10, ' ', [rfReplaceAll]);
  executestr := TrimRightSet(trim(executestr),[#10,#13]);
  logdatei.log('config string: ' +executestr, LLessential);
  for i := 1 to Paramcount do
  begin
    param := ParamStr(i);
    logdatei.log('param '+inttostr(i)+': ' + param, LLessential);
    // quote file path
    if fileexists(param) and (param[1] <> '"') then
      param := '"'+param+'"';
    executeStr := executeStr + ' ' + param;
  end;
  logdatei.log('Launch: ' + executeStr, LLessential);
  writeln('Launch: ' + executeStr);
  launchProgram(executeStr);
  logdatei.log('Launched - closing log.', LLessential);
  LogDatei.Close;
  LogDatei.Free;
end.


