program genericlaunch;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, FileUtil,
  Process
  ;

function launchProgram (strProgram: string): boolean;
// http://wiki.freepascal.org/Executing_External_Programs/de
var
  AProcess: TProcess;
  AStringList: TStringList;

begin
  launchProgram:=true;
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
    AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];

    AProcess.Execute;

    // Ausgabe Stringliste lesen
    //AStringList.LoadFromStream(AProcess.Output);
    // Speichern in eine Datei output.txt
    //AStringList.SaveToFile('output.txt');

    AStringList.Free;
    AProcess.Free;
  except
    launchProgram:=false;
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

var  filePath, confFileName, executeStr : String;
     param : String;
     i : integer;
begin
  filePath:=ExtractFilePath(ParamStr(0));
  confFileName:=ExtractFileNameWithoutExt(ParamStr(0)) + '.conf';
  // hier ev. noch andere Sonderzeichen Abfangen?
  executeStr := StringReplace(ReadFileToString(confFileName), #10, ' ', [rfReplaceAll]);
  for i:=1 to Paramcount do
  begin
     param:= ParamStr(i);
     executeStr := executeStr + ' ' + param;
  end;
  launchProgram(executeStr);
end.

