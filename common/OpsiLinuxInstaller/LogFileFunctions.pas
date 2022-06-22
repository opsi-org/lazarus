unit LogFileFunctions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  oslog;

procedure InitializeLogFile(logFileName: string);

implementation

procedure InitializeLogFile(logFileName: string);
begin
  LogDatei := TLogInfo.Create;
  LogDatei.CreateTheLogfile(logFileName);
  LogDatei.log('Log file created', LLnothing);
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  LogDatei.log('Working directory: ' + GetCurrentDir, LLessential);
end;

end.
