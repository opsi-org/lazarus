unit LogFileFunctions;

// Helper function(s) for the 'LogDatei' from 'oslog'.

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
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
