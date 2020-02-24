unit OpsiClientdLog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TPassLog = procedure(AMsg: string; LogLevel:integer) of object;

  { TLogData }

  TLogData = class(TObject)
    FLogMessage: string;
    FLevelofLine:integer;
    FLogLevel: integer;
    FPassLog: TPassLog;
    procedure SendLog;
    property OnPassLog: TPassLog read FPassLog write FPassLog;
  end;


implementation

{ TLogData }

procedure TLogData.SendLog;
begin
  if Assigned(FPassLog) then
    FPassLog(FLogMessage,FLevelOfLine);
end;

end.

