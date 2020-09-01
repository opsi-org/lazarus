unit OpsiClientdLog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TPassLog = procedure(aLogMessage: string; aLevelofLine:integer; aSourceOfLog:string) of object;

  { TLogData }

  TLogData = class(TObject)
    FLogMessage: string;
    FLevelofLine:integer;
    //FLogLevel: integer;
    FSourceOfLog: string;
    FPassLog: TPassLog;
    procedure SendLog;
    property OnPassLog: TPassLog read FPassLog write FPassLog;
  end;


implementation

{ TLogData }

procedure TLogData.SendLog;
begin
  if Assigned(FPassLog) then
    FPassLog(FLogMessage,FLevelOfLine,FSourceOfLog);
end;

end.

