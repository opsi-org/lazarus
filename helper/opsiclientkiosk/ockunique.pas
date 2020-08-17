unit OckUnique;

{$mode delphi}

interface

uses
  Classes, SysUtils,
  {$IFDEF WIN32}
  Windows,
  DSiWin32,
  //  JwaWinnt,
  //  jwawinbase,
  JwaWindows,

  {$ENDIF WIN32}
  oslog,
  osprocesses;

function CheckUnique(out InfoMessage:string):boolean;

implementation

function CheckUnique(out InfoMessage: string): boolean;
begin
  { is opsiclientd running? }
  if numberOfProcessInstances('opsiclientd') < 1 then
  begin
    LogDatei.log('opsiclientd is not running - so we abort', LLCritical);
    LogDatei.Close;
    LogDatei.Free;
    InfoMessage := 'opsiclientd is not running - so we abort';
    Result := False;
  end
  else
  { is opsiclientkiosk already running? }
    if numberOfProcessInstances(ExtractFileName(ParamStr(0))) > 1 then
    begin
      LogDatei.log('An other instance of this program is running - so we abort', LLCritical);
      LogDatei.Close;
      LogDatei.Free;
      InfoMessage := 'An other instance of this program is running - so we abort';
      Result := False;
    end;
end;

end.
