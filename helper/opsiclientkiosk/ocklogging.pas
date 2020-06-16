unit ocklogging;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, oslog, jwawinbase, proginfo;

function initLogging(const clientname: string): boolean;

var
  logfilename:string;

implementation

{:Returns user name of the current thread.
  @author  Miha-R, Lee_Nover
  @since   2002-11-25
}
function GetUserName_: string;
var
  buffer: PChar;
  bufferSize: DWORD;
begin
  bufferSize := 256; //UNLEN from lmcons.h
  buffer := AllocMem(bufferSize * SizeOf(char));
  try
    GetUserName(buffer, bufferSize);
    Result := string(buffer);
  finally
    FreeMem(buffer, bufferSize);
  end;
end; { DSiGetUserName}


function initLogging(const clientname: string): boolean;
var
  i : integer;
begin
  Result := True;
  LogDatei := TLogInfo.Create;
  logfilename := 'kiosk-' + GetUserName_ +'.log';
  LogDatei.WritePartLog := False;
  LogDatei.WriteErrFile:= False;
  LogDatei.WriteHistFile:= False;
  logdatei.CreateTheLogfile(logfilename, False);
  logdatei.LogLevel := 7; //myloglevel;
  //for i := 0 to preLogfileLogList.Count-1 do
  //  logdatei.log(preLogfileLogList.Strings[i], LLessential);
  //preLogfileLogList.Free;
  logdatei.log('opsi-client-kiosk: version: ' + ProgramInfo.Version, LLessential);
end;
end.

