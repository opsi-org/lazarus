unit OckWindows;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DSiWin32, jwawinbase;

function isAdmin:boolean;
function GetUserName_: string;


implementation

function isAdmin: boolean;
begin
  Result := DSiIsAdmin;
end;

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

end.

