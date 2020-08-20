unit OckWindows;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DSiWin32;

function isAdmin:boolean;

implementation

function isAdmin: boolean;
begin
  Result := DSiIsAdmin;
end;

end.

