unit OckLinux;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function isAdmin:boolean;

implementation

function isAdmin: boolean;
begin
  Result := True;
end;

end.

