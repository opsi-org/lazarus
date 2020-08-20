unit OckLinux;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process;

function isAdmin:boolean;
function GetUserName_: string;

implementation

function isAdmin: boolean;
begin
  Result := True;
end;

function GetUserName_: string;
var
  Output: String;
begin
  RunCommand('/bin/sh',['echo $USER'],Output,[],swoHIDE);
  Result := Output;
end;

end.

