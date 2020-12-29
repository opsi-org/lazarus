unit OckAdminTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;


function IsAdmin:boolean;

implementation

function IsAdmin: boolean;
begin
  {$IFDEF WINDOWS}
    result :=  DSiIsAdmin;;
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
    result :=  IsAdminLinux;
  {$ENDIF LINUX}

end;

end.

