unit OckLinuxAPI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, osLog,
  OckSystemAPI
  ;

type

{ TLinuxAPI }

TLinuxAPI = class(TSystemAPI)
  function IsAdmin:boolean;override;
  procedure SaveImagesOnDepot(const AppLocation:string; user:string; password:string);
end;

var
  SystemAPI : TSystemAPI;

implementation

{ TLinuxAPI }

function TLinuxAPI.IsAdmin: boolean;
begin

end;

initialization
  SystemAPI := TLinuxAPI.Create;

finalization
  FreeAndNil(SystemAPI);

end.

