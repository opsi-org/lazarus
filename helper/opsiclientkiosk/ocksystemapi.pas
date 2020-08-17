unit OckSystemAPI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  {$IFDEF WINDOWS}
    DSiWin32
  {$ENDIF WINDOWS}
  ;

type
  TSystemAPI = class(TObject)
    function IsAdmin:boolean;virtual;abstract;
    //function RunAsAdmin:boolean;virtual;abstract;
  end;

  { TWindowsAPI }

  TWindowsAPI = class(TSystemAPI)
    function IsAdmin:boolean;override;
    //RunAsAdmin(const Handle: DWord; const Path: string; Params: string): Boolean;
  end;

  { TLinuxAPI }

  TLinuxAPI = class(TSystemAPI)
    function IsAdmin:boolean;override;
    //function RunAsAdmin:boolean;override;
  end;


var
  SystemAPI : TSystemAPI;

implementation

{ TLinuxAPI }

function TLinuxAPI.IsAdmin: boolean;
begin

end;


{ TWindowsAPI }

function TWindowsAPI.IsAdmin: boolean;
begin
  result := DSiIsAdmin;
end;


initialization
  {$IFDEF WINDOWS}
   SystemAPI := TWindowsAPI.Create;
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
   SystemAPI := TLinuxAPI.Create;
  {$ENDIF LINUX}

finalization
  FreeAndNil(SystemAPI);
end.

