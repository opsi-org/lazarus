unit OckSystemAPI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, osLog;

type
  TSystemAPI = class(TObject)
    function IsAdmin:boolean;virtual;abstract;
    procedure RunApplication(const PathToExe:string);virtual;abstract;
    //function RunAsAdmin:boolean;virtual;abstract;
  end;



implementation


end.

