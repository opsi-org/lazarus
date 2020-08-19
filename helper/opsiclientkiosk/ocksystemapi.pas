unit OckSystemAPI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TSystemAPI = class(TObject)
    function IsAdmin:boolean;virtual;abstract;
    procedure RunApplication(const PathToExe:string);virtual;abstract;
  end;


implementation


end.

