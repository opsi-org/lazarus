unit OpsiJSONrpcArray;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser;

type

  { TOpsiJSONrpcArray }

  TOpsiJSONrpcArray = class(TJSONArray)
    function Find(const aParam: string): boolean;
  end;

implementation

{ TOpsiJSONrpcArray }

function TOpsiJSONrpcArray.Find(const aParam: string): boolean;
var
  Param: TJSONEnum;
begin
  Result := false;
  for Param in self do
  begin
    if Param.Value.AsString = aParam then
    begin
      Result := true;
      Break;
    end;
  end;
end;

end.

