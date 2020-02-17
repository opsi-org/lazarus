unit OpsiJSONRequest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, OpsiJsonrpcObject;

type

{ TOpsiJSONRequest }

  TOpsiJSONRequest = class(TOpsiJSONrpcObject)
  private
    function GetMethod:string;
    function GetParams: TJSONArray;
    procedure SetMethod(aMethod: string);
    procedure SetParams(theParams: TJSONArray);
  published
    property Method: string read GetMethod write SetMethod;
    property Params: TJSONArray read GetParams write SetParams;
  public
    constructor Create(aMethod: string; const theParams: array of const; aID: integer);overload;
  end;


implementation

{ TOpsiJSONRequest }


procedure TOpsiJSONRequest.SetMethod(aMethod: string);
begin
  Add('method', aMethod);
end;

procedure TOpsiJSONRequest.SetParams(theParams: TJSONArray);
begin
  Add('params', theParams);
end;


constructor TOpsiJSONRequest.Create(aMethod: string;
  const theParams: array of const; aID: integer);
begin
  inherited Create;
  Method := aMethod;
  Params := TJSONArray.Create(theParams);
  ID := aID;
end;


function TOpsiJSONRequest.GetMethod: string;
begin
  Result := Strings['method'];
end;

function TOpsiJSONRequest.GetParams: TJSONArray;
begin
  Result := Arrays['params'];
end;



end.

