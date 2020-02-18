unit OpsiJSONRequest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, OpsiJSONrpcObject, OpsiJSONrpcArray;

type

{ TOpsiJSONRequest }

  TOpsiJSONRequest = class(TOpsiJSONrpcObject)
  private
    function GetMethod:string;
    function GetParams: TOpsiJSONrpcArray;
    procedure SetMethod(aMethod: string);
    procedure SetParams(theParams: TOpsiJSONrpcArray);
  published
    property Method: string read GetMethod write SetMethod;
    property Params: TOpsiJSONrpcArray read GetParams write SetParams;
  public
    constructor Create(aMethod: string; const theParams: array of const; aID: integer);overload;
  end;


implementation

{ TOpsiJSONRequest }


procedure TOpsiJSONRequest.SetMethod(aMethod: string);
begin
  Add('method', aMethod);
end;

procedure TOpsiJSONRequest.SetParams(theParams: TOpsiJSONrpcArray);
begin
  Add('params', theParams);
end;


constructor TOpsiJSONRequest.Create(aMethod: string;
  const theParams: array of const; aID: integer);
begin
  inherited Create;
  Method := aMethod;
  Params := TOpsiJSONrpcArray.Create(theParams);
  ID := aID;
end;


function TOpsiJSONRequest.GetMethod: string;
begin
  Result := Strings['method'];
end;

function TOpsiJSONRequest.GetParams: TOpsiJSONrpcArray;
begin
  Result := TOpsiJSONrpcArray(Arrays['params']);
end;



end.

