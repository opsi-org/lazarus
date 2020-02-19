unit OpsiJSONRequest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, OpsiJsonrpcObject;

type

{ TOpsiJSONRequest }

  TOpsiJSONRequest = class(TOpsiJSONrpcObject)
  private
    //function GetID: integer;
    function GetMethod:string;
    function GetParams: TJSONArray;
    //procedure SetID(aID: integer);
    procedure SetMethod(aMethod: string);
    procedure SetParams(theParams: TJSONArray);
  published
    property Method: string read GetMethod write SetMethod;
    property Params: TJSONArray read GetParams write SetParams;
    //property ID: integer read GetID write SetID;
  public
    //constructor Create(aStream: TStream);overload;
    constructor Create(aMethod: string; const theParams: array of const; aID: integer);overload;
    //procedure SaveToMemoryStream(aMemoryStream: TMemoryStream);
  end;


implementation

{ TOpsiJSONRequest }

//procedure TOpsiJSONRequest.SetID(aID: integer);
//begin
//  Add('id', aID);
//end;

procedure TOpsiJSONRequest.SetMethod(aMethod: string);
begin
  Add('method', aMethod);
end;

procedure TOpsiJSONRequest.SetParams(theParams: TJSONArray);
begin
  Add('params', theParams);
end;

//constructor TOpsiJSONRequest.Create(aStream: TStream);
//begin
//  //inherited Create;
//  self := TOpsiJSONRequest(GetJSON(aStream));
//end;

constructor TOpsiJSONRequest.Create(aMethod: string;
  const theParams: array of const; aID: integer);
begin
  inherited Create;
  Method := aMethod;
  Params := TJSONArray.Create(theParams);
  ID := aID;
end;

//procedure TOpsiJSONRequest.SaveToMemoryStream(aMemoryStream: TMemoryStream);
//var
//  StringStream: TStringStream;
//begin
//  StringStream := TStringStream.Create(self.AsJSON);
//  aMemoryStream.LoadFromStream(StringStream);
//  FreeAndNil(StringStream);
//end;

//function TOpsiJSONRequest.GetID: integer;
//begin
//  Result := Integers['id'];
//end;

function TOpsiJSONRequest.GetMethod: string;
begin
  Result := Strings['method'];
end;

function TOpsiJSONRequest.GetParams: TJSONArray;
begin
  Result := Arrays['params'];
end;



end.

