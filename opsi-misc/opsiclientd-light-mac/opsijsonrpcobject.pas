unit OpsiJSONrpcObject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser;

type

{ TOpsiJSONrpcObject }

  TOpsiJSONrpcObject = class(TJSONObject)
  private
    function GetID: integer;
    procedure SetID(aID: integer);
  published
    property ID: integer read GetID write SetID;
  public
    //constructor Create(aStream: TStream);virtual;
    procedure SaveToMemoryStream(aMemoryStream: TMemoryStream);
  end;


implementation

{ TOpsiJSONrpcObject }

procedure TOpsiJSONrpcObject.SetID(aID: integer);
begin
  Add('id', aID);
end;

//constructor TOpsiJSONrpcObject.Create(aStream: TStream);
//begin
//  inherited Create;
//  self := TOpsiJSONrpcObject(GetJSON(aStream));
//end;

//constructor TOpsiJSONrpcObject.Create(aID: integer);
//begin
//  inherited Create;
//  ID := aID;
//end;

procedure TOpsiJSONrpcObject.SaveToMemoryStream(aMemoryStream: TMemoryStream);
var
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create(self.AsJSON);
  aMemoryStream.LoadFromStream(StringStream);
  FreeAndNil(StringStream);
end;

function TOpsiJSONrpcObject.GetID: integer;
begin
  Result := Integers['id'];
end;


end.

