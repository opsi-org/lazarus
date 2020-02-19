unit OpsiJSONResponse;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, OpsiJSONrpcObject;

type

  { TOpsiJSONResponse }

  TOpsiJSONResponse = class(TOpsiJSONrpcObject)
  private
    function GetError: TJSONObject;
    //function GetID: integer;
    function GetResult: TJSONData;
    procedure SetError(aError: TJSONObject);
    //procedure SetID(aID: integer);
    procedure SetResult(aResult: TJSONData);
  published
    property Error: TJSONObject read GetError write SetError;
    property Result : TJSONData read GetResult write SetResult;
    //property ID: integer read GetID write SetID;
  public
    //constructor Create(aStream: TStream);overload;
    constructor Create(aResult: TJSONData; aError: TJSONObject; aID: integer); overload;
    //procedure SaveToMemoryStream(aMemoryStream: TMemoryStream);
  end;

implementation


{ TOpsiJSONResponse }

function TOpsiJSONResponse.GetError: TJSONObject;
begin
  Result := Objects['error'];
end;

//function TOpsiJSONResponse.GetID: integer;
//begin
//  Result := Integers['id'];
//end;

function TOpsiJSONResponse.GetResult: TJSONData;
begin
  Result := FindPath('result');
end;

procedure TOpsiJSONResponse.SetError(aError: TJSONObject);
begin
  Add('error', aError);
end;

//procedure TOpsiJSONResponse.SetID(aID: integer);
//begin
//  Add('id', aID);
//end;

procedure TOpsiJSONResponse.SetResult(aResult: TJSONData);
begin
  Add('result', aResult);
end;

//constructor TOpsiJSONResponse.Create(aStream: TStream);
//begin
//  //inherited Create;
//  self := TOpsiJSONResponse(GetJSON(aStream));
//end;

constructor TOpsiJSONResponse.Create(aResult: TJSONData; aError: TJSONObject;
  aID: integer);
begin
  inherited Create;
  Result := aResult;
  Error := aError;
  ID := aID;
end;

//procedure TOpsiJSONResponse.SaveToMemoryStream(aMemoryStream: TMemoryStream);
//var
//  StringStream: TStringStream;
//begin
//  StringStream := TStringStream.Create(self.AsJSON);
//  aMemoryStream.LoadFromStream(StringStream);
//  FreeAndNil(StringStream);
//end;

end.

