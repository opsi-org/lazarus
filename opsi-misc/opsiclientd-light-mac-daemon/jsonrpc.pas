unit JSONrpc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, JSONRequest;

type



  { TJSONrpc }

  TJSONrpc = class(TObject)
    FJsonrpc: string;
    FMethod: string;
    FParams: TJSONArray;
    FID: integer;
    FResult: TJSONData;
    FError: TJSONObject;
    FRequest: TJSONRequest;
    FResponse: TJSONObject;
    constructor Create;
    destructor Destroy;override;
    procedure CreateRequest(aMethod: string; const theParams: array of const; aID:integer);
    procedure CreateResponse(aResult: TJSONData; aError: TJSONObject; aID: integer);
    procedure ReadRequest(aJSONRequest: TStream);overload;
    procedure WriteRequest(aMethod: string; const theParams: array of const; aID:integer);overload;

    //procedure SetRquest()
    function GetRequest: TStream;
    procedure ReadResponse(aJSONRequest: TStream);
    function ReadFromStringStream(aStringStream: TStringStream):TJSONObject;
    //ReadRequestFromStream()
    function WriteToStringStream(aJSONObject: TJSONObject): TStringStream;
    //property Request: TStream read GetRequest write SetRequest;
    //function WriteResponse;
    //procedure ReadRequest;
    //procedure ReadResponse;
  end;

implementation


{ TJSONrpc }

constructor TJSONrpc.Create;
begin
  FRequest := TJSONObject.Create;
  FResponse := TJSONObject.Create;
  inherited Create;
end;

destructor TJSONrpc.Destroy;
begin
  FreeAndNil(FRequest);
  FreeAndNil(FResponse);
  inherited Destroy;
end;

procedure TJSONrpc.CreateRequest(aMethod: string;
  const theParams: array of const; aID: integer);
begin
  FRequest.Add('method', aMethod);
  FRequest.Add('params', TJSONArray.Create(theParams));
  FRequest.Add('id', aID);
end;

procedure TJSONrpc.SetRequest(aJSONRequest: TStream);
begin
  FRequest := TJSONObject(GetJSON(aJSONRequest));
  FMethod := FRequest.Strings['method'];//JSONrpc.FindPath('method').AsString;
  FParams := FRequest.Arrays['params'];//JSONrpc.FindPath('params');
  FID := FRequest.Integers['id'];
end;

procedure TJSONrpc.SetRequest(aMethod: string; const theParams: array of const;
  aID: integer);
begin
  FMethod := aMethod;
  FParams := TJSONArray.Create(theParams);
  FID := aID;
  FRequest.Add('method', FMethod);
  FRequest.Add('params', FParams);
  FRequest.Add('id', FID);
end;

function TJSONrpc.GetRequest: TStream;
begin

end;

procedure TJSONrpc.ReadResponse(aJSONRequest: TStream);
begin

end;

procedure TJSONrpc.CreateResponse(aResult: TJSONData; aError: TJSONObject;
  aID: integer);
begin
  FResponse.Add('result', aResult);
  FResponse.Add('error', aError);
  FResponse.Add('id', aID);
end;

function TJSONrpc.ReadFromStringStream(aStringStream: TStringStream
  ): TJSONObject;
begin
  Result := TJSONObject(GetJSON(aStringStream));
end;

function TJSONrpc.WriteToStringStream(aJSONObject: TJSONObject): TStringStream;
begin
  Result := TStringStream.Create(aJSONObject.AsJSON);
end;

end.

