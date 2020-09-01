unit OpsiJSONMessageBody;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils , OpsiHTTPMessageBody, fpjson, jsonparser, OpsiJSONrpcObject;

type

  { TOpsiJSONMessageBody }

  TOpsiJSONMessageBody = class(TOpsiHTTPMessageBody)
    JSONrpcObject : TOpsiJSONrpcObject;
    constructor Create(aJSONrpcObject: TOPsiJSONrpcObject);
    //constructor Create(aURI: string; aPeerName: string);//creates a htmltestside giving back URI and PeerName
    procedure SaveToMemoryStream(aMemoryStream: TMemoryStream);override;
  end;

implementation

{ TOpsiJSONMessageBody }

constructor TOpsiJSONMessageBody.Create(aJSONrpcObject: TOpsiJSONrpcObject);
begin
  JSONrpcObject := aJSONrpcObject;
end;

procedure TOpsiJSONMessageBody.SaveToMemoryStream(aMemoryStream: TMemoryStream);
begin
  JSONrpcObject.SaveToMemoryStream(aMemoryStream);
end;

end.

