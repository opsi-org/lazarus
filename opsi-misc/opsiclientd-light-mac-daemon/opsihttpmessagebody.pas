unit OpsiHTTPMessageBody;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TOpsiHTTPMessageBody }

  TOpsiHTTPMessageBody = class
    Text : string;
    //HTML : TStringList;
    //JSON : TJSONObject;
    procedure SaveToMemoryStream(aMemoryStream: TMemoryStream);virtual;abstract;
  end;

implementation

{ TOpsiHTTPMessageBody }


end.

