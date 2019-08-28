unit dictstringlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TDictStringList }

  TDictStringList = class(TStringList)
    procedure SetKeyValuePair(Key:String; Value:String);
    function GetValue(Key:String):String;
    function GetKey(Value:String):String;
    function GetListIndexForKey(Key:String):integer;
  end;


implementation

{ TDictStringList }

procedure TDictStringList.SetKeyValuePair(Key: String; Value: String);
begin
  Add(Key + '=' + Value);
end;

function TDictStringList.GetValue(Key: String): String;
begin
  Result := Values[Key];
end;

function TDictStringList.GetKey(Value: String): String;
begin
  Result := Names[Value];
end;

function TDictStringList.GetListIndex(Key: String): integer;
begin
  Result := IndexOfName(Key);
end;

end.

