unit osurlparser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uriparser;

function parseUrl(url : string) : TStringList;
function createUrl(urlcomponents : TStringList) : string;

implementation

function parseUrl(url : string) : TStringList;
var
  uriobj : TUri;
  index : integer;
begin
  uriobj := ParseURI(url);

  result := TStringList.Create;

  try
    result.Add('Protocol='+uriobj.Protocol);
    result.Add('Username='+uriobj.Username);
    result.Add('Password='+uriobj.Password);
    result.Add('Host='+uriobj.Host);
    result.Add('Port='+IntToStr(uriobj.Port));
    result.Add('Path='+uriobj.Path);
    result.Add('Document='+uriobj.Document);
    result.Add('Params='+uriobj.Params);
    result.Add('Bookmark='+uriobj.Bookmark);

    for index := 0 to result.Count-1 do
        WriteLn(result[index]);

  finally
    if Assigned(result) then
      result.Free;
  end;
end;

function createUrl(urlcomponents : TStringList) : string;
var
  uriobj : TUri;
begin
  uriobj.Protocol := urlcomponents.ValueFromIndex[0];
  uriobj.Username := urlcomponents.ValueFromIndex[1];
  uriobj.Password := urlcomponents.ValueFromIndex[2];
  uriobj.Host := urlcomponents.ValueFromIndex[3];
  uriobj.Port := StrToInt(urlcomponents.ValueFromIndex[4]);
  uriobj.Path := urlcomponents.ValueFromIndex[5];
  uriobj.Document := urlcomponents.ValueFromIndex[6];
  uriobj.Params := urlcomponents.ValueFromIndex[7];
  uriobj.Bookmark := urlcomponents.ValueFromIndex[8];

  result := EncodeURI(uriobj);
end;

end.

