unit osurlparser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uriparser;

function parseUrl(url : string) : TStringList; // Return url components
function createUrl(urlcomponents : TStringList) : string;  // Returns url string

implementation

function parseUrl(url : string) : TStringList;
// Return url components
var
  uriobj : TUri;
  tmpstr : string;
begin
  uriobj := ParseURI(url);

  result := TStringList.Create;
  result.Add('Protocol='+uriobj.Protocol);
  result.Add('Username='+uriobj.Username);
  result.Add('Password='+uriobj.Password);
  { In the case of <host>:<path> we will get the host with trailing colon
   and (by the way) it should not be any where else }
  tmpstr :=  uriobj.Host;
  tmpstr := StringReplace(tmpstr, ':', '', [rfReplaceAll]);
  result.Add('Host='+ tmpstr);
  result.Add('Port='+IntToStr(uriobj.Port));
  result.Add('Path='+uriobj.Path);
  result.Add('Document='+uriobj.Document);
  result.Add('Params='+uriobj.Params);
  result.Add('Bookmark='+uriobj.Bookmark);
end;

function isInteger(value : string) : boolean;
// check whether the string contains integer
var
  count : integer;
begin
  Result:= False;
  for count:= 1 to length(value) do
  begin
    if value[count] in ['0'..'9'] then
      Result := True;
  end;
end;

function createUrl(urlcomponents : TStringList) : string;
// Returns url string
var
  uriobj : TUri;
begin
  // initialize uriobj.HasAuthority to true
  // it is not done in URIParser and results so to random results
  uriobj.HasAuthority:=true;
  uriobj.Protocol := urlcomponents.Values['protocol'];
  uriobj.Username := urlcomponents.Values['username'];
  uriobj.Password := urlcomponents.Values['password'];
  uriobj.Host := urlcomponents.Values['host'];

  if urlcomponents.IndexOfName('port') = -1 then
    uriobj.Port := 0
  else if isInteger(urlcomponents.Values['port']) then
    uriobj.Port := StrToInt(urlcomponents.Values['port'])
  else
    uriobj.Port := 0;
    
  uriobj.Path := urlcomponents.Values['path'];
  uriobj.Document := urlcomponents.Values['document'];
  uriobj.Params := urlcomponents.Values['params'];
  uriobj.Bookmark := urlcomponents.Values['bookmark'];

  result := EncodeURI(uriobj);
end;


end.

