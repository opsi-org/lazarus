// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: Rupert Roeder, detlef oertel

unit osnetutil;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  synaip,
  oslog,
  RegExpr;

function isValidFQDN(expr: string): boolean;
function isIPNumber(expr: string): boolean;
function IsValidEmail(const Value: string): boolean;

implementation

// Function to validate if the Domain Name is Fully Qualified
function isValidFQDN(expr: string): boolean;
var
  RegExprObj: TRegExpr;
begin
  Result := False;
  RegExprObj := TRegExpr.Create;
  RegExprObj.Expression :=
    '^[a-zA-Z0-9][a-zA-Z0-9\-_]{0,62}\.([a-zA-Z0-9][a-zA-Z0-9\-_]{0,62}\.){1,}[a-zA-Z]{2,63}$';
  if Length(expr) > 254 then
    Result := False
  else
  if RegExprObj.Exec(expr) then
    Result := True;
  RegExprObj.Free;
end;


function isIPNumber(expr: string): boolean;
begin
  Result := False;
  if IsIP(expr) then Result := True;
  if IsIP6(expr) then Result := True;
end;


// http://www.delphitricks.com/source-code/internet/validate_email_address.html
function IsValidEmail(const Value: string): boolean;

  function CheckAllowed(const s: string): boolean;
  var
    i: integer;
  begin
    Result := False;
    for i := 1 to Length(s) do
      if not (s[i] in ['a'..'z', 'A'..'Z', '0'..'9', '_', '-', '.']) then
      begin
        LogDatei.log('Warning: Not allowed chars in part: ' + s +
          ' in eMail: ' + Value, LLwarning);
        Exit;
      end;
    Result := True;
  end;

var
  i: integer;
  NamePart, ServerPart: string;
begin
  Result := False;
  i := Pos('@', Value);
  if i = 0 then
  begin
    LogDatei.log('Warning: No "@" in eMail: ' + Value, LLwarning);
    Exit;
  end;
  NamePart := Copy(Value, 1, i - 1);
  LogDatei.log('Debug: namePart: ' + NamePart + ' in  eMail: ' + Value, LLdebug2);
  ServerPart := Copy(Value, i + 1, Length(Value));
  LogDatei.log('Debug: ServerPart: ' + ServerPart + ' in  eMail: ' + Value, LLdebug2);
  if (Length(NamePart) = 0) then
  begin
    LogDatei.log('Warning: Empty namePart: ' + NamePart + ' in  eMail: ' +
      Value, LLwarning);
    Exit;
  end;
  if ((Length(ServerPart) < 5)) then
  begin
    LogDatei.log('Warning: ServerPart < 5 chars: ' + ServerPart +
      ' in  eMail: ' + Value, LLwarning);
    Exit;
  end;
  i := Pos('.', ServerPart);
  if (i = 0) then
  begin
    LogDatei.log('Warning: ServerPart has no dot: ' + ServerPart +
      ' in  eMail: ' + Value, LLwarning);
    Exit;
  end;
  if (i > (Length(serverPart) - 2)) then
  begin
    LogDatei.log('Warning: ServerPart has not a 2 char top level domain: ' +
      ServerPart + ' in  eMail: ' + Value, LLwarning);
    Exit;
  end;
  Result := CheckAllowed(NamePart) and CheckAllowed(ServerPart);
end;

end.
