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
  oslog;

function isIPNumber(expr : string) : boolean;
function IsValidEmail(const Value: string): Boolean;

implementation

function isIPNumber(expr : string) : boolean;
begin
  result := false;
  if IsIP(expr) then result := true;
  if IsIP6(expr) then result := true;
end;


// http://www.delphitricks.com/source-code/internet/validate_email_address.html
function IsValidEmail(const Value: string): Boolean;

  function CheckAllowed(const s: string): Boolean;
    var i: Integer;
    begin
      Result:= false;
      for i:= 1 to Length(s) do
        if not (s[i] in ['a'..'z',
                         'A'..'Z',
                         '0'..'9',
                         '_',
                         '-',
                         '.']) then
         begin
             LogDatei.log('Warning: Not allowed chars in part: '+s+' in eMail: '+Value,LLwarning);
             Exit;
         end;
      Result:= true;
    end;

var
  i: Integer;
  NamePart, ServerPart: string;
begin
  Result:= False;
  i:=Pos('@', Value);
  if i=0 then
  begin
    LogDatei.log('Warning: No "@" in eMail: '+Value,LLwarning);
    Exit;
  end;
  NamePart:=Copy(Value, 1, i-1);
  LogDatei.log('Debug: namePart: '+NamePart+' in  eMail: '+Value,LLdebug2);
  ServerPart:=Copy(Value, i+1, Length(Value));
  LogDatei.log('Debug: ServerPart: '+ServerPart+' in  eMail: '+Value,LLdebug2);
  if (Length(NamePart)=0) then
  begin
    LogDatei.log('Warning: Empty namePart: '+NamePart+' in  eMail: '+Value,LLwarning);
    Exit;
  end;
  if  ((Length(ServerPart)<5)) then
  begin
    LogDatei.log('Warning: ServerPart < 5 chars: '+ServerPart+' in  eMail: '+Value,LLwarning);
    Exit;
  end;
  i:=Pos('.', ServerPart);
  if (i=0) then
  begin
    LogDatei.log('Warning: ServerPart has no dot: '+ServerPart+' in  eMail: '+Value,LLwarning);
    Exit;
  end;
  if (i>(Length(serverPart)-2)) then
  begin
    LogDatei.log('Warning: ServerPart has not a 2 char top level domain: '+ServerPart+' in  eMail: '+Value,LLwarning);
    Exit;
  end;
  Result:= CheckAllowed(NamePart) and CheckAllowed(ServerPart);
end;

end.

