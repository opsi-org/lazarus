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

implementation

function isIPNumber(expr : string) : boolean;
begin
  result := false;
  if IsIP(expr) then result := true;
  if IsIP6(expr) then result := true;
end;

end.

