unit osfuncmac;

// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: Rupert Roeder, detlef oertel
// credits: http://www.opsi.org/credits/



{$mode delphi}

interface


uses
  Classes,
  osfunclin,
  SysUtils;

function checkForMacosDependencies(var Errstr : string) : boolean;

implementation

uses
{$IFDEF GUI}
  graphics,
osbatchgui,
osinteractivegui,
osshowsysinfo,
{$ENDIF GUI}
 osparser ;


function checkForMacosDependencies(var Errstr : string) : boolean;
var
  exitcode : longint;
  cmd : string;
begin
  result := true;
  Errstr := '';
  // check ip
  // https://superuser.com/questions/687310/ip-command-in-mac-os-x-terminal
  // https://github.com/brona/iproute2mac
  // curl --remote-name -L https://github.com/brona/iproute2mac/raw/master/src/ip.py
  // $ chmod +x ip.py
  // $ mv ip.py /usr/local/bin/ip
  if not which('ip', errstr) then
  begin
    cmd := 'curl -s -o /usr/local/bin/ip -L https://github.com/brona/iproute2mac/raw/master/src/ip.py';
    exitcode := RunCommandCaptureOutGetExitcode(cmd);
    cmd := 'chmod +x /usr/local/bin/ip';
    exitcode := RunCommandCaptureOutGetExitcode(cmd);
    if not which('ip', errstr) then result := false;
  end;

end;

end.
