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
  {$IFDEF OPSISCRIPT}
  osfunc,
  {$ENDIF OPSISCRIPT}
  oslog,
  SysUtils;

function checkForMacosDependencies(var Errstr : string) : boolean;
function getProfilesDirListMac: TStringList;

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

function getProfilesDirListMac: TStringList;
var
  resultstring: string;
  cmd, report: string;
  outlines, lineparts: TXStringlist;
  ExitCode: longint;
  i: integer;
begin
  Result := TStringList.Create;
  LogDatei.log('getProfilesDirListMac is not implemented on macos',LLError);
  (*
  outlines := TXStringList.Create;
  lineparts := TXStringList.Create;
  // we use the home directories from the passwd entries
  // get passwd
  cmd := 'getent passwd';
  if not RunCommandAndCaptureOut(cmd, True, outlines, report,
    SW_HIDE, ExitCode) then
  begin
    LogDatei.log('Error: ' + Report + 'Exitcode: ' + IntToStr(ExitCode), LLError);
  end
  else
  begin
    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 6;
    LogDatei.log('', LLDebug);
    LogDatei.log('output:', LLDebug);
    LogDatei.log('--------------', LLDebug);
    for i := 0 to outlines.Count - 1 do
    begin
      lineparts.Clear;
      LogDatei.log(outlines.strings[i], LLDebug);
      stringsplit(outlines.strings[i], ':', lineparts);
      //LogDatei.log(lineparts.Strings[2], LLDebug);
      // use only users with a pid >= 1000
      if StrToInt(lineparts.Strings[2]) >= 1000 then
      begin
        resultstring := lineparts.Strings[5];
        // use only existing direcories as profile
        if DirectoryExists(ExpandFileName(resultstring)) then
          Result.Add(ExpandFileName(resultstring));
      end;
    end;
    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 6;
    LogDatei.log('', LLDebug);
  end;
  outlines.Free;
  lineparts.Free;
  *)
end;


end.
