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
function getMacosProcessList: TStringList;
function getMacosProcessByPid(pid:DWORD): String;
function getMacosVersionMap: TStringList;
function GetMacosVersionInfo: String;

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

function getMacosProcessList: TStringList;
var
  resultstring, pidstr, userstr, cmdstr, fullcmdstr: string;
  pscmd, report: string;
  {$IFDEF OPSISCRIPT}
  outlines: TXStringlist;
  {$ELSE OPSISCRIPT}
  outlines: TStringlist;
  {$ENDIF OPSISCRIPT}
  lineparts: TXStringlist;
  ExitCode: longint;
  i,k: integer;
begin
  try
    try
      Result := TStringList.Create;
      {$IFDEF OPSISCRIPT}
      outlines := TXStringList.Create;
      {$ELSE OPSISCRIPT}
      outlines := TXStringList.Create;
      {$ENDIF OPSISCRIPT}
      lineparts := TXStringList.Create;
      pscmd := 'ps -eco pid,user,comm';
      if not RunCommandAndCaptureOut(pscmd, True, outlines, report,
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
        if outlines.Count > 0 then
        for i := 0 to outlines.Count - 1 do
        begin
          LogDatei.log(outlines.strings[i], LLDebug2);
          lineparts.Clear;
          resultstring := '';
          userstr := '';
          pidstr := '';
          cmdstr := '';
          fullcmdstr := '';
          stringsplitByWhiteSpace(trim(outlines.strings[i]), lineparts);
          for k := 0 to lineparts.Count-1 do
          begin
            if k = 0 then pidstr := lineparts.Strings[k]
            else if k = 1 then userstr := lineparts.Strings[k]
            else if k = 2 then cmdstr := lineparts.Strings[k]
            else fullcmdstr:= fullcmdstr+lineparts.Strings[k]+' ';
          end;
          resultstring := cmdstr+';'+pidstr+';'+userstr+';'+fullcmdstr;
          LogDatei.log(resultstring, LLDebug3);
          //resultstring := lineparts.Strings[0] + ';';
          //resultstring := resultstring + lineparts.Strings[1] + ';';
          //resultstring := resultstring + lineparts.Strings[2] + ';';
          Result.Add(resultstring);
        end;
        LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 6;
        LogDatei.log('', LLDebug);
      end;
    except
      on E: Exception do
        begin
          LogDatei.DependentAdd('Exception in getLinProcessList, system message: "' + E.Message + '"',
            LLError);
        end
    end;
  finally
    outlines.Free;
    lineparts.Free;
  end
end;

function getMacosProcessByPid(pid:DWORD): String;
var
  resultstring, pidstr, userstr, cmdstr, fullcmdstr: string;
  pscmd, report: string;
  {$IFDEF OPSISCRIPT}
  outlines: TXStringlist;
  {$ELSE OPSISCRIPT}
  outlines: TStringlist;
  {$ENDIF OPSISCRIPT}
  lineparts: TXStringlist;
  ExitCode: longint;
  i,k: integer;
begin
  try
    try
      pidstr := IntToStr(pid);
      Result := '';
      {$IFDEF OPSISCRIPT}
      outlines := TXStringList.Create;
      {$ELSE OPSISCRIPT}
      outlines := TXStringList.Create;
      {$ENDIF OPSISCRIPT}
      lineparts := TXStringList.Create;
      pscmd := 'ps -eco pid,user,comm';
      if not RunCommandAndCaptureOut(pscmd, True, outlines, report,
        SW_HIDE, ExitCode,false,2) then
      begin
        LogDatei.log('Error: ' + Report + 'Exitcode: ' + IntToStr(ExitCode), LLError);
      end
      else
      begin
        //LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 6;
        LogDatei.log('', LLDebug3);
        LogDatei.log('output:', LLDebug3);
        LogDatei.log('--------------', LLDebug3);
        if outlines.Count > 0 then
        for i := 0 to outlines.Count - 1 do
        begin
          LogDatei.log(outlines.strings[i], LLDebug3);
          lineparts.Clear;
          resultstring := '';
          userstr := '';
          //pidstr := '';
          cmdstr := '';
          fullcmdstr := '';
          stringsplitByWhiteSpace(trim(outlines.strings[i]), lineparts);
          if  pidstr = lineparts.Strings[0] then result :=  lineparts.Strings[2];
        end;
        //LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 6;
        //LogDatei.log('', LLDebug3);
      end;
    except
      on E: Exception do
        begin
          LogDatei.DependentAdd('Exception in getLinProcessList, system message: "' + E.Message + '"',
            LLError);
        end
    end;
  finally
    outlines.Free;
    lineparts.Free;
  end
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

function getMacosVersionMap: TStringList;
var
  resultstring: string;
  cmd, report: string;
  outlines, lineparts: TXStringlist;
  ExitCode: longint;
  i: integer;
begin
  Result := TStringList.Create;
  Result.Add('Release' + '=' + trim(getCommandResult('sw_vers -productVersion')));
  Result.Add('Build' + '=' + trim(getCommandResult('sw_vers -buildVersion')));
  Result.Add('kernel name' + '=' + trim(getCommandResult('uname -s')));
  Result.Add('node name' + '=' + trim(getCommandResult('uname -n')));
  Result.Add('kernel release' + '=' + trim(getCommandResult('uname -r')));
  Result.Add('kernel version' + '=' + trim(getCommandResult('uname -v')));
  Result.Add('machine' + '=' + trim(getCommandResult('uname -m')));
  Result.Add('processor' + '=' + trim(getCommandResult('uname -p')));
  //Result.Add('hardware platform' + '=' + trim(getCommandResult('uname -i')));
  Result.Add('operating system' + '=' + 'macOS');
end;

function GetMacosVersionInfo: String;
begin
  result := trim(getCommandResult('sw_vers -productVersion'));
end;

end.
