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
  Process,
  //osfunclin,
  {$IFDEF OPSISCRIPT}
  ostxstringlist,
  {$ENDIF OPSISCRIPT}
  OSProcessux,
  osparserhelper,
  oslog,
  LazFileUtils,
  fileutil,
  unix,
  baseunix,
  osregex,
  SysUtils;

function checkForMacosDependencies(var Errstr: string): boolean;
function getProfilesDirListMac: TStringList;
function getMacosProcessList: TStringList;
function getMacosProcessByPid(pid: DWORD): string;
function getMacosVersionMap: TStringList;
function GetMacosVersionInfo: string;
function isMounted(mountpoint: string): boolean;
function which(target: string; var pathToTarget: string): boolean;
function mountSmbShare(mymountpoint, myshare, mydomain, myuser, mypass, myoption: string)
  : integer;
function umount(mymountpoint: string): integer;
function os_shutdown(): boolean;
function getMacLang(var mylang: string; var report: string): boolean;

implementation

uses
  {$IFDEF OPSISCRIPT}
  osparser,
  {$ENDIF OPSISCRIPT}
  {$IFDEF GUI}
  Graphics,
  //osbatchgui,
  //osinteractivegui,
  //osshowsysinfo,
  {$ENDIF GUI}
  null;

function which(target: string; var pathToTarget: string): boolean;
var
  str: string;
  exitcode: longint;
  cmd: string;
  path: string;
begin
  Result := False;
  pathToTarget := '';
  (*
  cmd := '/bin/bash -c "';
  cmd := cmd + 'set PATH=''/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin'' ;';
  cmd := cmd + 'which '+target+' || exit $?"';
  *)
  path := '/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin';
  str := FileSearch(target, path);
  if fileexists(trim(str)) then
  begin
    Result := True;
    pathToTarget := trim(str);
  end;
end;


function checkForMacosDependencies(var Errstr: string): boolean;
var
  exitcode: longint;
  output: string;
  outint: integer;
  cmd: string;
begin
  Result := True;
  Errstr := '';
  // check ip
  // https://superuser.com/questions/687310/ip-command-in-mac-os-x-terminal
  // https://github.com/brona/iproute2mac
  // curl --remote-name -L https://github.com/brona/iproute2mac/raw/master/src/ip.py
  // $ chmod +x ip.py
  // $ mv ip.py /usr/local/bin/ip
  if not which('ip', errstr) then
  begin
    exitcode := RunCommandIndir('', 'curl',
      ['-s', '-o', '/usr/local/bin/ip', '-L',
      'https://github.com/brona/iproute2mac/raw/master/src/ip.py'], output, outint, []);
    exitcode := RunCommandIndir('', 'chmod', ['-x', ''], output, outint, []);
    if not which('ip', errstr) then
      Result := False;
    (*
    cmd := 'curl -s -o /usr/local/bin/ip -L https://github.com/brona/iproute2mac/raw/master/src/ip.py';
    exitcode := RunCommandCaptureOutGetExitcode(cmd);
    cmd := 'chmod +x /usr/local/bin/ip';
    exitcode := RunCommandCaptureOutGetExitcode(cmd);
    if not which('ip', errstr) then result := false;
    *)
  end;
end;

function getMacosProcessList: TStringList;
var
  resultstring, pidstr, userstr, cmdstr, fullcmdstr: string;
  pscmd, report: string;
  (*
  {$IFDEF OPSISCRIPT}
  outlines: TXStringlist;
  lineparts: TXStringlist;
  {$ELSE OPSISCRIPT}
  outlines: TStringlist;
  lineparts: TStringlist;
  {$ENDIF OPSISCRIPT}
  *)
  outlines: TStringList;
  lineparts: TStringList;
  ExitCode: longint;
  i, k: integer;
begin
  try
    try
      Result := TStringList.Create;
      (*
      {$IFDEF OPSISCRIPT}
      outlines := TXStringList.Create;
      lineparts := TXStringList.Create;
      {$ELSE OPSISCRIPT}
      outlines := TStringList.Create;
      lineparts := TStringList.Create;
      {$ENDIF OPSISCRIPT}
      *)
      outlines := TStringList.Create;
      lineparts := TStringList.Create;
      pscmd := 'ps -eco pid,user,comm';
      {$IFDEF OPSISCRIPT}
      if not RunCommandAndCaptureOut(pscmd, True, TXStringlist(outlines),
        report, SW_HIDE, ExitCode) then
      {$ELSE OPSISCRIPT}
      if not RunCommandAndCaptureOut(pscmd, True, outlines, report,
        SW_HIDE, ExitCode) then
      {$ENDIF OPSISCRIPT}

      begin
        LogDatei.log('Error: ' + Report + 'Exitcode: ' + IntToStr(ExitCode), LLError);
      end
      else
      begin
        
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
            for k := 0 to lineparts.Count - 1 do
            begin
              if k = 0 then
                pidstr := lineparts.Strings[k]
              else if k = 1 then
                userstr := lineparts.Strings[k]
              else if k = 2 then
                cmdstr := lineparts.Strings[k]
              else
                fullcmdstr := fullcmdstr + lineparts.Strings[k] + ' ';
            end;
            resultstring := cmdstr + ';' + pidstr + ';' + userstr + ';' + fullcmdstr;
            LogDatei.log(resultstring, LLDebug3);
            //resultstring := lineparts.Strings[0] + ';';
            //resultstring := resultstring + lineparts.Strings[1] + ';';
            //resultstring := resultstring + lineparts.Strings[2] + ';';
            Result.Add(resultstring);
          end;
        
        LogDatei.log('', LLDebug);
      end;
    except
      on E: Exception do
      begin
        Logdatei.Log('Exception in getLinProcessList, system message: "' +
          E.Message + '"',
          LLError);
      end
    end;
  finally
    outlines.Free;
    lineparts.Free;
  end;
end;

function getMacosProcessByPid(pid: DWORD): string;
var
  resultstring, pidstr, userstr, cmdstr, fullcmdstr: string;
  pscmd, report: string;
  {$IFDEF OPSISCRIPT}
  outlines: TXStringlist;
  //lineparts: TXStringlist;
  {$ELSE OPSISCRIPT}
  outlines: TStringList;

  {$ENDIF OPSISCRIPT}
  lineparts: TStringList;
  ExitCode: longint;
  i, k: integer;
begin
  try
    try
      pidstr := IntToStr(pid);
      Result := '';
      {$IFDEF OPSISCRIPT}
      outlines := TXStringList.Create;
      //lineparts := TXStringList.Create;
      {$ELSE OPSISCRIPT}
      outlines := TStringList.Create;

      {$ENDIF OPSISCRIPT}
      lineparts := TStringList.Create;
      pscmd := 'ps -eco pid,user,comm';
      if not RunCommandAndCaptureOut(pscmd, True, outlines, report,
        SW_HIDE, ExitCode, False, 2) then
      begin
        LogDatei.log('Error: ' + Report + 'Exitcode: ' + IntToStr(ExitCode), LLError);
      end
      else
      begin
        //
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
            if pidstr = lineparts.Strings[0] then
              Result := lineparts.Strings[2];
          end;
        //
        //LogDatei.log('', LLDebug3);
      end;
    except
      on E: Exception do
      begin
        Logdatei.Log('Exception in getLinProcessList, system message: "' +
          E.Message + '"',
          LLError);
      end
    end;
  finally
    outlines.Free;
    lineparts.Free;
  end;
end;


function getProfilesDirListMac: TStringList;
var
  resultstring: string;
  cmd, report, user: string;
  resultlines1, resultlines3: TStringList;
  {$IFDEF OPSISCRIPT}
  resultlines2: TXStringList;
  {$ELSE OPSISCRIPT}
  resultlines2: TStringList;
  {$ENDIF OPSISCRIPT}

  ExitCode: longint;
  i, k: integer;
begin
  Result := TStringList.Create;
  {$IFDEF OPSISCRIPT}
  resultlines2 := TXStringList.Create;
  {$ELSE OPSISCRIPT}
  resultlines2 := TStringList.Create;
  {$ENDIF OPSISCRIPT}
  //resultlines3 := TStringList.Create;
  resultlines1 := FindAllDirectories('/Users', False);
  cmd := 'dscl . list /Users';
  if not RunCommandAndCaptureOut(cmd, True, resultlines2, report,
    SW_HIDE, ExitCode, False, 2) then
  begin
    LogDatei.log('Error: ' + Report + 'Exitcode: ' + IntToStr(ExitCode), LLError);
  end
  else
  begin
     {$IFDEF OPSISCRIPT}
    resultlines2 := TXStringlist(removeFromListByContainingRegex('^_\S*', resultlines2));
  {$ELSE OPSISCRIPT}
    resultlines2 := removeFromListByContainingRegex('^_\S*', resultlines2);
  {$ENDIF OPSISCRIPT}

    for i := 0 to resultlines2.Count - 1 do
    begin
      user := resultlines2.Strings[i];
      resultstring := getCommandResult('dscl . -read /Users/' + user +
        ' NFSHomeDirectory');
      resultstring := trim(copy(resultstring, 18, length(resultstring)));
      for k := 0 to resultlines1.Count - 1 do
      begin
        LogDatei.log_prog(resultlines1.Strings[k] + ' = ' + resultstring, LLdebug);
        if resultlines1.Strings[k] = resultstring then
          Result.Add(resultstring);
      end;
    end;
  end;
  //LogDatei.log('getProfilesDirListMac is not implemented on macos', LLError);
end;

function getMacosVersionMap: TStringList;
var
  resultstring: string;
  cmd, report: string;
  outlines, lineparts: TStringList;
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

function GetMacosVersionInfo: string;
begin
  Result := trim(getCommandResult('sw_vers -productVersion'));
end;

function isMounted(mountpoint: string): boolean;
var
  output: string;
  exename: string;
  commands: array of string;
begin
  Result := False;
  if not RunCommand('/sbin/mount', [], output) then
    writeln('Mount run error')
  else
  begin
    if output.Contains(mountpoint) then
      Result := True;
  end;
end;

function mountSmbShare(mymountpoint, myshare, mydomain, myuser, mypass,
  myoption: string): integer;
var
  cmd, report: string;
  outlines: TStringList;
  ExitCode: longint;
  i: integer;
begin
  outlines := TStringList.Create;
  Result := -1;
  try
    if not directoryexists(mymountpoint) then
      mkdir(mymountpoint);
  except
    LogDatei.log('Error: could not create moutpoint: ' + mymountpoint, LLError);
  end;
  // todo: controling smb version ; saving pass to file
  if pos('//', myshare) > 0 then
    myshare := copy(myshare, 3, length(myshare));
  if mydomain = '' then
    cmd := '/bin/bash -c "/sbin/mount_smbfs -N //' + myuser + ':' +
      mypass + '@' + myshare + ' ' + mymountpoint + '"'
  else
    cmd := '/bin/bash -c "/sbin/mount_smbfs -N //' + myuser + ':' +
      mypass + '@' + myshare + ' ' + mymountpoint + '"';
  //cmd := '/bin/bash -c "/sbin/mount_smbfs -N //' +mydomain+'\;'+ myuser+':'+mypass+'@'+myshare+' '+mymountpoint+'"';

  Logdatei.Log('calling: ' + cmd, LLNotice);
  //if not RunCommandAndCaptureOut(cmd, True, TXStringlist(outlines), report,
  //if not RunCommandAndCaptureOut(cmd, True, outlines, report,
  {$IFDEF OPSISCRIPT}
  if not RunCommandAndCaptureOut(cmd, True, TXStringlist(outlines),
    report, SW_HIDE, ExitCode) then
      {$ELSE OPSISCRIPT}
  if not RunCommandAndCaptureOut(cmd, True, outlines, report, SW_HIDE, ExitCode) then
      {$ENDIF OPSISCRIPT}
  begin
    LogDatei.log('Error: ' + Report + 'Exitcode: ' + IntToStr(ExitCode), LLError);
    Result := -1;
  end
  else
  begin
    
    LogDatei.log('', LLDebug);
    LogDatei.log('output:', LLDebug);
    LogDatei.log('--------------', LLDebug);
    for i := 0 to outlines.Count - 1 do
    begin
      LogDatei.log(outlines.strings[i], LLDebug);
    end;
    
    LogDatei.log('', LLDebug);
    Result := ExitCode;
  end;
  outlines.Free;
end;

function umount(mymountpoint: string): integer;
var
  cmd, report: string;
  outlines: TStringList;
  ExitCode: longint;
  i: integer;
begin
  outlines := TStringList.Create;
  Result := -1;
  cmd := '/bin/bash -c "/sbin/umount ' + mymountpoint + '"';
  LogDatei.log('calling: ' + cmd, LLNotice);
  //if not RunCommandAndCaptureOut(cmd, True, TXStringlist(outlines), report,
  //if not RunCommandAndCaptureOut(cmd, True, outlines, report,
  {$IFDEF OPSISCRIPT}
  if not RunCommandAndCaptureOut(cmd, True, TXStringlist(outlines),
    report, SW_HIDE, ExitCode) then
      {$ELSE OPSISCRIPT}
  if not RunCommandAndCaptureOut(cmd, True, outlines, report, SW_HIDE, ExitCode) then
      {$ENDIF OPSISCRIPT}
  begin
    LogDatei.log('Error: ' + Report + 'Exitcode: ' + IntToStr(ExitCode), LLError);
    Result := -1;
  end
  else
  begin
    
    LogDatei.log('', LLDebug);
    LogDatei.log('output:', LLDebug);
    LogDatei.log('--------------', LLDebug);
    for i := 0 to outlines.Count - 1 do
    begin
      LogDatei.log(outlines.strings[i], LLDebug);
    end;
    
    LogDatei.log('', LLDebug);
    Result := ExitCode;
  end;
  outlines.Free;
end;

function os_shutdown(): boolean;
var
  exitcode: integer;
  exitcmd: string;
begin

  if LogDatei <> nil then
  begin
    LogDatei.LogSIndentLevel := 0;
    Logdatei.Log('============   ' + ExtractFileNameOnly(ParamStr(0)) +
      ' shutdown regularly and direct. Time ' + FormatDateTime(
      'yyyy-mm-dd  hh:mm:ss ', now) + '.', LLessential);

    sleep(1000);
    //LogDatei.Free;
    //LogDatei := nil;
  end;
  exitcmd := FindDefaultExecutablePath('shutdown');
  if exitcmd = '' then
    exitcmd := '/sbin/shutdown';
  if not FileExistsUTF8(exitcmd) then
    exitcmd := '/usr/sbin/shutdown';
  if not FileExistsUTF8(exitcmd) then
    exitcmd := '/usr/bin/shutdown';
  exitcmd := exitcmd + ' -h +1 opsi-reboot';
  LogDatei.log('Exit command is: ' + exitcmd, LLDebug2);
  exitcode := fpSystem(exitcmd);
  if exitcode = 0 then
  begin
    if LogDatei <> nil then
    begin
      LogDatei.Free;
      LogDatei := nil;
    end;
    Result := True;
    //Fehler := '';
  end
  else
  begin
    LogDatei.log('Got exitcode: ' + IntToStr(exitcode) + ' for command' + exitcmd,
      LLWarning);
    exitcmd := '/sbin/shutdown -h now';
    exitcode := fpSystem(exitcmd);
    if exitcode = 0 then
    begin
      if LogDatei <> nil then
      begin
        LogDatei.Free;
        LogDatei := nil;
      end;
      Result := True;
      //Fehler := '';
    end
    else
    begin
      LogDatei.log('Got exitcode: ' + IntToStr(exitcode) + ' for command' + exitcmd,
        LLWarning);
      Result := False;
      LogDatei.log('Got exitcode: ' + IntToStr(fpgetErrno) +
        ' for command' + exitcmd,
        LLWarning);
      //Fehler := 'Error no.: ' + IntToStr(fpgetErrno);
      if LogDatei <> nil then
      begin
        LogDatei.Free;
        LogDatei := nil;
      end;
    end;
  end;
end;


function getMacLang(var mylang: string; var report: string): boolean;
var
  outlines: TStringList;
  exitcode: integer;
begin
  Result := False;
  outlines := TStringList.Create;
  {$IFDEF OPSISCRIPT}
  if RunCommandAndCaptureOut('/bin/bash -c "defaults read -g AppleLanguages"',
    True, TXStringlist(outlines), report, 0, exitcode) then
  {$ELSE OPSISCRIPT}
  if RunCommandAndCaptureOut('/bin/bash -c "defaults read -g AppleLanguages"',
    True, outlines, report, 0, exitcode) then
  {$ENDIF OPSISCRIPT}
  begin
    if outlines.Count > 2 then
    begin
      mylang := trim(outlines.Strings[1]);
      mylang := copy(mylang, 2, 2);
      Result := True;
      report := 'Detected default primary lang on macos: ' + mylang;
    end
    else
      report := 'Unexpected Result at macos lang detection: ' + outlines.Text;
  end
  else
    report := 'Failed macos lang detection: ' + report;
  FreeAndNil(outlines);
end;

end.
