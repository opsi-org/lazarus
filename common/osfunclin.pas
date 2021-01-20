unit osfunclin;

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
  Classes, SysUtils,
  {$IFNDEF SYNAPSE}
  IdBaseComponent,
  IdComponent,
  IdIPWatch,
  {$ENDIF SYNAPSE}
  sockets,
  {$IFDEF OPSISCRIPT}
  //osfunc,
  ostxstringlist,
  {$ENDIF OPSISCRIPT}
  unix,
  fileutil,
  LazFileUtils,
  oslog,
  osparserhelper,
  synsock,
  synaip,
  netdb,
  baseunix,
  process,
  OSProcessux,
  IniFiles,
  osprocesses;

function getProfilesDirListLin: TStringList;
function getLinProcessList: TStringList;
function KillProcessbyname(const exename: string; var found: integer): integer;
function KillProcessbypid(pid: DWORD): boolean;
function getProcessByPid(pid: DWORD): string;
function FileCheckDate
  (const Sourcefilename, Targetfilename: string; OverwriteIfEqual: boolean): boolean;
//procedure FindLocalIPData(var ipName: string; var address: string);



function getLinuxVersionMap: TStringList;
function getLinuxDistroType: string;
function Is64BitSystemLin: boolean;

function getHostnameLin: string;
function getMyHostEnt: netdb.THostEntry;
function GetIPFromHost(var HostName, IPaddr, WSAErr: string): boolean;
function linIsUefi: boolean;
function getMyIpByTarget(target: string): string;
function getMyIpByDefaultRoute: string;
function getMyIpDeciceByDefaultRoute: string;
function getPackageLock(timeoutsec: integer; kill: boolean): boolean;
//function which(target: string; var pathToTarget: string): boolean;
function getLinuxDistroName: string;
function getLinuxDistroRelease: string;
function getLinuxDistroDescription: string;
function os_shutdown(): boolean;

{$IFNDEF SYNAPSE}
var
  IdIPWatch1: TIdIPWatch;
{$ENDIF SYNAPSE}

implementation

uses
{$IFDEF OPSISCRIPT}
  osparser,
{$ENDIF OPSISCRIPT}
{$IFDEF GUI}
  Graphics,
{$IFDEF OPSISCRIPT}
  osbatchgui,
  osinteractivegui,
  osshowsysinfo,
{$ENDIF OPSISCRIPT}
{$ENDIF GUI}
  LResources;

function FileCheckDate
  (const Sourcefilename, Targetfilename: string; OverwriteIfEqual: boolean): boolean;
  (* Assumption: Both files exist *)
var
  Handle: integer;
  Date1, Date2: longint;

begin
  Handle := fileopen(Sourcefilename, fmOpenRead);
  Date1 := filegetdate(Handle);
  fileclose(handle);
  Handle := fileopen(Targetfilename, fmOpenRead);
  Date2 := filegetdate(Handle);
  fileclose(handle);
  if (Date2 < Date1) or ((Date2 = Date1) and OverwriteIfEqual) then
  begin
    Result := True;
  end
  else
  begin
    Result := False;
  end;
end;




function KillProcessbyname(const exename: string; var found: integer): integer;
var
  cmd, report: string;
  {$IFDEF OPSISCRIPT}
  outlines: TXStringlist;
  {$ELSE OPSISCRIPT}
  outlines: TStringList;
  {$ENDIF OPSISCRIPT}
  ExitCode: longint;
  i: integer;
begin
  {$IFDEF OPSISCRIPT}
  outlines := TXStringList.Create;
  {$ELSE OPSISCRIPT}
  outlines := TStringList.Create;
  {$ENDIF OPSISCRIPT}

  cmd := 'pkill -9 ' + exename;
  found := 0;
  if not RunCommandAndCaptureOut(cmd, True, outlines, report, SW_HIDE, ExitCode) then
  begin
    LogDatei.log('Error: ' + Report + 'Exitcode: ' + IntToStr(ExitCode), LLError);
    Result := ExitCode;
  end;
  cmd := 'pgrep ' + exename;
  found := 0;
  if not RunCommandAndCaptureOut(cmd, True, outlines, report, SW_HIDE, ExitCode) then
  begin
    LogDatei.log('Error: ' + Report + 'Exitcode: ' + IntToStr(ExitCode), LLError);
    Result := ExitCode;
  end
  else
  begin
    //LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 6;
    //LogDatei.log('', LLDebug);
    //LogDatei.log('output:', LLDebug);
    //LogDatei.log('--------------', LLDebug);
    for i := 0 to outlines.Count - 1 do
    begin
      LogDatei.log(outlines.strings[i], LLDebug);
      Inc(found);
    end;
    //LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 6;
    if found > 0 then
      LogDatei.log('Found after kill :' + IntToStr(found) + ' instances of ' +
        exename, LLError);
  end;
  outlines.Free;
  Result := ExitCode;
end;

function KillProcessbypid(pid: DWORD): boolean;
var
  cmd, report: string;
  {$IFDEF OPSISCRIPT}
  outlines: TXStringlist;
  {$ELSE OPSISCRIPT}
  outlines: TStringList;
  {$ENDIF OPSISCRIPT}
  ExitCode: longint;
  i: integer;
begin
  {$IFDEF OPSISCRIPT}
  outlines := TXStringList.Create;
  {$ELSE OPSISCRIPT}
  outlines := TStringList.Create;
  {$ENDIF OPSISCRIPT}
  cmd := 'kill -9 ' + IntToStr(pid);
  if not RunCommandAndCaptureOut(cmd, True, outlines, report, SW_HIDE, ExitCode) then
  begin
    LogDatei.log('Error: ' + Report + 'Exitcode: ' + IntToStr(ExitCode), LLError);
    Result := False;
  end
  else
  begin
    Result := True;
    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 6;
    LogDatei.log('', LLDebug);
    LogDatei.log('output:', LLDebug);
    LogDatei.log('--------------', LLDebug);
    for i := 0 to outlines.Count - 1 do
    begin
      LogDatei.log(outlines.strings[i], LLDebug);
    end;
    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 6;
    LogDatei.log('', LLDebug);
  end;
  outlines.Free;
end;



function getLinProcessList: TStringList;
var
  resultstring, pidstr, userstr, cmdstr, fullcmdstr: string;
  pscmd, report: string;
  {$IFDEF OPSISCRIPT}
  outlines: TXStringlist;
  //lineparts: TXStringlist;
  {$ELSE OPSISCRIPT}
  outlines: TStringList;
  //lineparts: TStringlist;
  {$ENDIF OPSISCRIPT}
  lineparts: TStringList;
  ExitCode: longint;
  i, k: integer;
begin
  try
    try
      Result := TStringList.Create;
      {$IFDEF OPSISCRIPT}
      outlines := TXStringList.Create;
      //lineparts := TXStringList.Create;
      {$ELSE OPSISCRIPT}
      outlines := TStringList.Create;
      //lineparts := TXStringList.Create;
      {$ENDIF OPSISCRIPT}
      lineparts := TStringList.Create;
      pscmd := 'ps -eo pid,user,comm,cmd:110';
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
        LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 6;
        LogDatei.log('', LLDebug);
      end;
    except
      on E: Exception do
      begin
        LogDatei.DependentAdd('Exception in getLinProcessList, system message: "' +
          E.Message + '"',
          LLError);
      end
    end;
  finally
    outlines.Free;
    lineparts.Free;
  end;
end;

function getProcessByPid(pid: DWORD): string;
var
  resultstring, pidstr, userstr, cmdstr, fullcmdstr: string;
  pscmd, report: string;
  {$IFDEF OPSISCRIPT}
  outlines: TXStringlist;
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
      {$ELSE OPSISCRIPT}
      outlines := TStringList.Create;
      {$ENDIF OPSISCRIPT}
      lineparts := TStringList.Create;
      pscmd := 'ps -eo pid,user,comm,cmd:110';
      if not RunCommandAndCaptureOut(pscmd, True, outlines, report,
        SW_HIDE, ExitCode, False, 2) then
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
            if pidstr = lineparts.Strings[0] then
              Result := lineparts.Strings[2];
          end;
        //LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 6;
        //LogDatei.log('', LLDebug3);
      end;
    except
      on E: Exception do
      begin
        LogDatei.DependentAdd('Exception in getLinProcessList, system message: "' +
          E.Message + '"',
          LLError);
      end
    end;
  finally
    outlines.Free;
    lineparts.Free;
  end;
end;


function getProfilesDirListLin: TStringList;
var
  resultstring: string;
  cmd, report: string;
  {$IFDEF OPSISCRIPT}
  outlines: TXStringlist;
  {$ELSE OPSISCRIPT}
  outlines: TStringList;
  {$ENDIF OPSISCRIPT}
  lineparts: TStringList;
  ExitCode: longint;
  i: integer;
begin
  Result := TStringList.Create;
  {$IFDEF OPSISCRIPT}
  outlines := TXStringList.Create;
  {$ELSE OPSISCRIPT}
  outlines := TStringList.Create;
  {$ENDIF OPSISCRIPT}
  lineparts := TStringList.Create;
  // we use the home directories from the passwd entries
  // get passwd
  cmd := 'getent passwd';
  if not RunCommandAndCaptureOut(cmd, True, outlines, report, SW_HIDE, ExitCode) then
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
      { use only users with a pid >= 1000 }
      if StrToInt(lineparts.Strings[2]) >= 1000 then
      begin
        resultstring := lineparts.Strings[5];
        { no empty strings  }
        if resultstring <> '' then
          { no fs root  }
          if resultstring <> '/' then
            { no /home  }
            if resultstring <> '/home' then
              { use only existing direcories as profile }
              if DirectoryExists(ExpandFileName(resultstring)) then
                Result.Add(ExpandFileName(resultstring));
      end;
    end;
    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 6;
    LogDatei.log('', LLDebug);
  end;
  outlines.Free;
  lineparts.Free;
end;

function getLinuxDistroType: string;
var
  mycfg: TStringList;
  likestr: string;
begin
  Result := 'unknown_linux';
  if FileExists('/etc/debian_version') then
    Result := 'debian';
  if FileExists('/etc/redhat-release') then
    Result := 'redhat';
  if FileExists('/etc/SuSE-release') then
    Result := 'suse';
  // freedesktop org standard :
  if FileExists('/etc/os-release') then
  begin
    mycfg := TStringList.Create;
    mycfg.LoadFromFile('/etc/os-release');
    likestr := mycfg.Values['ID_LIKE'];
    mycfg.Free;
    if pos('suse', likestr) > 0 then
      Result := 'suse';
    if pos('debian', likestr) > 0 then
      Result := 'debian';
    if pos('rhel', likestr) > 0 then
      Result := 'redhat';
  end;
end;

function getLinuxVersionMap: TStringList;
var
  resultstring: string;
  cmd, report: string;
  {$IFDEF OPSISCRIPT}
  outlines: TXStringlist;
  {$ELSE OPSISCRIPT}
  outlines: TStringList;
  {$ENDIF OPSISCRIPT}
  lineparts: TStringList;
  ExitCode: longint;
  i: integer;
begin
  //todo : install lsb-release if not there
  Result := TStringList.Create;
  {$IFDEF OPSISCRIPT}
  outlines := TXStringList.Create;
  {$ELSE OPSISCRIPT}
  outlines := TStringList.Create;
  {$ENDIF OPSISCRIPT}
  lineparts := TStringList.Create;
  cmd := 'lsb_release --all';
  if not RunCommandAndCaptureOut(cmd, True, outlines, report, SW_HIDE, ExitCode) then
  begin
    LogDatei.log('Error: ' + Report + 'Exitcode: ' + IntToStr(ExitCode), LLError);
  end
  else
  begin
    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 6;
    LogDatei.log('', LLDebug2);
    LogDatei.log('output:', LLDebug2);
    LogDatei.log('--------------', LLDebug2);
    for i := 0 to outlines.Count - 1 do
    begin
      lineparts.Clear;
      LogDatei.log(outlines.strings[i], LLDebug2);
      stringsplit(outlines.strings[i], ':', lineparts);
      if lineparts.Count > 1 then
      begin
        resultstring := lineparts.Strings[0] + '=' + trim(lineparts.Strings[1]);
        Result.Add(resultstring);
      end;
    end;
  end;
  outlines.Clear;
  lineparts.Clear;
  // get SubRelease
  if FileExists('/etc/SuSE-release') then
  begin
    cmd := 'grep PATCHLEVEL /etc/SuSE-release';
    if not RunCommandAndCaptureOut(cmd, True, outlines, report,
      SW_HIDE, ExitCode) then
    begin
      LogDatei.log('Error: ' + Report + 'Exitcode: ' + IntToStr(ExitCode), LLError);
    end
    else
    begin
      LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 6;
      LogDatei.log('', LLDebug2);
      LogDatei.log('output:', LLDebug2);
      LogDatei.log('--------------', LLDebug2);
      for i := 0 to outlines.Count - 1 do
      begin
        lineparts.Clear;
        LogDatei.log(outlines.strings[i], LLDebug2);
        stringsplit(outlines.strings[i], '=', lineparts);
        if lineparts.Count > 1 then
        begin
          resultstring := 'SubRelease=' + trim(lineparts.Strings[1]);
          Result.Add(resultstring);
        end;
      end;
    end;
  end
  else
    Result.Add('SubRelease=');
  outlines.Free;
  lineparts.Free;

  Result.Add('kernel name' + '=' + trim(getCommandResult('uname -s')));
  Result.Add('node name' + '=' + trim(getCommandResult('uname -n')));
  Result.Add('kernel release' + '=' + trim(getCommandResult('uname -r')));
  Result.Add('kernel version' + '=' + trim(getCommandResult('uname -v')));
  Result.Add('machine' + '=' + trim(getCommandResult('uname -m')));
  Result.Add('processor' + '=' + trim(getCommandResult('uname -p')));
  Result.Add('hardware platform' + '=' + trim(getCommandResult('uname -i')));
  Result.Add('operating system' + '=' + trim(getCommandResult('uname -o')));
end;

function getLinuxDistroName: string;
var
  linuxinfo: TStringList;
begin
  try
    linuxinfo := getLinuxVersionMap;
    Result := linuxinfo.Values['Distributor ID'];
  finally
    linuxinfo.Free;
  end;
end;

function getLinuxDistroRelease: string;
var
  linuxinfo: TStringList;
begin
  try
    linuxinfo := getLinuxVersionMap;
    Result := linuxinfo.Values['Release'];
  finally
    linuxinfo.Free;
  end;
end;

function getLinuxDistroDescription: string;
var
  linuxinfo: TStringList;
begin
  try
    linuxinfo := getLinuxVersionMap;
    Result := linuxinfo.Values['Description'];
  finally
    linuxinfo.Free;
  end;
end;

function Is64BitSystemLin: boolean;
begin
  if trim(getCommandResult('uname -i')) = 'x86_64' then
    Result := True
  else if trim(getCommandResult('uname -m')) = 'x86_64' then
    Result := True
  else
    Result := False;
end;

function getHostnameLin: string;
var
  str: string = '';
  exitcode: longint;
begin
  //result := synsock.GetHostName;
  Result := '';
  try
    str := getCommandResult('/bin/bash -c "hostname -f || exit $?"', exitcode);
    if exitcode = 0 then
      Result := str;
  finally
  end;
end;

function getMyHostEnt: netdb.THostEntry;
begin
  try
    if not netdb.gethostbyname(synsock.GetHostName, Result) then
      Logdatei.log('gethostbyname error ' +
        IntToStr(wsagetlasterror), LLError);
  except
    Logdatei.log('gethostname error ' +
      IntToStr(wsagetlasterror), LLError);
  end;
end;



function GetIPFromHost(var HostName, IPaddr, WSAErr: string): boolean;
var
  HostEntry: THostEntry;
begin
  Result := False;
  try
    if isip(HostName) then
      IPAddr := Hostname
    else
    if netdb.gethostbyname(HostName, HostEntry) then
    begin
      Result := True;
      IPaddr := Format('%d.%d.%d.%d', [HostEntry.Addr.s_bytes[4],
        HostEntry.Addr.s_bytes[3], HostEntry.Addr.s_bytes[2],
        HostEntry.Addr.s_bytes[1]]);
    end
    else
      Logdatei.DependentAddError('gethostbyname error ' +
        IntToStr(wsagetlasterror), LLError);
  except
    WSAErr := 'Error resolving Host';
  end;
end;

function getDev2MacMap: TStringList;
var
  resultstring: string;
  cmd, report: string;
  {$IFDEF OPSISCRIPT}
  outlines: TXStringlist;
  {$ELSE OPSISCRIPT}
  outlines: TStringList;
  {$ENDIF OPSISCRIPT}
  lineparts: TStringList;
  ExitCode: longint;
  i: integer;
begin
  Result := TStringList.Create;
  {$IFDEF OPSISCRIPT}
  outlines := TXStringList.Create;
  {$ELSE OPSISCRIPT}
  outlines := TStringList.Create;
  {$ENDIF OPSISCRIPT}
  lineparts := TStringList.Create;
  cmd := 'ip -o link show | awk ''{for (i=1; i < NF; i++) if ($i == "link/ether") print $2, $(i+1)}'' '
  ;
  if not RunCommandAndCaptureOut(cmd, True, outlines, report, SW_HIDE, ExitCode) then
  begin
    LogDatei.log('Error: ' + Report + 'Exitcode: ' + IntToStr(ExitCode), LLError);
  end
  else
  begin
    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 6;
    LogDatei.log('', LLDebug2);
    LogDatei.log('output:', LLDebug2);
    LogDatei.log('--------------', LLDebug2);
    for i := 0 to outlines.Count - 1 do
    begin
      lineparts.Clear;
      LogDatei.log(outlines.strings[i], LLDebug2);
      stringsplitByWhiteSpace(outlines.strings[i], TStringList(lineparts));
      if lineparts.Count > 1 then
      begin
        resultstring := copy(lineparts.Strings[0], 1, length(lineparts.Strings[0]) - 2) +
          '=' + trim(lineparts.Strings[1]);
        Result.Add(resultstring);
      end;
    end;
  end;
  outlines.Free;
  lineparts.Free;
end;

function linIsUefi: boolean;
begin
  //not implemented yet
  Result := False;
end;

// moved to osprocesses
//function which(target: string; var pathToTarget: string): boolean;
//var
//  str: string;
//  exitcode: longint;
//  cmd: string;
//  path: string;
//begin
//  Result := False;
//  pathToTarget := '';
//  { Using FindDefaultExecutablePath from fileutil }
//  pathToTarget := FindDefaultExecutablePath(target);
//  if (pathToTarget <> '') and FileExistsUTF8(pathToTarget) then
//  begin
//    Result := true;
//    pathToTarget := Trim(pathToTarget);
//  end;
//  (*
//  cmd := '/bin/bash -c "';
//  cmd := cmd + 'set PATH=''/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin'' ;';
//  cmd := cmd + 'which '+target+' || exit $?"';
//  *)
//  (* my implementation
//  path := '/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin';
//  str := FileSearch(target, path);
//  if fileexists(trim(str)) then
//  begin
//    Result := True;
//    pathToTarget := trim(str);
//  end;    *)
//end;
//*)

function getMyIpByTarget(target: string): string;
var
  str: string;
  list: TStringList;
  i: integer;
begin
  Result := '';
  list := TStringList.Create;
  {$IFDEF UNIX}
  str := getCommandResult('/bin/bash -c "ip -o -4 route get ' + target + ' || exit $?"');
  {$ENDIF}
  {$IFDEF DARWIN}
  //str := getCommandResult('ip -o -4 route get '+target);
  // macos ip has no '-o'
  str := getCommandResult('/bin/bash -c "/usr/local/bin/ip -4 route get ' + target + ' || exit $?"');
  {$ENDIF DARWIN}
  LogDatei.log('ip out: ' + str , LLInfo);
  stringsplitByWhiteSpace(str, TStringList(list));
  i := list.IndexOf('src');
  if (i > -1) and (list.Count >= i) then
  begin
    Result := list[i + 1];
  end;
  list.Free;
end;

function getMyIpByDefaultRoute: string;
var
  str: string;
  cmd: string;
  list: TStringList;
  i: integer;
begin
  Result := '';
  list := TStringList.Create;
  //if not which('ip', cmd) then
  //  cmd := 'ip';
  {$IFDEF UNIX}
  str := getCommandResult('/bin/bash -c "ip -o -4 route get 255.255.255.255 || exit $?"');
  {$ENDIF}
  {$IFDEF DARWIN}
  //str := getCommandResult('ip -o -4 route get '+target);
  // macos ip has no '-o'
  str := getCommandResult('/bin/bash -c "/usr/local/bin/ip -4 route get 255.255.255.255 || exit $?"');
  {$ENDIF DARWIN}
  LogDatei.log('ip out: ' + str , LLInfo);
  stringsplitByWhiteSpace(str, list);
  LogDatei.log_list(list, LLDEBUG3);
  i := list.IndexOf('src');
  if (i > -1) and (list.Count >= i) then
  begin
    Result := list[i + 1];
  end;
  list.Free;
end;

function getMyIpDeciceByDefaultRoute: string;
var
  str: string;
  cmd: string;
  list: TStringList;
  i: integer;
begin
  Result := '';
  list := TStringList.Create;
  //if not which('ip', cmd) then
  //  cmd := 'ip';
  {$IFDEF UNIX}
  str := getCommandResult('/bin/bash -c "ip -o -4 route get 255.255.255.255 || exit $?"');
  {$ENDIF}
  {$IFDEF DARWIN}
  //str := getCommandResult('ip -o -4 route get '+target);
  // macos ip has no '-o'
  str := getCommandResult('/bin/bash -c "/usr/local/bin/ip -4 route get 255.255.255.255 || exit $?"');
  {$ENDIF DARWIN}
  LogDatei.log('ip out: ' + str , LLInfo);
  stringsplitByWhiteSpace(str, list);
  LogDatei.log_list(list, LLDEBUG3);
  i := list.IndexOf('dev');
  if (i > -1) and (list.Count >= i) then
  begin
    Result := list[i + 1];
  end;
  list.Free;
end;

function getPackageLock(timeoutsec: integer; kill: boolean): boolean;
var
  disttype: string;
  distname: string;
  lockfile, lockfile1: string;
  pid: string;
  pidnum: integer;
  pcmd: string;
  timeoutreached: boolean;
  timeoutstep, timeoutcounter: integer;
  {$IFDEF OPSISCRIPT}
  outlines: TXStringlist;
  {$ELSE OPSISCRIPT}
  outlines: TStringList;
  {$ENDIF OPSISCRIPT}
  lineparts: TStringList;

  function getPackageLockPid(lockfile: string): string;
  var
    mylockfile: Text;
    cmd, report: string;
  begin
    if (disttype = 'suse') or (disttype = 'redhat') then
    begin
      Result := '';
      if FileExists(lockfile) then
      begin
        try
          assignfile(mylockfile, lockfile);
          reset(mylockfile);
          readln(mylockfile, Result);
          Close(mylockfile);
        except
          LogDatei.log('Exception in getPackageLockPid opening existing lockfile: ' +
            lockfile, LLWarning);
        end;
      end
      else
        LogDatei.log('No package lock file found', LLDEBUG3);
    end
    else if (disttype = 'debian') then
    begin
      Result := '';
      if FileExists(lockfile) then
      begin
        try
          {$IFDEF OPSISCRIPT}
          outlines := TXStringList.Create;
          {$ELSE OPSISCRIPT}
          outlines := TStringList.Create;
          {$ENDIF OPSISCRIPT}
          lineparts := TStringList.Create;
          cmd := 'lsof -w ' + lockfile;
          if not RunCommandAndCaptureOut(cmd, True, outlines, report,
            SW_HIDE, ExitCode, False, 2) then
          begin
            LogDatei.log('Error: ' + Report + 'Exitcode: ' +
              IntToStr(ExitCode), LLError);
          end
          else
          begin
            LogDatei.log('', LLDebug3);
            LogDatei.log('output:', LLDebug3);
            LogDatei.log('--------------', LLDebug3);
            // process is in second line
            if outlines.Count > 1 then
            begin
              lineparts.Clear;
              LogDatei.log(outlines.strings[1], LLDebug2);
              stringsplitByWhiteSpace(outlines.strings[1], TStringList(lineparts));
              if lineparts.Count > 1 then
              begin
                Result := trim(lineparts.Strings[1]);
              end
              else
                LogDatei.log('Error in getPackageLockPid lsof on existing lockfile: ' +
                  lockfile,
                  LLWarning);
            end;
          end;
          outlines.Free;
          lineparts.Free;
        except
          LogDatei.log('Exception in getPackageLockPid lsof on existing lockfile: ' +
            lockfile, LLWarning);
        end;
      end
      else
        LogDatei.log('No package lock file found', LLDEBUG3);
    end;
  end;

  function getPackageLockbyFile(lockfile: string; timeoutsec: integer;
    kill: boolean): boolean;
  begin
    try
      timeoutreached := False;
      timeoutstep := 5;
      timeoutcounter := 0;
      pid := getPackageLockPid(lockfile);
      if not tryStrToInt(pid, pidnum) then
        pcmd := ''
      else
        pcmd := getProcessByPid(pidnum);
      while (pid <> '') and (not timeoutreached) and (pcmd <> '') do
      begin
        LogDatei.log('Waiting to get package lock from pid: ' + pid +
          ' : ' + pcmd, LLDEBUG);
        timeoutcounter := timeoutcounter + timeoutstep;
        if timeoutcounter >= timeoutsec then
          timeoutreached := True
        else
        begin
          sleep(timeoutstep * 1000);
          pid := getPackageLockPid(lockfile);
          if not tryStrToInt(pid, pidnum) then
            pcmd := ''
          else
            pcmd := getProcessByPid(pidnum);
        end;
      end;
      if timeoutreached then
      begin
        Result := False;
        LogDatei.log('Timeout waiting to get package lock from pid: ' +
          pid + ' : ' + pcmd,
          LLNotice);
        if kill then
        begin
          LogDatei.log('Killing to get Package lock from pid: ' +
            pid + ' : ' + pcmd, LLInfo);
          killProcessByPid(pidnum);
          pid := getPackageLockPid(lockfile);
          if not tryStrToInt(pid, pidnum) then
            pcmd := ''
          else
            pcmd := getProcessByPid(pidnum);
          if (pid <> '') and (pcmd <> '') then
            Result := False
          else
            Result := True;
        end;
      end
      else
        Result := True;
    except
      on ex: Exception do
      begin
        LogDatei.log('Exception in osfunclin at getPackageLockbyFile: Error: ' +
          ex.message, LLError);
      end;
    end;
  end;

begin
  try
    Result := False;
    lockfile1 := '';
    lockfile := '';
    disttype := getLinuxDistroType;
    // This is a memory leak:
    //distname := getLinuxVersionMap.Values['Distributor ID'];
    distname := getLinuxDistroName;
    if disttype = 'suse' then
      lockfile := '/run/zypp.pid'
    else if disttype = 'redhat' then
      lockfile := '/var/run/yum.pid'
    else if disttype = 'debian' then
    begin
      lockfile := '/var/lib/dpkg/lock';
      if distname = 'Univention' then
        lockfile1 := '/var/lib/apt/lists/lock';
    end
    else
    begin
      // unsupported distrotype
      LogDatei.log('unsupported distrotype in getPackageLock', LLERROR);
      Result := False;
    end;
    if lockfile1 <> '' then
      Result := getPackageLockbyFile(lockfile1, timeoutsec, kill);
    if lockfile <> '' then
      Result := getPackageLockbyFile(lockfile1, timeoutsec, kill);
  except
    on ex: Exception do
    begin
      LogDatei.log('Exception in osfunclin at getPackageLock: Error: ' +
        ex.message, LLError);
    end;
  end;
end;

function os_shutdown(): boolean;
var
  exitcode: integer;
  exitcmd: string;
begin

  if LogDatei <> nil then
  begin
    LogDatei.LogSIndentLevel := 0;
    LogDatei.DependentAdd('============   ' + ExtractFileNameOnly(ParamStr(0)) +
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



end.
