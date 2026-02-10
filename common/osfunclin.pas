unit osfunclin;

// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: Rupert Roeder, detlef oertel


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
  {$IFDEF GUI}
  Graphics,
  LResources,
  // dialogs e.g. for ShowMessage
  Dialogs,
  {$ENDIF GUI}
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
{$IFDEF OPSISCRIPT}
uses
  {$IFDEF GUI}
  osbatchgui,
  osinteractivegui,
  osshowsysinfo,
{$ENDIF GUI}
  osparser;

{$ENDIF OPSISCRIPT}

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
    //
    //LogDatei.log('', LLDebug);
    //LogDatei.log('output:', LLDebug);
    //LogDatei.log('--------------', LLDebug);
    for i := 0 to outlines.Count - 1 do
    begin
      LogDatei.log(outlines.strings[i], LLDebug);
      Inc(found);
    end;
    //
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
    LogDatei.log('', LLDebug);
    LogDatei.log('output:', LLDebug);
    LogDatei.log('--------------', LLDebug);
    for i := 0 to outlines.Count - 1 do
    begin
      LogDatei.log(outlines.strings[i], LLDebug);
    end;
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
  uid: integer;
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
    LogDatei.log('', LLDebug);
    LogDatei.log('output:', LLDebug);
    LogDatei.log('--------------', LLDebug);
    for i := 0 to outlines.Count - 1 do
    begin
      lineparts.Clear;
      LogDatei.log(outlines.strings[i], LLDebug);
      stringsplit(outlines.strings[i], ':', lineparts);
      //LogDatei.log(lineparts.Strings[2], LLDebug);
      // the line may something like: S daemon,,,:/var/run/avahi-daemon:/usr/sbin/nologin
      // so we have to test if we really have a uid:
      if tryStrtoint(lineparts.Strings[2], uid) then
      begin
        { use only users with a uid >= 1000 }
        if uid >= 1000 then
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
    end;
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

function getLinuxReleaseInfoFromLSBRelease(var ReleaseInfo: TStringList): boolean;
var
  ResultString: string;
  Output: string;
  OutLines: TStringList;
  LineParts: TStringList;
  i: integer;
begin
  OutLines := TStringList.Create;
  LineParts := TStringList.Create;
  Result := False;
  try
    if RunCommand('lsb_release', ['--all'], Output,
      [poWaitOnExit, poUsePipes, poStderrToOutPut], swoShow) then
    begin
      Result := True;
      LogDatei.log('', LLDebug2);
      LogDatei.log('output:', LLDebug2);
      LogDatei.log('--------------', LLDebug2);
      StringSplit(Output, #10, OutLines);
      for i := 0 to OutLines.Count - 1 do
      begin
        LogDatei.log(OutLines.Strings[i], LLDebug2);
        (* We are only interested in the key/value pairs (separated by ':').
           If LSB module is not available on Debian "lsb_release --all", the command still works but gives as first line "No LSB modules are available.".
           The last line of the RunCommand Output is usually empty. *)
        if (pos(':', OutLines.Strings[i]) > 0) then
        begin
          LineParts.Clear;
          StringSplit(OutLines.Strings[i], ':', LineParts);
          if LineParts.Count > 1 then
          begin
            ResultString := LineParts.Strings[0] + '=' + trim(LineParts.Strings[1]);
            ReleaseInfo.Add(ResultString);
          end;
        end;
      end;
    end
    else
    begin
      if Output = '' then Output := 'Probably lsb_release does not exist on the system.';
      LogDatei.log('Command "lsb_release" does not work: ' + Output, LLInfo);
    end;
  finally
    FreeAndNil(LineParts);
    FreeAndNil(OutLines);
  end;
end;


function getLinuxReleaseInfoFromFile(const FilePath:string; const Mapping: TStringList; var ReleaseInfo: TStringList):boolean;
var
  ReleaseInfoFromFile: TStringList;
  i: integer;
begin
  Result := False;
  if FileExists(FilePath) then
  begin
    LogDatei.log('Reading release info from "' + FilePath + '"', LLInfo);
    ReleaseInfoFromFile := TStringList.Create;
    try
      ReleaseInfoFromFile.LoadFromFile(FilePath);
      for i := 0 to Mapping.Count - 1 do
        ReleaseInfo.Add(Mapping.Names[i] + '=' + AnsiDequotedStr(ReleaseInfoFromFile.Values[Mapping.ValueFromIndex[i]],'"'));
      LogDatei.log('', LLDebug2);
      LogDatei.log('output:', LLDebug2);
      LogDatei.log('--------------', LLDebug2);
      LogDatei.log_list(ReleaseInfo, LLDebug2);
      Result := True;
    finally
      FreeAndNil(ReleaseInfoFromFile);
    end;
  end
  else
  begin
    LogDatei.log('"' + FilePath + '" does not exists', LLInfo);
  end;
end;

function getLinuxReleaseInfo:TStringList;
var
  Mapping: TStringList;
  Release: string;
begin
  //todo : install lsb-release if not there
  Result := TStringList.Create;
  Mapping := TStringList.Create;
  if not getLinuxReleaseInfoFromLSBRelease(Result) then
  begin
    Mapping.Add('Distributor ID=ID');
    Mapping.Add('Description=PRETTY_NAME');
    Mapping.Add('Release=VERSION_ID');
    Mapping.Add('Codename=VERSION_CODENAME');
    if not getLinuxReleaseInfoFromFile('/etc/os-release', Mapping, Result) then
    begin
      LogDatei.log('Error (getLinuxReleaseInfo): Could not get release info.', LLError);
    end;
  end;
  (*
  For SUSE distributions the subrelease (=patchlevel) is relevant because packages
  might be only compatible with a specific patchlevel.
  The subrelease is not relevant for other Linux distributions like Debian/Ubuntu or RedHat.
  *)
  if (pos('suse', lowercase(Result.Values['ID'])) > 0) then
  begin
    Release := Result.Values['Release'];
    Result.Add('SubRelease=' + Copy(Release, Pos('.', Release) + 1));
  end
  else
  begin
    Result.Add('SubRelease=');
  end;
  FreeAndNil(Mapping);
end;


function getLinuxVersionMap: TStringList;
begin
  Result := getLinuxReleaseInfo;
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
      Logdatei.log('gethostbyname error ' + IntToStr(wsagetlasterror), LLError);
  except
    Logdatei.log('gethostname error ' + IntToStr(wsagetlasterror), LLError);
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
      Logdatei.Log('gethostbyname error ' +
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
  Result := False;
  try
   if DirectoryExists('/sys/firmware/efi') then  Result := True;
  except
    LogDatei.log('Exception in linIsUefi while checking for /sys/firmware/efi', LLWarning);
  end;
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
  {$IFDEF LINUX}
  str := getCommandResult('/bin/bash -c "ip -o -4 route get ' + target + ' || exit $?"');
  {$ENDIF}
  {$IFDEF DARWIN}
  //str := getCommandResult('ip -o -4 route get '+target);
  // macos ip has no '-o'
  str := getCommandResult('/bin/bash -c "/usr/local/bin/ip -4 route get ' +
    target + ' || exit $?"');
  {$ENDIF DARWIN}
  LogDatei.log('ip out: ' + str, LLInfo);
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
  str := getCommandResult(
    '/bin/bash -c "/usr/local/bin/ip -4 route get 255.255.255.255 || exit $?"');
  {$ENDIF DARWIN}
  LogDatei.log('ip out: ' + str, LLInfo);
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
  str := getCommandResult(
    '/bin/bash -c "/usr/local/bin/ip -4 route get 255.255.255.255 || exit $?"');
  {$ENDIF DARWIN}
  LogDatei.log('ip out: ' + str, LLInfo);
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
  outstring: string;
  exitcode: longint;

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
  LogDatei.log_prog('getPackageLock called with: ' + IntToStr(timeoutsec) +
    ' - ' + booltostr(kill, True), LLinfo);
  try
    Result := False;
    lockfile1 := '';
    lockfile := '';
    disttype := getLinuxDistroType;
    // This is a memory leak:
    //distname := getLinuxVersionMap.Values['Distributor ID'];
    distname := getLinuxDistroName;
    if disttype = 'suse' then
    begin
      // we need to call zypper to fill zypper.pid
      // exitcode: 7 - ZYPPER_EXIT_ZYPP_LOCKED
      outstring := getCommandResult('zypper refresh', exitcode);
      if exitcode = 7 then LogDatei.log('zypper is locked', LLinfo);
      lockfile := '/run/zypp.pid';
    end
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
      Result := getPackageLockbyFile(lockfile, timeoutsec, kill);
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

end.
