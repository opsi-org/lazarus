unit osprocesses;

{$mode delphi}

interface

uses
  Classes, SysUtils,
  osparserhelper,
  {$IFDEF OPSISCRIPT}
  ostxstringlist,
  {$ENDIF OPSISCRIPT}
  {$IFDEF WINDOWS}
  Windows,
  JwaWindows,
  lazutf8,
  {$ENDIF WINDOWS}
  {$IFDEF WIN32}
  DSiWin32,
  {$ENDIF WIN32}
  {$IFDEF UNIX}
  OSProcessux,

  {$ENDIF UNIX}
  {$IFDEF DARWIN}
  osfuncmac,
  {$ENDIF DARWIN}
  fileutil,
  strutils,
  lazfileutils,
  oslog;

function ProcessIsRunning(searchproc: string): boolean;
function numberOfProcessInstances(searchproc: string): integer;
function which(target: string; var pathToTarget: string): boolean;
function isProcessChildOf(searchproc, parentproc: string): boolean;

implementation


// Process list

{$IFDEF WINDOWS}
//http://www.lazarus.freepascal.org/index.php?topic=3543.0

//http://www.delphigeist.com/2010/03/process-list.html

//http://www.delphi-library.de/topic_Prozessliste+erstellen_41114,0.html
//http://www.delphi-forum.de/viewtopic.php?p=563144&sid=db0be57e7b505dc7e7d0f4700653674f
//http://support.microsoft.com/kb/175030/de
function getWinProcessList: TStringList;
var
  ContinueLoop: boolean;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
  resultstring: string;
  UserName, Domain: ansistring;
begin
  Result := TStringList.Create;
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := Sizeof(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
  while ContinueLoop do
  begin
    resultstring := '';
    UserName := '';
    domain := '';

    resultstring := FProcessEntry32.szExeFile + ';' + IntToStr(
      FProcessEntry32.th32ProcessID) + ';' + IntToStr(
      FProcessEntry32.th32ParentProcessID) + ';';
    //  if GetProcessUserBypid(FProcessEntry32.th32ProcessID, UserName, Domain) then
    //    resultstring := resultstring + Domain + '\' + UserName;
    //if FProcessEntry32.th32ProcessID > 0 then
    //  resultstring := resultstring + ';' + FormatFloat('##0.00',GetProcessActivityByPid(FProcessEntry32.th32ProcessID))
    //else resultstring := resultstring + ';';
    resultstring := WinCPToUTF8(resultstring);
    Result.add(resultstring);
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;

{$ENDIF WINDOWS}

{$IFDEF UNIX}
function getUnixProcessList: TStringList;
var
  resultstring, pidstr, ppidstr, userstr, cmdstr, fullcmdstr: string;
  pscmd, report: string;
  outlines: TStringList;
  lineparts: TStringList;
  ExitCode: longint;
  i, k: integer;
  defunct: boolean;
  getProcessListSuccess: boolean;
  retryCounter: integer;
begin
  try
    try
      Result := TStringList.Create;
      outlines := TStringList.Create;
      lineparts := TStringList.Create;
      retryCounter := 0;
      {$IFDEF LINUX}
      getProcessListSuccess := True; // init for Linux
      pscmd := 'ps -eo pid,ppid,user,comm:40,cmd:110';
      {$ENDIF LINUX}
      {$IFDEF DARWIN}
      getProcessListSuccess := False; // init for darwin
      pscmd := 'ps -eco pid,ppid,user,comm';
      {$ENDIF DARWIN}
      repeat
        Inc(retryCounter);
      {$IFDEF OPSISCRIPT}
        if not RunCommandAndCaptureOut(pscmd, True, TXStringlist(outlines),
          report, SW_HIDE, ExitCode) then
      {$ELSE OPSISCRIPT}
          if not RunCommandAndCaptureOut(pscmd, True, outlines, report,
            SW_HIDE, ExitCode) then
      {$ENDIF OPSISCRIPT}
          begin
            LogDatei.log('Error: ' + Report + 'Exitcode: ' +
              IntToStr(ExitCode), LLError);
          end
          else
          begin
            
            LogDatei.log('', LLDebug3);
            LogDatei.log('output:', LLDebug3);
            LogDatei.log('--------------', LLDebug3);


            if outlines.Count > 0 then
            begin
            {$IFDEF DARWIN}
              getProcessListSuccess := False; // init for darwin
              LogDatei.log('first line is: '+outlines.strings[1], LLDebug);
              if ContainsText(outlines.strings[1], 'launchd') then
              begin
                getProcessListSuccess := True; // success for darwin
                LogDatei.log('get processlist ok', LLDebug);
              end
              else
              begin
                LogDatei.log('get processlist failed - retry count: '+inttostr(retryCounter-1), LLDebug);
                sleep(1000);
              end;
             {$ENDIF DARWIN}
              if getProcessListSuccess then
                for i := 0 to outlines.Count - 1 do
                begin
                  LogDatei.log(outlines.strings[i], LLDebug2);
                  lineparts.Clear;
                  resultstring := '';
                  userstr := '';
                  pidstr := '';
                  ppidstr := '';
                  cmdstr := '';
                  fullcmdstr := '';
                  defunct := False;
                  if pos('<defunct>', outlines.strings[i]) > 0 then
                    defunct := True;
                  stringsplitByWhiteSpace(trim(outlines.strings[i]), lineparts);
            {$IFDEF LINUX}
                  for k := 0 to lineparts.Count - 1 do
                  begin
                    if k = 0 then
                      pidstr := lineparts.Strings[k]
                    else if k = 1 then
                      ppidstr := lineparts.Strings[k]
                    else if k = 2 then
                      userstr := lineparts.Strings[k]
                    else if k = 3 then
                      cmdstr := lineparts.Strings[k]
                    else
                      fullcmdstr := fullcmdstr + lineparts.Strings[k] + ' ';
                    if defunct then
                    begin
                      // mark with brackets as defunct
                      cmdstr := '[' + cmdstr + ']';
                    end;
                  end;
            {$ENDIF LINUX}
            {$IFDEF DARWIN}
                  for k := 0 to lineparts.Count - 1 do
                  begin
                    if k = 0 then
                      pidstr := lineparts.Strings[k]
                    else if k = 1 then
                      ppidstr := lineparts.Strings[k]
                    else if k = 2 then
                      userstr := lineparts.Strings[k]
                    else
                      cmdstr := cmdstr + lineparts.Strings[k] + ' ';
                  end;
                  cmdstr := trim(cmdstr);
                  fullcmdstr := cmdstr;
            {$ENDIF DARWIN}
                  resultstring :=
                    cmdstr + ';' + trim(pidstr) + ';' + trim(ppidstr) +
                    ';' + userstr + ';' + fullcmdstr;
                  LogDatei.log(resultstring, LLDebug3);
                  Result.Add(resultstring);
                end;
            end;
            
            LogDatei.log('', LLDebug3);
          end;
      until getProcessListSuccess or (retryCounter > 5);
    except
      on E: Exception do
      begin
        LogDatei.log('Exception in getUnixProcessList, system message: "' +
          E.Message + '"',
          LLError);
      end
    end;
  finally
    outlines.Free;
    lineparts.Free;
  end;
end;

{$ENDIF UNIX}




function getProcessList: TStringList;
begin
  {$IFDEF WINDOWS}
  Result := getWinProcessList;
  {$ENDIF WINDOWS}
  {$IFDEF UNIX}
  Result := getUnixProcessList;
  {$ENDIF UNIX}
end;

function ProcessIsRunning(searchproc: string): boolean;
var
  list1: TStringList;
  i: integer;
  searchstr, shortcmd: string;
begin
  Result := False;
  searchproc := trim(searchproc);
  try
    list1 := TStringList.Create;
    try
      searchstr := searchproc;
      {$IFDEF LINUX}
      {in processlist we get " shortcmd ; ....." }
      {shortcmd has max length 15 and the rest does not help really }
      {so we try find an exact match in shortcmd }
      if length(searchproc) > 14 then
      begin
        searchstr := trim(copy(searchproc, 1, 15));
        logdatei.log(
          'Process name to find (' + searchproc +
          ') is wider then 14 chars. Searching for: (' + searchstr +
          '). The result may not be exact',
          LLwarning);
      end;
      {$ENDIF LINUX}
      {$IFDEF UNIX}
      list1.Text := getProcesslist.Text;
      //If Assigned(LogDatei) then LogDatei.log_list(list1,LLDebug2);
      for i := 0 to list1.Count - 1 do
      begin
        shortcmd := trim(copy(list1.Strings[i], 1, pos(';', list1.Strings[i]) - 1));
        if Assigned(LogDatei) then
          LogDatei.log_prog('found process shortcmd: ' + shortcmd, LLDebug2);
        //if pos(searchproc, list1.Strings[i]) > 0 then
        if LowerCase(searchstr) = LowerCase(shortcmd) then
          Result := True;
      end;
    {$ENDIF UNIX}
    {$IFDEF WINDOWS}
      searchstr := searchproc;
      list1.Text := getProcesslist.Text;
      for i := 0 to list1.Count - 1 do
      begin
        if pos(searchstr, list1.Strings[i]) > 0 then
          Result := True;
      end;
    {$ENDIF WINDOWS}
    except
      logdatei.log('Error: Exception in processIsRunning:  ' +
        searchproc + ' / ' + searchstr, LLError);
      Result := False;
    end;
  finally
    FreeAndNil(list1);
  end;
end;

function numberOfProcessInstances(searchproc: string): integer;
var
  list1: TStringList;
  i: integer;
begin
  Result := 0;
  list1 := TStringList.Create;
  try
    list1.Text := getProcesslist.Text;
    for i := 0 to list1.Count - 1 do
    begin
      if pos(searchproc, list1.Strings[i]) > 0 then
        Inc(Result);
    end;
  except
    logdatei.log('Error: Exception in processIsRunning:  ' + searchproc, LLError);
    Result := 0;
  end;
end;

function which(target: string; var pathToTarget: string): boolean;
var
  str: string;
  exitcode: longint;
  cmd: string;
  path: string;
begin
  Result := False;
  pathToTarget := '';
  { Using FindDefaultExecutablePath from fileutil }
  pathToTarget := FindDefaultExecutablePath(target);
  if (pathToTarget <> '') and FileExistsUTF8(pathToTarget) then
  begin
    Result := True;
    pathToTarget := Trim(pathToTarget);
  end;
end;


function isProcessChildOf(searchproc, parentproc: string): boolean;
var
  proclist: TStringList;
  pid2parent: TStringList;
  proc2pid: TStringList;
  i: integer;
  searchpid, parentpid: dword;
  tmpstr: string;
  basestr, searchstr: string;
  copystart1, copylength1, copystart2, copylength2: integer;

  function getPidOfProc(searchproc: string): dword;
  var
    mypidstr: string;
  begin
  (*
  result := 0;
  for i := 0 to proclist.Count - 1 do
    begin
      if pos(searchproc, proclist.Strings[i]) > 0 then
      begin
        mypidstr := copy(proclist.Strings[i], pos(';',proclist.Strings[i]),rpos(';',proclist.Strings[i]))
        if TryStrToDWord(result,mypidstr) then
        begin
          logdatei.log('getPidOfProc: valid pid for: ' + searchproc+ ' is: '+mypidstr, LLinfo);
        end
        else
        begin
        result := 0;
        logdatei.log('Error: getPidOfProc: found pid not valid: ' + mypidstr, LLerror);
        end;
      end;
    end;
    *)
    {$IFDEF LINUX}
    {in processlist we get " shortcmd ; ....." }
    {shortcmd has max length 15 and the rest does not help really }
    {so we try find an exact match in shortcmd }
    if length(searchproc) > 15 then
    begin
      searchstr := searchproc;
      searchproc := trim(copy(searchstr, 1, 15));
      logdatei.log(
        'Process name to find (' + searchstr +
        ') is wider then 15 chars. Searching for: (' + searchproc +
        '). The result may not be exact',
        LLInfo);
    end;
    {$ENDIF LINUX}
    Result := 0;
    mypidstr := proc2pid.Values[searchproc];
    if mypidstr <> '' then
      if TryStrToDWord(mypidstr, Result) then
      begin
        logdatei.log('getPidOfProc: valid pid for: ' + searchproc +
          ' is: ' + mypidstr, LLDebug);
      end
      else
      begin
        Result := 0;
        logdatei.log('Error: getPidOfProc: found pid not valid: ' + mypidstr, LLerror);
      end;
    if Result = 0 then
      logdatei.log('getPidOfProc: No pid found for: ' + searchproc, LLDebug);
  end;

  function getParentPid(basepid: dword): dword;
  begin
    if not TryStrToDWord(pid2parent.Values[IntToStr(basepid)], Result) then
      Result := 0;
  end;

  function isParentByPid(basepid, parentpid: dword): boolean;
  var
    aktpid: dword;
    i: integer;
  begin
    Result := False;
    aktpid := basepid;
    i := 0;
    if aktpid <> parentpid then
    begin
      repeat
        aktpid := getParentPid(aktpid);
        Inc(i);
      until (aktpid <= 4) or (aktpid = parentpid) or (i > 100);
      if (i > 100) then
        LogDatei.Log('Error while searchin for parent PID (' +
          IntToStr(parentPid) + '). Start (base) PID was ' + IntToStr(basePID) +
          '. Last (aktPID) PID: ' + IntToStr(aktPID), LLError);
    end;
    if aktpid = parentpid then Result := True;
  end;

begin
  try
    Result := False;
    proclist := TStringList.Create;
    pid2parent := TStringList.Create;
    proc2pid := TStringList.Create;
    try
      proclist.Text := getProcesslist.Text;
      for i := 0 to proclist.Count - 1 do
      begin
        basestr := proclist.Strings[i];
        logdatei.log('proclist:  ' + basestr, LLdebug2);
        // create  pid2parent entry
        // get the pid
        copystart1 := pos(';', basestr) + 1;
        copylength1 := npos(';', basestr, 2) - copystart1;
        // get the parent pid
        copystart2 := npos(';', basestr, 2) + 1;
        copylength2 := npos(';', basestr, 3) - copystart2;
        tmpstr := copy(basestr, copystart1, copylength1) + '=' +
          copy(basestr, copystart2, copylength2);
        pid2parent.Add(tmpstr);
        logdatei.log('pid2parent:  ' + tmpstr, LLdebug2);
        //tmpstr := copy(proclist.Strings[i], 0,pos(';',proclist.Strings[i])-1)
        //               + '=' + copy(proclist.Strings[i], pos(';',proclist.Strings[i])+1,length(proclist.Strings[i])-(pos(';',proclist.Strings[i])+1);
        // create  proc2pid entry
        // get the proc name
        copystart1 := 0;
        copylength1 := npos(';', basestr, 1) - 1 - copystart1;
        // get the pid
        copystart2 := npos(';', basestr, 1) + 1;
        copylength2 := npos(';', basestr, 2) - copystart2;
        tmpstr := copy(basestr, copystart1, copylength1) + '=' +
          copy(basestr, copystart2, copylength2);
        proc2pid.Add(tmpstr);
        logdatei.log('proc2pid:  ' + tmpstr, LLdebug2);
      end;
      searchpid := getPidOfProc(searchproc);
      parentpid := getPidOfProc(parentproc);
      if (searchpid > 0) and (parentpid > 0) then
        Result := isParentByPid(searchpid, parentpid);
    except
      logdatei.log('Error: Exception in isProcessChildOf:  ' + searchproc, LLError);
      Result := False;
    end;
  finally
    FreeAndNil(proclist);
    FreeAndNil(pid2parent);
    FreeAndNil(proc2pid);
  end;
end;

end.
