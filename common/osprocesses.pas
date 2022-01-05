unit osprocesses;

{$mode delphi}

interface

uses
  Classes, SysUtils,
  osparserhelper,
  {$IFDEF OPSISCRIPT}
  ostxstringlist,
  {$ENDIF OPSISCRIPT}
  {$IFDEF WIN32}
  Windows,
  DSiWin32,
  //  JwaWinnt,
  //  jwawinbase,
  JwaWindows,
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

(*
{$IFDEF OPSISCRIPT}
uses
  osfunc;
  {$ENDIF OPSISCRIPT}
*)

// Process list

{$IFDEF WIN32}
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
    Result.add(resultstring);
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;

{$ENDIF WIN32}
{$IFDEF UNIX}
function getUnixProcessList: TStringList;
var
  resultstring, pidstr, ppidstr, userstr, cmdstr, fullcmdstr: string;
  pscmd, report: string;
  (*
  {$IFDEF OPSISCRIPT}
  outlines: TXStringlist;
  {$ELSE OPSISCRIPT}
  outlines: TStringList;
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
      {$ELSE OPSISCRIPT}
      outlines := TStringList.Create;
      {$ENDIF OPSISCRIPT}
      *)
      outlines := TStringList.Create;
      lineparts := TStringList.Create;
      pscmd := 'ps -eo pid,ppid,user,comm:40,cmd:110';
      {$IFDEF DARWIN}
      pscmd := 'ps -eco pid,pppid,user,comm';
      {$ENDIF DARWIN}
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
          LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 6;
          LogDatei.log('', LLDebug3);
          LogDatei.log('output:', LLDebug3);
          LogDatei.log('--------------', LLDebug3);
          if outlines.Count > 0 then
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
              fullcmdstr := cmdstr;
            {$ENDIF DARWIN}
              resultstring := cmdstr + ';' + pidstr + ';' + ppidstr +
                ';' + userstr + ';' + fullcmdstr;
              LogDatei.log(resultstring, LLDebug3);
              //resultstring := lineparts.Strings[0] + ';';
              //resultstring := resultstring + lineparts.Strings[1] + ';';
              //resultstring := resultstring + lineparts.Strings[2] + ';';
              Result.Add(resultstring);
            end;
          LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 6;
          LogDatei.log('', LLDebug3);
        end;
    except
      on E: Exception do
      begin
        LogDatei.DependentAdd('Exception in getUnixProcessList, system message: "' +
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
  {$IFDEF WIN32}
  Result := getWinProcessList;
  {$ENDIF WIN32}
  {$ENDIF WINDOWS}
  {$IFDEF UNIX}
  Result := getUnixProcessList;
  {$ENDIF UNIX}
  (*
  {$IFDEF DARWIN}
  Result := getMacOSProcessList;
  {$ENDIF DARWIN}
  *)
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
  basestr: string;
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
    Result := 0;
    mypidstr := proc2pid.Values[searchproc];
    if TryStrToDWord(mypidstr, Result) then
    begin
      logdatei.log('getPidOfProc: valid pid for: ' + searchproc +
        ' is: ' + mypidstr, LLinfo);
    end
    else
    begin
      Result := 0;
      logdatei.log('Error: getPidOfProc: found pid not valid: ' + mypidstr, LLerror);
    end;
    if Result = 0 then
      logdatei.log('getPidOfProc: No pid found for: ' + searchproc, LLinfo);
  end;

  function getParentPid(basepid: dword): dword;
  begin
    if not TryStrToDWord(pid2parent.Values[IntToStr(basepid)], Result) then
      Result := 0;
  end;

  function isParentByPid(basepid, parentpid: dword): boolean;
  var
    aktpid: dword;
  begin
    Result := False;
    aktpid := basepid;
    repeat
      aktpid := getParentPid(aktpid);
    until (aktpid <= 4) or (aktpid = parentpid);
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
