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
  lazfileutils,
  oslog;

function ProcessIsRunning(searchproc: string): boolean;
function numberOfProcessInstances(searchproc: string): integer;
function which(target: string; var pathToTarget: string): boolean;

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
      FProcessEntry32.th32ProcessID) + ';';
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
{$IFDEF LINUX}
function getLinProcessList: TStringList;
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
      Result := TStringList.Create;
      {$IFDEF OPSISCRIPT}
      outlines := TXStringList.Create;
      {$ELSE OPSISCRIPT}
      outlines := TStringList.Create;
      {$ENDIF OPSISCRIPT}
      lineparts := TStringList.Create;
      pscmd := 'ps -eo pid,user,comm:30,cmd:110';
      if not RunCommandAndCaptureOut(pscmd, True, outlines, report,
        SW_HIDE, ExitCode) then
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
        LogDatei.log('', LLDebug3);
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

{$ENDIF LINUX}



function getProcessList: TStringList;
begin
  {$IFDEF WINDOWS}
  {$IFDEF WIN32}
  Result := getWinProcessList;
  {$ENDIF WIN32}
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
  Result := getLinProcessList;
  {$ENDIF LINUX}
  {$IFDEF DARWIN}
  Result := getMacOSProcessList;
  {$ENDIF DARWIN}
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
          ') is wider then 14 chars. Searching for: (' + searchstr + '). The result may not be exact',
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
      logdatei.log('Error: Exception in processIsRunning:  ' + searchproc+' / '+searchstr, LLError);
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


end.
