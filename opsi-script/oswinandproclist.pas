unit oswinandproclist;

(******************************************************************
This unit is part of opsi-script background-install extension
Winows only

This unit handles to make a list of all windows over all desktops
and put this together with the processlist
In the case that a process has multiple windows,
we will get the info of the last visible window.
If no window is visible,
we will get the info of the last unvisible window.

In order to get the list of open windows from diffrent desktops
we need for every desktop a own thread

******************************************************************)

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
//  Windows,
//  JwaWinnt,
//  jwawinbase,
  JwaWindows,
  oslog,
  osfuncwin2,
  LazUTF8;

type

  Tmythread = class(TThread)
  private
  public
    procedure Execute; override;
    constructor Create(CreateSuspended: boolean);
  end;

{$IFDEF WINDOWS}
function getWinListFromDesktop(searchDesktop: string): TStringList;
function getWinProcessListWithPathAndWindowInfo: TStringList;
(*
make a list of all windows over all desktops
and put this together with the processlist
In the case that a process has multiple windows,
we will get the info of the last visible window.
If no window is visible,
we will get the info of the last unvisible window.
*)
function getRunningForegroundProcesses: TStringList;
// hereby means 'foreground' that a process has a visible window.
{$ENDIF WINDOWS}

implementation

{$IFDEF WINDOWS}

var
  stopped: boolean;
  mythread: Tmythread;
  winlist: TStringList;
  mydesktop: string = '';
  //windowList: TStringList;

constructor TMyThread.Create(CreateSuspended: boolean);
begin
  FreeOnTerminate := True;
  inherited Create(CreateSuspended);
end;


procedure Tmythread.Execute;
var
  Current, parent: hwnd;
  i: integer;
  resultstring: string;
  Visible, iconic: boolean;
  myhandle: hwnd;
  ProcID: DWord;
  winVisible, winIsIconic: string;
  Titel: string = '';
  myClassName: string = '';
  wcharArray: array[0..260] of widechar;
  hdesktop: HDESK;
  hwinstaUser: THandle;
  mywinstation: string;
begin
  mywinstation := 'winsta0';
  logdatei.log('Starting winlist-Thread on desktop: ' + mydesktop, LLnotice);
  try
    // get first window
    i := 0;
    hwinstaUser := OpenWindowStation(PChar(mywinstation), False, MAXIMUM_ALLOWED);
    if hwinstaUser = 0 then
      logdatei.log('Failed to OpenWindowStation: ' + mywinstation +
        ' : ' + SysErrorMessage(GetLastError), LLerror)
    else
    begin
      if not SetProcessWindowStation(hwinstaUser) then
        logdatei.log('Failed to SetProcessWindowStation: ' + mydesktop +
          ' : ' + SysErrorMessage(GetLastError), LLerror);
      // else: try anyway
      hdesktop := OpenDesktop(PChar(mydesktop), 0, False, MAXIMUM_ALLOWED);
      if hdesktop = 0 then
        logdatei.log('Failed to OpenDesktop: ' + mydesktop + ' : ' +
          SysErrorMessage(GetLastError), LLerror)
      else
      begin
        if not SetThreadDesktop(hdesktop) then
          logdatei.log('Failed to switch to desktop: ' + mydesktop +
            ' : ' + SysErrorMessage(GetLastError), LLerror)
        else
        begin
          Current := GetTopWindow(GetDesktopWindow);

          while Current <> 0 do
          begin

            // get window data
            Visible := IsWindowVisible(Current);
            iconic := IsIconic(Current);
            myhandle := Current;
            parent := GetParent(Current);
            GetWindowThreadProcessID(Current, @ProcID);
            if GetWindowTextW(Current, wcharArray, 255) > 0 then
              Titel := SystoUTF8(strpas(wcharArray))
            else
              Titel := '';
            if GetClassNameW(Current, wcharArray, 256) > 0 then
              myClassName := SystoUTF8(strpas(wcharArray))
            else
              myClassName := '';

            if Visible then winVisible := 'winVisible'
            else
              winVisible := 'winUnvisible';

            if iconic then winIsIconic := 'winIsIconic'
            else
              winIsIconic := 'winIsNotIconic';

            // create key=value list with pid as key and window data as semicolon separated string
            // pid=winVisible;winIsIconic;handle;parent;Titel;Classname
            resultstring := IntToStr(ProcID) + '=' + winVisible +
              ';' + winIsIconic + ';' + IntToStr(myhandle) + ';' +
              IntToStr(parent) + ';' + Titel + ';' + myClassName;
            winlist.add(resultstring);

            // next window
            Current := GetNextWindow(Current, GW_HWNDNEXT);
            Inc(i);
            logdatei.log_prog('got window info for: ' + inttostr(i) +
            ' : ' + resultstring, LLDebug);
          end;
        end;
      end;
    end;
    stopped := True;
    Terminate;
  except
    on E: Exception do
      logdatei.log('exception in Thread execute :' + E.Message, LLError);
  end;
end;

(*
// from:
// https://www.delphipraxis.net/132461-fensterhandle-ueber-den-prozess-ermitteln.html
// https://forum.lazarus.freepascal.org/index.php?topic=59739.0
function getWindowList: TStringList;
var
  Current, parent: hwnd;
  i: integer;
  resultstring: string;
  Visible, iconic: boolean;
  handle: hwnd;
  ProcID: DWord;
  winVisible, winIsIconic: string;
  Titel: string = '';
  ClassName: string = '';
  wcharArray: array[0..260] of widechar;
begin
  Result := TStringList.Create;
  // make it sorted to increase search for win by oid performance
  Result.Sorted := True;
  // get first window
  i := 0;
  Current := GetTopWindow(GetDesktopWindow);


  while Current <> 0 do
  begin

    // get window data
    Visible := IsWindowVisible(Current);
    iconic := IsIconic(Current);
    handle := Current;
    parent := GetParent(Current);
    GetWindowThreadProcessID(Current, @ProcID);
    if GetWindowTextW(Current, wcharArray, 255) > 0 then
      Titel := SystoUTF8(strpas(wcharArray))
    else
      Titel := '';
    if GetClassNameW(Current, wcharArray, 256) > 0 then
      ClassName := SystoUTF8(strpas(wcharArray))
    else
      ClassName := '';

    if Visible then winVisible := 'winVisible'
    else
      winVisible := 'winUnvisible';

    if iconic then winIsIconic := 'winIsIconic'
    else
      winIsIconic := 'winIsNotIconic';

    // create key=value list with pid as key and window data as semicolon separated string
    // pid=winVisible;winIsIconic;handle;parent;Titel;Classname
    resultstring := IntToStr(ProcID) + '=' + winVisible + ';' +
      winIsIconic + ';' + IntToStr(handle) + ';' + IntToStr(parent) + ';' + Titel + ';' + ClassName;
    Result.add(resultstring);

    // Nächstes Fenster
    Current := GetNextWindow(Current, GW_HWNDNEXT);
    Inc(i);
  end;
  LogDatei.log('Got getWindowList: ', LLdebug2);
  LogDatei.log_list(Result, LLdebug2);
end;
*)

(* not used any more
// from:
// https://www.delphipraxis.net/132461-fensterhandle-ueber-den-prozess-ermitteln.html
// https://forum.lazarus.freepascal.org/index.php?topic=59739.0
// https://lazplanet.blogspot.com/2013/04/get-all-running-windows-titles.html
function EnumWindowsProc(Current: HWND; LParM: LParam): longbool; stdcall; export;
var
  parent: hwnd;
  resultstring: string;
  Visible, iconic: boolean;
  handle: hwnd;
  ProcID: DWord;
  winVisible, winIsIconic: string;
  Titel: string = '';
  ClassName: string = '';
  wcharArray: array[0..260] of widechar;
begin
  Result := True;
  if Current <> 0 then
  begin
    // get window data
    Visible := IsWindowVisible(Current);
    iconic := IsIconic(Current);
    handle := Current;
    parent := GetParent(Current);
    GetWindowThreadProcessID(Current, @ProcID);
    if GetWindowTextW(Current, wcharArray, 255) > 0 then
      Titel := SystoUTF8(strpas(wcharArray))
    else
      Titel := '';
    if GetClassNameW(Current, wcharArray, 256) > 0 then
      ClassName := SystoUTF8(strpas(wcharArray))
    else
      ClassName := '';

    if Visible then winVisible := 'winVisible'
    else
      winVisible := 'winUnvisible';

    if iconic then winIsIconic := 'winIsIconic'
    else
      winIsIconic := 'winIsNotIconic';

    // create key=value list with pid as key and window data as semicolon separated string
    // pid=winVisible;winIsIconic;handle;parent;Titel;Classname
    resultstring := IntToStr(ProcID) + '=' + winVisible + ';' +
      winIsIconic + ';' + IntToStr(handle) + ';' + IntToStr(parent) +
      ';' + Titel + ';' + ClassName;
    winList.add(resultstring);
  end;
end;
*)


(* not used any more
function getWindowList: TStringList;
begin
  windowList := TStringList.Create;
  // make it sorted to increase search for win by oid performance
  windowList.Sorted := True;
  EnumWindows(@EnumWindowsProc,0);
  Result := windowList;
end;
*)
(*
function getWindowList: TStringList;
begin
  Result := getWinListFromDesktop('default');
end;
*)



function getWinListFromDesktop(searchDesktop: string): TStringList;
begin
  mydesktop := searchDesktop;
  winlist := TStringList.Create;
  // make it sorted to increase search for win by pid performance
  winlist.Sorted := True;
  stopped := False;
  mythread := Tmythread.Create(False);
  mythread.FreeOnTerminate:= true;
  //mythread.WaitFor;  // do not use 'waitfor' - thread some times freeze
  // start with timeout (500 ms)
  if WaitForThreadTerminate(MyThread.Handle, 500) <> 0 then
  begin
    killThread(MyThread.Handle);
    LogDatei.log('Timeout: Thread killed.', LLnotice);
  end
  else LogDatei.log('Thread ended.', LLnotice);
  Result := winlist;
end;


function getWinListFromAllDesktops: TStringList;
var
  partlist: TStringList;
begin
  Result := TStringList.Create;
  // make it sorted to increase search for win by pid performance
  Result.Sorted := True;
  partlist := getWinListFromDesktop('winlogon');
  Result.AddDelimitedText(partlist.DelimitedText);
  FreeAndNil(partlist);
  partlist := getWinListFromDesktop('default');
  Result.AddDelimitedText(partlist.DelimitedText);
  FreeAndNil(partlist);
end;

// https://forum.lazarus.freepascal.org/index.php?topic=47466.0
function getWinProcessListWithPathAndWindowInfo: TStringList;
var
  windowList, partlist: TStringList;
  processstr, windowstr, window4PidStr, pidstr: string;
  i, k: integer;
  winFound, winVisible: boolean;
begin
  // get window list
  windowList := getWinListFromAllDesktops;
  // get processlist
  Result := getWinProcessListWithPath;

  partlist := TStringList.Create;
  partlist.Delimiter := ';';
  partlist.StrictDelimiter := True;
  // loop over all processes
  for i := 0 to Result.Count - 1 do
  begin
    // get process
    processstr := Result[i];
    partlist.DelimitedText := processstr;
    // get process pid
    pidstr := partlist[1];
    // get window data from pid
    // there may be more than one window per pid
    winFound := False;
    winVisible := False;
    window4PidStr := '';
    windowstr := '';
    // loop over all windows with this pid
    repeat
      k := windowList.IndexOfName(pidstr);
      if k >= 0 then
      begin
        winFound := True;
        windowstr := windowList.ValueFromIndex[k];
        if windowstr.StartsWith('winVisible') then
        begin
          // use the info from last visible window
          window4PidStr := windowstr;
          winVisible := True;
        end;
        // remove found window from list to find the next one
        windowList.Delete(k);
      end;
    until (k < 0);

    if winFound then
    begin
      if not winVisible then
        // use the info from last existing window
        window4PidStr := windowstr;
    end
    else
      // no window found
      window4PidStr := 'nowin;;;;;';
    // write new result string back
    Result[i] := processstr + ';' + window4PidStr;
  end;
  LogDatei.log('Got WinProcessListWithPathAndWindowInfo: ', LLdebug2);
  LogDatei.log_list(Result, LLdebug2);

  FreeAndNil(windowList);
  FreeAndNil(partlist);
end;

// get list of processes with visible window
function getRunningForegroundProcesses: TStringList;
var
  partlist: TStringList;
  processstr, windowstr: string;
  i: integer;
begin
  partlist := TStringList.Create;
  partlist.Delimiter := ';';
  // part strings have not to be quoted if they contain white spaces
  partlist.StrictDelimiter := True;
  // get processlist
  Result := getWinProcessListWithPathAndWindowInfo;
  for i := Result.Count - 1 downto 0 do
  begin
    // get process str
    processstr := Result[i];
    partlist.DelimitedText := processstr;
    // get window state
    windowstr := partlist[4];
    if not (windowstr = 'winVisible') then
      // remove process from list
      Result.Delete(i);
  end;
  FreeAndNil(partlist);
end;

{$ENDIF WINDOWS}

end.
