unit linux_system;

//http://rick-ross.com/papers/borcon2001/6204.html
//linkill , linpopen from
// Written for "Kylix Power Solutions"
// Copyright (c) 2001, Coriolis Group Books


interface
{$IFDEF LINUX}
uses Classes, libc, SysUtils, fork_exec, ProcStuff
//LibcExec
;

const
// dummy definition (for later implementation)
SW_HIDE = 0;
SW_NORMAL = 1;
SW_MAXIMIZE = 3;
SW_SHOW = 5;
SW_MINIMIZE = 6;
SW_SHOWDEFAULT = 10;
SW_SHOWMAXIMIZED = 3;
SW_SHOWMINIMIZED = 2;
SW_SHOWMINNOACTIVE = 7;
SW_SHOWNA = 8;
SW_SHOWNOACTIVATE = 4;
SW_SHOWNORMAL = 1;


// forward definition

function linStart(cmdline : string;
                       ShowWindowFlag : Integer;
                       WaitForReturn : Boolean;
                       WaitForWindowVanished : Boolean;
                       WaitForWindowAppearing : Boolean;
                       WindowIdent : String;
                       WaitSecs : Word;
                       Var Report : String): Boolean;
procedure linKill(name : string);
procedure linPopen(cmd : String; var output : TStringList);
function linSysInfo : string;
{$ENDIF}

implementation
{$IFDEF LINUX}




//Using the Classes - A Simple OO Shell

//Now that the TForker and TExecuter classes have been created, lets re-write the simple shell example.

function linStart(cmdline : string;
                       ShowWindowFlag : Integer;
                       WaitForReturn : Boolean;
                       WaitForWindowVanished : Boolean;
                       WaitForWindowAppearing : Boolean;
                       WindowIdent : String;
                       WaitSecs : Word;
                       Var Report : String): Boolean;
var
  cmd     : string;
  f       : TForker;

  procedure ParseArgs(commandline : string; var cmd : string;
                      strlst : TStrings);
  var
    tmp    : string;
    i      : integer;

  begin
    // start with an empty parameter list
    strlst.Clear;

    tmp := commandline;
    i   := Pos(' ',tmp);
    while (i > 0) do
    begin
      // found an argument
      // extract the string from 1 to i-1
      strlst.Add(  Copy(tmp,1,(i-1)) );

      tmp := Copy(tmp,i+1,length(tmp));
      i   := Pos(' ',tmp);
    end;

    if (tmp <> '') then
    begin
      // found an argument
      // extract the string from 1 to len
      strlst.Add( Copy(tmp,1,length(tmp)) );
    end;

    // now get the command from the first argument
    cmd := strlst.Strings[0];

    // and delete the first parameter
    strlst.Delete(0);
  end; // ParseArgs

begin
  // create the forker object
  f := TForker.Create;
  f.Exec := TExecuter.Create;
  f.WaitForChild := WaitForReturn;


  // search for arguments in the string
  ParseArgs(cmdline, cmd, f.Exec.Parameters);
  f.Exec.ProcessName := cmd;
  try
   f.DoFork;
  except
   on e : Exception do
   begin
    Report := 'Error executing '+cmdline+' : '+e.message;
    result := false;
   end;
  end;
  result := true;
  Report := 'Process executed    ' + CmdLine;
  if not WaitForReturn then
   sleep(1000*WaitSecs);
end;

procedure linKill(name : string);
var
 i : Integer;
 L : TList;
 PRec : PProcInfoRec;
begin
 L := GetProcessListByName(name);
 if L.Count > 0
  then for i := 0 to L.Count - 1 do
   begin
    PRec := L.Items[i];
    with PRec^ do
    (*
     if MessageDlg('Kill Process ',
         'Process ' + IntToStr(i + 1) + ' of ' + IntToStr(L.Count)
          + LF + LF
         + 'Kill this process?' + LF + LF
         + 'Process ID: ' + IntToStr(PID) + LF
         + 'Status: ' + Status + LF
         + 'User name: ' + UName + LF
         + 'Command: ' + CmdName + LF,
         mtConfirmation, [mbYes, mbNo], 0) = mrYes
     then *)
     kill(PID, SIGTERM);
   end; { for }
   //else ShowMessage('No matches found for ' + PROCESSNAME);
 L.Free;
end;

procedure linPopen(cmd : String; var output : TStringList);
const
 LF = ^J; { ASCII linefeed/newline }
 READ_IOMode = 'r'; { read mode from pipe }
var
 CmdArr : array[0..512] of char;
 StrArr : array[0..1024] of char;
 F : PIOFile;
 pPipeStr : Pointer;
 s : String;
begin
 output.Clear;

 { Set up to run the ps command }
 StrCopy(CmdArr, Pchar(cmd));


{ Open a pipe for reading from ps's output }
 F := popen(CmdArr, READ_IOMode);
 if assigned(F)
  then begin
        repeat
         { Read a complete line from the ps output stream }
         pPipeStr := fgets(StrArr, 1024, F);
         if Assigned(pPipeStr)
          then begin
                s := StrPas(pPipeStr);
                if pos(LF, s) > 0 then delete(s, pos(LF, s), 1);
                output.Add(s);
               end;
        until not Assigned(pPipeStr);
        { Close the pipe - don't try to catch errors }
        pclose(F);
       end;
end;


function linSysInfo : string;
var
 strList : TStringList;
begin
 strList := TStringList.create;
 linPopen('uname -a',strList);
 result := strList.Text;
 strList.Clear;
 if FileExists('/etc/issue.net') then
 begin
  strList.LoadFromFile('/etc/issue.net');
  result := strList.Text;
  strList.Clear;
 end;
 if FileExists('/etc/SuSE-release') then
 begin
  strList.LoadFromFile('/etc/SuSE-release');
  result := strList.Text;
  strList.Clear;
 end;
 if FileExists('/etc/mandrake-release') then
 begin
  strList.LoadFromFile('/etc/mandrake-release');
  result := strList.Text;
  strList.Clear;
 end;
 strList.Free;
end;

{$ENDIF}
end.
