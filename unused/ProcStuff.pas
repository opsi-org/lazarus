// 
//          Process Information Library Unit 
//             ProcStuff.pas : Unit File 
//              Developed by Don Taylor 
// 
// Description: Routines for retrieving process status
// and a list of currently running processes.
// 
// Written for "Kylix Power Solutions" 
// Copyright (c) 2001, Coriolis Group Books 
//       Last revised 31 May 2001 
//

unit ProcStuff;

interface
{$IFDEF LINUX}

uses SysUtils, Classes, Libc;

const
 NO_ERR    =  0;
 NOT_FOUND = -1;
 PIPE_ERR  = -2;
 MISC_ERR  = -3;

type
 PProcInfoRec = ^TProcInfoRec;
 TProcInfoRec =
  record
   PID : Integer; { process id }
   Status : String; { process status }
   uName : String; { name of process initiator }
   CmdName : String; { name of process (no path) }
  end; { record }

{
  - For a given processs ID, return the process
    status, the login of the user who invoked the
    process, and the command line used to invoke it.
    Return value indicates success.
}
function GetProcessStatus(    PID : Integer;
                          var Status : String;
                          var UName  : String;
                          var Cmd    : String) : Integer;

{
  - For a given process name, return a pointer
    to a list of records that contain the process
    ID, its status and the name of the user who
    initiated the process. If no matches were
    found, return nil.
}
function GetProcessListByName(Cmd : String)
  : TList;
{$ENDIF}
implementation
{$IFDEF LINUX}

function ParseToSpace(var InStr : String) : String;
var
 OutStr : String;
begin
 OutStr := '';
 while (Length(Instr) > 0) and (InStr[1] = ' ') do
  Delete(Instr, 1, 1);
 while (Length(Instr) > 0) and (InStr[1] <> ' ') do
  begin
   OutStr := OutStr + InStr[1];
   Delete(InStr, 1, 1);
  end; { while }
 Result := OutStr;
end; { ParseToSpace }

function GetProcessStatus(    PID : Integer;
                          var Status : String;
                          var UName  : String;
                          var Cmd    : String) : Integer;
const
 PIPE_CMD : PChar = 'ps -eo pid,stat,user,args | grep ';
 PIPE_TYPE : PChar = 'r'; { read from the pipe }
var
 CmdArr : array[0..512] of char;
 StrArr : array[0..1024] of char;
 F : PIOFile;
 s : String;
 ErrResult : Integer;
 PtrResult : Pointer;
 Found : Boolean;
 PSLine : String;
begin
 ErrResult := NO_ERR;
 StrPCopy(CmdArr, PIPE_CMD);
 StrPCopy(StrArr, IntToStr(PID));
 StrCat(CmdArr, StrArr);

 F := popen(CmdArr, PIPE_TYPE);
 if F = nil
  then ErrResult := PIPE_ERR
  else begin
        Found := False;
        repeat
         PtrResult := fgets(StrArr, 1024, F);
         if PtrResult <> nil
          then begin
                PSLine := StrPas(StrArr);
                PSLine := Copy(PSLine, 1, Length(PSLine) - 1);
                s := ParseToSpace(PSLine);
                Found := StrToInt(s) = PID;
                if Found
                 then begin
                       { Parse out the values }
                       Status := ParseToSpace(PSLine);
                       UName  := ParseToSpace(PSLine);
                       Cmd    := ParseToSpace(PSLine);
                      end;
               end;
        until Found or (PtrResult = nil);

        if (PtrResult = nil) and (ErrResult = NO_ERR)
         then ErrResult := NOT_FOUND;
        if (pclose(F) = -1) and (ErrResult = NO_ERR)
         then ErrResult := PIPE_ERR;
       end;

 Result := ErrResult;
end;

function GetProcessListByName(Cmd : String)
  : TList;
const
 PIPE_CMD : PChar = 'ps -eo pid,stat,user,args | grep ';
 PIPE_TYPE : PChar = 'r'; { read from the pipe }
var
 CmdArr : array[0..512] of char;
 StrArr : array[0..1024] of char;
 F : PIOFile;
 PtrResult : Pointer;
 AList : TList;
 Found : Boolean;
 PSLine : String;
 ProcRec : PProcInfoRec;
 PID : Integer;
 Status : String;
 UName : String;
 CmdName : String;
begin
 AList := TList.Create;
 StrPCopy(CmdArr, PIPE_CMD);
 StrPCopy(StrArr, Cmd);
 StrCat(CmdArr, StrArr);

 F := popen(CmdArr, PIPE_TYPE);
 if F = nil
  then begin
        Result := nil;
        AList.Free;
        Exit;
       end;

 repeat
  PtrResult := fgets(StrArr, 1024, F);
  if PtrResult <> nil
   then begin
         PSLine := StrPas(StrArr);
         PSLine := Copy(PSLine, 1, Length(PSLine) - 1);
         PID := StrToInt(ParseToSpace(PSLine));
         Status := ParseToSpace(PSLine);
         UName := ParseToSpace(PSLine);
         CmdName := ParseToSpace(PSLine);
         CmdName := ExtractFileName(CmdName);
         Found := CmdName = Cmd;
         if Found
          then begin
                ProcRec := New(PProcInfoRec);
                ProcRec.PID := PID;
                ProcRec.Status := Status;
                ProcRec.UName := UName;
                ProcRec.CmdName := CmdName;
                AList.Add(ProcRec);
               end;
        end;
   until PtrResult = nil;

 pclose(F);

 Result := AList;
end;

{$ENDIF}
end.
