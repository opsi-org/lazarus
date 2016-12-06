unit fork_exec;
//http://rick-ross.com/papers/borcon2001/6204.html


interface
{$IFDEF LINUX}
uses Classes, libc, SysUtils;//, Process;
// forward definition

type
TExecuter = class;

TForkerEventProc = procedure of Object;
TForkerWhichProc = (wpParent, wpChild);

TForker = class
private
  FDebug        : boolean;
  FOnChild      : TForkerEventProc;
  FWait         : boolean;
  FExec         : TExecuter;
  FStatus       : integer;

//  procedure DebugMsg(msg : string);
public
  constructor Create;
  function    DoFork : TForkerWhichProc;
  procedure   DoWait;

published
  property Debug        : boolean          read FDebug   write FDebug;
  property OnChild      : TForkerEventProc read FOnChild write FOnChild;
  property WaitForChild : boolean          read FWait    write FWait;
  property Exec         : TExecuter        read FExec    write FExec;
  property WaitStatus   : integer          read FStatus;
end;


TArgArray = array of AnsiString;

TExecuter = class
private
  FDebug    : Boolean;
  FParms    : TStrings;
  FProcName : AnsiString;

  function StringListToCarray( cmd : AnsiString;
                               strlst : TStrings ) : TArgArray;
//  procedure DebugMsg(msg : string);
protected
//  function ListArgArray(aa : TArgArray) : string;

public
  constructor Create;
  destructor  Destroy; override;
  procedure   Exec;

published
  property Debug       : boolean      read FDebug    write FDebug;
  property Parameters  : TStrings     read FParms    write FParms;
  property ProcessName : AnsiString   read FProcName write FProcName;
end;

{$ENDIF}

implementation
{$IFDEF LINUX}

//Constructor

//The constructor initializes some default values.

constructor TForker.Create;
begin
  inherited Create;
  FDebug := true;
  FWait  := true;
end;


//Forking

//The DoFork method is the most complex function.
//It handles the actual forking code and determines what the class needs
//to do. Immeadiately, it calls the fork function.
//If fork returns an error, an exception is raised.
//When the child code is being executed,
//it first checks to see if the OnChild event has been assigned.
//If so, it calls the OnChild event.
//Next, it checks to see if the Exec property has been assigned.
//If so, it calls the Exec method.
//Finally, it returns notifying the caller that it is the child process.
//When the parent code is being executed,
//it checks the WaitForChild property and waits if necessary.
//When the waiting is over, or if there is no reason to wait,
//it returns, notifying the caller that it is in the parent process.

function   TForker.DoFork : TForkerWhichProc;
var
  i : integer;

begin
  i := fork;
  if i = -1 then
  begin
    raise Exception.CreateFmt('Unable to fork: Error is %d',[GetLastError]);
  end
  else if i = 0 then
  begin
    // we are in the child...
    Result := wpChild;

    // call the child
    if Assigned (FOnChild) then
      FOnChild
    else if Assigned (FExec) then
    begin
      // do the exec thing..
      FExec.Exec;
    end;

    // otherwise we fall through and let the
    // caller handle it..
  end
  else
  begin
    // we are the parent...
    Result := wpParent;

    if FWait then
    begin
      // wait for child
      wait(@FStatus);
    end;
  end;
end;


//Waiting

//The DoWait method, is using the blocking version of wait. Call this method when the parent process needs more control and does not want to have the TForker class do the waiting.

procedure TForker.DoWait;
begin  if not FWait then
    wait(@FStatus);
end;

//OnChild Property

//The OnChild property provides a callback method when the child process is being executed.
//WaitForChild Property

//This property determines if the TForker class will wait for the child or allow the parent to decide to wait or not.
//Creating an "Exec"ing Class

//Now it is time to write a class that wraps an exec function. This class will take a process name, a list of parameters and exec the process. In this implementation, only the execv function is being used. In order to support the other variations of the exec family functions, an environment property would need to be added and a method of choosing which exec function to use.
//Class Definition

//---------------
//Constructor

//In the constructor, properties are created and initialized.

constructor TExecuter.Create;
begin
  inherited Create;
  FDebug    := false;
  FProcName := '';
  FParms    := TStringList.Create;
end;


//Destructor

//The destructor releases the parameter list that was created in the constructor.

destructor TExecuter.Destroy;
begin
  FParms.Free;
  inherited Destroy;
end;


//Exec Method

//The Exec method takes the process name and parameters, puts them into an array and calls the execv function to overlay the current process with the one specified.

procedure TExecuter.Exec;
var
  parms : TArgArray;
  cmd   : AnsiString;
  j     : integer;

begin
  cmd   := FProcName;
  parms := StringListToCarray(cmd,FParms);
 
  j := execv(PChar(cmd), PPChar(@parms[0]));
  if j = -1 then
    raise Exception.CreateFmt('execv failed error %d',[GetLastError]);
  // when properly executed, execv will never return...
end;


//Parameters Property

//Parameters play a crucial role in executing a process
//and even more so, when using the execv function.
//In order to pass the parameters to it, the private method StringListToCarray
//is called to convert the string list to a structure that
//the execv function needs. This structure is an array of AnsiStrings.
//The first value is the command or process name.
//Subsequent positions in the array are filled with the parameters
//and the last position is nil, indicating the end of the array.

function TExecuter.StringListToCarray( cmd : AnsiString;
                                       strlst : TStrings ) : TArgArray;

var
  i,cnt : integer;

begin
  // set the array one bigger to account for the "NULL" end of array terminator

  cnt := strlst.Count+1;
  if cmd <> '' then
    inc(cnt);

  SetLength(Result, cnt);

  // when cmd is nothing, this will be overwritten
  Result[0] := cmd;

  for i:= 0 to strlst.Count-1 do
  begin
    Result[i+1] := strlst.Strings[i];
  end;
end;

{$ENDIF}
end.
