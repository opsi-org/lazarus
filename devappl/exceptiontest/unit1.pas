unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure log_exception(E: Exception);
    procedure DumpCallStack;
    function getCallAddrStr : string;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

//http://wiki.freepascal.org/Logging_exceptions

procedure TForm1.log_exception(E: Exception);
var
  I: Integer;
  Frames: PPointer;
  Report: string;
begin
  //DumpCallStack;
  memo1.Append(getCallAddrStr);
   memo1.Append('++++++++++++++++++++');
  //Report := 'Program exception! ' + LineEnding +'Stacktrace:' + LineEnding + LineEnding;
  if E <> nil then begin
    Report := 'Exception class: ' + E.ClassName + LineEnding +
    'Message: ' + E.Message + LineEnding;
    memo1.Append(Report);
  end;
  Report := BackTraceStrFunc(ExceptAddr);
  memo1.Append(Report);
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    begin
      Report := BackTraceStrFunc(Frames[I]);
      memo1.Append(Report);
    end;
end;

procedure TForm1.DumpCallStack;
var
  I: Longint;
  prevbp: Pointer;
  CallerFrame,
  CallerAddress,
  bp: Pointer;
  Report: string;
const
  MaxDepth = 20;
begin
  Report := '';
  bp := get_frame;
  // This trick skip SendCallstack item
  // bp:= get_caller_frame(get_frame);
  try
    prevbp := bp - 1;
    I := 0;
    while bp > prevbp do begin
       CallerAddress := get_caller_addr(bp);
       CallerFrame := get_caller_frame(bp);
       if (CallerAddress = nil) then
         Break;
       Report := Report + BackTraceStrFunc(CallerAddress) + LineEnding;
       Inc(I);
       if (I >= MaxDepth) or (CallerFrame = nil) then
         Break;
       prevbp := bp;
       bp := CallerFrame;
     end;
   except
     { prevent endless dump if an exception occured }
   end;
  memo1.Append(Report);
end;

function TForm1.getCallAddrStr : string;
// derivated from DumpCallStack
// http://wiki.freepascal.org/Logging_exceptions
var
  CallerAddress,
  bp: Pointer;
begin
  result := '';
  bp := get_frame;
  // This trick skip SendCallstack item
  // bp:= get_caller_frame(get_frame);
  try
       CallerAddress := get_caller_addr(bp);
       if (CallerAddress <> nil) then
         result := BackTraceStrFunc(CallerAddress) + LineEnding;
   except
     { prevent endless dump if an exception occured }
   end;
end;


procedure TForm1.Button1Click(Sender: TObject);
var
  myint, i,k : integer;
  list : TStringlist;
begin
  try
    i := 0;
    list.Append('huhu');
    //myint := 25 div i;
  except
     on E: Exception do
     begin
       memo1.Append('Message:'+e.Message);
       memo1.Append('Unit:'+e.UnitName);
       memo1.Append('Method:'+e.MethodName(ExceptAddr));
       //DumpCallStack;
       memo1.Append(getCallAddrStr);
       memo1.Append('++++++++++++++++++++');
       log_exception(e);
       //DumpExceptionBackTrace(stdout);
     end;
  end;
end;

end.

