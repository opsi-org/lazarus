unit IndependentMessageDisplayer;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF GUI}
  Forms, Controls, Graphics, Dialogs, ExtCtrls,
  {$ENDIF GUI}
  Classes, SysUtils;

type
  TMessageThread = class(TThread)
    FMessage: string;
    procedure DisplayMessageOnForm; virtual; abstract;
    procedure DisplayMessageDialog; virtual; abstract;
  end;

  TIndependentMessageDisplayer = class(TObject)
    FThread: TMessageThread;
    constructor Create(Thread: TMessageThread = nil);
    procedure DisplayMessage(Message: string; DisplayOnForm: boolean = False);
  end;


implementation

constructor TIndependentMessageDisplayer.Create(Thread: TMessageThread = nil);
begin
  inherited Create;
  FThread := Thread;
end;

procedure TIndependentMessageDisplayer.DisplayMessage(Message: string;
  DisplayOnForm: boolean = False);
begin
  {$IFDEF GUI}
  if DisplayOnForm then
  begin
    FThread.FMessage := Message;
    FThread.Synchronize(@FThread.DisplayMessageOnForm);
  end
  else
  begin
    FThread.FMessage := Message;
    FThread.Synchronize(@FThread.DisplayMessageDialog);
  end;
  {$ENDIF GUI}

  {$IFDEF NOGUI}
  // For a generalized procedure we need the DisplayOnForm parameter since there
  // are two ways in a gui program to display a message but for console applications
  // the parameter doesn't play any role.
  if DisplayOnForm or not DisplayOnForm then
    writeln(Message);
  {$ENDIF NOGUI}
end;

end.
