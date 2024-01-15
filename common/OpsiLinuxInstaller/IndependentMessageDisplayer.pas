unit IndependentMessageDisplayer;

// Display a message on a Label (GUI), by the 'ShowMessage' function (GUI) or on the console (NOGUI).

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF GUI}
  Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  {$ENDIF GUI}
  Classes, SysUtils;

type

  TIndependentMessageDisplayer = class(TObject)
    {$IFDEF GUI}
    FMessageLabel: TLabel;
    constructor Create(MessageLabel: TLabel = nil);
    {$ENDIF GUI}
    procedure DisplayMessage(Message: string; DisplayOnLabel: boolean = False);
  end;


implementation

{$IFDEF GUI}
constructor TIndependentMessageDisplayer.Create(MessageLabel: TLabel = nil);
begin
  inherited Create;
  FMessageLabel := MessageLabel;
end;
{$ENDIF GUI}

procedure TIndependentMessageDisplayer.DisplayMessage(Message: string;
  DisplayOnLabel: boolean = False);
begin
  // For a generalized procedure we need the DisplayOnLabel parameter since there
  // are two ways in a gui program to display a message but for console applications
  // the parameter doesn't play any role.
  {$IFDEF GUI}
  if DisplayOnLabel then
  begin
    FMessageLabel.Caption := Message;
    Application.ProcessMessages;
  end
  else
  begin
    ShowMessage(Message);
  end;
  {$ENDIF GUI}
  {$IFDEF NOGUI}
  writeln(Message);
  {$ENDIF NOGUI}
end;

end.
