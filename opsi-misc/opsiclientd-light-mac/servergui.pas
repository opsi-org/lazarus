unit ServerGUI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, OpsiHTTPSListeningThread;

type

  { TMainForm }

  TMainForm = class(TForm)
    ButtonStopServer: TButton;
    ButtonStartServer: TButton;
    Memo1: TMemo;
    procedure ButtonStopServerClick(Sender: TObject);
    procedure ButtonStartServerClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    ListeningThread: TOpsiHTTPSListeningThread;
    procedure GetListeningStatus(aMessage:string);
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.ButtonStartServerClick(Sender: TObject);
begin
  ListeningThread := TOpsiHTTPSListeningThread.Create;
  ListeningThread.OnPassMessage:=@GetListeningStatus;
end;

procedure TMainForm.ButtonStopServerClick(Sender: TObject);
begin
  if ListeningThread <> nil then
    ListeningThread.Terminate;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ListeningThread <> nil then
    ListeningThread.Terminate;
end;

procedure TMainForm.GetListeningStatus(aMessage: string);
begin
  Memo1.Lines.Append(aMessage);
end;

end.

