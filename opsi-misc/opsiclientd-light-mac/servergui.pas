unit ServerGUI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  OpsiHTTPSListeningThread;

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
    procedure DisplayStatus(aMessage:string; aLevelofLine:integer);
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
  ListeningThread.OnPassMessage:=@DisplayStatus;
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

procedure TMainForm.DisplayStatus(aMessage: string; aLevelofLine:integer);
begin
  Memo1.Lines.Append('(' + IntToStr(Memo1.Lines.Count) + ')' + #9 + '['
    + IntToStr(aLevelofLine) + '] ' + aMessage  );
end;

end.

