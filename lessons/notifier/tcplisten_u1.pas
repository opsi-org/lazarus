unit tcplisten_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, IdTCPClient;

type

  { TForm1 }

  TForm1 = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    procedure FormActivate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  myTCPClient : TIdTCPClient;
  stopped :boolean;

implementation

{$R *.lfm}

{ TForm1 }



procedure TForm1.FormActivate(Sender: TObject);
var
  receiveline : string;
  i : integer;
begin
  stopped := false;
  myTCPClient := TIdTCPClient.Create;
  myTCPClient.Port:= 44003;
  myTCPClient.Host:= '127.0.0.1';
  myTCPClient.ReadTimeout:=1000;
  Application.ProcessMessages;
  repeat
    try
      myTCPClient.Connect;
    except
    end;
  until myTCPClient.Connected;
  for i := 1 to 40 do
  begin;
  receiveline := myTCPClient.Socket.ReadLn();
  Memo1.Append(receiveline);
  Memo1.Append(InttoStr(i)+' --------------------------------------------------');
  end;

end;

end.

