unit tcplisten_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, IdTCPClient, IdSocketHandle, IdAntiFreezeBase, IdAntiFreeze;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  private
    { private declarations }
  public
    { public declarations }
  end;

type
  Tmythread = class(TThread)
  public
    procedure Execute; override;
  end;

var
  Form1: TForm1;
  myTCPClient: TIdTCPClient;
  //myFreeze : TIdAntiFreeze ;
  stopped: boolean;

implementation

{$R *.lfm}

var
  mythread: Tmythread;

procedure Tmythread.Execute;
var
  receiveline: string;
  i: integer;

begin
  if not Terminated then
  begin
    //myFreeze := TIdAntiFreeze.Create;
    //myFreeze.Active:=true;
    myTCPClient := TIdTCPClient.Create;
    myTCPClient.Port := 44003;
    myTCPClient.Host := '127.0.0.1';
    myTCPClient.ReadTimeout := 1000;

    //Application.ProcessMessages;
    repeat
      try
        myTCPClient.Connect;
      except
      end;
    until myTCPClient.Connected;
    i := 1;
    while (not Terminated) and (i< 160) do
    begin
      ;
      //Application.ProcessMessages;
      receiveline := myTCPClient.Socket.ReadLn();
      receiveline := receiveline + myTCPClient.Socket.rea;
      Form1.Memo1.Append(receiveline);
      Form1.Memo1.Append(IntToStr(i) +
        ' --------------------------------------------------');
      inc(i);
    end;
  end;
end;

{ TForm1 }



procedure TForm1.FormActivate(Sender: TObject);
var
  receiveline: string;
  i: integer;
begin
  stopped := False;
  mythread := Tmythread.Create(False);

end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  mythread.Terminate;
  Application.Terminate;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  mythread.Terminate;
  Memo1.Lines.SaveToFile('C:\opsi.org\tmp\nofier-sniff.log');
end;

end.
