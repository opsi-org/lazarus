unit tcplisten2_u1;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  windows,
  ExtCtrls,
  IdTCPServer,
  IdSocketHandle,
  IdContext,
  IdSync;
  //IdTCPClient, IdSocketHandle, IdAntiFreezeBase, IdAntiFreeze;

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

  TCPServerEvents = class
   public
     class procedure ExecuteHandler(AContext: TIdContext);
   end;

  TMySync = Class(TIDSync)
  private
    FContent: String;
    FDestination: TStrings;
  protected
    Procedure DoSynchronize; override;
  public
    Constructor Create(const Content: String; Destination: TStrings); overload;
  End;

var
  Form1: TForm1;
  //myTCPClient: TIdTCPClient;
  //myFreeze : TIdAntiFreeze ;
  stopped: boolean;

implementation

{$R *.lfm}

//http://stackoverflow.com/questions/13357329/tidtcpserver-onexecute-in-a-console-application
//http://stackoverflow.com/questions/10874447/delphi-tidtcpserver-and-tidtcpclient-transferring-a-record
//http://stackoverflow.com/questions/26629513/how-to-set-up-a-simple-connection-between-tidtcpclient-and-tidtcpserver-to-send
var
  //mythread: Tmythread;
  TCPServer: TIdTCPServer;


  constructor TMySync.Create(const Content: String; Destination: TStrings);
  begin
    inherited Create;
    FContent := Content;
    FDestination := Destination;
  end;

  procedure TMySync.DoSynchronize;
  begin
    //inherited;
    FDestination.Add(FContent);
  end;

  class procedure TCPServerEvents.ExecuteHandler(AContext: TIdContext);
  begin
     With TMySync.Create(AContext.Connection.Socket.ReadLn, Form1.Memo1.Lines) do
  begin
    Synchronize;
    Free;
  end;
  end;

(*
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
*)

{ TForm1 }

procedure starttcpserver;
begin
  //setup and start TCPServer
  TCPServer := TIdTCPServer.Create(nil);
  try
    with TCPServer.Bindings.Add do
    begin
      IP := '127.0.0.1';
      Port := 44004;
    end;

    TCPServer.OnExecute := TCPServerEvents.ExecuteHandler;

    try
      TCPServer.Active := True;
    except
      on E: Exception do
        OutputDebugString(PChar(E.ToString));
    end;
 except
      on E: Exception do
        OutputDebugString(PChar(E.ToString));
    end;
end;


procedure TForm1.FormActivate(Sender: TObject);
var
  receiveline: string;
  i: integer;
begin
  starttcpserver;
  //stopped := False;
  //mythread := Tmythread.Create(False);

end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  //mythread.Terminate;
   TCPServer.Active := False;
    TCPServer.Free;
  Application.Terminate;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  //mythread.Terminate;
  Memo1.Lines.SaveToFile('C:\opsi.org\tmp\nofier-sniff.log');
  Button1Click(sender);
end;

end.
