unit HTTPSServer;

{$mode objfpc}{$H+}{$M+}

{ Marked some code with !!!}

interface

uses
  Classes, SysUtils, blcksock, winsock, Synautil, ssl_openssl, fpjson, jsonparser, OpsiJSONRequest, jsonresponse;

type
  TPassMessage = procedure(AMsg: string) of object;

  { TConnectingThread }

  TConnectingThread = class(TThread)
  private
    ConnectionSocket:TTCPBlockSocket;
    Method: string;
    Uri: string;
    Protocol: string;
    Headers: TStringList;
    ContentLength: integer;
    InputBody, OutputBody: TMemoryStream;
    TimeOut: integer;
    StatusCode: integer;
    StatusLine: string;
    RequestLine: string;
    ReasonPhrase: string;
    HTMLTestSide: TStringList;
    procedure InitSSLCertificate;
    procedure ReadMessageBody;
    procedure ReadJSONRequest;
    procedure ReadHeaders;
    procedure ReadRequestLine;
    procedure SetStatusCode(AMethod: string);
    procedure SendStatusLine;
    procedure SendHeaders;
    procedure SendMessageBody;
    procedure WriteStatusLine;
    procedure WriteHeaders;
    procedure WriteMessageBody(MessageBody: TObject);
    procedure WriteMessageBodyHTMLTestSide;
  public
    Constructor Create (ASocket:TSocket);
    Destructor Destroy; override;
    procedure Execute; override;
  end;

  { TListeningThread }

  TListeningThread = class(TThread)
  private
    ListenerSocket:TTCPBlockSocket;
    //ConnectingThread: TConnectingThread;
    PassMessage: TPassMessage;
    StatusMessage: string;
    procedure DisplayMessage;
  public
    Constructor Create;
    Destructor Destroy; override;
    procedure Execute; override;
    property OnPassMessage: TPassMessage read PassMessage write PassMessage;
  end;

implementation

{ TListeningThread }

procedure TListeningThread.DisplayMessage;
begin
  if Assigned(PassMessage) then
    PassMessage(StatusMessage);
end;

constructor TListeningThread.Create;
begin
  FreeOnTerminate := True;
  ListenerSocket := TTCPBlockSocket.create;
  ListenerSocket.CreateSocket;
  ListenerSocket.SetLinger(true,10000);
  ListenerSocket.Bind('127.0.0.1','4443');
  ListenerSocket.Listen;
  inherited Create(false);
end;

destructor TListeningThread.Destroy;
begin
  ListenerSocket.free;
  inherited Destroy;
end;

procedure TListeningThread.Execute;
var
  ClientSocket:TSocket;
begin
  StatusMessage := 'Server started';
  Synchronize(@DisplayMessage);
  repeat
    if not Terminated then
    begin
      if ListenerSocket.CanRead(1000) then
        begin
          ClientSocket:=ListenerSocket.accept;
          StatusMessage := 'Accepting...  New socket: ' + ClientSocket.ToString;
          Synchronize(@DisplayMessage);
          if ListenerSocket.LastError = 0 then
          begin
            //ConnectingThread :=
            with TConnectingThread.Create(ClientSocket) do
              StatusMessage := 'New ConnectingThread created. ThreadID: ' + IntToStr(ThreadID);
            Synchronize(@DisplayMessage);
          end;
        end;
     end;
  until Terminated;
end;

{ TConnectingThread }

procedure TConnectingThread.InitSSLCertificate;
begin
  // Note: There's no need for installing a client certificate in the
  //       webbrowser. The server asks the webbrowser to send a certificate but
  //       if nothing is installed the software will work because the server
  //       doesn't check to see if a client certificate was supplied. If you
  //       want you can install:
  //
  //       file: c_cacert.p12
  //       password: c_cakey
  //
  ConnectionSocket.SSL.CertCAFile := ExtractFilePath(ParamStr(0)) + 's_cabundle'
    +'.pem';
  ConnectionSocket.SSL.CertificateFile := ExtractFilePath(ParamStr(0)) + 's_'
    +'cacert.pem';
  ConnectionSocket.SSL.PrivateKeyFile := ExtractFilePath(ParamStr(0)) + 's_cake'
    +'y.pem';
  ConnectionSocket.SSL.KeyPassword := 's_cakey';
  ConnectionSocket.SSL.verifyCert := True;
end;

procedure TConnectingThread.ReadMessageBody;
var
  SizeReceived: integer;
begin
  //recv message body
  InputBody.Clear;
  if ContentLength >= 0 then
  begin
    InputBody.SetSize(ContentLength);
    SizeReceived := ConnectionSocket.RecvBufferEx(InputBody.Memory, ContentLength, Timeout);
    InputBody.SetSize(SizeReceived);
  end;
end;

procedure TConnectingThread.ReadJSONRequest;
var
  //JSONStream: TStringStream;
  //JSONrpc: TJSONObject;
  //JSONTest: TJSONObject;
  rpcMethod: string;
  rpcParams: TJSONArray;
  rpcID: integer;
  Param: TJSONEnum;
  OnDemand: boolean = false;
  OpsiJSONRequest: TOpsiJSONRequest;
begin
  OpsiJSONRequest := TOpsiJSONRequest.Create('fireEvent', ['on_demand'], 1);// only for testing
  //JSONStream := TStringStream.Create(JSONRequest.AsJSON);
  rpcMethod := OpsiJSONRequest.AsJSON;
  InputBody.Clear;
  OpsiJSONRequest.SaveToMemoryStream(InputBody);
  FreeAndNil(OpsiJSONRequest);
  //InputBody.Clear;
  //InputBody.LoadFromStream(JSONStream);
  OpsiJSONRequest := TOpsiJSONRequest.Create(InputBody);
  rpcMethod := OpsiJSONRequest.AsJSON;
  rpcMethod := OpsiJSONRequest.Method;//JSONrpc.FindPath('method').AsString;
  rpcParams := OpsiJSONRequest.Params;//JSONrpc.FindPath('params');
  rpcID := OpsiJSONRequest.ID;
  for Param in OpsiJSONRequest.Params do
  begin
    if Param.Value.AsString = 'on_demand' then OnDemand := true;
  end;
  //OnDemand := rpcParams;
  //rawJSON.DataString;
  //FreeAndNil(JSONStream);
  FreeAndNil(OpsiJSONRequest);
end;

procedure TConnectingThread.ReadHeaders;
var
  Header: string = '';
begin
  //read request headers
  Headers.Clear;
  ContentLength := -1;
  if (pos('HTTP/', protocol) = 1) then
  begin
    repeat
      Header := ConnectionSocket.RecvString(Timeout);
      if (Header <> '') and (ConnectionSocket.LastError = 0) then
      begin
        Headers.add(Header);
        if Pos('CONTENT-LENGTH:', Uppercase(Header)) = 1 then
          ContentLength := StrToIntDef(SeparateRight(Header, ' '), -1);
      end;
    until Header = '';
  end;
end;

procedure TConnectingThread.ReadRequestLine;
begin
  //read request line
  RequestLine := ConnectionSocket.RecvString(timeout);
  if (ConnectionSocket.LastError = 0) and (RequestLine <> '') then
  begin
    Method := fetch(RequestLine, ' ');
    if (Method <> '') and (RequestLine <> '') then
    begin
      Uri := fetch(RequestLine, ' ');
    end;
    if (Uri <> '') then
    begin
      Protocol := fetch(RequestLine, ' ');
    end;
  end;
end;

procedure TConnectingThread.SetStatusCode(AMethod: string);
begin
  StatusCode := 504;
  if AMethod = 'GET' then
  begin
    StatusCode := 200;
    ReasonPhrase := 'OK';
  end;
  if AMethod = 'POST' then
  begin
    StatusCode := 200;
    ReasonPhrase := 'OK';
  end;
end;

procedure TConnectingThread.SendStatusLine;
begin
  ConnectionSocket.SendString(StatusLine);
end;

procedure TConnectingThread.SendHeaders;
var
  i: integer;
begin
  if Protocol <> '' then
  begin
    for i := 0 to Headers.count - 1 do
      ConnectionSocket.SendString(Headers[i] + CRLF);
  end;
end;

procedure TConnectingThread.SendMessageBody;
begin
  if (ConnectionSocket.LastError = 0) then
  begin
    ConnectionSocket.SendBuffer(OutputBody.Memory, OutputBody.Size);
  end;
end;

procedure TConnectingThread.WriteStatusLine;
begin
  StatusLine :=  'HTTP/1.1' + ' ' + IntTostr(StatusCode) + ' ' + ReasonPhrase + CRLF;
end;

procedure TConnectingThread.WriteHeaders;
begin
  if Protocol <> '' then
  begin
    Headers.Clear;
    Headers.Add('Content-type: text/html; charset=UTF-8');
    Headers.Add('Content-length: ' + IntTostr(OutputBody.Size));
    Headers.Add('Connection: close');
    Headers.Add('Date: ' + Rfc822DateTime(now));
    Headers.Add('Server: opsiclientd-light');
    Headers.Add('');
  end;
end;

procedure TConnectingThread.WriteMessageBody(MessageBody: TObject);
begin
  if MessageBody is TStringList then (MessageBody as TStringList).SaveToStream(OutputBody);
  if MessageBody is TOpsiJSONRequest then (MessageBody as TOpsiJSONRequest).SaveToMemoryStream(OutputBody);
  if MessageBody is TJSONResponse then (MessageBody as TJSONResponse).SaveToMemoryStream(OutputBody);
end;


procedure TConnectingThread.WriteMessageBodyHTMLTestSide;
var
  MessageBody: TStringlist;
begin
  MessageBody := TStringList.Create;
  try
    MessageBody.Add('<html>');
    MessageBody.Add('<head></head>');
    MessageBody.Add('<body>');
    MessageBody.Add('<h2>This document is generated by opsiclientd-light for mac!</h2>');
    MessageBody.Add('Request Uri: ' + Uri);
    MessageBody.Add('<br>');
    if ConnectionSocket.SSL.GetPeerName = '' then
      MessageBody.Add('No client certificate received')
    else
      MessageBody.Add('Client certificate received from ' +
        ConnectionSocket.SSL.GetPeerName);
    MessageBody.Add('</body>');
    MessageBody.Add('</html>');
    OutputBody.Clear;
    MessageBody.SaveToStream(OutputBody);
  finally
    MessageBody.free;
  end;
end;

constructor TConnectingThread.Create(ASocket: TSocket);
begin
  FreeOnTerminate:=true;
  ConnectionSocket:=TTCPBlockSocket.create;
  InitSSLCertificate;
  TimeOut := 120000;
  Headers := TStringList.Create;
  InputBody := TMemoryStream.Create;
  OutputBody := TMemoryStream.Create;
  ConnectionSocket.Socket:=ASocket;
  inherited create(false);
end;

destructor TConnectingThread.Destroy;
begin
  ConnectionSocket.free;
  Headers.Free;
  InputBody.Free;
  OutputBody.Free;
  inherited Destroy;
end;

procedure TConnectingThread.Execute;
begin
  try
    if  ConnectionSocket.SSLAcceptConnection
      and (ConnectionSocket.SSL.LastError = 0) then
    begin
      { read request }
      ReadRequestLine;
      ReadHeaders;
      ReadMessageBody;
      ReadJSONRequest;
      { do something with the received data }
      SetStatusCode(Method);
      { write response }
      WriteStatusLine;
      if (StatusCode = 200) then WriteMessageBodyHTMLTestSide;
      if (StatusCode = 200) then WriteMessageBodyHTMLTestSide;

      { WriteMessageBody MUST BE before WriteHeaders otherwise the content-lenght is not known (e.g. 0)
        and the Message Body MIGHT BE not read by the client!!!
        Only relvant for plain text if content-encoding is set then NO CONTENT-LENGTH MUST BE SET }
      WriteHeaders;
      { send response }
      if ConnectionSocket.lasterror = 0 then
      begin
        SendStatusLine;
        SendHeaders;
        SendMessageBody;
      end;
    end
    else
    begin
      //StatusMessage := 'Error while accepting SSL connection: ' + ConnectionSocket.SSL.LastErrorDesc;
    end;
  except
    //StatusMessage := 'Exception while accepting SSL connection';
  end;
end;


end.
