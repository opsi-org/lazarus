unit OpsiHTTPSAcceptingThread;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Classes, SysUtils, blcksock, sockets, Synautil, ssl_openssl, fpjson, jsonparser, OpsiJSONRequest, OpsiJSONResponse, Process,
  OpsiHTMLMessageBody;

type

  { TOpsiHTTPSAcceptingThread }

  TOpsiHTTPSAcceptingThread = class(TThread)
  private
    AcceptorSocket:TTCPBlockSocket;
    Method: string;
    Uri: string;
    Protocol: string;
    Headers: TStringList;
    ContentLength: integer;
    InputBody, OutputBody: TMemoryStream;
    JSONRequest: TOpsiJSONRequest;
    JSONResponse: TOpsiJSONResponse;
    OnDemand: boolean;
    TimeOut: integer;
    StatusCode: integer;
    StatusLine: string;
    RequestLine: string;
    ReasonPhrase: string;
    FormerThread: TThread;
    //MessageBody: TOpsiHTTPMessageBody;
    procedure CreateTestJSONRequestInputBody;// This function is only for testing
    procedure InitSSLCertificate;
    procedure InitSSLOpsi;
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
    //procedure WriteMessageBody(aMessageBody: TObject);
    //procedure WriteHTMLTestSide;
    procedure WriteMessageBodyHTMLTestSide;
    procedure WriteMessageBodyJSONResponse;
  public
    Constructor Create (ASocket:TSocket; AFormerThread:TThread);
    Destructor Destroy; override;
    procedure Execute; override;
  end;


implementation

{ TOpsiHTTPSAcceptingThread }

procedure TOpsiHTTPSAcceptingThread.WriteMessageBodyHTMLTestSide;
var
  HTML : TOpsiHTMLMessageBody;
begin
  if (StatusCode = 200) then
  begin
    HTML := TOpsiHTMLMessageBody.Create(Uri,
      AcceptorSocket.SSL.GetPeerName);
    OutputBody.Clear;
    HTML.SaveToMemoryStream(OutputBody);
    FreeAndNil(HTML);
  end;
end;

procedure TOpsiHTTPSAcceptingThread.WriteMessageBodyJSONResponse;
//var
  //ResponseError : TJSONOBject;
  //JSONResult : TJSONOBject;
begin
  if (StatusCode = 200) then
  begin
    //MessageBody := TOpsiJSONMessageBody.Create(nil);
    //ResponseError := TJSONObject.Create;
    //REsponseError :=
    //ResponseError.Add('error', 'null');
    //JSONResult := TJSONObject.Create;
    //JSONResult.Add('result', 'null');
    JSONResponse := TOPsiJSONResponse.Create(nil,nil,JSONRequest.ID);
    //JSONREsponse.FormatJSON();
    OutputBody.Clear;
    JSONResponse.SaveToMemoryStream(OutputBody);
  end;
end;


procedure TOpsiHTTPSAcceptingThread.InitSSLCertificate;
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
  AcceptorSocket.SSL.CertCAFile := ExtractFilePath(ParamStr(0)) + 's_cabundle'
    +'.pem';
  AcceptorSocket.SSL.CertificateFile := ExtractFilePath(ParamStr(0)) + 's_'
    +'cacert.pem';
  AcceptorSocket.SSL.PrivateKeyFile := ExtractFilePath(ParamStr(0)) + 's_cake'
    +'y.pem';
  AcceptorSocket.SSL.KeyPassword := 's_cakey';
  AcceptorSocket.SSL.verifyCert := True;
end;

procedure TOpsiHTTPSAcceptingThread.InitSSLOpsi;
begin
  AcceptorSocket.SSL.Username:= 'vmmacdev1onmm1.uib.local';
  AcceptorSocket.SSL.Password:= 'aead8f8c57a92e14ac820bf8d3df1805';
end;


procedure TOpsiHTTPSAcceptingThread.CreateTestJSONRequestInputBody; // This function is only for testing
var
  TestJSONRequest: TOpsiJsonRequest;
begin
  TestJSONRequest := TOpsiJSONRequest.Create('fireEvent', ['on_demand'], 1);
  //JSONStream := TStringStream.Create(JSONRequest.AsJSON);
  //teststring := TestJSONRequest.AsJSON;
  InputBody.Clear;
  TestJSONRequest.SaveToMemoryStream(InputBody);
  FreeAndNil(TestJSONRequest);
end;

procedure TOpsiHTTPSAcceptingThread.ReadMessageBody;
var
  SizeReceived: integer;
  rpcMethod: string;
  rpcParams: TJSONArray;
  rpcID: integer;
  //Param: TJSONEnum;
  //OpsiJSONRequest: TOpsiJSONRequest;
begin
  //recv message body
  InputBody.Clear;
  if ContentLength >= 0 then
  begin
    InputBody.SetSize(ContentLength);
    SizeReceived := AcceptorSocket.RecvBufferEx(InputBody.Memory, ContentLength, Timeout);
    InputBody.SetSize(SizeReceived);
    //AcceptorSocket.RecvStream(InputBody, TimeOut);
    //rpcMethod := AcceptorSocket.RecvString(TimeOut);
  end;
  //CreateTestJSONRequestInputBody;
  //InputBody.Clear;
  //InputBody.LoadFromStream(JSONStream);
  JSONRequest := TOpsiJSONRequest.Create(InputBody);
  //rpcMethod := JSONRequest.AsJSON; //for testing
  //rpcMethod := JSONRequest.Method; //for testing
  //rpcParams := JSONRequest.Params; //for testing
  //rpcID := JSONRequest.ID;  //for testing

  //for Param in JSONRequest.Params do
  //begin
  //  if Param.Value.AsString = 'on_demand' then OnDemand := true;
  //end;
end;

procedure TOpsiHTTPSAcceptingThread.ReadJSONRequest;
begin

end;


procedure TOpsiHTTPSAcceptingThread.ReadHeaders;
var
  Header: string = '';
begin
  //read request headers
  Headers.Clear;
  ContentLength := -1;
  if (pos('HTTP/', protocol) = 1) then
  begin
    repeat
      Header := AcceptorSocket.RecvString(Timeout);
      if (Header <> '') and (AcceptorSocket.LastError = 0) then
      begin
        Headers.add(Header);
        if Pos('CONTENT-LENGTH:', Uppercase(Header)) = 1 then
          ContentLength := StrToIntDef(SeparateRight(Header, ' '), -1);
      end;
    until Header = '';
  end;
end;

procedure TOpsiHTTPSAcceptingThread.ReadRequestLine;
begin
  //read request line
  RequestLine := AcceptorSocket.RecvString(timeout);
  if (AcceptorSocket.LastError = 0) and (RequestLine <> '') then
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

procedure TOpsiHTTPSAcceptingThread.SetStatusCode(AMethod: string);
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

procedure TOpsiHTTPSAcceptingThread.SendStatusLine;
begin
  AcceptorSocket.SendString(StatusLine);
end;

procedure TOpsiHTTPSAcceptingThread.SendHeaders;
var
  i: integer;
begin
  if Protocol <> '' then
  begin
    for i := 0 to Headers.count - 1 do
      AcceptorSocket.SendString(Headers[i] + CRLF);
  end;
end;

procedure TOpsiHTTPSAcceptingThread.SendMessageBody;
begin
  if (AcceptorSocket.LastError = 0) then
  begin
    AcceptorSocket.SendBuffer(OutputBody.Memory, OutputBody.Size);
  end;
end;

procedure TOpsiHTTPSAcceptingThread.WriteStatusLine;
begin
  StatusLine :=  'HTTP/1.1' + ' ' + IntTostr(StatusCode) + ' ' + ReasonPhrase + CRLF;
end;

procedure TOpsiHTTPSAcceptingThread.WriteHeaders;
begin
  if Protocol <> '' then
  begin
    Headers.Clear;
    Headers.Add('Content-type: application/json; charset=UTF-8');
    Headers.Add('Content-length: ' + IntTostr(OutputBody.Size));
    Headers.Add('Connection: close');
    Headers.Add('Date: ' + Rfc822DateTime(now));
    Headers.Add('Server: opsiclientd-light');
    Headers.Add('');
  end;
end;


//procedure TOpsiHTTPSAcceptingThread.WriteMessageBody(aMessageBody: TObject);
//begin
//  if aMessageBody is TStringList then (aMessageBody as TStringList).SaveToStream(OutputBody);
//  if aMessageBody is TOpsiJSONRequest then (aMessageBody as TOpsiJSONRequest).SaveToMemoryStream(OutputBody);
//  if aMessageBody is TOpsiJSONResponse then (aMessageBody as TOpsiJSONResponse).SaveToMemoryStream(OutputBody);
//end;


constructor TOpsiHTTPSAcceptingThread.Create(ASocket: TSocket; AFormerThread: TThread);
begin
  FreeOnTerminate:=true;
  FormerThread := AFormerThread;
  AcceptorSocket:=TTCPBlockSocket.Create;
  AcceptorSocket.Socket:=ASocket;
  InitSSLOpsi;
  //InitSSLCertificate;
  TimeOut := 120000;
  OnDemand := false;
  Headers := TStringList.Create;
  //MessageBody := TOpsiHTTPMessageBody.Create;
  InputBody := TMemoryStream.Create;
  OutputBody := TMemoryStream.Create;
  inherited Create(false);
end;

destructor TOpsiHTTPSAcceptingThread.Destroy;
begin
  FreeAndNil(AcceptorSocket);
  FreeAndNil(Headers);
  //FreeAndNil(MessageBody);
  FreeAndNil(InputBody);
  FreeAndNil(OutputBody);
  FreeAndNil(JSONRequest);
  FreeAndNil(JSONResponse);
  //FreeAndNil(FormerThread);
  inherited Destroy;
  self := nil;
end;

procedure TOpsiHTTPSAcceptingThread.Execute;
var
  s: string;
begin
  try
    if  AcceptorSocket.SSLAcceptConnection
      and (AcceptorSocket.SSL.LastError = 0) then
    begin
      { read request }
      ReadRequestLine;
      ReadHeaders;
      ReadMessageBody;
      SetStatusCode(Method);

      if (JSONRequest.Method = 'fireEvent') then
      begin
        { write response }
        WriteMessageBodyJSONResponse;
        //WriteMessageBodyHTMLTestSide; //for testing with webbrowser
        { WriteMessageBody MUST BE executed before WriteHeaders otherwise the content-lenght is not known (e.g. 0)
        and the Message Body MIGHT BE not read by the client!!!
        Only relvant for plain text if content-encoding is set then NO CONTENT-LENGTH MUST BE SET }
        WriteHeaders;
        WriteStatusLine;
        { send response }
        if AcceptorSocket.lasterror = 0 then
        begin
          SendStatusLine;
          SendHeaders;
          SendMessageBody;
        end;
        //while FormerThread <> nil do;
          //FormerThread.WaitFor;
        if JSONRequest.Params.Find('on_demand') then
        begin
          //RunCommand('/usr/local/bin/opsiscriptstarter',[ ], s , [ ]);
          RunCommand('/Applications/TextEdit.app/Contents/MacOS/TextEdit',[ ], s , [ ]);
        end;
      end
      else ; //SendError (has to be implemented);
    end
    else
    begin
      //StatusMessage := 'Error while accepting SSL connection: ' + AcceptorSocket.SSL.LastErrorDesc;
    end;
  except
    //StatusMessage := 'Exception while accepting SSL connection';
  end;
end;


end.
