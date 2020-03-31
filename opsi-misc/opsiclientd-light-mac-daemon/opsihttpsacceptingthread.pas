unit OpsiHTTPSAcceptingThread;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Classes, SysUtils, blcksock, sockets, Synautil, ssl_openssl, fpjson,
  jsonparser, OpsiJSONRequest, OpsiJSONResponse, Process,
  OpsiHTMLMessageBody, OPsiClientdLog;

type
  //TPassLog = procedure(AMsg: string; LogLevel:integer) of object;

  { TOpsiHTTPSAcceptingThread }

  TOpsiHTTPSAcceptingThread = class(TThread)
  private
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
    ThreadNumber: integer;
    //MessageBody: TOpsiHTTPMessageBody;
    procedure CreateTestJSONRequestInputBody;// This function is only for testing
    procedure InitSSLCertificate;
    //procedure InitSSLOpsi;
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
    //procedure SetSSLPassword;
    //procedure SetSSLUsername;
  public
    LogData: TLogData;
    AcceptorSocket: TTCPBlockSocket;
    constructor Create(aSocket: TSocket; const aFormerThread: TThread);
    destructor Destroy; override;
    procedure Execute; override;
    //property SSLPassword: string read GetSSLPassword write SetSSLPassword;
    //property SSLUsername: string read GetSSLUsername write SetSSLUsername;
  end;


implementation

{ TOpsiHTTPSAcceptingThread }

procedure TOpsiHTTPSAcceptingThread.WriteMessageBodyHTMLTestSide;
var
  HTML: TOpsiHTMLMessageBody;
begin
  if (StatusCode = 200) then
  begin
    HTML := TOpsiHTMLMessageBody.Create(Uri, AcceptorSocket.SSL.GetPeerName);
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
    JSONResponse := TOPsiJSONResponse.Create(nil, nil, JSONRequest.ID);
    //JSONREsponse.FormatJSON();
    OutputBody.Clear;
    JSONResponse.SaveToMemoryStream(OutputBody);
  end;
end;

//procedure TOpsiHTTPSAcceptingThread.SetSSLPassword(aSSLPassword:string);
//begin
//  if Assigned(AcceptorSocket) then
//    AcceptorSocket.SSL.Password := aSSLPassword;
//end;

//procedure TOpsiHTTPSAcceptingThread.SetSSLUsername(aSSLUsername:string);
//begin
//  if Assigned(AcceptorSocket) then
//    AcceptorSocket.SSL.Username := aSSLUsername;
//end;

//procedure TOpsiHTTPSAcceptingThread.SendLog;
//begin
//  if Assigned(LogData.FPassLog) then
//    LogData.FPassLog(LogData.FLogMessage,LogData.FLevelofLine,LogData.FSourceOfLog);
//end;


procedure TOpsiHTTPSAcceptingThread.InitSSLCertificate;
begin
  // Note: There's no need for installing a client certificate in the
  //       webbrowser. The server asks the webbrowser to send a certificate but
  //       if nothing is installed the software will work because the server
  //       doesn't check to see if a client certificate was supplied. If you
  //       want you can install:

  //       file: c_cacert.p12
  //       password: c_cakey
  (*AcceptorSocket.SSL.CertCAFile :=
    '/etc/opsi-client-agent/' + 's_cabundle.pem';
  AcceptorSocket.SSL.CertificateFile :=
    '/etc/opsi-client-agent/' + 's_cacert.pem';
  AcceptorSocket.SSL.PrivateKeyFile :=
    '/etc/opsi-client-agent/' + 's_cakey.pem';
  AcceptorSocket.SSL.KeyPassword := 's_cakey';
  AcceptorSocket.SSL.verifyCert := True;*)

  AcceptorSocket.SSL.PrivateKeyFile := '/etc/opsi-client-agent/opsiclientd.pem';
  AcceptorSocket.SSL.CertificateFile := '/etc/opsi-client-agent/opsiclientd.pem';
  AcceptorSocket.SSL.verifyCert := True;
end;

//procedure TOpsiHTTPSAcceptingThread.InitSSLOpsi;
//var
//  ClientdConf:TIniFile;
//begin

//  AcceptorSocket.SSL.Username:= SSLUsername;//'vmmacdev1onmm1.uib.local';
//  AcceptorSocket.SSL.Password:= ClientdConf.ReadString('global','opsi_host_key','');//'aead8f8c57a92e14ac820bf8d3df1805'; //'linux123';
//  //AcceptorSocket.SSL.Username:= 'adminuser';//'vmmacdev1onmm1.uib.local';
//  //AcceptorSocket.SSL.Password:= 'linux123';//'aead8f8c57a92e14ac820bf8d3df1805'; //'linux123';
//  FreeAndNil(ClientdConf);
//end;

procedure TOpsiHTTPSAcceptingThread.CreateTestJSONRequestInputBody;
// This function is only for testing
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
    SizeReceived := AcceptorSocket.RecvBufferEx(InputBody.Memory,
      ContentLength, Timeout);
    InputBody.SetSize(SizeReceived);
    //AcceptorSocket.RecvStream(InputBody, TimeOut);
    //rpcMethod := AcceptorSocket.RecvString(TimeOut);
  end;
  JSONRequest := TOpsiJSONRequest.Create(InputBody);

  //CreateTestJSONRequestInputBody;
  //InputBody.Clear;
  //InputBody.LoadFromStream(JSONStream);
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
    for i := 0 to Headers.Count - 1 do
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
  StatusLine := 'HTTP/1.1' + ' ' + IntToStr(StatusCode) + ' ' + ReasonPhrase + CRLF;
end;

procedure TOpsiHTTPSAcceptingThread.WriteHeaders;
begin
  if Protocol <> '' then
  begin
    Headers.Clear;
    Headers.Add('Accept-Encoding: Identity');
    Headers.Add('Content-type: application/json; charset=UTF-8');
    Headers.Add('Content-length: ' + IntToStr(OutputBody.Size));
    //Headers.Add('Connection: close');
    //Headers.Add('Date: ' + Rfc822DateTime(now));
    Headers.Add('User-Agent: opsiclientd-light');
    Headers.Add('');
  end;
end;



constructor TOpsiHTTPSAcceptingThread.Create(ASocket: TSocket;
  const AFormerThread: TThread);
begin
  //FreeOnTerminate:=true;
  FormerThread := AFormerThread;
  AcceptorSocket := TTCPBlockSocket.Create;
  AcceptorSocket.Socket := ASocket;
  LogData := TLogData.Create;
  //InitSSLOpsi;
  InitSSLCertificate;
  TimeOut := 120000;
  OnDemand := False;
  Headers := TStringList.Create;
  //MessageBody := TOpsiHTTPMessageBody.Create;
  InputBody := TMemoryStream.Create;
  OutputBody := TMemoryStream.Create;
  inherited Create(False);
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
  FreeAndNil(LogData);
  FreeAndNil(FormerThread);
  inherited Destroy;
  //self := nil;
end;

procedure TOpsiHTTPSAcceptingThread.Execute;
var
  s: string;
  i: integer;
  mycommandToStart: string;
  //commandparams: TStringlist;
  commandparams: TStringArray;
  runmode: string;
begin
  {$IFDEF DARWIN}
  mycommandToStart := '/usr/local/bin/opsiscriptstarter';
  {$ENDIF DARWIN}
  {$IFDEF LINUX}
  mycommandToStart := '/usr/bin/opsiscriptstarter';
  {$ENDIF LINUX}
  //commandparams := TStringlist.create;
  setlength(commandparams, 0);

  if not Terminated then
  begin
    try
      LogData.FSourceOfLog := 'opsiclientd-light accepting thread';
      LogData.FLogMessage := 'started';
      LogData.FLevelofLine := 6;
      Synchronize(@LogData.SendLog);
      if AcceptorSocket.SSLAcceptConnection and
        (AcceptorSocket.SSL.LastError = 0) then
      begin
        LogData.FSourceOfLog := 'opsiclientd-light accepting thread';
        LogData.FLogMessage := 'SSL accepted';
        LogData.FLevelofLine := 6;
        Synchronize(@LogData.SendLog);
        { read request }
        ReadRequestLine;
        LogData.FSourceOfLog := 'opsiclientd-light accepting thread';
        LogData.FLogMessage := 'ReadRequestLine';
        LogData.FLevelofLine := 7;
        Synchronize(@LogData.SendLog);
        LogData.FLogMessage :=
          'Method: ' + self.Method + ' URI: ' + self.Uri + ' Protocol: ' + self.Protocol;
        LogData.FLevelOfLine := 6;
        Synchronize(@LogData.SendLog);
        ReadHeaders;
        LogData.FSourceOfLog := 'opsiclientd-light accepting thread';
        LogData.FLogMessage := 'ReadHeaders';
        LogData.FLevelofLine := 7;
        Synchronize(@LogData.SendLog);
        for i := 0 to (Headers.Count - 1) do
        begin
          LogData.FLogMessage := Headers[i];
          LogData.FLevelOfLine := 7;
          Synchronize(@LogData.SendLog);
        end;
        ReadMessageBody;
        LogData.FSourceOfLog := 'opsiclientd-light accepting thread';
        LogData.FLogMessage := 'ReadMessageBody';
        LogData.FLevelofLine := 7;
        Synchronize(@LogData.SendLog);
        SetStatusCode(Method);
        LogData.FSourceOfLog := 'opsiclientd-light accepting thread';
        LogData.FLogMessage := 'SetStatusCode';
        LogData.FLevelofLine := 7;
        Synchronize(@LogData.SendLog);
        LogData.FLogMessage := 'got method: ' + JSONRequest.Method;
        LogData.FLevelofLine := 7;
        Synchronize(@LogData.SendLog);

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
          if Assigned(FormerThread) then
          begin
            if not Terminated then
              FormerThread.WaitFor
            else
              FormerThread.Terminate;
            //FormerThread.WaitFor;
            FreeAndNil(FormerThread);
            LogData.FLogMessage :=
              'Former Thread terminated and freed (OpsiHTTPSAcceptingThread.pas)';
            LogData.FLevelOfLine := 5;
            Synchronize(@LogData.SendLog);
          end;
          if JSONRequest.Params.Find('on_demand') and (not Terminated) then
          begin
            LogData.FLogMessage := 'got event: on_demand';
            LogData.FLevelofLine := 5;
            Synchronize(@LogData.SendLog);
            runmode := 'nogui';
            {$IFDEF DARWIN}
            {check if somebody is logged in to the GUI (root=no)}
            if RunCommand('/bin/bash', ['-c', 'stat -f "%Su" /dev/console'], s) then
              //stat -f "%Su" /dev/console
            begin
              LogData.FLogMessage := 'User logged in: ' + s;
              LogData.FLevelofLine := 5;
              Synchronize(@LogData.SendLog);
              if trim(s) = 'root' then  //maybe must be adapted
              begin
                {Nobody is logged in to the GUI so we start in the nogui mode}
                runmode := 'nogui';
              end
              else
                runmode := 'gui';
            end
            else
            begin
              LogData.FLogMessage := 'could not determine loggedin user';
              LogData.FLevelofLine := 3;
              Synchronize(@LogData.SendLog);
            end;
            {$ENDIF DARWIN}
            {$IFDEF LINUX}
            runmode := 'nogui';
            {$ENDIF LINUX}
            if runmode = 'nogui' then
            begin
              LogData.FLogMessage := 'Starting : opsiscriptstarter --nogui';
              LogData.FLevelofLine := 5;
              Synchronize(@LogData.SendLog);
              setlength(commandparams, 1);
              commandparams[0] := '--nogui';
              //commandparams.Add('--nogui');
              if not RunCommand(mycommandToStart, commandparams, s, []) then
              begin
                LogData.FLogMessage :=
                  'Error: Starting : "opsiscriptstarter --nogui" failed';
                LogData.FLevelofLine := 1;
                Synchronize(@LogData.SendLog);
              end
              else
              begin
                LogData.FLogMessage := 'Finished : opsiscriptstarter --nogui';
                LogData.FLevelofLine := 5;
                Synchronize(@LogData.SendLog);
              end;
            end
            else
            begin
              {Somebody is logged in to the GUI so we start in the gui mode}
              LogData.FLogMessage := 'Starting : opsiscriptstarter';
              LogData.FLevelofLine := 5;
              Synchronize(@LogData.SendLog);
              setlength(commandparams, 0);
              if not RunCommand(mycommandToStart, commandparams, s, []) then
              begin
                LogData.FLogMessage := 'Error: Starting : "opsiscriptstarter" failed';
                LogData.FLevelofLine := 1;
                Synchronize(@LogData.SendLog);
              end
              else
              begin
                LogData.FLogMessage := 'Finished : opsiscriptstarter';
                LogData.FLevelofLine := 5;
                Synchronize(@LogData.SendLog);
              end;
            end;
          end;
        end
        else
        begin
          LogData.FLogMessage := 'found method <> fireEvent: ' + JSONRequest.Method;
          LogData.FLevelofLine := 4;
          Synchronize(@LogData.SendLog);
        end;
      end
      else
      begin
        LogData.FLogMessage :=
          'Error while accepting SSL connection: ' + AcceptorSocket.SSL.LastErrorDesc;
        LogData.FLevelofLine := 2;
        Synchronize(@LogData.SendLog);
      end;
    except
      on E: Exception do
      begin
        LogData.FLogMessage := 'Exception while processing request: ' + E.Message;
        LogData.FLevelofLine := 1;
        Synchronize(@LogData.SendLog);
      end;
    end;
  end;
end;


end.
