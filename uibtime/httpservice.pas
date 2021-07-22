unit httpservice;


//{$mode delphi}{$H+}
{$mode objfpc}{$H+}


interface

uses
  {$IFDEF UNIX}
   cthreads,
   {$ENDIF}
  Classes, SysUtils,
  fphttpapp,
  //fpwebfile,
  HTTPDefs,
  //websession,
  fpHTTP,
  //fpWeb,
  fphttpserver;

const
  MyPort = 4440;

type
  // http://wiki.freepascal.org/Multithreaded_Application_Tutorial/de

  THTTPServerThread = class(TThread)
  private
    FServer: TFPHTTPServer;
    FEvent: string;
    FResponseText : string;
    procedure handleEvent;
  public
    constructor Create(APort: word; const OnRequest: THTTPServerRequestHandler;const OnRequestError: TRequestErrorHandler);
    procedure Execute; override;
    procedure DoTerminate; override;
    property Server: TFPHTTPServer read FServer;
    (*
    procedure DoHandleRequest(Sender: TObject;
       var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
    procedure DoHandleRequestError(Sender : TObject; E : Exception);
    procedure CustomExceptionHandler(Thread: TThread; E: Exception);
    procedure DumpExceptionCallStack;
    *)
  end;


   ThttpDataModule1 = class(TDataModule)
  private
    { private declarations }
  public
    { public declarations }
    procedure DoHandleRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
    procedure DoHandleRequestError(Sender : TObject; E : Exception);
    procedure CustomExceptionHandler(Thread: TThread; E: Exception);
    procedure DumpExceptionCallStack;
  end;

procedure starthttpserver;
procedure stophttpserver;




var
  FServerThread: THTTPServerThread;
  httpDataModule1: ThttpDataModule1;
  globalexception  : Exception;


implementation
uses
  ontop,
  uibdata;

{$R *.lfm}

constructor THTTPServerThread.Create(APort: word;
  const OnRequest: THTTPServerRequestHandler;const OnRequestError: TRequestErrorHandler);
begin
  FServer := TFPHTTPServer.Create(nil);
  //FServer.Threaded:=true;
  FServer.Port := APort;
  Fserver.OnRequestError:=OnRequestError;
  FServer.OnRequest := OnRequest;
  FServer.FreeOnRelease;
  inherited Create(False);
end;

procedure THTTPServerThread.Execute;
begin

  try
    if FServer <> nil then  FServer.Active := True;
  finally
    //if FServer <> nil then  FreeAndNil(FServer);
  end;
end;

procedure THTTPServerThread.DoTerminate;
begin
  //FServer.Active := False;
  //FServer.Destroy;
  //FServer.Free;
  inherited DoTerminate;
  FServer.Active := False;
end;




procedure starthttpserver;
begin
  //MLog.Lines.Add('Starting server');
  FServerThread := THTTPServerThread.Create(MyPort, @httpDataModule1.DoHandleRequest,@httpDataModule1.DoHandleRequestError);
  //FServer := THTTPServerThread.Create(MyPort, @THTTPServerThread.DoHandleRequest,@THTTPServerThread.DoHandleRequestError);
end;

procedure stophttpserver;
var
  handle : TThreadId;
begin
  //MLog.Lines.Add('Stopping server');
  handle := FServerThread.Handle;
  FServerThread.FreeOnTerminate:=true;
  FServerThread.Terminate;
  if 0 <> WaitForThreadTerminate(handle,1000) then
  begin
    KillThread(handle);
  end;
  //FServerThread.FServer.Active:=false;
  //FServerThread.Terminate;
  //FServerThread.WaitFor;
  //FServerThread.DoTerminate;
  //FServerThread.Destroy;
  //if Assigned(FServerThread) then FServerThread.Free;
  //FServer.Free;
end;

procedure THTTPServerThread.handleEvent;
  // Diese Methode wird vom MainThread ausgeführt und kann deshalb auf alle GUI-Elemente zugreifen
var
  validevent: boolean;
  begin
      validevent := DataModule1.isValidEvent(FEvent);

      if (validevent) then
      begin
        fontop.eventhandler(FEvent);
        //FResponseText := FEvent + ' als aktuelles Event gesetzt.';
        FResponseText := '<html><head></head>'
          + '<body bgcolor="lightgreen"><font size="+2" color="green">'+ FEvent+' als aktuelles uibtime Event gesetzt.'
          + '<br/><br/>'
          //+ '<a href="http://erp.uib.local/support/support.php?event='+ FEvent+ '" title="zur&uuml;ck zur SupporterApp (geht auch mit der Browser Zur&uuml;ck-Taste)">zur SupporterApp</a>'
          + '<a href="https://supporterapp.uib.gmbh/?event='+ FEvent+ '" title="zur&uuml;ck zur SupporterApp (geht auch mit der Browser Zur&uuml;ck-Taste)">zur SupporterApp</a>'
          + '</font></body></html>';
        DataModule1.debugOut(5, 'httpservice' ,FEvent + ' selected via httpservice.');
      end
      else
      begin
        //FResponseText := 'Fehler: '+FEvent + ' ist kein gültiges Event.';
        FResponseText := '<html><head></head>'
          + '<body bgcolor="darksalmon"><font size="+4" color="darkred">Fehler: '+ FEvent+' ist kein g&uuml;ltiges uibtime Event!'
          + '<br/><br/>'
          + '<a href="https://supporterapp.uib.gmbh/?event='+ FEvent+ '" title="zur&uuml;ck zur SupporterApp (geht auch mit der Browser Zur&uuml;ck-Taste)">zur SupporterApp</a>'
          + '</font></body></html>';
        DataModule1.debugOut(2, 'httpservice' ,FEvent + ' ist kein gültiges Event.');
      end;
  end;

procedure ThttpDataModule1.DoHandleRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
var
  //EventName: string;
  ResponseText: string;
  //validevent: boolean;
  //str : string;
begin
  try
    //http://localhost:4440/?event=huhu.support
    if ARequest.Host = 'localhost:4440' then
    begin;
      //DataModule1.debugOut(5, 'httpservice' ,'Request accepted from localhost:4440');
      FServerThread.FEvent := ARequest.QueryFields.Values['event'];
      FServerThread.Synchronize(@FServerThread.handleEvent);
      ResponseText := FServerThread.FResponseText;
    end
    else
    begin
      ResponseText := 'Fehler: Request von != localhost:4440 verweigert.';
      //DataModule1.debugOut(2, 'httpservice' ,'Fehler: Request von != localhost:4440 verweigert.');
    end;
    AResponse.ContentType := 'text/html;charset=utf-8';
    AResponse.ProtocolVersion:='Http/1.1';

    //AResponse.Contents.Text := '<html><head></head><body>' + ResponseText + '</body></html>';
    AResponse.Contents.Text :=  ResponseText;
    (*
        '<html><head></head><body>'
        +ResponseText
        + '<br/><br/><a href="http://erp.uib.local/support/support.php?event='
        + FServer.FEvent
        +'"title="zur&uuml;ck zur SupporterApp / '
        + FServer.FEvent
        + ' (geht auch mit der Browser Zur&uuml;ck-Taste)">zur SupporterApp</a> </body></html>';
        *)

  except
     on e: Exception do
    begin
      DataModule1.debugOut(2,'httpservice', 'exception in ThttpDataModule1.DoHandleRequest');
      DataModule1.debugOut(2,'httpservice', e.Message);
      DataModule1.debugOut(2, 'httpservice' ,'Exception:'+FServerThread.FEvent + ' ist kein gültiges Event.');
    end;
  end;
end;

procedure ThttpDataModule1.DoHandleRequestError(Sender : TObject; E : Exception);
begin
  DataModule1.debugOut(2,'httpservice', 'exception in ThttpDataModule1.DoHandleRequest');
  DataModule1.debugOut(2,'httpservice', 'RequestError: '+e.Message);
end;


procedure ThttpDataModule1.DumpExceptionCallStack;
var
  I: Integer;
  Frames: PPointer;
  Report: string;
  E: Exception;
begin
  //http://wiki.freepascal.org/Logging_exceptions
  e:=globalexception;
  Report := 'Program exception! ' + LineEnding +
    'Stacktrace:' + LineEnding + LineEnding;
  if E <> nil then begin
    Report := Report + 'Exception class: ' + E.ClassName + LineEnding +
    'Message: ' + E.Message + LineEnding;
  end;
  Report := Report + BackTraceStrFunc(ExceptAddr);
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    Report := Report + LineEnding + BackTraceStrFunc(Frames[I]);
  datamodule1.debugOut(1,'Exception',Report);
  //ShowMessage(Report);
  //Halt; // End of program execution
end;

procedure ThttpDataModule1.CustomExceptionHandler(Thread: TThread; E: Exception);
begin
  globalexception := E;
  Thread.Synchronize(Thread, @DumpExceptionCallStack);
end;

end.


