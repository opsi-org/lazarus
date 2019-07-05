program ssltest;

{$mode objfpc}{$H+}



uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  ssl_openssl_lib, oswebservice,
  httpsend,
  ssl_openssl,
  Classes,
  SysUtils,
  fphttpclient,
  fpopenssl,
  openssl,
  oslog,
  fileutil,
  {$IFDEF UNIX}
  baseunix,
  {$ENDIF UNIX}
  fpjson { you can add units after this };



procedure closelog;
begin
  LogDatei.log('Close the log', LLNotice);
  LogDatei.Close;
end;

procedure initlog;
var
  lfilename: string;
begin
  // Initialize logging
  // create the object
  LogDatei := TLogInfo.Create;
  // we do not need part logs
  //LogDatei.WritePartLog := False;
  // set the loglevel
  LogDatei.LogLevel := 8;
  // get the name for the log file
  lfilename := ExtractFileName(ParamStr(0));
  lfilename := ExtractFileNameWithoutExt(lfilename);
  // set the name for the log file
  // yes that is confusing and has to be cleaned up
  LogDatei.FileName := lfilename;
  LogDatei.StandardLogFileext := '.log';
  LogDatei.StandardLogFilename := lfilename;
  LogDatei.CreateTheLogfile(lfilename + '.log', True);
  // now we can start with the inital logentry
  LogDatei.log('Log for: ' + paramstr(0) + ' opend at : ' +
    DateTimeToStr(now), LLNotice);
  // now you can use logdatei everywhere if oslog is in the uses list
  // do not forget to close the log at the end of the program

  // The following part is not needed and only for your information:

  // some pre compiler definitions that influence the logpath
  // have a look at the end of the oslog.pas for mor information
  // have a look at project settings / compiler settings / custom settings
  // to create cutom defines ( WINDOWS or LINUX are defined automatically)
  {$IFDEF WINDOWS}
  LogDatei.log('WINDOWS is defined', LLNotice);
  {$IFDEF OPSI}
  LogDatei.log('OPSI is defined', LLNotice);
  {$IFDEF OPSI_AS_USER}
  LogDatei.log('OPSI_AS_USER is defined', LLNotice);
  {$ENDIF OPSI_AS_USER}
  {$ELSE OPSI}
  LogDatei.log('OPSI is not defined', LLNotice);
  {$ENDIF OPSI}
  {$ELSE WINDOWS}
  LogDatei.log('WINDOWS is not defined', LLNotice);
  if 0 = fpGetEUid then
  begin
    {$IFDEF OPSI}
    LogDatei.log('OPSI is defined and we running as root', LLNotice);
    {$ELSE OPSI}
    LogDatei.log('OPSI is not defined and we running as root', LLNotice);
    {$ENDIF OPSI}
  end
  else
  begin
    LogDatei.log('We are not running as root', LLNotice);
  end;
  {$ENDIF}
  LogDatei.log('Log path is: ' + LogDatei.FileName, LLNotice);
  LogDatei.LogLevel := 8;

end;


  function DownloadHTTP(URL, TargetFile: string): boolean;
  var
    HTTPGetResult: boolean;
    HTTPSender: THTTPSend;
  begin
    Result := False;
    HTTPSender := THTTPSend.Create;
    try
      HTTPGetResult := HTTPSender.HTTPMethod('GET', URL);
      if (HTTPSender.ResultCode >= 100) and (HTTPSender.ResultCode <= 299) then
      begin
        HTTPSender.Document.SaveToFile(TargetFile);
        Result := True;
      end;
    finally
      HTTPSender.Free;
    end;
  end;

  function DownloadHTTPS(URL: string; AStrings: TStrings; out AErrMsg: string): boolean;
    //http://www.lazarusforum.de/viewtopic.php?f=26&t=10941&p=96987&hilit=synapse+ssl#p96987
  var
    ok: boolean;
    realURL: string;
    i, p: integer;
    s: string;
    HTTPSender: THTTPSend;
  begin
    Result := False;
    AErrMsg := '';
    HTTPSender := THTTPSend.Create;
    //https://stackoverflow.com/questions/1857871/how-to-create-https-client-with-synapse
    HTTPSender.Sock.CreateWithSSL(TSSLOpenSSL);
    //HTTPSender.Sock.Create;
    if HTTPSender.Sock.SSL.Accept then writeln('ready to accept')
    else
      begin
        writeln('not !! ready to accept');
        //writeln(ssl_openssl_lib.s ssl_openssl_lib.ErrGetError);
      end;
    if ssl_openssl_lib.InitSSLInterface then
    begin
      writeln('after init, true: '+BoolToStr(ssl_openssl_lib.IsSSLloaded));
    end;
    writeln('after init: '+BoolToStr(ssl_openssl_lib.IsSSLloaded));
    writeln('Lib should be: '+ssl_openssl_lib.DLLSSLName);
    HTTPSender.Sock.SSLDoConnect;
    writeln('SLLVersion : ' + HTTPSender.Sock.SSL.GetSSLVersion);
    if HTTPSender.Sock.SSL.LibName = 'ssl_none' then
    begin
      // no SSL available, loading libs failed
      writeln('no SSL available, loading libs failed');
    end
    else writeln('Loaded: '+ HTTPSender.Sock.SSL.LibName);
    writeln('Version: '+HTTPSender.Sock.SSL.LibVersion);
    //HTTPSender.Sock.SSL.

    with HTTPSender do
      try
        ok := HTTPMethod('GET', URL);
        if ok then
        begin
          // If its a 301 or 302 we need to do more processing
          if (ResultCode = 301) or (ResultCode = 302) then
          begin
            realURL := '';
            // Check the headers for the Location header
            for i := 0 to Headers.Count - 1 do
            begin
              // Extract the URL
              s := Headers[i];
              p := pos('location', Lowercase(s));
              if p = 1 then
              begin
                p := Length('location') + 1;
                while (p < Length(s)) and (s[p] in [':', ' ', '"']) do
                  Inc(p);
                realURL := copy(s, p, MaxInt);
                while (realURL[Length(realURL)] in ['"', '/']) do
                  Delete(realURL, Length(realURL), 1);
                break;
              end;
            end;
            if realURL <> '' then
            begin
              writeln('Redirecting to ' + realURL);
              // If we have a URL, run it through the same function
              Result := DownloadHTTPS(realURL, AStrings, AErrMsg);
            end
            else
              AErrMsg := 'Redirection error';
          end
          else
          begin
            AStrings.LoadFromStream(Document);
            Result := True;
          end;
        end
        else
          AErrMsg := 'Error code ' + IntToStr(ResultCode);
      finally
        Free;
      end;
  end;

  function GetJSON(const AURL: string; out AJSON: TJSONStringType): boolean;
  var
    Ms: TMemoryStream;
    HTTPClient: TFPHTTPClient;
  begin
    Result := False;
    Ms := TMemoryStream.Create;
    try
      HTTPClient := TFPHTTPClient.Create(nil);
      try
        HTTPClient.AllowRedirect := True;
        HTTPClient.AddHeader('User-Agent', 'Mozilla/5.0 (compatible; fpweb)');
        HTTPClient.AddHeader('Content-Type', 'application/json');
        //HTTPClient.HTTPMethod('GET', AURL, MS, []);
        HTTPClient.SimpleGet(AURL);
        writeln('Result: '+inttostr(HTTPClient.ResponseStatusCode));
        if HTTPClient.ResponseStatusCode = 200 then
        begin
          (*
          if Ms.Size > 0 then
          begin
            MS.Position := 0;
            SetLength(AJSON, MS.Size);
            MS.Read(Pointer(AJSON)^, Length(AJSON));
            Result := Length(AJSON) > 0;

          end;
          *)
        end
        else writeln('Result: '+inttostr(HTTPClient.ResponseStatusCode))
      except
        on E: Exception do
        begin
          writeln(e.message) ;
          Result := False;
        end;
      end;
    finally
      Ms.Free;
    end;
  end;


var
  t: TStringList;
  URL, errmsg: string;
  JSON: TJSonStringType;
  session, result : string;
begin
  {try
    DownloadHTTP('http://aktiv-fuer-fluechtlinge-rlp.de/kontakt/', 'http-test.html');
  except
    writeln('Fehler');
  end;
  }
  {
  t := TStringList.Create;
  //URL:='https://83.246.66.165:443';
  //URL := 'https://ini-migration.de/';
  URL:='https://192.168.0.3:4447';
  try
    writeln('Lese Quelltext von "' + URL + '" ein:');
    if DownloadHTTPS(URL, t, errmsg) then
    begin
      t.SaveToFile('https-test.html');
    end
    else
      writeln('--> Fehler: ' + errmsg);
  finally
    t.Free;
  end;
  }

  initlog;
  result := getOpsiServerVersion('https://192.168.1.14:4447/rpc','adminuser','linux123',session);
  writeln('result: '+result);
  closelog;

end.
