program dbloptin2canias;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  fileinfo,
  winpeimagereader, // {need this for reading exe info}
  elfreader, // {needed for reading ELF executables}
  oslog,
  osnetutil,
  readcsv,
  writeerp,
  datam;

type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

var
  myversion: string;

  { TMyApplication }

  function getDatasetFor(mycustomer: string; mycontact: string;
  var pernum: integer): boolean;
  begin
    Result := False;
    if DataModule1.SQLQuery1.Active then
      DataModule1.SQLQuery1.Close;
    DataModule1.SQLQuery1.SQL.Clear;
    DataModule1.SQLQuery1.SQL.Add(
      'select distinct contactnum , p.customer as ccustno, email , surname, p.mailing, p.pernum,  v.VERIFEDDATE, v.VERIFEDBY, v.ISVERIFED , v.MAILVERIFEDDATE, v.MAILVERIFEDBY, v.ISMAILVERIFED ');
    DataModule1.SQLQuery1.SQL.Add('from IASADRBOOKCONTACT c, iascustper p  ');
    DataModule1.SQLQuery1.SQL.Add(
      'LEFT JOIN IASCUSTPERVERIFED v on ((p.customer= v.customer) and (p.pernum = v.pernum)) ');
    DataModule1.SQLQuery1.SQL.Add(
      'where (Ccompany="UIB") and (c.contactnum=p.persnum) ');
    DataModule1.SQLQuery1.SQL.Add(' and (contactnum = :mycontact)');
    DataModule1.SQLQuery1.SQL.Add(' and (p.customer = :mycustomer)');
    DataModule1.SQLQuery1.ParamByName('mycontact').AsString := mycontact;
    DataModule1.SQLQuery1.ParamByName('mycustomer').AsString := mycustomer;
    DataModule1.SQLQuery1.Open;
    if DataModule1.SQLQuery1.EOF then
    begin
      Result := False;
    end
    else
    begin
      Result := True;
      with  DataModule1.SQLQuery1 do
        LogDatei.log(FieldByName('email').AsString + '  ' +
          FieldByName('contactnum').AsString + '  ' +
          FieldByName('ccustno').AsString + ' sn: ' +
          FieldByName('surname').AsString + ' ml: ' +
          FieldByName('mailing').AsString + ' pn: ' +
          IntToStr(FieldByName('pernum').AsInteger) + ' vd: ' +
          DateToStr(FieldByName('MAILVERIFEDDATE').AsDateTime) + ' vb: ' +
          FieldByName('MAILVERIFEDBY').AsString + ' iv: ' +
          IntToStr(FieldByName('ISMAILVERIFED').AsInteger) + ' bvd: ' +
          DateToStr(FieldByName('VERIFEDDATE').AsDateTime) + ' bvb: ' +
          FieldByName('VERIFEDBY').AsString + ' biv: ' +
          IntToStr(FieldByName('ISVERIFED').AsInteger)
          , LLinfo);
      pernum := DataModule1.SQLQuery1.FieldByName('pernum').AsInteger;
    end;
  end;


  function getVerifiedDatasetFor(mycustomer: string; pernum: integer): boolean;
  begin
    Result := False;
    if DataModule1.SQLQuery1.Active then
      DataModule1.SQLQuery1.Close;
    DataModule1.SQLQuery1.SQL.Clear;
    DataModule1.SQLQuery1.SQL.Add(
      'select distinct customer, pernum, VERIFEDDATE, VERIFEDBY, ISVERIFED , MAILVERIFEDDATE, MAILVERIFEDBY, ISMAILVERIFED ');
    //DataModule1.SQLQuery1.SQL.Add('from IASADRBOOKCONTACT c, iascustper p  ');
    DataModule1.SQLQuery1.SQL.Add('from IASCUSTPERVERIFED  ');
    DataModule1.SQLQuery1.SQL.Add('where (company="UIB") and (client="00") ');
    DataModule1.SQLQuery1.SQL.Add(' and (pernum = :pernum)');
    DataModule1.SQLQuery1.SQL.Add(' and (customer = :mycustomer)');
    DataModule1.SQLQuery1.ParamByName('pernum').AsInteger := pernum;
    DataModule1.SQLQuery1.ParamByName('mycustomer').AsString := mycustomer;
    DataModule1.SQLQuery1.Open;
    if DataModule1.SQLQuery1.EOF then
    begin
      Result := False;
      LogDatei.log('Warning: No Verify dataset for customer: ' + mycustomer, LLWarning);
    end
    else
    begin
      Result := True;
      with  DataModule1.SQLQuery1 do
        LogDatei.log(FieldByName('customer').AsString + ' pn: ' +
          IntToStr(FieldByName('pernum').AsInteger) + ' mvd: ' +
          DateToStr(FieldByName('MAILVERIFEDDATE').AsDateTime) + ' mvb: ' +
          FieldByName('MAILVERIFEDBY').AsString + ' imv: ' +
          IntToStr(FieldByName('ISMAILVERIFED').AsInteger) + ' bvd: ' +
          DateToStr(FieldByName('VERIFEDDATE').AsDateTime) + ' bvb: ' +
          FieldByName('VERIFEDBY').AsString + ' biv: ' +
          IntToStr(FieldByName('ISVERIFED').AsInteger)
          , LLinfo);
    end;
  end;


  function updateDatasetFor(mycustomer: string; pernum: integer;
    mvdate: TDateTime): boolean;
  begin
    try
      Result := False;
      if DataModule1.SQLQuery1.Active then
        DataModule1.SQLQuery1.Close;
      DataModule1.SQLQuery1.SQL.Clear;
      DataModule1.SQLQuery1.SQL.Add(
        'update IASCUSTPERVERIFED set   CHANGEDBY="doubleOptIn", CHANGEDAT=:now, MAILVERIFEDDATE=:mvdate, MAILVERIFEDBY="doubleOptIn", ISMAILVERIFED=1 ');
      DataModule1.SQLQuery1.SQL.Add('where (company="UIB") and (client="00") ');
      DataModule1.SQLQuery1.SQL.Add(' and (pernum = :pernum)');
      DataModule1.SQLQuery1.SQL.Add(' and (customer = :mycustomer)');
      DataModule1.SQLQuery1.ParamByName('mycustomer').AsString := mycustomer;
      DataModule1.SQLQuery1.ParamByName('pernum').AsInteger := pernum;
      DataModule1.SQLQuery1.ParamByName('mvdate').AsDateTime := mvdate;
      DataModule1.SQLQuery1.ParamByName('now').AsDateTime := now;
      LogDatei.log('SQL: ' + DataModule1.SQLQuery1.SQL.Text, LLInfo);
      DataModule1.SQLQuery1.ExecSQL;
      DataModule1.SQLTransaction1.CommitRetaining;
      Result := True;
    except
      on E: Exception do
      begin
        LogDatei.log('Exception in updateDatasetFor', LLcritical);
        LogDatei.log('Error: Message: ' + E.message, LLcritical);
      end;
    end;
  end;

  function insertDatasetFor(mycustomer: string; pernum: integer;
    mvdate: TDateTime): boolean;
  begin
    try
      Result := False;
      if DataModule1.SQLQuery1.Active then
        DataModule1.SQLQuery1.Close;
      DataModule1.SQLQuery1.SQL.Clear;
      DataModule1.SQLQuery1.SQL.Add('insert into IASCUSTPERVERIFED ');
      DataModule1.SQLQuery1.SQL.Add(
        '(client,company,customer,BUSAREA,ADRNUM,ISCUSTORVEND,PERNUM,VERIFEDDATE,VERIFEDBY,ISVERIFED,VERIFEDLASTDATE,MAILVERIFEDDATE,MAILVERIFEDBY,ISMAILVERIFED,CHANGEDBY, CHANGEDAT,CREATEDBY, CREATEDAT) ');
      DataModule1.SQLQuery1.SQL.Add(
        'Values("00","uib",:mycustomer,"*",0,0,:pernum,:mvdate,"doubleOptIn",1,"2030.01.01",:mvdate,"doubleOptIn",1,"doubleOptIn",:now,"doubleOptIn",:now) ');
      DataModule1.SQLQuery1.ParamByName('mycustomer').AsString := mycustomer;
      DataModule1.SQLQuery1.ParamByName('pernum').AsInteger := pernum;
      DataModule1.SQLQuery1.ParamByName('mvdate').AsDateTime := mvdate;
      DataModule1.SQLQuery1.ParamByName('now').AsDateTime := now;
      LogDatei.log('SQL: ' + DataModule1.SQLQuery1.SQL.Text, LLInfo);
      DataModule1.SQLQuery1.ExecSQL;
      DataModule1.SQLTransaction1.CommitRetaining;
      Result := True;
    except
      on E: Exception do
      begin
        LogDatei.log('Exception in insertDatasetFor', LLcritical);
        LogDatei.log('Error: Message: ' + E.message, LLcritical);
      end;
    end;
  end;

function updateMailing(mycustomer: string; pernum: integer): boolean;
begin
  try
    Result := False;
    if DataModule1.SQLQuery1.Active then
      DataModule1.SQLQuery1.Close;
    DataModule1.SQLQuery1.SQL.Clear;
    DataModule1.SQLQuery1.SQL.Add('update IASCUSTPER set CHANGEDBY="doubleOptIn", CHANGEDAT=:now, mailing=0 ');
    DataModule1.SQLQuery1.SQL.Add('where (company="UIB") and (client="00") ');
    DataModule1.SQLQuery1.SQL.Add(' and (pernum = :pernum)');
    DataModule1.SQLQuery1.SQL.Add(' and (customer = :mycustomer)');
    DataModule1.SQLQuery1.ParamByName('mycustomer').AsString := mycustomer;
    DataModule1.SQLQuery1.ParamByName('pernum').AsInteger := pernum;
    //DataModule1.SQLQuery1.ParamByName('mvdate').AsDateTime := mvdate;
    DataModule1.SQLQuery1.ParamByName('now').AsDateTime := now;
    LogDatei.log('SQL: ' + DataModule1.SQLQuery1.SQL.Text, LLInfo);
    DataModule1.SQLQuery1.ExecSQL;
    DataModule1.SQLTransaction1.CommitRetaining;
    Result := True;
  except
    on E: Exception do
    begin
      LogDatei.log('Exception in updateMailing', LLcritical);
      LogDatei.log('Error: Message: ' + E.message, LLcritical);
    end;
  end;
end;


  procedure main;
  var
    i, k: integer;
    optin_date, optin_time: TDateTime;
    optin_date_str, optin_email, mycustomer, mycontact, mysurname: string;
    my2charcountry, teststr: string;
    myint: int64;
    FileVerInfo: TFileVersionInfo;
    csv_dataset_ok: boolean;
    teststr1, teststr2: string;
    pernum: integer;
  begin
    //from http://wiki.freepascal.org/Show_Application_Title,_Version,_and_Company
    FileVerInfo := TFileVersionInfo.Create(nil);
    try
      FileVerInfo.FileName := ParamStr(0);
      FileVerInfo.ReadFileInfo;
      myversion := FileVerInfo.VersionStrings.Values['FileVersion'];
    finally
      FileVerInfo.Free;
    end;
    LogDatei := TLogInfo.Create;
    LogDatei.CreateTheLogfile('dbloptin2canias.log');
    LogDatei.LogLevel := 8;
    logdatei.log(ExtractFileName(ParamStr(0)) + ' Version: ' + myversion, LLessential);
    logdatei.log('Started: ' + DateTimeToStr(now), LLessential);
    apppath := ExtractFilePath(ParamStr(0));
    logdatei.log(apppath, LLdebug);
    readcsv_file;
    linelist := TStringList.Create;
    for i := 0 to inlist.Count - 1 do
    begin
      logdatei.log(inlist.Strings[i], LLdebug);
      linelist.Clear;
      linelist.Delimiter := ';';
      linelist.StrictDelimiter := True;
      linelist.DelimitedText := inlist.Strings[i];
      for k := 0 to linelist.Count - 1 do
        logdatei.log('>> ' + linelist.Strings[k], LLdebug2);
      //for k := 0 to linelist.Count -1 do
      begin
        optin_date_str := '';
        my2charcountry := '';
        optin_email := '';
        mycustomer := '';
        mycontact := '';
        mysurname := '';
        csv_dataset_ok := True;
        // date
        teststr := trim(linelist.Strings[0]);
        teststr1 := copy(teststr, 1, pos(' ', teststr));
        teststr2 := copy(teststr, pos(' ', teststr) + 1, length(teststr));
        if (teststr <> '') then
        begin
          if TryStrToDate(teststr1, optin_date, 'yyyy-mm-dd', '-') then
          begin
            optin_date_str := DateTimeToStr(optin_date);
            logdatei.log('Valid conversion: ' + teststr1 + ' -> ' + optin_date_str, LLdebug2);
            if TryStrToTime(teststr2, optin_time, ':') then
            begin
              optin_date_str := DateTimeToStr(optin_time);
              logdatei.log('Valid conversion: ' + teststr2 + ' -> ' + optin_date_str, LLdebug2);
              optin_date := optin_date + optin_time;
              optin_date_str := DateTimeToStr(optin_date);
              logdatei.log('optin_date_str: ' + optin_date_str, LLdebug2);
            end
            else
            begin
              logdatei.log('Error: Could not convert to time: ' + teststr2, LLError);
              csv_dataset_ok := False;
            end;
          end
          else
          begin
            logdatei.log('Error: Could not convert to date: ' + teststr1, LLError);
            csv_dataset_ok := False;
          end;
        end;
        // 2char country
        teststr := trim(linelist.Strings[1]);
        if (teststr <> '') and (Length(teststr) = 2) then
        begin
          my2charcountry := teststr;
          logdatei.log('my2charcountry: ' + my2charcountry, LLdebug2);
        end
        else
        begin
          logdatei.log('Error: Not a valid 2char country: my2charcountry: ' +
            teststr, LLError);
          csv_dataset_ok := False;
        end;
        // email
        teststr := trim(linelist.Strings[2]);
        if (teststr <> '') and IsValidEmail(teststr) then
        begin
          optin_email := teststr;
          logdatei.log('optin_email: ' + optin_email, LLdebug2);
        end
        else
        begin
          logdatei.log('Error: Not a valid email: optin_email: ' + teststr, LLError);
          csv_dataset_ok := False;
        end;
        // customer
        teststr := trim(linelist.Strings[3]);
        if (teststr <> '') and (TryStrToInt64(teststr, myint) or
          (teststr = 'XXX-SAMMEL-XXX')) then
        begin
          mycustomer := teststr;
          logdatei.log('mycustomer: ' + mycustomer, LLdebug2);
        end
        else
        begin
          logdatei.log('Error: Not a valid customer: ' + teststr, LLError);
          csv_dataset_ok := False;
        end;
        // person
        teststr := trim(linelist.Strings[4]);
        if (teststr <> '') and TryStrToInt64(teststr, myint) then
        begin
          mycontact := teststr;
          logdatei.log('mycontact: ' + mycontact, LLdebug2);
        end
        else
        begin
          logdatei.log('Error: Not a valid mycontact: ' + teststr, LLError);
          csv_dataset_ok := False;
        end;
        // surname
        teststr := trim(linelist.Strings[5]);
        if (teststr <> '') then
        begin
          mysurname := teststr;
          logdatei.log('mysurname: ' + mysurname, LLdebug2);
        end
        else
        begin
          logdatei.log('Error: Not a surname: ' + teststr, LLError);
          //csv_dataset_ok := false;
        end;

        // find db record
        pernum := 0;
        if not csv_dataset_ok then
          logdatei.log('The read dataset has errors: ' + inlist.Strings[i], LLError)
        else
        if getDatasetFor(mycustomer, mycontact, pernum) then
        begin
          logdatei.log('Found dataset for mycontact: ' + mycontact+' with pernum: '+inttostr(pernum), LLNotice);
          if updateMailing(mycustomer, pernum) then
              logdatei.log('update mailing done.', LLInfo)
            else
              logdatei.log('update mailing failed.', LLError);
          if getVerifiedDatasetFor(mycustomer, pernum) then
          begin
            logdatei.log('We do update ...', LLInfo);
            if updateDatasetFor(mycustomer, pernum, optin_date) then
              logdatei.log('update done.', LLInfo)
            else
              logdatei.log('update failed.', LLError);
          end
          else
          begin
            logdatei.log('We do insert ...', LLInfo);
            if insertDatasetFor(mycustomer, pernum, optin_date) then
              logdatei.log('Insert done.', LLInfo)
            else
              logdatei.log('Insert failed.', LLError);
          end;
        end
        else
        begin
          logdatei.log('No dataset for mycontact: ' + mycontact, LLError);
        end;
      end;
    end;
    if Assigned(inlist) then
      inlist.Free;
    if Assigned(linelist) then
      linelist.Free;
    LogDatei.Close;
    LogDatei.Free;
  end;

  procedure TMyApplication.DoRun;
  var
    ErrorMsg: string;
  begin
    // quick check parameters
    ErrorMsg := CheckOptions('h', 'help');
    if ErrorMsg <> '' then
    begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    { add your program here }

    main;

    // stop program loop
    Terminate;
  end;

  constructor TMyApplication.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor TMyApplication.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TMyApplication.WriteHelp;
  begin
    { add your help code here }
    writeln('Usage: ', ExeName, ' -h');
  end;

var
  Application: TMyApplication;

{$R *.res}

begin
  Application := TMyApplication.Create(nil);
  DataModule1 := TDataModule1.Create(nil);
  Application.Run;
  Application.Free;
end.
