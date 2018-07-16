program dbloptin2canias;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
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



{ TMyApplication }

function getDatasetFor(mycustomer : string; mycontact : string) : boolean;
var
  myurl, basep, customerp, contactnump, emailp, langp, contactnamep : string;
begin
  result := false;
  if DataModule1.SQLQuery1.Active then DataModule1.SQLQuery1.Close;
  DataModule1.SQLQuery1.SQL.Clear;
  DataModule1.SQLQuery1.SQL.Add('select distinct contactnum , p.customer as ccustno, email , surname  ');
  DataModule1.SQLQuery1.SQL.Add('from IASADRBOOKCONTACT c, iascustper p  ');
  //DataModule1.SQLQuery1.SQL.Add('LEFT JOIN IASCUSTPERVERIFED v on ((p.customer= v.customer) and (p.pernum = v.pernum)) ');
  DataModule1.SQLQuery1.SQL.Add('where (Ccompany="UIB") and (c.contactnum=p.persnum) ');
  DataModule1.SQLQuery1.SQL.Add(' and (contactnum like :mycontact)');
  DataModule1.SQLQuery1.SQL.Add(' and (p.customer like :mycustomer)');
  DataModule1.SQLQuery1.ParamByName('mycontact').AsString:=mycontact;
  DataModule1.SQLQuery1.ParamByName('mycustomer').AsString:=mycustomer;
  DataModule1.SQLQuery1.Open;
  if DataModule1.SQLQuery1.EOF then
  begin
    //edit2.Text := 'Fehler: keinen Datensatz gefunden';
    result := false;
  end
  else
  begin
    result := true;
    (*
    basep := 'https://download.uib.de/verify/newsletter.php?';
    customerp := 'CUSTOMER='+DataModule1.SQLQuery1.FieldByName('ccustno').AsString;
    contactnump := '&CONTACTNUM='+DataModule1.SQLQuery1.FieldByName('contactnum').AsString;
    emailp := '&EMAIL='+DataModule1.SQLQuery1.FieldByName('email').AsString;
    langp := '&LANG=DE';
    contactnamep := '&CONTACTNAME='+DataModule1.SQLQuery1.FieldByName('surname').AsString;
    myurl := basep+customerp+contactnump+emailp+langp+contactnamep;
    edit2.Text:= myurl;
    if customerp = 'CUSTOMER=' then edit2.Text := 'Fehler: keinen CUSTOMER gefunden';
    if contactnump = '&CONTACTNUM=' then edit2.Text := 'Fehler: keinen CONTACTNUM gefunden';
    if emailp = '&EMAIL=' then edit2.Text := 'Fehler: keinen EMAIL gefunden';
    if langp = '&LANG=' then edit2.Text := 'Fehler: keinen LANG gefunden';
    if contactnamep = '&CONTACTNAME=' then edit2.Text := 'Fehler: keinen CONTACTNAME gefunden';
    *)
  end;


end;

procedure main;
var
  i,k : integer;
  optin_date : TDateTime;
  optin_date_str, optin_email, mycustomer, mycontact, mysurname : string;
  my2charcountry, teststr : string;
  myint : int64;
begin
  apppath := ExtractFilePath(ParamStr(0));
  writeln(apppath);
  readcsv_file;
  linelist := TStringlist.Create;
  for i := 0 to inlist.Count -1 do
  begin
    writeln(inlist.Strings[i]);
    linelist.Clear;
    linelist.Delimiter:=';';
    linelist.StrictDelimiter:= true;
    linelist.DelimitedText:= inlist.Strings[i];
    for k := 0 to linelist.Count -1 do writeln('>> '+linelist.Strings[k]);
    //for k := 0 to linelist.Count -1 do
    begin
      optin_date_str :='';
      my2charcountry :='';
      optin_email :='';
      mycustomer :='';
      mycontact :='';
      mysurname :='';
      // date
      teststr := linelist.Strings[0];
      if (teststr <> '') and TryStrToDate(teststr,optin_date) then
        optin_date_str := teststr;
      // 2char country
      teststr := linelist.Strings[1];
      if (teststr <> '') and (Length(teststr) = 2) then
        my2charcountry := teststr;
      // email
      teststr := linelist.Strings[2];
      if (teststr <> '') and IsValidEmail(teststr) then
        optin_email := teststr;
      // customer
      teststr := linelist.Strings[3];
      if (teststr <> '') and TryStrToInt64(teststr, myint) then
        mycustomer := teststr;
      // person
      teststr := linelist.Strings[3];
      if (teststr <> '') and TryStrToInt64(teststr, myint) then
        mycontact := teststr;
      // surname
      teststr := linelist.Strings[4];
      if (teststr <> '')  then
        mysurname := teststr;

      // find db record
      getDatasetFor(mycustomer, mycontact)
    end;
  end;
  if Assigned(inlist) then inlist.Free;
  if Assigned(linelist) then linelist.Free;
end;

procedure TMyApplication.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
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
  StopOnException:=True;
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
  Application:=TMyApplication.Create(nil);
  Application.Run;
  Application.Free;
end.

