unit mailsend;

{$mode objfpc}{$H+}

//http://www.freepascal.org/~michael/articles/lazmail/lazmail-en.pdf

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls,
  Graphics, Dialogs, StdCtrls, smtpsend, blcksock, synautil, mimemess, mimepart;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure ShowStatus(Sender: TObject; Reason: THookSocketReason;
      const Value: string);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  MailFrom, MailTo, Subject, SMTPHost: string;
  MailData: TStringList;
  Username, Password: string;
  CurrentSent, sendsize: integer;

implementation

{$R *.lfm}

{ TForm1 }
procedure TForm1.ShowStatus(Sender: TObject; Reason: THookSocketReason;
  const Value: string);
var
  S: string;
  d: double;
begin
  case reason of
    HR_ResolvingBegin:
      memo1.Append('Resolving SMTP server IP');
    HR_ResolvingEnd:
      memo1.Append('End Resolving SMTP server IP');
    HR_SocketCreate:
    begin
      memo1.Append('Socket create');
      //PBStatus.Visible := True;
      //PBStatus.Position := 0;
    end;

    HR_Connect:
      memo1.Append('Connected to SMTP server');
    HR_SocketClose: memo1.Append('socket close');
    //PBStatus.Visible := False;

    HR_WriteCount:
    begin
      CurrentSent := CurrentSent + StrToInt(Value);
      D := CurrentSent / SendSize;
      //PBStatus.Position := Round(D * PBStatus.Max);
      S := Format('%d/%d bytes sent (%5.2f %%)', [CurrentSent, SendSize, D * 100]);
      memo1.Append(S);
    end;
  end;
  Application.ProcessMessages;
end;


procedure TForm1.Button1Click(Sender: TObject);
var
  SMTP: TSMTPSend;
  Mime: TMimeMess;
  P: TMimePart;
  Result: boolean;
begin
  MailData := TStringList.Create;
  MailFrom := 'oertel@groupware.uib.local';
  MailTo := 'oertel@groupware.uib.local';
  Subject := 'uibtime calling .....';
  SMTPHost := '192.168.1.12';
  Username := 'oertel';
  Password := '';
  MailData.Append('Hallo User,');
  MailData.Append('bring Deine Daten in Ordnung.');
  MailData.Append('The uibtime checker is watching you.');

  Result := False;

  try
    Mime := TMimeMess.Create;
    // Set some headers
    Mime.Header.ToList.Text := MailTo;
    Mime.Header.Subject := Subject;
    Mime.Header.From := MailFrom;
    // Create a MultiPart part
    P := Mime.AddPartMultipart('mixed', nil);
    // Add as first part the mail text
    Mime.AddPartText(MailData, P);
    // Add all attachments:
    // Compose message
    Mime.EncodeMessage;
    SMTP := TSMTPSend.Create;
    SMTP.TargetHost := Trim(SMTPHost);
    SMTP.TargetPort := '25';
    SMTP.Username := Username;
    SMTP.Password := Password;
    //SMTP.AutoTLS:=true;
    // Set status callback:
    SMTP.Sock.OnStatus := @ShowStatus;
    CurrentSent := 0;
    SendSize := Length(MailData.Text);
    // Log in to SMTP server
    if SMTP.Login then
    begin
      memo1.Append('login done');
      Application.ProcessMessages;
      // Set sender address and total send size
      if SMTP.MailFrom(GetEmailAddr(MailFrom), SendSize) then
      begin
        memo1.Append('from done');
        Application.ProcessMessages;
        //s := ATo;
        // Add all recipient addresses
        //repeat
        //t:=GetEmailAddr(Trim(FetchEx(s, ’,’, ’"’)));
        //if (t<>’’) then
        Result := SMTP.MailTo(MailTo);
        if Result then
          memo1.Append('to done')
        else
          memo1.Append('from failed');
        Application.ProcessMessages;
        //Break;
        //until s = ’’;
        // Now send e-mail content
        if Result then
          Result := SMTP.MailData(Mime.Lines);
        if Result then
          memo1.Append('data done')
        else
          memo1.Append('data failed');
        Application.ProcessMessages;
      end;
      // And log out...
      SMTP.Logout;
      memo1.Append('logout done');
      Application.ProcessMessages;
    end
    else
    begin
      memo1.Append('login failed');
      memo1.Append(SMTP.ResultString);
      memo1.Append(IntToStr(SMTP.ResultCode));
    end;
    Application.ProcessMessages;
  finally
    SMTP.Free;
    Mime.Free;
  end;
    (*
  if SendTo(MailFrom, MailTo, Subject, SMTPHost, MailData) then
    memo1.Append('yep')
  else
    memo1.Append('nop');
    *)
  MailData.Free;
end;



end.
