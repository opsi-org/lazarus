unit ocsnetboot;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  IdBaseComponent, IdComponent, IdUDPBase,
  //IdUDPServer,
  //IdTrivialFTPServer,
  IdUDPClient,
  IdTrivialFTPBase,
  IdTrivialFTP,
  SysUtils,
  ocsglobal,
  oslog;

procedure doNetboot(mymac : string; mynetbootserver : string);

implementation
var
  myTftpClient : TIdTrivialFTP;

procedure readTftp(filename : string; server : string);
var
  ST: TMemoryStream;
  tftplist : TStringlist;
  appendlist : TStringlist;
  i : integer;
  defaultlabel : string;
  appendline : string;
  appendarray : array of string;
begin
  try
    //writeln('Try to get '+filename+' from '+server);
    LogDatei.log('Try to get '+filename+' from '+server,LLNotice);
    tftplist := TStringlist.Create;
    appendlist := TStringlist.Create;
    myTftpClient.Host:=server;
    ST := TMemoryStream.Create;
    myTftpClient.ReceiveTimeout:=10000;
    myTftpClient.Get(filename, ST);
    if Assigned(ST) then
    begin
      LogDatei.log('Got '+filename+' from '+server,LLNotice);
      //writeln('Filesize=' + IntToStr(ST.Size));
      //st.SaveToFile('tftpfile.txt');
      st.Position:=0;
      tftplist.LoadFromStream(ST);
      FreeAndNil(ST);
      //for i := 0 to tftplist.Count -1 do writeln(tftplist.Strings[i]);
      defaultlabel := tftplist.Strings[0];
      // replace default with label
      defaultlabel := StringReplace(defaultlabel,'default','label',[]);
      i := 1;
      while (i< tftplist.Count) and (appendline = '') do
      begin
        if trim(tftplist.Strings[i]) = defaultlabel then
          appendline := tftplist.Strings[i+2];
        inc(i);
      end;
      //writeln(appendline);
      appendarray := appendline.Split([' ']);
      for i:= low(appendarray) to high(appendarray) do
        appendlist.Add(appendarray[i]);
      //for i := 0 to appendlist.Count -1 do writeln(appendlist.Strings[i]);
      //writeln('--------------------');
      myclientid:=appendlist.Values['hn']+'.'+appendlist.Values['dn'];
      //writeln('myclientid : '+myclientid);
      myservice_url:=appendlist.Values['service'];
      //writeln('myservice_url : '+myservice_url);
      myservice_pass:=appendlist.Values['pckey'];
      //writeln('myservice_pass : '+myservice_pass);
      myservice_user:= myclientid;
      //writeln('myservice_user : '+myservice_user);
      myproduct:=appendlist.Values['product'];
      //writeln('myproduct : '+myproduct);
      LogDatei.log('myservice_url : '+myservice_url,LLInfo);
      LogDatei.log('myservice_pass : '+myservice_pass,LLInfo);
      LogDatei.log('myservice_user : '+myservice_user,LLInfo);
      LogDatei.log('myproduct : '+myproduct,LLInfo);
      LogDatei.log('myclientid : '+myclientid,LLInfo);

    end;
  finally
    tftplist.Free;
    appendlist.Free;
  end;
end;

procedure doNetboot(mymac : string; mynetbootserver : string);
var
  filename :string;
begin
  filename :=  StringReplace(mymac,':','-',[rfReplaceAll]);
  filename := 'linux/pxelinux.cfg/01-'+ filename;
  readTftp(filename, mynetbootserver);
end;

initialization
  myTftpClient := TIdTrivialFTP.create;
  myTftpClient.TransferMode:=tfOctet;

finalization
  myTftpClient.Free;
end.

