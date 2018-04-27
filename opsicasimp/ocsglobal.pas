unit ocsglobal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
var
  myexitcode, mydebug_level: integer;
  myip, myport, myconffile, myclientid, myerror: string;
  myservice_url, myservice_user, myservice_pass: string;
  mywebdav: boolean;
  myversion : string;
  myDepotId: string;
  mymac : string;
  mymode : string;
  mynetbootserver : string;
  myproduct : string;



implementation


  (*
  if HasOption('conffile') then
  begin
    myconffile := trim(GetOptionValue('conffile'));
    if not FileExists(myconffile) then
      myconffile := ExtractFilePath(paramstr(0))+PathDelim+myconffile;
    if not FileExists(myconffile) then
    begin
      myerror := 'Error: Given conffile: '+myconffile+' does not exist.';
      WriteHelp;
    end;
    // try to read it as config.ini
    INI := TINIFile.Create(myconffile);
    myservice_url := trim(INI.ReadString('service','service_url',''));
    myservice_user := trim(INI.ReadString('service','service_user',''));
    myservice_pass := trim(INI.ReadString('service','service_pass',''));
    ini.free;
    if myservice_url = '' then
    begin
       myerror := 'Error: No service_url in conffile: '+myconffile;
       WriteHelp;
    end;
    if myservice_user = '' then
    begin
       myerror := 'Error: No service_user in conffile: '+myconffile;
       WriteHelp;
    end;
    if myservice_pass = '' then
    begin
       myerror := 'Error: No service_pass in conffile: '+myconffile;
       WriteHelp;
    end;
  end
  else
  begin
    myerror := 'Error: No conffile given.';
    WriteHelp;
  end;
  writeln('service_url='+myservice_url);
  writeln('service_user='+myservice_user);
  writeln('service_pass=*******');//+myservice_pass);
  *)


initialization

myIp := '';
myPort := '';
myexitcode := 0;
myerror := '';
myDepotId := '';
mydebug_level := 9;
mywebdav := False;
myconffile := '';
myclientid := 'detlefvm10.uib.local';
myservice_url :='';
myservice_user :='';
myservice_pass :='';
myversion :='';
mymac :='';
mymode :='';
mynetbootserver := '192.168.1.14';


end.

