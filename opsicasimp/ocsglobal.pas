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



implementation

initialization

myIp := '';
myPort := '';
myexitcode := 0;
myerror := '';
myDepotId := '';
mydebug_level := 8;
mywebdav := False;
myconffile := '';
myclientid :='';
myservice_url :='';
myservice_user :='';
myservice_pass :='';
myversion :='';
mymac :='';
mymode :='';
mynetbootserver := '192.168.1.14';


end.

