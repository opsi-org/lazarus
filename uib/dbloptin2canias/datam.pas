unit datam;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mysql55conn, sqldb, sqldblib, db, FileUtil,
  inifiles;

type

  { TDataModule1 }

  TDataModule1 = class(TDataModule)
    DataSource1: TDataSource;
    MySQL55Connection1: TMySQL55Connection;
    SQLDBLibraryLoader1: TSQLDBLibraryLoader;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure DataModuleCreate(Sender: TObject);
  private

  public

  end;

var
  DataModule1: TDataModule1;
  inlist : TStringlist;
  linelist : TStringlist;
  Apppath : string;

implementation

{$R *.lfm}

{ TDataModule1 }
(*
use uibprod;
grant select, update, delete, insert on IASCUSTPERVERIFED to "dbloptin-writer";
grant select, update on IASCUSTPER to "dbloptin-writer";
grant select on IASADRBOOKCONTACT to "dbloptin-writer";
grant select on IASADRBOOKCONTACT to "dbloptin-writer";
*)

procedure TDataModule1.DataModuleCreate(Sender: TObject);
var
 myini : TIniFile;
 inipath : string;
begin
  inipath := ExtractFileDir(ParamStr(0))+PathDelim+'custom'+PathDelim+'dbconnect.ini';
  myini := TIniFile.Create(inipath);
  MySQL55Connection1.HostName:=myini.ReadString('dbconnect','server','');
  MySQL55Connection1.DatabaseName:=myini.ReadString('dbconnect','database','');
  MySQL55Connection1.UserName:=myini.ReadString('dbconnect','user','');
  MySQL55Connection1.Password:=myini.ReadString('dbconnect','pass','');
  MySQL55Connection1.Open;
  myini.Free;
end;

end.

