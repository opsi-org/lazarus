unit getoptinlinkd1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mysql55conn, mysql80conn, sqldb, sqldblib, db, FileUtil,
  inifiles;

type

  { TDataModule1 }

  TDataModule1 = class(TDataModule)
    DataSource1: TDataSource;
    MySQL80Connection1: TMySQL80Connection;
    SQLDBLibraryLoader1: TSQLDBLibraryLoader;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure DataModuleCreate(Sender: TObject);
  private

  public

  end;

var
  DataModule1: TDataModule1;

implementation

{$R *.lfm}

{ TDataModule1 }

procedure TDataModule1.DataModuleCreate(Sender: TObject);
var
 myini : TIniFile;
 inipath : string;
begin
  inipath := ExtractFileDir(ParamStr(0))+PathDelim+'custom'+PathDelim+'dbconnect.ini';
  myini := TIniFile.Create(inipath);
  MySQL80Connection1.HostName:=myini.ReadString('dbconnect','server','');
  MySQL80Connection1.DatabaseName:=myini.ReadString('dbconnect','database','');
  MySQL80Connection1.UserName:=myini.ReadString('dbconnect','user','');
  MySQL80Connection1.Password:=myini.ReadString('dbconnect','pass','');
  MySQL80Connection1.Open;
  myini.Free;
end;

end.

