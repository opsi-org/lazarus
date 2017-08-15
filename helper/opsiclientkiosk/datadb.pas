unit datadb;

{$mode delphi}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, db, FileUtil,oslog;

type

  { TDataModule1 }

  TDataModule1 = class(TDataModule)
    SQLite3Connection1: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
    SQLQuery2: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure DataModuleCreate(Sender: TObject);
    procedure SQLQuery1AfterPost(DataSet: TDataSet);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  DataModule1: TDataModule1;

implementation

{$R *.lfm}

{ TDataModule1 }

procedure TDataModule1.DataModuleCreate(Sender: TObject);
begin
   // Datamodule1 := Tdatamodule1.create(self);
end;

procedure TDataModule1.SQLQuery1AfterPost(DataSet: TDataSet);
begin
  try
  TsqlQuery(dataset).ApplyUpdates;
  //TsqlQuery(dataset).Refresh;
   except
    on e: Exception do
    begin
      logdatei.log('Exception SQLQuery1AfterPost', LLError);
      logdatei.log('Exception: ' + E.message, LLError);
      logdatei.log('Exception handled at: ' + getCallAddrStr, LLError);
      logdatei.log_exception(E,LLError);
    end;
  end;
end;

end.

