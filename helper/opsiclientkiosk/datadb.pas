unit datadb;

{$mode delphi}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, db, FileUtil,oslog;

type

  { TDataModuleOCK }

  TDataModuleOCK = class(TDataModule)
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    SQLite3Connection1: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
    SQLQuery2: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure SQLQuery1AfterPost(DataSet: TDataSet);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  DataModuleOCK: TDataModuleOCK;

implementation

{$R *.lfm}

{ TDataModuleOCK }


procedure TDataModuleOCK.SQLQuery1AfterPost(DataSet: TDataSet);
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

