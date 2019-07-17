unit datadb;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  sqlite3conn,
  sqldb,
  db,
  FileUtil,
  oslog,
  progresswindow,
  lazfileutils;

type

  { TDataModuleOCK }

  TDataModuleOCK = class(TDataModule)
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    SQLite3Connection1: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
    SQLQuery2: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure InitDB;
    procedure SQLQuery1AfterPost(DataSet: TDataSet);
  private
    { private declarations }
  public
    { public declarations }
  end;


var
  DataModuleOCK: TDataModuleOCK;
  //ZMQuerydataset1: TSQLQuery;
  //ZMQuerydataset2: TSQLQuery;

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

procedure TDataModuleOCK.InitDB;
var
  newFile: boolean;
begin
  logdatei.log('startinitdb ', LLInfo);
  with FormProgressWindow do
  begin
    LabelDataload.Caption := 'Create database';
    ProgressBar1.StepIt;
  end;
  SQLite3Connection1.Close; // Ensure the connection is closed when we start
  try
    // Since we're making this database for the first time,
    // check whether the file already exists
    DatamoduleOCK.SQLite3Connection1.DatabaseName := GetTempDir + 'opsikiosk.db';
    logdatei.log('db is : ' + DatamoduleOCK.SQLite3Connection1.DatabaseName, LLInfo);
    if FileExists(DatamoduleOCK.SQLite3Connection1.DatabaseName) then
      DeleteFileUTF8(DatamoduleOCK.SQLite3Connection1.DatabaseName);
    newFile := not FileExists(DatamoduleOCK.SQLite3Connection1.DatabaseName);

    if newFile then
    begin
      // Create the database and the tables
      try
        logdatei.log('Creating new database ', LLInfo);
        SQLite3Connection1.Open;
        SQLTransaction1.Active := True;
        //ZMQuerydataset1 := SQLQuery1;
        //ZMQuerydataset2 := SQLQuery2;

        try
          DatamoduleOCK.SQLite3Connection1.ExecuteDirect(
            'CREATE TABLE products (' + 'ProductId String not null primary key, ' +
            'ProductName String, ' + 'description String, ' +
            'advice String, ' + 'productversion String, ' +
            'packageversion String, ' + 'versionstr String, ' +
            'priority Integer, ' + 'producttype String, ' +
            'installationStatus String, ' + 'installedprodver String, ' +
            'installedpackver String, ' + 'installedverstr String, ' +
            'actionrequest String, ' + 'actionresult String, ' +
            'updatePossible String,' + 'hasSetup String, ' +
            'hasUninstall String, ' + 'possibleAction String);');
          logdatei.log('Finished products ', LLInfo);
        except
          on e: Exception do
          begin
            logdatei.log('Exception CREATE TABLE products', LLError);
            logdatei.log('Exception: ' + E.message, LLError);
            logdatei.log('Exception handled at: ' + getCallAddrStr, LLError);
            logdatei.log_exception(E,LLError);
          end;
        end;


        //Datamodule1.SQLTransaction1.Commit;
        try
          SQLite3Connection1.ExecuteDirect(
            'CREATE TABLE dependencies (ProductId String not null, ' +
            'requiredProductId String, required String, ' +
            'prerequired String, postrequired String, ' +
            'PRIMARY KEY(ProductId,requiredProductId));');

          logdatei.log('Finished dependencies ', LLInfo);
        except
          on e: Exception do
          begin
            logdatei.log('Exception CREATE TABLE dependencies', LLError);
            logdatei.log('Exception: ' + E.message, LLError);
            logdatei.log('Exception handled at: ' + getCallAddrStr, LLError);
            logdatei.log_exception(E,LLError);
          end;
        end;

        try
          SQLTransaction1.Commit;
        except
          on e: Exception do
          begin
            logdatei.log('Exception commit', LLError);
            logdatei.log('Exception: ' + E.message, LLError);
            logdatei.log('Exception handled at: ' + getCallAddrStr, LLError);
            logdatei.log_exception(E,LLError);
          end;
        end;

        if SQLQuery1.Active then
          SQLQuery1.Close;
        SQLQuery1.SQL.Clear;
        SQLQuery1.SQL.Add('select * from products order by Upper(ProductName)');
        SQLQuery1.Open;
        if SQLQuery2.Active then
          SQLQuery2.Close;
        SQLQuery2.SQL.Clear;
        SQLQuery2.SQL.Add('select * from dependencies order by ProductId');
        SQLQuery2.Open;
        logdatei.log('Finished initdb', LLInfo);

        //ShowMessage('Succesfully created database.');
      except
        //ShowMessage('Unable to Create new Database');
        on e: Exception do
        begin
          logdatei.log('Exception Unable to Create new Database', LLError);
          logdatei.log('Exception: ' + E.message, LLError);
          logdatei.log('Exception handled at: ' + getCallAddrStr, LLError);
          logdatei.log_exception(E,LLError);
        end;
      end;
    end;
  except
    //ShowMessage('Unable to check if database file exists');
    on e: Exception do
    begin
      logdatei.log('Exception check if database file exists', LLError);
      logdatei.log('Exception: ' + E.message, LLError);
      logdatei.log('Exception handled at: ' + getCallAddrStr, LLError);
      logdatei.log_exception(E,LLError);
    end;
  end;
  DataSource1.DataSet := SQLQuery1;
  DataSource2.DataSet := SQLQuery2;
  SQLQuery2.DataSource := DataModuleOCK.DataSource1;
  //FormOpsiClientKiosk.DBGrid1.DataSource := DataModuleOCK.DataSource1;
  //FormOpsiClientKiosk.DBGrid2.DataSource := DataModuleOCK.DataSource2;
  //FormOpsiClientKiosk.PanelProductDetail.Height := 0;
  with FormProgressWindow do
  begin
    LabelDataLoad.Caption := 'Initialize Database';
    //LabelDataLoadDetail.Caption := 'initdb';
    //Progressbar1.Position := 15;
    ProgressBar1.StepIt;
    ProgressbarDetail.Position := 0;
    ProcessMess;
  end;
end;

end.

