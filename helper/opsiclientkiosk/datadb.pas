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
  opsiconnection,
  //progresswindow,
  lazfileutils;

type

  { TDataModuleOCK }

  TDataModuleOCK = class(TDataModule)
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    SQLite3Connection: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
    SQLQuery2: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure InitDB;
    procedure OpsiProductsToDataset(Dataset: TDataset);
    procedure SQLQuery1AfterPost(Dataset: TDataset);
    constructor Create(AOwner: TComponent);override;
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


procedure TDataModuleOCK.SQLQuery1AfterPost(Dataset: TDataset);
begin
  try
  TSQLQuery(Dataset).ApplyUpdates;
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

constructor TDataModuleOCK.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DataSource1.Dataset := SQLQuery1;
  DataSource2.Dataset := SQLQuery2;
  SQLQuery2.DataSource := DataModuleOCK.DataSource1;
end;

procedure TDataModuleOCK.InitDB;
var
  newFile: boolean;
begin
  logdatei.log('startinitdb ', LLInfo);
  SQLite3Connection.Close; // Ensure the connection is closed when we start
  try
    { Since we're making this database for the first time,
     check whether the file already exists }
    DatamoduleOCK.SQLite3Connection.DatabaseName := GetTempDir + 'opsikiosk.db';
    logdatei.log('db is : ' + DatamoduleOCK.SQLite3Connection.DatabaseName, LLInfo);
    if FileExists(DatamoduleOCK.SQLite3Connection.DatabaseName) then
      DeleteFileUTF8(DatamoduleOCK.SQLite3Connection.DatabaseName);
    newFile := not FileExists(DatamoduleOCK.SQLite3Connection.DatabaseName);

    { Create the database and the tables }
    if newFile then
    begin
      try
        logdatei.log('Creating new database ', LLInfo);
        SQLite3Connection.Open;
        SQLTransaction1.StartTransaction;
        try
          DatamoduleOCK.SQLite3Connection.ExecuteDirect(
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
          SQLite3Connection.ExecuteDirect(
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

        {try
          SQLTransaction1.Commit;
        except
          on e: Exception do
          begin
            logdatei.log('Exception commit', LLError);
            logdatei.log('Exception: ' + E.message, LLError);
            logdatei.log('Exception handled at: ' + getCallAddrStr, LLError);
            logdatei.log_exception(E,LLError);
          end;
        end;}


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
  if SQLQuery1.Active then
      SQLQuery1.Close;
  SQLQuery1.SQL.Clear;
  SQLQuery1.SQL.Add('select * from products order by Upper(ProductName)');
  //SQLQuery1.Open;
  if SQLQuery2.Active then
    SQLQuery2.Close;
  SQLQuery2.SQL.Clear;
  SQLQuery2.SQL.Add('select * from dependencies order by ProductId');
  //SQLQuery2.Open;
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
  logdatei.log('Finished initdb', LLInfo);
end;

procedure TDataModuleOCK.OpsiProductsToDataset(Dataset:TDataset);
var
  i: integer;
  Product: TProduct;
  //productdatarecord: TProductData;
begin
    logdatei.log('starting OpsiProductToDataset ....', LLInfo);
  { product data to database }
  Dataset.Open;
  for i := 0 to OCKOpsiConnection.LengthOpsiProducts - 1 do
  begin
    Product := OCKOpsiConnection.fetchProduct(i);
    logdatei.log('read: ' + Product.ProductID, LLInfo);
    Dataset.Append;
    Dataset.FieldByName('ProductId').AsString := Product.ProductID;
    Dataset.FieldByName('productVersion').AsString := Product.ProductVersion;
    Dataset.FieldByName('packageVersion').AsString := Product.PackageVersion;
    Dataset.FieldByName('versionstr').AsString := Product.VersionStr;
    Dataset.FieldByName('ProductName').AsString := Product.ProductName;
    Dataset.FieldByName('description').AsString := Product.Description;
    Dataset.FieldByName('advice').AsString := Product.Advice;
    Dataset.FieldByName('priority').AsString := Product.Priority;
    Dataset.FieldByName('producttype').AsString := Product.ProductType;
    Dataset.FieldByName('hasSetup').AsString := Product.hasSetup;
    Dataset.FieldByName('hasUninstall').AsString := Product.hasUninstall;
    Dataset.FieldByName('installationStatus').AsString := Product.InstallationStatus;
    Dataset.FieldByName('installedprodver').AsString := Product.InstalledProdVer;
    Dataset.FieldByName('installedpackver').AsString := Product.InstalledPackVer;
    Dataset.FieldByName('installedverstr').AsString := Product.InstalledVerStr;
    Dataset.FieldByName('actionrequest').AsString := Product.ActionRequest;
    Dataset.FieldByName('actionresult').AsString := Product.ActionResult;
    Dataset.FieldByName('updatePossible').AsString := Product.UpdatePossible;
    Dataset.FieldByName('possibleAction').AsString := Product.PossibleAction;
    Dataset.Post;

    //Product Dependencies

  end;
  Dataset.Close;
  //Data.Open;
  //Data.First;
end;

end.

