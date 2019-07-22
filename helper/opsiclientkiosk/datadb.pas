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
    DataSourceProductData: TDataSource;
    DataSourceProductDependencies: TDataSource;
    SQLite3Connection: TSQLite3Connection;
    SQLQueryProductData: TSQLQuery;
    SQLQueryProductDependencies: TSQLQuery;
    SQLTransaction: TSQLTransaction;
    procedure InitDatabase;
    procedure OpsiProductsToDataset(SQLQuery: TSQLQuery);
    procedure SQLQueryProductDataAfterPost(Dataset: TDataset);
  private
    { private declarations }
    procedure CreateDatabaseTables(Connection: TSQLite3Connection);
  public
    { public declarations }
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
  end;


var
  DataModuleOCK: TDataModuleOCK;

implementation

uses
  opsiclientkioskgui;
{$R *.lfm}

{ TDataModuleOCK }


procedure TDataModuleOCK.SQLQueryProductDataAfterPost(Dataset: TDataset);
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
  //InitDatabase;
  DataSourceProductData.Dataset := SQLQueryProductData;
  //FormopsiClientkiosk.DataSource1.Dataset := SQLQueryProductData;
  DataSourceProductDependencies.Dataset := SQLQueryProductDependencies;
  SQLQueryProductDependencies.DataSource := DataSourceProductData;
  //SQLQueryProductDependencies.DataSource := FormopsiClientkiosk.DataSource1;
end;

destructor TDataModuleOCK.Destroy;
begin
  SQLite3Connection.Close;
  inherited Destroy;
end;

procedure TDataModuleOCK.InitDatabase;
var
  newFile: boolean;
begin
  logdatei.log('startinitdb ', LLInfo);
  SQLite3Connection.Close; // Ensure the connection is closed when we start
  try
    { Since we're making this database for the first time,
     check whether the file already exists }
    SQLite3Connection.DatabaseName := GetTempDir + 'opsikiosk.db';
    logdatei.log('db is : ' + SQLite3Connection.DatabaseName, LLInfo);
    if FileExists(SQLite3Connection.DatabaseName) then
      DeleteFileUTF8(SQLite3Connection.DatabaseName);
    newFile := not FileExists(SQLite3Connection.DatabaseName);
    { Create the database and the tables }
    if newFile then
    begin
      try
        logdatei.log('Creating new database ', LLInfo);
        SQLite3Connection.Open;
        SQLTransaction.StartTransaction;
        CreateDatabaseTables(SQLite3Connection);
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
  { Commit the transactions }
  try
   SQLTransaction.Commit;
  except
    on e: Exception do
    begin
      logdatei.log('Exception commit', LLError);
      logdatei.log('Exception: ' + E.message, LLError);
      logdatei.log('Exception handled at: ' + getCallAddrStr, LLError);
      logdatei.log_exception(E,LLError);
    end;
  end;
  logdatei.log('Finished InitDatabase', LLInfo);
end;

procedure TDataModuleOCK.CreateDatabaseTables(Connection: TSQLite3Connection);
begin
  { Create Table products }
  try
     Connection.ExecuteDirect(
       'CREATE TABLE products ('
       + 'ProductID STRING NOT NULL PRIMARY KEY, '
       + 'ProductName STRING, '
       + 'Description STRING, '
       + 'Advice STRING, '
       + 'ProductVersion STRING, '
       + 'PackageVersion STRING, '
       + 'VersionStr STRING, '
       + 'Priority INTEGER, '
       + 'ProductType STRING, '
       + 'InstallationStatus STRING, '
       + 'InstalledProdVer STRING, '
       + 'InstalledPackVer STRING, '
       + 'InstalledVerStr STRING, '
       + 'ActionRequest STRING, '
       + 'ActionResult STRING, '
       + 'UpdatePossible STRING, '
       + 'hasSetup STRING, '
       + 'hasUninstall STRING, '
       + 'possibleAction STRING'
       + ');'
       );
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
   { Create Table dependencies }
   try
     Connection.ExecuteDirect(
       'CREATE TABLE dependencies ('
       + 'ProductID STRING NOT NULL, '
       + 'RequiredProductID STRING, '
       + 'Required STRING, '
       + 'PreRequired STRING, '
       + 'PostRequired STRING, '
       + 'PRIMARY KEY(ProductID,RequiredProductID)'
       + ');'
       );
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
 end;


procedure TDataModuleOCK.OpsiProductsToDataset(SQLQuery:TSQLQuery);
var
  i: integer;
  Product: TProduct;
  SQLStatment: String;
  //productdatarecord: TProductData;
begin
  //if SQLTransaction.Active then SQLTransaction.Active:=FALSE;
  logdatei.log('starting OpsiProductToDataset ....', LLInfo);
  { product data to database }
  SQLStatment := 'INSERT INTO products VALUES ('
                   + ':ProductID, '
                   + ':ProductName, '
                   + ':Description, '
                   + ':Advice, '
                   + ':ProductVersion, '
                   + ':PackageVersion, '
                   + ':VersionStr, '
                   + ':Priority, '
                   + ':ProductType, '
                   + ':InstallationStatus, '
                   + ':InstalledProdVer, '
                   + ':InstalledPackVer, '
                   + ':InstalledVerStr, '
                   + ':ActionRequest, '
                   + ':ActionResult, '
                   + ':UpdatePossible, '
                   + ':hasSetup, '
                   + ':hasUninstall, '
                   + ':PossibleAction'
                   + ');';

  //SQLStatment := 'SELECT * FROM products ORDER BY UPPER(ProductName)';
  SQLQueryProductData.SQL.Text := SQLStatment;
  SQLTransaction.StartTransaction;
  //SQLQueryProductData.Open;
  //SQLQueryProductData.SQL.Clear;
  //SQLQueryProductData.SQL.Add(SQLStatment);
  for i := 0 to OCKOpsiConnection.LengthOpsiProducts - 1 do
  begin
    Product := OCKOpsiConnection.fetchProduct(i);
    logdatei.log('read: ' + Product.ProductID, LLInfo);
    {SQLQueryProductData.AppendRecord([
     Product.ProductID,
     Product.ProductName,
     Product.Description,
     Product.Advice,
     Product.ProductVersion,
     Product.PackageVersion,
     Product.VersionStr,
     Product.Priority,
     Product.ProductType,
     Product.InstallationStatus,
     Product.InstalledProdVer,
     Product.InstalledPackVer,
     Product.InstalledVerStr,
     Product.ActionRequest,
     Product.ActionResult,
     Product.UpdatePossible,
     Product.hasSetup,
     Product.hasUninstall,
     Product.PossibleAction
     ]);}
    //SQLQueryProductData.ExecSQL;
     //SQLQueryProductData.Close;

    with SQLQueryProductData do
    begin
      ParamByName('ProductID').AsString := Product.ProductID;
      ParamByName('ProductVersion').AsString := Product.ProductVersion;
      ParamByName('PackageVersion').AsString := Product.PackageVersion;
      ParamByName('VersionStr').AsString := Product.VersionStr;
      ParamByName('ProductName').AsString := Product.ProductName;
      ParamByName('Description').AsString := Product.Description;
      ParamByName('Advice').AsString := Product.Advice;
      ParamByName('Priority').AsInteger := Product.Priority;
      ParamByName('ProductType').AsString := Product.ProductType;
      ParamByName('hasSetup').AsString := Product.hasSetup;
      ParamByName('hasUninstall').AsString := Product.hasUninstall;
      ParamByName('InstallationStatus').AsString := Product.InstallationStatus;
      ParamByName('Installedprodver').AsString := Product.InstalledProdVer;
      ParamByName('Installedpackver').AsString := Product.InstalledPackVer;
      ParamByName('Installedverstr').AsString := Product.InstalledVerStr;
      ParamByName('ActionRequest').AsString := Product.ActionRequest;
      ParamByName('ActionResult').AsString := Product.ActionResult;
      ParamByName('UpdatePossible').AsString := Product.UpdatePossible;
      ParamByName('PossibleAction').AsString := Product.PossibleAction;
    end;
    SQLQueryProductData.ExecSQL;

    {with SQLQueryProductData do
    begin
      Append;
      FieldByName('ProductId').AsString := Product.ProductID;
      FieldByName('productVersion').AsString := Product.ProductVersion;
      FieldByName('packageVersion').AsString := Product.PackageVersion;
      FieldByName('versionstr').AsString := Product.VersionStr;
      FieldByName('ProductName').AsString := Product.ProductName;
      FieldByName('description').AsString := Product.Description;
      FieldByName('advice').AsString := Product.Advice;
      FieldByName('priority').AsInteger := Product.Priority;
      FieldByName('producttype').AsString := Product.ProductType;
      FieldByName('hasSetup').AsString := Product.hasSetup;
      FieldByName('hasUninstall').AsString := Product.hasUninstall;
      FieldByName('installationStatus').AsString := Product.InstallationStatus;
      FieldByName('installedprodver').AsString := Product.InstalledProdVer;
      FieldByName('installedpackver').AsString := Product.InstalledPackVer;
      FieldByName('installedverstr').AsString := Product.InstalledVerStr;
      FieldByName('actionrequest').AsString := Product.ActionRequest;
      FieldByName('actionresult').AsString := Product.ActionResult;
      FieldByName('updatePossible').AsString := Product.UpdatePossible;
      FieldByName('possibleAction').AsString := Product.PossibleAction;
      Post;
    end;}
    //Product Dependencies

  end;

  //SQLQueryProductData.ExecSQL;
  SQLQueryProductData.Close;
  SQLTransaction.Commit;
 end;

end.

