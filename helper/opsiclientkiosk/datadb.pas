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
  fpjson,
  jsonParser,
  //progresswindow,
  dialogs,
  lazfileutils;

type

  { TDataModuleOCK }

  TDataModuleOCK = class(TDataModule)
    SQLite3Connection: TSQLite3Connection;
    SQLQueryProductData: TSQLQuery;
    SQLQueryProductDependencies: TSQLQuery;
    SQLTransaction: TSQLTransaction;
    procedure CreateDatabaseAndTables;
    procedure LoadTableProducts;
    procedure SaveTableProducts;
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
  //DataSourceProductData.Dataset := SQLQueryProductData;
  FormOpsiClientKiosk.DataSourceProductData.Dataset := SQLQueryProductData;
  FormOpsiClientKiosk.DataSourceProductDependencies.Dataset := SQLQueryProductDependencies;
  //SQLQueryProductDependencies.DataSource := DataSourceProductData;
  SQLQueryProductDependencies.DataSource := FormOpsiClientKiosk.DataSourceProductData;
end;

destructor TDataModuleOCK.Destroy;
begin
  SQLite3Connection.Close;
  inherited Destroy;
end;

procedure TDataModuleOCK.CreateDatabaseAndTables;
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

procedure TDataModuleOCK.LoadTableProducts;
begin
   SQLQueryProductData.SQL.Text := 'SELECT * FROM products ORDER BY UPPER (ProductName)';
   SQLTransaction.StartTransaction;
   SQLQueryProductData.Open;
   //SQLQueryProductData.First;
end;

procedure TDataModuleOCK.SaveTableProducts;
begin
  DataModuleOCK.SQLQueryProductData.Close;
  DataModuleOCK.SQLTransaction.Commit;
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
  //Product: TProduct;
  SQLStatment: String;
  JSONObjectProduct: TJSONObject;

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

  { JSON to database table products }
  for i := 0 to OCKOpsiConnection.JSONObjectProducts.Count - 2 do
  begin
    JSONObjectProduct := TJSONObject(OCKOpsiConnection.JSONObjectProducts.Items[i]);
    logdatei.log('read: ' + JSONObjectProduct.Strings['productId'], LLInfo);
    with SQLQueryProductData do
    begin
      if not JSONObjectProduct.Nulls['productId'] then
        ParamByName('ProductID').AsString := JSONObjectProduct.Strings['productId'];
      if not JSONObjectProduct.Nulls['productVersion'] then
        ParamByName('ProductVersion').AsString := JSONObjectProduct.Strings['productVersion'];
      if not JSONObjectProduct.Nulls['packageVersion'] then
        ParamByName('PackageVersion').AsString := JSONObjectProduct.Strings['packageVersion'];
      if not JSONObjectProduct.Nulls['productVersion'] and not JSONObjectProduct.Nulls['packageVersion'] then
        ParamByName('VersionStr').AsString := JSONObjectProduct.Strings['productVersion'] + '-'
                                            + JSONObjectProduct.Strings['packageVersion'];
      if not JSONObjectProduct.Nulls['productName'] then
        ParamByName('ProductName').AsString := JSONObjectProduct.Strings['productName'];
      if not JSONObjectProduct.Nulls['description'] then
        ParamByName('Description').AsString := JSONObjectProduct.Strings['description'];
      if not JSONObjectProduct.Nulls['advice'] then
        ParamByName('Advice').AsString := JSONObjectProduct.Strings['advice'];
      if not JSONObjectProduct.Nulls['priority'] then
        ParamByName('Priority').AsInteger := JSONObjectProduct.Integers['priority'];
      if not JSONObjectProduct.Nulls['productType'] then
        ParamByName('ProductType').AsString := JSONObjectProduct.Strings['productType'];
      if not JSONObjectProduct.Nulls['hasSetup'] then
        ParamByName('hasSetup').AsString := JSONObjectProduct.Strings['hasSetup'];
      if not JSONObjectProduct.Nulls['hasUninstall'] then
        ParamByName('hasUninstall').AsString := JSONObjectProduct.Strings['hasUninstall'];
      if not JSONObjectProduct.Nulls['actionResult'] then
        if JSONObjectProduct.Strings['installationStatus'] = 'not_installed' then
          ParamByName('InstallationStatus').AsString := ''
        else
          ParamByName('InstallationStatus').AsString := JSONObjectProduct.Strings['installationStatus'];

      //if JSONObjectProduct.Nulls['installedProdVer'] then ShowMessage('InstalledProdVers : Null');
      if not JSONObjectProduct.Nulls['installedProdVer'] then
        ParamByName('InstalledProdVer').AsString := JSONObjectProduct.Strings['installedProdVer'];
      if not JSONObjectProduct.Nulls['installedPackVer'] then
        ParamByName('InstalledPackVer').AsString := JSONObjectProduct.Strings['installedPackVer'];
      if not JSONObjectProduct.Nulls['installedVerStr'] then
        ParamByName('InstalledVerStr').AsString := JSONObjectProduct.Strings['installedVerStr'];
      if not JSONObjectProduct.Nulls['actionRequest'] then
        if JSONObjectProduct.Strings['actionRequest'] = 'none' then
          ParamByName('ActionRequest').AsString := ''
        else
          ParamByName('ActionRequest').AsString := JSONObjectProduct.Strings['actionRequest'];
      if not JSONObjectProduct.Nulls['actionResult'] then
        ParamByName('ActionResult').AsString := JSONObjectProduct.Strings['actionResult'];
      if not JSONObjectProduct.Nulls['updatePossible'] then
        ParamByName('UpdatePossible').AsString := JSONObjectProduct.Strings['updatePossible'];
      if not JSONObjectProduct.Nulls['possibleAction'] then
        ParamByName('PossibleAction').AsString := JSONObjectProduct.Strings['possibleAction'];
    end;
  SQLQueryProductData.ExecSQL;
  //JSONObjectProduct.Free;
  end;

  //SQLQueryProductData.ExecSQL;
  SQLQueryProductData.Close;
  SQLTransaction.Commit;
  logdatei.log('End of OpsiProductToDataset', LLInfo);
 end;

end.

