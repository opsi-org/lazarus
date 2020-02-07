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
  variants,
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
    procedure LoadTableProductsIntoMemory;
    procedure RemoveTableProductsFromMemory;
    procedure OpsiProductsToDataset;
    function SetActionRequestToDataset(fSelectedProduct: string; fActionRequest:string): boolean;
    procedure SQLQueryProductDataAfterPost(Dataset: TDataset);
    function GoToProduct(fSelectedProduct:string):boolean;
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
  //TSQLQuery(Dataset).Edit;
  //SQLQueryProductData.Edit
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

function TDataModuleOCK.GoToProduct(fSelectedProduct: string):boolean;
begin
  with SQLQueryProductData do
  begin
    First;
    //gefunden := Locate('ProductID', VarArrayOf([SelectedProduct]),[loCaseInsensitive]);
    Result := Locate('ProductID',VarArrayOf([fSelectedProduct]),[loCaseInsensitive]);
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
  logdatei.log('Started CreateDatabaseAndTables ', LLNotice);
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
        logdatei.log('Creating new database ', LLNotice);
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
  logdatei.log('Finished CeateDatabaseAndTables', LLNotice);
end;

procedure TDataModuleOCK.LoadTableProductsIntoMemory;
begin
   logdatei.log('Loading TABLE products into memory ordered by upper product name', LLInfo);
   SQLQueryProductData.SQL.Text := 'SELECT * FROM products ORDER BY UPPER (ProductName)';
   SQLTransaction.StartTransaction;
   SQLQueryProductData.Open;
   //SQLQueryProductData.First;
end;

procedure TDataModuleOCK.RemoveTableProductsFromMemory;
begin
  logdatei.log('Removing TABLE products from memory', LLDebug);
  DataModuleOCK.SQLQueryProductData.Close;
  DataModuleOCK.SQLTransaction.Commit;
end;

procedure TDataModuleOCK.CreateDatabaseTables(Connection: TSQLite3Connection);
begin
  { CREATE TABLE products }
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
       + 'UpdatePossible BOOLEAN, '
       + 'hasSetup BOOLEAN, '
       + 'hasUninstall BOOLEAN, '
       + 'possibleAction STRING'
       + ');'
       );
     logdatei.log('Success: CREATE TABLE products', LLInfo);
   except
     on e: Exception do
     begin
       logdatei.log('Exception CREATE TABLE products', LLError);
       logdatei.log('Exception: ' + E.message, LLError);
       logdatei.log('Exception handled at: ' + getCallAddrStr, LLError);
       logdatei.log_exception(E,LLError);
     end;
   end;
   { CREATE TABLE dependencies}
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
     logdatei.log('Success: CREATE TABLE dependencies ', LLInfo);
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


procedure TDataModuleOCK.OpsiProductsToDataset;
var
  i: integer;
  SQLStatment: String;
  //JSONObjectProduct: TJSONObject;
begin
  //if SQLTransaction.Active then SQLTransaction.Active:=FALSE;
  logdatei.log('Starting OpsiProductToDataset', LLNotice);

  { prepare database }
  SQLStatment := 'SELECT * FROM products'; //ORDER BY UPPER(ProductName)';
  SQLQueryProductData.SQL.Text := SQLStatment;
  SQLTransaction.StartTransaction;
  SQLQueryProductData.Open;

  { JSON to TABLE products }
  logdatei.log('Insert product into local database:', LLInfo);

  for i := 0 to OCKOpsiConnection.ProductCount - 1 do
  begin
    //JSONObjectProduct := TJSONObject(OCKOpsiConnection.JSONObjectProducts.Items[i]);
    //logdatei.log('read: ' + JSONObjectProduct.Strings['productId'], LLInfo);
    //with SQLQueryProductData do
    //begin
    OCKOpsiConnection.SelectProduct(i);
    SQLQueryProductData.Append;
    //if not JSONObjectProduct.Nulls['productId'] then
    SQLQueryProductData.FieldByName('ProductID').AsString := OCKOpsiConnection.GetProductValueAsString('productId');
    SQLQueryProductData.FieldByName('ProductVersion').AsString := OCKOpsiConnection.GetProductValueAsString('productVersion');
    SQLQueryProductData.FieldByName('PackageVersion').AsString := OCKOpsiConnection.GetProductValueAsString('packageVersion');
    SQLQueryProductData.FieldByName('VersionStr').AsString :=
      OCKOpsiConnection.GetProductValueAsString('productVersion') + '-'
      + OCKOpsiConnection.GetProductValueAsString('packageVersion');
    SQLQueryProductData.FieldByName('ProductName').AsString := OCKOpsiConnection.GetProductValueAsString('productName');
    SQLQueryProductData.FieldByName('Description').AsString := OCKOpsiConnection.GetProductValueAsString('description');
    SQLQueryProductData.FieldByName('Advice').AsString := OCKOpsiConnection.GetProductValueAsString('advice');
    SQLQueryProductData.FieldByName('Priority').AsInteger := OCKOpsiConnection.GetProductValueAsInteger('priority');
    SQLQueryProductData.FieldByName('ProductType').AsString := OCKOpsiConnection.GetProductValueAsString('productType');
    SQLQueryProductData.FieldByName('hasSetup').AsBoolean := OCKOpsiConnection.GetProductValueAsBoolean('hasSetup');
    SQLQueryProductData.FieldByName('hasUninstall').AsBoolean := OCKOpsiConnection.GetProductValueAsBoolean('hasUninstall');
    if OCKOpsiConnection.GetProductValueAsString('installationStatus') = 'not_installed' then
      SQLQueryProductData.FieldByName('InstallationStatus').AsString := ''
    else
      SQLQueryProductData.FieldByName('InstallationStatus').AsString := OCKOpsiConnection.GetProductValueAsString('installationStatus');
    SQLQueryProductData.FieldByName('InstalledProdVer').AsString := OCKOpsiConnection.GetProductValueAsString('installedProdVer');
    SQLQueryProductData.FieldByName('InstalledPackVer').AsString := OCKOpsiConnection.GetProductValueAsString('installedPackVer');
    SQLQueryProductData.FieldByName('InstalledVerStr').AsString := OCKOpsiConnection.GetProductValueAsString('installedVerStr');
    if OCKOpsiConnection.GetProductValueAsString('actionRequest') = 'none' then
      SQLQueryProductData.FieldByName('ActionRequest').AsString := ''
    else
      SQLQueryProductData.FieldByName('ActionRequest').AsString := OCKOpsiConnection.GetProductValueAsString('actionRequest');
    SQLQueryProductData.FieldByName('ActionResult').AsString := OCKOpsiConnection.GetProductValueAsString('actionResult');
    SQLQueryProductData.FieldByName('UpdatePossible').AsBoolean := OCKOpsiConnection.GetProductValueAsBoolean('updatePossible');
    SQLQueryProductData.FieldByName('PossibleAction').AsString := OCKOpsiConnection.GetProductValueAsString('possibleAction');
    logdatei.log(' - ' + OCKOpsiConnection.GetProductValueAsString('productId') , LLInfo);
    //end;
  end;
  SQLQueryProductData.ApplyUpdates;
  SQLQueryProductData.Close;
  SQLTransaction.Commit;
  logdatei.log('Finished OpsiProductToDataset', LLNotice);
 end;

function TDataModuleOCK.SetActionRequestToDataset(fSelectedProduct: string;
  fActionRequest:string): boolean;
begin
  with SQLQueryProductData do
  begin
    First;
    //gefunden := Locate('ProductID', VarArrayOf([SelectedProduct]),[loCaseInsensitive]);
    if Locate('ProductID',VarArrayOf([fSelectedProduct]),[loCaseInsensitive])
    then
    begin
      Edit;
      //FieldByName('ActionRequest').AsString:= fActionRequest;
      SQLQueryProductData.FieldByName('ActionRequest').AsString := fActionRequest;
      Post;
      Result := True;
    end;
    Open;
  end;
end;

end.

