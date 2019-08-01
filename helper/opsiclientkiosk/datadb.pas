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
    procedure OpsiProductsToDataset(SQLQuery: TSQLQuery);
    function SetActionRequestToDataset(fSelectedProduct: string; fActionRequest:string): boolean;
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

procedure TDataModuleOCK.LoadTableProductsIntoMemory;
begin
   logdatei.log('Loading TABLE products into memory ordered by upper product name', LLDebug);
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


procedure TDataModuleOCK.OpsiProductsToDataset(SQLQuery:TSQLQuery);
var
  i: integer;
  SQLStatment: String;
  JSONObjectProduct: TJSONObject;
begin
  //if SQLTransaction.Active then SQLTransaction.Active:=FALSE;
  logdatei.log('starting OpsiProductToDataset ....', LLInfo);

  { prepare database }
  SQLStatment := 'SELECT * FROM products'; //ORDER BY UPPER(ProductName)';
  SQLQueryProductData.SQL.Text := SQLStatment;
  SQLTransaction.StartTransaction;
  SQLQueryProductData.Open;

  { JSON to TABLE products }
  for i := 0 to OCKOpsiConnection.JSONObjectProducts.Count - 1 do
  begin
    JSONObjectProduct := TJSONObject(OCKOpsiConnection.JSONObjectProducts.Items[i]);
    logdatei.log('read: ' + JSONObjectProduct.Strings['productId'], LLInfo);
    //with SQLQueryProductData do
    //begin
      SQLQueryProductData.Append;
      if not JSONObjectProduct.Nulls['productId'] then
        SQLQueryProductData['ProductID'] := JSONObjectProduct.Strings['productId'];
      if not JSONObjectProduct.Nulls['productVersion'] then
        SQLQueryProductData['ProductVersion'] := JSONObjectProduct.Strings['productVersion'];
      if not JSONObjectProduct.Nulls['packageVersion'] then
        SQLQueryProductData['PackageVersion'] := JSONObjectProduct.Strings['packageVersion'];
      if not JSONObjectProduct.Nulls['productVersion'] and not JSONObjectProduct.Nulls['packageVersion'] then
        SQLQueryProductData['VersionStr'] := JSONObjectProduct.Strings['productVersion'] + '-'
                                            + JSONObjectProduct.Strings['packageVersion'];
      if not JSONObjectProduct.Nulls['productName'] then
        SQLQueryProductData['ProductName'] := JSONObjectProduct.Strings['productName'];
      if not JSONObjectProduct.Nulls['description'] then
        SQLQueryProductData['Description'] := JSONObjectProduct.Strings['description'];
      if not JSONObjectProduct.Nulls['advice'] then
        SQLQueryProductData['Advice'] := JSONObjectProduct.Strings['advice'];
      if not JSONObjectProduct.Nulls['priority'] then
        SQLQueryProductData['Priority'] := JSONObjectProduct.Integers['priority'];
      if not JSONObjectProduct.Nulls['productType'] then
        SQLQueryProductData['ProductType'] := JSONObjectProduct.Strings['productType'];
      if not JSONObjectProduct.Nulls['hasSetup'] then
        SQLQueryProductData['hasSetup'] := JSONObjectProduct.Booleans['hasSetup'];
      if not JSONObjectProduct.Nulls['hasUninstall'] then
        SQLQueryProductData['hasUninstall'] := JSONObjectProduct.Booleans['hasUninstall'];
      if not JSONObjectProduct.Nulls['actionResult'] then
        if JSONObjectProduct.Strings['installationStatus'] = 'not_installed' then
          SQLQueryProductData['InstallationStatus'] := ''
        else
          SQLQueryProductData['InstallationStatus'] := JSONObjectProduct.Strings['installationStatus'];
       //if JSONObjectProduct.Nulls['installedProdVer'] then ShowMessage('InstalledProdVers : Null');
      if not JSONObjectProduct.Nulls['installedProdVer'] then
        SQLQueryProductData['InstalledProdVer'] := JSONObjectProduct.Strings['installedProdVer'];
      if not JSONObjectProduct.Nulls['installedPackVer'] then
        SQLQueryProductData['InstalledPackVer'] := JSONObjectProduct.Strings['installedPackVer'];
      if not JSONObjectProduct.Nulls['installedVerStr'] then
        SQLQueryProductData['InstalledVerStr'] := JSONObjectProduct.Strings['installedVerStr'];
      if not JSONObjectProduct.Nulls['actionRequest'] then
        if JSONObjectProduct.Strings['actionRequest'] = 'none' then
          SQLQueryProductData['ActionRequest'] := ''
        else
          SQLQueryProductData['ActionRequest'] := JSONObjectProduct.Strings['actionRequest'];
      if not JSONObjectProduct.Nulls['actionResult'] then
        SQLQueryProductData['ActionResult'] := JSONObjectProduct.Strings['actionResult'];
      if not JSONObjectProduct.Nulls['updatePossible'] then
        SQLQueryProductData['UpdatePossible'] := JSONObjectProduct.Booleans['updatePossible'];
      if not JSONObjectProduct.Nulls['possibleAction'] then
        SQLQueryProductData['PossibleAction'] := JSONObjectProduct.Strings['possibleAction'];
    //end;
  end;
  SQLQueryProductData.Close;
  SQLTransaction.Commit;
  logdatei.log('Finished OpsiProductToDataset', LLInfo);
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
      SQLQueryProductData['ActionRequest'] := fActionRequest;
      Post;
    end;
    Open;
  end;
end;

end.

