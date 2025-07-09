unit osdcontrolfile_io;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  inifiles,
  oslog,
  osjson,
  //ostoml,
  TOML,
  TOMLParser,
  osdbasedata;

type

  TImportContolFilter = class
  private
  public
    packageversion: boolean;
    producttype: boolean;
    productId: boolean;
    productName: boolean;
    productversion: boolean;
    priority: boolean;
    licenserequired: boolean;
    setupscript: boolean;
    uninstallscript: boolean;
    updatescript: boolean;
    alwaysScript: boolean;
    oncescript: boolean;
    customscript: boolean;
    userLoginscript: boolean;
    description: boolean;
    advice: boolean;
    { public declarations }
    constructor Create;
  end;




// parameter filter = true : do not import everything
procedure readControlFile42(filename: string; filter: boolean = False);
procedure readControlFileToml(filename: string; filter: boolean = False);

implementation

var
  import: TImportContolFilter;

constructor TImportContolFilter.Create;
begin
  packageversion := False;
  producttype := True;
  productId := True;
  productName := True;
  productversion := False;
  priority := True;
  licenserequired := True;
  setupscript := False;
  uninstallscript := False;
  updatescript := False;
  alwaysScript := False;
  oncescript := False;
  customscript := False;
  userLoginscript := False;
  description := True;
  advice := True;
end;

procedure readControlFile42(filename: string; filter: boolean = False);
var
  //myfile : TInifile = nil;
  myfile: TextFile;
  mystrings: TStringList = nil;
  tmpstr, tmpline: string;
  lastsection: string = '';
  section: string = '';
  i, starti, endi: integer;
  myprop: TPProperty;
  aktdependency: TPDependency;
  tmpstrlist: TStringList;

  function readSection: string;
  var
    line: string;
    sectionfound: boolean = False;
  begin
    Result := lastsection;
    mystrings.Clear;
    repeat
      readln(myfile, line);
      line := trim(line);
      if (pos('[', line) = 1) and (pos(']', line) = length(line)) then
      begin
        sectionfound := True;
        lastsection := copy(line, 2, length(line) - 2);
      end
      else
        mystrings.Add(line);
    until sectionfound or EOF(myfile);
  end;

  function getAndLogValue(key: string): string;
  var
    tmpstr: string;
  begin
    tmpstr := trim(mystrings.Values[key]);
    LogDatei.log('in section: ' + section + ' with key: ' + key +
      ' got: ' + tmpstr, LLdebug);
    Result := tmpstr;
  end;

begin
  logdatei.log('readPreOpsi4.3ControlFile from: ' + filename, LLDebug);
  AssignFile(myfile, filename);
  Reset(myfile);
  mystrings := TStringList.Create;
  mystrings.NameValueSeparator := ':';

  repeat
    section := lowercase(readSection);
    if section = 'package' then
    begin
      tmpstr := getAndLogValue('version');
      if (not filter) or import.packageversion then
        aktProduct.productdata.packageversion := StrToInt(tmpstr);
    end
    else if section = 'product' then
    begin
      tmpstr := getAndLogValue('type');
      if (not filter) or import.producttype then
        aktProduct.productdata.producttype := tmpstr;
      tmpstr := getAndLogValue('id');
      if (not filter) or import.productId then
        aktProduct.productdata.productId := tmpstr;
      tmpstr := getAndLogValue('name');
      if (not filter) or import.productName then
        aktProduct.productdata.productName := tmpstr;
      tmpstr := getAndLogValue('version');
      if (not filter) or import.productversion then
        aktProduct.productdata.productversion := tmpstr;
      tmpstr := getAndLogValue('priority');
      if (not filter) or import.priority then
        aktProduct.productdata.priority := StrToInt(tmpstr);
      tmpstr := getAndLogValue('licenseRequired');
      if (not filter) or import.packageversion then
        aktProduct.productdata.licenserequired := StrToBool(tmpstr);
      tmpstr := getAndLogValue('setupScript');
      if (not filter) or import.setupscript then
        aktProduct.productdata.setupscript := tmpstr;
      tmpstr := getAndLogValue('uninstallScript');
      if (not filter) or import.uninstallscript then
        aktProduct.productdata.uninstallscript := tmpstr;
      tmpstr := getAndLogValue('updateScript');
      if (not filter) or import.updatescript then
        aktProduct.productdata.updatescript := tmpstr;
      tmpstr := getAndLogValue('alwaysScript');
      if (not filter) or import.alwaysScript then
        aktProduct.productdata.alwaysScript := tmpstr;
      tmpstr := getAndLogValue('onceScript');
      if (not filter) or import.oncescript then
        aktProduct.productdata.oncescript := tmpstr;
      tmpstr := getAndLogValue('customScript');
      if (not filter) or import.customscript then
        aktProduct.productdata.customscript := tmpstr;
      tmpstr := getAndLogValue('userLoginScript');
      if (not filter) or import.userLoginscript then
        aktProduct.productdata.userLoginscript := tmpstr;

      //description
      tmpstr := getAndLogValue('description');
      starti := mystrings.IndexOfName('description');
      endi := mystrings.IndexOfName('advice');
      for i := starti + 1 to endi - 1 do
      begin
        tmpline := mystrings[i];
        tmpstr := tmpstr + LineEnding + tmpline;
      end;
      if (not filter) or import.description then
        aktProduct.productdata.description := tmpstr;

      //advice
      tmpstr := getAndLogValue('advice');
      starti := mystrings.IndexOfName('advice');
      endi := mystrings.IndexOfName('version');
      for i := starti + 1 to endi - 1 do
      begin
        tmpline := mystrings[i];
        tmpstr := tmpstr + LineEnding + tmpline;
      end;
      if (not filter) or import.advice then
        aktProduct.productdata.advice := tmpstr;
    end

    else if section = 'productdependency' then
    begin
      aktdependency := TPDependency(osdbasedata.aktProduct.dependencies.add);
      aktdependency.init;
      // Action request
      tmpstr := getAndLogValue('action');
      aktdependency.action := tmpstr;

      tmpstr := getAndLogValue('requiredProduct');
      aktdependency.Required_ProductId := tmpstr;

      tmpstr := getAndLogValue('requiredStatus');
      case tmpstr of
        '': aktdependency.Required_State := noState;
        'installed': aktdependency.Required_State := installed;
        'not_installed': aktdependency.Required_State := not_installed;
        'unknown': aktdependency.Required_State := unknown;
      end;

      tmpstr := getAndLogValue('requiredAction');
      case tmpstr of
        '': aktdependency.Required_Action := noRequest;
        'setup': aktdependency.Required_Action := setup;
        'uninstall': aktdependency.Required_Action := uninstall;
        'update': aktdependency.Required_Action := TPActionRequest.update;
      end;

      // requirement Type
      tmpstr := getAndLogValue('requirementType');
      case tmpstr of
        '': aktdependency.Required_Type := doNotMatter;
        'before': aktdependency.Required_Type := before;
        'after': aktdependency.Required_Type := after;
      end;
    end

    else if section = 'productproperty' then
    begin
      myprop := TPProperty(osdbasedata.aktProduct.properties.add);
      myprop.init;
      tmpstr := getAndLogValue('name');
      myprop.Property_Name := tmpstr;
      tmpstr := getAndLogValue('description');
      myprop.description := tmpstr;
      tmpstr := getAndLogValue('type');
      if tmpstr = 'bool' then
      begin
        myprop.Property_Type := bool;
        tmpstr := getAndLogValue('default');
        myprop.boolDefault := StrToBool(tmpstr);
        tmpstrlist := TStringList.Create;
        myprop.SetDefaultLines(TStrings(tmpstrlist));
        myprop.SetValueLines(TStrings(tmpstrlist));
        FreeAndNil(tmpstrlist);
      end
      else
      begin
        tmpstrlist := TStringList.Create;
        myprop.Property_Type := unicode;  //type
        tmpstr := getAndLogValue('values');
        osjson.jsonAsArrayToStringList(tmpstr, tmpstrlist);
        myprop.SetValueLines(TStrings(tmpstrlist));

        tmpstr := getAndLogValue('default');
        osjson.jsonAsArrayToStringList(tmpstr, tmpstrlist);
        myprop.SetDefaultLines(TStrings(tmpstrlist));
        FreeAndNil(tmpstrlist);

        tmpstr := getAndLogValue('multivalue');
        myprop.multivalue := StrToBool(tmpstr); //multivalue
        tmpstr := getAndLogValue('editable');
        myprop.editable := StrToBool(tmpstr);  //editable
      end;
    end;
  until EOF(myfile);
  CloseFile(myfile);
  if mystrings <> nil then FreeAndNil(mystrings);
end;

// inspriration from:
// https://github.com/tamtam96/fpTOML/blob/master/tests/Examples.pas
procedure readControlFileToml(filename: string; filter: boolean = False);
var
  mytoml: string;
  mytables: TStringList;
  mylist: TStringList;
  i: integer;
  tmpstr: string;
  doc: TTOMLDocument;
  myprop: TPProperty;
  aktdependency: TPDependency;
  table: TTOMLData;
  tmpstrlist: TStringList;
  tmpint: integer;
  tmpbool: boolean;

  function getAndLogValue(section, key: string): string;
  var
    tmpstr: string;
    Value: TTOMLData;
  begin
    try
      Value := doc[section][key];
      tmpstr := trim(string(Value));
      tmpstr := trim(Value.ToString);
      LogDatei.log('in section: ' + section + ' with key: ' + key +
        ' got: ' + tmpstr, LLdebug);
      Result := tmpstr;
    except
      on E: Exception do
      begin
        logdatei.log('readControlFileToml: Exception: ', LLWarning);
        logdatei.log('While reading: ' + section + ' : ' + key, LLWarning);
        logdatei.log('Exception: ' + E.Message, LLWarning);
        Result := '';
      end;
    end;
  end;

  function getAndLogValueFromTable(indata: TTOMLData; key: string;
  var myvalue: string): boolean;
  var
    tmpstr: string;
    Value: TTOMLData;
    table: TTOMLTable;
    tmpvalue: TTOMLValue;
    tmpstrlist: TStringList;
    i: integer;
  begin
    Result := False;
    myvalue := '';
    table := TTOMLTable(indata);
    if table.Contains(key, TTOMLData) then
    begin
      Value := table.Find(key);
      // is it a scalar data type (String, Number, boolean)
      // boolean is also TTOMLNumber
      if Value.ClassNameIs('TTOMLValue') or Value.ClassNameIs('TTOMLNumber') then
        tmpstr := trim(string(Value))
      else if Value.ClassNameIs('TTOMLArray') then
      begin
        if TTOMLArray(Value).Count > 0 then
        begin
          // is it a single boolean in a array (default at boolean property)
          tmpvalue := TTOMLValue(TTOMLArray(Value)[0]);
          if tmpvalue.TypeString = 'Boolean' then
            tmpstr := string(tmpvalue)
          else
          begin
            tmpstrlist := TStringList.Create;
            for i := 0 to TTOMLArray(Value).Count - 1 do
            begin
              tmpvalue := TTomlvalue(Value.Items[i]);
              if tmpvalue.TypeString = 'Dynamic string' then
              begin
                tmpstr := string(tmpvalue);
                tmpstrlist.Add(tmpstr);
              end;
            end;
            tmpstr := tmpstrlist.Text;
          end;
        end;
      end;
      LogDatei.log('in section: ' + table.Name + ' with key: ' + key +
        ' got: ' + tmpstr, LLdebug);
      myvalue := tmpstr;
      Result := True;
    end
    else
    begin
      LogDatei.log('in section: ' + table.Name + ' key: ' + key +
        ' not found.', LLdebug);
    end;
  end;

begin
  logdatei.log('readTomlControlFile from: ' + filename, LLDebug);
  mylist := TStringList.Create;
  mylist.LoadFromFile(filename);
  //tmpstr := mylist.Text;
  doc := GetTOML(mylist.Text);
  //tmpstr := doc.AsJSON.FormatJSON();
  (*
  Value := doc['Package']['version'];
  LogDatei.log('in section: package with key: version got: ' + string(value), LLdebug);
  *)
           (*
  mytoml := ReadTOMLFile(filename);
  //mytables := TStringlist.Create;
  *)
  tmpstr := getAndLogValue('Package', 'version');
  if (not filter) or import.packageversion then
    aktProduct.productdata.packageversion := StrToInt(tmpstr);
  tmpstr := getAndLogValue('Product', 'type');
  if (not filter) or import.Producttype then
    aktProduct.Productdata.Producttype := tmpstr;
  tmpstr := getAndLogValue('Product', 'id');
  if (not filter) or import.ProductId then
    aktProduct.Productdata.ProductId := tmpstr;
  tmpstr := getAndLogValue('Product', 'name');
  if (not filter) or import.ProductName then
    aktProduct.Productdata.ProductName := tmpstr;
  tmpstr := getAndLogValue('Product', 'version');
  if (not filter) or import.Productversion then
    aktProduct.Productdata.Productversion := tmpstr;
  tmpstr := getAndLogValue('Product', 'priority');
  if (not filter) or import.priority then
    if TryStrToInt(tmpstr, tmpint) then
      aktProduct.Productdata.priority := tmpint;
  tmpstr := getAndLogValue('Product', 'licenseRequired');
  if (not filter) or import.licenserequired then
    if TryStrToBool(tmpstr, tmpbool) then
      aktProduct.Productdata.licenserequired := tmpbool;
  tmpstr := getAndLogValue('Product', 'setupScript');
  if (not filter) or import.setupscript then
    aktProduct.Productdata.setupscript := tmpstr;
  tmpstr := getAndLogValue('Product', 'uninstallScript');
  if (not filter) or import.uninstallscript then
    aktProduct.Productdata.uninstallscript := tmpstr;
  tmpstr := getAndLogValue('Product', 'updateScript');
  if (not filter) or import.updatescript then
    aktProduct.Productdata.updatescript := tmpstr;
  tmpstr := getAndLogValue('Product', 'alwaysScript');
  if (not filter) or import.alwaysScript then
    aktProduct.Productdata.alwaysScript := tmpstr;
  tmpstr := getAndLogValue('Product', 'onceScript');
  if (not filter) or import.oncescript then
    aktProduct.Productdata.oncescript := tmpstr;
  tmpstr := getAndLogValue('Product', 'customScript');
  if (not filter) or import.customscript then
    aktProduct.Productdata.customscript := tmpstr;
  tmpstr := getAndLogValue('Product', 'userLoginScript');
  if (not filter) or import.userLoginscript then
    aktProduct.Productdata.userLoginscript := tmpstr;
  tmpstr := getAndLogValue('Product', 'description');
  if (not filter) or import.description then
    aktProduct.Productdata.description := tmpstr;
  tmpstr := getAndLogValue('Product', 'advice');
  if (not filter) or import.advice then
    aktProduct.Productdata.advice := tmpstr;

  // ProductDependency
  if doc.Contains('ProductDependency') then
    for table in doc['ProductDependency'] do
    begin
      aktdependency := TPDependency(osdbasedata.aktProduct.dependencies.add);
      aktdependency.init;
      if getAndLogValueFromTable(table, 'action', tmpstr) then
        aktdependency.action := tmpstr;
      if getAndLogValueFromTable(table, 'requiredProduct', tmpstr) then
        aktdependency.Required_ProductId := tmpstr;

      if getAndLogValueFromTable(table, 'requiredStatus', tmpstr) then
        case tmpstr of
          '': aktdependency.Required_State := noState;
          'installed': aktdependency.Required_State := installed;
          'not_installed': aktdependency.Required_State := not_installed;
          'unknown': aktdependency.Required_State := unknown;
        end;

      if getAndLogValueFromTable(table, 'requiredAction', tmpstr) then
        case tmpstr of
          '': aktdependency.Required_Action := noRequest;
          'setup': aktdependency.Required_Action := setup;
          'uninstall': aktdependency.Required_Action := uninstall;
          'update': aktdependency.Required_Action := TPActionRequest.update;
        end;

      // requirement Type
      if getAndLogValueFromTable(table, 'requirementType', tmpstr) then
        case tmpstr of
          '': aktdependency.Required_Type := doNotMatter;
          'before': aktdependency.Required_Type := before;
          'after': aktdependency.Required_Type := after;
        end;
    end;

  if doc.Contains('ProductProperty') then
    for table in doc['ProductProperty'] do
    begin
      myprop := TPProperty(osdbasedata.aktProduct.properties.add);
      myprop.init;
      if getAndLogValueFromTable(table, 'name', tmpstr) then
        myprop.Property_Name := tmpstr;
      if getAndLogValueFromTable(table, 'description', tmpstr) then
        myprop.description := tmpstr;

      if getAndLogValueFromTable(table, 'multivalue', tmpstr) then
        if TryStrToBool(tmpstr, tmpbool) then
          myprop.multivalue := tmpbool; //multivalue
      if getAndLogValueFromTable(table, 'editable', tmpstr) then
        if TryStrToBool(tmpstr, tmpbool) then
          myprop.editable := tmpbool;  //editable

      getAndLogValueFromTable(table, 'type', tmpstr);
      if tmpstr = 'bool' then
      begin
        myprop.Property_Type := bool;
        if getAndLogValueFromTable(table, 'default', tmpstr) then
          if TryStrToBool(tmpstr, tmpbool) then
            myprop.boolDefault := tmpbool;
        tmpstrlist := TStringList.Create;
        myprop.SetDefaultLines(TStrings(tmpstrlist));
        myprop.SetValueLines(TStrings(tmpstrlist));
        FreeAndNil(tmpstrlist);
      end
      else
      begin
        tmpstrlist := TStringList.Create;
        myprop.Property_Type := unicode;  //type
        // always call : we need tmpstr to initialize the stringlist
        getAndLogValueFromTable(table, 'values', tmpstr);
        //osjson.jsonAsArrayToStringList(tmpstr, tmpstrlist);
        tmpstrlist.Text := tmpstr;
        myprop.SetValueLines(TStrings(tmpstrlist));

        // always call : we need tmpstr to initialize the stringlist
        getAndLogValueFromTable(table, 'default', tmpstr);
        //osjson.jsonAsArrayToStringList(tmpstr, tmpstrlist);
        tmpstrlist.Text := tmpstr;
        myprop.SetDefaultLines(TStrings(tmpstrlist));
        FreeAndNil(tmpstrlist);
      end;
    end;
  doc.Free;
end;

begin
  import := TImportContolFilter.Create;
end.
