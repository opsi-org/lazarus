unit osdcontrolfile_io;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  inifiles,
  oslog,
  osjson,
  //ostoml,
  toml,
  TOMLParser,
  osdbasedata;

procedure readControlFile42(filename: string);
procedure readControlFileToml(filename: string);

implementation

procedure readControlFile42(filename: string);
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
      aktProduct.productdata.packageversion := StrToInt(tmpstr);
    end
    else if section = 'product' then
    begin
      tmpstr := getAndLogValue('type');
      aktProduct.productdata.producttype := tmpstr;
      tmpstr := getAndLogValue('id');
      aktProduct.productdata.productId := tmpstr;
      tmpstr := getAndLogValue('name');
      aktProduct.productdata.productName := tmpstr;
      tmpstr := getAndLogValue('version');
      aktProduct.productdata.productversion := tmpstr;
      tmpstr := getAndLogValue('priority');
      aktProduct.productdata.priority := StrToInt(tmpstr);
      tmpstr := getAndLogValue('licenseRequired');
      aktProduct.productdata.licenserequired := StrToBool(tmpstr);
      tmpstr := getAndLogValue('setupScript');
      aktProduct.productdata.setupscript := tmpstr;
      tmpstr := getAndLogValue('uninstallScript');
      aktProduct.productdata.uninstallscript := tmpstr;
      tmpstr := getAndLogValue('updateScript');
      aktProduct.productdata.updatescript := tmpstr;
      tmpstr := getAndLogValue('alwaysScript');
      aktProduct.productdata.alwaysScript := tmpstr;
      tmpstr := getAndLogValue('onceScript');
      aktProduct.productdata.oncescript := tmpstr;
      tmpstr := getAndLogValue('customScript');
      aktProduct.productdata.customscript := tmpstr;
      tmpstr := getAndLogValue('userLoginScript');
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

procedure readControlFileToml(filename: string);
var
  mytoml: string;
  mytables: TStringList;
  mylist: TStringList;
  i: integer;
  tmpstr: string;
  doc: TTOMLDocument;
  Value: TTOMLData;

  function getAndLogValue(section, key: string): string;
  var
    tmpstr: string;
    tomlkeypath: string;
  begin
    Value := doc[section][key];
    tmpstr := trim(string(Value));
    LogDatei.log('in section: ' + section + ' with key: ' + key +
      ' got: ' + tmpstr, LLdebug);
    Result := tmpstr;
  end;


begin

  logdatei.log('readTomlControlFile from: ' + filename, LLDebug);
  mylist:= TStringList.Create;
  mylist.LoadFromFile(filename);
  doc := GetTOML(mylist.Text);
  (*
  Value := doc['Package']['version'];
  LogDatei.log('in section: package with key: version got: ' + string(value), LLdebug);
  *)
           (*
  mytoml := ReadTOMLFile(filename);
  //mytables := TStringlist.Create;
  *)
  tmpstr := getAndLogValue('Package', 'version');
  aktProduct.productdata.packageversion := StrToInt(tmpstr);
  tmpstr := getAndLogValue('Product', 'type');
  aktProduct.Productdata.Producttype := tmpstr;
  tmpstr := getAndLogValue('Product', 'id');
  aktProduct.Productdata.ProductId := tmpstr;
  tmpstr := getAndLogValue('Product', 'name');
  aktProduct.Productdata.ProductName := tmpstr;
  tmpstr := getAndLogValue('Product', 'version');
  aktProduct.Productdata.Productversion := tmpstr;
  tmpstr := getAndLogValue('Product', 'priority');
  aktProduct.Productdata.priority := StrToInt(tmpstr);
  tmpstr := getAndLogValue('Product', 'licenseRequired');
  aktProduct.Productdata.licenserequired := StrToBool(tmpstr);
  tmpstr := getAndLogValue('Product', 'setupScript');
  aktProduct.Productdata.setupscript := tmpstr;
  tmpstr := getAndLogValue('Product', 'uninstallScript');
  aktProduct.Productdata.uninstallscript := tmpstr;
  tmpstr := getAndLogValue('Product', 'updateScript');
  aktProduct.Productdata.updatescript := tmpstr;
  tmpstr := getAndLogValue('Product', 'alwaysScript');
  aktProduct.Productdata.alwaysScript := tmpstr;
  tmpstr := getAndLogValue('Product', 'onceScript');
  aktProduct.Productdata.oncescript := tmpstr;
  tmpstr := getAndLogValue('Product', 'customScript');
  aktProduct.Productdata.customscript := tmpstr;
  tmpstr := getAndLogValue('Product', 'userLoginScript');
  aktProduct.Productdata.userLoginscript := tmpstr;

  doc.Free;
  (*


  mytables := GetTOMLTableNames(mytoml);
  for i := 0 to mytables.Count - 1 do
  begin
    tmpstr := lowercase(trim(mytables[i]));
    if tmpstr = 'Productdependency' then
    begin
    end;
    if tmpstr = 'Productproperty' then
    begin
    end;
  end;
  *)

end;

end.
