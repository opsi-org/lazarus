program opsiTOML;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  SysUtils,
  TOML, TOMLParser, TOMLfunc, osencoding,
  FGL, FPJSON, fpJSONrtti;

Type
  TA = Array[0..9] of Integer;

var
  filePath, newFilePath, txtFile, JSONpath: String;

  myTOMLString : String;
  myNewTOMLString : ansistring;

  myTOML : TTOMLDocument;
  myJSON : TJSONData;
  myJSONStreamer: TJSONStreamer;
  myStream : TMemoryStream;
  myJSONString : String;
  myStrings : TStringList;

  myKey : String = '';
  myValue : String;
  myData : TTOMLData;
  myArray : TTOMLArray;
  (*
  myOctal : Number = 0o01234567;
  myHexadecimal : Hexadecimal = 0xDEADBEEF;
  myBinary : Binary = 0b11010110  ;
  myDateTime : TDateTime =  1979-05-27 ;
  *)

  myNewTOML : TTOMLDocument;

  i : integer = 0;
  tableNamesList : TStringList;
  myTOMLStringList : TStringList;

  myTOMLTable : TTOMLTable;
  newTOMLTable : TTOMLTable;

  nb : integer;

begin
  writeln('--- Begin');

  filePath := '/home/jinene/gitwork/lazarus/helper/opsi-TOML/tests/TOMLexample.toml';
  newFilePath := '/home/jinene/gitwork/lazarus/helper/opsi-TOML/tests/TOMLempty.toml';
  JSONpath := '/home/jinene/gitwork/lazarus/helper/opsi-TOML/tests/myJSONfromTOMLdata.json' ;
  txtFile := '/home/jinene/gitwork/lazarus/helper/opsi-TOML/tests/TXTempty.txt' ;

  writeln('--- Testing LoadTOMLFile :  ');
  myTOMLStringList := TStringList.Create;
  myTOMLStringList.AddStrings(LoadTOMLFile(filePath));
  //writeln(myTOMLStringList.Text);

  writeln('--- Testing ReadTOMLFile :');
  myTOMLString := ReadTOMLFile(filePath);

  writeln('--- Testing GetTOMLDocument ');
  myTOML := GetTOMLDocument(filePath) ;

  //writeln('--- Testing GetTOML :');
  //myTOML := GetTOML(myTOMLString);

  writeln('--- Testing TTOMLTable.AsTOMLStringList : ');
  myTOMLStringList := myTOML.AsTOMLStringList;
  writeln(myTOMLStringList.Text);

  writeln('--- Testing TTOMLTable.AsTOMLString : ');
  myNewTOMLString := myTOML.AsTOMLString;
  writeln(myNewTOMLString);

  //writeln('--- Testing TOML.AsJSON :');
  //writeln(myTOML.AsJSON.FormatJSON);

  writeln('--- Testing SaveToTOMLFile (from String to File)') ;
  if ( SaveToTOMLFile(myNewTOMLString, txtFile) ) then
     writeln('- SaveToTOMLFile with String parameter done')
  else
      writeln('- SaveToTOMLFile with String parameter failed');

  writeln('--- Testing SaveToTOMLFile (from TTOMLDocument to File)') ;
  if ( SaveToTOMLFile(myTOML, newFilePath) ) then
     writeln('- SaveToTOMLFile with TTOMLDocument parameter done')
  else
      writeln('- SaveToTOMLFile with TTOMLDocument parameter failed');

  writeln('--- Testing ConvertTOMLfiletoJSONfile ') ;
  if ( ConvertTOMLfiletoJSONfile(filePath, JSONpath)) then
     writeln('--- ConvertTOMLfiletoJSONfile done')
  else
      writeln('--- ConvertTOMLfiletoJSONfile failed');
  
  writeln('--- Testing HasTables : ');
  nb := HasTables(myTOML);
  writeln('myTOML has :', nb, ' tables');

  writeln('--- Testing GetTOMLTableNames with a myTOMLString parameter:  ');
  tableNamesList := GetTOMLTableNames(myTOMLString);
  writeln('The TOMLTable [servers] has sub-tables :');
  writeln(tableNamesList.Text);

  writeln('--- Testing GetTOMLTableNames with a file parameter:  ');
  tableNamesList := GetTOMLTableNames(myTOMLString);
  writeln(tableNamesList.Text);


  writeln('--- Testing GetTOMLTable from TTOMLDocument: TTOMLTable ');
  myTOMLTable:= GetTOMLTable(myTOML,'servers');
  writeln('TOML Table "servers" : ');
  writeln(myTOMLTable.AsJSON.FormatJSON);

  writeln('--- Testing GetTOMLTableAsString from TTOMLDocument: String ');
  writeln('TOML Table "clients" : ');
  writeln(GetTOMLTableAsString(myTOML,'clients'));

  writeln('--- Testing GetTOMLTableAsString from myTOMLString: String ');
  writeln('TOML Table "clients" : ');
  writeln(GetTOMLTableAsString(myTOMLString,'clients'));

  writeln('--- Testing GetTOMLTable from file :  ');
  myTOMLStringList.Free;
  myTOMLStringList.AddStrings(GetTOMLTable(filePath,'servers'));
  writeln('TOML Table "servers" : ');
  writeln(myTOMLStringList.Text);


  writeln('--- Finding (root Table) keys : ');
  writeln('myTOML.Find("title"): ' + String(myTOML.Find('title'))  );
  writeln('myTOML.Find("owner"): ' + String(myTOML.Find('owner'))  );
  writeln('myTOML.Find("servers"): ' + String(myTOML.Find('servers'))  );
  //writeln('myTOML.Find("servers.alpha"): ' + String(myTOML.Find('servers.alpha'))  );

  writeln('--- Testing TOML.Name + Keys[] + Values[] ');

  writeln('myTOML.Name : ' + myTOML.Name);
  writeln('myTOML.Keys[0] :' + myTOML.Keys[0]);
  writeln('myTOML.Values[0]:' + String(myTOML.Values[0]));

  writeln('myTOML.Keys[1] :' + myTOML.Keys[1]);
  writeln('myTOML.Values[1]:' + String(myTOML.Values[1]));

  writeln('myTOML.Keys[2] :' + myTOML.Keys[2]);
  writeln('myTOML.Values[2]:' + String(myTOML.Values[2]));

  writeln('myTOML.Keys[3] :' + myTOML.Keys[3]);
  writeln('myTOML.Values[3]:' + String(myTOML.Values[3]));

  writeln('myTOML.Keys[4] :' + myTOML.Keys[4]);
  writeln('myTOML.Values[4]:' + String(myTOML.Values[4]));


  writeln('--- Testing accessing TOML data  ');

  myData := myTOML['title'];
  writeln('myTOML["title"] : ', String(myData));

  myTOMLTable := TTOMLTable(myTOML.Items[3]) ;

  writeln('servers.Keys[0] :' + myTOMLTable.Keys[0]);
  writeln('servers.Values[0]:' + String(myTOMLTable.Values[0]));

  writeln('servers.Keys[1] :' + myTOMLTable.Keys[1]);
  writeln('servers.Values[1]:' + String(myTOMLTable.Values[1]));

  myData := myTOMLTable.Items[0];
  myData := TTOMLTable(myData).Items[0] ;
  writeln('myTOML[servers][alpha][ip] : ', String(myData));


  writeln('--- Getting values from keys');

  writeln('- Searching for unexisting key in root table:');
  writeln( GetValueFromTOML(myTOMLString,'key','default') );

  writeln('- Searching for unexisting key in sub-table:');
  writeln( GetValueFromTOML(myTOMLString,'servers.beta.key','default') );

  writeln('- Searching for value of key title :');
  myValue := GetValueFromTOML(myTOMLString,'title','default') ;
  writeln( 'myValue :' + myValue);
  writeln('myTOML["title"] : ' + String(myTOML['title']));

  writeln('- Searching for key owner.name :');
  //writeln( String(myTOML.Find('owner.name')));
  writeln( GetValueFromTOML(myTOMLString,'owner.name','default')  );
  writeln('myTOML["owner"]["name"]: ' + String(myTOML['owner']['name']));

  writeln('- Searching for value of key database.connection_max :');
  writeln( GetValueFromTOML(myTOMLString,'database.connection_max','default')  );
  writeln('myTOML["database"]["connection_max"] : ' + String(myTOML['database']['connection_max']));

  writeln('- Searching for value of key servers.alpha.ip :');
  writeln( GetValueFromTOML(myTOMLString,'servers.alpha.ip','default') );
  writeln('myTOML["servers"]["alpha"]["ip"] : ' + String(myTOML['servers']['alpha']['ip']));


  (* // Testing new TTOMLTable.Insert
  writeln('--- Testing inserting data to TOML  ');
  myTOMLTable := TTOMLTable(myTOML.Items[3]) ;
  myTOMLTable.Insert('newInsertedKeyInServers','"newInsertedValueInServers"');

  myTOML.Insert('newInsertedKeyInRootTable','"newInsertedValueInRootTable"');
  *)

  AddKeyValueToTOML(myTOML,'newKeyInRootTable', '"newValueInRootTable"');

  (* // Testing TTOMLTable.Add
  writeln('--- Testing adding data to TOML  ');

  myTOML.Add('newKey','newValue');

  myTOMLTable.Create('newTable');
  myTOMLTable.Add('newKey', 'newValue');

  myTOML.Add('newTable',myTOMLTable);

  newTOMLTable:= TTOMLTable(myTOML['database']);
  newTOMLTable.Add('newSubTable',myTOMLTable);
  *)

  (*
  writeln('--- Testing AddKeyValueToTOML in root Table ');
  if AddKeyValueToTOML(myTOML,'newKeyInRootTable', '"newValueInRootTable"') = true then
     writeln('- AddKeyValueToTOML in root Table done')
  else
      writeln('- AddKeyValueToTOML in root Table failed');

  writeln('--- Testing AddKeyValueToTOML in sub-Table ');
  if AddKeyValueToTOML(myTOML,'servers.alpha.newKeyInAlpha', '"newValueInAlpha"') = true then
     writeln('- AddKeyValueToTOML in sub-Table done')
  else
      writeln('- AddKeyValueToTOML in sub-Table failed');

    writeln('--- Testing AddKeyValueToTOML in new Table ');
  if AddKeyValueToTOML(myTOML,'newTable.newNewKey', '"newNewValue"') = true then
     writeln('- AddKeyValueToTOML in new Table done')
  else
      writeln('- AddKeyValueToTOML in new Table failed');
  *)

  (* // Testing ADDING directly to the TOML file
    writeln('--- Testing AddKeyValueToTOMLFile in sub-Table ');
  if AddKeyValueToTOMLFile(filePath,'servers.alpha.newKeyInAlpha', '"newValueInAlpha"') = true then
     writeln('- AddKeyValueToTOMLFile in sub-Table done')
  else
      writeln('- AddKeyValueToTOMLFile in sub-Table failed');

    writeln('--- Testing AddKeyValueToTOMLFile in new Table ');
  if AddKeyValueToTOMLFile(filePath,'newTable.newNewKey', '"newNewValue"') = true then
     writeln('- AddKeyValueToTOMLFile in new Table done')
  else
      writeln('- AddKeyValueToTOMLFile in new Table failed');

  writeln('--- Testing AddKeyValueToTOMLFile with a new table ');
  if AddKeyValueToTOMLFile(filePath,'newTable.newTableKey', '"newValue in newTable"') = true then
     writeln('- AddKeyValueToTOMLFile with a new table done')
  else
      writeln('- AddKeyValueToTOMLFile with a new table failed');

  writeln('--- Testing AddKeyValueToTOMLFile with a new sub-table ');

  if AddKeyValueToTOMLFile(filePath,'newSubTable.newSubTableKey', '"newValue in newSubTable"') = true then
     writeln('- AddKeyValueToTOMLFile with a new sub-table done')
  else
      writeln('- AddKeyValueToTOMLFile with a new sub-table failed');

  writeln('--- Getting final TOMLTable :  ');

  myTOMLTable:= GetTOMLTable(myTOML,'newTable');
  writeln(myTOMLTable.AsJSON.FormatJSON);
  *)

  writeln('--- Testing ConvertTOMLtoJSON :');
  writeln(ConvertTOMLtoJSON(myTOMLString));

  (*
  // These data types are supported
  writeln('--- Testing adding String data to TOML sub Table ');
  AddKeyValueToTOML(myTOML,'owner.newKeyInOwner','newValueInOwner');

  writeln('--- Testing adding Integer data to TOML sub Table ');
  AddKeyValueToTOML(myTOML,'owner.newIntegerValue',2020);

  writeln('--- Testing adding Float data to TOML sub Table ');
  AddKeyValueToTOML(myTOML,'owner.newFloatValue',20.20);

  writeln('--- Testing adding Boolean data to TOML sub Table ');
  AddKeyValueToTOML(myTOML,'owner.newBooleanValue', true);

  writeln('--- Testing adding DateTime data to TOML sub Table ');
  AddKeyValueToTOML(myTOML,'owner.newDateTimeValue','1979-05-27');

  writeln('--- Testing adding Array data to TOML sub Table ');
  myArray := TTOMLArray.Create;
  myArray.Add('First element');
  myArray.Add('Second element');
  myArray.Add('Third element');

  //myValue:='[''First element'',''Second element'',''Third element'']' ;
  //myNewTOML:= GetTOML(myValue);

  AddKeyValueToTOML(myTOML,'owner.newArrayValue',myArray);
  *)

  (*
  // These data types are not supported in TTOMLValueType
  writeln('--- Testing adding Octal data to TOML sub Table ');
  AddKeyValueToTOML(myTOML,'owner.newOctalValue',myOctal);

  writeln('--- Testing adding Hexadecimal data to TOML sub Table ');
  AddKeyValueToTOML(myTOML,'owner.newHexadecimalValue',myHexadecimal);

  writeln('--- Testing adding Hexadecimal data to TOML sub Table ');
  AddKeyValueToTOML(myTOML,'owner.newBinaryValue',myBinary);
  *)



  (*
  if map.TryGetData('title', myData) then
    writeln('myTOML["title"] : ', String(myData))
  else
    writeln('myTOML["title"] : NIL');
  *)

  //myData := myTOML['owner']['name'];
  //myData := myData.Items[0];
  //writeln('myTOML["owner"]["Items"][0] : ', string(myData));

  //myData := myTOML['servers']['servers.alpha']['ip'][0];
  //writeln('myTOML["servers.alpha.ip"][0] : ', string(myData));

  //myData := myTOML['database']['ports'][0];
  //writeln('Port #0: ', string(myData));

  //myData := myTOML['database']['ports'];
  //writeln('Ports: ', string(myData));
  //writeln('Ports: ', myData);

  //writeln('ports:');
  //for myData in myTOML['database']['ports'] do
  //  writeln('  - ', string(myData));


  {*
  // First attempt

  myTOML := GetTOML(myFile.Text);
  writeln('GetTOML done');

  myJSON := myTOML.AsJSON;

  with TJSONStreamer.Create(nil) do try
      //myJSONStreamer := TJSONStreamer.Create(nil);
      myStream := TMemoryStream.Create;
      try
          //myJSONString := myJSONStreamer.ObjectToJSONString(myJSON);
          myJSONString := ObjectToJSONString(myJSON);
          //myStream.Write(myJSONString[1], length(myJSONString));
          myStream.Write(myJSON,myJSON.Count);
          myStream.SaveToFile('/home/jinene/gitwork/lazarus/helper/opsi-TOML/tests/myJSONfromTOMLdata.json');
          writeln('SaveToFile done');
      finally
          myStream.Free;
          //myJSONStreamer.Free;
      end;
  finally
    Free;
  end;
  *}

  {*
  // Second attempt

  myTOML := nil;
  myTOML := GetTOML(myFile.Text);
  //writeln(myTOML);
  myJSON := myTOML.AsJSON;
  writeln(myJSON.FormatJSON);
  //myJSON.SaveToFile('/home/jinene/gitwork/lazarus/helper/opsi-TOML/tests/myJSONfromTOMLdata.json');
  myJSON.Free;

  myTOML.Free;

  //myStrings := myTOML.AsStrings;
  *}


end.

