program opsiTOML;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  SysUtils,
  TOML, TOMLParser, TOMLfunc,
  FGL, FPJSON, fpJSONrtti;


Type
  TA = Array[0..9] of Integer;

var
  path : String;
  myFile: TStringList;
  filePath: String;
  JSONpath : String;

  myTOMLString: String;

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
  *)
  myDateTime : TDateTime =  1979-05-27 ;
  myNewTOML : TTOMLDocument;

  i : integer = 0;
  tableNamesList : TStringList;
  myTOMLStringList : TStringList;

  myTOMLTable : TTOMLTable;

  nb : integer;

begin
  writeln('--- Begin');

  path := '/home/jinene/gitwork/lazarus/helper/opsi-TOML/tests/TOMLexample.toml';

  writeln('--- Testing ReadTOMLFile :');
  //myFile := TStringList.Create;
  myTOMLString := ReadTOMLFile(path);

  writeln('--- Testing GetTOML :');

  myTOML := GetTOML(myTOMLString);

  writeln('--- Testing HasTables : ');
  nb := HasTables(myTOML);

  //writeln('--- Testing TOML.AsJSON :');
  //writeln(myTOML.AsJSON.FormatJSON);

  writeln('--- Testing SaveToTOMLFile') ;
  filePath := '/home/jinene/gitwork/lazarus/helper/opsi-TOML/tests/TOMLempty.toml';

  if ( SaveToTOMLFile(myTOMLString, filePath) ) then
     writeln('--- SaveToTOMLFile with String parameter done')
  else
      writeln('--- SaveToTOMLFile with String parameter not done');
  (*
  if ( SaveToTOMLFile(myTOML, filePath) ) then
     writeln('--- SaveToTOMLFile with TTOMLDocument parameter done')
  else
      writeln('--- SaveToTOMLFile with TTOMLDocument parameter not done');
  *)
  JSONpath := '/home/jinene/gitwork/lazarus/helper/opsi-TOML/tests/myJSONfromTOMLdata.json' ;
  if ( ConvertTOMLtoJSON(path, JSONpath)) then
     writeln('--- ConvertTOMLtoJSON done')
  else
      writeln('--- ConvertTOMLtoJSON not done');


  writeln('--- Testing LoadTOMLFile :  ');

  myTOMLStringList := TStringList.Create;
  myTOMLStringList.AddStrings(LoadTOMLFile(path));
  //writeln(myTOMLStringList.Text);


  writeln('--- Testing GetTOMLTableNames with a file parameter:  ');

  tableNamesList := GetTOMLTableNames(path);
  writeln(tableNamesList.Text);


  writeln('--- Testing GetTOMLTable :  ');

  myTOMLStringList.Free;
  myTOMLStringList.AddStrings(GetTOMLTable(path,'servers'));
  writeln(myTOMLStringList.Text);

  writeln('--- Testing GetTOMLTable :  ');

  myTOMLTable:= GetTOMLTable(myTOML,'servers');
  writeln(myTOMLTable.AsJSON.FormatJSON);


  writeln('--- Finding key "title" ');
  myKey := String(myTOML.Find('title'));
  writeln( 'myKey found: ' + myKey);


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

  writeln('myKey title : ' + String(myTOML.Find('title')) );


  writeln('--- Getting values from keys');

  writeln('- Searching for unexisting key in root table:');
  writeln( GetValueFromTOMLfile(path,'key','default') );

  writeln('- Searching for unexisting key in sub-table:');
  writeln( GetValueFromTOMLfile(path,'servers.beta.key','default') );

  writeln('- Searching for value of key title :');
  myValue := GetValueFromTOMLfile(path,'title','default') ;
  writeln( 'myValue :' + myValue);
  writeln(String(myTOML['title']));

  writeln('- Searching for key name :');
  //writeln( String(myTOML.Find('owner.name')));
  writeln( GetValueFromTOMLfile(path,'owner.name','default')  );
  writeln(String(myTOML['owner']['name']));

  writeln('- Searching for value of key database.connection_max :');
  writeln( GetValueFromTOMLfile(path,'database.connection_max','default')  );
  writeln(String(myTOML['database']['connection_max']));

  writeln('- Searching for value of key servers.alpha.ip :');
  writeln( GetValueFromTOMLfile(path,'servers.alpha.ip','default') );
  writeln(String(myTOML['servers']['alpha']['ip']));

  writeln('--- Accessing TOML data  ');

  writeln(Length(myTOML.Keys[1])) ;
  //writeln(Length(myTOML.Values[0]))  ;


  //myData := myTOML['']['title'];
  myData := myTOML['title'];
  writeln('myTOML["title"] : ', String(myData));


  myTOMLTable := TTOMLTable(myTOML.Items[3]) ;
  myData := myTOMLTable.Items[0];
  myData := TTOMLTable(myData).Items[0] ;

  writeln('servers.Keys[0] :' + myTOMLTable.Keys[0]);
  writeln('servers.Values[0]:' + String(myTOMLTable.Values[0]));

  writeln('servers.Keys[1] :' + myTOMLTable.Keys[1]);
  writeln('servers.Values[1]:' + String(myTOMLTable.Values[1]));

  writeln('myTOML[servers][servers.alpha][ip] : ', String(myData));

  writeln('--- Testing GetTOMLTableNames with a TTOMLTable parameter:  ');

  tableNamesList := GetTOMLTableNames(myTOMLTable);
  writeln('The TOMLTable [servers] has sub-tables :');
  writeln(tableNamesList.Text);

  writeln('--- Testing adding data to TOML  ');

  myTOML.Add('newKey','newValue');
  writeln('myTOML.Keys[0] :' + myTOML.Keys[0]);
  writeln('myTOML.Values[0]:' + String(myTOML.Values[0]));

  writeln('myTOML.Keys[1] :' + myTOML.Keys[1]);
  writeln('myTOML.Values[1]:' + String(myTOML.Values[1]));

  writeln('myTOML.Keys[5] :' + myTOML.Keys[5]);
  writeln('myTOML.Values[5]:' + String(myTOML.Values[5]));

  writeln('--- Testing adding String data to TOML sub Table ');
  AddKeyValueToTOML(myTOML,'owner.newKeyInOwner','owner.newValueInOwner');

  writeln('--- Testing adding Integer data to TOML sub Table ');
  AddKeyValueToTOML(myTOML,'owner.newIntegerValue',2020);

  writeln('--- Testing adding Float data to TOML sub Table ');
  AddKeyValueToTOML(myTOML,'owner.newFloatValue',20.20);

  writeln('--- Testing adding Boolean data to TOML sub Table ');
  AddKeyValueToTOML(myTOML,'owner.newBooleanValue', true);

  (*
  writeln('--- Testing adding Octal data to TOML sub Table ');
  AddKeyValueToTOML(myTOML,'owner.newOctalValue',myOctal);

  writeln('--- Testing adding Hexadecimal data to TOML sub Table ');
  AddKeyValueToTOML(myTOML,'owner.newHexadecimalValue',myHexadecimal);

  writeln('--- Testing adding Hexadecimal data to TOML sub Table ');
  AddKeyValueToTOML(myTOML,'owner.newBinaryValue',myBinary);
  *)
  writeln('--- Testing adding DateTime data to TOML sub Table ');
  AddKeyValueToTOML(myTOML,'owner.newDateTimeValue',myDatetime);

  //writeln(myTOML.AsJSON.FormatJSON);

  writeln('--- Testing adding Array data to TOML sub Table ');
  myArray := TTOMLArray.Create;
  myArray.Add('First element');
  myArray.Add('Second element');
  myArray.Add('Third element');

  //myValue:='[''First element'',''Second element'',''Third element'']' ;
  //myNewTOML:= GetTOML(myValue);

  AddKeyValueToTOML(myTOML,'owner.newArrayValue',myArray);

  //myTOMLTable:= GetTOMLTable(myTOML,'owner');
  myTOMLTable:= TTOMLTable(myTOML.Items[1]);
  //writeln(myTOMLTable.AsJSON.FormatJSON);

  writeln('owner.Keys[6] :' + myTOMLTable.Keys[6]);
  writeln('owner.Values[6]:' + String(myTOMLTable.Values[6]));

  writeln('owner.Keys[7] :' + myTOMLTable.Keys[7]);
  writeln('owner.Values[7]:' + String(myTOMLTable.Values[7]));


  writeln('--- Getting final TOMLTable :  ');

  myTOMLStringList.Free;
  myTOMLStringList.AddStrings(GetTOMLTable(path,'owner'));
  writeln(myTOMLStringList.Text);

  writeln('--- Testing TOML.AsJSON :');
  //writeln(myTOML.AsJSON.FormatJSON);

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

