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
  myTValue : Double;

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

  filePath := '/home/jinene/gitwork/lazarus/helper/opsi-TOML/tests/TOMLempty.toml';

  writeln('--- Testing SaveToTOMLFile') ;

  if ( SaveToTOMLFile(myTOMLString, filePath) ) then
     writeln('--- SaveToTOMLFile done')
  else
      writeln('--- SaveToTOMLFile not done');

  JSONpath := '/home/jinene/gitwork/lazarus/helper/opsi-TOML/tests/myJSONfromTOMLdata.json' ;
  if ( ConvertTOMLtoJSON(path, JSONpath)) then
     writeln('--- ConvertTOMLtoJSON done')
  else
      writeln('--- ConvertTOMLtoJSON not done');


  writeln('--- Testing LoadTOMLFile :  ');

  myTOMLStringList := TStringList.Create;
  myTOMLStringList.AddStrings(LoadTOMLFile(path));
  //writeln(myTOMLStringList.Text);


  writeln('--- Testing GetTOMLTableNames :  ');

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

  writeln('- Searching for value of key title :');
  myValue := GetValueFromTOMLfile(path,'title','default') ;
  writeln( 'myValue :' + myValue);

  writeln('- Searching for key name :');
  //writeln( String(myTOML.Find('name')));
  writeln( GetValueFromTOMLfile(path,'owner.name','default')  );

  writeln('- Searching for value of key connection_max :');
  writeln( GetValueFromTOMLfile(path,'database.connection_max','default')  );

  writeln('- Searching for value of key servers.alpha.ip :');
  writeln( GetValueFromTOMLfile(path,'servers.alpha.ip','default') );


  writeln('--- Accessing TOML data  ');

  writeln(Length(myTOML.Keys[1])) ;
  //writeln(Length(myTOML.Values[0]))  ;


  //myData := myTOML['']['title'];
  myData := myTOML['title'];
  //writeln('myTOML["title"] : ', String(myData));


  myTOMLTable := TTOMLTable(myTOML.Items[3]) ;
  myData := myTOMLTable.Items[0];
  myData := TTOMLTable(myData).Items[0] ;

  writeln('servers.Keys[0] :' + myTOMLTable.Keys[0]);
  writeln('servers.Values[0]:' + String(myTOMLTable.Values[0]));

  writeln('servers.Keys[1] :' + myTOMLTable.Keys[1]);
  writeln('servers.Values[1]:' + String(myTOMLTable.Values[1]));

  writeln('myTOML[servers][servers.alpha][ip] : ', String(myData));

  writeln('--- Testing adding data to TOML  ');

  myTOML.Add('newKey','newValue');
  writeln(myTOML.AsJSON.FormatJSON);
  writeln('myTOML.Keys[0] :' + myTOML.Keys[0]);
  writeln('myTOML.Values[0]:' + String(myTOML.Values[0]));

  writeln('myTOML.Keys[1] :' + myTOML.Keys[1]);
  writeln('myTOML.Values[1]:' + String(myTOML.Values[1]));

  writeln('myTOML.Keys[5] :' + myTOML.Keys[5]);
  writeln('myTOML.Values[5]:' + String(myTOML.Values[5]));


  writeln('--- Testing adding data to TOML sub Table ');

  myTValue := 2.5 ;
  AddKeyValueToTOML(myTOML,'owner.newKeyInOwner',myTValue);
  writeln(myTOML.AsJSON.FormatJSON);

  myTOMLTable:= GetTOMLTable(myTOML,'owner');
  writeln(myTOMLTable.AsJSON.FormatJSON);

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

