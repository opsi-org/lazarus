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

  i : integer = 0;
  sectionNamesList : TStringList;
  myTOMLStringList : TStringList;

begin
  writeln('--- Begin');

  path := '/home/jinene/gitwork/lazarus/helper/opsi-TOML/tests/TOMLexample.toml';

  //myFile := TStringList.Create;
  myTOMLString := ReadTOMLFile(path);

  writeln('--- File read');

  myTOML := GetTOML(myTOMLString);

  writeln('--- GetTOML done');

  //writeln(myTOML.AsJSON.FormatJSON);
  //writeln('--- Writeln asJSON done');

  filePath := '/home/jinene/gitwork/lazarus/helper/opsi-TOML/tests/TOMLempty.toml';
  //myFile := TStringList.Create;
  //myFile[0] := myTOMLString;
  //myFile.Add(['aaaa', 'bbbb', 'ccccc']);
  //myFile.SaveToFile(filePath);

  if ( SaveToTOMLFile(myTOMLString, filePath) ) then
     writeln('--- SaveToTOMLFile done')
  else
      writeln('--- SaveToTOMLFile not done');

  JSONpath := '/home/jinene/gitwork/lazarus/helper/opsi-TOML/tests/myJSONfromTOMLdata.json' ;
  if ( ConvertTOMLtoJSON(path, JSONpath)) then
     writeln('--- ConvertTOMLtoJSON done')
  else
      writeln('--- ConvertTOMLtoJSON not done');


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

  writeln('--- Getting TOML sections ');

  writeln('Testing GetTOMLSectionNames :');

  sectionNamesList := GetTOMLSectionNames(path);
  writeln(sectionNamesList.Text);

  writeln('GetTOMLSectionNames done');

  writeln('--- Getting values from keys');

  writeln('- Searching for value of key : title  ');
  myValue := GetValueFromTOMLfile(path,'','title','default') ;
  writeln( 'myValue :' + myValue);

  writeln('- Searching for key name :');
  //writeln( String(myTOML.Find('name')));
  //writeln( GetValueFromTOMLfile(path,'','name','default')  );

  writeln('- Searching for value of key ip :');
  //writeln( GetValueFromTOMLfile(path,'','ip','default')  );

  writeln('- Searching for value of key servers.alpha :');
  //writeln( GetValueFromTOMLfile(path,'','servers.alpha','default')  );

  writeln('- Searching for value of key servers.alpha.ip :');
  //myValue := GetValueFromInifile(path,'','servers.alpha.ip','default') ;


  writeln('--- Getting TOML as StringList  ');

  myTOMLStringList := TStringList.Create;
  myTOMLStringList.AddStrings(GetTOMLFile(path));
  //writeln(myTOMLStringList.Text);


  writeln('--- Getting TOML section  ');

  myTOMLStringList.Free;
  myTOMLStringList.AddStrings(GetTOMLSection(path,'database'));
  writeln(myTOMLStringList.Text);

  writeln('--- Accessing TOML data  ');

  writeln(Length(myTOML.Keys[1])) ;
  //writeln(Length(myTOML.Values[0]))  ;


  //myData := myTOML['']['title'];
  myData := myTOML['title'];
  myData := myData.Items[0];
  writeln('myTOML["title"] : ', myData.ToString);
  writeln('myTOML["title"] : ', String(myData));

  myData := myTOML['owner']['name'];
  writeln('myTOML["owner"]["name"] : ', string(myData));

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

