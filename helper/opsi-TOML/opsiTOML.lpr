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

begin
  writeln('Begin');

  path := '/home/jinene/gitwork/lazarus/helper/opsi-TOML/tests/TOMLexample.toml';

  //myFile := TStringList.Create;
  myTOMLString := ReadTOMLFile(path);

  writeln('File read');

  myTOML := GetTOML(myTOMLString);

  writeln('GetTOML done');
  //writeln(myTOML.AsJSON.FormatJSON);

  writeln('Writeln asJSON done');

  filePath := '/home/jinene/gitwork/lazarus/helper/opsi-TOML/tests/TOMLempty.toml';
  myFile := TStringList.Create;
  //myFile[0] := myTOMLString;
  myFile.Add('aaaa');
  //, 'bbbb', 'ccccc'];





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

