unit TOMLfunc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  TOML, TOMLParser, TOMLTypes,
  FGL, FPJSON, fpJSONrtti;


function ReadTOMLFile(filePath: String): String;
function SaveToTOMLFile(TOMLcontents : String; filePath: String): boolean;
function ConvertTOMLtoJSON(TOMLfile: String; JSONfile: String): boolean;
function GetValueFromTOMLfile(TOMLfile: String; section: String; key: String; defaultValue: String): String;

implementation

 
function ReadTOMLFile(filePath: String): String;
var
  myFile: TStringList;
begin
  myFile := TStringList.Create;
  filePath := ExpandFileName(filePath);
  try
  myFile.LoadFromFile(filePath);
  result := myFile.Text;
  except
    on E:Exception do
      writeln('Exception in LoadFromFile '+ filePath +': ', E.Message);
  end;
  myFile.Free;
end;

function SaveToTOMLFile(TOMLcontents : String; filePath: String): boolean;
var
  myFile: TStringList;
begin
  result := False;
  myFile := TStringList.Create;
  filePath := ExpandFileName(filePath);
  myFile.Add(TOMLcontents);
  //writeln('' + myFile.Text);
  try
  myFile.SaveToFile(filePath);
  result := True;
  except
    on E:Exception do
      writeln('Exception in SaveToFile '+ filePath +': ', E.Message);
  end;
  myFile.Free;
end;

function ConvertTOMLtoJSON(TOMLfile: String; JSONfile: String): boolean;
var
  myFile: TStringList;
  myTOML : TTOMLDocument;
  myJSON : TJSONData;
begin
  result := False;
  myFile := TStringList.Create;
  TOMLfile := ExpandFileName(TOMLfile);
  JSONfile := ExpandFileName(JSONfile);
  try
  myFile.LoadFromFile(TOMLfile);
  except
    on E:Exception do
      writeln('Exception in LoadFromFile '+ TOMLfile +': ', E.Message);
  end;
  myTOML := GetTOML(myFile.Text);
  myJSON := myTOML.AsJSON;
  myFile.Clear;
  myFile.Add(myJSON.FormatJSON);
  try
  myFile.SaveToFile(JSONfile);
  result := True;
  except
    on E:Exception do
      writeln('Exception in SaveToFile'+ JSONfile +': ', E.Message);
  end;
  myFile.Free;
end;

function GetValueFromTOMLfile(TOMLfile: String; section: String; key: String; defaultValue: String): String;
var
  myFile : String;
  myTOML : TTOMLDocument;
begin
  result := defaultValue;
  TOMLfile := ExpandFileName(TOMLfile);
  myFile := ReadTOMLFile(TOMLfile);
  myTOML := GetTOML(myFile);
  result := String(myTOML.GetItem(key));
  if result='' then
     result := defaultValue;
end;



end.

