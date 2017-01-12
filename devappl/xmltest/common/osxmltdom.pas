unit osxmltdom;
(* Defines global objects, especially those for handling xml basec on TDOM *)

// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: martina hammel
// credits: http://www.opsi.org/credits/


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLRead, XMLWrite;


//******************************************************************************
function createXmlDocFromStringlist(docstrlist: TStringlist): boolean;
function createXmlDocFromFile(fileName: String): boolean;
function getXMLDoc (): TXMLDocument;
function freeXmlDoc(): boolean;
function getDocumentElement(): TDOMNode;
function getDocNodeName(): string;
function getUniqueChildNodeByName(mynode: TDOMNode; NodeName: string;
         var Value: TDOMNode): boolean;
function getNodeattributeByKey (mynode: TDOMNode; attributekey: String;
         var value: String) :boolean;

//******************************************************************************
implementation
var XML: TXMLDocument;

function createXmlDocFromStringlist(docstrlist: TStringlist): boolean;
// XML global, da sonst die erzeugten Knoten im Nirvana landen
var
  mystream: TStringStream;
begin
  Result := false;
  mystream := TStringStream.Create(docstrlist.Text);
  mystream.Position := 0;
  XML:=Nil;
  try
    ReadXMLFile(XML, mystream);
    result:=true;
  finally
    mystream.Free;
  end;
end;

function createXmlDocFromFile(fileName: String): boolean;
var
  mystream: TFileStream;
begin
  Result := false;
  mystream := TFilestream.Create(fileName,fmOpenRead);
  mystream.Position := 0;
  XML:=Nil;
  try
    ReadXMLFile(XML, mystream);
    result:=true;
  finally
    mystream.Free;
  end;
end;

function getXMLDoc (): TXMLDocument;
begin
  Result:= XML;
end;
function freeXmlDoc(): boolean;
begin
  Result := True;
  try
    XML.Free;
  except
    Result := False;
    exit;
  end;
end;

function getDocumentElement(): TDOMNode;
begin
  if XML<> NIL then
    Result:= getXMLDoc.DocumentElement
  else
    Result:=NIL;
end;
function getDocNodeName(): string;
begin
  Result:='';
  if XML<>NIL then
    Result := getXMLDoc.DocumentElement.NodeName;
end;
function getUniqueChildNodeByName(mynode: TDOMNode; NodeName: string; var Value: TDOMNode): boolean;
var
  i: integer;
begin
  Result := False;
  i := 0;
  if (mynode <> nil) then
    while i < mynode.ChildNodes.Count do
    begin
      if NodeName = mynode.ChildNodes.Item[i].NodeName then
      begin
        Value := mynode.ChildNodes.Item[i];
        Result := True;
      end;
      Inc(i);
    end;
end;


function getNodeattributeByKey (mynode: TDOMNode; attributekey: String; var value: String) :boolean;
var
  i, test : integer;  // test - wg eindeutigkeit???
begin
  result:=false;
  test:=0;
  for i := 0 to mynode.Attributes.Length - 1 do
  begin
    if mynode.Attributes[i].NodeName = attributekey then
      begin
        value:= mynode.Attributes[i].TextContent;
        inc(test)
      end;
  end;
  if (test=1) then result:=true;
end;
end.

