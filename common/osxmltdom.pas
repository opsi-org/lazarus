unit osxmltdom;

(* Defines global objects, especially those for handling xml basec on TDOM *)

// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: martina hammel


{$mode objfpc}{$H+}

interface

uses
  {$IFDEF OPSISCRIPT}
  oslog,
  {$ENDIF OPSISCRIPT}
  Classes, SysUtils, DOM, XMLRead, XMLWrite;


//******************************************************************************
function createXmlDocFromStringlist(docstrlist: TStringList): boolean;
function createXmlDocFromFile(fileName: string): boolean;
function XMLexists(): boolean;
function getXMLDoc(): TXMLDocument;
function freeXmlDoc(): boolean;
function getDocumentElement(): TDOMNode;
function getDocNodeName(): string;
//******************************************************************************
function createXMLNodeFromString(nodestrlist: TStringList): TDOMNode;
function createStringListFromXMLNode(mynode: TDOMNode;
  var mystringlist: TStringList): boolean;
//******************************************************************************
// actualNode manipulation
function setActualValueTDOMNode(nodeValue: string): boolean;
function setActualAttributeValueTDOMNode(attributekey: string;
  attributevalue: string): boolean;
//******************************************************************************
// by name
function getUniqueChildNodeByName(mynode: TDOMNode; myName: string;
  var Value: TDOMNode): boolean;

function getChildnodeByNameAndAttributeKeyAndValue(var mynode: TDOMNode;
  myName: string; attributekey: string;
  attributevalue: string; var newnode: TDOMNode): boolean;

function replaceUniqueChildNodeByName(var mynode: TDOMNode; myName: string;
  Value: TDOMNode): boolean;
function selectAllNodesByName(var mynode: TDOMNode; myName: string): boolean;
function replaceChildnodeByNameAndAttributeKeyAndValue(var mynode: TDOMNode;
  myname: string; attributekey: string; attributevalue: string;
  newnode: TDOMNode): boolean;
//******************************************************************************
// by index
function getChildTDOMNodeByIndex(mynode: TDOMNode; index: integer;
  var Value: TDOMNode): boolean;
function setChildTDOMNodeValueByIndex(var mynode: TDOMNode; index: integer;
  Value: string): boolean;
function getChildTDOMNodeValueByIndex(mynode: TDOMNode; index: integer;
  var Value: string): boolean;
function deleteChildTDOMNodeByIndex(var mynode: TDOMNode; index: integer): boolean;
//******************************************************************************
// attributes
function getAttributesValueList(mynode: TDOMNode;
  var attributevaluelist: TStringList): boolean;
function getChildnodeByAttributeKeyAndValue(var mynode: TDOMNode;
  attributekey: string; attributevalue: string; var Value: TDOMNode): boolean;
function getAttributeKeyList(mynode: TDOMNode): TStringList;
function getNodeattributeByKey(mynode: TDOMNode; attributekey: string;
  var Value: string): boolean;
function deleteChildAttributeByKey(var mynode: TDOMNode; attributekey: string): boolean;
function getChildAttributeValueTDOMNode(mynode: TDOMNode; attributekey: string;
  var attributevalue: string): boolean;
function setChildAttributeValueTDOMNode(var mynode: TDOMNode;
  attributekey: string; attributevalue: string): boolean;
//******************************************************************************
function getNodeType(mynode: TDOMNode): string;
function getTextEntry(mynode: TDOMNode): string;
function docElementsAsStringlist(myxdoc: TXMLDocument): TStringList;
function getNodeElementsAsStringlist(mynode: TDOMNode): TStringList;
//******************************************************************************
// rekursive Testfunktionen
function getNodeName(mynode: TDOMNode; var valueList: TStringList): boolean;
function getSubNodeNames(myxdoc: TXMLDocument): TStringList;
function getNodeValue(mynode: TDOMNode; var valueList: TStringList): boolean;
function getSubNodeValues(myxdoc: TXMLDocument): TStringList;
//******************************************************************************
// Helper
function stringlistWithoutBreaks(strlist: TStringList): TStringList;
//******************************************************************************
implementation

var
  XML: TXMLDocument;
  actualNode: TDOMNode;

function createXmlDocFromStringlist(docstrlist: TStringList): boolean;
  // XML global, da sonst die zu erzeugenden erzeugten Knoten im Nirvana landen
var
  mystream: TStringStream;
begin
  Result := False;
  mystream := TStringStream.Create(docstrlist.Text);
  mystream.Position := 0;
  XML := nil;
  try
    ReadXMLFile(XML, mystream);
    if XMLexists() then
      Result := True;
  finally
    mystream.Free;
  end;
end;

function createXmlDocFromFile(fileName: string): boolean;
var
  mystream: TFileStream;
begin
  Result := False;
  mystream := TFilestream.Create(fileName, fmOpenRead);
  mystream.Position := 0;
  XML := nil;
  try
  try
    ReadXMLFile(XML, mystream);
    if XMLexists() then
      Result := True;

  except
        on E: Exception do
        begin
          {$IFDEF OPSISCRIPT}
          LogDatei.log('Exception in createXmlDocFromFile', LLError);
          LogDatei.log('Error: Message: ' + E.message, LLError);
          {$ENDIF OPSISCRIPT}
        end;
      end;
  finally
    mystream.Free;
  end;
end;

function XMLexists(): boolean;
begin
  if XML <> nil then
    Result := True
  else
    Result := False;
end;

function getXMLDoc(): TXMLDocument;
begin
  Result := XML;
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
  if XML <> nil then
    Result := getXMLDoc.DocumentElement
  else
    Result := nil;
end;

function getDocNodeName(): string;
begin
  Result := '';
  if XML <> nil then
    Result := getXMLDoc.DocumentElement.NodeName;
end;

//*****************************************************************************
function createXMLNodeFromString(nodestrlist: TStringList): TDOMNode;
var
  nodestream: TStringStream;
  mynode: TDOMNode;
begin
  Result := nil;
  nodestream := TStringStream.Create(stringlistWithoutBreaks(nodestrlist).Text);
  nodestream.Position := 0;
  try
    if XML <> nil then
    begin
      mynode := XML.CreateElement('opsibase');
      ReadXMLFragment(mynode, nodestream);
      Result := mynode.FirstChild;
    end
  finally
    nodestream.Free;
  end;
end;

function createStringListFromXMLNode(mynode: TDOMNode;
  var mystringlist: TStringList): boolean;
var
  nodeStream: TMemoryStream;
begin
  Result := False;
  mystringlist := TStringList.Create;
  nodeStream := TMemoryStream.Create;
  try
    WriteXML(mynode, nodeStream);
    nodeStream.Position := 0;
    mystringlist.LoadFromStream(nodestream);
    Result := True;
  finally
    nodestream.Free;
  end;
end;

//*****************************************************************************
// actualNode manipulation
function setActualValueTDOMNode(nodeValue: string): boolean;
var
  i: integer;
begin
  Result := False;
  if actualNode <> nil then
  begin
    actualNode.TextContent := nodeValue;
    Result := True;
  end;
end;

function setActualAttributeValueTDOMNode(attributekey: string;
  attributevalue: string): boolean;
var
  i: integer;
begin
  Result := False;
  if actualNode <> nil then
    if actualNode.HasAttributes then
      if actualNode.Attributes[i].NodeName = attributekey then
      begin
        actualNode.Attributes[i].TextContent := attributevalue;
        Result := True;
      end;
end;

//*****************************************************************************
// by name
function getUniqueChildNodeByName(mynode: TDOMNode; myName: string;
  var Value: TDOMNode): boolean;
var
  i: integer;
  str: string;
begin
  Result := False;
  i := 0;
  if (mynode <> nil) then
    while i < mynode.ChildNodes.Count do
    begin
      str := mynode.ChildNodes.Item[i].NodeName;
      if myName = str then
      begin
        Value := mynode.ChildNodes.Item[i];
        actualNode := mynode.ChildNodes.Item[i];
        Result := True;
      end;
      Inc(i);
    end;
end;

function getChildnodeByNameAndAttributeKeyAndValue(var mynode: TDOMNode;
  myName: string; attributekey: string;
  attributevalue: string; var newnode: TDOMNode): boolean;
var
  j, i: integer;
begin
  Result := False;
  try
    j := 0;
    while (mynode.hasChildNodes) and (j < mynode.ChildNodes.Count) and (Result = False) do
    begin
      if (mynode.ChildNodes.Item[j].HasAttributes) then
      begin
        for i := 0 to mynode.ChildNodes.Item[j].Attributes.Length - 1 do
        begin
          if (mynode.ChildNodes.Item[j].NodeName = myName) and
            (mynode.ChildNodes.Item[j].Attributes[i].NodeName = attributekey) and
            (mynode.ChildNodes.Item[j].Attributes[i].TextContent =
            attributevalue) then
          begin
            newnode := mynode.ChildNodes.Item[j];
            actualNode := mynode.ChildNodes.Item[j];
            Result := True;
          end;
        end;
      end;
      Inc(j);
    end;
  finally

  end;

end;

function replaceUniqueChildNodeByName(var mynode: TDOMNode; myName: string;
  Value: TDOMNode): boolean;
var
  i: integer;
  selNode: TDOMNode;
begin
  Result := False;
  i := 0;
  if (mynode <> nil) then
    while (mynode.hasChildNodes) and (i < mynode.ChildNodes.Count) do
    begin
      if (myName = mynode.ChildNodes.Item[i].NodeName) and (Result = False) then
      begin
        selNode := mynode.ChildNodes.Item[i];
        selNode.Free;   // remove
        mynode.AppendChild(Value); // append new
        Result := True;
      end;
      Inc(i);
    end;
end;

// get all Nodes by Name ????
function selectAllNodesByName(var mynode: TDOMNode; myname: string): boolean; // TODO
var
  i: integer;
  selNode: TDOMNode;
  // eigener Knoten für die Auswahl - mynode in Ruhe lassen??
begin
  Result := False;
  i := 0;
  if (mynode <> nil) then
    while (mynode.hasChildNodes) and (i < mynode.ChildNodes.Count) do
    begin
      if (myname <> mynode.ChildNodes.Item[i].NodeName) or
        (mynode.ChildNodes.Item[i].NodeType = COMMENT_NODE) then
      begin
        selNode := mynode.ChildNodes.Item[i];
        selNode.Free;   // remove
      end;
      Inc(i);
    end;
  if (mynode <> nil) then
    Result := True;
end;

function replaceChildnodeByNameAndAttributeKeyAndValue(var mynode: TDOMNode;
  myName: string; attributekey: string; attributevalue: string;
  newnode: TDOMNode): boolean;
var
  i, j: integer;
  selNode: TDOMNode;
begin
  Result := False;
  i := 0;
  if (mynode <> nil) then
    while (mynode.hasChildNodes) and (i < mynode.ChildNodes.Count) do
    begin
      if (myName = mynode.ChildNodes.Item[i].NodeName) and (Result = False) then
      begin
        for j := 0 to mynode.ChildNodes.Count - 1 do
        begin
          if mynode.ChildNodes.Item[i].HasAttributes then
            if (mynode.ChildNodes.Item[i].Attributes[j].NodeName =
              attributekey) and
              (mynode.ChildNodes.Item[i].Attributes[j].TextContent = attributevalue) then
            begin
              selNode := mynode.ChildNodes.Item[i];
              selNode.Free;   // remove
              mynode.AppendChild(newnode); // append new
              Result := True;
            end;
        end;
      end;
      Inc(i);
    end;
end;
//************************************************************

//************************************************************
// by index
function getChildTDOMNodeByIndex(mynode: TDOMNode; index: integer;
  var Value: TDOMNode): boolean;
begin
  Result := False;
  if mynode.hasChildNodes and (index < mynode.ChildNodes.Count - 1) then
  begin
    Value := mynode.ChildNodes.Item[index];
    Result := True;
  end;
end;

function getChildTDOMNodeValueByIndex(mynode: TDOMNode; index: integer;
  var Value: string): boolean;
begin
  Result := False;
  try
    if mynode.hasChildNodes and (index < mynode.ChildNodes.Count - 1) then
    begin
      with mynode.ChildNodes do
        Value := Item[index].TextContent;
      Result := True;
    end;
  finally

  end;
end;

function setChildTDOMNodeValueByIndex(var mynode: TDOMNode; index: integer;
  Value: string): boolean;
var
  selNode: TDOMNode;
begin
  Result := False;
  try
    if mynode.hasChildNodes and (index < mynode.ChildNodes.Count - 1) then
    begin
      selNode := mynode.ChildNodes.Item[index];
      selNode.TextContent := Value;
      Result := True;
    end;
  finally

  end;
end;

function deleteChildTDOMNodeByIndex(var mynode: TDOMNode; index: integer): boolean;
var
  selNode: TDOMNode;
begin
  Result := False;
  try
    if (mynode.hasChildNodes) and (index < mynode.ChildNodes.Count - 1) then
      with mynode.ChildNodes do
        selNode := Item[index];
    selNode.Free;
    Result := True;
  finally

  end;
end;

//******************************************************************************
// attributes
function getAttributesValueList(mynode: TDOMNode;
  var attributevaluelist: TStringList): boolean;
var
  i: integer;
begin
  Result := False;
  attributevaluelist := TStringList.Create;
  for i := 0 to mynode.Attributes.Length - 1 do
  begin
    attributevaluelist.Append(mynode.Attributes[i].TextContent);
    Result := True;
  end;
end;

function getChildnodeByAttributeKeyAndValue(var mynode: TDOMNode;
  attributekey: string; attributevalue: string;
  var Value: TDOMNode): boolean;
var
  j, i: integer;
begin
  Result := False;
  try
    j := 0;
    while (mynode.hasChildNodes) and (j < mynode.ChildNodes.Count) and (Result = False) do
    begin
      if (mynode.ChildNodes.Item[j].HasAttributes) then
      begin
        for i := 0 to mynode.ChildNodes.Item[j].Attributes.Length - 1 do
        begin
          if (mynode.ChildNodes.Item[j].Attributes[i].NodeName = attributekey) and
            (mynode.ChildNodes.Item[j].Attributes[i].TextContent =
            attributevalue) then
          begin
            Value := mynode.ChildNodes.Item[j];
            Result := True;
          end;
        end;
      end;
      Inc(j);
    end;
  finally
  end;
end;

function getAttributeKeyList(mynode: TDOMNode): TStringList;
var
  i: integer;
begin
  Result := TStringList.Create;
  for i := 0 to mynode.Attributes.Length - 1 do
    Result.Append(mynode.Attributes[i].NodeName);
end;

function getNodeattributeByKey(mynode: TDOMNode; attributekey: string;
  var Value: string): boolean;
var
  i, test: integer;  // test - wg eindeutigkeit???
begin
  Result := False;
  test := 0;
  for i := 0 to mynode.Attributes.Length - 1 do
  begin
    if mynode.Attributes[i].NodeName = attributekey then
    begin
      Value := mynode.Attributes[i].TextContent;
      Inc(test);
    end;
  end;
  if (test = 1) then
    Result := True;
end;

function deleteChildAttributeByKey(var mynode: TDOMNode; attributekey: string): boolean;
var
  i: integer;
begin
  Result := False;
  if mynode.HasAttributes then
    for i := 0 to mynode.Attributes.Length - 1 do
      if mynode.Attributes[i].NodeName = attributekey then
      begin
        mynode.Attributes[i].Destroy;
        Result := True;
      end;
end;

function getChildAttributeValueTDOMNode(mynode: TDOMNode; attributekey: string;
  var attributevalue: string): boolean;
var
  i: integer;
begin
  Result := False;
  if mynode.HasAttributes then
    for i := 0 to mynode.Attributes.Length - 1 do
      if mynode.Attributes[i].NodeName = attributekey then
      begin
        attributevalue := mynode.Attributes[i].TextContent;
        Result := True;
      end;
end;

function setChildAttributeValueTDOMNode(var mynode: TDOMNode;
  attributekey: string; attributevalue: string): boolean;
var
  i: integer;
begin
  Result := False;
  if mynode.HasAttributes then
    for i := 0 to mynode.Attributes.Length - 1 do
      if mynode.Attributes[i].NodeName = attributekey then
      begin
        mynode.Attributes[i].TextContent := attributevalue;
        Result := True;
      end;
end;

//******************************************************************************

function getNodeType(mynode: TDOMNode): string;
  // ungetestet  , notwendig??
begin
  Result := '';
  try
    if (mynode.NodeType = ELEMENT_NODE) then
      Result := 'ELEMENT_NODE'
    else if (mynode.NodeType = ENTITY_NODE) then
      Result := 'ENTITY_NODE'
    else if (mynode.NodeType = DOCUMENT_NODE) then
      Result := 'DOCUMENT_NODE'
    else if (mynode.NodeType = PROCESSING_INSTRUCTION_NODE) then
      Result := 'PROCESSING_INSTRUCTION_NODE'
    else if (mynode.NodeType = ATTRIBUTE_NODE) then
      Result := 'ATTRIBUTE_NODE'
    else if (mynode.NodeType = DOCUMENT_FRAGMENT_NODE) then
      Result := 'DOCUMENT_FRAGMENT_NODE'
    else if (mynode.NodeType = ENTITY_NODE) then
      Result := 'ENTITY_REFERENCE_NODE'
    else if (mynode.NodeType = ENTITY_REFERENCE_NODE) then
      Result := 'CDATA_SECTION_NODE'
    else if (mynode.NodeType = COMMENT_NODE) then
      Result := 'COMMENT_NODE'
    else
      Result := 'nodetype unknown'
  finally

  end;
end;

function getTextEntry(mynode: TDOMNode): string;
begin
  Result := '';
  if mynode.NodeType = TEXT_NODE then
    Result := mynode.TextContent;
end;

function docElementsAsStringlist(myxdoc: TXMLDocument): TStringList;
var
  i: integer;
begin
  Result := TStringList.Create;
  i := 0;
  while i < myxdoc.ChildNodes.Count do
  begin
    if myxdoc.ChildNodes.Item[i].NodeType = 1 then
      Result.Append(myxdoc.ChildNodes.Item[i].NodeName);
    Inc(i);
  end;
end;

function getNodeElementsAsStringlist(mynode: TDOMNode): TStringList;
  // element names, child level
var
  i: integer;
begin
  Result := TStringList.Create;
  i := 0;
  while i < mynode.ChildNodes.Count do
  begin
    if mynode.ChildNodes.Item[i].NodeType = ELEMENT_NODE then
      Result.Append(mynode.ChildNodes.Item[i].NodeName);
    Inc(i);
  end;
end;

//************************************************************
// rekursiver Block für TXMLDocument, alle Elementnamen
// zum Testen
function getNodeName(mynode: TDOMNode; var valueList: TStringList): boolean;
  // recursiv
var
  i: integer;
begin
  Result := False;
  try
    if (mynode.NodeType = ELEMENT_NODE) then
      valueList.Append(mynode.NodeName)
    else if (mynode.NodeType = CDATA_SECTION_NODE) then
      valueList.Append('is CDATA_SECTION_NODE')
    else if (mynode.NodeType = ENTITY_NODE) then
      valueList.Append('is Entity Node')
    else if (mynode.NodeType = COMMENT_NODE) then
      valueList.Append('is COMMENT_NODE')
    else
      valueList.Append(getNodeType(mynode));
    Result := True;
  finally
    // ignore comments and wrong elements
  end;
  with mynode.ChildNodes do
    for i := 0 to (Count - 1) do
      if (Item[i].NodeType <> TEXT_NODE) then
        getNodeName(Item[i], valueList);
end;

function getSubNodeNames(myxdoc: TXMLDocument): TStringList;
  // done
var
  i: integer;
  myStringlist: TStringList;
begin
  myStringlist := TStringList.Create;
  Result := myStringlist;
  try
    with myxdoc.DocumentElement.ChildNodes do
      for i := 0 to (Count - 1) do
        if getNodeName(Item[i], myStringlist) then
          //if ( Item[i].NodeType= ELEMENT_NODE) then
          //  myStringlist.Add(Item[i].NodeName);
          Result := myStringlist
  finally
    //myStringList.Free;
  end;
end;

//************************************************************
// rekursiver Block für TXMLDocument, alle Element Values
// zum Testen
function getNodeValue(mynode: TDOMNode; var valueList: TStringList): boolean;
  // recursiv
var
  i: integer;
begin
  Result := False;
  try
    if (mynode.NodeType = ELEMENT_NODE) then
    begin
      if (mynode.TextContent <> '') then   //is Textnode
        valueList.Append(mynode.FirstChild.NodeValue);
    end
    else if (mynode.NodeType = CDATA_SECTION_NODE) then
      valueList.Append('is CDATA_SECTION_NODE')
    else if (mynode.NodeType = COMMENT_NODE) then
      valueList.Append('is COMMENT_NODE')
    else
      valueList.Append(getNodeType(mynode));
    Result := True;
  finally
    // ignore comments and wrong elements
  end;
  with mynode.ChildNodes do
    for i := 0 to (Count - 1) do
      if (Item[i].NodeType <> TEXT_NODE) then
        getNodeValue(Item[i], valueList);
end;

function getSubNodeValues(myxdoc: TXMLDocument): TStringList;
var
  i: integer;
  myStringlist: TStringList;

begin
  myStringlist := TStringList.Create;
  Result := myStringlist;
  with myxdoc.DocumentElement.ChildNodes do
    for i := 0 to (Count - 1) do
      try
        if getNodeValue(Item[i], myStringlist) then
          Result := myStringlist
      finally

      end;
end;

//************************************************************
function stringlistWithoutBreaks(strlist: TStringList): TStringList;
  // eliminate breaks and empty lines
var
  intI: integer;
  teststr: string;
begin
  Result := TStringList.Create;
  if strlist.Count > 0 then
    for intI := 0 to strList.Count - 1 do
    begin
      teststr := StringReplace(strlist[intI], #10, '', [rfReplaceAll]);
      if (teststr <> '') then
        Result.Add(teststr);
    end;
end;

end.
