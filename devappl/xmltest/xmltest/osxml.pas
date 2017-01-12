unit osxml;

// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: martina hammel, detlef oertel
// credits: http://www.opsi.org/credits/

//***************************************************************************
// Subversion:
// $Revision: 518 $
// $Author: hammel $
// $Date: 2016-12-06 14:15:37 +0100 (Di, 06. Dez 2016) $
//***************************************************************************


{$mode delphi}

interface

uses
  Classes, SysUtils, DOM, XMLRead, XMLWrite;

(*
type
  Topsiscriptxml = class(TXMLDocument)
  protected
    procedure create(str : String;);
    function createXmlDocFromString(str:string) : TXMLDocument;

    function nodeElementsCount(mynode : TDOMNode) : integer;
    function docElementsCount(myxdoc : TXMLDocument) : integer;
    function nodeElements(mynode : TDOMNode) : TStringlist;
    function docElements(myxdoc : TXMLDocument) : TStringlist;
    function getNodeByName(mynode : TDOMNode; name:string) : TDOMNode;
    function getNodeByIndex(mynode : TDOMNode; index:integer) : TDOMNode;
    function getTextEntry(mynode : TDOMNode) : string;
    function getAttributeList(mynode : TDOMNode) : TStringlist;
    function getAttributeKeyList(mynode : TDOMNode) : TStringlist;
    function getAttributeValueList(mynode : TDOMNode) : TStringlist;

  public
    start(list : TStringlist;);

    function xmldoc(list : TStringlist;) : Tstringlist;
    function xmlnode(list : TStringlist;) : Tstringlist;

  end;
*)
(* DOM
  ELEMENT_NODE = 1;
  ATTRIBUTE_NODE = 2;
  TEXT_NODE = 3;
  CDATA_SECTION_NODE = 4;
  ENTITY_REFERENCE_NODE = 5;
  ENTITY_NODE = 6;
  PROCESSING_INSTRUCTION_NODE = 7;
  COMMENT_NODE = 8;
  DOCUMENT_NODE = 9;
  DOCUMENT_TYPE_NODE = 10;
  DOCUMENT_FRAGMENT_NODE = 11;
  NOTATION_NODE = 12;
  *)

function createXmlDocFromStringlist(docstrlist: TStringlist): boolean;
function getXMLDocAsTStringlist(): TStringlist;
function freeXmlDoc(): boolean;
function getDocumentElementAsStringlist(docstrlist: TStringList): TStringList;
function setDocumentElementAsStringlist(var docstrlist: TStringList; docelemstrlist:TStringlist): boolean;
function getDocNodeName(docstrlist: TStringList): string;
function getDocNodeType(docstrlist: TStringList): string;
//******************************************************************************
function createXMLNodeFromString(nodestrlist: TStringList): TDOMNode;
function createStringListFromXMLNode(mynode: TDOMNode;
  var mystringlist: TStringList): boolean;
//******************************************************************************
function appendXmlNodeToDocFromStringlist(docstrlist: TStringList;
  nodestrlist: TStringList; var strList: TStringList): boolean;
function appendXmlNodeToNodeFromStringlist(var nodestrlist1:
  TStringlist; nodestrlist2: TStringlist): boolean;
//******************************************************************************
// direkte node-Stringvalue operation   TODO
function xmlAsStringlistSetNodevalue (var nodestrlist: TStringlist; value: String) :boolean;
function xmlAsStringlistGetNodevalue (nodestrlist: TStringlist; var value: String) :boolean;
function xmlAsStringlistGetNodeAttributeKeys (nodestrlist: TStringlist; var keylist: TStringlist) :boolean;
function xmlAsStringlistSetNodeattributeByKey (var nodestrlist: TStringlist; attributekey: String; value: String) :boolean;
//******************************************************************************
function getChildTDOMNodeByIndex(mynode: TDOMNode; index: integer;
  var Value: TDOMNode): boolean;
function xmlAsStringlistGetChildnodeByIndex(nodestrlist: TStringList;
  index: integer; var Value: TStringList): boolean;

function setChildTDOMNodeValueByIndex(var mynode: TDOMNode; index: integer;
  Value: string): boolean;
function xmlAsStringlistSetChildnodeValueByIndex (var mynodeAsStringlist: TStringList;
  index: integer; value: string ): boolean;

function getChildTDOMNodeValueByIndex(mynode: TDOMNode; index: integer;
  var Value: string): boolean;
function xmlAsStringlistGetChildnodeValueByIndex(mynodeAsStringlist: TStringList; index: integer;
  var Value: string): boolean;

function deleteChildTDOMNodeByIndex(var mynode: TDOMNode; index: integer): boolean;
function xmlAsStringlistDeleteChildnodeByIndex(var mynodeAsStringlist: TStringList;
  index: integer): boolean;
//******************************************************************************
function getChildTDOMNodeByName(mynode: TDOMNode; NodeName: string;
  var Value: TDOMNode): boolean;
function xmlAsStringlistGetChildnodeByName(nodestrlist: TStringList;
  Name: string; var childnodeSL: TStringList): boolean;
function setChildTDOMNodeByName(var mynode: TDOMNode; NodeName: string;
  Value: TDOMNode): boolean;
function xmlAsStringlistSetChildnodeByName(var nodestrlist: TStringList;
  NodeName: string; childnodeSL: TStringList): boolean;

function getAllNodesByName(mynode: TDOMNode; NodeName: string;
  var nodelist: TStringList): boolean; // TODO
//******************************************************************************
function getNodeElements(mynode: TDOMNode): TStringList;
function getTextEntry(mynode: TDOMNode): string;
//******************************************************************************
function getAttributesValueList(mynode: TDOMNode;
     var attributevaluelist:TStringList): boolean;
function xmlAsStringlistgetAttributesValueList(nodeAsStringlist: TStringList;
     var attributeValueList:TStringlist): boolean;
function getAttributeKeyList(mynode: TDOMNode): TStringList;
function deleteChildAttributeByKey(var mynode: TDOMNode; attributekey: string) : boolean;
function xmlAsStringlistdeleteChildAttributeByKey(var nodeAsStringlist: TStringList;
     attributekey: string) : boolean;
function getChildAttributeValueTDOMNode(mynode: TDOMNode;
  attributekey:string;var attributevalue:string): boolean;
function xmlAsStringlistGetChildAttributeValueByNameAndKey(nodeAsStringlist: TStringList;
  nodename:string; attributekey:string; var attributevalue:string) :boolean;
function setChildAttributeValueTDOMNode(var mynode: TDOMNode;
  attributekey:string; attributevalue:string): boolean;
function xmlAsStringlistSetChildAttributeValueByNameAndKey(var nodeAsStringlist: TStringList;
  nodename:string; attributekey:string; attributevalue:string) :boolean;
//******************************************************************************
function xmlAsStringlistAllElementsCount(nodeAsStringlist: TStringList): integer;
function xmlAsStringlistElementsCount(nodeAsStringlist: TStringList): integer;
function xmlAsStringlistCommentElementsCount(nodeAsStringlist: TStringList): integer;
function xmlAsStringlistHasChildNodes(nodeAsStringlist: TStringList): boolean;
function xmlAsStringlistGetChildNodes(nodeAsStringlist: TStringList;
  var childnodes: TStringList): boolean;
function isValid(str: string): boolean;      // tut nix
function getNodeType(mynode: TDOMNode): string;
//******************************************************************************
function getNodeName(mynode: TDOMNode; var valueList: TStringList): boolean;
function getSubNodeNames(myxdoc: TXMLDocument): TStringList;
function getNodeValue(mynode: TDOMNode; var valueList: TStringList): boolean;
function getSubNodeValues(myxdoc: TXMLDocument): TStringList;
//******************************************************************************
// Helper
function stringlistWithoutBreaks(strlist: TStringList): TStringList;

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
function getXMLDocAsTStringlist(): TStringlist;
var nodeStream: TMemoryStream;
    mystringlist: TStringList;
begin
  result:=NIL;
  if XML<>NIL then
    try
      mystringlist := TStringList.Create;
      nodeStream := TMemoryStream.Create;
      WriteXML(XML, nodestream);
      nodeStream.Position := 0;
      mystringlist.LoadFromStream(nodestream);
      Result := mystringlist;
    finally
      nodestream.Free;
    end;
end;
function setDocumentElementAsStringlist(var docstrlist: TStringList; docelemstrlist:TStringlist): boolean;
var mynode: TDOMNode;
begin
  result:=false;
  try
    mynode:= createXMLNodeFromString(docelemstrlist);
    XML.RemoveChild(XML.FirstChild);
    XML.AppendChild(mynode);
    docstrlist:= getXMLDocAsTStringlist();
    result:=true;
  finally
    mynode.Free;
  end;
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
//*****************************************************************************
function getDocumentElementAsStringlist(docstrlist: TStringList): TStringList;
var
  nodeStream: TMemoryStream;
  mystringlist: TStringList;
  mynode: TDOMNode;
begin
  mystringlist := TStringList.Create;
  nodeStream := TMemoryStream.Create;
  if createXmlDocFromStringlist(docstrlist) then
    begin
      mynode := XML.DocumentElement;
      try
        WriteXML(mynode, nodeStream);
        nodeStream.Position := 0;
        mystringlist.LoadFromStream(nodestream);
        Result := mystringlist;
      finally
        nodestream.Free;
        mynode.free;
        //mystringlist.Free;       muss bleiben, damit result bleibt
      end;
    end;
end;

function getDocNodeName(docstrlist: TStringList): string;
begin
  Result:='';
  if createXmlDocFromStringlist(docstrlist) then
    Result := XML.DocumentElement.NodeName;
end;

function getDocNodeType(docstrlist: TStringList): string;
begin
  Result:='';
  if createXmlDocFromStringlist(docstrlist) then
    Result := getNodeType(XML.DocumentElement);
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
    mynode := XML.CreateElement('opsibase');
    ReadXMLFragment(mynode, nodestream);
    Result := mynode.FirstChild;
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
//************************************************************
function appendXmlNodeToDocFromStringlist(docstrlist: TStringList;
  nodestrlist: TStringList; var strList: TStringList): boolean;
  // done -- erneutes erzeugen des XML-Dokuments????
var
  docstream: TMemoryStream;
  mynode: TDOMNode;
begin
  Result := False;
  if createXmlDocFromStringlist(docstrlist) then
    begin
      docstream := TMemoryStream.Create;
      try
        mynode:=createXMLNodeFromString(nodestrlist);
        XML.DocumentElement.AppendChild(mynode);
        WriteXMLFile(XML, docstream);
        docstream.Position := 0;
        strList := TStringList.Create;
        strList.LoadFromStream(docstream);
        Result := True;
      finally
        docstream.Free;
      end;
    end;
end;

// TODO
function appendXmlNodeToNodeFromStringlist(var nodestrlist1: TStringlist;
  nodestrlist2: TStringlist): boolean;
var mynode1,mynode2:TDOMNode;

begin
  Result:= false;
  try
    mynode1 := createXMLNodeFromString(nodestrlist1);
    mynode2 := createXMLNodeFromString(nodestrlist2);
    mynode1.AppendChild(mynode2);
    createStringListFromXMLNode(mynode1, nodestrlist1);
    result:=true;
  except
    //
  end;

end;
//************************************************************
// direkte node operation

function xmlAsStringlistSetNodevalue (var nodestrlist: TStringlist; value: String) :boolean;
var mynode: TDOMNode;
begin
  result:=false;
  try
    mynode := createXMLNodeFromString(nodestrlist);
    mynode.TextContent:=value;
    // nodestrlist.clear?
    if createStringListFromXMLNode(mynode, nodestrlist) then
      result:=true;
  except
    //
  end;
end;
function xmlAsStringlistGetNodevalue (nodestrlist: TStringlist; var value: String) :boolean;
var mynode: TDOMNode;
begin
  result:=false;
  try
    mynode := createXMLNodeFromString(nodestrlist);
    value:=mynode.TextContent;
    result:=true;
  except
    //
  end;
end;
function xmlAsStringlistGetNodeAttributeKeys (nodestrlist: TStringlist; var keylist: TStringlist) :boolean;
var mynode: TDOMNode;
begin
  result:=false;
  keylist:= TStringlist.Create;
  try
     mynode := createXMLNodeFromString(nodestrlist);
     keylist:= getAttributeKeylist(mynode);
     result:=true;
  except
    //
  end;
end;
function xmlAsStringlistSetNodeattributeByKey (var nodestrlist: TStringlist; attributekey: String; value: String) :boolean;
// TODO
var mynode: TDOMNode;
begin

end;

//************************************************************
// alle über index
function getChildTDOMNodeByIndex(mynode: TDOMNode; index: integer;
  var Value: TDOMNode): boolean;
begin
  Result := false;
  if mynode.hasChildNodes AND (index<mynode.ChildNodes.Count-1) then
  begin
    value := mynode.ChildNodes.Item[index];
    result:=true;
  end;
end;
function xmlAsStringlistGetChildnodeByIndex(nodestrlist: TStringList;
  index: integer; var Value: TStringList): boolean;
var mynode,newnode:TDOMNode;
begin
  result:=false;
  mynode := createXMLNodeFromString(nodestrlist);
  if getChildTDOMNodeByIndex(mynode, index, newnode) then
    if createStringListFromXMLNode(newnode, Value) then
    begin
      Result := True;
    end;
end;

function getChildTDOMNodeValueByIndex(mynode: TDOMNode; index: integer;
  var Value: string): boolean;
begin
  Result := False;
  try
    if mynode.hasChildNodes AND (index<mynode.ChildNodes.Count-1) then
    begin
      with mynode.ChildNodes do
        Value:= Item[index].TextContent;
      Result := True;
    end;
  finally
    //
  end;
end;

function xmlAsStringlistGetChildnodeValueByIndex(mynodeAsStringlist: TStringList; index: integer;
  var Value: string): boolean;
// TODO
var
  mynode: TDOMNode;
begin
  result:=false;
  mynode:= createXMLNodeFromString(mynodeAsStringlist);
  if getChildTDOMNodeValueByIndex(mynode,index, value) then
    result:=true;
end;

function setChildTDOMNodeValueByIndex(var mynode: TDOMNode; index: integer;
  value: string): boolean;
var
  selNode: TDOMNode;
begin
  Result := False;
  try
    if mynode.hasChildNodes AND (index<mynode.ChildNodes.Count-1)then
    begin
      selNode := mynode.ChildNodes.Item[index];
      selNode.TextContent := Value;
      Result := True;
    end;
  finally

  end;
end;
function xmlAsStringlistSetChildnodeValueByIndex (var mynodeAsStringlist: TStringList;
  index: integer; value: string ): boolean;
var
  mynode: TDOMNode;
begin
  result:=false;
  mynode := createXMLNodeFromString(mynodeAsStringlist);
  if setChildTDOMNodeValueByIndex(mynode, index, value) then
    if createStringListFromXMLNode(mynode, mynodeAsStringlist) then
    begin
      Result := True;
    end;
end;

function deleteChildTDOMNodeByIndex(var mynode: TDOMNode; index: integer): boolean;
var
  selNode: TDOMNode;
begin
  Result := False;
  try
    if (mynode.hasChildNodes) AND (index<mynode.ChildNodes.Count-1) then
      with mynode.ChildNodes do
        selNode := Item[index];
    selNode.Free;
    Result := True;
  finally

  end;
end;

function xmlAsStringlistDeleteChildnodeByIndex(var mynodeAsStringlist: TStringList;
  index: integer): boolean;
var
  mynode: TDOMNode;
begin
  Result := False;
  mynode := createXMLNodeFromString(mynodeAsStringlist);
  if deleteChildTDOMNodeByIndex(mynode, index) then
    if createStringListFromXMLNode(mynode, mynodeAsStringlist) then
    begin
      Result := True;
    end;
end;

//************************************************************
// alle über name
function getChildTDOMNodeByName(mynode: TDOMNode; NodeName: string; var Value: TDOMNode): boolean;
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

function xmlAsStringlistGetChildnodeByName(nodestrlist: TStringList;
  Name: string; var childnodeSL: TStringList): boolean; ///
var
  mynode, newnode: TDOMNode;
begin
  Result := False;
  mynode := createXMLNodeFromString(nodestrlist);
  if getChildTDOMNodeByName(mynode, Name, newnode) then
    if createStringListFromXMLNode(newnode, childnodeSL) then
    begin
      Result := True;
    end;
end;
function setChildTDOMNodeByName(var mynode: TDOMNode; NodeName: string; Value: TDOMNode): boolean;
var i: integer;
    selNode: TDOMNode;
begin
  Result := False;
  i := 0;
  if (mynode <> nil) then
    while  (mynode.hasChildNodes) AND (i < mynode.ChildNodes.Count) do
    begin
      if (NodeName = mynode.ChildNodes.Item[i].NodeName)  AND (Result=false) then
      begin
        selNode := mynode.ChildNodes.Item[i];
        selNode.free;   // remove
        mynode.AppendChild(Value); // append new
        Result := True;
      end;
      Inc(i);
    end;
end;
function xmlAsStringlistSetChildnodeByName(var nodestrlist: TStringList;
  NodeName: string; childnodeSL: TStringList): boolean;
var
  mynode, childnode: TDOMNode;

begin
  Result := False;
  mynode := createXMLNodeFromString(nodestrlist);
  childnode:= createXMLNodeFromString(childnodeSL);
  if setChildTDOMNodeByName(mynode, NodeName, childnode) then
    if createStringListFromXMLNode(mynode, nodestrlist) then
    begin
      Result := True;
    end;

end;

function getAllNodesByName(mynode: TDOMNode; NodeName: string;
  var nodelist: TStringList): boolean; // TODO
begin

end;

//************************************************************

function getTextEntry(mynode: TDOMNode): string;
begin
  Result := '';
  if mynode.NodeType = TEXT_NODE then
    Result := mynode.TextContent;
end;

function docElements(myxdoc: TXMLDocument): TStringList;
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

function getNodeElements(mynode: TDOMNode): TStringList;
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
// attributes
function getAttributesValueList(mynode: TDOMNode;
     var attributevaluelist:TStringList): boolean;
var
  i: integer;
begin
  Result := false;
  attributevaluelist:= TStringList.Create;
  for i := 0 to mynode.Attributes.Length - 1 do
  begin
    attributevaluelist.Append(mynode.Attributes[i].TextContent);
    result:=true;
  end;
end;
function xmlAsStringlistGetAttributesValueList(nodeAsStringlist: TStringList;
     var attributeValueList:TStringlist): boolean;
var mynode, newnode: TDOMNode;
begin
  result:=false;
  mynode := createXMLNodeFromString(nodeAsStringlist);
  if getAttributesValueList(mynode,attributeValueList) then
    begin
      result:=true;
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
function deleteChildAttributeByKey(var mynode: TDOMNode; attributekey: string) : boolean;
var
  i:integer;
begin
  result:=false;
  if mynode.HasAttributes then
    for i := 0 to mynode.Attributes.Length - 1 do
      if mynode.Attributes[i].NodeName=attributekey then
        begin
          mynode.Attributes[i].Destroy;
          result:=true;
        end;
end;
function xmlAsStringlistdeleteChildAttributeByKey(var nodeAsStringlist: TStringList;
     attributekey: string) : boolean;
var mynode: TDOMNode;
begin
  result:=false;
  mynode := createXMLNodeFromString(nodeAsStringlist);
  if deleteChildAttributeByKey(mynode, attributekey) then
    if createStringListFromXMLNode(mynode,nodeAsStringlist) then
      result:= true;
end;

function getChildAttributeValueTDOMNode(mynode: TDOMNode;
  attributekey:string; var attributevalue:string): boolean;
var
  i: integer;
begin
  Result := false;
  if mynode.HasAttributes then
    for i := 0 to mynode.Attributes.Length - 1 do
      if mynode.Attributes[i].NodeName=attributekey then
        begin
          attributevalue:=mynode.Attributes[i].TextContent;
          result:=true;
        end;
end;

function xmlAsStringlistGetChildAttributeValueByNameAndKey(nodeAsStringlist: TStringList;
  nodename:string; attributekey:string; var attributevalue:string) :boolean;
var mynode, newnode: TDOMNode;
begin
  result:=false;
  mynode := createXMLNodeFromString(nodeAsStringlist);
  if mynode.HasChildNodes then
    if getChildTDOMNodeByName(mynode,nodename,newnode) then
      if getChildAttributeValueTDOMNode(newnode,attributekey,attributevalue) then
        begin
          result:=true;
        end;
end;

function setChildAttributeValueTDOMNode(var mynode: TDOMNode;
  attributekey:string; attributevalue:string): boolean;
var
  i: integer;
begin
  Result := false;
  if mynode.HasAttributes then
    for i := 0 to mynode.Attributes.Length - 1 do
      if mynode.Attributes[i].NodeName=attributekey then
        begin
          mynode.Attributes[i].TextContent:= attributevalue;
          result:=true;
        end;
end;

function xmlAsStringlistSetChildAttributeValueByNameAndKey(var nodeAsStringlist: TStringList;
  nodename:string; attributekey:string; attributevalue:string) :boolean;
var mynode, newnode: TDOMNode;
begin
  result:=false;
  mynode := createXMLNodeFromString(nodeAsStringlist);
  if mynode.HasChildNodes then
    if getChildTDOMNodeByName(mynode,nodename,newnode) then
      if setChildAttributeValueTDOMNode(newnode,attributekey,attributevalue) then
         if createStringListFromXMLNode(mynode,nodeAsStringlist) then
            result:=true;
end;
//************************************************************
function xmlAsStringlistAllElementsCount(nodeAsStringlist: TStringList): integer;
var
  mynode: TDOMNode;
  teststrlst: TStringList;
  teststr: string;
begin
  Result := 0;
  teststrlst:= TStringlist.Create;
  try
    mynode := createXmlNodeFromString(nodeAsStringlist);
    if createStringListFromXMLNode(mynode, teststrlst) then
      teststr := teststrlst.Text;
    Result := mynode.ChildNodes.Count;
  finally
    mynode.Free;
  end;
end;

function xmlAsStringlistElementsCount(nodeAsStringlist: TStringList): integer;
  //  only elements, no comments
var
  mynode: TDOMNode;
  i: integer;
begin
  Result := 0;
  try
    mynode := createXmlNodeFromString(nodeAsStringlist);
    with mynode.ChildNodes do
      for i := 0 to (Count - 1) do
        if (Item[i].NodeType <> COMMENT_NODE) then
          Result := Result + 1;
  finally
    mynode.Free;
  end;
end;

function xmlAsStringlistCommentElementsCount(nodeAsStringlist: TStringList): integer;
  //  only comments
var
  mynode: TDOMNode;
  i: integer;
begin
  Result := 0;
  try
    mynode := createXmlNodeFromString(nodeAsStringlist);
    with mynode.ChildNodes do
      for i := 0 to (Count - 1) do
        if (Item[i].NodeType = COMMENT_NODE) then
          Result := Result + 1;
  finally
    mynode.Free;
  end;
end;

function xmlAsStringlistHasChildNodes(nodeAsStringlist: TStringList): boolean;
var
  mynode: TDOMNode;
begin
  Result := False;
  try
    mynode := createXmlNodeFromString(nodeAsStringlist);
    if mynode.hasChildNodes then
        Result := True;
  finally
    mynode.Free;
  end;
end;

function xmlAsStringlistGetChildNodes(nodeAsStringlist: TStringList;
  var childnodes: TStringList): boolean;
var
  mynode: TDOMNode;
  i: integer;
  tempstrlist: TStringList;
begin
  Result := False;
  childnodes := TStringList.Create;
  try
    mynode := createXmlNodeFromString(nodeAsStringlist);
    with mynode.ChildNodes do
      for i := 0 to (Count - 1) do
        if createStringListFromXMLNode(Item[i], tempstrlist) then
        begin
          if tempstrlist.Count>0 then
            childnodes.Add(stringlistWithoutBreaks(tempstrlist).Text);
          tempstrlist.Clear;
        end;
    Result := True;
  finally
    //
  end;
end;

function isValid(str: string): boolean;
  // TODO  - iss noch nix
var
  Parser: TDOMParser;
  Src: TXMLInputSource;
  TheDoc: TXMLDocument;
begin
  try
    Result := False;
    Parser := TDOMParser.Create;
    Src := TXMLInputSource.Create(str);
    Parser.Options.Validate := True;
    // Festlegen einer Methode, die bei Fehlern aufgerufen wird
    // Parser.OnError := @ErrorHandler;
    Parser.Parse(Src, TheDoc);
  finally
    Src.Free;
    Parser.Free;
  end;
end;

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
