unit osxml;

// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: martina hammel, detlef oertel


{$mode delphi}

interface

uses
  {$IFDEF OPSISCRIPT}
  oslog,
  {$ENDIF OPSISCRIPT}
  Classes, SysUtils, DOM, osxmltdom, XMLRead, XMLWrite;

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


function getXMLDocAsTStringlist(): TStringList;
function getDocumentElementAsStringlist(docstrlist: TStringList): TStringList;
function getXMLDocumentElementfromFile(filename: string): TStringList;
function setDocumentElementAsStringlist(var docstrlist: TStringList;
  docelemstrlist: TStringList): boolean;
function getDocNodeNameFromStringList(docstrlist: TStringList): string;
function getDocNodeNameFromXMLDoc(): string;
function getDocNodeType(docstrlist: TStringList): string;
function getXmlDeclarationFromStringList(docstrlist: TStringList): TStringList;

//******************************************************************************
function appendXmlNodeToDocFromStringlist(docstrlist: TStringList;
  nodestrlist: TStringList; var strList: TStringList): boolean;
function appendXmlNodeToNodeFromStringlist(var nodestrlist1: TStringList;
  nodestrlist2: TStringList): boolean;
//******************************************************************************
// direkte node-Stringvalue operation   TODO
function xmlAsStringlistSetNodevalue(var nodestrlist: TStringList;
  Value: string): boolean;
function xmlAsStringlistGetNodevalue(nodestrlist: TStringList;
  var Value: string): boolean;
function xmlAsStringlistGetNodeAttributeKeys(nodestrlist: TStringList;
  var keylist: TStringList): boolean;
function xmlAsStringlistSetNodeattributeByKey(var nodestrlist: TStringList;
  attributekey: string; Value: string): boolean;
//******************************************************************************
// by index
function xmlAsStringlistGetChildnodeByIndex(nodestrlist: TStringList;
  index: integer; var Value: TStringList): boolean;
function xmlAsStringlistSetChildnodeValueByIndex(var mynodeAsStringlist: TStringList;
  index: integer; Value: string): boolean;
function xmlAsStringlistGetChildnodeValueByIndex(mynodeAsStringlist: TStringList;
  index: integer; var Value: string): boolean;
function xmlAsStringlistDeleteChildnodeByIndex(var mynodeAsStringlist: TStringList;
  index: integer): boolean;
//******************************************************************************
// by name
function xml2GetFirstChildNodeByName(nodestrlist: TStringList;
  Name: string; var childnodeSL: TStringList): boolean; ///

function xmlAsStringlistGetUniqueChildnodeByName(nodestrlist: TStringList;
  Name: string; var childnodeSL: TStringList): boolean;
function xmlAsStringlistReplaceUniqueChildnodeByName(var nodestrlist: TStringList;
  NodeName: string; childnodeSL: TStringList): boolean;

//******************************************************************************
// attributes
function getXml2AttributeValueByKey(nodeAsStringlist: TStringList;
  attributekey: string; var attributevalue: string): boolean;

function xmlAsStringlistgetAttributesValueList(nodeAsStringlist: TStringList;
  var attributeValueList: TStringList): boolean;
function xmlAsStringlistdeleteChildAttributeByKey(var nodeAsStringlist: TStringList;
  attributekey: string): boolean;
function xmlAsStringlistGetChildAttributeValueByNameAndKey(nodeAsStringlist: TStringList;
  nodename: string; attributekey: string; var attributevalue: string): boolean;
function xmlAsStringlistGetChildnodeByNameAndAttributeKeyAndValue(
  nodeAsStringlist: TStringList; nodename: string; attributekey: string;
  attributevalue: string; var Value: TStringList): boolean;
function xmlAsStringlistReplaceChildnodeByNameAndAttributeKeyAndValue(
  var nodeAsStringlist: TStringList; nodename: string; attributekey: string;
  attributevalue: string; Value: TStringList): boolean;
function xmlAsStringlistSetChildAttributeValueByNameAndKey(
  var nodeAsStringlist: TStringList; nodename: string; attributekey: string;
  attributevalue: string): boolean;
//******************************************************************************
function xmlAsStringlistAllElementsCount(nodeAsStringlist: TStringList): integer;
function xmlAsStringlistElementsCount(nodeAsStringlist: TStringList): integer;
function xmlAsStringlistCommentElementsCount(nodeAsStringlist: TStringList): integer;
function xmlAsStringlistHasChildNodes(nodeAsStringlist: TStringList): boolean;
function xmlAsStringlistGetChildNodes(nodeAsStringlist: TStringList;
  var childnodes: TStringList): boolean;
function isValid(str: string): boolean;      // tut nix

//******************************************************************************
// text
function getXml2Text(nodeAsStringlist: TStringList; var mytext: string): boolean;


implementation
//var XMLasStringList: TStringList;


function getXMLDocAsTStringlist(): TStringList;
var
  nodeStream: TMemoryStream;
  mystringlist: TStringList;
begin
  Result := nil;
  if XMLexists then
    try
      mystringlist := TStringList.Create;
      nodeStream := TMemoryStream.Create;
      WriteXML(getXMLDoc, nodestream);
      nodeStream.Position := 0;
      mystringlist.LoadFromStream(nodestream);
      Result := mystringlist;
    finally
      nodestream.Free;
    end;
end;

function setDocumentElementAsStringlist(var docstrlist: TStringList;
  docelemstrlist: TStringList): boolean;
var
  mynode: TDOMNode;
begin
  Result := False;
  try
    mynode := createXMLNodeFromString(docelemstrlist);
    getXMLDoc.RemoveChild(getXMLDoc.FirstChild);
    getXMLDoc.AppendChild(mynode);
    docstrlist := getXMLDocAsTStringlist();
    Result := True;
  finally
    mynode.Free;
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
    mynode := getXMLDoc.DocumentElement;
    try
      WriteXML(mynode, nodeStream);
      nodeStream.Position := 0;
      mystringlist.LoadFromStream(nodestream);
      Result := mystringlist;
    finally
      nodestream.Free;
      mynode.Free;
      //mystringlist.Free;       muss bleiben, damit result bleibt
    end;
  end;
end;

function getXMLDocumentElementfromFile(filename: string): TStringList;
var
  nodeStream: TMemoryStream;
  //mystringlist: TStringList;
  mynode: TDOMNode;
begin
  Result := TStringList.Create;
  nodeStream := TMemoryStream.Create;
  mynode := nil;
  try
    try
      if createXmlDocFromFile(filename) then
      begin
        mynode := getXMLDoc.DocumentElement;

        WriteXML(mynode, nodeStream);
        nodeStream.Position := 0;
        Result.LoadFromStream(nodestream);
        //Result := mystringlist;
      end;
    except
      on E: Exception do
      begin
        {$IFDEF OPSISCRIPT}
        LogDatei.log('Exception in getXMLDocumentElementfromFile', LLError);
        LogDatei.log('Error: Message: ' + E.message, LLError);
        {$ENDIF OPSISCRIPT}
      end;
    end;
  finally
    nodestream.Free;
    if Assigned(mynode) then
      mynode.Free;
  end;
end;


function getXmlDeclarationFromStringList(docstrlist: TStringList): TStringList;
begin
  Result := TStringList.Create;
  if createXmlDocFromStringlist(docstrlist) then
  begin
    Result.Add('version=' + getXMLDoc.XMLVersion);
    Result.Add('encoding=' + getXMLDoc.XMLEncoding);
    Result.Add('standalone=' + boolToStr(getXMLDoc.XMLStandalone, True));
  end;
  freeXmlDoc();
end;


function getDocNodeNameFromStringList(docstrlist: TStringList): string;
begin
  if createXmlDocFromStringlist(docstrlist) then
    if XMLexists then
      Result := getDocNodeName()
    else
      Result := '';
  freeXmlDoc();
end;

function getDocNodeNameFromXMLDoc(): string;
begin
  if XMLexists then
    Result := getDocNodeName()
  else
    Result := '';
end;

function getDocNodeType(docstrlist: TStringList): string;
begin
  Result := '';
  if createXmlDocFromStringlist(docstrlist) then
    Result := getNodeType(getXMLDoc.DocumentElement);
  freeXmlDoc();
end;

//*****************************************************************************
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
      mynode := createXMLNodeFromString(nodestrlist);
      getXMLDoc.DocumentElement.AppendChild(mynode);
      WriteXMLFile(getXMLDoc, docstream);
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
function appendXmlNodeToNodeFromStringlist(var nodestrlist1: TStringList;
  nodestrlist2: TStringList): boolean;
var
  mynode1, mynode2: TDOMNode;

begin
  Result := False;
  try
    mynode1 := createXMLNodeFromString(nodestrlist1);
    mynode2 := createXMLNodeFromString(nodestrlist2);
    mynode1.AppendChild(mynode2);
    createStringListFromXMLNode(mynode1, nodestrlist1);
    Result := True;
  except

  end;
  mynode1.Free;
  mynode2.Free;
end;
//************************************************************
// direkte node operation

function xmlAsStringlistSetNodevalue(var nodestrlist: TStringList;
  Value: string): boolean;
var
  mynode: TDOMNode;
begin
  Result := False;
  try
    mynode := createXMLNodeFromString(nodestrlist);
    mynode.TextContent := Value;
    // nodestrlist.clear?
    if createStringListFromXMLNode(mynode, nodestrlist) then
      Result := True;
  except

  end;
  mynode.Free;
end;

function xmlAsStringlistGetNodevalue(nodestrlist: TStringList;
  var Value: string): boolean;
var
  mynode: TDOMNode;
begin
  Result := False;
  try
    mynode := createXMLNodeFromString(nodestrlist);
    Value := mynode.TextContent;
    Result := True;
  except

  end;
  mynode.Free;
end;

function xmlAsStringlistGetNodeAttributeKeys(nodestrlist: TStringList;
  var keylist: TStringList): boolean;
var
  mynode: TDOMNode;
begin
  Result := False;
  keylist := TStringList.Create;
  try
    mynode := createXMLNodeFromString(nodestrlist);
    keylist := getAttributeKeylist(mynode);
    Result := True;
  except

  end;
  mynode.Free;
end;

function xmlAsStringlistSetNodeattributeByKey(var nodestrlist: TStringList;
  attributekey: string; Value: string): boolean;
  // TODO
var
  mynode: TDOMNode;
begin
  Result := False;
end;

//************************************************************
// by index

function xmlAsStringlistGetChildnodeByIndex(nodestrlist: TStringList;
  index: integer; var Value: TStringList): boolean;
var
  mynode, newnode: TDOMNode;
begin
  Result := False;
  mynode := createXMLNodeFromString(nodestrlist);
  if getChildTDOMNodeByIndex(mynode, index, newnode) then
    if createStringListFromXMLNode(newnode, Value) then
    begin
      Result := True;
    end;
end;

function xmlAsStringlistGetChildnodeValueByIndex(mynodeAsStringlist: TStringList;
  index: integer; var Value: string): boolean;
  // TODO
var
  mynode: TDOMNode;
begin
  Result := False;
  mynode := createXMLNodeFromString(mynodeAsStringlist);
  if getChildTDOMNodeValueByIndex(mynode, index, Value) then
    Result := True;
end;

function xmlAsStringlistSetChildnodeValueByIndex(var mynodeAsStringlist: TStringList;
  index: integer; Value: string): boolean;
var
  mynode: TDOMNode;
begin
  Result := False;
  mynode := createXMLNodeFromString(mynodeAsStringlist);
  if setChildTDOMNodeValueByIndex(mynode, index, Value) then
    if createStringListFromXMLNode(mynode, mynodeAsStringlist) then
    begin
      Result := True;
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
// by name

function xml2GetFirstChildNodeByName(nodestrlist: TStringList;
  Name: string; var childnodeSL: TStringList): boolean; ///
var
  mynode, newnode, searchnode: TDOMNode;

  function findnodeInList(nodelist: TDOMNodeList; Name: string): TDOMNode;
  var
    i, childcount: integer;
    str: string;
    aktnode: TDomNode;
    haschild: boolean;
  begin
    i := 0;
    Result := nil;
    childcount := nodelist.Count;
    while (Result = nil) and (i < childcount) do
    begin
      aktnode := nodelist.Item[i];
      haschild := aktnode.HasChildNodes and (aktnode.ChildNodes.Count > 0);
      str := nodelist.Item[i].NodeName;
      Result := nodelist.Item[i].FindNode(Name);
      if (Result = nil) and haschild then
        Result := findnodeInList(nodelist.Item[i].ChildNodes, Name);
      Inc(i);
    end;
  end;

begin
  try
    Result := False;
    mynode := createXMLNodeFromString(nodestrlist);
    if mynode <> nil then
    begin
      if mynode.NodeName = Name then
        newnode := mynode
      else
      begin
        newnode := mynode.FindNode(Name);
        if newnode = nil then
          newnode := findnodeInList(mynode.ChildNodes, Name);
      end;
      if newnode <> nil then
        if createStringListFromXMLNode(newnode, childnodeSL) then
        begin
          Result := True;
          newnode.Free;
        end;
      if Assigned(mynode) then
        mynode.Free;
      Result := True;
    end;
  except
    on e: Exception do
    begin
      Result := False;
      {$IFDEF OPSISCRIPT}
      LogDatei.log('Exception in xml2GetFirstChildNodeByName: ' + e.message, LLError);
      {$ENDIF OPSISCRIPT}
    end;
  end;
end;


function xmlAsStringlistGetUniqueChildnodeByName(nodestrlist: TStringList;
  Name: string; var childnodeSL: TStringList): boolean; ///
var
  mynode, newnode: TDOMNode;
begin
  Result := False;
  mynode := createXMLNodeFromString(nodestrlist);
  if getUniqueChildNodeByName(mynode, Name, newnode) then
    if createStringListFromXMLNode(newnode, childnodeSL) then
    begin
      Result := True;
      newnode.Free;
    end;
  mynode.Free;
end;

function xmlAsStringlistReplaceUniqueChildnodeByName(var nodestrlist: TStringList;
  NodeName: string; childnodeSL: TStringList): boolean;
var
  mynode, childnode: TDOMNode;
begin
  Result := False;
  mynode := createXMLNodeFromString(nodestrlist);
  childnode := createXMLNodeFromString(childnodeSL);
  if replaceUniqueChildNodeByName(mynode, NodeName, childnode) then
    if createStringListFromXMLNode(mynode, nodestrlist) then
    begin
      Result := True;
    end;
  mynode.Free;
  childnode.Free;
end;

//************************************************************
// attributes

function getXml2AttributeValueByKey(nodeAsStringlist: TStringList;
  attributekey: string; var attributevalue: string): boolean;
var
  mynode, newnode: TDOMNode;
begin
  try
    Result := False;
    mynode := createXMLNodeFromString(nodeAsStringlist);
    if mynode <> nil then
    begin
      if getNodeattributeByKey(mynode, attributekey, attributevalue) then
        Result := True;
      mynode.Free;
      Result := True;
    end
    else
    begin
    {$IFDEF OPSISCRIPT}
      LogDatei.log('Error: Empty xmlnode input in getXml2AttributeValueByKey.', LLError);
    {$ENDIF OPSISCRIPT}
    end;
  except
    on e: Exception do
    begin
      Result := False;
      {$IFDEF OPSISCRIPT}
      LogDatei.log('Exception in getXml2AttributeValueByKey: ' + e.message, LLError);
      {$ENDIF OPSISCRIPT}
    end;
  end;
end;

function xmlAsStringlistGetAttributesValueList(nodeAsStringlist: TStringList;
  var attributeValueList: TStringList): boolean;
var
  mynode: TDOMNode;
begin
  Result := False;
  mynode := createXMLNodeFromString(nodeAsStringlist);
  if getAttributesValueList(mynode, attributeValueList) then
  begin
    Result := True;
  end;
  mynode.Free;
end;

function xmlAsStringlistGetChildnodeByNameAndAttributeKeyAndValue(
  nodeAsStringlist: TStringList; nodename: string; attributekey: string;
  attributevalue: string; var Value: TStringList): boolean;
var
  mynode, newnode: TDOMNode;
begin
  try
    try
      newnode := nil;
      Result := False;
      mynode := createXMLNodeFromString(nodeAsStringlist);
      if selectAllNodesByName(mynode, nodename) then
      begin
        if getChildnodeByAttributeKeyAndValue(mynode, attributekey,
          attributevalue, newnode) then
          if createStringListFromXMLNode(newnode, Value) then
            Result := True;
      end;
    except
      on e: Exception do
      begin
        Result := False;
        {$IFDEF OPSISCRIPT}
        LogDatei.log('Exception in xmlAsStringlistGetChildnodeByNameAndAttributeKeyAndValue: '
          + e.message, LLError);
        {$ENDIF OPSISCRIPT}
      end;
    end;
  finally
    if (newnode <> nil) then
      newnode.Free;
    mynode.Free;
  end;
end;

function xmlAsStringlistReplaceChildnodeByNameAndAttributeKeyAndValue(
  var nodeAsStringlist: TStringList; nodename: string; attributekey: string;
  attributevalue: string; Value: TStringList): boolean;
var
  mynode, newnode: TDOMNode;
begin
  Result := False;
  mynode := createXMLNodeFromString(nodeAsStringlist);
  newnode := createXMLNodeFromString(Value);
  if replaceChildnodeByNameAndAttributeKeyAndValue(mynode, nodename,
    attributekey, attributevalue, newnode) then
    if createStringListFromXMLNode(mynode, nodeAsStringlist) then
      Result := True;
  if (newnode <> nil) then
    newnode.Free;
  mynode.Free;
end;

function xmlAsStringlistdeleteChildAttributeByKey(var nodeAsStringlist: TStringList;
  attributekey: string): boolean;
var
  mynode: TDOMNode;
begin
  Result := False;
  mynode := createXMLNodeFromString(nodeAsStringlist);
  if deleteChildAttributeByKey(mynode, attributekey) then
    if createStringListFromXMLNode(mynode, nodeAsStringlist) then
      Result := True;
  mynode.Free;
end;

function xmlAsStringlistGetChildAttributeValueByNameAndKey(nodeAsStringlist: TStringList;
  nodename: string; attributekey: string; var attributevalue: string): boolean;
var
  mynode, newnode: TDOMNode;
begin
  Result := False;
  mynode := createXMLNodeFromString(nodeAsStringlist);
  if mynode.HasChildNodes then
    if getUniqueChildNodeByName(mynode, nodename, newnode) then
      if getChildAttributeValueTDOMNode(newnode, attributekey, attributevalue) then
      begin
        Result := True;
        newnode.Free;
      end;
  mynode.Free;
end;

function xmlAsStringlistSetChildAttributeValueByNameAndKey(
  var nodeAsStringlist: TStringList; nodename: string; attributekey: string;
  attributevalue: string): boolean;
var
  mynode, newnode: TDOMNode;
begin
  Result := False;
  mynode := createXMLNodeFromString(nodeAsStringlist);
  if mynode.HasChildNodes then
    if getUniqueChildNodeByName(mynode, nodename, newnode) then
      if setChildAttributeValueTDOMNode(newnode, attributekey, attributevalue) then
        if createStringListFromXMLNode(mynode, nodeAsStringlist) then
        begin
          Result := True;
          newnode.Free;
        end;
  mynode.Free;
end;
//************************************************************
function xmlAsStringlistAllElementsCount(nodeAsStringlist: TStringList): integer;
var
  mynode: TDOMNode;
  teststrlst: TStringList;
begin
  Result := 0;
  teststrlst := TStringList.Create;
  try
    mynode := createXmlNodeFromString(nodeAsStringlist);
    if createStringListFromXMLNode(mynode, teststrlst) then
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
          if tempstrlist.Count > 0 then
            childnodes.Add(stringlistWithoutBreaks(tempstrlist).Text);
          tempstrlist.Clear;
        end;
    Result := True;
    mynode.Free;
  finally

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

//******************************************************************************
// text

function getXml2Text(nodeAsStringlist: TStringList; var mytext: string): boolean;
var
  mynode: TDOMNode;
  childlist: TDOMNodeList;
  i: integer;
begin
  Result := False;
  mytext := '';
  mynode := createXMLNodeFromString(nodeAsStringlist);
  try
    childlist := mynode.ChildNodes;
    for i := 0 to childlist.Count - 1 do
      if childlist.Item[i].NodeType = TEXT_NODE then
      begin
        mytext := childlist.Item[i].TextContent;
        Result := True;
      end;
  except
  end;
  mynode.Free;
end;


end.
