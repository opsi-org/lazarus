(* Copyright (c) uib umwelt informatik buero gmbh (www.uib.de)
 This sourcecode is owned by uib
 and published under the Terms of the General Public License.
#*###CVS_HEAD_START###******************************************************
# $Id: xmlpatch.pas,v 1.1 2004/03/14 12:49:07 oertel Exp $
#
#*###EXCLUDE_CVS_LOG###*****************************************************
# History:
# ========
#
# $Log: xmlpatch.pas,v $
# Revision 1.1  2004/03/14 12:49:07  oertel
# Initial commit version 9.29
#
*)

unit xmlpatch;

interface

uses
  SysUtils, Classes, QTypes, xmldom, XMLIntf, XMLDoc,
  QDialogs, StrUtils, oxmldom, QExtCtrls;


type
 TDatamoduleXML = class(TDataModule)
    XMLDocument1: TXMLDocument;
  private
  public
    { Public-Deklarationen }
  end;



TLogCallbackProcedure = procedure(level:integer;logmsg:string) of object;

//public
// xml handling
procedure openXmlFile(filename : string);
procedure saveXmlFile(filename : string);
procedure createXmlFile(filename : string; stringlist : TStringList);
// The main idea is
// you have to 'open' a node to manipulate there
// attributes or childnodes

// The nodePath looks like this:
// nodedescription PATHSEPARATOR nodedescripton (and so on)
// nodedescription looks like this:
// nodename attributeName="attributeValue"
// (!) attributeValue should be surrounded with ""
// (!) attributeValue may contain a PATHSEPARATOR string
// nodeName may include a namespace prefix
// attributeName may include a namespace prefix
// (!) namespace prefix are case sensitiv
// a node without attributes looks like this:
// nodename =""
// PATHSEPARATOR string is '/'
// (!) no whitspaces between attributeValue and Pathseparator
// eg:
// foo bar="bar1"/childfoo =""/RDF:foo NC:bar=nonesense

procedure openNode(nodePath : string);
// open the node
// if the node not exists it will be created (also parent nodes)
// makes this node to the actual node
procedure delNode(nodePath : string);
// this node (and all childs) will be deleted
procedure setNodeAtPosition(nodeName, attributeName, attributeValue : string;
                            position : integer);
// adds a child node below the actual node at a specific position
// Remark: Postion -1 means append. This (-1, append) is used by
// openNode creating new nodes
procedure setNodeText(text : string);
// set text at the actual node
procedure setAttribute(attributeName, attributeValue : string);
// set the attribute at the actual node
// if it exists it will be changed
 // attributeName may include a namespace prefix
procedure addAttribute(attributeName, attributeValue : string);
// add the attribute at the actual node
// if it exists it will not (!) be changed
// attributeName may include a namespace prefix
procedure delAttribute(attributeName : string);
// delete the attribute  at the actual node
// attributeName may include a namespace prefix

function nodeExists(nodePath : string) : boolean;
// tells if a node exists without changing anything
function getAttributeValue(attributeName : string) : string;
// get the attribute value at the actual node
// ATTENTION: it returns an empty string:
// - if there is no attribute with this name
// - if the value of this attribute is an empty string
// attributeName may include a namespace prefix


// non xml handling procedures
procedure setLogProcedure(procName : TLogCallbackProcedure);
// tells us where to log. Needs a procedure:
// procname(debuglevel: integer; logmessage : string);
// loglevels see below
procedure setShowErrors(bValue : boolean);
// tells us if we have to show errors in a messageDialog
function getXmlStrings : TStrings;
// gives the actual xml source

// private
function getNode(var newNode:IXMLNode; parentNode:IXMLNode;
                 nodeName, attributeName, attributeValue : string):boolean;
function makeNode(var newNode:IXMLNode; parentNode:IXMLNode;
                 nodeName, attributeName, attributeValue : string):boolean;
function makeNodeAtPos(var newNode:IXMLNode; parentNode:IXMLNode;
                     nodeName, attributeName, attributeValue : string;
                     Position : integer):boolean;


var
 //XMLDocument1: TXMLDocument;
 DatamoduleXML : TDatamoduleXML;

implementation
{$R *.xfm}



const
 PATHSEPARATOR : string ='/';

var
 aktNode  :IXMLNode;
 showErrors : boolean = true;
 externLogProc : TLogCallbackProcedure;
 //XMLDocument1: TXMLDocument;

// ******* help procedures Logging + Errorhandling *************
// loglevels:
// 0 : Error
// 1 : Warn
// 2 : Info
// 3 - 5 : debug
procedure setLogProcedure(procName : TLogCallbackProcedure);
begin
 externLogProc := procName;
end;

procedure setShowErrors(bValue : boolean);
begin
 showErrors := bValue;
end;

 procedure writelog(level: integer; logmessage : string);
 begin
  //if externLogProc <> nil then
   externLogProc(level,logmessage);
  if showErrors and (level=0) then
  begin
   MessageDlg('Error: '+logmessage, mtError, [mbOK], 0, mbOK);
  end;
 end;


//*************  XML Handling ***********************************
//************* Namspace handling *******************************
procedure getNamespaceAndBasename(prefixedName :String;
                                  var namespaceUri : String;
                                  var baseName : String);
 // prefixedName may include a namespace prefix
var
 // if we hav a namespace like xmlns:NC="http://home.netscape.com/NC-rdf#"
namespacePrefix : String; // is NC
//namespaceUri : String;   // is http://home.netscape.com/NC-rdf#
attribbasename :String;  // attributname without prefix
namespace_separator : integer; // pos of prefix separator ':'
begin
 try
  namespacePrefix := '';
  namespaceUri := '';
  writelog(5,'try to get namespace from : '+prefixedName);
  namespace_separator := pos(':',prefixedName);
  if namespace_separator > 0 then
  begin
   namespacePrefix := copy(prefixedName,1,namespace_separator-1);
   namespaceUri :=
    DatamoduleXML.XMLDocument1.DocumentElement.FindNamespaceURI(namespacePrefix);
   baseName := copy(prefixedName,namespace_separator+1,length(prefixedName));
  end
  else baseName := prefixedName;
 except
  on e: Exception do writelog(0,'Error in getNamespaceAndBasename : '+e.Message);
 end;
end;

//*************  XML File-Handling ***********************************
procedure createXmlFile(filename: String; stringlist : TStringList);
begin
 try
  writelog(5,'try to create File: '+filename);
  stringlist.SaveToFile(filename);
  writelog(4,'File: '+filename+' created');
 except
  on e: Exception do writelog(0,'Error in createXmlFile : '+e.Message);
 end;
 openXmlFile(filename);
end;


//procedure TDataModuleXML.openXmlFile(filename : string);
procedure openXmlFile(filename : string);
begin
 try
  writelog(5,'try to open File: '+filename);
  //DatamoduleXML.XMLDocument1 := TXMLDocument.Create(nil);
  //DatamoduleXML.XMLDocument1.DOMVendor:= GetDOMVendor('Xerces XML');
  DatamoduleXML.XMLDocument1.DOMVendor:= GetDOMVendor('Open XML');
  writelog(5,'XMLDocument1 domvendor ');
  DatamoduleXML.XMLDocument1.ParseOptions := [];
  writelog(5,'XMLDocument1 parseoptions [] ');
  if DatamoduleXML.XMLDocument1.Active then DatamoduleXML.XMLDocument1.Active := false;
  writelog(5,'XMLDocument1 not active ');
  DatamoduleXML.XMLDocument1.LoadFromFile(FileName);
  writelog(5,'try to load File: '+filename);
  // possible values: (doNodeAutoCreate, doNodeAutoIndent, doAttrNull, doAutoPrefix, doNamespaceDecl, doAutoSave);
  // (doAttrNull=false) give empty string if attributename not exists
  DatamoduleXML.XMLDocument1.Options := [doNodeAutoIndent, doAutoPrefix, doNamespaceDecl];
  writelog(5,'XMLDocument1 options  ');
  //DatamoduleXML.XMLDocument1.Options := [];
  DatamoduleXML.XMLDocument1.Active := true;
  writelog(4,'File: '+filename+' opend');
 except
  on e: Exception do writelog(0,'Error in openXmlFile : '+e.Message);
 end;
end;

procedure formatMyXml(input : String; var output : TStrings);
 var
  endFound : boolean;
  restString, newstr : String;
  endPos : integer;
 begin
  endFound := false;
  output.Clear;
  newstr := '';
  restString := input;
  while not endFound do
  begin
   endPos := pos('>',restString);
   if endPos > 0 then
   begin
    newstr := trim(copy(restString,0,endPos));
    restString := trim(copy(restString,endPos+1, length(restString)));
    if not AnsiStartsStr('<',restString) then
    begin
     endPos := pos('>',restString);
     if endPos > 0 then
     begin
      newstr := newstr + trim(copy(restString,0,endPos));
      restString := trim(copy(restString,endPos+1, length(restString)));
     end;
    end;
    output.Add(newstr);
    newstr :='';
   end
   else endFound := true;
  end;
 end;

procedure saveXmlFile(filename : string);
var
 myFile : TextFile;
 myXml : String;
 xmlStrings : TStrings;
 i : integer;

begin
 try
  writelog(5,'try to save File: '+filename);
  //DatamoduleXML.XMLDocument1.saveToFile(FileName);
  DatamoduleXML.XMLDocument1.saveToXML(myXml);
  xmlStrings := TStringList.Create;
  formatMyXml(myXml,xmlStrings);
  AssignFile(myFile,filename);
{$IFDEF LINUX}
  SetLineBreakStyle(myFile, tlbsLF);
{$ENDIF}
  //SetLineBreakStyle(myFile, tlbsCRLF);
  rewrite(myFile);
  for i:=0 to xmlStrings.Count-1 do
   writeln(myFile,xmlStrings[i]);
  CloseFile(myFile);
 except
  on e: Exception do writelog(0,'Error in saveXmlFile : '+e.Message);
 end;
end;

function getXmlStrings : TStrings;
var
myXml : String;
xmlStrings : TStrings;
begin
 if DatamoduleXML.XMLDocument1.Active then
 begin
  DatamoduleXML.XMLDocument1.saveToXML(myXml);
  xmlStrings := TStringList.Create;
  formatMyXml(myXml,xmlStrings);
  getXmlStrings := xmlStrings;
 end
 else getXmlStrings := TStringList.Create;
end;

//*************  XML Node-Handling ***********************************

procedure delNode(nodePath : string);
var
 removeNode : IXMLNode;
begin
 try
  writelog(5,'try to del Node: '+nodePath);
  openNode(nodePath);
  removeNode := aktNode;
  aktNode := aktNode.ParentNode;
  if aktNode.ChildNodes.Remove(removeNode) = -1 then
   writelog(0,'Error: last node of nodePath: '+nodePath+' was not removed');
 except
  on e: Exception do writelog(0,'Error in delNode : '+e.Message);
 end;
end;

procedure openNode(nodePath : string);
var
 nodesInPath : array[0..50] of IXMLNode;
 i : integer;
 endOfPath : boolean;
 leavingPath, nodeName, attributeName, attributeValue : string;
begin
 try
  // the root node
  nodesInPath[0] := DatamoduleXML.XMLDocument1.DocumentElement;
  // walk the path
  // The Path looks like this:
  // nodedescription PATHSEPARATOR nodedescripton (and so on)
  // nodedescription looks like this
  // nodename attributeName="attributeValue"
  // (!) attributeValue may contain a PATHSEPARATOR string
  i := 1;
  endOfPath := false;
  leavingPath := nodePath;
  while not endOfPath do
  begin
   writelog(5,'try to open nodepath: '+leavingPath);
   nodeName := copy(leavingPath,1,pos(' ',leavingPath)-1);
   leavingPath := copy(leavingPath,pos(' ',leavingPath)+1,length(leavingPath));
   attributeName := copy(leavingPath,1,pos('=',leavingPath)-1);
   leavingPath := copy(leavingPath,pos('=',leavingPath)+1,length(leavingPath));
   if pos('"'+PATHSEPARATOR,leavingPath) > 0 then
    attributeValue := copy(leavingPath,1,pos('"'+PATHSEPARATOR,leavingPath))
   else
    attributeValue := leavingPath;
   attributeValue := Trim(attributeValue);
   if AnsiStartsStr('"',attributeValue) then
    attributeValue := copy(attributeValue,2,length(attributeValue));
   if AnsiEndsStr('"',attributeValue) then
    attributeValue := copy(attributeValue,1,length(attributeValue)-1);
   writelog(5,'node '+IntToStr(i)+': nodename: '+nodename
               +' attributeName: '+attributeName
               +' attributeValue: '+attributeValue
               +' leavingPath: '+leavingPath);
   if not getNode(nodesInPath[i],nodesInPath[i-1],
                     nodeName, attributeName, attributeValue) then
   begin
    makeNode(nodesInPath[i],nodesInPath[i-1],
                     nodeName, attributeName, attributeValue);
    writelog(4,'Created node '+IntToStr(i)+': nodename: '+nodename
               +' attributeName: '+attributeName
               +' attributeValue: '+attributeValue
               +' leavingPath: '+leavingPath);
   end
   else writelog(4,'Found node '+IntToStr(i)+': nodename: '+nodename
               +' attributeName: '+attributeName
               +' attributeValue: '+attributeValue
               +' leavingPath: '+leavingPath);
   if pos('"'+PATHSEPARATOR,leavingPath) = 0 then
    endOfPath := true
   else
   begin
    leavingPath := copy(leavingPath,
                        pos('"'+PATHSEPARATOR,leavingPath)+2,
                        length(leavingPath));
    inc(i);
   end;
  end;
  aktNode := nodesInPath[i];
  writelog(4,'aktNode know node '+IntToStr(i)+': nodename: '+nodename
              +' attributeName: '+attributeName
              +' attributeValue: '+attributeValue);
 except
  on e: Exception do writelog(0,'Error in openNode : '+e.Message);
 end;
end;

function getNode(var newNode:IXMLNode; parentNode:IXMLNode;
                     nodeName, attributeName, attributeValue : string):boolean;
var
 namespaceUri,attribbasename :String;
 found : boolean;
 i, nodezahl : integer;
 searchname, searchvalue : string;
begin
 getNode := false;
 try
  writelog(5,'try to find node  nodename: '+nodename
               +' attributeName: '+attributeName
               +' attributeValue: '+attributeValue);
  getNamespaceAndBasename(attributeName,namespaceUri,attribbasename);
  found := false;
  nodezahl := parentNode.ChildNodes.Count;
  i := 0;
  // knoten suchen...
  while (i <=nodezahl-1) and (not found) do
  begin
   //perhaps here we have also to look for attribute name ???????????
   searchname := parentNode.ChildNodes[i].NodeName;
   writelog(5,'try to get attribute: name: '
              +attributeName+' value: '+attributeValue);
   //searchvalue := parentNode.ChildNodes[i].Attributes[attributeName];
   if parentNode.ChildNodes[i].HasAttribute(attribbasename,namespaceUri) then
    searchvalue := parentNode.ChildNodes[i].GetAttributeNS(attribbasename,namespaceUri)
   else searchvalue := '';
   if (compareStr(searchname,nodename) = 0)
      and (compareStr(searchvalue,attributeValue) = 0) then
   begin
    found := true;
    getNode := true;
    newNode := parentNode.ChildNodes[i];
   end;
   inc(i);
  end;
 except
  on e: Exception do writelog(0,'Error in getNode: '+e.Message);
 end;
end;

function makeNode(var newNode:IXMLNode; parentNode:IXMLNode;
                     nodeName, attributeName, attributeValue : string):boolean;
begin
 makeNode := makeNodeAtPos(newNode, parentNode,
                           nodeName, attributeName,
                           attributeValue, -1);
end;

function makeNodeAtPos(var newNode:IXMLNode; parentNode:IXMLNode;
                     nodeName, attributeName, attributeValue : string;
                     Position : integer):boolean;
var
 namespaceUri,attribbasename :String;
begin
 makeNodeAtPos := false;
 try
  writelog(5,'try to make node nodename: '+nodename
               +' attributeName: '+attributeName
               +' attributeValue: '+attributeValue
               +' at Position (-1=append): '+IntToStr(Position));
  // create , set attributes...

  newNode := parentNode.AddChild(nodeName,Position);
  if attributeName <> '' then
  begin
   getNamespaceAndBasename(attributeName,namespaceUri,attribbasename);
   newNode.SetAttributeNS(attribbasename,namespaceUri,attributeValue);
  end;
  makeNodeAtPos := true;
 except
  on e: Exception do writelog(0,'Error in makeNodeAtPos : '+e.Message);
 end;
end;

procedure setNodeAtPosition(nodeName, attributeName, attributeValue : string;
                             position : integer);
var
 newNode : IXMLNode;
begin
 try
  //
  if makeNodeAtPos(newNode, aktNode,
                     nodeName, attributeName, attributeValue,
                     Position) then
  begin
   writelog(4,'new node created: '+nodeName+' '+attributeName+'='+attributeValue);
   aktNode := newNode;
  end
  else writelog(1,'new node not created: '+nodeName+' '+attributeName+'='+attributeValue)
 except
  on e: Exception do writelog(0,'Error in setNodeAtPosition: '+e.Message);
 end;
end;

procedure setNodeText(text : string);
begin
 try
  writelog(5,'try to set text to aktNode: '+text);
  aktNode.Text := text;
 except
  on e: Exception do writelog(0,'Error in setNodeText: '+e.Message);
 end;
end;

function nodeExists(nodePath : string) : boolean;
// tells if a node exists without changing anything
var
 nodesInPath : array[0..50] of IXMLNode;
 i : integer;
 endOfPath, found : boolean;
 leavingPath, nodeName, attributeName, attributeValue : string;
begin
 nodeExists := false;
 try
  // the root node
  nodesInPath[0] := DatamoduleXML.XMLDocument1.DocumentElement;
  // walk the path
  // The Path looks like this:
  // nodedescription PATHSEPARATOR nodedescripton (and so on)
  // nodedescription looks like this
  // nodename attributeName="attributeValue"
  // (!) attributeValue may contain a PATHSEPARATOR string
  i := 1;
  endOfPath := false;
  found := true;
  leavingPath := nodePath;
  while (not endOfPath) and found do
  begin
   writelog(5,'try to open nodepath: '+leavingPath);
   nodeName := copy(leavingPath,1,pos(' ',leavingPath)-1);
   leavingPath := copy(leavingPath,pos(' ',leavingPath)+1,length(leavingPath));
   attributeName := copy(leavingPath,1,pos('=',leavingPath)-1);
   leavingPath := copy(leavingPath,pos('=',leavingPath)+1,length(leavingPath));
   if pos('"'+PATHSEPARATOR,leavingPath) > 0 then
    attributeValue := copy(leavingPath,1,pos('"'+PATHSEPARATOR,leavingPath))
   else
    attributeValue := leavingPath;
   attributeValue := Trim(attributeValue);
   if AnsiStartsStr('"',attributeValue) then
    attributeValue := copy(attributeValue,2,length(attributeValue));
   if AnsiEndsStr('"',attributeValue) then
    attributeValue := copy(attributeValue,1,length(attributeValue)-1);
   writelog(5,'node '+IntToStr(i)+': nodename'+nodename
               +' attributeName: '+attributeName
               +' attributeValue: '+attributeValue
               +' leavingPath: '+leavingPath);
   if not getNode(nodesInPath[i],nodesInPath[i-1],
                     nodeName, attributeName, attributeValue) then
   begin
    found := false;
    writelog(4,'not found node '+IntToStr(i)+': nodename: '+nodename
               +' attributeName: '+attributeName
               +' attributeValue: '+attributeValue
               +' leavingPath: '+leavingPath);
   end
   else
   begin
    writelog(4,'Found node '+IntToStr(i)+': nodename: '+nodename
               +' attributeName: '+attributeName
               +' attributeValue: '+attributeValue
               +' leavingPath: '+leavingPath);
    found := true;
   end;
   if pos('"'+PATHSEPARATOR,leavingPath) = 0 then
    endOfPath := true
   else
   begin
    leavingPath := copy(leavingPath,
                        pos('"'+PATHSEPARATOR,leavingPath)+2,
                        length(leavingPath));
    inc(i);
   end;
  end;
  if found then nodeExists := true
  else nodeExists := false;
 except
  on e: Exception do writelog(0,'Error in nodeExists : '+e.Message);
 end;
end;


//*************  XML Attribute-Handling ***********************************

procedure setAttribute(attributeName, attributeValue : string);
 // attributeName may include a namespace prefix
var
namespaceUri,attribbasename :String;
begin
 try
  writelog(5,'try to set attribute: name: '
              +attributeName+' value: '+attributeValue);
  getNamespaceAndBasename(attributeName,namespaceUri,attribbasename);
  aktNode.SetAttributeNS(attribbasename,namespaceUri,attributeValue);
  //aktNode.Attributes[attribbasename] := attributeValue;
 except
  on e: Exception do writelog(0,'Error in setAttribute: '+e.Message);
 end;
end;

procedure addAttribute(attributeName, attributeValue : string);
var
namespaceUri,attribbasename :String;
begin
 try
  writelog(5,'try to add attribute: name: '
              +attributeName+' value: '+attributeValue);
  getNamespaceAndBasename(attributeName,namespaceUri,attribbasename);
  if not aktNode.HasAttribute(attribbasename,namespaceUri) then
   aktNode.SetAttributeNS(attribbasename,namespaceUri,attributeValue);
 except
  on e: Exception do writelog(0,'Error in addAttribute: '+e.Message);
 end;
end;

procedure delAttribute(attributeName : string);
var
namespaceUri,attribbasename :String;
begin
 try
  writelog(5,'try to del attribute: name: '+attributeName);
  getNamespaceAndBasename(attributeName,namespaceUri,attribbasename);
  if aktNode.HasAttribute(attribbasename,namespaceUri) then
   aktNode.AttributeNodes.Delete(attribbasename,namespaceUri);
 except
  on e: Exception do writelog(0,'Error in addAttribute: '+e.Message);
 end;
end;

function getAttributeValue(attributeName : string) : string;
var
namespaceUri,attribbasename :String;
// get the attribute value at the actual node
// ATTENTION: it returns an empty string:
// - if there is no attribute with this name
// - if the value of this attribute is an empty string
begin
 try
  writelog(5,'try to get value of attribute: name: '
              +attributeName);
  getAttributeValue := '';
  getNamespaceAndBasename(attributeName,namespaceUri,attribbasename);
  if aktNode.HasAttribute(attribbasename,namespaceUri) then
   getAttributeValue := aktNode.GetAttributeNS(attribbasename,namespaceUri);
 except
  on e: Exception do writelog(0,'Error in addAttribute: '+e.Message);
 end;
end;



end.

