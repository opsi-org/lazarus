unit osxmlsections;

// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the General Public License.

// Text of the GPL: http://www.gnu.org/licenses/gpl.html
// Unofficial GPL Translations: http://www.gnu.org/licenses/translations.html

// author: Rupert Roeder, APohl, detlef oertel, m.hammel

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  DOM,
  XMLRead,
  xmlreader,
  XMLWrite,
  //Dialogs,
  StrUtils,
  //ExtCtrls,
  key_valueCollection,
  oslog,
  osparserhelper;

type
  TNodeSet = array of TDOMNode;

  TStringsArray = array of string;
  TbooleansArray = array of boolean;

  TLogCallbackProcedure = procedure(level: integer; logmsg: string) of object;

  TuibXMLDocument = class(TXMLDocument)
  private
    XML: TXMLDocument;
    actNode: TDOMNode;
      {actNode.FirstChild: TDOMNode, LastChild: TDOMNode, NextSibling: TDOMNode,
      PreviousSibling: TDOMNode, ParentNode: TDOMNode
      }

    fActNodeSet: TNodeSet;
    fDerivedNodeSet: TNodeSet;
    fDebugLevel: integer;

    dictionary: TList;

    function getCountNotNil: integer;
    // length actNodeSet without NIL-Elements

    function getCountDerivedNotNil: integer;
    // length derivedNodeSet without NIL-Elements

    function getNodeStrict(var newNode: TDOMNode; myparentNode: TDOMNode;
      mynodeName: string; attributeList: TList): boolean;
    // nodename has to fit and all attributes

    function getNodeByNameAndAttribute(var newNode: TDOMNode;
      myparentNode: TDOMNode; mynodeName: string; attributename: string;
      attributevalue: string): boolean;
    // nodename has to fit, one attribute can fit

    function getNodeByName(var newNode: TDOMNode; myparentNode: TDOMNode;
      mynodeName: string): boolean;
    // nodename has to fit

    function makeAttributesSL(var attributeStringList: TStringList;
      attributePath: string; var errormessage: string): boolean;

    procedure ErrorHandler(E: EXMLReadError);

  public
    destructor Destroy; override;
    //*************  XML File-Handling ***********************************
    function openXmlFile(filename: string): boolean;
    function writeXmlAndCloseFile(filename: string): boolean;

    //*************  XML Handling ***********************************
    function createXmlDocFromStringlist(docstrlist: TStringList): boolean;
    function getDocumentElement(): TDOMNode;
    function isValidXML(xmlString: TStringList): boolean;
    function getXmlStrings: TStringList;
    //function nodeExistsByPathInXMLFile(filename,
    //                 path:string; attributes_strict: boolean) : boolean;
    //*************  NodeSet-Handling ***********************************
    procedure makeTopAsActNodeSet;
    // set actNodeSet from DocumentElement (top)

    procedure getNextGenerationActNodeSet;
    // set derivedNodeSet as actNodeset

    procedure makeNewDerivedNodeSet;
    // create derivedNodeSet. For every node in actNodeSet collect the next
    // childs. Therefore: helpfull if actNodeSet contains only one node

    procedure setlengthActNodeSet(newlength: integer);


    procedure setlengthDerivedNodeSet(newlength: integer);


    procedure cleanUpActNodeSet();
    // eliminate NIL-Elements from actNodeset

    procedure cleanUpDerivedNodeSet();
    // eliminate NIL-Elements from derivedNodeSet

    procedure logNodeSets;
    procedure logActNodeSet;

    //*************  Filters on Nodesets ********************************
    function filterByAttributeList
      (var attributenames: Tstringsarray; var attributevalueExists: Tbooleansarray;
      var attributevalues: TStringsArray; attributes_strict: boolean): boolean;

    function filterByAttributeName_existing(Name: string): boolean;

    function filterByAttribute_existing(Name: string; Value: string): boolean;

    function filterByText(textvalue: string): boolean;

    function filterByChildElement(elementname: string): boolean;
    // tested


    //*************  Node-Operations ***********************************
    // The main idea for the following syntax:

    // The nodePath looks like this:
    // nodedescription XML2PATHSEPARATOR nodedescripton (and so on)
    // nodedescription looks like this:
    // nodename attributeName="attributeValue"
    // (!) attributeValue should be surrounded with ""
    // (!) attributeValue may contain a XML2PATHSEPARATOR string
    // nodeName may include a namespace prefix - no more
    // attributeName may include a namespace prefix - no more
    // (!) namespace prefix are case sensitiv - no more
    // a node without attributes looks like this:
    // nodename =""
    // XML2PATHSEPARATOR string is ' // '

    // eg:
    // foo bar="bar1" // childfoo ="" // RDF:foo NC:bar=nonesense

    function nodeExists(nodePath: string; attributes_strict: boolean;
      var errorinfo: string): boolean;
    // tells if a node exists without changing anything
    // if attributes_strict=true: check all attributes, true if path and all
    //                            attributes fit, false if anything is wrong
    // if attributes_strict=false: check only node path,
    //                             true if node path exists, false if not

    function openNode(nodePath: string; attributes_strict: boolean;
      var errorinfo: string): boolean;
    // open the node, analog nodeExists
    // afterwards this node is actNode


    function getNodeByNameAndTextContent(var newNode: TDOMNode;
      myparentNode: TDOMNode; mynodeName: string; mynodeTextContent: string): boolean;
    // nodename and text has to fit

    // overload
    function makeNode(mynodeName: string): boolean; overload;
    // create new node, append to actNode, set newnode as actNode

    function makeNode(mynodeName, attributeName, attributeValue: string): boolean;
      overload;
    // create new node, append to actNode, set newnode as actNode

    // overload
    function makeNodeAndKeepActNode(mynodeName: string): boolean; overload;
    // create new node, append to actNode, actNode is kept

    function makeNodeAndKeepActNode(mynodeName, attributeName,
      attributeValue, Text: string): boolean; overload;
    // create new node, append to actNode, actNode is kept


    function makeNodePathWithTextContent(nodePath: string; Text: string;
      var errorinfo: string): boolean;
    // create note path with textcontent
    // in nodepath all attributes have to fit or they will be created
    // actNode will be last created node, text will be set as TextContent

    function makeNodes
      (mynodeName: string; const textArray: TStringList): boolean;
    // creates couple of nodes in actnode with equal names
    // and different TextContent, actNode will be same after creation

    // overload procedure
    procedure delNode(nodePath: string; attributes_strict: boolean;
      var errorinfo: string); overload;
    // this node (and all childs) will be deleted, afterwards parent will be actnode
    // node will be opened with openNode

    procedure delNode(); overload;
    // aktnode (and all childs) will be deleted, afterwards parent will be actnode

    procedure setTopNodeAsActNode();
    // set top node as actual node

    procedure setFirstChildAsActNode();
    // set first child as actual node

    procedure setParentNodeAsActNode();
    // set parent node as actual node

    procedure setNodeTextActNode(Text: string);
    // set text at the actual node

    function setActNodeIfText(textvalue: string): boolean;
    // set actnode if text found

    function getNodeTextActNode(): string;
    // get text from the actual node

    function getNodeNameActNode(): string;
    // get name from the actual node

    //*************  Node-Operations on dicts *****************************
    function setNodePair(keyNodeName: string; keyNodeTextContent: string;
      valueNodeName: string; valueNodeTextContent: string): boolean;
    // set value node for key node

    //*************  Attributes *******************************************
    function countAttributes(myxmlnode: TDOMNode): integer;
    // count attributes of this node

    function hasAttribute(attributename: string): boolean;
    // checks if the actual node has the attribute

    procedure setAttribute(attributeName, attributeValue: string);
    // set the attribute at the actual node
    // if it exists it will be changed, otherwis create


    procedure addAttribute(attributeName, attributeValue: string);
    // add the attribute at the actual node
    // if it exists it will not (!) be changed

    procedure delAttribute(attributeName: string);
    // delete the attribute  at the actual node


    function getAttributeValue(attributeName: string): string;
    // get the attribute value at the actual node
    // ATTENTION: it returns an empty string:
    // - if there is no attribute with this name
    // - if the value of this attribute is an empty string

    // properties
    property debuglevel: integer read FDebugLevel write FDebugLevel;
    property actNodeSet: TNodeSet read factNodeSet write factNodeSet;
    property derivedNodeSet: TNodeSet read fDerivedNodeSet write fDerivedNodeSet;
    property CountNotNil: integer read getCountNotNil;
    property CountDerivedNotNil: integer read getCountDerivedNotNil;


  end;

function nodeExistsByPathInXMLFile(myfilename, path: string;
  attributes_strict: boolean; var errorinfo: string): boolean;


implementation

const
  XML2PATHSEPARATOR: string = ' // ';
  extraindent = '   ';


var
  showErrors: boolean = True;
//#####################
// from osfunc
function divideAtFirst(const partialS, S: string; var part1, part2: string): boolean;
  //   teilt den String S beim ersten Vorkommen des Teilstrings partialS;
  //   liefert true, wenn partialS vorkommt,
  //   andernfalls false;
  //   wenn partialS nicht vorkommt, enthaelt part1 den Gesamtstring, part2 ist leer

var
  i: integer = 0;
begin
  i := pos(lowercase(partialS), lowercase(s));
  if i > 0 then
  begin
    part1 := copy(S, 1, i - 1);
    part2 := copy(S, i + length(partialS), length(S));
    Result := True;
  end
  else
  begin
    part1 := s;
    part2 := '';
    Result := False;
  end;
end;

// from osfunc
procedure stringsplit(const s, delimiter: string; var Result: TStringList);
// produziert eine Stringliste aus den Teilstrings, die zwischen den Delimiter-Strings stehen
var
  remainder: string = '';
  item: string = '';
  found: boolean;
begin
  Result := TStringList.Create;
  found := divideAtFirst(delimiter, s, item, remainder);
  while found do
  begin
    Result.add(item);
    found := divideAtFirst(delimiter, remainder, item, remainder);
  end;
  Result.add(item);
end;

function max(num1, num2: integer): integer;
begin
  if (num1 > num2) then
    Result := num1
  else
    Result := num2;
end;

{function getTag(line: string): string;
var
  p: integer;
  r, s: string;
begin
  Result := '';
  p := pos('<', line);
  if p > -1 then
  begin
    s := copy(line, p + 1, length(line));
    // TODO: getWord  (s, result, r, [' ', '>', '/']);
  end;
end;
}

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

procedure setShowErrors(bValue: boolean);
begin
  showErrors := bValue;
end;

procedure TuibXMLDocument.ErrorHandler(E: EXMLReadError);
//http://wiki.lazarus.freepascal.org/XML_Tutorial/de#.C3.9Cberpr.C3.BCfen_der_G.C3.BCltigkeit_der_Struktur_einer_XML-Datei
begin
  if E.Severity = esError then
  begin
    LogDatei.log(E.Message, LLerror);
    // an dieser Stelle können wir auch alles andere machen, was bei einem Fehler getan werden sollte
  end;
end;


//*************  TuibXMLDocument ***********************************
destructor TuibXMLDocument.Destroy;
begin
  inherited Destroy;
end;

//*************  XML File-Handling ***********************************
function TuibXMLDocument.openXmlFile(filename: string): boolean;
var
  mystream: TFileStream;
begin
  openXmlFile := False;
  LogDatei.log('try to open File: ' + filename, LLinfo);
  try
    try
      mystream := TFilestream.Create(fileName, fmOpenRead);
      mystream.Position := 0;
      XML := nil;
      LogDatei.log('try to load File: ' + filename, oslog.LLinfo);
      ReadXMLFile(XML, mystream);
      LogDatei.log('File: ' + filename + ' read', oslog.LLinfo);
      openXmlFile := True;
    except
      on e: Exception do
        LogDatei.log('Error in readXmlFile : ' + e.Message, oslog.LLerror);
    end;
  finally
    if Assigned(mystream) then
      mystream.Free;
  end;
end;

function TuibXMLDocument.writeXMLAndCloseFile(filename: string): boolean;
  { TODO : Schließen des Streams bei Except }
var
  mystream: TFileStream;
begin
  writeXMLAndCloseFile := False;
  try
    try
      LogDatei.log('try to open File: ' + filename, oslog.LLinfo);
      mystream := TFilestream.Create(fileName, fmCreate);
      mystream.Position := 0;
      WriteXMLFile(XML, mystream);
      LogDatei.log('file saved: ' + filename, oslog.LLInfo);
      writeXMLAndCloseFile := True;
    except
      on e: Exception do
        LogDatei.log('Error in writeXmlFile : ' + e.Message, oslog.LLerror);
    end;
  finally
    if Assigned(mystream) then
      mystream.Free;
  end;
end;




//*************  XML Handling ***********************************
function TuibXMLDocument.createXmlDocFromStringlist(docstrlist: TStringList): boolean;
var
  mystream: TStringStream;
  settings: TXMLReaderSettings;
begin
  settings:= TXMLReaderSettings.Create;
  LogDatei.log('CanonicalForm: '+BoolToStr(settings.CanonicalForm,true),5);
  settings.CanonicalForm:=true;
  createXmlDocFromStringlist := False;
  LogDatei.log('begin to create XMLDoc ', oslog.LLinfo);
  mystream := TStringStream.Create(docstrlist.Text);
  mystream.Position := 0;
  XML := nil;
  try
    try
      ReadXMLFile(XML, mystream);
      //ReadXMLFile(XML, mystream,'stream:',settings);
      LogDatei.log('XMLDoc created from Stringlist', LLinfo);
      createXmlDocFromStringlist := True;
    except
      on e: Exception do
      begin
        LogDatei.log('createXmlDocFromStringlist failed: XMLDoc create from Stringlist',
          LLerror);
        LogDatei.log('Exception in createXmlDocFromStringlist: ' +
          E.ClassName + ': ' + E.Message, LLerror);
      end;
    end;
  finally
    if Assigned(mystream) then
      mystream.Free;
  end;
  LogDatei.log('XMLDoc created', LLinfo);
end;

function TuibXMLDocument.getDocumentElement(): TDOMNode;
begin
  getDocumentElement := XML.DocumentElement;
end;

function TuibXMLDocument.isValidXML(xmlString: TStringList): boolean;
{ TODO : Exception abfangen
 }
var
  nodestream: TStringStream;
  Parser: TDOMParser;
  Src: TXMLInputSource;
  TheDoc: TXMLDocument;
begin
  isValidXML := False;
  try
    try
      Parser := TDOMParser.Create;
      nodestream := TStringStream.Create(stringlistWithoutBreaks(xmlString).Text);
      nodestream.Position := 0;
      Src := TXMLInputSource.Create(nodestream);
      Parser.Options.Validate := True;
      Parser.OnError := @ErrorHandler;
      Parser.Parse(Src, TheDoc);
      isValidXML := True;
    except
      LogDatei.log('isValidXML failed', LLerror);
    end;
  finally
    if Assigned(Src) then
      Src.Free;
    if Assigned(Parser) then
      Parser.Free;
    if Assigned(nodestream) then
      nodestream.Free;
    if Assigned(TheDoc) then
      TheDoc.Free;
  end;

end;

function TuibXMLDocument.getXmlStrings: TStringList;
var
  nodeStream: TMemoryStream;
  mystringlist: TStringList;
begin
  Result := TStringList.Create;
  if XML <> nil then
    try
      mystringlist := TStringList.Create;
      nodeStream := TMemoryStream.Create;
      WriteXML(XML, nodestream);
      nodeStream.Position := 0;
      mystringlist.LoadFromStream(nodestream);
      getXmlStrings := mystringlist;
    except
      LogDatei.log('getXmlStrings failed', LLerror);
    end;
end;

//*************  Operations NodeSet  ***********************************
procedure TuibXMLDocument.makeTopAsActNodeSet;
var
  k: integer;
begin
  setlengthActNodeSet(1);
  actnodeset[0] := getDocumentElement;
  for k := 0 to length(actNodeSet) - 1 do
    if actNodeSet[k] <> nil then
      LogDatei.log('actNodeSet <> nil', oslog.LLinfo)
    else
      LogDatei.log('actNodeSet = nil', oslog.LLinfo);
end;

function TuibXMLDocument.getCountNotNil: integer;
  // length actNodeSet without NIL-Elements
var
  i: integer;
  preresult: integer;
begin
  preresult := 0;
  for i := 0 to length(actnodeset) - 1 do
  begin
    if actnodeset[i] <> nil then
      Inc(preresult);
  end;
  getCountNotNil := preresult;
end;

function TuibXMLDocument.getCountDerivedNotNil: integer;
  // length DerivedNodeSet without NIL-Elements
var
  i: integer;
  preresult: integer;
begin
  preresult := 0;
  for i := 0 to length(DerivedNodeSet) - 1 do
  begin
    if derivednodeset[i] <> nil then
      Inc(preresult);
  end;
  getCountDerivedNotNil := preresult;
end;

procedure TuibXMLDocument.getNextGenerationActNodeSet;
// thereby removing the nil elements
var
  i, n: integer;
begin
  LogDatei.log('Going to the next level ...', LLinfo);
  setlengthActNodeSet(0);
  n := length(FderivedNodeSet);
  for i := 0 to n - 1 do
  begin
    if derivednodeset[i] <> nil then
    begin
      setlengthActNodeSet(length(FActnodeset) + 1);
      FActNodeset[length(FActNodeSet) - 1] := derivednodeset[i];
    end;
  end;
end;


procedure TuibXMLDocument.makeNewDerivedNodeSet;
var
  i, n, j, basejindex: integer;

begin
  setLengthDerivedNodeSet(0);
  n := length(actNodeSet);
  for i := 0 to n - 1 do
  begin
    if actnodeset[i] <> nil then
      setlengthDerivedNodeSet(length(FDerivednodeset) + actnodeset[i].ChildNodes.Count);
  end;
  { fill with values }
  basejindex := 0;
  for i := 0 to n - 1 do
  begin
    if actnodeset[i] <> nil then
    begin
      for  j := 0 to actNodeSet[i].childnodes.Count - 1 do
        FDerivednodeset[basejindex + j] := actNodeset[i].childnodes[j];
      basejindex := basejindex + actNodeSet[i].childnodes.Count;
    end;
  end;
end;


procedure TuibXMLDocument.setlengthActNodeSet(newlength: integer);
begin
  setlength(factnodeset, newlength);
end;

procedure TuibXMLDocument.setlengthDerivedNodeSet(newlength: integer);
begin
  setlength(fderivednodeset, newlength);
end;

procedure TuibXMLDocument.cleanUpActNodeSet();
var
  i, n: integer;
  myNodeSet: TNodeSet;
begin
  n := length(actnodeset);
  for i := 0 to n - 1 do
  begin
    if actNodeSet[i] <> nil then
    begin
      setlength(myNodeSet, length(myNodeSet) + 1);
      mynodeset[length(myNodeSet) - 1] := actnodeset[i];
    end;
  end;
  actnodeset := mynodeset;
end;

procedure TuibXMLDocument.cleanUpDerivedNodeSet();
var
  i, n: integer;
  myNodeSet: TNodeSet;
begin
  n := length(fderivednodeset);
  for i := 0 to n - 1 do
  begin
    if fderivednodeset[i] <> nil then
    begin
      setlength(myNodeSet, length(mynodeset) + 1);
      mynodeset[length(myNodeSet) - 1] := fderivednodeset[i];
    end;
  end;
  fderivednodeset := mynodeset;
end;

procedure TuibXMLDocument.logNodeSets;
var
  i, basejindex, j: integer;
  count_not_nil: integer;
  count_not_nil_2: integer;

begin
  cleanUpActNodeSet();
  cleanUpDerivedNodeSet();
  LogDatei.log_prog('', LLinfo);
  LogDatei.log_prog('actNodeSet: ', LLinfo);
  count_not_nil := 0;
  for i := 0 to length(actNodeSet) - 1 do
  begin
    if actnodeset[i] = nil then
      LogDatei.log_prog(extraindent + 'node ' + IntToStr(i) + ' null ', LLinfo)
    else
    begin
      LogDatei.log_prog(extraindent + 'node ' + IntToStr(i) +
        ' elementname: "' + actNodeSet[i].NodeName + '"', LLinfo);
      Inc(count_not_nil);
    end;
  end;

  LogDatei.log_prog('Non-null element(s) in act node set: ' +
    IntToStr(count_not_nil), LLinfo);

  count_not_nil_2 := 0;
  LogDatei.log_prog('derivedNodeSet: ', LLinfo);

  for i := 0 to length(derivedNodeSet) - 1 do
  begin
    if derivednodeset[i] = nil then
      LogDatei.log_prog(extraindent + 'node ' + IntToStr(i) + ' null ', LLinfo)
    else
    begin
      LogDatei.log_prog(extraindent + 'node ' + IntToStr(i) +
        ' elementname: "' + derivedNodeSet[i].NodeName + '"', LLinfo);
      Inc(count_not_nil_2);
    end;
  end;

  LogDatei.log_prog('Non-null element(s) remaining in children node set: ' +
    IntToStr(count_not_nil_2), LLinfo);
  LogDatei.log_prog('', LLinfo);
  LogDatei.log('    -------', LLinfo);
end;

procedure TuibXMLDocument.logActNodeSet;
var
  i: integer;
  count_not_nil: integer;

begin
  cleanUpActNodeSet();
  LogDatei.log_prog('', LLinfo);
  LogDatei.log_prog('actNodeSet: ', LLinfo);
  count_not_nil := 0;
  for i := 0 to length(actNodeSet) - 1 do
  begin
    if actnodeset[i] = nil then
      LogDatei.log_prog(extraindent + 'node ' + IntToStr(i) + ' null ', LLinfo)
    else
    begin
      LogDatei.log_prog(extraindent + 'node ' + IntToStr(i) +
        ' elementname: "' + actNodeSet[i].NodeName + '"', LLinfo);
      Inc(count_not_nil);
    end;
  end;
  LogDatei.log_prog('Non-null element(s) in act node set: ' +
    IntToStr(count_not_nil), LLinfo);

end;


//*************  Filters on Nodesets ********************************
function TuibXMLDocument.filterByAttributeList
  (var attributenames: Tstringsarray; var attributevalueExists: Tbooleansarray;
  var attributevalues: TStringsArray; attributes_strict: boolean): boolean;
  // TODO - muss noch umgearbeitet und getestet werden
var
  i, n, j, basejindex, k, numberOfAttributes: integer;

  b0, b1, b2: boolean; // results of the crucial evaluations for each attribute

  goon: boolean;
  attributename: string; // ergänzt, warum feht es?
  attributeList: TList;

begin
  Result := True;
  numberOfAttributes := length(attributenames);

  if (numberOfAttributes = 0) and not attributes_strict then
  begin
    LogDatei.log('no filtering by attributes requested', LLWarning);
    exit;
  end;


  LogDatei.log('retaining child elements with the following attribute(s):', LLinfo);
  for k := 0 to numberOfAttributes - 1 do
  begin
    if attributevalueExists[k] then
      LogDatei.log('   "' + attributenames[k] + '" value="' +
        attributevalues[k] + '"', LLinfo)
    else
      LogDatei.log('   "' + attributenames[k] + '"', LLinfo);
  end;


  basejindex := 0;

  i := 0;

  while i < length(actNodeSet) do
  begin

    if actNodeSet[i] <> nil then
    begin

      n := actNodeSet[i].childnodes.Count;

      for j := 0 to n - 1 do
      begin

        goon := True;

        if (FderivedNodeSet[basejindex + j] = nil) then
          goon := False;

        if goon then
        begin
          if attributes_strict
          // TODO
          //and
          //( actNodeSet[i].childnodes[j].AttributeNodes.Count <> length (attributenames) )
          then
          begin
            FderivedNodeSet[basejindex + j] := nil;
            LogDatei.log('node ' + IntToStr(basejindex + j) +
              ' not accepted: Number of attributes does not match', LLinfo);
            goon := False;
          end;

          k := 0;
          while goon and (k < numberOfAttributes) do
          begin
            // TODO
            //getNamespaceAndBasename (attributenames[k], uri, attributename);
            attributename := '';
            // nur damit es übersetzt, muss noch umgebaut und getestet werden
            //b1 := actNodeSet[i].childnodes.Item[j].HasAttributes (attributename);
            if b1 then
            //b2 := actNodeSet[i].childnodes.Item[j].getattributes(attributename) = attributevalues[k]
            else
              b2 := False;

            if not b1 then
            begin
              FderivedNodeSet[basejindex + j] := nil;

              LogDatei.log('node ' + IntToStr(basejindex + j) +
                ' not accepted: No attribute "' + attributenames[k] + '"', LLinfo);
              goon := False;
            end

            else
            begin
              if attributevalueExists[k] and not b2 then
              begin
                FderivedNodeSet[basejindex + j] := nil;

                LogDatei.log('node ' + IntToStr(basejindex + j) +
                  ' not accepted: Not the required value "' +
                  attributevalues[k] + '" for attribute "' +
                  attributenames[k] + '"', LLinfo);
                goon := False;
              end;
            end;

            Inc(k);

          end;
        end;

      end;

      basejindex := basejindex + n;

    end;

    Inc(i);
  end;
end;



function TuibXMLDocument.filterByAttributeName_existing(Name: string): boolean;
var
  i, n, j, basejindex: integer;
  b0, b1: boolean;

begin
  LogDatei.log('retaining child elements with attribute "' + Name + '"', LLinfo);
  Result := True;
  basejindex := 0;

  i := 0;

  while i < length(actNodeSet) do
  begin

    if actNodeSet[i] <> nil then
    begin
      n := actNodeSet[i].childnodes.Count;

      for j := 0 to n - 1 do
      begin
        actnode := actNodeSet[i].ChildNodes.Item[j];
        b1 := hasAttribute(Name);
        b0 := FderivedNodeSet[basejindex + j] <> nil;

        if b1 and b0 then
          FderivedNodeSet[basejindex + j] := actNodeSet[i].childnodes.Item[j]
        else
          FderivedNodeSet[basejindex + j] := nil;
      end;
      basejindex := basejindex + n;
    end;
    Inc(i);
  end;
end;


function TuibXMLDocument.filterByAttribute_existing(Name: string;
  Value: string): boolean;
var
  i, n, j, basejindex: integer;
  b0, b1, b2: boolean;

begin
  LogDatei.log('retaining child elements with attribute "' + Name +
    '" value: "' + Value + '"', fdebuglevel);
  Result := True;
  basejindex := 0;

  i := 0;

  while i < length(actNodeSet) do
  begin

    if actNodeSet[i] <> nil then
    begin

      n := actNodeSet[i].childnodes.Count;

      for j := 0 to n - 1 do
      begin
        actnode := actNodeSet[i].ChildNodes.Item[j];
        b0 := FderivedNodeSet[basejindex + j] <> nil;
        b1 := hasAttribute(Name);
        b2 := '"' + getAttributeValue(Name) + '"' = Value;

        if b0 and b1 and b2 then
          FderivedNodeSet[basejindex + j] := actnode
        else
          FderivedNodeSet[basejindex + j] := nil;
      end;

      basejindex := basejindex + n;

    end;

    Inc(i);
  end;

end;

function TuibXMLDocument.filterByText(textvalue: string): boolean;
  // nodeset filter: after filtering the derived nodeset contains only elements with
  // text_content = textvalue
var
  i, n, j, basejindex: integer;
  comparetext: string;
  myparentNode: TDOMNode;
begin
  myparentNode := actNode;
  filterByText := True;

  LogDatei.log('retaining child elements with text  "' + textvalue + '"', LLinfo);

  basejindex := 0;
  i := 0;
  while i < length(actNodeSet) do
  begin

    if actNodeSet[i] <> nil then
    begin

      n := actNodeSet[i].childnodes.Count;

      for j := 0 to n - 1 do
      begin
        actnode := actNodeSet[i].childnodes.Item[j];
        if actnode.NodeType = TEXT_NODE then
          comparetext := actnode.TextContent
        else
          comparetext := '';

        if AnsiCompareStr(textvalue, comparetext) = 0 then
        begin
          FderivedNodeSet[basejindex + j] := actnode;
        end

        else
          FderivedNodeSet[basejindex + j] := nil;
      end;

      basejindex := basejindex + n;

    end;

    Inc(i);
  end;
  cleanUpActNodeSet();  // eliminate nil elements
  if getCountDerivedNotNil() > 0 then
  begin
    actnode := FderivedNodeSet[0];
    filterByText := True;
    if length(FderivedNodeSet) > 1 then
      LogDatei.log('more than one item has TEXT_CONTENT ' + textvalue +
        ', the first item becomes actNode', oslog.LLwarning)
    else
      LogDatei.log('item with TEXT_CONTENT ' + actNode.TextContent +
        ' found, this item becomes actNode', oslog.LLinfo);
  end
  else
  begin
    filterByText := False;
    actNode := myparentNode;
    LogDatei.log('none of the items has TEXT_CONTENT ' + textvalue +
      ',  name of actNode is ' + actNode.NodeName, oslog.LLwarning);
  end;
end;

function TuibXMLDocument.filterByChildElement(elementname: string): boolean;
  // nodeset filter: after filtering the derived nodeset contains only elements with
  // node name = elementname
var
  i, n, j, basejindex: integer;
begin
  filterByChildElement := False;

  LogDatei.log('retaining child elements with name = "' + elementname + '"', LLInfo);
  basejindex := 0;
  i := 0;
  while i < length(actNodeSet) do
  begin
    if actNodeSet[i] <> nil then
    begin
      n := actNodeSet[i].childnodes.Count;
      for j := 0 to n - 1 do
      begin
        actNode := actNodeSet[i].childnodes.Item[j];
        if (actNode.NodeName = elementname) then
        begin
          FderivedNodeSet[basejindex + j] := actNode;
          filterByChildElement := True;
        end
        else
          FderivedNodeSet[basejindex + j] := nil;
      end;
      basejindex := basejindex + n;
    end;

    Inc(i);
  end;
  cleanUpDerivedNodeSet();
  if length(FderivedNodeSet) > 0 then
    actnode := FderivedNodeSet[0].ParentNode;

end;

//*************  Node-Operations ***********************************
function TuibXMLDocument.nodeExists(nodePath: string; attributes_strict: boolean;
  var errorinfo: string): boolean;
  // tells if a node exists without changing anything
  // if attributes_strict=true: check all attributes, true if path and all
  //                            attributes fit, false if anything is wrong
  // if attributes_strict=false: check only node path,
  //                             true if node path exists, false if not

var
  nodesInPath: array[0..50] of TDOMNode;
  attributesSL, pathes: TStringList;
  k, i, j: integer;
  endOfPath, found, error: boolean;
  leavingPath, thisnodeName: string;
  attributeList: TList;
  errormessage: string;
begin
  nodeExists := False;
  attributeList := TList.Create;
  attributesSL := TStringList.Create;
  try
    try
      begin
        // the root node
        nodesInPath[0] := XML.DocumentElement;
        stringsplit(nodepath, XML2PATHSEPARATOR, pathes);
        // walk the path
        // The Path looks like this:
        // nodedescription XML2PATHSEPARATOR nodedescripton (and so on)
        // nodedescription looks like this
        // nodename attributeName="attributeValue"  or
        // nodename (node without attributes)
        // (!) attributeValue may contain a XML2PATHSEPARATOR string
        // XML2PATHSEPARATOR = ' // '
        i := 1;
        endOfPath := False;
        found := True;
        error := False;
        leavingPath := nodePath;
        LogDatei.log('begin to open nodepath, exists?  ' + nodepath, LLinfo);
        while (i < pathes.Count + 1) and (found or error) do
        begin
          attributeList.Clear;
          //logdatei.log('path element ' + IntToStr(i) + ' : ' + pathes[i - 1], LLinfo);
          thisnodeName := Trim(copy(pathes[i - 1], 1, pos(' ', pathes[i - 1]) - 1));
          //logdatei.log('thisnodename ' + thisnodeName, LLinfo);
          leavingPath := copy(pathes[i - 1], pos(' ', pathes[i - 1]) +
            1, length(pathes[i - 1]));
          //logdatei.log('leavingPath ' + leavingPath, LLinfo);
          if (pos('=', pathes[i - 1]) > 0) then // only in this case attributes
          begin
            // split on blank, list of attributes
            if not makeAttributesSL(attributesSL, leavingPath, errormessage) then
            begin
              // handle error
              errorinfo := errormessage;
              Result := False;
              attributeList.Free;
              exit;
            end;
            for k := 0 to attributesSL.Count - 1 do
              logdatei.log('Attribute ' + attributesSL[k], LLinfo);
            logdatei.log('Anzahl Attribute ' + IntToStr(attributesSL.Count), LLinfo);
            j := 0;
            while j < attributesSL.Count do
            begin
              // List of [attributename, attributevalue]
              attributeList.Add;
              attributeList.Items[j].key :=
                Trim(copy(attributesSL[j], 1, pos('=', attributesSL[j]) - 1));
              attributeList.Items[attributeList.Count - 1].Value :=
                Trim(copy(attributesSL[j], pos('=', attributesSL[j]) +
                1, length(attributesSL[j])));
              if AnsiStartsStr('"', attributeList.Items[j].Value) then
                attributeList.Items[j].Value :=
                  copy(attributeList.Items[j].Value, 2,
                  length(attributeList.Items[j].Value));
              if AnsiEndsStr('"', attributeList.Items[j].Value) then
                attributeList.Items[j].Value :=
                  Trim(copy(attributeList.Items[j].Value, 1,
                  length(attributeList.Items[j].Value) - 1));
              Inc(j);
            end;
          end
          else
            thisnodeName := Trim(pathes[i - 1]);


          LogDatei.log('node ' + IntToStr(i) + ': nodename ' + thisnodename, LLInfo);
          if attributes_strict then
          begin
            if not getNodeStrict(nodesInPath[i], nodesInPath[i - 1],
              thisnodeName, attributeList) then
            begin
              found := False;
              LogDatei.log('opennode: node with attributes_strict not found ' +
                IntToStr(i) + ': nodename: ' + thisnodeName +
                ', check nodename and attributes - exit function', LLInfo);
              // failed - make all final settings
              Result := False;
              attributeList.Free;
              exit;
            end
            else
            begin
              found := True;
              if actnode <> nil then
                logdatei.log('Found node with attributes_strict' +
                  IntToStr(i) + ': nodename: ' + actNode.NodeName, LLinfo)
              else
                logdatei.log('Found more then one node ' + IntToStr(i) +
                  ': ' + IntToStr(length(actnodeset)) + ' nodes'
                  , LLinfo);
            end;
          end
          else
          begin   // only check nodenames
            if getNodeByName(nodesInPath[i], nodesInPath[i - 1], thisnodeName) then
            begin
              LogDatei.log('Found node ' + IntToStr(i) + ': nodename: ' +
                thisnodename, LLinfo);
              found := True;
            end
            else
            begin
              found := False;
              LogDatei.log('not found node ' + IntToStr(i) +
                ': nodename: ' + thisnodename, LLInfo);
            end;
          end;

          Inc(i);
        end;
        attributeList.Free;
        Result := found;
      end;
    except
      on e: Exception do
      begin
        LogDatei.log('nodeExists: node not found' + ': nodename: ' +
          thisnodename, LLInfo);
        Result := False;
      end;
    end;
  finally
    FreeAndNil(attributesSL);
  end;
end;

function TuibXMLDocument.openNode(nodePath: string; attributes_strict: boolean;
  var errorinfo: string): boolean;
  // if attributes_strict = true all attributes have to exist
  //                      = false, other attributes may exist, no matter
  // set actNode
var
  nodesInPath: array[0..50] of TDOMNode;
  attributesSL, pathes: TStringList;
  i, j, k: integer;
  found: boolean;
  leavingPath, thisnodeName, attribute: string;
  attributeList: TList;
  errormessage: string;
begin
  Result := True;
  attributeList := TList.Create;
  attributesSL := TStringList.Create;
  try
    try
      for i := 0 to length(nodesInPath) - 1 do
        nodesInPath[i] := nil;
      // the root node
      nodesInPath[0] := XML.DocumentElement;
      stringsplit(nodepath, XML2PATHSEPARATOR, pathes);
      // walk the path
      // The Path looks like this:
      // nodedescription XML2PATHSEPARATOR nodedescripton (and so on)
      // nodedescription looks like this
      // nodename attributeName="attributeValue"  or
      // nodename                          (node without attributes)
      // (!) attributeValue may contain a XML2PATHSEPARATOR string
      // XML2PATHSEPARATOR = ' // '
      i := 1;

      found := True;
      leavingPath := nodePath;
      logdatei.log_prog('begin to open nodepath  : ' + nodepath, LLinfo);
      logdatei.log_prog('-- pathes.Count: ' + IntToStr(pathes.Count), LLinfo);
      while i < pathes.Count + 1 do
      begin
        attributeList.Clear;
        logdatei.log_prog('path element ' + IntToStr(i) + ' : ' + pathes[i - 1], LLinfo);
        thisnodeName := Trim(copy(pathes[i - 1], 1, pos(' ', pathes[i - 1]) - 1));
        logdatei.log_prog('thisnodename ' + thisnodeName, LLinfo);
        leavingPath := copy(pathes[i - 1], pos(' ', pathes[i - 1]) +
          1, length(pathes[i - 1]));
        logdatei.log_prog('leavingPath ' + leavingPath, LLinfo);
        if (pos('=', pathes[i - 1]) > 0) then // only in this case attributes
        begin
          // split on blank, list of attributes
          if not makeAttributesSL(attributesSL, leavingPath, errormessage) then
          begin
            // handle error
            errorinfo := errormessage;
            Result := False;
            attributeList.Free;
            exit;
          end;
          for k := 0 to attributesSL.Count - 1 do
            logdatei.log_prog('Attribute ' + attributesSL[k], LLinfo);
          logdatei.log_prog('Anzahl Attribute ' + IntToStr(attributesSL.Count), LLinfo);
          j := 0;
          while j < attributesSL.Count do
          begin
            // List of [attributename, attributevalue]
            attributeList.Add;
            attributeList.Items[j].key :=
              Trim(copy(attributesSL[j], 1, pos('=', attributesSL[j]) - 1));
            attributeList.Items[attributeList.Count - 1].Value :=
              Trim(copy(attributesSL[j], pos('=', attributesSL[j]) +
              1, length(attributesSL[j])));
            if AnsiStartsStr('"', attributeList.Items[j].Value) then
              attributeList.Items[j].Value :=
                copy(attributeList.Items[j].Value, 2,
                length(attributeList.Items[j].Value));
            if AnsiEndsStr('"', attributeList.Items[j].Value) then
              attributeList.Items[j].Value :=
                Trim(copy(attributeList.Items[j].Value, 1,
                length(attributeList.Items[j].Value) - 1));
            // ????
          (*
          leavingPath := copy(leavingPath, pos(' ', leavingPath) + 1, length(leavingPath));
          if pos('"' + XML2PATHSEPARATOR, leavingPath) > 0 then
            attributeList.Items[attributeList.Count-1].value := copy(leavingPath, 1, pos('"' + XML2PATHSEPARATOR, leavingPath))
          else
            attributeList.Items[attributeList.Count-1].value := leavingPath;
          *)
            Inc(j);
          end;
        end
        else
          thisnodeName := Trim(pathes[i - 1]);

        logdatei.log_prog('node ' + IntToStr(i) + ': nodename ' + thisnodename
          , LLinfo
          );
        if attributes_strict then
        begin
          if not getNodeStrict(nodesInPath[i], nodesInPath[i - 1],
            thisnodeName, attributeList) then
          begin
            found := False;
            LogDatei.log('opennode: node with attributes_strict not found ' +
              IntToStr(i) + ': nodename: ' + thisnodeName +
              ', check nodename and attributes - exit function', LLInfo);
            // failed - make all final settings
            Result := False;
            attributeList.Free;
            exit;
          end
          else
          begin
            found := True;
            if actnode <> nil then
              logdatei.log('Found node with attributes_strict' +
                IntToStr(i) + ': nodename: ' + actNode.NodeName, LLinfo)
            else
              logdatei.log('Found more then one node ' + IntToStr(i) +
                ': ' + IntToStr(length(actnodeset)) + ' nodes'
                , LLinfo);
          end;
        end
        else  // not strict!
        begin
          // we ignore always attributes
          //if attributeList.Count = 0 then
          begin
            attributeList.Add;
            attributeList.Items[0].key := '';
            attributeList.Items[0].Value := '';
          end;
          //else  attributeList contains one pair

          if not getNodeByNameAndAttribute(nodesInPath[i], nodesInPath[i - 1],
            thisnodeName, attributeList.Items[0].key, attributeList.Items[0].Value) then
          begin
            found := False;
            LogDatei.log('opennode: node not found ' + IntToStr(i) +
              ': nodename: ' + thisnodeName, LLInfo
              );
          end
          else
          begin
            found := True;
            logdatei.log('Found node ' + IntToStr(i) + ': nodename: ' +
              actNode.NodeName, LLinfo);
          end;
        end;
        Inc(i);
      end;

      // ??? hier noch unklar
      if found and (actnode <> nil) then  // ????
      begin
        //actNode := nodesInPath[i - 1];
        LogDatei.log('actNode know node ' + IntToStr(i - 1) +
          ': nodename: ' + actNode.NodeName, LLinfo
          // TODO if multiple nodes with same textcontent, continous string of text content -
          );
      end
      else
      begin
        // was soll actNode sein?
        actNode := nil;
        LogDatei.log('actNode=nil; opennode: node not found, maybe ' +
          IntToStr(i - 1) + ': nodename: ' + thisnodename, LLInfo
          );
      end;
      Result := found;
      attributeList.Free;
    except
      on e: Exception do
      begin
        Result := False;
        LogDatei.log('Could not open ' + nodepath + '; exception: ' +
          e.Message, LLerror);
      end;
    end;
  finally
    FreeAndNil(attributesSL);
    cleanupactnodeset();
  end;
end;

function TuibXMLDocument.makeNodePathWithTextContent(nodePath: string;
  Text: string; var errorinfo: string): boolean;
var
  nodesInPath: array[0..50] of TDOMNode;
  attributesSL, pathes: TStringList;
  i, j, k: integer;
  found: boolean;
  leavingPath, thisnodeName: string;
  attributeList: TList;
  errormessage: string;
begin
  LogDatei.log('begin to make node with path: ' + nodePath +
    ' and  TEXT_CONTENT: ' + textContent, LLinfo);
  makeNodePathWithTextContent := False;
  setTopNodeAsActNode();
  makeTopAsActNodeSet();
  Result := True;
  attributeList := TList.Create;
  attributesSL := TStringList.Create;
  try
    try
      // the root node
      nodesInPath[0] := XML.DocumentElement;
      stringsplit(nodepath, XML2PATHSEPARATOR, pathes);
      // walk the path and create all nodes
      // The Path looks like this:
      // nodedescription XML2PATHSEPARATOR nodedescripton (and so on)
      // nodedescription looks like this
      // nodename attributeName="attributeValue"  or
      // nodename                          (node without attributes)
      // (!) attributeValue may contain a XML2PATHSEPARATOR string
      // XML2PATHSEPARATOR = ' // '
      i := 1;

      found := True;
      leavingPath := nodePath;
      logdatei.log('begin to open nodepath  : ' + nodepath, LLinfo);
      logdatei.log_prog('-- pathes.Count: ' + IntToStr(pathes.Count), oslog.LevelInfo);
      while i < pathes.Count + 1 do
      begin
        attributeList.Clear;
        logdatei.log_prog('path element ' + IntToStr(i) + ' : ' + pathes[i - 1], LLinfo);
        thisnodeName := Trim(copy(pathes[i - 1], 1, pos(' ', pathes[i - 1]) - 1));
        logdatei.log_prog('thisnodename ' + thisnodeName, LLinfo);
        leavingPath := copy(pathes[i - 1], pos(' ', pathes[i - 1]) +
          1, length(pathes[i - 1]));
        logdatei.log_prog('leavingPath ' + leavingPath, LLinfo);
        if (pos('=', pathes[i - 1]) > 0) then // only in this case attributes
        begin
          // split on blank, list of attributes
          if not makeAttributesSL(attributesSL, leavingPath, errormessage) then
          begin
            // handle error
            errorinfo := errormessage;
            Result := False;
            attributeList.Free;
            exit;
          end;
          for k := 0 to attributesSL.Count - 1 do
            logdatei.log_prog('Attribute ' + attributesSL[k], LLinfo);
          logdatei.log_prog('Anzahl Attribute ' + IntToStr(attributesSL.Count), LLinfo);
          j := 0;
          while j < attributesSL.Count do
          begin
            // List of [attributename, attributevalue]
            attributeList.Add;

            attributeList.Items[j].key :=
              Trim(copy(attributesSL[j], 1, pos('=', attributesSL[j]) - 1));
            logdatei.log_prog(' attributeList.Items[j].key ' +
              attributeList.Items[j].key, LLinfo);
            attributeList.Items[attributeList.Count - 1].Value :=
              Trim(copy(attributesSL[j], pos('=', attributesSL[j]) +
              1, length(attributesSL[j])));
            if AnsiStartsStr('"', attributeList.Items[j].Value) then
              attributeList.Items[j].Value :=
                copy(attributeList.Items[j].Value, 2,
                length(attributeList.Items[j].Value));
            if AnsiEndsStr('"', attributeList.Items[j].Value) then
              attributeList.Items[j].Value :=
                Trim(copy(attributeList.Items[j].Value, 1,
                length(attributeList.Items[j].Value) - 1));
            Inc(j);
          end;
        end
        else
          thisnodeName := Trim(pathes[i - 1]);

        logdatei.log_prog('node ' + IntToStr(i) + ': nodename ' + thisnodename
          , LLinfo
          );


        if actNode <> nil then
          logdatei.log('actnode: ' + actnode.NodeName, LLinfo)
        else
          logdatei.log('actnode: is nil', LLinfo);

        if found then
        begin
          if (not getNodeStrict(nodesInPath[i], nodesInPath[i - 1],
            thisnodeName, attributeList)) then
          begin
            found := False;
            actNode := nodesInPath[i - 1];
            LogDatei.log('makeNodePathWithTextContent: node not found ' +
              IntToStr(i) + ': nodename: ' + thisnodeName +
              ', Node will be created', LLInfo
              );
            makeNode(thisnodeName);
            k := 0;
            while k < attributeList.Count do
            begin
              addAttribute(attributeList.Items[k].key, attributeList.Items[k].Value);
              Inc(k);
            end;
          end
          else
          begin
            found := True;
            if actnode <> nil then
              logdatei.log('Found node ' + IntToStr(i) + ': nodename: ' +
                actNode.NodeName, LLinfo)
            else
              logdatei.log('Found more then one node ' + IntToStr(i) +
                ': ' + IntToStr(length(actnodeset)) + ' nodes'
                , LLinfo);
          end;
        end
        else
        begin
          LogDatei.log('makeNodePathWithTextContent: node not found ' +
            IntToStr(i) + ': nodename: ' + thisnodeName +
            ', Node will be created', LLInfo
            );
          makeNode(thisnodeName);
          k := 0;
          while k < attributeList.Count do
          begin
            addAttribute(attributeList.Items[k].key, attributeList.Items[k].Value);
            Inc(k);
          end;
        end;
        Inc(i);
      end;
      if Assigned(attributeList) then
        attributeList.Free;

      // ??? hier noch unklar, actNode sollte der letzte Knoten sein
      if (actnode <> nil) then  // ????
      begin
        //actNode := nodesInPath[i - 1];
        LogDatei.log('actNode know node ' + IntToStr(i - 1) +
          ': nodename: ' + actNode.NodeName, LLinfo
          );
        actNode.TextContent := Text;
      end
      else
      begin
        // was soll actNode sein?
        actNode := nil;
        LogDatei.log('actNode=nil; makeNodePathWithTextContent: node not found, maybe ' +
          IntToStr(i - 1) + ': nodename: ' + thisnodename, LLInfo
          );
      end;
    except
      on e: Exception do
      begin
        Result := False;
        LogDatei.log('makeNodePathWithTextContent: Error in creating path ' +
          nodepath + ' with textcontent' + Text + '; exception: ' + e.Message, LLerror);
      end;
    end;
    cleanupactnodeset();

  finally
    if Assigned(attributesSL) then
      FreeAndNil(attributesSL);
    if Assigned(pathes) then
      pathes.Free;
  end;
end;

function TuibXMLDocument.makeNode(mynodeName: string): boolean;
  // create new node, append to actNode, set newnode as actNode
var
  newnode: TDOMNode;
begin
  LogDatei.log('begin to make node with nodename: ' + mynodeName, LLinfo);
  makeNode := False;
  try
    if actNode <> nil then
    begin
      newnode := XML.CreateElement(mynodeName);
      actNode.AppendChild(newnode);
      actNode := newnode;
      makeNode := True;
    end
    else
      LogDatei.log('actNode is nil. cannot make and append node', LLwarning);
  except
    LogDatei.log('error to make node with nodename: ' + mynodeName, LLerror);
  end;
end;

function TuibXMLDocument.makeNode(mynodeName, attributeName, attributeValue:
  string): boolean;
  // create new node, append to actNode, set newnode as actNode
var
  newnode: TDOMNode;
begin
  LogDatei.log('begin to make node with nodename: ' + mynodeName +
    ' attributeName: ' + attributeName + ' attributeValue: ' + attributeValue, LLinfo
    );
  makeNode := False;
  try
    if actNode <> nil then
    begin
      newnode := XML.CreateElement(mynodeName);
      if attributeName <> '' then
      begin
        TDOMElement(newnode).SetAttribute(attributeName, attributeValue);
      end;
      actNode.AppendChild(newnode);
      actNode := newnode;
      makeNode := True;
    end
    else
      LogDatei.log('actNode is nil. cannot make and append node', LLwarning);
  except
    LogDatei.log('error to make node with nodename: ' + mynodeName +
      ' attributeName: ' + attributeName + ' attributeValue: ' + attributeValue, LLerror
      );
  end;
end;

function TuibXMLDocument.makeNodeAndKeepActNode(mynodeName: string): boolean;
  // create new node, append to actNode, actNode is kept
var
  newnode: TDOMNode;
begin
  LogDatei.log('begin to make node with nodename: ' + mynodeName, LLinfo);
  makeNodeAndKeepActNode := False;
  try
    if actNode <> nil then
    begin
      newnode := XML.CreateElement(mynodename);
      actNode.AppendChild(newnode);
      makeNodeAndKeepActNode := True;
    end
    else
      LogDatei.log('actNode is nil. cannot make and append node', LLwarning);
  except
    LogDatei.log('error to make node with nodename: ' + mynodeName, LLerror);
  end;
end;

function TuibXMLDocument.makeNodeAndKeepActNode(mynodeName, attributeName,
  attributeValue, Text: string): boolean;
  // create new node, append to actNode, actNode is kept
var
  newnode: TDOMNode;
begin
  LogDatei.log('begin to make node with nodename: ' + mynodeName +
    ' attributeName: ' + attributeName + ' attributeValue: ' + attributeValue, LLinfo
    );
  makeNodeAndKeepActNode := False;
  try
    if actNode <> nil then
    begin
      newnode := XML.CreateElement(mynodename);
      newnode.TextContent := Text;
      if attributeName <> '' then
      begin
        TDOMElement(newnode).SetAttribute(attributeName, attributeValue);
      end;
      actNode.AppendChild(newnode);
      makeNodeAndKeepActNode := True;
    end
    else
      LogDatei.log('actNode is nil. cannot make and append node', LLwarning);
  except
    LogDatei.log('error to make node with nodename: ' + mynodeName +
      ' attributeName: ' + attributeName + ' attributeValue: ' + attributeValue, LLerror
      );
  end;
end;

function TuibXMLDocument.makeNodes
  (mynodeName: string; const textArray: TStringList): boolean;
  // creates couple of nodes in actnode with equal names
  // and different TextContent, actNode will be same after creation
var
  index: integer;
begin
  if textArray.Count > 0 then
  begin
    makeNodes := True;
    for index := 0 to textArray.Count - 1 do
    begin
      makeNodeAndKeepActNode(mynodeName, '', '', textArray[index]);
    end;
  end
  else
  begin
    LogDatei.log('length of Array is 0: ', LLerror);
    makeNodes := False;
  end;
end;

// overload!!!
procedure TuibXMLDocument.delNode(nodePath: string; attributes_strict: boolean;
  var errorinfo: string);
var
  removeNode: TDOMNode;
begin
  LogDatei.log('delnode: strict: ' + BoolToStr(attributes_strict, True) + ' del Node: ' +
    nodePath, LLInfo);
  openNode(nodePath, attributes_strict, errorinfo);
  if actNode <> nil then
  begin
    removeNode := actNode;
    actNode := actNode.ParentNode;
    try
      removenode.Free;
      LogDatei.log('delNode: ' + nodePath + '. Node was  removed', LLInfo);
    except
      LogDatei.log('Error in delNode: ' + nodePath +
        '. Node was not removed', LLerror);
    end;
  end
  else
    LogDatei.log('Error in delNode: ' + nodePath + ', node not found', LLInfo);
end;

procedure TuibXMLDocument.delNode;
// aktnode (and all childs) will be deleted, afterwards parentnode will be aktnode
// before del node check node with nodeExists
var
  removeNode: TDOMNode;
begin
  if actNode <> nil then
  begin
    LogDatei.log('begin to del Node: ' + actNode.NodeName, LLinfo);
    removeNode := actNode;
    actNode := actNode.ParentNode;
    try
      removenode.Free;
    except
      LogDatei.log('Error in delNode. Node was not removed', LLerror);
    end;
  end
  else
    LogDatei.log('delNode: actNode is nil', LLwarning);
end;

procedure TuibXMLDocument.setTopNodeAsActNode();
// set top node as actual node
begin
  actNode := getDocumentElement();
end;

procedure TuibXMLDocument.setFirstChildAsActNode();
// set first child as actual node
begin
  if actNode <> nil then
    actNode := actNode.FirstChild;
end;

procedure TuibXMLDocument.setParentNodeAsActNode();
// set parent node as actual node
begin
  if actNode <> nil then
    actNode := actNode.ParentNode;
end;

procedure TuibXMLDocument.setNodeTextActNode(Text: string);
begin
  LogDatei.log('begin to set text to actNode: ' + Text, LLinfo);
  if actNode <> nil then
  begin
    actNode.TextContent := Text;
    LogDatei.log('setNodeTextActNode - TextContent is: ' +
      actNode.TextContent, LLinfo);
  end
  else
    LogDatei.log('actNode is nil, text not set: ' + Text, LLwarning);
end;

function TuibXMLDocument.setActNodeIfText(textvalue: string): boolean;
  // get first and hopefully unique node with text_content = nodeText
  // set this Node as actNode

var
  i, j, n, basejindex: integer;
  comparetext: string;
  myparentNode: TDOMNode;

begin
  myparentNode := actNode;
  setActNodeIfText := False;
  LogDatei.log('set child element of aktnode as aktnode, text has to be "' +
    textvalue + '"', oslog.LLinfo);

  basejindex := 0;
  i := 0;
  while i < length(actNodeSet) do
  begin

    if actNodeSet[i] <> nil then
    begin

      n := actNodeSet[i].childnodes.Count;

      for j := 0 to n - 1 do
      begin
        actnode := actNodeSet[i].childnodes.Item[j];
        if actnode.NodeType = ELEMENT_NODE then
          comparetext := actnode.TextContent
        else
          comparetext := '';

        if AnsiCompareStr(textvalue, comparetext) = 0 then
        begin
          FderivedNodeSet[basejindex + j] := actnode;
        end

        else
          FderivedNodeSet[basejindex + j] := nil;
      end;

      basejindex := basejindex + n;

    end;

    Inc(i);
  end;
  cleanUpDerivedNodeSet();
  if getCountDerivedNotNil() > 0 then
  begin
    actnode := FderivedNodeSet[0];
    setActNodeIfText := True;
    if length(FderivedNodeSet) > 1 then
      LogDatei.log('more than one item has TEXT_CONTENT ' + textvalue +
        ', the first item becomes actNode', oslog.LLwarning)
    else
      LogDatei.log('item with TEXT_CONTENT ' + actNode.TextContent +
        ' found, this item becomes actNode', oslog.LLinfo);
  end
  else
  begin
    setActNodeIfText := False;
    actNode := myparentNode;
    LogDatei.log('none of the items has TEXT_CONTENT ' + textvalue +
      ',  name of actNode is ' + actNode.NodeName, oslog.LLwarning);
  end;
end;

function TuibXMLDocument.getNodeStrict(var newNode: TDOMNode;
  myparentNode: TDOMNode; mynodeName: string; attributeList: TList): boolean;
  // private
  // nodename and all attributes have to fit
  // if not found: actnode = myparentnode - result false
  // if found and unique : actnode = node found - result true
  // if found and not unique : actnode = nil, check actnodeset - result true
var
  n, j, i, al: integer;
  attributename, attributevalue: string;
  actattributename, actattributevalue: string;
  boolarray: array [0..9] of boolean;
  namefound, attributesfound: boolean;
  attributecount1, attributecount2: integer;
begin
  Result := False;

  logdatei.log('begin to get node  nodename: ' + mynodename +
    ' with attributes: ', LLinfo);
  for al := 0 to attributeList.Count - 1 do
    logdatei.log(attributelist.Items[al].key + ' : ' +
      attributelist.Items[al].Value, LLinfo);
  // find nodes with name
  namefound := False;
  try
    if (myparentNode <> nil) then
    begin
      j := 0;
      i := 0;
      // find nodes with nodename
      while (myparentNode.hasChildNodes) and
        (j < myparentNode.ChildNodes.Count) do
      begin
        if (myparentNode.ChildNodes.Item[j].NodeName = mynodeName) then
        begin
          namefound := True;
          //newNode := myparentNode.ChildNodes.Item[j];
          setLengthActNodeset(i + 1);
          actNode := myparentNode.ChildNodes.Item[j];
          actNodeSet[i] := actNode;
          Inc(i);
        end;
        Inc(j);
      end;
    end;
  finally
  end;

  logdatei.log_prog('node(s) found with name ' + mynodeName + ': ' +
    IntToStr(length(actNodeSet)), oslog.LLinfo);

  if namefound then
  begin
    //result:= true;
    if attributeList.Count < 10 then  // only 10 attributes
    begin
      j := 0;
      n := 1;
      while j < length(actNodeSet) do  // check attributes for node j
      begin
        for al := 0 to 9 do
          boolarray[al] := False;
        for al := 0 to attributelist.Count - 1 do
          attributelist.Items[al].isvalid := False;
        logdatei.log_prog(' ', oslog.LLinfo);
        logdatei.log_prog(IntToStr(N) + ' -> find attributes for node ' +
          actnodeset[j].NodeName + ', number of attributes ' +
          IntToStr(attributeList.Count)
          , LLinfo);
        if actnodeset[j].HasAttributes then
        begin
          al := 0;
          if attributeList <> nil then
            while (al < attributeList.Count) do  // proof attributes key/value
            begin
              attributename := attributelist.Items[al].key;
              attributevalue := attributelist.Items[al].Value;
              // check attributes
              for i := 0 to actnodeset[j].Attributes.Length - 1 do
              begin
                actattributename := actnodeset[j].Attributes[i].NodeName;
                actattributevalue := actnodeset[j].Attributes[i].TextContent;
                if (actattributename = attributeName) and
                  (actattributevalue = attributeValue) then
                begin
                  logdatei.log_prog('attribut found ' + attributename +
                    ' ' + attributevalue, LLinfo);
                  boolarray[al] := True;
                  attributelist.Items[al].isvalid := True;
                end;
                //else attributelist.Items[al].isvalid:=false;
              end;
              Inc(al);
            end;

        end; // has attributes

        logdatei.log_prog('all attributes have to fit, nodename ' +
          actnodeset[j].NodeName, oslog.LLinfo);

        attributecount1 := attributeList.Count;
        attributecount2 := actnodeset[j].Attributes.Length;
        if (attributecount1 = attributecount2) then // check if all attributes fit
        begin
          attributesfound := True;
          // to max(attributeList.Count-1, myparentNode.ChildNodes.Item[j].Attributes.Length - 1)
          for al := 0 to attributeList.Count - 1 do
          begin
            if attributelist.Items[al].isvalid then
              logdatei.log_prog('key/value found ' + attributelist.Items[al].key +
                ':' + attributelist.Items[al].Value, oslog.LLinfo)
            else
              logdatei.log_prog('key/value not found ' +
                attributelist.Items[al].key + ':' + attributelist.Items[al].Value,
                oslog.LLDebug2);
            attributesfound := attributesfound and boolarray[al];
          end;
          if not attributesfound then
          begin
            logdatei.log_prog('one or more attributes does not match, nodename ' +
              actnodeset[j].NodeName, oslog.LLinfo);
            // remove node
            actnodeset[j] := nil;
            j := j - 1;
          end;
        end
        else // strict: identical number of attributes!
        begin
          logdatei.log('Attribute count mismatch: given by path: ' +
            IntToStr(attributecount1) + ' but node has: ' +
            IntToStr(attributecount2), oslog.LLDebug);
          actnodeset[j] := nil;
          j := j - 1;
        end;
        cleanupactnodeset();
        Inc(j);
        Inc(n);
      end;  // end traverse actNodeSet

      logdatei.log_prog('actnodeset after retrieving key/value ', oslog.LLinfo);
      logActNodeSet;
      if length(actnodeset) > 1 then
      begin
        logdatei.log('There is more than one matching node here - just taking the first',
          oslog.LLWarning);
        for j := 1 to length(actnodeset) - 1 do
          actnodeset[j] := nil;
        cleanupactnodeset();
      end;
      if length(actnodeset) = 1 then
      begin
        actnode := actnodeset[0];
        newNode := actnodeset[0];  // ??
        logdatei.log('result true, actNode and newnode is ' +
          actNode.NodeName, oslog.LLinfo);
        Result := True;
      end
      else
      begin
        actnode := nil;
        logdatei.log('result false, actnode is nil, lenght of actNodeSet is ' +
          IntToStr(length(actnodeset)), oslog.LLinfo);
      end;
    end
    else
      logdatei.log('handling of more then 10 attributes not implemented', oslog.LLerror);
  end // end nodename found
  else
  begin
    Result := False;
    actNode := myparentnode;
    logdatei.log('node name not found, actnode is parentnode', oslog.LLinfo);
  end;
end;


function TuibXMLDocument.getNodeTextActNode(): string;
  // get text at the actual node
begin
  getNodeTextActNode := '';
  if actNode <> nil then
  begin
    getNodeTextActNode := actNode.TextContent;
    LogDatei.log('get text from actNode: ' + actNode.TextContent, LLinfo);
  end
  else
    LogDatei.log('actNode is nil, can not get text: ', LLwarning);
end;

function TuibXMLDocument.getNodeNameActNode(): string;
begin
  getNodeNameActNode := '';
  if actNode <> nil then
  begin
    getNodeNameActNode := actNode.NodeName;
    LogDatei.log('get nodeName from actNode: ' + actNode.NodeName, LLinfo);
  end
  else
    LogDatei.log('actNode is nil, can not get nodeName ', LLInfo);
end;

function TuibXMLDocument.getNodeByName(var newNode: TDOMNode;
  myparentNode: TDOMNode; mynodeName: string): boolean;
  // private
var
  j, i: integer;
begin
  getNodeByName := False;
  logdatei.log('begin to get node  nodename: ' + mynodename, LLinfo);
  if (myparentNode <> nil) then
  begin
    j := 0;
    while (myparentNode.hasChildNodes) and (j < myparentNode.ChildNodes.Count) and
      (getNodeByName = False) do
    begin
      // compare nodenames
      if (myparentNode.ChildNodes.Item[j].NodeName = mynodeName) then
      begin
        newNode := myparentNode.ChildNodes.Item[j];
        actNode := myparentNode.ChildNodes.Item[j];
        getNodeByName := True;
      end;
      Inc(j);
    end;
  end
  else
  begin
    logdatei.log('parentnode not valid', oslog.LLInfo);
    newNode := nil;
  end;

end;

function TuibXMLDocument.getNodeByNameAndAttribute(var newNode: TDOMNode;
  myparentNode: TDOMNode; mynodeName: string; attributename: string;
  attributevalue: string): boolean;
  // private
  // for name and only one attribute
var
  j, i: integer;
  Name, Text: string;
  haschilds: boolean;
  childcount: integer;
begin
  getNodeByNameAndAttribute := False;
  logdatei.log('begin to get node  nodename: ' + mynodename +
    ' with attributes: ', LLinfo);

  try
    if Assigned(myparentNode) then
    begin
      j := 0;
      haschilds := myparentNode.hasChildNodes;
      childcount := myparentNode.ChildNodes.Count;
      while (haschilds) and (j < childcount) and
        (getNodeByNameAndAttribute = False) do
      begin
        // compare attributes if given in parameter and existing
        if (myparentNode.ChildNodes.Item[j].NodeName = mynodeName) then
          if (myparentNode.ChildNodes.Item[j].HasAttributes) and
            (attributename <> '') then
          begin
            for i := 0 to myparentNode.ChildNodes.Item[j].Attributes.Length - 1 do
            begin
              Name := myparentNode.ChildNodes.Item[j].Attributes[i].NodeName;
              Text := myparentNode.ChildNodes.Item[j].Attributes[i].TextContent;
              if (Name = attributeName) and (Text = attributeValue) then
              begin
                newNode := myparentNode.ChildNodes.Item[j];
                actNode := myparentNode.ChildNodes.Item[j];
                getNodeByNameAndAttribute := True;
              end;
            end;
          end
          else   // also found if there are no attributes but name found
          begin
            newNode := myparentNode.ChildNodes.Item[j];
            actNode := myparentNode.ChildNodes.Item[j];
            getNodeByNameAndAttribute := True;
          end;
        Inc(j);
      end;
    end
    else
    begin
      logdatei.log('parentnode not valid', oslog.LLInfo);
      newNode := nil;
    end;
  except
    logdatei.log('getNodeByNameAndAttribute failed', oslog.LLerror);
  end;
end;

function TuibXMLDocument.getNodeByNameAndTextContent(var newNode: TDOMNode;
  myparentNode: TDOMNode; mynodeName: string; mynodeTextContent: string): boolean;
  // nodename and text has to fit

var
  j, i: integer;
  haschilds: boolean;
  childcount: integer;
begin
  getNodeByNameAndTextContent := False;
  logdatei.log('begin to get node nodename: ' + mynodename + ' with text: ' +
    mynodeTextContent, LLinfo);

  try
    if Assigned(myparentNode) then
    begin
      j := 0;
      haschilds := myparentNode.hasChildNodes;
      childcount := myparentNode.ChildNodes.Count;
      while (haschilds) and (j < childcount) and
        (getNodeByNameAndTextContent = False) do
      begin
        // compare text if given
        if (myparentNode.ChildNodes.Item[j].NodeName = mynodeName) then
          if (myparentNode.ChildNodes.Item[j].TextContent = mynodeTextContent) and
            (mynodeTextContent <> '') then
          begin
            newNode := myparentNode.ChildNodes.Item[j];
            actNode := myparentNode.ChildNodes.Item[j];
            getNodeByNameAndTextContent := True;
          end;
        Inc(j);
      end;
    end
    else
      logdatei.log('parentnode not valid', oslog.LLerror);
  except
    logdatei.log('getNodeByNameAndTextContent failed', oslog.LLerror);
  end;
end;

//*************  Node-Operations on dicts *****************************
// TODO
function TuibXMLDocument.setNodePair(keyNodeName: string;
  keyNodeTextContent: string; valueNodeName: string;
  valueNodeTextContent: string): boolean;
var
  myparentNode, newNode, oldNode: TDOMNode;
begin
  setNodePair := False;
  logdatei.log('starting setNodePair', oslog.LLinfo);
  if (actNode <> nil) then
  begin
    if (actNode.NodeName = 'dict') then
    begin
      // testing string
      //keyNodeName := 'key';
      //keyNodeTextContent := 'CFBundleShortVersionString';
      //valueNodeName := 'string';
      //valueNodeTextContent := '4.12.4.11';

      // testing boolean
      //keyNodeName := 'key';
      //keyNodeTextContent := 'NSAppleScriptEnabled';
      //valueNodeName := 'false';

      // testing create node with key string
      //keyNodeName := 'key';
      //keyNodeTextContent := 'testCFBundleShortVersionString';
      //valueNodeName := 'string';
      //valueNodeTextContent := 'test4.12.4.11';

      // testing create node boolean
      // keyNodeName := 'key';
      // keyNodeTextContent := 'testNSAppleScriptEnabled';
      // valueNodeName := 'false';

      if getNodeByNameAndTextContent(newNode, actNode, keyNodeName,
        keyNodeTextContent) then
      begin
        LogDatei.log('setNodePair: key ' + keyNodeName + ' with TextContent ' +
          keyNodeTextContent + ' found', LLinfo);
        // nächsten Knoten prüfen, ob er als key den ValueNodeName hat
        if (UpperCase(valueNodeName) = UpperCase('string')) then
        begin
          actNode := actNode.NextSibling;
          LogDatei.log('setNodePair: valueNodeName ' + valueNodeName +
            ' actNode.NodeName: ' + actNode.NodeName, LLinfo);
          setNodeTextActNode(valueNodeTextContent);
          LogDatei.log('setNodePair - string: valueNodeTextContent set, now ' +
            actNode.TextContent, LLinfo);
        end
        else if (UpperCase(valueNodeName) = UpperCase('true')) or
          (UpperCase(valueNodeName) = UpperCase('false')) then
        begin
          // create new node with value and replace old node
          if (UpperCase(valueNodeName) = UpperCase('string')) then
          begin
            LogDatei.log('boolean setNodePair: make valueNodeName ' +
              valueNodeName, LLinfo);
            LogDatei.log('boolean setNodePair: actNode NodeName is ' +
              actNode.NodeName, LLinfo);
            myparentNode := actNode.ParentNode;
            oldNode := actNode.NextSibling;
            LogDatei.log('boolean setNodePair: oldNode NodeName is ' +
              oldNode.NodeName, LLinfo);
            makeNode(valueNodeName); // new node is actNode
            myparentNode.ReplaceChild(actNode, oldNode);
            LogDatei.log('setNodePair - true/false: valueNodeName set, now ' +
              actNode.NodeName, LLinfo);
          end;
        end
        else if (UpperCase(valueNodeName) = UpperCase('array')) then
        begin
          // TODO ???
        end
        else
          LogDatei.log('setNodePair: valueNode NOT set ' + valueNodeName, LLinfo);
      end
      else
      begin
        LogDatei.log('setNodePair: key ' + keyNodeName + ' with TextContent ' +
          keyNodeTextContent + ' NOT found', LLinfo);
        if (UpperCase(valueNodeName) = UpperCase('string')) then
        begin
          LogDatei.log('setNodePair: creating node ' +
            keyNodeName + ' with TextContent ' + keyNodeTextContent, LLinfo);
          makeNode(keyNodeName);     // new node ist actNode and was added as last node
          setNodeTextActNode(keyNodeTextContent);
          // make valueNode as next node and next sibling, before actNode has to be dict
          setParentNodeAsActNode();
          makeNode(valueNodeName);  // new node ist actNode and was added as last node
          setNodeTextActNode(valueNodeTextContent);
        end
        else if (UpperCase(valueNodeName) = UpperCase('true')) or
          (UpperCase(valueNodeName) = UpperCase('false')) then
        begin
          LogDatei.log('setNodePair: key ' + keyNodeName + ' with TextContent ' +
            keyNodeTextContent + ' NOT found', LLinfo);
          LogDatei.log('setNodePair: creating node ' +
            keyNodeName + ' with TextContent ' + keyNodeTextContent, LLinfo);
          makeNode(keyNodeName);     // new node ist actNode and was added as last node
          setNodeTextActNode(keyNodeTextContent);
          // make valueNode as next node and next sibling, before actNode has to be dict
          setParentNodeAsActNode();
          makeNode(valueNodeName);  // new node ist actNode and was added as last node
        end
        else if (UpperCase(valueNodeName) = UpperCase('array')) then
        begin
          // TODO ???
          LogDatei.log('setNodePair is not implemented for valueNodeName: ' +
            valueNodeName, LLwarning);
          LogDatei.log('setNodePair: node NOT created, valueNodeName: ' +
            valueNodeName, LLinfo);
        end
        else
        begin
          LogDatei.log('setNodePair is not implemented for valueNodeName: ' +
            valueNodeName, LLwarning);
          LogDatei.log('setNodePair: node NOT created, valueNodeName: ' +
            valueNodeName, LLinfo);
        end;
      end;
      setParentNodeAsActNode();
    end
    else
      LogDatei.log('setNodePair only for dicts. node is not dict ' +
        actNode.NodeName, LLWarning);
  end
  else
    LogDatei.log('setNodePair failed, actNode is nil, tried to get keyNodeName: ' +
      keyNodeName, LLerror);
end;

//*************  XML Attribute-Handling ***********************************
function TuibXMLDocument.countAttributes(myxmlnode: TDOMNode): integer;
begin
  if myxmlnode.HasAttributes then
    Result := myxmlnode.Attributes.Length
  else
    Result := 0;
end;

function TuibXMLDocument.hasAttribute(attributename: string): boolean;
var
  i: integer;
begin
  Result := False;
  if (actNode <> nil) then
  begin
    if (actNode.HasAttributes) then
    begin
      for i := 0 to actNode.Attributes.Length - 1 do
      begin
        if (actNode.Attributes[i].NodeName = attributename) then
        begin
          Result := True;
          LogDatei.log('actNode has attribute with name: ' +
            attributeName, LLinfo);
        end;
      end;
    end;
    if Result = False then
      LogDatei.log('actNode has no attribute with name: ' +
        attributeName, LLwarning);
  end
  else
    LogDatei.log('hasAttribute failed, actNode is nil: ' + attributeName, LLerror);
end;

procedure TuibXMLDocument.setAttribute(attributeName, attributeValue: string);
// set if exists, otherwise create
var
  i: integer;
  found: boolean;
begin
  LogDatei.log('begin setAttribute name: ' + attributeName + ', value: ' +
    attributeValue, oslog.LLinfo);
  found := False;
  if (actNode <> nil) then
  begin
    if actNode.HasAttributes then
    begin
      for i := 0 to actNode.Attributes.Length - 1 do
        if actNode.Attributes[i].NodeName = attributeName then
        begin
          actNode.Attributes[i].TextContent := attributevalue;
          LogDatei.log('setAttribute with name: ' + attributeName +
            ' value: ' + attributeValue, LLinfo);
          found := True;
        end;
    end;
    if not found then
    begin
      TDOMElement(actNode).SetAttribute(attributeName, attributeValue);
      LogDatei.log('setAttribute, create attribute with name: ' +
        attributeName + ' value: ' + attributeValue, LLinfo);
    end;
  end
  else
    LogDatei.log('setAttribute failed, actNode ist nil: ' + attributeName +
      ' value: ' + attributeValue, LLerror);
end;

procedure TuibXMLDocument.addAttribute(attributeName, attributeValue: string);
// only add attribute if attribute does not exist
begin
  LogDatei.log('begin to add attribute: name: ' + attributeName +
    ' value: ' + attributeValue, LLinfo);
  if (actNode <> nil) then
    if not (TDOMElement(actNode).hasAttribute(attributeName)) then
      TDOMElement(actNode).SetAttribute(attributeName, attributeValue)
    else
  else
    LogDatei.log('addAttribute failed, actNode ist nil: name' +
      attributeName + ' value: ' + attributeValue, LLerror);
end;

procedure TuibXMLDocument.delAttribute(attributeName: string);
begin
  LogDatei.log('begin to del attribute: name: ' + attributeName, LLinfo);
  if (actNode <> nil) then
    if (actNode.HasAttributes) then
      TDOMElement(actNode).RemoveAttribute(attributeName)
    else
      LogDatei.log('delAttribute failed, name: ' + attributeName +
        'does not exist', LLerror)
  else
    LogDatei.log('delAttribute failed, actNode ist nil: name: ' +
      attributeName, LLerror);
end;


function TuibXMLDocument.getAttributeValue(attributeName: string): string;
  // get the attribute value at the actual node
  // ATTENTION: it returns an empty string:
  // - if there is no attribute with this name
  // - if the value of this attribute is an empty string
begin
  LogDatei.log('begin to get value of attribute: name: ' + attributeName, LLinfo);
  getAttributeValue := '';
  if (actNode <> nil) and actNode.HasAttributes then
    getAttributeValue := TDOMElement(actNode).GetAttribute(attributeName)
  else
    LogDatei.log('getAttribute failed, name: ' + attributeName +
      'does not exist or actNode is nil', LLwarning);

end;

function TuibXMLDocument.makeAttributesSL(var attributeStringList: TStringList;
  attributePath: string; var errormessage: string): boolean;
  // private
var
  i: integer;
  attribute: string;
  leavingpath: string;
  error: boolean;
  localAttributeList: TStringList;
  localAtrribute, value0, value1, value2: string;
  localAtrributeIndex: integer;
  tmpstr: string;
begin
  Result := True;
  try
    localAttributeList := TStringList.Create;
    logdatei.log_prog('makeAttributesSL: attribute path element : ' +
      attributePath, LLinfo);
    //attributeStringList := TStringList.Create;
    // has to be created outside
    if not Assigned(attributeStringList) then
    begin
      errormessage := 'Error: makeAttributesSL: attributeStringList not initialized';
      logdatei.log(errormessage, LLERROR);
      Result := False;
      exit;
    end;
    if Result = True then
    begin
      attributeStringList.Clear;
      i := 1;
      error := False;
      // this will fail with some thing like 'name="Update Check"'
      // so we have to look for the second part
      stringsplitByWhiteSpace(attributePath, localAttributeList);
      //for i := 0 to localAttributeList.Count - 1 do
      localAtrributeIndex := 0;
      while localAtrributeIndex <= localAttributeList.Count - 1 do
      begin
        localAtrribute := trim(localAttributeList[localAtrributeIndex]);
        if (localAtrribute <> '') then
        begin
          // check syntax:
          if pos('=', localAtrribute) > 0 then
          begin
            if pos('"', localAtrribute) > 0 then
            begin
              // we expect that the value is quoted
              // the attribute name with trailing '='
              value0 := trim(copy(localAtrribute, 1, pos('=', localAtrribute)));
              // the attribute value with leading '"'
              value1 := trim(copy(localAtrribute, pos('=', localAtrribute) +
                1, length(localAtrribute)));
              value2 := opsiunquotestr2(value1, '"');
              while (value1 <> '"' + value2 + '"') and
                (localAtrributeIndex <= localAttributeList.Count - 1) do
              begin
                // may be we have a whitespce in the attribute value - let us try to fix
                Inc(localAtrributeIndex);
                tmpstr := trim(localAttributeList[localAtrributeIndex]);
                // do not use the next attribute name
                if pos('=', tmpstr) = 0 then
                begin
                  value1 := value1 + ' ' + tmpstr;
                  value2 := opsiunquotestr2(value1, '"');
                  // we hope the fix is:
                  localAtrribute := value0 + value1;
                  logdatei.log_prog('makeAttributesSL: Test attribute: ' +
                    localAtrribute, LLdebug);
                end;
              end;
              if value1 <> '"' + value2 + '"' then
              begin
                errormessage :=
                  'Error: makeAttributesSL: attributePath syntax Error: quoting error in: '
                  + attributePath + ' value is. ' + value1;
                logdatei.log(errormessage, LLERROR);
                Result := False;
                exit;
              end;
            end;
          end;
        end
        else
        begin
          errormessage :=
            'Error: makeAttributesSL: attributePath syntax Error: missing equal sign error in: '
            + attributePath + ' value is. ' + value1;
          logdatei.log(errormessage, LLERROR);
          Result := False;
          exit;
        end;
        if Result then
        begin
          logdatei.log('makeAttributesSL: Found attribute: ' + localAtrribute, LLdebug);
          AttributeStringList.Add(localAtrribute);
        end;
        Inc(localAtrributeIndex);
      end;
    end;
  finally
    localAttributeList.Free;
  end;
end;

//********************************************

function nodeExistsByPathInXMLFile(myfilename, path: string;
  attributes_strict: boolean; var errorinfo: string): boolean;
var
  XMLDocObject: TuibXMLDocument;
begin
  try
    try
      nodeExistsByPathInXMLFile := False;
      // createXMLDoc
      XMLDocObject := TuibXMLDocument.Create;
      XMLDocObject.debuglevel := oslog.LLinfo;
      // open xmlfile
      if XMLDocObject.openXmlFile(myfilename) then
      begin
        LogDatei.log('success: create xmldoc from file: ' + myfilename, oslog.LLinfo);
        if XMLDocObject.nodeExists(path, attributes_strict, errorinfo) then
          nodeExistsByPathInXMLFile := True;
      end
      else
        LogDatei.log('failed: create xmldoc from file: ' + myfilename, oslog.LLError);
    except
      on e: Exception do
      begin
        Result := False;
        LogDatei.log('Exception in nodeExistsByPathInXMLFile: ' + e.message, LLError);
      end;
    end;
  finally
    if Assigned(XMLDocObject) then
      FreeAndNil(XMLDocObject);
  end;
end;


end.
