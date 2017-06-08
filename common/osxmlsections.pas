unit osxmlsections;

// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the General Public License.

// Text of the GPL: http://www.gnu.org/licenses/gpl.html
// Unofficial GPL Translations: http://www.gnu.org/licenses/translations.html

// author: Rupert Roeder, APohl, detlef oertel, m.hammel
// credits: http://www.opsi.org/credits/



interface

uses
  SysUtils, Classes,
  DOM,
  XMLRead,
  XMLWrite,
  osxmltdom,
  //XMLIntf,
  //XMLDoc,
  Dialogs, StrUtils,
  ExtCtrls,
  key_valueCollection,
  oslog;
//xmlpluginfunc,
//xmlpluginsynth,
//pluginlog;


type

  TNodeSet = array of TDOMNode;

  TStringsArray = array of string;
  TbooleansArray = array of boolean;

  TLogCallbackProcedure = procedure(level: integer; logmsg: string) of object;

  TuibXMLDocument = class(TXMLDocument)
  private
    XML: TXMLDocument;
    actNode: TDOMNode;

    fActNodeSet: TNodeSet;
    fDerivedNodeSet: TNodeSet;
    fDebugLevel: integer;

    //function getCountNotNil : Integer;
    function getCountDerivedNotNil: integer;

    // nodename has to fit and all attributes
    function getNodeStrict(var newNode: TDOMNode; myparentNode: TDOMNode;
      mynodeName: string; attributeList : TList): boolean;
    // nodename has to fit, one attribute can fit
    function getNode(var newNode: TDOMNode; myparentNode: TDOMNode;
      mynodeName: string; attributename: string; attributevalue : string): boolean;

    {
    function makeNodeAtPos(var newNode:TDOMNode; myparentNode:TDOMNode;
                         mynodeName, attributeName, attributeValue : string;
                         Position : integer):boolean;
    }

    procedure makeAttributesSL (var attributeStringList: TStringList; attributePath : String);

    procedure ErrorHandler(E: EXMLReadError);


  public
    destructor Destroy; override;
    function createXmlDocFromStringlist(docstrlist: TStringList): boolean;
    function makeNode(mynodeName, attributeName, attributeValue: string): boolean;
    function getDocumentElement(): TDOMNode;
    function getCountNotNil: integer;
    property debuglevel: integer read FDebugLevel write FDebugLevel;
    property actNodeSet: TNodeSet read factNodeSet write factNodeSet;
    property derivedNodeSet: TNodeSet read fDerivedNodeSet write fDerivedNodeSet;
    property CountNotNil: integer read getCountNotNil;
    property CountDerivedNotNil: integer read getCountDerivedNotNil;
    procedure setlengthActNodeSet(newlength: integer);
    procedure setlengthDerivedNodeSet(newlength: integer);
    procedure cleanUpActNodeSet ();
    procedure cleanUpDerivedNodeSet ();
    procedure logNodeSets;
    procedure logActNodeSet;

    {
    function makeNodes
                     (mynodeName : String;
                     const attributeNames:TStringsArray;
                     const attributeValues : TStringsArray;
                     mytextcontent : String;
                     index : Integer
                     )
                     :boolean;
    }

    function openXmlFile(filename: string): boolean;
    function writeXmlFile(filename: string): boolean;
    //procedure createXmlFile(filename : string; stringlist : TStringList);

    function getXmlStrings: TStringList;


    procedure getNextGenerationActNodeSet;
    procedure makeNewDerivedNodeSet;
    {
    function filterByAttributeList
                   (var attributenames : Tstringsarray;
                    var attributevalueExists : Tbooleansarray;
                    var attributevalues : TStringsArray;
                    attributes_strict : boolean) : Boolean;
                    }
    function filterByAttributeName_existing (name: string) : Boolean;

    function filterByAttribute_existing (name: string; value : string) : Boolean;

    function filterByText (filtering: boolean; textvalue: string) : Boolean;

    function countAttributes ( myxmlnode: TDOMNode ) : integer;

    function filterByChildElement (filtering: boolean; elementname: string) : Boolean;

    function setActNodeIfText(textvalue: string): boolean;

    // The main idea for the following syntax:

    // The nodePath looks like this:
    // nodedescription PATHSEPARATOR nodedescripton (and so on)
    // nodedescription looks like this:
    // nodename attributeName="attributeValue"
    // (!) attributeValue should be surrounded with ""
    // (!) attributeValue may contain a PATHSEPARATOR string
    // nodeName may include a namespace prefix - no more
    // attributeName may include a namespace prefix -no more
    // (!) namespace prefix are case sensitiv - no more
    // a node without attributes looks like this:
    // nodename =""
    // PATHSEPARATOR string is ' // '

    // eg:
    // foo bar="bar1" // childfoo ="" // RDF:foo NC:bar=nonesense



    function openNode(nodePath: string; attributes_strict: boolean): boolean;
    // open the node
    // if the node not exists it will be created (also parent nodes)
    // makes this node to the actual node

    // TODO: überladene prozedur???
    procedure delNode(nodePath: string); overload;
    // this node (and all childs) will be deleted, afterwards parent will be aktnode

    procedure delNode(); overload;
    // aktnode (and all childs) will be deleted, afterwards parent will be aktnode

    procedure setNodeText(Text: string);
    // set text at the actual node

    procedure setAttribute(attributeName, attributeValue: string);
    // set the attribute at the actual node
    // if it exists it will be changed, otherwis create
    // attributeName may include a namespace prefix - no more


    function hasAttribute(attributename: string): boolean;
    // checks if the actual node has the attribute
    // attributeName may include a namespace prefix

    procedure addAttribute(attributeName, attributeValue: string);
    // add the attribute at the actual node
    // if it exists it will not (!) be changed
    // attributeName may include a namespace prefix - no more

    procedure delAttribute(attributeName: string);
    // delete the attribute  at the actual node
    // attributeName may include a namespace prefix


    function nodeExists(nodePath: string): boolean;
    // tells if a node exists without changing anything


    function getAttributeValue(attributeName: string): string;
    // get the attribute value at the actual node
    // ATTENTION: it returns an empty string:
    // - if there is no attribute with this name
    // - if the value of this attribute is an empty string
    // attributeName may include a namespace prefix - no more

  end;




implementation

const
  PATHSEPARATOR: string = ' // ';
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
      result := num1
   else
      result := num2;
end;
function getTag(line: string): string;
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


procedure setShowErrors(bValue: boolean);
begin
  showErrors := bValue;
end;

{
procedure writelog(level: integer; logmessage: string);
begin
  LogDatei.DependentAdd(logmessage, level);
  {
  //if externLogProc <> nil then
   externLogProc(logmessage, level);
  if showErrors and (level=0) then
  begin
   MessageDlg('Error: '+logmessage, mtError, [mbOK], 0, mbOK);
  end;
  }
end;
}

procedure TuibXMLDocument.ErrorHandler(E: EXMLReadError);
begin
  if E.Severity = esError then
    // wir sind nur in Fehlern bezüglich der Verletzung der DTD interessiert
    LogDatei.log(E.Message, LLerror );
  // an dieser Stelle können wir auch alles andere machen, was bei einem Fehler getan werden sollte
end;


//*************  TuibXMLDocument ***********************************
destructor TuibXMLDocument.Destroy;
begin
  inherited Destroy;
end;






//*************  XML File-Handling ***********************************
{
procedure TuibXMLDocument.createXmlFile(filename: String; stringlist : TStringList);
// TODO
begin
 writelog(fdebuglevel + 1,'begin to create File: '+filename);
 stringlist.SaveToFile(filename);
 writelog(fdebuglevel,'File: '+filename+' created');
 //openXmlFile(filename);
end;
}

function TuibXMLDocument.openXmlFile(filename: string): boolean;
var
  mystream: TFileStream;
begin
  Result := False;
  try
    LogDatei.log( 'try to open File: ' + filename, LLinfo);
    mystream := TFilestream.Create(fileName, fmOpenRead);
    mystream.Position := 0;
    XML := nil;
    LogDatei.log('try to load File: ' + filename, oslog.LLinfo);
    ReadXMLFile(XML, mystream);
    LogDatei.log('File: ' + filename + ' read', oslog.LLinfo);
    Result := True;
  except
    on e: Exception do
      LogDatei.log('Error in readXmlFile : ' + e.Message, oslog.LLerror);
  end;
  if mystream <> nil then
    mystream.Free;
end;



function TuibXMLDocument.writeXmlFile(filename: string): boolean;
  // TODO
var
  myFile: TextFile;
  myXml: string;
  xmlStrings: TStrings;
  i: integer;

begin

  LogDatei.log('begin to save File: ' + filename, LLInfo);
  Result := False;
  try
    WriteXML(XML, filename);
    (*
    LogDatei.log(5,'try to open File: '+filename);
    mystream := TFilestream.Create(fileName,fmOpenWrite);
    mystream.Position := 0;
    XML:=Nil;
    LogDatei.log(5,'try to save File: '+filename);
    WriteXML();
    ReadXMLFile(XML, mystream);
    LogDatei.log(4,'File: '+filename+' read');
    *)
    Result := True;
  except
    on e: Exception do
      LogDatei.log('Error in writeXmlFile : ' + e.Message, oslog.LLerror);
  end;
  //if mystream<>nil then mystream.Free;
  //DatamoduleXML.XMLDocument.saveToFile(FileName);
  //saveToXML(myXml);
  //xmlStrings := TStringList.Create;
  //formatMyXml(myXml,xmlStrings);
  //AssignFile(myFile,filename);
  //SetLineBreakStyle(myFile, tlbsCRLF);
  //rewrite(myFile);
  //for i:=0 to xmlStrings.Count-1 do
  // writeln(myFile,xmlStrings[i]);
  //CloseFile(myFile);
  //xmlStrings.Free;
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
    finally
      nodestream.Free;
    end;
 (*
 var myXml : String;
     xmlStrings : TStrings;
 if Active then
 begin
  saveToXML(myXml);
  xmlStrings := TStringList.Create;
  formatMyXml(myXml,xmlStrings);
  getXmlStrings := xmlStrings;
 end
 else getXmlStrings := TStringList.Create;
 *)
end;

// nodeSet & methods

procedure TuibXMLDocument.setlengthActNodeSet(newlength: integer);
begin
  setlength(factnodeset, newlength);
end;

procedure TuibXMLDocument.setlengthDerivedNodeSet(newlength: integer);
begin
  setlength(fderivednodeset, newlength);
end;

procedure TuibXMLDocument.cleanUpActNodeSet ();
var i, n: integer;
 myNodeSet: TNodeSet;
begin
  n:= length(actnodeset);
  for i := 0 to n - 1 do
  begin
    if actNodeSet[i] <> nil then
    begin
      setlength(myNodeSet, length(mynodeset) + 1);
      mynodeset[length(myNodeSet) - 1] := actnodeset[i];
    end;
  end;
  actnodeset:=mynodeset;
end;

procedure TuibXMLDocument.cleanUpDerivedNodeSet ();
var i, n: integer;
 myNodeSet: TNodeSet;
begin
  n:= length(fderivednodeset);
  for i := 0 to n - 1 do
  begin
    if fderivednodeset[i] <> nil then
    begin
      setlength(myNodeSet, length(mynodeset) + 1);
      mynodeset[length(myNodeSet) - 1] := fderivednodeset[i];
    end;
  end;
  fderivednodeset:=mynodeset;
end;

function TuibXMLDocument.getCountNotNil: integer;
var
  i: integer;
begin
  getCountNotNil := 0;
  for i := 0 to length(actnodeset) - 1 do
  begin
    if actnodeset[i] <> nil then
      Inc(getCountNotNil);
  end;
end;

function TuibXMLDocument.getCountDerivedNotNil: integer;
var
  i: integer;
begin
  getCountDerivedNotNil := 0;
  for i := 0 to length(DerivedNodeSet) - 1 do
  begin
    if derivednodeset[i] <> nil then
      Inc(getCountDerivedNotNil);
  end;
end;
//*************  XML Node-Handling ***********************************
function TuibXMLDocument.createXmlDocFromStringlist(docstrlist: TStringList): boolean;
var
  mystream: TStringStream;
begin
  Result := False;
  LogDatei.log('begin to create XMLDoc ', oslog.LLinfo);
  mystream := TStringStream.Create(docstrlist.Text);
  mystream.Position := 0;
  XML := nil;
  try
    //DOMFromStream(mystream); - unklar ob das sinnvoll ist
    // Beispiel hatte am Anfang einen DTD
    ReadXMLFile(XML, mystream);
    LogDatei.log('XMLDoc created from Stringlist', LLinfo);
    Result := True;
  finally
    mystream.Free;
  end;
  LogDatei.log('XMLDoc created', LLinfo);
end;

function TuibXMLDocument.getDocumentElement(): TDOMNode;
begin
  getDocumentElement := XML.DocumentElement;
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

(*
function TuibXMLDocument.filterByAttributeList
                   (var attributenames : Tstringsarray;
                    var attributevalueExists : Tbooleansarray;
                    var attributevalues : TStringsArray;
                    attributes_strict : boolean) : Boolean;

  var
  i, n, j, basejindex, k, numberOfAttributes: Integer;

  b0, b1, b2 : boolean; // results of the crucial evaluations for each attribute

  goon : Boolean;

begin
  result := true;
  numberOfAttributes := length(attributenames);

  if (numberOfAttributes = 0) and not attributes_strict
  then
  begin
    LogDatei.log (fdebuglevel, 'no filtering by attributes requested');
    exit;
  end;


  LogDatei.log (fdebuglevel, 'retaining child elements with the following attribute(s):');
  for k := 0 to numberOfAttributes - 1
  do
  begin
    if attributevalueExists[k]
    then
      LogDatei.log (fdebuglevel, '   "' + attributenames[k] + '" value="' + attributevalues[k] + '"')
    else
      LogDatei.log (fdebuglevel, '   "' + attributenames[k] + '"')
  end;


  basejindex := 0;

  i := 0;

  while i < length(actNodeSet)
  do
  Begin

     if actNodeSet[i] <> nil
     then
     begin

       n := actNodeSet[i].childnodes.count;

       for j:=0 to n-1
       do
       begin

         goon := true;

         if (FderivedNodeSet[basejindex + j] = nil)
         then
           goon := false;

         if goon
         then
         begin
           if attributes_strict
              //and
              //( actNodeSet[i].childnodes[j].AttributeNodes.Count <> length (attributenames) )
           then
           begin
             FderivedNodeSet[basejindex + j] := nil;
             LogDatei.log (fdebuglevel, 'node ' + inttostr (basejindex + j) + ' not accepted: Number of attributes does not match');
             goon := false;
           end;

           k := 0;
           while goon and (k < numberOfAttributes)
           do
           begin
             getNamespaceAndBasename (attributenames[k], uri, attributename);

             b1 := actNodeSet[i].childnodes.Item[j].HasAttributes (attributename, uri);
             if b1
             then
               b2 := actNodeSet[i].childnodes.Item[j].getattributes(attributename, uri) = attributevalues[k]
             else
               b2 := false;

             if not b1
             then
             begin
                FderivedNodeSet[basejindex + j] := nil;

                LogDatei.log (fdebuglevel, 'node ' + inttostr (basejindex + j) + ' not accepted: No attribute "' + attributenames[k] + '"');
                goon := false;
             end

             else
             begin
                if attributevalueExists[k] and not b2
                then
                begin
                  FderivedNodeSet[basejindex + j] := nil;

                  LogDatei.log (fdebuglevel, 'node ' + inttostr (basejindex + j)
                     + ' not accepted: Not the required value "' + attributevalues[k] + '" for attribute "'
                      + attributenames[k] + '"');
                  goon := false;
                end
             end;

             inc (k);

           end;
         end;

       end;

       basejindex := basejindex + n;

     end;

     inc (i);
  end;
end;
*)


function TuibXMLDocument.filterByAttributeName_existing (name: string) : Boolean;
var
  i, n, j, basejindex: Integer;
  b0, b1 : boolean;
  uri, attributename : String;

begin
  LogDatei.log ('retaining child elements with attribute "' + name + '"', LLinfo);
  result:=true;
  basejindex := 0;

  i := 0;

  while i < length(actNodeSet)
  do
  Begin

     if actNodeSet[i] <> nil
     then
     begin

       n := actNodeSet[i].childnodes.count;

       for j:=0 to n-1
       do
       begin
         b1 := actNodeSet[i].childnodes.Item[j].Attributes[i].NodeName = name;
         b0 := FderivedNodeSet[basejindex + j] <> nil;

         if  b1 and b0
         then
            FderivedNodeSet[basejindex + j] := actNodeSet[i].childnodes.Item[j]
         else
            FderivedNodeSet[basejindex + j] := nil;
       end;

       basejindex := basejindex + n;

     end;

     inc (i);
  end;

end;


function TuibXMLDocument.filterByAttribute_existing (name: string; value : string) : Boolean;
var
  i, n, j, basejindex: Integer;
  b0, b1, b2 : boolean;


begin
  LogDatei.log ('retaining child elements with attribute "' + name + '" value: "' + value + '"', fdebuglevel );
  result:=true;
  basejindex := 0;

  i := 0;

  while i < length(actNodeSet)
  do
  Begin

     if actNodeSet[i] <> nil
     then
     begin

       n := actNodeSet[i].childnodes.count;

       for j:=0 to n-1
       do
       begin
         actnode:= actNodeSet[i].ChildNodes.Item[j];
         b0 := FderivedNodeSet[basejindex + j] <> nil;
         b1 := hasAttribute(name);
         b2 := actnode.NodeName = value;

         if b0 and b1 and b2
         then
            FderivedNodeSet[basejindex + j] := actnode
         else
            FderivedNodeSet[basejindex + j] := nil;
       end;

       basejindex := basejindex + n;

     end;

     inc (i);
  end;

end;


function TuibXMLDocument.filterByChildElement (filtering: boolean; elementname: string) : Boolean;
var
  i, n, j, basejindex: Integer;

begin
  result := true;

  if not filtering
  then
  begin
    LogDatei.log ('no filtering by elementname requested', LLwarning);
    filterByChildElement:=false;
    exit;
  end;
  LogDatei.log ('retaining child elements with name = "' + elementname + '"', LLInfo);
  basejindex := 0;
  i := 0;
  while i < length(actNodeSet)
  do
  Begin

     if actNodeSet[i] <> nil
     then
     begin

       n := actNodeSet[i].childnodes.count;

       for j:=0 to n-1
       do
       begin
         actnode:= actNodeSet[i].childnodes.Item[j];
         if (actnode.NodeName = elementname) then
           begin
             FderivedNodeSet[basejindex + j] := actnode;
           end
         else
            FderivedNodeSet[basejindex + j] := nil;
       end;
       basejindex := basejindex + n;
     end;

     inc (i);
  end;

end;



function TuibXMLDocument.setActNodeIfText(textvalue: string): boolean;
var
  i: integer;
  comparetext: string;

begin
  Result := False;
  LogDatei.log('set aktnode as child element with text  "' + textvalue +
    '"', oslog.LLinfo);
  i := 0;
  while (i < actNode.ChildNodes.Count - 1) and (Result = False) do
  begin
    if actNode.ChildNodes.Item[i] <> nil then
    begin
      if actNode.ChildNodes.Item[i].NodeType = ELEMENT_NODE then
      begin
        LogDatei.log('aktnode as child element with text  "' +
          actNode.ChildNodes.Item[i].TextContent + '"', oslog.LLinfo);
        comparetext := actNode.ChildNodes.Item[i].TextContent;
        if AnsiCompareStr(textvalue, comparetext) = 0 then
        begin
          actNode := actnode.ChildNodes.Item[i];
          Result := True; // aktnode is
        end
        else
          Result := False;
      end
      else
        comparetext := '';
    end;
    Inc(i);
  end;
end;

function TuibXMLDocument.filterByText (filtering: boolean; textvalue: string) : Boolean;
var
  i, n, j, basejindex: Integer;
  comparetext : String;

begin
  filterByText := true;

  if not filtering
  then
  begin
    LogDatei.log ('no filtering by textvalue requested', LLinfo);
    filterByText:=false;
    exit;
  end;

  LogDatei.log ('retaining child elements with text  "' + textvalue + '"', LLinfo);

  // TODO: klären, warum hier der returnwert nicht gesetzt wird, wird false nicht gebraucht?
  basejindex := 0;
  i := 0;
  while i < length(actNodeSet)
  do
  begin

     if actNodeSet[i] <> nil
     then
     begin

       n := actNodeSet[i].childnodes.count;

       for j:=0 to n-1
       do
       begin
         actnode:= actNodeSet[i].childnodes.Item[j];
         if actnode.NodeType = TEXT_NODE
         then
           comparetext := actnode.TextContent
         else
           comparetext := '';

         if AnsiCompareStr ( textvalue, comparetext ) = 0
         then
         begin
            FderivedNodeSet[basejindex + j] := actnode
         end

         else
            FderivedNodeSet[basejindex + j] := nil;

       end;

       basejindex := basejindex + n;

     end;

     inc (i);
  end;
  cleanUpActNodeSet();
end;



procedure TuibXMLDocument.logNodeSets;
var
  i, basejindex, j: integer;
  count_not_nil: integer;
  count_not_nil_2: integer;

begin
  cleanUpActNodeSet();
  cleanUpDerivedNodeSet();
  LogDatei.log('', LLinfo);
  LogDatei.log('actNodeSet: ', LLinfo);
  count_not_nil := 0;
  for i := 0 to length(actNodeSet) - 1 do
  begin
    if actnodeset[i] = nil then
      LogDatei.log(extraindent + 'node ' + IntToStr(i) + ' null ', LLinfo)
    else
    begin
      LogDatei.log(extraindent + 'node ' + IntToStr(i) +
        ' elementname: "' + actNodeSet[i].NodeName + '"', LLinfo);
      Inc(count_not_nil);
    end;
  end;

  LogDatei.log('Non-null element(s) in act node set: ' + IntToStr(count_not_nil), LLinfo);

  count_not_nil_2 := 0;
  LogDatei.log('derivedNodeSet: ', LLinfo);

  for i := 0 to length(derivedNodeSet) - 1 do
  begin
    if derivednodeset[i] = nil then
      LogDatei.log(extraindent + 'node ' + IntToStr(i) + ' null ' , LLinfo)
    else
    begin
      LogDatei.log(extraindent + 'node ' + IntToStr(i) +
        ' elementname: "' + derivedNodeSet[i].NodeName + '"', LLinfo);
      Inc(count_not_nil_2);
    end;
  end;

  LogDatei.log('Non-null element(s) remaining in children node set: ' +
    IntToStr(count_not_nil_2), LLinfo);
  LogDatei.log('', LLinfo);
  LogDatei.log('    -------', LLinfo);
end;

procedure TuibXMLDocument.logActNodeSet;
var
  i, basejindex, j: integer;
  count_not_nil: integer;
  count_not_nil_2: integer;

begin
  cleanUpActNodeSet();
  LogDatei.log( '', LLinfo);
  LogDatei.log( 'actNodeSet: ', LLinfo);
  count_not_nil := 0;
  for i := 0 to length(actNodeSet) - 1 do
  begin
    if actnodeset[i] = nil then
      LogDatei.log( extraindent + 'node ' + IntToStr(i) + ' null ', LLinfo)
    else
    begin
      LogDatei.log( extraindent + 'node ' + IntToStr(i) +
        ' elementname: "' + actNodeSet[i].NodeName + '"', LLinfo);
      Inc(count_not_nil);
    end;
  end;

  LogDatei.log('Non-null element(s) in act node set: ' + IntToStr(count_not_nil), LLinfo);

end;

procedure TuibXMLDocument.delNode(nodePath: string );
var
  removeNode: TDOMNode;
begin
  LogDatei.log('begin to del Node: ' + nodePath, LLinfo);
  openNode(nodePath, false);  // not strict
  removeNode := actNode;
  actNode := actNode.ParentNode;
  try
    removenode.Free;
  except
    LogDatei.log('Error in delNode: ' + nodePath + '. Node was not removed', LLerror);
  end;
end;

procedure TuibXMLDocument.delNode;
// aktnode (and all childs) will be deleted, afterwards aktnode will be parent
// before del node check node with nodeExists
var
  removeNode: TDOMNode;
begin
  LogDatei.log('begin to del Node: ' + actNode.NodeName, LLinfo);
  removeNode := actNode;
  actNode := actNode.ParentNode;
  try
    removenode.Free;
  except
    LogDatei.log('Error in delNode. Node was not removed', LLerror);
  end;
end;

procedure TuibXMLDocument.makeAttributesSL (var attributeStringList: TStringList; attributePath : String);
var splitPos, i: integer;
  attribute : String;
  leavingpath: String;
begin
  logdatei.log('attribute path element : ' + attributePath, LLinfo);
  attributeStringList:= TStringList.Create;
  i:=1;
  while (length(attributePath)>0) do
  begin
    if Length(attributePath)-Length(StringReplace(attributePath, '"','', [rfReplaceAll, rfIgnoreCase])) = 2  then
    begin // only one remaining attribute
      AttributeStringList.Add(Trim(attributePath));
      attributePath:='';
    end
    else
    begin
      // find with '" ' the end of an attribute
      attribute:= Trim(copy(attributePath, 1, pos('" ', attributePath) ));
      //logdatei.log('attribute ' + IntToStr(i) + ': ' + attribute, LLinfo);
      leavingPath := copy(attributePath, pos('" ', attributePath) + 1, length(attributePath));
      //logdatei.log( 'leavingPath ' + leavingPath, LLinfo);
      AttributeStringList.Add(attribute);
      attributePath:=leavingPath;
    end;
    inc(i);
  end;

end;

function TuibXMLDocument.openNode(nodePath: string; attributes_strict: boolean): boolean;
// if attributes_strict = true all attributes have to exist
//                      = false, other attributes may exist, no matter
// set actNode
var
  nodesInPath: array[0..50] of TDOMNode;
  attributesSL, pathes: TStringList;
  i,j: integer;
  found: boolean;
  leavingPath, thisnodeName, attribute : string;
  attributeList: TList;
begin
  Result := True;
  try
    // the root node
    nodesInPath[0] := XML.DocumentElement;
    stringsplit(nodepath, PATHSEPARATOR, pathes);
    // walk the path
    // The Path looks like this:
    // nodedescription PATHSEPARATOR nodedescripton (and so on)
    // nodedescription looks like this
    // nodename attributeName="attributeValue"  or
    // nodename                          (node without attributes)
    // (!) attributeValue may contain a PATHSEPARATOR string
    // PATHSEPARATOR = ' // '
    i := 1;

    found := True;
    leavingPath := nodePath;
    logdatei.log('begin to open nodepath  : ' + nodepath, LLinfo);
    logdatei.log('-- pathes.Count: ' + IntToStr(pathes.Count), oslog.LevelInfo);
    while i < pathes.Count + 1 do
    begin
      logdatei.log('path element ' + IntToStr(i) + ' : ' + pathes[i - 1], LLinfo);
      thisnodeName := Trim(copy(pathes[i - 1], 1, pos(' ', pathes[i - 1]) - 1));
      logdatei.log('thisnodename ' + thisnodeName, LLinfo);
      leavingPath := copy(pathes[i - 1], pos(' ', pathes[i - 1]) + 1, length(pathes[i - 1]));
      logdatei.log( 'leavingPath ' + leavingPath, LLinfo);
      if (pos('=', pathes[i - 1]) > 0) then // only in this case attributes
      begin
        // split on blank, list of attributes
        makeAttributesSL(attributesSL, leavingPath);
        for j:=0 to attributesSL.Count-1 do
           logdatei.log( 'Attribute ' +attributesSL[j], LLinfo );
        logdatei.log( 'Anzahl Attribute ' + IntToStr(attributesSL.Count), LLinfo );
        j:=0;
        attributeList := TList.Create;
        while j < attributesSL.Count do
        begin
          // List of [attributename, attributevalue]
          attributeList.Add;
          attributeList.Items[j].key := Trim(
                   copy(attributesSL[j], 1, pos('=', attributesSL[j]) - 1));
          attributeList.Items[attributeList.Count-1].value :=  Trim(
                   copy(attributesSL[j], pos('=', attributesSL[j]) + 1, length(attributesSL[j])));
          if AnsiStartsStr('"', attributeList.Items[j].value) then
            attributeList.Items[j].value := copy(attributeList.Items[j].value,
                  2, length(attributeList.Items[j].value));
          if AnsiEndsStr('"', attributeList.Items[j].value) then
            attributeList.Items[j].value := Trim(copy(attributeList.Items[j].value,
                  1, length(attributeList.Items[j].value)-1));
          // ????
          (*
          leavingPath := copy(leavingPath, pos(' ', leavingPath) + 1, length(leavingPath));
          if pos('"' + PATHSEPARATOR, leavingPath) > 0 then
            attributeList.Items[attributeList.Count-1].value := copy(leavingPath, 1, pos('"' + PATHSEPARATOR, leavingPath))
          else
            attributeList.Items[attributeList.Count-1].value := leavingPath;
          *)
          inc(j);
          end;
      end
      else
        thisnodeName := Trim(pathes[i - 1]);

      logdatei.log('node ' + IntToStr(i) + ': nodename ' +
        thisnodename
        //+ ' attributeName: ' + attributeName +
        //' attributeValue: ' + attributeValue
        ,LLinfo
        );
      j:=0;
      (*
      while (j< attributeList.Count) do
      begin
        logdatei.log( attributeList.Items[j].key + ' : ' + attributeList.Items[j].value, LLinfo );
        inc(j);
      end;
      *)
      if attributes_strict then
      begin
        if not getNodeStrict(nodesInPath[i], nodesInPath[i - 1], thisnodeName, attributeList) then
          begin
            found := False;
            LogDatei.log( 'opennode: node with attributes_strict not found ' + IntToStr(i) + ': nodename: ' +
              thisnodeName + ', check nodename and attributes', LLwarning
              //+ ' attributeName: ' + attributeName
              //+ ' attributeValue: ' + attributeValue
              );
          end
          else
          begin
            found := True;
            if actnode<>nil then
               logdatei.log('Found node ' + IntToStr(i) + ': nodename: ' +
                 actNode.NodeName, LLinfo)
              // + ' attributeName: ' + attributeName
              //+ ' attributeValue: ' + attributeValue

            else
              logdatei.log('Found more then one node ' + IntToStr(i) + ': ' + IntToStr(length(actnodeset)) + ' nodes'
                 , LLinfo);
          end
       end
       else  // not strict!
         if not getNode(nodesInPath[i], nodesInPath[i - 1], thisnodeName, attributeList.Items[0].key, attributeList.Items[0].value) then  // TODO
           begin
            found := False;
            LogDatei.log( 'opennode: node not found ' + IntToStr(i) + ': nodename: ' +
              thisnodeName , LLwarning
              );
          end
          else
          begin
            found := True;
            logdatei.log('Found node ' + IntToStr(i) + ': nodename: ' +
               actNode.NodeName, LLinfo)
          end;
      Inc(i);
    end;

    // ??? hier noch unklar
    if found and (actnode<>nil) then  // ????
    begin
      //actNode := nodesInPath[i - 1];
      LogDatei.log('actNode know node ' + IntToStr(i-1) + ': nodename: ' +
        actNode.NodeName, LLinfo
        // TODO if multiple nodes with same textcontent, continous string of text content -
        );
    end
    else
    begin
      // was soll actNode sein?
      actNode:= nil;
      LogDatei.log( 'actNode=nil; opennode: node not found, maybe ' + IntToStr(i-1) + ': nodename: ' +
        thisnodename , LLwarning
        );
    end;
    Result:=found;
    attributeList.Free;
  except
    on e: Exception do
    begin
      Result := False;
      LogDatei.log('Could not open ' + nodepath + '; exception: ' + e.Message, LLerror);
    end;
  end;
end;


function TuibXMLDocument.getNodeStrict(var newNode: TDOMNode; myparentNode: TDOMNode;
  mynodeName: string; attributeList : TList): boolean;
// nodename and all attributes have to fit
// if not found: actnode = myparentnode - result false
// if found and unique : actnode = node found - result true
// if found and not unique : actnode = nil, check actnodeset - result true
var
  j, i, al: integer;
  attributename, attributevalue : string;
  boolarray : array [0..9] of boolean;
  namefound, attributesfound: boolean;
begin
  Result := False;
  logdatei.log('begin to get node  nodename: ' + mynodename + ' with attributes: ', LLinfo );
  for al:= 0 to attributeList.Count-1 do
    logdatei.log(attributelist.Items[al].key + ' : ' + attributelist.Items[al].value, LLinfo );
  // find nodes with name
  namefound:= false;
  try
    if (myparentNode <> nil) then
      begin
        j := 0;
        i:=0;
        // find nodes with nodename
        while (myparentNode.hasChildNodes) and (j < myparentNode.ChildNodes.Count)
               do
        begin
          if (myparentNode.ChildNodes.Item[j].NodeName = mynodeName) then
            begin
              namefound:= true;
              //newNode := myparentNode.ChildNodes.Item[j];
              setLengthActNodeset(i+1);
              actNode := myparentNode.ChildNodes.Item[j];
              actNodeSet[i]:=actNode;
              inc(i);
            end;
          inc(j);
        end;
      end;
  finally
  end;

  logdatei.log('node(s) found with name ' + mynodeName + ': ' + IntToStr(length(actNodeSet)), oslog.LLinfo);

  if namefound then
  begin
    //result:= true;
    if attributeList.Count<10 then  // only 10 attributes
    begin
      for j:=0 to length(actNodeSet)-1 do  // check attributes for node j
      begin
        for al:=0 to 9 do
          boolarray[al]:=false;
        //attributesfound:=false;
        logdatei.log('find attributes for node ' +IntToStr(j) + ' ' + actnodeset[j].NodeName, LLinfo );
        if actnodeset[j].HasAttributes then
        begin
          al:=0;
          while (al<attributeList.Count) do
          begin
            attributename:=attributelist.Items[al].key;
            attributevalue:=attributelist.Items[al].value;
            // check attributes
            for i := 0 to actnodeset[j].Attributes.Length - 1 do
            begin
              if (actnodeset[j].Attributes[i].NodeName = attributeName)
              and (actnodeset[j].Attributes[i].TextContent = attributeValue) then
              begin
                logdatei.log('attribut found ' + attributename + ' ' + attributevalue, LLinfo );
                boolarray[al]:=true;
              end
            end;
            Inc(al);
          end;

        end; // has attributes
        logActNodeSet;
        logdatei.log('all attributes have to fit, nodename ' + actnodeset[j].NodeName , oslog.LLinfo);
        if (attributeList.Count=actnodeset[j].Attributes.Length) then
          begin
            attributesfound:=true;
            // to max(attributeList.Count-1, myparentNode.ChildNodes.Item[j].Attributes.Length - 1)
            for al:=0 to attributeList.Count-1 do
            begin
              logdatei.log('boolarray ' + IntToStr(al) , oslog.LLinfo);
              if boolarray[al] then logdatei.log(attributelist.Items[al].key + ': true' , oslog.LLinfo)
              else logdatei.log(attributelist.Items[al].key + ': false' , oslog.LLinfo) ;
              attributesfound:= attributesfound AND boolarray[al];
            end;
            result:=attributesfound;
            if not attributesfound then
            begin
              // remove node
              actnodeset[j]:=nil;
            end
          end
          else // strict: identical number of attributes!
            actnodeset[j]:=nil;

      end;  // end traverse actNodeSet
      cleanupactnodeset();

      if result then
      begin
        if  length(actnodeset) = 1 then
        begin
          actnode:=actnodeset[0];
          newNode := actnodeset[0];  // ??
          logdatei.log('result true, actNode is ' + actNode.NodeName, oslog.LLinfo) ;
        end
        else
        begin
          actnode:=nil;
          logdatei.log('result true, actnode is nil, lenght of actNodeSet is ' + IntToStr(length(actnodeset)), oslog.LLinfo) ;
        end
     end
     else
       actnode:= myparentnode;
    end
   else
        logdatei.log('handling of more then 10 attributes not implemented', oslog.LLerror);
  end
  else
    begin
      result:=false;
      actNode:=myparentnode;
    end
end;


// for only one attribute
function TuibXMLDocument.getNode(var newNode: TDOMNode; myparentNode: TDOMNode;
  mynodeName: string; attributename: string; attributevalue : string): boolean;
//
var
  j, i: integer;
begin
  getNode := False;
  logdatei.log('begin to get node  nodename: ' + mynodename + ' with attributes: ', LLinfo );

  try
    if (myparentNode <> nil) then
    begin
      j := 0;
      while (myparentNode.hasChildNodes) and (j < myparentNode.ChildNodes.Count) and
        (getNode = False) do
      begin
        if (myparentNode.ChildNodes.Item[j].NodeName = mynodeName) then
          if (myparentNode.ChildNodes.Item[j].HasAttributes) then
          begin
            for i := 0 to myparentNode.ChildNodes.Item[j].Attributes.Length - 1 do
            begin
              if (myparentNode.ChildNodes.Item[j].Attributes[i].NodeName =
                attributeName) and (myparentNode.ChildNodes.Item[
                j].Attributes[i].TextContent = attributeValue) then
              begin
                newNode := myparentNode.ChildNodes.Item[j];
                actNode := myparentNode.ChildNodes.Item[j];
                getNode := True;
              end;
            end;
          end
          else   // also found if there are no attributes but name found
          begin
            newNode := myparentNode.ChildNodes.Item[j];
            actNode := myparentNode.ChildNodes.Item[j];
            getNode := True;
          end;

        Inc(j);
      end;
    end
    else
      logdatei.log('parentnode not valid', oslog.LLerror);
  finally

  end;
end;

function TuibXMLDocument.makeNode(mynodeName,
  attributeName, attributeValue: string): boolean;
  // create new node, append to actNode, set newnode as actNode
var
  newnode: TDOMNode;
begin
  LogDatei.log('begin to make node with nodename: ' +
    nodename + ' attributeName: ' + attributeName +
    ' attributeValue: ' + attributeValue , LLinfo
    );
  makeNode := False;
  try
    if actNode <> nil then
    begin
      newnode := XML.CreateElement(mynodename);
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
    LogDatei.log('error to make node with nodename: ' +
      nodename + ' attributeName: ' + attributeName +
      ' attributeValue: ' + attributeValue , LLerror
      );
  end;
end;

procedure TuibXMLDocument.setNodeText(Text: string);
begin
  begin
    LogDatei.log('begin to set text to actNode: ' + Text, LLinfo);
    if actNode <> nil then
      actNode.TextContent := Text
    else
      LogDatei.log('actNode is nil, text not set: ' + Text, LLwarning);
  end;
end;


function TuibXMLDocument.nodeExists(nodePath: string): boolean;
// tells if a node exists without changing anything
// TODO : selection if text
// same as openNode??? redundant.
var
  nodesInPath: array[0..50] of TDOMNode;
  pathes: TStringList;
  i: integer;
  endOfPath, found: boolean;
  leavingPath, thisnodeName : string;
  attributeName, attributeValue: string;
begin
  nodeExists := False;
  try
    begin
      // the root node
      nodesInPath[0] := XML.DocumentElement;
      stringsplit(nodepath, PATHSEPARATOR, pathes);
      // walk the path
      // The Path looks like this:
      // nodedescription PATHSEPARATOR nodedescripton (and so on)
      // nodedescription looks like this
      // nodename attributeName="attributeValue"  or
      // nodename (node without attributes)
      // (!) attributeValue may contain a PATHSEPARATOR string
      // PATHSEPARATOR = ' // '
      i := 1;
      endOfPath := False;
      found := True;
      leavingPath := nodePath;
      LogDatei.log('begin to open nodepath, exists?  ' + nodepath, LLinfo);
      while (i < pathes.Count + 1) and found do
      begin
        attributevalue := '';
        attributename := '';
        LogDatei.log('path element ' + IntToStr(i) + ' : ' + pathes[i - 1], LLinfo);
        if (pos('=', pathes[i - 1]) > 0) then // only in this case attributes
        begin
          thisnodeName := copy(pathes[i - 1], 1, pos(' ', pathes[i - 1]) - 1);
          leavingPath := copy(pathes[i - 1], pos(' ', pathes[i - 1]) + 1, length(pathes[i - 1]));
          attributeName := copy(leavingPath, 1, pos('=', leavingPath) - 1);
          leavingPath := copy(leavingPath, pos('=', leavingPath) + 1, length(leavingPath));
          if pos('"' + PATHSEPARATOR, leavingPath) > 0 then
            attributeValue := copy(leavingPath, 1, pos('"' + PATHSEPARATOR, leavingPath))
          else
            attributeValue := leavingPath;
          if AnsiStartsStr('"', attributeValue) then
            attributeValue := copy(attributeValue, 2, length(attributeValue));
          if AnsiEndsStr('"', attributeValue) then
            attributeValue := copy(attributeValue, 1, length(attributeValue) - 1);
        end
        else
          thisnodeName := pathes[i - 1];
        thisnodeName := Trim(thisnodeName);
        attributeValue := Trim(attributeValue);
        attributename := Trim(attributename);
        LogDatei.log('node ' + IntToStr(i) + ': nodename ' +
          thisnodename + ' attributeName: ' + attributeName
          + ' attributeValue: ' + attributeValue, LLinfo
          );

        if not getNode(nodesInPath[i], nodesInPath[i - 1], thisnodeName,
          attributeName, attributeValue) then
        begin
          found := False;
          LogDatei.log( 'not found node ' + IntToStr(i) + ': nodename: ' +
            thisnodename + ' attributeName: ' + attributeName
            + ' attributeValue: ' + attributeValue, LLinfo
            );
        end
        else
        begin
          LogDatei.log( 'Found node ' + IntToStr(i) + ': nodename: ' +
            thisnodename + ' attributeName: ' + attributeName
            + ' attributeValue: ' + attributeValue, LLinfo
            );
          found := True;
        end;

        Inc(i);
      end;

      if found then
        nodeExists := True
      else
        nodeExists := False;
    end;
  except
    LogDatei.log('node not found' + ': nodename: ' + thisnodename
      + ' attributeName: ' + attributeName +
      ' attributeValue: ' + attributeValue, LLwarning);
    nodeExists := False;
  end;
end;



//*************  XML Attribute-Handling ***********************************
function TuibXMLDocument.countAttributes ( myxmlnode: TDOMNode ) : integer;
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
  if (actNode<>nil) then
  begin
    if (actNode.HasAttributes) then
    begin
      for i := 0 to actNode.Attributes.Length - 1 do
      begin
        if (actNode.Attributes[i].NodeName = attributename) then
        begin
          Result := True;
          LogDatei.log('actNode has attribute with name: '
            + attributeName, LLinfo);
        end;
      end;
    end;
    if Result = False then
      LogDatei.log('actNode has no attribute with name: '
        + attributeName,  LLwarning);
  end
  else
    LogDatei.log('hasAttribute failed, actNode is nil: ' +
        attributeName, LLerror);
end;

procedure TuibXMLDocument.setAttribute(attributeName, attributeValue: string);
// set if exists, otherwise create
// attributeName may include a namespace prefix --- no more
var
  i: integer;
  found: boolean;
begin
  LogDatei.log('begin setAttribute name: ' +
    attributeName + ', value: ' + attributeValue, oslog.LLinfo);
  found:= false;
  if (actNode<>nil) then
  begin
    if actNode.HasAttributes then
    begin
      for i := 0 to actNode.Attributes.Length - 1 do
        if actNode.Attributes[i].NodeName = attributeName then
          begin
            actNode.Attributes[i].TextContent := attributevalue;
            LogDatei.log('setAttribute with name: ' +
              attributeName + ' value: ' + attributeValue, LLinfo);
            found:= true;
          end
    end;
    if not found then
      begin
        TDOMElement(actNode).SetAttribute(attributeName, attributeValue);
        LogDatei.log('setAttribute, create attribute with name: ' +
          attributeName + ' value: ' + attributeValue, LLinfo);
      end
  end
  else
    LogDatei.log('setAttribute failed, actNode ist nil: ' +
        attributeName + ' value: ' + attributeValue, LLerror);
end;

procedure TuibXMLDocument.addAttribute(attributeName, attributeValue: string);
// only add attribute if attribute does not exist
// attributeName may include a namespace prefix --- no more
begin
  LogDatei.log('begin to add attribute: name: ' +
    attributeName + ' value: ' + attributeValue, LLinfo);
  if (actNode<>nil) then
    if NOT(TDOMElement(actNode).hasAttribute(attributeName)) then
      TDOMElement(actNode).SetAttribute(attributeName, attributeValue)
    else
  else
    LogDatei.log('addAttribute failed, actNode ist nil: name' +
        attributeName + ' value: ' + attributeValue, LLerror);
end;

procedure TuibXMLDocument.delAttribute(attributeName: string);
begin
  LogDatei.log('begin to del attribute: name: ' + attributeName, LLinfo);
  if (actNode<>nil) then
    if (actNode.HasAttributes) then
      TDOMElement(actNode).RemoveAttribute(attributeName)
    else
      LogDatei.log('delAttribute failed, name: ' +
        attributeName + 'does not exist', LLerror)
  else
    LogDatei.log('delAttribute failed, actNode ist nil: name: ' +
        attributeName,  LLerror);
end;


function TuibXMLDocument.getAttributeValue(attributeName: string): string;
  // var
  // get the attribute value at the actual node
  // ATTENTION: it returns an empty string:
  // - if there is no attribute with this name
  // - if the value of this attribute is an empty string
begin
  LogDatei.log('begin to get value of attribute: name: '
    + attributeName, LLinfo);
  getAttributeValue := '';
  if (actNode <> nil) and actNode.HasAttributes then
    getAttributeValue := TDOMElement(actNode).GetAttribute(attributeName)
  else
    LogDatei.log('getAttribute failed, name: ' +
        attributeName + 'does not exist or actNode is nil',  LLerror)

end;



end.
