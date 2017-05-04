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
  //osxmltdom,
  //XMLIntf,
  //XMLDoc,
  Dialogs, StrUtils,
  ExtCtrls,
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
    aktNode: TDOMNode;

    fActNodeSet: TNodeSet;
    fDerivedNodeSet: TNodeSet;
    fDebugLevel: integer;

    //function getCountNotNil : Integer;
    function getCountDerivedNotNil: integer;

    function getNode(var newNode: TDOMNode; myparentNode: TDOMNode;
      mynodeName, attributeName, attributeValue: string): boolean;
    {
    function makeNodeAtPos(var newNode:TDOMNode; myparentNode:TDOMNode;
                         mynodeName, attributeName, attributeValue : string;
                         Position : integer):boolean;
    }
    {
    procedure DOMFromStream(AStream: TStream);
    }
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

    {procedure getNamespaceAndBasename(prefixedName :String;
                                  var mynamespaceUri : String;
                                  var baseName : String);

    }

    procedure setlengthActNodeSet(newlength: integer);
    procedure setlengthDerivedNodeSet(newlength: integer);
    procedure logNodeSets;

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
    function filterByAttributeName_existing (name: string) : Boolean;
    function filterByAttribute_existing (name: string; value : string) : Boolean;

    function filterByChildElement (filtering: boolean; elementname: string) : Boolean;

    //function filterByText (filtering: boolean; textvalue: string) : Boolean;
    }
    function setAktnodeIfText(textvalue: string): boolean;

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


    function openNode(nodePath: string): boolean;
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
//*********************************************************************
{procedure TuibXMLDocument.DOMFromStream(AStream: TStream);
var
  Parser: TDOMParser;
  Src: TXMLInputSource;
  TheDoc: TXMLDocument;
begin
  try
    // ein neues Parserobjekt erstellen
    Parser := TDOMParser.Create;
    // die XML-Datei muss über ein TXMLInputSource-Objekt geladen werden
    Src := TXMLInputSource.Create(AStream);
    // wir wollen eine Überprüfung der Dokumentstruktur
    Parser.Options.Validate := True;
    // Festlegen einer Methode, die bei Fehlern aufgerufen wird
    Parser.OnError := @ErrorHandler;
    // nun beginnen wir die Überprüfung
    Parser.Parse(Src, TheDoc);
    // ... und räumen hinterher wieder auf
  finally
    Src.Free;
    Parser.Free;
  end;
end;
}
procedure TuibXMLDocument.ErrorHandler(E: EXMLReadError);
begin
  if E.Severity = esError then
    // wir sind nur in Fehlern bezüglich der Verletzung der DTD interessiert
    writelog(LLerror, E.Message);
  // an dieser Stelle können wir auch alles andere machen, was bei einem Fehler getan werden sollte
end;


//*************  TuibXMLDocument ***********************************
destructor TuibXMLDocument.Destroy;
begin
  inherited Destroy;
end;


{
procedure TuibXMLDocument.getNamespaceAndBasename(prefixedName :String;
                                  var mynamespaceUri : String;
                                  var baseName : String);
// prefixedName may include a namespace prefix
var
 // if we have a namespace like xmlns:NC="http://home.netscape.com/NC-rdf#"
namespacePrefix : String; // is NC
//mynamespaceUri : String;   // is http://home.netscape.com/NC-rdf#
namespace_separator : integer; // pos of prefix separator ':'
begin
  namespacePrefix := '';
  mynamespaceUri := '';
  //writelog(fdebuglevel + 1,'begin to get namespace from : '+prefixedName);
  namespace_separator := pos(':',prefixedName);
  if namespace_separator > 0 then
  begin
   namespacePrefix := copy(prefixedName,1,namespace_separator-1);
   //mynamespaceUri := DocumentElement.FindNamespaceURI(namespacePrefix);
   mynamespaceUri := copy(prefixedName,pos('"',prefixedName),length(prefixedName));
   baseName := copy(prefixedName,namespace_separator+1,length(prefixedName));
  end
  else baseName := prefixedName;
end;
}

function TuibXMLDocument.getCountNotNil: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to length(actNodeSet) - 1 do
  begin
    if actnodeset[i] <> nil then
      Inc(Result);
  end;
end;

function TuibXMLDocument.getCountDerivedNotNil: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to length(DerivedNodeSet) - 1 do
  begin
    if derivednodeset[i] <> nil then
      Inc(Result);
  end;
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
    writelog(5, 'try to open File: ' + filename);
    mystream := TFilestream.Create(fileName, fmOpenRead);
    mystream.Position := 0;
    XML := nil;
    writelog(5, 'try to load File: ' + filename);
    ReadXMLFile(XML, mystream);
    writelog(4, 'File: ' + filename + ' read');
    Result := True;
  except
    on e: Exception do
      writelog(0, 'Error in readXmlFile : ' + e.Message);
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

  writelog(fdebuglevel + 1, 'begin to save File: ' + filename);
  Result := False;
  try
    WriteXML(XML, filename);
    (*
    writelog(5,'try to open File: '+filename);
    mystream := TFilestream.Create(fileName,fmOpenWrite);
    mystream.Position := 0;
    XML:=Nil;
    writelog(5,'try to save File: '+filename);
    WriteXML();
    ReadXMLFile(XML, mystream);
    writelog(4,'File: '+filename+' read');
    *)
    Result := True;
  except
    on e: Exception do
      writelog(0, 'Error in writeXmlFile : ' + e.Message);
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


procedure TuibXMLDocument.setlengthActNodeSet(newlength: integer);
begin
  setlength(factnodeset, newlength);
end;

procedure TuibXMLDocument.setlengthDerivedNodeSet(newlength: integer);
begin
  setlength(fderivednodeset, newlength);
end;


//*************  XML Node-Handling ***********************************
function TuibXMLDocument.createXmlDocFromStringlist(docstrlist: TStringList): boolean;
var
  mystream: TStringStream;
begin
  Result := False;
  writelog(fdebuglevel + 1, 'begin to create XMLDoc ');
  mystream := TStringStream.Create(docstrlist.Text);
  mystream.Position := 0;
  XML := nil;
  try
    //DOMFromStream(mystream); - unklar ob das sinnvoll ist
    // Beispiel hatte am Anfang einen DTD
    ReadXMLFile(XML, mystream);
    writelog(LLinfo, 'XMLDoc created from Stringlist');
    Result := True;
  finally
    mystream.Free;
  end;
  writelog(fdebuglevel, 'XMLDoc created');
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
  writelog(fdebuglevel, 'Going to the next level ...');
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

{
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
  (*
  numberOfAttributes := length(attributenames);

  if (numberOfAttributes = 0) and not attributes_strict
  then
  begin
    writelog (fdebuglevel, 'no filtering by attributes requested');
    exit;
  end;


  writelog (fdebuglevel, 'retaining child elements with the following attribute(s):');
  for k := 0 to numberOfAttributes - 1
  do
  begin
    if attributevalueExists[k]
    then
      writelog (fdebuglevel, '   "' + attributenames[k] + '" value="' + attributevalues[k] + '"')
    else
      writelog (fdebuglevel, '   "' + attributenames[k] + '"')
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
             writelog (fdebuglevel, 'node ' + inttostr (basejindex + j) + ' not accepted: Number of attributes does not match');
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

                writelog (fdebuglevel, 'node ' + inttostr (basejindex + j) + ' not accepted: No attribute "' + attributenames[k] + '"');
                goon := false;
             end

             else
             begin
                if attributevalueExists[k] and not b2
                then
                begin
                  FderivedNodeSet[basejindex + j] := nil;

                  writelog (fdebuglevel, 'node ' + inttostr (basejindex + j)
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
  *)

end;


function TuibXMLDocument.filterByAttributeName_existing (name: string) : Boolean;
var
  i, n, j, basejindex: Integer;
  b0, b1 : boolean;
  uri, attributename : String;

begin
  writelog (fdebuglevel , 'retaining child elements with attribute "' + name + '"');
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
  writelog (fdebuglevel, 'retaining child elements with attribute "' + name + '" value: "' + value + '"');
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
         aktnode:= actNodeSet[i].ChildNodes.Item[j];
         b0 := FderivedNodeSet[basejindex + j] <> nil;
         b1 := hasAttribute(name);
         b2 := aktnode.NodeName = value;

         if b0 and b1 and b2
         then
            FderivedNodeSet[basejindex + j] := aktnode
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
    writelog (fdebuglevel, 'no filtering by elementname requested');
    exit;
  end;


  writelog (fdebuglevel, 'retaining child elements with name = "' + elementname + '"');

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
         aktnode:= actNodeSet[i].childnodes.Item[j];
         if (aktnode.NodeName = elementname) then
           begin
             //FderivedNodeSet[basejindex + j] := aktnode;
           end
         else
            FderivedNodeSet[basejindex + j] := nil;
       end;
       basejindex := basejindex + n;
     end;

     inc (i);
  end;

end;
}
function TuibXMLDocument.setAktnodeIfText(textvalue: string): boolean;
var
  i: integer;
  comparetext: string;

begin
  Result := False;
  LogDatei.log('set aktnode as child element with text  "' + textvalue +
    '"', oslog.LLinfo);
  i := 0;
  while (i < aktnode.ChildNodes.Count - 1) and (Result = False) do
  begin
    if aktnode.ChildNodes.Item[i] <> nil then
    begin
      if aktnode.ChildNodes.Item[i].NodeType = ELEMENT_NODE then
      begin
        LogDatei.log('aktnode as child element with text  "' +
          aktnode.ChildNodes.Item[i].TextContent + '"', oslog.LLinfo);
        comparetext := aktnode.ChildNodes.Item[i].TextContent;
        if AnsiCompareStr(textvalue, comparetext) = 0 then
        begin
          aktnode := aktnode.ChildNodes.Item[i];
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

{function TuibXMLDocument.filterByText (filtering: boolean; textvalue: string) : Boolean;
var
  i, n, j, basejindex: Integer;
  comparetext : String;

begin
  result := true;

  if not filtering
  then
  begin
    writelog (fdebuglevel, 'no filtering by textvalue requested');
    exit;
  end;

  writelog (fdebuglevel, 'retaining child elements with text  "' + textvalue + '"');

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
         aktnode:= actNodeSet[i].childnodes.Item[j];
         if aktnode.NodeType = TEXT_NODE
         then
           comparetext := aktnode.TextContent
         else
           comparetext := '';

         if AnsiCompareStr ( textvalue, comparetext ) = 0
         then
         begin
            FderivedNodeSet[basejindex + j] := aktnode
         end

         else
            FderivedNodeSet[basejindex + j] := nil;

       end;

       basejindex := basejindex + n;

     end;

     inc (i);
  end;

end;
}


procedure TuibXMLDocument.logNodeSets;
var
  i, basejindex, j: integer;
  count_not_nil: integer;
  count_not_nil_2: integer;

begin
  writelog(fdebuglevel, '');
  writelog(fdebuglevel, 'actNodeSet: ');
  count_not_nil := 0;
  for i := 0 to length(actNodeSet) - 1 do
  begin
    if actnodeset[i] = nil then
      writelog(fdebuglevel, extraindent + 'node ' + IntToStr(i) + ' null ')
    else
    begin
      writelog(fdebuglevel, extraindent + 'node ' + IntToStr(i) +
        ' elementname: "' + actNodeSet[i].NodeName + '"');
      Inc(count_not_nil);
    end;
  end;

  writelog(0, 'Non-null element(s) in act node set: ' + IntToStr(count_not_nil));

  count_not_nil_2 := 0;
  writelog(fdebuglevel, 'derivedNodeSet: ');

  for i := 0 to length(derivedNodeSet) - 1 do
  begin
    if derivednodeset[i] = nil then
      writelog(fdebuglevel, extraindent + 'node ' + IntToStr(i) + ' null ')
    else
    begin
      writelog(fdebuglevel, extraindent + 'node ' + IntToStr(i) +
        ' elementname: "' + derivedNodeSet[i].NodeName + '"');
      Inc(count_not_nil_2);
    end;
  end;

  writelog(fdebuglevel, 'Non-null element(s) remaining in children node set: ' +
    IntToStr(count_not_nil_2));
  writelog(fdebuglevel, '');
  writelog(fDebugLevel, '    -------');
end;


procedure TuibXMLDocument.delNode(nodePath: string);
var
  removeNode: TDOMNode;
begin
  writelog(fdebuglevel + 1, 'begin to del Node: ' + nodePath);
  openNode(nodePath);
  removeNode := aktNode;
  aktNode := aktNode.ParentNode;
  try
    removenode.Free;
  except
    writelog(-1, 'Error in delNode: ' + nodePath + '. Node was not removed');
  end;
end;

procedure TuibXMLDocument.delNode;
// aktnode (and all childs) will be deleted, afterwards aktnode will be parent
var
  removeNode: TDOMNode;
begin
  writelog(fdebuglevel + 1, 'begin to del Node: ' + aktNode.NodeName);
  removeNode := aktNode;
  aktNode := aktNode.ParentNode;
  try
    removenode.Free;
  except
    writelog(-1, 'Error in delNode. Node was not removed');
  end;
end;

function TuibXMLDocument.openNode(nodePath: string): boolean;
  // set aktnode
var
  nodesInPath: array[0..50] of TDOMNode;
  pathes: TStringList;
  i: integer;
  found: boolean;
  leavingPath, thisnodeName, attributeName, attributeValue: string;
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
    writelog(fdebuglevel + 1, 'begin to open nodepath: ' + nodepath);
    while i < pathes.Count + 1 do
    begin
      attributevalue := '';
      attributename := '';
      writelog(fdebuglevel + 1, 'path element ' + IntToStr(i) + ' : ' + pathes[i - 1]);
      if (pos('=', pathes[i - 1]) > 0) then // only in ths case attributes
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
      writelog(fdebuglevel + 1, 'node ' + IntToStr(i) + ': nodename ' +
        thisnodename + ' attributeName: ' + attributeName +
        ' attributeValue: ' + attributeValue
        );
      if not getNode(nodesInPath[i], nodesInPath[i - 1], thisnodeName,
        attributeName, attributeValue) then
      begin
        found := False;
        writelog(fdebuglevel, 'not found node ' + IntToStr(i) + ': nodename: ' +
          thisnodename + ' attributeName: ' + attributeName
          + ' attributeValue: ' + attributeValue
          );
      end
      else
      begin
        writelog(fdebuglevel, 'Found node ' + IntToStr(i) + ': nodename: ' +
          thisnodename + ' attributeName: ' + attributeName
          + ' attributeValue: ' + attributeValue
          );
        found := True;
      end;
      Inc(i);
    end;
    if found then
    begin
      aktNode := nodesInPath[i - 1];
      writelog(fdebuglevel, 'aktNode know node ' + IntToStr(i) + ': nodename: ' +
        aktnode.TextContent
        // TODO if multiple nodes with same textcontent, continous string of text content -
        + ' attributeName: ' + attributeName +
        ' attributeValue: ' + attributeValue);
    end
    else
      writelog(fdebuglevel, 'node not found ' + IntToStr(i) + ': nodename: ' +
        thisnodename + ' attributeName: ' + attributeName +
        ' attributeValue: ' + attributeValue);
  except
    on e: Exception do
    begin
      Result := False;
      writelog(oslog.LLerror, 'Could not open ' + nodepath + '; exception: ' + e.Message);
    end;
  end;
end;


function TuibXMLDocument.getNode(var newNode: TDOMNode; myparentNode: TDOMNode;
  mynodeName, attributeName, attributeValue: string): boolean;
var
  j, i: integer;
begin
  Result := False;
  writelog(fdebuglevel + 1, 'begin to find node  nodename: ' + mynodename
    + ' attributeName: ' + attributeName +
    ' attributeValue: ' + attributeValue);
  try
    if (myparentNode <> nil) then
    begin
      j := 0;
      while (myparentNode.hasChildNodes) and (j < myparentNode.ChildNodes.Count) and
        (Result = False) do
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
                aktNode := myparentNode.ChildNodes.Item[j];
                Result := True;
              end;
            end;
          end
          else
          begin
            newNode := myparentNode.ChildNodes.Item[j];
            aktNode := myparentNode.ChildNodes.Item[j];
            Result := True;
          end;

        Inc(j);
      end;
    end
    else
      writelog(oslog.LLerror, 'parentnode not valid');
  finally

  end;
end;

function TuibXMLDocument.makeNode(mynodeName,
  attributeName, attributeValue: string): boolean;
  // create new node, append to aktnode, set newnode as aktnode
var
  newnode: TDOMNode;
begin
  writelog(fdebuglevel + 1, 'begin to make node with nodename: ' +
    nodename + ' attributeName: ' + attributeName +
    ' attributeValue: ' + attributeValue
    );
  Result := False;
  try
    if aktnode <> nil then
    begin
      newnode := XML.CreateElement(mynodename);
      if attributeName <> '' then
      begin
        TDOMElement(newnode).SetAttribute(attributeName, attributeValue);
      end;
      aktnode.AppendChild(newnode);
      aktnode := newnode;
      Result := True;
    end
    else
      writelog(fdebuglevel + 1, 'aktnode is nil. cannot make and append node');
  except
    writelog(fdebuglevel + 1, 'error to make node with nodename: ' +
      nodename + ' attributeName: ' + attributeName +
      ' attributeValue: ' + attributeValue
      );
  end;
end;

procedure TuibXMLDocument.setNodeText(Text: string);
begin
  begin
    writelog(fdebuglevel + 1, 'begin to set text to aktNode: ' + Text);
    if aktNode <> nil then
      aktNode.TextContent := Text
    else
      writelog(fdebuglevel + 1, 'aktNode is nil, text not set: ' + Text);
  end;
end;


function TuibXMLDocument.nodeExists(nodePath: string): boolean;
  // tells if a node exists without changing anything
var
  nodesInPath: array[0..50] of TDOMNode;
  pathes: TStringList;
  i: integer;
  endOfPath, found: boolean;
  leavingPath, thisnodeName, attributeName, attributeValue: string;
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
      // nodename                          (node without attributes)
      // (!) attributeValue may contain a PATHSEPARATOR string
      // PATHSEPARATOR = ' // '
      i := 1;
      endOfPath := False;
      found := True;
      leavingPath := nodePath;
      writelog(fdebuglevel + 1, 'begin to open nodepath: ' + nodepath);
      while (i < pathes.Count + 1) and found do
      begin
        attributevalue := '';
        attributename := '';
        writelog(fdebuglevel + 1, 'path element ' + IntToStr(i) + ' : ' + pathes[i - 1]);
        if (pos('=', pathes[i - 1]) > 0) then // only in ths case attributes
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
        writelog(fdebuglevel + 1, 'node ' + IntToStr(i) + ': nodename ' +
          thisnodename + ' attributeName: ' + attributeName
          + ' attributeValue: ' + attributeValue
          );
        if not getNode(nodesInPath[i], nodesInPath[i - 1], thisnodeName,
          attributeName, attributeValue) then
        begin
          found := False;
          writelog(fdebuglevel, 'not found node ' + IntToStr(i) + ': nodename: ' +
            thisnodename + ' attributeName: ' + attributeName
            + ' attributeValue: ' + attributeValue
            );
        end
        else
        begin
          writelog(fdebuglevel, 'Found node ' + IntToStr(i) + ': nodename: ' +
            thisnodename + ' attributeName: ' + attributeName
            + ' attributeValue: ' + attributeValue
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
      ' attributeValue: ' + attributeValue, oslog.LLwarning);
    nodeExists := False;
  end;
end;



//*************  XML Attribute-Handling ***********************************
function TuibXMLDocument.hasAttribute(attributename: string): boolean;
var
  i: integer;
begin
  Result := False;
  if (aktnode.HasAttributes) then
  begin
    for i := 0 to aktnode.Attributes.Length - 1 do
    begin
      if (aktnode.Attributes[i].NodeName = attributename) then
      begin
        Result := True;
        writelog(fdebuglevel + 1, 'aktnode has attribute with name: '
          + attributeName);
      end;
    end;
  end;
  if Result = False then
    writelog(fdebuglevel + 1, 'aktnode has no attribute with name: '
      + attributeName);
end;

procedure TuibXMLDocument.setAttribute(attributeName, attributeValue: string);
// attributeName may include a namespace prefix --- no more
// set if exists otherwise create
var
  i: integer;
begin
  writelog(fdebuglevel + 1, 'begin to set attribute: name: ' +
    attributeName + ' value: ' + attributeValue);
  if aktnode.HasAttributes then
  begin
    for i := 0 to aktnode.Attributes.Length - 1 do
      if aktnode.Attributes[i].NodeName = attributeName then
      begin
        aktnode.Attributes[i].TextContent := attributevalue;
      end;
  end
  else
  begin
    TDOMElement(aktnode).SetAttribute(attributeName, attributeValue);
    LogDatei.log('create attribute with name: ' +
      attributeName + ' value: ' + attributeValue, oslog.LLinfo);
    //writelog(fdebuglevel + 1,'create attribute with name: '
    //        +attributeName+' value: '+attributeValue);
  end;
end;

procedure TuibXMLDocument.addAttribute(attributeName, attributeValue: string);
// attributeName may include a namespace prefix --- no more
begin
  writelog(fdebuglevel + 1, 'begin to add attribute: name: ' +
    attributeName + ' value: ' + attributeValue);
  TDOMElement(aktnode).SetAttribute(attributeName, attributeValue);
end;

procedure TuibXMLDocument.delAttribute(attributeName: string);
begin
  writelog(fdebuglevel + 1, 'begin to del attribute: name: ' + attributeName);
  if (aktnode.HasAttributes) then
    TDOMElement(aktnode).RemoveAttribute(attributeName);
end;


function TuibXMLDocument.getAttributeValue(attributeName: string): string;
  // var
  // get the attribute value at the actual node
  // ATTENTION: it returns an empty string:
  // - if there is no attribute with this name
  // - if the value of this attribute is an empty string
begin
  writelog(fdebuglevel + 1, 'begin to get value of attribute: name: '
    + attributeName);
  getAttributeValue := '';
  if (aktnode <> nil) and aktNode.HasAttributes then
    getAttributeValue := TDOMElement(aktnode).GetAttribute(attributeName);
end;



end.
