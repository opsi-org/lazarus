unit wixml;

// This code is part of the opsi.org project
//
// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the General Public License.
//
// Text of the GPL: http://www.gnu.org/licenses/gpl.html
// Unofficial GPL Translations: http://www.gnu.org/licenses/translations.html
//
// author: Rupert Roeder, APohl, detlef oertel
// credits: http://www.opsi.org/credits/



interface

uses
	SysUtils, Classes, Types,
	//dom,
	xmldom,
	XMLIntf,
	XMLDoc,
	Dialogs, StrUtils,
	oxmldom,
	ExtCtrls,
	xmlpluginfunc,
	//xmlpluginsynth,
	pluginlog;


type

 TNodeSet = array of IXMLNode;

 TStringsArray = array of string;
 TbooleansArray = array of boolean;

 TuibXMLDocument = class (TXMLDocument)
   private

    fActNodeSet : TNodeSet;
    fDerivedNodeSet : TNodeSet;
    fDebugLevel : Integer;

    function getCountNotNil : Integer;
    function getCountDerivedNotNil : Integer;

    function getNode(var newNode:IXMLNode; parentNode:IXMLNode;
                     nodeName, attributeName, attributeValue : string):boolean;
    function makeNode(var newNode:IXMLNode; parentNode:IXMLNode;
                     nodeName, attributeName, attributeValue : string):boolean;
    function makeNodeAtPos(var newNode:IXMLNode; parentNode:IXMLNode;
                         nodeName, attributeName, attributeValue : string;
                         Position : integer):boolean;

    function filterByAttributeName_existing (name: string) : Boolean;
    function filterByAttribute_existing (name: string; value : string) : Boolean;

   public
    destructor destroy; override;

    property debuglevel : Integer read FDebugLevel write FDebugLevel;
    property actNodeSet : TNodeSet read factNodeSet write factNodeSet;
    property derivedNodeSet : TNodeSet read fDerivedNodeSet write fDerivedNodeSet;
    property CountNotNil : Integer read getCountNotNil;
    property CountDerivedNotNil : Integer read getCountDerivedNotNil;

    procedure getNamespaceAndBasename(prefixedName :String;
                                  var namespaceUri : String;
                                  var baseName : String);


    procedure setlengthActNodeSet (newlength : Integer);
    procedure setlengthDerivedNodeSet (newlength : Integer);
    procedure logNodeSets;

    function makeNodes
                     (nodeName : String;
                     const attributeNames:TStringsArray;
                     const attributeValues : TStringsArray;
                     textcontent : String;
                     index : Integer
                     )
                     :boolean;


    procedure openXmlFile(filename : string);
    procedure saveXmlFile(filename : string);
    procedure formatMyXml(input : String; var output : TStrings);
    procedure createXmlFile(filename : string; stringlist : TStringList);
    function getXmlStrings : TStrings;

    procedure getNextGenerationActNodeSet;
    procedure makeNewDerivedNodeSet;
    function filterByAttributeList
                   (var attributenames : Tstringsarray;
                    var attributevalueExists : Tbooleansarray;
                    var attributevalues : TStringsArray;
                    attributes_strict : boolean) : Boolean;

    function filterByChildElement (filtering: boolean; elementname: string) : Boolean;

    function filterByText (filtering: boolean; textvalue: string) : Boolean;

    {
    // The main idea for the following syntax:

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
    // (!) no whitespaces between attributeValue and Pathseparator
    // eg:
    // foo bar="bar1"/childfoo =""/RDF:foo NC:bar=nonesense

    function openNode(nodePath : string) : Boolean;
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

    function hasAttribute (attributename : String): boolean;
    // checks if the actual node has the attribute
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
    }
   end;


const
  domvendorconfigured = 'Xerces XML';


implementation

const
 PATHSEPARATOR : string ='/';
 extraindent = '   ';


var

 showErrors : boolean = true;
 //XMLDocument: TXMLDocument;


function getTag (line : String) : String;
 var
   p :Integer;
   r, s : String;
begin
   result := '';
   p := pos ('<', line);
   if p > -1
   then
   Begin
      s := copy (line, p + 1, length(line));
      getWord  (s, result, r, [' ', '>', '/']);
   end;
end;


procedure setShowErrors(bValue : boolean);
begin
 showErrors := bValue;
end;

 procedure writelog(level: integer; logmessage : string);
 begin
  LogDatei.DependentAdd (logmessage, level);
  {
  //if externLogProc <> nil then
   externLogProc(logmessage, level);
  if showErrors and (level=0) then
  begin
   MessageDlg('Error: '+logmessage, mtError, [mbOK], 0, mbOK);
  end;
  }
 end;



//*************  TuibXMLDocument ***********************************
//************* Namespace handling *******************************

destructor TuibXMLDocument.destroy;
begin
  inherited destroy;
end;


procedure TuibXMLDocument.getNamespaceAndBasename(prefixedName :String;
                                  var namespaceUri : String;
                                  var baseName : String);
 // prefixedName may include a namespace prefix
var
 // if we have a namespace like xmlns:NC="http://home.netscape.com/NC-rdf#"
namespacePrefix : String; // is NC
//namespaceUri : String;   // is http://home.netscape.com/NC-rdf#
attribbasename :String;  // attributname without prefix
namespace_separator : integer; // pos of prefix separator ':'
begin
 begin
  namespacePrefix := '';
  namespaceUri := '';
  writelog(fdebuglevel + 1,'begin to get namespace from : '+prefixedName);
  namespace_separator := pos(':',prefixedName);
  if namespace_separator > 0 then
  begin
   namespacePrefix := copy(prefixedName,1,namespace_separator-1);
   namespaceUri :=
    DocumentElement.FindNamespaceURI(namespacePrefix);
   baseName := copy(prefixedName,namespace_separator+1,length(prefixedName));
  end
  else baseName := prefixedName;
 end;
end;


function TuibXMLDocument.getCountNotNil : Integer;
var
 i : Integer;
begin
  result := 0;
  for i := 0 to length(actNodeSet) - 1
  do
  begin
    if actnodeset[i] <> nil
    then
      inc (result);
  end;
end;

function TuibXMLDocument.getCountDerivedNotNil : Integer;
var
 i : Integer;
begin
  result := 0;
  for i := 0 to length(DerivedNodeSet) - 1
  do
  begin
    if derivednodeset[i] <> nil
    then
      inc (result);
  end;
end;



//*************  XML File-Handling ***********************************
procedure TuibXMLDocument.createXmlFile(filename: String; stringlist : TStringList);
begin
 begin
  writelog(fdebuglevel + 1,'begin to create File: '+filename);
  stringlist.SaveToFile(filename);
  writelog(fdebuglevel,'File: '+filename+' created');
 end;
 openXmlFile(filename);
end;


procedure TuibXMLDocument.openXmlFile(filename : string);
begin
 begin
  writelog(fdebuglevel + 1,'begin to open File: '+filename);
  //DatamoduleXML.XMLDocument := TXMLDocument.Create(nil);
  //DatamoduleXML.XMLDocument.DOMVendor:= GetDOMVendor('Xerces XML');
  DOMVendor:= GetDOMVendor('Open XML');
  writelog(fdebuglevel + 1,'XMLDocument domvendor ');
  ParseOptions := [];
  writelog(fdebuglevel + 1,'XMLDocument parseoptions [] ');
  if Active then Active := false;
  writelog(fdebuglevel + 1,'XMLDocument not active ');
  LoadFromFile(FileName);
  writelog(fdebuglevel + 1,'begin to load File: '+filename);
  // possible values: (doNodeAutoCreate, doNodeAutoIndent, doAttrNull, doAutoPrefix, doNamespaceDecl, doAutoSave);
  // (doAttrNull=false) give empty string if attributename not exists
  Options := [doNodeAutoIndent, doAutoPrefix, doNamespaceDecl, doAttrNull];
  writelog(fdebuglevel + 1,'XMLDocument options  ');
  //DatamoduleXML.XMLDocument.Options := [];
  Active := true;
  writelog(fdebuglevel + 1,'File: '+filename+' opend');
 end;
end;

procedure TuibXMLDocument.formatMyXml(input : String; var output : TStrings);
 var
  endFound : boolean;
  restString, newstr, outstr, teststr : String;
  endPos : integer;
  einruecken : Integer;
  j : Integer;
  line0 : boolean;
 begin
  endFound := false;
  output.Clear;
  newstr := '';
  restString := input;

  // uri
  endPos := pos('>',restString);
  if endPos > 0 then
  begin
    newstr := trim(copy(restString,0,endPos));
    restString := trim(copy(restString, endPos+1, length(restString)));
    output.Add(newstr);
  end
  else endfound := true;

  einruecken := 0;

  while not endFound do
  begin
   endPos := pos('>',restString);
   if endPos > 0 then
   begin
    newstr := trim(copy(restString,0,endPos));
    restString := trim(copy(restString, endPos+1, length(restString)));
    if not AnsiStartsStr('<',restString) then
    begin
     endPos := pos('>',restString);
     if endPos > 0 then
     begin
      newstr := newstr + trim(copy(restString,0,endPos));
      restString := trim(copy(restString,endPos+1, length(restString)));
     end;
    end;

    if (AnsiStartsStr ('</', newstr) or AnsiEndsStr ('/>', newstr))
     then einruecken := einruecken - 4;

    outstr := newstr;
    for j:= 1 to einruecken do outstr := ' ' + outstr;
    output.Add(outstr);
    if not  (AnsiStartsStr ('<!', newstr) or  AnsiStartsStr ('<?', newstr))
    then
    begin
     if  not ((AnsiStartsStr ('</', newstr) or AnsiEndsStr ('/>', newstr)))
     then
     Begin

       teststr := '</' + GetTag (newStr);

       if not AnsiContainsText (newStr, teststr)
         //indent only if the opening tag is not closed again
       then
         einruecken := einruecken + 4;
     end;
    end;

    newstr :='';
   end
   else endFound := true;
  end;
 end;

procedure TuibXMLDocument.saveXmlFile(filename : string);
var
 myFile : TextFile;
 myXml : String;
 xmlStrings : TStrings;
 i : integer;

begin
 begin
  writelog(fdebuglevel + 1,'begin to save File: '+filename);
  //DatamoduleXML.XMLDocument.saveToFile(FileName);
  saveToXML(myXml);
  xmlStrings := TStringList.Create;
  formatMyXml(myXml,xmlStrings);
  AssignFile(myFile,filename);
  SetLineBreakStyle(myFile, tlbsCRLF);
  rewrite(myFile);
  for i:=0 to xmlStrings.Count-1 do
   writeln(myFile,xmlStrings[i]);
  CloseFile(myFile);
  xmlStrings.Free;
 end;
end;

function TuibXMLDocument.getXmlStrings : TStrings;
var
myXml : String;
xmlStrings : TStrings;
begin
 if Active then
 begin
  saveToXML(myXml);
  xmlStrings := TStringList.Create;
  formatMyXml(myXml,xmlStrings);
  getXmlStrings := xmlStrings;
 end
 else getXmlStrings := TStringList.Create;
end;


procedure TuibXMLDocument.setlengthActNodeSet (newlength : Integer);
begin
  setlength (factnodeset, newlength);
end;

procedure TuibXMLDocument.setlengthDerivedNodeSet (newlength : Integer);
begin
  setlength (fderivednodeset, newlength);
end;


//*************  XML Node-Handling ***********************************



procedure TuibXMLDocument.makeNewDerivedNodeSet;
 var i, n, j, basejindex: Integer;

begin
  setLengthDerivedNodeSet (0);
  n := length (actNodeSet);
  for i:= 0 to n - 1
  do
  begin
    if actnodeset[i]<> nil
    then setlengthDerivedNodeSet (length(FDerivednodeset) + actnodeset[i].ChildNodes.Count);
  end;
  { fill with values }
  basejindex := 0;
  for i:= 0 to n - 1
  do
  begin
    if actnodeset[i]<> nil
    then
    begin
      for  j := 0 to actNodeSet[i].childnodes.count - 1
      do
        FDerivednodeset[basejindex + j] := actNodeset[i].childnodes[j];
      basejindex := basejindex + actNodeSet[i].childnodes.count;
    end
  end;
end;

procedure TuibXMLDocument.getNextGenerationActNodeSet;
// thereby removing the nil elements
 var
 i, n : Integer;
begin
  writelog (fdebuglevel, 'Going to the next level ...');
  setlengthActNodeSet (0);
  n := length (FderivedNodeSet);
  for i:= 0 to n - 1
  do
  begin
    if derivednodeset[i]<> nil
    then
    begin
      setlengthActNodeSet (length(FActnodeset) + 1);
      FActNodeset[length(FActNodeSet) - 1] := derivednodeset[i]
    end
  end;
end;

function TuibXMLDocument.filterByAttributeList
                   (var attributenames : Tstringsarray;
                    var attributevalueExists : Tbooleansarray;
                    var attributevalues : TStringsArray;
                    attributes_strict : boolean) : Boolean;

  var
  i, n, j, basejindex, k, numberOfAttributes: Integer;

  b0, b1, b2 : boolean; // results of the crucial evaluations for each attribute

  uri, attributename : String;


  goon : Boolean;

begin
  result := true;

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
              and
              ( actNodeSet[i].childnodes[j].AttributeNodes.Count <> length (attributenames) )
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

             b1 := actNodeSet[i].childnodes[j].hasAttribute (attributename, uri);
             if b1
             then
               b2 := actNodeSet[i].childnodes[j].getattributeNs(attributename, uri) = attributevalues[k]
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


end;


function TuibXMLDocument.filterByAttributeName_existing (name: string) : Boolean;
var
  i, n, j, basejindex: Integer;
  b0, b1 : boolean;

  attributeparts : TXStringList;
  uri, attributename : String;

begin
  writelog (fdebuglevel , 'retaining child elements with attribute "' + name + '"');

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

         getNamespaceAndBasename (name, uri, attributename);

         b1 := actNodeSet[i].childnodes[j].hasAttribute (attributename, uri);
         b0 := FderivedNodeSet[basejindex + j] <> nil;

         if  b1 and b0
         then
            FderivedNodeSet[basejindex + j] := actNodeSet[i].childnodes[j]
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

  attributeparts : TXStringList;
  uri, attributename : String;


begin
  writelog (fdebuglevel, 'retaining child elements with attribute "' + name + '" value: "' + value + '"');

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

         getNamespaceAndBasename (name, uri, attributename);

         b0 := FderivedNodeSet[basejindex + j] <> nil;
         b1 := actNodeSet[i].childnodes[j].hasAttribute (attributename, uri);
         b2 := actNodeSet[i].childnodes[j].getattributeNs(attributename, uri) = value;

         if b0 and b1 and b2
         then
            FderivedNodeSet[basejindex + j] := actNodeSet[i].childnodes[j]
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
  comparename : String;

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
         comparename := actNodeSet[i].childnodes[j].NodeName;

         if compareStr ( elementname, comparename ) = 0
         then
         begin
            //FderivedNodeSet[basejindex + j] := actNodeSet[i].childnodes[j];
         end

         else
            FderivedNodeSet[basejindex + j] := nil;

       end;

       basejindex := basejindex + n;

     end;

     inc (i);
  end;

end;


function TuibXMLDocument.filterByText (filtering: boolean; textvalue: string) : Boolean;
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
         if actNodeSet[i].childnodes[j].IsTextElement
         then
           comparetext := actNodeSet[i].childnodes[j].Text
         else
           comparetext := '';

         if AnsiCompareStr ( textvalue, comparetext ) = 0
         then
         begin
            //FderivedNodeSet[basejindex + j] = actNodeSet[i].childnodes[j] stays
         end

         else
            FderivedNodeSet[basejindex + j] := nil;

       end;

       basejindex := basejindex + n;

     end;

     inc (i);
  end;

end;


procedure TuibXMLDocument.logNodeSets ;
 var i, basejindex, j: Integer;
   count_not_nil : Integer;
   count_not_nil_2 : Integer;

begin
  writelog (fdebuglevel, '');
  writelog (fdebuglevel, 'actNodeSet: ');
  count_not_nil := 0;
  for i := 0 to length(actNodeSet) - 1
  do
  begin
    if actnodeset[i] = nil
    then
    writelog (fdebuglevel, extraindent + 'node ' + inttostr (i) + ' null ')
    else
    begin
      writelog (fdebuglevel, extraindent + 'node ' + inttostr (i) + ' elementname: "' + actNodeSet[i].NodeName  + '"');
      inc (count_not_nil);
    end
  end;

  writelog (0, 'Non-null element(s) in act node set: ' + inttostr (count_not_nil));

  count_not_nil_2 := 0;
  writelog (fdebuglevel, 'derivedNodeSet: ');

  for i := 0 to length(derivedNodeSet) - 1
  do
  begin
    if derivednodeset[i] = nil
    then
    writelog (fdebuglevel, extraindent +  'node ' + inttostr (i) + ' null ')
    else
    begin
      writelog (fdebuglevel, extraindent +  'node ' + inttostr (i) + ' elementname: "' + derivedNodeSet[i].NodeName  + '"');
      inc (count_not_nil_2);
    end
  end;

  writelog (fdebuglevel, 'Non-null element(s) remaining in children node set: ' + inttostr (count_not_nil_2));
  writelog (fdebuglevel, '');
  writelog (fDebugLevel, '    -------');
end;

{
procedure TuibXMLDocument.delNode(nodePath : string);
var
 removeNode : IXMLNode;
begin
 begin
  writelog(fdebuglevel + 1,'begin to del Node: '+nodePath);
  openNode(nodePath);
  removeNode := aktNode;
  aktNode := aktNode.ParentNode;
  if aktNode.ChildNodes.Remove(removeNode) = -1 then
   writelog(-1,'Error: last node of nodePath: '+nodePath+' was not removed');
 end;
end;
}

{
function TuibXMLDocument.openNode(nodePath : string) : boolean;
var
 nodesInPath : array[0..50] of IXMLNode;
 i : integer;
 endOfPath : boolean;
 leavingPath, nodeName, attributeName, attributeValue : string;
begin

 result := true;
 try
  // the root node
  nodesInPath[0] := DocumentElement;
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
   writelog(fdebuglevel + 1,'begin to open nodepath: '+leavingPath);
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
   writelog(fdebuglevel + 1,'node '+IntToStr(i)+': nodename: '+nodename
               +' attributeName: '+attributeName
               +' attributeValue: '+attributeValue
               +' leavingPath: '+leavingPath);
   if not getNode(nodesInPath[i],nodesInPath[i-1],
                     nodeName, attributeName, attributeValue) then
   begin
    makeNode(nodesInPath[i],nodesInPath[i-1],
                     nodeName, attributeName, attributeValue);
    writelog(fdebuglevel,'Created node '+IntToStr(i)+': nodename: '+nodename
               +' attributeName: '+attributeName
               +' attributeValue: '+attributeValue
               +' leavingPath: '+leavingPath);
   end
   else writelog(fdebuglevel,'Found node '+IntToStr(i)+': nodename: '+nodename
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
  writelog(fdebuglevel,'aktNode know node '+IntToStr(i)+': nodename: '+nodename
              +' attributeName: '+attributeName
              +' attributeValue: '+attributeValue);
 except
   on e: Exception do
    begin
      result := false;
      //errorInfo := 'Could not open ' + nodepath + '; exception: ' + e.Message
    end;
 end;

end;
}

function TuibXMLDocument.makeNodes
                     (nodeName : String;
                     const attributeNames:TStringsArray;
                     const attributeValues : TStringsArray;
                     textcontent : String;
                     index : Integer)
                     :boolean;
 var
  i, j, k, numberOfAttributes : Integer;
  attributename, namespaceUri : String;
  nodenameStr : String;
  newNode, parentNode : IXMLNode;
begin
  result := true;

  for i := 0 to length(factNodeset) - 1
  do
  Begin
    parentNode := factNodeset[i];
    if parentNode <> nil
    then
    begin


        //APO(02.05.2007:
        // newNode := parentNode.AddChild(nodeName,index);
        //Die Methode AddChild oben ist eine spezielle Form der nachfolgenden Methode
        //Da der Namespace des Parents weitervererbt wird, erscheint der zugeh. Pr‰fix
        //bei Unterknoten, wenn kein Pr‰fix explizit angegeben wird. Dies ist aber
        //i.d.R. nicht gewollt. Deshalb ist das
        //Besorgen des Namespaces anhand des konkreten Prefixes,
        //der explizit angegeben werden muﬂ, der sinnvollere Weg
        getNamespaceAndBasename(nodename,namespaceUri,nodenameStr);
        newNode := parentNode.AddChild(nodeName,namespaceUri{parentNode.GetNamespaceURI},False,index);
        //APO Ende

        for k := 0 to length(attributeNames)-1
        do
        begin
          if attributeNames[k] <> '' then
          begin
           getNamespaceAndBasename(attributeNames[k],namespaceUri,attributename);
           newNode.SetAttributeNS(attributename,namespaceUri,attributeValues[k]);
          end;
        end;

        if textcontent <> ''
        then
        try
          newNode.text := textcontent;
          writelog (fdebuglevel, extraindent + extraindent + 'text "' + textcontent
                + '" set');

        except
          writelog (fdebuglevel - 3, extraindent + extraindent + 'text "' + textcontent
                + '" can not be set for this node');
        end;


        setlength (fDerivedNodeSet, length (fDerivedNodeSet) + 1);
        fDerivedNodeSet[length(fDerivedNodeSet) - 1] := newNode;

        writelog (fdebuglevel - 1, extraindent + 'Node created with: ');
        writelog (fdebuglevel - 1, extraindent + extraindent + 'elementname = "' + nodename + '"');

        for k := 0 to length(attributeNames)-1
        do
        begin
          if attributeNames[k] <> '' then
          begin
            writelog (fdebuglevel - 1, extraindent + extraindent + 'attribute "' + attributeNames[k]
                + '" value = "' + attributevalues[k] + '"');
          end;
        end;



    end;


  end;

end;


function TuibXMLDocument.getNode(var newNode:IXMLNode; parentNode:IXMLNode;
                     nodeName, attributeName, attributeValue : string):boolean;
var
 namespaceUri,attribbasename :String;
 found : boolean;
 i, nodezahl : integer;
 searchname, searchvalue : string;
begin
 getNode := false;
 begin
  writelog(fdebuglevel + 1,'begin to find node  nodename: '+nodename
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
   writelog(fdebuglevel + 1,'begin to get attribute: name: '
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
 end;
end;


function TuibXMLDocument.makeNode(var newNode:IXMLNode; parentNode:IXMLNode;
                     nodeName, attributeName, attributeValue : string):boolean; 
begin
 makeNode := makeNodeAtPos(newNode, parentNode,
                           nodeName, attributeName,
                           attributeValue, -1);
end;


function TuibXMLDocument.makeNodeAtPos(var newNode:IXMLNode; parentNode:IXMLNode;
                     nodeName, attributeName, attributeValue : string;
                     Position : integer):boolean;
var
 namespaceUri,attribbasename :String;
begin
 makeNodeAtPos := false;
 begin
  writelog(fdebuglevel + 1,'begin to make node nodename: '+nodename
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
 end;
end;

{
procedure TuibXMLDocument.setNodeAtPosition(nodeName, attributeName, attributeValue : string;
                             position : integer);
var
 newNode : IXMLNode;
begin
 begin
  //
  if makeNodeAtPos(newNode, aktNode,
                     nodeName, attributeName, attributeValue,
                     Position) then
  begin
   writelog(fdebuglevel - 1,'new node created: '+nodeName+' '+attributeName+'='+attributeValue);
   aktNode := newNode;
  end
  else writelog(0,'new node not created: '+nodeName+' '+attributeName+'='+attributeValue)
 end;
end;
}

{
procedure TuibXMLDocument.setNodeText(text : string);
begin
 begin
  writelog(fdebuglevel + 1,'begin to set text to aktNode: '+text);
  aktNode.Text := text;
 end;
end;
}

{
function TuibXMLDocument.nodeExists(nodePath : string) : boolean;
// tells if a node exists without changing anything
var
 nodesInPath : array[0..50] of IXMLNode;
 i : integer;
 endOfPath, found : boolean;
 leavingPath, nodeName, attributeName, attributeValue : string;
begin
 nodeExists := false;
 begin
  // the root node
  nodesInPath[0] := DocumentElement;
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
   writelog(fdebuglevel + 1,'begin to open nodepath: '+leavingPath);
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
   writelog(fdebuglevel + 1,'node '+IntToStr(i)+': nodename'+nodename
               +' attributeName: '+attributeName
               +' attributeValue: '+attributeValue
               +' leavingPath: '+leavingPath);
   if not getNode(nodesInPath[i],nodesInPath[i-1],
                     nodeName, attributeName, attributeValue) then
   begin
    found := false;
    writelog(fdebuglevel,'not found node '+IntToStr(i)+': nodename: '+nodename
               +' attributeName: '+attributeName
               +' attributeValue: '+attributeValue
               +' leavingPath: '+leavingPath);
   end
   else
   begin
    writelog(fdebuglevel,'Found node '+IntToStr(i)+': nodename: '+nodename
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
 end;
end;

}

//*************  XML Attribute-Handling ***********************************

{
procedure TuibXMLDocument.setAttribute(attributeName, attributeValue : string);
 // attributeName may include a namespace prefix
var
namespaceUri,attribbasename :String;
begin
 begin
  writelog(fdebuglevel + 1,'begin to set attribute: name: '
              +attributeName+' value: '+attributeValue);
  getNamespaceAndBasename(attributeName,namespaceUri,attribbasename);
  aktNode.SetAttributeNS(attribbasename,namespaceUri,attributeValue);
  //aktNode.Attributes[attribbasename] := attributeValue;
 end;
end;


function TuibXMLDocument.hasAttribute (attributename : String): boolean;
 var
 namespaceUri,attribbasename :String;
begin
  getNamespaceAndBasename(attributeName,namespaceUri,attribbasename);
  result := aktNode.HasAttribute(attribbasename,namespaceUri)
end;

/// ???

procedure TuibXMLDocument.addAttribute(attributeName, attributeValue : string);
var
namespaceUri,attribbasename :String;
begin
 begin
  writelog(fdebuglevel + 1,'begin to add attribute: name: '
              +attributeName+' value: '+attributeValue);
  getNamespaceAndBasename(attributeName,namespaceUri,attribbasename);
  if not aktNode.HasAttribute(attribbasename,namespaceUri) then
   aktNode.SetAttributeNS(attribbasename,namespaceUri,attributeValue);
 end;
end;



procedure TuibXMLDocument.delAttribute(attributeName : string);
var
namespaceUri,attribbasename :String;
begin
 begin
  writelog(fdebuglevel + 1,'begin to del attribute: name: '+attributeName);
  getNamespaceAndBasename(attributeName,namespaceUri,attribbasename);
  if aktNode.HasAttribute(attribbasename,namespaceUri) then
   aktNode.AttributeNodes.Delete(attribbasename,namespaceUri);
 end;
end;

/// ???
function TuibXMLDocument.getAttributeValue(attributeName : string) : string;
var
namespaceUri,attribbasename :String;
// get the attribute value at the actual node
// ATTENTION: it returns an empty string:
// - if there is no attribute with this name
// - if the value of this attribute is an empty string
begin
 begin
  writelog(fdebuglevel + 1,'begin to get value of attribute: name: '
              +attributeName);
  getAttributeValue := '';
  getNamespaceAndBasename(attributeName,namespaceUri,attribbasename);
  if aktNode.HasAttribute(attribbasename,namespaceUri) then
   getAttributeValue := aktNode.GetAttributeNS(attribbasename,namespaceUri);
 end;
end;

}

end.


