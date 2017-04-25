unit xmlsectiontestu1;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons,  DOM, oslog, osxmlsections;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Close: TBitBtn;
    memo3tomemo1: TButton;
    openFile: TButton;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    Memo3: TMemo;
    procedure addPackagesClick(Sender: TObject);
    procedure unatt_win10Click(Sender: TObject);
    procedure CloseClick(Sender: TObject);
    procedure configXMLClick(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure openFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Memo2Change(Sender: TObject);
    private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  logfilename: String;

implementation
{$R *.lfm}

{ TForm1 }

function memoToTStringlist (mymemo: TMemo) : TStringlist;
begin
  Result := TStringList.Create;
  Result.Assign(mymemo.Lines);
end;
function textToTStringlist (text: string) : TStringlist;
begin
  Result := TStringList.Create;
  Result.Append(text);
end;

procedure TForm1.openFileClick(Sender: TObject);
var XMLDocObject: TuibXMLDocument;
begin
   XMLDocObject:= TuibXMLDocument.Create;
   Memo1.Clear;
   OpenDialog1.Filter:='xml-file | *.xml';
   OpenDialog1.Title:='Vorhandene XML-Datei öffnen';
   if OpenDialog1.Execute then
   begin
      if XMLDocObject.openXmlFile(OpenDialog1.FileName) then
        Memo1.Append(XMLDocObject.getXmlStrings().Text)
      else
        Memo1.Append('can not read file');
   end
   else
     Memo1.Append (' no file ');
   XMLDocObject.destroy;
end;



procedure TForm1.FormCreate(Sender: TObject);
begin

end;

procedure TForm1.Memo2Change(Sender: TObject);
begin

end;

procedure TForm1.CloseClick(Sender: TObject);
begin
  Logdatei.log('end of logging ', oslog.LLinfo);
  LogDatei.Close;
  Application.Terminate;
end;

procedure TForm1.unatt_win10Click(Sender: TObject);
var XMLDocObject: TuibXMLDocument;
    k:integer;
begin
  Memo3.Clear;
  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
  XMLDocObject:= TuibXMLDocument.Create;
  // createXMLDoc
  if XMLDocObject.createXmlDocFromStringlist(memoToTStringlist(memo1)) then
    LogDatei.log('success: create xmldoc from stringlist',oslog.LLinfo)
  else
    LogDatei.log('failed: create xmldoc from stringlist',oslog.LLinfo);
  XMLDocObject.setlengthActNodeSet  (1);
  XMLDocObject.actnodeset[0] := XMLDocObject.getDocumentElement;
  for k:= 0 to length(XMLDocObject.actNodeSet)-1 do
    if XMLDocObject.actNodeSet[k] <> nil then
      LogDatei.log('actNodeSet <> nil',oslog.LLinfo)
    else
      LogDatei.log('actNodeSet = nil',oslog.LLinfo);
  XMLDocObject.makeNewDerivedNodeSet;
  XMLDocObject.logNodeSets;
  //XMLDocObject.filterByChildElement (true, 'settings');
  //XMLDocObject.logNodeSets;
  // Nodetext setzen und Attribut setzen :   SetText, SetAttribute
  if XMLDocObject.nodeExists('settings pass="windowsPE" // component name="Microsoft-Windows-Setup" // DiskConfiguration // Disk wcm:action="add"') then
    if XMLDocObject.openNode('settings pass="windowsPE" // component name="Microsoft-Windows-Setup" // DiskConfiguration // Disk wcm:action="add" // ModifyPartitions') then
    begin
      XMLDocObject.setNodeText('***ModifyPartitions wurde ersetzt***');
      XMLDocObject.setAttribute('testname','testvalue');
    end;

  // Knoten löschen: DeleteElement
  // muss kein openNode gemacht werden, ist implizit. Wenn der Knoten nicht gefunden wird, wird der zuletzt gefundene
  // übergeordnete Knoten gelöscht. Daher zuvor ein nodeExists!!
  if XMLDocObject.nodeExists('settings pass="windowsPE" // component name="Microsoft-Windows-Setup" // DiskConfiguration // Disk wcm:action="add" // WillWipeDisk') then
    XMLDocObject.delNode('settings pass="windowsPE" // component name="Microsoft-Windows-Setup" // DiskConfiguration // Disk wcm:action="add" // WillWipeDisk');

  // neuen Knoten setzen : am aktuellen Knoten wird ein Knoten angehängt, der neue Knoten wird aktueller Knoten
  if XMLDocObject.nodeExists('settings pass="windowsPE" // component name="Microsoft-Windows-Setup" // DiskConfiguration') then
    if XMLDocObject.openNode('settings pass="windowsPE" // component name="Microsoft-Windows-Setup" // DiskConfiguration') then
    begin
      XMLDocObject.makeNode('newnode','','');
      XMLDocObject.setNodeText('newnode Text setzen');
    end;

  // Attribute löschen und setzen :  DeleteAttribute, AddAttribute, SetAttribute
  if XMLDocObject.nodeExists('settings pass="windowsPE" // component name="Microsoft-Windows-Setup" // DiskConfiguration // Disk wcm:action="add"') then
    if XMLDocObject.openNode('settings pass="windowsPE" // component name="Microsoft-Windows-Setup" // DiskConfiguration // Disk wcm:action="add"') then
    begin

      XMLDocObject.delAttribute('wcm:action');
      XMLDocObject.addAttribute('newwcm','newwcmAttribute');
      XMLDocObject.setAttribute('newAttribute','newValue');
    end;

  // TODO : wie geht Suche mit mehreren Attributen? Notwendig?
  // TODO : addText
  // AddText "rtf" : sets the text only if there was no text node given

  // setText: neuen Text setzen. Jeglicher anderer Inhalt wird ersetzt, auch XML_Blätter. Kein XML-Fragment!
  if XMLDocObject.nodeExists ('settings pass="windowsPE" // component name="Microsoft-Windows-Setup" // UserData // ProductKey // WillShowUI') then
    if XMLDocObject.openNode('settings pass="windowsPE" // component name="Microsoft-Windows-Setup" // UserData // ProductKey // WillShowUI') then
        XMLDocObject.setNodeText('nodeText wurde gesetzt');

  // setText '' : löschen des komplette Knotens

  {
    return elements
    fills the selected elements completely (element name and attributes) into the return list.
    return attributes
    produces a list of the attributes.
    return elementnames
    produces a list of the element names.
    return attributenames gives a list only of the attribute names.
    return text
    list all textual content of the selected elements.
    return counting
    gives a report with numerical informations: line 0 contains the number of selected elements, line 1 the number of attributes.
  }
  Memo3.Append('aktnode Attributvalue für wcm-nix: ' + XMLDocObject.getAttributeValue('wcm-nix'));  // liefert nichts zurück, da es das Attribut nicht gibt.
  Memo3.Append('aktnode Attributvalue für Leerstring: ' + XMLDocObject.getAttributeValue(''));  // liefert auch nichts zurück, da es das Attribut leer ist nicht gibt.  XMLDocObject.makeNode('neuerKnoten','mitAttribut','undValue');
  XMLDocObject.logNodeSets;
  //XMLDocObject.getNextGenerationActNodeSet;
  {
  XMLDocObject.makeNewDerivedNodeSet;
  if xmldoc.actNodeSet[k] <> nil
  xmldoc.getNamespaceAndBasename (attributename, uri, attributename0);
  xmldoc.actNodeSet[k].hasAttribute (attributename0, uri);
  xmldoc.actNodeSet[k].getattributeNs(attributename0, uri);
  xmldoc.actNodeSet[k].setAttributeNS(attributename0,uri,attributeValue);
  xmldoc.actNodeSet[k].IsTextElement
  xmldoc.actNodeSet[k].text
  xmldoc.actNodeset[k].AttributeNodes.Count

  xmldoc.filterByChildElement(elementnamefiltering, elementname);

  xmldoc.filterByText(textfiltering, textvalue);
  xmldoc.filterByAttributeList
			 (attributenames, attributevalueExists, attributevalues,
				attributes_strict);
  }
  memo3.Append(XMLDocObject.getXmlStrings().Text);
  memo3.Repaint;
  Application.ProcessMessages;
  XMLDocObject.destroy;
end;

procedure TForm1.addPackagesClick(Sender: TObject);
var XMLDocObject: TuibXMLDocument;
    k: integer;
begin
  Memo3.Clear;
  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
  XMLDocObject:= TuibXMLDocument.Create;
  // createXMLDoc
  if XMLDocObject.createXmlDocFromStringlist(memoToTStringlist(memo1)) then
    LogDatei.log('success: create xmldoc from stringlist',oslog.LLinfo)
  else
    LogDatei.log('failed: create xmldoc from stringlist',oslog.LLinfo);
  XMLDocObject.setlengthActNodeSet  (1);
  XMLDocObject.actnodeset[0] := XMLDocObject.getDocumentElement;
  for k:= 0 to length(XMLDocObject.actNodeSet)-1 do
    if XMLDocObject.actNodeSet[k] <> nil then
      LogDatei.log('actNodeSet <> nil',oslog.LLinfo)
    else
      LogDatei.log('actNodeSet = nil',oslog.LLinfo);
  XMLDocObject.makeNewDerivedNodeSet;
  XMLDocObject.logNodeSets;
  // add nodes
  if XMLDocObject.nodeExists('packages config:type="list"') then
    if XMLDocObject.openNode('packages config:type="list"') then
    begin
      XMLDocObject.makeNode('package','','');
      XMLDocObject.setNodeText('thunderbird');
    end;
  if XMLDocObject.nodeExists('packages config:type="list"') then
    if XMLDocObject.openNode('packages config:type="list"') then
    begin
      XMLDocObject.makeNode('package','','');
      XMLDocObject.setNodeText('firefox');
    end;
  if XMLDocObject.nodeExists('packages config:type="list"') then
    if XMLDocObject.openNode('packages config:type="list"') then
    begin
      XMLDocObject.makeNode('package','','');
      XMLDocObject.setNodeText('flowerpower');
    end;
  // TODO delete node if text is ... das funktioniert so noch nicht
  // select node name=package, text=snapper
  if XMLDocObject.nodeExists('packages config:type="list"') then
    if XMLDocObject.openNode('packages config:type="list"') then
    begin
      if XMLDocObject.setAktnodeIfText('snapper') then
      begin
        LogDatei.log('found: package snapper',oslog.LLinfo);
        XMLDocObject.delNode;
      end
      else
        LogDatei.log('not found: package snapper',oslog.LLinfo);
    end;
  // select node name=package, text=glibc
  if XMLDocObject.nodeExists('packages config:type="list"') then
    if XMLDocObject.openNode('packages config:type="list"') then
    begin
      if XMLDocObject.setAktnodeIfText('glibc') then
      begin
        LogDatei.log('found: package glibc',oslog.LLinfo);
        XMLDocObject.delNode;
      end
      else
        LogDatei.log('not found: package glibc',oslog.LLinfo);
    end;

  memo3.Append(XMLDocObject.getXmlStrings().Text);
  memo3.Repaint;
  Application.ProcessMessages;
  //XMLDocObject.writeXmlFile('newpackagesfile.xml',memoToTStringlist(memo3));
  XMLDocObject.destroy;
end;

procedure TForm1.configXMLClick(Sender: TObject);
var XMLDocObject: TuibXMLDocument;
    k: integer;
begin
  Memo3.Clear;
  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
  XMLDocObject:= TuibXMLDocument.Create;
  // createXMLDoc
  if XMLDocObject.createXmlDocFromStringlist(memoToTStringlist(memo1)) then
    LogDatei.log('success: create xmldoc from stringlist',oslog.LLinfo)
  else
    LogDatei.log('failed: create xmldoc from stringlist',oslog.LLinfo);
  XMLDocObject.setlengthActNodeSet  (1);
  XMLDocObject.actnodeset[0] := XMLDocObject.getDocumentElement;
  for k:= 0 to length(XMLDocObject.actNodeSet)-1 do
    if XMLDocObject.actNodeSet[k] <> nil then
      LogDatei.log('actNodeSet <> nil',oslog.LLinfo)
    else
      LogDatei.log('actNodeSet = nil',oslog.LLinfo);
  XMLDocObject.makeNewDerivedNodeSet;
  XMLDocObject.logNodeSets;
  // add packages
  if XMLDocObject.nodeExists('PIDKEY Value="XXXXXXXXXXXXXXXXXXXXXXXXX"') then
    if XMLDocObject.openNode('PIDKEY Value="XXXXXXXXXXXXXXXXXXXXXXXXX"') then
    begin
      XMLDocObject.setAttribute('Value','Value wird gesetzt');
    end;
  memo3.Append(XMLDocObject.getXmlStrings().Text);
  memo3.Repaint;
  Application.ProcessMessages;
  XMLDocObject.destroy;
end;




procedure TForm1.Memo1Change(Sender: TObject);
begin

end;

begin
  logdatei := Tloginfo.Create;
  StandardMainLogPath:='/tmp';
  StandardPartLogPath := '/tmp';
  StandardPartLogFilename := 'xmlpatch';
  logfilename:= 'xmlsectiontest.log';
  CreateTheLogfile(logfilename);
  logdatei.AktProduktId:='xmlpatch';
  LogDatei.log('start logging',oslog.LLinfo);
end.

