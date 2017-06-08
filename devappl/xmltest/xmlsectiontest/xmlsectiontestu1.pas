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
    if XMLDocObject.openNode('settings pass="windowsPE" // component name="Microsoft-Windows-Setup" // DiskConfiguration // Disk wcm:action="add" // ModifyPartitions', false) then
    begin
      XMLDocObject.setNodeText('***ModifyPartitions wurde ersetzt***');
      XMLDocObject.setAttribute('testname','testvalue');
    end;

  // Knoten löschen: DeleteElement
  // muss kein openNode gemacht werden, ist bei delNode implizit. Wenn der Knoten nicht gefunden wird, wird der zuletzt gefundene
  // übergeordnete Knoten gelöscht. Daher zuvor ein nodeExists!!
  if XMLDocObject.nodeExists('settings pass="windowsPE" // component name="Microsoft-Windows-Setup" // DiskConfiguration // Disk wcm:action="add" // WillWipeDisk') then
    XMLDocObject.delNode('settings pass="windowsPE" // component name="Microsoft-Windows-Setup" // DiskConfiguration // Disk wcm:action="add" // WillWipeDisk');

  // neuen Knoten setzen : am aktuellen Knoten wird ein Knoten angehängt, der neue Knoten wird aktueller Knoten
  if XMLDocObject.nodeExists('settings pass="windowsPE" // component name="Microsoft-Windows-Setup" // DiskConfiguration') then
    if XMLDocObject.openNode('settings pass="windowsPE" // component name="Microsoft-Windows-Setup" // DiskConfiguration', false) then
    begin
      XMLDocObject.makeNode('newnode','','');
      XMLDocObject.setNodeText('newnode Text setzen');
    end;

  // Attribute löschen und setzen :  DeleteAttribute, AddAttribute, SetAttribute
  if XMLDocObject.nodeExists('settings pass="windowsPE" // component name="Microsoft-Windows-Setup" // DiskConfiguration // Disk wcm:action="add"') then
    if XMLDocObject.openNode('settings pass="windowsPE" // component name="Microsoft-Windows-Setup" // DiskConfiguration // Disk wcm:action="add"', false) then
    begin

      XMLDocObject.delAttribute('wcm:action');
      // TODO add nur wenn key nicht existiert, nicht überschreiben!
      XMLDocObject.addAttribute('newwcm','newwcmAttribute');
      XMLDocObject.setAttribute('newAttribute','newValue');
    end;

  // TODO : Suche mit mehreren Attributen - openNode mit attributes_strict
  // TODO : Suche mit Attribut, Toleranz bei weiteren Attributen, die nicht auftauchen
  // TODO : addText
  // AddText "rtf" : sets the text only if there was no text node given

  // setText: neuen Text setzen. Jeglicher anderer Inhalt wird ersetzt, auch XML_Blätter. Kein XML-Fragment!
  if XMLDocObject.nodeExists ('settings pass="windowsPE" // component name="Microsoft-Windows-Setup" // UserData // ProductKey // WillShowUI') then
    if XMLDocObject.openNode('settings pass="windowsPE" // component name="Microsoft-Windows-Setup" // UserData // ProductKey // WillShowUI', false) then
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
  LogDatei.log('sollte leer sein, aktnode Attributvalue für wcm-nix: ' + XMLDocObject.getAttributeValue('wcm-nix'), LLinfo);  // liefert nichts zurück, da es das Attribut nicht gibt.
  LogDatei.log('sollte leer sein, aktnode Attributvalue für Leerstring: ' + XMLDocObject.getAttributeValue(''), LLinfo);  // liefert auch nichts zurück, da es das Attribut leer ist nicht gibt.  XMLDocObject.makeNode('neuerKnoten','mitAttribut','undValue');
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

  LogDatei.log('vor filter byChildElement, kein filtern ',oslog.LLinfo);
  XMLDocObject.filterByChildElement(true, 'packages');
  XMLDocObject.logNodeSets;
  LogDatei.log('nach filter byChildElement ',oslog.LLinfo);
  XMLDocObject.getNextGenerationActNodeSet;
  XMLDocObject.makeNewDerivedNodeSet;
  XMLDocObject.logNodeSets;

  // Anhängen von Knoten, wie???
  XMLDocObject.filterByChildElement(true, 'package');
  XMLDocObject.getNextGenerationActNodeSet;
  XMLDocObject.makeNewDerivedNodeSet;
  XMLDocObject.logNodeSets;
  // Löschen des Elements snapper
    // filterByText arbeitet auf actNodeSet
  if XMLDocObject.filterByText(true, 'snapper') then
    begin
      LogDatei.log('found: package snapper',oslog.LLwarning);
      //
    end
  else
    begin
      LogDatei.log('not found: package snapper',oslog.LLwarning);
    end;
  // Übernehmen des/der Knoten/s in actNodeSet ??
  XMLDocObject.logNodeSets;

  // snapper Knoten in actNodeSet
  XMLDocObject.getNextGenerationActNodeSet;
  XMLDocObject.makeNewDerivedNodeSet;
  XMLDocObject.logNodeSets;


  { anderer Weg über den nodePath und actNode
  // add nodes
  if XMLDocObject.nodeExists('packages config:type="list"') then
    if XMLDocObject.openNode('packages config:type="list"', false) then
    begin
      XMLDocObject.makeNode('package','','');
      XMLDocObject.setNodeText('thunderbird');
    end;
  if XMLDocObject.nodeExists('packages config:type="list"') then
    if XMLDocObject.openNode('packages config:type="list"', false) then
    begin
      XMLDocObject.makeNode('package','','');
      XMLDocObject.setNodeText('firefox');
    end;
  if XMLDocObject.nodeExists('packages config:type="list"') then
    if XMLDocObject.openNode('packages config:type="list"', false) then
    begin
      XMLDocObject.makeNode('package','','');
      XMLDocObject.setNodeText('flowerpower');
    end;

  // delete node if text is
  // select node name=package, text=snapper
  if XMLDocObject.nodeExists('packages config:type="list"') then
    if XMLDocObject.openNode('packages config:type="list"', false) then
    begin
      if XMLDocObject.setActnodeIfText('snapper') then
      begin
        LogDatei.log('found: package snapper',oslog.LLinfo);
        XMLDocObject.delNode;
      end
      else
        LogDatei.log('not found: package snapper',oslog.LLinfo);
    end;
  // select node name=package, text=glibc
  if XMLDocObject.nodeExists('packages config:type="list"') then
    if XMLDocObject.openNode('packages config:type="list"', false) then
    begin
      if XMLDocObject.setActnodeIfText('glibc') then
      begin
        LogDatei.log('found: package glibc',oslog.LLinfo);
        XMLDocObject.delNode;
      end
      else
        LogDatei.log('not found: package glibc',oslog.LLinfo);
    end;
  }
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

  {  funktionieren
  // config.xml --- <Display Level="basic" CompletionNotice="no" SuppressModal="no" AcceptEula="yes" />
  // test openNode and getNodeStrict
  if XMLDocObject.openNode('Display Level="basic" CompletionNotice="no" AcceptEula="yes" SuppressModal="no"', true) then
    LogDatei.log('node Display found',oslog.LLinfo)
  else
    LogDatei.log('Display not found',oslog.LLinfo);
  if XMLDocObject.openNode('Display Level="basic" CompletionNotice="no" AcceptEula="yes" SuppressModal="yes"', true) then
    LogDatei.log('node Display found',oslog.LLinfo)
  else
    LogDatei.log('Display not found',oslog.LLinfo);
  if XMLDocObject.openNode('Display CompletionNotice="no" AcceptEula="yes" SuppressModal="yes"', true) then
    LogDatei.log('node Display found',oslog.LLinfo)
  else
    LogDatei.log('Display not found',oslog.LLinfo);
  }
  { funktionieren
  // test openNode and getNode
  if XMLDocObject.openNode('Display Level="basic" CompletionNotice="no" AcceptEula="yes" SuppressModal="no"', false) then
    LogDatei.log('node Display found',oslog.LLinfo)
  else
    LogDatei.log('Display not found',oslog.LLinfo);
  if XMLDocObject.openNode('Display Level="basic" CompletionNotice="no" AcceptEula="yes" SuppressModal="yes"', false) then
    LogDatei.log('node Display found',oslog.LLinfo)
  else
    LogDatei.log('Display not found',oslog.LLinfo);
  if XMLDocObject.openNode('Display CompletionNotice="no" AcceptEula="yes" SuppressModal="yes"', false) then
    LogDatei.log('node Display found',oslog.LLinfo)
  else
    LogDatei.log('Display not found',oslog.LLinfo);
  }

  // if XMLDocObject.nodeExists('PIDKEY Value="XXXXXXXXXXXXXXXXXXXXXXXXX"') then TODO

  // <PIDKEY Value="XXXXXXXXXXXXXXXXXXXXXXXXX" />
  // test openNode and getNode
  if XMLDocObject.openNode('PIDKEY Value="XXXXXXXXXXXXXXXXXXXXXXXXX" test="test"', false) then
    begin
      XMLDocObject.setAttribute('Value','Value wird gesetzt');
    end;

  if XMLDocObject.openNode('PIDKEY Value="Value wird gesetzt"', false) then
    begin
      XMLDocObject.setAttribute('Value','Value wird gesetzt, 2');
    end;
  // test openNode and getNodeStrict
  if XMLDocObject.openNode('PIDKEY Value="Value wird gesetzt, 2"', true) then
    begin
      XMLDocObject.setAttribute('Value','Value wird gesetzt, 3');
    end;

  if XMLDocObject.openNode('PIDKEY Value="Value wird gesetzt, 3" test="test"', true) then
    begin
      XMLDocObject.setAttribute('Value','Value wird gesetzt, 4');
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
  logdatei.StandardPartLogFilename := 'xmlsectiontest';
  logfilename:= 'xmlsectiontest.log';
  logdatei.CreateTheLogfile(logfilename);
  logdatei.AktProduktId:='xmlsectiontest';
  LogDatei.log('start logging',oslog.LLinfo);
end.

