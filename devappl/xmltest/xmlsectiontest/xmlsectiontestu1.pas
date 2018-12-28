unit xmlsectiontestu1;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,
  Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons,
  oslog, osxmlsections;

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
  writexmlfilename: String;

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
      begin
        Memo1.Append(XMLDocObject.getXmlStrings().Text);
        writexmlfilename:=OpenDialog1.FileName;
      end
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
// getestet mit unattend_win10_x64.xml
var XMLDocObject: TuibXMLDocument;
    k:integer;
begin
  Memo3.Clear;
  //LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
  XMLDocObject:= TuibXMLDocument.Create;
  // createXMLDoc
  if XMLDocObject.createXmlDocFromStringlist(memoToTStringlist(memo1)) then
    LogDatei.log('success: create xmldoc from stringlist',oslog.LLinfo)
  else
    LogDatei.log('failed: create xmldoc from stringlist',oslog.LLinfo);
  XMLDocObject.makeTopAsActNodeSet;
  XMLDocObject.makeNewDerivedNodeSet;
  XMLDocObject.logNodeSets;

  if XMLDocObject.makeNodePathWithTextContent('settings pass="windowsPE" // component language="neutral" xmlns:wcm="http://schemas.microsoft.com/WMIConfig/2002/State" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" versionScope="nonSxS" publicKeyToken="31bf3856ad364e35" processorArchitecture="amd64" // UserDataXY // ProductKeyXY // WillShowUIXY', 'Hallo Text') then
      LogDatei.log('success: makeNodePathWithTextContent',oslog.LLinfo)
  else
      LogDatei.log('failed: makeNodePathWithTextContent',oslog.LLinfo);

{
  //XMLDocObject.filterByChildElement (true, 'settings');
  //XMLDocObject.logNodeSets;
  // Nodetext setzen und Attribut setzen :   SetText, SetAttribute
  if XMLDocObject.nodeExists('settings pass="windowsPE" // component name="Microsoft-Windows-Setup" // DiskConfiguration // Disk wcm:action="add"') then
    if XMLDocObject.openNode('settings pass="windowsPE" // component name="Microsoft-Windows-Setup" language="neutral" xmlns:wcm="http://schemas.microsoft.com/WMIConfig/2002/State" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" versionScope="nonSxS" publicKeyToken="31bf3856ad364e35" processorArchitecture="amd64" // DiskConfiguration // Disk wcm:action="add" // ModifyPartitions', true) then
    begin
      XMLDocObject.setNodeTextActNode('***ModifyPartitions wurde ersetzt***');
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
      XMLDocObject.setNodeTextActNode('newnode Text setzen');
    end;

  // Attribute löschen und setzen :  DeleteAttribute, AddAttribute, SetAttribute
  {
  if XMLDocObject.nodeExists('settings pass="windowsPE" // component name="Microsoft-Windows-Setup" // DiskConfiguration // Disk wcm:action="add"') then
    if XMLDocObject.openNode('settings pass="windowsPE" // component name="Microsoft-Windows-Setup" // DiskConfiguration // Disk wcm:action="add"', false) then
    begin

      XMLDocObject.delAttribute('wcm:action');
      // TODO add nur wenn key nicht existiert, nicht überschreiben!
      XMLDocObject.addAttribute('newwcm','newwcmAttribute');
      XMLDocObject.setAttribute('newAttribute','newValue');
    end;
  }
  // TODO : Suche mit mehreren Attributen - openNode mit attributes_strict
  // TODO : Suche mit Attribut, Toleranz bei weiteren Attributen, die nicht auftauchen
  // TODO : addText
  // AddText "rtf" : sets the text only if there was no text node given
  {
  // setText: neuen Text setzen. Jeglicher anderer Inhalt wird ersetzt, auch XML_Blätter. Kein XML-Fragment!
  if XMLDocObject.nodeExists ('settings pass="windowsPE" // component name="Microsoft-Windows-Setup" // UserData // ProductKey // WillShowUI') then
    if XMLDocObject.openNode('settings pass="windowsPE" // component name="Microsoft-Windows-Setup" // UserData // ProductKey // WillShowUI', false) then
        XMLDocObject.setNodeTextActNode('nodeText wurde gesetzt');
  }
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



  XMLDocObject.logNodeSets;

  memo3.Append(XMLDocObject.getXmlStrings().Text);
  memo3.Repaint;
  Application.ProcessMessages;
  XMLDocObject.destroy;
end;

procedure TForm1.addPackagesClick(Sender: TObject);
var XMLDocObject: TuibXMLDocument;
    textArray: TStringList;
    cdataString : String;

// Testdatei opensuse_software_final_test.xml
// 1) Suche Knoten software, subknoten packages, hänge subsubknoten package 1,2,3
// 2) Suche Knoten software, hänge substuktur  <patterns config:type="list">
//      <pattern>yast2_basis</pattern> ... ein
begin
  Memo3.Clear;
  //LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
  XMLDocObject:= TuibXMLDocument.Create;
  // createXMLDoc
  if XMLDocObject.createXmlDocFromStringlist(memoToTStringlist(memo1)) then
    LogDatei.log('success: create xmldoc from stringlist',oslog.LLinfo)
  else
    LogDatei.log('failed: create xmldoc from stringlist',oslog.LLinfo);


  // oberstes Nodeset als actNodeSet setzen
  XMLDocObject.makeTopAsActNodeSet;
  XMLDocObject.makeNewDerivedNodeSet;
  XMLDocObject.logNodeSets;

  // An dieser Stelle sind lediglich die Nodesets bekannt, actNode wäre Nil
  XMLDocObject.setTopNodeAsActNode;
  LogDatei.log('actnode name is: ' + XMLDocObject.getNodeNameActNode + ' -> text ' +
                          XMLDocObject.getNodeTextActNode ,oslog.LLinfo);

  // mach das derivedNodeset zum aktnodeset und mach neue derivedNodeSets
  XMLDocObject.getNextGenerationActNodeSet;
  XMLDocObject.makeNewDerivedNodeSet;
  // diese Nodes sind alle nächsten Childs von allen Knoten in actNodeSet
  // wozu soll das gut sein?
  XMLDocObject.logNodeSets;

  // Anlegen von Knoten, filtern des actNodesets
  LogDatei.log('vor filterByChildElement, kein filtern ',oslog.LLinfo);
  XMLDocObject.filterByChildElement('software');
  XMLDocObject.getNextGenerationActNodeSet;
  LogDatei.log('nach filterByChildElement software',oslog.LLinfo);
  LogDatei.log('actnode is: ' + XMLDocObject.getNodeNameActNode + ' -> text ' +
                          XMLDocObject.getNodeTextActNode ,oslog.LLinfo);
  XMLDocObject.logNodeSets;

  XMLDocObject.filterByChildElement('packages');
  XMLDocObject.getNextGenerationActNodeSet;
  LogDatei.log('nach filter byChildElement packages',oslog.LLinfo);
  LogDatei.log('actnode is: ' + XMLDocObject.getNodeNameActNode + ' -> text ' +
                          XMLDocObject.getNodeTextActNode ,oslog.LLinfo);
  XMLDocObject.logNodeSets;

  // create Node from TStringList and test if valid
  // TODO: Baustelle
  {
  textArray := TStringList.Create;
  textArray.Add('<package>thunderbird</package>');
  if XMLDocObject.isValidXML(textArray) then
    LogDatei.log('valid XML' ,oslog.LLinfo)
  else
    LogDatei.log(textArray.ToString + ' : not valid XML',oslog.LLerror) ;
  }

  // add node
  XMLDocObject.makeNode('package','','');
  XMLDocObject.setNodeTextActNode('thunderbird');
  XMLDocObject.makeNewDerivedNodeSet;
  XMLDocObject.logNodeSets;
  LogDatei.log('actnode is: ' + XMLDocObject.getNodeNameActNode + ' -> text ' +
                          XMLDocObject.getNodeTextActNode ,oslog.LLinfo);

  // nach dem Anhängen des neuen Knotens ist actnode der neue Knoten (package thunderbird)

  // nochmal den Parent-Knoten aus actNodeSet holen
  XMLDocObject.setParentNodeAsActNode();
  LogDatei.log('actnode is: ' + XMLDocObject.getNodeNameActNode + ' -> text ' +
                          XMLDocObject.getNodeTextActNode ,oslog.LLinfo);
  // Knoten pillepalle anhängen
  XMLDocObject.makeNode('package','','');
  XMLDocObject.setNodeTextActNode('pillepalle');
  XMLDocObject.makeNewDerivedNodeSet;
  XMLDocObject.logNodeSets;
  LogDatei.log('actnode is: ' + XMLDocObject.getNodeNameActNode + ' -> text ' +
                          XMLDocObject.getNodeTextActNode ,oslog.LLinfo);

  // wieder im Knoten Packages einen Knotenanlegen, Knoten packages als actnode beibehalten
  // diesmal gleich mit allen Parametern
  XMLDocObject.setParentNodeAsActNode();
  XMLDocObject.makeNodeAndKeepActNode('package','name1','attribut1','palimpalim');
  XMLDocObject.makeNewDerivedNodeSet;
  XMLDocObject.logNodeSets;
  LogDatei.log('actnode is: ' + XMLDocObject.getNodeNameActNode + ' -> text ' +
                          XMLDocObject.getNodeTextActNode ,oslog.LLinfo);

  //  eine Liste von Knoten anlegen, Knoten packages als actNode beibehalten
  textArray := TStringList.Create;
  textArray.Add('alpha');
  textArray.Add('beta');
  textArray.Add('gamma');
  textArray.Add('delta');
  textArray.Add('epsilon');
  if XMLDocObject.makeNodes('package',textArray) then
  // makeNodes: Namen der Knoten (identisch), im Array die Texte der Knoten
  begin
    LogDatei.log('actnode is: ' + XMLDocObject.getNodeNameActNode + ' -> text ' +
                          XMLDocObject.getNodeTextActNode ,oslog.LLinfo);
  end
  else
    LogDatei.log('irgendwas ist falsch gelaufen ' ,oslog.LLerror);
  textArray.Clear;

  // anderer Weg für das Anlegen von Knoten
  // über den nodeExisits und openNode mit dem nodePath,
  // danach an actNode makeNode und setzen des Textes
  // hier openNode strict=false - stimmt aber trotzdem
  if XMLDocObject.nodeExists('software // packages config:type="list"') then
    if XMLDocObject.openNode('software // packages config:type="list"', false) then
    begin
      XMLDocObject.makeNode('package','','');
      XMLDocObject.setNodeTextActNode('thunderbird');
    end;
  // hier openNode strict=true
  if XMLDocObject.nodeExists('software // packages config:type="list"') then
    if XMLDocObject.openNode('software // packages config:type="list"', true) then
    begin
      XMLDocObject.makeNode('package','','');
      XMLDocObject.setNodeTextActNode('firefox');
    end;
  // hier openNode strict=false - geht auch
  if XMLDocObject.nodeExists('software // packages') then
    if XMLDocObject.openNode('software // packages', false) then
    begin
      XMLDocObject.makeNode('package','','');
      XMLDocObject.setNodeTextActNode('flowerpower');
    end;

  // nochmal auf Anfang und die Knoten auslesen
  XMLDocObject.makeTopAsActNodeSet;
  XMLDocObject.makeNewDerivedNodeSet;
  XMLDocObject.logNodeSets;

  XMLDocObject.filterByChildElement('software');
  XMLDocObject.getNextGenerationActNodeSet;
  XMLDocObject.makeNewDerivedNodeSet;
  XMLDocObject.logNodeSets;

  XMLDocObject.filterByChildElement('packages');
  XMLDocObject.getNextGenerationActNodeSet;
  XMLDocObject.makeNewDerivedNodeSet;
  XMLDocObject.logNodeSets;



  // Löschen des Elements package mit TextContent snapper
  // delete node if text is
  // select node name=package, text=snapper
  if XMLDocObject.nodeExists('software // packages config:type="list"') then
    if XMLDocObject.openNode('software // packages config:type="list"', false) then
    begin
       LogDatei.log('actnode is: ' + XMLDocObject.getNodeNameActNode + ' -> text_content is: ' +
                          XMLDocObject.getNodeTextActNode ,oslog.LLinfo);
      if XMLDocObject.setActNodeIfText('snapper') then
      begin
        LogDatei.log('actnode is: ' + XMLDocObject.getNodeNameActNode + ', text_content is:' +
                          XMLDocObject.getNodeTextActNode ,oslog.LLinfo);
        XMLDocObject.delNode;
        LogDatei.log('actnode is: ' + XMLDocObject.getNodeNameActNode,oslog.LLinfo);
      end
      else
        LogDatei.log('not found: package snapper',oslog.LLinfo);
    end;


  // andere Variante des Löschens ohne Angabe des Text_Content, löscht das erste Element
  XMLDocObject.delNode('software // packages config:type="list" // package');
  LogDatei.log('actnode is: ' + XMLDocObject.getNodeNameActNode,oslog.LLinfo);

  // Wie sieht der Zweig packages jetzt aus?
  XMLDocObject.makeNewDerivedNodeSet;
  XMLDocObject.logNodeSets;

  // einsetzen eines CDATA-Strings
  cdatastring := #$3C + '![CDATA['#13#10'#! /bin/sh'#13#10'set -x\nexport PATH=/sbin:/usr/sbin:/usr/local/sbin:/root/bin:/usr/local/bin:/usr/bin:/bin:/usr/games'#13#10'/usr/bin/zypper lr'#13#10'rm /etc/zypp/repos.d/SLES*.repo'#13#10'/usr/bin/zypper lr'#13#10'/usr/bin/zypper rr 1'#13#10']]'#$3e;
  if XMLDocObject.nodeExists('deploy_image') then
    if XMLDocObject.openNode('deploy_image', false) then
    begin
      XMLDocObject.makeNode('source','','');
      XMLDocObject.setNodeTextActNode(cdatastring);
    end;

  memo3.Append(XMLDocObject.getXmlStrings().Text);
  memo3.Repaint;
  Application.ProcessMessages;
  XMLDocObject.writeXMLAndCloseFile(writexmlfilename + '.new');
  XMLDocObject.destroy;
end;

procedure TForm1.configXMLClick(Sender: TObject);
var XMLDocObject: TuibXMLDocument;
    k: integer;
begin
  Memo3.Clear;
  //LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
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

  XMLDocObject.setlengthActNodeSet  (1);
  XMLDocObject.actnodeset[0] := XMLDocObject.getDocumentElement;
  for k:= 0 to length(XMLDocObject.actNodeSet)-1 do
    if XMLDocObject.actNodeSet[k] <> nil then
      LogDatei.log('actNodeSet <> nil',oslog.LLinfo)
    else
      LogDatei.log('actNodeSet = nil',oslog.LLinfo);
  XMLDocObject.makeNewDerivedNodeSet;
  XMLDocObject.logNodeSets;

  // filtern nach Attribut key/value
  //XMLDocObject.filterByAttribute_existing ('Level', '"basic"');

  // filtern nach Attribut key
  XMLDocObject.filterByAttributeName_existing ('Level');

  XMLDocObject.getNextGenerationActNodeSet;
  XMLDocObject.makeNewDerivedNodeSet;
  XMLDocObject.logNodeSets;

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
  LogDatei.log('start logging',oslog.LLinfo)  ;
end.

