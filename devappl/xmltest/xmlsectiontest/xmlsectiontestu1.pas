unit xmlsectiontestu1;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons,  DOM, osxml, oslog, osxmltdom, osxmlsections;

type

  { TForm1 }

  TForm1 = class(TForm)
      Button1: TButton;
      Button2: TButton;
    Close: TBitBtn;
    memo3tomemo1: TButton;
    openFile: TButton;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    Memo3: TMemo;
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
var xmlnode, selnode:TDOMNode;
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
begin
   Memo1.Clear;
   OpenDialog1.Filter:='xml-file | *.xml';
   OpenDialog1.Title:='Vorhandene XML-Datei öffnen';
   if OpenDialog1.Execute then
   begin
      createXmlDocFromFile(OpenDialog1.FileName);
      Memo1.Append(getXMLDocAsTStringlist().Text);
   end
   else
     Memo1.Append (' no file ');
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
    mystringlist: TStringList;
    k:integer;
    ps : String;
    testnode:TDOMNode;
begin
  Memo3.Clear;
  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

  XMLDocObject:= TuibXMLDocument.Create;
  mystringlist:=TStringList.Create;
  // createXMLDoc
  if XMLDocObject.createXmlDocFromStringlist(memoToTStringlist(memo1)) then
    Memo3.Append('hat geklappt')
  else
    Memo3.Append('hat nicht geklappt');
  Memo3.Append(mystringlist.Text);

  XMLDocObject.openXmlFile('test.xml');

  XMLDocObject.setlengthActNodeSet  (1);
  XMLDocObject.actnodeset[0] := XMLDocObject.getDocumentElement;
  for k:= 0 to length(XMLDocObject.actNodeSet)-1 do
    begin
    if XMLDocObject.actNodeSet[k] <> nil then
      Memo3.Append('actNodeSet <> nil')
    else
      Memo3.Append('actNodeSet = nil');
    end;
  XMLDocObject.makeNewDerivedNodeSet;
  XMLDocObject.logNodeSets;
  //XMLDocObject.getNextGenerationActNodeSet;
  //XMLDocObject.logNodeSets;
  XMLDocObject.filterByChildElement (true, 'settings');
  XMLDocObject.logNodeSets;
  if XMLDocObject.nodeExists('settings pass="windowsPE" // component name="Microsoft-Windows-Setup" // DiskConfiguration // Disk wcm:action="add"') then
    begin Memo3.Append('Knoten gefunden'); Logdatei.log('Knoten gefunden',oslog.LLinfo); end
  else
    begin Memo3.Append('Knoten nicht gefunden'); Logdatei.log('Knoten nicht gefunden',oslog.LLinfo); end;

  if XMLDocObject.openNode('settings pass="windowsPE" // component name="Microsoft-Windows-Setup" // DiskConfiguration // Disk wcm:action="add" // ModifyPartitions') then
    begin Memo3.Append('aktnode gesetzt'); Logdatei.log('aktnode gesetzt',oslog.LLinfo); end
  else
    begin Memo3.Append('aktnode nicht gesetzt'); Logdatei.log('aktnode nicht gesetzt',oslog.LLinfo); end;
  XMLDocObject.setNodeText('***ModifyPartitions ersetzt***');
  XMLDocObject.setAttribute('testname','testvalue');
  if XMLDocObject.openNode('settings pass="windowsPE" // component name="Microsoft-Windows-Setup" // DiskConfiguration // Disk wcm:action="add"') then
    begin Memo3.Append('aktnode gesetzt'); Logdatei.log('aktnode gesetzt',oslog.LLinfo); end
  else
    begin Memo3.Append('aktnode nicht gesetzt'); Logdatei.log('aktnode nicht gesetzt',oslog.LLinfo); end;
  XMLDocObject.delAttribute('wcm:action');
  XMLDocObject.addAttribute('wcm','neues Attribut');
  Memo3.Append('aktnode Attributvalue für wcm: ' + XMLDocObject.getAttributeValue('wcm'));
  Memo3.Append('aktnode Attributvalue für wcm-nix: ' + XMLDocObject.getAttributeValue('wcm-nix'));  // liefert nichts zurück, da es das Attribut nicht gibt.
  Memo3.Append('aktnode Attributvalue für Leerstring: ' + XMLDocObject.getAttributeValue(''));  // liefert auch nichts zurück, da es das Attribut leer ist nicht gibt.  XMLDocObject.makeNode('neuerKnoten','mitAttribut','undValue');
  XMLDocObject.setNodeText('nodeText gesetzt');
  XMLDocObject.delNode('settings pass="windowsPE" // component name="Microsoft-Windows-Setup" // DiskConfiguration');
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

  //**************************************************************
  {
  if createXmlDocFromStringlist(memoToTStringlist(memo1)) then
    begin
      xmlnode:=getDocumentElement;
      if getChildnodeByNameAndAttributeKeyAndValue(xmlnode,'settings',
                                     'pass','windowsPE',selnode) then
        begin
          xmlnode:=selnode;
          if getChildnodeByNameAndAttributeKeyAndValue(xmlnode,'component',
                                  'name','Microsoft-Windows-Setup',selnode) then
            begin
              xmlnode:=selnode;
              if getUniqueChildNodeByName(xmlnode,'DiskConfiguration',selnode) then
                begin
                  xmlnode:=selnode;
                  if getUniqueChildNodeByName(xmlnode,'Disk',selnode) then
                    begin
                      xmlnode:=selnode;
                      if getUniqueChildNodeByName(xmlnode,'ModifyPartitions',selnode) then
                        begin
                          if setActualValueTDOMNode('***modify_partitions***') then
                            memo3.Append('***modify_partitions*** set')
                          else Memo3.Append('setActualValueTDOMNode ***modify_partitions*** failed')
                        end
                      else Memo3.Append('getUniqueChildNodeByName ModifyPartitions failed')
                    end
                  else Memo3.Append('getUniqueChildNodeByName Disk failed')
                end
              else Memo3.Append('getUniqueChildNodeByName DiskConfiguration failed');
            end
          else Memo3.Append('getChildnodeByNameAndAttributeKeyAndValue(component name Microsoft-Windows-Setup) failed')
        end
      else Memo3.Append('getChildnodeByNameAndAttributeKeyAndValue(settings pass windowsPE) failed');
      //**********************************************************************
      xmlnode:=getDocumentElement;
      if getChildnodeByNameAndAttributeKeyAndValue(xmlnode,'settings',
                                     'pass','windowsPE',selnode) then
        begin
          xmlnode:=selnode;
          if getChildnodeByNameAndAttributeKeyAndValue(xmlnode,'component',
                                  'name','Microsoft-Windows-Setup',selnode) then
            begin
              xmlnode:=selnode;
              if getUniqueChildNodeByName(xmlnode,'UserData',selnode)  then
              begin
                xmlnode:=selnode;
                if getUniqueChildNodeByName(xmlnode,'FullName',selnode)  then
                  if setActualValueTDOMNode('***fullname***') then
                        memo3.Append('***fullname*** set')
                  else memo3.Append('setActualValueTDOMNode FullName failed')
                else memo3.Append('getUniqueChildNodeByName UserData failed');
                if getUniqueChildNodeByName(xmlnode,'Organization',selnode) then
                  if setActualValueTDOMNode('***orgname***') then
                     memo3.Append('***orgname*** set')
                  else memo3.Append('can not set node Organization')
                else memo3.Append('Node Organization not found');
              end
            else memo3.Append('getUniqueChildNodeByName UserData failed');
            end
          else Memo3.Append('getChildnodeByNameAndAttributeKeyAndValue(component name Microsoft-Windows-Setup) failed')
        end
      else Memo3.Append('getChildnodeByNameAndAttributeKeyAndValue(settings pass windowsPE) failed');
      //**********************************************************************

    end
  else Memo3.Append('createXmlDocFromStringlist failed');
  }
  memo3.Append(XMLDocObject.getXmlStrings().Text);

  memo3.Repaint;
  Application.ProcessMessages;
  XMLDocObject.destroy;
end;



procedure TForm1.configXMLClick(Sender: TObject);

begin
  Memo3.Clear;
  // OpenNodeSet
  if createXmlDocFromStringlist(memoToTStringlist(memo1)) then
  begin
    // documentroot
    // allchildelemets with elementname: "PIKEY"
     xmlnode:=getDocumentElement;
     if getUniqueChildNodeByName(xmlnode, 'PIDKEY',selnode) then
       if setChildAttributeValueTDOMNode(selnode, 'Value', 'NewValue') then
         // SetAttribute "Value="XXXXXXXXXXXXXXXXXXXXXXXXX" value="NewValue"
         memo3.Append(getXMLDocAsTStringlist.Text)
       else Memo3.Append('setChildAttributeValueTDOMNode failed')
     else Memo3.Append('selectAllNodesByName failed')
  end
  else
    Memo3.Append('createXmlDocFromStringlist failed');
  memo3.Repaint;
  Application.ProcessMessages;
  freeXmlDoc();
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

