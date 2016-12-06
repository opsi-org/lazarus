unit xmltestu1;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ComCtrls, osxml, DOM, XMLRead, XMLWrite;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    sles12sp1XML: TButton;
    configXML: TButton;
    nodes: TButton;
    count: TButton;
    memo3tomemo1: TButton;
    attributes: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    procedure BitBtn1Click(Sender: TObject);
    procedure configXMLClick(Sender: TObject);
    procedure sles12sp1XMLClick(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure nodesClick(Sender: TObject);
    procedure countClick(Sender: TObject);
    procedure memo3tomemo1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Memo2Change(Sender: TObject);
    procedure attributesClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

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

procedure TForm1.nodesClick(Sender: TObject);
// add as last node
var
  valstr: string;
  childnode, docelemstrlist, docStringlist, newnodestringlist : TStringList;
  intI: integer;
begin
  // diverse Tests, Haltepunkte an stringlist.Clear setzen
  //
  valstr:='';
  docStringlist:=  TStringList.Create;
  docelemstrlist:= TStringList.Create;
  newnodestringlist:= TStringList.Create;
  childnode:= TStringList.Create;
  newnodestringlist.Append('<huhu>lilalala</huhu>');
  if appendXmlNodeToDocFromStringlist(memoToTStringlist(memo1),newnodestringlist,docStringlist) then
    begin
      memo3.Clear;
      memo3.Append(docStringlist.Text);
      memo3.Repaint;
      Application.ProcessMessages;
    end;
  if appendXmlNodeToDocFromStringlist(memoToTStringlist(memo3),memoToTStringlist(memo2),docstringlist) then
    begin
      memo3.Clear;
      memo3.Append(docstringList.Text);
      memo3.Repaint;
      Application.ProcessMessages;
    end;
  newnodestringlist.clear;
  newnodestringlist.Append('<xx>blabla</xx>');
  if (appendXmlNodeToDocFromStringlist(memoToTStringlist(memo3),newnodestringlist, docStringlist)) then
    begin
      memo3.Clear;
      memo3.Append(docStringlist.Text);
      memo3.Repaint;
      Application.ProcessMessages;
    end;
  newnodestringlist.Clear;
  // neuen knoten auf Memo1
  memo1.clear;
  memo1.Append(docstringlist.text);
  memo1.Repaint;
  Application.ProcessMessages;

  // hier erstmal die Position bestimmen. docstringlist ist das komplette xmldokument
  memo3.Clear;
  memo3.append('doc node name: ' + getDocNodeName(docStringlist));
  memo3.append('doc node type: ' + getDocNodeType(docStringlist));
  memo3.Append('getDocumentElement');
  docelemstrlist:= getDocumentElementAsStringlist(docstringlist);
  memo3.Append(docelemstrlist.Text);
  memo3.Repaint;
  Application.ProcessMessages;
  memo3.Clear;
  memo3.Append('Childnodes');
  if xmlAsStringlistGetChildNodes(docelemstrlist,newnodestringlist) then
    begin
      for intI := 0 to newnodestringlist.count -1 do
        // TODO: wo kommen die Breaks her????
        memo3.Append(StringReplace(newnodestringlist[intI], #10, '', [rfReplaceAll]));
    end
  else
    memo3.Append('keine child nodes');
  memo3.Repaint;
  Application.ProcessMessages;
  // get node by name
  memo3.Append('');
  if xmlAsStringlistGetChildnodeByName(getDocumentElementAsStringlist(docstringlist),
                                  'xx', childnode) then
    begin
      memo3.Append('get node xx from stringlist: ' + childnode.Text);
    end
  else
    begin
      memo3.Append('no node xx found: ' + docstringlist.Text);
  end;
  memo3.Repaint;
  Application.ProcessMessages;

    // sles
  if xmlAsStringlistGetChildnodeByName(docelemstrlist,
                                  'software', childnode) then
     //if xmlAsStringlistSetNodevalue(childnode,'ohoho') then
       memo3.append('node software gefunden')
  else memo3.append('node software nicht gefunden');
  memo3.Repaint;
  Application.ProcessMessages;
  memo3.clear;
  // get node value by index
  memo3.Clear;
  if xmlAsStringlistGetChildnodeValueByIndex(docstringlist, 2, valstr) then
    memo3.Append('value at index 2: ' + valstr)
  else
    memo3.Append('no value at index 2 found');
  memo3.Repaint;
  Application.ProcessMessages;
  // set node value by index
  memo3.clear;
  if xmlAsStringlistSetChildnodeValueByIndex(docstringlist, 2, 'ahahahaha') then
    memo3.Append(docstringlist.text)
  else
    memo3.Append('error setting Value');
  memo3.Repaint;
  Application.ProcessMessages;
  // get node and set value
  if xmlAsStringlistGetChildnodeByName(docelemstrlist,
                                  'xx', childnode) then      // da tut was nicht
     if xmlAsStringlistSetNodevalue(childnode,'ohoho') then
       memo3.append(docelemstrlist.Text);
  memo3.Repaint;
  Application.ProcessMessages;
  memo3.clear;


  // get node by index
  memo3.clear;
  childnode.clear;
  if xmlAsStringlistGetChildnodeByIndex(docstringlist,1,childnode) then
     memo3.Append(childnode.text)
  else
  memo3.Append('error getting node by index');
  memo3.Repaint;
  Application.ProcessMessages;
  // get node by name and attribut value
  // delete node by index
  memo3.Clear;
  if xmlAsStringlistDeleteChildnodeByIndex(docstringlist,0) then
   begin
     memo3.Append('docstring nach Löschen node mit index 0: ' + docstringlist.Text);
   end
 else
   begin
     memo3.Append('Löschen hat nicht geklappt!');
   end;
 memo3.Repaint;
 Application.ProcessMessages;

end;

procedure TForm1.countClick(Sender: TObject);
var docelemstrlist: TStringlist;
begin
  memo3.Clear;
  docelemstrlist := TStringlist.Create;
  // den document node herausholen
  docelemstrlist:= getDocumentElementAsStringlist(memoToTStringlist(memo1));
  if xmlAsStringlistHasChildNodes( docelemstrlist) then
   begin
      memo3.Append('elements, also comments: '
                    + inttostr(xmlAsStringlistAllElementsCount(docelemstrlist)));
      memo3.Append('elements, no comments: '
                    + inttostr(xmlAsStringlistElementsCount(docelemstrlist)));
      memo3.Append('elements, only comments: '
                    + inttostr(xmlAsStringlistCommentElementsCount(docelemstrlist)));
    end
  else memo3.Append('has no child nodes');
  memo3.Repaint;
  Application.ProcessMessages;
end;


procedure TForm1.attributesClick(Sender: TObject);
var
  attributevalue: String;
  index:integer;
  childnode, newnodestringlist, attributevaluelist, docelemstrlist: TStringlist;
begin
  memo3.clear;
  docelemstrlist := TStringlist.Create;
  attributevaluelist  := TStringlist.Create;
  newnodestringlist := TStringlist.Create;
  childnode:= TStringlist.Create;
  docelemstrlist:= getDocumentElementAsStringlist(memoToTStringlist(memo1));
  if xmlAsStringlistGetChildAttributeValueByNameAndKey(docelemstrlist,
            'username','Level',attributevalue) then
     begin
       memo3.Append('attibute value: ' + attributevalue);
     end
  else
    memo3.Append('attibute value not found for username Level ');
  memo3.Repaint;
  Application.ProcessMessages;
  if xmlAsStringlistSetChildAttributeValueByNameAndKey(docelemstrlist,
             'username','Level','neuer Value') then
    memo3.Append(docelemstrlist.Text)
  else
    memo3.Append('attibute value not set for username Level ');
  memo3.Repaint;
  Application.ProcessMessages;
  // node
  if xmlAsStringlistgetAttributesValueList(docelemstrlist,attributevaluelist) then
    memo3.Append(attributevaluelist.Text)
  else
    memo3.Append('no attibute values');
  memo3.Repaint;
  Application.ProcessMessages;
  // node     -- hier weiter Attribute auslesen
  if xmlAsStringlistGetChildNodes(docelemstrlist,newnodestringlist) then
    // folgendes nimmt nur den ersten knoten
    if xmlAsStringlistgetAttributesValueList(newnodestringlist,attributevaluelist) then
      memo3.Append(attributevaluelist.Text)
    else
      memo3.Append('no attibute values');
  memo3.Repaint;
  Application.ProcessMessages;
  // attribute keylist
  if xmlAsStringlistGetNodeAttributeKeys(newnodestringlist,attributevaluelist) then
    memo3.Append(attributevaluelist.Text)
  else
     memo3.Append('no attibute values');
  memo3.Repaint;
  Application.ProcessMessages;
  // delete by key  im docelement
  memo3.clear;
  if xmlAsStringlistdeleteChildAttributeByKey(docelemstrlist, 'lalala') then
    memo3.Append(docelemstrlist.Text)
  else
    memo3.Append('can not delete attibute lalala');
  memo3.Repaint;
  Application.ProcessMessages;
  memo3.clear;
  // hole knoten mit namen PIDKEY, schau ob Value XXXXX, setze Value teststring
  // wie sieht dann das docelement aus?
  if xmlAsStringlistGetChildnodeByName(docelemstrlist,
                                  'PIDKEY', childnode) then
     begin
       memo3.append('childnode: ' + childnode.Text);
       if xmlAsStringlistgetAttributesValueList(childnode,attributevaluelist) then
         begin
           // testen ob attributevaluelist einen bestimmten Wert enthält
           if attributevaluelist.IndexOf('XXXXXXXXXXXXXXXXXXXXXXXXX')>-1 then
           begin
             memo3.Append('attrlist: ' +  attributevaluelist.Text);
             if xmlAsStringlistSetChildAttributeValueByNameAndKey(docelemstrlist,
               'PIDKEY','Value','teststring') then
               memo3.Append(docelemstrlist.Text)
             else
               memo3.Append('setattribute failed')
           end else memo3.append('attr value not found')
         end
       else
         begin
           memo3.Append('getAttributevaluelist failed');
           memo3.Append(childnode.text);
           memo3.Append('attr: ' + attributevaluelist.text);
         end
      end
  else memo3.Append('getchildnodebyname failed');
  memo3.Repaint;
  Application.ProcessMessages;
end;


procedure TForm1.memo3tomemo1Click(Sender: TObject);
begin
   Memo1.Clear;
   Memo1.Append(Memo3.text);
end;



procedure TForm1.FormCreate(Sender: TObject);
begin

end;

procedure TForm1.Memo2Change(Sender: TObject);
begin

end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TForm1.configXMLClick(Sender: TObject);
var
  attributevalue: String;
  //index:integer;
  childnodeSL, attributevalueSL, docelemSL: TStringlist;
begin
  memo3.clear;
  docelemSL := TStringlist.Create;
  attributevalueSL  := TStringlist.Create;
  childnodeSL:= TStringlist.Create;
  docelemSL:= getDocumentElementAsStringlist(memoToTStringlist(memo1));
  // hole knoten mit namen PIDKEY, schau ob Value XXXXX, setze Value teststring
  // wie sieht dann das docelement aus?
  if xmlAsStringlistGetChildnodeByName(docelemSL,
                                  'PIDKEY', childnodeSL) then
     begin
       memo3.append('childnode: ' + childnodeSL.Text);
       if xmlAsStringlistgetAttributesValueList(childnodeSL,attributevalueSL) then
         begin
           // testen ob attributevaluelist einen bestimmten Wert enthält
           if attributevalueSL.IndexOf('XXXXXXXXXXXXXXXXXXXXXXXXX')>-1 then
           begin
             memo3.Append('attrlist: ' +  attributevalueSL.Text);
             if xmlAsStringlistSetChildAttributeValueByNameAndKey(docelemSL,
               'PIDKEY','Value','teststring') then
               memo3.Append(docelemSL.Text)
             else
               memo3.Append('setattribute failed')
           end else memo3.append('attr value not found')
         end
       else
         begin
           memo3.Append('getAttributevaluelist failed');
           memo3.Append(childnodeSL.text);
           memo3.Append('attr: ' + attributevalueSL.text);
         end
      end
  else memo3.Append('getchildnodebyname failed');
  // wieder in ordentliches XML umwandeln
  memo3.Repaint;
  Application.ProcessMessages;
end;
procedure TForm1.sles12sp1XMLClick(Sender: TObject);
var
  attributevalue: String;
  //index:integer;
  childnodeSL, childnodeSL2, attributevalueSL, docelemSL: TStringlist;
begin
  memo3.clear;
  docelemSL := TStringlist.Create;
  attributevalueSL  := TStringlist.Create;
  childnodeSL:= TStringlist.Create;
  childnodeSL2:= TStringlist.Create;
  docelemSL:= getDocumentElementAsStringlist(memoToTStringlist(memo1));
  if xmlAsStringlistGetChildnodeByName(docelemSL,
                                  'software', childnodeSL) then
     if xmlAsStringlistGetChildnodeByName(childnodeSL,
                                  'packages', childnodeSL2) then
       if appendXmlNodeToNodeFromStringlist( childnodeSL2, textToTStringList('<package>fitzlibutzli</package>')) then
       begin
         memo3.Append('angehängter Node fitzlibutzli an packages');
         memo3.Append(childnodeSL2.Text);
         memo3.Repaint;
         Application.ProcessMessages;
         memo3.clear;
         memo3.Append('Node software');
         if xmlAsStringlistSetChildnodeByName(childnodeSL,'packages',childnodeSL2) then
           memo3.Append(childnodeSL.Text);
       end;
  if xmlAsStringlistSetChildnodeByName(docelemSL,'software',childnodeSL) then
     begin
        memo3.clear;
        memo3.Append(docelemSL.Text);
        memo3.Repaint;
        Application.ProcessMessages;
     end;
end;
procedure TForm1.Memo1Change(Sender: TObject);
begin

end;



end.

