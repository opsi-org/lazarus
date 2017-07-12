unit xmltestu1;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ComCtrls, osxml, osxmltdom;

type

  { TForm1 }

  TForm1 = class(TForm)
    Close: TBitBtn;
    unatt_win10: TButton;
    sles12sp1XML: TButton;
    configXML: TButton;
    nodes: TButton;
    count: TButton;
    openFile: TButton;
    attributes: TButton;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    Memo2: TMemo;
    Memo3: TMemo;
    procedure CloseClick(Sender: TObject);
    procedure configXMLClick(Sender: TObject);
    procedure sles12sp1XMLClick(Sender: TObject);
    procedure unatt_win10Click(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure nodesClick(Sender: TObject);
    procedure countClick(Sender: TObject);
    procedure openFileClick(Sender: TObject);
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
  memo3.append('doc node name: ' + osxml.getDocNodeNameFromStringList(docStringlist));
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
  if xmlAsStringlistGetUniqueChildnodeByName(getDocumentElementAsStringlist(docstringlist),
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
  if xmlAsStringlistGetUniqueChildnodeByName(docelemstrlist,
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
  if xmlAsStringlistGetUniqueChildnodeByName(docelemstrlist,
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
  if xmlAsStringlistGetUniqueChildnodeByName(docelemstrlist,
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
  else memo3.Append('getchildnodebyname PIDKEY failed');
  memo3.Repaint;
  Application.ProcessMessages;
end;


procedure TForm1.openFileClick(Sender: TObject);
begin
   Memo1.Clear;
   OpenDialog1.Filter:='xml-file | *.xml';
   OpenDialog1.Title:='Vorhandene package.xml öffnen';
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
  Application.Terminate;
end;

procedure TForm1.configXMLClick(Sender: TObject);
var
  childnodeSL, attributevalueSL, docelemSL: TStringlist;
  header : string;
begin
  memo3.clear;
  docelemSL := TStringlist.Create;
  attributevalueSL  := TStringlist.Create;
  childnodeSL:= TStringlist.Create;
  header :=  getXmlDeclarationFromStringList(memoToTStringlist(memo1)).Text;
  memo3.append('header: ' +  header);
  docelemSL:= getDocumentElementAsStringlist(memoToTStringlist(memo1));
  // hole knoten mit namen PIDKEY, schau ob Value XXXXX, setze Value teststring
  // wie sieht dann das docelement aus?
  if xmlAsStringlistGetUniqueChildnodeByName(docelemSL,
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
  childnodeSL, childnodeSL2, docelemSL: TStringlist;
begin
  memo3.clear;
  docelemSL := TStringlist.Create;
  childnodeSL:= TStringlist.Create;
  childnodeSL2:= TStringlist.Create;
  docelemSL:= getDocumentElementAsStringlist(memoToTStringlist(memo1));
  if xmlAsStringlistGetUniqueChildnodeByName(docelemSL,
                                  'software', childnodeSL) then
     if xmlAsStringlistGetUniqueChildnodeByName(childnodeSL,
                                  'packages', childnodeSL2) then
       if appendXmlNodeToNodeFromStringlist( childnodeSL2, textToTStringList('<package>fitzlibutzli</package>')) then
       begin
         memo3.Append('angehängter Node fitzlibutzli an packages');
         memo3.Append(childnodeSL2.Text);
         memo3.Repaint;
         Application.ProcessMessages;
         memo3.clear;
         memo3.Append('Node software');
         // zum Erzeugen der Knoten wird das XMLDoc XML in osxmltdom gebraucht
         createXmlDocFromStringlist(memoToTStringlist(memo1));
         if xmlAsStringlistReplaceUniqueChildnodeByName(childnodeSL,'packages',childnodeSL2) then
           memo3.Append(childnodeSL.Text);
         freeXmlDoc();
       end;
  // zum Erzeugen der Knoten wird das XMLDoc XML in osxmltdom gebraucht
  createXmlDocFromStringlist(memoToTStringlist(memo1));
  if xmlAsStringlistReplaceUniqueChildnodeByName(docelemSL,'software',childnodeSL) then
     begin
        memo3.clear;
        memo3.Append(docelemSL.Text);
        memo3.Repaint;
        Application.ProcessMessages;
     end;
  freeXmlDoc();
end;

procedure TForm1.unatt_win10Click(Sender: TObject);
var childnodeSL, childnodeSL2, childnodeSL3, childnodeSL4, childnodeSL5, docelemSL: TStringlist;
begin
  // #@fullname*#, #@orgname*#,
  // #@imagename*#, #@windows_partition_number*#, #@winpe_uilanguage*#,
  // #@winpe_inputlocale*#, #@system_language*#, #@winpe_uilanguage_fallback*#
  // #@winpe_uilanguage*#, #@pcname*#, #@system_keyboard_layout*#, #@system_language*#
  // UILanguage:#@system_language*#, UILanguageFallback:#@system_language*#,
  // UserLocale:#@system_language*#
  // TimeZone:#@system_timezone*#
  // <CommandLine>cmd.exe /c #@winpe_partition_letter*#:\opsi\postinst.cmd</CommandLine>
  //
  // settings / component / DiskConfiguration / Disk / ModifyPartitions : #@modify_partitions*#
  // settings / component / UserData / Key : #@productkey*#
  memo3.clear;
  docelemSL := TStringlist.Create;
  childnodeSL:= TStringlist.Create;
  childnodeSL2:= TStringlist.Create;
  childnodeSL3:= TStringlist.Create;
  childnodeSL4:= TStringlist.Create;
  childnodeSL5:= TStringlist.Create;
  docelemSL:= getDocumentElementAsStringlist(memoToTStringlist(memo1));
  //memo3.Append(docelemSL.Text);
  if xmlAsStringlistGetChildnodeByNameAndAttributeKeyAndValue(docelemSL,'settings',
                                   'pass','windowsPE',childnodeSL) then
        if xmlAsStringlistGetChildnodeByNameAndAttributeKeyAndValue(childnodeSL,'component',
                                  'name','Microsoft-Windows-Setup',childnodeSL2) then
          begin
            //**************************************************************************************

            if xmlAsStringlistGetUniqueChildnodeByName(childnodeSL2,
                  'DiskConfiguration',childnodeSL3)  then
              if xmlAsStringlistGetUniqueChildnodeByName(childnodeSL3,
                  'Disk',childnodeSL4)  then
                if xmlAsStringlistGetUniqueChildnodeByName(childnodeSL4,
                  'ModifyPartitions',childnodeSL5)  then
                  if xmlAsStringlistSetNodevalue(childnodeSL5,'***modify_partitions***') then
                    if xmlAsStringlistReplaceUniqueChildnodeByName(childnodeSL4,
                       'ModifyPartitions',childnodeSL5)  then
                      if xmlAsStringlistReplaceUniqueChildnodeByName(childnodeSL3,
                        'Disk',childnodeSL4)  then
                        if xmlAsStringlistReplaceUniqueChildnodeByName(childnodeSL2,
                          'DiskConfiguration',childnodeSL3)  then
                            memo3.Append('***modify_partitions*** set')
                        else memo3.Append('can not replace node DiskConfiguration')
                      else memo3.Append('can not replace node Disk')
                    else memo3.Append('can not replace node ModifyPartitions')
                  else memo3.Append('can not set node ModifyPartitions')
                else memo3.Append('Node ModifyPartitions not found')
              else memo3.Append('Node Disk not found')
            else memo3.Append('Node DiskConfiguration not found');

            //**************************************************************************************
            (*
            if xmlAsStringlistGetUniqueChildnodeByName(childnodeSL2,
                  'UserData',childnodeSL3)  then
              if xmlAsStringlistGetUniqueChildnodeByName(childnodeSL3,
                  'ProductKey',childnodeSL4)  then
                if xmlAsStringlistGetUniqueChildnodeByName(childnodeSL4,
                  'Key',childnodeSL5)  then
                  if xmlAsStringlistSetNodevalue(childnodeSL5,'***productkey***') then
                    if xmlAsStringlistReplaceUniqueChildnodeByName(childnodeSL4,
                       'Key',childnodeSL5)  then
                      if xmlAsStringlistReplaceUniqueChildnodeByName(childnodeSL3,
                        'ProductKey',childnodeSL4)  then   // Fehler, warum???
                        if xmlAsStringlistReplaceUniqueChildnodeByName(childnodeSL2,
                          'UserData',childnodeSL3)  then
                        else memo3.Append('can not replace node UserData')
                      else memo3.Append('can not replace node ProductKey')
                    else memo3.Append('can not replace node Key')
                  else memo3.Append('can not set node Key')
                else memo3.Append('Node Key not found')
              else memo3.Append('Node ProductKey not found')
            else memo3.Append('Node UserData not found');
            *)
            if xmlAsStringlistGetUniqueChildnodeByName(childnodeSL2,
                  'UserData',childnodeSL3)  then
              begin
                if xmlAsStringlistGetUniqueChildnodeByName(childnodeSL3,
                    'FullName',childnodeSL4)  then
                  if xmlAsStringlistSetNodevalue(childnodeSL4,'***fullname***') then
                    if xmlAsStringlistReplaceUniqueChildnodeByName(childnodeSL3,
                         'FullName',childnodeSL4)  then
                        memo3.Append('***fullname*** set')
                    else memo3.Append('can not replace node FullName')
                  else memo3.Append('can not set node FullName')
                else memo3.Append('Node FullName not found');

                if xmlAsStringlistGetUniqueChildnodeByName(childnodeSL3,
                    'Organization',childnodeSL4)  then
                  if xmlAsStringlistSetNodevalue(childnodeSL4,'***orgname***') then
                    if xmlAsStringlistReplaceUniqueChildnodeByName(childnodeSL3,
                         'Organization',childnodeSL4)  then
                      memo3.Append('***orgname*** set')
                    else memo3.Append('can not replace node Organization')
                  else memo3.Append('can not set node Organization')
                else memo3.Append('Node Organization not found');

                if xmlAsStringlistReplaceUniqueChildnodeByName(childnodeSL2,
                    'UserData',childnodeSL3)  then
                        memo3.Append('UserDate replaced')
                else memo3.Append('can not replace node UserData')

              end
            else memo3.Append('Node UserData not found');

            //**************************************************************************************
            if xmlAsStringlistReplaceChildnodeByNameAndAttributeKeyAndValue(childnodeSL,
               'component','name','Microsoft-Windows-Setup',childnodeSL2) then
              if xmlAsStringlistReplaceChildnodeByNameAndAttributeKeyAndValue(docelemSL,
                'settings','pass','windowsPE',childnodeSL) then
                 if createXmlDocFromStringlist(docelemSL) then
                   memo3.Append(getXMLDocAsTStringlist.Text)
                 else memo3.Append('can not create xml')
               else memo3.Append('can not replace node settings')
             else memo3.Append('can not replace node component')
            //**************************************************************************************
          end
        else memo3.Append('Node component not found')
    else memo3.Append('Node settings not found');
  freeXmlDoc();
end;

procedure TForm1.Memo1Change(Sender: TObject);
begin

end;



end.

