unit xmlpatch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, ExtCtrls, osxmltdom,myoslog, winpatchcollection, Dom, Inifiles;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    closeBtn: TButton;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    procedure BitBtn1Click(Sender: TObject);
    procedure closeBtnClick(Sender: TObject);
  private
    { private declarations }

  public
    { public declarations }
  end;

var
  Form1: TForm1;
  inifilename, packagefileName, logfilename: String;
  valuesTStringlist : TStringlist;
  inifile : TIniFile;
  cabList: TStringList;

implementation

{$R *.lfm}

{ TForm1 }

function memoToTStringlist (mymemo: TMemo) : TStringlist;
begin
  Result := TStringList.Create;
  Result.Assign(mymemo.Lines);
end;

function createCablist(const inifilename: String) : TStringList ;
var mycabList: TStringList;
    sl: TStringlist;
    i: integer;

begin
  inifile:= TIniFile.Create(inifilename,False);
  mycabList:=TStringList.Create;
  sl:=TStringList.Create;
  try
    iniFile.ReadSections(sl);
  except
    // TODO
  end;
  for i:=0 to (sl.Count-1) do
  begin
    if (sl[i]<>'global') AND (sysutils.ExtractFileExt(sl[i])='.cab')  then
      mycabList.Append(sl[i]);
  end;
  sl.Free;
  Result:= mycablist;
end;

function writePatchedInifile (const inifilename:String; patchItemList: TListe) : boolean;
var i: integer;
begin
  result:=false;
  inifile:= TIniFile.Create(inifilename,False);
  for i:=0 to (patchItemList.Count-1) do
  begin
    try
      inifile.WriteString(patchItemList.Items[i].a_cabname, 'title',
                          patchItemList.Items[i].a_title);
      inifile.WriteString(patchItemList.Items[i].a_cabname, 'description',
                          patchItemList.Items[i].a_description);
      if (patchItemList.Items[i].a_cabname<>'') then
        inifile.WriteString(patchItemList.Items[i].a_cabname, 'releaseid',
                          patchItemList.Items[i].a_releaseid);
      if (patchItemList.Items[i].a_installationtype<>'') then
        inifile.WriteString(patchItemList.Items[i].a_cabname, 'installationtype',
                          patchItemList.Items[i].a_installationtype);
      result:=true;
    except
      //TODO
    end;
  end;
end;

function getFileLocationIds (listofchildnodes: TDOMNode; var patchItemList: TListe) : boolean;
var j:integer;
    FileUrl, FileLocId, filename: String;
    cNode: TDOMNode;
begin
  result:=false;
  try
    if (listofchildnodes <> nil) AND (listofchildnodes.hasChildNodes) then
    begin
      cNode := listofchildnodes.ChildNodes.Item[0];
      while cNode <> nil do
      begin
            if getNodeattributeByKey(cNode, 'Url', FileUrl) then
               begin
                 filename:= ExtractFilename(FileUrl);
                 for j:=0 to cablist.Count-1 do
                   if (filename=cablist[j]) AND (sysutils.ExtractFileExt(cablist[j])='.cab') then
                     if (getNodeattributeByKey(cNode, 'Id', FileLocId)) then
                       begin
                         patchItemList.Add;
                         patchItemList.Items[patchItemList.Count-1].a_fileId:= FileLocId;
                         patchItemList.Items[patchItemList.Count-1].a_cabname:= filename;
                       end
                     else
                       patchItemList.Items[patchItemList.Count-1].a_fileId:= '';
                   //else würde alle ungültigen Vergleiche schreiben
                   //  logdatei.log('file not found??? ' + cablist[j], oslog.LLwarning);
                   cNode:= cNode.NextSibling;
                end
            else
              cNode := cNode.NextSibling;
       end;
    end;
    if patchItemList.Count>0 then result:=true;
  finally
    logdatei.log('getFileLocationIds finished ',myoslog.LLinfo);
  end;
end;

function  getRevisionId (listofchildnodes: TDOMNode; var patchItemList: TListe) : boolean;
var i,j:integer;
    fileId, filename, defaultlang: String;
    cNode, childNode, childNode2: TDOMNode;
begin
  result:=false;
  try
    if (listofchildnodes <> nil) AND (listofchildnodes.hasChildNodes) then
    begin
      cNode := listofchildnodes.ChildNodes.Item[0];
      while cNode <> nil do
      begin
        // wenn der update-Knoten den Unterknoten PayloadFiles hat
        if getUniqueChildnodeByName(cNode, 'PayloadFiles', childNode) then
          if getUniqueChildnodeByName(childnode, 'File', childNode2) then
            if (getNodeattributeByKey(childNode2, 'Id', fileId)) then
              begin
                for j:=0 to patchItemList.Count-1 do
                  if fileid = patchitemList.Items[j].a_fileId then  // find Item
                    begin
                      // hole den BundleBy-Knoten und die ID in Revision  = Dateiname
                      if getUniqueChildnodeByName(cNode, 'BundledBy', childNode) then
                        if getUniqueChildnodeByName(childnode, 'Revision', childNode2) then
                          if getNodeattributeByKey(childNode2, 'Id', filename) then
                            begin
                              patchItemList.Items[j].a_filename:= filename;
                              if getNodeattributeByKey(cNode, 'DefaultLanguage', defaultlang) then
                                patchItemList.Items[j].a_defaultlang := defaultlang;
                              result:=true;
                            end
                          else patchItemList.Items[j].a_filename:= '';
                    end;
               end;
       cNode:=cNode.NextSibling;
      end
    end;
  finally
    i:=  patchItemList.Count;
    logdatei.log('getRevisionId finished ',myoslog.LLinfo);
  end;
end;
procedure TForm1.BitBtn1Click(Sender: TObject);
var childnode1, docelem: TDOMNode;
    Liste: TListe;
    i : integer;
    tfIn: TextFile;
    filepath,langfilename: string;
    xversion : String;
begin
  try
  begin
    Memo1.Clear;
    Liste := TListe.Create;
    // open inifile
    OpenDialog1.Filter:='ini-file | *.ini';
    OpenDialog1.Title:='Vorhandene ini-Datei öffnen';
    if OpenDialog1.Execute then
      begin
        inifilename:='';
        inifileName:=OpenDialog1.FileName;
        if inifilename <> '' then
          begin
            logdatei.log('opening file: '+inifilename,myoslog.LLinfo);
            // get sysVersion
            if (pos('_x64_', inifilename) > 0)
               then xVersion:='x64'
            else if (pos('_x86_', inifilename) > 0)
               then xVersion:='x86';
            cabList:=createCablist(inifilename);
            Memo1.Append('ini file ' + inifilename);
          end
        else
          begin
            Memo1.Append('no file ' + inifilename);
            logdatei.log('no inifile', myoslog.LLerror);
            exit;
          end;
        for i:=0 to (cablist.Count-1) do
          logdatei.log('cablist item ' + inttostr(i) + ' : ' + cablist[i],myoslog.LLinfo);
        // open package.xml
        OpenDialog1.Filter:='xml-file | *.xml';
        OpenDialog1.Title:='Vorhandene package.xml öffnen';
        if OpenDialog1.Execute then
        begin
          packagefileName:=OpenDialog1.FileName;
          logdatei.log('opening file: '+packagefileName,5);
          createXmlDocFromFile(packagefileName);
          Memo1.Append('read file ' + packagefileName);
          docelem:=getDocumentElement();
          Memo1.Append('DocNodeName ' + getDocNodeName);
          if getUniqueChildNodeByName(docelem,'FileLocations', childnode1) then
            begin
              if childnode1<>NIL then
               Memo1.Append('Anzahl ChildNodes FileLocations:' + inttostr(childNode1.ChildNodes.Count));
               if getFileLocationIds(childnode1,Liste) then
                  begin
                    logdatei.log('file location Ids found : ' + inttostr(Liste.Count) ,myoslog.LLinfo)
                  end
               else
                 logdatei.log('getFileLocationIds failed',myoslog.LLwarning)
            end
          else
            logdatei.log('getUniqueChildNodeByName FileLocations failed',myoslog.LLwarning);
          //**********************************
          if getUniqueChildNodeByName(docelem,'Updates', childnode1) then
            if getRevisionId(childnode1,Liste) then
              begin
                Memo1.Append('Collection, Items found : ' + inttostr(Liste.Count));
                logdatei.log('Collection, Items found : ' + inttostr(Liste.Count), myoslog.LLinfo);
               end
            else
              logdatei.log('getRevisionId failed',myoslog.LLwarning)
          else
            logdatei.log('getUniqueChildNodeByName Updates failed',myoslog.LLwarning);
          memo1.Repaint;
          Application.ProcessMessages;
          osxmltdom.freeXmlDoc();
          //Memo1.Clear;
          for i:=0 to Liste.Count-1 do
          begin
            filepath:=ExtractFilePath(packagefileName);
            langfilename:=filepath + 'l' +PathDelim + Liste.Items[i].a_defaultlang + PathDelim +  Liste.Items[i].a_filename;
            Memo1.Append('languagefile : ' + langfilename);
            AssignFile(tfIn, langfilename);
            try
              reset(tfIn);
              logdatei.log('opening file: '+ langfilename,myoslog.LLinfo);
              // read xml language file
              createXmlDocFromFile(langfilename);
              docelem:=getDocumentElement();
              // get title and description
              if getUniqueChildNodeByName(docelem,'Title', childnode1) then
                 if childnode1 <> NIL then
                  begin
                   Liste.Items[i].a_title := childnode1.TextContent;
                   logdatei.log('Title : ' + Liste.Items[i].a_title,myoslog.LLinfo);
                   if (pos(xversion, Liste.Items[i].a_title) = 0) then // not found
                     logdatei.log(xversion + ' not in title : ' + Liste.Items[i].a_title,myoslog.LLwarning);
                   if (pos('Windows 10', Liste.Items[i].a_title) = 0) then // not found
                     logdatei.log('Windows 10 not in title : ' + Liste.Items[i].a_title,myoslog.LLwarning);
                   // get releaseid
                   if (pos('Version', Liste.Items[i].a_title) > 0) then
                     begin
                       Liste.Items[i].a_releaseid := copy (Liste.Items[i].a_title,
                             pos('Version', Liste.Items[i].a_title)+8,
                             4);
                     end
                   else
                   if (pos('Windows Server 2016',Liste.Items[i].a_title) > 0) and (pos('x64-based',Liste.Items[i].a_title)>0) then
                     Liste.Items[i].a_releaseid:='1607'
                   else
                   if (pos('Windows Server 2016 Technical Preview',Liste.Items[i].a_title) > 0) then // found
                     begin
                       Liste.Items[i].a_releaseid:='1511Server';
                       Liste.Items[i].a_installationtype:='server';
                     end
                   else // default
                     Liste.Items[i].a_releaseid:='1507';
                  end
                 else
                   begin
                     Liste.Items[i].a_title := '';
                     logdatei.log('Title not found',myoslog.LLwarning);
                   end;
               childnode1.Free;
               if getUniqueChildNodeByName(docelem,'Description', childnode1) then
                 if childnode1 <> NIL then
                  begin
                   Liste.Items[i].a_description := childnode1.TextContent;
                   logdatei.log('Description : ' + Liste.Items[i].a_description,myoslog.LLinfo);
                  end
                 else
                   begin
                     Liste.Items[i].a_title := '';
                     logdatei.log('Description not found',myoslog.LLwarning);
                   end;
              CloseFile(tfIn);
              osxmltdom.freeXmlDoc();
            except
                on E: EInOutError do
                 logdatei.log('File handling error occurred. Details: ' + E.Message, myoslog.LLerror);
            end;
          end;
          for i:=0 to Liste.Count-1 do
             logdatei.log('Collection Item  : ' +
                      Liste.Items[i].a_cabname + ' : '  +
                      Liste.Items[i].a_fileId + ' : ' +
                      Liste.Items[i].a_filename + ' : ' +
                      Liste.Items[i].a_defaultlang + ' : ' +
                      Liste.Items[i].a_title + ' : ' +
                      Liste.Items[i].a_description + ' : ' +
                      Liste.Items[i].a_releaseid  + ' : ' +
                      Liste.Items[i].a_installationtype
                      , myoslog.LLinfo);
          // patch ini-Datei
          if writePatchedInifile(inifileName, Liste) then
           begin
             logdatei.log('success: patch inifile ' + inifilename, myoslog.LLinfo);
             Memo1.Append('success: patch inifile ' + inifilename)
           end
          else
            logdatei.log('patch inifile failed ' + sysutils.ExtractFilePath(inifilename) + sysutils.ExtractFileName(inifilename) +'_patched.ini', myoslog.LLerror);
        end
        else
          logdatei.log('open packagefile, no filename ', myoslog.LLwarning);
        inifile.Free;
     end
   else logdatei.log('open inifile, no filename ', myoslog.LLwarning);
  end
  finally
    Liste.Free;
  end;
end;

procedure TForm1.closeBtnClick(Sender: TObject);
begin
  logdatei.log('end of logging ', myoslog.LLinfo);
  LogDatei.Close;
  Application.Terminate;
end;

begin
  logdatei := Tloginfo.Create;
  StandardMainLogPath:='/tmp';
  StandardPartLogPath := '/tmp';
  StandardPartLogFilename := 'xmlpatch';
  logfilename := 'xmlpatch.log';
  CreateTheLogfile(logfilename);
  logdatei.AktProduktId:='xmlpatch';
  LogDatei.log('start logging',myoslog.LLinfo);
end.

