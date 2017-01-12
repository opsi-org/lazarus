unit xmlpatch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, ExtCtrls, osxmltdom,oslog, winpatchcollection, Dom, Inifiles;

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
      result:=true;
    except
      //TODO
    end;
  end;

  inifile.Free;
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
    logdatei.log('getFileLocationIds finished ',oslog.LLinfo);
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
    logdatei.log('getRevisionId finished ',oslog.LLinfo);
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
    // open inifile
    OpenDialog1.Filter:='ini-file | *.ini';
    OpenDialog1.Title:='Vorhandene ini-Datei öffnen';
    OpenDialog1.Execute;
    inifilename:='';
    inifileName:=OpenDialog1.FileName;
    if inifilename <> '' then
      begin
        logdatei.log('opening file: '+inifilename,oslog.LLinfo);
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
        logdatei.log('no inifile', oslog.LLerror);
        exit;
      end;
    for i:=0 to (cablist.Count-1) do
      logdatei.log('cablist item ' + inttostr(i) + ' : ' + cablist[i],oslog.LLinfo);
    // open package.xml
    OpenDialog1.Filter:='xml-file | *.xml';
    OpenDialog1.Title:='Vorhandene package.xml öffnen';
    OpenDialog1.Execute;
    packagefileName:=OpenDialog1.FileName;
    logdatei.log('opening file: '+packagefileName,5);
    createXmlDocFromFile(packagefileName);

    Memo1.Append('read file ' + packagefileName);
    docelem:=getDocumentElement();
    Memo1.Append('DocNodeName ' + getDocNodeName);
    Liste := TListe.Create;
    if getUniqueChildNodeByName(docelem,'FileLocations', childnode1) then
      begin
        if childnode1<>NIL then
         Memo1.Append('Anzahl ChildNodes FileLocations:' + inttostr(childNode1.ChildNodes.Count));
         if getFileLocationIds(childnode1,Liste) then
            begin
              logdatei.log('file location Ids found : ' + inttostr(Liste.Count) ,oslog.LLinfo)
            end
         else
           logdatei.log('getFileLocationIds failed',oslog.LLwarning)
      end
    else
      logdatei.log('getUniqueChildNodeByName FileLocations failed',oslog.LLwarning);
    //**********************************
    if getUniqueChildNodeByName(docelem,'Updates', childnode1) then
      if getRevisionId(childnode1,Liste) then
        begin
          Memo1.Append('Collection, Items found : ' + inttostr(Liste.Count));
          logdatei.log('Collection, Items found : ' + inttostr(Liste.Count), oslog.LLinfo);
         end
      else
        logdatei.log('getRevisionId failed',oslog.LLwarning)
    else
      logdatei.log('getUniqueChildNodeByName Updates failed',oslog.LLwarning);
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
        logdatei.log('opening file: '+ langfilename,oslog.LLinfo);
        // read xml language file
        createXmlDocFromFile(langfilename);
        docelem:=getDocumentElement();
        // get title and description
        childnode1.CleanupInstance;
        if getUniqueChildNodeByName(docelem,'Title', childnode1) then
           if childnode1 <> NIL then
            begin
             Liste.Items[i].a_title := childnode1.TextContent;
             logdatei.log('Title : ' + Liste.Items[i].a_title,oslog.LLinfo);
             if (pos(xversion, Liste.Items[i].a_title) = 0) then // not found
               logdatei.log(xversion + ' not in title : ' + Liste.Items[i].a_title,oslog.LLwarning);
             if (pos('Windows 10', Liste.Items[i].a_title) = 0) then // not found
               logdatei.log('Windows 10 not in title : ' + Liste.Items[i].a_title,oslog.LLwarning);
             if (pos('Version', Liste.Items[i].a_title) > 0) then
               begin
                 Liste.Items[i].a_releaseid := copy (Liste.Items[i].a_title,
                       pos('Version', Liste.Items[i].a_title)+8,
                       4);
               end
             else
               Liste.Items[i].a_releaseid:='1507';
            end
           else
             begin
               Liste.Items[i].a_title := '';
               logdatei.log('Title not found',oslog.LLwarning);
             end;
         childnode1.Free;
         if getUniqueChildNodeByName(docelem,'Description', childnode1) then
           if childnode1 <> NIL then
            begin
             Liste.Items[i].a_description := childnode1.TextContent;
             logdatei.log('Description : ' + Liste.Items[i].a_description,oslog.LLinfo);
            end
           else
             begin
               Liste.Items[i].a_title := '';
               logdatei.log('Description not found',oslog.LLwarning);
             end;
        CloseFile(tfIn);
        osxmltdom.freeXmlDoc();
      except
          on E: EInOutError do
           logdatei.log('File handling error occurred. Details: ' + E.Message, oslog.LLerror);
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
                Liste.Items[i].a_releaseid
                , oslog.LLinfo);
    // patch ini-Datei
    if writePatchedInifile(inifileName, Liste) then
     begin
       logdatei.log('success: patch inifile ' + inifilename, oslog.LLinfo);
       Memo1.Append('success: patch inifile ' + inifilename)
     end
    else
      logdatei.log('patch inifile failed ' + inifilename, oslog.LLerror);
  end

  finally
    Liste.Free;
  end;
end;

procedure TForm1.closeBtnClick(Sender: TObject);
begin
  logdatei.log('end of logging ', oslog.LLinfo);
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
  LogDatei.log('start logging',oslog.LLinfo);
end.

