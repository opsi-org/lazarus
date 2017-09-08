unit xmlpatch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, CustApp,
  osxmltdom,myoslog, winpatchcollection, Dom, Inifiles;



procedure openAndPatch(ininame,packagename :string);
procedure initlog(logpath : string);

var
  inifilename, packagefileName, logfilename: String;
  valuesTStringlist : TStringlist;
  inifile : TIniFile;
  cabList: TStringList;


implementation
uses
  cliapp;


{$R *.lfm}

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
    logdatei.log('error in createCablist - ' + inifilename, myoslog.LLError);
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
      logdatei.log('error in writePatchedInifile - ' + inifilename, myoslog.LLError);
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


procedure openAndPatch(ininame,packagename :string);
var childnode1, docelem: TDOMNode;
    Liste: TListe;
    i : integer;
    tfIn: TextFile;
    filepath,langfilename: string;
    xversion : String;
begin
  try
  begin
    Liste := TListe.Create;
    inifileName:=ininame;
    if FileExists(inifileName) then
    begin
        if inifilename <> '' then
          begin
            logdatei.log('opening file: '+inifilename,myoslog.LLinfo);
            writeln('opening file: '+inifilename);
            // get sysVersion
            if (pos('_x64_', inifilename) > 0)
               then xVersion:='x64'
            else if (pos('_x86_', inifilename) > 0)
               then xVersion:='x86';
            cabList:=createCablist(inifilename);
          end
        else
          begin
            logdatei.log('no inifile', myoslog.LLerror);
            exit;
          end;
        for i:=0 to (cablist.Count-1) do
        begin
          logdatei.log('cablist item ' + inttostr(i) + ' : ' + cablist[i],myoslog.LLinfo);
          writeln('cablist item ' + inttostr(i) + ' : ' + cablist[i]);
        end;
        // open package.xml
        packagefileName:=packagename;
        if FileExists(packagefileName) then
        begin
          logdatei.log('opening file: '+packagefileName,myoslog.LLinfo);
          writeln('opening file: '+packagefileName);
          createXmlDocFromFile(packagefileName);
          docelem:=getDocumentElement();
          if getUniqueChildNodeByName(docelem,'FileLocations', childnode1) then
            begin
              if childnode1<>NIL then
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
                logdatei.log('Collection, Items found : ' + inttostr(Liste.Count), myoslog.LLinfo);
               end
            else
              logdatei.log('getRevisionId failed',myoslog.LLwarning)
          else
            logdatei.log('getUniqueChildNodeByName Updates failed',myoslog.LLwarning);
          osxmltdom.freeXmlDoc();

          for i:=0 to Liste.Count-1 do
          begin
            filepath:=ExtractFilePath(packagefileName);
            langfilename:=filepath + 'l' +PathDelim + Liste.Items[i].a_defaultlang + PathDelim +  Liste.Items[i].a_filename;
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
          inifileName:= sysutils.ExtractFilePath(inifilename) +
              copy(sysutils.ExtractFileName(inifilename),1,pos('.',sysutils.ExtractFileName(inifilename))-1) +'_patched.ini';
          if writePatchedInifile(inifileName, Liste) then
           begin
             logdatei.log('success: patch inifile ' + inifilename, myoslog.LLinfo);
             writeln('success: patch inifile ' + inifilename);
           end
          else
          begin
            logdatei.log('patch inifile failed ' + inifilename, myoslog.LLerror);
            writeln('failed: patch inifile ' + inifilename);
          end
        end
        else
        begin
          logdatei.log('open packagefile: no valid filename or file does not exist - ' + packagefilename, myoslog.LLError);
          Application.WriteHelp;
          Application.Terminate;
          Exit;
        end;
        inifile.Free;
     end
   else
   begin
     logdatei.log('open inifile: no valid filename or file does not exist - ' + inifilename, myoslog.LLError);
     Application.WriteHelp;
     Application.Terminate;
     Exit;
   end
  end
  finally
    Liste.Free;
  end;
end;


procedure initlog(logpath : string);
begin
  //default
  logdatei := Tloginfo.Create;
  StandardMainLogPath:='/tmp';
  StandardPartLogPath :='/tmp';
  StandardPartLogFilename := 'xmlpatch';
  if logpath<>'' then
  begin
    if directoryExists(logpath) then
    begin
      StandardMainLogPath:=logpath;
      StandardPartLogPath :=logpath;
    end
    else
      if not(createDir(logpath)) then
      begin
        writeln('can not create directory ' + logpath);
        Application.WriteHelp;
        Application.Terminate;
        Exit;
      end
      else
      begin
       StandardMainLogPath:=logpath;
       StandardPartLogPath :=logpath;
      end;
  end;
  writeln ('logpath: ' + StandardMainLogPath);

  logfilename := 'xmlpatch.log';
  CreateTheLogfile(logfilename);
  logdatei.AktProduktId:='xmlpatch';
  LogDatei.log('start mshotfixpatchxml',myoslog.LLinfo);
end;

begin
end.
end;

