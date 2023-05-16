unit ocr_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  ExtCtrls, StdCtrls, Buttons,
  fpjsonrtti,
  fileutil,
  inifiles,
  lazfileutils,
  strutils,
  ocrbasedata,
  ocrconfigdlg,
  oswebservice,
  osmessagedialog,
  oslog,
  osjson,
  osparserhelper;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtnConnect: TBitBtn;
    BtnSwitchOn: TBitBtn;
    BtnSwitchOff: TBitBtn;
    CBoxRules: TComboBox;
    FlowPanel1: TFlowPanel;
    FlowPanel2: TFlowPanel;
    ImageList1: TImageList;
    Label1: TLabel;
    Label3: TLabel;
    ListBoxClient: TListBox;
    ListBoxServer: TListBox;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MExit: TMenuItem;
    MFile: TMenuItem;
    MAbout: TMenuItem;
    MConfiguration: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    ToolBar1: TToolBar;
    TreeView1: TTreeView;
    procedure BitBtnConnectClick(Sender: TObject);
    procedure BtnSwitchOffClick(Sender: TObject);
    procedure BtnSwitchOnClick(Sender: TObject);
    procedure CBoxRulesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBoxServerSelectionChange(Sender: TObject; User: boolean);
    procedure MAboutClick(Sender: TObject);
    procedure MConfigurationClick(Sender: TObject);
    procedure MExitClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TreeView1Click(Sender: TObject);
    procedure TreeView1DblClick(Sender: TObject);
    procedure TreeView1SelectionChanged(Sender: TObject);
  private

  public

  end;

function startOpsiServiceConnection: boolean;
procedure readserver;
procedure readgroups;
procedure readclients;


var
  Form1: TForm1;
  localservicedata: TOpsi4Data = nil;
  passwordToUse: string;
  opsiserviceversion, sessionid: string;
  defaultConfigList: TStringList;
  configIdsList: TStringList;
  configSwitchList: TStringList;
  clientIdsList: TStringList;
  defaultCommandlist: TStringList;
  myinifilename: string;
  myConfigStateObectJsonStr, myConfigStateArrayJsonStr: string;
  myConfigStateList, allClientIdsList: TStringList;

resourcestring
  rsServiceConnectionFailed =
    'Could not connect to the opsi-web-service. Check URL, user and password';


implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  ruleslist: TStringList;
  i: integer;
begin
  try
    ruleslist := FindAllFiles(ExtractFileDir(ParamStr(0)) + PathDelim +
      'opsi-config-rules.d', '*.ini', False);
    for i := 0 to ruleslist.Count - 1 do
      ruleslist[i] := ExtractFileNameOnly(ruleslist[i]);
    CBoxRules.Items.AddStrings(ruleslist);
    CBoxRules.ItemIndex := 0;
    CBoxRulesChange(Sender);
  finally
    FreeAndNil(ruleslist)
  end;
  BtnSwitchOff.Enabled := False;
  BtnSwitchOn.Enabled := False;
  allClientIdsList := TStringList.Create;
  Application.ProcessMessages;
  //if myconfiguration.Service_pass <> '' then BitBtnConnectClick(nil);
  defaultConfiglist := TStringList.Create;
  defaultCommandlist := TStringList.Create;
  configIdsList := TStringList.Create;
  configSwitchList := TStringList.Create;
  clientIdsList := TStringList.Create;
  myConfigStateList := TStringList.Create;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Application.ProcessMessages;
  Timer1.Enabled := True;
end;

procedure TForm1.ListBoxServerSelectionChange(Sender: TObject; User: boolean);
begin
  readgroups;
  readclients;
end;

procedure switchConfig(switchList: TStringList; clientIdsList: TStringList);
begin
end;

function configExists(configId: string): boolean;
var
  method, jsonstr: string;
  params: array of string;
  LogErrorMessage, serviceresult: string;
begin
  serviceresult := '';
  Result := False;
  method := 'config_getObjects';
  params := ['', '{"id":"' + configId + '"}'];
  LogErrorMessage :=
    'Warning: Could not get config defaults from service (oswebservice: TOpsi4Data.getConfigObjectsFromService)';
  serviceresult := localservicedata.getJSONFromService(method, params, logErrorMessage);
  LogDatei.log('Serviceresult from config_getObjects (in configExists): ' +
    serviceresult, LLdebug);
  jsonAsObjectGetValueByKey(serviceresult, 'result', jsonstr);
  if jsonAsArrayCountElements(jsonstr) > 0 then
  begin
    Result := True;
    LogDatei.log('configExists: found existing config: ' + configId, LLinfo);
  end;
end;

procedure createConfig(defaultCommand: string);
var
  method: string;
  params: array of string;
  LogErrorMessage, serviceresult: string;
  i: integer;
begin
  // method config_createBool id *description *defaultValues
  // method config_createUnicode id *description *possibleValues *defaultValues *editable *multiValue
  serviceresult := '';
  // get method from command
  //method := copy(defaultCommand,0,pos(' ',defaultCommand));
  method := Copy2SpaceDel(defaultCommand);
  // get params from command
  //params := copy(defaultCommand,pos(' ',defaultCommand),length(defaultCommand));
  params := SplitString(defaultCommand, ' ');
  for i := 0 to length(params) - 1 do
    params[i] := opsiunquotestr2(params[i], '"');
  // call method
  LogErrorMessage := 'Warning: Could not create config defaults with service method: ' +
    method;
  serviceresult := localservicedata.getJSONFromService(method, params, logErrorMessage);
  LogDatei.log('Serviceresult from createConfig: ' + serviceresult, LLdebug);
end;

procedure checkConfig(configId: string; defaultCommand: string);
var
  key, val: string;

begin
  if not configExists(configId) then
  begin
    LogDatei.log('Will create (default) config: ' + configId, LLinfo);
    createConfig(defaultCommand);
  end
  else
    LogDatei.log('config: ' + configId + ' already exists.', LLdebug);
end;

procedure readAndCheckDefault;
var
  clientIdsList: TStringList;
  clientId: string;
  ruleId: string;
  //myinifilename : string;
  myIni: TIniFile;
  i: integer;
  id, val: string;
  isBool: boolean;
begin
  LogDatei.log('readAndCheckDefault', LLnotice);
  //myinifilename := CBoxRules.SelText;
  //myinifilename := ExtractFileDir(paramstr(0))+PathDelim+'opsi-config-rules.d'
  //                  +PathDelim+myinifilename + '.ini';
  if FileExists(myinifilename) then
  begin
    myIni := TIniFile.Create(myinifilename);
    myIni.ReadSectionValues('default', defaultConfiglist);
    myIni.ReadSection('default', configIdsList);
    myIni.ReadSectionValues('default_commands', defaultCommandlist);
    //memo1.Text:= myIni.ReadString('general','description');
  end;
  myIni.Free;
  for i := 0 to configIdsList.Count - 1 do
  begin
    id := configIdsList.strings[i];
    //val := trim(defaultConfiglist.Values[id]);
    //if  (LowerCase(val) = 'true') or (LowerCase(val) = 'false') then
    //isBool:= true
    //else isBool:= false;
    checkConfig(id, defaultCommandlist[i]);
  end;
end;

procedure addConfigState(configId, val, clientId: string);
begin
  myConfigStateObectJsonStr :=
    '{"configId":"' + configId + '",' + '"objectId":"' + clientId +
    '",' + ' "values": [' + val + '],' + '"type": "ConfigState"}';
  myConfigStateList.Add(myConfigStateObectJsonStr);
  LogDatei.log('Added configState: ' + myConfigStateObectJsonStr, LLdebug);
end;

procedure readAndCheckSwitch(switchOn: boolean);
var
  clientIdsList: TStringList;
  clientId: string;
  ruleId: string;
  //myinifilename : string;
  myIni: TIniFile;
  i: integer;
  id, val: string;
  isBool: boolean;
  sectionName: string;
begin
  LogDatei.log('readAndCheckSwitch', LLnotice);
  if switchOn then sectionName := 'switch_on'
  else
    sectionName := 'switch_off';
  //myinifilename := CBoxRules.SelText;
  //myinifilename := ExtractFileDir(paramstr(0))+PathDelim+'opsi-config-rules.d'
  //                  +PathDelim+myinifilename + '.ini';
  if FileExists(myinifilename) then
  begin
    myIni := TIniFile.Create(myinifilename);
    myIni.ReadSectionValues(sectionName, configSwitchList);
  end;
  myIni.Free;
end;

procedure getClientIdsList;
var
  str: string;
begin
  LogDatei.log('getClientIdsList', LLnotice);
  clientIdsList.Clear;
  //clientIdsList.Add(form1.Edit1.Caption);
  str := Form1.ListBoxClient.GetSelectedText;
  clientIdsList.Text := str;

end;

procedure updateConfigStates;
var
  i, k: integer;
  id, val, clientid: string;
  method: string;
  params: array of string;
  LogErrorMessage, serviceresult: string;
begin
  LogDatei.log('updateConfigStates', LLnotice);
  for i := 0 to configIdsList.Count - 1 do
    for k := 0 to clientIdsList.Count - 1 do
    begin
      id := configIdsList.strings[i];
      val := trim(configSwitchList.Values[id]);
      clientid := clientIdsList.Strings[k];
      addConfigState(id, val, clientid);
    end;
  serviceresult := '';
  method := 'configState_updateObjects';
  // get params
  if stringListToJsonArray(myConfigStateList, myConfigStateArrayJsonStr) then
  begin
    params := [myConfigStateArrayJsonStr];
  end;
  for i := 0 to length(params) - 1 do
    params[i] := opsiunquotestr2(params[i], '"');
  // call method
  LogErrorMessage := 'Warning: Could not create config defaults with service method: ' +
    method;
  serviceresult := localservicedata.getJSONFromService(method, params, logErrorMessage);
  LogDatei.log('Serviceresult from createConfig: ' + serviceresult, LLdebug);
end;

procedure TForm1.BtnSwitchOnClick(Sender: TObject);
begin
  LogDatei.log('BtnSwitchOnClick', LLnotice);
  readAndCheckDefault;
  readAndCheckSwitch(True);
  getClientIdsList;
  updateConfigStates;
end;

procedure TForm1.BtnSwitchOffClick(Sender: TObject);
begin
  LogDatei.log('BtnSwitchOffClick', LLnotice);
  readAndCheckDefault;
  readAndCheckSwitch(False);
  getClientIdsList;
  updateConfigStates;
end;

procedure TForm1.BitBtnConnectClick(Sender: TObject);
begin
  if startOpsiServiceConnection then
  begin
    try
      Screen.Cursor := crHourGlass;
      readserver;
      //readgroups;
      //readclients;
      BtnSwitchOff.Enabled := True;
      BtnSwitchOn.Enabled := True;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TForm1.CBoxRulesChange(Sender: TObject);
var
  //myinifilename : string;
  myIni: TIniFile;
begin
  myinifilename := CBoxRules.Items[CBoxRules.ItemIndex];
  myinifilename := ExtractFileDir(ParamStr(0)) + PathDelim +
    'opsi-config-rules.d' + PathDelim + myinifilename + '.ini';
  if FileExists(myinifilename) then
  begin
    try
      myIni := TIniFile.Create(myinifilename);
      memo1.Text := myIni.ReadString('general', 'description', '');
    finally
      myIni.Free;
    end;
  end;
end;

procedure TForm1.MAboutClick(Sender: TObject);
var
  progname: string;
  msg: string;
  list: TStringList;
begin
  list := TStringList.Create;
  progname := ExtractFileName(ParamStr(0));
  msg := progname + ' Version: ' + myVersion;
  list.Add(msg);
  list.Add('(c) uib gmbh under AGPLv3');
  list.Add('This is a part of the opsi.org project: https://opsi.org');
  list.Add('');
  list.add('Icons from Iconic (https://useiconic.com/) under MIT License.');
  //list.add('https://github.com/iconic/open-iconic/blob/master/ICON-LICENSE');
  list.Add('');
  list.Add('Configuration: ');
  list.Add(aktconfigfile);
  list.Add('Log: ');
  list.Add(logdatei.FileName);
  MyMessageDlg.ShowMessage('opsi-config-rules-manager', list.Text, [mrOk]);
  //ShowMessage(list.Text);
  list.Free;
end;

procedure TForm1.MConfigurationClick(Sender: TObject);
var
  Streamer: TJSONStreamer;
  JSONString: string;
begin
  logdatei.log('Start MenuItemConfigClick', LLDebug2);
  FOSDConfigdlg.ShowModal;
  logdatei.log('After configdialog: create jsonstr', LLDebug2);
  //FOSDConfigdlg.Hide;
  Streamer := TJSONStreamer.Create(nil);
  try
    Streamer.Options := Streamer.Options + [jsoTStringsAsArray];
    JSONString := Streamer.ObjectToJSONString(myconfiguration);
    logdatei.log('After configdialog: ' + JSONString, LLDebug2);
    myconfiguration.writeconfig;
  finally
    Streamer.Destroy;
  end;
  logdatei.log('Finished MenuItemConfigClick', LLDebug2);
end;

procedure TForm1.MExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if localservicedata = nil then
    if myconfiguration.Service_pass <> '' then BitBtnConnectClick(nil);
end;

procedure addClientsFromNode(myNode: TTreeNode);
var
  myNewNode: TTreeNode;
  str: string;
  i: integer;
begin
  for i := 0 to myNode.Count - 1 do
  begin
    if mynode.Items[i].HasChildren then
      // recursive call
      addClientsFromNode(mynode.Items[i])
    else
    // check if it is really a client
    // a client has ImageIndex = 0 (see procedure readclients)
    if mynode.Items[i].ImageIndex = 0 then
      form1.ListBoxClient.Items.Add(mynode.Items[i].Text);
  end;
  //logdatei.log('myNode.Count: '+intToStr(myNode.Count), LLDebug2);
end;

procedure TForm1.TreeView1Click(Sender: TObject);
var
  myNode, myNewNode: TTreeNode;
  str: string;
  i: integer;
begin
  ListBoxClient.Items.Clear;
  myNode := TTreeview(Sender).Items.GetSelections(0);
  addClientsFromNode(mynode);
  (*
  str := myNode.Text;
  for i := 0 to myNode.Count - 1 do
  begin
    ListBoxClient.Items.Add(mynode.Items[i].Text);
  end;
  logdatei.log('myNode.Count: '+intToStr(myNode.Count), LLDebug2);
  *)
end;

procedure TForm1.TreeView1DblClick(Sender: TObject);
var
  myNode, myNewNode: TTreeNode;
  str: string;
  i: integer;
begin
  ListBoxClient.Items.Clear;
  myNode := TTreeview(Sender).Items.GetSelections(0);
  addClientsFromNode(mynode);
  ListBoxClient.SelectAll;
  (*
  str := myNode.Text;
  for i := 0 to myNode.Count - 1 do
    ListBoxClient.Items.Add(mynode.Items[i].Text);
  ListBoxClient.SelectAll;
  logdatei.log('myNode.Count: '+intToStr(myNode.Count), LLDebug2);
  *)
end;

procedure TForm1.TreeView1SelectionChanged(Sender: TObject);
begin

end;

function startOpsiServiceConnection: boolean;
var
  i: integer;
  //passwordToUse: string; is a global var
  strlist: TStringList;
  sessionid: string;
begin
  LogDatei.log('startOpsiServiceConnection', LLnotice);
  if localservicedata = nil then
  begin
    try
      strlist := TStringList.Create;
      localservicedata := TOpsi4Data.Create;
      if (myconfiguration.Service_URL <> '') and
        (myconfiguration.Service_user <> '') then
      begin
        if passwordToUse = '' then
          passwordToUse := myconfiguration.Service_pass;
        if passwordToUse = '' then
          passwordToUse :=
            PasswordBox('service: ' + myconfiguration.Service_URL +
            ' user: ' + myconfiguration.Service_user, 'Password for opsi web service');
        Screen.Cursor := crHourGlass;
        //procmess;
        localservicedata.initOpsiConf(myconfiguration.Service_URL,
          myconfiguration.Service_user,
          passwordToUse);
        opsiserviceversion := oswebservice.getOpsiServerVersion(
          myconfiguration.Service_URL, myconfiguration.Service_user,
          passwordToUse, sessionid);
        if localservicedata.isConnected then
        begin
          LogDatei.log('Service connection initialized to :' +
            myconfiguration.Service_URL + ' version: ' + opsiserviceversion, LLinfo);
          Form1.StatusBar1.Panels.Items[0].Text :=
            'Connected to opsi server: ' + myconfiguration.Service_URL;
          // fetch produtIds from service
          strlist.Text := localservicedata.getLocalbootProductIds.Text;
        end
        else
        begin
          // service not connected
          MessageDlg('opsi-setup-detctor', rsServiceConnectionFailed,
            mtError, [mbOK], '');
          LogDatei.log('Service connection not possible: Url, user or password wrong.',
            LLwarning);
          Form1.StatusBar1.Panels.Items[0].Text := 'Not connected to opsi server';
          FreeAndNil(localservicedata);
        end;
      end
      else
      begin
        // service data missing
        LogDatei.log('Service connection not possible: Url or user missing.', LLwarning);
        Form1.StatusBar1.Panels.Items[0].Text := 'Not connected to opsi server';
        FreeAndNil(localservicedata);
      end;
    finally

      FreeAndNil(strlist);
      Screen.Cursor := crDefault;
      ;
    end;
  end;
  Application.ProcessMessages;
end;

procedure readserver;
var
  method: string;
  params: array of string;
  LogErrorMessage, serviceresult: string;
  jsonstr, tmpstr: string;
  i: integer;
begin
  LogDatei.log('readserver', LLnotice);
  serviceresult := '';
  //result := false;
  method := 'host_getIdents';
  params := ['', '{"type":"OpsiConfigServer"}'];
  LogErrorMessage :=
    'Warning: Could not get config defaults from service (oswebservice: TOpsi4Data.getConfigObjectsFromService)';
  serviceresult := localservicedata.getJSONFromService(method, params, logErrorMessage);
  LogDatei.log('Serviceresult from config_getObjects: ' + serviceresult, LLdebug);
  jsonAsObjectGetValueByKey(serviceresult, 'result', jsonstr);
  if jsonAsArrayCountElements(jsonstr) > 0 then
  begin
    for i := 0 to jsonAsArrayCountElements(jsonstr) - 1 do
    begin
      jsonAsArrayGetElementByIndex(jsonstr, i, tmpstr);
      form1.ListBoxServer.Items.Add(tmpstr);
      LogDatei.log('Add Server: ' + tmpstr, LLDebug);
    end;
  end;
  params := ['', '{"type":"OpsiDepotServer"}'];
  serviceresult := localservicedata.getJSONFromService(method, params, logErrorMessage);
  LogDatei.log('Serviceresult from config_getObjects: ' + serviceresult, LLdebug);
  jsonAsObjectGetValueByKey(serviceresult, 'result', jsonstr);
  if jsonAsArrayCountElements(jsonstr) > 0 then
  begin
    for i := 0 to jsonAsArrayCountElements(jsonstr) - 1 do
    begin
      jsonAsArrayGetElementByIndex(jsonstr, i, tmpstr);
      form1.ListBoxServer.Items.Add(tmpstr);
      LogDatei.log('Add Server: ' + tmpstr, LLDebug);
    end;
  end;
  // make first server selected
  form1.ListBoxServer.ItemIndex := 0;
  //form1.ListBoxServer.SelectRange(0,0,true);
  Application.ProcessMessages;
end;

procedure readgroups;
var
  method: string;
  params: array of string;
  LogErrorMessage, serviceresult: string;
  jsonstr, tmpstr, parent, Name: string;
  i: integer;
  myNode, myNewNode: TTreeNode;
begin
  try
    Screen.Cursor := crHourGlass;
    Form1.StatusBar1.Panels.Items[1].Text := 'Reading group data';
    Application.ProcessMessages;
    form1.TreeView1.Items.Clear;
    LogDatei.log('readgroups', LLnotice);
    myNode := Form1.TreeView1.Items.Add(nil, 'Gruppen');
    myNode.Text := 'Gruppen';
    myNode := Form1.TreeView1.Items.Add(nil, 'clientdirectory');
    myNode.Text := 'clientdirectory';
    serviceresult := '';
    //result := false;
    method := 'group_getObjects';
    params := ['', '{"type":"HostGroup"}'];
    LogErrorMessage := 'Warning: Could not group_getObjects from service';
    serviceresult := localservicedata.getJSONFromService(method, params, logErrorMessage);
    LogDatei.log('Serviceresult from readgroups: ' + serviceresult, LLdebug);
    jsonAsObjectGetValueByKey(serviceresult, 'result', jsonstr);
    if jsonAsArrayCountElements(jsonstr) > 0 then
    begin
      for i := 0 to jsonAsArrayCountElements(jsonstr) - 1 do
      begin
        jsonAsArrayGetElementByIndex(jsonstr, i, tmpstr);
        jsonAsObjectGetValueByKey(tmpstr, 'parentGroupId', parent);
        jsonAsObjectGetValueByKey(tmpstr, 'id', Name);
        myNode.FreeAllNodeData;
        myNode := nil;
        myNode := form1.TreeView1.Items.FindTopLvlNode(parent);
        if myNode = nil then
          myNode := form1.TreeView1.Items.FindNodeWithText(parent);
        if myNode = nil then
          myNode := form1.TreeView1.Items.FindNodeWithText('Gruppen');
        myNewNode := form1.TreeView1.Items.AddChild(myNode, Name);
        myNewNode.Text := Name;
        myNewNode.ImageIndex := 2;
        myNewNode.SelectedIndex := 2;
        myNewNode.StateIndex := 3;
        //LogDatei.log('Add Group: ' + name + ' as child of: '+myNode.Text, LLDebug);
        LogDatei.log('Add Group: ' + Name, LLDebug);
        if myNode <> nil then LogDatei.log(' as child of: ' + myNode.Text, LLDebug);
      end;
    end;
  finally
    Screen.Cursor := crDefault;
    Form1.StatusBar1.Panels.Items[1].Text := 'Reading client data';
  end;
  Application.ProcessMessages;
end;

procedure readclients;
var
  method: string;
  params: array of string;
  LogErrorMessage, serviceresult: string;
  jsonstr, tmpstr, parent, Name, json2str, groupid, otgObj, clientid: string;
  jsonObjStr, depotid: string;
  i, k: integer;
  myNode, myNewNode: TTreeNode;
  selectedServerList: TStringList;
begin
  LogDatei.log('readclients', LLnotice);
  try
    Screen.Cursor := crHourGlass;
    Form1.StatusBar1.Panels.Items[1].Text := 'Reading client data';
    Application.ProcessMessages;
    allClientIdsList.Clear;
    form1.ListBoxClient.Clear;
    selectedServerList := TStringList.Create;
    serviceresult := '';
    // configState_getClientToDepotserver ["bonifax.uib.local"]
  (*
  method :=  'host_getIdents';
  params := ['', '{"type":"OpsiClient"}'];
  *)
    method := 'configState_getClientToDepotserver';
    // get selected servers
    (*
    for i := 0 to Form1.ListBoxServer.Count - 1 do
      if Form1.ListBoxServer.Selected[i] then
      begin
        Name := Form1.ListBoxServer.Items[i];
        selectedServerList.Add(Name);
        LogDatei.log('Selected server: ' + Name, LLdebug);
      end;
     *)
    tmpstr := Form1.ListBoxServer.Items[Form1.ListBoxServer.ItemIndex];
    selectedServerList.Add(tmpstr);
    if stringListToJsonArray(selectedServerList, jsonstr) then
    begin
      params := [jsonstr];
    end;
    for i := 0 to length(params) - 1 do
      params[i] := opsiunquotestr2(params[i], '"');
    LogErrorMessage := 'Warning: Could not host_getObjects from service';
    serviceresult := localservicedata.getJSONFromService(method,
      params, logErrorMessage);
    LogDatei.log('Serviceresult from configState_getClientToDepotserver in readclients: '
      + serviceresult, LLdebug);
    jsonAsObjectGetValueByKey(serviceresult, 'result', jsonstr);
    if jsonAsArrayCountElements(jsonstr) > 0 then
    begin
      for i := 0 to jsonAsArrayCountElements(jsonstr) - 1 do
      begin
        jsonAsArrayGetElementByIndex(jsonstr, i, jsonObjStr);
        jsonAsObjectGetValueByKey(jsonObjStr, 'depotId', depotid);
        tmpstr := selectedServerList.Strings[0];
        if selectedServerList.IndexOf(depotid) >= 0 then
        begin
          jsonAsObjectGetValueByKey(jsonObjStr, 'clientId', clientid);
          LogDatei.log('Found clientId: ' + clientid, LLdebug);
          allClientIdsList.Add(clientid);
          serviceresult := '';
          method := 'objectToGroup_getObjects';
          params := ['', '{"groupType":"HostGroup","objectId":"' + clientid + '"}'];
          LogErrorMessage := 'Warning: Could not objectToGroup_getObjects from service';
          serviceresult := localservicedata.getJSONFromService(method,
            params, logErrorMessage);
          LogDatei.log('Serviceresult from readgroups: ' + serviceresult, LLdebug);
          jsonAsObjectGetValueByKey(serviceresult, 'result', json2str);
          if jsonAsArrayCountElements(json2str) > 0 then
          begin
            for k := 0 to jsonAsArrayCountElements(json2str) - 1 do
            begin

              jsonAsArrayGetElementByIndex(json2str, k, otgObj);
              jsonAsObjectGetValueByKey(otgObj, 'groupId', groupid);
              myNode := form1.TreeView1.Items.FindTopLvlNode(groupid);
              if myNode = nil then
                myNode := form1.TreeView1.Items.FindNodeWithText(groupid);
              if myNode = nil then
                myNode := form1.TreeView1.Items.FindNodeWithText('Gruppen');
              myNewNode := form1.TreeView1.Items.AddChild(myNode, clientid);
              myNewNode.Text := clientid;
              myNewNode.ImageIndex := 0;
              myNewNode.SelectedIndex := 0;
              myNewNode.StateIndex := 3;
              //LogDatei.log('Add Group: ' + name + ' as child of: '+myNode.Text, LLDebug);
              LogDatei.log('Add clientid: ' + clientid, LLDebug);
              if myNode <> nil then
                LogDatei.log(' as child of: ' + myNode.Text, LLDebug);
            end;
          end;
        end;
      end;
      form1.ListBoxClient.Items.AddStrings(allClientIdsList);
    end;

  finally
    Form1.StatusBar1.Panels.Items[1].Text := '';
    FreeAndNil(selectedServerList);
    Screen.Cursor := crDefault;
  end;
  Application.ProcessMessages;
end;

end.
