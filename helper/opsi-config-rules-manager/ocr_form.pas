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
  ocrbasedata,
  ocrconfigdlg,
  oswebservice,
  osmessagedialog,
  oslog,
  osjson;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnSwitchOn: TBitBtn;
    BtnSwitchOff: TBitBtn;
    CBoxRules: TComboBox;
    Edit1: TEdit;
    FlowPanel1: TFlowPanel;
    FlowPanel2: TFlowPanel;
    Label1: TLabel;
    Label2: TLabel;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MExit: TMenuItem;
    MFile: TMenuItem;
    MAbout: TMenuItem;
    MConfiguration: TMenuItem;
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    TreeView1: TTreeView;
    procedure BtnSwitchOffClick(Sender: TObject);
    procedure BtnSwitchOnClick(Sender: TObject);
    procedure CBoxRulesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MAboutClick(Sender: TObject);
    procedure MConfigurationClick(Sender: TObject);
    procedure MExitClick(Sender: TObject);
  private

  public

  end;

  function startOpsiServiceConnection: boolean;

var
  Form1: TForm1;
  localservicedata: TOpsi4Data = nil;
  passwordToUse : string;
  opsiserviceversion, sessionid : string;
  defaultConfigList : TStringlist;
  configIdsList : TStringlist;
  configSwitchList : TStringlist;
  clientIdsList : TStringlist;
  myinifilename : string;

resourcestring
  rsServiceConnectionFailed =
    'Could not connect to the opsi-web-service. Check URL, user and password';


implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  ruleslist : TStringlist;
  i : integer;
begin
  try
    ruleslist :=  FindAllFiles(ExtractFileDir(paramstr(0))+PathDelim+'opsi-config-rules.d','*.ini',false);
    for i := 0 to  ruleslist.Count -1 do
      ruleslist[i] := ExtractFileNameOnly(ruleslist[i]);
    CBoxRules.Items.AddStrings(ruleslist);
    CBoxRules.ItemIndex:=0;

  finally
    FreeAndNil(ruleslist)
  end;
  startOpsiServiceConnection;
  defaultConfiglist := Tstringlist.Create;
  configIdsList := TStringlist.Create;
  configSwitchList := TStringlist.Create;
  clientIdsList := TStringlist.Create;
end;

procedure switchConfig(switchList : TStringlist; clientIdsList : Tstringlist);
begin
end;

function configExists(configId : string) : boolean;
var
  method: string;
  params: array of string;
  LogErrorMessage, serviceresult: string;
begin
  serviceresult := '';
  result := false;
  method :=  'config_getObjects';
  params := ['', '{"id":"' + configId + '"}'];
  LogErrorMessage := 'Warning: Could not get config defaults from service (oswebservice: TOpsi4Data.getConfigObjectsFromService)';
  serviceresult := localservicedata.getJSONFromService(method, params, logErrorMessage);
  if jsonAsArrayCountElements(serviceresult) > 0 then result := true;
end;

procedure createConfig(configId : string; configVal : string; isBool : boolean);
var
  method: string;
  params: array of string;
  LogErrorMessage, serviceresult: string;
begin
  // method config_createBool id *description *defaultValues
  // method config_createUnicode id *description *possibleValues *defaultValues *editable *multiValue
  serviceresult := '';
  if isBool then
  begin
    method :=  'config_createBool';
    params := ['', '{"id":"' + configId + '","defaultValues":['+configVal+']}'];
  end
  else
  begin
    method :=  'config_createUnicode';
  end;
  LogErrorMessage := 'Warning: Could not create config defaults with service method: '+method;
  serviceresult := localservicedata.getJSONFromService(method, params, logErrorMessage);
  //if jsonAsArrayCountElements(serviceresult) > 0 then result := true;
end;

procedure checkConfig(configId : string; configVal : string; isBool : boolean; defaultValue : string);
var
  key, val : string;

begin
  if not configExists(configId) then
  begin
    createConfig(configId, configVal, isBool);
  end
  else LogDatei.log('config: '+configId+' already exists.',LLdebug);
end;

procedure readAndCheckDefault;
var
  clientIdsList : Tstringlist;
  clientId : string;
  ruleId : string;
  //myinifilename : string;
  myIni : TIniFile ;
  i : integer;
  id, val : string;
  isBool : boolean;
begin
  //myinifilename := CBoxRules.SelText;
  //myinifilename := ExtractFileDir(paramstr(0))+PathDelim+'opsi-config-rules.d'
  //                  +PathDelim+myinifilename + '.ini';
  if FileExists(myinifilename) then
  begin
    myIni := TIniFile.Create(myinifilename);
    myIni.ReadSectionValues('default',defaultConfiglist);
    myIni.ReadSection('default',configIdsList);
    //memo1.Text:= myIni.ReadString('general','description');
  end;
  myIni.Free;
  for i := 0 to configIdsList.Count -1 do
    begin
      id :=  configIdsList.strings[i];
      val := trim(defaultConfiglist.Values[id]);
      if  (LowerCase(val) = 'true') or (LowerCase(val) = 'false') then
      isBool:= true
      else isBool:= false;
      checkConfig(id, val, isBool, val);
    end;
end;

procedure addConfigState(configId, val, clientId : string);
begin

end;

procedure readAndCheckSwitch(switchOn : boolean);
var
  clientIdsList : Tstringlist;
  clientId : string;
  ruleId : string;
  //myinifilename : string;
  myIni : TIniFile ;
  i : integer;
  id, val : string;
  isBool : boolean;
  sectionName : string;
begin
  if switchOn then sectionName := 'switch_on'
  else sectionName := 'switch_off';
  //myinifilename := CBoxRules.SelText;
  //myinifilename := ExtractFileDir(paramstr(0))+PathDelim+'opsi-config-rules.d'
  //                  +PathDelim+myinifilename + '.ini';
  if FileExists(myinifilename) then
  begin
    myIni := TIniFile.Create(myinifilename);
    myIni.ReadSectionValues(sectionName,configSwitchList);
    //myIni.ReadSection(sectionName,configIdsList);
    //memo1.Text:= myIni.ReadString('general','description');
  end;
  myIni.Free;
end;

procedure getClientIdsList;
begin

end;

procedure updateConfigStates;
var
  i, k : integer;
  id, val, clientid : string;
begin
  for i := 0 to configIdsList.Count -1 do
    for k := 0 to clientIdsList.Count -1 do
    begin
      id :=  configIdsList.strings[i];
      val := trim(configSwitchList.Values[id]);
      clientid :=  clientIdsList.Strings[k];
      addConfigState(id, val, clientid);
    end;
end;

procedure TForm1.BtnSwitchOnClick(Sender: TObject);
begin
  readAndCheckDefault;
  readAndCheckSwitch(true);
  getClientIdsList;
  updateConfigStates;
end;

procedure TForm1.BtnSwitchOffClick(Sender: TObject);
begin
  readAndCheckDefault;
  readAndCheckSwitch(false);
  getClientIdsList;
  updateConfigStates;
end;

procedure TForm1.CBoxRulesChange(Sender: TObject);
var
  //myinifilename : string;
  myIni : TIniFile ;
begin
  myinifilename := CBoxRules.Items[CBoxRules.ItemIndex];
  myinifilename := ExtractFileDir(paramstr(0))+PathDelim+'opsi-config-rules.d'
                    +PathDelim+myinifilename + '.ini';
  if FileExists(myinifilename) then
  begin
    try
      myIni := TIniFile.Create(myinifilename);
      memo1.Text:= myIni.ReadString('general','description','');
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
  (*
  if fileexists(myconfiguration.PathToOpsiPackageBuilder) then
  begin
    logdatei.log('After configdialog: packagebuilder exists', LLDebug2);
    RadioButtonBuildPackage.Enabled := True;
    RadioButtonPackageBuilder.Enabled := True;
    CheckGroupBuildMode.Enabled := True;
  end
  else
  begin
    logdatei.log('After configdialog: packagebuilder not found', LLDebug2);
    RadioButtonBuildPackage.Enabled := False;
    RadioButtonPackageBuilder.Enabled := False;
    CheckGroupBuildMode.Enabled := False;
  end;
  *)
  logdatei.log('Finished MenuItemConfigClick', LLDebug2);
end;

procedure TForm1.MExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

function startOpsiServiceConnection: boolean;
var
  i: integer;
  //passwordToUse: string; is a global var
  strlist: TStringList;
  sessionid: string;
begin
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
          Form1.StatusBar1.Panels.Items[1].Text :=
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
          Form1.StatusBar1.Panels.Items[1].Text := 'Not connected to opsi server';
          FreeAndNil(localservicedata);
        end;
      end
      else
      begin
        // service data missing
        LogDatei.log('Service connection not possible: Url or user missing.', LLwarning);
        Form1.StatusBar1.Panels.Items[1].Text := 'Not connected to opsi server';
        FreeAndNil(localservicedata);
      end;
    finally

      FreeAndNil(strlist);
      Screen.Cursor := crDefault;
      ;
    end;
  end;
end;


end.

