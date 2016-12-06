unit oswebservice;

{$MODE Delphi}
//{$RANGECHECKS ON}
{$OVERFLOWCHECKS ON}
{$IOCHECKS ON}
{$OBJECTCHECKS ON}
{$VARSTRINGCHECKS ON}
{$LONGSTRINGS ON}



// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: Rupert Roeder, detlef oertel
// credits: http://www.opsi.org/credits/

//***************************************************************************
// Subversion:
// $Revision: 500 $
// $Author: oertel $
// $Date: 2016-10-05 18:42:17 +0200 (Mi, 05 Okt 2016) $
//***************************************************************************


interface

uses
  SysUtils, Classes, Variants,
  //IdComponent,
  IdHTTP,
  IdWebDAV,
  oslog,
  superobject,
  IdIOHandler,
  IdSSLOpenSSL,
  //IdSocketHandle,
  synacode,
  TypInfo,
  {$IFDEF GUI}
  Forms,
  {$ENDIF GUI}
  {$IFDEF OPSIWINST}
  osfunc,
  osparser,
  osconf,
  //lconvencoding,
  //utf8scanner,
  //Character,
  //utf8info,
  {$ENDIF OPSIWINST}

{$IFDEF WINDOWS}
  Windows,
{$ENDIF WINDOWS}
  //widatahelper,
  zstream;
//LCLIntf,
//LResources,
//DefaultTranslator;
//;


{
   COPYRIGHT NOTICE

 OpenSSL libraries for Indy
***********************************
Copyright 2000-2002,
Gregor Ibic, gregor.ibic@intelicom.si
Intelicom d.o.o.
www.intelicom.si


This library is tested on Indy 9.0 and higher.
Support forum is avaiable at http://www.intelicom.si/forum

Please note, that you have to specify the usage of Indy and IndySSL with a copyright notice
in your applications.

Comments and suggestions are welcome to my email.
***************************************

}

const

  nameRequirementTypeBefore: string = 'before';
  nameRequirementTypeAfter: string = 'after';
  nameRequirementTypeUndefined: string = '';

  logtype = 'instlog';

type
  //TArtProduktbehandlung = (tapSet, tapSetup, tapDeinstall, tapUpdate, tapOnce, tapAlways, tapNull);
  TAction = (tacNull,
    tacSetup,
    tacUpdate,
    tacDeinstall,
    tacAlways,
    tacOnce,
    tacCustom,
    tacLogin);

  TActionRequest = (tapNull, tapUndefined, tapNull_byPolicy,
    tapSetup, tapSetup_byPolicy,
    tapUpdate, tapUpdate_byPolicy,
    tapDeinstall, tapDeinstall_byPolicy,
    tapAlways,
    tapOnce,
    tap_byPolicy,
    tapCustom,
    tapLogin);

  TActionRequest4 = (tac4None,
    tac4Setup,
    tac4Update,
    tac4Uninstall,
    tac4Always,
    tac4Once,
    tac4Custom,
    tac4Login);

  TProductstate4 = (tps4Installed,
    tps4Not_installed,
    tps4Unkown);

  TTargetConfiguration4 = (ttc4Installed,
    ttc4Forbidden,
    ttc4Always,
    ttc4undefined);

  TActionResult4 = (tar4None, tar4Failed, tar4Successful);


  TProductState = (tpsUndefined, tpsNotInstalled, tpsInstalling,
    tpsInstalled, tpsFailed);
  //  TProductState4 = (tpsUndefined, tpsNotInstalled, tpsInstalled);
  //  TProductProgress = (tppUndefined, tppInstalling, tppNone);
  //  TProductResult = (tprNone, tprFailed, tprSuccessful);

  TString = class(TObject)
  private
    fStr: string;
  public
    constructor Create(const AStr: string);
    property Str: string read FStr write FStr;
  end;


  TOpsiData = class

  protected

    actualProduct: string;
    actualVersion: string;
    actualClient: string;
    options: string;
    FSortByServer: boolean;

  public

    constructor Create;

    procedure saveOpsiConf; virtual; abstract;
    procedure finishOpsiConf; virtual;

    function stateToString(state: TProductState): string; virtual; abstract;
    function actionToString(action: TAction): string; virtual; abstract;
    function actionRequestToString(actionRequest: TActionRequest): string;
      virtual; abstract;

    function getLogFileName(const LogFilename: string): string; virtual; abstract;

    function UpdateSwitches(extremeErrorLevel: TErrorLevel): boolean; virtual;

    procedure setActualClient(computername: string); virtual; abstract;
    procedure setOptions(serviceoptions: string);
    procedure setActualProductName(const productname: string); virtual;
    function getActualProductName: string; virtual;
    function getActualProductVersion: string; virtual;

    function initProduct: boolean; virtual; abstract;
    procedure finishProduct; virtual; abstract;

    function getSpecialScriptPath: string; virtual;

    function getProductproperties: TStringList; virtual; abstract;

    function getBeforeRequirements: TStringList; virtual; abstract;
    function getAfterRequirements: TStringList; virtual; abstract;

    function getProductState: TProductState; virtual; abstract;
    function getProductAction: TAction; virtual; abstract;
    function getProductActionRequest: TActionRequest; virtual; abstract;

    function getListOfProducts: TStringList; virtual; abstract;

    function getProductScriptPath(actionType: TAction): string;
      virtual; abstract;

    function getInstallationPriority: integer; virtual; abstract;

    procedure setProductActionRequest(newAction: TActionRequest); virtual; abstract;
    procedure setProductState(newState: TProductState); virtual; abstract;

    procedure setProductStateActionRequest(newState: TProductState;
      newActiont: TActionRequest);
      virtual; abstract;


    property sortByServer: boolean read FSortByServer;

    function withLicenceManagement: boolean; virtual;
    function getOpsiServiceVersion: string; virtual; abstract;

  end;

  // opsidata-Service

  // helper classes

  TOpsiMethodCall = class(TObject)
  protected
    FOpsiMethodName: string;
    FParameterlist: TStringList;
    Fhashlist: TStringList;

  public

    constructor Create(const method: string; parameters: array of string); overload;
    constructor Create(const method: string; hashlist: TStringList;
      parameters: array of string); overload;
    destructor Destroy; override;

    function getJsonUrlString: string;
    function getJsonHashListString: string;

    property opsimethodname: string read FOpsiMethodname write FOpsiMethodname;
    property parameterlist: TStringList read FParameterlist write FParameterlist;
    property hashlist: TStringList read Fhashlist write Fhashlist;
    property jsonUrlString: string read getJsonUrlString;

  end;

  TJsonThroughHTTPS = class(TObject)
  private
    FResultLines: TStringList;
    mymemorystream: TMemoryStream;
    //nullO : NULL;
    methodGET: boolean;
  protected
    IdSSLIOHandlerSocket: TIdSSLIOHandlerSocketOpenSSL;

    //IdHTTP: TIdHTTP;
    IdHTTP: TIdWebDAV;
    portHTTP: integer;
    portHTTPS: integer;
    Furl: string;
    FServiceURL: string;
    Fusername: string;
    Fpassword: string;
    FError: string;
    FErrorInfo: TStringList;
    FSessionId: string;

    procedure makeURL(const omc: TOpsiMethodCall);
    procedure createSocket; overload;
    procedure createSocket(const agent, ip, port: string); overload;

  public
    constructor Create(const serviceURL, username, password: string); overload;
    constructor Create(const serviceURL, username, password, sessionid: string);
      overload;
    //constructor Create(const serviceURL, username, password, sessionid: string;
    //port: integer); overload;
    constructor Create(const serviceURL, username, password, sessionid,
      ip, port: string); overload;
    destructor Destroy; override;
    function retrieveJSONObject(const omc: TOpsiMethodCall;
      logging: boolean; retry: boolean; readOmcMap: boolean): ISuperObject; overload;
    function retrieveJSONObject(const omc: TOpsiMethodCall;
      logging: boolean; retry: boolean): ISuperObject; overload;
    function retrieveJSONObject(const omc: TOpsiMethodCall;
      logging: boolean): ISuperObject; overload;
    function retrieveJSONObject(const omc: TOpsiMethodCall): ISuperObject; overload;
    function retrieveJSONArray(const omc: TOpsiMethodCall; logging: boolean;
      retry: boolean; readOmcMap: boolean): TSuperArray; overload;
    function retrieveJSONArray(const omc: TOpsiMethodCall; logging: boolean;
      retry: boolean): TSuperArray; overload;
    function retrieveJSONArray(const omc: TOpsiMethodCall;
      logging: boolean): TSuperArray; overload;
    function retrieveJSONArray(const omc: TOpsiMethodCall): TSuperArray; overload;

    function retrieveJSONObjectByHttpPost(const instream: TMemoryStream;
      logging: boolean): ISuperObject;

    function getMapResult(const omc: TOpsiMethodCall): TStringList;
    function getHashListResult(const omc: TOpsiMethodCall): TStringList;
    function getListResult(const omc: TOpsiMethodCall): TStringList;
    function getStringListResult(const omc: TOpsiMethodCall): TStringList;
    function getSubListResult(const omc: TOpsiMethodCall; subkey: string): TStringList;

    function getFileFromDepot(filename: string; toStringList: boolean;
      var ListResult: TStringList): boolean;

    property resultLines: TStringList read FResultLines;
    property LastError: string read FError;
    property LastErrorInfo: TStringList read FErrorInfo;
  end;


  TOpsi4Data = class(TOpsiData)

  private
    FjsonExecutioner: TJsonThroughHTTPS;
    ProfildateiChange: boolean;
    FServiceUrl: string;
    FDepotId: string;
    //actualClient: string;

    mapOfMethodSignatures: TStringList;
    //AllProductsDependencies: TStringList;
    allDependencies: TStringList;
    //ProductDependencies : TList;
    FPreRequirements: TStringList;
    FPostRequirements: TStringList;
    FProductStates: TStringList;
    FProductActionRequests: TStringList;
    FInstallableProducts: TStringList;
    ProductVars: TStringList;
    FOpsiModules: ISuperObject;
    FOpsiInformation: ISuperObject;
    FOpsiServiceVersion: string;
    //FactualClient: string;

    FProductOnClient_objects: TSuperArray;
    FProductOnClient_aktobject: ISuperObject;
    FProductOnClientIndex: TStringList;
    FSslProtocol: TIdSSLVersion;

    mylist: TStringList;


    //Function getMapOfProductSwitches : TStringList;
    //Function getProductRequirements (requirementType : String) : TStringList;
    function getProductRequirements(productname: string;
      requirementType: string): TStringList;

    function getMapOfProductStates: TStringList;
    function productonClients_getObjects__actionrequests: TStringList;
    procedure productOnClient_getobject_actualclient;
    function getInstallableProducts: TStringList;
    function getOpsiModules: ISuperObject;

  protected
    FServiceLastErrorInfo: TStringList;
    //FactualClient: string;

  public
    //actualClient: string;
    constructor Create;
    destructor Destroy; override;

    procedure initOpsiConf(serviceURL, username, password: string); overload;
    procedure initOpsiConf(serviceURL, username, password, sessionid: string); overload;
    procedure initOpsiConf(serviceURL, username, password, sessionid, ip, port: string);
      overload;

    property sslProtocol: TIdSSLVersion read FSslProtocol write FSslProtocol;
    property serviceUrl: string read FServiceURL;
    property depotId: string read FDepotId;
    //property actualclient: string read FactualClient write FactualClient;

    procedure saveOpsiConf; override;
    procedure finishOpsiConf; override;

    function checkAndRetrieve(const omc: TOpsiMethodCall;
      var errorOccured: boolean): string;
    //returns the retrieved JSON object as string

    function checkAndRetrieveMap(const omc: TOpsiMethodCall;
      var errorOccured: boolean): TStringList;
    function checkAndRetrieveHashList(const omc: TOpsiMethodCall;
      var errorOccured: boolean): TStringList;
    function checkAndRetrieveList(const omc: TOpsiMethodCall;
      var errorOccured: boolean): TStringList; virtual;

    function checkAndRetrieveStringList(const omc: TOpsiMethodCall;
      var errorOccured: boolean): TStringList;
    function checkAndRetrieveString(const omc: TOpsiMethodCall;
      var errorOccured: boolean): string;

    function getLogFileName(const LogFilename: string): string; override;
    //uses the parameter LogFilename as default

    function sendLog: boolean; overload;
    function sendLog(logtype: string): boolean; overload;
    function sendLog(logtype: string; appendmode: boolean): boolean; overload;

    function stateToString(state: TProductState): string; override;
    function actionToString(action: TAction): string; override;
    function actionRequestToString(actionRequest: TActionRequest): string;
      overload; override;

    function stateStringToState(s: string): TProductState;
    function actionRequestStringToActionRequest(s: string): TActionRequest;
    function actionRequestStringToAction(s: string): TAction;

    procedure setActualClient(computername: string); override;
    procedure setActualProductName(const productname: string); override;
    procedure setActionProgress(const progress: string);

    function getMethodSignature(const methodname: string): TStringList;

    function initProduct: boolean; override;
    procedure finishProduct; override;
    function getActualProductVersion: string; override;
    function getSpecialScriptPath: string; override;

    function getProductproperties: TStringList; override;

    function getBeforeRequirements: TStringList; override;
    function getAfterRequirements: TStringList; override;


    function getListOfProducts: TStringList; override;
    function setAddDependentProductOnClients(switchon: boolean): boolean;
    function setAddProductPropertyStateDefaults(switchon: boolean): boolean;
    function setAddProductOnClientDefaults(switchon: boolean): boolean;
    function getProductPropertyList(myproperty: string;
      defaultlist: TStringList): TStringList;  overload;
    function getProductPropertyList(myproperty: string;
        defaultlist: TStringList;
        myClientId : string;
        myProductId: string): TStringList;  overload;
    // getInstallableLocalBootProductIds_list

    function getProductState: TProductState; override;
    function getProductAction: TAction; override;
    function getProductActionRequest: TActionRequest; override;
    function getProductPackagageVersion: string; virtual;
    function getOpsiServiceVersion: string; override;
    function getActualProductDescription: string;
    function getActualProductAdvice: string;
    function getActualProductPriority: string;
    function getActualProductPackageVersion: string;
    function getActualProductId: string;
    function getActualProductName: string; override;
    function getActualProductProductVersion: string;
    function getActualProductInstallationState: string;
    function getActualProductLastActionRequest: string;
    function getActualProductProductLastActionReport: string;
    function getActualProductInstalledVersion: string;
    function getActualProductInstalledPackage: string;
    function getActualProductInstalledModificationTime: string;
    function getActualProductActionRequest: string;
    procedure setActualProductActionRequest(request: string);




    function getProductScriptPath(actionType: TAction): string; override;

    function getInstallationPriority: integer; override;

    procedure setProductActionRequest(newAction: TActionRequest); override;
    procedure setProductState(newState: TProductState); override;
    procedure setProductProgress(myprogres: string);

    procedure setProductStateActionRequest(newState: TProductState;
      newAction: TActionRequest); override;
    procedure ProductOnClient_update(actionProgressS: string;
      actionResult: TActionResult4; actionRequest: TActionRequest4;
      targetConfiguration: TTargetConfiguration4; lastAction: TActionRequest4;
      installationStatus: TProductstate4);
    function actionResultToString(actionResult: TActionResult4): string;
    function actionRequest4ToString(actionRequest: TActionRequest4): string;
    function targetConfigurationToString(targetConfiguration: TTargetConfiguration4)
      : string;
    function installationStatusToString(installationStatus: TProductstate4): string;
    function UpdateSwitches(extremeErrorLevel: TErrorLevel): boolean; override; overload;
    function UpdateSwitches(extremeErrorLevel: TErrorLevel; Progress: string): boolean;
      overload;


    property ServiceLastErrorInfo: TStringList read FServiceLastErrorInfo;

    function withLicenceManagement: boolean; override;
    function withRoamingProfiles: boolean;
    function linuxAgentActivated: boolean;
    function isConnected: boolean;
    function getMapOfLoginscripts2Run(allscripts: boolean): TStringList;
    function getMapOfProductActionRequests: TStringList;
    function getFileFromDepot(filename: string; toStringList: boolean;
      var ListResult: TStringList): boolean;
    function decreaseSslProtocol: boolean;
  end;

var
  //IdHTTP1: TIdHTTP;
  //DataModule2: TDataModule2;
  opsidata: TOpsi4Data = nil;
  FValidCredentials: boolean;

function sayActionType(action: TAction): string;
function getOpsiServerVersion(const serviceUrl: string; const username: string;
  const password: string; var sessionid: string): string;
function getOpsiServiceVersion(const serviceUrl: string; const username: string;
  const password: string; var sessionid: string): string;
function escapeControlChars(t: string): string;
procedure getStringlistFromJsonObject(const jO: ISuperObject; var list: TStringList);

resourcestring
  rsSendLog = 'Sending log file to server ...';

implementation

{$IFDEF OPSIWINST}
uses
  {$IFDEF GUI}
  osbatchgui,
  {$ENDIF GUI}
  osmain;

{$ENDIF}

const
  // fuer opsiClassic

  ProduktestatusSektionsname = 'Products-installed';
  Produktsektionskennzeichnung = '-install';



var
  testresult: string;
  ContentTypeCompress: string = 'application/json';
  ContentTypeNoCompress: string = 'application/json';
  ContentEncodingCommpress: string = 'deflate';
  ContentEncodingNoCommpress: string = '';
  AcceptCompress: string = '';
  AcceptNoCompress: string = '';
  AcceptEncodingCompress: string = 'deflate';
  AcceptEncodingNoCompress: string = '';

{$IFNDEF OPSIWINST}
function StringReplace(const S, OldPattern, NewPattern: string): string;
begin
  Result := SysUtils.StringReplace(s, OldPattern, NewPattern,
    [rfReplaceAll, rfIgnoreCase]);
end;

{$ENDIF}

function escapeControlChars(t: string): string;
begin
  t := stringreplace(t, char($5c), '\\'); //  \
  t := stringreplace(t, char($22), '\"'); //  "
  t := stringreplace(t, char($2f), '\/'); //  /
  t := stringreplace(t, #0, '\u0000'); //  null
  t := stringreplace(t, #2, '\c'); //  #10
  t := stringreplace(t, #3, '\c'); //  #10
  t := stringreplace(t, #4, '\c'); //  #10
  t := stringreplace(t, #5, '\c'); //  #10
  t := stringreplace(t, #6, '\c'); //  #10
  t := stringreplace(t, #7, '\a'); //  #10
  t := stringreplace(t, #8, '\b'); //  backspace
  t := stringreplace(t, #9, '\t'); // tab
  t := stringreplace(t, #10, '\n'); //  newline
  t := stringreplace(t, #11, '\v'); //  vertical tab
  t := stringreplace(t, #12, '\f'); //  form feed
  t := stringreplace(t, #13, '\r'); //  carriage return
  t := stringreplace(t, #14, '\c'); //  SO
  t := stringreplace(t, #15, '\c'); //  SI
  t := stringreplace(t, #16, '\c'); //  DLE
  t := stringreplace(t, #17, '\c'); //  DC1
  t := stringreplace(t, #18, '\c'); //  DC2
  t := stringreplace(t, #19, '\c'); //  DC3
  t := stringreplace(t, #20, '\c'); //  DC4
  t := stringreplace(t, #21, '\c'); //  NAK
  t := stringreplace(t, #22, '\c'); //  SYN
  t := stringreplace(t, #23, '\c'); //  ETB
  t := stringreplace(t, #24, '\c'); //  Cancel
  t := stringreplace(t, #25, '\c'); //  EM
  t := stringreplace(t, #26, '\c'); //  SUB
  t := stringreplace(t, #27, '\c'); //  ESC
  t := stringreplace(t, #28, '\c'); //  FS
  t := stringreplace(t, #29, '\c'); //  GS
  t := stringreplace(t, #30, '\c'); //  RS
  t := stringreplace(t, #31, '\c'); //  US
  t := stringreplace(t, #127, '\c'); //  DEL
  Result := t;
end;

// Superclass =========================

function getOpsiServerVersion(const serviceUrl: string; const username: string;
  const password: string; var sessionid: string): string;
var
  resultmap: TStringList;
  ///resultlist : TList;
  omc: TOpsiMethodCall;
  ///i : Integer;
  jsonEntry: ISuperObject;
  FjsonExecutioner: TJsonThroughHTTPS;
  testresult: string;
  credentialsValid: boolean;
begin
  Result := '';
  credentialsValid := True;
  FValidCredentials := credentialsValid;
  omc := nil;
  FJsonExecutioner := nil;
  //LogDatei.LogLevel := LLdebug2;
  try
    //if FJsonExecutioner <> nil then FJsonExecutioner.free;
    FjsonExecutioner := TJsonThroughHTTPS.Create(serviceUrl, username,
      password, sessionid);
    omc := TOpsiMethodCall.Create('backend_info', []);
    resultmap := FjsonExecutioner.getMapResult(omc);
    omc.Free;
    if resultmap <> nil then
    begin
      testresult := resultmap.Strings[0];
      Result := resultmap.Values['opsiVersion'];
      Result := AnsiDequotedStr(trim(Result), '"');
      testresult := Result;
      sessionid := FjsonExecutioner.FSessionId;
    end;
    if Result = '' then
    begin
      //result := '3';
      credentialsValid := FValidCredentials;
      if credentialsValid then
      begin
        LogDatei.DependentAdd('backend_info with no result', LLdebug2);
        LogDatei.NumberOfErrors := LogDatei.NumberOfErrors - 1;
        try
          if FJsonExecutioner <> nil then
            FJsonExecutioner.Free;
          //if omc <> nil then omc.Free;
          FjsonExecutioner := TJsonThroughHTTPS.Create(serviceUrl, username, password);
          omc := TOpsiMethodCall.Create('getOpsiInformation_hash', []);
          jsonEntry := FjsonExecutioner.retrieveJsonObject(omc);
          //result := jsonEntry.getJSONObject('result').getString('opsiVersion');
          Result := jsonEntry.O['result'].S['opsiVersion'];
          omc := TOpsiMethodCall.Create('exit', []);
          jsonEntry := FjsonExecutioner.retrieveJSONObject(omc);
          FjsonExecutioner.Free;
          omc.Free;
          Result := AnsiDequotedStr(trim(Result), '"');
          //if result = '' then result := '3'
        except
          if omc <> nil then
            omc.Free;
          LogDatei.DependentAdd('Exception in getOpsiServerVersion ', LLdebug2);
          //result := '3';
        end;
      end;
    end;
  except
    //result := '3';
    try
      if FJsonExecutioner <> nil then
        FJsonExecutioner.Free;
      if omc <> nil then
        omc.Free;
      FjsonExecutioner := TJsonThroughHTTPS.Create(serviceUrl, username, password);
      omc := TOpsiMethodCall.Create('getOpsiInformation_hash', []);
      jsonEntry := FjsonExecutioner.retrieveJsonObject(omc);
      //result := jsonEntry.geTSuperObject('result').getString('opsiVersion');
      Result := jsonEntry.O['result'].S['opsiVersion'];
      omc := TOpsiMethodCall.Create('exit', []);
      jsonEntry := FjsonExecutioner.retrieveJSONObject(omc);
      FjsonExecutioner.Free;
      omc.Free;
      Result := AnsiDequotedStr(trim(Result), '"');
      //if result = '' then result := '3'
    except
      if omc <> nil then
        omc.Free;
      LogDatei.DependentAdd('Exception in getOpsiServerVersion ', LLdebug2);
      //result := '3';
    end;
  end;
  LogDatei.DependentAdd('opsi Server Version : ' + Result, LLnotice);
  //LogDatei.LogLevel := LLnotice;
end;

function getOpsiServiceVersion(const serviceUrl: string; const username: string;
  const password: string; var sessionid: string): string;
var
  verstr: string;
  sign: integer;
  InfoSyntaxError: string;
begin
  Result := '4'; //default to opsi 4.x
  verstr := getOpsiServerVersion(serviceUrl, username, password, sessionid);
  (*
  if verstr = '' then
    Result := ''
  else
  begin
    if getDecimalCompareSign(verstr, '3.5', sign, InfoSyntaxError, False) then
    begin
      if sign = 0 then
        LogDatei.DependentAdd('Internal Error: Opsi Server Version:>' +
          verstr + '< is >3.5< which was never released', LLerror);
      if sign = 1 then
        Result := '4';
      if sign = -1 then
        Result := '3';
    end
    else
      LogDatei.DependentAdd('Internal Error: Opsi Server Version:>' +
        verstr + '< is not doted numbers', LLerror);
    end;
  end;
*)
end;

function sayActionType(action: TAction): string;
begin
  Result := '';
  case action of
    tacNull: ;
    tacSetup: Result := 'setup';
    tacUpdate: Result := 'update';
    tacDeinstall: Result := 'deinstall';
    tacAlways: Result := 'always';
    tacOnce: Result := 'once';
  end;
end;

constructor TString.Create(const AStr: string);
begin
  inherited Create;
  FStr := AStr;
end;


constructor TOpsiData.Create;
begin
  FSortByServer := False;
  actualclient := '';
end;

function TOpsiData.getSpecialScriptPath: string;
begin
  Result := '';
end;



procedure TOpsiData.finishOpsiConf;
begin
end;

procedure TOpsiData.setOptions(serviceoptions: string);
begin
  options := serviceoptions;
end;


procedure TOpsiData.setActualProductName(const productname: string);
begin
  actualProduct := productname;
end;


function TOpsiData.getActualProductName: string;
begin
  Result := actualProduct;
end;

function TOpsiData.getActualProductVersion: string;
begin
  Result := actualVersion;
end;

function TOpsiData.UpdateSwitches(extremeErrorLevel: TErrorLevel): boolean;
var
  ar: TActionRequest;
  action: TAction;
  newState: TProductState;
  newActionRequest: TActionRequest;
begin
  Result := False;
  ar := getProductActionRequest;
  action := getProductAction;

  newActionRequest := tapNull;

  if extremeErrorLevel > levelfatal then
  begin

    case action of
      tacDeinstall:
      begin
        newState := tpsNotInstalled;
        newActionRequest := tapNull;
        Result := True;
      end;

      tacSetup, tacUpdate:
      begin
        //NeuerProduktschalter := 'on';
        Result := True;
        newState := tpsInstalled;
        newActionRequest := tapNull;
      end;

      tacOnce:
      begin
        Result := True;
        //newState := tpsUndefined;
        newState := tpsNotInstalled;
        newActionRequest := tapNull;
      end;

      tacAlways:
      begin
        Result := True;
        newState := tpsInstalled;
        newActionRequest := tapAlways;
      end;
    end;

    // but if we did the action on hehalf of a policy we stay on following policies and reset new the action request:

    if ar in [tapNull_byPolicy, tapSetup_byPolicy, tapUpdate_byPolicy,
      tapDeinstall_byPolicy] then
    begin
      newActionRequest := tap_byPolicy;
    end;

  end
  else
  begin
    Result := True;
    newState := tpsFailed;
    newActionRequest := tapNull;
  end;

  setProductStateActionRequest(newState, newActionRequest);

end;

function TOpsiData.withLicenceManagement: boolean;
begin
  Result := False;
end;

{
Function TOpsiData.getListOfProducts: TStringList;
begin
   result := TStringList.create;
end;
}

// TOpsiDataClassic =========================
(*

procedure TOpsiDataClassic.initOpsiConf(PathnamsinfoFilename,
  Profildateiname, ProdukteinfoFilename: string);
begin
end;

procedure TOpsiDataClassic.saveOpsiConf;
begin
end;

procedure TOpsiDataClassic.finishOpsiConf;
begin
end;

function TOpsiDataClassic.stateToString(state: TProductState): string;
begin
  Result := 'undefined';
  case state of
    tpsNotInstalled: Result := 'off';
    tpsInstalled: Result := 'on';
    tpsInstalling: Result := 'installing';
    tpsFailed: Result := 'failed';
  end;
end;


function TOpsiDataClassic.actionToString(action: TAction): string;
begin
  Result := 'undefined';
  case action of
    tacSetup: Result := 'setup';
    tacUpdate: Result := 'update';
    tacDeinstall: Result := 'deinstall';
    tacAlways: Result := 'always';
    tacOnce: Result := 'once';
  end;
end;

function TOpsiDataClassic.actionRequestToString(actionRequest: TActionRequest): string;
begin
  Result := 'undefined';
  case actionRequest of
    tapSetup: Result := 'setup';
    tapUpdate: Result := 'update';
    tapDeinstall: Result := 'deinstall';
    tapAlways: Result := 'always';
    tapOnce: Result := 'once';
  end;
end;


procedure TOpsiDataClassic.setActualClient(computername: string);
begin
  //inherited setActualClient (computername);
  actualClient := computername;
  ProdukteStatus := getMapOfProductSwitches;
end;


function TOpsiDataClassic.translateSwitchToState(s: string): TProductState;
var
  Produktschalter: string;
begin
  Result := tpsUndefined;
  Produktschalter := UpperCase(s);
  if Produktschalter = 'ON' then
    Result := tpsInstalled
  else if Produktschalter = 'OFF' then
    Result := tpsNotInstalled
  else if Produktschalter = 'INSTALLING' then
    Result := tpsInstalling
  else if pos('FAILED', Produktschalter) > 0 then
    Result := tpsFailed;
end;



function TOpsiDataClassic.translateSwitchToAction(s: string): TAction;
var
  Produktschalter: string;
begin
  Result := tacNull;
  Produktschalter := UpperCase(s);

  if (Produktschalter = 'SETUP') then
    Result := tacSetup

  else if (Produktschalter = 'INSTALLING')
  // State that has to be handled like the setup action in classic mode
  then
    Result := tacSetup

  else if (Produktschalter = 'DEINSTALL') then
    Result := tacDeinstall

  else if (Produktschalter = 'UPDATE') then
    Result := tacUpdate

  else if (Produktschalter = 'ONCE') then
    Result := tacOnce

  else if (Produktschalter = 'ALWAYS') then
    Result := tacAlways;
end;

function TOpsiDataClassic.translateSwitchToActionRequest(s: string): TActionRequest;
var
  Produktschalter: string;
begin
  Result := tapNull;
  Produktschalter := UpperCase(s);

  if (Produktschalter = 'SETUP') then
    Result := tapSetup

  else if (Produktschalter = 'DEINSTALL') then
    Result := tapDeinstall

  else if (Produktschalter = 'UPDATE') then
    Result := tapUpdate

  else if (Produktschalter = 'ONCE') then
    Result := tapOnce

  else if (Produktschalter = 'ALWAYS') then
    Result := tapAlways;
end;




procedure TOpsiDataClassic.setActualProductName(const productname: string);
begin
  inherited setActualProductName(productname);
end;

procedure TOpsiDataClassic.initProduct;
begin
end;

procedure TOpsiDataClassic.finishProduct;
begin
  ProductVarsGeneral.Free;
end;


function TOpsiDataClassic.getLogFileName(const LogFilename: string): string;
begin
end;


function TOpsiDataClassic.getSpecialScriptPath: string;
begin
  Result := ProductvarsGeneral.Values['SetupPath'];
end;


function TOpsiDataClassic.getProductproperties: TStringList;
begin
end;

function TOpsiDataClassic.getBeforeRequirements: TStringList;
begin
end;

function TOpsiDataClassic.getAfterRequirements: TStringList;
begin
end;

function TOpsiDataClassic.getListOfProducts: TStringList;
begin
end;

function TOpsiDataClassic.getMapOfProductSwitches: TStringList;
begin
end;


function TOpsiDataClassic.getProductState: TProductState;
begin
  Result := translateSwitchToState(ProdukteStatus.Values[ActualProduct]);
end;

function TOpsiDataClassic.getProductAction: TAction;
begin
  Result := translateSwitchToAction(ProdukteStatus.Values[ActualProduct]);
end;

function TOpsiDataClassic.getProductActionRequest: TActionRequest;
begin
  Result := translateSwitchToActionRequest(ProdukteStatus.Values[ActualProduct]);
end;


function TOpsiDataClassic.getProductScriptPath(actionType: TAction): string;
begin
  Result := '';

  case actionType of
    tacDeinstall:
      Result := ProductvarsGeneral.Values['DeinstallWinst'];
    tacSetup:
      Result := ProductvarsGeneral.Values['SetupWinst'];
    tacOnce:
      Result := ProductvarsGeneral.Values['OnceWinst'];
    tacAlways:
      Result := ProductvarsGeneral.Values['AlwaysWinst'];
    tacUpdate:
      Result := ProductvarsGeneral.Values['UpdateWinst'];
  end;
end;

function TOpsiDataClassic.getInstallationPriority: integer;
begin
  Result := 0;
end;

procedure TOpsiDataClassic.setProductActionRequest(newAction: TActionRequest);
begin
end;

procedure TOpsiDataClassic.setProductState(newState: TProductState);
begin
end;

procedure TOpsiDataClassic.setProductStateActionRequest(newState: TProductState;
  newAction: TActionRequest);
begin
  if newAction in [tapNull] then
    setProductState(newState)
  else
    setProductActionRequest(newAction);
end;
*)

// opsidata-Service ================================================================

// helper classes

constructor TOpsiMethodCall.Create(const method: string; parameters: array of string);
var
  i: integer;
begin

  FOpsiMethodname := method;

  Fparameterlist := TStringList.Create;

  for i := 0 to high(parameters) do
  begin
    LogDatei.DependentAdd('Parameters in OpsiMethodCall: ' + parameters[i], LLdebug3);
    Fparameterlist.add(parameters[i]);
  end;

end;

constructor TOpsiMethodCall.Create(const method: string; hashlist: TStringList;
  parameters: array of string);
var
  i: integer;
begin

  FOpsiMethodname := method;

  Fparameterlist := TStringList.Create;
  Fhashlist := TStringList.Create;

  for i := 0 to high(parameters) do
  begin
    LogDatei.DependentAdd('Parameters in OpsiMethodCall: ' + parameters[i], LLdebug3);
    Fparameterlist.add(parameters[i]);
  end;
  for i := 0 to hashlist.Count - 1 do
  begin
    LogDatei.DependentAdd('hashlist in OpsiMethodCall: ' +
      hashlist.Strings[i], LLdebug3);
    Fhashlist.add(escapeControlChars(hashlist.Strings[i]));
  end;

end;


destructor TOpsiMethodCall.Destroy;
begin
  inherited Destroy;
end;

function TOpsiMethodCall.getJsonUrlString: string;
const
  defaultJsonID = 1;
var
  jO: ISuperObject;
  joParams: ISuperObject;
  i: integer;
  testresult: string;
begin
  Result := '';
  try
    jO := TSuperObject.Create;
    jo.I['id'] := defaultJsonID;
    jo.S['method'] := opsimethodname;
    joParams := TSuperObject.Create(stArray);

    for i := 0 to parameterlist.Count - 1 do
    begin
      if (length(parameterlist.Strings[i]) > 0) and
        ((parameterlist.Strings[i][1] = '{')) and
        ((parameterlist.Strings[i][length(parameterlist.Strings[i])] = '}')) then
      begin
        testresult := parameterlist.Strings[i];
        LogDatei.DependentAdd('Creating TSuperObject with: ' +
          parameterlist.Strings[i], LLdebug3);
        //joParams.put(TSuperObject.create(parameterlist.Strings[i]));
        joParams.AsArray.Add(SO(parameterlist.Strings[i]));
        //joParams.AsArray.Add()
        //LogDatei.DependentAdd ('resulting TSuperArray string: ' + joParams. ,LLdebug2);
      end
      else
      begin
        testresult := parameterlist.Strings[i];
        LogDatei.DependentAdd('Putting to TSuperArray with: ' +
          SO('"' + parameterlist.Strings[i] + '"').AsJSon, LLdebug3);
        //joParams.put(parameterlist.Strings[i]);
        if (length(parameterlist.Strings[i]) > 0) and
          ((parameterlist.Strings[i][1] = '[')) and
          ((parameterlist.Strings[i][length(parameterlist.Strings[i])] = ']')) then
          if parameterlist.Strings[i] = '[]' then
          begin
            joParams.AsArray.Add(SA([]));
            LogDatei.log('Adding empty Array: ' + parameterlist.Strings[i], LLdebug3);
          end
          else
          begin
            LogDatei.log('Adding Array: ' + parameterlist.Strings[i], LLdebug3);
            LogDatei.log('Adding Array as Json: ' + SO(parameterlist.Strings[i]).AsString, LLdebug3);
            joParams.AsArray.Add(SO(parameterlist.Strings[i]));
            //LogDatei.log('Adding Array as Json: ' + SA([copy(parameterlist.Strings[i], 2, length(parameterlist.Strings[i]) - 2)]).AsJSon, LLdebug3);
            //joParams.AsArray.Add(SA([copy(parameterlist.Strings[i], 2, length(parameterlist.Strings[i]) - 2)]));
            //LogDatei.log('Adding Array as String: ' + SA([copy(parameterlist.Strings[i], 2,length(parameterlist.Strings[i]) - 2)]).AsString , LLdebug3);
            //joParams.AsArray.Add(SA([copy(parameterlist.Strings[i], 2,length(parameterlist.Strings[i]) - 2)]).AsString);
            //joParams.AsArray.Add(copy(parameterlist.Strings[i], 2,length(parameterlist.Strings[i]) - 2));
            //joParams.AsArray.Add(parameterlist.Strings[i]);
          end

        else
          joParams.AsArray.Add(SO('"' + parameterlist.Strings[i] + '"'));
        //joParams.AsArray.Add(SO(parameterlist.Strings[i]));
        //LogDatei.log ('resulting TSuperArray string: ' + joParams.tostring ,LLDebug3);
      end;
    end;

    //LogDatei.DependentAdd ('resulting TSuperArray string: ' + joParams.tostring ,LLdebug2);
    testresult := joParams.AsJSon(False, True);
    jO['params'] := TSuperObject.Create(stArray);
    jO['params'] := joParams;
    //jO.put('params', joParams);


    //result := jO.toString;
    Result := jO.AsJSon(False, False);
    testresult := Result;
    //LogDatei.DependentAdd ('resulting getJsonUrlString: ' + Result ,LLdebug2);

    //System.out.println ("a JSONObject  as String>> "  + jO.toString());
    //result = URLEncoder.encode (  jO.toString(), "UTF8");
    //System.out.println ("a JSONObject  as String>> "  + result);
         {
          catch ( UnsupportedEncodingException ux)

              logging.debugOut(this, logging.LEVEL_ERROR, ux.toString());

          catch (org.json.JSONException jex)

               logging.debugOut(this, logging.LEVEL_ERROR, "Exception while produccing a JSONObject, " + jex.toString());

         }

  except
    on ex: Exception do
    begin
      LogDatei.DependentAddWarning('Exception in OpsiMethodCall.getJsonUrlString: ' +
        ex.message, LLdebug2);
    end;
  end;
end;

function getJsonObjectStringfromList(const inlist: TStringList): WideString;
var
  i: integer;
  str: WideString;
  new_obj: ISuperObject;
begin
  str := '{ ' + inlist.Strings[0];
  for i := 1 to inlist.Count - 1 do
  begin
    str := str + ' , ' + inlist.Strings[i];
  end;
  str := str + ' }';
  new_obj := SO(str);
  Result := new_obj.asJson;
end;

function TOpsiMethodCall.getJsonHashListString: string;
const
  defaultJsonID = 1;
var
  jO, jotmp: ISuperObject;
  joParams, joParams0: ISuperObject;
  i, testint: integer;
  inmap: boolean;
  ///objstr: TString;
  ///myindex : integer;
  //jostr,
  aktvalue: SOString;
begin
  try
    jO := TSuperObject.Create;
    jotmp := TSuperObject.Create;
    jo.I['id'] := defaultJsonID;
    jo.S['method'] := opsimethodname;
    joParams0 := TSuperObject.Create(stArray);
    joParams := TSuperObject.Create(stArray);
    LogDatei.DependentAdd('while getJsonHashListString: ' +
      jo.AsJSon(False, False) + '<', LLdebug3);
    inmap := False;
    //jostr := '';
    for i := 0 to hashlist.Count - 1 do
    begin
      if hashlist.Strings[i] = '[newmap]' then
      begin
        if inmap and not jotmp.IsType(stNull) then
        begin
          LogDatei.DependentAdd('while getJsonHashListString try to add: ' +
            jotmp.AsJSon(False, False), LLdebug3);
          try
            //jotmp := SO('{'+jostr+'}');
            if not jotmp.IsType(stNull) then
            begin
              joParams.AsArray.Add(jotmp);
            end
            else
              LogDatei.DependentAdd('while getJsonHashListString failed to add: ' +
                jotmp.AsJSon(False, False), LLdebug2);
            LogDatei.DependentAdd('while getJsonHashListString: ' +
              jOParams.AsJSon(False, False), LLdebug2);
          except
            on ex: Exception do
            begin
              LogDatei.DependentAddWarning(
                'Exception in OpsiMethodCall.getJsonHashListString: {' +
                jotmp.AsJSon(False, False) + '} is no JSON object ' +
                ex.message, LLError);
            end
          end;
          jotmp := TSuperObject.Create;
        end
        else
        begin
          inmap := True;
        end;
        jotmp := TSuperObject.Create;
      end
      else
      begin
        aktvalue := hashlist.ValueFromIndex[i];
        if aktvalue = '' then
        begin
          jOtmp.S[hashlist.Names[i]] := '';
        end
        else if aktvalue = 'null' then
        begin
          jOtmp.N[hashlist.Names[i]] := SO('null');
        end
        else if (aktvalue = 'true') then
        begin
          jOtmp.B[hashlist.Names[i]] := True;
        end
        else if (aktvalue = 'false') then
        begin
          jOtmp.B[hashlist.Names[i]] := False;
        end
        else if copy(aktvalue, 0, 1) = 'x' then
        begin
          jOtmp.S[hashlist.Names[i]] := aktvalue;
        end
        else if copy(aktvalue, 0, 11) = 'OPSISTRING:' then
        begin
          jOtmp.S[hashlist.Names[i]] := copy(aktvalue, 12, length(aktvalue));
        end
        else
        begin
          try
            testint := StrToInt(aktvalue);
            jOtmp.I[hashlist.Names[i]] := testint;
          except
            //we assume it is a string
            //jOtmp.S[hashlist.Names[i]] := aktvalue;
            jOtmp.S[hashlist.Names[i]] := SO('"' + aktvalue + '"').AsString;
          end;
        end;
        //jostr := jostr + hashlist.Names[i]+':'+ aktvalue+',';
      end;
    end;
    if inmap and not jotmp.IsType(stNull) then
    begin
      LogDatei.DependentAdd('while getJsonHashListString added: ' +
        jotmp.AsJSon(False, False), LLdebug3);
      try
        joParams.AsArray.Add(jotmp);
        LogDatei.DependentAdd('while getJsonHashListString: ' +
          jOParams.AsJSon(False, False), LLdebug3);
      except
        on ex: Exception do
        begin
          LogDatei.DependentAddWarning(
            'Exception in OpsiMethodCall.getJsonHashListString: {' +
            jotmp.AsJSon(False, False) + '} is no JSON object ' + ex.message, LLError);
        end;
      end;
    end;
    LogDatei.DependentAdd('while getJsonHashListString: ' +
      jOParams.AsJSon(False, False), LLdebug3);
    jO['params'] := TSuperObject.Create(stArray);
    joParams0.AsArray.Add(joParams);
    jO['params'] := joParams0;
    Result := jO.AsJSon(False, False);
    testresult := Result;
    LogDatei.DependentAdd('final getJsonHashListString: ' +
      jO.AsJSon(True, False), LLdebug3);
  except
    on ex: Exception do
    begin
      LogDatei.DependentAddWarning('Exception in OpsiMethodCall.getJsonHashListString: '
        + ex.message, LLError);
    end;
  end;
end;



constructor TJsonThroughHTTPS.Create(const serviceURL, username, password: string);
begin
  TJsonThroughHTTPS.Create(serviceUrl, username, password, '', '', '');
end;

constructor TJsonThroughHTTPS.Create(
  const serviceURL, username, password, sessionid: string);
begin
  Create(serviceUrl, username, password, sessionid, '', '');
end;

constructor TJsonThroughHTTPS.Create(
  const serviceURL, username, password, sessionid, ip, port: string);
begin
  //portHTTPS := port;
  //portHTTP := 4444;
  FserviceURL := serviceURL;
  Fusername := username;
  Fpassword := password;
  FSessionId := sessionid;
  mymemorystream := TMemoryStream.Create;
  FResultLines := TStringList.Create;
  FErrorInfo := TStringList.Create;
  {$IFDEF OPSIWINST}
  createSocket(osconf.selfProductName + ' ' + osconf.WinstVersion, '', '');
  {$ELSE OPSIWINST}
    {$IFDEF OCASIMP}
  createSocket('ocasimp ', ip, port);
    {$ELSE OCASIMP}
  createSocket(ExtractFileName(ParamStr(0)), '', '');
    {$ENDIF OCASIMP}
  {$ENDIF OPSIWINST}
  //nullO := Null.create;
  methodGET := False;
end;

procedure TJsonThroughHTTPS.createSocket;
begin
  createSocket('', '', '');
end;


procedure TJsonThroughHTTPS.createSocket(const agent, ip, port: string);
begin
  try
    IdSSLIOHandlerSocket := TIdSSLIOHandlerSocketOpenSSL.Create(nil);

    with IdSSLIOHandlerSocket do
    begin
      SSLOptions.VerifyMode := [];
      SSLOptions.VerifyDepth := 0;
      LogDatei.log('Working with ssl protocol: sslvSSLv23 - auto negotation', LLInfo);
      // sslvSSLv23 means: try all possible Versions
      SSLOptions.Method := sslvSSLv23;
      SSLOptions.Mode := sslmUnassigned;
    end;


    //IdHTTP := TIdHTTP.Create(nil);
    IdHTTP := TIdWebDAV.Create(nil);
    //LogDatei.DependentAdd('createSocket--->4', LLdebug2);
    IdHTTP.IOHandler := IdSSLIOHandlerSocket;
    //LogDatei.DependentAdd('createSocket--->5', LLdebug2);
    IdHTTP.Request.BasicAuthentication := True;
    IdHTTP.Request.Username := Fusername;
    IdHTTP.Request.Password := Fpassword;
    IdHTTP.Request.UserAgent := agent;
    //LogDatei.DependentAdd('createSocket--->6', LLdebug2);
    try
      if ip <> '' then
      begin
        IdHTTP.BoundIP := ip;
      end;
    except
      on ex: Exception do
      begin
        LogDatei.DependentAdd(
          'Exception in TJsonThroughHTTPS.createSocket: IdHTTP.BoundIP:' +
          ex.message, LLError);
      end;
    end;
    //LogDatei.DependentAdd('createSocket-BoundIp: '+IdSSLIOHandlerSocket.BoundIP, LLdebug2);
    LogDatei.DependentAdd('createSocket-BoundIp: ' + IdHTTP.BoundIP, LLdebug2);
  except
    on ex: Exception do
    begin
      LogDatei.DependentAddWarning('Exception in TJsonThroughHTTPS.createSocket: ' +
        ex.message, LLError);
    end;
  end;
end;



destructor TJsonThroughHTTPS.Destroy;
begin
  //nullO.free;
  mymemorystream.Free;
  FResultLines.Free;

  IdSSLIOHandlerSocket.Close;
  IdSSLIOHandlerSocket.Free;

  IdHTTP.Free;
  inherited Destroy;
end;

procedure TJsonThroughHTTPS.makeURL(const omc: TOpsiMethodCall);
begin
  //Furl := 'https://' + fhost + ':' + intToStr(portHTTPS) + '/rpc?' + EncodeUrl(omc.jsonUrlString);
  LogDatei.DependentAdd('got omc.jsonUrlString: ' + omc.jsonUrlString, LLdebug3);
  //LogDatei.DependentAdd('got omc.jsonUrlString: ' + omc.getJsonUrlString, LLdebug2);
  // connections to localhost may come from the WAN-Client or the opsiservicall /
  if (FserviceURL = 'https://localhost:4441/opsiclientd')
      or (FserviceURL = 'https://localhost:4441/kiosk') then
  begin
    //Furl := FserviceURL + '/opsiclientd';
    Furl := FserviceURL;
  end
  else
  begin
    if methodGet then
      Furl := FserviceURL + '/rpc?' + EncodeUrl(omc.jsonUrlString)
    else
      Furl := FserviceURL + '/rpc';
  end;
  LogDatei.DependentAdd('got Furl: ' + Furl, LLdebug3);
end;


procedure getStringlistFromJsonObject(const jO: ISuperObject; var list: TStringList);
var
  iter: TSuperObjectIter;
  testresult: string;
begin
  if list = nil then
    exit;

  list.Clear;

  if (jO = nil) then
    exit;

  if ObjectFindFirst(jo, iter) then
  begin
    repeat
      testresult := iter.key + '=' + iter.Val.AsString;
      list.Add(iter.key + '=' + iter.Val.AsString);
    until not ObjectFindNext(iter);
    ObjectFindClose(iter);
  end;
end;



function TJsonThroughHTTPS.retrieveJSONObject(const omc: TOpsiMethodCall): ISuperObject;
begin
  Result := retrieveJSONObject(omc, True);
end;



function TJsonThroughHTTPS.retrieveJSONObject(const omc: TOpsiMethodCall;
  logging: boolean): ISuperObject;
begin
  Result := retrieveJSONObject(omc, logging, True);
end;

function TJsonThroughHTTPS.retrieveJSONObject(const omc: TOpsiMethodCall;
  logging: boolean; retry: boolean): ISuperObject;
begin
  Result := retrieveJSONObject(omc, logging, True, False);
end;


function TJsonThroughHTTPS.retrieveJSONObject(const omc: TOpsiMethodCall;
  logging: boolean; retry: boolean; readOmcMap: boolean): ISuperObject;

var
  errorOccured: boolean;
  cookieVal: string;
  posColon: integer;
  s, t, teststring: string;
  jO: ISuperObject;
  utf8str: UTF8String;
  sendstream, ReceiveStream: TMemoryStream;
  CompressionSendStream: TCompressionStream;
  CompressionReceiveStream: TDeCompressionStream;
  buffer: ^byte;
  readcount: integer;
  compress: boolean;
  startTime: TDateTime;

begin
  errorOccured := False;
  Result := nil;
  resultlines.Clear;
  fErrorInfo.Clear;
  compress := False;
  startTime := now;

  try
    FError := '';
    mymemorystream.Clear;

    makeurl(omc);

    try
      ////ProcessMess;
      if FSessionId <> '' then
      begin
        //IdHttp.Request.RawHeaders.Add('Cookie');
        IdHttp.Request.RawHeaders.Values['Cookie'] := FSessionId;
      end;
      LogDatei.DependentAdd('Sessionid ' + FSessionId, LLdebug2);
      testresult := IdHttp.Request.RawHeaders.Text;

      if FSessionId <> '' then
        // not the first call and we log
        if logging then
          //LogDatei.DependentAdd (DateTimeToStr(now) + ' JSON service request ' + Furl , LLnotice);
          LogDatei.DependentAdd('JSON service request ' + Furl + ' ' +
            omc.FOpsiMethodName, LLinfo);

      if methodGet then
      begin
        utf8str := AnsiToUtf8(Furl);
        LogDatei.DependentAdd(' JSON service request ' + Furl, LLdebug2);
        IdHTTP.Get(utf8str, mymemorystream);
      end

      else
      begin
        if readOmcMap then
        begin
          s := omc.getJsonHashListString;
          utf8str := AnsiToUtf8(s);
          LogDatei.DependentAdd(' JSON service request Furl ' + Furl, LLdebug2);
          LogDatei.DependentAdd(' JSON service request str ' + utf8str, LLdebug2);

        end
        else
        begin
          s := omc.jsonUrlString;
          utf8str := AnsiToUtf8(s);
          LogDatei.DependentAdd(' JSON service request Furl ' + Furl, LLdebug2);
          LogDatei.DependentAdd(' JSON service request str ' + utf8str, LLdebug2);
        end;
        try
          sendstream := TMemoryStream.Create;
          ReceiveStream := TMemoryStream.Create;
          sendstream.Clear;
          // we assume opsi4
          compress := True;
          LogDatei.log('Using MimeType: ' + ContentTypeCompress, LLDebug2);
          (*
          if opsidata <> nil then
          begin
            //LogDatei.DependentAdd('getOpsiServiceVersion:'+myopsidata.getOpsiServiceVersion, LLdebug2);
            if myopsidata.getOpsiServiceVersion = '4' then
            begin
              compress := True;
              LogDatei.log('Using MimeType: ' + ContentTypeCompress, LLDebug2);
              //compress := false;
            end
            else
            begin
              compress := False;
              LogDatei.log('Using MimeType: ' + ContentTypeNoCompress, LLDebug2);
            end;
          end
          else
            compress := False;
          *)
          if compress then
          begin
            IdHttp.Request.ContentType := ContentTypeCompress;
            IdHttp.Request.ContentEncoding := ContentEncodingCommpress;
            IdHttp.Request.Accept := AcceptCompress;
            IdHttp.Request.AcceptEncoding := AcceptEncodingCompress;
            LogDatei.log('HTTP Header: ' + IdHttp.Request.RawHeaders.Text, LLDebug2);
            CompressionSendStream := TCompressionStream.Create(clMax, sendstream);
            CompressionSendStream.Write(utf8str[1], length(utf8str));
            CompressionSendStream.Free;
            //writeln('ddebug: bpost');
            IdHTTP.Post(Furl, sendstream, ReceiveStream);
            //writeln('ddebug: apost');
            ReceiveStream.Seek(0, 0);
            CompressionReceiveStream := TDeCompressionStream.Create(ReceiveStream);
            GetMem(buffer, 655360);
            repeat
              FillChar(buffer^, 655360, ' ');
              readcount := CompressionReceiveStream.Read(buffer^, 655360);
              if readcount > 0 then
                mymemorystream.Write(buffer^, 655360);
            until readcount < 655360;
            CompressionReceiveStream.Free;
            FreeMem(buffer);
          end
          else
          begin
            IdHttp.Request.ContentType := ContentTypeNoCompress;
            IdHttp.Request.ContentEncoding := ContentEncodingNoCommpress;
            IdHttp.Request.Accept := AcceptNoCompress;
            IdHttp.Request.AcceptEncoding := AcceptEncodingNoCompress;
            LogDatei.log('HTTP Header: ' + IdHttp.Request.RawHeaders.Text, LLDebug2);
            sendstream.Write(utf8str[1], length(utf8str));
            IdHTTP.Post(Furl, sendstream, mymemorystream);
          end;
          //// test memorystream to raise exception here
          //mymemorystream.Position := 0;
          //teststring := mymemorystream.ReadAnsiString;
        except
          on e: Exception do
          begin
            LogDatei.DependentAdd('Exception in retrieveJSONObject0: ' +
              e.message, LLdebug2);
            //writeln('ddebug: Exception in retrieveJSONObject0: ' + e.message);
            if e.message = 'HTTP/1.1 401 Unauthorized' then
              FValidCredentials := False;
            t := s;
            // retry with other parameters
            if ContentTypeCompress = 'application/json' then
            begin
              ContentTypeCompress := 'gzip-application/json-rpc';
              AcceptCompress := 'gzip-application/json-rpc';
              ContentTypeNoCompress := 'application/json-rpc';
              AcceptNoCompress := 'application/json-rpc';
              ContentEncodingNoCommpress := '';
              ContentEncodingCommpress := '';
              AcceptEncodingCompress := '';
              AcceptEncodingNoCompress := '';
            end
            else
            begin
              ContentTypeCompress := 'application/json';
              AcceptCompress := '';
              ContentTypeNoCompress := 'application/json';
              AcceptNoCompress := '';
              ContentEncodingNoCommpress := '';
              ContentEncodingCommpress := 'deflate';
              AcceptEncodingCompress := 'deflate';
              AcceptEncodingNoCompress := '';
            end;

            LogDatei.log('Changing to MimeType: ' + ContentTypeCompress, LLDebug);
            sendstream.Free;
            (****************)
            try
              sendstream := TMemoryStream.Create;
              ReceiveStream := TMemoryStream.Create;
              sendstream.Clear;
              // we assume opsi4
              compress := True;
              LogDatei.log('Using MimeType: ' + ContentTypeCompress, LLDebug2);
              (*
              if opsidata <> nil then
              begin
                //LogDatei.DependentAdd('getOpsiServiceVersion:'+opsidata.getOpsiServiceVersion, LLdebug2);
                if opsidata.getOpsiServiceVersion = '4' then
                begin
                  compress := True;
                  //compress := false;
                end
                else
                  compress := False;
              end
              else
                compress := False;
              *)
              if compress then
              begin
                IdHttp.Request.ContentType := ContentTypeCompress;
                IdHttp.Request.Accept := AcceptCompress;
                IdHttp.Request.AcceptEncoding := AcceptEncodingCompress;
                CompressionSendStream := TCompressionStream.Create(clMax, sendstream);
                CompressionSendStream.Write(utf8str[1], length(utf8str));
                CompressionSendStream.Free;
                //writeln('ddebug: bpost');
                IdHTTP.Post(Furl, sendstream, ReceiveStream);
                //writeln('ddebug: apost');
                ReceiveStream.Seek(0, 0);
                CompressionReceiveStream := TDeCompressionStream.Create(ReceiveStream);
                GetMem(buffer, 655360);
                repeat
                  FillChar(buffer^, 655360, ' ');
                  readcount := CompressionReceiveStream.Read(buffer^, 655360);
                  if readcount > 0 then
                    mymemorystream.Write(buffer^, 655360);
                until readcount < 655360;
                CompressionReceiveStream.Free;
                FreeMem(buffer);
              end
              else
              begin
                IdHttp.Request.ContentType := ContentTypeNoCompress;
                IdHttp.Request.Accept := AcceptNoCompress;
                IdHttp.Request.AcceptEncoding := AcceptEncodingNoCompress;
                sendstream.Write(utf8str[1], length(utf8str));
                IdHTTP.Post(Furl, sendstream, mymemorystream);
              end;
            except
              on e: Exception do
              begin
                LogDatei.log('Exception in retrieveJSONObject0: ' +
                  e.message, LLdebug2);
                //writeln('ddebug: Exception in retrieveJSONObject0: ' + e.message);
                if e.message = 'HTTP/1.1 401 Unauthorized' then
                  FValidCredentials := False;
                t := s;
              end;
            end;

            (****************)
          end;
        end;
        sendstream.Free;
      end;
      //endTime := now;
      //if FSessionId <> '' then
      // not the first call and we log
      if logging then
      begin
        LogDatei.log('JSON Bench for ' + omc.OpsiMethodName +
          ' ' + copy(omc.getJsonUrlString, pos(',', omc.getJsonUrlString) +
          1, 50) + ' Start: ' + FormatDateTime('hh:nn:ss:zzz', startTime) +
          ' Time: ' + FormatDateTime('hh:nn:ss:zzz', now - startTime), LLinfo);
        LogDatei.log('JSON Call: ' + omc.getJsonUrlString +
          ' Time: ' + FormatDateTime('hh:nn:ss:zzz', now - startTime), LLDebug2);
      end;

      testResult := IdHTTP.ResponseText;
      cookieVal := IdHTTP.Response.RawHeaders.Values['Set-Cookie'];
      //LogDatei.DependentAdd('JSON retrieveJSONObject: after IdHTTP', LLDebug2);
      posColon := -1;
      if cookieVal <> '' then
        posColon := pos(';', cookieVal);

      if posColon > 0 then
        FSessionId := copy(cookieVal, 1, posColon - 1)
      else
        FSessionId := '';

      //LogDatei.DependentAdd('JSON retrieveJSONObject: after cookie', LLDebug2);
    except
      on E: Exception do
      begin
        errorOccured := True;
        FError := FError + '->retrieveJSONObject:2 : ' + E.Message;
        LogDatei.log('Exception in retrieveJSONObject:2: ' +
                  e.message, LLdebug2);
      end;
    end;

    if not ErrorOccured then
    begin
      mymemorystream.Position := 0;
      //LogDatei.DependentAdd('JSON retrieveJSONObject: memorystream reseted', LLDebug2);
      resultLines.Clear;
      ResultLines.LoadFromStream(mymemorystream);
      //LogDatei.DependentAdd('JSON retrieveJSONObject: resultlines loaded', LLDebug2);
      // should be one line

      if ResultLines.Count < 1 then
      begin
        FError := FError +
          '-> unexpected Result from webservice in retrieveJSONObject, number of lines: '
          +
          IntToStr(ResultLines.Count);
        LogDatei.log('Error: Got in stream: ' + mymemorystream.ReadAnsiString, LLError);
      end
      else
      begin
        Result := SO(ResultLines.Strings[0]);
        //LogDatei.DependentAdd('JSON retrieveJSONObject: result loaded', LLDebug2);
        if Result = nil then
          LogDatei.DependentAdd('JSON retrieveJSONObject: result nil', LLError)
        else
        begin

          if not (Result.N['error'].IsType(stNull)) then
          begin
            FError := FError + '-> ' + Result.N['error'].AsJSon();
            jO := Result.O['error'];
            getStringlistFromJsonObject(jO, fErrorInfo);
            Result := nil;
          end;
        end;
      end;

    end;

  except
    on E: Exception do
    begin
      errorOccured := True;
      FError := FError + '-> retrieveJSONObject:1: ' + E.Message;
      LogDatei.log('Exception in retrieveJSONObject:1: ' +
                  e.message, LLdebug2);
    end;
  end;

  if Result = nil then
  begin
    if logging then
      LogDatei.log('Error: retrieveJSONObject --- opsi service problem ---' +
        FError, LLerror);
    if pos('10054', FError) > 0 //Socket error, connection reset by peer
    then
    begin
      LogDatei.log('trying to rebuild connection', LLnotice);
      sleep(1000);
      createSocket;
      retrieveJSONObject(omc, logging, False);
    end
    else
      LogDatei.log('Received (first 512): ' + copy(ResultLines.Strings[0], 1, 512), LLerror);
  end;
end;

function TJsonThroughHTTPS.retrieveJSONArray(const omc: TOpsiMethodCall): TSuperArray;
begin
  Result := retrieveJSONArray(omc, True);
end;


function TJsonThroughHTTPS.retrieveJSONArray(const omc: TOpsiMethodCall;
  logging: boolean): TSuperArray;
begin
  Result := retrieveJSONArray(omc, logging, True);
end;

function TJsonThroughHTTPS.retrieveJSONArray(const omc: TOpsiMethodCall;
  logging: boolean; retry: boolean): TSuperArray;
begin
  Result := retrieveJSONArray(omc, logging, True, False);
end;

function TJsonThroughHTTPS.retrieveJSONArray(const omc: TOpsiMethodCall;
  logging: boolean; retry: boolean; readOmcMap: boolean): TSuperArray;

var
  errorOccured: boolean;
  cookieVal: string;
  posColon: integer;
  s: string;
  t: string;
  jO, jO1: ISuperObject;
  ///jA :  TSuperArray;
  utf8str: UTF8String;
  sourcestringlist: TStrings;
  sendstream, ReceiveStream: TMemoryStream;
  CompressionSendStream: TCompressionStream;
  CompressionReceiveStream: TDeCompressionStream;
  buffer: ^byte;
  readcount: integer;
  compress: boolean;

begin
  errorOccured := False;
  Result := nil;
  resultlines.Clear;
  fErrorInfo.Clear;
  compress := False;

  try
    mymemorystream.Clear;

    makeurl(omc);

    try
      if FSessionId <> '' then
      begin
        //IdHttp.Request.RawHeaders.Add('Cookie');
        IdHttp.Request.RawHeaders.Values['Cookie'] := FSessionId;
      end;
      testresult := IdHttp.Request.RawHeaders.Text;

      if FSessionId <> '' then
        // not the first call and we log
        if logging then
          //LogDatei.DependentAdd (DateTimeToStr(now) + ' JSON service request ' + Furl + ' '+ omc.FOpsiMethodName, LLnotice);
          LogDatei.DependentAdd('JSON service request ' + Furl + ' ' +
            omc.FOpsiMethodName, LLinfo);


      if methodGet then
      begin
        utf8str := AnsiToUtf8(Furl);
        LogDatei.DependentAdd(' JSON service request ' + Furl, LLdebug2);
        IdHTTP.Get(utf8str, mymemorystream);
      end
      else
      begin
        if readOmcMap then
        begin
          s := omc.getJsonHashListString;
          utf8str := AnsiToUtf8(s);
          //sourcestringlist.add(utf8str);
          LogDatei.DependentAdd(' JSON service request Furl ' + Furl, LLdebug2);
          LogDatei.DependentAdd(' JSON service request str ' + utf8str, LLdebug2);

        end
        else
        begin
          sourcestringlist := TStringList.Create;
          s := omc.jsonUrlString;
          utf8str := AnsiToUtf8(s);
          //sourcestringlist.add(utf8str);
          LogDatei.DependentAdd(' JSON service request Furl ' + Furl, LLdebug2);
          LogDatei.DependentAdd(' JSON service request str ' + utf8str, LLdebug2);
        end;
        try
          sendstream := TMemoryStream.Create;
          ReceiveStream := TMemoryStream.Create;
          sendstream.Clear;
          // we assume opsi4
          compress := True;
          LogDatei.log('Using MimeType: ' + ContentTypeCompress, LLDebug2);
          (*
          if opsidata <> nil then
          begin
            //LogDatei.DependentAdd('getOpsiServiceVersion:'+opsidata.getOpsiServiceVersion, LLdebug2);
            if opsidata.getOpsiServiceVersion = '4' then
            begin
              compress := True;
              LogDatei.log('Using MimeType: ' + ContentTypeCompress, LLDebug2);
              //compress := false;
            end
            else
            begin
              compress := False;
              LogDatei.log('Using MimeType: ' + ContentTypeNoCompress, LLDebug2);
            end;
          end
          else
            compress := False;
          *)
          if compress then
          begin
            IdHttp.Request.ContentType := ContentTypeCompress;
            IdHttp.Request.ContentEncoding := ContentEncodingCommpress;
            IdHttp.Request.Accept := AcceptCompress;
            IdHttp.Request.AcceptEncoding := AcceptEncodingCompress;
            LogDatei.log('HTTP Header: ' + IdHttp.Request.RawHeaders.Text, LLDebug2);
            CompressionSendStream := TCompressionStream.Create(clMax, sendstream);
            CompressionSendStream.Write(utf8str[1], length(utf8str));
            CompressionSendStream.Free;
            //writeln('ddebug: bpost');
            IdHTTP.Post(Furl, sendstream, ReceiveStream);
            //writeln('ddebug: apost');
            ReceiveStream.Seek(0, 0);
            CompressionReceiveStream := TDeCompressionStream.Create(ReceiveStream);
            GetMem(buffer, 655360);
            repeat
              FillChar(buffer^, 655360, ' ');
              readcount := CompressionReceiveStream.Read(buffer^, 655360);
              if readcount > 0 then
                mymemorystream.Write(buffer^, 655360);
            until readcount < 655360;
            CompressionReceiveStream.Free;
            FreeMem(buffer);
          end
          else
          begin
            IdHttp.Request.ContentType := ContentTypeNoCompress;
            IdHttp.Request.ContentEncoding := ContentEncodingNoCommpress;
            IdHttp.Request.Accept := AcceptNoCompress;
            IdHttp.Request.AcceptEncoding := AcceptEncodingNoCompress;
            LogDatei.log('HTTP Header: ' + IdHttp.Request.RawHeaders.Text, LLDebug2);
            sendstream.Write(utf8str[1], length(utf8str));
            IdHTTP.Post(Furl, sendstream, mymemorystream);
          end;
        except
          on e: Exception do
          begin
            LogDatei.DependentAdd('Exception in retrieveJSONObject0: ' +
              e.message, LLdebug2);
            //writeln('ddebug: Exception in retrieveJSONObject0: ' + e.message);
            if e.message = 'HTTP/1.1 401 Unauthorized' then
              FValidCredentials := False;
            t := s;
            // retry with other parameters
            if ContentTypeCompress = 'application/json' then
            begin
              ContentTypeCompress := 'gzip-application/json-rpc';
              AcceptCompress := 'gzip-application/json-rpc';
              ContentTypeNoCompress := 'application/json-rpc';
              AcceptNoCompress := 'application/json-rpc';
              ContentEncodingNoCommpress := '';
              ContentEncodingCommpress := '';
              AcceptEncodingCompress := '';
              AcceptEncodingNoCompress := '';
            end
            else
            begin
              ContentTypeCompress := 'application/json';
              AcceptCompress := '';
              ContentTypeNoCompress := 'application/json';
              AcceptNoCompress := '';
              ContentEncodingNoCommpress := '';
              ContentEncodingCommpress := 'deflate';
              AcceptEncodingCompress := 'deflate';
              AcceptEncodingNoCompress := '';
            end;
            LogDatei.log('Changing to MimeType: ' + ContentTypeCompress, LLDebug);
            sendstream.Free;
            (****************)
            try
              sendstream := TMemoryStream.Create;
              ReceiveStream := TMemoryStream.Create;
              sendstream.Clear;
              // we assume opsi4
              compress := True;
              LogDatei.log('Using MimeType: ' + ContentTypeCompress, LLDebug2);
              (*
              if opsidata <> nil then
              begin
                //LogDatei.DependentAdd('getOpsiServiceVersion:'+opsidata.getOpsiServiceVersion, LLdebug2);
                if opsidata.getOpsiServiceVersion = '4' then
                begin
                  compress := True;
                  //compress := false;
                end
                else
                  compress := False;
              end
              else
                compress := False;
              *)
              if compress then
              begin
                IdHttp.Request.ContentType := ContentTypeCompress;
                IdHttp.Request.ContentEncoding := ContentEncodingCommpress;
                IdHttp.Request.Accept := AcceptCompress;
                IdHttp.Request.AcceptEncoding := AcceptEncodingCompress;
                CompressionSendStream := TCompressionStream.Create(clMax, sendstream);
                CompressionSendStream.Write(utf8str[1], length(utf8str));
                CompressionSendStream.Free;
                //writeln('ddebug: bpost');
                IdHTTP.Post(Furl, sendstream, ReceiveStream);
                //writeln('ddebug: apost');
                ReceiveStream.Seek(0, 0);
                CompressionReceiveStream := TDeCompressionStream.Create(ReceiveStream);
                GetMem(buffer, 655360);
                repeat
                  FillChar(buffer^, 655360, ' ');
                  readcount := CompressionReceiveStream.Read(buffer^, 655360);
                  if readcount > 0 then
                    mymemorystream.Write(buffer^, 655360);
                until readcount < 655360;
                CompressionReceiveStream.Free;
                FreeMem(buffer);
              end
              else
              begin
                IdHttp.Request.ContentType := ContentTypeNoCompress;
                IdHttp.Request.ContentEncoding := ContentEncodingNoCommpress;
                IdHttp.Request.Accept := AcceptNoCompress;
                IdHttp.Request.AcceptEncoding := AcceptEncodingNoCompress;
                sendstream.Write(utf8str[1], length(utf8str));
                IdHTTP.Post(Furl, sendstream, mymemorystream);
              end;
            except
              on e: Exception do
              begin
                LogDatei.DependentAdd('Exception in retrieveJSONObject0: ' +
                  e.message, LLdebug2);
                //writeln('ddebug: Exception in retrieveJSONObject0: ' + e.message);
                if e.message = 'HTTP/1.1 401 Unauthorized' then
                  FValidCredentials := False;
                t := s;
              end;
            end;

            (****************)
          end;
        end;
        sourcestringlist.Free;
        sendstream.Free;
      end;


      testResult := IdHTTP.ResponseText;
      cookieVal := IdHTTP.Response.RawHeaders.Values['Set-Cookie'];

      posColon := -1;
      if cookieVal <> '' then
        posColon := pos(';', cookieVal);

      if posColon > 0 then
        FSessionId := copy(cookieVal, 1, posColon - 1)
      else
        FSessionId := '';

    except
      on E: Exception do
      begin
        errorOccured := True;
        FError := E.Message;
      end;
    end;

    if not ErrorOccured then
    begin
      mymemorystream.Position := 0;
      ResultLines.LoadFromStream(mymemorystream);
      // should be one line

      if ResultLines.Count < 1 then
      begin
        FError := 'unexpected Result from webservice in retrieveJSONArray, number of lines: '
          + IntToStr(ResultLines.Count);
      end
      else
      begin
        jO := SO(ResultLines.Strings[0]);


        testresult := jO.AsJSon(True, True);
        //LogDatei.DependentAdd (DateTimeToStr(now) + ' JSON result ' + testresult ,  LLnotice);
        //jO := result.geTSuperObject(0);
        testresult := jO.S['error'];

        if not (jO.N['error'].IsType(stNull)) then
        begin
          FError := jO.S['error'];
          jO1 := jO.O['error'];
          getStringlistFromJsonObject(jO1, fErrorInfo);
          Result := nil;
        end;
      end;
    end;

  except
    on E: Exception do
    begin
      errorOccured := True;
      FError := E.Message;
    end;
  end;

  if Result = nil then
  begin
    if logging then
      LogDatei.DependentAddError('Error: --- opsi service problem ---' +
        FError, LLerror);

    if pos('10054', FError) > 0 //Socket error, connection reset by peer
    then
    begin
      LogDatei.DependentAdd('trying to rebuild connection', LLnotice);
      sleep(1000);
      createSocket;
      retrieveJSONArray(omc, logging, False);
    end;
  end;
end;

function TJsonThroughHTTPS.retrieveJSONObjectByHttpPost(const instream: TMemoryStream;
  logging: boolean): ISuperObject;
var
  errorOccured: boolean;
  cookieVal: string;
  posColon: integer;
  //  s,t : String;
  sendstream, ReceiveStream: TMemoryStream;
  CompressionSendStream: TCompressionStream;
  CompressionReceiveStream: TDeCompressionStream;
  buffer: ^byte;
  readcount: integer;
  compress: boolean;

begin
  errorOccured := False;
  Result := nil;
  resultlines.Clear;

  try
    mymemorystream.Clear;

    try
      if FSessionId <> '' then
      begin
        //IdHttp.Request.RawHeaders.Add('Cookie');
        IdHttp.Request.RawHeaders.Values['Cookie'] := FSessionId;
      end;
      testresult := IdHttp.Request.RawHeaders.Text;

      if FSessionId <> '' then
        // not the first call and we log
        if logging then
          LogDatei.DependentAdd(DateTimeToStr(now) + ' JSON service request ' +
            Furl, LLnotice);
      //LogDatei.DependentAdd ('JSON service request ' + Furl + ' '+ omc.FOpsiMethodName, LLinfo);




      try
        sendstream := TMemoryStream.Create;
        ReceiveStream := TMemoryStream.Create;
        sendstream.Clear;
        // we assume opsi4
        compress := True;
        LogDatei.log('Using MimeType: ' + ContentTypeCompress, LLDebug2);

        if compress then
        begin
          LogDatei.log('Using MimeType: ' + ContentTypeCompress, LLDebug2);
          IdHttp.Request.ContentType := ContentTypeCompress;
          IdHttp.Request.ContentEncoding := ContentEncodingCommpress;
          IdHttp.Request.Accept := AcceptCompress;
          IdHttp.Request.AcceptEncoding := AcceptEncodingCompress;
          LogDatei.log('HTTP Header: ' + IdHttp.Request.RawHeaders.Text, LLDebug2);
          CompressionSendStream := TCompressionStream.Create(clMax, sendstream);
          Instream.Seek(0, 0);
          GetMem(buffer, 655360);
          repeat
            readcount := Instream.Read(buffer^, 655360);
            if readcount > 0 then
              CompressionSendStream.Write(buffer^, readcount);
          until readcount < 655360;
          CompressionSendStream.Free;
          IdHTTP.post(FserviceURL + '/rpc', sendstream, ReceiveStream);
          ReceiveStream.Seek(0, 0);
          CompressionReceiveStream := TDeCompressionStream.Create(ReceiveStream);
          repeat
            readcount := CompressionReceiveStream.Read(buffer^, 655360);
            if readcount > 0 then
              mymemorystream.Write(buffer^, readcount);
          until readcount < 655360;
          CompressionReceiveStream.Free;
          FreeMem(buffer);
        end
        else
        begin
          LogDatei.log('Using MimeType: ' + ContentTypeNoCompress, LLDebug2);
          IdHttp.Request.ContentType := ContentTypeNoCompress;
          IdHttp.Request.ContentEncoding := ContentEncodingNoCommpress;
          IdHttp.Request.Accept := AcceptNoCompress;
          IdHttp.Request.AcceptEncoding := AcceptEncodingNoCompress;
          LogDatei.log('HTTP Header: ' + IdHttp.Request.RawHeaders.Text, LLDebug2);
          IdHTTP.Post(FserviceURL + '/rpc', instream, mymemorystream);
        end;
        (*
        if opsidata <> nil then
        begin
          if opsidata.getOpsiServiceVersion = '4' then
          begin
            LogDatei.log('Using MimeType: ' + ContentTypeCompress, LLDebug2);
            IdHttp.Request.ContentType := ContentTypeCompress;
            IdHttp.Request.ContentEncoding:= ContentEncodingCommpress;
            IdHttp.Request.Accept := AcceptCompress;
            IdHttp.Request.AcceptEncoding:= AcceptEncodingCompress;
            LogDatei.log('HTTP Header: '+ IdHttp.Request.RawHeaders.Text,LLDebug2);
            CompressionSendStream := TCompressionStream.Create(clMax, sendstream);
            Instream.Seek(0, 0);
            GetMem(buffer, 655360);
            repeat
              readcount := Instream.Read(buffer^, 655360);
              if readcount > 0 then
                CompressionSendStream.Write(buffer^, readcount);
            until readcount < 655360;
            CompressionSendStream.Free;
            IdHTTP.post(FserviceURL + '/rpc', sendstream, ReceiveStream);
            ReceiveStream.Seek(0, 0);
            CompressionReceiveStream := TDeCompressionStream.Create(ReceiveStream);
            repeat
              readcount := CompressionReceiveStream.Read(buffer^, 655360);
              if readcount > 0 then
                mymemorystream.Write(buffer^, readcount);
            until readcount < 655360;
            CompressionReceiveStream.Free;
            FreeMem(buffer);
          end

          else
          begin
            LogDatei.log('Using MimeType: ' + ContentTypeCompress, LLDebug2);
            IdHttp.Request.ContentType := ContentTypeNoCompress;
            IdHttp.Request.ContentEncoding:= ContentEncodingNoCommpress;
            IdHttp.Request.Accept := AcceptNoCompress;
            IdHttp.Request.AcceptEncoding:= AcceptEncodingNoCompress;
            LogDatei.log('HTTP Header: '+ IdHttp.Request.RawHeaders.Text,LLDebug2);
            IdHTTP.Post(FserviceURL + '/rpc', instream, mymemorystream);
          end;
        end
        else
        begin
          IdHttp.Request.ContentType := ContentTypeNoCompress;
          IdHttp.Request.ContentEncoding:= ContentEncodingNoCommpress;
          IdHttp.Request.Accept := AcceptNoCompress;
          IdHttp.Request.AcceptEncoding:= AcceptEncodingNoCompress;
          LogDatei.log('HTTP Header: '+ IdHttp.Request.RawHeaders.Text,LLDebug2);
          IdHTTP.Post(FserviceURL + '/rpc', instream, mymemorystream);

        end;
        *)
      except
        LogDatei.DependentAdd(
          'Exception in retrieveJSONObjectByHttpPost: stream handling'
          , LLError);
        // retry with other parameters
        if ContentTypeCompress = 'application/json' then
        begin
          ContentTypeCompress := 'gzip-application/json-rpc';
          AcceptCompress := 'gzip-application/json-rpc';
          ContentTypeNoCompress := 'application/json-rpc';
          AcceptNoCompress := 'application/json-rpc';
          ContentEncodingNoCommpress := '';
          ContentEncodingCommpress := '';
          AcceptEncodingCompress := '';
          AcceptEncodingNoCompress := '';
        end
        else
        begin
          ContentTypeCompress := 'application/json';
          AcceptCompress := '';
          ContentTypeNoCompress := 'application/json';
          AcceptNoCompress := '';
          ContentEncodingNoCommpress := '';
          ContentEncodingCommpress := 'deflate';
          AcceptEncodingCompress := 'deflate';
          AcceptEncodingNoCompress := '';
        end;
        LogDatei.log('Changing to MimeType: ' + ContentTypeCompress, LLDebug);
        sendstream.Free;
        (************************)
        try
          sendstream := TMemoryStream.Create;
          ReceiveStream := TMemoryStream.Create;
          sendstream.Clear;
          // we assume opsi4
          compress := True;
          LogDatei.log('Using MimeType: ' + ContentTypeCompress, LLDebug2);

          if compress then
          begin
            LogDatei.log('Using MimeType: ' + ContentTypeCompress, LLDebug2);
            IdHttp.Request.ContentType := ContentTypeCompress;
            IdHttp.Request.ContentEncoding := ContentEncodingCommpress;
            IdHttp.Request.Accept := AcceptCompress;
            IdHttp.Request.AcceptEncoding := AcceptEncodingCompress;
            LogDatei.log('HTTP Header: ' + IdHttp.Request.RawHeaders.Text, LLDebug2);
            CompressionSendStream := TCompressionStream.Create(clMax, sendstream);
            Instream.Seek(0, 0);
            GetMem(buffer, 655360);
            repeat
              readcount := Instream.Read(buffer^, 655360);
              if readcount > 0 then
                CompressionSendStream.Write(buffer^, readcount);
            until readcount < 655360;
            CompressionSendStream.Free;
            IdHTTP.post(FserviceURL + '/rpc', sendstream, ReceiveStream);
            ReceiveStream.Seek(0, 0);
            CompressionReceiveStream := TDeCompressionStream.Create(ReceiveStream);
            repeat
              readcount := CompressionReceiveStream.Read(buffer^, 655360);
              if readcount > 0 then
                mymemorystream.Write(buffer^, readcount);
            until readcount < 655360;
            CompressionReceiveStream.Free;
            FreeMem(buffer);
          end
          else
          begin
            LogDatei.log('Using MimeType: ' + ContentTypeNoCompress, LLDebug2);
            IdHttp.Request.ContentType := ContentTypeNoCompress;
            IdHttp.Request.ContentEncoding := ContentEncodingNoCommpress;
            IdHttp.Request.Accept := AcceptNoCompress;
            IdHttp.Request.AcceptEncoding := AcceptEncodingNoCompress;
            LogDatei.log('HTTP Header: ' + IdHttp.Request.RawHeaders.Text, LLDebug2);
            IdHTTP.Post(FserviceURL + '/rpc', instream, mymemorystream);
          end;
          (*
          if opsidata <> nil then
          begin
            if opsidata.getOpsiServiceVersion = '4' then
            begin
              IdHttp.Request.ContentType := ContentTypeCompress;
              IdHttp.Request.ContentEncoding:= ContentEncodingCommpress;
              IdHttp.Request.Accept := AcceptCompress;
              IdHttp.Request.AcceptEncoding:= AcceptEncodingCompress;
              LogDatei.log('HTTP Header: '+ IdHttp.Request.RawHeaders.Text,LLDebug2);
              CompressionSendStream := TCompressionStream.Create(clMax, sendstream);
              Instream.Seek(0, 0);
              GetMem(buffer, 655360);
              repeat
                readcount := Instream.Read(buffer^, 655360);
                if readcount > 0 then
                  CompressionSendStream.Write(buffer^, readcount);
              until readcount < 655360;
              CompressionSendStream.Free;
              IdHTTP.post(FserviceURL + '/rpc', sendstream, ReceiveStream);
              ReceiveStream.Seek(0, 0);
              CompressionReceiveStream := TDeCompressionStream.Create(ReceiveStream);
              repeat
                readcount := CompressionReceiveStream.Read(buffer^, 655360);
                if readcount > 0 then
                  mymemorystream.Write(buffer^, readcount);
              until readcount < 655360;
              CompressionReceiveStream.Free;
              FreeMem(buffer);
            end

            else
            begin
              IdHttp.Request.ContentType := ContentTypeNoCompress;
              IdHttp.Request.ContentEncoding:= ContentEncodingNoCommpress;
              IdHttp.Request.Accept := AcceptNoCompress;
              IdHttp.Request.AcceptEncoding:= AcceptEncodingNoCompress;
              LogDatei.log('HTTP Header: '+ IdHttp.Request.RawHeaders.Text,LLDebug2);
              IdHTTP.Post(FserviceURL + '/rpc', instream, mymemorystream);
            end;
          end
          else
          begin
            IdHttp.Request.ContentType := ContentTypeNoCompress;
            IdHttp.Request.ContentEncoding:= ContentEncodingNoCommpress;
            IdHttp.Request.Accept := AcceptNoCompress;
            IdHttp.Request.AcceptEncoding:= AcceptEncodingNoCompress;
            LogDatei.log('HTTP Header: '+ IdHttp.Request.RawHeaders.Text,LLDebug2);
            IdHTTP.Post(FserviceURL + '/rpc', instream, mymemorystream);

          end;
          *)
        except
          LogDatei.DependentAdd(
            'Exception in retrieveJSONObjectByHttpPost: stream handling'
            , LLError);
        end;
        (************************)
      end;

      testResult := IdHTTP.ResponseText;
      cookieVal := IdHTTP.Response.RawHeaders.Values['Set-Cookie'];

      posColon := -1;
      if cookieVal <> '' then
        posColon := pos(';', cookieVal);

      if posColon > 0 then
        FSessionId := copy(cookieVal, 1, posColon - 1)
      else
        FSessionId := '';

    except
      on E: Exception do
      begin
        errorOccured := True;
        FError := E.Message;
      end;
    end;
    if not ErrorOccured then
    begin
      mymemorystream.Position := 0;
      ResultLines.LoadFromStream(mymemorystream);
      // should be one line

      if ResultLines.Count < 1 then
      begin
        FError :=
          'unexpected Result from webservice in retrieveJSONObjectByHttpPost, number of lines: '
          + IntToStr(ResultLines.Count);
      end
      else
      begin
        Result := SO(ResultLines.Strings[0]);
        testresult := Result.AsJSon(True, True);
        //LogDatei.DependentAdd (DateTimeToStr(now) + ' JSON result ' + testresult ,  LLnotice);
        testresult := Result.S['error'];
        if not (Result.N['error'].IsType(stNull)) then
        begin
          FError := Result.S['error'];
          Result := nil;
        end;
      end;

    end;

  except
    on E: Exception do
    begin
      errorOccured := True;
      FError := E.Message;
    end;
  end;

  if Result = nil then
    if logging then
      LogDatei.DependentAddError('Error: --- opsi service problem ---' +
        FError, LLerror);

end;



function TJsonThroughHTTPS.getMapResult(const omc: TOpsiMethodCall): TStringList;
var
  jO, jOResult: ISuperObject;
  ///i : Integer;
begin
  Result := TStringList.Create; // default value is an empty string list

  jO := retrieveJSONObject(omc);

  if jO = nil then
  begin
    Result := nil;
    LogDatei.DependentAdd('retrieveJSONObject = nil in getMapResult', LLdebug2);
  end
  else
  begin
    testresult := jO.AsJSon(False, True);
    jOResult := jO.O['result'];

    if (jOResult <> nil) then
    begin
      testresult := jOResult.AsJSon(False, True);
      getStringlistFromJsonObject(jOResult, Result);
      testresult := Result.Text;
    end;
  end;
end;

function TJsonThroughHTTPS.getHashListResult(const omc: TOpsiMethodCall): TStringList;
var
  jO, jOResult: ISuperObject;
  ///i : Integer;
begin
  Result := TStringList.Create; // default value is an empty string list

  jO := retrieveJSONObject(omc, True, True, True);
  //testresult := jO.toString;

  if jO = nil then
  begin
    exit;
  end;

  jOResult := jO.O['result'];
  //testresult := jOResult.toString;


  if (jOResult <> nil) then
  begin
    getStringlistFromJsonObject(jOResult, Result);
    testresult := Result.Text;
  end;
end;


function TJsonThroughHTTPS.getListResult(const omc: TOpsiMethodCall): TStringList;
var
  jO, jO1: ISuperObject;
  jA: TSuperArray;
  i: integer;
  testresult: string;
begin
  Result := TStringList.Create;

  jO := retrieveJSONObject(omc);

  if jO = nil then
  begin
    exit;
  end;
  testresult := jO.AsJSon(False, False);
  jO1 := jO.O['result'];
  jA := jO.O['result'].AsArray;
  if jA.Length > 0 then
    testresult := jA.S[0];

  if jA <> nil then
  begin
    //testresult := jA.
    for i := 0 to jA.Length - 1 do
    begin
      testresult := jA.S[i];
      Result.append(jA.S[i]);
    end;
  end;
end;

function TJsonThroughHTTPS.getSubListResult(const omc: TOpsiMethodCall;
  subkey: string): TStringList;
var
  jO, jO1, jO2: ISuperObject;
  jA, jA1: TSuperArray;
  i: integer;
  testresult: string;
begin
  try
    Result := TStringList.Create;

    jO := retrieveJSONObject(omc);

    if jO = nil then
    begin
      LogDatei.DependentAdd('Got no object from web service', LLWarning);
      exit;
    end;
    // we have something like
    // {"reslut":[{subkey:["a","r","r","a","y"]}]}
    // get value of path 'result' as Array (which contains one object)
    jA := jO.A['result'];
    // get this single object
    jO1 := jA.O[0];
    // get from this object the value for the key: subkey as array
    jA1 := jO1.A[subkey];
    (*
    if jA1.Length > 0 then
    begin
      testresult := jA1.S[0];
      LogDatei.DependentAdd('ja1 as json: '+testresult, LLDebug2);
    end;
     *)
    if jA1 <> nil then
    begin
      //testresult := jA.
      for i := 0 to jA1.Length - 1 do
      begin
        testresult := jA1.S[i];
        Result.append(jA1.S[i]);
      end;
    end;

  except
    on E: Exception do
    begin
      Logdatei.DependentAdd('Exception in getSubListResult, system message: "' +
        E.Message + '"', LLwarning);
      Result := nil;
    end
  end;
end;


function TJsonThroughHTTPS.getStringListResult(const omc: TOpsiMethodCall): TStringList;
var
  jO, jO1: ISuperObject;
  ///jA : TSuperArray;
  i: integer;
begin
  Result := TStringList.Create;

  jO := retrieveJSONObject(omc);
  //testresult := jO.toString;


  if jO = nil then
  begin
    exit;
  end;

  jO1 := jO.O['result'];

  if jO1 <> nil then
  begin
    testresult := jO1.AsJSon(True, True);
    for i := 0 to jO1.asArray.Length - 1 do
    begin
      testresult := jO1.asArray.O[i].AsJSon(True, True);
      Result.add(jO1.asArray.S[i]);
    end;
  end;
end;

function TJsonThroughHTTPS.getFileFromDepot(filename: string;
  toStringList: boolean; var ListResult: TStringList): boolean;
var
  resultstring, localurl: string;
  utf8str: UTF8String;
begin
  try
    Result := False;
    mymemorystream.Clear;
    if FSessionId <> '' then
    begin
      //IdHttp.Request.RawHeaders.Add('Cookie');
      IdHttp.Request.RawHeaders.Values['Cookie'] := FSessionId;
    end;
    LogDatei.DependentAdd('Sessionid ' + FSessionId, LLdebug2);
    testresult := IdHttp.Request.RawHeaders.Text;
    LogDatei.log('HTTP Header: ' + IdHttp.Request.RawHeaders.Text, LLDebug2);
    if pos('/rpc', FserviceURL) = 0 then
      localurl := FserviceURL
    else
      localurl := copy(FserviceURL, 0, pos('/rpc', FserviceURL));
    utf8str := AnsiToUtf8(localurl + '/depot/' + filename);
    Logdatei.DependentAdd('Loading file: ' + utf8str, LLDebug2);
    IdHttp.Request.ContentType := 'text';
    IdHttp.Request.Accept := 'text';
    IdHTTP.Get(utf8str, mymemorystream);
    mymemorystream.Position := 0;
    //LogDatei.DependentAdd('JSON retrieveJSONObject: memorystream reseted', LLDebug2);
    // ResultLines.LoadFromStream(mymemorystream);

    //resultstring := IdHTTP.Get(FserviceURL+'/'+filename);
    if toStringList then
      ListResult.LoadFromStream(mymemorystream);
    Result := True;
  except
    on E: Exception do
    begin
      Logdatei.log('Exception in getFileFromDepot, system message: "' +
        E.Message + '"', LLError);
      Result := False;
    end
  end;
end;



// TOpsi4Data =========================

constructor TOpsi4Data.Create;
begin
  //inherited create;
  actualclient := '';
  FJsonExecutioner := nil;
  FSortByServer := False;
  FSslProtocol := sslvTLSv1_2;
end;

destructor TOpsi4Data.Destroy;
begin
  inherited Destroy;
end;

function TOpsi4Data.decreaseSslProtocol: boolean;
begin
  try
    Result := True;
    FSslProtocol := TIdSSLVersion(Dec(FSslProtocol));
  except
    on E: Exception do
    begin
      Logdatei.log('Exception in decreaseSslProtocol, system message: "' +
        E.Message + '"', LLError);
      Logdatei.log('Exception in decreaseSslProtocol, last Protokoll: "' +
        GetEnumName(TypeInfo(FSslProtocol), integer(FSslProtocol)) + '"', LLError);
      Result := False;
    end
  end;
end;


procedure TOpsi4Data.initOpsiConf(serviceURL, username, password: string);
begin
  initOpsiConf(serviceURL, username, password, '', '', '');
  (*
  if FJsonExecutioner <> nil then
    FJsonExecutioner.Free;
  FjsonExecutioner := TJsonThroughHTTPS.Create(serviceUrl, username, password);
  ProfildateiChange := False;
  FServiceUrl := serviceURL;
  FDepotId := '';
  FOpsiServiceVersion := '4';
  FOpsiModules := nil;
  FOpsiInformation := nil;
  FProductOnClientIndex := TStringList.Create;
  FProductStates := TStringList.Create;
  *)
end;

procedure TOpsi4Data.initOpsiConf(serviceURL, username, password, sessionid: string);
begin
  initOpsiConf(serviceURL, username, password, sessionid, '', '');
  (*
  if FJsonExecutioner <> nil then
    FJsonExecutioner.Free;
  FjsonExecutioner := TJsonThroughHTTPS.Create(serviceUrl, username,
    password, sessionid);
  ProfildateiChange := False;
  FServiceUrl := serviceURL;
  FDepotId := '';
  FOpsiServiceVersion := '4';
  FOpsiModules := nil;
  FOpsiInformation := nil;
  FProductOnClientIndex := TStringList.Create;
  FProductStates := TStringList.Create;
  *)
end;

procedure TOpsi4Data.initOpsiConf(serviceURL, username, password,
  sessionid, ip, port: string);
begin
  try
    if FJsonExecutioner <> nil then
      FJsonExecutioner.Free;
    FjsonExecutioner := TJsonThroughHTTPS.Create(serviceUrl, username,
      password, sessionid, ip, port);
    ProfildateiChange := False;
    FServiceUrl := serviceURL;
    FDepotId := '';
    FOpsiServiceVersion := '4';
    FOpsiModules := nil;
    FOpsiInformation := nil;
    FProductOnClientIndex := TStringList.Create;
    FProductStates := TStringList.Create;
  except
    on E: Exception do
    begin
      LogDatei.DependentAdd('Exception in initOpsiConf: ' + e.message, LLError);
    end;
  end;
end;


function TOpsi4Data.getOpsiModules: ISuperObject;
  //  lazily initalizes FOpsiModules
var
  omc: TOpsiMethodCall;
  //teststring : string;
  //testbool : boolean;
begin
  FOpsiModules := nil;
  FOpsiInformation := nil;
  if FOpsiInformation = nil then
  begin
    omc := TOpsiMethodCall.Create('backend_info', []);
    if omc <> nil then
    begin
      try
        FOpsiInformation := FjsonExecutioner.retrieveJsonObject(omc);
        if (FOpsiInformation <> nil) and (FOpsiInformation.O['result'] <>
          nil) and (FopsiInformation.O['result'].O['modules'] <> nil) then
          FOpsiModules := FopsiInformation.O['result'].O['modules']
        else
          LogDatei.DependentAdd('Problem getting modules from service', LLerror);
      except
        LogDatei.DependentAdd('Exeception getting modules from service', LLerror);
      end;
    end
    else
      LogDatei.DependentAdd('Problem creating OpsiMethodCall backend_info', LLerror);
  end;
  //teststring := FOpsiInformation.AsJSon(true,true);
  //teststring := FOpsiInformation.O['result'].AsJSon(true,true);
  //teststring := FOpsiInformation.O['result'].O['modules'].AsJSon(true,true);
  //teststring := FOpsiModules.AsJSon(true,true);
  //teststring := FOpsiModules.S['license_management'];
  //testbool := FOpsiModules.B['license_management'];
  Result := FOpsiModules;
  omc.Free;
end;


function TOpsi4Data.withLicenceManagement: boolean;
var
  mymodules: ISuperObject;
begin
  try
    mymodules := getOpsiModules;
    LogDatei.DependentAdd('modules found', LLDebug);
    if mymodules <> nil then
      Result := mymodules.B['license_management']
    else
    begin
      LogDatei.DependentAdd('licence management info not found (modules = nil)',
        LLWarning);
      Result := False;
    end;
  except
    LogDatei.DependentAdd(
      'licence management info not found (exception in withLicenceManagement)',
      LLWarning);
    Result := False;
  end;
end;

function TOpsi4Data.withRoamingProfiles: boolean;
var
  mymodules: ISuperObject;
begin
  try
    mymodules := getOpsiModules;
    LogDatei.DependentAdd('modules found', LLDebug);
    if mymodules <> nil then
      Result := mymodules.B['roaming_profiles']
    else
    begin
      LogDatei.DependentAdd('Roaming Profiles info not found (modules = nil)',
        LLWarning);
      Result := False;
    end;
  except
    LogDatei.DependentAdd(
      'Roaming Profiles info not found (exception in withRoamingProfiles)', LLWarning);
    Result := False;
  end;
end;

function TOpsi4Data.isConnected: boolean;
var
  omc: TOpsiMethodCall;
begin
  Result := False;
  FOpsiInformation := nil;
  if FOpsiInformation = nil then
  begin
    omc := TOpsiMethodCall.Create('backend_info', []);
    if omc <> nil then
    begin
      try
        FOpsiInformation := FjsonExecutioner.retrieveJsonObject(omc);
        if (FOpsiInformation <> nil) and (FOpsiInformation.O['result'] <>
          nil) then
          Result := True
        else
          LogDatei.DependentAdd('Problem getting backend_info from service', LLerror);
      except
        LogDatei.DependentAdd('Exeception getting backend_info from service', LLerror);
      end;
    end
    else
      LogDatei.DependentAdd('Problem creating OpsiMethodCall backend_info', LLerror);
    omc.Free;
  end;
end;


function TOpsi4Data.linuxAgentActivated: boolean;
var
  mymodules: ISuperObject;
begin
  try
    mymodules := getOpsiModules;
    LogDatei.DependentAdd('modules found', LLDebug);
    if mymodules <> nil then
      Result := mymodules.B['linux_agent']
    else
    begin
      LogDatei.DependentAdd('linux_agent info not found (modules = nil)',
        LLWarning);
      Result := False;
    end;
  except
    LogDatei.DependentAdd(
      'linux_agent info not found (exception in linuxAgentActivated)', LLWarning);
    Result := False;
  end;
end;


procedure TOpsi4Data.saveOpsiConf;
begin
  ProfildateiChange := False;
end;


function TOpsi4Data.getOpsiServiceVersion: string;
begin
  Result := FOpsiServiceVersion;
end;



{
function TOpsi4Data.retrieve (const omc: TOpsiMethodCall) : String;
begin
  if FJsonExecutioner.retrieveJSONObject(omc) <> nil
  then
     result := FjsonExecutioner.resultLines[0]
  else
     result := 'error: ' + FjsonExecutioner.lastError;
end;
}

function TOpsi4Data.checkAndRetrieve(const omc: TOpsiMethodCall;
  var errorOccured: boolean): string;
begin
  errorOccured := False;
  Result := '';
  if FJsonExecutioner.retrieveJSONObject(omc) <> nil then
    Result := FjsonExecutioner.resultLines[0]
  else
  begin
    errorOccured := True;
    Result := FjsonExecutioner.lastError;
  end;

  FServiceLastErrorInfo := FjsonExecutioner.lastErrorInfo;
end;

function TOpsi4Data.checkAndRetrieveString(const omc: TOpsiMethodCall;
  var errorOccured: boolean): string;
var
  resultO: ISuperObject;
begin
  errorOccured := False;
  try
    if FJsonExecutioner <> nil then
    begin
      resultO := FJsonExecutioner.retrieveJSONObject(omc);
      if resultO <> nil then
        Result := resultO.S['result']
      else
      begin
        errorOccured := True;
        Result := FjsonExecutioner.lastError;
      end;
      FServiceLastErrorInfo := FjsonExecutioner.lastErrorInfo;
    end
    else
    begin
      errorOccured := True;
      Result := 'JSON connection not established';
    end;
  except
    errorOccured := True;
    Result := 'JSON connection not established';
  end;
end;



function TOpsi4Data.checkAndRetrieveMap
  (const omc: TOpsiMethodCall; var errorOccured: boolean): TStringList;
begin
  errorOccured := False;

  Result := FJsonExecutioner.getMapResult(omc);
  // cannot be nil, is at least an empty String list

  if Result = nil then
  begin
    errorOccured := True;
    //result := FjsonExecutioner.lastError;
  end;

  FServiceLastErrorInfo := FjsonExecutioner.lastErrorInfo;

end;

function TOpsi4Data.checkAndRetrieveHashList
  (const omc: TOpsiMethodCall; var errorOccured: boolean): TStringList;
begin
  errorOccured := False;

  Result := FJsonExecutioner.getHashListResult(omc);
  // cannot be nil, is at least an empty String list

  if Result = nil then
  begin
    errorOccured := True;
    //result := FjsonExecutioner.lastError;
  end;

  FServiceLastErrorInfo := FjsonExecutioner.lastErrorInfo;

end;


function TOpsi4Data.checkAndRetrieveList
  (const omc: TOpsiMethodCall; var errorOccured: boolean): TStringList;
begin
  errorOccured := False;

  Result := FJsonExecutioner.getListResult(omc);

  FServiceLastErrorInfo := FjsonExecutioner.lastErrorInfo;
end;

function TOpsi4Data.checkAndRetrieveStringList
  (const omc: TOpsiMethodCall; var errorOccured: boolean): TStringList;
begin
  errorOccured := False;

  Result := FJsonExecutioner.getStringListResult(omc);

  FServiceLastErrorInfo := FjsonExecutioner.lastErrorInfo;

end;



function TOpsi4Data.stateToString(state: TProductState): string;
begin
  Result := '';
  case state of
    tpsUndefined: Result := 'undefined';
    tpsNotInstalled: Result := 'not_installed';
    //tpsInstalling   :   result := 'installing';
    tpsInstalled: Result := 'installed';
    //tpsFailed       :   result := 'failed';
  end;
end;

function TOpsi4Data.actionToString(action: TAction): string;
begin
  Result := 'undefined';
  case action of
    tacNull: Result := 'none';
    tacSetup: Result := 'setup';
    tacUpdate: Result := 'update';
    tacDeinstall: Result := 'uninstall';
    tacAlways: Result := 'always';
    tacOnce: Result := 'once';
    tacCustom: Result := 'custom';
    tacLogin: Result := 'login';
  end;
end;


function TOpsi4Data.actionRequestToString(actionRequest: TActionRequest): string;
begin
  Result := '';
  case actionRequest of
    tapNull: Result := 'none';
    tapNull_byPolicy: Result := 'none_by_policy';
    tapUndefined: Result := 'undefined';
    tapSetup: Result := 'setup';
    tapSetup_byPolicy: Result := 'setup_by_policy';
    tapUpdate: Result := 'update';
    tapUpdate_byPolicy: Result := 'update_by_policy';
    tapDeinstall: Result := 'uninstall';
    tapDeinstall_byPolicy: Result := 'uninstall_by_policy';
    tapAlways: Result := 'always';
    tapOnce: Result := 'once';
    tapCustom: Result := 'custom';
    tap_byPolicy: Result := 'by_policy';
  end;
end;



procedure TOpsi4Data.setActualClient(computername: string);
var
  omc: TOpsiMethodCall;
  errorOccured: boolean;
begin
  //inherited setActualClient (computername);
  actualClient := computername;
  omc := TOpsiMethodCall.Create('getDepotId', [computername]);
  FDepotId := checkAndRetrieveString(omc, errorOccured);
  omc.Free;

  //FProductStates := getMapOfProductStates;
  //FProductActionRequests := getMapOfProductActionRequests;
  //FInstallableProducts := getInstallableProducts;
  //testresult := FInstallableProducts.text;
end;


function TOpsi4Data.stateStringToState(s: string): TProductState;
var
  sValue: string;
begin
  Result := tpsUndefined;

  sValue := lowercase(s);
  if sValue = 'installed' then
    Result := tpsInstalled
  //else if sValue = 'installing' then result := tpsInstalling
  else if sValue = 'not_installed' then
    Result := tpsNotInstalled;
  //else if sValue = 'failed' then result := tpsFailed
end;


function TOpsi4Data.actionRequestStringToAction(s: string): TAction;
var
  sValue: string;
begin
  Result := tacNull;
  sValue := lowercase(s);

  if (sValue = 'setup') or (sValue = 'setup_by_policy') then
    Result := tacSetup

  else if (sValue = 'update') or (sValue = 'update_by_policy') then
    Result := tacUpdate

  else if (sValue = 'uninstall') or (sValue = 'uninstall_by_policy') then
    Result := tacDeinstall

  else if (sValue = 'always') then
    Result := tacAlways

  else if (sValue = 'once') then
    Result := tacOnce

  else if (sValue = 'custom') then
    Result := tacCustom

  else if (sValue = 'login') then
    Result := tacLogin;

end;

function TOpsi4Data.actionRequestStringToActionRequest(s: string): TActionRequest;
var
  sValue: string;
begin
  Result := tapNull;
  sValue := lowercase(s);
  if (sValue = 'setup') then
    Result := tapSetup
  else if (sValue = 'setup_by_policy') then
    Result := tapSetup_byPolicy
  else if (sValue = 'update') then
    Result := tapUpdate
  else if (sValue = 'update_by_policy') then
    Result := tapUpdate_byPolicy
  else if (sValue = 'uninstall') then
    Result := tapDeinstall
  else if (sValue = 'uninstall_by_policy') then
    Result := tapDeinstall_byPolicy
  else if (sValue = 'always') then
    Result := tapAlways
  else if (sValue = 'once') then
    Result := tapOnce
  else if (sValue = 'custom') then
    Result := tapCustom;
end;

function TOpsi4Data.getMethodSignature(const methodname: string): TStringList;
var
  methodsList: TStringList;
  omc: TOpsiMethodCall;
  i, j: integer;
  methodEntryString: string;
  methodEntry, jO1: ISuperObject;
  Name: string;
  ///jA  : TSuperArray;
  slist: TStringList;

begin
  if mapOfMethodSignatures = nil then
  begin
    omc := TOpsiMethodCall.Create('getPossibleMethods_listOfHashes', []);
    methodsList := FjsonExecutioner.getStringListResult(omc);
    omc.Free;
    if methodsList <> nil then
    begin
      mapOfMethodSignatures := TStringList.Create;
      for i := 1 to methodsList.Count - 1 do
      begin
        methodEntryString := methodsList.Strings[i];
        methodEntry := SO(methodEntryString);
        Name := methodEntry.S['name'];

        //if name = 'getProduct_hash'
        //then
        //begin

        jO1 := methodEntry.O['params'];
        //testresult := methodEntry.getString('params');

        slist := TStringList.Create;
        if jO1 <> nil then
        begin
          testresult := jO1.AsJSon(True, True);
          for j := 0 to jO1.asArray.Length - 1 do
          begin
            //testresult := jA.get(j).toString;
            slist.add(jO1.asArray.O[i].AsJSon());
          end;
        end;

        mapOfMethodSignatures.addObject(Name, slist);

        //end;

      end;
    end;
  end;

  if (mapOfMethodSignatures = nil) or
    (mapOfMethodSignatures.indexOf(methodname) = -1) then
    Result := nil //TStringList.create
  else
    Result := TStringList(mapOfMethodSignatures.objects[mapOfMethodSignatures.indexOf(
      methodname)]);
end;




procedure TOpsi4Data.setActualProductName(const productname: string);
begin
  actualProduct := productname;
  logdatei.AktProduktId := productname;
end;

function TOpsi4Data.initProduct: boolean;
var
  omc: TOpsiMethodCall;
  str: SOString;
  jO: ISuperObject;
  ite: TSuperObjectIter;
  omcresult: TStringList;
begin
  Result := True;
  // reset FProductOnClient_aktobject
  FProductOnClient_aktobject := nil;
  omc := TOpsiMethodCall.Create('getProduct_hash', [actualProduct, FDepotId]);
  omcresult := FjsonExecutioner.getMapResult(omc);
  omc.Free;
  if omcresult = nil then
  begin
    logdatei.DependentAdd('Error: Opsi4Data.initProduct: getProduct_hash failed: retry',
      LLWarning);
    //ProcessMess;
    Sleep(500);
    omc := TOpsiMethodCall.Create('getProduct_hash', [actualProduct, FDepotId]);
    omcresult := FJsonExecutioner.getMapResult(omc);
    omc.Free;
  end;
  if omcresult = nil then
  begin
    ProductVars := TStringList.Create;
    Result := False;
    logdatei.DependentAdd('Opsi4Data.initProduct: getProduct_hash failed: giving up',
      LLError);
  end
  else
    ProductVars := omcresult;
  if ProductVars.Count > 0 then
  begin
    Logdatei.DependentAdd('in TOpsi4Data.initProduct : ' +
      ProductVars.Values['productId'], LLDebug);
    omc := TOpsiMethodCall.Create('productOnClient_getObjects',
      ['', '{"clientId": "' + actualClient + '", "productId": "' +
      Productvars.Values['productId'] + '", "productType": "LocalbootProduct"}']);
    mylist := FjsonExecutioner.getListResult(omc);
    omc.Free;
    if (mylist <> nil) then
      if mylist.Count >= 1 then
      begin
        jO := SO(mylist.Strings[0]);
        str := jO.AsJSon;
        //superobject.
        //FProductOnClient_aktobject := TSuperobject.Create(mylist.Items[0]);
        FProductOnClient_aktobject := SO(str);
        //FProductOnClient_aktobject := TSuperobject.create(str);
        //FProductOnClient_aktobject := SO(str);
        str := FProductOnClient_aktobject.AsJSon(False, False);
        str := FProductOnClient_aktobject.asJson(False, False);
        str := FProductOnClient_aktobject.S['"clientId"'];
        str := FProductOnClient_aktobject.AsObject.S['clientId'];
        str := FProductOnClient_aktobject.S['"clientId"'];
        if ObjectFindFirst(FProductOnClient_aktobject, ite) then
          repeat
            str := ite.key + ': ' + ite.val.AsJson;
          until not ObjectFindNext(ite);
        ObjectFindClose(ite);
        //FProductOnClient_aktobject := TSuperObject.create(str);
      end
      else
        {$IFDEF OPSIWINST}
      if runLoginScripts then
        {$ELSE}
        if False then
        {$ENDIF}
          LogDatei.DependentAdd('no productOnClient found for client: ' +
            actualClient + ' and product: ' + Productvars.Values['productId'], LLInfo)
        else
          LogDatei.DependentAdd('no productOnClient found for client: ' +
            actualClient + ' and product: ' + Productvars.Values['productId'], LLerror)
      else
        LogDatei.DependentAdd('no productOnClient found for depot: ' +
          actualClient + ' and product: ' + Productvars.Values['productId'], LLerror);
    //FProductOnClient_aktobject := FjsonExecutioner.retrieveJSONObject(omc);
  end
  else
    LogDatei.DependentAdd('no product_hash found for client: ' +
      FDepotId + ' and product: ' + actualProduct, LLerror);
end;


procedure TOpsi4Data.finishProduct;
begin
  ProductVars.Free;
end;

function TOpsi4Data.getLogFileName(const LogFilename: string): string;
begin
  Result := logfilename;
  // alternative source for logfilename not yet implemented
end;

procedure TOpsi4Data.finishOpsiConf;
var
  sendToLogFlag: boolean;
  omc: TOpsiMethodCall;
  jsonEntry: ISuperObject;
  sendtype: string;
begin
  sendToLogFlag := True;
  {$IFDEF OPSIWINST}
  if runLoginScripts then
  {$ELSE}
    if False then
  {$ENDIF}
      sendtype := 'userlogin'
    else
      sendtype := 'instlog';

  {$IFDEF OPSIWINST}
  if runproductlist then
    Logdatei.Appendmode := True;
  {$ENDIF OPSIWINST}

  if sendToLogFlag then
  begin
    {$IFDEF GUI}
    FBatchOberflaeche.setInfoLabel(rsSendLog);
    ProcessMess;
    Application.ProcessMessages;
    {$ENDIF}
    sendLog(sendtype);
  end;
  try
    // close the session after all is done
    omc := TOpsiMethodCall.Create('backend_exit', []);
    jsonEntry := FjsonExecutioner.retrieveJsonObject(omc);
    omc.Free;
    FjsonExecutioner.Free;
  except
    on e: Exception do
    begin
      LogDatei.log('exception in finishOpsiConf: backend_exit ' + e.message, LLError);
    end;
  end;
end;



function TOpsi4Data.sendLog: boolean;
begin
  Result := sendLog('instlog');
end;

function TOpsi4Data.sendLog(logtype: string): boolean;
begin
  if logtype = 'instlog' then
    Result := sendLog(logtype, Logdatei.Appendmode)
  else
    Result := sendLog(logtype, True);
end;


function TOpsi4Data.sendLog(logtype: string; appendmode: boolean): boolean;
  //const logtype : String;

var
  logstream: TMemoryStream;
  s, t: string;
  found: boolean;
  utf8str: UTF8String;
  errorinfo: string;
  Count: longint;
  //scan: TUTF8Scanner;
  //ch: UCS4Char;

begin
  t := '';
  Result := True;

  if Logdatei.PartbiggerthanMB(5) then
  begin
    Logdatei.log('Shrinking Logfile to 5 MB....', LLNotice);
    Logdatei.PartShrinkToMB(5);
    Logdatei.log('Shrinking Logfile to 5 MB finidhed.', LLNotice);
  end;
  Logdatei.setLogSIndentLevel(0);
  Logdatei.log(
    '-------- submitted part of log file ends here, see the rest of log file on client ----------',
    LLNotice);
  Logdatei.PartOpenForReading;
  try
    //Logdatei.DependentAdd('->3',LLNotice);
    logstream := TMemoryStream.Create;
    //Logdatei.DependentAdd('->4',LLNotice);
    logstream.Clear;
    //Logdatei.DependentAdd('->5',LLNotice);
    //s := '{"method":"writeLog","params":["' + logtype + '","';
    s := '{"method":"log_write","params":["' + logtype + '","';
    //Logdatei.DependentAdd('->6',LLNotice);
    logstream.Write(s[1], length(s));
    //Logdatei.DependentAdd('->7',LLNotice);
    s := '\n';
    Logdatei.DependentAdd('start reading read file ...', LLNotice);
    Found := Logdatei.getPartLine(t);
    while found do
    begin
      Logdatei.log('read line from read file ...', LLDebug2);
      logstream.Write(s[1], 2);
      t := escapeControlChars(t);
      //{$IFDEF WINDOWS}
      //utf8str := AnsiToUtf8(t);
      //logstream.Write(utf8str[1], length(utf8str));
      //{$ELSE WINDOWS}
      logstream.Write(t[1], length(t));
      //{$ENDIF WINDOWS}
      //utf8str := lconvencoding.ConvertEncoding(t,lconvencoding.GetDefaultTextEncoding,'utf8');

      Logdatei.log('write line from read file to service...', LLDebug2);
      //logstream.write(t[1],length(t));
      found := Logdatei.getPartLine(t);
    end;
    if appendmode then
    begin
      s := '", "' + actualClient + '", "true"], "id": 1}';
    end
    else
    begin
      s := '", "' + actualClient + '", "false"], "id": 1}';
    end;
    Logdatei.log('write line: >' + s + '<  to service...', LLNotice);
    logstream.Write(s[1], length(s));
  except
    on E: Exception do
    begin
      Logdatei.DependentAdd('oswebservice: sendLog: "' + E.Message + '"',
        LLError);
    end
  end;
  Logdatei.DependentAdd('start sending read file ...', LLNotice);
  try
    Result := (FJsonExecutioner.retrieveJSONObjectByHttpPost(logstream, False) <> nil);
    // we should perhaps not log inside this because of circularity
    errorinfo := FjsonExecutioner.LastError;

  except
    on e: Exception do
    begin
      Result := False;
    end;
  end;
  //Logdatei.PartReopen;
  Logdatei.PartCloseFromReading;
  logstream.Free;
end;


function TOpsi4Data.getSpecialScriptPath: string;
begin
  Result := '';
end;

function TOpsi4Data.getProductproperties: TStringList;
var
  omc: TOpsiMethodCall;
begin
  //result := TStringList.create;
  omc := TOpsiMethodCall.Create('getProductProperties_hash',
    [actualproduct, actualclient]);
  Result := FJsonExecutioner.getMapResult(omc);
  omc.Free;
  if Result = nil then
  begin
    logdatei.DependentAdd('Error: Opsi4Data.getProductproperties failed: retry',
      LLWarning);
    //ProcessMess;
    Sleep(500);
    omc := TOpsiMethodCall.Create('getProductProperties_hash',
      [actualproduct, actualclient]);
    Result := FJsonExecutioner.getMapResult(omc);
    omc.Free;
  end;
  if Result = nil then
  begin
    Result := TStringList.Create;
    logdatei.DependentAdd('Error: Opsi4Data.getProductproperties failed: giving up',
      LLError);
  end;
end;


function TOpsi4Data.getBeforeRequirements: TStringList;
begin
  Result := FPreRequirements;
end;

function TOpsi4Data.getAfterRequirements: TStringList;
begin
  Result := FPostRequirements;
end;


function TOpsi4Data.getListOfProducts: TStringList;
var
  productmaps: TStringList;
  i: integer;

begin
  Result := TStringList.Create;
  FProductActionRequests := TStringList.Create;

  try
    begin
      productmaps := getMapOfProductActionRequests;
      FProductActionRequests.AddStrings(productmaps);
      //productmaps := productonClients_getObjects__actionrequests;
      for i := 0 to productmaps.Count - 1 do
      begin
        Result.add(productmaps.Names[i]);
        LogDatei.DependentAdd('Product : ' + productmaps.Names[i], LLDebug);
        testresult := Result.Text;
      end;
      FSortByServer := True;
      LogDatei.DependentAdd('Product sequence calculated by opsi-server', LLDebug);
    end
  except
    FSortByServer := False;
    //result := FInstallableProducts;
    LogDatei.DependentAdd('Product sequence calculated by opsi-script', LLWarning);
  end;
end;


function TOpsi4Data.setAddDependentProductOnClients(switchon: boolean): boolean;
var
  omc: TOpsiMethodCall;
  productEntry: ISuperObject;
begin
  Result := False;
  try
    if switchon then
      omc := TOpsiMethodCall.Create('backend_setOptions',
        ['{"addDependentProductOnClients": true}'])
    else
      omc := TOpsiMethodCall.Create('backend_setOptions',
        ['{"addDependentProductOnClients": false}']);
    productEntry := FjsonExecutioner.retrieveJSONObject(omc);
    Result := True;
    //LogDatei.DependentAdd('Product sequence calculated with dependencies', LLerror);
  except
    on E: Exception do
    begin
      Logdatei.log('Exception in setAddDependentProductOnClients, system message: "' +
          E.Message + '"', LLwarning);
      Result := False;
    end;
  end;
end;

function TOpsi4Data.setAddProductPropertyStateDefaults(switchon: boolean): boolean;
var
  omc: TOpsiMethodCall;
  productEntry: ISuperObject;
begin
  Result := False;
  try
    if switchon then
      omc := TOpsiMethodCall.Create('backend_setOptions',
        ['{"addProductPropertyStateDefaults": true}'])
    else
      omc := TOpsiMethodCall.Create('backend_setOptions',
        ['{"addProductPropertyStateDefaults": false}']);
    productEntry := FjsonExecutioner.retrieveJSONObject(omc);
    Result := True;
    //LogDatei.DependentAdd('Product sequence calculated with dependencies', LLerror);
  except
    //LogDatei.DependentAdd('Product sequence not calculated with dependencies', LLerror);
    Result := False;
  end;
end;

function TOpsi4Data.setAddProductOnClientDefaults(switchon: boolean): boolean;
var
  omc: TOpsiMethodCall;
  productEntry: ISuperObject;
begin
  Result := False;
  try
    if switchon then
      omc := TOpsiMethodCall.Create('backend_setOptions',
        ['{"addProductOnClientDefaults": true}'])
    else
      omc := TOpsiMethodCall.Create('backend_setOptions',
        ['{"addProductOnClientDefaults": false}']);
    productEntry := FjsonExecutioner.retrieveJSONObject(omc);
    Result := True;
  except
    Result := False;
  end;
end;

function TOpsi4Data.getProductPropertyList(myproperty: string;
  defaultlist: TStringList): TStringList;
begin
  try
    Result := TStringList.Create;
    Result := getProductPropertyList(myproperty, defaultlist,
                                   actualclient, Productvars.Values['productId']);

  except
      on E: Exception do
      begin
        Logdatei.log('Exception in getProductPropertyList(1,2), system message: "' +
          E.Message + '"', LLwarning);
        LogDatei.log(' - using defaults.', LLWarning);
        Result.AddStrings(defaultlist);
      end
    end;
end;

function TOpsi4Data.getProductPropertyList(myproperty: string;
        defaultlist: TStringList;
        myClientId : string;
        myProductId : string): TStringList;
var
  resultlist: TStringList;
  omc: TOpsiMethodCall;
  i: integer;
  propertyEntry: ISuperObject;
begin
  Result := TStringList.Create;
  try
    try
      if setAddProductPropertyStateDefaults(True) then
      begin
        omc := TOpsiMethodCall.Create('productPropertyState_getObjects',
          ['', '{"objectId": "' + myClientId + '", "propertyId": "' +
          myproperty + '", "productId": "' + myProductId + '"}']);
        resultList := FjsonExecutioner.getSubListResult(omc, 'values');
        omc.Free;
        if resultList <> nil then
        begin
          if resultList.Count > 0 then
          begin
            for i := 0 to resultList.Count - 1 do
            begin
              LogDatei.DependentAdd('ProductPropertyList[' + IntToStr(
                i) + ']: ' + resultList.Strings[i], LLDebug3);
              Result.add(resultList.Strings[i]);
            end;
          end
          else
          begin
            LogDatei.DependentAdd('Got empty property from service', LLInfo);
          end;
        end
        else
        begin
          LogDatei.DependentAdd('Got no property from service - using default',
            LLWarning);
          Result.AddStrings(defaultlist);
        end;
      end
      else
      begin
        LogDatei.log('Could not set backend properties - using defaults.',
          LLWarning);
        Result.AddStrings(defaultlist);
      end;
    except
      on E: Exception do
      begin
        Logdatei.log('Exception in getProductPropertyList, system message: "' +
          E.Message + '"', LLwarning);
        LogDatei.DependentAdd(' - using defaults.', LLWarning);
        Result.AddStrings(defaultlist);
      end
    end;
  finally
    setAddProductPropertyStateDefaults(False);
  end;
end;


function TOpsi4Data.getMapOfProductStates: TStringList;
var
  resultlist: TStringList;
  omc: TOpsiMethodCall;
  i: integer;
  productEntry: ISuperObject;
begin
  Result := TStringList.Create;
  omc := TOpsiMethodCall.Create('getProductInstallationStatus_listOfHashes',
    [actualClient]);
  resultList := FjsonExecutioner.getListResult(omc);
  omc.Free;
  if resultList <> nil then
    for i := 0 to resultList.Count - 1 do
    begin
      productEntry := SO(resultList.Strings[i]);
      if (productEntry.O['productId'] <> nil) then
      begin
        Result.add(
          productEntry.S['productId']);
        Result.Values[productEntry.S['productId']] :=
          productEntry.S['installationStatus'];
      end;
    end;
end;

function TOpsi4Data.getMapOfProductActionRequests: TStringList;
begin
  //result := TStringList.create;
  //result.AddStrings(productonClients_getObjects__actionrequests);
  Result := productonClients_getObjects__actionrequests;
end;


function TOpsi4Data.productonClients_getObjects__actionrequests: TStringList;
var
  resultlist: TStringList;
  omc: TOpsiMethodCall;
  productEntry: ISuperObject;
  jo, new_obj, sort_obj: ISuperObject;
  sort_array, poc_array: TSuperArray;
  sortlist: TStringList;
  productid: string;
  i, k: integer;
  found: boolean;

  ///ptr : pointer;
begin
  Result := TStringList.Create;
  // switch on product sorting
  //omc := TOpsiMethodCall.Create('backend_setOptions',
  //  ['{"processProductOnClientSequence": true}']);
  //productEntry := FjsonExecutioner.retrieveJSONObject(omc);
  //omc := TOpsiMethodCall.create ('productOnClient_getObjects', ['','{"actionRequest": ["setup", "uninstall", "custom", "always", "update"], "clientId": "'+actualClient+'", "productType": "LocalbootProduct"}']);
  try
    logdatei.log('Starting sorting POC ', LLinfo);
    // get sorted productId list
    omc := TOpsiMethodCall.Create('getProductOrdering', [FDepotId]);
    jo := FjsonExecutioner.retrieveJSONObject(omc);
    new_obj := jo.O['result'];
    sort_array := new_obj.A['sorted'];
    sortlist := TStringList.Create;
    //if new_obj.IsType(stArray) then
    //begin
    for i := 0 to sort_array.Length - 1 do
    begin
      sortlist.Append(sort_array.S[i]);
      logdatei.log('Sorted productId: ' + sortlist.Strings[i] + ' at pos: ' +
        IntToStr(i), LLDebug2);
    end;
    // get unsorted poc list
    omc := TOpsiMethodCall.Create('productOnClient_getObjects',
      ['', '{"clientId": "' + actualClient + '", "productType": "LocalbootProduct"}']);

    resultList := FjsonExecutioner.getListResult(omc);
    //FProductOnClient_objects := FjsonExecutioner.retrieveJSONArray(omc);
    //resultList := FProductOnClient_objects.toList;


    if FProductStates = nil then
      FProductStates := TStringList.Create;
    FProductStates.Clear;
    if resultList <> nil then
    begin
      for i := 0 to sortlist.Count - 1 do
      begin
        found := False;
        k := 0;
        repeat
          productEntry := SO(resultlist.Strings[k]);
          if sortlist.Strings[i] = productEntry.S['productId'] then
            found := True;
          Inc(k);
        until found or (k = resultList.Count);
        if found then
        begin
          Result.add(productEntry.S['productId'] + '=' + productEntry.S['actionRequest']);
          //result.Values[productEntry.get('productId').toString] :=productEntry.get('actionRequest').toString;
          FProductStates.Add(productEntry.S['productId'] + '=' +
            productEntry.S['installationStatus']);
          LogDatei.DependentAdd('action entry : ' + productEntry.S['productId'] +
            '=' + productEntry.S['actionRequest'], LLDebug2);
          LogDatei.DependentAdd('state entry : ' + productEntry.S['productId'] +
            '=' + productEntry.S['installationStatus'], LLDebug2);
          logdatei.log('found POC entry with productId: ' + sortlist.Strings[i] +
            ' with pos: ' + IntToStr(i) + ' / ' + IntToStr(k - 1), LLDebug2);
        end
        else
          logdatei.log('No POC entry with productId: ' + sortlist.Strings[i], LLDebug2);
      end;
      logdatei.log('Finished sorting POC  ', LLinfo);
    (*
      for i := 0 to resultList.Count - 1 do
      begin
        productEntry := SO(resultlist.Strings[i]);
        //productEntry := resultList.
        if (productEntry.O['productId'] <> nil) then
        begin
          Result.add(productEntry.S['productId'] + '=' + productEntry.S['actionRequest']);
          //result.Values[productEntry.get('productId').toString] :=productEntry.get('actionRequest').toString;
          FProductStates.Add(productEntry.S['productId'] + '=' +
            productEntry.S['installationStatus']);
          LogDatei.DependentAdd('action entry : ' + productEntry.S['productId'] +
            '=' + productEntry.S['actionRequest'], LLDebug);
          LogDatei.DependentAdd('state entry : ' + productEntry.S['productId'] +
            '=' + productEntry.S['installationStatus'], LLDebug);
        end;
        testresult := Result.Text;
      end;
      *)
    end;

  except
    logdatei.log('Failed fetching sorted POC list !', LLError);
    logdatei.log('No correct calculated installation sequence !', LLError);
    logdatei.log('Starting fetching unsorted POC list', LLinfo);
    // switch on old (wrong) product sorting
    omc := TOpsiMethodCall.Create('backend_setOptions', ['{"processProductOnClientSequence": true}']);
    productEntry := FjsonExecutioner.retrieveJSONObject(omc);
    omc := TOpsiMethodCall.create ('productOnClient_getObjects', ['','{"actionRequest": ["setup", "uninstall", "custom", "always", "update"], "clientId": "'+actualClient+'", "productType": "LocalbootProduct"}']);
    resultList := FjsonExecutioner.getListResult(omc);
    for i := 0 to resultList.Count - 1 do
    begin
      productEntry := SO(resultlist.Strings[i]);
      //productEntry := resultList.
      if (productEntry.O['productId'] <> nil) then
      begin
        Result.add(productEntry.S['productId'] + '=' + productEntry.S['actionRequest']);
        //result.Values[productEntry.get('productId').toString] :=productEntry.get('actionRequest').toString;
        FProductStates.Add(productEntry.S['productId'] + '=' +
          productEntry.S['installationStatus']);
        LogDatei.DependentAdd('action entry : ' + productEntry.S['productId'] +
          '=' + productEntry.S['actionRequest'], LLDebug);
        LogDatei.DependentAdd('state entry : ' + productEntry.S['productId'] +
          '=' + productEntry.S['installationStatus'], LLDebug);
      end;
      testresult := Result.Text;
    end;
    // switch off product sorting again
    omc := TOpsiMethodCall.Create('backend_setOptions',
      ['{"processProductOnClientSequence": false}']);
    productEntry := FjsonExecutioner.retrieveJSONObject(omc);
    logdatei.log('Finished fetching unsorted POC list', LLinfo);
  end;
  omc.Free;
end;


function TOpsi4Data.getMapOfLoginscripts2Run(allscripts: boolean): TStringList;
var
  resultlist, productlist, loginscriptmap, productondepotlist,
  productondepotmap: TStringList;
  omc: TOpsiMethodCall;
  i: integer;
  productEntry, productondepotentry: ISuperObject;
  ///ptr : pointer;
begin
  Result := TStringList.Create;
  productondepotmap := TStringList.Create;
  //LogDatei.LogLevel := LLdebug2;

  // we need to know which products are installed
  omc := TOpsiMethodCall.Create('productOnClient_getObjects',
    ['', '{"clientId": "' + actualClient + '", "productType": "LocalbootProduct"}']);
  resultList := FjsonExecutioner.getListResult(omc);
  omc.Free;

  // we need to know which products have a login script
  omc := TOpsiMethodCall.Create('product_getObjects',
    ['', '{"userLoginScript": "*.*"}']);
  productlist := FjsonExecutioner.getListResult(omc);
  omc.Free;

  // we need to know which products are on the used depot
  omc := TOpsiMethodCall.Create('productOnDepot_getObjects',
    ['', '{"depotId": "' + FDepotId + '"}']);
  productondepotlist := FjsonExecutioner.getListResult(omc);
  omc.Free;
  if productondepotlist.Count > 0 then
    for i := 0 to productondepotlist.Count - 1 do
    begin
      productondepotentry := SO(productondepotlist.Strings[i]);
      productondepotmap.Add(productondepotentry.S['productId'] + '=' +
        productondepotentry.S['productVersion'] + '-' +
        productondepotentry.S['packageVersion']);
    end;



  if FProductStates = nil then
    FProductStates := TStringList.Create;
  FProductStates.Clear;

  // build a map <product>=<loginscript>
  loginscriptmap := TStringList.Create;
  if productlist <> nil then
    for i := 0 to productlist.Count - 1 do
    begin
      productEntry := SO(productlist.Strings[i]);
      //LogDatei.DependentAdd ('productEntry : '+productEntry.AsString, LLessential);
      //LogDatei.DependentAdd ('product : '+productEntry.S['id']+'='+productEntry.S['userLoginScript'], LLessential);
      if (productEntry.O['id'] <> nil) and
        (productEntry.S['userLoginScript'] <> '') and
        (productondepotmap.Values[productEntry.S['id']] <> '') and
        (loginscriptmap.Values[productEntry.S['id']] = '') then
      begin
        loginscriptmap.add(productEntry.S['id'] + '=' +
          productEntry.S['userLoginScript']);
        LogDatei.DependentAdd('product : ' + productEntry.S['id'] +
          '=' + productEntry.S['userLoginScript'], LLessential);
        if allscripts then
        begin
          Result.add(productEntry.S['id']);
          FProductStates.Add(productEntry.S['id'] + '=none');
        end;
      end;
    end;


  if not allscripts then
  begin
    // get from all installed products only these which have login script and fill FProductStates

    if resultList <> nil then
      for i := 0 to resultList.Count - 1 do
      begin
        productEntry := SO(resultlist.Strings[i]);
        //LogDatei.DependentAdd (productEntry.S['productId']+'='+loginscriptmap.Values[productEntry.S['productId']]+'<', LLessential);
        //if (productEntry.O['productId'] <> nil) and (productEntry.S['installationStatus'] = 'installed') then
        if (productEntry.O['productId'] <> nil) and
          (((productEntry.S['actionResult'] = 'successful') and
          ((productEntry.S['lastAction'] = 'setup') or
          (productEntry.S['lastAction'] = 'uninstall')))) then
        begin
          if loginscriptmap.Values[productEntry.S['productId']] <> '' then
          begin
            // product is in productOnClient, so let us read the states
            Result.add(productEntry.S['productId']);
            FProductStates.Add(productEntry.S['productId'] + '=' +
              productEntry.S['installationStatus']);
            LogDatei.DependentAdd(productEntry.S['productId'] +
              '= lastcation: ' + productEntry.S['lastAction'], LLessential);
          end;
        end;
      end;
  end;
  productondepotmap.Free;
end;


procedure TOpsi4Data.productOnClient_getobject_actualclient;
var
  ///resultlist : TList;
  omc: TOpsiMethodCall;
  ///i : Integer;
  jo, new_obj, sort_obj: ISuperObject;
  sort_array, poc_array: TSuperArray;
  sortlist: TStringList;
  productid: string;
  i, k: integer;
  found: boolean;
begin
  //result := TStringList.create;
  //LogDatei.LogLevel := LLdebug2;
  // switch off product sorting again
  // 4.11.6.1 24.5.2016: sorting by server broken
  //omc := TOpsiMethodCall.Create('backend_setOptions',
  //  ['{"processProductOnClientSequence": true}']);
  logdatei.log('Starting sorting POC : productOnClient_getobject_actualclient', LLinfo);
  // getting sorted productIdList
  omc := TOpsiMethodCall.Create('getProductOrdering', [FDepotId]);
  jo := FjsonExecutioner.retrieveJSONObject(omc);
  new_obj := jo.O['result'];
  sort_array := new_obj.A['sorted'];
  sortlist := TStringList.Create;
  //if new_obj.IsType(stArray) then
  //begin
  for i := 0 to sort_array.Length - 1 do
  begin
    sortlist.Append(sort_array.S[i]);
    logdatei.log('Sorted productId: ' + sortlist.Strings[i] + ' at pos: ' +
      IntToStr(i), LLDebug2);
  end;
  //end;
  //else LogDatei.log('Error handling getProductOrdering result', LLError);
  // getting unsorted POC
  omc := TOpsiMethodCall.Create('productOnClient_getObjects',
    ['', '{ "clientId": "' + actualClient + '", "productType": "LocalbootProduct"}']);
  // sorting result to  FProductOnClient_objects
  poc_array := FjsonExecutioner.retrieveJSONArray(omc);
  sort_array.Clear(True);
  for i := 0 to sortlist.Count - 1 do
  begin
    found := False;
    k := 0;
    repeat
      if sortlist.Strings[i] = poc_array.O[k].S['productId'] then
        found := True;
      Inc(k);
    until found or (k = poc_array.Length);
    if found then
    begin
      sort_array.Add(poc_array.O[k - 1]);
      logdatei.log('found POC entry with productId: ' + sortlist.Strings[i] +
        ' with pos: ' + IntToStr(i) + ' / ' + IntToStr(k - 1), LLDebug2);
    end
    else
      logdatei.log('No POC entry with productId: ' + sortlist.Strings[i], LLDebug2);
  end;
  logdatei.log('Finished sorting POC ', LLinfo);
  FProductOnClient_objects := sort_array;
  // switch off product sorting again
  //omc := TOpsiMethodCall.Create('backend_setOptions',
  //  ['{"processProductOnClientSequence": false}']);
  //jo := FjsonExecutioner.retrieveJSONObject(omc);
  omc.Free;
  sortlist.Free;
  //LogDatei.LogLevel := LLnotice;
end;




function TOpsi4Data.getInstallableProducts: TStringList;
var
  objectlist: TStringList;
  omc: TOpsiMethodCall;
  i: integer;
begin
  omc := TOpsiMethodCall.Create('getInstallableLocalBootProductIds_list',
    [actualClient]);
  objectlist := FjsonExecutioner.getListResult(omc);
  omc.Free;

  Result := TStringList.Create;

  for i := 0 to objectlist.Count - 1 do
  begin
    testresult := SO(objectlist.Strings[i]).AsJSon(False, False);
    Result.add(testresult);
  end;
end;

function TOpsi4Data.getProductRequirements(productname: string;
  requirementType: string): TStringList;
var
  i: integer;
  requType: string;
  jO: ISuperObject;
  productId: string;
  teststring: string;
  resultstring: string;
begin

  requType := lowercase(requirementType);
  productId := lowercase(productname);

  Result := TStringList.Create;
  if AllDependencies <> nil then
  begin
    for i := 0 to AllDependencies.Count - 1 do
    begin
      jO := SO(AllDependencies.Strings[i]);
      teststring := jO.S['productId'];

      if (lowercase(teststring) = productId) and
        (lowercase(jO.S['requirementType']) = requType) then
      begin
        resultstring := jO.S['requiredProductId'];
        Result.add(resultstring);
      end;
    end;
  end;
end;

function TOpsi4Data.getProductState: TProductState;
begin
  Result := stateStringToState(FProductStates.Values[ActualProduct]);
end;


function TOpsi4Data.getProductAction: TAction;
begin
  testresult := FProductActionRequests.Values[ActualProduct];
  Result := actionRequestStringToAction(FProductActionRequests.Values[ActualProduct]);
end;

function TOpsi4Data.getProductActionRequest: TActionRequest;
begin
  Result := actionRequestStringToActionRequest(
    FProductActionRequests.Values[ActualProduct]);
end;


function TOpsi4Data.getProductScriptPath(actionType: TAction): string;
begin
  Result := '';
  //Logdatei.DependentAdd('in TOpsi4Data.getProductScriptPath ', LLessential);
  if Productvars <> nil then
    case actionType of
      tacDeinstall:
        Result := Productvars.Values['uninstallScript'];
      tacSetup:
        Result := Productvars.Values['setupScript'];
      tacOnce:
        Result := Productvars.Values['onceScript'];
      tacAlways:
        Result := Productvars.Values['alwaysScript'];
      tacCustom:
        Result := Productvars.Values['customScript'];
      tacUpdate:
      begin
        if Productvars.IndexOfName('updateScript') > -1 then
          Result := Productvars.Values['updateScript']
        else
          Result := '';
      end;
      tacLogin:
        Result := Productvars.Values['userLoginScript'];

    end
  else
    Logdatei.DependentAdd(
      'In TOpsi4Data.getProductScriptPath Productvars unexpected nil',
      LLcritical);

end;


function TOpsi4Data.getInstallationPriority: integer;
begin
  Result := 0;
  try
    Result := StrToInt(Productvars.Values['priority'])
  except
  end;
end;


function TOpsi4Data.getProductPackagageVersion: string;
begin
  Result := Productvars.Values['productVersion'] + '-' +
    Productvars.Values['packageVersion'];
end;

function TOpsi4Data.getActualProductVersion: string;
begin
  Result := getProductPackagageVersion;
end;

function TOpsi4Data.getActualProductDescription: string;
begin
  Result := Productvars.Values['description'];
end;

function TOpsi4Data.getActualProductAdvice: string;
begin
  Result := Productvars.Values['advice'];
end;

function TOpsi4Data.getActualProductPriority: string;
begin
  Result := Productvars.Values['priority'];
end;

function TOpsi4Data.getActualProductPackageVersion: string;
begin
  Result := Productvars.Values['packageVersion'];
end;

function TOpsi4Data.getActualProductId: string;
begin
  Result := actualProduct;
  //Result := Productvars.Values['productId'];
end;

function TOpsi4Data.getActualProductName: string;
begin
  Result := Productvars.Values['name'];
end;

function TOpsi4Data.getActualProductProductVersion: string;
begin
  Result := Productvars.Values['productVersion'];
end;

function TOpsi4Data.getActualProductInstallationState: string;
begin
  if (FProductOnClient_aktobject = nil) or
    (FProductOnClient_aktobject.AsObject.N['installationStatus'] = nil) then
    Result := 'none'
  else
    Result := FProductOnClient_aktobject.AsObject.S['installationStatus'];
end;

function TOpsi4Data.getActualProductLastActionRequest: string;
begin
  if (FProductOnClient_aktobject = nil) or
    (FProductOnClient_aktobject.AsObject.N['lastAction'] = nil) then
    Result := 'none'
  else
    Result := FProductOnClient_aktobject.AsObject.S['lastAction'];
end;

function TOpsi4Data.getActualProductProductLastActionReport: string;
begin
  if (FProductOnClient_aktobject = nil) or
    (FProductOnClient_aktobject.AsObject.N['actionResult'] = nil) then
    Result := 'none'
  else
    Result := FProductOnClient_aktobject.AsObject.S['actionResult'];
end;

function TOpsi4Data.getActualProductInstalledVersion: string;
begin
  if (FProductOnClient_aktobject = nil) or
    (FProductOnClient_aktobject.AsObject.N['productVersion'] = nil) then
    Result := ''
  else
    Result := FProductOnClient_aktobject.AsObject.S['productVersion'];
end;

function TOpsi4Data.getActualProductInstalledPackage: string;
begin
  if (FProductOnClient_aktobject = nil) or
    (FProductOnClient_aktobject.AsObject.N['packageVersion'] = nil) then
    Result := ''
  else
    Result := FProductOnClient_aktobject.AsObject.S['packageVersion'];
end;

function TOpsi4Data.getActualProductInstalledModificationTime: string;
begin
  if (FProductOnClient_aktobject = nil) or
    (FProductOnClient_aktobject.AsObject.N['modificationTime'] = nil) then
    Result := ''
  else
    Result := FProductOnClient_aktobject.AsObject.S['modificationTime'];
end;

function TOpsi4Data.getActualProductActionRequest: string;
begin
  if (FProductOnClient_aktobject = nil) or
    (FProductOnClient_aktobject.AsObject.N['actionRequest'] = nil) then
    Result := ''
  else
    Result := FProductOnClient_aktobject.AsObject.S['actionRequest'];
end;

procedure TOpsi4Data.setActualProductActionRequest(request: string);
begin
  if (FProductOnClient_aktobject <> nil) then
    FProductOnClient_aktobject.AsObject.S['actionRequest'] := request;
end;



procedure TOpsi4Data.setProductActionRequest(newAction: TActionRequest);
var
  omc: TOpsiMethodCall;
  jO: ISuperObject;
  arS: string;

begin
  //if FInstallableProducts.IndexOf(actualProduct) = -1 then exit;

  arS := actionRequestToString(newAction);
  omc := TOpsiMethodCall.Create('setProductActionRequest',
    [actualProduct, actualClient, ars]);

  jO := FjsonExecutioner.retrieveJSONObject(omc);
  omc.Free;
  if FProductActionRequests = nil then
    FProductActionRequests := TStringList.Create;
  FProductActionRequests.Values[actualProduct] := ars;
end;

procedure Topsi4data.setActionProgress(const progress: string);
var
  omc: TOpsiMethodCall;
  jO: ISuperObject;
  parastr: string;
begin
  FProductOnClient_aktobject.AsObject.S['actionProgress'] := progress;
  parastr := FProductOnClient_aktobject.asJson(False, False);
  omc := TOpsiMethodCall.Create('productOnClient_updateObject', [parastr]);
  jO := FjsonExecutioner.retrieveJSONObject(omc);
  omc.Free;
      (*
  omc := TOpsiMethodCall.Create('setProductActionProgress',
    [actualProduct, actualClient, progress]);

  jO := FjsonExecutioner.retrieveJSONObject(omc);
  omc.Free;
  *)
end;

procedure TOpsi4Data.setProductState(newState: TProductState);
var
  omc: TOpsiMethodCall;
  jO: ISuperObject;
  stateS, parastr: string;
begin
  //if FInstallableProducts.IndexOf(actualProduct) = -1 then exit;
  try
    //stateS := stateToString(newState);
    if newState = tpsInstalling then
    begin
      stateS := 'installing';
      //FProductOnClient_aktobject.put('actionProgress',stateS);
      //FProductOnClient_aktobject :=
      FProductOnClient_aktobject.AsObject.N['modificationTime'] := nil;
      parastr := FProductOnClient_aktobject.asJson(False, False);
      //FProductOnClient_aktobject :=
      FProductOnClient_aktobject.AsObject.N['actionSequence'] := nil;
      parastr := FProductOnClient_aktobject.asJson(False, False);
      //FProductOnClient_aktobject :=
      FProductOnClient_aktobject.AsObject.S['installationStatus'] :=
        installationStatusToString(tps4Unkown);
      //.putS('installationStatus',installationStatusToString(tps4Unkown));
      //FProductOnClient_aktobject :=
      FProductOnClient_aktobject.AsObject.S['actionProgress'] := stateS;
      parastr := FProductOnClient_aktobject.asJson(False, False);
      omc := TOpsiMethodCall.Create('productOnClient_updateObject', [parastr]);
      //omc := TOpsiMethodCall.create('setProductInstallationStatus',[actualProduct, actualClient, stateS]);
      jO := FjsonExecutioner.retrieveJSONObject(omc);
      omc.Free;
    end;
  except
    LogDatei.DependentAdd('Exception in opsi4data.setProductState , parastr: ' +
      parastr, LLerror);
  end;
  // save the new value in the local cache as well
  if FProductStates.IndexOf(actualProduct) > -1 then
    FProductStates.Values[actualProduct] := installationStatusToString(tps4Unkown);
end;

procedure TOpsi4Data.setProductProgress(myprogres: string);
var
  omc: TOpsiMethodCall;
  jO: ISuperObject;
  stateS, parastr: string;
begin
  try
    begin
      stateS := myprogres;
      //FProductOnClient_aktobject.put('actionProgress',stateS);
      //FProductOnClient_aktobject :=
      FProductOnClient_aktobject.AsObject.N['modificationTime'] := nil;
      parastr := FProductOnClient_aktobject.asJson(False, False);
      //FProductOnClient_aktobject :=
      FProductOnClient_aktobject.AsObject.N['actionSequence'] := nil;
      parastr := FProductOnClient_aktobject.asJson(False, False);
      //FProductOnClient_aktobject :=
      FProductOnClient_aktobject.AsObject.S['installationStatus'] :=
        installationStatusToString(tps4Unkown);
      //.putS('installationStatus',installationStatusToString(tps4Unkown));
      //FProductOnClient_aktobject :=
      FProductOnClient_aktobject.AsObject.S['actionProgress'] := stateS;
      parastr := FProductOnClient_aktobject.asJson(False, False);
      omc := TOpsiMethodCall.Create('productOnClient_updateObject', [parastr]);
      //omc := TOpsiMethodCall.create('setProductInstallationStatus',[actualProduct, actualClient, stateS]);
      jO := FjsonExecutioner.retrieveJSONObject(omc);
      omc.Free;
    end;
  except
    LogDatei.DependentAdd('Exception in opsi4data.setProductState , parastr: ' +
      parastr, LLerror);
  end;
  // save the new value in the local cache as well
  if FProductStates.IndexOf(actualProduct) > -1 then
    FProductStates.Values[actualProduct] := installationStatusToString(tps4Unkown);
end;




procedure TOpsi4Data.ProductOnClient_update(actionProgressS: string;
  actionResult: TActionResult4; actionRequest: TActionRequest4;
  targetConfiguration: TTargetConfiguration4; lastAction: TActionRequest4;
  installationStatus: TProductstate4);

var
  omc: TOpsiMethodCall;
  jO: ISuperObject;
  installationStatusS, actionResultS, actionRequestS, targetConfigurationS,
  lastActionS: string;
  parastr: string;
  versionstr: string;

begin
  //if FInstallableProducts.IndexOf(actualProduct) = -1 then exit;
  try
    actionResultS := actionResultToString(actionResult);
    actionRequestS := actionRequest4toString(actionRequest);
    targetConfigurationS := targetConfigurationToString(targetConfiguration);
    lastActionS := actionRequest4toString(lastAction);
    installationStatusS := installationStatusToString(installationStatus);

    //FProductOnClient_aktobject.put('actionProgress',stateS);
    parastr := FProductOnClient_aktobject.asJson(False, False);
    //FProductOnClient_aktobject :=
    FProductOnClient_aktobject.S['actionProgress'] := actionProgressS;
    parastr := FProductOnClient_aktobject.asJson(False, False);
    //FProductOnClient_aktobject :=
    FProductOnClient_aktobject.S['actionResult'] := actionResultS;
    parastr := FProductOnClient_aktobject.asJson(False, False);
    //FProductOnClient_aktobject :=
    FProductOnClient_aktobject.S['actionRequest'] := actionRequestS;
    parastr := FProductOnClient_aktobject.asJson(False, False);
    if lastAction <> tac4Custom then
      //FProductOnClient_aktobject :=
      FProductOnClient_aktobject.S['targetConfiguration'] := targetConfigurationS;
    parastr := FProductOnClient_aktobject.asJson(False, False);
    if lastAction <> tac4Custom then
    begin
      if (Productvars <> nil) and (Productvars.values['productVersion'] <> '') then
      begin
        versionstr := Productvars.Values['productVersion'];
        //FProductOnClient_aktobject :=
        FProductOnClient_aktobject.S['productVersion'] :=
          Productvars.Values['productVersion'];
        //        if FProductOnClient_aktobject.isNull('productVersion') then
        //          FProductOnClient_aktobject := FProductOnClient_aktobject.put('productVersion',nil);
        parastr := FProductOnClient_aktobject.asJson(False, False);
        //FProductOnClient_aktobject :=
        FProductOnClient_aktobject.S['productVersion'] :=
          Productvars.Values['productVersion'];
        parastr := FProductOnClient_aktobject.asJson(False, False);
      end;
    end;
    if lastAction <> tac4Custom then
    begin
      if (Productvars <> nil) and (Productvars.values['packageVersion'] <> '') then
      begin
        versionstr := Productvars.Values['packageVersion'];
        //        if FProductOnClient_aktobject.isNull('packageVersion') then
        //          FProductOnClient_aktobject := FProductOnClient_aktobject.put('packageVersion',nil);
        //        parastr := FProductOnClient_aktobject.toString;
        //FProductOnClient_aktobject :=
        FProductOnClient_aktobject.S['packageVersion'] :=
          Productvars.Values['packageVersion'];
        parastr := FProductOnClient_aktobject.asJson(False, False);
      end;
    end;
    //FProductOnClient_aktobject :=
    FProductOnClient_aktobject.S['lastAction'] := lastActionS;
    if lastAction <> tac4Custom then
      //FProductOnClient_aktobject :=
      FProductOnClient_aktobject.S['installationStatus'] := installationStatusS;
    parastr := FProductOnClient_aktobject.asJson(False, False);
    omc := TOpsiMethodCall.Create('productOnClient_updateObject', [parastr]);
    jO := FjsonExecutioner.retrieveJSONObject(omc);
    omc.Free;
  except
    Logdatei.DependentAdd('Error in opsi4data.ProductOnClient_update, parastr: ' +
      parastr, LLerror);
  end;
end;

procedure TOpsi4Data.setProductStateActionRequest(newState: TProductState;
  newAction: TActionRequest);
begin
  setProductState(newState);
  setProductActionRequest(newAction);
end;

function TOpsi4Data.UpdateSwitches(extremeErrorLevel: TErrorLevel): boolean;
begin
  Result := UpdateSwitches(extremeErrorLevel, '');
end;


function TOpsi4Data.UpdateSwitches(extremeErrorLevel: TErrorLevel;
  Progress: string): boolean;
var
  ///i : integer;
  ActionStr: string;
  str: string;
begin
  try
    Result := True;
    //ActionStr := FProductOnClient_aktObject.get('actionRequest').toString;
    str := FProductOnClient_aktobject.asJson(False, False);
    ActionStr := FProductOnClient_aktObject.AsObject.S['actionRequest'];
    Logdatei.DependentAdd('In opsi4data.UpdateSwitches, Actionstr: ' +
      Actionstr, LLDebug2);
    if extremeErrorLevel > levelfatal then
    begin
      if ActionStr = 'setup' then
      begin
        //successful after setup
        ProductOnClient_update('',
          tar4Successful,
          tac4None,
          ttc4Installed,
          tac4Setup,
          tps4Installed);
      end
      else if ActionStr = 'update' then
      begin
        //successful after update
        ProductOnClient_update('',
          tar4Successful,
          tac4None,
          ttc4Installed,
          tac4Update,
          tps4Installed);
      end
      else if ActionStr = 'uninstall' then
      begin
        //successful after uninstall
        ProductOnClient_update('',
          tar4Successful,
          tac4None,
          ttc4Forbidden,
          tac4Uninstall,
          tps4Not_installed);
      end
      else if ActionStr = 'always' then
      begin
        //successful after always
        ProductOnClient_update('',
          tar4Successful,
          tac4Always,
          ttc4Always,
          tac4Always,
          tps4Installed);
      end
      else if ActionStr = 'once' then
      begin
        //successful after Once
        ProductOnClient_update('',
          tar4Successful,
          tac4None,
          ttc4Forbidden,
          tac4Once,
          tps4Not_installed);
      end
      else if ActionStr = 'custom' then
      begin
        //successful after Custom
        ProductOnClient_update('',
          tar4Successful,
          tac4None,
          ttc4undefined,
          tac4Custom,
          tps4Unkown);
      end
      else
      begin
        //unknown combination
      end;
    end
    else //failed
    begin
      if ActionStr = 'setup' then
      begin
        //failed after setup
        ProductOnClient_update(Progress,
          tar4Failed,
          tac4None,
          ttc4Installed,
          tac4Setup,
          tps4Unkown);
      end
      else if ActionStr = 'update' then
      begin
        //failed after update
        ProductOnClient_update(Progress,
          tar4Failed,
          tac4None,
          ttc4Installed,
          tac4Update,
          tps4Unkown);
      end
      else if ActionStr = 'uninstall' then
      begin
        //failed after uninstall
        ProductOnClient_update(Progress,
          tar4Failed,
          tac4None,
          ttc4Forbidden,
          tac4Uninstall,
          tps4Unkown);
      end
      else if ActionStr = 'always' then
      begin
        //failed after always
        ProductOnClient_update(Progress,
          tar4Failed,
          tac4Always,
          ttc4Always,
          tac4Always,
          tps4Unkown);
      end
      else if ActionStr = 'once' then
      begin
        //failed after Once
        ProductOnClient_update(Progress,
          tar4Failed,
          tac4None,
          ttc4Forbidden,
          tac4Once,
          tps4Unkown);
      end
      else if ActionStr = 'custom' then
      begin
        //failed after Custom
        ProductOnClient_update(Progress,
          tar4Failed,
          tac4None,
          ttc4undefined,
          tac4Custom,
          tps4Unkown);
      end
      else
      begin
        //unknown combination
      end;
    end; // failed
  except
    Logdatei.DependentAdd('Error in opsi4data.UpdateSwitches, Actionstr: ' +
      Actionstr, LLerror);
  end;
end;

function TOpsi4Data.actionResultToString(actionResult: TActionResult4): string;
begin
  case actionResult of
    tar4None: Result := 'none';
    tar4Failed: Result := 'failed';
    tar4Successful: Result := 'successful';
  end;
end;

function TOpsi4Data.actionRequest4toString(actionRequest: TActionRequest4): string;
begin
  case actionRequest of
    tac4None: Result := 'none';
    tac4Setup: Result := 'setup';
    tac4Update: Result := 'update';
    tac4Uninstall: Result := 'uninstall';
    tac4Always: Result := 'always';
    tac4Once: Result := 'once';
    tac4Custom: Result := 'custom';
  end;
end;

function TOpsi4Data.targetConfigurationToString(targetConfiguration:
  TTargetConfiguration4): string;
begin
  case targetConfiguration of
    ttc4Installed: Result := 'installed';
    ttc4Forbidden: Result := 'forbidden';
    ttc4Always: Result := 'always';
    ttc4undefined: Result := 'undefined';
  end;
end;

function TOpsi4Data.installationStatusToString(installationStatus:
  TProductstate4): string;
begin
  case installationStatus of
    tps4Installed: Result := 'installed';
    tps4Not_installed: Result := 'not_installed';
    tps4Unkown: Result := 'unknown';
  end;
end;

function TOpsi4Data.getFileFromDepot(filename: string; toStringList: boolean;
  var ListResult: TStringList): boolean;
begin
  Result := FjsonExecutioner.getFileFromDepot(filename, toStringList, ListResult);
end;


initialization
  // {$i widatamodul.lrs}
end.
