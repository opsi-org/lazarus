unit oswebservice;

{$MODE Delphi}
//{$RANGECHECKS ON}
{$OVERFLOWCHECKS ON}
{$IOCHECKS ON}
{$OBJECTCHECKS ON}
{$VARSTRINGCHECKS ON}
{$LONGSTRINGS ON}
{$MACRO ON}

{$DEFINE SYNAPSE}
{$DEFINE SSL_OPENSSL_UNIT:=ssl_openssl3}
{$DEFINE SSL_OPENSSL_LIB_UNIT:=ssl_openssl3_lib}

// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: Rupert Roeder, detlef oertel, Jan Werner


interface

uses
  SysUtils, Classes, Variants,
  oslog,
  superobject,
  osjson,
  strutils,
  {$IFDEF SYNAPSE}
  httpsend, SSL_OPENSSL_UNIT, SSL_OPENSSL_LIB_UNIT, blcksock,
  {$ELSE SYNAPSE}
  IdComponent,
  IdHTTP,
  IdWebDAV,
  IdIOHandler,
  IdSSLOpenSSL,
  IdSocketHandle,
  {$ENDIF SYNAPSE}
  synacode,
  TypInfo,
  {$IFDEF GUI}
  Forms,
  osGUIControl,
  {$ENDIF GUI}
  {$IFDEF OPSISCRIPT}
  osfunc,
  osconf,
  {$ENDIF OPSISCRIPT}
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF WINDOWS}
  GZIPUtils,
  zstream,
  LazUTF8;


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

gziputils.pas
*****************************************************************************
Copyright (c) 2007-2019, Udo Schmal <udo.schmal@t-online.de>

Permission to use, copy, modify, and/or distribute the software for any purpose
with or without fee is hereby granted, provided that the above copyright notice
and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT,
OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*****************************************************************************
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


  (************** opsidata-Service  *********************************************)

  // helper classes

  { TOpsiMethodCall }

  TOpsiMethodCall = class(TObject)
  protected
    FOpsiMethodName: string;
    FParameterlist: TStringList;
    Fhashlist: TStringList;
    FTimeout: integer;
    FJSONValueSyntaxInParameterList: boolean;
  public
    { constructor }
    constructor Create(const method: string; parameters: array of string); overload;
    constructor Create(const method: string; hashlist: TStringList;
      parameters: array of string); overload;
    { destructor }
    destructor Destroy; override;
    { functions }
    function getJsonUrlString: string;
    function getJsonHashListString: string;
    { properties }
    property opsimethodname: string read FOpsiMethodname write FOpsiMethodname;
    property parameterlist: TStringList read FParameterlist write FParameterlist;
    property hashlist: TStringList read Fhashlist write Fhashlist;
    property jsonUrlString: string read getJsonUrlString;
    property timeout: integer read FTimeout write FTimeout;
    property JSONValueSyntaxInParameterList: boolean
      read FJSONValueSyntaxInParameterList write FJSONValueSyntaxInParameterList;
  end;


  { TJsonThroughHTTPS }

  TJsonThroughHTTPS = class(TObject)
  private
    FResultLines: TStringList;
    mymemorystream: TMemoryStream;
    //nullO : NULL;
    methodGET: boolean;
    function GetSynapseSocketFamilyAsString: string;
    procedure LogHostIPs;
  protected
    {$IFDEF SYNAPSE}
    HTTPSender: THTTPSend;
    {$ELSE SYNAPSE}
    IdSSLIOHandlerSocket: TIdSSLIOHandlerSocketOpenSSL;
    //IdHTTP: TIdHTTP;
    IdHTTP: TIdWebDAV;
    {$ENDIF SYNAPSE}
    portHTTP: integer;
    portHTTPS: integer;
    Furl: string;
    FServiceURL: string;
    Fusername: string;
    Fpassword: string;
    FError: string;
    FErrorInfo: TStringList;
    FSessionId: string;
    FCommunicationMode: integer;
    procedure makeURL(const omc: TOpsiMethodCall);
    procedure createSocket; overload;
    procedure createSocket(const agent, ip, port: string); overload;
    function CompressStream(InStream: TMemoryStream;
      const ContentEncoding: string): TMemoryStream;
  public
    { constructor }
    constructor Create(const serviceURL, username, password: string); overload;
    constructor Create(const serviceURL, username, password, sessionid: string);
      overload;
    constructor Create(const serviceURL, username, password, sessionid,
      ip, port: string); overload;
    constructor Create(const serviceURL, username, password, sessionid,
      ip, port, agent: string); overload;
    { destructor }
    destructor Destroy; override;
    { function }
    (*
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
    *)
    function retrieveJSONObject(const omc: TOpsiMethodCall;
      logging: boolean = True; retry: boolean = True; readOmcMap: boolean = False;
      communicationmode: integer = 0): ISuperObject;
    function retrieveJSONObjectByHttpPost(InStream: TMemoryStream;
      logging: boolean; communicationmode: integer): ISuperObject;
    function getMapResult(const omc: TOpsiMethodCall): TStringList;
    function getHashListResult(const omc: TOpsiMethodCall): TStringList;
    function getListResult(const omc: TOpsiMethodCall): TStringList;
    function getStringListResult(const omc: TOpsiMethodCall): TStringList;
    function getSubListResult(const omc: TOpsiMethodCall; subkey: string): TStringList;
    function getFileFromDepot(filename: string; toStringList: boolean;
      var ListResult: TStringList): boolean;
    { properties }
    property resultLines: TStringList read FResultLines;
    property LastError: string read FError;
    property LastErrorInfo: TStringList read FErrorInfo;
  end;

  (****************************** Opsi Data *************************************)

  { TOpsiData}

  TOpsiData = class
  protected
    actualProduct: string;
    actualVersion: string;
    actualClient: string;
    options: string;
    FSortByServer: boolean;
  public
    {constructor}
    constructor Create;
    {functions}
    function stateToString(state: TProductState): string; virtual; abstract;
    function actionToString(action: TAction): string; virtual; abstract;
    function actionRequestToString(actionRequest: TActionRequest): string;
      virtual; abstract;
    function getLogFileName(const LogFilename: string): string; virtual; abstract;
    function UpdateSwitches(extremeErrorLevel: TErrorLevel): boolean; virtual;
    function getActualProductName: string; virtual;
    function getActualProductVersion: string; virtual;
    function initProduct: boolean; virtual; abstract;
    function getSpecialScriptPath: string; virtual;
    function getProductproperties: TStringList; virtual; abstract;
    function getBeforeRequirements: TStringList; virtual; abstract;
    function getAfterRequirements: TStringList; virtual; abstract;
    function getProductState: TProductState; virtual; abstract;
    function getProductAction: TAction; virtual; abstract;
    function getProductActionRequest: TActionRequest; virtual; abstract;
    function getListOfProductIDs: TStringList; virtual; abstract;
    function getProductScriptPath(actionType: TAction): string;
      virtual; abstract;
    function getInstallationPriority: integer; virtual; abstract;
    function withLicenceManagement: boolean; virtual;
    function getOpsiServiceVersion: string; virtual; abstract;
    {procedures}
    procedure setActualClient(computername: string); virtual; abstract;
    procedure setOptions(serviceoptions: string);
    procedure setActualProductName(const productname: string); virtual;
    procedure finishProduct; virtual; abstract;
    procedure saveOpsiConf; virtual; abstract;
    procedure finishOpsiConf; virtual;
    procedure setProductActionRequest(newAction: TActionRequest); virtual; abstract;
    procedure setProductState(newState: TProductState); virtual; abstract;
    procedure setProductStateActionRequest(newState: TProductState;
      newActiont: TActionRequest);
      virtual; abstract;
    {Properties}
    property sortByServer: boolean read FSortByServer;
  end;

  { TOpsi4Data }

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
    FSortedProductIDsWhereActionIsSet: TStringList;
    FInstallableProducts: TStringList;
    ProductVars: TStringList;
    FOpsiModules: ISuperObject;
    FOpsiInformation: ISuperObject;
    FOpsiServiceVersion: string;
    //FactualClient: string;
    FProductOnClient_objects: TSuperArray;
    FProductOnClient_aktobject: ISuperObject;
    FProductOnClientIndex: TStringList;
    //FSslProtocol: TIdSSLVersion;
    mylist: TStringList;
    FCommunicationMode: integer;
    //Function getMapOfProductSwitches : TStringList;
    //Function getProductRequirements (requirementType : String) : TStringList;
    function getProductRequirements(productname: string;
      requirementType: string): TStringList;
    function getMapOfProductStates: TStringList;
    procedure reverseProductOrderByUninstall(var MapOfProductStates: TStringList);
    //procedure productOnClient_getobject_actualclient;
    function getInstallableProducts: TStringList;
    function getOpsiModules: TStringList;
    function getLicenseOnClientObject(const parameters: array of string;
      var errorOccured: boolean): string;
    procedure SetProductOnClientData(const ProductOnClientData: TStringList);
    function GetSortedProductOnClientListFromService: TStringList;
  protected
    FServiceLastErrorInfo: TStringList;
    //FactualClient: string;
  public
    //actualClient: string;
    { constructor }
    constructor Create;
    { destructor }
    destructor Destroy; override;
    { function }
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
    function getMethodSignature(const methodname: string): TStringList;
    function initProduct: boolean; override;
    function getActualProductVersion: string; override;
    function getSpecialScriptPath: string; override;
    function getProductproperties: TStringList; override;
    function getBeforeRequirements: TStringList; override;
    function getAfterRequirements: TStringList; override;
    function getListOfProductIDs: TStringList; override;
    function getProductPropertyList(myproperty: string;
      defaultlist: TStringList; var usedefault: boolean): TStringList; overload;
    function getProductPropertyList(myproperty: string;
      defaultlist: TStringList; myClientId: string; myProductId: string;
      var usedefault: boolean): TStringList; overload;
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
    function getProductScriptPath(actionType: TAction): string; override;
    function getInstallationPriority: integer; override;
    function actionResultToString(actionResult: TActionResult4): string;
    function actionRequest4ToString(actionRequest: TActionRequest4): string;
    function targetConfigurationToString(targetConfiguration: TTargetConfiguration4)
      : string;
    function installationStatusToString(installationStatus: TProductstate4): string;
    function UpdateSwitches(extremeErrorLevel: TErrorLevel): boolean; override; overload;
    function UpdateSwitches(extremeErrorLevel: TErrorLevel; Progress: string): boolean;
      overload;
    function withLicenceManagement: boolean; override;
    function withRoamingProfiles: boolean;
    function linuxAgentActivated: boolean;
    function macosAgentActivated: boolean;
    function isConnected: boolean;
    function isConnected2(loglist: TStringList): boolean;
    function getMapOfLoginscripts2Run(allscripts: boolean): TStringList;
    function getMapOfProductActionRequests: TStringList;
    function getFileFromDepot(filename: string; toStringList: boolean;
      var ListResult: TStringList): boolean;
    function getJSONFromService(const method: string; const params: array of string; logErrorMessage: string):string;
    function getOpsiServiceConfigs: string;
    function getConfigStateObjectsFromService(ConfigIDsAsJsonArray:string):string;
    function getConfigObjectsFromService(ConfigIDsAsJsonArray:string):string;
    function getLogSize: int64;
    function getProductIds: TStringList;
    function getLocalbootProductIds: TStringList;
    function getNetbootProductIds: TStringList;
    function demandLicenseKey(const parameters: array of string; var errorOccured: boolean):string;
    {$IFNDEF SYNAPSE}
    function decreaseSslProtocol: boolean;
    {$ENDIF SYNAPSE}
    { procedure }
    procedure initOpsiConf(serviceURL, username, password: string); overload;
    procedure initOpsiConf(serviceURL, username, password, sessionid: string); overload;
    procedure initOpsiConf(serviceURL, username, password, sessionid, ip, port: string);
      overload;
    procedure initOpsiConf(serviceURL, username, password, sessionid,
      ip, port, agentstring: string);
      overload;
    procedure saveOpsiConf; override;
    procedure finishOpsiConf; override;
    procedure setActualClient(computername: string); override;
    procedure setActualProductName(const productname: string); override;
    procedure setActionProgress(const progress: string);
    procedure finishProduct; override;
    procedure setActualProductActionRequest(request: string);
    procedure setProductActionRequest(newAction: TActionRequest); override;
    procedure setProductActionRequestWithDependencies(newAction: TActionRequest);
    procedure setProductState(newState: TProductState); override;
    procedure setProductProgress(myprogres: string);
    procedure setProductStateActionRequest(newState: TProductState;
      newAction: TActionRequest); override;
    procedure ProductOnClient_update(actionProgressS: string;
      actionResult: TActionResult4; actionRequest: TActionRequest4;
      targetConfiguration: TTargetConfiguration4; lastAction: TActionRequest4;
      installationStatus: TProductstate4);
    function getOpsiVersion: string;  // get opsiversion from backend_info
    // get list of methods via service:
    function getOpsiServiceMethods(allow_deprecated : boolean) : TStringlist;
    // is a given method well known:
    function isMethodProvided(Method : string): boolean;
    { properties }
    //property sslProtocol: TIdSSLVersion read FSslProtocol write FSslProtocol;
    property serviceUrl: string read FServiceURL;
    property depotId: string read FDepotId;
    property ServiceLastErrorInfo: TStringList read FServiceLastErrorInfo;
    //property actualclient: string read FactualClient write FactualClient;
    property CommunicationMode: integer read FCommunicationMode
      write FCommunicationMode;
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

(******************************************************************************)
(*                      Implementations                                       *)
(******************************************************************************)

{$IFDEF OPSISCRIPT}
uses
  {$IFDEF GUI}
  osbatchgui,
  {$ENDIF GUI}
  osparser,
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
  //ContentEncodingCommpress: string = 'gzip, deflate';
  //ContentEncodingCommpress: string = 'gzip';
  ContentEncodingCommpress: string = 'deflate';
  //ContentEncodingCommpress: string = 'deflate, gzip';
  ContentEncodingNoCommpress: string = 'plain';
  AcceptCompress: string = 'application/json';
  AcceptNoCompress: string = 'application/json';
  //AcceptEncodingCompress: string = 'gzip, deflate';
  //AcceptEncodingCompress: string = 'deflate, gzip';
  AcceptEncodingCompress: string = 'deflate';
  AcceptEncodingNoCompress: string = 'plain';
  OpsiVersion: string = '';

{$IFNDEF OPSISCRIPT}
function StringReplace(const S, OldPattern, NewPattern: string): string;
begin
  Result := SysUtils.StringReplace(s, OldPattern, NewPattern,
    [rfReplaceAll, rfIgnoreCase]);
end;

{$ENDIF}

{$IFDEF SYNAPSE}
function MemoryStreamToString(M: TMemoryStream): string;
begin
  SetString(Result, PChar(M.Memory), M.Size div SizeOf(char));
end;

{$ENDIF SYNAPSE}

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
  LogDatei.log_prog('Start of function getOpsiServerVersion, Line: ' +
    {$INCLUDE %LINE%}, LLNotice);
  Result := '';
  credentialsValid := True;
  FValidCredentials := credentialsValid;
  omc := nil;
  FJsonExecutioner := nil;
  //LogDatei.LogLevel := LLdebug2;
  try
    //if FJsonExecutioner <> nil then FJsonExecutioner.free;
    if Assigned(FJsonExecutioner) then
      FreeAndNil(FJsonExecutioner);
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
    (* this sems to be buggy opsi3 code (do 3.2019)
    if Result = '' then
    begin
      //result := '3';
      credentialsValid := FValidCredentials;
      if credentialsValid then
      begin
        LogDatei.log('backend_info with no result', LLdebug2);
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
          LogDatei.log('Exception in getOpsiServerVersion ', LLdebug2);
          //result := '3';
        end;
      end;
    end;
    *)
  except
    //result := '3';
    try
      if Assigned(FJsonExecutioner) then
        FreeAndNil(FJsonExecutioner);
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
      LogDatei.log('Exception in getOpsiServerVersion ', LLdebug2);
      //result := '3';
    end;
  end;
  LogDatei.log('opsi Server Version : ' + Result, LLnotice);
  //LogDatei.LogLevel := LLnotice;
  LogDatei.log_prog('End of function getOpsiServerVersion, Line: ' +
    {$INCLUDE %LINE%}, LLNotice);
end;

function getOpsiServiceVersion(const serviceUrl: string; const username: string;
  const password: string; var sessionid: string): string;
var
  sign: integer;
  InfoSyntaxError: string;
begin
  Result := '4'; //default to opsi 4.x
  OpsiVersion := getOpsiServerVersion(serviceUrl, username, password, sessionid);
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


constructor TString.Create(const AStr: string);
begin
  inherited Create;
  FStr := AStr;
end;

(******************************************************************************)
(*                                                                            *)
(*                    Implementation of class TOpsiData                       *)
(*                                                                            *)
(******************************************************************************)
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

    (*
    // but if we did the action on hehalf of a policy we stay on following policies and reset new the action request:

    if ar in [tapNull_byPolicy, tapSetup_byPolicy, tapUpdate_byPolicy,
      tapDeinstall_byPolicy] then
    begin
      newActionRequest := tap_byPolicy;
    end;
    *)

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

//========== opsidata-Service ==================================================

{ helper classes }

(******************************************************************************)
(*                                                                            *)
(*                 Implementation of class TOpsiMethodCall                    *)
(*                                                                            *)
(******************************************************************************)
constructor TOpsiMethodCall.Create(const method: string; parameters: array of string);
var
  i: integer;
begin

  FOpsiMethodname := method;

  Fparameterlist := TStringList.Create;
  FJSONValueSyntaxInParameterList := False;

  for i := 0 to high(parameters) do
  begin
    LogDatei.log('Parameters in OpsiMethodCall: ' + parameters[i], LLdebug3);
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
  FJSONValueSyntaxInParameterList := False;

  for i := 0 to high(parameters) do
  begin
    LogDatei.log('Parameters in OpsiMethodCall: ' + parameters[i], LLdebug3);
    Fparameterlist.add(parameters[i]);
  end;
  for i := 0 to hashlist.Count - 1 do
  begin
    LogDatei.log('hashlist in OpsiMethodCall: ' + hashlist.Strings[i], LLdebug3);
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
        LogDatei.log('Creating TSuperObject with: ' +
          parameterlist.Strings[i], LLdebug3);
        //joParams.put(TSuperObject.create(parameterlist.Strings[i]));
        joParams.AsArray.Add(SO(parameterlist.Strings[i]));
        //joParams.AsArray.Add()
        //LogDatei.log ('resulting TSuperArray string: ' + joParams. ,LLdebug2);
      end
      else
      begin
        testresult := parameterlist.Strings[i];
        if JSONValueSyntaxInParameterList then
          LogDatei.log('Putting to TSuperArray with: ' +
            SO(parameterlist.Strings[i]).AsJSon, LLdebug3)
        else
          LogDatei.log('Putting to TSuperArray with: ' +
            SO('"' + parameterlist.Strings[i] + '"').AsJSon, LLdebug3);
        //joParams.put(parameterlist.Strings[i]);
        if (length(parameterlist.Strings[i]) > 0) and
          ((parameterlist.Strings[i][1] = '[')) and
          ((parameterlist.Strings[i][length(parameterlist.Strings[i])] = ']')) then
        begin
          if parameterlist.Strings[i] = '[]' then
          begin
            joParams.AsArray.Add(SA([]));
            LogDatei.log('Adding empty Array: ' + parameterlist.Strings[i], LLdebug3);
          end
          else
          begin
            LogDatei.log('Adding Array: ' + parameterlist.Strings[i], LLdebug3);
            LogDatei.log('Adding Array as Json: ' +
              SO(parameterlist.Strings[i]).AsString, LLdebug3);
            joParams.AsArray.Add(SO(parameterlist.Strings[i]));
            //LogDatei.log('Adding Array as Json: ' + SA([copy(parameterlist.Strings[i], 2, length(parameterlist.Strings[i]) - 2)]).AsJSon, LLdebug3);
            //joParams.AsArray.Add(SA([copy(parameterlist.Strings[i], 2, length(parameterlist.Strings[i]) - 2)]));
            //LogDatei.log('Adding Array as String: ' + SA([copy(parameterlist.Strings[i], 2,length(parameterlist.Strings[i]) - 2)]).AsString , LLdebug3);
            //joParams.AsArray.Add(SA([copy(parameterlist.Strings[i], 2,length(parameterlist.Strings[i]) - 2)]).AsString);
            //joParams.AsArray.Add(copy(parameterlist.Strings[i], 2,length(parameterlist.Strings[i]) - 2));
            //joParams.AsArray.Add(parameterlist.Strings[i]);
          end;
        end
        else
        begin
          if JSONValueSyntaxInParameterList then
            joParams.AsArray.Add(SO(parameterlist.Strings[i]))
          else
            joParams.AsArray.Add(SO('"' + parameterlist.Strings[i] + '"'));
        end;
        //joParams.AsArray.Add(SO(parameterlist.Strings[i]));
        //LogDatei.log ('resulting TSuperArray string: ' + joParams.tostring ,LLDebug3);
      end;
    end;

    //LogDatei.log ('resulting TSuperArray string: ' + joParams.tostring ,LLdebug2);
    testresult := joParams.AsJSon(False, True);
    jO['params'] := TSuperObject.Create(stArray);
    jO['params'] := joParams;
    //jO.put('params', joParams);


    //result := jO.toString;
    Result := jO.AsJSon(False, False);
    testresult := Result;
    //LogDatei.log ('resulting getJsonUrlString: ' + Result ,LLdebug2);

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
      LogDatei.log('Exception in OpsiMethodCall.getJsonUrlString: ' +
        ex.message, LLdebug2);
    end;
  end;
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
    LogDatei.log('while getJsonHashListString: ' + jo.AsJSon(False, False) +
      '<', LLdebug3);
    inmap := False;
    //jostr := '';
    for i := 0 to hashlist.Count - 1 do
    begin
      if hashlist.Strings[i] = '[newmap]' then
      begin
        if inmap and not jotmp.IsType(stNull) then
        begin
          LogDatei.log('while getJsonHashListString try to add: ' +
            jotmp.AsJSon(False, False), LLdebug3);
          try
            //jotmp := SO('{'+jostr+'}');
            if not jotmp.IsType(stNull) then
            begin
              joParams.AsArray.Add(jotmp);
            end
            else
              LogDatei.log('while getJsonHashListString failed to add: ' +
                jotmp.AsJSon(False, False), LLdebug2);
            LogDatei.log('while getJsonHashListString: ' +
              jOParams.AsJSon(False, False), LLdebug2);
          except
            on ex: Exception do
            begin
              LogDatei.log(
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
      LogDatei.log('while getJsonHashListString added: ' +
        jotmp.AsJSon(False, False), LLdebug3);
      try
        joParams.AsArray.Add(jotmp);
        LogDatei.log('while getJsonHashListString: ' +
          jOParams.AsJSon(False, False), LLdebug3);
      except
        on ex: Exception do
        begin
          LogDatei.log(
            'Exception in OpsiMethodCall.getJsonHashListString: {' +
            jotmp.AsJSon(False, False) + '} is no JSON object ' + ex.message, LLError);
        end;
      end;
    end;
    LogDatei.log('while getJsonHashListString: ' +
      jOParams.AsJSon(False, False), LLdebug3);
    jO['params'] := TSuperObject.Create(stArray);
    joParams0.AsArray.Add(joParams);
    jO['params'] := joParams0;
    Result := jO.AsJSon(False, False);
    testresult := Result;
    LogDatei.log('final getJsonHashListString: ' +
      jO.AsJSon(True, False), LLdebug3);
  except
    on ex: Exception do
    begin
      LogDatei.log('Exception in OpsiMethodCall.getJsonHashListString: ' +
        ex.message, LLError);
    end;
  end;
end;

(******************* End of TOpsiMethodCall ***********************************)

(******************************************************************************)
(*                                                                            *)
(*  Implementation of class TJsonThroughHTTPS                                 *)
(*                                                                            *)
(******************************************************************************)
constructor TJsonThroughHTTPS.Create(const serviceURL, username, password: string);
begin
  Create(serviceUrl, username, password, '', '', '');
end;

constructor TJsonThroughHTTPS.Create(
  const serviceURL, username, password, sessionid: string);
begin
  Create(serviceUrl, username, password, sessionid, '', '');
end;

constructor TJsonThroughHTTPS.Create(
  const serviceURL, username, password, sessionid, ip, port: string);
begin
  Create(serviceUrl, username, password, sessionid, ip, port,
    ExtractFileName(ParamStr(0)));
end;

constructor TJsonThroughHTTPS.Create(
  const serviceURL, username, password, sessionid, ip, port, agent: string);
begin
  inherited Create;
  //portHTTPS := port;
  //portHTTP := 4444;
  FserviceURL := serviceURL;
  Fusername := username;
  Fpassword := password;
  FSessionId := sessionid;
  FCommunicationMode := -1; //-1 means Communication mode is not set e.g. it is unknown
  MyMemoryStream := TMemoryStream.Create;
  FResultLines := TStringList.Create;
  FErrorInfo := TStringList.Create;
  {$IFDEF OPSISCRIPT}
  createSocket(osconf.selfProductName + ' / ' + osconf.OpsiscriptVersion, ip, port);
  {$ELSE OPSIWINST}
    {$IFDEF OCASIMP}
  createSocket('ocasimp ', ip, port);
    {$ELSE OCASIMP}
  createSocket(agent, '', '');
    {$ENDIF OCASIMP}
  {$ENDIF OPSISCRIPT}
  //nullO := Null.create;
  methodGET := False;
end;

procedure TJsonThroughHTTPS.createSocket;
begin
  createSocket('', '', '');
end;


procedure TJsonThroughHTTPS.createSocket(const agent, ip, port: string);
begin
  {$IFDEF SYNAPSE}
  // Timeout
  // https://forum.lazarus.freepascal.org/index.php?topic=40167.0
  try
    if Assigned(HTTPSender) then FreeAndNil(HTTPSender);
    HTTPSender := THTTPSend.Create;
    HTTPSender.Protocol := '1.1';
    //HTTPSender.Sock.PreferIP4:= False; //might be switched to false if IPv6 is standard
    //HTTPSender.Sock.Family:= SF_IP6; //do not set this if IPv4 addresses are still in use
    HTTPSender.Sock.CreateWithSSL(TSSLOpenSSL3);
    HTTPSender.Sock.Connect(ip, port);
    //LogDatei.log('IP: ' + ip + ' Resolved: ' + Httpsender.Sock.GetRemoteSinIP, LLDebug);
    if SSL_OPENSSL_LIB_UNIT.InitSSLInterface then
    begin
      LogDatei.log_prog('InitSSLInterface = true, IsSSLloaded: ' +
        BoolToStr(SSL_OPENSSL_LIB_UNIT.IsSSLloaded, True), LLdebug);
    end
    else
      LogDatei.log_prog('InitSSLInterface = false, IsSSLloaded: ' +
        BoolToStr(SSL_OPENSSL_LIB_UNIT.IsSSLloaded, True), LLdebug);
    LogDatei.log('SSL lib (path) should be: ' + SSL_OPENSSL_LIB_UNIT.DLLSSLName, LLInfo);
    HTTPSender.Sock.SSLDoConnect;
    //LogDatei.log('SLLVersion : ' + HTTPSender.Sock.SSL.LibVersion, LLdebug);
    if not SSL_OPENSSL_LIB_UNIT.IsSSLloaded then
    begin
      // no SSL available, loading libs failed
      LogDatei.log('no SSL available, loading libs failed: ' +
        SSL_OPENSSL_LIB_UNIT.DLLSSLName, LLError);
    end
    else
    begin
      LogDatei.log('SSL lib loaded: ' + HTTPSender.Sock.SSL.LibVersion, LLDebug);
      LogDatei.log('Used Protocol: ' + HTTPSender.Sock.SSL.GetSSLVersion, LLdebug);
    end;

    HTTPSender.Sock.SSL.VerifyCert := False;
    HTTPSender.Username := Fusername;
    LogDatei.log_prog('HTTPSender.Username ' + HTTPSender.Username, LLdebug2);
    HTTPSender.Password := Fpassword;
    HTTPSender.UserAgent := agent;
    try
      if ip <> '' then
      begin
        //IdHTTP.BoundIP := ip;
        // ????
      end;
    except
      on ex: Exception do
      begin
        LogDatei.log(
          'Exception in TJsonThroughHTTPS.createSocket: IdHTTP.BoundIP:' +
          ex.message, LLError);
      end;
    end;
    LogDatei.log('HTTPSender.Sock.GetLocalSinIP : ' +
      HTTPSender.Sock.GetLocalSinIP, LLdebug2);
  except
    on ex: Exception do
    begin
      LogDatei.log('Exception in TJsonThroughHTTPS.createSocket: ' +
        ex.message, LLError);
    end;
  end;
  {$ELSE SYNAPSE}
  try
    IdSSLIOHandlerSocket := TIdSSLIOHandlerSocketOpenSSL.Create(nil);

    with IdSSLIOHandlerSocket do
    begin
      SSLOptions.VerifyMode := [];
      SSLOptions.VerifyDepth := 0;
      LogDatei.log('Working with ssl protocol: sslvSSLv23 - auto negotation', LLDebug);
      // sslvSSLv23 means: try all possible Versions
      SSLOptions.Method := sslvSSLv23;
      SSLOptions.Mode := sslmUnassigned;
    end;


    //IdHTTP := TIdHTTP.Create(nil);
    IdHTTP := TIdWebDAV.Create(nil);
    //LogDatei.log('createSocket--->4', LLdebug2);
    IdHTTP.IOHandler := IdSSLIOHandlerSocket;
    //LogDatei.log('createSocket--->5', LLdebug2);
    IdHTTP.Request.BasicAuthentication := True;
    IdHTTP.Request.Username := Fusername;
    IdHTTP.Request.Password := Fpassword;
    IdHTTP.Request.UserAgent := agent;
    //Logdatei.Log('createSocket--->6', LLdebug2);
    try
      if ip <> '' then
      begin
        IdHTTP.BoundIP := ip;
      end;
    except
      on ex: Exception do
      begin
        LogDatei.log(
          'Exception in TJsonThroughHTTPS.createSocket: IdHTTP.BoundIP:' +
          ex.message, LLError);
      end;
    end;
    //LogDatei.log('createSocket-BoundIp: '+IdSSLIOHandlerSocket.BoundIP, LLdebug2);
    LogDatei.log('createSocket-BoundIp: ' + IdHTTP.BoundIP, LLdebug2);
    LogDatei.log('Using OpenSSL Version:: ' + OpenSSLVersion, LLdebug);
  except
    on ex: Exception do
    begin
      LogDatei.log('Exception in TJsonThroughHTTPS.createSocket: ' +
        ex.message, LLError);
    end;
  end;
  {$ENDIF SYNAPSE}
end;

destructor TJsonThroughHTTPS.Destroy;
begin
  //nullO.free;
  {$IFDEF SYNAPSE}
  mymemorystream.Free;
  FResultLines.Free;
  HTTPSender.Sock.CloseSocket;
  HTTPSender.Sock.Free;
  HTTPSender.Free;
  inherited Destroy;
  {$ELSE SYNAPSE}
  mymemorystream.Free;
  FResultLines.Free;

  IdSSLIOHandlerSocket.Close;
  IdSSLIOHandlerSocket.Free;

  IdHTTP.Free;
  inherited Destroy;
  {$ENDIF SYNAPSE}

end;

function TJsonThroughHTTPS.CompressStream(InStream: TMemoryStream;
  const ContentEncoding: string): TMemoryStream;
  //var
  //  CompressedMemoryStream:TMemoryStream;
begin
  Result := TMemoryStream.Create;
  { gzip format (RFC 1952) }
  if ContentEncoding = 'gzip' then //opsi 4.1 and opsi 4.2
  begin
    zipStream(InStream, Result, zcDefault, zsGZIP);
    //Result.LoadFromStream(CompressedMemoryStream);
  end
  else
  { zlib format (RFC 1950) in combination with "deflate" compression (RCF 1952) }
  if (ContentEncoding = 'deflate') or (ContentEncoding = '') then // opsi 4.0
  begin
    zipStream(InStream, Result, zcDefault, zsZLib);
    //Result.LoadFromStream(CompressedMemoryStream);
  end
  else
  begin
    { if ContentEncoding not gzip or deflate give InStream back}
    Result.LoadFromStream(Instream);
  end;
  //CompressedMemoryStream.Free;
end;

function TJsonThroughHTTPS.GetSynapseSocketFamilyAsString: string;
var
  Family: string;
begin
  case HTTPSender.Sock.Family of
    SF_ANY: Family := 'SF_ANY';
    SF_IP4: Family := 'SF_IP4';
    SF_IP6: Family := 'SF_IP6';
  end;
  Result := Family;
end;

procedure TJsonThroughHTTPS.LogHostIPs;
var
  IPAddrList: TStringList;
  i: integer;
begin
  IPAddrList := TStringList.Create;
  HTTPSender.Sock.ResolveNameToIP(HttpSender.TargetHost, IPAddrList);
  LogDatei.log('Resolve (' + HttpSender.TargetHost + ') to following IPs: ',
    LLInfo);
  for i := 0 to IPAddrList.Count - 1 do
    LogDatei.log(' ' + IntToStr(i + 1) + '. IP: ' + IPAddrList.Strings[i],
      LLInfo);
  FreeAndNil(IPAddrList);
end;

procedure TJsonThroughHTTPS.makeURL(const omc: TOpsiMethodCall);
var
  rpcstr: string;
begin
  //Furl := 'https://' + fhost + ':' + intToStr(portHTTPS) + '/rpc?' + EncodeUrl(omc.jsonUrlString);
  LogDatei.log('got omc.jsonUrlString: ' + omc.jsonUrlString, LLdebug3);
  //LogDatei.log('got omc.jsonUrlString: ' + omc.getJsonUrlString, LLdebug2);
  // connections to localhost may come from the WAN-Client or the opsiservicall /
  if (FserviceURL = 'https://localhost:4441/opsiclientd') or
    (FserviceURL = 'https://localhost:4441/kiosk') then
  begin
    //Furl := FserviceURL + '/opsiclientd';
    Furl := FserviceURL;
  end
  else
  begin
    if pos('/rpc', FserviceURL) = 0 then
      rpcstr := '/rpc'
    else
      rpcstr := '';
    if methodGet then
      Furl := FserviceURL + rpcstr + '?' + EncodeUrl(omc.jsonUrlString)
    else
      Furl := FserviceURL + rpcstr;
  end;
  LogDatei.log('got Furl: ' + Furl, LLdebug3);
end;

(*
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
begin
  Result := retrieveJSONObject(omc, logging, True, False, 0);
end;*)

function TJsonThroughHTTPS.retrieveJSONObject(const omc: TOpsiMethodCall;
  logging: boolean = True; retry: boolean = True; readOmcMap: boolean = False;
  communicationmode: integer = 0): ISuperObject;

var
  errorOccured: boolean;
  cookieVal: string;
  posColon: integer;
  s, t, teststring: string;
  jO: ISuperObject;
  utf8str: string;//UTF8String;
  SendStream, ReceiveStream: TMemoryStream;
  InStream: TMemoryStream;
  CompressionSendStream: TCompressionStream;
  DeCompressionReceiveStream: TDeCompressionStream;
  //IdCompressorZLib.CompressStream(tmpStream,AResponseInfo.ContentStream,9,GZIP_WINBITS,9,0);
  // IdCompressorZLib.DecompressGZipStream(ARequestInfo.PostStream,tmpStream);
  //CompressionSendStream: Tgzipstream;
  //CompressionReceiveStream: Tungzipstream;
  //gzipstream : TIdCompressorZLib;
  buffer: ^byte;
  readcount: integer;
  compress: boolean;
  startTime: TDateTime;
  ContentType, Accept, ContentEncoding, AcceptEncoding: string;
  {$IFDEF SYNAPSE}
  //HTTPSenderResult: boolean;
  testresultSyn: string;
  i: integer;
  //sendresultcode: integer;
  //sendresultstring: string;
  finished: boolean;
  {$ENDIF SYNAPSE}
  oldTwistedServer: string;
  oldTwistedServerVer: integer = 0;
  responseContentType: string;
  opsi40: boolean = False;

begin
  LogDatei.log_prog('Start of function retrieveJSONObject(omc: ' +
    omc.FOpsiMethodName + ', logging: ' + BoolToStr(logging, True) +
    ', retry: ' + BoolToStr(retry, True) + ', readOmcMap: ' +
    BoolToStr(readOmcMap, True) + ', communicationmode: ' +
    IntToStr(communicationmode) + '), Line: ' + {$INCLUDE %LINE%}
    , LLNotice);
  try
    SendStream := TMemoryStream.Create;
    ReceiveStream := TMemoryStream.Create;
    errorOccured := False;
    Result := nil;
    resultlines.Clear;
    fErrorInfo.Clear;
    compress := False;
    startTime := now;
    finished := False;
    {---------------------------------------------------
    communicationmode : 0 = opsi 4.1 / 4.2 / Request: gzip, Response: gzip, deflate, identity
    communicationmode : 1 = opsi 4.0 / Request: deflate, Response: gzip, deflate, identity
    communicationmode : 2 = opsi 4.2 / Request: identity, Response: gzip, deflate, identity
     ----------------------------------------------------}
    if FCommunicationMode <> -1 then   //if Communictaion mode is set
    begin
      CommunicationMode := FCommunicationMode;
    end;
    case CommunicationMode of
      0:
      begin
        LogDatei.log_prog('Use opsi 4.1 / 4.2 HTTP Header, compress', LLnotice);
        compress := True;
        ContentType := 'application/json; charset=UTF-8';
        Accept := 'application/json';
        AcceptEncoding := 'gzip, deflate, identity';
        //AcceptEncoding := 'deflate';
        ContentEncoding := 'gzip';//'deflate';
        //ContentEncoding := 'deflate';
      end;
      (*1:
      begin
        LogDatei.log('Use opsi 4.1 / 4.2 HTTP Header, plain', LLnotice);
        compress := False;
        ContentType := 'application/json; charset=UTF-8';
        Accept := 'application/json';
        ContentEncoding := 'identity'; //'gzip, deflate, identity';
        AcceptEncoding := 'identity';
        //ContentEncoding := '';
        //AcceptEncoding  := '';
      end;*)
      1:
      begin
        LogDatei.log_prog('Use opsi 4.0  HTTP Header, compress', LLnotice);
        compress := True;
        ContentType := 'gzip-application/json-rpc';
        Accept := 'gzip-application/json-rpc';
        ContentEncoding := '';
        AcceptEncoding := '';
      end;
      2:
      begin
        LogDatei.log_prog('Use opsi 4.1 / 4.2 HTTP Header, identity', LLnotice);
        compress := False;
        ContentType := 'application/json; charset=UTF-8';
        Accept := 'application/json';
        AcceptEncoding := 'gzip, deflate, identity';
        //AcceptEncoding := 'deflate';
        ContentEncoding := 'identity';//'deflate';
        //ContentEncoding := 'deflate';
      end;
      else
      begin
        LogDatei.log('Unkown communication mode', LLcritical);
        exit(nil);
      end;
    end;

    try
      FError := '';
      MyMemoryStream.Clear;
      SendStream.Clear;
      ResultLines.Clear;
      Result := nil;
      //HTTPSender.Clear;
      makeURL(omc);
    {$IFDEF SYNAPSE}
      try
        ////ProcessMess;
        HTTPSender.Document.Clear;
        (*
        if Assigned(HTTPSender.Document) then
        begin
          FreeAndNil(HTTPSender.Document);
          HTTPSender.Document := TMemoryStream.Create;
        end;
        *)

        if FSessionId <> '' then
        begin
          HTTPSender.Cookies.Add(FSessionId);
        end;
        LogDatei.log_prog('Sessionid ' + FSessionId, LLdebug);
        if (HTTPSender.Cookies.Count > 0) then
          LogDatei.log_prog('Cookies synapse: ' + HTTPSender.Cookies[0], LLdebug);
        //@detlef for testing??? (Jan)
        //HTTPSender.Headers.Clear;
        //testresultSyn := HTTPSender.Headers.Text;
        //LogDatei.log_prog('HTTPSender.Headers ' + testresultSyn, LLdebug);

        if FSessionId <> '' then
          // not the first call and we log
          if logging then
            //Logdatei.Log (DateTimeToStr(now) + ' JSON service request ' + Furl , LLnotice);
            LogDatei.log_prog('JSON service request ' + Furl + ' ' +
              omc.FOpsiMethodName, LLinfo);
        if omc.Timeout > 0 then
        begin
          HTTPSender.Timeout := omc.Timeout * 1000;
          HTTPSender.Sock.SetRecvTimeout(omc.Timeout * 1000);
        end;


        if methodGet then
        begin
          //utf8str := Furl;//AnsiToUtf8(Furl);
          LogDatei.log_prog(' JSON service request ' + Furl, LLdebug);
          if HTTPSender.HTTPMethod('GET', Furl) then
          begin
            mymemorystream := HTTPSender.Document;
            LogDatei.log_prog('methodGet ' + Furl + ' memorystream ' +
              mymemorystream.ToString, LLdebug);
          end
          else
            LogDatei.log_prog('methodGet ' + Furl + ' HTTPMethod failed', LLdebug);
        end
        else
        begin
          if readOmcMap then
          begin
            s := omc.getJsonHashListString;
            utf8str := s; //AnsiToUtf8(s);
            LogDatei.log_prog(' JSON service request Furl ' + Furl, LLdebug);
            LogDatei.log_prog(' JSON service request str ' + utf8str, LLdebug);
          end
          else
          begin
            s := omc.jsonUrlString;
            utf8str := s; //AnsiToUtf8(s);
            LogDatei.log_prog(' JSON service request Furl ' + Furl, LLdebug);
            LogDatei.log_prog(' JSON service request str ' + utf8str, LLdebug);
          end;

          try
            begin
              { Preparing Request }
              { Set Headers }
              HTTPSender.Clear; //reset headers, document and Mimetype
              //HTTPSender.Sock.PreferIP4:= False;//might be switched to false if ipv6 is standard
              //HTTPSender.Sock.Family:= SF_IP6; //do not set this if IPv4 addresses are still in use
              //HTTPSender.Cookies.Clear; //do not clear cookies!
              HTTPSender.MimeType := ContentType;
              HTTPSender.Headers.NameValueSeparator := ':';
              // message-header = field-name ":" [ field-value ]  (RFC 2616)
              HTTPSender.Headers.Add('Accept: ' + Accept);
              HTTPSender.Headers.Add('Accept-Encoding: ' + AcceptEncoding);
              if ContentEncoding <> 'identity' then
                HTTPSender.Headers.Add('Content-Encoding: ' + ContentEncoding);
              HTTPSender.Headers.Add('Content-Type: ' + ContentType);
              //HTTPSender.Headers.Add('Clear-Site-Data: "cache", "cookies", "storage", "executionContexts"');
              //HTTPSender.Headers.Add('Cache-Control: no-cache' );
              //HTTPSender.Headers.Add('User-Agent: ' );
              //HTTPSender.Headers.Add('Connection: close' );
              {header logging}
              for i := 0 to HTTPSender.Headers.Count - 1 do
                LogDatei.log_prog('HTTPSender Request Header.Strings: ' +
                  HTTPSender.Headers.Strings[i], LLDebug);
              LogDatei.log_prog('SslLib should be: ' + SSL_OPENSSL_LIB_UNIT.DLLSSLName +
                ' Line:' + {$INCLUDE %LINE%}, LLDebug);
              { Set Body }
              // before writing utf8str to HTTPSender.Document we need to replace all #10(newline), #13 and #9(TAB) by their
              // escape code equivalent for the correct json server request (otherwise we get a 400 bad request)
              utf8str := SysUtils.StringReplace(utf8str, #10, '\n', [rfReplaceAll]);
              utf8str := SysUtils.StringReplace(utf8str, #13, '\r', [rfReplaceAll]);
              utf8str := SysUtils.StringReplace(utf8str, #9, '\t', [rfReplaceAll]);
              HTTPSender.Document.Write(utf8str[1], length(utf8str));
              if ContentEncoding <> 'identity' then
                //change to ContentEncoding = 'gzip' if using deflate alternative
              begin
                SendStream := CompressStream(HTTPSender.Document, ContentEncoding);
                //SendStream:=CreateSendStream(ContentEncoding, utf8str);
                HTTPSender.Document.LoadFromStream(SendStream);
              end;

              {********* defalte alternative ***********}
              {if (ContenCoding = 'deflate') or (ContenCoding = '') then
              begin
                HTTPSender.Document.Clear;
                CompressionSendStream := TCompressionStream.Create(clMax, HTTPSender.Document);
                CompressionSendStream.Write(utf8str[1], length(utf8str));
                CompressionSendStream.Free;
              end;}
              LogDatei.log_prog(' JSON service request Furl ' + Furl, LLdebug);
              LogDatei.log_prog(' JSON service request str ' + utf8str, LLdebug);

              { Send Request }

              LogDatei.log_prog('Socket-Family: ' +
                GetSynapseSocketFamilyAsString, LLDebug);
              LogDatei.log_prog('PreferIP4: ' +
                BoolToStr(HTTPSender.Sock.PreferIP4, True), LLDebug);
              if HTTPSender.HTTPMethod('POST', Furl) then
              begin
                { Evaluate Response Metadata/Headers}
                LogDatei.log('HTTPSender Post succeeded', LLInfo);
                LogDatei.log('Server-FQDN: ' + HttpSender.TargetHost + ' Server-IP: ' +
                  HttpSender.Sock.GetRemoteSinIP, LLInfo);
                LogDatei.log_prog('HTTPSender result: ' +
                  IntToStr(HTTPSender.ResultCode) + ' msg: ' +
                  HTTPSender.ResultString, LLdebug);
                for i := 0 to HTTPSender.Headers.Count - 1 do
                  LogDatei.log_prog('HTTPSender Response Header.Strings: ' +
                    HTTPSender.Headers.Strings[i], LLdebug);
                //LogDatei.log('ReceiveStream: ' + MemoryStreamToString(ReceiveStream),  LLDebug2);
                if HTTPSender.ResultString = 'OK' then
                begin
                  if FCommunicationMode = -1 then
                  begin
                    // we have no saved communication mode
                    if CommunicationMode = 0 then
                    begin
                      LogDatei.log_prog('Communication mode is ' +
                        IntToStr(CommunicationMode) + '. ' +
                        'Opsi Version should be >=4.1', LLDebug);
                    end
                    else
                    begin
                      LogDatei.log_prog('Communication mode is ' +
                        IntToStr(CommunicationMode) + '. ' +
                        'Opsi Version should be 4.0', LLDebug);
                    end;
                    // save the communication mode
                    FCommunicationMode := CommunicationMode;
                  end;
                end
                else
                begin
                  FError := 'HTTP Error occurred. Error code: ' +
                    IntToStr(HTTPSender.ResultCode) + ', msg: ' +
                    HTTPSender.ResultString + ', Server-FQDN: ' + HttpSender.TargetHost + ', Server-IP: ' +
                    HttpSender.Sock.GetRemoteSinIP;
                  raise Exception.Create(FError);
                end;
                //HTTPSender.Headers.NameValueSeparator:= ':';

                if (HTTPSender.Headers.IndexOfName('Content-Encoding') <> -1) then
                  //Content-Encoding header available
                begin
                  ContentEncoding := trim(HTTPSender.Headers.Values['Content-Encoding']);
                  LogDatei.log_prog('Response Content-Encoding: ' +
                    ContentEncoding, llDebug);
                end
                else // no Content-Encoding header available, due to HTTP specification: Content-Encoding = identity
                begin
                  ContentEncoding := 'identity';
                  LogDatei.log_prog(
                    'No Content-Encoding header. Guess identity', llDebug);
                end;
                responseContentType := trim(HTTPSender.Headers.Values['Content-Type']);
                LogDatei.log_prog('Content-Type: ' + responseContentType, llDebug);
                oldTwistedServer := trim(HTTPSender.Headers.Values['Server']);
                LogDatei.log_prog('Server: ' + oldTwistedServer, llDebug);
                if StartsText('Twisted/', oldTwistedServer) then
                begin
                  LogDatei.log_prog('Server is Twisted: opsi 4.0 / 4.1', LLinfo);
                  (*
                  oldTwistedServer := copy(oldTwistedServer, 9, 2);
                  LogDatei.log_prog('Server: ' +
                    oldTwistedServer, llDebug);
                  if TryStrToInt(oldTwistedServer, oldTwistedServerVer) then
                    if (oldTwistedServerVer < 17) and (oldTwistedServerVer > 0) then
                    *)
                  // We cant detect the opsi version by the twisted version (do 20220520)
                  // We try to use the content type
                  if responseContentType = 'gzip-application/json;charset=utf-8' then
                  begin
                    opsi40 := True;
                    LogDatei.log_prog('opsi 4.0 Server detected: content type: ' +
                      responseContentType, llDebug);
                  end
                  else
                  begin
                    opsi40 := False;
                    LogDatei.log_prog('opsi 4.1 Server detected: content type: ' +
                      responseContentType, llDebug);
                  end;
                end;

                { Read Response Body}

                { identity = uncompressed}
                if (ContentEncoding = 'identity') then
                begin
                  ReceiveStream.LoadFromStream(HTTPSender.Document);
                  LogDatei.log_prog('identity : loaded uncompressed to ReceiveStream',
                    llDebug);
                end
                else
                { deflate = zlib format}
                if (ContentEncoding = 'deflate') or
                  (ContentEncoding = '') or opsi40 then
                begin
                  //DeCompressionReceiveStream := TDeCompressionStream.Create(HTTPSender.Document);
                  //ReceiveStream.Seek(0, 0);
                  //DeCompressionReceiveStream.read(ReceiveStream,0);
                  //DeCompressionReceiveStream.Free;
                  //ReceiveStream := HTTPSender.Document;
                  //ReceiveStream.Seek(0, 0);
                  //mymemorystream.Seek(0, 0);
                  DeCompressionReceiveStream :=
                    TDeCompressionStream.Create(HTTPSender.Document);
                  GetMem(buffer, 655360);
                  repeat
                    FillChar(buffer^, 655360, ' ');
                    readcount := DeCompressionReceiveStream.Read(buffer^, 655360);
                    if readcount > 0 then
                      ReceiveStream.Write(buffer^, readcount);
                  until readcount < 655360;
                  DeCompressionReceiveStream.Free;
                  FreeMem(buffer);
                  LogDatei.log_prog('deflate : loaded zlib compressed to ReceiveStream',
                    llDebug);
                end
                else
                { gzip = gzip format}
                if ContentEncoding = 'gzip' then
                begin
                  unzipStream(HTTPSender.Document, ReceiveStream);
                  LogDatei.log_prog('gzip : loaded gzip compressed to ReceiveStream',
                    llDebug);
                end
                else
                  LogDatei.log('Unknown Content-Encoding: ' +
                    ContentEncoding, LLWarning);

                //HTTPSender.Document.SaveToFile('C:\Users\Jan\Documents\ReceiveStream.txt');  //for testing

                //ReceiveStream.LoadFromStream(HTTPSender.Document);
              end
              else
              begin
                { Request failed }
                ErrorOccured := True;
                FError :=
                  'Request failed (Method Post). No connection to server could be established. Server-FQDN: '
                  + HttpSender.TargetHost + ', Server-IP: ' +
                  HttpSender.Sock.GetRemoteSinIP + ' SLL lib loaded: ' +
                  BoolToStr(SSL_OPENSSL_LIB_UNIT.IsSSLloaded, True);
                LogDatei.log_prog(
                  'Request failed (Method Post). No connection to server could be established. Line: '
                  + {$INCLUDE %LINE%}, LLError);
                LogDatei.log_prog('Server-FQDN: ' +
                  HttpSender.TargetHost + ' Server-IP: ' +
                  HttpSender.Sock.GetRemoteSinIP + ' Line: ' + {$INCLUDE %LINE%}, LLInfo);
                LogHostIPs;
                LogDatei.log_prog('SLL lib loaded: ' +
                  BoolToStr(SSL_OPENSSL_LIB_UNIT.IsSSLloaded, True) + ' SSL lib (path) should be: ' +
                  SSL_OPENSSL_LIB_UNIT.DLLSSLName + ' Line: ' + {$INCLUDE %LINE%}, LLInfo);
                raise Exception.Create(FError);
              end;
            end;
          except
            on e: Exception do
            begin
              LogDatei.log('Exception in retrieveJSONObject0: ' +
                e.message, LLdebug2);
              //writeln('debug: Exception in retrieveJSONObject0: ' + e.message);
              if e.message = 'HTTP/1.1 401 Unauthorized' then
                FValidCredentials := False;
              t := s;
              if HTTPSender.ResultCode = 400 then
              begin
                // we have a 400 Bad Request - perhaps opsi server with other communication mode
                LogDatei.log(
                  'We had a 400 (bad request) result - so we retry with other parameters / communication compatibility modes',
                  LLInfo);
                FCommunicationMode := -1;
                Inc(CommunicationMode);
                if (CommunicationMode <= 2) then
                begin
                  LogDatei.log('Retry with communicationmode: ' +
                    IntToStr(communicationmode), LLinfo);
                  Result := retrieveJSONObject(omc, logging, retry,
                    readOmcMap, CommunicationMode);
                end;
              end;
              if (HTTPSender.ResultCode = 500) and (FCommunicationMode = -1) then
              begin
                if (HttpSender.Sock.GetRemoteSinIP = '') then
                begin
                  FError := 'Server (' + HttpSender.TargetHost +
                    ') unreachable. Could not resolve FQDN to valid IP-Address.';
                  LogDatei.log(FError, LLError);
                end
                else
                if not SSL_OPENSSL_LIB_UNIT.IsSSLloaded then
                begin
                  FError := 'Could not load ssl lib: ' + SSL_OPENSSL_LIB_UNIT.DLLSSLName;
                  LogDatei.log(FError, LLError);
                end
                else
                begin
                  // we have a 500 Internal Server Error - perhaps opsi server with other communication mode
                  LogDatei.log(
                    'We had a 500 Internal Server Error result - so we retry with other parameters / communication compatibility modes',
                    LLInfo);
                  FCommunicationMode := -1;
                  Inc(CommunicationMode);
                  if (CommunicationMode <= 2) then
                  begin
                    LogDatei.log('Retry with communicationmode: ' +
                      IntToStr(communicationmode), LLinfo);
                    Result := retrieveJSONObject(omc, logging, retry,
                      readOmcMap, CommunicationMode);
                  end;
                end;
              end;
              finished := True; //Communication failed thus nothing more to do
            end;
          end;
        end;
        if not finished then
        begin
          //endTime := now;
          //if FSessionId <> '' then
          // not the first call and we log
          if logging then
          begin
            LogDatei.log('JSON Bench for ' + omc.OpsiMethodName +
              ' ' + copy(omc.getJsonUrlString, pos(',', omc.getJsonUrlString) +
              1, 50) + ' Start: ' + FormatDateTime('hh:nn:ss:zzz', startTime) +
              ' Time: ' + FormatDateTime('hh:nn:ss:zzz', now - startTime), LLinfo);
            LogDatei.log('JSON Call: ' + omc.getJsonUrlString + ' Time: ' +
              FormatDateTime('hh:nn:ss:zzz', now - startTime), LLDebug2);
          end;

          for i := 0 to HTTPSender.Headers.Count - 1 do
            LogDatei.log_prog('HTTPSender Header.Strings: ' +
              HTTPSender.Headers.Strings[i], LLdebug);
          for i := 0 to HTTPSender.Cookies.Count - 1 do
            LogDatei.log_prog('HTTPSender Cookies.Strings: ' +
              HTTPSender.Cookies.Strings[i], LLdebug);

          //testResult := MemoryStreamToString(HTTPSender.Document); //for debugging
          cookieVal := HTTPSender.Cookies.GetText;

          posColon := -1;
          if cookieVal <> '' then
            posColon := pos(';', cookieVal);

          if posColon > 0 then
            FSessionId := copy(cookieVal, 1, posColon - 1)
          else
            FSessionId := '';
          //Logdatei.Log('JSON retrieveJSONObject: after cookie', LLDebug2);
        end;
      except
        on E: Exception do
        begin
          errorOccured := True;
          FError := FError + '->retrieveJSONObject:2 : ' + E.Message;
          LogDatei.log('Exception in retrieveJSONObject:2: ' +
            e.message, LLdebug2);
        end;
      end;
    {$ELSE SYNAPSE}
      try
        ////ProcessMess;
        if FSessionId <> '' then
        begin
          //IdHttp.Request.RawHeaders.Add('Cookie');
          IdHttp.Request.RawHeaders.Values['Cookie'] := FSessionId;
        end;
        LogDatei.log_prog('Sessionid ' + FSessionId, LLdebug2);
        testresult := IdHttp.Request.RawHeaders.Text;

        if FSessionId <> '' then
          // not the first call and we log
          if logging then
            //LogDatei.log (DateTimeToStr(now) + ' JSON service request ' + Furl , LLnotice);
            LogDatei.log('JSON service request ' + Furl + ' ' +
              omc.FOpsiMethodName, LLDebug);

        if methodGet then
        begin
          utf8str := AnsiToUtf8(Furl);
          LogDatei.log_prog(' JSON service request ' + Furl, LLdebug);
          IdHTTP.Get(utf8str, mymemorystream);
        end

        else
        begin
          if readOmcMap then
          begin
            s := omc.getJsonHashListString;
            utf8str := AnsiToUtf8(s);
            LogDatei.log_prog(' JSON service request Furl ' + Furl, LLdebug);
            LogDatei.log_prog(' JSON service request str ' + utf8str, LLdebug);

          end
          else
          begin
            s := omc.jsonUrlString;
            utf8str := AnsiToUtf8(s);
            LogDatei.log_prog(' JSON service request Furl ' + Furl, LLdebug);
            LogDatei.log_prog(' JSON service request str ' + utf8str, LLdebug);
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
            //LogDatei.log('getOpsiServiceVersion:'+myopsidata.getOpsiServiceVersion, LLdebug2);
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
              LogDatei.log('Exception in retrieveJSONObject0: ' +
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
                //LogDatei.log('getOpsiServiceVersion:'+opsidata.getOpsiServiceVersion, LLdebug2);
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
                  LogDatei.log('Exception in retrieveJSONObject1: ' +
                    e.message, LLdebug2);
                  //writeln('ddebug: Exception in retrieveJSONObject1: ' + e.message);
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
          LogDatei.log_prog('JSON Bench for ' + omc.OpsiMethodName +
            ' ' + copy(omc.getJsonUrlString, pos(',', omc.getJsonUrlString) +
            1, 50) + ' Start: ' + FormatDateTime('hh:nn:ss:zzz', startTime) +
            ' Time: ' + FormatDateTime('hh:nn:ss:zzz', now - startTime), LLinfo);
          LogDatei.log('JSON Call: ' + omc.getJsonUrlString + ' Time: ' +
            FormatDateTime('hh:nn:ss:zzz', now - startTime), LLDebug2);
        end;

        testResult := IdHTTP.ResponseText;
        cookieVal := IdHTTP.Response.RawHeaders.Values['Set-Cookie'];
        LogDatei.log_prog('JSON retrieveJSONObject: after IdHTTP', LLDebug2);
        posColon := -1;
        if cookieVal <> '' then
          posColon := pos(';', cookieVal);

        if posColon > 0 then
          FSessionId := copy(cookieVal, 1, posColon - 1)
        else
          FSessionId := '';

        LogDatei.log_prog('JSON retrieveJSONObject: after cookie', LLDebug2);
      except
        on E: Exception do
        begin
          errorOccured := True;
          FError := FError + '->retrieveJSONObject:2 : ' + E.Message;
          LogDatei.log('Exception in retrieveJSONObject:2: ' +
            e.message, LLdebug2);
        end;
      end;
    {$ENDIF SYNAPSE}
      if not finished then
      begin
        if not ErrorOccured then
        begin
          {if compress then
          begin
            mymemorystream.Position := 0;
            LogDatei.log_prog('JSON retrieveJSONObject: memorystream reseted', LLdebug);
            resultLines.Clear;
            ResultLines.LoadFromStream(mymemorystream);
          end
          else}
          begin
            ReceiveStream.Position := 0;
            LogDatei.log_prog('JSON retrieveJSONObject: ReceiveStream reseted', LLdebug);
            resultLines.Clear;
            ResultLines.LoadFromStream(ReceiveStream);
          end;
          LogDatei.log_prog('JSON retrieveJSONObject: resultlines loaded', LLdebug);
          // should be one line


          if ResultLines.Count < 1 then
          begin
            FError := FError +
              '-> unexpected Result from webservice in retrieveJSONObject, number of lines: '
              + IntToStr(ResultLines.Count);
            LogDatei.log('Error: Got in stream: ' +
              mymemorystream.ReadAnsiString, LLError);
          end
          else
          begin
            //testResult := ResultLines.Text; //for debugging
            Result := SO(ResultLines.Strings[0]);
            LogDatei.log_prog('JSON retrieveJSONObject: result loaded', LLdebug);
            if Result = nil then
              LogDatei.log('JSON retrieveJSONObject: result nil', LLError)
            else
            begin
              try
                if not (Result.N['error'].IsType(stNull)) then
                begin
                  FError := FError + '-> ' + Result.N['error'].AsJSon();
                  jO := Result.O['error'];
                  getStringlistFromJsonObject(jO, fErrorInfo);
                  Result := nil;
                end;
              except
                on e: Exception do
                begin
                  LogDatei.log('exception in retrieveJSONObject: while checking error ' +
                    e.message, LLError);
                end;
              end;
            end;
          end;

        end;

      end;

    except
      on E: Exception do
      begin
        errorOccured := True;
        FError := FError + '-> retrieveJSONObject:1: ' + E.Message;
        LogDatei.log('Exception in retrieveJSONObject:1: ' + e.message, LLdebug2);
        if (E.Message = 'Stream read error') then
        begin
          // we have a Stream read error - perhaps opsi server with other communication mode
          LogDatei.log(
            'We had a Stream read error result - so we retry with other parameters / communication compatibility modes',
            LLInfo);
          FCommunicationMode := -1;
          Inc(CommunicationMode);
          if (CommunicationMode <= 2) then
          begin
            LogDatei.log('Retry with communicationmode: ' +
              IntToStr(communicationmode), LLinfo);
            Result :=
              retrieveJSONObject(omc, logging, retry, readOmcMap,
              CommunicationMode);
          end;
        end;
      end;
    end;
    if not finished then
    begin

      if Result = nil then
      begin
        if logging then
          LogDatei.log('Error: retrieveJSONObject --- opsi service problem ---' +
            FError, LLerror);
        if pos('10054', FError) > 0 //Socket error, connection reset by peer
        then
        begin
          LogDatei.log('trying to rebuild connection', LLInfo);
          sleep(1000);
          //createSocket;
          retrieveJSONObject(omc, logging, False);
        end
        else
        begin
          if pos('Stream read error', FError) = 0 then
            if Assigned(ResultLines) and (resultLines.Count > 0) then
              LogDatei.log('Received (first 512): ' +
                copy(ResultLines.Strings[0], 1, 512), LLerror);
        end;
      end;
    end;
  finally
    SendStream.Free;
    ReceiveStream.Free;
    LogDatei.log_prog('End of function retrieveJSONObject, Line: ' +
      {$INCLUDE %LINE%}, LLNotice);
  end;
end;

(* seems to be not in use do 20190826

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
  {$IFDEF SYNAPSE}
  HTTPSenderResult: boolean;
  testresultSyn: string;
  i: integer;
  sendresultcode: integer;
  sendresultstring: string;
  {$ENDIF SYNAPSE}

begin
  errorOccured := False;
  Result := nil;
  resultlines.Clear;
  fErrorInfo.Clear;
  compress := False;

  try
    mymemorystream.Clear;

    makeurl(omc);
    //    TODO
    try
      if FSessionId <> '' then
      begin
        {$IFDEF SYNAPSE}
        HTTPSender.Cookies.Add(FSessionId);
        {$ELSE SYNAPSE}
        IdHttp.Request.RawHeaders.Values['Cookie'] := FSessionId;
        {$ENDIF SYNAPSE}
      end;
      {$IFDEF SYNAPSE}
      HTTPSender.Headers.Clear;
      testresult := HTTPSender.Headers.Text;
      {$ELSE SYNAPSE}
      testresult := IdHttp.Request.RawHeaders.Text;
      {$ENDIF SYNAPSE}

      if FSessionId <> '' then
        // not the first call and we log
        if logging then
          //LogDatei.log (DateTimeToStr(now) + ' JSON service request ' + Furl + ' '+ omc.FOpsiMethodName, LLInfo);
          LogDatei.log_prog('JSON service request ' + Furl + ' ' +
            omc.FOpsiMethodName, LLDebug);


      if methodGet then
      begin
        utf8str := AnsiToUtf8(Furl);
        LogDatei.log_prog(' JSON service request ' + Furl, LLdebug);
        {$IFDEF SYNAPSE}
        mymemorystream := HTTPSender.Document;
        HTTPSenderResult := HTTPSender.HTTPMethod('GET', Furl);
        mymemorystream := HTTPSender.Document;
        {$ELSE SYNAPSE}
        IdHTTP.Get(utf8str, mymemorystream);
        {$ENDIF SYNAPSE}
      end
      else
      begin
        if readOmcMap then
        begin
          s := omc.getJsonHashListString;
          utf8str := AnsiToUtf8(s);
          //sourcestringlist.add(utf8str);
          LogDatei.log_prog(' JSON service request Furl ' + Furl, LLdebug);
          LogDatei.log_prog(' JSON service request str ' + utf8str, LLdebug);

        end
        else
        begin
          sourcestringlist := TStringList.Create;
          s := omc.jsonUrlString;
          utf8str := AnsiToUtf8(s);
          //sourcestringlist.add(utf8str);
          LogDatei.log_prog(' JSON service request Furl ' + Furl, LLdebug);
          LogDatei.log_prog(' JSON service request str ' + utf8str, LLdebug);
        end;
        try
          sendstream := TMemoryStream.Create;
          ReceiveStream := TMemoryStream.Create;
          sendstream.Clear;
          // we assume opsi4
          compress := True;
          LogDatei.log_prog('Using MimeType: ' + ContentTypeCompress, LLDebug);
          if compress then
          begin
            {$IFNDEF SYNAPSE}
            IdHttp.Request.ContentType := ContentTypeCompress;
            IdHttp.Request.ContentEncoding := ContentEncodingCommpress;
            IdHttp.Request.Accept := AcceptCompress;
            IdHttp.Request.AcceptEncoding := AcceptEncodingCompress;
            LogDatei.log_prog('HTTP Header: ' + IdHttp.Request.RawHeaders.Text, LLDebug);
            {$ENDIF SYNAPSE}
            CompressionSendStream := TCompressionStream.Create(clMax, sendstream);
            CompressionSendStream.Write(utf8str[1], length(utf8str));
            CompressionSendStream.Free;
            //writeln('ddebug: bpost');
            {$IFDEF SYNAPSE}
            HTTPSender.Document.LoadFromStream(sendstream);
            HTTPSenderResult := HTTPSender.HTTPMethod('POST', Furl);
            if HTTPSenderResult then
            begin
              LogDatei.log('HTTPSender Post ok', LLDebug2);
            end
            else
              LogDatei.log('HTTPSender Post failed', LLDebug2);
            sendresultcode := HTTPSender.ResultCode;
            sendresultstring := HTTPSender.ResultString;
            LogDatei.log('HTTPSender result: ' + IntToStr(sendresultcode) +
              ' msg: ' + sendresultstring, LLDebug2);
            if sendresultstring <> 'OK' then
            begin
              raise Exception.Create(HTTPSender.Headers.Strings[0]);
            end;
            ReceiveStream := HTTPSender.Document;
            {$ELSE SYNAPSE}
            IdHTTP.Post(Furl, sendstream, ReceiveStream);
            //writeln('ddebug: apost');
            {$ENDIF SYNAPSE}

            ReceiveStream.Seek(0, 0);
            mymemorystream.Seek(0, 0);
            CompressionReceiveStream := TDeCompressionStream.Create(ReceiveStream);
            GetMem(buffer, 655360);
            repeat
              FillChar(buffer^, 655360, ' ');
              readcount := CompressionReceiveStream.Read(buffer^, 655360);
              if readcount > 0 then
                mymemorystream.Write(buffer^, readcount);
            until readcount < 655360;
            CompressionReceiveStream.Free;
            FreeMem(buffer);
          end
          else
          begin
            {$IFDEF SYNAPSE}
            HTTPSender.Headers.Add('accept-encoding: ' +
              ContentEncodingCommpress + ',identity');
            HTTPSender.Headers.Add('content-encoding: ' + ContentEncodingCommpress);
            HTTPSender.Headers.Add('content-type: ' + ContentTypeCompress + ',identity');
            for i := 0 to HTTPSender.Headers.Count - 1 do
              LogDatei.log('HTTPSender Header.Strings: ' +
                HTTPSender.Headers.Strings[i], LLDebug2);
            sendstream.Write(utf8str[1], length(utf8str));
            HTTPSender.Document.LoadFromStream(sendstream);
            HTTPSenderResult := HTTPSender.HTTPMethod('POST', Furl);
            if HTTPSenderResult then
            begin
              LogDatei.log('HTTPSender Post ok', LLDebug2);
            end
            else
              LogDatei.log('HTTPSender Post failed', LLDebug2);
            sendresultcode := HTTPSender.ResultCode;
            sendresultstring := HTTPSender.ResultString;
            LogDatei.log('HTTPSender result: ' + IntToStr(sendresultcode) +
              ' msg: ' + sendresultstring, LLDebug2);
            if sendresultstring <> 'OK' then
            begin
              raise Exception.Create(HTTPSender.Headers.Strings[0]);
            end;
            mymemorystream := HTTPSender.Document;
            {$ELSE SYNAPSE}
            IdHttp.Request.ContentType := ContentTypeNoCompress;
            IdHttp.Request.ContentEncoding := ContentEncodingNoCommpress;
            IdHttp.Request.Accept := AcceptNoCompress;
            IdHttp.Request.AcceptEncoding := AcceptEncodingNoCompress;
            LogDatei.log('HTTP Header: ' + IdHttp.Request.RawHeaders.Text, LLDebug2);
            sendstream.Write(utf8str[1], length(utf8str));
            IdHTTP.Post(Furl, sendstream, mymemorystream);
            {$ENDIF SYNAPSE}
          end;
        except
          on e: Exception do
          begin
            LogDatei.log_prog('Exception in retrieveJSONArray0: ' +
              e.message, LLdebug);
            //writeln('ddebug: Exception in retrieveJSONArray0: ' + e.message);
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
            LogDatei.log_prog('Changing to MimeType: ' + ContentTypeCompress, LLDebug);
            sendstream.Free;
            //****************
            try
              sendstream := TMemoryStream.Create;
              ReceiveStream := TMemoryStream.Create;
              sendstream.Clear;
              // we assume opsi4
              compress := True;
              LogDatei.log_prog('Using MimeType: ' + ContentTypeCompress, LLDebug2);
              if compress then
              begin
                {$IFDEF SYNAPSE}
                HTTPSender.Headers.Clear;
                HTTPSender.Headers.Add('accept-encoding: ' +
                  ContentEncodingCommpress + ',identity');
                HTTPSender.Headers.Add('content-encoding: ' + ContentEncodingCommpress);
                HTTPSender.Headers.Add('content-type: ' +
                  ContentTypeCompress + ',identity');
                for i := 0 to HTTPSender.Headers.Count - 1 do
                  LogDatei.log('HTTPSender Header.Strings: ' +
                    HTTPSender.Headers.Strings[i], LLDebug2);
                {$ELSE SYNAPSE}
                IdHttp.Request.ContentType := ContentTypeCompress;
                IdHttp.Request.ContentEncoding := ContentEncodingCommpress;
                IdHttp.Request.Accept := AcceptCompress;
                IdHttp.Request.AcceptEncoding := AcceptEncodingCompress;
                {$ENDIF SYNAPSE}
                CompressionSendStream := TCompressionStream.Create(clMax, sendstream);
                CompressionSendStream.Write(utf8str[1], length(utf8str));
                CompressionSendStream.Free;
                //writeln('ddebug: bpost');
                {$IFDEF SYNAPSE}
                HTTPSender.Document.LoadFromStream(sendstream);
                HTTPSenderResult := HTTPSender.HTTPMethod('POST', Furl);
                if HTTPSenderResult then
                  LogDatei.log('HTTPSender Post ok', LLDebug2)
                else
                  LogDatei.log('HTTPSender Post failed', LLDebug2);
                sendresultcode := HTTPSender.ResultCode;
                sendresultstring := HTTPSender.ResultString;
                LogDatei.log('HTTPSender result: ' + IntToStr(sendresultcode) +
                  ' msg: ' + sendresultstring, LLDebug2);
                if sendresultstring <> 'OK' then
                begin
                  raise Exception.Create(HTTPSender.Headers.Strings[0]);
                end;
                ReceiveStream := HTTPSender.Document;
                {$ELSE SYNAPSE}
                IdHTTP.Post(Furl, sendstream, ReceiveStream);
                {$ENDIF SYNAPSE}
                //writeln('ddebug: apost');
                ReceiveStream.Seek(0, 0);
                mymemorystream.Seek(0, 0);
                CompressionReceiveStream := TDeCompressionStream.Create(ReceiveStream);
                GetMem(buffer, 655360);
                repeat
                  FillChar(buffer^, 655360, ' ');
                  readcount := CompressionReceiveStream.Read(buffer^, 655360);
                  if readcount > 0 then
                    mymemorystream.Write(buffer^, readcount);
                until readcount < 655360;
                CompressionReceiveStream.Free;
                FreeMem(buffer);
              end
              else
              begin
                {$IFDEF SYNAPSE}
                HTTPSender.Headers.Clear;
                HTTPSender.Headers.Add('accept-encoding: ' +
                  ContentEncodingCommpress + ',identity');
                HTTPSender.Headers.Add('content-encoding: ' + ContentEncodingCommpress);
                HTTPSender.Headers.Add('content-type: ' +
                  ContentTypeCompress + ',identity');
                for i := 0 to HTTPSender.Headers.Count - 1 do
                  LogDatei.log('HTTPSender Header.Strings: ' +
                    HTTPSender.Headers.Strings[i], LLDebug2);
                HTTPSender.Document.LoadFromStream(sendstream);
                HTTPSenderResult := HTTPSender.HTTPMethod('POST', Furl);
                if HTTPSenderResult then
                  LogDatei.log('HTTPSender Post ok', LLDebug2)
                else
                  LogDatei.log('HTTPSender Post failed', LLDebug2);
                sendresultcode := HTTPSender.ResultCode;
                sendresultstring := HTTPSender.ResultString;
                LogDatei.log('HTTPSender result: ' + IntToStr(sendresultcode) +
                  ' msg: ' + sendresultstring, LLDebug2);
                if sendresultstring <> 'OK' then
                begin
                  raise Exception.Create(HTTPSender.Headers.Strings[0]);
                end;
                mymemorystream := HTTPSender.Document;
                {$ELSE SYNAPSE}
                IdHttp.Request.ContentType := ContentTypeNoCompress;
                IdHttp.Request.ContentEncoding := ContentEncodingNoCommpress;
                IdHttp.Request.Accept := AcceptNoCompress;
                IdHttp.Request.AcceptEncoding := AcceptEncodingNoCompress;
                sendstream.Write(utf8str[1], length(utf8str));
                IdHTTP.Post(Furl, sendstream, mymemorystream);
                {$ENDIF SYNAPSE}
              end;
            except
              on e: Exception do
              begin
                LogDatei.log_prog('Exception in retrieveJSONArray1: ' +
                  e.message, LLdebug);
                //writeln('ddebug: Exception in retrieveJSONArray1: ' + e.message);
                if e.message = 'HTTP/1.1 401 Unauthorized' then
                  FValidCredentials := False;
                t := s;
              end;
            end;

            //****************
          end;
        end;
        sourcestringlist.Free;
        sendstream.Free;
      end;

      {$IFDEF SYNAPSE}
      testresult := HTTPSender.Headers.Text;
      cookieVal := HTTPSender.Cookies[0];
      {$ELSE SYNAPSE}
      testResult := IdHTTP.ResponseText;
      cookieVal := IdHTTP.Response.RawHeaders.Values['Set-Cookie'];
      {$ENDIF SYNAPSE}
      // TODO: hier muss das mit dem Auslesen der Cookies noch anders gemacht werden.
      // Was will man hier wissen???
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
        //LogDatei.log (DateTimeToStr(now) + ' JSON result ' + testresult ,  LLInfo);
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
      LogDatei.log('Error: --- opsi service problem ---' + FError, LLerror);

    if pos('10054', FError) > 0 //Socket error, connection reset by peer
    then
    begin
      LogDatei.log_prog('trying to rebuild connection', LLInfo);
      sleep(1000);
      //createSocket;
      retrieveJSONArray(omc, logging, False);
    end;
  end;
end;

*)

function TJsonThroughHTTPS.retrieveJSONObjectByHttpPost(InStream: TMemoryStream;
  logging: boolean; communicationmode: integer): ISuperObject;
  // This function is used by sendlog
var
  errorOccured: boolean;
  cookieVal: string;
  posColon: integer;
  //  s,t : String;
  SendStream, ReceiveStream: TMemoryStream;
  CompressionSendStream: TCompressionStream;
  DeCompressionReceiveStream: TDeCompressionStream;
  buffer: ^byte;
  readcount: integer;
  compress: boolean;
  ContentType, Accept, ContentEncoding, AcceptEncoding: string;
  {$IFDEF SYNAPSE}
  //HTTPSenderResult: boolean;
  testresultSyn: string;
  i: integer;
  //sendresultcode: integer;
  //sendresultstring: string;
  {$ENDIF SYNAPSE}
begin
  LogDatei.log_prog('Start of retrieveJSONObjectByHttpPost(InStream (pointer address): '
    +
    IntToHex(QWord(InStream)) + ', logging: ' + BoolToStr(logging, True) +
    ', communicationmode: ' + IntToStr(communicationmode) + '), Line: ' + {$INCLUDE %LINE%}
    , LLDebug2);
  errorOccured := False;
  Result := nil;
  resultlines.Clear;
  {---------------------------------------------------
    communicationmode : 0 = opsi 4.1 / 4.2 / Request: gzip, Response: gzip, deflate, identity
    communicationmode : 1 = opsi 4.0 / Request: deflate, Response: gzip, deflate, identity
    communicationmode : 2 = opsi 4.2 / Request: identity, Response: gzip, deflate, identity
   ----------------------------------------------------}
  if FCommunicationMode <> -1 then   //if Communictaion mode is set
  begin
    CommunicationMode := FCommunicationMode;
  end;
  case CommunicationMode of
    0:
    begin
      LogDatei.log_prog('Use opsi 4.1 / 4.2 HTTP Header, compress', LLnotice);
      compress := True;
      ContentType := 'application/json; charset=UTF-8';
      Accept := 'application/json';
      AcceptEncoding := 'gzip, deflate, identity';
      //AcceptEncoding := 'deflate';
      ContentEncoding := 'gzip';
      //ContentEncoding := 'deflate';
    end;
    {1:
    begin
      LogDatei.log('Use opsi 4.1 / 4.2 HTTP Header, plain', LLnotice);
      compress := False;
      ContentType := 'application/json; charset=UTF-8';
      Accept := 'application/json';
      ContentEncoding := 'identity'; //'gzip, deflate, identity';
      AcceptEncoding := 'identity';
      //ContentEncoding := '';
      //AcceptEncoding  := '';
    end;}
    1:
    begin
      LogDatei.log_prog('Use opsi 4.0  HTTP Header, compress', LLnotice);
      compress := True;
      ContentType := 'gzip-application/json-rpc';
      Accept := 'gzip-application/json-rpc';
      ContentEncoding := '';
      AcceptEncoding := '';
    end;
    2:
    begin
      LogDatei.log_prog('Use opsi 4.1 / 4.2 HTTP Header, identity', LLnotice);
      compress := False;
      ContentType := 'application/json; charset=UTF-8';
      Accept := 'application/json';
      AcceptEncoding := 'gzip, deflate, identity';
      //AcceptEncoding := 'deflate';
      ContentEncoding := 'identity';//'deflate';
      //ContentEncoding := 'deflate';
    end;
    else
    begin
      LogDatei.log('Unkown communication mode', LLcritical);
      exit(nil);
    end;
  end;

  try
    MyMemoryStream.Clear;
    try
      if FSessionId <> '' then
      begin
        {$IFDEF SYNAPSE}
        HTTPSender.Cookies.Add(FSessionId);
        {$ELSE SYNAPSE}
        IdHttp.Request.RawHeaders.Values['Cookie'] := FSessionId;
        {$ENDIF SYNAPSE}
      end;
      {$IFDEF SYNAPSE}
      testresult := HTTPSender.Headers.Text;
      {$ELSE SYNAPSE}
      testresult := IdHttp.Request.RawHeaders.Text;
      {$ENDIF SYNAPSE}
      if FSessionId <> '' then
        // not the first call and we log
        if logging then
          LogDatei.log_prog(DateTimeToStr(now) + ' JSON service request ' +
            Furl, LLInfo);
      //LogDatei.log ('JSON service request ' + Furl + ' '+ omc.FOpsiMethodName, LLinfo);
      try
        SendStream := TMemoryStream.Create;
        ReceiveStream := TMemoryStream.Create;
        SendStream.Clear;
        (*
        // we assume opsi4
        compress := True;
        *)
        LogDatei.log_prog('Using MimeType: ' + ContentType, LLDebug);
        //if compress then
        begin
          LogDatei.log_prog('Using MimeType: ' + ContentType, LLDebug);
          {$IFDEF SYNAPSE}
          { Preparing Request }

          { Set Headers }
          HTTPSender.Clear; //reset headers, document and Mimetype
          //HTTPSender.Cookies.Clear; //Do not clear cookies
          HTTPSender.MimeType := ContentType;
          HTTPSender.Headers.NameValueSeparator := ':';
          // message-header = field-name ":" [ field-value ]  (RFC 2616)
          HTTPSender.Headers.Add('Accept: ' + Accept);
          HTTPSender.Headers.Add('Accept-Encoding: ' + AcceptEncoding);
          if ContentEncoding <> 'identity' then
            HTTPSender.Headers.Add('Content-Encoding: ' + ContentEncoding);
          HTTPSender.Headers.Add('Content-Type: ' + ContentType);
          //HTTPSender.Headers.Add('Clear-Site-Data: "cache", "cookies", "storage", "executionContexts"');
          //HTTPSender.Headers.Add('Cache-Control: no-cache' );
          //HTTPSender.Headers.Add('User-Agent: ' );
          //HTTPSender.Headers.Add('Connection: close' );
          { header logging }
          for i := 0 to HTTPSender.Headers.Count - 1 do
            LogDatei.log_prog('HTTPSender Request Header.Strings: ' +
              HTTPSender.Headers.Strings[i], LLDebug);

          {$ELSE SYNAPSE}
          IdHttp.Request.ContentType := ContentTypeCompress;
          IdHttp.Request.ContentEncoding := ContentEncodingCommpress;
          IdHttp.Request.Accept := AcceptCompress;
          IdHttp.Request.AcceptEncoding := AcceptEncodingCompress;
          LogDatei.log_prog('HTTP Header: ' + IdHttp.Request.RawHeaders.Text, LLDebug);
          {$ENDIF SYNAPSE}
          { Set Body }

          if ContentEncoding <> 'identity' then
            //change to ContentEncoding = 'gzip' if using deflate alternative
          begin
            SendStream := CompressStream(InStream, ContentEncoding);
            //SendStream:=CreateSendStream(ContentEncoding, utf8str);
            HTTPSender.Document.LoadFromStream(SendStream);
          end
          else
          begin
            HTTPSender.Document.LoadFromStream(InStream);
          end;
          {********* deflate alternative ***********}
          {if (ContenCoding = 'deflate') or (ContenCoding = '') then
          begin
            CompressionSendStream := TCompressionStream.Create(clMax, SendStream);
            InStream.Seek(0, 0);
            GetMem(buffer, 655360);
            repeat
              FillChar(buffer^, 655360, ' ');
              readcount := InStream.Read(buffer^, 655360);
              if readcount > 0 then
                CompressionSendStream.Write(buffer^, 655360);
            until readcount < 655360;
            CompressionSendStream.Free;
            FreeMem(buffer);
          end;}

          LogDatei.log_prog(' JSON service request Furl ' + Furl, LLdebug);
          //LogDatei.log(' JSON service request str ' + utf8str, LLdebug);
          {$IFDEF SYNAPSE}
          LogDatei.log_prog('HTTPSender FURL: ' + Furl, LLDebug2);
          {if not HTTPSenderResult then
          begin
            LogDatei.log('HTTPSender Post failed', LLError);
            LogDatei.log('HTTPSender result: ' + IntToStr(HTTPSender.ResultCode) +
              ' msg: ' + HTTPSender.ResultString, LLError);
            ErrorOccured := True;
            raise Exception.Create(HTTPSender.Headers.Strings[0]);
          end
          else}
          if HTTPSender.HTTPMethod('POST', Furl) then
          begin
            { Communication successful }
            LogDatei.log_prog('HTTPSender Post succeeded', LLdebug);
            LogDatei.log_prog('HTTPSender result: ' + IntToStr(HTTPSender.ResultCode) +
              ' msg: ' + HTTPSender.ResultString, LLdebug);
            for i := 0 to HTTPSender.Headers.Count - 1 do
              LogDatei.log_prog('HTTPSender Response Header.Strings: ' +
                HTTPSender.Headers.Strings[i], LLDebug2);
            LogDatei.log_prog('got mimetype: ' + HTTPSender.MimeType, LLDebug2);
            //LogDatei.log('ReceiveStream: ' + MemoryStreamToString(ReceiveStream), LLDebug2);
            if HTTPSender.ResultString = 'OK' then
            begin
              { Log and set CommunicationMode}
              if FCommunicationMode = -1 then
              begin
                if CommunicationMode = 0 then
                begin
                  LogDatei.log_prog('Communication mode is ' +
                    IntToStr(CommunicationMode) + '. ' +
                    'Opsi Version should be >=4.1', LLDebug);
                end
                else
                begin
                  LogDatei.log_prog('Communication mode is ' +
                    IntToStr(CommunicationMode) + '. ' +
                    'Opsi Version should be 4.0', LLDebug);
                end;
                FCommunicationMode := CommunicationMode;
              end;
            end
            else
              { Communication unsuccessful }
            begin
              LogDatei.log('Communication unsucessful', LLError);
              LogDatei.log('HTTPSender result: ' + IntToStr(HTTPSender.ResultCode) +
                ' msg: ' + HTTPSender.ResultString, LLError);
              raise Exception.Create(HTTPSender.Headers.Strings[0]);
            end;

            if (HTTPSender.Headers.IndexOfName('Content-Encoding') <> -1) then
              // Content-Encoding header available
            begin
              ContentEncoding := trim(HTTPSender.Headers.Values['Content-Encoding']);
              LogDatei.log('Response Content-Encoding: ' +
                ContentEncoding, llDebug);
            end
            else // no Content-Encoding header available, due to HTTP specification: Content-Encoding = identity
            begin
              ContentEncoding := 'identity';
              LogDatei.log('No Content-Encoding header. Guess identity', llDebug);
            end;
            { identity = uncompressed}
            if (ContentEncoding = 'identity') then
            begin
              ReceiveStream.LoadFromStream(HTTPSender.Document);
            end
            else
            { defalte = zlib format}
            if (ContentEncoding = 'deflate') or (ContentEncoding = '') then
            begin
              DeCompressionReceiveStream :=
                TDeCompressionStream.Create(HTTPSender.Document);
              LogDatei.log_prog(
                'HTTPSender Post: created CompressionReceiveStream', LLInfo);
              GetMem(buffer, 655360);
              repeat
                FillChar(buffer^, 655360, ' ');
                readcount := DeCompressionReceiveStream.Read(buffer^, 655360);
                LogDatei.log_prog(
                  'HTTPSender Post: readed from CompressionReceiveStream: ' +
                  IntToStr(readcount), LLInfo);
                if readcount > 0 then
                  ReceiveStream.Write(buffer^, readcount);
                LogDatei.log_prog('HTTPSender Post: write to ReceiveStream: ' +
                  IntToStr(readcount), LLInfo);
              until readcount < 655360;
              LogDatei.log_prog(
                'HTTPSender Post: read CompressionReceiveStream finished',
                LLInfo);
              DeCompressionReceiveStream.Free;
              FreeMem(buffer);
            end
            else
            { gzip = gzip format}
            if ContentEncoding = 'gzip' then
            begin
              unzipStream(HTTPSender.Document, ReceiveStream);
            end
            else
              LogDatei.log('Unknown Content-Encoding: ' + ContentEncoding, LLDebug);
            //HTTPSender.Document.SaveToFile('C:\Users\Jan\Documents\ReceiveStream.txt');  //for testing
            LogDatei.log_prog('HTTPSender Post: got document', LLInfo);
            {$ELSE SYNAPSE}
            IdHTTP.post(FserviceURL + '/rpc', sendstream, ReceiveStream);
            {$ENDIF SYNAPSE}
          end
          else
          begin
            LogDatei.log('HTTPSender Post failed', LLError);
            LogDatei.log('HTTPSender result: ' + IntToStr(HTTPSender.ResultCode) +
              ' msg: ' + HTTPSender.ResultString, LLError);
            ErrorOccured := True;
            raise Exception.Create(HTTPSender.Headers.Strings[0]);
          end;
        end;
        (*
        else
        begin
          LogDatei.log_prog('Using MimeType: ' + ContentTypeNoCompress, LLDebug);
          {$IFDEF SYNAPSE}
          HTTPSender.Headers.Clear;
          HTTPSender.Headers.Add('accept: ' + AcceptNoCompress);
          HTTPSender.Headers.Add('accept-encoding: ' + AcceptEncodingNoCompress);
          HTTPSender.Headers.Add('content-encoding: ' + ContentEncodingNoCommpress);
          HTTPSender.Headers.Add('content-type: ' + ContentTypeNoCompress);
          for i := 0 to HTTPSender.Headers.Count - 1 do
            LogDatei.log_prog('HTTPSender Header.Strings: ' +
              HTTPSender.Headers.Strings[i], LLDebug2);
          HTTPSender.Document.LoadFromStream(sendstream);
          HTTPSenderResult := HTTPSender.HTTPMethod('POST', Furl);
          if HTTPSenderResult then
            LogDatei.log_prog('HTTPSender Post ok', LLDebug2)
          else
            LogDatei.log('HTTPSender Post failed', LLError);
          sendresultcode := HTTPSender.ResultCode;
          sendresultstring := HTTPSender.ResultString;
          LogDatei.log_prog('HTTPSender result: ' + IntToStr(sendresultcode) +
            ' msg: ' + sendresultstring, LLDebug2);
          if sendresultstring <> 'OK' then
          begin
            raise Exception.Create(HTTPSender.Headers.Strings[0]);
          end;
          mymemorystream := HTTPSender.Document;
          {$ELSE SYNAPSE}
          IdHttp.Request.ContentType := ContentTypeNoCompress;
          IdHttp.Request.ContentEncoding := ContentEncodingNoCommpress;
          IdHttp.Request.Accept := AcceptNoCompress;
          IdHttp.Request.AcceptEncoding := AcceptEncodingNoCompress;
          LogDatei.log('HTTP Header: ' + IdHttp.Request.RawHeaders.Text, LLDebug);
          IdHTTP.Post(FserviceURL + '/rpc', instream, mymemorystream);
          {$ENDIF SYNAPSE}
        end;
        *)
      except
        on E: Exception do
        begin
          LogDatei.log_prog(
            'Exception in retrieveJSONObjectByHttpPost: stream handling: ' + e.message
            , LLError);
          if HTTPSender.ResultCode = 400 then
          begin
            // we have a 400 Bad Request - perhaps opsi server with other communication mode
            LogDatei.log(
              'We had a 400 (bad request) result - so we retry with other parameters / communication compatibility modes',
              LLInfo);
            FCommunicationMode := -1;
            //-1 means CommunicationMode is not set e.g. it is unknown
            // retry with other parameters
            Inc(CommunicationMode);
            if (CommunicationMode <= 2) then
            begin
              LogDatei.log('Retry with communicationmode: ' + IntToStr(
                communicationmode), LLinfo);
              Result := retrieveJSONObjectByHttpPost(instream, logging,
                CommunicationMode);
            end;
          end;
        end;
          (*
          if ContentTypeCompress = 'application/json' then
          begin
            LogDatei.log('Use opsi 4.0 HTTP Header', LLnotice);
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
            LogDatei.log('Use opsi 4.1 / 4.2 HTTP Header', LLnotice);
            ContentTypeCompress := 'application/json';
            AcceptCompress := 'application/json';
            ContentTypeNoCompress := 'application/json';
            AcceptNoCompress := 'application/json';
            ContentEncodingNoCommpress := 'plain';
            ContentEncodingCommpress := 'deflate';
            AcceptEncodingCompress := 'deflate';
            AcceptEncodingNoCompress := 'plain';
          end;
          LogDatei.log_prog('Changing to MimeType: ' + ContentTypeCompress, LLDebug);
          sendstream.Free;

          try
            sendstream := TMemoryStream.Create;
            ReceiveStream := TMemoryStream.Create;
            sendstream.Clear;
            // we assume opsi4
            compress := True;
            LogDatei.log_prog('Using MimeType: ' + ContentTypeCompress, LLDebug);

            if compress then
            begin
              LogDatei.log_prog('Using MimeType: ' + ContentTypeCompress, LLDebug);
              {$IFDEF SYNAPSE}
              HTTPSender.Headers.Clear;
              HTTPSender.MimeType := ContentTypeCompress;
              HTTPSender.Headers.Add('accept: ' + AcceptCompress);
              HTTPSender.Headers.Add('accept-encoding: ' + AcceptEncodingCompress);
              HTTPSender.Headers.Add('content-encoding: ' + ContentEncodingCommpress);
              HTTPSender.Headers.Add('content-type: ' + ContentTypeCompress);
              for i := 0 to HTTPSender.Headers.Count - 1 do
                LogDatei.log_prog('HTTPSender Header.Strings: ' +
                  HTTPSender.Headers.Strings[i], LLDebug2);
              {$ELSE SYNAPSE}
              IdHttp.Request.ContentType := ContentTypeCompress;
              IdHttp.Request.ContentEncoding := ContentEncodingCommpress;
              IdHttp.Request.Accept := AcceptCompress;
              IdHttp.Request.AcceptEncoding := AcceptEncodingCompress;
              LogDatei.log_prog('HTTP Header: ' +
                IdHttp.Request.RawHeaders.Text, LLDebug);
              {$ENDIF SYNAPSE}
              CompressionSendStream := TCompressionStream.Create(clMax, sendstream);
              Instream.Seek(0, 0);
              GetMem(buffer, 655360);
              repeat
                FillChar(buffer^, 655360, ' ');
                readcount := Instream.Read(buffer^, 655360);
                if readcount > 0 then
                  CompressionSendStream.Write(buffer^, readcount);
              until readcount < 655360;
              CompressionSendStream.Free;
              {$IFDEF SYNAPSE}
              HTTPSender.Document.LoadFromStream(sendstream);
              //HTTPSender.Document.SaveToFile('c:\opsi.org\log\senddoc.txt');
              HTTPSenderResult := HTTPSender.HTTPMethod('POST', Furl);
              if HTTPSenderResult then
                LogDatei.log_prog('HTTPSender Post ok', LLDebug)
              else
                LogDatei.log('HTTPSender Post failed', LLError);
              sendresultcode := HTTPSender.ResultCode;
              sendresultstring := HTTPSender.ResultString;
              LogDatei.log_prog('HTTPSender result: ' + IntToStr(sendresultcode) +
                ' msg: ' + sendresultstring, LLDebug2);
              if sendresultstring <> 'OK' then
              begin
                raise Exception.Create(HTTPSender.Headers.Strings[0]);
              end;
              ReceiveStream := HTTPSender.Document;
              {$ELSE SYNAPSE}
              IdHTTP.post(FserviceURL + '/rpc', sendstream, ReceiveStream);
              {$ENDIF SYNAPSE}
              ReceiveStream.Seek(0, 0);
              mymemorystream.Seek(0, 0);
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
              LogDatei.log_prog('Using MimeType: ' + ContentTypeNoCompress, LLDebug);
              {$IFDEF SYNAPSE}
              HTTPSender.Headers.Clear;
              HTTPSender.MimeType := ContentTypeNoCompress;
              HTTPSender.Headers.Add('accept: ' + AcceptNoCompress);
              HTTPSender.Headers.Add('accept-encoding: ' + AcceptEncodingNoCompress);
              HTTPSender.Headers.Add('content-encoding: ' + ContentEncodingNoCommpress);
              HTTPSender.Headers.Add('content-type: ' + ContentTypeNoCompress);
              for i := 0 to HTTPSender.Headers.Count - 1 do
                LogDatei.log_prog('HTTPSender Header.Strings: ' +
                  HTTPSender.Headers.Strings[i], LLDebug2);
              HTTPSender.Document.LoadFromStream(sendstream);
              HTTPSenderResult := HTTPSender.HTTPMethod('POST', Furl);
              if HTTPSenderResult then
                LogDatei.log_prog('HTTPSender Post ok', LLDebug)
              else
                LogDatei.log('HTTPSender Post failed', LLError);
              sendresultcode := HTTPSender.ResultCode;
              sendresultstring := HTTPSender.ResultString;
              LogDatei.log_prog('HTTPSender result: ' + IntToStr(sendresultcode) +
                ' msg: ' + sendresultstring, LLDebug2);
              if sendresultstring <> 'OK' then
              begin
                raise Exception.Create(HTTPSender.Headers.Strings[0]);
              end;
              ReceiveStream := HTTPSender.Document;
              {$ELSE SYNAPSE}
              IdHttp.Request.ContentType := ContentTypeNoCompress;
              IdHttp.Request.ContentEncoding := ContentEncodingNoCommpress;
              IdHttp.Request.Accept := AcceptNoCompress;
              IdHttp.Request.AcceptEncoding := AcceptEncodingNoCompress;
              LogDatei.log_prog('HTTP Header: ' +
                IdHttp.Request.RawHeaders.Text, LLDebug);
              IdHTTP.Post(FserviceURL + '/rpc', instream, mymemorystream);
              {$ENDIF SYNAPSE}
            end;
          except
            on e: Exception do
            begin
              LogDatei.log_prog(
                'Exception in retrieveJSONObjectByHttpPost: stream handling: ' +
                e.message
                , LLError);
            end;
          end;
        end;   *)
        (************************)
      end;
      {$IFDEF SYNAPSE}
      testresult := HTTPSender.Headers.Text;
      cookieVal := HTTPSender.Cookies[0];
      {$ELSE SYNAPSE}
      testResult := IdHTTP.ResponseText;
      cookieVal := IdHTTP.Response.RawHeaders.Values['Set-Cookie'];
      {$ENDIF SYNAPSE}
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
      ReceiveStream.Position := 0;
      ResultLines.LoadFromStream(ReceiveStream);
      // should be one line  @Detlef ??? (Jan)
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
        //LogDatei.log (DateTimeToStr(now) + ' JSON result ' + testresult ,  LLInfo);
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
      LogDatei.log_prog('Error: --- opsi service problem ---' + FError, LLerror);
  LogDatei.log_prog('End of function retrieveJSONObjectByHttpPost, Line: ' +
    {$INCLUDE %LINE%}
    , LLDebug2);
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
    LogDatei.log_prog('retrieveJSONObject = nil in getMapResult', LLdebug);
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
begin
  Result := TStringList.Create;
  jO := retrieveJSONObject(omc);
  if jO = nil then
  begin
    exit;
  end;
  jO1 := jO.O['result'];
  jA := jO.O['result'].AsArray;
  if jA <> nil then
  begin
    for i := 0 to jA.Length - 1 do
    begin
      Result.append(jA.S[i]);
    end;
  end;
end;

function TJsonThroughHTTPS.getSubListResult(const omc: TOpsiMethodCall;
  subkey: string): TStringList;
var
  jO, jO1: ISuperObject;
  jA, jA1: TSuperArray;
  i: integer;
begin
  try
    Result := TStringList.Create;
    jO := retrieveJSONObject(omc);
    if jO = nil then
    begin
      LogDatei.log_prog('Got no JSON object from web service', LLWarning);
      Result.Text := 'Error';
      exit;
    end;
    // we have something like
    // {"result":[{subkey:["a","r","r","a","y"]}]}
    // get value of path 'result' as Array (which contains one object)
    if jsonAsObjectHasKey(jO.AsString, 'result') then
    begin
      jA := jO.A['result'];
      if jA.Length > 0 then
      begin
        // get this single object
        jO1 := jA.O[0];
        // get from this object the value for the key: subkey as array
        jA1 := jO1.A[subkey];
        if (jA1 <> nil) then
        begin
          if jA1.Length > 0 then
          begin
            for i := 0 to jA1.Length - 1 do
            begin
              Result.append(jA1.S[i]);
            end;
          end
          else
          begin
            LogDatei.log('getSubListResult: Key: "' + subkey + '"' +
              ' has empty value. Received object: ' + jO.AsString, LLDebug2);
            Result.Text := 'Empty value';
          end;
        end
        else
        begin
          LogDatei.log('Error in getSubListResult: Result has no key "' +
            subkey + '". Received object: ' + jO.AsString, LLError);
          Result.Text := 'Error';
        end;
      end
      else
      begin
        LogDatei.log('getSubListResult: JSON-Object has empty "result". Received object: '
          + jO.AsString, LLDebug2);
        Result.Text := 'Empty result';
      end;
    end
    else
    begin
      LogDatei.log(
        'Error in getSubListResult: JSON-Object has no key "result". Received object: ' +
        jO.AsString, LLError);
      Result.Text := 'Error';
    end;
  except
    on E: Exception do
    begin
      LogDatei.log_prog('Exception in getSubListResult, system message: "' +
        E.Message + '" Received object: ' + jO.AsString, LLError);
      Result.Text := 'Error';
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
  utf8str: string;//UTF8String;
begin
  try
    Result := False;
    mymemorystream.Clear;
    if FSessionId <> '' then
      {$IFDEF SYNAPSE}
      HTTPSender.Cookies.Add(FSessionId);
      {$ELSE SYNAPSE}
    IdHttp.Request.RawHeaders.Values['Cookie'] := FSessionId;
      {$ENDIF SYNAPSE}
    LogDatei.log_prog('Sessionid ' + FSessionId, LLdebug2);
    {$IFDEF SYNAPSE}
    testresult := HTTPSender.Headers.Text;
    {$ELSE SYNAPSE}
    testresult := IdHttp.Request.RawHeaders.Text;
    LogDatei.log_prog('HTTP Header: ' + IdHttp.Request.RawHeaders.Text, LLDebug);
    {$ENDIF SYNAPSE}
    if pos('/rpc', FserviceURL) = 0 then
      localurl := FserviceURL
    else
      localurl := copy(FserviceURL, 0, pos('/rpc', FserviceURL));
    utf8str := localurl + '/depot/' + filename;
    // AnsiToUtf8(localurl + '/depot/' + filename);
    LogDatei.log('Loading file: ' + utf8str, LLDebug2);
    {$IFDEF SYNAPSE}
    HTTPSender.Headers.Clear;
    HTTPSender.Headers.Add('content-type: text ,identity');
    HTTPSender.Headers.Add('accept-encoding: text ,identity');
    // TODO GET
    {$ELSE SYNAPSE}
    IdHttp.Request.ContentType := 'text';
    IdHttp.Request.Accept := 'text';
    IdHTTP.Get(utf8str, mymemorystream);
    {$ENDIF SYNAPSE}
    mymemorystream.Position := 0;
    //LogDatei.log('JSON retrieveJSONObject: memorystream reseted', LLDebug2);
    // ResultLines.LoadFromStream(mymemorystream);

    //resultstring := IdHTTP.Get(FserviceURL+'/'+filename);
    if toStringList then
      ListResult.LoadFromStream(mymemorystream);
    Result := True;
  except
    on E: Exception do
    begin
      Logdatei.log_prog('Exception in getFileFromDepot, system message: "' +
        E.Message + '"', LLError);
      Result := False;
    end
  end;
end;

(******************* End of TJsonThroughHTTPS *********************************)

(******************************************************************************)
(*                                                                            *)
(*         Implementation of class TOpsi4Data                                 *)
(*                                                                            *)
(******************************************************************************)
constructor TOpsi4Data.Create;
begin
  //inherited create;
  actualclient := '';
  FJsonExecutioner := nil;
  FSortByServer := False;
  FCommunicationMode := -1;
  //Set stringlists to nil, so it can be checked if they are nil
  //DoTo: It would be much better to create the stringlists here but it is not clear
  //if this might be problematic, thus a carefull check and larger refactoring might be necessary
  mapOfMethodSignatures := nil;
  allDependencies := nil;
  FPreRequirements := nil;
  FPostRequirements := nil;
  FProductStates := nil;
  FProductActionRequests := nil;
  FSortedProductIDsWhereActionIsSet := nil;
  FInstallableProducts := nil;
  ProductVars := nil;
  FProductOnClientIndex := nil;
  mylist := nil;


  {$IFNDEF SYNAPSE}
  //FSslProtocol := sslvTLSv1_2;
  {$ENDIF SYNAPSE}
end;

destructor TOpsi4Data.Destroy;
begin
  inherited Destroy;
end;

{$IFNDEF SYNAPSE}
function TOpsi4Data.decreaseSslProtocol: boolean;
begin
  try
    Result := True;
    FSslProtocol := TIdSSLVersion(Dec(FSslProtocol));
  except
    on E: Exception do
    begin
      Logdatei.log_prog('Exception in decreaseSslProtocol, system message: "' +
        E.Message + '"', LLError);
      Logdatei.log_prog('Exception in decreaseSslProtocol, last Protokoll: "' +
        GetEnumName(TypeInfo(FSslProtocol), integer(FSslProtocol)) + '"', LLError);
      Result := False;
    end
  end;
end;

{$ENDIF SYNAPSE}

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
  initOpsiConf(serviceURL, username, password, sessionid, ip, port, '');
end;

procedure TOpsi4Data.initOpsiConf(serviceURL, username, password,
  sessionid, ip, port, agentstring: string);
begin
  try
    if Assigned(FJsonExecutioner) then
      FreeAndNil(FJsonExecutioner);
    FjsonExecutioner := TJsonThroughHTTPS.Create(serviceUrl, username,
      password, sessionid, ip, port, agentstring);
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
      LogDatei.log_prog('Exception in initOpsiConf: ' + e.message, LLError);
    end;
  end;
end;


function TOpsi4Data.getOpsiModules: TStringList;
{
Juristischer Hinweis:
Dies ist Code zum Schutz der Begrenzung der Nutzung kostenpflichtiger opsi Erweiterungen.
Auch wenn dies Opensource Code (AGPLv3) ist, kann die Nutzung oder Verbreitung
von Veränderungen dieses Codes Illegal sein, insbesondere wenn diese
Veränderungen das Schutzziel schwächen oder sonst wie verletzen.
Es kann eine Verletzung der AGB's der Firma uib gmbh sein.
Darüberhinaus kann es eine strafbare Verletzung des Urheberrechts sein z.B.  des
"§ 95a Schutz technischer Maßnahmen" des deutschen
"Gesetz über Urheberrecht und verwandte Schutzrechte (Urheberrechtsgesetz)"

Legal Notice:
This code to protect the limits of using of opsi extensions with costs.
Even if this is open source code (AGPLv3), it may be illegal to use or
distribute modifications this code,
especially if these modifications compromise the protection goals.
It may be a violation of the General Terms and Conditions of the uib gmbh.
It may be also a punishable violation of the Copyright, so for example of the
"§ 95a Schutz technischer Maßnahmen"  of the german law:
"Gesetz über Urheberrecht und verwandte Schutzrechte (Urheberrechtsgesetz)"
}
var
  omc: TOpsiMethodCall;
  //teststring : string;
  //testbool : boolean;
  myModulesList: TStringList;
  myJsonModulesArray: ISuperObject;
  i: integer;
  str: string;
begin
  myModulesList := TStringList.Create;
  FOpsiInformation := nil;
  if FOpsiInformation = nil then
  begin
    omc := TOpsiMethodCall.Create('backend_getLicensingInfo', []);
    if omc <> nil then
    begin
      try
        FOpsiInformation := FjsonExecutioner.retrieveJsonObject(omc);
        if (FOpsiInformation <> nil) and (FOpsiInformation.O['result'] <>
          nil) and (FopsiInformation.O['result'].O['available_modules'] <> nil) then
        begin
          myJsonModulesArray := FopsiInformation.O['result'].O['available_modules'];
          for i := 0 to myJsonModulesArray.AsArray.Length - 1 do
          begin
            str := myJsonModulesArray.AsArray.S[i];
            myModulesList.Add(str);
          end;
          Result := myModulesList;
        end
        else
        begin
          //  backend_getLicensingInfo failed - retry with backend_info
          LogDatei.log(
            'Problem getting backend_getLicensingInfo from service - perhaps old server, we fallback to backend_info',
            LLwarning);
          FOpsiInformation := nil;
          if FOpsiInformation = nil then
          begin
            omc := TOpsiMethodCall.Create('backend_info', []);
            if omc <> nil then
            begin
              try
                FOpsiInformation := FjsonExecutioner.retrieveJsonObject(omc);
                if (FOpsiInformation <> nil) and
                  (FOpsiInformation.O['result'] <> nil) and
                  (FopsiInformation.O['result'].O['modules'] <> nil) then
                begin
                  FOpsiModules := FopsiInformation.O['result'].O['modules'];
                  if FOpsiModules <> nil then
                    if FOpsiModules.AsObject.GetNames.IsType(stArray) then
                    begin

                      for i := 0 to FOpsiModules.AsObject.GetNames.AsArray.Length - 1 do
                      begin
                        if FOpsiModules.AsObject.GetNames.AsArray.B[i] then
                        begin
                          str := FOpsiModules.AsObject.GetNames.AsArray.S[i];
                          myModulesList.Add(str);
                        end;
                        Result := myModulesList;
                      end;
                    end;
                end
                else
                  LogDatei.log('Problem getting backend_info from service', LLerror);
              except
                LogDatei.log_prog('Exeception getting backend_info from service',
                  LLerror);
              end;
            end
            else
              LogDatei.log_prog('Problem creating OpsiMethodCall backend_info', LLerror);
          end;
        end;
      except
        LogDatei.log_prog('Exeception backend_getLicensingInfocalling ', LLerror);
      end;
    end
    else
      LogDatei.log_prog('Problem creating OpsiMethodCall backend_getLicensingInfo',
        LLerror);
  end;



(*
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
          LogDatei.log_prog('Problem getting modules from service', LLerror);
      except
        LogDatei.log_prog('Exeception getting modules from service', LLerror);
      end;
    end
    else
      LogDatei.log_prog('Problem creating OpsiMethodCall backend_info', LLerror);
  end;
  //teststring := FOpsiInformation.AsJSon(true,true);
  //teststring := FOpsiInformation.O['result'].AsJSon(true,true);
  //teststring := FOpsiInformation.O['result'].O['modules'].AsJSon(true,true);
  //teststring := FOpsiModules.AsJSon(true,true);
  //teststring := FOpsiModules.S['license_management'];
  //testbool := FOpsiModules.B['license_management'];
  Result := FOpsiModules;
  *)
  omc.Free;
end;

function TOpsi4Data.getLicenseOnClientObject(const parameters: array of string; var errorOccured: boolean):string;
var
  omc: TOpsiMethodCall;
begin
  omc := TOpsiMethodCall.Create('licenseOnClient_getOrCreateObject', parameters);
  try
    Result := CheckAndRetrieveString(omc, errorOccured);
  finally
    FreeAndNil(omc);
  end;
end;

procedure TOpsi4Data.SetProductOnClientData(const ProductOnClientData: TStringList);
var
  ProductEntry: ISuperObject;
  i: integer; //count variable
begin
  try
    if not Assigned(FProductActionRequests) then
      FProductActionRequests := TStringList.Create
    else
      FProductActionRequests.Clear;
    if not Assigned(FProductStates) then
      FProductStates := TStringList.Create
    else
      FProductStates.Clear;
    If not Assigned(FSortedProductIDsWhereActionIsSet) then
      FSortedProductIDsWhereActionIsSet := TStringList.Create
    else
      FSortedProductIDsWhereActionIsSet.Clear;
    if (ProductOnClientData <> nil) then
    begin
      for i := 0 to ProductOnClientData.Count - 1 do
      begin
        ProductEntry := SO(ProductOnClientData.Strings[i]);
        LogDatei.log_prog('found ProductOnClient entry with productId: ' +
          ProductEntry.S['productId'] + ' with pos: ' + IntToStr(i) + ' / ' +
          IntToStr(ProductOnClientData.Count - 1), LLDebug2);
        //Set FProductActionRequests
        FProductActionRequests.add(ProductEntry.S['productId'] + '=' +
          ProductEntry.S['actionRequest']);
        LogDatei.log_prog('action entry : ' + ProductEntry.S['productId'] + '=' +
          ProductEntry.S['actionRequest'], LLDebug2);
        //Set FProductStates
        FProductStates.Add(ProductEntry.S['productId'] + '=' +
          ProductEntry.S['installationStatus']);
        LogDatei.log_prog('state entry : ' + ProductEntry.S['productId'] + '=' +
          productEntry.S['installationStatus'], LLDebug2);
        //Add Product ID to result list if action request is set for this product.
        if ProductEntry.S['actionRequest'] <> 'none' then
        begin
          FSortedProductIDsWhereActionIsSet.Add(ProductEntry.S['productId']);
          LogDatei.log_prog('FSortedProductIDsWhereActionIsSet:added Id : ' +
            ProductEntry.S['productId'], LLnotice);
        end;
      end;
    end;
  except
    on E: Exception do
      LogDatei.log('Exception in SetProductOnClientData: ' + E.Message, LLerror);
  end;
end;


function TOpsi4Data.withLicenceManagement: boolean;
var
  index: integer;
  mylist: TStringList;
begin
  try
    Result := False;
    mylist := getOpsiModules;
    if Assigned(mylist) then
    begin
      index := mylist.IndexOf('license_management');
      FreeAndNil(mylist);
    end;
    if index >= 0 then Result := True;
  except
    LogDatei.log(
      'licence management info not found (exception in withLicenceManagement)',
      LLWarning);
    Result := False;
  end;
end;

function TOpsi4Data.withRoamingProfiles: boolean;
var
  index: integer;
begin
  try
    Result := False;
    mylist := getOpsiModules;
    if Assigned(mylist) then
    begin
      index := mylist.IndexOf('roaming_profiles');
      FreeAndNil(mylist);
    end;
    if index >= 0 then Result := True;
  except
    LogDatei.log(
      'roaming_profiles info not found (exception in withRoamingProfiles)',
      LLWarning);
    Result := False;
  end;
end;

(*
var
  mymodules: ISuperObject;
begin
  try
    mymodules := getOpsiModules;
    LogDatei.log('modules found', LLDebug);
    if mymodules <> nil then
      Result := mymodules.B['roaming_profiles']
    else
    begin
      LogDatei.log('Roaming Profiles info not found (modules = nil)',
        LLWarning);
      Result := False;
    end;
  except
    LogDatei.log(
      'Roaming Profiles info not found (exception in withRoamingProfiles)', LLWarning);
    Result := False;
  end;
end;
*)

function TOpsi4Data.getOpsiVersion: string;
var
  omc: TOpsiMethodCall;
begin
  Result := '';
  try
    omc := TOpsiMethodCall.Create('backend_info', []);
    try
      try
        FOpsiInformation := FjsonExecutioner.retrieveJsonObject(omc);
      finally
        omc.Free;
      end;
      if (FOpsiInformation <> nil) and (FOpsiInformation.O['result'] <>
        nil) then
      begin
        Result := FOpsiInformation.O['result'].S['opsiVersion'];
        Result := AnsiDequotedStr(trim(Result), '"');
      end
      else
        LogDatei.log('Could not get backend_info from service. No json object retrieved', LLerror);
    except
      on E: Exception do
        LogDatei.log('Exception while getting backend_info from service: ' + E.Message, LLerror);
    end;
  except
    on E: Exception do
      LogDatei.log('Exception in getOpsiVersion: ' + E.Message, LLerror);
  end;
end;

function TOpsi4Data.getOpsiServiceMethods(allow_deprecated : boolean) : TStringlist;
var
  omc: TOpsiMethodCall;
  i : integer; //count variable
  OpsiServiceMethods : TSuperArray;
  isDeprecated : boolean = false;
begin
  Result := TStringlist.create;
  // method backend_getInterface exist since opsi 4.0.6 (or earlier)
  // but the key 'deprecated' exists since opsi 4.2
  try
    omc := TOpsiMethodCall.Create('backend_getInterface', []);
    try
      FOpsiInformation := FjsonExecutioner.retrieveJsonObject(omc);
    finally
      omc.Free;
    end;
    try
      if (FOpsiInformation <> nil) and (FOpsiInformation.O['result'] <>
          nil) then
        begin
          OpsiServiceMethods := FOpsiInformation.O['result'].AsArray;
          for i := 0 to OpsiServiceMethods.Length -1 do
          begin
            // do we have the deprecated key (opsi >= 4.2) ?
            if jsonAsObjectHasKey(OpsiServiceMethods[i].AsString,'deprecated') then
              isDeprecated := OpsiServiceMethods[i].B['deprecated']
            else isDeprecated := false; // default to false
            if (not isDeprecated) or allow_deprecated then
               result.Add(OpsiServiceMethods[i].S['name']);
          end;
        end
        else
          LogDatei.log('Could not get backend_getInterface from service. No json object or result retrieved', LLerror);
      except
        on E: Exception do
          LogDatei.log('Exception while getting backend_getInterface from service: ' + E.Message, LLerror);
      end;
  except
    on E: Exception do
      LogDatei.log('Exception in getOpsiServiceMethods: ' + E.Message, LLerror);
  end;
end;

function TOpsi4Data.isMethodProvided(Method : string): boolean;
var
  OpsiServiceMethods : TStringlist;
begin
  Result := False; //if method is not provided or an exception occurs the result is false
  try
    OpsiServiceMethods := getOpsiServiceMethods(True);
    try
      if OpsiServiceMethods.IndexOf(Method) > 0 then Result := True;
    finally
      FreeAndNil(OpsiServiceMethods);
    end;
  except
    on E: Exception do
      LogDatei.log('Exception in isMethodProvided: ' + E.Message, LLerror);
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
          LogDatei.log('Problem getting backend_info from service', LLerror);
      except
        LogDatei.log('Exeception getting backend_info from service', LLerror);
      end;
    end
    else
      LogDatei.log('Problem creating OpsiMethodCall backend_info', LLerror);
    omc.Free;
  end;
end;

function TOpsi4Data.isConnected2(loglist: TStringList): boolean;
var
  omc: TOpsiMethodCall;
begin
  Result := False;
  FOpsiInformation := nil;
  if FOpsiInformation = nil then
  begin
    try
      loglist.Append('Starting Servicecall: backend_info');
      omc := TOpsiMethodCall.Create('backend_info', []);
      if omc <> nil then
      begin
        try
          FOpsiInformation := FjsonExecutioner.retrieveJsonObject(omc);
          if (FOpsiInformation <> nil) and (FOpsiInformation.O['result'] <>
            nil) then
          begin
            Result := True;
            loglist.Append('Success Servicecall: backend_info');
          end
          else
            loglist.Append('Problem getting backend_info from service');
        except
          loglist.Append('Exeception getting backend_info from service');
        end;
      end
      else
        loglist.Append('Problem creating OpsiMethodCall backend_info');

    except
      on e: Exception do
      begin
        loglist.Append('Exception in isConnected2: ' + e.message +
          ' ' + DateTimeToStr(Now));
        Result := False;
      end;
    end;
    if Assigned(omc) then
      omc.Free;
  end;
end;

function TOpsi4Data.linuxAgentActivated: boolean;
var
  index: integer;
begin
  try
    Result := False;
    mylist := getOpsiModules;
    if Assigned(mylist) then
    begin
      index := mylist.IndexOf('linux_agent');
      FreeAndNil(mylist);
    end;
    if index >= 0 then Result := True;
  except
    LogDatei.log(
      'linux_agent info not found (exception in linuxAgentActivated)',
      LLWarning);
    Result := False;
  end;
end;

(*
var
  mymodules: ISuperObject;
begin
  try
    mymodules := getOpsiModules;
    LogDatei.log('modules found', LLDebug);
    if mymodules <> nil then
      Result := mymodules.B['linux_agent']
    else
    begin
      LogDatei.log('linux_agent info not found (modules = nil)',
        LLWarning);
      Result := False;
    end;
  except
    LogDatei.log(
      'linux_agent info not found (exception in linuxAgentActivated)', LLWarning);
    Result := False;
  end;
end;
*)

function TOpsi4Data.macosAgentActivated: boolean;
var
  index: integer;
begin
  try
    Result := False;
    mylist := getOpsiModules;
    if Assigned(mylist) then
    begin
      index := mylist.IndexOf('macos_agent');
      FreeAndNil(mylist);
    end;
    if index >= 0 then Result := True;
  except
    LogDatei.log(
      'macos_agent info not found (exception in macosAgentActivated)',
      LLWarning);
    Result := False;
  end;
end;

(*
var
  mymodules: ISuperObject;
begin
  try
    mymodules := getOpsiModules;
    LogDatei.log('modules found', LLDebug);
    if mymodules <> nil then
      Result := mymodules.B['macos_agent']
    else
    begin
      LogDatei.log('macos_agent info not found (modules = nil)',
        LLWarning);
      Result := False;
    end;
  except
    LogDatei.log(
      'macos_agent info not found (exception in macosAgentActivated)', LLWarning);
    Result := False;
  end;
end;
*)

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
  if FCommunicationMode > -1 then
  begin
    if FJsonExecutioner.retrieveJSONObject(omc, True, True, False,
      FCommunicationMode) <> nil then
      Result := FjsonExecutioner.resultLines[0]
    else
    begin
      errorOccured := True;
      Result := FjsonExecutioner.lastError;
    end;
  end
  else
  begin
    if FJsonExecutioner.retrieveJSONObject(omc) <> nil then
      Result := FjsonExecutioner.resultLines[0]
    else
    begin
      errorOccured := True;
      Result := FjsonExecutioner.lastError;
    end;
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
  omcresult: TStringList;
begin
  Result := True;
  FProductOnClient_aktobject := nil;
  omc := TOpsiMethodCall.Create('getProduct_hash', [actualProduct, FDepotId]);
  omcresult := FjsonExecutioner.getMapResult(omc);
  omc.Free;
  if omcresult = nil then
  begin
    LogDatei.log('Error: Opsi4Data.initProduct: getProduct_hash failed: retry',
      LLWarning);
    Sleep(500);
    omc := TOpsiMethodCall.Create('getProduct_hash', [actualProduct, FDepotId]);
    omcresult := FJsonExecutioner.getMapResult(omc);
    omc.Free;
  end;
  if omcresult = nil then
  begin
    ProductVars := TStringList.Create;
    Result := False;
    LogDatei.log('Opsi4Data.initProduct: getProduct_hash failed: giving up',
      LLError);
  end
  else
    ProductVars := omcresult;
  if ProductVars.Count > 0 then
  begin
    LogDatei.log('in TOpsi4Data.initProduct : ' +
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
        FProductOnClient_aktobject := SO(str);
      end
      else
        {$IFDEF OPSISCRIPT}
      if runLoginScripts then
        {$ELSE}
        if False then
        {$ENDIF}
          LogDatei.log('no productOnClient found for client: ' +
            actualClient + ' and product: ' + Productvars.Values['productId'], LLInfo)
        else
          LogDatei.log('no productOnClient found for client: ' +
            actualClient + ' and product: ' + Productvars.Values['productId'], LLerror)
      else
        LogDatei.log('no productOnClient found for depot: ' +
          actualClient + ' and product: ' + Productvars.Values['productId'], LLerror);
  end
  else
    LogDatei.log('no product_hash found for client: ' + FDepotId +
      ' and product: ' + actualProduct, LLerror);
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
  {$IFDEF OPSISCRIPT}
  if runLoginScripts then
  {$ELSE}
    if False then
  {$ENDIF}
      sendtype := 'userlogin'
    else
      sendtype := 'instlog';

  {$IFDEF OPSISCRIPT}
  if runproductlist then
    Logdatei.Appendmode := True;
  {$ENDIF OPSISCRIPT}

  if sendToLogFlag then
  begin
    {$IFDEF GUI}
    FBatchOberflaeche.SetMessageText(rsSendLog, mInfo);//setInfoLabel(rsSendLog);
    ProcessMess;
    Application.ProcessMessages;
    {$ENDIF}
    if LogDatei.Appendmode then
      sendLog(sendtype, True)
    else
      sendLog(sendtype);
  end;
  try
    { close the session after all is done  }
    omc := TOpsiMethodCall.Create('backend_exit', []);
    jsonEntry := FjsonExecutioner.retrieveJsonObject(omc);
    LogDatei.log('in finishOpsiConf: backend_exit done', LLDebug2);
  except
    on e: Exception do
    begin
      LogDatei.log('exception in finishOpsiConf: backend_exit ' + e.message, LLError);
    end;
  end;
  try
    { free some objects  }
    if omc <> nil then
      FreeAndNil(omc);
  except
    on e: Exception do
    begin
      LogDatei.log('exception in finishOpsiConf: free omc ' + e.message, LLError);
    end;
  end;
  { free FJsonExecutioner gives a Access Violation }
  (*
  try
    { free some objects  }
    if Assigned(FJsonExecutioner) then
      FreeAndNil(FJsonExecutioner);
  except
    on e: Exception do
    begin
      LogDatei.log('exception in finishOpsiConf: free FJsonExecutioner ' + e.message, LLError);
    end;
  end;
  *)
end;

function TOpsi4Data.getLogSize: int64;
var
  omc: TOpsiMethodCall;
  str: string;
  jo: ISuperObject;
begin
  Result := -1;
  FOpsiInformation := nil;
  if FOpsiInformation = nil then
  begin
    try
      omc := TOpsiMethodCall.Create('backend_getSystemConfiguration', []);
      if omc <> nil then
      begin
        try
          FOpsiInformation := FjsonExecutioner.retrieveJsonObject(omc);
          if (FOpsiInformation <> nil) and (FOpsiInformation.O['result'] <>
            nil) then
          begin
            try
              jo := FOpsiInformation.O['result'];
              jo := jo.O['log'];
              Result := jo.I['size_limit'];

            except
              on e: Exception do
              begin
                LogDatei.log('Exception in opsi4data.getLogSize: result: ' +
                  FOpsiInformation.S['result'] + ';  Error: ' + e.message, LLError);
              end;
            end;
          end
          else
            LogDatei.log('Problem getting backend_getSystemConfiguration from service',
              LLerror);
        except
          LogDatei.log('Exeception getting backend_getSystemConfiguration from service',
            LLerror);
        end;
      end
      else
        LogDatei.log('Problem creating OpsiMethodCall backend_getSystemConfiguration',
          LLerror);
    finally
      omc.Free;
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
  s, t, t2: string;
  found: boolean;
  errorinfo: string;
  Count: longint;
  maxlogsizebyte: int64;
  aktlogsize: int64;
  //scan: TUTF8Scanner;
  //ch: UCS4Char;

begin
  t := '';
  Result := True;
  { 5 MB }
  maxlogsizebyte := 5242880;

  { try to ask for actual maxlogsizebytes at the server }
  aktlogsize := getLogsize;
  if aktlogsize = -1 then
  begin
    Logdatei.log('Failed to get max log file size from server - using default of 5 MB',
      LLnotice);
    aktlogsize := maxlogsizebyte;
  end;
  { byte to MB }
  aktlogsize := aktlogsize div (1024 * 1024);

  Logdatei.log('Checking if partlog: is bigger than ' + IntToStr(aktlogsize) +
    ' MB :.', LLInfo);
  if Logdatei.PartbiggerthanMB(aktlogsize) then
  begin
    Logdatei.log('Shrinking Logfile to ' + IntToStr(aktlogsize) + ' MB....', LLInfo);
    Logdatei.PartShrinkToMB(aktlogsize);
    Logdatei.log('Shrinking Logfile to ' + IntToStr(aktlogsize) +
      ' MB finidhed.', LLInfo);
  end;
  Logdatei.setLogSIndentLevel(0);
  Logdatei.log(
    '-------- submitted part of log file ends here, see the rest of log file on client ----------',
    LLNotice);
  Logdatei.PartOpenForReading;
  try
    //LogDatei.log('->3',LLInfo);
    logstream := TMemoryStream.Create;
    //LogDatei.log('->4',LLInfo);
    logstream.Clear;
    //LogDatei.log('->5',LLInfo);
    //s := '{"method":"writeLog","params":["' + logtype + '","';
    s := '{"method":"log_write","params":["' + logtype + '","';
    //LogDatei.log('->6',LLInfo);
    //UTF8FixBroken(s);
    logstream.Write(s[1], length(s));
    //LogDatei.log('->7',LLInfo);
    s := '\n';
    LogDatei.log('start reading read file ...', LLInfo);
    Found := Logdatei.getPartLine(t);
    while found do
    begin
      Logdatei.log('read line from read file ...', LLDebug2);
      //Logdatei.log('line1: '+t, LLDebug2);
      //UTF8FixBroken(s);
      logstream.Write(s[1], 2);
      t := escapeControlChars(t);
      //Logdatei.log('line2: '+t, LLDebug2);
      try
        t2 := t;
        if t2 <> '' then
        begin
          if FindInvalidUTF8Codepoint(PChar(t2), length(t2)) > -1 then
          begin
            { utf8fixbroken may freeze do 06.10.2020 }
            //UTF8FixBroken(t2);
            t := 'log line contains non UTF8 chars';
          end
          else
            t := t2;
        end;
        { Utf8EscapeControlChars calls utf8fixbroken see above }
        // t := Utf8EscapeControlChars(t2);
        //Logdatei.log('line3: '+t, LLDebug2);
      except
        on E: Exception do
        begin
          LogDatei.log('oswebservice: UTF8FixBroken: "' + E.Message + '"',
            LLError);
          t := 'log line contains non fixable non UTF8 chars';
        end
      end;
      //{$IFDEF WINDOWS}
      //utf8str := AnsiToUtf8(t);
      //logstream.Write(utf8str[1], length(utf8str));
      //{$ELSE WINDOWS}
      //Logdatei.log('line4: '+t, LLDebug2);
      logstream.Write(t[1], length(t));
      //{$ENDIF WINDOWS}
      //utf8str := lconvencoding.ConvertEncoding(t,lconvencoding.GetDefaultTextEncoding,'utf8');

      Logdatei.log('wrote line: ' + t, LLDebug2);
      //logstream.write(t[1],length(t));
      found := Logdatei.getPartLine(t);
    end;
    if appendmode then
    begin
      s := '", "' + actualClient + '", true], "id": 1}';
    end
    else
    begin
      s := '", "' + actualClient + '", false], "id": 1}';
    end;
    Logdatei.log('write line: >' + s + '<  to service...', LLInfo);
    //UTF8FixBroken(s);
    logstream.Write(s[1], length(s));
  except
    on E: Exception do
    begin
      LogDatei.log('oswebservice: sendLog: "' + E.Message + '"',
        LLError);
    end
  end;
  LogDatei.log('start sending read file ...', LLInfo);
  try
    Result := (FJsonExecutioner.retrieveJSONObjectByHttpPost(logstream,
      False, 0) <> nil);
    { we should perhaps not log inside this because of circularity }
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
    LogDatei.log('Error: Opsi4Data.getProductproperties failed: retry',
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
    LogDatei.log('Error: Opsi4Data.getProductproperties failed: giving up',
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

procedure TOpsi4Data.reverseProductOrderByUninstall(
  var MapOfProductStates: TStringList);
var
  i: integer;
begin
  for i := 0 to MapOfProductStates.Count - 1 do
  begin
    if LowerCase(MapOfProductStates.ValueFromIndex[i]) = 'uninstall' then
      MapOfProductStates.Move(i, 0);
  end;
end;

function TOpsi4Data.GetSortedProductOnClientListFromService: TStringList;
var
  omc: TOpsiMethodCall;
begin
  Result := nil;
  // use opsi 4.3 method
  try
    omc := TOpsiMethodCall.Create('productOnClient_getObjectsWithSequence',
      ['', '{"clientId": "' + actualClient + '", "productType": "LocalbootProduct"}']);
    try
      Result := FjsonExecutioner.getListResult(omc);
    finally
      omc.Free;
    end;
  except
    on E: Exception do
      LogDatei.log('Exception in GetSortedProductOnClientListFromService: ' + E.Message, LLerror);
  end;
end;


function TOpsi4Data.getListOfProductIDs: TStringList;
var
  productmaps: TStringList;
  i : integer; //count variable
  SortedProductOnClientList: TStringList;
begin
  Result := nil;
  LogDatei.log('getListOfProducts:getOpsiVersion : ' + getOpsiVersion, LLDebug);
  // is opsi 4.3 method for product sequence provided ?
  if isMethodProvided('productOnClient_getObjectsWithSequence') then
  begin
    try
      SortedProductOnClientList := GetSortedProductOnClientListFromService;
      try
        SetProductOnClientData(SortedProductOnClientList);
      finally
        FreeAndNil(SortedProductOnClientList);
      end;
      Result := FSortedProductIDsWhereActionIsSet;
      FSortByServer := True;
    except
      on E: Exception do
      LogDatei.log('Exception in getListOfProdcutIDs using ops 4.3 method productOnClient_getObjectsWithSequence: ' +
        E.Message, LLerror);
    end;
  end
  else // no 4.3 method - let us use old stuff
  begin
    Result := TStringList.Create;
    if not Assigned(FProductActionRequests) then
      FProductActionRequests := TStringList.Create
    else
      FProductActionRequests.Clear;
    try
      productmaps := getMapOfProductActionRequests;
      try
        {$IFDEF OPSISCRIPT}
        if configReverseProductOrderByUninstall then
          reverseProductOrderByUninstall(productmaps);
        {$ENDIF OPSISCRIPT}
        FProductActionRequests.AddStrings(productmaps);
        for i := 0 to productmaps.Count - 1 do
        begin
          Result.add(productmaps.Names[i]);
          LogDatei.log('ProductID : ' + productmaps.Names[i], LLDebug2);
        end;
        FSortByServer := True;
        LogDatei.log('Product sequence calculated by opsi-server', LLDebug2);
      finally
        FreeAndNil(productmaps);
      end;
    except
      on E: Exception do
      begin
        LogDatei.log('Exception in getListOfProdcutIDs: ' + E.Message
          + ' Product sequence calculated by opsi-script', LLWarning);
        FSortByServer := False;
      end;
    end;
  end;
end;

function TOpsi4Data.getProductPropertyList(myproperty: string;
  defaultlist: TStringList; var usedefault: boolean): TStringList;
var
  clientId, values: string;
begin
  try
    usedefault := False;
    Result := TStringList.Create;
    if Productvars <> nil then
    begin
      clientid := actualclient;
      values := Productvars.Values['productId'];
      Result.AddStrings(TStrings(getProductPropertyList(myproperty,
        defaultlist, clientid, values, usedefault)));
    end
    else
    begin
      LogDatei.log('opsi.data.productvars=nil - using defaults.', LLWarning);
      Result.AddStrings(TStrings(defaultlist));
      usedefault := True;
    end;

  except
    on E: Exception do
    begin
      Logdatei.log('Exception in getProductPropertyList(1,2), system message: "' +
        E.Message + '"', LLwarning);
      LogDatei.log(' - using defaults.', LLWarning);
      Result.AddStrings(defaultlist);
      usedefault := True;
    end
  end;
end;

function TOpsi4Data.getProductPropertyList(myproperty: string;
  defaultlist: TStringList; myClientId: string; myProductId: string;
  var usedefault: boolean): TStringList;
var
  omc: TOpsiMethodCall;
begin
  usedefault := False;
  omc := TOpsiMethodCall.Create('productPropertyState_getObjects',
    ['', '{"objectId": "' + myClientId + '", "propertyId": "' +
    myproperty + '", "productId": "' + myProductId + '"}']);
  try
    try
      LogDatei.log('Try to get PRODUCT PROPERTY STATE object for CLIENT(' + myClientId + ') (productPropertyState_getObjects)', LLDebug);
      Result := FjsonExecutioner.getSubListResult(omc, 'values');

      //result is probably empty because productPropertyState_getObjects returns
      //an empty value if the property has the depot or server default value
      if (Result[0] = 'Empty result') or (Result[0] = 'Error')  then
      begin
        LogDatei.log('Got no PRODUCT PROPERTY STATE object for CLIENT(' + myClientId + ') from service (productPropertyState_getObjects)', LLDebug);
        if assigned(omc) then omc.Free;
        //get depot default value
        LogDatei.log('Try to get PRODUCT PROPERTY STATE object for DEPOT(' + DepotId + ') (productPropertyState_getObjects)', LLDebug);
        omc := TOpsiMethodCall.Create('productPropertyState_getObjects',
          ['', '{"objectId": "' + DepotId + '", "propertyId": "' +
          myproperty + '", "productId": "' + myProductId + '"}']);
        Result := FjsonExecutioner.getSubListResult(omc, 'values');

        //result is probably empty because productPropertyState_getObjects returns
        //an empty value if the property has the server default value
        if (Result[0] = 'Empty result') or (Result[0] = 'Error')  then
        begin
          LogDatei.log('Got no PRODUCT PROPERTY STATE object for DEPOT(' + DepotId + ') from service (productPropertyState_getObjects)', LLDebug);
          if assigned(omc) then omc.Free;
          //get server default value
          LogDatei.log('Try to get PRODUCT PROPERTY object (SERVER defaults) from service (productProperty_getObjects)', LLDebug);
          omc := TOpsiMethodCall.Create('productProperty_getObjects',
          ['', '{"propertyId": "' + myproperty + '", "productId": "' + myProductId + '"}']);
          Result := FjsonExecutioner.getSubListResult(omc, 'defaultValues');
        end;
      end;


      if (Result[0] = 'Empty value') then
      begin
        Result.Text := '';
        LogDatei.log('Got empty property value from service', LLWarning);
      end
      else
      begin
        if (Result[0] = 'Empty result') or (Result[0] = 'Error') then
        begin
          LogDatei.log('Got no property from service - using default',
            LLWarning);
          Result.AddStrings(TStrings(defaultlist));
          usedefault := True;
        end;
      end;
    except
      on E: Exception do
      begin
        Logdatei.log('Exception in getProductPropertyList, system message: "' +
          E.Message + '"', LLwarning);
        LogDatei.log(' - using defaults.', LLWarning);
        Result.AddStrings(TStrings(defaultlist));
        usedefault := True;
      end
    end;
  finally
    if Assigned(omc)then FreeAndNil(omc);
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
var
  resultlist: TStringList;
  omc: TOpsiMethodCall;
  productEntry: ISuperObject;
  jo, new_obj: ISuperObject;
  sort_array: TSuperArray;
  sortlist: TStringList;
  i, k: integer;
  found: boolean;
begin
  Result := TStringList.Create;
  try
    logdatei.log_prog('Starting sorting POC ', LLinfo);
    // get sorted productId list
    omc := TOpsiMethodCall.Create('getProductOrdering', [FDepotId]);
    jo := FjsonExecutioner.retrieveJSONObject(omc);
    new_obj := jo.O['result'];
    sort_array := new_obj.A['sorted'];
    sortlist := TStringList.Create;

    for i := 0 to sort_array.Length - 1 do
    begin
      sortlist.Append(sort_array.S[i]);
      logdatei.log_prog('Sorted productId: ' + sortlist.Strings[i] +
        ' at pos: ' + IntToStr(i), LLDebug2);
    end;
    // get unsorted poc list
    omc := TOpsiMethodCall.Create('productOnClient_getObjects',
      ['', '{"clientId": "' + actualClient + '", "productType": "LocalbootProduct"}']);

    resultList := FjsonExecutioner.getListResult(omc);

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
          Result.add(productEntry.S['productId'] + '=' +
            productEntry.S['actionRequest']);
          FProductStates.Add(productEntry.S['productId'] + '=' +
            productEntry.S['installationStatus']);
          LogDatei.log_prog('action entry : ' + productEntry.S['productId'] +
            '=' + productEntry.S['actionRequest'], LLDebug2);
          LogDatei.log_prog('state entry : ' + productEntry.S['productId'] +
            '=' + productEntry.S['installationStatus'], LLDebug2);
          logdatei.log_prog('found POC entry with productId: ' +
            sortlist.Strings[i] + ' with pos: ' + IntToStr(i) + ' / ' +
            IntToStr(k - 1), LLDebug2);
        end
        else
          logdatei.log_prog('No POC entry with productId: ' +
            sortlist.Strings[i], LLDebug2);
      end;
      logdatei.log_prog('Finished sorting POC  ', LLinfo);
    end;

  except
    logdatei.log('Failed fetching sorted POC list !', LLError);
    logdatei.log('No correct calculated installation sequence !', LLError);
    logdatei.log('Starting fetching unsorted POC list', LLinfo);

    // switch on old (wrong) product sorting
    omc := TOpsiMethodCall.Create('backend_setOptions',
      ['{"processProductOnClientSequence": true}']);

    productEntry := FjsonExecutioner.retrieveJSONObject(omc);
    omc := TOpsiMethodCall.Create('productOnClient_getObjects',
      ['', '{"actionRequest": ["setup", "uninstall", "custom", "always", "update"], "clientId": "'
      + actualClient + '", "productType": "LocalbootProduct"}']);
    resultList := FjsonExecutioner.getListResult(omc);
    for i := 0 to resultList.Count - 1 do
    begin
      productEntry := SO(resultlist.Strings[i]);
      if (productEntry.O['productId'] <> nil) then
      begin
        Result.add(productEntry.S['productId'] + '=' + productEntry.S['actionRequest']);
        FProductStates.Add(productEntry.S['productId'] + '=' +
          productEntry.S['installationStatus']);
        LogDatei.log_prog('action entry : ' + productEntry.S['productId'] +
          '=' + productEntry.S['actionRequest'], LLDebug);
        LogDatei.log_prog('state entry : ' + productEntry.S['productId'] +
          '=' + productEntry.S['installationStatus'], LLDebug);
      end;
      testresult := Result.Text;
    end;

    // switch off product sorting again
    omc := TOpsiMethodCall.Create('backend_setOptions',
      ['{"processProductOnClientSequence": false}']);
    productEntry := FjsonExecutioner.retrieveJSONObject(omc);

    logdatei.log_prog('Finished fetching unsorted POC list', LLinfo);
    omc.Free;
  end;
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
      //LogDatei.log ('productEntry : '+productEntry.AsString, LLessential);
      //LogDatei.log ('product : '+productEntry.S['id']+'='+productEntry.S['userLoginScript'], LLessential);
      if (productEntry.O['id'] <> nil) and
        // do we have a login script
        (productEntry.S['userLoginScript'] <> '') and
        //(productondepotmap.Values[productEntry.S['id']] <> '') and
        // do we have the same productId,productVersion, packageVersion  as on the depot
        (productondepotmap.Values[productEntry.S['id']] =
        productEntry.S['productVersion'] + '-' +
        productEntry.S['packageVersion']) and
        // do we have still an entry here ?
        (loginscriptmap.Values[productEntry.S['id']] = '') then
      begin
        loginscriptmap.add(productEntry.S['id'] + '=' +
          productEntry.S['userLoginScript']);
        LogDatei.log_prog('product : ' + productEntry.S['id'] +
          '=' + productEntry.S['userLoginScript'], LLInfo);
        if allscripts then
        begin
          Result.add(productEntry.S['id']);
          FProductStates.Add(productEntry.S['id'] + '=none');
        end;
      end;
    end;

  if not allscripts then
  begin
    { get productOnClient for actual Client and all localboot products}
    omc := TOpsiMethodCall.Create('productOnClient_getObjects',
      ['', '{"clientId": "' + actualClient + '", "productType": "LocalbootProduct"}']);
    resultList := FjsonExecutioner.getListResult(omc);
    omc.Free;

    if resultList <> nil then
      for i := 0 to resultList.Count - 1 do
      begin
        productEntry := SO(resultlist.Strings[i]);

        { changed 15.1.2019 do}
        { we want to run the login script if installed ......}
        if ((productEntry.O['productId'] <> nil) and
          (productEntry.S['installationStatus'] = 'installed'))
          { or last successful action was uninstall } or
          ((productEntry.S['actionResult'] = 'successful') and
          (productEntry.S['lastAction'] = 'uninstall')) then
        begin
          if loginscriptmap.Values[productEntry.S['productId']] <> '' then
          begin
            { product loginsript should run, so let us read the states }
            Result.add(productEntry.S['productId']);
            FProductStates.Add(productEntry.S['productId'] + '=' +
              productEntry.S['installationStatus']);
            LogDatei.log_prog(productEntry.S['productId'] +
              '= lastAction: ' + productEntry.S['lastAction'], LLNotice);
          end;
        end;
      end;
  end;
  FreeAndNil(productondepotmap);
  FreeAndNil(loginscriptmap);
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
    testresult := objectlist.Strings[i];
    testresult := SO('"' + testresult + '"').AsJSon(False, False);
    Result.add(testresult);
  end;
end;

function TOpsi4Data.getProductIds: TStringList;
var
  objectlist: TStringList;
  omc: TOpsiMethodCall;
  i: integer;
begin
  omc := TOpsiMethodCall.Create('getProductIds_list', []);
  objectlist := FjsonExecutioner.getListResult(omc);
  omc.Free;

  Result := TStringList.Create;

  for i := 0 to objectlist.Count - 1 do
  begin
    testresult := objectlist.Strings[i];
    testresult := SO('"' + testresult + '"').AsJSon(False, False);
    Result.add(testresult);
  end;
end;

function TOpsi4Data.getLocalbootProductIds: TStringList;
var
  objectlist: TStringList;
  omc: TOpsiMethodCall;
  i: integer;
begin
  omc := TOpsiMethodCall.Create('getProductIds_list', ['localboot']);
  objectlist := FjsonExecutioner.getListResult(omc);
  omc.Free;

  Result := TStringList.Create;

  for i := 0 to objectlist.Count - 1 do
  begin
    testresult := objectlist.Strings[i];
    testresult := SO('"' + testresult + '"').AsJSon(False, False);
    Result.add(testresult);
  end;
end;

function TOpsi4Data.getNetbootProductIds: TStringList;
var
  objectlist: TStringList;
  omc: TOpsiMethodCall;
  i: integer;
begin
  omc := TOpsiMethodCall.Create('getProductIds_list', ['netboot']);
  objectlist := FjsonExecutioner.getListResult(omc);
  omc.Free;

  Result := TStringList.Create;

  for i := 0 to objectlist.Count - 1 do
  begin
    testresult := objectlist.Strings[i];
    testresult := SO('"' + testresult + '"').AsJSon(False, False);
    Result.add(testresult);
  end;
end;

function TOpsi4Data.demandLicenseKey(const parameters: array of string;
  var errorOccured: boolean): string;
var
  JSONString: string;
begin
  JSONString := getLicenseOnClientObject(parameters, errorOccured);
  jsonAsObjectGetValueByKey(JSONString, 'licenseKey', Result);
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
  //LogDatei.log('in TOpsi4Data.getProductScriptPath ', LLessential);
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
    LogDatei.log(
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

procedure TOpsi4Data.setProductActionRequestWithDependencies(newAction: TActionRequest);
var
  omc: TOpsiMethodCall;
  jO: ISuperObject;
  actionRequest: string;

begin
  //if FInstallableProducts.IndexOf(actualProduct) = -1 then exit;

  actionRequest := actionRequestToString(newAction);
  omc := TOpsiMethodCall.Create('setProductActionRequestWithDependencies',
    [actualProduct, actualClient, actionRequest]);

  jO := FjsonExecutioner.retrieveJSONObject(omc);
  omc.Free;
  if FProductActionRequests = nil then
    FProductActionRequests := TStringList.Create;
  FProductActionRequests.Values[actualProduct] := actionRequest;
end;


procedure TOpsi4Data.setProductActionRequest(newAction: TActionRequest);
var
  omc: TOpsiMethodCall;
  jO: ISuperObject;
  actionRequest: string;

begin
  //if FInstallableProducts.IndexOf(actualProduct) = -1 then exit;

  actionRequest := actionRequestToString(newAction);
  omc := TOpsiMethodCall.Create('setProductActionRequest',
    [actualProduct, actualClient, actionRequest]);

  jO := FjsonExecutioner.retrieveJSONObject(omc);
  omc.Free;
  if FProductActionRequests = nil then
    FProductActionRequests := TStringList.Create;
  FProductActionRequests.Values[actualProduct] := actionRequest;
end;

procedure TOpsi4Data.setActionProgress(const progress: string);
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
    LogDatei.log('Exception in opsi4data.setProductState , parastr: ' +
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
    LogDatei.log('Exception in opsi4data.setProductState , parastr: ' +
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

begin
  //if FInstallableProducts.IndexOf(actualProduct) = -1 then exit;
  try
    actionResultS := actionResultToString(actionResult);
    actionRequestS := actionRequest4toString(actionRequest);
    targetConfigurationS := targetConfigurationToString(targetConfiguration);
    lastActionS := actionRequest4toString(lastAction);
    installationStatusS := installationStatusToString(installationStatus);

    FProductOnClient_aktobject.S['actionProgress'] := actionProgressS;
    FProductOnClient_aktobject.S['actionResult'] := actionResultS;
    FProductOnClient_aktobject.S['actionRequest'] := actionRequestS;
    if lastAction <> tac4Custom then
      FProductOnClient_aktobject.S['targetConfiguration'] := targetConfigurationS;
    parastr := FProductOnClient_aktobject.asJson(False, False);
    if lastAction <> tac4Custom then
    begin
      if (Productvars <> nil) and (Productvars.values['productVersion'] <> '') then
      begin
        if Assigned(LogDatei) then
           LogDatei.log('productVersion: ' + Productvars.Values['productVersion'] + ' (TOPsi4Data.ProductOnClient_update)', LLDebug);
        FProductOnClient_aktobject.S['productVersion'] :=
          Productvars.Values['productVersion'];
        //if FProductOnClient_aktobject.isNull('productVersion') then
        //  FProductOnClient_aktobject := FProductOnClient_aktobject.put('productVersion',nil);
        FProductOnClient_aktobject.S['productVersion'] :=
          Productvars.Values['productVersion'];
      end;
    end;
    if lastAction <> tac4Custom then
    begin
      if (Productvars <> nil) and (Productvars.values['packageVersion'] <> '') then
      begin
        //if FProductOnClient_aktobject.isNull('packageVersion') then
        //  FProductOnClient_aktobject := FProductOnClient_aktobject.put('packageVersion',nil);
        FProductOnClient_aktobject.S['packageVersion'] :=
          Productvars.Values['packageVersion'];
      end;
    end;
    FProductOnClient_aktobject.S['lastAction'] := lastActionS;
    if lastAction <> tac4Custom then
      FProductOnClient_aktobject.S['installationStatus'] := installationStatusS;
    parastr := FProductOnClient_aktobject.asJson(False, False);
    LogDatei.log('Opsi4Data.ProductOnClient_update, params (JSON): ' +
      parastr, LLDebug2);
    omc := TOpsiMethodCall.Create('productOnClient_updateObject', [parastr]);
    jO := FjsonExecutioner.retrieveJSONObject(omc);
    omc.Free;
  except
    LogDatei.log('Error in opsi4data.ProductOnClient_update, parastr: ' +
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
  testsyntaxmode: boolean = False;
begin
  try
    Result := True;
    //ActionStr := FProductOnClient_aktObject.get('actionRequest').toString;
    str := FProductOnClient_aktobject.asJson(False, False);
    ActionStr := FProductOnClient_aktObject.AsObject.S['actionRequest'];
    LogDatei.log('In opsi4data.UpdateSwitches, Actionstr: ' + Actionstr, LLDebug2);

    {$IFDEF OPSISCRIPT}
    if osconf.configTestSyntax then
      testsyntaxmode := True;
    {$ENDIF OPSISCRIPT}

    if testsyntaxmode then
    begin
      // testsyntax mode
      Progress := 'testsyntax';
      if extremeErrorLevel > levelfatal then
      begin
        Progress := 'ok: testsyntax';
        if ActionStr = 'setup' then
        begin
          //successful after setup
          ProductOnClient_update(Progress,
            tar4Successful,
            tac4None,
            ttc4Installed,
            tac4Setup,
            tps4Unkown);
        end
        else if ActionStr = 'update' then
        begin
          //successful after update
          ProductOnClient_update(Progress,
            tar4Successful,
            tac4None,
            ttc4Installed,
            tac4Update,
            tps4Unkown);
        end
        else if ActionStr = 'uninstall' then
        begin
          //successful after uninstall
          ProductOnClient_update(Progress,
            tar4Successful,
            tac4None,
            ttc4Forbidden,
            tac4Uninstall,
            tps4Unkown);
        end
        else if ActionStr = 'always' then
        begin
          //successful after always
          ProductOnClient_update(Progress,
            tar4Successful,
            tac4Always,
            ttc4Always,
            tac4Always,
            tps4Unkown);
        end
        else if ActionStr = 'once' then
        begin
          //successful after Once
          ProductOnClient_update(Progress,
            tar4Successful,
            tac4None,
            ttc4Forbidden,
            tac4Once,
            tps4Unkown);
        end
        else if ActionStr = 'custom' then
        begin
          //successful after Custom
          ProductOnClient_update(Progress,
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
    end
    else
    begin
      // normal (no testsyntax) mode
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
    end;
  except
    LogDatei.log('Error in opsi4data.UpdateSwitches, Actionstr: ' +
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

function TOpsi4Data.actionRequest4ToString(actionRequest: TActionRequest4): string;
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

function TOpsi4Data.getJSONFromService(const method: string;
  const params: array of string; logErrorMessage: string): string;
var
  omc: TOpsiMethodCall;
  Error: boolean;
begin
  omc := TOpsiMethodCall.Create(method, params);
  try
    Result := checkAndRetrieve(omc, Error);
    if Error then
    begin
      Result := '';
      Logdatei.Log(logErrorMessage, LLWarning);
    end;
  finally
    omc.Free;
  end;
end;

function TOpsi4Data.getOpsiServiceConfigs: string;
var
  parastr: string;
  errorOccured: boolean = False;
  omc: TOpsiMethodCall;
begin
  Result := '';
  parastr := '{"objectId": "' + actualClient + '"}';
  omc := TOpsiMethodCall.Create('configState_getObjects', ['', parastr]);
  Result := checkAndRetrieve(omc, errorOccured);
  omc.Free;
end;


function TOpsi4Data.getConfigStateObjectsFromService(
  ConfigIDsAsJsonArray: string): string;
var
  method: string;
  params: array of string;
  LogErrorMessage: string;
begin
  Result := '';
  method :=  'configState_getObjects';
  params := ['', '{"objectId":"' + actualClient + '" ,' + '"configId":' +
    ConfigIDsAsJsonArray + '}'];
  LogErrorMessage := 'Warning: Could not get config states from service (oswebservice: TOpsi4Data.getConfigStateObjectsFromService)';
  Result := getJSONFromService(method, params, logErrorMessage);
end;


function TOpsi4Data.getConfigObjectsFromService(
  ConfigIDsAsJsonArray: string): string;
var
  method: string;
  params: array of string;
  LogErrorMessage: string;
begin
  Result := '';
  method :=  'config_getObjects';
  params := ['', '{"id":' + ConfigIDsAsJsonArray + '}'];
  LogErrorMessage := 'Warning: Could not get config defaults from service (oswebservice: TOpsi4Data.getConfigObjectsFromService)';
  Result := getJSONFromService(method, params, logErrorMessage);
end;



(************************ End of TOpsi4Data ***********************************)

initialization
  // {$i widatamodul.lrs}
end.
