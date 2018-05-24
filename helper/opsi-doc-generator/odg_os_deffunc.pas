unit odg_os_deffunc;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  osparserhelper,
  oslog,
  SysUtils;

(*

type
  //TosdfDataTypes = (dfpString,dfpStringlist,dfpBoolean);
  TosdfDataTypes = (dfpString,dfpStringlist,dfpVoid);
  TosdfDataTypesNames = Array [TosdfDataTypes] of String [50];
//  TPosdfParameterTypesNames = TosdfParameterTypesNames^;

  TOsDefinedFunctionParameter =
    record
      callByReference : boolean;
      paramName : string;
      paramDataType : TosdfDataTypes;
    end;

  TOsDefinedLocalVar =
    record
      varName : string;
      varDataType : TosdfDataTypes;
      varValueString : String;
      //varValueBool : String;  // has to be string, since opsi-script do not know boolean variables
      VarValueList : Tstringlist;
      callByReference : boolean;
      referencevarname : string;
      referencevarscopeindex : integer; // <-1: invalid, -1 : global, >=0: funcindex
    end;

  TOsDefinedFunction  = class(TObject)
  private
    DFName : string;
    DFparamCount : integer;
    DFparamList : array [0..64] of TOsDefinedFunctionParameter;
    DFcontent : TStringList;
    DFLocalVarList : Array of TOsDefinedLocalVar;
    DFResultType : TosdfDataTypes;
    DFResultString : String;
    DFResultList : Tstringlist;
    //DFResultBool : boolean;
    DFindex : integer;
    DFParentFunc : string;
    DFActive : boolean;
    DFOriginFile : string;
    DFOriginFileStartLineNumber : integer;

  public
    constructor create;
    destructor destroy;
    function parseDefinition(definitionStr : string; var errorstr : string) : boolean;

    procedure addContent(contentstr : string);
    function  checkContent(var errorstr : string) : boolean;
    function validIdentifier(identifier : string; var errorstr : string) : boolean;
    function stringTofunctiontype(const str : string; var ftype : TosdfDataTypes) : boolean;
    //function addLocalVar(name : string; datatype : TosdfDataTypes; value : variant) : boolean;
    function locaVarExists(name : string) : boolean;
    function locaVarIndex (name : string) : integer;
    function getLocalVarDatatype(name : string) : TosdfDataTypes;
    function getLocalVarValueString(name : string) : string;
    function getLocalVarValueList(name : string) : Tstringlist;
    //function getLocalVarValueBool(name : string) : boolean;
    function setLocalVarValueString(name : string; value : string) : boolean;
    function setLocalVarValueList(name : string; value : Tstringlist) : boolean;
    //function setLocalVarValueBool(name : string; value : boolean) : boolean;
    function addLocalVarValueString(name : string; value : string) : boolean;
    function addLocalVarValueList(name : string; value : Tstringlist) : boolean;
    //function addLocalVarValueBool(name : string; value : boolean) : boolean;
    function addLocalVar(name : string; datatype : TosdfDataTypes; callByReference :boolean) : boolean;
    //function getLocalVarReference(name : string) : pointer;

    function parseCallParameter(paramline : string; var remaining : string;
               var errorstr : string; NestLevel : integer;
               inDefFuncIndex : integer) : boolean;
    //function call(paramline : string; var remaining : string) : boolean;
    function call(paramline : string; var remaining : string;
               var NestLevel : integer) : boolean;

    property Name : String read DFName;
    property datatype : TosdfDataTypes read DFResultType;
    property Resultstring : String read DFResultString;
    //property ResultBool : boolean read DFResultBool;
    property ResultList : Tstringlist read DFResultList;
    property Index : integer read DFindex write DFindex;
    property ParentFunc : String read DFParentFunc;
    property Active : boolean read DFActive write DFActive;
    property OriginFile : String read DFOriginFile write DFOriginFile;
    property OriginFileStartLineNumber : integer read DFOriginFileStartLineNumber write DFOriginFileStartLineNumber;
  end;

    TDefinedFunctionsArray = Array of TOsDefinedFunction;

  function isVisibleLocalVar(varname: string; var index : integer) : boolean;
  function isVisibleGlobalStringVar(varname: string; var index : integer) : boolean;
  function isVisibleGlobalStringlistVar(varname: string; var index : integer) : boolean;
  procedure freeDefinedFunctions;
  function isVisibleStringVar(varname: string) : boolean;
  function isVisibleStringlistVar(varname: string) : boolean;
  function getFirstLineAfterEndFunc(list : TStringlist; startline : integer) : integer;
  function getVisibleLocalStringVarNameValueList : TStringlist;
  function IsEndOfLocalFunction(const s: string): boolean;

var
  osdfParameterTypesNames : TosdfDataTypesNames;
  remaining : string;
  definedFunctionNames : Tstringlist;
  inDefinedFuncNestCounter : integer = 0;
  definedFunctionsCallStack : TStringlist;
  definedFunctionArray : TDefinedFunctionsArray;
  definedFunctioncounter : integer = 0;


implementation
uses
  osparser,
  oslog;
*)
//#####################################################################


Type

TosdfDataTypes = (dftString,dftStringlist,dftVoid);
TParamDataTypes = (dptString,dptStringlist);
TfuncTypesNames = Array [TosdfDataTypes] of String [50];
TParamTypesNames = Array [TParamDataTypes] of String [50];

TParamDoc = class
  private
    FParamName : string;
    FcallByReference : boolean;
    FParamType : TParamDataTypes;
    FParamDesc : string;
    FParamAdvice : string;
  public
    constructor Create;
    destructor Destroy;
    function getParamTypestring : string;
    property ParamName : string  read FParamName;
    property callByReference : boolean  read FcallByReference;
    property ParamType : string  read getParamTypestring;
    property ParamDesc : string  read FParamDesc;
    property ParamAdvice : string  read FParamAdvice;
end;

TFuncDoc =  class
  private
    FDefinitionline : string;
    FName : string;
    FResultType : TosdfDataTypes;
    FAuthor : string;
    FEmail : string;
    FVersion : string;
    FDate : String;
    FCopyright : string;
    FDescription : string;
    FReturns :string;
    FParamCounter : integer;
    FOnError : string;
    FSpecialCase : string;
    FReferences : string;
    FLinks : string;
    FRequires : string;
  public
    Fparams : array of TParamDoc;
    constructor Create;
    destructor Destroy;
    property Definitionline : string  read FDefinitionline write FDefinitionline;
    property Name : string  read Fname write Fname;
    property ResultType : TosdfDataTypes  read FResultType write FResultType;
    property Author : string  read FAuthor write FAuthor;
    property Date : String  read FDate write FDate;
    property Copyright : string  read FCopyright write FCopyright;
    property Description : string  read FDescription write FDescription;
    property Returns :string  read FReturns write FReturns;
    property ParamCounter : integer  read FParamCounter write FParamCounter;
    property OnError : string  read FOnError write FOnError;
    property SpecialCase : string  read FSpecialCase write FSpecialCase;
    property References : string  read FReferences write FReferences;
    property Links : string  read FLinks write FLinks;
    property Requires : string  read FRequires write FRequires;
    property Email : string  read FEmail write FEmail;
    property Version : string  read FVersion write FVersion;
  end;

TFileDoc =  class
  private
    Fname : string;
    FAuthor : string;
    FEmail : string;
    FVersion : string;
    FDate : String;
    FCopyright : string;
    Ffiledesc : string;
    //Ffunctions : array of TFuncDoc;
    FfunctionCounter : integer;
  public
    Ffunctions : array of TFuncDoc;
    constructor Create;
    destructor Destroy;
    property name : string  read Fname write Fname;
    property filedesc : string  read Ffiledesc write Ffiledesc;
    property Email : string  read FEmail write FEmail;
    property Author : string  read FAuthor write FAuthor;
    property Version : string  read FVersion write FVersion;
    property Date : String  read FDate write FDate;
    property Copyright : string  read FCopyright write FCopyright;
    //property functions : array of TFuncDoc   read Ffunctions write Ffunctions;
    property functionCounter : integer  read FfunctionCounter write FfunctionCounter;
  end;

const
  ccomment = ';';
  cdeffunc = 'deffunc';
  cendfunc = 'endfunc';
  cfiledesc = '@filedesc';
  cauthor = '@author';
  cdate = '@date';
  cemail = '@email';
  cversion = '@version';
  ccopyright = '@copyright';
  CDescription = '@Description';
  COnError = '@OnError';
  CReturns = '@Returns';
  CSpecialCase = '@SpecialCase';
  CReferences = '@References';
  CLinks = '@Links';
  CRequires = '@Requires';
  CParamDesc = '@ParamDesc_';
  CParamAdvice = '@ParamAdvice_';
  CParam = '@Param';

function parseInput_opsiscriptlibrary(filename : string) : boolean;

var
  docobject : TFileDoc;

implementation

uses
  odg_main;

var
  ParamTypesNames : TParamTypesNames;
  funcTypesNames  : TfuncTypesNames;


constructor TFileDoc.create;
begin
  FName := '';
  Ffiledesc := '';
  FAuthor := '';
  FEmail := '';
  FVersion := '';
  FDate := '';
  FCopyright := '';
  FfunctionCounter := 0;
  Inherited;
end;

destructor TFileDoc.destroy;
begin
  SetLength(Ffunctions,0);
  inherited;
end;

constructor TFuncDoc.create;
begin
  FDefinitionline := '';
  FName := '';
  FResultType :=  dftVoid;
  FAuthor := '';
  FDate := '';
  FCopyright := '';
  FDescription := '';
  FReturns := '';
  //Fparams : array of TParamDoc;
  FParamCounter := 0;
  FOnError := '';
  FSpecialCase := '';
  FReferences := '';
  FLinks := '';
  FRequires := '';
  Inherited;
end;

destructor TFuncDoc.destroy;
begin
  SetLength(Fparams,0);
  inherited;
end;

constructor TParamDoc.create;
begin
  FParamName := '';
  FcallByReference := false;
  FParamType := dptString;
  FParamDesc := '';
  FParamAdvice := '';
  Inherited;
end;

destructor TParamDoc.destroy;
begin
  inherited;
end;

function TParamDoc.getParamTypestring : string;
begin
  result := ParamTypesNames[FParamType];
end;

function  stringTofunctiontype(const str : string; var ftype : TosdfDataTypes) : boolean;
begin
  result := false;
  if LowerCase(str) = LowerCase(funcTypesNames[dftString]) then
  begin
    result := true;
    ftype := dftString;
  end
  else if LowerCase(str) = LowerCase(funcTypesNames[dftStringlist]) then
  begin
    result := true;
    ftype := dftStringlist;
  end
  else if LowerCase(str) = LowerCase(funcTypesNames[dftVoid]) then
  begin
    result := true;
    ftype := dftVoid;
  end
end;


procedure parseDefFunc(definitionStr : string; myfunc : TFuncDoc);
var
  paramnamestr, paramtypestr, calltype : string;
  paramcounter : integer;
  endOfParamlist,mycallByReference : boolean;
  tmpstr : string;
  remaining,errorstr : string;
begin
  //parseDefFunc := false;
  //syntax_ok := true;
  endOfParamlist := false;
  paramcounter := -1;
  myfunc.FDefinitionline:=trim(definitionStr);
  // get function name
  GetWord(trim(definitionStr), myfunc.FName, remaining,WordDelimiterSet5);

  LogDatei.log('Found new defined function name: '+myfunc.FName,LLDebug2);
  if  skip('(',remaining,remaining,errorstr) then
  begin
    // test on no parameters
    tmpstr := remaining;
    if skip(')',remaining,remaining,errorstr) then
    begin
      endOfParamlist := true;
      myfunc.FParamCounter := 0;
    end
    else
      remaining := tmpstr;

    while  not endOfParamlist do
    begin
      // check call type
      calltype := 'val';
      if skip('val',LowerCase(remaining),remaining,errorstr) then calltype := 'val';
      if skip('ref',LowerCase(remaining),remaining,errorstr) then calltype := 'ref';
      if lowercase(calltype) = 'ref' then mycallByReference := true
      else mycallByReference := false;
      // check paramname
      GetWord(remaining, paramnamestr, remaining,[':']);
      paramnamestr := trim(paramnamestr);

      LogDatei.log('Found defined function parametername: '+paramnamestr,LLDebug2);
      inc(paramcounter);

      myfunc.FParamCounter := paramcounter +1;
      setlength(myfunc.Fparams,myfunc.FParamCounter);
      myfunc.Fparams[paramcounter] := TParamDoc.Create;
      with myfunc.Fparams[paramcounter] do
      begin
        if mycallByReference then FcallByReference:= true
        else  FcallByReference:= false;
        LogDatei.log('Parameter has call type: '+calltype,LLDebug2);
        // is a new param
        FparamName:= paramnamestr;
        //DFLocalVarList.Add(paramname);
        if  skip(':',remaining,remaining,errorstr) then
        begin
          GetWord(remaining, paramtypestr, remaining,[',',')']);
          paramtypestr := trim(paramtypestr);
          if lowercase(paramtypestr) = lowercase(ParamTypesNames[dptString]) then
          begin
            // String type
            FParamType:=dptString;
          end
          else if lowercase(paramtypestr) = lowercase(ParamTypesNames[dptStringlist]) then
          begin
            // Stringlist type
            FParamType := dptStringlist;
          end;
        end; // skip :
        // check for endOfParamlist
        tmpstr := remaining;
        if skip(')',remaining,remaining,errorstr) then
        begin
          endOfParamlist := true;
        end
        else
        begin
          remaining := tmpstr;
          if skip(',',remaining,remaining,errorstr) then
        end
      end; // with
    end; // while
    // get function type
    if skip(':',remaining,remaining,errorstr) then
    begin
      if stringTofunctiontype(remaining, myfunc.FResultType) then
      begin
        LogDatei.log('Function has valid data type: '+funcTypesNames[myfunc.FResultType],LLDebug2);
      end;
    end
  end;
  //parseDefinition := true;
end;

function onMarkerAddDocStringTo(marker : string;docstring : string;var target :string) : boolean;
var
  tmpstr1 : string;
begin
  result := false;
  if pos(lowercase(marker),lowercase(docstring)) = 1 then
  begin
    tmpstr1 := trim(copy(docstring,length(marker)+1,length(docstring)));
    if target = '' then target := tmpstr1
    else target := target+LineEnding+tmpstr1;
    result := true;
  end;
end;

function parseInput_opsiscriptlibrary(filename : string) : boolean;
var
  linecounter, funccounter,prun : integer;
  indeffunc : integer;
  aktline, expr, remaining, pname, tmpstr1, tmpstr2, tmpstr3 : string;
  incomment : boolean;
begin
  result := true;
  indeffunc := 0;
  if Assigned(docobject) and (docobject <> nil) then docobject.Destroy;
  docobject := TFileDoc.Create;
  docobject.Fname:=ExtractFileName(filename);
  for linecounter := 0 to sourcelist.Count-1 do
  begin
    incomment := false;
    aktline := trim(sourcelist.Strings[linecounter]);
    if indeffunc = 0 then
    begin  // not in defined function
      if pos(lowercase(cdeffunc),lowercase(aktline)) = 1 then
      begin // function definition line
        inc(indeffunc);
        funccounter := docobject.Ffunctioncounter;
        inc(funccounter);
        docobject.Ffunctioncounter := funccounter;
        setlength(docobject.Ffunctions,funccounter);
        docobject.Ffunctions[funccounter-1] := TFuncDoc.Create;
        parseDefFunc(copy(aktline,length(cdeffunc)+1,length(aktline)),docobject.Ffunctions[funccounter-1]);
      end
      else
      if pos(ccomment,aktline) = 1 then
      begin
        incomment := true;
        aktline := trim(copy(aktline,length(ccomment)+1,length(aktline)));
      end;
      if incomment then
      begin    // document related ?
        if not onMarkerAddDocStringTo(cauthor,aktline,docobject.FAuthor) then
        if not onMarkerAddDocStringTo(cdate,aktline,docobject.FDate) then
        if not onMarkerAddDocStringTo(ccopyright,aktline,docobject.FCopyright) then
        if not onMarkerAddDocStringTo(cemail,aktline,docobject.FEmail) then
        if not onMarkerAddDocStringTo(cversion,aktline,docobject.FVersion) then
        onMarkerAddDocStringTo(cfiledesc,aktline,docobject.Ffiledesc)
      end;
    end
    else
    begin  // in defined function
      if pos(lowercase(cendfunc),lowercase(aktline)) = 1 then
      begin // function end line
        dec(indeffunc);
      end
      else
      if pos(ccomment,aktline) = 1 then
      begin
        incomment := true;
        aktline := trim(copy(aktline,length(ccomment)+1,length(aktline)));
      end;
      if incomment then
      begin
        if not onMarkerAddDocStringTo(cauthor,aktline,docobject.Ffunctions[funccounter-1].FAuthor) then
        if not onMarkerAddDocStringTo(cdate,aktline,docobject.Ffunctions[funccounter-1].FDate) then
        if not onMarkerAddDocStringTo(cemail,aktline,docobject.Ffunctions[funccounter-1].FEmail) then
        if not onMarkerAddDocStringTo(cversion,aktline,docobject.Ffunctions[funccounter-1].FVersion) then
        if not onMarkerAddDocStringTo(ccopyright,aktline,docobject.Ffunctions[funccounter-1].FCopyright) then
        if not onMarkerAddDocStringTo(CDescription,aktline,docobject.Ffunctions[funccounter-1].FDescription) then
        if not onMarkerAddDocStringTo(COnError,aktline,docobject.Ffunctions[funccounter-1].FOnError) then
        if not onMarkerAddDocStringTo(CReturns,aktline,docobject.Ffunctions[funccounter-1].FReturns) then
        if not onMarkerAddDocStringTo(CSpecialCase,aktline,docobject.Ffunctions[funccounter-1].FSpecialCase) then
        if not onMarkerAddDocStringTo(CReferences,aktline,docobject.Ffunctions[funccounter-1].FReferences)then
            onMarkerAddDocStringTo(CLinks,aktline,docobject.Ffunctions[funccounter-1].FLinks);
        // parameter ?
        if pos(lowercase(CParam),lowercase(aktline)) = 1 then
        begin
          for prun := 0 to docobject.Ffunctions[funccounter-1].ParamCounter-1 do
          begin
            pname := docobject.Ffunctions[funccounter-1].Fparams[prun].FParamName;
            if not onMarkerAddDocStringTo(lowercase(CParamDesc+pname),aktline,docobject.Ffunctions[funccounter-1].Fparams[prun].FParamDesc)
            then onMarkerAddDocStringTo(lowercase(CParamAdvice+pname),aktline,docobject.Ffunctions[funccounter-1].Fparams[prun].FParamAdvice);
          end;
        end;
      end;
    end;
  end;
end;

begin
  ParamTypesNames[dptString] :=  'String';
  ParamTypesNames[dptStringlist] :=  'Stringlist';
  funcTypesNames[dftString] :=  'String';
  funcTypesNames[dftStringlist] :=  'Stringlist';
  funcTypesNames[dftVoid] :=  'Void';


end.

