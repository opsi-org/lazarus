unit odg_os_deffunc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

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
end;

TFuncDoc =  class
  private
    FDefinitionline : string;
    FName : string;
    FResultType : TosdfDataTypes;
    FAuthor : string;
    FDate : String;
    FCopyright : string;
    FDescription : string;
    FReturns :string;
    Fparams : array of TParamDoc;
    FParamNumber : integer;
    FOnError : string;
    FSpecialCase : string;
    FReferences : string;
    FLinks : string;
    FRequires : string;
  public
    constructor Create;
    destructor Destroy;
  end;

TFileDoc =  class
  private
    Fname : string;
    Ffiledesc : string;
    Ffunctions : array of TFuncDoc;
    FfunctionNumber : integer;
  public
    constructor Create;
    destructor Destroy;
  end;

const
  ccomment = ';';
  cdeffunc = 'deffunc';
  cendfunc = 'endfunc';
  cfiledesc = '@filedesc';
  cauthor = '@author';
  cdate = '@date';
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

function parseInput_opsiscriptlibrary(filename : string) : boolean;

var
  docobject : TFileDoc;

implementation

uses
  odg_main;


constructor TFileDoc.create;
begin
  Inherited;
end;

destructor TFileDoc.destroy;
begin
  SetLength(Ffunctions,0);
  inherited;
end;

constructor TFuncDoc.create;
begin
  Inherited;
end;

destructor TFuncDoc.destroy;
begin
  SetLength(Fparams,0);
  inherited;
end;

constructor TParamDoc.create;
begin
  Inherited;
end;

destructor TParamDoc.destroy;
begin
  inherited;
end;

procedure parseDefFunc(line : string);
begin

end;

procedure onMarkerAddDocStringTo(marker : string;docstring : string;var target :string);
var
  tmpstr1 : string;
begin
  if pos(marker,docstring) = 1 then
  begin
    tmpstr1 := copy(docstring,length(marker)+1,length(docstring));
      if target = '' then target := tmpstr1
    else target := target+LineEnding+tmpstr1;
  end
end;

function parseInput_opsiscriptlibrary(filename : string) : boolean;
var
  linecounter, funccounter : integer;
  indeffunc : integer;
  aktline, expr, remaining, tmpstr1, tmpstr2, tmpstr3 : string;
  incomment : boolean;
begin
  indeffunc := 0;
  if Assigned(docobject) and (docobject <> nil) then docobject.Destroy;
  docobject := TFileDoc.Create;
  docobject.Fname:=filename;
  for linecounter := 0 to sourcelist.Count-1 do
  begin
    incomment := false;
    aktline := trim(lowercase(sourcelist.Strings[linecounter]));
    if indeffunc = 0 then
    begin  // not in defined function
      if pos(cdeffunc,aktline) = 1 then
      begin // function definition line
        inc(indeffunc);
        funccounter := docobject.FfunctionNumber;
        inc(funccounter);
        setlength(docobject.Ffunctions,funccounter);
        docobject.Ffunctions[funccounter-1] := TFuncDoc.Create;
        parseDefFunc(copy(aktline,length(cdeffunc)+1,length(aktline)));
      end
      else
      if pos(ccomment,aktline) = 1 then
      begin
        incomment := true;
        aktline := trim(copy(aktline,length(ccomment)+1,length(aktline)));
      end;
      if incomment then
      begin
        onMarkerAddDocStringTo(cfiledesc,aktline,docobject.Ffiledesc);
      end;
    end
    else
    begin  // in defined function
      if pos(cendfunc,aktline) = 1 then
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
        onMarkerAddDocStringTo(cauthor,aktline,docobject.Ffunctions[funccounter-1].FAuthor);
        onMarkerAddDocStringTo(cdate,aktline,docobject.Ffunctions[funccounter-1].FDate);
        onMarkerAddDocStringTo(ccopyright,aktline,docobject.Ffunctions[funccounter-1].FCopyright);
        onMarkerAddDocStringTo(CDescription,aktline,docobject.Ffunctions[funccounter-1].FDescription);
        onMarkerAddDocStringTo(COnError,aktline,docobject.Ffunctions[funccounter-1].FOnError);
        onMarkerAddDocStringTo(CReturns,aktline,docobject.Ffunctions[funccounter-1].FReturns);
        onMarkerAddDocStringTo(CSpecialCase,aktline,docobject.Ffunctions[funccounter-1].FSpecialCase);
        onMarkerAddDocStringTo(CReferences,aktline,docobject.Ffunctions[funccounter-1].FReferences);
        onMarkerAddDocStringTo(CLinks,aktline,docobject.Ffunctions[funccounter-1].FLinks);
      end;
    end;
  end;


end;



end.

