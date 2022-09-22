unit osdefinedfunctions;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  TypInfo,
  osparserhelper,
  osparser,
  ostxstringlist,
  osconf,
  osfunc;

type
  //TosdfDataTypes = (dfpString,dfpStringlist,dfpBoolean);
  TosdfDataTypes = (dfpString, dfpStringlist, dfpVoid);
  TosdfDataTypesNames = array [TosdfDataTypes] of string [50];
  //  TPosdfParameterTypesNames = TosdfParameterTypesNames^;

  TOsDefinedLocalVarInstance = record
    varValueString: string;
    VarValueList: TStringList;
    referencevarname: string;
    referencevarscopeindex: integer; // <-1: invalid, -1 : global, >=0: funcindex
    inuse: boolean;
  end;

  TOsDefinedFunctionParameter = record
    callByReference: boolean;
    paramName: string;
    paramDataType: TosdfDataTypes;
    paramValueBuf: TOsDefinedLocalVarInstance;
  end;



  TOsDefinedLocalVar = record
    varName: string;
    varInstance: array of TOsDefinedLocalVarInstance;
    varDataType: TosdfDataTypes;
    //varValueString : String;
    //VarValueList : Tstringlist;
    callByReference: boolean;
    //referencevarname : string;
    //referencevarscopeindex : integer; // <-1: invalid, -1 : global, >=0: funcindex
  end;

  TOsDefinedFunction = class(TObject)
  private
    DFName: string;
    DFparamCount: integer;
    DFparamList: array [0..64] of TOsDefinedFunctionParameter;
    DFcontent: TStringList;
    DFLocalVarList: array of TOsDefinedLocalVar;
    DFVarInstanceIndex: integer; // the number of valid parallel (recursive) instances
    DFResultType: TosdfDataTypes;
    DFResultString: string;
    DFResultList: TStringList;
    //DFResultBool : boolean;
    DFindex: integer;
    DFParentFunc: string;
    DFActive: boolean;
    DFOriginFile: string;
    DFOriginFileStartLineNumber: integer;

  public
    constructor Create;
    destructor Destroy;
    function parseDefinition(definitionStr: string; var errorstr: string): boolean;

    function checkContent(var errorstr: string): boolean;
    function validIdentifier(identifier: string; var errorstr: string): boolean;
    function stringTofunctiontype(const str: string;
      var ftype: TosdfDataTypes): boolean;
    //function addLocalVar(name : string; datatype : TosdfDataTypes; value : variant) : boolean;
    function localVarNameExists(Name: string): boolean;
    function localVarExists(Name: string): boolean;
    function localVarNameIndex(Name: string): integer;
    function localVarIndex(Name: string): integer;
    function getLocalVarDatatype(Name: string): TosdfDataTypes;
    function getLocalVarValueString(Name: string): string;
    function getLocalVarValueList(Name: string): TStringList;
    //function getLocalVarValueBool(name : string) : boolean;
    function setLocalVarValueString(Name: string; Value: string): boolean;
    function setLocalVarValueList(Name: string; Value: TStringList): boolean;
    //function setLocalVarValueBool(name : string; value : boolean) : boolean;
    //function addLocalVarValueString(name : string; value : string) : boolean;
    //function addLocalVarValueList(name : string; value : Tstringlist) : boolean;
    //function addLocalVarValueBool(name : string; value : boolean) : boolean;
    function addLocalVar(Name: string; datatype: TosdfDataTypes;
      callByReference: boolean): boolean;
    function delLocalVar(Name: string): boolean;
    //function getLocalVarReference(name : string) : pointer;

    function parseCallParameter(paramline: string; var remaining: string;
      var errorstr: string; NestLevel: integer; inDefFuncIndex: integer): boolean;
    function copyParamBufToLocalVars: boolean;
    function call(paramline: string; var remaining: string;
      var NestLevel: integer): boolean;
    function createVarInstance(varindex: integer): boolean;
    // create a new Instance of local Variable
    function destroyVarInstance(varindex: integer): boolean;
    // release the var instance
    function createAllVarInstances: boolean;
    // create a new Instances of all local Variables  and inc DFVarInstanceIndex
    function destroyAllVarInstances: boolean;
    // release the var instance (DFVarInstanceIndex) and dec DFVarInstanceIndex

    property Name: string read DFName;
    property datatype: TosdfDataTypes read DFResultType;
    property Resultstring: string read DFResultString;
    //property ResultBool : boolean read DFResultBool;
    property ResultList: TStringList read DFResultList;
    property Index: integer read DFindex write DFindex;
    property ParentFunc: string read DFParentFunc;
    property Active: boolean read DFActive write DFActive;
    property OriginFile: string read DFOriginFile write DFOriginFile;
    property OriginFileStartLineNumber: integer
      read DFOriginFileStartLineNumber write DFOriginFileStartLineNumber;
    property Content: TStringList read DFContent write DFContent;
  end;

  TDefinedFunctionsArray = array of TOsDefinedFunction;

function isVisibleLocalVar(varname: string; var index: integer): boolean;
function isVisibleGlobalStringVar(varname: string; var index: integer): boolean;
function isVisibleGlobalStringlistVar(varname: string; var index: integer): boolean;
procedure freeDefinedFunctions;
function isVisibleStringVar(varname: string): boolean;
function isVisibleStringlistVar(varname: string): boolean;
function getFirstLineAfterEndFunc(list: TStringList; startline: integer): integer;
function getVisibleLocalStringVarNameValueList: TStringList;
function IsEndOfLocalFunction(const s: string): boolean;
function addLoopvarToVarList(const loopvar: string; var errmesg: string): boolean;
function delLoopvarFromVarList(const loopvar: string; var errmesg: string): boolean;

var
  osdfParameterTypesNames: TosdfDataTypesNames;
  remaining: string;
  definedFunctionNames: TStringList;
  inDefinedFuncNestCounter: integer = 0;
  definedFunctionsCallStack: TStringList;
  definedFunctionArray: TDefinedFunctionsArray;
  definedFunctioncounter: integer = 0;


implementation

uses
  //osparser,
  oslog;

constructor TOsDefinedFunction.Create;
begin
  DFcontent := TStringList.Create;
  DFResultList := TStringList.Create;
  DFActive := False;
  DFVarInstanceIndex := -1;
  inherited;
end;

destructor TOsDefinedFunction.Destroy;
begin
  DFcontent.Free;
  DFResultList.Free;
  SetLength(DFLocalVarList, 0);
  inherited;
end;

function TOsDefinedFunction.stringTofunctiontype(const str: string;
  var ftype: TosdfDataTypes): boolean;
begin
  Result := False;
  if LowerCase(str) = LowerCase(osdfParameterTypesNames[dfpString]) then
  begin
    Result := True;
    ftype := dfpString;
  end
  else if LowerCase(str) = LowerCase(osdfParameterTypesNames[dfpStringlist]) then
  begin
    Result := True;
    ftype := dfpStringlist;
  end
  else if LowerCase(str) = LowerCase(osdfParameterTypesNames[dfpVoid]) then
  begin
    Result := True;
    ftype := dfpVoid;
  end;
  //else if LowerCase(str) = LowerCase(osdfParameterTypesNames[dfpBoolean]) then
  //begin
  //  result := true;
  //  ftype := dfpBoolean;
  //end;
end;

function TOsDefinedFunction.validIdentifier(identifier: string;
  var errorstr: string): boolean;
var
  SectionSpecifier: TSectionSpecifier;
  myStatementKind: TStatement;
begin
  SectionSpecifier := tsecNoSection;
  validIdentifier := False;
  if identifier = '' then
  begin
    // empty identifier not allowed
    errorstr := errorstr + 'empty identifier not allowed';
  end
  else
  begin
    myStatementKind := findKindOfStatement(identifier, SectionSpecifier, identifier);
    if myStatementKind <> tsNotDefined then
      errorstr := errorstr + ' Given identifier: ' + identifier +
        ' is a reserved name (' + TypInfo.GetEnumName(TypeInfo(TStatement),
        integer(myStatementKind)) + ') and can not be used as identifier'
    else if localVarIndex(lowercase(identifier)) >= 0 then
      errorstr := errorstr + ' Given identifier: ' + identifier +
        ' is already defined as local variable name and can not  be used as identifier again'
    else
      validIdentifier := True;

  end;
  // todo global vars
end;

function TOsDefinedFunction.parseDefinition(definitionStr: string;
  var errorstr: string): boolean;
var
  paramname, paramtype, calltype: string;
  paramcounter: integer;
  syntax_ok, endOfParamlist, callByReference: boolean;
  searchindex, index: integer;
  tmpstr: string;
begin
  parseDefinition := False;
  syntax_ok := True;
  endOfParamlist := False;
  paramcounter := -1;
  // get function name
  GetWord(definitionStr, DFName, remaining, WordDelimiterSet5);
  if not validIdentifier(DFName, errorstr) then
  begin
    // given functionname not valid
    errorstr := errorstr + ' Given functionname: ' + DFName + ' not valid.';
    syntax_ok := False;
  end
  else
  begin
    // given functionname valid
    DFName := LowerCase(DFName);
    LogDatei.log('Found new defined function name: ' + DFName, LLDebug2);
    if not skip('(', remaining, remaining, errorstr) then
    begin
      // ( expected
      errorstr := errorstr + ' ( expected';
      syntax_ok := False;
    end
    else
    begin
      // test on no parameters
      tmpstr := remaining;
      if skip(')', remaining, remaining, errorstr) then
      begin
        endOfParamlist := True;
        DFparamCount := 0;
      end
      else
        remaining := tmpstr;
      try
        while syntax_ok and not endOfParamlist do
        begin
          // check call type
          calltype := 'val';
          if skip('val', LowerCase(remaining), remaining, errorstr) then
            calltype := 'val';
          if skip('ref', LowerCase(remaining), remaining, errorstr) then
            calltype := 'ref';
          if lowercase(calltype) = 'ref' then
            callByReference := True
          else
            callByReference := False;
          // check paramname
          GetWord(remaining, paramname, remaining, [':']);
          paramname := trim(paramname);
          if remaining = '' then
          begin
            errorstr := errorstr + ': <paramtype> expected after Parameter Name';
            syntax_ok := False;
          end
          else
          begin
            if not validIdentifier(paramname, errorstr) then
            begin
              // given paramname not valid
              errorstr := errorstr + 'given paramname: ' + paramname + ' not valid.';
              syntax_ok := False;
            end
            else
            begin
              // given parameter valid
              LogDatei.log('Found defined function parametername: ' +
                paramname, LLDebug2);
              Inc(paramcounter);
              DFparamCount := paramcounter + 1;
              if callByReference then
                DFparamList[paramcounter].callByReference := True
              else
                DFparamList[paramcounter].callByReference := False;
              LogDatei.log('Parameter has call type: ' + calltype, LLDebug2);
              if localVarNameIndex(paramname) >= 0 then
              begin
                // paramname has been defined before in this funstion
                errorstr := errorstr + ' paramname: ' + paramname +
                  ' has been defined before in this funstion';
                syntax_ok := False;
              end
              else
              begin
                // is a new param
                DFparamList[paramcounter].paramValueBuf.VarValueList :=
                  TStringList.Create;
                DFparamList[paramcounter].paramName := paramname;
                //DFLocalVarList.Add(paramname);
                if not skip(':', remaining, remaining, errorstr) then
                begin
                  // : <paramtype> expected
                  errorstr := errorstr + ' : <paramtype>  expected';
                  syntax_ok := False;
                end
                else
                begin
                  GetWord(remaining, paramtype, remaining, [',', ')']);
                  paramtype := trim(paramtype);
                  if remaining = '' then
                  begin
                    errorstr := errorstr + ', or ) expected after Parameter Type';
                    syntax_ok := False;
                  end
                  else
                  if lowercase(paramtype) = lowercase(
                    osdfParameterTypesNames[dfpString]) then
                  begin
                    // String type
                    DFparamList[paramcounter].paramDataType := dfpString;
                    addLocalVar(paramname, dfpString, callByReference);
                  end
                  else if lowercase(paramtype) =
                    lowercase(osdfParameterTypesNames[dfpStringlist]) then
                  begin
                    // Stringlist type
                    DFparamList[paramcounter].paramDataType := dfpStringlist;
                    addLocalVar(paramname, dfpStringlist, callByReference);
                  end
                  //else if lowercase(paramtype) = lowercase(osdfParameterTypesNames[dfpBoolean]) then
                  //begin
                  //  // Boolean type
                  //  DFparamList[paramcounter].paramDataType:=dfpBoolean;
                  //  addLocalVar(paramname,dfpBoolean);
                  //end
                  else
                  begin
                    // given param type not valid
                    errorstr :=
                      errorstr + 'given param type: ' + paramtype + ' not valid.';
                    syntax_ok := False;
                  end;
                  if not syntax_ok then
                    LogDatei.log('Parameter has invalid data type: ' +
                      paramtype, LLDebug2)
                  else
                  begin
                    LogDatei.log('Parameter has valid data type: ' +
                      paramtype, LLDebug2);
                    if not skip(',', remaining, remaining, errorstr) then
                      if skip(')', remaining, remaining, errorstr) then
                        endOfParamlist := True
                      else
                      begin
                        // syntax error
                        errorstr := errorstr + ' , or ) expected.';
                        syntax_ok := False;
                      end;
                  end; // valid data type
                end; // valid new param
              end; // not defined before
            end;  // is valid identifier
          end; // data type found behind colon
        end; // while
      except
        on e: Exception do
        begin
          LogDatei.log(
            'Exception in osdefinedfunctions:parseDefinition: endOfParamlist: ' +
            e.message, LLError);
          //raise e;
        end;
      end;
      try
        if syntax_ok then
        begin
          // get function type
          if skip(':', remaining, remaining, errorstr) then
          begin
            if not stringTofunctiontype(remaining, DFResultType) then
            begin
              // syntax error : wrong data type
              errorstr := errorstr + ' missing or illegal function data type: ' +
                remaining + ' - only string, stringlist and void allowed.';
              syntax_ok := False;
            end
            else
            begin
              LogDatei.log('Function has valid data type: ' + remaining, LLDebug2);
              // create local result variable from result type
              case DFResultType of
                dfpString: addLocalVar('$result$', dfpString, False);
                dfpStringlist: addLocalVar('$result$', dfpStringlist, False);
                //dfpBoolean : addLocalVar('$result$',dfpBoolean);
              end;
            end;
          end
          else
          begin
            // syntax error : wrong data type
            errorstr := errorstr + ' missing function data type: ' +
              remaining + ' - only string, stringlist and boolean allowed.';
            syntax_ok := False;
          end;
        end;
      except
        on e: Exception do
        begin
          LogDatei.log(
            'Exception in osdefinedfunctions: get function type: ' +
            e.message, LLError);
          //raise e;
        end;
      end;
    end;
  end;
  try
    // set parent func
    if inDefinedFuncNestCounter > 0 then
    begin
      searchindex := definedFunctionsCallStack.Count - 1;
      index := StrToInt(definedFunctionsCallStack.Strings[searchindex]);
      DFParentFunc := definedFunctionArray[index].Name;
    end
    else
    begin
      // we in global
      DFParentFunc := 'global';
    end;
    DFActive := True;
    if syntax_ok then
      parseDefinition := True;
  except
    on e: Exception do
    begin
      LogDatei.log(
        'Exception in osdefinedfunctions: set parent func : ' +
        e.message, LLError);
      //raise e;
    end;
  end;
end;


function TOsDefinedFunction.checkContent(var errorstr: string): boolean;
begin

end;

function TOsDefinedFunction.localVarNameExists(Name: string): boolean;
var
  arraycounter, i: integer;
begin
  Result := False;
  i := 0;
  arraycounter := length(DFLocalVarList);
  if arraycounter > 0 then
    repeat
      if (lowercase(DFLocalVarList[i].varName) = lowercase(Name)) then
        Result := True;
      Inc(i);
    until (i >= arraycounter) or (Result = True);
end;

function TOsDefinedFunction.localVarExists(Name: string): boolean;
var
  arraycounter, i: integer;
  tmpstr: string;
begin
  Result := False;
  i := 0;
  tmpstr := DFName;
  arraycounter := length(DFLocalVarList);
  if arraycounter > 0 then
    repeat
      if (lowercase(DFLocalVarList[i].varName) = lowercase(Name)) then
      begin
        if (DFVarInstanceIndex > -1) and
          (length(DFLocalVarList[i].varInstance) = DFVarInstanceIndex + 1) then
        begin
          if DFLocalVarList[i].varInstance[DFVarInstanceIndex].inuse then
          begin
            Result := True;
            LogDatei.log_prog('Found Local var name: ' + Name +
              ' and Instance with inUse=true  ', LLinfo);
          end
          else
            LogDatei.log_prog('Local var name: ' + Name +
              ' and Instance found but inUse=false ', LLDebug);
        end
        else
          LogDatei.log_prog('Local var name: ' + Name +
            ' found but no VarInstance ', LLDebug);
      end
      else
        LogDatei.log_prog('No local var name: ' + Name + ' found. ', LLDebug3);
      Inc(i);
    until (i >= arraycounter) or (Result = True);
end;

function TOsDefinedFunction.localVarNameIndex(Name: string): integer;
var
  arraycounter, i: integer;
begin
  Result := -1;
  i := 0;
  arraycounter := length(DFLocalVarList);
  if arraycounter > 0 then
    repeat
      if (lowercase(DFLocalVarList[i].varName) = lowercase(Name)) then
        Result := i;
      Inc(i);
    until (i >= arraycounter) or (Result = i - 1);
end;

function TOsDefinedFunction.localVarIndex(Name: string): integer;
var
  arraycounter, i: integer;
begin
  Result := -1;
  i := 0;
  arraycounter := length(DFLocalVarList);
  if arraycounter > 0 then
  (*
  if inDefFuncIndex > -1 then
    with definedFunctionArray[inDefFuncIndex] do
    begin
    *)
    repeat
      if (lowercase(DFLocalVarList[i].varName) = lowercase(Name)) then
      begin
        if (DFVarInstanceIndex > -1) and
          (length(DFLocalVarList[i].varInstance) = DFVarInstanceIndex + 1) then
        begin
          if DFLocalVarList[i].varInstance[DFVarInstanceIndex].inuse then
          begin
            Result := i;
          end
          else
            LogDatei.log_prog('Local var name: ' + Name +
              ' and Instance found but inUse=false ', LLDebug);
        end
        else
          LogDatei.log_prog('Local var name: ' + Name +
            ' found but no VarInstance ', LLDebug);
      end
      else
        LogDatei.log_prog('No local var name: ' + Name + ' found. ', LLDebug3);
      Inc(i);
    until (i >= arraycounter) or (Result = i - 1);
  // end;
end;


function TOsDefinedFunction.addLocalVar(Name: string; datatype: TosdfDataTypes;
  callByReference: boolean): boolean;
var
  arraycounter, varindex, instanceSize: integer;
begin
  LogDatei.log_prog('osdf addLocalVar: ' + Name, LLDebug2);
  Result := False;
  if not localVarNameExists(Name) then
  begin
    if not localVarExists(Name) then
    begin
      // we assume this is the first definition call
      LogDatei.log_prog(
        'osdf addLocalVar: we assume this is the first definition call for ' +
        Name, LLDebug2);
      Result := True;
      arraycounter := length(DFLocalVarList);
      Inc(arraycounter);
      SetLength(DFLocalVarList, arraycounter);
      DFLocalVarList[arraycounter - 1].varName := Name;
      DFLocalVarList[arraycounter - 1].varDataType := datatype;
      DFLocalVarList[arraycounter - 1].callByReference := callByReference;
      createVarInstance(arraycounter - 1);
      if (length(DFLocalVarList[arraycounter - 1].varInstance) =
        DFVarInstanceIndex + 1) and (DFVarInstanceIndex >= 0) then
        DFLocalVarList[arraycounter - 1].varInstance[DFVarInstanceIndex].inuse := True;
    end
    else
    begin
      LogDatei.log_prog('osdf addLocalVar: localVarNameExists but not localVarExists' +
        Name, LLDebug2);
      if DFVarInstanceIndex > 0 then
      begin
        // Instance are created from createAllVarInstances
        arraycounter := localVarNameIndex(Name);
        Result := True;
        if (length(DFLocalVarList[arraycounter].varInstance) =
          DFVarInstanceIndex + 1) and (DFVarInstanceIndex >= 0) then
          DFLocalVarList[arraycounter].varInstance[DFVarInstanceIndex].inuse := True;
      end;
    end;
  end
  else
  begin
    arraycounter := localVarNameIndex(Name);
    if localVarExists(Name) then
    begin
      LogDatei.log_prog('osdf addLocalVar: (localVarNameExists) and  localVarExists ' +
        Name, LLDebug2);
      //DFLocalVarList[arraycounter - 1].varInstance[DFVarInstanceIndex].inuse := True;
      Result := True;
      if (length(DFLocalVarList[arraycounter].varInstance) =
        DFVarInstanceIndex + 1) and (DFVarInstanceIndex >= 0) then
        DFLocalVarList[arraycounter].varInstance[DFVarInstanceIndex].inuse := True;
      //LogDatei.log('Syntax Error: Double definition of local variable: ' +  Name, LLCritical);
    end
    else
    begin
      // this may be the part for recreation of a loopvar ; array counter may be zero
      LogDatei.log_prog('osdf addLocalVar: (localVarNameExists) and  (not localVarExists) '
        +
        Name, LLDebug2);
      if DFVarInstanceIndex >= 0 then
        if (length(DFLocalVarList[arraycounter].varInstance) =
          DFVarInstanceIndex + 1) then
        begin
          DFLocalVarList[arraycounter].varInstance[DFVarInstanceIndex].inuse := True;
          Result := True;
        end
        else
        begin
          if (length(DFLocalVarList[arraycounter].varInstance) = DFVarInstanceIndex) then
          begin
            createVarInstance(arraycounter);
            DFLocalVarList[arraycounter].varInstance[DFVarInstanceIndex].inuse := True;
            Result := True;
          end;
        end;
    end;
  end;
end;

function TOsDefinedFunction.delLocalVar(Name: string): boolean;
var
  arrayindex: integer;
  datatype: TosdfDataTypes;
begin
  Result := False;
  if localVarNameExists(Name) then
  begin
    arrayindex := localVarNameIndex(Name);
    if localVarExists(Name) then
    begin
      DFLocalVarList[arrayindex].varInstance[DFVarInstanceIndex].inuse := False;
      Result := True;
    end
    else
    begin
      if DFVarInstanceIndex > 0 then
      begin
        Result := True;
        if (length(DFLocalVarList[arrayindex].varInstance) =
          DFVarInstanceIndex + 1) and (DFVarInstanceIndex >= 0) then
          DFLocalVarList[arrayindex].varInstance[DFVarInstanceIndex].inuse := False;
      end;

    end;
  end
  else
    LogDatei.log('Error: Deleting not existing local variable: ' + Name, LLCritical);
end;


(*
function TOsDefinedFunction.addLocalVarValueString(name : string; value : string) : boolean;
var
  arraycounter : integer;
begin
  result := false;
  name := lowercase(name);
  if not locaVarExists(name) then
  begin
    result := true;
    arraycounter := length(DFLocalVarList);
    inc(arraycounter);
    SetLength(DFLocalVarList,arraycounter);
    DFLocalVarList[arraycounter-1].varName:=name;
    DFLocalVarList[arraycounter-1].varDataType :=dfpString;
    DFLocalVarList[arraycounter-1].varInstance[DFVarInstanceIndex].varValueString :=  value;
  end
  else LogDatei.log('Syntax Error: Double definition of local variable: '+name,LLCritical );
end;
*)

(*
function TOsDefinedFunction.addLocalVarValueBool(name : string; value : boolean) : boolean;
var
  arraycounter : integer;
begin
  result := false;
  if not locaVarExists(name) then
  begin
    result := true;
    arraycounter := length(DFLocalVarList);
    inc(arraycounter);
    SetLength(DFLocalVarList,arraycounter);
    DFLocalVarList[arraycounter-1].varName:=name;
    DFLocalVarList[arraycounter-1].varDataType :=dfpBoolean;
    DFLocalVarList[arraycounter-1].varValueBool := BoolToStr(value);
  end
  else LogDatei.log('Syntax Error: Double definition of local variable: '+name,LLCritical );
end;
*)

(*

function TOsDefinedFunction.addLocalVarValueList(name : string; value : tstringlist) : boolean;
var
  arraycounter : integer;
begin
  result := false;
  name := lowercase(name);
  if not locaVarExists(name) then
  begin
    result := true;
    arraycounter := length(DFLocalVarList);
    inc(arraycounter);
    SetLength(DFLocalVarList,arraycounter);
    DFLocalVarList[arraycounter-1].varName:=name;
    DFLocalVarList[arraycounter-1].varDataType :=dfpStringlist;
    if datatype = dfpStringlist then
      DFLocalVarList[arraycounter-1].varInstance[DFVarInstanceIndex].VarValueList := TStringlist.Create;
    DFLocalVarList[arraycounter-1].varInstance[DFVarInstanceIndex].VarValueList.Text := value.Text;
  end
  else LogDatei.log('Syntax Error: Double definition of local variable: '+name,LLCritical );
end;
*)

function TOsDefinedFunction.setLocalVarValueString(Name: string;
  Value: string): boolean;
var
  arrayindex: integer;
  callByReference: boolean;
  scopeindex, VarIndex: integer;
  varname: string;
begin
  Result := False;
  Name := lowercase(Name);
  arrayindex := localVarIndex(Name);
  if arrayindex >= 0 then
  begin
    DFLocalVarList[arrayindex].varInstance[DFVarInstanceIndex].inuse := True;

    if (DFLocalVarList[arrayindex].varName = Name) and
      (DFLocalVarList[arrayindex].varDataType = dfpString) then
    begin
      callByReference := DFLocalVarList[arrayindex].callByReference;
      if callByReference then
      begin
        // call by ref
        scopeindex := DFLocalVarList[arrayindex].varInstance[
          DFVarInstanceIndex].referencevarscopeindex;
        varname := DFLocalVarList[arrayindex].varInstance[
          DFVarInstanceIndex].referencevarname;
        if scopeindex = -1 then
        begin
          // global
          VarIndex := script.VarList.IndexOf(LowerCase(varname));
          script.ValuesList[VarIndex] := Value;
        end
        else if scopeindex >= 0 then
        begin
          // local func
          if definedFunctionArray[scopeindex] = self then
          begin
            // points to it self - make only sense on recursion
            // so we assume that we have a recursive call with call by reference
            // so what we need is the very first refrence
            scopeindex := DFLocalVarList[arrayindex].varInstance[
              0].referencevarscopeindex;
            // this should be -1 (global)
            varname := DFLocalVarList[arrayindex].varInstance[0].referencevarname;
            if scopeindex = -1 then
            begin
              // global
              VarIndex := script.VarList.IndexOf(LowerCase(varname));
              script.ValuesList[VarIndex] := Value;
            end
            else
            if not (definedFunctionArray[scopeindex] = self) then
            begin
              // points to another func
              definedFunctionArray[scopeindex].setLocalVarValueString(varname, Value);
            end
            else
            begin
              LogDatei.log('Critical Error in local function: ' +
                self.Name +
                ' while try to set the root value of the recursive by reference called var: ' +
                varname, LLcritical);
              script.ExtremeErrorLevel := LevelFatal;
              LogDatei.log('Error level set to fatal', LLCritical);
              scriptstopped := True;
            end;
          end
          else // points to another func
            definedFunctionArray[scopeindex].setLocalVarValueString(varname, Value);
        end
        else
        begin
          // invalid
          LogDatei.log('Internal Error: Invalid scopeindex in setLocalVarValueString',
            LLCritical);
        end;
        //setLength(DFLocalVarList[arrayindex].referencevar, length(value));
        //StrCopy(DFLocalVarList[arrayindex].referencevar, pchar(value));
        //String(DFLocalVarList[arrayindex].referencevar^):=value;
      end
      else // call by value
        DFLocalVarList[arrayindex].varInstance[DFVarInstanceIndex].varValueString :=
          Value;
      Result := True;
    end
    else
      LogDatei.log(
        'Internal Error: setLocalVarValueString: Unexpected type mismatch of local variable: '
        + Name, LLCritical);
  end
  else
    LogDatei.log('Syntax Error: setLocalVarValueString: No definition of local variable: '
      + Name, LLCritical);
end;

(*
function TOsDefinedFunction.setLocalVarValueBool(name : string; value : boolean) : boolean;
var
  arrayindex : integer;
begin
  result := false;
  arrayindex := locaVarIndex(name);
  if arrayindex >= 0 then
  begin
    if (DFLocalVarList[arrayindex].varName=name)
       and (DFLocalVarList[arrayindex].varDataType =dfpBoolean) then
    begin
      DFLocalVarList[arrayindex].varValueBool := BoolToStr(value);
      result := true;
    end
    else LogDatei.log('Internal Error: Unexpected type mismatch of local variable: '+name,LLCritical );
  end
  else LogDatei.log('Syntax Error: No definition of local variable: '+name,LLCritical );
end;
*)

function TOsDefinedFunction.setLocalVarValueList(Name: string;
  Value: TStringList): boolean;
var
  arrayindex: integer;
  callByReference: boolean;
  scopeindex, VarIndex: integer;
  varname: string;
begin
  Result := False;
  Name := lowercase(Name);
  arrayindex := localVarIndex(Name);
  if arrayindex >= 0 then
  begin
    DFLocalVarList[arrayindex].varInstance[DFVarInstanceIndex].inuse := True;

    if (DFLocalVarList[arrayindex].varName = Name) and
      (DFLocalVarList[arrayindex].varDataType = dfpStringlist) then
    begin
      callByReference := DFLocalVarList[arrayindex].callByReference;
      if callByReference then
      begin
        // call by ref
        scopeindex := DFLocalVarList[arrayindex].varInstance[
          DFVarInstanceIndex].referencevarscopeindex;
        varname := DFLocalVarList[arrayindex].varInstance[
          DFVarInstanceIndex].referencevarname;
        if scopeindex = -1 then
        begin
          // global
          VarIndex := script.listOfStringLists.IndexOf(LowerCase(varname));
          script.ContentOfStringLists.Items[VarIndex] := Value;
        end
        else if scopeindex >= 0 then
        begin
          // local func
          if definedFunctionArray[scopeindex] = self then
          begin
            // points to it self - make only sense on recursion
            // so we assume that we have a recursive call with call by reference
            // so what we need is the very first refrence
            scopeindex := DFLocalVarList[arrayindex].varInstance[
              0].referencevarscopeindex;
            // this should be -1 (global)
            varname := DFLocalVarList[arrayindex].varInstance[0].referencevarname;
            if scopeindex = -1 then
            begin
              // global
              VarIndex := script.VarList.IndexOf(LowerCase(varname));
              script.ContentOfStringLists.Items[VarIndex] := Value;
            end
            else
              LogDatei.log('In local function: ' + self.Name +
                ' Error while try to set the root value of the recursive by reference called var: '
                + varname,
                LLcritical);
            ;
          end
          else // points to a other func
            definedFunctionArray[scopeindex].setLocalVarValuelist(varname, Value);

        end
        else
        begin
          // invalid
          LogDatei.log('Internal Error: Invalid scopeindex in setLocalVarValueList',
            LLCritical);
        end;
      end
      else // call by value
        DFLocalVarList[arrayindex].varInstance[DFVarInstanceIndex].VarValueList.Text :=
          Value.Text;
      Result := True;
    end
    else
      LogDatei.log(
        'Internal Error: setLocalVarValueList: Unexpected type mismatch of local variable: '
        + Name, LLCritical);
  end
  else
    LogDatei.log('Syntax Error: setLocalVarValueList: No definition of local variable: '
      + Name, LLCritical);
end;

(*
function TOsDefinedFunction.getLocalVarValueString(name : string) : string;
var
  arraycounter,i : integer;
  found : boolean;
begin
  result := '';
  i := 0;
  found := false;
  arraycounter := length(DFLocalVarList);
  if arraycounter > 0 then
    repeat
      if lowercase(DFLocalVarList[i].varName) = lowercase(name) then
      begin
        if DFLocalVarList[i].varDataType = dfpString then
          result := DFLocalVarList[i].varValueString;
        found := true;
      end;
      inc(i);
    until (i >= arraycounter) or found;
end;
*)
function TOsDefinedFunction.getLocalVarValueString(Name: string): string;
var
  arrayindex: integer;
  callByReference: boolean;
  scopeindex, VarIndex, instanceIndex: integer;
  varname: string;
begin
  Result := '';
  Name := lowercase(Name);
  arrayindex := localVarNameIndex(Name);
  if arrayindex >= 0 then
  begin
    if (DFLocalVarList[arrayindex].varName = Name) and
      (DFLocalVarList[arrayindex].varDataType = dfpString) then
    begin
      callByReference := DFLocalVarList[arrayindex].callByReference;
      if callByReference then
      begin
        // call by ref
        scopeindex := DFLocalVarList[arrayindex].varInstance[
          DFVarInstanceIndex].referencevarscopeindex;
        varname := DFLocalVarList[arrayindex].varInstance[
          DFVarInstanceIndex].referencevarname;
        LogDatei.log_prog('getLocalVarValueString: ref1: scope: ' +
          IntToStr(scopeindex) + ' varname: ' + varname, LLDebug2);
        if scopeindex = -1 then
        begin
          // global
          VarIndex := script.VarList.IndexOf(LowerCase(varname));
          Result := script.ValuesList[VarIndex];
        end
        else if scopeindex >= 0 then
        begin
          // local func
          if definedFunctionArray[scopeindex] = self then
          begin
            // points to it self - make only sense on recursion
            // so we assume that we have a recursive call with call by reference
            // so what we need is the very first refrence
            scopeindex := DFLocalVarList[arrayindex].varInstance[
              0].referencevarscopeindex;
            // this should be -1 (global)
            varname := DFLocalVarList[arrayindex].varInstance[0].referencevarname;
            LogDatei.log_prog('getLocalVarValueString: ref2: scope: ' +
              IntToStr(scopeindex) + ' varname: ' + varname, LLDebug2);
            if scopeindex = -1 then
            begin
              // global
              VarIndex := script.VarList.IndexOf(LowerCase(varname));
              Result := script.ValuesList[VarIndex];
            end
            else
            if not (definedFunctionArray[scopeindex] = self) then
            begin
              // points to a other func
              Result := definedFunctionArray[scopeindex].getLocalVarValueString(
                varname);
            end
            else
            begin
              LogDatei.log('Critical Error: In local function: ' +
                self.Name +
                ' Error while try to retrieve the root value of the recursive by reference called var: '
                + varname, LLcritical);
              script.ExtremeErrorLevel := LevelFatal;
              LogDatei.log('Error level set to fatal', LLCritical);
              //ActionResult := tsrFatalError;
              scriptstopped := True;
            end;
          end
          else // points to a other func
            Result := definedFunctionArray[scopeindex].getLocalVarValueString(varname);
        end
        else
        begin
          // invalid
          LogDatei.log('Internal Error: Invalid scopeindex in getLocalVarValueString',
            LLCritical);
        end;
        //setLength(DFLocalVarList[arrayindex].referencevar, length(value));
        //StrCopy(DFLocalVarList[arrayindex].referencevar, pchar(value));
        //String(DFLocalVarList[arrayindex].referencevar^):=value;
      end
      else // call by value
        //if Assigned(pointer(DFLocalVarList[arrayindex].varInstance[DFVarInstanceIndex].varValueString)) then
        try
          Result := DFLocalVarList[arrayindex].varInstance[
            DFVarInstanceIndex].varValueString;
        except
          begin
            Result := '';
            LogDatei.log(
              'Not assinged varInstance for name: ' + Name +
              ' in getLocalVarValueString. Default to empty string',
              LLError);
          end;
        end;
    end
    else
      LogDatei.log(
        'Internal Error: getLocalVarValueString: Unexpected type mismatch of local variable: '
        + Name, LLCritical);
  end
  else
    LogDatei.log('Syntax Error: getLocalVarValueString: No definition of local variable: '
      + Name, LLCritical);
end;


function TOsDefinedFunction.getLocalVarValueList(Name: string): TStringList;
var
  arraycounter, i: integer;
  arrayindex: integer;
  callByReference: boolean;
  scopeindex, VarIndex: integer;
  varname: string;
begin
  Result := TStringList.Create;
  Name := lowercase(Name);
  arrayindex := localVarIndex(Name);
  if arrayindex >= 0 then
  begin
    if (DFLocalVarList[arrayindex].varName = Name) and
      (DFLocalVarList[arrayindex].varDataType = dfpStringlist) then
    begin
      callByReference := DFLocalVarList[arrayindex].callByReference;
      if callByReference then
      begin
        // call by ref
        scopeindex := DFLocalVarList[arrayindex].varInstance[
          DFVarInstanceIndex].referencevarscopeindex;
        varname := DFLocalVarList[arrayindex].varInstance[
          DFVarInstanceIndex].referencevarname;
        if scopeindex = -1 then
        begin
          // global
          VarIndex := script.listOfStringLists.IndexOf(LowerCase(varname));
          Result := TStringList(script.ContentOfStringLists.Items[VarIndex]);
        end
        else if scopeindex >= 0 then
        begin
          // local func
          if definedFunctionArray[scopeindex] = self then
          begin
            // points to it self - make only sense on recursion
            // so we assume that we have a recursive call with call by reference
            // so what we need is the very first refrence
            scopeindex := DFLocalVarList[arrayindex].varInstance[
              0].referencevarscopeindex;
            // this should be -1 (global)
            varname := DFLocalVarList[arrayindex].varInstance[0].referencevarname;
            if scopeindex = -1 then
            begin
              // global
              VarIndex := script.VarList.IndexOf(LowerCase(varname));
              Result := TStringList(script.ContentOfStringLists.Items[VarIndex]);
            end
            else
              LogDatei.log('In local function: ' + self.Name +
                ' Error while try to retrieve the root valuelist of the recursive by reference called var: '
                + varname, LLcritical);
            ;
          end
          else // points to a other func
            Result := definedFunctionArray[scopeindex].getLocalVarValuelist(varname);
        end
        else
        begin
          // invalid
          LogDatei.log('Internal Error: Invalid scopeindex in getLocalVarValueList',
            LLCritical);
        end;
      end
      else // call by value
        Result := DFLocalVarList[arrayindex].varInstance[
          DFVarInstanceIndex].VarValueList;
    end
    else
      LogDatei.log(
        'Internal Error: getLocalVarValueList: Unexpected type mismatch of local variable: '
        + Name, LLCritical);
  end
  else
    LogDatei.log('Syntax Error: getLocalVarValueList: No definition of local variable: '
      + Name, LLCritical);
end;


(*
function TOsDefinedFunction.getLocalVarValueBool(name : string) : boolean;
var
  arraycounter,i : integer;
  found : boolean;
begin
  result := false;
  i := 0;
  found := false;
  arraycounter := length(DFLocalVarList);
  if arraycounter > 0 then
    repeat
      if lowercase(DFLocalVarList[i].varName) = lowercase(name) then
      begin
        if DFLocalVarList[i].varDataType = dfpBoolean then
          result := StrToBool(DFLocalVarList[i].varValueBool);
        found := true;
      end;
      inc(i);
    until (i >= arraycounter) or found;
end;
*)



function TOsDefinedFunction.getLocalVarDatatype(Name: string): TosdfDataTypes;
var
  arrayindex: integer;
begin
  Result := dfpString;
  Name := lowercase(Name);
  arrayindex := localVarIndex(Name);
  if arrayindex >= 0 then
  begin
    Result := DFLocalVarList[arrayindex].varDataType;
  end
  else
    LogDatei.log('Syntax Error: No definition of local variable: ' + Name, LLCritical);
end;


function TOsDefinedFunction.createVarInstance(varindex: integer): boolean;
  // create a new Instance of local Variable
begin
  if DFVarInstanceIndex >= 0 then
  begin
    // we expect that there is no instance of this var with  DFVarInstanceIndex
    if length(DFLocalVarList[varindex].varInstance) = DFVarInstanceIndex then
    begin
      SetLength(DFLocalVarList[varindex].varInstance, DFVarInstanceIndex + 1);
      DFLocalVarList[varindex].varInstance[DFVarInstanceIndex].VarValueList :=
        TStringList.Create;
      DFLocalVarList[varindex].varInstance[DFVarInstanceIndex].inuse := True;
      Result := True;
    end
    else
    begin
      if DFVarInstanceIndex > 0 then
      begin
        LogDatei.log('instance of var num: ' + IntToStr(varindex) +
          ' with index: ' + IntToStr(DFVarInstanceIndex) + ' still exists.', LLError);
        Result := False;
      end
      else
        Result := True;
    end;
  end
  else
  ; // do nothing and wait for creatAllInstances
end;

function TOsDefinedFunction.createAllVarInstances: boolean;
  // create a new Instances of local Variables  and inc DFVarInstanceIndex
var
  varcounter: integer;
  i: integer;
begin
  Inc(DFVarInstanceIndex);
  varcounter := length(DFLocalVarList);
  Result := True;
  for i := 0 to varcounter - 1 do
  begin
    if not createVarInstance(i) then
      Result := False;
  end;
end;

function TOsDefinedFunction.destroyVarInstance(varindex: integer): boolean;
  // release the var instance
begin
  // we expect that there is a instance of this var with  DFVarInstanceIndex
  if length(DFLocalVarList[varindex].varInstance) = DFVarInstanceIndex + 1 then
  begin
    DFLocalVarList[varindex].varInstance[DFVarInstanceIndex].VarValueList.Free;
    SetLength(DFLocalVarList[varindex].varInstance, DFVarInstanceIndex);
    Result := True;
  end
  else
  begin
    LogDatei.log('instance of var num: ' + IntToStr(varindex) +
      ' with index: ' + IntToStr(DFVarInstanceIndex) + ' not exists.', LLError);
    Result := False;
  end;
end;

function TOsDefinedFunction.destroyAllVarInstances: boolean;
  // release the var instances (DFVarInstanceIndex) and dec DFVarInstanceIndex
var
  varcounter: integer;
  i: integer;
begin
  varcounter := length(DFLocalVarList);
  Result := True;
  for i := 0 to varcounter - 1 do
  begin
    if not destroyVarInstance(i) then
      Result := False;
  end;
  Dec(DFVarInstanceIndex);
end;

function TOsDefinedFunction.parseCallParameter(paramline: string;
  var remaining: string; var errorstr: string; NestLevel: integer;
  inDefFuncIndex: integer): boolean;
var
  ParamStr: string;
  paramstrvalue: string = '';
  paramboolvalue: boolean;
  paramlistvalue: TXStringlist;
  paramcounter: integer;
  syntax_ok, endOfParamlist: boolean;
  section: TuibIniScript;
  NestingLevel: integer = 0;
  varindex: integer = 0;
  inputstr: string;

begin
  parseCallParameter := False;
  syntax_ok := True;
  endOfParamlist := False;
  paramcounter := -1;
  if not skip('(', paramline, remaining, errorstr) then
  begin
    // ( expected
    errorstr := errorstr + ' ( expected';
    syntax_ok := False;
  end
  else
  begin
    if DFparamCount = 0 then
    begin
      if not skip(')', remaining, remaining, errorstr) then
      begin
        // ( expected
        errorstr := errorstr + ' ) expected';
        syntax_ok := False;
      end;
    end
    else
    begin
      while syntax_ok and not endOfParamlist do
      begin
        if remaining = '' then
        begin
          errorstr := errorstr + ' Parameter and ")" expected';
          syntax_ok := False;
        end
        else
        begin
          Inc(paramcounter);
          inputstr := remaining;
          ParamStr := inputstr;
          if DFparamList[paramcounter].callByReference then
          begin
            // check if this should be the last parameter and we expect a ')' at the end
            if paramcounter = DFparamCount - 1 then
              GetWord(inputstr, ParamStr, remaining, [')'])
            else // this should be not the last parameter and we expect a ','
              GetWord(inputstr, ParamStr, remaining, [',']);

            ParamStr := Trim(ParamStr);

            // call by reference
            case DFparamList[paramcounter].paramDataType of
              dfpString:
              begin
                if not isVisibleStringVar(ParamStr) then
                begin
                  // parameter type mismatch
                  syntax_ok := False;
                  errorstr :=
                    errorstr + 'Error: String variable expected, but: "' +
                    ParamStr + '" is not a visible string variable';
                end
                else
                begin
                  // we got a string variable- make it to a local var
                  DFparamList[
                    paramcounter].paramValueBuf.referencevarname :=
                    ParamStr;
                  //DFLocalVarList[paramcounter].varInstance[DFVarInstanceIndex].referencevarname:= paramstr;
                  if isVisibleLocalVar(ParamStr, varindex) then
                  begin
                    // is local var
                    DFparamList[
                      paramcounter].paramValueBuf.referencevarscopeindex := varindex;
                    //DFLocalVarList[paramcounter].varInstance[DFVarInstanceIndex].referencevarscopeindex := varindex;
                    LogDatei.log(
                      'Paramnr: ' + IntToStr(paramcounter) +
                      ' is a reference to local: ' + ParamStr, LLDebug2);
                  end
                  else if isVisibleGlobalStringVar(
                    ParamStr, varindex) then
                  begin
                    // is global var : -1
                    DFparamList[
                      paramcounter].paramValueBuf.referencevarscopeindex := -1;
                    //DFLocalVarList[paramcounter].varInstance[DFVarInstanceIndex].referencevarscopeindex := -1;
                    LogDatei.log(
                      'Paramnr: ' + IntToStr(paramcounter) +
                      ' is a reference to global: ' + ParamStr, LLDebug2);
                  end
                  else
                  begin
                    // failed
                    LogDatei.log(
                      'Error: Did not found the reference to: ' + ParamStr, LLError);
                  end;
                end;
              end;
              dfpStringlist:
              begin
                if not isVisibleStringlistVar(ParamStr) then
                begin
                  // parameter type mismatch
                  syntax_ok := False;
                  errorstr :=
                    errorstr + 'Error: StringList expression expected, but: ' +
                    ParamStr + ' gives no stringlist';
                end
                else
                begin
                  // we got a stringlist - make it to a local var
                  DFparamList[
                    paramcounter].paramValueBuf.referencevarname :=
                    ParamStr;
                  //DFLocalVarList[paramcounter].varInstance[DFVarInstanceIndex].referencevarname:= paramstr;
                  if isVisibleLocalVar(ParamStr, varindex) then
                  begin
                    // is local var
                    DFparamList[
                      paramcounter].paramValueBuf.referencevarscopeindex := varindex;
                    //DFLocalVarList[paramcounter].varInstance[DFVarInstanceIndex].referencevarscopeindex := varindex;
                    LogDatei.log(
                      'Paramnr: ' + IntToStr(paramcounter) +
                      ' is a reference to local stringlist: ' + ParamStr, LLDebug2);
                  end
                  else if isVisibleGlobalStringlistVar(ParamStr,
                    varindex) then
                  begin
                    // is global var : -1
                    DFparamList[
                      paramcounter].paramValueBuf.referencevarscopeindex := -1;
                    //DFLocalVarList[paramcounter].varInstance[DFVarInstanceIndex].referencevarscopeindex := -1;
                    LogDatei.log(
                      'Paramnr: ' + IntToStr(paramcounter) +
                      ' is a reference to global stringlist: ' + ParamStr, LLDebug2);
                  end
                  else
                  begin
                    // failed
                    LogDatei.log(
                      'Error: Did not found the reference to: ' + ParamStr, LLError);
                  end;
                end;
              end;
            end; // end case
          end
          else
          begin
            // call by value
            case DFparamList[paramcounter].paramDataType of
              dfpString:
              begin
                if not Script.EvaluateString(ParamStr, remaining,
                  paramstrvalue, errorstr, NestLevel, inDefFuncIndex) then
                begin
                  // parameter type mismatch
                  syntax_ok := False;
                  errorstr :=
                    errorstr + 'Error: String expression expected, but: ' +
                    ParamStr + ' gives no string';
                end
                else
                begin
                  // we got a string - make it to a local var
                  DFparamList[
                    paramcounter].paramValueBuf.varValueString :=
                    paramstrvalue;
                  //DFLocalVarList[paramcounter].varInstance[DFVarInstanceIndex].varValueString:=paramstrvalue;
                  LogDatei.log('Paramnr: ' + IntToStr(
                    paramcounter) + ' is the string: ' + paramstrvalue, LLDebug2);
                end;
              end;
              dfpStringlist:
              begin
                section := TuibIniScript.Create;
                paramlistvalue := TXStringlist.Create;
                if not Script.produceStringList(
                  section, ParamStr, remaining, paramlistvalue, errorstr,
                  NestLevel, inDefFuncIndex) then
                begin
                  // parameter type mismatch
                  syntax_ok := False;
                  errorstr :=
                    errorstr + 'Error: StringList expression expected, but: ' +
                    ParamStr + ' gives no stringlist';
                end
                else
                begin
                  // we got a stringlist - make it to a local var
                  DFparamList[
                    paramcounter].paramValueBuf.VarValueList.Text := paramlistvalue.Text;
                  //DFLocalVarList[paramcounter].varInstance[DFVarInstanceIndex].VarValueList.text := paramlistvalue.Text;
                  LogDatei.log('Paramnr: ' + IntToStr(
                    paramcounter) + ' is a stringlist', LLDebug2);
                end;
                FreeAndNil(section);
                FreeAndNil(paramlistvalue);
              end;
              (*
              dfpBoolean :    begin
                                if not Script.EvaluateBoolean(paramstr,r,paramboolvalue,NestingLevel,errorstr) then
                                begin
                                  // parameter type mismatch
                                  syntax_ok := false;
                                  errorstr := errorstr + 'Error: boolean expression expected, but: '+paramstr+' gives no boolean';
                                end
                                else
                                begin
                                  // we got a bool - make it to a local var
                                  DFLocalVarList[paramcounter].varValueBool:=paramboolvalue;
                                  LogDatei.log('Paramnr: '+inttostr(paramcounter)+' is boolean: '+BoolToStr(paramboolvalue),LLDebug2);
                                end;
                              end;
                              *)
            end; // end case
          end; // reference or value
          if syntax_ok then
          begin
            if not skip(',', remaining, remaining, errorstr) then
              if skip(')', remaining, remaining, errorstr) then
                endOfParamlist := True
              else
              begin
                // syntax error
                errorstr := errorstr + ' , or ) expected.';
                syntax_ok := False;
              end;
          end; //syntax ok
        end; // remaining <> ''
      end; // while next paramstr
    end; // DFparamCount > 0;
  end; // start with '('
  if syntax_ok then
    parseCallParameter := True;
end;


function TOsDefinedFunction.copyParamBufToLocalVars: boolean;
var
  varcounter, i, locindex, parami, paramcounter: integer;
  actname: string;
  actDataType: TosdfDataTypes;
begin
  paramcounter := DFparamCount;
  for parami := 0 to paramcounter - 1 do
  begin
    if Assigned(DFparamList[parami].paramValueBuf.VarValueList) then
    begin
      LogDatei.log('copyParamBufToLocalVars: copy param: ' + IntToStr(parami), LLDebug2);
      DFLocalVarList[parami].varInstance[DFVarInstanceIndex].varValueString :=
        DFparamList[parami].paramValueBuf.varValueString;
      DFLocalVarList[parami].varInstance[DFVarInstanceIndex].VarValueList.Text :=
        DFparamList[parami].paramValueBuf.VarValueList.Text;
      DFLocalVarList[parami].varInstance[DFVarInstanceIndex].referencevarscopeindex :=
        DFparamList[parami].paramValueBuf.referencevarscopeindex;
      DFLocalVarList[parami].varInstance[DFVarInstanceIndex].referencevarname :=
        DFparamList[parami].paramValueBuf.referencevarname;
    end;
  end;
end;

// run the function
function TOsDefinedFunction.call(paramline: string; var remaining: string;
  var NestLevel: integer): boolean;
var
  errorstr: string;
  section: TWorkSection;
  callingsection: TWorkSection;
  sectionresult: TSectionResult;
  funcindex: integer;
  searchindex: integer;
  searchDFName: string;
  indexofcalledfunc, indexofcallingfunc: integer;
begin
  call := False;
  Inc(inDefFuncLevel);
  FuncIndex := definedFunctionNames.IndexOf(LowerCase(DFName));
  indexofcalledfunc := FuncIndex;
  indexofcallingfunc := inDefFuncIndex;
  //inDefFuncIndex := tmpi2;
  //inDefFuncIndex := tmpi1;
  LogDatei.log('We are coming from function with index: ' + IntToStr(
    inDefFuncIndex) + ' (-1 = base)', LLDebug2);
  LogDatei.log('We enter the defined function: ' + DFName + ' with ' +
    IntToStr(DFcontent.Count) + ' lines. inDefFuncLevel: ' +
    IntToStr(inDefFuncLevel) + ' and index: ' + IntToStr(Funcindex), LLDebug2);
  LogDatei.log_prog('paramline: ' + paramline + ' remaining: ' +
    remaining + ' Nestlevel: ' + IntToStr(NestLevel), LLDebug2);
  DFActive := True;
  //inc(inDefinedFuncNestCounter);
  //definedFunctionsCallStack.Append(InttoStr(DFIndex));


  //parse parameter
  // The varnames on the param line are in the name space of the calling function
  if not parseCallParameter(paramline, remaining, errorstr, NestLevel,
    indexofcallingfunc) then
  begin
    // parse parameter failed
    LogDatei.log('Syntax Error: Parameter parsing failed: ' + errorstr, LLCritical);
  end
  else
  begin
 if not (Script.testSyntax and (nestlevel > 20)) then
  begin
    try

      //now set inDefFuncIndex to the called function
      inDefFuncIndex := indexofcalledfunc;
      // inc var instance counter for recursive calls
      createAllVarInstances;
      LogDatei.log_prog('DFVarInstanceIndex: ' + IntToStr(DFVarInstanceIndex) +
        ' inDefinedFuncNestCounter: ' + IntToStr(inDefinedFuncNestCounter), LLDebug);
      copyParamBufToLocalVars;
      LogDatei.log_prog('definedFunctionsCallStack.Append ... ', LLDebug2);
      definedFunctionsCallStack.Append(IntToStr(DFIndex));
      LogDatei.log_prog('definedFunctionsCallStack.Appended', LLDebug2);
      (*
      LogDatei.log_prog('inDefFuncIndex: '+inttostr(inDefFuncIndex), LLDebug2);
      inDefFuncIndex := indexofcalledfunc;
      *)
      LogDatei.log_prog('inDefFuncIndex: ' + IntToStr(inDefFuncIndex), LLDebug2);
    except
      on e: Exception do
      begin
        LogDatei.log_prog('Exception: osd call: init vars: ' + e.message, LLError);
        raise;
      end;
    end;
    try
      // run the body of the function
      LogDatei.log_prog('prepare run the body of the function ... ', LLDebug2);
      Inc(inDefinedFuncNestCounter);
      if inDefinedFuncNestCounter < 100 then
      begin
      section := TWorkSection.Create(Nestlevel, nil);
      callingsection := TWorkSection.Create(0, nil);
      section.Assign(DFcontent);
      LogDatei.log_prog('start run the body of the function ... ', LLDebug2);
      sectionresult := script.doAktionen(section, callingsection);
      LogDatei.log_prog('finished run the body of the function ... ', LLDebug2);
      Nestlevel := section.NestingLevel;
      call := True;
      case DFResultType of
        dfpString:
        begin
          DFResultString := getLocalVarValueString('$result$');
        end;
        dfpStringlist:
        begin
          DFResultList.Text := getLocalVarValueList('$result$').Text;
        end;
        //dfpBoolean :    begin
        //                  DFResultBool := StrToBool(getLocalVarValueString('$result$'));
        //                end;
      end;
      end
      else
      begin
        LogDatei.log('Error: recursion depth >= 100 not allowed - stopping recursion',LLerror);
      end;
      Dec(inDefinedFuncNestCounter);
      definedFunctionsCallStack.Delete(definedFunctionsCallStack.Count - 1);
      // dec var instance counter for recursive calls
      destroyAllVarInstances;
      LogDatei.log_prog('DFVarInstanceIndex: ' + IntToStr(DFVarInstanceIndex) +
        ' inDefinedFuncNestCounter: ' + IntToStr(inDefinedFuncNestCounter), LLDebug);

    except
      on e: Exception do
      begin
        LogDatei.log_prog('Exception: osd call: run body: ' + e.message, LLError);
        raise;
      end;
    end;
  end
  else
  begin
    LogDatei.log('Error:In testsyntax mode recursion depth >= 20 not allowed - stopping recursion',LLerror);
    call := True;
    //if DFResultType = dfpVoid then remaining := '';
  end;
  end;
  // we leave a defined function
  if DFVarInstanceIndex = -1 then
  begin
    // free the local Vars - leave params + $result$
    case DFResultType of
      dfpString: SetLength(DFLocalVarList, DFparamCount + 1);
      dfpStringlist: SetLength(DFLocalVarList, DFparamCount + 1);
      dfpVoid: SetLength(DFLocalVarList, DFparamCount); // no $result$ here
    end;
  end;


  DFActive := False;
  Dec(inDefFuncLevel);
  searchindex := definedFunctionsCallStack.Count - 1;
  if searchindex > -1 then
  begin
    //searchDFName :=  definedFunctionsCallStack.Strings[searchindex];
    //inDefFuncIndex := definedFunctionNames.IndexOf(searchDFName)
    inDefFuncIndex := StrToInt(definedFunctionsCallStack.Strings[searchindex]);
  end
  else
    inDefFuncIndex := -1;
  //logdatei.log('We leave the defined function: inDefFunc3: '+IntToStr(inDefFunc3),LLInfo);
  LogDatei.log('We leave the defined function: ' + DFName +
    ' ; inDefFuncLevel: ' + IntToStr(inDefFuncLevel), LLDebug2);

end;

(*
function TOsDefinedFunction.getLocalVarReference(name : string) : pointer;
var
  index : integer;
begin
  index := locaVarIndex(name);
  result := DFLocalVarList[index].referencevar;
end;
*)

function createListOfVisibleParents: TStringList;
var
  fparent, fname: string;
  searchfinished: boolean;
  searchindex, index: integer;
begin
  Result := TStringList.Create;
  searchfinished := False;
  // first add the name of the running local function
  searchindex := definedFunctionsCallStack.Count - 1;
  index := StrToInt(definedFunctionsCallStack.Strings[searchindex]);
  fname := definedFunctionArray[index].Name;
  Result.Append(fname);
  repeat
    index := StrToInt(definedFunctionsCallStack.Strings[searchindex]);
    fname := definedFunctionArray[index].Name;
    if Result.IndexOf(fname) >= 0 then
    begin
      // the akt function is the running one or a sub func
      fparent := definedFunctionArray[index].ParentFunc;
      Result.Append(fparent);
      if fparent = 'global' then
        searchfinished := True;
    end;
    Dec(searchindex);
    if searchindex < 0 then
      searchfinished := True;
  until searchfinished;
end;


function isVisibleLocalVar(varname: string; var index: integer): boolean;

var
  found, searchfinished: boolean;
  searchindex: integer;
  parentlist: TStringList;
  searchedindexes: TStringList;

begin
  Result := False;
  found := False;
  searchfinished := False;
  if varname <> '' then
  begin
    searchedindexes := TStringList.Create;
    LogDatei.log_prog('Search local var: ' + varname + ' with inDefFuncIndex: ' +
      IntToStr(inDefFuncIndex) + ' and inDefinedFuncNestCounter: ' +
      IntToStr(inDefinedFuncNestCounter), LLDebug2);
    if inDefinedFuncNestCounter > 0 then
    begin
      // we are in a local function
      // first guess: it is local to the active local function
      LogDatei.log_prog('Search local var: ' + varname + ' with inDefFuncIndex: ' +
        IntToStr(inDefFuncIndex) + ' and Name: ' +
        definedFunctionArray[inDefFuncIndex].Name,
        LLDebug2);
      if definedFunctionArray[inDefFuncIndex].localVarNameExists(varname) then
        found := True;
      searchedindexes.Add(IntToStr(inDefFuncIndex));
    end;
    if found then
    begin
      if definedFunctionArray[inDefFuncIndex].localVarExists(varname) then
      begin
        index := inDefFuncIndex;
        LogDatei.log_prog('Found var: ' + varname + ' as local (1) in function ' +
          definedFunctionArray[index].Name + ' with index: ' +
          IntToStr(index), LLDebug2);
      end
      else
      begin
        index := inDefFuncIndex;
        LogDatei.log_prog('Found var: ' + varname +
          ' as local (1) with no instance in function ' +
          definedFunctionArray[index].Name + ' with index: ' +
          IntToStr(index), LLDebug2);
      end;
    end
    else
    begin
      if inDefinedFuncNestCounter > 0 then
      begin
        // we are in a local function
        // second guess: it is local to parents to the active local function
        parentlist := TStringList.Create;
        parentlist.Text := createListOfVisibleParents.Text;
        searchfinished := False;
        searchindex := definedFunctionsCallStack.Count - 1;
        repeat
          index := StrToInt(definedFunctionsCallStack.Strings[searchindex]);
          // avoid searches that are done right now
          if searchedindexes.IndexOf(IntToStr(index)) = -1 then
          begin
            // we did not this search before
            searchedindexes.Add(IntToStr(index));
            if parentlist.IndexOf(definedFunctionArray[index].Name) >= 0 then
            begin
              // local variable of this function are visible (global to this function)
              LogDatei.log_prog('Search local var: ' + varname +
                ' with Index: ' + IntToStr(index) + ' and Name: ' +
                definedFunctionArray[index].Name, LLDebug2);
              if (definedFunctionArray[index].Active and
                definedFunctionArray[index].localVarExists(varname)) then
                found := True;
            end;
          end;
          Dec(searchindex);
          if searchindex < 0 then
            searchfinished := True;
        until found or searchfinished;
        if found then
        begin
          LogDatei.log_prog('Found var: ' + varname + ' as local (2) in function ' +
            definedFunctionArray[index].Name + ' with index: ' +
            IntToStr(index), LLDebug2);
        end
        else
        begin
          LogDatei.log_prog('Did not find a local var ' + varname +
            ' as local (2) in function ' + definedFunctionArray[index].Name +
            ' with index: ' + IntToStr(index), LLDebug2);
        end;
      end;
    end;
    Result := found;
    searchedindexes.Free;
  end;
end;

function getVisibleLocalStringVarNameValueList: TStringList;
var
  resultentry, varname: string;
  searchfinished: boolean;
  searchindex, index: integer;
  parentlist: TStringList;
  i: integer;

begin
  Result := TStringList.Create;
  if inDefinedFuncNestCounter > 0 then
  begin
    // we are in a local function
    parentlist := TStringList.Create;
    parentlist.Text := createListOfVisibleParents.Text;
    searchfinished := False;
    searchindex := definedFunctionsCallStack.Count - 1;
    repeat
      index := StrToInt(definedFunctionsCallStack.Strings[searchindex]);
      if parentlist.IndexOf(definedFunctionArray[index].Name) >= 0 then
      begin
        // local variable of this function are visible (global to this function)
        if (definedFunctionArray[index].Active) then
        begin
          for i := 0 to length(definedFunctionArray[index].DFLocalVarList) - 1 do
            if definedFunctionArray[index].DFLocalVarList[i].varDataType = dfpString then
            begin
              varName := definedFunctionArray[index].DFLocalVarList[i].varName;
              resultentry := varName + '=';
              resultentry := resultentry +
                definedFunctionArray[index].getLocalVarValueString(varname);
              Result.Append(resultentry);
            end;
        end;
      end;
      Dec(searchindex);
      if searchindex < 0 then
        searchfinished := True;
    until searchfinished;
  end;
end;

function isVisibleGlobalStringVar(varname: string; var index: integer): boolean;
begin
  index := script.VarList.IndexOf(LowerCase(VarName));
  if index >= 0 then
    Result := True;
end;

function isVisibleGlobalStringlistVar(varname: string; var index: integer): boolean;
begin
  index := script.listOfStringLists.IndexOf(LowerCase(VarName));
  if index >= 0 then
    Result := True;
end;


function isVisibleStringVar(varname: string): boolean;
var
  funcindex: integer;
begin
  Result := False;
  if isVisibleLocalVar(VarName, funcindex) then
    if definedFunctionArray[FuncIndex].getLocalVarDatatype(varname) = dfpString then
      Result := True;
  if script.VarList.IndexOf(LowerCase(VarName)) >= 0 then
    Result := True;
end;

function isVisibleStringlistVar(varname: string): boolean;
var
  funcindex: integer;
begin
  Result := False;
  if isVisibleLocalVar(VarName, funcindex) then
    if definedFunctionArray[FuncIndex].getLocalVarDatatype(varname) = dfpStringlist then
      Result := True;
  if script.listOfStringLists.IndexOf(LowerCase(VarName)) >= 0 then
    Result := True;
end;

function getFirstLineAfterEndFunc(list: TStringList; startline: integer): integer;
var
  i: integer;
  inDefFunc: integer = 0;
  line: string;
  stopsearch: boolean = False;
begin
  Result := startline;
  if pos('endfunc', lowercase(list.Text)) > 0 then
  begin
    // quick test tells us that we have to make a closer look
    i := startline;
    repeat
      line := trim(lowercase(list.Strings[i]));
      if pos('deffunc', line) > 0 then
        Inc(inDefFunc)
      else if (inDefFunc > 0) and (pos('endfunc', line) > 0) then
      begin
        // endfunc
        Dec(inDefFunc);
        Result := i;
      end
      else if (inDefFunc = 0) and (pos('[', line) = 1) and
        (pos(']', line) = length(line)) and (length(line) > 2) then
      begin
        // section header found outside of local function
        stopsearch := True;
      end;
      Inc(i);
    until (i >= list.Count) or stopsearch;
  end;
end;

function IsEndOfLocalFunction(const s: string): boolean;
var
  TestS: string = '';
begin
  Result := False;
  if inDefinedFuncNestCounter > 0 then
  begin
    if trim(lowercase(s)) = 'endfunc' then
      Result := True;
  end;
end;


(*
function getReferenceToVar(varname: string) : pointer;
var
  funcindex, varindex : integer;
begin
   result := nil;
   if isVisibleLocalVar(VarName,funcindex)  then
     result := definedFunctionArray[FuncIndex].getLocalVarReference(VarName);
   varindex := script.VarList.IndexOf (LowerCase (VarName));
   if varindex >= 0 then
   begin
       result := pchar(script.valuesList[VarIndex]);
   end;
end;
*)

procedure freeDefinedFunctions;
var
  i: integer;
begin
  for i := definedFunctionNames.Count - 1 downto 0 do
  begin
    definedFunctionArray[i].Destroy;
  end;
  SetLength(definedFunctionArray, 0);
  definedFunctioncounter := 0;
  definedFunctionNames.Clear;
  definedFunctionsCallStack.Clear;
end;

function addLoopvarToVarList(const loopvar: string; var errmesg: string): boolean;
var
  funcindex: integer;
begin
  Result := False;
  // in local function ?
  if inDefinedFuncNestCounter > 0 then
  begin
    // get the function we are in
    funcindex := StrToInt(definedFunctionsCallStack.Strings[
      definedFunctionsCallStack.Count - 1]);
    if definedFunctionArray[funcindex].addLocalVar(
      lowercase(loopvar), dfpString, False) then
    begin
      LogDatei.log('Defined local string var: ' + lowercase(loopvar) +
        ' in local function: ' + definedFunctionArray[funcindex].Name +
        ' with index: ' + IntToStr(funcindex), LLDebug2);
      Result := True;
    end
    else
    begin
      errmesg := 'Name: ' + loopvar + ' is already in use in local func: ' +
        definedFunctionArray[funcindex].Name;
    end;
  end
  // not in local function - make it global
  else
  begin
    if Script.VarList.IndexOf(LowerCase(loopvar)) >= 0 then
    begin
      LogDatei.log('The loopvar: ' + loopvar + ' exists as string var with the index: ' +
        IntToStr(Script.VarList.IndexOf(LowerCase(loopvar))), LLError);
      errmesg := 'Existing variable must not be used als loop variable';
    end
    else
    if Script.listOfStringLists.IndexOf(LowerCase(loopvar)) >= 0 then
    begin
      LogDatei.log('The loopvar: ' + loopvar +
        ' exists as stringlist var with the index: ' +
        IntToStr(Script.listOfStringLists.IndexOf(LowerCase(loopvar))), LLError);
      errmesg := 'Existing variable must not be used als loop variable';
    end
    else
    begin
      Script.VarList.add(loopvar);
      Script.ValuesList.add('');
      Result := True;
    end;
  end;
end;

function delLoopvarFromVarList(const loopvar: string; var errmesg: string): boolean;
var
  funcindex: integer;
begin
  Result := False;
  // in local function ?
  if inDefinedFuncNestCounter > 0 then
  begin
    // get the function we are in
    funcindex := StrToInt(definedFunctionsCallStack.Strings[
      definedFunctionsCallStack.Count - 1]);
    if definedFunctionArray[funcindex].delLocalVar(lowercase(loopvar)) then
    begin
      LogDatei.log('Deleted local string var: ' + lowercase(loopvar) +
        ' in local function: ' + definedFunctionArray[funcindex].Name +
        ' with index: ' + IntToStr(funcindex), LLDebug2);
      Result := True;
    end
    else
    begin
      errmesg := 'Loopvar: ' + loopvar + ' could not be deleted from local func: ' +
        definedFunctionArray[funcindex].Name;
    end;
  end
  // not in local function - make it global
  else
  begin
    if Script.VarList.IndexOf(LowerCase(loopvar)) < 0 then
    begin
      LogDatei.log('The loopvar: ' + loopvar +
        ' not exists as string var with the index: ' +
        IntToStr(Script.VarList.IndexOf(LowerCase(loopvar))), LLError);
      errmesg := 'Not Existing variable can not be deleted als loop variable';
    end
    else
    if Script.listOfStringLists.IndexOf(LowerCase(loopvar)) >= 0 then
    begin
      LogDatei.log('The loopvar: ' + loopvar +
        ' not exists as stringlist var with the index: ' + IntToStr(
        Script.listOfStringLists.IndexOf(LowerCase(loopvar))), LLError);
      errmesg := 'Not Existing variable can not be deleted als loop variable';
    end
    else
    begin
      Script.ValuesList.Delete(Script.varlist.indexOf(loopvar));
      Script.varlist.Delete(Script.varlist.indexOf(loopvar));
      Result := True;
    end;
  end;
end;


begin
  osdfParameterTypesNames[dfpString] := 'String';
  osdfParameterTypesNames[dfpStringlist] := 'Stringlist';
  osdfParameterTypesNames[dfpVoid] := 'Void';
  definedFunctionNames := TStringList.Create;
  definedFunctionsCallStack := TStringList.Create;
end.
