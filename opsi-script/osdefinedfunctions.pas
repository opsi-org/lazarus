unit osdefinedfunctions;

{$mode delphi}

interface

uses
  Classes, SysUtils,
  oslog,
  osfunc;

type
  //TosdfDataTypes = (dfpString,dfpStringlist,dfpBoolean);
  TosdfDataTypes = (dfpString,dfpStringlist);
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

    function parseCallParameter(paramline : string; var remaining : string;  var errorstr : string) : boolean;
    //function call(paramline : string; var remaining : string) : boolean;
    function call(paramline : string; var remaining : string; var NestLevel : integer) : boolean;

    property Name : String read DFName;
    property datatype : TosdfDataTypes read DFResultType;
    property Resultstring : String read DFResultString;
    //property ResultBool : boolean read DFResultBool;
    property ResultList : Tstringlist read DFResultList;
    property Index : integer read DFindex write DFindex;
    property ParentFunc : String read DFParentFunc;
    property Active : boolean read DFActive write DFActive;
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
  osparser;

constructor TOsDefinedFunction.create;
begin
  DFcontent := Tstringlist.Create;
  DFResultList := TStringList.Create;
  DFActive:=false;
  Inherited;
end;

destructor TOsDefinedFunction.destroy;
begin
  DFcontent.Free;
  DFResultList.Free;
  SetLength(DFLocalVarList,0);
  inherited;
end;

function  TOsDefinedFunction.stringTofunctiontype(const str : string; var ftype : TosdfDataTypes) : boolean;
begin
  result := false;
  if LowerCase(str) = LowerCase(osdfParameterTypesNames[dfpString]) then
  begin
    result := true;
    ftype := dfpString;
  end
  else if LowerCase(str) = LowerCase(osdfParameterTypesNames[dfpStringlist]) then
  begin
    result := true;
    ftype := dfpStringlist;
  end
  //else if LowerCase(str) = LowerCase(osdfParameterTypesNames[dfpBoolean]) then
  //begin
  //  result := true;
  //  ftype := dfpBoolean;
  //end;
end;

function TOsDefinedFunction.validIdentifier(identifier : string; var errorstr : string) : boolean;
var
  SectionSpecifier : TSectionSpecifier;
begin
  validIdentifier := false;
  if  identifier = '' then
  begin
    // empty identifier not allowed
    errorstr := errorstr + 'empty identifier not allowed';
  end
  else if findKindOfStatement (identifier, SectionSpecifier, identifier) <> tsNotDefined then
    errorstr := errorstr +   'Reserved name, can not be used as identifier'
  else if locaVarIndex (lowercase(identifier)) >= 0 then
    errorstr := errorstr + 'Is already defined as local variable name and can not  be used as identifier again'
  else validIdentifier := true;
  // todo global vars
end;

function TOsDefinedFunction.parseDefinition(definitionStr : string; var errorstr : string) : boolean;
var
  paramname, paramtype, calltype : string;
  paramcounter : integer;
  syntax_ok, endOfParamlist,callByReference : boolean;
  searchindex, index : integer;
begin
  parseDefinition := false;
  syntax_ok := true;
  endOfParamlist := false;
  paramcounter := -1;
  // get function name
  GetWord(definitionStr, DFName, remaining,WordDelimiterSet5);
  if not validIdentifier(DFName, errorstr) then
  begin
    // given functionname not valid
    errorstr := errorstr + 'given functionname: '+DFName+' not valid.';
    syntax_ok := false;
  end
  else
  begin
    // given functionname valid
    DFName := LowerCase(DFName);
    LogDatei.log('Found new defined function name: '+DFName,LLDebug2);
    if not skip('(',remaining,remaining,errorstr) then
    begin
      // ( expected
      errorstr := errorstr + ' ( expected';
      syntax_ok := false;
    end
    else
    begin
      while syntax_ok and not endOfParamlist do
      begin
        // check call type
        calltype := 'val';
        if skip('val',LowerCase(remaining),remaining,errorstr) then calltype := 'val';
        if skip('ref',LowerCase(remaining),remaining,errorstr) then calltype := 'ref';
        if lowercase(calltype) = 'ref' then callByReference := true
        else callByReference := false;
        // check paramname
        GetWord(remaining, paramname, remaining,[':']);
        paramname := trim(paramname);
        if remaining = '' then
        begin
          errorstr := errorstr + ': <paramtype> expected after Parameter Name';
          syntax_ok := false;
        end
        else
        begin
          if not validIdentifier(paramname, errorstr) then
          begin
            // given paramname not valid
            errorstr := errorstr + 'given paramname: '+paramname+' not valid.';
            syntax_ok := false;
          end
          else
          begin
            // given parameter valid
            LogDatei.log('Found defined function parametername: '+paramname,LLDebug2);
            inc(paramcounter);
            DFparamCount := paramcounter +1;
            if callByReference then DFparamList[paramcounter].callByReference:= true
            else  DFparamList[paramcounter].callByReference:= false;
            LogDatei.log('Parameter has call type: '+calltype,LLDebug2);
            if locaVarIndex(paramname) >= 0 then
            begin
              // paramname has been defined before in this funstion
              errorstr := errorstr + ' paramname: '+paramname+' has been defined before in this funstion';
              syntax_ok := false;
            end
            else
            begin
              // is a new param
              DFparamList[paramcounter].paramName:= paramname;
              //DFLocalVarList.Add(paramname);
              if not skip(':',remaining,remaining,errorstr) then
              begin
                // : <paramtype> expected
                errorstr := errorstr + ' : <paramtype>  expected';
                syntax_ok := false;
              end
              else
              begin
                GetWord(remaining, paramtype, remaining,[',',')']);
                if remaining = '' then
                begin
                  errorstr := errorstr + ', or ) expected after Parameter Type';
                  syntax_ok := false;
                end
                else
                if lowercase(paramtype) = lowercase(osdfParameterTypesNames[dfpString]) then
                begin
                  // String type
                  DFparamList[paramcounter].paramDataType:=dfpString;
                  addLocalVar(paramname,dfpString,callByReference);
                end
                else if lowercase(paramtype) = lowercase(osdfParameterTypesNames[dfpStringlist]) then
                begin
                  // Stringlist type
                  DFparamList[paramcounter].paramDataType:=dfpStringlist;
                  addLocalVar(paramname,dfpStringlist,callByReference);
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
                  errorstr := errorstr + 'given param type: '+paramtype+' not valid.';
                  syntax_ok := false;
                end;
                if not syntax_ok then
                  LogDatei.log('Parameter has invalid data type: '+paramtype,LLDebug2)
                else
                begin
                  LogDatei.log('Parameter has valid data type: '+paramtype,LLDebug2);
                  if not skip(',',remaining,remaining,errorstr) then
                    if skip(')',remaining,remaining,errorstr) then endOfParamlist := true
                    else
                    begin
                      // syntax error
                      errorstr := errorstr + ' , or ) expected.';
                      syntax_ok := false;
                    end;
                end; // valid data type
              end; // valid new param
            end; // not defined before
          end;  // is valid identifier
        end; // data type found behind colon
      end; // while
      if syntax_ok then
      begin
        // get function type
        if skip(':',remaining,remaining,errorstr) then
        begin
          if not stringTofunctiontype(remaining, DFResultType) then
          begin
            // syntax error : wrong data type
            errorstr := errorstr + ' missing or illegal function data type: '+remaining+' - only string, stringlist and boolean allowed.';
            syntax_ok := false;
          end
          else
          begin
            LogDatei.log('Function has valid data type: '+remaining,LLDebug2);
            // create local result variable from result type
            case DFResultType of
              dfpString : addLocalVar('$result$',dfpString,false);
              dfpStringlist : addLocalVar('$result$',dfpStringlist,false);
              //dfpBoolean : addLocalVar('$result$',dfpBoolean);
            end;

          end;
        end
        else
        begin
          // syntax error : wrong data type
            errorstr := errorstr + ' missing function data type: '+remaining+' - only string, stringlist and boolean allowed.';
            syntax_ok := false;
        end;
      end;
    end;
  end;
  // set parent func
  if inDefinedFuncNestCounter > 0 then
  begin
    searchindex := definedFunctionsCallStack.Count-1;
    index := strToInt(definedFunctionsCallStack.Strings[searchindex]);
    DFParentFunc := definedFunctionArray[index].Name;
  end
  else
  begin
    // we in global
    DFParentFunc := 'global';
  end;
  DFActive:=true;
  if syntax_ok then parseDefinition := true;
end;

procedure TOsDefinedFunction.addContent(contentstr : string);
begin
  DFcontent.append(contentstr);
end;


function TOsDefinedFunction.checkContent(var errorstr : string) : boolean;
begin

end;

function TOsDefinedFunction.locaVarExists(name : string) : boolean;
var
  arraycounter,i : integer;
begin
  result := false;
  i := 0;
  arraycounter := length(DFLocalVarList);
  if arraycounter > 0 then
    repeat
      if lowercase(DFLocalVarList[i].varName) = lowercase(name) then
        result := True;
      inc(i);
    until (i >= arraycounter) or (result = true);
end;

function TOsDefinedFunction.locaVarIndex(name : string) : integer;
var
  arraycounter,i : integer;
begin
  result := -1;
  i := 0;
  arraycounter := length(DFLocalVarList);
  if arraycounter > 0 then
    repeat
      if lowercase(DFLocalVarList[i].varName) = lowercase(name) then
        result := i;
      inc(i);
    until (i >= arraycounter) or (result = i-1);
end;


function TOsDefinedFunction.addLocalVar(name : string; datatype : TosdfDataTypes; callByReference : boolean) : boolean;
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
    DFLocalVarList[arraycounter-1].varDataType :=datatype;
    if datatype = dfpStringlist then
      DFLocalVarList[arraycounter-1].VarValueList := TStringlist.Create;
    DFLocalVarList[arraycounter-1].callByReference := callByReference;
  end
  else LogDatei.log('Syntax Error: Double definition of local variable: '+name,LLCritical );
end;


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
    DFLocalVarList[arraycounter-1].varValueString :=  value;
  end
  else LogDatei.log('Syntax Error: Double definition of local variable: '+name,LLCritical );
end;

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
      DFLocalVarList[arraycounter-1].VarValueList := TStringlist.Create;
    DFLocalVarList[arraycounter-1].VarValueList.Text := value.Text;
  end
  else LogDatei.log('Syntax Error: Double definition of local variable: '+name,LLCritical );
end;

function TOsDefinedFunction.setLocalVarValueString(name : string; value : string) : boolean;
var
  arrayindex : integer;
  callByReference : boolean;
  scopeindex, VarIndex : integer;
  varname : string;
begin
  result := false;
  name := lowercase(name);
  arrayindex := locaVarIndex(name);
  if arrayindex >= 0 then
  begin
    if (DFLocalVarList[arrayindex].varName=name)
       and (DFLocalVarList[arrayindex].varDataType = dfpString) then
    begin
      callByReference := DFLocalVarList[arrayindex].callByReference;
      if callByReference then
      begin
        // call by ref
        scopeindex := DFLocalVarList[arrayindex].referencevarscopeindex;
        varname := DFLocalVarList[arrayindex].referencevarname;
        if scopeindex = -1 then
        begin
          // global
          VarIndex := script.VarList.IndexOf (LowerCase (varname));
          script.ValuesList [VarIndex] := value;
        end
        else if scopeindex >= 0 then
        begin
          // local func
          definedFunctionArray[scopeindex].setLocalVarValueString(varname,value);
        end
        else
        begin
          // invalid
          LogDatei.log('Internal Error: Invalid scopeindex in setLocalVarValueString',LLCritical );
        end;
        //setLength(DFLocalVarList[arrayindex].referencevar, length(value));
        //StrCopy(DFLocalVarList[arrayindex].referencevar, pchar(value));
        //String(DFLocalVarList[arrayindex].referencevar^):=value;
      end
      else // call by value
        DFLocalVarList[arrayindex].varValueString :=  value;
      result := true;
    end
    else LogDatei.log('Internal Error: setLocalVarValueString: Unexpected type mismatch of local variable: '+name,LLCritical );
  end
  else LogDatei.log('Syntax Error: setLocalVarValueString: No definition of local variable: '+name,LLCritical );
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

function TOsDefinedFunction.setLocalVarValueList(name : string; value : Tstringlist) : boolean;
var
  arrayindex : integer;
  callByReference : boolean;
  scopeindex, VarIndex : integer;
  varname : string;
begin
  result := false;
  name := lowercase(name);
  arrayindex := locaVarIndex(name);
  if arrayindex >= 0 then
  begin
    if (DFLocalVarList[arrayindex].varName=name)
       and (DFLocalVarList[arrayindex].varDataType =dfpStringlist) then
    begin
      callByReference := DFLocalVarList[arrayindex].callByReference;
      if callByReference then
      begin
        // call by ref
        scopeindex := DFLocalVarList[arrayindex].referencevarscopeindex;
        varname := DFLocalVarList[arrayindex].referencevarname;
        if scopeindex = -1 then
        begin
          // global
          VarIndex := script.listOfStringLists.IndexOf (LowerCase (varname));
          script.ContentOfStringLists.Items[VarIndex] := value;
        end
        else if scopeindex >= 0 then
        begin
          // local func
          definedFunctionArray[scopeindex].setLocalVarValuelist(varname,value);
        end
        else
        begin
          // invalid
          LogDatei.log('Internal Error: Invalid scopeindex in setLocalVarValueList',LLCritical );
        end;
      end
      else // call by value
        DFLocalVarList[arrayindex].VarValueList.Text := value.Text;
      result := true;
    end
    else LogDatei.log('Internal Error: setLocalVarValueList: Unexpected type mismatch of local variable: '+name,LLCritical );
  end
  else LogDatei.log('Syntax Error: setLocalVarValueList: No definition of local variable: '+name,LLCritical );
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
function TOsDefinedFunction.getLocalVarValueString(name : string) : string;
var
  arrayindex : integer;
  callByReference : boolean;
  scopeindex, VarIndex : integer;
  varname : string;
begin
  result := '';
  name := lowercase(name);
  arrayindex := locaVarIndex(name);
  if arrayindex >= 0 then
  begin
    if (DFLocalVarList[arrayindex].varName=name)
       and (DFLocalVarList[arrayindex].varDataType = dfpString) then
    begin
      callByReference := DFLocalVarList[arrayindex].callByReference;
      if callByReference then
      begin
        // call by ref
        scopeindex := DFLocalVarList[arrayindex].referencevarscopeindex;
        varname := DFLocalVarList[arrayindex].referencevarname;
        if scopeindex = -1 then
        begin
          // global
          VarIndex := script.VarList.IndexOf (LowerCase (varname));
          result := script.ValuesList [VarIndex];
        end
        else if scopeindex >= 0 then
        begin
          // local func
          definedFunctionArray[scopeindex].getLocalVarValueString(varname);
        end
        else
        begin
          // invalid
          LogDatei.log('Internal Error: Invalid scopeindex in getLocalVarValueString',LLCritical );
        end;
        //setLength(DFLocalVarList[arrayindex].referencevar, length(value));
        //StrCopy(DFLocalVarList[arrayindex].referencevar, pchar(value));
        //String(DFLocalVarList[arrayindex].referencevar^):=value;
      end
      else // call by value
        result := DFLocalVarList[arrayindex].varValueString;
    end
    else LogDatei.log('Internal Error: getLocalVarValueString: Unexpected type mismatch of local variable: '+name,LLCritical );
  end
  else LogDatei.log('Syntax Error: getLocalVarValueString: No definition of local variable: '+name,LLCritical );
end;


function TOsDefinedFunction.getLocalVarValueList(name : string) : Tstringlist;
var
  arraycounter,i : integer;
  arrayindex : integer;
  callByReference : boolean;
  scopeindex, VarIndex : integer;
  varname : string;
begin
  result := Tstringlist.Create;
  name := lowercase(name);
  arrayindex := locaVarIndex(name);
  if arrayindex >= 0 then
  begin
    if (DFLocalVarList[arrayindex].varName=name)
       and (DFLocalVarList[arrayindex].varDataType = dfpStringlist) then
    begin
      callByReference := DFLocalVarList[arrayindex].callByReference;
      if callByReference then
      begin
        // call by ref
        scopeindex := DFLocalVarList[arrayindex].referencevarscopeindex;
        varname := DFLocalVarList[arrayindex].referencevarname;
        if scopeindex = -1 then
        begin
          // global
          VarIndex := script.listOfStringLists.IndexOf (LowerCase (varname));
          result := TStringlist(script.ContentOfStringLists.Items[VarIndex]);
        end
        else if scopeindex >= 0 then
        begin
          // local func
          definedFunctionArray[scopeindex].getLocalVarValuelist(varname);
        end
        else
        begin
          // invalid
          LogDatei.log('Internal Error: Invalid scopeindex in getLocalVarValueList',LLCritical );
        end;
      end
      else // call by value
        result := DFLocalVarList[arrayindex].VarValueList;
    end
    else LogDatei.log('Internal Error: getLocalVarValueList: Unexpected type mismatch of local variable: '+name,LLCritical );
  end
  else LogDatei.log('Syntax Error: getLocalVarValueList: No definition of local variable: '+name,LLCritical );
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

function TOsDefinedFunction.getLocalVarDatatype(name : string) : TosdfDataTypes;
var
  arrayindex : integer;
begin
  result := dfpString;
  name := lowercase(name);
  arrayindex := locaVarIndex(name);
  if arrayindex >= 0 then
  begin
    result := DFLocalVarList[arrayindex].varDataType;
  end
  else LogDatei.log('Syntax Error: No definition of local variable: '+name,LLCritical );
end;



function TOsDefinedFunction.parseCallParameter(paramline : string; var remaining : string; var errorstr : string) : boolean;
var
  paramname : string;
    paramstr : string;
    paramtype, calltype : string;
    paramstrvalue : string;
    paramboolvalue : boolean;
    paramlistvalue : TXStringlist;
  paramcounter : integer;
  syntax_ok, endOfParamlist : boolean;
  //remaining,
  r : string;
  section : TuibIniScript;
  NestingLevel : Integer = 0;
  varindex : integer;

begin
  parseCallParameter := false;
  syntax_ok := true;
  endOfParamlist := false;
  paramcounter := -1;
  if not skip('(',paramline,remaining,errorstr) then
  begin
    // ( expected
    errorstr := errorstr + ' ( expected';
    syntax_ok := false;
  end
  else
  begin
    while syntax_ok and not endOfParamlist do
    begin
      if remaining = '' then
      begin
        errorstr := errorstr + ': <paramtype> expected after Parameter Name';
        syntax_ok := false;
      end
      else
      begin
        GetWord(remaining, paramstr, remaining,WordDelimiterSet6);
        inc(paramcounter);
        LogDatei.log('Paramnr: '+inttostr(paramcounter)+' is : '+paramstr,LLDebug2);
        if DFparamList[paramcounter].callByReference then
        begin
          // call by reference
          case DFparamList[paramcounter].paramDataType of
            dfpString :     begin
                              if not isVisibleStringVar(paramstr) then
                              begin
                                // parameter type mismatch
                                syntax_ok := false;
                                errorstr := errorstr + 'Error: String variable expected, but: '+paramstr+' is not a visible string variable';
                              end
                              else
                              begin
                                // we got a string variable- make it to a local var
                                //DFLocalVarList[paramcounter].referencevar:= getReferenceToVar(paramstr);
                                DFLocalVarList[paramcounter].referencevarname:= paramstr;
                                if isVisibleLocalVar(paramstr, varindex) then
                                begin
                                  // is local var
                                  DFLocalVarList[paramcounter].referencevarscopeindex := varindex;
                                  LogDatei.log('Paramnr: '+inttostr(paramcounter)+' is a reference to local: '+paramstr,LLDebug2);
                                end
                                else if isVisibleGlobalStringVar(paramstr, varindex) then
                                begin
                                  // is global var : -1
                                  DFLocalVarList[paramcounter].referencevarscopeindex := -1;
                                  LogDatei.log('Paramnr: '+inttostr(paramcounter)+' is a reference to global: '+paramstr,LLDebug2);
                                end
                                else
                                begin
                                  // failed
                                  LogDatei.log('Error: Did not found the reference to: '+paramstr,LLError);
                                end;
                              end;
                            end;
            dfpStringlist : begin
                              //if not Script.produceStringList(section,paramstr,r,paramlistvalue,errorstr) then
                              if not isVisibleStringlistVar(paramstr) then
                              begin
                                // parameter type mismatch
                                syntax_ok := false;
                                errorstr := errorstr + 'Error: StringList expression expected, but: '+paramstr+' gives no stringlist';
                              end
                              else
                              begin
                                // we got a stringlist - make it to a local var
                                DFLocalVarList[paramcounter].referencevarname:= paramstr;
                                if isVisibleLocalVar(paramstr, varindex) then
                                begin
                                  // is local var
                                  DFLocalVarList[paramcounter].referencevarscopeindex := varindex;
                                  LogDatei.log('Paramnr: '+inttostr(paramcounter)+' is a reference to local stringlist: '+paramstr,LLDebug2);
                                end
                                else if isVisibleGlobalStringlistVar(paramstr, varindex) then
                                begin
                                  // is global var : -1
                                  DFLocalVarList[paramcounter].referencevarscopeindex := -1;
                                  LogDatei.log('Paramnr: '+inttostr(paramcounter)+' is a reference to global stringlist: '+paramstr,LLDebug2);
                                end
                                else
                                begin
                                  // failed
                                  LogDatei.log('Error: Did not found the reference to: '+paramstr,LLError);
                                end
                              end;
                            end;
          end; // end case
        end
        else
        begin
          // call by value
          case DFparamList[paramcounter].paramDataType of
            dfpString :     begin
                              if not Script.EvaluateString(paramstr,r,paramstrvalue,errorstr) then
                              begin
                                // parameter type mismatch
                                syntax_ok := false;
                                errorstr := errorstr + 'Error: String expression expected, but: '+paramstr+' gives no string';
                              end
                              else
                              begin
                                // we got a string - make it to a local var
                                DFLocalVarList[paramcounter].varValueString:=paramstrvalue;
                                LogDatei.log('Paramnr: '+inttostr(paramcounter)+' is the string: '+paramstrvalue,LLDebug2);
                              end;
                            end;
            dfpStringlist : begin
                              if not Script.produceStringList(section,paramstr,r,paramlistvalue,errorstr) then
                              begin
                                // parameter type mismatch
                                syntax_ok := false;
                                errorstr := errorstr + 'Error: StringList expression expected, but: '+paramstr+' gives no stringlist';
                              end
                              else
                              begin
                                // we got a stringlist - make it to a local var
                                DFLocalVarList[paramcounter].VarValueList.text := paramlistvalue.Text;
                                LogDatei.log('Paramnr: '+inttostr(paramcounter)+' is a stringlist',LLDebug2);
                              end;
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

        if not skip(',',remaining,remaining,errorstr) then
          if skip(')',remaining,remaining,errorstr) then endOfParamlist := true
          else
          begin
            // syntax error
            errorstr := errorstr + ' , or ) expected.';
            syntax_ok := false;
          end;
      end; // remaining <> ''
    end; // while next paramstr
  end; // start with '('
  if syntax_ok then parseCallParameter := true;
end;


// run the function
function  TOsDefinedFunction.call(paramline : string; var remaining : string; var NestLevel : integer) : boolean;
var
  errorstr : string;
  section : TWorkSection;
  callingsection : TWorkSection;
  sectionresult : TSectionResult;
begin
  call := false;
  inc(inDefFuncLevel);
  LogDatei.log('We enter the defined function: '+DFName+' with '+IntToStr(DFcontent.Count)+' lines. inDefFuncLevel: '+inttostr(inDefFuncLevel),LLDebug2);
  LogDatei.log_prog('paramline: '+paramline+' remaining: '+remaining+' Nestlevel: '+inttostr(NestLevel),LLDebug2);
  DFActive:=true;
  //inc(inDefinedFuncNestCounter);
  definedFunctionsCallStack.Append(InttoStr(DFIndex));
  //parse parameter
  if not parseCallParameter(paramline, remaining, errorstr) then
  begin
    // parse parameter failed
    LogDatei.log('Syntax Error: Parameter parsing failed: '+errorstr,LLCritical);
  end
  else
  begin
    // run the body of the function
    inc(inDefinedFuncNestCounter);
    section := TWorkSection.create(Nestlevel);
    callingsection := TWorkSection.create(0);
    section.Assign(DFcontent);
    sectionresult := script.doAktionen(section,callingsection);
    Nestlevel := section.NestingLevel;
    call := true;
    case DFResultType of
       dfpString :     begin
                         DFResultString := getLocalVarValueString('$result$');
                       end;
       dfpStringlist : begin
                         DFResultList.Text := getLocalVarValueList('$result$').Text;
                       end;
       //dfpBoolean :    begin
       //                  DFResultBool := StrToBool(getLocalVarValueString('$result$'));
       //                end;
    end;
  end;
  // we leave a defined function
  // free the local Vars - leave params + $result$
  SetLength(DFLocalVarList,DFparamCount+1);
  dec(inDefinedFuncNestCounter);
  definedFunctionsCallStack.Delete(definedFunctionsCallStack.Count-1);
  DFActive:=false;
  inc(inDefFuncLevel);
  LogDatei.log('We leave the defined function: '+DFName+' ; inDefFuncLevel: '+inttostr(inDefFuncLevel),LLDebug2);
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

function createListOfVisibleParents : TStringlist;
var
  fparent, fname : string;
  searchfinished : boolean;
  searchindex, index : integer;
begin
  result := TStringlist.create;
  searchfinished := false;
  // first add the name of the running local function
  searchindex := definedFunctionsCallStack.Count-1;
  index := strToInt(definedFunctionsCallStack.Strings[searchindex]);
  fname := definedFunctionArray[index].Name;
  result.Append(fname);
  repeat
    index := strToInt(definedFunctionsCallStack.Strings[searchindex]);
    fname := definedFunctionArray[index].Name;
    if result.IndexOf(fname) >= 0 then
    begin
      // the akt function is the running one or a sub func
      fparent := definedFunctionArray[index].ParentFunc;
      result.Append(fparent);
      if fparent = 'global' then  searchfinished:=true;
    end;
    dec(searchindex);
    if searchindex < 0 then searchfinished:=true;
  until searchfinished;
end;


function isVisibleLocalVar(varname: string; var index : integer) : boolean;
var
  found, searchfinished : boolean;
  searchindex : integer;
  parentlist : tstringlist;

begin
  result := false;
  found := false;
  searchfinished := false;
  if inDefinedFuncNestCounter > 0 then
  begin
    // we are in a local function
    parentlist := TStringlist.Create;
    parentlist.Text := createListOfVisibleParents.Text;
    searchfinished := false;
    searchindex := definedFunctionsCallStack.Count-1;
    repeat
      index := strToInt(definedFunctionsCallStack.Strings[searchindex]);
      if parentlist.IndexOf(definedFunctionArray[index].Name) >= 0 then
      begin
        // local variable of this function are visible (global to this function)
        if (definedFunctionArray[index].Active
           and definedFunctionArray[index].locaVarExists(varname)) then found := true;
      end;
      dec(searchindex);
      if searchindex < 0 then searchfinished:=true;
    until found or searchfinished;
  end;
  result := found;
end;

function getVisibleLocalStringVarNameValueList : TStringlist;
var
  resultentry, varname : string;
  searchfinished : boolean;
  searchindex, index : integer;
  parentlist : tstringlist;
  i : integer;

begin
  result := Tstringlist.Create;
  if inDefinedFuncNestCounter > 0 then
  begin
    // we are in a local function
    parentlist := TStringlist.Create;
    parentlist.Text := createListOfVisibleParents.Text;
    searchfinished := false;
    searchindex := definedFunctionsCallStack.Count-1;
    repeat
      index := strToInt(definedFunctionsCallStack.Strings[searchindex]);
      if parentlist.IndexOf(definedFunctionArray[index].Name) >= 0 then
      begin
        // local variable of this function are visible (global to this function)
        if (definedFunctionArray[index].Active) then
        begin
          for i := 0 to length(definedFunctionArray[index].DFLocalVarList)-1 do
            if definedFunctionArray[index].DFLocalVarList[i].varDataType = dfpString then
            begin
              varName := definedFunctionArray[index].DFLocalVarList[i].varName;
              resultentry :=  varName + '=';
              resultentry :=  resultentry + definedFunctionArray[index].getLocalVarValueString(varname);
              result.Append(resultentry);
            end;
        end;
      end;
      dec(searchindex);
      if searchindex < 0 then searchfinished:=true;
    until searchfinished;
  end;
end;

function isVisibleGlobalStringVar(varname: string; var index : integer) : boolean;
begin
  index := script.VarList.IndexOf (LowerCase (VarName));
  if index >= 0 then
       result := true;
end;

function isVisibleGlobalStringlistVar(varname: string; var index : integer) : boolean;
begin
  index := script.listOfStringLists.IndexOf(LowerCase (VarName));
  if index >= 0 then
       result := true;
end;


function isVisibleStringVar(varname: string) : boolean;
var
  funcindex : integer;
begin
   result := false;
   if isVisibleLocalVar(VarName,funcindex)  then
     if definedFunctionArray[FuncIndex].getLocalVarDatatype(varname) = dfpString then
       result := true;
   if script.VarList.IndexOf (LowerCase (VarName)) >= 0 then
       result := true;
end;

function isVisibleStringlistVar(varname: string) : boolean;
var
  funcindex : integer;
begin
   result := false;
   if isVisibleLocalVar(VarName,funcindex)  then
     if definedFunctionArray[FuncIndex].getLocalVarDatatype(varname) = dfpStringlist then
       result := true;
   if script.listOfStringLists.IndexOf(LowerCase (VarName)) >= 0 then
       result := true;
end;

function getFirstLineAfterEndFunc(list : TStringlist; startline : integer) : integer;
var
  i : integer;
  inDefFunc : integer = 0;
  line : string;
  stopsearch : boolean = false;
begin
  result := startline;
  if pos('endfunc', lowercase(list.Text)) > 0 then
  begin
    // quick test tells us that we have to make a closer look
    i := startline;
    repeat
      line := trim(lowercase(list.Strings[i]));
      if  pos('deffunc',line) > 0 then inc(inDefFunc)
      else if (inDefFunc > 0) and (pos('endfunc',line) > 0) then
      begin
        // endfunc
        dec(inDefFunc);
        result := i;
      end
      else if (inDefFunc = 0) and (pos('[',line) = 1)
               and (pos(']',line) = length(line)) and (length(line) > 2) then
      begin
        // section header found outside of local function
        stopsearch := true;
      end;
      inc(i);
    until (i >= list.Count) or stopsearch;
  end;
end;

function IsEndOfLocalFunction(const s: string): boolean;
var
  TestS: string = '';
begin
  result := false;
  if inDefinedFuncNestCounter > 0 then
  begin
    if trim(lowercase(s)) = 'endfunc' then result := true;
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
  i : integer;
begin
  for i := definedFunctionNames.Count-1 downto 0 do
  begin
    definedFunctionArray[i].destroy;
  end;
  SetLength(definedFunctionArray,0);
  definedFunctioncounter := 0;
  definedFunctionNames.Clear;
  definedFunctionsCallStack.Clear;
end;


begin
  osdfParameterTypesNames[dfpString] :=  'String';
  osdfParameterTypesNames[dfpStringlist] :=  'Stringlist';
  //osdfParameterTypesNames[dfpBoolean] :=  'Boolean';
  definedFunctionNames := TStringList.Create;
  definedFunctionsCallStack := TStringList.Create;
end.
