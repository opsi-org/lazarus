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
    function addLocalVar(name : string; datatype : TosdfDataTypes) : boolean;

    function parseCallParameter(paramline : string; var remaining : string;  var errorstr : string) : boolean;
    function call(paramline : string; var remaining : string) : boolean;

    property Name : String read DFName;
    property datatype : TosdfDataTypes read DFResultType;
    property Resultstring : String read DFResultString;
    //property ResultBool : boolean read DFResultBool;
    property ResultList : Tstringlist read DFResultList;
    property Index : integer read DFindex write DFindex;
  end;

    TDefinedFunctionsArray = Array of TOsDefinedFunction;

  function isVisibleLocalVar(varname: string; var index : integer) : boolean;
  procedure freeDefinedFunctions;

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
  syntax_ok, endOfParamlist : boolean;
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
        calltype := 'val';
        if skip('val',LowerCase(remaining),remaining,errorstr) then calltype := 'val';
        if skip('ref',LowerCase(remaining),remaining,errorstr) then calltype := 'ref';
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
            if calltype = 'var' then DFparamList[paramcounter].callByReference:= true
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
                  addLocalVar(paramname,dfpString);
                end
                else if lowercase(paramtype) = lowercase(osdfParameterTypesNames[dfpStringlist]) then
                begin
                  // Stringlist type
                  DFparamList[paramcounter].paramDataType:=dfpStringlist;
                  addLocalVar(paramname,dfpStringlist);
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
              dfpString : addLocalVar('$result$',dfpString);
              dfpStringlist : addLocalVar('$result$',dfpStringlist);
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


function TOsDefinedFunction.addLocalVar(name : string; datatype : TosdfDataTypes) : boolean;
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
  end
  else LogDatei.log('Syntax Error: Double definition of local variable: '+name,LLCritical );
end;


function TOsDefinedFunction.addLocalVarValueString(name : string; value : string) : boolean;
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
begin
  result := false;
  arrayindex := locaVarIndex(name);
  if arrayindex >= 0 then
  begin
    if (DFLocalVarList[arrayindex].varName=name)
       and (DFLocalVarList[arrayindex].varDataType = dfpString) then
    begin
      DFLocalVarList[arrayindex].varValueString :=  value;
      result := true;
    end
    else LogDatei.log('Internal Error: Unexpected type mismatch of local variable: '+name,LLCritical );
  end
  else LogDatei.log('Syntax Error: No definition of local variable: '+name,LLCritical );
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
begin
  result := false;
  arrayindex := locaVarIndex(name);
  if arrayindex >= 0 then
  begin
    if (DFLocalVarList[arrayindex].varName=name)
       and (DFLocalVarList[arrayindex].varDataType =dfpStringlist) then
    begin
      DFLocalVarList[arrayindex].VarValueList.Text := value.Text;
      result := true;
    end
    else LogDatei.log('Internal Error: Unexpected type mismatch of local variable: '+name,LLCritical );
  end
  else LogDatei.log('Syntax Error: No definition of local variable: '+name,LLCritical );
end;

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

function TOsDefinedFunction.getLocalVarValueList(name : string) : Tstringlist;
var
  arraycounter,i : integer;
  found : boolean;
begin
  result := Tstringlist.Create;
  i := 0;
  found := false;
  arraycounter := length(DFLocalVarList);
  if arraycounter > 0 then
    repeat
      if lowercase(DFLocalVarList[i].varName) = lowercase(name) then
      begin
        if DFLocalVarList[i].varDataType = dfpStringlist then
          result.Text := DFLocalVarList[i].varValueList.Text;
        found := true;
      end;
      inc(i);
    until (i >= arraycounter) or found;
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
function  TOsDefinedFunction.call(paramline : string; var remaining : string) : boolean;
var
  errorstr : string;
  section : TWorkSection;
  callingsection : TWorkSection;
  sectionresult : TSectionResult;
begin
  call := false;
  // we enter a defined function
  inc(inDefinedFuncNestCounter);
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
    section := TWorkSection.create(0);
    callingsection := TWorkSection.create(0);
    section.Assign(DFcontent);
    sectionresult := script.doAktionen(section,callingsection);
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
  //DFResultString := 'huhu';
  // we leave a defined function
  dec(inDefinedFuncNestCounter);
  definedFunctionsCallStack.Delete(definedFunctionsCallStack.Count-1);
end;

function isVisibleLocalVar(varname: string; var index : integer) : boolean;
var
  found, searchfinished : boolean;
  searchindex : integer;
begin
  result := false;
  found := false;
  searchfinished := false;
  if inDefinedFuncNestCounter > 0 then
  begin
    searchindex := definedFunctionsCallStack.Count-1;
    repeat
      index := strToInt(definedFunctionsCallStack.Strings[searchindex]);
      if definedFunctionArray[index].locaVarExists(varname) then found := true;
      dec(searchindex);
      if searchindex < 0 then searchfinished:=true;
    until found or searchfinished;
  end;
  result := found;
end;

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
