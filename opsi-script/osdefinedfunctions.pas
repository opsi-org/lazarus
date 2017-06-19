unit osdefinedfunctions;

{$mode delphi}

interface

uses
  Classes, SysUtils,
  oslog,
  osfunc;

type
  TosdfDataTypes = (dfpString,dfpStringlist,dfpBoolean);
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
      varValueBool : boolean;
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
    DFResultBool : boolean;

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
    function getLocalVarValueString(name : string) : string;
    function getLocalVarValueList(name : string) : Tstringlist;
    function getLocalVarValueBool(name : string) : boolean;
    function setLocalVarValueString(name : string; value : string) : boolean;
    function setLocalVarValueList(name : string; value : Tstringlist) : boolean;
    function setLocalVarValueBool(name : string; value : boolean) : boolean;
    function addLocalVarValueString(name : string; value : string) : boolean;
    function addLocalVarValueList(name : string; value : Tstringlist) : boolean;
    function addLocalVarValueBool(name : string; value : boolean) : boolean;
    function addLocalVar(name : string; datatype : TosdfDataTypes) : boolean;

    function call(paramstr : string) : boolean;

    property Name : String read DFName;
    property datatype : TosdfDataTypes read DFResultType;
    property Resultstring : String read DFResultString;
    property ResultBool : boolean read DFResultBool;
    property ResultList : Tstringlist read DFResultList;
  end;

var
  osdfParameterTypesNames : TosdfDataTypesNames;
  remaining : string;
  definedFunctionNames : Tstringlist;

implementation
uses
  osparser;

constructor TOsDefinedFunction.create;
begin
  DFcontent := Tstringlist.Create;
  //DFLocalVarList := Tstringlist.Create;
  Inherited;
end;

destructor TOsDefinedFunction.destroy;
begin
  DFcontent.Free;
  //DFLocalVarList.Free;
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
  else if LowerCase(str) = LowerCase(osdfParameterTypesNames[dfpBoolean]) then
  begin
    result := true;
    ftype := dfpBoolean;
  end;
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
        calltype := 'const';
        if skip('const',LowerCase(remaining),remaining,errorstr) then calltype := 'const';
        if skip('var',LowerCase(remaining),remaining,errorstr) then calltype := 'var';
        GetWord(remaining, paramname, remaining,[':']);
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
                else if lowercase(paramtype) = lowercase(osdfParameterTypesNames[dfpBoolean]) then
                begin
                  // Boolean type
                  DFparamList[paramcounter].paramDataType:=dfpBoolean;
                  addLocalVar(paramname,dfpBoolean);
                end
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
          else LogDatei.log('Function has valid data type: '+remaining,LLDebug2)
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
    DFLocalVarList[arraycounter-1].varValueBool := value;
  end
  else LogDatei.log('Syntax Error: Double definition of local variable: '+name,LLCritical );
end;

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
    DFLocalVarList[arraycounter-1].VarValueList.Assign(value);
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
      DFLocalVarList[arrayindex].varValueBool :=  value;
      result := true;
    end
    else LogDatei.log('Internal Error: Unexpected type mismatch of local variable: '+name,LLCritical );
  end
  else LogDatei.log('Syntax Error: No definition of local variable: '+name,LLCritical );
end;

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
      DFLocalVarList[arrayindex].VarValueList.Assign(value);
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
  result := nil;
  i := 0;
  found := false;
  arraycounter := length(DFLocalVarList);
  if arraycounter > 0 then
    repeat
      if lowercase(DFLocalVarList[i].varName) = lowercase(name) then
      begin
        if DFLocalVarList[i].varDataType = dfpStringlist then
          result.Assign(DFLocalVarList[i].varValueList);
        found := true;
      end;
      inc(i);
    until (i >= arraycounter) or found;
end;

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
          result := DFLocalVarList[i].varValueBool;
        found := true;
      end;
      inc(i);
    until (i >= arraycounter) or found;
end;

function  TOsDefinedFunction.call(paramstr : string) : boolean;
begin
  // test
  result := true;
  DFResultString := 'huhu';
end;


begin
  osdfParameterTypesNames[dfpString] :=  'String';
  osdfParameterTypesNames[dfpStringlist] :=  'Stringlist';
  osdfParameterTypesNames[dfpBoolean] :=  'Boolean';
  definedFunctionNames := TStringList.Create;
end.
