unit osdefinedfunctions;

{$mode delphi}

interface

uses
  Classes, SysUtils,
  osfunc;

type
  TosdfParameterTypes = (dfpString,dfpStringlist,dfpBoolean);
  TosdfParameterTypesNames = Array [TosdfParameterTypes] of String [50];
//  TPosdfParameterTypesNames = TosdfParameterTypesNames^;
  TOsDefinedFunctionParameter =
    record
      callByReference : boolean;
      paramName : string;
      paramDataType : string;
    end;
  TOsDefinedFunction  = class(TObject)
  private
    DFName : string;
    DFparamCount : integer;
    DFparamList : array [0..64] of TOsDefinedFunctionParameter;
    DFcontent : TStringList;
    DFLocalVarList : TStringList;

  public
    constructor create;
    destructor destroy;
    function parseDefinition(definitionStr : string; var errorstr : string) : boolean;
    procedure addContent(contentstr : string);
    function  checkContent(var errorstr : string) : boolean;
    function validIdentifier(identifier : string; var errorstr : string) : boolean;
  end;

var
  osdfParameterTypesNames : TosdfParameterTypesNames;
  remaining : string;
 // PParameterTypesNames : TPStatementNames;

implementation
uses
  osparser;

constructor TOsDefinedFunction.create;
begin
  DFcontent := Tstringlist.Create;
  DFLocalVarList := Tstringlist.Create;
  Inherited;
end;

destructor TOsDefinedFunction.destroy;
begin
  DFcontent.Free;
  DFLocalVarList.Free;
  inherited;
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
    errorstr := errorstr +   'Reserved name, must not be used as identifier'
  else if DFLocalVarList.IndexOf (lowercase(identifier)) >= 0 then
    errorstr := errorstr + 'Is still defined as local variable name and can no more be used as identifier'
  else validIdentifier := true;
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
  GetWord(definitionStr, DFName, remaining,WordDelimiterWhiteSpace);
  if not validIdentifier(DFName, errorstr) then
  begin
    // given functionname not valid
    errorstr := errorstr + 'given functionname: '+DFName+' not valid.';
    syntax_ok := false;
  end
  else
  begin
    // given functionname valid
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
            inc(paramcounter);
            DFparamCount := paramcounter +1;
            if calltype = 'var' then DFparamList[paramcounter].callByReference:= true
            else  DFparamList[paramcounter].callByReference:= false;
            DFparamList[paramcounter].paramName:= paramname;
            DFLocalVarList.Add(paramname);
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
                DFparamList[paramcounter].paramDataType:=paramtype;
              end
              else if lowercase(paramtype) = lowercase(osdfParameterTypesNames[dfpStringlist]) then
              begin
                // Stringlist type
                DFparamList[paramcounter].paramDataType:=paramtype;
              end
              else if lowercase(paramtype) = lowercase(osdfParameterTypesNames[dfpBoolean]) then
              begin
                // Boolean type
                DFparamList[paramcounter].paramDataType:=paramtype;
              end
              else
              begin
                // given paramname not valid
                errorstr := errorstr + 'given paramname: '+paramname+' not valid.';
                syntax_ok := false;
              end;
              if not skip(',',remaining,remaining,errorstr) then
                if skip(')',remaining,remaining,errorstr) then endOfParamlist := false
                else
                begin
                  // syntax error
                  errorstr := errorstr + ' , or ) expected.';
                  syntax_ok := false;
                end;
            end;
          end;
        end;
      end; // while
      if syntax_ok then parseDefinition := true;
    end;
  end;
end;

procedure TOsDefinedFunction.addContent(contentstr : string);
begin

end;


function TOsDefinedFunction.checkContent(var errorstr : string) : boolean;
begin

end;

begin
  osdfParameterTypesNames[dfpString] :=  'String';
  osdfParameterTypesNames[dfpStringlist] :=  'Stringlist';
  osdfParameterTypesNames[dfpBoolean] :=  'Boolean';
end.
