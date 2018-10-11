unit odg_py_deffunc;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  osparserhelper,
  oslog,
  SysUtils;

Type

TParamDoc = class
  private
    FParamName : string;
    FParamDesc : string;
    FParamAdvice : string;
  public
    constructor Create;
    destructor Destroy;
    property ParamName : string  read FParamName;
    property ParamDesc : string  read FParamDesc;
    property ParamAdvice : string  read FParamAdvice;
end;

TFuncDoc =  class
  private
    FDefinitionline : string;
    FName : string;
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
    FExample : string;
  public
    Fparams : array of TParamDoc;
    constructor Create;
    destructor Destroy;
    property Definitionline : string  read FDefinitionline write FDefinitionline;
    property Name : string  read Fname write Fname;
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
    property Example : string  read FExample write FExample;
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
    property functionCounter : integer  read FfunctionCounter write FfunctionCounter;
  end;

const
  cpycomment = '#';
  cpydeffunc = 'def';
  cendfunc = 'return';
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
  CExample = '@Example';


function parseInput_pythonlibrary(filename : string) : boolean;

var
  docobject : TFileDoc;

implementation

uses
  odg_main;

var
  exampleident : integer;


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
  FAuthor := '';
  FDate := '';
  FCopyright := '';
  FDescription := '';
  FReturns := '';
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
  FParamDesc := '';
  FParamAdvice := '';
  Inherited;
end;

destructor TParamDoc.destroy;
begin
  inherited;
end;

procedure parsePyDef(definitionStr : string; myfunc : TFuncDoc);
var
  paramnamestr : string;
  paramcounter : integer;
  endOfParamlist : boolean;
  remaining,errorstr : string;
begin
  endOfParamlist := false;
  paramcounter := -1;
  myfunc.FDefinitionline:=trim(definitionStr);
  // get function name
  GetWord(trim(definitionStr), myfunc.FName, remaining,WordDelimiterSet5);

  LogDatei.log('Found new defined function name: '+myfunc.FName,LLDebug2);
  if  skip('(',remaining,remaining,errorstr) then
  begin
    skip('self',remaining,remaining,errorstr);
    // test on no parameters
    if skip('):',remaining,remaining,errorstr) then
    begin
      endOfParamlist := true;
      myfunc.FParamCounter := 0;
    end
    else
    begin
      skip(',',remaining,remaining,errorstr);
    end;

    while  not endOfParamlist do
    begin
      // check paramname
      GetWord(remaining, paramnamestr, remaining,[',',')']);
      paramnamestr := trim(paramnamestr);

      LogDatei.log('Found defined function parametername: '+paramnamestr,LLDebug2);
      inc(paramcounter);

      myfunc.FParamCounter := paramcounter +1;
      setlength(myfunc.Fparams,myfunc.FParamCounter);
      myfunc.Fparams[paramcounter] := TParamDoc.Create;
      with myfunc.Fparams[paramcounter] do
      begin
        // is a new param
        FparamName:= paramnamestr;
        // check for endOfParamlist
        if skip('):',remaining,remaining,errorstr) then
        begin
          endOfParamlist := true;
        end
        else
        begin
          if skip(',',remaining,remaining,errorstr) then
        end
      end; // with
    end; // while
  end;
end;

function onMarkerAddDocStringTo(marker : string;docstring : string;var target :string) : boolean;
var
  tmpstr1 : string;
begin
  result := false;
  if pos(lowercase(marker),lowercase(docstring)) = 1 then
  begin
    if lowercase(marker) = lowercase(CExample) then
    begin
      // is this the first line of example
      if target = '' then
      begin
        // get ident of first line
        tmpstr1 := copy(docstring,length(marker)+1,length(docstring));
        exampleident := length(tmpstr1) - length(trimleft(tmpstr1));
      end;
      tmpstr1 := trimRight(copy(docstring,length(marker)+1+exampleident,length(docstring)));
      if target = '' then target := tmpstr1
      else target := target+LineEnding+tmpstr1;
    end
    else
    begin
      tmpstr1 := trim(copy(docstring,length(marker)+1,length(docstring)));
      if target = '' then target := tmpstr1
      else target := target+LineEnding+tmpstr1;
    end;
    result := true;
  end;
end;


function parseInput_pythonlibrary(filename : string) : boolean;
var
  linecounter, funccounter,prun : integer;
  indeffunc : integer;
  aktline, pname : string;
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
    aktline := trim(sourcelist.Strings[linecounter]);      // all parsed datas
    if indeffunc = 0 then
    begin  // not in defined function
      if pos(lowercase(cpydeffunc),lowercase(aktline)) = 1 then
      begin // function definition line
        inc(indeffunc);
        funccounter := docobject.Ffunctioncounter;
        inc(funccounter);
        docobject.Ffunctioncounter := funccounter;
        setlength(docobject.Ffunctions,funccounter);
        docobject.Ffunctions[funccounter-1] := TFuncDoc.Create;
        parsePyDef(copy(aktline,length(cpydeffunc)+1,length(aktline)),docobject.Ffunctions[funccounter-1]);
      end
      else
      if pos(cpycomment,aktline) = 1 then
      begin
        incomment := true;
        aktline := trim(copy(aktline,length(cpycomment)+1,length(aktline)));
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
      if pos(cpycomment,aktline) = 1 then
      begin
        incomment := true;
        aktline := trim(copy(aktline,length(cpycomment)+1,length(aktline)));
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
        if not onMarkerAddDocStringTo(CExample,aktline,docobject.Ffunctions[funccounter-1].FExample)then
            onMarkerAddDocStringTo(CLinks,aktline,docobject.Ffunctions[funccounter-1].FLinks);
        // parameter
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

end.

