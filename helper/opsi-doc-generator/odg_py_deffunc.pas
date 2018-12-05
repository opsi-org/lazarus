unit odg_py_deffunc;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  osparserhelper,
  oslog,
  SysUtils,
  StrUtils,
  regexpr;

Type

TParamDoc = class
  private
    FParamName : string;
    FParamType : string;
    FParamDesc : string;
  public
    constructor Create;
    destructor Destroy;
    property ParamName : string  read FParamName;
    property ParamType : string  read FParamType;
    property ParamDesc : string  read FParamDesc;
end;

TFuncDoc =  class
  private
    FDefinitionline : string;
    FName : string;
    FAuthor : string;
    FLicense : string;
    FCopyright : string;
    FDescription : string;
    FReturns :string;
    FReturnType :string;
    FRType :string;
    FRaises :string;
    FParamCounter : integer;
  public
    Fparams : array of TParamDoc;
    constructor Create;
    destructor Destroy;
    property Definitionline : string  read FDefinitionline write FDefinitionline;
    property Name : string  read Fname write Fname;
    property Author : string  read FAuthor write FAuthor;
    property License : string  read FLicense write FLicense;
    property Copyright : string  read FCopyright write FCopyright;
    property Description : string  read FDescription write FDescription;
    property Returns :string  read FReturns write FReturns;
    property ReturnType :string  read FReturnType write FReturnType;
    property RType :string  read FRType write FRType;
    property Raises :string  read FRaises write FRaises;
    property ParamCounter : integer  read FParamCounter write FParamCounter;
  end;

TFileDoc =  class
  private
    Fname : string;
    FAuthor : string;
    FLicense : string;
    FCopyright : string;
    Ffiledesc : string;
    FfunctionCounter : integer;
  public
    Ffunctions : array of TFuncDoc;
    constructor Create;
    destructor Destroy;
    property name : string  read Fname write Fname;
    property filedesc : string  read Ffiledesc write Ffiledesc;
    property Author : string  read FAuthor write FAuthor;
    property License : string  read FLicense write FLicense;
    property Copyright : string  read FCopyright write FCopyright;
    property functionCounter : integer  read FfunctionCounter write FfunctionCounter;
  end;

const
  cmulticomment1 = '"""';
  cmulticomment2 = '''''''';
  cpycomment = '#';
  cpydeffunc = 'def ';
  cpydefnotpublic = 'def _';
  cpyclass = 'class ';
  cauthor = ':author:';
  clicense = ':license:';
  ccopyright = ':copyright:';
  CReturns = ':return:';
  CReturnType = ':returntype:';
  CRType = ':rtype:';
  CRaises = ':raises';
  CParamType = ':type ';
  CParam = ':param ';


function parseInput_pythonlibrary(filename : string) : boolean;

var
  docobject : TFileDoc;
  preprocessedlist : TStringlist;

implementation

uses
  odg_main;

constructor TFileDoc.create;
begin
  FName := '';
  Ffiledesc := '';
  FAuthor := '';
  FLicense := '';
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
  FLicense := '';
  FCopyright := '';
  FDescription := '';
  FReturns := '';
  FReturnType := '';
  FRType := '';
  FRaises := '';
  FParamCounter := 0;
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
  FParamType := '';
  FParamDesc := '';
  Inherited;
end;

destructor TParamDoc.destroy;
begin
  inherited;
end;


function onMarkerAddDocStringTo(marker : string;docstring : string;var target :string) : boolean;
var
  tmpstr1 : string;
begin
  result := false;
  if pos(marker,docstring) = 1 then
  begin
    tmpstr1 := trim(copy(docstring,length(marker)+1,length(docstring)));
    if target = '' then target := tmpstr1
    else target := target+', '+tmpstr1;
    result := true;
  end;
end;

procedure parsePyDef(definitionStr : string; myfunc : TFuncDoc);
var
  paramnamestr : string;
  paramcounter, posofequal : integer;
  endOfParamlist : boolean;
  remaining,errorstr : string;
begin
  endOfParamlist := false;
  paramcounter := -1;
  myfunc.FDefinitionline:=trim(definitionStr);
  GetWord(trim(definitionStr), myfunc.FName, remaining,WordDelimiterSet5);
  LogDatei.log('Found new defined function name: '+myfunc.FName,LLDebug2);

  if  skip('(',remaining,remaining,errorstr) then
  begin
    if skip('):',remaining,remaining,errorstr) then
    begin
      endOfParamlist := true;
      myfunc.FParamCounter := 0;
    end;

    while not endOfParamlist do
    begin
      GetWord(remaining, paramnamestr, remaining,[',',':']);
      if remaining = ':' then
      begin
        if pos('=', trim(paramnamestr)) > 1 then
        begin
          posofequal := pos('=', trim(paramnamestr));
          paramnamestr := copy(paramnamestr, 0, posofequal-1);
        end
        else
        begin
          delete(paramnamestr, length(paramnamestr), 1);
          paramnamestr := trim(paramnamestr);
        end
      end
      else if pos('=', trim(paramnamestr)) > 1 then
      begin
        posofequal := pos('=', trim(paramnamestr));
        paramnamestr := copy(paramnamestr, 0, posofequal-1);
      end
      else
        paramnamestr := trim(paramnamestr);

      LogDatei.log('Found defined function parametername: '+paramnamestr,LLDebug2);
      inc(paramcounter);

      myfunc.FParamCounter := paramcounter+1;
      setlength(myfunc.Fparams,myfunc.FParamCounter);
      myfunc.Fparams[paramcounter] := TParamDoc.Create;

      with myfunc.Fparams[paramcounter] do
      begin
        FparamName:= paramnamestr;
        if skip(':',remaining,remaining,errorstr) then
        begin
          endOfParamlist := true;
        end
        else
        begin
          if skip(',',remaining,remaining,errorstr) then
        end
      end;
    end;
  end;
end;

function getDefinitionLine(var currentlinenumber : integer) : string;
var
  deflinecounter, matchpos : integer;
  defstring : string;
begin
  result := '';
  defstring := trim(preprocessedlist.Strings[currentlinenumber]);
  deflinecounter := currentlinenumber;
  defstring := copy(defstring, length(cpydeffunc)+1,length(defstring));
  matchpos := pos(':', defstring);

  while matchpos = 0 do
  begin
    inc(deflinecounter);
    defstring := defstring + trim(preprocessedlist.Strings[deflinecounter]);
    matchpos := pos(':', defstring);
  end;

  defstring := copy(defstring,1,matchpos);
  if pos ('self', defstring) > 0 then
  begin
    if pos('self, ', defstring) > 0 then
    begin
      matchpos := pos('self, ', defstring);
      delete(defstring, matchpos, length('self, '));
    end
    else if pos('self', defstring) > 0 then
    begin
      matchpos := pos('self', defstring);
      delete(defstring, matchpos, length('self'));
    end
  end;

  currentlinenumber := deflinecounter+1;
  result := defstring;
end;

function getFileDoc(var currentlinenumber : integer) : boolean;
var
  docstringcounter, matchpos : integer;
  docstring, description : string;
begin
  result := false;
  docstring := trim(preprocessedlist.Strings[currentlinenumber]);
  docstringcounter:= currentlinenumber;

  if (pos(cmulticomment1, docstring) = 1) then
    docstring := copy(docstring, length(cmulticomment1)+1,length(docstring))
  else if (pos(cmulticomment2, docstring) = 1) then
    docstring := copy(docstring, length(cmulticomment2)+1,length(docstring));

  description := trim(docstring);
  matchpos := (rpos(cmulticomment1, docstring)) or (rpos(cmulticomment2, docstring));

  while matchpos = 0 do
  begin
    inc(docstringcounter);
    docstring := preprocessedlist.Strings[docstringcounter];
    if not onMarkerAddDocStringTo(cauthor,trim(docstring),docobject.FAuthor) then
    if not onMarkerAddDocStringTo(clicense,trim(docstring),docobject.FLicense) then
    if not onMarkerAddDocStringTo(ccopyright,trim(docstring),docobject.FCopyright) then
    description := description+LineEnding+trim(docstring);
    matchpos := rpos(cmulticomment1, docstring) or rpos(cmulticomment2, docstring);
  end;

  matchpos := rpos(cmulticomment1, description) or rpos(cmulticomment2, description);
  description := copy(description,1,matchpos-1);
  docobject.Ffiledesc := description;
  currentlinenumber := docstringcounter+1;
  result := true;
end;

function indentation(line : string) : integer;
var
  totallen, len : integer;
begin
  totallen := Length(Tab2Space(line,4));
  len := Length(TrimLeft(Tab2Space(line,4)));
  result := totallen-len;
end;

function processPublicDef(var currentlinenumber : integer) : boolean;
var
  indentofdef, funccounter, linecounter, prun, matchpos : integer;
  currentline, funcdescription, pname : string;
  indoc, docfound : boolean;
begin
  result := false;
  indoc := false;
  docfound := false;
  currentline := preprocessedlist.Strings[currentlinenumber];
  indentofdef := indentation(currentline);
  linecounter := currentlinenumber;

  funccounter := docobject.Ffunctioncounter;
  inc(funccounter);
  docobject.Ffunctioncounter := funccounter;
  setlength(docobject.Ffunctions,funccounter);
  docobject.Ffunctions[funccounter-1] := TFuncDoc.Create;
  parsePyDef(getDefinitionLine(currentlinenumber),docobject.Ffunctions[funccounter-1]);

  linecounter := currentlinenumber;
  currentline := preprocessedlist.Strings[currentlinenumber];

  while ((indentation(currentline) > indentofdef) or (currentline = '')) do
  begin
    if ((pos(cmulticomment1, trim(currentline)) = 1) or  (pos(cmulticomment2, trim(currentline)) = 1)) and not docfound then
    begin
      if  not indoc then
      begin
        if (rpos(cmulticomment1, trim(currentline)) > 1) or (rpos(cmulticomment2, trim(currentline)) > 1) then
        begin
          if (pos(cmulticomment1, trim(currentline)) = 1) then
            currentline := copy(trim(currentline), length(cmulticomment1)+1,length(trim(currentline)))
          else if (pos(cmulticomment2, trim(currentline)) = 1) then
            currentline := copy(trim(currentline), length(cmulticomment2)+1,length(trim(currentline)));

          funcdescription := trim(currentline);
          matchpos := rpos(cmulticomment1, currentline) or rpos(cmulticomment2, currentline);
          funcdescription := copy(funcdescription,1,matchpos-1);

          docobject.Ffunctions[funccounter-1].FDescription := trim(funcdescription);
          indoc := false;
          funcdescription := '';
          docfound := true;
        end
        else
          indoc := true;
      end
      else
      begin
        docobject.Ffunctions[funccounter-1].FDescription := trim(funcdescription);
        indoc := false;
        funcdescription := '';
        docfound := true;
      end
    end
    else if indoc then
    begin
      if not onMarkerAddDocStringTo(cauthor,trim(currentline),docobject.Ffunctions[funccounter-1].FAuthor) then
      if not onMarkerAddDocStringTo(clicense,trim(currentline),docobject.Ffunctions[funccounter-1].FLicense) then
      if not onMarkerAddDocStringTo(ccopyright,trim(currentline),docobject.Ffunctions[funccounter-1].FCopyright) then
      if not onMarkerAddDocStringTo(CReturns,trim(currentline),docobject.Ffunctions[funccounter-1].FReturns) then
      if not onMarkerAddDocStringTo(CReturnType,trim(currentline),docobject.Ffunctions[funccounter-1].FReturnType) then
      if not onMarkerAddDocStringTo(CRType,trim(currentline),docobject.Ffunctions[funccounter-1].FRType) then
      if not onMarkerAddDocStringTo(CRaises,trim(currentline),docobject.Ffunctions[funccounter-1].FRaises) then
      if (pos(CParam,trim(currentline)) = 1) or (pos(CParamType,trim(currentline)) = 1) then
      begin
        for prun := 0 to docobject.Ffunctions[funccounter-1].ParamCounter-1 do
        begin
          pname := docobject.Ffunctions[funccounter-1].Fparams[prun].FParamName;
          if not onMarkerAddDocStringTo(CParamType+pname,trim(currentline),docobject.Ffunctions[funccounter-1].Fparams[prun].FParamType) then
          onMarkerAddDocStringTo(CParam+pname,trim(currentline),docobject.Ffunctions[funccounter-1].Fparams[prun].FParamDesc);
        end;
      end
      else
        funcdescription := funcdescription+LineEnding+trim(currentline);
    end
    else if currentline <> '' then
      docfound := true;
    if linecounter < preprocessedlist.Count-2 then
    begin
      inc(linecounter);
      currentline := preprocessedlist.Strings[linecounter];
    end
    else Break;
  end;
  currentlinenumber := linecounter;
  result := true;
end;

function processPrivateDef(var currentlinenumber : integer) : boolean;
var
  indentofdef, linecounter : integer;
  defline, currentline  : string;
begin
  result := false;
  defline := preprocessedlist.Strings[currentlinenumber];
  indentofdef := indentation(defline);
  getDefinitionLine(currentlinenumber);
  linecounter := currentlinenumber;
  currentline := preprocessedlist.Strings[linecounter];
  while ((indentation(currentline) > indentofdef) or (currentline = '')) do
  begin
    if linecounter < preprocessedlist.Count-2 then
    begin
      inc(linecounter);
      currentline := preprocessedlist.Strings[linecounter];
    end
    else Break;
  end;
  currentlinenumber := linecounter;
  result := true;
end;

procedure preprocess();
var
  line, linetoadd : string;
  linenumber : integer;
begin
  linenumber := 0;
  preprocessedlist.Clear;
  while linenumber < sourcelist.Count do
  begin
    line := sourcelist.Strings[linenumber];
    if (line = '') then
      inc(linenumber)
    else if (line[length(line)] = '\') then
    begin
      delete(line, length(line), 1);
      linetoadd := line;
      inc(linenumber);
      line := sourcelist.Strings[linenumber];
      while (line[length(line)] = '\') do
      begin
        delete(line, length(line), 1);
        linetoadd := linetoadd + ' ' + trim(line);
        inc(linenumber);
        line := sourcelist.Strings[linenumber];
      end;
      inc(linenumber);
      linetoadd := linetoadd + ' ' + trim(line);
      preprocessedlist.Add(linetoadd);
    end
    else
    begin
      preprocessedlist.Add(line);
      inc(linenumber);
    end;
  end;
end;

function parseInput_pythonlibrary(filename : string) : boolean;
var
  linenumber, totallines : integer;
  trimmedline : string;
  filedocfound : Boolean;
begin
  filedocfound := false;
  linenumber := 0;
  if Assigned(docobject) and (docobject <> nil) then docobject.Destroy;
  docobject := TFileDoc.Create;
  docobject.Fname:=ExtractFileName(filename);
  preprocess();
  totallines:= preprocessedlist.Count;
  while linenumber < totallines do
    begin
      trimmedline := trim(preprocessedlist.Strings[linenumber]);
      if ((pos(cmulticomment1, trimmedline) = 1) or  (pos(cmulticomment2, trimmedline) = 1)) and not filedocfound then
      begin
        filedocfound := true;
        getFileDoc(linenumber);
      end
      else if (pos(cpydeffunc,trimmedline) = 1) then
      begin
        if pos(cpydefnotpublic,trimmedline) = 1 then
        begin
          filedocfound := true;
          processPrivateDef(linenumber);
        end
        else
        begin
          filedocfound := true;
          processPublicDef(linenumber);
        end;
      end
      else
      begin
        if not ((pos(cpycomment, trimmedline) = 1) or (trimmedline = '')) then
          filedocfound := true;
        inc(linenumber);
      end;
    end;
  result := True;
end;


initialization
  preprocessedlist := TStringlist.create;

finalization
  preprocessedlist.Free;

end.

