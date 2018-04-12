unit odg_main;

{$mode objfpc}{$H+}


interface

Type
TParamDoc = class
  private
  FParamName : string;
  FParamCallType :
  FParamType :
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
  FAuthor : string;
  FDate : String;
  FCopyright : string;
  FDescription : string;
  Fparams : array of TParamDoc;
  FOnError : string;
  SpecialCase : string;
  References : string;
  Links : string;
  Requires : string;
  public
    constructor Create;
    destructor Destroy;
  end;

TFileDoc =  class
  private
  name : string;
  filedesc : string;
  functions : array of TFuncDoc;
  public
    constructor Create;
    destructor Destroy;
  end;

const
  cfiledesc = '@filedesc';
  cauthor = '@author';
  cdate = '@date';
  ccopyright = '@copyright';
  CDescription = '@Description';
  COnError = '@OnError';
  CSpecialCase = '@SpecialCase';
  CReferences = '@References';
  CLinks = '@Links';
  CRequires = '@Requires';
  CParamDesc = '@ParamDesc_';
  CParamAdvice = '@ParamAdvice_';


uses
  Classes, SysUtils;

implementation
var
  sourcelist : TStringlist;
  docobject : TSourcefile;

end.

