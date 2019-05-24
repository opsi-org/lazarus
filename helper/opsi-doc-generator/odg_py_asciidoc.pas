unit odg_py_asciidoc;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  odg_py_deffunc,
  oslog,
  SysUtils;

procedure writePyDocToList;

implementation
uses
  odg_main;

var
  asciidoc_header : TStringlist;


procedure writePyDocToList;
var
  frun, prun, i, j, funccount : integer;
  tmpstr1, pname : string;
  tempfile: TFuncDoc;
begin
  LogDatei.log('Writing collected python data as asciidoc to a stringlist',LLnotice);
  if Assigned(docobject) and (docobject <> nil) then
  begin
    targetlist.Clear;
    asciidoc_header.Clear;
    asciidoc_header.Add('////                                                            ');
    asciidoc_header.Add('; Copyright (c) uib gmbh (www.uib.de)                           ');
    asciidoc_header.Add('; This documentation is owned by uib                            ');
    asciidoc_header.Add('; and published under the german creative commons by-sa license ');
    asciidoc_header.Add('; see:                                                          ');
    asciidoc_header.Add('; http://creativecommons.org/licenses/by-sa/3.0/de/             ');
    asciidoc_header.Add('; http://creativecommons.org/licenses/by-sa/3.0/de/legalcode    ');
    asciidoc_header.Add('; english:                                                      ');
    asciidoc_header.Add('; http://creativecommons.org/licenses/by-sa/3.0/                ');
    asciidoc_header.Add('; http://creativecommons.org/licenses/by-sa/3.0/legalcode       ');
    asciidoc_header.Add(';                                                               ');
    asciidoc_header.Add(';                          ');
    asciidoc_header.Add('////                                                            ');
    asciidoc_header.Add('                                                                ');
    asciidoc_header.Add(':Revision:                                                 ');
    asciidoc_header.Add(':doctype: book                                               ');

    {
    LogDatei.log('Writing file information for: '+docobject.name,LLinfo);
    if not (docobject.Author = '') then
      asciidoc_header.Add(':Author:    '+docobject.Author);
    }

    asciidoc_header.Add(':toc:');
    asciidoc_header.Add('   ');
    asciidoc_header.Add('   ');
    targetlist.Text:= asciidoc_header.Text;
    targetlist.Add('');
    targetlist.Add('= opsi API documentation');
    targetlist.Add('');

    funccount :=  docobject.functionCounter;
    for i:=0 to  funccount-2 do
    begin
      for j:=0 to (funccount-i-2) do
      begin
        if LowerCase(docobject.Ffunctions[j].Name) > LowerCase(docobject.Ffunctions[j+1].Name) then
        begin
          tempfile:= docobject.Ffunctions[j];
          docobject.Ffunctions[j]:= docobject.Ffunctions[j+1];
          docobject.Ffunctions[j+1] := tempfile;
        end;
      end;
    end;

    for frun := 0 to funccount-1 do
    begin
      LogDatei.log('Writing function information for: '+docobject.Ffunctions[frun].Name,LLinfo);
      targetlist.Add('anchor:'+docobject.Ffunctions[frun].Name+'[]');
      targetlist.Add('[Doc_func_'+docobject.Ffunctions[frun].Name+']');
      targetlist.Add('== `'+docobject.Ffunctions[frun].Name+'()`');
      {
      targetlist.add('== `'+docobject.Ffunctions[frun].Definitionline+'`');
      targetlist.Add('');
      }
      targetlist.add(docobject.Ffunctions[frun].Description);
      targetlist.Add('');

      for prun := 0 to docobject.Ffunctions[frun].ParamCounter -1 do
      begin
        pname := docobject.Ffunctions[frun].Fparams[prun].ParamName;
        LogDatei.log('Writing parameter information for: '+pname,LLinfo);
        targetlist.add('* Parameter:  `' +pname+'`');
        tmpstr1 := docobject.Ffunctions[frun].Fparams[prun].ParamType;
        if tmpstr1 <> '' then
        begin
          targetlist.add('** Type: `'+tmpstr1+'`');
        end;
        tmpstr1 := docobject.Ffunctions[frun].Fparams[prun].ParamDesc;
        if tmpstr1 <> '' then
        begin
          targetlist.add('** Description: '+tmpstr1);
        end;
        targetlist.Add('');
      end;

      tmpstr1 := docobject.Ffunctions[frun].RType;
      if tmpstr1 <> '' then targetlist.Add('Returned Type: `'+tmpstr1+'`');
      targetlist.Add('');

      tmpstr1 := docobject.Ffunctions[frun].Raises;
      if tmpstr1 <> '' then targetlist.Add('Raises: '+tmpstr1);
      targetlist.Add('');
      targetlist.Add('');
    end;
  end;
  LogDatei.log('Finished writing collected python data as asciidoc to a stringlist',LLinfo);
end;


initialization
  asciidoc_header := TStringlist.create;

finalization
  asciidoc_header.Free;
end.

