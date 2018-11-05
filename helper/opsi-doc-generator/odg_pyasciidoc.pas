unit odg_pyasciidoc;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  odg_py_deffunc,
  SysUtils;

procedure writePyDocToList;

implementation
uses
  odg_main;

var
  asciidoc_header : TStringlist;

procedure writePyDocToList;
var
  frun, prun: integer;
  tmpstr1, pname : string;
begin
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

    if not (docobject.Author = '') then
      asciidoc_header.Add(':Author:    '+docobject.Author);
    asciidoc_header.Add(':toc:');
    asciidoc_header.Add('   ');
    asciidoc_header.Add('   ');
    asciidoc_header.Add('   ');
    targetlist.Text:= asciidoc_header.Text;
    targetlist.add('[Doc_file_'+docobject.name+']');
    targetlist.add('= Documentation of Python library: `'+docobject.name+'`');
    targetlist.Add('');
    //targetlist.Add('');

    if not (docobject.filedesc = '') then
       targetlist.Add('* Description:  ' + docobject.filedesc);
    
    if not (docobject.License = '') then
      targetlist.Add('* License:  '+docobject.License);

    if not (docobject.Copyright = '') then
      targetlist.Add('* Copyright:  '+docobject.Copyright);

    targetlist.Add('');
    targetlist.Add('');
    targetlist.Add('');
    targetlist.Add('');

    for frun := 0 to docobject.functionCounter -1 do
    begin
      targetlist.Add('');
      targetlist.Add('anchor:'+docobject.Ffunctions[frun].Name+'[]');
      targetlist.Add('[Doc_func_'+docobject.Ffunctions[frun].Name+']');
      targetlist.add('== Documentation of local function `'+docobject.Ffunctions[frun].Name+'`');
      targetlist.Add('');
      //targetlist.Add('');
      targetlist.add('Definition:: ');
      targetlist.add('`'+docobject.Ffunctions[frun].Definitionline+'`');
      targetlist.Add('');
      targetlist.add('Description:: ');
      targetlist.add(docobject.Ffunctions[frun].Description);
      targetlist.Add('');
      //targetlist.Add('');
      for prun := 0 to docobject.Ffunctions[frun].ParamCounter -1 do
      begin
        pname := docobject.Ffunctions[frun].Fparams[prun].ParamName;
        targetlist.add('* Parameter: `' +pname+'`');
        tmpstr1 := docobject.Ffunctions[frun].Fparams[prun].ParamType;
        if tmpstr1 <> '' then
        begin
          targetlist.add('** Parameter `'+pname+'` Type: +');
          targetlist.add(tmpstr1);
        end;
        tmpstr1 := docobject.Ffunctions[frun].Fparams[prun].ParamDesc;
        if tmpstr1 <> '' then
        begin
          targetlist.add('** Parameter `'+pname+'` Description: +');
          targetlist.add(tmpstr1);
        end;
        targetlist.Add('');
      end;

      tmpstr1 := docobject.Ffunctions[frun].Returns;
      if tmpstr1 <> '' then targetlist.Add('* Return:  '+tmpstr1);

      tmpstr1 := docobject.Ffunctions[frun].ReturnType;
      if tmpstr1 <> '' then targetlist.Add('* ReturnType:  '+tmpstr1);

      tmpstr1 := docobject.Ffunctions[frun].RType;
      if tmpstr1 <> '' then targetlist.Add('* RType:  '+tmpstr1);

      tmpstr1 := docobject.Ffunctions[frun].Raises;
      if tmpstr1 <> '' then targetlist.Add('* Raises:  '+tmpstr1);

      tmpstr1 := docobject.Ffunctions[frun].License;
      if tmpstr1 = '' then tmpstr1 :=  docobject.License;
      if tmpstr1 <> '' then targetlist.Add('* License:  '+tmpstr1);

      tmpstr1 := docobject.Ffunctions[frun].Author;
      if tmpstr1 = '' then tmpstr1 :=  docobject.Author;
      if tmpstr1 <> '' then targetlist.Add('* Author:  '+tmpstr1);

      tmpstr1 := docobject.Ffunctions[frun].Copyright;
      if tmpstr1 = '' then tmpstr1 :=  docobject.Copyright;
      if tmpstr1 <> '' then targetlist.Add('* Copyright:  '+tmpstr1);

      targetlist.Add('');
      targetlist.Add('');
    end;
  end;
end;


initialization
  asciidoc_header := TStringlist.create;

finalization
  asciidoc_header.Free;
end.

