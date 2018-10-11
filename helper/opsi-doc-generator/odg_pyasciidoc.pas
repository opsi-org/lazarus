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
  frun, prun : integer;
  tmpstr1, pname : string;
begin
  if Assigned(docobject) and (docobject <> nil) then
  begin
    targetlist.Clear;
    if not (docobject.Author = '') then
      asciidoc_header.Add(':Author:    '+docobject.Author);
    if not (docobject.Email = '') then
      asciidoc_header.Add(':Email:    '+docobject.Email);
    asciidoc_header.Add(':toc:');
    asciidoc_header.Add('   ');
    asciidoc_header.Add('   ');
    asciidoc_header.Add('   ');
    targetlist.Text:= asciidoc_header.Text;
    targetlist.add('[Doc_file'+docobject.name+']');
    targetlist.add('= Documentation of Python library: `'+docobject.name+'`');
    targetlist.Add('');
    targetlist.Add('');
    targetlist.Add('');
    if not (docobject.Email = '') then
       targetlist.Add('* Email:     '+docobject.Email);
    if not (docobject.Version = '') then
       targetlist.Add('* Version:  '+docobject.Version);
    if not (docobject.Copyright = '') then
       targetlist.Add('* Copyright:  '+docobject.Copyright);
    targetlist.Add('');
    targetlist.Add('');
    targetlist.Add('');
    targetlist.Add('');

    for frun := 0 to docobject.functionCounter -1 do
    begin
      //targetlist.Add('--------------------------------');
      targetlist.Add('');
      targetlist.Add('anchor:'+docobject.Ffunctions[frun].Name+'[]');
      targetlist.Add('[Doc_func_'+docobject.Ffunctions[frun].Name+']');
      targetlist.add('== Documentation of local function `'+docobject.Ffunctions[frun].Name+'`');
      targetlist.Add('');
      targetlist.Add('');
      targetlist.add('Definition::');
      targetlist.add('`'+docobject.Ffunctions[frun].Definitionline+'`');
      targetlist.Add('');
      targetlist.add('Description::');
      targetlist.add(docobject.Ffunctions[frun].Description);
      targetlist.Add('');
      for prun := 0 to docobject.Ffunctions[frun].ParamCounter -1 do
      begin
        pname := docobject.Ffunctions[frun].Fparams[prun].ParamName;
        targetlist.add('* Parameter: `' +pname+'`');
        (*
        if docobject.Ffunctions[frun].Fparams[prun].callByReference then
           tmpstr1 := 'Calltype: `CallByReference`'
        else tmpstr1 := 'Calltype: `CallByValue`';


        targetlist.add('** Type: `' +docobject.Ffunctions[frun].Fparams[prun].getParamTypestring
                       + '`  -  '+tmpstr1);
        *)

        tmpstr1 := docobject.Ffunctions[frun].Fparams[prun].ParamDesc;
        if tmpstr1 <> '' then
        begin
          targetlist.add('** Parameter `'+pname+'` Description: +');
          targetlist.add(tmpstr1);
        end;
        tmpstr1 := docobject.Ffunctions[frun].Fparams[prun].ParamAdvice;
        if tmpstr1 <> '' then
        begin
          targetlist.add('** Parameter `'+pname+'` Advice: +');
          targetlist.add(tmpstr1);
        end;
        targetlist.Add('');
      end;

      tmpstr1 := docobject.Ffunctions[frun].Returns;
      if tmpstr1 <> '' then targetlist.Add('* Returns:     '+tmpstr1);

      tmpstr1 := docobject.Ffunctions[frun].OnError;
      if tmpstr1 <> '' then targetlist.Add('* OnError:     '+tmpstr1);

      tmpstr1 := docobject.Ffunctions[frun].SpecialCase;
      if tmpstr1 <> '' then targetlist.Add('* SpecialCase:     '+tmpstr1);

      tmpstr1 := docobject.Ffunctions[frun].Requires;
      if tmpstr1 <> '' then targetlist.Add('* Requires:     '+tmpstr1);

      tmpstr1 := docobject.Ffunctions[frun].References;
      if tmpstr1 <> '' then targetlist.Add('* References:     '+tmpstr1);

      tmpstr1 := docobject.Ffunctions[frun].Links;
      if tmpstr1 <> '' then targetlist.Add('* Links:     '+tmpstr1);

      tmpstr1 := docobject.Ffunctions[frun].Author;
      if tmpstr1 = '' then tmpstr1 :=  docobject.Author;
      if tmpstr1 <> '' then targetlist.Add('* Author:     '+tmpstr1);

      tmpstr1 := docobject.Ffunctions[frun].Date;
      if tmpstr1 = '' then tmpstr1 :=  docobject.Date;
      if tmpstr1 <> '' then targetlist.Add('* Date:     '+tmpstr1);

      tmpstr1 := docobject.Ffunctions[frun].Email;
      if tmpstr1 = '' then tmpstr1 :=  docobject.Email;
      if tmpstr1 <> '' then targetlist.Add('* Email:     '+tmpstr1);

      tmpstr1 := docobject.Ffunctions[frun].Version;
      if tmpstr1 = '' then tmpstr1 :=  docobject.Version;
      if tmpstr1 <> '' then targetlist.Add('* Version:     '+tmpstr1);

      tmpstr1 := docobject.Ffunctions[frun].Copyright;
      if tmpstr1 = '' then tmpstr1 :=  docobject.Copyright;
      if tmpstr1 <> '' then targetlist.Add('* Copyright:     '+tmpstr1);

      tmpstr1 := docobject.Ffunctions[frun].Example;
      if not(tmpstr1 = '') then
      begin
        targetlist.Add('');
        targetlist.Add('');
        targetlist.Add('Example:');
        targetlist.Add('[source,python]');
        targetlist.Add('----');
        targetlist.Add(tmpstr1);
        targetlist.Add('----');
      end;
      targetlist.Add('');
      targetlist.Add('');
    end;
  end;
end;


initialization
  asciidoc_header := TStringlist.create;
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
  //asciidoc_header.Add(':Author:    uib gmbh                                            ');
  //asciidoc_header.Add(':Email:     info@uib.de                                         ');
  asciidoc_header.Add(':Revision:                                                 ');
  asciidoc_header.Add(':doctype: book                                               ');


finalization
  asciidoc_header.Free;

end.

