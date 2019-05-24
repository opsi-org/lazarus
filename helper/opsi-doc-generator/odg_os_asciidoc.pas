unit odg_os_asciidoc;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  odg_os_deffunc,
  oslog,
  SysUtils;

procedure writeDocToList;

implementation
uses
  odg_main;

var
  asciidoc_header : TStringlist;

procedure writeDocToList;
var
  frun, prun, funccount, i, j : integer;
  tmpstr1, pname : string;
  tempfile: TFuncDoc;
begin
  LogDatei.log('Writing collected opsiscript data as asciidoc to stringlist',LLnotice);
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
    if not (docobject.Email = '') then
      asciidoc_header.Add(':Email:    '+docobject.Email);
    }

    asciidoc_header.Add(':toc:');
    asciidoc_header.Add('   ');
    asciidoc_header.Add('   ');
    targetlist.Text:= asciidoc_header.Text;
    targetlist.Add('');
    targetlist.Add('= opsi API documentation');
    targetlist.Add('');

    funccount :=  docobject.functionCounter;
    for i:=0 to funccount-2 do
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

    for frun := 0 to funccount -1 do
    begin
      LogDatei.log('Writing function information for: '+docobject.Ffunctions[frun].Name,LLinfo);
      targetlist.Add('anchor:'+docobject.Ffunctions[frun].Name+'[]');
      targetlist.Add('[Doc_func_'+docobject.Ffunctions[frun].Name+']');
      targetlist.Add('== '+docobject.Ffunctions[frun].Name+'()');
      targetlist.Add('');
      targetlist.Add('`' + docobject.Ffunctions[frun].Definitionline +'`');
      targetlist.Add('');
      targetlist.add(docobject.Ffunctions[frun].Description);
      targetlist.Add('');

      for prun := 0 to docobject.Ffunctions[frun].ParamCounter -1 do
      begin
        pname := docobject.Ffunctions[frun].Fparams[prun].ParamName;
        LogDatei.log('Writing parameter information for: '+pname,LLinfo);
        targetlist.add('* Parameter: `' +pname+'`');
        if docobject.Ffunctions[frun].Fparams[prun].callByReference then
           tmpstr1 := 'Calltype: `CallByReference`'
        else tmpstr1 := 'Calltype: `CallByValue`';
        targetlist.add('** Type: `'+docobject.Ffunctions[frun].Fparams[prun].getParamTypestring
                       + '`  -  '+tmpstr1);
        tmpstr1 := docobject.Ffunctions[frun].Fparams[prun].ParamDesc;
        if tmpstr1 <> '' then
        begin
          targetlist.add('** Description: '+tmpstr1);
        end;
        tmpstr1 := docobject.Ffunctions[frun].Fparams[prun].ParamAdvice;
        if tmpstr1 <> '' then
        begin
          targetlist.add('** Advice: '+tmpstr1);
        end;
        targetlist.Add('');
      end;

      tmpstr1 := docobject.Ffunctions[frun].Returns;
      if tmpstr1 <> '' then targetlist.Add('Returns: '+tmpstr1);
      targetlist.Add('');

      tmpstr1 := docobject.Ffunctions[frun].OnError;
      if tmpstr1 <> '' then targetlist.Add('OnError: '+tmpstr1);
      targetlist.Add('');

      tmpstr1 := docobject.Ffunctions[frun].SpecialCase;
      if tmpstr1 <> '' then targetlist.Add('SpecialCase: '+tmpstr1);
      targetlist.Add('');

      tmpstr1 := docobject.Ffunctions[frun].Requires;
      if tmpstr1 <> '' then targetlist.Add('Requires: '+tmpstr1);
      targetlist.Add('');

      tmpstr1 := docobject.Ffunctions[frun].References;
      if tmpstr1 <> '' then targetlist.Add('References: '+tmpstr1);
      targetlist.Add('');

      tmpstr1 := docobject.Ffunctions[frun].Links;
      if tmpstr1 <> '' then targetlist.Add('Links: '+tmpstr1);
      targetlist.Add('');

      {
      tmpstr1 := docobject.Ffunctions[frun].Author;
      if tmpstr1 = '' then tmpstr1 :=  docobject.Author;
      if tmpstr1 <> '' then targetlist.Add('Author: '+tmpstr1);
      targetlist.Add('');

      tmpstr1 := docobject.Ffunctions[frun].Date;
      if tmpstr1 = '' then tmpstr1 :=  docobject.Date;
      if tmpstr1 <> '' then targetlist.Add('Date: '+tmpstr1);
      targetlist.Add('');

      tmpstr1 := docobject.Ffunctions[frun].Email;
      if tmpstr1 = '' then tmpstr1 :=  docobject.Email;
      if tmpstr1 <> '' then targetlist.Add('Email: '+tmpstr1);
      targetlist.Add('');

      tmpstr1 := docobject.Ffunctions[frun].Version;
      if tmpstr1 = '' then tmpstr1 :=  docobject.Version;
      if tmpstr1 <> '' then targetlist.Add('Version: '+tmpstr1);
      targetlist.Add('');

      tmpstr1 := docobject.Ffunctions[frun].Copyright;
      if tmpstr1 = '' then tmpstr1 :=  docobject.Copyright;
      if tmpstr1 <> '' then targetlist.Add('Copyright: '+tmpstr1);
      targetlist.Add('');
      }

      tmpstr1 := docobject.Ffunctions[frun].Example;
      if not(tmpstr1 = '') then
      begin
        targetlist.Add('');
        targetlist.Add('');
        targetlist.Add('Example:');
        targetlist.Add('[source,winst]');
        targetlist.Add('----');
        targetlist.Add(tmpstr1);
        targetlist.Add('----');
      end;
      targetlist.Add('');
      targetlist.Add('');
    end;
  end;
  LogDatei.log('Finished writing collected opsiscript data as asciidoc to stringlist',LLinfo);
end;



initialization
  asciidoc_header := TStringlist.create;
  {
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
  }


finalization
  asciidoc_header.Free;

end.

