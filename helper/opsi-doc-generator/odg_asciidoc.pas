unit odg_asciidoc;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  odg_os_deffunc,
  SysUtils;

procedure writeDocToList;

implementation
uses
  odg_main;

var
  asciidoc_header : TStringlist;

procedure writeDocToList;
var
  frun, prun : integer;
  tmpstr1, pname : string;
begin
  if Assigned(docobject) and (docobject <> nil) then
  begin
    targetlist.Clear;
    targetlist.Text:= asciidoc_header.Text;
    targetlist.add('[Doc_file'+docobject.name+']');
    targetlist.add('= Documentation of '+docobject.name);
        targetlist.Add('');
    targetlist.add('Description::');
    targetlist.Add(docobject.filedesc);
    targetlist.Add('');
    targetlist.Add('');
    for frun := 0 to docobject.functionCounter -1 do
    begin
      //targetlist.Add('--------------------------------');
      targetlist.Add('[Doc_func_'+docobject.Ffunctions[frun].Name+']');
      targetlist.add('== Documentation of local fuction '+docobject.Ffunctions[frun].Name);
      targetlist.Add('');
      targetlist.add('Definition::');
      targetlist.add(docobject.Ffunctions[frun].Definitionline);
      targetlist.Add('');
      targetlist.add('Description::');
      targetlist.add(docobject.Ffunctions[frun].Description);
      targetlist.Add('');
      for prun := 0 to docobject.Ffunctions[frun].ParamCounter -1 do
      begin
        pname := docobject.Ffunctions[frun].Fparams[prun].ParamName;
        targetlist.add('* Parameter:' +pname+' :');
        targetlist.add('** Type:' +docobject.Ffunctions[frun].Fparams[prun].getParamTypestring);
        if docobject.Ffunctions[frun].Fparams[prun].callByReference then
           tmpstr1 := 'Calltype: CallByReference'
        else tmpstr1 := 'Calltype: CallByValue';
        targetlist.add('** Type:' +docobject.Ffunctions[frun].Fparams[prun].getParamTypestring
                       + '  -  '+tmpstr1);
        tmpstr1 := docobject.Ffunctions[frun].Fparams[prun].ParamDesc;
        if tmpstr1 <> '' then
        begin
          targetlist.add('** Parameter '+pname+' Description: +');
          targetlist.add(tmpstr1);
        end;
        tmpstr1 := docobject.Ffunctions[frun].Fparams[prun].ParamAdvice;
        if tmpstr1 <> '' then
        begin
          targetlist.add('** Parameter '+pname+' Advice: +');
          targetlist.add(tmpstr1);
        end;
        targetlist.Add('');
      end;
      targetlist.add('* Returns: '+docobject.Ffunctions[frun].Returns);
      targetlist.add('* OnError: '+docobject.Ffunctions[frun].OnError);
      targetlist.add('* SpecialCase: '+docobject.Ffunctions[frun].SpecialCase);
      targetlist.add('* Author: '+docobject.Ffunctions[frun].Author);
      targetlist.add('* Date: '+docobject.Ffunctions[frun].Date);
      targetlist.add('* Copyright: '+docobject.Ffunctions[frun].Copyright);
      targetlist.add('* Requires: '+docobject.Ffunctions[frun].Requires);
      targetlist.add('* References: '+docobject.Ffunctions[frun].References);
      targetlist.add('* Links: '+docobject.Ffunctions[frun].Links);
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
  asciidoc_header.Add(':Author:    uib gmbh                                            ');
  asciidoc_header.Add(':Email:     info@uib.de                                         ');
  asciidoc_header.Add(':Revision:                                                 ');
  asciidoc_header.Add(':doctype: book                                               ');
  asciidoc_header.Add('                                                                ');
  asciidoc_header.Add('                                ');
  asciidoc_header.Add('                      ');


finalization
  asciidoc_header.Free;

end.

