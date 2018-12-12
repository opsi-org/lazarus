unit osregex;   //regular expression unit for opsi-script

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RegExpr;

function isRegexMatch(inputtext,expr : string) : boolean;
function getSubListByRegexpMatch(expr : string; inputlist : TStringList) : TStringList;
function getSubListByRegexpMatch(exprlist : TStringList; inputlist : TStringList) : TStringList;
function removeFromListByRegexpMatch(expr : string; inputlist : TStringList) : TStringList;
function removeFromListByRegexpMatch(exprlist : TStringList; inputlist : TStringList) : TStringList;
function editStringRegexp(inputtext, expr, replacetext : string) : string;
function editStringRegexp(inputlist : TStringList; expr, replacetext : string) : TStringList;


implementation

function isRegexMatch(inputtext,expr : string) : boolean;
//Returns true if the expr matches the inputtext.
var
  regexobj : TRegExpr;
begin
  result := false;
  regexobj := TRegExpr.Create;
  try
    regexobj.Expression := expr;
    if regexobj.Exec(inputtext) then
      result := true;
  finally
    regexobj.Free;
  end;
end;

// (pointer assignment and freeing result stringlist) --->
//https://stackoverflow.com/questions/561282/how-do-i-return-an-object-from-a-function-in-delphi-without-causing-access-viola

function getSubListByRegexpMatch(expr : string; inputlist : TStringList) : TStringList;
// Returns a matching sublist of inputlist
var
  regexobj : TRegExpr;
  linecounter : integer;
  currentline : string;
begin
  regexobj := TRegExpr.Create;
  regexobj.Expression := expr;

  try
    result := TStringList.Create;
    try
      for linecounter:=0 to inputlist.Count-1  do
      begin
        currentline := trim(inputlist.Strings[linecounter]);
        if regexobj.Exec(currentline) then
        begin
          result.Add(regexobj.Match[0]);
          while regexobj.ExecNext do
          begin
            result.Add(regexobj.Match[0]);
          end;
          {
          for matchcount:=0 to regexobj.SubExprMatchCount do
          begin
            result.Add(regexobj.Match[matchcount]);
          end;
          }
        end;
      end;

      for linecounter := 0 to result.Count-1 do
        WriteLn(result[linecounter]);

    finally
      if Assigned(result) then
        result.Free;
    end;
  finally
    regexobj.Free;
  end;
end;

function getSubListByRegexpMatch(exprlist : TStringList; inputlist : TStringList) : TStringList;
// Returns a matching sublist of inputlist after checking with a list of regular expressions
var
  regexobj : TRegExpr;
  regexcounter, linecounter : integer;
  currentline : string;
begin
  for regexcounter:=0 to exprlist.Count-1 do
  begin
    regexobj := TRegExpr.Create;
    regexobj.Expression := trim(exprlist.Strings[regexcounter]);
    try
      result := TStringList.Create;
      try
        for linecounter:=0 to inputlist.Count-1  do
        begin
          currentline := trim(inputlist.Strings[linecounter]);
          if regexobj.Exec(currentline) then
          begin
            result.Add(regexobj.Match[0]);
            while regexobj.ExecNext do
              result.Add(regexobj.Match[0]);
          end;
        end;
        for linecounter := 0 to result.Count-1 do
          WriteLn(result[linecounter]);
      finally
        if Assigned(result) then
          result.Free;
      end;
    finally
      regexobj.Free;
    end;
  end;
end;

function removeFromListByRegexpMatch(expr : string; inputlist : TStringList) : TStringList;
// remove matches from inputlist after checking with a regular expression.
var
  regexobj : TRegExpr;
  linecounter : integer;
  currentline : string;
begin
  regexobj := TRegExpr.Create;
  regexobj.Expression := expr;
  try
    result := TStringList.Create;
    try
      for linecounter:=0 to inputlist.Count-1  do
      begin
        currentline := trim(inputlist.Strings[linecounter]);
        if regexobj.Exec(currentline) then
        begin
          Delete(currentline, regexobj.MatchPos[0], regexobj.MatchLen[0]);
          while regexobj.ExecNext do
            Delete(currentline, regexobj.MatchPos[0], regexobj.MatchLen[0]);
        end;
        result.Add(currentline);
      end;

      for linecounter := 0 to result.Count-1 do
        WriteLn(result[linecounter]);
    finally
      if Assigned(result) then
        result.Free;
    end;
  finally
    regexobj.Free;
  end;
end;

function removeFromListByRegexpMatch(exprlist : TStringList; inputlist : TStringList) : TStringList;
// removes the matches from inputlist after checking with a list of regular expressions
var
  regexobj : TRegExpr;
  regexcounter, linecounter : integer;
  currentline : string;
begin
  for regexcounter:=0 to exprlist.Count-1 do
  begin
    regexobj := TRegExpr.Create;
    regexobj.Expression := trim(exprlist.Strings[regexcounter]);
    try
      result := TStringList.Create;
      try
        for linecounter:=0 to inputlist.Count-1  do
        begin
          currentline := trim(inputlist.Strings[linecounter]);
          if regexobj.Exec(currentline) then
          begin
            Delete(currentline, regexobj.MatchPos[0], regexobj.MatchLen[0]);
            while regexobj.ExecNext do
              Delete(currentline, regexobj.MatchPos[0], regexobj.MatchLen[0]);
          end;
          result.Add(currentline);
        end;

        for linecounter := 0 to result.Count-1 do
          WriteLn(result[linecounter]);

      finally
        if Assigned(result) then
          result.Free;
      end;
    finally
      regexobj.Free;
    end;
  end;
end;

function editStringRegexp(inputtext, expr, replacetext : string) : string;
//Replace the regex match with replacetext.
begin
  result:= '';
  inputtext:= ReplaceRegExpr(expr, inputtext, replacetext, True);
  result:= inputtext;
end;

function editStringRegexp(inputlist : TStringList; expr, replacetext : string) : TStringList;
//Replace the regex match from inputlist with replacetext.
var
  linecounter : integer;
  currentline : string;
begin
  try
    result:= TStringList.Create;
    for linecounter:=0 to inputlist.Count-1  do
    begin
      currentline := trim(inputlist.Strings[linecounter]);
      currentline:= ReplaceRegExpr(expr, currentline, replacetext, True);
      result.Add(currentline);
    end;

    for linecounter := 0 to result.Count-1 do
      WriteLn(result[linecounter]);

  finally
    if Assigned(result) then
      result.Free;
  end;
end;

end.


