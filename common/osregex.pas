unit osregex;   //regular expression unit for opsi-script

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RegExpr; //oslog;

function isRegexMatch(inputtext, expr: string): boolean;
//Returns true if regex matches the inputtext.
function getSubListByContainingRegex(expr: string; inputlist: TStringList): TStringList;
//Returns list of matching lines for a single regex.
function getSubListByContainingRegex(exprlist: TStringList;
  inputlist: TStringList): TStringList;
//Returns list of matching lines for a list of regex.
function getRegexMatchString(expr: string; inputstring: string): String;
//Returns string of first exact match for a single regex.
function getRegexMatchList(expr: string; inputlist: TStringList): TStringList;
//Returns list of exact matches for a single regex.
function getRegexMatchList(exprlist: TStringList; inputlist: TStringList): TStringList;
//Returns list of exact matches for a list of regex.
function removeFromListByContainingRegex(expr: string;
  inputlist: TStringList): TStringList; //remove matching lines for a single regex.
function removeFromListByContainingRegex(exprlist: TStringList;
  inputlist: TStringList): TStringList; //remove matching lines for a list of regex.
function stringReplaceRegex(inputtext, expr, replacetext: string): string;
//Replace matches in string with replacetext.
function stringReplaceRegexInList(inputlist: TStringList;
  expr, replacetext: string): TStringList;
//Replace matches in the stringlist with replacetext.


implementation

function isRegexMatch(inputtext, expr: string): boolean;
  //Returns true if regex matches the inputtext.
var
  regexobj: TRegExpr;
begin
  Result := False;
  regexobj := TRegExpr.Create;
  try
    if expr <> '' then
      try
        regexobj.Expression := expr;
        if trim(inputtext) <> '' then
        begin
          if regexobj.Exec(inputtext) then //exception if string is empty
            Result := True;
        end;
      except
        on E: Exception do
        begin
          //Logdatei.log('Exception in isRegexMatch: ' + E.ClassName + ': ' + E.Message, LLError);
          Result := False;
        end;
      end;
  finally
    regexobj.Free;
  end;
end;

function getSubListByContainingRegex(expr: string; inputlist: TStringList): TStringList;
  //Returns list of matching lines for a single regex.
var
  regexobj: TRegExpr;
  linecounter: integer;
  currentline: string;
begin
  try
    try
      regexobj := TRegExpr.Create;
      regexobj.Expression := expr;

      Result := TStringList.Create;
      for linecounter := 0 to inputlist.Count - 1 do
      begin
        currentline := inputlist.Strings[linecounter];
        if trim(currentline) <> '' then
          if regexobj.Exec(currentline) then
            Result.Add(currentline);
      end;
    except
      on E: Exception do
      begin
        //Result := False;
      end;
    end;
  finally
    regexobj.Free;
  end;
end;

function getSubListByContainingRegex(exprlist: TStringList;
  inputlist: TStringList): TStringList;
  // Returns list of matching lines for a list of regex.
var
  regexobj: TRegExpr;
  linecounter: integer;
  currentline: string;
  i: integer;
begin
  try
    try
      Result := TStringList.Create;
      regexobj := TRegExpr.Create;

      // remove empty expressions from expression list
      for i := exprlist.Count - 1 downto 0 do
      begin
        if Trim(exprlist[i]) = '' then
          exprlist.Delete(i);
      end;

      exprlist.Delimiter := '|';
      regexobj.Expression := exprlist.DelimitedText;

      for linecounter := 0 to inputlist.Count - 1 do
      begin
        currentline := inputlist.Strings[linecounter];
        if trim(currentline) <> '' then
          if regexobj.Exec(currentline) then
          begin
            Result.Add(currentline);
          end;
      end;
    except
      on E: Exception do
      begin
        //Result := False;
      end;
    end;
  finally
    regexobj.Free;
  end;
end;

function getRegexMatchString(expr: string; inputstring: string): String;
  //Returns string of first exact match for a single regex.
var
  regexobj: TRegExpr;
begin
  try
    try
      Result := '';

      if trim(expr) <> '' then
      begin
        regexobj := TRegExpr.Create;
        regexobj.Expression := expr;

          if trim(inputstring) <> '' then
            if regexobj.Exec(inputstring) then
            begin
              Result := regexobj.Match[0];
            end;
        end;
    except
      on E: Exception do
      begin
        //Result := False;
      end;
    end;
  finally
    regexobj.Free;
  end;
end;


function getRegexMatchList(expr: string; inputlist: TStringList): TStringList;
  //Returns list of exact matches for a single regex.
var
  regexobj: TRegExpr;
  linecounter: integer;
  currentline: string;
begin
  try
    try
      Result := TStringList.Create;

      if trim(expr) <> '' then
      begin
        regexobj := TRegExpr.Create;
        regexobj.Expression := expr;

        for linecounter := 0 to inputlist.Count - 1 do
        begin
          currentline := inputlist.Strings[linecounter];
          if trim(currentline) <> '' then
            if regexobj.Exec(currentline) then
            begin
              Result.Add(regexobj.Match[0]);
              while regexobj.ExecNext do
                Result.Add(regexobj.Match[0]);
            end;
        end;
      end;
    except
      on E: Exception do
      begin
        //Result := False;
      end;
    end;
  finally
    regexobj.Free;
  end;
end;

function getRegexMatchList(exprlist: TStringList; inputlist: TStringList): TStringList;

  //Returns list of exact matches for a list of regex.
var
  regexobj: TRegExpr;
  linecounter: integer;
  currentline: string;
  i: integer;
begin
  try
    try
      Result := TStringList.Create;
      regexobj := TRegExpr.Create;

      // remove empty expressions from expression list
      for i := exprlist.Count - 1 downto 0 do
      begin
        if Trim(exprlist[i]) = '' then
          exprlist.Delete(i);
      end;

      exprlist.Delimiter := '|';
      regexobj.Expression := exprlist.DelimitedText;

      for linecounter := 0 to inputlist.Count - 1 do
      begin
        currentline := inputlist.Strings[linecounter];
        if trim(currentline) <> '' then
          if regexobj.Exec(currentline) then
          begin
            Result.Add(regexobj.Match[0]);
            while regexobj.ExecNext do
              Result.Add(regexobj.Match[0]);
          end;
      end;
    except
      on E: Exception do
      begin
        //Result := False;
      end;
    end;
  finally
    regexobj.Free;
  end;
end;

function removeFromListByContainingRegex(expr: string;
  inputlist: TStringList): TStringList;
  //remove matching lines for a single regex.
var
  regexobj: TRegExpr;
  linecounter: integer;
  currentline: string;
begin
  try
    try
      regexobj := TRegExpr.Create;
      regexobj.Expression := expr;

      Result := TStringList.Create;

      for linecounter := 0 to inputlist.Count - 1 do
      begin
        currentline := inputlist.Strings[linecounter];
        if trim(currentline) <> '' then
          if not regexobj.Exec(currentline) then
          begin
            Result.Add(currentline);
          end;
      end;
    except
      on E: Exception do
      begin
        //Result := False;
      end;
    end;
  finally
    regexobj.Free;
  end;
end;

function removeFromListByContainingRegex(exprlist: TStringList;
  inputlist: TStringList): TStringList;
  //remove matching lines for a list of regex.
var
  regexobj: TRegExpr;
  linecounter: integer;
  currentline: string;
begin
  try
    try
      Result := TStringList.Create;
      regexobj := TRegExpr.Create;

      exprlist.Delimiter := '|';
      regexobj.Expression := exprlist.DelimitedText;

      for linecounter := 0 to inputlist.Count - 1 do
      begin
        currentline := inputlist.Strings[linecounter];
        if trim(currentline) <> '' then
          if not regexobj.Exec(currentline) then
            Result.Add(currentline);
      end;
    except
      on E: Exception do
      begin
        //Result := False;
      end;
    end;
  finally
    regexobj.Free;
  end;
end;

function stringReplaceRegex(inputtext, expr, replacetext: string): string;
  //Replace matches in string with replacetext.
begin
  Result := '';
  if (inputtext <> '') and (expr <> '') then
    inputtext := ReplaceRegExpr(expr, inputtext, replacetext, True);
  Result := inputtext;
end;

function stringReplaceRegexInList(inputlist: TStringList;
  expr, replacetext: string): TStringList;
  //Replace matches in the stringlist with replacetext.
var
  linecounter: integer;
  currentline: string;
begin
  Result := TStringList.Create;
  for linecounter := 0 to inputlist.Count - 1 do
  begin
    currentline := trim(inputlist.Strings[linecounter]);
    if currentline <> '' then
      currentline := ReplaceRegExpr(expr, currentline, replacetext, True);
    Result.Add(currentline);
  end;
end;

end.
