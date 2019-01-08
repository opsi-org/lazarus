unit osregularexprtest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, osregex;

type
  OsRegexpTest= class(TTestCase)
  private
    inputlist, exprlist : TStringList;
    expr : string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure When_isRegexMatch_Then_True;
    procedure When_isRegexMatch_Then_False;
    procedure When_getSubListByContainingRegex_ForSingleExpr_Then_ProvideMatchingLines;
    procedure When_getSubListByContainingRegex_ForExprList_Then_ProvideMatchingLines;
    procedure When_getRegexMatchList_ForSingleExpr_Then_ProvideExactMatches;
    procedure When_getRegexMatchList_ForExprList_Then_ProvideExactMatches;
    procedure When_removeFromListByContainingRegex_ForSingleExpr_Then_RemoveMatchingLines;
    procedure When_removeFromListByContainingRegex_ForExprList_Then_RemoveMatchingLines;
    procedure When_stringReplaceRegex_Then_ReplaceMatchInString;
    procedure When_stringReplaceRegexInList_Then_ReplaceMatchInStringlist;
  end;

implementation

procedure OsRegexpTest.SetUp;
begin
  expr := '\w+(\.[\w-]+)*@([\w-]+\.)+[a-zA-Z]{2,6}';

  inputlist := TStringList.Create;
  exprlist := TStringList.Create;

  inputlist.Add('uib gmbh');
  inputlist.Add('client management system');
  inputlist.Add('Please e-mail us at example@xyz.com and example2@xyz.com');
  inputlist.Add('example3@xyz.com');
  inputlist.Add('opsi by uib gmbh');
  inputlist.Add('email : xyz@abc.com');

  exprlist.Add(expr);
  exprlist.Add('.*uib');
end;

procedure OsRegexpTest.TearDown;
begin
  inputlist.Free;
  exprlist.Free;
end;

procedure OsRegexpTest.When_isRegexMatch_Then_True;
begin
  AssertEquals('With a matching text and regex as input', True, isRegexMatch('uib gmbh','.*uib.*'));
end;

procedure OsRegexpTest.When_isRegexMatch_Then_False;
begin
  AssertEquals('With a non matching text and regex as input', False, isRegexMatch('gmbh','.*uib.*'));
end;

procedure OsRegexpTest.When_getSubListByContainingRegex_ForSingleExpr_Then_ProvideMatchingLines;
var
  expectedlist : TStringList;
begin
  expectedlist := TStringList.Create;
  expectedlist.Add('Please e-mail us at example@xyz.com and example2@xyz.com');
  expectedlist.Add('example3@xyz.com');
  expectedlist.Add('email : xyz@abc.com');

  AssertEquals('With an regex and stringlist as input', expectedlist.Text, getSubListByContainingRegex(expr, inputlist).Text);
  expectedlist.Free;
end;

procedure OsRegexpTest.When_getSubListByContainingRegex_ForExprList_Then_ProvideMatchingLines;
var
  expectedlist : TStringList;
begin
  expectedlist := TStringList.Create;
  expectedlist.Add('uib gmbh');
  expectedlist.Add('Please e-mail us at example@xyz.com and example2@xyz.com');
  expectedlist.Add('example3@xyz.com');
  expectedlist.Add('opsi by uib gmbh');
  expectedlist.Add('email : xyz@abc.com');

  AssertEquals('With an regexlist and stringlist as input', expectedlist.Text, getSubListByContainingRegex(exprlist, inputlist).Text);
  expectedlist.Free;
end;

procedure OsRegexpTest.When_getRegexMatchList_ForSingleExpr_Then_ProvideExactMatches;
var
  expectedlist : TStringList;
begin
  expectedlist := TStringList.Create;
  expectedlist.Add('example@xyz.com');
  expectedlist.Add('example2@xyz.com');
  expectedlist.Add('example3@xyz.com');
  expectedlist.Add('xyz@abc.com');

  AssertEquals('With expr and stringlist as input', expectedlist.Text, getRegexMatchList(expr, inputlist).Text);
  expectedlist.Free;
end;

procedure OsRegexpTest.When_getRegexMatchList_ForExprList_Then_ProvideExactMatches;
var
  expectedlist : TStringList;
begin
  expectedlist := TStringList.Create;
  expectedlist.Add('uib');
  expectedlist.Add('example@xyz.com');
  expectedlist.Add('example2@xyz.com');
  expectedlist.Add('example3@xyz.com');
  expectedlist.Add('opsi by uib');
  expectedlist.Add('xyz@abc.com');

  AssertEquals('With exprlist and stringlist as input', expectedlist.Text, getRegexMatchList(exprlist, inputlist).Text);
  expectedlist.Free;
end;

procedure OsRegexpTest.When_removeFromListByContainingRegex_ForSingleExpr_Then_RemoveMatchingLines;
var
  expectedlist : TStringList;
begin
  expectedlist := TStringList.Create;
  expectedlist.Add('uib gmbh');
  expectedlist.Add('client management system');
  expectedlist.Add('opsi by uib gmbh');

  AssertEquals('With expr and stringlist as input', expectedlist.Text, removeFromListByContainingRegex(expr, inputlist).Text);
  expectedlist.Free;
end;

procedure OsRegexpTest.When_removeFromListByContainingRegex_ForExprList_Then_RemoveMatchingLines;
var
  expectedlist : TStringList;
begin
  expectedlist := TStringList.Create;
  expectedlist.Add('client management system');

  AssertEquals('With exprlist and stringlist as input', expectedlist.Text, removeFromListByContainingRegex(exprlist, inputlist).Text);
  expectedlist.Free;
end;

procedure OsRegexpTest.When_stringReplaceRegex_Then_ReplaceMatchInString;
begin
  AssertEquals('With a string, regex and replace string as input', 'xyz abc gmbh', stringReplaceRegex('uib gmbh','.*uib', 'xyz abc'));
end;

procedure OsRegexpTest.When_stringReplaceRegexInList_Then_ReplaceMatchInStringlist;
var
  expectedlist : TStringList;
begin
  expectedlist := TStringList.Create;
  expectedlist.Add('uib gmbh');
  expectedlist.Add('client management system');
  expectedlist.Add('Please e-mail us at MATCH REMOVED and MATCH REMOVED');
  expectedlist.Add('MATCH REMOVED');
  expectedlist.Add('opsi by uib gmbh');
  expectedlist.Add('email : MATCH REMOVED');

  AssertEquals('With a stringlist, regex and replace string as input', expectedlist.Text, stringReplaceRegexInList(inputlist, expr, 'MATCH REMOVED').Text);
  expectedlist.Free;
end;


initialization

  RegisterTest(OsRegexpTest);
end.

