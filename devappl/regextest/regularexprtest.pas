unit regularexprtest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, osregex;

type

  RegexTest= class(TTestCase)
  private
    inputlist : TStringList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //procedure TestHookUp;
    procedure When_isRegexMatch_Then_True;
    procedure When_isRegexMatch_Then_False;
    procedure When_editStringRegexp_String_Then_ReplaceMatch;
    procedure When_editStringRegexp_Stringlist_Then_ReplaceMatch;
  end;

implementation

{
procedure RegexTest.TestHookUp;
begin
  Fail('Write your own test');
end;
}

procedure RegexTest.SetUp;
begin
  inputlist := TStringList.Create;

  inputlist.Add('hello');
  inputlist.Add('Please e-mail us at support@mycompany.com or sales@mycompany.com');
  inputlist.Add('how');
  inputlist.Add('jeejojojojo');
  inputlist.Add('are');
  inputlist.Add('email : jee@uib.com or joo@uib.com');
  inputlist.Add('jooojeejoo iiiiii');
end;

procedure RegexTest.TearDown;
begin
  if Assigned(inputlist) then
    inputlist.Free;
end;

procedure RegexTest.When_isRegexMatch_Then_True;
begin
  AssertEquals('With a matching text and regular expression as input', True, osregex.isRegexMatch('uib gmbh','.*uib.*'));
end;

procedure RegexTest.When_isRegexMatch_Then_False;
begin
  AssertEquals('With a non matching text and regular expression as input', False, osregex.isRegexMatch('u i b gmbh','.*uib.*'));
end;

procedure RegexTest.When_editStringRegexp_String_Then_ReplaceMatch;
begin
  AssertEquals('With a string, regular expression and replace string as input', 'hello how are you gmbh', osregex.editStringRegexp('uib gmbh','.*uib', 'hello how are you'));
end;

procedure RegexTest.When_editStringRegexp_Stringlist_Then_ReplaceMatch;
var
  outputlist : TStringList;
begin
  outputlist := TStringList.Create;
  try
    outputlist.Add('hello');
    outputlist.Add('Please e-mail us at hello how are you or hello how are you');
    outputlist.Add('how');
    outputlist.Add('jeejojojojo');
    outputlist.Add('are');
    outputlist.Add('email : hello how are you or hello how are you');
    outputlist.Add('jooojeejoo iiiiii');

    AssertEquals('With a stringlist, regular expression and replace string as input', TClass(outputlist), TClass(osregex.editStringRegexp(inputlist, '\w+(\.[\w-]+)*@([\w-]+\.)+[a-zA-Z]{2,6}', 'hello how are you')));
  finally
    if Assigned(outputlist) then
      outputlist.Free;
  end;
end;

initialization

  RegisterTest(RegexTest);
end.

