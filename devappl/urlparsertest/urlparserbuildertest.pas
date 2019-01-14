unit urlparserbuildertest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, osurlparser;

type
  UrlParserTest= class(TTestCase)
  private
    urlcomponents, urlcomponents1 : TStringList;
    url, url1 : string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure When_parseUrl_Then_SplitUrl;
    procedure When_createUrl_Then_BuiltUrl;
    procedure When_createUrlwithMissingField_Then_BuiltUrl;
  end;

implementation

procedure UrlParserTest.SetUp;
begin
  url := 'proto://user:pwd@host:8080/path/document?params#bookmark';

  urlcomponents := TStringList.Create;
  urlcomponents.Add('Protocol=proto');
  urlcomponents.Add('Username=user');
  urlcomponents.Add('Password=pwd');
  urlcomponents.Add('Host=host');
  urlcomponents.Add('Port=8080');
  urlcomponents.Add('Path=/path/');
  urlcomponents.Add('Document=document');
  urlcomponents.Add('Params=params');
  urlcomponents.Add('Bookmark=bookmark');

  url1 := 'https://example.com';

  urlcomponents1 := TStringList.Create;
  urlcomponents1.Add('Protocol=https');
  urlcomponents1.Add('Username=');
  urlcomponents1.Add('Password=');
  urlcomponents1.Add('Host=example.com');
  urlcomponents1.Add('Path=');
  urlcomponents1.Add('Document=');
  urlcomponents1.Add('Params=');
  urlcomponents1.Add('Bookmark=');
end;

procedure UrlParserTest.TearDown;
begin
  urlcomponents.Free;
  urlcomponents1.Free;
end;

procedure UrlParserTest.When_parseUrl_Then_SplitUrl;
begin
  AssertEquals('With a url string as input', urlcomponents.Text, parseUrl(url).Text);
end;

procedure UrlParserTest.When_createUrl_Then_BuiltUrl;
begin
  AssertEquals('With url components as input', url, createUrl(urlcomponents));
end;

procedure UrlParserTest.When_createUrlwithMissingField_Then_BuiltUrl;
begin
  AssertEquals('With url components as input', url1, createUrl(urlcomponents1));
end;


initialization
  RegisterTest(UrlParserTest);
end.

