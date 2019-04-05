unit urlparserbuildertest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, osurlparser;

type
  UrlParserTest= class(TTestCase)
  private
    urlcomponents, urlcomponents1, urlcomponents2, urlcomponents3 : TStringList;
    url, url1 : string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure When_parseUrl_withAllFields_Then_SplitUrl;
    procedure When_parseUrl_Then_SplitUrl;
    procedure When_createUrl_withAllFields_Then_BuiltUrl;
    procedure When_createUrl_Then_BuiltUrl;
    procedure When_createUrlwithNonIntegerPort_Then_BuiltUrl;
  end;

implementation

procedure UrlParserTest.SetUp;
begin
  url := 'proto://user:pwd@host:8080/path/document?params#bookmark';
  url1 := 'ftp://example.mit.edu/';

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

  urlcomponents1 := TStringList.Create;
  urlcomponents1.Add('Protocol=ftp');
  urlcomponents1.Add('Username=');
  urlcomponents1.Add('Password=');
  urlcomponents1.Add('Host=example.mit.edu');
  urlcomponents1.Add('Port=0');
  urlcomponents1.Add('Path=/');
  urlcomponents1.Add('Document=');
  urlcomponents1.Add('Params=');
  urlcomponents1.Add('Bookmark=');

  urlcomponents2 := TStringList.Create;
  urlcomponents2.Add('Protocol=ftp');
  urlcomponents2.Add('Host=example.mit.edu');
  urlcomponents2.Add('Path=/');

  urlcomponents3 := TStringList.Create;
  urlcomponents3.Add('Protocol=ftp');
  urlcomponents3.Add('Username=');
  urlcomponents3.Add('Password=');
  urlcomponents3.Add('Host=example.mit.edu');
  urlcomponents3.Add('Port=ABD');
  urlcomponents3.Add('Path=/');
  urlcomponents3.Add('Document=');
  urlcomponents3.Add('Params=');
  urlcomponents3.Add('Bookmark=');

end;

procedure UrlParserTest.TearDown;
begin
  urlcomponents.Free;
  urlcomponents1.Free;
  urlcomponents2.Free;
  urlcomponents3.Free;
end;

procedure UrlParserTest.When_parseUrl_withAllFields_Then_SplitUrl;
begin
  AssertEquals('With a complete url string as input', urlcomponents.Text, parseUrl(url).Text);
end;

procedure UrlParserTest.When_parseUrl_Then_SplitUrl;
begin
  AssertEquals('Url string with some fields as input', urlcomponents1.Text, parseUrl(url1).Text);
end;

procedure UrlParserTest.When_createUrl_withAllFields_Then_BuiltUrl;
begin
  AssertEquals('With complete url components as input', url, createUrl(urlcomponents));
end;

procedure UrlParserTest.When_createUrl_Then_BuiltUrl;
begin
  AssertEquals('With some url components as input', url1, createUrl(urlcomponents2));
end;

procedure UrlParserTest.When_createUrlwithNonIntegerPort_Then_BuiltUrl;
begin
  AssertEquals('INPUT: Url components with non integer port field. EXPECTED: Non integer Port should consider as zero.', url1, createUrl(urlcomponents3));
end;


initialization
  RegisterTest(UrlParserTest);
end.

