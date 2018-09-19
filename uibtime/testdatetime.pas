unit testdatetime;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,uibdatetime,dateutils;

type

  TTestCaseGetlastinterval= class(TTestCase)
  private
    procedure basetest_incl( interval : integer; startstr, stopstr, expectstr : string);
    procedure basetest_excl( interval : integer; startstr, stopstr, expectstr : string);
    procedure basetest_both( interval : integer; startstr, stopstr, expectstr : string; endexclude : boolean);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test1Month_1;
    procedure Test1Month_2;
    procedure Test1Month_3;
    procedure Test1Month_4;
    procedure Test1Month_5;
    procedure Test1Month_6;
    procedure Test1Month_7;
    procedure Test1Month_8;
    procedure Test1Month_9;

    procedure Test3Month_1;
    procedure Test3Month_2;
    procedure Test3Month_3;
    procedure Test3Month_4;
    procedure Test3Month_5;
    procedure Test3Month_6;
    procedure Test3Month_7;
    procedure Test3Month_8;
    procedure Test3Month_9;
    procedure Test12Month_1;
    procedure Test12Month_2;
    procedure Test12Month_3;
    procedure Test12Month_4;
    procedure Test12Month_5;
    procedure Test12Month_6;
    procedure Test12Month_7;
    procedure Test12Month_8;
    procedure Test12Month_9;
    procedure Test12Month_10;

    procedure Testincl1Month_1;
    procedure Testincl1Month_2;
    procedure Testincl1Month_3;
    procedure Testincl1Month_4;
    procedure Testincl1Month_5;
    procedure Testincl1Month_6;
    procedure Testincl1Month_7;
    procedure Testincl1Month_8;
    procedure Testincl1Month_9;

    procedure Testincl3Month_1;
    procedure Testincl3Month_2;
    procedure Testincl3Month_3;
    procedure Testincl3Month_4;
    procedure Testincl3Month_5;
    procedure Testincl3Month_6;
    procedure Testincl3Month_7;
    procedure Testincl3Month_8;
    procedure Testincl3Month_9;
    procedure Testincl12Month_1;
    procedure Testincl12Month_2;
    procedure Testincl12Month_3;
    procedure Testincl12Month_4;
    procedure Testincl12Month_5;
    procedure Testincl12Month_6;
    procedure Testincl12Month_7;
    procedure Testincl12Month_8;
    procedure Testincl12Month_9;
    procedure Testincl12Month_10;
  end;

implementation

var
  result : TDateTime;

procedure TTestCaseGetlastinterval.basetest_incl( interval : integer; startstr, stopstr, expectstr : string);
var
  startdt, stopdt, expectdt, result : TdateTime;
begin
  basetest_both( interval, startstr, stopstr, expectstr,false);
end;

procedure TTestCaseGetlastinterval.basetest_excl( interval : integer; startstr, stopstr, expectstr : string);
var
  startdt, stopdt, expectdt, result : TdateTime;
begin
  basetest_both( interval, startstr, stopstr, expectstr,true);
end;


procedure TTestCaseGetlastinterval.basetest_both( interval : integer; startstr, stopstr, expectstr : string; endexclude : boolean);
var
  startdt, stopdt, expectdt, result : TdateTime;
begin
   DefaultFormatSettings.ShortDateFormat := 'DD.MM.YYYY';
  startdt := scanDateTime('DD.MM.YYYY',startstr);
  stopdt := scanDateTime('DD.MM.YYYY',stopstr);
  expectdt := scanDateTime('DD.MM.YYYY',expectstr);
  (*startdt := StrToDate(startstr,'DD.MM.YYYY');
  stopdt := StrToDate(stopstr,DefaultFormatSettings);
  expectdt := StrToDate(expectstr,DefaultFormatSettings); *)
  result := getLastIntervalStart(startdt,stopdt,interval,endexclude);
  CheckEquals(result, expectdt,'end exclude: '+BoolToStr(endexclude,true)+'; interval: '+inttostr(interval)+ ' start: '+startstr+' stop: '+stopstr+' we got: '+DateToStr(result)+' we expected: '+expectstr);
  //AssertEquals(result, StrToDate('1.8.2018'));
end;

////////////////////// exclude tests //////////////////////////////
////////////////////// 1 Month //////////////////////////////

procedure TTestCaseGetlastinterval.Test1Month_1;
begin
  basetest_excl( 1, '1.8.2018','03.09.2018', '1.9.2018');
end;

procedure TTestCaseGetlastinterval.Test1Month_2;
begin
  basetest_excl( 1, '1.8.2017','3.9.2018', '1.9.2018');
end;

procedure TTestCaseGetlastinterval.Test1Month_3;
begin
  basetest_excl( 1, '1.8.2017','1.9.2018', '1.8.2018');
end;

procedure TTestCaseGetlastinterval.Test1Month_4;
begin
  basetest_excl( 1, '1.8.2017','2.9.2018', '1.9.2018');
end;

procedure TTestCaseGetlastinterval.Test1Month_5;
begin
  basetest_excl( 1, '1.8.2017','31.8.2018', '1.8.2018');
end;

procedure TTestCaseGetlastinterval.Test1Month_6;
begin
  basetest_excl( 1, '1.8.2017','30.8.2018', '1.8.2018');
end;

procedure TTestCaseGetlastinterval.Test1Month_7;
begin
  basetest_excl( 1, '1.2.2017','28.2.2018', '1.2.2018');
end;

procedure TTestCaseGetlastinterval.Test1Month_8;
begin
  basetest_excl( 1, '1.2.2017','1.3.2018', '1.2.2018');
end;

procedure TTestCaseGetlastinterval.Test1Month_9;
begin
  basetest_excl( 1, '1.2.2017','2.3.2018', '1.3.2018');
end;

////////////////////// 3 Month //////////////////////////////

procedure TTestCaseGetlastinterval.Test3Month_1;
begin
  basetest_excl( 3, '1.6.2018','3.9.2018', '1.9.2018');
end;

procedure TTestCaseGetlastinterval.Test3Month_2;
begin
  basetest_excl( 3, '1.6.2017','3.9.2018', '1.9.2018');
end;

procedure TTestCaseGetlastinterval.Test3Month_3;
begin
  basetest_excl( 3, '1.6.2017','1.9.2018', '1.6.2018');
end;

procedure TTestCaseGetlastinterval.Test3Month_4;
begin
  basetest_excl( 3, '1.6.2017','2.9.2018', '1.9.2018');
end;

procedure TTestCaseGetlastinterval.Test3Month_5;
begin
  basetest_excl( 3, '1.6.2017','31.8.2018', '1.6.2018');
end;

procedure TTestCaseGetlastinterval.Test3Month_6;
begin
  basetest_excl( 3, '1.6.2017','30.8.2018', '1.6.2018');
end;

procedure TTestCaseGetlastinterval.Test3Month_7;
begin
  basetest_excl( 3, '1.12.2017','28.2.2018', '1.12.2017');
end;

procedure TTestCaseGetlastinterval.Test3Month_8;
begin
  basetest_excl( 3, '1.12.2017','1.3.2018', '1.12.2017');
end;

procedure TTestCaseGetlastinterval.Test3Month_9;
begin
  basetest_excl( 3, '1.12.2017','2.3.2018', '1.3.2018');
end;

////////////////////// 12 Month //////////////////////////////

procedure TTestCaseGetlastinterval.Test12Month_1;
begin
  basetest_excl( 12, '1.1.2017','31.12.2017', '1.1.2017');
end;

procedure TTestCaseGetlastinterval.Test12Month_2;
begin
  basetest_excl( 12, '1.1.2017','1.1.2018', '1.1.2017');
end;

procedure TTestCaseGetlastinterval.Test12Month_3;
begin
  basetest_excl( 12, '1.1.2017','2.1.2018', '1.1.2018');
end;

procedure TTestCaseGetlastinterval.Test12Month_4;
begin
  basetest_excl( 12, '1.3.2017','28.2.2018', '1.3.2017');
end;

procedure TTestCaseGetlastinterval.Test12Month_5;
begin
  basetest_excl( 12, '1.3.2017','1.3.2018', '1.3.2017');
end;

procedure TTestCaseGetlastinterval.Test12Month_6;
begin
  basetest_excl( 12, '1.3.2015','28.2.2018', '1.3.2017');
end;

procedure TTestCaseGetlastinterval.Test12Month_7;
begin
  basetest_excl( 12, '1.3.2015','1.3.2018', '1.3.2017');
end;

procedure TTestCaseGetlastinterval.Test12Month_8;
begin
  basetest_excl( 12, '1.9.2013','30.8.2018', '1.9.2017');
end;

procedure TTestCaseGetlastinterval.Test12Month_9;
begin
  basetest_excl( 12, '1.9.2013','1.9.2018', '1.9.2017');
end;

procedure TTestCaseGetlastinterval.Test12Month_10;
begin
  basetest_excl( 12, '1.9.2013','2.9.2018', '1.9.2018');
end;


////////////////////// inclued tests //////////////////////////////
////////////////////// 1 Month //////////////////////////////

procedure TTestCaseGetlastinterval.Testincl1Month_1;
begin
  basetest_incl( 1, '1.8.2018','03.09.2018', '1.9.2018');
end;

procedure TTestCaseGetlastinterval.Testincl1Month_2;
begin
  basetest_incl( 1, '1.8.2017','3.9.2018', '1.9.2018');
end;

procedure TTestCaseGetlastinterval.Testincl1Month_3;
begin
  basetest_incl( 1, '1.8.2017','1.9.2018', '1.9.2018');
end;

procedure TTestCaseGetlastinterval.Testincl1Month_4;
begin
  basetest_incl( 1, '1.8.2017','2.9.2018', '1.9.2018');
end;

procedure TTestCaseGetlastinterval.Testincl1Month_5;
begin
  basetest_incl( 1, '1.8.2017','31.8.2018', '1.8.2018');
end;

procedure TTestCaseGetlastinterval.Testincl1Month_6;
begin
  basetest_incl( 1, '1.8.2017','30.8.2018', '1.8.2018');
end;

procedure TTestCaseGetlastinterval.Testincl1Month_7;
begin
  basetest_incl( 1, '1.2.2017','28.2.2018', '1.2.2018');
end;

procedure TTestCaseGetlastinterval.Testincl1Month_8;
begin
  basetest_incl( 1, '1.2.2017','1.3.2018', '1.3.2018');
end;

procedure TTestCaseGetlastinterval.Testincl1Month_9;
begin
  basetest_incl( 1, '1.2.2017','2.3.2018', '1.3.2018');
end;

////////////////////// 3 Month //////////////////////////////

procedure TTestCaseGetlastinterval.Testincl3Month_1;
begin
  basetest_incl( 3, '1.6.2018','3.9.2018', '1.9.2018');
end;

procedure TTestCaseGetlastinterval.Testincl3Month_2;
begin
  basetest_incl( 3, '1.6.2017','3.9.2018', '1.9.2018');
end;

procedure TTestCaseGetlastinterval.Testincl3Month_3;
begin
  basetest_incl( 3, '1.6.2017','1.9.2018', '1.9.2018');
end;

procedure TTestCaseGetlastinterval.Testincl3Month_4;
begin
  basetest_incl( 3, '1.6.2017','2.9.2018', '1.9.2018');
end;

procedure TTestCaseGetlastinterval.Testincl3Month_5;
begin
  basetest_incl( 3, '1.6.2017','31.8.2018', '1.6.2018');
end;

procedure TTestCaseGetlastinterval.Testincl3Month_6;
begin
  basetest_incl( 3, '1.6.2017','30.8.2018', '1.6.2018');
end;

procedure TTestCaseGetlastinterval.Testincl3Month_7;
begin
  basetest_incl( 3, '1.12.2017','28.2.2018', '1.12.2017');
end;

procedure TTestCaseGetlastinterval.Testincl3Month_8;
begin
  basetest_incl( 3, '1.12.2017','1.3.2018', '1.3.2018');
end;

procedure TTestCaseGetlastinterval.Testincl3Month_9;
begin
  basetest_incl( 3, '1.12.2017','2.3.2018', '1.3.2018');
end;

////////////////////// 12 Month //////////////////////////////

procedure TTestCaseGetlastinterval.Testincl12Month_1;
begin
  basetest_incl( 12, '1.1.2017','31.12.2017', '1.1.2017');
end;

procedure TTestCaseGetlastinterval.Testincl12Month_2;
begin
  basetest_incl( 12, '1.1.2017','1.1.2018', '1.1.2018');
end;

procedure TTestCaseGetlastinterval.Testincl12Month_3;
begin
  basetest_incl( 12, '1.1.2017','2.1.2018', '1.1.2018');
end;

procedure TTestCaseGetlastinterval.Testincl12Month_4;
begin
  basetest_incl( 12, '1.3.2017','28.2.2018', '1.3.2017');
end;

procedure TTestCaseGetlastinterval.Testincl12Month_5;
begin
  basetest_incl( 12, '1.3.2017','1.3.2018', '1.3.2018');
end;

procedure TTestCaseGetlastinterval.Testincl12Month_6;
begin
  basetest_incl( 12, '1.3.2015','28.2.2018', '1.3.2017');
end;

procedure TTestCaseGetlastinterval.Testincl12Month_7;
begin
  basetest_incl( 12, '1.3.2015','1.3.2018', '1.3.2018');
end;

procedure TTestCaseGetlastinterval.Testincl12Month_8;
begin
  basetest_incl( 12, '1.9.2013','30.8.2018', '1.9.2017');
end;

procedure TTestCaseGetlastinterval.Testincl12Month_9;
begin
  basetest_incl( 12, '1.9.2013','1.9.2018', '1.9.2018');
end;

procedure TTestCaseGetlastinterval.Testincl12Month_10;
begin
  basetest_incl( 12, '1.9.2013','2.9.2018', '1.9.2018');
end;


procedure TTestCaseGetlastinterval.SetUp;
begin
  // nothing
   DefaultFormatSettings.ShortDateFormat := 'd.m.yyyy';
end;

procedure TTestCaseGetlastinterval.TearDown;
begin
  // nothing
end;

initialization

  RegisterTest(TTestCaseGetlastinterval);
end.

