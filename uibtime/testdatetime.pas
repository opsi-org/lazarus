unit testdatetime;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,uibdatetime,dateutils;

type

  TTestCaseGetlastinterval= class(TTestCase)
  private
    procedure basetest( interval : integer; startstr, stopstr, expectstr : string);
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
  end;

implementation

var
  result : TDateTime;

procedure TTestCaseGetlastinterval.basetest( interval : integer; startstr, stopstr, expectstr : string);
var
  startdt, stopdt, expectdt, result : TdateTime;
begin
  startdt := StrToDate(startstr);
  stopdt := StrToDate(stopstr);
  expectdt := StrToDate(expectstr);
  result := getLastIntervalStart(startdt,stopdt,interval);
  CheckEquals(result, expectdt,'interval: '+inttostr(interval)+ 'start: '+startstr+' stop: '+stopstr+' we got: '+DateToStr(result)+' we expected: '+expectstr);
  //AssertEquals(result, StrToDate('1.8.2018'));
end;


procedure TTestCaseGetlastinterval.Test1Month_1;
begin
  basetest( 1, '1.8.2018','3.9.2018', '1.9.2018');
end;

procedure TTestCaseGetlastinterval.Test1Month_2;
begin
  basetest( 1, '1.8.2017','3.9.2018', '1.9.2018');
end;

procedure TTestCaseGetlastinterval.Test1Month_3;
begin
  basetest( 1, '1.8.2017','1.9.2018', '1.9.2018');
end;

procedure TTestCaseGetlastinterval.Test1Month_4;
begin
  basetest( 1, '1.8.2017','2.9.2018', '1.9.2018');
end;

procedure TTestCaseGetlastinterval.Test1Month_5;
begin
  basetest( 1, '1.8.2017','31.8.2018', '1.8.2018');
end;

procedure TTestCaseGetlastinterval.Test1Month_6;
begin
  basetest( 1, '1.8.2017','30.8.2018', '1.8.2018');
end;

procedure TTestCaseGetlastinterval.Test1Month_7;
begin
  basetest( 1, '1.2.2017','28.2.2018', '1.2.2018');
end;

procedure TTestCaseGetlastinterval.Test1Month_8;
begin
  basetest( 1, '1.2.2017','1.3.2018', '1.3.2018');
end;

procedure TTestCaseGetlastinterval.Test1Month_9;
begin
  basetest( 1, '1.2.2017','2.3.2018', '1.3.2018');
end;

////////////////////// 3 Month //////////////////////////////

procedure TTestCaseGetlastinterval.Test3Month_1;
begin
  basetest( 3, '1.6.2018','3.9.2018', '1.9.2018');
end;

procedure TTestCaseGetlastinterval.Test3Month_2;
begin
  basetest( 3, '1.6.2017','3.9.2018', '1.9.2018');
end;

procedure TTestCaseGetlastinterval.Test3Month_3;
begin
  basetest( 3, '1.6.2017','1.9.2018', '1.9.2018');
end;

procedure TTestCaseGetlastinterval.Test3Month_4;
begin
  basetest( 3, '1.6.2017','2.9.2018', '1.9.2018');
end;

procedure TTestCaseGetlastinterval.Test3Month_5;
begin
  basetest( 3, '1.6.2017','31.8.2018', '1.6.2018');
end;

procedure TTestCaseGetlastinterval.Test3Month_6;
begin
  basetest( 3, '1.6.2017','30.8.2018', '1.6.2018');
end;

procedure TTestCaseGetlastinterval.Test3Month_7;
begin
  basetest( 3, '1.12.2017','28.2.2018', '1.12.2017');
end;

procedure TTestCaseGetlastinterval.Test3Month_8;
begin
  basetest( 3, '1.12.2017','1.3.2018', '1.3.2018');
end;

procedure TTestCaseGetlastinterval.Test3Month_9;
begin
  basetest( 3, '1.12.2017','2.3.2018', '1.3.2018');
end;


procedure TTestCaseGetlastinterval.SetUp;
begin
  // nothing
end;

procedure TTestCaseGetlastinterval.TearDown;
begin
  // nothing
end;

initialization

  RegisterTest(TTestCaseGetlastinterval);
end.

