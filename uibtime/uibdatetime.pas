unit uibdatetime;

{$mode delphi}

// http://www.efg2.com/Lab/Library/Delphi/DatesAndTimes/
// http://wiki.delphi-jedi.org/wiki/Category:JclDateTime.pas_Functions
// https://www.freepascal.org/docs-html/rtl/sysutils/datetimeroutines.html
// https://www.freepascal.org/docs-html/rtl/dateutils/index-5.html

interface

uses
  Classes, SysUtils,dateutils;

function getLastIntervalStart(startdate : TdateTime; myMonthInterval : cardinal) : TdateTime;
function getLastIntervalEnd(startdate : TdateTime; myMonthInterval : cardinal) : TdateTime;
function timeFloatTohourminutesStr(mytime : double) : string;

implementation

function getLastIntervalStart(startdate : TdateTime; myMonthInterval : cardinal) : TdateTime;
var
  mymonthspan : double;
  mynow : double;
  mydayofmonth1, mydayofmonth2 : word;
  mymonthbetween : integer;
  numberofintervals : integer;
begin
  // warning for monthspan and monthsbetween:
  // This number is an approximation,
  // based on an average number of days of 30.4375 per month (average over 4 years).
  // https://www.freepascal.org/docs-html/rtl/dateutils/monthspan.html
  result := 0;
  mynow := today;
  mydayofmonth1 := dayof(mynow);
  mydayofmonth2 := dayof(startdate);
  If mydayofmonth1 = mydayofmonth2 then
  begin
    mynow := IncDay(mynow,1);
    // we add a day because the approximation may fail on same day of month
  end;
  mymonthspan := monthspan(startdate,mynow);
  mymonthbetween := monthsbetween(startdate,mynow);
  if (myMonthInterval = 0) or (mymonthspan < myMonthInterval) then
  begin
    //no interval finished
    result := startdate;
  end
  else
  begin
    numberofintervals := mymonthbetween div myMonthInterval;
    result := IncMonth(startdate,(numberofintervals * myMonthInterval));
  end;
end;

function getLastIntervalEnd(startdate : TdateTime; myMonthInterval : cardinal) : TdateTime;
var
  lastIntervalStart : Tdatetime;
begin
  result := 0;
  lastIntervalStart := getLastIntervalStart(startdate,myMonthInterval);
  result := IncMonth(lastIntervalStart, myMonthInterval);
end;

function timeFloatTohourminutesStr(mytime : double) : string;
begin
  result := Format('%.*d', [2,trunc(mytime)])
    + ':' + Format('%.*d', [2, round(frac(mytime) * 60)]);
end;

end.

