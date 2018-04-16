unit uibdatetime;

{$mode delphi}

// http://www.efg2.com/Lab/Library/Delphi/DatesAndTimes/
// http://wiki.delphi-jedi.org/wiki/Category:JclDateTime.pas_Functions
// https://www.freepascal.org/docs-html/rtl/sysutils/datetimeroutines.html
// https://www.freepascal.org/docs-html/rtl/dateutils/index-5.html

interface

uses
  Classes, SysUtils,dateutils;

function getLastIntervalStart(startdate : TdateTime;  enddate : TdateTime; myMonthInterval : cardinal) : TdateTime;
function getLastIntervalEnd(startdate : TdateTime; enddate : TdateTime; myMonthInterval : cardinal) : TdateTime;
function timeFloatTohourminutesStr(mytime : double) : string;
function dayOfWeekGerStrToLazDayofWeekbyte(daystr : string) : byte;
function lazDayofWeekbyteToDayOfWeekGerStr(daybyte : word) : string;

implementation

function getLastIntervalStart(startdate : TdateTime; enddate : TdateTime; myMonthInterval : cardinal) : TdateTime;
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
  mynow := enddate;
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

function getLastIntervalEnd(startdate : TdateTime; enddate : TdateTime; myMonthInterval : cardinal) : TdateTime;
var
  lastIntervalStart : Tdatetime;
begin
  result := 0;
  lastIntervalStart := getLastIntervalStart(startdate,enddate,myMonthInterval);
  result := IncMonth(lastIntervalStart, myMonthInterval);
end;

function timeFloatTohourminutesStr(mytime : double) : string;
begin
  result := Format('%.*d', [2,trunc(mytime)])
    + ':' + Format('%.*d', [2, round(frac(mytime) * 60)]);
end;

function dayOfWeekGerStrToLazDayofWeekbyte(daystr : string) : byte;
begin
  result := 0;
  if lowercase(daystr) = 'mo' then result := 1;
  if lowercase(daystr) = 'di' then result := 2;
  if lowercase(daystr) = 'mi' then result := 3;
  if lowercase(daystr) = 'do' then result := 4;
  if lowercase(daystr) = 'fr' then result := 5;
  if lowercase(daystr) = 'sa' then result := 6;
  if lowercase(daystr) = 'so' then result := 7;
end;

function lazDayofWeekbyteToDayOfWeekGerStr(daybyte : word) : string;
begin
  result := '';
  case daybyte of
    1 : result := 'Mo';
    2 : result := 'Di';
    3 : result := 'Mi';
    4 : result := 'Do';
    5 : result := 'Fr';
    6 : result := 'Sa';
    7 : result := 'So';
    else
    result := '';
  end;
end;



end.

