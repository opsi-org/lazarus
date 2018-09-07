unit uibdatetime;

{$mode delphi}

// http://www.efg2.com/Lab/Library/Delphi/DatesAndTimes/
// http://wiki.delphi-jedi.org/wiki/Category:JclDateTime.pas_Functions
// https://www.freepascal.org/docs-html/rtl/sysutils/datetimeroutines.html
// https://www.freepascal.org/docs-html/rtl/dateutils/index-5.html

interface

uses
  Classes, SysUtils,dateutils;

function getLastIntervalStart(startdate : TdateTime;  enddate : TdateTime; myMonthInterval : cardinal; endexcl : boolean) : TdateTime;
function getLastIntervalEnd(startdate : TdateTime; enddate : TdateTime; myMonthInterval : cardinal; endexcl : boolean) : TdateTime;
function timeFloatTohourminutesStr(mytime : double) : string;
function dayOfWeekGerStrToLazDayofWeekbyte(daystr : string) : byte;
function lazDayofWeekbyteToDayOfWeekGerStr(daybyte : word) : string;

implementation

function getLastIntervalStart(startdate : TdateTime; enddate : TdateTime; myMonthInterval : cardinal; endexcl : boolean) : TdateTime;
var
  mymonthspan : double;
  mynow : double;
  mydayofmonth1, mydayofmonth2 : word;
  mymonth1, mymonth2 : word;
  myyear1, myyear2 : word;
  mymonthbetween : integer;
  numberofintervals : integer;
  modofintervals : integer;
  mycalcmonthgap : integer;
begin
  result := 0;
  // if endexcl eg. it is 1.9. so the real (incl) end is 31.8.
  // we nee here (if (mydayofmonth1 > mydayofmonth2)) the incl. end day
  if endexcl then IncDay(enddate,-1);
  mydayofmonth1 := dayof(startdate);
  mydayofmonth2 := dayof(enddate);
  mymonth1 := MonthOf(startdate);
  mymonth2 := MonthOf(enddate);
  myyear1 := YearOf(startdate);
  myyear2 := YearOf(enddate);
  // warning for monthspan and monthsbetween:
  // This number is an approximation,
  // based on an average number of days of 30.4375 per month (average over 4 years).
  // https://www.freepascal.org/docs-html/rtl/dateutils/monthspan.html
  // so we use it here only for comparsion while debugging
  mymonthspan := monthspan(startdate,enddate);
  mymonthbetween := monthsbetween(startdate,enddate);
  // here we start the used calculation
  mycalcmonthgap := 0;
  if myyear1 = myyear2 then
  begin  // same year
    mycalcmonthgap := mymonth2 - mymonth1;
  end
  else
  begin // not same year
    if myyear1 +1 = myyear2 then
    begin  // next year
      // missing month in the old year
      mycalcmonthgap := 12 - mymonth1;
      // add month in the new year
      mycalcmonthgap := mycalcmonthgap + mymonth2;
    end
    else
    begin // not same year
      // get the full years
      mycalcmonthgap := (myyear2 - myyear1 -1) *12;
      // missing month in the old year
      mycalcmonthgap := mycalcmonthgap + (12 - mymonth1);
      // add month in the new year
      mycalcmonthgap := mycalcmonthgap + mymonth2;
    end;
  end;

  if myMonthInterval = 1 then
  begin
    if (mydayofmonth1 > mydayofmonth2) then
    begin
      dec(mycalcmonthgap);
    end;
  end
  else
  begin
    // myMonthInterval > 1
  end;

  if (myMonthInterval = 0) or (mycalcmonthgap < myMonthInterval) then
  begin
    //no interval finished
    result := startdate;
  end
  else
  begin
    numberofintervals := mycalcmonthgap div myMonthInterval;
    modofintervals := mycalcmonthgap mod myMonthInterval;
    // if numberofintervals = 1 and modofintervals=0 then we have finished the first interval
    // the interval start is still:  startdate
    //if (myMonthInterval > 1) and (modofintervals = 0) then dec(numberofintervals);
    result := IncMonth(startdate,(numberofintervals * myMonthInterval));
  end;
end;

(*
function getLastIntervalStart(startdate : TdateTime; enddate : TdateTime; myMonthInterval : cardinal) : TdateTime;
var
  mymonthspan : double;
  mynow : double;
  mydayofmonth1, mydayofmonth2 : word;
  mymonth1, mymonth2 : word;
  myyear1, myyear2 : word;
  mymonthbetween : integer;
  numberofintervals : integer;
  modofintervals : integer;
  mycalcmonthgap : integer;
begin
  // warning for monthspan and monthsbetween:
  // This number is an approximation,
  // based on an average number of days of 30.4375 per month (average over 4 years).
  // https://www.freepascal.org/docs-html/rtl/dateutils/monthspan.html
  result := 0;
  mynow := enddate;
  mydayofmonth1 := dayof(startdate);
  mydayofmonth2 := dayof(enddate);
  mymonth1 := MonthOf(startdate);
  mymonth2 := MonthOf(enddate);
  myyear1 := YearOf(startdate);
  myyear2 := YearOf(enddate);
  if myyear2 > myyear1 then mycalcmonthgap := (myyear2 - myyear1) * 12;
  if mymonth2 > mymonth1 then mycalcmonthgap := mycalcmonthgap + (mymonth2 - mymonth1)
  else mycalcmonthgap := mycalcmonthgap + mymonth2;
  mymonthspan := monthspan(startdate,mynow);
  mymonthbetween := monthsbetween(startdate,mynow);
  mymonthbetween := round(mymonthspan);
  If (mydayofmonth1 < mydayofmonth2) and
     ((mymonth1 = mymonth2) and (myMonthInterval = 1)) then
        mymonthbetween := trunc(mymonthspan);
  If (mydayofmonth1 < mydayofmonth2) and
     ((mycalcmonthgap mod myMonthInterval) = 0)  then
        mymonthbetween := trunc(mymonthspan);
  if (myMonthInterval = 0) or (mymonthspan < myMonthInterval) then
  begin
    //no interval finished
    result := startdate;
  end
  else
  begin
    numberofintervals := mymonthbetween div myMonthInterval;
    modofintervals := mymonthbetween mod myMonthInterval;
    // if numberofintervals = 1 and modofintervals=0 then we have finished the first interval
    // the interval start is still:  startdate
    if (myMonthInterval > 1) and (modofintervals = 0) then dec(numberofintervals);
    result := IncMonth(startdate,(numberofintervals * myMonthInterval));
  end;
end;

*)

function getLastIntervalEnd(startdate : TdateTime; enddate : TdateTime; myMonthInterval : cardinal; endexcl : boolean) : TdateTime;
var
  lastIntervalStart : Tdatetime;
begin
  result := 0;
  lastIntervalStart := getLastIntervalStart(startdate,enddate,myMonthInterval,endexcl);
  result := IncMonth(lastIntervalStart, myMonthInterval);
  if not endexcl then result := IncDay(result,-1); // make intervalend incl: eg. it is not 1.9. but 31.8. : incday -1
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

