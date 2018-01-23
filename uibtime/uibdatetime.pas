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


implementation

function getLastIntervalStart(startdate : TdateTime; myMonthInterval : cardinal) : TdateTime;
var
  mymonthspan : double;
  mymonthbetween : integer;
  numberofintervals : integer;
begin
  result := 0;
  mymonthspan := monthspan(startdate,now);
  mymonthbetween := monthsbetween(startdate,now);
  if mymonthspan < myMonthInterval then
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

end.

