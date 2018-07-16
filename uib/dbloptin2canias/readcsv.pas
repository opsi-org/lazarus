unit readcsv;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  datam;

procedure readcsv_file;


implementation

procedure readcsv_file;
var
  infilename : string;
begin
   inlist := TStringlist.Create;
   infilename := apppath + PathDelim + 'newsletter.csv';
   inlist.LoadFromFile(infilename);
   inlist.Delimiter:=';';
end;

end.

