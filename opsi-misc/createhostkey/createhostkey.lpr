program createhostkey;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  sysutils
  { you can add units after this };

{$R *.res}

var
  mykey : string;

begin
  Randomize;
  mykey := IntToHex(random(MAXINT),8);
  mykey := mykey+ IntToHex(random(MAXINT),8);
  mykey := mykey+ IntToHex(random(MAXINT),8);
  mykey := mykey+ IntToHex(random(MAXINT),8);
  writeln(lowercase(mykey));
end.

