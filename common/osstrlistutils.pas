unit osstrlistutils;

{$mode delphi}

interface

uses
  Classes, SysUtils, strutils;

function areStringlistsEqual(stringlist1 : TStringList; stringlist2 : TStringList; flag : string) : boolean;

implementation

const
    FLAG_AUTOMODE = $00000000;
    FLAG_AUTO_CS  = $00000010;
    FLAG_STRING_CS= $00000100;

// Function to validate if 2 stringlists are equal
function areStringlistsEqual(stringlist1 : TStringList; stringlist2 : TStringList; flag : string) : boolean;
var
  aux1, aux2 : string;
  i : integer;
begin
  result := True;
  try
    if (stringlist1.Count=0) or (stringlist2.Count=0) then
       result := False
    else
    if stringlist1.Count<>stringlist2.Count then
       result := False
    else
    begin
        IF flag.Equals('FLAG_AUTOMODE') THEN
           begin
             if stringlist1[0].Contains('=') then
             begin
              i :=0;
              repeat
                aux1:= DelSpace(LowerCase(stringlist1[i]));
                aux2:= DelSpace(LowerCase(stringlist2[i]));
                if aux1.Equals(aux2)=False then
                  result := False;
                i:=i+1
              until ((result=false) OR (i>=stringlist1.Count))
             end
             else
                begin
                  i :=0;
                  repeat
                  aux1:= LowerCase(stringlist1[i]);
                  aux2:= LowerCase(stringlist2[i]);
                  if aux1.Equals(aux2)=False then
                    result := False;
                  i:=i+1
                  until ((result=false) OR (i>=stringlist1.Count));
                end;
           end;
        IF flag.Equals('FLAG_AUTO_CS') THEN
           begin
             if stringlist1[0].Contains('=') then
             begin
              i :=0;
              repeat
                aux1:= DelSpace(stringlist1[i]);
                aux2:= DelSpace(stringlist2[i]);
                if aux1.Equals(aux2)=False then
                  result := False;
                i:=i+1
              until ((result=false) OR (i>=stringlist1.Count));
             end
             else
                begin
                  i :=0;
                  repeat
                  aux1:= stringlist1[i];
                  aux2:= stringlist2[i];
                  if aux1.Equals(aux2)=False then
                    result := False;
                  i:=i+1
                  until ((result=false) OR (i>=stringlist1.Count));
                end;
           end;
        IF flag.Equals('FLAG_STRING_CS') THEN
           begin
              i :=0;
              repeat
              aux1:= stringlist1[i];
              aux2:= stringlist2[i];
              if aux1.Equals(aux2)=False then
                result := False;
              i:=i+1
              until ((result=false) OR (i>=stringlist1.Count));
           end;
    end;
  except
    on E:Exception do
      writeln('Exception in areStringListsEqual : ', E.Message);
  end;
end;

end.

