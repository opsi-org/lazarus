unit osDoRegistryFunctions;

{$mode Delphi}

interface

uses
  Classes, SysUtils,
  osparserhelper;

function ParseRegistryListVariableCommand(var Separator: char; var RegistryVariableName: string;
  var Value: string; var Remaining: string; var ErrorInfo: string): boolean;

implementation

// Parse the registry section commands 'supp' and 'deleteListEntries'.
// Give back as parsed output: the name of the registry variable as RegistryVariableName,
// the list seperator char as Seperator, and the list to supplement or delete as Value.
function ParseRegistryListVariableCommand(var Separator: char; var RegistryVariableName: string;
  var Value: string; var Remaining: string; var ErrorInfo: string): boolean;
begin
  Result := False;
  if Skip('"', Remaining, Remaining, ErrorInfo) then
  begin
    GetWord(Remaining, RegistryVariableName, Remaining, ['"']);
    if Skip('"', Remaining, Remaining, ErrorInfo) then
    begin
      if (length(Remaining) = 0) then
        ErrorInfo := 'Separator expected'
      else
      begin
        if Remaining[1] = '"' then
          Separator := #0
        else
        begin
          Separator := Remaining[1];
          Skip(Separator, Remaining, Remaining, ErrorInfo);
        end;
        if Skip('"', Remaining, Remaining, ErrorInfo) then
        begin
          GetWord(Remaining, Value, Remaining, ['"']);
          if Skip('"', Remaining, Remaining, ErrorInfo) then
            Result := True;
        end;
      end;
    end;
  end;
end;

end.

