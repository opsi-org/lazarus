unit osEvaluateBooleanFunctions;

{$mode Delphi}

interface

uses
  Classes, SysUtils,
  oslog,
  osparserhelper;

procedure parseErrorsOccurredSinceMark(
  var RemainingInCurrentParsingLine: string; var InfoSyntaxError: string;
  var syntaxcheck: boolean; var BooleanResult: boolean);

implementation

procedure parseErrorsOccurredSinceMark(
  var RemainingInCurrentParsingLine: string; var InfoSyntaxError: string;
  var syntaxcheck: boolean; var BooleanResult: boolean);
var
  ErrorCompareNumber, relationSymbol: string;
  errNumber: integer;
begin
  getword(RemainingInCurrentParsingLine, relationSymbol,
    RemainingInCurrentParsingLine, WordDelimiterWhiteSpace);
  try
    getword(RemainingInCurrentParsingLine, ErrorCompareNumber,
      RemainingInCurrentParsingLine, [' ', #9, ')']);
    errNumber := StrToInt(ErrorCompareNumber);
    syntaxcheck := True;
    ErrorCompareNumber := '';
  except
    InfoSyntaxError := ErrorCompareNumber + ' is not a number'
  end;

  syntaxCheck :=
    syntaxcheck and hasRelation(relationSymbol,
    getCompareSign(Logdatei.NumberOfErrors - Logdatei.ErrorNumberMarked, errNumber),
    BooleanResult, InfoSyntaxError);
  LogDatei.log('(TotalErrors: ' + IntToStr(Logdatei.NumberOfErrors) +
    ' - ErrorMark: ' + IntToStr(Logdatei.ErrorNumberMarked) + ') ' +
    relationSymbol + ' ' + IntToStr(errNumber) + ' -> ' +
    BoolToStr(BooleanResult), LLDebug2);
end;

end.
