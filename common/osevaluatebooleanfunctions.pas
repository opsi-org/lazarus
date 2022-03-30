unit osEvaluateBooleanFunctions;

{$mode Delphi}

interface

uses
  Classes, SysUtils,
  oslog,
  osparserhelper;

procedure parseErrorsOccurredSinceMark(var RemainingInCurrentParsingLine: string;
  var InfoSyntaxError: string; var syntaxcheck: boolean; var BooleanResult: boolean);

implementation

procedure parseErrorsOccurredSinceMark(var RemainingInCurrentParsingLine: string;
  var InfoSyntaxError: string; var syntaxcheck: boolean; var BooleanResult: boolean);
var
  ErrorCompareNumberStr, relationSymbol: string;
  ErrorCompareNumber: integer;
begin
  getword(RemainingInCurrentParsingLine, relationSymbol,
    RemainingInCurrentParsingLine, WordDelimiterWhiteSpace);
  try
    getword(RemainingInCurrentParsingLine, ErrorCompareNumberStr,
      RemainingInCurrentParsingLine, [' ', #9, ')']);
    ErrorCompareNumber := StrToInt(ErrorCompareNumberStr);
    syntaxcheck := True;
  except
    InfoSyntaxError := ErrorCompareNumberStr + ' is not a number';
    syntaxcheck := False;
  end;

  syntaxCheck :=
    syntaxcheck and hasRelation(relationSymbol,
    getCompareSign(Logdatei.NumberOfErrors - Logdatei.ErrorNumberMarked,
    ErrorCompareNumber), BooleanResult, InfoSyntaxError);
  LogDatei.log('(TotalErrors: ' + IntToStr(Logdatei.NumberOfErrors) +
    ' - ErrorMark: ' + IntToStr(Logdatei.ErrorNumberMarked) + ') ' +
    relationSymbol + ' ' + ErrorCompareNumberStr + ' -> ' +
    BoolToStr(BooleanResult), LLDebug2);
end;

end.
