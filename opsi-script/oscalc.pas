// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: Rupert Roeder, detlef oertel

unit oscalc;

{$mode delphi}

interface

uses
  Classes, SysUtils, oslog;
Type EDivException = Class(Exception);

function opsicalc(expr: string; var strresult: string): boolean;

implementation


function Calculate(SMyExpression: string; digits: byte): string;
  // Calculate a simple expression
  // Supported are:  Real Numbers, parenthesis
  // http://www.swissdelphicenter.ch/torry/showcode.php?id=470
  // Author: Thomas Stutz
var
  z: char;
  ipos: integer;

  function StrToReal(chaine: string): real;
  var
    r: real;
    Pos: integer;
  begin
    Val(chaine, r, Pos);
    if Pos > 0 then
      Val(Copy(chaine, 1, Pos - 1), r, Pos);
    Result := r;
  end;

  function RealToStr(inreal: extended; digits: byte): string;
  var
    S: string;
  begin
    Str(inreal: 0: digits, S);
    realToStr := S;
  end;

  procedure NextChar;
  var
    s: string;
  begin
    if ipos > Length(SMyExpression) then
    begin
      z := #9;
      Exit;
    end
    else
    begin
      s := Copy(SMyExpression, ipos, 1);
      z := s[1];
      Inc(ipos);
    end;
    if z = ' ' then
      nextchar;
    if not (z in ['0'..'9', '.', '+', '-', '/', '*', '(', ')']) then
      raise Exception.Create('Invalid char: ' + z);
    ;
  end;

  function Expression: real;
  var
    w: real;

    function Factor: real;
    var
      ws: string;
    begin
      Nextchar;
      if z in ['0'..'9'] then
      begin
        ws := '';
        repeat
          ws := ws + z;
          nextchar
        until not (z in ['0'..'9', '.']);
        Factor := StrToReal(ws);
      end
      else if z = '(' then
      begin
        Factor := Expression;
        nextchar;
      end
      else if z = '+' then
        Factor := +Factor
      else if Z = '-' then
        Factor := -Factor;
    end;

    function Term: real;
    var
      W: real;
    begin
      try
        W := Factor;
        while Z in ['*', '/'] do
          if z = '*' then
            w := w * Factor
          else
          begin
          if Factor = 0 then
             Raise EDivException.Create ('Division by Zero would occur')
          else
            w := w / Factor;
          end;
        Term := w;
      except
        on E: Exception do
        begin
          //Result := False;
          //strresult := E.Message;
          LogDatei.log('Error in opsicalc: ' + E.Message, LLError);
        end;
      end;
    end;

  begin
    w := term;
    while z in ['+', '-'] do
      if z = '+' then
        w := w + term
      else
        w := w - term;
    Expression := w;
  end;

begin
  ipos := 1;
  Result := RealToStr(Expression, digits);
end;

function opsicalc(expr: string; var strresult: string): boolean;
begin
  Result := False;
  try
    strresult := Calculate(expr, 0);
    if strresult = '+Inf' then
    begin
      strresult := '';
      LogDatei.DependentAdd('Error in calculate (division by zero): ' +
        expr + ' to :' + strresult, LLDebug2);
      Result := False;
    end
    else
    begin
      LogDatei.DependentAdd('calculate: ' + expr + ' to :' + strresult, LLDebug2);
      Result := True;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      strresult := E.Message;
      LogDatei.DependentAdd('Error in opsicalc: ' + E.Message, LLError);
    end;
  end;
end;

end.
