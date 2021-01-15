unit ostxstringlist;

{$mode delphi}

interface

uses
  Classes, SysUtils,
  oslog,
  osencoding,
  osparserhelper;

Type

  TXStringList = class(TStringList)
  public
    function ReplaceInLine(const OldLine, SearchItem, Replaceitem: string;
      MatchCase: boolean; var NewLine: string): boolean;
    function GlobalReplace(Startline: integer; const SearchItem, Replaceitem: string;
      MatchCase: boolean): boolean;
    procedure EliminateLinesStartingWith(const startS: string; MatchCase: boolean);
    procedure SaveToFile(const FileName: string; encodingtype: string); overload;
    procedure SaveToFile(const FileName: string); override; overload;
    procedure SaveToFile(const FileName: string; encodingtype: string;
      raise_on_error: boolean); overload;
    function FuncSaveToFile(const FileName: string; encodingtype: string): boolean;
      overload;
    function FuncSaveToFile(const FileName: string): boolean; overload;
    //procedure loadFromUnicodeFile(const Filename: string; codepage: word);
    function getStringValue(const keyname: string): string;
    // returns the string value of a handmade properties list with separator either '=' or ':'
    // we return values[keyname], until further notice
  end;


implementation

{ TXStringList }

function TXStringList.ReplaceInLine(const OldLine, SearchItem, Replaceitem: string;
  MatchCase: boolean; var NewLine: string): boolean;

var
  remainder: string = '';
  searchline: string = '';
  searchUItem: string = '';
  position: integer = 0;

begin
  remainder := OldLine;
  NewLine := '';
  Result := False;
  if MatchCase then
  begin
    SearchLine := Remainder;
    SearchUItem := SearchItem;
  end
  else
  begin
    SearchLine := AnsiUpperCase(Remainder);
    SearchUItem := AnsiUpperCase(SearchItem);
  end;

  Position := pos(searchUItem, SearchLine);
  if Position > 0 then
  begin
    Result := True;

    while (length(SearchLine) > 0) and (Position > 0) do
    begin
      NewLine := NewLine + copy(Remainder, 1, Position - 1) + ReplaceItem;

      system.Delete(SearchLine, 1, Position + length(searchUItem) - 1);
      system.Delete(Remainder, 1, Position + length(searchUItem) - 1);

      Position := pos(searchUItem, SearchLine);
    end;

    NewLine := NewLine + Remainder;
  end;

end;


function TXStringList.GlobalReplace
  (Startline: integer; const SearchItem, Replaceitem: string;
  MatchCase: boolean): boolean;
var
  i: integer = 0;
  //found: boolean;
  NewLine: string = '';
begin

  Result := False;

  for i := Startline to Count do
    if ReplaceInLine(Strings[i - 1], SearchItem, ReplaceItem, MatchCase, NewLine) then
    begin
      Result := True;
      Delete(i - 1);
      insert(i - 1, NewLine);
    end;

end;

procedure TXStringList.EliminateLinesStartingWith(const startS: string;
  MatchCase: boolean);
var
  i: integer = 0;
  compareS: string = '';
  lineStart: string = '';
begin

  i := 0;
  if not matchCase then
    compareS := LowerCase(startS)
  else
    compareS := startS;

  while i < Count do
  begin

    lineStart := copy(Strings[i], 1, length(startS));

    if not MatchCase then
      lineStart := LowerCase(lineStart);

    if lineStart = compareS then
      Delete(i)
    else
      Inc(i);

  end;

end;


procedure TXStringList.SaveToFile(const FileName: string);
begin
  SaveToFile(Filename, 'system');
end;

procedure TXStringList.SaveToFile(const FileName: string; encodingtype: string);
begin
  SaveToFile(Filename, encodingtype, False);
end;

procedure TXStringList.SaveToFile(const FileName: string; encodingtype: string;
  raise_on_error: boolean);
var
  (*
  myfile: system.TextFile;
  i: integer;
  usedenc: string;
  *)
  myfilename: string;
  LogS : string;
begin
  LogDatei.log('Save to file with encoding: ' + encodingtype, LLDebug);
  try
    myfilename := ExpandFileName(FileName);
    saveTextFileWithEncoding(TStrings(self), myfilename, encodingtype);
    (*
    if LowerCase(encodingtype) = 'utf8' then
    begin
      //utf-8
      AssignFile(myfile, myfilename);
      Rewrite(myfile);
      LogDatei.log('Will save (utf8) to file: ' + myfilename + ' :', LLDebug2);
      LogDatei.log('-----------------', LLDebug3);
      for i := 0 to Count - 1 do
      begin
        writeln(myfile, strings[i]);
        LogDatei.log(strings[i], LLDebug3);
      end;
      LogDatei.log('-----------------', LLDebug3);
      CloseFile(myfile);
      if LogDatei.UsedLogLevel >= LLDebug3 then
      begin
        LogDatei.log('Read file: ' + myfilename + ' :', LLDebug2);
        LogDatei.log('-----------------', LLDebug3);
        logdatei.includelogtail(myfilename, Count, 'utf8');
        LogDatei.log('-----------------', LLDebug3);
      end;
    end
    else
    begin
      AssignFile(myfile, myfilename);
      Rewrite(myfile);
      LogDatei.log('Will save (' + encodingtype + ') to file: ' +
        myfilename + ' :', LLDebug2);
      LogDatei.log('-----------------', LLDebug3);

      //for i := 0 to Count - 1 do
      //begin
      //  writeln(myfile, reencode(strings[i], 'utf8', usedenc, encodingtype));
      //  LogDatei.log(reencode(strings[i], 'utf8', usedenc, encodingtype), LLDebug3);
      //end;

      writeln(myfile, reencode(text, 'utf8', usedenc, encodingtype));

      LogDatei.log('-----------------', LLDebug3);
      CloseFile(myfile);
      if LogDatei.UsedLogLevel >= LLDebug3 then
      begin
        LogDatei.log('Read file ' + myfilename + ' with encoding: ' +
          encodingtype, LLDebug2);
        LogDatei.log('-----------------', LLDebug3);
        logdatei.includelogtail(myfilename, Count, encodingtype);
        LogDatei.log('-----------------', LLDebug3);
      end;
    end;
  *)

  except
    on e: Exception do
    begin
      if not raise_on_error then
      begin
        LogS := e.message;
        LogS := 'Error: ' + myfilename +
          ' could not be saved back - will not retry, error message: "' +
          LogS + '"';
        LogDatei.log(LogS, LLError);
      end;
      if raise_on_error then
        raise;
    end;
  end;
end;

function TXStringList.FuncSaveToFile(const FileName: string): boolean;
begin
  Result := FuncSaveToFile(FileName, 'system');
end;

function TXStringList.FuncSaveToFile(const FileName: string;
  encodingtype: string): boolean;
var
  myfilename: string;
  i: integer;
  LogS : string;
begin
  Result := False;
  try
    myfilename := ExpandFileName(FileName);
    SaveToFile(myfilename, encodingtype, True);
    LogS := myfilename + ' saved back with encoding: ' + encodingtype;
    LogDatei.log_prog(LogS, LLDebug);
    Result := True;
  except
    on e: Exception do
    begin
      LogS := e.message;
      LogS := 'Warning: ' + myfilename +
        ' could not be saved back - will retry, error message: "' + LogS + '"';
      LogDatei.log_prog(LogS, LLWarning);
      try
        Sleep(100);
        SaveToFile(myfilename, encodingtype, True);
        LogS := myfilename + ' saved back with encoding: ' + encodingtype;
        LogDatei.log_prog(LogS, LLDebug);
        Result := True;
      except
        on e: Exception do
        begin
          LogS := e.message;
          LogS := 'Warning: ' + myfilename +
            ' could not be saved back - will retry, error message: "' + LogS + '"';
          LogDatei.log_prog(LogS, LLWarning);
          try
            Sleep(100);
            SaveToFile(myfilename, encodingtype, True);
            LogS := myfilename + ' saved back with encoding: ' + encodingtype;
            LogDatei.log_prog(LogS, LLDebug);
            Result := True;
          except
            on e: Exception do
            begin
              LogS := e.message;
              LogS := 'Warning: ' + myfilename +
                ' could not be saved back - will retry, error message: "' + LogS + '"';
              LogDatei.log_prog(LogS, LLWarning);
              try
                Sleep(100);
                SaveToFile(myfilename, encodingtype, True);
                LogS := myfilename + ' saved back with encoding: ' + encodingtype;
                LogDatei.log_prog(LogS, LLDebug);
                Result := True;
              except
                on e: Exception do
                begin
                  LogS := e.message;
                  LogS := 'Warning: ' + myfilename +
                    ' could not be saved back - will retry, error message: "' +
                    LogS + '"';
                  LogDatei.log_prog(LogS, LLWarning);
                  try
                    Sleep(100);
                    SaveToFile(myfilename, encodingtype, True);
                    LogS := myfilename + ' saved back with encoding: ' + encodingtype;
                    LogDatei.log_prog(LogS, LLDebug);
                    Result := True;
                  except
                    on e: Exception do
                    begin
                      LogS := e.message;
                      LogS := 'Error: ' + myfilename +
                        ' could not be saved back - will not retry, error message: "' +
                        LogS + '"';
                      LogDatei.log_prog(LogS, LLError);
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;


function TXStringList.getStringValue(const keyname: string): string;
var
  i: integer;
begin
  Result := NULL_STRING_VALUE;
  if indexOfName(keyname) <> -1 then
    // the fast way:
    Result := trim(values[keyname])
  else
  begin
    // let us retry with trimed keys
    for i := 0 to Count - 1 do
    begin
      if Names[i] <> '' then
        if lowerCase(trim(Names[i])) = lowerCase(trim(keyname)) then
          Result := trim(values[Names[i]]);
    end;
  end;
end;

(*
procedure TXStringlist.loadFromUnicodeFile(const Filename: string; codepage: word);
begin
  LoadFromFile(ExpandFileName(Filename));
  Text := reencode(Text, 'ucs2be');
end;
*)

end.

