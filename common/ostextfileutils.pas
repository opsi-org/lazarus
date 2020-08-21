unit osTextFileUtils;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Process;

procedure DeleteDuplicateLinesFromTextFile(FilePath: string);
{ deletes duplicate Lines from a text file where FilePath is the path to the text file }

procedure AddLineToTextFile(const NewLine: string; const FilePath: string;
  LineNumber: integer = -1; DublicateLines: boolean = False);
{ Add a new line at LineNumber or, if LineNumber := -1, at the end of a text file where NewLine is the content of the line. As default no line will
be added if the exact same line already exists anywhere in the text file. If DublicateLines is set to true
the line will be added irrespectively if the same line already exists in the text
file or not }

function FindLineInTextFile(const Line: string; const FilePath: string): integer;
{ Searches a line in a text file where Line is the complete content of the line for which is searched for.
As result the line number is given back or -1 when the line was not found }


implementation

procedure DeleteDuplicateLinesFromTextFile(FilePath: string);
var
  Text: TStringList;
  i: integer;
  j: integer;
begin
  try
    Text := TStringList.Create;
    Text.LoadFromFile(FilePath);
    for i := 0 to (Text.Count - 2) do
    begin
      for j := i + 1 to (Text.Count - 1) do
      begin
        if trim(Text[i]).CompareTo(trim(Text[j])) = 0 then
          //exact the same line already exists
          if ((trim(Text[i]) <> '') and (trim(Text[i]) <> LineEnding))
          //do not delete empty lines
          then
            Text.Delete(j);
      end;
    end;
    Text.SaveToFile(FilePath);
  finally
    FreeAndNil(Text);
  end;
end;

procedure AddLineToTextFile(const NewLine: string; const FilePath: string;
  LineNumber: integer; DublicateLines: boolean = False);
var
  Text: TStringList;
begin
  try
    Text := TStringList.Create;
    if FileExists(FilePath) then
      Text.LoadFromFile(FilePath)
    else
      FileCreate(FilePath);
    if (FindLineInTextFile(NewLine, FilePath) = -1) or DublicateLines then
      //-1 means not found
    begin
      if (LineNumber >= 1) then
        Text.Insert(LineNumber, NewLine)
      else
        Text.Add(NewLine);
      Text.SaveToFile(FilePath);
    end;
  finally
    FreeAndNil(Text);
  end;
end;

function FindLineInTextFile(const Line: string; const FilePath: string): integer;
var
  Text: TStringList;
  i: integer;
begin
  Result := -1;
  if FileExists(FilePath) then
  begin
    try
      Text := TStringList.Create;
      Text.LoadFromFile(FilePath);
      for i := 0 to (Text.Count - 1) do
      begin
        if trim(Text[i]).CompareTo(trim(Line)) = 0 then
          Result := i;
      end;
    finally
      FreeAndNil(Text);
    end;
  end;
end;


end.
