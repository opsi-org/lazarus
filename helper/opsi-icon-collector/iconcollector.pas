unit IconCollector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, StrUtils, ShellApi, LCLType, Graphics;

type

  TSearchShift = (ssFirst, ssAll);

  { TIconCollector }

  TIconCollector = class(TObject)
    private
      FileNames : TStringList;
      function ExtractLine(const SearchString:String; PathToScript:String):String;
      function ParseLineShowBitmap(Line:String; PathToScript:String):String;
      function PrepareLine(Line:String):String;
      function IsVariable(Token:String):boolean;
      function ReplaceTokenWithValue(Token:String; PathToScript:String):String;
      //function ConcatenateTokens
      //function ExtractIcon(OpsiScriptPath:String):String;
      //function ExtractProductID(OpsiScriptPath:String):String;
    public
      procedure ExtractIconFromExe(PathToExe:String);
      procedure ShowFilenames;
      constructor Create(DepotPath: String);overload;
      destructor Destroy; override;
      function GetPathToIcon(PathToScript:string):string;
  end;

implementation

{ TIconCollector }

function TIconCollector.ExtractLine(const SearchString: String; PathToScript: String): String;
var
  ScriptFile: TextFile;
  Line :string;
begin
  Result := '';
  if FileExists(PathToScript) then
  begin
    try
      AssignFile(ScriptFile,PathToScript);
      Reset(ScriptFile);
      while not EOF(ScriptFile) do
      begin
        ReadLn(ScriptFile, Line);
        if AnsiContainsText(PrepareLine(Line), SearchString) then Break;
      end;
      Result:= Line;
    finally
      CloseFile(ScriptFile);
    end;
  end;
end;


function TIconCollector.ParseLineShowBitmap(Line: String; PathToScript:String): String;
var
  SplittedLine : TStringList;
  i : integer;
  Concatenate : boolean;
  //PathToScriptFolder : String;
begin
  Result := '';
  Line := PrepareLine(Line);//one white space between every token
  Line := Trim(StringReplace(Line,'ShowBitmap','',[rfIgnoreCase]));//remove ShowBitmap
  //PathToScriptFolder := ExtractFilePath(PathToScript);//ExcludeTrailingPathDelimiter
  Line := StringReplace(Line,'%ScriptPath%', ExtractFilePath(PathToScript),[rfIgnoreCase]);//replace %ScriptPath% with path
  SplittedLine := TStringList.Create;
  try
    SplittedLine.Delimiter := ' ';
    SplittedLine.DelimitedText := Line;
    //SplittedLine := Line.Split(' ');
    //Line := DelChars(Line, '+');
    //Line := ExtractWord(0,Line,[' ']);
    Concatenate := True;
    for i := 0 to SplittedLine.Count-1 do
    begin
      if IsVariable(SplittedLine[i]) then
        SplittedLine[i] := ReplaceTokenWithValue(SplittedLine[i], PathToScript);
      if Concatenate then Result := Result + SplittedLine[i];
      if SplittedLine[i] = '+' then Concatenate := True else Concatenate := False;
    end;
    WriteLn(SplittedLine.Text);
    //Result := Line;
  finally
    if Assigned(SplittedLine) then
      FreeAndNil(SplittedLine);
  end;
end;

function TIconCollector.PrepareLine(Line: String): String;
begin
  Line := StringReplace(Line,'+',' + ',[rfReplaceAll, rfIgnoreCase]); //if no white spaces are between + and string(variable)
  Line := StringReplace(Line,'=',' = ',[rfReplaceAll, rfIgnoreCase]); //if no white spaces are between = and string(variable)
  Result:= DelSpace1(Trim(Line));//removes all not needed white spaces except of one white space between every token
end;

function TIconCollector.IsVariable(Token: String): boolean;
begin
  if (Token[1] = '$') and  (Token[Token.Length] = '$') then Result := True
  else  Result := False;
  //if Result then WriteLn('Token[1]: ', Token[1], ' Token[Token.Length]: ', Token[Token.Length]); //just for testing
end;

function TIconCollector.ReplaceTokenWithValue(Token: String;
  PathToScript: String):String;
var
  Line : String;
begin
  Line := ExtractLine('Set ' + Token + ' = ', PathToScript);
  Line := PrepareLine(Line);
  Result := Trim(StringReplace(Line,'Set ' + Token + ' = ','',[rfIgnoreCase])).DeQuotedString('"');
end;

procedure TIconCollector.ShowFilenames;
begin
  WriteLn(FileNames.Text);
end;


constructor TIconCollector.Create(DepotPath: String);
begin
  inherited Create;
  FileNames := FindAllFiles(DepotPath,'setup.opsiscript;setup32.opsiscript');
end;

destructor TIconCollector.Destroy;
begin
  inherited Destroy;
  FileNames.Free;
end;

function TIconCollector.GetPathToIcon(PathToScript: string): string;
var
  i : integer;
  Line : String;
begin
  for i := 0 to FileNames.Count-1 do
  begin
    Line := ExtractLine('ShowBitmap', FileNames[i]);
    WriteLn(ParseLineShowBitmap(Line,FileNames[i]));
  end;
end;

{Procedure is just a test not shure if useful in this application}
procedure TIconCollector.ExtractIconFromExe(PathToExe: String);
var
  IconLarge :HICON;
  IconSmall :HICON;
  Picture : TPicture;
begin
  if ExtractIconEx(PChar(PathToExe),0,IconLarge,IconSmall,1) > NULL then
  begin
    Picture := TPicture.Create;
    try
      Picture.Icon.Handle := IconLarge;
      Picture.SaveToFile('C:\Users\Jan\Test\anydesk\TestIcon_1.ico');
     finally
      Picture.Free;
    end;
   end;
end;

end.

