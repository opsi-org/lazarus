unit IconCollector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, StrUtils, ShellApi, LCLType, Graphics;

type

  { TIconCollector }

  TIconCollector = class(TObject)
    private
      FileNames :TStringList;
      function ExtractLine(PathToScript:String; const SearchString:String):String;
      function ParseLine(Line:String; PathToScript:String):String;

      //function ExtractIcon(OpsiScriptPath:String):String;
      //function ExtractProductID(OpsiScriptPath:String):String;
    public
      procedure ExtractIconFromExe(PathToExe:String);
      procedure ShowFilenames;
      constructor Create(DepotPath: String);
      destructor Destroy; override;
      function GetPathToIcon(PathToScript:string):string;
  end;

implementation

{ TIconCollector }

function TIconCollector.ExtractLine(PathToScript: String;
  const SearchString: String): String;
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
        if AnsiContainsText(Line, SearchString) then Break;
      end;
      Line := StringReplace(Line,'+',' + ',[rfReplaceAll, rfIgnoreCase]); //if no white spaces are between + and string(variable)
      Result:= DelSpace1(Trim(Line));//removes all not needed white spaces except of one white space between every token
    finally
      CloseFile(ScriptFile);
    end;
  end;
end;

function TIconCollector.ParseLine(Line: String; PathToScript:String): String;
var
  SplittedLine:TStringList;
begin
  Line := Trim(StringReplace(Line,'ShowBitmap','',[rfIgnoreCase]));
  Line := StringReplace(Line,'%ScriptPath%',PathToScript,[rfIgnoreCase]);
  SplittedLine := TStringList.Create;
  try
    SplittedLine.Delimiter := ' ';
    SplittedLine.DelimitedText := Line;
    //SplittedLine := Line.Split(' ');
    //Line := DelChars(Line, '+');
    //Line := ExtractWord(0,Line,[' ']);

    WriteLn(SplittedLine[0]);
    Result := Line;
  finally
    if Assigned(SplittedLine) then
      FreeAndNil(SplittedLine);
  end;
end;

procedure TIconCollector.ExtractIconFromExe(PathToExe: String);
var
  IconLarge :HICON;
  IconSmall :HICON;
  Picture : TPicture;
begin
  if ExtractIconEx(PChar(PathToExe),0,IconLarge,IconSmall,1) > NULL then
  begin
    try
      Picture := TPicture.Create;
      Picture.Icon.Handle := IconLarge;
      Picture.SaveToFile('C:\Users\Jan\Test\anydesk\TestIcon_1.ico');
     finally
      Picture.Free;
    end;
   end;
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
    Line := ExtractLine(FileNames[i],'ShowBitmap');
    PathToScript := ExtractFilePath(FileNames[i]);//ExcludeTrailingPathDelimiter
    WriteLn(ParseLine(Line,PathToScript));
  end;
end;

end.

