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
      function ParseLine(Line:String):String;

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
      Result:= Trim(Line);
    finally
      CloseFile(ScriptFile);
    end;
  end;
end;

function TIconCollector.ParseLine(Line: String): String;
begin

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
begin
  for i := 0 to FileNames.Count-1 do
  begin
    WriteLn(ExtractLine(FileNames[i],'ShowBitmap'));
  end;
end;

end.

