unit IconCollector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, StrUtils, LCLType, Graphics,
  DateUtils;

type

  TSearchShift = (ssFirst, ssAll);

  TShowStatus = procedure(MessageText:string) of object;
  //TSetFileNames = procedure(FileNames:TStringList) of object;


  { TFindFilesThread }

  TFindFilesThread = class(TThread)
      FFileNames : TStringList;
      FDepotPath : String;
      //FSetFileNames : TSetFileNames;
    protected
      procedure Execute; override;
      //procedure FileNames;
  public
    constructor Create(DepotPath:String);
    destructor Destroy; override;
  end;


  { TIconCollector }

  TIconCollector = class(TObject)
    private
      FPathToOckCustom : String;
      FMessageText : String; //for status report
      FFileNames : TStringList;
      FIconsList  : TStringList;
      FPathToDepot : string;
      FFindFilesThread : TFindFilesThread;
      function ExtractLine(const SearchString:String; PathToScript:String):String;
      //procedure SetFileNames(FileNames:TStringList);
      procedure ParseLineShowBitmap(Line:String; PathToScript:String);
      function PrepareLine(Line:String):String;
      function IsVariable(Token:String):boolean;
      function ReplaceTokenWithValue(Token:String; PathToScript:String):String;
    public
      procedure FindOpsiScriptFiles(ShowStatus:TShowStatus);
      procedure ExtractPathToIcon(ShowStatus:TShowStatus);
      //procedure ExtractIconFromExe(PathToExe:String);
      function ShowOpsiScriptFilenames:String;
      function ShowIconList:String;
      constructor Create(DepotPath: String);overload;
      destructor Destroy; override;
  end;



implementation


{ TFindScriptFilesThread }

procedure TFindFilesThread.Execute;
begin
  if not terminated then
  begin
    FFileNames := FindAllFiles(FDepotPath,'setup.opsiscript;setup32.opsiscript');
    Terminate;
    //Synchronize(@FileNames);
    //Terminate;
  end;
end;

(*
procedure TFindFilesThread.FileNames;
begin
  FSetFileNames(FFileNames);
end;
*)

constructor TFindFilesThread.Create(DepotPath: String);
begin
  inherited Create(false);
  FreeOnTerminate := False;
  FDepotPath := DepotPath;
  //FSetFileNames := SetFileNames;
end;

destructor TFindFilesThread.Destroy;
begin
  //FFileNames.Free; //do not free because this TSringList is used in main thread and freed there
  inherited Destroy;
end;

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

(*
procedure TIconCollector.SetFileNames(FileNames: TStringList);
begin
  FFileNames := FileNames;
end;
*)

procedure TIconCollector.FindOpsiScriptFiles(ShowStatus:TShowStatus);
var
  StartTime :TTime;
  i : integer = 0;
const
  StatusMessage = #13'Processing [%s] ';
  Progress: array [0..3] of char = ('-','\','|','/');
begin
  FFindFilesThread := TFindFilesThread.Create(FPathToDepot);
  StartTime := Time;
  while not FFindFilesThread.Terminated do begin
    if MilliSecondsBetween(Time, StartTime) > 100 then
    begin
      FMessageText := Format(StatusMessage,[Progress[i]]);
      StartTime := Time;
      if i < 3 then inc(i) else i := 0;
      ShowStatus(FMessageText);
    end;
  end;
  //FFindFilesThread.WaitFor; //should not be necessary because thread should be already terminated
  //FFileNames := TStringList.Create; //use this if the StringList is copied from FFindFilesThread.FFileNames e.g. see line below
  //FFileNames.Text := FFindFilesThread.FFileNames.Text;
  FFileNames := FFindFilesThread.FFileNames;
  FFindFilesThread.Free;
end;

procedure TIconCollector.ParseLineShowBitmap(Line: String; PathToScript:String);
var
  SplittedLine : TStringList;
  i            : integer;
  Concatenate  : boolean;
  IconPath     : String;
  ProductID    : String;
begin
  IconPath := '';
  ProductID := '';
  Line := PrepareLine(Line);//one white space between every token
  Line := Trim(StringReplace(Line,'ShowBitmap','',[rfIgnoreCase]));//remove ShowBitmap
  //PathToScriptFolder := ExtractFilePath(PathToScript);//ExcludeTrailingPathDelimiter
  Line := StringReplace(Line,'%ScriptPath%', ExtractFilePath(PathToScript),[rfIgnoreCase]);//replace %ScriptPath% with path
  SplittedLine := TStringList.Create;
  try
    { Splitt line into tokens }
    SplittedLine.Delimiter := ' ';
    SplittedLine.DelimitedText := Line;
    //SplittedLine := Line.Split(' ');
    //Line := DelChars(Line, '+');
    //Line := ExtractWord(0,Line,[' ']);
    { Parse tokens }
    Concatenate := True;
    for i := 0 to SplittedLine.Count-1 do
    begin
      if IsVariable(SplittedLine[i]) then
      begin
        if (LowerCase(SplittedLine[i]) = LowerCase('$ProductId$'))
          and (ProductID = '') then
        begin
          ProductID := ReplaceTokenWithValue(SplittedLine[i], PathToScript);
          SplittedLine[i] := ProductID;
        end
        else
          SplittedLine[i] := ReplaceTokenWithValue(SplittedLine[i], PathToScript);
      end;
      if Concatenate then IconPath := IconPath + SplittedLine[i];
      if SplittedLine[i] = '+' then Concatenate := True else Concatenate := False;
    end;
    //SplittedLine.Text := TrimFilename(SplittedLine.Text); //for testing/debugging
    //WriteLn(SplittedLine.Text); //for testing/debugging
    IconPath := TrimFilename(SwitchPathDelims(IconPath, pdsSystem));
    { Copy icon to new destination if ProductID exists}
    if (ProductID <> '') then
    begin
      if CopyFile(IconPath, FPathToOckCustom + PathDelim + ExtractFilename(IconPath)) then //add to list only if image could be copied e.g. exists
      begin
        //if (FIconsList.IndexOfName(ProductID) <> -1) then
        { if the name-value pair does not exist it will be created, otherwise the current value is overridden }
        FIconsList.Values[ProductID] := ExtractFilename(IconPath)
        //else FIconsList.Add(ProductID + '=' + ExtractFilename(IconPath));
      end;
    end;
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
  if (Token[1] = '$') and (Token[Token.Length] = '$') then Result := True
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

function TIconCollector.ShowOpsiScriptFilenames:String;
begin
  Result := FFileNames.Text;
end;

function TIconCollector.ShowIconList:String;
begin
  Result := FIconsList.Text;
end;

constructor TIconCollector.Create(DepotPath: String);
begin
  inherited Create;
  FPathToDepot := SwitchPathDelims(DepotPath,pdsSystem);
  FPathToOckCustom := FPathToDepot + PathDelim
    + SwitchPathDelims('opsi-client-agent\files\opsi\opsiclientkiosk\ock_custom',pdsSystem);
  //FPathToOckCustom := SwitchPathDelims('C:\Users\Jan\Test', pdsSystem); //testing windows
  //FPathToOckCustom := SwitchPathDelims('\home\user\Test',pdsSystem); //testing linux

  FIconsList := TStringList.Create;
  if FileExists(FPathToOckCustom+PathDelim+'IconsList.txt') then
    FIconsList.LoadFromFile(FPathToOckCustom+PathDelim+'IconsList.txt');
  //FFileNames := TStringList.Create;
  //FFileNames := FindAllFiles(DepotPath,'setup.opsiscript;setup32.opsiscript');
end;

destructor TIconCollector.Destroy;
begin
  inherited Destroy;
  FIconsList.SaveToFile(FPathToOckCustom+PathDelim+'IconsList.txt');
  FFileNames.Free;
  FIconsList.Free;
end;

procedure TIconCollector.ExtractPathToIcon(ShowStatus:TShowStatus);
var
  i : integer;
  Percent : integer;
  Line : String;
const
  StatusMessage = #13'Processing %d of %d [%d%%] ';
begin
  for i := 0 to FFileNames.Count-1 do
  begin
    Percent := Trunc((i+1)*1.0 /(FFileNames.Count) *100);
    Line := ExtractLine('ShowBitmap', FFileNames[i]);
    ParseLineShowBitmap(Line,FFileNames[i]);
    FMessageText := Format(StatusMessage,[(i+1),(FFileNames.Count), Percent]);
    ShowStatus(FMessageText);
  end;
end;

{Procedure is just a test not shure if useful in this application}
(*procedure TIconCollector.ExtractIconFromExe(PathToExe: String);
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
end;*)

end.

