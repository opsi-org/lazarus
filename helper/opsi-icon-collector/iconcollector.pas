unit IconCollector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil;

type

  { TIconCollector }

  TIconCollector = class(TObject)
    private
      FileNames :TStringList;
      //function ExtractIcon(OpsiScriptPath:String):String;
      //function ExtractProductID(OpsiScriptPath:String):String;
    public
      procedure ShowFilenames;
      constructor Create(DepotPath: String);
      destructor Destroy; override;
      function GetPathToIcon(PathToScript:string):string;
  end;

implementation

{ TIconCollector }

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

end.

