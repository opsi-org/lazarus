unit opsiDynamicLibraries;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DynLibs, SyncObjs;

type

  { TOpsiDynamicLibrary }

  TOpsiDynamicLibrary = class(TObject)
  protected
    FCSLoadLib: TCriticalSection;
    FLibHandle: THandle;
    FLibName: string;
    FLibPath: string;
    FLibLoaded: boolean;
    FLibVersion: string;
    FErrorMessage: string;
  public
    constructor Create(aLibName: string; aLibPath: string = ''); overload; virtual;
    destructor Destroy; override;
    procedure Load;
    property Loaded: boolean read FLibLoaded;
    property Name: string read FLibName write FLibName;
    property Path: string read FLibPath write FLibPath;
    property ErrorMessage: string read FErrorMessage; //write FErrorMessage;
  end;

implementation

{ TOpsiDynamicLibrary }

constructor TOpsiDynamicLibrary.Create(aLibName: string; aLibPath: string = '');
begin
  inherited Create;
  FLibPath := aLibPath;
  FLibName := aLibName;
  FCSLoadLib := TCriticalSection.Create;
end;

destructor TOpsiDynamicLibrary.Destroy;
begin
  FCSLoadLib.Enter;
  try
    if FLibHandle <> Nilhandle then UnloadLibrary(FLibHandle);
  finally
    FCSLoadLib.Leave;
    FreeAndNil(FCSLoadLib);
  end;
  inherited Destroy;
end;

procedure TOpsiDynamicLibrary.Load;
begin
  FCSLoadLib.Enter;
  try
    if FLibPath <> '' then
    begin
      // load from given path
      FLibHandle := LoadLibrary(FLibPath + PathDelim + FLibName);
      // try again from current directory if failed
      if FLibHandle = 0 then
        FLibHandle := LoadLibrary(ProgramDirectory + FLibName);
    end
    else
      FLibHandle := LoadLibrary(FLibName);
    // try again from current directory if failed
    if FLibHandle = 0 then
      FLibHandle := LoadLibrary(ProgramDirectory + FLibName);

    if FLibHandle = 0 then
    begin
      FLibLoaded := False;
      FErrorMessage := 'ERROR: could not load ' + FLibName + ' from ';
      if FLibPath <> '' then
        FErrorMessage := FErrorMessage + 'path ' + FLibPath + ' or ';
      FErrorMessage := FErrorMessage + 'current directory: ' + GetLoadErrorStr;
    end
    else
      FLibLoaded := True;
  finally
    FCSLoadLib.Leave;
  end;
end;

end.
