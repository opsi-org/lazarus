program opsiginatest;

//{$mode objfpc}{$H+}
{$MODE Delphi}
{$APPTYPE CONSOLE}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, windows
  { you can add units after this };

type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;
  TDllFunction = function: windows.BOOL; stdcall;

var
  mydll : Thandle;
  myFunc: TDllFunction;
  boolresult : winbool;
  myexitcode : integer;
  dllpath : array [0..max_path] of char;
  optionlist : TStringlist;



{ TMyApplication }

procedure TMyApplication.DoRun;
var
  ErrorMsg: String;
begin
  optionlist := TStringlist.Create;
  optionlist.Append('help');
  optionlist.Append('testdll:');

  // quick check parameters
  ErrorMsg:=CheckOptions('',optionlist);
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    halt(5);
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('help') or (paramcount=0) then begin
    WriteHelp;
    halt(5);
    Terminate;
    Exit;
  end;

  if HasOption('testdll')  then
  begin
  { add your program here }
    try
      dllpath := GetOptionValue('testdll');
      mydll := loadlibrary(dllpath);
      if (mydll < HINSTANCE_ERROR) then
      begin
        writeln('library can not be loaded or not found. ' + SysErrorMessage(GetLastError));
        myexitcode := 4;
        halt(myexitcode);
      end;
      if mydll <> 0 then
      begin
        myFunc := TDllFunction(GetProcAddress(mydll,'testload'));
        writeln('dll loaded');
        boolresult := myFunc();
        if boolresult then
        begin
          writeln('test succeeded');
          myexitcode := 0;
        end
        else
        begin
          writeln('test failed');
          myexitcode := 1;
        end;
      end
      else
      begin
        writeln('dll not loaded: '+IntToStr(GetLastError));
        myexitcode := 2;
      end;
    except
      writeln('dll load exception');
      myexitcode := 3;
       {unload a library}
       FreeLibrary(mydll);
    end;
    FreeLibrary(mydll);
    halt(myexitcode);
  end;
  Terminate;
end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMyApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TMyApplication.WriteHelp;
var
			filename : string;
begin
  filename := ExtractFileName(paramstr(0));
  writeln(paramstr(0));
  writeln(filename);
  writeln('Usage:');
  writeln(filename+ ' Option [Option]');
  writeln('Options:');
  writeln(' --help -> write this help and exit with exitcode 10');
  writeln(' --testdll="path-to-opsigina.dll" -> tests opsigina ');
  writeln('Exitcodes:');
  writeln('0 - test passed');
  writeln('1 - test failed ');
  writeln('2 - dll not loaded ');
  writeln('3 - dll load exception ');
  writeln('4 - library can not be loaded or not found ');
  writeln('5 - test not called ');
end;

var
  Application: TMyApplication;

//{$IFDEF WINDOWS}{$R opsiginatest.rc}{$ENDIF}

{$R *.res}

begin
  Application:=TMyApplication.Create(nil);
  Application.Title:='opsigina.dll test';
  Application.Run;
  Application.Free;
end.

