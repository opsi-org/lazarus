program opsi_doc_generator;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  fileinfo,
  winpeimagereader, // {need this for reading exe info}
  elfreader, // {needed for reading ELF executables}
  Classes,
  SysUtils,
  CustApp,
  oslog,
  odg_main;

type

  { opsidocgenerator }

  opsidocgenerator = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { opsidocgenerator }

var
  myversion : string;

  procedure opsidocgenerator.DoRun;
  var
    ErrorMsg: string;
    myoptions, mynonoptions : TStringList;
    infilename : string;
    FileVerInfo : TFileVersionInfo;
   begin

     //from http://wiki.freepascal.org/Show_Application_Title,_Version,_and_Company
     FileVerInfo := TFileVersionInfo.Create(nil);
     try
       FileVerInfo.FileName := ParamStr(0);
       FileVerInfo.ReadFileInfo;
       myversion := FileVerInfo.VersionStrings.Values['FileVersion'];
     finally
       FileVerInfo.Free;
     end;

    myoptions := TStringList.Create;
    mynonoptions := TStringList.Create;
    // quick check parameters
    ErrorMsg := CheckOptions('h', ['help'], myoptions, mynonoptions);
    if ErrorMsg <> '' then
    begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    if mynonoptions.Count = 0 then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    infilename := mynonoptions.Strings[0];
    infilename := ExpandFileName(infilename);
    if not FileExists(infilename) then
    begin
      writeln('Error: Could not find given file: '+infilename);
      WriteHelp;
      Terminate;
      Exit;
    end;

    sourcelist.LoadFromFile(infilename);
    convertOslibToAsciidoc(infilename);
    save_compile_show(infilename);
    // stop program loop
    Terminate;
  end;

  constructor opsidocgenerator.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor opsidocgenerator.Destroy;
  begin
    inherited Destroy;
  end;

  procedure opsidocgenerator.WriteHelp;
  var
    filename: string;
  begin
    filename := ExtractFileName(ParamStr(0));
    //writeln(ParamStr(0));
    writeln('Creates asciidoc from commented opsiscript library code');
    writeln(' and calls asciidoctor to convert asciidoc to html');
    writeln('and shows created html file in browser.');
    writeln(filename);
    writeln('Version: ' + myversion);
    writeln('Usage:');
    writeln(filename + ' [Options] inputfile');
    writeln('Options:');
    writeln(' --help -> write this help and exit');
    (*
    if Assigned(LogDatei) then
      LogDatei.Close;
      *)
    Terminate;
    halt(-1);
    Exit;
  end;

var
  Application: opsidocgenerator;

{$R *.res}

begin
  Application := opsidocgenerator.Create(nil);
  Application.Title := 'opsi doc generator';
  Application.Run;
  Application.Free;
end.
