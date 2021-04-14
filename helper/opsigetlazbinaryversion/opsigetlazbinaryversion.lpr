program opsigetlazbinaryversion;

{$mode objfpc}{$H+}

(* changelog
4.1.0.0 (15.12.2020) initial for linux and mac (d.oertel@uib.de)
*****************************************************)

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  fileinfo,
  elfreader,
  machoreader,
  winpeimagereader,
  (*
  fileinfo, {$IFDEF LINUX}
  elfreader, {$ENDIF LINUX} {$IFDEF Darwin}
  machoreader, {$ENDIF} *)
  CustApp { you can add units after this };

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

var
  myVersion: string;
  filename: string;
  fileVersion: string;
  optionlist: TStringList;
  Application: TMyApplication;

  { TMyApplication }

  procedure TMyApplication.DoRun;
  var
    ErrorMsg: string;
    FileVerInfo: TFileVersionInfo;
  begin
    FileVerInfo := TFileVersionInfo.Create(nil);
    try
      FileVerInfo.FileName := ParamStr(0);
      FileVerInfo.ReadFileInfo;
      myVersion := FileVerInfo.VersionStrings.Values['FileVersion'];
    finally
      FileVerInfo.Free;
    end;
    optionlist := TStringList.Create;
    optionlist.Add('help');
    optionlist.Add('file:');
    ErrorMsg := Application.CheckOptions('hf:', optionlist);
    if ErrorMsg <> '' then
    begin
      ShowException(Exception.Create(ErrorMsg));
      writeln('Given command line options are not valid');
      system.ExitCode := 20;
      WriteHelp;
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

    if Application.HasOption('f', 'file') then
    begin
      //preloglist.Add('Found Parameter file');
      filename := Application.GetOptionValue('f', 'file');
      if not FileExists(filename) then
      begin
        writeln('Given file: ' + filename + ' not found');
        system.ExitCode := 21;
      end
      else
      begin
        FileVerInfo := TFileVersionInfo.Create(nil);
        try
          try
            FileVerInfo.FileName := filename;
            FileVerInfo.ReadFileInfo;
            fileVersion := FileVerInfo.VersionStrings.Values['FileVersion'];
            writeln('fileversion=' + fileVersion);
          except
            on e: Exception do
            begin
              writeln('Exception while reading version from file: ' + filename);
              writeln('Error: ' + e.message);
              system.ExitCode := 22;
            end
          end;
        finally
          FileVerInfo.Free;
        end;
      end;
    end;


    // stop program loop
    Terminate;
    halt;
  end;

  constructor TMyApplication.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor TMyApplication.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TMyApplication.WriteHelp;
  var
    filename: string;
    msg: string;
  begin
    filename := ExtractFileName(Application.ExeName);
    writeln('This is ' + filename + ' version: ' + myVersion + ' (c) uib gmbh, AGPLv3');
    writeln('Usage: ' + filename + ' Options');
    writeln('Options:');
    writeln(' --help -> write this help and exit');
    writeln(' -h -> write this help and exit');
    writeln(' --file=<path to the file to analyze>');
    writeln(' -f=<path to the file to analyze>');
    writeln('Options -f or --file are required');
    writeln('Output if everything is ok:');
    writeln('fileversion=<found version string>');
  end;

{$R *.res}

begin
  Application := TMyApplication.Create(nil);
  Application.Title:='MyApplication';
  Application.Run;
  Application.Free;
end.








