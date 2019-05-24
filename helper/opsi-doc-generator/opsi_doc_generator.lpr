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
  FileUtil,
  oslog,
  LCLIntf,
  odg_main;

type
  { opsidocgenerator }

  opsidocgenerator = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateOutput; virtual;
    procedure ViewHtml; virtual;
    procedure WriteHelp; virtual;
  end;

  { opsidocgenerator }

var
  myversion, outputFile : string;

procedure opsidocgenerator.DoRun;
var
  FileVerInfo                      : TFileVersionInfo;
  ErrorMsg, inputFile  : string;
  fileList                         : array of String;
  fileCounter                      : Integer;
  tempFile                         : TStringList;
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

  ErrorMsg := CheckOptions('h, p:, s:, o:, v', ['help', 'py:', 'os:', 'out:', 'view']);
  if ErrorMsg <> '' then
  begin
    writeln('Error : Parameters missing or invalid.');
    WriteLn('Info  : Read help for more information.');
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  if not HasOption('h', 'help') and not HasOption('p', 'py')
    and not HasOption('s', 'os') and not HasOption('o', 'out') then
  begin
    WriteLn('Error : Options missing or invalid.');
    WriteLn('Info  : Read this help for more information.');
    WriteLn('');
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('h', 'help') then
  begin
    LogDatei.log('Writing Help message.', LLinfo);
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('p', 'py') then
  begin
    LogDatei.log('Had option -p or --py=', LLinfo);
    fileList:= GetOptionValues('p', 'py');
    for fileCounter :=0 to Length(fileList)-1 do
    begin
      inputFile:= fileList[fileCounter];
      inputFile := ExpandFileName(inputFile);
      if not FileExists(inputFile) then
      begin
        LogDatei.log('Could not find the file: '+inputFile,LLcritical);
        writeln('Error : Could not find the file: '+inputFile);
        Terminate;
        Exit;
      end;
      LogDatei.log('Python source file: '+inputFile,LLinfo);
      WriteLn('Info  : Python source file: ' + inputFile);
      try
        tempFile := TStringList.Create;
        tempFile.LoadFromFile(inputFile);
        sourcelist.AddStrings(tempFile);
      finally
        tempFile.Free;
      end;
    end;
    convertPylibToAsciidoc();
    if HasOption('o', 'out') then
    begin
      CreateOutput;
      if HasOption('v', 'view') then
        ViewHtml;
    end
    else
    begin
      LogDatei.log('No output file specified.', LLcritical);
      writeln('Error : No output file specified.');
      WriteLn('Info  : Read help for more information.');
      Terminate;
      Exit;
    end;
  end;

  if HasOption('s', 'os') then
  begin
    LogDatei.log('Had option -s or --os=', LLinfo);
    fileList:= GetOptionValues('s', 'os');
    for fileCounter :=0 to Length(fileList)-1 do
    begin
      inputFile:= fileList[fileCounter];
      inputFile := ExpandFileName(inputFile);
      if not FileExists(inputFile) then
      begin
        LogDatei.log('Could not find the file: '+inputFile,LLcritical);
        writeln('Error : Could not find the file: '+inputFile);
        Terminate;
        Exit;
      end;
      LogDatei.log('Opsiscript source file: '+inputFile,LLinfo);
      WriteLn('Info  : Opsiscript source file: ' + inputFile);
      try
        tempFile := TStringList.Create;
        tempFile.LoadFromFile(inputFile);
        sourcelist.AddStrings(tempFile);
      finally
        tempFile.Free;
      end;
    end;
    convertOslibToAsciidoc();
    if HasOption('o', 'out') then
    begin
      CreateOutput;
      if HasOption('v', 'view') then
        ViewHtml;
    end
    else
    begin
      LogDatei.log('No output file specified.', LLcritical);
      writeln('Error : No output file specified.');
      WriteLn('Info  : Read help for more information.');
      Terminate;
      Exit;
    end;
  end;

  Terminate;
  Exit;
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

procedure opsidocgenerator.CreateOutput;
begin
  outputFile := GetOptionValue('o', 'out');
  outputFile := ExpandFileName(outputFile);
  if not FileExists(outputFile) then
  begin
    LogDatei.log('Could not find the file: '+outputFile,LLcritical);
    WriteLn('Info  : Could not find the file: ', outputFile);
    LogDatei.log('Creating output file. '+outputFile,LLinfo);
    WriteLn('Info  : Creating output file.');
    FileCreate(outputFile);
  end;
  WriteLn('Info  : Outputfile: ', outputFile);
  targetlist.SaveToFile(outputFile);
  LogDatei.log('Wrote asciidoc file to: '+outputFile,LLinfo);
  WriteLn('Info  : Asciidoc saved to: '+outputFile);
end;

procedure opsidocgenerator.ViewHtml;
begin
  if not callasciidoctor(outputFile) then
  begin
    writeln('callasciidoctor failed');
    LogDatei.log('callasciidoctor failed',LLcritical);
  end
  else
  begin
    WriteLn('Info  : HTML saved to: '+(ExtractFileNameWithoutExt(outputFile)+'.html'));
    WriteLn('Info  : Opening HTML file...');
    OpenDocument(ExtractFileNameWithoutExt(outputFile)+'.html');
  end;
end;

procedure opsidocgenerator.WriteHelp;
var
  filename: string;
begin
  filename := ExtractFileName(ParamStr(0));
  WriteLn('');
  WriteLn('=========================================================================================================================');
  writeln('                                   '+filename);
  writeln('                                    Version: ' + myversion);
  WriteLn('=========================================================================================================================');
  writeln(filename + ' generates asciidoc file for Python or Opsiscript sourcecodes.');
  writeln('It converts python docstrings or opsiscript comments to asciidoc.');
  writeln('And also converts asciidoc to html and shows the created html file in the default browser.');
  WriteLn('');

  writeln('Usage:');
  writeln('./' + filename + ' [Options] [filename]     Convert python or opsiscript source files to asciidoc.');
  WriteLn('');

  writeln('Options:');
  writeln('   -h , --help                                Displays this message.');
  writeln('   -s , --os=                                 Convert opsiscript source file to asciidoc and save it to the output file.');
  writeln('   -p , --py=                                 Convert python source file to asciidoc and save it to the output file.');
  writeln('   -o , --out=                                Save output to the specified file');
  WriteLn('');
  WriteLn('');
  WriteLn('Examples:');
  WriteLn('');
  WriteLn('Convert Opsiscript source code to asciidoc:');
  WriteLn('$ ./'+filename+ ' -s /temp/xyz.opsiscript -s /temp/abc.opsiscript -o /temp/opsi.asciidoc');
  WriteLn('$ ./'+filename+ ' --os=/temp/xyz.opsiscript --os=/temp/abc.opsiscript --out=/temp/opsi.asciidoc');
  WriteLn('');
  WriteLn('Convert Opsiscript source code to asciidoc and view HTML:');
  WriteLn('$ ./'+filename+ ' -s /temp/xyz.opsiscript -s /temp/abc.opsiscript -o /temp/opsi.asciidoc -v');
  WriteLn('$ ./'+filename+ ' --os=/temp/xyz.opsiscript --os=/temp/abc.opsiscript --out=/temp/opsi.asciidoc --view');
  WriteLn('');
  WriteLn('');
  WriteLn('Convert Python source code to asciidoc:');
  WriteLn('$ ./'+filename+ ' -p /temp/xyz.py -p /temp/abc.py -o /temp/py.asciidoc');
  WriteLn('$ ./'+filename+ ' --py=/temp/xyz.py --py=/temp/abc.py --out=/temp/py.asciidoc');
  WriteLn('');
  WriteLn('Convert Python source code to asciidoc and view HTML:');
  WriteLn('$ ./'+filename+ ' -p /temp/xyz.py -p /temp/abc.py -o /temp/py.asciidoc -v');
  WriteLn('$ ./'+filename+ ' --py=/temp/xyz.py --py=/temp/abc.py --out=/temp/py.asciidoc --view');
  WriteLn('');
  WriteLn('=========================================================================================================================');
  WriteLn('');
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

