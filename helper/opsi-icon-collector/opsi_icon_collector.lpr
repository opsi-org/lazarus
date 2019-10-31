program opsi_icon_collector;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, IconCollector, Interfaces, LazFileUtils, DateUtils
  { you can add units after this };

type

  { TOpsiIconCollector }

  TOpsiIconCollector = class(TCustomApplication)
  protected
    FProgressStatusStartTime : TTime;
    FProgressStatusFirstCall : boolean;
    FProgressStatusSymbol : integer;
    procedure DoRun; override;
    procedure CollectIcons(DepotPath:String);
    procedure ProgressStatus();
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TOpsiIconCollector }

procedure TOpsiIconCollector.DoRun;
var
  ErrorMsg: String;
  ShortOpts: String = 'h';
  LongOpts: String = 'help';
  //ParamList : TStringList;
begin
  // quick check options
  ErrorMsg:=CheckOptions(ShortOpts, LongOpts);
  if ErrorMsg<>'' then
  begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse options
  if HasOption('h', 'help') then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }

  //parse parameter
  //ParamList := TStringList.Create;
  //GetNonOptions(ShortOpts, LongOpts, ParamList);
  try
    If Params[1] <> '' then
    begin
      WriteLn('Params[1]: ' + Params[1]);//for testing logging
      if DirPathExists(Params[1]) then
      begin
        //WriteLn('***************************');
        //WriteLn('*  opsi-icon- collector   *');
        //WriteLn('***************************');
        CollectIcons(Params[1]);
      end;
    end
    else
    begin
      WriteLn('No depot was given.');
      WriteLn('');
      WriteLn('Help:');
      WriteHelp;
      Terminate;
      Exit;
    end;
  finally
    { stop program loop }
    WriteLn('');
    WriteLn('Press enter ...');
    ReadLn;//only for testing, remove in productive environment
    Terminate;
  end;
end;

procedure TOpsiIconCollector.CollectIcons(DepotPath: String);
var
  IconCollector :TIconCollector;
begin
  WriteLn('Collecting icons for opsi client kiosk ...');
  WriteLn('Depot: ' + DepotPath);
  WriteLn('');
  IconCollector := TIconCollector.Create(DepotPath);
  //Write('Progress:[');
  Writeln('Search for opsi-script files:');
  Writeln('Startet at ' + TimeToStr(Time) + ' ... ');
  FProgressStatusFirstCall := True;
  IconCollector.FindOpsiScriptFiles(@ProgressStatus);


  //Writeln(#13'Done                                     ');
  Writeln('Finished at ' + TimeToStr(Time));
  WriteLn('');
  WriteLn('Paths to opsi-script files:');
  WriteLn(IconCollector.ShowOpsiScriptFilenames);
  Writeln('Extraction of icon paths:');
  Writeln('Startet at ' + TimeToStr(Time) + ' ... ');
  IconCollector.ExtractPathToIcon;
  Writeln('Finished at ' + TimeToStr(Time));
  WriteLn('');
  WriteLn('IconList:');
  WriteLn(IconCollector.ShowIconList);
  IconCollector.Free;
  //IconCollector.ExtractIconFromExe('C:\Users\Jan\Test\anydesk\AnyDesk.exe');
  Writeln('opsi-icon-collector finished at ' + TimeToStr(Time));
end;

procedure TOpsiIconCollector.ProgressStatus();
var
  TimeDiff : integer; //for testing/debugging

const
  StatusMessage = #13'Processing [%s] ';
  Progress: array [0..3] of char = ('-','\','|','/');
begin
  //TimeDiff := SecondsBetween(Time, StartTime); //for testing/debugging
  if FProgressStatusFirstCall then
  begin
    FProgressStatusStartTime := Time;
    FProgressStatusFirstCall := False;
  end
  else
  if MilliSecondsBetween(Time, FProgressStatusStartTime) > 100 then
  begin
    write(Format(StatusMessage,[Progress[FProgressStatusSymbol]]));
    FProgressStatusStartTime := Time;
    if FProgressStatusSymbol < 3 then inc(FProgressStatusSymbol)
      else FProgressStatusSymbol := 0;
  end;
end;

constructor TOpsiIconCollector.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FProgressStatusFirstCall := True;
  FProgressStatusSymbol := 0;
end;

destructor TOpsiIconCollector.Destroy;
begin
  inherited Destroy;
end;

procedure TOpsiIconCollector.WriteHelp;
begin
  { add your help code here }
  WriteLn('Searchs for product icons within the installed products on the given depot.');
  WriteLn('');
  WriteLn('Usage: opsi-icon-collector [DEPOT]');
  WriteLn('DEPOT is the path to the depot e.g. var/lib/opsi_depot');
  WriteLn('');
  WriteLn('Press enter ...');
  ReadLn;
end;

var
  Application: TOpsiIconCollector;

{$R *.res}

begin
  Application:=TOpsiIconCollector.Create(nil);
  Application.Title:='opsi-icon-collector';
  Application.Run;
  Application.Free;
end.

