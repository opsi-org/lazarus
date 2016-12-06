unit xmlpluginlog;

interface

uses
classes,
filectrl,
registry,
WinTypes,
IdSysLogMessage,
Sysutils, Controls , stdctrls , forms
  ;

type
  TRemoteErrorLogging = (trel_none, trel_filesystem, trel_syslog);
     // in registry values 0, 1, 2

  TLogInfo = class (TObject)
  private
    FLogLevel : Integer;
    FLogSIndentLevel : Integer;
    FNumberOfWarnings : Integer;
    FNumberOfErrors : Integer;
    FNumberOfHints : Integer;
    FErrorNumberMarked : Integer;
    FFileName : String;
    FErrorsFilename : String;
    FErrorsFileExists : boolean;
    FRemoteErrorsPath : String;
    FRemoteErrorsFilename : String;
    FRemoteErrorsFileExists : boolean;
    FReportErrorsToUser : boolean;
    FTraceMode : Boolean;
    FLogFileExists : Boolean;
		FRemoteFileLogging : String;
		FWinstVersionRequired : String;
		FLogcallcount : integer;
    

  protected
    LogFile : Text;
    ErrorFile : Text;
    RemoteErrorFile : Text;


  public
    constructor create;
    destructor destroy; override;
    procedure initiate (const Fname : String; Trace: Boolean);
    procedure Empty;
    procedure Reopen;
    procedure Close;
    class procedure DumpToLog (const S: string);
    procedure OpenForReading;
    function getLine (var S : String) : boolean;
    function DependentAdd (const S: string; LevelOfLine : Integer): Boolean;
    function DependentAddError (const S: string; LevelOfLine : Integer): Boolean;
    function DependentAddWarning (const S: string; LevelOfLine : Integer): Boolean;
    function DependentAddStringList (const list: TStrings; LevelOfLine : Integer): Boolean;
    procedure setLogSIndentLevel (const value : Integer);

    procedure SetNumberOfErrors (Number : Integer);
    property LogLevel : Integer read FLogLevel write FLogLevel;
    property NumberOfWarnings : Integer read FNumberOfWarnings write FNumberOfWarnings;
    property NumberOfErrors : Integer read FNumberOfErrors write SetNumberOfErrors;
    property NumberOfHints : Integer read FNumberOfHints write FNumberOfHints;
    property ErrorNumberMarked : Integer read FErrorNumberMarked write FErrorNumberMarked;

    property ReportErrorsToUser : boolean read FReportErrorsToUser write FReportErrorsToUser;
    property FileName : String read FFileName write FFileName;
    property TraceMode : Boolean read FTraceMode write FTraceMode;
    property RemoteFileLogging : String read FRemoteFileLogging;
    property LogFileExists : Boolean read FLogFileExists write FLogFileExists;

    property LogSIndentLevel : Integer read FLogSIndentLevel write setLogSIndentLevel;
    function LogSIndent : String;
    function LogSIndentPlus (const n : Integer) : String;
		property WinstVersionRequired : String read FWinstVersionRequired write FWinstVersionRequired;
  end;



type
  TErrorLevel = Integer;

const
(* old Loglevels before opsi 3.5
	LevelFatal    = -2; //not yet used;
	BaseLevel     = -1;
	LevelWarnings     =  0;
	LevelInfo  = 1;
	LevelComplete = 2;
	LevelDebug    = 3;
	Level_not_initialized = 9;
*)

(* new Loglevels since opsi 3.5:
0 = nothing (absolute nothing)
1 = essential ("unverzichtbares")
2 = critical (unexpected errors that my cause a program abort)
3 = error (Errors that don't will abort the running program)
4 = warning (you should have a look at this)
5 = notice (Important statements to the program flow)
6 = info (Additional Infos)
7 = debug (important debug messages)
8 = debug2 (a lot more debug informations and data)
9 = confidential (passwords and other security relevant data)
*)
	LLnothing 			= 0;
	LLessential 		= 1;
	LLcritical 			= 2;
	LLerror 				= 3;
	LLwarning 			= 4;
	LLnotice 				= 5;
	LLinfo 					= 6;
	LLdebug 				= 7;
	LLdebug2 				= 8;
	LLconfidential	= 9;




	LevelFatal    = 1; //not yet used;
	BaseLevel     = LLnotice;
	LevelError    = LLerror;
	LevelWarnings     = LLnotice;
	LevelInfo  = LLinfo;
	LevelComplete = LLinfo;
	LevelDebug    = LLdebug;
	Level_not_initialized = 9;

  passwordFiller = '*****(password)*****';


  // facility
  ID_SYSLOG_FACILITY_KERNEL     = 0;  // kernel messages
  ID_SYSLOG_FACILITY_USER       = 1;  // user-level messages
  ID_SYSLOG_FACILITY_MAIL       = 2;  // mail system
  ID_SYSLOG_FACILITY_SYS_DAEMON = 3;  // system daemons
  ID_SYSLOG_FACILITY_SECURITY1  = 4;  // security/authorization messages (1)
  ID_SYSLOG_FACILITY_INTERNAL   = 5;  // messages generated internally by syslogd
	ID_SYSLOG_FACILITY_LPR        = 6;  // line printer subsystem
	ID_SYSLOG_FACILITY_NNTP       = 7;  // network news subsystem
	ID_SYSLOG_FACILITY_UUCP       = 8;  // UUCP subsystem
	ID_SYSLOG_FACILITY_CLOCK1     = 9;  // clock daemon (1)
  ID_SYSLOG_FACILITY_SECURITY2  = 10; // security/authorization messages (2)
  ID_SYSLOG_FACILITY_FTP        = 11; // FTP daemon
  ID_SYSLOG_FACILITY_NTP        = 12; // NTP subsystem
  ID_SYSLOG_FACILITY_AUDIT      = 13; // log audit
  ID_SYSLOG_FACILITY_ALERT      = 14; // log alert
  ID_SYSLOG_FACILITY_CLOCK2     = 15; // clock daemon (2)
  ID_SYSLOG_FACILITY_LOCAL0     = 16; // local use 0  (local0)
  ID_SYSLOG_FACILITY_LOCAL1     = 17; // local use 1  (local1)
  ID_SYSLOG_FACILITY_LOCAL2     = 18; // local use 2  (local2)
  ID_SYSLOG_FACILITY_LOCAL3     = 19; // local use 3  (local3)
  ID_SYSLOG_FACILITY_LOCAL4     = 20; // local use 4  (local4)
  ID_SYSLOG_FACILITY_LOCAL5     = 21; // local use 5  (local5)
  ID_SYSLOG_FACILITY_LOCAL6     = 22; // local use 6  (local6)
  ID_SYSLOG_FACILITY_LOCAL7     = 23; // local use 7  (local7)

var
	LogDatei : TLogInfo;
	remoteErrorLogging : TRemoteErrorLogging;
		lastPeakLen : Integer = 0;
		  peakIndicator :  String = '';
  peakIndicatorLength  :  Integer = 100;
	peakIndicatorChar : Char = '|';



const
  syslogHost : String = 'localhost';
  syslogfacility : Longint = ID_SYSLOG_FACILITY_LOCAL2;



function getComputerName : string;


implementation



type
 TLogCallbackProcedure  = procedure (level:integer; logmsg:string) of object;


var
  syslogmessagetag : String;
  externLogProc : TLogCallbackProcedure;
  j : Integer;


procedure setLogProcedure(procName : TLogCallbackProcedure);
begin
 externLogProc := procName;
end;



function internalLogLevel_To_SyslogSeverity (loglevel : Integer) : TIdSyslogSeverity;
// we dont try to map the winst log level since the winst levels have a different
// intention
begin
  result := slInformational;
  {
  case loglevel of
   -2 : result := slCritical;
   -1 : result := slError;
   0  : result := slWarning;
   1  : result := slNotice;
   2  : result := slInformational;
   3  : result := slDebug
   end;

  if loglevel < -2 then result := slCritical;
  if loglevel > 3 then result := slDebug;
  }
end;


 (* copied from unit for documentation reasons
//  Copyright the Indy pit crew
//  Original Author: Stephane Grobety (grobety@fulgan.com)
//  Release history:
//  25/2/02; - Stephane Grobety
//    - Moved Facility and Severity translation functions out of the class
//    - Restored the "SendToHost" method
//    - Changed the ASCII check tzo include only the PRI and HEADER part.
//    - Now allow nul chars in message result (Special handeling should be required, though)
//  09/20/01;  - J. Peter Mugaas
//    Added more properties dealing with Msg parts of the SysLog Message
//  09/19/01; - J. Peter Mugaas
//     restructured syslog classes
//  08/09/01: Dev started

 TIdSyslogFacility = (sfKernel, { ID_SYSLOG_FACILITY_KERNEL}
                      sfUserLevel, { ID_SYSLOG_FACILITY_USER }
                      sfMailSystem, { ID_SYSLOG_FACILITY_MAIL }
                      sfSystemDaemon, { ID_SYSLOG_FACILITY_SYS_DAEMON }
                      sfSecurityOne, { ID_SYSLOG_FACILITY_SECURITY1 }
                      sfSysLogInternal, { ID_SYSLOG_FACILITY_INTERNAL }
                      sfLPR, {ID_SYSLOG_FACILITY_LPR}
                      sfNNTP, { ID_SYSLOG_FACILITY_NNTP }
                      sfClockDaemonOne, { CILITY_CLOCK1 }
                      sfUUCP, { ID_SYSLOG_FACILITY_UUCP }
                      sfSecurityTwo, { ID_SYSLOG_FACILITY_SECURITY2 }
                      sfFTPDaemon, { ID_SYSLOG_FACILITY_FTP }
                      sfNTP, { ID_SYSLOG_FACILITY_NTP }
                      sfLogAudit, { ID_SYSLOG_FACILITY_AUDIT  }
                      sfLogAlert, { ID_SYSLOG_FACILITY_ALERT }
                      sfClockDaemonTwo, { ID_SYSLOG_FACILITY_CLOCK2 }
                      sfLocalUseZero, { ID_SYSLOG_FACILITY_LOCAL0 }
                      sfLocalUseOne, { ID_SYSLOG_FACILITY_LOCAL1 }
                      sfLocalUseTwo, { ID_SYSLOG_FACILITY_LOCAL2 }
                      sfLocalUseThree, { ID_SYSLOG_FACILITY_LOCAL3 }
                      sfLocalUseFour, { ID_SYSLOG_FACILITY_LOCAL4 }
                      sfLocalUseFive, { ID_SYSLOG_FACILITY_LOCAL5 }
                      sfLocalUseSix, { ID_SYSLOG_FACILITY_LOCAL6 }
                      sfLocalUseSeven); { ID_SYSLOG_FACILITY_LOCAL7  }


 const
  // facility
  ID_SYSLOG_FACILITY_KERNEL     = 0;  // kernel messages
  ID_SYSLOG_FACILITY_USER       = 1;  // user-level messages
  ID_SYSLOG_FACILITY_MAIL       = 2;  // mail system
  ID_SYSLOG_FACILITY_SYS_DAEMON = 3;  // system daemons
  ID_SYSLOG_FACILITY_SECURITY1  = 4;  // security/authorization messages (1)
  ID_SYSLOG_FACILITY_INTERNAL   = 5;  // messages generated internally by syslogd
  ID_SYSLOG_FACILITY_LPR        = 6;  // line printer subsystem
  ID_SYSLOG_FACILITY_NNTP       = 7;  // network news subsystem
  ID_SYSLOG_FACILITY_UUCP       = 8;  // UUCP subsystem
  ID_SYSLOG_FACILITY_CLOCK1     = 9;  // clock daemon (1)
  ID_SYSLOG_FACILITY_SECURITY2  = 10; // security/authorization messages (2)
  ID_SYSLOG_FACILITY_FTP        = 11; // FTP daemon
  ID_SYSLOG_FACILITY_NTP        = 12; // NTP subsystem
  ID_SYSLOG_FACILITY_AUDIT      = 13; // log audit
  ID_SYSLOG_FACILITY_ALERT      = 14; // log alert
  ID_SYSLOG_FACILITY_CLOCK2     = 15; // clock daemon (2)
  ID_SYSLOG_FACILITY_LOCAL0     = 16; // local use 0  (local0)
  ID_SYSLOG_FACILITY_LOCAL1     = 17; // local use 1  (local1)
  ID_SYSLOG_FACILITY_LOCAL2     = 18; // local use 2  (local2)
  ID_SYSLOG_FACILITY_LOCAL3     = 19; // local use 3  (local3)
  ID_SYSLOG_FACILITY_LOCAL4     = 20; // local use 4  (local4)
  ID_SYSLOG_FACILITY_LOCAL5     = 21; // local use 5  (local5)
  ID_SYSLOG_FACILITY_LOCAL6     = 22; // local use 6  (local6)
  ID_SYSLOG_FACILITY_LOCAL7     = 23; // local use 7  (local7)

  // Severity
  ID_SYSLOG_SEVERITY_EMERGENCY     = 0; // Emergency: system is unusable
  ID_SYSLOG_SEVERITY_ALERT         = 1; // Alert: action must be taken immediately
  ID_SYSLOG_SEVERITY_CRITICAL      = 2; // Critical: critical conditions
  ID_SYSLOG_SEVERITY_ERROR         = 3; // Error: error conditions
  ID_SYSLOG_SEVERITY_WARNING       = 4; // Warning: warning conditions
  ID_SYSLOG_SEVERITY_NOTICE        = 5; // Notice: normal but significant condition
  ID_SYSLOG_SEVERITY_INFORMATIONAL = 6; // Informational: informational messages
  ID_SYSLOG_SEVERITY_DEBUG         = 7; // Debug: debug-level messages

 *)




(* TLogInfo *)

constructor TLogInfo.create;
begin
	inherited create;
	LogFileExists := false;
	remoteErrorLogging := trel_none;

end;


procedure informTheAdministrator (info : String);
Var
  ps : String;
begin
(*
  ps := info + '! '+ #13#10
        + 'Please inform the Administrator!';

  MyMessageDlg.WiMessage(ps, [mrOK]);
  *)
end;



//constructor TLogInfo.create (const Fname : String; Trace: Boolean);
procedure TLogInfo.initiate (const Fname : String; Trace: Boolean);
 var
  ps : String;
  YetATrial : Boolean;
  LogFileNo : Integer;
  Ext : String;

begin
  NumberOfErrors := 0;
  NumberOfWarnings := 0;
  NumberOfHints := 0;
	ErrorNumberMarked := 0;
	LogLevel := LLinfo;
	FLogcallcount := 0;

  LogSIndentLevel := 0;

  LogFileExists := false;

  TraceMode := Trace;
  FFilename := Fname;
  FErrorsFilename := ChangeFileExt (Fname, '.err');

  FErrorsFileExists := (Fname <> FErrorsFilename);
	FRemoteErrorsFileExists := false;
	//Centralform.SpeedButton4Click(nil);


  {
  case remoteErrorLogging of
     trel_filesystem : MyMessageDlg.WiMessage ('1: remote error logging on filesystem', [mrOK]);
     trel_syslog : MyMessageDlg.WiMessage ('1: remote error logging to syslogd', [mrOK]);
     else MyMessageDlg.WiMessage ('1: no remote error logging', [mrOK]);
    end;
  }

  if remoteerrorlogging = trel_filesystem
  then
  Begin
    remoteerrorlogging := trel_none; //check if we can reassign trel_filesystem
    //FRemoteErrorsPath := Profildateienverzeichnis + '\pclog\';
    //FRemoteErrorsFilename := FRemoteErrorsPath + ValueOfEnvVar ('Computername') + '.err';

    FRemoteFileLogging := 'No access to remote logging file "' + FRemoteErrorsFilename + '"';
    forcedirectories (FRemoteErrorsPath);
    if directoryexists (FRemoteErrorsPath)
      then
      try
        assignfile (RemoteErrorFile, FRemoteErrorsFilename);
        if not fileexists (FRemoteErrorsFilename)
        then
          rewrite (RemoteErrorFile);
        append (RemoteErrorFile);
        writeln (RemoteErrorFile);
        FRemoteFileLogging := 'Use remote logging file "' + FRemoteErrorsFilename + '"';
        FRemoteErrorsFileExists := true;
        remoteerrorlogging := trel_filesystem;
      except

        on E: Exception do
        Begin

           //MyMessageDlg.WiMessage ('Exception when trying remote error logging on filesystem', [mrOK]);
           {
              ps := 'Could not open remote logfile. Exception "'
                    + E.Message + '"';
              DependentAdd (ps, BaseLevel);
           }
        End;

    End;


    {
    case remoteErrorLogging of
     trel_filesystem : MyMessageDlg.WiMessage ('2: remote error logging on filesystem', [mrOK]);
     trel_syslog : MyMessageDlg.WiMessage ('2: remote error logging to syslogd', [mrOK]);
     else MyMessageDlg.WiMessage ('2: no remote error logging', [mrOK]);
    end;
    }


  End;

  LogFileExists := false;
  YetATrial := true;

  while YetATrial do
  Begin
    assignfile (LogFile, FileName);
    if FErrorsFileExists
    then assignfile (Errorfile, FErrorsFilename);

    ps := '';
    if fileExists (FileName)
    then
      try
        append (LogFile);
        LogFileExists := true;
      except
        on E: Exception do
            ps := 'Existing file "' + Filename + '" could not be opened as logfile. Exception "'
                  + E.Message + '"';
      end
    else
    Begin
      if FileName = ''
      then
        ps := 'An empty string does not work as logfilename.'
      else
        try
         rewrite (LogFile);
         LogFileExists := true;
        except
          on E: Exception do
              ps := '"' + Filename + '" could not be created as logfile. Exception "'
                    + E.Message + '"';
        end
    End;

    if FErrorsFileExists
    then
    Begin
      ps := '';
      if fileExists (FErrorsFileName)
      then
        try
          append (ErrorFile);
        except
          on E: Exception do
          Begin
              ps := 'Existing file "' + FErrorsFilename
                  + '" could not be opened as error-logfile. Exception "'
                    + E.Message + '"';
              FErrorsFileExists := false;
          End
        end
      else
      Begin
        if FErrorsFileName = ''
        then
        Begin
          ps := 'An empty string does not work as logfilename.';
          FErrorsFileExists := false;
        End
        else
          try
           rewrite (ErrorFile);
          except
            on E: Exception do
            Begin
                ps := '"' + Filename + '" could not be created as logfile. Exception "'
                      + E.Message + '"';
                FErrorsFileExists := false;
            End;
          end
      End;
    End;

    if LogFileExists
    then YetATrial := false
    else
    begin
      Ext := ExtractFileExt (Filename);
      System.delete (Ext, 1, 1);
      try
        LogfileNo := StrToInt (Ext);
        inc (LogFileNo);
      except
        LogFileNo := 0;
      end;

      Filename := copy (Filename, 1, pos ('.', Filename)) + IntToStr (LogFileNo);
      ps := ps + #13#10
            + 'use instead "' + Filename + '" ?' + #13#10
            + 'OK | Cancel (install without logging) | Exit program ';
     (*
      case MyMessageDlg.WiMessage(ps, [mrOK, mrCancel, mrAbort]) of
       mrOK     : ;
       mrCancel : begin YetATrial := false; informTheAdministrator('No logging'); end;
       mrAbort  : begin informTheAdministrator('Exit program'); Halt; end;
      end;
      *)
    end;
  End;

  FReportErrorsToUser := false;

  DependentAdd ('--',BaseLevel);
  DependentAdd ('--',BaseLevel);


end;

destructor TLogInfo.destroy;
begin
  //if LogFileExists then system.closefile (LogFile);
  if LogFileExists then system.close(LogFile);
  //if FErrorsFileExists then system.closefile (ErrorFile);
  if FErrorsFileExists then system.close(ErrorFile);
  if (RemoteErrorLogging = trel_filesystem) and FRemoteErrorsFileExists then
  Begin
   //system.closefile (RemoteErrorFile);
   system.close(RemoteErrorFile);

  End;
  inherited destroy;

end;




procedure TLogInfo.Reopen;
begin
  if LogFileExists
  then
    try
      append (LogFile);
		except
			LogFileExists := false;
    end;
  if FErrorsFileExists
  then
    try
      append (ErrorFile);
    except
      FErrorsFileExists := false;
    end;

  if RemoteErrorLogging = trel_filesystem
  then
    try
      append (RemoteErrorFile);
    except
      RemoteErrorLogging := trel_none;
    end
end;

procedure TLogInfo.OpenForReading;
begin
  if LogFileExists
  then
    try
      flush(LogFile);
      sleep(1000);
      reset (LogFile);
    except
      LogFileExists := false;
    end;
end;


procedure TLogInfo.Close;
begin
 if LogFileExists
 //then system.closefile (LogFile);
 then system.close(LogFile);
 if FErrorsFileExists
 //then system.closefile (ErrorFile);
 then system.close(ErrorFile);
 if RemoteErrorLogging = trel_filesystem then
   //system.closefile (RemoteErrorFile);
   system.close(RemoteErrorFile);
end;


procedure TLogInfo.Empty;
begin
  if LogFileExists then
  Begin
    //system.closefile (LogFile);
    system.close(LogFile);
    rewrite (LogFile);
  End;
end;


function TLogInfo.LogSIndentPlus (const n : Integer): String;
var i : Integer;
begin
  result := '';
  for i := 1 to n
  do
    Result := Result + '  '
End;

function TLogInfo.LogSIndent : String;
begin
 result := LogSIndentPlus (LogSIndentLevel);
end;


function TLogInfo.DependentAddError (const S: string; LevelOfLine : Integer): Boolean;
begin
	if (LogLevel < LevelOfLine)
  then result := false
  else
  Begin
      NumberOfErrors := NumberOfErrors + 1;
			//DependentAdd (s, LevelOfLine);
			DependentAdd (s, LLerror);
  End;

end;

function TLogInfo.DependentAddWarning (const S: string; LevelOfLine : Integer): Boolean;
begin
  if (LogLevel < LevelOfLine)
  then result := false
	else
  Begin
      NumberOfWarnings := NumberOfWarnings + 1;
			//DependentAdd (s, LevelOfLine);
			DependentAdd (s, LLwarning);
  End;
end;

class procedure TLogInfo.DumpToLog(const S: string);
 var
  st : String;
  wHandle : THandle;

 const
  windowName = 'winst debuginfo';
begin

  //st := StringReplace (s, opsiservicePassword, passwordFiller, [rfReplaceAll]);
  //messagebox (0, pchar(st), PChar(windowName), MB_OK);

end;

function TLogInfo.getLine (var S : String): boolean;
begin
  if logfileExists and not eof(LogFile)
  then
  Begin
    readln(Logfile, s);
    result := true;
  end
  else
    result := false;
end;


function TLogInfo.DependentAdd (const S: string; LevelOfLine : Integer): Boolean;
 var
  PasS  : String;
  st : String;
	peaklen : Integer;
	usedloglevel : integer;
	intresult : integer;
	errorinfo : string;
	dlgresult : TModalresult;
begin
	usedloglevel := loglevel;
(*
	// if not winst version >= 4.10.3 required loglevel := loglevel+4 is used for backward compatibility
	if getDecimalCompareSign(WinstVersionRequired, '4.10.3', intresult, errorinfo, false) then
		case intresult of
			-1 : usedloglevel := loglevel + 4; // < 4.10.3 required
			 0 : usedloglevel := loglevel + 4;
			 1 : usedloglevel := loglevel; // >= 4.10.3 required
		end
	else
		// no Version required
		usedloglevel := loglevel + 4;
*)
  //st := StringReplace (s, opsiservicePassword, passwordFiller, [rfReplaceAll]);
  st := s;

  //show activity
	 //FBatchOberflaeche.LabelProgress.Caption := st;

	begin
		if length(st) > peakindicatorlength
		 then peaklen := peakindicatorlength
		 else peaklen := length(st);

		peaklen := (lastpeaklen + peaklen) div 2;
		lastpeaklen := peaklen;

	//	if  FBatchOberflaeche <> nil //dont log before creating FBatchOberflaeche
	//	then FBatchOberflaeche .setActivityLabel(copy (peakindicator, 1, peaklen));

	//FBatchOberflaeche.Refresh;
	end;


	if (usedloglevel >= LevelOfLine) and LogFileExists
  then
  begin

		PasS := '['+intToStr(LevelOfLine)+'] ['+DateTimeToStr(Now)+'] '+ LogSIndent + st;

		try
		//	CentralForm.Memo1Add (PasS);
    except
			try
				Writeln (LogFile, '!!!Warning: memory problem, the following line could not be inserted into the memo field');
      except
      end;
      NumberOfWarnings := NumberOfWarnings + 1;
    end;

		try
			WriteLn (LogFile, PasS);
			flush (LogFile);
		except
			try
				LogDatei.Reopen;
				WriteLn (LogFile, PasS);
				flush (LogFile);
			except
	(*			if
				 MyMessageDlg.WiMessage
					 ('Logfile ' + Filename + ' not available.  Continue without logging? ', [mrYES, mrNO])
						= mrNO
				then halt
				else LogFileExists := false; *)
			end;
    end;


		if FErrorsFileExists and (LevelOfLine <= LLwarning)
    then
      try
        WriteLN (ErrorFile, PasS);
        flush (ErrorFile);
			except
				FErrorsFileExists := false
      end;


		if (RemoteErrorLogging = trel_filesystem) and (LevelOfLine <= LLwarning)
    then
      try
        WriteLN (RemoteErrorFile, PasS);
        flush (RemoteErrorFile);
			except
        RemoteErrorLogging := trel_none;

        if LogFileExists then
        begin
          WriteLn (LogFile, 'Exception when trying remote error logging on file system');
          flush (LogFile);
        end;
        DependentAdd ('Exception when trying remote error logging on file system', baselevel);
      //  MyMessageDlg.WiMessage ('Exception when trying remote error logging on file system', [mrOK]);
      end

		else if (RemoteErrorLogging = trel_syslog) and (LevelOfLine <= LLwarning)
    then
    begin

			try
      (*   CentralForm.IdSysLog1.SendMessage
          (
          syslogmessageTag +
          PasS,
          NoToFacility(syslogfacility),
          //sfLocalUseThree,
          internalLogLevel_To_SyslogSeverity(levelOfLine));    *)

      except
        RemoteErrorLogging := trel_none;
        try
          if LogFileExists then
          begin
            WriteLn (LogFile, 'Exception when trying remote error logging to syslogd');
            flush (LogFile);
          end;
        //  MyMessageDlg.WiMessage ('Exception when trying remote error logging to syslogd', [mrOK]);
        except
        end
      end;
    end;

    Result := true;

  end
  else
    Result := false;

  if (TraceMode) and (LogLevel >= LevelOfLine)
  //if (not LogFileExists or TraceMode) and (LogLevel >= LevelOfLine)
  then
	Begin
	//	dlgresult := MyMessageDlg.WiMessage ('TRACE:' + #10#13 + St, [mrOK,mrAbort]);
		if dlgresult = mrAbort then TraceMode := false;
    Result := true;
  End;


end;

function TLogInfo.DependentAddStringList (const list: TStrings; LevelOfLine : Integer): Boolean;
 var i : Integer;
begin
 i := 0;
 result := true;
 while (i < list.count) and result
 do
 begin
   result := dependentAdd (format ('(string %3d)', [i]) + list[i], levelOfLine);
   inc(i);
 end
end;

procedure TLogInfo.SetLogSIndentLevel (const value : Integer);
begin
 FLogSIndentLevel := value;
 (* debug
   MyMessageDlg.WiMessage ('winst-Debug' + #13#10 + 'Setze IndentLevel ' + IntToStr (value), [mrOK]);
 *)
end;

procedure TLogInfo.SetNumberOfErrors (Number : Integer);
begin
  FNumberOfErrors := Number;
  (* debug
   MyMessageDlg.WiMessage ('winst-Debug' + #13#10 + 'Setze Fehlerzahl ' + IntToStr (Number), [mrOK]);
  *)
end;


function getComputerName : string;
 Var
	 regist : TRegistry;
 begin

	result := '';
		try
		regist := TRegistry.Create;
		regist.RootKey := HKEY_LOCAL_MACHINE;

		if regist.openKey ('\SYSTEM\CurrentControlSet\Control\ComputerName\ComputerName', false)
		then
			result := regist.ReadString('ComputerName');

		regist.CloseKey;
		regist.free;

	except
	end;

//	if result = ''
//	then result := ValueOfEnvVar ('Computername');
 end;


begin
		syslogmessagetag := ' --- winst32 on ' + getcomputername + ' -- ';

		for j := 1 to peakIndicatorLength
		do
			peakIndicator := peakIndicator + peakIndicatorChar;

end.

