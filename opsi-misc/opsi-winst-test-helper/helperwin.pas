unit helperwin;

{$mode objfpc}{$H+}

interface



uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, helpermain, VersionInfo, Process, windows,
  wispecfolder, DSiWin32;

type

  { TForm1 }

  TForm1 = class(TForm)
   BitBtn1: TBitBtn;
   Edit1: TEdit;
   Label1: TLabel;
   Label2: TLabel;
   Timer1: TTimer;
   waittimer: TTimer;
   procedure BitBtn1Click(Sender: TObject);
   procedure FormCreate(Sender: TObject);
   procedure Timer1Timer(Sender: TObject);
   procedure showwindow(seconds:integer);
  private
    { private declarations }
  public
    { public declarations }
  end;

procedure main;

var
  Form1: TForm1;
  ErrorMsg: String;
		optionlist : TStringlist;
		myexitcode : integer;
    myfile : File;
    mysizemb : integer =5;
    myfilename : string;
    myblock : array[1..1024] of byte;
    i, k : integer;
    mybyte : byte;
    mylog : textfile;
    mydefaultlog : string;

implementation


function IsElevated: Boolean;
const
  TokenElevation = TTokenInformationClass(20);
  //TokenElevation = TTokenInformationClass;

type
  TOKEN_ELEVATION = record
    TokenIsElevated: DWORD;
  end;
var
  TokenHandle: THandle;
  ResultLength: Cardinal;
  ATokenElevation: TOKEN_ELEVATION;
begin
  //if (IsWinVista or IsWinServer2008 or IsWin7 or IsWinServer2008R2) then
  begin
    Result := False;
    TokenHandle := 0;
    if OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, TokenHandle) then
    begin
      try
        ResultLength := 0;
        if GetTokenInformation(TokenHandle, TTokenInformationClass(20), @ATokenElevation, SizeOf(ATokenElevation), ResultLength) then
          Result := ATokenElevation.TokenIsElevated <> 0
        else
          Result := False;
      finally
        CloseHandle(TokenHandle);
      end;
    end
    else
      Result := False;
  end;
  //else
  //  Result := true; //IsAdministrator;
end;

(*
function IsElevated: Boolean;
  {Checks if process is running elevated.
    @return True if process is elevated, False if not elevated or not running on
      Vista or later.
  }
var
  Token: THandle;             // access token for current process
  Elevation: TTokenElevation; // structure receiving elevation state
  Size: DWORD;                // size of value from GetTokenInformation
begin
  Result := False;
  if not IsVistaOrLater then
    // Elevation has no meaning if not on Vista
    Exit;
  // On Vista: find elevation of process token
  if not OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, Token) then
    Exit;
  try
    if not GetTokenInformation(
      Token, TokenElevation, @Elevation, SizeOf(Elevation), Size
    ) then
      Exit;
    Result := Elevation.TokenIsElevated <> 0;
  finally
    CloseHandle(Token);
  end;
end;
*)

function GetNetUser (Host : String; Var UserName : String; var ErrorInfo : String) : Boolean;
  (* for Host = '' Username will become the name of the current user of the process *)

var
  pLocalName : PChar;
  pUserName  : PChar;


  function ApiCall (var Username, ErrorInfo : String; BuffSize : DWord) : Boolean;
   var
   errorcode : DWord;
   nBuffSize  : DWord;
   pErrorBuff, pNameBuff : PChar;
   nErrorBuffSize : DWord=0;
   nNameBuffSize : DWord=0;


  begin
    result := false;
    GetMem (pUserName, BuffSize);
    nBuffSize := Buffsize;

    UserName := '';
    errorCode := WNetGetUser(nil, pUserName, nBuffSize);


    case errorCode of
      no_error                  :
				Begin
         ErrorInfo := '';
         SetLength (UserName, StrLen (pUserName));
         UserName := pUserName;
         result := true;
        End;
      ERROR_NOT_CONNECTED	    : ErrorInfo :=
      'The device specified by lpszLocalName is not a redirected device or a connected network name.';
      ERROR_MORE_DATA	    : ApiCall (UserName, ErrorInfo, nBuffSize + 1);
      ERROR_NO_NETWORK	    : ErrorInfo := 'No network is present.';
      ERROR_EXTENDED_ERROR	    :
         Begin
           GetMem (pErrorBuff, 300);
           GetMem (pNameBuff, 300);
           WNetGetLastError (errorcode, pErrorBuff, nErrorBuffSize, pNameBuff, nNameBuffSize);
           ErrorInfo := pErrorBuff;
           FreeMem (pErrorBuff);
           FreeMem (pNameBuff);
         End;
      ERROR_NO_NET_OR_BAD_PATH   : ErrorInfo :=
         'None of the providers recognized this local name as having a connection. '
       + 'However, the network is not available for at least one provider to whom the connection may belong';
      else
      errorInfo := 'NT-Error ' + SysErrorMessage (errorCode);
    end;

    if errorCode <> no_error then errorInfo := IntToStr (errorCode) + ' ' + errorInfo;

    FreeMem (pUserName);
  end;


Begin
  if Host <> ''
  then pLocalName := PChar (Host)
  else pLocalName := nil;

  if ApiCall (Username, ErrorInfo, 100)
  then result := true
  else result := false;
End;



procedure WriteHelp;
var
			filename : string;
begin
  filename := ExtractFileName(paramstr(0));
  writeln(paramstr(0));
  writeln(filename);
  writeln('Usage:');
  writeln(filename+ ' Option [Option]');
  writeln('Options:');
  writeln(' --help -> write this help and exit');
  writeln(' --exit-code=n -> exits with n as exit code (default 42)');
  writeln(' --log=<path\filename> -> writes log to <path\filename> (c:\opsi.org\tmp\opsiwinsttesthelper.log)');
  writeln(' --time-output -> write timestamp and exit');
  writeln(' --fork-and-stop=n -> starts a helperchild.exe and exits.');
  writeln('                     The child process waits 2 seconds, then shows a window for n seconds and stops');
  writeln(' --showwindow=n -> shows a window for n seconds and exit');
  writeln(' --version -> write version info and exit');
  writeln(' --wait=n -> waits n seconds before going on');
  writeln(' --createfile=fname -> creates a file fname');
  writeln(' --filesize=n -> fills the file to the size of n MB');
end;

procedure writeTimestamp;
begin
	writeln(DateTimeToStr(now));
end;

procedure TForm1.showwindow(seconds:integer);
begin
	//form1 = TForm1.Create(Application);
	//Application.CreateForm(TForm1, Form1);
	form1.Visible:= true;
	form1.timer1.Interval:=seconds*1000;
	form1.timer1.Enabled:=true;
	//form1.Show;
end;


procedure writeversioninfo;
var
i : Integer;
versionInfo : TVersionInfo;
begin
	versionInfo := TVersionInfo.create (paramstr(0));
	writeln(ExtractFileName(paramstr(0))+' version ' + versionInfo.GetFileVersionWithDots);

//	writeln('file version=' + IntToStr(versionInfo.FileVersion));
//	writeln('file version with dots=' + versionInfo.GetFileVersionWithDots);
//	writeln('product version=' + IntToStr(versionInfo.ProductVersion));

(*
	for i := 0 to versionInfo.TranslationCount-1 do
	Begin
		list.add('Language name ' + IntToStr(i) + '=' + versionInfo.LanguageNames[i]);
		list.add('Language ID ' + IntToStr(i) + '=' + IntToStr(versionInfo.LanguageID[i]));
	End;

	list.add('file version=' + IntToStr(versionInfo.FileVersion));
	list.add('file version with dots=' + versionInfo.GetFileVersionWithDots);
	list.add('product version=' + IntToStr(versionInfo.ProductVersion));

		for i := Low(versionInfoX.PredefinedStrings) to High(versionInfoX.PredefinedStrings) do
		list.add (versionInfoX.PredefinedStrings[i] + '=' + versionInfo.getString(PredefinedStrings[i]) );
*)
		versionInfo.free;
end;

procedure startchild(childsec: integer);
var
AProcess : TProcess;
begin
 AProcess := TProcess.Create(nil);
 AProcess.CommandLine := '"'+ExtractFilePath(paramstr(0))+'helperchild.exe" --wait=2 --showwindow='+IntToStr(childsec);
 //AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
 try
  AProcess.Execute;
 except
   writeln('Error starting: '+AProcess.CommandLine);
 end;
end;

procedure main;
var
paramvaluestr,mystr,dummystr : string;
waitsec, childsec : integer;

begin
  myexitcode := 42;
  optionlist := TStringlist.Create;
  optionlist.Append('help');
  optionlist.Append('log::');
  optionlist.Append('exit-code::');
  optionlist.Append('time-output');
  optionlist.Append('fork-and-stop::');
  optionlist.Append('showwindow::');
  optionlist.Append('version');
  optionlist.Append('wait:');
  optionlist.Append('createfile::');
  optionlist.Append('filesize:');
  // quick check parameters
  ErrorMsg:= Application.CheckOptions('',optionlist);
  if ErrorMsg<>'' then begin
    Application.ShowException(Exception.Create(ErrorMsg));
    Application.Terminate;
    Exit;
  end;

  // parse parameters
  if Application.HasOption('help') or (paramcount=0) then begin
  //if Application.HasOption('help')  then begin
    WriteHelp;
    halt(myexitcode);
  end;

  if Application.HasOption('exit-code') then
  begin
 	 	paramvaluestr := Application.GetOptionValue('exit-code');
  	try
				myexitcode := StrToInt(paramvaluestr);
				writeln('Will use '+paramvaluestr+' as exit code.');
	  except
  		writeln('>'+paramvaluestr+'< is not a integer. Using default of '+InttoStr(myexitcode)+' as exit code.');
  	end;
  end;

  if Application.HasOption('log') then
  begin
    mydefaultlog := 'c:\opsi.org\tmp\opsiwinsttesthelper.log';
 	 	paramvaluestr := Application.GetOptionValue('log');
  	try
      assign(mylog,paramvaluestr);
      rewrite(mylog);
				writeln('Will use '+paramvaluestr+' as log file.');
	  except
  		writeln('>'+paramvaluestr+'< is not a filename. Using default of '+mydefaultlog+' as logfile.');
      try
        assign(mylog,mydefaultlog);
        rewrite(mylog);
				  writeln('Will use '+mydefaultlog+' as log file.');
	  except
      writeln('Failed to create log file.');
          halt(myexitcode);
    Application.Terminate;
  	end;
    end;
    GetNetUser('',mystr,dummystr);
    writeln(mylog,'Running as = '+mystr);
    if DSiIsAdmin then writeln(mylog,'Running as admin = true')
    else writeln(mylog,'Running as admin = false');
    if GetNTVersionMajor = 6 then
    begin
      writeln(mylog,'Running on = nt6');
      if IsElevated then writeln(mylog,'Running elevated on nt6 = true')
      else writeln(mylog,'Running elevated on nt6 = false');
    end
    else writeln(mylog,'Running on = nt5');
    retrieveFoldersFromWinApi;
    writeln(mylog,'Appdata is = '+GetAppDataPath);
    writeln(mylog,'Desktop is = '+GetDesktopPath);
    writeln(mylog,'current profile is = '+GetUserProfilePath);
    closefile(mylog);
  end;


  if Application.HasOption('wait') then
  begin
    	paramvaluestr := Application.GetOptionValue('wait');
   try
    waitsec := StrToInt(paramvaluestr);
    Sleep(waitsec*1000);
   except
    writeln('>'+paramvaluestr+'< is not a integer.');
   end;
  end;

  if Application.HasOption('fork-and-stop') then
  begin
     paramvaluestr := Application.GetOptionValue('fork-and-stop');
   try
    childsec := StrToInt(paramvaluestr);
    startchild(childsec);
   except
    writeln('>'+paramvaluestr+'< is not a integer.');
    startchild(5);
   end;
    halt(myexitcode);
    Application.Terminate;
  end;


  if Application.HasOption('time-output') then
  begin
    writeTimestamp;
    halt(myexitcode);
    Application.Terminate;
  end;

  if Application.HasOption('version') then
  begin
    writeversioninfo;
    halt(myexitcode);
    Application.Terminate;
  end;

  if Application.HasOption('createfile') then
  begin
 	 	myfilename := Application.GetOptionValue('createfile');
    writeln('Will use '+myfilename+' as file name.');
    if Application.HasOption('filesize') then
    begin
 	 	  paramvaluestr := Application.GetOptionValue('filesize');
  	  try
				  mysizemb := StrToInt(paramvaluestr);
				  writeln('Will use '+paramvaluestr+' as file size.');
	    except
  		  writeln('>'+paramvaluestr+'< is not a integer. Using default of '+InttoStr(mysizemb)+' as file size.');
  	  end;
      k := 0;
      for i := 1 to 4 do
      begin
        for mybyte := 0 to 255 do
        begin
          k := k+1;
          myblock[k] := mybyte ;
        end;
      end;
      assign(myfile,myfilename);
      rewrite(myfile,1024);
      for i := 1 to mysizemb * 1024 do blockwrite(myfile, myblock,1);
      closefile(myfile);
    end;
    halt(myexitcode);
    Application.Terminate;
  end;

  if Application.HasOption('wait') then
  begin
    	paramvaluestr := Application.GetOptionValue('wait');
   try
    waitsec := StrToInt(paramvaluestr);
    Sleep(waitsec*1000);
   except
    writeln('>'+paramvaluestr+'< is not a integer.');
   end;
  end;



  { add your program here }

  // stop program loop
  //Application.Terminate;
  if not(Application.HasOption('showwindow')) then
  begin
    halt(myexitcode);
    Application.Terminate;
  end;

end;


{ TForm1 }

procedure TForm1.Timer1Timer(Sender: TObject);
begin
	Timer1.Enabled:=false;
	form1.Visible:=false;
	halt(myexitcode);
	Application.Terminate;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
showtimestr : string;
showtimeint : integer;
begin
  if Application.HasOption('showwindow') then
  begin
  	showtimestr := Application.GetOptionValue('showwindow');
  	try
				showtimeint := StrToInt(showtimestr);
	  except
  		writeln('<'+showtimestr+'< is not a integer. Using default of 1 second.');
  		showtimeint := 1;
  	end;
    form1.showwindow(showtimeint);
  end;
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
	Timer1.Enabled:=false;
	form1.Visible:=false;
	halt(myexitcode);
	Application.Terminate;
end;

initialization
 // {$I helperwin.lrs}



end.

