unit helperwin;

{$mode objfpc}{$H+}

interface

{$Define GUI}

uses
    //Classes,
  SysUtils, FileUtil,
  {$IFDEF GUI}
  LResources,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  Buttons,
  opsiscripttesthelper_main,
  {$ENDIF GUI}
  //helpermain,
  //VersionInfo,

  {$IFDEF WINDOWS}
  Windows,
  helperwispecfolder,
  DSiWin32,
  //winpeimagereader,
  {$ENDIF WINDOWS}
  {$IFDEF UNIX}
  //elfreader,


  {$ENDIF UNIX}
  {$IFDEF LINUX}
  elfreader,
  lispecfolder,
  osfunclin,


  {$ENDIF LINUX}
  Classes;
  //fileinfo;


type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure showwindow(seconds: integer);
  private

  public

  end;


//procedure main;

var
  Form1: TForm1;


implementation

{$IFDEF WINDOWS}
function IsElevated: boolean;
const
  TokenElevation = TTokenInformationClass(20);
  //TokenElevation = TTokenInformationClass;

type
  TOKEN_ELEVATION = record
    TokenIsElevated: DWORD;
  end;
var
  TokenHandle: THandle;
  ResultLength: cardinal;
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
        if GetTokenInformation(TokenHandle, TTokenInformationClass(20),
          @ATokenElevation, SizeOf(ATokenElevation), ResultLength) then
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


function GetNetUser(Host: string; var UserName: string;
  var ErrorInfo: string): boolean;
  { for Host = '' Username will become the name of the current user of the process }

var
  pLocalName: PChar;
  pUserName: PChar;


  function ApiCall(var Username, ErrorInfo: string; BuffSize: DWord): boolean;
  var
    errorcode: DWord;
    nBuffSize: DWord;
    pErrorBuff, pNameBuff: PChar;
    nErrorBuffSize: DWord = 0;
    nNameBuffSize: DWord = 0;

  begin
    Result := False;
    GetMem(pUserName, BuffSize);
    nBuffSize := Buffsize;

    UserName := '';
    errorCode := WNetGetUser(nil, pUserName, nBuffSize);


    case errorCode of
      no_error:
      begin
        ErrorInfo := '';
        SetLength(UserName, StrLen(pUserName));
        UserName := pUserName;
        Result := True;
      end;
      ERROR_NOT_CONNECTED: ErrorInfo :=
          'The device specified by lpszLocalName is not a redirected device or a connected network name.';
      ERROR_MORE_DATA: ApiCall(UserName, ErrorInfo, nBuffSize + 1);
      ERROR_NO_NETWORK: ErrorInfo := 'No network is present.';
      ERROR_EXTENDED_ERROR:
      begin
        GetMem(pErrorBuff, 300);
        GetMem(pNameBuff, 300);
        WNetGetLastError(errorcode, pErrorBuff, nErrorBuffSize,
          pNameBuff, nNameBuffSize);
        ErrorInfo := pErrorBuff;
        FreeMem(pErrorBuff);
        FreeMem(pNameBuff);
      end;
      ERROR_NO_NET_OR_BAD_PATH: ErrorInfo :=
          'None of the providers recognized this local name as having a connection. '
          +
          'However, the network is not available for at least one provider to whom the connection may belong';
      else
        errorInfo := 'NT-Error ' + SysErrorMessage(errorCode);
    end;

    if errorCode <> no_error then
      errorInfo := IntToStr(errorCode) + ' ' + errorInfo;

    FreeMem(pUserName);
  end;

begin
  if Host <> '' then
    pLocalName := PChar(Host)
  else
    pLocalName := nil;

  if ApiCall(Username, ErrorInfo, 100) then
    Result := True
  else
    Result := False;
end;
{$ENDIF WINDOWS}


procedure TForm1.FormCreate(Sender: TObject);
var
  showtimestr: string;
  showtimeint: integer;
begin
  if Application.HasOption('showwindow') then
  begin
    showtimestr := Application.GetOptionValue('showwindow');
    try
      showtimeint := StrToInt(showtimestr);
    except
      writeln('<' + showtimestr + '< is not a integer. Using default of 1 second.');
      showtimeint := 1;
    end;
    form1.showwindow(showtimeint);
  end;
end;

procedure TForm1.showwindow(seconds: integer);
begin
  form1.Visible := True;
  Application.ProcessMessages;
  opsiscripttesthelper_main.timer1.Interval := seconds * 1000;
  opsiscripttesthelper_main.timer1.Enabled := True;
end;


procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  opsiscripttesthelper_main. Timer1.Enabled := False;
  form1.Visible := False;
  halt(myexitcode);
  Application.Terminate;
end;

initialization
   {$I helperwin.lrs}



end.
