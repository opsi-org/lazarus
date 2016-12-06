program getipbytarget;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp ,windows, winsock
  { you can add units after this };



const
  MAX_ADAPTER_DESCRIPTION_LENGTH = 128; // arb.
  MAX_ADAPTER_NAME_LENGTH        = 256; // arb.
  MAX_ADAPTER_ADDRESS_LENGTH     = 8;  // arb.



type
    { getIpByTarget }

  mygetIpByTarget = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;


  PIP_ADDRESS_STRING = ^IP_ADDRESS_STRING;
  IP_ADDRESS_STRING =
    packed record
      acString : array [1..16] of Char;
    end;

  PIP_MASK_STRING = ^IP_MASK_STRING;
  IP_MASK_STRING = IP_ADDRESS_STRING;

  PIP_ADDR_STRING = ^IP_ADDR_STRING;
  IP_ADDR_STRING =
    packed record
      Next     : PIP_ADDR_STRING;
      IpAddress : IP_ADDRESS_STRING;
      IpMask   : IP_MASK_STRING;
      Context  : DWORD;
    end;

  time_t = int64;

  PIP_ADAPTER_INFO = ^IP_ADAPTER_INFO;
  IP_ADAPTER_INFO =
    packed record
      Next               : PIP_ADAPTER_INFO;
      ComboIndex         : DWORD;
      AdapterName        : array [1..MAX_ADAPTER_NAME_LENGTH+4] of Char ;
      Description        : array [1..MAX_ADAPTER_DESCRIPTION_LENGTH+4] of Char;
      AddressLength      : UINT;
      Address            : array [1..MAX_ADAPTER_ADDRESS_LENGTH] of Byte;
      Index              : DWORD;
      dwType             : UINT;
      DhcpEnabled        : UINT;
      CurrentIpAddress   : PIP_ADDR_STRING;
      IpAddressList      : IP_ADDR_STRING;
      GatewayList        : IP_ADDR_STRING;
      DhcpServer         : IP_ADDR_STRING;
      HaveWins           : Boolean;
      PrimaryWinsServer  : IP_ADDR_STRING;
      SecondaryWinsServer : IP_ADDR_STRING;
      LeaseObtained      : time_t;
      LeaseExpires       : time_t;
    end;

function GetAdaptersInfo(const pAdapterInfo : PIP_ADAPTER_INFO;const pOutBufLen : PULONG) : DWORD; stdcall; external 'IPHLPAPI.DLL' name 'GetAdaptersInfo';
function GetBestInterface(const dwDestAddr: cardinal; var pdwBestIfIndex: DWORD)  : DWORD; stdcall; external 'IPHLPAPI.DLL' name 'GetBestInterface';

//implementation




{ mygetIpByTarget }

procedure mygetIpByTarget.DoRun;
var
  ErrorMsg: String;
      dwResult    : DWORD;
      dwLen       : DWORD;
      pAdapterWork : PIP_ADAPTER_INFO;
      pAdapterList : PIP_ADAPTER_INFO;
      iasWork     : IP_ADDR_STRING;
      adapterindex : DWORD;
      optionlist : TStringlist;
      myserver : AnsiString;

begin
  optionlist := TStringlist.Create;
  optionlist.Append('help');
  optionlist.Append('target:');

  // quick check parameters
  ErrorMsg:=CheckOptions('',optionlist);
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }
  if HasOption('target')  then
  begin
  { add your program here }
      myserver := GetOptionValue('target');
      //writeln('myserver = ',myserver);

       adapterindex := 0;
      GetBestinterface(inet_addr(PAnsiChar(myserver)),adapterindex);
      pAdapterList := nil;
      dwLen       := 0;
      dwResult    := GetAdaptersInfo(pAdapterList,@dwLen);
      if dwResult = ERROR_BUFFER_OVERFLOW then
      begin
        pAdapterList := AllocMem(dwLen);
        try
          dwResult := GetAdaptersInfo(pAdapterList,@dwLen);
          if dwResult = ERROR_SUCCESS then
          begin
            pAdapterWork := pAdapterList;
            repeat
              if pAdapterWork.Index = adapterindex then
                writeln('found:'+trim(pAdapterWork.IpAddressList.IpAddress.acString));
              pAdapterWork := pAdapterWork.Next;
            until pAdapterWork = nil;
          end;
        finally
          FreeMem(pAdapterList,dwLen);
        end;
      end;
      Terminate;
    end;


  // stop program loop
  Terminate;
end;

constructor mygetIpByTarget.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor mygetIpByTarget.Destroy;
begin
  inherited Destroy;
end;


procedure mygetIpByTarget.WriteHelp;
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
  writeln(' --target=IP-Number-of-target -> writes out IP-Number of best interface ');
  //writeln('Exitcodes:');
end;


var
  Application: mygetIpByTarget;

{$R *.res}

begin
  Application:=mygetIpByTarget.Create(nil);
  Application.Title:='getIpByTarget';
  Application.Run;
  Application.Free;
end.

