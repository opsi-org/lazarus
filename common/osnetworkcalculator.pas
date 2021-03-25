unit osnetworkcalculator;  // for manipulating ip addresses.

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  resolve,
  RegExpr,
  Process;

type
  StringArray = array of string;

function isValidIP4(ip4adr: string): boolean;
//return true if the IPv4 address is valid.
function getIP4NetworkByAdrAndMask(ip4adr, netmask: string): string;
// return network address for the IP address and netmask.
function isValidIP4Network(ip4adr, netmask: string): boolean;
// return true for a valid network address.
function isValidIP4Host(ip4adr, netmask: string): boolean;
// return true for a valid host address.
function getDefaultNetmaskByIP4adr(ip4adr: string): string;
// return default netmask for the IPv4 address.
function GetHostByName(HostName: string): string;
function getNetmaskByIP4adr(cidr: string): string;
// return netmask for the IPv4 address.
function getNetworkDetails(Requests: array of string): StringArray;
// return requested network details

implementation

function isValidIP4(ip4adr: string): boolean;
  //return true if the IPv4 address is valid.
var
  regexobj: TRegExpr;
begin
  Result := False;
  if ip4adr <> '' then
  begin
    regexobj := TRegExpr.Create;
    try
      regexobj.Expression :=
        '^(((25[0-5])|(2[0-4]\d)|(1\d{2})|(\d{1,2}))\.){3}(((25[0-5])|(2[0-4]\d)|(1\d{2})|(\d{1,2})))$';
      if regexobj.Exec(trim(ip4adr)) then
        Result := True;
    finally
      regexobj.Free;
    end;
  end;
end;

function binToDec(binary: string): integer;
  //convert binary to decimal.
var
  decimal: longint;
  pos: byte;
begin
  Result := 0;
  decimal := 0;
  for pos := 1 to length(binary) do
  begin
    decimal := (decimal shl 1) + (Ord(binary[pos]) - 48);
  end;
  Result := decimal;
end;

function cidrToNetmask(cidr: string): string;
  // convert cidr notation to dotted decimal notation.
var
  shortmask, cidrcounter: integer;
  longmask, netmask, longmaskdotted: string;
  binaryoctets: array of string;
begin
  Result := '';

  shortmask := StrToInt(cidr);

  for cidrcounter := 1 to shortmask do
    longmask += '1';
  for cidrcounter := shortmask + 1 to 32 do
    longmask += '0';

  longmaskdotted := Copy(longmask, 1, 8) + '.' + Copy(longmask, 9, 8) +
    '.' + Copy(longmask, 17, 8) + '.' + Copy(longmask, 25, 8);
  binaryoctets := longmaskdotted.Split(['.']);

  netmask := IntToStr(binToDec(binaryoctets[0])) + '.';
  netmask += IntToStr(binToDec(binaryoctets[1])) + '.';
  netmask += IntToStr(binToDec(binaryoctets[2])) + '.';
  netmask += IntToStr(binToDec(binaryoctets[3]));

  Result := netmask;
end;

function getIP4NetworkByAdrAndMask(ip4adr, netmask: string): string;
  // return network address for the IP address and netmask.
var
  ipoctets, netmaskoctets: array of string;
  networkadr: string;
  validinputs: boolean;
begin
  Result := '';
  networkadr := '';
  validinputs := False;

  if isValidIP4(ip4adr) then
  begin
    if pos('.', netmask) = 0 then
    begin
      if (StrToInt(netmask) > 0) and (StrToInt(netmask) < 32) then
      begin
        netmask := cidrToNetmask(netmask);
        validinputs := True;
      end;
    end
    else if isValidIP4(netmask) then
      validinputs := True;
  end;

  if validinputs then
  begin
    ipoctets := ip4adr.Split(['.']);
    netmaskoctets := netmask.Split(['.']);

    networkadr := IntToStr(StrToInt(ipoctets[0]) and StrToInt(netmaskoctets[0])) + '.';
    networkadr += IntToStr(StrToInt(ipoctets[1]) and
      StrToInt(netmaskoctets[1])) + '.';
    networkadr += IntToStr(StrToInt(ipoctets[2]) and
      StrToInt(netmaskoctets[2])) + '.';
    networkadr += IntToStr(StrToInt(ipoctets[3]) and StrToInt(netmaskoctets[3]));

    Result := networkadr;
  end
  else
    Result := '';
end;

function isValidIP4Network(ip4adr, netmask: string): boolean;
  // return true for a valid network address.
var
  validinputs: boolean;
begin
  Result := False;
  validinputs := False;

  if isValidIP4(ip4adr) then
  begin
    if pos('.', netmask) = 0 then
    begin
      if (StrToInt(netmask) > 0) and (StrToInt(netmask) < 32) then
      begin
        netmask := cidrToNetmask(netmask);
        validinputs := True;
      end;
    end
    else if isValidIP4(netmask) then
      validinputs := True;
  end;

  if validinputs then
  begin
    if getIP4NetworkByAdrAndMask(ip4adr, netmask) = ip4adr then
      Result := True;
  end
  else
    Result := False;
end;

function findBroadcastAddress(ip4adr, netmask: string): string;
  // return broadcast address for the ip and netmask.
var
  ipoctets, netmaskoctets: array of string;
  broadcastadr: string;
begin
  Result := '';
  broadcastadr := '';

  if pos('.', netmask) = 0 then
    netmask := cidrToNetmask(netmask);

  ipoctets := ip4adr.Split(['.']);
  netmaskoctets := netmask.Split(['.']);

  broadcastadr := IntToStr(StrToInt(ipoctets[0]) or
    (255 - (StrToInt(netmaskoctets[0])))) + '.';
  broadcastadr += IntToStr(StrToInt(ipoctets[1]) or
    (255 - (StrToInt(netmaskoctets[1])))) + '.';
  broadcastadr += IntToStr(StrToInt(ipoctets[2]) or
    (255 - (StrToInt(netmaskoctets[2])))) + '.';
  broadcastadr += IntToStr(StrToInt(ipoctets[3]) or
    (255 - (StrToInt(netmaskoctets[3]))));

  Result := broadcastadr;
end;

function isValidIP4Host(ip4adr, netmask: string): boolean;
  // return true for a valid host address.
var
  networkadr, broadcastadr: string;
  ipoctets, networkoctets, broadcastoctets: array of string;
  octet: integer;
  validinputs: boolean;
begin
  Result := False;

  validinputs := False;

  if isValidIP4(ip4adr) then
  begin
    if pos('.', netmask) = 0 then
    begin
      if (StrToInt(netmask) > 0) and (StrToInt(netmask) < 32) then
      begin
        netmask := cidrToNetmask(netmask);
        validinputs := True;
      end;
    end
    else if isValidIP4(netmask) then
      validinputs := True;
  end;

  if validinputs then
  begin
    networkadr := getIP4NetworkByAdrAndMask(ip4adr, netmask);
    broadcastadr := findBroadcastAddress(ip4adr, netmask);

    ipoctets := ip4adr.Split(['.']);
    networkoctets := networkadr.Split(['.']);
    broadcastoctets := broadcastadr.Split(['.']);

    for octet := 0 to 2 do
    begin
      if (ipoctets[octet] >= networkoctets[octet]) and
        (ipoctets[octet] <= broadcastoctets[octet]) then
      begin
        if (ipoctets[3] > networkoctets[3]) and (ipoctets[3] < broadcastoctets[3]) then
          Result := True;
      end;
    end;
  end
  else
    Result := False;
end;

function findNetworkClass(ip4adr: string): string;
  // return network's class for the IP address.
var
  ipoctets: array of string;
  networkclass: string;
begin
  Result := '';
  networkclass := '';

  ipoctets := ip4adr.Split(['.']);

  if (StrToInt(ipoctets[0]) >= 0) and (StrToInt(ipoctets[0]) <= 127) then
    networkclass := 'A'
  else if (StrToInt(ipoctets[0]) >= 128) and (StrToInt(ipoctets[0]) <= 191) then
    networkclass := 'B'
  else if (StrToInt(ipoctets[0]) >= 192) and (StrToInt(ipoctets[0]) <= 223) then
    networkclass := 'C'
  else if (StrToInt(ipoctets[0]) >= 224) and (StrToInt(ipoctets[0]) <= 239) then
    networkclass := 'D'
  else if (StrToInt(ipoctets[0]) >= 240) and (StrToInt(ipoctets[0]) <= 255) then
    networkclass := 'E';

  Result := networkclass;
end;

function getDefaultNetmaskByIP4adr(ip4adr: string): string;
  // return default netmask for the IPv4 address.
var
  netclass, netmask: string;
begin
  Result := '';
  netmask := '';

  if isValidIP4(ip4adr) then
  begin
    netclass := findNetworkClass(ip4adr);

    if netclass = 'A' then
      netmask := '255.0.0.0'
    else if netclass = 'B' then
      netmask := '255.255.0.0'
    else if netclass = 'C' then
      netmask := '255.255.255.0'
    else if netclass = 'D' then
      netmask := 'not defined'
    else if netclass = 'E' then
      netmask := 'not defined';

    Result := netmask;
  end
  else
    Result := '';
end;

// https://www.lazarusforum.de/viewtopic.php?t=1396
function GetHostByName(HostName: string): string;
var
  host: THostResolver;
begin
  if isValidIP4(HostName) then
    Result := HostName
  else
  begin
    host := THostResolver.Create(nil);
    if host.NameLookup(HostName) then
      Result := host.AddressAsString
    else
      Result := '';
    host.Free;
  end;
end;


function getNetmaskByIP4adr(cidr: string): string;
  // return netmask for the IPv4 address.
begin
  Result := cidrToNetmask(cidr);
end;

function getNetworkDetails(Requests: array of string): StringArray;
  // return requested network details
var
  NetworkDetails, request: string;
  i, index: integer;
begin
  // get network details with console command 'nmcli dev show' (requires unit "process")
  if RunCommand('/bin/sh', ['-c', 'echo | nmcli dev show'], NetworkDetails) then
  begin
    // for each request store the answer at the respective position in array of string 'Result'
    SetLength(Result, Length(Requests));
    // go through requests
    for i := 0 to Length(Requests) - 1 do
    begin
      Result[i] := '';
      // find request in NetworkDetails
      request := Requests[i] + ':';
      index := NetworkDetails.IndexOf(request);
      if index <> -1 then
      begin
        // go to position of answer
        index += request.Length + 1;
        while NetworkDetails[index] = ' ' do
          index += 1;
        // write answer in 'Result'
        while (NetworkDetails[index] <> ' ') and (NetworkDetails[index] <> #10) do
        begin
          Result[i] += NetworkDetails[index];
          index += 1;
        end;
      end;
    end;
  end;
end;

end.
