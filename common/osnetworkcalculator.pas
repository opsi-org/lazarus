unit osnetworkcalculator;  // for manipulating ip addresses.

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  resolve,
  RegExpr,
  Process,
  strutils;

type
  StringArray = array of string;

type
  TNetworkIP = record
    case Mode: boolean of
      True: (IP1, IP2, IP3, IP4: byte);
      False: (IP: longword);
  end;

function isValidIP4(ip4adr: string): boolean;
//return true if the IPv4 address is valid.
function isValidIP6(ip6adr: string): boolean;
//return true if the IPv6 address is valid. ToDo: testing
function isValidIP(ipadr: string): boolean;
//return true if the IP address (ipv4 or ipv6) is valid.
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
function cidrToNetmask(cidr: string): string;
// convert cidr notation to dotted decimal notation.
function netmaskToCidr(netmask: string): string;
// convert dotted decimal notation notation to cidr.

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

function isValidIP6(ip6adr: string): boolean;
  //return true if the IPv6 address is valid.
  //function isIP6 from unit synaIP is not used due to an additional unit is needed and more important gives false negatives for Hybrid dual-stack IPv6/IPv4 implementations.
  //Hybrid dual-stack IPv6/IPv4 implementations recognize a special class of addresses, the IPv4-mapped IPv6 addresses. These addresses are typically written with a 96-bit prefix in the standard IPv6 format, and the remaining 32 bits are written in the customary dot-decimal notation of IPv4.
  //Addresses in this group consist of an 80-bit prefix of zeros, the next 16 bits are ones, and the remaining, least-significant 32 bits contain the IPv4 address. For example, ::ffff:192.0.2.128 represents the IPv4 address 192.0.2.128.
  //from https://en.wikipedia.org/wiki/IPv6 accessed on 31.01.2022
var
  regexobj: TRegExpr;
begin
  Result := False;
  if ip6adr <> '' then
  begin
    regexobj := TRegExpr.Create;
    try
      //regexpr from https://stackoverflow.com/questions/53497/regular-expression-that-matches-valid-ipv6-addresses
      regexobj.Expression :=
        '^(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))$';
      if regexobj.Exec(trim(ip6adr)) then
        Result := True;
    finally
      regexobj.Free;
    end;
  end;
end;

function isValidIP(ipadr: string): boolean;
begin
  if isValidIP4(ipadr) or isValidIP6(ipadr) then
    Result := True
  else
    Result := False;
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
  if TryStrToInt(cidr, shortmask) then
  begin

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
  end
  else
    // not a valid number
  ;
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

{#######################################################
begin of functions from
https://fpc-pascal.freepascal.narkive.com/j0XK4BHH/re-library-for-network-calculation
########################################################}

function StringToIP(aIP: ansistring): TNetworkIP;
begin
  Result.Mode := True;
  Result.IP4 := StrToInt(Copy2SymbDel(aIP, '.'));
  Result.IP3 := StrToInt(Copy2SymbDel(aIP, '.'));
  Result.IP2 := StrToInt(Copy2SymbDel(aIP, '.'));
  Result.IP1 := StrToInt(Copy2SymbDel(aIP, '.'));
end;

function IPToString(aIP: TNetworkIP): string;
begin
  aIP.Mode := True;
  Result := IntToStr(aIP.IP4) + '.' + IntToStr(aIP.IP3) + '.' +
    IntToStr(aIP.IP2) + '.' + IntToStr(aIP.IP1);
end;

function IPToBits(aIP: TNetworkIP): string;
begin
  aIP.Mode := True;
  Result := BinStr(aIP.IP4, 8) + BinStr(aIP.IP3, 8) + BinStr(aIP.IP2, 8) +
    BinStr(aIP.IP1, 8);
end;

function NetMaskToHostMask(NetMask: TNetworkIP): TNetworkIP;
begin
  Result.Mode := False;
  NetMask.Mode := False;
  Result.IP := NetMask.IP xor %11111111111111111111111111111111;
end;

function HostMaskToNetMask(HostMask: TNetworkIP): TNetworkIP;
begin
  Result.Mode := False;
  HostMask.Mode := False;
  Result.IP := HostMask.IP xor %11111111111111111111111111111111;
end;

function CIDRToNetMask(Bits: byte): TNetworkIP;
var
  CurBit: byte;
  HostMask: TNetworkIP;
begin
  HostMask.Mode := False;
  HostMask.IP := 0;
  for CurBit := 1 to 32 - Bits do
    HostMask.IP := HostMask.IP + (1 shl (CurBit - 1));
  Result := HostMaskToNetMask(HostMask);
end;

function NetMaskToCIDR(NetMask: TNetworkIP): byte;
var
  CurBit: byte;
begin
  Result := 0;
  for CurBit := 32 downto 1 do
    if (NetMask.IP and (1 shl (CurBit - 1))) = (1 shl (CurBit - 1)) then
      Result := CurBit;
end;

function BroadcastOf(IP, NetMask: TNetworkIP): TNetworkIP;
var
  HostMask: TNetworkIP;
begin
  HostMask := NetMaskToHostMask(NetMask);
  Result.Mode := False;
  IP.Mode := False;
  HostMask.Mode := False;
  Result.IP := IP.IP or HostMask.IP;
end;

function NetworkOf(IP, NetMask: TNetworkIP): TNetworkIP;
begin
  IP.Mode := False;
  NetMask.Mode := False;
  Result.Mode := False;
  Result.IP := IP.IP and NetMask.IP;
end;

function HostCount(NetMask: TNetworkIP): longword;
var
  HostMask: TNetworkIP;
begin
  HostMask := NetMaskToHostMask(NetMask);
  HostMask.Mode := False;
  Result := HostMask.IP + 1;
end;

function SubNetCount(DefMask, Mask: TNetworkIP): longword;
var
  DefHosts, Hosts: longword;
begin
  DefHosts := HostCount(DefMask);
  Hosts := HostCount(Mask);
  Result := DefHosts div Hosts;
end;

function IPToRange(IP, NetMask: TNetworkIP): string;
var
  Net, Broad: TNetworkIP;
begin
  Net := NetworkOf(IP, NetMask);
  Broad := BroadCastOf(IP, NetMask);
  Net.Mode := False;
  Broad.Mode := False;
  Net.IP := Net.IP + 1;
  Broad.IP := Broad.IP - 1;
  Result := IPToString(Net) + '-' + IPToString(Broad);
end;

function IncSubNet(BaseIP, Mask: TNetworkIP): TNetworkIP;
var
  HostMask: TNetworkIP;
begin
  HostMask := NetMaskToHostMask(Mask);
  HostMask.Mode := False;
  HostMask.IP := HostMask.IP + 1;
  BaseIP.Mode := False;
  Result.Mode := False;
  Result.IP := BaseIP.IP + HostMask.IP;
end;

function HostCountToCIDR(Hosts: longword): byte;
var
  CurBit: byte;
  HostMask: TNetworkIP;
  Last1Bit: byte;
begin
  HostMask.Mode := False;
  HostMask.IP := Hosts;
  Last1Bit := 0;
  for CurBit := 1 to 32 do
    if (HostMask.IP and (1 shl (CurBit - 1))) = (1 shl (CurBit - 1)) then
      Last1Bit := CurBit;
  Result := 32 - Last1Bit;
end;

function HostCountToNetMask(Hosts: longword): TNetworkIP;
begin
  Result := CIDRToNetMask(HostCountToCIDR(Hosts));
end;

function IsMemberOfSubNet(IP, SubNet: TNetworkIP): boolean;
begin
  IP.Mode := False;
  Subnet.Mode := False;
  Result := IP.IP and Subnet.IP = Subnet.IP;
end;


{#######################################################
end of functions from
https://fpc-pascal.freepascal.narkive.com/j0XK4BHH/re-library-for-network-calculation
########################################################}

function netmaskToCidr(netmask: string): string;
  // convert dotted decimal notation notation to cidr.end.
var
  lookup: TStringList;
begin
  Result := '';
  try
    lookup := TStringList.Create;
    lookup.add('0.0.0.0=0');
    lookup.add('240.0.0.0=4');
    lookup.add('255.0.0.0=8');
    lookup.add('255.240.0.0=12');
    lookup.add('255.255.0.0=16');
    lookup.add('255.255.128.0=17');
    lookup.add('255.255.192.0=18');
    lookup.add('255.255.224.0=19');
    lookup.add('255.255.240.0=20');
    lookup.add('255.255.248.0=21');
    lookup.add('255.255.252.0=22');
    lookup.add('255.255.254.0=23');
    lookup.add('255.255.255.0=24');
    lookup.add('255.255.255.128=25');
    lookup.add('255.255.255.192=26');
    lookup.add('255.255.255.224=27');
    lookup.add('255.255.255.240=28');
    lookup.add('255.255.255.248=29');
    lookup.add('255.255.255.252=30');
    lookup.add('255.255.255.254=31');
    lookup.add('255.255.255.255=32');

    Result := lookup.Values[netmask];
  except
    FreeAndNil(lookup);
  end;
end;

end.
