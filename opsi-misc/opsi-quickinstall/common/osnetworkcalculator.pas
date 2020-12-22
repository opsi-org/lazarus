unit osnetworkcalculator;  // for manipulating ip addresses.

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RegExpr;

function isValidIP4(ip4adr: string) : boolean; //return true if the IPv4 address is valid.
function getIP4NetworkByAdrAndMask(ip4adr, netmask : string) : string; // return network address for the IP address and netmask.
function isValidIP4Network(ip4adr,netmask : string) : boolean;  // return true for a valid network address.
function isValidIP4Host(ip4adr, netmask : string) : boolean;  // return true for a valid host address.
function getDefaultNetmaskByIP4adr(ip4adr : string) : string; // return default netmask for the IPv4 address.
function getNetmaskByIP4adr(cidr : string) : string; // return netmask for the IPv4 address.

implementation

function isValidIP4(ip4adr: string) : boolean;
//return true if the IPv4 address is valid.
var
  regexobj : TRegExpr;
begin
  result := false;
  regexobj := TRegExpr.Create;
  try
    regexobj.Expression := '^(((25[0-5])|(2[0-4]\d)|(1\d{2})|(\d{1,2}))\.){3}(((25[0-5])|(2[0-4]\d)|(1\d{2})|(\d{1,2})))$';
    if regexobj.Exec(trim(ip4adr)) then
      result := true;
  finally
    regexobj.Free;
  end;
end;

function binToDec(binary : string) : integer;
//convert binary to decimal.
var
  decimal : longint ;
  pos : byte ;
begin
  result := 0;
  decimal := 0 ;
  for pos := 1 to length(binary) do
  begin
   decimal := (decimal shl 1) + (Ord(binary[pos])-48) ;
  end;
  result := decimal;
end;

function cidrToNetmask(cidr: string) : string;
// convert cidr notation to dotted decimal notation.
var
  shortmask, cidrcounter : integer;
  longmask, netmask, longmaskdotted : string;
  binaryoctets : array of string;
begin
  result := '';

  shortmask := StrToInt(cidr);

  for cidrcounter:=1 to shortmask do
    longmask += '1';
  for cidrcounter:=shortmask+1 to 32 do
    longmask += '0';

  longmaskdotted := Copy(longmask,1,8) + '.' + Copy(longmask,9,8) + '.' +  Copy(longmask,17,8) + '.' + Copy(longmask,25,8);
  binaryoctets := longmaskdotted.Split(['.']);

  netmask := IntToStr(binToDec(binaryoctets[0])) + '.';
  netmask += IntToStr(binToDec(binaryoctets[1])) + '.';
  netmask += IntToStr(binToDec(binaryoctets[2])) + '.';
  netmask += IntToStr(binToDec(binaryoctets[3]));

  result := netmask;
end;

function getIP4NetworkByAdrAndMask(ip4adr, netmask : string) : string;
// return network address for the IP address and netmask.
var
  ipoctets, netmaskoctets : array of String;
  networkadr : string;
  validinputs : boolean;
begin
  result := '';
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
      end
    end
    else if isValidIP4(netmask) then
      validinputs := True;
  end;

  if validinputs then
  begin
    ipoctets := ip4adr.Split(['.']);
    netmaskoctets := netmask.Split(['.']);

    networkadr := IntToStr(StrToInt(ipoctets[0]) and StrToInt(netmaskoctets[0])) + '.';
    networkadr += IntToStr( StrToInt( ipoctets[1] ) and StrToInt(netmaskoctets[1] ) ) + '.';
    networkadr += IntToStr( StrToInt( ipoctets[2] ) and StrToInt(netmaskoctets[2] ) ) + '.';
    networkadr += IntToStr( StrToInt( ipoctets[3] ) and StrToInt(netmaskoctets[3] ) );

    result := networkadr;
  end
  else
    result := '';
end;

function isValidIP4Network(ip4adr, netmask : string) : boolean;
// return true for a valid network address.
var
  validinputs : boolean;
begin
  result := false;
  validinputs := False;

  if isValidIP4(ip4adr) then
  begin
    if pos('.', netmask) = 0 then
    begin
      if (StrToInt(netmask) > 0) and (StrToInt(netmask) < 32) then
      begin
        netmask := cidrToNetmask(netmask);
        validinputs := True;
      end
    end
    else if isValidIP4(netmask) then
      validinputs := True;
  end;

  if validinputs then
  begin
    if getIP4NetworkByAdrAndMask(ip4adr,netmask) = ip4adr then
      result := true;
  end
  else
    result := false;
end;

function findBroadcastAddress(ip4adr, netmask : string) : string;
// return broadcast address for the ip and netmask.
var
  ipoctets, netmaskoctets : array of String;
  broadcastadr : string;
begin
  result := '';
  broadcastadr := '';

  if pos('.', netmask) = 0 then
    netmask := cidrToNetmask(netmask);

  ipoctets := ip4adr.Split(['.']);
  netmaskoctets := netmask.Split(['.']);

  broadcastadr := IntToStr( StrToInt(ipoctets[0]) or (255 - ( StrToInt(netmaskoctets[0]) )) ) + '.';
  broadcastadr += IntToStr( StrToInt(ipoctets[1]) or (255 - ( StrToInt(netmaskoctets[1]) )) ) + '.';
  broadcastadr += IntToStr( StrToInt(ipoctets[2]) or (255 - ( StrToInt(netmaskoctets[2]) )) ) + '.';
  broadcastadr += IntToStr( StrToInt(ipoctets[3]) or (255 - ( StrToInt(netmaskoctets[3]) )) );

  result := broadcastadr;
end;

function isValidIP4Host(ip4adr, netmask : string) : boolean;
// return true for a valid host address.
var
  networkadr, broadcastadr : string;
  ipoctets, networkoctets, broadcastoctets : array of String;
  octet : integer;
  validinputs : boolean;
begin
  result := false;

  validinputs := False;

  if isValidIP4(ip4adr) then
  begin
    if pos('.', netmask) = 0 then
    begin
      if (StrToInt(netmask) > 0) and (StrToInt(netmask) < 32) then
      begin
        netmask := cidrToNetmask(netmask);
        validinputs := True;
      end
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

    for octet:=0 to 2 do
    begin
      if (ipoctets[octet] >= networkoctets[octet]) and (ipoctets[octet] <= broadcastoctets[octet]) then
      begin
        if(ipoctets[3]>networkoctets[3]) and (ipoctets[3]<broadcastoctets[3]) then
          result := true;
      end;
    end;
  end
  else
    result := false;
end;

function findNetworkClass(ip4adr : string) : string;
// return network's class for the IP address.
var
  ipoctets : array of String;
  networkclass : string;
begin
  result := '';
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

  result := networkclass;
end;

function getDefaultNetmaskByIP4adr(ip4adr : string) : string;
// return default netmask for the IPv4 address.
var
  netclass, netmask : string;
begin
  result := '';
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

    result := netmask;
  end
  else
    result:= '';
end;

function getNetmaskByIP4adr(cidr : string) : string; // return netmask for the IPv4 address.
begin
  result := cidrToNetmask(cidr);
end;

end.

