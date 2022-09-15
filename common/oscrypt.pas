// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: Rupert Roeder, detlef oertel


unit oscrypt;

{$mode delphi}


interface

uses
  Classes,
  SysUtils,
  osHash,
  blowfish,
  DCPblowfish,
  DCPcrypt2,
  DCPblockciphers,
  DCPmd5,
  DCPsha1;

type
  bytearray = array[0..255] of byte;

function encryptStringBlow(KeyStr, Data: string): string;
function decryptStringBlow(KeyStr, Data: string): string;
procedure transformHex(const hexstring: string; var hexarray: bytearray);
function decrypt_hex_blow(hexkey, hexpass: string): string;
function encrypt_hex_blow(hexkey, hexpass: string): string;
function DecryptBlowfish(const myencrypted, mykey: string): string;
function MD5fromFile(filename: string): string;

implementation

procedure transformHex(const hexstring: string; var hexarray: bytearray);
var
  section: string;
  thesize: integer;
  onehex: string;
  i: integer;
begin
  theSize := length(hexstring) div 2;
  section := hexstring;
  for i := 0 to theSize - 1 do
  begin
    onehex := copy(section, 1, 2);
    HexToBin(PChar(onehex), @hexarray[i], 1);
    section := copy(section, 3, length(section));
  end;
end;

function decrypt_hex_blow(hexkey, hexpass: string): string;
var
  passByte, keyByte: bytearray;
  passByteDecry: bytearray;
  passSize: integer;
  keySize: integer;
  i: integer;
  Cipher: TDCP_blowfish;
begin
  transformHex(hexkey, keyByte);
  transformHex(hexpass, passByte);

  passSize := length(hexpass) div 2;
  keySize := length(hexkey) div 2;

  //DCP_blowfish1.Init(keyByte,keySize*8,PChar('OPSI1234'));
  //DCP_blowfish1.Decrypt(passByte,passByteDecry,passSize);

  Cipher := TDCP_blowfish.Create(nil);
  Cipher.Init(keyByte, keySize * 8, PChar('OPSI1234')); // remember key size is in BITS

  Cipher.DecryptCBC(passByte, passByteDecry, passSize);
  Cipher.Burn;
  Cipher.Free;

  Result := '';
  for i := 0 to passSize - 1 do
    Result := Result + char(passByteDecry[i]);
end;

function encrypt_hex_blow(hexkey, hexpass: string): string;
var
  passByte, keyByte: bytearray;
  passByteEncry: bytearray;
  passSize: integer;
  keySize: integer;
  i: integer;
  Cipher: TDCP_blowfish;
begin
  transformHex(hexkey, keyByte);
  transformHex(hexpass, passByte);

  passSize := length(hexpass) div 2;
  keySize := length(hexkey) div 2;

  //DCP_blowfish1.Init(keyByte,keySize*8,PChar('OPSI1234'));
  //DCP_blowfish1.Decrypt(passByte,passByteDecry,passSize);

  Cipher := TDCP_blowfish.Create(nil);
  Cipher.Init(keyByte, keySize * 8, PChar('OPSI1234')); // remember key size is in BITS

  Cipher.EncryptCBC(passByte, passByteEncry, passSize);
  Cipher.Burn;
  Cipher.Free;

  Result := '';
  for i := 0 to passSize - 1 do
    Result := Result + char(passByteEncry[i]);
end;


function DecryptBlowfish(const myencrypted, mykey: string): string;
var
  Cipher: TDCP_blowfish;
  Key: array[0..15] of byte; // key can be any size upto 448bits with blowfish (56bytes)
  //Buffer: array[0..127] of byte; // buffer can be any size
  s: string;
  i: integer;
begin
  //Cipher.Reset;
  //Cipher.DecryptCBC(Buffer,Buffer,Sizeof(Buffer));
  s := myencrypted;
  for i := 0 to 15 do
  begin
    key[i] := StrToInt('$' + mykey[i * 2 + 1] + mykey[i * 2 + 2]);
  end;
  Cipher := TDCP_blowfish.Create(nil);
  Cipher.Init(Key, Sizeof(Key) * 8, nil); // remember key size is in BITS

  Cipher.DecryptCBC(s[1], s[1], Length(s));
  Cipher.Burn;
  Cipher.Free;
  Result := s;
end;

// dcpcrypt/Docs/Ciphers.html
function encryptStringBlow(KeyStr: string; Data: string): string;
var
  Cipher: TDCP_blowfish;
begin
  Cipher := TDCP_blowfish.Create(nil);
  Cipher.InitStr(KeyStr, TDCP_sha1);
  // initialize the cipher with a hash of the passphrase
  Data := Cipher.EncryptString(Data);
  Cipher.Burn;
  Cipher.Free;
  Result := Data;
end;


function decryptStringBlow(KeyStr: string; Data: string): string;
var
  Cipher: TDCP_blowfish;
begin
  Cipher := TDCP_blowfish.Create(nil);
  Cipher.InitStr(KeyStr, TDCP_sha1);
  // initialize the cipher with a hash of the passphrase
  Data := Cipher.DecryptString(Data);
  Cipher.Burn;
  Cipher.Free;
  Result := Data;
end;


function MD5fromFile(filename: string): string;
begin
  Result := HashFromFile(filename, 'MD5');
end;


end.
