unit oscertificates;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  Windows,
  jwawincrypt,
  strutils;

function open_cert_system_store(var mystore: HCERTSTORE): boolean;
function close_cert_store(hstore: HCERTSTORE): boolean;
//function read_cert() : CERT_CONTEXT;
function install_ca(hstore: HCERTSTORE; binaryBuffer: pbyte; bufsize: dword): boolean;
function remove_ca(hstore: HCERTSTORE; ca_cert_name: string): boolean;
function pemfileToBinarybuf(filename: string; var binaryBuffer: pbyte;
  var bufsize: dword): boolean;
function pemBinarybufToCert_context(binaryBuffer: pbyte; bufsize: dword;
  var ca_cert: CERT_CONTEXT): boolean;
function pemfileToEncodedline(filename: string; var myPemString: string): boolean;
function pemfileToSystemStore(filename: string): boolean;

implementation

// https://docs.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-cryptstringtobinarya
// https://docs.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-cryptdecodeobjectex
// https://docs.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certaddencodedcertificatetostore
// https://docs.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certopenstore
// https://www.generacodice.com/en/articolo/139882/load-an-pem-encoded-x-509-certificate-into-windows-cryptoapi
// https://stackoverflow.com/questions/8168770/how-can-i-access-windows-root-certificate-authorities-certificates-with-delphi

function pemBinarybufToCert_context(binaryBuffer: pbyte; bufsize: dword;
  var ca_cert: CERT_CONTEXT): boolean;
begin
  // function CryptDecodeObjectEx(dwCertEncodingType: DWORD; lpszStructType: LPCSTR;
  //  pbEncoded: Pointer; cbEncoded, dwFlags: DWORD; pDecodePara: PCRYPT_DECODE_PARA;
  //  pvStructInfo: Pointer; var pcbStructInfo: DWORD): BOOL; stdcall;

  // first call to get the size of the buffer
  Result := CryptDecodeObjectEx(X509_ASN_ENCODING, X509_PUBLIC_KEY_INFO,
    binaryBuffer, bufsize, 0, nil, ca_cert.pbCertEncoded, ca_cert.cbCertEncoded);
end;


function pemfileToBinarybuf(filename: string; var binaryBuffer: pbyte;
  var bufsize: dword): boolean;
var
  myPemString, myPemLine: string;
  myPemFile: Textfile;
  stream: TFileStream;
begin
  stream := TFileStream.Create(filename, fmOpenRead or fmShareDenyNone);
  try
    SetLength(myPemString, stream.Size);
    stream.Read(myPemString[1], stream.Size);
  finally
    stream.Free;
  end;
  (*
  assignFile(myPemFile,filename);
  reset(myPemFile);
  myPemString := '';
  while not eof(myPemFile) do
  begin
    readln(myPemFile,myPemLine);
    if not (StartsStr('-----',myPemLine)) then
     myPemString := myPemString +myPemLine;
  end;
  CloseFile(myPemFile);
  *)
  //function CryptStringToBinary(pszString: LPCSTR; cchString: DWORD;
  //dwFlags: DWORD; ppBinary: BYTE; var ppcbBinary: DWORD;
  //ppdwSkip: DWORD): BOOL; stdcall;

  (*
  // first call to get the size of the buffer
  result := cryptstringtobinarya(pchar(myPemString),length(myPemString),CRYPT_STRING_BASE64,
                                 nil,bufsize,nil);
  if result then
   begin
      bufsize := bufsize +1;
      Getmem(binaryBuffer,bufsize); *)
  Result := cryptstringtobinarya(PChar(myPemString), length(myPemString),
    CRYPT_STRING_BASE64HEADER, binaryBuffer, bufsize, nil);
  // end;
end;

function pemfileToEncodedline(filename: string; var myPemString: string): boolean;
var
  //myPemString,
  myPemLine: string;
  myPemFile: Textfile;
begin
  assignFile(myPemFile, filename);
  reset(myPemFile);
  myPemString := '';
  while not EOF(myPemFile) do
  begin
    readln(myPemFile, myPemLine);
    if not (StartsStr('-----', myPemLine)) then
      myPemString := myPemString + myPemLine;
  end;
  CloseFile(myPemFile);
  //function CryptStringToBinary(pszString: LPCSTR; cchString: DWORD;
  //dwFlags: DWORD; ppBinary: BYTE; var ppcbBinary: DWORD;
  //ppdwSkip: DWORD): BOOL; stdcall;

  (*
  // first call to get the size of the buffer
  result := cryptstringtobinarya(pchar(myPemString),length(myPemString),CRYPT_STRING_BASE64,
                                 nil,bufsize,nil);
  if result then
   begin
      bufsize := bufsize +1;
      Getmem(binaryBuffer,bufsize); *)
  //result := cryptstringtobinarya(pchar(myPemString),length(myPemString),CRYPT_STRING_BASE64,
  //                               binaryBuffer,bufsize,nil);
  // end;
end;


function open_cert_system_store(var mystore: HCERTSTORE): boolean;
begin
  try
    Result := False;
    //mystore := CertOpenSystemStore(0, 'Root');
    mystore := CertOpenStore(CERT_STORE_PROV_SYSTEM,0,0,CERT_SYSTEM_STORE_LOCAL_MACHINE,PWchar('Root'));
    //mystore := CertOpenStore(CERT_STORE_PROV_SYSTEM,0,0,
    //                         CERT_SYSTEM_STORE_LOCAL_MACHINE or CERT_STORE_OPEN_EXISTING_FLAG,pchar('Root'));
    if mystore = nil then
    begin
      Result := False;
      //RaiseLastWin32Error;
    end
    else
      Result := True;
  except
    //RaiseLastWin32Error;
  end;
end;

function close_cert_store(hstore: HCERTSTORE): boolean;
begin
  if CertCloseStore(hstore, CERT_CLOSE_STORE_FORCE_FLAG) then
    Result := True
  else
    Result := False;
end;

function read_cert(): CERT_CONTEXT;
begin
  // result := CertCreateCertificateContext( );
end;

function install_ca(hstore: HCERTSTORE; binaryBuffer: pbyte; bufsize: dword): boolean;
var
  Pca_cert: PCCERT_CONTEXT;
begin
  //Pca_cert := @ca_cert;
  //result := CertAddEncodedCertificateToStore(hstore,X509_ASN_ENCODING,ca_cert.pbCertEncoded,ca_cert.cbCertEncoded,CERT_STORE_ADD_REPLACE_EXISTING,@Pca_cert);
  Result := CertAddEncodedCertificateToStore(hstore, X509_ASN_ENCODING,
    binaryBuffer, bufsize, CERT_STORE_ADD_REPLACE_EXISTING, nil);
end;

function remove_ca(hstore: HCERTSTORE; ca_cert_name: string): boolean;
var
  ca_cert: CERT_CONTEXT;
  Pca_cert: PCCERT_CONTEXT;
  cbsize: dword;
begin
  Result := False;
  Pca_cert := @ca_cert;
  Pca_cert := CertFindCertificateInStore(hstore, X509_ASN_ENCODING,
    0, CERT_FIND_SUBJECT_STR, PChar(ca_cert_name), Pca_cert);
  if Pca_cert = nil then
  begin
    // failed
    Result := False;
  end
  else
  begin

    cbsize := CertGetNameStringW(Pca_cert, CERT_NAME_FRIENDLY_DISPLAY_TYPE,
      0, nil, nil, 0);
    if CertDeleteCertificateFromStore(Pca_cert) then
    begin
      // failed
    end;
    CertFreeCertificateContext(Pca_cert);
    Result := True;
  end;
end;

function pemfileToSystemStore(filename: string): boolean;
var
  mystore: HCERTSTORE;
  binaryBuffer: pbyte;
  bufsize: dword;
  ca_cert: CERT_CONTEXT;
  mybool: boolean;
  mypem: string;
  message: string;
  i: integer;
  myPemFile: textfile;
begin
  Result := False;
  bufsize := 4096;
  binaryBuffer := GetMem(bufsize);
  mybool := pemfileToBinarybuf(filename, binaryBuffer, bufsize);
  //mybool := pemfileToEncodedline(ExtractFilePath(ParamStr(0)) + '\opsi-ca-cert.pem',mypem);
  if not mybool then
  begin
    message := SysErrorMessage(GetLastOSError);
    writeln('failed: pemfileToBinarybuf: ' + message);
    exit;
  end;
  writeln('done: pemfileToBinarybuf');
  mybool := open_cert_system_store(mystore);
  if not mybool then
  begin
    message := SysErrorMessage(GetLastOSError);
    writeln('failed: open_cert_system_store: ' + message);
    exit;
  end;
  writeln('done: open_cert_system_store');
  mybool := install_ca(mystore, binaryBuffer, bufsize);
  if not mybool then
  begin
    message := SysErrorMessage(GetLastOSError);
    writeln('failed: install_ca: ' + message);
    exit;
  end;
  writeln('done: install_ca');
  mybool := close_cert_store(mystore);
  if not mybool then
  begin
    message := SysErrorMessage(GetLastOSError);
    writeln('failed: close_cert_store: ' + message);
    exit;
  end;
  writeln('done: close_cert_store');
  writeln('done: pemfileToSystemStore');
  Result := mybool;
end;

end.