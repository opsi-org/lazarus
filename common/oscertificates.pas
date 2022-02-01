unit oscertificates;

{$mode ObjFPC}{$H+}

(*   macos
# Install
security add-trusted-cert -d -r trustRoot -k /Library/Keychains/System.keychain <pem-file>
# Load
security find-certificate -p -c <subject_name> /Library/Keychains/System.keychain
# Uninstall (pseudo code)
cert = find-certificate(<subject_name>)
while cert:
   security delete-certificate -Z <cert.sha1_hash> /Library/Keychains/System.keychain -t
   cert = find-certificate(<subject_name>)
#####################################
   Linux:
   def _get_cert_path_and_cmd():
       dist = {distro.id()}
       for name in (distro.like() or "").split(" "):
           if name:
               dist.add(name)
       if "centos" in dist or "rhel" in dist:
           # /usr/share/pki/ca-trust-source/anchors/
           return ("/etc/pki/ca-trust/source/anchors", "update-ca-trust")
       if "debian" in dist or "ubuntu" in dist:
           return ("/usr/local/share/ca-certificates", "update-ca-certificates")
       if "sles" in dist or "suse" in dist:
           return ("/usr/share/pki/trust/anchors", "update-ca-certificates")

       logger.error("Failed to set system cert path on distro '%s', like: %s", distro.id(), distro.like())
       raise RuntimeError(f"Failed to set system cert path on distro '{distro.id()}', like: {distro.like()}")


   def install_ca(ca_cert: crypto.X509):
       system_cert_path, cmd = _get_cert_path_and_cmd()

       logger.info("Installing CA '%s' into system store", ca_cert.get_subject().CN)

       cert_file = os.path.join(
           system_cert_path,
           f"{ca_cert.get_subject().CN.replace(' ', '_')}.crt"
       )
       with open(cert_file, "wb") as file:
           file.write(crypto.dump_certificate(crypto.FILETYPE_PEM, ca_cert))

       output = subprocess.check_output([cmd], shell=False)
       logger.debug("Output of '%s': %s", cmd, output)


   def load_ca(subject_name: str) -> crypto.X509:
       system_cert_path, _cmd = _get_cert_path_and_cmd()
       if os.path.exists(system_cert_path):
           for root, _dirs, files in os.walk(system_cert_path):
               for entry in files:
                   with open(os.path.join(root, entry), "rb") as file:
                       try:
                           ca_cert = crypto.load_certificate(crypto.FILETYPE_PEM, file.read())
                           if ca_cert.get_subject().CN == subject_name:
                               return ca_cert
                       except crypto.Error:
                           continue
       return None


   def remove_ca(subject_name: str) -> bool:
       system_cert_path, cmd = _get_cert_path_and_cmd()
       removed = 0
       if os.path.exists(system_cert_path):
           for root, _dirs, files in os.walk(system_cert_path):
               for entry in files:
                   filename = os.path.join(root, entry)
                   with open(filename, "rb") as file:
                       try:
                           ca_cert = crypto.load_certificate(crypto.FILETYPE_PEM, file.read())
                           if ca_cert.get_subject().CN == subject_name:
                               logger.info("Removing CA '%s' (%s)", subject_name, filename)
                               os.remove(filename)
                               removed += 1
                       except crypto.Error:
                           continue

       if removed:
           output = subprocess.check_output([cmd], shell=False)
           logger.debug("Output of '%s': %s", cmd, output)
       else:
           logger.info(
               "CA '%s' not found in '%s', nothing to remove",
               subject_name, system_cert_path
           )
   *)

interface

uses
  Classes, SysUtils,
  fileutil,
  {$IFDEF WINDOWS}
  Windows,
  jwawincrypt,
  {$ENDIF WINDOWS}
  {$IFDEF UNIX}
  OSProcessux,
  {$ENDIF UNIX}
  {$IFDEF LINUX}
  osfunclin,
  {$ENDIF LINUX}
  {$IFDEF OPSISCRIPT}
  ostxstringlist,
  {$ENDIF OPSISCRIPT}
  oslog,
  strutils;

{$IFDEF WINDOWS}
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
{$ENDIF WINDOWS}
function pemfileToSystemStore(filename: string): boolean;

implementation

{$IFDEF DARWIN}
function pemfileToSystemStore(filename: string): boolean;
begin
  // sudo security add-trusted-cert -d -r trustRoot -k /Library/Keychains/System.keychain <new-root-certificate>
  command := 'security add-trusted-cert -d -r trustRoot -k /Library/Keychains/System.keychain ';
  command := command + '"' + filename + '"';
  if not RunCommandAndCaptureOut(command, True, outlines, report,
    SW_HIDE, ExitCode, False, 1) then
  begin
    // Error
    logdatei.log('pemfileToSystemStore: failed: update store command with exitcode: '
      + IntToStr(exitcode), LLError);
  end
  else
  begin
    // success
    if exitcode = 0 then
      logdatei.log('Successful imported to system store: ' + filename, LLinfo)
    else
      logdatei.log('pemfileToSystemStore: failed: update store command with exitcode: '
        + IntToStr(exitcode), LLError);
  end;
end;

{$ENDIF DARWIN}

{$IFDEF LINUX}
// Ubuntu
// https://askubuntu.com/questions/73287/how-do-i-install-a-root-certificate
// https://wiki.ubuntuusers.de/CA/

// macos
// https://apple.stackexchange.com/questions/80623/import-certificates-into-the-system-keychain-via-the-command-line
// https://derflounder.wordpress.com/2011/03/13/adding-new-trusted-root-certificates-to-system-keychain/


function pemfileToSystemStore(filename: string): boolean;
var
  command: string;

  report: string;
  showcmd: integer;
  ExitCode: longint;
  distrotype, pathToStore, storeCommand: string;
  targetfile, certExt: string;
  {$IFDEF OPSISCRIPT}
  outlines: TXStringList;
  {$ELSE OPSISCRIPT}
  outlines: TStringList;
  {$ENDIF OPSISCRIPT}
begin
  Result := False;
  try
  {$IFDEF OPSISCRIPT}
    outlines := TXStringList.Create;
  {$ELSE OPSISCRIPT}
    outlines := TStringList.Create;
  {$ENDIF OPSISCRIPT}
    distrotype := getLinuxDistroType;
    if distrotype = 'debian' then
    begin
      pathToStore := '/usr/local/share/ca-certificates/';
      storeCommand := 'update-ca-certificates';
    end
    else if distrotype = 'suse' then
    begin
      pathToStore := '/usr/share/pki/trust/anchors/';
      storeCommand := 'update-ca-certificates';
    end
    else if distrotype = 'redhat' then
    begin
      pathToStore := '/etc/pki/ca-trust/source/anchors/';
      storeCommand := 'update-ca-trust';
    end;
    targetfile := pathToStore + ExtractFileName(filename) + '.crt';
    certExt := LowerCase(ExtractFileExt(filename));

    command := '';
    if certExt = '.pem' then
    begin
      // convert pem to crt
      command := 'openssl x509 -inform PEM -in ' + filename + ' -out ' + targetfile;
    end;
    if certExt = '.cer' then
    begin
      // convert cer to crt
      command := 'openssl x509 -inform DER -in ' + filename + ' -out ' + targetfile;
    end;
    if certExt = '.crt' then
    begin
      // just copy
      CopyFile(filename, targetfile);
    end;

    if command <> '' then
    begin
      if not RunCommandAndCaptureOut(command, True, outlines,
        report, SW_HIDE, ExitCode, False, 1) then
      begin
        // Error
        logdatei.log('pemfileToSystemStore: failed: convert&copy with exitcode: ' +
          IntToStr(exitcode), LLError);
      end
      else
      begin
        // success
        if exitcode = 0 then
          logdatei.log_prog('pemfileToSystemStore: Successful convert&copy : ' +
            filename, LLinfo)
        else
          logdatei.log('pemfileToSystemStore: failed: update store command with exitcode: '
            + IntToStr(exitcode), LLError);
      end;
    end;

    command := storeCommand;

    if not RunCommandAndCaptureOut(command, True, outlines, report,
      SW_HIDE, ExitCode, False, 1) then
    begin
      // Error
      logdatei.log('pemfileToSystemStore: failed: update store command with exitcode: '
        + IntToStr(exitcode), LLError);
    end
    else
    begin
      // success
      if exitcode = 0 then
        logdatei.log('Successful imported to system store: ' + filename, LLinfo)
      else
        logdatei.log('pemfileToSystemStore: failed: update store command with exitcode: '
          + IntToStr(exitcode), LLError);
    end;
  finally
    FreeAndNil(outlines);
  end;
end;

{$ENDIF LINUX}

{$IFDEF WINDOWS}
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
    mystore := CertOpenStore(CERT_STORE_PROV_SYSTEM, 0, 0,
      CERT_SYSTEM_STORE_LOCAL_MACHINE, PWchar('Root'));
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
    logdatei.log('pemfileToSystemStore: failed: pemfileToBinarybuf: ' +
      message, LLError);
    exit;
  end;
  logdatei.log_prog('done: pemfileToBinarybuf', LLdebug);
  mybool := open_cert_system_store(mystore);
  if not mybool then
  begin
    message := SysErrorMessage(GetLastOSError);
    logdatei.log('pemfileToSystemStore: failed: open_cert_system_store: ' +
      message, LLError);
    exit;
  end;
  logdatei.log_prog('done: open_cert_system_store', LLdebug);
  mybool := install_ca(mystore, binaryBuffer, bufsize);
  if not mybool then
  begin
    message := SysErrorMessage(GetLastOSError);
    logdatei.log('pemfileToSystemStore: failed: install_ca: ' + message, LLError);
    exit;
  end;
  logdatei.log_prog('done: install_ca', LLdebug);
  mybool := close_cert_store(mystore);
  if not mybool then
  begin
    message := SysErrorMessage(GetLastOSError);
    logdatei.log('pemfileToSystemStore: failed: close_cert_store: ' + message, LLError);
    exit;
  end;
  logdatei.log_prog('done: close_cert_store', LLdebug);
  logdatei.log_prog('done: pemfileToSystemStore', LLdebug);
  logdatei.log('Successful imported to system store: ' + filename, LLinfo);
  Result := mybool;
end;

{$ENDIF WINDOWS}


end.
