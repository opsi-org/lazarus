unit oscertificates;

{$mode ObjFPC}{$H+}

(*
   *)


interface

uses
  Classes, SysUtils,
  fileutil,
  lazfileutils,
  {$IFDEF WINDOWS}
  Windows,
  jwawincrypt,
  osfuncwin,
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

type
  TlinCertRec = record
    pathToStore, storeCommand: string;
    masterCrt: string; // where we find all known certs
  end;


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
function removeCertFromSystemStore(labelstr: string): boolean;
function listCertificatesFromSystemStore(): TStringList;
function isCertInstalledInSystemStore(labelstr: string): boolean;

implementation

{$IFDEF DARWIN}
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
#######   #######
// macos
// https://apple.stackexchange.com/questions/80623/import-certificates-into-the-system-keychain-via-the-command-line
// https://derflounder.wordpress.com/2011/03/13/adding-new-trusted-root-certificates-to-system-keychain/
https://ss64.com/osx/security.html
https://ss64.com/osx/security-find-cert.html
https://ss64.com/osx/security-delete-cert.html
https://ss64.com/osx/security-cert.html
*)

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
    // sudo security add-trusted-cert -d -r trustRoot -k /Library/Keychains/System.keychain <new-root-certificate>
    command :=
      'security add-trusted-cert -d -r trustRoot -k /Library/Keychains/System.keychain ';
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
      begin
        logdatei.log('Successful imported to system store: ' + filename, LLinfo);
        Result := True;
      end
      else
        logdatei.log('pemfileToSystemStore: failed: update store command with exitcode: '
          + IntToStr(exitcode), LLError);
    end;
  finally
    logdatei.log_list(outlines, LLInfo);
    FreeAndNil(outlines);
  end;
end;

function listCertificatesFromSystemStore(): TStringList;
begin
  Result := TStringList.Create;
end;

function removeCertFromSystemStore(labelstr: string): boolean;
begin
  Result := False;
end;

function isCertInstalledInSystemStore(labelstr: string): boolean;
begin
  Result := False;
end;


{$ENDIF DARWIN}

{$IFDEF LINUX}
// Ubuntu
// https://askubuntu.com/questions/73287/how-do-i-install-a-root-certificate
// https://wiki.ubuntuusers.de/CA/
(*
https://unix.stackexchange.com/questions/97244/list-all-available-ssl-ca-certificates
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


function getLinuxCertInfos(): TlinCertRec;
var
  distrotype: string;
begin
  Result.pathToStore := '';
  Result.storeCommand := '';
  Result.masterCrt := '';
  distrotype := getLinuxDistroType;
  if distrotype = 'debian' then
  begin
    Result.pathToStore := '/usr/local/share/ca-certificates/';
    Result.storeCommand := 'update-ca-certificates';
    Result.masterCrt := '/etc/ssl/certs/ca-certificates.crt';
  end
  else if distrotype = 'suse' then
  begin
    Result.pathToStore := '/usr/share/pki/trust/anchors/';
    Result.storeCommand := 'update-ca-certificates';
    Result.masterCrt := '/etc/ssl/certs/ca-certificates.crt';
  end
  else if distrotype = 'redhat' then
  begin
    Result.pathToStore := '/etc/pki/ca-trust/source/anchors/';
    Result.storeCommand := 'update-ca-trust';
    Result.masterCrt := '/etc/ssl/certs/ca-bundle.crt';
  end;
end;


function pemfileToSystemStore(filename: string): boolean;
var
  command: string;

  report: string;
  ExitCode: longint;
  targetfile, certExt: string;
  {$IFDEF OPSISCRIPT}
  outlines: TXStringList;
  {$ELSE OPSISCRIPT}
  outlines: TStringList;
  {$ENDIF OPSISCRIPT}
  linCertRec: TlinCertRec;
begin
  Result := False;
  try
  {$IFDEF OPSISCRIPT}
    outlines := TXStringList.Create;
  {$ELSE OPSISCRIPT}
    outlines := TStringList.Create;
  {$ENDIF OPSISCRIPT}
    linCertRec := getLinuxCertInfos();
    targetfile := linCertRec.pathToStore + ExtractFileNameOnly(filename) + '.crt';
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
      if not CopyFile(filename, targetfile) then
      begin
        // Error
        logdatei.log('pemfileToSystemStore: failed: copy  ', LLError);
      end
      else
      begin
        // success
        logdatei.log_prog('pemfileToSystemStore: Successful copy to : ' +
          targetfile, LLinfo);
      end;
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

    command := linCertRec.storeCommand;

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
      begin
        logdatei.log('Successful imported to system store: ' + filename, LLinfo);
        Result := True;
      end
      else
        logdatei.log('pemfileToSystemStore: failed: update store command with exitcode: '
          + IntToStr(exitcode), LLError);
    end;
  finally
    FreeAndNil(outlines);
  end;
end;

function listCertificatesFromSystemStore(): TStringList;
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
  tmplines: TStringArray;
  linCertRec: TlinCertRec;
  i, k: integer;
  tmpstr: string;

begin
  linCertRec := getLinuxCertInfos();
  try
  {$IFDEF OPSISCRIPT}
    outlines := TXStringList.Create;
  {$ELSE OPSISCRIPT}
    outlines := TStringList.Create;
  {$ENDIF OPSISCRIPT}
    Result := TStringList.Create;
    //tmplines:= TStringList.Create;
    command := 'awk -v cmd="openssl x509 -noout -subject" "/BEGIN/{close(cmd)};{print | cmd}"';
    command := command + ' < ' + linCertRec.masterCrt;
    command := '/bin/bash -c ''' + command + '''';

    if not RunCommandAndCaptureOut(command, True, outlines, report,
      SW_HIDE, ExitCode, False, 1) then
    begin
      // Error
      logdatei.log('listCertificates: failed: list certs with exitcode: ' +
        IntToStr(exitcode), LLError);
    end
    else
    begin
      // success
      if exitcode = 0 then
      begin
        for i := 0 to outlines.Count - 1 do
        begin
          tmplines := SplitString(outlines.Strings[i], ',');
          for k := 0 to length(tmplines) - 1 do
          begin
            tmpstr := trim(tmplines[k]);
            if AnsiStartsStr('CN =', tmpstr) then
            begin
              tmpstr := trim(tmpstr.Split('=')[1]);
              Result.Add(tmpstr);
            end;
          end;
        end;
      end
      else
        logdatei.log('listCertificates: failed: list certs with exitcode: ' +
          IntToStr(exitcode), LLError);
    end;
  finally
    FreeAndNil(outlines);
  end;
end;

function removeCertFromSystemStore(labelstr: string): boolean;
var
  command: string;

  report: string;
  showcmd: integer;
  ExitCode: longint;
  {$IFDEF OPSISCRIPT}
  outlines: TXStringList;
  {$ELSE OPSISCRIPT}
  outlines: TStringList;
  {$ENDIF OPSISCRIPT}
  linCertRec: TlinCertRec;
  certfilelist: TStringList;
  i, k: integer;
  tmpstr: string;
  tmplines: TStringArray;
begin
  Result := False;
  try
  {$IFDEF OPSISCRIPT}
    outlines := TXStringList.Create;
  {$ELSE OPSISCRIPT}
    outlines := TStringList.Create;
  {$ENDIF OPSISCRIPT}
    certfilelist := TStringList.Create;
    linCertRec := getLinuxCertInfos();

    command := '';
    certfilelist := FindAllFiles(linCertRec.pathToStore, '*.crt', False);
    for i := 0 to certfilelist.Count - 1 do
    begin
      command := 'openssl x509 -subject -noout -in ';
      command := command + certfilelist.Strings[i];
      logdatei.log_prog('starting: ' + command, LLInfo);
      if not RunCommandAndCaptureOut(command, True, outlines,
        report, SW_HIDE, ExitCode, False, 1) then
      begin
        // Error
        logdatei.log('listCertificates: failed: list certs with exitcode: ' +
          IntToStr(exitcode), LLError);
      end
      else
      begin
        // success
        if exitcode = 0 then
        begin
          if outlines.Count >= 1 then
          begin
            tmplines := SplitString(outlines.Strings[0], ',');
            for k := 0 to length(tmplines) - 1 do
            begin
              tmpstr := trim(tmplines[k]);
              if AnsiStartsStr('CN =', tmpstr) then
              begin
                tmpstr := trim(tmpstr.Split('=')[1]);
                if tmpstr = labelstr then  // we found it
                begin
                  //delete the file
                  if DeleteFile(certfilelist.Strings[i]) then
                  begin
                    logdatei.log('cert removed: ' + certfilelist.Strings[i], LLinfo);
                    Result := True;
                  end
                  else
                    logdatei.log('could nor remove: ' + certfilelist.Strings[i], LLError);
                end;
              end;
            end;
          end;
        end
        else
          logdatei.log('removeCertFromSystemStore: failed: list certs with exitcode: ' +
            IntToStr(exitcode), LLError);
      end;
    end;

    command := linCertRec.storeCommand;

    if not RunCommandAndCaptureOut(command, True, outlines, report,
      SW_HIDE, ExitCode, False, 1) then
    begin
      // Error
      logdatei.log('removeCertFromSystemStore: failed: update store command with exitcode: '
        + IntToStr(exitcode), LLError);
    end
    else
    begin
      // success
      if exitcode = 0 then
      begin
        logdatei.log('Successful updated system store: ', LLinfo);
        //Result := True;
      end
      else
        logdatei.log('removeCertFromSystemStore: failed: update store command with exitcode: '
          + IntToStr(exitcode), LLError);
    end;
  finally
    FreeAndNil(outlines);
    FreeAndNil(certfilelist);
  end;
end;

function isCertInstalledInSystemStore(labelstr: string): boolean;
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
  tmplines: TStringArray;
  linCertRec: TlinCertRec;
  i, k: integer;
  tmpstr: string;
begin
  linCertRec := getLinuxCertInfos();
  try
  {$IFDEF OPSISCRIPT}
    outlines := TXStringList.Create;
  {$ELSE OPSISCRIPT}
    outlines := TStringList.Create;
  {$ENDIF OPSISCRIPT}
    Result := False;
    //tmplines:= TStringList.Create;
    command := 'awk -v cmd="openssl x509 -noout -subject" "/BEGIN/{close(cmd)};{print | cmd}"';
    command := command + ' < ' + linCertRec.masterCrt;
    command := '/bin/bash -c ''' + command + '''';

    if not RunCommandAndCaptureOut(command, True, outlines, report,
      SW_HIDE, ExitCode, False, 1) then
    begin
      // Error
      logdatei.log('listCertificates: failed: list certs with exitcode: ' +
        IntToStr(exitcode), LLError);
    end
    else
    begin
      // success
      if exitcode = 0 then
      begin
        for i := 0 to outlines.Count - 1 do
        begin
          tmplines := SplitString(outlines.Strings[i], ',');
          for k := 0 to length(tmplines) - 1 do
          begin
            tmpstr := trim(tmplines[k]);
            if AnsiStartsStr('CN =', tmpstr) then
            begin
              tmpstr := trim(tmpstr.Split('=')[1]);
              if tmpstr = labelstr then
              begin
                Result := True;
                break;
              end;
            end;
          end;
        end;
      end
      else
        logdatei.log('isCertInstalledInSystemStore: failed: list certs with exitcode: ' +
          IntToStr(exitcode), LLError);
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
  //Pca_cert := @ca_cert;
  Pca_cert := nil;
  Pca_cert := CertFindCertificateInStore(hstore, X509_ASN_ENCODING or
    PKCS_7_ASN_ENCODING, 0, CERT_FIND_SUBJECT_STR, PChar(ca_cert_name), Pca_cert);
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
      // success
      Result := True;
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
  //ca_cert: CERT_CONTEXT;
  mybool: boolean;
  //mypem: string;
  message: string;
  i: integer;
  //myPemFile: textfile;
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

function listCertificatesFromSystemStore(): TStringList;
var
  command: string;

  report: string;
  //showcmd: integer;
  ExitCode: longint;
  //distrotype, pathToStore, storeCommand: string;
  //targetfile, certExt: string;
  {$IFDEF OPSISCRIPT}
  outlines: TXStringList;
  {$ELSE OPSISCRIPT}
  outlines: TStringList;
  {$ENDIF OPSISCRIPT}
  tmplines: TStringArray;
  //linCertRec: TlinCertRec;
  i, k: integer;
  tmpstr: string;

begin
  // powershell -command  "get-childitem -path cert:\currentuser\root\ | select subject
  try
  {$IFDEF OPSISCRIPT}
    outlines := TXStringList.Create;
  {$ELSE OPSISCRIPT}
    outlines := TStringList.Create;
  {$ENDIF OPSISCRIPT}
    Result := TStringList.Create;
    //tmplines:= TStringList.Create;
    command :=
      'powershell -command  "get-childitem -path cert:\localmachine\root\ | select subject';

    if not RunCommandAndCaptureOut(command, True, outlines, report,
      SW_HIDE, ExitCode, False, 1) then
    begin
      // Error
      logdatei.log('listCertificates: failed: list certs with exitcode: ' +
        IntToStr(exitcode), LLError);
    end
    else
    begin
      // success
      if exitcode = 0 then
      begin
        for i := 0 to outlines.Count - 1 do
        begin
          tmplines := SplitString(outlines.Strings[i], ',');
          for k := 0 to length(tmplines) - 1 do
          begin
            tmpstr := trim(tmplines[k]);
            if AnsiStartsStr('CN=', tmpstr) then
            begin
              tmpstr := trim(tmpstr.Split('=')[1]);
              Result.Add(tmpstr);
            end;
          end;
        end;
      end
      else
        logdatei.log('listCertificates: failed: list certs with exitcode: ' +
          IntToStr(exitcode), LLError);
    end;
  finally
    FreeAndNil(outlines);
  end;
end;

(*
function removeCertFromSystemStore(labelstr: string): boolean;
var
  mystore: HCERTSTORE;
  //binaryBuffer: pbyte;
  //bufsize: dword;
  //ca_cert: CERT_CONTEXT;
  mybool: boolean;
  //mypem: string;
  message: string;
//  i: integer;
  //myPemFile: textfile;
begin
  Result := False;
  //bufsize := 4096;
  //binaryBuffer := GetMem(bufsize);
  mybool := open_cert_system_store(mystore);
  if not mybool then
  begin
    message := SysErrorMessage(GetLastOSError);
    logdatei.log('removeCertFromSystemStore: failed: open_cert_system_store: ' +
      message, LLError);
    exit;
  end;
  logdatei.log_prog('done: open_cert_system_store', LLdebug);
  mybool := remove_ca(mystore, labelstr);
  if not mybool then
  begin
    message := SysErrorMessage(GetLastOSError);
    logdatei.log('removeCertFromSystemStore: failed: remove_ca: ' + message, LLError);
    exit;
  end;
  logdatei.log_prog('done: remove_ca', LLdebug);
  mybool := close_cert_store(mystore);
  if not mybool then
  begin
    message := SysErrorMessage(GetLastOSError);
    logdatei.log('pemfileToSystemStore: failed: close_cert_store: ' + message, LLError);
    exit;
  end;
  logdatei.log_prog('done: close_cert_store', LLdebug);
  logdatei.log_prog('done: removeCertFromSystemStore', LLdebug);
  logdatei.log('Successful removed from system store: ' + labelstr, LLinfo);
  Result := mybool;
end;
*)

function removeCertFromSystemStore(labelstr: string): boolean;
var
  command: string;
  report: string;
  //showcmd: integer;
  ExitCode: longint;
  //distrotype, pathToStore, storeCommand: string;
  //targetfile, certExt: string;
  {$IFDEF OPSISCRIPT}
  outlines: TXStringList;
  {$ELSE OPSISCRIPT}
  outlines: TStringList;
  {$ENDIF OPSISCRIPT}
  //tmplines: TStringArray;
  //linCertRec: TlinCertRec;
  i, k: integer;
  // tmpstr: string;

begin
  // powershell -command  "get-childitem -path cert:\currentuser\root\ | where DNSNameList -eq
  try
  {$IFDEF OPSISCRIPT}
    outlines := TXStringList.Create;
  {$ELSE OPSISCRIPT}
    outlines := TStringList.Create;
  {$ENDIF OPSISCRIPT}
    Result := False;
    //tmplines:= TStringList.Create;
    command := 'powershell -command  "get-childitem -path cert:\localmachine\root\';
    command := command + ' | where DNSNameList -eq ''' + labelstr +
      ''' | select   -expandProperty thumbprint';
    logdatei.log_prog('starting: ' + command, LLInfo);

    if not RunCommandAndCaptureOut(command, True, outlines, report,
      SW_HIDE, ExitCode, False, 1) then
    begin
      // Error
      logdatei.log('removeCertFromSystemStore: failed: list certs with exitcode: ' +
        IntToStr(exitcode), LLError);
    end
    else
    begin
      // success
      if exitcode = 0 then
      begin
        if outlines.Count > 0 then
        begin
          for i := 0 to outlines.Count - 1 do
          begin
            command := 'powershell -command  "remove-item -path cert:\localmachine\root\';
            command := command + outlines[i] + ' -verbose"';
            logdatei.log_prog('starting: ' + command, LLInfo);

            if not RunCommandAndCaptureOut(command, True, outlines, report,
              SW_HIDE, ExitCode, False, 1) then
            begin
              // Error
              logdatei.log('removeCertFromSystemStore: failed: remove cert with exitcode: ' +
                IntToStr(exitcode), LLError);
            end
            else
            begin
              // success
              logdatei.log('removed cert with thumbprint: ' + outlines[i], LLInfo);
            end;
          end;
          Result := True;

        end;
      end
      else
        logdatei.log('removeCertFromSystemStore: failed: list certs with exitcode: ' +
          IntToStr(exitcode), LLError);
    end;
  finally
    FreeAndNil(outlines);
  end;
end;


//  | where DNSNameList -eq
function isCertInstalledInSystemStore(labelstr: string): boolean;
var
  command: string;
  report: string;
  //showcmd: integer;
  ExitCode: longint;
  //distrotype, pathToStore, storeCommand: string;
  //targetfile, certExt: string;
  {$IFDEF OPSISCRIPT}
  outlines: TXStringList;
  {$ELSE OPSISCRIPT}
  outlines: TStringList;
  {$ENDIF OPSISCRIPT}
  //tmplines: TStringArray;
  //linCertRec: TlinCertRec;
  // i, k: integer;
  // tmpstr: string;

begin
  // powershell -command  "get-childitem -path cert:\currentuser\root\ | where DNSNameList -eq
  try
  {$IFDEF OPSISCRIPT}
    outlines := TXStringList.Create;
  {$ELSE OPSISCRIPT}
    outlines := TStringList.Create;
  {$ENDIF OPSISCRIPT}
    Result := False;
    //tmplines:= TStringList.Create;
    command := 'powershell -command  "get-childitem -path cert:\localmachine\root\';
    command := command + ' | where DNSNameList -eq ''' + labelstr +
      ''' | select   -expandProperty subject';
    logdatei.log_prog('starting: ' + command, LLInfo);

    if not RunCommandAndCaptureOut(command, True, outlines, report,
      SW_HIDE, ExitCode, False, 1) then
    begin
      // Error
      logdatei.log('listCertificates: failed: list certs with exitcode: ' +
        IntToStr(exitcode), LLError);
    end
    else
    begin
      // success
      if exitcode = 0 then
      begin
        if outlines.Count > 0 then Result := True;
      end
      else
        logdatei.log('listCertificates: failed: list certs with exitcode: ' +
          IntToStr(exitcode), LLError);
    end;
  finally
    FreeAndNil(outlines);
  end;
end;


{$ENDIF WINDOWS}


end.
