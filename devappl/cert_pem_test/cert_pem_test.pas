program cert_pem_test;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX}
  cthreads, {$ENDIF}
  Classes,
  oscertificates,
  jwawincrypt,
  SysUtils,
  lazfileutils;

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

  {$R manifest.rc}


begin
  bufsize := 4096;
  binaryBuffer := GetMem(bufsize);
  mybool := pemfileToBinarybuf(ExtractFilePath(ParamStr(0)) +
    '\opsi-ca-cert.pem', binaryBuffer, bufsize);
  //mybool := pemfileToEncodedline(ExtractFilePath(ParamStr(0)) + '\opsi-ca-cert.pem',mypem);
  if mybool then
  begin
    mybool := open_cert_system_store(mystore);
    if mybool then
    begin
      (*
      assignFile(myPemFile, ExtractFilePath(ParamStr(0)) + '\hexout.txt');
      rewrite(myPemFile);
      for i := 0 to bufsize do Write(myPemFile, chr(binaryBuffer[i]));
      CloseFile(myPemFile);
      *)
      mybool := install_ca(mystore, binaryBuffer, bufsize);
      if mybool then
      begin
        writeln('done');
      end
      else
      begin
        message := SysErrorMessage(GetLastOSError);
        writeln('failed');
      end;
    end
    else
      begin
        message := SysErrorMessage(GetLastOSError);
        writeln('failed');
      end;
  end
  else
      begin
        message := SysErrorMessage(GetLastOSError);
        writeln('failed');
      end;

end.
