unit oscertificates;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  windows,
  jwawincrypt;

function open_cert_store(store_name: string) : boolean;
function close_cert_store(hstore : HCERTSTORE) : boolean;
//function read_cert() : CERT_CONTEXT;
function install_ca(hstore : HCERTSTORE; ca_cert : CERT_CONTEXT) : boolean;
function remove_ca(hstore : HCERTSTORE; ca_cert_name : string) : boolean;

implementation


function open_cert_store(store_name: string) : HCERTSTORE;
begin
   try
   result := CertOpenStore(CERT_STORE_PROV_SYSTEM,0,NULL, CERT_SYSTEM_STORE_LOCAL_MACHINE or CERT_STORE_OPEN_EXISTING_FLAG,pchar(store_name));
   if result = nil then
   begin
      RaiseLastWin32Error;
   end
   except
     RaiseLastWin32Error;
   end;
end;

function close_cert_store(hstore : HCERTSTORE) : boolean;
begin
   if CertCloseStore(hstore,CERT_CLOSE_STORE_FORCE_FLAG) then
   result := true
   else result := false;
end;

function read_cert() : CERT_CONTEXT;
begin
  // result := CertCreateCertificateContext( );
end;

function install_ca(hstore : HCERTSTORE; ca_cert : CERT_CONTEXT) : boolean;
var
 Pca_cert : PCCERT_CONTEXT;
begin
  Pca_cert := @ca_cert;
  result := CertAddEncodedCertificateToStore(hstore,X509_ASN_ENCODING,ca_cert.pbCertEncoded,ca_cert.cbCertEncoded,CERT_STORE_ADD_REPLACE_EXISTING,@Pca_cert);
end;

function remove_ca(hstore : HCERTSTORE; ca_cert_name : string) : boolean;
var
 ca_cert : CERT_CONTEXT;
  Pca_cert : PCCERT_CONTEXT;
  cbsize : DWORD;
begin
  result := false;
    Pca_cert := @ca_cert;
  Pca_cert := CertFindCertificateInStore(hstore,X509_ASN_ENCODING,0,
            CERT_FIND_SUBJECT_STR,pchar(ca_cert_name),Pca_cert);
  if Pca_cert = nil then
  begin
     // failed
     result := false;
  end
  else
  begin

    cbsize := CertGetNameStringW(Pca_cert,
                                 CERT_NAME_FRIENDLY_DISPLAY_TYPE,
                                 0,nil,nil,0);
    if CertDeleteCertificateFromStore(Pca_cert) then
    begin
     // failed
  end;
    CertFreeCertificateContext(Pca_cert);
    result := true;
  end;
end;

end.

