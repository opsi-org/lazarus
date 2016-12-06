program ip2mac;

{$MODE Delphi}
{$H+}

{$APPTYPE CONSOLE}

//code derivated from:
//http://theroadtodelphi.wordpress.com/2011/04/21/accesing-the-wmi-from-delphi-and-fpc-via-com-without-late-binding-or-wbemscripting_tlb/


uses
  Windows,
  Variants,
  SysUtils,
  ActiveX,
  JwaWbemCli;

const
  RPC_C_AUTHN_LEVEL_DEFAULT = 0;
  RPC_C_IMP_LEVEL_IMPERSONATE = 3;
  RPC_C_AUTHN_WINNT = 10;
  RPC_C_AUTHZ_NONE = 0;
  RPC_C_AUTHN_LEVEL_CALL = 3;
  EOAC_NONE = 0;


procedure IWbemServices_ExecQuery;
const
  strLocale    = '';
  strUser      = '';
  strPassword  = '';
  strNetworkResource = 'root\cimv2';
  strAuthority       = '';
  WQL                = 'Select Description, IPAddress, MacAddress from Win32_NetworkAdapterConfiguration';
var
  FWbemLocator         : IWbemLocator;
  FWbemServices        : IWbemServices;
  FUnsecuredApartment  : IUnsecuredApartment;
  ppEnum               : IEnumWbemClassObject;
  apObjects            : IWbemClassObject;
  puReturned           : ULONG;
  pDesc, pIP, pMac, pVal     : OleVariant;
  pType                : Integer;
  plFlavor             : Integer;
  Succeed              : HRESULT;
  I : Integer;
begin
  // Set general COM security levels --------------------------
  // Note: If you are using Windows 2000, you need to specify -
  // the default authentication credentials for a user by using
  // a SOLE_AUTHENTICATION_LIST structure in the pAuthList ----
  // parameter of CoInitializeSecurity ------------------------
  if Failed(CoInitializeSecurity(nil, -1, nil, nil, RPC_C_AUTHN_LEVEL_DEFAULT, RPC_C_IMP_LEVEL_IMPERSONATE, nil, EOAC_NONE, nil)) then Exit;
  // Obtain the initial locator to WMI -------------------------
  if Succeeded(CoCreateInstance(CLSID_WbemLocator, nil, CLSCTX_INPROC_SERVER, IID_IWbemLocator, FWbemLocator)) then
  try
    // Connect to WMI through the IWbemLocator::ConnectServer method
    if Succeeded(FWbemLocator.ConnectServer(strNetworkResource, strUser, strPassword, strLocale,  WBEM_FLAG_CONNECT_USE_MAX_WAIT, strAuthority, nil, FWbemServices)) then
    try
      // Set security levels on the proxy -------------------------
      if Failed(CoSetProxyBlanket(FWbemServices, RPC_C_AUTHN_WINNT, RPC_C_AUTHZ_NONE, nil, RPC_C_AUTHN_LEVEL_CALL, RPC_C_IMP_LEVEL_IMPERSONATE, nil, EOAC_NONE)) then Exit;
      if Succeeded(CoCreateInstance(CLSID_UnsecuredApartment, nil, CLSCTX_LOCAL_SERVER, IID_IUnsecuredApartment, FUnsecuredApartment)) then
      try
        // Use the IWbemServices pointer to make requests of WMI
        //Succeed := FWbemServices.ExecQuery('WQL', WQL, WBEM_FLAG_FORWARD_ONLY OR WBEM_FLAG_RETURN_IMMEDIATELY, nil, ppEnum);
        Succeed := FWbemServices.ExecQuery('WQL', WQL, WBEM_FLAG_FORWARD_ONLY, nil, ppEnum);
        if Succeeded(Succeed) then
        begin
           // Get the data from the query
           while (ppEnum.Next(WBEM_INFINITE, 1, apObjects, puReturned)=0) do
           begin
             apObjects.Get('description', 0, pDesc, pType, plFlavor);
             apObjects.Get('IPAddress', 0, pIP, pType, plFlavor);
             apObjects.Get('MacAddress', 0, pMac, pType, plFlavor);

             if VarIsArray(pIp) then
             begin
               I := VarArrayLowBound(pIp,1);
               if VarIsStr(VarArrayGet(pIp,[I])) then
               begin
                 writeln(VarToStr(VarArrayGet(pIp,[I]))+' '+VarToStr(pMac));
                (*
                writeln('IPAddress: '+ VarToStr(VarArrayGet(pIp,[I])));
                writeln('MacAddress: '+  VarToStr(pMac));
                writeln('description: '+  VarToStr(pDesc));
                *)
               end;
             end;
             VarClear(pDesc);
             VarClear(pIp);
             VarClear(pMac);
           end;
        end
        else
        Writeln(Format('Error executing WQL sentence %x',[Succeed]));
      finally
        FUnsecuredApartment := nil;
      end;
    finally
      FWbemServices := nil;
    end;
  finally
    FWbemLocator := nil;
  end;
end;

begin
  try
      // Initialize COM. ------------------------------------------
    if Succeeded(CoInitializeEx(nil, COINIT_MULTITHREADED)) then
    try
      IWbemServices_ExecQuery;
    finally
      CoUninitialize();
    end;
  except
      on E:Exception do
      writeln('Error in ip2mac: '+E.Message);
  end;
  exit;
end.
