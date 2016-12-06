unit jclexcerpt;
// This code is part of the opsi.org project

// This unit contains excerpts from different JCL units
// (because I failed in comiling the complete library at lazarus):
// jclsecurity: SetUserObjectFullAccess,   GetUserObjectName
// jclstrings: StrResetLength
// Part of these functions are modified
// detlef oertel (uib gmbh / opsi.org) 21.10.2011

// The original copyright disclaimer of the jcl library is:
{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclSecurity.pas.                                                            }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel van Brakel.                                 }
{ Portions created by Marcel van Brakel are Copyright (C) Marcel van Brakel. All Rights Reserved.  }
{                                                                                                  }
{ Contributor(s):  see original files                                                              }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Various NT security related routines to perform commen asks such as enabling and disabling       }
{ privileges.                                                                                      }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}


{$mode delphi}
{$RANGECHECKS ON}
{$OVERFLOWCHECKS ON}
{$IOCHECKS ON}
{$OBJECTCHECKS ON}
{$VARSTRINGCHECKS ON}
{$LONGSTRINGS ON}


interface

uses
  Classes, SysUtils,
  DSiWin32,
  JwaWindows,
  LCLIntf;


function SetUserObjectFullAccess(hUserObject: THandle): Boolean;
function GetUserObjectName(hUserObject: THandle): string;
procedure StrResetLength(var S: WideString); overload;
procedure StrResetLength(var S: AnsiString); overload;


implementation

procedure StrResetLength(var S: WideString);
var
  I: SizeInt;
begin
  for I := 0 to Length(S) - 1 do
    if S[I + 1] = #0 then
    begin
      SetLength(S, I);
      Exit;
    end;
end;

procedure StrResetLength(var S: AnsiString);
var
  I: SizeInt;
begin
  for I := 0 to Length(S) - 1 do
    if S[I + 1] = #0 then
    begin
      SetLength(S, I);
      Exit;
    end;
end;


function SetUserObjectFullAccess(hUserObject: THandle): Boolean;
var
  Sd: PSecurity_Descriptor;
  Si: Security_Information;
begin
//  Result := not IsWinNT;
//  if Result then  // Win9x/ME
//    Exit;
  { TODO : Check the success of called functions }
  Sd := PSecurity_Descriptor(LocalAlloc(LPTR, SECURITY_DESCRIPTOR_MIN_LENGTH));
  InitializeSecurityDescriptor(Sd, SECURITY_DESCRIPTOR_REVISION);
  SetSecurityDescriptorDacl(Sd, True, nil, False);

  Si := DACL_SECURITY_INFORMATION;
  Result := SetUserObjectSecurity(hUserObject, Si, Sd);

  LocalFree(HLOCAL(Sd));
end;

function GetUserObjectName(hUserObject: THandle): string;
var
  Count: DWORD;
begin
//  if IsWinNT then
  begin
    // have the API function determine the required string length
    Count := 0;
    Result := '';
    GetUserObjectInformation(hUserObject, UOI_NAME, PChar(Result), 0, Count);
    SetLength(Result, Count + 1);

    if GetUserObjectInformation(hUserObject, UOI_NAME, PChar(Result), Count, Count) then
      StrResetLength(Result)
    else
      Result := '';
  end;
  //else
  //  Result := '';
end;


end.

