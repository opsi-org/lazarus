unit oswinacl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  //oslog,
  JwaWindows;


function AddAccessRightsToACL(TargetPath, TrusteeName: AnsiString; AccessPermissions: DWord;
                AccessMode: ACCESS_MODE; Inheritance: DWord): boolean; stdcall;

implementation

{$IFDEF WINDOWS}
{This function adds Access Rights to a file or directory ACL under Windows.
When calling this function, use :
AccessPermissions : GENERIC_ALL
AccessMode :  SET_ACCESS
Inheritance : SUB_CONTAINERS_AND_OBJECTS_INHERIT

Example of calling this function :
if AddAccessRightsToACL(FilePath, 'SYSTEM', GENERIC_ALL, SET_ACCESS,
                     SUB_CONTAINERS_AND_OBJECTS_INHERIT ) = True then
}

function AddAccessRightsToACL(TargetPath, TrusteeName: AnsiString; AccessPermissions: DWord;
                AccessMode: ACCESS_MODE; Inheritance: DWord): boolean; stdcall;
var
  ExplicitAccess: EXPLICIT_ACCESS;
  ExistingDacl: ACL;
  PExistingDacl: PACL;
  newACL: PACL;
  mySD: SECURITY_DESCRIPTOR;
  pSD : PSECURITY_DESCRIPTOR;
  errorstr: String;
  myDWord: DWord = 1;
begin
  //logdatei.log('-- Entering AddAccessRightsToACL function -- ', LLinfo);
  newACL := nil;
  pSD := @mySD;
  PExistingDacl := @ExistingDacl;
  Result := False;
  myDWord := GetNamedSecurityInfo(pAnsiChar(TargetPath), SE_FILE_OBJECT,
                DACL_SECURITY_INFORMATION, nil, nil, @PExistingDacl, nil, pSD);
  if myDWord = ERROR_SUCCESS then
  begin
    //logdatei.log('First Success 1/3 : GetNamedSecurityInfo ', LLinfo);
    BuildExplicitAccessWithName(@ExplicitAccess, pAnsiChar(TrusteeName),
                                    AccessPermissions, AccessMode, Inheritance);
    myDWord := SetEntriesInAcl(1, @ExplicitAccess, PExistingDacl, newACL);
    if myDWord = ERROR_SUCCESS then
    begin
      //logdatei.log('Second Success 2/3 : SetEntriesInAcl', LLinfo);
      try
        myDWord := SetNamedSecurityInfo(pAnsiChar(TargetPath),
                SE_FILE_OBJECT, DACL_SECURITY_INFORMATION, nil, nil, newACL, nil);
        if myDWord = ERROR_SUCCESS then
        begin
          //logdatei.log('Third Success 3/3 : SetNamedSecurityInfo ', LLinfo);
          Result := True;
        end;
      except
        on e: Exception do
        begin
          errorstr := e.ClassName + ' -- ' + e.Message;
          writeln(errorstr);
        end;
      end;
    end;
  end;
  //logdatei.log('-- Finishing AddAccessRightsToACL function -- ', LLinfo);
end;
{$ENDIF WINDOWS}


end.
