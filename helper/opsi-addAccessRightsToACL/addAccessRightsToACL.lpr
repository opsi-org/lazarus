{This program is a prototype to test modifying Access Rights
of a file or directory under Windows, using the ACL Windows API

We use in this example :
AccessPermissions : GENERIC_ALL
AccessMode :  SET_ACCESS
Inheritance : SUB_CONTAINERS_AND_OBJECTS_INHERIT }


program addAccessRightsToACL;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  JwaWindows;

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
  writeln('-- Entering AddingAccessRightsToACL function -- ');
  newACL := nil;
  pSD := @mySD;
  PExistingDacl := @ExistingDacl;
  Result := False;
  myDWord := GetNamedSecurityInfo(pAnsiChar(TargetPath), SE_FILE_OBJECT,
                DACL_SECURITY_INFORMATION, nil, nil, @PExistingDacl, nil, pSD);
  if myDWord = ERROR_SUCCESS then
  begin
    writeln('First Success 1/3 : GetNamedSecurityInfo ');
    BuildExplicitAccessWithName(@ExplicitAccess, pAnsiChar(TrusteeName),
                                    AccessPermissions, AccessMode, Inheritance);
    //ExistingDacl := @ExistingDacl^;
    myDWord := SetEntriesInAcl(1, @ExplicitAccess, PExistingDacl, newACL);
    if myDWord = ERROR_SUCCESS then
    begin
      writeln('Second Success 2/3 : SetEntriesInAcl');
      try
        myDWord := SetNamedSecurityInfo(pAnsiChar(TargetPath),
              SE_FILE_OBJECT, DACL_SECURITY_INFORMATION, nil, nil, newACL, nil);
        if myDWord = ERROR_SUCCESS then
        begin
          writeln('Third Success 3/3 : SetNamedSecurityInfo');
          Result := True;
          //if Assigned(newACL) then Free
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
  writeln('-- Finishing AddingAccessRightsToACL function -- ');
end;

var
  user : String;
  TargetPath: String;
  userPos : integer;

begin
  //user := 'Jinene';

  //Testing on a file:

  TargetPath :=
   'C:\Users\Jinene\Documents\gituib\lazarus\helper\opsi-addAccessRightsToACL\tests\fileAccessRights-test.txt';

  if FileExists(TargetPath) then
  begin

  userPos := Pos(':\Users\',TargetPath)+8;
  if userPos > 8 then
     user := copy(TargetPath, userPos, Pos('\', copy(TargetPath, userPos, Length(TargetPath)- 9))-1)
  else
     writeln('Retrieving userProfile from FilePath failed');

  if fileOpen(TargetPath, fmOpenReadWrite) = THandle(-1) then
  begin
    writeln('No access rights : fileOpenReadWrite returned error THandle(-1)');
    if AddAccessRightsToACL(TargetPath, user, GENERIC_ALL, SET_ACCESS,
                    SUB_CONTAINERS_AND_OBJECTS_INHERIT) = True then
      if (fileOpen(TargetPath, fmOpenReadWrite) <> THandle(-1)) then
         writeln('Access rights modified : fileOpenReadWrite succeeded !')
      else
        writeln('Access rights not modified : fileOpenReadWrite returned error THandle(-1)');
  end
  else
    writeln('Already have access rights : fileOpen without error');

  end
  else
    writeln('Target file : '+ TargetPath+' does not exist');

end.

