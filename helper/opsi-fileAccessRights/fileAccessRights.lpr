program fileAccessRights;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  JwaWindows;

function AddFileACL(Filename, TrusteeName: AnsiString; AccessMode: ACCESS_MODE;
    Inheritance: dWord): boolean; stdcall;
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
  newACL := nil;
  pSD := @mySD;
  PExistingDacl := @ExistingDacl;
  Result := False;
  myDWord := GetNamedSecurityInfo(pAnsiChar(Filename), SE_FILE_OBJECT,
                DACL_SECURITY_INFORMATION, nil, nil, @PExistingDacl, nil, pSD);
  if myDWord = ERROR_SUCCESS then
  begin
    writeln('First Success');
    BuildExplicitAccessWithName(@ExplicitAccess, pAnsiChar(TrusteeName),
                                        GENERIC_ALL, AccessMode, Inheritance);
    //ExistingDacl := @ExistingDacl^;
    myDWord := SetEntriesInAcl(1, @ExplicitAccess, PExistingDacl, newACL);
    if myDWord = ERROR_SUCCESS then
    begin
      writeln('Second Success');
      try
        myDWord := SetNamedSecurityInfo(pAnsiChar(Filename),
              SE_FILE_OBJECT, DACL_SECURITY_INFORMATION, nil, nil, newACL, nil);
        if myDWord = ERROR_SUCCESS then
        begin
          writeln('Third Success');
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
end;

var
  user : String;
  fileName: String;

begin
  user := 'Jinene';
  fileName :=
    'C:\Users\Jinene\Documents\gituib\lazarus\helper\opsi-fileAccessRights\fileAccessRights-test.txt';

  if fileOpen(fileName, fmOpenReadWrite) = THandle(-1) then
  begin
    writeln('fileOpen returned error: THandle(-1)');
    if AddFileACL(fileName, user, GRANT_ACCESS,
                    SUB_CONTAINERS_AND_OBJECTS_INHERIT) = True then
      writeln('It works!')
    else
      writeln('Doesnt work!');
  end
  else
    writeln('fileOpen without error');
end.
