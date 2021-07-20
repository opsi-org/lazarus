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
  ExplicitAccess: PEXPLICIT_ACCESS;
  ExistingDacl: PACL;
  NewACL: PACL;
  myACL: ACL;
  pSD: PSECURITY_DESCRIPTOR;
  errorstr: String;
  myDWord: DWord = 0;
begin
  NewACL := @myACL;
  pSD := nil;
  Result := False;
  if GetNamedSecurityInfo(pAnsiChar(Filename), SE_FILE_OBJECT,
    DACL_SECURITY_INFORMATION, nil, nil, @ExistingDacl, nil, pSD) = ERROR_SUCCESS then
  begin
    writeln('First Success');
    BuildExplicitAccessWithName(@ExplicitAccess, pAnsiChar(TrusteeName),
      GENERIC_ALL, AccessMode, Inheritance);
    ExistingDacl := @ExistingDacl^;
    if SetEntriesInAcl(1, @ExplicitAccess, ExistingDacl, NewACL) = ERROR_SUCCESS then
    begin
      writeln('Second Success');
      //if SetNamedSecurityInfo(pAnsiChar(Filename), SE_FILE_OBJECT,
      //   DACL_SECURITY_INFORMATION, nil, nil, NewACL, nil) = ERROR_SUCCESS then

      //
      try
        if FileExists(Filename) then
          myDWord := SetNamedSecurityInfo(pAnsiChar(Filename),
            SE_FILE_OBJECT, DACL_SECURITY_INFORMATION, nil, nil, NewACL, nil);

        if myDWord = ERROR_SUCCESS then
        begin
          writeln('Third Success');
          Result := True;
        end;
      except
        on e: Exception do
        begin
          errorstr := e.ClassName + ' -- ' + e.Message;
          writeln(errorstr);
        end;
      end;
      //
    end;
  end;
end;

var
  fileName: string;

begin
  fileName :=
    'C:\Users\Jinene\Documents\gituib\lazarus\helper\opsi-fileAccessRights\fileAccessRights-test.txt';
  if fileOpen(fileName, fmOpenReadWrite) = THandle(-1) then
  begin
    writeln('fileOpen returned error: THandle(-1)');
    if AddFileACL(fileName, 'Jinene', GRANT_ACCESS,
      SUB_CONTAINERS_AND_OBJECTS_INHERIT) = True then
      writeln('It works!')
    else
      writeln('Doesnt work!');
  end
  else
    writeln('fileOpen without error');
end.
