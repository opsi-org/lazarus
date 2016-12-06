unit linux_func;

interface
{$IFDEF LINUX}
uses
SysUtils, Classes, libc;

Const
  (* additive Konstanten für den Typ TcpSpecify *)
  cpFlat                       = 0;  (* keine Rekursion und keine Subdirectories,
                                      unbegrenztes OverWrite *)
  cpCreateEmptySubdirectories  = 1;
  cpRecursive                  = 2;
  cpVersionControl             = 4;  (* Overwrite bei Dateien mit Extension in ExtensionsForVersionsControl
                                      nur nach vorheriger Versionskontrolle *)
  cpDateControl                = 8;  (* kein Overwrite bei Dateien mit Extension EXE wenn Zieldatei jüngeren Datums *)
  cpUpdate                     = 16; (* kein Overwrite, wenn Zieldatei jünger oder gleich alt *)
  cpNoOverwrite                = 32; (* kein Overwrite *)
  cpExtractLHA                 = 64; (* beim Kopieren Lharc-komprimierte Dateien dekomprimieren *)


Type
  TcpSpecify = Byte;



function ValueOfEnvVar (Const VarName : String) : String;

Function linFileCopy
      (Const sourcefilename, targetfilename: String;
       preserveDate, preserveAttr : boolean;
       Var problem : string)
      : Boolean;

function linDereferenceLink(const name : string) : string;

function linksymbolic(const source : string; const dest : string) : boolean;



{$ENDIF}
implementation
{$IFDEF LINUX}
uses
wimain, wiglob;

var
LogS : String;

function ValueOfEnvVar (Const VarName : String) : String;
begin
 ValueOfEnvVar := GetEnvironmentVariable(VarName)
end;

Function linFileCopy
      (Const sourcefilename, targetfilename: String;
       preserveDate, preserveAttr : boolean;
       Var problem : string)
      : Boolean;
var
  NewFile: TFileStream;  OldFile: TFileStream;
  rv: Integer;
  statBuf : TStatBuf;
  perms : Cardinal;

begin
 linFileCopy := false;

 OldFile := TFileStream.Create(sourcefilename, fmOpenRead or fmShareDenyWrite);
 try
  NewFile := TFileStream.Create(targetfilename, fmCreate or fmShareExclusive);
  try
   NewFile.CopyFrom(OldFile, OldFile.Size);
   linFileCopy := true;
  finally
   FreeAndNil(NewFile);
  end;
 finally
  FreeAndNil(OldFile);
 end;
 if preserveAttr then
 begin
  rv := lstat( PChar( sourcefilename ), statBuf );
  if ( rv = -1 ) then
   LogDatei.DependentAddError ('Unable to stat '+sourcefilename, BaseLevel)
  else
  begin
   perms := statBuf.st_mode;
   rv := chmod( PChar( targetfilename ), perms );
   if ( rv = -1 ) then
   begin
    LogDatei.DependentAddError ('Unable to chmod '+targetfilename, BaseLevel)
   end;
  end;
 end;
 if preserveDate then
 begin
  FileSetDate(targetfilename,FileAge(sourcefilename));
 end
end;

Function FileCheckDate
         (Const Sourcefilename, Targetfilename: String; OverwriteIfEqual: Boolean) : Boolean;
 (* Assumption: Both files exist *)
  Var
   Handle : Integer;
   Date1, Date2 : LongInt;

begin
  Handle := fileopen (Sourcefilename, fmOpenRead);
  Date1 := filegetdate (Handle);
  fileclose (handle);
  Handle := fileopen (Targetfilename, fmOpenRead);
  Date2 := filegetdate (Handle);
  fileclose (handle);
  if (Date2 < Date1) or ((Date2 = Date1) and OverwriteIfEqual)
  then
  Begin
    Result := true;
  End
  else
  Begin
    Result := false;
  End
end;

function linDereferenceLink(const name : string) : string;
var
 rv: Integer;
 bufsize : integer;
 statBuf : TStatBuf;
 buffer: array[0..254] of char;
 linkedname : string;
begin
 bufsize := 255;
 if FileExists(name) then
 begin
  result := '';
  rv := lstat( PChar( name ), statBuf );
  if ( rv = -1 ) then
   LogDatei.DependentAddError ('Unable to stat '+name, BaseLevel)
  else
  begin
   if (S_ISLNK(statBuf.st_mode) = true) then
   begin
    rv := readlink(PChar( name ),buffer,bufsize);
    if (rv = -1) then
    begin
     //error
    end
    else
    begin
     buffer[rv] := #0;
     linkedname := string(buffer);
     result := linDereferenceLink(linkedname);
    end;
   end
   else result := name; // it is not a link
  end;
 end
 else
 begin
  LogDatei.DependentAddError ('File does not exist: '+name, BaseLevel);
  result :='';
 end;
end;

function linksymbolic(const source : string; const dest : string) : boolean;
begin
 if 0=symlink(PChar(source),PChar(dest)) then
 begin
  result := true;
 end
 else
 begin
  //EEXIST 	 Path2 already exists.
  //EACCES 	The requested operation requires writing in a directory with a mode that denies write permission.
  //EROFS 	The requested operation requires writing in a directory on a read-only file system.
  //ENOSPC 	The directory in which the entry for the symbolic link is being placed cannot be extended because there is no space left on the file system containing the directory.
  //EDQUOT 	The directory in which the entry for the new symbolic link is being placed cannot be extended or disk blocks could not be allocated for the symbolic link because the user's or group's quota of disk blocks on the file system containing the directory has been exhausted.
  result := false;
 end; 
end;

{$ENDIF}
end.
