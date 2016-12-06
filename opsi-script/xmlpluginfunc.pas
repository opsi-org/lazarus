unit xmlpluginfunc;


interface

uses
classes,
sysutils,
pluginlog;


type
TCharset = Set of Char;

  TStatement = (tsNotDefined,
                (* start of sectionnames *)
                tsActions,
                tsPatchAnyTextFile,
                tsTests, tsPatchIniFile,
                tsHostsPatch, tsRegistryHack, tsXMLPatch, tsIdapiConfig, tsLDAPsearch,
                tsFileActions, tsLinkFolder,
                tsWinBatch, tsDOSBatchFile, tsDOSInAnIcon,
                tsShellBatchFile, tsShellInAnIcon, tsExecutePython,
								tsExecuteWith, tsExecuteWith_escapingStrings,
								tsOpsiServiceCall,
								tsOpsiServiceHashList,
                tsDDEwithProgman,  // end of section names
                (* start of other commands *)
                tsWorkOnStringList,
				//tsTestCommand,
                tsStayWhileWindowOpen,
                tsCondOpen, tsCondThen, tsCondElse, tsCondClose,
                tsLoopStringList,
                tsMessage, tsMessageFile, tsShowBitmap,
                tsKillTask,
                tsPause, tsSleep, tsComment,tsLogWarning, tsLogError, tsSetSkinDir,
                tsStop, tsExitWindows,
                tsAddConnection,
								tsSetOldLogLevel,
								tsSetLogLevel,
								tsSetOutputLevel,
                tsSetExitOnError,
                tsSetFatalError, tsSetMarkerErrorNumber,
                tsSetReportMessages, tsSetTimeMark, tsGetDiffTime,
                tsSetTraceMode, tsSetStayOnTop,
                tsIconizeWinst, tsRestoreWinst, tsWinstVersionRequired,
								tsDefineVar, tsDefineStringList, tsSetVar);

  TStatementNames = Array [TStatement] of String [50];
  TPStatementNames = ^TStatementNames;



  TXStringList = class (TStringList)
  public
    function GlobalReplace (Startline : Integer; Const SearchItem, Replaceitem : String; MatchCase : Boolean) : Boolean;
    procedure EliminateLinesStartingWith(const startS : String; MatchCase : Boolean);
    procedure SaveToFile(const FileName: string); override;
    procedure loadFromUnicodeFile (const Filename: string; codepage: Word);
	function getStringValue(const keyname : String) : String;
    // returns the string value of a handmade properties list with separator either '=' or ':'
    // we return values[keyname], until further notice
  end;

  TEditScript = class (TXStringList)
  protected
    procedure SetString (const i : Integer; const s : String);
    (* sets the string with number i, if i < count
       else add the string to the string list *)
  public
  end;


  TuibIniScript = class (TEditScript)
  protected
    function FindEndOfSectionIndex (const OffLine: Integer) : Integer;
    function FindSectionheaderIndex (const Sectionname: String) : Integer;
  public
    procedure GetSectionTitles (var resultlist: TXStringlist);
    procedure GetSectionNames (var resultlist: TXStringlist);
    function GetSectionLines (const Sectionname: string; var Resultlist: TXStringList;
                               var StartlineNo : Integer;
                               takeCommentLines, takeEmptyLines, trimmed: Boolean) : Boolean;
       (*
          reads alle lines of a sektion including the title into a txstringlist
          returns true if there is a non-empty section

        *)

    (*    Procedure ReadSection (const Sectionname: string; var Resultlist : TStringList);
          Liest alle Idents einer Sektion mit der Zeilenstruktur Ident=Value in Strings ein *)
	end;

TSectionResult   = Integer;


const
NULL_STRING_VALUE = 'NULL';
LineIsCommentChar = ';';
	CitMark          = '"';
	WordDelimiterSet0 = [' ', #9, '=', '[', ']'];
	WordDelimiterSetHosts = [' ', '#', #9];
	WordDelimiterSetDBAlias = [':', '='];
	WordDelimiterSet1 = [' ', #9, '=', '[', ']', '(', ')', '"', '''', ',', '+'];
	WordDelimiterSet3 = [' ', #9, '=', '[', ']', '(', ')', '"', '''', ',', '+', ':'];
	WordDelimiterSet2 = [' ', #9, '"', ''''];
	WordDelimiterWhiteSpace = [' ', #9];



procedure GetWord
	(const s : String; var Expression, Remaining : String; const WordDelimiterSet : TCharset);

function CutLeftBlanks (const s: String)  : String;
function CutRightBlanks (const s: String) : String;
function Skip (const partialS, S : String; var Remaining : String; var Error : String) : Boolean;
(* versucht partialS am Anfang von S zu eliminieren, loescht fuehrende Leerzeichen vom Rest  *)
function SkipA (const partialS, S : String; var Remaining : String; var Error : String) : Boolean;
function CreateTextfile (const FName : String; Var ErrorInfo : String) : Boolean;


implementation
/////////////////////////////////////////////////////////////////////////////

var
 Ident, Value,
 LogSCommand, LogS : String;

 function Skip (const partialS, S : String; var Remaining : String; var Error : String) : Boolean;
	// versucht partialS am Anfang von S zu eliminieren, loescht fuehrende Leerzeichen vom Rest;
	//   wird partialS nicht gefunden, ist Remaining = S
	var
		p2 : String;
	begin
		Remaining := S;
		if length (S) < length (partialS)
		then result := false
		else
		Begin
			p2 := copy (AnsiUpperCase (S), 1, length (partialS));
			if p2 <> AnsiUpperCase (PartialS)
			then result := false
			else
			Begin
				Remaining := copy (S, length (partialS)+1, length (S));
				Remaining := CutLeftBlanks (Remaining);
				result := true;
			End
		End;
		if result
		then
			Error := ''
		else
			Error := '"' + partialS + '" expected ';
	end;

  function SkipA (const partialS, S : String; var Remaining : String; var Error : String) : Boolean;
  (* versucht partialS am Anfang von S zu eliminieren, loescht NICHT die fuehrenden Leerzeichen vom Rest;
     wird partialS nicht gefunden, ist Remaining = S *)
  var
    p2 : String;
  begin
    Remaining := S;
    if length (S) < length (partialS)
    then result := false
    else
    Begin
      p2 := copy (AnsiUpperCase (S), 1, length (partialS));
      if p2 <> AnsiUpperCase (PartialS)
      then result := false
      else
      Begin
        Remaining := copy (S, length (partialS)+1, length (S));
        result := true;
      End
    End;
    if result
    then
      Error := ''
    else
      Error := '"' + partialS + '" expected ';
  end;
	

   function KappeBlanks (const s: String) : String;
  begin
    result := trim (s);
    (* result := CutLeftBlanks (s);
    result := CutRightBlanks (result); *)
	end;

  procedure GetWord (const s : String; var Expression, Remaining : String;
                     const WordDelimiterSet : TCharset);
    (* Expression ist Teilstring von s bis zu einem Zeichen von WordDelimiterSet (ausschliesslich),
       Remaining beginnt mit dem auf dieses Zeichen folgenden Zeichen, wobei fuehrender Whitespace
       eliminiert wird  *)

   var
     i : Integer;
     t : String;

   begin
		 t := s;
		 setLength (t, length (t));
     i := 1;

     while (i <= length (t)) and
            not (t [i] in WordDelimiterSet)
     do inc (i);

     Expression := copy (t, 1, i-1);
     Remaining := copy (t, i, length (t) - i +1);
     Remaining := CutLeftBlanks (Remaining);
	 end;

  function CutLeftBlanks (const s: String) : String;
  begin
    Result := s;
    while (length (Result) > 0) and ((Result[1]= ' ') or (Result [1] = #9))
    do system.delete (Result,1,1);
  end;


  function CutRightBlanks (const s: String) : String;
  begin
    Result := s;
    while (length (Result) > 0)
      and ((Result[length (Result)]= ' ') or (Result[length (Result)]= #9))
    do system.delete (Result,length (Result),1);
  end;

(* TXStringList *)

function ReplaceInLine (Const OldLine, SearchItem, Replaceitem : String; MatchCase : Boolean; Var NewLine : String) : Boolean;

Var
   remainder, searchline, searchUItem : String;
   position : Integer;

begin
   remainder := OldLine;
   NewLine := '';
   result := false;
   if MatchCase then
   Begin
     SearchLine := Remainder;
     SearchUItem := SearchItem;
   End
   else
   Begin
     SearchLine := AnsiUpperCase (Remainder);
     SearchUItem := AnsiUpperCase (SearchItem);
   End;

   Position := pos (searchUItem, SearchLine);
   if Position > 0
   then
   Begin
     Result := true;

     while (length (SearchLine) > 0) and (Position > 0)
     do
     Begin
       NewLine := NewLine + copy (Remainder, 1, Position - 1) + ReplaceItem;

       system.delete (SearchLine, 1, Position + length (searchUItem) - 1);
       system.delete (Remainder, 1, Position + length (searchUItem) - 1);

       Position := pos (searchUItem, SearchLine);
     End;

     NewLine := NewLine + Remainder;
   End;

end;

 function CreateTextfile (const FName : String; Var ErrorInfo : String) : Boolean;

 var
	 dirname  : String;
	 NullFile : Textfile;
 begin
   result := true;
   ErrorInfo := '';
   if not sysutils.FileExists (FName)
   then
   Begin
     dirname := ExtractFilePath (FName);
     if not (dirname = '')
        and not (* dirname is drivename *) ((dirname[2]=':') and (length (dirname)<= 3))
        and not DirectoryExists (dirname)
     then
     Begin
       try
         ForceDirectories (Dirname);
         ErrorInfo := 'Directory ' + dirname + ' did not exist und was created';
       except
         on e: exception
         do
         Begin
           ErrorInfo := 'Directory ' + dirname + ' could not be created. ("'
                        + e.message + '")';
           result := false;
         End;
       End;
     End;

     if result (* Directory exists *) then
     Begin
       try
         AssignFile (NullFile, Fname);
         rewrite (NullFile);
         CloseFile (NullFile);
       except
         on e: exception
         do
				 Begin
					 ErrorInfo := 'File ' + ExtractFileName (Fname)
                        + ' could not be created in Directory ' + dirname + '. ("'
                        + e.message + '")';
           result := false;
         End;
       End;
     End;
   End;
 End;



function TXStringList.GlobalReplace
   (Startline : Integer; Const SearchItem, Replaceitem : String; MatchCase : Boolean) : Boolean;
var
  i : Integer;
  found : Boolean;
  NewLine : String;
begin

 result := false;

 for i:= Startline to count
 do
    if ReplaceInLine (Strings [i-1], SearchItem, ReplaceItem, MatchCase, NewLine)
   then
   Begin
     Result := true;
     delete (i-1);
     insert (i-1, NewLine);
   End;

end;

procedure TXStringList.EliminateLinesStartingWith(const startS : String; MatchCase : Boolean);
var
   i : Integer;
   compareS : String;
   lineStart : String;
begin

    i := 0;
    if not matchCase 
    then compareS := LowerCase(startS)
    else  compareS := startS;

    while i < count
    do
    Begin
    
      lineStart := copy(Strings[i], 1, length(startS));

      if not MatchCase
      then lineStart := LowerCase(lineStart);

      if lineStart = compareS
      then
        delete (i)
      else
        inc(i);
        
   end;   

end;

procedure TXStringList.SaveToFile(const FileName: string);
begin
  try
    inherited SaveToFile (FileName);
    LogS := FileName + ' saved back' ;
    LogDatei.DependentAdd (LogS, LevelComplete);
  except
    on e: exception do
    begin
     LogS := e.message;

     LogS := 'Error: ' +  FileName + ' could not be saved back, error message: "' + LogS + '"';
     LogDatei.DependentAddError (LogS, BaseLevel);
    end
  end;
end;


function TXStringList.getStringValue(const keyname : String) : String;
{
  Var
    i : Integer;
    found : Boolean;
    separatorPosition : Integer;
    line : String;
    compareToKey : String;
    }
begin
    if indexOfName(keyname) = -1
    then
      result:= NULL_STRING_VALUE
    else
      result := values[keyname];
{
    i := 0;
    found := false;

    result := '';

    while (i < count) and not found do
    begin
      line := Strings[i];
      separatorPosition := pos('=', line);
      if separatorPosition < 0 then separatorPosition := pos(':', line);
      if separatorPosition > 0
      then
      begin
        compareToKey := lowercase(trim (copy(line, 1, separatorPosition-1)));
        if lowercase(keyname) = compareToKey
        then
        Begin
          result := copy (line, separatorPosition+1, length(line));
          found := true;
        end;
      end;

      inc(i);
    end

    // if not found, result remains ''
}

end;

procedure TXStringlist.loadFromUnicodeFile (const Filename: string; codepage: Word);
var
  ws : widestring;
begin
end;

(* TEditScript *)
procedure TEditScript.SetString (const i : Integer; const s : String);
begin
  if (i >= 0) and (i + 1 <= count)
  then
    Strings [i] := s
  else
    Add (s);
end;



(* TuibIniScript *)

function IsHeaderLine (const s : String) : boolean;
var
 TestS : String;
begin
  TestS := KappeBlanks (s);
  if (length (TestS) > 2)
     and (TestS [1] = '[')
     and (TestS [length (TestS)] = ']')
  then IsHeaderLine := true
  else IsHeaderLine := false;
end;


function TuibIniScript.FindEndOfSectionIndex (const OffLine: Integer) : Integer;
 (* Annahme : OffLine gehoert zur Sektion *)
var
 i:Integer;
 s : String;
 weitersuchen : Boolean;
 inBlock : boolean;
begin
  i:= OffLine;
  result := OffLine;
  inc (i);
  weitersuchen := true;
  inBlock := false;
  while weitersuchen and (i+1 <= count)
  do
  Begin
       s := Kappeblanks (Strings [i]);

       (*     versuch mit zuordnung eines Blocks zu einer Sektion
              funktioniert, aber nur auf einer Ebene
       if (s<>'') and (s[1] = '{')
       then
       Begin
         inBlock := true;
         s := copy(s,2,length(s)-1);
       End;

       if inBlock and (s<>'') and (s[length(s)] = '}')
       then
       Begin
         inBlock := false;
         s := copy(s, 1, length(s)-1);
       End;

       if inBlock
       then
       Begin
         inc(i);
         result := i; // the next line is candidate for being the last line of section + block
       End

       else *)
       begin
         if IsHeaderLine (s)
         then
         begin
           weitersuchen := false;
         end
         else
         begin
           if (s <> '') and (s[1] <> LineIsCommentChar)
           then
            (* i ist neuer Kandidat fuer Sektionsende *)
              result := i;
           inc (i);
         end;
       end;
  End;
end;


function TuibIniScript.FindSectionheaderIndex (const Sectionname: String) : Integer;
  var
  found : Boolean;
  i : Integer;
  s : String;
begin
  if count = 0
  then result := -1
  else
  Begin
    found := false;
    i := 1;
    while not found and (i <= count)
		do
    begin
			s := KappeBlanks (Strings [i-1]);
			if AnsiUpperCase (s) = '[' + AnsiUpperCase (Sectionname) + ']'
			then found := true
			else
				// look for old german Aktionen sections
				if (AnsiUpperCase (Sectionname) = 'ACTIONS') and (AnsiUpperCase (s) = '[AKTIONEN]')
				then found := true
				else inc (i);
    end;

    if found
    then result := i-1
    else result := -1;
  End;
end;

procedure TuibIniScript.GetSectionTitles (var Resultlist: TXStringList);
var
 i : Integer;
 s : String;
begin
  for i := 0 to count -1
  do
  begin
    s := trim (Strings[i]);
    if (length (s) > 1) and (s[1] = '[') and ( s[length(s)]  = ']' )
    then resultlist.add(s)
  end;
end;

procedure TuibIniScript.GetSectionNames (var Resultlist: TXStringList);
var
 i : Integer;
 s : String;
begin
  for i := 0 to count -1
  do
  begin
    s := trim (Strings[i]);
    if (length (s) > 1) and (s[1] = '[') and ( s[length(s)]  = ']' )
    then
      resultlist.add(copy(s,2,length(s)-2))
  end;
end;


function TuibIniScript.GetSectionLines
   (const Sectionname: string; var Resultlist: TXStringList;
    var StartlineNo : Integer; 
	takeCommentLines, takeEmptyLines, trimmed : Boolean) :Boolean;

var
  i, j, n : Integer;
  s : String;
  ersteZeileSuchen : Boolean;

Begin
  result := false;
  i := FindSectionheaderIndex (Sectionname);
  if i >= 0 then StartlineNo := i + 1 else StartlineNo := i;
  if     (i >= 0)      (* Sektionsheader existiert *)
    and  (i + 1 <= count - 1)   (* es gibt die i+1-te Zeile, also eine Zeile nach dem Sektionsheader *)
  then
  Begin
    inc (i);
      // Zeile nach dem Header
      // kommt da vielleicht schon der n?chste Header, die Sektion w?re also leer?

    ersteZeileSuchen := true;
    while (i <= count - 1) and ersteZeileSuchen
    do
    begin
      s := trim (Strings [i]);
      if (length(s) > 0) and (s[1] <> LineIsCommentChar)
      then
      Begin
        ersteZeileSuchen := false;
        if isHeaderLine(s)
        then
         result := false
        else //i ist Index der ersten Sektionszeile
         result := true;
      End
      else
        inc(i)
    end;

    if result then
    begin // Sektionsinhalt existiert
      n := FindEndOfSectionIndex (i);
      for j := i to n
      do
      begin
        s := KappeBlanks (Strings [j]);
        if s = ''
        then
        Begin
           if takeEmptyLines then Resultlist.Add (s);
        End
        else (* s <> '' *)
        Begin
          if takeCommentLines  or (s[1] <> LineIsCommentChar)
          then
          Begin
            (*
            if (s<> '') and (s[1] = '{')
            then
              s := copy (s,2, length(s) -1);

            if (s<> '') and (s[length(s)] = '}')
            then
              s := copy (s,1, length(s) -1);
            *)
            if trimmed then
              Resultlist.Add (s)
            else
 			        Resultlist.Add (cutRightBlanks (Strings [j]));

          End
        End;
      end;
    end //Sektionsinhalt existiert
  End
End;


end.
 