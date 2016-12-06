unit xmlpluginsynth;

interface

uses
forms,
xmldom,
classes,
Contnrs,
SysUtils,
XMLIntf,
wixml,
xmlpluginfunc,
pluginlog;


Const
    tsrFatalError   = -2;
		tsrExitWindows  = -1;
    tsrExitProcess  =  0;
		tsrPositive     =  1;
		XMLDOMVendor = 'Open XML'; // 'Xerces XML'

type
TWorkSection = class (TuibIniScript) // class (TXStringList)
	private
		FSectionkind : TStatement;
		FStartLineNo    : Integer;
		FSectionName    : String;
		FNestingLevel   : Integer;

	public
		constructor create (const NestLevel : Integer);
		destructor destroy; override;

		property StartLineNo : Integer read FStartLineNo write FStartLineNo;
		property Name : String read FSectionName write FSectionName;
		property NestingLevel : Integer read FNestingLevel write FNestingLevel;
		property SectionKind : TStatement read FSectionKind write FSectionKind;

	end;


TuibXMLNodeDescription = class (TObject)
  private
    Fxmldoc : TuibXMLDocument;
    elementname : WideString;
    attributes : TStringList;
    text : WideString;
    positioning : boolean;
    positionOfElement : Integer;
    position : Integer;
  public
    constructor Create (xmldoc : TuibXMLDocument);
    destructor Destroy; override;

    function evaluate (s : String; var Remaining: String; var InfoSyntaxError : String) : Boolean;

    procedure evaluateElementname;
    procedure evaluateAttribute;
    procedure evaluateText;
 end;



TuibInstScript = class (TuibIniScript)
private
	FLogLevel : Integer;
	FOutputLevel : Integer;
  FNumberOfWarnings : Integer;
  FNumberOfErrors : Integer;
  FTraceMode : Boolean;
  FExitOnError : Boolean;
  FReportMessages : Boolean;
  FExtremeErrorLevel : Integer;

  FVarList : TStringList;
  listOfStringLists : TStringList;
  FValuesList : TStringList;
  ContentOfStringLists : TObjectList;
  FConstList : TStringList;
	FConstValuesList : TStringList;
	FLastExitCodeOfExe : LongInt;

  
protected
  function getVarValues : TStringList; //nicht verwendet

public
  constructor create;
  destructor destroy; override;

  (* Properties *)
	property LogLevel : Integer read FLogLevel write FLogLevel;
	property OutputLevel : Integer read FOutputLevel write FOutputLevel;
	property NumberOfWarnings : Integer read FNumberOfWarnings;
  property NumberOfErrors : Integer read FNumberOfErrors;
  property TraceMode : Boolean read FTraceMode write FTraceMode;
  property ExitOnError : Boolean read FExitOnError write FExitOnError;


	property ReportMessages : Boolean read FReportMessages write FReportMessages;

  property varList : TStringList read FvarList write FvarList;
  property valuesList : TStringList read Fvalueslist write FvaluesList;
  property constList : TStringList read FconstList write FConstList;
  property constValuesList : TStringList read FconstValuesList write FconstValuesList;

  (* Infofunktionen *)
  function doInfo (Meldung : String)                          : TSectionResult;
  function doLogEntries (const Lines : String; EntryLevel : Integer)
                                                              : TSectionResult;
  function reportError (const Sektion: TWorkSection; LineNo : Integer;
                        const Content : String; Comment: String) : TSectionResult;


	(* Skriptvariable setzen, Ausdruecke analysieren und auswerten *)
	 function EvaluateString
    (const s0 : String; var Remaining: String;
     var StringResult : String; var InfoSyntaxError : String ) : Boolean;

	procedure GetWordOrStringExpression (const s: String;
													 var resultString, Remaining, errorinfo : String);
	function doXMLAddNamespace(const filename:string;
		const elementname:string;
		const namespace:string):boolean;

	function doXMLRemoveNamespace(const filename:string;
		const elementname:string;
		const namespace:string):boolean;



	(* Spezielle Methoden *)


	(* Sektion erstellen *)
	procedure loadValidLinesFromFile (Const FName : String; var Section : TWorkSection);
	//procedure getLinesFromUnicodeFile (Const FName : String; var Section : TWorkSection);
///	procedure ApplyTextVariables (var Sektion : TXStringList; CStringEscaping : Boolean);
///	procedure ApplyTextConstants (var Sektion : TXStringList; CStringEscaping : Boolean);

	(* Sektionsbearbeitungsmethoden *)
	(* Hilfsmethoden *)
	function initSection (const Sektion: TWorkSection; var SaveErrorNumber, SaveWarningNumber: Integer) : Boolean;
	procedure finishSection (const Sektion: TWorkSection; const SaveErrorNumber, SaveWarningNumber: Integer;
														var DiffNumberOfErrors, DiffNumberOfWarnings : Integer);

	(* fuer primaere Sektionen *)
		(* fuer andere Sektionen *)


	function doXMLPatch (const Sektion: TWorkSection; Const XMLFilename : String;
					 var output: TXStringList)
																																 : TSectionResult;
end;

var
  PStatNames : TPStatementNames;

implementation

var
 ps : String;
  OldNumberOfErrors, DiffNumberOfErrors,
 OldNumberOfWarnings, DiffNumberOfWarnings : Integer;

constructor TWorkSection.create (const NestLevel : Integer);
begin
  inherited create;
  FStartLineNo  := 0;
	FSectionName  := '';
	FNestingLevel := NestLevel;
end;

destructor TWorkSection.destroy;
begin
  inherited destroy;
end;

function GetString
  (const s : String; var ResultString, Remaining, errorinfo : String;
   StringInStringAllowed : Boolean) : Boolean;
(* reads a string that is delimited either by ' or by "  (citation mark);
   if StringInStringallowed then the function interprets a double mark
   of the appropriate kind as being contained in the string.
   resultstring takes the correctly interpreted chars
   remaining gets the original value s if result is false *)

 var
   TheMark: Char;
   r, Info, DoubleMark : String;
   continue : Boolean;
   PartValue : String;

   //i : Integer;
   //foundDelimiter : Boolean;

begin
  result := false;
	resultString := '';
	r := s;
	SetLength (r, length (s));


  if (length (r) < 2)
  then
    ErrorInfo := 'String marker expected'
  else
  Begin
    TheMark := r[1];
    if (TheMark <> '''') and (TheMark <> '"')
    then
        ErrorInfo := '" or '' expected'
    else
    Begin
      DoubleMark := TheMark + TheMark;
      //skipA(TheMark, r, r, errorinfo);
      skip(TheMark, r, r, errorinfo);
      Continue := true;
      while Continue do
      begin
        (*
        if (r = '') or (r[1] = theMark)
        then PartValue := ''
        else
        Begin
           i := 1;
           foundDelimiter := false;
           while not foundDelimiter and (i <= length (r))
           do
           Begin
             if r[i] = theMark
             then foundDelimiter := true
             else inc (i)
           End;

           if foundDelimiter
           then
           Begin
             PartValue := copy (r, 1, i-1);
             r := copy (r, i, length(r));
           End
           else
           Begin
             PartValue := r;
             r := '';
           End;
        End;
        *)
        GetWord (r, PartValue, r, [TheMark]);
        ResultString := ResultString + PartValue;

        if StringInStringAllowed then
        Begin
          if SkipA (DoubleMark, r, r, Info)  //SkipA (DoubleMark, r, r, Info)
          then
            ResultString := ResultString + TheMark
          else
            Continue := false;
        End
        else
          Continue := false;
      End;
      if SkipA (TheMark, r, r, ErrorInfo)//SkipA (TheMark, r, r, ErrorInfo)
      then result := true;
    End;
  End;

  if result
  then Remaining := cutLeftBlanks (r)
  else Remaining := s;
end;


// *** TuibXMLNodeDescription ****
//

constructor TuibXMLNodeDescription.Create (xmldoc : TuibXMLDocument);
begin
 inherited create;
 fxmldoc := xmldoc;
end;

destructor TuibXMLNodeDescription.Destroy;
begin

 inherited destroy;
end;


function TuibXMLNodeDescription.evaluate (s : String; var Remaining: String;
                                           var  InfoSyntaxError : String) : Boolean;

 var
  r : String;
  syntaxCheck : Boolean;
  syntaxInfo : String;
  elementname : String;

begin
  r := lowercase (s);

  syntaxCheck := false;
  InfoSyntaxError := '';

  if Skip ('(', s, r, syntaxInfo)  // geklammerter Ausdruck
  then
  begin
   if evaluate (r, r, syntaxInfo)
   and Skip (')', r, r, syntaxInfo)
   then
      syntaxCheck := true;
  end


  else if Skip ('elementname', r, r, syntaxInfo)
  then
  Begin

    if GetString (r, elementname, r, InfoSyntaxError, false)
    then
    begin
      syntaxCheck := true;
      //Fxmldoc.filterByChildelement(elementname)
    end;

  end

  else if Skip ('attribute', r, r, syntaxInfo)
  then
  Begin

  end

  else if Skip ('position', r, r, syntaxInfo)
  then
  Begin

  end

  else
  begin
     InfoSyntaxError := 'XML Path description not accepted'
  end;


  Remaining := r;
end;

procedure TuibXMLNodeDescription.evaluateElementname;
begin
end;
procedure TuibXMLNodeDescription.evaluateAttribute;
begin
end;
procedure TuibXMLNodeDescription.evaluateText;
begin
end;


(* TuibInitScript *)


constructor TuibInstScript.create;

Begin
  inherited create;
  FNumberOfErrors := 0;
  FNumberOfWarnings := 0;

  FExtremeErrorLevel := level_not_initialized;

	FLogLevel := LLinfo;
  FExitOnError := false;
  FReportMessages := true;
  FTraceMode := false;

  VarList := TStringList.create;
  //VarList.add(PreDefinedVariableSkinDirectory);
  ValuesList := TStringList.create;
  //ValuesList.add(PreDefinedVariableSkinDirectoryValue);

  listOfStringLists := TStringList.create;
  ContentOfStringLists := TObjectList.create;
  ConstList := TStringList.create;
  ConstValuesList := TStringList.create;
  
End;

destructor TuibInstScript.destroy;
begin
  VarList.free; VarList := nil;
  ValuesList.free; ValuesList := nil;
  listOfStringLists.free; listOfStringLists := nil;
  ContentOfStringLists.free; ContentOfStringLists := nil;
end;



procedure TuibInstScript.LoadValidLinesFromFile (const FName : String; var Section : TWorkSection);
 var
  OriginalList : TXStringList;
  s : String;
  i : Integer;
begin
  Section.Clear;
  OriginalList := TXStringList.create;
  OriginalList.LoadFromFile (FName);
  for i := 1 to OriginalList.count do
  Begin
      s := cutLeftBlanks (OriginalList.Strings [i-1]);
      (* if (s<>'') and  (s[1] <> LineIsCommentChar) then *)
        Section.Add (s);
  End;
  OriginalList.free;
End;


function TuibInstScript.reportError (const Sektion: TWorkSection; LineNo : Integer;
             Const Content : String; Comment: String) : TSectionResult;

begin
  result := tsrPositive;

  if ReportMessages then
  Begin
    ps := 'Sektion   ' + Sektion.Name +
                ' (Kommando in Zeile ' + IntToStr (Sektion.StartLineNo + LineNo) + '):'
          + #10#13 + Content
          + #10#13 + Comment;

///    if MyMessageDlg.WiMessage (ps, [mrOK, mrAbort]) = mrAbort
///      then result := tsrExitProcess;
  End;

end;


function TuibInstScript.getVarValues : TStringList;
var
  k : Integer;
begin
  if FVarList <> nil
  then
  begin
    k := FVarList.IndexOf('%loglevel%');
    if k > -1
    then
      FValuesList.strings[k] := IntToStr (Logdatei.LogLevel);
  end;
  result := FValuesList;
end;


function TuibInstScript.doInfo (Meldung: String) : TSectionResult;
begin
  result := tsrPositive;
///  CentralForm.Label1.Caption := Meldung;

  //FBatchOberflaeche.LabelInfo.alignment := labelinfoAlignmentSave;

  (*
  if FBatchOberflaeche.LabelInfo.Canvas.TextWidth(Meldung) > FBatchOberflaeche.LabelInfo.Width
  then
  Begin
    labelinfoAlignmentSave := FBatchOberflaeche.LabelInfo.alignment;
    FBatchOberflaeche.LabelInfo.alignment := taLeftJustify;
    //FBatchOberflaeche.LabelInfo.Width := FBatchOberflaeche.LabelInfo.Canvas.TextWidth(Meldung);
  End;
  *)


///	FBatchOberflaeche.setInfoLabel(Meldung);

  //Application.ProcessMessages; 
end;

function TuibInstScript.doLogEntries (const Lines : String; EntryLevel : Integer)
   : TSectionResult;
var
	ManyLines, line : String;
  i : Byte;
begin
  result := 0;
  ManyLines := Lines;
  repeat
    i := pos (#13#10, ManyLines);
    if i > 0 then
    Begin
      line := copy (ManyLines, 1, i - 1);
      ManyLines := copy (ManyLines, i+2, length (ManyLines))
    End
    else
      line := ManyLines;
		LogDatei.DependentAdd (Line, EntryLevel);

  until i = 0;
(*
	if logdatei.LogLevel >= EntryLevel then
	begin
		if Line = PStatNames^ [tsCondOpen] then Line := '|';
		if Line = PStatNames^ [tsCondThen] then Line := '/';
		if Line = PStatNames^ [tsCondElse] then Line := '—';
		if Line = PStatNames^ [tsCondClose] then Line := '\';
		CentralForm.Label2.caption := Line;
		FBatchOberflaeche.setDetailLabel(Line);
	end;
*)
	Application.ProcessMessages; 

  result := tsrPositive;
end;


function TuibInstScript.initSection (const Sektion: TWorkSection;
   var SaveErrorNumber, SaveWarningNumber : Integer) : Boolean;
begin
  result := true;
  if Sektion.count = 0 then result := false;

  SaveErrorNumber := LogDatei.NumberOfErrors;
  SaveWarningNumber := LogDatei.NumberOfWarnings;
  LogDatei.ErrorNumberMarked := SaveErrorNumber;

  (* LogDatei.LogSIndentLevel := Sektion.NestingLevel; *)
  ps := '';
  LogDatei.DependentAdd (ps, LevelWarnings);
  if Sektion.count > 0 then
  begin
    ps := 'Execution of ' + Sektion.Name;
    LogDatei.DependentAdd (ps, LevelWarnings);
  end
  else
  // this case should be captured beforehand
    LogDatei.DependentAddWarning ('Warning: Section  "' + Sektion.Name + '"  does not exist or is empty', LevelWarnings);

////  ps := (*  'Ausfuehrung von ' +  *) copy (Sektion.Name, length (PStatNames^ [Sektion.fSectionKind]) + 1,
////                                  length (Sektion.Name));
///  CentralForm.Label2.caption := ps;
///  FBatchOberflaeche.setDetailLabel(CentralForm.Label2.caption);
end;

procedure TuibInstScript.finishSection (const Sektion: TWorkSection;
   const SaveErrorNumber, SaveWarningNumber : Integer;
   var DiffNumberOfErrors, DiffNumberOfWarnings : Integer);
begin
  DiffNumberOfErrors := LogDatei.NumberOfErrors - SaveErrorNumber;
	DiffNumberOfWarnings := LogDatei.NumberOfWarnings - SaveWarningNumber;
	FNumberOfErrors := SaveErrorNumber + DiffNumberOfErrors;
  FNumberOfWarnings := SaveWarningNumber + DiffNumberOfWarnings;

///  CentralForm.Label2.caption := CentralForm.Label2.caption + ' finished';
///  FBatchOberflaeche.setDetailLabel(CentralForm.Label2.caption);

  (* LogDatei.LogSIndentLevel := Sektion.NestingLevel; *)
  Application.ProcessMessages; 
end;

// string functions
function TuibInstScript.EvaluateString
	 (const s0 : String;
	 var Remaining: String;
	 var StringResult : String;
	 var InfoSyntaxError : String ) : Boolean;


var
 VarIndex : Integer;
 a1 : Integer;
 syntaxCheck : boolean;
 s, sx, r, s1, s2, r1, s3, s4 : String;
 n1, n2 : Integer;
 listindex : Integer;
 randomInt : Integer;

 errorinfo : String;

 intresult : Integer;

 list1 : TXStringlist;

/// HostsImage : TuibPatchHostsFile;
 HostsLocation : String;
 i : Integer;
 ///m, n, rest : Integer;
 itemlist : TXStringList;

///	 Regist : TuibRegistry;
///	 RegType : TuibRegDataType;
///	 majorver : TuibNTVersion;
///	 minorver : DWORD;
	 ///programfilesdir, diffx86 : String;


	 key0, key, valuename : String;
	 StartIndentLevel : Integer;

	 errorOccured : Boolean;
	 continue : Boolean;
	 j : Integer;
	 parameters : Array of String;

///	 omc : TOpsiMethodCall;

/// IniFile : TIniFile;



begin

 syntaxCheck := false;
 InfoSyntaxError := '';
 StringResult := '';

 StartIndentLevel := LogDatei.LogSIndentLevel;

 (* string variable? *)
 GetWord (s0, s, r, WordDelimiterSet3);
 VarIndex := VarList.IndexOf (LowerCase (s));
 if VarIndex >= 0 then
 begin
	 if ValuesList.count - 1 < VarIndex
	 then
	 Begin
		InfoSyntaxError := 'kein Stringwert auf Variable';
	 End
	 else
	 Begin
		 StringResult := ValuesList [VarIndex];
		 syntaxCheck := true;
	 End
 end

 (* string constant?  *)
 else if (length (s0) > 0) and (s0[1] = '"') then
 Begin
	 r := copy (s0, 2, length (s0)-1);
	 GetWord (r, StringResult, r, ['"']);
	 if skip ('"', r, r, InfoSyntaxError)
	 then syntaxCheck := true;

 End

 (* string constant delimited by "'" ?  *)
 else if (length (s0) > 0) and (s0[1] = '''') then
 Begin
	 r := copy (s0, 2, length (s0)-1);
	 GetWord (r, StringResult, r, ['''']);
	 if skip ('''', r, r, InfoSyntaxError)
	 then syntaxCheck := true;
 End

 (* checking our pseudo function name for retrieving a string avoiding any escape problems of citations marks *)
 else if LowerCase (s) = LowerCase ('EscapeString') then
 Begin
	 if Skip (':', r, s1, InfoSyntaxError)
	 then
	 Begin
		 StringResult := s1;
		 r := '';
		 syntaxCheck := true;
	 End
 End

	(* string functions ? *)

 else
		InfoSyntaxError := s0 + ' illegal String expression';


 (* Addition weiterer Teilstrings mit + *)
 if syntaxCheck and skip ('+', r, r, sx)
 then
 Begin
		syntaxCheck := EvaluateString (r, r, s1, InfoSyntaxError);
		StringResult := StringResult + s1;
 End;

 if syntaxcheck
 then
		Remaining := r;

 result := syntaxCheck;

			 (*  if Script.ExitOnError then
				Begin
					tsr := tsrExitProcess;
					.DependentAdd (StopInfo, BaseLevel);
				End; *)

	LogDatei.LogSIndentLevel := StartIndentLevel;

End;


procedure TuibInstScript.GetWordOrStringExpression (const s: String;
													 var resultString, Remaining, errorinfo : String);
 { nur fuer AktionenSektion-Syntax 0 }
 var s0 : String;

begin
	Resultstring := '';
	s0 := s;
	setLength (s0, length (s)); //should create a new instance, but does not always work
	if not EvaluateString (s0, Remaining, resultString, errorInfo)
	then
		GetWord (s0, resultString, Remaining, WordDelimiterWhiteSpace);
end;



function TuibInstScript.doXMLAddNamespace(const filename:string;
	const elementname:string;
	const namespace:string):boolean;

 var list:TStringList;
	s1,s2,s3,s4:string;
	i,p1,p2:integer;
	bChanged:Boolean;
	///done : Boolean;
 begin
	Result:=false;
	bChanged:=False;
	list:=TStringList.Create;
	try
		list.LoadFromFile(filename);
		for i:=0 to list.Count-1 do
		begin
			p1:=pos('<'+elementname,list[i]);
			if p1>0 then
			begin //Element gefunden
				s1:=copy(list[i],1,p1-1); //String vor '<element'
				s2:=copy(list[i],p1,Length(list[i])); //String ab '<element'
				p2:=pos('>',s2);
				s3:=copy(s2,1,p2); //String von '<element' bis '>'
				s4:=copy(s2,p2+1,Length(list[i])); //Alles hinter '>'

				p2:=pos(namespace,s3);
				if p2=0 then //sonst schon vorhanden
				begin
					s3:=copy(s3,1,Length(s3)-1); //'>' abschneiden
					s3:=s3+' '+namespace+'>'; //und wieder anfï¿½gen

					list[i]:=s1+s3+s4;
					bChanged:=True;
					Result:=True;
					break; //wir suchen nur nach dem ersten Vorkommen
				end;
			end;
		end;
		if bChanged then
		list.SaveToFile(filename);

	finally
		list.Free;
	end;

 end;

 function TuibInstScript.doXMLRemoveNamespace(const filename:string;
	const elementname:string;
	const namespace:string):boolean;
	var
		list:TStringList;
		s1,s2,s3,s4:string;
		i,p1,p2:integer;
		bChanged:Boolean;
	begin
		Result:=false;
		bChanged:=False;
		list:=TStringList.Create;
		try
			list.LoadFromFile(filename);
			for i:=0 to list.Count-1 do
			begin
				p1:=pos('<'+elementname,list[i]);
				if p1>0 then
				begin //Element gefunden
					s1:=copy(list[i],1,p1-1); //String vor '<element'
					s2:=copy(list[i],p1,Length(list[i])); //String ab '<element'
					p2:=pos('>',s2);
					s3:=copy(s2,1,p2); //String von '<element' bis '>'
					s4:=copy(s2,p2+1,Length(list[i])); //Alles hinter '>'

					p2:=pos(namespace,s3);
					if p2>0 then //sonst nicht vorhanden
					begin
						System.delete(s3,p2,Length(namespace));
					if s3[p2-1]=' ' then //wir haben bei Add ein Leerzeichen hinzugefï¿½gt
					System.delete(s3,p2-1,1);

					list[i]:=s1+s3+s4;
					bChanged:=True;
					Result:=True;
					break; //wir suchen nur nach dem ersten Vorkommen
				end;
			end;
		end;
		if bChanged then
		list.SaveToFile(filename);
	finally
		list.Free;
	end;
 end;



function TuibInstScript.doXMLPatch (const Sektion: TWorkSection; Const XMLFilename : String;
			var output: TXStringList) : TSectionResult;



Var

 XMLDoc : TuibXMLDocument;
 myComp : TComponent;
 //XMLDoc : IXMLDocument;
 fname : String;

 teststr : string;
 testnode : IXMLNode;

 i,k,j, n : Integer;
 line : String;
 errorinfo : String;
 syntaxerrorinfo : String;
 startSIndentLevel : Integer;
 domchanged : boolean;
 openNodeCommandExists : boolean;
 SyntaxCheck : boolean;
 nodepath : String;
 r, expression, remaining : String;
 erroroccured : boolean;

 attributename, attributename0, uri, attributevalue, oldattributevalue : String;
 hasTheAttribute : boolean;

 textcontent, oldtext : String;

 in_node_description, started_searchpath,
 nodeset_empty : boolean;
 level_of_path : Integer;

 //parts : TXStringList;

 textvalue : String;
 elementname : String;
 s1, s2 : String;
 attributenames : TStringsArray;
 attributevalues : TStringsArray;
 attributevalueExists : TbooleansArray;

 count_of_removed : Integer;

 elementnamefiltering : boolean;
 textfiltering : boolean;

 attributes_strict: boolean;

 create_when_node_not_existing : boolean;
 error_when_no_node_existing : boolean;
 warning_when_no_node_existing : boolean;
 error_when_nodecount_greater_1 : boolean;
 warning_when_nodecount_greater_1 : boolean;

 procedure initparameters;
	 //set default values for parameters
 begin
	attributes_strict := false;
	create_when_node_not_existing := false;
	error_when_no_node_existing := false;
	warning_when_no_node_existing := true;
	error_when_nodecount_greater_1 := false;
	warning_when_nodecount_greater_1 := false;
 end;

 procedure expect_true_or_false (var b : boolean; const s : String; var r : String;
																 var info : String; var syntaxcheck : boolean;
																 name_of_b : String);
 begin
		 if Skip ('true', s, r, info) and (r = '')
		 then
		 begin
				b := true;
				LogDatei.DependentAdd(name_of_b + ' set to true', levelcomplete);
		 end
		 else if Skip ('false', s, r, info) and (r = '')
		 then
		 begin
				b := false;
				LogDatei.DependentAdd(name_of_b + ' set to false', levelcomplete);
		 end
		 else
		 begin
				syntaxcheck := false;
				info := 'true or false expected'
		 end;
 end;


 procedure setParameters (const s : String; var syntaxcheck : boolean; var syntaxerrorinfo : String);
	 var
	 Remaining : String;
 begin
		Remaining := s;
		if Skip ('create_when_node_not_existing',Remaining, Remaining, syntaxerrorinfo)
		then
		begin
			expect_true_or_false(create_when_node_not_existing, Remaining, Remaining,
													syntaxerrorinfo, syntaxcheck, 'create_when_node_not_existing');
		end

		else if Skip ('error_when_no_node_existing', Remaining, Remaining, syntaxerrorinfo)
		then
		begin
			expect_true_or_false(error_when_no_node_existing, Remaining, Remaining,
													syntaxerrorinfo, syntaxcheck, 'error_when_no_node_existing');
		end

		else if Skip('warning_when_no_node_existing', Remaining, Remaining, syntaxerrorinfo)
		then
		begin
		 expect_true_or_false(warning_when_no_node_existing, Remaining, Remaining,
													syntaxerrorinfo, syntaxcheck, 'warning_when_no_node_existing');
		end

		else if Skip ('error_when_nodecount_greater_1', Remaining, Remaining, syntaxerrorinfo)
		then
		begin
			expect_true_or_false(error_when_nodecount_greater_1, Remaining, Remaining,
													syntaxerrorinfo, syntaxcheck, 'error_when_nodecount_greater_1');
		end

		else if Skip('warning_when_nodecount_greater_1', Remaining, Remaining, syntaxerrorinfo)
		then
		begin
		 expect_true_or_false(warning_when_nodecount_greater_1, Remaining, Remaining,
													syntaxerrorinfo, syntaxcheck, 'warning_when_nodecount_greater_1');
		end

		else if Skip('attributes_strict', Remaining, Remaining, syntaxerrorinfo)
		then
		begin
			expect_true_or_false(attributes_strict, Remaining, Remaining,
													syntaxerrorinfo, syntaxcheck, 'attributes_strict')
		end

		else
		begin
			syntaxCheck := false;
			syntaxerrorinfo := 'not a known option'
		end
 end;

 procedure initnextstep;
 begin
	setlength (attributenames, 0);
	setlength (attributevalues, 0);
	setlength (attributevalueExists, 0);

	xmldoc.makeNewDerivedNodeSet;

	elementnamefiltering := false;
	elementname := '';
	textfiltering := false;
	textvalue := '';
 end;

 procedure finish_evaluation (nodeset_empty : boolean; var erroroccured : boolean);
 begin
		if nodeset_empty
		then
		begin
			if error_when_no_node_existing
			then
			begin
				 LogDatei.DependentAddError('Error: No fitting XML path found', levelcomplete);
				 erroroccured := true;
			end
			else if warning_when_no_node_existing
			then
				 LogDatei.DependentAddWarning('Warning: No fitting XML path found', levelwarnings)
		end
		else if xmldoc.CountNotNil > 1
		then
		begin
			if error_when_nodecount_greater_1
			then
			begin
				 LogDatei.DependentAddError('Error: There should be only one XML path', levelcomplete);
				 erroroccured := true;
			end
			else if warning_when_nodecount_greater_1
			then
				 LogDatei.DependentAddWarning('Warning: There should be only one XML path', levelwarnings)
		end
 end;

 procedure process_end_of_level;
 begin
		LogDatei.DependentAdd('', levelcomplete);
		xmldoc.filterByChildElement(elementnamefiltering, elementname);

		xmldoc.filterByText(textfiltering, textvalue);

		xmldoc.filterByAttributeList
			 (attributenames, attributevalueExists, attributevalues,
				attributes_strict);

		if xmldoc.CountDerivedNotNil = 0
		then
		begin
			if create_when_node_not_existing
			then
			begin
					if elementname = ''
					then
					Begin
						syntaxcheck := false;
						syntaxerrorinfo := 'elementname missing, creating node not possible';
					end
					else
					Begin
						LogDatei.DependentAdd('creating a fitting node', levelcomplete);
						XMLDoc.makeNodes(elementname, attributenames, attributevalues, textvalue, -1);
					end;
				end;
		end;

		if xmldoc.debuglevel <= logdatei.loglevel then xmldoc.logNodeSets;

		xmldoc.getNextGenerationActNodeSet;
		nodeset_empty := (xmldoc.CountNotNil = 0);

		initnextstep;

		XMLDoc.logNodeSets;
 end;


begin

	result := tsrPositive;

	domchanged :=false;
	erroroccured := false;


	startSIndentLevel :=  LogDatei.LogSIndentLevel;

	if not initSection (Sektion, OldNumberOfErrors, OldNumberOfWarnings) then exit;

	LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

	if not getString (XMLFilename, fname, remaining, syntaxerrorinfo, false)
	then // filename is perhaps not in citations marks
		fname := XMLFilename;

	ps := LogDatei.LogSIndentPlus (+3) + 'FILE ' +  fname;
	LogDatei.DependentAdd (ps, LevelWarnings);

	if not FileExists (fname)
	then
	Begin
		ps := LogDatei.LogSIndentPlus (+3) + 'Info: This file does not exist and will be created ';
		LogDatei.DependentAdd (ps, LevelInfo);
		LogDatei.NumberOfHints := Logdatei.NumberOfHints + 1;

		LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

		if CreateTextfile (fname, ErrorInfo)
		then
		Begin
		 if ErrorInfo <> ''
		 then
		 Begin
			 ps := LogDatei.LogSIndentPlus (+3) + 'Warning: ' + ErrorInfo;
			 LogDatei.DependentAddWarning (ps, LevelWarnings);
		 End;
		End
		else
		Begin
			 ps := LogDatei.LogSIndentPlus (+3) + 'Error: ' + ErrorInfo;
			 LogDatei.DependentAddError (ps, LevelWarnings);
			 errorOccured := true;
		End;
	End;


	// rebuild XMLDocument1
	if not erroroccured then try
		//LogDatei.DependentAdd ('trying to load XMLFile ' + fname, LevelComplete);

		LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
		mycomp := TComponent.Create(nil);
		XMLDoc := TuibXMLDocument.create (mycomp);
		//XMLDoc := IXMLDocument.create (nil);
		XMLDoc.debuglevel := levelcomplete;
		xmldoc.DOMVendor := GetDOMVendor(XMLDOMVendor);
		LogDatei.DependentAdd ('DOM Vendor ' + XMLDOMVendor, levelcomplete);
		xmldoc.ParseOptions := [];
		// possible values: (doNodeAutoCreate, doNodeAutoIndent, doAttrNull, doAutoPrefix, doNamespaceDecl, doAutoSave);
		// (doAttrNull=false) give empty string if attributename not exists
		xmldoc.Options := [doNodeAutoIndent, doAutoPrefix, doNamespaceDecl];
		LogDatei.DependentAdd ('XML Options [doNodeAutoIndent, doAutoPrefix, doNamespaceDecl]',
				levelComplete);

		XMLDoc.loadFromFile (fname);
				// internal exception when empty
		teststr := xmldoc.getXmlStrings[0];
		testnode := xmldoc.DocumentElement;

		LogDatei.DependentAdd ('Activated XMLDocument from file  ' + fname, LevelInfo);
		LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;

	except
		on e: Exception do
		begin
			erroroccured := true;
			LogDatei.DependentAddError ('Error: Could not construct XML Document from ' + fname + ' info: ' + e.Message,
			 BaseLevel);
		end;

	end;

	Application.ProcessMessages;

	OpenNodeCommandExists := false;

	if not errorOccured then
	begin

		i := 1;

		SyntaxCheck := true;

		initparameters;

		while (i <= Sektion.count) and syntaxcheck and not erroroccured
		do
		Begin

			r := trim(Sektion.strings [i-1]);
			syntaxcheck := false;

			if (r = '') or (r [1] = LineIsCommentChar)
			then
			begin
				syntaxcheck := true;
				(* continue *)
			end

			else
			Begin

				GetWord (r, Expression, r, WordDelimiterSet1);


				if Expression = '-'
				then
				Begin
					syntaxcheck := true;
					setParameters (r, syntaxcheck, syntaxerrorinfo);
				end


				(*
					There are a long and detailled version
					for the openNodeSet command
					and a short one.


					The short one describes xml pathes in the form

						subnodename {attributename="attributevalue"}/subnodename [attributename="attributevalue"}/...

					The parameters have to be set beforehand and can not be set specifically to node levels.


					The long syntax looks as follows:

					openNodeSet
							- error_when_no_node_existing false
							- warning_when_no_node_existing true
							- error_when_nodecount_greater_1 false
							- warning_when_nodecount_greater_1 false
							- create_when_node_not_existing true
							- attributes_strict true

						 documentroot

						 all_childelements_with:
							 elementname: "x1"
							 attribute: "attributename"  value="a1_v"
							 attribute: "attributename"
							 position: "first" // not implemented
							 text: "yyy" // not implemented

						 all_childelements_with:

						 end


					//the following commands are implemented

					setAttribute
					addAttribute
					deleteAttribute

					setText
					addText
					deleteText

					deleteElement

					return
						nodes
						elements
						text

					*)



				else if LowerCase (Expression) = LowerCase ('OpenNodeSet')
				then
				Begin
					if OpenNodeCommandExists (* i.e., existed already *)
					then
						 LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1
					else
						 OpenNodeCommandExists := true;


				 LogDatei.DependentAdd('', levelcomplete);
				 LogDatei.DependentAdd('', levelcomplete);
				 LogDatei.DependentAdd('Open node set', levelcomplete);
				 LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;


				 if r <> ''
				 then  // short version of xml path syntax
				 begin
					 syntaxcheck := GetString (r, remaining, r, syntaxerrorinfo, false);

					 if syntaxcheck then
					 begin
						testnode := xmldoc.DocumentElement;
						 // documentroot
						 started_searchpath := true;
						 xmldoc.setlengthActNodeSet  (1);
						 xmldoc.actnodeset[0] := xmldoc.DocumentElement;
						 initnextstep;

						 // now we are searching for node descriptions

						 while (Remaining <> '') and syntaxcheck do
						 begin
								GetWord (Remaining, elementname, Remaining, [' ', #9, '/']);
								if elementname <> ''
								then
									elementnamefiltering := true;


								if (Remaining = '')
								then
									in_node_description := false
								else
									if Skip ('/', Remaining, Remaining, syntaxerrorinfo)
									then
										in_node_description := false
									else
										in_node_description := true;


								// here we are looking for attributes
								while (Remaining <> '') and SyntaxCheck and in_node_description
								do
								begin
									 // add an attribute to the arrays
									 n := length(attributenames);
									 setLength (attributenames, n + 1);
									 setLength (attributevalues, n + 1);
									 setLength (attributevalueexists, n + 1);
									 attributeValueexists[n] := true;

									 GetWord (Remaining, attributenames[n], Remaining, ['=']);

									 syntaxcheck :=
										Skip ('=', Remaining, Remaining, syntaxerrorinfo)
										and GetString (Remaining, attributevalues[n], Remaining, syntaxerrorinfo, false);


										if (Remaining = '')
										then
											in_node_description := false
										else
											if Skip ('/', Remaining, Remaining, syntaxerrorinfo)
											then
												in_node_description := false
											else
												in_node_description := true;
								end;

								process_end_of_level;

						 end;

						 finish_evaluation ( (xmldoc.CountNotNil = 0) , erroroccured);
					 end;
				 end

				 else  // long version of xml path syntax
				 begin

					 level_of_path := 0;
					 //initparameters;

					 inc (i);


					 in_node_description := true;
					 started_searchpath := false;

					 syntaxcheck := true;

					 nodeset_empty := false;

					 while (i <= Sektion.count) and syntaxcheck and in_node_description do
					 Begin
							Remaining :=  trim(Sektion.strings [i-1]);

							if (Remaining = '') or (Remaining [1] = LineIsCommentChar)
							then // continue

							else if Skip ('-', Remaining, Remaining, syntaxerrorinfo)  // Parametrisierung
							then
							Begin
								setParameters (Remaining, syntaxcheck, syntaxerrorinfo);
							end

							else if lowercase(Remaining) = 'documentroot'
							then
							begin
								if started_searchpath
								then
								Begin
									syntaxcheck := false;
									syntaxerrorinfo := '"documentroot" cannot be placed here'
								end
								else
								Begin
									started_searchpath := true;
									xmldoc.setlengthActNodeSet  (1);
									xmldoc.actnodeset[0] := xmldoc.DocumentElement;
									initnextstep;
								end
							end

							else
							Begin
								if syntaxcheck
								then
								Begin
									if Skip('all_childelements_with:', Remaining, Remaining, syntaxerrorinfo)
									then
									begin
										if level_of_path > 0
										then
											if not nodeset_empty
											then
											Begin
												process_end_of_level;
											end;
											// evaluation of expressions of the old level not for document root
										inc (level_of_path);
									end

									{ for debugging reasons we may introduce an end of level statement
									else if Skip ('end_childs', Remaining, Remaining, syntaxerrorinfo)
									then
									Begin
										levelfinished := true;
									end
									}

									else if Skip ('elementname:', Remaining, Remaining, syntaxerrorinfo)
									then
									Begin
										if Remaining = ''
										then
										begin
											syntaxcheck := false;
											syntaxerrorinfo := 'missing elementname';
										end
										else
										begin
											syntaxcheck := getString (remaining, elementname, remaining, syntaxerrorinfo, false);
											elementnamefiltering := true;
										end;
									End

									else if Skip ('text:', Remaining, Remaining, syntaxerrorinfo)
									then
									Begin
										if Remaining = ''
										then
										begin
											syntaxcheck := false;
											syntaxerrorinfo := 'missing text';
										end
										else
										begin
											syntaxcheck := getString (remaining, textvalue, remaining, syntaxerrorinfo, false);
											textfiltering := true;
										end;
									End


									else if Skip('attribute:', Remaining, Remaining, syntaxerrorinfo)
									then
									Begin
										if Remaining = ''
										then
										begin
											syntaxcheck := false;
											syntaxerrorinfo := 'missing attribute strings';
										end
										else
										begin
											n := length(attributenames);
											setLength (attributenames, n + 1);
											setLength (attributevalues, n + 1);
											setLength (attributevalueexists, n + 1);
											if getString (remaining, s1, remaining, syntaxerrorinfo, false)
											then
											begin
												 if Skip ('value', remaining, remaining, syntaxerrorinfo)
												 then
												 Begin
													 if Skip ('=', remaining, remaining, syntaxerrorinfo)
													 and getString (remaining, s2, remaining, syntaxerrorinfo, false)
													 then
													 Begin
															attributenames[n] := s1;
															attributevalues[n] := s2;
															attributevalueExists[n] := true;
													 end
													 else
														 syntaxcheck := false;
												 end
												 else
												 begin
													 if Remaining = ''
													 then
													 Begin
															attributenames[n] := s1;
															attributevalueExists[n] := false;
													 end
													 else
													 Begin
															syntaxcheck := false;
															syntaxerrorinfo := 'remaining "' + Remaining + '" not valid';
													 end;
												 end
											end
										end;
									End

									else if Skip ('end', Remaining, Remaining, syntaxerrorinfo)
									then
									Begin
										in_node_description := false;
										if not nodeset_empty
										then
										Begin
											 process_end_of_level;
										end;
									end

									else
									Begin
										Syntaxcheck := false;
										syntaxerrorinfo := 'Not recognized';
									End;

								end;

							End;

							if syntaxcheck and in_node_description then inc (i);

							if not in_node_description
							then
								finish_evaluation (nodeset_empty, erroroccured);
					 end;

				 end

				End

				else
				begin

				 if not OpenNodeCommandExists
				 then SyntaxErrorInfo := 'OpenNodeSet command missing'
				 else
				 Begin
					 if LowerCase (Expression) = LowerCase ('SetAttribute')
					 then
					 Begin
						 if getString (r, attributename, r, syntaxerrorinfo, false)
						 and Skip ('value', r, r, syntaxerrorinfo)
						 and Skip ('=', r, r, syntaxerrorinfo)
						 and getString (r, attributevalue, r, syntaxerrorinfo, false)
						 then
						 Begin
							 syntaxcheck := true;

							 LogDatei.DependentAdd('', levelComplete);
							 LogDatei.DependentAdd('Setting attribute "' + attributename + '" value="' + attributevalue + '"', levelComplete);

							 for k:= 0 to length(xmldoc.actNodeSet)-1
							 do
							 begin
								 xmldoc.getNamespaceAndBasename (attributename, uri, attributename0);

								 hasTheAttribute := xmldoc.actNodeSet[k].hasAttribute (attributename0, uri);
								 if hasTheAttribute
								 then
									 oldattributevalue := xmldoc.actNodeSet[k].getattributeNs(attributename0, uri);

								 if hasTheAttribute
								 then
								 begin
									 if oldattributevalue <> attributevalue
									 then
									 begin
										 xmldoc.actNodeSet[k].setAttributeNS(attributename0,uri,attributeValue);
										 LogDatei.DependentAdd
										 ('node ' + inttostr(k) + ': attribute "' + attributename + '" value="' + attributevalue + '" set, old value was "'
											 + oldattributevalue  + '"',
										 levelComplete)
									 end
									 else
										 LogDatei.DependentAdd
										 ('node ' + inttostr(k) + ': has already attribute "' + attributename + '" value="' + attributevalue + '"',
											levelComplete);
								 end
								 else
								 begin
										 xmldoc.actNodeSet[k].setAttributeNS(attributename0,uri,attributeValue);
										 LogDatei.DependentAdd
										 ('node ' + inttostr(k) + ': attribute "' + attributename + '" value="' + attributevalue + '" set',
										 levelComplete);
								 end
							 end;
						 end;
					 end


					 else if LowerCase (Expression) = LowerCase ('AddAttribute')
					 then
					 Begin
						 if getString (r, attributename, r, syntaxerrorinfo, false)
						 and Skip ('value', r, r, syntaxerrorinfo)
						 and Skip ('=', r, r, syntaxerrorinfo)
						 and getString (r, attributevalue, r, syntaxerrorinfo, false)
						 then
						 Begin
							 syntaxcheck := true;

							 LogDatei.DependentAdd('', levelComplete);
							 LogDatei.DependentAdd('Adding attribute "' + attributename + '" value="' + attributevalue + '"', levelComplete);

							 for k:= 0 to length(xmldoc.actNodeSet)-1
							 do
							 begin
								 xmldoc.getNamespaceAndBasename (attributename, uri, attributename0);

								 hasTheAttribute := xmldoc.actNodeSet[k].hasAttribute (attributename0, uri);
								 if hasTheAttribute
								 then
									 oldattributevalue := xmldoc.actNodeSet[k].getattributeNs(attributename0, uri);

								 if hasTheAttribute
								 then
									 LogDatei.DependentAdd
										 ('node ' + inttostr(k) + ': The attribute "' + attributename + '" already exists, value="'
											+ oldattributevalue + '"',
										 levelComplete)
								 else
								 begin
										 xmldoc.actNodeSet[k].setAttributeNS(attributename0,uri,attributeValue);
										 LogDatei.DependentAdd
										 ('node ' + inttostr(k) + ': attribute "' + attributename + '" value="' + attributevalue + '" added',
										 levelComplete);
								 end
							 end;
						 end;
					 end

					 else if LowerCase (Expression) = LowerCase ('DeleteAttribute')
					 then
					 Begin
						 if getString (r, attributename, r, syntaxerrorinfo, false)
						 then
						 Begin
							 syntaxcheck := true;

							 LogDatei.DependentAdd('', levelComplete);
							 LogDatei.DependentAdd('Deleting attribute "' + attributename + '"', levelComplete);

							 for k:= 0 to length(xmldoc.actNodeSet)-1
							 do
							 begin
								 xmldoc.getNamespaceAndBasename (attributename, uri, attributename0);

								 hasTheAttribute := xmldoc.actNodeSet[k].hasAttribute (attributename0, uri);

								 if hasTheAttribute
								 then
								 begin
										 xmldoc.actNodeSet[k].AttributeNodes.Delete(attributename0,uri);
										 LogDatei.DependentAdd
										 ('node ' + inttostr(k) + ': attribute "' + attributename + '" has been deleted',
										 levelComplete)
								 end
								 else
								 begin
										 LogDatei.DependentAdd
										 ('node ' + inttostr(k) + ': attribute "' + attributename + '" did not exist',
										 levelComplete);
								 end
							 end;
						 end;
					 end


					 else if LowerCase (Expression) = LowerCase ('SetText')
					 then
					 Begin
						 if getString (r, textcontent, r, syntaxerrorinfo, false)
						 then
						 Begin
							 syntaxcheck := true;

							 LogDatei.DependentAdd('', levelComplete);
							 LogDatei.DependentAdd('Setting text "' + textcontent + '"', levelComplete);

							 for k:= 0 to length(xmldoc.actNodeSet)-1
							 do
							 begin
								 if xmldoc.actNodeSet[k] <> nil
								 then
									 try
										 xmldoc.actNodeSet[k].text := textcontent;
										 LogDatei.DependentAdd('node ' + inttostr(k) + ': text set', LevelComplete)
									 except
										 // if there are subnodes other than a textnode we get an exception
										 LogDatei.DependentAddWarning('Warning:  node ' + inttostr(k) + ': there is a non-text subnode, setting text not possible', baselevel)
									 end
							 end;
						 end;
					 end

					 else if LowerCase (Expression) = LowerCase ('AddText')
					 then
					 Begin
						 if getString (r, textcontent, r, syntaxerrorinfo, false)
						 then
						 Begin
							 syntaxcheck := true;

							 LogDatei.DependentAdd('', levelComplete);
							 LogDatei.DependentAdd('Adding text "' + textcontent + '"', levelComplete);

							 for k:= 0 to length(xmldoc.actNodeSet)-1
							 do
							 begin
								 if xmldoc.actNodeSet[k] <> nil
								 then
								 begin
									 oldtext := '';

									 if xmldoc.actNodeSet[k].IsTextElement
									 then
										 oldtext := xmldoc.actNodeSet[k].text;

									 if oldtext <> ''
									 then
										 LogDatei.DependentAdd('node ' + inttostr(k) + ': leaving old text "' + oldtext + '"', LevelComplete)
									 else
									 begin
										 try
											 xmldoc.actNodeSet[k].text := textcontent;
											 LogDatei.DependentAdd('node ' + inttostr(k) + ': added text', LevelComplete)
										 except
											 // if there are subnodes other than a textnode we get an exception
											 LogDatei.DependentAddError('node ' + inttostr(k) + ': there is a non-text subnode, setting text not possible', baselevel)
										 end
									 end;
								 end;
							 end;
						 end;
					 end

					 else if LowerCase (Expression) = LowerCase ('DeleteElement')
					 then
					 Begin
						 if getString (r, elementname, r, syntaxerrorinfo, false)
						 then
						 Begin
							 syntaxcheck := true;

							 LogDatei.DependentAdd('', levelComplete);
							 LogDatei.DependentAdd('Deleting elements "' + elementname + '"', levelComplete);

							 count_of_removed := 0;

							 for k:= 0 to length(xmldoc.actNodeSet)-1
							 do
							 begin
									if xmldoc.actNodeSet[k].NodeName = elementname
									then
									Begin
										if xmldoc.actNodeSet[k].ParentNode.ChildNodes.remove (xmldoc.actNodeSet[k]) > -1
										then
										begin
											inc (count_of_removed);
											xmldoc.actNodeSet[k] := nil;
											LogDatei.DependentAdd
											('node ' + inttostr(k) + ' "' + elementname + '" has been deleted',
											levelDebug)
										end;
									end;
							 end
						 end;
						 LogDatei.DependentAdd(IntToStr(count_of_removed) + ' deleted', levelComplete);

					 end

					 else if LowerCase (Expression) = LowerCase ('return')
					 then
					 Begin
						 syntaxcheck := true;

						 output := TXStringlist.create;

						 r := lowercase(r);

						 if r = 'elementnames'
						 then
						 begin
							 LogDatei.DependentAdd('returning all nodenames of selected elements', LevelComplete);
							 for k:= 0 to length(xmldoc.actNodeSet)-1
							 do
							 begin
								 if xmldoc.actNodeset[k] <> nil
								 then
									 output.Add(xmldoc.actNodeset[k].NodeName);
							 end;
						 end
						 else if r = 'elements'
						 then
						 begin
							 LogDatei.DependentAdd('returning all selected elements', LevelComplete);
							 for k:= 0 to length(xmldoc.actNodeSet)-1
							 do
							 begin
								 line :=xmldoc.actNodeset[k].NodeName + ' ';
								 for j := 0 to xmldoc.actNodeset[k].AttributeNodes.Count - 1
								 do
								 begin
									 if xmldoc.actNodeset[k] <> nil
									 then
									 begin
										 line := line + xmldoc.actNodeset[k].AttributeNodes[j].NodeName + '='
														+ '"' + xmldoc.actNodeset[k].AttributeNodes[j].NodeValue + '"';
									 end;
								 end;
								 output.add (line);

							 end;
						 end
						 else if r = 'attributes'
						 then
						 begin
							 LogDatei.DependentAdd('returning all attributes of selected elements', LevelComplete);
							 for k:= 0 to length(xmldoc.actNodeSet)-1
							 do
							 begin
								 if xmldoc.actNodeset[k] <> nil
								 then
									 for j := 0 to xmldoc.actNodeset[k].AttributeNodes.Count - 1
									 do
									 begin
										 line := xmldoc.actNodeset[k].AttributeNodes[j].NodeName + '='
														 + '"' + xmldoc.actNodeset[k].AttributeNodes[j].NodeValue + '"';
										 output.Add(line);
									 end;

							 end;
						 end
						 else if r = 'attributenames'
						 then
						 begin
							 LogDatei.DependentAdd('returning all attribute names of selected elements', LevelComplete);
							 if xmldoc.actNodeset[k] <> nil
							 then
								 for k:= 0 to length(xmldoc.actNodeSet)-1
								 do
								 begin
									 if xmldoc.actNodeset[k] <> nil
									 then
										 for j := 0 to xmldoc.actNodeset[k].AttributeNodes.Count - 1
										 do
										 begin
											 line := xmldoc.actNodeset[k].AttributeNodes[j].NodeName;
											 output.Add(line);
										 end;
								 end;

						 end
						 else if r = 'counting'
						 then
						 begin
								LogDatei.DependentAdd('returning in line 0 the number of selected elements, in line 1 the number of all attributes belonging to them', LevelComplete);
								output.Add(inttostr ( length(xmldoc.actNodeSet) ) );
								j := 0;
								for k:= 0 to length(xmldoc.actNodeSet)-1
								do
								begin
									if xmldoc.actNodeset[k] <> nil
									then
										j := j + xmldoc.actNodeset[k].AttributeNodes.Count;
								end;
								output.Add (inttostr ( j ));
						 end

						 else if r = 'text'
						 then
						 begin
							 LogDatei.DependentAdd('returning text content of the selected elements', LevelComplete);
							 for k:= 0 to length(xmldoc.actNodeSet)-1
							 do
							 begin
								 if xmldoc.actNodeset[k] <> nil
								 then
								 begin
									 if xmldoc.actNodeset[k].IsTextElement then
										 output.add (xmldoc.actNodeset[k].Text)
									 else
										 output.add ('');
								 end;
							 end;
						 end

						 else
						 begin
								syntaxcheck := false;
								syntaxerrorinfo := 'no valid kind of return content';
						 end
					 end

					 else
						 SyntaxErrorInfo := Expression + ' Operation nicht definiert';

				 end
				end;
			End;

			if syntaxcheck then inc (i);

		end;

		if not SyntaxCheck then reportError (Sektion, i, Sektion.strings [i-1], syntaxErrorInfo);

		if not erroroccured then xmldoc.saveXMLFile (fname);
	end;

	finishSection (Sektion, OldNumberOfErrors, OldNumberOfWarnings,
								 DiffNumberOfErrors, DiffNumberOfWarnings);

	LogDatei.LogSIndentLevel := startSIndentLevel;

	if ExitOnError and (DiffNumberOfErrors > 0)
	then result := tsrExitProcess;
//XML
end;


end.
 