unit pluginhelper;

interface

uses
custapp4delphi,
classes,
sysutils,
xmlpluginsynth,
xmlpluginfunc
;

procedure WriteHelp;
procedure main;

implementation

var
myexitcode : integer;

procedure WriteHelp;
var
			filename : string;
begin
  filename := ExtractFileName(paramstr(0));
  writeln(paramstr(0));
  writeln(filename);
  writeln('Usage:');
  writeln(filename+ ' Option [Option]');
  writeln('Options:');
  writeln(' --help -> write this help and exit');
	writeln(' --scriptfile=<script file> -> path to the script');
	writeln(' --xmlfile=<xml file> -> path to the xml file');
	writeln(' --functionname=<function name> -> name of the function: doXMLAddNamespace');
	writeln(' --functionparameter1=<parameter> -> first parameter for function');
	writeln(' --functionparameter2=<parameter> -> second parameter for function');
	writeln(' --version -> write version info and exit');
end;

procedure main;
var
paramvaluestr : string;
waitsec, childsec : integer;
optionlist : TStringlist;
ErrorMsg : string;
functionname, param1, param2, xmlfilename : string;
mysection : TWorksection;
myscript : TuibInstScript;
myoutput: TXStringList;
scriptfilename : string;
i : integer;

begin
	optionlist := TStringlist.Create;
	optionlist.Append('help');
	optionlist.Append('version');
	optionlist.Append('scriptfile:');
	optionlist.Append('xmlfile:');
	optionlist.Append('functionname:');
	optionlist.Append('functionparameter1:');
	optionlist.Append('functionparameter2:');

  // quick check parameters
	ErrorMsg:= thisapp.CheckOptions('',optionlist);
  if ErrorMsg<>'' then begin
    thisapp.ShowException(Exception.Create(ErrorMsg));
    thisapp.Terminate;
    Exit;
  end;

  // parse parameters
  if thisapp.HasOption('help') or (paramcount=0) then begin
  //if thisapp.HasOption('help')  then begin
    WriteHelp;
		halt(myexitcode);
  end;
(*
	if thisapp.HasOption('functionname') then
	begin
			functionname := thisapp.GetOptionValue('functionname');
			if functionname = 'doXMLAddNamespace' then
			begin
				writeln('function found: '+functionname);
				if thisapp.HasOption('xmlfile') and
					 thisapp.HasOption('functionparameter1') and
					 thisapp.HasOption('functionparameter2') then
				begin
					param1 := thisapp.GetOptionValue('functionparameter1');
					param2 := thisapp.GetOptionValue('functionparameter2');
					xmlfilename := thisapp.GetOptionValue('xmlfile');

					if doXMLAddNamespace(xmlfilename, param1, param2) then
					begin
						myexitcode := 0;
					end
					else
					begin
						myexitcode := 1;
					end;

				end
				else
				begin
					writeln('Error: Not all needed parameters given');
					myexitcode := 10;
				end;
			end;
			halt(myexitcode);
	end;
*)

	if thisapp.HasOption('scriptfile') then
	begin
			scriptfilename := thisapp.GetOptionValue('scriptfile');
				//writeln('scriptfile found: '+scriptfilename);
				if thisapp.HasOption('xmlfile') then
				begin
					xmlfilename := thisapp.GetOptionValue('xmlfile');
					mysection := TWorksection.create(0);
					myscript := TuibInstScript.create;
					myoutput:= TXStringList.Create;
					mysection.LoadFromFile(scriptfilename);
					myscript.doXMLPatch(mysection,xmlfilename,myoutput);
					for i := 0 to myoutput.Count -1 do
						writeln(myoutput.strings[i]);
//					doXMLPatch
					(*
					if doXMLAddNamespace(xmlfilename, param1, param2) then
					begin
						myexitcode := 0;
					end
					else
					begin
						myexitcode := 1;
					end;
					*)
				end
				else
				begin
					writeln('Error: Not all needed parameters given');
					myexitcode := 10;
				end;
			mysection.Free;
			myscript.Free;
			myoutput.Free;
			halt(myexitcode);
	end;

	if thisapp.HasOption('wait') then
  begin
    	paramvaluestr := thisapp.GetOptionValue('wait');
   try
    waitsec := StrToInt(paramvaluestr);
    Sleep(waitsec*1000);
   except
    writeln('>'+paramvaluestr+'< is not a integer.');
   end;
  end;

  if thisapp.HasOption('fork-and-stop') then
  begin
     paramvaluestr := thisapp.GetOptionValue('fork-and-stop');

		halt(myexitcode);
    thisapp.Terminate;
  end;


  if thisapp.HasOption('time-output') then
  begin
		halt(myexitcode);
		thisapp.Terminate;
	end;

	if thisapp.HasOption('version') then
	begin
				halt(myexitcode);
		thisapp.Terminate;
	end;




	{ add your program here }
		halt(myexitcode);
		thisapp.Terminate;

end;


end.
