unit osdbasedata;

{$mode delphi}

interface

uses
  Classes, SysUtils, LCLProc, LResources, TypInfo, Forms, Controls, Graphics,
  Dialogs, StdCtrls,
  {$IFDEF WINDOWS}
  osdhelper,
  shlobj,
  {$ENDIF WINDOWS}
  fpjsonrtti,
  oslog,
  RTTICtrls;

type

  TArchitecture = (a32, a64, aUnknown);
  TArchitectureMode = (am32only_fix, am64only_fix, amBoth_fix, amSystemSpecific_fix,
    amSelectable);
  TKnownInstaller = (stAdvancedMSI, stInno, stInstallShield, stInstallShieldMSI,
    stMsi, stNsis, st7zip, st7zipsfx, stUnknown);


  TdetectInstaller = function(parent: TClass; markerlist: TStrings): boolean;


  TInstallerData = class
  private
  public

    installerId: TKnownInstaller;
    Name: string;
    description: string;
    patterns: TStringList;
    silentsetup: string;
    unattendedsetup: string;
    silentuninstall: string;
    unattendeduninstall: string;
    uninstall_waitforprocess: string;
    uninstallProg: string;
    comment: string;
    Link: string;
    uib_exitcode_function: string;
    detected: TdetectInstaller;
    { public declarations }
    constructor Create;
    destructor Destroy;

  end;

  TInstallers = array of TInstallerData;

  TSetupFile = class(TPersistent)
  private
    FsetupFileNamePath: string;
    FsetupFileName: string;
    FsetupFullFileName: string;
    FsetupFileSize: cardinal;     // MB
    Farchitecture: TArchitecture;
    FmsiId: string;
    FMstFullFileName: string;
    FmstFileNamePath: string;
    FmstFileName: string;
    FmsiFullFileName: string;
    FinstallerId: TKnownInstaller;
    FrequiredSpace: cardinal;      // MB
    FinstallDirectory: string;
    Fmarkerlist: TStrings;
    FSoftwareVersion: string;
    Fwinbatch_del_argument: string;
    FinstallCommandLine: string;
    FuninstallCommandLine: string;
    FuninstallProg: string;
    FuninstallCheck: TStrings;
    FisExitcodeFatalFunction: string;
    Funinstall_waitforprocess: string;
    Fanalyze_progess: integer;
    procedure SetMarkerlist(const AValue: TStrings);
    procedure SetUninstallCheck(const AValue: TStrings);
  published
    // proc
    (*
    procedure SetSetupFileNamePath(const AValue: string);
    procedure SetSetupFileNamePath(const AValue: string);
    procedure SetSetupFileNamePath(const AValue: cardinal);
    *)
    procedure SetArchitecture(const AValue: TArchitecture);
    procedure SetSetupFullFileName(const AValue: string);
    procedure SetMstFullFileName(const AValue: string);
    (*
    procedure SetSetupFileNamePath(const AValue: string);
    procedure SetSetupFileNamePath(const AValue: string);
    procedure SetSetupFileNamePath(const AValue: TKnownInstaller);
    procedure SetSetupFileNamePath(const AValue: cardinal)
    procedure SetSetupFileNamePath(const AValue: string);;
    procedure SetSetupFileNamePath(const AValue: TStringList);
    procedure SetSetupFileNamePath(const AValue: string);
    *)
    property setupFileNamePath: string read FsetupFileNamePath;
    property setupFileName: string read FsetupFileName write FsetupFileName;
    property setupFullFileName: string read FsetupFullFileName
      write SetSetupFullFileName;
    property setupFileSize: cardinal read FsetupFileSize write FsetupFileSize;
    property architecture: TArchitecture read Farchitecture write Farchitecture;
    property msiId: string read FmsiId write FmsiId;
    property mstFullFileName: string read FMstFullFileName write SetMstFullFileName;
    property mstFileNamePath: string read FmstFileNamePath write FmstFileNamePath;
    property mstFileName: string read FmstFileName write FmstFileName;
    property msiFullFileName: string read FmsiFullFileName write FmsiFullFileName;
    property installerId: TKnownInstaller read FinstallerId write FinstallerId;
    property requiredSpace: cardinal read FrequiredSpace write FrequiredSpace;
    property installDirectory: string read FinstallDirectory write FinstallDirectory;
    property markerlist: TStrings read Fmarkerlist write SetMarkerlist;
    property SoftwareVersion: string read FSoftwareVersion write FSoftwareVersion;
    property winbatch_del_argument: string read Fwinbatch_del_argument
      write Fwinbatch_del_argument;
    property installCommandLine: string read FinstallCommandLine
      write FinstallCommandLine;
    property isExitcodeFatalFunction: string
      read FisExitcodeFatalFunction write FisExitcodeFatalFunction;
    property uninstallCommandLine: string read FuninstallCommandLine
      write FuninstallCommandLine;
    property uninstallProg: string read FuninstallProg write FuninstallProg;
    property uninstallCheck: TStrings read FuninstallCheck write SetUninstallCheck;
    property uninstall_waitforprocess: string
      read Funinstall_waitforprocess write Funinstall_waitforprocess;
    property analyze_progess: integer read Fanalyze_progess write Fanalyze_progess;
    procedure initValues;

  public
    { public declarations }

    constructor Create;
    destructor Destroy;
  end;

(*
Dependency for Action
Für welche Aktion des Produktes, welches Sie gerade erstellen, soll die Abhängigkeit gelten (setup, deinstall . . . ).
Required product id
Productid (Bezeichner) des Produkts zu dem eine Abhängigkeit besteht.
Required action
Sie können entweder eine Aktion anfordern oder (siehe unten) einen Status. Aktionen können z.B. sein : setup,
uninstall, update . . .
Required installation status
Status den das Produkt, zu dem eine Abhängigkeit besteht, haben soll (typischerweise installed). Liegt ein
anderer Status vor, so wird das Produkt auf setup gestellt.
Requirement type
Installationsreihenfolge. Wenn das Produkt, zu dem eine Abhängigkeit besteht, installiert sein muss bevor mit
der Installation des aktuellen Produkts begonnen werden kann, dann ist dies before. Muss es nach dem aktuellen
Produkt installiert werden so ist dies after. Ist die Reihenfolge egal so muss hier nichts eingetragen werden.

*)
(*
{ TMyCollectionItem }

 TMyCollectionItem = class(TCollectionItem)
 private
   FInt: integer;
   FStr: string;
   FColor: TColor;
 published
   property Color: TColor read FColor write FColor;
   property Str: string read FStr write FStr;
   property Int: integer read FInt write FInt;
 end;
*)

TPDtype = (before,after,doNotMatter);
//['before','after',''];
TPActionRequest = (setup,uninstall,update,noRequest);
TPInstallationState = (installed,not_installed,unknown, noState);

TPDependency = class(TCollectionItem)
  private
    FAction: String;
    FRequProductId: string;
    FRequAction: TPActionRequest;
    FRequState: TPInstallationState;
    FRequType: TPDtype;
  published
    property RequType: TPDtype read FRequType write FRequType;
    property action: string read FAction;
    property requProductId: string read FRequProductId write FRequProductId;
    property requState: TPInstallationState read FRequState write FRequState;
    property requAction: TPActionRequest read FRequAction write FRequAction;
    procedure init;
  public
    { public declarations }
    //constructor Create;
    //destructor Destroy;
  end;

(*

TPDependencies = class(TCollection)
  private

  published

  public
    { public declarations }
    constructor Create;
    //destructor Destroy;
  end;
 *)

(*
  [ProductProperty]
type: bool
name: force_newest_ubuntu
description: Update even to not published versions
default: False

[ProductProperty]
type: unicode
name: update_from_to
multivalue: False
editable: False
description: Which method to use
values: ["jessie_stretch", "trusty_xenial", "wheezy_jessie", "xenial_bionic"]
default: ["xenial_bionic"]
*)

TPPtype = (bool, unicode);

TPProperties = class(TPersistent)
  private
    Ftype: TPPtype;
    Fname: string;
    Fmultivalue: boolean;
    Feditable: boolean;
    Fdescription: string;
    FStrvalues: Tstrings;
    FStrDefault: Tstrings;
    FBoolDefault: boolean;
    procedure SetValueLines(const AValue: TStrings);
    procedure SetDefaultLines(const AValue: TStrings);
  published
    property ptype: TPPtype read Ftype write Ftype;
    property name: string read Fname write Fname;
    property description: string read Fdescription write Fdescription;
    property multivalue: boolean read Fmultivalue write Fmultivalue;
    property editable: boolean read Feditable write Feditable;
    property StrDefault: TStrings read FStrDefault write SetDefaultLines;
    property Strvalues: TStrings read FStrvalues write FStrvalues;
    property boolDefault: boolean read FBoolDefault write FBoolDefault;
  public
    { public declarations }
    constructor Create;
    destructor Destroy;
  end;

  TPriority  = -100..100;

  TProductData = class(TPersistent)
  private
    FarchitectureMode: TArchitectureMode;
    Fcomment: string;
    Fdescription: string;
    Fadvice: string;
    FproductId: string;
    FproductName: string;
    Fproductversion: string;
    Fpackageversion: cardinal;
    Fversionstr: string;
    Fpriority: TPriority;
    Fproducttype: string;
    Fsetupscript: string;
    Funinstallscript: string;
    Fdelsubscript: string;
    Flicenserequired: boolean;
    procedure SetPriority(const AValue: TPriority);
  published
    property architectureMode: TArchitectureMode
      read FarchitectureMode write FarchitectureMode;
    property comment: string read Fcomment write Fcomment;
    property description: string read Fdescription write Fdescription;
    property advice: string read Fadvice write Fadvice;
    property productId: string read FproductId write FproductId;
    property productName: string read FproductName write FproductName;
    property productversion: string read Fproductversion write Fproductversion;
    property packageversion: cardinal read Fpackageversion write Fpackageversion;
    property versionstr: string read Fversionstr write Fversionstr;
    property priority: TPriority read Fpriority write setpriority;
    property producttype: string read Fproducttype write Fproducttype;
    property setupscript: string read Fsetupscript write Fsetupscript;
    property uninstallscript: string read Funinstallscript write Funinstallscript;
    property delsubscript: string read Fdelsubscript write Fdelsubscript;
    property licenserequired: boolean read Flicenserequired write Flicenserequired;
  public
    { public declarations }
    //constructor Create;
    //destructor Destroy;
  end;

  TopsiProduct = class(TPersistent)
    private
          published
                public
    SetupFiles: array[0..1] of TSetupFile;
    productdata: TProductData;
    //dependeciesCount : integer;
    dependencies: TCollection;


    { public declarations }
    constructor Create;
  end;

  TConfiguration = class(TPersistent)
  private
    Fworkbench_share: string;
    Fworkbench_Path: string;
    Fworkbench_mounted: boolean;
    Fconfig_filled: boolean;
    FregisterInFilemanager: boolean;
    Femail_address: string;
    Fimport_libraries: TStrings;
    FpreInstallLines: TStrings;
    FpostInstallLines: TStrings;
    FpreUninstallLines: TStrings;
    FpostUninstallLines: TStrings;
    procedure SetLibraryLines(const AValue: TStrings);
    procedure SetPreInstallLines(const AValue: TStrings);
    procedure SetPostInstallLines(const AValue: TStrings);
    procedure SetPreUninstallLines(const AValue: TStrings);
    procedure SetPostUninstallLines(const AValue: TStrings);
  published
    property workbench_share: string read Fworkbench_share write Fworkbench_share;
    property workbench_Path: string read Fworkbench_Path write Fworkbench_Path;
    property workbench_mounted: boolean read Fworkbench_mounted write Fworkbench_mounted;
    property config_filled: boolean read Fconfig_filled write Fconfig_filled;
    property registerInFilemanager: boolean
      read FregisterInFilemanager write FregisterInFilemanager;
    property email_address: string read Femail_address write Femail_address;
    property import_libraries: TStrings read Fimport_libraries write SetLibraryLines;
    property preInstallLines: TStrings read FpreInstallLines write SetPreInstallLines;
    property postInstallLines: TStrings read FpostInstallLines write SetPostInstallLines;
    property preUninstallLines: TStrings read FpreUninstallLines
      write SetPreUninstallLines;
    property postUninstallLines: TStrings read FpostUninstallLines
      write SetPostUninstallLines;
    procedure writeconfig;
    procedure readconfig;
  public
    { public declarations }
    constructor Create;
    destructor Destroy;
  end;

  (*
  TConfigHint = class(TCollectionItem)
  private
    FName: String;
    FHint: Tstrings;
  published
    property Name: string read FName;
    property Hint: TStrings read FHint
  public
    { public declarations }
    //constructor Create;
    //destructor Destroy;
  end;
    *)



function archModeStrToArchmode(modestr: string): TArchitectureMode;
function installerToInstallerstr(installerId: TKnownInstaller): string;
function instIdToint(installerId: TKnownInstaller): integer;
procedure initaktproduct;
procedure freebasedata;

var
  aktProduct: TopsiProduct;
  aktSetupFile: TSetupFile;
  knownInstallerList: TStringList;
  architectureModeList: TStringList;
  installerArray: TInstallers;
  counter: integer;
  myconfiguration: TConfiguration;
  myconfigurationhints : TStringlist;
 // newhint : TConfigHint;
//myobject : TMyClass;

implementation

(*
{ TMyClass }

constructor TMyClass.Create;
begin
  FMyList := TStringList.Create;
  inherited;
end;

destructor TMyClass.Destroy;
begin
  FMyList.Free;
  inherited;
end;
*)
(*
procedure TMyClass.SetMyEnum(const AValue: TMyEnum);
begin
  if AValue=MyEnum then exit;
  FMyEnum:=AValue;
  //Log('TMyClass.SetMyEnum '+GetEnumProp(Self,'MyEnum'));
end;

procedure TMyClass.SetMyRange(const AValue: TMyRange);
begin
  if AValue=MyRange then exit;
  FMyRange:=AValue;
  //Log('TMyClass.SetMyRange '+IntToStr(MyRange));
end;

procedure TMyClass.SetMyString(const AValue: string);
begin
  if AValue=MyString then exit;
  FMyString:=AValue;
  //Log('TMyClass.SetMyString '+MyString);
end;
 *)
// TInstallerData ************************************

constructor TInstallerData.Create;
begin
  patterns := TStringList.Create;
  inherited;
end;

destructor TInstallerData.Destroy;
begin
  patterns.Free;
  inherited;
end;

// TSetupFile ************************************

constructor TSetupFile.Create;
begin
  Fmarkerlist := TStringList.Create;
  FuninstallCheck := TStringList.Create;
  inherited;
  //initValues;
end;

destructor TSetupFile.Destroy;
begin
  FreeAndNil(Fmarkerlist);
  FreeAndNil(FuninstallCheck);
  inherited;
end;

procedure TSetupFile.SetArchitecture(const AValue: TArchitecture);
begin
  if AValue = Architecture then
    exit;
  FArchitecture := AValue;
  //Log('SetArchitecture '+GetEnumProp(Self,'Architecture'));
end;

procedure TSetupFile.SetSetupFullFileName(const AValue: string);
begin
  if AValue = SetupFullFileName then
    exit;
  FsetupFullFileName := AValue;
  FSetupFileNamePath := ExtractFileDir(AValue);
  FsetupFileName := ExtractFileName(AValue);
  //Log('SetSetupFileNamePath '+MyString);
end;

procedure TSetupFile.SetMstFullFileName(const AValue: string);
begin
  if AValue = mstFullFileName then
    exit;
  FmstFullFileName := AValue;
  FmstFileNamePath := ExtractFileDir(AValue);
  FmstFileName := ExtractFileName(AValue);
  //Log('SetSetupFileNamePath '+MyString);
end;

procedure TSetupFile.SetUninstallCheck(const AValue: TStrings);
begin
  FuninstallCheck.Assign(AValue);
end;

procedure TSetupFile.SetMarkerlist(const AValue: TStrings);
begin
  Fmarkerlist.Assign(AValue);
end;

procedure TSetupFile.initValues;
begin
  FsetupFileNamePath := '';
  FsetupFileName := '';
  FsetupFullFileName := '';
  FsetupFileSize := 0;
  Farchitecture := aUnknown;
  FmsiId := '';
  FmstFullFileName := '';
  FmstFileNamePath := '';
  FmstFileName := '';
  FmsiFullFileName := '';
  FinstallerId := stUnknown;
  FrequiredSpace := 0;
  FinstallDirectory := '# SET THE INSTALL DIRECTORY #';
  Fmarkerlist.Clear;
  FSoftwareVersion := '0.0';
  Fwinbatch_del_argument := '';
  FinstallCommandLine := '';
  FuninstallCommandLine := '';
  FuninstallProg := '';
  FuninstallCheck.Clear;
  Fanalyze_progess:=0;
  FisExitcodeFatalFunction := 'isMsExitcodeFatal_short';
  Funinstall_waitforprocess := '';
end;

// TPProperties **********************************

constructor TPProperties.Create;
begin
  inherited;
  FStrvalues := TStringList.Create;
  FStrvalues := TStringList.Create;
  Ftype := bool;
  Fmultivalue := false;
  Feditable := false;
  FBoolDefault := false;
end;

destructor TPProperties.Destroy;
begin
  FreeAndNil(FStrvalues);
  FreeAndNil(FStrdefault);
  inherited;
end;

procedure TPProperties.SetValueLines(const AValue: TStrings);
begin
  FStrvalues.Assign(AValue);
end;

procedure TPProperties.SetDefaultLines(const AValue: TStrings);
begin
  FStrDefault.Assign(AValue);
end;


// TPDependency **********************************
procedure TPDependency.init;
begin
  FAction := 'setup';
  FRequType := doNotMatter;
  FRequAction:=noRequest;
  FRequState:=noState;
  FRequProductId:='';
end;

// TopsiProduct **********************************
constructor TopsiProduct.Create;
begin
  inherited;
  //initaktproduct;
end;

// TProductData **********************************
procedure TProductData.SetPriority(const AValue: TPriority);
begin
  if AValue=priority then exit;
  Fpriority:=AValue;
end;
// TopsiProduct **********************************

// TConfiguration ************************************

constructor TConfiguration.Create;
begin
  inherited;
  Fimport_libraries := TStringList.Create;
  FpreInstallLines := TStringList.Create;
  FpostInstallLines := TStringList.Create;
  FpreUninstallLines := TStringList.Create;
  FpostUninstallLines := TStringList.Create;
  readconfig;
end;

destructor TConfiguration.Destroy;
begin
  //writeconfig;
  FreeAndNil(Fimport_libraries);
  FreeAndNil(FpreInstallLines);
  FreeAndNil(FpostInstallLines);
  FreeAndNil(FpreUninstallLines);
  FreeAndNil(FpostUninstallLines);
  inherited;
end;

procedure TConfiguration.SetLibraryLines(const AValue: TStrings);
begin
  Fimport_libraries.Assign(AValue);
end;

procedure TConfiguration.SetPreInstallLines(const AValue: TStrings);
begin
  FpreInstallLines.Assign(AValue);
end;

procedure TConfiguration.SetPostInstallLines(const AValue: TStrings);
begin
  FpostInstallLines.Assign(AValue);
end;

procedure TConfiguration.SetPreUninstallLines(const AValue: TStrings);
begin
  FpreUninstallLines.Assign(AValue);
end;

procedure TConfiguration.SetPostUninstallLines(const AValue: TStrings);
begin
  FpostUninstallLines.Assign(AValue);
end;

(*
procedure TConfiguration.writeconfig;

  var
  myfilename : string;
  myconfig : Tstringlist;
  configDir :  Array[0..MaxPathLen] of Char; //Allocate memory
  fConfig : text;
begin
   // read persomal configuration
  configDir:='';
  {$IFDEF Windows}
  SHGetFolderPath(0,CSIDL_APPDATA,0,SHGFP_TYPE_CURRENT,configDir);
  {$ELSE}
  configDir := GetAppConfigDir(False);
  {$ENDIF WINDOWS}
  myfilename := configDir+PathDelim+'opsi.org'+PathDelim+'opsisetupdetector.cfg';
  if not DirectoryExists(configDir+PathDelim+'opsi.org') then
    CreateDir(configDir);

    myconfig := TStringlist.Create;
    myconfig.Add('workbench_share='+Fworkbench_share);
    myconfig.Add('workbench_path='+Fworkbench_Path);
    myconfig.Add('workbench_mounted='+booltostr(Fworkbench_mounted,true));
    myconfig.Add('config_filled='+booltostr(Fconfig_filled,true));
    myconfig.Add('registerInFilemanager='+booltostr(FregisterInFilemanager,true));
    myconfig.SaveToFile(myfilename);
    Application.ProcessMessages;
    FreeAndNil(myconfig);
end;
*)
(*
procedure TConfiguration.readconfig;
var
  myfilename : string;
  myconfig : Tstringlist;
  oldconfigDir,oldconfigFileName, tmpstr : string;
  configdir :  Array[0..MaxPathLen] of Char; //Allocate memory
  fConfig : text;
begin
   // read persomal configuration
   configDir:='';
  {$IFDEF Windows}
  SHGetFolderPath(0,CSIDL_APPDATA,0,SHGFP_TYPE_CURRENT,configDir);
  {$ELSE}
  configDir := GetAppConfigDir(False);
  {$ENDIF WINDOWS}
  myfilename := configDir+PathDelim+'opsi.org'+PathDelim+'opsisetupdetector.cfg';
  if FileExists(myfilename) then
  begin
    myconfig := TStringlist.Create;
    myconfig.LoadFromFile(myfilename);
    Fworkbench_share := myconfig.Values['workbench_share'];
    Fworkbench_Path := myconfig.Values['workbench_path'];
    Fworkbench_mounted := false;
    Fconfig_filled := strToBool(myconfig.Values['config_filled']);
    FregisterInFilemanager := strToBool(myconfig.Values['registerInFilemanager']);
    myconfig.Free;
  end
  else
  begin
    tmpstr := '';
    // check for old config
    // get global ConfigDir
  oldconfigDir := GetAppConfigDir(True);
  oldconfigFileName := oldconfigDir + 'config.txt';
  if FileExists(oldconfigFileName) then
  begin
    AssignFile(fConfig, oldconfigFileName);
    Reset(fConfig);
    if not EOF(fConfig) then
      ReadLn(fConfig, tmpstr);
    CloseFile(fConfig);
  end;
    // init empty
    Fworkbench_share := '';
    Fworkbench_Path := tmpstr;
    Fworkbench_mounted := false;
    if tmpstr = '' then Fconfig_filled := false
    else  Fconfig_filled := true;
  end;
  Fworkbench_mounted := false;
  {$IFDEF WINDOWS}
  registerForExplorer(FregisterInFilemanager);
  {$ELSE}
  {$ENDIF WINDOWS}
end;
*)

procedure TConfiguration.writeconfig;
var
  Streamer: TJSONStreamer;
  JSONString: string;
  myfilename: string;
  configDir: array[0..MaxPathLen] of char; //Allocate memory
  configDirUtf8: UTF8String;
  myfile: TextFile;

  // http://wiki.freepascal.org/File_Handling_In_Pascal
  // SaveStringToFile: function to store a string of text into a diskfile.
  //   If the function result equals true, the string was written ok.
  //   If not then there was some kind of error.
  function SaveStringToFile(theString, filePath: ansistring): boolean;
  var
    fsOut: TFileStream;
  begin
    // By default assume the writing will fail.
    Result := False;

    // Write the given string to a file, catching potential errors in the process.
    try
      fsOut := TFileStream.Create(filePath, fmCreate);
      fsOut.Write(theString[1], length(theString));
      fsOut.Free;

      // At his point it is known that the writing went ok.
      Result := True

    except
      on E: Exception do
        LogDatei.log('String could not be written. Details: ' + E.ClassName +
          ': ' + E.Message, LLError);
    end;
  end;

begin
  configDir := '';
  {$IFDEF Windows}
  SHGetFolderPath(0, CSIDL_APPDATA, 0, SHGFP_TYPE_CURRENT, configDir);
  configDir := configDir + PathDelim + 'opsi.org' + PathDelim;
  {$ELSE}
  configDir := GetAppConfigDir(False);
  {$ENDIF WINDOWS}
  configDirUtf8 := configDir;
  configDirUtf8 := StringReplace(configDirUtf8, 'opsi-setup-detector',
    'opsi.org', [rfReplaceAll]);
  configDirUtf8 := StringReplace(configDirUtf8, 'opsisetupdetector',
    'opsi.org', [rfReplaceAll]);
  myfilename := configDirUtf8 + PathDelim + 'opsisetupdetector.cfg';
  myfilename := ExpandFileName(myfilename);
  if not DirectoryExists(configDirUtf8) then
    if not ForceDirectories(configDirUtf8) then
      LogDatei.log('failed to create configuration directory: ' + configDirUtf8, LLError);

  // http://wiki.freepascal.org/Streaming_JSON
  Streamer := TJSONStreamer.Create(nil);
  try
    Streamer.Options := Streamer.Options + [jsoTStringsAsArray];
    // Save strings as JSON array
    // JSON convert and output
    JSONString := Streamer.ObjectToJSONString(myconfiguration);
    logdatei.log(JSONString, LLDebug);
    if not SaveStringToFile(JSONString, myfilename) then
      LogDatei.log('failed save configuration', LLError);
    //AssignFile(myfile, myfilename);
    //Rewrite(myfile);
    //Write(myfile, JSONString);
  finally
    //CloseFile(myfile);
    Streamer.Destroy;
  end;
end;


procedure TConfiguration.readconfig;
var
  DeStreamer: TJSONDeStreamer;
  Streamer: TJSONStreamer;
  JSONString: string;
  myfilename: string;
  configDir: array[0..MaxPathLen] of char; //Allocate memory
  configDirUtf8: UTF8String;
  configDirstr: string;
  myfile: Text;
  oldconfigDir, oldconfigFileName, tmpstr: string;
  fConfig: Text;

  // http://wiki.freepascal.org/File_Handling_In_Pascal
  // LoadStringFromFile: function to load a string of text from a diskfile.
  //   If the function result equals true, the string was load ok.
  //   If not then there was some kind of error.
  function LoadStringFromFile(theString, filePath: ansistring): boolean;
  var
    fsOut: TFileStream;
  begin
    // By default assume the writing will fail.
    Result := False;

    // Write the given string to a file, catching potential errors in the process.
    try
      fsOut := TFileStream.Create(filePath, fmOpenRead);
      fsOut.Read(theString[1], length(theString));
      fsOut.Free;

      // At his point it is known that the writing went ok.
      Result := True

    except
      on E: Exception do
        LogDatei.log('String could not be written. Details: ' + E.ClassName +
          ': ' + E.Message, LLError);
    end;
  end;

begin
  configDir := '';
  {$IFDEF Windows}
  SHGetFolderPath(0, CSIDL_APPDATA, 0, SHGFP_TYPE_CURRENT, configDir);
  configDir := configDir + PathDelim + 'opsi.org' + PathDelim;
  {$ELSE}
  configDir := GetAppConfigDir(False);
  {$ENDIF WINDOWS}
  configDirstr := configDir;
  configDirUtf8 := configDirstr;
  configDirUtf8 := StringReplace(configDirUtf8, 'opsi-setup-detector',
    'opsi.org', [rfReplaceAll]);
  configDirUtf8 := StringReplace(configDirUtf8, 'opsisetupdetector',
    'opsi.org', [rfReplaceAll]);
  myfilename := configDirUtf8 + PathDelim + 'opsisetupdetector.cfg';
  myfilename := ExpandFileName(myfilename);
  if FileExists(myfilename) then
  begin
    AssignFile(myfile, myfilename);
    Reset(myfile);
    readln(myfile, JSONString);
    //// http://wiki.freepascal.org/Streaming_JSON
    // DeStreamer object create
    DeStreamer := TJSONDeStreamer.Create(nil);
    try
      // Load JSON data in the object
      DeStreamer.JSONToObject(JSONString, myconfiguration);
      // Cleanup
    finally
      DeStreamer.Destroy;
      CloseFile(myfile);
    end;
    (*
    Streamer := TJSONStreamer.Create(nil);
    try
      Streamer.Options := Streamer.Options + [jsoTStringsAsArray];
      JSONString := Streamer.ObjectToJSONString(myconfiguration);
//      logdatei.log('After readconfig: '+JSONString, LLDebug);
    finally
      Streamer.Destroy;
    end;
    *)
  end
  else
  begin
    tmpstr := '';
    // check for old config
    // get global ConfigDir
    oldconfigDir := GetAppConfigDir(True);
    oldconfigFileName := oldconfigDir + 'config.txt';
    if FileExists(oldconfigFileName) then
    begin
      AssignFile(fConfig, oldconfigFileName);
      Reset(fConfig);
      if not EOF(fConfig) then
        ReadLn(fConfig, tmpstr);
      CloseFile(fConfig);
    end;
  end;
  Fworkbench_mounted := False;
end;


// Installer related ************************************

function archModeStrToArchmode(modestr: string): TArchitectureMode;
begin
  Result := TArchitectureMode(architectureModeList.IndexOf(modestr));
end;

function installerToInstallerstr(installerId: TKnownInstaller): string;
begin
  Result := knownInstallerList.Strings[integer(installerId)];
end;

function instIdToint(installerId: TKnownInstaller): integer;
begin
  Result := integer(installerId);
end;

function detecteddummy(parent: TClass; markerlist: TStringList): boolean;
var
  i1, i2, i3, i4, i5, i6: integer;
begin
  Result := False;
end;


function detectedinno(parent: TClass; markerlist: TStringList): boolean;
var
  i1, i2, i3, i4, i5, i6: integer;
begin
  Result := False;
  markerlist.Sort;
  if markerlist.Find(TInstallerData(parent).patterns[0], i1) or
    markerlist.Find(TInstallerData(parent).patterns[1], i2) then
    Result := True;
end;

function detectedbypatternwithor(parent: TClass; markerlist: TStringList): boolean;
var
  tmpint: integer;
  patternindex: integer;
  pattern: string;
begin
  Result := False;
  //markerlist.Sort;
  for patternindex := 0 to TInstallerData(parent).patterns.Count - 1 do
  begin
    pattern := TInstallerData(parent).patterns[patternindex];
    if markerlist.IndexOf(LowerCase(pattern)) >= 0 then
      Result := True;
  end;
end;

function detectedbypatternwithAnd(parent: TClass; markerlist: TStringList): boolean;
var
  tmpint: integer;
  patternindex: integer;
  pattern: string;
  numberOfPatterndetected : integer =0;
begin
  Result := False;
  //markerlist.Sort;
  for patternindex := 0 to TInstallerData(parent).patterns.Count - 1 do
  begin
    pattern := TInstallerData(parent).patterns[patternindex];
    if markerlist.IndexOf(LowerCase(pattern)) >= 0 then
      inc(numberOfPatterndetected);
  end;
  if numberOfPatterndetected = TInstallerData(parent).patterns.Count then
    Result := true;
end;


(*
procedure initSetupFile(var mysetupfile : TSetupFile);
begin
  with mysetupfile do
  begin
    setupFullFileName := '';
    setupFileNamePath := '';
    setupFileName := '';
    setupFileSize := 0;
    msiId := '';
    mstFileNamePath := '';
    mstFileName := '';
    msiFullFileName := '';
    installerId := stUnknown;
    markerlist.Clear;
    architecture:=aUnknown;
    requiredSpace := 0;
    installDirectory:='';
    SoftwareVersion :='';
  end;
end;
*)
procedure initaktproduct;
var
  i: integer;
  newdep : TPDependency;
begin
  for i := 0 to 1 do
  begin
    if not Assigned(aktProduct.SetupFiles[i]) then
      aktProduct.SetupFiles[i] := TSetupFile.Create;
    aktProduct.SetupFiles[i].initValues;
  end;
  if not Assigned(aktProduct.productdata) then
    aktProduct.productdata := TProductData.Create;
  with aktProduct.productdata do
  begin
    comment := '';
    description := '';
    advice := '';
    productId := '';
    productName := '';
    productversion := '';
    packageversion := 1;
    versionstr := '';
    priority := 0;
    producttype := 'localboot';
    setupscript := 'setup.opsiscript';
    uninstallscript := 'uninstall.opsiscript';
    delsubscript := 'delsub.opsiscript';
    licenserequired := False;
  end;
  // Create Dependencies
  aktProduct.dependencies := TCollection.Create(TPDependency);
  newdep := TPDependency(aktProduct.dependencies.add);
  newdep.init;
end;

procedure freebasedata;
var
  i: integer;
begin
  for i := 0 to 1 do
  begin
    if Assigned(aktProduct.SetupFiles[i]) then
      aktProduct.SetupFiles[i].Destroy;
  end;
  if Assigned(aktProduct.productdata) then
    aktProduct.productdata.Destroy;
  if Assigned(aktProduct.dependencies) then
    FreeAndNil(aktProduct.dependencies);
  if Assigned(myconfiguration) then
  begin
    myconfiguration.writeconfig;
    myconfiguration.Destroy;
  end;
end;


begin
  knownInstallerList := TStringList.Create;
  knownInstallerList.Add('AdvancedMSI');
  knownInstallerList.Add('Inno');
  knownInstallerList.Add('InstallShield');
  knownInstallerList.Add('InstallShieldMSI');
  knownInstallerList.Add('MSI');
  knownInstallerList.Add('NSIS');
  knownInstallerList.Add('7zip');
  knownInstallerList.Add('7zipsfx');
  knownInstallerList.Add('Unknown');


  for counter := 0 to knownInstallerList.Count - 1 do
  begin
    SetLength(installerArray, counter + 1);
    installerArray[counter] := TInstallerData.Create;
    installerArray[counter].installerId := TKnownInstaller(counter);
    installerArray[counter].Name := knownInstallerList.Strings[counter];
  end;


  // inno
  with installerArray[integer(stInno)] do
  begin
    description := 'Inno Setup';
    silentsetup :=
      '/sp- /verysilent /norestart /nocancel /SUPPRESSMSGBOXES';
    unattendedsetup :=
      '/sp- /silent /norestart /nocancel /SUPPRESSMSGBOXES';
    silentuninstall :=
      '/verysilent /norestart /nocancel /SUPPRESSMSGBOXES';
    unattendeduninstall :=
      '/silent /norestart /nocancel /SUPPRESSMSGBOXES';
    uninstall_waitforprocess := '';
    uninstallProg := 'unins000.exe';
    patterns.Add('<description>inno setup</description>');
    patterns.Add('jr.inno.setup');
    link :=
      'http://www.jrsoftware.org/ishelp/topic_setupcmdline.htm';
    comment := '';
    uib_exitcode_function := 'isInnoExitcodeFatal';
    detected := @detectedbypatternwithor;
  end;

  // NSIS
  with installerArray[integer(stNsis)] do
  begin
    description := 'Nullsoft Install System';
    silentsetup := '/S';
    unattendedsetup := '/S';
    silentuninstall := '/S';
    unattendeduninstall := '/S';
    uninstall_waitforprocess := '/WaitForProcessEnding "Au_.exe" /TimeOutSeconds 20';
    uninstallProg := 'uninstall.exe';
    patterns.Add('Nullsoft.NSIS.exehead');
    patterns.Add('nullsoft install system');
    patterns.Add('http://nsis.sf.net/');
    link :=
      'http://nsis.sourceforge.net/Docs/Chapter3.html#installerusage';
    comment := '';
    uib_exitcode_function := 'isNsisExitcodeFatal';
    detected := @detectedbypatternwithor;

  end;
  // InstallShield
  with installerArray[integer(stInstallShield)] do
  begin
    description :=
      'InstallShield Setup (classic)';
    silentsetup := '/s /sms';
    unattendedsetup := '/s /sms';
    silentuninstall := '/s /sms';
    unattendeduninstall := '/s /sms';
    uninstall_waitforprocess := '';
    uninstallProg := 'uninstall.exe';
    patterns.Add('InstallShield');
    patterns.Add(
      '<description>InstallShield.Setup</description>');
    link :=
      'http://helpnet.flexerasoftware.com/installshield19helplib/helplibrary/IHelpSetup_EXECmdLine.htm';
    comment := '';
    uib_exitcode_function := 'isInstallshieldExitcodeFatal';
    detected := @detectedbypatternwithAnd;
  end;
  // InstallShieldMSI
  with installerArray[integer(stInstallShieldMSI)] do
  begin
    description :=
      'InstallShield+MSI Setup (InstallShield with embedded MSI)';
    silentsetup :=
      '/s /v" /qn ALLUSERS=1 REBOOT=ReallySuppress"';
    unattendedsetup :=
      '/s /v" /qb-! ALLUSERS=1 REBOOT=ReallySuppress"';
    silentuninstall :=
      '/s /v" /qn ALLUSERS=1 REBOOT=ReallySuppress"';
    unattendeduninstall :=
      '/s /v" /qb-! ALLUSERS=1 REBOOT=ReallySuppress"';
    uninstall_waitforprocess := '';
    uninstallProg := 'uninstall.exe';
    patterns.Add('nstallshield');
    patterns.Add('installer,msi,database');
    link :=
      'http://helpnet.flexerasoftware.com/installshield19helplib/helplibrary/IHelpSetup_EXECmdLine.htm';
    comment := '';
    uib_exitcode_function := 'isInstallshieldExitcodeFatal';
    detected := @detectedbypatternwithAnd;
  end;
  // MSI
  with installerArray[integer(stMSI)] do
  begin
    description :=
      'MSI Setup';
    silentsetup :=
      '/l* "$LogDir$\$ProductId$.install_log.txt" /qn ALLUSERS=1 REBOOT=ReallySuppress';
    unattendedsetup :=
      '/l* "$LogDir$\$ProductId$.install_log.txt" /qb-! ALLUSERS=1 REBOOT=ReallySuppress';
    silentuninstall :=
      ' /qn REBOOT=ReallySuppress';
    unattendeduninstall :=
      ' /qb-! REBOOT=ReallySuppress';
    uninstall_waitforprocess := '';
    uninstallProg := '';
    //patterns.Add('nstallshield');
    //patterns.Add('installer,msi,database');
    link :=
      'http://helpnet.flexerasoftware.com/installshield19helplib/helplibrary/IHelpSetup_EXECmdLine.htm';
    comment := '';
    uib_exitcode_function := 'isMsiExitcodeFatal';
    detected := @detectedbypatternwithor;
  end;

  // 7zip
  with installerArray[integer(st7zip)] do
  begin
    description := '7zip installer';
    silentsetup := '/S';
    unattendedsetup := '/S';
    silentuninstall := '/S';
    unattendeduninstall := '/S';
    uninstall_waitforprocess := '';
    uninstallProg := 'uninstall.exe';
    patterns.Add('7-Zip Installer');
    link := 'https://www.7-zip.org/faq.html';
    comment := '';
    uib_exitcode_function := 'isMsExitcodeFatal_short';
    detected := @detectedbypatternwithor;
  end;
  // st7zipsfx
  with installerArray[integer(st7zipsfx)] do
  begin
    description := '7zipsfx installer';
    silentsetup := '-y';
    unattendedsetup := '-y';
    silentuninstall := '-y';
    unattendeduninstall := '-y';
    uninstall_waitforprocess := '';
    uninstallProg := '';
    patterns.Add('7zipsfx');
    link := 'https://sourceforge.net/p/s-zipsfxbuilder/code/ci/master/tree/7zSD_EN.chm';
    comment := '';
    uib_exitcode_function := 'isMsExitcodeFatal_short';
    detected := @detectedbypatternwithor;
  end;



  architectureModeList := TStringList.Create;
  architectureModeList.Add('32BitOnly - fix');
  architectureModeList.Add('64BitOnly - fix');
  architectureModeList.Add('both - fix');
  architectureModeList.Add('systemSpecific - fix');
  architectureModeList.Add('selectable');

  myconfiguration := TConfiguration.Create;
  myconfiguration.readconfig;

  //aktSetupFile := TSetupFile.Create;
  aktProduct := TopsiProduct.Create;

    // Create Config Hints
    myconfigurationhints := TStringList.Create;
    myconfigurationhints.Add('workbench_Path=Path to the opsi_workbench');
    myconfigurationhints.Add('preInstallLines=opsi-script code, that will be included before the start of the installation.');
    myconfigurationhints.Add('workbench_mounted=Automatically detected. Is the opsi workbench reachable at workbench_Path.');
    myconfigurationhints.Add('config_filled=Automatically detected. Do we have all needed configurations');
    myconfigurationhints.Add('registerInFilemanager=Should this progrem redistred to the Filemanger (Explorer) context menu ?');
    myconfigurationhints.Add('email_address=Your email address, used for the changelog entry');
    myconfigurationhints.Add('import_libraries=List of opsi-script libraries that have to be imported.'+LineEnding+'One per line. May be empty. Example:'+LineEnding+'myinstallhelperlib.opsiscript');
    myconfigurationhints.Add('preInstallLines=List of opsi-script code lines that should be included before the installation starts.'+LineEnding+'One per line. May be empty. Example:'+LineEnding+'comment "Start the installation ..."');
    myconfigurationhints.Add('postInstallLines=List of opsi-script code lines that should be included after the installation finished.'+LineEnding+'One per line. May be empty. Example:'+LineEnding+'comment "Installation finished..."');
    myconfigurationhints.Add('preUninstallLines=List of opsi-script code lines that should be included before the uninstallation starts.'+LineEnding+'One per line. May be empty. Example:'+LineEnding+'comment "Start the uninstallation ..."');
    myconfigurationhints.Add('postUninstallLines=List of opsi-script code lines that should be included after the uninstallation finished.'+LineEnding+'One per line. May be empty. Example:'+LineEnding+'comment "Uninstall finished..."');
(*
  myconfigurationhints := TCollection.Create(TConfigHint);
  newhint := TConfigHint(myconfigurationhints.add);
  newhint.FName:= workbench_Path;
  newhint.FHint := TStringList.Create;
  newhint.FHint.Add('Path to the opsi_workbench');
  newhint := TConfigHint(myconfigurationhints.add);
  newhint.FName:= preInstallLines;
  newhint.FHint := TStringList.Create;
  newhint.FHint.Add('opsi-script code,);
  newhint.FHint.Add('that will be included');
  newhint.FHint.Add('before the start of the installation.');
 *)

end.
