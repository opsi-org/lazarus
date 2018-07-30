unit osdbasedata;

{$mode delphi}

interface

uses
  Classes, SysUtils, LCLProc, LResources, TypInfo, Forms, Controls, Graphics,
  Dialogs, RTTICtrls, StdCtrls;

type

  TMyEnum = (MyEnum1,MyEnum2,MyEnum3);
  TMyRange = 3..7;


  TMyClass = class(TPersistent)
  private
    FMyEnum: TMyEnum;
    FMyRange: TMyRange;
    FMyString: string;
    FMyList: TStringlist;
    //procedure SetMyEnum(const AValue: TMyEnum);
    //procedure SetMyRange(const AValue: TMyRange);
    //procedure SetMyString(const AValue: string);
  published
    (*
    property MyString: string read FMyString write SetMyString;
    property MyEnum: TMyEnum read FMyEnum write SetMyEnum;
    property MyRange: TMyRange read FMyRange write SetMyRange;
    *)
    property MyString: string read FMyString write FMyString;
    property MyEnum: TMyEnum read FMyEnum write FMyEnum;
    property MyRange: TMyRange read FMyRange write FMyRange;
    property MyList: TStringlist read FMyList write FMyList;
  public
    constructor Create;
    destructor Destroy;
  end;

  (***********************************)
  TArchitecture = (a32, a64, aUnknown);
  TArchitectureMode = (am32only_fix, am64only_fix, amBoth_fix, amSystemSpecific_fix,
    amSelectable);
  TKnownInstaller = (stAdvancedMSI, stInno, stInstallShield, stInstallShieldMSI,
    stMsi, stNsis, st7zip, stUnknown);


  TdetectInstaller = function(parent: TClass; markerlist: TStringList): boolean;


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
    comment: string;
    Link: string;

    detected: TdetectInstaller;
    { public declarations }
    constructor Create;
    destructor Destroy;

  end;

  TInstallers = array of TInstallerData;

  TSetupFile =  class(TPersistent)
  private
    FsetupFileNamePath: string;
    FsetupFileName: string;
    FsetupFileSize: cardinal;     // MB
    Farchitecture: TArchitecture;
    FmsiId: string;
    FmstFileNamePath: string;
    FmstFileName: string;
    FmsiFullFileName: string;
    FistallerId: TKnownInstaller;
    FrequiredSpace: cardinal;      // MB
    FinstallDirectory: string;
    Fmarkerlist: TStringList;
    FSoftwareVersion: string;
  published
    // proc
    (*
    procedure SetSetupFileNamePath(const AValue: string);
    procedure SetSetupFileNamePath(const AValue: string);
    procedure SetSetupFileNamePath(const AValue: cardinal);
    *)
    procedure SetArchitecture(const AValue: TArchitecture);
    procedure SetSetupFullFileName(const AValue: string);
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
    property setupFileName: string read FsetupFileName write SetSetupFullFileName;
    property setupFileSize: cardinal  read FsetupFileSize write FsetupFileSize;
    property architecture: TArchitecture  read Farchitecture write Farchitecture;
    property msiId: string  read FmsiId write FmsiId;
    property mstFileNamePath:  string read FmstFileNamePath write FmstFileNamePath;
    property mstFileName: string  read FmstFileName write FmstFileName;
    property msiFullFileName:  string read FmsiFullFileName write FmsiFullFileName;
    property istallerId: TKnownInstaller  read FistallerId write FistallerId;
    property requiredSpace: cardinal  read FrequiredSpace write FrequiredSpace;
    property installDirectory: string  read FinstallDirectory write FinstallDirectory;
    property markerlist: TStringList  read Fmarkerlist write Fmarkerlist;
    property SoftwareVersion: string  read FSoftwareVersion write FSoftwareVersion;
    procedure initValues;

  public
    { public declarations }

    constructor Create;
    destructor Destroy;
  end;

  TProductProperies  =  class(TPersistent)
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
    Fpriority: integer;
    Fproducttype: string;
    Fsetupscript: string;
    Funinstallscript: string;
    Flicenserequired: boolean;
  published
    property architectureMode: TArchitectureMode read FarchitectureMode write FarchitectureMode;
    property comment: string read Fcomment write Fcomment;
    property description: string read Fdescription write Fdescription;
    property advice: string read Fadvice write Fadvice;
    property productId: string read FproductId write FproductId;
    property productName: string read FproductName write FproductName;
    property productversion: string read Fproductversion write Fproductversion;
    property packageversion: cardinal read Fpackageversion write Fpackageversion;
    property versionstr: string read Fversionstr write Fversionstr;
    property priority: integer read Fpriority write Fpriority;
    property producttype: string read Fproducttype write Fproducttype;
    property setupscript: string read Fsetupscript write Fsetupscript;
    property uninstallscript: string read Funinstallscript write Funinstallscript;
    property licenserequired: boolean read Flicenserequired write Flicenserequired;
  public
    { public declarations }
    //constructor Create;
    //destructor Destroy;
  end;

  TProductData = record
    SetupFiles : array[0..1] of TSetupFile;
    produktpropties : TProductProperies;
  end;

function archModeStrToArchmode(modestr: string): TArchitectureMode;
function installerToInstallerstr(installerId: TKnownInstaller): string;
function instIdToint(installerId: TKnownInstaller): integer;
procedure initaktproduct;

var
  aktProduct: TProductData;
  aktSetupFile : TSetupFile;
  knownInstallerList: TStringList;
  architectureModeList: TStringList;
  installerArray: TInstallers;
  counter: integer;
  myobject : TMyClass;

implementation
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
  markerlist := TStringList.Create;
  inherited;
  //initValues;
end;

destructor TSetupFile.Destroy;
begin
  markerlist.Free;
  inherited;
end;

procedure TSetupFile.SetArchitecture(const AValue: TArchitecture);
begin
  if AValue=Architecture then exit;
  FArchitecture:=AValue;
  //Log('SetArchitecture '+GetEnumProp(Self,'Architecture'));
end;

procedure TSetupFile.SetSetupFullFileName(const AValue: string);
begin
  if AValue=SetupFileNamePath+PathDelim+setupFileName then exit;
  FSetupFileNamePath:=ExtractFileDir(AValue);
  FsetupFileName:= ExtractFileName(AValue);
  //Log('SetSetupFileNamePath '+MyString);
end;

procedure TSetupFile.initValues;
begin
  FsetupFileNamePath := '';
  FsetupFileName := '';
  FsetupFileSize := 0;
  FmsiId := '';
  FmstFileNamePath := '';
  FmstFileName := '';
  //FmsiFullFileName := '';
  FistallerId := stUnknown;
  Fmarkerlist.Clear;
  Farchitecture:=aUnknown;
end;

// TProductProperies **********************************

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

procedure initSetupFile(var mysetupfile : TSetupFile);
begin
  with mysetupfile do
  begin
    //setupFileNamePath := '';
    setupFileName := '';
    setupFileSize := 0;
    msiId := '';
    mstFileNamePath := '';
    mstFileName := '';
    //msiFullFileName := '';
    istallerId := stUnknown;
    markerlist.Clear;
    architecture:=aUnknown;
  end;
end;

procedure initaktproduct;
var
  i : integer;
begin
  for i := 0 to 1 do
    initSetupFile(aktProduct.SetupFiles[i]);
  with aktProduct.produktpropties do
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
    uninstallscript := 'uninstall.psiscript';
    licenserequired := False;
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

  for counter := 0 to knownInstallerList.Count - 1 do
  begin
    SetLength(installerArray, counter + 1);
    installerArray[counter] := TInstallerData.Create;
    installerArray[counter].installerId := TKnownInstaller(counter);
    installerArray[counter].Name := knownInstallerList.Strings[counter];
  end;
  // inno
  installerArray[integer(stInno)].description := 'Inno Setup';
  installerArray[integer(stInno)].silentsetup :=
    '/sp- /verysilent /norestart /nocancel /SUPPRESSMSGBOXES';
  installerArray[integer(stInno)].unattendedsetup :=
    '/sp- /silent /norestart /nocancel /SUPPRESSMSGBOXES';
  installerArray[integer(stInno)].silentuninstall :=
    '/verysilent /norestart /nocancel /SUPPRESSMSGBOXES';
  installerArray[integer(stInno)].unattendeduninstall :=
    '/silent /norestart /nocancel /SUPPRESSMSGBOXES';
  installerArray[integer(stInno)].uninstall_waitforprocess := '';
  installerArray[integer(stInno)].patterns.Add('<description>inno setup</description>');
  installerArray[integer(stInno)].patterns.Add('jr.inno.setup');
  installerArray[integer(stInno)].link :=
    'http://www.jrsoftware.org/ishelp/topic_setupcmdline.htm';
  installerArray[integer(stInno)].comment := '';
  installerArray[integer(stInno)].detected := @detectedbypatternwithor;
  // NSIS
  installerArray[integer(stNsis)].description := 'Nullsoft Install System';
  installerArray[integer(stNsis)].silentsetup := '/S';
  installerArray[integer(stNsis)].unattendedsetup := '/S';
  installerArray[integer(stNsis)].silentuninstall := '/S';
  installerArray[integer(stNsis)].unattendeduninstall := '/S';
  installerArray[integer(stNsis)].uninstall_waitforprocess := 'Au_.exe';
  installerArray[integer(stNsis)].patterns.Add('Nullsoft.NSIS.exehead');
  installerArray[integer(stNsis)].patterns.Add('nullsoft install system');
  installerArray[integer(stNsis)].link :=
    'http://nsis.sourceforge.net/Docs/Chapter3.html#installerusage';
  installerArray[integer(stNsis)].comment := '';
  installerArray[integer(stNsis)].detected := @detectedbypatternwithor;
  // InstallShieldMSI
  installerArray[integer(stInstallShieldMSI)].description :=
    'InstallShield+MSI Setup (InstallShield with embedded MSI)';
  installerArray[integer(stInstallShieldMSI)].silentsetup :=
    '/s /v" /qn ALLUSERS=1 REBOOT=ReallySuppress"';
  installerArray[integer(stInstallShieldMSI)].unattendedsetup :=
    '/s /v" /qb-! ALLUSERS=1 REBOOT=ReallySuppress"';
  installerArray[integer(stInstallShieldMSI)].silentuninstall :=
    '/s /v" /qn ALLUSERS=1 REBOOT=ReallySuppress"';
  installerArray[integer(stInstallShieldMSI)].unattendeduninstall :=
    '/s /v" /qb-! ALLUSERS=1 REBOOT=ReallySuppress"';
  installerArray[integer(stInstallShieldMSI)].uninstall_waitforprocess := '';
  installerArray[integer(stInstallShieldMSI)].patterns.Add('nstallshield');
  installerArray[integer(stInstallShieldMSI)].patterns.Add('installer,msi,database');
  installerArray[integer(stInstallShieldMSI)].link :=
    'http://helpnet.flexerasoftware.com/installshield19helplib/helplibrary/IHelpSetup_EXECmdLine.htm';
  installerArray[integer(stInstallShieldMSI)].comment := '';
  installerArray[integer(stInstallShieldMSI)].detected := @detectedbypatternwithor;
  // InstallShield
  installerArray[integer(stInstallShield)].description :=
    'InstallShield Setup (classic)';
  installerArray[integer(stInstallShield)].silentsetup := '/s /sms';
  installerArray[integer(stInstallShield)].unattendedsetup := '/s /sms';
  installerArray[integer(stInstallShield)].silentuninstall := '/s /sms';
  installerArray[integer(stInstallShield)].unattendeduninstall := '/s /sms';
  installerArray[integer(stInstallShield)].uninstall_waitforprocess := '';
  installerArray[integer(stInstallShield)].patterns.Add('InstallShield');
  installerArray[integer(stInstallShield)].patterns.Add(
    '<description>InstallShield.Setup</description>');
  installerArray[integer(stInstallShield)].link :=
    'http://helpnet.flexerasoftware.com/installshield19helplib/helplibrary/IHelpSetup_EXECmdLine.htm';
  installerArray[integer(stInstallShield)].comment := '';
  installerArray[integer(stInstallShield)].detected := @detectedbypatternwithor;

  architectureModeList := TStringList.Create;
  architectureModeList.Add('32BitOnly - fix');
  architectureModeList.Add('64BitOnly - fix');
  architectureModeList.Add('both - fix');
  architectureModeList.Add('systemSpecific - fix');
  architectureModeList.Add('selectable');


  aktSetupFile := TSetupFile.Create;

end.
