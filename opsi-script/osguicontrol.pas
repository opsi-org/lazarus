unit osGUIControl;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, IniFiles, osEncoding;

type
  TBatchWindowMode = (bwmNotActivated, bwmIcon, bwmNormalWindow, bwmMaximized);

  //TSenderID might be useful in the future but not necessary now, just an idea to rember
  //TSenderID = (seBuildPC, seCentralFormFormCreate, seChangeProductStatusOnReinst, seInstScriptdoInfo);

  //Expand/include here further IDs if necessary
  TMessageID = (mInfo, mDetail, mCommand, mProgress, mVersion, mProduct, mActivity);
  TProgressValueID = (pInteger, pPercent);
  TElementID = (eMainForm, eLabelInfo, eLabelDetail, eLabelCommand,
    eLabelProgress, eLabelVersion, eLabelProduct, ePanel1, ePanel2, eImage1,
    eImage2, eLogo1, eLogo2, eProductImage1, eProductImage2, eActivityBar,
    eTimerProcessMess, eProgressBar);

  { TGUIControl }

  TGUIControl = class(TForm)
    class function GetGUITheme(const SkinDirectory: string):string;
    class function GetSkinDirectory(const SkinDirectory: string = ''):string;
  public
    procedure LoadSkin(const SkinDirectory: string);virtual;
    procedure SetMessageText(MessageText: string; MessageID: TMessageID); virtual;
    procedure SetProgress(Progress: integer; ProgressValueID: TProgressValueID); virtual;
    procedure SetForceStayOnTop(ForceStayOnTop: boolean); virtual;
    procedure SetBatchWindowMode(BatchWindowMode:TBatchWindowMode); virtual;
    procedure SetElementVisible(Visible:boolean; ElementID:TElementID); virtual;
    procedure SetElementEnabled(Enabled: boolean; ElementID:TElementID); virtual;
    procedure BringElementToFront(ElementID:TElementID); virtual;
    procedure SetElementTop(Top: integer; ElementID:TElementID); virtual;
    procedure SetElementLeft(Left: integer; ElementID:TElementID); virtual;
    procedure SetWindowPosition(Position:TPosition); virtual;
    procedure SetPicture(const BitmapFile: string; const theLabel: string); virtual;
    procedure SetTracingLevel(const Level: integer);virtual;
    //procedure SetHorizontalTextAlignment(HorizontalTextAlignment: TAlignment; ElementID: TElementID);virtual;abstract;
  end;

const
  {$IFDEF WINDOWS}
  skindirectoryDefault = 'winstskin';
  skindirectoryCustomWin = '..'+PathDelim+'custom'+PathDelim+'winstskin';
  skindirectoryDevelopment = 'winstskin';
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
  skindirectoryDevelopment = 'winstskin';
  skindirectoryDefault = '/usr/share/opsi-script/skin';
  skindirectoryCustomWin = '/usr/share/opsi-script/customskin';
  {$ENDIF LINUX}
  {$IFDEF DARWIN}
  skindirectoryDevelopment = 'winstskin';
  skindirectoryDefault = '../Resources/skin';
  skindirectoryCustomWin = '/usr/local/share/opsi-script/customskin';
  {$ENDIF DARWIN}

var
  FBatchOberflaeche: TGUIControl;//TFBatchOberflaeche;

implementation

{ TGUIControl }

class function TGUIControl.GetGUITheme(const SkinDirectory: string): string;
var
  ThemeFile: TIniFile;
begin
  //GetSkinFilePath(SkinDirectory);
  ThemeFile := TIniFile.Create(GetSkinDirectory(SkinDirectory) + PathDelim + 'skin.ini');
  Result := ThemeFile.ReadString('Window', 'Theme' , 'Default');
  ThemeFile.Free;
end;

class function TGUIControl.GetSkinDirectory(const SkinDirectory: string = ''): string;
var
  paramstr0enc: string;
  skindir: string;
begin
  {$IFDEF WINDOWS}
  paramstr0enc := reencode(ParamStr(0),'system');
  if FileExists(skindirectory+PathDelim+'skin.ini') then
    skindir := skindirectory
  else if FileExists(ExtractFilePath(paramstr0enc) + skindirectoryCustomWin+PathDelim+'skin.ini') then
    skinDir := ExtractFilePath(paramstr0enc) + skindirectoryCustomWin
  else if FileExists(ExtractFilePath(paramstr0enc) + skindirectoryDefault+PathDelim+'skin.ini') then
    skinDir := ExtractFilePath(paramstr0enc) + skindirectoryDefault
  else if FileExists(ExtractFilePath(paramstr0enc) + skindirectoryDevelopment+PathDelim+'skin.ini') then
    skinDir := ExtractFilePath(paramstr0enc) + skindirectoryDevelopment;
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
   if FileExists(skindirectory+PathDelim+'skin.ini') then
    skindir := skindirectory
  else if FileExists(skindirectoryCustomWin+PathDelim+'skin.ini') then
    skinDir := skindirectoryCustomWin
  else if FileExists(skindirectoryDefault+PathDelim+'skin.ini') then
    skinDir := skindirectoryDefault
  else if FileExists(ExtractFilePath(ParamStr(0)) + skindirectoryDevelopment+PathDelim+'skin.ini') then
    skinDir := ExtractFilePath(ParamStr(0)) + skindirectoryDevelopment;
  {$ENDIF LINUX}
  {$IFDEF DARWIN}
  paramstr0enc := reencode(ParamStr(0),'system');
  if FileExists(skindirectory+PathDelim+'skin.ini') then
    skindir := skindirectory
  else if FileExists(ExtractFilePath(paramstr0enc) + skindirectoryCustomWin+PathDelim+'skin.ini') then
    skinDir := ExtractFilePath(paramstr0enc) + skindirectoryCustomWin
  else if FileExists(ExtractFilePath(paramstr0enc) + skindirectoryDefault+PathDelim+'skin.ini') then
    skinDir := ExtractFilePath(paramstr0enc) + skindirectoryDefault
  else if FileExists(ExtractFilePath(paramstr0enc) + skindirectoryDevelopment+PathDelim+'skin.ini') then
    skinDir := ExtractFilePath(paramstr0enc) + skindirectoryDevelopment;
  {$ENDIF DARWIN}
  //skinDir := ExtractFilePath(paramstr0enc) + skindirectoryDevelopment;
  Result:= skinDir;
end;

procedure TGUIControl.LoadSkin(const SkinDirectory: string);
begin

end;

procedure TGUIControl.SetMessageText(MessageText: string; MessageID: TMessageID
  );
begin

end;

procedure TGUIControl.SetProgress(Progress: integer;
  ProgressValueID: TProgressValueID);
begin

end;

procedure TGUIControl.SetForceStayOnTop(ForceStayOnTop: boolean);
begin

end;

procedure TGUIControl.SetBatchWindowMode(BatchWindowMode: TBatchWindowMode);
begin

end;

procedure TGUIControl.SetElementVisible(Visible: boolean; ElementID: TElementID
  );
begin

end;

procedure TGUIControl.SetElementEnabled(Enabled: boolean; ElementID: TElementID
  );
begin

end;

procedure TGUIControl.BringElementToFront(ElementID: TElementID);
begin

end;

procedure TGUIControl.SetElementTop(Top: integer; ElementID: TElementID);
begin

end;

procedure TGUIControl.SetElementLeft(Left: integer; ElementID: TElementID);
begin

end;

procedure TGUIControl.SetWindowPosition(Position: TPosition);
begin

end;

procedure TGUIControl.SetPicture(const BitmapFile: string; const theLabel: string);
begin

end;

procedure TGUIControl.SetTracingLevel(const Level: integer);
begin

end;

end.

