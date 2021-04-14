unit osGUIControl;
{ Contains the class TGUIControl. Forms that come to front in batch mode must
  be subclasses of this class. Do not use TGUIControl direct. It is used
  like an abstract class but provides empty procedures so that in the
  subclass only the procedures necessary to get the desired GUI behaviour
  had to be overriden

  This code is part of the opsi.org project

  Copyright (c) uib gmbh (www.uib.de)
  This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
  and published under the Terms of the GNU Affero General Public License.
  Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
  author: Detlef Oertel, Jan Werner
  credits: http://www.opsi.org/credits/ }

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, IniFiles, osEncoding;

type
  TBatchWindowMode = (bwmNotActivated, bwmIcon, bwmNormalWindow, bwmMaximized);

  { The following IDs are used to specify the message, progress or element type.
    Based on these IDs the GUI can than be adapted accordingly. For example
    override SetMessageText in the subclass like this:

    procedure SetMessageText(MessageText: string; MessageID: TMessageID); override;
    begin
      case MessageID of
        mInfo: LabelInfo.Caption  := MessageText;
        mDetail: MemoXY.Text := MessageText;
      end;
    end; }

  { Expand/include here further IDs if necessary. }
  TMessageID = (mInfo, mDetail, mCommand, mProgress, mVersion, mProduct, mActivity);
  TProgressValueID = (pInteger, pPercent);
  TElementID = (eMainForm, eLabelInfo, eLabelDetail, eLabelCommand,
    eLabelProgress, eLabelVersion, eLabelProduct, ePanel1, ePanel2, eImage1,
    eImage2, eLogo1, eLogo2, eProductImage1, eProductImage2, eActivityBar,
    eTimerProcessMess, eProgressBar);
  { TSenderID might be useful in the future but not necessary now }
  //TSenderID = (seBuildPC, seCentralFormFormCreate, seChangeProductStatusOnReinst, seInstScriptdoInfo);

  { TGUIControl }

  TGUIControl = class(TForm)
    { GetGUITheme gives back the name of the theme from skin.ini }
    class function GetGUITheme(const SkinDirectory: string):string;
    class function GetSkinDirectory(const SkinDirectory: string = ''):string;
  public
    { These are the procedures to control the behaviour of the GUI.
      Override them in the subclass to get the desired functionality. }
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
  skindirectoryDevelopment = 'winstskin';
  skindirectoryDefault = 'skin';
  skindirectoryCustomWin = '..'+PathDelim+'custom'+PathDelim+'customskin';
  skindirectoryCustomWinOld = '..'+PathDelim+'custom'+PathDelim+'winstskin';
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
  FilePath: string;
begin
  FilePath := GetSkinDirectory(SkinDirectory) + PathDelim + 'skin.ini';
  if FileExists(FilePath) then
  begin
    ThemeFile := TIniFile.Create(FilePath);
    Result := ThemeFile.ReadString('Form', 'Theme' , 'Default');
    ThemeFile.Free;
  end
  else
  begin
    Result := 'Default';
  end;
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
  else if FileExists(ExtractFilePath(paramstr0enc) + skindirectoryCustomWinOld+PathDelim+'skin.ini') then
    skinDir := ExtractFilePath(paramstr0enc) + skindirectoryCustomWinOld
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

