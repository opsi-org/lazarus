unit osbatchgui;

{$MODE DELPHI}
{$RANGECHECKS ON}
{$OVERFLOWCHECKS ON}
{$IOCHECKS ON}
{$OBJECTCHECKS ON}
{$VARSTRINGCHECKS ON}
{$LONGSTRINGS ON}



(* contains the form that comes to front in batch mode *)

// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: Rupert Roeder, detlef oertel



interface

uses
{$IFDEF WINDOWS}
  Windows,
  CommCtrl,
{$ENDIF}
  osconf, LCLIntf, SysUtils, Classes, Graphics, Controls, IniFiles, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, LResources,
  //Sensors, indGnouMeter,
  osencoding,
  typinfo,
{$IFDEF CPUI386}
  // it is a Intel x86 architecture
  {$define CPUINTEL}
{$ENDIF}
{$IFDEF CPUX86_64}
  // it is a Intel x86 architecture
  {$define CPUINTEL}
{$ENDIF}
  osGUIControl;

type

  //TBatchWindowMode = (bwmNotActivated, bwmIcon, bwmNormalWindow, bwmMaximized);

  { TFBatchOberflaeche }

  //TFBatchOberflaeche = class(TForm, IViewService)
  TFBatchOberflaeche = class(TGUIControl)
    PanelFillScreen: TPanel;
    Panel: TPanel;
    ImageBackground: TImage;
    LabelProgress: TLabel;
    LabelVersion: TLabel;
    ImageProduct: TImage;
    LabelProduct: TLabel;
    LabelCommand: TLabel;
    LabelDetail: TLabel;
    ImageLogo1: TImage;
    ImageLogo2: TImage;
    Image1Over: TImage;
    Image2Over: TImage;
    ImageOpsiBackground: TImage;
    LabelInfo: TLabel;
    ActivityBar: TProgressBar;
    TimerOnTop: TTimer;
    TimerProcessMess: TTimer;
    TimerActivity: TTimer;
    TimerDetail: TTimer;
    TimerCommand: TTimer;


    //procedure WMEraseB kGnd (var t:tmessage); message WM_ERASEBKGND;
    {$IFDEF WINDOWS}
    procedure EnableFontSmoothing(LabelName: TLabel);
    {$ENDIF WINDOWS}
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure ProgressBarActive(YesNo: boolean);
    procedure ShowProgress(Prozente: integer);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);

    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);


    procedure FormResize(Sender: TObject);
    procedure TimerCommandTimer(Sender: TObject);
    procedure TimerActivityTimer(Sender: TObject);
    procedure TimerDetailTimer(Sender: TObject);
    procedure TimerOnTopTimer(Sender: TObject);
    procedure TimerProcessMessTimer(Sender: TObject);

  private
    FOldProgress: integer;

    procedure doInfo(aMessage: string);
    procedure ForceStayOnTop(YesNo: boolean);
    procedure setInfoLabel(s: string);
    procedure setVersionLabel(s: string);
    procedure setWindowState(BatchWindowMode: TBatchWindowMode);
    procedure setDetailLabel(s: string);
    procedure setCommandLabel(s: string);
    procedure centerWindow;
    procedure showActivityBar(Show: boolean);
    procedure showProgressBar(b: boolean);
    procedure setActivityLabel(s: string);
    procedure SetBoundsFromSkinIni(Component: TControl; NameInSkinIni:string;
      SkinIniFile:TIniFile; DefaultLeft:integer; DefaultTop:integer;
      DefaultWidth:integer; DefaultHeight:integer);

    //Bit: TBitmap32;
    //BlendF: TBlendFunction;
    //P: TPoint;
    //Size: TSize;
  public
    { Public-Deklarationen }

    //GUIControl interface
    procedure LoadSkin(const SkinDirectory: string; setLabelInfo: boolean = True);
      override;
    procedure SetMessageText(MessageText: string; MessageID: TMessageID); override;
    procedure SetProgress(NewProgress: integer; ProgressValueID: TProgressValueID);
      override;
    procedure SetForceStayOnTop(StayOnTop: boolean); override;
    procedure SetBatchWindowMode(BatchWindowMode: TBatchWindowMode); override;
    procedure SetElementVisible(Visible: boolean; ElementID: TElementID); override;
    procedure SetElementEnabled(Enabled: boolean; ElementID: TElementID); override;
    procedure BringElementToFront(ElementID: TElementID); override;
    procedure SetElementTop(Top: integer; ElementID: TElementID); override;
    procedure SetElementLeft(Left: integer; ElementID: TElementID); override;
    procedure SetWindowPosition(Position: TPosition); override;
    procedure SetTracingLevel(const Level: integer); override;
    procedure SetPicture(const BitmapFile: string; const theLabel: string); override;

    //ToDO: include in new interface



    //procedure setCPUActivityLabel(s: string);


  end;

var

  //viewService : IViewService;
  //FBatchOberflaeche:  IViewService;   this seems to produce erratic null pointer exceptions when application terminates
  //FBatchOberflaeche: TosGUIControl;//TFBatchOberflaeche;
  LableInfoDefaultFontSize: integer;

  BatchWindowMode, SavedBatchWindowMode, aktBatchWindowMode: TBatchWindowMode;
  FormMoving: boolean;
  MousePos: TPoint;
  z: TBitmap;
  w, h: integer;
  useCommandLabel: boolean;
  useDetailLabel: boolean;
  useActivityLabel: boolean;
  timeCommandLabel: boolean;
  timeDetailLabel: boolean;
  timeActivityLabel: boolean;

  Progressbar: TProgressBar;

  panelWidth: integer;
  panelHeight: integer;



const
  BatchScreenOnTop: boolean = False;
  centralImageNo = 2;
  bitmapFilenameProductDefault = 'winst3.png';
  StartTop: integer = 100;
  StartLeft: integer = 100;
  InnerWidth = 605;
  InnerHeight = 430;
  standardTopMargin = 50;

resourcestring
  rsLoadingSkin = 'Loading skin...';
  rsGetListOfProducts = 'get list of products';

implementation


uses osmessagedialog, osfunc, osmain, oslog;

procedure TFBatchOberflaeche.FormCreate(Sender: TObject);
var
  MyFavoriteFont: string = '';
  SecondFont: string = '';
  //Properties: TStringList;
  labelcontent: string = '';
  i: integer = 0;
  strColor: string = '';
  //textcolor: TColor;
  //Alpha: boolean;

begin
  Progressbar := TProgressBar.Create(nil);
  with Progressbar do
  begin
    Visible := False;
    Parent := Panel;
    Left := 275;
    Top := 160;
    Width := 280;
    Height := 20;
    Orientation := pbHorizontal;
    Smooth := False;
    Step := 10;
    Position := 0;
    Min := 0;
    Max := 100;
    font.Color := clWindowText;
    font.Height := -11;
    font.Name := 'MS Sans Serif';
    font.Style := [];
  end;
  BorderIcons := [];
  useCommandLabel := True;
  useDetailLabel := True;
  useActivityLabel := True;
  timeCommandLabel := True;
  timeDetailLabel := True;
  timeActivityLabel := True;

  SetTracingLevel(3);


  Panel.DoubleBuffered := True;

  Color := clBlue;
  Panel.Color := clBlue;

  LabelProduct.Font.Color := clWhite;
  LabelVersion.Font.Color := clWhite;
  LabelInfo.Font.Color := clWhite;
  LabelDetail.Font.Color := clWhite;
  LabelCommand.Font.Color := clWhite;
  LabelProgress.Font.Color := clWhite;
  //LabelProgress1.Font.Color := clRed;


  LabelVersion.Caption := OpsiscriptVersionName;
  LabelProduct.Caption := selfProductName;
  LabelInfo.Caption := rsGetListOfProducts;
  LabelDetail.Caption := '';
  LabelCommand.Caption := '';
  LabelProgress.Caption := '';
  //LabelProgress1.Caption := '';

  LoadSkin('');
end;

procedure TFBatchOberflaeche.FormShow(Sender: TObject);
begin
  // window state is controlled commandline parameters
  //setWindowState(bwmNormalWindow);
  Position := poScreenCenter;
  if ScaleDesignToForm(Height) < ScaleDesignToForm(InnerHeight) + ScaleDesignToForm(StartTop) then
    StartTop := (ScaleDesignToForm(Height) - ScaleDesignToForm(InnerHeight)) div ScaleDesignToForm(2);
  if ScaleDesignToForm(Width) < ScaleDesignToForm(InnerWidth) + ScaleDesignToForm(StartLeft) then
    StartLeft := (ScaleDesignToForm(Width) - ScaleDesignToForm(InnerWidth)) div ScaleDesignToForm(2);

  Panel.Left := ScaleDesignToForm(StartLeft);
  Panel.Top := ScaleDesignToForm(StartTop);
  Panel.Width := ScaleDesignToForm(InnerWidth);
  Panel.Height := ScaleDesignToForm(InnerHeight);
  Left := ScaleDesignToForm(StartLeft);
  Top := ScaleDesignToForm(StartTop);
  Width := ScaleDesignToForm(InnerWidth);
  Height := ScaleDesignToForm(InnerHeight);
  MoveToDefaultPosition;



  {$IFDEF WINDOWS}
  EnableFontSmoothing(LabelVersion);
  EnableFontSmoothing(LabelProduct);
  EnableFontSmoothing(LabelInfo);
  EnableFontSmoothing(LabelDetail);
  EnableFontSmoothing(LabelCommand);
  EnableFontSmoothing(LabelProgress);

  {$ENDIF WINDOWS}
  {$IFDEF DARWIN}
  ForceStayOnTop(true);
  {$ENDIF DARWIN}
   ProcessMess;
end;

function rgbStringToColor(str: string): TColor;
var
  red, green, blue: byte;
  start, Count: integer;
  remaining, tempstr: string;
  tmpcol: TCOLOR;
begin
  //try
  start := 1;
  Count := pos(',', str) - start;
  red := StrToInt(copy(str, 1, Count));
  start := start + Count + 1;
  remaining := copy(str, start, 500);
  Count := pos(',', remaining) - 1;
  green := StrToInt(copy(remaining, 1, Count));
  start := start + Count + 1;
  remaining := copy(str, start, 500);
  blue := StrToInt(copy(remaining, 1, 3));
  //Result := RGBToColor(red, green, blue);
  tempstr := '$';
  tempstr := tempstr + inttohex(blue, 2) + inttohex(green, 2) + inttohex(red, 2);
  tmpcol := StringToColor(tempstr);
  Result := tmpcol;
  (*
  except
      on E: Exception do
      begin
        LogDatei.log('Error: Could not convert to Color: ' + str, LLError);
        LogDatei.log('Error: Message: ' + E.message, LLError);
      end;
    end;
    *)
end;


function myStringToTColor(str: string): TColor;
begin
  try
    Result := rgbStringToColor(str);
  except
    try
      Result := StringToColor(str);
    except
    end;
  end;
end;


procedure SetLabelFontsFromSkinIni(skinIni: TIniFile; LabelControl: TLabel;
  NameInSkinIni: string; DefaultFontSize: integer);
begin
  LabelControl.Font.Name :=
    skinIni.ReadString(NameInSkinIni, 'FontName', 'Arial');
  LabelControl.Font.Size := skinIni.ReadInteger(NameInSkinIni, 'FontSize', DefaultFontSize);
  LabelControl.Font.Color :=
    myStringToTColor(skinIni.ReadString(NameInSkinIni, 'FontColor', 'clBlack'));
  LabelControl.Font.Style := [];
  if ('true' = skinIni.ReadString(NameInSkinIni, 'FontBold', 'false')) then
    LabelControl.Font.Style := LabelControl.Font.Style + [fsBold];
  if ('true' = skinIni.ReadString(NameInSkinIni, 'FontItalic', 'false')) then
    LabelControl.Font.Style := LabelControl.Font.Style + [fsItalic];
  if ('true' = skinIni.ReadString(NameInSkinIni, 'FontUnderline', 'false')) then
    LabelControl.Font.Style := LabelControl.Font.Style + [fsUnderline];
end;

procedure LoadImageFromSkinIni(skinDir: string; skinIni: TIniFile;
  Image: TImage; NameInSkinIni: string; ImageName: string);
var
  ImageFilePath: string;
begin
  ImageFilePath := skinDir + PathDelim + skinIni.ReadString(
    NameInSkinIni, 'File', ImageName);
  ImageFilePath := ExpandFileName(ImageFilePath);
  if FileExists(ImageFilePath) and not IsDirectory(ImageFilePath) then
    Image.picture.loadFromFile(ImageFilePath);
end;

procedure TFBatchOberflaeche.LoadSkin(const SkinDirectory: string;
  setLabelInfo: boolean = True);
var
  skindir: string = '';
  skinFile: string = '';
  skinIni: TIniFile;

  procedure setAlignment(var theLabel: TLabel; const newAligment: string);
  begin
    if lowerCase(newAligment) = 'left' then
      theLabel.Alignment := taLeftJustify
    else if lowerCase(newAligment) = 'right' then
      theLabel.Alignment := taRightJustify
    else if lowerCase(newAligment) = 'center' then
      theLabel.Alignment := taCenter;
    //otherwise it stays as it was
  end;

begin
  skinDir := GetSkinDirectory(SkinDirectory);
  startupmessages.Append('Loading skin from: ' + skinDir);
  skinFile := skinDir + PathDelim + 'skin.ini';
  if FileExists(skinFile) then
  begin
    if setLabelInfo then
      LabelInfo.Caption := rsLoadingSkin;
    try
      skinIni := TIniFile.Create(skinFile);
      Color := myStringToTColor(skinIni.ReadString('Form', 'Color', 'clBlack'));

      //Adaped this code to expand the skin.ini to also set the Form
      //try
        //Left := ScaleDesignToForm(skinIni.ReadInteger('Form', 'Left', StartLeft));
        //Top :=  ScaleDesignToForm(skinIni.ReadInteger('Form', 'Top', StartTop));
        //Width := ScaleDesignToForm(skinIni.ReadInteger('Form', 'Width', 605));
        //Height := ScaleDesignToForm(skinIni.ReadInteger('Form', 'Height', 430));
        //Position := poScreenCenter;
        //MoveToDefaultPosition;
      //except
      //end;

      try
        Panel.Color := myStringToTColor(skinIni.ReadString('Form', 'Color', 'clBlack'));
      except
      end;

      try
        setAlignment(LabelVersion, skinIni.ReadString('LabelVersion', 'Alignment', ''));
        SetBoundsFromSkinIni(LabelVersion, 'LabelVersion', SkinIni, 76, 16, 85, 16);
        SetLabelFontsFromSkinIni(SkinIni, LabelVersion, 'LabelVersion', 8);
      except
      end;

      try
        setAlignment(LabelProduct, skinIni.ReadString('LabelProduct','Alignment',''));
        SetBoundsFromSkinIni(LabelProduct, 'LabelProduct', SkinIni, 144, 207, 313, 41);
        SetLabelFontsFromSkinIni(SkinIni, LabelProduct, 'LabelProduct', 32);
        LabelProduct.OptimalFill := True;
      except
      end;

      try
        setAlignment(LabelCommand, skinIni.ReadString('LabelCommand', 'Alignment', ''));
        SetBoundsFromSkinIni(LabelCommand, 'LabelCommand', SkinIni, 29, 316, 524, 15);
        SetLabelFontsFromSkinIni(SkinIni, LabelCommand, 'LabelCommand', 8);
      except
      end;

      try
        setAlignment(LabelInfo, skinIni.ReadString('LabelInfo', 'Alignment', ''));
        SetBoundsFromSkinIni(LabelInfo, 'LabelInfo', SkinIni, 29, 272, 524, 20);
        SetLabelFontsFromSkinIni(SkinIni, LabelInfo, 'LabelInfo', 11);
      except
      end;

      try
        setAlignment(LabelDetail, skinIni.ReadString('LabelDetail', 'Alignment', ''));
        SetBoundsFromSkinIni(LabelDetail, 'LabelDetail', SkinIni, 29, 292, 524, 20);
        SetLabelFontsFromSkinIni(SkinIni, LabelDetail, 'LabelDetail', 8);
      except
      end;

      try
        setAlignment(LabelProgress, skinIni.ReadString('LabelProgress', 'Alignment', ''));
        SetBoundsFromSkinIni(LabelProgress, 'LabelProgress', SkinIni, 29, 331, 524, 15);
        SetLabelFontsFromSkinIni(SkinIni, LabelProgress, 'LabelProgress', 8); // 2
      except
      end;

      try
        (* temporary disabled do 16.12.20
        ImageBackground.Left := skinIni.ReadInteger('ImageBackground', 'Left', 0);
        Panel.Left := skinIni.ReadInteger('ImageBackground', 'Left', 0);
        ImageBackground.Top := skinIni.ReadInteger('ImageBackground', 'Top', 0);
        Panel.Top := skinIni.ReadInteger('ImageBackground', 'Top', 0);
        ImageBackground.Width := skinIni.ReadInteger('ImageBackground', 'Width', 605);
        Panel.Width := skinIni.ReadInteger('ImageBackground', 'Width', 605);
        ImageBackground.Height := skinIni.ReadInteger('ImageBackground', 'Height', 430);
        Panel.Height := skinIni.ReadInteger('ImageBackground', 'Height', 430);
         *)
        LoadImageFromSkinIni(skinDir, skinIni, ImageBackground, 'ImageBackground', 'bg.png');
      except
      end;

      try
        SetBoundsFromSkinIni(ImageProduct, 'ImageProduct', SkinIni, 224, 44, 144, 144);
        LoadImageFromSkinIni(skinDir, skinIni, ImageProduct, 'ImageProduct', 'product.png');
      except
      end;

      try
        SetBoundsFromSkinIni(ImageLogo1, 'ImageLogo1', SkinIni, 60, 80, 160, 160);
        LoadImageFromSkinIni(skinDir, skinIni, ImageLogo1, 'ImageLogo1', 'logo1.png');
      except
      end;

      try
        SetBoundsFromSkinIni(ImageLogo2, 'ImageLogo2', SkinIni, 450, 80, 531, 91);
        LoadImageFromSkinIni(skinDir, skinIni, ImageLogo2, 'ImageLogo2', 'logo2.png');
      except
      end;

      try
        SetBoundsFromSkinIni(Image1Over, 'Image1Over', SkinIni, 216, 36, 160, 160);
        LoadImageFromSkinIni(skinDir, skinIni, Image1Over, 'Image1Over', 'over1.png');
      except
      end;

      try
        SetBoundsFromSkinIni(Image2Over, 'Image2Over', SkinIni, 26, 267, 531, 91);
        LoadImageFromSkinIni(skinDir, skinIni, Image2Over, 'Image2Over', 'over2.png');
      except
      end;

      try
        SetBoundsFromSkinIni(Progressbar, 'ProgressBar', SkinIni, 96, 235, 401, 17);
      except
      end;

      try
        SetBoundsFromSkinIni(ActivityBar, 'ActivityBar', SkinIni, 60, 350, 320, 10);
        {$IFDEF WINDOWS}
        SendMessage(ActivityBar.Handle, PBM_SETBARCOLOR, 0,
          myStringToTColor(skinIni.ReadString('ActivityBar', 'BarColor', 'clBlue')));
        {$ENDIF WINDOWS}
        ActivityBar.Enabled := True;
        ActivityBar.Position := 50;
      except
      end;


    except
      LabelInfo.Caption := 'Failed to load skin!';
    end;
    skinIni.Free;

    PanelFillScreen.Color := Panel.Color;
    Repaint;
  end;
end;

procedure TFBatchOberflaeche.SetTracingLevel(const Level: integer);
begin
  case level of
    0:
    begin
      LabelCommand.Caption := '';
      LabelDetail.Caption := '';
      LabelProgress.Caption := '';
      useDetailLabel := False;
      useCommandLabel := False;
      useActivityLabel := False;
    end;
    1:
    begin
      LabelCommand.Caption := '';
      LabelDetail.Caption := '';
      LabelProgress.Caption := '';
      useCommandLabel := True;
      useActivityLabel := False;
      useDetailLabel := False;
    end;
    2:
    begin
      LabelCommand.Caption := '';
      LabelDetail.Caption := '';
      LabelProgress.Caption := '';
      useCommandLabel := True;
      useActivityLabel := False;
      useDetailLabel := True;
    end;
    3:
    begin
      LabelCommand.Caption := '';
      LabelDetail.Caption := '';
      LabelProgress.Caption := '';
      useCommandLabel := True;
      useActivityLabel := True;
      useDetailLabel := True;
    end
    else
    begin
      LabelCommand.Caption := '';
      LabelDetail.Caption := '';
      LabelProgress.Caption := '';
      useCommandLabel := True;
      useActivityLabel := True;
      useDetailLabel := True;
    end;
  end;
end;

{$IFDEF WINDOWS}
procedure TFBatchOberflaeche.EnableFontSmoothing(LabelName: TLabel);
var
  tagLOGFONT: TLogFont;
begin
  GetObject(LabelName.Font.Handle, SizeOf(TLogFont), @tagLOGFONT);
  tagLOGFONT.lfQuality := ANTIALIASED_QUALITY;
  LabelName.Font.Handle := CreateFontIndirect(tagLOGFONT);
end;

{$ENDIF WINDOWS}

procedure TFBatchOberflaeche.FormActivate(Sender: TObject);
begin
  ForceStayOnTop(BatchScreenOnTop);
  FOldProgress := 0;
end;



procedure TFBatchOberflaeche.ForceStayOnTop(YesNo: boolean);
begin
    if Assigned(LogDatei) then
  LogDatei.log_prog('ForceStayOnTop start: '+BoolToStr(YesNo,true), LLnotice);
  if YesNo then
  begin
    { make to system wide top most window }
    FormStyle := fsSystemStayOnTop;
    BringToFront;
    { now allow new started windows (setup) to get the system wide top most position }
    FormStyle := fsStayOnTop;
    BatchScreenOnTop := True;
    // call onTopTimer event
    TimerOnTopTimer(TimerOnTop);
    // enable onTopTimer
    TimerOnTop.Enabled:= true;
  end
  else
  begin
    FormStyle := fsnormal;
    BatchScreenOnTop := False;
    // disable onTopTimer
    TimerOnTop.Enabled:= false;
  end;
  setWindowState(aktBatchWindowMode);
  Application.ProcessMessages;
end;


procedure TFBatchOberflaeche.setWindowState(BatchWindowMode: TBatchWindowMode);
begin
  {$IFNDEF DARWIN}
  case BatchWindowMode of
    bwmNotActivated: WindowState := wsnormal;
    (*
    bwmIcon: if WindowState <> wsMinimized then WindowState := wsminimized;
    bwmNormalWindow: if WindowState <> wsnormal then WindowState := wsnormal;
    bwmMaximized: if WindowState <> wsMaximized then WindowState := wsMaximized;
    *)
    bwmIcon: WindowState := wsminimized;
    bwmNormalWindow: WindowState := wsnormal;
    bwmMaximized: WindowState := wsMaximized;
  end;
  Application.ProcessMessages;
  if Assigned(LogDatei) then
    LogDatei.log_prog('Switch window state to: ' + GetEnumName(
      TypeInfo(TBatchWindowMode), Ord(BatchWindowMode)), LLDebug);
  {$ENDIF}
end;


procedure TFBatchOberflaeche.ProgressBarActive(YesNo: boolean);
begin
  if YesNo then
  begin
    Progressbar.Visible := True;
    Progressbar.Enabled := True;
    Progressbar.Position := 0;
  end
  else
  begin
    Progressbar.Visible := False;
    Progressbar.Enabled := False;
  end;
end;

procedure TFBatchOberflaeche.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  //prevents closing batchmode via ALT-F4
  CloseAction := caNone;
end;


procedure TFBatchOberflaeche.FormWindowStateChange(Sender: TObject);
begin
  if Assigned(LogDatei) then
    LogDatei.log('Window state was switched by : ' + Sender.ClassName +
      ' to: ' + GetEnumName(TypeInfo(TWindowState), Ord(FBatchOberflaeche.WindowState)),
      LLDebug);
end;

procedure TFBatchOberflaeche.ShowProgress(Prozente: integer);
begin
  Progressbar.Position := Prozente;
end;

procedure TFBatchOberflaeche.showActivityBar(Show: boolean);
begin
  ActivityBar.Visible := Show;
end;


procedure TFBatchOberflaeche.SetPicture(const BitmapFile: string;
  const theLabel: string);
var
  BitmapFilename: string = '';
  errorinfo: string = '';
  shorty: string = '';
begin
  if BitmapFile = '' then
    bitmapFilename := bitmapFilenameProductDefault
  else
    bitmapfilename := bitmapfile;
  try
    BitmapFilename := ExpandFileName(BitmapFilename);
    if fileexists(BitmapFilename) then
      ImageProduct.picture.loadFromFile(BitmapFilename)
    else
      ImageProduct.picture := nil;
    ImageProduct.Repaint;
    ProcessMess;
    LabelProduct.Caption := theLabel;
    LabelProduct.OptimalFill := True;
    // do not auto size short labels because this will look ugly
    // use optimalfill to decrease the font only on large labels
    If LabelProduct.Font.Size < -32 then
    begin
      LabelProduct.OptimalFill := False;
      LabelProduct.Font.Size := -32;
    end;
  except
    on e: Exception do
    begin
      ErrorInfo := 'Error: ' + e.message;
    end
  end;
  processMess;
end;

procedure TFBatchOberflaeche.SetMessageText(MessageText: string; MessageID: TMessageID);
begin
  case MessageID of
    mInfo: DoInfo(MessageText);
    mVersion: SetVersionLabel(MessageText);
    mDetail: SetDetailLabel(MessageText);
    mCommand: SetCommandLabel(MessageText);
    mActivity: SetActivityLabel(MessageText);
  end;
end;

procedure TFBatchOberflaeche.SetProgress(NewProgress: integer;
  ProgressValueID: TProgressValueID);
begin
  case ProgressValueID of
    pPercent:
    begin
      // Only call FBatchOberflaeche.ShowProgress when a next round percent is reached (NewProgress > FOldProgress).
      // This is important to ensures that FBatchOberflaeche.SetProgress isn't called too often
      // because calling too often can slow down the whole process by multiple seconds (e.g. if you handle many files)
      if FOldProgress <> NewProgress then
      begin
        ShowProgress(NewProgress);
        FOldProgress := NewProgress;
        Application.ProcessMessages;
      end;
    end;
  end;
end;

procedure TFBatchOberflaeche.SetForceStayOnTop(StayOnTop: boolean);
begin
  ForceStayOnTop(StayOnTop);
end;

procedure TFBatchOberflaeche.SetBatchWindowMode(BatchWindowMode: TBatchWindowMode);
var
  BWModeStr: string;
begin
  if Assigned(LogDatei) then
  begin
    WriteStr(BWModeStr, BatchWindowMode);
    LogDatei.log_prog('SetBatchWindowMode start: ' + BWModeStr, LLinfo);
  end;
  setWindowState(BatchWindowMode);
  aktBatchWindowMode := BatchWindowMode;
end;

procedure TFBatchOberflaeche.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := False;
  // SetBatchWindowMode also store the mode in aktBatchWindowMode;
  SetBatchWindowMode(bwmNormalWindow);
  Position := poScreenCenter;
  SetBounds(StartLeft, StartTop, InnerWidth, InnerHeight);
end;

procedure TFBatchOberflaeche.FormMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  BringToFront;
  if Button = mbLeft then
  begin
    MousePos.X := X;
    MousePos.Y := Y;
    FormMoving := True;
  end;
  if Button = mbRight then
  begin
    if WindowState = wsMaximized then
    begin
      WindowState := wsnormal;
    end
    else
    begin
      WindowState := wsMaximized;
    end;
  end;
end;

procedure TFBatchOberflaeche.FormMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  if FormMoving then
  begin
    Left := Mouse.CursorPos.X - MousePos.X;
    Top := Mouse.CursorPos.Y - MousePos.Y;
  end;
end;

procedure TFBatchOberflaeche.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  FormMoving := False;
end;

procedure TFBatchOberflaeche.FormResize(Sender: TObject);
var
  i: integer;
begin
  i := (ScaleDesignToForm(Width) - ScaleDesignToForm(Panel.Width)) div ScaleDesignToForm(2);
  Panel.Left := i;
  if ScaleDesignToForm(Height) > ScaleDesignToForm(Panel.Height) + ScaleDesignToForm(standardTopMargin) then
    Panel.Top := ScaleDesignToForm(standardTopMargin)
  else
    Panel.Top := 0;

  ImageOpsiBackground.Left := ScaleDesignToForm(Width) - ScaleDesignToForm(ImageOpsiBackground.Width);
  ImageOpsiBackground.Top := ScaleDesignToForm(Height) - ScaleDesignToForm(ImageOpsiBackground.Height);
  Panel.Repaint;
end;

//interface
procedure TFBatchOberflaeche.SetElementVisible(Visible: boolean; ElementID: TElementID);
begin
  case ElementID of
    eMainForm: self.Visible := Visible;
    eActivityBar: showActivityBar(Visible);
    eProgressBar: showProgressBar(Visible);
  end;
end;

procedure TFBatchOberflaeche.SetElementEnabled(Enabled: boolean; ElementID: TElementID);
begin
  case ElementID of
    eTimerProcessMess: TimerProcessMess.Enabled := Enabled;
  end;
end;

procedure TFBatchOberflaeche.BringElementToFront(ElementID: TElementID);
begin
  case ElementID of
    eMainForm: self.BringToFront;
  end;
end;

procedure TFBatchOberflaeche.SetElementTop(Top: integer; ElementID: TElementID);
begin
  case ElementID of
    eMainForm: self.Top := Top;
  end;
end;

procedure TFBatchOberflaeche.SetElementLeft(Left: integer; ElementID: TElementID);
begin
  case ElementID of
    eMainForm: self.Left := Left;
  end;
end;

procedure TFBatchOberflaeche.SetWindowPosition(Position: TPosition);
begin
  case Position of
    poScreenCenter: centerWindow;
  end;
end;

procedure TFBatchOberflaeche.showProgressBar(b: boolean);
begin
  progressBarActive(b);
end;


procedure TFBatchOberflaeche.doInfo(aMessage: string);
begin
  setInfoLabel(aMessage);
  LabelInfo.OptimalFill := True;
  If LabelInfo.Font.Size < LableInfoDefaultFontSize then
  begin
    LabelInfo.OptimalFill := False;
    LabelInfo.Font.Size := LableInfoDefaultFontSize;
  end;
end;

procedure TFBatchOberflaeche.setCommandLabel(s: string);
begin
  if useCommandLabel and timeCommandLabel then
  begin
    labelCommand.Caption := s;
    timeCommandLabel := False;
    TimerCommand.Enabled := True;
  end;
end;

procedure TFBatchOberflaeche.setInfoLabel(s: string);
begin
  labelInfo.Caption := s;
end;

procedure TFBatchOberflaeche.setDetailLabel(s: string);
begin
  if useDetailLabel and timeDetailLabel then
  begin
    labelDetail.Caption := s;
    ProcessMess;
    timeDetailLabel := False;
    TimerDetail.Enabled := True;
  end;
end;

procedure TFBatchOberflaeche.setActivityLabel(s: string);
begin
  if useActivityLabel and timeActivityLabel then
  begin
    LabelProgress.Caption := s;
    ProcessMess;
    timeActivityLabel := False;
    TimerActivity.Enabled := True;
    ProcessMess;
  end;
end;

procedure TFBatchOberflaeche.SetBoundsFromSkinIni(Component: TControl;
  NameInSkinIni: string; SkinIniFile:TIniFile; DefaultLeft: integer;
  DefaultTop: integer; DefaultWidth: integer; DefaultHeight: integer);
begin
  //This function reads the position (Left,Top) and size (Width, Height) of a component
  //from the SkinIni file and sets these values for the given component.
  //It uses ScaleDesignToForm to be compatible with (windows) scaling.
  //Note: Scaling is usually only relevant for HighDPI screens.
  Component.Left := ScaleDesignToForm(SkinIniFile.ReadInteger(NameInSkinIni, 'Left', DefaultLeft));
  Component.Top := ScaleDesignToForm(SkinIniFile.ReadInteger(NameInSkinIni, 'Top', DefaultTop));
  Component.Width := ScaleDesignToForm(SkinIniFile.ReadInteger(NameInSkinIni, 'Width', DefaultWidth));
  Component.Height := ScaleDesignToForm(SkinIniFile.ReadInteger(NameInSkinIni, 'Height', DefaultHeight));
end;


procedure TFBatchOberflaeche.setVersionLabel(s: string);
begin
  LabelVersion.Caption := s;
end;


procedure TFBatchOberflaeche.TimerCommandTimer(Sender: TObject);
begin
  timeCommandLabel := True;
  TimerCommand.Enabled := False;
end;

procedure TFBatchOberflaeche.TimerActivityTimer(Sender: TObject);
begin
  timeActivityLabel := True;
  TimerActivity.Enabled := False;
  //setActivityLabel(copy(peakindicator, 1, peaklen));
end;

procedure TFBatchOberflaeche.TimerDetailTimer(Sender: TObject);
begin
  timeDetailLabel := True;
  TimerDetail.Enabled := False;
end;

procedure TFBatchOberflaeche.TimerOnTopTimer(Sender: TObject);
begin
  if TTimer(Sender).Enabled then
  begin
    if Assigned(LogDatei) then
      LogDatei.log_prog('TimerOnTopTimer start ', LLnotice);
    BringToFront;
    Application.ProcessMessages;
  end;
end;

procedure TFBatchOberflaeche.TimerProcessMessTimer(Sender: TObject);
begin
  TimerProcessMess.Enabled := True;
  ProcessMess;
end;

procedure TFBatchOberflaeche.centerWindow;
begin
  Position := poScreenCenter;
  MoveToDefaultPosition;
end;


initialization


 {$i osbatchgui.lrs}


end.
