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
{$IFDEF CPUINTEL}
  // QProgBar contains Intel x86 Assembler code and so it cannot be used with other cpu architecture
  QProgBar,
{$ENDIF CPUINTEL}
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
    procedure FormWindowStateChange(Sender: TObject);
    procedure ProgressBarActive(YesNo: boolean);
    procedure ShowProgress(Prozente: integer);
    procedure FormShow(Sender: TObject);
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

  BatchWindowMode, SavedBatchWindowMode: TBatchWindowMode;
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

  {$IFDEF CPUINTEL}
  Progressbar: TQProgressBar;
  {$ENDIF CPUINTEL}
  //ActivityBar: TQProgressBar;

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

procedure TFBatchOberflaeche.FormShow(Sender: TObject);
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
  {$IFDEF CPUINTEL}
  Progressbar := TQProgressBar.Create(nil);
  Progressbar.Position := 0;
  ProgressBar.Visible := False;
  with Progressbar do
  begin
    Parent := Panel;
    Left := 275;
    Top := 160;
    Width := 280;
    Height := 20;
    orientation := boHorizontal;
    barKind := bkCylinder;
    barLook := blGlass;
    roundCorner := True;
    backgroundColor := clWhite;
    barColor := 15198183;
    startColor := 15198183;
    finalColor := 15198183;
    showInactivePos := False;
    invertInactPos := False;
    inactivePosColor := clGray;
    shaped := True;
    shapeColor := 15198183;
    blockSize := 10;
    spaceSize := 3;
    showFullBlock := False;
    maximum := 100;
    position := 50;
    captionAlign := taLeftJustify;
    //        font.Charset := DEFAULT_CHARSET;
    font.Color := clWindowText;
    font.Height := -11;
    font.Name := 'MS Sans Serif';
    font.Style := [];
    AutoCaption := False;
    AutoHint := False;
    ShowPosAsPct := False;
  end;
  {$ENDIF CPUINTEL}

  BorderIcons := [];
  useCommandLabel := True;
  useDetailLabel := True;
  useActivityLabel := True;
  timeCommandLabel := True;
  timeDetailLabel := True;
  timeActivityLabel := True;

  SetTracingLevel(3);


  Panel.DoubleBuffered := True;
  { will be set in osmain accourding to cli parameters -> runSilent }
  //Visible := True;

  setWindowState(bwmNormalWindow);
  Position := poScreenCenter;
  if ScaleDesignToForm(Height) < ScaleDesignToForm(InnerHeight) + ScaleDesignToForm(StartTop) then
    StartTop := (ScaleDesignToForm(Height) - ScaleDesignToForm(InnerHeight)) div ScaleDesignToForm(2);
  if ScaleDesignToForm(Width) < ScaleDesignToForm(InnerWidth) + ScaleDesignToForm(StartLeft) then
    StartLeft := (ScaleDesignToForm(Width) - ScaleDesignToForm(InnerWidth)) div ScaleDesignToForm(2);

  Panel.SetBounds(ScaleDesignToForm(StartLeft), ScaleDesignToForm(StartTop),
    ScaleDesignToForm(InnerWidth), ScaleDesignToForm(InnerHeight));
  SetBounds(ScaleDesignToForm(StartLeft), ScaleDesignToForm(StartTop),
    ScaleDesignToForm(InnerWidth), ScaleDesignToForm(InnerHeight));
  MoveToDefaultPosition;

  Color := clBlue;
  Panel.Color := clBlue;

  LabelProduct.Font.Color := clWhite;
  LabelVersion.Font.Color := clWhite;
  LabelInfo.Font.Color := clWhite;
  LabelDetail.Font.Color := clWhite;
  LabelCommand.Font.Color := clWhite;
  LabelProgress.Font.Color := clWhite;
  //LabelProgress1.Font.Color := clRed;


  LabelVersion.Caption := '';
  LabelProduct.Caption := selfProductName;
  LabelInfo.Caption := rsGetListOfProducts;
  LabelDetail.Caption := '';
  LabelCommand.Caption := '';
  LabelProgress.Caption := '';
  //LabelProgress1.Caption := '';

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
   LoadSkin('');
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
var
  message: string;
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


procedure TFBatchOberflaeche.LoadSkin(const SkinDirectory: string;
  setLabelInfo: boolean = True);
var
  skindir: string = '';
  skinFile: string = '';
  skinIni: TIniFile;
  filename: string = '';
  paramstr0enc: string;

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
        //SetBounds(Left, Top, Width, Height);
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
        LabelVersion.Font.Name :=
          skinIni.ReadString('LabelVersion', 'FontName', 'Arial');
        LabelVersion.Font.Size := skinIni.ReadInteger('LabelVersion', 'FontSize', 8);
        LabelVersion.Font.Color :=
          myStringToTColor(skinIni.ReadString('LabelVersion', 'FontColor', 'clBlack'));
        LabelVersion.Font.Style := [];
        if ('true' = skinIni.ReadString('LabelVersion', 'FontBold', 'false')) then
          LabelVersion.Font.Style := LabelVersion.Font.Style + [fsBold];
        if ('true' = skinIni.ReadString('LabelVersion', 'FontItalic', 'false'))
        then
          LabelVersion.Font.Style := LabelVersion.Font.Style + [fsItalic];
        if ('true' = skinIni.ReadString('LabelVersion', 'FontUnderline', 'false'))
        then
          LabelVersion.Font.Style := LabelVersion.Font.Style + [fsUnderline];

      except
      end;

      try
        setAlignment(LabelProduct, skinIni.ReadString('LabelProduct','Alignment',''));
        SetBoundsFromSkinIni(LabelProduct, 'LabelProduct', SkinIni, 144, 207, 313, 41);
        LabelProduct.Font.Name :=
          skinIni.ReadString('LabelProduct', 'FontName', 'Arial');
        LabelProduct.Font.Size := skinIni.ReadInteger('LabelProduct', 'FontSize', 32);
        LabelProduct.Font.Color :=
          myStringToTColor(skinIni.ReadString('LabelProduct', 'FontColor', 'clBlack'));
        LabelProduct.Font.Style := [];
        if ('true' = skinIni.ReadString('LabelProduct', 'FontBold', 'false')) then
          LabelProduct.Font.Style := LabelProduct.Font.Style + [fsBold];
        if ('true' = skinIni.ReadString('LabelProduct', 'FontItalic', 'false'))
        then
          LabelProduct.Font.Style := LabelProduct.Font.Style + [fsItalic];
        if ('true' = skinIni.ReadString('LabelProduct', 'FontUnderline', 'false'))
        then
          LabelProduct.Font.Style := LabelProduct.Font.Style + [fsUnderline];
        LabelProduct.OptimalFill := True;
      except
      end;

      try
        setAlignment(LabelCommand, skinIni.ReadString('LabelCommand', 'Alignment', ''));
        SetBoundsFromSkinIni(LabelCommand, 'LabelCommand', SkinIni, 29, 316, 524, 15);
        LabelCommand.Font.Name :=
          skinIni.ReadString('LabelCommand', 'FontName', 'Arial');
        LabelCommand.Font.Size := skinIni.ReadInteger('LabelCommand', 'FontSize', 8);
        LabelCommand.Font.Color :=
          myStringToTColor(skinIni.ReadString('LabelCommand', 'FontColor', 'clBlack'));
        LabelCommand.Font.Style := [];
        if ('true' = skinIni.ReadString('LabelCommand', 'FontBold', 'false')) then
          LabelCommand.Font.Style := LabelCommand.Font.Style + [fsBold];
        if ('true' = skinIni.ReadString('LabelCommand', 'FontItalic', 'false'))
        then
          LabelCommand.Font.Style := LabelCommand.Font.Style + [fsItalic];
        if ('true' = skinIni.ReadString('LabelCommand', 'FontUnderline', 'false'))
        then
          LabelCommand.Font.Style := LabelCommand.Font.Style + [fsUnderline];
      except
      end;

      try
        setAlignment(LabelInfo, skinIni.ReadString('LabelInfo', 'Alignment', ''));
        SetBoundsFromSkinIni(LabelInfo, 'LabelInfo', SkinIni, 29, 272, 524, 20);
        LabelInfo.Font.Name := skinIni.ReadString('LabelInfo', 'FontName', 'Arial');
        LableInfoDefaultFontSize := skinIni.ReadInteger('LabelInfo', 'FontSize', 11);
        LabelInfo.Font.Size := LableInfoDefaultFontSize;
        LabelInfo.Font.Color :=
          myStringToTColor(skinIni.ReadString('LabelInfo', 'FontColor', 'clBlack'));
        LabelInfo.Font.Style := [];
        if ('true' = skinIni.ReadString('LabelInfo', 'FontBold', 'false')) then
          LabelInfo.Font.Style := LabelInfo.Font.Style + [fsBold];
        if ('true' = skinIni.ReadString('LabelInfo', 'FontItalic', 'false')) then
          LabelInfo.Font.Style := LabelInfo.Font.Style + [fsItalic];
        if ('true' = skinIni.ReadString('LabelInfo', 'FontUnderline', 'false'))
        then
          LabelInfo.Font.Style := LabelInfo.Font.Style + [fsUnderline];
      except
      end;

      try
        setAlignment(LabelDetail, skinIni.ReadString('LabelDetail', 'Alignment', ''));
        SetBoundsFromSkinIni(LabelDetail, 'LabelDetail', SkinIni, 29, 292, 524, 20);
        LabelDetail.Font.Name :=
          skinIni.ReadString('LabelDetail', 'FontName', 'Arial');
        LabelDetail.Font.Size := skinIni.ReadInteger('LabelDetail', 'FontSize', 8);
        LabelDetail.Font.Color :=
          myStringToTColor(skinIni.ReadString('LabelDetail', 'FontColor', 'clBlack'));
        LabelDetail.Font.Style := [];
        if ('true' = skinIni.ReadString('LabelDetail', 'FontBold', 'false')) then
          LabelDetail.Font.Style := LabelDetail.Font.Style + [fsBold];
        if ('true' = skinIni.ReadString('LabelDetail', 'FontItalic', 'false'))
        then
          LabelDetail.Font.Style := LabelDetail.Font.Style + [fsItalic];
        if ('true' = skinIni.ReadString('LabelInfo', 'FontUnderline', 'false'))
        then
          LabelDetail.Font.Style := LabelDetail.Font.Style + [fsUnderline];
      except
      end;

      try
        setAlignment(LabelProgress, skinIni.ReadString('LabelProgress', 'Alignment', ''));
        SetBoundsFromSkinIni(LabelProgress, 'LabelProgress', SkinIni, 29, 331, 524, 15);
        LabelProgress.Font.Name :=
          skinIni.ReadString('LabelProgress', 'FontName', 'Arial');
        LabelProgress.Font.Size :=
          (*2; *) skinIni.ReadInteger('LabelProgress', 'FontSize', 8);
        LabelProgress.Font.Color :=
          myStringToTColor(skinIni.ReadString('LabelProgress', 'FontColor', 'clBlack'));
        LabelProgress.Font.Style := [];
        if ('true' = skinIni.ReadString('LabelProgress', 'FontBold', 'false'))
        then
          LabelProgress.Font.Style := LabelProgress.Font.Style + [fsBold];
        if ('true' = skinIni.ReadString('LabelProgress', 'FontItalic', 'false'))
        then
          LabelProgress.Font.Style := LabelProgress.Font.Style + [fsItalic];
        if ('true' = skinIni.ReadString('LabelProgress', 'FontUnderline', 'false'))
        then
          LabelProgress.Font.Style := LabelProgress.Font.Style + [fsUnderline];
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
        filename := skinDir + PathDelim + skinIni.ReadString('ImageBackground',
          'File', 'bg.png');
        if FileExists(filename) and not IsDirectory(filename) then
          ImageBackground.picture.loadFromFile(filename);
      except
      end;


      try
        SetBoundsFromSkinIni(ImageProduct, 'ImageProduct', SkinIni, 224, 44, 144, 144);
        filename := skinDir + PathDelim + skinIni.ReadString(
          'ImageProduct', 'File', 'product.png');
        FileName := ExpandFileName(FileName);
        if FileExists(filename) and not IsDirectory(filename) then
          ImageProduct.picture.loadFromFile(filename);
      except
      end;

      try
        SetBoundsFromSkinIni(ImageLogo1, 'ImageLogo1', SkinIni, 60, 80, 160, 160);
        filename := skinDir + PathDelim + skinIni.ReadString(
          'ImageLogo1', 'File', 'logo1.png');
        FileName := ExpandFileName(FileName);
        if FileExists(filename) and not IsDirectory(filename) then
          ImageLogo1.picture.loadFromFile(filename);
      except
      end;

      try
        SetBoundsFromSkinIni(ImageLogo2, 'ImageLogo2', SkinIni, 450, 80, 531, 91);
        filename := skinDir + PathDelim + skinIni.ReadString(
          'ImageLogo2', 'File', 'logo2.png');
        FileName := ExpandFileName(FileName);
        if FileExists(filename) and not IsDirectory(filename) then
          ImageLogo2.picture.loadFromFile(filename);
      except
      end;

      try
        SetBoundsFromSkinIni(Image1Over, 'Image1Over', SkinIni, 216, 36, 160, 160);
        filename := skinDir + PathDelim + skinIni.ReadString(
          'Image1Over', 'File', 'over1.png');
        FileName := ExpandFileName(FileName);
        if FileExists(filename) and not IsDirectory(filename) then
          Image1Over.picture.loadFromFile(filename);
      except
      end;

      try
        SetBoundsFromSkinIni(Image2Over, 'Image2Over', SkinIni, 26, 267, 531, 91);
        filename := skinDir + PathDelim + skinIni.ReadString(
          'Image2Over', 'File', 'over2.png');
        FileName := ExpandFileName(FileName);
        if FileExists(filename) and not IsDirectory(filename) then
          Image2Over.picture.loadFromFile(filename);
      except
      end;

      {$IFDEF CPUINTEL}
      try
        SetBoundsFromSkinIni(Progressbar, 'ProgressBar', SkinIni, 96, 235, 401, 17);
        ProgressBar.BarColor :=
          myStringToTColor(skinIni.ReadString('ProgressBar', 'BarColor', 'clBlack'));
        ProgressBar.StartColor :=
          myStringToTColor(skinIni.ReadString('ProgressBar', 'StartColor', 'clBlack'));
        ProgressBar.FinalColor :=
          myStringToTColor(skinIni.ReadString('ProgressBar', 'FinalColor', 'clBlack'));
        ProgressBar.ShapeColor :=
          myStringToTColor(skinIni.ReadString('ProgressBar', 'ShapeColor', 'clBlack'));
        ProgressBar.backgroundColor :=
          myStringToTColor(skinIni.ReadString('ProgressBar',
          'BackgroundColor', 'clWhite'));
        if ('true' = skinIni.ReadString('ProgressBar', 'Shaped', 'false')) then
          ProgressBar.Shaped := True
        else
          ProgressBar.Shaped := False;
        ProgressBar.BlockSize := ScaleDesignToForm(skinIni.ReadInteger('ProgressBar', 'BlockSize', 6));
        ProgressBar.SpaceSize := ScaleDesignToForm(skinIni.ReadInteger('ProgressBar', 'SpaceSize', 1));
        if ('true' = skinIni.ReadString('ProgressBar', 'Cylinder', 'false')) then
          ProgressBar.BarKind := bkCylinder
        else
          ProgressBar.BarKind := bkFlat;
        if ('true' = skinIni.ReadString('ProgressBar', 'Glass', 'false')) then
          ProgressBar.BarLook := blGlass
        else
          ProgressBar.BarLook := blMetal;
        if ('true' = skinIni.ReadString('ProgressBar', 'ShowFullBlock', 'false')) then
          ProgressBar.ShowFullBlock := True
        else
          ProgressBar.ShowFullBlock := False;
        if ('true' = skinIni.ReadString('ProgressBar', 'RoundCorner', 'false')) then
          ProgressBar.RoundCorner := True
        else
          ProgressBar.RoundCorner := False;
      except
      end;
      {$ENDIF CPUINTEL}

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
  if YesNo then
  begin
    setWindowState(bwmMaximized);
    { make to system wide top most window }
    FormStyle := fsSystemStayOnTop;
    BringToFront;
    Application.ProcessMessages;
    { now allow new started windows (setup) to get the system wide top most position }
    FormStyle := fsStayOnTop;
    BatchScreenOnTop := True;
  end
  else
  begin
    FormStyle := fsnormal;
    BatchScreenOnTop := False;
    Application.ProcessMessages;
  end;
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
  {$IFDEF CPUINTEL}
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
  {$ENDIF CPUINTEL}
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
  {$IFDEF CPUINTEL}
  Progressbar.Position := Prozente;
  {$ENDIF CPUINTEL}
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
begin
  setWindowState(BatchWindowMode);
end;

procedure TFBatchOberflaeche.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := False;
  setWindowState(bwmNormalWindow);
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
  Component.SetBounds(
    ScaleDesignToForm(SkinIniFile.ReadInteger(NameInSkinIni, 'Left', DefaultLeft)),
    ScaleDesignToForm(SkinIniFile.ReadInteger(NameInSkinIni, 'Top', DefaultTop)),
    ScaleDesignToForm(SkinIniFile.ReadInteger(NameInSkinIni, 'Width', DefaultWidth)),
    ScaleDesignToForm(SkinIniFile.ReadInteger(NameInSkinIni, 'Height', DefaultHeight))
    );
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
