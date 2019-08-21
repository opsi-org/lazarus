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
// credits: http://www.opsi.org/credits/


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
  QProgBar;

type

  TBatchWindowMode = (bwmNotActivated, bwmIcon, bwmNormalWindow, bwmMaximized);

  { TFBatchOberflaeche }

  //TFBatchOberflaeche = class(TForm, IViewService)
  TFBatchOberflaeche = class(TForm)
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
    procedure setOutputLevel(const level: integer);
    procedure TimerCommandTimer(Sender: TObject);
    procedure TimerActivityTimer(Sender: TObject);
    procedure TimerDetailTimer(Sender: TObject);
    procedure TimerProcessMessTimer(Sender: TObject);

  private
    //Bit: TBitmap32;
    //BlendF: TBlendFunction;
    //P: TPoint;
    //Size: TSize;
  public
    { Public-Deklarationen }

    procedure ForceStayOnTop(YesNo: boolean);
    procedure setWindowState(BatchWindowMode: TBatchWindowMode);

    procedure LoadSkin(const skindirectory: string);

    function SetPicture(No: integer; const BitmapFile: string;
      const theLabel: string): boolean; overload;

    //interface
    procedure setVisible(b: boolean);
    procedure showProgressBar(b: boolean);
    procedure setProgress(percent: integer);

    procedure setCommandLabel(s: string);
    procedure setInfoLabel(s: string);
    procedure setDetailLabel(s: string);
    procedure setActivityLabel(s: string);
    procedure setVersionLabel(s: string);
    procedure showAcitvityBar(show : boolean);
    //procedure setCPUActivityLabel(s: string);

    //procedure ForceStayOnTop (YesNo : Boolean);

    //procedure setWindowState (BatchWindowMode: TBatchWindowMode);



    function setPicture(const BitmapFile: string; const theLabel: string): boolean;
      overload;
    procedure centerWindow;
  end;

var

  //viewService : IViewService;
  //FBatchOberflaeche:  IViewService;   this seems to produce erratic null pointer exceptions when application terminates
  FBatchOberflaeche: TFBatchOberflaeche;
  LableInfoDefaultFontSize : integer;

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
  Progressbar: TQProgressBar;
  //ActivityBar: TQProgressBar;



const
  BatchScreenOnTop: boolean = False;

  centralImageNo = 2;

  bitmapFilenameProductDefault = 'winst3.png';
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
  skindirectoryDefault = 'skin';
  skindirectoryCustomWin = '/usr/local/share/opsi-script/customskin';
  {$ENDIF DARWIN}


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
  MyFavoriteFont : String='';
  SecondFont : String='';
  //Properties: TStringList;
  labelcontent : String='';
  i: integer=0;
  strColor : String='';
  //textcolor: TColor;
  //Alpha: boolean;

begin
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
  (*
  ActivityBar := TQProgressBar.Create(nil);
  ActivityBar.Position := 0;
  ActivityBar.Visible := true;
  with ActivityBar do
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
   *)

  //FBatchOberflaeche.DoubleBuffered:=True;
  //FBatchOberflaeche.ControlStyle := ControlStyle + [csOpaque] ;
  BorderIcons := [];
  //Visible  := true;
  useCommandLabel := True;
  useDetailLabel := True;
  useActivityLabel := True;
  timeCommandLabel := True;
  timeDetailLabel := True;
  timeActivityLabel := True;

  setOutputLevel(3);


  Panel.DoubleBuffered := True;
  //if not runSilent then
  Visible := True;


  setWindowState(bwmNormalWindow);
  Position := poScreenCenter;
  if Height < InnerHeight + StartTop then
    StartTop := (Height - InnerHeight) div 2;
  if Width < InnerWidth + StartLeft then
    StartLeft := (Width - InnerWidth) div 2;

  SetBounds(StartLeft, StartTop, InnerWidth, InnerHeight);



  Color := clBlue;
  Panel.Color := clBlue;

  LabelProduct.Font.Color := clWhite;
  LabelVersion.Font.Color := clWhite;
  LabelInfo.Font.Color := clWhite;
  LabelDetail.Font.Color := clWhite;
  LabelCommand.Font.Color := clWhite;
  LabelProgress.Font.Color := clWhite;
  //LabelProgress1.Font.Color := clRed;


  loadskin('');

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
  //EnableFontSmoothing(LabelProgress1);
  {$ENDIF WINDOWS}

  //Visible  := true;
  //Repaint;
  ProcessMess;
  //Refresh;
  //ProcessMess;
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


procedure TFBatchOberflaeche.LoadSkin(const skindirectory: string);
var
  skindir : String='';
  skinFile : String='';
  skinIni: TIniFile;
  filename : String='';
  paramstr0enc : string;

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

  //logdatei.DependentAdd('Loading skin from: '+skindir,LLessential);
  //skinDir := ExtractFilePath(paramstr0enc) + skindirectoryDevelopment;
  startupmessages.Append('Loading skin from: '+skindir);
  skinFile := skinDir +PathDelim+ 'skin.ini';

  if FileExists(skinFile) then
  begin
    LabelInfo.Caption := rsLoadingSkin;
    try
      skinIni := TIniFile.Create(skinFile);
      Color := myStringToTColor(skinIni.ReadString('Form', 'Color', 'clBlack'));
      try
        Panel.Color := myStringToTColor(skinIni.ReadString('Form', 'Color', 'clBlack'));

      except
      end;

      try
        setAlignment(LabelVersion, skinIni.ReadString('LabelVersion', 'Alignment', ''));
        LabelVersion.Left := skinIni.ReadInteger('LabelVersion', 'Left', 76);
        LabelVersion.Top := skinIni.ReadInteger('LabelVersion', 'Top', 16);
        LabelVersion.Width := skinIni.ReadInteger('LabelVersion', 'Width', 85);
        LabelVersion.Height := skinIni.ReadInteger('LabelVersion', 'Height', 16);
        LabelVersion.Font.Name :=
          skinIni.ReadString('LabelVersion', 'FontName', 'Arial');
        LabelVersion.Font.Size := skinIni.ReadInteger('LabelVersion', 'FontSize', 8);
        LabelVersion.Font.Color :=
          myStringToTColor(skinIni.ReadString('LabelVersion', 'FontColor', 'clBlack'));
        LabelVersion.Font.Style := [];
        if ('true' = skinIni.ReadString('LabelVersion', 'FontBold', 'false'))
        then
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
        //setAlignment(LabelProduct, skinIni.ReadString('LabelProduct','Alignment','') );
        LabelProduct.Left := skinIni.ReadInteger('LabelProduct', 'Left', 144);
        LabelProduct.Top := skinIni.ReadInteger('LabelProduct', 'Top', 207);
        LabelProduct.Width := skinIni.ReadInteger('LabelProduct', 'Width', 313);
        LabelProduct.Height := skinIni.ReadInteger('LabelProduct', 'Height', 41);
        LabelProduct.Font.Name :=
          skinIni.ReadString('LabelProduct', 'FontName', 'Arial');
        LabelProduct.Font.Size := skinIni.ReadInteger('LabelProduct', 'FontSize', 32);
        LabelProduct.Font.Color :=
          myStringToTColor(skinIni.ReadString('LabelProduct', 'FontColor', 'clBlack'));
        LabelProduct.Font.Style := [];
        if ('true' = skinIni.ReadString('LabelProduct', 'FontBold', 'false'))
        then
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
        setAlignment(LabelCommand,
          skinIni.ReadString('LabelCommand', 'Alignment', ''));
        LabelCommand.Left := skinIni.ReadInteger('LabelCommand', 'Left', 29);
        LabelCommand.Top := skinIni.ReadInteger('LabelCommand', 'Top', 316);
        LabelCommand.Width := skinIni.ReadInteger('LabelCommand', 'Width', 524);
        LabelCommand.Height := skinIni.ReadInteger('LabelCommand', 'Height', 15);
        LabelCommand.Font.Name :=
          skinIni.ReadString('LabelCommand', 'FontName', 'Arial');
        LabelCommand.Font.Size := skinIni.ReadInteger('LabelCommand', 'FontSize', 8);
        LabelCommand.Font.Color :=
          myStringToTColor(skinIni.ReadString('LabelCommand', 'FontColor', 'clBlack'));
        LabelCommand.Font.Style := [];
        if ('true' = skinIni.ReadString('LabelCommand', 'FontBold', 'false'))
        then
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
        LabelInfo.Left := skinIni.ReadInteger('LabelInfo', 'Left', 29);
        LabelInfo.Top := skinIni.ReadInteger('LabelInfo', 'Top', 272);
        LabelInfo.Width := skinIni.ReadInteger('LabelInfo', 'Width', 524);
        LabelInfo.Height := skinIni.ReadInteger('LabelInfo', 'Height', 20);
        LabelInfo.Font.Name := skinIni.ReadString('LabelInfo', 'FontName', 'Arial');
        LableInfoDefaultFontSize := skinIni.ReadInteger('LabelInfo', 'FontSize', 11);
        LabelInfo.Font.Size :=  LableInfoDefaultFontSize;
        LabelInfo.Font.Color :=
          myStringToTColor(skinIni.ReadString('LabelInfo', 'FontColor', 'clBlack'));
        LabelInfo.Font.Style := [];
        if ('true' = skinIni.ReadString('LabelInfo', 'FontBold', 'false'))
        then
          LabelInfo.Font.Style := LabelInfo.Font.Style + [fsBold];
        if ('true' = skinIni.ReadString('LabelInfo', 'FontItalic', 'false'))
        then
          LabelInfo.Font.Style := LabelInfo.Font.Style + [fsItalic];
        if ('true' = skinIni.ReadString('LabelInfo', 'FontUnderline', 'false'))
        then
          LabelInfo.Font.Style := LabelInfo.Font.Style + [fsUnderline];
      except
      end;

      try
        setAlignment(LabelDetail, skinIni.ReadString('LabelDetail', 'Alignment', ''));
        LabelDetail.Left := skinIni.ReadInteger('LabelDetail', 'Left', 29);
        LabelDetail.Top := skinIni.ReadInteger('LabelDetail', 'Top', 292);
        LabelDetail.Width := skinIni.ReadInteger('LabelDetail', 'Width', 524);
        LabelDetail.Height := skinIni.ReadInteger('LabelDetail', 'Height', 20);
        LabelDetail.Font.Name :=
          skinIni.ReadString('LabelDetail', 'FontName', 'Arial');
        LabelDetail.Font.Size := skinIni.ReadInteger('LabelDetail', 'FontSize', 8);
        LabelDetail.Font.Color :=
          myStringToTColor(skinIni.ReadString('LabelDetail', 'FontColor', 'clBlack'));
        LabelDetail.Font.Style := [];
        if ('true' = skinIni.ReadString('LabelDetail', 'FontBold', 'false'))
        then
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
        setAlignment(LabelProgress,
          skinIni.ReadString('LabelProgress', 'Alignment', ''));
        LabelProgress.Left := skinIni.ReadInteger('LabelProgress', 'Left', 29);
        LabelProgress.Top := skinIni.ReadInteger('LabelProgress', 'Top', 331);
        LabelProgress.Width := skinIni.ReadInteger('LabelProgress', 'Width', 524);
        LabelProgress.Height := skinIni.ReadInteger('LabelProgress', 'Height', 15);
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

      (*
       try
        setAlignment(LabelProgress1,
          skinIni.ReadString('LabelProgress1', 'Alignment', ''));
        LabelProgress1.Left := skinIni.ReadInteger('LabelProgress1', 'Left', 60);
        LabelProgress1.Top := skinIni.ReadInteger('LabelProgress1', 'Top', 350);
        LabelProgress1.Width := skinIni.ReadInteger('LabelProgress1', 'Width', 520);
        LabelProgress1.Height := skinIni.ReadInteger('LabelProgress1', 'Height', 20);
        LabelProgress1.Font.Name :=
          skinIni.ReadString('LabelProgress1', 'FontName', 'Arial');
        LabelProgress1.Font.Size :=
          skinIni.ReadInteger('LabelProgress1', 'FontSize', 8);
        LabelProgress1.Font.Color :=
          StringToColor(skinIni.ReadString('LabelProgress1', 'FontColor', 'clRed'));
        LabelProgress1.Font.Style := [];
        if ('true' = skinIni.ReadString('LabelProgress1', 'FontBold', 'false'))
        then
          LabelProgress1.Font.Style := LabelProgress1.Font.Style + [fsBold];
        if ('true' = skinIni.ReadString('LabelProgress1', 'FontItalic', 'false'))
        then
          LabelProgress1.Font.Style := LabelProgress1.Font.Style + [fsItalic];
        if ('true' = skinIni.ReadString('LabelProgress1', 'FontUnderline', 'false'))
        then
          LabelProgress1.Font.Style := LabelProgress1.Font.Style + [fsUnderline];
      except
      end;
      *)

      try
        filename := skinDir +PathDelim+ skinIni.ReadString('ImageBackground', 'File', 'bg.png');
        if FileExists(filename) and not IsDirectory(filename) then
          ImageBackground.picture.loadFromFile(filename);
      except
      end;

      try
        ImageProduct.Left := skinIni.ReadInteger('ImageProduct', 'Left', 224);
        ImageProduct.Top := skinIni.ReadInteger('ImageProduct', 'Top', 44);
        ImageProduct.Width := skinIni.ReadInteger('ImageProduct', 'Width', 144);
        ImageProduct.Height := skinIni.ReadInteger('ImageProduct', 'Height', 144);
        filename := skinDir +PathDelim+
          skinIni.ReadString('ImageProduct', 'File', 'product.png');
        FileName := ExpandFileName(FileName);
        if FileExists(filename) and not IsDirectory(filename) then
          ImageProduct.picture.loadFromFile(filename);
      except
      end;

      try
        ImageLogo1.Left := skinIni.ReadInteger('ImageLogo1', 'Left', 60);
        ImageLogo1.Top := skinIni.ReadInteger('ImageLogo1', 'Top', 80);
        ImageLogo1.Width := skinIni.ReadInteger('ImageLogo1', 'Width', 160);
        ImageLogo1.Height := skinIni.ReadInteger('ImageLogo1', 'Height', 160);
        filename := skinDir +PathDelim+
          skinIni.ReadString('ImageLogo1', 'File', 'logo1.png');
        FileName := ExpandFileName(FileName);
        if FileExists(filename) and not IsDirectory(filename) then
          ImageLogo1.picture.loadFromFile(filename);
      except
      end;

      try
        ImageLogo2.Left := skinIni.ReadInteger('ImageLogo2', 'Left', 450);
        ImageLogo2.Top := skinIni.ReadInteger('ImageLogo2', 'Top', 80);
        ImageLogo2.Width := skinIni.ReadInteger('ImageLogo2', 'Width', 531);
        ImageLogo2.Height := skinIni.ReadInteger('ImageLogo2', 'Height', 91);
        filename := skinDir +PathDelim+
          skinIni.ReadString('ImageLogo2', 'File', 'logo2.png');
        FileName := ExpandFileName(FileName);
        if FileExists(filename) and not IsDirectory(filename) then
          ImageLogo2.picture.loadFromFile(filename);
      except
      end;

      try
        Image1Over.Left := skinIni.ReadInteger('Image1Over', 'Left', 216);
        Image1Over.Top := skinIni.ReadInteger('Image1Over', 'Top', 36);
        Image1Over.Width := skinIni.ReadInteger('Image1Over', 'Width', 160);
        Image1Over.Height := skinIni.ReadInteger('Image1Over', 'Height', 160);
        filename := skinDir +PathDelim+
          skinIni.ReadString('Image1Over', 'File', 'over1.png');
        FileName := ExpandFileName(FileName);
        if FileExists(filename) and not IsDirectory(filename) then
          Image1Over.picture.loadFromFile(filename);
      except
      end;

      try
        Image2Over.Left := skinIni.ReadInteger('Image2Over', 'Left', 26);
        Image2Over.Top := skinIni.ReadInteger('Image2Over', 'Top', 267);
        Image2Over.Width := skinIni.ReadInteger('Image2Over', 'Width', 531);
        Image2Over.Height := skinIni.ReadInteger('Image2Over', 'Height', 91);
        filename := skinDir +PathDelim+
          skinIni.ReadString('Image2Over', 'File', 'over2.png');
        FileName := ExpandFileName(FileName);
        if FileExists(filename) and not IsDirectory(filename) then
          Image2Over.picture.loadFromFile(filename);
      except
      end;

      try
        ProgressBar.Left := skinIni.ReadInteger('ProgressBar', 'Left', 96);
        ProgressBar.Top := skinIni.ReadInteger('ProgressBar', 'Top', 235);
        ProgressBar.Width := skinIni.ReadInteger('ProgressBar', 'Width', 401);
        ProgressBar.Height := skinIni.ReadInteger('ProgressBar', 'Height', 17);
        ProgressBar.BarColor := myStringToTColor(
          skinIni.ReadString('ProgressBar', 'BarColor', 'clBlack'));
        ProgressBar.StartColor :=
          myStringToTColor(skinIni.ReadString('ProgressBar', 'StartColor', 'clBlack'));
        ProgressBar.FinalColor :=
          myStringToTColor(skinIni.ReadString('ProgressBar', 'FinalColor', 'clBlack'));
        ProgressBar.ShapeColor :=
          myStringToTColor(skinIni.ReadString('ProgressBar', 'ShapeColor', 'clBlack'));
        ProgressBar.backgroundColor :=
          myStringToTColor(skinIni.ReadString('ProgressBar', 'BackgroundColor', 'clWhite'));
        if ('true' = skinIni.ReadString('ProgressBar', 'Shaped', 'false')) then
          ProgressBar.Shaped := True
        else
          ProgressBar.Shaped := False;
        ProgressBar.BlockSize := skinIni.ReadInteger('ProgressBar', 'BlockSize', 6);
        ProgressBar.SpaceSize := skinIni.ReadInteger('ProgressBar', 'SpaceSize', 1);
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

      try
        ActivityBar.Left := skinIni.ReadInteger('ActivityBar', 'Left', 60);
        ActivityBar.Top := skinIni.ReadInteger('ActivityBar', 'Top', 350);
        ActivityBar.Width := skinIni.ReadInteger('ActivityBar', 'Width', 320);
        ActivityBar.Height := skinIni.ReadInteger('ActivityBar', 'Height', 10);
        {$IFDEF WINDOWS}
        SendMessage(ActivityBar.Handle, PBM_SETBARCOLOR, 0,
          myStringToTColor(skinIni.ReadString('ActivityBar', 'BarColor', 'clBlue')));
        //ActivityBar.Perform(PBM_SETBARCOLOR, 0,
        //  StringToColor(skinIni.ReadString('ActivityBar', 'BarColor', 'clBlue')));
        //ProgressBarActivity.Brush.Color:= clNone; // Set Background colour
        {$ENDIF WINDOWS}
        ActivityBar.Enabled:=true;
        ActivityBar.Position:=50;
      except
      end;


    except
      LabelInfo.Caption := 'Failed to load skin!';
    end;
    skinIni.Free;

    PanelFillScreen.Color := Panel.Color;
    //panelLocateInfoPanel.Color := Panel.Color;
  end;

end;

procedure TFBatchOberflaeche.setOutputLevel(const level: integer);
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
end;



procedure TFBatchOberflaeche.ForceStayOnTop(YesNo: boolean);
begin
  if YesNo then
  begin
    //setWindowState (bwmMaximized);
    FormStyle := fsSystemStayOnTop;
    BringToFront;
    Application.ProcessMessages;
    FormStyle := fsStayOnTop;
    (* FBatchOberflaeche.BorderIcons := []; *)
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
  case BatchWindowMode of
    bwmNotActivated: WindowState := wsnormal;
    bwmIcon: WindowState := wsminimized;
    bwmNormalWindow: WindowState := wsnormal;
    bwmMaximized: WindowState := wsMaximized;
  end;

end;

(*
procedure TFBatchOberflaeche.WMEraseBkGnd (var t:tmessage);
begin
  t.result := LRESULT(TRUE);
end;
*)

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

procedure TFBatchOberflaeche.ShowProgress(Prozente: integer);
begin
  Progressbar.Position := Prozente;
  //ProcessMess;
end;

procedure TFBatchOberflaeche.ShowAcitvityBar(show : boolean);
begin
  ActivityBar.Visible:=show;
  //ProcessMess;
end;


function TFBatchOberflaeche.setPicture
  (No: integer; const BitmapFile: string; const theLabel: string): boolean;
var
  //bitmap, resizedBitmap: TBitmap;
  //newHeight, newWidth: integer;
  //stretchRect: TRect;
  BitmapFilename : String='';
  errorinfo : String='';
  shorty : String='';
begin

  if BitmapFile = '' then
    bitmapFilename := bitmapFilenameProductDefault
  else
    bitmapfilename := bitmapfile;
  try
    //bitmap := TBitmap.create;
    //resizedBitmap := TBitmap.create;
    //bitmap.loadFromFile(BitmapFilename);

    BitmapFilename := ExpandFileName(BitmapFilename);
    if fileexists(BitmapFilename) then
      ImageProduct.picture.loadFromFile(BitmapFilename)
    else
      ImageProduct.picture := nil;

    ImageProduct.Repaint;
    ProcessMess;
    // do not auto size short labels because this will look ugly
    // use optimalfill to decrease the font only on large labels
    if LabelProduct.Canvas.TextWidth(theLabel) > (LabelProduct.Width -
      LabelProduct.Width div 5) then
      LabelProduct.OptimalFill := True
    else
    begin
      LabelProduct.OptimalFill := False;
      LabelProduct.Font.Size := 32;
    end;

    LabelProduct.Caption := theLabel;
    Result := True;
  except
    on e: Exception do
    begin
      ErrorInfo := 'Error: ' + e.message;
      Result := False;
    end
  end;

  processMess;

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
  i := (Width - panel.Width) div 2;
  Panel.Left := i;
  if Height > panel.Height + standardTopMargin then
    panel.top := standardTopMargin
  else
    panel.top := 0;

  panel.Repaint;

  ImageOpsiBackground.Left := Width - ImageOpsiBackground.Width;
  ImageOpsiBackground.Top := Height - ImageOpsiBackground.Height;

  //LabelVersion.BringToFront
end;

//interface
procedure TFBatchOberflaeche.setVisible(b: boolean);
begin
  Visible := b;
end;

procedure TFBatchOberflaeche.showProgressBar(b: boolean);
begin
  progressBarActive(b);
end;


procedure TFBatchOberflaeche.setProgress(percent: integer);
begin
  showProgress(percent);
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
    //if s <> '' then
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

(*
procedure TFBatchOberflaeche.setCPUActivityLabel(s: string);
begin
    LabelProgress1.Caption := s;
    ProcessMess;
end;
*)

procedure TFBatchOberflaeche.setVersionLabel(s: string);
begin
  LabelVersion.Caption := s;
end;



//procedure TFBatchOberflaeche.ForceStayOnTop (YesNo : Boolean);

//procedure TFBatchOberflaeche.setWindowState (BatchWindowMode: TBatchWindowMode);

function TFBatchOberflaeche.setPicture(const BitmapFile: string;
  const theLabel: string): boolean;
begin
  Result := setPicture(0, BitmapFile, theLabel);
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
  TimerProcessMess.Enabled:= true;
  ProcessMess;
end;

procedure TFBatchOberflaeche.centerWindow;
begin
  Position:=poScreenCenter;
  FBatchOberflaeche.MoveToDefaultPosition;
end;



initialization


 {$i osbatchgui.lrs}


end.

