unit notifierguicontrol;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF WINDOWS}
  Classes, SysUtils,
  notifierform,
  inifiles,
  contnrs,
  Graphics,
  Controls,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  Buttons,
  Forms,
  oslog;

type
  TNFormPos = (fpTopRight, fpBottomRight, fpTopLeft, fpCenter, fpCustom);
  TNFormAppear = (fapNone, fapStd, fapFade, fapFadeUp, fapFadeDown,
    fapUp, fapDown, fapUnknown);
  TNFormDisappear = (fdpNone, fdpStd, fdpFade, fdpFadeUp, fdpFadeDown,
    fdpUp, fdpDown, fdpUnknown);
  TLabels = array of TLabel;
  //TButtons = array of TButton;
  TButtons = array of TSpeedButton;

  TTransparentMemo = class(TScrollBox)
  private
    scrolllabel: TLabel;
    repainttimer: TTimer;
    { private declarations }
    //objlist: TObjectList;
  public
    procedure onrepainttimer(Sender: TObject);
    { public declarations }
    constructor Create(Aowner: TComponent);
    destructor Destroy;
  end;

  TMemos = array of TTransparentMemo;

procedure openSkinIni(ininame: string);
procedure myChoiceClick(Sender: TObject);
procedure hideNForm;
function setLabelCaptionById(aktId, aktMessage: string): boolean;
procedure setButtonCaptionById(choiceindex: integer; aktMessage: string);
procedure shutdownNotifier;

const
  appearStepSize = 5;

var
  inHideNForm: boolean = False;

implementation

uses
  notifierdatamodule,
  notifier_json,
  notifier_base;


var
  myini: TIniFile;
  navlist: TStringList;
  labellist: TStringList;
  buttonlist: TStringList;
  memolist: TStringList;
  sectionlist: TStringList;
  nformpos: TNFormPos;
  appearmode: TNFormAppear;
  disappearmode: TNFormDisappear;
  fadein, fadeout, hidden, transparent: boolean;
  slidein, slideout: string;
  LabelArray: TLabels;
  ButtonArray: TButtons;
  MemoArray: Tmemos;
  labelcounter, buttoncounter, memocounter: integer;
  designPPI : integer;



// from
// http://stackoverflow.com/questions/41068387/how-to-make-transparent-form-in-lazarus
procedure WindowTransparent(const f: THandle; const tpcolor: integer);
begin
  {$IFDEF WINDOWS}
  SetWindowLongPtr(f, GWL_EXSTYLE, GetWindowLongPtr(f, GWL_EXSTYLE) or WS_EX_LAYERED);
  SetLayeredWindowAttributes(f, tpcolor, 0, LWA_COLORKEY);
  {$ENDIF WINDOWS}
end;

procedure TTransparentMemo.onrepainttimer(Sender: TObject);
begin
  Nform.Image1.repaint;
end;

constructor TTransparentMemo.Create(Aowner: TComponent);
begin
  inherited;
  scrolllabel := TLabel.Create(self);
  scrolllabel.Parent := self;
  scrolllabel.Align := alClient;
  self.HorzScrollBar.Visible := False;
  self.VertScrollBar.Visible := True;
  self.BorderStyle := bsNone;
  repainttimer := TTimer.Create(self);
  repainttimer.Interval := 100;
  repainttimer.OnTimer := @onrepainttimer;
  repainttimer.Enabled := True;
end;

destructor TTransparentMemo.Destroy;
begin
  scrolllabel.Free;
  repainttimer.Free;
  inherited;
end;

procedure free_runtime_objects;
var
  i: integer;
begin
  try
    for i := 0 to memocounter - 1 do
      if Assigned(memoarray[i]) then
        memoarray[i].Free;
    for i := 0 to labelcounter - 1 do
      if Assigned(LabelArray[i]) then
        LabelArray[i].Free;
    for i := 0 to buttoncounter - 1 do
      if Assigned(ButtonArray[i]) then
        ButtonArray[i].Free;
    if Assigned(mythread) then
      mythread.Free;
    //LogDatei.Close;
    //LogDatei.Free;
  finally
  end;
end;

procedure shutdownNotifier;
begin
  logdatei.log('Terminate Thread', LLInfo);
  if Assigned(mythread) then mythread.Terminate;
  logdatei.log('Hide Form', LLInfo);
  hideNForm;
  Nform.Close;
  logdatei.log('Wait a scond', LLInfo);
  DataModule1.ProcessMess;
  sleep(1000);
  //logdatei.log('free_runtime_objects', LLnotice);
  //free_runtime_objects;
  //DataModule1.DataModuleDestroy(nil);
  logdatei.log('terminate', LLnotice);
  Application.Terminate;
  Halt(0);
end;

function setLabelCaptionById(aktId, aktMessage: string): boolean;
var
  index: integer;
  indexstr: string;
begin
  Result := False;
  logdatei.log('Set for id: "' + aktId + '" the message: "' + aktMessage + '"', LLInfo);
  try
    // get labelarray index for aktid stored in labellist
    indexstr := labellist.Values[aktId];
    if indexstr <> '' then
    begin
      index := StrToInt(indexstr);
      logdatei.log('Found index: ' + IntToStr(index) + ' for id: "' + aktId, LLDebug2);
      logdatei.log('Label name by index: Found index: ' +
        LabelArray[index].Name, LLDebug2);
      LabelArray[index].Caption := aktMessage;
      LabelArray[index].Repaint;
      Application.ProcessMessages;
      logdatei.log('Finished: Set for id: "' + aktId + '" the message: "' +
        aktMessage + '"', LLInfo);
      Result := True;
    end
    else
      LogDatei.log('No index found for id: ' + aktId, LLDebug2);
    indexstr := memolist.Values[aktId];
    if indexstr <> '' then
    begin
      index := StrToInt(indexstr);
      logdatei.log('Found memoindex: ' + IntToStr(index) + ' for id: "' +
        aktId, LLDebug2);
      logdatei.log('scrollbox name by index: Found index: ' +
        memoarray[index].Name, LLDebug2);
      memoarray[index].scrolllabel.Caption := aktMessage;
      memoarray[index].Repaint;
      Application.ProcessMessages;
      logdatei.log('Finished: Set for id: "' + aktId + '" the message: "' +
        aktMessage + '"', LLInfo);
      Result := True;
    end
    else
      LogDatei.log('No index found for id: ' + aktId, LLDebug2);
  except
    on E: Exception do
    begin
      LogDatei.log('Error: Label / scrollbox not found by index: ' +
        IntToStr(index) + ' id: ' + aktId, LLError);
      LogDatei.log('Error: Message: ' + E.Message, LLError);
      Result := False;
    end;
  end;
end;

procedure setButtonCaptionById(choiceindex: integer; aktMessage: string);
var
  index: integer;
  indexstr: string;
begin
  logdatei.log('Set for Button id: "' + IntToStr(choiceindex) +
    '" the caption: "' + aktMessage + '"', LLInfo);
  try
    // get labelarray index for aktid stored in labellist
    indexstr := buttonlist.Values[IntToStr(choiceindex)];
    if indexstr <> '' then
    begin
      index := StrToInt(indexstr);
      logdatei.log('Found Button index: ' + indexstr + ' for id: "' +
        IntToStr(choiceindex) + '"', LLDebug2);
      logdatei.log('Button name by index: Found index: ' +
        ButtonArray[index].Name, LLDebug2);
      ButtonArray[index].Caption := aktMessage;
      ButtonArray[index].Repaint;
      Application.ProcessMessages;
      logdatei.log('Finished: Set for Button id: "' + IntToStr(choiceindex) +
        '" the message: "' + aktMessage + '"', LLInfo);
    end
    else
      LogDatei.log('No index found for Button id: ' + IntToStr(choiceindex), LLDebug2);
  except
    on E: Exception do
    begin
      LogDatei.log('Error: Button not found by index: ' + IntToStr(index) +
        ' id: ' + IntToStr(choiceindex), LLError);
      LogDatei.log('Error: Message: ' + E.Message, LLError);
    end;
  end;
end;


procedure myChoiceClick(Sender: TObject);
var
  choice: integer;

begin
  choice := TSpeedButton(Sender).Tag;
  logdatei.log('Button clicked: choice: ' + IntToStr(choice), LLInfo);
  buttonPushedToService(choice);
  if mynotifierkind = 'popup' then
  begin
    mythread.Terminate;
    logdatei.log('We are in popup, button close clicked: terminate', LLInfo);
    hideNForm;
    DataModule1.DataModuleDestroy(nil);
  end;
end;


function fontresize(num: integer): integer;
begin
  Result := round(num * 0.5);

  {$IFDEF LINUX}
  //Result :=  round(Result * ((Nform.DesignTimePPI / Screen.PixelsPerInch) + 0.2));
  Result :=  round(Result * ((Screen.PixelsPerInch / Nform.DesignTimePPI) + 0.0));
  {$ENDIF LINUX}
  if Result < 8 then
    Result := 8;
end;

function StringToAlignment(str: string): TAlignment;
begin
  Result := taCenter;
  try
    if LowerCase(str) = 'left' then
      Result := taLeftJustify
    else if LowerCase(str) = 'right' then
      Result := taRightJustify
    else if LowerCase(str) = 'center' then
      Result := taCenter
    else
      LogDatei.log('Error: Could not convert to Alignment: ' + str, LLError);
    case Result of
      taCenter: LogDatei.log('Using Alignment taCenter from: ' + str, LLDebug2);
      taLeftJustify: LogDatei.log('Using Alignment taLeftJustify from: ' +
          str, LLDebug2);
      taRightJustify: LogDatei.log('Using Alignment taRightJustify from: ' +
          str, LLDebug2);
    end;
  except
    on E: Exception do
    begin
      LogDatei.log('Error: Could not convert to Alignment: ' + str, LLError);
      LogDatei.log('Error: Message: ' + E.Message, LLError);
    end;
  end;
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
  //var
  //  message: string;
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

function calculate_appearmode: TNFormAppear;
begin
  {$IFDEF DARWIN}
  slidein := '';
  {$ENDIF DARWIN}
  if hidden then
    Result := fapNone
  else
  begin // not hidden
    // transparent could not work with fadein
    if transparent then
      fadein := False;


    if fadein = False then
    begin
      if slidein = '' then
      begin
        Result := fapStd;
      end
      else
      begin
        if slidein = 'up' then
          Result := fapUp
        else if slidein = 'down' then
          Result := fapDown
        else
          Result := fapUnknown;
      end;
    end
    else // now fadein
    begin
      if slidein = '' then
      begin
        Result := fapFade;
      end
      else
      begin
        if slidein = 'up' then
          Result := fapFadeUp
        else if slidein = 'down' then
          Result := fapFadeDown
        else
          Result := fapUnknown;
      end;
    end;
  end; // not hidden
  if Result = fapUnknown then
  begin
    LogDatei.log('Warning: could not calculate appearmode - fall back to standard',
      LLWarning);
    Result := fapStd;
  end;
end;

function calculate_disappearmode: TNFormDisappear;
begin
  if hidden then
    Result := fdpNone
  else
  begin // not hidden
    // transparent could not work with fadein
    if transparent then
      fadeout := False;

    if fadeout = False then
    begin
      if slideout = '' then
      begin
        Result := fdpStd;
      end
      else
      begin
        if slideout = 'up' then
          Result := fdpUp
        else if slideout = 'down' then
          Result := fdpDown
        else
          Result := fdpUnknown;
      end;
    end
    else // now fadeout
    begin
      if slideout = '' then
      begin
        Result := fdpFade;
      end
      else
      begin
        if slideout = 'up' then
          Result := fdpFadeUp
        else if slideout = 'down' then
          Result := fdpFadeDown
        else
          Result := fdpUnknown;
      end;
    end;
  end; // not hidden
  if Result = fdpUnknown then
  begin
    LogDatei.log('Warning: could not calculate disappearmode - fall back to standard',
      LLWarning);
    Result := fdpStd;
  end;
end;


procedure showNForm;
var
  startx, starty, stopy, x, y, i: integer;
begin
  // position

  case nformpos of
    fpTopRight:
    begin
      x := screen.Width;
      starty := 20;
      startx := x - nform.Width - 20;
      LogDatei.log('Form position: fpTopRight', LLInfo);
    end;
    fpBottomRight:
    begin
      x := screen.Width;
      //y := screen.Height;
      y := screen.WorkAreaHeight;
      starty := y - nform.Height;
      startx := x - nform.Width;
      LogDatei.log('Form position: fpBottomRight', LLInfo);
    end;
    fpTopLeft:
    begin
      starty := 20;
      startx := 20;
      LogDatei.log('Form position: fpTopLeft', LLInfo);
    end;
    fpCenter:
    begin
      x := screen.Width div 2;
      y := screen.Height div 2;
      starty := y - (nform.Height div 2);
      startx := x - (nform.Width div 2);
      LogDatei.log('Form position: fpCenter', LLInfo);
    end;
    fpCustom:
    begin
      // nothing to change
      LogDatei.log('Form position: fpCustom', LLInfo);
    end;
    else
      LogDatei.log('Error: Unknown Form position', LLError);
  end;

  // show with appearmode

  case appearmode of
    fapNone: LogDatei.log('Will not show: fapNone', LLWarning);
    fapStd:
    begin
      nform.Top := starty;
      nform.Left := startx;
      nform.Show;
    end;
    fapFade:
    begin
      nform.Top := starty;
      nform.Left := startx;
      nform.AlphaBlend := True;
      nform.AlphaBlendValue := 0;
      nform.Show;
      //for i := 1 to 255 do
      { make it faster - most time is needed for repaint/processmsg }
      i := appearStepSize;
      while i <= 255 do
      begin
        sleep(1);
        nform.AlphaBlendValue := i;
        nform.BringToFront;
        nform.Repaint;
        DataModule1.ProcessMess;
        i := i + appearStepSize;
      end;
    end;
    fapFadeUp:
    begin
      x := screen.Width;
      stopy := nform.Height;
      nform.Height := 0;
      y := screen.WorkAreaHeight;
      {$IFDEF LINUX}
      { no valid control toolbar detection on Linux - so guess }
      y := screen.WorkAreaHeight - 40;
      {$ENDIF LINUX}
      nform.Top := y;
      nform.Left := startx;
      nform.AlphaBlend := True;
      nform.AlphaBlendValue := 0;
      nform.Show;
      //for i := 1 to stopy do
      i := appearStepSize;
      while i <= stopy do
      begin
        //Sleep(1);
        nform.AlphaBlendValue := i;
        //y := screen.WorkAreaHeight;
        nform.Top := y - i;
        nform.Height := nform.Height + appearStepSize;
        nform.BringToFront;
        nform.Repaint;
        DataModule1.ProcessMess;
        i := i + appearStepSize;
      end;
      //for i := stopy to 255 do
      while i <= 255 do
      begin
        //sleep(1);
        nform.AlphaBlendValue := i;
        nform.BringToFront;
        nform.Repaint;
        DataModule1.ProcessMess;
        i := i + appearStepSize;
      end;
      nform.Refresh;
      DataModule1.ProcessMess;
    end;
    fapFadeDown:
    begin
      x := screen.Width;
      stopy := nform.Height;
      nform.Height := 0;
      y := 0;
      nform.Top := y;
      nform.Left := startx;
      nform.AlphaBlend := True;
      nform.AlphaBlendValue := 0;
      nform.Show;
      //for i := 1 to stopy do
      i := appearStepSize;
      while i <= stopy do
      begin
        Sleep(1);
        nform.AlphaBlendValue := i;
        nform.Height := nform.Height + appearStepSize;
        nform.BringToFront;
        nform.Repaint;
        DataModule1.ProcessMess;
        i := i + appearStepSize;
      end;
      //for i := stopy to 255 do
      while i <= 255 do
      begin
        sleep(1);
        nform.AlphaBlendValue := i;
        nform.BringToFront;
        nform.Repaint;
        DataModule1.ProcessMess;
        i := i + appearStepSize;
      end;
    end;
    fapUp:
    begin
      x := screen.Width;
      stopy := nform.Height;
      nform.Height := 0;
      y := screen.WorkAreaHeight;
      {$IFDEF LINUX}
      { no valid control toolbar detection on Linux - so guess }
      y := screen.WorkAreaHeight - 40;
      {$ENDIF LINUX}
      nform.Top := y;
      nform.Left := startx;
      nform.Show;
      //for i := 1 to stopy do
      i := appearStepSize;
      while i <= stopy do
      begin
        Sleep(1);
        y := screen.WorkAreaHeight;
        nform.Top := y - i;
        nform.Height := nform.Height + appearStepSize;
        nform.BringToFront;
        nform.Repaint;
        DataModule1.ProcessMess;
        i := i + appearStepSize;
      end;
    end;
    fapDown:
    begin
      x := screen.Width;
      stopy := nform.Height;
      nform.Height := 0;
      y := 0;
      nform.Top := y;
      nform.Left := startx;
      nform.Show;
      //for i := 1 to stopy do
      i := appearStepSize;
      while i <= stopy do
      begin
        Sleep(1);
        nform.Height := nform.Height + appearStepSize;
        nform.BringToFront;
        nform.Repaint;
        DataModule1.ProcessMess;
        i := i + appearStepSize;
      end;
    end;
  end;
  if mynotifierkind = 'event' then
  begin
    nform.FormStyle := fsNormal;
    logdatei.log('FormStyle := fsNormal', LLDebug);
    nform.Repaint;
    DataModule1.ProcessMess;
  end;
end;

procedure hideNForm;
var
  //startx, starty, stopx,
  stopy, y, i: integer;
begin
  try
    inHideNForm := True;
    // hide with disappearmode

    case disappearmode of
      fdpNone: LogDatei.log('Will not hide: fdpNone', LLWarning);
      fdpStd:
      begin
        LogDatei.log('Will hide with: fdpStd', LLDebug2);
        nform.hide;
      end;
      fdpFade:
      begin
        LogDatei.log('Will hide with: fdpFade', LLDebug2);
        nform.AlphaBlend := True;
        nform.AlphaBlendValue := 255;
        //for i := 1 to  255 do
        i := appearStepSize;
        while i <= 255 do
        begin
          sleep(1);
          nform.AlphaBlendValue := 255 - i;
          nform.Repaint;
          DataModule1.ProcessMess;
          i := i + appearStepSize;
        end;
        nform.hide;
      end;
      fdpFadeUp:
      begin
        LogDatei.log('Will hide with: fdpFadeUp', LLDebug2);
        stopy := nform.Height;
        nform.AlphaBlend := True;
        nform.AlphaBlendValue := 255;
        //for i := 1 to stopy do
        i := appearStepSize;
        while i <= stopy do
        begin
          Sleep(1);
          nform.AlphaBlendValue := 255 - i;
          nform.Height := nform.Height - appearStepSize;
          nform.Repaint;
          DataModule1.ProcessMess;
          i := i + appearStepSize;
        end;
      end;
      fdpFadeDown:
      begin
        LogDatei.log('Will hide with: fdpFadeDown', LLDebug2);
        stopy := nform.Height;
        y := nform.Top;
        nform.AlphaBlend := True;
        nform.AlphaBlendValue := 255;
        nform.Repaint;
        DataModule1.ProcessMess;
        //for i := 1 to stopy do
        i := appearStepSize;
        while i <= stopy do
        begin
          Sleep(1);
          nform.AlphaBlendValue := 255 - i;
          nform.Height := stopy - i;
          nform.Top := y + i;
          nform.Repaint;
          DataModule1.ProcessMess;
          i := i + appearStepSize;
        end;
        LogDatei.log('Finished hide with: fdpFadeDown', LLDebug2);
      end;
      fdpUp:
      begin
        LogDatei.log('Will hide with: fdpUp', LLDebug2);
        stopy := nform.Height;
        //for i := 1 to stopy do
        i := appearStepSize;
        while i <= stopy do
        begin
          Sleep(1);
          nform.Height := stopy - appearStepSize;
          nform.Repaint;
          DataModule1.ProcessMess;
          i := i + appearStepSize;
        end;
      end;
      fdpDown:
      begin
        LogDatei.log('Will hide with: fdpDown', LLDebug2);
        stopy := nform.Height;
        //for i := 1 to stopy do
        i := appearStepSize;
        while i <= stopy do
        begin
          Sleep(1);
          nform.Height := stopy - appearStepSize;
          nform.Top := stopy + i;
          nform.Repaint;
          DataModule1.ProcessMess;
          i := i + appearStepSize;
        end;
      end;
    end;

  finally
    inHideNForm := False;
    LogDatei.log('Finished hideNForm', LLDebug2);
  end;
end;


procedure objectByIndex(myIni: TIniFile; aktsection: string);
var
  //myLabel: TLabel;
  //myButton: TButton;
  mytmpstr: string;
  mytmpint1, mytmpint2: integer;
  choiceindex: integer;
  tmpinistr: string;
  tmpbool: boolean;
  //myscreen : TScreen;
begin
  //myscreen := TScreen.Create(Application);
  if aktsection = 'Form' then
  begin
    nform.Color := myStringToTColor(myini.ReadString(aktsection, 'color', 'clWhite'));
    //Transparent = true
    // StayOnTop = true
    tmpinistr := myini.ReadString(aktsection, 'StayOnTop', 'false');
    if not TryStrToBool(tmpinistr, tmpbool) then
    begin
      tmpbool := False;
      LogDatei.log('Error: No valid boolean value for StayOnTop: ' + tmpinistr, LLError);
    end;
    if tmpbool then
    begin
      if mynotifierkind = 'event' then
      begin
        nform.FormStyle := fsSystemStayOnTop;
        logdatei.log('FormStyle := fsSystemStayOnTop', LLDebug);
      end
      else
      begin
        nform.FormStyle := fsSystemStayOnTop;
        logdatei.log('FormStyle := fsSystemStayOnTop', LLDebug);
      end;
    end
    else
      logdatei.log('FormStyle := fsNormal', LLDebug);

    //Frame = false
    tmpinistr := myini.ReadString(aktsection, 'Frame', 'false');
    if not TryStrToBool(tmpinistr, tmpbool) then
    begin
      tmpbool := False;
      LogDatei.log('Error: No valid boolean value for Frame: ' + tmpinistr, LLError);
    end;
    if not tmpbool then
      nform.BorderStyle := bsNone;

    //Resizable = false
    //Closeable = false
    //Minimizable = false
    //Text = Opsi Dialog
    nform.Caption := myini.ReadString(aktsection, 'Text', 'opsi');
    //Width = 100
    nform.Width := myini.ReadInteger(aktsection, 'Width', 10);
    //Height = 100
    nform.Height := myini.ReadInteger(aktsection, 'Height', 10);
    //Left = -1
    // Top = 1
    mytmpint1 := myini.ReadInteger(aktsection, 'Left', 10);
    mytmpint2 := myini.ReadInteger(aktsection, 'Top', 10);
    if (mytmpint1 < 0) and (mytmpint2 > 0) then
      nformpos := fpTopRight
    else if (mytmpint1 < 0) and (mytmpint2 < 0) then
      nformpos := fpBottomRight
    else if (mytmpint1 > 0) and (mytmpint2 < 0) then
      nformpos := fpTopLeft
    else if (mytmpint1 = 0) and (mytmpint2 = 0) then
      nformpos := fpCenter
    else
    begin
      nformpos := fpCustom;
      nform.Left := mytmpint1;
      nform.Top := mytmpint2;
    end;
    {$IFDEF LINUX}
    // scale new scrollbox:
    nform.AutoAdjustLayout(lapAutoAdjustForDPI, nform.DesignTimePPI, screen.PixelsPerInch, 0, 0);
    {$ENDIF LINUX}
    //Hidden = false
    tmpinistr := myini.ReadString(aktsection, 'Hidden', 'false');
    if not TryStrToBool(tmpinistr, hidden) then
    begin
      hidden := False;
      LogDatei.log('Error: No valid boolean value for hidden: ' + tmpinistr, LLError);
    end;
    //FadeIn = true
    //fadein := strToBool(myini.ReadString(aktsection, 'FadeIn', 'false'));
    tmpinistr := myini.ReadString(aktsection, 'FadeIn', 'false');
    if not TryStrToBool(tmpinistr, fadein) then
    begin
      fadein := False;
      LogDatei.log('Error: No valid boolean value for fadein: ' + tmpinistr, LLError);
    end;
    //FadeOut = true
    //fadeout := strToBool(myini.ReadString(aktsection, 'FadeOut', 'false'));
    tmpinistr := myini.ReadString(aktsection, 'FadeOut', 'false');
    if not TryStrToBool(tmpinistr, fadeout) then
    begin
      fadeout := False;
      LogDatei.log('Error: No valid boolean value for fadeout: ' + tmpinistr, LLError);
    end;
    //SlideIn = up
    slidein := myini.ReadString(aktsection, 'SlideIn', '');
    //SlideOut = down
    slideout := myini.ReadString(aktsection, 'SlideOut', '');
    //Systray = true
    //Icon = opsi.ico
    mytmpstr := ExtractFilePath(myini.FileName);
    mytmpstr := mytmpstr + myini.ReadString(aktsection, 'Icon', 'not_existing');
    if FileExists(mytmpstr) then
      nform.Icon.LoadFromFile(mytmpstr);
    //Transparent = true
    //transparent := strToBool(myini.ReadString(aktsection, 'Transparent', 'false'));
    tmpinistr := myini.ReadString(aktsection, 'Transparent', 'false');
    if not TryStrToBool(tmpinistr, transparent) then
    begin
      transparent := False;
      LogDatei.log('Error: No valid boolean value for transparent: ' +
        tmpinistr, LLError);
    end;
    //TransparentColor = 255,0,255
    if transparent then
    begin
      mytmpint1 := 0;
      mytmpint1 := myStringToTColor(myini.ReadString(aktsection,
        'TransparentColor', 'clWhite'));
      WindowTransparent(nform.Handle, mytmpint1);
    end;
  end
  else
  if aktsection = 'ImageBg' then
  begin
    mytmpstr := ExtractFilePath(myini.FileName);
    mytmpstr := mytmpstr + myini.ReadString(aktsection, 'File', '');
    nform.Image1.Picture.LoadFromFile(mytmpstr);
    //nform.Image1.AutoAdjustLayout(lapAutoAdjustForDPI, nform.DesignTimePPI, screen.PixelsPerInch, 0, 0);
    nform.Image1.AutoAdjustLayout(lapAutoAdjustForDPI, designPPI, screen.PixelsPerInch, 0, 0);
    nform.Image1.Repaint;
    DataModule1.ProcessMess;
  end
  else
  if aktsection = 'LabelMessage' then
  begin
    LogDatei.log('Start reading: ' + aktsection, LLDebug);
    Inc(memocounter);
    SetLength(memoarray, memocounter + 1);
    memoarray[memocounter] := TTransparentMemo.Create(nform);
    memoarray[memocounter].Parent := nform;
    //memoarray[memocounter].AutoSize := True;
    memoarray[memocounter].Name := aktsection;
    memoarray[memocounter].scrolllabel.WordWrap := True;
    memoarray[memocounter].Left := myini.ReadInteger(aktsection, 'Left', 10);
    memoarray[memocounter].Top := myini.ReadInteger(aktsection, 'Top', 10);
    memoarray[memocounter].Width := myini.ReadInteger(aktsection, 'Width', 10);
    memoarray[memocounter].Height := myini.ReadInteger(aktsection, 'Height', 10);
    //memoarray[labelcounter].Anchors := [akTop,akLeft,akRight,akBottom];
    memoarray[memocounter].Anchors := [akTop, akLeft, akRight];

    mytmpstr := myini.ReadString(aktsection, 'FontName', 'Arial');
    if screen.Fonts.IndexOf(mytmpstr) = -1 then
    begin
      {$IFDEF WINDOWS} mytmpstr := 'Arial'; {$ENDIF WINDOWS}
      //{$IFDEF LINUX} mytmpstr := 'Liberation Sans Narrow'; {$ENDIF LINUX}
      {$IFDEF LINUX} mytmpstr := 'Liberation Sans'; {$ENDIF LINUX}
    end;
    memoarray[memocounter].scrolllabel.Font.Name := mytmpstr;
    { fontresize makes also hdpi correction for linux}
    memoarray[memocounter].scrolllabel.Font.Size :=
      fontresize(myini.ReadInteger(aktsection, 'FontSize', 10));
    memoarray[memocounter].scrolllabel.Font.Color :=
      myStringToTColor(myini.ReadString(aktsection, 'FontColor', 'clBlack'));
    memoarray[memocounter].scrolllabel.Font.Bold :=
      strToBool(myini.ReadString(aktsection, 'FontBold', 'false'));
    memoarray[memocounter].scrolllabel.Font.Italic :=
      strToBool(myini.ReadString(aktsection, 'FontItalic', 'false'));
    memoarray[memocounter].scrolllabel.Font.Underline :=
      strToBool(myini.ReadString(aktsection, 'FontUnderline', 'false'));
    memoarray[memocounter].scrolllabel.Alignment :=
      StringToAlignment(myini.ReadString(aktsection, 'Alignment', 'left'));
    //memoarray[memocounter].Transparent :=
    //  strToBool(myini.ReadString(aktsection, 'Transparent', 'false'));
    memoarray[memocounter].Tag := memocounter;
    memoarray[memocounter].scrolllabel.Caption :=
      myini.ReadString(aktsection, 'Text', '');
    //memoarray[memocounter].scrolllabel.Caption := 'test'+#10+#13+
    //  'test'+#10+#13+'test'+#10+#13+'test'+#10+#13+'test'+#10+#13+'test'+#10+#13+'test'+#10+#13+
    //  'test'+#10+#13+'test'+#10+#13+'test'+#10+#13+'test'+#10+#13+'test'+#10+#13+'test'+#10+#13;
    //memoarray[memocounter].ReadOnly:=true;
    //memoarray[memocounter].ScrollBars:=ssAutoVertical;
    //{$IFDEF WINDOWS}
    // scale new scrollbox:
    //memoarray[memocounter].AutoAdjustLayout(lapAutoAdjustForDPI, nform.DesignTimePPI, screen.PixelsPerInch, 0, 0);
    memoarray[memocounter].AutoAdjustLayout(lapAutoAdjustForDPI, designPPI, screen.PixelsPerInch, 0, 0);

    //{$ENDIF WINDOWS}
   // make transparent
    memoarray[memocounter].ControlStyle :=
      memoarray[memocounter].ControlStyle - [csOpaque] + [csParentBackground];
    memoarray[memocounter].Visible := True;

    // feed memolist: id = index of memoarray ; id = aktsection striped by 'Label'
    memolist.Add(copy(aktsection, 6, 100) + '=' + IntToStr(memocounter));
    logdatei.log('memolist add: ' + copy(aktsection, 6, 100) + '=' +
      IntToStr(memocounter), LLDebug2);
    LogDatei.log('Finished reading: ' + aktsection, LLDebug2);
  end
  else
  if pos('Label', aktsection) > 0 then
  begin
    LogDatei.log('Start reading: ' + aktsection, LLDebug);
    Inc(labelcounter);
    SetLength(LabelArray, labelcounter + 1);
    LabelArray[labelcounter] := TLabel.Create(nform);
    LabelArray[labelcounter].Parent := nform;
    LabelArray[labelcounter].AutoSize := True;
    LabelArray[labelcounter].Name := aktsection;
    LabelArray[labelcounter].WordWrap := True;
    (*
    {$IFDEF LINUX}
    LabelArray[labelcounter].AutoSize := False;
    LabelArray[labelcounter].WordWrap := False;
    LabelArray[labelcounter].AdjustFontForOptimalFill;
    {$ENDIF LINUX}
    *)
    LabelArray[labelcounter].Left := myini.ReadInteger(aktsection, 'Left', 10);
    LabelArray[labelcounter].Top := myini.ReadInteger(aktsection, 'Top', 10);
    LabelArray[labelcounter].Width := myini.ReadInteger(aktsection, 'Width', 10);
    LabelArray[labelcounter].Height := myini.ReadInteger(aktsection, 'Height', 10);
    //LabelArray[labelcounter].Anchors := [akTop,akLeft,akRight,akBottom];
    LabelArray[labelcounter].Anchors := [akTop, akLeft, akRight];

    mytmpstr := myini.ReadString(aktsection, 'FontName', 'Arial');
    if screen.Fonts.IndexOf(mytmpstr) = -1 then
    begin
      {$IFDEF WINDOWS} mytmpstr := 'Arial'; {$ENDIF WINDOWS}
      //{$IFDEF LINUX} mytmpstr := 'Liberation Sans Narrow'; {$ENDIF LINUX}
      {$IFDEF LINUX} mytmpstr := 'Liberation Sans'; {$ENDIF LINUX}
    end;
    LabelArray[labelcounter].Font.Name :=  mytmpstr;
    { fontresize makes also hdpi correction for linux}
    LabelArray[labelcounter].Font.Size :=
      fontresize(myini.ReadInteger(aktsection, 'FontSize', 10));
    LabelArray[labelcounter].Font.Color :=
      myStringToTColor(myini.ReadString(aktsection, 'FontColor', 'clBlack'));
    LabelArray[labelcounter].Font.Bold :=
      strToBool(myini.ReadString(aktsection, 'FontBold', 'false'));
    LabelArray[labelcounter].Font.Italic :=
      strToBool(myini.ReadString(aktsection, 'FontItalic', 'false'));
    LabelArray[labelcounter].Font.Underline :=
      strToBool(myini.ReadString(aktsection, 'FontUnderline', 'false'));
    LabelArray[labelcounter].Alignment :=
      StringToAlignment(myini.ReadString(aktsection, 'Alignment', 'left'));
    LabelArray[labelcounter].Transparent :=
      strToBool(myini.ReadString(aktsection, 'Transparent', 'false'));
    LabelArray[labelcounter].Tag := labelcounter;
    LabelArray[labelcounter].Caption := myini.ReadString(aktsection, 'Text', '');
    //LabelArray[labelcounter].AdjustSize;
    //{$IFDEF WINDOWS}
    // scale new Label:
    //LabelArray[labelcounter].AutoAdjustLayout(lapAutoAdjustForDPI,
    //  96, nform.PixelsPerInch, 0, 0);
    //LabelArray[labelcounter].AutoAdjustLayout(lapAutoAdjustForDPI, nform.DesignTimePPI,nform.PixelsPerInch, 0, 0);
    LabelArray[labelcounter].AutoAdjustLayout(lapAutoAdjustForDPI, designPPI,nform.PixelsPerInch, 0, 0);

    //{$ENDIF WINDOWS}
    // feed labellist: id = index of LabelArray ; id = aktsection striped by 'Label'
    labellist.Add(copy(aktsection, 6, 100) + '=' + IntToStr(labelcounter));
    logdatei.log('labellist add: ' + copy(aktsection, 6, 100) + '=' +
      IntToStr(labelcounter), LLDebug2);
    LogDatei.log('Finished reading: ' + aktsection, LLDebug2);
  end
  else
  if pos('Button', aktsection) > 0 then
  begin
    LogDatei.log('Start reading: ' + aktsection, LLDebug);
    Inc(buttoncounter);
    SetLength(ButtonArray, buttoncounter + 1);
    ButtonArray[buttoncounter] := Tspeedbutton.Create(nform);
    ButtonArray[buttoncounter].Parent := nform;
    ButtonArray[buttoncounter].AutoSize := False;
    ButtonArray[buttoncounter].Name := aktsection;
    ButtonArray[buttoncounter].Left := myini.ReadInteger(aktsection, 'Left', 10);
    ButtonArray[buttoncounter].Top := myini.ReadInteger(aktsection, 'Top', 10);
    ButtonArray[buttoncounter].Width := myini.ReadInteger(aktsection, 'Width', 10);
    ButtonArray[buttoncounter].Height := myini.ReadInteger(aktsection, 'Height', 10);

    mytmpstr := myini.ReadString(aktsection, 'FontName', 'Arial');
    if screen.Fonts.IndexOf(mytmpstr) = -1 then
    begin
      {$IFDEF WINDOWS} mytmpstr := 'Arial'; {$ENDIF WINDOWS}
      //{$IFDEF LINUX} mytmpstr := 'Liberation Sans Narrow'; {$ENDIF LINUX}
      {$IFDEF LINUX} mytmpstr := 'Liberation Sans'; {$ENDIF LINUX}
    end;
    ButtonArray[buttoncounter].Font.Name := mytmpstr;
    { fontresize makes also hdpi correction for linux}
    ButtonArray[buttoncounter].Font.Size :=
      fontresize(myini.ReadInteger(aktsection, 'FontSize', 10));
    //ButtonArray[buttoncounter].Font.Color :=
    //  myStringToTColor(myini.ReadString(aktsection, 'FontColor', 'clBlack'));
    ButtonArray[buttoncounter].Font.Bold :=
      strToBool(myini.ReadString(aktsection, 'FontBold', 'false'));
    ButtonArray[buttoncounter].Font.Italic :=
      strToBool(myini.ReadString(aktsection, 'FontItalic', 'false'));
    ButtonArray[buttoncounter].Font.Underline :=
      strToBool(myini.ReadString(aktsection, 'FontUnderline', 'false'));
    //ButtonArray[buttoncounter].Alignment :=
    //  StringToAlignment(myini.ReadString(aktsection, 'Alignment', 'alLeft'));
    //ButtonArray[buttoncounter].Transparent :=
    //  strToBool(myini.ReadString(aktsection, 'Transparent', 'false'));
    choiceindex := myini.ReadInteger(aktsection, 'ChoiceIndex', 0);
    ButtonArray[buttoncounter].Tag := choiceindex;
    ButtonArray[buttoncounter].OnClick := @nform.ChoiceClick;
    //ButtonArray[buttoncounter].TabStop:= false;
    //ButtonArray[buttoncounter].TabOrder:=-1;
    ButtonArray[buttoncounter].Caption := myini.ReadString(aktsection, 'Text', '');
    //{$IFDEF WINDOWS}
    // scale new Button:
    //ButtonArray[buttoncounter].AutoAdjustLayout(lapAutoAdjustForDPI, nform.DesignTimePPI, nform.PixelsPerInch, 0, 0);
    ButtonArray[buttoncounter].AutoAdjustLayout(lapAutoAdjustForDPI, designPPI, nform.PixelsPerInch, 0, 0);
    //{$ENDIF WINDOWS}
    // feed buttonlist: id = index of ButtonArray ; id = ChoiceIndex'
    buttonlist.Add(IntToStr(choiceindex) + '=' + IntToStr(buttoncounter));
    LogDatei.log('Finished reading: ' + aktsection, LLDebug2);
  end;
  DataModule1.ProcessMess;
end;

function fillnavlist(var myIni: TIniFile): TStrings;
var
  //sectionlist,
  keylist: TStringList;
  i: integer;
  aktsection: string;
begin
  //sectionlist := Tstringlist.Create;
  keylist := TStringList.Create;
  Result := TStringList.Create;
  myini.ReadSections(sectionlist);
  for i := 0 to sectionlist.Count - 1 do
  begin
    aktsection := sectionlist[i];
    keylist.Clear;
    myIni.readsection(aktsection, keylist);
    if keylist.IndexOf('SubjectId') > -1 then
    begin
      //Result.Add(keylist.Values['SubjectId']+'='+aktsection);
      Result.Add(myIni.ReadString(aktsection, 'SubjectId', 'null') + '=' + aktsection);
    end;
  end;
end;



procedure openSkinIni(ininame: string);
var
  i: integer;
  aktsection: string;
begin
  LogDatei.log('screen.PixelsPerInch: ' + IntToStr(screen.PixelsPerInch), LLInfo);
  LogDatei.log('nform.PixelsPerInch: ' + IntToStr(nform.PixelsPerInch), LLInfo);
  //LogDatei.log('nform.DesignTimePPI: ' + nform.DesignTimePPI.ToString, LLInfo);
  LogDatei.log('designPPI: ' + intToStr(designPPI), LLInfo);

  LogDatei.log('Loading Skin config from: ' + ininame, LLInfo);
  myini := TIniFile.Create(ininame);
  navlist.AddStrings(fillnavlist(myIni));
  for i := 0 to sectionlist.Count - 1 do
  begin
    aktsection := sectionlist[i];
    LogDatei.log('Interpreting section: ' + aktsection, LLInfo);
    objectByIndex(myIni, aktsection);
  end;
  // set appearmode
  appearmode := calculate_appearmode;
  // show the form
  showNForm;
  // set disappearmode
  disappearmode := calculate_disappearmode;
  LogDatei.log('Finished initial form build in openSkinIni. ', LLDebug2);
end;




begin
  labelcounter := 0;
  buttoncounter := 0;
  memocounter := 0;
  navlist := TStringList.Create;
  labellist := TStringList.Create;
  buttonlist := TStringList.Create;
  sectionlist := TStringList.Create;
  memolist := TStringList.Create;
  designPPI:=96;
end.
