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

procedure openSkinIni(ininame: string);
procedure myChoiceClick(Sender: TObject);
procedure hideNForm;

var
  inHideNForm : boolean = false;

implementation

uses
  notifierdatamodule;

var
  myini: TIniFile;
  navlist: TStringList;
  labellist: TStringList;
  buttonlist: TStringList;
  sectionlist: TStringList;
  nformpos: TNFormPos;
  appearmode: TNFormAppear;
  disappearmode: TNFormDisappear;
  fadein, fadeout, hidden, transparent: boolean;
  slidein, slideout: string;
  LabelArray: TLabels;
  ButtonArray: TButtons;
  labelcounter, buttoncounter: integer;

  {$IFDEF WINDOWS}
// from
// http://stackoverflow.com/questions/41068387/how-to-make-transparent-form-in-lazarus
procedure WindowTransparent(const f: THandle; const tpcolor: integer);
begin
  SetWindowLongPtr(f, GWL_EXSTYLE, GetWindowLongPtr(f, GWL_EXSTYLE) or WS_EX_LAYERED);
  SetLayeredWindowAttributes(f, tpcolor, 0, LWA_COLORKEY);
end;

  {$ENDIF WINDOWS}

procedure myChoiceClick(Sender: TObject);
var
  choice: integer;
begin
  choice := TSpeedButton(Sender).Tag;
  logdatei.log('Button clicked: choice: ' + IntToStr(choice), LLInfo);
  (*
  tileindex := TProductPanel(Sender).Tag;
    pid := ProductTilesArray[tileindex].LabelId.Caption;
    *)
  if choice = 1 then
    DataModule1.Destroy;
end;


function fontresize(num: integer): integer;
begin
  Result := round(num * 0.8);
end;

function StringToAlignment(str: string): TAlignment;
begin
  Result := taCenter;
  try
    if LowerCase(str) = 'left' then
      Result := taLeftJustify
    else if LowerCase(str) = 'right' then
      Result := taLeftJustify
    else if LowerCase(str) = 'center' then
      Result := taCenter
    else
      LogDatei.log('Error: Could not convert to Alignment: ' + str, LLError);
  except
    on E: Exception do
    begin
      LogDatei.log('Error: Could not convert to Color: ' + str, LLError);
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

function calculate_appearmode: TNFormAppear;
begin
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
    LogDatei.log('Error: could not calculate appearmode - fall back to standard',
      LLError);
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
    LogDatei.log('Error: could not calculate disappearmode - fall back to standard',
      LLError);
    Result := fdpStd;
  end;
end;


procedure showNForm;
var
  startx, starty, stopx, stopy, x, y, i: integer;
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
      y := screen.Height;
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
      for i := 1 to 255 do
      begin
        sleep(1);
        nform.AlphaBlendValue := i;
        nform.Repaint;
        DataModule1.ProcessMess;
      end;
    end;
    fapFadeUp:
    begin
      x := screen.Width;
      stopy := nform.Height;
      nform.Height := 0;
      y := screen.WorkAreaHeight;
      nform.Top := y;
      nform.Left := startx;
      nform.AlphaBlend := True;
      nform.AlphaBlendValue := 0;
      nform.Show;
      for i := 1 to stopy do
      begin
        Sleep(1);
        nform.AlphaBlendValue := i;
        nform.Top := y - i;
        nform.Height := nform.Height + 1;
        nform.Repaint;
        //DataModule1.ProcessMess;
      end;
      for i := stopy to 255 do
      begin
        sleep(1);
        nform.AlphaBlendValue := i;
        nform.Repaint;
        DataModule1.ProcessMess;
      end;
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
      for i := 1 to stopy do
      begin
        Sleep(1);
        nform.AlphaBlendValue := i;
        nform.Height := nform.Height + 1;
        nform.Repaint;
        DataModule1.ProcessMess;
      end;
      for i := stopy to 255 do
      begin
        sleep(1);
        nform.AlphaBlendValue := i;
        nform.Repaint;
        DataModule1.ProcessMess;
      end;
    end;
    fapUp:
    begin
      x := screen.Width;
      stopy := nform.Height;
      nform.Height := 0;
      y := screen.WorkAreaHeight;
      nform.Top := y;
      nform.Left := startx;
      nform.Show;
      for i := 1 to stopy do
      begin
        Sleep(1);
        nform.Top := y - i;
        nform.Height := nform.Height + 1;
        nform.Repaint;
        DataModule1.ProcessMess;
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
      for i := 1 to stopy do
      begin
        Sleep(1);
        nform.Height := nform.Height + 1;
        nform.Repaint;
        DataModule1.ProcessMess;
      end;
    end;
  end;
end;

procedure hideNForm;
var
  startx, starty, stopx, stopy, x, y, i: integer;
begin
  try
  inHideNForm := true;
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
      for i := 255 to 1 do
      begin
        sleep(1);
        nform.AlphaBlendValue := i;
        nform.Repaint;
        DataModule1.ProcessMess;
      end;
      nform.hide;
    end;
    fdpFadeUp:
    begin
      LogDatei.log('Will hide with: fdpFadeUp', LLDebug2);
      stopy := nform.Height;
      nform.AlphaBlend := True;
      nform.AlphaBlendValue := 255;
      for i := 1 to stopy do
      begin
        Sleep(1);
        nform.AlphaBlendValue := 255 - i;
        nform.Height := nform.Height - 1;
        nform.Repaint;
        DataModule1.ProcessMess;
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
      for i := 1 to stopy do
      begin
        Sleep(1);
        nform.AlphaBlendValue := 255 - i;
        nform.Height := stopy - i;
        nform.Top := y + i;
        nform.Repaint;
        DataModule1.ProcessMess;
      end;
      LogDatei.log('Finished hide with: fdpFadeDown', LLDebug2);
    end;
    fdpUp:
    begin
      LogDatei.log('Will hide with: fdpUp', LLDebug2);
      stopy := nform.Height;
      for i := 1 to stopy do
      begin
        Sleep(1);
        nform.Height := stopy - 1;
        nform.Repaint;
        DataModule1.ProcessMess;
      end;
    end;
    fdpDown:
    begin
      LogDatei.log('Will hide with: fdpDown', LLDebug2);
      stopy := nform.Height;
      for i := 1 to stopy do
      begin
        Sleep(1);
        nform.Height := stopy - 1;
        nform.Top := stopy + i;
        nform.Repaint;
        DataModule1.ProcessMess;
      end;
    end;
  end;

  finally
    inHideNForm := false;
  end;
end;


function objectByIndex(myIni: TIniFile; aktsection: string): TObject;
var
  myLabel: TLabel;
  myButton: TButton;
  mytmpstr: string;
  mytmpint1, mytmpint2: integer;
begin
  if aktsection = 'Form' then
  begin
    nform.Color := myStringToTColor(myini.ReadString(aktsection, 'color', 'clWhite'));
    //Transparent = true
    // StayOnTop = true
    if strToBool(myini.ReadString(aktsection, 'StayOnTop', 'false')) then
      nform.FormStyle := fsSystemStayOnTop;

    //Frame = false
    if not strToBool(myini.ReadString(aktsection, 'Frame', 'false')) then
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
    //Hidden = false
    hidden := strToBool(myini.ReadString(aktsection, 'Hidden', 'false'));
    //FadeIn = true
    fadein := strToBool(myini.ReadString(aktsection, 'FadeIn', 'false'));
    //FadeOut = true
    fadeout := strToBool(myini.ReadString(aktsection, 'FadeOut', 'false'));
    //SlideIn = up
    slidein := myini.ReadString(aktsection, 'SlideIn', '');
    //SlideOut = down
    slideout := myini.ReadString(aktsection, 'SlideOut', '');
    //Systray = true
    //Icon = opsi.ico
    mytmpstr := ExtractFilePath(myini.FileName);
    mytmpstr := mytmpstr + myini.ReadString(aktsection, 'Icon', '');
    if FileExists(mytmpstr) then
      nform.Icon.LoadFromFile(mytmpstr);
    //Transparent = true
    transparent := strToBool(myini.ReadString(aktsection, 'Transparent', 'false'));
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
    nform.Image1.Repaint;
    DataModule1.ProcessMess;
  end
  else
  if pos('Label', aktsection) > 0 then
  begin
    LogDatei.log('Start reading: ' + aktsection, LLDebug);
    Inc(labelcounter);
    SetLength(LabelArray, labelcounter);
    LabelArray[labelcounter] := TLabel.Create(nform);
    LabelArray[labelcounter].Parent := nform;
    LabelArray[labelcounter].AutoSize := False;
    LabelArray[labelcounter].Name := aktsection;
    LabelArray[labelcounter].Left := myini.ReadInteger(aktsection, 'Left', 10);
    LabelArray[labelcounter].Top := myini.ReadInteger(aktsection, 'Top', 10);
    LabelArray[labelcounter].Width := myini.ReadInteger(aktsection, 'Width', 10);
    LabelArray[labelcounter].Height := myini.ReadInteger(aktsection, 'Height', 10);
    LabelArray[labelcounter].Font.Name :=
      myini.ReadString(aktsection, 'FontName', 'Arial');
    LabelArray[labelcounter].Font.Size :=
      fontresize(myini.ReadInteger(aktsection, 'FontSize', 10));
    LabelArray[labelcounter].Font.Color :=
      myStringToTColor(myini.ReadString(aktsection, 'FontColor', 'clBlack'));
    LabelArray[labelcounter].Font.Bold :=
      strToBool(myini.ReadString(aktsection, 'FontUnderline', 'false'));
    LabelArray[labelcounter].Font.Italic :=
      strToBool(myini.ReadString(aktsection, 'FontUnderline', 'false'));
    LabelArray[labelcounter].Font.Underline :=
      strToBool(myini.ReadString(aktsection, 'FontUnderline', 'false'));
    LabelArray[labelcounter].Alignment :=
      StringToAlignment(myini.ReadString(aktsection, 'Alignment', 'alLeft'));
    LabelArray[labelcounter].Transparent :=
      strToBool(myini.ReadString(aktsection, 'Transparent', 'false'));
    LabelArray[labelcounter].Tag := labelcounter;
    LabelArray[labelcounter].Caption := myini.ReadString(aktsection, 'Text', '');
    labellist.Add(IntToStr(labelcounter) + '=' + aktsection);
    LogDatei.log('Finished reading: ' + aktsection, LLDebug2);
  end
  else
  if pos('Button', aktsection) > 0 then
  begin
    LogDatei.log('Start reading: ' + aktsection, LLDebug);
    Inc(buttoncounter);
    SetLength(ButtonArray, buttoncounter);
    ButtonArray[buttoncounter] := Tspeedbutton.Create(nform);
    ButtonArray[buttoncounter].Parent := nform;
    ButtonArray[buttoncounter].AutoSize := False;
    ButtonArray[buttoncounter].Name := aktsection;
    ButtonArray[buttoncounter].Left := myini.ReadInteger(aktsection, 'Left', 10);
    ButtonArray[buttoncounter].Top := myini.ReadInteger(aktsection, 'Top', 10);
    ButtonArray[buttoncounter].Width := myini.ReadInteger(aktsection, 'Width', 10);
    ButtonArray[buttoncounter].Height := myini.ReadInteger(aktsection, 'Height', 10);
    ButtonArray[buttoncounter].Font.Name :=
      myini.ReadString(aktsection, 'FontName', 'Arial');
    ButtonArray[buttoncounter].Font.Size :=
      fontresize(myini.ReadInteger(aktsection, 'FontSize', 10));
    //ButtonArray[buttoncounter].Font.Color :=
    //  myStringToTColor(myini.ReadString(aktsection, 'FontColor', 'clBlack'));
    ButtonArray[buttoncounter].Font.Bold :=
      strToBool(myini.ReadString(aktsection, 'FontUnderline', 'false'));
    ButtonArray[buttoncounter].Font.Italic :=
      strToBool(myini.ReadString(aktsection, 'FontUnderline', 'false'));
    ButtonArray[buttoncounter].Font.Underline :=
      strToBool(myini.ReadString(aktsection, 'FontUnderline', 'false'));
    //ButtonArray[buttoncounter].Alignment :=
    //  StringToAlignment(myini.ReadString(aktsection, 'Alignment', 'alLeft'));
    //ButtonArray[buttoncounter].Transparent :=
    //  strToBool(myini.ReadString(aktsection, 'Transparent', 'false'));
    ButtonArray[buttoncounter].Tag := myini.ReadInteger(aktsection, 'ChoiceIndex', 0);
    ;
    ButtonArray[buttoncounter].OnClick := @nform.ChoiceClick;
    //ButtonArray[buttoncounter].TabStop:= false;
    //ButtonArray[buttoncounter].TabOrder:=-1;
    ButtonArray[buttoncounter].Caption := myini.ReadString(aktsection, 'Text', '');
    buttonlist.Add(IntToStr(buttoncounter) + '=' + aktsection);
    LogDatei.log('Finished reading: ' + aktsection, LLDebug2);
    (*
    if aktsection = 'ButtonStop' then
    begin
      myButton := TButton.Create(nform);
      myButton.Parent := nform;
      myButton.Name := aktsection;
      myButton.Caption := myini.ReadString(aktsection, 'Text', 'emppty');
    end;
    *)
  end;
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
end;




begin
  labelcounter := 0;
  buttoncounter := 0;
  navlist := TStringList.Create;
  labellist := TStringList.Create;
  buttonlist := TStringList.Create;
  sectionlist := TStringList.Create;
end.
