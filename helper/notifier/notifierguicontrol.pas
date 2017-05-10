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
  Forms,
  oslog;

type
  TNFormPos = (fpTopRight, fpBottomRight, fpTopLeft, fpCenter, fpCustom);
  TNFormAppear = (fapNone, fapStd, fapFade, fapFadeUp, fapFadeDown,
    fapUp, fapDown, fapUnknown);
  TNFormDisappear = (fpdNone, fdpStd, fdpFade, fdpFadeUp, fdpFadeDown);
  TLabels = array of TLabel;
  TButtons = array of TButton;

procedure openSkinIni(ininame: string);

implementation

uses
  notifierdatamodule;

var
  myini: TIniFile;
  navlist: TStringList;
  sectionlist: TStringList;
  nformpos: TNFormPos;
  appearmode: TNFormAppear;
  disappearmode: TNFormDisappear;
  fadein, fadeout, hidden, transparent: boolean;
  slidein, slideout: string;
  LabelArray :  TLabels;
  ButtonArray : TButtons;
  labelcounter : integer;

  {$IFDEF WINDOWS}
// from
// http://stackoverflow.com/questions/41068387/how-to-make-transparent-form-in-lazarus
procedure WindowTransparent(const f: THandle; const tpcolor: integer);
begin
  SetWindowLongPtr(f, GWL_EXSTYLE, GetWindowLongPtr(f, GWL_EXSTYLE) or WS_EX_LAYERED);
  SetLayeredWindowAttributes(f, tpcolor, 0, LWA_COLORKEY);
end;

  {$ENDIF WINDOWS}



function rgbStringToColor(str: string): TColor;
var
  red, green, blue: byte;
  start, Count: integer;
  remaining: string;
begin
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
  Result := RGBToColor(red, green, blue);
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

    end;
    fapFadeDown:
    begin

    end;
    fapUp:
    begin
      x := screen.Width;
      stopy := nform.Height;
      nform.Height := 0;
      y := screen.WorkAreaHeight;
      nform.Top := y;
      nform.Left := startx;
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

    end;
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
    mytmpstr := myini.ReadString(aktsection, 'color', '255,255,255');
    try
      nform.Color := StringToColor(mytmpstr);
    except
      try
        nform.Color := rgbStringToColor(mytmpstr);
      except
      end;
    end;

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
      mytmpstr := myini.ReadString(aktsection, 'TransparentColor', '255,255,255');
      try
        mytmpint1 := StringToColor(mytmpstr);
      except
        try
          mytmpint1 := rgbStringToColor(mytmpstr);
        except
        end;
      end;
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
    (*
     SetLength(ProductTilesArray, counter + 1);
    ProductTilesArray[counter] := TProductPanel.Create(FopsiClientKiosk.FlowPanelTiles);
    ProductTilesArray[counter].LabelId.Caption :=
      ZMQueryDataSet1.FieldByName('ProductId').AsString;
      *)
    if aktsection = 'LabelStatus' then
    begin
      myLabel := TLabel.Create(nform);
      myLabel.Parent := nform;
      myLabel.Name := aktsection;
      myLabel.Caption := myini.ReadString(aktsection, 'Text', '');
    end;
  end
  else
  if pos('Button', aktsection) > 0 then
  begin
    if aktsection = 'ButtonStop' then
    begin
      myButton := TButton.Create(nform);
      myButton.Parent := nform;
      myButton.Name := aktsection;
      myButton.Caption := myini.ReadString(aktsection, 'Text', 'emppty');
    end;
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
end;


begin
  navlist := TStringList.Create;
  sectionlist := TStringList.Create;
end.
