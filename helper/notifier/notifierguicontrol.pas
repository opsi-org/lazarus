unit notifierguicontrol;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF WINDOWS}
  lclintf,
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
  oslog,
  combobutton,
  fgl,
  Lazfileutils;

type
  TNFormPos = (fpTopRight, fpBottomRight, fpTopLeft, fpCenter, fpCustom);
  TNFormAppear = (fapNone, fapStd, fapFade, fapFadeUp, fapFadeDown,
    fapUp, fapDown, fapUnknown);
  TNFormDisappear = (fdpNone, fdpStd, fdpFade, fdpFadeUp, fdpFadeDown,
    fdpUp, fdpDown, fdpUnknown);
  TLabels = array of TLabel;
  //TButtons = array of TButton;
  //TButtons = array of TSpeedButton;
  TButtons = array of TComboButton;

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

  TMyRange = class(TObject)
    startval: integer;
    endval: integer;
    btnarrayindex: integer;
  end;

  TMyRangeList = specialize TFPGObjectList<TMyRange>;

 (*
  TConfirmRecord = record
      show: boolean;
      title : string;
      text : string;
   end;
  TConfirms = array of TConfirmRecord;
 *)

procedure openSkinIni(ininame: string);
procedure myChoiceClick(Sender: TObject);
procedure hideNForm;
function setLabelCaptionById(aktId, aktMessage: string): boolean;
procedure setButtonCaptionById(choiceindex: integer; aktMessage: string);
procedure shutdownNotifier;
procedure logmouseenter(Sender: TObject);
procedure logmouseleave(Sender: TObject);

const
  appearStepSize = 5;

var
  inHideNForm: boolean = False;
//useConfirmDialog: boolean = False;

implementation

uses
  notifierdatamodule,
  notifier_json,
  notifier_base;

var
  myini: TIniFile;
  navlist: TStringList;
  labellist: TStringList;
  //buttonlist: TStringList;
  rangelist: TStringList;
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
  labelcounter, memocounter: integer;
  buttoncounter: integer = -1;
  designPPI: integer;
  //mymouseenter: TMethod;
  //mymouseleave: TMethod;
  myRangelistList: TMyRangeList;
//ConfirmArray: TConfirms;


  (*
  type
   TMyClass = class(TObject)
      fld1 : string;
   end;

   TMyList = specialize TFPGObjectList<TMyClass>;

var
   list : TMyList;
   c : TMyClass;

begin
   // create the list and add an element
   list := TMyList.Create;
   c := TMyClass.Create;
   c.fld1 := 'c1';
   list.Add(c);
   // retrieve an element from the list
   c := list[0];
   *)

function getIndexFromRangeList(searchval: integer; var startval: integer;
  var btnarrayindex: integer): integer;
var
  i: integer;
  found: boolean;
begin
  Result := -1;
  found := False;
  i := 0;
  while (not found) and (i < myRangelistList.Count) do
  begin
    if (searchval >= myRangelistList.Items[i].startval) and
      (searchval <= myRangelistList.Items[i].endval) then
    begin
      found := True;
      startval := myRangelistList.Items[i].startval;
      btnarrayindex := myRangelistList.Items[i].btnarrayindex;
      Result := i;
    end;
    Inc(i);
  end;
end;

function addRangeToList(startval, endval, btnarrayindex: integer): integer;
var
  range: TMyRange;
  index: integer;
  dummyint: integer;
begin
  Result := -1;
  index := getIndexFromRangeList(startval, dummyint, dummyint);
  if (index = -1) then
  begin
    index := getIndexFromRangeList(endval, dummyint, dummyint);
    if (index = -1) then
    begin
      range := TMyRange.Create;
      range.startval := startval;
      range.endval := endval;
      range.btnarrayindex := btnarrayindex;
      myRangelistList.Add(range);
      Result := getIndexFromRangeList(startval, dummyint, dummyint);
    end
    else
      LogDatei.log('Could not add range: ' + IntToStr(startval) +
        ' : ' + IntToStr(endval) + 'because endval exists with index: ' +
        IntToStr(index), LLerror);
  end
  else
    LogDatei.log('Could not add range: ' + IntToStr(startval) + ' : ' +
      IntToStr(endval) + 'because startval exists with index: ' +
      IntToStr(index), LLerror);

end;

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
  logdatei.log('Hide Form', LLInfo);
  hideNForm;
  Nform.Close;
  logdatei.log('Wait a scond', LLInfo);
  DataModule1.ProcessMess;
  logdatei.log('Terminate Thread', LLInfo);
  if Assigned(mythread) then
    mythread.Terminate;
  // this will end main ... and then terminate
  //sleep(1000);
  //logdatei.log('free_runtime_objects', LLnotice);
  //free_runtime_objects;
  //DataModule1.DataModuleDestroy(nil);
  //logdatei.log('terminate', LLnotice);
  //Application.Terminate;
  //DataModule1.DataModuleDestroy(nil);
  //logdatei.log('Program halted (2)', LLnotice);
  //Halt(0);
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
  rangeindex, btnindex, startval: integer;
  tmpint: integer;
  indexstr: string;
  btnfound: boolean;
begin
  logdatei.log('Set for Button id: "' + IntToStr(choiceindex) +
    '" the caption: "' + aktMessage + '"', LLInfo);
  try
    btnfound := False;
    // get ButtonArray index for aktid stored in buttonlist
    //indexstr := buttonlist.Values[IntToStr(choiceindex)];
    rangeindex := getIndexFromRangeList(choiceindex, startval, btnindex);
    if rangeindex <> -1 then
    begin
      if btnindex < length(ButtonArray) then
      begin
        logdatei.log('Found Button index: ' + IntToStr(btnindex) +
          ' for id: "' + IntToStr(choiceindex) + '"', LLDebug2);
        if startval <> ButtonArray[btnindex].btn.Tag then
          logdatei.log('Button tag: ' + IntToStr(ButtonArray[btnindex].btn.Tag) +
            ' differs from startval: ' + IntToStr(startval), LLerror);
        logdatei.log('Button name by index: Found index: ' +
          ButtonArray[btnindex].panel.Name, LLDebug2);
        if ButtonArray[btnindex].button_only then
        begin
          ButtonArray[btnindex].btn.Caption := aktMessage;
        end
        else
        begin
          tmpint := ButtonArray[btnindex].btn.Tag +
            ButtonArray[btnindex].cbox.Items.Count;
          if choiceindex > tmpint then
            // we need a new entry
            ButtonArray[btnindex].cbox.Items.Add(aktMessage)
          else

            ButtonArray[btnindex].cbox.Items[choiceindex -
              ButtonArray[btnindex].btn.Tag] :=
              aktMessage;
        end;
        ButtonArray[btnindex].panel.Repaint;
        Application.ProcessMessages;
      end
      else
        LogDatei.log('index found out of range for Button id: ' +
          IntToStr(choiceindex), LLerror);
    end
    else
      LogDatei.log('No index found for Button id: ' + IntToStr(choiceindex), LLerror);
    (*
    if indexstr = '' then
    begin
      // we did not found the button in the buttonlist - is it a combo field ?
      if (choiceindex > buttonlist.Count -1) and
        not ButtonArray[buttonlist.Count -1].button_only then
       begin
        index := buttonlist.Count -1;
        btnfound := true;
       end
       else
      LogDatei.log('No index found for Button id: ' + IntToStr(choiceindex), LLDebug2);
     end
     else
     begin
       // we did found the button in the buttonlist
       index := StrToInt(indexstr);
       btnfound := true;
     end;
     if btnfound = true then
     begin
      logdatei.log('Found Button index: ' + indexstr + ' for id: "' +
        IntToStr(choiceindex) + '"', LLDebug2);
      logdatei.log('Button name by index: Found index: ' +
        ButtonArray[index].panel.Name, LLDebug2);
      if ButtonArray[index].button_only then
      ButtonArray[index].btn.Caption := aktMessage
      end
      else
      begin
        tmpint := ButtonArray[index].btn.Tag
            + ButtonArray[index].cbox.Items.Count;
        if choiceindex > tmpint then
        // we need a new entry
        ButtonArray[index].cbox.Items.Add(aktMessage)
        else
        //
        ButtonArray[index].cbox.Items[choiceindex - ButtonArray[index].btn.Tag] := aktMessage;
      end;
      ButtonArray[index].panel.Repaint;
      Application.ProcessMessages;
      *)
    logdatei.log('Finished: Set for Button id: "' + IntToStr(choiceindex) +
      '" the message: "' + aktMessage + '"', LLInfo);


  except
    on E: Exception do
    begin
      LogDatei.log('Error: Button not found by index: ' + IntToStr(btnindex) +
        ' id: ' + IntToStr(choiceindex), LLError);
      LogDatei.log('Error: Message: ' + E.Message, LLError);
    end;
  end;
end;


procedure myChoiceClick(Sender: TObject);
var
  choice, tag: integer;
  btnindex, startvalue, rangeindex: integer;
  button_only: boolean;
  confirmed: boolean = True;

begin
  tag := TSpeedButton(Sender).Tag;
  rangeindex := getIndexFromRangeList(tag, startvalue, btnindex);
  if rangeindex <> -1 then
  begin
    LogDatei.log('got tag: ' + IntToStr(tag) + ' startvalue: ' +
      IntToStr(startvalue) + ' btnindex: ' + IntToStr(btnindex) +
      ' length array: ' + IntToStr(length(ButtonArray)), LLinfo);
    if not Assigned(ButtonArray[btnindex]) then
      logdatei.log('ButtonArray[btnindex] is not assingned :', LLError);
    try
      button_only := TComboButton(ButtonArray[btnindex]).button_only;
    except
      on E: Exception do
      begin
        logdatei.log('exception in myChoiceClick :' + E.Message, LLError);
        // this is a hard + dirty work around
        button_only := False;
      end;
    end;

    //if ButtonArray[btnindex].confirmshow and
    //  (ButtonArray[btnindex].btn.Caption <> '') then
    if ButtonArray[btnindex].confirmshow then
    begin
      with TTaskDialog.Create(NForm) do
    try
      Title := ButtonArray[btnindex].confirmtitle;
      Caption := 'opsi-setup-detector';
      Text := ButtonArray[btnindex].confirmtext;
      CommonButtons := [];
      (*
      CommonButtons := [tcbYes, tcbNo];
      DefaultButton := tcbNo;
      Buttons.Items[0].Caption:= ButtonArray[btnindex].confirmYesText;
      *)
      with TTaskDialogButtonItem(Buttons.Add) do
      begin
        Caption := ButtonArray[btnindex].confirmNoText;
        ModalResult := mrNo;
      end;
      with TTaskDialogButtonItem(Buttons.Add) do
      begin
        Caption := ButtonArray[btnindex].confirmYesText;
        ModalResult := mrYes;
      end;
      Buttons.DefaultButton := Buttons.FindButton(mrNo);
      (*
       // to confirm is not the default
      Buttons.Items[0].Default:= false;
      Buttons.Items[1].Default:= true;
      *)
      MainIcon := tdiQuestion;
      Flags := [tfUseCommandLinks, tfAllowDialogCancellation];
      if Execute then
      begin
        if ModalResult <> mrYes then
       begin
        confirmed := False;
        LogDatei.log('Button action aborted by user-confirm: ' +
          ButtonArray[btnindex].confirmtext, LLwarning);
      end
      else
        LogDatei.log('Button action confirmed by user,  text: ' +
          ButtonArray[btnindex].confirmtext, LLnotice);
      end;
    finally
      Free;
    end;
      (*
      if MessageDlg(ButtonArray[btnindex].confirmtitle,
        ButtonArray[btnindex].confirmtext, mtConfirmation,
        [mbYes, mbCancel], 0) <> mrYes then
      begin
        confirmed := False;
        LogDatei.log('Button action aborted by user-confirm: ' +
          ButtonArray[btnindex].confirmtext, LLwarning);
      end
      else
        LogDatei.log('Button action confirmed by user,  text: ' +
          ButtonArray[btnindex].confirmtext, LLnotice);
          *)
    end
    else
      LogDatei.log('Button action should not be confirmed by user.', LLinfo);

    if confirmed then
    begin
      if button_only then
      begin
        choice := TSpeedButton(Sender).Tag;
        LogDatei.log('choice is button_only, tag: ' + IntToStr(tag) +
          ' startvalue: ' + IntToStr(startvalue), LLinfo);
      end
      else
      begin
        choice := ButtonArray[btnindex].btn.Tag +
          ButtonArray[btnindex].cbox.ItemIndex;
    (*
    choice := TSpeedButton(Sender).Tag +
      TComboButton(TSpeedButton(Sender).Parent).cbox.ItemIndex;
      *)
        LogDatei.log('choice is combobutton, tag: ' + IntToStr(tag) +
          ' startvalue: ' + IntToStr(startvalue), LLinfo);
      end;


      logdatei.log('Button clicked: choice: ' + IntToStr(choice), LLnotice);
      buttonPushedToService(choice);
    end;
  end
  else
    LogDatei.log('Could not found btn for tag: ' + IntToStr(tag), LLerror);

  // this is workaround if the opsicliend do not close the popup
  if mynotifierkind = 'popup' then
  begin
    DataModule1.TimerClose.Interval := 10000;
    DataModule1.TimerClose.Enabled := True;
  end;

end;


function fontresize(num: integer): integer;
begin
  Result := round(num * 0.5);

  {$IFDEF LINUX}
  //Result :=  round(Result * ((Nform.DesignTimePPI / Screen.PixelsPerInch) + 0.2));
  Result := round(Result * ((Screen.PixelsPerInch / Nform.DesignTimePPI) + 0.0));
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

  (* removed in 4.1.1.6 : unified popup done by opsicliend
  if mynotifierkind = 'popup' then
  begin
    nform.FormStyle := fsNormal;
    nform.BorderStyle:= bsSizeable;
    logdatei.log('FormStyle := fsNormal', LLDebug);
    starty := starty + Random(starty div 2);
    startx := startx + Random(startx div 2);
    //nform.Position:= poDefaultPosOnly;
    //nform.Repaint;
    //DataModule1.ProcessMess;
  end;
  *)

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
  mytmpstr, tmpstr2, tmpstr3: string;
  mytmpint1, mytmpint2: integer;
  choiceindex: integer;
  choiceindexstr: string;
  choiceIsArray: boolean;
  choiceArrayEnd: integer;
  tmpinistr: string;
  tmpbool: boolean;
  //myscreen : TScreen;
  //myconfirm : TConfirmRecord;
begin
  try
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
        LogDatei.log('Error: No valid boolean value for StayOnTop: ' +
          tmpinistr, LLError);
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
      if not tmpbool then  // no frame
      begin
        nform.BorderStyle := bsNone;
        logdatei.log('Frame=false - so we ignore Resizable,Closeable,Minimizable ',
          LLDebug);
      end
      else
      begin // with frame
        logdatei.log('Frame=true - so we check Resizable,Closeable,Minimizable ',
          LLDebug);

        //Resizable = false
        tmpinistr := myini.ReadString(aktsection, 'Resizable', 'false');
        if not TryStrToBool(tmpinistr, tmpbool) then
        begin
          tmpbool := False;
          LogDatei.log('Error: No valid boolean value for Resizable: ' +
            tmpinistr, LLError);
        end;
        if not tmpbool then  // no Resizable
        begin
          nform.BorderStyle := bsSingle;
          logdatei.log('Resizable=false ', LLDebug);
        end
        else
        begin
          logdatei.log('Resizable=true ', LLDebug);
          nform.BorderStyle := bsSizeable;
        end;

        //Closeable = false
        tmpinistr := myini.ReadString(aktsection, 'Closeable', 'false');
        if not TryStrToBool(tmpinistr, tmpbool) then
        begin
          tmpbool := False;
          LogDatei.log('Error: No valid boolean value for Closeable: ' +
            tmpinistr, LLError);
        end;
        if not tmpbool then  // no Closeable
          logdatei.log('Closeable=false ', LLDebug)
        else
        begin
          logdatei.log('Closeable=True ', LLDebug);
          nform.BorderIcons := nform.BorderIcons + [biSystemMenu];
        end;

        //Minimizable = false
        tmpinistr := myini.ReadString(aktsection, 'Minimizable', 'false');
        if not TryStrToBool(tmpinistr, tmpbool) then
        begin
          tmpbool := False;
          LogDatei.log('Error: No valid boolean value for Minimizable: ' +
            tmpinistr, LLError);
        end;
        if not tmpbool then  // no Minimizable
          logdatei.log('Minimizable=false ', LLDebug)
        else
        begin
          logdatei.log('Minimizable=True ', LLDebug);
          nform.BorderIcons := nform.BorderIcons + [biMinimize];
        end;
      end;


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

    {$IFDEF DARWIN}
      // at bottom we have the dock
      if nformpos = fpBottomRight then
        nformpos := fpTopRight;
    {$ENDIF DARWIN}
      tmpstr2 := 'Form initial: ';
      with nform do
      begin
        tmpstr2 := tmpstr2 + ' L:' + IntToStr(Left) + ' T:' + IntToStr(Top);
        tmpstr2 := tmpstr2 + ' W:' + IntToStr(Width) + ' H:' + IntToStr(Height);
      end;
      LogDatei.log(tmpstr2, LLDebug);
    {$IFNDEF DARWIN}
      // scale new scrollbox:
      nform.AutoAdjustLayout(lapAutoAdjustForDPI, nform.DesignTimePPI,
        screen.PixelsPerInch, 0, 0);
    {$ENDIF DARWIN}
      tmpstr2 := 'Form rescale: ';
      with nform do
      begin
        tmpstr2 := tmpstr2 + ' L:' + IntToStr(Left) + ' T:' + IntToStr(Top);
        tmpstr2 := tmpstr2 + ' W:' + IntToStr(Width) + ' H:' + IntToStr(Height);
      end;
      LogDatei.log(tmpstr2, LLDebug);
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
      if not FileExists(mytmpstr) then
        LogDatei.log('Error: Background image file not found: ' +
          mytmpstr, LLError)
      else
      begin
        nform.Image1.Picture.LoadFromFile(mytmpstr);
        //nform.Image1.AutoAdjustLayout(lapAutoAdjustForDPI, nform.DesignTimePPI, screen.PixelsPerInch, 0, 0);
        nform.Image1.AutoAdjustLayout(lapAutoAdjustForDPI, designPPI,
          screen.PixelsPerInch, 0, 0);
        nform.Image1.Repaint;
        DataModule1.ProcessMess;
      end;
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
      {$IFDEF WINDOWS}
        mytmpstr := 'Arial';
      {$ENDIF WINDOWS}
        //{$IFDEF LINUX} mytmpstr := 'Liberation Sans Narrow'; {$ENDIF LINUX}
      {$IFDEF LINUX}
        mytmpstr := 'Liberation Sans';
      {$ENDIF LINUX}
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
      {$IFNDEF LINUX}
      memoarray[memocounter].AutoAdjustLayout(lapAutoAdjustForDPI, designPPI, screen.PixelsPerInch, 0, 0);
      {$ENDIF LINUX}

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
      //LabelArray[labelcounter].WordWrap := True;
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
      {$IFDEF WINDOWS}
        mytmpstr := 'Arial';
      {$ENDIF WINDOWS}
        //{$IFDEF LINUX} mytmpstr := 'Liberation Sans Narrow'; {$ENDIF LINUX}
      {$IFDEF LINUX}
        mytmpstr := 'Liberation Sans';
      {$ENDIF LINUX}
      end;
      LabelArray[labelcounter].Font.Name := mytmpstr;
      mytmpint1 := myini.ReadInteger(aktsection, 'FontSize', 10);
      mytmpint2 := fontresize(mytmpint1);
      {$IFDEF LINUX}
      { fontresize makes not a correct hdpi correction for linux}
      mytmpint2 := trunc(mytmpint1 * (designPPI / nform.PixelsPerInch)) - 1;
      {$ENDIF LINUX}
      LabelArray[labelcounter].Font.Size := mytmpint2;
      LogDatei.log('Fontsize from ini: '+inttostr(mytmpint1) +
                   ' - using Fontsize:  '+inttostr(mytmpint2), LLDebug);

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
      {$IFNDEF LINUX}
      LabelArray[labelcounter].AutoAdjustLayout(lapAutoAdjustForDPI,
        designPPI, nform.PixelsPerInch, 0, 0);
      {$ENDIF LINUX}

      //{$ENDIF WINDOWS}
      // feed labellist: id = index of LabelArray ; id = aktsection striped by 'Label'
      labellist.Add(copy(aktsection, 6, 100) + '=' + IntToStr(labelcounter));
      logdatei.log('labellist add: ' + copy(aktsection, 6, 100) +
        '=' + IntToStr(labelcounter), LLDebug2);
      LogDatei.log('Finished reading: ' + aktsection, LLDebug2);
    end
    else
    if pos('Button', aktsection) > 0 then
    begin
      LogDatei.log('Start reading: ' + aktsection, LLDebug);
      Inc(buttoncounter);
      LogDatei.log('buttoncounter: ' + IntToStr(buttoncounter), LLinfo);
      SetLength(ButtonArray, buttoncounter + 1);
      // should we create a combo or button only
    (*
    choiceindexstr : string;
    choiceIsArray : boolean;
    choiceArrayEnd : integer;
    *)
      // we have to read the choice index here in order to decide which kind of button we need
      choiceindexstr := trim(myini.ReadString(aktsection, 'ChoiceIndex', '0'));
      LogDatei.log('choiceindex from file: ' + choiceindexstr, LLinfo);
      if Pos(':', choiceindexstr) > 0 then
        choiceIsArray := True
      else
        choiceIsArray := False;
      if choiceIsArray then
      begin
        choiceindex := 0;
        mytmpstr := trim(copy(choiceindexstr, 1, Pos(':', choiceindexstr) - 1));
        LogDatei.log('choiceindex array will start with: ' + mytmpstr, LLdebug);
        if not TryStrToInt(mytmpstr, choiceindex) then
        begin
          LogDatei.log('choiceindex from file: ' + choiceindexstr +
            ' Could not convert to int: ' + mytmpstr, LLerror);
        end
        else
        begin
          LogDatei.log('choiceindex array starts with: ' +
            IntToStr(choiceindex), LLinfo);
          // get the array end
          mytmpstr := trim(copy(choiceindexstr, Pos(':', choiceindexstr) + 1,
            length(choiceindexstr)));
          LogDatei.log('choiceindex array will end with: ' + mytmpstr, LLdebug);
          if mytmpstr = '' then // Array ends never
            choiceArrayEnd := MaxInt
          else
          if not TryStrToInt(mytmpstr, choiceArrayEnd) then
            LogDatei.log('choiceArrayEnd from file: ' + choiceindexstr +
              ' Could not convert to int: ' + mytmpstr, LLerror);
          LogDatei.log('choiceindex array ends with: ' +
            IntToStr(choiceArrayEnd), LLinfo);
          mytmpint1 := addRangeToList(choiceindex, choiceArrayEnd, buttoncounter);
          if -1 = mytmpint1 then
            LogDatei.log('Error adding range. Rangeindex is: ' +
              IntToStr(mytmpint1) + ' Buttoncounter is: ' +
              IntToStr(buttoncounter), LLwarning);
        end;
      end
      else
      begin
        if TryStrToInt(choiceindexstr, choiceindex) then
        begin
          mytmpint1 := addRangeToList(choiceindex, choiceindex, buttoncounter);
          if -1 = mytmpint1 then
            LogDatei.log('Error adding range. Rangeindex is: ' +
              IntToStr(mytmpint1) + ' Buttoncounter is: ' +
              IntToStr(buttoncounter), LLwarning);
        end
        else
        begin
          LogDatei.log('choiceindex from file: ' + choiceindexstr +
            ' Could not convert to int: ' + trim(choiceindexstr), LLerror);
        end;
        LogDatei.log('choiceindex (no array) is: ' + IntToStr(choiceindex), LLinfo);
      end;
      //tmpbool := not strToBool(myini.ReadString(aktsection, 'ComboButton', 'false'));
      // constructing the path to the button icon
      try
        tmpstr2 := ExtractFileNameWithoutExt(myconfigfile) + '_button_icon.png';
        if FileExists(tmpstr2) then
          ButtonArray[buttoncounter] :=
            TComboButton.Create(nform, tmpstr2, not choiceIsArray)
        else
          ButtonArray[buttoncounter] :=
            TComboButton.Create(nform, '', not choiceIsArray);
      except
        on E: Exception do
        begin
          LogDatei.log('Failed to create Combubutton with button_only: ' +
            BoolToStr(not choiceIsArray, True) + ' pathToIcon: ' + tmpstr2, LLError);
          LogDatei.log('Error: Message: ' + E.Message, LLError);
        end;
      end;
      ButtonArray[buttoncounter].panel.Parent := nform;
      //ButtonArray[buttoncounter].AutoSize := False;
      ButtonArray[buttoncounter].panel.Name := aktsection;
      ButtonArray[buttoncounter].panel.Left := myini.ReadInteger(aktsection, 'Left', 10);
      ButtonArray[buttoncounter].panel.Top := myini.ReadInteger(aktsection, 'Top', 10);
      ButtonArray[buttoncounter].panel.Width :=
        myini.ReadInteger(aktsection, 'Width', 10);
      ButtonArray[buttoncounter].panel.Height :=
        myini.ReadInteger(aktsection, 'Height', 10);
      tmpstr2 := 'Button' + IntToStr(buttoncounter);
      with ButtonArray[buttoncounter].panel do
      begin
        tmpstr2 := tmpstr2 + ' L:' + IntToStr(Left) + ' T:' + IntToStr(Top);
        tmpstr2 := tmpstr2 + ' W:' + IntToStr(Width) + ' H:' + IntToStr(Height);
      end;
      LogDatei.log(tmpstr2, LLDebug);

      mytmpstr := myini.ReadString(aktsection, 'FontName', 'Arial');
      if screen.Fonts.IndexOf(mytmpstr) = -1 then
      begin
      {$IFDEF WINDOWS}
        mytmpstr := 'Arial';
      {$ENDIF WINDOWS}
        //{$IFDEF LINUX} mytmpstr := 'Liberation Sans Narrow'; {$ENDIF LINUX}
      {$IFDEF LINUX}
        mytmpstr := 'Liberation Sans';
      {$ENDIF LINUX}
      end;
      ButtonArray[buttoncounter].panel.Font.Name := mytmpstr;
      { fontresize makes also hdpi correction for linux}
      ButtonArray[buttoncounter].panel.Font.Size :=
        fontresize(myini.ReadInteger(aktsection, 'FontSize', 10));
      tmpstr2 := 'Font: ' + mytmpstr + ' Size:' +
        IntToStr(ButtonArray[buttoncounter].panel.Font.Size);
      LogDatei.log(tmpstr2, LLDebug);
      //ButtonArray[buttoncounter].Font.Color :=
      //  myStringToTColor(myini.ReadString(aktsection, 'FontColor', 'clBlack'));
      ButtonArray[buttoncounter].panel.Font.Bold :=
        strToBool(myini.ReadString(aktsection, 'FontBold', 'false'));
      ButtonArray[buttoncounter].panel.Font.Italic :=
        strToBool(myini.ReadString(aktsection, 'FontItalic', 'false'));
      ButtonArray[buttoncounter].panel.Font.Underline :=
        strToBool(myini.ReadString(aktsection, 'FontUnderline', 'false'));
      //ButtonArray[buttoncounter].Alignment :=
      //  StringToAlignment(myini.ReadString(aktsection, 'Alignment', 'alLeft'));
      //ButtonArray[buttoncounter].Transparent :=
      //  strToBool(myini.ReadString(aktsection, 'Transparent', 'false'));
      //choiceindex := myini.ReadInteger(aktsection, 'ChoiceIndex', 0);
      ButtonArray[buttoncounter].btn.Tag := choiceindex;
      ButtonArray[buttoncounter].btn.OnClick := @nform.ChoiceClick;
      //ButtonArray[buttoncounter].TabStop:= false;
      //ButtonArray[buttoncounter].TabOrder:=-1;
      mytmpstr := myini.ReadString(aktsection, 'Text', '');
      if ButtonArray[buttoncounter].button_only then
        ButtonArray[buttoncounter].btn.Caption := mytmpstr
      else
      begin
        ButtonArray[buttoncounter].cbox.Items.Add(mytmpstr);
        if showtest then
        begin
          ButtonArray[buttoncounter].cbox.Items.Add('Jetzt neu starten');
          ButtonArray[buttoncounter].cbox.Items.Add('Neustart um 18:00');
          ButtonArray[buttoncounter].cbox.Items.Add('Reboot at 18:00');
        end;
        ButtonArray[buttoncounter].cbox.ItemIndex := 0;
        ButtonArray[buttoncounter].cbox.OnClick := @nform.cboxClick;
        ButtonArray[buttoncounter].cbox.OnEditingDone := @nform.cboxEditdone;
      end;
      //{$IFDEF WINDOWS}
      // scale new Button:
      //ButtonArray[buttoncounter].AutoAdjustLayout(lapAutoAdjustForDPI, nform.DesignTimePPI, nform.PixelsPerInch, 0, 0);

      {$IFNDEF LINUX}
      ButtonArray[buttoncounter].panel.AutoAdjustLayout(lapAutoAdjustForDPI,
        designPPI, nform.PixelsPerInch, 0, 0);
      {$ENDIF LINUX}

      //mytmpint1 := ButtonArray[buttoncounter].Height;
      //ButtonArray[buttoncounter].Height := trunc(mytmpint1 * (nform.PixelsPerInch / designPPI));
      tmpstr2 := 'After ReScale: Button' + IntToStr(buttoncounter);
      with ButtonArray[buttoncounter].panel do
      begin
        tmpstr2 := tmpstr2 + ' L:' + IntToStr(Left) + ' T:' + IntToStr(Top);
        tmpstr2 := tmpstr2 + ' W:' + IntToStr(Width) + ' H:' + IntToStr(Height);
      end;
      LogDatei.log(tmpstr2, LLDebug);
      //{$ENDIF WINDOWS}
      // feed buttonlist: id = index of ButtonArray ; id = ChoiceIndex'
      //buttonlist.Add(IntToStr(choiceindex) + '=' + IntToStr(buttoncounter));
      ButtonArray[buttoncounter].panel.Repaint;
      if LogDatei.LogLevel > 6 then
      begin
        ButtonArray[buttoncounter].panel.OnMouseEnter := @nform.mymouseenter;
        ButtonArray[buttoncounter].panel.OnMouseLeave := @nform.mymouseleave;
        LogDatei.log('Finished reading: ' + aktsection, LLDebug);
      end;
      // handle confirm dialog configuration
      ButtonArray[buttoncounter].confirmshow :=
        strToBool(myini.ReadString(aktsection, 'Confirmation', 'false'));
      ButtonArray[buttoncounter].confirmtitle :=
        myini.ReadString(aktsection, 'ConfirmationTitle', '');
      ButtonArray[buttoncounter].confirmtext :=
        myini.ReadString(aktsection, 'ConfirmationText', '');
      ButtonArray[buttoncounter].confirmYesText :=
        myini.ReadString(aktsection, 'confirmYesText', '');
      ButtonArray[buttoncounter].confirmNoText :=
        myini.ReadString(aktsection, 'confirmNoText', '');
    end;
    DataModule1.ProcessMess;

  except
    on E: Exception do
    begin
      Logdatei.log('Exception in objectByIndex: ', LLCritical);
      Logdatei.log(e.ClassName + ' system message: "' + E.Message,
        LLCritical);
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
  LogDatei.log('screen.PixelsPerInch: ' + IntToStr(screen.PixelsPerInch), LLInfo);
  LogDatei.log('nform.PixelsPerInch: ' + IntToStr(nform.PixelsPerInch), LLInfo);
  //LogDatei.log('nform.DesignTimePPI: ' + nform.DesignTimePPI.ToString, LLInfo);
  LogDatei.log('designPPI: ' + IntToStr(designPPI), LLInfo);

  LogDatei.log('Loading Skin config from: ' + ininame, LLInfo);
  myini := TIniFile.Create(ininame);
  navlist.AddStrings(fillnavlist(myIni));
  buttoncounter := -1;
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

procedure logmouseenter(Sender: TObject);
var
  a, b: TPoint;
  str: string;
begin
  str := TComboButton(Sender).panel.Name + ' : ' +
    IntToStr(TComboButton(Sender).btn.Tag);
  a.X := Mouse.CursorPos.X;
  a.Y := Mouse.CursorPos.Y;
  ScreenToClient(nform.Handle, a);
  b := a;
  // b.x and b.y are your coordinates

  LogDatei.log(str + ' mouseenter: x: ' + IntToStr(b.x) + ' Y: ' +
    IntToStr(b.y), LLdebug);
end;

procedure logmouseleave(Sender: TObject);
var
  a, b: TPoint;
  str: string;
begin
  str := TComboButton(Sender).Name + ' : ' + IntToStr(TComboButton(Sender).btn.Tag);
  a.X := Mouse.CursorPos.X;
  a.Y := Mouse.CursorPos.Y;
  ScreenToClient(nform.Handle, a);
  b := a;
  // b.x and b.y are your coordinates

  LogDatei.log(str + ' mouseleave: x: ' + IntToStr(b.x) + ' Y: ' +
    IntToStr(b.y), LLdebug);
end;



begin
  labelcounter := 0;
  buttoncounter := 0;
  memocounter := 0;
  navlist := TStringList.Create;
  labellist := TStringList.Create;
  //buttonlist := TStringList.Create;
  //rangelist := TStringList.Create;
  myRangelistList := TMyRangelist.Create(True);
  sectionlist := TStringList.Create;
  memolist := TStringList.Create;
  designPPI := 96;
  Randomize;
  (*
  mymouseenter.Code := @onmouseenter;
  mymouseenter.Data := nil;
  mymouseleave.Code := @onmouseleave;
  mymouseleave.Data := nil;
  *)
end.
