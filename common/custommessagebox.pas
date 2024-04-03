unit CustomMessageBox;

(*
This unit contains the class TCustomMessageForm. TCustomMessageForm implements a CustomMessageBox which shows a form with a message, up to three buttons, a countdown and an information about the countdown. The form's title is also editable.
Use the procedure TCustomMessageForm.ShowBox(...) to display the form.

The time format for the timeout is hh:mm:ss. If the given timeout is 00:00:00 then no countdown is shown.
The countdown is implemented within a thread for not blocking the form while it runs down.

The form closes automatically if a button is pressed or if the countdown expires.
After showing the form, read the property ExitCode to get the action by which the form was closed:
The ExitCode of a button is the index of the button in the given button list (starting with 0).
If no button is clicked and the countdown finished, then ExitCode is -1.
If the user closed the form, then ExitCode is -2.

Use the TCustomMessageForm similar to the following example:

CustomMessageForm := TCustomMessageForm.Create(nil);
CustomMessageForm.ShowBox("Form Title", MessageLines, ButtonCaptions, "This window closes in", "00:01:00");
MessageBoxExitCode := CustomMessageForm.ExitCode;
if Assigned(CustomMessageForm) then FreeAndNil(CustomMessageForm);
*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  FormAppearanceFunctions,
  osregex,
  oslog;

type

  TCountdownThread = class(TThread)
  private
    FTimeoutMessage: string;
    FTimeout: string;
    procedure UpdateCountdown;
    procedure CountOneSecondDown;
    function IsButtonClicked: boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(TimeoutMessage: string; Timeout: string);
  end;

  { TCustomMessageForm }

  TCustomMessageForm = class(TForm)
    ButtonLeft: TButton;
    ButtonMiddle: TButton;
    ButtonRight: TButton;
    Countdown: TLabel;
    MessageMemo: TMemo;
    procedure ButtonLeftClick(Sender: TObject);
    procedure ButtonMiddleClick(Sender: TObject);
    procedure ButtonRightClick(Sender: TObject);
    procedure FormClose(Sender: TObject);
  private
    FNumberButtons: integer;
    FExitCode: string;
    procedure InitializeButtons;
    procedure DefineButtons(Buttons: TStringList);
    procedure InitializeCountdown(TimeoutMessage: string; Timeout: string);
    procedure StartCountDown(TimeoutMessage: string; Timeout: string);
    procedure SetSize(NumberMessageLines: integer);
  public
    (*
    Meaning of exit codes in property ExitCode:
       0..2 = index of the clicked button 
         -1 = coundown finished
         -2 = form was closed by user
    *)
    property ExitCode: string read FExitCode;
    procedure ShowBox(Title: string; Message: TStringList;
      Buttons: TStringList; TimeoutMessage: string; Timeout: string);
  end;

var
  CustomMessageForm: TCustomMessageForm;
  CountdownThread: TCountdownThread;

implementation

{$R *.lfm}

{ TCountdownThread }

constructor TCountdownThread.Create(TimeoutMessage: string; Timeout: string);
begin
  inherited Create(True);
  FTimeout := Timeout;
  FTimeoutMessage := TimeoutMessage;
end;

// This method is executed by the mainthread and can therefore access all GUI elements.
procedure TCountdownThread.UpdateCountdown;
begin
  CustomMessageForm.Countdown.Caption := FTimeoutMessage + #10 + FTimeout;
  // center horizontal position of countdown label on form
  CustomMessageForm.Countdown.Left := Round((CustomMessageForm.Width - CustomMessageForm.Countdown.Width)/2);
end;

// Count FTimeout one second down, the time format (of FTimeout) is hh:mm:ss
procedure TCountdownThread.CountOneSecondDown;
var
  TimeStamp: TTimeStamp;
begin
  TimeStamp := DateTimeToTimeStamp(StrToTime(FTimeout));
  Dec(TimeStamp.Time, 1000);
  FTimeout := TimeToStr(TimeStampToDateTime(TimeStamp));
end;

function TCountdownThread.IsButtonClicked: boolean;
begin
  Result := Terminated; // click on a button stops the countdown
end;

procedure TCountdownThread.Execute;
begin
  Synchronize(@UpdateCountdown); // show initial value
  while (not IsButtonClicked) and (FTimeout <> '00:00:00') do
  begin
    Sleep(1000);
    CountOneSecondDown;
    Synchronize(@UpdateCountdown);
  end;
  if not IsButtonClicked then
  begin
    CustomMessageForm.Close;
    CustomMessageForm.FExitCode := '-1';
  end;
end;


{ TCustomMessageForm }

procedure TCustomMessageForm.ButtonLeftClick(Sender: TObject);
begin
  (*
  The left button is shown when two or three buttons are given.
  In these cases the left button always represents the first button from the button list (index 0).
  *)
  FExitCode := '0';
  Close;
end;

procedure TCustomMessageForm.ButtonMiddleClick(Sender: TObject);
begin
  (*
  The middle button is only shown when three buttons are given.
  In this case the middle button represents the second button from the button list (index 1).
  *)
  FExitCode := '1';
  Close;
end;

procedure TCustomMessageForm.ButtonRightClick(Sender: TObject);
begin
  // The right button is always shown when the button list is not empty.
  case FNumberButtons of
    1: FExitCode := '0'; // If only one button is shown, then it is the right one (index 0).
    2: FExitCode := '1'; // If two buttons are shown, then the left (index 0) and the right (index 1).
    3: FExitCode := '2'; // All three buttons are shown (left is 0, middle is 1, right is 2)
  end;
  Close;
end;

procedure TCustomMessageForm.FormClose(Sender: TObject);
begin
  // Since the countdown thread accesses GUI elemnts we have to be sure to stop the countdown before closing the form.
  if Assigned(CountdownThread) then
  begin
    CountdownThread.Terminate;
    FreeAndNil(CountdownThread);
  end;
  inherited Close;
end;

procedure TCustomMessageForm.InitializeButtons;
begin
  ButtonLeft.Visible := False;
  ButtonMiddle.Visible := False;
  ButtonRight.Visible := False;
  ButtonLeft.Caption := '';
  ButtonMiddle.Caption := '';
  ButtonRight.Caption := '';
end;

procedure TCustomMessageForm.DefineButtons(Buttons: TStringList);
begin
  InitializeButtons;

  (*
  The number of buttons in the button list defines which buttons are shown:
  0 buttons -> none shown
  1 button -> right one
  2 buttons -> left and right
  3 buttons -> all (left, middle, right)
  *)
  FNumberButtons := Buttons.Count;
  if FNumberButtons >= 1 then
  begin
    ButtonRight.Visible := True;
    ButtonRight.Caption := Buttons[FNumberButtons - 1];
  end;
  if FNumberButtons >= 2 then
  begin
    ButtonLeft.Visible := True;
    ButtonLeft.Caption := Buttons[0];
  end;
  if FNumberButtons = 3 then
  begin
    ButtonMiddle.Visible := True;
    ButtonMiddle.Caption := Buttons[1];
  end;
end;

procedure TCustomMessageForm.InitializeCountdown(TimeoutMessage: string; Timeout: string);
begin
  Countdown.Caption := TimeoutMessage;

  if Timeout <> '00:00:00' then
    Countdown.Caption := Countdown.Caption + #10 + Timeout;

  Countdown.Left := Round((self.Width - Countdown.Width)/2);
end;

procedure TCustomMessageForm.StartCountDown(TimeoutMessage: string; Timeout: string);
begin
  CountdownThread := TCountdownThread.Create(TimeoutMessage, Timeout);
  CountdownThread.Start;
end;

procedure TCustomMessageForm.SetSize(NumberMessageLines: integer);
var
  MessageMemoHeight: integer;
  RequiredBoxWidth: integer;
begin
  Width := 500;

  // If the buttons are so wide that they would overlap, increase the form width:
  Show;
  Visible := False;
  RequiredBoxWidth := 2 * ButtonLeft.Left + (ButtonLeft.Width +
    ButtonMiddle.Width + ButtonRight.Width) + 10;
  if RequiredBoxWidth > self.Width then
  begin
    Width := RequiredBoxWidth;
    Show;
    Visible := False;
  end;
  // Always position the middle button in the middle between the other two buttons:
  ButtonMiddle.Left := Round((ButtonRight.Left + (ButtonLeft.Left +
      ButtonLeft.Width) - ButtonMiddle.Width) / 2);

  (*
  For a nice presentation of the message:
  - The message memo has a base hight and grows a bit with the number of message lines.
  - For messages with many lines the memo has scroll bars so that it won't get too big.
  *)
  if NumberMessageLines < 20 then
    MessageMemoHeight := 50 + 15 * NumberMessageLines
  else
    MessageMemoHeight := 350;

  (*
  Set required form hight depending on the components sizes and distances:
  Space between form bottom and buttons = 15
  If buttons are shown, then ButtonRight is always visible and we have to take the button hight into account.
  Space between buttons and countdown label = 15
  Space between countdown label and message Memo = 15
  *)
  Height := 15 + ButtonRight.Height + 15 + Countdown.Height + 15 + MessageMemoHeight;

  CenterFormOnScreen(self);
end;

// This message box can only hold up to three buttons
procedure CheckNumberOfButtons(Buttons: TStringList);
begin
  if Buttons.Count > 3 then
  begin
    LogDatei.log('You gave ' + Buttons.Count.ToString +
      ' buttons to the message box but it can only hold up to 3 buttons! ' +
      'Therefore we will use the first three buttons and ignore the rest.', LLWarning);
    // delete unused elements from list to avoid accidental access
    while Buttons.Count > 3 do
      Buttons.Delete(Buttons.Count - 1);
  end;
end;


// Seconds will be converted to hh:mm:ss
function ConvertToTimeFormatIfSeconds(Timeout: String): String;
const
  SecPerHour = 3600;
  SecPerMinute = 60;
var ms, ss, mm, hh: Cardinal;
  Seconds: Cardinal;
  newTimeout: string;
begin
  // check if Timeout is given as hh:mm:ss or as seconds
  if Pos(':',Timeout) = 0 then
  begin
    // no ':' found => assume Timeout is given in seconds, convert to hh:mm:ss
    // see https://www.swissdelphicenter.ch/en/showcode.php?id=1163
    try
      Seconds := StrToInt(Timeout);
      hh :=  Seconds div SecPerHour;                    // hours
      mm := (Seconds mod SecPerHour) div SecPerMinute;  // minutes
      ss := (Seconds mod SecPerHour) mod SecPerMinute;  // seconds
      ms := 0;                                          // milliseconds

      if hh > 23 then
      begin
        newTimeout := '23:59:59';
        LogDatei.log('maximum timeout is ' + newTimeout,LLWarning);
      end
      else
      begin
        newTimeout := FormatDateTime('hh:mm:ss', EncodeTime(hh, mm, ss, ms));
      end;
      Timeout := newTimeout;
    except
       LogDatei.log('converting MessageBox timeout from seconds "' + Timeout +
            '" to hh:mm:ss failed.',LLWarning)
    end;
  end;
  Result := Timeout;
end;


// The timeout must have the format hh:mm:ss
procedure CheckTimeFormat(var Timeout: string);
begin
  if not ((Timeout.Length = 'hh:mm:ss'.Length) and isRegexMatch(Timeout,
    '(0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]')) then
  begin
    LogDatei.log('The string "' + Timeout +
      '" does not match the required time format hh:mm:ss (max 23:59:59) for the timeout! ' +
      'Therefore we will show the message box without timeout.',
      LLError);
    Timeout := '00:00:00';
  end;
end;


procedure TCustomMessageForm.ShowBox(Title: string;
  Message: TStringList; Buttons: TStringList; TimeoutMessage: string; Timeout: string);
begin
  CheckNumberOfButtons(Buttons);
  Timeout := ConvertToTimeFormatIfSeconds(Timeout);
  CheckTimeFormat(Timeout);

  CenterFormOnScreen(self);

  self.Caption := Title;
  MessageMemo.Lines.Assign(Message);
  DefineButtons(Buttons);

  self.SetSize(Message.Count);

  FExitCode := '-2'; // default exit code (= exit code if user closes the form)

  InitializeCountdown(TimeoutMessage, Timeout);
  if Timeout <> '00:00:00' then StartCountDown(TimeoutMessage, Timeout);

  ShowModal;
end;

end.
