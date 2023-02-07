unit CustomMessageBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  FormAppearanceFunctions;

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
    Read the result code of the clicked button from the property ExitCode.
    The result code of a button is the index of the button in the button list which
    is given to the procedure ShowBox (starting with 0).
    If no button is clicked and the countdown finished, then ExitCode is -1.
    If the user closed the form, then ExitCode is -2.
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

procedure Decrease(var TimePart: string);
var
  CalculatedTime: integer;
begin
  CalculatedTime := StrToInt(Copy(TimePart, 1, 1)) * 10 + StrToInt(Copy(TimePart, 2, 1));
  Dec(CalculatedTime);
  TimePart := CalculatedTime.ToString;
  if CalculatedTime < 10 then TimePart := '0' + TimePart;
end;

// Count FTimeout one second down, the time format (of FTimeout) is hh:mm:ss
procedure TCountdownThread.CountOneSecondDown;
var
  Hours: string;
  Minutes: string;
  Seconds: string;
begin
  Hours := Copy(FTimeout, 1, 2);
  Minutes := Copy(FTimeout, 4, 2);
  Seconds := Copy(FTimeout, 7, 2);

  if Seconds = '00' then
  begin
    Seconds := '59';
    if Minutes = '00' then
    begin
      Minutes := '59';
      Decrease(Hours);
    end
    else
    begin
      Decrease(Minutes);
    end;
  end
  else
  begin
    Decrease(Seconds);
  end;

  FTimeout := Hours + ':' + Minutes + ':' + Seconds;

  Sleep(1000);
end;

function TCountdownThread.IsButtonClicked: boolean;
begin
  Result := Terminated; // click on a button stops the countdown
end;

procedure TCountdownThread.Execute;
begin
  while (not IsButtonClicked) and (FTimeout <> '00:00:00') do
  begin
    Synchronize(@UpdateCountdown);
    CountOneSecondDown;
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

procedure TCustomMessageForm.ShowBox(Title: string;
  Message: TStringList; Buttons: TStringList; TimeoutMessage: string; Timeout: string);
begin
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
