unit osSimpleWinBatchGUI;

{$mode Delphi}{$H+}
{$RANGECHECKS ON}
{$OVERFLOWCHECKS ON}
{$IOCHECKS ON}
{$OBJECTCHECKS ON}
{$VARSTRINGCHECKS ON}
{$LONGSTRINGS ON}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, osGUIControl, INIFiles;

type

  { TSimpleWinBatchGUI }

  TSimpleWinBatchGUI = class(TGUIControl)
    LabelVersion: TLabel;
    LabelInfo: TLabel;
    LabelActivity: TLabel;
    PanelCenter: TPanel;
    TimerActivity: TTimer;
    procedure TimerActivityTimer(Sender: TObject);
  private

  public
    procedure LoadSkin(const SkinDirectory: string); override;
    procedure SetMessageText(MessageText: string; MessageID: TMessageID); override;
    procedure SetForceStayOnTop(ForceStayOnTop: boolean); override;
    procedure SetBatchWindowMode(BatchWindowMode:TBatchWindowMode); override;
    procedure SetElementVisible(Visible:boolean; ElementID:TElementID); override;
    procedure SetWindowPosition(Position:TPosition); override;
  end;

var
  SimpleWinBatchGUI: TSimpleWinBatchGUI;

implementation

{$R *.lfm}

{ TSimpleWinBatchGUI }


procedure TSimpleWinBatchGUI.TimerActivityTimer(Sender: TObject);
begin
  if LabelActivity.Caption = '. . . ' then
  begin
    LabelActivity.Caption:= '. ';
  end
  else
  begin
    LabelActivity.Caption := LabelActivity.Caption + '. ';
  end;
  Application.ProcessMessages;

  (*if (LabelActivity.Caption = '|') or (LabelActivity.Caption = '') then
  begin
    LabelActivity.Caption:= '/';
  end
  else
  if LabelActivity.Caption = '/' then
  begin
    LabelActivity.Caption:= '-';
  end
  else
  if LabelActivity.Caption = '-' then
  begin
    LabelActivity.Caption:= '\';
  end
  else
  if LabelActivity.Caption = '\' then
  begin
    LabelActivity.Caption:= '|';
  end;
  Application.ProcessMessages;*)
end;

procedure TSimpleWinBatchGUI.LoadSkin(const SkinDirectory: string);
var
  SkinFile: TIniFile;
  FilePath: string;
  //debug:string;
begin
  FilePath := GetSkinDirectory(SkinDirectory) + PathDelim + 'skin.ini';
  if FileExists(FilePath) then
  begin
    SkinFile := TIniFile.Create(FilePath);
    Color := StringToColor(SkinFile.ReadString('Form', 'Color' , 'clHotLight'));
    LabelInfo.Caption := SkinFile.ReadString('LabelInfo', 'Caption' , 'Software wird installiert. Bitte warten.');
    LabelInfo.Repaint;
    //debug:= LabelInfo.Caption;
    SkinFile.Free;
  end
  else
  begin
    LabelInfo.Caption := 'Software wird installiert. Bitte warten';
    LabelInfo.Repaint;
  end;
  Application.ProcessMessages;
end;

procedure TSimpleWinBatchGUI.SetMessageText(MessageText: string;
  MessageID: TMessageID);
begin
  case MessageID of
    mVersion: LabelVersion.Caption := MessageText;
  end;
end;

procedure TSimpleWinBatchGUI.SetForceStayOnTop(ForceStayOnTop: boolean);
begin

end;

procedure TSimpleWinBatchGUI.SetBatchWindowMode(
  BatchWindowMode: TBatchWindowMode);
begin

end;

procedure TSimpleWinBatchGUI.SetElementVisible(Visible: boolean;
  ElementID: TElementID);
begin
  case ElementID of
    eMainForm: self.Visible := Visible;
  end;
end;

procedure TSimpleWinBatchGUI.SetWindowPosition(Position: TPosition);
begin

end;



end.

