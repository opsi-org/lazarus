unit osGUIControl;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms;

type
  TBatchWindowMode = (bwmNotActivated, bwmIcon, bwmNormalWindow, bwmMaximized);

  //TSenderID might be useful in the future but not necessary now, just an idea to rember
  //TSenderID = (seBuildPC, seCentralFormFormCreate, seChangeProductStatusOnReinst, seInstScriptdoInfo);

  //Expand/include here further IDs if necessary
  TMessageID = (mInfo, mDetail, mCommand, mProgress, mVersion, mProduct, mActivity);
  TProgressValueID = (pInteger, pPercent);
  TElementID = (eMainForm, eLabelInfo, eLabelDetail, eLabelCommand,
    eLabelProgress, eLabelVersion, eLabelProduct, ePanel1, ePanel2, eImage1,
    eImage2, eLogo1, eLogo2, eProductImage1, eProductImage2, eActivityBar,
    eTimerProcessMess, eProgressBar);

  { TosGUIControl }

  { TGUIControl }

  TGUIControl = class(TForm)
  public
    procedure LoadSkin(const SkinDirectory: string);virtual;
    procedure SetMessageText(MessageText: string; MessageID: TMessageID); virtual;
    procedure SetProgress(Progress: integer; ProgressValueID: TProgressValueID); virtual;
    procedure SetForceStayOnTop(ForceStayOnTop: boolean); virtual;
    procedure SetBatchWindowMode(BatchWindowMode:TBatchWindowMode); virtual;
    procedure SetElementVisible(Visible:boolean; ElementID:TElementID); virtual;
    procedure SetElementEnabled(Enabled: boolean; ElementID:TElementID); virtual;
    procedure BringElementToFront(ElementID:TElementID); virtual;
    procedure SetElementTop(Top: integer; ElementID:TElementID); virtual;
    procedure SetElementLeft(Left: integer; ElementID:TElementID); virtual;
    procedure SetWindowPosition(Position:TPosition); virtual;
    procedure SetPicture(const BitmapFile: string; const theLabel: string); virtual;
    procedure SetTracingLevel(const Level: integer);virtual;
    //procedure SetHorizontalTextAlignment(HorizontalTextAlignment: TAlignment; ElementID: TElementID);virtual;abstract;
  end;

var
  FBatchOberflaeche: TGUIControl;//TFBatchOberflaeche;

implementation

{ TGUIControl }

procedure TGUIControl.LoadSkin(const SkinDirectory: string);
begin

end;

procedure TGUIControl.SetMessageText(MessageText: string; MessageID: TMessageID
  );
begin

end;

procedure TGUIControl.SetProgress(Progress: integer;
  ProgressValueID: TProgressValueID);
begin

end;

procedure TGUIControl.SetForceStayOnTop(ForceStayOnTop: boolean);
begin

end;

procedure TGUIControl.SetBatchWindowMode(BatchWindowMode: TBatchWindowMode);
begin

end;

procedure TGUIControl.SetElementVisible(Visible: boolean; ElementID: TElementID
  );
begin

end;

procedure TGUIControl.SetElementEnabled(Enabled: boolean; ElementID: TElementID
  );
begin

end;

procedure TGUIControl.BringElementToFront(ElementID: TElementID);
begin

end;

procedure TGUIControl.SetElementTop(Top: integer; ElementID: TElementID);
begin

end;

procedure TGUIControl.SetElementLeft(Left: integer; ElementID: TElementID);
begin

end;

procedure TGUIControl.SetWindowPosition(Position: TPosition);
begin

end;

procedure TGUIControl.SetPicture(const BitmapFile: string; const theLabel: string);
begin

end;

procedure TGUIControl.SetTracingLevel(const Level: integer);
begin

end;

end.

