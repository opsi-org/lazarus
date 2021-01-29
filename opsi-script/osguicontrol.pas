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

  TosGUIControl = class(TForm)
  public
    procedure LoadSkin(const SkinDirectory: string);virtual;abstract;
    procedure SetMessageText(MessageText: string; MessageID: TMessageID); virtual; abstract;
    procedure SetProgress(Progress: integer; ProgressValueID: TProgressValueID); virtual; abstract;
    procedure SetForceStayOnTop(ForceStayOnTop: boolean); virtual; abstract;
    procedure SetBatchWindowMode(BatchWindowMode:TBatchWindowMode); virtual; abstract;
    procedure SetElementVisible(Visible:boolean; ElementID:TElementID); virtual; abstract;
    procedure SetElementEnabled(Enabled: boolean; ElementID:TElementID); virtual; abstract;
    procedure SetElementTop(Top: integer; ElementID:TElementID); virtual; abstract;
    procedure SetElementLeft(Left: integer; ElementID:TElementID); virtual; abstract;
    procedure SetWindowPosition(Position:TPosition); virtual; abstract;
    function SetPicture(const BitmapFile: string; const theLabel: string): boolean; virtual; abstract;
    procedure SetTracingLevel(const Level: integer);virtual; abstract;
    //procedure SetHorizontalTextAlignment(HorizontalTextAlignment: TAlignment; ElementID: TElementID);virtual;abstract;
  end;

var
  FBatchOberflaeche: TosGUIControl;//TFBatchOberflaeche;

implementation

end.

