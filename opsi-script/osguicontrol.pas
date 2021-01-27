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
  TMessageID = (mInfo, mDetail, mCommand, mProgress, mVersion, mProduct);
  TProgressValueID = (pInteger, pPercent);
  TElementID = (eMainForm, eLabelInfo, eLabelDetail, eLabelCommand,
    eLabelProgress, eLabelVersion, eLabelProduct, ePanel1, ePanel2, eImage1, eImage2);

  TosGUIControl = class(TForm)
  public
    procedure SetMessageText(MessageText: string; MessageID: TMessageID);virtual;abstract;
    procedure SetProgress(Progress: integer; ProgressValueID: TProgressValueID);virtual;abstract;
    procedure SetForceStayOnTop(ForceStayOnTop: boolean);virtual;abstract;
    procedure SetBatchWindowMode(BatchWindowMode:TBatchWindowMode);virtual;abstract;
    procedure SetElementVisible(Visible:boolean; ElementID:TElementID);virtual;abstract;
    //procedure SetHorizontalTextAlignment(HorizontalTextAlignment: TAlignment; SenderID: TSenderID);virtual;abstract;
  end;

var
  FBatchOberflaeche: TosGUIControl;//TFBatchOberflaeche;

implementation

end.

