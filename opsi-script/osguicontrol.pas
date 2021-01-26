unit osGUIControl;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms;

type
   TSenderID = (seInstScriptdoInfo, seFormCreate);

   TosGUIControl = class(TForm)
   public
    procedure SetMessageText(MessageText: string; SenderID: TSenderID);virtual;abstract;
    procedure SetProgress(Progress: integer; SenderID: TSenderID);virtual;abstract;
    procedure SetForceStayOnTop(ForceStayOnTop: boolean; SenderID: TSenderID);virtual;abstract;
    //procedure SetHorizontalTextAlignment(HorizontalTextAlignment: TAlignment; SenderID: TSenderID);virtual;abstract;
   end;

implementation

end.

