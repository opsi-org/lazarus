unit osViewControl;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TSenderID = (seInstScriptdoInfo);

  TSetMessageText = procedure(MessageText: string; SenderID: TSenderID) of object;
  TSetProgress = procedure(Progress: integer; SenderID: TSenderID) of object;
  TSetHorizontalTextAlignment = procedure(HorizontalTextAlignment:TAlignment; SenderID: TSenderID) of object;

  { TOSViewControl }

  TOSViewControl = class(TObject)
    FOnProgress: TSetProgress;
    FOnMessageText: TSetMessageText;
    FOnHorizontalTextAlignment: TSetHorizontalTextAlignment;
    procedure SetMessageText(MessageText: string; SenderID: TSenderID);
    procedure SetProgress(Progress: integer; SenderID: TSenderID);
    procedure SetHorizontalTextAlignment(HorizontalTextAlignment: TAlignment; SenderID: TSenderID);
  end;

implementation

{ TOSViewControl }

procedure TOSViewControl.SetMessageText(MessageText: string; SenderID: TSenderID);
begin
  if Assigned(FOnMessageText) then FOnMessageText(MessageText, SenderID);
end;

procedure TOSViewControl.SetProgress(Progress: integer; SenderID: TSenderID);
begin
  if Assigned(FOnProgress) then FOnProgress(Progress, SenderID);
end;

procedure TOSViewControl.SetHorizontalTextAlignment(HorizontalTextAlignment:TAlignment; SenderID:TSenderID);
begin
  if Assigned(FOnHorizontalTextAlignment) then FOnHorizontalTextAlignment(HorizontalTextAlignment, SenderID);
end;

end.

