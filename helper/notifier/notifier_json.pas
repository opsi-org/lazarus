unit notifier_json;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  osjson,
  notifierguicontrol,
  oslog,
  strutils;

procedure newMessageFromService(message: string);
//function newMessageToService(message: string) : boolean;
procedure buttonPushedToService(buttonindex: integer);

implementation

uses
  notifier_base,
  notifierdatamodule;

var
  globalchoicearray: string;

function opsiunquotestr2(s1, s2: string): string;
  // removes only quotes if they found at start and end
  // s2 may be two chars long. Then the first char is the start mark
  // and the second char is the end mark
  // used by unquote2
var
  markstr, startmark, endmark: string;
begin
  Result := '';
  markstr := trim(s2);
  if (length(s1) >= 1) and (length(markstr) >= 1) then
  begin
    startmark := markstr[1];
    if length(markstr) >= 2 then
      endmark := markstr[2] // different marks (brackets) at begin and end
    else
      endmark := startmark; // the same mark (quote) at begin and end
    if (pos(startmark, s1) = 1) and AnsiEndsStr(endmark, s1) then
      Result := copy(s1, 2, length(s1) - 2)
    else
      Result := s1;
  end;
end;



procedure buttonPushedToService(buttonindex: integer);
var
  myJsonCall: string;
begin
  // pass JSON answer to notifier_base unit. Will be there handled by messageFromMainThread
  // create answer string 1
  myJsonCall := '{"params": ["choice", [' + IntToStr(buttonindex) +
    ']], "id": null, ' + '"method": "setSelectedIndexes"}' + #13#10 +
    '{"params": ["choice"], "id": null, "method": "selectChoice"}';
  // push answer string in tcp write buffer
  notifier_base.myJsonAnswer := myJsonCall;
  logdatei.log('JSON for Button call1: ' + myJsonCall, LLDebug2);
  logdatei.log('JSON for Button clicked: choice: ' + IntToStr(buttonindex), LLInfo);
end;


procedure newMessageFromService(message: string);
var
  messagearray, aktId, aktMessage, aktMethod: string;
  messagelist: TStringList;
  i, choicecounter: integer;
  indexarray, indexstr: string;
  choicearray, choicestr: string;
  nkind: string = '';
  labelMatch: boolean;
begin
  // a flag if we found a matching labe in this message
  labelMatch := False;
  logdatei.log('Got Message ' + message, LLDebug2);

  // should be a json object
  if not jsonIsObject(message) then
    logdatei.log('Error: is not a JSON Object', LLError);

  // there should be a method
  if not jsonAsObjectGetValueByKey(message, 'method', aktMethod) then
  begin
    logdatei.log('Error: No method found', LLCritical);
    DataModule1.Destroy;
  end;
  logdatei.log('Got method: ' + aktMethod, LLDebug);


  if (lowercase(aktMethod) = lowerCase('messageChanged')) or
    (lowercase(aktMethod) = lowerCase('subjectsChanged')) or
    (lowercase(aktMethod) = lowerCase('endConnection')) or
    (lowercase(aktMethod) = lowerCase('selectedIndexesChanged')) then
  begin
    // wellknown methods
    logdatei.log('Is well known method: ' + aktMethod, LLDebug2);
    // should have key=params:[]
    if not jsonAsObjectGetValueByKey(message, 'params', messagearray) then
    begin
      logdatei.log('Error: params not found', LLCritical);
      DataModule1.Destroy;
    end;
    logdatei.log('params now: ' + messagearray, LLDebug2);
    if (lowercase(aktMethod) = lowerCase('subjectsChanged')) or
      (lowercase(aktMethod) = lowerCase('endConnection')) then
    begin
      // should have key=params:[[]]
      // get inner array (only on subjectsChanged or endConnection)
      if not jsonAsArrayGetElementByIndex(messagearray, 0, messagearray) then
      begin
        logdatei.log('Error: inner array not found', LLCritical);
        DataModule1.Destroy;
      end;
      logdatei.log('params now: ' + messagearray, LLDebug2);
    end;
    if lowercase(aktMethod) = lowerCase('endConnection') then
    begin
      nkind := opsiunquotestr2(messagearray, '[]');
      nkind := opsiunquotestr2(nkind, '""');
    end
    else
    begin
      // get inner array as stringlist
      messagelist := TStringList.Create;
      if not jsonAsArrayToStringList(messagearray, messagelist) then
      begin
        logdatei.log('Error: could not get messagelist', LLCritical);
        DataModule1.Destroy;
      end;
      logdatei.log('params[0] now: ' + messagelist.Strings[0], LLDebug2);
    end;

    // check methods
    if lowercase(aktMethod) = lowerCase('endConnection') then
    begin
      // method  endConnection
      // hideNForm
      logdatei.log('Got method endConnection for: ' + nkind, LLDebug);
      if (lowerCase(nkind) = lowerCase(mynotifierkind)) or
        ((mynotifierkind = 'event') and (nkind = '')) then
      begin
        // got end call for this notifier kind : hide form
        hideNForm;
        //shutdownNotifier;
      end;
    end
    else  // other methods
    begin
      // iterate over messages
      for i := 0 to messagelist.Count - 1 do
      begin

        if not jsonAsObjectGetValueByKey(messagelist.Strings[i], 'id', aktId) then
        begin
          logdatei.log('Error: could not get id', LLError);
        end
        else
        begin
          if lowercase(aktId) = 'choice' then
          begin
            //if labelMatch then
            //begin
              //button
              if jsonAsObjectGetValueByKey(messagelist.Strings[i],
                'choices', choicearray) then
              begin
                globalchoicearray := choicearray;
                for choicecounter := 0 to jsonAsArrayCountElements(choicearray) - 1 do
                begin
                  if not jsonAsArrayGetElementByIndex(choicearray,
                    choicecounter, choicestr) then
                    logdatei.log('Error: choicearray not found', LLError)
                  else
                    setButtonCaptionById(choicecounter, choicestr);
                end;
              end
              else
                logdatei.log('Error: could not get choicearray for id: ' + aktId, LLError);
           // end
           // else
           //   logdatei.log('Warning: Button caption not written because no known labe in the message.',
           //     LLWarning);
          end
          else
          begin
            //label
            if jsonAsObjectGetValueByKey(messagelist.Strings[i],
              'message', aktMessage) then
            begin
              if setLabelCaptionById(aktId, aktMessage) then
                labelMatch := True;
            end
            else
              logdatei.log('Error: could not get message for id: ' + aktId, LLError);
          end; //button or label
        end;  // id found
      end; // iterate over messages
    end;  // endConnection or other methods
  end   // known methods
  else
  begin
    logdatei.log('Error: unkonwn method: ' + aktMethod, LLCritical);
    DataModule1.Destroy;
  end;

end;

end.
