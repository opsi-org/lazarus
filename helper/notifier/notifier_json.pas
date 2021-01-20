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
  msgseparator1: string;
  msgseparator2: string;
begin
  try
    { separate the answer stings in rpc call }
    msgseparator1 := #13#10;  // CRLN
    msgseparator2 := #30;  // 30 = 001E = record separator
    { pass JSON answer to notifier_base unit. Will be there handled by messageFromMainThread  }
    { create answer string 1 }
    myJsonCall := '{"params": ["choice", [' + IntToStr(buttonindex) +
      ']], "id": null, ' + '"method": "setSelectedIndexes"}' +
      msgseparator1 + '{"params": ["choice"], "id": null, "method": "selectChoice"}' +
      msgseparator1;
    { push answer string in tcp write buffer }
    notifier_base.myJsonAnswer := myJsonCall;
    logdatei.log('JSON for Button call1: ' + myJsonCall, LLDebug2);
    logdatei.log('JSON for Button clicked: choice: ' + IntToStr(buttonindex), LLInfo);
  except
    on E: Exception do
      logdatei.log('exception in buttonPushedToService :' + E.Message, LLError);
  end;
end;


procedure newMessageFromService(message: string);
var
  messagearray, aktId, aktMessage, aktMethod: string;
  messagelist: TStringList;
  i, choicecounter: integer;
  //  indexarray, indexstr: string;
  choicearray, choicestr: string;
  //nkind: string = '';
  labelMatch: boolean;
begin
  try
    // a flag if we found a matching labe in this message
    labelMatch := False;
    logdatei.log('Got Message ' + message, LLInfo);

    // should be a json object
    if not jsonIsObject(message) then
      logdatei.log('Error: is not a JSON Object', LLError);

    // there should be a method
    if not jsonAsObjectGetValueByKey(message, 'method', aktMethod) then
    begin
      logdatei.log('Error: No method found', LLCritical);
      DataModule1.DataModuleDestroy(nil);
    end;
    logdatei.log('Got method: ' + aktMethod, LLDebug);


    if (lowercase(aktMethod) = lowerCase('messageChanged')) or
      (lowercase(aktMethod) = lowerCase('subjectsChanged')) or
      (lowercase(aktMethod) = lowerCase('endConnection')) or
      (lowercase(aktMethod) = lowerCase('choicesChanged')) or
      (lowercase(aktMethod) = lowerCase('selectedIndexesChanged')) then
    begin
      // wellknown methods
      logdatei.log('Is well known method: ' + aktMethod, LLDebug2);
      // should have key=params:[]
      if not jsonAsObjectGetValueByKey(message, 'params', messagearray) then
      begin
        logdatei.log('Error: params not found', LLCritical);
        DataModule1.DataModuleDestroy(nil);
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
          DataModule1.DataModuleDestroy(nil);
        end;
        logdatei.log('params now: ' + messagearray, LLDebug2);
      end;
      if lowercase(aktMethod) = lowerCase('endConnection') then
      begin
        //nkind := opsiunquotestr2(messagearray, '[]');
        //nkind := opsiunquotestr2(nkind, '""');
        // get inner array as stringlist
        messagelist := TStringList.Create;
        messagelist.CaseSensitive:= false;
        if jsonAsArrayCountElements(messagearray) > 0 then
        begin
          if not jsonAsArrayToStringList(messagearray, messagelist) then
          begin
            logdatei.log('Error: could not get messagelist', LLCritical);
            DataModule1.DataModuleDestroy(nil);
          end
          else
            logdatei.log('messagelist now: ' + messagelist.Text, LLDebug2);
        end
        else
          logdatei.log('Got empty messagelist.', LLDebug2);
      end
      else
      begin
        // get inner array as stringlist
        messagelist := TStringList.Create;
        messagelist.CaseSensitive:= false;
        if jsonAsArrayCountElements(messagearray) > 0 then
        begin
          if not jsonAsArrayToStringList(messagearray, messagelist) then
          begin
            logdatei.log('Error: could not get messagelist', LLCritical);
            DataModule1.DataModuleDestroy(nil);
          end
          else
            logdatei.log('messagelist now: ' + messagelist.Text, LLDebug2);
        end
        else
          logdatei.log('Got empty messagelist.', LLDebug2);
      end;

      // check methods
      if lowercase(aktMethod) = lowerCase('endConnection') then
      begin
        // method  endConnection
        // hideNForm
        logdatei.log('Got method endConnection for: ' + messagelist.Text, LLInfo);
        if (messagelist.IndexOf(mynotifierkind) > -1) or  // found
          (messagelist.Count = 0) then                    // empty
      (*  if (lowerCase(nkind) = lowerCase(mynotifierkind)) or
          ((mynotifierkind = 'event') and (nkind = '')) or
          ((mynotifierkind = 'shutdown') and (nkind = '')) then  *)
        begin
          // got end call for this notifier kind : hide form
          shutdownNotifier;
        end;
      end
      else  // other methods
      begin
        // iterate over messages
        logdatei.log('Number of messages is: ' + IntToStr(messagelist.Count), LLDebug2);
        for i := 0 to messagelist.Count - 1 do
        begin
          try
            logdatei.log('Index of message is: ' + IntToStr(i), LLDebug2);
            if not (jsonIsObject(messagelist.Strings[i]) and
              jsonAsObjectGetValueByKey(messagelist.Strings[i], 'id', aktId)) then
            begin
              if i = messagelist.Count - 1 then
                // The 'last' message is sometimes only a header
                logdatei.log('Warning: could not get id from :' + messagelist.Strings[i],
                  LLWarning)
              else
                logdatei.log('Error: could not get id from :' + messagelist.Strings[i],
                  LLError);
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
                  logdatei.log('Error: could not get choicearray for id: ' +
                    aktId, LLError);
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
                  begin
                    labelMatch := True;
                  end;
                  logdatei.log('after setLabelCaptionById.', LLDebug2);
                end
                else
                  logdatei.log('Error: could not get message for id: ' + aktId, LLError);
              end; //button or label
            end;  // id found
          except
            logdatei.log('Exception in parsing :' + messagelist.Strings[i], LLError)
          end;
          logdatei.log('Finished with index of message: ' + IntToStr(i), LLDebug2);
        end; // iterate over messages
      end;  // endConnection or other methods
      logdatei.log('finished newMessageFromService.', LLDebug2);
    end   // known methods
    else
    begin
      logdatei.log('Error: unknown method: ' + aktMethod, LLWarning);
      //DataModule1.DataModuleDestroy(nil);
    end;
    if Assigned(messagelist) then
      FreeAndNil(messagelist);
  except
    on E: Exception do
      logdatei.log('exception in newMessageFromService :' + E.Message, LLError);
  end;
end;

end.
