unit notifier_json;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  osjson,
  notifierguicontrol,
  oslog;

procedure newMessageFromService(message: string);
function newMessageToService(message: string) : boolean;
procedure buttonPushedToService(buttonindex: integer);

implementation

uses
  notifier_base,
  notifierdatamodule;

var
  globalchoicearray: string;


function newMessageToService(message: string) : boolean;
begin
  result := false;
  if notifier_base.myJsonAnswer = '' then
  begin
    notifier_base.myJsonAnswer := myJsonCall;
    result := true;
  end;
end;

procedure buttonPushedToService(buttonindex: integer);
var
  myJsonCall: string;
begin
  // pass JSON answer to notifier_base unit. Will be there handled by messageFromMainThread
  result := false;
  // create answer string 1
  myJsonCall := '{"params": ["choice": ['+ IntToStr(buttonindex)+']], "id": null,' +
    '"method": "setSelectedIndexes"}';
  // push answer string in tcp write buffer
  if notifier_base.myJsonAnswer = '' then
  begin
    if newMessageToService(myJsonCall) then
    begin
    end;
  end;
  logdatei.log('JSON for Button call1: ' + myJsonCall, LLDebug2);
  // create answer string 2
  myJsonCall := '{"params": ["choice"] , "id": null, "method": "selectChoice"}';
  // push answer string in tcp write buffer
  // push answer string in tcp write buffer
  if notifier_base.myJsonAnswer = '' then
  begin
    if newMessageToService(myJsonCall) then
    begin
    end;
  end;
  logdatei.log('JSON for Button call2: ' + myJsonCall, LLDebug2);
  logdatei.log('JSON for Button clicked: choice: ' + IntToStr(buttonindex), LLInfo);
end;

(*
procedure buttonPushedToService(buttonindex: integer);
var
  myJsonCall: string;
begin
  myJsonCall := '{"params": [{"severity": 0, "title": "", "id": "choice",' +
    '"selectedIndexes": [' + IntToStr(buttonindex) + '],' +
    '"message": "", "choices": ' + globalchoicearray + ',' +
    '"type": "", "class": "ChoiceSubject"}, [0]], "id": null,' +
    '"method": "selectedIndexesChanged"}';
  // pass JSON answer to notifier_base unit. Will be there handled by messageFromMainThread
  notifier_base.myJsonAnswer := myJsonCall;
  logdatei.log('JSON for Button clicked: choice: ' + IntToStr(buttonindex), LLInfo);
end;
*)

procedure newMessageFromService(message: string);
var
  messagearray, aktId, aktMessage, aktMethod: string;
  messagelist: TStringList;
  i, choicecounter: integer;
  indexarray, indexstr: string;
  choicearray, choicestr: string;
begin
  logdatei.log('Got Message ' + message, LLDebug2);

  // should be a json object
  if not jsonIsObject(message) then
    logdatei.log('Error: is not a JSON Object', LLError);

  // there should be a method
  if not jsonAsObjectGetValueByKey(message, 'method', aktMethod) then
    logdatei.log('Error: No method found', LLError);

  // method  endConnection
  if lowercase(aktMethod) = lowerCase('endConnection') then
  begin
    // stop, hideNForm and terminate
    //shutdownNotifier;
  end;

  if (lowercase(aktMethod) = lowerCase('messageChanged')) or
    (lowercase(aktMethod) = lowerCase('subjectsChanged')) then
  begin
    // should have key=params:[[]]
    if not jsonAsObjectGetValueByKey(message, 'params', messagearray) then
    begin
      logdatei.log('Error: params not found', LLError);
    end;
    if lowercase(aktMethod) = lowerCase('subjectsChanged') then
    begin
      // get inner array (only on subjectsChanged)
      if not jsonAsArrayGetElementByIndex(messagearray, 0, messagearray) then
      begin
        logdatei.log('Error: inner array not found', LLError);
      end;
    end;
    // get inner array as stringlist
    messagelist := TStringList.Create;
    if not jsonAsArrayToStringList(messagearray, messagelist) then
    begin
      //critical
      logdatei.log('Error: could not get messagelist', LLError);
    end;
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
          //button
          (*
          if jsonAsObjectGetValueByKey(messagelist.Strings[i], 'selectedIndexes', indexarray) then
          begin
            if not jsonAsArrayGetElementByIndex(indexarray, 0, indexstr) then
               logdatei.log('Error: indexarray not found', LLError);
          end
          else logdatei.log('Error: could not get indexarray for id: '+aktId, LLError);
            *)
          if jsonAsObjectGetValueByKey(messagelist.Strings[i],
            'choices', choicearray) then
          begin
            globalchoicearray := choicearray;
            for choicecounter := 0 to jsonAsArrayCountElements(choicearray) - 1 do
            begin
              if not jsonAsArrayGetElementByIndex(choicearray, choicecounter,
                choicestr) then
                logdatei.log('Error: choicearray not found', LLError)
              else
                setButtonCaptionById(choicecounter, choicestr);
            end;
          end
          else
            logdatei.log('Error: could not get choicearray for id: ' + aktId, LLError);

        end
        else
        begin
          //label
          if jsonAsObjectGetValueByKey(messagelist.Strings[i], 'message',
            aktMessage) then
          begin
            setLabelCaptionById(aktId, aktMessage);
          end
          else
            logdatei.log('Error: could not get message for id: ' + aktId, LLError);
        end;
      end;
    end;
  end
  else
  ; //unkonwn method
end;

end.
