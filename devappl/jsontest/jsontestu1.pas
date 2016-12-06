unit jsontestu1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, osjson;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

function SetValueAndDisplay (myMemo: TMemo; str: String; key:String; value:String) : String;
var stringIsSet, datatypeString, valuestr: String;
begin
  valuestr:='';
  result:='';
  myMemo.Append('');
  myMemo.Append('Set ' + key + ' to ' + value);
  stringIsSet := '';
  if jsonAsObjectGetValueByKey(str,key,valuestr) then
  begin
    myMemo.Append(key + ' value before setting: '+ valuestr);
    datatypeString := jsonISOElementGetType(valuestr);
    myMemo.append(key + ' datatype before setting: '+ datatypeString);
    if  not (jsonAsObjectSetValueByKey(str, key, value, stringIsSet)) then
        result:=str; (* Fehlermeldung *)
  end
  else (* kein key / value *)
    begin
      myMemo.Append(key + ' no key/value before setting: ');
      jsonAsObjectAddKeyAndValue(str, key, value, stringIsSet);
    end;

  if ( not(stringIsSet='') and jsonIsValid(stringIsSet)) then
    begin
      if jsonAsObjectGetValueByKey (stringIsSet, key, valuestr) then
      begin
        myMemo.Append(key + ' value after setting: '+ valuestr);
        datatypeString := jsonISOElementGetType(valuestr);
        myMemo.append(key + ' datatype after setting st: '+ datatypeString);
        myMemo.append(key + ' datatype after setting jt: '+ jsonTJElementGetType(valuestr));
        result := stringIsSet
      end
      else myMemo.Append(key + ' no key/value after setting: ');
    end
    else begin
      result :=str; // return original string
      myMemo.append('error SetValueAndDisplay');
    end;

end;

procedure TForm1.Button1Click(Sender: TObject);
var
  mystr, strresult, valuestr, elementstr, displayString, datatypeString : string;
  i,j,k : integer;
  mylist, mylist2 : TStringlist;
  jsonArray : string;
begin
  Memo2.Clear;
  mystr := memo1.Text;
  strresult:='';
  if not jsonIsValid(mystr) then Memo2.Append('String is NOT valid.')
  else
    begin
      if jsonGetValidString(mystr, strresult) then
         Memo2.Append('valid string: ' + jsonClearString(strresult))
      else
        Memo2.Append('Error jsonGetValidString');
      Memo2.Append('');
      displayString := 'String is valid';
      strresult:=  jsonClearString(strresult); // eliminate CR, LF, NL
      if jsonIsArray(strresult) then
      begin
        AppendStr(displayString, ' and is array. ');
        AppendStr(displayString,' Array has '+intToStr(jsonAsObjectCountElements(strresult))+' element(s).');
        Memo2.Append(displayString);

        for i := 0 to jsonAsArrayCountElements (strresult) -1 do
          if jsonAsArrayGetElementByIndex(strresult,i, elementstr) then
          begin
            Memo2.Append('Element '+inttostr(i)+' : ' + elementstr);
            Memo2.Append('DataType of Element: ' + jsonISOElementGetType(elementstr));
            // object: delete by key
            Memo2.Append('--------------------------------------------------');
            Memo2.Append('delete by key - clientId');
            if (jsonAsObjectDeleteByKey(elementstr,'clientId')) then
              Memo2.Append(elementstr);
            // end object: delete by key
            Memo2.Append('Element has '+intToStr(jsonAsObjectCountElements(elementstr))+' subelements');
            case jsonISOElementGetType(elementstr) of
              'String st' :
                begin
                  Memo2.Append('DataType of Element stString, nothing to do');
                end;
              'Object st' :
                begin
                  Memo2.Append('--------------------------------------------------------');
                  Memo2.Append('----- getting values by key ----------------------');
                  if jsonAsArrayToStringList(strresult, mylist) then
                     for j := 0 to mylist.Count-1 do
                     begin
                      Memo2.Append('opsi '+inttostr(j)+' -> '+ mylist.Strings[j]);
                      Memo2.Append('DataType of Element: ' + jsonTJElementGetType(mylist.Strings[i]));
                      if jsonAsObjectGetKeyList(mylist.Strings[j], mylist2) then
                        Memo2.Append (' index    :    key    :     value    :    datatype ');
                        for k := 0 to mylist2.Count-1 do
                        begin
                          valuestr:='';
                           if jsonAsObjectGetValueByKey(mylist[j],mylist2.Strings[k], valuestr) then
                           begin
                             datatypeString :=  jsonTJElementGetType(valuestr);
                             Memo2.append( inttostr(k) + '    :    ' + mylist2.Strings[k] + '    :    ' +
                             valuestr + '    :    '
                              + datatypeString
                             )
                           end;
                        end;

                        Memo2.Append('---------------------------------------------------');

                    end;
                  end;
            end;
          end;
        // end for
        Memo2.Append('--------------------------------------------------');
        Memo2.Append('----- setting values by key ----------------------');
        for j := 0 to mylist.Count-1 do
        begin
          if jsonAsObjectGetKeyList(mylist.Strings[j],mylist2) then
            begin
              (*
              for k := 0 to mylist2.Count-1 do
              begin
                case mylist2.Strings[k] of   // keys
                    'actionRequest':
                      begin
                        mylist.Strings[j]:= SetValueAndDisplay(Memo2, mylist.Strings[j],'actionRequest', 'setup');
                      end;
                     'packageVersion':
                       begin
                        mylist.Strings[j]:= SetValueAndDisplay(Memo2, mylist.Strings[j],'packageVersion', '10');
                      end;
                     'productClassIds':
                       begin
                        mylist.Strings[j]:= SetValueAndDisplay(Memo2, mylist.Strings[j],'productClassIds', '[1,2]');
                      end;
                     'productVersion':
                      begin
                        mylist.Strings[j]:= SetValueAndDisplay(Memo2, mylist.Strings[j],'productVersion', '5.5');
                      end;
                     'priority':
                      begin
                        mylist.Strings[j]:= SetValueAndDisplay(Memo2, mylist.Strings[j],'priority', '10');
                      end;
                     'defaultValues':
                      begin
                        mylist.Strings[j]:= SetValueAndDisplay(Memo2, mylist.Strings[j],'defaultValues', '[true]');
                      end;
                     'possibleValues':
                      begin
                        mylist.Strings[j]:= SetValueAndDisplay(Memo2, mylist.Strings[j],'possibleValues', '[true,false]');
                      end;
                     'adress':
                        mylist.Strings[j]:= SetValueAndDisplay(Memo2, mylist.Strings[j],'adress', '{"adress":"lalala","pc":"55118","city":"Mainz"}');
                     'licenseRequired':
                      begin
                        mylist.Strings[j]:= SetValueAndDisplay(Memo2,mylist.Strings[j],'licenseRequired', 'true');
                      end;
                     else ;
                end;

              end; *)
              // new key value
              //mylist.Strings[j]:= SetValueAndDisplay(Memo2, mylist.Strings[j],'arraytest', '[true,false]');
              //Memo2.Append('---------------------------------------------------');
              //mylist.Strings[j]:= SetValueAndDisplay(Memo2, mylist.Strings[j],'objecttest', '{"adress":"kurfuerstenstr","pc":"71180","city":"Hinterdupfingen"}');
              //Memo2.Append('---------------------------------------------------');
              mylist.Strings[j]:= SetValueAndDisplay(Memo2, mylist.Strings[j],'inttest', '25');
              mylist.Strings[j]:= SetValueAndDisplay(Memo2, mylist.Strings[j],'noCurrency', '3.5');
              mylist.Strings[j]:= SetValueAndDisplay(Memo2, mylist.Strings[j],'ip-Addresstest', '"123.345.677.900/255"');
              //mylist.Strings[j]:= SetValueAndDisplay(Memo2, mylist.Strings[j],'booleantest', 'true', 'stBoolean');

              Memo2.Append('---------------------------------------------------');
              Memo2.Append('new String, only string : ' + mylist.Strings[j]);
              Memo2.Append('---------------------------------------------------');
              if jsonAsArrayPutObjectByIndex(mylist.Strings[j], strresult, j) then
                 if jsonAsArrayGetElementByIndex(strresult,j, elementstr) then
                  Memo2.Append('new String, jsonAsArrayGetElementByIndex: ' + elementstr)
              else Memo2.Append('Error jsonAsArrayPutElementByIndex ');
            end;
          end; // end for

          jsonArray:='';
          if  stringListToJsonArray(mylist, jsonArray) then
            begin
              Memo2.Append('------------------Stringlist to Array ---------------------------------');
              Memo2.Append ('without clearing : ' + jsonArray);
              if not jsonIsValid(jsonArray) then Memo2.Append('String is NOT valid.')
              else if jsonGetValidString(jsonArray, strresult) then
                begin
                  Memo2.Append('--------------------------------------------------');
                  Memo2.Append('valid string - cleared: ' + jsonClearString(strresult));
                  Memo2.Append('--------------------------------------------------');
                  jsonAsArrayDeleteObjectByIndex(strresult,1);
                  Memo2.Append('delete element index 1: ' + strresult);
                end
              else
                Memo2.Append('Error jsonGetValidString');
            end
          else Memo2.Append('Error stringListToJsonArray ');
         end
        else Memo2.Append ('no String no Object');
    end;
end;
procedure TForm1.FormCreate(Sender: TObject);
begin

end;

procedure TForm1.Memo1Change(Sender: TObject);
begin

end;


end.

