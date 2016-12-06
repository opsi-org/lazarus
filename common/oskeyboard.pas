unit oskeyboard;

//{$mode delphi}
{$mode objfpc}

// http://www.delphifree.de/list21.htm
// see also unit menu

interface

uses
  Classes, SysUtils, lcltype,
  menus, Windows, CommCtrl;


function stringToVK(vkstring : string) : word;
function stringToShiftState(statestring : string) : TshiftState;
function shiftStateToString(state : TshiftState) : string;
//function vkAndShiftstateToWinApiWord(vk, shiftstate : word) : word;
function SetHotKey(AHotKey: TShortCut): Word;
function ShortCutStringToWinApiWord(shortcutstring : string) : word;
function VKtoString(key : word) : string;

implementation

{$IFDEF OPSI}
  uses oslog;
{$ENDIF OPSI}

const
    LLnothing = 0;
  LLessential = 1;
  LLcritical = 2;
  LLerror = 3;
  LLwarning = 4;
  LLnotice = 5;
  LLinfo = 6;
  LLdebug = 7;
  LLdebug2 = 8;
  LLdebug3 = 9;
  LLconfidential = LLdebug3;

procedure log(str : string; level:integer);
begin
  {$IFDEF OPSI}
  logdatei.log(str,level);
  {$ENDIF OPSI}
end;


function ShortCutStringToWinApiWord(shortcutstring : string) : word;
var
  state : TShiftState;
  givenKeyStr : string;
  key : word;
  myShortCut : TShortCut;
begin
  result := 0;
  state := stringToShiftState(shortcutstring);
  givenKeyStr := lowercase(shortcutstring);
  //log('Try to convert :"'+givenKeyStr+'" to virtual key',LLdebug);
  givenKeyStr := StringReplace(givenKeyStr,'shift','',[rfReplaceAll]);
  //log('Try to convert :"'+givenKeyStr+'" to virtual key',LLdebug);
  givenKeyStr := StringReplace(givenKeyStr,'ctrl','',[rfReplaceAll]);
  //log('Try to convert :"'+givenKeyStr+'" to virtual key',LLdebug);
  givenKeyStr := StringReplace(givenKeyStr,'alt','',[rfReplaceAll]);
  //log('Try to convert :"'+givenKeyStr+'" to virtual key',LLdebug);
  givenKeyStr := StringReplace(givenKeyStr,'+','',[rfReplaceAll]);
  //log('Try to convert :"'+givenKeyStr+'" to virtual key',LLdebug);
  givenKeyStr := StringReplace(givenKeyStr,'-','',[rfReplaceAll]);
  //log('Try to convert :"'+givenKeyStr+'" to virtual key',LLdebug);
  givenKeyStr := uppercase(trim(givenKeyStr));
  log('Try to convert :"'+givenKeyStr+'" to virtual key',LLdebug);
  if pos('VK_',givenKeyStr) = 0 then  givenKeyStr := 'VK_'+givenKeyStr;
  key := stringToVK(givenKeyStr);
  if key = 0 then
  begin
    // not a valid key
    log('Convert result not in valid key',LLError);
  end
  else
  begin
    myShortCut := ShortCut(Key,State);
    result :=  SetHotKey(myShortCut);
  end;
end;

// http://www.delphifree.de/list21.htm
function SetHotKey(AHotKey: TShortCut): Word;
var
  Key: Word;
  Shift: TShiftState;
begin
  ShortCutToKey(AHotKey, Key, Shift);
  Key := Swap(Key);
  if ssShift in Shift then Key := Key + HOTKEYF_SHIFT;
  if ssCtrl in Shift then Key := Key + HOTKEYF_CONTROL;
  if ssAlt in Shift then Key := Key + HOTKEYF_ALT;
  Key := Swap(Key);
  result := Key;
end;

function stringToShiftState(statestring : string) : TshiftState;
begin
  result := [];
  if pos('shift',LowerCase(statestring)) > 0 then
  begin
    Include(result,ssShift);
    log('Detected Shift Key',LLdebug);
  end;
  if pos('alt',LowerCase(statestring)) > 0 then
  begin
    Include(result,ssAlt);
    log('Detected Alt Key',LLdebug);
  end;
  if pos('ctrl',LowerCase(statestring)) > 0 then
  begin
    Include(result,ssCtrl);
    log('Detected Ctrl Key',LLdebug);
  end;
end;

function shiftStateToString(state : TshiftState) : string;
begin
  result := '';
  if ssShift in state then result := result+'Shift ';
  if ssCtrl in state then result := result+'Ctrl ';
  if ssAlt in state then result := result+'Alt ';
end;

function stringToVK(vkstring : string) : word;
begin
  result := 0;
  case vkstring of
    'VK_UNKNOWN'        : result := VK_UNKNOWN;
    'VK_LBUTTON'        : result := VK_LBUTTON;
    'VK_RBUTTON'        : result := VK_RBUTTON;
    'VK_CANCEL'         : result := VK_CANCEL;
    'VK_MBUTTON'        : result := VK_MBUTTON;
    'VK_XBUTTON1'       : result := VK_XBUTTON1;
    'VK_XBUTTON2'       : result := VK_XBUTTON2;
    'VK_BACK'           : result := VK_BACK;
    'VK_TAB'            : result := VK_TAB;
    'VK_CLEAR'          : result := VK_CLEAR;
    'VK_RETURN'         : result := VK_RETURN;
    'VK_SHIFT'          : result := VK_SHIFT;
    'VK_CONTROL'        : result := VK_CONTROL;
    'VK_MENU'           : result := VK_MENU;
    'VK_PAUSE'          : result := VK_PAUSE;
    'VK_CAPITAL'        : result := VK_CAPITAL;
    'VK_KANA'           : result := VK_KANA;
    'VK_HANGUL'         : result := VK_HANGUL;
    'VK_JUNJA'          : result := VK_JUNJA;
    'VK_FINAL'          : result := VK_FINAL;
    'VK_HANJA'          : result := VK_HANJA;
    'VK_KANJI'          : result := VK_KANJI;
    'VK_ESCAPE'         : result := VK_ESCAPE;
    'VK_CONVERT'        : result := VK_CONVERT;
    'VK_NONCONVERT'     : result := VK_NONCONVERT;
    'VK_ACCEPT'         : result := VK_ACCEPT;
    'VK_MODECHANGE'     : result := VK_MODECHANGE;
    'VK_SPACE'          : result := VK_SPACE;
    'VK_PRIOR'          : result := VK_PRIOR;
    'VK_NEXT'           : result := VK_NEXT;
    'VK_END'            : result := VK_END;
    'VK_HOME'           : result := VK_HOME;
    'VK_LEFT'           : result := VK_LEFT;
    'VK_UP'             : result := VK_UP;
    'VK_RIGHT'          : result := VK_RIGHT;
    'VK_DOWN'           : result := VK_DOWN;
    'VK_SELECT'         : result := VK_SELECT;
    'VK_PRINT'          : result := VK_PRINT;
    'VK_EXECUTE'        : result := VK_EXECUTE;
    'VK_SNAPSHOT'       : result := VK_SNAPSHOT;
    'VK_INSERT'         : result := VK_INSERT;
    'VK_DELETE'         : result := VK_DELETE;
    'VK_HELP'           : result := VK_HELP;
    'VK_0'              : result := VK_0;
    'VK_1'              : result := VK_1;
    'VK_2'              : result := VK_2;
    'VK_3'              : result := VK_3;
    'VK_4'              : result := VK_4;
    'VK_5'              : result := VK_5;
    'VK_6'              : result := VK_6;
    'VK_7'              : result := VK_7;
    'VK_8'              : result := VK_8;
    'VK_9'              : result := VK_9;

    'VK_A'              : result := VK_A;
    'VK_B'              : result := VK_B;
    'VK_C'              : result := VK_C;
    'VK_D'              : result := VK_D;
    'VK_E'              : result := VK_E;
    'VK_F'              : result := VK_F;
    'VK_G'              : result := VK_G;
    'VK_H'              : result := VK_H;
    'VK_I'              : result := VK_I;
    'VK_J'              : result := VK_J;
    'VK_K'              : result := VK_K;
    'VK_L'              : result := VK_L;
    'VK_M'              : result := VK_M;
    'VK_N'              : result := VK_N;
    'VK_O'              : result := VK_O;
    'VK_P'              : result := VK_P;
    'VK_Q'              : result := VK_Q;
    'VK_R'              : result := VK_R;
    'VK_S'              : result := VK_S;
    'VK_T'              : result := VK_T;
    'VK_U'              : result := VK_U;
    'VK_V'              : result := VK_V;
    'VK_W'              : result := VK_W;
    'VK_X'              : result := VK_X;
    'VK_Y'              : result := VK_Y;
    'VK_Z'              : result := VK_Z;

    'VK_LWIN'           : result := VK_LWIN;
    'VK_RWIN'           : result := VK_RWIN;
    'VK_APPS'           : result := VK_APPS;

    'VK_SLEEP'          : result := VK_SLEEP;

    'VK_NUMPAD0'        : result := VK_NUMPAD0;
    'VK_NUMPAD1'        : result := VK_NUMPAD1;
    'VK_NUMPAD2'        : result := VK_NUMPAD2;
    'VK_NUMPAD3'        : result := VK_NUMPAD3;
    'VK_NUMPAD4'        : result := VK_NUMPAD4;
    'VK_NUMPAD5'        : result := VK_NUMPAD5;
    'VK_NUMPAD6'        : result := VK_NUMPAD6;
    'VK_NUMPAD7'        : result := VK_NUMPAD7;
    'VK_NUMPAD8'        : result := VK_NUMPAD8;
    'VK_NUMPAD9'        : result := VK_NUMPAD9;
    'VK_MULTIPLY'       : result := VK_MULTIPLY;
    'VK_ADD'            : result := VK_ADD;
    'VK_SEPARATOR'      : result := VK_SEPARATOR;
    'VK_SUBTRACT'       : result := VK_SUBTRACT;
    'VK_DECIMAL'        : result := VK_DECIMAL;
    'VK_DIVIDE'         : result := VK_DIVIDE;
    'VK_F1'             : result := VK_F1;
    'VK_F2'             : result := VK_F2;
    'VK_F3'             : result := VK_F3;
    'VK_F4'             : result := VK_F4;
    'VK_F5'             : result := VK_F5;
    'VK_F6'             : result := VK_F6;
    'VK_F7'             : result := VK_F7;
    'VK_F8'             : result := VK_F8;
    'VK_F9'             : result := VK_F9;
    'VK_F10'            : result := VK_F10;
    'VK_F11'            : result := VK_F11;
    'VK_F12'            : result := VK_F12;
    'VK_F13'            : result := VK_F13;
    'VK_F14'            : result := VK_F14;
    'VK_F15'            : result := VK_F15;
    'VK_F16'            : result := VK_F16;
    'VK_F17'            : result := VK_F17;
    'VK_F18'            : result := VK_F18;
    'VK_F19'            : result := VK_F19;
    'VK_F20'            : result := VK_F20;
    'VK_F21'            : result := VK_F21;
    'VK_F22'            : result := VK_F22;
    'VK_F23'            : result := VK_F23;
    'VK_F24'            : result := VK_F24;

    'VK_NUMLOCK'        : result := VK_NUMLOCK;
    'VK_SCROLL'         : result := VK_SCROLL;
    'VK_OEM_1'               : result := VK_OEM_1;
    'VK_OEM_PLUS'            : result := VK_OEM_PLUS;
    'VK_OEM_COMMA'           : result := VK_OEM_COMMA;
    'VK_OEM_MINUS'           : result := VK_OEM_MINUS;
    'VK_OEM_PERIOD'          : result := VK_OEM_PERIOD;
    'VK_OEM_2'               : result := VK_OEM_2;
    'VK_OEM_3'               : result := VK_OEM_3;
    'VK_OEM_4'               : result := VK_OEM_4;
    'VK_OEM_5'               : result := VK_OEM_5;
    'VK_OEM_6'               : result := VK_OEM_6;
    'VK_OEM_7'               : result := VK_OEM_7;
    'VK_OEM_8'               : result := VK_OEM_8;
    'VK_OEM_102'             : result := VK_OEM_102;
    'VK_PROCESSKEY'          : result := VK_PROCESSKEY;
    'VK_ATTN'                : result := VK_ATTN;
    'VK_CRSEL'               : result := VK_CRSEL;
    'VK_EXSEL'               : result := VK_EXSEL;
    'VK_EREOF'               : result := VK_EREOF;
    'VK_PLAY'                : result := VK_PLAY;
    'VK_ZOOM'                : result := VK_ZOOM;
    'VK_NONAME'              : result := VK_NONAME;
    'VK_PA1'                 : result := VK_PA1;
    'VK_OEM_CLEAR'           : result := VK_OEM_CLEAR;
  else
    // unknown VK string
    result := VK_UNKNOWN;
    raise Exception.Create('Unknown virtual key: '+vkstring);
  end;
end;

function VKtoString(key : word) : string;
begin
  result := '';
  case key of
    VK_UNKNOWN         : result := 'VK_UNKNOWN';
    VK_LBUTTON         : result := 'VK_LBUTTON';
    VK_RBUTTON         : result := 'VK_RBUTTON';
    VK_CANCEL          : result := 'VK_CANCEL';
    VK_MBUTTON         : result := 'VK_MBUTTON';
    VK_XBUTTON1        : result := 'VK_XBUTTON1';
    VK_XBUTTON2        : result := 'VK_XBUTTON2';
    VK_BACK            : result := 'VK_BACK';
    VK_TAB             : result := 'VK_TAB';
    VK_CLEAR           : result := 'VK_CLEAR';
    VK_RETURN          : result := 'VK_RETURN';
    VK_SHIFT           : result := 'VK_SHIFT';
    VK_CONTROL         : result := 'VK_CONTROL';
    VK_MENU            : result := 'VK_MENU';
    VK_PAUSE           : result := 'VK_PAUSE';
    VK_CAPITAL         : result := 'VK_CAPITAL';
    VK_KANA            : result := 'VK_KANA';
    //VK_HANGUL          : result := 'VK_HANGUL';  // = VK_KANA
    VK_JUNJA           : result := 'VK_JUNJA';
    VK_FINAL           : result := 'VK_FINAL';
    VK_HANJA           : result := 'VK_HANJA';
    //VK_KANJI           : result := 'VK_KANJI';  // = VK_HANJA
    VK_ESCAPE          : result := 'VK_ESCAPE';
    VK_CONVERT         : result := 'VK_CONVERT';
    VK_NONCONVERT      : result := 'VK_NONCONVERT';
    VK_ACCEPT          : result := 'VK_ACCEPT';
    VK_MODECHANGE      : result := 'VK_MODECHANGE';
    VK_SPACE           : result := 'VK_SPACE';
    VK_PRIOR           : result := 'VK_PRIOR';
    VK_NEXT            : result := 'VK_NEXT';
    VK_END             : result := 'VK_END';
    VK_HOME            : result := 'VK_HOME';
    VK_LEFT            : result := 'VK_LEFT';
    VK_UP              : result := 'VK_UP';
    VK_RIGHT           : result := 'VK_RIGHT';
    VK_DOWN            : result := 'VK_DOWN';
    VK_SELECT          : result := 'VK_SELECT';
    VK_PRINT           : result := 'VK_PRINT';
    VK_EXECUTE         : result := 'VK_EXECUTE';
    VK_SNAPSHOT        : result := 'VK_SNAPSHOT';
    VK_INSERT          : result := 'VK_INSERT';
    VK_DELETE          : result := 'VK_DELETE';
    VK_HELP            : result := 'VK_HELP';
    VK_0               : result := 'VK_0';
    VK_1               : result := 'VK_1';
    VK_2               : result := 'VK_2';
    VK_3               : result := 'VK_3';
    VK_4               : result := 'VK_4';
    VK_5               : result := 'VK_5';
    VK_6               : result := 'VK_6';
    VK_7               : result := 'VK_7';
    VK_8               : result := 'VK_8';
    VK_9               : result := 'VK_9';

    VK_A               : result := 'VK_A';
    VK_B               : result := 'VK_B';
    VK_C               : result := 'VK_C';
    VK_D               : result := 'VK_D';
    VK_E               : result := 'VK_E';
    VK_F               : result := 'VK_F';
    VK_G               : result := 'VK_G';
    VK_H               : result := 'VK_H';
    VK_I               : result := 'VK_I';
    VK_J               : result := 'VK_J';
    VK_K               : result := 'VK_K';
    VK_L               : result := 'VK_L';
    VK_M               : result := 'VK_M';
    VK_N               : result := 'VK_N';
    VK_O               : result := 'VK_O';
    VK_P               : result := 'VK_P';
    VK_Q               : result := 'VK_Q';
    VK_R               : result := 'VK_R';
    VK_S               : result := 'VK_S';
    VK_T               : result := 'VK_T';
    VK_U               : result := 'VK_U';
    VK_V               : result := 'VK_V';
    VK_W               : result := 'VK_W';
    VK_X               : result := 'VK_X';
    VK_Y               : result := 'VK_Y';
    VK_Z               : result := 'VK_Z';

    VK_LWIN            : result := 'VK_LWIN';
    VK_RWIN            : result := 'VK_RWIN';
    VK_APPS            : result := 'VK_APPS';

    VK_SLEEP           : result := 'VK_SLEEP';

    VK_NUMPAD0         : result := 'VK_NUMPAD0';
    VK_NUMPAD1         : result := 'VK_NUMPAD1';
    VK_NUMPAD2         : result := 'VK_NUMPAD2';
    VK_NUMPAD3         : result := 'VK_NUMPAD3';
    VK_NUMPAD4         : result := 'VK_NUMPAD4';
    VK_NUMPAD5         : result := 'VK_NUMPAD5';
    VK_NUMPAD6         : result := 'VK_NUMPAD6';
    VK_NUMPAD7         : result := 'VK_NUMPAD7';
    VK_NUMPAD8         : result := 'VK_NUMPAD8';
    VK_NUMPAD9         : result := 'VK_NUMPAD9';
    VK_MULTIPLY        : result := 'VK_MULTIPLY';
    VK_ADD             : result := 'VK_ADD';
    VK_SEPARATOR       : result := 'VK_SEPARATOR';
    VK_SUBTRACT        : result := 'VK_SUBTRACT';
    VK_DECIMAL         : result := 'VK_DECIMAL';
    VK_DIVIDE          : result := 'VK_DIVIDE';
    VK_F1              : result := 'VK_F1';
    VK_F2              : result := 'VK_F2';
    VK_F3              : result := 'VK_F3';
    VK_F4              : result := 'VK_F4';
    VK_F5              : result := 'VK_F5';
    VK_F6              : result := 'VK_F6';
    VK_F7              : result := 'VK_F7';
    VK_F8              : result := 'VK_F8';
    VK_F9              : result := 'VK_F9';
    VK_F10             : result := 'VK_F10';
    VK_F11             : result := 'VK_F11';
    VK_F12             : result := 'VK_F12';
    VK_F13             : result := 'VK_F13';
    VK_F14             : result := 'VK_F14';
    VK_F15             : result := 'VK_F15';
    VK_F16             : result := 'VK_F16';
    VK_F17             : result := 'VK_F17';
    VK_F18             : result := 'VK_F18';
    VK_F19             : result := 'VK_F19';
    VK_F20             : result := 'VK_F20';
    VK_F21             : result := 'VK_F21';
    VK_F22             : result := 'VK_F22';
    VK_F23             : result := 'VK_F23';
    VK_F24             : result := 'VK_F24';

    VK_NUMLOCK         : result := 'VK_NUMLOCK';
    VK_SCROLL          : result := 'VK_SCROLL';
    VK_OEM_1                : result := 'VK_OEM_1';
    VK_OEM_PLUS             : result := 'VK_OEM_PLUS';
    VK_OEM_COMMA            : result := 'VK_OEM_COMMA';
    VK_OEM_MINUS            : result := 'VK_OEM_MINUS';
    VK_OEM_PERIOD           : result := 'VK_OEM_PERIOD';
    VK_OEM_2                : result := 'VK_OEM_2';
    VK_OEM_3                : result := 'VK_OEM_3';
    VK_OEM_4                : result := 'VK_OEM_4';
    VK_OEM_5                : result := 'VK_OEM_5';
    VK_OEM_6                : result := 'VK_OEM_6';
    VK_OEM_7                : result := 'VK_OEM_7';
    VK_OEM_8                : result := 'VK_OEM_8';
    VK_OEM_102              : result := 'VK_OEM_102';
    VK_PROCESSKEY           : result := 'VK_PROCESSKEY';
    VK_ATTN                 : result := 'VK_ATTN';
    VK_CRSEL                : result := 'VK_CRSEL';
    VK_EXSEL                : result := 'VK_EXSEL';
    VK_EREOF                : result := 'VK_EREOF';
    VK_PLAY                 : result := 'VK_PLAY';
    VK_ZOOM                 : result := 'VK_ZOOM';
    VK_NONAME               : result := 'VK_NONAME';
    VK_PA1                  : result := 'VK_PA1';
    VK_OEM_CLEAR            : result := 'VK_OEM_CLEAR';
  else
    // unknown VK string
    result := '';
    raise Exception.Create('No virtual key found fot: '+IntTostr(key));
  end;
end;


end.

