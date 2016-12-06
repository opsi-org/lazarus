unit unicode;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}


// parts adopted from unicode.pas by Mike Lischke
// Copyright (c) 1999, 2000 Mike Lischke (public@lischke-online.de)
// Portions Copyright (c) 1999, 2000 Azret Botash (az)
//
// This code is part of the opsi.org project
//
// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the General Public License.
//
// Text of the GPL: http://www.gnu.org/licenses/gpl.html
// Unofficial GPL Translations: http://www.gnu.org/licenses/translations.html
//
// author: Rupert Roeder, detlef oertel
// credits: http://www.opsi.org/credits/



interface

uses
sysutils,classes;

const
  // definitions of often used characters:
  // Note: Use them only for tests of a certain character not to determine character classes like
  //       white spaces as in Unicode are often many code points defined being in a certain class.
  //       Hence your best option is to use the various UnicodeIs* functions.

  // can't use identifier "Null" here as this is already in a special Variant identifier
  WideNull = WideChar(#0);
  Tabulator = WideChar(#9);
  Space = WideChar(#32);

  // logical line breaks
  LF = WideChar($A);
  LineFeed = WideChar($A);
  VerticalTab = WideChar($B);
  FormFeed = WideChar($C);
  CR = WideChar($D);
  CarriageReturn = WideChar($D);
  CRLF: WideString = #$D#$A;
  LineSeparator = WideChar($2028);
  ParagraphSeparator = WideChar($2029);

  // byte order marks for strings
  // Unicode text files should contain $FFFE as first character to identify such a file clearly. Depending on the system
  // where the file was created on this appears either in big endian or little endian style.
  BOM_LSB_FIRST = WideChar($FEFF); // this is how the BOM appears on x86 systems when written by a x86 system
  BOM_MSB_FIRST = WideChar($FFFE);

type
  // Unicode transformation formats (UTF) data types
  UTF7 = Char;
  UTF8 = Char;
  UTF16 = WideChar;
  UTF32 = Cardinal;

  // UTF conversion schemes (UCS) data types
  PUCS4 = ^UCS4;
  UCS4 = Cardinal;
  PUCS2 = PWideChar;
  UCS2 = WideChar;

const
  ReplacementCharacter: UCS4 = $0000FFFD;
  MaximumUCS2: UCS4 = $0000FFFF;
  MaximumUTF16: UCS4 = $0010FFFF;
  MaximumUCS4: UCS4 = $7FFFFFFF;

  SurrogateHighStart: UCS4 = $D800;
  SurrogateHighEnd: UCS4 = $DBFF;
  SurrogateLowStart: UCS4 = $DC00;
  SurrogateLowEnd: UCS4 = $DFFF;

  SDuplicateString = 'duplicate string';
  SListIndexError = 'list index error';
  SSortedListError = 'sorted list error';


procedure getStrListFromWS(const Value: widestring; var sl : TStringList);

procedure WSLoadFromStream(Stream: TStream; var slist : TStringList);

procedure WSLoadFromFile(const FileName: String; var slist : TStringList);

implementation


procedure StrSwapByteOrder(Str: PWideChar); 

// exchanges in each character of the given string the low order and high order
// byte to go from LSB to MSB and vice versa.
// EAX contains address of string

asm
         PUSH ESI
         PUSH EDI
         MOV ESI, EAX
         MOV EDI, ESI
         XOR EAX, EAX  // clear high order byte to be able to use 32bit operand below
@@1:     LODSW
         OR EAX, EAX
         JZ @@2
         XCHG AL, AH
         STOSW
         JMP @@1
         
@@2:     POP EDI
         POP ESI
end;


procedure getStrListFromWS(const Value: widestring; var sl : TStringList);

var
  Head,
  Tail: PWideChar;
  wS: WideString;

  myS : String;


begin
    sl.Clear;
    Head := PWideChar(Value);
    while Head^ <> WideNull do
    begin
      Tail := Head;
      while not (Tail^ in [WideNull, LineFeed, CarriageReturn, VerticalTab, FormFeed]) and
            (Tail^ <> LineSeparator) and
            (Tail^ <> ParagraphSeparator) do Inc(Tail);
      SetString(ws, Head, Tail - Head);

      myS := ws; // implicit conversion ??
      sl.add (myS);

      Head := Tail;
      if Head^ <> WideNull then
      begin
        Inc(Head);
        if (Tail^ = CarriageReturn) and
           (Head^ = LineFeed) then Inc(Head);
      end;
    end;
end;


procedure WSLoadFromStream(Stream: TStream; var slist : TStringList);
var
  Size,
  BytesRead: Integer;
  Order: WideChar;
  SW: WideString;
  ///SA: String;
  SaveUnicode : Boolean;

begin
    Size := Stream.Size - Stream.Position;
    BytesRead := Stream.Read(Order, 2);
    if (Order = BOM_LSB_FIRST) or (Order = BOM_MSB_FIRST) then
    begin
      SaveUnicode := True;
      SetLength(SW, (Size - 2) div 2);
      Stream.Read(PWideChar(SW)^, Size - 2);
      if Order = BOM_MSB_FIRST then StrSwapByteOrder(PWideChar(SW));
      getStrListFromWS(SW, slist);
    end
end;


procedure WSLoadFromFile(const FileName: String; var slist : TStringList);
var
  Stream: TStream;

begin
  try
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    try
      WSLoadFromStream (Stream, slist);
    finally
      Stream.Free;
    end;
  except
    {$IFDEF WINDOWS}RaiseLastWin32Error;{$ENDIF}
  end;
end;


begin
end.
