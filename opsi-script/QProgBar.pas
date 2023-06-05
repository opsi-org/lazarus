unit QProgBar;

{$IFDEF FPC}
	{$MODE Delphi}
{$ENDIF}


    { ****************************************************************** }
    { v 1.1                                                              }
    {           Delphi (6) unit  --   progressbar replacement, with      }
    {                                 several features...                }
    {                                                                    }
    {           Copyright © 2004 by Olivier Touzot "QnnO"                }
    {    (http://mapage.noos.fr/qnno/delphi_en.htm  -  qnno@noos.fr)     }
    {                                                                    }
    {              ----------------------------------                    }
    {                                                                    }
    { History :                                                          }
    { v 1.1 : 2004-05-12 (!)  Correction of the "extreme colors" bug in  }
    {         the GetGradientAr2(); function by Bernd Kirchhoff, allowing}
    {         the use of pure white or black colors in the bars. Thanks  }
    {         and congratulations (he made the work under cbuilder 4.0 !)}
    { v 1.0 : 2004-05-11 First release ;                                 }
    { ****************************************************************** }


    //  This unit is freeware, but under copyrights that remain mine for my
    //  parts of the code, and original writters for their parts of the code.
    //  This is mainly the case with :
    //  -> The polynomial expression of the MakeCylinder(); function, provided
    //     by Matthieu Contensou, (with lots of help too, on many other
    //     subjects (see below)).
    //     (http://www25.brinkster.com/waypointfrance/cpulog/index.asp)
    //  -> The RGBtoHLS(); and HLStoRGB(); procedures, that come from a
    //     Microsoft knowledge base article (Q29240), at :
    //     http://support.microsoft.com/default.aspx?scid=kb;en-us;29240
    //  -> The GetColorBetween(); function, which computes the main gradient,
    //     found at efg's colors page, and which author is saddly unknown :
    //     http://homepages.borland.com/efg2lab/Library/Delphi/Graphics/Color.htm
    //     http://homepages.borland.com/efg2lab/Library/UseNet/2001/0821.txt
    //  -> The GetGradientAr2(); new version, by Bernd Kirchhoff, which now
    //     correctly handles white and black colors in bars.
    //     (http://home.germany.net/100-445474/)

    //  This unit can be freely used in any application, freeware, shareware
    //  or commercial. However, I would apreciate your sending me an email if
    //  you decide to use it. Of course, you use it under your own and single
    //  responsability. Neither me, nor contributors, could be held responsible
    //  for any problem resulting from the use of this unit.  ;-)

    //  It can also be freely distributed, provided all the above (and current)
    //  lines remain within it unchanged, and the readme.txt file be distributed
    //  with it too.

    //  Many thanks go to Matthieu Contensou, who spent a lot of time (and
    //  patience ... ) trying to explain me the subtleties of the RGB -> YUV
    //  and return conversions.)
    //  He gave the idea of using the HLS space too, which is now used in this
    //  component.



interface

uses
{$IFDEF FPC}
	LCLIntf,
//Interfaces,
        LCLType,
        interfaceBase,
        lazcanvas,
{$ELSE}
	Messages,
	StdCtrls,
        windows,
{$ENDIF}
 SysUtils, Classes, Graphics, Controls, Forms, Dialogs,Math;


type
  TQBarKind = (bkFlat,bkCylinder);
  TQBarLook = (blMetal,blGlass);
  TQBarOrientation = (boHorizontal,boVertical);

  TRGBArray = array[0..2] of Byte;
  TCLRArray = array of TColor;
  THLSRange = 0..240;

  THLSRec   = record                            // Color conversion -> RgbToHls and return
    hue: THLSRange;
    lum: THLSRange;
    sat: THLSRange;
  end;

  TPosDescr = Record                            // Bar description, rows or column ...
    isInBlock: Boolean;                         // ... depending on orientation
    blkLimit : Integer;
  End;

  TQProgressBar = class(TCustomControl)
  private
    fPosDescr      : Array of TPosDescr;        // Bar description, blocks and spaces
    fPixDescr      : Array of TCLRArray;        // Bar description, pixels colors
    fInactDescr    : TCLRArray;                 // Bar description, inactive positions colors (if reversed gradient);
    fInitsLocked   : Boolean;                   // Avoid too many computings of above arrays ;
    fBarKind       : TQBarKind;                 // flat or rounded
    fBarLook       : TQBarLook;                 // blMetal or blGlass
    fOrientation   : TQBarOrientation;          // horizontal or vertical
    fInternalBorder,                            // space between the shape and the bar itself (1 or two pixels)
    fUSefullDrawSpace,                          // size of the bar minus border
    fBorderSize    : Integer;                   // 2*(border+shape)
    fHasShape      : Boolean;                   // the surrounding line
    fShapeClr      : TColor;                    // above' color
    fCorner        : Integer;                   // shape' corner
    fStartClr,                                  // left (or bottom) color
    fFinalClr,                                  // right (or top) color
    fBkgClr        : TColor;                    // background color.
    fMonoClr       : Boolean;                   // True if StartColor = FinalColor.
    fInvInactPos,                               // If true, and gradient, -> inverted;
    fShowInactPos  : Boolean;                   // Bars corresp. to positions above actual are drawn in fInactPosClr
    fInactPosClr   : TColor;                    // Above's color
    fUSerPosPct    : Real;                      // same as below, as percent, for displays
    fUserPos,                                   // value sent by user
    fPosition,                                  // above, normalized to width or height, and max;
    fMinVisPos,                                 // Minimum position to send to Paint(), to see at least one bar
    fMaxPos        : Integer;                   // max position as sent by user.
    fByBlock,                                   // if true, alternates colored and not colored pixels
    fFullBlock     : Boolean;                   // if true, blocks are drawn only when their max position is reached;
    fSpaceSize,                                 // space between two blocks
    fBlockSize     : Integer;                   // width (or height) of a block
    fHideOnTerm    : Boolean;                   // Hides the bar a tenth of a second after the painting of the last pixel row/column;
    fCaption       : String;                    // A user defined string, or the current position dep. on fCaptionOvr & fShowPosAsPct
    fCapAlign      : TAlignment;                // left - right - centered
    fCapPos        : TPoint;                    // Internal - caption's top and left, based on canvas' current font
    fHasCaption    : Boolean;                   // Internal
    fFont          : TFont;                     // A user defined font can be assigned to the progress bar's canvas
    fShowPosAsPct  : Boolean;                   // If True, Hint and/or caption will show the value as a percent of the maximum.
    fCaptionOvr    : Boolean;                   // id. bellow;
    fHintOvr       : Boolean;                   // if True, each position changes => Hint <- fUserPos or fUSerPosPct
                                                // dep. on ShowPosAsPct True/false;

  protected
    procedure Paint;  Override;
    procedure Resize; Override;
    procedure SetUsefullWidth;
    procedure InitBlockArray;
    procedure InitPixArray;
    function  MakeCylinder(h:real):extended;
    function  GetGradientAr2(aColor:TColor;sz:Integer):TClrArray;
    function  HLStoRGB(hue,lum,sat: THLSRange): TColor;
    function  RGBtoHLS(RGBColor: TColor): THLSRec;
    (* Following function is commented out since it caused an access violation. (a.schmitz@uib.de)
    function  GetColorBetween(StartColor, EndColor: TColor; Pointvalue,
                              Von, Bis : Extended): TColor; *)
    procedure SetOrientation(value:TQBarOrientation);
    procedure SetBarKind    (value:TQBarKind);
    procedure SetBarLook    (value:TQBarLook);
    procedure SetFCorner    (isRounded:Boolean);
    function  GetBoolCorner:Boolean;
    procedure SetBkgColor   (aColor:TColor);
    procedure SetShape      (value:Boolean);
    procedure SetShapeColor (value:TColor);
    procedure SetBlockSize  (value:Integer);
    procedure SetSpaceSize  (value:Integer);
    procedure SetFullBlock  (value:Boolean);
    procedure SetMaxPos     (value:Integer);
    procedure SetPosition   (value:Integer);
    procedure SetStartClr   (value:TColor);
    procedure SetFinalClr   (value:TColor);
    procedure SetBothColors (value:TColor);
    procedure SetInactivePos(value:Boolean);
    procedure SetInactPosClr(value:TColor);
    procedure SetInvInactPos(value:Boolean);
    procedure SetCaption    (value:String);
    procedure SetCapAlign   (value:TAlignment);
    procedure SetFont       (value:TFont);
    procedure SetCaptionOvr (value:Boolean);

  public
    constructor Create (AOwner : TComponent); override;
    destructor  Destroy; override;
    procedure   LockInits;
    procedure   UnlockInits;

  published
    property orientation     : TQBarOrientation read fOrientation  write SetOrientation;
    property barKind         : TQBarKind        read fBarKind      write SetBarKind;
    property barLook         : TQBarLook        read fBarLook      write SetBarLook;
    property roundCorner     : Boolean          read GetBoolCorner write SetFCorner;
    property backgroundColor : TColor           read fBkgClr       write SetBkgColor;
    property barColor        : TColor           read fStartClr     write SetBothColors;
    property startColor      : TColor           read fStartClr     write SetStartClr;
    property finalColor      : TColor           read fFinalClr     write SetFinalClr;
    property showInactivePos : Boolean          read fShowInactPos write SetInactivePos;
    property invertInactPos  : Boolean          read fInvInactPos  write SetInvInactPos;
    property inactivePosColor: TColor           read fInactPosClr  write SetInactPosClr;
    property shaped          : Boolean          read fHasShape     write SetShape;
    property shapeColor      : TColor           read fShapeClr     write SetShapeColor;
    property blockSize       : Integer          read fBlockSize    write SetBlockSize;
    property spaceSize       : Integer          read fSpaceSize    write SetSpaceSize;
    property showFullBlock   : Boolean          read fFullBlock    write SetFullBlock;
    property maximum         : Integer          read fMaxPos       write SetMaxPos;
    property position        : Integer          read fUserPos      write SetPosition;
    property hideOnTerminate : Boolean          read fHideOnTerm   write fHideOnTerm default False;
    property caption         : String           read fCaption      write SetCaption;
    property captionAlign    : TAlignment       read fCapAlign     write SetCapAlign;
    property font            : TFont            read fFont         write SetFont;
    property AutoCaption     : Boolean          read fCaptionOvr   write SetCaptionOvr;
    property AutoHint        : Boolean          read fHintOvr      write fHintOvr;
    property ShowPosAsPct    : Boolean          read fShowPosAsPct write fShowPosAsPct;
    property Visible;
    property height;
    property width;
  end;


const
  // NIH... Out a Microsoft knowledge base article, see below "RGBtoHLS" and "HLStoRGB"
  HLSMAX = High(THLSRange);	// H,L, and S vary over 0-HLSMAX
  RGBMAX = 255;		        // R,G, and B vary over 0-RGBMAX
				// HLSMAX BEST IF DIVISIBLE BY 6
				// RGBMAX, HLSMAX must each fit in a byte.
  // Hue is undefined if Saturation is 0 (grey-scale)
  // This value determines where the Hue scrollbar is
  // initially set for achromatic colors 
  UNDEFINED = HLSMAX * 2 div 3;


procedure Register;

implementation

Procedure Register;
begin
  RegisterComponents('Samples', [TQProgressBar]);
end;


Procedure TQProgressBar.InitBlockArray;
// fPosDescr[n] describes each possible position, storing :
// - wether it is in a block or not ;               <- drawing blocks instead of a continuous line
// - what is the block limit for this position;     <- (if full blocks only are to be drawn, then
//   only those which limit is bellow(H) above(V) current position will be drawn.)
// Computed on size/resize and blocks/space sizes changes only, to avoid computations at runTime.
var i,
    blkStart,
    blkStop : Integer;
Begin
  If (fBlockSize = 0) or (fSpaceSize = 0) Then exit;

  If fUSefullDrawSpace <= 0
     Then SetLength(Self.fPosDescr, 1)                               // Position 0 is allways False
     Else SetLength(Self.fPosDescr, fUSefullDrawSpace+1);          

  Case Self.fOrientation of
    boHorizontal :
      Begin
        fPosDescr[0].isInBlock := False;
        blkStart := 3;
        blkStop  := blkStart + fBlockSize -1 ;
        For i := 1 To High(fPosDescr) Do 
        Begin                            
          fPosDescr[i].isInBlock := (i >= blkStart) And (i <= blkStop);
          fPosDescr[i].blkLimit  := blkStop;
          If i = blkStop Then
          Begin
            blkStart := blkStop  + fSpaceSize + 1;
            blkStop  := blkStart + fBlockSize - 1;
            If blkStop > High(fPosDescr) Then blkStop := High(fPosDescr);
          End;
        End;
      End;{boHrz}
    Else                                                             // boVertical; "Else" avoids compiler warnings
      Begin
        fPosDescr[High(fPosDescr)].isInBlock := False;
        blkStart := High(fPosDescr)-3;
        blkStop  := blkStart - fBlockSize + 1 ;
        For i := fUSefullDrawSpace DownTo fBorderSize Do
        Begin                                                        
          fPosDescr[i].isInBlock := (i <= blkStart) And (i >= blkStop);
          fPosDescr[i].blkLimit  := blkStop;
          If i = blkStop Then
          Begin
            blkStart := blkStop  - fSpaceSize - 1;
            blkStop  := blkStart - fBlockSize + 1;
            If blkStop < fBorderSize Then blkStop := fBorderSize;
          End;
        End;
      End;{boVert}
  End;{case}
End;


Procedure TQProgressBar.InitPixArray;
// Compute and stores each pixel color, in the case of a gradient, or a double 
// gradient (both directions) in order to speed up things at run time.
var i, j,
    rowSz : integer;
    clr   : TColor;
    HLSr  : THLSRec;
Begin
  If Self.fInitsLocked Then exit;                        // Avoids intermediate computings

  Case fOrientation of
    boHorizontal : rowSz := Height - (fBorderSize) + 1;
    Else           rowSz := Width  - (fBorderSize) + 1;  // boVertical;
  End;{Case} 

  If fUSefullDrawSpace <= 0
     Then SetLength(fPixDescr, 1)                        // Position 0 is allways False
     Else SetLength(fPixDescr, fUSefullDrawSpace + 1);

  // Populates active positions colors array ;
  // -> GetColorBetween works on the horizontal gradient, in the case of a 
  //    boHorizontal bar, with two colors (or on the vertical one, if the 
  //    bar is vertical).
  // -> GetGradientAr2  then returns the row gradient, based upon the header
  //    pixel value for that row in order to give the cylinder appearance.

  For i := 0 To fUSefullDrawSpace Do
  Begin
    // Always use StartColor instead of function GetColorBetween since it caused an access violation. (a.schmitz@uib.de)
    clr := fStartClr;
    If fBarKind = bkCylinder
       Then Self.fPixDescr[i] := GetGradientAr2( clr, rowSz )
       Else For j := 0 To rowSz -1 Do
            Begin
              SetLength(fPixDescr[i],rowSz);
              fPixDescr[i,j] := clr;
            End;
  End;

  // inactive positions decription, used in case 'showInactive positions' is true;
  If ( Height - fBorderSize ) <= 0 Then
  Begin
    SetLEngth( fInactDescr, 1 );
    fInactDescr[0] := self.fInactPosClr;
  End Else
  Begin
    If fBarKind = bkCylinder
    Then fInactDescr := GetGradientAr2(fInactPosClr, rowSz )
    Else Begin
           SetLength(fInactDescr,rowSz);
           For j := 0 To rowSz -1 Do
            fInactDescr[j] := fInactPosClr;
         End;
  End;
  
  // case cylindric bar : the background can be basically reversed.
  If (fBarKind=bkCylinder) And (fInvInactPos) Then
     For i := 0 To rowSz -1 Do
     Begin
       HLSr := RGBtoHLS(fInactDescr[i]);
       HLSr.lum := 240-HLSr.lum;
       fInactDescr[i] := HLStoRGB(HLSr.hue,HLSr.lum,HLSr.sat);
     End;

End;


Function TQProgressBar.MakeCylinder(h:real):extended;                      // NIH
// (c) Matthieu Contensou (http://www25.brinkster.com/waypointfrance/cpulog/index.asp)
// who computed the polynome used to provide the "cylinder" appearence to bars :
// "f (h) = -4342,9 h^5 + 10543 h^4 - 8216 h^3 + 2018,1 h^2 + 11,096 h + 164,6"
// "h is the order of the wanted pixel in a column (horizontal bar), or in
// a row (vertical bar), with a value between 0 and 1 (0 -> 100%)"

Begin
  result := ( (-4342.9 * ( power(h,5) ) )
          +   ( 10543  * ( power(h,4) ) )
          -   ( 8216   * ( power(h,3) ) )
          +   ( 2018.1 * ( power(h,2) ) )
          +   ( 11.096 * h ) + 164.6  ) ;
End;


Function TQProgressBar.GetGradientAr2(aColor:TColor;sz:Integer):TClrArray;
// Version corrected by Bernd Kirchhoff (http://home.germany.net/100-445474/)
// Returns an array of size sz, filled up with a basic gradient; Used to
// provide the "cylindric" appearance.
var i,RP: Integer;
    HLSr: THLSRec;
Begin
  SetLength(result,sz);
  For i := 0 To sz -1 Do
  Begin
    HLSr := RGBtoHLS(aColor);
    // (c) Bernd Kirchhoff >>>--------------------------------------------------
    If self.fBarLook = blGlass Then
      HLSr.lum := Round(MakeCylinder( (i / sz)) )
    Else
    begin
      rp:=HLSr.lum - 212;
      rp:=rp+Trunc(MakeCylinder( i / sz));
      if rp < 0   then rp:=0;
      if rp > 240 then rp:=240;
      HLSr.lum :=rp;
    end;
   // <<<-----------------------------------------------------------------------
    result[i] := HLStoRGB(HLSr.hue,HLSr.lum,HLSr.sat);
  End;
End;


function TQProgressBar.RGBtoHLS(RGBColor: TColor): THLSRec;              // NIH
// (c) Microsoft. http://support.microsoft.com/default.aspx?scid=kb;en-us;29240
// This is the translation of a Microsoft knowledge base article, pubilshed
// under number Q29240. Msft's knowledge base has a lot of interesting articles.

//(knowledge base = http://support.microsoft.com/default.aspx?scid=FH;EN-US;KBHOWTO)

var
   R, G, B: Integer;                        // input RGB values
   H, L, S: Integer;
   cMax, cMin: Byte;                        // max and min RGB values
   Rdelta,Gdelta,Bdelta: Integer;           // intermediate value: % of spread from max
begin
  // get R, G, and B out of DWORD
  R := GetRValue(RGBColor);
  G := GetGValue(RGBColor);
  B := GetBValue(RGBColor);

  // calculate lightness
  cMax := max( max(R,G), B);
  cMin := min( min(R,G), B);
  L := ( ( (cMax+cMin) * HLSMAX) + RGBMAX ) div (2*RGBMAX);

  If (cMax = cMin) then                     // r=g=b --> achromatic case
  Begin
     S := 0;                                // saturation
     H := UNDEFINED;                        // hue
  end else
  begin	                                    // chromatic case
     If (L <= (HLSMAX div 2) )              // saturation
     Then S := ( ( (cMax-cMin) * HLSMAX ) + ( (cMax+cMin) div 2) ) div (cMax+cMin)
     Else S := ( ( (cMax-cMin) * HLSMAX ) + ( (2*RGBMAX-cMax-cMin) div 2) ) div (2*RGBMAX-cMax-cMin);
     // hue
     Rdelta := ( ( (cMax-R) * (HLSMAX div 6) ) + ((cMax-cMin) div 2) ) div (cMax-cMin);
     Gdelta := ( ( (cMax-G) * (HLSMAX div 6) ) + ((cMax-cMin) div 2) ) div (cMax-cMin);
     Bdelta := ( ( (cMax-B) * (HLSMAX div 6) ) + ((cMax-cMin) div 2) ) div (cMax-cMin);

     If R = cMax Then      H := Bdelta - Gdelta
     Else If G = cMax Then H := (HLSMAX div 3) + Rdelta - Bdelta
          Else {B=cMax}    H := ( (2*HLSMAX) div 3) + Gdelta - Rdelta;
     If (H < 0)      Then  H := H + HLSMAX;
     If (H > HLSMAX) Then  H := H - HLSMAX;
  end;

  Result.Hue := H;
  Result.Lum := L;
  Result.Sat := S;
end;


function TQProgressBar.HLStoRGB(hue,lum,sat: THLSRange): TColor;         // NIH
// (c) Microsoft. http://support.microsoft.com/default.aspx?scid=kb;en-us;29240
var
   R,G,B : Integer;                         // RGB component values
   Magic1,Magic2: Integer;                  // calculated magic numbers (really!)


   { -----------------  LOCAL  -----------------}

     Function HueToRGB(n1,n2,hue: Integer): Integer;                // (c) Microsoft.
     // utility routine for HLStoRGB
     Begin
       // range check: note values passed add/subtract thirds of range
       If      hue < 0      Then Inc(hue, HLSMAX)
       Else If hue > HLSMAX Then Dec(hue, HLSMAX);

       (* return r,g, or b value from this tridrant *)
       If hue < (HLSMAX div 6)
       Then result := ( n1 + ( ( (n2-n1) * hue + (HLSMAX div 12) ) div (HLSMAX div 6) ) )
       Else If hue < (HLSMAX div 2)
            Then result := n2
            Else If hue < ( (HLSMAX*2) div 3 )
                 Then result := ( n1 + ( ( (n2-n1) * ( ( (HLSMAX*2) div 3 ) - hue )
                                     + (HLSMAX div 12) ) div (HLSMAX div 6) ) )
                 Else result := n1;
     end;{HueToRGB}

   { ----------------- \LOCAL\ -----------------}


begin
  If Sat = 0 then                           // achromatic case
  Begin
    R :=(Lum*RGBMAX) div HLSMAX;
    G := R;
    B := R;
    If Not(Hue = UNDEFINED) then
    begin
      // ...trap impossible conversions (?)...
    End;
  End Else
  Begin                                     // chromatic case
    If (Lum <= (HLSMAX div 2))              // set up magic numbers
    Then Magic2 := ( Lum * ( HLSMAX + Sat )  + ( HLSMAX div 2 ) ) div HLSMAX
    Else Magic2 := Lum + Sat - ( (Lum * Sat) + ( HLSMAX div 2 ) ) div HLSMAX;
    Magic1 := 2*Lum - Magic2;

    // get RGB, change units from HLSMAX to RGBMAX
    R := ( HueToRGB( Magic1, Magic2, Hue  + ( HLSMAX div 3 ) ) * RGBMAX + ( HLSMAX div 2) ) div HLSMAX;
    G := ( HueToRGB( Magic1, Magic2, Hue )* RGBMAX +(HLSMAX div 2 ) ) div HLSMAX;
    B := ( HueToRGB( Magic1, Magic2, Hue  - ( HLSMAX div 3 ) ) * RGBMAX + ( HLSMAX div 2) ) div HLSMAX;
  end;
  Result :=  RGB(R,G,B);
end;


// Following function GetColorBetween is commented out since the assembly line
// 'call CalcColorBytes' caused an access violation in on variable F. (a.schmitz@uib.de)
(*
function TQProgressBar.GetColorBetween(StartColor, EndColor: TColor; Pointvalue,
                                       Von, Bis : Extended): TColor;                 // NIH
{$IFDEF CPU64}
begin
  result :=  StartColor;
end;
{$ELSE CPU64}
// Found on efg's colors pages, at http://homepages.borland.com/efg2lab/Library/Delphi/Graphics/Color.htm
// "Color gradient" row, cworn's UseNet Post.
// Author is unknown, but remains holder for intellectual property.
// High speed function which returns the gradient color value for a pixel depending
// on start and final color, size of the gradient area , and the place of the current pixel;

var F: Extended; r1, r2, r3, g1, g2, g3, b1, b2, b3: Byte;
  function CalcColorBytes(fb1, fb2: Byte): Byte;
  begin
    result := fb1;
    if fb1 < fb2 then Result := FB1 + Trunc(F * (fb2 - fb1));
    if fb1 > fb2 then Result := FB1 - Trunc(F * (fb1 - fb2));
  end;
begin
  If (fMonoClr) Or (Pointvalue <= Von) then begin
    result := StartColor;
    exit;
  end;
  if Pointvalue >= Bis then begin
    result := EndColor;
    exit;
  end;
  F := (Pointvalue - von) / (Bis - Von);
  asm
     mov EAX, Startcolor
     cmp EAX, EndColor
     je @@exit
     mov r1, AL
     shr EAX,8
     mov g1, AL
     shr Eax,8
     mov b1, AL
     mov Eax, Endcolor
     mov r2, AL
     shr EAX,8
     mov g2, AL
     shr EAX,8
     mov b2, AL
     push ebp
     mov al, r1
     mov dl, r2
     call CalcColorBytes
     pop ecx
     push ebp
     Mov r3, al
     mov dL, g2
     mov al, g1
     call CalcColorBytes
     pop ecx
     push ebp
     mov g3, Al
     mov dL, B2
     mov Al, B1
     call CalcColorBytes
     pop ecx
     mov b3, al
     XOR EAX,EAX
     mov AL, B3
     SHL EAX,8
     mov AL, G3
     SHL EAX,8
     mov AL, R3
@@Exit:
     mov @result, eax

  end;
End;
{$ENDIF CPU64}
*)

procedure TQProgressBar.Paint;
// Main loop. Called each time a setting changes, notably, each time
// a new position is sent.
// Surround is drawn first, then the bar itself. Caption is added lastly (if needed).

var i,k,sp: Integer;
    OldBkMode : integer;
Begin
  // -1- Bevel
  If Self.fHasShape Then With Canvas Do
  Begin
    Pen.Width   := 1;
    Brush.Style := bsSolid;
    Brush.Color := fBkgClr;
    Pen.Color   := fShapeClr;
    RoundRect (0, 0, Width, Height, fCorner, fCorner);
  End;

  // -2- The bar itself
  Case Self.fOrientation of
    boHorizontal :
      Begin
        For i := (fBorderSize -1) To fPosition  Do
        Begin
          If (fByBlock) Then
          Begin
            If (fPosDescr[i].isInBlock = True) Then
            Begin
              If  ( (fFullBlock) And (fPosition >= fPosDescr[i].blkLimit) )
              Or Not(fFullBlock)
              Then For k := (fBorderSize -1) To (self.Height -(fBorderSize))
                       Do Canvas.Pixels [i,k] := Self.fPixDescr[i,k]
              Else If fShowInactPos Then
                      For k := (fBorderSize -1) To (self.Height -(fBorderSize))
                          Do Canvas.Pixels [i,k] := fInactDescr[k];
            End;
          End Else
          Begin
            For k := (fBorderSize -1) To (self.Height -(fBorderSize))  Do
                Canvas.Pixels [i,k] := fPixDescr[i,k];
          End;
        End;
        // Now dealing with inactive positions, if they're to be drawn.
        If fShowInactPos Then
        Begin
          If Self.fPosition < 3 Then sp := 3 Else sp := fPosition + 1;
          For i := sp To fUSefullDrawSpace Do
          Begin
            If (fByBlock) Then
            Begin
              If (fPosDescr[i].isInBlock = True) Then
              Begin
                 For k := (fBorderSize -1) To (self.Height -(fBorderSize)) Do
                       Canvas.Pixels [i,k] := fInactDescr[k];
              End;
            End Else  //If not(byBlock), all pixels must be drawn
            Begin
              For k := (fBorderSize -1) To (self.Height -(fBorderSize))  Do
                  Canvas.Pixels [i,k] := fInactDescr[k];
            End;
          End; {for}
        End; {inactive}
      End; {boHorizontal}
    boVertical :
      Begin
        For i := (fUSefullDrawSpace-1) DownTo Self.height - fPosition  Do
        Begin
          If (fByBlock) Then
          Begin
            If (fPosDescr[i].isInBlock = True) Then
            Begin
              If  ( (fFullBlock) And ((Self.height - fPosition) <= fPosDescr[i].blkLimit) )
              Or Not(fFullBlock)
              Then For k := (fBorderSize -1) To (self.Width -(fBorderSize))
                       Do Canvas.Pixels [k,i] := Self.fPixDescr[i,k]
              Else If fShowInactPos Then
                       For k := (fBorderSize -1) To (self.Width -(fBorderSize))
                           Do Canvas.Pixels [k,i] := fInactDescr[k];
            End;
          End Else For k := (fBorderSize -1) To (self.Width -(fBorderSize))
                       Do Canvas.Pixels [k,i] := fPixDescr[i,k];
        End;
        // inactive positions :
        If fShowInactPos Then
        Begin
          If Self.fPosition < 3 Then sp := Self.fUSefullDrawSpace Else sp := Self.height - fPosition - 1;
          For i := sp DownTo fBorderSize Do
          Begin
            If (fByBlock) Then
            Begin
              If (fPosDescr[i].isInBlock = True) Then
              Begin
                 For k := (fBorderSize -1) To (self.Width -(fBorderSize)) Do
                     Canvas.Pixels [k,i] := fInactDescr[k];
              End;
            End Else
              For k := (fBorderSize -1) To (self.Width -(fBorderSize))
                  Do Canvas.Pixels [k,i] := fInactDescr[k];
          End; {for... downto}
        End; {inactive}
      End; {boVertical}
    End; // Case

    // caption management. The font is the canvas' one. Can be overrided
    // using the Font property :
    If fCaptionOvr Then
       If fShowPosAsPct Then SetCaption(FloatToStr(fUSerPosPct) + '%')
       Else SetCaption(IntToStr(fUSerPos));
    If fHasCaption Then
    Begin
      OldBkMode := SetBkMode(Canvas.Handle,TRANSPARENT);
      With Canvas Do
      Begin
        TextOut(fCapPos.X, fCapPos.Y, fCaption);
      End;
      SetBkMode(Canvas.Handle,OldBkMode);
    End;
End;


constructor TQProgressBar.Create (AOwner : TComponent);
begin
  Inherited Create (AOwner);
  fFont          := TFont.Create;
  SetLength(fPosDescr,1);
  fPosDescr[0].IsInBlock := False;
  fByBlock       := False;
  fFullBlock     := False;
  fBlockSize     := 0;
  fSpaceSize     := 0;
  fOrientation   := boHorizontal;
  fBarKind       := bkFlat;
  fBarLook       := blMetal;
  DoubleBuffered := True;
  fPosition      := 0;
  Width          := 64;
  Height         := 16;
  fHasShape      := True;
  fShapeClr      := RGB (0, 60, 116);
  fStartClr      := clLime;
  fFinalClr      := clLime;
  fMonoClr       := True;
  fBkgClr        := clWhite;
  fShowInactPos  := False;
  fInactPosClr   := clGray;
  fInvInactPos   := False;
  fMaxPos        := 100;
  fInternalBorder:= 2;
  fBorderSize    := 4;
  SetUsefullWidth;
  InitPixArray;
  fCorner        := 5;
  fCaption       := '';
  fCapPos.X      := 0;
  fCapPos.Y      := 0;
  fHasCaption    := False;
  fCaptionOvr    := False;
  fHintOvr       := False;
  fShowPosAsPct  := False;
  Enabled        := True;
  TabStop        := False;
  invalidate;
  If csDesigning in ComponentState
     Then self.position := 50
     Else self.position := 0;  // Will compute fPosition
end;


destructor TQProgressBar.Destroy;
begin
  fFont.Free;
  SetLength(fPosDescr,0);
  SetLength(fPixDescr,0);
  inherited;
end;


Procedure TQProgressBar.LockInits;
Begin
  self.fInitsLocked := True;
End;


Procedure TQProgressBar.UnlockInits;
Begin
  Self.fInitsLocked := False;
  InitPixArray;
End;


procedure TQProgressBar.Resize;
Begin
  inherited Resize;
  fBorderSize := fInternalBorder SHL 1;
  SetUsefullWidth;

  If self.fInitsLocked Then exit;

  If fByBlock   Then InitBlockArray;
  InitPixArray;
  position := fUserPos;                   // position  is computed, then bar is invalidated ;
End;


procedure TQProgressBar.SetUsefullWidth;
Begin
  Case Self.fOrientation of
    boHorizontal : fUSefullDrawSpace := (Self.Width  - ( fBorderSize ));
    boVertical   : fUSefullDrawSpace := (Self.Height - ( fBorderSize ));
  End;
  fMinVisPos := fBorderSize + 1;
End;


procedure TQProgressBar.SetFCorner(isRounded:Boolean);
Begin
  If isRounded Then Self.fCorner := 5
               Else Self.fCorner := 0;
  Invalidate;
End;


function TQProgressBar.GetBoolCorner : Boolean;
Begin
  result := Self.fCorner > 0;
End;


procedure TQProgressBar.SetBarKind(value:TQBarKind);
Begin
  Self.fBarKind := value;
  InitPixArray;
  Invalidate;
End;


procedure TQProgressBar.SetBarLook(value:TQBarLook);
Begin
  self.fBarLook := value;
  InitPixArray;
  Invalidate;
End;


procedure TQProgressBar.SetOrientation(value:TQBarOrientation);
var tmpClr:TColor;
    newH,
    newW:Integer;
Begin
  LockInits;
  If value <> Self.fOrientation Then
  Begin
    If ( (value = boVertical)   and (Self.Height < Self.width)  )
    Or ( (value = boHorizontal) and (Self.width  < Self.height) )
    Then begin
       newW := Self.Height;
       newH := Self.Width;
       Self.Height := newH;
       Self.Width  := newW;
    End;
    fOrientation := value;
    If  (csDesigning in componentState) then
     Begin
      tmpClr    := fStartClr;
      fStartClr := fFinalClr;
      fFinalClr := tmpClr;
    End;
  End;
  Case value of
    boHorizontal : If Self.Height < 10
                      Then Self.fInternalBorder := 1
                      Else Self.fInternalBorder := 2;
    boVertical   : If Self.Width  < 10
                      Then Self.fInternalBorder := 1
                      Else Self.fInternalBorder := 2;
  End; //Case
  fBorderSize := fInternalBorder SHL 1;
  SetUsefullWidth;
  Self.fInitsLocked := False;
  InitBlockArray;
  InitPixArray;
  Invalidate;
End;


procedure TQProgressBar.SetBkgColor(aColor:TColor);
Begin
  Self.fBkgClr := aColor;
  Invalidate;
End;


procedure TQProgressBar.SetShape(value:Boolean);
Begin
  Self.fHasShape := value;
  Invalidate;
End;


procedure TQProgressBar.SetShapeColor(value:TColor);
Begin
  Self.fShapeClr := value;
  Invalidate;
End;


procedure TQProgressBar.SetBlockSize(value:Integer);
Begin
  Case Self.fOrientation of
    boHorizontal : If value > Self.Width  - (fInternalBorder SHL 1) Then Exit;
    boVertical   : If value > Self.Height - (fInternalBorder SHL 1) Then Exit;
  End;{case}

  fBlockSize := Abs(value);
  fByBlock   := (fBlockSize > 0) And (fSpaceSize > 0);
  If fByBlock   Then InitBlockArray;
  InitPixArray;
  Invalidate;
end;


procedure TQProgressBar.SetSpaceSize(value:Integer);
Begin
  Case Self.fOrientation of
    boHorizontal : If value > Self.Width  - (fInternalBorder SHL 1) Then Exit;
    boVertical   : If value > Self.Height - (fInternalBorder SHL 1) Then Exit;
  End;{case}

  fSpaceSize := Abs(value);
  fByBlock   := (fBlockSize > 0) And (fSpaceSize > 0);
  If fByBlock   Then InitBlockArray;
  InitPixArray;
  Invalidate;
end;


procedure TQProgressBar.SetFullBlock(value:Boolean);
Begin
  Self.fFullBlock := value;
  If value Then InitBlockArray;
  InitPixArray;
  Invalidate;
end;


procedure TQProgressBar.SetMaxPos(value:Integer);
Begin
  If value < 0 Then self.fMaxPos := 0
  Else self.fMaxPos := value;
  Invalidate;
end;


procedure TQProgressBar.SetPosition(value:Integer);
var tmpfPos : real;
Begin
  fUserPos := value;
  If fMaxPos = 0 Then exit;
  TRY
    If (value <= 0) Then
    Begin
      self.fPosition := 0;
      Exit;
    End Else If value > fMaxPos Then value := fMaxPos;

    fUSerPosPct := (100 * value)/fMaxPos;
    tmpfPos := fUsefullDrawSpace * fUSerPosPct / 100;
    // If value( user position) > 0, make sure that at least one bar is visible
    If (tmpfPos > 0.00) and (tmpfPos < fMinVisPos )
       Then self.fPosition := fMinVisPos
       Else If tmpfPos  > fUsefullDrawSpace
            Then self.fPosition := fUsefullDrawSpace
            Else self.fPosition := Round( tmpfPos );
    // Hint is managed here (whereas caption, which ahs to be painted,
    // is managed in the paint() proc).
    If fHintOvr Then
       If fShowPosAsPct Then Hint := FloatToStr(fUSerPosPct) + ' %'
       Else Hint := IntToStr(fUSerPos);
  FINALLY
    Invalidate;
    If (fHideOnTerm) and (value = fMaxPos) Then Begin Sleep(100); Hide; End;
  END;
end;


procedure TQProgressBar.SetStartClr(value:TColor);
Begin
  Self.fStartClr :=  value;
  Self.fMonoClr  := (fStartClr = fFinalClr);
  InitPixArray;
  Invalidate;
End;


procedure TQProgressBar.SetFinalClr(value:TColor);
Begin
  Self.fFinalClr :=  value;
  Self.fMonoClr  := (fStartClr = fFinalClr);
  InitPixArray;
  Invalidate;
End;


procedure TQProgressBar.SetBothColors(value:TColor);
Begin
  Self.fMonoClr  := True;
  Self.fStartClr := value;
  Self.fFinalClr := value;
  InitPixArray;
  Invalidate;
End;


procedure TQProgressBar.SetInactivePos(value:Boolean);
Begin
  Self.fShowInactPos :=  value;
  InitPixArray;
  Invalidate;
End;


procedure TQProgressBar.SetInactPosClr(value:TColor);
Begin
  Self.fInactPosClr :=  value;
  InitPixArray;
  Invalidate;
End;


procedure TQProgressBar.SetInvInactPos(value:Boolean);
// invert Inactive Positions lum.
Begin
  Self.fInvInactPos := value;
  InitPixArray;
  Invalidate;
End;


procedure TQProgressBar.SetCaption(value:String);
Begin
  Self.fCaption := Value;
  fHasCaption := Not(Value = '');
  If fHasCaption Then
  Begin
    //-1- Centering vertically
    fCapPos.Y := ( Height - Canvas.textHeight('Pg')) Div 2 ;

    Case fCapAlign Of
      taLeftJustify :
            Begin
              fCapPos.X := 0;
            End;
      taCenter :
            Begin
              fCapPos.X := ( Width - Canvas.textWidth(value) ) Div 2;
            End;
      Else  Begin  //right alignment;
              fCapPos.X := ( Width - Canvas.textWidth(value) ) -1 ;
            End;
    End;{case}
  End;
End;


procedure TQProgressBar.SetCapAlign(value:TAlignment);
Begin
  Self.fCapAlign := Value;
  SetCaption(fCaption);              //forces the computing of top and left, based on current caption;
End;


procedure TQProgressBar.SetCaptionOvr(value:Boolean);
Begin
  Self.fCaptionOvr := value;
  Invalidate;
end;


procedure TQProgressBar.SetFont(value:TFont);
Begin
  TRY self.Canvas.Font.Assign(value);
  EXCEPT; END;
End;


end.
