unit GR32_PNG;

interface

uses
  SysUtils, Windows, Classes, Graphics, Gr32;

procedure LoadPNGintoBitmap32(DstBitmap: TBitmap32; Filename: String; out AlphaChannelUsed: Boolean); overload;
procedure LoadPNGintoBitmap32(DstBitmap: TBitmap32; SrcStream: TStream; out AlphaChannelUsed: Boolean); overload;

implementation

uses PNGImage;

procedure LoadPNGintoBitmap32(DstBitmap: TBitmap32; SrcStream: TStream; out AlphaChannelUsed: Boolean);
var
  PNGObject: TPNGObject;
  TransparentColor: TColor32;
  PixelPtr: PColor32;
  AlphaPtr: PByte;
  X, Y: Integer;
begin
  PNGObject := nil;
  try
    PNGObject := TPngObject.Create;
    PNGObject.LoadFromStream(SrcStream);

    DstBitmap.Assign(PNGObject);
    DstBitmap.ResetAlpha;

    case PNGObject.TransparencyMode of
      ptmPartial:
        begin
          if (PNGObject.Header.ColorType = COLOR_GRAYSCALEALPHA) or
             (PNGObject.Header.ColorType = COLOR_RGBALPHA) then
          begin
            PixelPtr := PColor32(@DstBitmap.Bits[0]);
            for Y := 0 to DstBitmap.Height - 1 do
            begin
              AlphaPtr := PByte(PNGObject.AlphaScanline[Y]);
              for X := 0 to DstBitmap.Width - 1 do
              begin
                PixelPtr^ := (PixelPtr^ and $00FFFFFF) or (TColor32(AlphaPtr^) shl 24);
                Inc(PixelPtr);
                Inc(AlphaPtr);
              end;
            end;

            AlphaChannelUsed := True;
          end;
        end;
      ptmBit:
        begin
          TransparentColor := Color32(PNGObject.TransparentColor);
          PixelPtr := PColor32(@DstBitmap.Bits[0]);
          for X := 0 to (DstBitmap.Height - 1) * (DstBitmap.Width - 1) do
          begin
            if PixelPtr^ = TransparentColor then
              PixelPtr^ := PixelPtr^ and $00FFFFFF;
            Inc(PixelPtr);
          end;

          AlphaChannelUsed := True;
        end;
      ptmNone:
        AlphaChannelUsed := False;
    end;
  finally
    if Assigned(PNGObject) then PNGObject.Free;
  end;
end;

procedure LoadPNGintoBitmap32(DstBitmap: TBitmap32; Filename: String; out AlphaChannelUsed: Boolean);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(Filename, fmOpenRead);
  try
    LoadPNGintoBitmap32(DstBitmap, FileStream, AlphaChannelUsed);
  finally
    FileStream.Free;
  end;
end;

end.
