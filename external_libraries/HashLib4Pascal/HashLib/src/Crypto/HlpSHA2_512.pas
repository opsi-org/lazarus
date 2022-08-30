unit HlpSHA2_512;

{$I ..\Include\HashLib.inc}

interface

uses
{$IFDEF DELPHI2010}
  SysUtils, // to get rid of compiler hint "not inlined" on Delphi 2010.
{$ENDIF DELPHI2010}
  HlpHashLibTypes,
{$IFDEF DELPHI}
  HlpHashBuffer,
  HlpHash,
{$ENDIF DELPHI}
  HlpIHash,
  HlpSHA2_512Base,
  HlpConverters;

type
  TSHA2_512 = class sealed(TSHA2_512Base)

  strict protected
    function GetResult(): THashLibByteArray; override;

  public
    constructor Create();
    procedure Initialize(); override;
    function Clone(): IHash; override;

  end;

implementation

{ TSHA2_512 }

function TSHA2_512.Clone(): IHash;
var
  LHashInstance: TSHA2_512;
begin
  LHashInstance := TSHA2_512.Create();
  LHashInstance.FState := System.Copy(FState);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TSHA2_512.Create;
begin
  Inherited Create(64);
end;

function TSHA2_512.GetResult: THashLibByteArray;
begin
  System.SetLength(result, 8 * System.SizeOf(UInt64));
  TConverters.be64_copy(PUInt64(FState), 0, PByte(result), 0,
    System.Length(result));
end;

procedure TSHA2_512.Initialize;
begin
  FState[0] := $6A09E667F3BCC908;
  FState[1] := UInt64($BB67AE8584CAA73B);
  FState[2] := $3C6EF372FE94F82B;
  FState[3] := UInt64($A54FF53A5F1D36F1);
  FState[4] := $510E527FADE682D1;
  FState[5] := UInt64($9B05688C2B3E6C1F);
  FState[6] := $1F83D9ABFB41BD6B;
  FState[7] := $5BE0CD19137E2179;
  Inherited Initialize();
end;

end.
