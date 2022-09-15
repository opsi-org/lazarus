{
    Copyright (c) uib GmbH 2022 by Jinene Laajili
    Unit for hashing functions using HashLib4Pascal library
}

unit oshash;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, HlpHashFactory;

function HashFromFile(filename: string; algo : string): string;

implementation


function HashFromFile(filename: string; algo : string): string;
var
  Source: TFileStream;
  HashResult: string;
begin
  Result := '';
  Source := nil;
  Source := TFileStream.Create(filename, fmOpenRead);
  if Source <> nil then
  begin
    case algo of
      'Gost' :
      HashResult := THashFactory.TCrypto.CreateGost().ComputeStream(Source, Source.Size).ToString();
      'Grindahl256' :
      HashResult := THashFactory.TCrypto.CreateGrindahl256().ComputeStream(Source, Source.Size).ToString();
      'Grindahl512' :
      HashResult := THashFactory.TCrypto.CreateGrindahl512().ComputeStream(Source, Source.Size).ToString();
      'HAS160' :
      HashResult := THashFactory.TCrypto.CreateHAS160().ComputeStream(Source, Source.Size).ToString();
      'Haval_3_128' :
      HashResult := THashFactory.TCrypto.CreateHaval_3_128().ComputeStream(Source, Source.Size).ToString();
      'Haval_4_128' :
      HashResult := THashFactory.TCrypto.CreateHaval_4_128().ComputeStream(Source, Source.Size).ToString();
      'Haval_5_128' :
      HashResult := THashFactory.TCrypto.CreateHaval_5_128().ComputeStream(Source, Source.Size).ToString();
      'Haval_3_160' :
      HashResult := THashFactory.TCrypto.CreateHaval_3_160().ComputeStream(Source, Source.Size).ToString();
      'Haval_4_160' :
      HashResult := THashFactory.TCrypto.CreateHaval_4_160().ComputeStream(Source, Source.Size).ToString();
      'Haval_5_160' :
      HashResult := THashFactory.TCrypto.CreateHaval_5_160().ComputeStream(Source, Source.Size).ToString();
      'Haval_3_192' :
      HashResult := THashFactory.TCrypto.CreateHaval_3_192().ComputeStream(Source, Source.Size).ToString();
      'Haval_4_192' :
      HashResult := THashFactory.TCrypto.CreateHaval_4_192().ComputeStream(Source, Source.Size).ToString();
      'Haval_5_192' :
      HashResult := THashFactory.TCrypto.CreateHaval_5_192().ComputeStream(Source, Source.Size).ToString();
      'Haval_3_224' :
      HashResult := THashFactory.TCrypto.CreateHaval_3_224().ComputeStream(Source, Source.Size).ToString();
      'Haval_4_224' :
      HashResult := THashFactory.TCrypto.CreateHaval_4_224().ComputeStream(Source, Source.Size).ToString();
      'Haval_5_224' :
      HashResult := THashFactory.TCrypto.CreateHaval_5_224().ComputeStream(Source, Source.Size).ToString();
      'Haval_3_256' :
      HashResult := THashFactory.TCrypto.CreateHaval_3_256().ComputeStream(Source, Source.Size).ToString();
      'Haval_4_256' :
      HashResult := THashFactory.TCrypto.CreateHaval_4_256().ComputeStream(Source, Source.Size).ToString();
      'Haval_5_256' :
      HashResult := THashFactory.TCrypto.CreateHaval_5_256().ComputeStream(Source, Source.Size).ToString();
      'MD2' :
      HashResult := THashFactory.TCrypto.CreateMD2().ComputeStream(Source, Source.Size).ToString();
      'MD4' :
      HashResult := THashFactory.TCrypto.CreateMD4().ComputeStream(Source, Source.Size).ToString();
      'MD5' :
      HashResult := THashFactory.TCrypto.CreateMD5().ComputeStream(Source, Source.Size).ToString();
      'Panama' :
      HashResult := THashFactory.TCrypto.CreatePanama().ComputeStream(Source, Source.Size).ToString();
      'RadioGatun32' :
      HashResult := THashFactory.TCrypto.CreateRadioGatun32().ComputeStream(Source, Source.Size).ToString();
      'RadioGatun64' :
      HashResult := THashFactory.TCrypto.CreateRadioGatun64().ComputeStream(Source, Source.Size).ToString();
      'RIPEMD' :
      HashResult := THashFactory.TCrypto.CreateRIPEMD().ComputeStream(Source, Source.Size).ToString();
      'RIPEMD128' :
      HashResult := THashFactory.TCrypto.CreateRIPEMD128().ComputeStream(Source, Source.Size).ToString();
      'RIPEMD160' :
      HashResult := THashFactory.TCrypto.CreateRIPEMD160().ComputeStream(Source, Source.Size).ToString();
      'RIPEMD256' :
      HashResult := THashFactory.TCrypto.CreateRIPEMD256().ComputeStream(Source, Source.Size).ToString();
      'RIPEMD320' :
      HashResult := THashFactory.TCrypto.CreateRIPEMD320().ComputeStream(Source, Source.Size).ToString();
      'SHA0' :
      HashResult := THashFactory.TCrypto.CreateSHA0().ComputeStream(Source, Source.Size).ToString();
      'SHA1' :
      HashResult := THashFactory.TCrypto.CreateSHA1().ComputeStream(Source, Source.Size).ToString();
      'SHA2_244' :
      HashResult := THashFactory.TCrypto.CreateSHA2_224().ComputeStream(Source, Source.Size).ToString();
      'SHA2_256' :
      HashResult := THashFactory.TCrypto.CreateSHA2_256().ComputeStream(Source, Source.Size).ToString();
      'SHA2_384' :
      HashResult := THashFactory.TCrypto.CreateSHA2_384().ComputeStream(Source, Source.Size).ToString();
      'SHA2_512' :
      HashResult := THashFactory.TCrypto.CreateSHA2_512().ComputeStream(Source, Source.Size).ToString();
      'SHA2_512_224' :
      HashResult := THashFactory.TCrypto.CreateSHA2_512_224().ComputeStream(Source, Source.Size).ToString();
      'SHA2_512_256' :
      HashResult := THashFactory.TCrypto.CreateSHA2_512_256().ComputeStream(Source, Source.Size).ToString();
      'SHA3_224' :
      HashResult := THashFactory.TCrypto.CreateSHA3_224().ComputeStream(Source, Source.Size).ToString();
      'SHA3_256' :
      HashResult := THashFactory.TCrypto.CreateSHA3_256().ComputeStream(Source, Source.Size).ToString();
      'SHA3_384' :
      HashResult := THashFactory.TCrypto.CreateSHA3_384().ComputeStream(Source, Source.Size).ToString();
      'SHA3_512' :
      HashResult := THashFactory.TCrypto.CreateSHA3_512().ComputeStream(Source, Source.Size).ToString();
      'Shake_128' :
      HashResult := THashFactory.TXOF.CreateShake_128(128).ComputeStream(Source, Source.Size).ToString();
      'Shake_256' :
      HashResult := THashFactory.TXOF.CreateShake_256(256).ComputeStream(Source, Source.Size).ToString();
      'Snefru_8_128' :
      HashResult := THashFactory.TCrypto.CreateSnefru_8_128().ComputeStream(Source, Source.Size).ToString();
      'Snefru_8_256' :
      HashResult := THashFactory.TCrypto.CreateSnefru_8_256().ComputeStream(Source, Source.Size).ToString();
      'Tiger_3_128' :
      HashResult := THashFactory.TCrypto.CreateTiger_3_128().ComputeStream(Source, Source.Size).ToString();
      'Tiger_4_128' :
      HashResult := THashFactory.TCrypto.CreateTiger_4_128().ComputeStream(Source, Source.Size).ToString();
      'Tiger_5_128' :
      HashResult := THashFactory.TCrypto.CreateTiger_5_128().ComputeStream(Source, Source.Size).ToString();
      'Tiger_3_160' :
      HashResult := THashFactory.TCrypto.CreateTiger_3_160().ComputeStream(Source, Source.Size).ToString();
      'Tiger_4_160' :
      HashResult := THashFactory.TCrypto.CreateTiger_4_160().ComputeStream(Source, Source.Size).ToString();
      'Tiger_5_160' :
      HashResult := THashFactory.TCrypto.CreateTiger_5_160().ComputeStream(Source, Source.Size).ToString();
      'Tiger_3_192' :
      HashResult := THashFactory.TCrypto.CreateTiger_3_192().ComputeStream(Source, Source.Size).ToString();
      'Tiger_4_192' :
      HashResult := THashFactory.TCrypto.CreateTiger_4_192().ComputeStream(Source, Source.Size).ToString();
      'Tiger_5_192' :
      HashResult := THashFactory.TCrypto.CreateTiger_5_192().ComputeStream(Source, Source.Size).ToString();
      'Tiger2_3_128' :
      HashResult := THashFactory.TCrypto.CreateTiger2_3_128().ComputeStream(Source, Source.Size).ToString();
      'Tiger2_4_128' :
      HashResult := THashFactory.TCrypto.CreateTiger2_4_128().ComputeStream(Source, Source.Size).ToString();
      'Tiger2_5_128' :
      HashResult := THashFactory.TCrypto.CreateTiger2_5_128().ComputeStream(Source, Source.Size).ToString();
      'Tiger2_3_160' :
      HashResult := THashFactory.TCrypto.CreateTiger2_3_160().ComputeStream(Source, Source.Size).ToString();
      'Tiger2_4_160' :
      HashResult := THashFactory.TCrypto.CreateTiger2_4_160().ComputeStream(Source, Source.Size).ToString();
      'Tiger2_5_160' :
      HashResult := THashFactory.TCrypto.CreateTiger2_5_160().ComputeStream(Source, Source.Size).ToString();
      'Tiger2_3_192' :
      HashResult := THashFactory.TCrypto.CreateTiger2_3_192().ComputeStream(Source, Source.Size).ToString();
      'Tiger2_4_192' :
      HashResult := THashFactory.TCrypto.CreateTiger2_4_192().ComputeStream(Source, Source.Size).ToString();
      'Tiger2_5_192' :
      HashResult := THashFactory.TCrypto.CreateTiger2_5_192().ComputeStream(Source, Source.Size).ToString();
      'WhirlPool' :
      HashResult := THashFactory.TCrypto.CreateWhirlPool().ComputeStream(Source, Source.Size).ToString();
      'Blake2B' :
      HashResult := THashFactory.TCrypto.CreateBlake2B().ComputeStream(Source, Source.Size).ToString();
      'Blake2S' :
      HashResult := THashFactory.TCrypto.CreateBlake2S().ComputeStream(Source, Source.Size).ToString();
      'Keccak_224' :
      HashResult := THashFactory.TCrypto.CreateKeccak_224().ComputeStream(Source, Source.Size).ToString();
      'Keccak_256' :
      HashResult := THashFactory.TCrypto.CreateKeccak_256().ComputeStream(Source, Source.Size).ToString();
      'Keccak_288' :
      HashResult := THashFactory.TCrypto.CreateKeccak_288().ComputeStream(Source, Source.Size).ToString();
      'Keccak_384' :
      HashResult := THashFactory.TCrypto.CreateKeccak_384().ComputeStream(Source, Source.Size).ToString();
      'Keccak_512' :
      HashResult := THashFactory.TCrypto.CreateKeccak_512().ComputeStream(Source, Source.Size).ToString();
      'GOST3411_2012_256' :
      HashResult := THashFactory.TCrypto.CreateGOST3411_2012_256().ComputeStream(Source, Source.Size).ToString();
      'GOST3411_2012_512' :
      HashResult := THashFactory.TCrypto.CreateGOST3411_2012_512().ComputeStream(Source, Source.Size).ToString();
      'Blake2XS' :
      HashResult := THashFactory.TXOF.CreateBlake2XS(Nil, 256).ComputeStream(Source, Source.Size).ToString();
      'Blake2XB' :
      HashResult := THashFactory.TXOF.CreateBlake2XB(Nil, 512).ComputeStream(Source, Source.Size).ToString();
      'Blake3_256' :
      HashResult := THashFactory.TCrypto.CreateBlake3_256(Nil).ComputeStream(Source, Source.Size).ToString();
      'Blake3XOF' :
      HashResult := THashFactory.TXOF.CreateBlake3XOF(Nil, 512).ComputeStream(Source, Source.Size).ToString();
      'Blake2BP' :
      HashResult := THashFactory.TCrypto.CreateBlake2BP(64, Nil).ComputeStream(Source, Source.Size).ToString();
      'Blake2SP' :
      HashResult := THashFactory.TCrypto.CreateBlake2SP(32, Nil).ComputeStream(Source, Source.Size).ToString();
     else
      HashResult := '';
     end;
  end;
  Result :=  HashResult;
  FreeAndNil(Source);
end;

end.

