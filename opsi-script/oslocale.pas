// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: Rupert Roeder, detlef oertel


// locale function in opsi-script


//http://msdn.microsoft.com/en-us/library/cc233968.aspx
//http://msdn.microsoft.com/en-us/library/0h88fahh.aspx
//bcp 47 validator:
//http://schneegans.de/lv/?tags=de-de-1996&format=text
//http://www.iana.org/assignments/language-subtag-registry
//http://www.the-localization-tool.com/?p=698


unit oslocale;

{$MODE Delphi}
{$RANGECHECKS ON}
{$OVERFLOWCHECKS ON}
{$IOCHECKS ON}
{$OBJECTCHECKS ON}
{$VARSTRINGCHECKS ON}
{$LONGSTRINGS ON}



interface

uses
  SysUtils, Classes;

//type

//const

function getlangcodeByHexvalueStr(const hexvalue: string): string;

var

  langCodeHash: TStringList;




implementation

procedure createLangcodeHash;
begin
  langCodeHash := TStringList.Create;

  langCodeHash.Append('0x0000=xx-Null');
  langCodeHash.Append('0x0001=ar');
  langCodeHash.Append('0x0002=bg');
  langCodeHash.Append('0x0003=ca');
  langCodeHash.Append('0x0004=zh-Hans');
  langCodeHash.Append('0x7c04=zh-Hant');
  langCodeHash.Append('0x0005=cs');
  langCodeHash.Append('0x0006=da');
  langCodeHash.Append('0x0007=de');
  langCodeHash.Append('0x0008=el');
  langCodeHash.Append('0x0009=en');
  langCodeHash.Append('0x000a=es');
  langCodeHash.Append('0x000b=fi');
  langCodeHash.Append('0x000c=fr');
  langCodeHash.Append('0x000d=he');
  langCodeHash.Append('0x000e=hu');
  langCodeHash.Append('0x000f=is');
  langCodeHash.Append('0x0010=it');
  langCodeHash.Append('0x0011=ja');
  langCodeHash.Append('0x0012=ko');
  langCodeHash.Append('0x0013=nl');
  langCodeHash.Append('0x0014=no');
  langCodeHash.Append('0x0015=pl');
  langCodeHash.Append('0x0016=pt');
  langCodeHash.Append('0x0017=rm');
  langCodeHash.Append('0x0018=ro');
  langCodeHash.Append('0x0019=ru');
  langCodeHash.Append('0x001a=hr');
  langCodeHash.Append('0x001b=sk');
  langCodeHash.Append('0x001c=sq');
  langCodeHash.Append('0x001d=sv');
  langCodeHash.Append('0x001e=th');
  langCodeHash.Append('0x001f=tr');
  langCodeHash.Append('0x0020=ur');
  langCodeHash.Append('0x0021=id');
  langCodeHash.Append('0x0022=uk');
  langCodeHash.Append('0x0023=be');
  langCodeHash.Append('0x0024=sl');
  langCodeHash.Append('0x0025=et');
  langCodeHash.Append('0x0026=lv');
  langCodeHash.Append('0x0027=lt');
  langCodeHash.Append('0x0028=tg');
  langCodeHash.Append('0x0029=fa');
  langCodeHash.Append('0x002a=vi');
  langCodeHash.Append('0x002b=hy');
  langCodeHash.Append('0x002c=az');
  langCodeHash.Append('0x002d=eu');
  langCodeHash.Append('0x002e=hsb');
  langCodeHash.Append('0x002f=mk');
  langCodeHash.Append('0x0032=tn');
  langCodeHash.Append('0x0034=xh');
  langCodeHash.Append('0x0035=zu');
  langCodeHash.Append('0x0036=af');
  langCodeHash.Append('0x0037=ka');
  langCodeHash.Append('0x0038=fo');
  langCodeHash.Append('0x0039=hi');
  langCodeHash.Append('0x003a=mt');
  langCodeHash.Append('0x003b=se');
  langCodeHash.Append('0x003c=ga');
  langCodeHash.Append('0x003e=ms');
  langCodeHash.Append('0x003f=kk');
  langCodeHash.Append('0x0040=ky');
  langCodeHash.Append('0x0041=sw');
  langCodeHash.Append('0x0042=tk');
  langCodeHash.Append('0x0043=uz');
  langCodeHash.Append('0x0044=tt');
  langCodeHash.Append('0x0045=bn');
  langCodeHash.Append('0x0046=pa');
  langCodeHash.Append('0x0047=gu');
  langCodeHash.Append('0x0048=or');
  langCodeHash.Append('0x0049=ta');
  langCodeHash.Append('0x004a=te');
  langCodeHash.Append('0x004b=kn');
  langCodeHash.Append('0x004c=ml');
  langCodeHash.Append('0x004d=as');
  langCodeHash.Append('0x004e=mr');
  langCodeHash.Append('0x004f=sa');
  langCodeHash.Append('0x0050=mn');
  langCodeHash.Append('0x0051=bo');
  langCodeHash.Append('0x0052=cy');
  langCodeHash.Append('0x0053=km');
  langCodeHash.Append('0x0054=lo');
  langCodeHash.Append('0x0056=gl');
  langCodeHash.Append('0x0057=kok');
  langCodeHash.Append('0x005a=syr');
  langCodeHash.Append('0x005b=si');
  langCodeHash.Append('0x005d=iu');
  langCodeHash.Append('0x005e=am');
  langCodeHash.Append('0x005f=tzm');
  langCodeHash.Append('0x0061=ne');
  langCodeHash.Append('0x0062=fy');
  langCodeHash.Append('0x0063=ps');
  langCodeHash.Append('0x0064=fil');
  langCodeHash.Append('0x0065=dv');
  langCodeHash.Append('0x0068=ha');
  langCodeHash.Append('0x006a=yo');
  langCodeHash.Append('0x006b=quz');
  langCodeHash.Append('0x006c=nso');
  langCodeHash.Append('0x006d=ba');
  langCodeHash.Append('0x006e=lb');
  langCodeHash.Append('0x006f=kl');
  langCodeHash.Append('0x0070=ig');
  langCodeHash.Append('0x0078=ii');
  langCodeHash.Append('0x007a=arn');
  langCodeHash.Append('0x007c=moh');
  langCodeHash.Append('0x007e=br');
  langCodeHash.Append('0x0080=ug');
  langCodeHash.Append('0x0081=mi');
  langCodeHash.Append('0x0082=oc');
  langCodeHash.Append('0x0083=co');
  langCodeHash.Append('0x0084=gsw');
  langCodeHash.Append('0x0085=sah');
  langCodeHash.Append('0x0086=qut');
  langCodeHash.Append('0x0087=rw');
  langCodeHash.Append('0x0088=wo');
  langCodeHash.Append('0x008c=prs');
  langCodeHash.Append('0x0091=gd');
  langCodeHash.Append('0x0401=ar-SA');
  langCodeHash.Append('0x0402=bg-BG');
  langCodeHash.Append('0x0403=ca-ES');
  langCodeHash.Append('0x0404=zh-TW');
  langCodeHash.Append('0x0405=cs-CZ');
  langCodeHash.Append('0x0406=da-DK');
  langCodeHash.Append('0x0407=de-DE');
  langCodeHash.Append('0x0408=el-GR');
  langCodeHash.Append('0x0409=en-US');
  //langCodeHash.Append('0x040A=es-ES_tradnl');
  langCodeHash.Append('0x040B=fi-FI');
  langCodeHash.Append('0x040C=fr-FR');
  langCodeHash.Append('0x040D=he-IL');
  langCodeHash.Append('0x040E=hu-HU');
  langCodeHash.Append('0x040F=is-IS');
  langCodeHash.Append('0x0410=it-IT');
  langCodeHash.Append('0x0411=ja-JP');
  langCodeHash.Append('0x0412=ko-KR');
  langCodeHash.Append('0x0413=nl-NL');
  langCodeHash.Append('0x0414=nb-NO');
  langCodeHash.Append('0x0415=pl-PL');
  langCodeHash.Append('0x0416=pt-BR');
  langCodeHash.Append('0x0417=rm-CH');
  langCodeHash.Append('0x0418=ro-RO');
  langCodeHash.Append('0x0419=ru-RU');
  langCodeHash.Append('0x041A=hr-HR');
  langCodeHash.Append('0x041B=sk-SK');
  langCodeHash.Append('0x041C=sq-AL');
  langCodeHash.Append('0x041D=sv-SE');
  langCodeHash.Append('0x041E=th-TH');
  langCodeHash.Append('0x041F=tr-TR');
  langCodeHash.Append('0x0420=ur-PK');
  langCodeHash.Append('0x0421=id-ID');
  langCodeHash.Append('0x0422=uk-UA');
  langCodeHash.Append('0x0423=be-BY');
  langCodeHash.Append('0x0424=sl-SI');
  langCodeHash.Append('0x0425=et-EE');
  langCodeHash.Append('0x0426=lv-LV');
  langCodeHash.Append('0x0427=lt-LT');
  langCodeHash.Append('0x0428=tg-Cyrl-TJ');
  langCodeHash.Append('0x0429=fa-IR');
  langCodeHash.Append('0x042A=vi-VN');
  langCodeHash.Append('0x042B=hy-AM');
  langCodeHash.Append('0x042C=az-Latn-AZ');
  langCodeHash.Append('0x042D=eu-ES');
  langCodeHash.Append('0x042E=wen-DE');
  langCodeHash.Append('0x042F=mk-MK');
  langCodeHash.Append('0x0430=st-ZA');
  langCodeHash.Append('0x0431=ts-ZA');
  langCodeHash.Append('0x0432=tn-ZA');
  langCodeHash.Append('0x0433=ven-ZA');
  langCodeHash.Append('0x0434=xh-ZA');
  langCodeHash.Append('0x0435=zu-ZA');
  langCodeHash.Append('0x0436=af-ZA');
  langCodeHash.Append('0x0437=ka-GE');
  langCodeHash.Append('0x0438=fo-FO');
  langCodeHash.Append('0x0439=hi-IN');
  langCodeHash.Append('0x043A=mt-MT');
  langCodeHash.Append('0x043B=se-NO');
  langCodeHash.Append('0x043E=ms-MY');
  langCodeHash.Append('0x043F=kk-KZ');
  langCodeHash.Append('0x0440=ky-KG');
  langCodeHash.Append('0x0441=sw-KE');
  langCodeHash.Append('0x0442=tk-TM');
  langCodeHash.Append('0x0443=uz-Latn-UZ');
  langCodeHash.Append('0x0444=tt-RU');
  langCodeHash.Append('0x0445=bn-IN');
  langCodeHash.Append('0x0446=pa-IN');
  langCodeHash.Append('0x0447=gu-IN');
  langCodeHash.Append('0x0448=or-IN');
  langCodeHash.Append('0x0449=ta-IN');
  langCodeHash.Append('0x044A=te-IN');
  langCodeHash.Append('0x044B=kn-IN');
  langCodeHash.Append('0x044C=ml-IN');
  langCodeHash.Append('0x044D=as-IN');
  langCodeHash.Append('0x044E=mr-IN');
  langCodeHash.Append('0x044F=sa-IN');
  langCodeHash.Append('0x0450=mn-MN');
  langCodeHash.Append('0x0451=bo-CN');
  langCodeHash.Append('0x0452=cy-GB');
  langCodeHash.Append('0x0453=km-KH');
  langCodeHash.Append('0x0454=lo-LA');
  langCodeHash.Append('0x0455=my-MM');
  langCodeHash.Append('0x0456=gl-ES');
  langCodeHash.Append('0x0457=kok-IN');
  langCodeHash.Append('0x0458=mni');
  langCodeHash.Append('0x0459=sd-IN');
  langCodeHash.Append('0x045A=syr-SY');
  langCodeHash.Append('0x045B=si-LK');
  langCodeHash.Append('0x045C=chr-US');
  langCodeHash.Append('0x045D=iu-Cans-CA');
  langCodeHash.Append('0x045E=am-ET');
  langCodeHash.Append('0x045F=tmz');
  langCodeHash.Append('0x0461=ne-NP');
  langCodeHash.Append('0x0462=fy-NL');
  langCodeHash.Append('0x0463=ps-AF');
  langCodeHash.Append('0x0464=fil-PH');
  langCodeHash.Append('0x0465=dv-MV');
  langCodeHash.Append('0x0466=bin-NG');
  langCodeHash.Append('0x0467=fuv-NG');
  langCodeHash.Append('0x0468=ha-Latn-NG');
  langCodeHash.Append('0x0469=ibb-NG');
  langCodeHash.Append('0x046A=yo-NG');
  langCodeHash.Append('0x046B=quz-BO');
  langCodeHash.Append('0x046C=nso-ZA');
  langCodeHash.Append('0x046D=ba-RU');
  langCodeHash.Append('0x046E=lb-LU');
  langCodeHash.Append('0x046F=kl-GL');
  langCodeHash.Append('0x0470=ig-NG');
  langCodeHash.Append('0x0471=kr-NG');
  langCodeHash.Append('0x0472=gaz-ET');
  langCodeHash.Append('0x0473=ti-ER');
  langCodeHash.Append('0x0474=gn-PY');
  langCodeHash.Append('0x0475=haw-US');
  langCodeHash.Append('0x0477=so-SO');
  langCodeHash.Append('0x0478=ii-CN');
  langCodeHash.Append('0x0479=pap-AN');
  langCodeHash.Append('0x047A=arn-CL');
  langCodeHash.Append('0x047C=moh-CA');
  langCodeHash.Append('0x047E=br-FR');
  langCodeHash.Append('0x0480=ug-CN');
  langCodeHash.Append('0x0481=mi-NZ');
  langCodeHash.Append('0x0482=oc-FR');
  langCodeHash.Append('0x0483=co-FR');
  langCodeHash.Append('0x0484=gsw-FR');
  langCodeHash.Append('0x0485=sah-RU');
  langCodeHash.Append('0x0486=qut-GT');
  langCodeHash.Append('0x0487=rw-RW');
  langCodeHash.Append('0x0488=wo-SN');
  langCodeHash.Append('0x048C=prs-AF');
  langCodeHash.Append('0x048D=plt-MG');
  langCodeHash.Append('0x0491=gd-GB');
  langCodeHash.Append('0x0801=ar-IQ');
  langCodeHash.Append('0x0804=zh-CN');
  langCodeHash.Append('0x0807=de-CH');
  langCodeHash.Append('0x0809=en-GB');
  langCodeHash.Append('0x080A=es-MX');
  langCodeHash.Append('0x080C=fr-BE');
  langCodeHash.Append('0x0810=it-CH');
  langCodeHash.Append('0x0813=nl-BE');
  langCodeHash.Append('0x0814=nn-NO');
  langCodeHash.Append('0x0816=pt-PT');
  langCodeHash.Append('0x0818=ro-MO');
  langCodeHash.Append('0x0819=ru-MO');
  langCodeHash.Append('0x081A=sr-Latn-CS');
  langCodeHash.Append('0x081D=sv-FI');
  langCodeHash.Append('0x0820=ur-IN');
  langCodeHash.Append('0x082C=az-Cyrl-AZ');
  langCodeHash.Append('0x082E=dsb-DE');
  langCodeHash.Append('0x083B=se-SE');
  langCodeHash.Append('0x083C=ga-IE');
  langCodeHash.Append('0x083E=ms-BN');
  langCodeHash.Append('0x0843=uz-Cyrl-UZ');
  langCodeHash.Append('0x0845=bn-BD');
  langCodeHash.Append('0x0846=pa-PK');
  langCodeHash.Append('0x0850=mn-Mong-CN');
  langCodeHash.Append('0x0851=bo-BT');
  langCodeHash.Append('0x0859=sd-PK');
  langCodeHash.Append('0x085D=iu-Latn-CA');
  langCodeHash.Append('0x085F=tzm-Latn-DZ');
  langCodeHash.Append('0x0861=ne-IN');
  langCodeHash.Append('0x086B=quz-EC');
  langCodeHash.Append('0x0873=ti-ET');
  langCodeHash.Append('0x0C01=ar-EG');
  langCodeHash.Append('0x0C04=zh-HK');
  langCodeHash.Append('0x0C07=de-AT');
  langCodeHash.Append('0x0C09=en-AU');
  langCodeHash.Append('0x0C0A=es-ES');
  langCodeHash.Append('0x0C0C=fr-CA');
  langCodeHash.Append('0x0C1A=sr-Cyrl-CS');
  langCodeHash.Append('0x0C3B=se-FI');
  langCodeHash.Append('0x0C5F=tmz-MA');
  langCodeHash.Append('0x0C6B=quz-PE');
  langCodeHash.Append('0x1001=ar-LY');
  langCodeHash.Append('0x1004=zh-SG');
  langCodeHash.Append('0x1007=de-LU');
  langCodeHash.Append('0x1009=en-CA');
  langCodeHash.Append('0x100A=es-GT');
  langCodeHash.Append('0x100C=fr-CH');
  langCodeHash.Append('0x101A=hr-BA');
  langCodeHash.Append('0x103B=smj-NO');
  langCodeHash.Append('0x1401=ar-DZ');
  langCodeHash.Append('0x1404=zh-MO');
  langCodeHash.Append('0x1407=de-LI');
  langCodeHash.Append('0x1409=en-NZ');
  langCodeHash.Append('0x140A=es-CR');
  langCodeHash.Append('0x140C=fr-LU');
  langCodeHash.Append('0x141A=bs-Latn-BA');
  langCodeHash.Append('0x143B=smj-SE');
  langCodeHash.Append('0x1801=ar-MA');
  langCodeHash.Append('0x1809=en-IE');
  langCodeHash.Append('0x180A=es-PA');
  langCodeHash.Append('0x180C=fr-MC');
  langCodeHash.Append('0x181A=sr-Latn-BA');
  langCodeHash.Append('0x183B=sma-NO');
  langCodeHash.Append('0x1C01=ar-TN');
  langCodeHash.Append('0x1C09=en-ZA');
  langCodeHash.Append('0x1C0A=es-DO');
  //langCodeHash.Append('0x1C0C=fr-West Indies');   // not bcp 47
  langCodeHash.Append('0x1C1A=sr-Cyrl-BA');
  langCodeHash.Append('0x1C3B=sma-SE');
  langCodeHash.Append('0x2001=ar-OM');
  langCodeHash.Append('0x2009=en-JM');
  langCodeHash.Append('0x200A=es-VE');
  langCodeHash.Append('0x200C=fr-RE');
  langCodeHash.Append('0x201A=bs-Cyrl-BA');
  langCodeHash.Append('0x203B=sms-FI');
  langCodeHash.Append('0x2401=ar-YE');
  langCodeHash.Append('0x2409=en-CB');
  langCodeHash.Append('0x240A=es-CO');
  langCodeHash.Append('0x240C=fr-CG');
  langCodeHash.Append('0x241a=sr-Latn-RS');
  langCodeHash.Append('0x243B=smn-FI');
  langCodeHash.Append('0x2801=ar-SY');
  langCodeHash.Append('0x2809=en-BZ');
  langCodeHash.Append('0x280A=es-PE');
  langCodeHash.Append('0x280C=fr-SN');
  langCodeHash.Append('0x281a=sr-Cyrl-RS');
  langCodeHash.Append('0x2C01=ar-JO');
  langCodeHash.Append('0x2C09=en-TT');
  langCodeHash.Append('0x2C0A=es-AR');
  langCodeHash.Append('0x2C0C=fr-CM');
  langCodeHash.Append('0x2c1a=sr-Latn-ME');
  langCodeHash.Append('0x3001=ar-LB');
  langCodeHash.Append('0x3009=en-ZW');
  langCodeHash.Append('0x300A=es-EC');
  langCodeHash.Append('0x300C=fr-CI');
  langCodeHash.Append('0x301a=sr-Cyrl-ME');
  langCodeHash.Append('0x3401=ar-KW');
  langCodeHash.Append('0x3409=en-PH');
  langCodeHash.Append('0x340A=es-CL');
  langCodeHash.Append('0x340C=fr-ML');
  langCodeHash.Append('0x3801=ar-AE');
  langCodeHash.Append('0x3809=en-ID');
  langCodeHash.Append('0x380A=es-UY');
  langCodeHash.Append('0x380C=fr-MA');
  langCodeHash.Append('0x3C01=ar-BH');
  langCodeHash.Append('0x3C09=en-HK');
  langCodeHash.Append('0x3c0a=es-PY');
  langCodeHash.Append('0x3C0C=fr-HT');
  langCodeHash.Append('0x4001=ar-QA');
  langCodeHash.Append('0x4009=en-IN');
  langCodeHash.Append('0x400A=es-BO');
  langCodeHash.Append('0x4409=en-MY');
  langCodeHash.Append('0x440A=es-SV');
  langCodeHash.Append('0x4809=en-SG');
  langCodeHash.Append('0x480A=es-HN');
  langCodeHash.Append('0x4C0A=es-NI');
  langCodeHash.Append('0x500A=es-PR');
  langCodeHash.Append('0x540A=es-US');
  langCodeHash.Append('0x641a=bs-Cyrl');
  langCodeHash.Append('0x681a=bs-Latn');
  langCodeHash.Append('0x6c1a=sr-Cyrl');
  langCodeHash.Append('0x701a=sr-Latn');
  langCodeHash.Append('0x703b=smn');
  langCodeHash.Append('0x742c=az-Cyrl');
  langCodeHash.Append('0x743b=sms');
  langCodeHash.Append('0x7804=zh');
  langCodeHash.Append('0x7814=nn');
  langCodeHash.Append('0x781a=bs');
  langCodeHash.Append('0x782c=az-Latn');
  langCodeHash.Append('0x783b=sma');
  langCodeHash.Append('0x7843=uz-Cyrl');
  langCodeHash.Append('0x7850=mn-Cyrl');
  langCodeHash.Append('0x785d=iu-Cans');
  langCodeHash.Append('0x7c14=nb');
  langCodeHash.Append('0x7c1a=sr');
  langCodeHash.Append('0x7c28=tg-Cyrl');
  langCodeHash.Append('0x7c2e=dsb');
  langCodeHash.Append('0x7c3b=smj');
  langCodeHash.Append('0x7c43=uz-Latn');
  langCodeHash.Append('0x7c50=mn-Mong');
  langCodeHash.Append('0x7c5d=iu-Latn');
  langCodeHash.Append('0x7c5f=tzm-Latn');
  langCodeHash.Append('0x7c68=ha-Latn');
end;

procedure freeLangcodeHash;
begin
  langCodeHash.Free;
end;

function getlangcodeByHexvalueStr(const hexvalue: string): string;
begin
  if langCodeHash = nil then
    createLangcodeHash;
  Result := langCodeHash.Values[hexvalue];
end;

function getlangcodeByDecvalueStr(const decvalue: string): string;
begin
  Result := getlangcodeByHexvalueStr(IntToHex(StrToInt(decvalue), 4));
end;

initialization
  createLangcodeHash;

finalization
  freeLangcodeHash;
end.

