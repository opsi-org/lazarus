unit windatamodul;

{$MODE Delphi}

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
{$IFDEF WINDOWS}
{$ENDIF}
LCLIntf,
LResources,
SysUtils, Classes;

type
  TDataModule1 = class(TDataModule)
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  datamodule1: TDataModule1;

implementation


initialization
 {$i windatamodul.lrs}



end.
 
