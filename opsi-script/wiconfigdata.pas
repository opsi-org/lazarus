unit wiconfigdata;


// will constitute the implementation of the interface configurationdata in delphi
// momentarily only for developing the concept

interface

uses
 classes,
 wifunc;

type


 TConfigurationData = class (TObject)

     function getProductScriptPathes (productname : String) : TStringList; virtual; abstract; 
     function getPcInstallMap (pcname : String) : TStringList; virtual; abstract;
     procedure setPcProductSwitches ( pcname : String; productname : String;
                                      state : String;  action : String ); virtual; abstract;

 end;

 TConfigurationData_classic = class (TConfigurationData)
   public
     function getProductScriptPathes (productname : String) : TStringList;
     function getPcInstallMap (pcname : String) : TStringList;
     procedure setPcProductSwitches ( pcname : String; productname : String;
                                     state : String;  action : String );
 end;



implementation

     function TConfigurationData_classic.getProductScriptPathes (productname : String) : TStringList;
     begin
     end;

     function TConfigurationData_classic.getPcInstallMap (pcname : String) : TStringList;
     begin
     end;

     procedure TConfigurationData_classic.setPcProductSwitches ( pcname : String; productname : String;
                                      state : String;  action : String );
     begin
     end;
end.
