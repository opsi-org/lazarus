{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit SubFormPackage; 

interface

uses
  subform, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('subform', @subform.Register); 
end; 

initialization
  RegisterPackage('SubFormPackage', @Register); 
end.
