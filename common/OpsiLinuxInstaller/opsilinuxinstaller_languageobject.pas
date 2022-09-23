unit OpsiLinuxInstaller_LanguageObject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // requires package lazUtils in project
  Translations,
  oslog;

type
  TLanguageObject = class(TObject)
  public
    Abbreviation: string;
    constructor Create;
    procedure TranslateProjectResourceStrings(
      NameOfResourceStringsUnit: string; PathOfTranslationFile: string);
  end;

var
  Language: TLanguageObject;

implementation

constructor TLanguageObject.Create;
begin
  inherited Create;
  Abbreviation := 'en';
end;

procedure TLanguageObject.TranslateProjectResourceStrings(
  NameOfResourceStringsUnit: string; PathOfTranslationFile: string);
begin
  if not FileExists(PathOfTranslationFile) then
  begin
    LogDatei.log('Translations file "' + PathOfTranslationFile + '" not found',
      LLWarning);
  end;

  TranslateUnitResourceStrings(NameOfResourceStringsUnit, PathOfTranslationFile);
end;

end.
