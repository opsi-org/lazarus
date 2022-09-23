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
  private
    FPathToLazarusRepo: string;
  public
    Abbreviation: string;
    constructor Create(PathToLazarusRepo: string);
    procedure TranslateCommonResourceStrings(NameOfResourceStringsUnit: string;
      NameOfTranslationFile: string);
    procedure TranslateProjectResourceStrings(
      NameOfResourceStringsUnit: string; PathOfTranslationFile: string);
  end;

var
  Language: TLanguageObject;

implementation

constructor TLanguageObject.Create(PathToLazarusRepo: string);
begin
  FPathToLazarusRepo := PathToLazarusRepo;
  Abbreviation := 'en';
end;

procedure TLanguageObject.TranslateCommonResourceStrings(
  NameOfResourceStringsUnit: string; NameOfTranslationFile: string);
begin
  if not FileExists(FPathToLazarusRepo + 'common/OpsiLinuxInstaller/locale/' +
    NameOfTranslationFile) then
  begin
    LogDatei.log('Translations file "' + FPathToLazarusRepo +
      'common/OpsiLinuxInstaller/locale/' + NameOfTranslationFile +
      '" not found', LLWarning);
  end;

  TranslateUnitResourceStrings(NameOfResourceStringsUnit,
    FPathToLazarusRepo + '/common/OpsiLinuxInstaller/locale/' + NameOfTranslationFile);
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
