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
    FPathToCommonLocaleFolder: string;
    FPathToProjectLocaleFolder: string;
  public
    Abbreviation: string;
    constructor Create(PathToCommonLocaleFolder: string;
      PathToProjectLocaleFolder: string);
    procedure TranslateResourceStrings(NameOfResourceStringsUnit: string;
      NameOfTranslationFile: string);
  end;

var
  Language: TLanguageObject;

implementation

constructor TLanguageObject.Create(PathToCommonLocaleFolder: string;
  PathToProjectLocaleFolder: string);
begin
  FPathToCommonLocaleFolder := PathToCommonLocaleFolder;
  FPathToProjectLocaleFolder := PathToProjectLocaleFolder;
  Abbreviation := 'en';
end;

procedure TLanguageObject.TranslateResourceStrings(NameOfResourceStringsUnit: string;
  NameOfTranslationFile: string);
begin
  if FileExists(FPathToCommonLocaleFolder + NameOfTranslationFile) then
  begin
    TranslateUnitResourceStrings(NameOfResourceStringsUnit,
      FPathToCommonLocaleFolder + NameOfTranslationFile);
  end
  else if FileExists(FPathToProjectLocaleFolder + NameOfTranslationFile) then
  begin
    TranslateUnitResourceStrings(NameOfResourceStringsUnit,
      FPathToProjectLocaleFolder + NameOfTranslationFile);
  end
  else
  begin
    LogDatei.log('Translations file "' + FPathToCommonLocaleFolder +
      'common/OpsiLinuxInstaller/locale/' + NameOfTranslationFile +
      '" not found', LLWarning);
  end;
end;

end.
