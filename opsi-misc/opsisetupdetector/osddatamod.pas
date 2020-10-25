unit osddatamod;

{$mode delphi}

interface

uses
  Classes, SysUtils,
  Controls,
  Graphics,
  typinfo;

type
  TDataModule1 = class(TDataModule)
  private

  public
    procedure SetFontName(Control: TControl; Name: string);

  end;

var
  DataModule1: TDataModule1;
  myFont : string;

implementation

{$R *.lfm}


{ from https://stackoverflow.com/questions/10588660/font-consistency-throughout-project
  modified for font.name only }
procedure TDataModule1.SetFontName(Control: TControl; Name: string);
// Set font properties
var
  Index: integer;
  Font: TFont;
  AnObject: TObject;
  ChildControl: TControl;
begin
  // Set font properties
  try
    { The control may have no property 'font' - this will lead to an exception }
    if IsPublishedProp(Control, 'Font') then
    begin
      AnObject := GetObjectProp(Control, 'Font', TControl);
      if AnObject is TFont then
      begin
        // Set properties
        Font := TFont(AnObject);
        Font.Name := Name;
      end;
    end;

  except
  end;

  // Set child font properties
  if Control is TWinControl then
  begin
    // Set
    for Index := 0 to TWinControl(Control).ControlCount - 1 do
    begin
      // Child control
      ChildControl := TWinControl(Control).Controls[Index];

      // Set font properties
      SetFontName(ChildControl, Name);
    end;
  end;
end;


initialization

myFont := 'Arial';
{$IFDEF LINUX}
//myFont := 'Liberation Sans Narrow';
myFont := 'Liberation Sans';
{$ENDIF LINUX}


end.

