unit key_valueCollection;

// http://wiki.freepascal.org/TCollection/de

{$mode objfpc}{$H+}

interface

uses
  Classes;

type

  // TListItem
  // TListitem ist eine Unterelement von TListe
  TListItem = class(TCollectionItem)
  private
    ikey : string;
    ivalue: string;
  public
    // überschreibt den von TCollection geerbten Construktor
    constructor Create(colCollection: TCollection); override;
    property key: string read ikey write ikey;
    property value: string read ivalue write ivalue;
  end;


  // TList
  // An TList werden die Elemente von TListItem angehängt
  TList = class(TCollection)
  private
    function GetItems(index: integer): TListItem;
    procedure SetItems(index: integer; Value: TListItem);
  public
    constructor Create;
    // Die Funktion hängt an das Objekt vom Typ TListe
    // ein Objekt vom Typ TCollectionItem hinzu.
    function Add: TCollectionItem;
    property Items[index: integer]: TListItem read GetItems write SetItems;
  end;


implementation

// TListe

function TList.Add: TCollectionItem;
begin
  // Es können beliebig viele Objekte vom Typ TCollectionItem,
  // als Elemente angefügt werden.
  Result := TCollectionItem(inherited Add);
end;

constructor TList.Create;
begin
  inherited Create(TListItem);
end;

function TList.GetItems(index: integer): TListItem;
begin
  try
     Result := TListItem(inherited Items[Index]);
  except
  end;
end;

procedure TList.SetItems(index: integer; Value: TListItem);
begin
  Items[Index].Assign(Value);
end;

// TListItem

constructor TListItem.Create(colCollection: TCollection);
begin
  if assigned(colCollection) and (colCollection is TList) then
    inherited Create(colCollection);
end;

end.
