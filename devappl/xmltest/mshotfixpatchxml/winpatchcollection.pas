unit winpatchCollection;

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
    cabname : string;
    fileId: string;
    filename: string;
    //bundledbyRevisionId: string;
    defaultlang: string;
    title: string;
    description : string;
    releaseid : string;
    installationtype : string;
  public
    // überschreibt den von TCollection geerbten Construktor
    constructor Create(colCollection: TCollection); override;
    property a_cabname: string read cabname write cabname;
    property a_fileId: string read fileId write fileId;
    property a_filename: string read filename write filename;
    property a_defaultlang: string read defaultlang write defaultlang;
    property a_title: string read title write title;
    property a_description: string read description write description;
    property a_releaseid: string read releaseid write releaseid;
    property a_installationtype: string read installationtype write installationtype;
  end;


  // TListe
  // An TListe werden die Elemente von TListItem angehängt
  TListe = class(TCollection)
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

function TListe.Add: TCollectionItem;
begin
  // Es können beliebig viele Objekte vom Typ TCollectionItem,
  // als Elemente angefügt werden.
  Result := TCollectionItem(inherited Add);
end;

constructor TListe.Create;
begin
  inherited Create(TListItem);
end;

function TListe.GetItems(index: integer): TListItem;
begin
  Result := TListItem(inherited Items[Index]);
end;

procedure TListe.SetItems(index: integer; Value: TListItem);
begin
  Items[Index].Assign(Value);
end;



// TListItem

constructor TListItem.Create(colCollection: TCollection);
begin
  if assigned(colCollection) and (colCollection is TListe) then
    inherited Create(colCollection);
end;

end.
