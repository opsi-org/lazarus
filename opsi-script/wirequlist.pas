unit wirequlist;

{$MODE Delphi}
{$RANGECHECKS ON}
{$OVERFLOWCHECKS ON}
{$IOCHECKS ON}
{$OBJECTCHECKS ON}
{$VARSTRINGCHECKS ON}
{$LONGSTRINGS ON}



// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: Rupert Roeder, detlef oertel
// credits: http://www.opsi.org/credits/


interface

uses
{$IFDEF WINDOWS}
{$ENDIF}
  //LCLIntf,
  Classes,
  SysUtils,
  oswebservice;

type
  TCardinalnumber = integer;
  TElement = integer;
  TOrder = array of integer;

  TOrderbuild = class(TObject)
    places: TOrder;
    usedCount: integer;
    errorfound: boolean;
  public
    constructor Create(const productlist: TStringList);
    destructor Destroy; override;
  end;

  TRequirement = class(TObject)
    prior: TElement;
    posterior: TElement;
    fulfilled: boolean;
  end;

  TPlacement = class(TObject)
    indexValue: integer;
  end;


  TRequList = class(TList)

  private
    FOrder_byPrior: TOrder;
    FOrder_byPosterior: TOrder;
    capacityincrement: integer;
    FAllFullfilled: boolean;
    FResultorder: TOrderbuild;
    FProductlist: TStringList;
    ProductEntries: array of boolean;
    isAmongPosteriors: array of boolean;
    FResultString: string;

  public
    constructor Create(const Productlist: TStringList);
    destructor Destroy; override;
    procedure clone(const master: TRequList);
    function add(const item: TRequirement): integer;

    procedure retrieveRequirements(const opsidata: TOpsiData);
    //procedure readFromInfoFile
    //(const productsInfoFile : TuibInifile);

    property Order_byPrior: TOrder read FOrder_byPrior write FOrder_byPrior;
    property Order_byPosterior: TOrder read FOrder_byPosterior
      write FOrder_byPosterior;

    property AllFullfilled: boolean read FAllFullfilled write FAllFullfilled;

    property Productlist: TStringList read FProductlist;
    property Resultorder: TOrderbuild read FResultorder;
    property ResultString: string read FResultString;

    function PosteriorIndexOf(const posterior: TElement): integer;
    // search the first occurrence of a TRequirement with the posterior component having value
    // posterior

    function doNextStep: boolean;
  end;


implementation

constructor TOrderbuild.Create(const productlist: TStringList);
begin
  inherited Create;
  usedCount := 0;
  errorFound := False;
  setLength(places, productlist.Count);
end;

destructor TOrderbuild.Destroy;
begin
  inherited Destroy;
end;

function requ_prior_IsLess(item1, item2: Pointer): boolean;
begin
  Result := (TRequirement(item1).prior < TRequirement(item2).prior);
end;

function requ_posterior_IsLess(item1, item2: Pointer): boolean;
begin
  Result := (TRequirement(item1).posterior < TRequirement(item2).posterior);
end;


constructor TRequList.Create(const Productlist: TStringList);
var
  i: integer;
begin
  inherited Create;

  capacityincrement := 10;
  setLength(FOrder_byPrior, capacityincrement);
  setLength(FOrder_byPosterior, capacityincrement);

  FAllFullfilled := False;
  FProductlist := Productlist;
  FResultString := '';

  setLength(ProductEntries, productlist.Count);
  for i := 0 to productlist.Count - 1 do
    ProductEntries[i] := False;


  setLength(isAmongPosteriors, productlist.Count);
  for i := 0 to productlist.Count - 1 do
    isAmongPosteriors[i] := False;


  FResultorder := TOrderbuild.Create(Productlist);
end;


destructor TRequList.Destroy;
begin
  inherited Destroy;
end;



function TRequList.add(const item: TRequirement): integer;
var
  located: boolean;

  i, j: integer;

begin
  Result := inherited add(item);

  if length(FOrder_byPrior) < Count then
  begin
    setLength(FOrder_byPrior, Count + capacityincrement);
    setLength(FOrder_byPosterior, Count + capacityincrement);
  end;

  // determine the place of the added item in the
  // ordered sequence of  i -> Items[FOrder_byPrior[i]}
  located := False;
  i := 0;
  while not located and (i < Count - 1) do
  begin
    if requ_prior_IsLess(Items[FOrder_byPrior[i]],
      item) then
      Inc(i)
    else
    begin
      located := True;

      // make free place i
      for j := Count - 1 downto i + 1 do
        FOrder_byPrior[j] := FOrder_byPrior[j - 1];


      // set element i on the index of the added item
      FOrder_byPrior[i] := Count - 1;
    end;
  end;

  if not located // i = count - 1
  then
    FOrder_byPrior[i] := Count - 1;


  // determine the place of item in the sequence of  i -> Items[FOrder_byPosterior[i]}
  located := False;
  i := 0;
  while not located and (i < Count - 1) do
  begin
    if requ_posterior_IsLess(Items[FOrder_byPosterior[i]],
      item) then
      Inc(i)
    else
    begin
      located := True;

      // make free place i
      for j := Count - 1 downto i + 1 do
        FOrder_byPosterior[j] := FOrder_byPosterior[j - 1];


      // set element i on the index of the added item
      FOrder_byPosterior[i] := Count - 1;
    end;
  end;

  if not located // i = count - 1
  then
    FOrder_byPosterior[i] := Count - 1;

end;


procedure TRequList.clone(const master: TRequList);
var
  i: integer;
begin
  Clear;
  setcapacity(master.Count);
  for i := 0 to master.Count - 1 do
    add(master.items[i]);
end;


procedure TRequList.retrieveRequirements(const opsidata: TOpsiData);
var
  i, j: integer;
  InstallBefore, InstallAfter: TStringList;

  aRequirement: TRequirement;
  saveProductname: string;
begin
  InstallBefore := TStringList.Create;
  //aRequirement.fulfilled := false;

  saveProductname := Topsi4data(opsidata).getActualProductId;

  for i := 0 to productlist.Count - 1 do
  begin
    //-- ProductsInfoFile.ReadSection (productlist[i] + '-requires_before', InstallBefore);

    opsidata.setActualProductName(productlist[i]);
    InstallBefore := opsidata.getBeforeRequirements;

    if InstallBefore.Count > 0 then
    begin
      for j := 0 to InstallBefore.Count - 1 do
      begin
        aRequirement := TRequirement.Create;
        aRequirement.fulfilled := False;
        aRequirement.posterior := i;
        if productlist.IndexOf(InstallBefore[j]) > -1 then
        begin
          aRequirement.prior := productlist.IndexOf(InstallBefore[j]);
          Add(aRequirement);
        end;
      end;
    end;

    InstallBefore.Free;

  end;

  InstallAfter := TStringList.Create;

  for i := 0 to productlist.Count - 1 do
  begin
    //--ProductsInfoFile.ReadSection (productlist[i] + '-requires_after', InstallAfter);

    opsidata.setActualProductName(productlist[i]);
    InstallAfter := opsidata.getAfterRequirements;

    if InstallAfter.Count > 0 then
    begin
      for j := 0 to InstallAfter.Count - 1 do
      begin
        aRequirement := TRequirement.Create;
        aRequirement.fulfilled := False;
        aRequirement.prior := i;
        if productlist.IndexOf(InstallAfter[j]) > -1 then
        begin
          aRequirement.posterior := productlist.IndexOf(InstallAfter[j]);
          Add(aRequirement);
        end;
      end;
    end;

    InstallAfter.Free;

  end;


  opsidata.setActualProductName(saveProductname);

end;


 {
 Procedure TRequList.readFromInfoFile
              (const productsInfoFile : TuibInifile);
 var
   i, j : Integer;
   InstallBefore, InstallAfter : TStringList;

   aRequirement : TRequirement;
 begin
   InstallBefore := TStringList.create;
   //aRequirement.fulfilled := false;
   for i:= 0 to productlist.count-1
   do
   Begin
     //aRequirement.posterior := i;
     ProductsInfoFile.ReadSection (productlist[i] + '-requires_before', InstallBefore);
     if InstallBefore.count > 0
     then
     Begin
       for j := 0 to InstallBefore.count - 1
       do
       Begin
         aRequirement := TRequirement.create;
         aRequirement.fulfilled := false;
         aRequirement.posterior := i;
         if productlist.IndexOf(InstallBefore[j]) > -1
         then
         Begin
            aRequirement.prior := productlist.IndexOf(InstallBefore[j]);
            Add (aRequirement);
         end;
       End;
     End
   end;
   InstallBefore.free;
   InstallAfter := TStringList.create;
   for i:= 0 to productlist.count-1
   do
   Begin
     //aRequirement.posterior := i;
     ProductsInfoFile.ReadSection (productlist[i] + '-requires_after', InstallAfter);
     if InstallAfter.count > 0
     then
     Begin
       for j := 0 to InstallAfter.count - 1
       do
       Begin
         aRequirement := TRequirement.create;
         aRequirement.fulfilled := false;
         aRequirement.prior := i;
         if productlist.IndexOf(InstallAfter[j]) > -1
         then
         Begin
            aRequirement.posterior := productlist.IndexOf(InstallAfter[j]);
            Add (aRequirement);
         end;
       End;
     End
   end;
   InstallAfter.free;

 end;
 }

function TRequList.posteriorIndexOf(const posterior: TElement): integer;
  // search first occurence of posterior in the series of posteriors
var
  j: integer;
  candidate: TRequirement;
  searching: boolean;

begin
  j := 0;

  searching := True;

  while (j < Count) and searching do
  begin
    candidate := TRequirement(items[FOrder_byPosterior[j]]);
    if (candidate.fulfilled or (candidate.posterior < posterior)) then
      Inc(j)
    else
      searching := False;
  end;

  if searching
  // all candidates were less than the comparevalue or were not to be regarded any more
  then
    Result := -1
  else
    // candidate is not fulfilled
    // and is >= posterior
  begin
    if candidate.posterior = posterior then
      Result := j
    else
      // there are no more possible occurrences of posterior
      Result := -1;
  end;
end;



function TRequList.donextStep: boolean;
var
  found_notfulfilled_requirement: boolean;

  i, j, k: integer;
  foundValidItem: boolean;
  candidate: integer;
  lastcandidate: integer;
  lastSortedCount: integer;
  newOrder: TOrder;

begin
  Result := True;
  lastSortedCount := 0;

  if FResultOrder.usedCount >= productlist.Count then
    exit;

  found_notfulfilled_requirement := False;
  i := 0;
  while not found_notfulfilled_requirement and (i < Count) do
  begin
    if TRequirement(items[order_byPrior[i]]).fulfilled then
      Inc(i)
    else
      found_notfulfilled_requirement := True;
  end;


  if not found_notfulfilled_requirement // i > count - 1
  then
  begin
    AllFullfilled := True;


    // get the posteriors that did not occur as priors
    for j := 0 to Productlist.Count - 1 do
    begin
      if isAmongPosteriors[j] and not ProductEntries[j] then
      begin

        ProductEntries[j] := True;
        Fresultorder.places[FresultOrder.usedCount] := j;

        FResultOrder.usedCount := FResultOrder.usedCount + 1;
      end;
    end;

    lastSortedCount := FResultOrder.usedCount;


    // take the rest from productlist
    for j := 0 to Productlist.Count - 1 do
    begin
      if not ProductEntries[j] then
      begin
        ProductEntries[j] := True;
        Fresultorder.places[FresultOrder.usedCount] := j;

        FResultOrder.usedCount := FResultOrder.usedCount + 1;
      end;
    end;

    // move the sorted items to the end of the list
    if lastSortedCount > 0 then
    begin
      setLength(newOrder, length(FResultOrder.places));

      for k := 1 to lastSortedCount do
        newOrder[length(newOrder) - k] := FResultOrder.places[lastSortedCount - k];

      for k := lastSortedCount + 1 to length(newOrder) do
        newOrder[length(newOrder) - k] :=
          FResultOrder.places[length(newOrder) + lastSortedCount - k];

      for k := 0 to (length(newOrder) - 1) do
        FResultOrder.places[k] := newOrder[k];
    end;

  end

  else   // we found a not fulfilled requirement, lets try to fulfill it
  begin
    // starting with the prior at i
    // we look if there is, in the sequence of priors,
    // any prior that does not appear as posterior in a requirement

    // initialize loop
    foundValidItem := False;
    j := i;
    candidate := TRequirement(items[order_byPrior[j]]).prior;
    lastcandidate := -1;
    FResultString := ' potentially conflicting requirements for: ';

    while (j < Count) and not foundValidItem do
    begin
      if not TRequirement(items[order_byPrior[j]]).fulfilled and
        (PosteriorIndexOf(candidate) = -1) then
        // if requirement j was still not fulfilled
        // and candidate does not occur as posterior among the not fulfilled
        // then we adopt candidate (i.e. the prior element of requirement j)
        // as next element in our ordered sequence

        foundValidItem := True

      else
        // we go on searching

      begin
        Inc(j);
        if (j < Count) then
          candidate := TRequirement(items[order_byPrior[j]]).prior;

        if (PosteriorIndexOf(candidate) > -1) and (lastcandidate <> candidate)
        then
        begin
          FResultString := FResultString +
            FProductlist.Strings[candidate] + '; ';
          lastcandidate := candidate;
        end;

      end;
    end;

    if foundValidItem then
    begin
      // get candidate into resultorder
      Fresultorder.places[FresultOrder.usedCount] := candidate;
      FResultOrder.usedCount := FResultOrder.usedCount + 1;

      // mark all requirements with candidate in prior position as fulfilled
      // --- and collect the posteriors
      k := j;
      while (k < Count) and (candidate =
          TRequirement(items[order_byPrior[k]]).prior) do
      begin
        TRequirement(items[order_byPrior[k]]).fulfilled := True;
        isAmongPosteriors[TRequirement(items[order_byPrior[k]]).posterior] :=
          True;
         {messagebox (0,
         PChar ('isAmongPosteriors ' + IntToStr (
                                             TRequirement ( items[ order_byPrior[k] ] ).posterior )),
         'info', mb_ok);
         }
        Inc(k);
      end;

      // remember that candidate is taken into account
      productEntries[candidate] := True;
      FResultString := '';
    end

    else
    begin
      FResultOrder.errorFound := True;
      Result := False;
    end;
  end;

end;


end.
