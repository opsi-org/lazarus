unit wmitestu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, jan_wmi;
  //,oswmi;

type

  { TMainForm }

  TMainForm = class(TForm)
    ButtonArrowRight: TButton;
    ButtonArrowLeft: TButton;
    ButtonQuit: TButton;
    ButtonExecute: TButton;
    ComboBoxAlias: TComboBox;
    ComboBoxWMIClass: TComboBox;
    EditCondition: TEdit;
    EditQuery: TEdit;
    GroupBoxConnection: TGroupBox;
    GroupBoxRequest: TGroupBox;
    Image1: TImage;
    LabelQuery: TLabel;
    LabelSelectedWMIProperties: TLabel;
    LabelClass: TLabel;
    LabelAlias: TLabel;
    LabelEditUser: TLabeledEdit;
    LabelEditNameSpace: TLabeledEdit;
    LabelEditComputer: TLabeledEdit;
    LabelEditPassword: TLabeledEdit;
    LabelAvailableWMIProperties: TLabel;
    LabelCondition: TLabel;
    ListBoxSelectedWMIProperties: TListBox;
    ListBoxAvailableWMIProperties: TListBox;
    ToggleBoxExpertMode: TToggleBox;
    procedure ButtonQuitClick(Sender: TObject);
    procedure ButtonArrowLeftClick(Sender: TObject);
    procedure ButtonArrowRightClick(Sender: TObject);
    procedure ButtonExecuteClick(Sender: TObject);
    procedure ComboBoxAliasSelect(Sender: TObject);
    procedure ComboBoxAliasKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ComboBoxWMIClassSelect(Sender: TObject);
    procedure ComboBoxWMIClassKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBoxAvailableWMIPropertiesDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure ListBoxAvailableWMIPropertiesDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure ListBoxSelectedWMIPropertiesDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure ListBoxSelectedWMIPropertiesDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure ToggleBoxExpertModeChange(Sender: TObject);
  private
    WMIClass : TWMIClass;
    procedure fillComboBoxWMIClass();
    function GetWMIPropertiesFromListBox():string;
    procedure MoveItemsToIndex(LB:TListBox; NewIndex: Integer);
    procedure MoveItemsToListBox(LBTo,LBFrom: TListBox; NewIndex: Integer);
    procedure IfNotExistCreateResultForm(Sender:TObject);
  public

  end;

var
  MainForm: TMainForm;


implementation

uses
  resultwindow;
{$R *.lfm}

{ TMainForm }

//Execution of the request to WMI Server after selecting properties
procedure TMainForm.ButtonExecuteClick(Sender: TObject);
var
  WMIProperties: String;
begin
  IfNotExistCreateResultForm(MainForm);
  if (ComboBoxWMIClass.Text = 'or select WMI Class') or (ComboBoxWMIClass.Text = '') then
  begin
    ResultForm.MemoQueryResult.Text := 'Please select a WMI Class!';
  end
  else
  begin
    WMIProperties := GetWMIPropertiesFromListBox();
    //MemoWMIPropertyNames.Append(WMIProperties);//just for testing
    try
    if WMIClass.ConnectToWMIService(WideString(LabelEditComputer.Text),
    WideString(LabelEditNameSpace.Text),WideString(LabelEditUser.Text),
    WideString(LabelEditPassword.Text)) then
      begin
        ResultForm.MemoQueryResult.Text:= 'Waiting for query response ...';
        WMIClass.RequestToWMIService(WMIProperties,ComboBoxWMIClass.Text,EditCondition.Text);
        EditQuery.Text:= WMIClass.FinalQuery ;
        ResultForm.MemoQueryResult.Clear;
        ResultForm.MemoQueryResult.Text:= WMIClass.WMIRequestResult.Text;
      end;
    except
      on E: Exception do ResultForm.MemoQueryResult.Text := WMIClass.ErrorMsg;
    end;
  end;
end;

//Choosing from Alias
procedure TMainForm.ComboBoxAliasSelect(Sender: TObject);
begin
  try
    ListBoxAvailableWMIProperties.Clear;
    ListBoxSelectedWMIProperties.Clear;
    ResultForm.MemoQueryResult.Clear;
    ComboBoxWMIClass.Text:= WMIClass.WMIClassNames.Values[ComboBoxAlias.Text];
    if WMIClass.ConnectToWMIService(WideString(LabelEditComputer.Text),
    WideString(LabelEditNameSpace.Text),WideString(LabelEditUser.Text),
    WideString(LabelEditPassword.Text))
      then
      begin
        ListBoxAvailableWMIProperties.Items.Add('Waiting for property list ...');
        WMIClass.InitWMIProperties(ComboBoxWMIClass.Text);
        //ListBoxAvailableWMIProperties.Clear;
        ListBoxAvailableWMIProperties.Items.Assign(WMIClass.WMIPropertyNames);
      end
      else ListBoxAvailableWMIProperties.Items.Add('Could not initialise property names.');
  except
    on E: Exception do
    begin
      ListBoxAvailableWMIProperties.Clear;
      ListBoxSelectedWMIProperties.Clear;
      IFNotExistCreateResultForm(MainForm);
      ResultForm.MemoQueryResult.Text := WMIClass.ErrorMsg;
    end;
  end;
end;

procedure TMainForm.ComboBoxAliasKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 13 then ComboBoxAliasSelect(Sender);
end;

//Choosing from Class
procedure TMainForm.ComboBoxWMIClassSelect(Sender: TObject);
var
  i :integer;
begin
  try
    ListBoxAvailableWMIProperties.Clear;
    ListBoxSelectedWMIProperties.Clear;
    ResultForm.MemoQueryResult.Clear;

    for i := 0 to WMIClass.WMIClassNames.Count-1 do
    begin
       //Assigning Class and displaying Alias
         if trim(WMIClass.WMIClassNames.ValueFromIndex[i]).Equals(Trim(ComboBoxWMIClass.Text)) then
            begin
            ComboBoxAlias.Text :=  WMIClass.WMIClassNames.Names[i] ;
            break ;
            end
         else
            ComboBoxAlias.Text := '<No Alias>';
    end;
    if WMIClass.ConnectToWMIService(WideString(LabelEditComputer.Text),
    WideString(LabelEditNameSpace.Text),WideString(LabelEditUser.Text),
    WideString(LabelEditPassword.Text))
      then
      begin
        //Assigning and displaying properties
        ListBoxAvailableWMIProperties.Items.Add('Waiting for property list ...');
        WMIClass.InitWMIProperties(ComboBoxWMIClass.Text);
        //ListBoxAvailableWMIProperties.Clear;
        ListBoxAvailableWMIProperties.Items.Assign(WMIClass.WMIPropertyNames);
      end
      else ListBoxAvailableWMIProperties.Items.Add('Could not initialise property names.');
  except
    on E: Exception do
    begin
      ListBoxAvailableWMIProperties.Clear;
      ListBoxSelectedWMIProperties.Clear;
      IFNotExistCreateResultForm(MainForm);
      ResultForm.MemoQueryResult.Text := WMIClass.ErrorMsg;
    end;
  end;
end;

procedure TMainForm.ComboBoxWMIClassKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 13 then ComboBoxWMIClassSelect(Sender);
end;

//Selecting properties
procedure TMainForm.ButtonArrowRightClick(Sender: TObject);
begin
  MoveItemsToListBox(ListBoxSelectedWMIProperties,ListBoxAvailableWMIProperties,
  ListBoxSelectedWMIProperties.Count);
end;

//Removing selected properties
procedure TMainForm.ButtonArrowLeftClick(Sender: TObject);
begin
  MoveItemsToListBox(ListBoxAvailableWMIProperties,ListBoxSelectedWMIProperties,
  ListBoxAvailableWMIProperties.Count);
end;

//Quit
procedure TMainForm.ButtonQuitClick(Sender: TObject);
begin
  MainForm.Close;
end;

//Filling Class and Alias list
procedure TMainForm.fillComboBoxWMIClass();
var
  i : integer;
begin
  ComboBoxWMIClass.Items.Clear; //Alle vorhandenen Auswahlmöglichkeiten löschen
  //Here are displayed all the classes;
  //and for the ones without Alias, an empty alias field is displayed
  for i := 1 to WMIClass.WMIClassesResult.Count-3 do
  begin
    //ComboBoxAlias.Items.Add(WMIClass.WMIClassNames.Names[i]);
    //ComboBoxWMIClass.Items.Add(WMIClass.WMIClassNames.ValueFromIndex[i]);

    ComboBoxWMIClass.Items.Add(WMIClass.WMIClassesResult[i]);
  end;
  for i := 0 to WMIClass.WMIClassNames.Count-1 do
  begin
    ComboBoxAlias.Items.Add(WMIClass.WMIClassNames.Names[i]);
  end;
end;


function TMainForm.GetWMIPropertiesFromListBox(): string;
var i:integer;
begin
  result := '';
  for i:= 0 to ListBoxSelectedWMIProperties.Items.Count-2 do
    result := result + ListBoxSelectedWMIProperties.Items[i]+',';
  if ListBoxSelectedWMIProperties.Items.Count-1 >=0 then
     result := result + ListBoxSelectedWMIProperties.Items[ListBoxSelectedWMIProperties.Items.Count-1];//letzter Eintrag ohne ,
end;


procedure TMainForm.MoveItemsToIndex(LB: TListBox; NewIndex: Integer);
var
  L : TStringList;
  i: Integer;
begin
  LB.Items.BeginUpdate;
  try
    L:=TStringList.Create;
  try
    for i := LB.Count-1 downto 0 do
      if LB.Selected[i] then
        begin
          L.AddObject(LB.Items[i],LB.Items.Objects[i]);
          LB.Items.Delete(i);
          if i <= NewIndex then Dec(NewIndex);
        end;
    for i := 0 to L.Count-1 do
      LB.Items.InsertObject(NewIndex, L[i], L.Objects[i]);
  finally
    L.Free;
  end;
  finally
    LB.Items.EndUpdate;
  end;
end;


procedure TMainForm.MoveItemsToListBox(LBTo, LBFrom: TListBox;
  NewIndex: Integer);
var
  i :Integer;
begin
  if NewIndex = -1 then NewIndex := 0;
  LBFrom.Items.BeginUpdate;
  try
    LBTo.Items.BeginUpdate;
    try
      for i := LBFrom.Count -1 downto 0 do
        if LBFrom.Selected[i] then
        begin
          LBTo.Items.InsertObject(NewIndex,LBFrom.Items[i], LBFrom.Items.Objects[i]);
          LBFrom.Items.Delete(i);
        end;
    finally
      LBTo.Items.EndUpdate;
    end;
  finally
    LBFrom.Items.EndUpdate;
  end;
end;


procedure TMainForm.IfNotExistCreateResultForm(Sender: TObject);
begin
  if ResultForm = nil then TResultForm.Create(Sender as TComponent);
  if not ResultForm.Visible then ResultForm.Show;
end;


procedure TMainForm.FormCreate(Sender: TObject);
begin
   WMIClass := TWMIClass.Create;
   //WMIPropertyNames := TStringList.Create;
   //MemoQueryResult.Text := WMIClass.WMIClassNames.Text;
   //MemoQueryResult.Append(IntToStr(WMIClass.WMIClassNames.Count));
   FillComboBoxWMIClass(); //ComboBox mit Auswahlmöglichkeiten füllen.
end;


procedure TMainForm.FormDestroy(Sender: TObject);
begin
  WMIClass.Free;
end;


procedure TMainForm.ListBoxAvailableWMIPropertiesDragDrop(Sender,
  Source: TObject; X, Y: Integer);
var
  LBFrom, LBTo: TListBox;
begin
  LBFrom := (Source as TListBox);
  LBTo :=(Sender as TListBox);
  if LBTo = LBFrom then MoveItemsToIndex(LBTo, LBTo.GetIndexAtXY(X,Y))
                   else MoveItemsToListBox(LBTo, LBFrom, LBTo.GetIndexAtXY(X,Y));
end;

procedure TMainForm.ListBoxAvailableWMIPropertiesDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source =  ListBoxAvailableWMIProperties) or (Source =  ListBoxSelectedWMIProperties);
end;

procedure TMainForm.ListBoxSelectedWMIPropertiesDragDrop(Sender,
  Source: TObject; X, Y: Integer);
var
  LBFrom, LBTo: TListBox;
begin
  LBFrom := (Source as TListBox);
  LBTo :=(Sender as TListBox);
  if LBTo = LBFrom then MoveItemsToIndex(LBTo, LBTo.GetIndexAtXY(X,Y))
                   else MoveItemsToListBox(LBTo, LBFrom, LBTo.GetIndexAtXY(X,Y));
end;


procedure TMainForm.ListBoxSelectedWMIPropertiesDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source =  ListBoxAvailableWMIProperties) or (Source =  ListBoxSelectedWMIProperties);
end;

procedure TMainForm.ToggleBoxExpertModeChange(Sender: TObject);
begin
  if ToggleBoxExpertMode.Checked then
  begin
    LabelEditComputer.Enabled := True;
    LabelEditUser.Enabled:= True;
    LabelEditPassword.Enabled := True;
  end
  else
  begin
    LabelEditComputer.Enabled := False;
    LabelEditUser.Enabled:= False;
    LabelEditPassword.Enabled := False;
  end;
end;

end.

//Container
(*
function TMainForm.fGetWMIClassNames(var StrList: TStringList): TStringList;
var
  WMIProperties: TStringList;
  i: integer;
begin
  try
    WMIProperties := TStringList.Create;
    WMIProperties.AddStrings(['FriendlyName','Target']);
    if osGetWMI('root\cli','Msft_CliAlias',WMIProperties,'',StrList,errormsg) then
    begin
      i := 0; //counter for loop
      //extraction Aliases and WMIClassNames from resulting StringList
      while i < StrList.Count-1 do
      begin
        StrList[i] := StrList[i].Remove(0,Length('FriendlyName='));
        StrList[i] := StrList[i] + '=' + StrList[i+1].Remove(0,Length('Target=Select * from '));
        StrList.Delete(i+1);
        i := i + 1;
      end;
      result := StrList;
    end
    else
    begin
      ShowMessage('Failed! '+ errormsg);
    end;
  finally
    WMIProperties.Free;
  end;
end;


procedure ReadFromFileIntoComboBox();
var
  f: TextFile;
  s,s1 : String;
begin
  ComboBoxWMIClass.Items.Clear; //Alle vorhandenen Auswahlmöglichkeiten löschen
  try
    AssignFile(f,'alias_classnames1.txt');
    Reset(f);
    ReadLn(f);
    while not EOF(f) do
    begin
      ReadLn(f,s);
      s1 := s;
      Delete(s,Pos(' ',s),Length(s)); //s beinhaltet jetzt nur noch Alias
      s := Trim(s);
      ComboBoxAlias.Items.Add(s);
      Delete(s1,1,Pos(' ',s1));  //s1 beinhaltet WMIClass
      s1 := Trim(s1);
      ComboBoxWMIClass.Items.Add(s1);
    end;
  finally
    CloseFile(f);
  end;
end;

procedure TMainForm.ButtonExecuteClick(Sender: TObject);
var
  myWMIResultList : TStringList;
  //i : integer;
begin
  try
  MemoQueryResult.Clear;
  myWMIResultList := TStringList.Create;
  if (ComboBoxWMIClass.Text = 'or select WMI Class') or (ComboBoxWMIClass.Text = '') then
     begin
       MemoQueryResult.Text := 'Please choose a WMI Class!';
     end
  else
  begin
  if not osGetWMI(LabelEditNamespace.Text, ComboBoxWMIClass.Text,ListBoxSelectedWMIProperties.Items,EditCondition.Text,myWMIResultList,errormsg) then
     begin
       MemoQueryResult.Append('Failed');
       MemoQueryResult.Append(errormsg);
     end
  else
    begin
      MemoQueryResult.Text:= myWMIResultList.Text;
      MemoQueryResult.Append('Number of Lines: '+inttostr(myWMIResultList.Count));
    end;
  end;
  (*
    for i := 0 to Pred(myWMIResultList.Count) do
    begin
     MemoQueryResult.Append(myWMIResultList.Strings[i]);
    end;
    *)
  finally
    myWMIResultList.Free;
  end;
end;

procedure TMainForm.ButtonTestClick(Sender: TObject);
var
  //WMIClass: TWMIClass;
  WMIProperties: String;
begin
  if (ComboBoxWMIClass.Text = 'or select WMI Class') or (ComboBoxWMIClass.Text = '') then
  begin
    ResultForm.MemoQueryResult.Text := 'Please select a WMI Class!';
  end
  else
  begin
    WMIProperties := GetWMIPropertiesFromListBox();
    //MemoWMIPropertyNames.Append(WMIProperties);//just for testing
    try
    if WMIClass.ConnectToWMIService(WideString(LabelEditComputer.Text),
    WideString(LabelEditNameSpace.Text),WideString(LabelEditUser.Text),
    WideString(LabelEditPassword.Text)) then
      begin
        WMIClass.RequestToWMIService(WMIProperties,ComboBoxWMIClass.Text,EditCondition.Text);
        ResultForm.MemoQueryResult.Text:= WMIClass.WMIRequestResult.Text;
      end;
    except
      on e: Exception do ResultForm.MemoQueryResult.Text := WMIClass.ErrorMsg;
    end;
  end;
end;
*)

