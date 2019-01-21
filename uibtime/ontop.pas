unit ontop;

{$MODE Delphi}

interface

uses
  {$IFDEF UNIX}
  cthreads,
   {$ENDIF}
 //{$IFDEF WINDOWS}
  //Windows,
  //{$ENDIF WINDOWS}
  LCLIntf, LCLType,
  //LMessages, Messages,
  SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs,
  ComCtrls, Buttons, DBCtrls, DB, sqldb,
  //ToolWin,
  ExtCtrls, StdCtrls,
  //MaskEdit,
  //inifiles,
  strutils,
  //Grids,
  DBGrids, treescrolldown,
  httpservice, runprocess,
  uibdatetime;

type

  { TFOnTop }

  TFOnTop = class(TForm)
    DBLCB_topten_event: TDBLookupComboBox;
    DSQueryaktprojekt: TDataSource;
    DS_topten_events: TDataSource;
    Edit1: TEdit;
    EditProjektzeit: TEdit;
    Labeluid: TLabel;
    QueryDefproj: TSQLQuery;
    QueryProjektzeit: TSQLQuery;
    QueryUserButtons: TSQLQuery;
    QueryUserEvents: TSQLQuery;
    Query_top_ten_events: TSQLQuery;
    Btn_work_description: TSpeedButton;
    BtnProjekt: TSpeedButton;
    SpeedButton1: TSpeedButton;
    BtnBye: TSpeedButton;
    TimerCallCount: TTimer;
    TimerNachfrage: TTimer;
    TimerNoDblClick: TTimer;
    TimerProjektzeit: TTimer;
    TimerScrollDisable: TTimer;
    Timer_top_ten: TTimer;
    ToolBar1: TToolBar;
    procedure DBLCB_topten_eventChange(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    function isSpecialButton(event: string): boolean;
    procedure BtnByeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BtnProjektClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure eventhandler(newevent: string);
    procedure BtnArrayClick(Sender: TObject);
    procedure TimerNachfrageTimer(Sender: TObject);
    //procedure BtnCallCountClick(Sender: TObject);
    //procedure TimerCallCountTimer(Sender: TObject);
    procedure AppDeactivate(Sender: TObject);
    procedure AppActivate(Sender: TObject);
    procedure ComboBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure TimerProjektzeitTimer(Sender: TObject);
    procedure TimerScrollDisableTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtnProjektDblClick(Sender: TObject);
    procedure setDefaultEvent;
    procedure TimerNoDblClickTimer(Sender: TObject);
    procedure ReBuildForm;
    procedure SpeedButton1Click(Sender: TObject);
    procedure buildtree(mytable: TDBDataset);
    //procedure TreeView1MouseMove(Sender: TObject; Shift: TShiftState; X,
    //  Y: Integer);
    //procedure TreeView1Click(Sender: TObject);
    procedure Edit1Enter(Sender: TObject);
    procedure Edit1KeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure Edit1KeyPress(Sender: TObject; var Key: char);
    procedure reload_topten;
    procedure Timer_top_tenTimer(Sender: TObject);
    procedure Btn_work_descriptionClick(Sender: TObject);
    procedure DBLCB_topten_eventExit(Sender: TObject);
    procedure DBLCB_topten_eventEnter(Sender: TObject);
    procedure CustomExceptionHandler(Sender: TObject; E: Exception);
  private
    { Private-Deklarationen}
  public
    { Public-Deklarationen}
    ineventhandler: boolean;
    ineditmode: boolean;
    intoptenbox: boolean;
    //showallprojects: boolean;
  end;

var
  FOnTop: TFOnTop;
  lastevent: string;
  aktlist: TStringList;
  commentlist: TStringList;
  nodelist: TStringList;
  oldselnode: TTreeNode;
  oldselstr, newtyped: string;
  editready: boolean;

implementation

uses uibdata, login, logoff, work_description, nachf, loggedin_;
///logoff, nachf, login, debug{, loggedin_}, work_description;

{$R *.lfm}
var

  ontopactivated: boolean;
  BtnArray: array [0..20] of TSpeedButton;
  //BtnCallCount: TSpeedButton;
  //DBEdit1: TDBEdit;
  specialButtonList: TStringList;
  wasInDblclick: boolean;
  inLogoff: boolean = False;



(****************************************************************
Hilfsfunktionen
***********************************************************)

function TFOnTop.isSpecialButton(event: string): boolean;
begin
  try
    if specialButtonList.IndexOf(event) = -1 then
      Result := False
    else
      Result := True;
  except
    datamodule1.debugOut(5, 'except in TFOnTop.isSpecialButton');
  end;
end;

procedure TFOnTop.DBLCB_topten_eventChange(Sender: TObject);
begin

end;

procedure TFOnTop.Edit1Change(Sender: TObject);
begin
  oldselstr := Edit1.Text;
end;

procedure TFOnTop.FormDeactivate(Sender: TObject);
begin
  datamodule1.debugOut(5, 'start TFOnTop.FormDeactivate');
end;

procedure TFOnTop.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to specialButtonList.Count - 1 do
  begin
    BtnArray[i].Destroy;
  end;
  //stophttpserver;
end;


procedure TFOnTop.reload_topten;
begin
  datamodule1.debugOut(5, 'start TFOnTop.reload_topten');

  if Query_top_ten_events.Active then
    Query_top_ten_events.Close;
  Query_top_ten_events.ParamByName('userid').AsString := uid;
  Query_top_ten_events.ParamByName('inittime').AsDate := (Date - 30);
  Query_top_ten_events.Open;
  datamodule1.debugOut(5, 'finished TFOnTop.reload_topten');
end;


(************************************************************
Create show activate.....
*********************************************)

procedure TFOnTop.ReBuildForm;
var
  //myini: TIniFile;
  leftint, i, iconlength: integer;
  //mypath: string;
  //Hour, Min, Sec, MSec: word;
  //ringIcon: TBitmap;
  buttonwidth: integer;
begin
  datamodule1.debugOut(5, 'start TFOnTop.ReBuildForm');
  ontopactivated := False;
  for i := 0 to specialButtonList.Count - 1 do
  begin
    BtnArray[i].Destroy;
  end;
  specialButtonList.Clear;
  (*
  if BtnCallCount <> nil then
  begin
    BtnCallCount.Destroy;
    for i := 0 to specialButtonList.Count - 1 do
    begin
      BtnArray[i].Destroy;
    end;
    specialButtonList.Clear;
  end;
  *)
  Toolbar1.Height := DataModule1.getOntopHeight;
  if QueryUserButtons.Active then
    QueryUserButtons.Close;
  QueryUserButtons.SQL.Clear;
  QueryUserButtons.SQL.Add('select a.event as event ');
  QueryUserButtons.SQL.Add('from uibuserevent a, uiballevent b');
  QueryUserButtons.SQL.Add(' where (userid = :uid) and (button=1)');
  QueryUserButtons.SQL.Add(' and (a.event=b.event)');
  QueryUserButtons.ParamByName('uid').AsString := uid;
  QueryUserButtons.Open;
  i := 0;
  buttonwidth := 0;
  if QueryUserButtons.RecordCount > 0 then
  begin
    repeat
      BtnArray[i] := TSpeedButton.Create(toolbar1);
      BtnArray[i].Parent := toolbar1;
      BtnArray[i].Caption := QueryUserButtons.FieldByName('event').AsString;
      //   BtnArray[i].Glyph.Assign(QueryUserButtons.fieldbyname('icon'));
      //   BtnArray[i].NumGlyphs := 2;
      //   if QueryUserButtons.fieldbyname('icon').isNull then
      iconlength := 0;
      //   else
      //    iconlength := 20;
      BtnArray[i].Width :=
        round(length(QueryUserButtons.FieldByName('event').AsString) *
        5.4 + 14 + iconlength);
      {$IFDEF LINUX}
      BtnArray[i].Width := round(BtnArray[i].Width * scalefactor*1.4);
      {$ENDIF LINUX}
      buttonwidth := buttonwidth + BtnArray[i].Width;
      BtnArray[i].OnClick := BtnArrayClick;
      BtnArray[i].GroupIndex := 1;
      specialButtonList.Add(QueryUserButtons.FieldByName('event').AsString);
      QueryUserButtons.Next;
      Inc(i);
    until (QueryUserButtons.EOF);
  end;
  QueryUserButtons.Close;
  // Put ring Button and Counter to the left side of the toolbar
  //DBEdit1 := TDBEdit.Create(toolbar1);
  reload_topten;


  // Größe zurecht setzten
  //Clientheight := Toolbar1.height;
  //height := Toolbar1.height;
  // height := 360;
  //ToolBar1.Refresh;
  leftint := Datamodule1.getLeftint();
  Left := leftint;
  Width := 816 + buttonwidth;
  Toolbar1.Width := 816 + buttonwidth;
  //toolbar1.height :=  round(25 * (screen.PixelsPerInch / 96));
  //toolbar1.height :=  100;
  //FOnTop.Height:= 100;
  EditProjektzeit.Height:=ontopheight;
  Datamodule1.setontopwidth(Toolbar1.Width);
  eventhandler(lastevent);
  datamodule1.debugOut(5, 'finished TFOnTop.ReBuildForm');
end;


procedure TFOnTop.FormCreate(Sender: TObject);
var
  //myini: TIniFile;
  //leftint, i, iconlength: integer;
  //mypath: string;
  Hour, Min, Sec, MSec: word;
  //ringIcon: TBitmap;
begin
  try
    datamodule1.debugOut(5, 'start TFOnTop.FormCreate');

    //mypath := ExtractFilePath(paramstr(0));
    //myini := TIniFile.Create(mypath+'uibtime.ini');
    //leftint := myini.ReadInteger('desktop', 'left', 200);
    //myini.Free;
    edit1.Text := '';
    lastevent := '';
    //Application.OnException := CustomExceptionHandler;
    Application.OnDeactivate := AppDeactivate;
    Application.OnActivate := AppActivate;
    ontopactivated := False;
    // calltimer auf zeit bis zu nächsten vollen Stunde
    decodetime(time, Hour, Min, Sec, MSec);
    ///TimerCallCount.interval := 3600000 - (msec + (sec + (min * 60)) * 1000);
    Top := 0;
    ineventhandler := True;
    //TimerCallCount.enabled := true;    wird in login enabled
    ineditmode := False;

    // new special buttons
    specialButtonList := TStringList.Create;

    rebuildform;

    wasInDblclick := False;
    //showallprojects := Datamodule1.getshowallprojects();
    if DataModule1.geteditonly then
    begin
      ineventhandler := False;
      ///TimerCallCount.Enabled := False;
      //Position := poDesktopCenter;
      Top := 200;
      ineditmode := True;
    end;
    starthttpserver;

    datamodule1.debugOut(5, 'finished TFOnTop.FormCreate');

  except
    datamodule1.debugOut(5, 'except in TFOnTop.FormCreate');
  end;
end;

procedure TFOnTop.AppDeactivate(Sender: TObject);
begin
  try
    datamodule1.debugOut(5, 'start AppDeactivate');
    if not ineditmode then
    begin
      datamodule1.debugOut(5, 'AppDeactivate not ineditmode');
      //if ComboBox.Enabled and (ComboBox.Items[ComboBox.ItemIndex] <> '')
      if intoptenbox and (not inLogoff) then
      begin
        datamodule1.debugOut(5, 'AppDeactivate: was in toptenbox');
        intoptenbox := False;
        if (lastevent = 'Pause') then
          Datamodule1.setloggedin(True);
        if Datamodule1.getloggedin then
        begin
          eventhandler(DBLCB_topten_event.Text);
          //ComboBox.Enabled := false;
          TimerScrollDisable.Enabled := False;
        end;
      end
      else
      begin
        if Edit1.Enabled and (Edit1.Text <> '') and (not inLogoff) then
        begin
          datamodule1.debugOut(5, 'AppDeactivate: CB enabled and != ""');
          if (lastevent = 'Pause') then
            Datamodule1.setloggedin(True);
          if Datamodule1.getloggedin then
          begin
            eventhandler(Edit1.Text);
            //ComboBox.Enabled := false;
            TimerScrollDisable.Enabled := False;
          end;
        end;
      end;
    end;
  except
    datamodule1.debugOut(3, '', 'exception in appdeactivate');
    raise;
  end;
end;

procedure TFOnTop.AppActivate(Sender: TObject);
begin
  try
    datamodule1.debugOut(5, 'start AppActivate');
    if not ineditmode then
    begin
      datamodule1.debugOut(5, 'AppActivate not ineditmode');
      //if ComboBox.Enabled and (ComboBox.Items[ComboBox.ItemIndex] <> '')
      if not inLogoff then
      begin
        datamodule1.debugOut(5, 'AppActivate: is in toptenbox');
        //intoptenbox := false;
        if (lastevent = 'Pause') then
          Datamodule1.setloggedin(True);
        if Datamodule1.getloggedin then
        begin
          DBLCB_topten_eventEnter(Sender);
          //ComboBox.Enabled := false;
          TimerScrollDisable.Enabled := False;
        end;
      end
      else;
    end;
  except
    datamodule1.debugOut(3, '', 'exception in appdeactivate');
    raise;
  end;
end;

procedure TFOnTop.FormActivate(Sender: TObject);
begin
  datamodule1.debugOut(5, 'Activate FOntop');
end;


procedure TFOnTop.FormShow(Sender: TObject);
//var
//  Hour, Min, Sec, MSec: word;
begin
  datamodule1.debugOut(5, 'ontop', 'Show FOntop');
  if not setwindowtoalldesktops('FOntop') then
    datamodule1.debugOut(2, 'ontop', 'failed FOntop to all desktops');
  try
    if not ontopactivated then
    begin
      ontopactivated := True;
      if not (uid = 'admin') then
      begin
        Edit1.Enabled := False;
        queryUserEvents.Close;
        queryUserEvents.ParamByName('uid').AsString := uid;
        queryUserEvents.Open;
        setDefaultEvent;
        (*
        // startzeit für calls auf volle Stunde stellen
        decodetime(time, Hour, Min, Sec, MSec);
        if not Datamodule1.SQuibcalls.Active then
          Datamodule1.SQuibcalls.Open;
        Datamodule1.SQuibcalls.append;
        Datamodule1.SQuibcalls.FieldByName('userid').AsString := uid;
        Datamodule1.SQuibcalls.FieldByName('day_h').AsDateTime :=
          date + encodetime(Hour, 0, 0, 0);
        Datamodule1.SQuibcalls.FieldByName('calls').AsInteger := 0;
        *)
      end;
      //FLogin.Free;    /// do not free here - we have to come back to FLogin !
      if not Datamodule1.geteditonly then
        Datamodule1.TimerOntop.Enabled := True;
    end;
  except
    datamodule1.debugOut(3, '', 'exception in ontopactivate');
    raise;
  end;
  ReBuildForm;
end;


(****************************************************
Button eventhandler
****************************************************)

procedure TFOnTop.setDefaultEvent;
var
  defproj: string;
  newevent: string;
begin
  if not Datamodule1.geteditonly then
  begin
    try
      datamodule1.debugOut(5, 'start setdef');
      if (not (uid = 'admin')) or (Datamodule1.geteditonly) then
      begin
        querydefproj.Active := False;
        querydefproj.ParamByName('uid').AsString := uid;
        querydefproj.Open;
        defproj := querydefproj.FieldByName('event').AsString;
        querydefproj.Close;
        if defproj = '' then
          newevent := 'Allgemein'
        else
          newevent := defproj;
        eventhandler(newevent);
      end;

    except
      datamodule1.debugOut(3, '', 'exception in setDefaultEvent');
      raise;
    end;
  end;
end;


procedure TFOnTop.BtnByeClick(Sender: TObject);
var
  Result: integer;
  loggedin_visible: boolean;
begin
  datamodule1.debugOut(5, 'BtnByeClick', 'start');
  // hier nicht in den Nil greifen
  while not datamodule1.getfloggedin_created do
    sleep(100);
  loggedin_visible := FLoggedin.Visible;
  if loggedin_visible then
    FLoggedin.hide;
  try
    if not Datamodule1.geteditonly then
    begin
      if not (uid = 'admin') then
      begin
        Datamodule1.SQuibevent.last;
        Datamodule1.SQuibevent.edit;
        datamodule1.debugOut(5, timetostr(now) + ' byeedit ' + lastevent);
        //Datamodule1.SQuibevent.FieldByName('event').AsString := lastevent;
        Datamodule1.SQuibevent.FieldByName('stoptime').AsDateTime := now;
        try
          Datamodule1.SQuibevent.post;
          //DataModule1.SQuibevent.ApplyUpdates;
        except
          Datamodule1.SQuibevent.post;
          //DataModule1.SQuibevent.ApplyUpdates;
        end;
        datamodule1.debugOut(5, 'in btnbye: set exit time');
        Datamodule1.setloggedin(False);
      end;
    end;
    TimerNachfrage.Enabled := False;
    inLogoff := True;
    if Assigned(Flogoff) then  datamodule1.debugOut(5, 'in btnbye: Flogoff exists');
    if Flogoff.Visible then Flogoff.Visible:=false;
    datamodule1.debugOut(5, 'in btnbye: Flogoff.showmodal');
    Application.ProcessMessages;
    try
      {$IFDEF WINDOWS}
      Result := Flogoff.showmodal;
      {$ENDIF WINDOWS}
      {$IFDEF LINUX}
      Flogoff.show;
      Flogoff.WindowState:=wsMaximized;
      Flogoff.FormStyle:=fsSystemStayOnTop;
      Flogoff.FLogofftimer.Enabled:=true;
      while Flogoff.Visible do
      begin
        Application.ProcessMessages;
        Sleep(100);
      end;
      Result := Flogoff.mymodresult;
      Flogoff.WindowState:=wsNormal;
      Flogoff.FormStyle:=fsNormal;
      Flogoff.FLogofftimer.Enabled:=false;
      {$ENDIF LINUX}
    finally
      datamodule1.debugOut(5, 'in btnbye: after Flogoff.showmodal');
    end;
    datamodule1.debugOut(5, 'in btnbye: logoff result: '+ModalResultStr[result]);
    //Result := Flogoff.show;
    inLogoff := False;
    if loggedin_visible and (Result <> mrAbort) then
      FLoggedin.Show;
    if not setwindowtoalldesktops('Presenz') then
      datamodule1.debugOut(2, 'ontop', 'failed presenz to all desktops');
    // back again (from break)
    if Result = mrOk then
    begin
      Datamodule1.setloggedin(True);
      lastevent := '';
      setDefaultEvent;
      Datamodule1.TimerOnTop.Enabled := True;
      //BtnProjekt.down := true;
      //BtnProjekt.click;
    end;
    // never was absent
    if Result = mrCancel then
    begin
      Datamodule1.SQuibevent.last;
      //  Datamodule1.SQuibevent.edit;
      Datamodule1.setloggedin(True);
      TimerNachfrage.Enabled := True;
      Datamodule1.TimerOnTop.Enabled := True;
      eventhandler(lastevent);
    end;
    // exit program
    if Result = mrAbort then
    begin
      //this is the end my friend
      login.automatic := False;
      DataModule1.TerminateApplication;
    end;
    if Result = mrYes then
    begin
      //show work description
      Datamodule1.SQuibevent.last;
      //  Datamodule1.SQuibevent.edit;
      Datamodule1.setloggedin(True);
      TimerNachfrage.Enabled := True;
      eventhandler(lastevent);
      Datamodule1.TimerOnTop.Enabled := True;
      if Fwork_description = nil then
        Fwork_description := TFwork_description.Create(self);
      Fwork_description.Show;
    end;

  except
    on e: Exception do
    begin
      Datamodule1.debugOut(3, '', 'exception in logoff:BtnByeClick');
      Datamodule1.debugOut(3, e.Message);
      raise;
    end;
  end;
  datamodule1.debugOut(5, 'BtnByeClick', 'end');
end;

procedure TFOnTop.eventhandler(newevent: string);
var
  defproj: string;
  specialbutton: boolean;
begin
  datamodule1.debugOut(5, 'eventhandler', 'start');
  if not Datamodule1.geteditonly then
  begin
    try
      if not Datamodule1.SQuibevent.Active then
        Datamodule1.SQuibevent.Open;
      ineventhandler := True;
      datamodule1.debugOut(5, 'eventhandler', timetostr(now) +
        ' evstart >' + lastevent + '>/' + '->' + newevent + '<');
      specialbutton := isSpecialButton(newevent);
      Edit1.Enabled := False;
      if (not ((uid = 'admin') or (Datamodule1.geteditonly) or (newevent = ''))) then
      begin
        datamodule1.debugOut(5, 'eventhandler', timetostr(now) +
          ' evnewp ' + lastevent + '/' + '->' + newevent);
        if specialbutton then
        begin
          datamodule1.debugOut(5, timetostr(now) + ' is specialbutton->' + newevent);
          if specialButtonList.IndexOf(newevent) > -1 then
          begin
            BtnArray[specialButtonList.IndexOf(newevent)].down := True;
            Edit1.Text := newevent;
          end;
        end
        else
        begin
          datamodule1.debugOut(5, 'eventhandler', timetostr(now) +
            ' is no specialbutton->' + newevent);
          BtnProjekt.Down := True;
          //ComboBox.ItemIndex :=  ComboBox.Items.IndexOf(newevent);
          Edit1.Text := newevent;
        end;
        // haben wir was neues
        if (newevent <> lastevent) then
        begin
          if lastevent <> '' then // gibts einen aktuellen ?
          begin
            datamodule1.debugOut(5, 'eventhandler', timetostr(now) +
              ' eveditlast ' + lastevent);
            Datamodule1.SQuibevent.edit;
            Datamodule1.SQuibevent.FieldByName('event').AsString := lastevent;
            Datamodule1.SQuibevent.FieldByName('stoptime').AsDateTime := now;
            try
              Datamodule1.SQuibevent.post;
              //DataModule1.SQuibevent.ApplyUpdates;
              if not Datamodule1.SQuibevent.Active then
                Datamodule1.SQuibevent.Open;
            except
              Datamodule1.SQuibevent.post;
              //DataModule1.SQuibevent.ApplyUpdates;
              if not Datamodule1.SQuibevent.Active then
                Datamodule1.SQuibevent.Open;
            end;
            Datamodule1.SQuibevent.last;
            FOnTop.TimerProjektzeitTimer(FOnTop);
          end;

          // Neuen Datensatz anfügen
          // Sind wir neu hier ? -> Default holen
          if lastevent = '' then
          begin
            (*
            querydefproj.Active := False;
            querydefproj.ParamByName('uid').AsString := uid;
            querydefproj.Open;
            defproj := querydefproj.FieldByName('event').AsString;
            querydefproj.Close;
            if defproj = '' then
              defproj := 'Allgemein';
            lastevent := defproj;
            *)
          end;
          datamodule1.debugOut(5, 'eventhandler', timetostr(now) +
            ' evappend ' + lastevent + '/' + '->' + newevent);
          Datamodule1.SQuibevent.append;
          Datamodule1.SQuibevent.FieldByName('userid').AsString := uid;
          Datamodule1.SQuibevent.FieldByName('event').AsString := newevent;
          Datamodule1.SQuibevent.FieldByName('starttime').AsDateTime := now;
          Datamodule1.SQuibevent.FieldByName('stoptime').AsDateTime := now;
          if specialbutton then
          begin
            datamodule1.debugOut(5, 'eventhandler', timetostr(now) +
              ' is specialbutton->' + newevent);
            if specialButtonList.IndexOf(newevent) > -1 then
              BtnArray[specialButtonList.IndexOf(newevent)].down := True;
          end
          else
          begin
            datamodule1.debugOut(5, 'eventhandler', timetostr(now) +
              ' is no specialbutton->' + newevent);
            BtnProjekt.Down := True;
            Edit1.Text := newevent;
          end;
          datamodule1.debugOut(5, 'eventhandler', timetostr(now) +
            ' evpostnew ' + lastevent + '/' + '->' + newevent);
          Datamodule1.SQuibevent.post;
          try
            DataModule1.SQuibevent.ApplyUpdates;
          except
            datamodule1.debugOut(2, 'eventhandler',
              'exception in eventhandler.SQuibevent.ApplyUpdates');
          end;
          Datamodule1.SQuibevent.last;
          FOnTop.TimerProjektzeitTimer(FOnTop);
          if not Datamodule1.SQuibevent.Active then
            Datamodule1.SQuibevent.Open;
          lastevent := newevent;
          // Timeout für Nachfrage setzen
          Datamodule1.SQquerytimeout.parambyname('userid').AsString := uid;
          Datamodule1.SQquerytimeout.parambyname('event').AsString := newevent;
          Datamodule1.SQquerytimeout.Open;
          if Datamodule1.SQquerytimeout.RecordCount = 0 then
          begin
            Datamodule1.SQquerytimeout.Close;
            Datamodule1.SQquerytimeout.parambyname('userid').AsString := uid;
            Datamodule1.SQquerytimeout.parambyname('event').AsString := defproj;
            Datamodule1.SQquerytimeout.Open;
          end;
          if Datamodule1.SQquerytimeout.FieldByName('timeout_min').AsInteger = 0 then
          begin
            TimerNachfrage.interval := 10 * 60 * 1000;
          end
          else
          begin
            TimerNachfrage.interval :=
              Datamodule1.SQquerytimeout.FieldByName('timeout_min').AsInteger *
              60 * 1000;
          end;
          datamodule1.debugOut(5, 'eventhandler', 'Timeout auf ' +
            IntToStr(TimerNachfrage.Interval div 60000) + ' Minuten.');
          TimerNachfrage.Enabled := True;
          Datamodule1.SQquerytimeout.Close;
        end;
        // loggedin setzen - false bei pause
        if newevent = 'Pause' then
          Datamodule1.setloggedin(False)
        else
          Datamodule1.setloggedin(True);
      end;

      ineventhandler := False;
      datamodule1.debugOut(5, 'eventhandler', timetostr(now) +
        ' evleave ' + lastevent + '/' + '->' + newevent);
    except
      datamodule1.debugOut(2, 'eventhandler', 'exception in eventhandler');
      raise;
    end;
  end;
  datamodule1.debugOut(5, 'eventhandler', 'finished');
end;

procedure TFOnTop.BtnArrayClick(Sender: TObject);
begin
  if FTreeview.Visible then
    FTreeview.Hide;
  eventhandler((Sender as TSpeedbutton).Caption);
end;

(*
procedure TFOnTop.BtnCallCountClick(Sender: TObject);
var
  btnIndex: integer;
begin
  try
    eventhandler('Call');
    if isSpecialButton('Call') then
    begin
      btnIndex := specialButtonList.IndexOf('Call');
      BtnArray[btnIndex].Down := True;
    end
    else
    begin
      BtnProjekt.Down := True;
    end;
    Datamodule1.SQuibcalls.FieldByName('calls').AsInteger :=
      Datamodule1.SQuibcalls.FieldByName('calls').AsInteger + 1;
  except
    datamodule1.debugOut(3,'', 'exception in BtnCallCountClick');
    raise;
  end;
end;
*)

procedure TFOnTop.BtnProjektClick(Sender: TObject);
begin
  try
    datamodule1.debugOut(5, 'start of BtnProjektClick');
    if wasInDblclick then
      wasInDblclick := False
    else
    begin
      if Edit1.Text <> lastevent then
        eventhandler(Edit1.Text)
      else
      begin
        TimerNoDblClick.Enabled := True;
      end;
    end;
    datamodule1.debugOut(5, 'end of BtnProjektClick');
  except
    datamodule1.debugOut(3, '', 'exception in BtnProjekt');
    raise;
  end;
end;

procedure TFOnTop.BtnProjektDblClick(Sender: TObject);
begin
  try
    datamodule1.debugOut(5, 'start of BtnProjektDblClick');
    TimerNoDblClick.Enabled := False;
    wasInDblclick := True;
    FTreeview.remotechange := True;
    activate;
    //  DBEdit1.setfocus;
    Application.ProcessMessages;
    TimerScrollDisable.Enabled := True;
    Edit1.Enabled := True;
    Application.ProcessMessages;
    //showallprojects := Datamodule1.getshowallprojects();
    if Datamodule1.getshowallprojects() then
    begin
      datamodule1.debugOut(6, 'buildtree allprojects');
      DataModule1.SQuibaktevent.Refresh;
      buildtree(DataModule1.SQuibaktevent);
    end
    else
    begin
      if QueryUserEvents.Active then
        QueryUserEvents.Close;
      QueryUserEvents.Open;
      datamodule1.debugOut(6, 'buildtree myprojects');
      buildtree(QueryUserEvents);
    end;
    //eventhandler(BtnProjekt.Caption,false);
    Edit1.Enabled := True;
    // ComboBox.dropdown;
    // ComboBox.closeup(true);
    FTreeview.remotechange := False;
    datamodule1.debugOut(5, 'end of BtnProjektDblClick');

  except
    datamodule1.debugOut(3, '', 'exception in BtnProjektDblClick');
    raise;
  end;
end;

procedure TFOnTop.ComboBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);

begin
  try
    lastevent := lastevent;
  except
    datamodule1.debugOut(3, '', 'exception in DBLCB1Mousedown');
    raise;
  end;
end;



(*********************************************************************
Timer Timer Timer Timer Timer Timer Timer Timer Timer Timer
**********************************************************************)
function minute2str(minutes: integer): string;
begin
  if minutes < 10 then
    minute2str := ':0' + IntToStr(minutes)
  else
    minute2str := ':' + IntToStr(minutes);
end;


procedure TFOnTop.TimerProjektzeitTimer(Sender: TObject);
var
  available_min, total_min, used_min: integer;
  total, used, available: double;
  //base: string;
  projektstart: TDatetime;
  endmonth, year, endyear, month, day, startyear, startmonth, startday,
  aktstartyear, aktstartmonth, aktstartday: word;
  helperint: integer;
  suchevent: string;
  monthsmod, monthdiv, basemonth, acc_per_monthnum_int: integer;
  acc_per_monthnum: double;
  lastIntervalStart : TDateTime;
  quota_lifetime_month : integer;
  messagelist : TStringlist;
begin
  aktstartyear := 2001;
  aktstartmonth := 1;
  DataModule1.debugOut(6, 'enter TimerProjektzeitTimer');
  suchevent := edit1.Text;
  if QueryProjektzeit.Active then
    QueryProjektzeit.Close;
  //QueryProjektzeit.databasename :='uibtime';
  QueryProjektzeit.SQL.Clear;
  QueryProjektzeit.sql.Add(' select time_h, acc_per_monthnum, projectstart, quota_lifetime_month ');
  QueryProjektzeit.sql.Add('    from uibaktevent');
  QueryProjektzeit.sql.Add('    where (event = :suchevent)');
  QueryProjektzeit.parambyname('suchevent').AsString := suchevent;
  QueryProjektzeit.Open;
  if not QueryProjektzeit.FieldByName('time_h').IsNull then
  begin
    total := QueryProjektzeit.FieldByName('time_h').AsFloat;
    acc_per_monthnum := QueryProjektzeit.FieldByName('acc_per_monthnum').AsFloat;
    basemonth := trunc(acc_per_monthnum);
    acc_per_monthnum_int := trunc(acc_per_monthnum);
    monthsmod := acc_per_monthnum_int mod 12;
    monthdiv := acc_per_monthnum_int div 12;
    projektstart := QueryProjektzeit.FieldByName('projectstart').AsDateTime;
    quota_lifetime_month := round(QueryProjektzeit.FieldByName('quota_lifetime_month').AsFloat);
    if quota_lifetime_month > 0 then
    begin
      // Stundenkontingent
      if QueryProjektzeit.Active then
      QueryProjektzeit.Close;
      QueryProjektzeit.SQL.Clear;
      QueryProjektzeit.sql.Add(' select sum(stunden) as stunden from uibevent');
      QueryProjektzeit.sql.Add('    where (event = :suchevent)');
      QueryProjektzeit.sql.Add('    and  (starttime >= :von)');
      QueryProjektzeit.sql.Add('    and (stoptime < :bis)');
      QueryProjektzeit.parambyname('suchevent').AsString := suchevent;
      QueryProjektzeit.parambyname('von').AsDateTime:=projektstart;
      QueryProjektzeit.parambyname('bis').AsString := datetostr(now+1);
    end
    else
    begin
      // here is the result for the last Interval
      lastIntervalStart := getLastIntervalStart(projektstart,now,acc_per_monthnum_int,false);
      decodeDate(lastIntervalStart, aktstartyear, aktstartmonth, aktstartday);
      DataModule1.debugOut(6, 'getLastIntervalInfo', 'lastIntervalStart :'+DateToStr(lastIntervalStart));

      if QueryProjektzeit.Active then
        QueryProjektzeit.Close;
      //QueryProjektzeit.databasename :='uibtime';
      QueryProjektzeit.SQL.Clear;
      if acc_per_monthnum_int > 0 then
      begin
        QueryProjektzeit.sql.Add(' select sum(stunden) as stunden from uibevent');
        QueryProjektzeit.sql.Add('    where (event = :suchevent)');
        QueryProjektzeit.sql.Add('    and  (starttime >= :von)');
        QueryProjektzeit.sql.Add('    and (stoptime < :bis)');
        QueryProjektzeit.parambyname('suchevent').AsString := suchevent;
        QueryProjektzeit.parambyname('von').AsString :=
          datetostr(encodedate(aktstartyear, aktstartmonth, aktstartday));
        QueryProjektzeit.parambyname('bis').AsString :=
          datetostr(now+1);
      end
      else
      begin
        // monthsmod (acc_per_monthnum) is 0
        QueryProjektzeit.sql.Add(' select sum(stunden) as stunden from uibevent');
        QueryProjektzeit.sql.Add('    where event = :suchevent;');
        QueryProjektzeit.parambyname('suchevent').AsString := edit1.Text;
      end;
    end;
    QueryProjektzeit.Open;
    used := QueryProjektzeit.FieldByName('stunden').AsFloat;
    used := used + ((now - DataModule1.SQuibevent.FieldByName('stoptime').AsFloat) * 24);
    available := total - used;
    available_min := round(abs((available - trunc(available)) * 60));
    used_min := round(abs((used - trunc(used)) * 60));
    total_min := round(abs((total - trunc(total)) * 60));
    EditProjektzeit.Hint := IntToStr(total_min) + '-' + IntToStr(
      used_min) + ' since ' + DatetoStr(lastIntervalStart);
    DataModule1.debugOut(6, 'ProjektzeitTimer: ' + FloatToStr(total) +
      '-' + FloatToStr(used) + '=' + floattostr(available));
    EditProjektzeit.Text := IntToStr(trunc(available)) + minute2str(available_min);
    EditProjektzeit.Hint := IntToStr(trunc(total)) + minute2str(
      total_min) + '-' + IntToStr(trunc(used)) + minute2str(used_min) +
      ' since 1.' + IntToStr(aktstartmonth) + '.' + IntToStr(aktstartyear);
    if available < 0 then
      EditProjektzeit.Font.Color := clRed
    else
      EditProjektzeit.Font.Color := clBlack;
    if (trunc(available) = 0) and (available_min < 5) and (available < 0) then
    begin
      {$IFDEF WINDOWS}
      DataModule1.TrayIcon1.BalloonHint:='Warnung: übrige Zeit: '+inttostr(available_min)+' Minuten';
      DataModule1.TrayIcon1.ShowBalloonHint;
      {$ENDIF WINDOWS}
      {$IFDEF LINUX}
      try
        messagelist := TStringlist.create;
        messagelist.Add('Warnung: übrige Zeit: '+inttostr(available_min)+' Minuten');
        DataModule1.ProcessTrayNotify.Parameters.AddStrings(messagelist);
        DataModule1.ProcessTrayNotify.Execute;
        messagelist.Free;
      except
        DataModule1.debugOut(3,'trayicon', 'Exception starting notify-send ');
      end;
      {$ENDIF LINUX}
    end;
  end
  else
    EditProjektzeit.Text := '';

  if QueryProjektzeit.Active then
    QueryProjektzeit.Close;

  DataModule1.debugOut(6, 'leave TimerProjektzeitTimer');
end;

procedure TFOnTop.TimerNachfrageTimer(Sender: TObject);
var
  griff: HWND;
  //szb: array[0..64] of char;

begin
  try
    griff := GetForeGroundWindow;
    ///GetWindowText(griff, szb, Sizeof(szb));
    // Lieber ausschalten damit Memo nicht überläuft
    //  datamodule1.debugOut(5,'Focus auf: '+szb);
    TimerNachfrage.Enabled := False;
    FNachfrage.timer1.Enabled := True;
    FNachfrage.timer2.Enabled := True;
    SetWindowPos(handle, HWND_TOPMOST, 100, 100, 500, 240,
      SWP_NOMOVE and SWP_NOSIZE);
    FNachfrage.Left := 100;
    FNachfrage.Top := 100;
    FNachfrage.Width := 500;
    FNachfrage.Height := 240;
    FNachfrage.showmodal;
    FNachfrage.timer1.Enabled := False;
    FNachfrage.timer2.Enabled := False;
    case (FNachfrage.modalresult) of
      mrNo:
      begin
        TimerNachfrage.interval := 60000;
        TimerNachfrage.Enabled := True;
      end;

      mrOk:
      begin
              { Wenns ok ist muss man nicht nachfragen....
              Datamodule1.SQquerytimeout.parambyname('userid').asstring := uid;
              Datamodule1.SQquerytimeout.parambyname('event').asstring
                := Datamodule1.SQuibevent.fieldbyname('event').asstring;
              Datamodule1.SQquerytimeout.open;
              TimerNachfrage.interval :=
                 Datamodule1.SQquerytimeout.fieldbyname('timeout_min').asinteger
                 * 60 * 1000;
              Datamodule1.SQquerytimeout.close;
              }
        TimerNachfrage.Enabled := True;
      end;
      mrAbort:
      begin
        TimerNachfrage.Enabled := False;
        datamodule1.debugOut(5, 'Nachfrage abgelaufen-> Timeout');
        BtnBye.down := True;
        BtnBye.click;
      end;
    end;
    SetForegroundWindow(griff);
    datamodule1.TimerOntop.Enabled := True;

  except
    datamodule1.debugOut(3, '', 'exception in TimerNachfrage');
    raise;
  end;
end;


(*
procedure TFOnTop.TimerCallCountTimer(Sender: TObject);
var
  Hour, Min, Sec, MSec: word;
begin
  try
    datamodule1.debugOut(5, timetostr(now) + 'calltimer start');
    ///   TimerCallCount.Enabled := False;
    ///   TimerCallCount.interval := 3600000;
    if Datamodule1.SQuibcalls.FieldByName('calls').AsInteger > 0 then
    begin
      datamodule1.debugOut(5, timetostr(now) + 'calltimer post');
      Datamodule1.SQuibcalls.post;
    end
    else
    begin
      datamodule1.debugOut(5, timetostr(now) + 'calltimer cancel');
      Datamodule1.SQuibcalls.cancel;
    end;
    Datamodule1.SQuibcalls.append;
    Datamodule1.SQuibcalls.FieldByName('userid').AsString := uid;
    decodetime(time, Hour, Min, Sec, MSec);
    Datamodule1.SQuibcalls.FieldByName('day_h').AsDateTime :=
      date + encodetime(Hour, 0, 0, 0);
    Datamodule1.SQuibcalls.FieldByName('calls').AsInteger := 0;
    ///    TimerCallCount.Enabled := True;
    datamodule1.debugOut(5, timetostr(now) + 'calltimer stop');
  except
    datamodule1.debugOut(3,'', 'exception in TimerCallCount');
    raise;
  end;
end;
*)

procedure TFOnTop.TimerScrollDisableTimer(Sender: TObject);
begin
  try
    TimerScrollDisable.Enabled := False;
    edit1.Enabled := False;
    datamodule1.debugOut(5, 'End of TimerScrollDisableTimer');
  except
    datamodule1.debugOut(3, '', 'exception in timerScrollDisable');
    raise;
  end;
end;




procedure TFOnTop.TimerNoDblClickTimer(Sender: TObject);
begin
  TimerNoDblClick.Enabled := False;
  datamodule1.debugOut(5, 'in TimerNoDblClick');
  ///Beep;
  if specialButtonList.IndexOf(lastevent) > -1 then
    BtnArray[specialButtonList.IndexOf(lastevent)].down := True;
end;

(************************************************************
Treeview..........................
*********************************************)

function getchildstr(lastchildstr: string; parentchildstr: string;
  var newchildstr: string): boolean;
var
  aktnodestr: string;
begin
  try
    newchildstr := aktlist.Values[parentchildstr];

    if newchildstr = '' then
    begin
      getchildstr := False;
    end
    else
    begin
      // gefundenen Knoten löschen
      aktnodestr := parentchildstr + '=' + newchildstr;
      aktlist.Delete(aktlist.IndexOf(aktnodestr));
      getchildstr := True;
    end;
  except
    datamodule1.debugOut(3, '', 'exception in getchildstr');
    raise;
  end;
end;

procedure addchilds(var baum: TTreeNodes; parentnode: TTreenode);
var
  newchild: TTreenode;
  lastchildstr, parentchildstr, newchildstr: string;
begin
  try
    lastchildstr := '';
    if parentnode = nil then
      parentchildstr := 'root'
    else
      parentchildstr := parentnode.Text;
    while getchildstr(lastchildstr, parentchildstr, newchildstr) do
    begin
      newchild := baum.Addchild(parentnode, newchildstr);
      nodelist.Add(newchildstr + '=' + IntToStr(newchild.absoluteIndex));
      if parentnode <> nil then
        parentnode.Expand(False);
      addchilds(baum, newchild);
      lastchildstr := newchildstr;
    end;
  except
    datamodule1.debugOut(3, '', 'exception in addchilds');
    raise;
  end;
end;



procedure TFOnTop.buildtree(mytable: TDBDataset);
var
  //Node1, Node2: TTreeNode;
  myItems: TTreenodes;
  //node2str,
  mystr: string;
  colnames: TStringList;
begin
  try
    datamodule1.debugOut(5, 'buildtree started');
    aktlist := TStringList.Create;
    commentlist := TStringList.Create;
    nodelist := TStringList.Create;
    colnames := TStringList.Create;
    //myItems := TTreenodes.Create(nil);
    if not mytable.Active then
      mytable.Open;
    if not mytable.IsEmpty then
    begin
      mytable.First;
      mytable.GetFieldNames(colnames);
      if colnames.IndexOf('PARENTEVENT') = -1 then
      begin  // meine events als liste
        while not mytable.EOF do
        begin
          mystr := 'root=' + mytable.FieldByName('event').AsString;
          aktlist.Add(mystr);
          mytable.Next;
        end;
        //aktlist.Delete(aktlist.IndexOf('root=root'));
      end
      else
      begin  // alle events als baum
        while not mytable.EOF do
        begin
          mystr := mytable.FieldByName('parentevent').AsString +
            '=' + mytable.FieldByName('event').AsString;
          aktlist.Add(mystr);
          mystr := mytable.FieldByName('event').AsString + '=' +
            mytable.FieldByName('comment').AsString;
          commentlist.Add(mystr);
          mytable.Next;
        end;
        aktlist.Delete(aktlist.IndexOf('root=root'));
      end;

    end;
    FTreeview.TreeView1.Items.Clear; { vorhandene Knoten entfernen }
    //myItems.Assign(Treeview1.Items);
    myItems := FTreeview.Treeview1.Items;
    addChilds(myItems, nil);
    //Treeview1.Items.Assign(myItems);
    //Treeview1.Items := myItems

  except
    datamodule1.debugOut(3, '', 'exception in buildtree');
  end;
end;


procedure TFOnTop.SpeedButton1Click(Sender: TObject);
begin
  try
    //application.ProcessMessages;
    BtnProjektDblClick(Sender);
    // ProjektClick vor falscher ignoranz schützen
    wasInDblclick := False;
    //FTreeview.remotechange := True;
    FTreeview.TreeView1.Selected := nil;
    if FTreeview.Visible then
      FTreeview.Hide
    else
      FTreeview.Show;
    if (oldselstr <> '') and (FTreeview.Treeview1.items.Count > 0) then
    begin
      try
        oldselnode := FTreeview.Treeview1.items[StrToInt(nodelist.Values[oldselstr])];
        if oldselnode <> nil then
        begin
          FTreeview.remotechange := True;
          application.ProcessMessages;
          FTreeview.Treeview1.Selected := oldselnode;
          datamodule1.debugOut(5, 'ontop.Speedbutton1Click change selection');
          ///FTreeview.Treeview1.Select(oldselnode, [ssLeft]);
          application.ProcessMessages;
          FTreeview.remotechange := False;
        end;
        FTreeview.Treeview1.Repaint;
      except
        // die alte selection wurde nicht gefunden z.B. bei wechsel von aktuelle proj
        // nach meine proj.
      end;
    end;
    FTreeview.remotechange := False;

  except
    datamodule1.debugOut(3, '',
      'exception in ontop.Speedbutton1Click (pseudo-combobox)');
    ///FTreeview.remotechange := False;
  end;
end;


procedure TFOnTop.Edit1KeyPress(Sender: TObject; var Key: char);
begin
  editready := False;
  case key of
    #8:
    begin
      newtyped := copy(newtyped, 1, length(newtyped) - 1);
    end;
    #13: editready := True;
    else
      newtyped := newtyped + key;
  end;
end;

procedure TFOnTop.Edit1Enter(Sender: TObject);
begin
  newtyped := '';
  editready := False;
end;

procedure TFOnTop.Edit1KeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
var
  i: integer;
  findstr: string;
begin
  try
    case key of
      36: newtyped := '';
      46: newtyped := edit1.Text;
    end;
    if not editready then
    begin
      if not nodelist.Sorted then
        nodelist.Sort;
      nodelist.CaseSensitive := False;
      nodelist.Find(newtyped, i);
      if i < nodelist.Count then
      begin
        findstr := nodelist.Names[i];
        if ansistartstext(newtyped, findstr) then
        begin
          edit1.Text := findstr;
          //edit1.SelStart := 0;
          edit1.SelStart := length(newtyped);
          edit1.SelLength := length(findstr) - length(newtyped);

        end
        else
        begin
          edit1.Text := newtyped;
          edit1.SelStart := length(newtyped);
        end;
      end;
    end
    else
    begin
      try
        oldselnode := FTreeview.Treeview1.items[StrToInt(nodelist.Values[edit1.Text])];
        if oldselnode <> nil then
        begin
          FTreeview.Treeview1.Selected := oldselnode;
          ///FTreeview.Treeview1.Select(oldselnode, [ssLeft]);
          oldselstr := edit1.Text;
          edit1.SelStart := length(edit1.Text);
          Ftreeview.Visible := False;
        end;
      except
        //ignorieren
      end;
      //FTreeview.Treeview1.Repaint;
      Ftreeview.Visible := False;
    end;

  except
    datamodule1.debugOut(3, '', 'exception in ontop.Edit1.keyup');
    raise;
  end;
end;




procedure TFOnTop.Timer_top_tenTimer(Sender: TObject);
begin
  reload_topten;
end;

procedure TFOnTop.Btn_work_descriptionClick(Sender: TObject);
begin
  try
    if Fwork_description = nil then
      Fwork_description := TFwork_description.Create(self);
    //Fwork_description.showmodal();
    Fwork_description.Show;
    //Fwork_description.Free;
  except
    datamodule1.debugOut(3, '', 'exception in ontop.Btn_work_descriptionClick');
    raise;
  end;
end;

procedure TFOnTop.DBLCB_topten_eventExit(Sender: TObject);
begin
  datamodule1.debugOut(5, 'Start eventExit topten combobox');
  //BtnProjektDblClick(sender);
  eventhandler(DBLCB_topten_event.Text);
  BtnProjektDblClick(Sender);
  intoptenbox := False;
  datamodule1.debugOut(8, 'Stop eventExit topten combobox');
end;

procedure TFOnTop.DBLCB_topten_eventEnter(Sender: TObject);
begin
  datamodule1.debugOut(5, 'Start eventEnter topten combobox');
  eventhandler(DBLCB_topten_event.Text);
  ineditmode := False;
  intoptenbox := True;
  datamodule1.debugOut(8, 'Stop eventEnter topten combobox');
end;

procedure DumpExceptionCallStack(E: Exception);
var
  I: integer;
  Frames: PPointer;
  Report: string;
begin
  //http://wiki.freepascal.org/Logging_exceptions
  Report := 'Program exception! ' + LineEnding + 'Stacktrace:' +
    LineEnding + LineEnding;
  if E <> nil then
  begin
    Report := Report + 'Exception class: ' + E.ClassName + LineEnding +
      'Message: ' + E.Message + LineEnding;
  end;
  Report := Report + BackTraceStrFunc(ExceptAddr);
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    Report := Report + LineEnding + BackTraceStrFunc(Frames[I]);

  ShowMessage(Report);
  datamodule1.debugOut(1, 'Exception', Report);
  //Halt; // End of program execution
end;

procedure TFOnTop.CustomExceptionHandler(Sender: TObject; E: Exception);
begin
  Application.ShowException(E);
  DumpExceptionCallStack(E);
  datamodule1.debugOut(1, 'Exception', 'Exception dumped - terminating');
  //Application.Terminate;
end;


end.
