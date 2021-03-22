(*
uibtime (4.1.2.0-1) stable; urgency=low

 * uibdate: gotoLastTodayEvent;
     //try to go to to the last event
     //that is not starting tomorrow or later
 * monthrep.rlf width of data  field adjusted  ; fixes #4639
 * monthrep changed for new sollstunden brutto / netto

-- detlef oertel <d.oertel@uib.de>  Mon, 15 Mar 2021 15:00:00 +0000

uibtime (4.1.1.72-1) stable; urgency=low

 * nachf: timeout: boolean : make sure to startr logoff after nachf is timeout
 * ontop: btnUnlock
 * uibdata: make LockInput a config in the context menue

-- detlef oertel <d.oertel@uib.de>  Fri, 29 May 2020 10:00:00 +0000

uibtime (4.1.1.71-1) stable; urgency=low

 * uibdata: better logging in SQuibeventAfterEdit
 * ontop: eventhandler always post stoptime

-- detlef oertel <d.oertel@uib.de>  Tue, 21 Apr 2020 10:00:00 +0000

uibtime (4.1.1.70-1) stable; urgency=low

 * uibdata: fix TimerOnTopTimer: for windows

-- detlef oertel <d.oertel@uib.de>  Mon, 20 Apr 2020 10:00:00 +0000

uibtime (4.1.1.69-1) stable; urgency=low

 * uibdata: fix brokenSQuibeventAfterPost: killed by 4.1.1.68

-- detlef oertel <d.oertel@uib.de>  Sat, 18 Apr 2020 18:00:00 +0000

uibtime (4.1.1.68-1) stable; urgency=low

 * uibdata: SQuibeventAfterEdit: plausibility checks
    * also called by SQuibeventBeforePost

-- detlef oertel <d.oertel@uib.de>  Thu, 16 Apr 2020 18:00:00 +0000

uibtime (4.1.1.67-1) stable; urgency=low

 * possible confs:
    forceontopleft=1000 (default=-1)
    ontopintaskbar=1    (default=0)
    ontoptimer=0        (default=1)

-- detlef oertel <d.oertel@uib.de>  Wed, 15 Apr 2020 18:00:00 +0000

uibtime (4.1.1.66-1) stable; urgency=low

 * possibility to disable wmctrl at linux
 * configure DBLookupCombobox also for dataedit and multiday
 * possible confs:
    linuxwmctrl=0       (default=1)

-- detlef oertel <d.oertel@uib.de>  Tue, 14 Apr 2020 18:00:00 +0000

uibtime (4.1.1.65-1) stable; urgency=low

 * uibdata: mycombo.ReadOnly := True; (avoid editing in topten combo)
 * ontop: eventhandler: check if newevent is valid before try to handle it
 * ontop: start 5 sec timer on enter topten-combobox
 * ontop: disable timer on any visable exit

-- detlef oertel <d.oertel@uib.de>  Fri, 27 Mar 2020 18:00:00 +0000

uibtime (4.1.1.64-1) stable; urgency=low

 * uibdate: mycombo.ReadOnly := True; (avoid editing in topten combo)

-- detlef oertel <d.oertel@uib.de>  Fri, 27 Mar 2020 15:00:00 +0000

uibtime (4.1.1.63-1) stable; urgency=low

 * uibdata: glob var for server / use in checknet timer
 * checknet timer disabled

-- detlef oertel <d.oertel@uib.de>  Wed, 25 Mar 2020 15:00:00 +0000

uibtime (4.1.1.62-1) stable; urgency=low

 * uibdata: timerontop: linux: use wmctrl do push to front
   fixed call:
 * renamed runprocess to linhandlewin
 * uibdata: new configureLookupComboBox to configure for Win / Lin
 * dataedit: configureLookupComboBox(DBLookupComboBox1);
 * ontop: configureLookupComboBox(DBLCB_topten_event);

-- detlef oertel <d.oertel@uib.de>  Wed, 10 Mar 2020 15:00:00 +0000

uibtime (4.1.1.61-1) stable; urgency=low

 * unique instance: kann nur einmal gestartet werden
 * uibdata: central my Font for different OS
 * uibdata: new recursive function SetFontName(Control: TControl; Name: string);
 * uibdata: timerontop: linux: use wmctrl do push to front

-- detlef oertel <d.oertel@uib.de>  Wed, 08 Jan 2020 15:00:00 +0000

uibtime (4.1.1.60-1) stable; urgency=low

 * statistik: better behavior of node dropdown
 * multiday: avoid double Feiertag entries

-- detlef oertel <d.oertel@uib.de>  Wed, 08 Jan 2020 15:00:00 +0000

uibtime (4.1.1.59-1) stable; urgency=low

 * uibdata: do not raise catched exception in getLoggedInList

-- detlef oertel <d.oertel@uib.de>  Thu, 02 Jan 2020 15:00:00 +0000

uibtime (4.1.1.58-1) stable; urgency=low

 * httpservice:ServerThread.Execute: comment out finally
 * uibtime.ini: chacnged host to uibtime.uib.local

-- detlef oertel <d.oertel@uib.de>  Mon, 16 Dec 2019 15:00:00 +0000

uibtime (4.1.1.57-1) stable; urgency=low

 * uibdata: eventhandler onendsession

-- detlef oertel <d.oertel@uib.de>  Thu, 07 Nov 2019 15:00:00 +0000

uibtime (4.1.1.56-1) stable; urgency=low

 * workdescription: more space (wider) in left grid for row event
 * oslog: more debug messages in order to find Exception EInvalidOp: Invalid floating point operation

-- detlef oertel <d.oertel@uib.de>  Fri, 25 Oct 2019 15:00:00 +0000

uibtime (4.1.1.55-1) stable; urgency=low

 * workdescription: more space (wider) in left grid
 * oslog: more debug messages in order to find Exception EInvalidOp: Invalid floating point operation

-- detlef oertel <d.oertel@uib.de>  Thu, 24 Oct 2019 15:00:00 +0000

uibtime (4.1.1.54-1) stable; urgency=low

 * nachf: more debug messages in order to find Exception EInvalidOp: Invalid floating point operation

-- detlef oertel <d.oertel@uib.de>  Fri, 23 Aug 2019 15:00:00 +0000

uibtime (4.1.1.53-1) stable; urgency=low

 * result: fixed result.lfm / pakagelist
 * result.pas: Search field for locate and filter
   on selectable field
 * Treescrolldown: wider for longer event names

-- detlef oertel <d.oertel@uib.de>  Fri, 16 Aug 2019 15:00:00 +0000

uibtime (4.1.1.52-1) stable; urgency=low

 * Exceptionhandling: not always terminate
 * Exceptionhandling: debug outlist
 * Exceptionhandling: dump stack only on unhandled exceptions
 * Exceptionhandling: handling for EConvertError ; fixes: #4110 , #3833 , #3439
 * dataedit: event umbenennung Basis aktuelles Datum statt august 2017 ; fixes: #4120
 * dataedit: event umbenennung: do commitRetaining ; fixes: #4119


-- detlef oertel <d.oertel@uib.de>  Tue, 13 Aug 2019 15:00:00 +0000

uibtime (4.1.1.51-1) stable; urgency=low

 * uib2erp: createReport: fix in query for presumme_h

-- detlef oertel <d.oertel@uib.de>  Mon, 17 Jun 2019 15:00:00 +0000

uibtime (4.1.1.50-1) stable; urgency=low

 * uibdata: Dateneditieren1Click: make dataedit modal again
 * uibdata: Dateneditieren1Click: popupParent=Fdataedit
 * dataedit: popupmode= explizit

-- detlef oertel <d.oertel@uib.de>  Wed, 05 Jun 2019 15:00:00 +0000

uibtime (4.1.1.49-1) stable; urgency=low

 * uib2erp: better handling of projects (time_h_is_quota > 0)
 * uib2erp: if quota and acc_per_month = 0  always marked

-- detlef oertel <d.oertel@uib.de>  Wed, 05 Jun 2019 15:00:00 +0000

uibtime (4.1.1.48-1) stable; urgency=low

 * uibdata: quibsoll sql: select * from uib soll order by Jahr,monat ; fixes missing 2019 entries

-- detlef oertel <d.oertel@uib.de>  Wed, 05 Jun 2019 15:00:00 +0000

uibtime (4.1.1.47-1) stable; urgency=low

 * ontop: TimerProjektzeitTimer: more debugging output to find 'invalid floatingpoint operation' (again)
 * more debugging around using uibtime.conf

-- detlef oertel <d.oertel@uib.de>  Wed, 22 May 2019 15:00:00 +0000

uibtime (4.1.1.46-1) stable; urgency=low

 * ontop: TimerProjektzeitTimer: more debugging output to find 'invalid floatingpoint operation'

-- detlef oertel <d.oertel@uib.de>  Fri, 26 Apr 2019 15:00:00 +0000

uibtime (4.1.1.45-1) stable; urgency=low

 * uibdata: DumpExceptionCallStack: MessageDlg changed
 * ontop: edit1 + editprjectzeit autosize=false fix height
 * multiday.create: ComboBoxEvent: Autodropdown different for Windows / Linux
 * uib2erp: more logging in createreport

-- detlef oertel <d.oertel@uib.de>  Wed, 5 Dec 2018 15:00:00 +0000

uibtime (4.1.1.44-1) stable; urgency=low

 * uibdata: DumpExceptionCallStack: MessageDlg changed
 * multiday: Combobox1 -> ComboboxEvent
 * multiday: ComboBoxEvent: AutoCompleteText enabled
 * uibdata: avoid sigsev while using debugout

-- detlef oertel <d.oertel@uib.de>  Wed, 5 Dec 2018 15:00:00 +0000

uibtime (4.1.1.43-1) stable; urgency=low

 * ontop: bigger buttons buttons
 * experiments on calendar dialog vs. date picker

-- detlef oertel <d.oertel@uib.de>  Mon, 5 Nov 2018 15:00:00 +0000

uibtime (4.1.1.42-1) stable; urgency=low

 * logging via oslog : uibdata

-- detlef oertel <d.oertel@uib.de>  Wed, 31 Oct 2018 15:00:00 +0000

uibtime (4.1.1.41-1) stable; urgency=low

 * uibdata: new TDataModule1.IBConnection1Log
 * uibdata: TDataModule1.TimerCheckNetTimer: retrys

-- detlef oertel <d.oertel@uib.de>  Fri, 26 Oct 2018 15:00:00 +0000

uibtime (4.1.1.40-1) stable; urgency=low

 * uibdatetime: fix in  getLastIntervalStart at IncDay(enddate,-1)
 * statistik.pas fix at Edit2Exit ; bug created with 4.1.1.38

-- detlef oertel <d.oertel@uib.de>  Wed, 19 Sep 2018 15:00:00 +0000

uibtime (4.1.1.39-1) stable; urgency=low

 * uibdatetime: redesign from getLastIntervalStart
     (if (myMonthInterval > 1) and (modofintervals = 0) then dec(numberofintervals); )
     new boolean param endexcl to flag if end is incl or excl
 * multiday: more space for buttons and edits ; refernces #3608
 * change multiple window titles; fixes #3604
 * created unittest for getLastIntervalStart: uibtimeunittest.lpi

-- detlef oertel <d.oertel@uib.de>  Thu, 09 Sep 2018 15:00:00 +0000

uibtime (4.1.1.38-1) stable; urgency=low

 * Date format changes to handle differen default date format in varous units (much in statistik)
 * in uib2erp is no datetime issue fixed

-- detlef oertel <d.oertel@uib.de>  Tue, 04 Sep 2018 15:00:00 +0000

uibtime (4.1.1.37-1) stable; urgency=low

 * uibdatetime: fix in getLastIntervalStart (mymonthbetween := round(mymonthspan);)

-- detlef oertel <d.oertel@uib.de>  Mon, 03 Sep 2018 15:00:00 +0000

uibtime (4.1.1.36-1) stable; urgency=low

 * uibdatetime: fix in getLastIntervalStart

-- detlef oertel <d.oertel@uib.de>  Mon, 27 Aug 2018 15:00:00 +0000

uibtime (4.1.1.35-1) stable; urgency=low

 * uib2erp: check 4 all since project start
 * ontop / logoff: linux logoff notify

-- detlef oertel <d.oertel@uib.de>  Tue, 08 May 2018 15:00:00 +0000

uibtime (4.1.1.34-1) stable; urgency=low

 * fix in showmodal workaround for Linux: mrOK and mrCancel interchanged
 * cosmetic fix in login.lfm

-- detlef oertel <d.oertel@uib.de>  Mon, 23 Apr 2018 15:00:00 +0000


uibtime (4.1.1.33-1) stable; urgency=low

 * workaround for Linux Flogoff.showmodal crash in flogoff and ontop
 * login: new checkbox remeberme

-- detlef oertel <d.oertel@uib.de>  Tue, 17 Apr 2018 15:00:00 +0000

uibtime (4.1.1.32-1) stable; urgency=low

 * uibdata: checknettimer
 * debugout : close button
 * uibt2erp, uibdatetime: fic on getlastIntervalstart / getlastintervalend
 * ontop: projektzeittimer: keine negativen warnungen ; fixes #3463
 * dataedit: DBLookupComboBoxMouseWheel ; fixes #3462

-- detlef oertel <d.oertel@uib.de>  Mon, 16 Apr 2018 15:00:00 +0000

uibtime (4.1.1.31-1) stable; urgency=low

 * ontop: rebuilForm: button wide factor increased from 4.8. to 5.2
 * uibdata: timertryicon Linux: use of libnotify
 * ontop: fix calculating warning time in TFOnTop.TimerProjektzeitTimer
 * multiday: more space for text ; fixes #3445
 * login: more space for text
 * setup.opsiscript: Linux: create libnotify.so
 * logoff: more color
 * login: also user environment var UIBTIMEUSER
 * ontop: fix calculating warning time in TFOnTop.TimerProjektzeitTimer
 * multiday: more space for text ; fixes #3445

-- detlef oertel <d.oertel@uib.de>  Fri, 30 Mar 2018 15:00:00 +0000

uibtime (4.1.1.30-1) stable; urgency=low

 * ontop: call rebuildform from the end of formshow
 * logoff: Buttons for lastweek, lastmonth, standard this day again
 * log off richmemo

-- detlef oertel <d.oertel@uib.de>  Thu, 29 Mar 2018 15:00:00 +0000

uibtime (4.1.1.29-1) stable; urgency=low

 * uibdata: dateIsHolyday: switch filter off
 * fix: pdf export

-- detlef oertel <d.oertel@uib.de>  Wed, 26 Mar 2018 15:00:00 +0000

uibtime (4.1.1.28-1) stable; urgency=low

 * removed UI access from application Tab
   fix: "Eine Referenzauswertung wurde vom Server zurückgesendet."


-- detlef oertel <d.oertel@uib.de>  Wed, 26 Mar 2018 15:00:00 +0000


uibtime (4.1.1.27-1) stable; urgency=low

 * switch to lazarus 1.8.2
 * switch HighDPI on
 * uib2erp: new report (uib2erp_quota_workrep.lrf) for contingent hours (quota_lifetime_month)
 * new data structure uibtime22.sql (quota_lifetime_month, holydays, work days)
 * uibdata: pause TimerOnTop while popup
 * multidays: easier edit of Urlaub, Karnk, Feiertag, ...
 * logoff: check for missing entries

-- detlef oertel <d.oertel@uib.de>  Wed, 21 Mar 2018 15:00:00 +0000


uibtime (4.1.1.26-1) stable; urgency=low

 * uibdatetime: getLastIntervalStart: add one day to today
     if dayOfMonth of projectstart and today is equal to
     fix problems with approximation in monthbetween function

-- detlef oertel <d.oertel@uib.de>  Thu, 1 Mar 2018 15:00:00 +0000

uibtime (4.1.1.25-1) stable; urgency=low

 * uibdatetime: new getLastIntervalEnd
 * uib2erp: getLastIntervalInfo: use getLastIntervalEnd

-- detlef oertel <d.oertel@uib.de>  Wed, 31 Jan 2018 15:00:00 +0000


uibtime (4.1.1.24-1) stable; urgency=low

 * dataedit: ButtonReactivateClick: Ask via messagebox for override olfound warning
 * uibdatetime: getLastIntervalStart: prevent division by zero if myMonthInterval=0

-- detlef oertel <d.oertel@uib.de>  Wed, 24 Jan 2018 15:00:00 +0000


uibtime (4.1.1.23-1) stable; urgency=low

 * new unit uibdatetime for calendar calculations
 * ontop: TimerProjektzeitTimer: use getLastIntervalStart from uibdatetime

-- detlef oertel <d.oertel@uib.de>  Tue, 23 Jan 2018 15:00:00 +0000


uibtime (4.1.1.22-1) stable; urgency=low

 * uib2erp: createReport: use uibtimename (event) for multiple reports for one uibname

-- detlef oertel <d.oertel@uib.de>  Tue, 09 Jan 2018 15:00:00 +0000


uibtime (4.1.1.21-1) stable; urgency=low

 * uib2erp: createReport: create uibname for filename with reverse search for first dot in event

-- detlef oertel <d.oertel@uib.de>  Fri, 08 Dec 2017 15:00:00 +0000


uibtime (4.1.1.20-1) stable; urgency=low

 * uib2erp: fix reports without installed components

-- detlef oertel <d.oertel@uib.de>  Tue, 10 Oct 2017 15:00:00 +0000

uibtime (4.1.1.19-1) stable; urgency=low

 * add notification for Linus
 * notificationdlg larger (for Linux)

-- detlef oertel <d.oertel@uib.de>  Mon, 18 Sep 2017 15:00:00 +0000

uibtime (4.1.1.18-1) stable; urgency=low

 * add trayicon
 * context menu item 'Tray Konfiguration'
 * add unit notificationdlg

-- detlef oertel <d.oertel@uib.de>  Son, 10 Sep 2017 15:00:00 +0000


uibtime (4.1.1.17-1) stable; urgency=low

 * mov lazreport dir to  git project lazarus:
   lazarus/lazlib/lazreport/
   fix in lr_e_pdf.pas
   add path to project
 * may now compiled without any installations to lazarus


-- detlef oertel <d.oertel@uib.de>  Son, 10 Sep 2017 15:00:00 +0000


uibtime (4.1.1.16-1) stable; urgency=low

 * fix in external_libraries/powerpdf/PDFDoc/:
   constructor TPdfDestination.Create
   for i := 0 to 3 do

-- detlef oertel <d.oertel@uib.de>  Thu, 06 Sep 2017 15:00:00 +0000


uibtime (4.1.1.15-1) stable; urgency=low

 * ontop: handle projectStartTime with day <> 1
 * ontop: handle acc_per_monthnum  > 12

-- detlef oertel <d.oertel@uib.de>  Tue, 04 Sep 2017 15:00:00 +0000


uibtime (4.1.1.14-1) stable; urgency=low

 * uib2erp: more debugging
 * uib2erp: handle acc_per_monthnum  > 12

-- detlef oertel <d.oertel@uib.de>  Tue, 15 Aug 2017 15:00:00 +0000

uibtime (4.1.1.13-1) stable; urgency=low

 * uib2erp: StringGrid1DblClick: calculate ex gratia
 * result: TFResult.DBGrid1TitleClick(Column: TColumn); (sort by header click)

-- detlef oertel <d.oertel@uib.de>  Wed, 24 May 2017 15:00:00 +0000

uibtime (4.1.1.12-1) stable; urgency=low

 * uib2erp: createreport: format freehours as h:mm
 * dataedit: ButtonRenameEventClick: renamed in UIBEVENTACCOUNTREPORT
 * dataedit: new ButtonReactivateClick
 * uibdata: Handler for: SQuibeventPostError,  SQwork_descriptionAfterPost


-- detlef oertel <d.oertel@uib.de>  Wed, 26 Apr 2017 15:00:00 +0000

uibtime (4.1.1.11-1) stable; urgency=low

  * uib2erp: fix in calculate lastIntervalStart (calc end first)
  * uibdata: allevent / aktevent: order by lower(event)
  * uib2erp: // Alles was kleiner als 0.001 (1,5 Minuten) ist, wird ignoriert
            if usedtime < 0.001 then usedtime := 0;

-- detlef oertel <d.oertel@uib.de>  Mon, 06 Mar 2017 15:00:00 +0000

uibtime (4.1.1.10-1) stable; urgency=low

 * uib2erp: work on the events that really apeared inth the last 3 Month
   (not only on uibaktevent)
 * dataedit: new tab rename event

-- detlef oertel <d.oertel@uib.de>  Tue, 28 Feb 2017 15:00:00 +0000


uibtime (4.1.1.9-1) stable; urgency=low

 * statistik: not stoptime <= stop but startime <= stop

-- detlef oertel <d.oertel@uib.de>  Tue, 28 Feb 2017 15:00:00 +0000


uibtime (4.1.1.8-1) stable; urgency=low

 * uib2erp: fix in calculate lastIntervalStart

-- detlef oertel <d.oertel@uib.de>  Fri, 10 Feb 2017 15:00:00 +0000

uibtime (4.1.1.7-1) stable; urgency=low

 * uib2erp: fix in calculate lastIntervalEnd

-- detlef oertel <d.oertel@uib.de>  Wed, 08 Feb 2017 15:00:00 +0000


uibtime (4.1.1.6-1) stable; urgency=low

 * uib2erp: round problems at report
 * uib2erp: sort report pre table by date

-- detlef oertel <d.oertel@uib.de>  Mon, 16 Jan 2017 15:00:00 +0000


uibtime (4.1.1.5-1) stable; urgency=low

 * uib2erp: pdf export to N:\verwaltung\kunden (report | <uibname>)
 * uib2erp: export button lock after use
 * uib2erp: exporttable butto set readonly after use
 * uib2erp: checkbox 'markiere ganzes Interval' disbled
 * uib2erp: do not use datetime for hours that may be > 24
 * uib2erp: some hints
 * uib2erp: Stringgrid1 column header
 * code cleanup

-- detlef oertel <d.oertel@uib.de>  Wed, 11 Jan 2017 15:00:00 +0000


uibtime (4.1.1.4-1) stable; urgency=low

 * uibdata: TerminateApplication: fix for Linux
 * uib2erp: BtnLoadRequiredReportsClick more try except
 * uib2erp: BtnExportZeroAccountingClick more try except
 * uib2erp: getLastIntervalInfo: false if projectstart is in future
 * uib2erp: fix at lock report entries

-- detlef oertel <d.oertel@uib.de>  Wed, 04 Jan 2017 15:00:00 +0000

uibtime (4.1.1.3-1) stable; urgency=low

 * work on uibtime2erp module
 * uibdata: new query4result
 * result: now based on uibdata.query4result
 * result: now editable based on query4result.readonly
 * statistik: changes for use query4result in result
 * uib2erp: changes for use query4result in result
 * uib2erp: make uibaccountexport editable (using result)
 * uib2erp: monthrep now with Strings for weekdays (Mo,Di,...) using new table uibtwoche
 * uib2erp: revised StringGrid1DblClick with new table uibevrtemp
 * code cleanup

-- detlef oertel <d.oertel@uib.de>  Tue, 20 Dec 2016 15:00:00 +0000


uibtime (4.1.1.2-1) stable; urgency=low

 * work on uibtime2erp module
 * result: added csv export
 * result added pdf export
 * uibWorkRepChooser: use acc_per_monthnum and reportrequired
 * uib2erp: uibeventaccountreport with generrator pirmary key
 * uib2erp: uibeventaccountreport may be edited and aggregated to uibaccountexport
 * uib2erp: Umrechner decimal stunden / stunden:minuten
 * uib2erp: DBGrid1 (Tätigkeitsbericht: format stunden ##0.00
 * ontop: TimerNachfrageTimer: TimerOntop.Enabled := True;
 * statistik: Fresult.DataSource1.DataSet := query1;
 * result: not use statistik.query1
 * uib2erp: use result to display uibaccountexport
 * uib2erp: set cursor on stringgriddblclick

-- detlef oertel <d.oertel@uib.de>  Thu, 08 Dec 2016 15:00:00 +0000


uibtime (4.1.1.1-1) stable; urgency=low

 * work on uibtime2erp module
 * ontop: BtnProjektDblClick: SQuibaktevent.Refresh;
 * login: SQLTransaction1.Params.Add('isc_tpb_read_committed');
          SQLTransaction1.Params.Add('isc_tpb_rec_version');
 * logoff: removed: fix for modal form always on top (not needed at Lazarus 1.6.2)

-- detlef oertel <d.oertel@uib.de>  Wed, 07 Dec 2016 15:00:00 +0000

uibtime (4.1.1.0-1) stable; urgency=low

 * start with uibtime2erp module
 * lazarus 1.6.2

 -- detlef oertel <d.oertel@uib.de>  Tue, 06 Dec 2016 15:00:00 +0000

uibtime (4.1.0.2-1) stable; urgency=low

 * fixes at work report (de)
 * new function createreport(ouput)
 * new editbutton with export directory dialog

-- detlef oertel <d.oertel@uib.de>  Wed, 07 Sep 2016 15:00:00 +0000


uibtime (4.1.0.1-1) stable; urgency=low

 * fix checkdb on logoff show

 -- detlef oertel <d.oertel@uib.de>  Wed, 24 Aug 2016 15:00:00 +0000

uibtime (4.1.0.0-1) stable; urgency=low

 * work report report

 -- detlef oertel <d.oertel@uib.de>  Fri, 12 Aug 2016 15:00:00 +0000

uibtime (4.0.12.1-1) stable; urgency=low

 * extended httpservice reply
 * Thread terminate

 -- detlef oertel <d.oertel@uib.de>  Tue, 02 Aug 2016 15:00:00 +0000

uibtime (4.0.12-1) stable; urgency=low

 * new unit httpservice
 * uibdata: reading version via fileinfo unit
 * uibdata: reading verDate via fileage
 * httpservice: do not accept requests from != localhost:4440
 * httpservice: extended response text
 * linux: to all desktops (wmctrl -r <win> -t -2)
 * generic exception handler

 -- detlef oertel <d.oertel@uib.de>  Tue, 02 Aug 2016 15:00:00 +0000

uibtime (4.0.11-1) stable; urgency=low

 * uibdata: version = 4.0.11  / verDatum = '14.12.2015';
 * fix at special report für Monatsbericht: no autosize for tag

 -- detlef oertel <d.oertel@uib.de>  Mon, 14 Dec 2015 15:00:00 +0000

uibtime (4.0.10-1) stable; urgency=low

 * uibdata: version = 4.0.10  / verDatum = '12.11.2015';
 * ontop.pas: adding 6month support
 * fix at special report für Monatsbericht

 -- detlef oertel <d.oertel@uib.de>  Thu, 12 Nov 2015 15:00:00 +0000

uibtime (4.0.9-1) stable; urgency=low

 * uibdata: version = 4.0.9  / verDatum = '05.11.2015';
 * adding report and printer
 * special report für Monatsbericht
 * FrPrintGrid1 in result

 -- detlef oertel <d.oertel@uib.de>  Thu, 05 Nov 2015 15:00:00 +0000

uibtime (4.0.8-1) stable; urgency=low

 * uibdata: version = 4.0.8  / verDatum = '27.10.2015';
 * uibdata: SQuibloggedinAfterPost: logging
 * more logging changes
 * uibdata: TerminateApplication: fix path to uibtime.conf

 -- detlef oertel <d.oertel@uib.de>  Tue, 27 Oct 2015 15:00:00 +0000

uibtime (4.0.7-1) stable; urgency=low

 * uibdata: version = 4.0.7  / verDatum = '22.10.2015';
 * remove ApplyUpdates
 *result: Autosize columns = true

 -- detlef oertel <d.oertel@uib.de>  Thu, 22 Oct 2015 15:00:00 +0000

uibtime (4.0.6-1) stable; urgency=low

 * uibdata: version = 4.0.6  / verDatum = '21.10.2015';
 * change logging to opsi style
 * disable border icons in FDebug

 -- detlef oertel <d.oertel@uib.de>  Wed, 21 Oct 2015 15:00:00 +0000

uibtime (4.0.5-1) stable; urgency=low

 * uibdata: version = 4.0.5  / verDatum = '20.10.2015';
 * fix work_description: uibdata: use EditButtonDate.text

 -- detlef oertel <d.oertel@uib.de>  Tue, 20 Oct 2015 15:00:00 +0000

uibtime (4.0.4-1) stable; urgency=low

 * uibdata: version = 4.0.4  / verDatum = '16.10.2015';
 * Editbutton use fixes in multiday, work_description and logoff
 * statistik: disable non working reports
 * logoff: checkDB: reoprt-report: works now not only for one day

 -- detlef oertel <d.oertel@uib.de>  Fri, 16 Oct 2015 11:52:45 +0000

uibtime (4.0.3-1) stable; urgency=low

 * uibdata: version = 4.0.3  / verDatum = '15.10.2015';
 * uibdata: fixes for loggedin:
   * second DBConnection and transaction
 * TFOnTop.Edit1Change

 -- detlef oertel <d.oertel@uib.de>  Thu, 15 Oct 2015 11:52:45 +0000

uibtime (4.0.2-1) stable; urgency=low

 * multiday: init calendar dialogs to now
 * multiday: remove focus (default) from 'okbtn'button
 * multiday: diable OK-Btn while writing to DB
 * logoff: bigger output window
 * workdescription: init calendar dialogs to now
 * uibdata: version = 4.0.2  / verDatum = '12.10.2015';
 * uibdata: setloggedin: use locate instead of filter
 * uibdata: setloggedin: call FLoggedin.BtnAktualisierenClick
 * login: do not filter SQuibloggedin
 * login: setloggedin to true (if not admin)
 * TFTreeview.TreeView1Change: additional check if we really have a change

 -- detlef oertel <d.oertel@uib.de>  Mon, 12 Oct 2015 11:52:45 +0000

uibtime (4.0.1-1) stable; urgency=low

 * TFTreeview.FormShow: do not set remotechange to false

 -- detlef oertel <d.oertel@uib.de>  Wed, 17 Sep 2015 11:52:45 +0000

uibtime (4.0.0-1) stable; urgency=low

 * Initial entry

 -- detlef oertel <d.oertel@uib.de>  Wed, 16 Sep 2015 11:52:45 +0000

4.0.0 16.7.2015
  * port to lazarus

############
ver 3.5.7
- bugfix errchenung restliche Zeit bei 3MONTH

ver 3.5.5
- uib-workdescription in import und export aufgenommen

ver 3.0.7.4 23.5.03
- uibtime8.sql: procedure buildbsz_tmp_monthrep
- statistik butto dazu 'Bsz Controlling bericht'
- expo2 default auf .txt geändert und Datei %TEMP%\ergeb.txt

ver 3.0.7.3 8.5.03
- uibdata debuglevel (default 5) eingeführt:
 0 = quiet
 1 = error
 2 = warnig
 3 = message
 4 = debug1
 5 = debug2
- debugout aufruf überall geändert mit level 3
- in mexpo loging und fehler abfang verbessert

ver 3.0.7 5.5.03
- in import logging verbessert und batchmove tables nach
 GetEnvironmentVariable('TEMP') verlegt.
- in uibdata versions info abgefragt -> für log
 -> für version (keine konstante mehr)

 ver 3.06 21.2.03
 - in statistik für summe ab knoten
 uibtime8.sql angepasst an
 - ausgewählter knoten soll mit angezeigt werden
 - userid oder '%' für alle kann mit übergeben werden.
 - procedur in statistik an neue parameter angepasst.
 - in rgenrep
 theQR.Bands.PageHeaderband.Font.Size := C_Fontsize*2;
 ersetzt durch
 theQR.Bands.PageHeaderband.Font.Size := C_Fontsize;
 damit lange überschriften aus Statistik noch passen.

 ver 3.05 18.12.03
 - in ontop Speedbuton1Click nach aufruf von ProjektDblClick
 war_in_dblclick auf false gesetzt.
 - debugmeldungen für timeout gesetzt
 - in TFOnTop.TimerNachfrageTimer erneute Abfrage der Timeoutzeit unterbunden
 - loggeidin abfrage Intervall auf 11 Minuten gesetzt um inteferenzen mit
 Nachfrage zu minimieren
 - in TFOnTop.BtnByeClick wird result auf mrAbort überprüft um zu vermeiden,
 dass während des beendens die loggedin abfrage ausgeführt wird.

 ver 3.04 10.2.03
 - Fehler in Aufruf von statistik/freier SQL-Text beseitigt.
 - Fast alle Forms auf Window Formstyle fsNormal gesetzt um
 Probleme mit kinder fenstern zu beheben
 - für die Zeit von Statistik den onTop-Timer disabled
 - in fresult
 if fGenQuickReport <> nil
 then
 Begin
 fGenQuickReport.free;
 fGenQuickReport := nil
 End;

 ver 3.03 7.2.03
 - in statistik 'summen ab knoten' eingeführt
 - auf basis von uibtime8.sql

 ver 3.02 16.1.03
 - erzeugung von:
 //Application.CreateForm(TFResult, FResult);
 //Application.CreateForm(TFSQL, FSQL);
 //Application.CreateForm(TFExport2, FExport2);
 //Application.CreateForm(TFColEd, FColEd);
 //Application.CreateForm(TFgenQuickReport, FgenQuickReport);
 //Application.CreateForm(TFRmonat, FRmonat);
 //Application.CreateForm(TFMultiday, FMultiday);
 //Application.CreateForm(TFExport, FExport);
 beim start ausgeschaltet.
 Werden jetzt bei bedarf erzeugt.

 ver 3.01 9.1.03
 - treeview readonly
 - fehler bei projectdoppelclick beseitigt
 - erzeugung von weristda vorgeszogen
 - in treeview selectieren der defaultselectierung möglich
 - edit1 wird beim start gelöscht
 - edit1 wird bei specialbutton nichtmehr gelöscht
 - bei specialbutton wird treeview eingeklappt, so noch offen
 - rote schrift bei Nachfrage nach links geschoben
 - breite der specialbuttons reduziert
 - breite callzähler reduziert

 ver 3.0 7.1.03
 Großer umbau auf neue Datenstruktur:
 - Alleevents mit Baumstruktur
 - Esgibt wieder aktevents
 - aftrennung alleuser und aktuser
 - alle* Tabellen werden über trigger gefüttert
 - pseudo DBLookuptreeviewcombobox
 - in logoff nach datumseingabe 'Durchsuchen' Defaultbutton
 - in Statistik und Datenweitergabe 'letzter Monat' setzt jetzt auch im Januar
 das Startdatum richtig
 - Dateneditiern/ 'mehrere Tage': aktuelles Datum wird gesetzt
 - 'Weristda' wird defaultmäßig geöffnet mit Intervall 5 min, beep entfernt

 ver 2.05 25.9.01
 - Geister Anwesenheiten im wer ist da (immer erster der Liste)
 bei Programmstart beseitigt: Kein setloggedin im eventhandler wenn
 newevent = ''.

 ver 2.04 21.9.01
 - erzeugt Logfile: c:\temp\uibtime.log

 Ver 2.03 18.9.01
 - Nachfragetimer wenn kein Timeout für Default eingestellt ist, auf 10 Min
 gesetzt.
 - Kein Appdeaktivate wenn 'inLogoff'

 Ver 2.02 23.8.01
 - schmaleLeiste eingeführt
 - Breite der Knöpfe reduziert (auch abhängig von Icon)
 - DBCombobox ducrh combobox ersetzt damit man zum Projekt zurückkehren kann

 Ver 2.01 26.6.01
 - Info eingeführt
 - Zeitfenster bei wer ist da auf 10 Minuten erhöht
 - Probleme beim editieren von uiballevent auf die edit-Methode eingegrenzt
 aber ungelöst.

 ver 2.0
 Neue Datenstruktur: uibtime6.sql
 Anpassung an Datenstruktur
 - login aus umgebungsvariable uname
 - wiederholtes login nach fehleingabe möglich
 - Sollstunden duplizierer
 - Autologin
 - wer ist da ? zeigt servernamen an
 - statistik an neue datenstruktur und an view uibeventtimesplit angepasst
 - statistik wochentage, monate, jahre in Abfragen mit aufgenommen
 - rmonat: wochentage
 - Datenweitergabe: calls auf zeitraum reduziert.
 - wer ist da design des oberen panels vereinfacht
 - hilfe mit htmlLite aktiviert
 - Projektanzeige zwischen Alle / Meine Projekte Umschaltbar
 - Buttonleiste aktualisierbar

 ver 1.91
 aktionen von Flogoff.activate nach onshow verlagert,
 da sonst GPF bei Miriam und Ruppert

 ver 1.9
 - Umstieg auf Delphi5
 - Test mit IB6
 - login: Datamodule1.setloggedin(true); nicht bei admin
 - statistik: default auf 2001 gessetzt
 - Delphi5 bedingte probleme im Aufruf behoben.
 - uibsoll year in jahr und month in monat umbenannt -> uibtime5.sql

 ver 183 30.1.01
 - dm.getloggedin eingeführt
 - in OnAppDeactivate getloggedin abgefragt um logoff-Geister zu vermeiden

 ver 1.82 29.1.01
 - loggedin verwendet timestamp statt boolschen wert
 - setloggedin schaltet timerloggedin ein und aus
 - getloggedinlist akzeptiert jetzt 4 Minuten alte Timestamps

 ver 1.8 28.12.00
 - fontop.timer3 (timer für calls) wird nicht mehr in fontop.create sondern in login enabled
 - Anzeige der Anwesenden (
 Dazu Änderungen in: loggedin_, uibdata, login, uibtime.ini,uibtime4.sql)

 ver 1.72 15.12.00
 - Datenweitergabe auch für einzelne Personen oder für alle möglich

 ver 1.71 21.11.00
 - Bug bei onlyedit behoben: beim ausloggen wurde der letzte
 Datensatz auf aktuelle Zeit gesetzt
 - Zugriffsverletzung bei doppelklick auf Projekte beim user glaes
 durch auskommentieren von dbedit1.setfocus behoben.
 - In Timer1 wird das aktuelle Fenster festgestellt und nach der
 Nachfrage wieder der Focus zurückgegeben.

 ver 1.7 19.10.00
 - Möglichkeit des Editierens einer DB auf anderem Server ohne
 dabei dort in die Zeiterfassung zu kommen (onlyedit)

 ver 1.64 5.9.00
 - bei import einzeln bei exception ein raise eingefügt

 ver 1.63 24.8.00
 - 800x600 Bug beseitigt: FLogoffOnTopTimer wird nur enabled,
 wenn das Fenster auch sichtbar ist.

 ver 1.62 14.8.00
 - Meldung nach vollendeter Datenweitergabe
 - Transaktionskontrolle für Daten Import

 ver 1.61 11.8.00
 - in multiday werden event_kind ab first geladen
 - in multiday wird das aktuelle datum im kalender gesetzt

ver 1.6 28.7.00:
- SQuibevent afterinsert für userid (uid) stoptime, starttime (now)
- SQuibsoll afterinsert für userid (uid)
- SQuibtimeout afterinsert für userid (uid)
- in dataedit in dbgrid 1 und 4 cancel on exit auf false
- multiday eingeführt
- sollstunden werden bei Datenweitergabe mit exportiert.

*)

// Nachfolgend nur dummy

unit changelog;

{$MODE Delphi}

interface

uses
SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

(*
type
// TForm1 = class(TForm)
 private
  { Private-Deklarationen }
 public
  { Public-Deklarationen }
 end;

//var
// Form1: TForm1;
*)
implementation

//{$R *.DFM}

end.

