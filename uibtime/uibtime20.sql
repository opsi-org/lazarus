/* Dokumentation und Skript zur Erstellung der Tabellen */
/* Stand 18.4.2001 */
/* Version uibtime 2.0 , interbase 6, neue Datenstruktur */
/* Version uibtime 3.0 , interbase 6, neue Datenstruktur dezember 2002*/

set sql dialect 1;

/*
create database 'bonifax:/opt/uibtime/uibtime3.gdb' user 'uibtime' password 'uibtime' default character set ISO8859_1 ;
connect 'bonifax:/opt/uibtime/uibtime3.gdb' user 'uibtime' password 'uibtime';
*/

/*
create database 'sepia:/opt/uibtime/uibtime3.gdb' user 'uibtime' password 'uibtime' default character set ISO8859_1 ;
connect 'sepia:/opt/uibtime/uibtime3.gdb' user 'uibtime' password 'uibtime';
*/


create database "c:\Programme\uib\uibtime\uibtime3.gdb" user "uibtime" password "uibtime" default character set ISO8859_1 ;
connect "c:\Programme\uib\uibtime\uibtime3.gdb" user "uibtime" password "uibtime";

/**********************************************************************/
/*                            Domains                                 */

create domain d_float as DECIMAL(12,4)
       ;

create domain boolean as smallint default 0 not null check(value in (0,1));

/***************** tables *************************/

/*   Hintergrundtabelle alle user */
create table uiballuser
   (userid   	varchar(20) not null,
    name     	varchar(50) not null,
    uibmember 	boolean,
    CONSTRAINT PK_alluser_userid     PRIMARY KEY (userid));

/* tabelle nur aktuelle user */
create table uibaktuser
   (userid   	varchar(20) not null,
    name     	varchar(50) not null,
    uibmember 	boolean,
    CONSTRAINT PK_aktuser_userid     PRIMARY KEY (userid),
    CONSTRAINT FK_uibaktuser_uiballuser Foreign key (userid)
                   references uiballuser(userid));


/*
create table uibbase_for_time
  (base_for_time varchar(50),
  CONSTRAINT PK_base_for_time     PRIMARY KEY (base_for_time));
*/


/*   Hintergrundtabelle alle Projekte und sub projekte */
create table uiballevent
   (event	varchar(50) not null,
    parentevent  varchar(50) not null,
    comment  varchar(500),
    id_string varchar(50),
    time_h float,
    base_for_time varchar(50),
    projectstart date,
    CONSTRAINT PK_allevent_event     PRIMARY KEY (event),
    CONSTRAINT FK_self_uiballevent_parentevent Foreign key (parentevent)
                   references uiballevent(event),
    Constraint CK_one_root_uiballevent check ((parentevent like 'root') or (parentevent in (select event from uiballevent))));

/* tabelle nur aktuelle Projekte und sub projekte */
create table uibaktevent
   (event	varchar(50) not null,
    parentevent  varchar(50) not null,
    comment  varchar(500),
    id_string varchar(50),
    time_h float,
    base_for_time varchar(50),
    projectstart date,
    CONSTRAINT PK_aktevent_event     PRIMARY KEY (event),
    CONSTRAINT FK_self_uibaktevent_parentevent Foreign key (parentevent)
                   references uibaktevent(event)
               on update cascade,
    Constraint CK_one_root_uibaktevent check ((parentevent like 'root') or (parentevent in (select event from uibaktevent))),
    CONSTRAINT FK_uibaktevent_event Foreign key (event)
                   references uiballevent(event));



/* tabelle nur individuelle Projekte und sub projekte */
create table uibuserevent
   (userid      varchar(20) not null,
    event       varchar(50) not null,
    button      boolean,
    CONSTRAINT PK_userevent     PRIMARY KEY (userid,event),
    CONSTRAINT FK_userevent_userid Foreign key (userid)
                   references uibaktuser(userid) on delete cascade,
    CONSTRAINT FK_userevent_aktevent Foreign key(event)
                   references uibaktevent(event) on delete cascade);


create table uibevent
   (pkuserid    varchar(20) not null,
    pkinittime  date not null,
    userid      varchar(20) not null,
    starttime   date not null,
    event       varchar(50) not null,
    stoptime    date not null,
    stunden     d_float,
    CONSTRAINT PK_uibevent     PRIMARY KEY (pkuserid,pkinittime),
    CONSTRAINT FK_uibevent_userid Foreign key (userid)
                   references uiballuser(userid),
    CONSTRAINT FK_uibevent_event Foreign key (event)
                   references uiballevent(event));

create unique index UK_uibevent on uibevent (userid,starttime); 

create table uibaccount
   (pkuserid    varchar(20) not null,
    pkinittime  date not null,
    userid      varchar(20) not null,
    starttime   date not null,
    event       varchar(50) not null,
    stoptime    date not null,
    stunden     d_float,
    CONSTRAINT PK_uibaccount     PRIMARY KEY (pkuserid,pkinittime),
    CONSTRAINT FK_uibaccount_userid Foreign key (userid)
                   references uiballuser(userid),
    CONSTRAINT FK_uibaccount_event Foreign key (event)
                   references uiballevent(event));

create unique index UK_uibaccount on uibaccount (userid,starttime); 


create table uibmonthevent
   (userid      varchar(20) not null,
    jahr        integer not null,
    monat       integer not null,
    event       varchar(50) not null,
    stunden     d_float,
    CONSTRAINT PK_uibmonthevent     PRIMARY KEY (userid,jahr,monat,event),
    CONSTRAINT FK_uibmonthevent_userid Foreign key (userid)
                   references uiballuser(userid),
    CONSTRAINT FK_uibmonthevent_event Foreign key (event)
                   references uiballevent(event));


create table uibtimeout
   (userid	varchar(20) not null,
    event  	varchar(50) not null,
    timeout_min  integer,
    CONSTRAINT PK_uibtimeout     PRIMARY KEY (userid,event),
    CONSTRAINT FK_uibtimeout_userid Foreign key (userid)
                   references uibaktuser(userid) on delete cascade,
    CONSTRAINT FK_uibtimeout_activity Foreign key (event)
                   references uibaktevent(event) on delete cascade);

create table uibcalls
   (userid    varchar(20) not null,
    day_h     date not null,
    calls     integer not null,
    CONSTRAINT PK_uibcalls     PRIMARY KEY (userid,day_h),
    CONSTRAINT FK_uibcalls_userid Foreign key (userid)
                   references uiballuser(userid));

create table uibsoll
   (userid    varchar(20) not null,
    jahr      integer not null,
    monat     integer not null,
    stunden   d_float not null,
    CONSTRAINT PK_uibsoll     PRIMARY KEY (userid,jahr,monat),
    CONSTRAINT FK_uibsoll_userid Foreign key (userid)
                   references uibaktuser(userid) on delete cascade);

create table uibdefproj
   (userid   varchar(20) not null,
    event    varchar(50) not null,
    CONSTRAINT PK_defproj_userid     PRIMARY KEY (userid),
    CONSTRAINT FK_uibdefproj_userid Foreign key (userid)
                   references uibaktuser(userid) on delete cascade,
    CONSTRAINT FK_defproj_event Foreign key (event)
                   references uibaktevent(event) on delete cascade);

create table uibloggedin
   (userid   varchar(20) not null,
    loggedin date not null,
    CONSTRAINT PK_loggedin_userid     PRIMARY KEY (userid),
    CONSTRAINT FK_loggedin_userid Foreign key (userid)
                   references uibaktuser(userid) on delete cascade);

create table uib_work_description
   (userid       varchar(20) not null,
    jahr         integer not null,
    monat        integer not null,
    tag          integer not null,
    event        varchar(50) not null,
    description  varchar (255),
    CONSTRAINT PK_wdescription_uid_ymd_event     PRIMARY KEY (userid, jahr, monat, tag, event),
    CONSTRAINT FK_wdescription_userid Foreign key (userid)
                   references uiballuser(userid),
    CONSTRAINT FK_wdescription_event Foreign key (event)
                   references uiballevent(event));



/* views für auswertung */

create view uibeventtimesplit
   (userid,
    starttime,
    stoptime,
    minute_,
    stunde,
    tag,
    monat,
    jahr,
    wochentag,
    jahrestag,
    event,
    stunden)
as select
    userid,
    starttime,
    stoptime,
    extract(minute from UIBEVENT.STARTTIME) as minute_,
    extract(hour from UIBEVENT.STARTTIME) as stunde,
    extract(day from UIBEVENT.STARTTIME) as tag,
    extract(month from UIBEVENT.STARTTIME) as monat,
    extract(year from UIBEVENT.STARTTIME) as jahr,
    extract(weekday from UIBEVENT.STARTTIME) as wochentag,
    extract(yearday from UIBEVENT.STARTTIME) as jahrestag,
    event,
    stunden
from
UIBEVENT;

create view uibcallstimesplit
   (userid,
    day_h,
    calls,
    minute_,
    stunde,
    tag,
    monat,
    jahr,
    wochentag,
    jahrestag)
as select
    userid,
    day_h,
    calls,
    extract(minute from UIBCALLS.DAY_H) as minute_,
    extract(hour from UIBCALLS.DAY_H) as stunde,
    extract(day from UIBCALLS.DAY_H) as tag,
    extract(month from UIBCALLS.DAY_H) as monat,
    extract(year from UIBCALLS.DAY_H) as jahr,
    extract(weekday from UIBCALLS.DAY_H) as wochentag,
    extract(yearday from UIBCALLS.DAY_H) as jahrestag
from
    uibcalls;



create view uibeventmonthtimesplit
   (userid,
    jahr,
    monat,
    event,
    stunden)
as select
    userid,
    jahr,
    monat,
    event,
    sum(stunden) as stunden
   from
    UIBEVENTTIMESPLIT
   group by userid, jahr, monat, event;

   





/* trigger für stunden */

set term !! ;
CREATE TRIGGER  TR_uibevent_up  for uibevent
         before update as
         begin
          new.stunden = (new.stoptime - new.starttime) * 24;
         end !!
set term ; !!

set term !! ;
CREATE TRIGGER  TR_uibevent_in for uibevent
         before insert as
         begin
          new.stunden = (new.stoptime - new.starttime) * 24;
         end !!
set term ; !!

set term !! ;
CREATE TRIGGER  TR_uibaccount_up  for uibaccount
         before update as
         begin
          new.stunden = (new.stoptime - new.starttime) * 24;
         end !!
set term ; !!

set term !! ;
CREATE TRIGGER  TR_uibaccount_in for uibaccount
         before insert as
         begin
          new.stunden = (new.stoptime - new.starttime) * 24;
         end !!
set term ; !!

/* trigger für uibevent !! nur initialer import von uibtime9 nach 10 !! */
/* nicht für den Betrieb */
/*
set term !! ;
CREATE TRIGGER  TR_uibevent_in for uibevent
         before insert as
         begin
          new.stunden = (new.stoptime - new.starttime) * 24;
          new.pkuserid = new.userid;
          new.pkinittime = new.starttime;
         end !!
set term ; !!
*/

/* Trigger zum füttern von uiballevent */

/* update  */
set term !! ;
CREATE TRIGGER  TR_uibaktevent2uiball_up  for uibaktevent
         before update as
         begin
          update uiballevent
            set parentevent = new.parentevent,
                 comment    = new.comment,
                 id_string = new.id_string,
                 base_for_time = new.base_for_time,
                 time_h = new.time_h,
		 projectstart = new.projectstart
           where (event = old.event);
         end !!
set term ; !!

/* insert */
set term !! ;
CREATE TRIGGER  TR_uibaktevent2uiball_in  for uibaktevent
         before insert as
          DECLARE VARIABLE isInAllevent smallint;
         begin
          select count(*) from uiballevent
           where event = new.event
           into :isInAllevent;
          if (:isInAllevent > 0) then
          begin
           update uiballevent
            set parentevent = new.parentevent,
                 comment    = new.comment,
                 id_string = new.id_string,
                 base_for_time = new.base_for_time,
                 time_h = new.time_h,
		 projectstart = new.projectstart
            where (event = new.event);
          end
          else
          begin
           insert into uiballevent
             values
               (new.event,
		new.parentevent,
		new.comment,
                new.id_string,
                new.base_for_time,
                new.time_h,
		new.projectstart);
          end
         end !!
set term ; !!


/* Trigger zum füttern von uiballuser */

set term !! ;
CREATE TRIGGER  TR_uibaktuser2uiball_in  for uibaktuser
         before insert as
          DECLARE VARIABLE isInAlluser smallint;
         begin
          select count(*) from uiballuser
           where userid = new.userid
           into :isInAlluser;
          if (:isInAlluser > 0) then
          begin
            update uiballuser
            set name = new.name,
                uibmember = new.uibmember
              where (userid = new.userid);
          end
          else
          begin
           insert into uiballuser
                 values
                      ( new.userid,
			new.name,
			new.uibmember);
          end
         end !!
set term ; !!

set term !! ;
CREATE TRIGGER  TR_uibaktuser2uiball_up  for uibaktuser
         before update as
         begin
          update uiballuser
            set name = new.name,
                uibmember = new.uibmember
              where (userid = old.userid);
         end !!
set term ; !!





/* Baumauswertung....*/

/* Tabelle für temporäre Auswertungsdaten....*/
create table uibtmptreesum
 (userid      varchar(20) not null,
  event varchar(50) not null,
  id_string varchar(50),
  stunden     d_float,
  CONSTRAINT PK_uibtmptreesum     PRIMARY KEY (userid,event));

create table uibtmptreesum2
 (userid      varchar(20) not null,
  event varchar(50) not null,
  id_string varchar(50),
  uibuser     varchar(20) not null,
  stunden     d_float,
  CONSTRAINT PK_uibtmptreesum2     PRIMARY KEY (userid,event,uibuser));

create table uibtmp_sapevent
 (event varchar(50) not null,
  sapevent varchar(50) not null,
  id_string varchar(50) not null,
  CONSTRAINT PK_uibtmp_sapevent     PRIMARY KEY (event,sapevent, id_string));



/* stored proc für baumauswertung */
/* sammelt summen von unterevents unterhalb von below_event
   ergänszt um die summe in below_event selber
   unter der Bedingung von - bis
   und userid = searchid
   wobei als seachid '%' angegeben werden kann, dann sind alle gemeint.
   aktuser dient nur zu identifizierung des datensatzes in der temporärentabelle
*/

/* Version für uiballevent */
SET TERM !! ;
CREATE PROCEDURE uibbuild_tmp_treesum (below_event varchar(50)
                                       , aktuser varchar(20)
                                       , searchid varchar(20)
                                       , von date, bis date)
AS
DECLARE VARIABLE aktevent varchar(50);
DECLARE VARIABLE aktid varchar(50);
BEGIN
 delete from uibtmptreesum where userid= :aktuser;
 select id_string from uiballevent where event = :below_event into :aktid;
 insert into uibtmptreesum Values (:aktuser, :below_event, :aktid,
 (select sum(stunden) from uibevent where
  (event = :below_event)
  and (starttime >= :von)
  and (stoptime < :bis)
  and (userid like :searchid)));
 FOR select event, id_string from uiballevent
     where parentevent = :below_event
     into :aktevent, :aktid
 DO
 BEGIN
  if (:aktevent <> 'root') then
  begin
   insert into uibtmptreesum Values (:aktuser, :aktevent, :aktid,
    (select sum(stunden) from uibevent where
    (
    (event in (select event from uiballevent where
    (parentevent in (select event from uiballevent where
    (parentevent in (select event from uiballevent where
    (parentevent in (select event from uiballevent where
    (parentevent in (select event from uiballevent where
    (parentevent in (select event from uiballevent where
    (parentevent = :aktevent)))))))))))))
    or
    (event in (select event from uiballevent where
    (parentevent in (select event from uiballevent where
    (parentevent in (select event from uiballevent where
    (parentevent in (select event from uiballevent where
    (parentevent in (select event from uiballevent where
    (parentevent = :aktevent)))))))))))
    or
    (event in (select event from uiballevent where
    (parentevent in (select event from uiballevent where
    (parentevent in (select event from uiballevent where
    (parentevent in (select event from uiballevent where
    (parentevent = :aktevent)))))))))
    or
    (event in (select event from uiballevent where
    (parentevent in (select event from uiballevent where
    (parentevent in (select event from uiballevent where
    (parentevent = :aktevent)))))))
    or
    (event in (select event from uiballevent where
    (parentevent in (select event from uiballevent where
    (parentevent = :aktevent)))))
    or
    (event in (select event from uiballevent
    where (parentevent like :aktevent)))
    or
    (event = :aktevent)
    )
    and (starttime >= :von)
    and (stoptime < :bis)
    and (userid like :searchid)));
  end
 END
END !!
set term ; !!

/* Version für uibaktevent  */
SET TERM !! ;
CREATE PROCEDURE uibbuild_tmp_treesum_aktevent (below_event varchar(50)
                                       , aktuser varchar(20)
                                       , searchid varchar(20)
                                       , von date, bis date)

AS
DECLARE VARIABLE aktevent varchar(50);
DECLARE VARIABLE aktid varchar(50);
BEGIN
 delete from uibtmptreesum where userid= :aktuser;
 select id_string from uibaktevent where event = :below_event into :aktid;
 insert into uibtmptreesum Values (:aktuser, :below_event, :aktid,
 (select sum(stunden) from uibevent where
  (event = :below_event)
  and (starttime >= :von)
  and (stoptime < :bis)
  and (userid like :searchid)));
 FOR select event, id_string from uibaktevent
     where parentevent = :below_event
     into :aktevent, :aktid
 DO
 BEGIN
  if (:aktevent <> 'root') then
  begin
   insert into uibtmptreesum Values (:aktuser, :aktevent, :aktid,
    (select sum(stunden) from uibevent where
    (
    (event in (select event from uibaktevent where
    (parentevent in (select event from uibaktevent where
    (parentevent in (select event from uibaktevent where
    (parentevent in (select event from uibaktevent where
    (parentevent in (select event from uibaktevent where
    (parentevent in (select event from uibaktevent where
    (parentevent = :aktevent)))))))))))))
    or
    (event in (select event from uibaktevent where
    (parentevent in (select event from uibaktevent where
    (parentevent in (select event from uibaktevent where
    (parentevent in (select event from uibaktevent where
    (parentevent in (select event from uibaktevent where
    (parentevent = :aktevent)))))))))))
    or
    (event in (select event from uibaktevent where
    (parentevent in (select event from uibaktevent where
    (parentevent in (select event from uibaktevent where
    (parentevent in (select event from uibaktevent where
    (parentevent = :aktevent)))))))))
    or
    (event in (select event from uibaktevent where
    (parentevent in (select event from uibaktevent where
    (parentevent in (select event from uibaktevent where
    (parentevent = :aktevent)))))))
    or
    (event in (select event from uibaktevent where
    (parentevent in (select event from uibaktevent where
    (parentevent = :aktevent)))))
    or
    (event in (select event from uibaktevent
    where (parentevent like :aktevent)))
    or
    (event = :aktevent)
    )
    and (starttime >= :von)
    and (stoptime < :bis)
    and (userid like :searchid)));
  end
 END
END !!
set term ; !!

/* Version für uiballevent */
SET TERM !! ;
CREATE PROCEDURE buildbsz_tmp_monthrep (aktuser varchar(20)
                                       , searchid varchar(20)
                                       , von date, bis date)
AS
DECLARE VARIABLE aktevent varchar(50);
DECLARE VARIABLE below_event varchar(50);
DECLARE VARIABLE onelevelupevent varchar(50);
DECLARE VARIABLE aktid varchar(50);
BEGIN
 delete from uibtmptreesum where userid= :aktuser;

 FOR select event, id_string from uiballevent
     where not ((id_string is null) or
     (id_string = ''))
     order by event
     into :aktevent, :aktid
 DO
 BEGIN
  insert into uibtmptreesum Values (:aktuser, :aktevent, :aktid,
  (select sum(stunden) from uibevent where
  (
  (event in (select event from uiballevent where
  (parentevent in (select event from uiballevent where
  (parentevent in (select event from uiballevent where
  (parentevent in (select event from uiballevent where
  (parentevent = :aktevent)))))))))
  or
  (event in (select event from uiballevent where
  (parentevent in (select event from uiballevent where
  (parentevent in (select event from uiballevent where
  (parentevent = :aktevent)))))))
  or
  (event in (select event from uiballevent where
  (parentevent in (select event from uiballevent where
  (parentevent = :aktevent)))))
  or
  (event in (select event from uiballevent
  where (parentevent like :aktevent)))
  or
  (event = :aktevent)
  )
  and (starttime >= :von)
  and (stoptime < :bis)
  and (userid like :searchid)));
 END


END !!
set term ; !!


/* Version für uibaktevent */
SET TERM !! ;
CREATE PROCEDURE buildbsz_tmp_monthrep_aktevent (aktuser varchar(20)
                                       , searchid varchar(20)
                                       , von date, bis date)
AS
DECLARE VARIABLE aktevent varchar(50);
DECLARE VARIABLE below_event varchar(50);
DECLARE VARIABLE onelevelupevent varchar(50);
DECLARE VARIABLE aktid varchar(50);
BEGIN
 delete from uibtmptreesum where userid= :aktuser;

 FOR select event, id_string from uibaktevent
     where not ((id_string is null) or
     (id_string = ''))
     order by event
     into :aktevent, :aktid
 DO
 BEGIN
  insert into uibtmptreesum Values (:aktuser, :aktevent, :aktid,
  (select sum(stunden) from uibevent where
  (
  (event in (select event from uibaktevent where
  (parentevent in (select event from uibaktevent where
  (parentevent in (select event from uibaktevent where
  (parentevent in (select event from uibaktevent where
  (parentevent = :aktevent)))))))))
  or
  (event in (select event from uibaktevent where
  (parentevent in (select event from uibaktevent where
  (parentevent in (select event from uibaktevent where
  (parentevent = :aktevent)))))))
  or
  (event in (select event from uibaktevent where
  (parentevent in (select event from uibaktevent where
  (parentevent = :aktevent)))))
  or
  (event in (select event from uibaktevent
  where (parentevent like :aktevent)))
  or
  (event = :aktevent)
  )
  and (starttime >= :von)
  and (stoptime < :bis)
  and (userid like :searchid)));
 END


END !!
set term ; !!


/* Version für uiballevent */
SET TERM !! ;
CREATE PROCEDURE buildbsz2_tmp_monthrep (aktuser varchar(20)
                                       , searchid varchar(20)
                                       , von date, bis date)
AS
DECLARE VARIABLE aktevent varchar(50);
DECLARE VARIABLE below_event varchar(50);
DECLARE VARIABLE onelevelupevent varchar(50);
DECLARE VARIABLE aktid varchar(50);
BEGIN
 delete from uibtmptreesum2 where userid= :aktuser;

 FOR select event, id_string from uiballevent
     where not ((id_string is null) or
     (id_string = ''))
     order by event
     into :aktevent, :aktid
 DO
 BEGIN
  insert into uibtmptreesum2 select :aktuser, :aktevent, :aktid,
  userid, sum(stunden) from uibevent where
  (
  (event in (select event from uiballevent where
  (parentevent in (select event from uiballevent where
  (parentevent in (select event from uiballevent where
  (parentevent in (select event from uiballevent where
  (parentevent = :aktevent)))))))))
  or
  (event in (select event from uiballevent where
  (parentevent in (select event from uiballevent where
  (parentevent in (select event from uiballevent where
  (parentevent = :aktevent)))))))
  or
  (event in (select event from uiballevent where
  (parentevent in (select event from uiballevent where
  (parentevent = :aktevent)))))
  or
  (event in (select event from uiballevent
  where (parentevent like :aktevent)))
  or
  (event = :aktevent)
  )
  and (starttime >= :von)
  and (stoptime < :bis)
  group by userid;
 END


END !!
set term ; !!

/* Version für uibaktevent */
SET TERM !! ;
CREATE PROCEDURE buildbsz2_tmp_monthrep_aktevent (aktuser varchar(20)
                                       , searchid varchar(20)
                                       , von date, bis date)
AS
DECLARE VARIABLE aktevent varchar(50);
DECLARE VARIABLE below_event varchar(50);
DECLARE VARIABLE onelevelupevent varchar(50);
DECLARE VARIABLE aktid varchar(50);
BEGIN
 delete from uibtmptreesum2 where userid= :aktuser;

 FOR select event, id_string from uibaktevent
     where not ((id_string is null) or
     (id_string = ''))
     order by event
     into :aktevent, :aktid
 DO
 BEGIN
  insert into uibtmptreesum2 select :aktuser, :aktevent, :aktid,
  userid, sum(stunden) from uibevent where
  (
  (event in (select event from uibaktevent where
  (parentevent in (select event from uibaktevent where
  (parentevent in (select event from uibaktevent where
  (parentevent in (select event from uibaktevent where
  (parentevent = :aktevent)))))))))
  or
  (event in (select event from uibaktevent where
  (parentevent in (select event from uibaktevent where
  (parentevent in (select event from uibaktevent where
  (parentevent = :aktevent)))))))
  or
  (event in (select event from uibaktevent where
  (parentevent in (select event from uibaktevent where
  (parentevent = :aktevent)))))
  or
  (event in (select event from uibaktevent
  where (parentevent like :aktevent)))
  or
  (event = :aktevent)
  )
  and (starttime >= :von)
  and (stoptime < :bis)
  group by userid;
 END


END !!
set term ; !!

/* Version für uiballevent */
SET TERM !! ;
CREATE PROCEDURE buildbsz_tmp_sapevent
AS
DECLARE VARIABLE aktevent varchar(50);
DECLARE VARIABLE sapevent varchar(50);
DECLARE VARIABLE aktid varchar(50);
BEGIN
 delete from uibtmp_sapevent ;

 FOR select event, id_string from uiballevent
     where not ((id_string is null) or
     (id_string = ''))
     order by event
     into :sapevent, :aktid
 DO
 BEGIN
  insert into uibtmp_sapevent select event, :sapevent, :aktid
  from uiballevent where
  (
  (event in (select event from uiballevent where
  (parentevent in (select event from uiballevent where
  (parentevent in (select event from uiballevent where
  (parentevent in (select event from uiballevent where
  (parentevent = :sapevent)))))))))
  or
  (event in (select event from uiballevent where
  (parentevent in (select event from uiballevent where
  (parentevent in (select event from uiballevent where
  (parentevent = :sapevent)))))))
  or
  (event in (select event from uiballevent where
  (parentevent in (select event from uiballevent where
  (parentevent = :sapevent)))))
  or
  (event in (select event from uiballevent
  where (parentevent like :sapevent)))
  or
  (event = :sapevent)
  );
 END


END !!
set term ; !!

/* Version für uibaktevent */
SET TERM !! ;
CREATE PROCEDURE buildbsz_tmp_sapevent_aktevent
AS
DECLARE VARIABLE aktevent varchar(50);
DECLARE VARIABLE sapevent varchar(50);
DECLARE VARIABLE aktid varchar(50);
BEGIN
 delete from uibtmp_sapevent ;

 FOR select event, id_string from uibaktevent
     where not ((id_string is null) or
     (id_string = ''))
     order by event
     into :sapevent, :aktid
 DO
 BEGIN
  insert into uibtmp_sapevent select event, :sapevent, :aktid
  from uibaktevent where
  (
  (event in (select event from uibaktevent where
  (parentevent in (select event from uibaktevent where
  (parentevent in (select event from uibaktevent where
  (parentevent in (select event from uibaktevent where
  (parentevent = :sapevent)))))))))
  or
  (event in (select event from uibaktevent where
  (parentevent in (select event from uibaktevent where
  (parentevent in (select event from uibaktevent where
  (parentevent = :sapevent)))))))
  or
  (event in (select event from uibaktevent where
  (parentevent in (select event from uibaktevent where
  (parentevent = :sapevent)))))
  or
  (event in (select event from uibaktevent
  where (parentevent like :sapevent)))
  or
  (event = :sapevent)
  );
 END


END !!
set term ; !!



/* grants */

grant all on uiballuser       	to uibtime;
grant all on uibaktuser       	to uibtime;
grant all on uiballevent   	to uibtime;
grant all on uibaktevent   	to uibtime;
grant all on uibuserevent  	to uibtime;
grant all on uibevent      	to uibtime;
grant all on uibaccount      	to uibtime;
grant all on uibmonthevent    	to uibtime;
grant all on uibtimeout    	to uibtime;
grant all on uibcalls      	to uibtime;
grant all on uibsoll       	to uibtime;
grant all on uibdefproj    	to uibtime;
grant all on uibloggedin   	to uibtime;
grant all on uibtmptreesum       	to uibtime;
grant all on uibtmptreesum2       	to uibtime;
grant all on uib_work_description       	to uibtime;
grant execute on procedure uibbuild_tmp_treesum	to uibtime;
grant execute on procedure buildbsz_tmp_monthrep to uibtime;
grant execute on procedure buildbsz2_tmp_monthrep to uibtime;
grant execute on procedure buildbsz_tmp_sapevent to uibtime;
grant execute on procedure uibbuild_tmp_treesum_aktevent	to uibtime;
grant execute on procedure buildbsz_tmp_monthrep_aktevent to uibtime;
grant execute on procedure buildbsz2_tmp_monthrep_aktevent to uibtime;
grant execute on procedure buildbsz_tmp_sapevent_aktevent to uibtime;

grant all on uiballuser       	to repl;
grant all on uibaktuser       	to repl;
grant all on uiballevent   	to repl;
grant all on uibaktevent   	to repl;
grant all on uibuserevent  	to repl;
grant all on uibevent      	to repl;
grant all on uibmonthevent    	to repl;
grant all on uibtimeout    	to repl;
grant all on uibcalls      	to repl;
grant all on uibsoll       	to repl;
grant all on uibdefproj    	to repl;
grant all on uibloggedin   	to repl;
grant all on uibtmptreesum     	to repl;
grant all on uibtmptreesum2     	to repl;
grant all on uib_work_description     	to repl;


GRANT EXECUTE
 ON PROCEDURE BUILDBSZ2_TMP_MONTHREP TO UIBTIMEREAD;

GRANT EXECUTE
 ON PROCEDURE BUILDBSZ2_TMP_MONTHREP_AKTEVENT TO UIBTIMEREAD;


GRANT EXECUTE
 ON PROCEDURE BUILDBSZ_TMP_MONTHREP TO UIBTIMEREAD;

GRANT EXECUTE
 ON PROCEDURE BUILDBSZ_TMP_MONTHREP_AKTEVENT TO UIBTIMEREAD;


GRANT EXECUTE
 ON PROCEDURE BUILDBSZ_TMP_SAPEVENT TO UIBTIMEREAD;

GRANT EXECUTE
 ON PROCEDURE BUILDBSZ_TMP_SAPEVENT_AKTEVENT TO UIBTIMEREAD;

GRANT EXECUTE
 ON PROCEDURE UIBBUILD_TMP_TREESUM TO UIBTIMEREAD;

GRANT EXECUTE
 ON PROCEDURE UIBBUILD_TMP_TREESUM_AKTEVENT TO UIBTIMEREAD;



GRANT  SELECT
 ON UIBAKTEVENT TO UIBTIMEREAD;


GRANT  SELECT
 ON UIBAKTUSER TO UIBTIMEREAD;


GRANT  SELECT
 ON UIBALLEVENT TO UIBTIMEREAD;


GRANT  SELECT
 ON UIBALLUSER TO UIBTIMEREAD;


GRANT  SELECT
 ON UIBCALLS TO UIBTIMEREAD;


GRANT  SELECT
 ON UIBDEFPROJ TO UIBTIMEREAD;


GRANT  SELECT
 ON UIBEVENT TO UIBTIMEREAD;

GRANT  SELECT
 ON UIBACCOUNT TO UIBTIMEREAD;

GRANT  SELECT
 ON UIBLOGGEDIN TO UIBTIMEREAD;


GRANT  SELECT
 ON UIBMONTHEVENT TO UIBTIMEREAD;


GRANT  SELECT
 ON UIBSOLL TO UIBTIMEREAD;


GRANT  SELECT
 ON UIBTIMEOUT TO UIBTIMEREAD;


GRANT  SELECT
 ON UIBTMPTREESUM TO UIBTIMEREAD;


GRANT  SELECT
 ON UIBTMPTREESUM2 TO UIBTIMEREAD;

GRANT  SELECT
 ON UIBTMP_SAPEVENT TO UIBTIMEREAD;


GRANT  SELECT
 ON UIBUSEREVENT TO UIBTIMEREAD;


GRANT  SELECT
 ON UIB_WORK_DESCRIPTION TO UIBTIMEREAD;

GRANT  SELECT
 ON UIBCALLSTIMESPLIT TO UIBTIMEREAD;

GRANT  SELECT
 ON UIBEVENTMONTHTIMESPLIT TO UIBTIMEREAD;

GRANT  SELECT
 ON UIBEVENTTIMESPLIT TO UIBTIMEREAD;






commit;

/* initial population */
insert into uibaktevent (event,parentevent) values ('root','root');
insert into uibaktevent (event,parentevent) values ('test','root');
commit;
