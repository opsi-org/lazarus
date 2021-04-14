/********************* ROLES **********************/

/********************* UDFS ***********************/

/****************** SEQUENCES ********************/

CREATE SEQUENCE CANIASIMPORTERROR_SEQU;
CREATE SEQUENCE REPL_GENERATOR;
CREATE SEQUENCE UIBACCOUNTEXPORT_SEQU;
CREATE SEQUENCE UIBACCOUNTREPORT_SEQU;
/******************** DOMAINS *********************/

CREATE DOMAIN BOOLEAN
 AS smallint
 DEFAULT 0
 NOT NULL
 check(value in (0,1))
;
CREATE DOMAIN D_FLOAT
 AS numeric(15,4)
;
CREATE DOMAIN MON$SEC_DATABASE
 AS char(7) CHARACTER SET ASCII
 NOT NULL
 COLLATE ASCII;
CREATE DOMAIN SEC$KEY
 AS varchar(10) CHARACTER SET UNICODE_FSS
 COLLATE UNICODE_FSS;
CREATE DOMAIN SEC$NAME_PART
 AS varchar(10) CHARACTER SET UNICODE_FSS
 COLLATE UNICODE_FSS;
CREATE DOMAIN SEC$USER_NAME
 AS varchar(10) CHARACTER SET UNICODE_FSS
 COLLATE UNICODE_FSS;
CREATE DOMAIN SEC$VALUE
 AS varchar(85) CHARACTER SET UNICODE_FSS
 COLLATE UNICODE_FSS;
/******************* PROCEDURES ******************/

SET TERM ^ ;
CREATE PROCEDURE BUILDBSZ2_TMP_MONTHREP (
    AKTUSER varchar(20),
    SEARCHID varchar(20),
    VON timestamp,
    BIS timestamp )
AS
BEGIN SUSPEND; END^
SET TERM ; ^

SET TERM ^ ;
CREATE PROCEDURE BUILDBSZ2_TMP_MONTHREP_AKTEVENT (
    AKTUSER varchar(20),
    SEARCHID varchar(20),
    VON timestamp,
    BIS timestamp )
AS
BEGIN SUSPEND; END^
SET TERM ; ^

SET TERM ^ ;
CREATE PROCEDURE BUILDBSZ_TMP_MONTHREP (
    AKTUSER varchar(20),
    SEARCHID varchar(20),
    VON timestamp,
    BIS timestamp )
AS
BEGIN SUSPEND; END^
SET TERM ; ^

SET TERM ^ ;
CREATE PROCEDURE BUILDBSZ_TMP_MONTHREP_AKTEVENT (
    AKTUSER varchar(20),
    SEARCHID varchar(20),
    VON timestamp,
    BIS timestamp )
AS
BEGIN SUSPEND; END^
SET TERM ; ^

SET TERM ^ ;
CREATE PROCEDURE BUILDBSZ_TMP_SAPEVENT
AS
BEGIN SUSPEND; END^
SET TERM ; ^

SET TERM ^ ;
CREATE PROCEDURE BUILDBSZ_TMP_SAPEVENT_AKTEVENT
AS
BEGIN SUSPEND; END^
SET TERM ; ^

SET TERM ^ ;
CREATE PROCEDURE UIBBUILD_TMP_TREESUM (
    BELOW_EVENT varchar(50),
    AKTUSER varchar(20),
    SEARCHID varchar(20),
    VON timestamp,
    BIS timestamp )
AS
BEGIN SUSPEND; END^
SET TERM ; ^

SET TERM ^ ;
CREATE PROCEDURE UIBBUILD_TMP_TREESUM_AKTEVENT (
    BELOW_EVENT varchar(50),
    AKTUSER varchar(20),
    SEARCHID varchar(20),
    VON timestamp,
    BIS timestamp )
AS
BEGIN SUSPEND; END^
SET TERM ; ^

/******************** TABLES **********************/

CREATE TABLE CANIASIMPORTERROR
(
  CANIMP_REP_ID integer NOT NULL,
  ERRMESG varchar(255),
  CREATED timestamp,
  CONSTRAINT PK_CANIMPERR PRIMARY KEY (CANIMP_REP_ID)
);
CREATE TABLE EVAL_MONTH
(
  UIB_USER varchar(24) NOT NULL,
  UIB_YEAR integer NOT NULL,
  UIB_MONTH integer NOT NULL,
  UIB_MONTH_SOLL numeric(15,5),
  UIB_MONTH_IST numeric(15,5),
  UIB_MONTH_KORR numeric(15,5),
  BUNDESLAND varchar(10) DEFAULT 'RP',
  H_PER_DAY D_FLOAT DEFAULT 0,
  MO_IS_WORK BOOLEAN DEFAULT 1 NOT NULL,
  DI_IS_WORK BOOLEAN DEFAULT 1 NOT NULL,
  MI_IS_WORK BOOLEAN DEFAULT 1 NOT NULL,
  DO_IS_WORK BOOLEAN DEFAULT 1 NOT NULL,
  FR_IS_WORK BOOLEAN DEFAULT 1 NOT NULL,
  UIB_MONTH_PRODUKTIV integer,
  UIB_MONTH_SOLL_CONTRACT integer,
  UIB_COMMENT varchar(120) CHARACTER SET UTF8,
  UIB_TIMESTAMP timestamp,
  UIB_CHANGEDBY varchar(24),
  CONSTRAINT PK_EVAL_MONTH PRIMARY KEY (UIB_USER,UIB_YEAR,UIB_MONTH)
);
CREATE TABLE EVAL_USER
(
  UIB_USER varchar(24) NOT NULL,
  FROM_DATE varchar(10) NOT NULL,
  TO_DATE varchar(10),
  CONSTRAINT PK_EVAL_USER PRIMARY KEY (UIB_USER,FROM_DATE)
);
CREATE TABLE EVAL_VACATION_IST
(
  UIB_USER varchar(24) NOT NULL,
  ACCOUNTING_YEAR integer NOT NULL,
  FROM_YEAR integer NOT NULL,
  FROM_MONTH integer NOT NULL,
  FROM_DAY integer NOT NULL,
  TO_YEAR integer,
  TO_MONTH integer,
  TO_DAY integer,
  VACATION_DAYS numeric(5,2),
  H_PER_DAY numeric(5,2),
  UIB_COMMENT varchar(120) CHARACTER SET UTF8,
  UIB_TIMESTAMP timestamp,
  UIB_CHANGEDBY varchar(24),
  CONSTRAINT PK_EVAL_VACATION_IST PRIMARY KEY (UIB_USER,ACCOUNTING_YEAR,FROM_YEAR,FROM_MONTH,FROM_DAY)
);
CREATE TABLE EVAL_VACATION_SOLL
(
  UIB_USER varchar(24) NOT NULL,
  UIB_YEAR integer NOT NULL,
  VACATION_DAYS numeric(5,2),
  UIB_COMMENT varchar(120) CHARACTER SET UTF8,
  UIB_TIMESTAMP timestamp,
  UIB_CHANGEDBY varchar(24),
  CONSTRAINT PK_VACATION_SOLL PRIMARY KEY (UIB_USER,UIB_YEAR)
);
CREATE TABLE EVAL_YEAR
(
  UIB_USER varchar(24) NOT NULL,
  UIB_YEAR integer NOT NULL,
  UIB_YEAR_SOLL numeric(15,5),
  UIB_YEAR_IST numeric(15,5),
  UIB_SUM_PRE numeric(15,5),
  UIB_SUM_AFTER numeric(15,5),
  UIB_COMMENT varchar(120) CHARACTER SET UTF8,
  UIB_TIMESTAMP timestamp,
  UIB_CHANGEDBY varchar(24),
  UIB_KORREKTUR numeric(15,5),
  CONSTRAINT PK_EVAL_YEAR PRIMARY KEY (UIB_USER,UIB_YEAR)
);
CREATE TABLE MANUAL_LOG
(
  REPLNO integer NOT NULL,
  PUBDBNO integer NOT NULL,
  SUBDBNO integer NOT NULL,
  SEQNO integer NOT NULL,
  RELATIONNO integer NOT NULL,
  REPTYPE char(1),
  OLDKEY varchar(256),
  NEWKEY varchar(256),
  SEP char(1),
  FORCECOUNT integer DEFAULT 0,
  ERROR_CODE integer,
  ERROR_MSG varchar(256),
  CONSTRAINT INTEG_53 PRIMARY KEY (REPLNO,PUBDBNO,SUBDBNO,SEQNO)
);
CREATE TABLE PUBLIC_HOLIDAYS
(
  HOLYDATE timestamp NOT NULL,
  HOLYNAME varchar(30),
  HOLYFACTOR numeric(3,2),
  BY_LAW smallint DEFAULT 1,
  BUNDESLAND varchar(10) DEFAULT 'RP' NOT NULL,
  CONSTRAINT PK_HOLYDAY PRIMARY KEY (HOLYDATE,BUNDESLAND)
);
CREATE TABLE REPL_LOG
(
  REPLNO integer NOT NULL,
  PUBDBNO integer NOT NULL,
  SUBDBNO integer NOT NULL,
  SEQNO integer NOT NULL,
  RELATIONNO integer NOT NULL,
  REPTYPE char(1),
  OLDKEY varchar(256),
  NEWKEY varchar(256),
  SEP char(1),
  FORCECOUNT integer DEFAULT 0,
  CONSTRAINT INTEG_47 PRIMARY KEY (REPLNO,PUBDBNO,SUBDBNO,SEQNO)
);
CREATE TABLE REPL_SEPARATOR
(
  REPLNO integer NOT NULL,
  PUBDBNO integer NOT NULL,
  SUBDBNO integer NOT NULL,
  RELATIONNO integer NOT NULL,
  RELATIONNAME varchar(100) NOT NULL,
  SEP char(1),
  CONSTRAINT INTEG_41 PRIMARY KEY (REPLNO,PUBDBNO,SUBDBNO,RELATIONNO)
);
CREATE TABLE UIBACCOUNT
(
  PKUSERID varchar(20) NOT NULL,
  PKINITTIME timestamp NOT NULL,
  USERID varchar(20) NOT NULL,
  STARTTIME timestamp NOT NULL,
  EVENT varchar(50) NOT NULL,
  STOPTIME timestamp NOT NULL,
  STUNDEN D_FLOAT,
  CONSTRAINT PK_UIBACCOUNT PRIMARY KEY (PKUSERID,PKINITTIME)
);
CREATE TABLE UIBACCOUNTEXPORT
(
  ACC_EXP_ID integer NOT NULL,
  EVENT varchar(50) NOT NULL,
  FROMDAY timestamp NOT NULL,
  UNTILDAY timestamp NOT NULL,
  STUNDEN D_FLOAT,
  INERP BOOLEAN NOT NULL,
  ERPERROR BOOLEAN NOT NULL,
  ERP_ERRORSTR varchar(255),
  CONSTRAINT PK_UIBACCEXP PRIMARY KEY (ACC_EXP_ID),
  CONSTRAINT UK_UIBACCEXP UNIQUE ("EVENT",FROMDAY)
);
CREATE TABLE UIBAKTEVENT
(
  EVENT varchar(50) NOT NULL,
  PARENTEVENT varchar(50) NOT NULL,
  COMMENT varchar(500),
  ID_STRING varchar(50),
  TIME_H float,
  PROJECTSTART timestamp,
  REPORTREQUIRED BOOLEAN NOT NULL,
  ACC_PER_MONTHNUM float,
  ACCOUNTINGREQUIRED BOOLEAN NOT NULL,
  KD_STRING varchar(50),
  MATTYPE varchar(50),
  TIME_H_IS_QUOTA BOOLEAN NOT NULL,
  UIB3MONTHINVOICE BOOLEAN NOT NULL,
  MATERIAL varchar(50),
  QUOTA_LIFETIME_MONTH float DEFAULT 0 NOT NULL,
  CONSTRAINT PK_AKTEVENT_EVENT PRIMARY KEY ("EVENT")
);
CREATE TABLE UIBAKTUSER
(
  USERID varchar(20) NOT NULL,
  NAME varchar(50) NOT NULL,
  UIBMEMBER BOOLEAN,
  EMAIL varchar(50),
  MYPASSWORD varchar(50),
  H_PER_DAY D_FLOAT DEFAULT 0,
  MO_IS_WORK BOOLEAN DEFAULT 0 NOT NULL,
  DI_IS_WORK BOOLEAN DEFAULT 0 NOT NULL,
  MI_IS_WORK BOOLEAN DEFAULT 0 NOT NULL,
  DO_IS_WORK BOOLEAN DEFAULT 0 NOT NULL,
  FR_IS_WORK BOOLEAN DEFAULT 0 NOT NULL,
  BUNDESLAND varchar(10) DEFAULT 'RP',
  CONSTRAINT PK_AKTUSER_USERID PRIMARY KEY (USERID)
);
CREATE TABLE UIBALLEVENT
(
  EVENT varchar(50) NOT NULL,
  PARENTEVENT varchar(50) NOT NULL,
  COMMENT varchar(500),
  ID_STRING varchar(50),
  TIME_H float,
  PROJECTSTART timestamp,
  REPORTREQUIRED BOOLEAN NOT NULL,
  ACC_PER_MONTHNUM float,
  ACCOUNTINGREQUIRED BOOLEAN NOT NULL,
  KD_STRING varchar(50),
  MATERIAL varchar(50),
  MATTYPE varchar(50),
  TIME_H_IS_QUOTA BOOLEAN NOT NULL,
  UIB3MONTHINVOICE BOOLEAN NOT NULL,
  QUOTA_LIFETIME_MONTH float DEFAULT 0 NOT NULL,
  CONSTRAINT PK_ALLEVENT_EVENT PRIMARY KEY ("EVENT")
);
CREATE TABLE UIBALLUSER
(
  USERID varchar(20) NOT NULL,
  NAME varchar(50) NOT NULL,
  UIBMEMBER BOOLEAN,
  EMAIL varchar(50),
  MYPASSWORD varchar(50),
  H_PER_DAY D_FLOAT DEFAULT 0,
  MO_IS_WORK BOOLEAN DEFAULT 0 NOT NULL,
  DI_IS_WORK BOOLEAN DEFAULT 0 NOT NULL,
  MI_IS_WORK BOOLEAN DEFAULT 0 NOT NULL,
  DO_IS_WORK BOOLEAN DEFAULT 0 NOT NULL,
  FR_IS_WORK BOOLEAN DEFAULT 0 NOT NULL,
  BUNDESLAND varchar(10) DEFAULT 'RP',
  CONSTRAINT PK_ALLUSER_USERID PRIMARY KEY (USERID)
);
CREATE TABLE UIBCALLS
(
  USERID varchar(20) NOT NULL,
  DAY_H timestamp NOT NULL,
  CALLS integer NOT NULL,
  CONSTRAINT PK_UIBCALLS PRIMARY KEY (USERID,DAY_H)
);
CREATE TABLE UIBDEFPROJ
(
  USERID varchar(20) NOT NULL,
  EVENT varchar(50) NOT NULL,
  CONSTRAINT PK_DEFPROJ_USERID PRIMARY KEY (USERID)
);
CREATE TABLE UIBEVENT
(
  PKUSERID varchar(20) NOT NULL,
  PKINITTIME timestamp NOT NULL,
  USERID varchar(20) NOT NULL,
  STARTTIME timestamp NOT NULL,
  EVENT varchar(50) NOT NULL,
  STOPTIME timestamp NOT NULL,
  STUNDEN D_FLOAT,
  CONSTRAINT PK_UIBEVENT PRIMARY KEY (PKUSERID,PKINITTIME)
);
CREATE TABLE UIBEVENTACCOUNTREPORT
(
  ACC_REP_ID integer NOT NULL,
  USERID varchar(20) NOT NULL,
  DATEDAY timestamp NOT NULL,
  EVENT varchar(50) NOT NULL,
  STUNDEN D_FLOAT,
  DESCRIPTION varchar(255),
  LOCKED BOOLEAN DEFAULT 0,
  CONSTRAINT PK_UIBEVENTAC PRIMARY KEY (ACC_REP_ID)
);
CREATE TABLE UIBEVRTEMP
(
  USERID varchar(20) NOT NULL,
  DATEDAY timestamp NOT NULL,
  EVENT varchar(50) NOT NULL,
  STUNDEN D_FLOAT,
  DESCRIPTION varchar(255)
);
CREATE TABLE UIBLOGGEDIN
(
  USERID varchar(20) NOT NULL,
  LOGGEDIN timestamp NOT NULL,
  CONSTRAINT PK_LOGGEDIN_USERID PRIMARY KEY (USERID)
);
CREATE TABLE UIBMONTHEVENT
(
  USERID varchar(20) NOT NULL,
  JAHR integer NOT NULL,
  MONAT integer NOT NULL,
  EVENT varchar(50) NOT NULL,
  STUNDEN D_FLOAT,
  CONSTRAINT PK_UIBMONTHEVENT PRIMARY KEY (USERID,JAHR,MONAT,"EVENT")
);
CREATE TABLE UIBSOLL
(
  USERID varchar(20) NOT NULL,
  JAHR integer NOT NULL,
  MONAT integer NOT NULL,
  STUNDEN D_FLOAT NOT NULL,
  CONSTRAINT PK_UIBSOLL PRIMARY KEY (USERID,JAHR,MONAT)
);
CREATE TABLE UIBTIMEOUT
(
  USERID varchar(20) NOT NULL,
  EVENT varchar(50) NOT NULL,
  TIMEOUT_MIN integer,
  CONSTRAINT PK_UIBTIMEOUT PRIMARY KEY (USERID,"EVENT")
);
CREATE TABLE UIBTMPTREESUM
(
  USERID varchar(20) NOT NULL,
  EVENT varchar(50) NOT NULL,
  ID_STRING varchar(50),
  STUNDEN D_FLOAT,
  CONSTRAINT PK_UIBTMPTREESUM PRIMARY KEY (USERID,"EVENT")
);
CREATE TABLE UIBTMPTREESUM2
(
  USERID varchar(20) NOT NULL,
  EVENT varchar(50) NOT NULL,
  ID_STRING varchar(50),
  UIBUSER varchar(20) NOT NULL,
  STUNDEN D_FLOAT,
  CONSTRAINT PK_UIBTMPTREESUM2 PRIMARY KEY (USERID,"EVENT",UIBUSER)
);
CREATE TABLE UIBTMP_SAPEVENT
(
  EVENT varchar(50) NOT NULL,
  SAPEVENT varchar(50) NOT NULL,
  ID_STRING varchar(50) NOT NULL,
  CONSTRAINT PK_UIBTMP_SAPEVENT PRIMARY KEY ("EVENT",SAPEVENT,ID_STRING)
);
CREATE TABLE UIBTVERSION
(
  MAJOR varchar(20) NOT NULL,
  MINOR varchar(50) NOT NULL,
  DATEDAY timestamp NOT NULL
);
CREATE TABLE UIBTWOCHE
(
  DAYNUM integer NOT NULL,
  DAYSTR varchar(2) NOT NULL,
  CONSTRAINT PK_UIBTWOCHE PRIMARY KEY (DAYNUM)
);
CREATE TABLE UIBUSEREVENT
(
  USERID varchar(20) NOT NULL,
  EVENT varchar(50) NOT NULL,
  BUTTON BOOLEAN,
  CONSTRAINT PK_USEREVENT PRIMARY KEY (USERID,"EVENT")
);
CREATE TABLE UIB_WORK_DESCRIPTION
(
  USERID varchar(20) NOT NULL,
  JAHR integer NOT NULL,
  MONAT integer NOT NULL,
  TAG integer NOT NULL,
  EVENT varchar(50) NOT NULL,
  DESCRIPTION varchar(255),
  CONSTRAINT PK_WDESCRIPTION_UID_YMD_EVENT PRIMARY KEY (USERID,JAHR,MONAT,TAG,"EVENT")
);
/********************* VIEWS **********************/

CREATE VIEW UIBCALLSTIMESPLIT (USERID, DAY_H, CALLS, MINUTE_, STUNDE, TAG, MONAT, JAHR, WOCHENTAG, JAHRESTAG)
AS  select
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
    uibcalls
;
CREATE VIEW UIBEVENTMONTHTIMESPLIT (USERID, JAHR, MONAT, EVENT, STUNDEN)
AS  select
    userid,
    jahr,
    monat,
    event,
    sum(stunden) as stunden
   from
    UIBEVENTTIMESPLIT
   group by userid, jahr, monat, event
;
CREATE VIEW UIBEVENTTIMESPLIT (USERID, STARTTIME, STOPTIME, MINUTE_, STUNDE, TAG, MONAT, JAHR, WOCHENTAG, JAHRESTAG, EVENT, STUNDEN)
AS  select
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
UIBEVENT
;
/******************* EXCEPTIONS *******************/

/******************** TRIGGERS ********************/

SET TERM ^ ;
CREATE TRIGGER CHECK_1 FOR UIBALLEVENT ACTIVE
BEFORE insert POSITION 0
check ((parentevent like 'root') or (parentevent in (select event from uiballevent)))^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER CHECK_10 FOR UIBAKTUSER ACTIVE
AFTER delete POSITION 1
^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER CHECK_11 FOR UIBAKTUSER ACTIVE
AFTER delete POSITION 1
^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER CHECK_12 FOR UIBAKTEVENT ACTIVE
AFTER delete POSITION 1
^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER CHECK_13 FOR UIBAKTUSER ACTIVE
AFTER delete POSITION 1
^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER CHECK_2 FOR UIBALLEVENT ACTIVE
BEFORE update POSITION 0
check ((parentevent like 'root') or (parentevent in (select event from uiballevent)))^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER CHECK_3 FOR UIBAKTEVENT ACTIVE
AFTER update POSITION 1
^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER CHECK_4 FOR UIBAKTEVENT ACTIVE
BEFORE insert POSITION 0
check ((parentevent like 'root') or (parentevent in (select event from uibaktevent)))^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER CHECK_5 FOR UIBAKTEVENT ACTIVE
BEFORE update POSITION 0
check ((parentevent like 'root') or (parentevent in (select event from uibaktevent)))^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER CHECK_6 FOR UIBAKTUSER ACTIVE
AFTER delete POSITION 1
^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER CHECK_7 FOR UIBAKTEVENT ACTIVE
AFTER delete POSITION 1
^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER CHECK_8 FOR UIBAKTUSER ACTIVE
AFTER delete POSITION 1
^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER CHECK_9 FOR UIBAKTEVENT ACTIVE
AFTER delete POSITION 1
^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_10_D_2_2 FOR UIBSOLL INACTIVE
AFTER delete POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,OLDKEY)
  VALUES (2,2,1,10,'D',OLD.JAHR||''||OLD.MONAT||''||OLD.USERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_10_I_2_2 FOR UIBSOLL INACTIVE
AFTER insert POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,NEWKEY)
  VALUES (2,2,1,10,'I',NEW.JAHR||''||NEW.MONAT||''||NEW.USERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_10_U_2_2 FOR UIBSOLL INACTIVE
AFTER update POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,OLDKEY,NEWKEY)
  VALUES (2,2,1,10,'U',OLD.JAHR||''||OLD.MONAT||''||OLD.USERID,NEW.JAHR||''||NEW.MONAT||''||NEW.USERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_1_D_2_2 FOR UIBAKTEVENT INACTIVE
AFTER delete POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,OLDKEY)
  VALUES (2,2,1,1,'D',OLD.EVENT);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_1_I_2_2 FOR UIBAKTEVENT INACTIVE
AFTER insert POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,NEWKEY)
  VALUES (2,2,1,1,'I',NEW.EVENT);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_1_U_2_2 FOR UIBAKTEVENT INACTIVE
AFTER update POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,OLDKEY,NEWKEY)
  VALUES (2,2,1,1,'U',OLD.EVENT,NEW.EVENT);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_27_D_2_2 FOR UIBLOGGEDIN INACTIVE
AFTER delete POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,OLDKEY)
  VALUES (2,2,1,27,'D',OLD.USERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_27_I_2_2 FOR UIBLOGGEDIN INACTIVE
AFTER insert POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,NEWKEY)
  VALUES (2,2,1,27,'I',NEW.USERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_27_U_2_2 FOR UIBLOGGEDIN INACTIVE
AFTER update POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,OLDKEY,NEWKEY)
  VALUES (2,2,1,27,'U',OLD.USERID,NEW.USERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_29_D_2_2 FOR UIB_WORK_DESCRIPTION INACTIVE
AFTER delete POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,OLDKEY)
  VALUES (2,2,1,29,'D',OLD.EVENT||''||OLD.JAHR||''||OLD.MONAT||''||OLD.TAG||''||OLD.USERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_29_I_2_2 FOR UIB_WORK_DESCRIPTION INACTIVE
AFTER insert POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,NEWKEY)
  VALUES (2,2,1,29,'I',NEW.EVENT||''||NEW.JAHR||''||NEW.MONAT||''||NEW.TAG||''||NEW.USERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_29_U_2_2 FOR UIB_WORK_DESCRIPTION INACTIVE
AFTER update POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,OLDKEY,NEWKEY)
  VALUES (2,2,1,29,'U',OLD.EVENT||''||OLD.JAHR||''||OLD.MONAT||''||OLD.TAG||''||OLD.USERID,NEW.EVENT||''||NEW.JAHR||''||NEW.MONAT||''||NEW.TAG||''||NEW.USERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_2_D_2_2 FOR UIBAKTUSER INACTIVE
AFTER delete POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,OLDKEY)
  VALUES (2,2,1,2,'D',OLD.USERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_2_I_2_2 FOR UIBAKTUSER INACTIVE
AFTER insert POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,NEWKEY)
  VALUES (2,2,1,2,'I',NEW.USERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_2_U_2_2 FOR UIBAKTUSER INACTIVE
AFTER update POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,OLDKEY,NEWKEY)
  VALUES (2,2,1,2,'U',OLD.USERID,NEW.USERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_3_D_2_2 FOR UIBALLEVENT INACTIVE
AFTER delete POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,OLDKEY)
  VALUES (2,2,1,3,'D',OLD.EVENT);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_3_I_2_2 FOR UIBALLEVENT INACTIVE
AFTER insert POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,NEWKEY)
  VALUES (2,2,1,3,'I',NEW.EVENT);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_3_U_2_2 FOR UIBALLEVENT INACTIVE
AFTER update POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,OLDKEY,NEWKEY)
  VALUES (2,2,1,3,'U',OLD.EVENT,NEW.EVENT);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_4_D_2_2 FOR UIBALLUSER INACTIVE
AFTER delete POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,OLDKEY)
  VALUES (2,2,1,4,'D',OLD.USERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_4_I_2_2 FOR UIBALLUSER INACTIVE
AFTER insert POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,NEWKEY)
  VALUES (2,2,1,4,'I',NEW.USERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_4_U_2_2 FOR UIBALLUSER INACTIVE
AFTER update POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,OLDKEY,NEWKEY)
  VALUES (2,2,1,4,'U',OLD.USERID,NEW.USERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_5_D_2_2 FOR UIBCALLS INACTIVE
AFTER delete POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,OLDKEY)
  VALUES (2,2,1,5,'D',OLD.DAY_H||''||OLD.USERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_5_I_2_2 FOR UIBCALLS INACTIVE
AFTER insert POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,NEWKEY)
  VALUES (2,2,1,5,'I',NEW.DAY_H||''||NEW.USERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_5_U_2_2 FOR UIBCALLS INACTIVE
AFTER update POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,OLDKEY,NEWKEY)
  VALUES (2,2,1,5,'U',OLD.DAY_H||''||OLD.USERID,NEW.DAY_H||''||NEW.USERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_7_D_2_2 FOR UIBEVENT INACTIVE
AFTER delete POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,OLDKEY)
  VALUES (2,2,1,7,'D',OLD.PKINITTIME||''||OLD.PKUSERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_7_I_2_2 FOR UIBEVENT INACTIVE
AFTER insert POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,NEWKEY)
  VALUES (2,2,1,7,'I',NEW.PKINITTIME||''||NEW.PKUSERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_7_U_2_2 FOR UIBEVENT INACTIVE
AFTER update POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,OLDKEY,NEWKEY)
  VALUES (2,2,1,7,'U',OLD.PKINITTIME||''||OLD.PKUSERID,NEW.PKINITTIME||''||NEW.PKUSERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL_GEN_SEQNO FOR REPL_LOG ACTIVE
BEFORE insert POSITION 0
AS BEGIN NEW.SEQNO = GEN_ID(REPL_GENERATOR,1); END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER TR_CANIASIMPORTERROR_IN FOR CANIASIMPORTERROR ACTIVE
BEFORE insert POSITION 0
as
         begin
           new.canimp_rep_id = NEXT VALUE FOR caniasimporterror_SEQU;
           new.created = current_timestamp;
         end^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER TR_UIBACCOUNTEXPORT_IN FOR UIBACCOUNTEXPORT ACTIVE
BEFORE insert POSITION 0
as
         begin
           new.acc_exp_id = NEXT VALUE FOR uibaccountexport_SEQU;
         end^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER TR_UIBACCOUNTREPORT_IN FOR UIBEVENTACCOUNTREPORT ACTIVE
BEFORE insert POSITION 0
as
         begin
           new.acc_rep_id = NEXT VALUE FOR uibaccountreport_SEQU;
         end^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER TR_UIBACCOUNT_IN FOR UIBACCOUNT ACTIVE
BEFORE insert POSITION 0
as
         begin
          new.stunden = (new.stoptime - new.starttime) * 24;
         end^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER TR_UIBACCOUNT_UP FOR UIBACCOUNT ACTIVE
BEFORE update POSITION 0
as
         begin
          new.stunden = (new.stoptime - new.starttime) * 24;
         end^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER TR_UIBAKTEVENT2UIBALL_IN FOR UIBAKTEVENT ACTIVE
BEFORE insert POSITION 0
as
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
                 kd_string = new.kd_string,
                 time_h = new.time_h,
                 projectstart = new.projectstart,
                 reportrequired = new.reportrequired,
                 accountingrequired = new.accountingrequired,
                 acc_per_monthnum = new.acc_per_monthnum,
                 MATERIAL = new.MATERIAL,
                 MATTYPE = new.MATTYPE,
                 time_h_is_quota = new.time_h_is_quota,
                 UIB3MONTHINVOICE = new.UIB3MONTHINVOICE,
                 quota_lifetime_month = new.quota_lifetime_month
            where (event = new.event);
          end
          else
          begin
           insert into uiballevent
               (event,
                parentevent,
                comment,
                id_string,
                kd_string,
                time_h,
                projectstart,
                reportrequired,
                accountingrequired,
                acc_per_monthnum,
                MATERIAL,
                MATTYPE,
                time_h_is_quota,
                UIB3MONTHINVOICE,
                quota_lifetime_month)
             values
               (new.event,
                new.parentevent,
                new.comment,
                new.id_string,
                new.kd_string,
                new.time_h,
                new.projectstart,
                new.reportrequired,
                new.accountingrequired,
                new.acc_per_monthnum,
                new.MATERIAL,
                new.MATTYPE,
                new.time_h_is_quota,
                new.UIB3MONTHINVOICE,
                new.quota_lifetime_month);
          end
         end^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER TR_UIBAKTEVENT2UIBALL_UP FOR UIBAKTEVENT ACTIVE
BEFORE update POSITION 0
as
         begin
          update uiballevent
            set parentevent = new.parentevent,
                 comment    = new.comment,
                 id_string = new.id_string,
                 kd_string = new.kd_string,
                 time_h = new.time_h,
                 projectstart = new.projectstart,
                 reportrequired = new.reportrequired,
                 accountingrequired = new.accountingrequired,
                 acc_per_monthnum = new.acc_per_monthnum,
                 MATERIAL = new.MATERIAL,
                 MATTYPE = new.MATTYPE,
                 time_h_is_quota = new.time_h_is_quota,
                 UIB3MONTHINVOICE = new.UIB3MONTHINVOICE,
                 quota_lifetime_month = new.quota_lifetime_month
           where (event = old.event);
         end^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER TR_UIBAKTUSER2UIBALL_IN FOR UIBAKTUSER ACTIVE
BEFORE insert POSITION 0
as
          DECLARE VARIABLE isInAlluser smallint;
         begin
          select count(*) from uiballuser
           where userid = new.userid
           into :isInAlluser;
          if (:isInAlluser > 0) then
          begin
            update uiballuser
            set name = new.name,
                uibmember = new.uibmember,
                email = new.email,
                mypassword = new.mypassword,
                h_per_day = new.h_per_day,
                mo_is_work = new.mo_is_work ,
                di_is_work = new.di_is_work,
                mi_is_work = new.mi_is_work,
                do_is_work = new.do_is_work,
                fr_is_work = new.fr_is_work,
                bundesland = new.bundesland
              where (userid = new.userid);
          end
          else
          begin
           insert into uiballuser
                   (userid,
                    name,
                    uibmember,
                    email,
                    mypassword,
                    h_per_day,
                    mo_is_work,
                    di_is_work,
                    mi_is_work,
                    do_is_work,
                    fr_is_work,
                    bundesland)
                 values
                   (new.userid,
                    new.name,
                    new.uibmember,
                    new.email,
                    new.mypassword,
                    new.h_per_day,
                    new.mo_is_work, 
                    new.di_is_work,
                    new.mi_is_work,
                    new.do_is_work,
                    new.fr_is_work,
                    new.bundesland);
          end
         end^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER TR_UIBAKTUSER2UIBALL_UP FOR UIBAKTUSER ACTIVE
BEFORE update POSITION 0
as
         begin
          update uiballuser
            set name = new.name,
                uibmember = new.uibmember,
                email = new.email,
                mypassword = new.mypassword,
                h_per_day = new.h_per_day,
                mo_is_work = new.mo_is_work ,
                di_is_work = new.di_is_work,
                mi_is_work = new.mi_is_work,
                do_is_work = new.do_is_work,
                fr_is_work = new.fr_is_work,
                bundesland = new.bundesland
              where (userid = old.userid);
         end^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER TR_UIBEVENT_IN FOR UIBEVENT ACTIVE
BEFORE insert POSITION 0
as
         begin
          new.stunden = (new.stoptime - new.starttime) * 24;
         end^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER TR_UIBEVENT_UP FOR UIBEVENT ACTIVE
BEFORE update POSITION 0
as
         begin
          new.stunden = (new.stoptime - new.starttime) * 24;
         end^
SET TERM ; ^

SET TERM ^ ;
ALTER PROCEDURE BUILDBSZ2_TMP_MONTHREP (
    AKTUSER varchar(20),
    SEARCHID varchar(20),
    VON timestamp,
    BIS timestamp )
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
END^
SET TERM ; ^


SET TERM ^ ;
ALTER PROCEDURE BUILDBSZ2_TMP_MONTHREP_AKTEVENT (
    AKTUSER varchar(20),
    SEARCHID varchar(20),
    VON timestamp,
    BIS timestamp )
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
END^
SET TERM ; ^


SET TERM ^ ;
ALTER PROCEDURE BUILDBSZ_TMP_MONTHREP (
    AKTUSER varchar(20),
    SEARCHID varchar(20),
    VON timestamp,
    BIS timestamp )
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
END^
SET TERM ; ^


SET TERM ^ ;
ALTER PROCEDURE BUILDBSZ_TMP_MONTHREP_AKTEVENT (
    AKTUSER varchar(20),
    SEARCHID varchar(20),
    VON timestamp,
    BIS timestamp )
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
END^
SET TERM ; ^


SET TERM ^ ;
ALTER PROCEDURE BUILDBSZ_TMP_SAPEVENT
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
END^
SET TERM ; ^


SET TERM ^ ;
ALTER PROCEDURE BUILDBSZ_TMP_SAPEVENT_AKTEVENT
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
END^
SET TERM ; ^


SET TERM ^ ;
ALTER PROCEDURE UIBBUILD_TMP_TREESUM (
    BELOW_EVENT varchar(50),
    AKTUSER varchar(20),
    SEARCHID varchar(20),
    VON timestamp,
    BIS timestamp )
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
END^
SET TERM ; ^


SET TERM ^ ;
ALTER PROCEDURE UIBBUILD_TMP_TREESUM_AKTEVENT (
    BELOW_EVENT varchar(50),
    AKTUSER varchar(20),
    SEARCHID varchar(20),
    VON timestamp,
    BIS timestamp )
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
END^
SET TERM ; ^


comment on column EVAL_MONTH.UIB_USER is 'uibtime name of user';
comment on column EVAL_MONTH.UIB_YEAR is 'Jahr';
comment on column EVAL_MONTH.UIB_MONTH is 'Monat';
comment on column EVAL_MONTH.UIB_MONTH_SOLL is 'Sollstunden des Monats (ohne Feiertage)';
comment on column EVAL_MONTH.UIB_MONTH_IST is 'Iststunden des Monats';
comment on column EVAL_MONTH.UIB_MONTH_KORR is 'Korrekturstunden (plus/minus)';
comment on column EVAL_MONTH.BUNDESLAND is 'Bundesland (Feiertage)';
comment on column EVAL_MONTH.H_PER_DAY is 'Arbeitsstunden pro Tag';
comment on column EVAL_MONTH.MO_IS_WORK is 'Montag ist Arbeitstag';
comment on column EVAL_MONTH.DI_IS_WORK is 'Dienstag ist Arbeitstag';
comment on column EVAL_MONTH.MI_IS_WORK is 'Mittwoch ist Arbeitstag';
comment on column EVAL_MONTH.DO_IS_WORK is 'Donnerstag ist Arbeitstag';
comment on column EVAL_MONTH.FR_IS_WORK is 'Freitag ist Arbeitstag';
comment on column EVAL_MONTH.UIB_MONTH_PRODUKTIV is 'Produktivstunden (fuer Ausschuettung)';
comment on column EVAL_MONTH.UIB_MONTH_SOLL_CONTRACT is 'Sollstunden des Monats incl. Feiertagen';
comment on column EVAL_MONTH.UIB_COMMENT is 'Kommentar';
comment on column EVAL_MONTH.UIB_TIMESTAMP is 'change timestamp';
comment on column EVAL_MONTH.UIB_CHANGEDBY is 'changed by user';
comment on table EVAL_MONTH is 'uibtime Monatsabschluss';
comment on column EVAL_USER.UIB_USER is 'uibtime user name';
comment on column EVAL_USER.FROM_DATE is 'YYYY_MM_DD start of this active period';
comment on column EVAL_USER.TO_DATE is 'YYYY_MM_DD stop of this active period (including end date)';
comment on table EVAL_USER is 'uibtime user active periods';
comment on column EVAL_VACATION_IST.UIB_USER is 'uibtime name of user';
comment on column EVAL_VACATION_IST.ACCOUNTING_YEAR is 'Jahr Urlaubsanspruch';
comment on column EVAL_VACATION_IST.FROM_YEAR is 'Urlaub von Jahr';
comment on column EVAL_VACATION_IST.FROM_MONTH is 'Urlaub von Monat';
comment on column EVAL_VACATION_IST.FROM_DAY is 'Urlaub von Tag';
comment on column EVAL_VACATION_IST.TO_YEAR is 'Urlaub bis incl. Jahr';
comment on column EVAL_VACATION_IST.TO_MONTH is 'Urlaub bis incl. Monat';
comment on column EVAL_VACATION_IST.TO_DAY is 'Urlaub bis incl. Tag';
comment on column EVAL_VACATION_IST.VACATION_DAYS is 'entspricht Anzahl Urlaubstage';
comment on column EVAL_VACATION_IST.H_PER_DAY is 'Stunden pro Tag';
comment on column EVAL_VACATION_IST.UIB_COMMENT is 'Kommentar';
comment on column EVAL_VACATION_IST.UIB_TIMESTAMP is 'change timestamp';
comment on column EVAL_VACATION_IST.UIB_CHANGEDBY is 'changed by user';
comment on table EVAL_VACATION_IST is 'Laut Urlaubsschein genommener Urlaub von - bis (incl.)';
comment on column EVAL_VACATION_SOLL.UIB_USER is 'uibtime name of user';
comment on column EVAL_VACATION_SOLL.UIB_YEAR is 'Jahr des Urlaubsanspruchs';
comment on column EVAL_VACATION_SOLL.VACATION_DAYS is 'Anzahl Tage Urlaubsanspruch';
comment on column EVAL_VACATION_SOLL.UIB_COMMENT is 'Kommentar';
comment on column EVAL_VACATION_SOLL.UIB_TIMESTAMP is 'changed timestamp';
comment on column EVAL_VACATION_SOLL.UIB_CHANGEDBY is 'changed by user';
comment on table EVAL_VACATION_SOLL is 'Urlaubsanspruch des Jahres';
comment on column EVAL_YEAR.UIB_USER is 'uibtime name of user';
comment on column EVAL_YEAR.UIB_YEAR is 'Jahr';
comment on column EVAL_YEAR.UIB_YEAR_SOLL is 'Jahr Sollstunden';
comment on column EVAL_YEAR.UIB_YEAR_IST is 'Jahr Iststunden';
comment on column EVAL_YEAR.UIB_SUM_PRE is 'Uebertrag Ueberstunden aus Vorjahr';
comment on column EVAL_YEAR.UIB_SUM_AFTER is 'Uebertrag Ueberstunden ins Folgejahr';
comment on table EVAL_YEAR is 'uibtime Jahresabschluss';
CREATE INDEX I_REPL_LOG ON REPL_LOG (SEQNO);
ALTER TABLE UIBACCOUNT ADD CONSTRAINT FK_UIBACCOUNT_EVENT
  FOREIGN KEY ("EVENT") REFERENCES UIBALLEVENT ("EVENT");
ALTER TABLE UIBACCOUNT ADD CONSTRAINT FK_UIBACCOUNT_USERID
  FOREIGN KEY (USERID) REFERENCES UIBALLUSER (USERID);
CREATE UNIQUE INDEX UK_UIBACCOUNT ON UIBACCOUNT (USERID,STARTTIME);
ALTER TABLE UIBACCOUNTEXPORT ADD CONSTRAINT FK_UIBACCEXP_EVENT
  FOREIGN KEY ("EVENT") REFERENCES UIBALLEVENT ("EVENT");
ALTER TABLE UIBAKTEVENT ADD CONSTRAINT FK_SELF_UIBAKTEVENT_PARENTEVENT
  FOREIGN KEY (PARENTEVENT) REFERENCES UIBAKTEVENT ("EVENT") ON UPDATE CASCADE;
ALTER TABLE UIBAKTEVENT ADD CONSTRAINT FK_UIBAKTEVENT_EVENT
  FOREIGN KEY ("EVENT") REFERENCES UIBALLEVENT ("EVENT");
ALTER TABLE UIBAKTEVENT ADD CONSTRAINT CK_ONE_ROOT_UIBAKTEVENT
  check ((parentevent like 'root') or (parentevent in (select event from uibaktevent)));
ALTER TABLE UIBAKTUSER ADD CONSTRAINT FK_UIBAKTUSER_UIBALLUSER
  FOREIGN KEY (USERID) REFERENCES UIBALLUSER (USERID);
ALTER TABLE UIBALLEVENT ADD CONSTRAINT FK_SELF_UIBALLEVENT_PARENTEVENT
  FOREIGN KEY (PARENTEVENT) REFERENCES UIBALLEVENT ("EVENT");
ALTER TABLE UIBALLEVENT ADD CONSTRAINT CK_ONE_ROOT_UIBALLEVENT
  check ((parentevent like 'root') or (parentevent in (select event from uiballevent)));
ALTER TABLE UIBCALLS ADD CONSTRAINT FK_UIBCALLS_USERID
  FOREIGN KEY (USERID) REFERENCES UIBALLUSER (USERID);
ALTER TABLE UIBDEFPROJ ADD CONSTRAINT FK_DEFPROJ_EVENT
  FOREIGN KEY ("EVENT") REFERENCES UIBAKTEVENT ("EVENT") ON DELETE CASCADE;
ALTER TABLE UIBDEFPROJ ADD CONSTRAINT FK_UIBDEFPROJ_USERID
  FOREIGN KEY (USERID) REFERENCES UIBAKTUSER (USERID) ON DELETE CASCADE;
ALTER TABLE UIBEVENT ADD CONSTRAINT FK_UIBEVENT_EVENT
  FOREIGN KEY ("EVENT") REFERENCES UIBALLEVENT ("EVENT");
ALTER TABLE UIBEVENT ADD CONSTRAINT FK_UIBEVENT_USERID
  FOREIGN KEY (USERID) REFERENCES UIBALLUSER (USERID);
CREATE UNIQUE INDEX UK_UIBEVENT ON UIBEVENT (USERID,STARTTIME);
ALTER TABLE UIBEVENTACCOUNTREPORT ADD CONSTRAINT FK_UIBEVENTAC_EVENT
  FOREIGN KEY ("EVENT") REFERENCES UIBALLEVENT ("EVENT");
ALTER TABLE UIBEVENTACCOUNTREPORT ADD CONSTRAINT FK_UIBEVENTAC_USERID
  FOREIGN KEY (USERID) REFERENCES UIBALLUSER (USERID);
ALTER TABLE UIBLOGGEDIN ADD CONSTRAINT FK_LOGGEDIN_USERID
  FOREIGN KEY (USERID) REFERENCES UIBAKTUSER (USERID) ON DELETE CASCADE;
ALTER TABLE UIBMONTHEVENT ADD CONSTRAINT FK_UIBMONTHEVENT_EVENT
  FOREIGN KEY ("EVENT") REFERENCES UIBALLEVENT ("EVENT");
ALTER TABLE UIBMONTHEVENT ADD CONSTRAINT FK_UIBMONTHEVENT_USERID
  FOREIGN KEY (USERID) REFERENCES UIBALLUSER (USERID);
ALTER TABLE UIBSOLL ADD CONSTRAINT FK_UIBSOLL_USERID
  FOREIGN KEY (USERID) REFERENCES UIBAKTUSER (USERID) ON DELETE CASCADE;
ALTER TABLE UIBTIMEOUT ADD CONSTRAINT FK_UIBTIMEOUT_ACTIVITY
  FOREIGN KEY ("EVENT") REFERENCES UIBAKTEVENT ("EVENT") ON DELETE CASCADE;
ALTER TABLE UIBTIMEOUT ADD CONSTRAINT FK_UIBTIMEOUT_USERID
  FOREIGN KEY (USERID) REFERENCES UIBAKTUSER (USERID) ON DELETE CASCADE;
ALTER TABLE UIBUSEREVENT ADD CONSTRAINT FK_USEREVENT_AKTEVENT
  FOREIGN KEY ("EVENT") REFERENCES UIBAKTEVENT ("EVENT") ON DELETE CASCADE;
ALTER TABLE UIBUSEREVENT ADD CONSTRAINT FK_USEREVENT_USERID
  FOREIGN KEY (USERID) REFERENCES UIBAKTUSER (USERID) ON DELETE CASCADE;
ALTER TABLE UIB_WORK_DESCRIPTION ADD CONSTRAINT FK_WDESCRIPTION_EVENT
  FOREIGN KEY ("EVENT") REFERENCES UIBALLEVENT ("EVENT");
ALTER TABLE UIB_WORK_DESCRIPTION ADD CONSTRAINT FK_WDESCRIPTION_USERID
  FOREIGN KEY (USERID) REFERENCES UIBALLUSER (USERID);
GRANT EXECUTE
 ON PROCEDURE BUILDBSZ2_TMP_MONTHREP TO  UIBTIME;

GRANT EXECUTE
 ON PROCEDURE BUILDBSZ2_TMP_MONTHREP TO  UIBTIMEREAD;

GRANT EXECUTE
 ON PROCEDURE BUILDBSZ2_TMP_MONTHREP_AKTEVENT TO  UIBTIME;

GRANT EXECUTE
 ON PROCEDURE BUILDBSZ2_TMP_MONTHREP_AKTEVENT TO  UIBTIMEREAD;

GRANT EXECUTE
 ON PROCEDURE BUILDBSZ_TMP_MONTHREP TO  UIBTIME;

GRANT EXECUTE
 ON PROCEDURE BUILDBSZ_TMP_MONTHREP TO  UIBTIMEREAD;

GRANT EXECUTE
 ON PROCEDURE BUILDBSZ_TMP_MONTHREP_AKTEVENT TO  UIBTIME;

GRANT EXECUTE
 ON PROCEDURE BUILDBSZ_TMP_MONTHREP_AKTEVENT TO  UIBTIMEREAD;

GRANT EXECUTE
 ON PROCEDURE BUILDBSZ_TMP_SAPEVENT TO  UIBTIME;

GRANT EXECUTE
 ON PROCEDURE BUILDBSZ_TMP_SAPEVENT TO  UIBTIMEREAD;

GRANT EXECUTE
 ON PROCEDURE BUILDBSZ_TMP_SAPEVENT_AKTEVENT TO  UIBTIME;

GRANT EXECUTE
 ON PROCEDURE BUILDBSZ_TMP_SAPEVENT_AKTEVENT TO  UIBTIMEREAD;

GRANT EXECUTE
 ON PROCEDURE UIBBUILD_TMP_TREESUM TO  UIBTIME;

GRANT EXECUTE
 ON PROCEDURE UIBBUILD_TMP_TREESUM TO  UIBTIMEREAD;

GRANT EXECUTE
 ON PROCEDURE UIBBUILD_TMP_TREESUM_AKTEVENT TO  UIBTIME;

GRANT EXECUTE
 ON PROCEDURE UIBBUILD_TMP_TREESUM_AKTEVENT TO  UIBTIMEREAD;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON CANIASIMPORTERROR TO  UIBTIME WITH GRANT OPTION;

GRANT SELECT
 ON CANIASIMPORTERROR TO  UIBTIMEREAD;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON CANIASIMPORTERROR TO  UIBTIME_ERP;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON EVAL_MONTH TO  SYSDBA WITH GRANT OPTION;

GRANT SELECT
 ON EVAL_MONTH TO  UIBTIME;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON EVAL_MONTH TO  UIBTIMEAUSW WITH GRANT OPTION;

GRANT SELECT
 ON EVAL_MONTH TO  UIBTIMEREAD;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON EVAL_USER TO  SYSDBA WITH GRANT OPTION;

GRANT SELECT
 ON EVAL_USER TO  UIBTIME;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON EVAL_USER TO  UIBTIMEAUSW;

GRANT SELECT
 ON EVAL_USER TO  UIBTIMEREAD;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON EVAL_VACATION_IST TO  SYSDBA WITH GRANT OPTION;

GRANT SELECT
 ON EVAL_VACATION_IST TO  UIBTIME;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON EVAL_VACATION_IST TO  UIBTIMEAUSW WITH GRANT OPTION;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON EVAL_VACATION_IST TO  UIBTIMEREAD WITH GRANT OPTION;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON EVAL_VACATION_SOLL TO  SYSDBA WITH GRANT OPTION;

GRANT SELECT
 ON EVAL_VACATION_SOLL TO  UIBTIME;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON EVAL_VACATION_SOLL TO  UIBTIMEAUSW;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON EVAL_VACATION_SOLL TO  UIBTIMEREAD WITH GRANT OPTION;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON EVAL_YEAR TO  SYSDBA WITH GRANT OPTION;

GRANT SELECT
 ON EVAL_YEAR TO  UIBTIME;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON EVAL_YEAR TO  UIBTIMEAUSW WITH GRANT OPTION;

GRANT SELECT
 ON EVAL_YEAR TO  UIBTIMEREAD;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON MANUAL_LOG TO  REPL;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON MANUAL_LOG TO  SYSDBA WITH GRANT OPTION;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON MANUAL_LOG TO  UIBTIME;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON PUBLIC_HOLIDAYS TO  SYSDBA WITH GRANT OPTION;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON PUBLIC_HOLIDAYS TO  UIBTIME WITH GRANT OPTION;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON REPL_LOG TO  REPL;

GRANT INSERT
 ON REPL_LOG TO TRIGGER REPL$1_10_D_2_2;

GRANT INSERT
 ON REPL_LOG TO TRIGGER REPL$1_10_I_2_2;

GRANT INSERT
 ON REPL_LOG TO TRIGGER REPL$1_10_U_2_2;

GRANT INSERT
 ON REPL_LOG TO TRIGGER REPL$1_1_D_2_2;

GRANT INSERT
 ON REPL_LOG TO TRIGGER REPL$1_1_I_2_2;

GRANT INSERT
 ON REPL_LOG TO TRIGGER REPL$1_1_U_2_2;

GRANT INSERT
 ON REPL_LOG TO TRIGGER REPL$1_27_D_2_2;

GRANT INSERT
 ON REPL_LOG TO TRIGGER REPL$1_27_I_2_2;

GRANT INSERT
 ON REPL_LOG TO TRIGGER REPL$1_27_U_2_2;

GRANT INSERT
 ON REPL_LOG TO TRIGGER REPL$1_29_D_2_2;

GRANT INSERT
 ON REPL_LOG TO TRIGGER REPL$1_29_I_2_2;

GRANT INSERT
 ON REPL_LOG TO TRIGGER REPL$1_29_U_2_2;

GRANT INSERT
 ON REPL_LOG TO TRIGGER REPL$1_2_D_2_2;

GRANT INSERT
 ON REPL_LOG TO TRIGGER REPL$1_2_I_2_2;

GRANT INSERT
 ON REPL_LOG TO TRIGGER REPL$1_2_U_2_2;

GRANT INSERT
 ON REPL_LOG TO TRIGGER REPL$1_3_D_2_2;

GRANT INSERT
 ON REPL_LOG TO TRIGGER REPL$1_3_I_2_2;

GRANT INSERT
 ON REPL_LOG TO TRIGGER REPL$1_3_U_2_2;

GRANT INSERT
 ON REPL_LOG TO TRIGGER REPL$1_4_D_2_2;

GRANT INSERT
 ON REPL_LOG TO TRIGGER REPL$1_4_I_2_2;

GRANT INSERT
 ON REPL_LOG TO TRIGGER REPL$1_4_U_2_2;

GRANT INSERT
 ON REPL_LOG TO TRIGGER REPL$1_5_D_2_2;

GRANT INSERT
 ON REPL_LOG TO TRIGGER REPL$1_5_I_2_2;

GRANT INSERT
 ON REPL_LOG TO TRIGGER REPL$1_5_U_2_2;

GRANT INSERT
 ON REPL_LOG TO TRIGGER REPL$1_7_D_2_2;

GRANT INSERT
 ON REPL_LOG TO TRIGGER REPL$1_7_I_2_2;

GRANT INSERT
 ON REPL_LOG TO TRIGGER REPL$1_7_U_2_2;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON REPL_LOG TO  SYSDBA WITH GRANT OPTION;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON REPL_LOG TO  UIBTIME;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON REPL_SEPARATOR TO  REPL;

GRANT SELECT
 ON REPL_SEPARATOR TO TRIGGER REPL$1_10_D_2_2;

GRANT SELECT
 ON REPL_SEPARATOR TO TRIGGER REPL$1_10_I_2_2;

GRANT SELECT
 ON REPL_SEPARATOR TO TRIGGER REPL$1_10_U_2_2;

GRANT SELECT
 ON REPL_SEPARATOR TO TRIGGER REPL$1_1_D_2_2;

GRANT SELECT
 ON REPL_SEPARATOR TO TRIGGER REPL$1_1_I_2_2;

GRANT SELECT
 ON REPL_SEPARATOR TO TRIGGER REPL$1_1_U_2_2;

GRANT SELECT
 ON REPL_SEPARATOR TO TRIGGER REPL$1_27_D_2_2;

GRANT SELECT
 ON REPL_SEPARATOR TO TRIGGER REPL$1_27_I_2_2;

GRANT SELECT
 ON REPL_SEPARATOR TO TRIGGER REPL$1_27_U_2_2;

GRANT SELECT
 ON REPL_SEPARATOR TO TRIGGER REPL$1_29_D_2_2;

GRANT SELECT
 ON REPL_SEPARATOR TO TRIGGER REPL$1_29_I_2_2;

GRANT SELECT
 ON REPL_SEPARATOR TO TRIGGER REPL$1_29_U_2_2;

GRANT SELECT
 ON REPL_SEPARATOR TO TRIGGER REPL$1_2_D_2_2;

GRANT SELECT
 ON REPL_SEPARATOR TO TRIGGER REPL$1_2_I_2_2;

GRANT SELECT
 ON REPL_SEPARATOR TO TRIGGER REPL$1_2_U_2_2;

GRANT SELECT
 ON REPL_SEPARATOR TO TRIGGER REPL$1_3_D_2_2;

GRANT SELECT
 ON REPL_SEPARATOR TO TRIGGER REPL$1_3_I_2_2;

GRANT SELECT
 ON REPL_SEPARATOR TO TRIGGER REPL$1_3_U_2_2;

GRANT SELECT
 ON REPL_SEPARATOR TO TRIGGER REPL$1_4_D_2_2;

GRANT SELECT
 ON REPL_SEPARATOR TO TRIGGER REPL$1_4_I_2_2;

GRANT SELECT
 ON REPL_SEPARATOR TO TRIGGER REPL$1_4_U_2_2;

GRANT SELECT
 ON REPL_SEPARATOR TO TRIGGER REPL$1_5_D_2_2;

GRANT SELECT
 ON REPL_SEPARATOR TO TRIGGER REPL$1_5_I_2_2;

GRANT SELECT
 ON REPL_SEPARATOR TO TRIGGER REPL$1_5_U_2_2;

GRANT SELECT
 ON REPL_SEPARATOR TO TRIGGER REPL$1_7_D_2_2;

GRANT SELECT
 ON REPL_SEPARATOR TO TRIGGER REPL$1_7_I_2_2;

GRANT SELECT
 ON REPL_SEPARATOR TO TRIGGER REPL$1_7_U_2_2;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON REPL_SEPARATOR TO  SYSDBA WITH GRANT OPTION;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON REPL_SEPARATOR TO  UIBTIME;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBACCOUNT TO  UIBTIME WITH GRANT OPTION;

GRANT SELECT
 ON UIBACCOUNT TO  UIBTIMEAUSW;

GRANT SELECT
 ON UIBACCOUNT TO  UIBTIMEREAD;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBACCOUNTEXPORT TO  SYSDBA WITH GRANT OPTION;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBACCOUNTEXPORT TO  UIBTIME WITH GRANT OPTION;

GRANT SELECT
 ON UIBACCOUNTEXPORT TO  UIBTIMEREAD;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBACCOUNTEXPORT TO  UIBTIME_ERP;

GRANT DELETE, INSERT, REFERENCES, SELECT, SELECT, UPDATE
 ON UIBAKTEVENT TO  REPL;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBAKTEVENT TO  UIBTIME WITH GRANT OPTION;

GRANT SELECT
 ON UIBAKTEVENT TO  UIBTIMEAUSW;

GRANT SELECT
 ON UIBAKTEVENT TO  UIBTIMEREAD;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBAKTEVENT TO  UIBTIME_ERP;

GRANT DELETE, INSERT, REFERENCES, SELECT, SELECT, UPDATE
 ON UIBAKTUSER TO  REPL;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBAKTUSER TO  UIBTIME WITH GRANT OPTION;

GRANT SELECT
 ON UIBAKTUSER TO  UIBTIMEAUSW;

GRANT SELECT
 ON UIBAKTUSER TO  UIBTIMEREAD;

GRANT DELETE, INSERT, REFERENCES, SELECT, SELECT, UPDATE
 ON UIBALLEVENT TO  REPL;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBALLEVENT TO  UIBTIME WITH GRANT OPTION;

GRANT SELECT
 ON UIBALLEVENT TO  UIBTIMEAUSW;

GRANT SELECT
 ON UIBALLEVENT TO  UIBTIMEREAD;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBALLEVENT TO  UIBTIME_ERP;

GRANT DELETE, INSERT, REFERENCES, SELECT, SELECT, UPDATE
 ON UIBALLUSER TO  REPL;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBALLUSER TO  UIBTIME WITH GRANT OPTION;

GRANT SELECT
 ON UIBALLUSER TO  UIBTIMEAUSW;

GRANT SELECT
 ON UIBALLUSER TO  UIBTIMEREAD;

GRANT DELETE, INSERT, REFERENCES, SELECT, SELECT, UPDATE
 ON UIBCALLS TO  REPL;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBCALLS TO  UIBTIME WITH GRANT OPTION;

GRANT SELECT
 ON UIBCALLS TO  UIBTIMEREAD;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBDEFPROJ TO  REPL;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBDEFPROJ TO  UIBTIME WITH GRANT OPTION;

GRANT SELECT
 ON UIBDEFPROJ TO  UIBTIMEAUSW;

GRANT SELECT
 ON UIBDEFPROJ TO  UIBTIMEREAD;

GRANT DELETE, INSERT, REFERENCES, SELECT, SELECT, UPDATE
 ON UIBEVENT TO  REPL;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBEVENT TO  UIBTIME WITH GRANT OPTION;

GRANT SELECT
 ON UIBEVENT TO  UIBTIMEAUSW;

GRANT SELECT
 ON UIBEVENT TO  UIBTIMEREAD;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBEVENTACCOUNTREPORT TO  UIBTIME WITH GRANT OPTION;

GRANT SELECT
 ON UIBEVENTACCOUNTREPORT TO  UIBTIMEAUSW;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBEVRTEMP TO  UIBTIME WITH GRANT OPTION;

GRANT DELETE, INSERT, REFERENCES, SELECT, SELECT, UPDATE
 ON UIBLOGGEDIN TO  REPL;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBLOGGEDIN TO  UIBTIME WITH GRANT OPTION;

GRANT SELECT
 ON UIBLOGGEDIN TO  UIBTIMEREAD;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBMONTHEVENT TO  REPL;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBMONTHEVENT TO  UIBTIME WITH GRANT OPTION;

GRANT SELECT
 ON UIBMONTHEVENT TO  UIBTIMEREAD;

GRANT DELETE, INSERT, REFERENCES, SELECT, SELECT, UPDATE
 ON UIBSOLL TO  REPL;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBSOLL TO  UIBTIME WITH GRANT OPTION;

GRANT DELETE, INSERT, SELECT, SELECT, UPDATE
 ON UIBSOLL TO  UIBTIMEAUSW;

GRANT SELECT
 ON UIBSOLL TO  UIBTIMEREAD;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBTIMEOUT TO  REPL;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBTIMEOUT TO  UIBTIME WITH GRANT OPTION;

GRANT SELECT
 ON UIBTIMEOUT TO  UIBTIMEREAD;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBTMPTREESUM TO  REPL;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBTMPTREESUM TO  UIBTIME WITH GRANT OPTION;

GRANT SELECT
 ON UIBTMPTREESUM TO  UIBTIMEREAD;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBTMPTREESUM2 TO  REPL;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBTMPTREESUM2 TO  UIBTIME WITH GRANT OPTION;

GRANT SELECT
 ON UIBTMPTREESUM2 TO  UIBTIMEREAD;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBTMP_SAPEVENT TO  UIBTIME WITH GRANT OPTION;

GRANT SELECT
 ON UIBTMP_SAPEVENT TO  UIBTIMEREAD;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBTVERSION TO  UIBTIME WITH GRANT OPTION;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBTVERSION TO  UIBTIMEREAD;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBTWOCHE TO  UIBTIME WITH GRANT OPTION;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBUSEREVENT TO  REPL;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBUSEREVENT TO  UIBTIME WITH GRANT OPTION;

GRANT SELECT
 ON UIBUSEREVENT TO  UIBTIMEREAD;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIB_WORK_DESCRIPTION TO  REPL;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIB_WORK_DESCRIPTION TO  UIBTIME WITH GRANT OPTION;

GRANT SELECT
 ON UIB_WORK_DESCRIPTION TO  UIBTIMEAUSW;

GRANT SELECT
 ON UIB_WORK_DESCRIPTION TO  UIBTIMEREAD;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBCALLSTIMESPLIT TO  UIBTIME WITH GRANT OPTION;

GRANT SELECT
 ON UIBCALLSTIMESPLIT TO  UIBTIMEREAD;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBEVENTMONTHTIMESPLIT TO  UIBTIME WITH GRANT OPTION;

GRANT SELECT
 ON UIBEVENTMONTHTIMESPLIT TO  UIBTIMEREAD;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBEVENTTIMESPLIT TO  UIBTIME WITH GRANT OPTION;

GRANT SELECT
 ON UIBEVENTTIMESPLIT TO  UIBTIMEREAD;

