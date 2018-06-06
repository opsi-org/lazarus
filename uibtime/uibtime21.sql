/********************* ROLES **********************/

/********************* UDFS ***********************/

/****************** GENERATORS ********************/

CREATE GENERATOR REPL_GENERATOR;
CREATE GENERATOR UIBACCOUNTEXPORT_SEQU;
CREATE GENERATOR UIBACCOUNTREPORT_SEQU;
/******************** DOMAINS *********************/

CREATE DOMAIN "BOOLEAN"
 AS Smallint
 DEFAULT 0
 NOT NULL
 check(value in (0,1))
;
CREATE DOMAIN D_FLOAT
 AS Numeric(15,4)
;
/******************* PROCEDURES ******************/

SET TERM ^ ;
CREATE PROCEDURE BUILDBSZ2_TMP_MONTHREP (
    AKTUSER Varchar(20),
    SEARCHID Varchar(20),
    VON Timestamp,
    BIS Timestamp )
AS
BEGIN SUSPEND; END^
SET TERM ; ^

SET TERM ^ ;
CREATE PROCEDURE BUILDBSZ2_TMP_MONTHREP_AKTEVENT (
    AKTUSER Varchar(20),
    SEARCHID Varchar(20),
    VON Timestamp,
    BIS Timestamp )
AS
BEGIN SUSPEND; END^
SET TERM ; ^

SET TERM ^ ;
CREATE PROCEDURE BUILDBSZ_TMP_MONTHREP (
    AKTUSER Varchar(20),
    SEARCHID Varchar(20),
    VON Timestamp,
    BIS Timestamp )
AS
BEGIN SUSPEND; END^
SET TERM ; ^

SET TERM ^ ;
CREATE PROCEDURE BUILDBSZ_TMP_MONTHREP_AKTEVENT (
    AKTUSER Varchar(20),
    SEARCHID Varchar(20),
    VON Timestamp,
    BIS Timestamp )
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
    BELOW_EVENT Varchar(50),
    AKTUSER Varchar(20),
    SEARCHID Varchar(20),
    VON Timestamp,
    BIS Timestamp )
AS
BEGIN SUSPEND; END^
SET TERM ; ^

SET TERM ^ ;
CREATE PROCEDURE UIBBUILD_TMP_TREESUM_AKTEVENT (
    BELOW_EVENT Varchar(50),
    AKTUSER Varchar(20),
    SEARCHID Varchar(20),
    VON Timestamp,
    BIS Timestamp )
AS
BEGIN SUSPEND; END^
SET TERM ; ^

/******************** TABLES **********************/

CREATE TABLE MANUAL_LOG
(
  REPLNO Integer NOT NULL,
  PUBDBNO Integer NOT NULL,
  SUBDBNO Integer NOT NULL,
  SEQNO Integer NOT NULL,
  RELATIONNO Integer NOT NULL,
  REPTYPE Char(1),
  OLDKEY Varchar(256),
  NEWKEY Varchar(256),
  SEP Char(1),
  FORCECOUNT Integer DEFAULT 0,
  ERROR_CODE Integer,
  ERROR_MSG Varchar(256),
  PRIMARY KEY (REPLNO,PUBDBNO,SUBDBNO,SEQNO)
);
CREATE TABLE REPL_LOG
(
  REPLNO Integer NOT NULL,
  PUBDBNO Integer NOT NULL,
  SUBDBNO Integer NOT NULL,
  SEQNO Integer NOT NULL,
  RELATIONNO Integer NOT NULL,
  REPTYPE Char(1),
  OLDKEY Varchar(256),
  NEWKEY Varchar(256),
  SEP Char(1),
  FORCECOUNT Integer DEFAULT 0,
  PRIMARY KEY (REPLNO,PUBDBNO,SUBDBNO,SEQNO)
);
CREATE TABLE REPL_SEPARATOR
(
  REPLNO Integer NOT NULL,
  PUBDBNO Integer NOT NULL,
  SUBDBNO Integer NOT NULL,
  RELATIONNO Integer NOT NULL,
  RELATIONNAME Varchar(100) NOT NULL,
  SEP Char(1),
  PRIMARY KEY (REPLNO,PUBDBNO,SUBDBNO,RELATIONNO)
);
CREATE TABLE UIBACCOUNT
(
  PKUSERID Varchar(20) NOT NULL,
  PKINITTIME Timestamp NOT NULL,
  USERID Varchar(20) NOT NULL,
  STARTTIME Timestamp NOT NULL,
  "EVENT" Varchar(50) NOT NULL,
  STOPTIME Timestamp NOT NULL,
  STUNDEN D_FLOAT,
  CONSTRAINT PK_UIBACCOUNT PRIMARY KEY (PKUSERID,PKINITTIME)
);
CREATE TABLE UIBACCOUNTEXPORT
(
  ACC_EXP_ID Integer NOT NULL,
  "EVENT" Varchar(50) NOT NULL,
  FROMDAY Timestamp NOT NULL,
  UNTILDAY Timestamp NOT NULL,
  STUNDEN D_FLOAT,
  INERP "BOOLEAN" DEFAULT 0 NOT NULL,
  ERPERROR "BOOLEAN" DEFAULT 0 NOT NULL,
  ERP_ERRORSTR Varchar(255),
  CONSTRAINT PK_UIBACCEXP PRIMARY KEY (ACC_EXP_ID),
  CONSTRAINT UK_UIBACCEXP UNIQUE ("EVENT",FROMDAY)
);
CREATE TABLE UIBAKTEVENT
(
  "EVENT" Varchar(50) NOT NULL,
  PARENTEVENT Varchar(50) NOT NULL,
  "COMMENT" Varchar(500),
  ID_STRING Varchar(50),
  BASE_FOR_TIME Varchar(50),
  TIME_H Float,
  PROJECTSTART Timestamp,
  REPORTREQUIRED "BOOLEAN" DEFAULT 0 NOT NULL,
  ACC_PER_MONTHNUM Float,
  ACCOUNTINGREQUIRED "BOOLEAN" DEFAULT 0 NOT NULL,
  KD_STRING Varchar(50),
  CONSTRAINT PK_AKTEVENT_EVENT PRIMARY KEY ("EVENT")
);
CREATE TABLE UIBAKTUSER
(
  USERID Varchar(20) NOT NULL,
  NAME Varchar(50) NOT NULL,
  UIBMEMBER "BOOLEAN" DEFAULT 0,
  EMAIL Varchar(50),
  MYPASSWORD Varchar(50),
  CONSTRAINT PK_AKTUSER_USERID PRIMARY KEY (USERID)
);
CREATE TABLE UIBALLEVENT
(
  "EVENT" Varchar(50) NOT NULL,
  PARENTEVENT Varchar(50) NOT NULL,
  "COMMENT" Varchar(500),
  ID_STRING Varchar(50),
  BASE_FOR_TIME Varchar(50),
  TIME_H Float,
  PROJECTSTART Timestamp,
  REPORTREQUIRED "BOOLEAN" DEFAULT 0 NOT NULL,
  ACC_PER_MONTHNUM Float,
  ACCOUNTINGREQUIRED "BOOLEAN" DEFAULT 0 NOT NULL,
  KD_STRING Varchar(50),
  CONSTRAINT PK_ALLEVENT_EVENT PRIMARY KEY ("EVENT")
);
CREATE TABLE UIBALLUSER
(
  USERID Varchar(20) NOT NULL,
  NAME Varchar(50) NOT NULL,
  UIBMEMBER "BOOLEAN" DEFAULT 0,
  EMAIL Varchar(50),
  MYPASSWORD Varchar(50),
  CONSTRAINT PK_ALLUSER_USERID PRIMARY KEY (USERID)
);
CREATE TABLE UIBCALLS
(
  USERID Varchar(20) NOT NULL,
  DAY_H Timestamp NOT NULL,
  CALLS Integer NOT NULL,
  CONSTRAINT PK_UIBCALLS PRIMARY KEY (USERID,DAY_H)
);
CREATE TABLE UIBDEFPROJ
(
  USERID Varchar(20) NOT NULL,
  "EVENT" Varchar(50) NOT NULL,
  CONSTRAINT PK_DEFPROJ_USERID PRIMARY KEY (USERID)
);
CREATE TABLE UIBEVENT
(
  PKUSERID Varchar(20) NOT NULL,
  PKINITTIME Timestamp NOT NULL,
  USERID Varchar(20) NOT NULL,
  STARTTIME Timestamp NOT NULL,
  "EVENT" Varchar(50) NOT NULL,
  STOPTIME Timestamp NOT NULL,
  STUNDEN D_FLOAT,
  CONSTRAINT PK_UIBEVENT PRIMARY KEY (PKUSERID,PKINITTIME)
);
CREATE TABLE UIBEVENTACCOUNTREPORT
(
  ACC_REP_ID Integer NOT NULL,
  USERID Varchar(20) NOT NULL,
  DATEDAY Timestamp NOT NULL,
  "EVENT" Varchar(50) NOT NULL,
  STUNDEN D_FLOAT,
  DESCRIPTION Varchar(255),
  CONSTRAINT PK_UIBEVENTAC PRIMARY KEY (ACC_REP_ID)
);
CREATE TABLE UIBLOGGEDIN
(
  USERID Varchar(20) NOT NULL,
  LOGGEDIN Timestamp NOT NULL,
  CONSTRAINT PK_LOGGEDIN_USERID PRIMARY KEY (USERID)
);
CREATE TABLE UIBMONTHEVENT
(
  USERID Varchar(20) NOT NULL,
  JAHR Integer NOT NULL,
  MONAT Integer NOT NULL,
  "EVENT" Varchar(50) NOT NULL,
  STUNDEN D_FLOAT,
  CONSTRAINT PK_UIBMONTHEVENT PRIMARY KEY (USERID,JAHR,MONAT,"EVENT")
);
CREATE TABLE UIBSOLL
(
  USERID Varchar(20) NOT NULL,
  JAHR Integer NOT NULL,
  MONAT Integer NOT NULL,
  STUNDEN D_FLOAT NOT NULL,
  CONSTRAINT PK_UIBSOLL PRIMARY KEY (USERID,JAHR,MONAT)
);
CREATE TABLE UIBTIMEOUT
(
  USERID Varchar(20) NOT NULL,
  "EVENT" Varchar(50) NOT NULL,
  TIMEOUT_MIN Integer,
  CONSTRAINT PK_UIBTIMEOUT PRIMARY KEY (USERID,"EVENT")
);
CREATE TABLE UIBTMPTREESUM
(
  USERID Varchar(20) NOT NULL,
  "EVENT" Varchar(50) NOT NULL,
  ID_STRING Varchar(50),
  STUNDEN D_FLOAT,
  CONSTRAINT PK_UIBTMPTREESUM PRIMARY KEY (USERID,"EVENT")
);
CREATE TABLE UIBTMPTREESUM2
(
  USERID Varchar(20) NOT NULL,
  "EVENT" Varchar(50) NOT NULL,
  ID_STRING Varchar(50),
  UIBUSER Varchar(20) NOT NULL,
  STUNDEN D_FLOAT,
  CONSTRAINT PK_UIBTMPTREESUM2 PRIMARY KEY (USERID,"EVENT",UIBUSER)
);
CREATE TABLE UIBTMP_SAPEVENT
(
  "EVENT" Varchar(50) NOT NULL,
  SAPEVENT Varchar(50) NOT NULL,
  ID_STRING Varchar(50) NOT NULL,
  CONSTRAINT PK_UIBTMP_SAPEVENT PRIMARY KEY ("EVENT",SAPEVENT,ID_STRING)
);
CREATE TABLE UIBTVERSION
(
  MAJOR Varchar(20) NOT NULL,
  MINOR Varchar(50) NOT NULL,
  DATEDAY Timestamp NOT NULL
);
CREATE TABLE UIBUSEREVENT
(
  USERID Varchar(20) NOT NULL,
  "EVENT" Varchar(50) NOT NULL,
  BUTTON "BOOLEAN" DEFAULT 0,
  CONSTRAINT PK_USEREVENT PRIMARY KEY (USERID,"EVENT")
);
CREATE TABLE UIB_WORK_DESCRIPTION
(
  USERID Varchar(20) NOT NULL,
  JAHR Integer NOT NULL,
  MONAT Integer NOT NULL,
  TAG Integer NOT NULL,
  "EVENT" Varchar(50) NOT NULL,
  DESCRIPTION Varchar(255),
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
CREATE VIEW UIBEVENTMONTHTIMESPLIT (USERID, JAHR, MONAT, "EVENT", STUNDEN)
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
CREATE VIEW UIBEVENTTIMESPLIT (USERID, STARTTIME, STOPTIME, MINUTE_, STUNDE, TAG, MONAT, JAHR, WOCHENTAG, JAHRESTAG, "EVENT", STUNDEN)
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
BEFORE INSERT POSITION 0
check ((parentevent like 'root') or (parentevent in (select event from uiballevent)))^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER CHECK_10 FOR UIBAKTUSER ACTIVE
AFTER DELETE POSITION 1
^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER CHECK_11 FOR UIBAKTUSER ACTIVE
AFTER DELETE POSITION 1
^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER CHECK_12 FOR UIBAKTEVENT ACTIVE
AFTER DELETE POSITION 1
^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER CHECK_13 FOR UIBAKTUSER ACTIVE
AFTER DELETE POSITION 1
^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER CHECK_2 FOR UIBALLEVENT ACTIVE
BEFORE UPDATE POSITION 0
check ((parentevent like 'root') or (parentevent in (select event from uiballevent)))^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER CHECK_3 FOR UIBAKTEVENT ACTIVE
AFTER UPDATE POSITION 1
^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER CHECK_4 FOR UIBAKTEVENT ACTIVE
BEFORE INSERT POSITION 0
check ((parentevent like 'root') or (parentevent in (select event from uibaktevent)))^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER CHECK_5 FOR UIBAKTEVENT ACTIVE
BEFORE UPDATE POSITION 0
check ((parentevent like 'root') or (parentevent in (select event from uibaktevent)))^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER CHECK_6 FOR UIBAKTUSER ACTIVE
AFTER DELETE POSITION 1
^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER CHECK_7 FOR UIBAKTEVENT ACTIVE
AFTER DELETE POSITION 1
^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER CHECK_8 FOR UIBAKTUSER ACTIVE
AFTER DELETE POSITION 1
^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER CHECK_9 FOR UIBAKTEVENT ACTIVE
AFTER DELETE POSITION 1
^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_10_D_2_2 FOR UIBSOLL INACTIVE
AFTER DELETE POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,OLDKEY)
  VALUES (2,2,1,10,'D',OLD.JAHR||''||OLD.MONAT||''||OLD.USERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_10_I_2_2 FOR UIBSOLL INACTIVE
AFTER INSERT POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,NEWKEY)
  VALUES (2,2,1,10,'I',NEW.JAHR||''||NEW.MONAT||''||NEW.USERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_10_U_2_2 FOR UIBSOLL INACTIVE
AFTER UPDATE POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,OLDKEY,NEWKEY)
  VALUES (2,2,1,10,'U',OLD.JAHR||''||OLD.MONAT||''||OLD.USERID,NEW.JAHR||''||NEW.MONAT||''||NEW.USERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_1_D_2_2 FOR UIBAKTEVENT INACTIVE
AFTER DELETE POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,OLDKEY)
  VALUES (2,2,1,1,'D',OLD.EVENT);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_1_I_2_2 FOR UIBAKTEVENT INACTIVE
AFTER INSERT POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,NEWKEY)
  VALUES (2,2,1,1,'I',NEW.EVENT);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_1_U_2_2 FOR UIBAKTEVENT INACTIVE
AFTER UPDATE POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,OLDKEY,NEWKEY)
  VALUES (2,2,1,1,'U',OLD.EVENT,NEW.EVENT);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_27_D_2_2 FOR UIBLOGGEDIN INACTIVE
AFTER DELETE POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,OLDKEY)
  VALUES (2,2,1,27,'D',OLD.USERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_27_I_2_2 FOR UIBLOGGEDIN INACTIVE
AFTER INSERT POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,NEWKEY)
  VALUES (2,2,1,27,'I',NEW.USERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_27_U_2_2 FOR UIBLOGGEDIN INACTIVE
AFTER UPDATE POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,OLDKEY,NEWKEY)
  VALUES (2,2,1,27,'U',OLD.USERID,NEW.USERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_29_D_2_2 FOR UIB_WORK_DESCRIPTION INACTIVE
AFTER DELETE POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,OLDKEY)
  VALUES (2,2,1,29,'D',OLD.EVENT||''||OLD.JAHR||''||OLD.MONAT||''||OLD.TAG||''||OLD.USERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_29_I_2_2 FOR UIB_WORK_DESCRIPTION INACTIVE
AFTER INSERT POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,NEWKEY)
  VALUES (2,2,1,29,'I',NEW.EVENT||''||NEW.JAHR||''||NEW.MONAT||''||NEW.TAG||''||NEW.USERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_29_U_2_2 FOR UIB_WORK_DESCRIPTION INACTIVE
AFTER UPDATE POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,OLDKEY,NEWKEY)
  VALUES (2,2,1,29,'U',OLD.EVENT||''||OLD.JAHR||''||OLD.MONAT||''||OLD.TAG||''||OLD.USERID,NEW.EVENT||''||NEW.JAHR||''||NEW.MONAT||''||NEW.TAG||''||NEW.USERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_2_D_2_2 FOR UIBAKTUSER INACTIVE
AFTER DELETE POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,OLDKEY)
  VALUES (2,2,1,2,'D',OLD.USERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_2_I_2_2 FOR UIBAKTUSER INACTIVE
AFTER INSERT POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,NEWKEY)
  VALUES (2,2,1,2,'I',NEW.USERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_2_U_2_2 FOR UIBAKTUSER INACTIVE
AFTER UPDATE POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,OLDKEY,NEWKEY)
  VALUES (2,2,1,2,'U',OLD.USERID,NEW.USERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_3_D_2_2 FOR UIBALLEVENT INACTIVE
AFTER DELETE POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,OLDKEY)
  VALUES (2,2,1,3,'D',OLD.EVENT);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_3_I_2_2 FOR UIBALLEVENT INACTIVE
AFTER INSERT POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,NEWKEY)
  VALUES (2,2,1,3,'I',NEW.EVENT);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_3_U_2_2 FOR UIBALLEVENT INACTIVE
AFTER UPDATE POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,OLDKEY,NEWKEY)
  VALUES (2,2,1,3,'U',OLD.EVENT,NEW.EVENT);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_4_D_2_2 FOR UIBALLUSER INACTIVE
AFTER DELETE POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,OLDKEY)
  VALUES (2,2,1,4,'D',OLD.USERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_4_I_2_2 FOR UIBALLUSER INACTIVE
AFTER INSERT POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,NEWKEY)
  VALUES (2,2,1,4,'I',NEW.USERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_4_U_2_2 FOR UIBALLUSER INACTIVE
AFTER UPDATE POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,OLDKEY,NEWKEY)
  VALUES (2,2,1,4,'U',OLD.USERID,NEW.USERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_5_D_2_2 FOR UIBCALLS INACTIVE
AFTER DELETE POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,OLDKEY)
  VALUES (2,2,1,5,'D',OLD.DAY_H||''||OLD.USERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_5_I_2_2 FOR UIBCALLS INACTIVE
AFTER INSERT POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,NEWKEY)
  VALUES (2,2,1,5,'I',NEW.DAY_H||''||NEW.USERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_5_U_2_2 FOR UIBCALLS INACTIVE
AFTER UPDATE POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,OLDKEY,NEWKEY)
  VALUES (2,2,1,5,'U',OLD.DAY_H||''||OLD.USERID,NEW.DAY_H||''||NEW.USERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_7_D_2_2 FOR UIBEVENT INACTIVE
AFTER DELETE POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,OLDKEY)
  VALUES (2,2,1,7,'D',OLD.PKINITTIME||''||OLD.PKUSERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_7_I_2_2 FOR UIBEVENT INACTIVE
AFTER INSERT POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,NEWKEY)
  VALUES (2,2,1,7,'I',NEW.PKINITTIME||''||NEW.PKUSERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL$1_7_U_2_2 FOR UIBEVENT INACTIVE
AFTER UPDATE POSITION 32760
AS
BEGIN
IF (USER <> 'REPL') THEN BEGIN
  INSERT INTO REPL_LOG(REPLNO,PUBDBNO,SUBDBNO,RELATIONNO,REPTYPE,OLDKEY,NEWKEY)
  VALUES (2,2,1,7,'U',OLD.PKINITTIME||''||OLD.PKUSERID,NEW.PKINITTIME||''||NEW.PKUSERID);END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER REPL_GEN_SEQNO FOR REPL_LOG ACTIVE
BEFORE INSERT POSITION 0
AS BEGIN NEW.SEQNO = GEN_ID(REPL_GENERATOR,1); END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER TR_UIBACCOUNTEXPORT_IN FOR UIBACCOUNTEXPORT ACTIVE
BEFORE INSERT POSITION 0
as
         begin
           new.acc_exp_id = NEXT VALUE FOR uibaccountexport_SEQU;
         end^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER TR_UIBACCOUNTREPORT_IN FOR UIBEVENTACCOUNTREPORT ACTIVE
BEFORE INSERT POSITION 0
as
         begin
           new.acc_rep_id = NEXT VALUE FOR uibaccountreport_SEQU;
         end^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER TR_UIBACCOUNT_IN FOR UIBACCOUNT ACTIVE
BEFORE INSERT POSITION 0
as
         begin
          new.stunden = (new.stoptime - new.starttime) * 24;
         end^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER TR_UIBACCOUNT_UP FOR UIBACCOUNT ACTIVE
BEFORE UPDATE POSITION 0
as
         begin
          new.stunden = (new.stoptime - new.starttime) * 24;
         end^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER TR_UIBAKTEVENT2UIBALL_IN FOR UIBAKTEVENT ACTIVE
BEFORE INSERT POSITION 0
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
                 base_for_time = new.base_for_time,
                 time_h = new.time_h,
                 projectstart = new.projectstart,
                 reportrequired = new.reportrequired,
                 accountingrequired = new.accountingrequired,
                 acc_per_monthnum = new.acc_per_monthnum
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
                base_for_time,
                time_h,
                projectstart,
                reportrequired,
                accountingrequired,
                acc_per_monthnum)
             values
               (new.event,
                new.parentevent,
                new.comment,
                new.id_string,
                new.kd_string,
                new.base_for_time,
                new.time_h,
                new.projectstart,
                new.reportrequired,
                new.accountingrequired,
                new.acc_per_monthnum);
          end
         end^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER TR_UIBAKTEVENT2UIBALL_UP FOR UIBAKTEVENT ACTIVE
BEFORE UPDATE POSITION 0
as
         begin
          update uiballevent
            set parentevent = new.parentevent,
                 comment    = new.comment,
                 id_string = new.id_string,
                 kd_string = new.kd_string,
                 base_for_time = new.base_for_time,
                 time_h = new.time_h,
                 projectstart = new.projectstart,
                 reportrequired = new.reportrequired,
                 accountingrequired = new.accountingrequired,
                 acc_per_monthnum = new.acc_per_monthnum
           where (event = old.event);
         end^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER TR_UIBAKTUSER2UIBALL_IN FOR UIBAKTUSER ACTIVE
BEFORE INSERT POSITION 0
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
                mypassword = new.mypassword
              where (userid = new.userid);
          end
          else
          begin
           insert into uiballuser
                   (userid,
                    name,
                    uibmember,
                    email,
                    mypassword)
                 values
                   (new.userid,
                    new.name,
                    new.uibmember,
                    new.email,
                    new.mypassword);
          end
         end^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER TR_UIBAKTUSER2UIBALL_UP FOR UIBAKTUSER ACTIVE
BEFORE UPDATE POSITION 0
as
         begin
          update uiballuser
            set name = new.name,
                uibmember = new.uibmember,
                email = new.email,
                mypassword = new.mypassword
              where (userid = old.userid);
         end^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER TR_UIBEVENT_IN FOR UIBEVENT ACTIVE
BEFORE INSERT POSITION 0
as
         begin
          new.stunden = (new.stoptime - new.starttime) * 24;
         end^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER TR_UIBEVENT_UP FOR UIBEVENT ACTIVE
BEFORE UPDATE POSITION 0
as
         begin
          new.stunden = (new.stoptime - new.starttime) * 24;
         end^
SET TERM ; ^

SET TERM ^ ;
ALTER PROCEDURE BUILDBSZ2_TMP_MONTHREP (
    AKTUSER Varchar(20),
    SEARCHID Varchar(20),
    VON Timestamp,
    BIS Timestamp )
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
    AKTUSER Varchar(20),
    SEARCHID Varchar(20),
    VON Timestamp,
    BIS Timestamp )
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
    AKTUSER Varchar(20),
    SEARCHID Varchar(20),
    VON Timestamp,
    BIS Timestamp )
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
    AKTUSER Varchar(20),
    SEARCHID Varchar(20),
    VON Timestamp,
    BIS Timestamp )
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
    BELOW_EVENT Varchar(50),
    AKTUSER Varchar(20),
    SEARCHID Varchar(20),
    VON Timestamp,
    BIS Timestamp )
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
    BELOW_EVENT Varchar(50),
    AKTUSER Varchar(20),
    SEARCHID Varchar(20),
    VON Timestamp,
    BIS Timestamp )
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
 ON MANUAL_LOG TO  REPL;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON MANUAL_LOG TO  SYSDBA WITH GRANT OPTION;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON MANUAL_LOG TO  UIBTIME;

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
 ON UIBAKTEVENT TO  UIBTIMEREAD;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBAKTEVENT TO  UIBTIME_ERP;

GRANT DELETE, INSERT, REFERENCES, SELECT, SELECT, UPDATE
 ON UIBAKTUSER TO  REPL;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBAKTUSER TO  UIBTIME WITH GRANT OPTION;

GRANT SELECT
 ON UIBAKTUSER TO  UIBTIMEREAD;

GRANT DELETE, INSERT, REFERENCES, SELECT, SELECT, UPDATE
 ON UIBALLEVENT TO  REPL;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBALLEVENT TO  UIBTIME WITH GRANT OPTION;

GRANT SELECT
 ON UIBALLEVENT TO  UIBTIMEREAD;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBALLEVENT TO  UIBTIME_ERP;

GRANT DELETE, INSERT, REFERENCES, SELECT, SELECT, UPDATE
 ON UIBALLUSER TO  REPL;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBALLUSER TO  UIBTIME WITH GRANT OPTION;

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
 ON UIBDEFPROJ TO  UIBTIMEREAD;

GRANT DELETE, INSERT, REFERENCES, SELECT, SELECT, UPDATE
 ON UIBEVENT TO  REPL;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBEVENT TO  UIBTIME WITH GRANT OPTION;

GRANT SELECT
 ON UIBEVENT TO  UIBTIMEREAD;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBEVENTACCOUNTREPORT TO  UIBTIME WITH GRANT OPTION;

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


