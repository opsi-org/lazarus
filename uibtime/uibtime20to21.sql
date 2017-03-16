
/* tabelle nur alle user */
alter table uiballuser ADD email varchar(50);
alter table uiballuser ADD mypassword varchar(50);

/* tabelle nur aktuelle user */
alter table uibaktuser ADD email varchar(50);
alter table uibaktuser ADD mypassword varchar(50);

/*   Hintergrundtabelle alle Projekte und sub projekte */
alter table uiballevent ADD reportrequired	boolean not null;
alter table uiballevent ADD acc_per_monthnum	float;
alter table uiballevent ADD accountingrequired	boolean not null;
alter table uiballevent ADD KD_STRING Varchar(50);
alter table UIBALLEVENT DROP BASE_FOR_TIME;
alter table uiballevent ADD MATERIAL Varchar(50);
alter table uiballevent ADD MATTYPE Varchar(50);
alter table uiballevent ADD time_h_is_quota	boolean not null;
alter table uiballevent ADD UIB3MONTHINVOICE	boolean not null;

/* tabelle nur aktuelle Projekte und sub projekte */
alter table uibaktevent ADD reportrequired	boolean not null;
alter table uibaktevent ADD acc_per_monthnum	float;
alter table uibaktevent ADD accountingrequired	boolean not null;
alter table uibaktevent ADD KD_STRING Varchar(50);
alter table UIBAKTEVENT DROP BASE_FOR_TIME;
alter table uibaktevent ADD MATERIAL Varchar(50);
alter table uibaktevent ADD MATTYPE Varchar(50);
alter table uibaktevent ADD time_h_is_quota	boolean not null;
alter table uibaktevent ADD UIB3MONTHINVOICE	boolean not null;


create table caniasimporterror
   (canimp_rep_id       Integer NOT NULL,
    errmesg  varchar (255),
    created timestamp,
    CONSTRAINT PK_canimperr     PRIMARY KEY (canimp_rep_id));

create table uibeventaccountreport
   (acc_rep_id       Integer NOT NULL,
    userid    varchar(20) not null,
    dateday  date not null,
    event       varchar(50) not null,
    stunden     d_float,
    description  varchar (255),
    locked       boolean not null,
    CONSTRAINT PK_uibeventac     PRIMARY KEY (acc_rep_id),
    CONSTRAINT FK_uibeventac_userid Foreign key (userid)
                   references uiballuser(userid),
    CONSTRAINT FK_uibeventac_event Foreign key (event)
                   references uiballevent(event));

create table uibevrtemp
   (userid    varchar(20) not null,
    dateday  date not null,
    event       varchar(50) not null,
    stunden     d_float,
    description  varchar (255));
                   
CREATE SEQUENCE uibaccountexport_SEQU;
CREATE SEQUENCE uibaccountreport_SEQU;
CREATE SEQUENCE caniasimporterror_SEQU;

create table uibaccountexport
   (acc_exp_id       Integer NOT NULL,
    event       varchar(50) not null,
    fromday  date not null,
    untilday  date not null,
    stunden     d_float,
    inerp       boolean not null,
    erperror       boolean not null,
    erp_errorstr   varchar(255),
    CONSTRAINT PK_uibaccexp     PRIMARY KEY (acc_exp_id),
    CONSTRAINT UK_uibaccexp     UNIQUE (event,fromday),
    CONSTRAINT FK_uibaccexp_event Foreign key (event)
                   references uiballevent(event));


                   
create table uibtversion
   (major    varchar(20) not null,
    minor       varchar(50) not null,
    dateday  date not null);

create table uibtwoche
   (daynum    Integer not null,
    daystr    varchar(2) not null,
    CONSTRAINT PK_uibtwoche     PRIMARY KEY (daynum));
    
    
update uiballevent set reportrequired = 1
where (not (base_for_time = "")) and time_h is not null;

update uiballevent set accountingrequired = 1
where reportrequired = 1;


update uibaktevent set reportrequired = 1
where (not (base_for_time = "")) and time_h is not null;

update uibaktevent set accountingrequired = 1
where reportrequired = 1;


update uibaktevent set acc_per_monthnum = 1
where (base_for_time = "month");

update uibaktevent set acc_per_monthnum = 2
where (base_for_time = "2month");

update uibaktevent set acc_per_monthnum = 3
where (base_for_time = "3month");

update uibaktevent set acc_per_monthnum = 4
where (base_for_time = "4month");

update uibaktevent set acc_per_monthnum = 6
where (base_for_time = "6month");

update uibaktevent set acc_per_monthnum = 12
where (base_for_time = "12month");

update uiballevent set acc_per_monthnum = 1
where (base_for_time = "month");

update uiballevent set acc_per_monthnum = 2
where (base_for_time = "2month");

update uiballevent set acc_per_monthnum = 3
where (base_for_time = "3month");

update uiballevent set acc_per_monthnum = 4
where (base_for_time = "4month");

update uiballevent set acc_per_monthnum = 6
where (base_for_time = "6month");

update uiballevent set acc_per_monthnum = 12
where (base_for_time = "12month");

/* Trigger */

set term !! ;
RECREATE TRIGGER  TR_caniasimporterror_in  for caniasimporterror
         before insert POSITION 0 as
         begin
           new.canimp_rep_id = NEXT VALUE FOR caniasimporterror_SEQU;
           new.created = current_timestamp;
         end !!
set term ; !!


set term !! ;
RECREATE TRIGGER  TR_uibaccountexport_in  for uibaccountexport
         before insert POSITION 0 as
         begin
           new.acc_exp_id = NEXT VALUE FOR uibaccountexport_SEQU;
         end !!
set term ; !!

set term !! ;
RECREATE TRIGGER  TR_uibaccountreport_in  for uibeventaccountreport
         before insert POSITION 0 as
         begin
           new.acc_rep_id = NEXT VALUE FOR uibaccountreport_SEQU;
         end !!
set term ; !!


/* update  */
set term !! ;
RECREATE TRIGGER  TR_uibaktevent2uiball_up  for uibaktevent
         before update as
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
                 UIB3MONTHINVOICE = new.UIB3MONTHINVOICE
           where (event = old.event);
         end !!
set term ; !!

/* insert */
set term !! ;
RECREATE TRIGGER  TR_uibaktevent2uiball_in  for uibaktevent
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
                 kd_string = new.kd_string,
                 time_h = new.time_h,
                 projectstart = new.projectstart,
                 reportrequired = new.reportrequired,
                 accountingrequired = new.accountingrequired,
                 acc_per_monthnum = new.acc_per_monthnum,
                 MATERIAL = new.MATERIAL,
                 MATTYPE = new.MATTYPE,
                 time_h_is_quota = new.time_h_is_quota,
                 UIB3MONTHINVOICE = new.UIB3MONTHINVOICE
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
                UIB3MONTHINVOICE)
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
                new.UIB3MONTHINVOICE);
          end
         end !!
set term ; !!


/* Trigger zum füttern von uiballuser */

set term !! ;
RECREATE TRIGGER  TR_uibaktuser2uiball_in  for uibaktuser
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
         end !!
set term ; !!

set term !! ;
RECREATE TRIGGER  TR_uibaktuser2uiball_up  for uibaktuser
         before update as
         begin
          update uiballuser
            set name = new.name,
                uibmember = new.uibmember,
                email = new.email,
                mypassword = new.mypassword
              where (userid = old.userid);
         end !!
set term ; !!

/* Grants */

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON uibeventaccountreport TO  UIBTIME WITH GRANT OPTION;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON uibaccountexport TO  UIBTIME WITH GRANT OPTION;
 
 GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON uibtwoche TO  UIBTIME WITH GRANT OPTION;

 GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON uibevrtemp TO  UIBTIME WITH GRANT OPTION;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON uibtversion TO  UIBTIME;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON caniasimporterror TO  UIBTIME;


GRANT SELECT
 ON uibeventaccountreport TO  UIBTIMEREAD;

GRANT SELECT
 ON uibaccountexport TO  UIBTIMEREAD;

GRANT SELECT
 ON uibtversion TO  UIBTIMEREAD;

GRANT SELECT
 ON caniasimporterror TO  UIBTIMEREAD;


 
GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBAKTEVENT TO  UIBTIME_ERP;
 
GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON UIBALLEVENT TO  UIBTIME_ERP;


GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON uibaccountexport TO  UIBTIME_ERP;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON caniasimporterror TO  UIBTIME_ERP;

