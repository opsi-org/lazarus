
/* tabelle nur alle user */
alter table uiballuser ADD h_per_day D_FLOAT DEFAULT 0;
alter table uiballuser ADD mo_is_work boolean DEFAULT 0 not null;
alter table uiballuser ADD di_is_work boolean DEFAULT 0 not null;
alter table uiballuser ADD mi_is_work boolean DEFAULT 0 not null;
alter table uiballuser ADD do_is_work boolean DEFAULT 0 not null;
alter table uiballuser ADD fr_is_work boolean DEFAULT 0 not null;

/* tabelle nur aktuelle user */
alter table uibaktuser ADD h_per_day D_FLOAT  DEFAULT 0;
alter table uibaktuser ADD mo_is_work boolean DEFAULT 0 not null;
alter table uibaktuser ADD di_is_work boolean DEFAULT 0 not null;
alter table uibaktuser ADD mi_is_work boolean DEFAULT 0 not null;
alter table uibaktuser ADD do_is_work boolean DEFAULT 0 not null;
alter table uibaktuser ADD fr_is_work boolean DEFAULT 0 not null;

/*   Hintergrundtabelle alle Projekte und sub projekte */
alter table uiballevent ADD quota_lifetime_month	float DEFAULT 0 not null;

/* tabelle nur aktuelle Projekte und sub projekte */
alter table uibaktevent ADD quota_lifetime_month	float DEFAULT 0 not null;

create table public_holidays
   (holydate  timestamp,  
    CONSTRAINT PK_holyday     PRIMARY KEY (holydate));


/* Trigger */


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
                 UIB3MONTHINVOICE = new.UIB3MONTHINVOICE,
                 quota_lifetime_month = new.quota_lifetime_month
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
                mypassword = new.mypassword,
                h_per_day = new.h_per_day,
                mo_is_work = new.mo_is_work ,
                di_is_work = new.di_is_work,
                mi_is_work = new.mi_is_work,
                do_is_work = new.do_is_work,
                fr_is_work = new.fr_is_work
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
                    fr_is_work)
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
                    new.fr_is_work);
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
                mypassword = new.mypassword,
                h_per_day = new.h_per_day,
                mo_is_work = new.mo_is_work ,
                di_is_work = new.di_is_work,
                mi_is_work = new.mi_is_work,
                do_is_work = new.do_is_work,
                fr_is_work = new.fr_is_work
              where (userid = old.userid);
         end !!
set term ; !!

/* Grants */
