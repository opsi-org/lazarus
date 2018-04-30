
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

grant all on uibaccount      	to uibtime;

GRANT  SELECT
 ON UIBACCOUNT TO UIBTIMEREAD;
