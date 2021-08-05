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
 FOR select event, id_string from uibaktevent
     where parentevent = :below_event
     into :aktevent, :aktid
 DO
 BEGIN
  if (:aktevent <> 'root') then
  begin
   insert into uibtmptreesum Values (:aktuser, :aktevent, :aktid,
    (select sum(stunden) from uibevent where
    (event in (select event from uibaktevent
    where (parentevent like :aktevent)
	or
    (event = :aktevent)))
    and (starttime >= :von)
    and (stoptime < :bis)
    and (userid like :searchid)));
  end
 END
END^
SET TERM ; ^
