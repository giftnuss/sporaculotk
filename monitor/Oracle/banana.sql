SELECT greatest (
((TO_CHAR(sysdate,'J') * (60 * 60 * 24)) +
         TO_CHAR (sysdate, 'SSSSS')) -
((TO_CHAR(to_date(c.first_time,'MM/DD/YYYY HH24:MI:SS'),'J') * (60 * 60 * 24)) +
         TO_CHAR (to_date(c.first_time,'MM/DD/YYYY HH24:MI:SS'), 'SSSSS'))
,
 ((TO_CHAR(to_date(a.first_time,'MM/DD/YYYY HH24:MI:SS'),'J') * (60 * 60 * 24)) +
         TO_CHAR (to_date(a.first_time,'MM/DD/YYYY HH24:MI:SS'), 'SSSSS')) -
   ((TO_CHAR(to_date(b.first_time,'MM/DD/YYYY HH24:MI:SS'),'J') * (60 * 60 * 24)) +
         TO_CHAR (to_date(b.first_time,'MM/DD/YYYY HH24:MI:SS'), 'SSSSS')))
FROM   v$log a, v$log b, v$log c
WHERE  a.sequence# = ( SELECT MAX(d.sequence#)
                       FROM v$log d)
AND    b.sequence# = ( SELECT (MAX(e.sequence#) - 1)
                       FROM v$log e)
AND    c.sequence# = ( SELECT MAX(f.sequence#)
                       FROM v$log f)
and 0 < (select count(*) from v$version where banner like 'Oracle7%')
union
SELECT greatest (
((TO_CHAR(sysdate,'J') * (60 * 60 * 24)) +
         TO_CHAR (sysdate, 'SSSSS')) -
((TO_CHAR(c.first_time,'J') * (60 * 60 * 24)) +
         TO_CHAR (c.first_time, 'SSSSS'))
,
 ((TO_CHAR(a.first_time,'J') * (60 * 60 * 24)) +
         TO_CHAR (a.first_time, 'SSSSS')) -
   ((TO_CHAR(b.first_time,'J') * (60 * 60 * 24)) +
         TO_CHAR (b.first_time, 'SSSSS')))
FROM   v$log a, v$log b, v$log c
WHERE  a.sequence# = ( SELECT MAX(d.sequence#)
                       FROM v$log d)
AND    b.sequence# = ( SELECT (MAX(e.sequence#) - 1)
                       FROM v$log e)
AND    c.sequence# = ( SELECT MAX(f.sequence#)
                       FROM v$log f)
and 0 < (select count(*) from v$version where banner like 'Oracle8%')
