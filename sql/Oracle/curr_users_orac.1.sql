select s.username "User", s.osuser "OS User",
       s.sid "Sid",           
       decode(s.type, 'USER', 'User', 'BACKGROUND', 'Backgd', s.type) "Type",
       decode(s.status,'INACTIVE','Inact ' || 
                                  round((s.last_call_et/60),0) || 
                                  ' min', 
                       'ACTIVE', 'Active',
              s.status) "Status",     
       to_char(s.logon_time,'dd/mm hh24:mi') "Logged On",
       p.spid "Spid",
       s.program "Program", s.module "Module",
       s.server "Server", s.machine "Machine",   s.terminal "Terminal",
       decode(s.command, 0,'',                 1,'Create Table',
                         2,'Insert',           3,'Select',
                         4,'Create Cluster',   5,'Alter Cluster',
                         6,'Update',           7,'Delete',
                         8,'Drop',             9,'Create Index',
                         10,'Drop Index',      11,'Alter Index',
                         12,'Drop Table',      15,'Alter Table',
                         17,'Grant',           18,'Revoke',
                         19,'Create Synonym',  20,'Drop Synonym',
                         21,'Create View',     22,'Drop View',
                         26,'Lock Table',
                         28,'Rename',          29,'Comment',
                         30,'Audit',           31,'Noaudit',
                         32,'Cre Ext Data',    33,'Drop Ext Dat',
                         34,'Create Data',     35,'Alter Data',
                         36,'Create Rollback Segment',
                         37,'Alter Rollback Segment',
                         38,'Drop Rollback Segment',
                         39,'Create Tablespace',
                         40,'Alter Tablespace',
                         41,'Drop Tablespace',
                         42,'Alter Session',   43,'Alter User',
                         44,'Commit',          45,'Rollback',
                         46,'Save Point',      47,'PL/SQL',
                         to_char(command))     "Command Type",
       decode(s.lockwait,'','','Yes') "Lock Wait?"
from   v$session s, v$process p
where  s.paddr = p.addr
order by 1, 2, 3, 4, 5