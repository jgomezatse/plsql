
  CREATE OR REPLACE FORCE VIEW "ALBAADM"."VPOLICIAS" ("USUID", "LOGIN", "INDICATIVO", "AGENTE", "UNIDAD", "NOMBRE", "EMAIL", "ACTIVO", "R_BASICO", "R_ADMON", "R_LISTABOL", "PL_AMPLIADO", "R_PENDIENTES") AS 
  select "USUID","LOGIN","INDICATIVO","AGENTE","UNIDAD","NOMBRE","EMAIL","ACTIVO","R_BASICO","R_ADMON","R_LISTABOL","PL_AMPLIADO","R_PENDIENTES" from (
select 
u.usuid,
u.usulogin login,
decode(u.usuusuregistro,null,'',u.usuusuregistro) indicativo,
(select ag.agenumero
from albaadm.agentes ag
where u.usuid=ag.usuid) agente,
(select ag.ageunidad
from albaadm.agentes ag
where u.usuid=ag.usuid) unidad,
u.usunombre nombre,
u.USUEMAIL email,
u.usuactivo activo,
(select decode(count(*),0,'','S')
from albaadm.perfilesusuarios pu
where pu.usuid=u.usuid
and pu.prfid=2410) R_BASICO,
(select decode(count(*),0,'','S')
from albaadm.perfilesusuarios pu
where pu.usuid=u.usuid
and pu.prfid=2409) R_ADMON,
(select decode(count(*),0,'','S')
from albaadm.perfilesusuarios pu
where pu.usuid=u.usuid
and pu.prfid=2525) R_LISTABOL,
(select decode(count(*),0,'','S')
from albaadm.perfilesusuarios pu
where pu.usuid=u.usuid
and pu.prfid=1165) PL_AMPLIADO,
r.total R_pendientes
from albaadm.usuarios u,
(
  select a1.usuid, count(*) total
  from albaadm.escritosmultas e1,
  albaadm.multas m1,
  albaadm.agentes a1
  where e1.mulid=m1.mulid
  and m1.age1_ageid=a1.ageid
  and e1.tidomid=6
  and e1.tdmresid=28
  and e1.esmid>1697352
  and a1.usuid is not null
  group by a1.usuid
) r
where u.usuemail like '%.pl%'
and u.usuid=r.usuid(+)
union

  select a1.usuid, 
  null login, null indicativo,a1.agenumero,a1.ageunidad,null nombre,null email,null activo,null r_basico,null r_admon,null r_listabol,null pl_ampliado,
  count(*) r_pendientes
  from albaadm.escritosmultas e1,
  albaadm.multas m1,
  albaadm.agentes a1
  where e1.mulid=m1.mulid
  and m1.age1_ageid=a1.ageid
  and e1.tidomid=6
  and e1.tdmresid=28
  and e1.esmid>1697352  
  and a1.usuid is null
  group by a1.usuid,
  null , null ,a1.agenumero,a1.ageunidad,null ,null ,null ,null ,null ,null ,null
)
order by agente asc;