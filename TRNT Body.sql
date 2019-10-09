create or replace PACKAGE BODY TRNT is

function crmRes(vCrmid number) return varchar2 is

cursor cResultados(vCrmid number) is
select upper(substr(resultado,1,3)) || '=' || trim(to_char(total,'999g999')) resultado
from (
SELECT rn.des || ' ' || n.ntfresultado Resultado,
count(n.ntfid) total
FROM alba.decodificadora d1,
alba.personas p1,
alba.actosnotificacion a,
alba.cabremesas c,
alba.remesas r,
(SELECT DISTINCT rtnresultado res, rtndescripcion des FROM alba.resultadosnotif) rn,
alba.notificaciones n
WHERE n.ntfid=r.ntfid
AND r.crmid=c.crmid
AND c.atoid=a.atoid
AND n.ntfsicergrupo IS NULL
AND n.ntfresultado=rn.res(+)
AND n.perid= p1.perid
AND n.perversion= p1.perversion
AND d1.deccodigotabla=(SELECT decid FROM alba.decodificadora WHERE decelemento='TIPODIREC')
AND n.ntfxtipodireccion=d1.decid
AND n.ntfcrmid=r.crmid
AND c.crmid=vCrmid
and n.ntfresultado is not null
group by rn.des || ' ' || n.ntfresultado
);

vSalida  varchar2(1000):='';
i number:=0;

begin

    vSalida:='';
    i:=0;

    for r in cResultados(vCrmid) loop
      if i=0 then
        vSalida:=r.resultado;
      else
        vSalida:=vSalida || '. ' || r.resultado;
      end if;
      i:=i+1;
    end loop;

    return vSalida;

end;


end trnt;