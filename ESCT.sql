CREATE OR REPLACE PACKAGE ESCT as

  type esc is record
  (
    esmid number, 
    mulid number,
    expid number,
    expidexp number,  
    boletin number,
    expediente varchar2(12),
    liquidacion varchar2(12), 
    liqxestado number,
    tidomid number, 
    tdmresid number,
    tipodocumento VARCHAR2(200), 
    estado varchar2(200), 
    fecha date, 
    fechaestado date, 
    mulfhoramod date, 
    esmfgrabacion date, 
    esmfhoramod date, 
    observaciones VARCHAR2(300), 
    usuario VARCHAR2(200),
    distver number,
    nombre varchar2(300), 
    nif VARCHAR2(30), 
    perid number, 
    perversion number, 
    rdiid number,  
    esmcaja number, 
    esmlote number, 
    esmindice number,
    decid number, 
    decelemento VARCHAR2(100), 
    decdescr VARCHAR2(200), 
    tpeid number, 
    tpercodigo varchar2(10), 
    tpedesc VARCHAR2(100),
    prioridad number, 
    color_html varchar2(200), 
    total number
  );
  type escl is table of esc;


  function buscar
  (
    pExpediente in varchar2:=null,  
    pBoletin in number:=null,  
    pTipoBoletin in number:=null,    
    pTidomid in number:=null,
    pTdmresid in number:=null,
    pFmoddesde in varchar2:=null,
    pFmodhasta in varchar2:=null,
    pFdocdesde in varchar2:=null, 
    pFdochasta in varchar2:=null,
    pUsuario in number:=null,
    pDni in varchar2:=null,
    pEsmid in number:=null,
    pCargaFicheros in number:=null,
    pRecibo in varchar2:=null,
    pCaja in varchar2:=null,
    pLote in varchar2:=null,
    pIndice in varchar2:=null
  ) 
  return escl pipelined;
    
end ESCT;
/


create or replace PACKAGE BODY ESCT is

vEsc esc;

procedure resetvEsc is
begin
  vEsc.esmid:=0;   vEsc.mulid:=0;  vEsc.mulfhoramod:=null;  vEsc.boletin:=0; 
  vEsc.expediente:='';  vEsc.tipodocumento:='';  vEsc.fecha:=null;  vEsc.observaciones:='';
  vEsc.usuario:='';  vEsc.estado:='';  vEsc.fechaestado:=null;  vEsc.tdmresid:=0; 
  vEsc.tidomid:=0;   vEsc.distver:=0;  vEsc.perid:=0;   vEsc.perversion:=0; 
  vEsc.rdiid:=0;   vEsc.esmcaja:=0;   vEsc.esmlote:=0;   vEsc.esmindice:=0;
  vEsc.esmfgrabacion:=null;  vEsc.esmfhoramod:=null;  vEsc.liquidacion:='';  vEsc.liqxestado:=0; 
  vEsc.expidexp:=0;  vEsc.nombre:='';  vEsc.nif:='';  vEsc.decid:=0;   vEsc.decelemento:='';
  vEsc.decdescr:='';  vEsc.tpeid:=0;   vEsc.tpercodigo:='';  vEsc.tpedesc:='';  vEsc.expid:=0; 
  vEsc.prioridad:=0;   vEsc.color_html:='';  vEsc.total:=0;  
end;

procedure leerEscrito(vEsmid number) is

cursor cEscritos is
select e.*, u.usunombre , t.tidomnombre, r.tdmnombre
from 
alba.usuarios u,
alba.tipos_doc_multas t,
alba.tipos_doc_multas_res r,
alba.escritosmultas e
where e.esmid=vEsmid
and e.usuidmod=u.usuid
and e.tidomid=t.tidomid
and e.tdmresid=r.tdmresid;

begin

  for e in cEscritos loop
    vEsc.esmid:=e.esmid;   
    vEsc.mulid:=e.mulid;  
    vEsc.mulfhoramod:=trunc(e.esmfhoramod);  
    vEsc.boletin:=e.esmnumbol; 
    vEsc.tipodocumento:=e.tidomnombre;  
    vEsc.fecha:=trunc(e.esmfescrito);  
    vEsc.observaciones:=e.esmobservaciones;
    vEsc.usuario:=e.usunombre;  
    vEsc.estado:=e.tdmnombre;  
    vEsc.fechaestado:=trunc(e.esmfgrabacion);  
    vEsc.tdmresid:=e.tdmresid; 
    vEsc.tidomid:=e.tidomid;   
    vEsc.esmcaja:=e.esmcaja;   
    vEsc.esmlote:=e.esmlote;   
    vEsc.esmindice:=e.esmindice;
    vEsc.esmfgrabacion:=trunc(e.esmfgrabacion);  
    vEsc.esmfhoramod:=trunc(e.esmfhoramod);
 end loop;

end;


procedure leerColor(vEsmid number) is

cursor cEscritos is
select e.esmid
from alba.escritosmultas e
where e.esmid=vEsmid
and exists
(
select 1 
from alba.escritosmultas e2
where e2.esmid<>e.esmid
and e2.esmcaja=e.esmcaja
and e2.esmlote=e.esmlote
and e2.esmindice=e.esmindice
);

begin

  for e in cEscritos loop
    --vEsc.color_html:='';  
    vEsc.total:=1;      
  end loop;

end;


procedure leerExpediente(vMulid number) is

/*cursor cEscritos is
select ex.EXPCOD expte, l.LIQNUMERORECLIQUIDACION recibo,
l.liqxestado, ex.expid,
--alba.dni_exp(ex.EXPID, 'SUJETOPASIVO') nif,
--alba.nombre_exp(ex.EXPID, 'SUJETOPASIVO') nombre
'' nif,
'' nombre
from 
alba.expingre ex,
alba.liquidaciones l,
alba.multas m
where m.mulid=vMulid
and m.EXPIDEXP=ex.expid
and l.expid=ex.EXPID
;*/

cursor cEscritos is
select m.expediente expte, m.liquidacion recibo,
0 liqxestado, m.expid, '' nif, '' nombre
from alba.multasestados m
where m.mulid=vMulid
;

begin

  for e in cEscritos loop

    vEsc.expediente:=e.expte;  
    vEsc.distver:=0;  
    vEsc.perid:=0;   
    vEsc.perversion:=0; 
    vEsc.rdiid:=0;   
    vEsc.liquidacion:=e.recibo;  
    vEsc.liqxestado:=e.liqxestado; 
    vEsc.expidexp:=e.expid;  
    vEsc.expid:=e.expid;    
    vEsc.nombre:=e.nombre;  
    vEsc.nif:=e.nif;  
    vEsc.decelemento:='SUJETO PASIVO';    
    vEsc.decid:=0;   
    vEsc.decdescr:='';  
    vEsc.tpeid:=0;   
    vEsc.tpercodigo:='';  
    vEsc.tpedesc:='';  
    vEsc.prioridad:=0;
    
 end loop;

end;



function 
buscar
(
    pExpediente in varchar2:=null,  
    pBoletin in number:=null,  
    pTipoBoletin in number:=null,    
    pTidomid in number:=null,
    pTdmresid in number:=null,
    pFmoddesde in varchar2:=null,
    pFmodhasta in varchar2:=null,
    pFdocdesde in varchar2:=null, 
    pFdochasta in varchar2:=null,     
    pUsuario in number:=null,
    pDni in varchar2:=null,
    pEsmid in number:=null,
    pCargaFicheros in number:=null,
    pRecibo in varchar2:=null,
    pCaja in varchar2:=null,
    pLote in varchar2:=null,
    pIndice in varchar2:=null
) 
return escl pipelined is

cursor cEscritos
(vEsmid number, vExpediente varchar2, vTidomid number,
vTdmresid number, vRecibo varchar2,vFmoddesde varchar2,
vFmodhasta varchar2, vFdocdesde varchar2, vFdochasta varchar2, vBoletin number)
is
/*select e.esmid
from alba.escritosmultas e
where trunc(e.esmfescrito)>=to_date(vfdocdesde,'DD/MM/RRRR')
and trunc(e.esmfescrito)<=to_date(vfdochasta, 'DD/MM/RRRR')
and e.tidomid=vTidomid
and e.tdmresid=vTdmresid
union*/
select e.esmid, e.mulid
from alba.escritosmultas e
where e.tidomid=vTidomid
and e.tdmresid=vTdmresid
and e.esmfhoramod>=to_date(vfmoddesde || '00:00:00','DD/MM/YYYY hh24:mi:ss')
and e.esmfhoramod<=to_date(vfmodhasta || '23:59:59','DD/MM/YYYY hh24:mi:ss')
--and upper(substr(e.esmobservaciones,1,3))='FNP
/*union
select e.esmid
from alba.escritosmultas e
where e.esmid=vEsmid
union
select e.esmid
from alba.escritosmultas e
where e.esmnumbol=vBoletin
union
select e.esmid
from 
alba.expingre l,
alba.multas m,
alba.escritosmultas e
where e.mulid=m.mulid
and m.expidexp=l.expid
and l.expcod=vExpediente
union
select e.esmid
from 
alba.liquidaciones l,
alba.multas m,
alba.escritosmultas e
where e.mulid=m.mulid
and m.expidexp=l.expid
and l.liqnumerorecliquidacion=vRecibo*/
;


/**/


begin
  
  for e in 
  cEscritos(pEsmid, pExpediente, pTidomid,
  pTdmresid, pRecibo, pFmoddesde,
  pFmodhasta, pFdocdesde, pFdochasta, pBoletin) 
  loop
  
    resetvEsc;
    
    leerEscrito(e.esmid);
    --leerColor(e.esmid);
    leerExpediente(e.mulid);
  
    pipe row(vEsc);     
    
  end loop;
  
  return;      
  
exception when others then
  dbms_output.put_line('KO. ' || sqlerrm);
end;

end;
/

/*

select * from table(esct.buscar(vEsmid=>1209081, vBoletin=>60004434))
select * from table(esct.buscar(vfmoddesde=>'21/04/2011', vfmodhasta=>'22/04/2014', vTidomid=>41, vTdmresid=>63))
select * from table(esct.buscar(vfmoddesde=>'21/04/2011', vfmodhasta=>'22/04/2014', vTidomid=>41, vTdmresid=>63))
select * from table(esct.buscar(vfdocdesde=>'03/03/2011', vfdochasta=>'03/03/2014', vTidomid=>41, vTdmresid=>63))
select * from table(esct.buscar(vRecibo=>'201401609424'))
select * from table(esct.buscar(vExpediente=>'200800145594'))
;

select e.esmid, m.mulid, m.mulfhoramod, e.esmnumbol Boletin, ee.expcod Expediente, 
d.tidomnombre TipoDocumento,  e.esmfescrito Fecha,e.esmobservaciones Observaciones, u.usunombre Usuario,  r.tdmnombre estado, 
e.esmfestado fechaestado, r.tdmresid, d.tidomid, 
decode((select max(mm.mulversion) from alba.multas mm where mm.seoidexp=m.seoidexp),
m.mulversion,0,1) distver,  e.perid, e.perversion, e.rdiid , e.esmcaja, e.esmlote, e.esmindice, e.esmfgrabacion, e.esmfhoramod, 
l.liqnumerorecliquidacion liquidacion, l.liqxestado, m.expidexp,
alba.componer_nombre(p.pernombre, p.perapell1, p.perpart1, p.perapell2, p.perpart2, p.perrazon) nombre,
p.periden nif, d.decid, d.decelemento, d.decdescr, t.tpeid, t.tpercodigo, t.tpedesc, expi.expid, 
decode(d.decelemento, 'TUTOR', 1, 'CONDUCTOR', 2, 'TITULAR', 3, 'SUJETOPASIVO', 4) prioridad, color.color_html,
(select distinct 1 from escritosmultas e2
where e2.esmid <> e.esmid
and e2.esmcaja = e2.esmcaja
and e2.esmlote = e2.esmlote
and e2.esmindice = e2.esmindice and rownum < 2) totalCLI
from alba.escritosmultas e 
left outer join alba.multas m on m.mulid = e.mulid
left outer join alba.expingre ee on ee.expid = m.expidexp
left outer join alba.liquidaciones l on l.expid = ee.expid
left outer join alba.expedientesingresoscontr expi on m.expidexp = expi.expid 
left outer join alba.decodificadora d on expi.eicxtipocontribuyente = d.decid 
left outer join alba.contribuyentes c on expi.conid = c.conid          
left outer join alba.personas p on c.perid = p.perid 
left outer join alba.tipospersona t on p.tpeid = t.tpeid 
left outer join alba.tipos_doc_multas_color color on color.tidomid = e.tidomid 
and color.tdmresid = e.tdmresid 
and color.liqxestado = l.liqxestado, 
alba.usuarios u, alba.tipos_doc_multas d, alba.tipos_doc_multas_res r      
where e.esmfhoramod >= to_date('21/04/2014 00:00:00', 'DD/MM/YYYY HH24:MI:SS') 
AND e.esmfhoramod <= to_date('22/04/2014 23:59:59', 'DD/MM/YYYY HH24:MI:SS')
and d.tidomid = e.tidomid 
and e.usuidmod = u.usuid
AND d.tidomid = e.tidomid 
AND r.tdmresid = e.tdmresid 
AND expi.eicfvigencia IS NULL   
;


*/