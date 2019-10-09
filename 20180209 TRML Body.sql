create or replace PACKAGE BODY TRML is

vMul mul;
vFmul fmul;
vDmul fmul;
vmul2 mul2;


vBop bop;
vPres prescripcion;
vDc dc;
vtea tea;
vEov eov;

vAgente agente_reg;


--------------------------------------------------------------------------------
-- Procesa Ries BEGIN
--------------------------------------------------------------------------------
procedure procesaRies is

cursor cEscritos is
select r.id_entrada,
upper(substr(r.asunto,1,1)) tipo, 
nvl(r.fecha_doc,r.fecha_ent) fdoc, 
replace(upper(fic.token(replace(replace(r.asunto,'.',','),', ',','),1,' ')),upper(substr(r.asunto,1,1)),'') valor
from alba.regries r
where upper(substr(r.asunto,1,1)) in ('I','A')
and r.id_unidad in ('50003')
and regexp_replace(fic.token(replace(r.asunto,', ',','),1,' '),'[^0-9]') is not null
and not exists 
    (select 1 from alba.escritosmultasrec r2 
    where r2.id_entrada=r.id_entrada)
;

cursor cEntrada(vEntrada varchar2, vId_entrada varchar2, vTipo varchar2, vFdoc date) is
select
alba.escritosmultasrec_sq.nextval EREGID,
case when substr(b.valor,1,1)='A' then 1
      else 
          case when substr(b.valor,1,1)='I' then 2
          else decode(vTipo,'A',1,'I',2) end
      end EREGTIPOESCRITO,
case when regexp_replace(b.valor,'[^0-9]')>200600000000 
      and regexp_replace(b.valor,'[^0-9]')<202000000000
      then '' 
      else regexp_replace(b.valor,'[^0-9]') end EREGBOLETIN,
case when regexp_replace(b.valor,'[^0-9]')>200600000000 
      and regexp_replace(b.valor,'[^0-9]')<202000000000
      then regexp_replace(b.valor,'[^0-9]') 
      else '' end EREGEXPEDIENTE,
vFdoc EREGFECHADOCUMENTO,
'40000000' EREGCAJA,
'1' EREGLOTE,
alba.escritosmultasrec_sq.nextval EREGINDICE,
vId_entrada ID_ENTRADA,
sysdate EREGFHORAMOD,
0 USUIDMOD,
'RIES' EREGMOTIVO,
sysdate EREGFPROCESO
from 
(select regexp_substr(vEntrada,'[^,]+', 1, level) valor from dual
connect by regexp_substr(vEntrada, '[^,]+', 1, level) is not null) b
;


vEficfid number:=0;
i number:=0;
vOut varchar2(2000):='';

begin

  
  i:=0;
  for d in cEscritos loop
    for e in cEntrada(d.valor, d.id_entrada, d.tipo, d.fdoc) loop
  
        if i=0 then
        
            --Alta de fichero
            select alba.escritosmultasfic_sq.nextval into vEficfid from dual;
            
            insert into alba.escritosmultasfic
            select
            vEficfid,
            'RIES'||to_char(vEficfid)||'.txt' eficfnombre,
            'I' eficfestado,
            sysdate eficffhoramod,
            0 usuidmod,
            sysdate eficffcarga,
            sysdate eficffproceso,
            'RIES' eficfmotivo,
            1 eficftotaldetalle
            from dual;
       
        end if;
      
        dbms_output.put_line(e.EREGBOLETIN);
      
        insert into alba.escritosmultasrec 
        (EREGID,EFICFID,EREGTIPOESCRITO,EREGBOLETIN,EREGEXPEDIENTE,
        EREGFECHADOCUMENTO,EREGCAJA,EREGLOTE,EREGINDICE,EREGFHORAMOD,EREGFPROCESO,ID_ENTRADA)
        values
        (e.EREGID,vEficfid,e.EREGTIPOESCRITO,e.EREGBOLETIN,e.EREGEXPEDIENTE,
        e.EREGFECHADOCUMENTO,e.EREGCAJA,e.EREGLOTE,
        e.EREGINDICE,e.EREGFHORAMOD,e.EREGFPROCESO,e.ID_ENTRADA);
        
        i:=i+1;
    
    end loop;    
  end loop;
  
  commit;
  
  if i>0 then
      update alba.escritosmultasfic f
      set f.eficftotaldetalle=i
      where f.EFICFID=vEficfid;
      commit;
      
      vOut:=trml.cargaFicheroEscritos(vEficfid);
      commit;
  end if;
end;
--------------------------------------------------------------------------------
-- Procesa Ries END
--------------------------------------------------------------------------------

function agente(vAgcodigo varchar2) return agente_lista pipelined is

cursor cAgentes(vAgcodigo varchar2) is
select *
from alba.agentes a
where a.agecodigo=vAgcodigo
;

begin
  pipe row(vAgente);
end;


procedure cargaFicheroDenuncias is
cursor cFic is
select 
ma.fecha_carga fecha,
ma.nomfich_cargado fichero,
mt.total exptes,
ma.total_fichero registros,
ma.total_correctos correctos,
ma.total_incidencias incidencias
from
(
select trunc(m.mulfecgrab) fecha, mc.nomfich_cargado, 
count(m.mulid) total
from
alba.multacar mc,
alba.multas m
where m.mulnumbol=mc.boletin
and m.mulanobol=mc.annoboletin
and m.mulfecgrab>sysdate-4
and m.mulversion=
(select max(mm.mulversion) from alba.multas mm where mm.seoidexp=m.seoidexp)
group by m.mulfecgrab, mc.nomfich_cargado
) mt,
(
select * from alba.maestra_carga m where m.fecha_carga>sysdate-4
) ma
where ma.nomfich_cargado=mt.nomfich_cargado(+)
and ma.total_incidencias=0
and ma.total_fichero>0
and mt.total is null
order by ma.fecha_carga desc, ma.nomfich_cargado asc
;

    tipobol varchar2(100):='';
    tini date;
    tfin date;
    ttxt_procesados varchar2(3000):='Ok';
    tprocesados number;
    tpendiente number;
    pcuantos_exp number;
    pcuantas_fotos number;
    
    vfichero varchar2(200):='';

begin

    for f in cFic loop
    
        vfichero:=f.fichero;
    
        select tipoboletin into tipobol from alba.multacar 
        where nomfich_cargado = vfichero
        group by tipoboletin;
      
        alba.mul_boletines_externos.pcarga_bol_multacar(vfichero,'0','Carga de Boletines',tipobol);
        alba.mul_boletines_externos.pgrabo_maestra_multacar(pnomfich=>vfichero,pestado_exp=>'GENERADOS');
        
        select     max(decode(est_procesado,'PROCESADO',count(1))) procesados,
        max(decode(est_procesado,'PROCESADO PDTE. DGT',count(1))) pendiente
        into    tprocesados, tpendiente    
        from alba.multacar    
        where upper(nomfich_cargado) = upper(vfichero)    
        and  est_procesado in ('PROCESADO','PROCESADO PDTE. DGT')    
        group by est_procesado;
        
        Ttxt_procesados := 'El -Proceso de carga de denuncias-, ha generado los siguientes expedientes: '
        ||chr(13)|| 'Expedientes generados: ' ||tprocesados ||chr(13)|| 'Expedientes sin datos de titular: '||tpendiente;
        
        dbms_output.put_line(Ttxt_procesados);
        
        commit;
        
    end loop;
    
    commit;

end;

procedure generaEstados(vFechaHora varchar2) Is

ntrabajo number:=0;
begin

  for i in 2006..2018 loop
    for j in 1..12 loop
      dbms_job.submit(ntrabajo,'trml.matestado('||to_char(i)||lpad(j,2,0)||');',
      to_date(vFechaHora,'dd/mm/rrrr hh24:mi:ss'));
      commit;
    end loop;
  end loop;
  
  dbms_job.submit(ntrabajo,'trml.matEstadoEscritos;',to_date(trunc(sysdate)||' 21:00:00','dd/mm/rrrr hh24:mi:ss'));
  commit;
  
  dbms_job.submit(ntrabajo,'trml.matDatos;',to_date(trunc(sysdate)||' 23:50:00','dd/mm/rrrr hh24:mi:ss'));
  commit;
  
  --dbms_job.submit(ntrabajo,'trml.cargaFicheroDenuncias;',to_date(sysdate));
  --commit;



end;


procedure matEstado (vYear number) Is
    --
    cursor cmul is
      select m.mulid, m.mulfecgrab 
      from alba.multas m
      where m.mulfecgrab>=to_date('01/08/2006','dd/mm/rrrr')
      and to_char(m.mulfecgrab ,'rrrrmm')=to_char(vyear)
      ;       
       
    --
    cursor cEstados (cMulId number) is
        select a.mulid, a.cestado, a.fprescripcion,
               a.estado, a.expediente, a.mulnumbol, a.expid, a.liqid, a.liquidacion,
               a.peridt, a.peridc, a.peridtt, a.origendenuncia, a.xml,
               a.cestadocobro, a.estadocobro, a.fcobro,
               a.cobrado, a.total, a.pendiente, a.info
          from table(depuracion.trml.emultas(cMulId)) a;
    --
    parresultado varchar2(400);
    i number:=0;
    --
begin
  --
  insert into alba.multasestados 
  values(0,(to_char(vYear)||'1')+0,sysdate,null,
  'Ini: ' || to_char(sysdate,'dd/mm/rrrr hh24:mi:ss'),null,
  null,null,null,null,null,null,null,null,null,null,
  null,null,null,null,null,null,null);
  commit;
  --
  for l in cMul loop
    --
    for e in cEstados(l.MulId) loop
    
      insert into alba.multasestados 
        values( e.mulid, e.cestado, l.mulfecgrab , e.fprescripcion,
                e.estado, e.expediente, e.mulnumbol, e.expid, e.liqid, e.liquidacion,
                e.peridt, e.peridc, e.peridtt, e.origendenuncia, sysdate, e.xml,
                e.cestadocobro, e.estadocobro,e.fcobro, e.cobrado, e.total, e.pendiente,e.info);
      --
        commit;
      --
    End Loop;
    --
  end loop;
  --
  commit;
  --
  insert into alba.multasestados 
  values(0,(to_char(vYear)||'2')+0,sysdate,null,
  'Fin: ' || to_char(sysdate,'dd/mm/rrrr hh24:mi:ss'),null,
  null,null,null,null,null,null,null,null,null,null,
  null,null,null,null,null,null,null);
  commit;
  --   
  ----------------------------------------------------------------
  -- FINALIZACION DEL PROCESO CON EXCEPCION
  ----------------------------------------------------------------
exception
    when others then
       parresultado := 'KO. ' || SQLERRM;         
       dbms_output.put_line(parresultado);
       rollback;
end;


procedure matEstadoEscritos Is
    --
    cursor cEsc is
      select e.esmid
      from alba.escritosmultas e
      where e.tidomid=101
      and e.tdmresid in (261,281,385,401,421,521)
      ;       
       
    --
    cursor cEstados (vEsmid number) is
      select 
      ee.cestado, ee.estado, ee.esmid, ee.mulid, ee.rea,
      ee.pleno, ee.exp_tea, ee.numerolibro, ee.libro, ee.firma,
      ee.notrea, ee.fnotrea, ee.crmnumero, ee.crmcodigo, ee.iescrito,
      ee.cestadoliq, ee.estadoliq, l.liqnumerorecliquidacion liq
      from
      alba.multas m, alba.liquidaciones l, alba.escritosmultas e,
      table(depuracion.trml.eescrito(e.esmid)) ee
      where e.esmid=ee.esmid
      and e.mulid=m.mulid
      and l.expid=m.expidexp
      and e.tidomid=101
      and e.tdmresid in (261,281,385,401,421,521)
      and e.esmid=vEsmid
      ;
    --
    parresultado varchar2(400);
    i number:=0;
    --
begin
  --
  
  delete from depuracion.escritosestados;
  commit;
  
  --
  for l in cEsc loop
    --
    for ee in cEstados(l.esmid) loop
    
      insert /*+ append */ into depuracion.escritosestados 
        values(  ee.cestado, ee.estado, ee.esmid, ee.mulid, ee.rea,
                  ee.pleno, ee.exp_tea, ee.numerolibro, ee.libro, ee.firma,
                  ee.notrea, ee.fnotrea, ee.crmnumero, ee.crmcodigo, ee.iescrito,
                  ee.cestadoliq, ee.estadoliq, ee.liq);
      --
      i:=i+1;
      if i=10000 then
        commit;
        i:=0;
      end if;
      --
    End Loop;
    --
  end loop;
  --
  commit;
  --
  ----------------------------------------------------------------
  -- FINALIZACION DEL PROCESO CON EXCEPCION
  ----------------------------------------------------------------
exception
    when others then
       parresultado := 'KO. ' || SQLERRM;         
       dbms_output.put_line(parresultado);
       rollback;
end;


----------------------------------------------------------------
-- GENERACIÓN DE DATOS SOBRE EXPEDIENTES SANCIONADORES
----------------------------------------------------------------
procedure matDatos Is
  --
  parresultado varchar2(400);
  --
begin
  --
  
  delete from depuracion.estmul;
  commit;
  

insert into depuracion.estmul
-- Resumen Denuncias por Fecha de Denuncias
select 
'EXPEDIENTES' Tipo,
decode(mulxboletin,
        543,'SEMÁFORO ROJO',
        523,'BLACKBERRY',
        423,'ANTIVANDÁLICA',
        465,'CENTRO_HISTÓRICO',
        403,'DISCIPLINA_VIAL',
        464,'CONVIVENCIA',
        444,'TRÁFICO', --SIN MATRÍCULA
        144,'TRÁFICO',
        303,'ZONA_AZUL',
        623,'ORD_ANIMALES',
        664,'ORD_CIRCULACIÓN',
        723,'LEY_TRANSPORTE',
        743,'COCHES_CABALLO',
        800,'LIMPIEZA',
        'ERROR') Tipo2,
'' Tipo3,
Mes,        
Total
from
( 
select to_char(m.mulfecgrab,'YYYYMM') Mes,
decode(m.mulxboletin,444,144,m.mulxboletin) mulxboletin,
count( distinct(m.mulanobol||m.mulnumbol)) Total
from alba.multas m
where m.mulversion=(select max(mm.mulversion) from alba.multas mm where mm.seoidexp=m.seoidexp)
--and to_char(m.mulfecgrab,'YYYY')='2010'
group by to_char(m.mulfecgrab,'YYYYMM'),decode(m.mulxboletin,444,144,m.mulxboletin)
)

union

-- Resumen Notificaciones
select
'NOTIFICACIONES' tipo,
'' Tipo2,
'' Tipo3,
to_char(c.crmfecha,'yyyymm') mes, count(p.pntid) total
from alba.propuestasnotificacion p, alba.notificaciones n,
alba.remesas r, alba.cabremesas c
where r.crmid=c.crmid
and r.ntfid=n.ntfid
and n.pntid=p.pntid
and n.ntfcrmid=r.crmid
and c.crmnumero is not null
and p.pntorigen='I'
group by to_char(c.crmfecha,'yyyymm')

union

-- Resumen Total Escritos Recibidos por Tipo y Mes
select 
decode(t.tidomid,1,'ALEGACIÓN',2,'RECURSO',3,'REC_EXT_REV',21,'IDENT_COND') tipo,
decode(m.mulxboletin,
        543,'SEMÁFORO ROJO',
        523,'BLACKBERRY',
        423,'ANTIVANDÁLICA',
        465,'CENTRO_HISTÓRICO',
        403,'DISCIPLINA_VIAL',
        464,'CONVIVENCIA',
        444,'TRÁFICO', --SIN MATRÍCULA
        144,'TRÁFICO',
        303,'ZONA_AZUL',
        623,'ORD_ANIMALES',
        664,'ORD_CIRCULACIÓN',
        723,'LEY_TRANSPORTE',
        743,'COCHES_CABALLO',
        800,'LIMPIEZA',        
        'ERROR') Tipo2,
'' tipo3,        
to_char(e.esmfescrito,'rrrrmm') mes, 
count(e.esmid) total
from alba.escritosmultas e, alba.tipos_doc_multas t, alba.tipos_doc_multas_res r, alba.multas m
where e.tidomid=t.tidomid
and e.tdmresid=r.tdmresid
and e.mulid=m.mulid
and t.tidomid in (1,2,3,21)
group by
decode(t.tidomid,1,'ALEGACIÓN',2,'RECURSO',3,'REC_EXT_REV',21,'IDENT_COND'),
decode(m.mulxboletin,
        543,'SEMÁFORO ROJO',
        523,'BLACKBERRY',
        423,'ANTIVANDÁLICA',
        465,'CENTRO_HISTÓRICO',
        403,'DISCIPLINA_VIAL',
        464,'CONVIVENCIA',
        444,'TRÁFICO', --SIN MATRÍCULA
        144,'TRÁFICO',
        303,'ZONA_AZUL',
        623,'ORD_ANIMALES',
        664,'ORD_CIRCULACIÓN',
        723,'LEY_TRANSPORTE',
        743,'COCHES_CABALLO',
        800,'LIMPIEZA',        
        'ERROR'),
to_char(e.esmfescrito,'rrrrmm')


union

-- Resumen Total Escritos Recibidos por Mes
select 'ESCRITOS' tipo,
decode(m.mulxboletin,
        543,'SEMÁFORO ROJO',
        523,'BLACKBERRY',
        423,'ANTIVANDÁLICA',
        465,'CENTRO_HISTÓRICO',
        403,'DISCIPLINA_VIAL',
        464,'CONVIVENCIA',
        444,'TRÁFICO', --SIN MATRÍCULA
        144,'TRÁFICO',
        303,'ZONA_AZUL',
        623,'ORD_ANIMALES',
        664,'ORD_CIRCULACIÓN',
        723,'LEY_TRANSPORTE',
        743,'COCHES_CABALLO',
        800,'LIMPIEZA',        
        'ERROR') Tipo2,
'' tipo3, 
to_char(e.esmfescrito,'rrrrmm') mes, count(e.esmid) total
from alba.escritosmultas e, alba.tipos_doc_multas t, alba.tipos_doc_multas_res r, alba.multas m
where e.tidomid=t.tidomid
and e.tdmresid=r.tdmresid
and e.mulid=m.mulid
and t.tidomid in (1,2,3,21)
group by
decode(m.mulxboletin,
        543,'SEMÁFORO ROJO',
        523,'BLACKBERRY',
        423,'ANTIVANDÁLICA',
        465,'CENTRO_HISTÓRICO',
        403,'DISCIPLINA_VIAL',
        464,'CONVIVENCIA',
        444,'TRÁFICO', --SIN MATRÍCULA
        144,'TRÁFICO',
        303,'ZONA_AZUL',
        623,'ORD_ANIMALES',
        664,'ORD_CIRCULACIÓN',
        723,'LEY_TRANSPORTE',
        743,'COCHES_CABALLO',
        800,'LIMPIEZA',        
        'ERROR'),
to_char(e.esmfescrito,'rrrrmm')

union

--insert into depuracion.estmul
select 
'ESTADO ESCRITOS' tipo,
decode(t.tidomid,1,'ALEGACIÓN',2,'RECURSO',3,'REC_EXT_REV',21,'IDENT_COND',61,'BAJA') ||' '||
upper(r.tdmnombre) Tipo2,
'' tipo3,        
to_char(e.esmfescrito,'rrrrmm') mes, 
count(e.esmid) total
from alba.escritosmultas e, alba.tipos_doc_multas t, alba.tipos_doc_multas_res r, alba.multas m
where e.tidomid=t.tidomid
and e.tdmresid=r.tdmresid
and e.mulid=m.mulid
and t.tidomid in (1,2,3,21,61)
group by
'ESTADO ESCRITOS',
decode(t.tidomid,1,'ALEGACIÓN',2,'RECURSO',3,'REC_EXT_REV',21,'IDENT_COND',61,'BAJA')||' '||
upper(r.tdmnombre),
to_char(e.esmfescrito,'rrrrmm')

union

-- Total cobros
select 'INGRESOS' tipo, tipo2, '' tipo3, Mes, Sum(TotalImp) total
from
(
		select 
    '' tipo2,
    to_char(c.cslfcobro,'YYYYMM') Mes, 
    count(c.cslid) Total, 
    sum(c.cslimporte) TotalImp
		from alba.cobrossinliquidacion c
		where c.mexid=21
		group by to_char(c.cslfcobro,'YYYYMM')
		union all
		select 
    decode(m.mulxboletin,
    543,'SEMÁFORO ROJO',
    523,'BLACKBERRY',
    423,'ANTIVANDÁLICA',
    465,'CENTRO_HISTÓRICO',
    403,'DISCIPLINA_VIAL',
    464,'CONVIVENCIA',
    444,'TRÁFICO', --SIN MATRÍCULA
    144,'TRÁFICO',
    303,'ZONA_AZUL',
    623,'ORD_ANIMALES',
    664,'ORD_CIRCULACIÓN',
    723,'LEY_TRANSPORTE',
    743,'COCHES_CABALLO',
    800,'LIMPIEZA',    
    'ERROR') Tipo2,        
    to_char(l.liqfcobro,'YYYYMM') Mes, count(l.liqid) Total, sum(l.liqimporte) TotalImp
		from alba.expingre e, alba.liquidaciones l, alba.multas m
		where m.expidexp=l.expid
		and l.liqxestado=13
		and l.expid=e.expid
		and e.mexid=21
		and l.liqfcobro is not null
		and not exists
			(select 1 from alba.cobrossinliquidacion c where
			c.mexid=21 and c.cslcodexpediente=to_char(m.mulnumbol))
		group by 
    decode(m.mulxboletin,
    543,'SEMÁFORO ROJO',
    523,'BLACKBERRY',
    423,'ANTIVANDÁLICA',
    465,'CENTRO_HISTÓRICO',
    403,'DISCIPLINA_VIAL',
    464,'CONVIVENCIA',
    444,'TRÁFICO', --SIN MATRÍCULA
    144,'TRÁFICO',
    303,'ZONA_AZUL',
    623,'ORD_ANIMALES',
    664,'ORD_CIRCULACIÓN',
    723,'LEY_TRANSPORTE',
    743,'COCHES_CABALLO',
    800,'LIMPIEZA',    
    'ERROR'),
    to_char(l.liqfcobro,'YYYYMM')
)
group by tipo2, mes


union
  
select 
decode(e.cestadocobro,3,'COBRADO EN VOL',4,'COBRADO EN EJE',2,'EN EJECUTIVA',5,'FIN SIN COBRO',6,'BAJA','EN TRÁMITE') tipo,
decode(m.mulxboletin,
        543,'SEMÁFORO ROJO',
        523,'BLACKBERRY',
        423,'ANTIVANDÁLICA',
        465,'CENTRO_HISTÓRICO',
        403,'DISCIPLINA_VIAL',
        464,'CONVIVENCIA',
        444,'TRÁFICO', --SIN MATRÍCULA
        144,'TRÁFICO',
        303,'ZONA_AZUL',
        623,'ORD_ANIMALES',
        664,'ORD_CIRCULACIÓN',
        723,'LEY_TRANSPORTE',
        743,'COCHES_CABALLO',
        800,'LIMPIEZA',        
        'ERROR') Tipo2,
'' tipo3, 
to_char(e.mulfecgrab,'YYYYMM') mes, 
count(e.mulid) total
from alba.multasestados e, alba.multas m where e.mulid=m.mulid and e.mulid>0
group by
decode(e.cestadocobro,3,'COBRADO EN VOL',4,'COBRADO EN EJE',2,'EN EJECUTIVA',5,'FIN SIN COBRO',6,'BAJA','EN TRÁMITE'),
decode(m.mulxboletin,
        543,'SEMÁFORO ROJO',
        523,'BLACKBERRY',
        423,'ANTIVANDÁLICA',
        465,'CENTRO_HISTÓRICO',
        403,'DISCIPLINA_VIAL',
        464,'CONVIVENCIA',
        444,'TRÁFICO', --SIN MATRÍCULA
        144,'TRÁFICO',
        303,'ZONA_AZUL',
        623,'ORD_ANIMALES',
        664,'ORD_CIRCULACIÓN',
        723,'LEY_TRANSPORTE',
        743,'COCHES_CABALLO',
        800,'LIMPIEZA',        
        'ERROR'),
to_char(e.mulfecgrab,'YYYYMM')

union

select 
'EXP_COBRADOS' tipo,
decode(m.mulxboletin,
        543,'SEMÁFORO ROJO',
        523,'BLACKBERRY',
        423,'ANTIVANDÁLICA',
        465,'CENTRO_HISTÓRICO',
        403,'DISCIPLINA_VIAL',
        464,'CONVIVENCIA',
        444,'TRÁFICO', --SIN MATRÍCULA
        144,'TRÁFICO',
        303,'ZONA_AZUL',
        623,'ORD_ANIMALES',
        664,'ORD_CIRCULACIÓN',
        723,'LEY_TRANSPORTE',
        743,'COCHES_CABALLO',
        800,'LIMPIEZA',        
        'ERROR') Tipo2,
'' tipo3, 
to_char(e.mulfecgrab,'YYYYMM') mes, 
count(e.mulid) total
from alba.multasestados e, alba.multas m where e.mulid=m.mulid and e.mulid>0 and e.cobrado>0
group by
'EXPTES_COBRADOS',
decode(m.mulxboletin,
        543,'SEMÁFORO ROJO',
        523,'BLACKBERRY',
        423,'ANTIVANDÁLICA',
        465,'CENTRO_HISTÓRICO',
        403,'DISCIPLINA_VIAL',
        464,'CONVIVENCIA',
        444,'TRÁFICO', --SIN MATRÍCULA
        144,'TRÁFICO',
        303,'ZONA_AZUL',
        623,'ORD_ANIMALES',
        664,'ORD_CIRCULACIÓN',
        723,'LEY_TRANSPORTE',
        743,'COCHES_CABALLO',
        800,'LIMPIEZA',        
        'ERROR'),
to_char(e.mulfecgrab,'YYYYMM')

union

select 
'IMP_COBRADO' tipo,
decode(m.mulxboletin,
        543,'SEMÁFORO ROJO',
        523,'BLACKBERRY',
        423,'ANTIVANDÁLICA',
        465,'CENTRO_HISTÓRICO',
        403,'DISCIPLINA_VIAL',
        464,'CONVIVENCIA',
        444,'TRÁFICO', --SIN MATRÍCULA
        144,'TRÁFICO',
        303,'ZONA_AZUL',
        623,'ORD_ANIMALES',
        664,'ORD_CIRCULACIÓN',
        723,'LEY_TRANSPORTE',
        743,'COCHES_CABALLO',
        800,'LIMPIEZA',        
        'ERROR') Tipo2,
'' tipo3, 
to_char(e.mulfecgrab,'YYYYMM') mes, 
sum(e.cobrado) total
from alba.multasestados e, alba.multas m where e.mulid=m.mulid and e.mulid>0 and e.cobrado>0
group by
'IMP_COBRADO',
decode(m.mulxboletin,
        543,'SEMÁFORO ROJO',
        523,'BLACKBERRY',
        423,'ANTIVANDÁLICA',
        465,'CENTRO_HISTÓRICO',
        403,'DISCIPLINA_VIAL',
        464,'CONVIVENCIA',
        444,'TRÁFICO', --SIN MATRÍCULA
        144,'TRÁFICO',
        303,'ZONA_AZUL',
        623,'ORD_ANIMALES',
        664,'ORD_CIRCULACIÓN',
        723,'LEY_TRANSPORTE',
        743,'COCHES_CABALLO',
        800,'LIMPIEZA',        
        'ERROR'),
to_char(e.mulfecgrab,'YYYYMM')

union

select
'HECHO DENUNCIADO' tipo,
'PARADA ESTACIONAMIENTO' tipo2,
'' tipo3,
to_char(m.mulfecgrab,'YYYYMM') Mes,
count(*) total
from alba.multasestados m
where m.origendenuncia in (1,2,3,5)
and m.info like '%<POE>%'
group by 'HECHO DENUNCIADO',
'PARADA ESTACIONAMIENTO',
'',
to_char(m.mulfecgrab,'YYYYMM')

union
select
'HECHO DENUNCIADO' tipo,
'VELOCIDAD' tipo2,
'' tipo3,
to_char(m.mulfecgrab,'YYYYMM') Mes,
count(*) total
from alba.multasestados m
where m.origendenuncia in (1,2,3,5)
and m.info like '%<VEL>%'
group by 'HECHO DENUNCIADO',
'VELOCIDAD',
'',
to_char(m.mulfecgrab,'YYYYMM')

union

select
'HECHO DENUNCIADO' tipo,
'CASCO' tipo2,
'' tipo3,
to_char(m.mulfecgrab,'YYYYMM') Mes,
count(*) total
from alba.multasestados m
where m.origendenuncia in (1,2,3,5)
and m.info like '%<CSC>%'
group by 'HECHO DENUNCIADO',
'CASCO',
'',
to_char(m.mulfecgrab,'YYYYMM')

union
select
'HECHO DENUNCIADO' tipo,
'CINTURÓN' tipo2,
'' tipo3,
to_char(m.mulfecgrab,'YYYYMM') Mes,
count(*) total
from alba.multasestados m
where m.origendenuncia in (1,2,3,5)
and m.info like '%<CIN>%'
group by 'HECHO DENUNCIADO',
'CINTURÓN',
'',
to_char(m.mulfecgrab,'YYYYMM')

union
select
'HECHO DENUNCIADO' tipo,
'MÓVIL' tipo2,
'' tipo3,
to_char(m.mulfecgrab,'YYYYMM') Mes,
count(*) total
from alba.multasestados m
where m.origendenuncia in (1,2,3,5)
and m.info like '%<MOV>%'
group by 'HECHO DENUNCIADO',
'MÓVIL',
'',
to_char(m.mulfecgrab,'YYYYMM')

union
select
'HECHO DENUNCIADO' tipo,
'ALCOHOL' tipo2,
'' tipo3,
to_char(m.mulfecgrab,'YYYYMM') Mes,
count(*) total
from alba.multasestados m
where m.origendenuncia in (1,2,3,5)
and m.info like '%<ALH>%'
group by 'HECHO DENUNCIADO',
'ALCOHOL',
'',
to_char(m.mulfecgrab,'YYYYMM')

union
select
'HECHO DENUNCIADO' tipo,
'SEMÁFORO ROJO' tipo2,
'' tipo3,
to_char(m.mulfecgrab,'YYYYMM') Mes,
count(*) total
from alba.multasestados m
where m.origendenuncia in (1,2,3,5)
and m.info like '%<ROJ>%'
group by 'HECHO DENUNCIADO',
'SEMÁFORO ROJO',
'',
to_char(m.mulfecgrab,'YYYYMM')

union
select
'HECHO DENUNCIADO' tipo,
'DROGAS' tipo2,
'' tipo3,
to_char(m.mulfecgrab,'YYYYMM') Mes,
count(*) total
from alba.multasestados m
where m.origendenuncia in (1,2,3,5)
and m.info like '%<DRG>%'
group by 'HECHO DENUNCIADO',
'DROGAS',
'',
to_char(m.mulfecgrab,'YYYYMM')
;
  --------------------------------------------------------------------------------
  -- FIN: GENERACIÓN DE DATOS ESTADÍSTICOS
  --------------------------------------------------------------------------------
  
  commit;
  
  ----------------------------------------------------------------
  -- FINALIZACION DEL PROCESO CON EXCEPCION
  ----------------------------------------------------------------
exception
    when others then
       parresultado := 'KO. ' || SQLERRM;         
       dbms_output.put_line(parresultado);
       rollback;
end;


--------------------------------------------------------------------------------
-- Info BEGIN
--------------------------------------------------------------------------------
procedure setInfo(vMulid number) is

--Infracciones No Identificación
cursor cOriginal(vMulid number) is
select /*+RULE*/ i.infid, m.mulid
from
alba.infracciones i,
alba.multas m
where m.infid=i.infid
and m.seoidexp=
(
select /*+RULE*/   m2.seoidexp
from alba.multas m2
where m2.mulid=vMulid
and m2.infid in (2646,1808,1809,1810,2697,2698,6000881,6000882,9002328,9002344,9004031)
)
and m.mulid=
(
select /*+RULE*/   min(m3.mulid)
from alba.multas m3
where m3.seoidexp=m.seoidexp
)
;

cursor cInf(vMulid number) is
select /*+RULE*/ i.infid, m.mulid, 
case when upper(m.muldenuncia) like '%ALCOH%'
then '<ALH>' end ||
case when (upper(m.muldenuncia) like '%CINTUR%'
  or (upper(m.muldenuncia) like '%DISPOSITIVO HOMOL%' and upper(m.muldenuncia) like '%MENOR%')
  or (upper(m.muldenuncia) like '%SUJECIÓN%'and upper(m.muldenuncia) like '%MENOR%') 
  or (upper(m.muldenuncia) like '%SUJECIÓN%'and upper(m.muldenuncia) like '%PESO%') )
then '<CIN>' end ||
case when upper(m.muldenuncia) like '%VELOC%'
then '<VEL>' end ||
case when (upper(m.muldenuncia) like '%DROGA%' or upper(m.muldenuncia) like '%ESTUPEFACI%')
then '<DRG>' end ||
case when upper(m.muldenuncia) like '%CASCO%' and upper(m.muldenuncia) like '%PROTECC%'
then '<CSC>' end ||
case when upper(m.muldenuncia) like '%PARAR%' or upper(m.muldenuncia) like '%ESTACION%'
then '<POE>' end ||
case when upper(m.muldenuncia) like '% ROJA%' or (upper(m.muldenuncia) like '%NO OBEDEC%' and upper(m.muldenuncia) like '%SEM%' and upper(m.muldenuncia) like '%FORO%')
then '<ROJ>' end ||
case when 
      ((upper(m.muldenuncia) like '%TELÉFONO%' or upper(m.muldenuncia) like '%TELEFONO%') 
      and (upper(m.muldenuncia) like '% MOVIL%' or upper(m.muldenuncia) like '% MÓVIL%') )
      or (upper(m.muldenuncia) like '%AURICULARES%'and upper(m.muldenuncia) like '%SONIDO%')
then '<MOV>' end
info
from
alba.infracciones i,
alba.multas m
where m.infid=i.infid
and m.mulid=vMulid
;

cursor cLibros(vMulid number) is
select 
case when t.lrtid in (71,325) then '<L21>' end ||
case when t.lrtid in (67,324) then '<L16>' end ||
case when t.lrtid in (66,323) then '<L14>' end 
info
from 
alba.multas m,
alba.libro_resol_tipos t, 
alba.libro_resoluciones l, 
alba.det_libro_resol d
where d.lbrid=l.lbrid
and l.lrtid=t.lrtid
and m.mulid=d.mulid
and t.lrtid in (71,325,67,324,66,323)
and m.seoidexp in
(select mm.seoidexp from alba.multas mm where mm.mulid=vMulid)
order by l.lbrid asc
;

cursor cReq(vMulid number) is
select 
case when t.lrtid in (71,325)
then '<L21>' end ||
case when t.lrtid in (67,324)
then '<L16>' end ||
case when t.lrtid in (66,323)
then '<L14>' end 
info
from 
alba.multas m,
alba.libro_resol_tipos t, 
alba.libro_resoluciones l, 
alba.det_libro_resol d
where d.lbrid=l.lbrid
and l.lrtid=t.lrtid
and m.mulid=d.mulid
and t.lrtid in (71,325,67,324,66,323)
and m.seoidexp in
(select mm.seoidexp from alba.multas mm where mm.mulid=vMulid)
order by l.lbrid asc
;

vMulid_o number:=0;
v21 boolean:=false;
v14 boolean:=false;
v16 boolean:=false;

begin

    vMulid_o:=vMulid;
    
    for o in cOriginal(vMulid) loop
    
      vMulid_o:=o.mulid;
      vMul.info:=vMul.info || '<NID>';
     
    end loop;
    
    for i in cInf(vMulid_o) loop

      vMul.info:=vMul.info || i.info;

    end loop;      
    
    --Analiza requerimientos
    v21:=false;
    v14:=false;
    v16:=false;
    
    --Genera etiqueta requerimientos
    vMul.info:=vMul.info || '<req>';
    for r in cReq(vMulid) loop
    
      if r.info='<L21>' and v21=false then
        v21:=true;
        vMul.info:=vMul.info || '<L21>';
      end if;

      if r.info='<L14>' and v14=false then
        v14:=true;
        vMul.info:=vMul.info || '<L14>';
      end if;

      if r.info='<L16>' and v16=false then
        v16:=true;
        vMul.info:=vMul.info || '<L16>';
      end if;
      
    end loop;
    vMul.info:=vMul.info || '</req>';
    
    

end;
--------------------------------------------------------------------------------
-- Info END
--------------------------------------------------------------------------------




-- TRATA Y ELEVA ERROR
Procedure error ( PTexto_Error VarChar2, PNombre_Proceso Varchar2, PCodigo Number default 20999 ) Is
Begin
        Raise_Application_Error (-1*Pcodigo,     PTexto_Error
                                ||Chr(10)||'Error en: ' ||PNombre_Proceso|| ':'
                                 ||Chr(10)||'ORA-'||LPad(Abs(SQLCODE),5,'0')     -- SQLCODE, muestra el típico "ORA-00904:"
                                 ||Chr(10)||SQLERRM);                            -- SQLERRM, Muestra el mensaje "PL/SQL: numeric or value error"
Exception
    When Others Then
        Raise;
End error;
--------------------------------------------------------------------------------
-- Inicialización de variables
--------------------------------------------------------------------------------
procedure iniciaRegistro is
begin
    -- Denuncia
    vmul.mulid:=0;
    vmul.mulnumbol:=0;
    vmul.expid:=0;
    vmul.expediente:=null;
    vmul.tnotden:=null;
    vmul.tnotsan:=null;
    vmul.fcomcon:=null;
    vmul.fecomcon:=null;
    vmul.matricula:=null;
    vmul.marca:=null;
    vmul.tipo:=null;
    -- Titular
    vmul.titular:=null;
    vmul.nift:=null;
    vmul.tpt:=0;
    vmul.domt:=null;
    vmul.loct:=null;
    vmul.prot:=null;
    vmul.cpt:=null;
    vmul.rdiidt:=0;
    vmul.peridt:=0;
    vmul.peridpt:=0;
    -- Conductor
    vmul.conductor:=null;
    vmul.nifc:=null;
    vmul.tpc:=0;
    vmul.domc:=null;
    vmul.locc:=null;
    vmul.proc:=null;
    vmul.cpc:=null;
    vmul.rdiidc:=0;
    vmul.peridc:=0;
    vmul.peridpc:=0;
    -- Tutor
    vmul.tutor:=null;
    vmul.niftt:=null;
    vmul.tptt:=0;
    vmul.domtt:=null;
    vmul.loctt:=null;
    vmul.prott:=null;
    vmul.cptt:=null;
    vmul.rdiidtt:=0;
    vmul.peridtt:=0;
    vmul.peridptt:=0;
    -- Cobros
    vmul.csl:=0;
    -- Denuncia
    vmul.notden:=0;
    vmul.fnotden:=null;
    vmul.libinc:=0;
    vmul.flibinc:=null;
    vmul.notdenrdiid:=0;
    vmul.notdenperid:=0;
    vmul.notdenperv:=0;
    vmul.notdenacto:=null;
    -- Sanción
    vmul.notsan:=0;
    vmul.fnotsan:=null;
    vmul.libsan:=0;
    vmul.flibsan:=null;
    vmul.notsanrdiid:=0;
    vmul.notsanperid:=0;
    vmul.notsanperv:=0;
    vmul.notsanacto:=null;
    -- Requerimiento
    vmul.notreq:=0;
    vmul.fnotreq:=null;
    vmul.libreq:=0;
    vmul.flibreq:=null;
    vmul.notreqrdiid:=0;
    vmul.notreqperid:=0;
    vmul.notreqperv:=0;
    vmul.notreqacto:=null;
    -- Alegación
    vmul.aleg:=0;
    vmul.faleg:=null;
    vmul.fraleg:=null;
    vmul.notaleg:=0;
    vmul.fnotaleg:=null;
    vmul.libaleg:=0;
    vmul.flibaleg:=null;
    vmul.notalegrdiid:=0;
    vmul.notalegperid:=0;
    vmul.notalegperv:=0;
    vmul.notalegacto:=null;
    vmul.alegesmid:=0;
    -- Recurso
    vmul.rec:=0;
    vmul.frec:=null;
    vmul.frrec:=null;
    vmul.notrec:=0;
    vmul.fnotrec:=null;
    vmul.librec:=0;
    vmul.flibrec:=null;
    vmul.notrecrdiid:=0;
    vmul.notrecperid:=0;
    vmul.notrecperv:=0;
    vmul.notrecacto:=null;
    vmul.recesmid:=0;
    -- Recurso Extraordinario de Revisión
    vmul.rer:=0;
    vmul.frer:=null;
    vmul.frrer:=null;
    vmul.notrer:=0;
    vmul.fnotrer:=null;
    vmul.librer:=0;
    vmul.flibrer:=null;
    vmul.notrerrdiid:=0;
    vmul.notrerperid:=0;
    vmul.notrerperv:=0;
    vmul.notreracto:=null;
    vmul.reresmid:=0;
    -- Baja
    vmul.baja:=0;
    vmul.fbaja:=null;
    vmul.frbaja:=null;
    vmul.notbaja:=0;
    vmul.fnotbaja:=null;
    vmul.libbaja:=0;
    vmul.flibbaja:=null;
    vmul.notbajardiid:=0;
    vmul.notbajaperid:=0;
    vmul.notbajaperv:=0;
    vmul.notbajaacto:=null;
    vmul.bajaesmid:=0;
    -- Informe Denunciante
    vmul.tipoinfd:=0;
    vmul.infd:=0;
    vmul.finfd:=null;
    vmul.frinfd:=null;
    vmul.notinfd:=0;
    vmul.fnotinfd:=null;
    vmul.libinfd:=0;
    vmul.flibinfd:=null;
    vmul.notinfdrdiid:=0;
    vmul.notinfdperid:=0;
    vmul.notinfdperv:=0;
    vmul.notinfdacto:=null;
    vmul.infdesmid:=0;
    -- Informe Requerimiento
    vmul.tipoinfq:=0;
    vmul.infq:=0;
    vmul.finfq:=null;
    vmul.frinfq:=null;
    vmul.notinfq:=0;
    vmul.fnotinfq:=null;
    vmul.libinfq:=0;
    vmul.flibinfq:=null;
    vmul.notinfqrdiid:=0;
    vmul.notinfqperid:=0;
    vmul.notinfqperv:=0;
    vmul.notinfqacto:=null;
    vmul.infqesmid:=0;
    -- Estado
    vmul.cestado:=0;
    vmul.estado:=null;
    vmul.ffirmeza:=null;
    vmul.finivol:=null;
    vmul.ffinvol:=null;
    -- Prescripcion
    vmul.forigencalculoprescripcion:=null;
    vmul.fprescripcion:=null;
    -- Libro Sanción Firme
    vmul.libsanfir:=0;
    vmul.flibsanfir:=null;
    -- BOP denuncia
    vmul.fnotdenintento:=null;
    vmul.libbopden:=0;
    vmul.flibbopden:=null;
    -- BOP sanción
    vmul.fnotsanintento:=null;
    vmul.libbopsan:=0;
    vmul.flibbopsan:=null;
    -- BOP requerimientos
    vmul.fnotreqintento:=null;
    vmul.libbopreq:=0;
    vmul.flibbopreq:=null;
    -- BOP alegaciones
    vmul.fnotalegintento:=null;
    vmul.libbopaleg:=0;
    vmul.flibbopaleg:=null;
    -- BOP recursos
    vmul.fnotrecintento:=null;
    vmul.libboprec:=0;
    vmul.flibboprec:=null;
    -- BOP recursos extraordinarios de revisión
    vmul.fnotrerintento:=null;
    vmul.libboprer:=0;
    vmul.flibboprer:=null;
    -- BOP recursos providencias de apremio
    vmul.fnotrpaintento:=null;
    vmul.libboprpa:=0;
    vmul.flibboprpa:=null;
    -- Consulta Datos
    vMul.fconsultaDatos:=null; --Fecha de incorporación de datos
    vMul.libcondat:=0;
    vMul.flibcondat:=null; --Fecha libro incorporación de datos
    --
    vmul.origendenuncia:=0;
    --
    -- Recurso Providencia de Apremio
    vmul.rpa:=0;
    vmul.frpa:=null;
    vmul.frrpa:=null;
    vmul.notrpa:=0;
    vmul.fnotrpa:=null;
    vmul.librpa:=0;
    vmul.flibrpa:=null;
    vmul.notrpardiid:=0;
    vmul.notrpaperid:=0;
    vmul.notrpaperv:=0;
    vmul.notrpaacto:='';
    vmul.rpaesmid:=0;
    vmul.numerolibroapremio:='';
    vmul.fresolucionlibroapremio:='';
    -- Providencia de Apremio
    vmul.notpa:=0;
    vmul.fnotpa:=null;
    -- Datos Liquidacion
    vmul.liqid:=0;
    vmul.liquidacion:='';
    vmul.importeliqtexto:='';
    vmul.importeliqnum:=0;
    vmul.edictoapremio:='';
    vmul.fsancionfirmeliq:='';
    vmul.ffinvoluntarialiq:='';
    vmul.finicioejecutivaliq:='';
    vmul.estadoliq:=0;
    vmul.intentonotpa:='';
    vmul.boppa:='';
    -- Requerimiento Juzgado
    vmul.rej:=0;
    vmul.frej:=null;
    vmul.frrej:=null;
    vmul.rejesmid:=0;
    -- REA
    vmul.rea:=0;
    vmul.frea:=null;
    vmul.frrea:=null;
    vmul.notrea:=0;
    vmul.fnotrea:=null;
    vmul.librea:=0;
    vmul.flibrea:=null;
    vmul.notreardiid:=0;
    vmul.notreaperid:=0;
    vmul.notreaperv:=0;
    vmul.notreaacto:='';
    vmul.reaesmid:=0;
  vmul.xml:=0;
  vmul.errorenvehiculo:=0;
  vmul.cestadocobro:=0;
  vmul.estadocobro:='';
  vmul.importeexpte:=0;
  vmul.fcobro:=null;
  vmul.cobrado:=0;
  vmul.total:=0;
  vMul.pendiente:=0;
      -- Prescripción y Caducidad
      vMul.pres:=0;
      vMul.cpres:='';
      vMul.cad:=0;
      vMul.ccad:='';
      vMul.info:='';
  
    -- Variables privadas
    vmul2.vtiposp:=0;
    vmul2.vesni:=0;
    vmul2.vesdr:=0;
    vmul2.vesda:=0;
    vmul2.vesdv:=0;
    vmul2.vestado:=0;
    vmul2.vtemp:=0;
    vmul2.vftemp:=null;
    vmul2.vfechaprescripcion:=null;
    vmul2.vfechaorigenprescripcion:=null;
    vmul2.vatocodigo:='';
    vmul2.vestado2:='';
    vmul2.vestado3:='';
    vmul2.vestado4:='';
    vmul2.vexpte:=0;
    vmul2.vtidomid:=0;
    vmul2.vtdmresid:=0;
    vmul2.vtotalresoluciones:=0;
    vmul2.vparalizado:=0;
    vmul2.vidconductor:=0;
    --
    vmul2.vgrado:=0;
    vmul2.vgrado2:=0;
    vmul2.vcalculoprescripciondenuncia:=0;
    vmul2.vescritoduplicado:=false;
  vmul2.fechadenuncia:=null;
  vmul2.total:=0;
    -- Variables prescripcion
    vpres.tipofecha:='';
    vpres.fecha:=null;
    vpres.dias:=0;
    vpres.limite:=0;
    vpres.fechalimite:=null;
    vpres.enplazo:=0;
    
end;



--------------------------------------------------------------------------------
-- Iniciar Variable vMul
--------------------------------------------------------------------------------
procedure iniciarVariableDmul is
begin
    vDmul.mulid :=0;
    vDmul.expediente :=0;
    vDmul.peridD :=0;
    vDmul.perversionD :=0;
    vDmul.rdiidD :=0;
    vDmul.peridT :=0;
    vDmul.perversionT :=0;
    vDmul.rdiidT :=0;
    --
    vDmul.tipoNot :='';
    vDmul.fLimitePago :='';
    vDmul.Emisora :='';
    vDmul.Referencia :='';
    vDmul.DC :='';
    vDmul.Identificacion :='';
    vDmul.Importe :='';
    vDmul.Importe2 :=0;
    vDmul.Importe3 :='';
    vDmul.Importe4 :=0;
    vDmul.nExpediente :='';
    vDmul.Boletin :='';
    --
    vDmul.fIncoacion :='';
    vDmul.Lugar :='';
    vDmul.fDenuncia :='';
    vDmul.Hora :='';
    vDmul.Matricula :='';
    vDmul.Vehiculo :='';
    --
    vDmul.nifD :='';
    vDmul.nombreD :='';
    vDmul.domicilioD :='';
    vDmul.cpD :='';
    vDmul.localidadD :='';
    vDmul.provinciaD :='';
    --
    vDmul.denunciante :='';
    vDmul.motivoNoEntrega :='';
    vDmul.hecho :='';
    vDmul.normativa :='';
    vDmul.calificacion :='';
    vDmul.tiempoRetirada :='';
    --
    vDmul.nifT :='';
    vDmul.nombreT :='';
    vDmul.domicilioT :='';
    vDmul.cpT :='';
    vDmul.localidadT :='';
    vDmul.provinciaT :='';
    --
    vDmul.velocidad :='';
    vDmul.limiteVelocidad :='';
    vDmul.textoCinemometro :='';
    --    
    vDmul.alcoholAire :='';
    vDmul.alcoholSangre :='';
    vDmul.fNotDenunica :='';
    --    
    vDmul.lugarOrg :='';
    vDmul.fechaOrg :='';
    vDmul.horaOrg :='';
    vDmul.matriculaOrg :='';
    vDmul.hechoOrg :='';
    vDmul.cbsicer :='';
    vDmul.cbcuaderno60 :='';
    vDmul.firma :='';
    --
    vDmul.a1 :='';
    vDmul.b1 :='';
    vDmul.a2 :='';
    vDmul.b2 :='';
    vDmul.a3 :='';
    vDmul.b3 :='';
    vDmul.a4 :='';
    vDmul.b4 :='';
    vDmul.a5 :='';
    vDmul.b5 :='';
    vDmul.a6 :='';
    vDmul.b6 :='';
    vDmul.a7 :='';
    vDmul.b7 :='';
    vDmul.a8 :='';
    vDmul.b8 :='';
    vDmul.a9 :='';
    vDmul.b9 :='';
    vDmul.a10 :='';
    vDmul.b10 :='';
    vDmul.a11 :='';
    vDmul.b11 :='';
    vDmul.a12 :='';
    vDmul.b12 :='';
    vDmul.a13 :='';
    vDmul.b13 :='';
    vDmul.a14 :='';
    vDmul.b14 :='';
    vDmul.a15 :='';
    vDmul.b15 :='';

  
    vDmul.informe :='';
    
    vDmul.recibo:='';
    vDmul.sancionFirme:='';
    vDmul.finVoluntaria:='';
    vDmul.inicioEjecutiva:='';
    vDmul.importeSancionFirme:='';
    vDmul.providenciaApremio:='';
    vDmul.intentoNotPA:='';
    vDmul.edictoPA:='';
    vDmul.bopPa:='';
    vDmul.numeroLibroApremio:='';
    vDmul.fResolucionLibroApremio:='';
    
    vDmul.caja:='';
    vDmul.lote:='';
    vDmul.indice:='';
    
    vDmul.fot01:='';
    vDmul.fot02:='';
    vDmul.fot03:='';
    vDmul.fot04:='';
    
end;



--------------------------------------------------------------------------------
-- Iniciar Variable vFul
--------------------------------------------------------------------------------
procedure iniciarVariableFmul is
begin
    vFmul.mulid :=0;
    vFmul.expediente :=0;
    vFmul.peridD :=0;
    vFmul.perversionD :=0;
    vFmul.rdiidD :=0;
    vFmul.peridT :=0;
    vFmul.perversionT :=0;
    vFmul.rdiidT :=0;
    --
    vFmul.tipoNot :='';
    vFmul.fLimitePago :='';
    vFmul.Emisora :='';
    vFmul.Referencia :='';
    vFmul.DC :='';
    vFmul.Identificacion :='';
    vFmul.Importe :='';
    vFmul.Importe2 :=0;
    vFmul.Importe3 :='';
    vFmul.Importe4 :=0;
    vFmul.nExpediente :='';
    vFmul.Boletin :='';
    --
    vFmul.fIncoacion :='';
    vFmul.Lugar :='';
    vFmul.fDenuncia :='';
    vFmul.Hora :='';
    vFmul.Matricula :='';
    vFmul.Vehiculo :='';
    --
    vFmul.nifD :='';
    vFmul.nombreD :='';
    vFmul.domicilioD :='';
    vFmul.cpD :='';
    vFmul.localidadD :='';
    vFmul.provinciaD :='';
    --
    vFmul.denunciante :='';
    vFmul.motivoNoEntrega :='';
    vFmul.hecho :='';
    vFmul.normativa :='';
    vFmul.calificacion :='';
    vFmul.tiempoRetirada :='';
    --
    vFmul.nifT :='';
    vFmul.nombreT :='';
    vFmul.domicilioT :='';
    vFmul.cpT :='';
    vFmul.localidadT :='';
    vFmul.provinciaT :='';
    --
    vFmul.velocidad :='';
    vFmul.limiteVelocidad :='';
    vFmul.textoCinemometro :='';
    --    
    vFmul.alcoholAire :='';
    vFmul.alcoholSangre :='';
    vFmul.fNotDenunica :='';
    --    
    vFmul.lugarOrg :='';
    vFmul.fechaOrg :='';
    vFmul.horaOrg :='';
    vFmul.matriculaOrg :='';
    vFmul.hechoOrg :='';
    vFmul.cbsicer :='';
    vFmul.cbcuaderno60 :='';
    vFmul.firma :='';
    --
    vFmul.a1 :='';
    vFmul.b1 :='';
    vFmul.a2 :='';
    vFmul.b2 :='';
    vFmul.a3 :='';
    vFmul.b3 :='';
    vFmul.a4 :='';
    vFmul.b4 :='';
    vFmul.a5 :='';
    vFmul.b5 :='';
    vFmul.a6 :='';
    vFmul.b6 :='';
    vFmul.a7 :='';
    vFmul.b7 :='';
    vFmul.a8 :='';
    vFmul.b8 :='';
    vFmul.a9 :='';
    vFmul.b9 :='';
    vFmul.a10 :='';
    vFmul.b10 :='';
    vFmul.a11 :='';
    vFmul.b11 :='';
    vFmul.a12 :='';
    vFmul.b12 :='';
    vFmul.a13 :='';
    vFmul.b13 :='';
    vFmul.a14 :='';
    vFmul.b14 :='';
    vFmul.a15 :='';
    vFmul.b15 :='';

    
    vFmul.informe :='';
    
    
    vFmul.caja:='';
    vFmul.lote:='';
    vfmul.indice:='';
    vFmul.mulid:=0;
    
end;



--------------------------------------------------------------------------------
-- Iniciar Variable vFul
--------------------------------------------------------------------------------
procedure iniciarDocCobratorio is
begin
        vDc.cdc:=0;
        vDc.ddc:='';
        --
        vDc.fLimitePago:='';
        vDc.Emisora:='';
        vDc.Referencia:='';
        vDc.DC:='';
        vDc.Identificacion:='';
        vDc.Importe:='';
        vDc.Importe2:=0;
        vDc.Importe3:='';
        vDc.Importe4:=0;
        vDc.Expediente:='';
        vDc.Boletin:='';
        --
        vDc.fDenuncia:='';
        vDc.Hora:='';
        vDc.Matricula:='';
        vDc.Vehiculo:='';
        --
        vDc.nifD:='';
        vDc.nombreD:='';
        vDc.domicilioD:='';
        vDc.cpD:='';
        vdc.localidadd:='';
        vDc.provinciaD:='';
        --
        vDc.cbcuaderno60:='';
        vDc.informe:='';
        vDc.cbcuaderno60num:='';
        --
        vdc.lugar:='';
        vDc.hecho:='';
        vDc.mulid:=0;
        --
        vDc.liqid:=0;
        vDc.expid:=0;
        vDc.principal:='';
        vDc.costas:='';
        vDc.apremio:='';
        vdc.intereses:='';
        vDc.acuenta:='';
end;


--------------------------------------------------------------------------------
-- Iniciar Variable vEov
--------------------------------------------------------------------------------
procedure iniciarEov is
begin
        vEov.esmid:=0;
        vEov.mulid:=0;
        vEov.Boletin:='';       
        vEov.Expediente:='';
        vEov.fechadoc:=''; 
        vEov.tipodoc:='';
        vEov.estadodoc:='';
end;


--------------------------------------------------------------------------------
-- Iniciar TEA
--------------------------------------------------------------------------------
procedure iniciarVariableTEA is
begin
      vtea.cestado:=0;
      vtea.estado:='';
      vtea.esmid:=0;
      vtea.mulid:=0;
      vtea.rea:=0;
      vtea.pleno:='';
      vtea.exp_tea:='';
      vtea.numeroLibro:=0;
      vtea.libro:=0;
      vtea.firma:=null;
      vtea.notrea:=0;
      vtea.fnotrea:=null;
      vtea.crmnumero:=0;
      vtea.crmcodigo:='';
      vtea.iescrito:='';
      vtea.cestadoliq:=0;
      vtea.estadoliq:='';
      vtea.liq:='';
end;


--------------------------------------------------------------------------------
-- Aplica Máscara de Matrícula
--------------------------------------------------------------------------------
function fMasMat(vTexto varchar2) return boolean is
vOk boolean;
vRetorno varchar2(20);
i number:=0;
vLongitud number:=0;
begin
    
    vOk:=false;
    vRetorno:='';
    i:=1;
    vLongitud:=0;
    select length(vTexto) into vLongitud from dual;
    
    while i<=vLongitud loop
        if substr(vTexto,i,1) in ('1','2','3','4','5','6','7','8','9','0') then
            vRetorno:=vRetorno||'N';
        else 
            vRetorno:=vRetorno||'A';
        end if;
        i:=i+1;
    end loop;
    
    if vRetorno in
        ('AANNNNNN','AANNNNAA','NNNNAAA','ANNNNAA','NNNNNNN','NNNNNAAA','AANNNNNNN',
        'NNNNNAAA','AANNNNNN','NNNNNAAA','NNNNNAAA','NNNNNAAA','NNNNNAAA','NNNNNAAA',
        'NANNNN','NNNNNN','NNNNNN','NNNNNN','NNNNNAAA','NNNNNN','NNNNNNNN','NNNNN',
        'AANNNNN','NAA','AANNNNA','ANNNNA','ANNNNNA','ANNNNNAA','NNNNNNNN','NNNNNNN',
        'ANNNNAAA')
    then
        vOk:=false;
    else
        vOk:=true;
    end if;
    
    return vOk;
    
end;



--------------------------------------------------------------------------------
-- Análisis de Cobros Sin Liquidación y Liquidaciones
--------------------------------------------------------------------------------
procedure leerCobros(vMulid number) is

    cursor ccob(vmulid number) is
    select /*+RULE*/ c.cslid, c.cslfcobro fechacobro, c.cslimporte
  from alba.cobrossinliquidacion c, alba.multas m
    where to_char(m.mulnumbol)=c.cslcodexpediente
    and c.cslfcobro>=vmul2.fechadenuncia
  and c.mexid in (1,4,21)
    and m.mulid=vmulid
  union
  select c.cslid, c.cslfcobro fechacobro, c.cslimporte
  from alba.cobrossinliquidacion c, alba.multas m
    where to_char(m.boletin2)=c.cslcodexpediente
    and c.cslfcobro>=vmul2.fechadenuncia    
  and c.mexid in (1,4,21)
    and m.mulid=vMulid
  ;
    
    cursor ccob2(vmulid number) is
    select /*+RULE*/ l.liqid, l.liqfcobro fechacobro,
  l.liqnumerorecliquidacion recibo, l.liqxestado estado
    from alba.liquidaciones l
    where l.expid in
    (select m2.expidexp from alba.multas m2 where m2.seoidexp in
    (select m.seoidexp from alba.multas m where m.mulid=vMulid))
    and to_number(l.liqxestado) in (481,480,13);
    
    cursor cCob3(vMulid number) is
    select /*+RULE*/ d.*
    from alba.multas m,
    alba.detallecobroentidades d
    where m.mulid=vMulid
    and d.dceboletin=m.mulnumbol
  and d.dcefcobro>=vmul2.fechadenuncia
  and (d.mexid in (1,4,21) or d.mexid is null)
  union
    select /*+RULE*/ d.*
    from alba.multas m,
    alba.detallecobroentidades d
    where m.mulid=vmulid
    and d.dceanualrec=m.boletin2
  and d.dcefcobro>=vmul2.fechadenuncia
  and (d.mexid in (1,4,21) or d.mexid is null)
  ;

begin

    -- Analiza liquidaciones
    for c2 in cCob2(vMulid) loop
        --vMul.csl:=c.cslid;
        vmul.csl:=10;
    vmul.fcobro:=to_date(c2.fechacobro,'dd/mm/rrrr');
    vMul.liqid:=c2.liqid;
    vmul.liquidacion:=c2.recibo;
    vmul.ffirmeza:=to_date(c2.fechacobro,'dd/mm/rrrr')+1;
    vmul.estadoliq:=c2.estado;
    end loop;

      -- Analiza cobros sin liquidación
    if vmul.csl in (0,10) then
        for c in cCob(vMulid) loop
          --vMul.csl:=c.cslid;
          vmul.csl:=1;
          vmul.fcobro:=to_date(c.fechacobro,'dd/mm/rrrr');
          vmul.cobrado:=c.cslimporte;
        end loop;
    end if;
  
    -- Analiza incidencias entidades
    if vMul.csl in (0,1) then
        for c3 in cCob3(vMulid) loop
          --vMul.csl:=c.cslid;
          vmul.csl:=100;
          vmul.fcobro:=to_date(c3.dcefcobro,'dd/mm/rrrr');
          vmul.cobrado:=c3.dceimporte;
        end loop;
    end if;

end;


--------------------------------------------------------------------------------
-- Análisis de Liquidaciones
--------------------------------------------------------------------------------
procedure leerLiquidaciones(vMulid number) is

    cursor cCob2(vMulid number) is
    select /*+RULE*/ l.liqid, l.liqnumerorecliquidacion recibo, l.liqxestado estado,
    l.liqfcobro fechacobro
    from alba.liquidaciones l
    where l.expid in
    (select m2.expidexp from alba.multas m2 where m2.seoidexp in
    (select m.seoidexp from alba.multas m where m.mulid=vmulid))
    order by l.liqid asc;

  cursor cLiq(vLiqid number) is    
  select /*+RULE*/ dacuenta cobrado,importe+dacuenta total,importe pendiente
  from table(alba.pkg_calculoimportes.ctd(vLiqid,sysdate));   
  
  vCobrado number:=0;

begin

    vCobrado:=0;

    for c2 in ccob2(vmulid) loop
      if vCobrado=0 then
        vMul.liqid:=c2.liqid;
        vmul.liquidacion:=c2.recibo;
        vmul.ffirmeza:=to_date(c2.fechacobro,'dd/mm/rrrr')+1;
        vmul.estadoliq:=c2.estado;
        vmul.fcobro:=to_date(c2.fechacobro,'dd/mm/rrrr');
        if vmul.estadoliq in (481,480,13) then
          vCobrado:=1;
        end if;
      end if;
    end loop;
    
    for cl in cliq(vmul.liqid) loop
        vmul.total:=cl.total;
        vmul.cobrado:=cl.cobrado;
        vmul.pendiente:=cl.pendiente;
    end loop;
    
end;

--------------------------------------------------------------------------------
-- Establece Estado Cobro
--------------------------------------------------------------------------------
procedure setEstadoCobro is

  vtotal number:=0;
  vtotal2 number:=0;

begin

    -- 20120409 Control de la reducción del 50%
    -- Reducción sobre denuncias cobradas (Comparación con fecha de cobro)
    if vmul.fnotden is null
    or (vmul.fnotden is not null and to_date(vmul.fnotden,'dd/mm/rrrr')+20>=vmul.fcobro) then

          if to_date(vmul2.fechadenuncia,'dd/mm/rrrr')>to_date('24/05/2010','dd/mm/rrrr') then
            vtotal:=(round(vmul2.total*(50/100),2));
          else
            vtotal:=(round(vmul2.total*(70/100),2));
          end if;
    else 
         vtotal:=(round(vmul2.total,2)); 
    end if;
    
    -- Reducción sobre denuncias en trámite y no en ejecutiva (Comparación con fecha de Denuncia
    if vmul.fnotden is null
    or (vmul.fnotden is not null and to_date(vmul.fnotden,'dd/mm/rrrr')+20>=to_date(sysdate,'dd/mm/rrrr')) then
          if to_date(vmul2.fechadenuncia,'dd/mm/rrrr')>to_date('24/05/2010','dd/mm/rrrr') then
            vtotal2:=(round(vmul2.total*(50/100),2));
          else
            vtotal2:=(round(vmul2.total*(70/100),2));
          end if;
    else 
         vtotal2:=(round(vmul2.total,2)); 
    end if;
    
    --Botellón y Convivencia no llevan reducción
    if vmul.origendenuncia in (4,6,7,8) then
        vtotal:=(round(vmul2.total,2));
        vtotal2:=vtotal;
    end if;


    --Estado Cobro 1.Cobrado en Ejecutiva, 2.Cobrado en Procedimiento Sancionador, 3. Finalizado, 4. En Trámite, 5.En Ejecutiva, 6.Baja
    if vmul.csl=10 then
        if vmul.flibsanfir is not null
        and vmul.fcobro is not null
        and vmul.flibsanfir<vmul.fcobro
        then
          vmul.cestadocobro:=4;
          vmul.estadoCobro:='Cobrado en Ejecutiva';
        else
          vmul.cestadocobro:=3;
          vmul.estadocobro:='Cobrado en Procedimiento Sancionador';
          --vmul.total:=vtotal; --En caso de cobros por c60 siempre se cosideran correctos
          vmul.pendiente:=vmul.total-vmul.cobrado;
        end if;
    elsif vmul.csl in (1,100) then
        vmul.cestadocobro:=3;
        vmul.estadocobro:='Cobrado en Procedimiento Sancionador';
        vmul.total:=vtotal;
        vmul.pendiente:=vmul.total-vmul.cobrado;
    elsif vmul.csl=0 then
        if vmul.estadoliq is not null and vmul.estadoliq in (4,5,172) then --ejecutiva, fraccionado
            vmul.cestadocobro:=2;
            vmul.estadoCobro:='En Ejecutiva';
        elsif vmul.estadoliq is not null and vmul.estadoliq in (32,35,170,200) then --baja, suspendido
            vmul.cestadocobro:=6;
            vmul.estadoCobro:='Finalizado por Baja';
        else
            if vmul.cestado in (2320) then --Finalizado automáticamente
                vmul.cestadocobro:=6;
                vmul.estadoCobro:='Finalizado por Baja';
                vmul.total:=vtotal2;
                vmul.pendiente:=vmul.total-vmul.cobrado;       
            elsif vmul.cestado>=40000 and vmul.cestado<=49999 then
                vmul.cestadocobro:=5;
                vmul.estadoCobro:='Finalizado Sin Cobro';
                vmul.total:=vtotal2;
                vmul.pendiente:=vmul.total-vmul.cobrado;  
            elsif vmul.cestado in (30000,30001) then
                vmul.cestadocobro:=3;
                vmul.estadoCobro:='Cobrado en Procedimiento Sancionador';
                vmul.total:=vtotal2;
                vmul.pendiente:=vmul.total-vmul.cobrado;                  
            elsif vmul.cestado>=50000 and vmul.cestado<=67325 then
                vmul.cestadocobro:=6;
                vmul.estadoCobro:='Finalizado por Baja';
                vmul.total:=vtotal2;
                vmul.pendiente:=vmul.total-vmul.cobrado;                  
            else
                vmul.cestadocobro:=1;
                vmul.estadocobro:='En Trámite';
                vmul.total:=vtotal2;
                vmul.pendiente:=vmul.total-vmul.cobrado;
            end if;
        end if;
    end if;
    
    --Expedientes con cobros parciales no se consideran cobrados a efetos de tramitación.
    /*if vmul.csl>0 and vmul.pendiente>0 then
      vmul.csl:=0;
    end if;*/
    
end;


-------------------------------
-- Análisis de Matrícula
-------------------------------
procedure leerMatricula(vMulid number) is

    cursor cMat(vMulid number) is
    select /*+RULE*/  mulid, matricula, marca, tipo from
    (
    select m1.mulid mulid, v1.vehmatri matricula, v1.vehmarca marca, t1.tvtmdes tipo, v1.vehid vehid
    from alba.tivdgt_m t1, alba.vehiculos_d v1, alba.multas m1
    where m1.seoidveh=v1.seoid
    and v1.tvtmid=t1.tvtmid(+)
    and v1.vehversion=
    (select max(vehversion) from alba.vehiculos_d v where v.seoid = m1.SeoIdVeh)
    and m1.mulid=vMulid
    UNION
    select m2.mulid mulid, v2.vemmatri matricula, v2.vemmarca marca, t2.tvtmdes tipo, 1 vehid
    from alba.tivdgt_m t2, alba.vehiculos_mult v2, alba.multas m2
    where m2.vemid=v2.vemid
    and m2.tvtmid=t2.tvtmid(+)
    and m2.mulid=vMulid
    UNION
    select d.mulid, w.wsmatricula matricula, trim(w.wsmarca) || ' ' ||trim(w.wsmodelo) marca, '' tipo, 9999999999 vehid
    from alba.multas mm, alba.datoswsdgt w, alba.det_libro_resol d
    where mm.mulid=d.mulid
    and d.idwsd=w.idwsd
    and mm.expidexp = (select m.expidexp from alba.multas m where m.mulid=vMulid)    
    ) where rownum<2 order by vehid desc;

begin

    for m in cMat(vMulid) loop
        vMul.matricula:=m.matricula;
        vMul.marca:=m.marca;
        vMul.tipo:=m.tipo;
    end loop;
    
end;


-------------------------------
-- Análisis de Personas
-------------------------------
procedure leerPersonas(vMulid number) is

    cursor cPer(vMulid number) is
    select /*+RULE*/ m.mulid, m.mulnumbol, alba.componer_nombre(p.pernombre, p.perapell1, p.perpart1,
        p.perapell2, p.perpart2, p.perrazon) nombre,
        nvl(p.periden, p.peridenext) nif, d.decid, d.decelemento, d.decdescr,
        t.tpeid, t.tpercodigo, t.tpedesc, e.expid, ee.expcod expediente, p.perid, p.perversion
    from alba.expedientesingresoscontr e, alba.decodificadora d, alba.expingre ee,
        alba.tipospersona t, alba.personas p, alba.contribuyentes c, alba.multas m
    where e.eicxtipocontribuyente=d.decid
    and e.conid=c.conid
    and c.perid=p.perid
    and p.tpeid=t.tpeid
    and m.expidexp=e.expid
    and ee.expid=e.expid
    and e.eicfvigencia is null
    and m.mulid=vMulid;
    
begin

    for p in cPer(vMulid) loop
    --vMul.expediente:=p.expediente;
        if p.decelemento in ('WSDGT_ARR','WSDGT_CH','WSDGT_POS','WSDGT_TIT','WSDGT_TRA') then
            vMul.titular:=p.nombre;
            vMul.nift:=p.nif;
            vMul.tpt:=p.tpeid;
            vMul.peridt:=p.perid;
            vMul.peridpt:=p.perversion;
        elsif p.decelemento='TITULAR' then
            vMul.titular:=p.nombre;
            vMul.nift:=p.nif;
            vMul.tpt:=p.tpeid;
            vMul.peridt:=p.perid;
            vMul.peridpt:=p.perversion;            
        elsif p.decelemento='CONDUCTOR' then
            vMul.conductor:=p.nombre;
            vMul.nifc:=p.nif;
            vMul.tpc:=p.tpeid;
            vMul.peridc:=p.perid;
            vMul.peridpc:=p.perversion;
        elsif p.decelemento='TUTOR' then
            vMul.tutor:=p.nombre;
            vMul.niftt:=p.nif;
            vMul.tptt:=p.tpeid;
            vMul.peridtt:=p.perid;
            vMul.peridptt:=p.perversion;
        end if;
    end loop;
    
end;


-------------------------------
-- Análisis de Direcciones
-------------------------------
procedure leerDirecciones(vMulid number) is

    cursor cDir(vMulid number) is
    SELECT /*+RULE*/ rdi.rdiid, alba.componer_direccion
        (acc.accnum1, acc.accinum1, acc.accnum2, acc.accinum2, acc.acckm1,acc.accikm1, acc.accdirnoest,
        rdi.rdixpor, rdi.rdixesc, rdi.rdixpiso, rdi.rdixpuerta, rdi.rdiipuertadup, rdi.rdidirnoest,
        tvi.tvicodigo, via.viadesc, rdi.rdiref3, rdi.rdirefcon1, rdi.rdirefcon2, acc.codid, cod.codposcodigo) domicilio,
        nvl(cod.CODPOSCODIGO, '') cp, mu.mundesc localidad, p.mprdesc provincia,
        decdir.decelemento tipo_direc, decper.decelemento tipo_per
    FROM alba.expedientesingresoscontr eic, alba.direccionesexpedientes ted,
        alba.decodificadora decdir, alba.decodificadora decper, alba.direccion rdi,
        alba.tvia tvi, alba.vias via, alba.accesos acc, alba.municipios mu,
        alba.m_codpostal cod, alba.provincia p, alba.multas m
    WHERE eic.eicxtipocontribuyente = decper.decid
    --AND decper.decelemento = ctipoper
    AND ted.eicid = eic.eicid
    AND ted.tedxtipodireccion = decdir.decid
    --AND decdir.decelemento = ctipodir
    --AND ted.tedfvigencia IS NULL
    AND ted.rdiid = rdi.rdiid
    AND rdi.accid = acc.accid
    AND acc.viaid = via.viaid
    AND via.tviid = tvi.tviid
    AND eic.expid = m.expidexp
    AND via.MunId = mu.MunId
    AND mu.mprid = p.mprid
    AND acc.codid = cod.codid (+)
    and m.mulid=vMulid;

begin

    for d in cDir(vMulid) loop
        if d.tipo_per in ('WSDGT_ARR','WSDGT_CH','WSDGT_POS','WSDGT_TIT','WSDGT_TRA','WSDGT_DIRDEV') then
            vMul.domt:=d.domicilio;
            vMul.loct:=d.localidad;
            vMul.prot:=d.provincia;
            vMul.cpt:=d.cp;    
            vMul.rdiidt:=d.rdiid;
        elsif d.tipo_per='TITULAR' then
            vMul.domt:=d.domicilio;
            vMul.loct:=d.localidad;
            vMul.prot:=d.provincia;
            vMul.cpt:=d.cp;    
            vMul.rdiidt:=d.rdiid;            
        elsif d.tipo_per='CONDUCTOR' then
            vMul.domc:=d.domicilio;
            vMul.locc:=d.localidad;
            vMul.proc:=d.provincia;
            vMul.cpc:=d.cp;
            vMul.rdiidc:=d.rdiid;    
        elsif d.tipo_per='TUTOR' then
            vMul.domtt:=d.domicilio;
            vMul.loctt:=d.localidad;
            vMul.prott:=d.provincia;
            vMul.cptt:=d.cp;    
            vMul.rdiidtt:=d.rdiid;
        end if;
    end loop;
end;


-------------------------------
-- Análisis de Escritos
-------------------------------
procedure leerEscritos(vMulid number) is

    cursor cEsc(vMulid number) is
    select distinct a.algid, m.mulid, m.mulfecalegden, a.algfecha, a.algtipo,
           m.mulfecrecsan, a.algfresolucion, a.algresultado, ra.algid detalleresolucion
    from alba.resoluciones r, alba.resol_alegaciones ra,
         alba.alegacionesrecursos a, alba.multas m
    where m.seoidexp=a.seoid(+)
    --and m.expidexp=a.expid(+)
    and a.algid=ra.algid(+)
    and ra.resid=r.resid(+)
    and m.mulid=vMulid;

    cursor cEsc2(vMulid number) is
    select e.esmid, e.tidomid, e.tdmresid, e.mulid, e.esmnumbol,
    e.esmfescrito, e.esmfestado, e.esmfgrabacion,
    t.tidomnombre escrito, r.tdmnombre estado, count(rsaid) totalResoluciones
    from
    alba.tipos_doc_multas t,
    alba.tipos_doc_multas_res r,
    alba.resol_alegaciones ra,
    alba.escritosmultas e
    where e.tidomid=t.tidomid
    and e.tdmresid=r.tdmresid
    and r.tdmresid not in (25,26,27,32,35,201)
    and e.esmid=ra.esmid(+)
    and e.mulid=vMulid
    and not exists (select 1 from alba.det_libro_resol dl where dl.esmid=e.esmid and dl.lbrid=55837)
    group by e.esmid, e.tidomid, e.tdmresid, e.mulid, e.esmnumbol,
    e.esmfescrito, e.esmfestado, e.esmfgrabacion,
    t.tidomnombre, r.tdmnombre
    order by
    (select count(pntid)
    from alba.propuestasnotificacion p
    where p.pntobjid=e.esmid and pntorigen='I' and p.atoid=817) desc,
    count(rsaid), e.esmid desc;
    
alegacionEstimada number:=0;    

begin

    alegacionEstimada:=0;  
    
    for e in cEsc2(vMulid) loop
    
        if e.tidomid=21 then -- Escrito identificando al conductor
          vmul.fecomcon:=to_date(e.esmfescrito,'dd/mm/rrrr');
        end if;
    
        if e.tidomid=1 and alegacionEstimada=0 then -- ALEGACIÓN
            if e.tdmresid=1 and e.totalresoluciones>0 then
                vMul.aleg:=2; -- Desestimada
            elsif e.tdmresid=2 and e.totalResoluciones>0 then
                vmul.aleg:=3; -- Estimada
                alegacionEstimada:=1;
            elsif e.tdmresid=321 then
                vMul.aleg:=0; -- Extemporanea
            elsif e.tdmresid=361 then
                vMul.aleg:=0; -- No presentada
            else
                vMul.aleg:=1; -- Pendiente de resolución
            end if;
            
            if e.tdmresid not in (321,361) then --Considera alegación si no es Extemporanea ni No Presentada
              vMul.faleg:=to_date(e.esmfescrito,'dd/mm/rrrr');
              vmul.alegesmid:=e.esmid;
              
              -- Fecha de Notificación de Denuncia es la Fecha del Escrito
              vMul.fnotden:=to_date(e.esmfescrito,'dd/mm/rrrr');              
            end if;
            

            
        elsif e.tidomid=2 then -- RECURSO
            if e.tdmresid=3 and e.totalResoluciones>0 then
                vMul.rec:=3; -- Estimado
            elsif e.tdmresid=4 and e.totalResoluciones>0 then
                vMul.rec:=2; -- Desestimado
            else
                vMul.rec:=1; -- Pendiente de resolución
            end if;
            vMul.frec:=to_date(e.esmfescrito,'dd/mm/rrrr');
            vMul.recesmid:=e.esmid;     
            
            -- Fecha de Notificación de Sanción es la Fecha del Escrito
            vMul.fnotsan:=to_date(e.esmfescrito,'dd/mm/rrrr');

        elsif e.tidomid=3 then -- RECURSO EXTRAORDINARIO DE REVISIÓN
            if e.tdmresid=5 and e.totalResoluciones>0 then
                vMul.rer:=3; -- Estimado
            elsif e.tdmresid=6 and e.totalResoluciones>0 then
                vMul.rer:=2; -- Desestimado
            else
                vMul.rer:=1; -- Pendiente de resolución
            end if;
            vMul.frer:=to_date(e.esmfescrito,'dd/mm/rrrr');
            vMul.reresmid:=e.esmid;
            
            -- Fecha de Notificación de Sanción es la Fecha del Escrito
            --vMul.fnotrec:=to_date(e.esmfescrito,'dd/mm/rrrr');
            
            
        elsif e.tidomid=41 then -- RECURSO PROVIDENCIA DE APREMIO
            if e.tdmresid=62 and e.totalResoluciones>0 then
                vMul.rpa:=3; -- Estimado
            elsif e.tdmresid=61 and e.totalResoluciones>0 then
                vMul.rpa:=2; -- Desestimado
            else
                vMul.rpa:=1; -- Pendiente de resolución
            end if;
            vMul.frpa:=to_date(e.esmfescrito,'dd/mm/rrrr');
            vMul.rpaesmid:=e.esmid;     

            
        elsif e.tidomid=101 then -- REA
            if e.tdmresid in (261,385) and e.totalResoluciones>0 then
                vMul.rea:=3; -- Estimado
            elsif e.tdmresid in (281,401) and e.totalResoluciones>0 then
                vMul.rea:=2; -- Desestimado
            else
                vMul.rea:=1; -- Pendiente de resolución
            end if;
            vmul.frea:=to_date(e.esmfescrito,'dd/mm/rrrr');
            vMul.reaesmid:=e.esmid;     

            
        elsif e.tidomid=82 then -- REQUERIMIENTO JUZGADO
            if e.tdmresid=101 then
                vMul.rej:=1; -- Suspendido con caución
            elsif e.tdmresid=102 then
                vMul.rej:=2; -- Suspendido sin caución
            else
                vMul.rej:=3; -- Levantado
            end if;
            vMul.frej:=to_date(e.esmfestado,'dd/mm/rrrr');
            vMul.rejesmid:=e.esmid;     

            -- Fecha de Notificación de Sanción es la Fecha del Escrito
            vMul.fnotsan:=to_date(e.esmfestado,'dd/mm/rrrr');
            
        elsif e.tidomid=61 then -- BAJA
            if e.tdmresid=81 and e.totalResoluciones>0 then
                vMul.baja:=2; -- Tramitada
            else
                vMul.baja:=1; -- Pendiente de definición
            end if;
            vMul.frbaja:=to_date(e.esmfestado,'dd/mm/rrrr');
            vMul.bajaesmid:=e.esmid;     

        elsif e.tidomid=6 then -- INFORME DENUNCIANTE
            if e.tdmresid=28 then
                vMul.infd:=2; -- Solicitado
                vMul.finfd:=to_date(e.esmfgrabacion,'dd/mm/rrrr');
                vMul.infdesmid:=e.esmid;
            elsif e.tdmresid=29 then -- Recibido
                vMul.infd:=0;
                vMul.finfd:=to_date(e.esmfgrabacion,'dd/mm/rrrr');
                vMul.frinfd:=to_date(e.esmfestado,'dd/mm/rrrr');
                vMul.infdesmid:=e.esmid;
            end if;

        elsif e.tidomid in (7,8) then -- REQUERIMIENTO 10 DIAS
            if e.tdmresid in (31,34) and e.totalResoluciones>0 then
                vMul.infq:=2; -- Solicitados
            else
                vMul.infq:=1; -- Pendiente de resolución
            end if;
            vMul.finfq:=to_date(e.esmfestado,'dd/mm/rrrr');
            vMul.infqesmid:=e.esmid;
            if e.tidomid=7 then
                vMul.tipoinfq:=10;
            else
                vMul.tipoinfq:=15;
            end if;
            
        elsif e.tidomid in (205,221) then -- REQUERIMIENTO DENUNCIA, DENUNCIANTE VOL
            if e.tdmresid in (581,601) and e.totalResoluciones>0 then
                vMul.infq:=2; -- Solicitados
            elsif e.tdmresid in (582,602) then --Recibido
                vMul.infq:=0; -- Una vez recibido no tiene efecto
            else
                vMul.infq:=1; -- Pendiente de resolución
            end if;
            vMul.finfq:=to_date(e.esmfestado,'dd/mm/rrrr');
            vMul.infqesmid:=e.esmid;
            if e.tidomid=205 then
                vMul.tipoinfq:=20;
            else
                vmul.tipoinfq:=22;
            end if;            

        elsif e.tidomid in (9,21) then -- PARALIZADO

            
            vMul.aleg:=1;
            vMul.faleg:=to_date(e.esmfescrito,'dd/mm/rrrr');
            vMul.alegesmid:=e.esmid;

            vMul2.vParalizado:=e.tidomid;
            
            if e.tdmresid<>323 then --NO es identificación extemporanea
              vMul2.vIdConductor:=e.tdmresid;
            else
              vMul2.vParalizado:=0;
              vMul2.vIdConductor:=0;
              vMul.aleg:=0;
              vMul.faleg:=null;
              vMul.alegesmid:=0;
            end if;
            
        end if;

    end loop;
end;



-------------------------------
-- Análisis de Libros
-------------------------------
procedure leerLibros(vMulid number) is

    cursor cLib(vMulid number) is
    select l.lbrfecha, l.lbrid, t.lrtdescorta,
           decode(t.lrtrequierefirma,'N',l.lbrfecha,l.lbrfirma) lbrfirma,
           l.lbrnumero numeroLibro, l.lbrfirma fresolucionLibro, d.esmid, d.mulid
    from alba.libro_resol_tipos t, alba.libro_resoluciones l, alba.det_libro_resol d
    where d.lbrid=l.lbrid
    and l.lrtid=t.lrtid
    and d.mulid=vMulid
    order by l.lbrid asc;
    
vEsmidAnterior number:=0;
vMulidAnterior number:=0;

begin

    vMul2.vEscritoDuplicado:=false;

    for l in cLib(vMulid) loop

        if l.lbrfirma is not null then
            vMul2.vTemp:=2;
            vMul2.vFtemp:=to_date(l.lbrfirma,'dd/mm/RRRR');
        else
            vMul2.vTemp:=1;
            vMul2.vFtemp:=null;
        end if;

        if l.lrtdescorta in ('31','33','34','37','38','314','315','316','317','318','319','3110') then -- Sanción
            vMul.libsan:=vMul2.vTemp;
            vmul.flibsan:=vmul2.vftemp;
        elsif l.lrtdescorta in ('41','414','415','416','417','418','515','514','516','517','5110','4110') then -- Alegación
            vmul.libaleg:=vmul2.vtemp;
            vMul.flibaleg:=vMul2.vFtemp;
            
            -- Control de Escrito Duplicado
            if vEsmidAnterior>0
            and vMulidAnterior>0
            and vEsmidAnterior=l.esmid
            and vMulidAnterior=l.mulid then
                vMul2.vEscritoDuplicado:=true;
            end if;
            vEsmidAnterior:=l.esmid;
            vMulidAnterior:=l.mulid;

        elsif l.lrtdescorta in ('42','424','425','426','524','525','526') then -- Recurso
            vMul.librec:=vMul2.vTemp;
            vMul.flibrec:=vMul2.vFtemp;
            
            -- Control de Escrito Duplicado
            if vEsmidAnterior>0
            and vMulidAnterior>0
            and vEsmidAnterior=l.esmid
            and vMulidAnterior=l.mulid then
                vMul2.vEscritoDuplicado:=true;
            end if;
            vEsmidAnterior:=l.esmid;
            vMulidAnterior:=l.mulid;
            
        elsif l.lrtdescorta in ('43','435','53','535') then -- Recurso Extraordinario de Revisión
            vMul.librer:=vMul2.vTemp;
            vMul.flibrer:=vMul2.vFtemp;
            
            -- Control de Escrito Duplicado
            if vEsmidAnterior>0
            and vMulidAnterior>0
            and vEsmidAnterior=l.esmid
            and vMulidAnterior=l.mulid then
                vMul2.vEscritoDuplicado:=true;
            end if;
            vEsmidAnterior:=l.esmid;
            vMulidAnterior:=l.mulid;
            
        elsif l.lrtdescorta in ('105') then -- Consulta de Datos
            --vMul.fconsultaDatos:=null; --Fecha de incorporación de datos
            vMul.libcondat:=l.numerolibro;
            vMul.flibcondat:=l.lbrfecha; --Fecha libro incorporación de datos            
            
        elsif l.lrtdescorta in ('11','13','14','16','17','18','19','22','114','115','135','145','165','116','117','118','119','1110','1410','1610') then -- Incoación
            vMul.libinc:=vMul2.vTemp;
            vMul.flibinc:=vMul2.vFtemp;
        elsif l.lrtdescorta in ('21','215') then -- Req
            vMul.libreq:=vMul2.vTemp;
            vmul.flibreq:=vmul2.vftemp;
        elsif l.lrtdescorta in ('50','235','505','504','506','507','5010') then -- Baja
            vMul.libbaja:=vMul2.vTemp;
            vMul.flibbaja:=vMul2.vFtemp;
        elsif l.lrtdescorta in ('44','45','46') then -- Informe Denunciante
            vMul.libinfd:=vMul2.vTemp;
            vmul.flibinfd:=vmul2.vftemp;
        elsif l.lrtdescorta in ('47','125','155','126','124','154','156','157','1510') then -- Informe Requerimiento
            vMul.libinfq:=vMul2.vTemp;
            vmul.flibinfq:=vmul2.vftemp;
        elsif l.lrtdescorta in ('49','495','494','496','497','4910') then -- Sanción Firme
            vMul.libSanFir:=vMul2.vTemp;
            vMul.flibSanFir:=vMul2.vFtemp;
        elsif l.lrtdescorta in ('91','915','914','916','917','9110') then -- BOP denuncia
            vMul.libbopden:=vMul2.vTemp;
            vMul.flibbopden:=vMul2.vFtemp;
            if vMul2.vFtemp is not null and trunc(vMul2.vFtemp)<=trunc(sysdate) then -- Libro BOP Firmado » Fecha Notificación Denuncia = Fecha Firma
                vMul.fnotden:=vMul2.vFtemp;
            end if;
        elsif l.lrtdescorta in ('92','925','924','926') then -- BOP requerimiento
            vMul.libbopreq:=vMul2.vTemp;
            vMul.flibbopreq:=vMul2.vFtemp;
            if vMul2.vFtemp is not null then -- Libro BOP Firmado » Fecha Notificación Requerimiento = Fecha Firma
                vMul.fnotreq:=vMul2.vFtemp;
            end if;
        elsif l.lrtdescorta in ('93','935','934','936','937','9310') then -- BOP sanción
            vMul.libbopsan:=vMul2.vTemp;
            vMul.flibbopsan:=vMul2.vFtemp;
            if vMul2.vFtemp is not null then -- Libro BOP Firmado » Fecha Notificación Sanción = Fecha Firma
                vMul.fnotsan:=vMul2.vFtemp;
            end if;
        elsif l.lrtdescorta in ('94','945','944','946') then -- BOP r.alegaciones
            vMul.libbopaleg:=vMul2.vTemp;
            vMul.flibbopaleg:=vMul2.vFtemp;
            if vMul2.vFtemp is not null then -- Libro BOP Firmado » Fecha Notificación Resol. Alegaciones y Sanción = Fecha Firma
                vMul.fnotsan:=vMul2.vFtemp;
                vMul.fnotaleg:=vMul2.vFtemp;
            end if;
        elsif l.lrtdescorta in ('95','955','954','956') then -- BOP r.recursos
            vMul.libboprec:=vMul2.vTemp;
            vMul.flibboprec:=vMul2.vFtemp;
            if vMul2.vFtemp is not null then -- Libro BOP Firmado » Fecha Notificación Resol. Recursos = Fecha Firma
                vMul.fnotrec:=vMul2.vFtemp;
            end if;
        elsif l.lrtdescorta in ('96','965','964','966') then -- BOP r.rec.ext.rev
            vMul.libboprer:=vMul2.vTemp;
            vMul.flibboprer:=vMul2.vFtemp;
            if vMul2.vFtemp is not null then -- Libro BOP Firmado » Fecha Notificación Resol. Rec. Ext. Rev. = Fecha Firma
                vMul.fnotrer:=vMul2.vFtemp;
            end if;
        elsif l.lrtdescorta='70' then -- REA
            vMul.librea:=vMul2.vTemp;
            vMul.flibrea:=vMul2.vFtemp;
           
        elsif l.lrtdescorta in ('55','57') and l.esmid=vMul.rpaesmid then -- Recurso Providencia de Apremio
            vMul.librpa:=vMul2.vTemp;
            vMul.flibrpa:=vMul2.vFtemp;
            vMul.numeroLibroApremio:=to_char(l.numeroLibro);
            vMul.fResolucionLibroApremio:=to_char(trunc(l.fResolucionLibro));

            -- Si el escrito incluido en el libro no coincide con el escrito pendiente
            -- deja el estado del libro a 0 para que el expediente pueda volver a incluirse en
            -- un nuevo libro de resolución
            /*if l.esmid<>vMul.rpaesmid then
                vMul.librpa:=0;
                vMul.flibrpa:=null;
                vMul.numeroLibroApremio:=null;
                vMul.fResolucionLibroApremio:=null;
            end if;*/

            -- Control de Escrito Duplicado
            if vEsmidAnterior>0
            and vMulidAnterior>0
            and vEsmidAnterior=l.esmid
            and vMulidAnterior=l.mulid then
                vMul2.vEscritoDuplicado:=true;
            end if;
            vEsmidAnterior:=l.esmid;
            vMulidAnterior:=l.mulid;

            
        end if;
    end loop;
end;


-------------------------------
-- Análisis de Notificaciones
-------------------------------
procedure leerNotificaciones(vMulid number) is

    cursor cNot(vMulid number) is
        select /*+RULE*/ * from
        (

                select p.pntid,n.ntfid,r.remid,c.crmid,a.atoid,
                    p.perid,p.perversion,p.rdiid,
                    c.crmfecha,c.crmnumero,n.ntfresultado,n.ntffecnot,a.atocodigo, 0 esmid
                from alba.actosnotificacion a, alba.cabremesas c, alba.remesas r,
                    alba.notificaciones n, alba.propuestasnotificacion p
                where p.pntid=n.pntid
                and n.ntfid=r.ntfid
                and r.crmid=c.crmid
                and p.atoid=a.atoid
                and p.pntorigen='I'
                and n.ntfcrmid=c.crmid
                and p.pntobjid=vMulid
            union
                select p.pntid,n.ntfid,r.remid,c.crmid,a.atoid,
                    p.perid,p.perversion,p.rdiid,
                    c.crmfecha,c.crmnumero,n.ntfresultado,n.ntffecnot,a.atocodigo, p.pntobjid esmid
                from alba.actosnotificacion a, alba.cabremesas c, alba.remesas r,
                    alba.notificaciones n, alba.propuestasnotificacion p
                where p.pntid=n.pntid
                and n.ntfid=r.ntfid
                and r.crmid=c.crmid
                and p.atoid=a.atoid
                and p.pntorigen='I'
                and n.ntfcrmid=c.crmid
                and p.pntorigenid=vMulid
            --DEV
            union
              select 
              n.pnnid pntid,
              n.pnnid ntfid,
              n.pnnid remid,
              n.pnnid crmid,
              a.atoid,
              n.perid,
              n.perversion,
              n.rdiid,
              n.pnnfprop crmfecha,
              n.pnnid crmnumero,
              decode(n.PNNFNOTIFICACION,null,null,
                  decode(n.pnnacuse,'LE','M3','CD','M5','M3')      ) ntfresultado,
              to_date(n.PNNFNOTIFICACION,'dd/mm/rrrr') ntffecnot,
              a.atocodigo,
              0 esmid
              --select *
              from alba.propuestasnotifnostradev n,
              alba.actosnotificacion a
              where n.atoid=a.atoid
              and n.mulid=vMulid

        )
        order by ntfid asc;
        
        cursor cNotPA(vMulid number) is
        select /*+RULE*/ p.pntid,n.ntfid,r.remid,c.crmid,a.atoid,c.crmtipo,
            c.crmfecha,c.crmnumero,n.ntfresultado,n.ntffecnot,a.atocodigo,
            c.crmedicto, c.crmboletin, l.liqid, l.liqnumerorecliquidacion recibo,
            to_char(round(l.liqimporte,2)) importeLiqTexto,
            round(l.liqimporte,2) importeLiqNumero,
            to_char(l.liqfsancionfirme,'dd/mm/rrrr') fsanfir, to_char(l.liqffinperiodovol,'dd/mm/rrrr') ffinvol,
            l.liqteorvtoeje flimitepago, l.liqxestado estadoLiq, p.pntobjid esmid
        from alba.actosnotificacion a,
             alba.cabremesas c,
             alba.remesas r,
             alba.liquidaciones l,
             alba.multas m2,
             alba.notificaciones n,
           alba.propuestasnotificacion p
        where n.pntid=p.pntid
        and n.ntfid=r.ntfid
        and r.crmid=c.crmid
        and p.atoid=a.atoid
        and p.pntorigen='L'
        and n.ntfcrmid=r.crmid
        and m2.expidexp=l.expid
        and p.pntorigenid=l.liqid
        and p.atoid in (57,77)
        --and n.ntfresultado is not null
        and m2.mulid=vMulid
        --in (select mulid from alba.expingre e, alba.multas m where e.expid=m.expidexp and e.expcod=vExp and e.mexid=21)
        order by n.ntfid asc;

                
        cursor cActEje(vLiqid number) is
        select /*+RULE*/ ae.aejcodigo rd, to_char(pf.pyffdocu,'dd/mm/rrrr') finieje
        from alba.actuacionesejecutiva ae, alba.detallesactuacionesejecu de,
        alba.pliegosfacturas pf, alba.detallepliegosfras df,
        alba.liquidaciones l
        where de.aejid=ae.aejid
        and ae.dacid=58
        and de.liqid=l.liqid
        and df.pyfid=pf.pyfid
        and pf.ormid=2
        and pf.org_ormid=2
        and pf.pyfperiodo='E'
        and pf.pyftipodocu='P'
        and pf.pyfsubtipo='P'
        and pf.pyfdocanulado is null
        and pf.pyfxestado=93     
        and df.liqid=l.liqid
        and l.liqid=vLiqid
        order by 1 asc;
        
begin

    for n in cNotPA(vMulid) loop
        vMul2.vAtocodigo:=n.atocodigo;
        vMul.liqid:=n.liqid;
        vMul.liquidacion:=n.recibo;
        vMul.importeLiqTexto:=n.importeLiqTexto;
        vMul.importeLiqNum:=n.importeLiqNumero;
        vMul.estadoLiq:=n.estadoLiq;
        
        vMul.fSancionFirmeLiq:=n.fsanfir;
        vMul.fFinVoluntariaLiq:=n.ffinvol;

        for ae in cActEje(n.liqid) loop

            vMul.fInicioEjecutivaLiq:=ae.finieje;

        end loop;

        -------------------------------------
        -- Providencia de Apremio
        -------------------------------------
        if n.atocodigo in ('NOAPRE') then
            ------------------------------------
            if n.pntid is not null and n.crmid is null then
                vMul.notpa:=1; -- Existe propuesta
            elsif n.pntid is not null and n.crmid is not null and n.crmnumero is null then
                vMul.notpa:=2; -- Remesa creada
            elsif n.pntid is not null and n.crmid is not null and n.crmnumero is not null and n.ntfresultado is null then
                vMul.notpa:=3; -- Remesa emitida
            elsif n.pntid is not null and n.crmid is not null and n.crmnumero is not null and n.ntfresultado is not null then
                ------------------------------------------------------
                if (n.ntfresultado='A1' or n.ntfresultado='A8' or n.ntfresultado='NP') then
                    vMul.notpa:=4;
                    vMul.fnotpa:=n.ntffecnot;
                elsif (n.ntfresultado='B1') then
                    vMul.notpa:=4;
                    vMul.fnotpa:=n.ntffecnot;
                    vMul.edictoApremio:=n.crmedicto;
                else
                    vMul.notpa:=3;
                end if;
                -------------------------------------------------------
            end if;
            
            if n.crmfecha is not null and n.crmtipo='C' then
              vMul.fnotinfq:=n.crmfecha;
            end if;
            
        end if;
    end loop;



    for n in cNot(vMulid) loop
        vMul2.vAtocodigo:=n.atocodigo;
        
        -------------------------------------
        -- denuncia
        -------------------------------------
        if n.atocodigo in ('MULTDN','MULTDP','MULTDT','MULTDU','MULTDE','MULTDM','MULTDA','MULTDV','MULTDR','DENFOT') then
            vMul.notdenrdiid:=n.rdiid;
            vMul.notdenperid:=n.perid;
            vMul.notdenperv:=n.perversion;
            vMul.notdenacto:=n.ntfresultado; --n.atoid;
            ------------------------------------
            if n.pntid is not null and n.crmid is null then
                vMul.notden:=1; -- Existe propuesta
            elsif n.pntid is not null and n.crmid is not null and n.crmnumero is null then
                vMul.notden:=2; -- Remesa creada
            elsif n.pntid is not null and n.crmid is not null and n.crmnumero is not null and n.ntfresultado is null then
                vMul.notden:=3; -- Remesa emitida
            elsif n.pntid is not null and n.crmid is not null and n.crmnumero is not null and n.ntfresultado is not null then
                ------------------------------------------------------
                if (n.ntfresultado='M3' or n.ntfresultado='M4' or n.ntfresultado='M2') then
                    vMul.notden:=4;
                    --if vMul.fnotden is null then
                        vMul.fnotden:=n.ntffecnot;
                    --end if;
                elsif (n.ntfresultado='M5') then -- AUSENTE pasa a estado publicación BOP
                    vMul.notden:=5;
                    vMul.fnotdenIntento:=n.ntffecnot;
                elsif (n.ntfresultado='M1') then --
                
                    if n.atocodigo in ('MULTDU') then
                      vMul.notden:=6; -- 6, si no queremos que se publique DESCONOCIDO (DIRECCIÓN INCORRECTA O INCOMPLETA)
                    else
                      vMul.notden:=5;
                    end if;
                    vMul.fnotdenIntento:=n.ntffecnot;
                else
                    vMul.notden:=3;
                end if;
                -------------------------------------------------------
            end if;
        end if;
        
        
        -------------------------------------
        -- sancion
        --------------------------------------
        if n.atocodigo in ('MULTSN','MULTSA','MULTSV','MULTDO') then
                vMul.notsanrdiid:=n.rdiid;
                vMul.notsanperid:=n.perid;
                vMul.notsanperv:=n.perversion;
                vMul.notsanacto:=n.ntfresultado;--n.atoid;
                -------------------------------------
                if n.pntid is not null and n.crmid is null then
                        vMul.notsan:=1; -- Existe propuesta
                elsif n.pntid is not null and n.crmid is not null and n.crmnumero is null then
                        vMul.notsan:=2; -- Remesa creada
                elsif n.pntid is not null and n.crmid is not null and n.crmnumero is not null and n.ntfresultado is null then
                           vMul.notsan:=3; -- Remesa emitida
                elsif n.pntid is not null and n.crmid is not null and n.crmnumero is not null and n.ntfresultado is not null then
                -------------------------------------
                    if (n.ntfresultado='M3' or n.ntfresultado='M4' or n.ntfresultado='M2') then
                        vMul.notsan:=4;
                        --if vMul.fnotsan is null then
                                vMul.fnotsan:=n.ntffecnot;
                        --end if;
                    elsif (n.ntfresultado='M5') then
                        vMul.notsan:=5;
                        vMul.fnotsanIntento:=n.ntffecnot;
                    elsif (n.ntfresultado='M1') then
                        vMul.notsan:=5;
                        vMul.fnotsanIntento:=n.ntffecnot;
                    else
                        vMul.notsan:=3;
                    end if;
                -------------------------------------
                end if;
                -------------------------------------
        end if;
        

      -------------------------------------
        -- requerimiento
        -------------------------------------
        if n.atocodigo in ('MULTDC','MULFOT') then
            vMul.notreqrdiid:=n.rdiid;
            vMul.notreqperid:=n.perid;
            vMul.notreqperv:=n.perversion;
            vMul.notreqacto:=n.ntfresultado; --n.atoid;
            if n.pntid is not null and n.crmid is null then
                vMul.notreq:=1; -- Existe propuesta
            elsif n.pntid is not null and n.crmid is not null and n.crmnumero is null then
                vMul.notreq:=2; -- Remesa creada
            elsif n.pntid is not null and n.crmid is not null and n.crmnumero is not null and n.ntfresultado is null then
                vMul.notreq:=3; -- Remesa emitida
            elsif n.pntid is not null and n.crmid is not null and n.crmnumero is not null and n.ntfresultado is not null then
                if (n.ntfresultado='M3' or n.ntfresultado='M4' or n.ntfresultado='M2') then
                    vMul.notreq:=4;
                    --if vMul.fnotreq is null then
                            vMul.fnotreq:=n.ntffecnot;
                    --end if;
                elsif (n.ntfresultado='M5') then
                    vMul.notreq:=5;
                    vMul.fnotreqIntento:=n.ntffecnot;
                elsif (n.ntfresultado='M1') then
                    vMul.notreq:=5;
                    vMul.fnotreqIntento:=n.ntffecnot;                    
                else
                    vMul.notreq:=3;
                end if;
            end if;

        end if;

        
        -------------------------------------
        -- alegación
        -------------------------------------
        if n.atocodigo in ('MULTDG','M51') then
            vMul.notalegrdiid:=n.rdiid;
            vMul.notalegperid:=n.perid;
            vMul.notalegperv:=n.perversion;
            vMul.notalegacto:=n.ntfresultado; --n.atoid;                          
            if n.pntid is not null and n.crmid is null then
                vMul.notaleg:=1; -- Existe propuesta
            elsif n.pntid is not null and n.crmid is not null and n.crmnumero is null then
                    vMul.notaleg:=2; -- Remesa creada
            elsif n.pntid is not null and n.crmid is not null and n.crmnumero is not null and n.ntfresultado is null then
                    vMul.notaleg:=3; -- Remesa emitida
            elsif n.pntid is not null and n.crmid is not null and n.crmnumero is not null and n.ntfresultado is not null then
                    if (n.ntfresultado='M3' or n.ntfresultado='M4' or n.ntfresultado='M2') then
                        vMul.notaleg:=4;
                        --if vMul.fnotaleg is null then
                                vMul.fnotaleg:=n.ntffecnot;
                        --end if;
                        --if vMul.fnotsan is null then
                                vMul.fnotsan:=n.ntffecnot;
                        --end if;
                    elsif (n.ntfresultado='M5') then
                        vMul.notaleg:=5;
                        vMul.fnotalegIntento:=n.ntffecnot;
                        vMul.fnotsanIntento:=n.ntffecnot;
                    elsif (n.ntfresultado='M1') then
                        vMul.notaleg:=5;
                        vMul.fnotalegIntento:=n.ntffecnot;
                        vMul.fnotsanIntento:=n.ntffecnot;
                    else
                        vMul.notaleg:=3;
                    end if;
           end if;
        end if;


        -------------------------------------
        -- recurso
        -------------------------------------        
        if n.atocodigo in ('MULTDX','M52') then
            vMul.notrecrdiid:=n.rdiid;
            vMul.notrecperid:=n.perid;
            vMul.notrecperv:=n.perversion;
            vMul.notrecacto:=n.ntfresultado; --n.atoid;                          
            if n.pntid is not null and n.crmid is null then
                vMul.notrec:=1; -- Existe propuesta
            elsif n.pntid is not null and n.crmid is not null and n.crmnumero is null then
                vMul.notrec:=2; -- Remesa creada
            elsif n.pntid is not null and n.crmid is not null and n.crmnumero is not null and n.ntfresultado is null then
                vMul.notrec:=3; -- Remesa emitida
            elsif n.pntid is not null and n.crmid is not null and n.crmnumero is not null and n.ntfresultado is not null then
                if (n.ntfresultado='M3' or n.ntfresultado='M4' or n.ntfresultado='M2') then
                    vMul.notrec:=4;
                    --if vMul.fnotrec is null then
                            vMul.fnotrec:=n.ntffecnot;
                    --end if;
                elsif (n.ntfresultado='M5') then
                    vMul.notrec:=5;
                    vMul.fnotrecIntento:=n.ntffecnot;
                elsif (n.ntfresultado='M1') then
                    vMul.notrec:=5;
                    vMul.fnotrecIntento:=n.ntffecnot;                        
                else
                    vMul.notrec:=3;
                end if;
            end if;
        end if;
        
        
        -------------------------------------
        -- Recurso Providencia de Apremio
        -------------------------------------        
        if n.atocodigo='MULRPA' and n.esmid=vMul.rpaesmid then
            vMul.notrpardiid:=n.rdiid;
            vMul.notrpaperid:=n.perid;
            vMul.notrpaperv:=n.perversion;
            vMul.notrpaacto:=n.ntfresultado; --n.atoid;                          
            if n.pntid is not null and n.crmid is null then
                vMul.notrpa:=1; -- Existe propuesta
            elsif n.pntid is not null and n.crmid is not null and n.crmnumero is null then
                vMul.notrpa:=2; -- Remesa creada
            elsif n.pntid is not null and n.crmid is not null and n.crmnumero is not null and n.ntfresultado is null then
                vMul.notrpa:=3; -- Remesa emitida
            elsif n.pntid is not null and n.crmid is not null and n.crmnumero is not null and n.ntfresultado is not null then
                ------------------------------------------------------
                if (n.ntfresultado='A1' or n.ntfresultado='A8' or n.ntfresultado='NP') then
                    vMul.notrpa:=4;
                    vMul.fnotrpa:=n.ntffecnot;
                elsif (n.ntfresultado='B1') then
                    vMul.notrpa:=4;
                    vMul.fnotrpa:=n.ntffecnot;
                else
                    vMul.notrpa:=3;
                end if;
                -------------------------------------------------------
            end if;
        end if;

        -- Si el último escrito es distinto del notificado y al menos se ha emitido la notificación
        -- deja el estado de la notificación a 0 para que el expediente pueda volver a incluirse en
        -- un nuevo libro de resolución
       
        
        if n.esmid<>vMul.rpaesmid and vMul.notrpa=3 then
            vMul.notrpa:=0;
            vMul.fnotrpa:=null;
        end if;


        -------------------------------------
        -- REA
        -------------------------------------        
        if n.atocodigo='REA' then
            vMul.notreardiid:=n.rdiid;
            vMul.notreaperid:=n.perid;
            vMul.notreaperv:=n.perversion;
            vMul.notreaacto:=n.ntfresultado; --n.atoid;                          
            if n.pntid is not null and n.crmid is null then
              vMul.notrea:=1; -- Existe propuesta
                elsif n.pntid is not null and n.crmid is not null and n.crmnumero is null then
              vMul.notrea:=2; -- Remesa creada
                elsif n.pntid is not null and n.crmid is not null and n.crmnumero is not null and n.ntfresultado is null then
              vMul.notrea:=3; -- Remesa emitida
            elsif n.pntid is not null and n.crmid is not null and n.crmnumero is not null and n.ntfresultado is not null then
                    ------------------------------------------------------
                    if (n.ntfresultado='A1' or n.ntfresultado='A8' or n.ntfresultado='NP') then
                        vMul.notrea:=4;
                        vMul.fnotrea:=n.ntffecnot;
                    elsif (n.ntfresultado='B1') then
                        vMul.notrea:=4;
                        vMul.fnotrea:=n.ntffecnot;
                    else
                        vMul.notrea:=3;
                    end if;
                    -------------------------------------------------------
            end if;
        end if;



        -------------------------------------
        -- recurso extraordinario de revisión
        -------------------------------------        
        if n.atocodigo in ('MULTRR','M53') then
            vMul.notrerrdiid:=n.rdiid;
            vMul.notrerperid:=n.perid;
            vMul.notrerperv:=n.perversion;
            vMul.notreracto:=n.ntfresultado; --n.atoid;                          
            if n.pntid is not null and n.crmid is null then
                vMul.notrer:=1; -- Existe propuesta
            elsif n.pntid is not null and n.crmid is not null and n.crmnumero is null then
                vMul.notrer:=2; -- Remesa creada
            elsif n.pntid is not null and n.crmid is not null and n.crmnumero is not null and n.ntfresultado is null then
                vMul.notrer:=3; -- Remesa emitida
            elsif n.pntid is not null and n.crmid is not null and n.crmnumero is not null and n.ntfresultado is not null then
                if (n.ntfresultado='M3' or n.ntfresultado='M4' or n.ntfresultado='M2') then
                    vMul.notrer:=4;
                    --if vMul.fnotrer is null then
                            vMul.fnotrer:=n.ntffecnot;
                    --end if;
                elsif (n.ntfresultado='M5') then
                    vMul.notrer:=5;
                    vMul.fnotrerIntento:=n.ntffecnot;
                elsif (n.ntfresultado='M1') then
                    vMul.notrer:=5;
                    vMul.fnotrerIntento:=n.ntffecnot;                        
                else
                    vMul.notrer:=3;
                end if;
            end if;
        end if;


        -------------------------------------
        -- informe requerimiento
        -------------------------------------        
        if n.atocodigo='MULTRQ' then
            vMul.notinfqrdiid:=n.rdiid;
            vMul.notinfqperid:=n.perid;
            vMul.notinfqperv:=n.perversion;
            vMul.notinfqacto:=n.ntfresultado; --n.atoid;                          
            if n.pntid is not null and n.crmid is null then
                vMul.notinfq:=1; -- Existe propuesta
            elsif n.pntid is not null and n.crmid is not null and n.crmnumero is null then
                vMul.notinfq:=2; -- Remesa creada
            elsif n.pntid is not null and n.crmid is not null and n.crmnumero is not null and n.ntfresultado is null then
                vMul.notinfq:=3; -- Remesa emitida
            elsif n.pntid is not null and n.crmid is not null and n.crmnumero is not null and n.ntfresultado is not null then
                if (n.ntfresultado='M3' or n.ntfresultado='M4' or n.ntfresultado='M2') then
                    vMul.notinfq:=4;
                    --if vMul.fnotinfq is null then
                            --vMul.fnotinfq:=n.ntffecnot;
                    --end if;
                else
                    vMul.notinfq:=3;
                end if;
            end if;
        end if;


        -------------------------------------
        -- informe denunciante
        -------------------------------------
        if n.atocodigo='MULTRQ' then
            vMul.notinfdrdiid:=n.rdiid;
            vMul.notinfdperid:=n.perid;
            vMul.notinfdperv:=n.perversion;
            vMul.notinfdacto:=n.ntfresultado; --n.atoid;                          
            if n.pntid is not null and n.crmid is null then
                vMul.notinfd:=1; -- Existe propuesta
            elsif n.pntid is not null and n.crmid is not null and n.crmnumero is null then
                vMul.notinfd:=2; -- Remesa creada
            elsif n.pntid is not null and n.crmid is not null and n.crmnumero is not null and n.ntfresultado is null then
                vMul.notinfd:=3; -- Remesa emitida
            elsif n.pntid is not null and n.crmid is not null and n.crmnumero is not null and n.ntfresultado is not null then
                if (n.ntfresultado='M3' or n.ntfresultado='M4' or n.ntfresultado='M2') then
                    vMul.notinfd:=4;
                    --if vMul.fnotinfd is null then
                            vMul.fnotinfd:=n.ntffecnot;
                    --end if;
                else
                    vMul.notinfd:=3;
                end if;
            end if;
        end if;
         --
    end loop;

end;


-----------------------------------
-- ANÁLISIS MOTIVO NO NOTIFICACIÓN
-----------------------------------
function leerMotivoNoNotificacion(vMulid number) return varchar2 is

cursor cMul(vId number) is
select /*+RULE*/
nvl(nvl(nvl(textoNoNotif,textoNoNotifPorParametro),textoNoNotifPorDefecto),textoNoNotifPorDefectoRadar) texto,
EsNoIdenCond
from
(
    select  m.mulnotmano notificada,
            d.decid idNoNotifPorParametro,
            decode(d.decid,190,null,d.decdescr) textoNoNotifPorParametro,
            m.descnonotif textoNoNotif,
            decode(m.mulxboletin,
                403,'Hecho observado a través de medio de captación y reproducción de imágenes que permiten la identificación del vehículo',
                303,'Conductor Ausente', null)
            textoNoNotifPorDefecto,
            decode(m.mulresmedida1,null,null,'Hecho observado a través de medio de captación y reproducción de imágenes que permiten la identificación del vehículo')
            textoNoNotifPorDefectoRadar,
            decode(m.infid,6000882,0,6000881,0,1) EsNoIdenCond
    from alba.decoingresos d, alba.multas m
    where m.mulxmotivo=d.decid(+)
    and m.seoidexp in
    (
        select m2.seoidexp
        from alba.multas m2
        where m2.mulid=vId
    )
    and m.mulnotmano='N'
    order by m.mulid desc
);


dev varchar2(302):=null;
noFin boolean:=true;

begin

    for m in cMul(vMulid) loop
    
        if noFin then
    
            if m.texto is not null then

                dev:=upper(substr(m.texto,1,300));
                
                -- No identificación de conductor lleva motivo en blanco
                if m.EsNoIdenCond=0 then
                    dev:='';
                end if;
                
                noFin:=false;

            end if;
            
        end if;

    end loop;

    return dev;

end;

function lc(vcadena varchar2) return varchar2 is

cadena varchar2(1000):='';

begin

  cadena:='';

  cadena:=upper(regexp_replace(vcadena,'[^A-z0-9]'));
  
  return cadena;
end;



--------------------------------
-- Comprueba si hay denunciante
--------------------------------
function hayDenunciante(vMulid number) return number is

cursor cAgente (vMulid number) is
select decode(nvl(to_char(a.agenumero),m.mulmiembrofse),null,0,1) agente
from 
alba.agentes a,
alba.multas m
where
m.age1_ageid = a.ageid
and m.mulid=vMulid
;

cursor cPersona (vMulid number) is
select substr(nvl(p.periden, p.peridenext) || ' ' ||
alba.componer_nombre (p.pernombre, p.perapell1, p.perpart1, p.perapell2, p.perpart2, p.perrazon),1,260)
from alba.expedientesingresoscontr  e, alba.decodificadora d,
alba.tipospersona t, alba.personas p, alba.contribuyentes c, alba.multas m
where e.eicxtipocontribuyente=d.decid
and e.conid=c.conid
and c.perid=p.perid
and p.tpeid=t.tpeid
and m.expidexp=e.expid
and p.perversion in
(select max(pp.perversion) from alba.personas pp where pp.perid = p.perid)
and m.mulid=vMulid
and d.decelemento='DENUNCIANTE'
and rownum <2;

vHayDenunciante number:=0;

begin

  vHayDenunciante:=0;
  
  for a in cAgente(vMulid) loop
    vHayDenunciante:=1;
  end loop;

  for p in cPersona(vMulid) loop
    vHayDenunciante:=1;
  end loop;

  return vHayDenunciante;
end;
--------------------------------

procedure errorEnVehiculo(vMulid number) is

cursor cdescripcionvehiculo(vid number) is
select expediente,
modelo_dgt,
modelo_multas,
distancia1,distancia2,distancia3,distancia4,distancia5
from
(
    select
    e.expediente, 
    lc(w.wsmarca || w.wsmodelo) modelo_dgt,
    lc(w.wsmodelo) mod,
    lc(w.wsmarca) mar,
    lc(m.mulmarca) modelo_multas,
    utl_match.edit_distance_similarity(
    lc(w.wsmarca || w.wsmodelo),
    lc(m.mulmarca)
    ) distancia1,
    utl_match.edit_distance_similarity(
    lc(w.wsmodelo),
    lc(m.mulmarca)
    ) distancia2,
    utl_match.edit_distance_similarity(
    lc(w.wsmarca),
    lc(m.mulmarca)
    ) distancia3,
    utl_match.edit_distance_similarity(
    substr(lc(m.mulmarca),1,5),
    lc(w.wsmodelo)
    ) distancia4,
    utl_match.edit_distance_similarity(
    substr(lc(m.mulmarca),1,5),
    lc(w.wsmarca)
    ) distancia5      
    from 
    alba.datoswsdgt w,
    alba.det_libro_resol d,
    alba.multas m,
    alba.multasestados e
    where e.mulid=m.mulid
    and e.mulid=d.mulid
    and d.idwsd=w.idwsd
    and lc(m.mulmarca) is not null
    and substr(w.wsmarca,1,1)<>'0'
    and e.mulid=vId
)
;

vReturn number:=0;

begin


  for i in cdescripcionvehiculo(vMulid) loop
  
      if i.distancia1=100 or i.distancia2=100 or i.distancia3=100 then
        vreturn:=0;
      else
        if i.distancia1<21 then
          vReturn:=1;
        end if;
      end if;
      
      if vreturn=1 and i.distancia2>49 or i.distancia3>49 then
        vreturn:=0;
      end if;
      
      if vreturn=1 and (i.distancia4>49 or i.distancia5>49)  then
        vreturn:=0;
      end if;
  
  end loop;

  vmul.errorenvehiculo:=vreturn;
  
exception
    when others then
    
        dbms_output.put_line('ERROR. ' || SQLERRM);        

        return;
end;


-------------------------------
-- Análisis de Libros
-------------------------------
procedure estadoAnotacionPuntos(vMulid number) is

cursor cLib(vMulid number) is
select l.lbrfecha, l.lbrid, t.lrtdescorta,
decode(t.lrtrequierefirma,'N',l.lbrfecha,l.lbrfirma) lbrfirma,
l.lbrnumero numerolibro, l.lbrfirma fresolucionlibro, d.esmid, d.mulid,
l.lbrremesaxml,an.resultado, an.mensaje
from 
alba.libro_resol_tipos t, alba.libro_resoluciones l, 
alba.det_libro_resol d, alba.ant_libro_resol an
where d.lbrid=l.lbrid
and l.lrtid=t.lrtid
and d.dlbid=an.dlbid(+)
and d.mulid=vmulid
and t.lrtdescorta in ('495','60')
order by l.lbrid asc;

--Los expedientes incluidos en estos libros no llevan anotación de puntos
cursor cNoLib(vMulid number) is
select decode(count(l.lbrid),0,1,0) anotar
from 
alba.libro_resol_tipos t, alba.libro_resoluciones l, 
alba.det_libro_resol d, alba.ant_libro_resol an
where d.lbrid=l.lbrid
and l.lrtid=t.lrtid
and d.dlbid=an.dlbid(+)
and d.mulid=vMulid
and t.lrtdescorta in ('81','82','85','86')
;

--Los expedientes con req.juzgado no llevan anotación de puntos
cursor cNoEsc(vMulid number) is
select decode(count(e.esmid),0,1,0) anotar
from alba.escritosmultas e
where e.mulid=vMulid
and e.tidomid=82
;

--444    SM    1 SIN_MATRÍCULA
--144    TR    2 TRÁFICO
--303    ZA    3 ORA
--403    CO    4 MULTACAR
--423    BT    5 BOTELLÓN
--464    OC    6 CONVIVENCIA
--465    CH    7 CENTRO HISTÓRICO
--523    bb    8 blackberry
--543    SR    9 SEMÁFORO ROJO

cursor cMul(vMulid number) is
select m.*, decode(i.infgrado,'L',4,'G',7,'M',7,0) grado,
decode(i.infgrado,'L',3,'G',6,'M',6,0) grado2,
decode(m.mulxboletin,144,1,303,1,403,1,444,0,423,0,464,0,465,1,523,1,543,1,623,0,664,0,723,0,743,0,800,0) conMatricula,
decode(i.puntos,null,0,1) conPuntos,
decode(i.infestacionamiento,null,0,'1',1) esEstacionamiento
from alba.infracciones i, alba.multas m
where m.infid=i.infid
and m.mulfecgrab>to_date('24/05/2010','dd/mm/rrrr')
and m.mulid=vMulid
and m.mulversion=
(select max(mm.mulversion) from alba.multas mm where mm.seoidexp=m.seoidexp);


-- Personas
cursor cPer(vMulid number) is
select m.mulid, m.mulnumbol, alba.componer_nombre(p.pernombre, p.perapell1, p.perpart1,
       p.perapell2, p.perpart2, p.perrazon) nombre,
       nvl(p.periden, p.peridenext) nif, d.decid, d.decelemento, d.decdescr,
           t.tpeid, t.tpercodigo, t.tpedesc, e.expid, ee.expcod expediente, p.perid, p.perversion
from alba.expedientesingresoscontr e, alba.decodificadora d, alba.expingre ee,
     alba.tipospersona t, alba.personas p, alba.contribuyentes c, alba.multas m
where e.eicxtipocontribuyente=d.decid
and e.conid=c.conid
and c.perid=p.perid
and p.tpeid=t.tpeid
and m.expidexp=e.expid
and ee.expid=e.expid
and e.eicfvigencia is null
and m.mulid=vMulid;


vprioridadpersona number:=0;
vPersona number:=0;
    
vLibxml number:=0;
-- 0.Sin libro, 1.Pendiente de Incluir en Libro, 2.Incluido en Libro, 3.Libro firmado, 4.Fichero XML Generado
vdetraerpuntos number:=0; -- 0. Sin detracción de Puntos, 1. Con detracción de Puntos
vtipopersona number:=0; -- 1. Física, 0. Otros.
vconsancionfirme number:=0; --0. Sin Sanción Firme, 1.Con Sanción Firme
vestadoanotable number:=0;
vNoLibros number:=0;
vNoEscritos number:=0;

begin

    -- Libros que excluyen al expediente de anotación
    for nl in cNoLib(vMulid) loop
        vNoLibros:=nl.anotar;
    end loop;
    
    -- Escritos que excluyen al expediente de anotación
    for ne in cNoEsc(vMulid) loop
        vNoEscritos:=ne.anotar;
    end loop;    

    -- Análisis de libros
    for l in cLib(vMulid) loop

        if l.lrtdescorta in ('495') then -- Sanción
            vLibxml:=vmul2.vtemp;
            if vmul2.vtemp in (2,3) then
              vlibxml:=1;
            end if;
        end if;   

        if l.lbrfirma is not null then
            if l.lbrremesaxml is not null then
                vmul2.vtemp:=4;
            else
                vmul2.vtemp:=3;
            end if;
        else
            vMul2.vTemp:=2;
        end if;
        

        if l.lrtdescorta in ('60') and vmul2.vtemp<5 then
            vLibxml:=vmul2.vtemp;
        end if;
        
        -- Con resultado erróneo es necesario volver a incluir en libro
        if l.lrtdescorta in ('60') and vmul2.vtemp=4 
        and l.resultado is not null
        and l.resultado in ('RESULTADO="ERPR"')
        and l.mensaje not like '%RCI4001%' --RCI4001 Antecedente duplicado en el RCI
        then
            vLibxml:=1;
        end if;
        
        
    end loop;
    
    -- Análisis de multa
    for m in cMul(vMulid) loop
    
        if m.conmatricula=1 and m.conpuntos=1 then
            vDetraerPuntos:=1;
            vMul.info:=vMul.info||'<PTS>';
        end if;
    
    end loop;
    
    -- Analiza persona
    for p in cPer(vMulid) loop
        -- DENUNCIADO
        if p.decelemento='SUJETOPASIVO' and vprioridadpersona<1 then
          vprioridadpersona:=1;
          vPersona:=p.tpeid;
        elsif p.decelemento in ('WSDGT_ARR','WSDGT_CH','WSDGT_POS','WSDGT_TIT','WSDGT_TRA')
        and vPrioridadPersona<2 then
          vPrioridadPersona:=2;
          vPersona:=p.tpeid;          
        elsif p.decelemento='TITULAR' and vPrioridadPersona<3 then
          vPrioridadPersona:=3;
          vpersona:=p.tpeid;
        elsif p.decelemento='CONDUCTOR' and vPrioridadPersona<4 then
          vPrioridadPersona:=4;
          vpersona:=p.tpeid;
        elsif p.decelemento='TUTOR' and vPrioridadPersona<5 then
          vPrioridadPersona:=5;
          vPersona:=p.tpeid;
        end if;
    end loop;
    
    if vpersona in (2,4) then 
        vTipoPersona:=1;
    end if;
    
    
    
    -- Análisis Firmeza
    if vmul.ffirmeza is not null 
    and vmul.ffirmeza>sysdate-365 and vmul.ffirmeza<sysdate-90
    then
        if vmul.csl>0 then -- Cobrados
          vconsancionfirme:=1;
        elsif vmul.csl=0 and vmul.libsanfir>1 then --No cobrados, con sanción firme, incluidos en libros
          vconsancionfirme:=1;
        end if;
    end if;
    
    -- Analiza Estados de tramitación
    if vmul.rer<>3 and vMul.aleg<>3 and vMul.rec<>3 then
        vestadoanotable:=1;
    end if;
    
    -- Estado de la anotación de puntos
    if vdetraerpuntos=1 and vtipopersona=1 and vconsancionfirme=1
    and vestadoanotable=1 
    and vNoLibros=1 and vNoEscritos=1
    then
    
        if vlibxml=0 then
            vmul.xml:=1;
        else
            vmul.xml:=vlibxml;
        end if;
    
    end if;
    
    -- Con puntos no susceptibles de anotación
    if vmul.xml=0 and vdetraerpuntos=1 then
        vmul.xml:=100;
    end if;
    
    /*if vmul.xml>0 and
    (vmul.estado like '%Caducado%' or vmul.estado like '%Suspendido%'
    or vmul.estado like '%Finalizado%' or vmul.estado like '%Error%')
    then
        vmul.xml:=100;    
    end if;*/

end;



---------------------
-- TIPO SUJETO PASIVO
---------------------
procedure procesaSujetoPasivo is

/*
1    JURIDICA
2    FISICA
3    ORGANISMO
4    FISICA EXTRANJERA
5    NO IDENTIFICADA
*/

begin
    -- TIPO SUJETO PASIVO
    --Conductor
    if vMul.tpc is not null and (vMul.tpc=2 or vMul.tpc=4) then
        vMul2.vTipoSp:=2;
    elsif vMul.tpc is not null and vMul.tpc=0 then
        vMul2.vTipoSp:=0;
    else
        vMul2.vTipoSp:=1;
    end if;      
    --Titular
    if vMul2.vTipoSp=0 and vMul.tpt is not null and (vMul.tpt=2 or vMul.tpt=4) then
        vMul2.vTipoSp:=2;
    elsif vMul2.vTipoSp=0 and vMul.tpt is not null and vMul.tpt=0 then
        vMul2.vTipoSp:=0;
    elsif vMul2.vTipoSp=0 then
        vMul2.vTipoSp:=1;
    end if;
    --Tutor
    if vMul.tptt is not null and vMul.tptt>0  then
        vMul2.vTipoSp:=20;
    end if;
  
end;



------------------------------
-- TIPO NOTIFICACIÓN DENUNCIA
------------------------------




------------------------
-- CÁLCULO PRESCRIPCION
------------------------
procedure calculoPrescripcion(vFechaDen date) is

    cursor cPres is
    select des, df
    from
    (
        select 'DENUNCIA' des, vFechaDen df from dual where vMul.fcomcon is null
        union select 'NOT. DENUNCIA' des, vMul.fnotden df from dual
        /* 22/01/2014
            Suprimir el indicador ¿POSIBLEMENTE PRESCRIPTO¿ actualmente vigente y sustituirlo por los 
            siguientes indicadores: 
            
            1.- PRESCRITO: 
            
            - Para infracciones leves, transcurridos 3 meses desde la ¿fecha de la denuncia¿ hasta la 
            ¿fecha de notificación¿ o intentos fallidos de notificación.
            - Para infracciones graves y muy graves, transcurridos 6 meses desde la ¿fecha de la 
            denuncia hasta la ¿fecha de notificación¿ o intentos fallidos de notificación.
            
            2.- CADUCADO: 
            
            En todos los expedientes transcurrido 1 año (365 días), desde la fecha del 
            ¿Libro de Incoación¿ o notificación en el acto de la denuncia hasta la fecha 
            de la ¿Resolución Sancionadora¿. 
            
            3.- POSIBLEMENTE CADUCADO: 
            
            Expedientes que se han resuelto entre la fecha del libro de Incoación y la 
            fecha de la Resolución Sancionadora, pero no se ha podido notificar dentro 
            de ese año. Este indicador ¿posiblemente caducado¿ no debe paralizar la 
            tramitación de los expedientes y no impedirá su agrupación o notificación. */
                    
        union select 'COMUNICACIÓN CONDUCTOR' des, vMul.fcomcon df from dual
        union select 'NOT. SANCIÓN' des, vMul.fnotsan df from dual
        union select 'NOT. REQUERIMIENTO' des, vMul.fnotreq df from dual
        union select 'INF. DEN. SOLICITADO' des, vMul.finfd df from dual
        union select 'INF. DEN. RECIBIDO' des, vMul.frinfd df from dual
        union select 'ALEGACIÓN' des, vMul.faleg df from dual
        union select 'NOT. RESOLUCIÓN ALEGACIÓN' des, vMul.fnotaleg df from dual
        union select 'RECURSO' des, vMul.frec df from dual
        union select 'NOT. RESOLUCIÓN RECURSO' des, vMul.fnotrec df from dual
        union select 'INTENTO NOT. DENUNCIA' des, vMul.fnotdenIntento df from dual
        union select 'INTENTO NOT. SANCIÓN' des, vMul.fnotsanIntento df from dual
        union select 'INTENTO NOT. REQUERIMIENTO' des, vMul.fnotreqIntento df from dual
        union select 'INTENTO NOT. RESOL. ALEGACIÓN' des, vMul.fnotalegIntento df from dual
        union select 'INTENTO NOT. RESOL. RECURSO' des, vMul.fnotrecIntento df from dual
        union select 'SANCIÓN FIRME' des, vMul.ffirmeza df from dual
        union select 'RECURSO CONTRA LA P.A.' des, vMul.frpa df from dual
        union select 'NOT. PROVIDENCIA APREMIO' des, vMul.fnotpa df from dual
        union select 'FECHA ACTUAL' des, trunc(sysdate) df from dual
        
        
        /*
        union
        -- Notificaciones Denuncia y Requerimiento Versiones Anteriores
        select
        decode(n.ntfresultado,'M3','','M4','','M2','','INTENTO ')||
        decode(a.atocodigo,'MULTDC','NOT.REQ.VERSIÓN ANTERIOR','NOT.DEN.VERSIÓN ANTERIOR') des,
        to_date(n.ntffecnot,'dd/mm/rrrr') df
        from alba.actosnotificacion a, alba.cabremesas c, alba.remesas r,
            alba.notificaciones n, alba.propuestasnotificacion p
        where p.pntid=n.pntid
        and n.ntfid=r.ntfid
        and r.crmid=c.crmid
        and p.atoid=a.atoid
        and p.pntorigen='I'
        and n.ntffecnot is not null
        and n.ntfcrmid=c.crmid
        and p.pntorigenid in
            (select mulid
            from alba.multas m
            where m.seoidexp in
            (select m2.seoidexp
            from alba.multas m2
            where m2.mulid=vMul.mulid)
            and m.mulid<>(select max(m3.mulid)
            from alba.multas m3
            where m3.mulid=vMul.mulid))
        and a.atocodigo in
        ('MULTDN','MULTDP','MULTDT','MULTDU','MULTDE','MULTDM','MULTDA','MULTDV','MULTDR','MULTDC')
        --and n.ntfresultado in ('M3','M4','M2')
        --union
        --select decode(e.tidomid,1,'GRABACIÓN ALEGACIÓN',2,'GRABACIÓN RECURSO',
        --3,'GRABACIÓN RER','GRABACIÓN I.CONDUCTOR') des, trunc(e.esmfgrabacion) df
        --from alba.escritosmultas e
        --where e.tidomid in (1,2,3,21)
        --and e.mulid=vMul.mulid
        union
        -- Publicación en BOP
        select upper(t.lrtdescripcion) || ' VER.ANT.' des, to_date(l.lbrfirma,'dd/mm/rrrr') df
        from alba.libro_resol_tipos t, alba.libro_resoluciones l, alba.det_libro_resol d
        where d.lbrid=l.lbrid
        and l.lrtid=t.lrtid
        and t.lrtdescorta
        in ('91','915','92','925','93','935','94','945','95','955','96','965')
        and d.mulid in
        (select mulid
        from alba.multas m
        where m.seoidexp in
        (select m2.seoidexp
        from alba.multas m2
        where m2.mulid=vMul.mulid)
        and m.mulid<>(select max(m3.mulid)
        from alba.multas m3
        where m3.mulid=vMul.mulid))
        */
        
    )
    where df is not null
    order by df asc;

    dfAnt date:=null;
    desAnt varchar2(200):='';
    i number:=1;
    noPrescrito boolean:=true;
    providenciaApremioNotificada boolean:=false;
    
    -- Control de Caducidad
    vNotificacionDenuncia date:=null;
    vFechaCaducidad date:=null;

begin

    -- Variables devueltas por emultas
    -- vMul.forigenCalculoPrescripcion
    -- vMul.fprescripcion
    
    -- Variables protegidas de caracter temporal
    -- vMul2.vFechaOrigenPrescripcion
    -- vMul2.vFechaPrescripcion
    
    -- Si se notificó la providencia de apremio no se tienen en cuenta
    -- posibles prescripciones en estados anteriores
    if vMul.rpa>0 then
            providenciaApremioNotificada:=true;
    else
            providenciaApremioNotificada:=false;
    end if;


    for cp in cPres loop
    

        --
        --
        if noPrescrito then

            vPres.tipoFecha:=cp.des;
            vPres.fecha:=cp.df;


            -- Ejecutiva
            if desAnt='SANCIÓN FIRME' or cp.des='NOT. PROVIDENCIA APREMIO' then

                    vPres.dias:=cp.df-dfAnt;
                    
                    if vMul.origendenuncia=5 then
                        vPres.limite:=48*30;
                        vPres.fechaLimite:=to_date(add_months(dfAnt,48),'dd/mm/rrrr');                    
                    else
                        vPres.limite:=12*30;
                        vPres.fechaLimite:=to_date(add_months(dfAnt,12),'dd/mm/rrrr');
                    end if;

            -- Recurso » 7 meses para resolver
            elsif desAnt in ('RECURSO','GRABACIÓN RECURSO') then

                    vPres.dias:=cp.df-dfAnt;
                    vPres.limite:=12*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,12),'dd/mm/rrrr');
                    
            -- Rec.Ext.Rev » 12 meses para resolver
            elsif desAnt in ('RECURSO EXT. REVISIÓN','GRABACIÓN RER') then

                    vPres.dias:=cp.df-dfAnt;
                    vPres.limite:=12*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,12),'dd/mm/rrrr');

            -- Providencia de Apremio » 12 meses para resolver
            elsif desAnt in ('NOT. PROVIDENCIA APREMIO','RECURSO CONTRA LA P.A.') then

                    vPres.dias:=cp.df-dfAnt;
                    vPres.limite:=12*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,12),'dd/mm/rrrr');
            -- Providencia de Apremio » 12 meses para resolver
            elsif desAnt in ('NOT. RESOLUCIÓN RECURSO','RECURSO CONTRA LA P.A.') then

                    vPres.dias:=cp.df-dfAnt;
                    vPres.limite:=12*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,12),'dd/mm/rrrr');                    

            -- Tramitación previa a ejectutiva
            else

                if i=1 then
                    vPres.dias:=0;
                    vPres.limite:=0;
                    vPres.fechaLimite:=null;
                    vPres.enPlazo:=1;
                elsif i=2 and desAnt='DENUNCIA' then
                    vPres.dias:=cp.df-dfAnt;
                    vPres.limite:=vMul2.vGrado2*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,vMul2.vGrado2),'dd/mm/rrrr');
                else
                    vPres.dias:=cp.df-dfAnt;
                    vPres.limite:=vMul2.vGrado*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,vMul2.vGrado),'dd/mm/rrrr');
                end if;

            end if;

            -- Finaliza cálculo si encuentra prescripción
            if vPres.fechaLimite is not null
            and vPres.fechaLimite<vPres.fecha
            and providenciaApremioNotificada=false then

                vPres.enPlazo:=0;
                noPrescrito:=false;

                if vMul.cestado<30000 then
                    vMul.cestado:=vMul.cestado+40000;
                    vMul.estado:=vMul.estado;
                    --vMul.cestado:=50000;
                    --vMul.estado:='Finalizado';
                end if;

            else
                vPres.enPlazo:=1;
            end if;
            
            --dbms_output.put_line('Fecha: ' || vPres.fecha || ' Fecha Límite: ' || vPres.fechaLimite);
            
            -- Guarda fechas de Prescripción
            if dfAnt is null then
                vMul.forigenCalculoPrescripcion:=vFechaDen;
            else
                vMul.forigenCalculoPrescripcion:=dfAnt;
            end if;
            
            if (vMul.fprescripcion is null or vMul.fprescripcion>vPres.fechaLimite) then
              vMul.fprescripcion:=vPres.fechaLimite;
            end if;
            
            
            --pipe row(vPres);
            vmul.cpres:=vmul.cpres ||
            to_char(vPres.tipoFecha) || '#' ||
            to_char(vPres.fecha) || '#' ||
            to_char(vPres.dias) || '#' ||
            to_char(vPres.limite) || '#' ||
            to_char(vPres.fechaLimite) || '#' ||
            to_char(vPres.enPlazo) || '¬';                  

            -- Actualiza controles bucle
            i:=i+1;
            dfAnt:=cp.df;
            desAnt:=cp.des;
            
        end if;

    end loop;

end;




--------------------------------
-- CÁLCULO PRESCRIPCION BOTELLÓN
--------------------------------
procedure calculoPrescripcion4(vFechaDen date) is

    cursor cPres is
    select des, df
    from
    (
        select 'DENUNCIA' des, vFechaDen df from dual
        union select 'NOT. DENUNCIA' des, vMul.fnotden df from dual
        union select 'COMUNICACIÓN CONDUCTOR' des, vMul.fcomcon df from dual
        union select 'NOT. SANCIÓN' des, vMul.fnotsan df from dual
        union select 'NOT. REQUERIMIENTO' des, vMul.fnotreq df from dual
        union select 'INF. DEN. SOLICITADO' des, vMul.finfd df from dual
        union select 'INF. DEN. RECIBIDO' des, vMul.frinfd df from dual
        union select 'ALEGACIÓN' des, vMul.faleg df from dual
        union select 'NOT. RESOLUCIÓN ALEGACIÓN' des, vMul.fnotaleg df from dual
        union select 'RECURSO' des, vMul.frec df from dual
        union select 'NOT. RESOLUCIÓN RECURSO' des, vMul.fnotrec df from dual
        union select 'RECURSO EXT. REVISIÓN' des, vMul.frer df from dual
        union select 'NOT. RESOL. REC. EXT.' des, vMul.fnotrer df from dual
        union select 'INTENTO NOT. DENUNCIA' des, vMul.fnotdenIntento df from dual
        union select 'INTENTO NOT. SANCIÓN' des, vMul.fnotsanIntento df from dual
        union select 'INTENTO NOT. REQUERIMIENTO' des, vMul.fnotreqIntento df from dual
        union select 'INTENTO NOT. RESOL. ALEGACIÓN' des, vMul.fnotalegIntento df from dual
        union select 'INTENTO NOT. RESOL. RECURSO' des, vMul.fnotrecIntento df from dual
        union select 'INTENTO NOT. RESOL. REC. EXT.' des, vMul.fnotrerIntento df from dual
        union select 'SANCIÓN FIRME' des, vMul.ffirmeza df from dual
        union select 'RECURSO CONTRA LA P.A.' des, vMul.frpa df from dual
        union select 'NOT. PROVIDENCIA APREMIO' des, vMul.fnotpa df from dual
        union select 'FECHA ACTUAL' des, trunc(sysdate) df from dual
        union
        -- Notificaciones Denuncia y Requerimiento Versiones Anteriores
        select
        decode(n.ntfresultado,'M3','','M4','','M2','','INTENTO ')||
        decode(a.atocodigo,'MULTDC','NOT.REQ.VERSIÓN ANTERIOR','NOT.DEN.VERSIÓN ANTERIOR') des,
        n.ntffecnot df
        from alba.actosnotificacion a, alba.cabremesas c, alba.remesas r,
            alba.notificaciones n, alba.propuestasnotificacion p
        where p.pntid=n.pntid
        and n.ntfid=r.ntfid
        and r.crmid=c.crmid
        and p.atoid=a.atoid
        and p.pntorigen='I'
        and n.ntffecnot is not null
        and n.ntfcrmid=c.crmid
        and p.pntorigenid in
            (select mulid
            from alba.multas m
            where m.seoidexp in
            (select m2.seoidexp
            from alba.multas m2
            where m2.mulid=vMul.mulid)
            and m.mulid<>(select max(m3.mulid)
            from alba.multas m3
            where m3.mulid=vMul.mulid))
        and a.atocodigo in
        ('MULTDN','MULTDP','MULTDT','MULTDU','MULTDE','MULTDM','MULTDA','MULTDV','MULTDR','MULTDC')
        --and n.ntfresultado in ('M3','M4','M2')
        union
        select decode(e.tidomid,1,'GRABACIÓN ALEGACIÓN',2,'GRABACIÓN RECURSO',
        3,'GRABACIÓN RER','GRABACIÓN I.CONDUCTOR') des, trunc(e.esmfgrabacion) df
        from alba.escritosmultas e
        where e.tidomid in (1,2,3,21)
        and e.mulid=vMul.mulid
        
    )
    where df is not null
    order by df asc;

    dfAnt date:=null;
    desAnt varchar2(200):='';
    i number:=1;
    noPrescrito boolean:=true;
    providenciaApremioNotificada boolean:=false;
    
    -- Control de Caducidad
    vNotificacionDenuncia date:=null;
    vFechaCaducidad date:=null;

begin

    -- Variables devueltas por emultas
    -- vMul.forigenCalculoPrescripcion
    -- vMul.fprescripcion
    
    -- Variables protegidas de caracter temporal
    -- vMul2.vFechaOrigenPrescripcion
    -- vMul2.vFechaPrescripcion
    
    -- Si se notificó la providencia de apremio no se tienen en cuenta
    -- posibles prescripciones en estados anteriores
    if vMul.rpa>0 then
            providenciaApremioNotificada:=true;
    else
            providenciaApremioNotificada:=false;
    end if;


    for cp in cPres loop
    

        --
        --
        if noPrescrito then

            vPres.tipoFecha:=cp.des;
            vPres.fecha:=cp.df;


            -- Ejecutiva
            if desAnt='SANCIÓN FIRME' then

                    vPres.dias:=cp.df-dfAnt;
                    vPres.limite:=12*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,12),'dd/mm/rrrr');
                    

            -- Recurso » 7 meses para resolver
            elsif desAnt in ('RECURSO','GRABACIÓN RECURSO') then

                    vPres.dias:=cp.df-dfAnt;
                    vPres.limite:=12*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,12),'dd/mm/rrrr');
                    
            -- Rec.Ext.Rev » 12 meses para resolver
            elsif desAnt in ('RECURSO EXT. REVISIÓN','GRABACIÓN RER') then

                    vPres.dias:=cp.df-dfAnt;
                    vPres.limite:=12*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,12),'dd/mm/rrrr');

            -- Providencia de Apremio » 12 meses para resolver
            elsif desAnt in ('NOT. PROVIDENCIA APREMIO','RECURSO CONTRA LA P.A.') then

                    vPres.dias:=cp.df-dfAnt;
                    vPres.limite:=12*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,12),'dd/mm/rrrr');

            -- Tramitación previa a ejectutiva
            else

                if i=1 then
                    vPres.dias:=0;
                    vPres.limite:=0;
                    vPres.fechaLimite:=null;
                    vPres.enPlazo:=1;
                elsif i=2 then
                    vPres.dias:=cp.df-dfAnt;
                    vPres.limite:=vMul2.vGrado2*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,vMul2.vGrado2),'dd/mm/rrrr');
                else
                    vPres.dias:=cp.df-dfAnt;
                    vPres.limite:=vMul2.vGrado2*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,vMul2.vGrado),'dd/mm/rrrr');
                end if;

            end if;

            -- Finaliza cálculo si encuentra prescripción
            if vPres.fechaLimite is not null
            and vPres.fechaLimite<vPres.fecha
            and providenciaApremioNotificada=false then

                vPres.enPlazo:=0;
                noPrescrito:=false;

                if vMul.cestado<30000 then
                    vMul.cestado:=vMul.cestado+40000;
                    vMul.estado:=vMul.estado;
                    --vMul.cestado:=50000;
                    --vMul.estado:='Finalizado';
                end if;

            else
                vPres.enPlazo:=1;
            end if;
            
            
            -- Control de Caducidad
            /*if vMul.flibinc is not null then
            if cp.des='FECHA ACTUAL' then
                vNotificacionDenuncia:=to_date(vMul.flibinc,'dd/mm/rrrr');
                vFechaCaducidad:=to_date(nvl(nvl(vMul.flibaleg,vMul.flibsan),sysdate),'dd/mm/rrrr');

                -- Finaliza cálculo si encuentra caducidad
                if vNotificacionDenuncia is not null and vFechaCaducidad is not null
                then
                    if vFechaCaducidad>vNotificacionDenuncia
                    and vFechaCaducidad-vNotificacionDenuncia>365 then
                        vPres.enPlazo:=0;
                        noPrescrito:=false;

                        if vMul.cestado<30000 then
                            --vMul.cestado:=vMul.cestado+40000;
                            --vMul.estado:=vMul.estado;
                            vMul.cestado:=51000;
                            vMul.estado:='Finalizado';
                        end if;
                    end if;
                end if;
                
            end if;
            end if;*/




            -- Guarda fechas de Prescripción
            if dfAnt is null then
                vMul.forigenCalculoPrescripcion:=vFechaDen;
            else
                vMul.forigenCalculoPrescripcion:=dfAnt;
            end if;
            
            vMul.fprescripcion:=vPres.fechaLimite;
            
            --pipe row(vPres);
            vmul.cpres:=vmul.cpres ||
            to_char(vPres.tipoFecha) || '#' ||
            to_char(vPres.fecha) || '#' ||
            to_char(vPres.dias) || '#' ||
            to_char(vPres.limite) || '#' ||
            to_char(vPres.fechaLimite) || '#' ||
            to_char(vPres.enPlazo) || '¬';                

            -- Actualiza controles bucle
            i:=i+1;
            dfAnt:=cp.df;
            desAnt:=cp.des;
            
        end if;

    end loop;

end;




-------------------------------------
-- CÁLCULO PRESCRIPCION ANTIVANDÁLICA
-------------------------------------
procedure calculoPrescripcion6(vFechaDen date) is

    cursor cPres is
    select des, df
    from
    (
        select 'DENUNCIA' des, vFechaDen df from dual
        union select 'NOT. DENUNCIA' des, vMul.fnotden df from dual
        union select 'COMUNICACIÓN CONDUCTOR' des, vMul.fcomcon df from dual
        union select 'NOT. SANCIÓN' des, vMul.fnotsan df from dual
        union select 'NOT. REQUERIMIENTO' des, vMul.fnotreq df from dual
        union select case when vMul.fnotden is null then null else 'INF. DEN. SOLICITADO' end des,
              case when vMul.fnotden is null then null else vMul.finfd end df from dual
        union select case when vMul.fnotden is null then null else 'INF. DEN. RECIBIDO' end des,
              case when vMul.fnotden is null then null else vMul.frinfd end df from dual              
        union select 'ALEGACIÓN' des, vMul.faleg df from dual
        union select 'NOT. RESOLUCIÓN ALEGACIÓN' des, vMul.fnotaleg df from dual
        union select 'RECURSO' des, vMul.frec df from dual
        union select 'NOT. RESOLUCIÓN RECURSO' des, vMul.fnotrec df from dual
        union select 'RECURSO EXT. REVISIÓN' des, vMul.frer df from dual
        union select 'NOT. RESOL. REC. EXT.' des, vMul.fnotrer df from dual
        union select 'INTENTO NOT. DENUNCIA' des, vMul.fnotdenIntento df from dual
        union select 'INTENTO NOT. SANCIÓN' des, vMul.fnotsanIntento df from dual
        union select 'INTENTO NOT. REQUERIMIENTO' des, vMul.fnotreqIntento df from dual
        union select 'INTENTO NOT. RESOL. ALEGACIÓN' des, vMul.fnotalegIntento df from dual
        union select 'INTENTO NOT. RESOL. RECURSO' des, vMul.fnotrecIntento df from dual
        union select 'INTENTO NOT. RESOL. REC. EXT.' des, vMul.fnotrerIntento df from dual
        union select 'SANCIÓN FIRME' des, vMul.ffirmeza df from dual
        union select 'RECURSO CONTRA LA P.A.' des, vMul.frpa df from dual
        union select 'NOT. PROVIDENCIA APREMIO' des, vMul.fnotpa df from dual
        union select 'FECHA ACTUAL' des, trunc(sysdate) df from dual
        union
        -- Notificaciones Denuncia y Requerimiento Versiones Anteriores
        select
        decode(n.ntfresultado,'M3','','M4','','M2','','INTENTO ')||
        decode(a.atocodigo,'MULTDC','NOT.REQ.VERSIÓN ANTERIOR','NOT.DEN.VERSIÓN ANTERIOR') des,
        n.ntffecnot df
        from alba.actosnotificacion a, alba.cabremesas c, alba.remesas r,
            alba.notificaciones n, alba.propuestasnotificacion p
        where p.pntid=n.pntid
        and n.ntfid=r.ntfid
        and r.crmid=c.crmid
        and p.atoid=a.atoid
        and p.pntorigen='I'
        and n.ntffecnot is not null
        and n.ntfcrmid=c.crmid
        and p.pntorigenid in
            (select mulid
            from alba.multas m
            where m.seoidexp in
            (select m2.seoidexp
            from alba.multas m2
            where m2.mulid=vMul.mulid)
            and m.mulid<>(select max(m3.mulid)
            from alba.multas m3
            where m3.mulid=vMul.mulid))
        and a.atocodigo in
        ('MULTDN','MULTDP','MULTDT','MULTDU','MULTDE','MULTDM','MULTDA','MULTDV','MULTDR','MULTDC')
        --and n.ntfresultado in ('M3','M4','M2')
        union
        select decode(e.tidomid,1,'GRABACIÓN ALEGACIÓN',2,'GRABACIÓN RECURSO',
        3,'GRABACIÓN RER','GRABACIÓN I.CONDUCTOR') des, trunc(e.esmfgrabacion) df
        from alba.escritosmultas e
        where e.tidomid in (1,2,3,21)
        and e.mulid=vMul.mulid
        
    )
    where df is not null
    order by df asc;

    dfAnt date:=null;
    desAnt varchar2(200):='';
    i number:=1;
    noPrescrito boolean:=true;
    providenciaApremioNotificada boolean:=false;
    
    -- Control de Caducidad
    vNotificacionDenuncia date:=null;
    vFechaCaducidad date:=null;

begin

    -- Variables devueltas por emultas
    -- vMul.forigenCalculoPrescripcion
    -- vMul.fprescripcion
    
    -- Variables protegidas de caracter temporal
    -- vMul2.vFechaOrigenPrescripcion
    -- vMul2.vFechaPrescripcion
    
    -- Si se notificó la providencia de apremio no se tienen en cuenta
    -- posibles prescripciones en estados anteriores
    if vMul.rpa>0 then
            providenciaApremioNotificada:=true;
    else
            providenciaApremioNotificada:=false;
    end if;


    for cp in cPres loop
    

        --
        --
        if noPrescrito then

            vPres.tipoFecha:=cp.des;
            vPres.fecha:=cp.df;


            -- Ejecutiva
            if desAnt='SANCIÓN FIRME' then

                    vPres.dias:=cp.df-dfAnt;
                    vPres.limite:=12*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,6),'dd/mm/rrrr');
                    
            -- NOTIFICACION DENUNCIA
            elsif desAnt in ('DENUNCIA') then

                    vPres.dias:=cp.df-dfAnt;
                    vPres.limite:=12*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,6),'dd/mm/rrrr');                    

            -- Recurso » 7 meses para resolver
            elsif desAnt in ('RECURSO','GRABACIÓN RECURSO') then

                    vPres.dias:=cp.df-dfAnt;
                    vPres.limite:=12*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,6),'dd/mm/rrrr');
                    
            -- Rec.Ext.Rev » 12 meses para resolver
            elsif desAnt in ('RECURSO EXT. REVISIÓN','GRABACIÓN RER') then

                    vPres.dias:=cp.df-dfAnt;
                    vPres.limite:=12*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,6),'dd/mm/rrrr');

            -- Providencia de Apremio » 12 meses para resolver
            elsif desAnt in ('NOT. PROVIDENCIA APREMIO','RECURSO CONTRA LA P.A.') then

                    vPres.dias:=cp.df-dfAnt;
                    vPres.limite:=12*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,6),'dd/mm/rrrr');

            -- Tramitación previa a ejectutiva
            else

               
                if i=1 then
                    vPres.dias:=0;
                    vPres.limite:=0;
                    vPres.fechaLimite:=null;
                    vPres.enPlazo:=1;
                elsif i=2 then
                    vPres.dias:=cp.df-dfAnt;
                    vPres.limite:=vMul2.vGrado2*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,vMul2.vGrado2),'dd/mm/rrrr');
                else
                    vPres.dias:=cp.df-dfAnt;
                    vPres.limite:=vMul2.vGrado*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,vMul2.vGrado),'dd/mm/rrrr');
                end if;                
                

            end if;

            -- Finaliza cálculo si encuentra prescripción
            if vPres.fechaLimite is not null
            and vPres.fechaLimite<vPres.fecha
            and providenciaApremioNotificada=false then

                vPres.enPlazo:=0;
                noPrescrito:=false;

                if vMul.cestado<30000 then
                    vMul.cestado:=vMul.cestado+40000;
                    vMul.estado:=vMul.estado;
                    --vMul.cestado:=50000;
                    --vMul.estado:='Finalizado';
                end if;

            else
                vPres.enPlazo:=1;
            end if;
            
            
            -- Control de Caducidad
            if vMul.flibinc is not null then
            if cp.des='FECHA ACTUAL' then
                vNotificacionDenuncia:=to_date(vMul.flibinc,'dd/mm/rrrr');
                vFechaCaducidad:=to_date(nvl(nvl(vMul.flibaleg,vMul.flibsan),sysdate),'dd/mm/rrrr');

                -- Finaliza cálculo si encuentra caducidad
                if vNotificacionDenuncia is not null and vFechaCaducidad is not null
                then
                    if vFechaCaducidad>vNotificacionDenuncia
                    and vFechaCaducidad-vNotificacionDenuncia>365+30 then
                        vPres.enPlazo:=0;
                        noPrescrito:=false;

                        if vMul.cestado<30000 then
                            --vMul.cestado:=vMul.cestado+40000;
                            --vMul.estado:=vMul.estado;
                            vMul.cestado:=51000;
                            vMul.estado:='Finalizado';
                        end if;
                    end if;
                end if;
                
            end if;
            end if;




            -- Guarda fechas de Prescripción
            if dfAnt is null then
                vMul.forigenCalculoPrescripcion:=vFechaDen;
            else
                vMul.forigenCalculoPrescripcion:=dfAnt;
            end if;
            
            vMul.fprescripcion:=vPres.fechaLimite;
            
            --pipe row(vPres);
            vmul.cpres:=vmul.cpres ||
            to_char(vPres.tipoFecha) || '#' ||
            to_char(vPres.fecha) || '#' ||
            to_char(vPres.dias) || '#' ||
            to_char(vPres.limite) || '#' ||
            to_char(vPres.fechaLimite) || '#' ||
            to_char(vPres.enPlazo) || '¬';                  

            -- Actualiza controles bucle
            i:=i+1;
            dfAnt:=cp.df;
            desAnt:=cp.des;
            
        end if;

    end loop;

end;





--------------------------------
-- MUESTRA FECHAS
--------------------------------
function muestraFechas(vMulid number) return fechas_lista2 pipelined is

    cursor cPres is
    select to_date(m.mulfec,'dd/mm/rrrr') fechaDenuncia, m.mulfecgrab, e.*,
    decode(i.infgrado,'L',4,'G',7,'M',7,0) limiteRestoMes,
    decode(i.infgrado,'L',3,'G',6,'M',6,0) limiteDenunciaMes
    from alba.infracciones i, alba.multas m, table(trml.emultas(m.mulid)) e
    where m.infid=i.infid
    and m.mulid=vMulid;

    dfAnt date:=null;
    desAnt varchar2(200):='';
    i number:=1;
    vFechas fechas2;

begin

    for cp in cPres loop
    
        for dt in
        (
            select des, trunc(df) df
            from
                (
                select 'DENUNCIA' des, cp.fechaDenuncia df from dual
                union select 'GRABACIÓN' des, cp.mulfecgrab df from dual
                union select 'CONSULTA DATOS IDENTIDAD Y DOMICILIO EN DGT Y PADRÓN' des, cp.flibcondat df from dual
                union select 'NOT. DENUNCIA' des, cp.fnotden df from dual
                union select 'ESCRITO COM. CONDUCTOR' des, cp.fecomcon df from dual
                union select 'COMUNICACIÓN CONDUCTOR' des, cp.fcomcon df from dual
                union select 'NOT. SANCIÓN' des, cp.fnotsan df from dual
                union select 'NOT. REQUERIMIENTO' des, cp.fnotreq df from dual
                union select 'INF. DEN. SOLICITADO' des, cp.finfd df from dual
                union select 'INF. DEN. RECIBIDO' des, cp.frinfd df from dual
                union select 'ALEGACIÓN' des, cp.faleg df from dual
                union select 'NOT. RESOLUCIÓN ALEGACIÓN' des, cp.fnotaleg df from dual
                union select 'RECURSO' des, cp.frec df from dual
                union select 'NOT. RESOLUCIÓN RECURSO' des, cp.fnotrec df from dual
                union select 'INTENTO NOT. DENUNCIA' des, cp.fnotdenIntento df from dual
                union select 'INTENTO NOT. SANCIÓN' des, cp.fnotsanIntento df from dual
                union select 'INTENTO NOT. REQUERIMIENTO' des, cp.fnotreqIntento df from dual
                union select 'INTENTO NOT. RESOL. ALEGACIÓN' des, cp.fnotalegIntento df from dual
                union select 'INTENTO NOT. RESOL. RECURSO' des, cp.fnotrecIntento df from dual
                union select 'SANCIÓN FIRME' des, cp.ffirmeza df from dual
                union select 'LIBRO INCOACIÓN' des, cp.flibinc df from dual
                union select 'LIBRO SANCIÓN' des, cp.flibsan df from dual
                union select 'LIBRO RESOLUCIÓN ALEGACIÓN' des, cp.flibaleg df from dual
                union select 'LIBRO RESOL RECURSO' des, cp.flibrec df from dual
                union select 'RECURSO EXTREVISIÓN' des, cp.frer df from dual
                union select 'LIBRO RESOL REC. EXT. REV' des, cp.flibrer df from dual
                union select 'NOT. RESOL REC. EXT. REV.' des, cp.fnotrer df from dual
                union select 'INICIO VOLUNTARIA' des, cp.finivol df from dual
                union select 'FIN VOLUNTARIA' des, cp.ffinvol df from dual
                )
            where df is not null
            order by df asc
        )
        loop
    
      if i=1 then
        dfAnt:=dt.df;
      end if;

      vFechas.descripcion:=dt.des;
      vFechas.fecha:=dt.df;
      vFechas.dias:=dt.df-dfAnt;
      
      -- Actualiza controles bucle
      i:=i+1;
      dfAnt:=dt.df;
      desAnt:=dt.des;
      
      pipe row(vFechas);

    end loop;
  end loop;
  
  return;

end;

----------------------------------------------
-- MUESTRA NUEVO CÁLCULO CADUCIDAD TRÁFICO 
----------------------------------------------
function muestraCalculoCaducidadSim(vMulid number) return prescripcion_lista pipelined is

    cursor cPres is
    select to_date(m.mulfec,'dd/mm/rrrr') fechaDenuncia, e.*,
    decode(i.infgrado,'L',3,'G',6,'M',6,0) limiteRestoMes,
    decode(i.infgrado,'L',3,'G',6,'M',6,0) limiteDenunciaMes
    from alba.infracciones i, alba.multas m, table(trml.emultas(m.mulid)) e
    where m.infid=i.infid
    and m.mulid=vMulid;
    
    dfAnt date:=null;
    desAnt varchar2(200):='';
    i number:=1;

    vPresLocal prescripcion;
    
    -- Control de Caducidad
    vNotificacionDenuncia date:=null;
    vFechaCaducidad date:=null;

begin

    for cp in cPres loop
    
        for dt in
        (
            select des, df
            from
                (
                
                /* 22/01/2014
                    Suprimir el indicador ¿POSIBLEMENTE PRESCRIPTO¿ actualmente vigente y sustituirlo por los 
                    siguientes indicadores: 
                    
                    1.- PRESCRITO: 
                    
                    - Para infracciones leves, transcurridos 3 meses desde la ¿fecha de la denuncia¿ hasta la 
                    ¿fecha de notificación¿ o intentos fallidos de notificación.
                    - Para infracciones graves y muy graves, transcurridos 6 meses desde la ¿fecha de la 
                    denuncia hasta la ¿fecha de notificación¿ o intentos fallidos de notificación.
                    
                    2.- CADUCADO: 
                    
                    En todos los expedientes transcurrido 1 año (365 días), desde la fecha del 
                    ¿Libro de Incoación¿ o notificación en el acto de la denuncia hasta la fecha 
                    de la ¿Resolución Sancionadora¿. 
                    
                    3.- POSIBLEMENTE CADUCADO: 
                    
                    Expedientes que se han resuelto entre la fecha del libro de Incoación y la 
                    fecha de la Resolución Sancionadora, pero no se ha podido notificar dentro 
                    de ese año. Este indicador ¿posiblemente caducado¿ no debe paralizar la 
                    tramitación de los expedientes y no impedirá su agrupación o notificación.              
                */                
                
                select 'LIBRO INCOACIÓN' des, cp.flibinc df from dual --where cp.flibinc<=cp.ffirmeza
                union select 'NOT. DENUNCIA' des, cp.fnotden df from dual where cp.flibinc is null
                union select 'SANCIÓN' des, cp.fnotsan df from dual where cp.fnotalegIntento is null
                --union select 'RECURSO CONTRA LA P.A.' des, cp.frpa df from dual
                --union select 'NOT. PROVIDENCIA APREMIO' des, cp.fnotpa df from dual
                union select 'INTENTO NOT. RESOL. ALEGACIÓN' des, cp.fnotalegIntento df from dual 
                union select 'FECHA ACTUAL' des, trunc(sysdate) df from dual where cp.fnotsan is null
                )
                
            where df is not null
            order by df asc
        )
        loop
        
        

            vPres:=vPresLocal;

            vPres.tipoFecha:=dt.des;
            vPres.fecha:=dt.df;
            


            -- 12 meses desde Notificación Denuncia para Caducidad
            if desAnt in ('LIBRO INCOACIÓN','NOT. DENUNCIA') then

                    vPres.dias:=dt.df-dfAnt;
                    vPres.limite:=360;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,12),'dd/mm/rrrr');
                

            -- Tramitación previa a ejectutiva
            else
            
                if i=1 then
                    vPres.dias:=0;
                    vPres.limite:=0;
                    vPres.fechaLimite:=null;
                    vPres.enPlazo:=1;
                elsif i=2 and desAnt='DENUNCIA' then
                    vPres.dias:=dt.df-dfAnt;
                    vPres.limite:=cp.limiteDenunciaMes*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,cp.limiteDenunciaMes),'dd/mm/rrrr');
                else
                    vPres.dias:=dt.df-dfAnt;
                    vPres.limite:=cp.limiteRestoMes*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,cp.limiteRestoMes),'dd/mm/rrrr');
                end if;

            end if;
            
            
            --Incrementa en un mes la fecha límite desde FECHA ACTUAL
            if vPres.tipoFecha='FECHA ACTUAL' then
              vPres.fechaLimite:=trunc(add_months(vPres.fechaLimite,1));
            end if;
            
            
            
            if vPres.fechaLimite is not null and vPres.fechaLimite<vPres.fecha then
                vPres.enPlazo:=0;
            else
                vPres.enPlazo:=1;
            end if;
            



            pipe row(vPres);
            
            i:=i+1;
            dfAnt:=dt.df;
            desAnt:=dt.des;
            
            
            

        end loop;

    end loop;

    return;

end;



----------------------------------------------
-- Calcular Prescripción
----------------------------------------------
procedure calcularPrescripcion(vMulid number) is

    cursor cPres is
    select 
    vMul.flibcondat,
    to_date(m.mulfec,'dd/mm/rrrr') fechaDenuncia,
    vmul.fnotden,
    vmul.fecomcon,    
    vmul.fcomcon,
    vmul.fnotreq,
    vmul.faleg,
    vmul.fnotdenIntento,
    vmul.fnotreqIntento,
    decode(i.infgrado,'L',4,'G',7,'M',7,0) limiteRestoMes,
    decode(i.infgrado,'L',3,'G',6,'M',6,0) limiteDenunciaMes
    from alba.infracciones i, alba.multas m 
    where m.infid=i.infid
    and m.mulid=vMulid;
    
    dfAnt date:=null;
    desAnt varchar2(200):='';
    i number:=1;

    vPresLocal prescripcion;
    
    -- Control de Caducidad
    vNotificacionDenuncia date:=null;
    vFechaCaducidad date:=null;
    vSalida varchar2(1000):='';
    vPrescrito number:=0;

begin

    vSalida:='';

    for cp in cPres loop
    
        for dt in
        (
            select des, df
            from
                (
                
                /* 22/01/2014
                    Suprimir el indicador ¿POSIBLEMENTE PRESCRIPTO¿ actualmente vigente y sustituirlo por los 
                    siguientes indicadores: 
                    
                    1.- PRESCRITO: 
                    
                    - Para infracciones leves, transcurridos 3 meses desde la ¿fecha de la denuncia¿ hasta la 
                    ¿fecha de notificación¿ o intentos fallidos de notificación.
                    - Para infracciones graves y muy graves, transcurridos 6 meses desde la ¿fecha de la 
                    denuncia hasta la ¿fecha de notificación¿ o intentos fallidos de notificación.
                    
                    2.- CADUCADO: 
                    
                    En todos los expedientes transcurrido 1 año (365 días), desde la fecha del 
                    ¿Libro de Incoación¿ o notificación en el acto de la denuncia hasta la fecha 
                    de la ¿Resolución Sancionadora¿. 
                    
                    3.- POSIBLEMENTE CADUCADO: 
                    
                    Expedientes que se han resuelto entre la fecha del libro de Incoación y la 
                    fecha de la Resolución Sancionadora, pero no se ha podido notificar dentro 
                    de ese año. Este indicador ¿posiblemente caducado¿ no debe paralizar la 
                    tramitación de los expedientes y no impedirá su agrupación o notificación.              
                */                
                
                select 'DENUNCIA' des, cp.fechaDenuncia df from dual where cp.fcomcon is null
                union select 'NOT. DENUNCIA' des, cp.fnotden df from dual
                union select 'CONSULTA DATOS IDENTIDAD Y DOMICILIO EN DGT Y PADRÓN' des, cp.flibcondat df from dual
                union select 'ESCRITO COM. CONDUCTOR' des, cp.fecomcon df from dual
                union select 'COMUNICACIÓN CONDUCTOR' des, cp.fcomcon df from dual
                --union select 'NOT. SANCIÓN' des, cp.fnotsan df from dual
                union select 'NOT. REQUERIMIENTO' des, cp.fnotreq df from dual
                --union select 'INF. DEN. SOLICITADO' des, cp.finfd df from dual
                --union select 'INF. DEN. RECIBIDO' des, cp.frinfd df from dual
                union select 'ALEGACIÓN' des, cp.faleg df from dual where cp.faleg<=cp.fnotden or cp.fnotden is null
                --union select 'NOT. RESOLUCIÓN ALEGACIÓN' des, cp.fnotaleg df from dual
                --union select 'RECURSO' des, cp.frec df from dual
                --union select 'NOT. RESOLUCIÓN RECURSO' des, cp.fnotrec df from dual
                union select 'INTENTO NOT. DENUNCIA' des, cp.fnotdenIntento df from dual
                --union select 'INTENTO NOT. SANCIÓN' des, cp.fnotsanIntento df from dual
                union select 'INTENTO NOT. REQUERIMIENTO' des, cp.fnotreqIntento df from dual
                --union select 'INTENTO NOT. RESOL. ALEGACIÓN' des, cp.fnotalegIntento df from dual
                --union select 'INTENTO NOT. RESOL. RECURSO' des, cp.fnotrecIntento df from dual
                --union select 'SANCIÓN FIRME' des, cp.ffirmeza df from dual
                --union select 'RECURSO CONTRA LA P.A.' des, cp.frpa df from dual
                --union select 'NOT. PROVIDENCIA APREMIO' des, cp.fnotpa df from dual
                union select 'FECHA ACTUAL' des, trunc(sysdate) df from dual where cp.fnotden is null


                --VERSIONES ANTERIORES DE INTENTOS DE NOTIFICACIÓN
                /*union select
                decode(n.ntfresultado,'M3','','M4','','M2','','INTENTO ')||
                decode(a.atocodigo,'MULTDC','NOT.REQ.VERSIÓN ANTERIOR','NOT.DEN.VERSIÓN ANTERIOR') des,
                to_date(n.ntffecnot,'dd/mm/rrrr') df
                from alba.actosnotificacion a, alba.cabremesas c, alba.remesas r,
                    alba.notificaciones n, alba.propuestasnotificacion p
                where p.pntid=n.pntid
                and n.ntfid=r.ntfid
                and r.crmid=c.crmid
                and p.atoid=a.atoid
                and p.pntorigen='I'
                and n.ntffecnot is not null
                and n.ntfcrmid=c.crmid
                and p.pntorigenid in
                    (select mulid
                    from alba.multas m
                    where m.seoidexp in
                    (select m2.seoidexp
                    from alba.multas m2
                    where m2.mulid=vMulid)
                    and m.mulid<>(select max(m3.mulid)
                    from alba.multas m3
                    where m3.mulid=vMulid))
                and a.atocodigo in
                ('MULTDN','MULTDP','MULTDT','MULTDU','MULTDE','MULTDM','MULTDA','MULTDV','MULTDR','MULTDC')
                --and n.ntfresultado in ('M3','M4','M2')*/

                )
                
            where df is not null
            order by df asc
        )
        loop

            vPres:=vPresLocal;
            vPres.tipoFecha:=dt.des;
            vPres.fecha:=dt.df;

            -- 12 meses desde Notificación Denuncia para Caducidad
            if desAnt in ('ALEG','NOT') then

                    vPres.dias:=dt.df-dfAnt;
                    vPres.limite:=365;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,12),'dd/mm/rrrr');
                
            -- Tramitación previa a ejectutiva
            else
            
                if i=1 then
                    vPres.dias:=0;
                    vPres.limite:=0;
                    vPres.fechaLimite:=null;
                    vPres.enPlazo:=1;
                elsif i=2 and desAnt='DENUNCIA' then
                    vPres.dias:=dt.df-dfAnt;
                    vPres.limite:=cp.limiteDenunciaMes*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,cp.limiteDenunciaMes),'dd/mm/rrrr');
                else
                    vPres.dias:=dt.df-dfAnt;
                    vPres.limite:=cp.limiteRestoMes*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,cp.limiteRestoMes),'dd/mm/rrrr');
                end if;

            end if;


            --Incrementa en un mes la fecha límite desde FECHA ACTUAL
            if vPres.tipoFecha='FECHA ACTUAL' then
              vPres.fechaLimite:=trunc(add_months(vPres.fechaLimite,1));
              vMul.fprescripcion:=vPres.fechaLimite;
            else 
              vMul.fprescripcion:=null;
            end if;    


            if vPres.fechaLimite is not null and vPres.fechaLimite<vPres.fecha then
                vPres.enPlazo:=0;
                vPrescrito:=1;
            else
                vPres.enPlazo:=1;
            end if;


            --pipe row(vPres);
            vSalida:=vSalida ||
            to_char(vPres.tipoFecha) || '#' ||
            to_char(vPres.fecha) || '#' ||
            to_char(vPres.dias) || '#' ||
            to_char(vPres.limite) || '#' ||
            to_char(vPres.fechaLimite) || '#' ||
            to_char(vPres.enPlazo) || '¬';    
            
            i:=i+1;
            dfAnt:=dt.df;
            desAnt:=dt.des;
            
        end loop;

    end loop;

    vmul.cpres:=vSalida;
    vmul.pres:=vPrescrito;
    
    
    if vmul.pres=1 and vmul.cad=0 and vMul.cestado<30000 
    and vMul.cestado not in (4920,4923,4924) 
    and vmul.cestado not in(2300 ,2310 ,2320)
    -- CÁLCULO PRESCRIPCIÓN. EXCLUYE EXPEDIENTES CON ESCRITOS ESTIMADOS
    and not(vmul.cestado>=7100 and vmul.cestado<=7399)
    then
        vMul.cestado:=vMul.cestado+40000;
        vMul.estado:=vMul.estado;
    end if;    


end;


----------------------------------------------
-- Calcular Prescripción Old
----------------------------------------------
procedure calcularPrescripcionOld(vMulid number) is

    cursor cPres is
    select 
    vMul.flibcondat,
    to_date(m.mulfec,'dd/mm/rrrr') fechaDenuncia,
    vmul.fnotden,
    vmul.fecomcon,    
    vmul.fcomcon,
    vmul.fnotreq,
    vmul.faleg,
    vmul.fnotdenIntento,
    vmul.fnotreqIntento,
    decode(i.infgrado,'L',4,'G',7,'M',7,0) limiteRestoMes,
    decode(i.infgrado,'L',3,'G',6,'M',6,0) limiteDenunciaMes
    from alba.infracciones i, alba.multas m 
    where m.infid=i.infid
    and m.mulid=vMulid;
    
    dfAnt date:=null;
    desAnt varchar2(200):='';
    i number:=1;

    vPresLocal prescripcion;
    
    -- Control de Caducidad
    vNotificacionDenuncia date:=null;
    vFechaCaducidad date:=null;
    vSalida varchar2(1000):='';
    vPrescrito number:=0;

begin

    vSalida:='';

    for cp in cPres loop
    
        for dt in
        (
            select des, df
            from
                (
                
                /* 22/01/2014
                    Suprimir el indicador ¿POSIBLEMENTE PRESCRIPTO¿ actualmente vigente y sustituirlo por los 
                    siguientes indicadores: 
                    
                    1.- PRESCRITO: 
                    
                    - Para infracciones leves, transcurridos 3 meses desde la ¿fecha de la denuncia¿ hasta la 
                    ¿fecha de notificación¿ o intentos fallidos de notificación.
                    - Para infracciones graves y muy graves, transcurridos 6 meses desde la ¿fecha de la 
                    denuncia hasta la ¿fecha de notificación¿ o intentos fallidos de notificación.
                    
                    2.- CADUCADO: 
                    
                    En todos los expedientes transcurrido 1 año (365 días), desde la fecha del 
                    ¿Libro de Incoación¿ o notificación en el acto de la denuncia hasta la fecha 
                    de la ¿Resolución Sancionadora¿. 
                    
                    3.- POSIBLEMENTE CADUCADO: 
                    
                    Expedientes que se han resuelto entre la fecha del libro de Incoación y la 
                    fecha de la Resolución Sancionadora, pero no se ha podido notificar dentro 
                    de ese año. Este indicador ¿posiblemente caducado¿ no debe paralizar la 
                    tramitación de los expedientes y no impedirá su agrupación o notificación.              
                */                
                
                select 'DENUNCIA' des, cp.fechaDenuncia df from dual where cp.fcomcon is null
                union select 'NOT. DENUNCIA' des, cp.fnotden df from dual
                union select 'ESCRITO COM. CONDUCTOR' des, cp.fecomcon df from dual
                union select 'COMUNICACIÓN CONDUCTOR' des, cp.fcomcon df from dual
                --union select 'NOT. SANCIÓN' des, cp.fnotsan df from dual
                union select 'NOT. REQUERIMIENTO' des, cp.fnotreq df from dual
                --union select 'INF. DEN. SOLICITADO' des, cp.finfd df from dual
                --union select 'INF. DEN. RECIBIDO' des, cp.frinfd df from dual
                union select 'ALEGACIÓN' des, cp.faleg df from dual where cp.faleg<=cp.fnotden or cp.fnotden is null
                --union select 'NOT. RESOLUCIÓN ALEGACIÓN' des, cp.fnotaleg df from dual
                --union select 'RECURSO' des, cp.frec df from dual
                --union select 'NOT. RESOLUCIÓN RECURSO' des, cp.fnotrec df from dual
                union select 'INTENTO NOT. DENUNCIA' des, cp.fnotdenIntento df from dual
                --union select 'INTENTO NOT. SANCIÓN' des, cp.fnotsanIntento df from dual
                union select 'INTENTO NOT. REQUERIMIENTO' des, cp.fnotreqIntento df from dual
                --union select 'INTENTO NOT. RESOL. ALEGACIÓN' des, cp.fnotalegIntento df from dual
                --union select 'INTENTO NOT. RESOL. RECURSO' des, cp.fnotrecIntento df from dual
                --union select 'SANCIÓN FIRME' des, cp.ffirmeza df from dual
                --union select 'RECURSO CONTRA LA P.A.' des, cp.frpa df from dual
                --union select 'NOT. PROVIDENCIA APREMIO' des, cp.fnotpa df from dual
                union select 'FECHA ACTUAL' des, trunc(sysdate) df from dual where cp.fnotden is null


                --VERSIONES ANTERIORES DE INTENTOS DE NOTIFICACIÓN
                /*union select
                decode(n.ntfresultado,'M3','','M4','','M2','','INTENTO ')||
                decode(a.atocodigo,'MULTDC','NOT.REQ.VERSIÓN ANTERIOR','NOT.DEN.VERSIÓN ANTERIOR') des,
                to_date(n.ntffecnot,'dd/mm/rrrr') df
                from alba.actosnotificacion a, alba.cabremesas c, alba.remesas r,
                    alba.notificaciones n, alba.propuestasnotificacion p
                where p.pntid=n.pntid
                and n.ntfid=r.ntfid
                and r.crmid=c.crmid
                and p.atoid=a.atoid
                and p.pntorigen='I'
                and n.ntffecnot is not null
                and n.ntfcrmid=c.crmid
                and p.pntorigenid in
                    (select mulid
                    from alba.multas m
                    where m.seoidexp in
                    (select m2.seoidexp
                    from alba.multas m2
                    where m2.mulid=vMulid)
                    and m.mulid<>(select max(m3.mulid)
                    from alba.multas m3
                    where m3.mulid=vMulid))
                and a.atocodigo in
                ('MULTDN','MULTDP','MULTDT','MULTDU','MULTDE','MULTDM','MULTDA','MULTDV','MULTDR','MULTDC')
                --and n.ntfresultado in ('M3','M4','M2')*/

                )
                
            where df is not null
            order by df asc
        )
        loop

            vPres:=vPresLocal;
            vPres.tipoFecha:=dt.des;
            vPres.fecha:=dt.df;

            -- 12 meses desde Notificación Denuncia para Caducidad
            if desAnt in ('ALEG','NOT') then

                    vPres.dias:=dt.df-dfAnt;
                    vPres.limite:=365;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,12),'dd/mm/rrrr');
                
            -- Tramitación previa a ejectutiva
            else
            
                if i=1 then
                    vPres.dias:=0;
                    vPres.limite:=0;
                    vPres.fechaLimite:=null;
                    vPres.enPlazo:=1;
                elsif i=2 and desAnt='DENUNCIA' then
                    vPres.dias:=dt.df-dfAnt;
                    vPres.limite:=cp.limiteDenunciaMes*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,cp.limiteDenunciaMes),'dd/mm/rrrr');
                else
                    vPres.dias:=dt.df-dfAnt;
                    vPres.limite:=cp.limiteRestoMes*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,cp.limiteRestoMes),'dd/mm/rrrr');
                end if;

            end if;


            --Incrementa en un mes la fecha límite desde FECHA ACTUAL
            if vPres.tipoFecha='FECHA ACTUAL' then
              vPres.fechaLimite:=trunc(add_months(vPres.fechaLimite,1));
              vMul.fprescripcion:=vPres.fechaLimite;
            else 
              vMul.fprescripcion:=null;
            end if;    


            if vPres.fechaLimite is not null and vPres.fechaLimite<vPres.fecha then
                vPres.enPlazo:=0;
                vPrescrito:=1;
            else
                vPres.enPlazo:=1;
            end if;


            --pipe row(vPres);
            vSalida:=vSalida ||
            to_char(vPres.tipoFecha) || '#' ||
            to_char(vPres.fecha) || '#' ||
            to_char(vPres.dias) || '#' ||
            to_char(vPres.limite) || '#' ||
            to_char(vPres.fechaLimite) || '#' ||
            to_char(vPres.enPlazo) || '¬';    
            
            i:=i+1;
            dfAnt:=dt.df;
            desAnt:=dt.des;
            
        end loop;

    end loop;

    vmul.cpres:=vSalida;
    vmul.pres:=vPrescrito;
    
    
    if vmul.pres=1 and vmul.cad=0 and vMul.cestado<30000 
    and vMul.cestado not in (4920,4923,4924) 
    and vmul.cestado not in(2300 ,2310 ,2320)
    -- CÁLCULO PRESCRIPCIÓN. EXCLUYE EXPEDIENTES CON ESCRITOS ESTIMADOS
    and not(vmul.cestado>=7100 and vmul.cestado<=7399)
    then
        vMul.cestado:=vMul.cestado+400000;
        vMul.estado:=vMul.estado;
    end if; 
    
end;


----------------------------------------------
-- Calcular Caducidad
----------------------------------------------
procedure calcularCaducidad(vMulid number) is

    cursor cPres is
    select to_date(m.mulfec,'dd/mm/rrrr') fechaDenuncia,
    vmul.flibinc,
    vmul.fnotden,
    vmul.fnotsan,
    vmul.ffirmeza,
    vmul.fnotalegIntento,
    decode(i.infgrado,'L',3,'G',6,'M',6,0) limiteRestoMes,
    decode(i.infgrado,'L',3,'G',6,'M',6,0) limiteDenunciaMes
    from alba.infracciones i, alba.multas m 
    where m.infid=i.infid
    and m.mulid=vMulid;    
    
    dfAnt date:=null;
    desAnt varchar2(200):='';
    i number:=1;

    vPresLocal prescripcion;
    
    -- Control de Caducidad
    vNotificacionDenuncia date:=null;
    vFechaCaducidad date:=null;
    vSalida varchar2(1000):='';
    vCaducado number:=0;    

begin

    for cp in cPres loop
    
        for dt in
        (
            select des, df
            from
                (
                
                /* 22/01/2014
                    Suprimir el indicador ¿POSIBLEMENTE PRESCRIPTO¿ actualmente vigente y sustituirlo por los 
                    siguientes indicadores: 
                    
                    1.- PRESCRITO: 
                    
                    - Para infracciones leves, transcurridos 3 meses desde la ¿fecha de la denuncia¿ hasta la 
                    ¿fecha de notificación¿ o intentos fallidos de notificación.
                    - Para infracciones graves y muy graves, transcurridos 6 meses desde la ¿fecha de la 
                    denuncia hasta la ¿fecha de notificación¿ o intentos fallidos de notificación.
                    
                    2.- CADUCADO: 
                    
                    En todos los expedientes transcurrido 1 año (365 días), desde la fecha del 
                    ¿Libro de Incoación¿ o notificación en el acto de la denuncia hasta la fecha 
                    de la ¿Resolución Sancionadora¿. 
                    
                    3.- POSIBLEMENTE CADUCADO: 
                    
                    Expedientes que se han resuelto entre la fecha del libro de Incoación y la 
                    fecha de la Resolución Sancionadora, pero no se ha podido notificar dentro 
                    de ese año. Este indicador ¿posiblemente caducado¿ no debe paralizar la 
                    tramitación de los expedientes y no impedirá su agrupación o notificación.              
                */                
                
                select 'LIBRO INCOACIÓN' des, cp.flibinc df from dual --where cp.flibinc<=cp.ffirmeza
                union select 'NOT. DENUNCIA' des, cp.fnotden df from dual where cp.flibinc is null
                union select 'SANCIÓN' des, cp.fnotsan df from dual where cp.fnotalegIntento is null
                --union select 'RECURSO CONTRA LA P.A.' des, cp.frpa df from dual
                --union select 'NOT. PROVIDENCIA APREMIO' des, cp.fnotpa df from dual
                union select 'INTENTO NOT. RESOL. ALEGACIÓN' des, cp.fnotalegIntento df from dual
                union select 'FECHA ACTUAL' des, trunc(sysdate) df from dual where cp.fnotsan is null 

                )
                
            where df is not null
            order by df asc
        )
        loop
        
            vPres:=vPresLocal;
            vPres.tipoFecha:=dt.des;
            vPres.fecha:=dt.df;

            -- 12 meses desde Notificación Denuncia para Caducidad
            if desAnt in ('LIBRO INCOACIÓN','NOT. DENUNCIA') then

                    vPres.dias:=dt.df-dfAnt;
                    vPres.limite:=360;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,12),'dd/mm/rrrr');

            -- Tramitación previa a ejectutiva
            else
            
                if i=1 then
                    vPres.dias:=0;
                    vPres.limite:=0;
                    vPres.fechaLimite:=null;
                    vPres.enPlazo:=1;
                elsif i=2 and desAnt='DENUNCIA' then
                    vPres.dias:=dt.df-dfAnt;
                    vPres.limite:=cp.limiteDenunciaMes*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,cp.limiteDenunciaMes),'dd/mm/rrrr');
                else
                    vPres.dias:=dt.df-dfAnt;
                    vPres.limite:=cp.limiteRestoMes*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,cp.limiteRestoMes),'dd/mm/rrrr');
                end if;

            end if;
            
            --Incrementa en un mes la fecha límite desde FECHA ACTUAL
            if vPres.tipoFecha='FECHA ACTUAL' then
              vPres.fechaLimite:=trunc(add_months(vPres.fechaLimite,1));
              if vMul.fprescripcion is null then
                vMul.fprescripcion:=vPres.fechaLimite;
              end if;
            else 
              vMul.fprescripcion:=null;
            end if;
            
            if vPres.fechaLimite is not null and vPres.fechaLimite<vPres.fecha then
                vPres.enPlazo:=0;
                vCaducado:=1;
            else
                vPres.enPlazo:=1;
            end if;
            
            
            --pipe row(vPres);
            vSalida:=vSalida ||
            to_char(vPres.tipoFecha) || '#' ||
            to_char(vPres.fecha) || '#' ||
            to_char(vPres.dias) || '#' ||
            to_char(vPres.limite) || '#' ||
            to_char(vPres.fechaLimite) || '#' ||
            to_char(vPres.enPlazo) || '¬';    
            
            i:=i+1;
            dfAnt:=dt.df;
            desAnt:=dt.des;
            
        end loop;

    end loop;

    vmul.ccad:=vSalida;
    vmul.cad:=vCaducado;
    
    
    if vmul.pres=0 and vmul.cad=1 and vMul.cestado<30000
    and vMul.cestado not in (4910,4920,4923,4924) then
        vMul.cestado:=vMul.cestado+40000;
        vMul.estado:=vMul.estado;
    end if;    


end;






----------------------------------------------
-- MUESTRA NUEVO CÁLCULO PRESCRIPCION TRÁFICO 
----------------------------------------------
function muestraCalculoPrescripcionSim(vMulid number) return prescripcion_lista pipelined is

    cursor cPres is
    select to_date(m.mulfec,'dd/mm/rrrr') fechaDenuncia, e.*,
    decode(i.infgrado,'L',4,'G',7,'M',7,0) limiteRestoMes,
    decode(i.infgrado,'L',3,'G',6,'M',6,0) limiteDenunciaMes
    from alba.infracciones i, alba.multas m, table(trml.emultas(m.mulid)) e
    where m.infid=i.infid
    and m.mulid=vMulid;
    
    dfAnt date:=null;
    desAnt varchar2(200):='';
    i number:=1;

    vPresLocal prescripcion;
    
    -- Control de Caducidad
    vNotificacionDenuncia date:=null;
    vFechaCaducidad date:=null;

begin

    for cp in cPres loop
    
        for dt in
        (
            select des, df
            from
                (
                
                /* 22/01/2014
                    Suprimir el indicador ¿POSIBLEMENTE PRESCRIPTO¿ actualmente vigente y sustituirlo por los 
                    siguientes indicadores: 
                    
                    1.- PRESCRITO: 
                    
                    - Para infracciones leves, transcurridos 3 meses desde la ¿fecha de la denuncia¿ hasta la 
                    ¿fecha de notificación¿ o intentos fallidos de notificación.
                    - Para infracciones graves y muy graves, transcurridos 6 meses desde la ¿fecha de la 
                    denuncia hasta la ¿fecha de notificación¿ o intentos fallidos de notificación.
                    
                    2.- CADUCADO: 
                    
                    En todos los expedientes transcurrido 1 año (365 días), desde la fecha del 
                    ¿Libro de Incoación¿ o notificación en el acto de la denuncia hasta la fecha 
                    de la ¿Resolución Sancionadora¿. 
                    
                    3.- POSIBLEMENTE CADUCADO: 
                    
                    Expedientes que se han resuelto entre la fecha del libro de Incoación y la 
                    fecha de la Resolución Sancionadora, pero no se ha podido notificar dentro 
                    de ese año. Este indicador ¿posiblemente caducado¿ no debe paralizar la 
                    tramitación de los expedientes y no impedirá su agrupación o notificación.              
                */                
                
                select 'DENUNCIA' des, cp.fechaDenuncia df from dual where cp.fcomcon is null
                union select 'NOT. DENUNCIA' des, cp.fnotden df from dual
                union select 'CONSULTA DATOS IDENTIDAD Y DOMICILIO EN DGT Y PADRÓN' des, cp.flibcondat df from dual
                union select 'ESCRITO COM. CONDUCTOR' des, cp.fecomcon df from dual
                union select 'COMUNICACIÓN CONDUCTOR' des, cp.fcomcon df from dual
                --union select 'NOT. SANCIÓN' des, cp.fnotsan df from dual
                union select 'NOT. REQUERIMIENTO' des, cp.fnotreq df from dual
                --union select 'INF. DEN. SOLICITADO' des, cp.finfd df from dual
                --union select 'INF. DEN. RECIBIDO' des, cp.frinfd df from dual
                union select 'ALEGACIÓN' des, cp.faleg df from dual where cp.faleg<=cp.fnotden or cp.fnotden is null
                --union select 'NOT. RESOLUCIÓN ALEGACIÓN' des, cp.fnotaleg df from dual
                --union select 'RECURSO' des, cp.frec df from dual
                --union select 'NOT. RESOLUCIÓN RECURSO' des, cp.fnotrec df from dual
                union select 'INTENTO NOT. DENUNCIA' des, cp.fnotdenIntento df from dual
                --union select 'INTENTO NOT. SANCIÓN' des, cp.fnotsanIntento df from dual
                union select 'INTENTO NOT. REQUERIMIENTO' des, cp.fnotreqIntento df from dual
                --union select 'INTENTO NOT. RESOL. ALEGACIÓN' des, cp.fnotalegIntento df from dual
                --union select 'INTENTO NOT. RESOL. RECURSO' des, cp.fnotrecIntento df from dual
                --union select 'SANCIÓN FIRME' des, cp.ffirmeza df from dual
                --union select 'RECURSO CONTRA LA P.A.' des, cp.frpa df from dual
                --union select 'NOT. PROVIDENCIA APREMIO' des, cp.fnotpa df from dual
                union select 'FECHA ACTUAL' des, trunc(sysdate) df from dual where cp.fnotden is null


                --VERSIONES ANTERIORES DE INTENTOS DE NOTIFICACIÓN
                /*
                union select
                decode(n.ntfresultado,'M3','','M4','','M2','','INTENTO ')||
                decode(a.atocodigo,'MULTDC','NOT.REQ.VERSIÓN ANTERIOR','NOT.DEN.VERSIÓN ANTERIOR') des,
                to_date(n.ntffecnot,'dd/mm/rrrr') df
                from alba.actosnotificacion a, alba.cabremesas c, alba.remesas r,
                    alba.notificaciones n, alba.propuestasnotificacion p
                where p.pntid=n.pntid
                and n.ntfid=r.ntfid
                and r.crmid=c.crmid
                and p.atoid=a.atoid
                and p.pntorigen='I'
                and n.ntffecnot is not null
                and n.ntfcrmid=c.crmid
                and p.pntorigenid in
                    (select mulid
                    from alba.multas m
                    where m.seoidexp in
                    (select m2.seoidexp
                    from alba.multas m2
                    where m2.mulid=vMulid)
                    and m.mulid<>(select max(m3.mulid)
                    from alba.multas m3
                    where m3.mulid=vMulid))
                and a.atocodigo in
                ('MULTDN','MULTDP','MULTDT','MULTDU','MULTDE','MULTDM','MULTDA','MULTDV','MULTDR','MULTDC')
                --and n.ntfresultado in ('M3','M4','M2')        */        
                )
                
                
            where df is not null
            order by df asc
        )
        loop
        
        

            vPres:=vPresLocal;

            vPres.tipoFecha:=dt.des;
            vPres.fecha:=dt.df;
            


            -- 12 meses desde Notificación Denuncia para Caducidad
            if desAnt in ('ALEG','NOT') then

                    vPres.dias:=dt.df-dfAnt;
                    vPres.limite:=365;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,12),'dd/mm/rrrr');
                

            -- Tramitación previa a ejectutiva
            else
            
                if i=1 then
                    vPres.dias:=0;
                    vPres.limite:=0;
                    vPres.fechaLimite:=null;
                    vPres.enPlazo:=1;
                elsif i=2 and desAnt='DENUNCIA' then
                    vPres.dias:=dt.df-dfAnt;
                    vPres.limite:=cp.limiteDenunciaMes*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,cp.limiteDenunciaMes),'dd/mm/rrrr');
                else
                    vPres.dias:=dt.df-dfAnt;
                    vPres.limite:=cp.limiteRestoMes*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,cp.limiteRestoMes),'dd/mm/rrrr');
                end if;

            end if;
            
            
            --Incrementa en un mes la fecha límite desde FECHA ACTUAL
            if vPres.tipoFecha='FECHA ACTUAL' then
              vPres.fechaLimite:=trunc(add_months(vPres.fechaLimite,1));
            end if;            

            
            if vPres.fechaLimite is not null and vPres.fechaLimite<vPres.fecha then
                vPres.enPlazo:=0;
            else
                vPres.enPlazo:=1;
            end if;
            



            pipe row(vPres);
            
            i:=i+1;
            dfAnt:=dt.df;
            desAnt:=dt.des;
            
            
            

        end loop;

    end loop;

    return;

end;


---------------------------------------
-- MUESTRA CÁLCULO PRESCRIPCION TRÁFICO
---------------------------------------
function muestraCalculoPrescripcion(vMulid number) return prescripcion_lista pipelined is

    cursor cPres is
    select to_date(m.mulfec,'dd/mm/rrrr') fechaDenuncia, e.*,
    decode(i.infgrado,'L',4,'G',7,'M',7,0) limiteRestoMes,
    decode(i.infgrado,'L',3,'G',6,'M',6,0) limiteDenunciaMes
    from alba.infracciones i, alba.multas m, table(trml.emultas(m.mulid)) e
    where m.infid=i.infid
    and m.mulid=vMulid;
    
    dfAnt date:=null;
    desAnt varchar2(200):='';
    i number:=1;

    vPresLocal prescripcion;
    
    -- Control de Caducidad
    vNotificacionDenuncia date:=null;
    vFechaCaducidad date:=null;

begin

    -- Variables devueltas por emultas
    -- vMul.forigenCalculoPrescripcion
    -- vMul.fprescripcion

    -- Variables protegidas de caracter temporal
    -- vMul2.vFechaOrigenPrescripcion
    -- vMul2.vFechaPrescripcion

    for cp in cPres loop
    
        for dt in
        (
            select des, df
            from
                (
                select 'DENUNCIA' des, cp.fechaDenuncia df from dual where cp.fcomcon is null
                union select 'NOT. DENUNCIA' des, cp.fnotden df from dual
                union select 'COMUNICACIÓN CONDUCTOR' des, cp.fcomcon df from dual
                union select 'NOT. SANCIÓN' des, cp.fnotsan df from dual
                union select 'NOT. REQUERIMIENTO' des, cp.fnotreq df from dual
                union select 'INF. DEN. SOLICITADO' des, cp.finfd df from dual
                union select 'INF. DEN. RECIBIDO' des, cp.frinfd df from dual
                union select 'ALEGACIÓN' des, cp.faleg df from dual
                union select 'NOT. RESOLUCIÓN ALEGACIÓN' des, cp.fnotaleg df from dual
                union select 'RECURSO' des, cp.frec df from dual
                union select 'NOT. RESOLUCIÓN RECURSO' des, cp.fnotrec df from dual
                union select 'INTENTO NOT. DENUNCIA' des, cp.fnotdenIntento df from dual
                union select 'INTENTO NOT. SANCIÓN' des, cp.fnotsanIntento df from dual
                union select 'INTENTO NOT. REQUERIMIENTO' des, cp.fnotreqIntento df from dual
                union select 'INTENTO NOT. RESOL. ALEGACIÓN' des, cp.fnotalegIntento df from dual
                union select 'INTENTO NOT. RESOL. RECURSO' des, cp.fnotrecIntento df from dual
                union select 'SANCIÓN FIRME' des, cp.ffirmeza df from dual
                union select 'RECURSO CONTRA LA P.A.' des, cp.frpa df from dual
                union select 'NOT. PROVIDENCIA APREMIO' des, cp.fnotpa df from dual
                union select 'FECHA ACTUAL' des, trunc(sysdate) df from dual
                
                
                /*union
                select decode(e.tidomid,1,'GRABACIÓN ALEGACIÓN',2,'GRABACIÓN RECURSO',
                3,'GRABACIÓN RER','GRABACIÓN I.CONDUCTOR') des, trunc(e.esmfgrabacion) df
                from alba.escritosmultas e
                where e.tidomid in (1,2,3,21)
                and e.mulid=vMulid*/
                
                /*
                union
                -- Publicación en BOP
                select upper(t.lrtdescripcion) || ' VER.ANT.' des, to_date(l.lbrfirma,'dd/mm/rrrr') df
                from alba.libro_resol_tipos t, alba.libro_resoluciones l, alba.det_libro_resol d
                where d.lbrid=l.lbrid
                and l.lrtid=t.lrtid
                and t.lrtdescorta
                in ('91','915','92','925','93','935','94','945','95','955','96','965')
                and d.mulid in
                (select mulid
                from alba.multas m
                where m.seoidexp in
                (select m2.seoidexp
                from alba.multas m2
                where m2.mulid=vMulid)
                and m.mulid<>(select max(m3.mulid)
                from alba.multas m3
                where m3.mulid=vMulid))
                union
                -- Notificaciones Denuncia y Requerimiento Versiones Anteriores
                select
                decode(n.ntfresultado,'M3','','M4','','M2','','INTENTO ')||
                decode(a.atocodigo,'MULTDC','NOT.REQ.VERSIÓN ANTERIOR','NOT.DEN.VERSIÓN ANTERIOR') des,
                to_date(n.ntffecnot,'dd/mm/rrrr') df
                from alba.actosnotificacion a, alba.cabremesas c, alba.remesas r,
                    alba.notificaciones n, alba.propuestasnotificacion p
                where p.pntid=n.pntid
                and n.ntfid=r.ntfid
                and r.crmid=c.crmid
                and p.atoid=a.atoid
                and p.pntorigen='I'
                and n.ntffecnot is not null
                and n.ntfcrmid=c.crmid
                and p.pntorigenid in
                    (select mulid
                    from alba.multas m
                    where m.seoidexp in
                    (select m2.seoidexp
                    from alba.multas m2
                    where m2.mulid=vMulid)
                    and m.mulid<>(select max(m3.mulid)
                    from alba.multas m3
                    where m3.mulid=vMulid))
                and a.atocodigo in
                ('MULTDN','MULTDP','MULTDT','MULTDU','MULTDE','MULTDM','MULTDA','MULTDV','MULTDR','MULTDC')
                --and n.ntfresultado in ('M3','M4','M2')
                */
                )
                
            where df is not null
            order by df asc
        )
        loop
        
        

            vPres:=vPresLocal;

            vPres.tipoFecha:=dt.des;
            vPres.fecha:=dt.df;
            

            -- Ejecutiva
            if desAnt='SANCIÓN FIRME' or dt.des='NOT. PROVIDENCIA APREMIO' then
            
                  if cp.origendenuncia=5 then
                        vPres.dias:=dt.df-dfAnt;
                        vPres.limite:=48*30;
                        vPres.fechaLimite:=to_date(add_months(dfAnt,48),'dd/mm/rrrr');
                  else
                        vPres.dias:=dt.df-dfAnt;
                        vPres.limite:=12*30;
                        vPres.fechaLimite:=to_date(add_months(dfAnt,12),'dd/mm/rrrr');
                  end if;

            -- Recurso » 7 meses para resolver
            elsif desAnt in ('RECURSO','GRABACIÓN RECURSO') then

                    vPres.dias:=dt.df-dfAnt;
                    vPres.limite:=12*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,12),'dd/mm/rrrr');


            -- Rec.Ext.Rev » 12 meses para resolver
            elsif desAnt in ('RECURSO EXT. REVISIÓN','GRABACIÓN RER') then

                    vPres.dias:=dt.df-dfAnt;
                    vPres.limite:=12*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,12),'dd/mm/rrrr');
                    
            -- Providencia de Apremio » 12 meses para resolver
            elsif desAnt in ('NOT. PROVIDENCIA APREMIO','RECURSO CONTRA LA P.A.') then

                    vPres.dias:=dt.df-dfAnt;
                    vPres.limite:=12*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,12),'dd/mm/rrrr');
            -- Providencia de Apremio » 12 meses para resolver
            elsif desAnt in ('NOT. RESOLUCIÓN RECURSO','RECURSO CONTRA LA P.A.') then

                    vPres.dias:=dt.df-dfAnt;
                    vPres.limite:=12*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,12),'dd/mm/rrrr');                    

            -- Tramitación previa a ejectutiva
            else
            
                if i=1 then
                    vPres.dias:=0;
                    vPres.limite:=0;
                    vPres.fechaLimite:=null;
                    vPres.enPlazo:=1;
                elsif i=2 and desAnt='DENUNCIA' then
                    vPres.dias:=dt.df-dfAnt;
                    vPres.limite:=cp.limiteDenunciaMes*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,cp.limiteDenunciaMes),'dd/mm/rrrr');
                else
                    vPres.dias:=dt.df-dfAnt;
                    vPres.limite:=cp.limiteRestoMes*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,cp.limiteRestoMes),'dd/mm/rrrr');
                end if;

            end if;
            
            if vPres.fechaLimite is not null and vPres.fechaLimite<vPres.fecha then
                vPres.enPlazo:=0;
            else
                vPres.enPlazo:=1;
            end if;
            
            /*
            -------------------------------------------
            INICIO » CONTROL DE CADUCIDAD DESHABILITADO
            -------------------------------------------
            
            -- Control de Caducidad 1
            -- Finaliza cálculo si encuentra caducidad
            if dt.des='FECHA ACTUAL' and
            vNotificacionDenuncia is not null and vFechaCaducidad is not null
            then
                if vFechaCaducidad>vNotificacionDenuncia
                and vFechaCaducidad-vNotificacionDenuncia>365 then
                    vPres.enPlazo:=3;
                end if;
            end if;
            
            
            -- Control de Caducidad 2
            if dt.des='FECHA ACTUAL' then
                vNotificacionDenuncia:=to_date((nvl(cp.flibinc,cp.fechaDenuncia)),'dd/mm/rrrr');
                vFechaCaducidad:=to_date(nvl(nvl(cp.flibaleg,cp.flibsan),sysdate),'dd/mm/rrrr');

                -- Finaliza cálculo si encuentra caducidad
                if vNotificacionDenuncia is not null and vFechaCaducidad is not null
                then
                    if vFechaCaducidad>vNotificacionDenuncia
                    and vFechaCaducidad-vNotificacionDenuncia>365 then
                        vPres.enPlazo:=3;
                    end if;
                end if;

            end if;
            
            ----------------------------------------
            FIN » CONTROL DE CADUCIDAD DESHABILITADO
            ----------------------------------------
            */


            pipe row(vPres);
            
            i:=i+1;
            dfAnt:=dt.df;
            desAnt:=dt.des;
            
            
            

        end loop;

    end loop;

    return;

    /*vMul.forigenCalculoPrescripcion:=vMul2.vFechaOrigenPrescripcion;

    vMul2.vFechaPrescripcion:=to_date(add_months(vMul2.vFechaOrigenPrescripcion,vMul2.vGrado),'dd/mm/rrrr');

    vMul.fprescripcion:=vMul2.vFechaPrescripcion;

    if vMul.cestado<30000 and vMul2.vFechaPrescripcion<to_date(sysdate,'dd/mm/rrrr') then
        vMul.cestado:=vMul.cestado+40000;
        vMul.estado:=vMul.estado;
    end if;
    */

end;


----------------------------------------
-- MUESTRA CÁLCULO PRESCRIPCION BOTELLÓN
----------------------------------------
function muestraCalculoPrescripcion4(vMulid number) return prescripcion_lista pipelined is

    cursor cPres is
    select to_date(m.mulfec,'dd/mm/rrrr') fechaDenuncia, e.*,
    decode(e.origendenuncia,10,decode(i.infgrado,'L',12,'G',24,'M',36,0),
                               decode(i.infgrado,'L',12,'G',24,'M',36,0)) limiteRestoMes,
    decode(e.origendenuncia,10,decode(i.infgrado,'L',12,'G',24,'M',36,0),    
                               decode(i.infgrado,'L',12,'G',24,'M',36,0)) limiteDenunciaMes
    from alba.infracciones i, alba.multas m, table(trml.emultas(m.mulid)) e
    where m.infid=i.infid
    and m.mulid=vMulid;
    
    
    dfAnt date:=null;
    desAnt varchar2(200):='';
    i number:=1;

    vPresLocal prescripcion;
    
    -- Control de Caducidad
    vNotificacionDenuncia date:=null;
    vFechaCaducidad date:=null;

begin

    -- Variables devueltas por emultas
    -- vMul.forigenCalculoPrescripcion
    -- vMul.fprescripcion

    -- Variables protegidas de caracter temporal
    -- vMul2.vFechaOrigenPrescripcion
    -- vMul2.vFechaPrescripcion

    for cp in cPres loop
    
        for dt in
        (
            select des, df
            from
                (
                select 'DENUNCIA' des, cp.fechaDenuncia df from dual
                union select 'NOT. DENUNCIA' des, cp.fnotden df from dual
                union select 'COMUNICACIÓN CONDUCTOR' des, cp.fcomcon df from dual
                union select 'NOT. SANCIÓN' des, cp.fnotsan df from dual
                union select 'NOT. REQUERIMIENTO' des, cp.fnotreq df from dual
                union select 'INF. DEN. SOLICITADO' des, cp.finfd df from dual
                union select 'INF. DEN. RECIBIDO' des, cp.frinfd df from dual
                union select 'ALEGACIÓN' des, cp.faleg df from dual
                union select 'NOT. RESOLUCIÓN ALEGACIÓN' des, cp.fnotaleg df from dual
                union select 'RECURSO' des, cp.frec df from dual
                union select 'NOT. RESOLUCIÓN RECURSO' des, cp.fnotrec df from dual
                union select 'RECURSO EXT. REVISIÓN' des, cp.frer df from dual
                union select 'NOT. RESOL. REC. EXT.' des, cp.fnotrer df from dual
                union select 'INTENTO NOT. DENUNCIA' des, cp.fnotdenIntento df from dual
                union select 'INTENTO NOT. SANCIÓN' des, cp.fnotsanIntento df from dual
                union select 'INTENTO NOT. REQUERIMIENTO' des, cp.fnotreqIntento df from dual
                union select 'INTENTO NOT. RESOL. ALEGACIÓN' des, cp.fnotalegIntento df from dual
                union select 'INTENTO NOT. RESOL. RECURSO' des, cp.fnotrecIntento df from dual
                union select 'INTENTO NOT. RESOL. REC. EXT.' des, cp.fnotrerIntento df from dual
                union select 'SANCIÓN FIRME' des, cp.ffirmeza df from dual
                union select 'RECURSO CONTRA LA P.A.' des, cp.frpa df from dual
                union select 'NOT. PROVIDENCIA APREMIO' des, cp.fnotpa df from dual
                union select 'FECHA ACTUAL' des, trunc(sysdate) df from dual
                union
                select decode(e.tidomid,1,'GRABACIÓN ALEGACIÓN',2,'GRABACIÓN RECURSO',
                3,'GRABACIÓN RER','GRABACIÓN I.CONDUCTOR') des, trunc(e.esmfgrabacion) df
                from alba.escritosmultas e
                where e.tidomid in (1,2,3,21)
                and e.mulid=vMulid
                union
                -- Notificaciones Denuncia y Requerimiento Versiones Anteriores
                select
                decode(n.ntfresultado,'M3','','M4','','M2','','INTENTO ')||
                decode(a.atocodigo,'MULTDC','NOT.REQ.VERSIÓN ANTERIOR','NOT.DEN.VERSIÓN ANTERIOR') des,
                to_date(n.ntffecnot,'dd/mm/rrrr') df
                from alba.actosnotificacion a, alba.cabremesas c, alba.remesas r,
                    alba.notificaciones n, alba.propuestasnotificacion p
                where p.pntid=n.pntid
                and n.ntfid=r.ntfid
                and r.crmid=c.crmid
                and p.atoid=a.atoid
                and p.pntorigen='I'
                and n.ntffecnot is not null
                and n.ntfcrmid=c.crmid
                and p.pntorigenid in
                    (select mulid
                    from alba.multas m
                    where m.seoidexp in
                    (select m2.seoidexp
                    from alba.multas m2
                    where m2.mulid=vMulid)
                    and m.mulid<>(select max(m3.mulid)
                    from alba.multas m3
                    where m3.mulid=vMulid))
                and a.atocodigo in
                ('MULTDN','MULTDP','MULTDT','MULTDU','MULTDE','MULTDM','MULTDA','MULTDV','MULTDR','MULTDC')
                --and n.ntfresultado in ('M3','M4','M2')
                )
            where df is not null
            order by df asc
        )
        loop
        
        

            vPres:=vPresLocal;

            vPres.tipoFecha:=dt.des;
            vPres.fecha:=dt.df;
            

            -- Ejecutiva
            if desAnt='SANCIÓN FIRME' then
            
                    vPres.dias:=dt.df-dfAnt;
                    vPres.limite:=12*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,12),'dd/mm/rrrr');
                    
            -- NOTIFICACIÓN DENUNCIA
            elsif desAnt in ('DENUNCIAA') then

                    vPres.dias:=dt.df-dfAnt;
                    vPres.limite:=12*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,12),'dd/mm/rrrr');                    

            -- Recurso » 7 meses para resolver
            elsif desAnt in ('RECURSO','GRABACIÓN RECURSO') then

                    vPres.dias:=dt.df-dfAnt;
                    vPres.limite:=12*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,12),'dd/mm/rrrr');


            -- Rec.Ext.Rev » 12 meses para resolver
            elsif desAnt in ('RECURSO EXT. REVISIÓN','GRABACIÓN RER') then

                    vPres.dias:=dt.df-dfAnt;
                    vPres.limite:=12*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,12),'dd/mm/rrrr');
                    
            -- Providencia de Apremio » 12 meses para resolver
            elsif desAnt in ('NOT. PROVIDENCIA APREMIO','RECURSO CONTRA LA P.A.') then

                    vPres.dias:=dt.df-dfAnt;
                    vPres.limite:=12*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,12),'dd/mm/rrrr');

            -- Tramitación previa a ejectutiva
            else
            
                if i=1 then
                    vPres.dias:=0;
                    vPres.limite:=0;
                    vPres.fechaLimite:=null;
                    vPres.enPlazo:=1;
                elsif i=2 then
                    vPres.dias:=dt.df-dfAnt;
                    vPres.limite:=cp.limiteDenunciaMes*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,cp.limiteDenunciaMes),'dd/mm/rrrr');
                else
                    vPres.dias:=dt.df-dfAnt;
                    vPres.limite:=cp.limiteRestoMes*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,cp.limiteRestoMes),'dd/mm/rrrr');
                end if;

            end if;
            
            if vPres.fechaLimite is not null and vPres.fechaLimite<vPres.fecha then
                vPres.enPlazo:=0;
            else
                vPres.enPlazo:=1;
            end if;
            
            
            ---------------------------------------------
            --INICIO » CONTROL DE CADUCIDAD DESHABILITADO
            ---------------------------------------------
            
            /*if cp.flibinc is not null then
            -- Control de Caducidad 1
            -- Finaliza cálculo si encuentra caducidad
            if dt.des='FECHA ACTUAL' and
            vNotificacionDenuncia is not null and vFechaCaducidad is not null
            then
                if vFechaCaducidad>vNotificacionDenuncia
                and vFechaCaducidad-vNotificacionDenuncia>365 then
                    vPres.enPlazo:=3;
                end if;
            end if;
            
            
            -- Control de Caducidad 2
            if dt.des='FECHA ACTUAL' then
                vNotificacionDenuncia:=to_date(cp.flibinc,'dd/mm/rrrr');
                vFechaCaducidad:=to_date(nvl(nvl(cp.flibaleg,cp.flibsan),sysdate),'dd/mm/rrrr');

                -- Finaliza cálculo si encuentra caducidad
                if vNotificacionDenuncia is not null and vFechaCaducidad is not null
                then
                    if vFechaCaducidad>vNotificacionDenuncia
                    and vFechaCaducidad-vNotificacionDenuncia>365 then
                        vPres.enPlazo:=3;
                    end if;
                end if;

            end if;
            end if;*/
            
            ------------------------------------------
            --FIN » CONTROL DE CADUCIDAD DESHABILITADO
            ----------------------------------------
            


            pipe row(vPres);
            
            i:=i+1;
            dfAnt:=dt.df;
            desAnt:=dt.des;
            
            
            

        end loop;

    end loop;

    return;

    /*vMul.forigenCalculoPrescripcion:=vMul2.vFechaOrigenPrescripcion;

    vMul2.vFechaPrescripcion:=to_date(add_months(vMul2.vFechaOrigenPrescripcion,vMul2.vGrado),'dd/mm/rrrr');

    vMul.fprescripcion:=vMul2.vFechaPrescripcion;

    if vMul.cestado<30000 and vMul2.vFechaPrescripcion<to_date(sysdate,'dd/mm/rrrr') then
        vMul.cestado:=vMul.cestado+40000;
        vMul.estado:=vMul.estado;
    end if;
    */

end;



---------------------------------------------
-- MUESTRA CÁLCULO PRESCRIPCION ANTIVANDÁLICA
---------------------------------------------
function muestraCalculoPrescripcion6(vMulid number) return prescripcion_lista pipelined is

    cursor cPres is
    select to_date(m.mulfec,'dd/mm/rrrr') fechaDenuncia, e.*,
    decode(i.infgrado,'L',12,'G',12,'M',12,0) limiteRestoMes,
    decode(i.infgrado,'L',12,'G',12,'M',12,0) limiteDenunciaMes
    from alba.infracciones i, alba.multas m, table(trml.emultas(m.mulid)) e
    where m.infid=i.infid
    and m.mulid=vMulid;
    
    dfAnt date:=null;
    desAnt varchar2(200):='';
    i number:=1;

    vPresLocal prescripcion;
    
    -- Control de Caducidad
    vNotificacionDenuncia date:=null;
    vFechaCaducidad date:=null;

begin

    -- Variables devueltas por emultas
    -- vMul.forigenCalculoPrescripcion
    -- vMul.fprescripcion

    -- Variables protegidas de caracter temporal
    -- vMul2.vFechaOrigenPrescripcion
    -- vMul2.vFechaPrescripcion

    for cp in cPres loop
    
        for dt in
        (
            select des, df
            from
                (
                select 'DENUNCIA' des, cp.fechaDenuncia df from dual
                union select 'NOT. DENUNCIA' des, cp.fnotden df from dual
                union select 'COMUNICACIÓN CONDUCTOR' des, cp.fcomcon df from dual
                union select 'NOT. SANCIÓN' des, cp.fnotsan df from dual
                union select 'NOT. REQUERIMIENTO' des, cp.fnotreq df from dual
                union select case when cp.fnotden is null then null else 'INF. DEN. SOLICITADO' end des,
                             case when cp.fnotden is null then null else cp.finfd end df from dual
                union select case when cp.fnotden is null then null else 'INF. DEN. RECIBIDO' end des,
                             case when cp.fnotden is null then null else cp.frinfd end df from dual                              
                union select 'ALEGACIÓN' des, cp.faleg df from dual
                union select 'NOT. RESOLUCIÓN ALEGACIÓN' des, cp.fnotaleg df from dual
                union select 'RECURSO' des, cp.frec df from dual
                union select 'NOT. RESOLUCIÓN RECURSO' des, cp.fnotrec df from dual
                union select 'RECURSO EXT. REVISIÓN' des, cp.frer df from dual
                union select 'NOT. RESOL. REC. EXT.' des, cp.fnotrer df from dual
                union select 'INTENTO NOT. DENUNCIA' des, cp.fnotdenIntento df from dual
                union select 'INTENTO NOT. SANCIÓN' des, cp.fnotsanIntento df from dual
                union select 'INTENTO NOT. REQUERIMIENTO' des, cp.fnotreqIntento df from dual
                union select 'INTENTO NOT. RESOL. ALEGACIÓN' des, cp.fnotalegIntento df from dual
                union select 'INTENTO NOT. RESOL. RECURSO' des, cp.fnotrecIntento df from dual
                union select 'INTENTO NOT. RESOL. REC. EXT.' des, cp.fnotrerIntento df from dual
                union select 'SANCIÓN FIRME' des, cp.ffirmeza df from dual
                union select 'RECURSO CONTRA LA P.A.' des, cp.frpa df from dual
                union select 'NOT. PROVIDENCIA APREMIO' des, cp.fnotpa df from dual
                union select 'FECHA ACTUAL' des, trunc(sysdate) df from dual
                union
                select decode(e.tidomid,1,'GRABACIÓN ALEGACIÓN',2,'GRABACIÓN RECURSO',
                3,'GRABACIÓN RER','GRABACIÓN I.CONDUCTOR') des, trunc(e.esmfgrabacion) df
                from alba.escritosmultas e
                where e.tidomid in (1,2,3,21)
                and e.mulid=vMulid
                union
                -- Notificaciones Denuncia y Requerimiento Versiones Anteriores
                select
                decode(n.ntfresultado,'M3','','M4','','M2','','INTENTO ')||
                decode(a.atocodigo,'MULTDC','NOT.REQ.VERSIÓN ANTERIOR','NOT.DEN.VERSIÓN ANTERIOR') des,
                to_date(n.ntffecnot,'dd/mm/rrrr') df
                from alba.actosnotificacion a, alba.cabremesas c, alba.remesas r,
                    alba.notificaciones n, alba.propuestasnotificacion p
                where p.pntid=n.pntid
                and n.ntfid=r.ntfid
                and r.crmid=c.crmid
                and p.atoid=a.atoid
                and p.pntorigen='I'
                and n.ntffecnot is not null
                and n.ntfcrmid=c.crmid
                and p.pntorigenid in
                    (select mulid
                    from alba.multas m
                    where m.seoidexp in
                    (select m2.seoidexp
                    from alba.multas m2
                    where m2.mulid=vMulid)
                    and m.mulid<>(select max(m3.mulid)
                    from alba.multas m3
                    where m3.mulid=vMulid))
                and a.atocodigo in
                ('MULTDN','MULTDP','MULTDT','MULTDU','MULTDE','MULTDM','MULTDA','MULTDV','MULTDR','MULTDC')
                --and n.ntfresultado in ('M3','M4','M2')
                )
            where df is not null
            order by df asc
        )
        loop
        
        

            vPres:=vPresLocal;

            vPres.tipoFecha:=dt.des;
            vPres.fecha:=dt.df;
            

            -- Ejecutiva
            if desAnt='SANCIÓN FIRME' then
            
                    vPres.dias:=dt.df-dfAnt;
                    vPres.limite:=12*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,6),'dd/mm/rrrr');
                    
            -- NOTIFICACIÓN DENUNCIA
            elsif desAnt in ('DENUNCIA') then

                    vPres.dias:=dt.df-dfAnt;
                    vPres.limite:=12*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,6),'dd/mm/rrrr');                    

            -- Recurso » 7 meses para resolver
            elsif desAnt in ('RECURSO','GRABACIÓN RECURSO') then

                    vPres.dias:=dt.df-dfAnt;
                    vPres.limite:=12*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,6),'dd/mm/rrrr');


            -- Rec.Ext.Rev » 12 meses para resolver
            elsif desAnt in ('RECURSO EXT. REVISIÓN','GRABACIÓN RER') then

                    vPres.dias:=dt.df-dfAnt;
                    vPres.limite:=6*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,6),'dd/mm/rrrr');
                    
            -- Providencia de Apremio » 12 meses para resolver
            elsif desAnt in ('NOT. PROVIDENCIA APREMIO','RECURSO CONTRA LA P.A.') then

                    vPres.dias:=dt.df-dfAnt;
                    vPres.limite:=12*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,6),'dd/mm/rrrr');

            -- Tramitación previa a ejectutiva
            else
            
                if i=1 then
                    vPres.dias:=0;
                    vPres.limite:=0;
                    vPres.fechaLimite:=null;
                    vPres.enPlazo:=1;
                elsif i=2 then
                    vPres.dias:=dt.df-dfAnt;
                    vPres.limite:=cp.limiteDenunciaMes*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,cp.limiteDenunciaMes),'dd/mm/rrrr');
                else
                    vPres.dias:=dt.df-dfAnt;
                    vPres.limite:=cp.limiteRestoMes*30;
                    vPres.fechaLimite:=to_date(add_months(dfAnt,cp.limiteRestoMes),'dd/mm/rrrr');
                end if;

            end if;
                
           
            if vPres.fechaLimite is not null and vPres.fechaLimite<vPres.fecha then
                vPres.enPlazo:=0;
            else
                vPres.enPlazo:=1;
            end if;
            
            
            ---------------------------------------------
            --INICIO » CONTROL DE CADUCIDAD DESHABILITADO
            ---------------------------------------------
            
            if cp.flibinc is not null then
            -- Control de Caducidad 1
            -- Finaliza cálculo si encuentra caducidad
            if dt.des='FECHA ACTUAL' and
            vNotificacionDenuncia is not null and vFechaCaducidad is not null
            then
                if vFechaCaducidad>vNotificacionDenuncia
                and vFechaCaducidad-vNotificacionDenuncia>365 then
                    vPres.enPlazo:=3;
                end if;
            end if;
            
            
            -- Control de Caducidad 2
            if dt.des='FECHA ACTUAL' then
                vNotificacionDenuncia:=to_date(cp.flibinc,'dd/mm/rrrr');
                vFechaCaducidad:=to_date(nvl(nvl(cp.flibaleg,cp.flibsan),sysdate),'dd/mm/rrrr');

                -- Finaliza cálculo si encuentra caducidad
                if vNotificacionDenuncia is not null and vFechaCaducidad is not null
                then
                    if vFechaCaducidad>vNotificacionDenuncia
                    and vFechaCaducidad-vNotificacionDenuncia>365 then
                        vPres.enPlazo:=3;
                    end if;
                end if;

            end if;
            end if;
            
            ------------------------------------------
            --FIN » CONTROL DE CADUCIDAD DESHABILITADO
            ----------------------------------------
            


            pipe row(vPres);
            
            i:=i+1;
            dfAnt:=dt.df;
            desAnt:=dt.des;
            
            
            

        end loop;

    end loop;

    return;

    /*vMul.forigenCalculoPrescripcion:=vMul2.vFechaOrigenPrescripcion;

    vMul2.vFechaPrescripcion:=to_date(add_months(vMul2.vFechaOrigenPrescripcion,vMul2.vGrado),'dd/mm/rrrr');

    vMul.fprescripcion:=vMul2.vFechaPrescripcion;

    if vMul.cestado<30000 and vMul2.vFechaPrescripcion<to_date(sysdate,'dd/mm/rrrr') then
        vMul.cestado:=vMul.cestado+40000;
        vMul.estado:=vMul.estado;
    end if;
    */

end;



--------------------------------------------------------------------------------
-- emultas1: Estado de Tramitación
--------------------------------------------------------------------------------
procedure emultas1(vID number) is

    -- Multas
    cursor cMul(vMulid number) is
    select /*+RULE*/ m.*,decode(i.infgrado,'L',4,'G',7,'M',13,0) grado,
    decode(i.infgrado,'L',3,'G',6,'M',12,0) grado2,
    decode(m.mulxboletin,144,1,303,2,403,1,444,3,423,4,464,6,465,7,543,5,623,7,664,9,723,8,743,10,800,11) origendenuncia,
  nvl(i.infimporte,0)+0 importe,
  m.muldesactivarcontroles incompleto,
    i.infgrupo
    from alba.infracciones i, alba.multas m
    where m.infid=i.infid
    and m.mulfecgrab>=to_date('04/08/2006','dd/mm/rrrr')
    and m.mulid=vMulid
    and m.mulversion=
    (select max(mm.mulversion) from alba.multas mm where mm.seoidexp=m.seoidexp);


vMulLocal mul;
vMul2Local mul2;

vEstadoAnterior number:=0;

begin

    iniciaRegistro();
    vEstadoAnterior:=0;

    for v in cMul(vID) loop


        vMul:=vMulLocal;
        vMul2:=vMul2Local;
        iniciaregistro();
        vmul2.fechadenuncia:=to_date(v.mulfec,'dd/mm/rrrr');
                
        -------------------------------
        -- DENUNCIA
        -------------------------------
        -- Datos Principales de Denuncia
        vMul.mulid:=v.mulid;
        vMul.mulnumbol:=v.mulnumbol;
        
        vmul.total:=v.importe;
        vmul2.total:=v.importe;
        vmul.pendiente:=v.importe;
        vmul.cobrado:=0;
        
        vMul.fnotden:=to_date(v.mulfecnotden,'dd/mm/rrrr');
        vMul.fnotsan:=to_date(v.mulfecsancion,'dd/mm/rrrr');
        vMul.faleg:=to_date(v.mulfecalegden,'dd/mm/rrrr');
        vMul.frec:=to_date(v.mulfecrecsan,'dd/mm/rrrr');
        --vMul.frer:=to_date(v.mulfrer,'dd/mm/rrrr');
        vMul.fnotreq:=to_date(v.mulfecrequer,'dd/mm/rrrr');
        vMul.fcomcon:=to_date(v.mulfecnotifcond,'dd/mm/rrrr');
        vMul.expid:=v.expidexp;
        vMul2.vFechaOrigenPrescripcion:=to_date(v.mulfec,'dd/mm/rrrr');
        vMul2.vGrado:=v.grado;
        vMul2.vGrado2:=v.grado2;
        vMul.origenDenuncia:=v.origenDenuncia;
        
        -- Recalcula origen de denuncia en función del hecho denunciado
        -- en denuncias de convivencia
        /*if v.infid in (6000908,6000907,6000906,6000905,6000904,6000903,6000902,6000901) then
        
          vMul.origenDenuncia:=4;
          
        else
          if v.origenDenuncia=4 then
             vmul.origendenuncia:=1;
          end if;
                
        end if;*/
        
        
        --
        select /*+RULE*/ e5.expcod into vMul2.vExpte
        from alba.expingre e5, alba.multas m5
        where m5.expidexp=e5.expid
        and m5.mulid=v.mulid
        and rownum<2;
        vMul.expediente:=vMul2.vExpte;
        
        leerMatricula(v.mulid);

        leerPersonas(v.mulid);
        
        leerDirecciones(v.mulid);
        
        leerLiquidaciones(v.mulid);

        leerCobros(v.mulid);

        leerEscritos(v.mulid);

        leerLibros(v.mulid);

        leerNotificaciones(v.mulid);
        
        procesaSujetoPasivo();


------------------------------
-- TIPO NOTIFICACIÓN DENUNCIA
-------------------------------


    if v.mulresmedida1 is not null then
        vMul2.vEsdr:=1;
    end if;
    if v.mulresmedida3 is not null then
        vMul2.vEsda:=1;
    end if;
    if v.mulxmedida4 is not null
    and v.infid in (6000882,6000881)
    then
        vMul2.vEsni:=1;
    end if;
    if vMul2.vTipoSp=1 and v.mulnotmano='N' and v.origendenuncia not in (8,9) then
        if vMul2.vEsni=0 then
            vMul.tnotden:='2.1';
        else
            vMul.tnotden:='1.6';
        end if;
    elsif vmul2.vtiposp=20 and v.mulnotmano='N' then
        --vMul.tnotden:='1.3';
      vMul.tnotden:='1.1';
    elsif vMul2.vTipoSp=2 and v.mulnotmano='S' and vMul2.vEsdr=0 and vMul2.vEsda=0 and vMul2.vEsdv=0 then
        vMul.tnotden:='1.0';
        vMul.fnotden:=v.mulfec;
    elsif vMul2.vTipoSp=2 and v.mulnotmano='S' and vMul2.vEsdr=0 and vMul2.vEsda=0 and vMul2.vEsdv=1 then
        vMul.tnotden:='1.4';
    elsif vMul2.vTipoSp=2 and v.mulnotmano='S' and vMul2.vEsdr=0 and vMul2.vEsda=1 and vMul2.vEsdv=0 then
        vMul.tnotden:='1.7';
    elsif vMul2.vTipoSp=2 and v.mulnotmano='N' and vMul2.vEsdr=0 and vMul2.vEsda=1 and vMul2.vEsdv=0 then
        vMul.tnotden:='1.7';
    elsif vMul2.vTipoSp=2 and v.mulnotmano='S' and vMul2.vEsdr=1 and vMul2.vEsda=0 and vMul2.vEsdv=0 then
        vMul.tnotden:='1.8';
    elsif vMul2.vTipoSp=2 and v.mulnotmano='N' and vMul2.vEsdr=1 and vMul2.vEsda=0 and vMul2.vEsdv=0 then
        if vMul2.vEsni=0 then
            if vMul.fcomcon is null then
                    vMul.tnotden:='1.9';
            else
                    vMul.tnotden:='1.4';
            end if;
        else
            vMul.tnotden:='1.6';
        end if;
    elsif vMul2.vTipoSp=2 and v.mulnotmano='N' and vMul2.vEsdr=0 and vMul2.vEsda=0 then
        if vMul2.vEsni=0 then
            if vMul.fcomcon is null then
                    vMul.tnotden:='1.1';
            else
                    vMul.tnotden:='1.4';
            end if;
        else
                       vMul.tnotden:='1.6';
        end if;
    elsif v.origendenuncia in (8,9) then
        vMul.tnotden:='1.1';
    else
        vMul.tnotden:='0.0';
    end if;
    

        ---- INFRACCIONES DEL GRUPO 200 ordenanza de convivencia
    if vmul.tnotden='2.1' and v.infgrupo in (200) then
        vmul.tnotden:='1.1';
    end if;


------------------------------
-- TIPO NOTIFICACIÓN SANCIÓN
------------------------------
                -- TIPO NOTIFICACIÓN SANCIÓN
                if vMul2.vEsdr=0 and vMul2.vEsda=1 then
                    vMul.tnotsan:='3.7';
                elsif vMul2.vEsdr=1 and vMul2.vEsda=0 then
                    vMul.tnotsan:='3.8';
                elsif vMul2.vEsdr=0 and vMul2.vEsda=0 then
                    vMul.tnotsan:='3.1';
                else
                    vMul.tnotsan:='0.0';
                end if;
                --
------------------------------
-- GENERACIÓN DE ESTADO INICIO
------------------------------
                -- sin datos
                if vMul.rec=0 and vMul.aleg=0 and vMul.tnotden='0.0' then
                      vMul.cestado:=1000;
                      vMul.estado:='Datos incompletos en Personas';
                elsif vMul.rdiidt=0 and vMul.rdiidc=0 and vMul.rdiidtt=0 then
                      vMul.cestado:=1000;
                      vMul.estado:='Datos incompletos en Direcciones';

                -- denuncia                                                    
        elsif vMul.rec=0 and vMul.aleg=0 and vMul.fnotden is null and vMul.tnotden<>'2.1'  then
                        if vMul.notden=5 then -- Estado de Publicación
                                vMul2.vFechaOrigenPrescripcion:=to_date(vMul.fnotdenIntento,'dd/mm/rrrr');
                                vMul.cestado:=9100+(10*vMul.libbopden);
                                vMul.estado:='Publicación BOP Notificación Denuncia';
                        else
                              if vMul.tnotden in ('1.1','1.3','1.4','1.6','1.7','1.8','1.9') then
                                  select decode(vMul.tnotden,'1.1',1100,'1.3',1300,'1.4',1400,
                                                  '1.6',1600,'1.7',1700,'1.8',1800,'1.9',1900)
                                                  into vMul.cestado from dual;
                                  vMul.cestado:=vMul.cestado+(10*vMul.libinc)+vMul.notden;
                                  vMul.estado:='Notificación Denuncia';
                              else
                                  vMul.cestado:=210000;
                                  vMul.estado:='Error en Notificación Denuncia';
                              end if;
                                if vMul.tnotden='1.4' and vMul.fcomcon is not null then
                                    vMul2.vFechaOrigenPrescripcion:=to_date(vMul.fcomcon,'dd/mm/rrrr');
                                else
                                    vMul2.vFechaOrigenPrescripcion:=to_date(v.mulfec,'dd/mm/rrrr');
                                    vMul2.vGrado:=vMul2.vGrado2;
                                end if;
                                -- 1.6 por dirección incorrecta
                                if vMul.notden=6 and vMul.tnotden in ('1.4')
                                and vMul.fcomcon is not null then
                                    vMul.cestado:=1600;
                                    vMul.estado:='Notificación Denuncia No Identificación de Conductor';
                                end if;                        
                        end if;

                -- requerimiento
                elsif vMul.rec=0 and vMul.aleg=0 and vMul.fnotden is null and vMul.tnotden='2.1' then
                        if vMul.notreq=5 then -- Estado de Publicación
                                vMul2.vFechaOrigenPrescripcion:=to_date(vMul.fnotreqIntento,'dd/mm/rrrr');
                                vMul.cestado:=9200+(10*vMul.libbopreq);
                                vMul.estado:='Publicación BOP Notificación Requerimiento';
                        else
                                -- Transcurridos 45 días desde la Notificación,
                                -- si no hay comunicación de conductor se genera 1.6 » 1600
                                if vMul.notreq=4
                                and to_date(vMul.fnotreq+45,'dd/mm/rrrr') < to_date(sysdate,'dd/mm/rrrr')
                                and vMul.fcomcon is null then
                                        vMul.cestado:=1600;
                                        vMul.estado:='Notificación Denuncia No Identificación de Conductor';
                                        vMul2.vFechaOrigenPrescripcion:=to_date(vMul.fnotreq,'dd/mm/rrrr');
                                        
                                else
                                          vMul.cestado:=2100+(10*vMul.libreq)+vMul.notreq;
                                          vMul.estado:='Notificacion Requerimiento';
                                        if vMul.fcomcon is null then
                                              if vMul.notreq=4 then
                                                  vMul2.vFechaOrigenPrescripcion:=to_date(v.mulfec,'dd/mm/rrrr');
                                              end if;
                                        else
                                                vMul2.vFechaOrigenPrescripcion:=to_date(vMul.fcomcon,'dd/mm/rrrr');
                                        end if;
                                end if;
                
        
                        end if;

                -- sanción
                elsif vMul.rec=0 and vMul.aleg=0 and vMul.fnotden is not null and vMul.fnotsan is null then
                        if vMul.notsan=5 then -- Estado de Publicación
                                vMul2.vFechaOrigenPrescripcion:=to_date(vMul.fnotsanIntento,'dd/mm/rrrr');
                                vMul.cestado:=9300+(10*vMul.libbopsan);
                                vMul.estado:='Publicación BOP Notificación Sanción';                
                        else
                                vMul.estado:='Notificación Sanción';
                                if vMul.tnotsan='3.7' then
                                        vMul.cestado:=3700+(10*vMul.libsan)+vMul.notsan;
                                elsif vMul.tnotsan='3.8' then
                                        vMul.cestado:=3800+(10*vMul.libsan)+vMul.notsan;
                                elsif vMul.tnotsan='3.1' then
                                        if v.mulxboletin=303 then
                                                vMul.cestado:=3300+(10*vMul.libsan)+vMul.notsan;
                                        else
                                                vMul.cestado:=3100+(10*vMul.libsan)+vMul.notsan;
                                        end if;
                                else
                                        vMul.cestado:=220000;
                                        vMul.estado:='Error en ' || vMul.estado;
                                end if;
                                -- Controla 45 dias desde notificacion denuncia
                                if add_months(to_date(vMul.fnotden,'dd/mm/rrrr'),3) > to_date(sysdate,'dd/mm/rrrr') then
                                        vMul.cestado:=3000;
                                        vMul.estado:='Notificación Sanción';
                                end if;
                                vMul2.vFechaOrigenPrescripcion:=to_date(vMul.fnotden,'dd/mm/rrrr');
                                if v.mulxboletin=303 then
                                    vMul2.vFechaOrigenPrescripcion:=to_date(vMul.fnotden,'dd/mm/rrrr')+30;
                                end if;
                        end if;

                -- alegación
                elsif (vmul.aleg>0 and vmul.fnotsan is null) --no está notificada la sanción 
        or (vmul.aleg>0 and vmul.fnotsan is not null and vmul.notaleg<3 and vmul.libsanfir=0) 
        --or (vmul.aleg>0 and vmul.fnotsan is not null and vmul.notaleg=5 and vmul.libsanfir=0) -- ó alegación no resuelta
        then
                        vMul2.vFechaOrigenPrescripcion:=to_date(vMul.faleg+30,'dd/mm/rrrr');
    
                           if vMul2.vParalizado>0 then -- PARALIZADO
                                            if vMul2.vParalizado=9 then
                                                vMul.cestado:=4100+vMul.aleg+(10*vMul.libaleg)+vMul.notaleg;
                                                  vMul.estado:='Alegación. Pendiente de Resolucion. Paralizado';
                                            else
                                                    if vMul2.vIdConductor=41 then
                                                            vMul.cestado:=8100;
                                                              vMul.estado:='Pendiente de Identificación de Conductor';
                                                    else
                                                            vMul.cestado:=8200;
                                                              vMul.estado:='Conductor Identificado';
                                                    end if;
                                            end if;
                                            vMul.alegesmid:=0;
    
                        elsif vMul2.vParalizado=0 and vMul.infq>0 then -- REQUERIMIENTO
                                            if vMul.infq=2 then
                                                 vMul.cestado:=6700+(10*vMul.libinfq)+vMul.notinfq;
                                                vMul.estado:='Alegación. Requerimiento Preparado';
                                            elsif vMul.infq=1 then
                                                 vMul.cestado:=4700+(10*vMul.libinfq)+vMul.notinfq;
                                                vMul.estado:='Alegación. Requerimiento Pendiente';
                                            end if;
                                            vMul.alegesmid:=vMul.infqesmid;
    
                        elsif vMul2.vParalizado=0 and vMul.infq=0 and vMul.infd>0 then -- INFORME DENUNCIANTE

                                            if vMul.infd=2 and v.mulxboletin<>303 then
                                                 vMul.cestado:=6500+(10*vMul.libinfd);
                                                vMul.estado:='Alegación. Informe a PL Solicitado';
                                            elsif vMul.infd=2 and v.mulxboletin=303 then
                                                 vMul.cestado:=6400+(10*vMul.libinfd);
                                                vMul.estado:='Alegación. Informe a Cont.ORA. Solicitado';                                                    
                                            end if;
                                            vMul.alegesmid:=vMul.infdesmid;
    
                        elsif vMul2.vParalizado=0 and vMul.infq=0 and vMul.infd=0 then -- ALEGACIÓN
                        
                                            if vMul.notaleg=5 then -- Estado de Publicación
                                                    vMul2.vFechaOrigenPrescripcion:=to_date(vMul.fnotalegIntento,'dd/mm/rrrr');
                                                    vMul.cestado:=9400+(10*vMul.libbopaleg);
                                                    vMul.estado:='Publicación BOP R.Alegaciones';                
                                            else                        
                                                    if vMul.aleg=2 then
                                                        vMul.cestado:=6100+(10*vMul.libaleg)+vMul.notaleg;
                                                          vMul.estado:='Alegación. Desestimada';
                                                    elsif vMul.aleg=3 then
                                                        vMul.cestado:=7100+(10*vMul.libaleg)+vMul.notaleg;
                                                          vMul.estado:='Alegación. Estimada';
                                                    elsif vMul.aleg=1 then
                                                        vMul.cestado:=4100+(10*vMul.libaleg)+vMul.notaleg;
                                                          vMul.estado:='Alegación. Pendiente de Resolución';                                    
                                                    end if;
                                            end if;

                        end if;
                -- recurso
                   elsif vMul.rec>0 and vMul.fnotrec is null then -- and vMul.fnotsan is null then

                            vMul2.vFechaOrigenPrescripcion:=to_date(vMul.frec+30,'dd/mm/rrrr');
    
                            if vMul2.vParalizado>0 then -- PARALIZADO
                                            if vMul2.vParalizado=9 then
                                                    vMul.cestado:=4200+vMul.rec+(10*vMul.librec)+vMul.notrec;
                                                  vMul.estado:='Recurso. Pendiente de Resolucion. Paralizado';
                                            else
                                                    if vMul2.vIdConductor=41 then
                                                                vMul.cestado:=8100;
                                                              vMul.estado:='Pendiente de Identificación de Conductor';
                                                    else
                                                                vMul.cestado:=8200;
                                                              vMul.estado:='Conductor Identificado';
                                                    end if;
                                            end if;
                                            vMul.alegesmid:=0;
    
                            elsif vMul2.vParalizado=0 and vMul.infq>0 then -- REQUERIMIENTO
                                            if vMul.infq=2 then
                                                 vMul.cestado:=6700+(10*vMul.libinfq)+vMul.notinfq;
                                                vMul.estado:='Recurso. Requerimiento Preparado';
                                            elsif vMul.infq=1 then
                                                 vMul.cestado:=4700+(10*vMul.libinfq)+vMul.notinfq;
                                                vMul.estado:='Recurso. Requerimiento Pendiente';
                                            end if;
                                            vMul.alegesmid:=vMul.infqesmid;
    
                            elsif vMul2.vParalizado=0 and vMul.infq=0 and vMul.infd>0 then -- INFORME DENUNCIANTE

                                            if vMul.infd=2 and v.mulxboletin<>303 then
                                                 vMul.cestado:=6500+(10*vMul.libinfd);
                                                vMul.estado:='Recurso. Informe a PL Solicitado';
                                            elsif vMul.infd=2 and v.mulxboletin=303 then
                                                 vMul.cestado:=6400+(10*vMul.libinfd);
                                                vMul.estado:='Recurso. Informe a Cont.ORA. Solicitado';                                                    
                                            end if;
                                            vMul.alegesmid:=vMul.infdesmid;
    
                            elsif vMul2.vParalizado=0 and vMul.infq=0 and vMul.infd=0 then -- RECURSO
                                            if vMul.notrec=5 then -- Estado de Publicación
                                                    vMul2.vFechaOrigenPrescripcion:=to_date(vMul.fnotrecIntento,'dd/mm/rrrr');
                                                    vMul.cestado:=9500+(10*vMul.libboprec);
                                                    vMul.estado:='Publicación BOP R.Recursos';                
                                            else                            
                                                    if vMul.rec=2 then
                                                        vMul.cestado:=6200+(10*vMul.librec)+vMul.notrec;
                                                          vMul.estado:='Recurso. Desestimado';
                                                    elsif vMul.rec=3 then
                                                        vMul.cestado:=7200+(10*vMul.librec)+vMul.notrec;
                                                          vMul.estado:='Recurso. Estimado';
                                                    elsif vMul.rec=1 then
                                                        vMul.cestado:=4200+(10*vMul.librec)+vMul.notrec;
                                                          vMul.estado:='Recurso. Pendiente de Resolución';                                    
                                                    end if;
                                            end if;
                                            vMul.alegesmid:=vMul.recesmid;

                            end if;

                -- recurso extraodinario de revisión
                elsif vMul.rer>1 and vMul.fnotrer is null then --and vMul.fnotsan is null

                            vMul2.vFechaOrigenPrescripcion:=to_date(vMul.frer+30,'dd/mm/rrrr');
    
                            if vMul2.vParalizado>0 then -- PARALIZADO
                                            if vMul2.vParalizado=9 then
                                                vMul.cestado:=4300+vMul.rer+(10*vMul.librer)+vMul.notrer;
                                                  vMul.estado:='Rec.Ext.Rev. Pendiente de Resolucion. Paralizado';
                                            else
                                                    if vMul2.vIdConductor=41 then
                                                                vMul.cestado:=8100;
                                                              vMul.estado:='Pendiente de Identificación de Conductor';
                                                    else
                                                                vMul.cestado:=8200;
                                                              vMul.estado:='Conductor Identificado';
                                                    end if;
                                            end if;
                                            vMul.alegesmid:=0;
    
                            elsif vMul2.vParalizado=0 and vMul.infq>0 then -- REQUERIMIENTO
                                            if vMul.infq=2 then
                                                 vMul.cestado:=6700+(10*vMul.libinfq)+vMul.notinfq;
                                                vMul.estado:='Rec.Ext.Rev. Requerimiento Preparado';
                                            elsif vMul.infq=1 then
                                                 vMul.cestado:=4700+(10*vMul.libinfq)+vMul.notinfq;
                                                vMul.estado:='Rec.Ext.Rev. Requerimiento Pendiente';
                                            end if;
                                            vMul.alegesmid:=vMul.infqesmid;
    
                            elsif vMul2.vParalizado=0 and vMul.infq=0 and vMul.infd>0 then -- INFORME DENUNCIANTE

                                            if vMul.infd=2 and v.mulxboletin<>303 then
                                                 vMul.cestado:=6500+(10*vMul.libinfd);
                                                vMul.estado:='Rec.Ext.Rev. Informe a PL Solicitado';
                                            elsif vMul.infd=2 and v.mulxboletin=303 then
                                                 vMul.cestado:=6400+(10*vMul.libinfd);
                                                vMul.estado:='Rec.Ext.Rev. Informe a Cont.ORA. Solicitado';                                                    
                                            end if;
                                            vMul.alegesmid:=vMul.infdesmid;
    
                            elsif vMul2.vParalizado=0 and vMul.infq=0 and vMul.infd=0 then -- REC.EXT.REV
                                            if vMul.notrer=5 then -- Estado de Publicación
                                                    vMul2.vFechaOrigenPrescripcion:=to_date(vMul.fnotrerIntento,'dd/mm/rrrr');
                                                    vMul.cestado:=9600+(10*vMul.libboprer);
                                                    vMul.estado:='Publicación BOP R.Rec.Ext.Revisión';                
                                            else                            
                                                    if vMul.rer=2 then
                                                            vMul.cestado:=6300+(10*vMul.librer)+vMul.notrer;
                                                          vMul.estado:='Rec.Ext.Rev. Desestimado';
                                                    elsif vMul.rer=3 then
                                                            vMul.cestado:=7300+(10*vMul.librer)+vMul.notrer;
                                                          vMul.estado:='Rec.Ext.Rev. Estimado';
                                                    elsif vMul.rer=1 then
                                                            vMul.cestado:=4300+(10*vMul.librer)+vMul.notrer;
                                                          vMul.estado:='Rec.Ext.Rev. Pendiente de Resolución';                                    
                                                    end if;
                                            end if;
                                            vMul.alegesmid:=vMul.reresmid;


                            end if;


                -- Sanción Firme
                elsif --(vMul.aleg=0 or vMul.aleg=4)
                            --and (vMul.rec=0 or vMul.rec=4)
                            --and (vMul.rer=0 or vMul.rer=4)
                            --and
                            
                            
                            vMul.fnotden is not null
                            and vMul.fnotsan is not null then

                            vMul2.vFechaOrigenPrescripcion:=to_date(vMul.fnotsan+365,'dd/mm/rrrr');

                            vMul.cestado:=4900+(10*vMul.libSanFir);
                            vMul.estado:='Ejecutiva';    

                            
                             -- Fechas Sanción Firme » ORIGEN TRAMITACIÓN
                             if vMul.rec=0 and vMul.rer=0 then
                                    vMul.ffirmeza:=to_date(vMul.fnotsan+30,'dd/mm/rrrr');
                            else
                                    vMul.ffirmeza:=nvl(nvl(vMul.fnotrer,vMul.fnotrec),to_date(vMul.fnotsan+30,'dd/mm/rrrr'));
                            end if;
                            vMul.finivol:=vMul.ffirmeza;
                            vMul.ffinvol:=to_date(vMul.ffirmeza+15,'dd/mm/rrrr');
    
    
                            -- Controla 4 mesess desde notificacion sanción
                            if add_months(to_date(vMul.ffirmeza,'dd/mm/rrrr'),4) > to_date(sysdate,'dd/mm/rrrr') then
                                    vMul.cestado:=4000+(10*vMul.libSanFir);
                                    vMul.estado:='Ejecutiva En Espera';
                            end if;


                            if vMul2.vParalizado>0
                            or vMul.infq>0
                            or vMul.infd>0
                            or (vMul.aleg>0 and vMul.notaleg<4)
                            or (vMul.rec>0 and vMul.notrec<4)
                            or (vMul.rer>0 and vMul.notrer<4)
                            or (vMul.faleg>=vMul.flibaleg)
                            or (vMul.frec>=vMul.flibrec)
                            then -- PARALIZADO
                                    vMul.cestado:=4800+(10*vMul.libSanFir);
                                      vMul.estado:='Ejecutiva. Escrito Pendiente de Revisión';
                            end if;
                            
                            -- Controla errores en la notificación
                            -- consecuencia de Correos
                            /*
                            if vMul.libbopden>0
                            and vMul.libbopsan>0
                            and vMul.notdenacto||vMul.notsanacto<>'M5M5'
                            and vMul.notdenacto||vMul.notsanacto<>'M5M3'
                            then
                            
                                    vMul.cestado:=230000+vMul.cestado;
                                    if vMul.notdenrdiid=vMul.notsanrdiid
                                    and vMul.notdenacto<>vMul.notsanacto
                                    then
                                        -- Ante una misma dirección resultados diferentes
                                          vMul.estado:='Ejecutiva. Incoherencias en resultados de notificación';
                                    else
                                       -- No existe ninguna notificación entregada o ausente
                                          vMul.estado:='Ejecutiva. Errores en resultados de notificación';    
                                    end if;

                            end if;
                            */

                            -- CONTROL DE ESCRITOS ESTIMADOS
                            if vMul.aleg=3 or vMul.rec=3 or vMul.rer=3 then
                                
                                if vMul.aleg=3 then
                                    vMul.cestado:=7124;
                                    vMul.estado:='Alegación Estimada';
                                elsif vMul.rec=3 then
                                    vMul.cestado:=7224;
                                    vMul.estado:='Recurso Estimado';
                                else
                                    vMul.cestado:=7324;
                                    vMul.estado:='Rec.Ext.Rev. Estimado';
                                end if;
                                                                
                            end if;
                            
                            
                -- error en estado
                else
                      vMul.cestado:=230000;
                      vMul.estado:='Estado Indeterminado';
                end if;
        

                 -- recurso providencia de apremio
                 if vMul.rpa>0 and vMul.rej=0 and vMul.baja=0
                            --and vMul.fnotrpa is null
                            --and vMul.fSancionFirmeLiq is not null
                            --and vMul.fnotpa is not null
                            --and (vMul.fnotpa-to_date(vMul.fSancionFirmeLiq,'dd/mm/yyyy'))<365
                            --and vMul.estadoLiq in (5) -- Ejecutiva
                            then -- and vMul.fnotsan is null then

                                    --vMul2.vFechaOrigenPrescripcion:=to_date(vMul.frpa+30,'dd/mm/rrrr');

                                    if vMul.rpa=2 then
                                        vMul.cestado:=5600+(10*vMul.librpa)+vMul.notrpa;
                                        vMul.estado:='Recurso PA. Desestimado';
                                    elsif vMul.rpa=3 then
                                        vMul.cestado:=5700+(10*vMul.librpa)+vMul.notrpa;
                                        vMul.estado:='Recurso PA. Estimado';
                                    elsif vMul.rpa=1 then
                                        vMul.cestado:=5500+(10*vMul.librpa)+vMul.notrpa;
                                        vMul.estado:='Recurso PA. Pendiente de Resolución';                                    
                                    end if;
                                    vMul.alegesmid:=vMul.rpaesmid;

                                    -- Fechas Sanción Firme » ORIGEN LIQUIDACIÓN
                                    /*vMul.ffirmeza:=to_date(vMul.fSancionFirmeLiq,'dd/mm/rrrr');
                                    vMul.finivol:=vMul.ffirmeza;
                                    vMul.ffinvol:=to_date(vMul.ffirmeza+15,'dd/mm/rrrr');*/
                                    
                                    
                                    -- Fechas Sanción Firme » ORIGEN TRAMITACIÓN
                                    if vMul.rec=0 and vMul.rer=0 then
                                            vMul.ffirmeza:=to_date(vMul.fnotsan+30,'dd/mm/rrrr');
                                    else
                                            vMul.ffirmeza:=nvl(nvl(vMul.fnotrer,vMul.fnotrec),to_date(vMul.fnotsan+30,'dd/mm/rrrr'));
                                    end if;
                                    vMul.finivol:=vMul.ffirmeza;
                                    vMul.ffinvol:=to_date(vMul.ffirmeza+15,'dd/mm/rrrr');
                                    
                                    
                                    
                                    /*if (vMul.fnotpa-to_date(vMul.fSancionFirmeLiq,'dd/mm/rrrr'))>=365 then
                                        vMul.cestado:=5500+(10*vMul.librpa)+vMul.notrpa;
                                        vMul.estado:='Recurso PA. Más de un año entre Sanción Firme y Not.P.A.';
                                       end if;*/
                                       
                                       /*if vMul.notpa=0 then
                                        vMul.cestado:=5500;
                                        vMul.estado:='Recurso PA. Desestimado. Sin Providencia de Apremio';
                                       end if;*/
                                       
                  else

                        -- cobro sin liquidación
                        if vMul.rej>0 then
                        
                            if vMul.rej=1 then
                                vMul.cestado:=5100;
                                vMul.estado:='Requerimiento Juzgado. Suspendido con Caución';
                            elsif vMul.rej=2 then
                                vMul.cestado:=5100;
                                vMul.estado:='Requerimiento Juzgado. Suspendido sin Caución';                        
                            else
                                vMul.cestado:=5100;
                                vMul.estado:='Requerimiento Juzgado. Levantado';                        
                            end if;
                            
                        elsif vMul.csl>0 then
                        
                            -- CONTROL DE RECURSO DE REPOSICIÓN COBRADO
                            if vMul.cestado not in (4200,4210,4220) 
                       and not(vMul.cestado>=7100 and vMul.cestado<=7199) -- Estimación Alegación
                       and not(vMul.cestado>=7200 and vMul.cestado<=7299) -- Estimación Recurso
                       and not(vMul.cestado>=7300 and vMul.cestado<=7399) -- Estimación RER
                       --BOTEllÓN DESESTIMACIÓN DE ALEGACIONES, RECURSOS, RER
                       and not(vMul.origendenuncia in (4) and vMul.cestado in (6100,6110,6120))
                       and not(vMul.origendenuncia in (4) and vMul.cestado in (6200,6210,6220))
                       and not(vMul.origendenuncia in (4) and vMul.cestado in (6300,6310,6320))
                       then
                            --En el caso de RESOLUCIÓN DE RECURSO DE REPOSICIÓN prevalece
                            --por encima de estar COBRADO. Una vez emitida la notificación
                            --volverá a estado COBRADO
                            
                                    
                                    vEstadoAnterior:=vMul.cestado;
                             
                             if vMul.cestado in (4900,4910,4920) then
                                        -- CONTROL DE EXPEDIENTES PAGADOS CON SANCIÓN FIRME PARA SU COMUNICACIÓN A DGT
                                        vMul.cestado:=30001;
                                    else
                                        --vMul.cestado:=vMul.cestado+30000;
                                        vMul.cestado:=30000;
                                    end if;
                                    
                                    --
                                    -- Actualiza Descripción del Estado
                                if vMul.csl=100 then
                                    vMul.estado:='Pendiente Aplicación Incidencia Cobro';
                                elsif vMul.csl=10 then
                                        --vMul.estado:='Cobro C60 en ' || vMul.estado;
                                        vMul.estado:='Cobro C60';
                                    else
                                        --vMul.estado:='Cobro CSL en ' || vMul.estado;
                                        vMul.estado:='Cobro CSL';
                                    end if;
                                    
                        end if;

                        -- escrito duplicado
                        elsif vMul2.vEscritoDuplicado then
                                vMul.cestado:=260000;
                                vMul.estado:='Resolución de Escritos Duplicada.';    
                        end if;


                end if;

                -- CÁLCULO PRESCRIPCIÓN. EXCLUYE EXPEDIENTES CON ESCRITOS ESTIMADOS
             if not(vMul.cestado>=7100 and vMul.cestado<=7399) then
                     
                     if vMul.origendenuncia in (4,8,9) then
                        select decode(vMul2.vGrado,4,7,7,25,13,37) into vMul2.vGrado from dual;
                        select decode(vMul2.vGrado2,3,6,6,24,12,36) into vMul2.vGrado2 from dual;                        
                        calculoPrescripcion4(to_date(v.mulfec,'dd/mm/rrrr'));
                     elsif vMul.origendenuncia in (6,7) then
                        select decode(vMul2.vGrado,4,7,7,25,13,37) into vMul2.vGrado from dual;
                        select decode(vMul2.vGrado2,3,6,6,24,12,36) into vMul2.vGrado2 from dual;                    
                        calculoPrescripcion6(to_date(v.mulfec,'dd/mm/rrrr'));
                     elsif vMul.origendenuncia in (10) then
                        select decode(vMul2.vGrado,4,7,7,25,13,36) into vMul2.vGrado from dual;
                        select decode(vMul2.vGrado2,3,6,6,24,12,36) into vMul2.vGrado2 from dual;                    
                        calculoPrescripcion4(to_date(v.mulfec,'dd/mm/rrrr'));                        
                     else
                        calculoPrescripcion(to_date(v.mulfec,'dd/mm/rrrr'));
                     end if;
                     
            end if;                     
                
                -- CÁLCULO DE CADUCIDAD APLICADA SOLO A EJECUTIVA
                if (vMul.cestado=4920 or vMul.origendenuncia in (4,6))
                and vmul.fnotsan is not null
                and vmul.fnotden is not null
                and to_date(vmul.fnotsan,'dd/mm/rrrr')-to_date(vmul.fnotden,'dd/mm/rrrr')>365
                then
                  vmul.cestado:=44920;
                        vMul.estado:='Posiblemente Caducado';
                end if;

                /*
                -- Amplia Información de Estado
                select decode(substr(to_char(vMul.cestado),length(to_char(vMul.cestado)),1),
                              1,'Propuesta Notif. Generada Pendiente de Remesa',
                      2,'Incluido en Remesa de Notif.',
                      3,'Notif. Emitida Sin Resultado',
                      4,'Notif. Notificada')
                              into vMul2.vEstado2 from dual;
                select decode(substr(to_char(vMul.cestado),length(to_char(vMul.cestado))-1,1),
                                            0,'Pendiente de Generar Libro',
                              1,'Libro Emitido Pendiente de Firma',
                      2,'Libro Firmado')
                              into vMul2.vEstado3 from dual;
                --
                if length(vMul2.vEstado2)>2 and vMul.cestado<=9900 then
                    vMul.estado:=vMul.estado || '. ' || vMul2.vEstado2;
                end if;
                if vMul.cestado<=9900 then
                        if length(vMul2.vEstado3)>2 then
                                if vMul.notden=0 then
                                        vMul.estado:=vMul.estado || '. ' || vMul2.vEstado3;
                                else
                                        vMul.estado:=vMul.estado;
                                end if;
                        end if;
                end if;
                */ 

                -- Comprueba Error en Matrícula
                /*if fMasMat(vMul.matricula) and vMul.origendenuncia in (0,1,2) then
                        vMul.cestado:=240000;
                        vMul.estado:='Error en Matrícula';
                end if;
                
                -- Error en matrícula, carga multacar
                if vMul.matricula<>v.mulmatri then
                        vMul.cestado:=250000;
                        vMul.estado:='Error en Carga Multacar. Matrícula Incoherente.';                
                end if;
                
                
                -- Error en lugar, carga multacar
                if vMul.origendenuncia in (0)
                and v.viaid in
                (
                --select v.viaid from alba.vias v where v.viadesc like '%ARGUIJ%'
                --union
                --select v.viaid from alba.vias v where v.viadesc like '%OSBORN%' and v.viadesc like 'J%'
                161450,161451,161452,161453,161454,162935,199335,200329,263815,263816,263830,263831,272172,
                276668,320591,338693,364547,378184,383679,391775,579981,649636,715952,726038,728940,791824,
                793023,818803,968092,975639,983046,985805,992416,1032304,1060138,1064703,1072973,1081421,
                1090858,1094868,1097706,1118715,1120084,1137159,1206471,1434574,1440683,1471950,1472845
                )
                then
                        vMul.cestado:=290000;
                        vMul.estado:='Error en Carga Multacar. Lugar Incorrecto.';                
                end if;            */    
        
        -- ESTIMACIÓN RER NO INCLUIDOS EN LIBROS DE BAJA     
        if vMul.rer=3 and vMul.baja=0 then

            vMul.cestado:=7300+(10*vMul.librer)+vMul.notrer;
            vMul.estado:='Rec.Ext.Rev. Estimado';
            vMul.alegesmid:=vMul.reresmid;
            
        end if;


   
        
        if trunc(v.mulfecrequer)=trunc(v.mulfecejec) then
        
                      vMul.cestado:=280000;
                      vMul.estado:='Error Versión';            
            
        end if;

        -- BAJA       
        if vMul.baja>0 then
            --No existe notificación de baja
            --vMul.cestado:=50000+(10*vMul.libbaja)+vMul.notbaja;
            vMul.cestado:=50000+(10*vMul.libbaja);
            vMul.estado:='Finalizado';
            if vMul.baja=2 then
                vMul.cestado:=5000+(10*vMul.libbaja);
                vMul.estado:='Finalizado';            
            end if;
            vMul.alegesmid:=vMul.bajaesmid;
        end if;
        
        
        -- REA        
        /*if vMul.rea>0 then --and vMul.rpa not in (2,3) then

                if vMul.rea=2 then
                  vMul.cestado:=7000+(10*vMul.librea)+vMul.notrea;
                  vMul.estado:='REA Desestimado';
                elsif vMul.rea=3 then
                  vMul.cestado:=7000+(10*vMul.librea)+vMul.notrea;
                  vMul.estado:='REA Estimado';
                elsif vMul.rea=1 then
                  vMul.cestado:=1300+(10*vMul.librea)+vMul.notrea;
                  vMul.estado:='REA Pendiente de Resolución';                                    
                end if;
                vMul.alegesmid:=vMul.reaesmid;        
        
        end if;*/
       if vEstadoAnterior=1000 and vMul.cestado in (30000,30001) then
            vMul.cestado:=1000;
       end if;
       
       
        -- Análisis de Estados Finalizados
        if vMul.cestado in
        (
        5020,--Finalizado
        5723,--Recurso PA. Estimado
        5724,--Recurso PA. Estimado
        5725,--Recurso PA. Estimado
        5726,--Recurso PA. Estimado
        7123,--Alegación. Estimada
        7124,--Alegación Estimada
        7125,--Alegación Estimada
        7126,--Alegación Estimada
        7323,--Rec.Ext.Rev. Estimado
        7324,--Rec.Ext.Rev. Estimado
        7325,--Rec.Ext.Rev. Estimado
        7326    --Rec.Ext.Rev. Estimado
        ) then
        
            vMul.cestado:=vMul.cestado+60000;
        
        end if;       
    
    setEstadoCobro();
    
    if v.incompleto='S' then
    
        vmul.cestado:=100;
        vMul.estado:='Pendiente de Requerimiento y Paralizado';    
    
    end if;
    
    if vmul.fnotsan is null then
        if vmul.infq=2 and vMul.tipoinfq=20 then
            vmul.cestado:=1200+(10*vmul.libinfq)+vmul.notinfq;
            vmul.estado:='Req. Subsanación Denuncia. Solicitado';
            vmul.alegesmid:=vMul.infqesmid;
        elsif vmul.infq=2 and vMul.tipoinfq=22 then
            vmul.cestado:=1500+(10*vmul.libinfq)+vmul.notinfq;
            vmul.estado:='Req. Denunciante Voluntario. Solicitado';
            vmul.alegesmid:=vMul.infqesmid;
        elsif vmul.infq=1 then
            vmul.cestado:=130;
            vmul.estado:='Req. Solicitado Sin Contenido';
            vmul.alegesmid:=vMul.infqesmid;
        end if;
    end if;
    
    -- Estimaciones que no entran en libro según estado de liquidación
    if vmul.cestado=5700 --Recurso PA. Estimado
    and vmul.estadoliq not in (481,480,13,35,5,170,171,172,200) then
        vmul.cestado:=5500;
        vmul.estado:='Recurso PA Estimado 
        ';    
    end if;

    -- Necesario datos del denunciante
    if vMul.cestado<9999 
    and vMul.origendenuncia in (4,6) 
    and hayDenunciante(vMul.mulid)<1
    then
      vmul.cestado:=1000;
      vmul.estado:='Datos incompletos en Denunciantes';
    end if;
    
    setEstadoCobro();
    setInfo(v.mulid);
                
    end loop; --------------------------------------(1)


exception
    when others then
       dbms_output.put_line('KO. ' || SQLERRM);
end;



--------------------------------------------------------------------------------
-- emultas2: Estado de Tramitación
--------------------------------------------------------------------------------
procedure emultas2(vID number) is

    -- Multas
    cursor cMul(vMulid number) is
    select /*+RULE*/ m.*, decode(i.infgrado,'L',4,'G',7,'M',7,0) grado,
    decode(i.infgrado,'L',3,'G',6,'M',6,0) grado2,
    decode(m.mulxboletin,144,5,303,5,403,5,444,5,423,4,464,6,465,5,523,5,543,5,623,7,664,9,723,8,743,10,800,11) origendenuncia,
    decode(i.puntos,null,0,1) conpuntos,
    i.infgrupo,
    decode(i.infestacionamiento,null,0,'1',1) esestacionamiento, nvl(i.infimporte,0)+0 importe,
    m.muldesactivarcontroles incompleto
    from alba.infracciones i, alba.multas m
    where m.infid=i.infid
    and m.mulfecgrab>to_date('24/05/2010','dd/mm/rrrr')
    and m.mulid=vMulid
    and m.mulversion=
    (select max(mm.mulversion) from alba.multas mm where mm.seoidexp=m.seoidexp);


vMulLocal mul;
vMul2Local mul2;

vBajaA number:=0;
vEstadoAnterior number:=0;

vDias number:=0;

begin

    iniciaRegistro();
    vEstadoAnterior:=0;    

    for v in cMul(vID) loop


        vMul:=vMulLocal;
        vMul2:=vMul2Local;
        iniciaregistro();
        vmul2.fechadenuncia:=to_date(v.mulfec,'dd/mm/rrrr');
                
        -------------------------------
        -- DENUNCIA
        -------------------------------
        -- Datos Principales de Denuncia
        vMul.mulid:=v.mulid;
        vmul.mulnumbol:=v.mulnumbol;
        vmul.total:=v.importe;
        vmul2.total:=v.importe;
        vmul.pendiente:=v.importe;
        vmul.cobrado:=0;
    
        vMul.fnotden:=to_date(v.mulfecnotden,'dd/mm/rrrr');
        vMul.fnotsan:=to_date(v.mulfecsancion,'dd/mm/rrrr');
        vMul.faleg:=to_date(v.mulfecalegden,'dd/mm/rrrr');
        vMul.frec:=to_date(v.mulfecrecsan,'dd/mm/rrrr');
        --vMul.frer:=to_date(v.mulfrer,'dd/mm/rrrr');
        vMul.fnotreq:=to_date(v.mulfecrequer,'dd/mm/rrrr');
        vMul.fcomcon:=to_date(v.mulfecnotifcond,'dd/mm/rrrr');
        vMul.expid:=v.expidexp;
        vMul2.vFechaOrigenPrescripcion:=to_date(v.mulfec,'dd/mm/rrrr');
        vMul2.vGrado:=v.grado;
        vMul2.vGrado2:=v.grado2;
        vMul.origenDenuncia:=v.origenDenuncia;
        
        -- Recalcula origen de denuncia en función del hecho denunciado
        -- en denuncias de convivencia
        /*if v.infid in (6000908,6000907,6000906,6000905,6000904,6000903,6000902,6000901) then
        
        vMul.origenDenuncia:=4;
        
        else
        if v.origenDenuncia=4 then
           vmul.origendenuncia:=1;
        end if;
             
        end if;*/
        
        
        --
        select  /*+RULE*/ e5.expcod into vMul2.vExpte
        from alba.expingre e5, alba.multas m5
        where m5.expidexp=e5.expid
        and m5.mulid=v.mulid
        and rownum<2;
        vMul.expediente:=vMul2.vExpte;
        
        leerMatricula(v.mulid);
        
        leerPersonas(v.mulid);
        
        leerDirecciones(v.mulid);
        
        leerLiquidaciones(v.mulid);
        
        leerCobros(v.mulid);
        
        leerEscritos(v.mulid);
        
        leerLibros(v.mulid);
        
        leerNotificaciones(v.mulid);
        
        procesaSujetoPasivo();


        ------------------------------
        -- TIPO NOTIFICACIÓN DENUNCIA
        -------------------------------
        if v.mulresmedida1 is not null then
            vmul2.vesdr:=1;
        end if;
        if v.mulresmedida3 is not null then
            vmul2.vesda:=1;
        end if;
        if v.mulresmedida1 is not null and v.mulresmedida3 is not null then
            vmul2.vesdr:=1;
            vmul2.vesda:=0;
        end if;
        if v.mulxmedida4 is not null and v.infid in(6000882 ,6000881 ,9002328 ,9002344) then
            vmul2.vesni:=1;
        end if;
    
        ----------------------------------------------------------------------------
        -- TIPO NOTIFICACIÓN
        ----------------------------------------------------------------------------
        --if vMul2.vTipoSp=1 and v.mulnotmano='N' then
        if vmul2.vtiposp=1 or (v.conpuntos=1 and vmul.tpc is not null and vmul.tpc=0) then
            if vmul2.vesni=0 then
                vmul.tnotden:='2.1';
            else
                vmul.tnotden:='1.6';
            end if;
        elsif vmul2.vtiposp=20 and v.mulnotmano='N' then
            --vMul.tnotden:='1.3';
            vmul.tnotden:='1.1';
        elsif vmul2.vtiposp=2 and v.mulnotmano='S' and vmul2.vesdr=0 and vmul2.vesda=0 and vmul2.vesdv=0 then
            vmul.tnotden:='1.0';
            vmul.fnotden:=v.mulfec;
        elsif vmul2.vtiposp=2 and v.mulnotmano='S' and vmul2.vesdr=0 and vmul2.vesda=0 and vmul2.vesdv=1 then
            vmul.tnotden:='1.4';
        elsif vmul2.vtiposp=2 and v.mulnotmano='S' and vmul2.vesdr=0 and vmul2.vesda=1 and vmul2.vesdv=0 then
            vmul.tnotden:='1.1';
        elsif vmul2.vtiposp=2 and v.mulnotmano='N' and vmul2.vesdr=0 and vmul2.vesda=1 and vmul2.vesdv=0 then
            vmul.tnotden:='1.1';
        elsif vmul2.vtiposp=2 and v.mulnotmano='S' and vmul2.vesdr=1 and vmul2.vesda=0 and vmul2.vesdv=0 then
            vmul.tnotden:='1.1';
        elsif vmul2.vtiposp=2 and v.mulnotmano='N' and vmul2.vesdr=1 and vmul2.vesda=0 and vmul2.vesdv=0 then
            if vmul2.vesni=0 then
                if vmul.fcomcon is null then
                    vmul.tnotden:='1.1';
                else
                    vmul.tnotden:='1.4';
                end if;
            else
                vmul.tnotden:='1.6';
            end if;
        elsif vmul2.vtiposp=2 and v.mulnotmano='N' and vmul2.vesdr=0 and vmul2.vesda=0 then
            if vmul2.vesni=0 then
                if vmul.fcomcon is null then
                    vmul.tnotden:='1.1';
                else
                    vmul.tnotden:='1.4';
                end if;
            else
                vmul.tnotden:='1.6';
            end if;
        else
            vmul.tnotden:='0.0';
            --vMul.estado:='Datos incoherentes';
        end if;
        if vmul.fnotsan is not null and vmul.fnotden is null then
            vmul.fnotden:=to_date(v.mulfec ,'dd/mm/rrrr') ;
        end if;
        if v.mulfec is not null and v.mulnotmano='S' then
            vmul.fnotden:=v.mulfec;
        end if;
        ---- NUEVA LEY DE TRÁFICO
        if vmul.tnotden='2.1' and v.esestacionamiento=1 then
            vmul.tnotden:='1.1';
        end if;
        
        ---- INFRACCIONES DEL GRUPO 101
        if vmul.tnotden='2.1' and v.infgrupo in (101,102) then
            vmul.tnotden:='1.1';
        end if;
        
        vmul.tpc:=vmul2.vtiposp;

        ------------------------------
        -- TIPO NOTIFICACIÓN SANCIÓN
        ------------------------------
        if vmul2.vesdr=0 and vmul2.vesda=1 then
            vmul.tnotsan:='3.1';
        elsif vmul2.vesdr=1 and vmul2.vesda=0 then
            vmul.tnotsan:='3.1';
        elsif vmul2.vesdr=0 and vmul2.vesda=0 then
            vmul.tnotsan:='3.1';
        else
            vmul.tnotsan:='0.0';
        end if;

        ------------------------------
        -- GENERACIÓN DE ESTADO INICIO
        ------------------------------
        -- sin datos
        if vmul.rec=0 and vmul.aleg=0 and vmul.tnotden='0.0'
        then
            vmul.cestado:=1000;
            vmul.estado:='Datos incompletos en Personas';
            select decode(count(d.dlbid) ,0 ,0 ,1)
            into vbajaa
            from alba.det_libro_resol d
            where d.mulid=vmul.mulid and(d.dlbmotivo='Operación incompleta (ATEX00500 - Matrícula incorrecta)' or d.dlbmotivo='Operación incompleta (ATEX00700 - Vehículo sin antecedentes)') ;
            -- A los tres meses de grabación baja automática
            if to_date(v.mulfecgrab ,'dd/mm/rrrr')+90<sysdate and vbajaa=1 then
                vmul.cestado:=2300+(10*vmul.libbaja) ;
                vmul.estado:='Finalizado Automáticamente · 3 meses desde grabación sin datos';
            end if;
        elsif vmul.rdiidt=0 and vmul.rdiidc=0 and vmul.rdiidtt=0
        then
            vmul.cestado:=1000;
            vmul.estado:='Datos incompletos en Direcciones';
            select decode(count(d.dlbid) ,0 ,0 ,1)
            into vbajaa
            from alba.det_libro_resol d
            where d.mulid=vmul.mulid and(d.dlbmotivo='Operación incompleta (ATEX00500 - Matrícula incorrecta)' or d.dlbmotivo='Operación incompleta (ATEX00700 - Vehículo sin antecedentes)') ;
            -- A los tres meses de grabación baja automática
            if to_date(v.mulfecgrab ,'dd/mm/rrrr')+90<sysdate and vbajaa=1 then
                vmul.cestado:=2300+(10*vmul.libbaja) ;
                vmul.estado:='Finalizado Automáticamente · 3 meses desde grabación sin datos';
            end if;
        
        ----------------------------------
        -- denuncia
        ----------------------------------
        elsif(vmul.rec=0 and vmul.aleg=0 and vmul.fnotden is null and vmul.tnotden<>'2.1') --or
            -- Existe un libro de incoación previo al de requerimiento
            --(vmul.rec=0 and vmul.aleg=0 and vmul.fnotden is null and vmul.tnotden='2.1' and vmul.libinc>0) or
            -- Existe una identificación de conductor
            --(vmul.rec=0 and vmul.aleg=0 and vmul.fnotden is null and vmul.tnotden='2.1' and vmul.fcomcon is not null)
        then
            
            if vmul.notden=5 then -- Estado de Publicación
                vmul2.vfechaorigenprescripcion:=to_date(vmul.fnotdenintento ,'dd/mm/rrrr') ;
                vmul.cestado:=9100+(10*vmul.libbopden) ;
                vmul.estado:='Publicación BOP Notificación Denuncia';
            else
                if vmul.tnotden in('1.1' ,'1.3' ,'1.4' ,'1.6' ,'1.7' ,'1.8' ,'1.9' ,'2.1') then
                    select decode(vmul.tnotden ,'1.1' ,1100 ,'1.3' ,1300 ,'1.4' ,1400 , '1.6' ,1600 ,'1.7' ,1700 ,'1.8' ,1800 ,'1.9' ,1900)
                    into vmul.cestado
                    from dual;
                    vmul.cestado:=vmul.cestado+(10*vmul.libinc)+vmul.notden;
                    vmul.estado:='Notificación Denuncia';
                else
                    vmul.cestado:=210000;
                    vmul.estado:='Error en Notificación Denuncia';
                end if;
                if vmul.tnotden in('1.4') and vmul.fcomcon is not null then
                    vmul2.vfechaorigenprescripcion:=to_date(vmul.fcomcon ,'dd/mm/rrrr') ;
                else
                    vmul2.vfechaorigenprescripcion:=to_date(v.mulfec ,'dd/mm/rrrr') ;
                    --vMul2.vGrado:=vMul2.vGrado2;
                end if;
                -- 1.6 por dirección incorrecta
                if vmul.notden=6 and vmul.tnotden in('1.4') and vmul.fcomcon is not null then
                    vmul.cestado:=1600;
                    vmul.estado:='Notificación Denuncia No Identificación de Conductor';
                end if;
            end if;
            
        ----------------------------------
        -- requerimiento
        ----------------------------------
        elsif vmul.rec=0 and vmul.aleg=0 and vmul.fnotden is null and vmul.tnotden='2.1'
        then
        
            if vmul.notreq=5 and vmul.libbopreq<1 then -- Estado de Publicación
                vmul2.vfechaorigenprescripcion:=to_date(vmul.fnotreqintento ,'dd/mm/rrrr') ;
                vmul.cestado:=9200+(10*vmul.libbopreq) ;
                vmul.estado:='Publicación BOP Notificación Requerimiento';
            else
                -- Transcurridos 45 días desde la Notificación,
                -- si no hay comunicación de conductor se genera 1.6 » 1600
                if (vmul.notreq=4 or vmul.libbopreq=2) and to_date(vmul.fnotreq+45 ,'dd/mm/rrrr')<to_date(sysdate ,'dd/mm/rrrr') and vmul.fcomcon is null then
                    vmul.cestado:=1600;
                    vmul.estado:='Notificación Denuncia No Identificación de Conductor';
                    vmul2.vfechaorigenprescripcion:=to_date(vmul.fnotreq ,'dd/mm/rrrr') ;
                else
                    vmul.cestado:=2100+(10*vmul.libreq)+vmul.notreq;
                    vmul.estado:='Notificacion Requerimiento';
                    if vmul.fcomcon is null then
                        if(vmul.notreq=4 or vmul.libbopreq=2) then
                            vmul2.vfechaorigenprescripcion:=to_date(v.mulfec ,'dd/mm/rrrr') ;
                        end if;
                    else
                        vmul2.vfechaorigenprescripcion:=to_date(vmul.fcomcon ,'dd/mm/rrrr') ;
                    end if;
                    
                    if vmul.cestado=2124 and to_date(vmul.fnotreq+45 ,'dd/mm/rrrr')<to_date(sysdate ,'dd/mm/rrrr') 
                    and vmul.fcomcon is not null and vmul.fcomcon<vmul.fnotreq then
                        vmul.cestado:=1600;
                        vmul.estado:='Notificación Denuncia No Identificación de Conductor';
                        vmul2.vfechaorigenprescripcion:=to_date(vmul.fnotreq ,'dd/mm/rrrr');                     
                    end if;
                end if;
            end if;
        
        ----------------------------------    
        -- sanción
        ----------------------------------
        elsif (
                 (vmul.rec=0 and vmul.aleg=0 and vmul.fnotden is not null
                 and vmul.fnotsan is null and v.grado=7 and v.conpuntos=1)
                 or
                 (vmul.rec=0 and vmul.aleg=0 and vmul.fnotden is not null 
                 and vmul.fnotsan is null and vmul2.vesni=1)
                 or
                 (vmul.rec=0 and vmul.aleg=0 and vmul.fnotden is not null 
                 and vmul.fnotsan is null and v.infgrupo in (102) and vmul.flibSanFir is null)                 
               ) 
               and
               (v.infgrupo is null or v.infgrupo not in (101))
        then
            if vmul.notsan=5 then -- Estado de Publicación
                vmul2.vfechaorigenprescripcion:=to_date(vmul.fnotsanintento ,'dd/mm/rrrr') ;
                vmul.cestado:=9300+(10*vmul.libbopsan) ;
                vmul.estado:='Publicación BOP Notificación Sanción';
            else
                vmul.estado:='Notificación Sanción Firme';
                if vmul.tnotsan='3.7' then
                    vmul.cestado:=3100+(10*vmul.libsan)+vmul.notsan;
                elsif vmul.tnotsan='3.8' then
                    vmul.cestado:=3100+(10*vmul.libsan)+vmul.notsan;
                elsif vmul.tnotsan='3.1' then
                    if v.mulxboletin=303 then
                        vmul.cestado:=3100+(10*vmul.libsan)+vmul.notsan;
                    else
                        vmul.cestado:=3100+(10*vmul.libsan)+vmul.notsan;
                    end if;
                else
                    vmul.cestado:=220000;
                    vmul.estado:='Error en ' || vmul.estado;
                end if;
                -- Controla 60 dias desde notificacion denuncia
                if to_date(vmul.fnotden+60 ,'dd/mm/rrrr')>to_date(sysdate ,'dd/mm/rrrr') then
                    vmul.cestado:=3000;
                    vmul.estado:='Notificación Sanción Firme';
                end if;
                vmul2.vfechaorigenprescripcion:=to_date(vmul.fnotden ,'dd/mm/rrrr') ;
                if v.mulxboletin=303 then
                    vmul2.vfechaorigenprescripcion:=to_date(vmul.fnotden ,'dd/mm/rrrr')+30;
                end if;
            end if;
            
        ----------------------------------
        -- alegación
        ----------------------------------
        elsif(vmul.aleg>0 and vmul.fnotsan is null) --no está notificada la sanción
        or(vmul.aleg>0 and vmul.fnotsan is not null and vmul.notaleg<3 and vmul.libsanfir=0)
        or(vmul.aleg>0 and vmul.fnotsan is not null and vmul.notaleg=5 and vmul.libsanfir=0
        and vmul.libbopaleg<2) then -- ó alegación no resuelta
        
            vmul2.vfechaorigenprescripcion:=to_date(vmul.faleg+30 ,'dd/mm/rrrr') ;
            if vmul2.vparalizado>0 then -- PARALIZADO
                if vmul2.vparalizado=9 then
                    vmul.cestado:=4100+vmul.aleg+(10*vmul.libaleg)+vmul.notaleg;
                    vmul.estado:='Alegación. Pendiente de Resolucion. Paralizado';
                else
                    if vmul2.vidconductor=41 then
                        vmul.cestado:=8100;
                        vmul.estado:='Pendiente de Identificación de Conductor';
                    else
                        vmul.cestado:=8200;
                        vmul.estado:='Conductor Identificado';
                    end if;
                end if;
                vmul.alegesmid:=0;
            
            elsif vmul2.vparalizado=0 and vmul.infq=0 and vmul.infd>0 then -- INFORME DENUNCIANTE
                if vmul.infd=2 and v.mulxboletin<>303 then
                    vmul.cestado:=6500+(10*vmul.libinfd) ;
                    vmul.estado:='Alegación. Informe a PL Solicitado';
                elsif vmul.infd=2 and v.mulxboletin=303 then
                    vmul.cestado:=6400+(10*vmul.libinfd) ;
                    vmul.estado:='Alegación. Informe a Cont.ORA. Solicitado';
                end if;
                vmul.alegesmid:=vmul.infdesmid;
            elsif vmul2.vparalizado=0 and vmul.infq=0 and vmul.infd=0 then -- ALEGACIÓN
                if vmul.notaleg=5 then -- Estado de Publicación
                    vmul2.vfechaorigenprescripcion:=to_date(vmul.fnotalegintento ,'dd/mm/rrrr') ;
                    vmul.cestado:=9400+(10*vmul.libbopaleg) ;
                    vmul.estado:='Publicación BOP R.Alegaciones';
                else
                    if vmul.aleg=2 then
                        vmul.cestado:=6100+(10*vmul.libaleg)+vmul.notaleg;
                        vmul.estado:='Alegación. Desestimada';
                    elsif vmul.aleg=3 then
                        vmul.cestado:=7100+(10*vmul.libaleg)+vmul.notaleg;
                        vmul.estado:='Alegación. Estimada';
                        /*elsif vMul.aleg=4 then
                        vMul.cestado:=1200+(10*vMul.libaleg)+vMul.notaleg;
                        vMul.estado:='Alegación. Extemporanea';*/
                    elsif vmul.aleg=1 then
                        vmul.cestado:=4100+(10*vmul.libaleg)+vmul.notaleg;
                        vmul.estado:='Alegación. Pendiente de Resolución';
                    end if;
                end if;
            end if;
            
        ----------------------------------            
        -- recurso
        ----------------------------------
        elsif(vmul.rec>1 and vmul.fnotrec is null)
        or(vmul.rec>1
        and vmul.fnotrec is null
        and to_date(vmul.frec+30 ,'dd/mm/rrrr')<to_date(sysdate ,'dd/mm/rrrr'))
        then -- and vMul.fnotsan is null then
        
            vmul2.vfechaorigenprescripcion:=to_date(vmul.frec+30 ,'dd/mm/rrrr') ;
            if vmul2.vparalizado>0 then -- PARALIZADO
                if vmul2.vparalizado=9 then
                    vmul.cestado:=4200+vmul.rec+(10*vmul.librec)+vmul.notrec;
                    vmul.estado:='Recurso. Pendiente de Resolucion. Paralizado';
                else
                    if vmul2.vidconductor=41 then
                        vmul.cestado:=8100;
                        vmul.estado:='Pendiente de Identificación de Conductor';
                    else
                        vmul.cestado:=8200;
                        vmul.estado:='Conductor Identificado';
                    end if;
                end if;
                vmul.alegesmid:=0;
            elsif vmul2.vparalizado=0 and vmul.infq>0 then -- REQUERIMIENTO
                if vmul.infq=2 then
                    vmul.cestado:=6700+(10*vmul.libinfq)+vmul.notinfq;
                    vmul.estado:='Recurso. Requerimiento Preparado';
                elsif vmul.infq=1 then
                    vmul.cestado:=4700+(10*vmul.libinfq)+vmul.notinfq;
                    vmul.estado:='Recurso. Requerimiento Pendiente';
                end if;
                vmul.alegesmid:=vmul.infqesmid;
            elsif vmul2.vparalizado=0 and vmul.infq=0 and vmul.infd>0 then -- INFORME DENUNCIANTE
                if vmul.infd=2 and v.mulxboletin<>303 then
                    vmul.cestado:=6500+(10*vmul.libinfd) ;
                    vmul.estado:='Recurso. Informe a PL Solicitado';
                elsif vmul.infd=2 and v.mulxboletin=303 then
                    vmul.cestado:=6400+(10*vmul.libinfd) ;
                    vmul.estado:='Recurso. Informe a Cont.ORA. Solicitado';
                end if;
                vmul.alegesmid:=vmul.infdesmid;
            elsif vmul2.vparalizado=0 and vmul.infq=0 and vmul.infd=0 then -- RECURSO
                if vmul.notrec=5 then -- Estado de Publicación
                    vmul2.vfechaorigenprescripcion:=to_date(vmul.fnotrecintento ,'dd/mm/rrrr') ;
                    vmul.cestado:=9500+(10*vmul.libboprec) ;
                    vmul.estado:='Publicación BOP R.Recursos';
                else
                    if vmul.rec=2 then
                        vmul.cestado:=6200+(10*vmul.librec)+vmul.notrec;
                        vmul.estado:='Recurso. Desestimado';
                    elsif vmul.rec=3 then
                        vmul.cestado:=7200+(10*vmul.librec)+vmul.notrec;
                        vmul.estado:='Recurso. Estimado';
                    elsif vmul.rec=1 then
                        vmul.cestado:=4200+(10*vmul.librec)+vmul.notrec;
                        vmul.estado:='Recurso. Pendiente de Resolución';
                    end if;
                end if;
                vmul.alegesmid:=vmul.recesmid;
            end if;
            
        ----------------------------------    
        -- recurso extraodinario de revisión
        ----------------------------------
        /*elsif vmul.rer>1 and vmul.fnotrer is null then --and vMul.fnotsan is null
        
            vmul2.vfechaorigenprescripcion:=to_date(vmul.frer+30 ,'dd/mm/rrrr');
            if vmul2.vparalizado>0 then -- PARALIZADO
                if vmul2.vparalizado=9 then
                    vmul.cestado:=4300+vmul.rer+(10*vmul.librer)+vmul.notrer;
                    vmul.estado:='Rec.Ext.Rev. Pendiente de Resolucion. Paralizado';
                else
                    if vmul2.vidconductor=41 then
                        vmul.cestado:=8100;
                        vmul.estado:='Pendiente de Identificación de Conductor';
                    else
                        vmul.cestado:=8200;
                        vmul.estado:='Conductor Identificado';
                    end if;
                end if;
                vmul.alegesmid:=0;
            elsif vmul2.vparalizado=0 and vmul.infq>0 then -- REQUERIMIENTO
                if vmul.infq=2 then
                    vmul.cestado:=6700+(10*vmul.libinfq)+vmul.notinfq;
                    vmul.estado:='Rec.Ext.Rev. Requerimiento Preparado';
                elsif vmul.infq=1 then
                    vmul.cestado:=4700+(10*vmul.libinfq)+vmul.notinfq;
                    vmul.estado:='Rec.Ext.Rev. Requerimiento Pendiente';
                end if;
                vmul.alegesmid:=vmul.infqesmid;
            elsif vmul2.vparalizado=0 and vmul.infq=0 and vmul.infd>0 then -- INFORME DENUNCIANTE
                if vmul.infd=2 and v.mulxboletin<>303 then
                    vmul.cestado:=6500+(10*vmul.libinfd) ;
                    vmul.estado:='Rec.Ext.Rev. Informe a PL Solicitado';
                elsif vmul.infd=2 and v.mulxboletin=303 then
                    vmul.cestado:=6400+(10*vmul.libinfd) ;
                    vmul.estado:='Rec.Ext.Rev. Informe a Cont.ORA. Solicitado';
                end if;
                vmul.alegesmid:=vmul.infdesmid;
            elsif vmul2.vparalizado=0 and vmul.infq=0 and vmul.infd=0 then -- REC.EXT.REV
                if vmul.notrer=5 then -- Estado de Publicación
                    vmul2.vfechaorigenprescripcion:=to_date(vmul.fnotrerintento ,'dd/mm/rrrr') ;
                    vmul.cestado:=9600+(10*vmul.libboprer) ;
                    vmul.estado:='Publicación BOP R.Rec.Ext.Revisión';
                else
                    if vmul.rer=2 then
                        vmul.cestado:=6300+(10*vmul.librer)+vmul.notrer;
                        vmul.estado:='Rec.Ext.Rev. Desestimado';
                    elsif vmul.rer=3 then
                        vmul.cestado:=7300+(10*vmul.librer)+vmul.notrer;
                        vmul.estado:='Rec.Ext.Rev. Estimado';
                    elsif vmul.rer=1 then
                        vmul.cestado:=4300+(10*vmul.librer)+vmul.notrer;
                        vmul.estado:='Rec.Ext.Rev. Pendiente de Resolución';
                    end if;
                end if;
                vmul.alegesmid:=vmul.reresmid;
            end if; */
            
        ----------------------------------            
        -- Sanción Firme
        ----------------------------------
        elsif --(vMul.aleg=0 or vMul.aleg=4)
            --and (vMul.rec=0 or vMul.rec=4)
            --and (vMul.rer=0 or vMul.rer=4)
            --and
            vmul.fnotden is not null then
            vmul.fnotsan:=nvl(vmul.fnotsan ,nvl(vmul.fnotrec ,nvl(vmul.fnotaleg ,vmul.fnotden))) ;
            vmul2.vfechaorigenprescripcion:=to_date(vmul.fnotsan+365 ,'dd/mm/rrrr') ;
            vmul.cestado:=4900+(10*vmul.libsanfir) ;
            vmul.estado:='Ejecutiva';
            -- Fechas Sanción Firme » ORIGEN TRAMITACIÓN
            if vmul.rec=0 and vmul.rer=0 then
                vmul.ffirmeza:=to_date(vmul.fnotsan+30 ,'dd/mm/rrrr') ;
            else
                vmul.ffirmeza:=nvl(nvl(vmul.fnotrer ,vmul.fnotrec) ,to_date(vmul.fnotsan+30 ,'dd/mm/rrrr')) ;
            end if;
            vmul.finivol:=vmul.ffirmeza;
            vmul.ffinvol:=to_date(vmul.ffirmeza+15 ,'dd/mm/rrrr') ;
            
            -- Controla 90 o 120 dias desde notificacion sanción
            if vmul.rec>0 or vmul.rer>0 then
              vDias:=120;
              --vDias:=75;
            else 
              vDias:=90;
              --vDias:=45;
            end if;
            
            -- Ejecutiva en espera
            if to_date(vmul.fnotsan+vDias ,'dd/mm/rrrr')>to_date(sysdate ,'dd/mm/rrrr') then
                vmul.cestado:=4000+(10*vmul.libsanfir) ;
                vmul.estado:='Ejecutiva En Espera';
            end if;
            
            -- Ejecutiva en espera si existe liquidación en estado notificado
            if vmul.estadoliq=6 and vmul.cestado in (4000,4900) then
                vmul.cestado:=4000+(10*vmul.libsanfir) ;
                vmul.estado:='Ejecutiva En Espera. Existe liquidación';                
            end if;
            
            -- CONTROL DE ESCRITOS ESTIMADOS
            if vmul.aleg=3 or vmul.rec=3 or vmul.rer=3 then
                if vmul.aleg=3 then
                    vmul.cestado:=7124;
                    vmul.estado:='Alegación Estimada';
                elsif vmul.rec=3 then
                    vmul.cestado:=7224;
                    vmul.estado:='Recurso Estimado';
                else
                    vmul.cestado:=7324;
                    vmul.estado:='Rec.Ext.Rev. Estimado';
                end if;
            end if;
        else
            vmul.cestado:=230000;
            vmul.estado:='Estado Indeterminado';
        end if;
        -----------------------------------
        -- FIN GENERACIÓN DE ESTADO INICIO
        ----------------------------------
    
    
    
        ----------------------------------    
        -- recurso extraodinario de revisión
        ----------------------------------
        if vmul.rer>1 and vmul.fnotrer is null then --and vMul.fnotsan is null
        
            vmul2.vfechaorigenprescripcion:=to_date(vmul.frer+30 ,'dd/mm/rrrr');
            if vmul2.vparalizado>0 then -- PARALIZADO
                if vmul2.vparalizado=9 then
                    vmul.cestado:=4300+vmul.rer+(10*vmul.librer)+vmul.notrer;
                    vmul.estado:='Rec.Ext.Rev. Pendiente de Resolucion. Paralizado';
                else
                    if vmul2.vidconductor=41 then
                        vmul.cestado:=8100;
                        vmul.estado:='Pendiente de Identificación de Conductor';
                    else
                        vmul.cestado:=8200;
                        vmul.estado:='Conductor Identificado';
                    end if;
                end if;
                vmul.alegesmid:=0;
            elsif vmul2.vparalizado=0 and vmul.infq>0 then -- REQUERIMIENTO
                if vmul.infq=2 then
                    vmul.cestado:=6700+(10*vmul.libinfq)+vmul.notinfq;
                    vmul.estado:='Rec.Ext.Rev. Requerimiento Preparado';
                elsif vmul.infq=1 then
                    vmul.cestado:=4700+(10*vmul.libinfq)+vmul.notinfq;
                    vmul.estado:='Rec.Ext.Rev. Requerimiento Pendiente';
                end if;
                vmul.alegesmid:=vmul.infqesmid;
            elsif vmul2.vparalizado=0 and vmul.infq=0 and vmul.infd>0 then -- INFORME DENUNCIANTE
                if vmul.infd=2 and v.mulxboletin<>303 then
                    vmul.cestado:=6500+(10*vmul.libinfd) ;
                    vmul.estado:='Rec.Ext.Rev. Informe a PL Solicitado';
                elsif vmul.infd=2 and v.mulxboletin=303 then
                    vmul.cestado:=6400+(10*vmul.libinfd) ;
                    vmul.estado:='Rec.Ext.Rev. Informe a Cont.ORA. Solicitado';
                end if;
                vmul.alegesmid:=vmul.infdesmid;
            elsif vmul2.vparalizado=0 and vmul.infq=0 and vmul.infd=0 then -- REC.EXT.REV
                if vmul.notrer=5 then -- Estado de Publicación
                    vmul2.vfechaorigenprescripcion:=to_date(vmul.fnotrerintento ,'dd/mm/rrrr') ;
                    vmul.cestado:=9600+(10*vmul.libboprer) ;
                    vmul.estado:='Publicación BOP R.Rec.Ext.Revisión';
                else
                    if vmul.rer=2 then
                        vmul.cestado:=6300+(10*vmul.librer)+vmul.notrer;
                        vmul.estado:='Rec.Ext.Rev. Desestimado';
                    elsif vmul.rer=3 then
                        vmul.cestado:=7300+(10*vmul.librer)+vmul.notrer;
                        vmul.estado:='Rec.Ext.Rev. Estimado';
                    elsif vmul.rer=1 then
                        vmul.cestado:=4300+(10*vmul.librer)+vmul.notrer;
                        vmul.estado:='Rec.Ext.Rev. Pendiente de Resolución';
                    end if;
                end if;
                vmul.alegesmid:=vmul.reresmid;
            end if;     
      
    end if;
    
    
          setEstadoCobro();    

        -----------------------------------
        -- recurso providencia de apremio
        -----------------------------------
        if vmul.rpa>0 and vmul.rej=0 and vmul.baja=0
            --and vMul.fnotrpa is null
            --and vMul.fSancionFirmeLiq is not null
            --and vMul.fnotpa is not null
            --and (vMul.fnotpa-to_date(vMul.fSancionFirmeLiq,'dd/mm/yyyy'))<365
            --and vMul.estadoLiq in (5) -- Ejecutiva
        then -- and vMul.fnotsan is null then
        
            --vMul2.vFechaOrigenPrescripcion:=to_date(vMul.frpa+30,'dd/mm/rrrr');
            if vmul.rpa=2 then
                vmul.cestado:=5600+(10*vmul.librpa)+vmul.notrpa;
                vmul.estado:='Recurso PA. Desestimado';
            elsif vmul.rpa=3 then
                vmul.cestado:=5700+(10*vmul.librpa)+vmul.notrpa;
                vmul.estado:='Recurso PA. Estimado';
            elsif vmul.rpa=1 then
                vmul.cestado:=5500+(10*vmul.librpa)+vmul.notrpa;
                vmul.estado:='Recurso PA. Pendiente de Resolución';
            end if;
            vmul.alegesmid:=vmul.rpaesmid;
            -- Fechas Sanción Firme » ORIGEN LIQUIDACIÓN
            /*vMul.ffirmeza:=to_date(vMul.fSancionFirmeLiq,'dd/mm/rrrr');
            vMul.finivol:=vMul.ffirmeza;
            vMul.ffinvol:=to_date(vMul.ffirmeza+15,'dd/mm/rrrr');*/
            -- Fechas Sanción Firme » ORIGEN TRAMITACIÓN
            if vmul.rec=0 and vmul.rer=0 then
                vmul.ffirmeza:=to_date(vmul.fnotsan+30 ,'dd/mm/rrrr') ;
            else
                vmul.ffirmeza:=nvl(nvl(vmul.fnotrer ,vmul.fnotrec) ,to_date(vmul.fnotsan+30 ,'dd/mm/rrrr')) ;
            end if;
            vmul.finivol:=vmul.ffirmeza;
            vmul.ffinvol:=to_date(vmul.ffirmeza+15 ,'dd/mm/rrrr') ;
            /*if (vMul.fnotpa-to_date(vMul.fSancionFirmeLiq,'dd/mm/rrrr'))>=365 then
            vMul.cestado:=5500+(10*vMul.librpa)+vMul.notrpa;
            vMul.estado:='Recurso PA. Más de un año entre Sanción Firme y Not.P.A.';
            end if;*/
            /*if vmul.notpa=0 then
                vmul.cestado:=5500;
                vmul.estado:='Recurso PA. Desestimado. Sin Providencia de Apremio';
            end if;*/
        else
            -- cobro sin liquidación
            if vmul.rej>0 then
                if vmul.rej=1 then
                    vmul.cestado:=5100;
                    vmul.estado:='Requerimiento Juzgado. Suspendido con Caución';
                elsif vmul.rej=2 then
                    vmul.cestado:=5100;
                    vmul.estado:='Requerimiento Juzgado. Suspendido sin Caución';
                else
                    vmul.cestado:=5100;
                    vmul.estado:='Requerimiento Juzgado. Levantado';
                end if;
                --- Control de Cobros
            elsif vmul.csl>0 then
            
                if (vmul.cestado>=6200 and vmul.cestado<=6299)  then
                    vmul.estado:=vmul.estado||'. Cobrado.';
                end if;
            
                -- CONTROL DE RECURSO DE REPOSICIÓN COBRADO
                if vmul.cestado not in(4200 ,4210 ,4220) and not(vmul.cestado>=7100 and vmul.cestado<=7199) -- Estimación Alegación
                    and not(vmul.cestado>=7200 and vmul.cestado<=7299) -- Estimación Recurso
                    and not(vmul.cestado>=7300 and vmul.cestado<=7399) -- Estimación RER
                    and not(vmul.cestado>=6200 and vmul.cestado<=6299) -- Destimación Recurso
                    and not(vmul.cestado>=6300 and vmul.cestado<=6399) -- Destimación RER                    
                    then
                    --En el caso de RESOLUCIÓN DE RECURSO DE REPOSICIÓN prevalece
                    --por encima de estar COBRADO. Una vez emitida la notificación
                    --volverá a estado COBRADO
                      vestadoanterior:=vmul.cestado;
                      
                    if vmul.cestado in(4900 ,4910 ,4920) then
                        -- CONTROL DE EXPEDIENTES PAGADOS CON SANCIÓN FIRME PARA SU COMUNICACIÓN A DGT
                        vmul.cestado:=30001;
                    else
                        --vMul.cestado:=vMul.cestado+30000;
                        vmul.cestado:=30000;
                    end if;
                    --
                    -- Actualiza Descripción del Estado
                    if vmul.csl=100 then
                        vmul.estado:='Pendiente Aplicación Incidencia Cobro';
                    elsif vmul.csl=10 then
                        --vMul.estado:='Cobro C60 en ' || vMul.estado;
                        vmul.estado:='Cobro C60';
                    else
                        --vMul.estado:='Cobro CSL en ' || vMul.estado;
                        vmul.estado:='Cobro CSL';
                    end if;
                end if;
                --- FIN Control de Cobros
                -- escrito duplicado
            elsif vmul2.vescritoduplicado then
                vmul.cestado:=260000;
                vmul.estado:='Resolución de Escritos Duplicada.';
            end if;
        end if;
        -------------------------------------
        -- FIN recurso providencia de apremio
        -------------------------------------



        ---------------------------------------
        -- Requerimiento denunciante voluntario
        ---------------------------------------
        if v.incompleto='S' then
        
            vmul.cestado:=100;
            vMul.estado:='Pendiente de Requerimiento y Paralizado';    
        
        end if;        
        
        if vmul.fnotsan is null then
            if vmul.infq=2 and vMul.tipoinfq=20 then
                vmul.cestado:=1200+(10*vmul.libinfq)+vmul.notinfq;
                vmul.estado:='Req. Subsanación Denuncia. Solicitado';
                vmul.alegesmid:=vMul.infqesmid;
            elsif vmul.infq=2 and vMul.tipoinfq=22 then
                vmul.cestado:=1500+(10*vmul.libinfq)+vmul.notinfq;
                vmul.estado:='Req. Denunciante Voluntario. Solicitado';
                vmul.alegesmid:=vMul.infqesmid;
            elsif vmul.infq=1 then
                vmul.cestado:=130;
                vmul.estado:='Req. Solicitado Sin Contenido';
                vmul.alegesmid:=vMul.infqesmid;
            end if;
        end if;



                
        -- CÁLCULO PRESCRIPCIÓN
        if vmul.origendenuncia=4 then
            calculoprescripcion4(to_date(v.mulfec ,'dd/mm/rrrr')) ;
        else
            --calculoprescripcion(to_date(v.mulfec ,'dd/mm/rrrr')) ;
            calcularPrescripcion(v.mulid);
            calcularCaducidad(v.mulid);
            --calcularPrescripcionOld(v.mulid);            
        end if;
                      
        -- CÁLCULO DE CADUCIDAD APLICADA SOLO A EJECUTIVA
        /*if vmul.cestado=4920 and vmul.fnotsan is not null
        and vmul.fnotden is not null
        and to_date(vmul.fnotsan ,'dd/mm/rrrr')-to_date(vmul.fnotden ,'dd/mm/rrrr')>365
        then
            vmul.cestado:=44920;
            vmul.estado:='Posiblemente Caducado';
        end if;*/
             
        -- ESTIMACIÓN RER NO INCLUIDOS EN LIBROS DE BAJA     
        if vMul.rer=3 and vMul.baja=0  then

            vMul.cestado:=7300+(10*vMul.librer)+vMul.notrer;
            vMul.estado:='Rec.Ext.Rev. Estimado';
            vMul.alegesmid:=vMul.reresmid;
            
        end if;
                         
        if trunc(v.mulfecrequer)=trunc(v.mulfecejec) then
            vMul.cestado:=280000;
            vMul.estado:='Error Versión';            
        end if;
        
        -- Estado Notificación Providencia de Apremio
        /*if vMul.cestado in (4920,44920) then
            vMul.cestado:=vMul.cestado+vMul.notpa;
        end if;*/
               
        -- CÁLCULO DE CADUCIDAD
        /*if vmul.origendenuncia=5 then
            if vmul.cestado<9999
            AND to_date(nvl(nvl(vmul.flibsan ,vmul.fnotsan) ,SYSDATE) ,'dd/mm/rrrr')>
            to_date(nvl(vmul.flibinc ,vmul.fnotsan) ,'dd/mm/rrrr')+365
            then
                vmul.cestado:=40000+vmul.cestado;
                vmul.estado:='Posiblemente Caducado';
            end if;
        end if;*/
               
        -- BAJA       
        if vMul.baja>0
        and vmul.cestado not in 
        -- y no hay estimaciones pendientes de notificar
        (
        47100,47110,47120, --Alegación Estimada  
        47200,47210,47220,--Recurso Estimado
        47300,47310,47320--Rec.Ext.Rev. Estimado
        )
        then
            --No existe notificación de baja
            --vMul.cestado:=50000+(10*vMul.libbaja)+vMul.notbaja;
            vMul.cestado:=50000+(10*vMul.libbaja);
            vMul.estado:='Finalizado';
            if vMul.baja=2 then
                vMul.cestado:=5000+(10*vMul.libbaja);
                vMul.estado:='Finalizado';            
            end if;
            vMul.alegesmid:=vMul.bajaesmid;
        end if;   
        
        
        -- REA        
        /*if vMul.rea>0 then -- and vMul.rpa not in (2,3) then
        
             if vMul.rea=2 then
               vMul.cestado:=7000+(10*vMul.librea)+vMul.notrea;
               vMul.estado:='REA Desestimado';
             elsif vMul.rea=3 then
               vMul.cestado:=7000+(10*vMul.librea)+vMul.notrea;
               vMul.estado:='REA Estimado';
             elsif vMul.rea=1 then
               vMul.cestado:=1300+(10*vMul.librea)+vMul.notrea;
               vMul.estado:='REA Pendiente de Resolución';                                    
             end if;
             vMul.alegesmid:=vMul.reaesmid;        
        
        end if;*/
        
        -- Consulta de Titular en DGT de expedientes cobrados
        if vEstadoAnterior=1000 and vMul.cestado in (30000,30001) then
            vMul.cestado:=1000;
        end if;
        
        -- Estimaciones caducadas
        if vMul.cestado in
        (
        47100,47110,47120,47123,47124, --Alegación Estimada  
        47200,47210,47220,47223,47224,--Recurso Estimado
        47300,47310,47320,47323,47324--Rec.Ext.Rev. Estimado
        ) then
        
          vMul.cestado:=substr(vMul.cestado,-4);
        
        end if;
        
        -- RPA Cáducados o Prescritos
        if vMul.cestado in
        (
        45500, --Pendiente de Resolución Pendiente de Incluir en Libro
        45600,45610,45620,45623,45624, --Recurso PA. Desestimado
        45700,45710,45720,45723,45724 --Recurso PA. Estimado
        ) then
        
          vMul.cestado:=substr(vMul.cestado,-4);
        
        end if;        

       
        -- Análisis de Estados Finalizados
        if vMul.cestado in
        (
        5020,--Finalizado
        5723,--Recurso PA. Estimado
        5724,--Recurso PA. Estimado
        5725,--Recurso PA. Estimado
        5726,--Recurso PA. Estimado
        7123,--Alegación. Estimada
        7124,--Alegación Estimada
        7125,--Alegación Estimada
        7126,--Alegación Estimada
        7323,--Rec.Ext.Rev. Estimado
        7324,--Rec.Ext.Rev. Estimado
        7325,--Rec.Ext.Rev. Estimado
        7326    --Rec.Ext.Rev. Estimado
        ) then
        
            vMul.cestado:=vMul.cestado+60000;
        
        end if;    
        

    


    estadoanotacionpuntos(v.mulid);
    --errorenvehiculo(v.mulid);
    
    if vMul.errorenvehiculo=1 and vMul.cestado<4900 then
    
        --vmul.cestado:=vmul.cestado+700000;
        vmul.cestado:=700000;
        vMul.estado:='Error Vehículo > '|| vMul.estado;
        
    end if;
    
    
    -- Estimaciones que no entran en libro según estado de liquidación
    --En la tarea 254 por petición de Eduardo se impiden estimaciones de RPA sobre valores que no estén en los siguientes estados:
    --EJ	EJECUTIVA
    --BA	BAJA
    --CO	COBRADO
    --AC	APLAZAMIENTO COBRADO
    --FC	FRACCIONAMIENTO COBRADO    
    
    -- 02/04/2014 Se añaden nuevos estados
    --170	SUSPENDIDO
    --171	SUSPENDIDO POR APLAZAMIENTO
    --172	SUSPENDIDO POR FRACCIONAMIENTO
    --200	SUSPENDIDO CAUTELARMENTE
    
    if vmul.cestado=5700 --Recurso PA. Estimado
    and vmul.estadoliq not in (481,480,13,35,5,170,171,172,200) then
        vmul.cestado:=5500;
        vmul.estado:='Recurso PA Estimado Sin Trámite';    
    end if;
    
    setEstadoCobro();
    setInfo(v.mulid);
        
                
    end loop; --------------------------------------(1)

    
    

exception
    when others then
       dbms_output.put_line('KO. ' || SQLERRM);
end;




--------------------------------------------------------------------------------
-- Escritos TEA
--------------------------------------------------------------------------------
function eescrito(vEsmid number) return tea_lista pipelined is

cursor cNot(vEsmid number, vLibroNumero number) is
select  /*+RULE*/ p.pntid,n.ntfid,r.remid,c.crmid,a.atoid,
p.perid,p.perversion,p.rdiid,c.crmcodigo,
c.crmfecha,c.crmnumero,n.ntfresultado,n.ntffecnot,a.atocodigo, p.pntobjid esmid
from alba.actosnotificacion a, alba.cabremesas c, alba.remesas r,
alba.notificaciones n, alba.propuestasnotificacion p
where p.pntid=n.pntid
and n.ntfid=r.ntfid
and r.crmid=c.crmid
and p.atoid=a.atoid
and p.pntorigen='I'
and p.atoid=937
and n.ntfcrmid=c.crmid
and p.pntobjid=vEsmid
and exists 
(select 1 from
alba.libro_resoluciones l
where l.lbrnumero=vLibroNumero
and l.lrtid=342
and to_date(p.PNTFECHA,'dd/mm/rrrr')>=to_date(l.lbrfirma,'dd/mm/rrrr'))
;

-- Libros
cursor cLib(vEsmid number) is
select l.lbrfecha, l.lbrid, t.lrtdescorta,
decode(t.lrtrequierefirma,'N',l.lbrfecha,l.lbrfirma) lbrfirma,
l.lbrnumero numeroLibro, l.lbrfirma fresolucionLibro, d.esmid, d.mulid
from alba.libro_resoluciones l, alba.det_libro_resol d, alba.libro_resol_tipos t
where d.lbrid=l.lbrid
and l.lrtid=t.lrtid
and t.lrtid=342
and d.esmid=vEsmid
order by l.lbrid
;

-- Escritos
cursor cEsc(vEsmid number) is
select e.esmid, e.tidomid, e.tdmresid, e.mulid, e.esmnumbol,
e.esmfescrito, e.esmfestado, e.esmfgrabacion,
t.tidomnombre escrito, r.tdmnombre estado, count(rsaid) totalResoluciones,
(select count(esmivalor) from alba.escritosmultasinfo i where i.esmietiqueta='PLENO' and i.esmid=e.esmid and i.esmiiestado=1) pleno,
(select count(esmivalor) from alba.escritosmultasinfo i2 where i2.esmietiqueta='EXPTE. TEA' and i2.esmid=e.esmid and i2.esmiiestado=1) expte_tea,
(select count(esmivalor) from alba.escritosmultasinfo i3 where i3.esmietiqueta='ASUNTO' and i3.esmid=e.esmid and i3.esmiiestado=1) asunto
from
alba.tipos_doc_multas t,
alba.tipos_doc_multas_res r,
alba.resol_alegaciones ra,
alba.escritosmultas e
where e.tidomid=t.tidomid
and e.tdmresid=r.tdmresid
and t.tidomid=101
and r.tdmresid in (261,281,385,401,421,521)
and e.esmid=ra.esmid(+)
and e.esmid=vEsmid
group by e.esmid, e.tidomid, e.tdmresid, e.mulid, e.esmnumbol,
e.esmfescrito, e.esmfestado, e.esmfgrabacion,
t.tidomnombre, r.tdmnombre
;

cursor cfte01(vesmid number) is
select distinct e.esmid, l.liqid, l.liqxestado, ra.resid, l.liqnumerorecliquidacion liqnum
from
alba.escritosmultas e,
alba.resol_alegaciones ra,
alba.multas m,
alba.liquidaciones l
where e.esmid=ra.esmid
and e.mulid=m.mulid
and l.expid=m.expidexp
and e.tidomid=101
and e.tdmresid=261
and ra.resid in (3081,1402,1403,1522,2862,2881,5261,6641,7101,7681,7701,7721,7881,8581,8601,8681,9381,9401,9402,9681,9721,9741,3661,7141,7741,9704,2821,9705,9706,5261,6641,7101,7881,7941,7961,8001,8021,8121,8141,8161,8601,8221,8681,8581,9381,9401)
and e.esmid=vEsmid
;
--A (3081,1402,1403,1522,2862,2881,5261,6641,7101,7681,7701,7721,7881,8581,8601,8681,9381,9401,9402,9681,9721,9741,5261,6641,7101,7881,7941,7961,8001,8021,8121,8141,8161,8601,8221,9381,9401)
--B (3661,7141,7741,9704,2821,9705,9706,8221)
--C (3681,7001,9683,9701,9702,9703)

cursor cFTE02(vEsmid number) is
select distinct e.esmid, l.liqid, l.liqxestado, mm.origendenuncia, l.liqfsancionfirme,
ra.resid, l.liqnumerorecliquidacion liqnum
from
alba.multas m,
alba.multasestados mm,
alba.resol_alegaciones ra,
alba.liquidaciones l,
alba.escritosmultas e
where e.esmid=ra.esmid
and e.mulid=m.mulid
and l.expid=m.expidexp
and m.mulid=mm.mulid
and e.tidomid=101
and e.tdmresid=261
and ra.resid in (3681,7001,9683,9701,9702,9703)
and e.esmid=vEsmid
;


-- Localiza el escrito de ejecución TEA
cursor cEscritoEjecucion(vEsmid number) is
select ee.esmid
from alba.escritosmultas ee
where ee.mulid in
(
select e.mulid
from alba.escritosmultas e
where e.esmid=vEsmid  
)
and ee.tidomid=141
and ee.tdmresid=481
;

-- Libro resolucion ejecución TEA
cursor cLibResTea(vEsmid number) is
select l.lbrfecha, l.lbrid, t.lrtdescorta,
decode(t.lrtrequierefirma,'N',l.lbrfecha,l.lbrfirma) lbrfirma,
l.lbrnumero numeroLibro, l.lbrfirma fresolucionLibro, d.esmid, d.mulid
from alba.libro_resoluciones l, alba.det_libro_resol d, alba.libro_resol_tipos t
where d.lbrid=l.lbrid
and l.lrtid=t.lrtid
and t.lrtid in (1021,1061)
and d.esmid=vEsmid
;

cursor usuario_escrito(vu_esmid number) is
select u.usunombre usuario
from
alba.usuarios u,
alba.escritosmultas e
where e.esmid=vu_esmid
and e.usuidmod=u.usuid;

cursor cestimacionnoautomatica(vesmid number) is
select ra.resid
from
alba.escritosmultas e,
alba.resol_alegaciones ra
where e.esmid=ra.esmid
and e.esmid=vesmid
and e.tidomid=101
and e.tdmresid=261
--and ra.resid in (3081,1402,1403,1522,2862,2881,5261,6641,7101,7681,7701,7721,7881,8581,8601,8681,9381,9401,9402,9681,9721,9741,3661,7141,7741,9704,2821,9705,9706,3681,7001,9683,9701,9702,9703,5261,6641)
;

cursor cestimacion(vesmid number) is
select e.esmid
from alba.escritosmultas e
where e.esmid=vesmid
and e.tidomid=101
and e.tdmresid=261
;

--2ªNotificación
cursor cNotif2(vEsmid number) is
select e.esmid
from alba.escritosmultasinfo e
where e.esmietiqueta='NOTIFICACIÓN'
and e.esmiiestado=1
and e.esmid=vEsmid
;

vestimacionnoautomatica number:=0;
vestadolibrestea number:=0;
vesmidescritoejecucion number:=0;

vdacuenta number:=0;
vHayDatos number:=0;
vTotalLibros number:=0;
      
begin

    vTea.cestado:=0;
    vTea.estado:='nulo';
    vTea.esmid:=0;
    vTea.mulid:=0;
    vTea.rea:=0;
    vTea.pleno:='nulo';
    vTea.exp_tea:='';
    vTea.numeroLibro:=0;
    vTea.libro:=0;
    vTea.firma:='';
    vTea.notrea:=0;
    vTea.fnotrea:='';
    vTea.crmnumero:=0;
    vtea.crmcodigo:='nulo';
    vTea.liq:='';
    vTotalLibros:=0;
  
  --iniciarVariableTEA();
  
  
  
  select count(e.esmid) into vHayDatos
  from alba.escritosmultas e
  where e.tidomid=101
  and e.tdmresid in (261,281,385,401,421,521)
  and e.esmid=vEsmid;
  
  if vHayDatos>0 then

    
        -- Analiza escritos
        for e in cEsc(vEsmid) loop
          vTea.esmid:=e.esmid;
          vTea.mulid:=e.mulid;
          
          if e.pleno=1 then
            select substr(esmivalor,1,200) into vTea.pleno
            from alba.escritosmultasinfo i
            where i.esmietiqueta='PLENO' and i.esmid=e.esmid and i.esmiiestado=1;
          end if;
          
          if e.expte_tea=1 then
            select substr(esmivalor,1,200) into vTea.exp_tea
            from alba.escritosmultasinfo i
            where i.esmietiqueta='EXPTE. TEA' and i.esmid=e.esmid and i.esmiiestado=1;
          end if;
          
          if e.totalResoluciones>0 and e.pleno=1 and e.expte_tea=1 then
            vTea.rea:=2; -- Resuelto
          else
            vTea.rea:=1; -- Pendiente de resolución
          end if;
          
          if vTea.rea=2 and e.tdmresid in (421) then
            vTea.rea:=3; -- Resuelto, Sin Notificación
          end if;
          
          vTea.iEscrito:='<a href="/RsToTable/escritos.jsp?esmid=' || e.esmid ||chr(38)||'debug=1"  target="_blank" >' || e.esmid || '</a>';
        end loop;
        
        vTotalLibros:=0;
        -- Analiza Libros    
        for l in cLib(vEsmid) loop
        
          vTea.numeroLibro:=l.numeroLibro;
        
          if l.lbrfirma is not null then
            vTea.libro:=2;
            vTea.firma:=to_date(l.lbrfirma,'dd/mm/RRRR');
          else
            vTea.libro:=1;
            vTea.firma:=null;
          end if;    
          
          vTotalLibros:=vTotalLibros+1;
        
        end loop;
        
        
        -- Analiza Notificaciones
        for n in cNot(vEsmid,vTea.numeroLibro) loop
        
          --vMul.notreardiid:=n.rdiid;
          --vMul.notreaperid:=n.perid;
          --vMul.notreaperv:=n.perversion;
          --vMul.notreaacto:=n.ntfresultado; --n.atoid;                          
          if n.pntid is not null and n.crmid is null then
            vTea.notrea:=1; -- Existe propuesta
          elsif n.pntid is not null and n.crmid is not null and n.crmnumero is null then
            vTea.notrea:=2; -- Remesa creada
          elsif n.pntid is not null and n.crmid is not null and n.crmnumero is not null and n.ntfresultado is null then
            vTea.notrea:=3; -- Remesa emitida
          elsif n.pntid is not null and n.crmid is not null and n.crmnumero is not null and n.ntfresultado is not null then
            ------------------------------------------------------
            if (n.ntfresultado='M3' or n.ntfresultado='M2') then
              vTea.notrea:=4;
              vTea.fnotrea:=n.ntffecnot;
            elsif (n.ntfresultado='M5') then
              vTea.notrea:=5;
              vTea.fnotrea:=n.ntffecnot;
            elsif (n.ntfresultado='M1') then       
              vTea.notrea:=6;
              vTea.fnotrea:=n.ntffecnot;            
            else
              vTea.notrea:=3;
            end if;
            -------------------------------------------------------
          end if;
          
          if n.crmnumero is not null then
            vTea.crmnumero:=n.crmnumero;
          end if;
          
          if n.crmcodigo is not null then
            vTea.crmcodigo:=n.crmcodigo;        
          end if;
          
        end loop;


--A (3081,1402,1403,1522,2862,2881,5261,6641,7101,7681,7701,7721,7881,8581,8601,8681,9381,9401,9402,9681,9721,9741,5261,6641,7101,7881,7941,7961,8001,8021,8121,8141,8161,8601,9381,9401)
--B (3661,7141,7741,9704,2821,9705,9706,8221)
--C (3681,7001,9683,9701,9702,9703)        
        -- Genera Estado
        if vTea.rea=1 then
          vTea.cestado:=1300;
          vTea.estado:='Escrito resuelto con datos incompletos';
        elsif vTea.rea in (2,3) and vTea.libro=0 then
          vTea.cestado:=7000;
          vtea.estado:='Pendiente de generar libro';
          ---------------------------------------------
          --Control de automatización en la estimación
          ---------------------------------------------
          for ena in cestimacionnoautomatica(vesmid) loop
              if ena.resid in (3081,1402,1403,1522,2862,2881,5261,6641,7101,7681,7701,7721,7881,8581,8601,8681,9381,9401,9402,9681,9721,9741,3661,7141,7741,9704,2821,9705,9706,3681,7001,9683,9701,9702,9703,5261,6641,7101,8221,7881,7941,7961,8001,8021,8121,8141,8161,8601,8681,8581,7921,9381,9401) and vestimacionnoautomatica=0 then
                  vestimacionnoautomatica:=1;
              end if;
          end loop;
          for est in cestimacion(vesmid) loop
              if vestimacionnoautomatica=0 then
                vTea.cestado:=7000;
                vtea.estado:='Estimada sin argumentos de automatización';          
              end if;
          end loop;
          ---------------------------------------------
          ---------------------------------------------          
        elsif vTea.numeroLibro=20160235 then
          vTea.cestado:=7040;
          vTea.estado:='Depósito';           
        elsif vTea.rea in (2,3) and vTea.libro=1 then
          vTea.cestado:=7010;
          vTea.estado:='Pendiente de firma';    
          if vTotalLibros=2 then vTea.estado:=vTea.estado|| ''; end if;
        elsif vTea.rea=2 and vTea.libro=2 and vTea.notrea=0 then
          vTea.cestado:=7020;
          vTea.estado:='Pendiente de emitir notificación';
           elsif vTea.rea=3 and vTea.libro=2 and vTea.notrea=0 then
          vTea.cestado:=7030;
          vTea.estado:='Finalizado. Sin Notificación';
        elsif vTea.rea=2 and vTea.libro=2 and vTea.notrea=3 then
          vTea.cestado:=7023;
          vTea.estado:='Pendiente de resultado de notificación';
          for n2 in cNotif2(vEsmid) loop
            vTea.cestado:=7000;
            vTea.estado:='Segunda Notificación';            
          end loop;          
        elsif vTea.rea=2 and vTea.libro=2 and vTea.notrea=4 then
          vTea.cestado:=7024;
          vTea.estado:='Resolución Notificada';
        elsif vTea.rea=2 and vTea.libro=2 and vTea.notrea=5 then
          vTea.cestado:=7025;
          vTea.estado:='Resolución Devuelta';
        elsif vTea.rea=2 and vTea.libro=2 and vTea.notrea=6 then
          vTea.cestado:=7025;
          vTea.estado:='Resolución Devuelta Dirección Incorrecta';
          for n2 in cNotif2(vEsmid) loop
            vTea.cestado:=7000;
            vTea.estado:='Segunda Notificación';            
          end loop;
        else
          vTea.cestado:=0;
          vTea.estado:='Error';
        end if;
        
        if vTotalLibros=2 and vTea.numeroLibro<>20160235 then
          vTea.estado:=vTea.estado || ' (2ºEnvío)'; 
        end if;
          
        -----------------------
        -- FALLO TEA ESTIMACIÓN
        -----------------------
        
        vTea.cestadoliq:=0;
        vTea.estadoliq:='';
        vestadolibrestea:=0;
        vEsmidEscritoEjecucion:=0;
        
        for ej in cescritoejecucion(vesmid) loop
          
          vEsmidEscritoEjecucion:=ej.esmid;
          
        end loop;
        
        if vEsmidEscritoEjecucion>0 and vEsmidEscritoEjecucion is not null then
        
            -- Analiza Libros    
            for l in cLibResTea(vEsmidEscritoEjecucion) loop
            
              if l.lbrfirma is not null then
                vEstadoLibResTea:=20;
              else
                vEstadoLibResTea:=10;
              end if;    
            
            end loop;
            
        end if;
        
        
        if vTea.cestado in (7020,7030,7023,7024,7025,7026,7040) then
        
            
            --1. Si contiene el argumento 3081,5261 o 3661, las actuaciones serán :
            --    1.1. BAJA si el estado del valor es ejecutiva sin ingresos a cuenta
            --    1.2. BAJA y DEVOLUCIÓN si el estado del valor es ejecutiva con ingresos a cuenta
            --    1.3. DEVOLUCIÓN si el estado del valor es cobrado.
              
            --Campos:
            --esmid, liqid, liqxestado(estado del valor), dacuenta(ingresos a cuenta)
            vTea.estadoliq:='Ausencia Argumentos 3081,5261,3661,7141,7741,9704,2821,9705,9706,3861,6641,7101,8681,8581,7921,9381,9401'; --6641
          
            for f0 in cFTE01(vEsmid) loop
            
              vdacuenta:=0;
              vTea.liq:=substr(f0.liqnum,1,12);
            
              select dacuenta into vdacuenta
              from table(alba.pkg_calculoimportes.ctd(f0.liqid,sysdate));
            
              vTea.estadoliq:='Error Estado Liquidación';
            
              if f0.liqxestado=5 and vdacuenta=0 then
                vTea.cestadoliq:=8100;
                vtea.estadoliq:='Baja';
                if f0.resid in (3081,1402,1403,1522,2862,2881,5261,6641,7101,7681,7701,7721,7881,8581,8601,8681,9381,9401,9402,9681,9721,9741,5261,6641,7101,7881,7941,7961,8001,8021,8121,8141,8161,8601,8681,8581,9381,9401) THEN
                  vTea.estadoliq:=vTea.estadoliq||';4621';
                elsif f0.resid in (3661,7141,7741,9704,2821,9705,9706,8221,7921) then
                  vTea.estadoliq:=vTea.estadoliq||';4561';
                end if;
              end if;
              if f0.liqxestado=5 and vdacuenta>0 then
                vTea.cestadoliq:=8200;
                vtea.estadoliq:='Baja y devolución';
                if f0.resid in (3081,1402,1403,1522,2862,2881,5261,6641,7101,7681,7701,7721,7881,8581,8601,8681,9381,9401,9402,9681,9721,9741,5261,6641,7101,7881,7941,7961,8001,8021,8121,8141,8161,8601,8681,8581,9381,9401) then
                  vTea.estadoliq:=vTea.estadoliq||';4543';
                elsif f0.resid in (3661,7141,7741,9704,2821,9705,9706,8221,7921) then
                  vTea.estadoliq:=vTea.estadoliq||';4501';
                end if;
              end if;
              if f0.liqxestado in (481,480,13) then
                vtea.cestadoliq:=8200;
                vtea.estadoliq:='Devolución';
                if f0.resid in (3081,5261,6641,7101,7881,7941,7961,8001,8021,8121,8141,8161,8601,8681,8581,9381,9401) THEN
                  vTea.estadoliq:=vTea.estadoliq||';4544';
                elsif f0.resid in (3661,8221,7921) then
                  vtea.estadoliq:=vtea.estadoliq||';4521';
                end if;
              end if;
            end loop;
            
            
            
            --2. Si contiene el argumento 3681,5261 y el estado del valor es ejecutiva con ingresos a cuenta:
            --    2.1. Si es un expediente de la antigua Ley de tráfico, entonces BAJA y DEVOLUCIÓN
            --    2.2. Si no es un expediente de la antigua Ley de tráfico, entonces:
            --        2.2.1. Si la fecha de firmeza de sanción más 3 años y 9 meses es inferior a la fecha
            --        del sistema, entonces BAJA Y DEVOLUCIÓN
            --        2.2.2. Si la fecha de firmeza de sanción más 3 años y 9 meses es posterior o igual a
            --        la fecha del sistema, entonces NOTIFICACIÓN DE LA PROVIDENCIA DE APREMIO
            --3. Si contiene el argumento 3681,5261 y el estado del valor es ejecutiva sin ingresos a cuenta,
            --las actuaciones a realizar serán:
            --    3.1. Si es un expediente de la antigua Ley de tráfico, entonces BAJA
            --    3.2. Si no es un expediente de la antigua Ley de tráfico, entonces:
            --        3.2.1. Si la fecha de firmeza de sanción más 3 años y 9 meses
            --        es inferior a la fecha del sistema, entonces BAJA
            --        3.2.2. Si la fecha de firmeza de sanción más 3 años y 9 meses
            --        es posterior o igual a la fecha del sistema,
            --        entonces NOTIFICACIÓN DE LA PROVIDENCIA DE APREMIO
            --4. Si contiene el argumento 3681 y el estado del valor es cobrado, las 
            --actuaciones a realizar serán DEVOLUCIÓN DE RECARGOS (menos el 5%),
            --INTERESES Y COSTAS
      
            
            --Campos:
            --esmid, liqid, liqxestado, dacuenta, origendenuncia, liqfsancionfirme
          
            for f2 in cFTE02(vEsmid) loop
            
              vdacuenta:=0;
              vTea.liq:=substr(f2.liqnum,1,12);
            
              select dacuenta into vdacuenta
              from table(alba.pkg_calculoimportes.ctd(f2.liqid,sysdate));
              
              vTea.estadoliq:='Error Estado Liquidación';
            
              if f2.liqxestado=5 and vdacuenta>0 then --Ejecutiva
              
                if f2.origendenuncia=5 then --NUEVA LEY
                  if add_months(f2.liqfsancionfirme,45)<sysdate then
                    vTea.cestadoliq:=8200;
                    vTea.estadoliq:='Baja y devolución';
                    vTea.estadoliq:=vTea.estadoliq||';4581';
                  else
                    vTea.cestadoliq:=8100;
                    vtea.estadoliq:='Notf. prov. apremio';
                    vTea.estadoliq:=vTea.estadoliq||';4541';
                  end if;
                else --ANTIGUA LEY
                  vTea.cestadoliq:=8200;
                  vTea.estadoliq:='Baja y devolución';
                  vtea.estadoliq:=vtea.estadoliq||';4581';
                end if;
                
              elsif f2.liqxestado=5 and vdacuenta=0 then
      
                if f2.origendenuncia=5 then --NUEVA LEY
                  if add_months(f2.liqfsancionfirme,45)<sysdate then
                    vtea.cestadoliq:=8100;
                    vTea.estadoliq:='Baja';
                    vTea.estadoliq:=vTea.estadoliq||';4601';
                  else
                    vTea.cestadoliq:=8100;
                    vTea.estadoliq:='Notf. prov. apremio';
                    vTea.estadoliq:=vTea.estadoliq||';4582';
                  end if;
                else --ANTIGUA LEY
                  vTea.cestadoliq:=8100;
                  vTea.estadoliq:='Baja';
                  vTea.estadoliq:=vTea.estadoliq||';4601';
                end if;
                
              elsif f2.liqxestado in (481,480,13) then --Cobrado
              
                vTea.cestadoliq:=8200;
                vtea.estadoliq:='Dev. Recargo Intereses Costas';
                vTea.estadoliq:=vTea.estadoliq||';4542';
              
              end if;
            end loop;
            
            
        end if;
        
        if vtea.estadoliq in
        ('Error Estado Liquidación','Ausencia Argumentos 3081,5261,3661,7141,7741,9704,2821,9705,9706,3861,6641,7101,8681,8581,7921,9381,9401')
        then
            for usu in usuario_escrito(vesmid) loop
              vtea.estadoliq:=vtea.estadoliq||' '||usu.usuario;
            end loop;
        end if;
        
        vtea.cestadoliq:=vtea.cestadoliq+vestadolibrestea;
              
        
        pipe row(vTea);
        
  end if;
    
exception
    when others then
       dbms_output.put_line('KO. ' || sqlerrm);
end;





--------------------------------------------------------------------------------
-- Escritos TCA
--------------------------------------------------------------------------------
function eescrito_tca(vEsmid number) return tea_lista pipelined is

-- Libros
cursor cLib(vEsmid number) is
select l.lrtid, l.lbrfecha, l.lbrid, t.lrtdescorta,
decode(t.lrtrequierefirma,'N',l.lbrfecha,l.lbrfirma) lbrfirma,
l.lbrnumero numeroLibro, l.lbrfirma fresolucionLibro, d.esmid, d.mulid
from alba.libro_resoluciones l, alba.det_libro_resol d, alba.libro_resol_tipos t
where d.lbrid=l.lbrid
and l.lrtid=t.lrtid
and t.lrtid in (1421,1422,1423,1424)
and d.esmid=vEsmid
order by l.lbrid
;

-- Escritos
cursor cEsc(vEsmid number) is
select e.esmid, e.tidomid, e.tdmresid, e.mulid, e.esmnumbol,
e.esmfescrito, e.esmfestado, e.esmfgrabacion,
t.tidomnombre escrito, r.tdmnombre estado, count(rsaid) totalResoluciones
from
alba.tipos_doc_multas t,
alba.tipos_doc_multas_res r,
alba.resol_alegaciones ra,
alba.escritosmultas e
where e.tidomid=t.tidomid
and e.tdmresid=r.tdmresid
and t.tidomid=242
and r.tdmresid in (641,642,682)
and e.esmid=ra.esmid(+)
and e.esmid=vEsmid
group by e.esmid, e.tidomid, e.tdmresid, e.mulid, e.esmnumbol,
e.esmfescrito, e.esmfestado, e.esmfgrabacion,
t.tidomnombre, r.tdmnombre
;


cursor cResol(vesmid number) is
select distinct e.esmid, l.liqid, l.liqxestado, ra.resid, l.liqnumerorecliquidacion liqnum
from
alba.escritosmultas e,
alba.resol_alegaciones ra,
alba.multas m,
alba.liquidaciones l
where e.esmid=ra.esmid
and e.mulid=m.mulid
and l.expid=m.expidexp
and e.tidomid=242
and e.tdmresid=682
--and ra.resid in (3081,3661,5261)
and e.esmid=vEsmid
union
select distinct e.esmid, l.liqid, l.liqxestado, ra.resid, l.liqnumerorecliquidacion liqnum
from
alba.escritosmultas e,
alba.resol_alegaciones ra,
alba.liquidaciones l
where e.esmid=ra.esmid
and e.liqid=l.liqid
and e.tidomid=242
and e.tdmresid=682
and e.esmid=vEsmid
;

vestimacionnoautomatica number:=0;
vestadolibrestea number:=0;
vesmidescritoejecucion number:=0;
vTotalResoluciones number:=0;

vdacuenta number:=0;
vHayDatos number:=0;
vTipoLibro number:=0;
      
begin

    vTea.cestado:=0;
    vTea.estado:='nulo';
    vTea.esmid:=0;
    vTea.mulid:=0;
    vTea.rea:=0;
    vTea.pleno:='nulo';
    vTea.exp_tea:='';
    vTea.numeroLibro:=0;
    vTea.libro:=0;
    vTea.firma:='';
    vTea.notrea:=0;
    vTea.fnotrea:='';
    vTea.crmnumero:=0;
    vtea.crmcodigo:='nulo';
    vTea.liq:='';
    
    vTea.cestadoliq:=0;
    vTea.estadoliq:='';
    vestadolibrestea:=0;
    vEsmidEscritoEjecucion:=0;
    vTipoLibro:=0;
  
    --iniciarVariableTEA();
  
  
  -- Acatamiento de Setencia
  select count(e.esmid) into vHayDatos
  from alba.escritosmultas e
  where e.tidomid=242
  and e.tdmresid in (641,642,682)
  and e.esmid=vEsmid;
  
  if vHayDatos>0 then

    
        -- Analiza escritos
        for e in cEsc(vEsmid) loop
          vTea.esmid:=e.esmid;
          vTea.mulid:=e.mulid;
          
          vTotalResoluciones:=e.totalResoluciones;
          
          if e.tdmresid=641 then
            vTea.rea:=1; -- Estimado
          elsif e.tdmresid=642 then
            vTea.rea:=2; -- Desestimado o Finalizado
          elsif e.tdmresid=682 then
            vTea.rea:=3; -- Desestimado o Finalizado            
          end if;
          
          vTea.iEscrito:='<a href="/RsToTable/escritos.jsp?esmid=' || e.esmid ||chr(38)||'debug=1"  target="_blank" >' || e.esmid || '</a>';
        end loop;
        
        
        -- Analiza Libros    
        for l in cLib(vEsmid) loop
          -- Acatamiento
          if l.lrtid in (1421,1422) then
                vTea.numeroLibro:=l.numeroLibro;
                
                if l.lbrfirma is not null then
                  vTea.libro:=2;
                  vTea.firma:=to_date(l.lbrfirma,'dd/mm/RRRR');
                else
                  vTea.libro:=1;
                  vTea.firma:=null;
                end if;    
          end if;
          -- Ejecución
          if l.lrtid in (1423,1424) then
                
                if l.lbrfirma is not null then
                  vestadolibrestea:=2;
                else
                  vestadolibrestea:=1;
                end if;    
          end if;     
          
          vTipoLibro:=l.lrtid;
        end loop;
        
        
        -- Genera Estado
        if vTea.rea in (1,2) and vTea.libro=0 then
          if vTea.rea=1 then
            vTea.cestado:=7500;
            vtea.estado:='Pendiente de generar libro';
          else
            vTea.cestado:=7600;
            vtea.estado:='Pendiente de generar libro';          
          end if;
        elsif vTea.rea in (1,2) and vTea.libro=1 then
          if vTea.rea=1 then
            vTea.cestado:=7510;
            vtea.estado:='Pendiente de firma';
          else
            vTea.cestado:=7610;
            vtea.estado:='Pendiente de firma';          
          end if;    
        elsif vTea.rea in (1,2) and vTea.libro=2 then
          if vTea.rea=1 then
            vTea.cestado:=7530;
            vtea.estado:='Pendiente de Ejecución';
          else
            vTea.cestado:=7620;
            vtea.estado:='Finalizado';          
          end if;            
        elsif vTea.rea=3 and vTotalResoluciones=0 then
          vTea.cestado:=7530;
          vTea.estado:='Pendiente de Ejecución. Sin Resolución';
        elsif vTea.rea=3 and vTotalResoluciones>0 and vestadolibrestea=0 then
          vTea.cestado:=8000;
          vTea.estado:='Pendiente de generar libro';          
        elsif vTea.rea=3 and vTotalResoluciones>0 and vestadolibrestea=1 then
          vTea.cestado:=8010;
          vTea.estado:='Pendiente de firma';          
        elsif vTea.rea=3 and vTotalResoluciones>0 and vestadolibrestea=2 then
          vTea.cestado:=8020;
          vTea.estado:='Finalizado. Estimación Ejecutada';
          if vTipoLibro=1423 then
            vTea.cestado:=8520;
          elsif vTipoLibro=1424 then
            vTea.cestado:=8620;
          end if;
        end if;
        
        if vTea.cestado in (8000) then
          
          for r in cResol(vTea.esmid) loop
            
            if r.resid in (6492,6496,6500,6493,6494) then
                vTea.cestado:=8500;
                vTea.estado:='STCA Ejecución';                
            else
                vTea.cestado:=8600;
                vTea.estado:='STCA Ejecución Devolución';                    
            end if;
              
          end loop;
          
        end if;
        

        pipe row(vTea);
        
  end if;
    
exception
    when others then
       dbms_output.put_line('KO. ' || sqlerrm);
end;



--------------------------------------------------------------------------------
-- Escritos TEA
--------------------------------------------------------------------------------
function eescrito_tea(vEsmid number) return tea_lista pipelined is

-- Libros
cursor cLib(vEsmid number) is
select l.lrtid, l.lbrfecha, l.lbrid, t.lrtdescorta,
decode(t.lrtrequierefirma,'N',l.lbrfecha,l.lbrfirma) lbrfirma,
l.lbrnumero numeroLibro, l.lbrfirma fresolucionLibro, d.esmid, d.mulid
from alba.libro_resoluciones l, alba.det_libro_resol d, alba.libro_resol_tipos t
where d.lbrid=l.lbrid
and l.lrtid=t.lrtid
and t.lrtid in (1021,1061)
and d.esmid=vEsmid
order by l.lbrid
;

-- Escritos
cursor cEsc(vEsmid number) is
select e.esmid, e.tidomid, e.tdmresid, e.mulid, e.esmnumbol,
e.esmfescrito, e.esmfestado, e.esmfgrabacion,
t.tidomnombre escrito, r.tdmnombre estado, count(rsaid) totalResoluciones
from
alba.tipos_doc_multas t,
alba.tipos_doc_multas_res r,
alba.resol_alegaciones ra,
alba.escritosmultas e
where e.tidomid=t.tidomid
and e.tdmresid=r.tdmresid
and t.tidomid=141
and r.tdmresid in (481,721) --Estimado, Pendiente
and e.esmid=ra.esmid(+)
and e.esmid=vEsmid
group by e.esmid, e.tidomid, e.tdmresid, e.mulid, e.esmnumbol,
e.esmfescrito, e.esmfestado, e.esmfgrabacion,
t.tidomnombre, r.tdmnombre
;


cursor cResol(vesmid number) is
select distinct e.esmid, l.liqid, l.liqxestado, ra.resid, l.liqnumerorecliquidacion liqnum
from
alba.escritosmultas e,
alba.resol_alegaciones ra,
alba.multas m,
alba.liquidaciones l
where e.esmid=ra.esmid
and e.mulid=m.mulid
and l.expid=m.expidexp
and e.tidomid=141
and e.tdmresid=481
and e.esmid=vEsmid
union
select distinct e.esmid, l.liqid, l.liqxestado, ra.resid, l.liqnumerorecliquidacion liqnum
from
alba.escritosmultas e,
alba.resol_alegaciones ra,
alba.liquidaciones l
where e.esmid=ra.esmid
and e.liqid=l.liqid
and e.tidomid=141
and e.tdmresid=481
and e.esmid=vEsmid
;

cursor cLiq(vesmid number) is
select distinct e.esmid, l.liqid, l.liqxestado, l.liqnumerorecliquidacion liqnum
from
alba.escritosmultas e,
alba.multas m,
alba.liquidaciones l
where e.mulid=m.mulid
and l.expid=m.expidexp
and e.tidomid=141
and e.tdmresid=481
and (select dacuenta from table(alba.pkg_calculoimportes.ctd(l.liqid,sysdate)))>0
and e.esmid=vEsmid
union
select distinct e.esmid, l.liqid, l.liqxestado, l.liqnumerorecliquidacion liqnum
from
alba.escritosmultas e,
alba.liquidaciones l
where e.liqid=l.liqid
and e.tidomid=141
and e.tdmresid=481
and (select dacuenta from table(alba.pkg_calculoimportes.ctd(l.liqid,sysdate)))>0
and e.esmid=vEsmid
;

cursor cAuto(vesmid number) is
select i.esmid, i.esmivalor, i.esmietiqueta
from 
alba.escritosmultasinfo i
where i.esmid=vesmid
and i.esmietiqueta='EXPTE. TEA'
;

vestimacionnoautomatica number:=0;
vestadolibrestea number:=0;
vesmidescritoejecucion number:=0;
vTotalResoluciones number:=0;

vdacuenta number:=0;
vHayDatos number:=0;
vTipoLibro number:=0;
vAutomatico number:=0;
      
begin

    vTea.cestado:=0;
    vTea.estado:='nulo';
    vTea.esmid:=0;
    vTea.mulid:=0;
    vTea.rea:=0;
    vTea.pleno:='nulo';
    vTea.exp_tea:='';
    vTea.numeroLibro:=0;
    vTea.libro:=0;
    vTea.firma:='';
    vTea.notrea:=0;
    vTea.fnotrea:='';
    vTea.crmnumero:=0;
    vtea.crmcodigo:='nulo';
    vTea.liq:='';
    
    vTea.cestadoliq:=0;
    vTea.estadoliq:='';
    vestadolibrestea:=0;
    vEsmidEscritoEjecucion:=0;
    vTipoLibro:=0;
  
  
  -- Acatamiento de Resolución TEA
  select count(e.esmid) into vHayDatos
  from alba.escritosmultas e
  where e.tidomid=141
  and e.tdmresid in (481,721)
  and e.esmid=vEsmid;
  
  --Comprueba si proviene de resolución automática
  for a in cAuto(vEsmid) loop
    vAutomatico:=1;
  end loop;
  
  if vHayDatos>0 and vAutomatico=0 then

        
        -- Analiza escritos
        for e in cEsc(vEsmid) loop
          vTea.esmid:=e.esmid;
          vTea.mulid:=e.mulid;
          
          vTotalResoluciones:=e.totalResoluciones;
          
          if e.tdmresid=481 then
            vTea.rea:=1; -- Estimado
          elsif e.tdmresid=721 then
            vTea.rea:=0; -- Pendiente
          end if;
          
          vTea.iEscrito:='<a href="/RsToTable/escritos.jsp?esmid=' || e.esmid ||chr(38)||'debug=1"  target="_blank" >' || e.esmid || '</a>';
        end loop;
        
        
        -- Analiza Libros    
        for l in cLib(vEsmid) loop
          -- Ejecución Resolución TEA
          if l.lrtid in (1021,1061) then
                vTea.numeroLibro:=l.numeroLibro;
                
                if l.lbrfirma is not null then
                  vTea.libro:=2;
                  vTea.firma:=to_date(l.lbrfirma,'dd/mm/RRRR');
                else
                  vTea.libro:=1;
                  vTea.firma:=null;
                end if;    
          end if;
         
          vTipoLibro:=l.lrtid;
        end loop;
        
        
        -- Genera Estado
        if vTea.rea=0 then
            vTea.cestado:=0;
            vtea.estado:='Pendiente de resolución';
        elsif vTea.rea=1 then
            if vTotalResoluciones=0 then
              vTea.cestado:=1;
              vtea.estado:='Resuelto sin argurmentos';      
            elsif vTotalResoluciones>0 then
              if vTea.libro=0 then
                -- Por defecto se incluye en libro de Baja
                vTea.cestado:=8100;
                vtea.estado:='Baja';
                -- Si hay cantidades a cuenta Baja y Devolución
                for l in cLiq(vEsmid) loop
                  vTea.cestado:=8200;
                  vtea.estado:='Baja y devolución';
                end loop;
              elsif vTea.libro=1 then
                vTea.cestado:=3;
                vtea.estado:='Pendiente de firma';
              elsif vTea.libro=2 then
                vTea.cestado:=4;
                vtea.estado:='Firmado';
              end if;
            end if;
        end if;
        
        pipe row(vTea);
        
  end if;
    
exception
    when others then
       dbms_output.put_line('KO. ' || sqlerrm);
end;



--------------------------------------------------------------------------------
-- emultas: Estado de Tramitación
--------------------------------------------------------------------------------
function emultas(vID number) return mul_lista pipelined is

-- Multas
cursor cmul(vmulid number)
is
    select /*+RULE*/ m.*
    from alba.multas m
    where m.mulversion=
        (select max(mm.mulversion)
        from alba.multas mm
        where mm.seoidexp=m.seoidexp
        ) and m.mulid=vmulid and m.mulfecgrab>=to_date('04/08/2006' ,'dd/mm/rrrr') ;
  
  mulfecOld date:=null;


begin

    iniciaregistro();
    mulfecOld:=null;

    for v in cMul(vId) loop
    
        /* Eliminado 20/12/2011
        if v.mulxboletin is not null and v.mulxboletin in (423,444,464) then
        -- Botellón, Sin Matrícula, Convivencia*/
        
        -- Nuevo 20/12/2011
        if v.mulxboletin is not null and v.mulxboletin in (423,464,623,664,723,743,800) then
        -- Botellón, Convivencia
        
            emultas1(vId);
        
        else
 
            if v.infid is not null and v.mulfec is not null
            and v.mulfec > to_date('24/05/2010','dd/mm/rrrr')
            and v.infid > 9000004 then
                emultas2(vId);
            else
                  if v.infid is not null and v.mulfec is not null
                  and v.mulxmedida4 is not null
                  and v.mulfec > to_date('24/05/2010','dd/mm/rrrr')
                  and v.infid in (6000882,6000881) then
                  
                          select m2.mulfec into mulfecOld from alba.multas m2 where m2.mulid=v.mulxmedida4;
                          
                          if mulfecOld is not null and mulfecOld > to_date('24/05/2010','dd/mm/rrrr') then
                              emultas2(vId);
                          else
                              emultas1(vId);
                          end if;
                  
                  else
                
                          if v.mulfec is not null
                          and v.mulfec > to_date('24/05/2010','dd/mm/rrrr')
                          and v.mulxboletin=303 then
                              emultas2(vId);
                          else
                              emultas1(vId);
                          end if;
                          
                  end if;
            end if;
          end if;

        pipe row(vMul);     
        
    end loop;
      
    return;      


exception
    when others then
       dbms_output.put_line('KO. ' || sqlerrm);
end;









--------------------------------------------------------------------------------
-- NUEVA VERSIÓN 1.6
--------------------------------------------------------------------------------
function nuevaVersion16(vMulid number, vPerid number, vPerversion number, vRdiid number, vFechaComCon date)
return number is

    -- Cursores
    cursor cPerSinConductor(vId number) is
    select e.eicid, e.repid, e.conid, e.expid, e.eicxtipocontribuyente,
           e.eicporc, e.eicobli, e.eicnotif, e.eicfhoramod, e.usuidmod,
           e.eicmotivo, e.eicfvigencia, e.eicxderechoprev, e.eicxtipotitular
    from alba.expedientesingresoscontr e
    where e.eicfvigencia is null
    and e.eicxtipocontribuyente<>327
    and e.expid=vId;


    cursor cPer(vId number) is
    select e.eicid, e.repid, e.conid, e.expid, e.eicxtipocontribuyente,
           e.eicporc, e.eicobli, e.eicnotif, e.eicfhoramod, e.usuidmod,
           e.eicmotivo, e.eicfvigencia, e.eicxderechoprev, e.eicxtipotitular
    from alba.expedientesingresoscontr e
    where e.eicfvigencia is null
    and e.expid=vId;
    
    cursor cPer2(vId number) is
    select e.eicid, e.repid, e.conid, e.expid, e.eicxtipocontribuyente,
    e.eicporc, e.eicobli, e.eicnotif, e.eicfhoramod, e.usuidmod,
    e.eicmotivo, e.eicfvigencia, e.eicxderechoprev, e.eicxtipotitular
    from alba.expedientesingresoscontr e
    where e.eicfvigencia is null
    and e.eicxtipocontribuyente<>327
    and e.expid=vId;

    -- Atributos
    parresultado varchar2(400);
    vHoyDia date:=null;
    vSysdate date:=null;
    vNewExpid number:=0;
    vNewMulid number:=0;
    vExpid number:=0;
    vNewEicid number:=0;
    vNewVersion number:=0;
    vNewConid number:=0;
    vNewTedid number:=0;
    vMulidFinal number:=0;
    vInfid number:=0;
    vInfImporte alba.multas.mulimp%type:=0;
    vInfDes alba.multas.muldenuncia%type:=0;
    vEsDenunciaNoIdentificacion number:=0;
    vImporte number:=0;

begin

        DBMS_OUTPUT.PUT_LINE('nv16.01 » Mulid='||vMulid);

        vHoyDia:=to_date(sysdate,'dd/mm/yyyy');
        vSysdate:=sysdate;
    
        -- Guarda el expid
        select m.expidexp, m.mulxmedida4 into vExpid, vEsDenunciaNoIdentificacion from alba.multas m where m.mulid=vMulid;
    DBMS_OUTPUT.PUT_LINE('nv16.02');

        
        if vEsDenunciaNoIdentificacion is null then
    
            DBMS_OUTPUT.PUT_LINE('nv16.03');    

                -- Nueva versión
                select max(m3.mulversion)+1 into vNewVersion from alba.multas m3 where m3.seoidexp=
                (select mm.seoidexp from alba.multas mm where mm.mulid=vMulid);
            
            DBMS_OUTPUT.PUT_LINE('nv16.04');    

                -- Nuevo expingre
                select alba.expingre_sq.NEXTVAL into vNewExpid from dual;
            
            DBMS_OUTPUT.PUT_LINE('nv16.05');    

                insert into alba.expingre
                (expid, expcod, expfalta, expfdecl, expfbaja,
                 act_atuid, mexid, expfhoramod, usuidmod, expmotivo,
                 expejercicio, expnumexpediente, expsufijo,expnumdocumento)
                select vNewExpid, e.expcod, vHoyDia, vHoyDia, e.expfbaja,
                       e.act_atuid, e.mexid, vSysdate, 0, e.expmotivo,
                       e.expejercicio, e.expnumexpediente, e.expsufijo,
                       e.expnumdocumento
                from alba.expingre e where e.expid=(select m.expidexp from alba.multas m where m.mulid=vMulid);
            
            DBMS_OUTPUT.PUT_LINE('nv16.06');    

                -- Nuevo objeto
                insert into alba.seobjetos
            (seoid, expid, seofhoramod, usuidmod, seomotivo, seofvigencia)
                select s.seoid, vNewExpid, vSysdate, 0, 'Se ha creado', s.seofvigencia
                from alba.seobjetos s where s.expid=vExpid;
            
            DBMS_OUTPUT.PUT_LINE('nv16.07');    

                update alba.expingre e
                set e.expfbaja=vHoyDia
                where e.expid=vExpid;
            
            DBMS_OUTPUT.PUT_LINE('nv16.08');    

                -- Clonación de multa
                select alba.multas_sq.NEXTVAL into vNewMulid from dual;
                vMulidFinal:=vNewMulid;
            
            DBMS_OUTPUT.PUT_LINE('nv16.09');    
                
                select decode(i.infgrado,'L',9002328,9002344),
                decode(i.infgrado,'L',i.infimporte*2,infimporte*3)
                into vInfid, vImporte
                from alba.infracciones i, alba.multas m
                where m.infid=i.infid
                and m.mulid=vMulid;
                
                DBMS_OUTPUT.PUT_LINE('nv16.10');    

                select i.infdescripcion, i.infimporte
                into vInfDes, vInfImporte
                from alba.infracciones i
                where i.infid=vInfid;
                
                DBMS_OUTPUT.PUT_LINE('nv16.11');    

                insert into alba.multas a
                       (a.mulid, a.infid, a.mulsituinfra, a.multipobolet, a.mulsitutrami,
                       a.mulnotestado, a.viaid, a.mullugar, a.mulfrente, a.vemid,
                       a.muldelante, a.mulanobol, a.mulxboletin, a.mulnumbol,
                       a.mulversion, a.seoidexp, a.expidexp, a.seoidveh, a.expidveh,
                       a.tvtmid, a.mulfec, a.mulhor, a.mulmin, a.mulfirma, a.mulmarca,
                       a.mulbasti, a.mulxcolor, a.mulatestado, a.mulatefecresol,
                       a.mulxateorginformante, a.mulrec, a.mulimp, a.age1_ageid,
                       a.age2_ageid, a.muldenuncia, a.mulnotmano, a.mulxmotivo,
                       a.mulxgrua, a.mulxmedida1, a.mulxmedida2, a.mulxmedida3,
                       a.mulxmedida4, a.mulresmedida1, a.mulresmedida2, a.mulresmedida3,
                       a.mulresmedida4, a.mulobsdir, a.mulpermiso, a.mulfecper,
                       a.mulclase, a.mulfecnac, a.mulexpperm, a.mulobservaciones,
                       a.mulretirada, a.mulunidad1, a.mulpresencio1, a.mulunidad2,
                       a.mulpresencio2, a.muldtodeposito, a.mulimpdeposito,
                       a.mulfecgrab, a.mulfeciniprod, a.mulfecprescrip,
                       a.mulfeccaducidad, a.mulfecrequer, a.mulfecfinplcomcon,
                       a.mulfecnotden, a.mulfecsancion, a.mulfecsanfir, a.mulfecejec,
                       a.mulfeccob, a.mulfecalegden, a.mulfecrecsan, a.mulfecanula,
                       a.mulfecarch, a.mulfdesde, a.mulfhasta, a.mulmiembrofse,
                       a.mulfoto, a.mulpruebas, a.mulfhoramod, a.usuidmod, a.mulmotivo,
                       a.mulfvigencia, a.mulfecnotifcond, a.mulmatri, a.descnonotif)
                select vNewMulid, vInfid, m.mulsituinfra, m.multipobolet, m.mulsitutrami,
                       m.mulnotestado, 516809, '', '', m.vemid,
                       '', m.mulanobol, m.mulxboletin, m.mulnumbol,
                       vNewVersion, m.seoidexp, vNewExpid, m.seoidveh, m.expidveh,
                       m.tvtmid, vHoyDia, 12, 00, m.mulfirma, m.mulmarca,
                       m.mulbasti, m.mulxcolor, m.mulatestado, m.mulatefecresol,
                       m.mulxateorginformante, m.mulrec, vImporte, null,
                       null, vInfDes, 'N', null, -- Notificada en Mano: N, motivo de no notificación:  nulo
                       m.mulxgrua, m.mulxmedida1, m.mulxmedida2, m.mulxmedida3,
                       vMulid, m.mulresmedida1, m.mulresmedida2, m.mulresmedida3,
                       m.mulresmedida4, null, m.mulpermiso, m.mulfecper,
                       m.mulclase, m.mulfecnac, m.mulexpperm, null,
                       m.mulretirada, m.mulunidad1, m.mulpresencio1, m.mulunidad2,
                       m.mulpresencio2, m.muldtodeposito, m.mulimpdeposito,
                       vSysdate, m.mulfeciniprod, m.mulfecprescrip,
                       m.mulfeccaducidad, null, m.mulfecfinplcomcon,
                       null, null, null, null,
                       m.mulfeccob, null, null, null,
                       m.mulfecarch, vSysdate, null, m.mulmiembrofse,
                       m.mulfoto, m.mulpruebas, vSysdate, 0, 'Nueva Versión',
                       m.mulfvigencia, null, m.mulmatri, null
                from alba.multas m where m.mulid=vMulid;
                
                DBMS_OUTPUT.PUT_LINE('nv16.12');    

                /*
                SELECT a.mulid, a.infid, a.mulsituinfra, a.multipobolet, a.mulsitutrami,

                  FROM alba.multas a
                  */

               -- Clonación de personas y direcciones asociadas al antiguo expediente
                for p in cPer2(vExpid) loop
            
                  DBMS_OUTPUT.PUT_LINE('nv16.13');    

                            select alba.expedientesingresoscontr_sq.NEXTVAL into vNewEicid from dual;
                  
                  DBMS_OUTPUT.PUT_LINE('nv16.14');    

                            insert into alba.expedientesingresoscontr ee
                                    (ee.eicid, ee.repid, ee.conid, ee.expid, ee.eicxtipocontribuyente, ee.eicporc, ee.eicobli, ee.eicnotif,
                                    ee.eicfhoramod, ee.usuidmod, ee.eicmotivo, ee.eicfvigencia, ee.eicxderechoprev, ee.eicxtipotitular)
                            select vNewEicid, e.repid, e.conid, vNewExpid, e.eicxtipocontribuyente,
                                   e.eicporc, e.eicobli, e.eicnotif, e.eicfhoramod, e.usuidmod,
                                   e.eicmotivo, e.eicfvigencia, e.eicxderechoprev, e.eicxtipotitular
                            from alba.expedientesingresoscontr e
                            where e.eicid=p.eicid;
                  
                  DBMS_OUTPUT.PUT_LINE('nv16.15');    

                            insert into alba.direccionesexpedientes
                            select alba.direccionesexpedientes_sq.NEXTVAL, a.rdiid, vNewEicid, a.fiaid, a.tedxtipodireccion,
                                   a.tedfhoramod, a.usuidmod, a.tedmotivo, a.tedfvigencia, a.tedorigen
                            from alba.direccionesexpedientes a
                            where a.eicid=p.eicid;
                  
                  DBMS_OUTPUT.PUT_LINE('nv16.16');    

                end loop;
                
                DBMS_OUTPUT.PUT_LINE('nv16.17');    
            
                if vPerid>0 then
            
                DBMS_OUTPUT.PUT_LINE('nv16.18');    
                
                    -- Nuevo conductor y dirección
                    select alba.contribuyentes_sq.NEXTVAL into vNewConid from dual;        
                
                DBMS_OUTPUT.PUT_LINE('nv16.19');    

                    insert into alba.contribuyentes c
                    (c.conid, c.perid, c.mexid, c.perversion, c.confhoramod, c.usuidmod, c.conmotivo, c.confvigencia)
                    values
                    (vNewConid, vPerid, 21, vPerversion, vSysdate, 0, 'Se ha creado', null);
                    
                    
                DBMS_OUTPUT.PUT_LINE('nv16.20');    
                
                    select alba.expedientesingresoscontr_sq.NEXTVAL into vNewEicid from dual;
                
                DBMS_OUTPUT.PUT_LINE('nv16.21');    

                    insert into alba.expedientesingresoscontr e
                    (e.eicid, e.repid, e.conid, e.expid, e.eicxtipocontribuyente, e.eicporc, e.eicobli, e.eicnotif,
                    e.eicfhoramod, e.usuidmod, e.eicmotivo, e.eicfvigencia, e.eicxderechoprev, e.eicxtipotitular)
                    values
                    (vNewEicid, null, vNewConid, vNewExpid, 327, null, 'S', null,
                    vSysdate, 0, 'Se ha creado', null, null, null);    

                
                DBMS_OUTPUT.PUT_LINE('nv16.22');    
                
                    select alba.direccionesexpedientes_sq.NEXTVAL into vNewTedid from dual;
                
                DBMS_OUTPUT.PUT_LINE('nv16.23');    

                    insert into alba.direccionesexpedientes d
                    (d.tedid, d.rdiid, d.eicid, d.fiaid, d.tedxtipodireccion,
                    d.tedfhoramod, d.usuidmod, d.tedmotivo, d.tedfvigencia, d.tedorigen)
                    values
                    (vNewTedid, vRdiid, vNewEicid, 5, 283,
                    vSysdate, 0, 'Se ha creado',null,null);        
                
                DBMS_OUTPUT.PUT_LINE('nv16.24');    
                    
                end if;
            
            DBMS_OUTPUT.PUT_LINE('nv16.25');                    

            return vMulidFinal;

            parresultado := 'OK';
            dbms_output.put_line(parresultado);

        else
        
            return 0;
        
        end if;
                
        ----------------------------------------------------------------
        -- FINALIZACION DEL PROCESO CON EXCEPCION
        ----------------------------------------------------------------
        
exception

    when others then

       parresultado := 'KO. '|| SQLERRM;
     dbms_output.put_line(parresultado);
     raise;

     return 0;
end;





--------------------------------------------------------------------------------
-- NUEVA VERSIÓN 1.4
--------------------------------------------------------------------------------
function nuevaVersion14(vMulid number, vPerid number, vPerversion number, vRdiid number, vFechaComCon date)
return number is

        -- Cursores
        -- Personas
        cursor cPer(cvNif varchar2, cvNombre varchar2, cvApe1 varchar2, cvApe2 varchar2) is
        select p.perid, p.perversion
        from alba.personas p
        where p.periden=upper(cvNif)
        and p.pernombre=upper(cvNombre)
        and p.perapell1=upper(cvApe1)
        and p.perapell2=upper(cvApe2)
        order by p.perid;


        cursor cPer2(vId number) is
        select e.eicid, e.repid, e.conid, e.expid, e.eicxtipocontribuyente,
        e.eicporc, e.eicobli, e.eicnotif, e.eicfhoramod, e.usuidmod,
        e.eicmotivo, e.eicfvigencia, e.eicxderechoprev, e.eicxtipotitular
        from alba.expedientesingresoscontr e
        where e.eicfvigencia is null
        and e.eicxtipocontribuyente<>327
        and e.expid=vId;
       
        -- Recupera Infracción Original tras 1.6
        cursor cMultaOriginal(vId number) is
        select m.mulid
        from alba.multas m
        where m.seoidexp=
        (
        select m2.seoidexp
        from alba.multas m2
        where m2.mulid=vId
        and m2.infid in (9002328,9002344)
        )
        and m.mulid=
        (
        select min(m3.mulid)
        from alba.multas m3
        where m3.seoidexp=m.seoidexp
        );       

        parresultado varchar2(400);

        vHoyDia date:=null;
        vSysdate date:=null;
        vNewExpid number:=0;
        vNewMulid number:=0;
        vExpid number:=0;
        vNewEicid number:=0;
        vNewVersion number:=0;
        vNewConid number:=0;
        vNewTedid number:=0;
        vMulidFinal number:=0;
       
       vMulid_origen number:=0;

begin

        DBMS_OUTPUT.PUT_LINE('nv16.01');
        
        vHoyDia:=to_date(sysdate,'dd/mm/yyyy');
        vSysdate:=sysdate;
    
        -- Guarda el expid
        select m.expidexp into vExpid from alba.multas m where m.mulid=vMulid;

        -- Nueva versión
        select max(m3.mulversion)+1 into vNewVersion from alba.multas m3 where m3.seoidexp=
        (select mm.seoidexp from alba.multas mm where mm.mulid=vMulid);

        -- Nuevo expingre
        select alba.expingre_sq.NEXTVAL into vNewExpid from dual;

        insert into alba.expingre
        (expid, expcod, expfalta, expfdecl, expfbaja,
         act_atuid, mexid, expfhoramod, usuidmod, expmotivo,
         expejercicio, expnumexpediente, expsufijo,expnumdocumento)        
        select vNewExpid, e.expcod, vHoyDia, vHoyDia, e.expfbaja,
               e.act_atuid, e.mexid, vSysdate, 0, e.expmotivo,
               e.expejercicio, e.expnumexpediente, e.expsufijo,
               e.expnumdocumento
        from alba.expingre e where e.expid=(select m.expidexp from alba.multas m where m.mulid=vMulid);


        -- Nuevo objeto
        insert into alba.seobjetos
    (seoid, expid, seofhoramod, usuidmod, seomotivo, seofvigencia)
        select s.seoid, vNewExpid, vSysdate, 0, 'Se ha creado', s.seofvigencia
        from alba.seobjetos s where s.expid=vExpid;

        update alba.expingre e
        set e.expfbaja=vHoyDia
        where e.expid=vExpid;


        -- Recupera Infracción Original tras 1.6
        vMulid_origen:=vMulid;
        for j in cMultaOriginal(vMulid) loop
        
            vMulid_origen:=j.mulid;
        
        end loop;

        -- Clonación de multa
        select alba.multas_sq.NEXTVAL into vNewMulid from dual;
        vMulidFinal:=vNewMulid;

        insert into alba.multas a
               (a.mulid, a.infid, a.mulsituinfra, a.multipobolet, a.mulsitutrami,
               a.mulnotestado, a.viaid, a.mullugar, a.mulfrente, a.vemid,
               a.muldelante, a.mulanobol, a.mulxboletin, a.mulnumbol,
               a.mulversion, a.seoidexp, a.expidexp, a.seoidveh, a.expidveh,
               a.tvtmid, a.mulfec, a.mulhor, a.mulmin, a.mulfirma, a.mulmarca,
               a.mulbasti, a.mulxcolor, a.mulatestado, a.mulatefecresol,
               a.mulxateorginformante, a.mulrec, a.mulimp, a.age1_ageid,
               a.age2_ageid, a.muldenuncia, a.mulnotmano, a.mulxmotivo,
               a.mulxgrua, a.mulxmedida1, a.mulxmedida2, a.mulxmedida3,
               a.mulxmedida4, a.mulresmedida1, a.mulresmedida2, a.mulresmedida3,
               a.mulresmedida4, a.mulobsdir, a.mulpermiso, a.mulfecper,
               a.mulclase, a.mulfecnac, a.mulexpperm, a.mulobservaciones,
               a.mulretirada, a.mulunidad1, a.mulpresencio1, a.mulunidad2,
               a.mulpresencio2, a.muldtodeposito, a.mulimpdeposito,
               a.mulfecgrab, a.mulfeciniprod, a.mulfecprescrip,
               a.mulfeccaducidad, a.mulfecrequer, a.mulfecfinplcomcon,
               a.mulfecnotden, a.mulfecsancion, a.mulfecsanfir, a.mulfecejec,
               a.mulfeccob, a.mulfecalegden, a.mulfecrecsan, a.mulfecanula,
               a.mulfecarch, a.mulfdesde, a.mulfhasta, a.mulmiembrofse,
               a.mulfoto, a.mulpruebas, a.mulfhoramod, a.usuidmod, a.mulmotivo,
               a.mulfvigencia, a.mulfecnotifcond, a.mulmatri, a.descnonotif)
        select vNewMulid, m.infid, m.mulsituinfra, m.multipobolet, m.mulsitutrami,
               m.mulnotestado, m.viaid, m.mullugar, m.mulfrente, m.vemid,
               m.muldelante, m.mulanobol, m.mulxboletin, m.mulnumbol,
               vNewVersion, m.seoidexp, vNewExpid, m.seoidveh, m.expidveh,
               m.tvtmid, m.mulfec, m.mulhor, m.mulmin, m.mulfirma, m.mulmarca,
               m.mulbasti, m.mulxcolor, m.mulatestado, m.mulatefecresol,
               m.mulxateorginformante, m.mulrec, m.mulimp, m.age1_ageid,
               m.age2_ageid, m.muldenuncia, m.mulnotmano, m.mulxmotivo,
               m.mulxgrua, m.mulxmedida1, m.mulxmedida2, m.mulxmedida3,
               m.mulxmedida4, m.mulresmedida1, m.mulresmedida2, m.mulresmedida3,
               m.mulresmedida4, m.mulobsdir, m.mulpermiso, m.mulfecper,
               m.mulclase, m.mulfecnac, m.mulexpperm, m.mulobservaciones,
               m.mulretirada, m.mulunidad1, m.mulpresencio1, m.mulunidad2,
               m.mulpresencio2, m.muldtodeposito, m.mulimpdeposito,
               m.mulfecgrab, m.mulfeciniprod, m.mulfecprescrip,
               m.mulfeccaducidad, m.mulfecrequer, m.mulfecfinplcomcon,
               null, null, null, null,
               m.mulfeccob, null, null, null,
               m.mulfecarch, m.mulfdesde, m.mulfhasta, m.mulmiembrofse,
               m.mulfoto, m.mulpruebas, vSysdate, 0, 'Se ha creado',
               m.mulfvigencia, vFechaComCon, m.mulmatri, m.descnonotif
        from alba.multas m where m.mulid=vMulid_origen;


        -- Clonación de personas y direcciones asociadas al antiguo expediente
        for p in cPer2(vExpid) loop

                    select alba.expedientesingresoscontr_sq.NEXTVAL into vNewEicid from dual;

                    insert into alba.expedientesingresoscontr ee
                            (ee.eicid, ee.repid, ee.conid, ee.expid, ee.eicxtipocontribuyente, ee.eicporc, ee.eicobli, ee.eicnotif,
                            ee.eicfhoramod, ee.usuidmod, ee.eicmotivo, ee.eicfvigencia, ee.eicxderechoprev, ee.eicxtipotitular)
                    select vNewEicid, e.repid, e.conid, vNewExpid, e.eicxtipocontribuyente,
                           e.eicporc, e.eicobli, e.eicnotif, e.eicfhoramod, e.usuidmod,
                           e.eicmotivo, e.eicfvigencia, e.eicxderechoprev, e.eicxtipotitular
                    from alba.expedientesingresoscontr e
                    where e.eicid=p.eicid;

                    insert into alba.direccionesexpedientes
                    select alba.direccionesexpedientes_sq.NEXTVAL, a.rdiid, vNewEicid, a.fiaid, a.tedxtipodireccion,
                           a.tedfhoramod, a.usuidmod, a.tedmotivo, a.tedfvigencia, a.tedorigen
                    from alba.direccionesexpedientes a
                    where a.eicid=p.eicid;

        end loop;

        -- Nuevo conductor y dirección
        select alba.contribuyentes_sq.NEXTVAL into vNewConid from dual;        

        insert into alba.contribuyentes c
        (c.conid, c.perid, c.mexid, c.perversion, c.confhoramod, c.usuidmod, c.conmotivo, c.confvigencia)
        values
        (vNewConid, vPerid, 21, vPerversion, vSysdate, 0, 'Se ha creado', null);
        
        
        select alba.expedientesingresoscontr_sq.NEXTVAL into vNewEicid from dual;

        insert into alba.expedientesingresoscontr e
        (e.eicid, e.repid, e.conid, e.expid, e.eicxtipocontribuyente, e.eicporc, e.eicobli, e.eicnotif,
        e.eicfhoramod, e.usuidmod, e.eicmotivo, e.eicfvigencia, e.eicxderechoprev, e.eicxtipotitular)
        values
        (vNewEicid, null, vNewConid, vNewExpid, 327, null, 'S', null,
        vSysdate, 0, 'Se ha creado', null, null, null);    


        select alba.direccionesexpedientes_sq.NEXTVAL into vNewTedid from dual;

        insert into alba.direccionesexpedientes d
        (d.tedid, d.rdiid, d.eicid, d.fiaid, d.tedxtipodireccion,
        d.tedfhoramod, d.usuidmod, d.tedmotivo, d.tedfvigencia, d.tedorigen)
        values
        (vNewTedid, vRdiid, vNewEicid, 5, 283,
        vSysdate, 0, 'Se ha creado',null,null);

        return vMulidFinal;

        parresultado := 'OK';
        dbms_output.put_line(parresultado);
        ----------------------------------------------------------------
        -- FINALIZACION DEL PROCESO CON EXCEPCION
        ----------------------------------------------------------------

exception

    when others then

       parresultado := 'KO. '|| SQLERRM;
       dbms_output.put_line(parresultado);
       raise;

       return 0;
end;



--------------------------------------------------------------------------------
-- PROCESA ESCRITO
--------------------------------------------------------------------------------
procedure procesaEscrito(    paresmid varchar2,parboletin varchar2,partipodocumento number,
                            parfechadocumento varchar2,parobservaciones varchar2, parusuario number,parresultado in out varchar2,
                            partipoestado number, parfechaestado varchar2,
                            pnif varchar2, pnombre varchar2, pape1 varchar2, pape2 varchar2, pperid varchar2, pperversion varchar2,
                            pdireccion varchar2, plocalidad varchar2, pprovincia varchar2, pcp varchar2, prdiid varchar2, ptipopersona number) is


-- Liquidaciones
cursor cLiq(vId Number) is
select /*RULE*/ distinct l.liqid
from alba.liquidaciones l, alba.expingre e, alba.seobjetos o
where l.expid=e.expid
and e.expid=o.expid
and e.mexid=21
and l.liqxestado in (5,6)
and o.seoid=vId;

-- Liquidaciones
cursor cLiq2(vNumBolLiq Number) is
select l.liqid
from alba.liquidaciones l 
where l.LIQNUMERORECLIQUIDACION=to_char(vNumBolLiq);


-- Variables
vEsmid alba.escritosmultas.esmid%type;
vNumbol alba.escritosmultas.esmnumbol%type;
vTipo alba.escritosmultas.tidomid%type;
vTipoEstado alba.escritosmultas.tdmresid%type;
vFechaGrabacion alba.escritosmultas.esmfgrabacion%type;
vFechaEstado alba.escritosmultas.esmfestado%type;
vFechaEscrito alba.escritosmultas.esmfescrito%type;
vObs alba.escritosmultas.esmobservaciones%type;
vUsuid alba.escritosmultas.usuidmod%type;
vResultado varchar2(1000);
vMulid alba.multas.mulid%type;
vSeoid alba.multas.seoidexp%type;
vExpid alba.multas.expidexp%type;
vAlgid alba.alegacionesrecursos.algid%type;
vAlgtipo alba.alegacionesrecursos.algtipo%type;
vEscrito number;
vPuedeModificar number;
vFechaActual date;
vNumEscrito number;
--
vPerid alba.personas.perid%type;
vPerversion alba.personas.perversion%type;
vRdiid alba.direccion.rdiid%type;
vTipoPersona alba.tipospersona.tpeid%type;
--
vNif  varchar2(200):='';
vNombre varchar2(200):='';
vApe1 varchar2(200):='';
vApe2 varchar2(200):='';
vDireccion varchar2(200):='';
vLocalidad varchar2(200):='';
vProvincia varchar2(200):='';
vCP varchar2(200):='';
vRazonSocial varchar2(200):='';
vMulidFinal number:=0;
vMensaje varchar2(1000):='';
vExpediente varchar2(200):='';
vDepuracion varchar2(200):='';
--
vMulidOriginal alba.multas.mulid%type;
vEjecutiva number:=0;
vFirmaEjecutiva date:=null;

vLiqidRec number:=0;


begin


    if PARESMID is null then
      select alba.escritosmultas_sq.NEXTVAL into vEsmid from dual;
    else
      vEsmid:=to_number(PARESMID);
    end if;
    
    select count(e.esmid) into vPuedeModificar from alba.escritosmultas e where e.esmid=vEsmid;-- and e.mulid is null;
    vNumbol:=to_number(PARBOLETIN);
    vTipo:=PARTIPODOCUMENTO;
    vTipoEstado:=PARTIPOESTADO;
    vFechaGrabacion:=to_date(sysdate);
    vFechaEscrito:=to_date(PARFECHADOCUMENTO,'dd/mm/rrrr');
    vFechaEstado:=to_date(PARFECHAESTADO,'dd/mm/rrrr');
    vFechaActual:=sysdate;
    vObs:=PAROBSERVACIONES;
    vUsuid:=PARUSUARIO;
    vResultado:=PARRESULTADO;
    vMulid:=0;

    -- Inicializa Parámetros
    vPerid:=PPERID;
    vPerversion:=PPERVERSION;
    vRdiid:=PRDIID;
    vTipoPersona:=PTIPOPERSONA;
    --
    vNif:=substr(PNIF,1,9);
    vNombre:=substr(PNOMBRE,1,100);
    vApe1:=substr(PAPE1,1,25);
    vApe2:=substr(PAPE2,1,25);
    vDireccion:=substr(PDIRECCION,1,200);
    vLocalidad:=substr(PLOCALIDAD,1,200);
    vProvincia:=substr(PPROVINCIA,1,200);
    vCP:=substr(PCP,1,200);
    
    --

    if PARESMID is not null and vPuedeModificar=0 then
        PARRESULTADO := 'El escrito no puede ser modificado, ya se procesó.';
    else
        -- Eliminar Escrito
        if vResultado='E' then
              PARRESULTADO := '01';
              delete from alba.escritosmultas e where e.esmid=vEsmid;-- and e.mulid is null;
              PARRESULTADO := 'El escrito se eliminó.';                    

        -- Alta/Modificación Escrito
        else
              select count(m.mulid) into vMulid from alba.multas m where m.mulnumbol=vNumbol;
              
              --Asocia Liquidación
              vLiqidRec:=null;
              for l2 in cLiq2(vNumbol) loop
                vLiqidRec:=l2.liqid;
              end loop;

              -- Alta Escrito Sin Expediente
              if vMulid=0 then
                              if PARESMID is null then
                                    PARRESULTADO := '02';
                                    insert into alba.escritosmultas e
                                    (e.esmid, e.esmnumbol, e.esmxtipo, e.esmfgrabacion, e.esmfescrito,
                                    e.esmobservaciones, e.esmfhoramod, e.usuidmod, e.esmmotivo,
                                    e.mulid, e.tidomid, e.tdmresid, e.esmfestado, e.rdiid, e.perid, e.perversion, e.esmcaja, e.esmlote, e.esmindice, e.liqid)
                                    values
                                    (vEsmid, vNumbol, 0, vFechaGrabacion, vFechaEscrito,
                                    vObs, vFechaActual, vUsuid, 'Se ha creado',
                                    null, vTipo, vTipoEstado, vFechaEstado, vRdiid, vPerid, vPerversion, vEsmid, vEsmid, vEsmid,vLiqidRec);
                              else
                                    PARRESULTADO := '03';
                                    update alba.escritosmultas e
                                    set e.esmnumbol=vNumbol,
                                    e.esmxtipo=0,
                                    e.esmobservaciones=vObs,
                                    e.esmfescrito=vFechaEscrito,
                                    e.esmfhoramod=vFechaActual,
                                    --e.esmfgrabacion=vFechaGrabacion,
                                    e.usuidmod=vUsuid,
                                    e.esmmotivo='Se ha modificado',
                                    e.tidomid=vTipo,
                                    e.tdmresid=vTipoEstado,
                                    e.esmfestado=vFechaEstado,
                                    e.rdiid=vRdiid,
                                    e.perid=vPerid,
                                    e.perversion=vPerversion,
                                    e.mulid=null,
                                    e.liqid=vLiqidRec
                                    where e.esmid=vEsmid;
                                    --and e.mulid is null;
                              end if;    
                              PARRESULTADO := 'El escrito se procesó correctamente.';

              -- Escrito Con Expediente
              else
          
                              select m.mulid, m.seoidexp, m.expidexp into vMulid, vSeoid, vExpid
                              from alba.multas m where m.mulnumbol=vNumbol
                              and m.mulid=(select max(m2.mulid) from alba.multas m2 where m2.mulnumbol=vNumbol)
                              and m.mulversion=(select max(mm.mulversion)
                              from alba.multas mm where mm.seoidexp=m.seoidexp);
                              
                              
                              -- Localiza Fecha Firma Libro Ejecutiva
                              select max(l.lbrfirma) into vFirmaEjecutiva
                              from alba.det_libro_resol d, alba.libro_resoluciones l
                              where d.lbrid=l.lbrid
                              and l.lrtid=101
                              and d.mulid in
                              (select m.mulid from alba.multas m where m.seoidexp=vSeoid);


                              vDepuracion:=vDepuracion||'.1001. '||to_char(vFechaEscrito,'dd/mm/rrrr')||'. '||to_char(vFirmaEjecutiva,'dd/mm/rrrr');

                              -- Determina si el escrito se presentó antes de firma libro de ejecutiva
                              if vFirmaEjecutiva is not null and vFechaEscrito>vFirmaEjecutiva and vTipo not in (41,61,62,82,124,101,141,242,261) then

                                  vEjecutiva:=1;
                                  
                              elsif vTipo=82 then

                                  vEjecutiva:=0;
                                  
                          vDepuracion:=vDepuracion||'.1011. ';

                              else

                                  vEjecutiva:=0;


                          vDepuracion:=vDepuracion||'.1012. ';

                              end if;

                              
                              if vEjecutiva=1 and vTipo<>3 then --
                                      PARRESULTADO := 'IMPOSIBLE DAR DE ALTA EL ESCRITO. Expediente en ejecutiva';                                                


                          vDepuracion:=vDepuracion||'.1010';
                          
                              else


                          vDepuracion:=vDepuracion||'.1002';
                          
                                      -- Alta Escrito Con Expediente
                                      if PARESMID is null then

                                          /*if vTipo not in (41,61,62,124,101) then

                                              if vTipo=82 then --Si Requerimiento Juzgado
                                                  -- 'SUSPENDE CAUTELARMENTE' Liquidaciones 'NOTIFICADAS' O 'EN EJECUTIVA'
                                                  for liq in cLiq(vSeoid) loop


                          vDepuracion:=vDepuracion||'.1003';
                                  
                                
                                          alba.utl_alba.penvia_mail('procesarEscrito','jgomez.agenrec@sevilla.org','Liquidación Suspendida cautelarmente. Req.juzgado',to_char(liq.liqid),'','','');

                                                      update alba.liquidaciones
                                                      set alba.liquidaciones.liqxestado=200,
                                                      alba.liquidaciones.liqfhoramod=sysdate,
                                                      alba.liquidaciones.liqmotivo='Suspendido cautelarmente. Req.juzgado',
                                                      alba.liquidaciones.usuidmod=vUsuid
                                                      where alba.liquidaciones.liqid=liq.liqid
                                                      and alba.liquidaciones.liqxestado in (5,6);

                                                  end loop;

                                              else
                                                  -- 'SUSPENDE' Liquidaciones 'NOTIFICADAS' O 'EN EJECUTIVA'
                                                  for liq in cLiq(vSeoid) loop


                          vDepuracion:=vDepuracion||'.1003';
                                  
                                                      update alba.liquidaciones
                                                      set alba.liquidaciones.liqxestado=170,
                                                      alba.liquidaciones.liqfhoramod=sysdate,
                                                      alba.liquidaciones.liqmotivo='Suspendido. Escrito anterior a ejecutiva',
                                                      alba.liquidaciones.usuidmod=vUsuid
                                                      where alba.liquidaciones.liqid=liq.liqid
                                                      and alba.liquidaciones.liqxestado in (5,6);

                                                  end loop;
                                              end if;

                                          end if;*/


                                          PARRESULTADO := '04';
                                          insert into alba.escritosmultas e
                                          (e.esmid, e.esmnumbol, e.esmxtipo, e.esmfgrabacion, e.esmfescrito,
                                          e.esmobservaciones, e.esmfhoramod, e.usuidmod, e.mulid, e.esmmotivo,
                                          e.tidomid, e.tdmresid, e.esmfestado, e.rdiid, e.perid, e.perversion, e.esmcaja, e.esmlote, e.esmindice)
                                          values
                                          (vEsmid, vNumbol, 0, vFechaGrabacion, vFechaEscrito,
                                          vObs, vFechaActual, vUsuid, vMulid, 'Se ha creado',
                                          vTipo,vTipoEstado,vFechaEstado, vRdiid, vPerid, vPerversion,vEsmid,vEsmid,vEsmid);

                          vDepuracion:=vDepuracion||'.1004';
                          
                                      -- Modificación Escrito Con Expediente
                                      else
                                          PARRESULTADO := '05';
                                          update alba.escritosmultas e
                                          set e.esmnumbol=vNumbol,
                                          e.esmxtipo=0,
                                          e.esmobservaciones=vObs,
                                          e.esmfescrito=vFechaEscrito,
                                          e.esmfhoramod=vFechaActual,
                                          --e.esmfgrabacion=vFechaGrabacion,
                                          e.usuidmod=vUsuid,
                                          e.esmmotivo='Se ha modificado',
                                          e.mulid=vMulid,
                                          e.tidomid=vTipo,
                                          e.tdmresid=vTipoEstado,
                                          e.esmfestado=vFechaEstado,
                                          e.rdiid=vRdiid,
                                          e.perid=vPerid,
                                          e.liqid=null,
                                          e.perversion=vPerversion
                                          where e.esmid=vEsmid;
                                          --and e.mulid is null;
                                      end if;    
                                      --
                                      PARRESULTADO := 'El boletín indicado tiene asignado expediente. El escrito se procesó correctamente';
                              end if;

              end if;
        end if;    
    end if;
    
    ----------------------------------------------------------------
    -- FINALIZACION DEL PROCESO
    ----------------------------------------------------------------
    --DBMS_OUTPUT.PUT_LINE(PARRESULTADO);
    --COMMIT;


    vDepuracion:=vDepuracion||'.1005';

    if vPerid>0 and vRdiid>0 and vTipo=21 and vTipoEstado=42 then

            vDepuracion:=vDepuracion||'.1006';

            select multas.mulxmedida4 into vMulidOriginal from alba.multas where multas.mulid=vMulid;

            if vMulidOriginal is null then
                vMulidFinal:=trml.nuevaVersion14(vMulid,vPerid,vPerversion,vRdiid,vFechaEscrito);
            else
                vMulidFinal:=trml.nuevaVersion14(vMulidOriginal,vPerid,vPerversion,vRdiid,vFechaEscrito);

            end if;

    elsif vPerid>0 and vRdiid>0 and vTipo=21 and vTipoEstado=121 then

            select multas.mulxmedida4 into vMulidOriginal from alba.multas where multas.mulid=vMulid;

            if vMulidOriginal is null then
                vMulidFinal:=trml.nuevaVersion16(vMulid,vPerid,vPerversion,vRdiid,vFechaEscrito);
            else
                vMulidFinal:=trml.nuevaVersion16(vMulidOriginal,vPerid,vPerversion,vRdiid,vFechaEscrito);

            end if;

    else
            vMulidFinal:=vMulid;

            vDepuracion:=vDepuracion||'.1009';                
    end if;

    if vMulid is not null and vMulid>0 then

            vDepuracion:=vDepuracion||'.1007. Mulid: ' ||to_char(vMulid)||'. MulidFinal: ' ||to_char(vMulidFinal);
            select (chr(13) || 'Expediente: ' || e.expediente || chr(13) || 'Estado: ' || e.cestado || '. ' || e.estado) into vMensaje from table(trml.emultas(vMulidFinal)) e;
            --vMensaje:='';                
            PARRESULTADO := PARRESULTADO || vMensaje;
            
            vDepuracion:=vDepuracion||'.1008';                
    end if;

    --PARRESULTADO := PARRESULTADO || chr(13) || vDepuracion;

    DBMS_OUTPUT.PUT_LINE(PARRESULTADO);
    COMMIT;
   alba.paq_multas.ComprobarAsociacion(paresmId);

    ----------------------------------------------------------------
    -- FINALIZACION DEL PROCESO CON EXCEPCION
    ----------------------------------------------------------------
EXCEPTION
WHEN OTHERS THEN
       PARRESULTADO := 'El proceso no finalizó correctamente. ' || sqlerrm || chr(13) || vDepuracion;
       DBMS_OUTPUT.PUT_LINE(PARRESULTADO);
       ROLLBACK;
      alba.paq_multas.ComprobarAsociacion(paresmId);
end;


--------------------------------------------------------------------------------
-- Generación de Libro de Multas
--------------------------------------------------------------------------------
procedure generaLibroMultas (
  in_lestado      in  number,
  in_usuidmod     in  number,
  in_sysdate      in  date,
  in_motivo       in  varchar2,
  out_procesados  out number,
  out_salida      out varchar2,
    in_fdesde                in  varchar2,
    in_fhasta                in  varchar2,
  in_tipoboletin  in  number,
  in_tipodocumento in number,
  in_estadodocumento in number
  ) is

    
    --
    cursor cExpOpt(vFdesde varchar2, vFhasta varchar2, vEstado number) is
    select /*RULE*/ e.cestado, e.mulid, e.expid, d.peridD, d.perversionD, d.rdiidD, e.alegesmid
    from alba.multasestados m,
         table(trml.emultas(m.mulid)) e,
         table(trml.dmultas(m.mulid,0)) d
    where m.mulid=e.mulid
    and m.mulid=d.mulid
    --and m.mulfecgrab<=TO_DATE(vFHasta || ' 23:59:59','dd/mm/rrrr HH24:MI:SS')
    and m.cestado=e.cestado
    and decode((substr(substr(vestado,-4),1,2)+0),64,1001,65,1001,66,1001,56,1001,57,1001,70,1001, --Tipos de Libros que no se desdoblan
    decode(m.origendenuncia,0,1000,1,1001,2,1001,3,1005,4,1004,5,1005,6,1006,7,1007,8,1008,9,1009,10,1010,1000))=substr(vestado,1,4)+0
    and m.cestado=substr(vestado,-4)+0;    

    
    -- Libros BOP
    cursor cExpPresOpt(vFdesde varchar2, vFhasta varchar2, vEstado number) is
    select /*RULE*/ e.cestado, e.mulid, e.expid, d.peridD, d.perversionD, d.rdiidD, e.alegesmid
    from alba.multasestados m,
         table(trml.emultas(m.mulid)) e,
         table(trml.dmultas(m.mulid,0)) d
    where m.mulid=e.mulid
    and m.mulid=d.mulid
    --and m.fprescripcion>=TO_DATE(vFdesde || ' 00:00:00','dd/mm/rrrr HH24:MI:SS')
    --and m.fprescripcion<=TO_DATE(vFHasta || ' 23:59:59','dd/mm/rrrr HH24:MI:SS')
    and m.cestado=e.cestado
    --and e.origendenuncia<>5
    and decode((substr(substr(vestado,-4),1,2)+0),64,1001,65,1001,66,1001,56,1001,57,1001,70,1001, --Tipos de Libros que no se desdoblan
    decode(m.origendenuncia,0,1000,1,1001,2,1001,3,1005,4,1004,5,1005,6,1006,7,1007,8,1008,9,1009,10,1010,1000))=substr(vestado,1,4)+0
    and m.cestado=substr(vEstado,-4)+0
    and rownum<3001;
    
    --Libro RPA estimado con ingresos
    cursor cLrpaci(vEstado number) is
    select /*RULE*/ e.cestado, e.mulid, e.expid, d.peridD, d.perversionD, d.rdiidD, e.alegesmid
    from alba.liquidaciones l,
         alba.multasestados m,
         table(trml.emultas(m.mulid)) e,
         table(trml.dmultas(m.mulid,0)) d
    where m.mulid=e.mulid
    and m.mulid=d.mulid
    --and m.mulfecgrab<=TO_DATE(vFHasta || ' 23:59:59','dd/mm/rrrr HH24:MI:SS')
    and m.cestado=e.cestado
    and m.liqid=l.liqid(+)
    and decode((substr(substr(vEstado,-4),1,2)+0),64,1001,65,1001,66,1001,56,1001,57,1001,70,1001, --Tipos de Libros que no se desdoblan
    decode(m.origendenuncia,0,1000,1,1001,2,1001,3,1005,4,1004,5,1005,6,1006,7,8,1008,9,1009,10,1010,1007,1000))=substr(vEstado,1,4)+0
    and m.cestado=substr(vEstado,-4)+0
    and (select dacuenta from table(pkg_calculoimportes.ctd(l.liqid,sysdate)))>0;
    
    --Libro RPA estimado sin ingresos
    cursor cLrpasi(vEstado number) is
    select /*RULE*/ e.cestado, e.mulid, e.expid, d.peridD, d.perversionD, d.rdiidD, e.alegesmid
    from alba.liquidaciones l,
         alba.multasestados m,
         table(trml.emultas(m.mulid)) e,
         table(trml.dmultas(m.mulid,0)) d
    where m.mulid=e.mulid
    and m.mulid=d.mulid
    --and m.mulfecgrab<=TO_DATE(vFHasta || ' 23:59:59','dd/mm/rrrr HH24:MI:SS')
    and m.cestado=e.cestado
    and m.liqid=l.liqid(+)
    and decode((substr(substr(vEstado,-4),1,2)+0),64,1001,65,1001,66,1001,56,1001,57,1001,70,1001, --Tipos de Libros que no se desdoblan
    decode(m.origendenuncia,0,1000,1,1001,2,1001,3,1005,4,1004,5,1005,6,1006,7,8,1008,9,1009,10,1010,1007,1000))=substr(vEstado,1,4)+0
    and m.cestado=substr(vEstado,-4)+0
    and (select dacuenta from table(pkg_calculoimportes.ctd(l.liqid,sysdate)))=0;    


    --Libro RPA estimado sin ingresos
    cursor cLrpasl(vEstado number) is
    select /*RULE*/ e.cestado, e.mulid, e.expid, d.peridD, d.perversionD, d.rdiidD, e.alegesmid
    from alba.liquidaciones l,
         alba.multasestados m,
         table(trml.emultas(m.mulid)) e,
         table(trml.dmultas(m.mulid,0)) d
    where m.mulid=e.mulid
    and m.mulid=d.mulid
    --and m.mulfecgrab<=TO_DATE(vFHasta || ' 23:59:59','dd/mm/rrrr HH24:MI:SS')
    and m.cestado=e.cestado
    and m.liqid=l.liqid(+)
    and l.liqid is null
    and decode((substr(substr(vEstado,-4),1,2)+0),64,1001,65,1001,66,1001,56,1001,57,1001,70,1001, --Tipos de Libros que no se desdoblan
    decode(m.origendenuncia,0,1000,1,1001,2,1001,3,1005,4,1004,5,1005,6,1006,7,8,1008,9,1009,10,1010,1007,1000))=substr(vEstado,1,4)+0
    and m.cestado=substr(vEstado,-4)+0;  
    
    cursor cTea is
    select /*RULE*/ ee.cestado, e.mulid, m.expidexp expid, d.peridD, d.perversionD, d.rdiidD, e.esmid alegesmid
    from alba.multas m,
    alba.escritosmultas e,
    table(depuracion.trml.eescrito(e.esmid)) ee,
    table(trml.dmultas(e.mulid,e.esmid)) d
    where e.esmid=ee.esmid
    and e.mulid=m.mulid
    and e.tidomid=101
    and e.tdmresid in (261,281,385,401,421,521)
    and ee.cestado=7000
    and ee.pleno=in_fhasta;    
   

  
    cursor cTca(vestado number) is
    select /*RULE*/ ee.cestado, e.mulid, m.expidexp expid, d.peridD, d.perversionD, d.rdiidD, e.esmid alegesmid, e.liqid
    from alba.multas m,
    alba.escritosmultas e,
    table(depuracion.trml.eescrito_tca(e.esmid)) ee,
    table(trml.dmultas(e.mulid,e.esmid)) d
    where e.esmid=ee.esmid
    and e.mulid=m.mulid
    and e.tidomid=242
    and e.tdmresid in (641,642,682)
    and ee.cestado=vEstado
    union
    select /*RULE*/ ee.cestado, e.mulid, l.expid, e.perid, e.perversion, e.rdiid, e.esmid alegesmid, e.liqid
    from alba.liquidaciones l,
    alba.escritosmultas e,
    table(depuracion.trml.eescrito_tca(e.esmid)) ee
    where e.esmid=ee.esmid
    and e.liqid=l.LIQID
    and e.tidomid=242
    and e.tdmresid in (641,642,682)
    and ee.cestado=vEstado;  
    
    cursor cresoejetea(vestado number) is
    select /*RULE*/ ee.cestadoliq cestado, ee.estadoliq, e.mulid, e.esmnumbol, m.expidexp expid,
    d.peridd, d.perversiond, d.rdiidd, e.esmid alegesmid, alba.escritosmultas_sq.nextval newesmid,
    e.rdiid newRdiid
    from alba.multas m,
    alba.escritosmultas e,
    table(depuracion.trml.eescrito(e.esmid)) ee,
    table(trml.dmultas(e.mulid,e.esmid)) d
    where e.esmid=ee.esmid
    and e.mulid=m.mulid
    and e.tidomid=101
    and e.tdmresid=261
    and ee.cestadoliq=vEstado;
  
    cursor cAnotacionPuntos is
    select /*RULE*/ e.cestado, e.mulid, e.expid, d.peridd, d.perversiond, d.rdiidd, e.alegesmid
    from alba.multasestados m,
         table(trml.emultas(m.mulid)) e,
         table(trml.dmultas(m.mulid,0)) d
    where m.mulid=e.mulid
    and m.mulid=d.mulid
    and m.xml=1
    and rownum<2501
    and e.xml=1
    order by m.mulid asc;
    
    cursor cResoEjeTeaManual(vestado number) is
    select /*RULE*/ ee.cestado, e.mulid, m.expidexp expid, d.peridD, d.perversionD, d.rdiidD, e.esmid alegesmid, e.liqid
    from alba.multas m,
    alba.escritosmultas e,
    table(depuracion.trml.eescrito_tea(e.esmid)) ee,
    table(trml.dmultas(e.mulid,e.esmid)) d
    where e.esmid=ee.esmid
    and e.mulid=m.mulid
    and e.tidomid=141
    and e.tdmresid=481
    and ee.cestado=vEstado
    union
    select /*RULE*/ ee.cestado, e.mulid, l.expid, e.perid, e.perversion, e.rdiid, e.esmid alegesmid, e.liqid
    from alba.liquidaciones l,
    alba.escritosmultas e,
    table(depuracion.trml.eescrito_tea(e.esmid)) ee
    where e.esmid=ee.esmid
    and e.liqid=l.LIQID
    and e.tidomid=141
    and e.tdmresid=481
    and ee.cestado=vEstado
    ;

    vLestado number:=0;
    vUsuidmod number:=0;
    vSysdate date:=null;
    vMotivo varchar2(200):='';
    vProcesados number:=0;
    vSalida varchar2(200):='';
    vPntid number:=0;
    vDesde date;
    vHasta date;
    vLbrid number;
    vLbrnumero number;
    vTipoLibro number;
    vDlbid number;
    vMulid number;  
    vtextolibro varchar2(300):='';
    newEicid number:=0;
    
    vInformeTex clob;

begin
    -- Inicialización de parámetros
    vLestado:=in_lestado;
    vUsuidmod:=in_usuidmod;
    vSysdate:=to_date(sysdate);
    vMotivo:=in_motivo;
    vProcesados:=0;
    vSalida:='';
    vDesde:=to_date(in_fdesde,'dd/mm/rrrr');
    vHasta:=to_date(in_fhasta,'dd/mm/rrrr');
    vTextoLibro:='De ' || in_fdesde || ' a '|| in_fhasta;
--
    If vLestado>0 and vDesde<=vHasta then
        
        -- Id libro
        select alba.libro_resoluciones_sq.nextval into vLbrid from dual;

        -- Número de Libro Temporal
        select '3000' || lpad(librosmultas_sq.NEXTVAL,4,0)+0 into vLbrnumero from dual;

        -- Tipo de Libro
        select t.lrtid into vTipoLibro
        from alba.libro_resol_tipos t where t.lrtestado=vLestado and rownum<2;

        -- Inserta Cabecera
        insert into alba.libro_resoluciones l
        (l.lbrid, l.lbrfecha, l.lbrnumero, l.lbrfirma, l.lbrfhoramod,
        l.usuidmod, l.lbrmotivo, l.lbrfvigencia, l.lbrnumlibro,
        l.lbrtipolibro, l.lrtid, l.lbrfpropnot, l.lbrdescripcion)
        values
        (vLbrid, to_date(sysdate,'dd/mm/rrrr'), vLbrnumero, null, sysdate,
        vUsuidmod, 'Se ha creado', null, null,
        null, vTipoLibro, null, 'De ' || in_fdesde || ' a '|| in_fhasta);
        
        --
        if vTipoLibro=342 then --TEA
       
            for e in cTea loop
      
                -- Id detalle libro
                select alba.det_libro_resol_sq.nextval into vDlbid from dual;
                
                -- Inserta Detalle
                insert into alba.det_libro_resol d
                (d.dlbid, d.expid, d.lbrid, d.dlbfhoramod, d.usuidmod, d.dlbmotivo,
                d.dlbfvigencia, d.mulid, d.perid, d.perversion, d.rdiid, d.esmid)
                values
                (vDlbid, e.expid, vLbrid, sysdate, vUsuidmod, 'Se ha creado',
                null, e.mulid, e.peridD, e.perversionD, e.rdiidD, e.alegesmid);                    
                
                vProcesados:=vProcesados+1;
      
            end loop;
            
        elsif vtipolibro in (1421,1422,1423,1424) then --TCA
        
            for e in cTca(vLestado) loop
      
                -- Id detalle libro
                select alba.det_libro_resol_sq.nextval into vDlbid from dual;
                
                -- Inserta Detalle
                insert into alba.det_libro_resol d
                (d.dlbid, d.expid, d.lbrid, d.dlbfhoramod, d.usuidmod, d.dlbmotivo,
                d.dlbfvigencia, d.mulid, d.perid, d.perversion, d.rdiid, d.esmid, d.liqid)
                values
                (vDlbid, e.expid, vLbrid, sysdate, vUsuidmod, 'Se ha creado',
                null, e.mulid, e.peridD, e.perversionD, e.rdiidD, e.alegesmid, e.liqid);                    
                
                vProcesados:=vProcesados+1;
      
            end loop;        
            
        elsif vtipolibro in (1081) then
        
            for e in cAnotacionPuntos loop
      
                -- Id detalle libro
                select alba.det_libro_resol_sq.nextval into vDlbid from dual;
                
                -- Inserta Detalle
                insert into alba.det_libro_resol d
                (d.dlbid, d.expid, d.lbrid, d.dlbfhoramod, d.usuidmod, d.dlbmotivo,
                d.dlbfvigencia, d.mulid, d.perid, d.perversion, d.rdiid, d.esmid)
                values
                (vDlbid, e.expid, vLbrid, sysdate, vUsuidmod, 'Se ha creado',
                null, e.mulid, e.peridD, e.perversionD, e.rdiidD, e.alegesmid);                    
                
                vProcesados:=vProcesados+1;

            end loop;        
        
                
            
        elsif vTipoLibro in (1021,1061) then --Resolución Ejecución TEA
        
                -- Manuales
                for e in cResoEjeTeaManual(vLestado) loop
                  -- Id detalle libro
                  select alba.det_libro_resol_sq.nextval into vDlbid from dual;
                  
                  -- Inserta Detalle
                  insert into alba.det_libro_resol d
                  (d.dlbid, d.expid, d.lbrid, d.dlbfhoramod, d.usuidmod, d.dlbmotivo,
                  d.dlbfvigencia, d.mulid, d.perid, d.perversion, d.rdiid, d.esmid, d.liqid)
                  values
                  (vDlbid, e.expid, vLbrid, sysdate, vUsuidmod, 'Se ha creado',
                  null, e.mulid, e.peridD, e.perversionD, e.rdiidD, e.alegesmid, e.liqid);                    
                  
                  vProcesados:=vProcesados+1;                
                end loop;
                
                
                -- Automáticos
                for e in cResoEjeTea(vLestado) loop
                
                    -- Inserta escrito
                    INSERT INTO alba.escritosmultas e2
                    (
                    e2.esmid,e2.esmnumbol,e2.esmxtipo,e2.esmfgrabacion,e2.esmfescrito,
                    e2.esmobservaciones,e2.esmfhoramod,e2.usuidmod,e2.mulid,e2.esmmotivo,
                    e2.tidomid,e2.tdmresid,e2.esmfestado,e2.perid,e2.perversion,e2.rdiid,
                    e2.eregid,e2.eficfid,e2.esmcaja,e2.esmlote,e2.esmindice,e2.esmbloqueo
                    )
                    VALUES
                    (
                    e.newEsmid,e.esmnumbol,0,trunc(SYSDATE),trunc(SYSDATE),
                    NULL,trunc(SYSDATE),0,e.mulid,'Se ha creado',
                    141,481,trunc(SYSDATE),0,0,e.newRdiid,
                    NULL,NULL,NULL,NULL,NULL,NULL
                    );
                              
                    -- Inserta resoluciones
                    insert into alba.resol_alegaciones
                    (RSAID,ALGID,RESID,RSAFHORAMOD,USUIDMOD,RSAMOTIVO,RSAFVIGENCIA,RSAMOTIVORESOL,ESMID,RSARESOLUCION)
                    select 
                    alba.resol_alegaciones_sq.nextval rsaid,
                    0 algid,
                    l.resid resid,
                    trunc(sysdate) rsafhoramod,
                    0 usuidmod,
                    'Se ha creado' rsamotivo,
                    null rsafvigencia,
                    resdesccompleta rsamotivoresol,
                    e.newEsmid esmid,
                    resresolucion rsaresolucion
                    FROM alba.resoluciones l
                    where l.tidomid=141
                    and l.tdmresid=481
                    and instr(e.estadoliq,to_char(l.resid))>0
                    ; 
                    
                    -- Replica Información Escrito
                    insert into alba.escritosmultasinfo
                    (ESIID,ESMID,TIDOMID,ESMIETIQUETA,ESMIVALOR,ESMIFHORAMOD,USUIDMOD,ESMIMOTIVO,ESMIIESTADO,PERID,PERVERSION,RDIID)
                    select
                    alba.escritosmultasinfo_sq.nextval ESIID,
                    e.newEsmid esmid,
                    141 tidomid,
                    esmietiqueta,
                    ESMIVALOR,
                    esmifhoramod,
                    USUIDMOD,
                    esmimotivo,
                    esmiiestado,
                    PERID,
                    perversion,
                    RDIID          
                    from alba.escritosmultasinfo emi
                    where emi.esmid=e.alegesmid
                    ;
                    
                    insert into alba.escritosmultasinfo
                    (ESIID,ESMID,TIDOMID,ESMIETIQUETA,ESMIVALOR,ESMIFHORAMOD,USUIDMOD,ESMIMOTIVO,ESMIIESTADO,PERID,PERVERSION,RDIID)
                    select
                    alba.escritosmultasinfo_sq.nextval ESIID,
                    e.newesmid esmid,
                    141 tidomid,
                    'esmidorg' esmietiqueta,
                    to_char(e.alegesmid) esmivalor,
                    sysdate esmifhoramod,
                    0 usuidmod,
                    'esmid' esmimotivo,
                    1 esmiiestado,
                    null perid,
                    null perversion,
                    null rdiid          
                    from dual
                    ;
                    
                    -- replica dirección
                    select alba.expedientesingresoscontr_sq.nextval
                    into neweicid
                    from dual;
                    
                    insert into alba.expedientesingresoscontr
                    (EICID,REPID,CONID,EXPID,EICXTIPOCONTRIBUYENTE,EICPORC,EICOBLI,EICNOTIF,EICFHORAMOD,USUIDMOD,
                    EICMOTIVO,EICFVIGENCIA,EICXDERECHOPREV,EICXTIPOTITULAR,EICNIF,EICIIVTNU,EICNTITULAR,EICNOBLIG,ESMID)
                    select
                    neweicid eicid,
                    eic.repid,
                    eic.conid,
                    eic.expid,
                    eic.eicxtipocontribuyente,
                    eic.eicporc,
                    eic.eicobli,
                    eic.eicnotif,
                    sysdate eicfhoramod,
                    0 usuidmod,
                    eic.eicmotivo,
                    eic.eicfvigencia,
                    eic.eicxderechoprev,
                    eic.eicxtipotitular,
                    eic.eicnif,
                    eic.eiciivtnu,
                    eic.eicntitular,
                    eic.eicnoblig,
                    e.newEsmid         
                    from alba.expedientesingresoscontr eic
                    where eic.esmid=e.alegesmid;
                    
                    insert into alba.direccionesexpedientes
                    select
                    alba.direccionesexpedientes_sq.nextval tedid,
                    dre.rdiid,
                    neweicid eicid,
                    dre.fiaid,
                    dre.tedxtipodireccion,
                    sysdate tedfhoramod,
                    0 usuidmod,
                    dre.tedmotivo,
                    dre.tedfvigencia,
                    dre.tedorigen
                    from alba.expedientesingresoscontr eic,
                    alba.direccionesexpedientes dre
                    where eic.esmid=e.alegesmid
                    and eic.eicid=dre.eicid;                    
                    
                      -- Id detalle libro
                    select alba.det_libro_resol_sq.nextval into vDlbid from dual;
                    
                    -- Inserta Detalle
                    insert into alba.det_libro_resol d
                    (d.dlbid, d.expid, d.lbrid, d.dlbfhoramod, d.usuidmod, d.dlbmotivo,
                    d.dlbfvigencia, d.mulid, d.perid, d.perversion, d.rdiid, d.esmid)
                    values
                    (vDlbid, e.expid, vLbrid, sysdate, vUsuidmod, 'Se ha creado',
                    null, e.mulid, e.peridD, e.perversionD, e.rdiidD, e.newEsmid);
                    
                    -- Actualiza Esrito REA a Ejecutado
                    update alba.escritosmultas emr
                    set emr.tdmresid=521
                    where emr.esmid=e.alegesmid
                    and emr.tidomid=101
                    and emr.tdmresid=261;
                    
                    vProcesados:=vProcesados+1;
          
                end loop;       
       
      elsif vTipoLibro in (121,141,142,361,663,664,681,682,701,721,722,723) then
            
        -- Para los libros de BOP utiliza la fecha de prescripción como criterio de selección
                for e in cExpPresOpt(in_fdesde,in_fhasta,vLestado) loop

                    -- Id detalle libro
                    select alba.det_libro_resol_sq.nextval into vDlbid from dual;

                    -- Inserta Detalle
                    insert into alba.det_libro_resol d
                    (d.dlbid, d.expid, d.lbrid, d.dlbfhoramod, d.usuidmod, d.dlbmotivo,
                    d.dlbfvigencia, d.mulid, d.perid, d.perversion, d.rdiid, d.esmid)
                    values
                    (vDlbid, e.expid, vLbrid, sysdate, vUsuidmod, 'Se ha creado',
                    null, e.mulid, e.peridD, e.perversionD, e.rdiidD, 0);

                    vProcesados:=vProcesados+1;

                end loop;
             
        elsif vtipolibro in (261,521,1161,541,1201,761,561,1241,27,601,1181,1342,1382,1381) then
        -- Libros de baja que necesitan multiplicarse en función del estado de las liquidaciones
        
                -- Creación Libro RPA Estimados con Liquidaciones Cobradas o con Cantidades a Cuenta
                for e in cLrpaci(vLestado) loop

                    -- Id detalle libro
                    select alba.det_libro_resol_sq.nextval into vDlbid from dual;

                    -- Inserta Detalle
                    insert into alba.det_libro_resol d
                    (d.dlbid, d.expid, d.lbrid, d.dlbfhoramod, d.usuidmod, d.dlbmotivo,
                    d.dlbfvigencia, d.mulid, d.perid, d.perversion, d.rdiid, d.esmid)
                    values
                    (vDlbid, e.expid, vLbrid, sysdate, vUsuidmod, 'Se ha creado',
                    null, e.mulid, e.peridD, e.perversionD, e.rdiidD, e.alegesmid);                    

                    vProcesados:=vProcesados+1;

                end loop;
                
                -- Si no hay datos para el libro anterior
                if vProcesados=0 then
                
                      -- Creación Libro RPA Estimados con Liquidaciones Cobradas o con Cantidades a Cuenta
                      for e in cLrpasi(vLestado) loop
      
                          -- Id detalle libro
                          select alba.det_libro_resol_sq.nextval into vDlbid from dual;
      
                          -- Inserta Detalle
                          insert into alba.det_libro_resol d
                          (d.dlbid, d.expid, d.lbrid, d.dlbfhoramod, d.usuidmod, d.dlbmotivo,
                          d.dlbfvigencia, d.mulid, d.perid, d.perversion, d.rdiid, d.esmid)
                          values
                          (vDlbid, e.expid, vLbrid, sysdate, vUsuidmod, 'Se ha creado',
                          null, e.mulid, e.peridD, e.perversionD, e.rdiidD, e.alegesmid);                    
                          
      
                          vProcesados:=vProcesados+1;
      
                      end loop;                    
                
                end if;
                
                -- Si no hay datos para el libro anterior se queda con datos sin liquidación
                if vProcesados=0 then
                
                      -- Creación Libro RPA Estimados sin Liquidaciones
                      for e in cLrpasl(vLestado) loop
      
                          -- Id detalle libro
                          select alba.det_libro_resol_sq.nextval into vDlbid from dual;
      
                          -- Inserta Detalle
                          insert into alba.det_libro_resol d
                          (d.dlbid, d.expid, d.lbrid, d.dlbfhoramod, d.usuidmod, d.dlbmotivo,
                          d.dlbfvigencia, d.mulid, d.perid, d.perversion, d.rdiid, d.esmid)
                          values
                          (vDlbid, e.expid, vLbrid, sysdate, vUsuidmod, 'Se ha creado',
                          null, e.mulid, e.peridD, e.perversionD, e.rdiidD, e.alegesmid);                    
                          
      
                          vProcesados:=vProcesados+1;
      
                      end loop;                    
                
                end if;                
        
        else
                for e in cExpOpt(in_fdesde,in_fhasta,vLestado) loop 


                            -- Id detalle libro
                            select alba.det_libro_resol_sq.nextval into vDlbid from dual;

                            vMulid:=e.mulid;

                            if vTipoLibro in (67,324) then --Identificacion de Conductor
                            
                                dbms_output.put_line('Nueva versión 1.6. MulidOld='||to_char(e.mulid));
                                vMulid:=nuevaVersion16(e.mulid,0,0,0,null);
                                dbms_output.put_line('Nueva versión 1.6. MulidNew='||to_char(vMulid));
                                
                            end if;

                            if vMulid>0 then
                            -- Inserta Detalle
                                insert into alba.det_libro_resol d
                                (d.dlbid, d.expid, d.lbrid, d.dlbfhoramod, d.usuidmod, d.dlbmotivo,
                                d.dlbfvigencia, d.mulid, d.perid, d.perversion, d.rdiid, d.esmid)
                                values
                                (vDlbid, e.expid, vLbrid, sysdate, vUsuidmod, 'Se ha creado',
                                null, vMulid, e.peridD, e.perversionD, e.rdiidD, e.alegesmid);
                            end if;

                            vProcesados:=vProcesados+1;
                            
                        --end loop;
                        
                end loop;
        end if;
        


    end if;



    if vProcesados>0 then
    
        commit;
        
        
        if vTipoLibro in (161,261) then

                -- Asocia al libro un número consecutivo para libros de Resol. Rec. PA.
                update alba.libro_resoluciones lr
                set lr.lbrnumero=
                (select to_char(sysdate,'rrrr')||lpad(numero,4,0)+0
                from (select max((substr(to_char(l.lbrnumero),5,4))+0)+1 numero
                from alba.libro_resoluciones l
                where substr(to_char(l.lbrnumero),1,4)=to_char(sysdate,'rrrr') and l.lrtid in (161,261)))
                where lr.lbrid=vLbrid;

        else
                -- Asocia al libro un número consecutivo
                update alba.libro_resoluciones lr
                set lr.lbrnumero=
                (select to_char(sysdate,'rrrr')||lpad(numero,4,0)+0
                from (select max((substr(to_char(l.lbrnumero),5,4))+0)+1 numero
                from alba.libro_resoluciones l
                where substr(to_char(l.lbrnumero),1,4)=to_char(sysdate,'rrrr') and l.lrtid not in (161,261)))
                where lr.lbrid=vLbrid;

        end if;
        
        
        --
        commit;
        
        --alba.paq_ficheros.generarinformetex(vLbrid);
        
        commit;
        
        out_procesados:=vProcesados;
        out_salida:='Libro ' || to_char(vLbrnumero) || '. Total Expedientes Incluidos = ' || to_char(vProcesados);
        
        vInformeTex:=alba.paq_ficheros.generarinformetex(vLbrid,0);
        
    else
    
        rollback;
        out_procesados:=vProcesados;
        out_salida:='--El proceso no encontró datos.';
        
    end if;


    exception when others then
        dbms_output.put_line('Error '||sqlerrm||' '||sqlcode);
        rollback;
        error ('Error al intentar generar el libro','trml.generaLibroMultas', 20001 );    
        Raise;
end generaLibroMultas;


--------------------------------------------------------------------------------
-- Cambiar exacción denuncias de botellón
--------------------------------------------------------------------------------
procedure cambiarExaccion(in_lbrid number) is

cursor clibro(vid number) is
select distinct e.expid, est.origendenuncia
from
alba.seobjetos s,
alba.expingre e,
alba.multas m,
alba.det_libro_resol d,
table(trml.emultas(m.mulid)) est
where m.mulid=d.mulid
and m.seoidexp=s.seoid
and s.expid=e.expid
and m.expidexp=e.expid
and m.mulid=est.mulid
and d.lbrid=vId
and e.mexid=21
and est.origendenuncia in (4,6);

begin

      for l in cLibro(in_lbrid) loop
      
        update alba.expingre e
        set e.mexid=decode(l.origendenuncia,4,1,6,4,21)
        where e.expid=l.expid;
      
      end loop;

exception
    when others then
       dbms_output.put_line('KO. ' || sqlerrm);
       raise;
end;


--------------------------------------------------------------------------------
-- Firma Libro
--------------------------------------------------------------------------------
procedure firmaLibro (in_lbrid number) is

  cursor cLib is
  select l.lbrid, l.lrtid, d.mulid, d.esmid
  from alba.libro_resoluciones l,
  alba.det_libro_resol d
  where d.lbrid=l.lbrid
  and l.lbrid=in_lbrid;
  
  cursor cTipoLibro is
  select l.lrtid from alba.libro_resoluciones l where l.lbrid=in_lbrid;
  
  vTipoLibro number:=0;
  
begin

  for t in cTipoLibro loop
    vTipoLibro:=t.lrtid;
  end loop;
  
  -- En libros Estimación TCA (75) actualiza estado escritos a Sentencia TCA. Pendiente Ejecución
  if vTipoLibro = 1421 then 
    for l in cLib loop
      update alba.escritosmultas e
      set e.tdmresid=681, e.ESMFHORAMOD=sysdate
      where e.esmid=l.esmid
      and e.mulid=l.mulid
      and e.tidomid=242
      and e.tdmresid=641;
    end loop;
  end if;
  
  commit;

exception when others then

  dbms_output.put_line('KO. ' || sqlerrm);
  rollback;

end firmaLibro;


--------------------------------------------------------------------------------
-- Generación de Propuestas de Notificación
--------------------------------------------------------------------------------
procedure generaPropNotifMultas ( in_lbrid in  number,
                                  in_usuidmod in  number,
                                  in_sysdate in  date,
                                  in_motivo in  varchar2,
                                  out_procesados out number    ,
                                  out_salida out varchar2
                                  ) is


  cursor cexp(vid alba.libro_resoluciones.lbrid%type) is
    select /*RULE*/ f.peridd, f.perversiond, f.rdiidd,
  --decode(t.atoid,5,decode(mulxboletin,543,917,5),t.atoid) atoid,
  t.atoid,
  d.dlbid, e.mulid, d.esmid,
    decode(e.origendenuncia,4,1,21) mexid, f.nombred, f.nifd, f.domiciliod
    from alba.multas m, alba.det_libro_resol d, alba.libro_resoluciones l, alba.libro_resol_tipos t,
    table(trml.dmultas(d.mulid,d.esmid)) f, table(trml.emultas(d.mulid)) e
    where l.lbrid=d.lbrid
    and d.mulid=f.mulid
    and d.mulid=e.mulid
    and l.lrtid=t.lrtid
    and d.mulid=m.mulid
    and e.cestado=t.lrtestadopropnotif
    and f.rdiidd>0
    and f.peridd>0
    and t.lrtid<>342
    and l.lbrid=vid
    union
    select f.peridd, f.perversiond, f.rdiidd,
    --decode(t.atoid,3,decode(mulxboletin,465,917,3),t.atoid) atoid,
  t.atoid,
    d.dlbid, d.mulid, d.esmid, 21 mexid, f.nombred, f.nifd, f.domiciliod
    from alba.multas m, alba.det_libro_resol d, alba.libro_resoluciones l, alba.libro_resol_tipos t,
    table(trml.dmultas(d.mulid,d.esmid)) f, table(trml.eescrito(d.esmid)) e
    where l.lbrid=d.lbrid
    and d.mulid=f.mulid
    and l.lrtid=t.lrtid
    and d.mulid=m.mulid
    and d.esmid=e.esmid
    and e.cestado=7020
    and f.rdiidd>0
    and f.peridd>0
    and t.lrtid=342
    and l.lbrid=vid
    union
    select f.peridd, f.perversiond, f.rdiidd,
    --decode(t.atoid,3,decode(mulxboletin,465,917,3),t.atoid) atoid,
  t.atoid,
    d.dlbid, d.mulid, d.esmid, 21 mexid, f.nombred, f.nifd, f.domiciliod
    from alba.multas m, alba.det_libro_resol d, alba.libro_resoluciones l, alba.libro_resol_tipos t,
    table(trml.dmultas(d.mulid,d.esmid)) f, table(trml.eescrito_tea(d.esmid)) e
    where l.lbrid=d.lbrid
    and d.mulid=f.mulid
    and l.lrtid=t.lrtid
    and d.mulid=m.mulid
    and d.esmid=e.esmid
    and e.cestado=4
    and f.rdiidd>0
    and f.peridd>0
    and t.lrtid in (1021,1061)
    and l.lbrid=vid    
    union
    select f.peridd, f.perversiond, f.rdiidd,
    --decode(t.atoid,3,decode(mulxboletin,465,917,3),t.atoid) atoid,
  t.atoid,
    d.dlbid, d.mulid, d.esmid, 21 mexid, f.nombred, f.nifd, f.domiciliod
    from alba.multas m, alba.det_libro_resol d, alba.libro_resoluciones l, alba.libro_resol_tipos t,
    table(trml.dmultas(d.mulid,d.esmid)) f, table(TRML.EESCRITO_TCA(d.esmid)) e
    where l.lbrid=d.lbrid
    and d.mulid=f.mulid
    and l.lrtid=t.lrtid
    and d.mulid=m.mulid
    and d.esmid=e.esmid
    and e.cestado in (8520,8620)
    and f.rdiidd>0
    and f.peridd>0
    and t.lrtid in (1423,1424)
    and l.lbrid=vid    
    union
    select /*RULE*/ d.perid peridd, d.perversion perversiond, d.rdiid rdiidd, t.atoid,
    d.dlbid, d.liqid mulid, d.esmid, null mexid, '' nombred, '' nifd, '' domiciliod
    from alba.det_libro_resol d, alba.libro_resoluciones l, 
    alba.libro_resol_tipos t, alba.liquidaciones ll, alba.expingre ep
    where l.lbrid=d.lbrid
    and l.lrtid=t.lrtid
    and d.rdiid>0
    and d.perid>0
    and d.liqid is not null
    and d.mulid is null
    and ll.expid=ep.expid
    and d.liqid=ll.liqid
    and t.lrtid in (1423,1424)
    and l.lbrid=vid      
  union
  select /*RULE*/ f.peridd, f.perversiond, f.rdiidd,
  --decode(t.atoid,3,decode(mulxboletin,465,917,3),t.atoid) atoid,
  t.atoid,
  d.dlbid, d.mulid, d.esmid, 21 mexid, f.nombred, f.nifd, f.domiciliod
  from alba.multas m, alba.det_libro_resol d, alba.libro_resoluciones l, alba.libro_resol_tipos t,
  table(trml.dmultas(d.mulid,d.esmid)) f
  where l.lbrid=d.lbrid
  and d.mulid=f.mulid
  and l.lrtid=t.lrtid
  and d.mulid=m.mulid
  and f.rdiidd>0
  and f.peridd>0
  and t.lrtid in (1021,1061)
  and l.lbrid=vid
  union
  select /*RULE*/ d.perid peridd, d.perversion perversiond, d.rdiid rdiidd, t.atoid,
  d.dlbid, d.liqid mulid, d.esmid, null mexid, '' nombred, '' nifd, '' domiciliod
  from alba.det_libro_resol d, alba.libro_resoluciones l, 
  alba.libro_resol_tipos t, alba.liquidaciones ll, alba.expingre ep
  where l.lbrid=d.lbrid
  and l.lrtid=t.lrtid
  and d.rdiid>0
  and d.perid>0
  and d.liqid is not null
  and d.mulid is null
  and ll.expid=ep.expid
  and d.liqid=ll.liqid
  and t.lrtid in (1021,1061)
  and l.lbrid=vid  
  ;
    
    
    -- Generar Documento Cobratorio
    cursor cDC(vId number) is
    select /*RULE*/
    lq.liqid,
    --
    alba.doccobratorios_sq.NEXTVAL dccid,
    substr(lq.liqnumerorecliquidacion,3,10) dccdocumento,
    --round(nvl(lq.liqimporte,0)+nvl(lq.liqcostas,0)+(nvl(lq.liqimporte,0)*2/10)+nvl(dintereses,0),2) dccimporte,
    (i.dprincipal-i.dprincipalco)+(i.dintereses-i.dinteresesco)+(i.dcostas-i.dcostasco)+(i.dapremio-i.dapremioco) dccimporte,    
    to_date(sysdate+60,'dd/mm/rrrr') dccfechalimpg,
    sysdate dccfhoramod,
    0 usuidmod,
    'Se ha creado' dccmotivo,
    null dccfvigencia,
    null dccofivir,
    null dcccodigodebarras,
    --
    alba.detdoccobratorios_sq.NEXTVAL ddcid,
    --nvl(round(dintereses,2),0) ddcintereses,
    --nvl(round(lq.liqcostas,2),0) ddccostas,
    --nvl(round((lq.liqimporte*2/10),2),0) ddcapremio,
    i.dintereses-i.dinteresesco ddcintereses,
    i.dcostas-i.dcostasco ddccostas,
    i.dapremio-i.dapremioco ddcapremio,
    sysdate ddcfhoramod,
    'Se ha creado' ddcmotivo,
    null ddcfvigencia,
    --nvl(round(lq.liqimporte,2),0) ddcprincipal,
    --round(nvl(lq.liqimporte,0)+nvl(lq.liqcostas,0)+(nvl(lq.liqimporte,0)*2/10)+nvl(dintereses,0),2) ddctotal,
    i.dprincipal-i.dprincipalco ddcprincipal,
    i.importe ddctotal,
    null ceeid,
    null ddcespec1,
    null ddcespec2
    --

    from alba.liquidaciones lq,
    table(alba.pkg_calculoimportes.ctd(lq.liqid,sysdate)) i,
    alba.libro_resoluciones l,
    alba.det_libro_resol d
    where d.lbrid=l.lbrid
    and lq.liqid=i.liqid
    and d.expid=lq.expid
    and l.lrtid in (161,261) -- Recurso contra la providencia de apremio
    and l.lbrid=vId;
    
    -- Genera Detalle Actuaciones Ejecutiva
    cursor cDAE(vId number, vMulid number, vPntid number, vAejid number) is
    select /*RULE*/
    alba.detallesactuacionesejecu_sq.nextval daeid,
    vAejid aejid,
    lq.liqid,
    null dxeid,
    sysdate daefhoramod,
    0 usuidmod,
    'Se ha creado' daemotivo,
    null daefvigencia,
    null daeimporte,
    sysdate daefnotifsa,
    vPntid pntid,
    e.esmfescrito
    from alba.escritosmultas e,
    alba.liquidaciones lq,
    alba.libro_resoluciones l,
    alba.det_libro_resol d
    where d.lbrid=l.lbrid
    and d.expid=lq.expid
    and d.esmid=e.esmid
    and l.lrtid in (161,261,342) -- Recurso contra la providencia de apremio
    and l.lbrid=vId
    and d.mulid=vMulid;    


    -- Genera Cabecera Actuaciones Ejecutiva
    cursor cAE(vId number, vAejid number) is
    select /*RULE*/
    vAejid aejid,
    decode(l.lrtid,
          161,1656,
          261,1656,
          342,1917) DACID,
    null ACT_AEJID,
    'L' AEJOBJETO,
    decode(l.lrtid,
          161,'Notificación Resolución RPA',
          261,'Notificación Resolución RPA',
          342,'Notificación Resolución REA') AEJDESCR,
    null AEJESTADO,
    l.lbrfhoramod AEJFCREACION,
    0 AEJIMPORTECOSTAS,
    'E' AEJMONEDA,
    null AEJFAPRUEBA,
    l.lbrfhoramod AEJFPETICIONNOT,
    'N' AEJNOTIFICAPERREL,
    'N' AEJCARTAPAGO,
    l.lbrfhoramod AEJFHORAMOD,
    0 USUIDMOD,
    'Se ha creado' AEJMOTIVO,
    null AEJFVIGENCIA,
    l.lbrnumero AEJCODIGO,
    null AEJFECHAVTO,
    null AEJFPROVI
    from 
    alba.libro_resoluciones l
    where l.lrtid in (161,261,342) -- Recurso contra la providencia de apremio
    and l.lbrid=vId;
    
    
    vlbrid number := 0;
    vUsuidmod number := 0;
    vSysDate date := null;
    vMotivo varchar2(200) := '';
    vProcesados    number := 0;
    vSalida varchar2(200) := '';
    vPntid number := 0;
    vPntid_dev number := 0;
    vMulid varchar2(30)    := '';
    vAejid number:=0;
    vAejid2 number:=0;
    vFechaEscrito date:=null;
    
    vatoid number:=0;
    v_total_registros number:=0;    

begin

    -- Inicialización de parámetros
    vLbrid:= in_lbrid;
    vUsuidmod:= in_usuidmod;
    vSysDate:= To_Date(sysDate);    
    vMotivo:= in_motivo;
    vProcesados:= 0;
    vSalida:= '';

    if vLbrid>0 then
    
    
        cambiarExaccion(vLbrid);
        
        -- Crea Cabecera Actuación Ejecutiva
        /*select alba.actuacionesejecutiva_sq.nextval into vAejid from dual;
        
        for ae in cAE(vLbrid,vAejid) loop
        
            insert into alba.actuacionesejecutiva ae2
              (ae2.aejid, ae2.DACID,ae2.ACT_AEJID,ae2.AEJOBJETO,ae2.AEJDESCR,ae2.AEJESTADO,ae2.AEJFCREACION,
              ae2.AEJIMPORTECOSTAS,ae2.AEJMONEDA,ae2.AEJFAPRUEBA,ae2.AEJFPETICIONNOT,
              ae2.AEJNOTIFICAPERREL,ae2.AEJCARTAPAGO,ae2.AEJFHORAMOD,ae2.USUIDMOD,
              ae2.AEJMOTIVO,ae2.AEJFVIGENCIA,ae2.AEJCODIGO,ae2.AEJFECHAVTO,ae2.AEJFPROVI)
            values
              (ae.aejid, ae.DACID,ae.ACT_AEJID,ae.AEJOBJETO,ae.AEJDESCR,ae.AEJESTADO,ae.AEJFCREACION,
              ae.AEJIMPORTECOSTAS,ae.AEJMONEDA,ae.AEJFAPRUEBA,ae.AEJFPETICIONNOT,
              ae.AEJNOTIFICAPERREL,ae.AEJCARTAPAGO,ae.AEJFHORAMOD,ae.USUIDMOD,
              ae.AEJMOTIVO,ae.AEJFVIGENCIA,ae.AEJCODIGO,ae.AEJFECHAVTO,ae.AEJFPROVI);
        
        end loop;*/    

        
        -- Bucle Principal
        for e in cExp(vLbrid) loop
        
            if e.rdiidd<>19129855 then --NO DEV
        
                    -- Genera Propuesta
                    vMulid := to_char(e.mulid);
                    
                    -- Generación de fotos en notificaciones
                    vAtoid:=e.atoid;
                    v_total_registros:=0;
                    if vAtoid in (3,5) and v_total_registros=0 then
                    
                          select count(*) total_registros into v_total_registros
                          from
                          alba.multas m,
                          alba.multacar_blob b
                          where m.mulnumbol=b.boletin
                          and m.mulanobol=b.annoboletin                  
                          and m.mulid=e.mulid
                          and (b.fichero1 is not null or b.fichero2 is not null);              
                          
                          if v_total_registros>0 and vAtoid=3 then
                            vatoid:=1257;
                          elsif v_total_registros>0 and vAtoid=5 then
                            vatoid:=917;
                          end if;
                      
                    end if;
                    
                    --
                    select alba.propuestasnotificacion_sq.nextval into vPntid from dual;
                            
                    insert into alba.propuestasnotificacion p
                        (p.pntid, p.tppid, p.perid, p.perversion, p.rdiid,
                        p.mexid, p.atoid, p.cmnid, p.pntorigen, p.pntorigenid,
                        p.pntobjentidad, p.pntobjsiglas, p.pntobjid    , p.pntfecha, p.pntlimite,
                        p.pntestado, p.pntfhoramod, p.usuidmod, p.pntmotivo, p.pntfvigencia    ,
                        p.pntimporte, p.pntpre1, p.pntpre2, p.pntpre3, p.pntpreorden,
                        p.pntpre4, p.daeid, p.resid, p.pntrutainspeccion, p.pntimportebak)
                    values
                        (vpntid, 1, e.peridd, e.perversiond, e.rdiidd,
                        decode(e.mexid,null,21,e.mexid), vAtoid, null, 'I', e.mulid,
                        decode(e.mexid,null,'LIQ','MULTAS'), decode(e.mexid,null,'LIQ','MUL'), e.esmid, vSysDate, to_date(vSysDate+50),
                        'P', vSysDate, vUsuidmod, 'Se ha creado', to_date(vSysDate+50),
                        0, 17491, 66, 6089, 1,
                      8249, null, null, null, null);
                      
                      vProcesados:=vProcesados+1;              
                        
                        
                    -- Genera Actuación Ejecutiva
                    /*for dae in cDAE(vLbrid,vMulid,vPntid,vAejid) loop
                    
                        -- DETALLE NOTIFICACIÓN RRPA
                        insert into alba.detallesactuacionesejecu dae2
                            (dae2.daeid,dae2.aejid,dae2.liqid,dae2.dxeid,
                            dae2.daefhoramod,dae2.usuidmod,dae2.daemotivo,dae2.daefvigencia,
                            dae2.daeimporte,dae2.daefnotifsa,dae2.pntid)
                        values
                            (dae.daeid,dae.aejid,dae.liqid,dae.dxeid,
                            dae.daefhoramod,dae.usuidmod,dae.daemotivo,dae.daefvigencia,
                            dae.daeimporte,dae.daefnotifsa,dae.pntid);
                    
                    end loop;*/
                    
        
                -- Genera Documentos Cobratorios
                /*for h in cDc(vLbrid) loop
        
                    insert into alba.doccobratorios a
                    (a.dccid, a.dccdocumento, a.dccimporte, a.dccfechalimpg,
                    a.dccfhoramod, a.usuidmod, a.dccmotivo, a.dccfvigencia,
                    a.dccofivir, a.dcccodigodebarras)
                    values
                    (h.dccid, h.dccdocumento, h.dccimporte, h.dccfechalimpg,
                    h.dccfhoramod, h.usuidmod, h.dccmotivo, h.dccfvigencia,
                    h.dccofivir, h.dcccodigodebarras);
        
                    --
                    insert into alba.detdoccobratorios a
                    (a.ddcid, a.dccid, a.liqid, a.ddcintereses, a.ddccostas,
                    a.ddcapremio, a.ddcfhoramod, a.usuidmod, a.ddcmotivo,
                    a.ddcfvigencia, a.ddcprincipal, a.ddctotal, a.ceeid, a.ddcespec1,
                    a.ddcespec2)
                    values
                    (h.ddcid, h.dccid, h.liqid, h.ddcintereses, h.ddccostas,
                    h.ddcapremio, h.ddcfhoramod, h.usuidmod, h.ddcmotivo,
                    h.ddcfvigencia, h.ddcprincipal, h.ddctotal, h.ceeid, h.ddcespec1,
                    h.ddcespec2);
        
        
                end loop;*/
          
          else 
          
                ------------------------------
                -- GENERA PROPUESTAS NOTIF DEV
                ------------------------------
                
                
                    -- Generación de fotos en notificaciones
                    vAtoid:=e.atoid;
                    v_total_registros:=0;
                    if vAtoid in (3,5) and v_total_registros=0 then
                    
                          select count(*) total_registros into v_total_registros
                          from
                          alba.multas m,
                          alba.multacar_blob b
                          where m.mulnumbol=b.boletin
                          and m.mulanobol=b.annoboletin                  
                          and m.mulid=e.mulid
                          and (b.fichero1 is not null or b.fichero2 is not null);              
                          
                          if v_total_registros>0 and vAtoid=3 then
                            vatoid:=1257;
                          elsif v_total_registros>0 and vAtoid=5 then
                            vatoid:=917;
                          end if;
                      
                    end if;                
                

                                    
                    select alba.propuestasnotifnostradev_sq.nextval into vPntid_dev from dual;
                    
                    insert into alba.propuestasnotifnostradev
                    (
                      pnnid,mexid,pnnfemision,
                      mulid,perid,perversion,rdiid,
                      atoid,PNNFPROP,PNNDESTINATARIO,PNNNIF,PNNDIRECCION,
                      PNNFHORAMOD,USUIDMOD,PNNMOTIVO,esmid
                    )
                    values
                    (
                      vPntid_dev,-- pnnid 
                      e.mexid,-- mexid
                      null,-- pnnfemision
                      e.mulid, --mulid
                      e.peridd, --perid
                      e.perversiond, --perversion
                      e.rdiidd,--rdiid
                      vAtoid, --atoid
                      sysdate, --PNNFPROP
                      e.nombred, --PNNDESTINATARIO
                      e.nifd, --PNNNIF
                      e.domiciliod, --PNNDIRECCION
                      sysdate, --PNNFHORAMOD
                      0, --USUIDMOD
                      'Se ha creado', --PNNMOTIVO
                      e.esmid
                    );   
                         
                    
                     vProcesados:=vProcesados+1;
                
          end if;
      end loop;          

    end if;


    if vprocesados>0 then
        update alba.libro_resoluciones l  set l.lbrfpropnot = to_date(sysdate,'dd/mm/rrrr')
        where l.lbrid=vLbrid;
        
        commit;
        
        out_procesados:=vProcesados;
        out_salida:='Total Propuestas de Notificación Generadas = ' || to_char(vProcesados);
    Else
        rollback;
        out_procesados:=0;
        out_salida:='Total Propuestas de Notificación Generadas = 0';
    
    End If;

    exception
        when others then
            out_procesados:=vProcesados;
            out_salida:='Error. MULID='||vMulid||'. '||sqlerrm;
            Rollback;
end;



--------------------------------------------------------------------------------
-- Generación de Propuestas de Notificación
--------------------------------------------------------------------------------
/*procedure generaRemesaMultas (in_lbrid number) is

cursor cPropuestasNotificacion
                  
begin
end generaRemesaMultas;*/


--------------------------------------------------------------------------------
-- fmultas: Generación de Fichero de Multas
--------------------------------------------------------------------------------
function fmultas(vCrmid number) return fmul_lista pipelined is
-- Cursores
-- Vehículo
cursor cveh(vmulid alba.multas.mulid%type) is
select /*RULE*/ * from (
select mulid, matricula, marca, tipo from
(
select m1.mulid mulid, v1.vehmatri matricula, nvl(v1.vehmarca,m1.mulmarca) marca, t1.tvtmdes tipo, 3 vehid
from alba.tivdgt_m t1, alba.vehiculos_d v1, alba.multas m1
where m1.seoidveh=v1.seoid
and v1.tvtmid=t1.tvtmid(+)
and v1.vehversion=
(select max(vehversion) from alba.vehiculos_d v where v.seoid = m1.SeoIdVeh)
and m1.mulid=vMulid
union
select m2.mulid mulid, v2.vemmatri matricula, nvl(v2.vemmarca,m2.mulmarca) marca, t2.tvtmdes tipo, 2 vehid
from alba.tivdgt_m t2, alba.vehiculos_mult v2, alba.multas m2
where m2.vemid=v2.vemid
and m2.tvtmid=t2.tvtmid(+)
and m2.mulid=vMulid
UNION
select d.mulid, w.wsmatricula matricula, trim(w.wsmarca) || ' ' ||trim(w.wsmodelo) marca, '' tipo, 1 vehid
from alba.multas mm, alba.datoswsdgt w, alba.det_libro_resol d
where mm.mulid=d.mulid
and d.idwsd=w.idwsd
and mm.seoidexp = (select m.seoidexp from alba.multas m where m.mulid=vmulid)
) order by vehid asc, mulid desc ) where rownum<2;
-- Remesa
cursor cRem(vClave number) is
select /*RULE*/ m.mulid, m.expidexp, NVL (p1.periden, p1.peridenext) dni,
        alba.componer_nombre (p1.pernombre, p1.perapell1, p1.perpart1, p1.perapell2, p1.perpart2, p1.perrazon) nombre,
        alba.direccion_rdi (n.rdiid) direccion,
        alba.codigopostal_rdi (n.rdiid) codigopostal,
        alba.municipio_rdi_sincp (n.rdiid) municipio,
        alba.provincia_rdi (n.rdiid) provincia,
        m.mulfeciniprod, a.atoid, m.mulfec, m.mulimp, e.expcod, m.mulnumbol,
        nvl(ltrim(rtrim(tvi.tvidesc || ' ' || via.viadesc)), nvl(m.mullugar,''))
        || decode (nvl(m.muldelante,''),'','',' Delante de: ' || nvl(m.muldelante,''))
        || decode (nvl(m.mulfrente,''),'','',' Frente a: ' || nvl(m.mulfrente,''))
        || decode (nvl(m.mulobsdir,''),'','',' Con dirección a: ' || nvl(m.mulobsdir,'')) lugar,
        lpad(m.mulhor,2,0) || ':' || lpad(m.mulmin,2,0) hora, age.agenumero,
        m.muldenuncia hd, nvl(n.ntfsicergrupo,n.ntfserade) sicer, i.infarticulo articulo, i.infapartado apartado,
        decode(i.infgrado,'L','Leve','G','Grave','M','Muy grave') grado, di.decdescr norma, m.mulmiembrofse fse,
        i.puntostxt puntos, c.crmfecha fechaEmision, m.mulfecnotden fecnotden, m.mulresmedida3 alcoholEnAire,
        p.pntobjid esmid, nvl(to_char(age.agenumero),m.mulmiembrofse) denunciante,
        decode(nvl(to_char(age.agenumero),m.mulmiembrofse),null,0,1) HayDenunciante, 0 liqid,
        0 importeTotalLiq, '' recibo,0 liqimporte,0 liqintereses,0 liqcostas,0 liqapremio,
        '' rd, '' fsanfir, '' ffinvol, '' finieje, null flimitepago, m.mulresmedida1 velocidad,
        decode(m.mulxboletin,144,1,303,2,403,0,444,3,423,4,465,7,464,6,543,8,800,11) origenDenuncia, m.mulxmedida4,
        m.mulobservaciones obs
from alba.personas p1, alba.unidadesnotf u, alba.vias via, alba.tvia tvi,
     alba.agentes age, alba.decoingresos mot, alba.decoingresos di,
     alba.infracciones i,
     alba.actosnotificacion a, alba.remesas r, alba.expingre e,
     alba.notificaciones n, alba.propuestasnotificacion p,
     alba.cabremesas c, alba.multas m
where n.perid = p1.perid
and n.perversion = p1.perversion
and    a.atoid = p.atoid
and p.pntid = n.pntid
and n.ntfid = r.ntfid
and n.ntfcrmid = c.crmid
and r.crmid = c.crmid
and c.udnid = u.udnid
and m.expidexp=e.expid
and m.viaid = via.viaid (+)
and via.tviid = tvi.tviid (+)
and m.age1_ageid = age.ageid (+)
and m.mulxmotivo = mot.decid (+)
and m.infid=i.infid(+)
and i.infxlegislacion = di.decid (+)
and p.pntorigen='I'
and p.pntobjsiglas='MUL'
and m.mulid=p.pntorigenid
and c.crmid = vcrmid
and vClave = 1
union
select --liquidaciones
/*RULE*/ 
0 mulid, l.expid expidexp, NVL (p1.periden, p1.peridenext) dni,
alba.componer_nombre (p1.pernombre, p1.perapell1, p1.perpart1, p1.perapell2, p1.perpart2, p1.perrazon) nombre,
alba.direccion_rdi (n.rdiid) direccion,
alba.codigopostal_rdi (n.rdiid) codigopostal,
alba.municipio_rdi_sincp (n.rdiid) municipio,
alba.provincia_rdi (n.rdiid) provincia,
null mulfeciniprod, a.atoid, null mulfec, null mulimp, e.expcod, 
null mulnumbol,
substr(ex.mexabrev,1,30) lugar,
null hora, null agenumero,
null hd, nvl(n.ntfsicergrupo,n.ntfserade) sicer, null articulo, null apartado,
null grado, null norma, null fse,
null puntos, null fechaEmision, null fecnotden, null alcoholEnAire,
p.pntobjid esmid, null denunciante,
null HayDenunciante, l.liqid,
nvl(l.liqimporte,0)+nvl(l.liqcostas,0)+nvl(l.liqintereses,0)+nvl(l.liqapremio,0) importeTotalLiq,
l.liqnumerorecliquidacion recibo,
nvl(l.liqimporte,0) liqimporte,
nvl(l.liqintereses,0) liqintereses,
nvl(l.liqcostas,0) liqcostas,
nvl(l.liqapremio,0)  liqapremio,
'' rd, '' fsanfir, '' ffinvol, '' finieje, null flimitepago, null velocidad,
null origenDenuncia, null mulxmedida4,
null obs
from 
alba.personas p1, alba.unidadesnotf u,
alba.actosnotificacion a, alba.remesas r, alba.expingre e,
alba.exacciones ex,
alba.notificaciones n, alba.propuestasnotificacion p,
alba.cabremesas c, alba.liquidaciones l
where n.perid = p1.perid
and n.perversion = p1.perversion
and a.atoid = p.atoid
and p.pntid = n.pntid
and n.ntfid = r.ntfid
and n.ntfcrmid = c.crmid
and r.crmid = c.crmid
and c.udnid = u.udnid
and l.expid=e.expid
and e.mexid=ex.mexid
and p.pntorigen='I'
and p.pntobjsiglas='LIQ'
and l.liqid=p.pntorigenid
and c.crmid = vcrmid
and vClave = 1
union
select /*RULE*/ m.mulid, m.expidexp, NVL (p1.periden, p1.peridenext) dni,
        alba.componer_nombre (p1.pernombre, p1.perapell1, p1.perpart1, p1.perapell2, p1.perpart2, p1.perrazon) nombre,
        alba.direccion_rdi (n.rdiid) direccion,
        alba.codigopostal_rdi (n.rdiid) codigopostal,
        alba.municipio_rdi_sincp (n.rdiid) municipio,
        alba.provincia_rdi (n.rdiid) provincia,
        m.mulfeciniprod, a.atoid, m.mulfec, m.mulimp, e.expcod, m.mulnumbol,
        nvl(ltrim(rtrim(tvi.tvidesc || ' ' || via.viadesc)), nvl(m.mullugar,''))
        || decode (nvl(m.muldelante,''),'','',' Delante de: ' || nvl(m.muldelante,''))
        || decode (nvl(m.mulfrente,''),'','',' Frente a: ' || nvl(m.mulfrente,''))
        || decode (nvl(m.mulobsdir,''),'','',' Con dirección a: ' || nvl(m.mulobsdir,'')) lugar,
        lpad(m.mulhor,2,0) || ':' || lpad(m.mulmin,2,0) hora, age.agenumero,
        m.muldenuncia hd, nvl(n.ntfsicergrupo,n.ntfserade) sicer, i.infarticulo articulo, i.infapartado apartado,
        decode(i.infgrado,'L','Leve','G','Grave','M','Muy grave') grado, di.decdescr norma, m.mulmiembrofse fse,
        i.puntostxt puntos, c.crmfecha fechaEmision, m.mulfecnotden fecnotden, m.mulresmedida3 alcoholEnAire,
        p.pntobjid esmid, nvl(to_char(age.agenumero),m.mulmiembrofse) denunciante,
        decode(nvl(to_char(age.agenumero),m.mulmiembrofse),null,0,1) HayDenunciante, l.liqid,
        decode(a.atoid,77,
              (nvl(l.liqimporte,0)+nvl(l.liqcostas,0)),
              (nvl(l.liqimporte,0)+nvl(l.liqcostas,0)+(nvl(l.liqimporte,0)/10))) importeTotalLiq,
        l.liqnumerorecliquidacion recibo,
        nvl(l.liqimporte,0) liqimporte,
        nvl(l.liqintereses,0) liqintereses,
        nvl(l.liqcostas,0) liqcostas,
        nvl(l.liqapremio,0) liqapremio,
        ae.aejcodigo rd, to_char(l.liqfsancionfirme,'dd/mm/rrrr') fsanfir, to_char(l.liqffinperiodovol,'dd/mm/rrrr') ffinvol,
        to_char(pf.pyffdocu,'dd/mm/rrrr') finieje,
        to_date(nvl(greatest(l.liqfvtopagoejec,l.liqteorvtoeje),l.liqteorvtoeje),'dd/mm/rrrr') flimitepago,
        m.mulresmedida1 velocidad,
        decode(m.mulxboletin,144,1,303,2,403,0,444,3,423,4,465,7,464,6,543,8,800,11) origenDenuncia, m.mulxmedida4,
        m.mulobservaciones obs
from alba.personas p1, alba.unidadesnotf u, alba.vias via, alba.tvia tvi,
         alba.agentes age, alba.decoingresos mot, alba.decoingresos di,
     alba.infracciones i, alba.actuacionesejecutiva ae, alba.detallesactuacionesejecu de,
         alba.pliegosfacturas pf, alba.detallepliegosfras df,
     alba.actosnotificacion a, alba.remesas r, alba.expingre e, alba.liquidaciones l,
     alba.notificaciones n, alba.propuestasnotificacion p,
     alba.cabremesas c, alba.multas m
where n.perid = p1.perid
and n.perversion = p1.perversion
and    a.atoid = p.atoid
and p.pntid = n.pntid
and n.ntfid = r.ntfid
and n.ntfcrmid = c.crmid
and r.crmid = c.crmid
and c.udnid = u.udnid
and m.expidexp=e.expid
and m.viaid = via.viaid (+)
and via.tviid = tvi.tviid (+)
and m.age1_ageid = age.ageid (+)
and m.mulxmotivo = mot.decid (+)
and m.infid=i.infid(+)
and i.infxlegislacion = di.decid (+)
and l.expid=e.expid
and p.pntorigen='L'
and p.pntorigenid=l.liqid
and de.aejid=ae.aejid
and ae.dacid=58
and de.liqid=l.liqid
and df.pyfid=pf.pyfid
and pf.ormid=2
and pf.org_ormid=2
and pf.pyfperiodo='E'
and pf.pyftipodocu='P'
and pf.pyfsubtipo='P'
and pf.pyfdocanulado is null
and pf.pyfxestado=93
and df.liqid=l.liqid
and c.crmid = vcrmid
and vClave = 2
union
select distinct /*RULE*/ m.mulid, m.expidexp, NVL (p1.periden, p1.peridenext) dni,
        alba.componer_nombre (p1.pernombre, p1.perapell1, p1.perpart1, p1.perapell2, p1.perpart2, p1.perrazon) nombre,
        alba.direccion_rdi (n.rdiid) direccion,
        alba.codigopostal_rdi (n.rdiid) codigopostal,
        alba.municipio_rdi_sincp (n.rdiid) municipio,
        alba.provincia_rdi (n.rdiid) provincia,
        m.mulfeciniprod, a.atoid, m.mulfec, m.mulimp, e.expcod, m.mulnumbol,
        nvl(ltrim(rtrim(tvi.tvidesc || ' ' || via.viadesc)), nvl(m.mullugar,''))
        || decode (nvl(m.muldelante,''),'','',' Delante de: ' || nvl(m.muldelante,''))
        || decode (nvl(m.mulfrente,''),'','',' Frente a: ' || nvl(m.mulfrente,''))
        || decode (nvl(m.mulobsdir,''),'','',' Con dirección a: ' || nvl(m.mulobsdir,'')) lugar,
        lpad(m.mulhor,2,0) || ':' || lpad(m.mulmin,2,0) hora, age.agenumero,
        m.muldenuncia hd, nvl(n.ntfsicergrupo,n.ntfserade) sicer, i.infarticulo articulo, i.infapartado apartado,
        decode(i.infgrado,'L','Leve','G','Grave','M','Muy grave') grado, di.decdescr norma, m.mulmiembrofse fse,
        i.puntostxt puntos, c.crmfecha fechaEmision, m.mulfecnotden fecnotden, m.mulresmedida3 alcoholEnAire,
        p.pntobjid esmid, nvl(to_char(age.agenumero),m.mulmiembrofse) denunciante,
        decode(nvl(to_char(age.agenumero),m.mulmiembrofse),null,0,1) HayDenunciante, l.liqid,
        --dc.ddctotal importeTotalLiq,
        --l.liqnumerorecliquidacion recibo,dc.ddcprincipal liqimporte,dc.ddcintereses liqintereses,dc.ddccostas liqcostas, dc.ddcapremio liqapremio,
        decode(a.atoid,77,
              (nvl(l.liqimporte,0)+nvl(l.liqcostas,0)),
              (nvl(l.liqimporte,0)+nvl(l.liqcostas,0)+(nvl(l.liqimporte,0)/10))) importeTotalLiq,
        l.liqnumerorecliquidacion recibo,
        nvl(l.liqimporte,0) liqimporte,
        nvl(l.liqintereses,0) liqintereses,
        nvl(l.liqcostas,0) liqcostas,
        nvl(l.liqapremio,0) liqapremio,
        --
        ae.aejcodigo rd, to_char(l.liqfsancionfirme,'dd/mm/rrrr') fsanfir, to_char(l.liqffinperiodovol,'dd/mm/rrrr') ffinvol,
        to_char(pf.pyffdocu,'dd/mm/rrrr') finieje,
        --to_date(nvl(greatest(l.liqfvtopagoejec,l.liqteorvtoeje),l.liqteorvtoeje),'dd/mm/rrrr') flimitepago,
        to_date(n.ntfflimitepago,'dd/mm/rrrr') flimitepago,
        m.mulresmedida1 velocidad,
        decode(m.mulxboletin,144,1,303,2,403,0,444,3,423,4,465,7,464,6,543,8,800,11) origenDenuncia, m.mulxmedida4,
        m.mulobservaciones obs
from alba.personas p1, alba.unidadesnotf u, alba.vias via, alba.tvia tvi,
     alba.agentes age, alba.decoingresos mot, alba.decoingresos di,
     alba.infracciones i, alba.detdoccobratorios dc,
     alba.actuacionesejecutiva ae, alba.detallesactuacionesejecu de,
     alba.pliegosfacturas pf, alba.detallepliegosfras df,
     alba.actosnotificacion a, alba.remesas r, alba.expingre e, alba.liquidaciones l,
     alba.notificaciones n, alba.propuestasnotificacion p,
     alba.cabremesas c, alba.multas m
where n.perid = p1.perid
and n.perversion = p1.perversion
and    a.atoid = p.atoid
and p.pntid = n.pntid
and n.ntfid = r.ntfid
and n.ntfcrmid = c.crmid
and r.crmid = c.crmid
and c.udnid = u.udnid
and m.expidexp=e.expid
and m.viaid = via.viaid (+)
and via.tviid = tvi.tviid (+)
and m.age1_ageid = age.ageid (+)
and m.mulxmotivo = mot.decid (+)
and m.infid=i.infid(+)
and i.infxlegislacion = di.decid (+)
and l.expid=e.expid
and p.pntorigen='I'
and p.pntobjsiglas='MUL'
--and m.mulid=p.pntobjid
and m.mulid=p.pntorigenid
and de.aejid=ae.aejid
and ae.dacid=58
and de.liqid=l.liqid
and df.pyfid=pf.pyfid
and pf.ormid=2
and pf.org_ormid=2
and pf.pyfperiodo='E'
and pf.pyftipodocu='P'
and pf.pyfsubtipo='P'
and pf.pyfdocanulado is null
and pf.pyfxestado=93
and l.liqid=df.liqid
and l.liqid=dc.liqid(+)
--and dc.ddcid=(select max(dmax.ddcid) from alba.detdoccobratorios dmax where dmax.liqid=l.liqid)
and c.crmid = vcrmid
and vClave = 3;
--

-- Datos Resoluciones
cursor cRes(vMulid alba.multas.mulid%type, vEsmid alba.escritosmultas.esmid%type) is
select 
/*RULE*/ ra.rsaid, e.esmid, e.mulid, e.esmfescrito fEscrito, t.tidomnombre tipoEscrito,
e.esmfestado fResolucion, tr.tdmnombre tipoResultado, ra.algid detalleresolucion,
nvl(tr.tdminforme,t.tidominforme) informe,
ra.rsamotivoresol txtAlegacion, r.resdescr motivo,
e.esmcaja caja, e.esmlote lote, e.esmindice indice,
decode(e.tidomid,101,
replace(replace(replace(replace(replace(replace(replace
(replace(replace(replace(replace(ra.rsaresolucion,
'@recibos',depuracion.trml.parametro(e.esmid,r.resid,'recibos')),
'@recibo',depuracion.trml.parametro(e.esmid,r.resid,'recibo')),
'@expedientes',depuracion.trml.parametro(e.esmid,null,'expedientes')),
'@estimadas',depuracion.trml.parametro(e.esmid,null,'estimadas')),
'@desestimadas',depuracion.trml.parametro(e.esmid,null,'desestimadas')),
'@hoy',depuracion.trml.parametro(e.esmid,null,'hoy')),
'@recurrente',depuracion.trml.parametro(e.esmid,null,'recurrente')),
'@domicilio',depuracion.trml.parametro(e.esmid,null,'domicilio')),
'@pleno',depuracion.trml.parametro(e.esmid,null,'pleno')),
'@expediente_tea',depuracion.trml.parametro(e.esmid,null,'expediente_tea')),
'@asunto',depuracion.trml.parametro(e.esmid,null,'asunto')),
ra.rsaresolucion)
txtResolucion
from alba.tipos_doc_multas t, alba.tipos_doc_multas_res tr,
alba.resoluciones r, alba.resol_alegaciones ra,
alba.escritosmultas e, alba.multas m
where m.mulid=e.mulid
and e.tidomid=t.tidomid
and e.tdmresid=tr.tdmresid
and ra.esmid=e.esmid
and ra.resid=r.resid
and m.mulid=vMulid
and e.esmid=vEsmid
order by ra.rsaid asc;

-- Datos Resoluciones Liquidaciones
cursor cResLiq(vLiqid number, vEsmid alba.escritosmultas.esmid%type) is
select /*RULE*/ 
ra.rsaid, e.esmid, e.liqid, e.esmfescrito fEscrito, t.tidomnombre tipoEscrito,
e.esmfestado fResolucion, tr.tdmnombre tipoResultado, ra.algid detalleresolucion,
nvl(tr.tdminforme,t.tidominforme) informe,
ra.rsamotivoresol txtAlegacion, r.resdescr motivo,
e.esmcaja caja, e.esmlote lote, e.esmindice indice,
ra.rsaresolucion txtResolucion
from alba.tipos_doc_multas t, alba.tipos_doc_multas_res tr,
alba.resoluciones r, alba.resol_alegaciones ra,
alba.escritosmultas e, alba.liquidaciones l
where l.liqid=e.liqid
and e.tidomid=t.tidomid
and e.tdmresid=tr.tdmresid
and ra.esmid=e.esmid
and ra.resid=r.resid
and e.liqid=vLiqid
and e.esmid=vEsmid
order by ra.rsaid asc;

--

-- Datos dmultas
cursor cDmul(vMulid alba.multas.mulid%type) is
select /*RULE*/ *
from table(trml.dmultas
((select m.mulxmedida4
from
alba.multas m
where m.mulid=vMulid),0));


-- Puntos Denuncias No Identificación de Conductor
cursor cPuntos(vMulid alba.multas.mulid%type) is
select /*RULE*/
'Puntos='||
to_char(decode(i.infgrado,'L',2,3)*i.puntos) puntosNI
from
alba.infracciones i,
alba.multas m
where m.infid=i.infid
and m.seoidexp=
(
select m2.seoidexp
from alba.multas m2
where m2.mulid=vMulid
)
and m.mulid=
(
select min(m3.mulid)
from alba.multas m3
where m3.seoidexp=m.seoidexp
)
and i.puntos>0 and i.puntos is not null;


cursor cExpteTEA(vEsmid number) is
select esmivalor
from alba.escritosmultasinfo inf where inf.esmid=vEsmid and inf.esmietiqueta='EXPTE. TEA'
and inf.esmiiestado=1 and rownum<2;

cursor cPlenoTEA(vEsmid number) is
select esmivalor
from alba.escritosmultasinfo inf where inf.esmid=vEsmid and inf.esmietiqueta='PLENO'
and inf.esmiiestado=1 and rownum<2;

cursor cAsuntoTEA(vEsmid number) is
select esmivalor
from alba.escritosmultasinfo inf where inf.esmid=vEsmid and inf.esmietiqueta='ASUNTO'
and inf.esmiiestado=1 and rownum<2;

cursor clibroejecuresoltea(vesmid number) is
select to_char(l.lbrnumero) libro, to_char(l.lbrfirma,'dd/mm/yyyy') firma
from alba.libro_resoluciones l,
alba.det_libro_resol d
where d.lbrid=l.lbrid
and d.esmid=vesmid
order by d.dlbid asc;

cursor cRecibo(vMulid number) is
select l.liqnumerorecliquidacion recibo
from
alba.liquidaciones l,
alba.multas m
where m.expidexp=l.expid
and m.mulid=vMulid;

cursor cLiq2(vMulid number) is
select e.notpa, e.fnotpa, e.liqid, e.liquidacion, e.importeLiqTexto,
e.importeLiqNum, e.edictoApremio, e.fSancionFirmeLiq,
e.fFinVoluntariaLiq, e.fInicioEjecutivaLiq, e.fnotsan
from alba.liquidaciones l,table(trml.emultas(vmulid)) e
where e.liqid=l.liqid;

cursor cLiq3(vEsmid number) is
select dacuenta Cobrado,importe+dacuenta total,importe pendiente
from alba.liquidaciones l, alba.escritosmultas e,
table(alba.pkg_calculoimportes.ctd(l.liqid,sysdate)) d
where e.liqid=l.liqid
and d.liqid=l.liqid
and e.esmid=vEsmid;   

cursor cEscritoOrgTEA (vEsmid_id number) is
select to_number(i.esmivalor) esmid
from alba.escritosmultasinfo i
where i.esmid=vEsmid_id
and i.esmietiqueta='esmidorg'
and rownum<2;

cursor cFoto01(vBol number) is
select b.boletin||'.01.jpg' foto
from ALBA.multacar_blob b --where b.tipoboletin='CH'
where b.boletin=vBol
and b.fichero1 is not null
and rownum<2;


cursor cfoto02(vbol number) is
select b.boletin||'.02.jpg' foto
from ALBA.multacar_blob b --where b.tipoboletin='CH'
where b.boletin=vBol
and b.fichero2 is not null
and rownum<2;

cursor cLiqEjecutiva(vMulid number) is
select l.liqid
from 
alba.liquidaciones l,
alba.multas mm
where mm.expidexp=l.expid
and l.liqxestado in (5,6) --Ejecutiva
and mm.seoidexp = 
(select m.seoidexp from alba.multas m where m.mulid=vMulid)
;


-- GLOBAL
type ttabla is table of varchar2(3) index by binary_integer;
tabla ttabla;
vTemp varchar2(100);
vLimitePago date:=null;
vLongitud number:=0;
i number:=0;
vTieneAsteriscos number:=0;
vImporte number:=0;
vClave2 number:=0;
vMulidError number:=0;
vFechaNotificacionDenuncia date:=null;
vLineaError varchar2(200):='';
vFechaNotProvidenciaApremio date:=null;
vNumeroLibroApremio varchar2(20):='';
vFResolucionLibroApremio varchar2(20):='';
vVersionVigente number:=0;
vIntereses number:=0;
vTipoNotificacion number:=0;

vFsanfir date:=null;
vFfinvol date:=null;
vFinieje date:=null;
vTributo varchar2(10):='';

vTipoDenuncia number:=0;
vDomicilioDenunciante varchar2(500):='';

vfmullocal fmul;
vorigendenuncia number:=0;
vIncoacion date:=null;

fot01 varchar2(100);
fot02 varchar2(100);
fot03 varchar2(100);
fot04 varchar2(100);

expteTEA varchar2(100):='';
fescritoTEA varchar2(100):='';
plenoTEA varchar2(100):='';
asuntoTEA varchar2(300):='';


vHdorg VARCHAR2(1200):='';
numreg number:=0;
vTemporalLiq varchar2(100):='';


vCobrado number:=0;
vTotal number:=0;
vPendiente number:=0;

vEsmidTmp number:=0;

-- BLOQUE PRINCIPAL
begin

    -- Inicialización Actos Notificación    
    tabla(3):='1.1';tabla(4):='1.2';tabla(5):='2.1';tabla(6):='1.7';
    tabla(7):='1.8';tabla(8):='1.3';tabla(9):='1.6';tabla(10):='1.4';
    tabla(11):='3.1';tabla(12):='3.2';tabla(13):='3.7';tabla(14):='3.8';
    tabla(15):='3.3';tabla(16):='3.6';tabla(177):='1.5';tabla(178):='1.9';
    tabla(179):='4.1';tabla(180):='4.2';tabla(377):='4.3';tabla(457):='4.6';
    tabla(398):='5.0';tabla(157):='1.5';tabla(57):='4.9';tabla(537):='3.4';
    tabla(817):='5.6';tabla(917):='2.1';tabla(937):='7.0';tabla(77):='4.9';
    tabla(1137):='5.1';tabla(1157):='5.2';tabla(1177):='5.3';tabla(1198):='8.0';
    tabla(1257):='1.1'; tabla(1419):='8.1';
  

    select decode(c.atoid,57,2,77,2,817,3,937,1,1198,1,1419,1,1) clave into vClave2
    from alba.cabremesas c where c.crmid=vCrmid;
    
    dbms_output.put_line('OK');
    
    numReg:=0;
    
    -- recorre remesas
    for v in cRem(vClave2) loop ---------------------(1)

              dbms_output.put_line('OK2');


            vFmul:=vFmulLocal;
            iniciarVariableFmul();

            vMulidError:=v.mulid;
            
            numReg:=numReg+1;
            dbms_output.put_line('Registro Número: ' || numReg || '. Mulid: ' || v.mulid);
            
            -- Obtiene Datos Temporales de la Función de Estado
            select count(est.mulid) into vVersionVigente
            from table(trml.emultas(v.mulid)) est;
            

            if vVersionVigente>0 then
            
                    select est.origendenuncia, est.flibinc into vOrigenDenuncia, vIncoacion
                    from table(trml.emultas(v.mulid)) est;
            

                    vLineaError:='Inicia Variables';
                    dbms_output.put_line(vLineaError);

                    vFechaNotificacionDenuncia:=null;

                    --vFmul.nifD:=r.dni;
                    --vFmul.nombreD:=r.nombre;


                    vLineaError:='Inicia Datos Dependientes del Tipo De Denuncia';
                    --dbms_output.put_line(vLineaError);
                    --
                    --(3.*)
                    -- Clave2=1 » Notificación Denuncia Voluntaria
                    if vClave2=1 then

                            if v.atoid in (11,12,13,14,15,16,179,180,377,457,537) then

                                vLineaError:='Inicia Fecha Notificación Denuncia » 1';
                                --dbms_output.put_line(vLineaError);

                                select est.fnotden
                                into vFechaNotificacionDenuncia
                                from table(trml.emultas(v.mulid)) est;

                                vLineaError:='Inicia Fecha Notificación Denuncia » 2';
                                --dbms_output.put_line(vLineaError);

                                --vFmul.fNotDenunica:='NOTIFICACIÓN DE DENUNCIA: '||to_char(v.fecnotden,'dd/mm/rrrr');
                                if vFechaNotificacionDenuncia is not null then

                                    vLineaError:='Inicia Fecha Notificación Denuncia » 3';
                                    --dbms_output.put_line(vLineaError);
                                    vFmul.fNotDenunica:='NOTIFICACIÓN DE DENUNCIA: '||to_char(vFechaNotificacionDenuncia,'dd/mm/rrrr');
                                else

                                    vLineaError:='Inicia Fecha Notificación Denuncia » 4';
                                    --dbms_output.put_line(vLineaError);
                                    vFmul.fNotDenunica:='';
                                end if;


                                vLineaError:='Inicia Importes';
                                --dbms_output.put_line(vLineaError);

                                vImporte:=(round(v.mulimp,2))*100;
                                vFmul.firma:='Victoria Eugenia Guerle Lara';
                                vFmul.tiempoRetirada:='SANCIÓN IMPUESTA: '||to_char(v.mulimp,'9G999D99');
                            else
                                vFmul.fNotDenunica:='';
                                vImporte:=(round(v.mulimp*(70/100),2))*100;
                                
                                if vOrigenDenuncia in (4,6,7,10) then    --decode(m.mulxboletin,144,1,303,2,403,0,444,3,423,4) origenDenuncia
                                    
                                    -- IMPORTE SIN REDUCCIÓN
                                    vImporte:=(round(v.mulimp*(80/100),2))*100;
                                    vfmul.firma:='Iluminada Cano Remesal';
                                    
                                elsif  vOrigenDenuncia=5 then
                                    
                                    vImporte:=(round(v.mulimp*(50/100),2))*100;
                                    vfmul.firma:='Filomena Cruz Villarino';
                                    
                                    if v.atoid=9 then --1.6 no lleva reducción
                                      vimporte:=(round(v.mulimp,2))*100;
                                    end if;
                                    
                                elsif  vOrigenDenuncia=8 then
                                    
                                    vImporte:=(round(v.mulimp*(70/100),2))*100;
                                    vfmul.firma:='Filomena Cruz Villarino';
                                    
                                else
                                
                                    vfmul.firma:='Filomena Cruz Villarino';
                                
                                end if;                                
                                
                                vFmul.tiempoRetirada:='SANCIÓN PROPUESTA: '||to_char(v.mulimp,'9G999D99');                                                                    
                                

                            end if;
                            vFmul.mulid:=v.mulid;
                            vMulidError:=v.mulid;

                    -- Clave2=2 » Providencia de Apremio
                    else
                            --vFmul.fNotDenunica:='NOTIFICACIÓN DE DENUNCIA: '||to_char(v.fecnotden,'dd/mm/rrrr');

                            select est.fnotden, est.fnotpa,
                            est.numeroLibroApremio, est.fResolucionLibroApremio,
                            ffirmeza, finivol, ffinvol+1
                            into vFechaNotificacionDenuncia, vFechaNotProvidenciaApremio,
                            vNumeroLibroApremio, vFResolucionLibroApremio,
                            vFsanfir,vFfinvol,vFinieje
                            from table(trml.emultas(v.mulid)) est where est.fnotden is not null;
                            

                            -- Obtiene Intereses
                            if vClave2=2 then
                                select dintereses into vIntereses from table(alba.pkg_calculoimportes.ctd(v.liqid,sysdate));
                                if vIntereses is null then
                                    vIntereses:=0;
                                end if;
                                vImporte:=(round(v.importeTotalLiq+vIntereses,2))*100;
                            else
                                vImporte:=(round(v.importeTotalLiq,2))*100;
                            end if;

                            vFmul.fNotDenunica:='NOTIFICACIÓN DE DENUNCIA: '||to_char(vFechaNotificacionDenuncia,'dd/mm/rrrr');
                            vFmul.firma:='Cristina Iglésias Puértolas';
                            vFmul.tiempoRetirada:='';
                            vFmul.mulid:=v.liqid;
                            

                            
                    end if;
                    --

                    vLineaError:='Decodifica Expediente y Tipo Notificación » Expid/Atoid'|| v.expidexp || '/' || v.atoid;
                    --dbms_output.put_line(vLineaError);

                    vFmul.expediente:=v.expidexp;
                    vfmul.tiponot:=tabla(v.atoid);
                    if vOrigenDenuncia in (4,5,6,7,8,9,10) and v.atoid not in (1198,1419) then    --decode(m.mulxboletin,144,1,303,2,403,0,444,3,423,4,465,7,464,6,543,8) origenDenuncia
                    
                         /* Tipo específico para denuncias de semáforo en rojo
                         if v.origendenuncia=8 and v.atoid in (917) then
                              vFmul.TipoNot:=vFmul.TipoNot||'.'||to_char(v.origenDenuncia);                         
                         else
                              vFmul.TipoNot:=vFmul.TipoNot||'.'||to_char(vOrigenDenuncia);                         
                         end if;
                         */
                         
                         vFmul.TipoNot:=vFmul.TipoNot||'.'||to_char(vOrigenDenuncia);                         
                    
                    end if;
                    

                    -- En resoluciones de Rec.P.A. estimatorias el tipo de notificación es 5.7.
                    select est.rpa
                    into vTipoNotificacion
                    from table(trml.emultas(v.mulid)) est;
                    
                    if vTipoNotificacion is not null and vFmul.TipoNot='5.6' and vTipoNotificacion=3 then
                        vFmul.TipoNot:='5.7';
                    end if;


                    vLineaError:='Establece Límite de Pago';
                    --dbms_output.put_line(vLineaError);

                    if vClave2=1 then
                            --vLimitePago:=to_date((v.fechaEmision+50),'dd/mm/rrrr');
                            vLimitePago:=to_date(sysdate+50,'dd/mm/rrrr');
                    else
                            --vLimitePago:=to_date(v.flimitepago,'dd/mm/rrrr');
                            --vLimitePago:=to_date(sysdate+50,'dd/mm/rrrr');
                            vLimitePago:=v.flimitepago;
                    end if;

                    vFmul.fLimitePago:='HASTA: ' || to_char(vLimitePago,'dd/mm/rrrr');
                    vFmul.Emisora:='410913';


                    vLineaError:='Establece Referencia Cuaderno 60';
                    --dbms_output.put_line(vLineaError);

                    if vClave2=1 then
                            vFmul.Referencia:=to_char(v.mulfec,'yy') || lpad(v.mulid,8,0);
                            vTributo:='1064';
                    else
                            vFmul.Referencia:=substr(v.recibo,3,10);
                            vTributo:='1964';
                            
                            -- Resolución Recurso Contra la Providencia de Apremio
                            -- Se genera documento cobratorio tras la creación de las
                            -- propuestas de notificación
                            if vTipoNotificacion is not null
                            and (vFmul.TipoNot='5.6' or vFmul.TipoNot='5.7' or vFmul.TipoNot='7.0') then
                                vFmul.Referencia:=substr(v.recibo,3,10);
                                vTributo:='1999';
                            end if;
                            
                    end if;


                    vLineaError:='Establece Resto Campos Cuaderno 60';
                    --dbms_output.put_line(vLineaError);

                    vFmul.Identificacion:=vTributo||to_char(v.fechaEmision,'yy')||to_char(vLimitePago,'Y')||to_char(vLimitePago,'DDD');
                    vFmul.Importe:=to_char(vImporte);
                    vTemp:=TO_CHAR(FLOOR(((((to_number(vFmul.Emisora))*76)+((to_number(vFmul.Referencia))*9)+
                    ((to_number(vFmul.Identificacion)+(vImporte)-1)*55))/97)*100));
                    vFmul.DC:=lpad(to_char(99-to_number(substr((vTemp),(length(vTemp)-1),2))),2,0);


                    vLineaError:='Establece Resto Campos Cuaderno 60';
                    --dbms_output.put_line(vLineaError);

                    vFmul.nExpediente:='EXPEDIENTE: '||v.expcod;
                    vfmul.boletin:='BOLETIN: '||v.mulnumbol;
                    --vFmul.fIncoacion:='FECHA INCOACION: '||to_char(v.mulfeciniprod,'dd/mm/rrrr');
                    vFmul.fIncoacion:='FECHA INCOACION: '||to_char(nvl(vIncoacion,v.mulfec),'dd/mm/rrrr');
                    vFmul.Lugar:=substr('LUGAR: '||v.lugar,1,300);
                    vFmul.fDenuncia:='FECHA DENUNCIA: '||to_char(v.mulfec,'dd/mm/rrrr');
                    vFmul.Hora:='HORA DENUNCIA: '||v.hora;


                    vLineaError:='Localiza Datos de Matrícula';
                    --dbms_output.put_line(vLineaError);
                    
                    if v.atoid<>9 then
                        for m in cVeh(v.mulid) loop ---------------------(1)
                          vFmul.Matricula:='MATRICULA: ' || m.matricula;
                          vFmul.Vehiculo:='VEHICULO: ' || m.tipo || ' ' || m.marca;
                        end loop;
                    end if;
                    

                    vLineaError:='Inicia Datos Denunciado';
                    --dbms_output.put_line(vLineaError);

                    vFmul.nifD:=v.dni;
                    vFmul.nombreD:=v.nombre;


                    vLineaError:='Inicia Domicilio Denunciado';
                    --dbms_output.put_line(vLineaError);
                    
                    -- Elimina Asteríscos
                    select length(v.direccion),decode(substr(v.direccion,1,1),'*',1,0),1 into vLongitud,vTieneAsteriscos,i from dual;
                    WHILE i<vLongitud and vTieneAsteriscos=1 LOOP
                       i:=i+1;
                         select decode(substr(v.direccion,i,1),'*',1,0) into vTieneAsteriscos from dual;
                    END LOOP;

                    select substr(substr(v.direccion,i,vLongitud),1,500) into vFmul.domicilioD from dual;
                    --vFmul.domicilioD:=v.direccion;
                    -- Fin Elimina Asteríscos
            

                    vLineaError:='Inicia Resto Domicilio Denunciado. Mulid=' || v.mulid;
                    --dbms_output.put_line(vLineaError);
                    
                    vLineaError:='Inicia CP';
                    vFmul.cpD:=v.codigopostal;
                    vLineaError:='Inicia Municipio';
                    vFmul.localidadD:=v.municipio;
                    vLineaError:='Inicia Provincia';
                    vFmul.provinciaD:=v.provincia;
                    vLineaError:='Inicia Denunciante';
                    vFmul.denunciante:=v.denunciante;


                    vLineaError:='Inicia Datos Denunciante';
                    
                    --if v.atoid<>9 then
                    --if v.HayDenunciante=1 then
                    
                            select m.mulxboletin
                            into vTipoDenuncia
                            from alba.multas m
                            where m.mulid=v.mulid;
                            
        
                            if v.HayDenunciante=0  and vTipoDenuncia=303 and v.atoid<>9 then--ORA
        
                                select substr(nvl(p.periden, p.peridenext) || ' ' ||
                                        alba.componer_nombre (p.pernombre, p.perapell1, p.perpart1, p.perapell2, p.perpart2, p.perrazon) || '. ' ||
                                        'CL FEDERICO SÁNCHEZ BEDOYA 2 41001 SEVILLA',1,260)
                                into vFmul.denunciante
                                from alba.expedientesingresoscontr  e, alba.decodificadora d,
                                     alba.tipospersona t, alba.personas p, alba.contribuyentes c, alba.multas m
                                where e.eicxtipocontribuyente=d.decid
                                and e.conid=c.conid
                                and c.perid=p.perid
                                and p.tpeid=t.tpeid
                                and m.expidexp=e.expid
                                and p.perversion in
                                    (select max(pp.perversion) from alba.personas pp where pp.perid = p.perid)
                                --and m.expidexp=(select max(se.expid) from seobjetos se where se.seoid=m.seoidexp)
                                and m.mulid=v.mulid
                                and d.decelemento='DENUNCIANTE'
                                and rownum <2;
                                
                            elsif v.HayDenunciante=0  and vTipoDenuncia=144 and v.mulxmedida4 is null and v.atoid<>9 
                            then --TRAFICO
                            
                                    -- Persona
                                    select substr(nvl(p.periden, p.peridenext) || ' ' ||
                                    alba.componer_nombre (p.pernombre, p.perapell1, p.perpart1, p.perapell2, p.perpart2, p.perrazon),1,260)
                                    into vFmul.denunciante
                                    from alba.expedientesingresoscontr  e, alba.decodificadora d,
                                    alba.tipospersona t, alba.personas p, alba.contribuyentes c, alba.multas m
                                    where e.eicxtipocontribuyente=d.decid
                                    and e.conid=c.conid
                                    and c.perid=p.perid
                                    and p.tpeid=t.tpeid
                                    and m.expidexp=e.expid
                                    and p.perversion in
                                    (select max(pp.perversion) from alba.personas pp where pp.perid = p.perid)
                                    --and m.expidexp=(select max(se.expid) from seobjetos se where se.seoid=m.seoidexp)
                                    and m.mulid=v.mulid
                                    and d.decelemento='DENUNCIANTE'
                                    and rownum <2;
        
                                    vDomicilioDenunciante:='';
                                    
                                    -- Direccion
                                    select substr(domicilio,1,260)
                                    into vDomicilioDenunciante
                                    from (
                                    SELECT decper.decelemento cont,alba.componer_direccion
                                     (acc.accnum1, acc.accinum1, acc.accnum2, acc.accinum2, acc.acckm1,acc.accikm1, acc.accdirnoest,
                                      rdi.rdixpor, rdi.rdixesc, rdi.rdixpiso, rdi.rdixpuerta, rdi.rdiipuertadup, rdi.rdidirnoest,
                                      tvi.tvicodigo, via.viadesc, rdi.rdiref3, rdi.rdirefcon1, rdi.rdirefcon2, acc.codid, cod.codposcodigo) || ' · ' ||
                                      nvl(cod.CODPOSCODIGO, '') || ' · ' ||  mu.mundesc || ' · ' ||  p.mprdesc domicilio,
                                      decdir.decelemento tipo_direc, rdi.rdiid id,
                                        decode(decdir.decelemento,'NOTIFICACION',1,'DIR PADRON',2,'FISCAL',3,'OBJETO',4,5) orden
                                    FROM alba.expedientesingresoscontr eic, alba.direccionesexpedientes ted,
                                         alba.decodificadora decdir, alba.decodificadora decper, alba.direccion rdi,
                                         alba.tvia tvi, alba.vias via, alba.accesos acc, alba.municipios mu,
                                         alba.m_codpostal cod, alba.provincia p, alba.multas m
                                    WHERE eic.eicxtipocontribuyente = decper.decid
                                    AND ted.eicid = eic.eicid
                                    AND ted.tedxtipodireccion = decdir.decid
                                    AND ted.rdiid = rdi.rdiid
                                    AND rdi.accid = acc.accid
                                    AND acc.viaid = via.viaid
                                    AND via.tviid = tvi.tviid
                                    AND eic.expid = m.expidexp
                                    AND via.MunId = mu.MunId
                                    AND mu.mprid = p.mprid
                                    AND acc.codid = cod.codid (+)
                                    and m.mulid=v.mulid
                                    and eic.eicxtipocontribuyente=329)
                                    where rownum<2
                                    order by orden, id desc;
                                    
                                    if vDomicilioDenunciante is not null and vDomicilioDenunciante<>'' and v.atoid<>9 then
                                    
                                        vFmul.denunciante:=substr(vFmul.denunciante||vDomicilioDenunciante,1,260);
        
                                    end if;
                                
                            end if;
                            
                            if v.HayDenunciante=0 and vTipoDenuncia in (464,623) then
                            
                                    -- Persona
                                    select substr(nvl(p.periden, p.peridenext) || ' ' ||
                                    alba.componer_nombre (p.pernombre, p.perapell1, p.perpart1, p.perapell2, p.perpart2, p.perrazon),1,260)
                                    into vFmul.denunciante
                                    from alba.expedientesingresoscontr  e, alba.decodificadora d,
                                    alba.tipospersona t, alba.personas p, alba.contribuyentes c, alba.multas m
                                    where e.eicxtipocontribuyente=d.decid
                                    and e.conid=c.conid
                                    and c.perid=p.perid
                                    and p.tpeid=t.tpeid
                                    and m.expidexp=e.expid
                                    and p.perversion in
                                    (select max(pp.perversion) from alba.personas pp where pp.perid = p.perid)
                                    --and m.expidexp=(select max(se.expid) from seobjetos se where se.seoid=m.seoidexp)
                                    and m.mulid=v.mulid
                                    and d.decelemento='DENUNCIANTE'
                                    and rownum <2;

                            
                            end if;
        
                            if vFmul.denunciante is not null and v.atoid<>9 then
                                vFmul.denunciante:=substr('DENUNCIANTE: ' || vFmul.denunciante,1,300);
                            end if;
                            
                    --end if;


                    vLineaError:='Inicia Motivo No Entrega';

                    -- Motivo No Notificación
                    vFmul.motivoNoEntrega:=leerMotivoNoNotificacion(v.mulid);

                    if vFmul.motivoNoEntrega is not null then
                        vFmul.motivoNoEntrega:='MOTIVO DE NO ENTREGA EN EL ACTO: '||vFmul.motivoNoEntrega;
                    end if;
                    vFmul.motivoNoEntrega:=replace(replace(vFmul.motivoNoEntrega,CHR (13),''),CHR (10),'');


                    vLineaError:='Inicia Normativa';

                    vFmul.hecho:=substr(v.hd,1,250);
                    vFmul.normativa:='NORMATIVA APLICABLE: '|| upper(v.NORMA)||'. ART. ' || upper(v.ARTICULO) ||' APDO. '|| upper(v.APARTADO);
                    if vTipoDenuncia not in (423,444,464,623) then
                      vFmul.calificacion:='CALIFICACIÓN JURÍDICA: '||v.GRADO||'  '||v.puntos;
                    else --No lleva anotación de puntos
                      vFmul.calificacion:='CALIFICACIÓN JURÍDICA: '||v.GRADO;
                    end if;
                    --vFmul.tiempoRetirada:='SANCIÓN PROPUESTA: '||to_char(v.mulimp,'9G999D99');


                    vLineaError:='Inicia Sicer';

                    vFmul.cbsicer:=v.sicer;


                    vLineaError:='Inicia C60';

                    vFmul.cbcuaderno60:='90521'||vFmul.Emisora||vFmul.Referencia||vFmul.DC||vFmul.Identificacion||LPAD((vImporte),8,0)||'0';


                    vLineaError:='Inicia Datos 1.6';

                    --(1.6)
                    if v.atoid=9 then
                    
                    
                                    
                            /*for pt in cPuntos(v.mulid) loop
                            
                                  vFmul.normativa:='NORMATIVA APLICABLE: '|| upper(v.NORMA)||'. ART. ' || upper(v.ARTICULO) ||' APDO. '|| upper(v.APARTADO);
                                  vFmul.calificacion:='CALIFICACIÓN JURÍDICA: '||v.GRADO||'  '||pt.puntosNI;   
                            
                            end loop;*/
                            
                            vFmul.nifT:='';
                            vFmul.nombreT:='';
                            vFmul.domicilioT:='';
                            vFmul.cpT:='';
                            vFmul.localidadT:='';
                            vFmul.provinciaT:='';
                            vFmul.lugarOrg:='';
                            vFmul.fechaOrg:='';
                            vFmul.horaOrg:='';
                            vFmul.matriculaOrg:='';
                            vFmul.hechoOrg:='';
                            vFmul.motivonoentrega:='';
                            vFmul.Lugar:='';
                            vFmul.hora:='';
                            vFmul.fIncoacion:='FECHA INCOACION: '||to_char(nvl(vIncoacion,v.mulfec),'dd/mm/rrrr');

                            /*for h in cDmul(v.mulid) loop
                              vFmul.lugarOrg:=h.lugar;
                                vFmul.fechaOrg:=h.fdenuncia;
                                vFmul.horaOrg:=h.hora;
                                vFmul.matriculaOrg:=h.matricula;
                                vFmul.hechoOrg:=substr(h.hecho || '. ' || h.normativa,1,400);
                            end loop;*/
                            
                            
                                                        


                                  select
                                  'FECHA: '||
                                  m.mulfec||
                                  ' HORA: '||
                                  lpad(m.mulhor,2,0) || ':' || lpad(m.mulmin,2,0) ||
                                  ' MATRÍCULA: '||
                                  m.mulmatri ||
                                  ' IMPORTE: '||
                                  m.mulimp ||
                                  ' LUGAR: '||
                                  upper(nvl(ltrim(rtrim(tvi.tvidesc || ' ' || via.viadesc)), nvl(m.mullugar,''))
                                          || decode (nvl(m.muldelante,''),'','',' Delante de ' || nvl(m.muldelante,''))
                                          || decode (nvl(m.mulfrente,''),'','',' Frente a ' || nvl(m.mulfrente,''))
                                          || decode (nvl(m.mulobsdir,''),'','',' Con dirección a ' || nvl(m.mulobsdir,''))) ||
                                  ' HECHO DENUNCIADO: '||        
                                  m.muldenuncia ||
                                  ' ART: '||
                                  i.infarticulo ||
                                  ' APTDO: '||
                                  i.infapartado ||
                                  ' GRADO: '||
                                  decode(i.infgrado,'L','LEVE','G','GRAVE','M','MUY GRAVE') ||
                                  ' NORMA: '||
                                  di.decdescr ||
                                  ' '||i.puntosTXT 
                                  into vHdorg
                                   
                                  from
                                  alba.decoingresos di,
                                  alba.infracciones i,
                                  alba.multas m,
                                  alba.vias via,
                                  alba.tvia tvi
                                  
                                  where m.infid=i.infid
                                  and m.viaid = via.viaid (+)
                                  and via.tviid = tvi.tviid (+)
                                  and i.infxlegislacion = di.decid (+)
                                  and m.seoidexp=
                                  (
                                  select m2.seoidexp
                                  from alba.multas m2
                                  where m2.mulid=v.mulid
                                  )
                                  and m.mulid=
                                  (
                                  select min(m3.mulid)
                                  from alba.multas m3
                                  where m3.seoidexp=m.seoidexp
                                  );
                            
                            vFmul.hechoOrg:=vHdorg;
                    end if;


                    vLineaError:='Inicia Datos 1.8,1.9,3.8';

                    --(1.8,1.9,3.8)
                    if v.velocidad is not null then
                        vFmul.velocidad:='VELOCIDAD: ' || v.velocidad;
                    end if;
                    vFmul.limiteVelocidad:='';
                    vFmul.textoCinemometro:='';


                    vLineaError:='Inicia Datos 1.7,3.7';

                    --(1.7,3.7)
                    if v.atoid in (6,13) then
                        vFmul.alcoholAire:='ALCOHOL EN AIRE: '||v.alcoholEnAire;
                        vFmul.alcoholSangre:='';
                    else
                        vFmul.alcoholAire:='';
                        vFmul.alcoholSangre:='';
                    end if;


                    vLineaError:='Inicia Datos 1.6';

                    --Datos Ejecutiva
                    if vClave2=1 then
                        vFmul.recibo:='';
                        vFmul.sancionFirme:='';
                        vFmul.finVoluntaria:='';
                        vFmul.inicioEjecutiva:='';
                        vFmul.importeSancionFirme:='';
                        vFmul.providenciaApremio:='';
                        vFmul.intentoNotPA:='';
                        vFmul.edictoPA:='';
                        vFmul.bopPa:='';
                        vFmul.numeroLibroApremio:='';
                        vFmul.fResolucionLibroApremio:='';
                    else

                        vFmul.recibo:='RECIBO: '|| v.recibo || '  RELACIÓN DE DEUDORES: ' || v.rd;
                        

                        if vClave2=2 then -- Importe Notificación Providencia de Apremio

                            -- Fechas Firmeza » ORIGEN LIQUIDACIÓN
                            -- En el caso de Notif. de Providencia de Apremio
                            -- imrpime los datos de la liquidación
                            vFmul.sancionFirme:='FECHA SANCIÓN FIRME: ' || v.fsanfir;
                            vFmul.finVoluntaria:='FECHA FIN VOLUNTARIA: ' || v.ffinvol;
                            vFmul.inicioEjecutiva:='FECHA INICIO EJECUTIVA: ' || v.finieje;

                            vFmul.importeSancionFirme:='SANCIÓN FIRME:    '||
                                                       to_char(v.liqimporte,'9990.99')||
                                                       '¿@RECARGO:          '||
                                                       to_char(v.liqapremio,'9990.99')||
                                                       '¿@INTERESES:        '||
                                                       to_char(vIntereses,'9990.00')||
                                                       '¿@COSTAS:           '||
                                                       to_char(v.liqcostas,'9990.99')||
                                                       '¿@SUMA:             '||
                                                       to_char(vImporte/100,'9990.99');

                            vFmul.providenciaApremio:='';
                            vFmul.intentoNotPA:='';
                            vFmul.edictoPA:='';
                            vFmul.bopPa:='';
                            vFmul.numeroLibroApremio:='';
                            vFmul.fResolucionLibroApremio:='';
                            
                           

                        else -- Importe Notificación Resolución Recurso Contra Providencia de Apremio
                        
                            -- Fechas Firmeza » ORIGEN TRAMITACIÓN
                            -- En el caso de Notif. de Resol. de Rec. contra la P.A.
                            -- imprime los datos del cálculo de estado
                            vFmul.sancionFirme:='FECHA SANCIÓN FIRME: ' || vFsanfir;
                            vFmul.finVoluntaria:='FECHA FIN VOLUNTARIA: ' || vFfinvol;
                            vFmul.inicioEjecutiva:='FECHA INICIO EJECUTIVA: ' || vFinieje;
                            

                            vFmul.importeSancionFirme:='SANCIÓN FIRME:    '||
                                                       to_char(v.liqimporte,'9990.99')||
                                                       '¿@RECARGO:          '||
                                                       to_char(v.liqapremio,'9990.99')||
                                                       '¿@INTERESES:        '||
                                                       to_char(vIntereses,'9990.00')||
                                                       '¿@COSTAS:           '||
                                                       to_char(v.liqcostas,'9990.99')||
                                                       '¿@SUMA:             '||
                                                       to_char(vImporte/100,'9990.99');

                            vFmul.providenciaApremio:='NOTIFICACIÓN PROVIDENCIA DE APREMIO: '||to_char(vFechaNotProvidenciaApremio,'dd/mm/rrrr');
                            vFmul.intentoNotPA:='';
                            vFmul.edictoPA:='';
                            vFmul.bopPa:='';
                            vFmul.numeroLibroApremio:=vNumeroLibroApremio;
                            vfmul.fresolucionlibroapremio:=vfresolucionlibroapremio;
                            vFmul.tiempoRetirada:=to_char(v.mulimp,'9G999D99');

                        end if;


                    end if;
                    
                    
                    --Ejecución TEA
                    vLineaError:='Inicia Ejecución TEA';
                    if v.atoid in (1198,1419) then
                          for ld in cLiq2(v.mulid) loop
                              
                              --vDatos.liquidacion:=ld.liquidacion;
                              --vDatos.sancion:=ld.importeLiqTexto;
                              select dacuenta,importe+dacuenta,importe
                              into vCobrado, vTotal, vPendiente
                              from table(alba.pkg_calculoimportes.ctd(ld.liqid,sysdate));
                              
                              /*
                              for libe in clibroejecuresoltea(v.esmid) loop

                                       vfmul.importesancionfirme:=
                                       'LIBRO Nº: '||
                                       libe.libro||
                                       '@FECHA LIBRO: '||
                                       libe.firma||'@';                       
                              
                              end loop;
                              */

                              
                              vfmul.importesancionfirme:=
                              vfmul.importesancionfirme || 'TOTAL:    '||
                                                       to_char(vTotal,'9990.99')||
                                                       '@COBRADO:          '||
                                                       to_char(vCobrado,'9990.99')||
                                                       '@PENDIENTE:        '||
                                                       to_char(vPendiente,'9990.00');
                              
                              -- Datos Origen Liquidación
                              vFmul.sancionFirme:='FECHA SANCIÓN FIRME: ' || ld.fSancionFirmeLiq;
                              vfmul.finvoluntaria:='FECHA FIN VOLUNTARIA: ' || ld.fFinVoluntariaLiq;
                              vfmul.inicioejecutiva:='FECHA INICIO EJECUTIVA: ' || ld.finicioejecutivaliq;
                              vFmul.providenciaApremio:='NOTIFICACIÓN PROVIDENCIA DE APREMIO: '|| to_char(ld.fnotpa,'dd/mm/rrrr');
                              
                              --vDatos.edictoApremio:=ld.edictoApremio;
                              --vDatos.fnotsan:=ld.fnotsan;
                              vFmul.tiempoRetirada:=to_char(v.mulimp,'9G999D99');

                              
                          end loop;
                    end if;
                    
                    if v.atoid in (1419) then
                        for x in cRecibo(v.mulid) loop
                            vFmul.recibo:='RECIBO: '|| x.recibo;
                        end loop;                        
                    end if;
                    
                    
                    vLineaError:='Inicia Denuncias de Convivencia';
                    -- Denuncias de Convivencia
                    if v.origenDenuncia=4 then    --decode(m.mulxboletin,144,1,303,2,403,0,444,3,423,4,465,7) origenDenuncia
                        
                        vfmul.matricula:='';
                        vfmul.vehiculo:='';
                      vfmul.motivoNoEntrega:='';                      
                    
                    end if;
                    
                    vLineaError:='Inicia Denuncias FOTOS';
                    -- FOTOS
                    if v.atoid in (1257,917) then
                    
                        vFmul.TipoNot:=vFmul.TipoNot||'.F';
                        
                        for cf in cfoto01(v.mulnumbol) loop
                          vFmul.fot01:=cf.foto;
                        end loop;
                        
                        for cf in cfoto02(v.mulnumbol) loop
                          vFmul.fot02:=cf.foto;
                        end loop;
                        
                        --vFmul.fot01:=replace(fot01,'Doc_Luz_Roja\','');
                        --vFmul.fot02:=replace(fot02,'Doc_Luz_Roja\','');
                        --vFmul.fot03:=replace(fot03,'Doc_CentroHistorico\','');
                        --vFmul.fot04:=replace(fot04,'Doc_CentroHistorico\','');
                        
                        
                        --vFmul.Lugar:=substr('VÍA DE SALIDA: '||v.lugar,1,300);
                        --vFmul.fDenuncia:='FECHA DE SALIDA: '||to_char(v.mulfec,'dd/mm/rrrr');
                        --vFmul.Hora:='HORA DE SALIDA: '||v.hora;
                        --vFmul.denunciante:=replace(vFmul.denunciante,'DENUNCIANTE:','IDENTIFICACIÓN DE LA CÁMARA DE CAPTACIÓN DE IMAGEN Nº: ');
                        --vFmul.hecho:=substr(v.hd||v.obs,1,400);
                        --vFmul.normativa:=vFmul.normativa||' LEY DE TRÁFICO: ART.53. NORMAS DE GRADUACIÓN DE SANCIONES DE TRÁFICO DEL AYUNTAMIENTO DE SEVILLA DE FECHA 22 DE OCTUBRE DE 2010: Nº 393';
                        --vFmul.normativa:=replace(vFmul.normativa,'APDO. 0 ','');
                    end if;
                    
                    
                            
                    --if v.atoid in (937,1198) then
                    if v.atoid in (937) then 
                    
                          vFmul.fNotDenunica:='NOTIFICACIÓN DE DENUNCIA: '||to_char(vFechaNotificacionDenuncia,'dd/mm/rrrr');
                          vfmul.firma:='';
                          --vFmul.tiempoRetirada:='';
                          vFmul.mulid:=v.liqid;
                          vFmul.cbcuaderno60:='';

                          vFmul.providenciaApremio:='';
                          expteTEA:='';
                          plenoTEA:='';
                          asuntoTEA:='';
                          fescritoTEA:='';
                          vEsmidTmp:=null;
                          
                          
                          for ccExpteTEA in cExpteTEA(v.esmid) loop
                            expteTEA:=ccExpteTEA.esmivalor;
                          end loop;
                          
                          for ccPlenoTEA in cPlenoTEA(v.esmid) loop
                            plenoTEA:=ccPlenoTEA.esmivalor;
                          end loop;                          
                          
                          asuntoTEA:='ACTUACION PROVIDENCIA DE APREMIO, DILIGENCIA DE EMBARGO DE CUENTAS BANCARIAS, A PLAZO, EFECTIVO.';
                          
                          for ccAsuntoTEA in cAsuntoTEA(v.esmid) loop
                            asuntoTEA:=ccAsuntoTEA.esmivalor;
                          end loop;                          
                          
                          
                          vEsmidTmp:=v.esmid;
                          
                          -- Recupera escrito original TEA para obtener fecha interposición de escrito
                          if v.atoid in (1198) then
                              for cesctea in cescritoorgtea(v.esmid) loop
                                vEsmidTmp:=cesctea.esmid;
                              end loop;
                          end if;
                          
                          
                          select to_char(e.esmfescrito,'dd/mm/rrrr') into fescritoTEA
                          from alba.escritosmultas e where e.esmid=vEsmidTmp;
                          
                          
                          if expteTEA is not null then
                          
                                                     
                              vFmul.importeSancionFirme:=vFmul.importeSancionFirme ||
                                                     'Rec.Econ.Adtva. Nº: '||
                                                     expteTEA ||
                                                     '@Fecha interposición reclamación: '||
                                                     fescritoTEA||
                                                     '@Pleno: '||
                                                     plenoTEA||                                                     
                                                     '@Asunto: '||
                                                     asuntoTEA||
                                                     '@Órgano que ha dictado el acto:'||
                                                     'TRIBUNAL ECONÓMICO ADMINISTRATIVO';
                                                     
                          else
                              vFmul.importeSancionFirme:=vFmul.importeSancionFirme;                            
                          end if;
                          
                     
                          
                          vFmul.intentoNotPA:='';
                          vFmul.edictoPA:='';
                          vFmul.bopPa:='';
                          vFmul.numeroLibroApremio:='';
                          vFmul.fResolucionLibroApremio:='';       
                          
                          for x in cRecibo(v.mulid) loop
                          
                            vFmul.recibo:=x.recibo;
                          
                          end loop;
                    
                    end if;                    


                    vLineaError:='Inicia Datos Resoluciones';

                    -- RESOLUCIONES
                    if v.atoid in (179,180,377,457,817,937,1198,1419,1137,1157,1177,157) then
                    
                          vLineaError:='Inicia Datos Resoluciones » 1';
                                i:=1;
                              
                                for r in cRes(v.mulid,v.esmid) loop
                              vLineaError:='Inicia Datos Resoluciones » 2 » esmid='||to_char(v.esmid);

                              vFmul.caja:=substr(r.caja,1,10);
                              vFmul.lote:=substr(r.lote,1,10);
                              vFmul.indice:=substr(r.indice,1,10);
                                                                                          
                                    
                            if i=1 then
                              vlineaerror:='Inicia Datos Resoluciones » 3 » esmid='||to_char(v.esmid);
                                        vFmul.a1:=replace(replace(substr(r.txtAlegacion,1,15000), CHR (13), '|'), CHR (10), '|');
                                        vFmul.b1:=replace(replace(substr(r.txtResolucion,1,15000), CHR (13), '|'), CHR (10), '|');
                                        --vFmul.hechoOrg:=substr(r.motivo,1,250);
                                        --vFmul.informe:=r.informe;
                                    elsif i=2 then
                                        vLineaError:='Inicia Datos Resoluciones » 4 » esmid='||to_char(v.esmid);
                                        vFmul.a2:=replace(replace(substr(r.txtAlegacion,1,15000), CHR (13), '|'), CHR (10), '|');
                                        vFmul.b2:=replace(replace(substr(r.txtResolucion,1,15000), CHR (13), '|'), CHR (10), '|');
                                    elsif i=3 then
                                        vLineaError:='Inicia Datos Resoluciones » 5 » esmid='||to_char(v.esmid);
                                        vFmul.a3:=replace(replace(substr(r.txtAlegacion,1,15000), CHR (13), '|'), CHR (10), '|');
                                        vFmul.b3:=replace(replace(substr(r.txtResolucion,1,15000), CHR (13), '|'), CHR (10), '|');
                                    elsif i=4 then
                                        vLineaError:='Inicia Datos Resoluciones » 6 » esmid='||to_char(v.esmid);
                                        vFmul.a4:=replace(replace(substr(r.txtAlegacion,1,15000), CHR (13), '|'), CHR (10), '|');
                                        vFmul.b4:=replace(replace(substr(r.txtResolucion,1,15000), CHR (13), '|'), CHR (10), '|');
                                    elsif i=5 then
                                        vLineaError:='Inicia Datos Resoluciones » 7 » esmid='||to_char(v.esmid);
                                        vFmul.a5:=replace(replace(substr(r.txtAlegacion,1,15000), CHR (13), '|'), CHR (10), '|');
                                        vFmul.b5:=replace(replace(substr(r.txtResolucion,1,15000), CHR (13), '|'), CHR (10), '|');
                                    elsif i=6 then
                                        vLineaError:='Inicia Datos Resoluciones » 8 » esmid='||to_char(v.esmid);
                                        vFmul.a6:=replace(replace(substr(r.txtAlegacion,1,15000), CHR (13), '|'), CHR (10), '|');
                                        vFmul.b6:=replace(replace(substr(r.txtResolucion,1,15000), CHR (13), '|'), CHR (10), '|');
                                    elsif i=7 then
                                        vLineaError:='Inicia Datos Resoluciones » 9 » esmid='||to_char(v.esmid);
                                        vFmul.a7:=replace(replace(substr(r.txtAlegacion,1,15000), CHR (13), '|'), CHR (10), '|');
                                        vFmul.b7:=replace(replace(substr(r.txtResolucion,1,15000), CHR (13), '|'), CHR (10), '|');
                                    elsif i=8 then
                                        vLineaError:='Inicia Datos Resoluciones » 10 » esmid='||to_char(v.esmid);
                                        vFmul.a8:=replace(replace(substr(r.txtAlegacion,1,15000), CHR (13), '|'), CHR (10), '|');
                                        vFmul.b8:=replace(replace(substr(r.txtResolucion,1,15000), CHR (13), '|'), CHR (10), '|');
                                    elsif i=9 then
                                        vLineaError:='Inicia Datos Resoluciones » 11 » esmid='||to_char(v.esmid);
                                        vFmul.a9:=replace(replace(substr(r.txtAlegacion,1,15000), CHR (13), '|'), CHR (10), '|');
                                        vFmul.b9:=replace(replace(substr(r.txtResolucion,1,15000), CHR (13), '|'), CHR (10), '|');
                                    elsif i=10 then
                                        vLineaError:='Inicia Datos Resoluciones » 12 » esmid='||to_char(v.esmid);
                                        vFmul.a10:=replace(replace(substr(r.txtAlegacion,1,15000), CHR (13), '|'), CHR (10), '|');
                                        vFmul.b10:=replace(replace(substr(r.txtResolucion,1,15000), CHR (13), '|'), CHR (10), '|');
                                    elsif i=11 then
                                        vLineaError:='Inicia Datos Resoluciones » 13 » esmid='||to_char(v.esmid);
                                        vFmul.a11:=replace(replace(substr(r.txtAlegacion,1,15000), CHR (13), '|'), CHR (10), '|');
                                        vFmul.b11:=replace(replace(substr(r.txtResolucion,1,15000), CHR (13), '|'), CHR (10), '|');
                                    elsif i=12 then
                                        vLineaError:='Inicia Datos Resoluciones » 14 » esmid='||to_char(v.esmid);
                                        vFmul.a12:=replace(replace(substr(r.txtAlegacion,1,15000), CHR (13), '|'), CHR (10), '|');
                                        vFmul.b12:=replace(replace(substr(r.txtResolucion,1,15000), CHR (13), '|'), CHR (10), '|');
                                    elsif i=13 then
                                        vLineaError:='Inicia Datos Resoluciones » 15 » esmid='||to_char(v.esmid);
                                        vFmul.a13:=replace(replace(substr(r.txtAlegacion,1,15000), CHR (13), '|'), CHR (10), '|');
                                        vFmul.b13:=replace(replace(substr(r.txtResolucion,1,15000), CHR (13), '|'), CHR (10), '|');
                                    elsif i=14 then
                                        vLineaError:='Inicia Datos Resoluciones » 16 » esmid='||to_char(v.esmid);
                                        vFmul.a14:=replace(replace(substr(r.txtAlegacion,1,15000), CHR (13), '|'), CHR (10), '|');
                                        vFmul.b14:=replace(replace(substr(r.txtResolucion,1,15000), CHR (13), '|'), CHR (10), '|');
                                    elsif i=15 then
                                        vLineaError:='Inicia Datos Resoluciones » 17 » esmid='||to_char(v.esmid);
                                        vFmul.a15:=replace(replace(substr(r.txtAlegacion,1,15000), CHR (13), '|'), CHR (10), '|');
                                        vFmul.b15:=replace(replace(substr(r.txtResolucion,1,15000), CHR (13), '|'), CHR (10), '|');
                                    end if;
                                    
                                    i:=i+1;
                            end loop;
                    end if;
                    
                    if v.atoid in (937,1198,1419) then
                    
                      vFmul.a1:=''; vFmul.a2:=''; vFmul.a3:=''; vFmul.a4:=''; vFmul.a5:='';
                      vFmul.a6:=''; vFmul.a7:=''; vFmul.a8:=''; vFmul.a9:=''; vFmul.a10:='';
                      vFmul.a11:=''; vFmul.a12:=''; vFmul.a13:=''; vFmul.a14:=''; vFmul.a15:='';
                      
                      vFmul.peridT :=0; vFmul.perversionT :=0; vFmul.rdiidT :=0;
                      --
                      vFmul.fLimitePago :=''; vFmul.Emisora :=''; vFmul.Referencia :='';
                      vFmul.DC :=''; vFmul.Identificacion :=''; vFmul.Importe :='';
                      vFmul.Importe2 :=0; vFmul.Importe3 :=''; vFmul.Importe4 :=0;
                      --
                      vFmul.fIncoacion :=''; vFmul.Lugar :=''; vFmul.fDenuncia :='';
                      vFmul.Hora :=''; vFmul.Matricula :=''; vFmul.Vehiculo :='';
                      --
                      vFmul.denunciante :=''; vFmul.motivoNoEntrega :=''; vFmul.hecho :='';
                      vFmul.normativa :=''; vFmul.calificacion :=''; vFmul.tiempoRetirada :='';
                      --
                      vFmul.nifT :=''; vFmul.nombreT :=''; vFmul.domicilioT :='';
                      vFmul.cpT :=''; vFmul.localidadT :=''; vFmul.provinciaT :='';
                      --
                      vFmul.velocidad :=''; vFmul.limiteVelocidad :=''; vFmul.textoCinemometro :='';
                      --    
                      vFmul.alcoholAire :=''; vFmul.alcoholSangre :=''; vFmul.fNotDenunica :='';
                      --    
                      vFmul.lugarOrg :=''; vFmul.fechaOrg :=''; vFmul.horaOrg :=''; vFmul.matriculaOrg :=''; 
                      vFmul.hechoOrg :=''; vFmul.cbcuaderno60 :=''; vFmul.firma :='';
                      --
                      vFmul.caja:=''; vFmul.lote:=''; vfmul.indice:='';
                      
                    end if;
                    
                    -- Si Estimaciones o Desestimaciones
                    -- y existe liquidación en ejecutiva 
                    -- ==> No imprimir importes ni referencia para el pago
                    if v.atoid in (179,180,377,1137,1157,1177) then
                    
                        for iliq in cLiqEjecutiva(v.mulid) loop
                            vFmul.fLimitePago:='';
                            vFmul.Emisora:='';
                            vFmul.Referencia:='';
                            vFmul.Identificacion:='';
                            vFmul.Importe:='';
                            vFmul.DC:='';
                            vFmul.importeSancionFirme:='';
                            vFmul.tiempoRetirada:='';                        
                        end loop;
                    end if;
                    
                    if v.atoid in (1198,1419) then
                            vFmul.sancionFirme:='';
                            vfmul.finvoluntaria:='';
                            vfmul.inicioejecutiva:='';
                            vFmul.providenciaApremio:='';
                            vFmul.tiempoRetirada:='';            
                    end if;                    
                    
                    dbms_output.put_line(vLineaError);
                    dbms_output.put_line('PIPE ROW');
                    --
                    pipe row(vFmul);

            end if; -- Versión Vigente
            

        
        -- Ejecución TEA, TCA    
        if v.mulid=0 and v.atoid in (1198,1419) and vVersionVigente=0 then
                iniciarVariableFmul();
                
                vFmul.nifD:=v.dni;
                vFmul.nombreD:=v.nombre;
                vFmul.domicilioD:=v.direccion;
                vFmul.cpD:=v.codigopostal;
                vFmul.localidadD:=v.municipio;
                vFmul.provinciaD:=v.provincia;
                
                vFmul.nexpediente:='EXPEDIENTE: ' || v.expcod;
                vFmul.boletin:='EXACCIÓN: ' || v.lugar;
                vFmul.recibo:=v.recibo;
                vFmul.expediente:=v.expidexp;
                vfmul.tiponot:=tabla(v.atoid);
                
                vFmul.cbsicer:=v.sicer;
                
                for ld in cLiq3(v.esmid) loop
                    
                    vfmul.importesancionfirme:='TOTAL:    '||
                                             to_char(ld.total,'9990.99')||
                                             '@COBRADO:          '||
                                             to_char(ld.cobrado,'9990.99')||
                                             '@PENDIENTE:        '||
                                             to_char(ld.pendiente,'9990.00');
                end loop;

                
                i:=1;
                for r in cResLiq(v.liqid,v.esmid) loop
                            
                      if i=1 then
                          vFmul.b1:=replace(replace(substr(r.txtResolucion,1,15000), CHR (13), '|'), CHR (10), '|');
                      elsif i=2 then
                          vFmul.b2:=replace(replace(substr(r.txtResolucion,1,15000), CHR (13), '|'), CHR (10), '|');
                      elsif i=3 then
                          vFmul.b3:=replace(replace(substr(r.txtResolucion,1,15000), CHR (13), '|'), CHR (10), '|');
                      elsif i=4 then
                          vFmul.b4:=replace(replace(substr(r.txtResolucion,1,15000), CHR (13), '|'), CHR (10), '|');
                      elsif i=5 then
                          vFmul.b5:=replace(replace(substr(r.txtResolucion,1,15000), CHR (13), '|'), CHR (10), '|');
                      elsif i=6 then
                          vFmul.b6:=replace(replace(substr(r.txtResolucion,1,15000), CHR (13), '|'), CHR (10), '|');
                      elsif i=7 then
                          vFmul.b7:=replace(replace(substr(r.txtResolucion,1,15000), CHR (13), '|'), CHR (10), '|');
                      elsif i=8 then
                          vFmul.b8:=replace(replace(substr(r.txtResolucion,1,15000), CHR (13), '|'), CHR (10), '|');
                      elsif i=9 then
                          vFmul.b9:=replace(replace(substr(r.txtResolucion,1,15000), CHR (13), '|'), CHR (10), '|');
                      elsif i=10 then
                          vFmul.b10:=replace(replace(substr(r.txtResolucion,1,15000), CHR (13), '|'), CHR (10), '|');
                      elsif i=11 then
                          vFmul.b11:=replace(replace(substr(r.txtResolucion,1,15000), CHR (13), '|'), CHR (10), '|');
                      elsif i=12 then
                          vFmul.b12:=replace(replace(substr(r.txtResolucion,1,15000), CHR (13), '|'), CHR (10), '|');
                      elsif i=13 then
                          vFmul.b13:=replace(replace(substr(r.txtResolucion,1,15000), CHR (13), '|'), CHR (10), '|');
                      elsif i=14 then
                          vFmul.b14:=replace(replace(substr(r.txtResolucion,1,15000), CHR (13), '|'), CHR (10), '|');
                      elsif i=15 then
                          vFmul.b15:=replace(replace(substr(r.txtResolucion,1,15000), CHR (13), '|'), CHR (10), '|');
                      end if;
                      i:=i+1;
                            
                end loop; 
                
                pipe row(vFmul);
        end if;
        

            
        end loop; --------------------------------------(1)
        
    return;
    
    
exception
    when others then
    
        vFmulLocal.mulid:=vMulidError;
        vFmulLocal.nexpediente:='ERROR » ' || to_char(vMulidError);
        vFmulLocal.a1:='ERROR. ' || SQLERRM || '. Mulid=' || to_char(vMulidError) || '. Linea Error:' || vLineaError;
        dbms_output.put_line('ERROR. ' || SQLERRM || '. Mulid=' || to_char(vMulidError) || '. Linea Error:' || vLineaError);

        return;
end;

--------------------------------------------------------------------------------
-- Unificación de Sicer
--------------------------------------------------------------------------------

procedure unificarSicer(vCrmid in number) is

cursor cNot(vid number) is
select n.ntfid, n.perid, n.rdiid, n.ntfserade sicer, n.ntfsicergrupo, n.nftgrupontfid, c.atoid
from alba.propuestasnotificacion p,
alba.notificaciones n,
alba.remesas r,
alba.cabremesas c
where c.crmid=r.crmid
and r.ntfid=n.ntfid
and r.crmid=n.ntfcrmid
and n.pntid=p.pntid
and c.crmid=vid
and c.atoid in (817,1198,1419) --817=MUL5.6: Resolución Recurso P.A., 1198=Ejecución TEA
order by n.perid, n.rdiid, n.ntfid;

ntfserade_ant varchar2(100):=null;
ntfid_ant number:=null;
perid_ant number:=0;
rdiid_ant number:=0;

total number:=0;

begin

  for n in cNot(vCrmid) loop
  
    if n.perid=perid_ant and
    n.rdiid=rdiid_ant and
    total<11 then
    
          update alba.notificaciones n2
          set n2.ntfsicergrupo=ntfserade_ant,
          n2.nftgrupontfid=ntfid_ant
          where n2.ntfid=n.ntfid;
          
          total:=total+1;
    
    else
    
          update alba.notificaciones n2
          set n2.ntfsicergrupo=null,
          n2.nftgrupontfid=null
          where n2.ntfid=n.ntfid;
          
          ntfserade_ant:=n.sicer;
          ntfid_ant:=n.ntfid;
          total:=0;
          perid_ant:=0;
          rdiid_ant:=0;          
   
    
    end if;
      

    perid_ant:=n.perid;
    rdiid_ant:=n.rdiid;  
  
  end loop;

  commit;

end;



---------------------------------------------------------------------------------
-- DBOP
---------------------------------------------------------------------------------
function dbop(vId number)
return bop_lista pipelined is

    cursor cBop(vIdLibro number) is
    select
    e.nombred infractor,
    e.nifd nif,
    e.nexpediente expediente,
    e.boletin boletin,
    e.fdenuncia fecha,
    e.hora hora,
    e.lugar lugar,
    e.matricula matricula,
    e.denunciante denunciante,
    e.normativa normativa_puntos,
    e.importe || '' cuantia
    from
    alba.det_libro_resol d, table(trml.dmultas(d.mulid,0)) e
    where
    d.lbrid=vIdLibro
    order by e.nombred;
    
vBopLocal bop;

begin

    for c in cBop(vId) loop

        vBop:=vBopLocal;
    
        vBop.infractor:=c.infractor;
        vBop.nif:=c.nif;
        vBop.expediente:=c.expediente;
        vBop.boletin:=c.boletin;
        vBop.fecha:=c.fecha;
        vBop.hora:=c.hora;
        vBop.lugar:=c.lugar;
        vBop.matricula:=c.matricula;
        vBop.denunciante:=c.denunciante;
        vBop.normativa_puntos:=c.normativa_puntos;
        vBop.cuantia:=c.cuantia;
        pipe row(vBop);
    end loop;
    return;
end;


---------------------------------------------------------------------------------
-- DMULTAS
---------------------------------------------------------------------------------
function dmultas(vMulidPar number, vAlgidPar number)
return fmul_lista pipelined is
-- Cursores
-- Vehículo
cursor cveh(vmulid alba.multas.mulid%type) is
select /*RULE*/ * from (
select mulid, matricula, marca, tipo from
(
select m1.mulid mulid, v1.vehmatri matricula, nvl(v1.vehmarca,m1.mulmarca) marca, t1.tvtmdes tipo, 3 vehid
from alba.tivdgt_m t1, alba.vehiculos_d v1, alba.multas m1
where m1.seoidveh=v1.seoid
and v1.tvtmid=t1.tvtmid(+)
and v1.vehversion=
(select max(vehversion) from alba.vehiculos_d v where v.seoid = m1.SeoIdVeh)
and m1.mulid=vMulid
union
select m2.mulid mulid, v2.vemmatri matricula, nvl(v2.vemmarca,m2.mulmarca) marca, t2.tvtmdes tipo, 2 vehid
from alba.tivdgt_m t2, alba.vehiculos_mult v2, alba.multas m2
where m2.vemid=v2.vemid
and m2.tvtmid=t2.tvtmid(+)
and m2.mulid=vMulid
UNION
select d.mulid, w.wsmatricula matricula, trim(w.wsmarca) || ' ' ||trim(w.wsmodelo) marca, '' tipo, 1 vehid
from alba.multas mm, alba.datoswsdgt w, alba.det_libro_resol d
where mm.mulid=d.mulid
and d.idwsd=w.idwsd
and mm.seoidexp = (select m.seoidexp from alba.multas m where m.mulid=vMulid)
) order by vehid asc, mulid desc ) where rownum<2;

-- Personas
cursor cPer(vMulid alba.multas.mulid%type) is
select m.mulid, m.mulnumbol, alba.componer_nombre(p.pernombre, p.perapell1, p.perpart1,
       p.perapell2, p.perpart2, p.perrazon) nombre,
       nvl(p.periden, p.peridenext) nif, d.decid, d.decelemento, d.decdescr,
           t.tpeid, t.tpercodigo, t.tpedesc, e.expid, ee.expcod expediente, p.perid, p.perversion
from alba.expedientesingresoscontr e, alba.decodificadora d, alba.expingre ee,
     alba.tipospersona t, alba.personas p, alba.contribuyentes c, alba.multas m
where e.eicxtipocontribuyente=d.decid
and e.conid=c.conid
and c.perid=p.perid
and p.tpeid=t.tpeid
and m.expidexp=e.expid
and ee.expid=e.expid
and e.eicfvigencia is null
and m.mulid=vMulid;

-- Direcciones
cursor cDir(vMulid alba.multas.mulid%type, vPersona varchar2) is
SELECT /*RULE*/ rdi.rdiid, alba.componer_direccion
 (acc.accnum1, acc.accinum1, acc.accnum2, acc.accinum2, acc.acckm1,acc.accikm1, acc.accdirnoest,
  rdi.rdixpor, rdi.rdixesc, rdi.rdixpiso, rdi.rdixpuerta, rdi.rdiipuertadup, rdi.rdidirnoest,
  tvi.tvicodigo, via.viadesc, rdi.rdiref3, rdi.rdirefcon1, rdi.rdirefcon2, acc.codid, cod.codposcodigo) domicilio,
  nvl(cod.CODPOSCODIGO, '') cp, mu.mundesc localidad, p.mprdesc provincia,
  decdir.decelemento tipo_direc, decper.decelemento tipo_per
FROM alba.expedientesingresoscontr eic, alba.direccionesexpedientes ted,
     alba.decodificadora decdir, alba.decodificadora decper, alba.direccion rdi,
     alba.tvia tvi, alba.vias via, alba.accesos acc, alba.municipios mu,
     alba.m_codpostal cod, alba.provincia p, alba.multas m
WHERE eic.eicxtipocontribuyente = decper.decid
AND ted.eicid = eic.eicid
AND ted.tedxtipodireccion = decdir.decid
AND ted.tedfvigencia IS NULL
AND ted.rdiid = rdi.rdiid
AND rdi.accid = acc.accid
AND acc.viaid = via.viaid
AND via.tviid = tvi.tviid
AND eic.expid = m.expidexp
AND via.MunId = mu.MunId
AND mu.mprid = p.mprid
AND acc.codid = cod.codid (+)
--and mu.mundesc<>' ' -- Elimina direcciones sin código postal
--and p.mprdesc<>' '
--and cod.CODPOSCODIGO is not null
and m.mulid=vMulid
and decper.decelemento=vPersona
order by rdi.rdiid desc;

-- Datos Multas
cursor cRem(vMulid alba.multas.mulid%type) is
select /*RULE*/
        m.mulid, m.expidexp, i.infid,
        --'' dni,'' nombre,'' direccion,'' codigopostal,'' municipio,'' provincia,
        m.mulfeciniprod, m.mulfec, m.mulimp, e.expcod, m.mulnumbol,
        nvl(ltrim(rtrim(tvi.tvidesc || ' ' || via.viadesc)), nvl(m.mullugar,''))
        || decode (nvl(m.muldelante,''),'','',' Delante de ' || nvl(m.muldelante,''))
        || decode (nvl(m.mulfrente,''),'','',' Frente a ' || nvl(m.mulfrente,''))
        || decode (nvl(m.mulobsdir,''),'','',' Con dirección a ' || nvl(m.mulobsdir,'')) lugar,
        lpad(m.mulhor,2,0) || ':' || lpad(m.mulmin,2,0) hora, age.agenumero,
        m.muldenuncia hd, '' sicer, i.infarticulo articulo, i.infapartado apartado,
        i.infarticulo2 articulo2, i.infapartado2 apartado2,
        decode(i.infgrado,'L','Leve','G','Grave','M','Muy grave') grado, di.decdescr norma, di2.decdescr norma2,
        m.mulmiembrofse fse,
        i.puntostxt puntos, sysdate fechaEmision, m.mulfecnotden fecnotden,
        decode(m.mulxboletin,144,1,303,2,403,0,444,3,423,4,464,6,465,5,523,5,543,5,623,7,664,9,723,8,743,10,800,11) origenDenuncia         
from alba.vias via, alba.tvia tvi,
         alba.agentes age, alba.decoingresos mot, alba.decoingresos di, alba.decoingresos di2,
     alba.infracciones i, alba.expingre e, alba.multas m
where m.expidexp=e.expid
and m.viaid = via.viaid(+)
and via.tviid = tvi.tviid(+)
and m.age1_ageid = age.ageid(+)
and m.mulxmotivo = mot.decid(+)
and m.infid=i.infid(+)
and i.infxlegislacion = di.decid(+)
and i.infxlegislacion2 = di2.decid(+)
and m.mulid = vMulid;


-- Datos Resoluciones
cursor cRes(vMulid alba.multas.mulid%type, vEsmid alba.escritosmultas.esmid%type) is
select /*RULE*/ ra.rsaid, e.esmid, e.mulid, e.esmfescrito fEscrito, t.tidomnombre tipoEscrito,
       e.esmfestado fResolucion, tr.tdmnombre tipoResultado, ra.algid detalleresolucion,
             nvl(tr.tdminforme,t.tidominforme) informe,
             ra.rsamotivoresol txtAlegacion, ra.rsaresolucion txtResolucion, r.resdescr motivo
from alba.tipos_doc_multas t, alba.tipos_doc_multas_res tr,
         alba.resoluciones r, alba.resol_alegaciones ra,
     alba.escritosmultas e, alba.multas m
where m.mulid=e.mulid
and e.tidomid=t.tidomid
and e.tdmresid=tr.tdmresid
and ra.esmid=e.esmid
and ra.resid=r.resid
and m.mulid=vMulid
and e.esmid=vEsmid
order by ra.rsaid asc;

-- Puntos Denuncias No Identificación de Conductor
cursor cPuntos(vMulid alba.multas.mulid%type) is
select /*RULE*/
'Puntos='||
to_char(decode(i.infgrado,'L',2,3)*i.puntos) puntosNI
from
alba.infracciones i,
alba.multas m
where m.infid=i.infid
and m.seoidexp=
(
select m2.seoidexp
from alba.multas m2
where m2.mulid=vMulid
)
and m.mulid=
(
select min(m3.mulid)
from alba.multas m3
where m3.seoidexp=m.seoidexp
)
and i.puntos>0 and i.puntos is not null;


-- Persona Escritos
cursor cPerEscrito(vMulid number,vEsmid number) is
select /*RULE*/ m.mulid, m.mulnumbol, alba.componer_nombre(p.pernombre, p.perapell1, p.perpart1,
       p.perapell2, p.perpart2, p.perrazon) nombre,
       nvl(p.periden, p.peridenext) nif, d.decid, d.decelemento, d.decdescr,
           t.tpeid, t.tpercodigo, t.tpedesc, e.expid, ee.expcod expediente, p.perid, p.perversion
from 
alba.escritosmultas em,
alba.expedientesingresoscontr e, alba.decodificadora d, alba.expingre ee,
alba.tipospersona t, alba.personas p, alba.contribuyentes c, alba.multas m
where e.eicxtipocontribuyente=d.decid
and e.conid=c.conid
and c.perid=p.perid
and p.tpeid=t.tpeid
and m.expidexp=e.expid
and ee.expid=e.expid
and m.mulid=em.mulid
and e.esmid=em.esmid
and e.eicfvigencia is null
and m.mulid=vMulid
and em.esmid=vEsmid;

-- Dirección Escrito
cursor cDirEscrito(vMulid number, vEsmid number) is
SELECT /*RULE*/ rdi.rdiid, alba.componer_direccion
 (acc.accnum1, acc.accinum1, acc.accnum2, acc.accinum2, acc.acckm1,acc.accikm1, acc.accdirnoest,
  rdi.rdixpor, rdi.rdixesc, rdi.rdixpiso, rdi.rdixpuerta, rdi.rdiipuertadup, rdi.rdidirnoest,
  tvi.tvicodigo, via.viadesc, rdi.rdiref3, rdi.rdirefcon1, rdi.rdirefcon2, acc.codid, cod.codposcodigo) domicilio,
  nvl(cod.CODPOSCODIGO, '') cp, mu.mundesc localidad, p.mprdesc provincia,
  decdir.decelemento tipo_direc, decper.decelemento tipo_per
FROM alba.escritosmultas em,
     alba.expedientesingresoscontr eic, alba.direccionesexpedientes ted,
     alba.decodificadora decdir, alba.decodificadora decper, alba.direccion rdi,
     alba.tvia tvi, alba.vias via, alba.accesos acc, alba.municipios mu,
     alba.m_codpostal cod, alba.provincia p, alba.multas m
WHERE eic.eicxtipocontribuyente = decper.decid
AND ted.eicid = eic.eicid
AND ted.tedxtipodireccion = decdir.decid
AND ted.tedfvigencia IS NULL
AND ted.rdiid = rdi.rdiid
AND rdi.accid = acc.accid
AND acc.viaid = via.viaid
AND via.tviid = tvi.tviid
AND eic.expid = m.expidexp
AND via.MunId = mu.MunId
AND mu.mprid = p.mprid
and m.mulid=em.mulid
and eic.esmid=em.esmid
AND acc.codid = cod.codid (+)
and m.mulid=vMulid
and em.esmid=vEsmid;

-- Informe Multas
cursor cInforme(vEsmid number) is
select /*RULE*/
decode(inf.tdminf_informe, null, decode(res.tdminforme, null, tipo.tidominforme, res.tdminforme), inf.tdminf_informe) informe
from (select tidomid, tdmresid, mulxboletin from alba.escritosmultas es, alba.multas where es.esmid = vEsmid and multas.mulid = es.mulid) escrito
left outer join alba.tipos_doc_multas tipo on tipo.tidomid = escrito.tidomid
left outer join alba.tipos_doc_multas_res res on res.tdmresid = escrito.tdmresid
left outer join alba.tipos_doc_multas_inf inf on inf.tdmresid = escrito.tdmresid
and inf.mulxboletin = escrito.mulxboletin;



-- GLOBAL
vTemp varchar2(100);
vLimitePago date:=null;
vLongitud number:=0;
i number:=0;
vTieneAsteriscos number:=0;
vPrioridadPersona number:=0;
vTipoPersona varchar2(100):='';
vPrioridadDireccion number:=0;
vImporte number:=0;
vEstado number:=0;
--
vSancionFirme date:=null;
vFinVoluntaria date:=null;
vInicioEjecutiva date:=null;
vTieneEstado number:=0;
vOrigenDen number:=0;

vdmullocal fmul;
vflibinc date:=null;


-- BLOQUE PRINCIPAL
begin

        -- Recorre Multas
        for v in cRem(vMulidPar) loop ---------------------(1)
        
                vDmul:=vDmulLocal;
                iniciarVariableDmul();
                --vDmul.nifD:=r.dni;
                --vDmul.nombreD:=r.nombre;
                --
                --(3.*)
                --if v.fecnotden is not null then
                --    vDmul.fNotDenunica:=to_char(v.fecnotden,'dd/mm/rrrr');
                --    vImporte:=(round(v.mulimp,2))*100;
                --else
                vDmul.fNotDenunica:=to_char(v.fecnotden,'dd/mm/rrrr');
                vImporte:=(round(v.mulimp,2));
                --end if;
                --
                vDmul.mulid:=v.mulid;
                vDmul.expediente:=v.expidexp;
                --vDmul.TipoNot:=tabla(v.atoid);
        
        -- RECIBO
        --select substr(lr.liqnumerorecliquidacion,1,12) into vDmul.Recibo from alba.liquidaciones lr, alba.multas mr where mr.expidexp=lr.expid and mr.mulid=v.mulid and rownum>0 and rownum<2;
                
                --vLimitePago:=to_date((sysdate+10),'dd/mm/rrrr');
                vDmul.fLimitePago:=to_char((sysdate+10),'dd/mm/rrrr');
                vDmul.Emisora:='410913';
                vDmul.Referencia:=to_char(v.mulfec,'yy') || lpad(v.mulid,8,0);
                vDmul.Identificacion:='1064'||to_char(v.fechaEmision,'yy')||to_char((sysdate+10),'Y')
                             ||to_char((sysdate+10),'DDD');
                --vDmul.Importe:=to_char((round(v.mulimp*(70/100),2))*100);
                vDmul.Importe:=to_char(vImporte);
                vDmul.Importe2:=vImporte;
                
                vTemp:=TO_CHAR(FLOOR(((((to_number(vDmul.Emisora))*76)+((to_number(vDmul.Referencia))*9)+
               ((to_number(vDmul.Identificacion)+(vImporte)-1)*55))/97)*100));
               
                vDmul.DC:=lpad(to_char(99-to_number(substr((vTemp),(length(vTemp)-1),2))),2,0);
                

                vDmul.nExpediente:=v.expcod;
                vDmul.Boletin:=v.mulnumbol;
                vDmul.fIncoacion:=to_char(v.mulfeciniprod,'dd/mm/rrrr');
                vDmul.Lugar:=substr(upper(v.lugar),1,300);
                vDmul.fDenuncia:=to_char(v.mulfec,'dd/mm/rrrr');
                vDmul.Hora:=v.hora;
                -- VEHÍCULOS
                for m in cVeh(v.mulid) loop ---------------------(1)
                    vDmul.Matricula:=upper(m.matricula);
                    vDmul.Vehiculo:=upper(m.tipo) || ' ' || upper(m.marca);
                end loop;
        
                if v.origenDenuncia=4 then -- Botellón
                            vDmul.Matricula:='';
                            vDmul.Vehiculo:='';        
                end if;
                
                -- INICIO Analiza estado
                select cEstado, ffirmeza, ffinvol, ffinvol+1, decode(cEstado,null,0,cEstado), e.origenDenuncia, e.flibinc
                into vEstado, vSancionFirme, vFinVoluntaria, vInicioEjecutiva, vTieneEstado, vOrigenden, vflibinc
                from alba.multas m, table(trml.emultas(m.mulid)) e
                where m.mulid=e.mulid(+)
                and m.mulid=v.mulid;                
                
                
                -- PERSONAS
                vDmul.denunciante:=nvl(to_char(v.agenumero),v.fse);
                vPrioridadPersona:=0;
                vTipoPersona:='';
                for p in cPer(v.mulid) loop
                        -- DENUNCIADO
                        if p.decelemento='SUJETOPASIVO' and vPrioridadPersona<1 then
                            vDmul.nombreD:=substr(p.nombre,1,80);
                            vDmul.nifD:=p.nif;
                            vDmul.peridD:=p.perid;
                            vDmul.perversionD:=p.perversion;    
                            vPrioridadPersona:=1;
                            vTipoPersona:=p.decelemento;
                        elsif p.decelemento in ('WSDGT_ARR','WSDGT_CH','WSDGT_POS','WSDGT_TIT','WSDGT_TRA')
                            and vPrioridadPersona<2 then
                            vDmul.nombreD:=substr(p.nombre,1,80);
                            vDmul.nifD:=p.nif;
                            vDmul.peridD:=p.perid;
                            vDmul.perversionD:=p.perversion;    
                            vPrioridadPersona:=2;
                            vTipoPersona:=p.decelemento;              
                        elsif p.decelemento='TITULAR' and vPrioridadPersona<3 then
                            vDmul.nombreD:=substr(p.nombre,1,80);
                            vDmul.nifD:=p.nif;
                            vDmul.peridD:=p.perid;
                            vDmul.perversionD:=p.perversion;    
                            vPrioridadPersona:=3;
                            vTipoPersona:=p.decelemento;
                        elsif p.decelemento='CONDUCTOR' and vPrioridadPersona<4 then
                            vDmul.nombreD:=substr(p.nombre,1,80);
                            vDmul.nifD:=p.nif;
                            vDmul.peridD:=p.perid;
                            vDmul.perversionD:=p.perversion;    
                            vPrioridadPersona:=4;
                            vTipoPersona:=p.decelemento;
                        elsif p.decelemento='TUTOR' and vPrioridadPersona<5 then
                            vDmul.nombreD:=substr(p.nombre,1,80);
                            vDmul.nifD:=p.nif;
                            vDmul.peridD:=p.perid;
                            vDmul.perversionD:=p.perversion;    
                            vPrioridadPersona:=5;
                            vTipoPersona:=p.decelemento;
                        end if;
                        -- DENUNCIANTE
                        if p.decelemento='DENUNCIANTE' and vDmul.denunciante is null then
                            vDmul.denunciante:='DENUNCIANTE: ' || p.nif;
                        end if;
                end loop;
        
                -- DIRECCIONES
                
                -- Antes de 29/11/2007: FISCAL,DIR PADRON,NOTIFSEGUNDA,NOTIFICACION,OBJETO
                -- Ahora:               NOTIFICACION,FISCAL,DIR PADRON,NOTIFSEGUNDA,OBJETO
                -- 02/04/2009:          COMUNICADA,NOTIFICACION,FISCAL,DIR PADRON,NOTIFSEGUNDA
                -- 21/04/2009:          OBJETO,COMUNICADA,NOTIFICACION,FISCAL,DIR PADRON,NOTIFSEGUNDA
                
                vPrioridadDireccion:=0;
                for d in cDir(v.mulid,vTipoPersona) loop
                      if d.tipo_direc='OBJETO' and vPrioridadDireccion<1 then
                            vDmul.domicilioD:=substr(d.domicilio,1,80);
                            vDmul.cpD:=d.cp;
                            vDmul.localidadD:=substr(d.localidad,1,80);
                            vDmul.provinciaD:=substr(d.provincia,1,80);
                            vDmul.rdiidD:=d.rdiid;
                            vprioridaddireccion:=1;
                      elsif d.tipo_direc='COMUNICADA' and vPrioridadDireccion<2 then
                            vDmul.domicilioD:=substr(d.domicilio,1,80);
                            vDmul.cpD:=d.cp;
                            vDmul.localidadD:=substr(d.localidad,1,80);
                            vDmul.provinciaD:=substr(d.provincia,1,80);
                        vDmul.rdiidD:=d.rdiid;
                            vPrioridadDireccion:=2;                
                      elsif d.tipo_direc='NOTIFSEGUNDA' and vPrioridadDireccion<3 then
                            vDmul.domicilioD:=substr(d.domicilio,1,80);
                            vDmul.cpD:=d.cp;
                            vDmul.localidadD:=substr(d.localidad,1,80);
                            vDmul.provinciaD:=substr(d.provincia,1,80);
                            vDmul.rdiidD:=d.rdiid;
                            vPrioridadDireccion:=3;
                      elsif d.tipo_direc='DIR PADRON' and vPrioridadDireccion<4 then
                            vDmul.domicilioD:=substr(d.domicilio,1,80);
                            vDmul.cpD:=d.cp;
                            vDmul.localidadD:=substr(d.localidad,1,80);
                            vDmul.provinciaD:=substr(d.provincia,1,80);
                        vDmul.rdiidD:=d.rdiid;
                            vPrioridadDireccion:=4;
                      elsif d.tipo_direc='FISCAL' and vPrioridadDireccion<5 then
                            vDmul.domicilioD:=substr(d.domicilio,1,80);
                            vDmul.cpD:=d.cp;
                            vDmul.localidadD:=substr(d.localidad,1,80);
                            vDmul.provinciaD:=substr(d.provincia,1,80);
                        vDmul.rdiidD:=d.rdiid;
                            vPrioridadDireccion:=5;
                      elsif d.tipo_direc='NOTIFICACION' and vPrioridadDireccion<6 then
                            vDmul.domicilioD:=substr(d.domicilio,1,80);
                            vDmul.cpD:=d.cp;
                            vDmul.localidadD:=substr(d.localidad,1,80);
                            vDmul.provinciaD:=substr(d.provincia,1,80);
                        vDmul.rdiidD:=d.rdiid;
                            vPrioridadDireccion:=6;
                      elsif d.tipo_direc in ('WSDGT_DIR_AR','WSDGT_DIR_CH','WSDGT_DIR_PO','WSDGT_DIR_TI','WSDGT_DIR_TR')
                      and vPrioridadDireccion<7 then
                            vDmul.domicilioD:=substr(d.domicilio,1,80);
                            vDmul.cpD:=d.cp;
                            vDmul.localidadD:=substr(d.localidad,1,80);
                            vDmul.provinciaD:=substr(d.provincia,1,80);
                            vDmul.rdiidD:=d.rdiid;
                            vprioridaddireccion:=7;
                      elsif d.tipo_direc in ('WSDGT_DIRDEV')
                      and vSancionFirme is null
                      and vPrioridadDireccion<8 
                      then
                            vDmul.domicilioD:=substr(d.domicilio,1,80);
                            vDmul.cpD:=d.cp;
                            vDmul.localidadD:=substr(d.localidad,1,80);
                            vDmul.provinciaD:=substr(d.provincia,1,80);
                            vDmul.rdiidD:=d.rdiid;
                            vprioridaddireccion:=8;
                      end if;
                end loop;
        
        
        -- Persona Escrito
        for p2 in cPerEscrito(v.mulid,vAlgidPar) loop
        
                            -- Copiamos el denunciado en otro campo
              vdmul.nombret:=vdmul.nombred;
                            vDmul.nifT:=vDmul.nifD;
                            
              --
                            vDmul.nombreD:=substr(p2.nombre,1,80);
                            vDmul.nifD:=p2.nif;
                            vDmul.peridD:=p2.perid;
                            vDmul.perversionD:=p2.perversion;    
                            vPrioridadPersona:=10;
                            vtipopersona:=p2.decelemento;        
        end loop;
        
                -- Dirección Escrito
        for d2 in cDirEscrito(v.mulid,vAlgidPar) loop
        
                -- Copiamos la dirección del denunciado en otros campos
                             vdmul.domicilioT:=vdmul.domiciliod;
                            vdmul.cpT:=vdmul.cpd;
                            vdmul.localidadT:=vdmul.localidadd;
                            vDmul.provinciaT:=vDmul.provinciaD;
        
                            --
                vDmul.domicilioD:=substr(d2.domicilio,1,80);
                            vDmul.cpD:=d2.cp;
                            vDmul.localidadD:=substr(d2.localidad,1,80);
                            vDmul.provinciaD:=substr(d2.provincia,1,80);
                vDmul.rdiidD:=d2.rdiid;
                            vPrioridadDireccion:=10;             
        end loop;
        
                
                -- Elimina Asteríscos
                select length(vDmul.domicilioD),decode(substr(vDmul.domicilioD,1,1),'*',1,0),1 into vLongitud,vTieneAsteriscos,i from dual;
                WHILE i<vLongitud and vTieneAsteriscos=1 LOOP
                   i:=i+1;
                     select decode(substr(vDmul.domicilioD,i,1),'*',1,0) into vTieneAsteriscos from dual;
                END LOOP;
                select ltrim(substr(vDmul.domicilioD,i,vLongitud)) into vDmul.domicilioD from dual;
                -- Fin Elimina Asteríscos

                -- Motivo No Notificación
                vDmul.motivoNoEntrega:=leerMotivoNoNotificacion(v.mulid);

                
                vDmul.calificacion:='CALIFICACIÓN JURÍDICA: '||upper(v.GRADO)||'  '||upper(v.puntos);
                vDmul.normativa:='NORMATIVA APLICABLE: '|| upper(v.NORMA)||'. ART. ' || upper(v.ARTICULO) ||' APDO. '|| upper(v.APARTADO) || '. ' ||upper(v.puntos);
                
                if v.origenDenuncia=8 then -- CIR
                  vDmul.normativa := vDmul.normativa || ' ' || upper(v.NORMA2) || '. ART. ' || upper(v.ARTICULO2) ||' APDO. '|| upper(v.APARTADO2);      
                end if;
                                
        
                /*if v.infid in (9002328,9002344) then --Puntos 1.6.5
                
                    for pt in cPuntos(v.mulid) loop
                    
                                  vDmul.calificacion:='CALIFICACIÓN JURÍDICA: '||upper(v.GRADO)||'  '||upper(pt.puntosNI);
                                  vDmul.normativa:='NORMATIVA APLICABLE: '|| upper(v.NORMA)||'. ART. ' || upper(v.ARTICULO) ||' APDO. '|| upper(v.APARTADO) || '. ' ||upper(pt.puntosNI);
                    
                    end loop;
                
                end if;*/
        
        
                vDmul.tiempoRetirada:=to_char(v.mulimp,'9G999D99');
                vDmul.hecho:=upper(substr(v.hd,1,300))|| ' · ' || vDmul.calificacion;
                vDmul.cbsicer:=v.sicer;
                vDmul.cbcuaderno60:='90521'||vDmul.Emisora||vDmul.Referencia||vDmul.DC||vDmul.Identificacion||LPAD((vImporte),8,0)||'0';
                vDmul.firma:='';
                --(1.2)
                --vDmul.nifT:='';
                --vDmul.nombreT:='';
                --vDmul.domicilioT:='';
                --vDmul.cpT:='';
                --vDmul.localidadT:='';
                --vDmul.provinciaT:='';
                --(1.8,1.9,3.8)
                vDmul.velocidad:='';
                vDmul.limiteVelocidad:='';
                vDmul.textoCinemometro:='';
                --(1.7,3.7)
                vDmul.alcoholAire:='';
                vDmul.alcoholSangre:='';
                --(1.6)
                vdmul.lugarorg:='';
                vDmul.fechaOrg:='';
                vDmul.horaOrg:='';
                vDmul.matriculaOrg:='';
                vDmul.hechoOrg:='';
                --
                -- RESOLUCIONES
                i:=1;
                for r in cRes(v.mulid,vAlgidPar) loop
        
                vDmul.fechaOrg:=to_char(r.fResolucion,'dd/mm/rrrr');
                    if i=1 then
                        vDmul.a1:=substr(r.txtAlegacion,1,15000);
                        vdmul.b1:=substr(r.txtresolucion,1,15000);
                        vDmul.hechoOrg:=substr(r.motivo,1,1000);
                        vDmul.informe:=r.informe;
                    elsif i=2 then
                        vDmul.a2:=substr(r.txtAlegacion,1,15000);
                        vDmul.b2:=substr(r.txtresolucion,1,15000);
                    elsif i=3 then
                        vDmul.a3:=substr(r.txtAlegacion,1,15000);
                        vDmul.b3:=substr(r.txtresolucion,1,15000);
                    elsif i=4 then
                        vDmul.a4:=substr(r.txtAlegacion,1,15000);
                        vDmul.b4:=substr(r.txtresolucion,1,15000);
                    elsif i=5 then
                        vDmul.a5:=substr(r.txtAlegacion,1,15000);
                        vDmul.b5:=substr(r.txtresolucion,1,15000);
                    elsif i=6 then
                        vDmul.a6:=substr(r.txtAlegacion,1,15000);
                        vDmul.b6:=substr(r.txtresolucion,1,15000);
                    elsif i=7 then
                        vDmul.a7:=substr(r.txtAlegacion,1,15000);
                        vDmul.b7:=substr(r.txtresolucion,1,15000);
                    elsif i=8 then
                        vDmul.a8:=substr(r.txtAlegacion,1,15000);
                        vDmul.b8:=substr(r.txtresolucion,1,15000);
                    elsif i=9 then
                        vDmul.a9:=substr(r.txtAlegacion,1,15000);
                        vDmul.b9:=substr(r.txtresolucion,1,15000);
                    elsif i=10 then
                        vDmul.a10:=substr(r.txtAlegacion,1,15000);
                        vDmul.b10:=substr(r.txtresolucion,1,15000);
                    elsif i=11 then
                        vDmul.a11:=substr(r.txtAlegacion,1,15000);
                        vDmul.b11:=substr(r.txtresolucion,1,15000);
                    elsif i=12 then
                        vDmul.a12:=substr(r.txtAlegacion,1,15000);
                        vDmul.b12:=substr(r.txtresolucion,1,15000);
                    elsif i=13 then
                        vDmul.a13:=substr(r.txtAlegacion,1,15000);
                        vDmul.b13:=substr(r.txtresolucion,1,15000);
                    elsif i=14 then
                        vDmul.a14:=substr(r.txtAlegacion,1,15000);
                        vDmul.b14:=substr(r.txtresolucion,1,15000);
                    elsif i=15 then
                        vDmul.a15:=substr(r.txtAlegacion,1,15000);
                        vDmul.b15:=substr(r.txtresolucion,1,15000);
           
                    end if;
                    i:=i+1;
                  end loop;
                --
                
                if vDmul.informe='' then
                    select t.tidominforme into vDmul.informe
                    from alba.tipos_doc_multas t, alba.escritosmultas e
                    where e.tidomid=e.tidomid
                    and e.mulid=v.mulid
                    and e.esmid=vAlgidPar
                    and rownum<2;
                end if;
                    
                    for ci in cInforme(vAlgidPar) loop
                         vDmul.informe:=ci.informe;
                    end loop;


                
                if vDmul.informe='MULINFALEG.RPT' and vOrigenden in (4,6) then
                    vDmul.informe:='MULINFALEG_' || to_char(vOrigenden) || '.RPT';
                end if;  
                
                -- Fecha Incoación
                if vflibinc is not null then
                  vdmul.fincoacion:=vflibinc;
                end if;
                
                --
                if vTieneEstado>0
                then
                    vDmul.sancionFirme:=vSancionFirme;
                    vDmul.finVoluntaria:=vFinVoluntaria;
                    vDmul.inicioEjecutiva:=vInicioEjecutiva;
                    
                    If vEstado is not null and vEstado in (9110,9120) then
                        vDmul.normativa:=upper(v.NORMA)||'. ART. ' || upper(v.ARTICULO) ||' APDO. '|| upper(v.APARTADO)||'. '||upper(v.puntos);
                    end if;
                end if;
                -- FIN Analiza estado                
                
                pipe row(vDmul);
    end loop; --------------------------------------(1)
    return;
end;



---------------------------------------------------------------------------------
-- Generación de Documento Cobratorio
---------------------------------------------------------------------------------
procedure generarDocCobratorio(vMulid number,tipoBol varchar2, boletin varchar2,
        fecha varchar2,hora varchar2,matricula varchar2,infraccion varchar2, usuario number,
        vOut in out number) is
        
vMdcid number:=0;
vTipoBol varchar2(20):='';
vBoletin varchar2(20):='';
vFecha date:=null;
vMatricula varchar2(20):='';
vInfraccion varchar2(20):='';
vImporte number:=0;
vSalida number:=0;

begin


    vTipoBol:=tipoBol;
    vBoletin:=boletin;
    vFecha:=to_date(fecha,'dd/mm/rrrr');
    vMatricula:=matricula;
    vInfraccion:=infraccion;
    vImporte:=0;
    vSalida:=0;
    
--    if usuario in (1003) or vFecha<to_date('25/05/2010','dd/mm/yyyy') then

        if vMulid=0 or vMulid is null then
    
            vSalida:=0;
            
            select i.infimporte into vImporte
            from alba.infracciones i
            where i.infcodigo=vInfraccion
            and rownum<2;
            
    
            if vBoletin is not null
            and length(vBoletin)>5 and length(vBoletin)<11
            and vfecha is not null and vfecha>=to_date('01/01/2008','dd/mm/rrrr')
            --and vMatricula is not null and length(vMatricula)>5
            and vImporte is not null and vImporte>0
            then
            
                select alba.multas_sq.NEXTVAL into vMdcid from dual;
    
                insert into alba.multasdc a
                (a.mdcid, a.tipobol, a.boletin, a.fecha, a.hora, a.matricula,
                 a.infraccion, a.importe, a.clsid, a.liqid, a.fhoramod,
                 a.usuidmod, a.motivo)
                values
                (vMdcid,vTipoBol,vBoletin,to_char(vFecha,'dd/mm/yyyy'),null,vMatricula,
                vInfraccion,vImporte,null,null,sysdate,
                0,'Se ha creado');
                
                vSalida:=vMdcid;
    
            else
    
                vSalida:=0;
    
            end if;
    
            
        else
    
           vSalida := vMulid;
            
        end if;
        
        commit;
        
    --else
    --    vSalida:=0;
    --end if;
    
    vOut:=vSalida;
    
end;


---------------------------------------------------------------------------------
-- Obtención de Documento Cobratorio
---------------------------------------------------------------------------------
function obtenerDocCobratorio(vMulid number)
return dc_lista pipelined is

-- Cursores

  -- Multas
    cursor cmul(vmulid number) is
    select m.mulfec, d.*, e.libsan, e.aleg, e.cestado, e.liqid, m.expidexp expid, e.origendenuncia,
    e.fnotsan, e.fnotden, m.infid
    from alba.multas m, table(trml.dmultas(m.mulid,0)) d, table(trml.emultas(m.mulid)) e
    where m.mulid=d.mulid
    and m.mulid=e.mulid
    and ((e.cestado>1000 and e.cestado<30000) or (e.cestado in (240000,250000,230000)))
    and (e.cestado>1000 and e.cestado<30000)
    and m.mulfecgrab>=to_date('04/08/2006','dd/mm/rrrr')
    --and (e.fnotden is not null or e.libinc>0 or e.libreq>0)
    and m.mulid=(select max(m2.mulid) from alba.multas m2
    where m2.seoidexp=(select m3.seoidexp from alba.multas m3 where m3.mulid=vMulid));
    
  cursor cMul2(vMulid number) is
  select m.*, i.infdescripcion hecho
  from alba.infracciones i, alba.multasdc m
  where m.infraccion=i.infcodigo
  and m.mdcid=vmulid
  --and m.mdcid=0 -- para impedir que entre en el bucle y genere dc de multas no grabadas
  and rownum<2;
  
  cursor cImportes(vLiqidImportes number) is
  select to_char(i.dprincipal) principal,
  to_char(i.dcostas) costas,
  to_char(i.dapremio) apremio,
  to_char(i.dintereses) intereses,
  to_char(i.dacuenta) acuenta,
  i.importe
  from table(alba.pkg_calculoimportes.ctd(vliqidimportes,sysdate)) i;
  
  cursor cDenOrg(vmulid number) is
  select 
  substr(('DENUNCIA NO IDENTIFICACIÓN. ORIGINAL FECHA: '||
  m.mulfec||
  ' HORA: '||
  lpad(m.mulhor,2,0) || ':' || lpad(m.mulmin,2,0) ||
  ' IMPORTE: '||
  m.mulimp ||
  ' LUGAR: '||
  upper(nvl(ltrim(rtrim(tvi.tvidesc || ' ' || via.viadesc)), nvl(m.mullugar,''))
          || decode (nvl(m.muldelante,''),'','',' Delante de ' || nvl(m.muldelante,''))
          || decode (nvl(m.mulfrente,''),'','',' Frente a ' || nvl(m.mulfrente,''))
          || decode (nvl(m.mulobsdir,''),'','',' Con dirección a ' || nvl(m.mulobsdir,''))) ||
  ' HECHO DENUNCIADO: '||
  m.muldenuncia ||
  ' ART: '||
  i.infarticulo ||
  ' APTDO: '||
  i.infapartado ||
  ' GRADO: '||
  decode(i.infgrado,'L','LEVE','G','GRAVE','M','MUY GRAVE') ||
  ' NORMA: '||
  di.decdescr ||
  ' '||i.puntosTXT),1,400)
  as hecho
  from
  alba.decoingresos di,
  alba.infracciones i,
  alba.multas m,
  alba.vias via,
  alba.tvia tvi
  where m.infid=i.infid
  and m.viaid = via.viaid (+)
  and via.tviid = tvi.tviid (+)
  and i.infxlegislacion = di.decid (+)
  and m.seoidexp=
  (
  select  m2.seoidexp
  from alba.multas m2
  where m2.mulid=vmulid
  and m2.infid in (9002328,9002344)
  )
  and m.mulid=
  (
  select /*+RULE*/   min(m3.mulid)
  from alba.multas m3
  where m3.seoidexp=m.seoidexp
  );  

-- GLOBAL
    vDcLocal dc;
    vNoHayDatos boolean;
    vLimitePago date;
    vFechaEmision date;
    vTemp number;
    vImporte number;
    vDescuento number;

-- BLOQUE PRINCIPAL
begin

        vNoHayDatos:=true;
        vDescuento:=0;

        -- Recorre Multas
        for v in cMul(vMulid) loop ---------------------(1)

            iniciarDocCobratorio;
            
            vDc.liqid:=v.liqid;
            vDc.expid:=v.expid;
            
            if v.cestado in (30000,30001) then
            
                    vDc.cdc:='2';
                    vdc.ddc:='Expediente Cobrado';
            
            elsif v.liqid is not null and v.liqid>10000 then

                    vDc.cdc:='1';
                    vdc.ddc:='Existe Liquidación » Generar Carta de Pago desda Caja';

            else
                    vDc.cdc:='0';
                    vdc.ddc:='Documento Cobratorio Generado Correctamente';
                    --
            end if;
            
            if vdc.cdc='0' then
                    
                    vLimitePago:=to_date((sysdate+10),'dd/mm/rrrr');
                    vDc.fLimitePago:=to_char(vLimitePago,'dd/mm/rrrr');
                    vDc.Emisora:='410913';
                    vDc.Referencia:=to_char(v.mulfec,'yy') || lpad(v.mulid,8,0);
                    vFechaEmision:=to_date(sysdate,'dd/mm/yyyy');
                    vDc.Identificacion:='1064'||to_char(vFechaEmision,'yy')||to_char(vLimitePago,'Y')
                                        ||to_char(vlimitepago,'DDD');
                                        
                    
                    /*if v.libsan=0 and v.aleg=0 then
                      if to_date(v.fdenuncia,'dd/mm/rrrr')>to_date('24/05/2010','dd/mm/rrrr') then
                        vImporte:=(round(v.importe*(50/100),2))*100;
                      else
                        vImporte:=(round(v.importe*(70/100),2))*100;
                      end if;
                    else
                        vImporte:=(round(v.importe,2))*100;
                    end if;*/
                    
                    -- 20120409 Control de la reducción del 50%
                    if v.fnotden is null
                    or (v.fnotden is not null and to_date(v.fnotden,'dd/mm/rrrr')+20>=trunc(sysdate)) then
                          if to_date(v.fdenuncia,'dd/mm/rrrr')>to_date('24/05/2010','dd/mm/rrrr') then
                            vImporte:=(round(v.importe*(50/100),2))*100;
                            vDescuento:=50;
                          else
                            vImporte:=(round(v.importe*(70/100),2))*100;
                            vDescuento:=30;
                          end if;
                    else 
                         vImporte:=(round(v.importe,2))*100;   
                         vDescuento:=0;
                    end if;
                    
                    --                    --
                    if v.origendenuncia in (4,6,7,10) then
                      vImporte:=(round(v.importe*(80/100),2))*100;
                      vDescuento:=20;
                    end if;
                    --                    --
                    if v.origendenuncia in (8) then
                      vImporte:=(round(v.importe*(70/100),2))*100;
                      vDescuento:=30;
                    end if;                    
                    --
                    if v.infid in (9002328,9002344) then
                      vImporte:=(round(v.importe,2))*100;
                      vDescuento:=0;
                    end if;
                    --
                    
                    --vDc.Importe2:=to_number(vDc.Importe);
                    vdc.importe3:=to_char(vimporte/100,'9,999.00');
                    vdc.importe2:=vimporte/100;
                    vDc.principal:=to_char(vimporte/100);
                    --vDc.Importe4:=to_number(vDc.Importe);
                    vDc.Importe4:=vDescuento;
                    
                    vTemp:=TO_CHAR(FLOOR(((((to_number(vDc.Emisora))*76)+((to_number(vDc.Referencia))*9)+
                       ((to_number(vDc.Identificacion)+(vImporte)-1)*55))/97)*100));
                    vDc.DC:=lpad(to_char(99-to_number(substr((vTemp),(length(vTemp)-1),2))),2,0);
                    
                    vDc.cbcuaderno60:=alba.codigo_de_barras('90521'||vDc.Emisora||vDc.Referencia||vDc.DC||vDc.Identificacion||LPAD((vImporte),8,0)||'0');
                    vDc.cbcuaderno60num:='90521'||vDc.Emisora||vDc.Referencia||vDc.DC||vDc.Identificacion||LPAD((vImporte),8,0)||'0';

                    vdc.referencia:=vdc.referencia||vdc.dc;
                    
            end if;
            
            
            
            vDc.Expediente:=v.nexpediente;
            vDc.Boletin:=v.boletin;
            --
            vDc.fDenuncia:=v.fDenuncia;
            vDc.Hora:=v.Hora;
            vDc.Matricula:=upper(v.matricula);
            vDc.Vehiculo:=upper(v.vehiculo);
            --
            vDc.nifD:=upper(v.nifd);
            vDc.nombreD:=upper(v.nombred);
            vDc.domicilioD:=upper(v.domiciliod);
            vDc.cpD:=upper(v.cpd);
            vDc.localidadD:=upper(v.localidadD);
            vDc.provinciaD:=upper(v.provinciaD);
            --
            vDc.informe:='MULDC.rpt';
            --
            vDc.lugar:=v.lugar;
            vdc.hecho:=v.hecho;
            
            -- En denuncias NO IDENTIFICACIÓN se muestra texto denuncia original
            for v3 in cDenOrg(vMulid) loop
              vDc.hecho:=v3.hecho;
            end loop;
          
            vDc.mulid:=v.mulid;
            
          
            vNoHayDatos:=false;
                
        end loop; --------------------------------------(1)
        
        if vNoHayDatos=false then
        
            for ii in cImportes(vDc.liqid) loop
            
              if ii.importe>0 then
              
                  vDc.principal:=ii.principal;
                  vDc.costas:=ii.costas;
                  vDc.apremio:=ii.apremio;
                  vdc.intereses:=ii.intereses;
                  vDc.acuenta:=ii.acuenta;
                  
              else
              
                  vDc.cdc:='2';
                  vdc.ddc:='Expediente Cobrado';
              
              end if;
              
            end loop;                
                    
            pipe row(vDc);
            
        end if;
        
       
        
        
        -- Recorre Multas
        for v2 in cMul2(vMulid) loop ---------------------(1)
        
            iniciarDocCobratorio;

            vDc.cdc:='0';
            vDc.ddc:='Documento Cobratorio Generado Correctamente';
            --
            vLimitePago:=to_date((sysdate+10),'dd/mm/rrrr');
            vDc.fLimitePago:=to_char(vLimitePago,'dd/mm/rrrr');
            vDc.Emisora:='410913';
            vDc.Referencia:=to_char(to_date(v2.fecha),'yy') || lpad(v2.mdcid,8,0);
            vFechaEmision:=to_date(sysdate,'dd/mm/yyyy');
            vDc.Identificacion:='1064'||to_char(vFechaEmision,'yy')||to_char(vLimitePago,'Y')
                                ||to_char(vLimitePago,'DDD');

            if to_date(v2.fecha,'dd/mm/rrrr')>to_date('24/05/2010','dd/mm/rrrr') then
              vimporte:=(round(v2.importe*(50/100),2))*100;
              vDescuento:=50;
            else
              vimporte:=(round(v2.importe*(70/100),2))*100;
              vDescuento:=30;
            end if;

            vDc.Importe3:=to_char(vImporte/100,'9,999.00');
            vDc.Importe2:=vImporte/100;
            vDc.Importe4:=vDescuento;

            vTemp:=TO_CHAR(FLOOR(((((to_number(vDc.Emisora))*76)+((to_number(vDc.Referencia))*9)+
            ((to_number(vDc.Identificacion)+(vImporte)-1)*55))/97)*100));
            vDc.DC:=lpad(to_char(99-to_number(substr((vTemp),(length(vTemp)-1),2))),2,0);
            
            vDc.cbcuaderno60:=alba.codigo_de_barras('90521'||vDc.Emisora||vDc.Referencia||vDc.DC||vDc.Identificacion||LPAD((vImporte),8,0)||'0');
            vDc.cbcuaderno60num:='90521'||vDc.Emisora||vDc.Referencia||vDc.DC||vDc.Identificacion||LPAD((vImporte),8,0)||'0';

            vDc.Referencia:=vDc.Referencia||vDc.Dc;

            vDc.Boletin:=v2.boletin;
            --
            vDc.fDenuncia:=v2.fecha;
            vDc.Matricula:=upper(v2.matricula);

            vDc.informe:='MULDC.rpt';
            vDc.hecho:=v2.hecho;


            -- En denuncias NO IDENTIFICACIÓN se muestra texto denuncia original
            for v3 in cDenOrg(vMulid) loop
              vDc.hecho:=v3.hecho;
            end loop;


            if vNoHayDatos then
                pipe row(vDc);
            end if;

            vNoHayDatos:=false;
                
        end loop; --------------------------------------(1)
        

        
        if vNoHayDatos then
        
            iniciarDocCobratorio;
            
            vDc.cdc:='1';
            vDc.ddc:='No hay datos para generar el Documento Cobratorio';
            
            pipe row(vDc);

        end if;

        return;
end;





/*******************************************************************/
/* Obtención de los informes asociados a un determinado libro-tipo */
/*******************************************************************/
Function obtenerInformesLibro(lLbrId in number) return tValores_lista pipelined as vDatos tValores;

    /* Cursores */
    cursor cInformes is
        select i.lriid, i.lriinforme, i.lridescripcion
        from alba.libro_resol_tipos t, alba.libro_resol_inf i, alba.libro_resoluciones l
        where l.lbrid = lLbrId
        and t.lrtid = l.lrtid
        and i.lrtid = t.lrtid;

begin

    for d in cInformes loop
    
        vDatos.id := d.lriid;
        vDatos.nombre := d.lriinforme;
        vDatos.descripcion := d.lridescripcion;
    
        pipe row(vDatos);
    
    end loop;

    return;

end;



/**********************************************************************/
/* Obtención de los datos a mostrar en el informe asociado a un libro */
/**********************************************************************/
Function informeLibroDeResolucion(lLbrId in number, sSujeto in varchar2,
    lEstado in number, bConsDetalle in number, bEsDetalle in number,
    lLriId in number) return tInformeLibro_lista pipelined as vDatos tInformeLibro;

    /* Cursores */
    
    -- Cursor dinámico
    type tipoCursor is ref cursor;
    cCursor tipoCursor;
    
    cursor cDatos1 is
       SELECT  f.nexpediente expediente, f.nombred infractor, f.hecho infraccion,
       f.importe sancion, NULL articulo, NULL apartado, l.lbrnumero libro,
       f.boletin, f.denunciante, f.lugar, f.fdenuncia fecha, f.hora,
       f.matricula, f.nifd nif, f.normativa licitacion,
       i.lriinforme informe, f.hechoorg motivo,
       decode(f.localidadd,f.provinciad,f.localidadd,f.localidadd || ' - ' || f.provinciad) ||
       decode(f.localidadd,null,null,' - ' || f.cpd) localidad,
       f.a1,f.b1,f.a2,f.b2,f.a3,f.b3,f.a4,f.b4,f.a5,f.b5,
       f.a6,f.b6,f.a7,f.b7,f.a8,f.b8,f.a9,f.b9,f.a10,f.b10,
       f.a11,f.b11,f.a12,f.b12,f.a13,f.b13,f.a14,f.b14,f.a15,f.b15,
       d.mulid,d.esmid,
       f.sancionFirme, f.finVoluntaria, f.inicioEjecutiva, d.dlbid
       FROM alba.det_libro_resol d,
            alba.libro_resoluciones l,
            alba.libro_resol_inf i,
            alba.libro_resol_tipos t,
            TABLE (trml.dmultas (d.mulid, d.esmid)) f
       WHERE l.lbrid = d.lbrid
       AND d.mulid = f.mulid
       And l.lrtid = t.lrtid
       And i.lrtid = t.lrtid
       And i.lriid = lLriId
       AND l.lbrid = lLbrId
       ORDER BY f.nexpediente;    
        
    cursor cDatos2 is
        SELECT d.esmid, e.nexpediente, e.nombred nombre, infarticulo, infapartado,
        infdescripcion AS infraccion, mulimp AS cuantia, deco.decdescr,
        lbrnumero,
        (SELECT res.resdescr FROM alba.resoluciones res
         WHERE res.resid = (SELECT rsa.resid
                     FROM alba.resol_alegaciones rsa
                    WHERE algid = (SELECT MAX (alg.algid)
                                     FROM alba.alegacionesrecursos alg
                                    WHERE seoid = multas.seoidexp)
                      AND ROWNUM < 2)) AS motivo,
        libro_resol_inf.lriinforme informe, d.dlbid, d.dlbfhoramod,
        libro_resoluciones.lbrid, d.expid, d.mulid,
        MULTAS.seoidexp,
        e.a1,e.b1,e.a2,e.b2,e.a3,e.b3,e.a4,e.b4,e.a5,e.b5,
        e.a6,e.b6,e.a7,e.b7,e.a8,e.b8,e.a9,e.b9,e.a10,e.b10,
        e.a11,e.b11,e.a12,e.b12,e.a13,e.b13,e.a14,e.b14,e.a15,e.b15      
        FROM alba.multas, alba.decoingresos deco, alba.infracciones inf, alba.det_libro_resol d,
        alba.libro_resoluciones left outer join alba.libro_resol_tipos
        on alba.libro_resoluciones.lrtid = alba.libro_resol_tipos.lrtid
        left outer join alba.libro_resol_inf
        on alba.libro_resol_inf.lrtid = alba.libro_resol_tipos.lrtid
        and alba.libro_resol_inf.lriid = lLriId,
        TABLE (trml.dmultas (multas.mulid, d.esmid)) e
        WHERE libro_resoluciones.lbrid = d.lbrid
        AND multas.infid = inf.infid
        AND multas.mulid = d.mulid
        AND libro_resoluciones.lbrid = lLbrId
        AND inf.infxlegislacion = deco.decid
        --and rownum<10
        AND multas.mulid = e.mulid;

    cursor cLiq2(vMulid number) is
        select e.notpa, e.fnotpa, e.liqid, e.liquidacion, e.importeLiqTexto,
        e.importeLiqNum, e.edictoApremio, e.fSancionFirmeLiq,
        e.fFinVoluntariaLiq, e.fInicioEjecutivaLiq, e.fnotsan
        from alba.liquidaciones l,table(trml.emultas(vMulid)) e
        where e.liqid=l.liqid;
       
    cursor cExpteTEA(vEsmid number) is
    select esmivalor
    from alba.escritosmultasinfo inf where inf.esmid=vEsmid and inf.esmietiqueta='EXPTE. TEA'
    and inf.esmiiestado=1 and rownum<2;
    
    cursor cPlenoTEA(vEsmid number) is
    select esmivalor
    from alba.escritosmultasinfo inf where inf.esmid=vEsmid and inf.esmietiqueta='PLENO'
    and inf.esmiiestado=1 and rownum<2;
    
    cursor cAsuntoTEA(vEsmid number) is
    select esmivalor
    from alba.escritosmultasinfo inf where inf.esmid=vEsmid and inf.esmietiqueta='ASUNTO'
    and inf.esmiiestado=1 and rownum<2;
  
  cursor cAsuntoTCA(vEsmid number) is
    select inf.esmobservaciones obs
    from alba.escritosmultas inf where inf.esmid=vEsmid
  and inf.esmobservaciones is not null
  and inf.tidomid=242 and inf.tdmresid in (641,642,681);
  
    cursor cResultadoTEA(vEsmid number) is
  SELECT
  r.tdmnombre resultado
  FROM 
  alba.tipos_doc_multas_res r,
  alba.escritosmultas e
  WHERE
  e.tdmresid=r.tdmresid
  AND e.esmid=vEsmid
  ;  
       
    cursor cimporteretiradapuntos (vlibro number) is
  select t.lrtid
  from alba.libro_resol_tipos t,
  alba.libro_resoluciones l
  where l.lrtid=t.lrtid
  and l.lbrid=vlibro
  and t.lrtid=1081
  ;
  
  cursor cLiqDetalle is
  select 
  substr(ep.EXPCOD,1,30) expediente,
  substr(ex.mexabrev,1,30) boletin, 
  substr(mexdesc,1,400) infraccion,
  alba.componer_nombre(p.pernombre, p.perapell1, p.perpart1,p.perapell2, p.perpart2, p.perrazon) infractor,
  nvl(p.periden, p.peridenext) nif,
  substr(to_char(ci.importe),1,20) sancion,
  substr(to_char(ci.dacuenta),1,20) impLiq,
  lq.liqnumerorecliquidacion liquidacion,
  l.lbrnumero libro,
  i.lriinforme informe,
  e.esmid
  FROM alba.det_libro_resol d,  alba.libro_resoluciones l,  alba.libro_resol_inf i,  alba.libro_resol_tipos t,
  alba.escritosmultas e,  alba.liquidaciones lq,  alba.expingre ep,  alba.exacciones ex,  alba.personas p,
  table(alba.pkg_calculoimportes.ctd(lq.liqid,sysdate)) ci
  WHERE ep.mexid=ex.mexid  and lq.expid=ep.expid  and e.perid=p.perid
  and e.perversion=p.perversion  and d.liqid=lq.liqid  and d.esmid=e.esmid
  and l.lbrid = d.lbrid  And l.lrtid = t.lrtid  And i.lrtid = t.lrtid
  and e.liqid=d.liqid  and e.perid=d.perid  and e.perversion=d.PERVERSION
  And i.lriid = lLriId  AND l.lbrid = lLbrId;  
  
  cursor cLiqDetalleResol(vEsmid number) is  
  select /*RULE*/ 
  ra.rsaid, e.esmid, e.mulid, e.esmfescrito fEscrito, t.tidomnombre tipoEscrito,
  e.esmfestado fResolucion, tr.tdmnombre tipoResultado, ra.algid detalleresolucion,
  nvl(tr.tdminforme,t.tidominforme) informe,
  ra.rsamotivoresol txtAlegacion, ra.rsaresolucion txtResolucion, r.resdescr motivo
  from 
  alba.tipos_doc_multas t, alba.tipos_doc_multas_res tr,
  alba.resoluciones r, alba.resol_alegaciones ra,
  alba.escritosmultas e
  where e.tidomid=t.tidomid
  and e.tdmresid=tr.tdmresid
  and ra.esmid=e.esmid
  and ra.resid=r.resid
  and e.esmid=vEsmid
  order by ra.rsaid asc;  

    /* Variables */
    datos cDatos2%rowtype;
    
    ri number:=0;
    
begin

    If bEsDetalle = 1 Then
         
        for d in cDatos1 loop
            
            vDatos.expediente := d.expediente;
            vDatos.infractor := d.infractor;
            vDatos.infraccion := d.infraccion;
            vDatos.sancion := d.sancion;
            vDatos.articulo := d.articulo;
            vDatos.apartado := d.apartado;
            vDatos.libro := d.libro;
            vDatos.boletin := d.boletin;
            vDatos.denunciante := d.denunciante;
            vDatos.lugar := d.lugar;
            vDatos.fecha := d.fecha;
            vDatos.hora := d.hora;
            vDatos.matricula := d.matricula;
            vDatos.nif := d.nif;
            vDatos.licitacion := d.licitacion;
            vDatos.informe := d.informe;
            vDatos.motivo := d.motivo;
            vDatos.localidad := d.localidad;
            vDatos.dlbid := d.dlbid;

            for ld in cLiq2(d.mulid) loop

                vDatos.liquidacion:=ld.liquidacion;
                --vDatos.sancion:=ld.importeLiqTexto;
                select substr(to_char(dacuenta),1,20), substr(to_char(importe),1,20) into vDatos.impLiq, vDatos.sancion
                from table(alba.pkg_calculoimportes.ctd(ld.liqid,sysdate));
                
                -- Datos Origen Liquidación
                /*vDatos.fSancionFirme:=ld.fSancionFirmeLiq;
                vDatos.fFinVoluntaria:=ld.fFinVoluntariaLiq;
                vDatos.fInicioEjecutiva:=ld.fInicioEjecutivaLiq;*/
                
                -- Datos Origen Función de Estado
                vDatos.fSancionFirme:=d.sancionFirme;
                vDatos.fFinVoluntaria:=d.finVoluntaria;
                vDatos.fInicioEjecutiva:=d.inicioEjecutiva;

                vDatos.fNotApremio:=ld.fnotpa;
                vDatos.edictoApremio:=ld.edictoApremio;
                vDatos.fnotsan:=ld.fnotsan;

            end loop;
     
            vDatos.a1:=d.a1;
            vDatos.b1:=substr(d.b1,1,4000);
            vDatos.a2:=d.a2;
            vDatos.b2:=substr(d.b2,1,4000);
            vDatos.a3:=d.a3;
            vDatos.b3:=substr(d.b3,1,4000);
            vDatos.a4:=d.a4;
            vDatos.b4:=substr(d.b4,1,4000);
            vDatos.a5:=d.a5;
            vDatos.b5:=substr(d.b5,1,4000);
            vDatos.a6:=d.a6;
            vDatos.b6:=substr(d.b6,1,4000);
            vDatos.a7:=d.a7;
            vDatos.b7:=substr(d.b7,1,4000);
            vDatos.a8:=d.a8;
            vDatos.b8:=substr(d.b8,1,4000);
            vDatos.a9:=d.a9;
            vDatos.b9:=substr(d.b9,1,4000);
            vDatos.a10:=d.a10;
            vDatos.b10:=substr(d.b10,1,4000);
            vDatos.a11:=d.a11;
            vDatos.b11:=substr(d.b11,1,4000);
            vDatos.a12:=d.a12;
            vDatos.b12:=substr(d.b12,1,4000);
            vDatos.a13:=d.a13;
            vDatos.b13:=substr(d.b13,1,4000);
            vDatos.a14:=d.a14;
            vDatos.b14:=substr(d.b14,1,4000);
            vDatos.a15:=d.a15;
            vDatos.b15:=substr(d.b15,1,4000);

          for et in cExpteTEA(d.esmid) loop
          
            vDatos.a1:=et.esmivalor;
          
          end loop;
      
          for rt in cResultadoTEA(d.esmid) loop
          
            vDatos.a1:=vDatos.a1 || ' ' || rt.resultado;
          
          end loop;      
          
          for pt in cPlenoTEA(d.esmid) loop
          
            vDatos.a2:=pt.esmivalor;
          
          end loop;
          
          for st in cAsuntoTEA(d.esmid) loop
          
            vDatos.a3:=st.esmivalor;
          
          end loop;    
      
          for stca in cAsuntoTCA(d.esmid) loop
          
            vDatos.a1:=stca.obs;
          
          end loop;          
      
      for irp in cimporteretiradapuntos(lLbrId) loop
      
        vDatos.sancion := d.sancion;
          
      end loop;
  
            pipe row(vDatos);

        end loop;
        
            
    else
                
        for d in cDatos2 loop

            vDatos.nexpediente := d.nexpediente;
            vDatos.nombre := d.nombre;
            vDatos.infarticulo := d.infarticulo;
            vDatos.infapartado := d.infapartado;
            vDatos.infraccion := d.infraccion;
            vDatos.cuantia := d.cuantia;
            vDatos.decdescr := d.decdescr;
            vDatos.lbrnumero := d.lbrnumero;
            vDatos.motivo := d.motivo;
            vDatos.informe := d.informe;
            vDatos.dlbid := d.dlbid;
            --
            
            if bConsDetalle = 1 then
                vDatos.dlbid := d.dlbid;
                vDatos.dlbfhoramod := d.dlbfhoramod;
                vDatos.lbrid := d.lbrid;
                vDatos.expid := d.expid;
                vDatos.mulid := d.mulid;
                vDatos.seoidexp := d.seoidexp;
            end if;
            
            vDatos.a1:=d.a1;
            vDatos.b1:=substr(d.b1,1,4000);
            vDatos.a2:=d.a2;
            vDatos.b2:=substr(d.b2,1,4000);
            vDatos.a3:=d.a3;
            vDatos.b3:=substr(d.b3,1,4000);
            vDatos.a4:=d.a4;
            vDatos.b4:=substr(d.b4,1,4000);
            vDatos.a5:=d.a5;
            vDatos.b5:=substr(d.b5,1,4000);
            vDatos.a6:=d.a6;
            vDatos.b6:=substr(d.b6,1,4000);
            vDatos.a7:=d.a7;
            vDatos.b7:=substr(d.b7,1,4000);
            vDatos.a8:=d.a8;
            vDatos.b8:=substr(d.b8,1,4000);
            vDatos.a9:=d.a9;
            vDatos.b9:=substr(d.b9,1,4000);
            vDatos.a10:=d.a10;
            vDatos.b10:=substr(d.b10,1,4000);
            vDatos.a11:=d.a11;
            vDatos.b11:=substr(d.b11,1,4000);
            vDatos.a12:=d.a12;
            vDatos.b12:=substr(d.b12,1,4000);
            vDatos.a13:=d.a13;
            vDatos.b13:=substr(d.b13,1,4000);
            vDatos.a14:=d.a14;
            vDatos.b14:=substr(d.b14,1,4000);
            vDatos.a15:=d.a15;
            vDatos.b15:=substr(d.b15,1,4000);
            
            pipe row(vDatos);

        end loop;

    end If;
    

    -----------------------------------
    -- Resoluciones sobre Liquidaciones
    -----------------------------------    
    for i in cLiqDetalle loop
        vDatos.expediente:=i.expediente;
        vDatos.boletin:=i.boletin;
        vDatos.infraccion:=i.infraccion;
        vDatos.nif:=i.nif;
        vDatos.infractor:=i.infractor;
        vDatos.sancion:=i.sancion;
        vDatos.impLiq:=i.impLiq;
        vDatos.liquidacion:=i.liquidacion;
        vDatos.libro:=i.libro;
        vDatos.informe:=i.informe;
        

        vDatos.articulo:=null;vDatos.apartado:=null;vDatos.lbrnumero:=null;vDatos.denunciante:=null;
        vDatos.lugar:=null;vDatos.fecha:=null;vDatos.hora:=null;vDatos.matricula:=null;
        vDatos.licitacion:=null;vDatos.motivo:=null;vDatos.localidad:=null;vDatos.nexpediente:=null;
        vDatos.nombre:=null;vDatos.infarticulo:=null;vDatos.infapartado:=null;vDatos.cuantia:=null;
        vDatos.decdescr:=null;vDatos.dlbid:=null;vDatos.dlbfhoramod:=null;vDatos.lbrid:=null;
        vDatos.expid:=null;vDatos.mulid:=null;vDatos.seoidexp:=null;vDatos.fSancionFirme:=null;
        vDatos.fFinVoluntaria:=null;vDatos.fInicioEjecutiva:=null;vDatos.fNotApremio:=null;
        vDatos.edictoApremio:=null;vDatos.fnotsan:=null;
        
        vDatos.a1:=null;vDatos.b1:=null;vDatos.a2:=null;vDatos.b2:=null;
        vDatos.a3:=null;vDatos.b3:=null;vDatos.a4:=null;vDatos.b4:=null;
        vDatos.a5:=null;vDatos.b5:=null;vDatos.a6:=null;vDatos.b6:=null;
        vDatos.a7:=null;vDatos.b7:=null;vDatos.a8:=null;vDatos.b8:=null;
        vDatos.a9:=null;vDatos.b9:=null;vDatos.a10:=null;vDatos.b10:=null;
        vDatos.a11:=null;vDatos.b11:=null;vDatos.a12:=null;vDatos.b12:=null;
        vDatos.a13:=null;vDatos.b13:=null;vDatos.a14:=null;vDatos.b14:=null;
        vDatos.a15:=null;vDatos.b15:=null;
        
        ri:=1;
        for r in cLiqDetalleResol(i.esmid) loop
        
                if ri=1 then
                    vDatos.a1:=substr(r.txtAlegacion,1,15000);
                    vDatos.b1:=substr(r.txtresolucion,1,15000);
                    vDatos.motivo:=substr(r.motivo,1,1000);
                elsif ri=2 then
                    vDatos.a2:=substr(r.txtAlegacion,1,15000);
                    vDatos.b2:=substr(r.txtresolucion,1,15000);
                elsif ri=3 then
                    vDatos.a3:=substr(r.txtAlegacion,1,15000);
                    vDatos.b3:=substr(r.txtresolucion,1,15000);
                elsif ri=4 then
                    vDatos.a4:=substr(r.txtAlegacion,1,15000);
                    vDatos.b4:=substr(r.txtresolucion,1,15000);
                elsif ri=5 then
                    vDatos.a5:=substr(r.txtAlegacion,1,15000);
                    vDatos.b5:=substr(r.txtresolucion,1,15000);
                elsif ri=6 then
                    vDatos.a6:=substr(r.txtAlegacion,1,15000);
                    vDatos.b6:=substr(r.txtresolucion,1,15000);
                elsif ri=7 then
                    vDatos.a7:=substr(r.txtAlegacion,1,15000);
                    vDatos.b7:=substr(r.txtresolucion,1,15000);
                elsif ri=8 then
                    vDatos.a8:=substr(r.txtAlegacion,1,15000);
                    vDatos.b8:=substr(r.txtresolucion,1,15000);
                elsif ri=9 then
                    vDatos.a9:=substr(r.txtAlegacion,1,15000);
                    vDatos.b9:=substr(r.txtresolucion,1,15000);
                elsif ri=10 then
                    vDatos.a10:=substr(r.txtAlegacion,1,15000);
                    vDatos.b10:=substr(r.txtresolucion,1,15000);
                elsif ri=11 then
                    vDatos.a11:=substr(r.txtAlegacion,1,15000);
                    vDatos.b11:=substr(r.txtresolucion,1,15000);
                elsif ri=12 then
                    vDatos.a12:=substr(r.txtAlegacion,1,15000);
                    vDatos.b12:=substr(r.txtresolucion,1,15000);
                elsif ri=13 then
                    vDatos.a13:=substr(r.txtAlegacion,1,15000);
                    vDatos.b13:=substr(r.txtresolucion,1,15000);
                elsif ri=14 then
                    vDatos.a14:=substr(r.txtAlegacion,1,15000);
                    vDatos.b14:=substr(r.txtresolucion,1,15000);
                elsif ri=15 then
                    vDatos.a15:=substr(r.txtAlegacion,1,15000);
                    vDatos.b15:=substr(r.txtresolucion,1,15000);
       
                end if;
                ri:=ri+1;
        
        end loop;        
        
        pipe row(vDatos);
        
    end loop;

    return;
    
end;


function ficheroMultas(vCrmid number) return fReimpresion_lista pipelined as vDatos fReimpresion;

    /* Cursores */
    cursor cDatos is
        SELECT TIPONOT,FLIMITEPAGO,EMISORA,REFERENCIA,
        DC,IDENTIFICACION,IMPORTE,NEXPEDIENTE,BOLETIN,
        FINCOACION,LUGAR,FDENUNCIA,HORA,MATRICULA,VEHICULO,
        NIFD,NOMBRED,DOMICILIOD,CPD,LOCALIDADD,PROVINCIAD,
        DENUNCIANTE,MOTIVONOENTREGA,HECHO,NORMATIVA,CALIFICACION,
        TIEMPORETIRADA,NIFT,NOMBRET,DOMICILIOT,CPT,LOCALIDADT,
        PROVINCIAT,VELOCIDAD,LIMITEVELOCIDAD,TEXTOCINEMOMETRO,
        ALCOHOLAIRE,ALCOHOLSANGRE,FNOTDENUNICA,LUGARORG,FECHAORG,
        HORAORG,MATRICULAORG,HECHOORG,CBSICER,CBCUADERNO60,
        firma,a1,b1,a2,b2,a3,b3,a4,b4,a5,b5,
        a6,b6,a7,b7,a8,b8,a9,b9,a10,b10,
        a11,b11,a12,b12,a13,b13,a14,b14,a15,b15,
        recibo,sancionFirme,finVoluntaria,inicioEjecutiva,
        importeSancionFirme,providenciaApremio,
        intentoNotPA,edictoPA,bopPa,
        numeroLibroApremio, fResolucionLibroApremio, caja, lote, indice,
        fot01, fot02, fot03, fot04
        from table(trml.fmultas(vCrmid));
        
textoTmp varchar2(30000);

begin


    for d in cDatos loop
    
        --vDatos:=d;
        
        textoTmp:='';
        
        vDatos.TIPONOT := d.TIPONOT;
        vDatos.FLIMITEPAGO  := d.FLIMITEPAGO;
        vDatos.EMISORA  := d.EMISORA;
        vDatos.REFERENCIA  := d.REFERENCIA;
        vDatos.DC := d.DC;
        vDatos.IDENTIFICACION := d.IDENTIFICACION;
        vDatos.IMPORTE := d.IMPORTE;
        vDatos.NEXPEDIENTE := d.NEXPEDIENTE;
        vDatos.BOLETIN := d.BOLETIN;
        vDatos.FINCOACION := d.FINCOACION;
        vDatos.LUGAR := d.LUGAR;
        vDatos.FDENUNCIA := d.FDENUNCIA;
        vDatos.HORA := d.HORA;
        vDatos.MATRICULA := d.MATRICULA;
        vDatos.VEHICULO := d.VEHICULO;
        vDatos.NIFD := d.NIFD;
        vDatos.NOMBRED := d.NOMBRED;
        vDatos.DOMICILIOD := d.DOMICILIOD;
        vDatos.CPD := d.CPD;
        vDatos.LOCALIDADD := d.LOCALIDADD;
        vDatos.PROVINCIAD := d.PROVINCIAD;
        vDatos.DENUNCIANTE := d.DENUNCIANTE;
        vdatos.motivonoentrega := d.motivonoentrega;
        vdatos.hecho := replace(replace(d.hecho,chr(13),''),chr(10),'');
        vDatos.NORMATIVA := replace(replace(d.NORMATIVA,chr(13),''),chr(10),'');
        vDatos.CALIFICACION := d.CALIFICACION;
        vDatos.TIEMPORETIRADA := d.TIEMPORETIRADA;
        vDatos.NIFT := d.NIFT;
        vDatos.NOMBRET := d.NOMBRET;
        vDatos.DOMICILIOT := d.DOMICILIOT;
        vDatos.CPT := d.CPT;
        vDatos.LOCALIDADT := d.LOCALIDADT;
        vDatos.PROVINCIAT := d.PROVINCIAT;
        vDatos.VELOCIDAD := d.VELOCIDAD;
        vDatos.LIMITEVELOCIDAD := d.LIMITEVELOCIDAD;
        vDatos.TEXTOCINEMOMETRO := d.TEXTOCINEMOMETRO;
        vDatos.ALCOHOLAIRE := d.ALCOHOLAIRE;
        vDatos.ALCOHOLSANGRE := d.ALCOHOLSANGRE;
        vDatos.FNOTDENUNICA := d.FNOTDENUNICA;
        vDatos.LUGARORG := d.LUGARORG;
        vDatos.FECHAORG := d.FECHAORG;
        vDatos.HORAORG := d.HORAORG;
        vDatos.MATRICULAORG := d.MATRICULAORG;
        vDatos.HECHOORG := d.HECHOORG;
        vDatos.CBSICER := d.CBSICER;
        vDatos.CBCUADERNO60 := d.CBCUADERNO60;
        vDatos.firma := d.firma;

        vDatos.a1 := d.a1;
        vDatos.b11 := substr(d.b1,1,4000); vDatos.b12 := substr(d.b1,4001,4000); vDatos.b13 := substr(d.b1,8001,4000); vDatos.b14 := substr(d.b1,12001,4000);
        vDatos.a2 := d.a2;
        vDatos.b21 := substr(d.b2,1,4000); vDatos.b22 := substr(d.b2,4001,4000); vDatos.b23 := substr(d.b2,8001,4000); vDatos.b24 := substr(d.b2,12001,4000);
        vDatos.a3 := d.a3;
        vDatos.b31 := substr(d.b3,1,4000); vDatos.b32 := substr(d.b3,4001,4000); vDatos.b33 := substr(d.b3,8001,4000); vDatos.b34 := substr(d.b3,12001,4000);
        vDatos.a4 := d.a4;
        vDatos.b41 := substr(d.b4,1,4000); vDatos.b42 := substr(d.b4,4001,4000); vDatos.b43 := substr(d.b4,8001,4000); vDatos.b44 := substr(d.b4,12001,4000);
        vDatos.a5 := d.a5;
        vDatos.b51 := substr(d.b5,1,4000); vDatos.b52 := substr(d.b5,4001,4000); vDatos.b53 := substr(d.b5,8001,4000); vDatos.b54 := substr(d.b5,12001,4000);
        vDatos.a6 := d.a6;
        vDatos.b61 := substr(d.b6,1,4000); vDatos.b62 := substr(d.b6,4001,4000); vDatos.b63 := substr(d.b6,8001,4000); vDatos.b64 := substr(d.b6,12001,4000);
        vDatos.a7:= d.a7;
        vDatos.b71 := substr(d.b7,1,4000); vDatos.b72 := substr(d.b7,4001,4000); vDatos.b73 := substr(d.b7,8001,4000); vDatos.b74 := substr(d.b7,12001,4000);
        vDatos.a8 := d.a8;
        vDatos.b81 := substr(d.b8,1,4000); vDatos.b82 := substr(d.b8,4001,4000); vDatos.b83 := substr(d.b8,8001,4000); vDatos.b84 := substr(d.b8,12001,4000);
        vDatos.a9 := d.a9;
        vDatos.b91 := substr(d.b9,1,4000); vDatos.b92 := substr(d.b9,4001,4000); vDatos.b93 := substr(d.b9,8001,4000); vDatos.b94 := substr(d.b9,12001,4000);
        vDatos.a10 := d.a10;
        vDatos.b101 := substr(d.b10,1,4000); vDatos.b102 := substr(d.b10,4001,4000); vDatos.b103 := substr(d.b10,8001,4000); vDatos.b104 := substr(d.b10,12001,4000);
        vDatos.a11 := d.a11;
        vDatos.b111 := substr(d.b11,1,4000); vDatos.b112 := substr(d.b11,4001,4000); vDatos.b113 := substr(d.b11,8001,4000); vDatos.b114 := substr(d.b11,12001,4000);
        vDatos.a12 := d.a12;
        vDatos.b121 := substr(d.b12,1,4000); vDatos.b122 := substr(d.b12,4001,4000); vDatos.b123 := substr(d.b12,8001,4000); vDatos.b124 := substr(d.b12,12001,4000);
        vDatos.a13 := d.a13;
        vDatos.b131 := substr(d.b13,1,4000); vDatos.b132 := substr(d.b13,4001,4000); vDatos.b133 := substr(d.b13,8001,4000); vDatos.b134 := substr(d.b13,12001,4000);
        vDatos.a14 := d.a14;
        vDatos.b141 := substr(d.b14,1,4000); vDatos.b142 := substr(d.b14,4001,4000); vDatos.b143 := substr(d.b14,8001,4000); vDatos.b144 := substr(d.b14,12001,4000);
        vDatos.a15 := d.a15;
        vDatos.b151 := substr(d.b15,1,4000); vDatos.b152 := substr(d.b15,4001,4000); vDatos.b153 := substr(d.b15,8001,4000); vDatos.b154 := substr(d.b15,12001,4000);
      
        -- Ejecutiva
        vDatos.recibo:=d.recibo;
        vDatos.sancionFirme:=d.sancionFirme;
        vDatos.finVoluntaria:=d.finVoluntaria;
        vDatos.inicioEjecutiva:=d.inicioEjecutiva;
        vDatos.importeSancionFirme:=d.importeSancionFirme;
        vDatos.providenciaApremio:=d.providenciaApremio;
        vDatos.intentoNotPA:=d.intentoNotPA;
        vDatos.edictoPA:=d.edictoPA;
        vDatos.bopPa:=d.bopPa;
        vDatos.numeroLibroApremio:=d.numeroLibroApremio;
        vDatos.fResolucionLibroApremio:=d.fResolucionLibroApremio;
        --
        vDatos.caja:=d.caja;
        vDatos.lote:=d.lote;
        vDatos.indice:=d.indice;
        
        --vDatos.fot01:=d.fot01;
        --vDatos.fot02:=d.fot02;
        --vDatos.fot03:=d.fot03;
        --vDatos.fot04:=d.fot04;
        

        pipe row(vDatos);

    end loop;
    
    return;

end;



function fbMultas(vCrmid number) return clob is

    /* Cursores */
    cursor cDatos is
        SELECT TIPONOT,FLIMITEPAGO,EMISORA,REFERENCIA,
        DC,IDENTIFICACION,IMPORTE,NEXPEDIENTE,BOLETIN,
        FINCOACION,LUGAR,FDENUNCIA,HORA,MATRICULA,VEHICULO,
        NIFD,NOMBRED,DOMICILIOD,CPD,LOCALIDADD,PROVINCIAD,
        DENUNCIANTE,MOTIVONOENTREGA,HECHO,NORMATIVA,CALIFICACION,
        TIEMPORETIRADA,NIFT,NOMBRET,DOMICILIOT,CPT,LOCALIDADT,
        PROVINCIAT,VELOCIDAD,LIMITEVELOCIDAD,TEXTOCINEMOMETRO,
        ALCOHOLAIRE,ALCOHOLSANGRE,FNOTDENUNICA,LUGARORG,FECHAORG,
        HORAORG,MATRICULAORG,HECHOORG,CBSICER,CBCUADERNO60,
        firma,a1,b1,a2,b2,a3,b3,a4,b4,a5,b5,
        a6,b6,a7,b7,a8,b8,a9,b9,a10,b10,
        a11,b11,a12,b12,a13,b13,a14,b14,a15,b15,
        recibo,sancionFirme,finVoluntaria,inicioEjecutiva,
        importeSancionFirme,providenciaApremio,
        intentoNotPA,edictoPA,bopPa,
        numeroLibroApremio, fResolucionLibroApremio, caja, lote, indice,
        fot01, fot02, fot03, fot04
        from table(trml.fmultas(vCrmid));
        
vClob clob;

begin

    
    vClob:=to_clob('');    
    for d in cDatos loop
    
        vClob:=vClob || to_clob(d.TIPONOT);
        vClob:=vClob || to_clob('¬' || d.FLIMITEPAGO);

        vClob:=vClob || to_clob('¬' || d.EMISORA);
        vClob:=vClob || to_clob('¬' || d.REFERENCIA);
        vClob:=vClob || to_clob('¬' || d.DC);
        vClob:=vClob || to_clob('¬' || d.IDENTIFICACION);
        vClob:=vClob || to_clob('¬' || d.IMPORTE);
        vClob:=vClob || to_clob('¬' || d.NEXPEDIENTE);
        vClob:=vClob || to_clob('¬' || d.BOLETIN);
        vClob:=vClob || to_clob('¬' || d.FINCOACION);
        vClob:=vClob || to_clob('¬' || d.LUGAR);
        vClob:=vClob || to_clob('¬' || d.FDENUNCIA);
        vClob:=vClob || to_clob('¬' || d.HORA);
        vClob:=vClob || to_clob('¬' || d.MATRICULA);
        vClob:=vClob || to_clob('¬' || d.VEHICULO);
        vClob:=vClob || to_clob('¬' || d.NIFD);
        vClob:=vClob || to_clob('¬' || d.NOMBRED);
        vClob:=vClob || to_clob('¬' || d.DOMICILIOD);
        vClob:=vClob || to_clob('¬' || d.CPD);
        vClob:=vClob || to_clob('¬' || d.LOCALIDADD);
        vClob:=vClob || to_clob('¬' || d.PROVINCIAD);
        vClob:=vClob || to_clob('¬' || d.DENUNCIANTE);
        vClob:=vClob || to_clob('¬' || d.motivonoentrega);
        vClob:=vClob || to_clob('¬' || replace(replace(d.hecho,chr(13),''),chr(10),''));
        vClob:=vClob || to_clob('¬' || replace(replace(d.NORMATIVA,chr(13),''),chr(10),''));
        vClob:=vClob || to_clob('¬' || d.CALIFICACION);
        vClob:=vClob || to_clob('¬' || d.TIEMPORETIRADA);
        vClob:=vClob || to_clob('¬' || d.NIFT);
        vClob:=vClob || to_clob('¬' || d.NOMBRET);
        
        vClob:=vClob || to_clob('¬' || d.DOMICILIOT);
        vClob:=vClob || to_clob('¬' || d.CPT);
        vClob:=vClob || to_clob('¬' || d.LOCALIDADT);
        vClob:=vClob || to_clob('¬' || d.PROVINCIAT);
        vClob:=vClob || to_clob('¬' || d.VELOCIDAD);
        vClob:=vClob || to_clob('¬' || d.LIMITEVELOCIDAD);
        vClob:=vClob || to_clob('¬' || d.TEXTOCINEMOMETRO);
        vClob:=vClob || to_clob('¬' || d.ALCOHOLAIRE);
        vClob:=vClob || to_clob('¬' || d.ALCOHOLSANGRE);
        vClob:=vClob || to_clob('¬' || d.FNOTDENUNICA);
        vClob:=vClob || to_clob('¬' || d.LUGARORG);
        vClob:=vClob || to_clob('¬' || d.FECHAORG);
        vClob:=vClob || to_clob('¬' || d.HORAORG);
        vClob:=vClob || to_clob('¬' || d.MATRICULAORG);
        vClob:=vClob || to_clob('¬' || d.HECHOORG);
        vClob:=vClob || to_clob('¬' || d.CBSICER);
        vClob:=vClob || to_clob('¬' || d.CBCUADERNO60);
        vClob:=vClob || to_clob('¬' || d.firma);
        
        vClob:=vClob || to_clob('¬' || d.a1);
        vClob:=vClob || to_clob('¬' || d.b1);
        vClob:=vClob || to_clob('¬' || d.a2);
        vClob:=vClob || to_clob('¬' || d.b2);
        vClob:=vClob || to_clob('¬' || d.a3);
        vClob:=vClob || to_clob('¬' || d.b3);
        vClob:=vClob || to_clob('¬' || d.a4);
        vClob:=vClob || to_clob('¬' || d.b4);
        vClob:=vClob || to_clob('¬' || d.a5);
        vClob:=vClob || to_clob('¬' || d.b5);
        
        if d.TIPONOT='7.0' then
        
            vClob:=vClob || to_clob('¬' || d.a6);
            vClob:=vClob || to_clob('¬' || d.b6);
            vClob:=vClob || to_clob('¬' || d.a7);
            vClob:=vClob || to_clob('¬' || d.b7);
            vClob:=vClob || to_clob('¬' || d.a8);
            vClob:=vClob || to_clob('¬' || d.b8);
            vClob:=vClob || to_clob('¬' || d.a9);
            vClob:=vClob || to_clob('¬' || d.b9);
            vClob:=vClob || to_clob('¬' || d.a10);
            vClob:=vClob || to_clob('¬' || d.b10);
            vClob:=vClob || to_clob('¬' || d.a11);
            vClob:=vClob || to_clob('¬' || d.b11);
            vClob:=vClob || to_clob('¬' || d.a12);
            vClob:=vClob || to_clob('¬' || d.b12);
            vClob:=vClob || to_clob('¬' || d.a13);
            vClob:=vClob || to_clob('¬' || d.b13);
            vClob:=vClob || to_clob('¬' || d.a14);
            vClob:=vClob || to_clob('¬' || d.b14);
            vClob:=vClob || to_clob('¬' || d.a15);
            vClob:=vClob || to_clob('¬' || d.b15);
        
        end if;
        
        -- Ejecutiva
        vClob:=vClob || to_clob('¬' || d.recibo);
        vClob:=vClob || to_clob('¬' || d.sancionFirme);
        vClob:=vClob || to_clob('¬' || d.finVoluntaria);
        vClob:=vClob || to_clob('¬' || d.inicioEjecutiva);
        vClob:=vClob || to_clob('¬' || d.importeSancionFirme);
        vClob:=vClob || to_clob('¬' || d.providenciaApremio);
        vClob:=vClob || to_clob('¬' || d.intentoNotPA);
        vClob:=vClob || to_clob('¬' || d.edictoPA);
        vClob:=vClob || to_clob('¬' || d.bopPa);
        vClob:=vClob || to_clob('¬' || d.numeroLibroApremio);
        vClob:=vClob || to_clob('¬' || d.fResolucionLibroApremio);
        --
        vClob:=vClob || to_clob('¬' || d.caja);
        vClob:=vClob || to_clob('¬' || d.lote);
        vClob:=vClob || to_clob('¬' || d.indice);
        
        vClob:=vClob || to_clob('¬' || d.fot01);
        vClob:=vClob || to_clob('¬' || d.fot02);
        vClob:=vClob || to_clob('¬' || d.fot03);
        vClob:=vClob || to_clob('¬' || d.fot04);
        
        -- Retorno de carro
        vClob:=vClob || to_clob(chr(13) || chr(10));

    end loop;
    
    return vClob;

end;


function ficheroIndexacionMultas(vLbrid number) return fReimpresion_lista pipelined as vDatos fReimpresion;

    /* Cursores */
    cursor cDatos is
    select TIPONOT,FLIMITEPAGO,EMISORA,REFERENCIA,
    DC,IDENTIFICACION,IMPORTE,NEXPEDIENTE,BOLETIN,
    FINCOACION,LUGAR,FDENUNCIA,HORA,MATRICULA,VEHICULO,
    NIFD,NOMBRED,DOMICILIOD,CPD,LOCALIDADD,PROVINCIAD,
    DENUNCIANTE,MOTIVONOENTREGA,HECHO,NORMATIVA,CALIFICACION,
    TIEMPORETIRADA,NIFT,NOMBRET,DOMICILIOT,CPT,LOCALIDADT,
    PROVINCIAT,VELOCIDAD,LIMITEVELOCIDAD,TEXTOCINEMOMETRO,
    ALCOHOLAIRE,ALCOHOLSANGRE,FNOTDENUNICA,LUGARORG,FECHAORG,
    HORAORG,MATRICULAORG,HECHOORG,CBSICER,CBCUADERNO60,
    firma,a1,b1,a2,b2,a3,b3,a4,b4,a5,b5,
    a6,b6,a7,b7,a8,b8,a9,b9,a10,b10,
    a11,b11,a12,b12,a13,b13,a14,b14,a15,b15,
    recibo,sancionFirme,finVoluntaria,inicioEjecutiva,
    importeSancionFirme,providenciaApremio,
    intentoNotPA,edictoPA,bopPa,
    numeroLibroApremio, fResolucionLibroApremio
    from alba.libro_resoluciones l,
    alba.det_libro_resol d,
    table(trml.dmultas(d.mulid,0)) e
    where d.lbrid=l.lbrid
    and d.mulid=e.mulid
    and l.lbrid=vLbrid;

begin

    for d in cDatos loop

        vDatos.TIPONOT := d.TIPONOT;
        vDatos.FLIMITEPAGO  := d.FLIMITEPAGO;
        vDatos.EMISORA  := d.EMISORA;
        vDatos.REFERENCIA  := d.REFERENCIA;
        vDatos.DC := d.DC;
        vDatos.IDENTIFICACION := d.IDENTIFICACION;
        vDatos.IMPORTE := d.IMPORTE;
        vDatos.NEXPEDIENTE := d.NEXPEDIENTE;
        vDatos.BOLETIN := d.BOLETIN;
        vDatos.FINCOACION := d.FINCOACION;
        vDatos.LUGAR := d.LUGAR;
        vDatos.FDENUNCIA := d.FDENUNCIA;
        vDatos.HORA := d.HORA;
        vDatos.MATRICULA := d.MATRICULA;
        vDatos.VEHICULO := d.VEHICULO;
        vDatos.NIFD := d.NIFD;
        vDatos.NOMBRED := d.NOMBRED;
        vDatos.DOMICILIOD := d.DOMICILIOD;
        vDatos.CPD := d.CPD;
        vDatos.LOCALIDADD := d.LOCALIDADD;
        vDatos.PROVINCIAD := d.PROVINCIAD;
        vDatos.DENUNCIANTE := d.DENUNCIANTE;
        vDatos.MOTIVONOENTREGA := d.MOTIVONOENTREGA;
        vDatos.HECHO := d.HECHO;
        vDatos.NORMATIVA := d.NORMATIVA;
        vDatos.CALIFICACION := d.CALIFICACION;
        vDatos.TIEMPORETIRADA := d.TIEMPORETIRADA;
        vDatos.NIFT := d.NIFT;
        vDatos.NOMBRET := d.NOMBRET;
        vDatos.DOMICILIOT := d.DOMICILIOT;
        vDatos.CPT := d.CPT;
        vDatos.LOCALIDADT := d.LOCALIDADT;
        vDatos.PROVINCIAT := d.PROVINCIAT;
        vDatos.VELOCIDAD := d.VELOCIDAD;
        vDatos.LIMITEVELOCIDAD := d.LIMITEVELOCIDAD;
        vDatos.TEXTOCINEMOMETRO := d.TEXTOCINEMOMETRO;
        vDatos.ALCOHOLAIRE := d.ALCOHOLAIRE;
        vDatos.ALCOHOLSANGRE := d.ALCOHOLSANGRE;
        vDatos.FNOTDENUNICA := d.FNOTDENUNICA;
        vDatos.LUGARORG := d.LUGARORG;
        vDatos.FECHAORG := d.FECHAORG;
        vDatos.HORAORG := d.HORAORG;
        vDatos.MATRICULAORG := d.MATRICULAORG;
        vDatos.HECHOORG := d.HECHOORG;
        vDatos.CBSICER := d.CBSICER;
        vDatos.CBCUADERNO60 := d.CBCUADERNO60;
        vDatos.firma := d.firma;
       
        /*vDatos.a1 := d.a1;
        vDatos.b1 := d.b1;
        vDatos.a2 := d.a2;
        vDatos.b2 := d.b2;
        vDatos.a3 := d.a3;
        vDatos.b3 := d.b3;
        vDatos.a4 := d.a4;
        vDatos.b4 := d.b4;
        vDatos.a5 := d.a5;
        vDatos.b5 := d.b5;
        vDatos.a6 := d.a6;
        vDatos.b6 := d.b6;
        vDatos.a7 := d.a7;
        vDatos.b7 := d.b7;
        vDatos.a8 := d.a8;
        vDatos.b8 := d.b8;
        vDatos.a9 := d.a9;
        vDatos.b9 := d.b9;
        vDatos.a10 := d.a10;
        vDatos.b10 := d.b10;
        vDatos.a11 := d.a11;
        vDatos.b11 := d.b11;
        vDatos.a12 := d.a12;
        vDatos.b12 := d.b12;
        vDatos.a13 := d.a13;
        vDatos.b13 := d.b13;
        vDatos.a14 := d.a14;
        vDatos.b14 := d.b14;
        vDatos.a15 := d.a15;
        vDatos.b15 := d.b15;*/
       
        vDatos.a1 := d.a1;
        vDatos.b11 := substr(d.b1,1,4000); vDatos.b12 := substr(d.b1,4001,4000); vDatos.b13 := substr(d.b1,8001,4000); vDatos.b14 := substr(d.b1,12001,4000);
        vDatos.a2 := d.a2;
        vDatos.b21 := substr(d.b2,1,4000); vDatos.b22 := substr(d.b2,4001,4000); vDatos.b23 := substr(d.b2,8001,4000); vDatos.b24 := substr(d.b2,12001,4000);
        vDatos.a3 := d.a3;
        vDatos.b31 := substr(d.b3,1,4000); vDatos.b32 := substr(d.b3,4001,4000); vDatos.b33 := substr(d.b3,8001,4000); vDatos.b34 := substr(d.b3,12001,4000);
        vDatos.a4 := d.a4;
        vDatos.b41 := substr(d.b4,1,4000); vDatos.b42 := substr(d.b4,4001,4000); vDatos.b43 := substr(d.b4,8001,4000); vDatos.b44 := substr(d.b4,12001,4000);
        vDatos.a5 := d.a5;
        vDatos.b51 := substr(d.b5,1,4000); vDatos.b52 := substr(d.b5,4001,4000); vDatos.b53 := substr(d.b5,8001,4000); vDatos.b54 := substr(d.b5,12001,4000);
        vDatos.a6 := d.a6;
        vDatos.b61 := substr(d.b6,1,4000); vDatos.b62 := substr(d.b6,4001,4000); vDatos.b63 := substr(d.b6,8001,4000); vDatos.b64 := substr(d.b6,12001,4000);
        vDatos.a7:= d.a7;
        vDatos.b71 := substr(d.b7,1,4000); vDatos.b72 := substr(d.b7,4001,4000); vDatos.b73 := substr(d.b7,8001,4000); vDatos.b74 := substr(d.b7,12001,4000);
        vDatos.a8 := d.a8;
        vDatos.b81 := substr(d.b8,1,4000); vDatos.b82 := substr(d.b8,4001,4000); vDatos.b83 := substr(d.b8,8001,4000); vDatos.b84 := substr(d.b8,12001,4000);
        vDatos.a9 := d.a9;
        vDatos.b91 := substr(d.b9,1,4000); vDatos.b92 := substr(d.b9,4001,4000); vDatos.b93 := substr(d.b9,8001,4000); vDatos.b94 := substr(d.b9,12001,4000);
        vDatos.a10 := d.a10;
        vDatos.b101 := substr(d.b10,1,4000); vDatos.b102 := substr(d.b10,4001,4000); vDatos.b103 := substr(d.b10,8001,4000); vDatos.b104 := substr(d.b10,12001,4000);
        vDatos.a11 := d.a11;
        vDatos.b111 := substr(d.b11,1,4000); vDatos.b112 := substr(d.b11,4001,4000); vDatos.b113 := substr(d.b11,8001,4000); vDatos.b114 := substr(d.b11,12001,4000);
        vDatos.a12 := d.a12;
        vDatos.b121 := substr(d.b12,1,4000); vDatos.b122 := substr(d.b12,4001,4000); vDatos.b123 := substr(d.b12,8001,4000); vDatos.b124 := substr(d.b12,12001,4000);
        vDatos.a13 := d.a13;
        vDatos.b131 := substr(d.b13,1,4000); vDatos.b132 := substr(d.b13,4001,4000); vDatos.b133 := substr(d.b13,8001,4000); vDatos.b134 := substr(d.b13,12001,4000);
        vDatos.a14 := d.a14;
        vDatos.b141 := substr(d.b14,1,4000); vDatos.b142 := substr(d.b14,4001,4000); vDatos.b143 := substr(d.b14,8001,4000); vDatos.b144 := substr(d.b14,12001,4000);
        vDatos.a15 := d.a15;
        vDatos.b151 := substr(d.b15,1,4000); vDatos.b152 := substr(d.b15,4001,4000); vDatos.b153 := substr(d.b15,8001,4000); vDatos.b154 := substr(d.b15,12001,4000);
       
     
        -- Ejecutiva
        vDatos.recibo:=d.recibo;
        vDatos.sancionFirme:=d.sancionFirme;
        vDatos.finVoluntaria:=d.finVoluntaria;
        vDatos.inicioEjecutiva:=d.inicioEjecutiva;
        vDatos.importeSancionFirme:=d.importeSancionFirme;
        vDatos.providenciaApremio:=d.providenciaApremio;
        vDatos.intentoNotPA:=d.intentoNotPA;
        vDatos.edictoPA:=d.edictoPA;
        vDatos.bopPa:=d.bopPa;
        vDatos.numeroLibroApremio:=d.numeroLibroApremio;
        vDatos.fResolucionLibroApremio:=d.fResolucionLibroApremio;


        pipe row(vDatos);

    end loop;

    return;

end;

-- Vínvulo mult@web
function expteWeb(expte varchar2) return varchar2 is

begin

    return '<a href="/RsToTable/expediente.jsp?usu=jjDO946ik'||chr(38)||'pas=f50HJkf6AS'||chr(38)||'uw=busform'||chr(38)||'Expediente=' || expte || '"  target="_blank" >' || expte || '</a>';

end;

-- Vínvulo mult@web
function expteWeb2(expte varchar2, usuario varchar2) return varchar2 is

begin

    return '<a href="/RsToTable/expediente.jsp?usu=jjDO946ik'||chr(38)||'pas=f50HJkf6AS'||chr(38)||'uw='|| usuario ||chr(38)||'Expediente=' || expte || '">' || expte || '</a>';

end;

-- Vínvulo mult@web
function bolWeb(bol varchar2) return varchar2 is

begin

    return '<a href="/RsToTable/expediente.jsp?usu=jjDO946ik'||chr(38)||'pas=f50HJkf6AS'||chr(38)||'uw=busform'||chr(38)||'Expediente=' || bol || '"  target="_blank" >' || bol || '</a>';

end;



-- Vínvulo escritos
function esmidWeb(vEsmid number) return varchar2 is

    cursor cMul(vId number) is
    select e.esmcaja, e.esmlote, e.esmindice
    from alba.escritosmultas e
    where e.esmid=vId;
    
    vEsmcaja number:=0;
    vEsmlote number:=0;
    vEsmindice number:=0;

begin

    for m in cMul(vEsmid) loop
        vEsmcaja:=m.esmcaja;
        vEsmlote:=m.esmlote;
        vEsmindice:=m.esmindice; 
    end loop;
    
    return '<a href="https://www.datasur.com.es/AGMUR6/ASP/resultados_multas.asp?'
    ||'caja='||vEsmcaja||chr(38)
    ||'lote='||vEsmlote||chr(38)
    ||'secuencia='||vEsmindice||chr(38)
    ||'bd=MULTAS'||chr(38)    
    ||'clave=albagd'
    || '"  target="_blank" >' 
    || vEsmid || '</a>';    

end;

function esmidWebResol(vEsmid number) return varchar2 is

    cursor cMul(vId number) is
    select e.esmcaja, e.esmlote, e.esmindice, decode(e.tidomid,101,'AMRAS','MULTAS') tipo
    from alba.escritosmultas e
    where e.esmid=vId;
    
    vEsmcaja number:=0;
    vEsmlote number:=0;
    vEsmindice number:=0;
    vTipo varchar2(20):='';

begin

    for m in cMul(vEsmid) loop
        vEsmcaja:=m.esmcaja;
        vEsmlote:=m.esmlote;
        vEsmindice:=m.esmindice; 
        vTipo:=m.tipo;
    end loop;
    
    return 'https://www.datasur.com.es/AGMUR6/ASP/getpdf_se.aspx?'
    ||'caja='||vEsmcaja||chr(38)
    ||'lote='||vEsmlote||chr(38)
    ||'secuencia='||vEsmindice||chr(38)
    || 'bd='||vTipo||chr(38)    
    ||'clave=albagd';    

end;


-- Vínvulo mult@web
function mulidWeb(mulid number) return varchar2 is

  cursor cMul(vId number) is
    select distinct e.expcod
    from alba.multas m, alba.expingre e
    where m.mulid=vId
    and m.expidexp=e.expid;
    --and m.mulversion=(select max(mm.mulversion) from alba.multas mm where mm.seoidexp=m.seoidexp);
    --and e.mexid=21;

expte varchar2(20):='';
i number:=0;

begin

    for m in cMul(mulid) loop
        expte:=m.expcod;
        i:=i+1;
    end loop;
    

    return '<a href="/RsToTable/expediente.jsp?usu=jjDO946ik'||chr(38)||'pas=f50HJkf6AS'||chr(38)||'uw=busform'||chr(38)||'Expediente=' || expte || '"  target="_blank" >' || expte || '</a>';

end;

-- Carga de Fichero de Escritos
function cargaFicheroEscritos(vId number) return varchar2 is

        
        cursor cLiq(vEficfid number) is
        select f.eficfid, d.eregid, e.mulid, d.eregboletin bol, d.eregexpediente expte
        from alba.personas p,
        alba.multasestados e,
        alba.escritosmultasfic f, alba.escritosmultasrec d
        where d.eficfid=f.eficfid
        and d.eficfid=vEficfid
        and to_char(d.eregexpediente)=to_char(e.liquidacion)
        and (p.perid=e.peridt or p.perid=e.peridc)
        and trim(leading '0' from upper(p.periden))=trim(leading '0' from upper(d.eregnifcifconductor))
        and d.mulid is null
        and d.esmid is null
        and d.eregtipoescrito='1';
        
        cursor cLiqNoMul(vEficfid number) is
        select f.eficfid, d.eregid, 0 mulid, null bol, d.eregexpediente expte
        from 
        alba.expingre e,
        alba.liquidaciones l,
        alba.escritosmultasfic f, 
        alba.escritosmultasrec d
        where d.eficfid=f.eficfid
        and d.eficfid=vEficfid
        and l.expid=e.EXPID
        and ( to_char(d.eregexpediente)=to_char(l.LIQNUMERORECLIQUIDACION)
        or to_char(d.eregexpediente)=e.EXPCOD )
        and trim(leading '0' from upper(d.eregnifcifconductor))=trim(leading '0' from upper(alba.dni_exp(l.EXPID, 'SUJETOPASIVO')))
        and e.mexid not in (1,4,21)
        and d.mulid is null
        and d.esmid is null
        and d.eregtipoescrito='1';
        
        cursor cEsc(vEficfid number) is
        --TEA
        select f.eficfid, d.eregid, e.mulid, d.eregboletin bol, d.eregexpediente expte
        from --alba.personas p,
        alba.multasestados e,
        alba.escritosmultasfic f, alba.escritosmultasrec d
        where d.eficfid=f.eficfid
        and d.eficfid=vEficfid
        and to_char(d.eregexpediente)=to_char(e.liquidacion)
        --and (p.perid=e.peridt or p.perid=e.peridc)
        --and (upper(p.periden)=upper(d.eregnifcifconductor) or upper(p.peridenext)=upper(d.eregnifcifconductor))
        and d.mulid is null
        and d.esmid is null
        and d.eregtipoescrito='3'
        union
        --Ejecución TEA
        select f.eficfid, d.eregid, e.mulid, d.eregboletin bol, d.eregexpediente expte
        from --alba.personas p,
        alba.multasestados e,
        alba.escritosmultasfic f, alba.escritosmultasrec d
        where d.eficfid=f.eficfid
        and d.eficfid=vEficfid
        and to_char(d.eregexpediente)=to_char(e.liquidacion)
        --and (p.perid=e.peridt or p.perid=e.peridc)
        --and (upper(p.periden)=upper(d.eregnifcifconductor) or upper(p.peridenext)=upper(d.eregnifcifconductor))
        and d.mulid is null
        and d.esmid is null
        and d.eregtipoescrito='4'        
        union
        --BOLETÍN
        select f.eficfid, d.eregid, e.mulid, d.eregboletin bol, d.eregexpediente expte
        from --alba.personas p,
        alba.multasestados e,
        alba.escritosmultasfic f, alba.escritosmultasrec d
        where d.eficfid=f.eficfid
        and d.eficfid=vEficfid
        and trim(leading '0' from upper(to_char(d.eregboletin+0)))=trim(leading '0' from upper(to_char(e.mulnumbol)))
        --and (p.perid=e.peridt or p.perid=e.peridc)
        --and trim(leading '0' from upper(p.periden))=trim(leading '0' from upper(d.eregnifcifconductor))
        and d.mulid is null
        and d.esmid is null
        and d.eregtipoescrito='1'
        union
        --EXPEDIENTE
        select f.eficfid, d.eregid, e.mulid, d.eregboletin bol, d.eregexpediente expte
        from alba.personas p,
        alba.multasestados e,
        alba.escritosmultasfic f, alba.escritosmultasrec d
        where d.eficfid=f.eficfid
        and d.eficfid=vEficfid
        and to_char(d.eregexpediente)=to_char(e.expediente)
        --and (p.perid=e.peridt or p.perid=e.peridc)
        --and trim(leading '0' from upper(p.periden))=trim(leading '0' from upper(d.eregnifcifconductor))
        and d.mulid is null
        and d.esmid is null
        and d.eregtipoescrito='1'
        union
        --IDENTIFICACIÓN DE CONCUTOR
        select f.eficfid, d.eregid, e.mulid, d.eregboletin bol, d.eregexpediente expte
        from alba.escritosmultasfic f, alba.escritosmultasrec d, alba.multasestados e
        where d.eficfid=f.eficfid
        and d.eficfid=vEficfid
        and trim(leading '0' from upper(to_char(d.eregboletin+0)))=trim(leading '0' from upper(to_char(e.mulnumbol)))
        and d.mulid is null
        and d.eregtipoescrito='2'
        union
        select f.eficfid, d.eregid, e.mulid, d.eregboletin bol, d.eregexpediente expte
        from alba.escritosmultasfic f, alba.escritosmultasrec d, alba.multasestados e
        where d.eficfid=f.eficfid
        and d.eficfid=vEficfid
        and to_char(d.eregexpediente)=to_char(e.expediente)
        and d.mulid is null
        and d.esmid is null
        and d.eregtipoescrito='2';
        

        cursor cEsc2(vEficfid number) is
        select f.eficfid, d.eregid, trim(leading '0' from upper(d.eregnifcifconductor)),
        d.eregboletin bol, d.eregexpediente expte,
        max(p.perid) perid, max(p.perversion) perversion
        from alba.escritosmultasfic f, alba.escritosmultasrec d, alba.personas p
        where d.eficfid=f.eficfid
        and d.eficfid=vEficfid
        and trim(leading '0' from upper(p.periden))=trim(leading '0' from upper(d.eregnifcifconductor))
        and d.perid is null
        and d.mulid is null
        and d.esmid is null        
        group by f.eficfid, d.eregid, trim(leading '0' from upper(d.eregnifcifconductor)),
        d.eregboletin, d.eregexpediente;

        cursor cEsc3(vEficfid number) is
        select f.eficfid, d.eregid, trim(leading '0' from upper(d.eregnifcifconductor)) nif, d.eregnombre nombre, d.eregape1 ape1,
        d.eregape2 ape2, 
        CASE WHEN substr(trim(leading '0' from upper(d.eregnifcifconductor)),1,1) IN ('K','L','1','2','3','4','5','6','7','8','9','0')
        THEN 2
        ELSE
          CASE WHEN substr(trim(leading '0' from upper(d.eregnifcifconductor)),1,1) IN ('A','B','C','D','E','F','G','H','J')
          THEN 1
          else
            CASE WHEN substr(trim(leading '0' from upper(d.eregnifcifconductor)),1,1) IN ('P','Q','R','S')
            THEN 3
            else
              CASE WHEN substr(trim(leading '0' from upper(d.eregnifcifconductor)),1,1) IN ('X','Y','Z')
              THEN 4
              ELSE 5
              END
            END
          END
        end tipoPersona,
        d.eregboletin bol, d.eregexpediente expte
        from alba.escritosmultasfic f, alba.escritosmultasrec d
        WHERE d.eficfid=f.eficfid
        and d.eficfid=vEficfid
        AND d.perid IS NULL
        and d.mulid is null
        and d.esmid is null        
        AND trim(d.eregnombre) IS NOT NULL
        and eregnifcifconductor is not null;

        cursor cEsc4(vEficfid number) is
        select f.eficfid, d.eregid,
        d.eregdomicilioconductor domicilio,
        d.eregcpconductor cp,
        d.ereglocalidadconductor localidad,
        d.eregprovinciaconductor provincia,
        d.eregboletin bol, d.eregexpediente expte
        from alba.escritosmultasfic f, alba.escritosmultasrec d
        where d.eficfid=f.eficfid
        and d.eficfid=vEficfid
        and d.eregdomicilioconductor is not null
        and d.rdiid is null
        and d.mulid is null
        and d.esmid is null;
        --and d.rdiid=0;
        
        cursor cCP(localidad varchar2, provincia varchar2) is
        select cp from
        (select c.codid, l.munid, p.mprid, c.codposcodigo cp,
        l.mundesc localidad, p.mprdesc provincia
        from alba.m_codpostal c, alba.municipios l, alba.provincia p
        where c.munid=l.munid
        and l.mprid=p.mprid
        and l.mundesc=upper(localidad)
        and p.mprdesc=upper(provincia)
        order by c.codid desc)
        where rownum<2;


        cursor cEsc5(vEficfid number) is
        select
        eficfid, eregid, mulid, fescrito, 
        decode(tdmresid,323,21,361,1,321,1,322,2,tidomid) tidomid,
        tdmresid, perid, perversion, rdiid,
        esmid, boletin,
        esmcaja, esmlote, esmindice        
        from
        (
        select g.eficfid, g.eregid, g.mulid, g.fescrito, g.tidomid,
        decode(tidomid,21,41,1,22,41,63,2,23,3,24,101,181,62,301,21323,323,1361,361,1321,321,2322,322,141,721,null,null) tdmresid, g.perid, g.perversion, g.rdiid,
        g.esmid, g.boletin,
        g.esmcaja, g.esmlote, g.esmindice
        from
        (
            select f.*,
            case when tipoEscrito=3 --REA
            then 101 --REA            
            else
              case when tipoEscrito=4 --EJECUCIÓN TEA
              then 141
              else
                    case when mulid is null -- NO SE HA GRABADO LA DENUNCIA
                    then --No existe mulid, por tanto la denuncia no se ha grabado
                                case when tipoEscrito=2 --Identificación de conductor
                                then 21 --ALEGACIÓN
                                else 1 -- IDENTIFICACIÓN DE CONDUCTOR
                                end
                    else
                                case when tipoEscrito=2 --Identificación de conductor
                                then 
                                          --COMPROBAR: 20 días naturades como máximo desde la fecha de notificación de la denuncia
                                          case when to_date(fescrito,'dd/mm/rrrr')<=to_date(fnotificacionDenuncia,'dd/mm/rrrr')+20
                                          then 21 -- Identificación de conductor
                                          else 21323 -- Identificación Extemporarea
                                          end
                                else -- tipoEscrito=1
                                          case when to_date(fescrito,'dd/mm/rrrr')>to_date(flibroSancionFirme,'dd/mm/rrrr')
                                          and flibroSancionFirme is not null
                                          -- Si la fecha del escrito es posterior al libro de sanción firme 
                                          then
                                                   case when notpa>0 --hay providencia de apremio
                                                   and to_date(fescrito,'dd/mm/rrrr')>=to_date(femisionnotpa,'dd/mm/rrrr')
                                                   and femisionnotpa is not null
                                                   then 41 --RPA
                                                   else 3 -- Recurso ER                                       
                                                   end
                                          else
                                                  case when to_date(fescrito,'dd/mm/rrrr')>to_date(flibrec,'dd/mm/rrrr')
                                                  and flibrec is not null
                                                  then  3 -- Recurso ER
                                                  else -- decide si es alegación o recurso
                                                      
                                                      case when csl>0 and origendenuncia not in (4,6,7,8,9)
                                                      then 1361 --Alegación No Presentada
                                                      else
                                                            case when to_date(fescrito,'dd/mm/rrrr')>=to_date(flibroSancion,'dd/mm/rrrr')
                                                            and flibroSancion is not null
                                                            then 
                                                                  case when to_date(fescrito,'dd/mm/rrrr')>add_months(to_date(fnotificacionSancion,'dd/mm/rrrr'),1)
                                                                  and flibroSancion is not null
                                                                  then 2322 -- Recurso extemporaneo                                                        
                                                                  else 2 -- Recurso
                                                                  end 
                                                            else  -- Alegación
                                                            --COMPROBAR: 20 días naturades como máximo desde la fecha de notificación de la denuncia
                                                                    case when to_date(fescrito,'dd/mm/rrrr')<=to_date(fnotificacionDenuncia,'dd/mm/rrrr')+20
                                                                    then 1 -- Alegción
                                                                    else 1321 -- Alegación Extemporánea
                                                                    end                                                      
                                                            end
                                                      end
                                                  end
                                          end
                                  
                                end
                      end
                end
            end tidomid
            from
            (
                select f.eficfid, d.eregid, e.mulid, d.eregtipoescrito tipoEscrito,
                d.eregfechadocumento fescrito,
                nvl(e.flibaleg,e.flibsan) flibroSancion,
                e.flibsanfir flibroSancionFirme,
                e.flibsan,e.flibaleg, e.flibrec, e.flibsanfir,
                d.perid, d.perversion, d.rdiid, d.esmid, e.mulnumbol boletin,
                d.eregcaja esmcaja, d.ereglote esmlote, d.eregindice esmindice,
                e.fnotinfq femisionnotpa, e.notpa, f.eficffproceso fgrabacionescrito, e.aleg,
                nvl(nvl(nvl(nvl(e.fnotden,e.flibbopden),e.fnotreq),e.flibbopreq),sysdate) fnotificacionDenuncia,
                nvl(nvl(e.fnotsan,e.flibbopsan),sysdate+365) fnotificacionSancion,            
                d.eregtipoescrito, e.csl, e.origendenuncia
                from alba.multas m, alba.escritosmultasfic f, alba.escritosmultasrec d, table(trml.emultas(d.mulid)) e
                where d.eficfid=f.eficfid
                and d.eficfid=vEficfid
                and d.mulid=m.mulid
                --and m.mulfec<to_date('25/05/2010','dd/mm/yyyy')
                and d.mulid=e.mulid(+)
            ) f
        ) g
        where esmid is null and boletin is not null
        );
        
        cursor cEsc6(vEficfid number) is
        select f.eficfid, d.eregid, d.mulid, d.eregtipoescrito tipoEscrito,
        d.eregfechadocumento fescrito,
        d.perid, d.perversion, d.rdiid, d.esmid, nvl(nvl(d.eregexpediente,d.eregboletin),to_number('99000'||d.eregcaja||d.ereglote||d.eregindice)) boletin,
        d.eregcaja esmcaja, d.ereglote esmlote, d.eregindice esmindice,
        decode(d.eregtipoescrito,4,141,3,101,62) tidomid, decode(d.eregtipoescrito,4,721,3,183,143) tdmresid
        from alba.escritosmultasfic f, alba.escritosmultasrec d
        where d.eficfid=f.eficfid
        and d.eficfid=vEficfid
        and d.esmid is null;
        
        
        cursor cDAE(vId number) is
        select
        --cabecera
        decode(e.tidomid,41,956,1156) DACID,
        null ACT_AEJID,
        'L' AEJOBJETO,
        decode(e.tidomid,41,'Interposición RPA','Interposición REA') AEJDESCR,
        null AEJESTADO,
        to_date (to_char(e.esmfescrito,'dd/mm/rrrr') || ' 00:00:00', 'DD/MM/YYYY HH24:MI:SS') AEJFCREACION,
        0 AEJIMPORTECOSTAS,
        'E' AEJMONEDA,
        null AEJFAPRUEBA,
        null AEJFPETICIONNOT,
        'N' AEJNOTIFICAPERREL,
        'N' AEJCARTAPAGO,
        to_date (to_char(e.esmfescrito,'dd/mm/rrrr') || ' 00:00:00', 'DD/MM/YYYY HH24:MI:SS') AEJFHORAMOD,
        'Se ha creado' AEJMOTIVO,
        null AEJFVIGENCIA,
        f.eficfnombre AEJCODIGO,
        null AEJFECHAVTO,
        null AEJFPROVI,       
        -- detalle
        alba.detallesactuacionesejecu_sq.nextval daeid,
        l.liqid,
        null dxeid,
        sysdate daefhoramod,
        0 usuidmod,
        'Se ha creado' daemotivo,
        null daefvigencia,
        null daeimporte,
        to_date (to_char(e.esmfescrito,'dd/mm/rrrr') || ' 00:00:00', 'DD/MM/YYYY HH24:MI:SS') daefnotifsa,
        null pntid
        from 
        alba.liquidaciones l,
        alba.multas m,
        alba.escritosmultas e,
        alba.escritosmultasrec r,        
        alba.escritosmultasfic f
        where f.eficfid=vId
        and e.tidomid in (41,101)
        and r.eficfid=f.eficfid
        and e.esmid=r.esmid
        and e.mulid=m.mulid
        and m.expidexp=l.expid;
        
        

        vPerid number:=0;
        vPerversion number:=0;
        vRdiid number:=0;
        vEsmid number:=0;
        
        vLog varchar2(1000):='';
        vCP varchar2(100):='';
        vEficfnombre varchar2(300):='';
        
        vAejid number:=0;
        
        vPerid_aux number:=0;
        vPerversion_aux number:=0;
        vPConId_aux number:=0;
        
        vTidomid number:=0;
        vTdmresid number:=0;
begin

        --completar mulid's y otras referencias
        
        select f.eficfnombre into vEficfnombre
        from alba.escritosmultasfic f
        where f.eficfid=vId;
        
        
        vLog:='1. Obtención de Liquidación » Selección de Datos';
        for e1 in cLiq(vId) loop
          vLog:='1. Obtención de Liquidaciones » Actualización de Datos » bol | exp » ' || e1.bol || ' | ' || e1.expte;
            update alba.escritosmultasrec e
            set e.mulid=e1.mulid
            where e.eregid=e1.eregid;
        end loop;        
        
        vLog:='1. Obtención de Liquidación Otros Tributos » Selección de Datos';
        for e1 in cLiqNoMul(vId) loop
          vLog:='1. Obtención de Liquidaciones » Actualización de Datos » bol | exp » ' || e1.bol || ' | ' || e1.expte;
            update alba.escritosmultasrec e
            set e.mulid=e1.mulid
            where e.eregid=e1.eregid;
        end loop;             
        
        vLog:='1. Obtención de Referencias » Selección de Datos';
        for e1 in cEsc(vId) loop
          vLog:='1. Obtención de Referencias » Actualización de Datos » bol | exp » ' || e1.bol || ' | ' || e1.expte;
            update alba.escritosmultasrec e
            set e.mulid=e1.mulid
            where e.eregid=e1.eregid;
        end loop;

        --localización de personas ya existentes en Alba
        vLog:='2. Localización de Personas » Selección de Datos';
        for e2 in cEsc2(vId) loop
        

            vLog:='2. Localización de Personas » Actualización de Datos » bol | exp » ' || e2.bol || ' | ' || e2.expte;
            update alba.escritosmultasrec e
            set e.perid=e2.perid,
            e.perversion=e2.perversion
            where e.eregid=e2.eregid;
        end loop;

        --alta de nuevas personas
        vLog:='3. Alta de Personas » Selección de Datos';
        for e3 in cEsc3(vId) loop
        
            vLog:='3. Alta de Personas » Actualización de Datos » bol | exp » ' || e3.bol || ' | ' || e3.expte;

            vPerid:=trml.obtenerPersona(e3.nif,e3.nombre,e3.ape1,e3.ape2,vPerversion,e3.tipoPersona);
            
            vLog:=vLog || ' » ' || e3.nif||' '||e3.nombre||' '||e3.ape1||' '||e3.ape2;

            update alba.escritosmultasrec e
            set e.perid=vPerid,
            e.perversion=vPerversion
            where e.eregid=e3.eregid;

        end loop;

        --alta direcciones
        vLog:='4. Alta de Direcciones » Selección de Datos';
        for e4 in cEsc4(vId) loop
        
            vLog:='4. Alta de Direcciones » Actualización de Datos » bol | exp » ' || e4.bol || ' | ' || e4.expte;
            
            -- Localiza CP si viene vacío.
            if e4.cp is null or e4.cp='' then
                for w in cCp(e4.localidad,e4.provincia) loop
                    vCP:=substr(w.cp,1,90);
                end loop;
            else
                vCP:=e4.cp;
            end if;
            
            if e4.domicilio is not null --and e4.domicilio<>''
            and e4.localidad is not null --and e4.localidad<>''
            and e4.provincia is not null --and e4.provincia<>''
            and vCP is not null --and vCP<>''
            then
                vRdiid:=0;
                --vRdiid:=trml.obtenerdireccion(e4.domicilio,e4.localidad,e4.provincia,vCP);
                alba.PAQ_MULTAS.alta_Persona_Direccion ('N','S',
                0,--pPerid number
                0,--pPerVersion number
                '',--pNif varchar2
                '',--pNombre varchar2
                '',--pApellido1 varchar2
                '',--pApellido2 varchar2
                0,--pMexId number
                0,--pOrigenPersona number
                sysdate,--pSysdate date
                'Carga Escritos',--pMotivo varchar2
                0,--pUsuIdMod number
                e4.provincia,--pProvincia varchar2
                e4.localidad,--pLocalidad varchar2
                vCP,--pCp varchar2
                e4.domicilio,--pDomicilio varchar2
                0,--pOrigenAcceso number
                vRdiid,--pRdiid in out number
                vPerid_aux,--pPeridN in out number
                vPerversion_aux,--pPerVersionN in out number
                vPConId_aux);--pConId in out number)
                
                update alba.escritosmultasrec e
                set e.rdiid=vRdiid
                where e.eregid=e4.eregid;                
                
            end if;
            
            vLog:=vLog || ' » ' || e4.domicilio||' '||e4.localidad||' '||e4.provincia||' '||vCP;



        end loop;

        --insercción en la tabla principal de escritos
        vLog:='5. Alta de Escritos » Selección de Datos';
        for e5 in cEsc5(vId) loop
        
                vLog:='5. Alta de Escritos » Actualización de Datos » bol» ' || e5.boletin;

                select alba.escritosmultas_sq.NEXTVAL into vEsmid from dual;
                
                vLog:=vLog || ' » ' || e5.boletin||' » CAJA¬LOTE¬INDICE: '||e5.esmcaja||'¬'||e5.esmlote||'¬'||e5.esmindice;

                insert into alba.escritosmultas a
                   (a.esmid, a.esmnumbol, a.esmxtipo, a.esmfgrabacion, a.esmfescrito,
                   a.esmobservaciones, a.esmfhoramod, a.usuidmod, a.mulid,
                   a.esmmotivo, a.tidomid, a.tdmresid, a.esmfestado, a.perid,
                   a.perversion, a.rdiid, a.eregid, a.eficfid, a.esmcaja, a.esmlote,
                   a.esmindice)
                values
                   (vEsmid, e5.boletin, 0, sysdate, e5.fescrito,
                   null,sysdate,0,e5.mulid,
                   'Carga Fichero', e5.tidomid, e5.tdmresid,sysdate, e5.perid,
                   e5.perversion, e5.rdiid, e5.eregid, e5.eficfid, e5.esmcaja, e5.esmlote,
                   e5.esmindice);
                   
                 vLog:=vLog || ' » Update';            
            
                update alba.escritosmultasrec e
                set e.esmid=vEsmid
                where e.eregid=e5.eregid;
        end loop;


        --insercción en la tabla principal de escritos. ESCRITOS SIN REFERENCIA.
        vLog:='6. Alta de Escritos Perdidos» Selección de Datos';
        for e6 in cEsc6(vId) loop
        
                vLog:='6. Alta de Escritos » Actualización de Datos » bol» ' || e6.boletin;

                select alba.escritosmultas_sq.NEXTVAL into vEsmid from dual;
                
                vLog:=vLog || ' » ' || e6.boletin||' » CAJA¬LOTE¬INDICE: '||e6.esmcaja||'¬'||e6.esmlote||'¬'||e6.esmindice;
                
                if e6.mulid=0 then -- RPA Otros Tributos.
                    vTidomid:=41; 
                    vTdmresid:=701;                      
                else
                    vTidomid:=e6.tidomid; 
                    vTdmresid:=e6.tdmresid;
                end if;

                insert into alba.escritosmultas a
                   (a.esmid, a.esmnumbol, a.esmxtipo, a.esmfgrabacion, a.esmfescrito,
                   a.esmobservaciones, a.esmfhoramod, a.usuidmod, a.mulid,
                   a.esmmotivo, a.tidomid, a.tdmresid, a.esmfestado, a.perid,
                   a.perversion, a.rdiid, a.eregid, a.eficfid, a.esmcaja, a.esmlote,
                   a.esmindice)
                values
                   (vEsmid, e6.boletin, 0, sysdate, e6.fescrito,
                   null,sysdate,0,null,
                   'Carga Fichero', vTidomid, vTdmresid,sysdate, e6.perid,
                   e6.perversion, e6.rdiid, e6.eregid, e6.eficfid, e6.esmcaja, e6.esmlote,
                   e6.esmindice);
                   
                 vLog:=vLog || ' » Update';
            
                update alba.escritosmultasrec e
                set e.esmid=vEsmid
                where e.eregid=e6.eregid;
        end loop;
        
        

        
        -- Genera Actuación Ejecutiva
        for dae in cDAE(vId) loop
        
            -- Crea Cabecera Actuación Ejecutiva
            select alba.actuacionesejecutiva_sq.nextval into vAejid from dual;        
        
            insert into alba.actuacionesejecutiva ae2
              (ae2.aejid, ae2.DACID,ae2.ACT_AEJID,ae2.AEJOBJETO,ae2.AEJDESCR,ae2.AEJESTADO,ae2.AEJFCREACION,
              ae2.AEJIMPORTECOSTAS,ae2.AEJMONEDA,ae2.AEJFAPRUEBA,ae2.AEJFPETICIONNOT,
              ae2.AEJNOTIFICAPERREL,ae2.AEJCARTAPAGO,ae2.AEJFHORAMOD,ae2.USUIDMOD,
              ae2.AEJMOTIVO,ae2.AEJFVIGENCIA,ae2.AEJCODIGO,ae2.AEJFECHAVTO,ae2.AEJFPROVI)
            values
              (vAejid, dae.DACID,dae.ACT_AEJID,dae.AEJOBJETO,dae.AEJDESCR,dae.AEJESTADO,dae.AEJFCREACION,
              dae.AEJIMPORTECOSTAS,dae.AEJMONEDA,dae.AEJFAPRUEBA,dae.AEJFPETICIONNOT,
              dae.AEJNOTIFICAPERREL,dae.AEJCARTAPAGO,dae.AEJFHORAMOD,dae.USUIDMOD,
              dae.AEJMOTIVO,dae.AEJFVIGENCIA,dae.AEJCODIGO,dae.AEJFECHAVTO,dae.AEJFPROVI);
        
        

            -- DETALLE PRESENTACIÓN DE ESCRITO
            insert into alba.detallesactuacionesejecu dae2
                (dae2.daeid,dae2.aejid,dae2.liqid,dae2.dxeid,
                dae2.daefhoramod,dae2.usuidmod,dae2.daemotivo,dae2.daefvigencia,
                dae2.daeimporte,dae2.daefnotifsa,dae2.pntid)
            values
                (alba.detallesactuacionesejecu_sq.nextval,vAejid,dae.liqid,dae.dxeid,
                dae.daefhoramod,dae.usuidmod,dae.daemotivo,dae.daefvigencia,
                dae.daeimporte,dae.daefnotifsa,dae.pntid);
                
        end loop;        
        
        
        
        
           dbms_output.put_line('OK');
           vLog:='OK';
           
           return vLog;
           
           --alba.utl_alba.penvia_mail('CargarEscrito','jgomez.agenrec@sevilla.org','OK. Cargar Escrito ' || vEficfnombre,'OK.','','','');
           
           commit;


----------------------------------------------------------------
-- FINALIZACION DEL PROCESO CON EXCEPCION
----------------------------------------------------------------
exception when others then

           dbms_output.put_line('KO. ' || sqlerrm);
           
           --alba.utl_alba.penvia_mail('CargarEscrito','jgomez.agenrec@sevilla.org','KO. Cargar Escrito ' || vEficfnombre,vLog,'','','');
           
           return vLog;
           
           rollback;
end;



function recuperaDireccionesEscritos return varchar2 is

        cursor cEsc4 is
        select f.eficfid, d.eregid,
        d.eregdomicilioconductor domicilio,
        d.eregcpconductor cp,
        d.ereglocalidadconductor localidad,
        d.eregprovinciaconductor provincia,
        d.eregboletin bol, d.eregexpediente expte,
        d.esmid
        from alba.escritosmultasfic f, alba.escritosmultasrec d
        where d.eficfid=f.eficfid
        and d.eregdomicilioconductor is not null
        and d.rdiid=0;

        cursor cCP(localidad varchar2, provincia varchar2) is
        select cp from
        (select c.codid, l.munid, p.mprid, c.codposcodigo cp,
        l.mundesc localidad, p.mprdesc provincia
        from alba.m_codpostal c, alba.municipios l, alba.provincia p
        where c.munid=l.munid
        and l.mprid=p.mprid
        and l.mundesc=upper(localidad)
        and p.mprdesc=upper(provincia)
        order by c.codid desc)
        where rownum<2;


        vPerid number:=0;
        vPerversion number:=0;
        vRdiid number:=0;
        vEsmid number:=0;

        vLog varchar2(1000):='';
        vCP varchar2(100):='';
        vTotalEscritos number:=0;

begin


        --alta direcciones
        vLog:='4. Alta de Direcciones » Selección de Datos';
        for e4 in cEsc4 loop

            vLog:='4. Alta de Direcciones » Actualización de Datos » bol | exp » ' || e4.bol || ' | ' || e4.expte;

            -- Localiza CP si viene vacío.
            if e4.cp is null or e4.cp='' then
                for w in cCp(e4.localidad,e4.provincia) loop
                    vCP:=substr(w.cp,1,90);
                end loop;
            else
                vCP:=e4.cp;
            end if;
            
            if e4.domicilio is not null
            and e4.localidad is not null
            and e4.provincia is not null
            and vCP is not null
            then
            
            dbms_output.put_line('Bol/Expte: ' || e4.bol||'/'||e4.expte ||' » ' || e4.domicilio||' '||e4.localidad||' '||e4.provincia||' '||vCP);
            
                --vRdiid:=trml.obtenerdireccion(e4.domicilio,e4.localidad,e4.provincia,vCP);
              vRdiid:=0;
               end if;
            
            vLog:=vLog || ' » ' || e4.domicilio||' '||e4.localidad||' '||e4.provincia||' '||vCP;

            update alba.escritosmultasrec e
            set e.rdiid=vRdiid
            where e.eregid=e4.eregid;
            
            update alba.escritosmultas r
            set r.rdiid=vRdiid
            where r.rdiid=0
            and r.esmid=e4.esmid;

            if vRdiid>0 then
                vTotalEscritos:=vTotalEscritos+1;
            end if;    

        end loop;

        vLog:='OK. Total de escritos con direcciones recuperadas: ' || to_char(vTotalEscritos);

           dbms_output.put_line(vLog);
           
           return vLog;
           
           commit;


----------------------------------------------------------------
-- FINALIZACION DEL PROCESO CON EXCEPCION
----------------------------------------------------------------
exception when others then

           dbms_output.put_line('KO. ' || sqlerrm);
           
           return vLog;
           
           rollback;
end;



-- Carga de Fichero de Escritos
function recuperaEscritosPerdidos return varchar2 is


      cursor cEsc is
      --TEA
      select f.eficfid, d.eregid, e.mulid, d.eregboletin bol, d.eregexpediente expte
      from --alba.personas p,
      alba.multasestados e,
      alba.escritosmultasfic f, alba.escritosmultasrec d
      where d.eficfid=f.eficfid
      and to_char(d.eregexpediente)=to_char(e.liquidacion)
      --and (p.perid=e.peridt or p.perid=e.peridc)
      --and (upper(p.periden)=upper(d.eregnifcifconductor) or upper(p.peridenext)=upper(d.eregnifcifconductor))
      and d.mulid is null
      and d.eregtipoescrito='3'
      union
      --BOLETÍN
      select f.eficfid, d.eregid, e.mulid, d.eregboletin bol, d.eregexpediente expte
      from --alba.personas p,
      alba.multasestados e,
      alba.escritosmultasfic f, alba.escritosmultasrec d
      where d.eficfid=f.eficfid
      and trim(leading '0' from upper(to_char(d.eregboletin+0)))=trim(leading '0' from upper(to_char(e.mulnumbol)))
      --and (p.perid=e.peridt or p.perid=e.peridc)
      --and trim(leading '0' from upper(p.periden))=trim(leading '0' from upper(d.eregnifcifconductor))
      and d.mulid is null
      and d.eregtipoescrito='1'
      union
      --EXPEDIENTE
      select f.eficfid, d.eregid, e.mulid, d.eregboletin bol, d.eregexpediente expte
      from alba.personas p,
      alba.multasestados e,
      alba.escritosmultasfic f, alba.escritosmultasrec d
      where d.eficfid=f.eficfid
      and to_char(d.eregexpediente)=to_char(e.expediente)
      and (p.perid=e.peridt or p.perid=e.peridc)
      and trim(leading '0' from upper(p.periden))=trim(leading '0' from upper(d.eregnifcifconductor))
      and d.mulid is null
      and d.eregtipoescrito='1'
      union
      --IDENTIFICACIÓN DE CONCUTOR
      select f.eficfid, d.eregid, e.mulid, d.eregboletin bol, d.eregexpediente expte
      from alba.escritosmultasfic f, alba.escritosmultasrec d, alba.multasestados e
      where d.eficfid=f.eficfid
      and trim(leading '0' from upper(to_char(d.eregboletin+0)))=trim(leading '0' from upper(to_char(e.mulnumbol)))
      and d.mulid is null
      and d.eregtipoescrito='2'
      union
      select f.eficfid, d.eregid, e.mulid, d.eregboletin bol, d.eregexpediente expte
      from alba.escritosmultasfic f, alba.escritosmultasrec d, alba.multasestados e
      where d.eficfid=f.eficfid
      and to_char(d.eregexpediente)=to_char(e.expediente)
      and d.mulid is null
      and d.eregtipoescrito='2';


      cursor cEsc5 is
      select
      eficfid, eregid, mulid, fescrito, 
      decode(tdmresid,323,21,361,1,321,1,322,2,tidomid) tidomid,
      tdmresid, perid, perversion, rdiid,
      esmid, boletin,
      esmcaja, esmlote, esmindice        
      from
      (
      select g.eficfid, g.eregid, g.mulid, g.fescrito, g.tidomid,
      decode(tidomid,21,41,1,22,41,63,2,23,3,24,101,181,62,301,21323,323,1361,361,1321,321,2322,322,null,null) tdmresid, g.perid, g.perversion, g.rdiid,
      g.esmid, g.boletin,
      g.esmcaja, g.esmlote, g.esmindice
      from
      (
          select f.*,
          case when tipoEscrito=3 --REA
          then 101 --REA            
          else
              case when mulid is null -- NO SE HA GRABADO LA DENUNCIA
              then --No existe mulid, por tanto la denuncia no se ha grabado
                          case when tipoEscrito=2 --Identificación de conductor
                          then 21 --ALEGACIÓN
                          else 1 -- IDENTIFICACIÓN DE CONDUCTOR
                          end
              else
                          case when tipoEscrito=2 --Identificación de conductor
                          then 
                                    --COMPROBAR: 20 días naturades como máximo desde la fecha de notificación de la denuncia
                                    case when to_date(fescrito,'dd/mm/rrrr')<=to_date(fnotificacionDenuncia,'dd/mm/rrrr')+20
                                    then 21 -- Identificación de conductor
                                    else 21323 -- Identificación Extemporarea
                                    end
                          else -- tipoEscrito=1
                                    case when to_date(fescrito,'dd/mm/rrrr')>to_date(flibroSancionFirme,'dd/mm/rrrr')
                                    and flibroSancionFirme is not null
                                    -- Si la fecha del escrito es posterior al libro de sanción firme 
                                    then
                                             case when notpa>0 --hay providencia de apremio
                                             and to_date(fescrito,'dd/mm/rrrr')>=to_date(femisionnotpa,'dd/mm/rrrr')
                                             and femisionnotpa is not null
                                             then 41 --RPA
                                             else 3 -- Recurso ER                                       
                                             end
                                    else
                                            case when to_date(fescrito,'dd/mm/rrrr')>to_date(flibrec,'dd/mm/rrrr')
                                            and flibrec is not null
                                            then  3 -- Recurso ER
                                            else -- decide si es alegación o recurso
                                                
                                                case when csl>0
                                                then 1361 --Alegación No Presentada
                                                else
                                                      case when to_date(fescrito,'dd/mm/rrrr')>=to_date(flibroSancion,'dd/mm/rrrr')
                                                      and flibroSancion is not null
                                                      then 
                                                            case when to_date(fescrito,'dd/mm/rrrr')>add_months(to_date(fnotificacionSancion,'dd/mm/rrrr'),1)
                                                            and flibroSancion is not null
                                                            then 2322 -- Recurso extemporaneo                                                        
                                                            else 2 -- Recurso
                                                            end 
                                                      else  -- Alegación
                                                      --COMPROBAR: 20 días naturades como máximo desde la fecha de notificación de la denuncia
                                                              case when to_date(fescrito,'dd/mm/rrrr')<=to_date(fnotificacionDenuncia,'dd/mm/rrrr')+20
                                                              then 1 -- Alegción
                                                              else 1321 -- Alegación Extemporánea
                                                              end                                                      
                                                      end
                                                end
                                            end
                                    end
                            
                          end
                end
          end tidomid
          from
          (
              select f.eficfid, d.eregid, e.mulid, d.eregtipoescrito tipoEscrito,
              d.eregfechadocumento fescrito,
              nvl(e.flibaleg,e.flibsan) flibroSancion,
              e.flibsanfir flibroSancionFirme,
              e.flibsan,e.flibaleg, e.flibrec, e.flibsanfir,
              d.perid, d.perversion, d.rdiid, d.esmid, e.mulnumbol boletin,
              d.eregcaja esmcaja, d.ereglote esmlote, d.eregindice esmindice,
              e.fnotinfq femisionnotpa, e.notpa, f.eficffproceso fgrabacionescrito, e.aleg,
              nvl(nvl(nvl(nvl(e.fnotden,e.flibbopden),e.fnotreq),e.flibbopreq),sysdate) fnotificacionDenuncia,
              nvl(nvl(e.fnotsan,e.flibbopsan),sysdate+365) fnotificacionSancion,            
              d.eregtipoescrito, e.csl
              from alba.escritosmultas ee,
              alba.multas m, alba.escritosmultasfic f, alba.escritosmultasrec d, table(trml.emultas(d.mulid)) e
              where d.eficfid=f.eficfid
              and d.mulid=m.mulid
              and d.mulid=e.mulid
              and d.esmid=ee.esmid
              and d.mulid is not null
              and d.mulid>0
              and ee.tidomid=62              
          ) f
      ) g
      );


        vPerid number:=0;
        vPerversion number:=0;
        vRdiid number:=0;
        vEsmid number:=0;

        vLog varchar2(1000):='';
        vCP varchar2(100):='';
        vTotalEscritos number:=0;



begin

        --completar mulid's y otras referencias
        vLog:='1. Obtención de Referencias » Selección de Datos';
        for e1 in cEsc loop
            vLog:='1. Obtención de Referencias » Actualización de Datos » bol | exp » ' || e1.bol || ' | ' || e1.expte;
            update alba.escritosmultasrec e
            set e.mulid=e1.mulid,
            e.eregboletin=e1.bol
            where e.eregid=e1.eregid;
            commit;
            
            vTotalEscritos:=vTotalEscritos+1;
            
        end loop;

        vLog:='OK. Total de escritos con referencias recuperadas: ' || to_char(vTotalEscritos);
        
        dbms_output.put_line(vLog);

        --insercción en la tabla principal de escritos
        vLog:='2. Update de Escritos » Selección de Datos';
        for e5 in cEsc5 loop

            vLog:='2. Update de Escritos » Selección de Datos » bol» ' || e5.boletin;
                
                vLog:=vLog || ' » ' || e5.boletin||' '||e5.esmcaja||' '||e5.esmlote||' '||e5.esmindice;
           
                update alba.escritosmultas a
                set a.esmnumbol=e5.boletin,
                a.mulid=e5.mulid,
                a.tidomid=e5.tidomid,
                a.tdmresid=e5.tdmresid,
                a.esmfhoramod=sysdate,
                a.esmfestado=sysdate,
                a.esmobservaciones='RAEP. '||a.esmobservaciones,
                a.usuidmod=0
                where a.esmid=e5.esmid;
                commit;
                 
                vTotalEscritos:=vTotalEscritos+1;         
            
        end loop;


        vLog:='OK. Total de escritos actualizados: ' || to_char(vTotalEscritos);

           dbms_output.put_line(vLog);
           
           return vLog;
           
           commit;


----------------------------------------------------------------
-- FINALIZACION DEL PROCESO CON EXCEPCION
----------------------------------------------------------------
exception when others then

      vLog:=vLog || ' ? KO. ' || sqlerrm;
           dbms_output.put_line('KO. ' || sqlerrm);
           
           return vLog;
           
           rollback;
end;





-- Obtener Dirección
function obtenerDireccion(vDireccion varchar2, vLocalidad varchar2, vProvincia varchar2, vCP varchar2) return number is

        -- Código Postal
        cursor cCP(cvCP alba.m_codpostal.codposcodigo%type, cvMunid alba.m_codpostal.munid%type) is
        select * from (select c.codid, c.munid from alba.m_codpostal c
        where c.codposcodigo=cvCP and c.munid=cvMunid) where rownum<2;

        cursor cCP2 (localidad varchar2, provincia varchar2, cp varchar2) is
        select * from
        (select c.codid, l.munid, p.mprid, c.codposcodigo cp,
        l.mundesc localidad, p.mprdesc provincia
        from alba.m_codpostal c, alba.municipios l, alba.provincia p
        where c.munid=l.munid
        and l.mprid=p.mprid
        and c.codposcodigo=upper(cp)
        and l.mundesc=upper(localidad)
        and p.mprdesc=upper(provincia)
        order by c.codid desc)
        where rownum<2;

        cursor cCP3 (localidad varchar2, provincia varchar2) is
        select * from
        (select l.munid, p.mprid,
        l.mundesc localidad, p.mprdesc provincia
        from alba.municipios l, alba.provincia p
        where l.mprid=p.mprid
        and l.mundesc=upper(localidad)
        and p.mprdesc=upper(provincia)
        order by l.munid desc)
        where rownum<2;

        -- Provincia
        cursor cPro(cvProvincia alba.provincia.mprdesc%type) is
        select * from (select p.mprid from alba.provincia p where p.mprdesc=upper(cvProvincia)) where rownum<2;

        -- Municipio
        cursor cMun(cvLocalidad alba.municipios.mundesc%type) is
        select * from (select m.munid from alba.municipios m where m.mundesc=upper(cvLocalidad)) where rownum<2;



        -- Variables
        vCodid alba.m_codpostal.codid%type;
        vAccid alba.accesos.accid%type;
        vMprid alba.provincia.mprid%type;
        vMunid alba.municipios.munid%type;
        vViaid alba.vias.viaid%type;
        vRdiid alba.direccion.rdiid%type;
        --
        vDepuracion varchar2(200):='';

        begin

                    vCodid:=0;
                    vAccid:=0;
                    vMprid:=0;
                    vMunid:=0;

                    vDepuracion:=vDepuracion||'INICIO.45';

                    
                    for c in cCP2(vLocalidad, vProvincia, vCP) loop
                         vCodid:=c.codid;
                         vMunid:=c.munid;
                         vMprid:=c.mprid;
                    end loop;
                    
                    for c in cCP3(vLocalidad, vProvincia) loop
                         vMunid:=c.munid;
                         vMprid:=c.mprid;
                    end loop;
                
                    
                    -- Localiza Provincia
                    if vMprid=0 then

                        for p in cPro(vProvincia) loop
                          vMprid:=p.mprid;
                        end loop;

                        /*
                        if vMprid=0 then
                        
                            vDepuracion:=vDepuracion||'.46 » Provincia: '||vProvincia;

                            -- Inserta Provincia
                            select alba.provincia_sq.NEXTVAL into vMprid from dual;

                            insert into alba.provincia a
                            (a.mprid, a.mccid, a.mpaid, a.mprcodigo, a.mprdesc, a.mprfhoramod,
                            a.usuidmod, a.mprmotivo, a.mprfvigencia, a.mprmostrable)
                            values
                            (vMprid, null, 9, to_char(vMprid), substr(upper(vProvincia),1,50), sysdate,
                            0, 'Se ha creado', null, 'V');


                        end if;
                        */

                    end if;
                    
                    -- Localiza Localidad
                    if vMunid=0 then
                    
                        for l in cMun(vLocalidad) loop
                           vMunid:=l.munid;
                        end loop;
                    
                        /*
                        if vMunid=0 then
                        
                            vDepuracion:=vDepuracion||'.46 » Localidad: '||vLocalidad;                        

                            -- Inserta Localidad
                            select alba.municipios_sq.NEXTVAL into vMunid from dual;
                
                            insert into alba.municipios a
                            (a.munid, a.mprid, a.muncodigo, a.mundigitodecontrol, a.muncun,
                            a.mundesc, a.mundes50, a.mundescor, a.munfhoramod, a.usuidmod,
                            a.munmotivo, a.munfvigencia, a.munmarca600, a.muncodcatastro)
                            values
                            (vMunid, vMprid, '000', '0', null,
                            substr(upper(vLocalidad),1,70), substr(upper(vLocalidad),1,50), substr(upper(vLocalidad),1,25), sysdate, 0,
                            'Se ha creado', null, null, null);    
                

                        end if;
                        */
                    
                    end if;
                    
                    -- Localiza CP
                    for c in cCP(vCP,vMunid) loop
                        vCodid:=c.codid;

                    end loop;                

                    -- Alta CP
                    /*if vCodid=0 then

                            select alba.m_codpostal_sq.NEXTVAL into vCodid from dual;

                            insert into alba.m_codpostal m
                            (m.codid, m.munid, m.codposcodigo, m.codposdesc, m.codfhoramod,
                            m.usuidmod, m.codmotivo, m.codfvigencia)
                            values
                            (vCodid, vMunid, vCP,null,sysdate,
                            0,'Se ha creado',null);
                            vDepuracion:=vDepuracion||'.47';


                    end if;*/
                    
                    -- Alta Vía
                    /*select alba.vias_sq.NEXTVAL into vViaid from dual;

                    vDepuracion:=vDepuracion||'.46 » Via: '||vDireccion;
                    insert into alba.vias a
                    (a.viaid, a.tviid, a.munid, a.viacodigo, a.viadesc, a.viadescorta,
                    a.viafhoramod, a.usuidmod, a.viamotivo, a.viafvigencia)
                    values
                    (vViaid,416,vMunid,'00000',substr(vDireccion,1,200),substr(vDireccion,1,200),
                    sysdate,0,'Se ha creado',null);*/
                                
                    
                    -- Alta Accesos
                    /*
                    select alba.accesos_sq.NEXTVAL into vAccid from dual;

                    insert into alba.accesos a
                    (a.accid, a.zonid, a.viaid, a.disid, a.rcaid, a.accxorigen,
                    a.accnum1, a.accinum1, a.accnum2, a.accinum2, a.acckm1,
                    a.accikm1, a.accdirnoest, a.accfhoramod, a.usuidmod, a.accmotivo,
                    a.accfvigencia, a.codid)
                    values
                    (vAccid, null, vViaid, null, null, 6189,
                    null, null, null, null, null,
                     null, null, sysdate, 0, 'Se ha creado',
                     null, vCodid);*/


                    /*select alba.direccion_sq.NEXTVAL into vRdiid from dual;                        

                    insert into alba.direccion a
                    (a.rdiid, a.accid, a.rdicodigo, a.rdiref3, a.rdirefcon1,
                    a.rdirefcon2, a.rdixpor, a.rdixesc, a.rdixpiso, a.rdixpuerta,
                    a.rdiipuertadup, a.rdidirnoest, a.rdifhoramod, a.usuidmod,
                    a.rdimotivo, a.rdifvigencia)
                    values
                    (vRdiid, vAccid, '-', null, null,
                    null, null, null, null, null,
                    null, null, sysdate, 0, 'Se ha creado', null);*/
                    
                    vDepuracion:=vDepuracion||'.50';
                    
                    if vRdiid is null 
                    or vRdiid=0
                    or vCodid=0
                    or vAccid=0
                    or vMprid=0
                    or vMunid=0
                    then
                     vRdiid:=0;
                    end if;

                    return vRdiid;

                    DBMS_OUTPUT.PUT_LINE('OK. Rdiid=' || to_char(vRdiid) );
                    COMMIT;

        ----------------------------------------------------------------
        -- FINALIZACION DEL PROCESO CON EXCEPCION
        ----------------------------------------------------------------
        EXCEPTION WHEN OTHERS THEN
                       DBMS_OUTPUT.PUT_LINE('KO. ' || sqlerrm || chr(13) || vDepuracion);
                       
                       return 0;
                     
                       ROLLBACK;
end;


-- Obtener persona
function obtenerPersona(vNif varchar2,vNombre varchar2,vApe1 varchar2,vApe2 varchar2,
vPerversion in out number, vTipoPersona alba.tipospersona.tpeid%type) return number is

        cursor cPer(cvNif varchar2, cvNombre varchar2, cvApe1 varchar2, cvApe2 varchar2,cvTipoPersona number) is
        select * from
        (select p.perid, p.perversion
        from alba.personas p
        where p.periden=upper(cvNif)
        and p.pernombre=upper(cvNombre)
        and p.perapell1=upper(cvApe1)
        and p.perapell2=upper(cvApe2)
        and p.tpeid=cvTipoPersona
        union
        select p.perid, p.perversion
        from alba.personas p
        where p.periden=upper(cvNif)
        and p.perrazon=upper(cvNombre)
        and p.tpeid=cvTipoPersona        
        union
        select p.perid, p.perversion
        from alba.personas p
        where p.peridenext=upper(cvNif)
        and p.pernombre=upper(cvNombre)
        and p.perapell1=upper(cvApe1)
        and p.perapell2=upper(cvApe2)
        and p.tpeid=cvTipoPersona)        
        order by perid desc;

        -- Variables
        vPerid alba.personas.perid%type;
        vDepuracion varchar2(200):='';
        vRazonSocial varchar2(200):='';

begin

        vPerid:=0;
        vPerversion:=0;
        
        if vNif is not null and vNombre is not null then
        
                    vDepuracion:=vDepuracion||'.40';
                    for p in cPer(vNif,vNombre,vApe1,vApe2,vTipoPersona) loop
                            vPerid:=p.perid;
                            vPerversion:=p.perversion;
                    end loop;

                    vDepuracion:=vDepuracion||'.41';
                    if vPerid=0 then

                            
                            select alba.personas_sq.NEXTVAL,1 into vPerid,vPerversion from dual;

                            if vTipoPersona in (1,3) then
                                    vRazonSocial:=substr(vNombre || ' ' || vApe1 || ' ' || vApe2,1,200);
                            end if;

                            vDepuracion:=vDepuracion||'.42';
                            
                            
                            insert into alba.personas a
                            (a.perid, a.perversion, a.tpeid, a.fiaid, a.perunico, a.pernombre,
                            a.perpart1, a.perapell1, a.perpart2, a.perapell2, a.perrazon,
                            a.peranagrama, a.pernomval, a.periden, a.peridenext,
                            a.perxtipoiden, a.peridenval, a.perxorigen, a.perfhoramod,
                            a.usuidmod, a.permotivo, a.perfvigencia, a.permigraperiden,
                            a.permigraperapelnomcorto, a.tcoid, a.perfallido, a.perorigendep, a.percategoria)
                            values
                            (vPerid, vPerversion, vTipoPersona, 5, 1, substr(vNombre,1,20),
                            null, substr(vApe1,1,25), null, substr(vApe2,1,25), vRazonSocial,
                            null, null, vNif, null,
                            144, 'S', 4245, sysdate,
                            0,'Se ha creado',null,null,
                            null,null,'N','P','P');


                            vDepuracion:=vDepuracion||'.43';

                    end if;
        end if;

        vDepuracion:=vDepuracion||'.44';



      return vPerid;

        DBMS_OUTPUT.PUT_LINE('OK');
        COMMIT;

----------------------------------------------------------------
-- FINALIZACION DEL PROCESO CON EXCEPCION
----------------------------------------------------------------
EXCEPTION WHEN OTHERS THEN
           DBMS_OUTPUT.PUT_LINE('KO. ' || sqlerrm || chr(13) || vDepuracion);
           
           return 0;
         
           ROLLBACK;
end;


-----



-- Actualización de expedientes de botellón grabados como expedientes de otro tipo
-- Selección por código de infracción de hecho denunciado
procedure regeneraExpedientesBotellon is


cursor cMul is
select m.mulid
from alba.multas m
where m.infid in
(
select i.infid
from alba.infracciones i
where i.infcodigo in ('901','902','903','904','905','906','907','908')
)
and m.mulxboletin<>423;

cursor cMul2 is
select m.mulid
from alba.multas m
where m.infid in
(
select i.infid
from alba.infracciones i
where i.infcodigo in ('901','902','903','904','905','906','907','908')
)
and (m.expidveh is not null
or m.seoidveh is not null
or m.vemid is not null
or m.mulmarca is not null
or m.mulmatri is not null);


begin


    for m in cMul loop
    
        update alba.multas m1
        set m1.mulxboletin=423
        where m1.mulid=m.mulid;
    
    end loop;
    
    
    for mm in cMul2 loop
    
        update alba.multas m2
        set m2.expidveh=null,
        m2.seoidveh=null,
        m2.vemid=null,
        m2.mulmarca=null,
        m2.mulmatri=null
        where m2.mulid=mm.mulid;
    
    end loop;    

    dbms_output.put_line('OK');

    commit;


exception
    when others then
       dbms_output.put_line('KO. ' || SQLERRM);
       rollback;
end;



-- Carga de Fichero de Escritos
function iboletin(vBoletin number) return varchar2 is

cursor cPdf is
select /* +RULE */
'file://agenciarec/fotosradar$/MULTA-CAR/'||
replace(fichero3,'\','/') img
from alba.multacar_blob b
where b.boletin=vboletin
and upper(substr(fichero3,-3))='PDF';

vLog varchar2(1000):='';
vexisteimagen number:=0;
vPdf varchar2(500):='';

begin

      select 
      /* +RULE */
      decode(count(b.idmultacar_blob),0,0,1) into vExisteImagen
      from alba.multacar_blob b
      where b.boletin=vboletin
      and b.tipoboletin in ('TR','BB')
      and b.boletin_pdf is not null;
      
      for p in cpdf loop
        vPdf:=p.img;
      end loop;
      
      if vExisteImagen=0 then
      
        vlog:='<a href="https://www.datasur.com.es/AGMUR6/ASP/resultados_multas.asp?bd=MULTAS' || chr(38) || 'clave=albagd' || chr(38) || 'tipo_doc=5.1' || chr(38) || 'boletin=' || vboletin || '"  target="_blank" >' || vboletin || '</a>';
        vLog:=vLog||'<a href="https://www.datasur.com.es/AGMUR6/ASP/resultados_multas.asp?bd=MULTAS' || chr(38) || 'clave=albagd' || chr(38) || 'boletin=' || vboletin || '"  target="_blank" >' || ' (Búsqueda completa por boletín)' || '</a>';
      
      else
      
        vlog:='<a href="' || vPdf || '"  target="_blank" >' || vboletin || '</a>';
        vLog:=vLog||'<a href="https://www.datasur.com.es/AGMUR6/ASP/resultados_multas.asp?bd=MULTAS' || chr(38) || 'clave=albagd' || chr(38) || 'boletin=' || vboletin || '"  target="_blank" >' || ' (Búsqueda completa por boletín)' || '</a>';
      
      end if;
           
      return vLog;
           
exception when others then

      vlog:='KO. ' || sqlerrm;
           dbms_output.put_line(vLog);
           
           return vLog;
 
end;

function esNumero (p_string_value varchar2) return number is

test_value number:=0;

begin

    if LENGTH(TRIM(TRANSLATE(p_string_value,'0123456789',' '))) is null then
        test_value:=1;
    else
        test_value:=0;
    end if;
    
    return (test_value);
    
exception when others then

    return 0;    

end;

--------------------------------------------------------------------------------
procedure recuperaPropuesta(vCrmnumeroIn number) is

cursor cnot(vcrmid number) is
select distinct n.ntfid, p.pntid, r.remid, c.crmcodigo
from 
alba.cabremesas c,
alba.remesas r,
alba.notificaciones n,
alba.propuestasnotificacion p,
alba.escritosmultas e
where c.crmid=r.crmid
and r.ntfid=n.ntfid
and n.ntfcrmid=c.crmid
and n.pntid=p.pntid
and c.crmid=vcrmid
and e.tidomid=181
and p.pntorigenid=e.mulid
;

cursor cnot_no_emitidas(vcrmcodigo varchar2) is
select distinct n.ntfid, r.remid
from 
alba.cabremesas c,
alba.remesas r,
alba.notificaciones n,
alba.propuestasnotificacion p,
alba.escritosmultas e
where c.crmid=r.crmid
and r.ntfid=n.ntfid
and n.ntfcrmid is null
and n.pntid=p.pntid
and c.crmcodigo=vcrmcodigo
and e.tidomid=181
and p.pntorigenid=e.mulid
;


cursor cremesas(vCrmnumero number) is
select c.crmid, c.crmcodigo
from alba.cabremesas c
where c.crmnumero=vCrmnumero;


vactual date;
vcrmid number:=0;
vCrmcodigo varchar2(20):='';

begin

    vactual:=sysdate;
    
    for c in cRemesas(vCrmnumeroIn) loop

          for n in cNot(c.crmid) loop
          
              vCrmcodigo:=n.crmcodigo;
          
              delete alba.remesas r where r.remid=n.remid;
              
              delete alba.notificaciones n2 where n2.ntfid=n.ntfid;
              
              update alba.propuestasnotificacion p
              set p.atoid=decode(p.atoid,1257,3,917,5,p.atoid),
              p.pntfecha=trunc(sysdate),
              p.pntlimite=trunc(sysdate)+20,
              p.pntfhoramod=vactual,
              p.pntestado='P',
              p.usuidmod=0,
              p.pntmotivo='Pendiente de generar remesa',
              p.pntfvigencia=trunc(sysdate)+20
              where p.pntid=n.pntid;
          
          end loop;
          
          for n in cNot_No_Emitidas(vCrmcodigo) loop
          
              delete alba.remesas r where r.remid=n.remid;
              
              delete alba.notificaciones n2 where n2.ntfid=n.ntfid;
              
          end loop;
          
    end loop;
    

    commit;

exception
    when others then
        dbms_output.put_line(SQLERRM);        
        rollback;
end;



function imgToHTML(vBoletin number) return varchar2 is

cursor cimg(vbol number) is
select * from (
select replace(fichero1,'\','/') img, 
upper(substr(fichero1,-3)) extension, 
idmultacar_blob id
from alba.multacar_blob b
where b.boletin=vBol
and fichero1 is not null
union
select replace(fichero2,'\','/') img, 
upper(substr(fichero2,-3)) extension, 
idmultacar_blob id
from alba.multacar_blob b
where b.boletin=vBol
and fichero2 is not null
union
select replace(fichero3,'\','/') img, 
upper(substr(fichero3,-3)) extension, 
idmultacar_blob id
from alba.multacar_blob b
where b.boletin=vBol
and fichero3 is not null
union
select replace(fichero4,'\','/') img, 
upper(substr(fichero4,-3)) extension, 
idmultacar_blob id
from alba.multacar_blob b
where b.boletin=vBol
and fichero4 is not null
union
select ruta img,
'RDR' extension,
(substr(fichero,1,length(fichero)-4))+0 id
from depuracion.indexmul
where upper(substr(fichero,-3))='JPG'
and ltrim(substr(fichero,1,length(fichero)-4),'0')=to_char(vBol)
union
select ruta img,
'RDR' extension,
(substr(fichero,1,length(fichero)-8))+0 id
from depuracion.indexmul
where upper(substr(fichero,-3)) in ('JPG')
and ltrim(substr(fichero,1,length(fichero)-8),'0')=to_char(vBol)
union
select ruta img,
'RDR' extension,
(substr(fichero,1,length(fichero)-4))+0 id
from depuracion.indexmul
where upper(substr(fichero,-3))='JPG'
and ltrim(substr(fichero,1,length(fichero)-4),'0')=to_char(vBol)
union
select ruta img,
'MP4' extension,
(substr(fichero,1,length(fichero)-8))+0 id
from depuracion.indexmul
where upper(substr(fichero,-3)) in ('MP4')
and ltrim(substr(fichero,1,length(fichero)-8),'0')=to_char(vBol)
)
order by extension asc
;

vraiz varchar2(400):='file://agenciarec/fotosradar$/MULTA-CAR/';
vSalida varchar2(2000):='';
vHtml varchar2(2000):='';

begin

  vSalida:='';
  
  for i in cimg(vBoletin) loop
  
    vHtml:='';
    
    if i.extension in ('JPG','PNG') then
        vhtml:='<A HREF="'|| vraiz || i.img ||'" target="_blank">'|| vraiz || i.img ||'</A>' ||
               '<br><img id="' || i.id || '" ' ||
               'src="' || vraiz || i.img || '" ' || 
               'width="640"/><br><br>';
    elsif i.extension in ('WMV') then
        vhtml:='<A HREF="'|| vraiz || i.img ||'" target="_blank">'|| vraiz || i.img ||'</A>' ||
               '<br><embed type="application/x-mplayer2" ' ||
               'src="'   || vraiz || i.img || '" ' ||
               'width="640" height="480" autostart="false" ' ||
               'showcontrols="true" ' ||
               'pluginspage="http://www.microsoft.com/Windows/MediaPlayer/"></embed><br><br>';  
    elsif i.extension in ('MP4') then
        vhtml:='<A HREF="' || i.img ||'" target="_blank">' || i.img ||'</A>' ||
               '<br><embed type="application/x-mplayer2" ' ||
               'src="' || i.img || '" ' ||
               'width="640" height="480" autostart="false" ' ||
               'showcontrols="true" ' ||
               'pluginspage="http://www.microsoft.com/Windows/MediaPlayer/"></embed><br><br>';                 
    elsif i.extension in ('RDR') then
        vhtml:='<A HREF="#" onclick="window.open(' || chr(39) || i.img || chr(39) ||');return false;" onkeypress="window.open(' || chr(39) || i.img || chr(39) ||'); return false;">'|| i.img ||'</A>' ||
               '<br><img id="' || i.id || '" ' ||
               'src="' || i.img || '" ' || 
               'width="640"/><br><br>';               
    end if;
    
    vSalida := vSalida || vHtml;
    
  end loop;
    
  return vSalida;    
  
exception when others then

    return '';    

end;




-- Carga de Fichero de Escritos
procedure resolverEscritos (vOut in out varchar2) is

cursor cPendientes is
select e.esmid, e.mulid from alba.escritosmultas e
where e.tidomid=41 and e.tdmresid=63;

cursor cDesestimados(vMulid number) is
select r.resid
from alba.escritosmultas e, alba.resol_alegaciones r
where e.tidomid=41 and e.tdmresid=62
and r.esmid=e.esmid
and r.resid in (2541)
and e.mulid=vMulid
and rownum=1
;

vTotal number:=0;

begin
    
    for p in cPendientes loop
        for d in cDesestimados(p.mulid) loop
        
          --Actualiza a escrito desestimado
          update alba.escritosmultas e
          set e.tidomid=41, e.tdmresid=61, 
          e.ESMFHORAMOD=trunc(sysdate), e.USUIDMOD=0,
          e.ESMFESTADO=trunc(sysdate), e.ESMMOTIVO='RA. Estimación previa',
          e.ESMOBSERVACIONES='RA. Estimación previa'
          where e.esmid=p.esmid
          and e.tidomid=41 and e.tdmresid=63;
          
          -- Inserta resoluciones
          insert into alba.resol_alegaciones
          (RSAID,ALGID,RESID,RSAFHORAMOD,USUIDMOD,RSAMOTIVO,RSAFVIGENCIA,RSAMOTIVORESOL,ESMID,RSARESOLUCION)
          select 
          alba.resol_alegaciones_sq.nextval rsaid,
          0 algid,
          l.resid resid,
          trunc(sysdate) rsafhoramod,
          0 usuidmod,
          'RA. Estimación previa' rsamotivo,
          null rsafvigencia,
          l.resdesccompleta rsamotivoresol,
          p.esmid esmid,
          l.resresolucion rsaresolucion
          from alba.resoluciones l
          where l.tidomid=41
          and l.tdmresid=61
          and l.resid=6581;
        
          -- Actualiza contador
          vTotal:=vTotal+1;
        end loop;
    end loop;
  
  vOut:='OK. '||vTotal;
  commit;
  
exception when others then

  vOut:='KO. '||sqlerrm;
  rollback;

end;



procedure clonar_resol(oEsmid in number, dEsmid in number) is 

cursor cOrigen is
select *
from alba.escritosmultas e
where e.tidomid in (41,101)
and e.tdmresid in (61,62,261,281)
and e.esmid=oEsmid
;

cursor cDestino is
select *
from alba.escritosmultas e
where e.tidomid in (41,101)
--and e.tdmresid in (63,282)
and e.esmid=dEsmid
;


begin

    for o in cOrigen loop
      for d in cDestino loop
        if o.tidomid=d.tidomid 
        and o.esmcaja=d.esmcaja
        and o.esmlote=d.esmlote
        and o.esmindice=d.esmindice
        then
        
          --Actualiza escrito
          update alba.escritosmultas e
          set e.tdmresid=o.tdmresid, 
          e.ESMFHORAMOD=trunc(sysdate), e.USUIDMOD=0,
          e.ESMFESTADO=trunc(sysdate), e.ESMMOTIVO='Clonación Resolución',
          e.ESMOBSERVACIONES='Clonación Resolución'
          where e.esmid=d.esmid;
        
          -- Inserta resoluciones
          insert into alba.resol_alegaciones
          (RSAID,ALGID,RESID,RSAFHORAMOD,USUIDMOD,RSAMOTIVO,RSAFVIGENCIA,RSAMOTIVORESOL,ESMID,RSARESOLUCION)
          select 
          alba.resol_alegaciones_sq.nextval rsaid,
          l.algid,
          l.resid,
          trunc(sysdate) rsafhoramod,
          0 usuidmod,
          'Se ha clonado' rsamotivo,
          l.rsafvigencia,
          l.rsamotivoresol,
          d.esmid esmid,
          l.rsaresolucion
          FROM alba.resol_alegaciones l, alba.resoluciones r
          where l.esmid=o.esmid
          and l.resid=r.resid
          and r.orden is null;
          
        end if;
      end loop;
    end loop;
    
    commit;

exception when others then

  rollback;

end;


-- Escritos Oficina Virtual
function escritosOV(vNif in varchar2:=null, vFechadoc in varchar2:=null) 
return eov_lista pipelined is

cursor cEsc1 is
select e.esmid, e.mulid, e.esmnumbol boletin, m.expediente, 
trunc(e.esmfescrito) fechadoc,
t.TIDOMDESC tipodoc, r.tdmnombre estadodoc
from
alba.tipos_doc_multas_res r,
alba.tipos_doc_multas t,
alba.escritosmultas e, 
alba.multasestados m,
alba.personas p
where (m.peridtt=p.perid or m.peridt=p.perid or m.peridc=p.perid)
and m.mulnumbol=e.esmnumbol
and e.tidomid=t.tidomid
and e.tdmresid=r.tdmresid
and e.tidomid in (1,2,3,21,41,101)
and exists (select 1 from alba.det_libro_resol d where d.esmid=e.esmid)
and p.periden=vNif;

cursor cEsc2 is
select e.esmid, e.mulid, e.esmnumbol boletin, m.expediente, 
trunc(e.esmfescrito) fechadoc,
t.TIDOMDESC tipodoc, r.tdmnombre estadodoc
from
alba.tipos_doc_multas_res r,
alba.tipos_doc_multas t,
alba.escritosmultas e, 
alba.multasestados m,
alba.personas p
where (m.peridtt=p.perid or m.peridt=p.perid or m.peridc=p.perid)
and m.mulnumbol=e.esmnumbol
and e.tidomid=t.tidomid
and e.tdmresid=r.tdmresid
and e.tidomid in (1,2,3,21,41,101)
and exists (select 1 from alba.det_libro_resol d where d.esmid=e.esmid)
and p.periden=vNif
and trunc(e.esmfescrito)=to_date(vFechadoc,'dd/mm/YYYY')
;


begin

  iniciarEov; 

  if vNif is not null then
    if vFechadoc is null then
      for e in cEsc1 loop
        vEov.esmid:=e.esmid;
        vEov.mulid:=e.mulid;
        vEov.Boletin:=e.boletin;       
        vEov.Expediente:=e.expediente;
        vEov.fechadoc:=e.fechadoc; 
        vEov.tipodoc:=e.tipodoc;
        vEov.estadodoc:=e.estadodoc;    
        pipe row(vEov);        
      end loop;
    else
      for e in cEsc2 loop
        vEov.esmid:=e.esmid;
        vEov.mulid:=e.mulid;
        vEov.Boletin:=e.boletin;       
        vEov.Expediente:=e.expediente;
        vEov.fechadoc:=e.fechadoc; 
        vEov.tipodoc:=e.tipodoc;
        vEov.estadodoc:=e.estadodoc;    
        pipe row(vEov);        
      end loop;
    end if;
  end if;
  return;    
  
exception when others then

    rollback;    

end;


function parametro(esmid number, resid number, par varchar2) return varchar2 is

cursor cExpedientes(vEsmid number) is
select 
'EXPTE. ' || ex.expcod || ' LIQ. ' || l.LIQNUMERORECLIQUIDACION liq
from 
alba.liquidaciones l,
alba.expingre ex,
alba.multas m,
alba.escritosmultas e
where e.mulid=m.mulid
and m.EXPIDEXP=ex.expid
and ex.expid=l.EXPID
and exists
(select 1 from alba.escritosmultas e2 
where e2.esmid=vEsmid 
and e.esmcaja=e2.esmcaja
and e.esmlote=e2.esmlote
and e.esmindice=e2.esmindice)
order by l.LIQNUMERORECLIQUIDACION asc
;

cursor cEstimadas(vEsmid number) is
select 
'EXPTE. ' || ex.expcod || ' LIQ. ' || l.LIQNUMERORECLIQUIDACION liq
from 
alba.liquidaciones l,
alba.expingre ex,
alba.multas m,
alba.escritosmultas e
where e.mulid=m.mulid
and m.EXPIDEXP=ex.expid
and ex.expid=l.EXPID
and e.tdmresid=261
and exists
(select 1 from alba.escritosmultas e2 
where e2.esmid=vEsmid 
and e.esmcaja=e2.esmcaja
and e.esmlote=e2.esmlote
and e.esmindice=e2.esmindice)
order by l.LIQNUMERORECLIQUIDACION asc
;

cursor cDesestimadas(vEsmid number) is
select 
'EXPTE. ' || ex.expcod || ' LIQ. ' || l.LIQNUMERORECLIQUIDACION liq
from 
alba.liquidaciones l,
alba.expingre ex,
alba.multas m,
alba.escritosmultas e
where e.mulid=m.mulid
and m.EXPIDEXP=ex.expid
and ex.expid=l.EXPID
and e.tdmresid=281
and exists
(select 1 from alba.escritosmultas e2 
where e2.esmid=vEsmid 
and e.esmcaja=e2.esmcaja
and e.esmlote=e2.esmlote
and e.esmindice=e2.esmindice)
order by l.LIQNUMERORECLIQUIDACION asc
;


cursor cRecibos(vEsmid number, vResid number) is
select 
'EXPTE. ' || ex.expcod || ' LIQ. ' || l.LIQNUMERORECLIQUIDACION liq
from 
alba.liquidaciones l,
alba.expingre ex,
alba.multas m,
alba.escritosmultas e
where e.mulid=m.mulid
and m.EXPIDEXP=ex.expid
and ex.expid=l.EXPID
and exists
(select 1
from alba.resol_alegaciones r
where r.esmid=e.esmid
and r.resid=vResid)
and exists
(select 1 from alba.escritosmultas e2 
where e2.esmid=vEsmid 
and e.esmcaja=e2.esmcaja
and e.esmlote=e2.esmlote
and e.esmindice=e2.esmindice)
order by l.LIQNUMERORECLIQUIDACION asc
;


cursor cRecibo(vEsmid number) is
select 
'EXPTE. ' || ex.expcod || ' LIQ. ' || l.LIQNUMERORECLIQUIDACION liq
from 
alba.liquidaciones l,
alba.expingre ex,
alba.multas m,
alba.escritosmultas e
where e.mulid=m.mulid
and m.EXPIDEXP=ex.expid
and ex.expid=l.EXPID
and e.esmid=vEsmid 
;


cursor cHoy is
select to_char(to_char(sysdate,'dd')+0) || 
        ' de ' || lower(trim(to_char(sysdate,'Month','NLS_DATE_LANGUAGE = SPANISH'))) || 
        ' de ' || to_char(sysdate,'yyyy') fecha from dual
;


cursor cExpteTEA(vEsmid number, vPar varchar2) is
select 
distinct decode(vPar,'recurrente',e.recurrente,'domicilio',e.domicilio,'pleno',e.pleno,'expediente_tea',e.expediente_tea,'asunto',e.asunto) texto
from
(
select
upper(d.nombred) recurrente,
upper(d.domiciliod) domicilio,
(SELECT
  esmivalor
FROM alba.escritosmultasinfo inf
WHERE inf.esmid=e.esmid
AND esmietiqueta='PLENO'
AND rownum      <2
AND esmiiestado =1
) pleno,
(SELECT
  esmivalor
FROM alba.escritosmultasinfo inf
WHERE inf.esmid=e.esmid
AND esmietiqueta='EXPTE. TEA'
AND rownum      <2
AND esmiiestado =1
) expediente_tea,
NVL(
(SELECT
  esmivalor
FROM alba.escritosmultasinfo inf
WHERE inf.esmid=e.esmid
AND esmietiqueta='ASUNTO'
AND rownum      <2
AND esmiiestado =1
), 'ACTUACION PROVIDENCIA DE APREMIO, DILIGENCIA DE EMBARGO DE CUENTAS BANCARIAS, A PLAZO, EFECTIVO.') asunto
from 
alba.liquidaciones l,
alba.expingre ex,
alba.multas m,
alba.escritosmultas e,
table(depuracion.trml.dmultas(m.mulid,e.esmid)) d
where e.mulid=m.mulid
and m.EXPIDEXP=ex.expid
and ex.expid=l.EXPID
and exists
(select 1 from alba.escritosmultasinfo ei
where ei.esmid=e.esmid
and ei.esmietiqueta='EXPTE. TEA')
and exists
(select 1 from alba.escritosmultas e2 
where e2.esmid=vEsmid
and e.esmcaja=e2.esmcaja
and e.esmlote=e2.esmlote
and e.esmindice=e2.esmindice)
order by l.LIQNUMERORECLIQUIDACION asc
) e
where rownum<2;


vSalida varchar2(2000):='';
i number:=0;

begin
  
  vSalida:='';
  i:=0;
  
  if par='expedientes'then
    for e in cExpedientes(esmid) loop
      if i=0 then
        vSalida:=e.liq;
      else
        vSalida:=vSalida||', '||e.liq;
      end if;
      i:=i+1;
    end loop;
  end if;
  
  if par='estimadas'then
    for e in cEstimadas(esmid) loop
      if i=0 then
        vSalida:=e.liq;
      else
        vSalida:=vSalida||', '||e.liq;
      end if;
      i:=i+1;
    end loop;
  end if;
  
  if par='desestimadas'then
    for e in cDesestimadas(esmid) loop
      if i=0 then
        vSalida:=e.liq;
      else
        vSalida:=vSalida||', '||e.liq;
      end if;
      i:=i+1;
    end loop;
  end if;  
  
  if par='recibos'then
    for e in cRecibos(esmid,resid) loop
      if i=0 then
        vSalida:=e.liq;
      else
        vSalida:=vSalida||', '||e.liq;
      end if;
      i:=i+1;
    end loop;
  end if; 
  
  if par='recibo'then
    for e in cRecibo(esmid) loop
      if i=0 then
        vSalida:=e.liq;
      else
        vSalida:=vSalida||', '||e.liq;
      end if;
      i:=i+1;
    end loop;
  end if;   
  
  if par='hoy'then
    for e in cHoy loop
        vSalida:=e.fecha;
    end loop;
  end if;
  
  if par in ('recurrente','domicilio','pleno','expediente_tea','asunto') then
    for e in cExpteTEA(esmid,par) loop
        vSalida:=e.texto;
    end loop;
  end if;  
  
  
  if vSalida is null then
    vSalida:=' ';
  end if;
  
  return vSalida;    
  
end;
--

--Liquidaciones incluidas en expte de embargo, pero no incluidas en escritos
function liqEscritos(esmid number) return liq_lista pipelined is

cursor cLiquidaciones(vEsmid number) is
--Liquidaciones expediente de embargo
(select liq2.liqid, liq2.LIQNUMERORECLIQUIDACION liquidacion,
ex2.expcod expediente, d.DECDESCR estado
from 
alba.liquidaciones liq2, alba.deudaexpediente dee2, 
alba.detexpedientesejecu dxe2, alba.cabexpedienteejecutiva epe2,
alba.expingre ex2, alba.decocobros d, alba.multas m
where epe2.epeid=dxe2.epeid
and dxe2.dxeid=dee2.dxeid
and dee2.liqid=liq2.liqid
and liq2.expid=ex2.expid
and d.decid=liq2.LIQXESTADO
and d.deccodigotabla=1
and ex2.expid=m.expidexp
and exists
(
select 1
from 
alba.liquidaciones liq, alba.deudaexpediente dee, alba.detexpedientesejecu dxe, 
alba.cabexpedienteejecutiva epe, alba.expingre ex, alba.multas m, 
alba.escritosmultas escritos 
where epe.epecodigoexp=epe2.epecodigoexp
and epe.epeejercicio=epe2.epeejercicio 
and epe.epeid=dxe.epeid
and dxe.dxeid=dee.dxeid
and dee.liqid=liq.liqid
and liq.expid=ex.expid
and m.expidexp=ex.expid
and escritos.mulid=m.mulid
and epe2.epecodigoexp=epe.epecodigoexp
and epe2.epeejercicio=epe.epeejercicio
and escritos.esmid=vEsmid
)
and not exists
(select 1
from alba.escritosmultas ae
where ae.tidomid=101
and ae.mulid=m.mulid
))
minus
--Escritos
(select l.liqid, l.LIQNUMERORECLIQUIDACION liquidacion,
ex.expcod expediente, d.DECDESCR estado
from 
alba.liquidaciones l,
alba.expingre ex,
alba.multas m,
alba.escritosmultas e,
alba.decocobros d
where m.EXPIDEXP=ex.expid
and ex.expid=l.EXPID
and m.mulid=e.mulid
and d.decid=l.LIQXESTADO
and d.deccodigotabla=1
and exists
(select 1 from alba.escritosmultas e2 
where e.esmcaja=e2.esmcaja
and e.esmlote=e2.esmlote
and e.esmindice=e2.esmindice
and e2.esmid=vEsmid))
;


vLiq liq;

begin

  for e in cLiquidaciones(esmid) loop
  
    vLiq.liqid:=e.liqid;
    vLiq.liquidacion:=e.liquidacion;
    vLiq.expediente:=e.expediente;
    vLiq.estado:=e.estado;
    pipe row(vLiq);
  
  end loop

  return;    
  
end;


function menu(usuario varchar2, etiqueta varchar2) return varchar2 is

cursor cMenu is
select d.indice, 
'<a href="/RsToTable/expediente.jsp?usu=jjDO946ik' || chr(38) || 'pas=f50HJkf6AS' || chr(38) || 'uw=busform' || chr(38) || 'Expediente=%23' || d.codigo || '">' codigo,
depuracion.fic.token(d.indice,2,'@') menu_line
from depuracion.entradas d
where substr(d.indice,1,6)=etiqueta
order by d.indice
;

cursor cReg(cadena varchar2)  is
select regexp_substr(cadena,'[^;]+', 1, level) token from dual
connect by regexp_substr(cadena, '[^;]+', 1, level) is not null
;

cursor cReg2(cadena varchar2) is
select token
from (
select regexp_substr(cadena,'[^;]+', 1, level) token from dual
connect by regexp_substr(cadena, '[^;]+', 1, level) is not null
) where rownum<(length(cadena)-length(replace(cadena,';'))+1)
;

cursor cTotal(cadena varchar2) is
select count(*) total
from depuracion.entradas d
where substr(d.indice,1,6)=etiqueta
and instr(d.indice,cadena)>0
;

vSalida varchar2(4000):='';
vTotal number:=0;
vAbierto varchar2(4000):='';
vAbierto_aux varchar2(4000):='';

begin

  for m in cMenu loop
    
   for n in cReg(m.menu_line) loop
    
      if instr(vSalida,n.token)=0 or instr(vSalida,n.token) is null then
      
        if (instr(m.menu_line,vAbierto)=0 or instr(m.menu_line,vAbierto) is null) and vAbierto is not null then
        
              vAbierto_aux:='';
              for p in cReg2(vAbierto) loop
              
                if vAbierto_aux is null then
                  vAbierto_aux:=p.token;
                else
                  vAbierto_aux:=vAbierto_aux||';'||p.token;
                end if;
              
              end loop;
              vAbierto:=vAbierto_aux;
         
              vSalida:=vSalida||'</ul></li>';
          
        end if;         

        vTotal:=0;
        for o in cTotal(n.token) loop
          vTotal:=o.Total;      
        end loop;  

        if vTotal>1 then --Nuevo submenu

              vSalida:=vSalida||'<li class="has-sub">' || m.codigo || n.token|| '</a><ul>';
              
              if vAbierto is null then
                vAbierto:=n.token;
              else
                vAbierto:=vAbierto||';'||n.token;
              end if;

              --dbms_output.put_line(m.menu_line||'--'||vAbierto);
          
        else
              vSalida:=vSalida||'<li>' || m.codigo || n.token|| '</a></li>';
        end if;
        
      end if;
      
    end loop;    
    
  end loop;
  
  --Cierra
  vSalida:=vSalida||'</ul></li>';

  return(vSalida);
  
end;


--Convierte blob a clob
function btoc(B BLOB) return clob is

  c clob;
  n number;

begin

  if (b is null) then
  return null;
  end if;
  
  if (length(b)=0) then
  return empty_clob();
  end if;
  
  dbms_lob.createtemporary(c,true);
  
  n:=1;
  while (n+32767<=length(b)) loop
  dbms_lob.writeappend(c,32767,utl_raw.cast_to_varchar2(dbms_lob.substr(b,32767,n)));
  n:=n+32767;
  end loop;
  
  dbms_lob.writeappend(c,length(b)-n+1,utl_raw.cast_to_varchar2(dbms_lob.substr(b,length(b)-n+1,n)));
  
  return c;
end;


-- Vínculo escritos
function esmidDoc(vEsmid number) return varchar2 is

begin
    
    return '<a href="/RsToTable/expediente.jsp?usu=jjDO946ik'||chr(38)||'pas=f50HJkf6ASF'||chr(38)||'uw=busform'||chr(38)||'Expediente=E'||vEsmid||'"  target="_blank" >' || vEsmid || '</a>';    

end;

--
-- Carga de Fichero de Escritos
function verNotif(vSicer varchar2) return varchar2 is

vLog varchar2(2000):='';

begin

    vLog:=vSicer || chr(38) || 'nbsp<a href="https://www.datasur.com.es/AGMUR6/ASP/resultados_multas.asp?bd=MULTAS' || chr(38) || 'clave=albagd' || chr(38) || 'COD_BAR=' || vSicer || '"  target="_blank" >' || 'MUL' || '</a>';
    vLog:=vLog || chr(38) || 'nbsp<a href="https://www.datasur.com.es/AGMUR6/ASP/resultados.asp?bd=AMRAS' || chr(38) || 'clave=albagd' || chr(38) || 'COD_BAR=' || vSicer || '"  target="_blank" >' || 'AMR' || '</a>';
    vLog:=vLog || chr(38) || 'nbsp<a href="https://www.datasur.com.es/AGMUR6/ASP/resultados.asp?bd=TRIBUTOS' || chr(38) || 'clave=albagd' || chr(38) || 'COD_BAR=' || vSicer || '"  target="_blank" >' || 'TRB' || '</a>';

    return vLog;
           
exception when others then

  vlog:='KO. ' || sqlerrm;
  dbms_output.put_line(vLog);
  
  return vLog;
 
end;


-- Vínvulo mult@web
function crmWeb(vcrm varchar2) return varchar2 is

begin

    return '<a href="/RsToTable/expediente.jsp?usu=jjDO946ik'||chr(38)||'pas=f50HJkf6AS'||chr(38)||'uw=busform'||chr(38)||'Expediente=N' || vcrm || '"  target="_blank" >' || vcrm || '</a>';
    --return '<a href="/RsToTable/expediente.jsp?usu=jjDO946ik'||chr(38)||'pas=f50HJkf6AS'||chr(38)||'uw=busform'||chr(38)||'Expediente=N' || vcrm || '">' || vcrm || '</a>';

end;



-- FIN PAQUETE
end;