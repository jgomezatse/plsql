create or replace PACKAGE BODY TRES is

--------------------------------------------------------------------------------
-- Normaliza caracteres Portafirmas
--------------------------------------------------------------------------------
function nz(vIn varchar2) return varchar2 is
begin
    return translate(vIn,
    '·ÈÌÛ˙‡ËÏÚ˘„ı‚ÍÓÙÙ‰ÎÔˆ¸Á¡…Õ”⁄¿»Ã“Ÿ√’¬ Œ‘€ƒÀœ÷‹«/Ò—',
    'aeiouaeiouaoaeiooaeioucAEIOUAEIOUAOAEIOOAEIOUC-nN');
end;



--------------------------------------------------------------------------------
-- AÒadir/Eliminar Destinatarios
--------------------------------------------------------------------------------
procedure procesarDestinatario(vEsmid number,vNif varchar2) is

cursor cDestinatario(vEsmid number,vNif varchar2) is
select d.esmid, d.destinatario, 
decode(instr(d.destinatario,vNif),null,2,0,0,1) fin,
decode(instr(d.destinatario,vNif||';'),null,2,0,0,1) medio
from depuracion.escritosmultas_doc d
where d.esmid=vEsmid and d.cestado=1;

vDestinatario varchar2(200):='';

begin

  for d in cDestinatario(vEsmid,vNif) loop
    if d.medio=2 then
        vDestinatario:=d.destinatario||vNif;
    elsif d.medio=1 then
        vDestinatario:=replace(d.destinatario,vNif||';','');
    elsif d.fin=1 then
        vDestinatario:=replace(d.destinatario,';'||vNif,'');
        vDestinatario:=replace(vDestinatario,vNif,'');
    else
        vDestinatario:=d.destinatario||';'||vNif;
    end if;
    
    update depuracion.escritosmultas_doc d2
    set d2.destinatario=vDestinatario
    where d2.esmid=d.esmid;
    
    commit;
  end loop;

end;


--------------------------------------------------------------------------------
-- Seleccionar Destinatarios
--------------------------------------------------------------------------------
function selectDestinatarios(vEsmid number) return varchar2 is

cursor cDes(vGrupo number) is
select '<option value="' || u.nif || '">' || u.nombre || '</option>' texto
from depuracion.usufirma u
where 
(login in ('cavidgar','rivilmac','esbenmar','mtmaqped','saferlop','viortsae','jurodrod') and vGrupo=2)
or
(login in ('veguelar','ficruvil','mapersev','criglpue','mjmorpal','jlloplem','agmurpei','ilcanrem') and vGrupo=1)
order by u.nombre
;

cursor cEscrito(vEsmid number) is
select decode(e.tidomid,101,2,1) grupo
from alba.escritosmultas e
where e.esmid=vEsmid;


vOut varchar2(2000);
begin

  vOut:='';
  
  for e in cEscrito(vEsmid) loop
    for t in cDes(e.grupo) loop
      vOut:=vOut||t.texto;
    end loop;
  end loop;

  return vOut;

end;


--------------------------------------------------------------------------------
-- GENERACI”N DATOS DE INFORME
--------------------------------------------------------------------------------
procedure generarInforme(vEsmid number) is

cursor cEscrito(vEsmid number) is
select 
e3.esmid, 
(select count(e2.esmid) from alba.escritosmultas e2 where e2.esmid=e3.esmid
--and e2.tdmresid in (261,281,385,401,421)
) escrito, 
(select count(d.esmid) from depuracion.escritosmultas_doc d where d.esmid=e3.esmid) informe
from
(select vEsmid esmid from dual) e3,
(select * from alba.escritosmultas) e
where e3.esmid=e.esmid(+)
;

cursor cDatos(vEsmid number) is
SELECT DISTINCT *
FROM
  (SELECT 
      upper(d.nombred) || ' ' || upper(d.nifd) recurrente,
      upper(d.domiciliod) domicilio,
      (SELECT esmivalor FROM alba.escritosmultasinfo inf
      WHERE inf.esmid =e.esmid AND esmietiqueta='PLENO'
      AND rownum<2 AND esmiiestado =1) AS "PLENO",
      (SELECT esmivalor FROM alba.escritosmultasinfo inf
      WHERE inf.esmid =e.esmid AND esmietiqueta='EXPTE. TEA'
      AND rownum<2 AND esmiiestado=1) AS "EXPTEA",
      NVL((SELECT esmivalor
      FROM alba.escritosmultasinfo inf WHERE inf.esmid =e.esmid
      AND esmietiqueta='ASUNTO' AND rownum<2 AND esmiiestado =1),
      'ACTUACION PROVIDENCIA DE APREMIO, DILIGENCIA DE EMBARGO DE CUENTAS BANCARIAS, A PLAZO, EFECTIVO.') AS "ASUNTO",
      e.usuidmod usuid, 
      l.liqid, l.liqnumerorecliquidacion liquidacion, ex.expcod expediente,
      (select tidomdesc from tipos_doc_multas where tidomid=e.tidomid) tipo,
      (select tdmnombre from tipos_doc_multas_res where tdmresid=e.tdmresid) estado
  FROM alba.liquidaciones l, alba.expingre ex, alba.multas m, alba.escritosmultas e,
      TABLE(depuracion.trml.dmultas(m.mulid,e.esmid)) d
  WHERE e.mulid =m.mulid and e.esmid=vEsmid
  AND m.EXPIDEXP=ex.expid AND ex.expid  =l.EXPID(+)
);

cursor usuarios(vUsuid number) is
select u.nif,u.nombre from depuracion.usufirma u 
where u.usuid=vUsuid and rownum<2;

cursor destinatario(vEsmid number) is
select d.destinatario from depuracion.escritosmultas_doc d 
where d.esmid=vEsmid;

cursor cInformec(vEsmid number) is
select depuracion.tres.informe(d.esmid) informeNew, 
d.informec informeOld, d.estado estado from depuracion.escritosmultas_doc d 
where d.esmid=vEsmid;

vInformen VARCHAR2(200):='';
vNifremite VARCHAR2(20):='';
vNombreremite VARCHAR2(200):='';
vReferencia VARCHAR2(200):='';
vAsunto VARCHAR2(200):='';
vTexto VARCHAR2(200):='';
vDestinatario VARCHAR2(200):='';
vEstado varchar2(30):='PENDIENTE';
vInformeNew clob:='';
vInformeOld  clob:='';

begin

  for i in cDatos(vEsmid) loop
  
    vNifremite:='';
    vNombreremite:=nz('');
    for u in usuarios(i.usuid) loop
      vNifremite:=nz(u.nif);
      vNombreremite:=nz(u.nombre);
    end loop;
    if vNombreremite is null or vNombreremite='' then
      vNombreremite:='No existe usuario ' || i.usuid;
    end if;
    
    vReferencia:='esmid'||vEsmid;
    vAsunto:=nz(substr(i.tipo || ' ' || i.estado || ' ' || nvl(i.exptea,i.expediente),1,200));
    vTexto:=nz(substr(i.tipo || ' ' || i.estado || ' ' || nvl(i.exptea,i.expediente) || ' ' || i.recurrente || ' ' || ' #WSDEB#',1,200));
    --vInformen:=nz('Exptea_' || i.exptea ||'.pdf');
    if i.exptea is null then
      vInformen:=nz(i.expediente || '_' || vEsmid || '_' || i.tipo || '.pdf');
    else
      vInformen:=nz('TEA_' || i.exptea || '_' || vEsmid || '.pdf');
    end if;
    
    vDestinatario:='';
    
  end loop;
  
  for d in destinatario(vEsmid) loop
    vDestinatario:=d.destinatario;
  end loop;

  for i in cEscrito(vEsmid) loop
  
    if i.escrito=1 then
    
      if vInformen is null or vInformen=''
      or vNifremite is null or vNifremite='' 
      or vReferencia is null or vReferencia='' 
      or vAsunto is null or vAsunto='' 
      or vTexto is null or vTexto='' 
      or vDestinatario is null or vDestinatario=''
      then
        vEstado:='INCOMPLETO';
      end if;
      
      if i.informe=0 then
      
        insert into depuracion.escritosmultas_doc d
        (esmid,esmdfhoramod,usuidmod,esmdmotivo,informec,cestado,estado,
        informen,nifremite,nombreremite,referencia,asunto,texto,destinatario)
        values
        (vEsmid,sysdate,0,'alta',(select depuracion.tres.informe(vEsmid) informe from dual),1,'GENERAR_PDF',
        vInformen,vNifremite,vNombreremite,vReferencia,vAsunto,vTexto,vDestinatario);
        
        commit;
        
      else
      
        for i in cInformec(vEsmid) loop
          vInformeNew:=i.informeNew;
          vInformeOld:=i.informeOld;
          if i.estado='GENERAR_PDF' then
            vEstado:='GENERAR_PDF';
          end if;
        end loop;
        
        if dbms_lob.compare(vInformeNew,vInformeOld)=0 then
            update depuracion.escritosmultas_doc d
            set 
            d.cestado=1,d.estado=vEstado,d.esmdfhoramod=sysdate,
            d.usuidmod=0, d.esmdmotivo='mofificacion',
            --d.informec=(select depuracion.tres.informe(vEsmid) informe from dual),
            --d.informeb=null,
            d.informen=vInformen,d.nifremite=vNifremite,d.nombreremite=vNombreremite,
            d.referencia=vReferencia,d.asunto=vAsunto,d.texto=vTexto,
            d.destinatario=vDestinatario        
            where d.esmid=vEsmid and d.cestado in (0,1);
        else
            update depuracion.escritosmultas_doc d
            set 
            d.cestado=1,d.estado='GENERAR_PDF',d.esmdfhoramod=sysdate,
            d.usuidmod=0,d.esmdmotivo='mofificacion',
            d.informec=vInformeNew,
            d.informeb=null,
            d.informen=vInformen,d.nifremite=vNifremite,
            d.nombreremite=vNombreremite,d.referencia=vReferencia,
            d.asunto=vAsunto,d.texto=vTexto,d.destinatario=vDestinatario        
            where d.esmid=vEsmid and d.cestado in (0,1);        
        end if;
        
        commit;        
      
      end if;
    
    end if;
  
  end loop;

end;


-------------------------------------------------------------------------------
-- INFORME MULTAS Y RESTO
--------------------------------------------------------------------------------
function informe1(vEsmid number) return clob is

cursor cDatos(vEsmid number) is
SELECT 
d.nexpediente expediente,d.boletin,d.fdenuncia fecha,d.hora,d.lugar,d.matricula,
d.vehiculo,d.motivonoentrega motivo,d.hecho,d.tiemporetirada importe,
upper(d.nifd) nif,
upper(d.nombred) denunciado,
upper(d.domiciliod) domicilio
FROM alba.liquidaciones l,
alba.expingre ex,
alba.multas m,
alba.escritosmultas e,
TABLE(depuracion.trml.dmultas(m.mulid,e.esmid)) d
WHERE e.mulid =m.mulid
AND m.EXPIDEXP=ex.expid
AND ex.expid=l.EXPID(+)
and e.esmid=vEsmid
;


cursor cLiquidaciones(vEsmid number) is
select distinct
  ex.expcod EXPTE, l.LIQNUMERORECLIQUIDACION LIQUIDACION
from 
  alba.liquidaciones l, alba.expingre ex, alba.multas m, alba.escritosmultas e
where e.mulid=m.mulid and m.EXPIDEXP=ex.expid and ex.expid=l.EXPID
and exists
  (select 1 from alba.escritosmultas e2 
  where e2.esmid=vEsmid and e.esmcaja=e2.esmcaja 
  and e.esmlote=e2.esmlote and e.esmindice=e2.esmindice)
and l.liqid in 
  (select max(l2.liqid) from liquidaciones l2 where l2.expid=l.expid)
order by l.LIQNUMERORECLIQUIDACION asc
;


cursor cCabecera(vEsmid number) is
select 
replace(
replace(rsaresolucion,CHR(13),'\\'||CHR(10)||CHR(13))
,'"','``')
res
from 
alba.resoluciones rs, alba.escritosmultas e, alba.resol_alegaciones r
WHERE r.resid=rs.resid
and r.ESMID=e.esmid
and rs.orden in ('CABECERA','PRUEBA CABECERA') and rownum<2
and exists
(select 1 from alba.escritosmultas e2 
where e2.esmid=vEsmid and e.esmcaja=e2.esmcaja
and e.esmlote=e2.esmlote and e.esmindice=e2.esmindice)
;

cursor cDetalle(vEsmid number) is
select
'\\'||CHR(10)||CHR(13)||
replace(
replace(
replace(
replace(rsaresolucion,CHR(13),'\\'||CHR(10)||CHR(13)),
'@recibos',
depuracion.trml.parametro(e.esmid,r.resid,'recibos')),
'@recibo',
depuracion.trml.parametro(e.esmid,r.resid,'recibo')
),'"','``') || '\\'
res
from 
alba.resoluciones rs, alba.escritosmultas e, alba.resol_alegaciones r
WHERE r.resid=rs.resid and r.ESMID=e.esmid
and r.rsaresolucion is not null
and (rs.orden is null or rs.orden='PRUEBA DETALLE')
and exists
(select 1 from alba.escritosmultas e2 
where e2.esmid=vEsmid and e.esmcaja=e2.esmcaja
and e.esmlote=e2.esmlote and e.esmindice=e2.esmindice)
order by r.rsaid asc
;

cursor cPie(vEsmid number) is
select 
replace(
replace(rsaresolucion,CHR(13),'\\'||CHR(10)||CHR(13))
,'"','``') 
res
from 
alba.resoluciones rs, alba.escritosmultas e, alba.resol_alegaciones r
WHERE r.resid=rs.resid and r.ESMID=e.esmid
and rs.orden in ('PIE','PRUEBA PIE') and rownum<2
and exists
(select 1 from alba.escritosmultas e2 
where e2.esmid=vEsmid and e.esmcaja=e2.esmcaja
and e.esmlote=e2.esmlote and e.esmindice=e2.esmindice)
;

vSalida clob;
vAux clob;
vTemp clob;

begin

  vSalida:=q'~
  \documentclass[10pt]{article}
  \usepackage[sfdefault,light]{roboto}
  \usepackage[T1]{fontenc}
  \usepackage[spanish,activeacute]{babel}
  \usepackage{mathtools}
  \usepackage{amsmath}
  \usepackage[utf8]{inputenc}
  \usepackage{caption}
  \usepackage{fancyhdr}
  \usepackage{anysize}
  \usepackage{float}
  \usepackage{lastpage}
  \usepackage[none]{hyphenat}  %evitar salto de linea  
  
  \papersize{29.7cm}{21.0cm} %para tamaÒo carta, para otros eligieran la correcta
  \marginsize{2cm}{2cm}{3cm}{2cm} %\marginsize{Izque}{Derec}{Arrib}{Abajo}

  \setlength{\parindent}{0cm} % evita sangrado de p·rrafos
  
  \pagestyle{fancy}
  \renewcommand{\headrulewidth}{0pt} % grosor de la lÌnea de la cabecera
  \renewcommand{\footrulewidth}{0pt} % grosor de la lÌnea del pie
  
  
  \lhead{\begin{picture}(0,0) \put(0,0){\includegraphics[width=0.25\textwidth]{/var/www/html/logo.jpg}} \end{picture} \\}
  \chead{{\tiny ¡REA DE COORDINACI”N\\DELEGACI”N DE HACIENDA\\AGENCIA TRIBUTARIA DE SEVILLA\\DEPARTAMENTO DE GESTI”N DE SANCIONES\\}}
  \rhead{@expediente \\}
  \lfoot{SRA. GERENTE DE LA AGENCIA TRIBUTARIA DE SEVILLA}
  \cfoot{}
  \rfoot{p·g. \thepage}
  
  \begin{document} 
  \sloppy %evitar salto de linea  
  
  \small
  @datos
  @lista_liquidaciones
  
  \small
  @contenido

  \end{document}
  ~';
  
  -----------------
  -- Datos
  -----------------
  for i in cDatos(vEsmid) loop
    
    vAux:=q'~
    \setlength\arrayrulewidth{0.1pt}
    \begin{table}[H]
    \begin{left}
    \begin{tabular}{|p{0.1\textwidth} p{0.85\textwidth}|}
    \hline
    {\small Fecha:}& \textbf{@fecha} \hspace{0.6cm} {\small Hora:} \textbf{@hora}\\
    {\small Lugar:}& \textbf{@lugar}\\
    {\small MatrÌcula:}& \textbf{@matricula} \hspace{0.6cm} {\small VehÌculo:} \textbf{@vehiculo}\\
    {\small Denunciado:}& \textbf{@denunciado} \hspace{0.6cm} {\small Nif:} \textbf{@nif}\\
    {\small Domicilio:}& \textbf{@domicilio}\\
    {\small Hecho Denunciado:}& \textbf{@hecho}\\
    {\small Importe:}& \textbf{Ä @importe} \hspace{0.6cm} {\small Motivo de no entrega en el acto:} \textbf{@motivo}\\    
    \hline
    \end{tabular}
    \end{center}
    \end{table}
    ~';
    
    vAux:=replace(vAux,'@fecha',i.fecha);
    vAux:=replace(vAux,'@hora',i.hora);
    vAux:=replace(vAux,'@lugar',i.lugar);
    vAux:=replace(vAux,'@matricula',i.matricula);
    vAux:=replace(vAux,'@vehiculo',i.vehiculo);
    vAux:=replace(vAux,'@denunciado',i.denunciado);
    vAux:=replace(vAux,'@nif',i.nif);
    vAux:=replace(vAux,'@domicilio',i.domicilio);
    vAux:=replace(vAux,'@motivo',i.motivo);
    vAux:=replace(vAux,'@importe',i.importe);
    vAux:=replace(vAux,'@hecho',i.hecho);    
    
    vSalida:=replace(vSalida,'@expediente','Expediente \textbf{' || i.expediente || '} \\ BoletÌn \textbf{' || i.boletin || '}');
    
  end loop;
  
  vSalida:=replace(vSalida,'@datos',vAux);
  
  

  
  
  -------------------------
  -- Cabecera, Detalle, Pie
  -------------------------  
  vAux:='';
  
  for i in cCabecera(vEsmid) loop
      vAux:=vAux || i.res;
  end loop;

  for i in cDetalle(vEsmid) loop
      vAux:=vAux || i.res;
  end loop;

  for i in cPie(vEsmid) loop
      vAux:=vAux || i.res;
  end loop;  
  
 
  vSalida:=replace(vSalida,'@contenido',vAux);
  
  return vSalida;

end;



--------------------------------------------------------------------------------
-- INFORME TEA
--------------------------------------------------------------------------------
function informe2(vEsmid number) return clob is

cursor cDatos(vEsmid number) is
SELECT DISTINCT *
FROM
  (SELECT 
      upper(d.nombred) recurrente,
      upper(d.domiciliod) domicilio,
      (SELECT esmivalor FROM alba.escritosmultasinfo inf WHERE inf.esmid =e.esmid
      AND esmietiqueta='PLENO' AND rownum<2 AND esmiiestado =1) AS "PLENO",
      (SELECT esmivalor FROM alba.escritosmultasinfo inf WHERE inf.esmid =e.esmid
      AND esmietiqueta='EXPTE. TEA' AND rownum<2 AND esmiiestado =1) AS "EXPTEA",
      NVL(
      (SELECT esmivalor FROM alba.escritosmultasinfo inf WHERE inf.esmid =e.esmid
      AND esmietiqueta='ASUNTO' AND rownum<2 AND esmiiestado =1
      ), 'ACTUACION PROVIDENCIA DE APREMIO, DILIGENCIA DE EMBARGO DE CUENTAS BANCARIAS, A PLAZO, EFECTIVO.') AS "ASUNTO"
  FROM alba.liquidaciones l,
      alba.expingre ex,
      alba.multas m,
      alba.escritosmultas e,
      TABLE(depuracion.trml.dmultas(m.mulid,e.esmid)) d
  WHERE e.mulid =m.mulid
  AND m.EXPIDEXP=ex.expid
  AND ex.expid=l.EXPID(+)
  AND EXISTS
      (SELECT 1 FROM alba.escritosmultasinfo ei WHERE ei.esmid=e.esmid AND ei.esmietiqueta='EXPTE. TEA')
  AND EXISTS
      (SELECT 1 FROM alba.escritosmultas e2 WHERE e2.esmid =vEsmid 
      AND e.esmcaja=e2.esmcaja AND e.esmlote=e2.esmlote AND e.esmindice=e2.esmindice)
  ORDER BY l.LIQNUMERORECLIQUIDACION ASC
)
;


cursor cLiquidaciones(vEsmid number) is
select distinct
  ex.expcod EXPTE, l.LIQNUMERORECLIQUIDACION LIQUIDACION
from 
  alba.liquidaciones l, alba.expingre ex, alba.multas m, alba.escritosmultas e
where e.mulid=m.mulid and m.EXPIDEXP=ex.expid and ex.expid=l.EXPID
and exists
  (select 1 from alba.escritosmultas e2 
  where e2.esmid=vEsmid and e.esmcaja=e2.esmcaja 
  and e.esmlote=e2.esmlote and e.esmindice=e2.esmindice)
and l.liqid in 
  (select max(l2.liqid) from liquidaciones l2 where l2.expid=l.expid)
order by l.LIQNUMERORECLIQUIDACION asc
;


cursor cCabecera(vEsmid number) is
select 
replace(
replace(rsaresolucion,CHR(13),'\\'||CHR(10)||CHR(13))
,'"','``')
res
from 
alba.resoluciones rs, alba.escritosmultas e, alba.resol_alegaciones r
WHERE r.resid=rs.resid
and r.ESMID=e.esmid
and rs.orden in ('CABECERA','PRUEBA CABECERA') and rownum<2
and exists
(select 1 from alba.escritosmultas e2 
where e2.esmid=vEsmid and e.esmcaja=e2.esmcaja
and e.esmlote=e2.esmlote and e.esmindice=e2.esmindice)
;

cursor cDetalle(vEsmid number) is
select
'\\'||CHR(10)||CHR(13)||
replace(
replace(
replace(
replace(rsaresolucion,CHR(13),'\\'||CHR(10)||CHR(13)),
'@recibos',
depuracion.trml.parametro(e.esmid,r.resid,'recibos')),
'@recibo',
depuracion.trml.parametro(e.esmid,r.resid,'recibo')
),'"','``') || '\\'
res
from 
alba.resoluciones rs, alba.escritosmultas e, alba.resol_alegaciones r
WHERE r.resid=rs.resid and r.ESMID=e.esmid
and r.rsaresolucion is not null
and (rs.orden is null or rs.orden='PRUEBA DETALLE')
and exists
(select 1 from alba.escritosmultas e2 
where e2.esmid=vEsmid and e.esmcaja=e2.esmcaja
and e.esmlote=e2.esmlote and e.esmindice=e2.esmindice)
order by r.rsaid asc
;

cursor cPie(vEsmid number) is
select 
replace(
replace(rsaresolucion,CHR(13),'\\'||CHR(10)||CHR(13))
,'"','``') 
res
from 
alba.resoluciones rs, alba.escritosmultas e, alba.resol_alegaciones r
WHERE r.resid=rs.resid and r.ESMID=e.esmid
and rs.orden in ('PIE','PRUEBA PIE') and rownum<2
and exists
(select 1 from alba.escritosmultas e2 
where e2.esmid=vEsmid and e.esmcaja=e2.esmcaja
and e.esmlote=e2.esmlote and e.esmindice=e2.esmindice)
;

vSalida clob;
vAux clob;
vTemp clob;

begin

  vSalida:=q'~
  \documentclass[10pt]{article}
  \usepackage[sfdefault,light]{roboto}
  \usepackage[T1]{fontenc}
  \usepackage[spanish,activeacute]{babel}
  \usepackage{mathtools}
  \usepackage{amsmath}
  \usepackage[utf8]{inputenc}
  \usepackage{caption}
  \usepackage{fancyhdr}
  \usepackage{anysize}
  \usepackage{float}
  \usepackage{lastpage}
  \usepackage[none]{hyphenat}  %evitar salto de linea  
  
  \papersize{29.7cm}{21.0cm} %para tamaÒo carta, para otros eligieran la correcta
  \marginsize{2cm}{2cm}{3cm}{2cm} %\marginsize{Izque}{Derec}{Arrib}{Abajo}

  \setlength{\parindent}{0cm} % evita sangrado de p·rrafos
  
  \pagestyle{fancy}
  \renewcommand{\headrulewidth}{0pt} % grosor de la lÌnea de la cabecera
  \renewcommand{\footrulewidth}{0pt} % grosor de la lÌnea del pie
  
  
  \lhead{\begin{picture}(0,0) \put(0,0){\includegraphics[width=0.3\textwidth]{/var/www/html/tea.jpg}} \end{picture}}
  \chead{}
  \rhead{@exptea}
  \lfoot{}
  \cfoot{}
  \rfoot{p·g. \thepage}
  
  \begin{document} 
  \sloppy %evitar salto de linea  

  @datos
  @lista_liquidaciones
  
  \small
  @contenido

  \end{document}
  ~';
  
  -----------------
  -- Datos
  -----------------
  for i in cDatos(vEsmid) loop
    
    vAux:=q'~
    \setlength\arrayrulewidth{0.1pt}
    Datos de la ReclamaciÛn
    \begin{table}[H]
    \begin{left}
    \begin{tabular}{|l p{0.81\textwidth}|}
    \hline
    RECURRENTE & \textbf{@recurrente}\\
    DOMICILIO & \textbf{@domicilio}\\
    PLENO & \textbf{@pleno}\\
    EXPEDIENTE TEA & \textbf{@exptea}\\
    ASUNTO & \textbf{@asunto}\\
    \hline
    \end{tabular}
    \end{center}
    \end{table}
    ~';
    
    vAux:=replace(vAux,'@recurrente',i.recurrente);
    vAux:=replace(vAux,'@domicilio',i.domicilio);
    vAux:=replace(vAux,'@pleno',i.pleno);
    vAux:=replace(vAux,'@exptea',i.exptea);
    vAux:=replace(vAux,'@asunto',i.asunto);
    
    vSalida:=replace(vSalida,'@exptea','Expediente ' || i.exptea);
    
  end loop;
  
  vSalida:=replace(vSalida,'@datos',vAux);
  
  
  -----------------
  -- Liquidaciones
  -----------------
  vAux:=q'~
  \setlength\arrayrulewidth{0.1pt}
  Expedientes de Tr·fico
  \begin{table}[H]
  \begin{left}
  \begin{tabular}{|l l|}
  \hline
  EXPEDIENTE & \ LIQUIDACI”N\\
  @liq_lista
  \hline
  \end{tabular}
  \end{center}
  \end{table}
  ~';
  vTemp:='';
  for i in cLiquidaciones(vEsmid) loop
    vTemp:=vTemp || ' ' || i.expte|| ' ' || chr(38) || ' ' || i.liquidacion || '\\';
  end loop;

  vAux:=replace(vAux,'@liq_lista',vTemp);
  vSalida:=replace(vSalida,'@lista_liquidaciones',vAux);  
  
  
  -------------------------
  -- Cabecera, Detalle, Pie
  -------------------------  
  vAux:='';
  
  for i in cCabecera(vEsmid) loop
      vAux:=vAux || i.res;
  end loop;

  for i in cDetalle(vEsmid) loop
      vAux:=vAux || i.res;
  end loop;

  for i in cPie(vEsmid) loop
      vAux:=vAux || i.res;
  end loop;  
  
  select depuracion.trml.parametro(vEsmid,null,'expedientes') into vTemp from dual;
  vAux:=replace(vAux,'@expedientes',vTemp); vTemp:='';
  select depuracion.trml.parametro(vEsmid,null,'estimadas') into vTemp from dual;
  vAux:=replace(vAux,'@estimadas',vTemp); vTemp:='';  
  select depuracion.trml.parametro(vEsmid,null,'desestimadas') into vTemp from dual;
  vAux:=replace(vAux,'@desestimadas',vTemp); vTemp:='';  
  select depuracion.trml.parametro(vEsmid,null,'hoy') into vTemp from dual;
  vAux:=replace(vAux,'@hoy',vTemp); vTemp:='';  
  select depuracion.trml.parametro(vEsmid,null,'recurrente') into vTemp from dual;
  vAux:=replace(vAux,'@recurrente',vTemp); vTemp:='';  
  select depuracion.trml.parametro(vEsmid,null,'domicilio') into vTemp from dual;
  vAux:=replace(vAux,'@domicilio',vTemp); vTemp:='';  
  select depuracion.trml.parametro(vEsmid,null,'pleno') into vTemp from dual;
  vAux:=replace(vAux,'@pleno',vTemp); vTemp:='';  
  select depuracion.trml.parametro(vEsmid,null,'expediente_tea') into vTemp from dual;
  vAux:=replace(vAux,'@expediente_tea',vTemp); vTemp:='';  
  select depuracion.trml.parametro(vEsmid,null,'asunto') into vTemp from dual;
  vAux:=replace(vAux,'@asunto',vTemp); vTemp:='';  
  -----------------
  -----------------  
  
  vSalida:=replace(vSalida,'@contenido',vAux);
  
  return vSalida;

end;


function informe(vEsmid number) return clob is

cursor cEscrito(vEsmid number) is
select decode(e.tidomid,101,2,1) informe
from alba.escritosmultas e
where e.esmid=vEsmid;

vOut clob:='';

begin

  for e in cEscrito(vEsmid) loop
    if e.informe=1 then
      vOut:=informe1(vEsmid);
    elsif e.informe=2 then
      vOut:=informe2(vEsmid);      
    end if;
  end loop;
  
  return vOut;
  
end;


end TRES;