create or replace PACKAGE BODY TRES is

--------------------------------------------------------------------------------
-- Normaliza caracteres Portafirmas
--------------------------------------------------------------------------------
function nz(vIn varchar2) return varchar2 is
begin
    return translate(vIn,
    'áéíóúàèìòùãõâêîôôäëïöüçÁÉÍÓÚÀÈÌÒÙÃÕÂÊÎÔÛÄËÏÖÜÇ/ñÑºªØ^_',
    'aeiouaeiouaoaeiooaeioucAEIOUAEIOUAOAEIOOAEIOUC-nN  O  ');
end;

function nz2(vIn varchar2) return varchar2 is
begin
    return replace(vIn,'&','\&');
end;

--------------------------------------------------------------------------------
-- Editar escrito firmado
--------------------------------------------------------------------------------
procedure editarEscritoFirmado(vEsmid number) is
begin
  update alba.escritosmultas_doc e
  set e.cestado=1, e.estado='PENDIENTE'
  where e.esmid=vEsmid 
  and
  (
    (e.cestado=4 and e.estado='FIRMADO')
    or
    (
    e.cestado=2
    and e.estado='PENDIENTE'
    and e.nifremite is not null 
    and e.destinatario is not null 
    and e.informeb is not null
    and instr(substr(e.firma,1,200),'CTRANSACTIONID = None')>0
    and instr(substr(e.firma,1,200),'CESTADO = "EN PROCESO')>0
    and e.esmdfhoramod<to_date(sysdate,'dd/mm/rrrr')-4    
    )
  )
  ;
  commit;
end;

--------------------------------------------------------------------------------
-- Añadir/Eliminar Destinatarios
--------------------------------------------------------------------------------
procedure procesarDestinatario(vEsmid number,vNif varchar2) is

cursor cDestinatario(vEsmid number,vNif varchar2) is
select d.esmid, d.destinatario, 
decode(instr(d.destinatario,vNif),null,2,0,0,1) fin,
decode(instr(d.destinatario,vNif||';'),null,2,0,0,1) medio
from alba.escritosmultas_doc d
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
    
    update alba.escritosmultas_doc d2
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
from alba.usufirma u
where 
(login in ('cavidgar','rivilmac','esbenmar','mtmaqped','saferlop','viortsae','jurodrod','mtmaqped','mesanort','sadomgar') and vGrupo=2)
or
(login in ('veguelar','ficruvil','mapersev','criglpue','mjmorpal','jlloplem','agmurpei','ilcanrem','maromlin') and vGrupo=1)
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
-- GENERACIÓN DATOS DE INFORME
--------------------------------------------------------------------------------
procedure generarInforme(vEsmid number) is

cursor cEscrito(vEsmid number) is
select 
e3.esmid, 
(select count(e2.esmid) from alba.escritosmultas e2 where e2.esmid=e3.esmid
--and e2.tdmresid in (261,281,385,401,421)
and e2.tidomid in (1,2,101,3,61,221,281,205,303) --Alegación, Recurso, REA, Recurso ER, Baja, Req.Den.Voluntario, Diligencia Personación, Req. Subsanación Denuncia, Oficio Genérico
) escrito, 
(select count(d.esmid) from alba.escritosmultas_doc d where d.esmid=e3.esmid) informe
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
      (select tdmnombre from tipos_doc_multas_res where tdmresid=e.tdmresid) estado,
      decode(m.mulxgrua,null,'','GRUA ') ||
      decode(m.infid,9002328,'NO IDENT.',9002344,'NO IDENT.','') etiqueta
  FROM alba.liquidaciones l, alba.expingre ex, alba.multas m, alba.escritosmultas e,
      TABLE(alba.trml.dmultas(m.mulid,e.esmid)) d
  WHERE e.mulid =m.mulid and e.esmid=vEsmid
  AND m.EXPIDEXP=ex.expid AND ex.expid=l.EXPID(+)
);

cursor usuarios(vUsuid number) is
select u.nif,u.nombre from alba.usufirma u 
where u.usuid=vUsuid and rownum<2;

cursor destinatario(vEsmid number) is
select d.destinatario from alba.escritosmultas_doc d 
where d.esmid=vEsmid;

cursor cInformec(vEsmid number) is
select depuracion.tres.informe(d.esmid) informeNew, 
d.informec informeOld, d.estado estado from alba.escritosmultas_doc d 
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
    --vTexto:=nz(substr(i.tipo || ' ' || i.estado || ' ' || nvl(i.exptea,i.expediente) || ' ' || i.recurrente || ' ' || ' #WSDEB#',1,200));
    --vInformen:=nz('Exptea_' || i.exptea ||'.pdf');
    if i.exptea is null then
      vInformen:=nz(i.expediente || ' ' || vEsmid || ' ' || i.tipo || '.pdf');
      vAsunto:=nz(substr(i.tipo || ' ' || i.estado || ' ' || nvl(i.exptea,i.expediente) || ' ' || i.etiqueta,1,200));
      vTexto:=nz(substr(i.tipo || ' ' || i.estado || ' ' || nvl(i.exptea,i.expediente) || ' ' || i.etiqueta || ' ' || i.recurrente || ' ' || ' #WSDEB#',1,200));
    else
      vInformen:=nz('TEA ' || i.exptea || ' ' || vEsmid || '.pdf');
      vAsunto:=nz(substr(i.tipo || ' ' || i.estado || ' ' || nvl(i.exptea,i.expediente),1,200));
      vTexto:=nz(substr(i.tipo || ' ' || i.estado || ' ' || nvl(i.exptea,i.expediente) || ' ' || i.recurrente || ' ' || ' #WSDEB#',1,200));
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
      
        insert into alba.escritosmultas_doc d
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
            update alba.escritosmultas_doc d
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
            update alba.escritosmultas_doc d
            set 
            d.cestado=1,d.estado='GENERAR_PDF',d.esmdfhoramod=sysdate,
            d.usuidmod=0,d.esmdmotivo='mofificacion',
            d.informec=vInformeNew,
            d.informeb=null,
            d.informen=vInformen,d.nifremite=vNifremite,
            d.nombreremite=vNombreremite,d.referencia=vReferencia,
            d.asunto=vAsunto,d.texto=vTexto,d.destinatario=vDestinatario        
            where d.esmid=vEsmid and d.cestado in (0,1,3);        
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
replace(d.vehiculo,'&','\&') vehiculo,d.motivonoentrega motivo,d.hecho,d.tiemporetirada importe,
upper(d.nifd) nif,
upper(d.nombred) denunciado,
upper(d.domiciliod) domicilio, d.localidadd, d.provinciad, d.cpd, d.fincoacion,
ee.fnotden,
decode(e.tidomid,
3,'SR. TTE. DE ALCALDE DEL ÁREA DE HACIENDA Y ADMINISTRACIÓN PÚBLICA',
221,upper(d.nombred)||'\\'|| upper(d.domiciliod)||'. '||d.localidadd||'. '||d.provinciad||'. '||d.cpd,
281,'',
205,'',
303,'',
'SRA. GERENTE DE LA AGENCIA TRIBUTARIA DE SEVILLA') dirigidoa,
decode(e.tidomid,221,'Denunciante:','Denunciado:') etqdenunciado
FROM alba.liquidaciones l,
alba.expingre ex,
alba.multas m,
alba.escritosmultas e,
TABLE(alba.trml.dmultas(m.mulid,e.esmid)) d,
TABLE(alba.trml.emultas(m.mulid)) ee
WHERE e.mulid =m.mulid
and e.mulid=ee.mulid
AND m.EXPIDEXP=ex.expid
AND ex.expid=l.EXPID(+)
and e.esmid=vEsmid
;

cursor cFijo(vEsmid number,vTipo varchar2) is
select 
replace(
replace(resresolucion,CHR(13),'\\'||CHR(10)||CHR(13))
,'"','``') ||'\\'||CHR(10)||CHR(13)
res,
to_char(to_char(e.esmfhoramod,'dd')+0) || 
        ' de ' || lower(trim(to_char(e.esmfhoramod,'Month','NLS_DATE_LANGUAGE = SPANISH'))) || 
        ' de ' || to_char(e.esmfhoramod,'yyyy') fechaDoc
--select rs.*
from 
alba.multas m, alba.resoluciones rs, alba.escritosmultas e
WHERE rs.tidomid=e.tidomid
and rs.tdmresid=e.tdmresid
and e.mulid=m.mulid
and rs.orden in (vTipo)
and 
(
instr(rs.rescodigo,'#'||decode(m.mulxboletin,144,5,303,5,403,5,444,5,423,4,464,6,465,5,523,5,543,5,623,7,664,9,723,8,743,10,800,11,804,12,806,13))>0
or
  (
    instr(rs.rescodigo,'##')>0
    and not exists
    (
      select 1
      from alba.multas m2, alba.resoluciones rs2, alba.escritosmultas e2
      WHERE rs2.tidomid=e2.tidomid and rs2.tdmresid=e2.tdmresid and e2.mulid=m2.mulid and e2.esmid=e.esmid
      and rs2.orden in (vtipo)
      and instr(rs2.rescodigo,'#'||decode(m2.mulxboletin,144,5,303,5,403,5,444,5,423,4,464,6,465,5,523,5,543,5,623,7,664,9,723,8,743,10,800,11,804,12,806,13))>0
    )
  )
)
and e.esmid=vEsmid
and not exists --Muestra la cabecera/pie gernérica si no existe la cabecera/pie particular
(
  select 1
  from alba.resoluciones rs2, alba.escritosmultas e2, alba.resol_alegaciones r2
  where r2.resid=rs2.resid and r2.ESMID=e2.esmid and r2.rsaresolucion is not null
  and rs2.orden is not null and e2.esmid=e.esmid and rs2.orden=rs.orden
)
;

cursor cDetalle(vEsmid number) is
select
replace(replace(replace(res,CHR(13),'\\'||CHR(10)||CHR(13)),'"','``'),'%','\%') || '\\'||CHR(10)||CHR(13) res,
orden
from
(
  select
  case when (e.tidomid in (61,221,281,303) or rs.orden in ('CABECERA','PIE'))
  then r.rsaresolucion
  else decode(e.tidomid,205,'\textbf{Motivo:} ','\textbf{Alegación:} ') || r.rsamotivoresol || CHR(13) ||
       '\textbf{Consideración:} ' || r.rsaresolucion
  end res,
  rs.orden
  from 
  alba.resoluciones rs, alba.escritosmultas e, alba.resol_alegaciones r
  WHERE r.resid=rs.resid and r.ESMID=e.esmid
  and r.rsaresolucion is not null
  --and rs.orden is null --CABECERA Y PIE particular es prioritario frente al fijo
  and e.esmid=Vesmid
  order by r.rsaid asc
)
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
  
  \papersize{29.7cm}{21.0cm} %para tamaño carta, para otros eligieran la correcta
  \marginsize{1.5cm}{1.5cm}{2cm}{3cm} %\marginsize{Izque}{Derec}{Arrib}{Abajo}

  \setlength{\parindent}{0cm} % evita sangrado de párrafos
  
  \pagestyle{fancy}
  \renewcommand{\headrulewidth}{0pt} % grosor de la línea de la cabecera
  \renewcommand{\footrulewidth}{0pt} % grosor de la línea del pie
  
  %Viudas y huérfanas
  \clubpenalty=10000
  \widowpenalty=10000
  
  
  \lhead{\begin{picture}(0,0) \put(0,0){\includegraphics[width=0.25\textwidth]{/var/www/html/logo.jpg}} \end{picture}\\}
  \chead{{\fontsize{8}{8}\selectfont ÁREA DE COORDINACIÓN\\DELEGACIÓN DE HACIENDA\\AGENCIA TRIBUTARIA DE SEVILLA\\DEPARTAMENTO DE GESTIÓN DE SANCIONES\\}}
  \rhead{@expediente\\}
  \lfoot{}
  \cfoot{}
  \rfoot{pág. \thepage}
  \setlength{\headheight}{40pt}
  
  \begin{document} 
  \sloppy %evitar salto de linea  
  
  @datos
 
  @contenido
  
  \vspace*{\fill}
  {\fontsize{12}{8}\selectfont \textbf{@dirigidoa}}

  \end{document}
  ~';
  
  -----------------
  -- Datos
  -----------------
  for i in cDatos(vEsmid) loop
    
    vAux:=q'~
    \setlength\arrayrulewidth{0.1pt}
    \renewcommand*{\arraystretch}{2.2}
    \begin{table}[H]
    \fontsize{10}{7}\selectfont
    \begin{tabular}{|p{0.10\textwidth} p{0.85\textwidth}|}
    \hline
    Fecha:& \textbf{@fecha} \hspace{0.1cm} Hora: \hspace{0.1cm} \textbf{@hora} \hspace{0.1cm} Incoación: \hspace{0.1cm} \textbf{@incoacion} \hspace{0.1cm} Notificación: \hspace{0.1cm} \textbf{@notificacion} \hspace{0.1cm} Importe: \hspace{0.1cm} \textbf{€ @importe}\\
    @verlugar \M Lugar:& \textbf{@lugar}\\
    @vermatricula \M Matrícula:& \textbf{@matricula} \hspace{0.6cm} Vehículo: \hspace{0.6cm} \textbf{@vehiculo}\\
    @etqdenunciado& \textbf{@denunciado} \hspace{0.6cm} Nif: \hspace{0.6cm}  \textbf{@nif}\\
    Domicilio:& \textbf{@domicilio}\\
    Hecho denunciado:& \textbf{@hecho}\\
    @vermotivo & \M Motivo no entrega en el acto: @motivo\\
    \hline
    \end{tabular}
    \end{table}
    ~';
    
    vAux:=replace(vAux,'@fecha',i.fecha);
    vAux:=replace(vAux,'@hora',i.hora);
    vAux:=replace(vAux,'@incoacion',i.fincoacion);
    vAux:=replace(vAux,'@lugar',i.lugar);
    vAux:=replace(vAux,'@matricula',i.matricula);
    vAux:=replace(vAux,'@vehiculo',i.vehiculo);
    vAux:=replace(vAux,'@denunciado',i.denunciado);
    vAux:=replace(vAux,'@etqdenunciado',i.etqdenunciado);
    vAux:=replace(vAux,'@nif',i.nif);
    vAux:=replace(vAux,'@domicilio',i.domicilio || '. CP ' || i.cpd || '. ' || i.localidadd  || '. ' || i.provinciad);
    vAux:=replace(vAux,'@motivo',i.motivo);
    vAux:=replace(vAux,'@importe',i.importe);
    vAux:=replace(vAux,'@hecho',i.hecho);
    vAux:=replace(vAux,'@notificacion',i.fnotden);
    
    if length(i.lugar)<5 then
      vAux:=replace(vAux,'@verlugar','%');
      vAux:=replace(vAux,'@vermotivo','%');
    else 
      vAux:=replace(vAux,'@verlugar','');
    end if;  
    
    if i.motivo is null then
      vAux:=replace(vAux,'@vermotivo','%');
    else 
      vAux:=replace(vAux,'@vermotivo','');
    end if;
    
    if i.matricula is null then
      vAux:=replace(vAux,'@vermatricula','%');
    else 
      vAux:=replace(vAux,'@vermatricula','');
    end if;    
    
    vSalida:=replace(vSalida,'@expediente','Expediente \textbf{' || i.expediente || '} \\ Boletín \textbf{' || i.boletin || '}');
    
    vSalida:=replace(vSalida,'@dirigidoa',i.dirigidoa);
    
  end loop;
  
  vSalida:=replace(vSalida,'@datos',vAux);
  
  

  
  
  -------------------------
  -- Cabecera, Detalle, Pie
  -------------------------  
  vAux:='';
  
  for i in cFijo(vEsmid,'CABECERA') loop
      vAux:=vAux || i.res;
  end loop;

  for i in cDetalle(vEsmid) loop
      vAux:=vAux || i.res;
  end loop;
  
  -- Replace ------------------
  vAux:=replace(vAux,'Sra. Gerente','\begin{center} \textbf{SRA. GERENTE} \end{center}');
  vAux:=replace(vAux,'Ilmo. Sr. Teniente de Alcalde del Área de Hacienda y Administración Pública',
                      '\begin{center} \textbf{ILMO. SR. TENIENTE DE ALCALDE DEL ÁREA DE HACIENDA Y ADMINISTRACIÓN PÚBLICA} \end{center}');      
  
  if instr(upper(vAux),'INFORME CON PROPUESTA DE RESOLUCIÓN')>0 then
    vAux:=replace(vAux,'INFORME CON PROPUESTA DE RESOLUCIÓN','\begin{center} \textbf{INFORME CON PROPUESTA DE RESOLUCIÓN} \end{center}');
  elsif instr(upper(vAux),'DILIGENCIA DE PERSONACIÓN')>0 then
    vAux:=replace(vAux,'DILIGENCIA DE PERSONACIÓN','\begin{center} \textbf{DILIGENCIA DE PERSONACIÓN} \end{center}');      
  elsif instr(upper(vAux),'INFORME REQUERIMIENTO DENUNCIA VOLUNTARIA')>0 then
    vAux:=replace(vAux,'INFORME REQUERIMIENTO DENUNCIA VOLUNTARIA','\begin{center} \textbf{INFORME REQUERIMIENTO DENUNCIA VOLUNTARIA} \end{center}');  
  elsif instr(upper(vAux),'INFORME REQUERIMIENTO PARA SUBSANACIÓN DE DENUNCIAS')>0 then
    vAux:=replace(vAux,'INFORME REQUERIMIENTO PARA SUBSANACIÓN DE DENUNCIAS','\begin{center} \textbf{INFORME REQUERIMIENTO PARA SUBSANACIÓN DE DENUNCIAS} \end{center}');      
  else
    vAux:=replace(vAux,'INFORME','\begin{center} \textbf{INFORME} \end{center}');
  end if;  
  -----------------------------

  for i in cFijo(vEsmid,'PIE') loop
      vAux:=vAux || '\vbox{' || i.res || '}'; --vbox para evitar separar el pie
      vAux:=replace(vAux,'@FechaDoc',i.fechaDoc);
      vAux:=replace(vAux,'PROPONE','\begin{center} \textbf{PROPONE} \end{center}');
  end loop;  
  
 
  vSalida:=replace(vSalida,'@contenido',vAux);
  
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
      TABLE(alba.trml.dmultas(m.mulid,e.esmid)) d
  WHERE e.mulid =m.mulid
  AND m.EXPIDEXP=ex.expid
  AND ex.expid=l.EXPID(+)
  AND EXISTS
      (SELECT 1 FROM alba.escritosmultasinfo ei WHERE ei.esmid=e.esmid AND ei.esmietiqueta='EXPTE. TEA')
  --AND EXISTS
  --    (SELECT 1 FROM alba.escritosmultas e2 WHERE e2.esmid =vEsmid 
  --    AND e.esmcaja=e2.esmcaja AND e.esmlote=e2.esmlote AND e.esmindice=e2.esmindice)
  and e.esmid=vEsmid -- para evitar resoluciones en grupo, es decir, la condición anterior
  ORDER BY l.LIQNUMERORECLIQUIDACION ASC
)
;


cursor cLiquidaciones(vEsmid number) is
/* 20171127 Se sustituye por nueva select que recoja liquidaciones de otras versiones de expte
select distinct
  ex.expcod EXPTE, l.LIQNUMERORECLIQUIDACION LIQUIDACION
from 
  alba.liquidaciones l, alba.expingre ex, alba.multas m, alba.escritosmultas e
where e.mulid=m.mulid and m.EXPIDEXP=ex.expid and ex.expid=l.EXPID
and e.esmid=vEsmid
--and exists
--  (select 1 from alba.escritosmultas e2 
--  where e2.esmid=vEsmid and e.esmcaja=e2.esmcaja 
--  and e.esmlote=e2.esmlote and e.esmindice=e2.esmindice)
and l.liqid in 
  (select max(l2.liqid) from liquidaciones l2 where l2.expid=l.expid)
order by l.LIQNUMERORECLIQUIDACION asc
*/
select distinct
  ex.expcod EXPTE, l.LIQNUMERORECLIQUIDACION LIQUIDACION
from 
  alba.liquidaciones l, alba.expingre ex
where ex.expid=l.EXPID
and l.expid in
	(select m2.expidexp from alba.multas m2 where m2.seoidexp in
    (
    select m.seoidexp 
    from alba.multas m, alba.escritosmultas e 
    where e.mulid=m.mulid
    and e.esmid=vEsmid
    )
  )
order by l.LIQNUMERORECLIQUIDACION asc
;


cursor cCabecera(vEsmid number) is
select 
replace(
replace(rsaresolucion,CHR(13),'\\'||CHR(10)||CHR(13))
,'"','``')
|| '\\'||CHR(10)||CHR(13)
res
from 
alba.resoluciones rs, alba.escritosmultas e, alba.resol_alegaciones r
WHERE r.resid=rs.resid
and r.ESMID=e.esmid
and rs.orden in ('CABECERA','PRUEBA CABECERA') and rownum<2
and e.esmid=vEsmid
/*and exists
(select 1 from alba.escritosmultas e2 
where e2.esmid=vEsmid and e.esmcaja=e2.esmcaja
and e.esmlote=e2.esmlote and e.esmindice=e2.esmindice)*/
;

cursor cDetalle(vEsmid number) is
select
replace(
replace(
replace(
replace(rsaresolucion,CHR(13),'\\'||CHR(10)||CHR(13)),
'@recibos',
alba.trml.parametro(e.esmid,r.resid,'recibos')),
'@recibo',
alba.trml.parametro(e.esmid,r.resid,'recibo')
),'"','``') || '\\'||CHR(10)||CHR(13)
res
from 
alba.resoluciones rs, alba.escritosmultas e, alba.resol_alegaciones r
WHERE r.resid=rs.resid and r.ESMID=e.esmid
and r.rsaresolucion is not null
and (rs.orden is null or rs.orden='PRUEBA DETALLE')
and e.esmid=vEsmid
/* and exists
(select 1 from alba.escritosmultas e2 
where e2.esmid=vEsmid and e.esmcaja=e2.esmcaja
and e.esmlote=e2.esmlote and e.esmindice=e2.esmindice) */
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
and e.esmid=vEsmid
/* and exists
(select 1 from alba.escritosmultas e2 
where e2.esmid=vEsmid and e.esmcaja=e2.esmcaja
and e.esmlote=e2.esmlote and e.esmindice=e2.esmindice) */
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
  
  \papersize{29.7cm}{21.0cm} %para tamaño carta, para otros eligieran la correcta
  \marginsize{2cm}{2cm}{3cm}{2cm} %\marginsize{Izque}{Derec}{Arrib}{Abajo}

  \setlength{\parindent}{0cm} % evita sangrado de párrafos
  
  \pagestyle{fancy}
  \renewcommand{\headrulewidth}{0pt} % grosor de la línea de la cabecera
  \renewcommand{\footrulewidth}{0pt} % grosor de la línea del pie
  
  
  \lhead{\begin{picture}(0,0) \put(0,0){\includegraphics[width=0.3\textwidth]{/var/www/html/tea.jpg}} \end{picture}}
  \chead{}
  \rhead{@exptea}
  \lfoot{}
  \cfoot{}
  \rfoot{pág. \thepage}
  
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
    Datos de la Reclamación
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
    \end{left}
    \end{table}
    ~';
    
    vAux:=replace(vAux,'@recurrente',nz2(i.recurrente));
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
  Expedientes de Tráfico
  \begin{table}[H]
  \begin{left}
  \begin{tabular}{|l l|}
  \hline
  EXPEDIENTE & \ LIQUIDACIÓN\\
  @liq_lista
  \hline
  \end{tabular}
  \end{left}
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
  
  select alba.trml.parametro(vEsmid,null,'expedientes') into vTemp from dual;
  vAux:=replace(vAux,'@expedientes',vTemp); vTemp:='';
  select alba.trml.parametro(vEsmid,null,'estimadas') into vTemp from dual;
  vAux:=replace(vAux,'@estimadas',vTemp); vTemp:='';  
  select alba.trml.parametro(vEsmid,null,'desestimadas') into vTemp from dual;
  vAux:=replace(vAux,'@desestimadas',vTemp); vTemp:='';  
  select alba.trml.parametro(vEsmid,null,'hoy') into vTemp from dual;
  vAux:=replace(vAux,'@hoy',vTemp); vTemp:='';  
  select alba.trml.parametro(vEsmid,null,'recurrente') into vTemp from dual;
  vAux:=replace(vAux,'@recurrente',nz2(vTemp)); vTemp:='';  
  select alba.trml.parametro(vEsmid,null,'domicilio') into vTemp from dual;
  vAux:=replace(vAux,'@domicilio',vTemp); vTemp:='';  
  select alba.trml.parametro(vEsmid,null,'pleno') into vTemp from dual;
  vAux:=replace(vAux,'@pleno',vTemp); vTemp:='';  
  select alba.trml.parametro(vEsmid,null,'expediente_tea') into vTemp from dual;
  vAux:=replace(vAux,'@expediente_tea',vTemp); vTemp:='';  
  select alba.trml.parametro(vEsmid,null,'asunto') into vTemp from dual;
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