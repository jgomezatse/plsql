--------------------------------------------------------------------------------
create or replace package denuncias as
--------------------------------------------------------------------------------


      type registro is record
      (
      -- Denuncia
      fichero varchar2(200),
      orden number,
      salida varchar2(4000)
      );
      type salida is table of registro;
      
      function limpieza(vFicid number) return salida pipelined;
      
      procedure estado(vFicid number);
      
      function errores(vFicid number) return clob;
      
      procedure altaExpedientes(vFicid number);
      
      procedure cargarLimpieza;

end denuncias
;
/


--------------------------------------------------------------------------------
create or replace package body denuncias is
--------------------------------------------------------------------------------

function er(prefijo varchar2,entrada varchar2,expresion varchar2) return varchar2 is
begin
  if prefijo is null or prefijo='' then
    return fic.er(entrada,expresion);
  else
    return prefijo || '¬' || fic.er(entrada,expresion);
  end if;
end;

--------------------------------------------------------------------------------
function limpieza(vFicid number) return salida pipelined is
--------------------------------------------------------------------------------

  cursor cDatos (vFicid number) is
  select 
  rownum num,
  a.fichero fichero,
  fic.token(a.salida,1) boletin,
  fic.token(a.salida,2) añoboletin,
  fic.token(a.salida,3) FechaDenuncia,
  fic.token(a.salida,4) HoraDenuncia,
  fic.token(a.salida,5) CodigoVia,
  fic.token(a.salida,6) ViaLugar,
  fic.token(a.salida,7) Lugar,
  fic.token(a.salida,8) CodigoHechoDenunciado,
  fic.token(a.salida,9) DescripcionHD,
  fic.token(a.salida,10) ObservacionesHD,
  fic.token(a.salida,11) NIFDenunciado,
  fic.token(a.salida,12) NombreDenunciado,
  fic.token(a.salida,13) DomicilioDenunciado,
  fic.token(a.salida,14) CPDenunciado,
  fic.token(a.salida,15) LocalidadDenunciado,
  fic.token(a.salida,16) ProvinciaDenunciado,
  fic.token(a.salida,17) NIFDenunciante1,
  fic.token(a.salida,18) NombreDenunciante1,
  fic.token(a.salida,19) DomicilioDenunciante1,
  fic.token(a.salida,20) CPDenunciante1,
  fic.token(a.salida,21) LocalidadDenunciante1,
  fic.token(a.salida,22) ProvinciaDenunciante1,
  fic.token(a.salida,23) NIFDenunciante2,
  fic.token(a.salida,24) NombreDenunciante2,
  fic.token(a.salida,25) DomicilioDenunciante2,
  fic.token(a.salida,26) CPDenunciante2,
  fic.token(a.salida,27) LocalidadDenunciante2,
  fic.token(a.salida,28) ProvinciaDenunciante2,
  fic.token(a.salida,29) Ficheros
  from alba.ficheros f,
  table(fic.separa_filas(f.idg)) a
  where f.ficgrupo=3
  and f.ficid=vFicid    
  and fic.token(a.salida,1) is not null;
  
  
  cursor cInfraccion(vCodigoHechoDenunciado varchar2) is
  select i.infid, i.infdescripcion descripcion, i.infimporte importe
  from alba.infracciones i
  where i.infcodigo=vCodigoHechoDenunciado;
  
  cursor cVia(vCodigoVia varchar2) is  
  select viaid
  from alba.vias v
  where v.viacodigo=vCodigoVia;
  
  v registro;
  b boolean;
  temp varchar2(2000):='';
  

  
begin
 
  for i in cDatos(vFicid) loop
  
    v.fichero:='';
    v.salida:='';  
    v.orden:=null;
  
    v.fichero:=i.fichero;
    v.orden:=to_number(i.num);
    
    v.salida:= er(v.salida,i.boletin,'^[1-9][0-9]{3,11}$');
    v.salida:= er(v.salida,i.añoboletin,'^(201[7-9]|202[0-9])$');
    v.salida:= er(v.salida,i.FechaDenuncia,'^(201[7-9]|202[0-9])(0[1-9]|1[0-2])([0-2][0-9]|3[0-1])$');
    v.salida:= er(v.salida,i.HoraDenuncia,'^([0-1][0-9]|2[0-3])([0-5][0-9])$');
    --CódigoVia¬ViaLugar¬Lugar
    v.salida:= er(v.salida,i.CodigoVia||'¬'||i.ViaLugar||'¬'||i.Lugar,'^([0-9]{2,7}¬.*¬.*)$');
    --CodigoHechoDenunciado,DescripcionHD,ObservacionesHD
    v.salida:= er(v.salida,i.CodigoHechoDenunciado||'¬'||i.DescripcionHD||'¬'||i.ObservacionesHD,'^([1-9][0-9]{0,6}¬.{20,255}(¬|¬.{5,100}))$');
    
    --v.salida:= er(v.salida,i.NIFDenunciado,'^(.*)$');
    v.salida:= v.salida||'¬'||i.NIFDenunciado;
    v.salida:= er(v.salida,i.NombreDenunciado,'^(.+(|@.+|@.+@.+))$');
    v.salida:= er(v.salida,i.DomicilioDenunciado,'^(.{5,200})$');
    v.salida:= er(v.salida,i.CPDenunciado,'^[1-9][0-9]{2,5}$');
    v.salida:= er(v.salida,i.LocalidadDenunciado,'^[0-9A-Za-z ]{4,100}$');
    v.salida:= er(v.salida,i.ProvinciaDenunciado,'^[0-9A-Za-z ]{4,100}$');
    
    v.salida:= er(v.salida,i.NIFDenunciante1,'^([0-9A-Za-z]{5,12})$');
    v.salida:= er(v.salida,i.NombreDenunciante1,'^(.+(|@.+|@.+@.+))$');
    v.salida:= er(v.salida,i.DomicilioDenunciante1,'^(.{5,200})$');
    v.salida:= er(v.salida,i.CPDenunciante1,'^[1-9][0-9]{2,5}$');
    v.salida:= er(v.salida,i.LocalidadDenunciante1,'^[0-9A-Za-z ]{4,100}$');
    v.salida:= er(v.salida,i.ProvinciaDenunciante1,'^[0-9A-Za-z ]{4,100}$');
    
    v.salida:= er(v.salida,i.NIFDenunciante2 ||'¬'|| i.NombreDenunciante2 || '¬' ||
                           i.DomicilioDenunciante2 ||'¬'|| i.CPDenunciante2 || '¬' ||
                           i.LocalidadDenunciante2 ||'¬'|| i.ProvinciaDenunciante2,
                           --'^(([0-9A-Za-z]{5,12})¬(.+(|@.+|@.+@.+))¬(^.{5,200})¬([1-9][0-9]{2,5})¬([0-9A-Za-z ]{5,100})¬([0-9A-Za-z ]{5,100})|¬¬¬¬¬)$');
                           '.+¬.+¬.+¬.+¬.+¬|¬¬¬¬¬');
    
    /*v.salida:= er(v.salida,i.NIFDenunciante2,'^([0-9A-Za-z]{5,12})$');
    v.salida:= er(v.salida,i.NombreDenunciante2,'^(.+(|@.+|@.+@.+))$');
    v.salida:= er(v.salida,i.DomicilioDenunciante2,'^.{5,200}$');
    v.salida:= er(v.salida,i.CPDenunciante2,'^[1-9][0-9]{2,5}$');
    v.salida:= er(v.salida,i.LocalidadDenunciante2,'^[0-9A-Za-z ]{5,100}$');
    v.salida:= er(v.salida,i.ProvinciaDenunciante2,'^[0-9A-Za-z ]{5,100}$');*/
    --Ficheros
    v.salida:= er(v.salida,i.Ficheros,'((|[0-9A-Za-z_.]+@)*)$');
    
    --Infracción
    temp:='¬#No existe cód. de hecho denunciado¬#Error importe';
    for j in cInfraccion(i.CodigoHechoDenunciado) loop
      temp:= '¬' || j.infid || '¬' || j.importe;
    end loop;
    v.salida:=v.salida || temp;

    --Vía
    temp:='¬#No existe cód. de vía';
    for j in cVia(i.CodigoVia) loop
      temp:= '¬' || j.viaid;
    end loop;
    v.salida:=v.salida || temp;

  
    pipe row(v);
    
  end loop;
  
  return;
  
end limpieza;



--------------------------------------------------------------------------------
procedure estado(vFicid number) is
--------------------------------------------------------------------------------

cursor cfichero(vficid number) is
select a.*,
(select count(*) from alba.multas m where m.mulexpperm=to_char(vFicid)) expedientes
from table(denuncias.limpieza(vFicid)) a
;

vRegistros number:=0;
vExpedientes number:=0;
vIncidencias number:=0;

begin

  for i in cFichero(vFicid) loop
  
    vRegistros:=vRegistros+1;

    if instr(i.salida,'#')>0 then
      vIncidencias:=vIncidencias+1;
    end if;
    
    vExpedientes:=i.expedientes;
  
  end loop;
  
  update alba.ficheros f
  set f.ficmotivo=to_char(vRegistros) || '¬' || to_char(vIncidencias) || '¬' || to_char(vExpedientes)
  where f.ficid=vFicid;
  
  commit;
  
end;

--------------------------------------------------------------------------------
function errores(vFicid number) return clob is
--------------------------------------------------------------------------------

  cursor cDatos(vFicid number) is
  select 'Registro nº: '||a.orden||'¬'||a.salida salida
  from
  table(denuncias.limpieza(vFicid)) a
  where instr(a.salida,'#')>0;
        
  vClob clob;

begin

    
    vClob:=to_clob('');    
    for d in cDatos(vFicid) loop
    
        vClob:=vClob || to_clob(d.salida);
        -- Retorno de carro
        vClob:=vClob || to_clob(chr(13) || chr(10));

    end loop;
    
    return vClob;

end;

--------------------------------------------------------------------------------
procedure altaExpedientes(vficid number) is
--------------------------------------------------------------------------------

  cursor cExpediente(vId number) is
  select 
  a.fichero,
  a.orden,
  to_number(fic.token(a.salida,1)) boletin,
  to_number(fic.token(a.salida,2)) añoboletin,
  to_date(fic.token(a.salida,3),'yyyymmdd') FechaDenuncia,
  fic.token(a.salida,4) HoraDenuncia,
  fic.token(a.salida,5) CodigoVia,
  fic.token(a.salida,6) ViaLugar,
  fic.token(a.salida,7) Lugar,
  fic.token(a.salida,8) CodigoHechoDenunciado,
  fic.token(a.salida,9) DescripcionHD,
  fic.token(a.salida,10) ObservacionesHD,
  fic.token(a.salida,11) NIFDenunciado,
  fic.token(a.salida,12) NombreDenunciado,
  fic.token(a.salida,13) DomicilioDenunciado,
  fic.token(a.salida,14) CPDenunciado,
  fic.token(a.salida,15) LocalidadDenunciado,
  fic.token(a.salida,16) ProvinciaDenunciado,
  fic.token(a.salida,17) NIFDenunciante1,
  fic.token(a.salida,18) NombreDenunciante1,
  fic.token(a.salida,19) DomicilioDenunciante1,
  fic.token(a.salida,20) CPDenunciante1,
  fic.token(a.salida,21) LocalidadDenunciante1,
  fic.token(a.salida,22) ProvinciaDenunciante1,
  fic.token(a.salida,23) NIFDenunciante2,
  fic.token(a.salida,24) NombreDenunciante2,
  fic.token(a.salida,25) DomicilioDenunciante2,
  fic.token(a.salida,26) CPDenunciante2,
  fic.token(a.salida,27) LocalidadDenunciante2,
  fic.token(a.salida,28) ProvinciaDenunciante2,
  fic.token(a.salida,29) Ficheros,
  to_number(fic.token(a.salida,30)) infid,
  to_number(fic.token(a.salida,31)) importe,
  to_number(fic.token(a.salida,32)) viaid,
  to_number(substr(fic.token(a.salida,4),1,2)) hh,
  to_number(substr(fic.token(a.salida,4),3,2)) mm,
  alba.expingre_sq.nextval expid,
  alba.seobjetos_sq.nextval seoid,
  alba.multas_sq.nextval mulid,
  0 mulversion,
  800 mulxboletin,
  to_number(to_char(sysdate,'YYYY')) año,
  sysdate fecha, 21 exaccion, 0 usuid,
  'Alta Expediente' motivo
  from
  table(denuncias.limpieza(vId)) a
  where instr(a.salida,'#')=0
  and not exists (select 1 from alba.multas where mulexpperm=to_char(vId) 
             and mulnumbol=to_number(fic.token(a.salida,1)))  
  ;
  
  cursor cPruebas(vEntrada varchar2) is
  select 
  a.valor fichero,
  'file://agenciarec/fotosradar$/MULTA-CAR/Doc_Limpieza/'||a.valor ruta,
  to_char(sysdate,'yyyy-mm-dd') fecha
  from
  (select regexp_substr(vEntrada,'[^@]+', 1, level) valor from dual
  connect by regexp_substr(vEntrada,'[^@]+', 1, level) is not null) a;
  
  cursor cEstado(vid number) is
  select ficid
  from (
  select f.ficid,f.fictotaldetalle,
  fic.token(f.ficmotivo,1)+0 registros,
  fic.token(f.ficmotivo,2)+0 incidencias,
  fic.token(f.ficmotivo,3)+0 expedientes
  from alba.ficheros f  
  where f.ficid=vid
  and f.ficgrupo=3
  and f.ficmotivo<>'Carga inicial'
  and fic.token(f.ficmotivo,1) is not null
  and fic.token(f.ficmotivo,2) is not null
  and fic.token(f.ficmotivo,3) is not null)
  where fictotaldetalle=registros
  and incidencias=0
  and expedientes<registros;  
  
  
  vExpediente number;
  vRdiid number;
  vPerid number;
  vPerversion number;
  vConid number;
  vNombre varchar2(100):='';
  vApe1 varchar2(100):='';
  vApe2 varchar2(100):='';
  veicid number:='';
  
  vCargarExpedientes boolean:=false;


begin

  denuncias.estado(vFicid);
  
  vcargarexpedientes:=false;
  for j in cestado(vFicid) loop
   vCargarExpedientes:=true;
  end loop;
  
  if vCargarExpedientes then
  
    for i in cExpediente(vFicid) loop
    
      vExpediente:=null; 
    
      alba.prcnuevocodigoexpingre (i.exaccion,i.año,i.usuid,i.motivo,vExpediente);
      commit;
      
      insert into alba.expingre (expid,expcod,expfalta,expfdecl,mexid,expfhoramod,usuidmod,expmotivo) 
      values (i.expid,vExpediente,i.fecha,i.fecha,i.exaccion,i.fecha,i.usuid,i.motivo);
      
      insert into alba.seobjetos (seoid,expid,seofhoramod,usuidmod,seomotivo)
      values (i.seoid,i.expid,i.fecha,i.usuid,i.motivo);
      
      insert into alba.multas 
      (mulid,infid,mulsituinfra,multipobolet,mulsitutrami,mulnotestado,
      viaid,mullugar,mulanobol,mulxboletin,mulnumbol,mulversion,
      seoidexp,expidexp,mulfec,
      mulhor,mulmin,
      mulimp,muldenuncia,mulnotmano,mulxmotivo,
      mulobsdir,mulobservaciones,
      mulfecgrab,mulfeciniprod,mulfdesde,
      mulfhoramod,usuidmod,mulmotivo,mulexpperm)
      values
      (i.mulid,i.infid,'D','N','N','P',
      i.viaid,i.vialugar || ' ' || i.lugar,i.añoboletin,i.mulxboletin,i.boletin,i.mulversion,
      i.seoid,i.expid,i.FechaDenuncia,
      i.hh,i.mm,
      i.importe,i.DescripcionHD,'N',190,
      null, i.ObservacionesHD,
      i.fecha,i.FechaDenuncia,i.fecha,        
      i.fecha,i.usuid,i.motivo,vFicid);
      
      for p in cPruebas(i.Ficheros) loop
      
        insert into depuracion.indexmul
        (fichero,ruta,fecha,id1)
        values
        (p.fichero,p.ruta,p.fecha,i.mulid);
      
      end loop;
      
      -------------
      -- DENUNCIADO
      -------------
      vRdiid:=0; vPerid:=0; vPerversion:=0; vConid:=0; vNombre:=null; vApe1:=null; vApe2:=null; vEicid:=null;
      
      if i.NIFDenunciado is not null and i.NombreDenunciado is not null 
      and i.DomicilioDenunciado is not null and i.CPDenunciado is not null
      and i.LocalidadDenunciado is not null and i.ProvinciaDenunciado is not null
      then
      
          select 
          fic.token(i.NombreDenunciado,1,'@'),
          fic.token(i.NombreDenunciado,2,'@'),
          fic.token(i.NombreDenunciado,3,'@') 
          into vNombre,vApe1,vApe2
          from dual;
      
          alba.paq_multas.Alta_Persona_Direccion ('S','S',0,0,
          i.NIFDenunciado,vNombre,vApe1,vApe2,
          i.exaccion,9309, i.fecha, i.motivo, i.usuid,
          i.ProvinciaDenunciado, i.LocalidadDenunciado, i.CPDenunciado, i.DomicilioDenunciado, 
          9309,vRdiid,vPerid,vPerversion,vConid); 
          
          select alba.expedientesingresoscontr_sq.NEXTVAL into vEicid from dual;
      
          insert into alba.expedientesingresoscontr
          (eicid,conid,expid,eicxtipocontribuyente,eicfhoramod,usuidmod,eicmotivo)
          values
          (vEicid,vConid,i.expid,29729,i.fecha,i.usuid,i.motivo); --29729 Denunciado
                
          insert into alba.direccionesexpedientes
          (tedid,rdiid,eicid,fiaid,tedxtipodireccion,tedfhoramod,usuidmod,tedmotivo,tedfvigencia,tedorigen)
          values
          (alba.direccionesexpedientes_sq.NEXTVAL,vRdiid,vEicid,5,283,
          i.fecha,i.usuid,i.motivo,null,null  );
          
          dbms_output.put_line('Expte: ' || vExpediente || ' Expid: ' || i.expid 
          || ' Mulid: ' || i.mulid || ' Perid: ' || vPerid || ' Rdiid: ' || vRdiid 
          || ' Conid: ' || vConid || ' Eicid: ' || vEicid);        
          
      end if;    
      
      if i.NIFDenunciado is null and i.NombreDenunciado is not null and vPerid=0
      then
          update alba.multas m
          set m.mulobservaciones=m.mulobservaciones || decode(m.mulobservaciones,null,'','. ') || 'Denunciado: ' || replace(i.NombreDenunciado,'@',' ')
          where m.mulid=i.mulid;
      end if;
      
      if i.NIFDenunciado is null and vPerid=0
      and i.DomicilioDenunciado is not null and i.CPDenunciado is not null
      and i.LocalidadDenunciado is not null and i.ProvinciaDenunciado is not null
      then
          update alba.multas m
          set m.mulobservaciones=m.mulobservaciones || decode(m.mulobservaciones,null,'','. ') || 'Domicilio: ' || i.DomicilioDenunciado 
          || ' CP: ' || i.CPDenunciado || ' ' || i.LocalidadDenunciado || ' ' || i.ProvinciaDenunciado
          where m.mulid=i.mulid;    
      end if;
      
      
      ----------------
      -- DENUNCIANTE 1
      ----------------
      vRdiid:=0; vPerid:=0; vPerversion:=0; vConid:=0; vNombre:=null; vApe1:=null; vApe2:=null; vEicid:=null;
      
      if i.NIFDenunciante1 is not null and i.NombreDenunciante1 is not null 
      and i.DomicilioDenunciante1 is not null and i.CPDenunciante1 is not null
      and i.LocalidadDenunciante1 is not null and i.ProvinciaDenunciante1 is not null
      then
      
          select 
          fic.token(i.NombreDenunciante1,1,'@'),
          fic.token(i.NombreDenunciante1,2,'@'),
          fic.token(i.NombreDenunciante1,3,'@') 
          into vNombre,vApe1,vApe2
          from dual;
      
          alba.paq_multas.Alta_Persona_Direccion ('S','N',0,0,
          i.NIFDenunciante1,vNombre,vApe1,vApe2,
          i.exaccion,9309, i.fecha, i.motivo, i.usuid,
          i.ProvinciaDenunciante1, i.LocalidadDenunciante1, i.CPDenunciante1, i.DomicilioDenunciante1, 
          9309,vRdiid,vPerid,vPerversion,vConid); 
          
          select alba.expedientesingresoscontr_sq.NEXTVAL into vEicid from dual;
      
          insert into alba.expedientesingresoscontr
          (eicid,conid,expid,eicxtipocontribuyente,eicfhoramod,usuidmod,eicmotivo)
          values
          (vEicid,vConid,i.expid,329,i.fecha,i.usuid,i.motivo); --329,8189 Denunciante1,denunciante2
                
          /*insert into alba.direccionesexpedientes
          (tedid,rdiid,eicid,fiaid,tedxtipodireccion,tedfhoramod,usuidmod,tedmotivo,tedfvigencia,tedorigen)
          values
          (alba.direccionesexpedientes_sq.NEXTVAL,vRdiid,vEicid,5,283,
          i.fecha,i.usuid,i.motivo,null,null  );  */
          
          dbms_output.put_line('Expte: ' || vExpediente || ' Expid: ' || i.expid 
          || ' Mulid: ' || i.mulid || ' Perid: ' || vPerid || ' Rdiid: ' || vRdiid 
          || ' Conid: ' || vConid || ' Eicid: ' || vEicid);        
          
      end if;    
      
      
      ----------------
      -- DENUNCIANTE 2
      ----------------
      vRdiid:=0; vPerid:=0; vPerversion:=0; vConid:=0; vNombre:=null; vApe1:=null; vApe2:=null; vEicid:=null;
      
      if i.NIFDenunciante2 is not null and i.NombreDenunciante2 is not null 
      and i.DomicilioDenunciante2 is not null and i.CPDenunciante2 is not null
      and i.LocalidadDenunciante2 is not null and i.ProvinciaDenunciante2 is not null
      then
      
          select 
          fic.token(i.NombreDenunciante2,1,'@'),
          fic.token(i.NombreDenunciante2,2,'@'),
          fic.token(i.NombreDenunciante2,3,'@') 
          into vNombre,vApe1,vApe2
          from dual;
      
          alba.paq_multas.Alta_Persona_Direccion ('S','N',0,0,
          i.NIFDenunciante2,vNombre,vApe1,vApe2,
          i.exaccion,9309, i.fecha, i.motivo, i.usuid,
          i.ProvinciaDenunciante2, i.LocalidadDenunciante2, i.CPDenunciante2, i.DomicilioDenunciante2, 
          9309,vRdiid,vPerid,vPerversion,vConid); 
          
          select alba.expedientesingresoscontr_sq.NEXTVAL into vEicid from dual;
      
          insert into alba.expedientesingresoscontr
          (eicid,conid,expid,eicxtipocontribuyente,eicfhoramod,usuidmod,eicmotivo)
          values
          (vEicid,vConid,i.expid,8189,i.fecha,i.usuid,i.motivo); --329,8189 Denunciante2,denunciante2
                
          /*insert into alba.direccionesexpedientes
          (tedid,rdiid,eicid,fiaid,tedxtipodireccion,tedfhoramod,usuidmod,tedmotivo,tedfvigencia,tedorigen)
          values
          (alba.direccionesexpedientes_sq.NEXTVAL,vRdiid,vEicid,5,283,
          i.fecha,i.usuid,i.motivo,null,null  );
          */
          
          dbms_output.put_line('Expte: ' || vExpediente || ' Expid: ' || i.expid 
          || ' Mulid: ' || i.mulid || ' Perid: ' || vPerid || ' Rdiid: ' || vRdiid 
          || ' Conid: ' || vConid || ' Eicid: ' || vEicid);        
          
      end if;     
      
      commit;  
      
    end loop;
    
  end if;
  
  denuncias.estado(vFicid);  
    
exception
    when others then
       dbms_output.put_line('KO. ' || SQLERRM);
end;


--------------------------------------------------------------------------------
procedure cargarlimpieza is
--------------------------------------------------------------------------------

  cursor cFicheros is
  select ficid,fictotaldetalle
  from (
  select f.ficid,f.fictotaldetalle,
  fic.token(f.ficmotivo,1)+0 registros,
  fic.token(f.ficmotivo,2)+0 incidencias,
  fic.token(f.ficmotivo,3)+0 expedientes
  from alba.ficheros f
  where f.ficgrupo=3
  and f.ficmotivo<>'Carga inicial'
  and fic.token(f.ficmotivo,1) is not null
  and fic.token(f.ficmotivo,2) is not null
  and fic.token(f.ficmotivo,3) is not null
  )
  where fictotaldetalle=registros
  and incidencias=0
  and expedientes=0
  union
  select f2.ficid,f2.fictotaldetalle
  from alba.ficheros f2
  where f2.ficgrupo=3
  and f2.ficmotivo='Carga inicial'
  ;

begin

  for i in cficheros loop
    denuncias.altaexpedientes(i.ficid);
  end loop;
end;

--------------------------------------------------------------------------------
end denuncias
--------------------------------------------------------------------------------
;








