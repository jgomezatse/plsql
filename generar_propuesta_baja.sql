create or replace PROCEDURE           "GENERAR_PROPUESTA_BAJA" (pLbrId NUMBER, pUsuario NUMBER, pResultado IN OUT varchar)
IS
--
CURSOR cLiquidaciones IS
select l.*
from 
liquidaciones l, 
det_libro_resol d, 
  (select decid 
    from decocobros d, 
    (select texto 
    from table(paq_notificaciones.split(
    (select parvalor from parametrosgeneralesapl where parcodigo = 'ESTADPROBAJA'),','))) valor
  where d.decelemento = replace(valor.texto, '''', '')) deco
where d.lbrid = pLbrId
and l.expid = d.expid 
and deco.decid = l.liqxestado
and d.liqid is null
and d.mulid is not null
union
select l.*
from 
liquidaciones l, 
det_libro_resol d, 
  (select decid 
    from decocobros d, 
    (select texto 
    from table(paq_notificaciones.split(
    (select parvalor from parametrosgeneralesapl where parcodigo = 'ESTADPROBAJA'),','))) valor
  where d.decelemento = replace(valor.texto, '''', '')) deco
where d.lbrid = pLbrId
and l.expid = d.expid 
and deco.decid = l.liqxestado
and d.liqid=l.liqid
and d.mulid is null
;
--
dL cLiquidaciones%ROWTYPE;
nPRBID NUMBER := 0;                     -- ID PROPUESTA DE BAJA
dFecha DATE;                            -- Fecha del Sistema
lUsuario number;                        -- Usuario que modifica
sMotivo H_LIQUIDACIONES.LIQMOTIVO%type; -- Descr. del motivo de la baja
sMotivoBaja varchar(100);
vCONT NUMBER := 0;                      -- Numero de Liquidaciones Propuestas para la Baja
lEstadoSB DECOCOBROS.decid%TYPE :=0;    -- ID Estado de Liquidacion SUSPENDIDO POR BAJA
lMotivo number := 0;
begin
    dFecha := SYSDATE;
    lUsuario := pUsuario;
    sMotivoBaja := 'Gener. autom. prop. baja por libro ' || to_char(pLbrId);
    sMotivo := '';
    -- Selecciona estado liquidacion = SUSPENDIDO POR BAJA (SB)
    SELECT DECID INTO lEstadoSB
        FROM DECOCOBROS
        WHERE DECCODIGOTABLA = (SELECT DECID FROM DECOCOBROS WHERE DECCODIGOTABLA = 0
        AND DECELEMENTO = 'ESTADOS LIQ')
        AND DECELEMENTO = 'SB';
    -- Selecciona motivo baja (MOTBE)
    SELECT DECID INTO lMotivo
        FROM DECOCOBROS
        WHERE DECCODIGOTABLA = (SELECT DECID FROM DECOCOBROS WHERE DECCODIGOTABLA = 0 AND DECELEMENTO = 'MOTIVOSBAJA')
        AND DECELEMENTO = 'MOTBE';
    SELECT PROPUESTASREPOBAJA_SQ.NEXTVAL INTO nPRBID FROM DUAL;
    -- INSERTA LA CABECERA DE LA PROPUESTA DE BAJA
    INSERT INTO PROPUESTASREPOBAJA
      (PRBID, PRBTIPO, PRBDESC, PRBFCREA, PRBXMOTIVORB, PRBFHORAMOD, USUIDMOD, 
      PRBMOTIVO, PRBFVIGENCIA, PRBCOMENTARIOS, USUID, PRBFAPRUEBA, PRBFCPPTA)
    VALUES
      (nPRBID, 'B', sMotivoBaja , dFecha, lMotivo, dFecha, lUsuario,
      sMotivoBaja, NULL, NULL, lUsuario, NULL, dFecha);
    sMotivo := 'INCLUIDO EN PROP. BAJA n? ' || to_CHAR(nPRBID);
    -- INICIA EL BUCLE QUE TRABAJARA CON EL DETALLE DE LAS LIQUIDACIONES
    FOR DL IN cLiquidaciones loop
        vCONT := vCONT + 1;
        -- INSERTA EL DETALLE DE LA PROPUESTA DE BAJA 
        INSERT INTO DETALLEPROPUESTASRB (dbrid, prbid, dbrxmotivorb, dbrfhoramod, usuidmod, dbrmotivo, dbrfvigencia, liqid)
        VALUES(DETALLEPROPUESTASRB_SQ.NEXTVAL,NPRBID,lMotivo,dFecha,lUsuario,sMotivo,NULL,DL.LIQID);
        -- INSERTA EL DETALLE DEL HISTORICO DE LA LIQUIDACION
        INSERT INTO H_LIQUIDACIONES
         (LIQID,EXPID,LIQFGENERACION,LIQNUMERORECLIQUIDACION,LIQDIGITOSCONPRE,LIQEJERECONDER,
          LIQREFERENCIACON,LIQXESTADO,LIQFESTADO,LIQORIGEN,LIQFPUESTACOBRO,LIQFINICIOPERIODOVOL,
          LIQFFINPERIODOVOL,LIQFVTOPAGOEJEC,LIQFAPROBACIONRECLIQ,LIQFNOTIFICA,LIQFPCOBRO,LIQCUENTABANPRE,
          LIQDIGITOSCONTROL,LIQFORMACOBREALIZADA,LIQFCOBRO,LIQIMPORTE,LIQIMPBR,LIQMONEDA,SUCID,RBAID,PLIID,
          LIQNUMLISTA,LIQFHORAMOD,USUIDMOD,LIQMOTIVO,LIQFVIGENCIA,LIQUSOHCO,LIQINDBAJA,LIQCAMPOSMOD,
          LIQFNOTIFICAEJE,LIQTEORVTOEJE,LIQDESCRLARGA,PCPID,LIQFEMICARTAPAGO,LIQFPETNOTIAPREMIO,
          LIQFCIERREAPREMIO,LIQFCIERREPERPAGO,LIQNUMERORECLIQSUFIJO,LIQPERIODO,LIQFINIPERIODOLIQ,
          LIQFFINPERIODOLIQ,LIQANUALRECIBO,LIQMOTANULA,LIQFENTRADADEU,LIQXESTADOAPFR,ORMID,LIQFSITUACION,
          LIQSITUACION,BANID,LIQINTERESES,LIQCOSTAS,LIQAPREMIO,LIQ_RBAID,LIQFDEVOL,LIQIMPBONIFICADOM)
        VALUES
         (DL.LIQID,DL.EXPID,DL.LIQFGENERACION,DL.LIQNUMERORECLIQUIDACION,DL.LIQDIGITOSCONPRE,DL.LIQEJERECONDER,
          DL.LIQREFERENCIACON,DL.LIQXESTADO,DL.LIQFESTADO,DL.LIQORIGEN,DL.LIQFPUESTACOBRO,DL.LIQFINICIOPERIODOVOL,
          DL.LIQFFINPERIODOVOL,DL.LIQFVTOPAGOEJEC,DL.LIQFAPROBACIONRECLIQ,DL.LIQFNOTIFICA,DL.LIQFPCOBRO,DL.LIQCUENTABANPRE,
          DL.LIQDIGITOSCONTROL,DL.LIQFORMACOBREALIZADA,DL.LIQFCOBRO,DL.LIQIMPORTE,DL.LIQIMPBR,DL.LIQMONEDA,DL.SUCID,DL.RBAID,DL.PLIID,
          DL.LIQNUMLISTA,DL.LIQFHORAMOD,DL.USUIDMOD,DL.LIQMOTIVO,DL.LIQFVIGENCIA,'N','N','LIQXESTADO;LIQFESTADO;LIQFHORAMOD;USUIDMOD;LIQMOTIVO;',
          DL.LIQFNOTIFICAEJE,DL.LIQTEORVTOEJE,DL.LIQDESCRLARGA,DL.PCPID,DL.LIQFEMICARTAPAGO,DL.LIQFPETNOTIAPREMIO,
          DL.LIQFCIERREAPREMIO,DL.LIQFCIERREPERPAGO,DL.LIQNUMERORECLIQSUFIJO,DL.LIQPERIODO,DL.LIQFINIPERIODOLIQ,
          DL.LIQFFINPERIODOLIQ,DL.LIQANUALRECIBO,DL.LIQMOTANULA,DL.LIQFENTRADADEU,DL.LIQXESTADOAPFR,DL.ORMID,DL.LIQFSITUACION,
          DL.LIQSITUACION,DL.BANID,DL.LIQINTERESES,DL.LIQCOSTAS,DL.LIQAPREMIO,DL.LIQ_RBAID,DL.LIQFDEVOL,DL.LIQIMPBONIFICADOM);
        -- ACTUALIZA EL ESTADO DE LAS LIQUIDACIONES A SUSPENDIDO POR BAJA
        UPDATE LIQUIDACIONES
           SET LIQXESTADO =  lEstadoSB,
               LIQFESTADO = dFecha,
               LIQFHORAMOD = dFecha,
               USUIDMOD = lUsuario,
               LIQMOTIVO = sMotivo
         WHERE LIQID = DL.LIQID;
         -- POR SI HAY QUE HACER ALGUN LEVANTAMIENTO DE EMBARGO
         albaadm.pkg_expedientesejecutiva.PETICIONES_LEVANTAMIENTOS(
                DL.LIQID, NULL, 'A', DL.LIQID, 'BAJA', DL.LIQIMPORTE,
                null, lUsuario);
         albaadm.pkg_expedientesejecutiva.PRC_ESTADO_LIQUIDACION(DL.LIQID);                
    end loop;
    IF VCONT > 0 THEN
        update libro_resoluciones set lbrnumcargo = nPRBID where lbrid = pLbrId;
        pResultado := 'Propuesta de baja numero: ' || nPRBID || '. Numero total de' ||
            ' liquidaciones incluidas en la propuesta: ' || TO_CHAR(VCONT);
        --DBMS_OUTPUT.PUT_LINE(pResultado);
        COMMIT;
    ELSE
        pResultado := 'ATENCION: El proceso no ha encontrado datos para generar propuesta de baja';
        --DBMS_OUTPUT.PUT_LINE(pResultado);
        ROLLBACK;
    END IF;
EXCEPTION
    WHEN OTHERS THEN
        pResultado := 'Error al intentar generar la propuesta de baja. Iteracion=' || TO_CHAR(VCONT) || ' --> ' || SQLERRM;
        --DBMS_OUTPUT.PUT_LINE(pResultado);
        ROLLBACK;
end;