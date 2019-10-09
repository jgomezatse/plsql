create or replace PROCEDURE crear_cargo(pLbrId in number, pUsuario in number, pResultado in out varchar2)
IS
-- Array con las direcciones a tratar
type TipoDirec is table of number index by binary_integer;
aTipoDirec TipoDirec;
-- Variables
vOrganismoOrigen number;
vOrganismoDestino number;
vFechaActual1 date;
vFechaActual2 date;
vEjercicio number;
vMotivo varchar2(30);
vNewRecibo NUMBER := 0;
vCont number := 0;
vIedtotal number := 0;
vTotal number := 0;
vEicid alba.expedientesingresoscontr.eicid%type;
vTipoDirNotif alba.decodificadora.decid%type;
vTipoDirObjeto alba.decodificadora.decid%type;
vConId alba.contribuyentes.conid%type;
vEstadoLiquidacion alba.liquidaciones.liqxestado%type;
vTipoSujetoPasivo alba.decodificadora.decid%type;
vNewLiqnumerorecliquidacion alba.liquidaciones.liqnumerorecliquidacion%type;
vNewPliid alba.propuestaliquidaciones.pliid%type;
vPyfid alba.pliegosfacturas.pyfid%type;                  -- ID del cargo que se crea
vCcfsecuencial alba.contadorcargofra.ccfsecuencial%type;
vPyfnumcargo alba.pliegosfacturas.pyfnumcargo%type;
vNewLiqid alba.liquidaciones.liqid%type;
vMexid alba.expingre.mexid%type;
vLiqejereconder alba.liquidaciones.liqejereconder%type;  -- Ejercicio Cargo
vFexid alba.formasexaccion.fexid%type;                   -- Forma de Exaccion
vOliid alba.origenliquidacion.oliid%type;                -- Origen liquidacion
vPyfxestado alba.decocobros.decid%type;                  -- Estado del cargo
vCcoidPrincipal alba.conceptoscontables.ccoid%type;               -- Concepto contable
vCcoidBruto alba.conceptoscontables.ccoid%type;               -- Concepto contable
vTliid alba.tiposliquidaciones.tliid%type;               -- Tipo liquidaciones
vOut varchar2(1000);
-- Cursores
cursor cDatos is
    select d.perid as sujetopasivo, d.perversion as sujetopasivoversion, d.rdiid as direccion, e.fnotsan,
           e.ffirmeza, e.finivol, e.ffinvol, t.*
    from alba.libro_resoluciones l, alba.libro_resol_tipos tipo, alba.det_libro_resol d,
	table(trml.dmultas(d.mulid, 0)) t, table(trml.emultas(d.mulid)) e
    where d.lbrid = pLbrId
    and d.mulid=e.mulid
    and l.lbrid = d.lbrid
    and l.lrtid = tipo.lrtid
    and l.lbrnumcargo is null
    and tipo.lrtestadopropnotif = e.cestado;
cursor cSujetoPasivo (lExpid alba.expingre.expid%type, lEicid number) is
    select e.eicid
    from alba.expedientesingresoscontr e, alba.expingre
    where e.expid = lExpid
    and expingre.expid = e.expid
    and expingre.mexid = vMexid
    and e.eicxtipocontribuyente = vTipoSujetoPasivo
    and e.eicid <> lEicid;
--
-- Pool contadores
vTotalRegistros number;
type tabla is table of varchar2(14) index by binary_integer;
vPoolRecibos tabla;
vContadorCursor number:=0;
--
BEGIN
    vFechaActual1 := sysdate;
    vFechaActual2 := to_date(sysdate,'dd/mm/yyyy');	
    vEjercicio := TO_NUMBER(TO_CHAR(vFechaActual1,'yyyy'));
    vLiqejereconder := vEjercicio;
    -- Organismo: AGENCIA
    vOrganismoDestino := 2;
    -- Organismo: AYUNTAMIENTO
    vOrganismoOrigen := 3;
    -- Exaccion: MULTAS
    select mexid into vMexid from alba.exacciones where mexcodigo = 'MULTAS';
    -- Forma de Exaccion: CONTRAIDO PREVIO INGRESO DIRECTO
    select fexid into vFexid from alba.formasexaccion where fexcodigo = 'CPID';
    -- Origen liquidacion: EJECUTIVA MULTAS
    select oliid into vOliid from alba.origenliquidacion where olicod='EJML';
    -- Estado del cargo: CERRADO
    select decid into vPyfxestado from alba.decocobros where decelemento='CE' and deccodigotabla=91;
    -- Conceptos contables: IMPORTE PRINCIPAL, IMPORTE BRUTO
    select ccoid into vCcoidPrincipal from alba.conceptoscontables where ccotextocorto='PRINCIPAL';
    select ccoid into vCcoidBruto from alba.conceptoscontables where ccotextocorto='IMPORTEBR';
    -- Tipo liquidaciones: DEFINITIVA
	select tliid into vTliid from alba.tiposliquidaciones where TLICODIGO='DEFF';
	-- Estado de la liquidacion: NOTIFICADO
	select decid into vEstadoLiquidacion from alba.decocobros where deccodigotabla = 1 and decdescr = 'NOTIFICADO' order by decid;
	-- Tipo de contribuyente: SUJETOPASIVO
	select decid into vTipoSujetoPasivo from alba.decodificadora d where d.deccodigotabla = 68 and decelemento = 'SUJETOPASIVO';
	-- Tipo de direccion: NOTIFICACION
	select decid into vTipoDirNotif from alba.decodificadora where deccodigotabla = 65 and decelemento = 'NOTIFICACION';
	-- Tipo de direccion: OBJETO
    select decid into vTipoDirObjeto from alba.decodificadora where deccodigotabla = 65 and decelemento = 'OBJETO';
    aTipoDirec(1) := vTipoDirNotif;
    aTipoDirec(2) := vTipoDirObjeto;
	/*********************************************/
    /* Comienza el proceso de creacion del cargo */
	/*********************************************/
	--dbms_output.put_line('Comienza el proceso de creacion del cargo');
	-- Obtiene ID del cargo que se crea
    select alba.pliegosfacturas_SQ.nextval into vPyfid from DUAL;   -- Obtiene ID del cargo que se crea
    -- Obtiene el numero del cargo que se crea
    select ccfsecuencial into vCcfsecuencial
    from alba.contadorcargofra
    where ccftipo='P'
    and ccfperiodo='V'
    and ccejercicio=vEjercicio;
    vCcfsecuencial := vCcfsecuencial + 1;
    --vPyfnumcargo := TO_CHAR(vLiqejereconder) || LPAD(vCcfsecuencial,8,'0');
    vPyfnumcargo := TO_CHAR(vLiqejereconder) || LPAD(vCcfsecuencial,7,'0') || '0';
    -- Actualizacion del contador de cargos
    UPDATE alba.contadorcargofra
    SET ccfsecuencial = vCcfsecuencial,
    ccffhoramod = vFechaActual1,
    usuidmod = pUsuario
    WHERE ccfid= (select ccfid from alba.contadorcargofra
    where ccftipo = 'P'
    and ccfperiodo = 'V'
    and ccejercicio = vLiqejereconder);
    vMotivo := 'Liquidacion/sancion firme';
    /*********************************************/
    /* Creacion de la cabecera del cargo         */
    /*********************************************/
    --dbms_output.put_line('Creacion de la cabecera del cargo');
    INSERT INTO alba.pliegosfacturas
        (pyfid, pyftipodocu, pyfsubtipo, pyffcrea, pyfxestado,
        pyffdocu, pyfperiodo, pyfperfdesde, pyfperfhasta,
        pyfnoapremiable, pyfmotivoanula, pyffhoramod, usuidmod,
        pyfmotivo, pyffvigencia, pyfnumcargo, ormid, org_ormid,
        pyfindtran, pyfmigra, pyfdoccontable, pyforigen,
        pyffvalida, pyfdocanulado, prbid, pyffregenera)
    VALUES
        (vPyfid,'P','P',vFechaActual1,vPyfxestado, --PP.Pliego de Cargo
        vFechaActual1,'V',null,null, --V.Voluntaria
        'N',null,vFechaActual1,pUsuario,
        'Se ha creado',null,vPyfnumcargo,vOrganismoDestino,vOrganismoOrigen,
        null,null,null,null,
        null,null,null,null);
--
--
    -- Obtencion de pool de recibos
    select count(distinct d.dlbid) into vTotalRegistros
    from alba.det_libro_resol d where d.lbrid=pLbrid;
    --
    if vTotalRegistros is null then
        vTotalRegistros:=0;
    end if;
    if vTotalRegistros >0 then
    	FOR j IN 1..vTotalRegistros LOOP
        /*
        -- Inicializar contador nuevo recibo
        --dbms_output.put_line('Inicia contador nuevo recibo');
        select CGACONTADOR into vNewRecibo from alba.CONTADORGENERALANUAL where CGAEJERCICIO = vEjercicio and CGATIPO = 'R';
        vNewRecibo := vNewRecibo + 1;
        -- Actualizacion del contador de liquidaciones
        --dbms_output.put_line('Actualizacion del contador de liquidaciones');
        update alba.CONTADORGENERALANUAL
        set CGACONTADOR = vNewRecibo
        where CGAEJERCICIO = vEjercicio
        and CGATIPO = 'R';
        */
    		vPoolRecibos(j):=Siguiente_Liquidacion (0,0,'R',vEjercicio);
    	END LOOP;
	end if;
	commit;
--
--
    /*********************************************/
    /* Creacion del detalle del cargo            */
    /*********************************************/
    commit;
   -- dbms_output.put_line('Creacion del detalle del cargo');
    vContadorCursor:=0;
    for h in cDatos loop
        --Ampliacion Motivo Liquidacion
        if h.matricula is not null and h.boletin is not null then
            vMotivo:=substr(vMotivo || ' ' || h.matricula || ' ' || h.boletin,1,30);
        end if;
        --dbms_output.put_line('Iteracion: ' || to_char(vCont));
        vCont := vCont + 1;
        vIedtotal := vIedtotal + h.importe;
        vIedtotal := ROUND(vIedtotal,2);
        --
          -----------------------------------------
          -- Obtencion del recibo mediante pool
          vContadorCursor:=vContadorCursor+1;
          /*vNewRecibo:=vPoolRecibos(vContadorCursor);
          vNewLiqnumerorecliquidacion := TRIM(TO_CHAR(vEjercicio)) || SUBSTR(TO_CHAR(vNewRecibo,'09999999'),2,8);*/
          vNewLiqnumerorecliquidacion:=vPoolRecibos(vContadorCursor);
          -----------------------------------------
        --
    	-- Hallar ConId (no se haya dentro de cDatos porque retrasa mucho la consulta)
		--dbms_output.put_line('Obtecion ConId');
		select max(c.conid) into vConId
        from alba.contribuyentes c
        where c.perid = h.sujetopasivo
        and c.perversion = h.sujetopasivoversion
        and c.mexid = vMexid
        and c.confvigencia is null;
		-- Obtener nuevo eicid
		--dbms_output.put_line('Obtebcion EicId');
        select alba.expedientesingresoscontr_sq.NEXTVAL into vEicid from dual;
        -- Insertar el nuevo Sujeto Pasivo
        --dbms_output.put_line('Inserta nuevo Sujeto Pasivo');
        insert into alba.expedientesingresoscontr (eicid, repid, conid, expid, eicxtipocontribuyente, eicporc,
        eicobli, eicnotif, eicfhoramod, usuidmod, eicmotivo, eicfvigencia, eicxderechoprev, eicxtipotitular)
        values (vEicid, null, vConId, h.expediente, vTipoSujetoPasivo, null, 'S', null, vFechaActual1, pUsuario,
        vMotivo, null, null, null);
        --commit;
        -- Eliminar todas las entradas del antiguo eicid de la tabla expedientesingresoscontr y direccionesexpedientes
        --dbms_output.put_line('Elimina ocurrencias antiguo eicId de ExpedientesIngresosContr y DireccionesExpedientes');
        for x in cSujetoPasivo(h.expediente, vEicid) loop
            delete from alba.direccionesexpedientes where eicid = x.eicid;
            delete from alba.expedientesingresoscontr where eicid = x.eicid;
        end loop;
        -- Insertar las nuevas direcciones (objeto, notificacion)
        --dbms_output.put_line('Inserta nuevas direcciones (objeto, notificacion)');
        for i in aTipoDirec.first..aTipoDirec.last loop
            insert into alba.direccionesexpedientes (tedid, rdiid, eicid, fiaid, tedxtipodireccion, tedfhoramod,
            usuidmod, tedmotivo, tedfvigencia)
            values (alba.direccionesexpedientes_sq.NEXTVAL, h.direccion, vEicid, 4, aTipoDirec(i), vFechaActual1,
            pUsuario, vMotivo, null);
        end loop;
	    -- Creacion nueva propuesta de liquidacion
	    --dbms_output.put_line('Crea nueva propuesta liquidacion');
        select alba.propuestaliquidaciones_sq.NEXTVAL into vNewPliid from dual;
        INSERT INTO alba.propuestaliquidaciones p
            (p.pliid, p.plitipo, p.pliobjetoliq, p.plifinivigorobj,
            p.plisubobjetoliq, p.plifiniperliq, p.pliffinperliq,
            p.plifpropuesta, p.plifaprobacionprop, p.plicuadre, p.pliimp,
            p.pliimpbr, p.pliimpbonif, p.pliimpexen, p.pliimpdes,
            p.pliimpsanins, p.pliimpotros, p.plianulacion, p.pliindrecalculo,
            p.pligenrecibo, p.pliimppactado, p.plimoneda, p.pro_pliid,
            p.fexid, p.expid, p.tliid, p.oliid, p.plifhoramod, p.usuidmod,
            p.plimotivo, p.plifvigencia, p.pliimprecargoprov,
            p.pliimprecargo613, p.pliimpdemora613,
            p.plidescr, p.crkey2,
            p.pliimporteiva, p.pliobserva, p.perid, p.perversion,
            p.rdiid_obj, p.rdiid_not, p.plifiniliq, p.pliffinliq,
            p.plifinidemora613, p.pliffindemora613)
        VALUES
            (vNewPliid, 'I', h.mulid, vFechaActual1,
            null, null, null,
            vFechaActual1, vFechaActual1, 'C', h.importe,
            h.importe, null, null, null,
            null, null, '0', null,
            'S', 'N', 'E', null,
            vFexid, h.expediente, vtliid, vOliid, vFechaActual1, pUsuario,
            'Se ha creado', null, null,
            null, null,
            vMotivo, null,
            0, null, h.sujetopasivo, h.sujetopasivoversion,
            h.direccion, h.direccion, null, null,
            null, null);
        -- Generacion de nueva liquidacion
        --dbms_output.put_line('Crea nueva liquidacion');
        select alba.liquidaciones_sq.NEXTVAL into vNewLiqid from dual;
        INSERT INTO alba.liquidaciones l
            (l.liqid, l.expid, l.liqfgeneracion, l.liqnumerorecliquidacion,
            l.liqdigitosconpre, l.liqejereconder, l.liqreferenciacon,
            l.liqxestado, l.liqfestado, l.liqorigen, l.liqfpuestacobro,
            l.liqfinicioperiodovol, l.liqffinperiodovol, l.liqfvtopagoejec,
            l.liqfaprobacionrecliq, l.liqfnotifica, l.liqfpcobro,
            l.liqcuentabanpre, l.liqdigitoscontrol, l.liqformacobrealizada,
            l.liqfcobro, l.liqimporte, l.liqimpbr, l.liqmoneda, l.sucid,
            l.rbaid, l.pliid, l.liqnumlista, l.liqfhoramod, l.usuidmod,
            l.liqmotivo, l.liqfvigencia, l.liqanualrecibo, l.liqmotanula,
            l.liqfentradadeu, l.liqxestadoapfr, l.liqfnotificaeje,
            l.liqteorvtoeje,
            l.liqdescrlarga,
            l.pcpid, l.liqfemicartapago,
            l.liqfpetnotiapremio, l.liqfcierreapremio, l.liqfcierreperpago,
            l.liqnumerorecliqsufijo, l.liqperiodo, l.liqfiniperiodoliq,
            l.liqffinperiodoliq, l.ormid, l.liqsituacion, l.liqfsituacion,
            l.banid, l.liqintereses, l.liqcostas, l.liqapremio, l.liq_rbaid,
            l.liqfdevol, l.liqinddev, l.liqimpbonificadom, l.liqnotvoldev,
            l.liqfsegnotif, l.liqfternotif, l.liqfsancionfirme)
       VALUES
            (vNewLiqid, h.expediente, vFechaActual1, vNewLiqnumerorecliquidacion,
            null, vEjercicio, null,
            vEstadoLiquidacion, vFechaActual1, 'G', vFechaActual1,
            h.finivol, h.ffinvol, null,
            vFechaActual1, h.fnotsan, 'N',
            null, 'O', null,
            null, h.importe, h.importe, 'E', null,
            null, vNewPliid, 0, vFechaActual1, pUsuario,
            'Se ha creado', null, null, null,
            null, null, null,
            null,
            vMotivo,
            null, null,
            null, null, null,
            0, 'V', null,
            null, null, null, null,
            null, null, null, null, null,
            null, null, null, null,
            null, null, h.ffirmeza);
        -- Generacion de otros conceptos
        --dbms_output.put_line('Crea nueva ocurrencia otros conceptos');
        INSERT INTO alba.otrosconceptos o
            (o.ocoid, o.ocoimporte, o.ocoestado, o.ocofcreacion, o.ocofcobro,
            o.ocoporcen, o.ocomoneda, o.ocoanularcobro, o.ocoobserva,
            o.liqid, o.ccoid, o.ocofhoramod, o.usuidmod, o.ocomotivo,
            o.ocofvigencia, o.ocoejerciciorecon, o.ocoimportecalc,
            o.ocoimporteapl)
        VALUES
            (alba.otrosconceptos_sq.NEXTVAL, h.importe, 'GE', vFechaActual1, null,
            null, 'E', 'N', null,
            vNewLiqid, vCcoidPrincipal, vFechaActual1, pUsuario, 'Se ha creado',
            vFechaActual1, null, null,
            null);
        INSERT INTO alba.otrosconceptos o
            (o.ocoid, o.ocoimporte, o.ocoestado, o.ocofcreacion, o.ocofcobro,
            o.ocoporcen, o.ocomoneda, o.ocoanularcobro, o.ocoobserva,
            o.liqid, o.ccoid, o.ocofhoramod, o.usuidmod, o.ocomotivo,
            o.ocofvigencia, o.ocoejerciciorecon, o.ocoimportecalc,
            o.ocoimporteapl)
        VALUES
            (alba.otrosconceptos_sq.NEXTVAL, h.importe, 'GE', vFechaActual1, null,
            null, 'E', 'N', null,
            vNewLiqid, vCcoidBruto, vFechaActual1, pUsuario, 'Se ha creado',
            vFechaActual1, null, null,
            null);
        -- Generacion del detalle del cargo (solo para el concepto contable PRINCIPAL)
        --dbms_output.put_line('Generacion del detalle del cargo');
        INSERT INTO alba.detallepliegosfras d
            (d.dpfid, d.pyfid, d.dpffhoramod, d.usuidmod, d.dpfmotivo,
            d.dpffvigencia, d.liqid, d.dioid, d.ccoid, d.dpfimporte, d.mexid,
            d.dpfejerciciorecon, d.dpfindaf, d.fexid)
        VALUES
            (alba.detallepliegosfras_sq.nextval, vPyfid, vFechaActual1, pUsuario, 'Se ha creado',
            null, vNewLiqid, null, vCcoidPrincipal, h.importe, vMexid,
            vLiqejereconder, 'N', vFexid);
    end loop;
    vIedtotal:=ROUND(vIedtotal,2);
    /******************************************************************************/
    /* Creacion del resumen del cargo  (solo para el concepto contable PRINCIPAL) */
    /******************************************************************************/
    --dbms_output.put_line('Generacion del resumen del cargo');
    INSERT INTO alba.importesexacciondocumen
        (iedid, pyfid, mexid, iedtotal, iedapremio,
        iedintedemo, iedcostas, iedfhoramod, usuidmod,
        iedmotivo, ccoid, iedejerciciorecon, iedindaf, fexid, iednumvalores)
    VALUES
        (alba.importesexacciondocumen_sq.nextval, vPyfid, vMexid, vIedtotal, null,
        null, null, vFechaActual1, pUsuario,
        vMotivo, vCcoidPrincipal, vLiqejereconder, 'N', vFexid, vCont);
    if vCont>0 then
        -- Actualizacion de los datos del libro, es decir, registro del numero de cargo asociado
        update alba.libro_resoluciones
        set lbrnumcargo = vPyfnumcargo
        where lbrid = pLbrId;
	    COMMIT;
	    depuracion.prccargarviasnuevas(0, vOut);
	    depuracion.prccargardirpadronmul('21', 0, vOut, vPyfnumcargo);
	    depuracion.prccargadiraeatmul('21', 0, vOut, vPyfnumcargo);
	    pResultado := 'Operacion finalizada correctamente. Se ha creado el cargo ' || vPyfnumcargo;
	    DBMS_OUTPUT.PUT_LINE(pResultado);
	else
		ROLLBACK;		
	    pResultado := 'Crear_Cargo. Error: No se encontraron datos';
	    DBMS_OUTPUT.PUT_LINE(pResultado);
	end if;
 EXCEPTION
    WHEN OTHERS THEN
         pResultado := 'Iteracion: ' || to_char(vCont) ||'. Crear_Cargo. Error: ' || SQLERRM;
         DBMS_OUTPUT.PUT_LINE(pResultado);
         ROLLBACK;
END;
 