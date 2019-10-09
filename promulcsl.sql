create or replace PROCEDURE         PROCSLMUL
   (pFechaDesde in varchar2, pFechaHasta in varchar2, pUsuario in number, pResultado in out varchar2)
IS
--
CURSOR cCobros(pFD varchar2, pFH varchar2) is
SELECT m.mulid, csl.cslimporte importe, e.expid, csl.cslid, csl.cslfcobro fcobro
FROM expingre e,
   liquidaciones l,
   personas pcsl,
   cobrossinliquidacion csl,
   multas m
WHERE pcsl.perid = csl.perid
AND csl.cslestado = 'P'
AND csl.cslcodexpediente=to_char(m.mulnumbol)
--AND NOT m.mulsitutrami IN ('E', 'U', 'S', 'L')
AND m.mulversion = (SELECT MAX (mulversion) FROM multas mm WHERE mm.seoidexp = m.seoidexp)
AND csl.cslfcobro >=TO_DATE (pFD || ' 00:00:00', 'DD/MM/YYYY HH24:MI:SS')
AND csl.cslfcobro <=TO_DATE (pFH || ' 23:59:59', 'DD/MM/YYYY HH24:MI:SS')
AND csl.mexid = 21
AND e.expid = m.expidexp
AND e.expid = l.expid(+)
union
SELECT m.mulid, csl.cslimporte importe, e.expid, csl.cslid, csl.cslfcobro fcobro
FROM expingre e,
   liquidaciones l,
   personas pcsl,
   cobrossinliquidacion csl,
   multas m
WHERE pcsl.perid = csl.perid
AND csl.cslestado = 'P'
AND csl.cslcodexpediente=to_char(m.boletin2)
--AND NOT m.mulsitutrami IN ('E', 'U', 'S', 'L')
AND m.mulversion = (SELECT MAX (mulversion) FROM multas mm WHERE mm.seoidexp = m.seoidexp)
AND csl.cslfcobro >=TO_DATE (pFD || ' 00:00:00', 'DD/MM/YYYY HH24:MI:SS')
AND csl.cslfcobro <=TO_DATE (pFH || ' 23:59:59', 'DD/MM/YYYY HH24:MI:SS')
AND csl.mexid = 21
AND e.expid = m.expidexp
AND e.expid = l.expid(+);
--
vUsuario number;
vFechaActual1 date;
vFechaActual2 date;
vFechaDesde date;
vFechaHasta date;
vResultado varchar2(200);
vCont number;
vNewPliid propuestaliquidaciones.pliid%type;
vFexid propuestaliquidaciones.fexid%type;
vOliid origenliquidacion.oliid%type;
vTliid tiposliquidaciones.tliid%type;
vNewLiqid liquidaciones.liqid%type;
vNewLiqnumerorecliquidacion liquidaciones.liqnumerorecliquidacion%type;
vEjercicio NUMBER := 0;
vNewRecibo NUMBER:=0;
vError varchar2(100):='';
vPrint boolean:=false;
--joanca 31/05/2006
vFechaCobro date;
--fin joanca 31/05/2006
--
BEGIN
   ----------------------------------------------------------------
   -- VALORES COMUNES
   ----------------------------------------------------------------
   vUsuario:=pUsuario;
   vFechaActual1:=sysdate;
   vFechaActual2:=trunc(sysdate);
   vFechaDesde:=to_date(pFechaDesde,'dd/mm/yyyy');
   vFechaHasta:=to_date(pFechaHasta,'dd/mm/yyyy');
     vResultado:='';
   vCont:=0;
   vEjercicio:=TO_NUMBER(TO_CHAR(vFechaActual1,'yyyy'));
   --
   select fexid into vFexid -- Forma de Exaccion: OTROS SIN CONTRAIDO PREVIO
   from formasexaccion where fexcodigo='OSCP';
   vError:=vError||'1.';
   --
   select oliid into vOliid
   from origenliquidacion where olidesc='UNICA';
   vError:=vError||'2.';
   --
     select tliid into vTliid
     from tiposliquidaciones where TLICODIGO='DEFF';
   vError:=vError||'3.';
   -----------------------------------------------------------------
   -- PRINCIPAL
   -----------------------------------------------------------------
   --
   for c in cCobros(pFechaDesde,pFechaHasta) loop
   
            -- Inicia Contador Nuevo Recibo
            /*select CGACONTADOR into vNewRecibo
            from CONTADORGENERALANUAL
            where CGAEJERCICIO = vEjercicio
            and CGATIPO = 'R';
            vNewRecibo:=vNewRecibo+1;
            vNewLiqnumerorecliquidacion:= TRIM(TO_CHAR(vEjercicio)) || SUBSTR(TO_CHAR(vNewRecibo,'09999999'),2,8);
            -- Actualiza Contador Liquidaciones
            update CONTADORGENERALANUAL
            set CGACONTADOR = vNewRecibo
            where CGAEJERCICIO = vEjercicio
            and CGATIPO = 'R';
            COMMIT;*/
             
            --22/02/2017 JGM. Modifico la obtención del número de liquidación por la función
            --                 genérica "Siguiente_Liquidacion"
            vNewLiqnumerorecliquidacion:= Siguiente_Liquidacion (  0, 0, 'R', vEjercicio);             
            COMMIT;
             
         -- GENERA NUEVA PROPUESTA DE LIQUIDACION
             select propuestaliquidaciones_sq.NEXTVAL into vNewPliid from dual;
       INSERT INTO propuestaliquidaciones p
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
           (vNewPliid, 'I', c.mulid, vFechaActual1,
           null, null, null,
           vFechaActual1, vFechaActual1, 'C', c.importe,
           c.importe, null, null, null,
           null, null, '0', null,
           'S', 'N', 'E', null,
           vFexid, c.expid, vtliid, vOliid, vFechaActual1, vUsuario,
           'Se ha creado', null, null,
           null, null,
           'Asoc.Aut. CSL-Boletin Multas', null,
           0, null, null, null,
           null, null, null, null,
           null, null);
       if vPrint=false then vError:=vError||'5.';
       end if;
       --
--joanca 31/05/2006
       vFechaCobro:=to_date(to_char(c.fcobro,'dd/mm/yyyy'),'dd/mm/yyyy');
--fin joanca 31/05/2006
       -- GENERA NUEVA LIQUIDACION
       select liquidaciones_sq.NEXTVAL into vNewLiqid from dual;
         INSERT INTO liquidaciones l
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
           l.liqfsegnotif, l.liqfternotif)
       VALUES
           (vNewLiqid, c.expid, vFechaActual1, vNewLiqnumerorecliquidacion,
           null, vEjercicio, null,
           13, vFechaActual1, 'G', vFechaActual1,
           vFechaActual1, null, null,
           vFechaActual1, vFechaActual1, 'N',
           null, 'O', null,
           vFechaCobro, c.importe, c.importe, 'E', null,
           null, vNewPliid, 0, vFechaActual1, vUsuario,
           'Se ha creado', null, null, null,
           null, null, null,
           null,
           'Asoc.Aut. CSL-Boletin Multas',
           null, null,
           null, null, null,
           0, 'V', null,
           null, null, null, null,
           null, null, null, null, null,
           null, null, null, null,
           null, null);
       if vPrint=false then vError:=vError||'6.';
       end if;
         --
         -- GENERA OTROS CONCEPTOS
       INSERT INTO otrosconceptos o
         (o.ocoid, o.ocoimporte, o.ocoestado, o.ocofcreacion, o.ocofcobro,
         o.ocoporcen, o.ocomoneda, o.ocoanularcobro, o.ocoobserva,
         o.liqid, o.ccoid, o.ocofhoramod, o.usuidmod, o.ocomotivo,
         o.ocofvigencia, o.ocoejerciciorecon, o.ocoimportecalc,
         o.ocoimporteapl)
       VALUES
         (otrosconceptos_sq.NEXTVAL, c.importe, 'GE', vFechaActual1, null,
         null, 'E', 'N', null,
         vNewLiqid, 15, vFechaActual1, vUsuario, 'Se ha creado',
         vFechaActual1, null, null,
         null);
       if vPrint=false then vError:=vError||'7.';
       end if;
       --
       INSERT INTO otrosconceptos o
         (o.ocoid, o.ocoimporte, o.ocoestado, o.ocofcreacion, o.ocofcobro,
         o.ocoporcen, o.ocomoneda, o.ocoanularcobro, o.ocoobserva,
         o.liqid, o.ccoid, o.ocofhoramod, o.usuidmod, o.ocomotivo,
         o.ocofvigencia, o.ocoejerciciorecon, o.ocoimportecalc,
         o.ocoimporteapl)
       VALUES
         (otrosconceptos_sq.NEXTVAL, c.importe, 'GE', vFechaActual1, null,
         null, 'E', 'N', null,
         vNewLiqid, 36, vFechaActual1, vUsuario, 'Se ha creado',
         vFechaActual1, null, null,
         null);
       if vPrint=false then vError:=vError||'8.';
       end if;
       --
       -- ACTUALIZA MULTAS
       update multas
        set multas.mulfeccob = vFechaCobro,
            multas.mulfhoramod = vFechaActual1,
            multas.usuidmod = vUsuario
        where multas.mulid = c.mulid
        and multas.mulfeccob is null;
       -- ACTUALIZA COBROS SIN LIQUIDACION
      update cobrossinliquidacion
        set cobrossinliquidacion.cslestado='A',
            cobrossinliquidacion.cslfhoramod=vFechaActual1,
            cobrossinliquidacion.usuidmod=vUsuario
        where cobrossinliquidacion.cslid=c.cslid
    and cobrossinliquidacion.cslestado<>'A';
    
    update diariooperaciones 
       set liqid = vNewLiqid 
     where cslid=c.cslid;
    
    
    --
    vCont:=vCont+1;
        vPrint:=true;
   end loop;
   --
   ----------------------------------------------------------------
   -- FINALIZACION DEL PROCESO
   ----------------------------------------------------------------
   IF vCont>0 THEN
       vResultado := 'Total cobros procesados correctamente: ' || TO_CHAR(vCont);
       COMMIT;
   ELSE
       vResultado := 'El proceso no ha encontrado datos';
       ROLLBACK;
   END IF;
   DBMS_OUTPUT.PUT_LINE(vResultado);
   pRESULTADO := vResultado;
   ----------------------------------------------------------------
   -- FINALIZACION DEL PROCESO CON EXCEPCION
   ----------------------------------------------------------------
   
END;