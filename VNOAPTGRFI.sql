
  CREATE OR REPLACE FORCE VIEW "ALBAADM"."VNOAPTGRFI" ("NTFID","CERTIFICADO_TOTCER", "FECHA_EMISION", "CRMNUMERO", "F_VTO_PAGO", "EXPEDIENTE", "FINICIOVOL", "FFINVOL", "EMISORA", "REFERENCIA", "IDENTIFICACION", "IMPORTE_DEU", "IMPORTE", "APREMIO", "INTERES", "COSTAS", "ACUENTA", "TOTSUMA", "MEXDESCRIP", "FINICIOPER", "FFINPER", "LIQUIDACION", "SUJETOPASIVO", "DNI_SUJETOPASIVO", "DIR_NOTIF", "MUN_NOTIF", "PROV_NOTIF", "COD_POSTAL_NOTIF", "CODIGO_EXP", "LINEA1", "LINEA2", "LINEA3", "LINEA4", "LINEA5", "LINEA6", "LINEA7", "LINEA71", "LINEA8", "LIQNUMERORECLIQUIDACION", "PERIODOLIQUIDADO", "CODIGO_BARRAS", "CODIGO_BARRAS2", "NUMERO_VALOR", "EJERCICIO", "CODIGO_BARRAS3") AS 
  SELECT
             --VISTA RECIBOS TASAS GENERICAS SIN MIGRAR EXPEDIENTES DE INGRESO FICHERO
             --CABECERA
             ntf.ntfid,
             ntf.ntfcertificado
          || '/'
          || ntf.ntftotcertificados certificado_totcer,
          ntf.ntffecemision fecha_emision, crm.crmnumero crmnumero,
          liquida.liqteorvtoeje f_vto_pago, liquida.liqid expediente,
          
          --el valor expediente ser+ el valor por el que se acceda al dato
          --es decir, PNT.PNTORIGENID, que para este caso es LIQID
          liquida.liqfinicioperiodovol finiciovol,
          liquida.liqffinperiodovol ffinvol,
             albaadm.corta_etiqueta (cadena, 'PROVINCIA')
          || albaadm.corta_etiqueta (cadena, 'MUNICIPIO')
          || albaadm.corta_etiqueta (cadena, 'DCEMISORA') emisora,
             albaadm.corta_etiqueta (cadena, 'REFERENCIA')
          || albaadm.corta_etiqueta (cadena, 'REFERENCIADC') referencia,
             '1'
          || albaadm.corta_etiqueta (cadena, 'TRIBUTO')
          || albaadm.corta_etiqueta (cadena, 'EJERCICIO')
          || albaadm.corta_etiqueta (cadena, 'ULTIMODIGITOVENCIMIENTO')
          || albaadm.corta_etiqueta (cadena, 'FECHAJULIANAVENCIMIENTO')
                                                               identificacion,
          
-- nuestro
---------- BEGIN JAOD 25/11/2004
----------      (nvl(LIQUIDA.LIQIMPORTE,0)+ nvl(LIQUIDA.LIQAPREMIO,0)+nvl(LIQUIDA.LIQINTERESES,0)+nvl(decode(greatest(liquida.liqffinperiodovol,to_date('30/06/2004','DD/MM/YYYY')),liquida.liqffinperiodovol,0,LIQUIDA.LIQCOSTAS),0)) || ' ' || chr(164) IMPORTE_DEU,
--------       (nvl(LIQUIDA.LIQIMPORTE,0)+ nvl(LIQUIDA.LIQAPREMIO,0)+nvl(LIQUIDA.LIQINTERESES,0)+nvl(LIQUIDA.LIQCOSTAS,0)) || ' ' || chr(164) IMPORTE_DEU,
----------  END  JAOD 25/11/2004
--------      'CUOTA TRIBUTARIA:' || LPAD(LTRIM(to_char(nvl(LIQUIDA.LIQIMPORTE,0),'9999990D90')),8,' ') IMPORTE,
--------       decode(greatest(liquida.liqffinperiodovol,to_date('30/06/2004','DD/MM/YYYY')),liquida.liqffinperiodovol,'RECARGO  10%....:','RECARGO  20%....:') || LPAD(LTRIM(to_char(nvl(LIQUIDA.LIQAPREMIO,0),'9999990D90')),8,' ') APREMIO,
--------       'INTERESES.......:' || LPAD(LTRIM(to_char(nvl(LIQUIDA.LIQINTERESES,0),'9999990D90')),8,' ') INTERES,
---------- BEGIN JAOD 25/11/2004
----------       'COSTAS..........:' || LPAD(LTRIM(to_char(decode(greatest(liquida.liqffinperiodovol,to_date('30/06/2004','DD/MM/YYYY')),liquida.liqffinperiodovol,0,LIQUIDA.LIQCOSTAS),'9999990D90')),8,' ') COSTAS,
----------       'SUMA............:' || LPAD(LTRIM(to_char((nvl(LIQUIDA.LIQIMPORTE,0)+ nvl(LIQUIDA.LIQAPREMIO,0)+nvl(LIQUIDA.LIQINTERESES,0)+nvl(decode( greatest (liquida.liqffinperiodovol,to_date('30/06/2004','DD/MM/YYYY')),liquida.liqffinperiodovol,0,LIQUIDA.LIQCOSTAS),0)) ,'9999990D90')),8,' ') TOTSUMA,
--------       'COSTAS..........:' || LPAD(LTRIM(to_char(LIQUIDA.LIQCOSTAS,'9999990D90')),8,' ') COSTAS,
--------       'SUMA............:' || LPAD(LTRIM(to_char((nvl(LIQUIDA.LIQIMPORTE,0)+
--------                                                  nvl(LIQUIDA.LIQAPREMIO,0)+
--------                                                  nvl(LIQUIDA.LIQINTERESES,0)+
--------                                                  nvl(LIQUIDA.LIQCOSTAS,0)) ,'9999990D90')),8,' ') TOTSUMA,
----------  END  JAOD 25/11/2004
----(1) Importe principal: liquidaciones.liqimporte (origen tabla)
----(2) Recargo 10%: 10% calculado sobre sobre liquidaciones.liqimporte (origen calculado) - el valor de APREMIO COBRADO devuelto por la funcion pl calculoimportes.ctd (origen funcion)
----(3) Intereses: 0 (valor fijo)
----(4) Costas: liquidaciones.liqcostas (origen tabla)
----(5) Ingresos a cuenta: el valor CANTIDAD A CUENTA COBRADA devuelto por la funcion pl calculoimportes.ctd (origen funcion)
-------------------------------------------------------------------------------------
----(6) Importe Total: (1)+(2)+(3)+(4)-(5) (origen calculado)
-- AGR, Obtiene el importe de la deuda.
          (SELECT    ROUND
                        ((  NVL (liquida.liqimporte, 0)       -- Importe Ppal.
                          + (ROUND
                                (DECODE
                                    (GREATEST (liquida.liqffinperiodovol,
                                               TO_DATE ('30/06/2004',
                                                        'DD/MM/YYYY'
                                                       )
                                              ),
                                     liquida.liqffinperiodovol, ROUND
                                                   (  NVL (t.dprincipal -t.dprincipalco,
                                                           0
                                                          )
                                                    * 0.1,
                                                    2
                                                   ),     -- Si aplica el 10%.
                                     ROUND (NVL (t.dprincipal -t.dprincipalco, 0) * 0.2,
                                            2
                                           )
                                    )     -- Si aplica el 20% y cierra decode.
                                     ,
                                 2
                                )
                            )
-- Cierro Round y al apremio obtenido del decode, le resto el apremio cobrado.
                          + ROUND
                               (DECODE
                                   (GREATEST (liquida.liqffinperiodovol,
                                              TO_DATE ('30/06/2004',
                                                       'DD/MM/YYYY'
                                                      )
                                             ),
                                    liquida.liqffinperiodovol, 0
                                                                -- Devuelve 0.
                                                                ,
                                    t.dintereses + t.dinteresesco --para los intereses cobrados en A/F anulados
                                   )
                               -- Devuelve los intereses desde la funcion CTD.
                                    ,
                                2
                               )                                    -- INTERES
                          + NVL (liquida.liqcostas, 0)              -- Costas.
                          - t.dprincipalco
                         )                                  -- Ingresos a Cta.
                          ,
                         2
                        )
                  || ' '
                  || CHR (164) importe_deu
                -- Cierra Round y concatena el resto del formato.
             FROM dual) importe_deu,
             
             -- AGR, Fin Obtiene el importe de la deuda.
             -- AGR, Obtencion de los importes de la deuda en Ejecutiva.
             'IMPORTE PRINCIPAL:'
          || LPAD (LTRIM (TO_CHAR (NVL (liquida.liqimporte, 0), '9999990D90')),
                   8,
                   ' '
                  ) importe,
          (SELECT DECODE
                     (GREATEST (liquida.liqffinperiodovol,
                                TO_DATE ('30/06/2004', 'DD/MM/YYYY')
                               ),
                      liquida.liqffinperiodovol, 'RECARGO  10%....:'
                       || LPAD
                             (LTRIM
                                  (TO_CHAR (ROUND (  NVL (t.dprincipal -t.dprincipalco,
                                                          0
                                                         )
                                                   * 0.1,
                                                   2
                                                  ),
                                            '9999990D90'
                                           )
                                  ),
                              8,
                              ' '
                             ),
                         'RECARGO  20%....:'
                      || LPAD
                            (LTRIM
                                  (TO_CHAR (ROUND (  NVL (t.dprincipal -t.dprincipalco,
                                                          0
                                                         )
                                                   * 0.2,
                                                   2
                                                  ),
                                            '9999990D90'
                                           )
                                  ),
                             8,
                             ' '
                            )
                     ) apremio
             FROM dual) apremio,
          
          --INTERESES
          (SELECT    'INTERESES.......:'
                  || LPAD
                        (LTRIM
                            (TO_CHAR
                                (ROUND
                                    (DECODE
                                        (GREATEST (liquida.liqffinperiodovol,
                                                   TO_DATE ('30/06/2004',
                                                            'DD/MM/YYYY'
                                                           )
                                                  ),
                                         liquida.liqffinperiodovol, 0
                                                                -- Devuelve 0.
                                                                     ,
                                         t.dintereses + t.dinteresesco --para los intereses cobrados en A/F anulados
                                        )
                               -- Devuelve los intereses desde la funcion CTD.
                                         ,
                                     2
                                    ),
                                 '9999990D90'
                                )
                            ),
                         8,
                         ' '
                        )
             FROM dual) interes,
             'COSTAS..........:'
          || LPAD (LTRIM (TO_CHAR (NVL (liquida.liqcostas, 0), '9999990D90')),
                   8,
                   ' '
                  ) costas,
          (SELECT    'INGRESOS A CUENTA:'
                  || LPAD (LTRIM (TO_CHAR (t.dprincipalco, '9999990D90')), 8, ' ')
                                                             acuenta
             FROM dual) acuenta,
          (SELECT    'SUMA............:'
                  || LPAD
                        (LTRIM
                            (TO_CHAR
                                (ROUND
                                    (                           -- Abre Round.
                                     (  NVL
                                           (liquida.liqimporte, 0)
                                                            -- Importe Ppal. .
                                      + (ROUND
                                            (DECODE
                                                (GREATEST
                                                    (liquida.liqffinperiodovol,
                                                     TO_DATE ('30/06/2004',
                                                              'DD/MM/YYYY'
                                                             )
                                                    ),
                                                 liquida.liqffinperiodovol, ROUND
                                                    (  NVL
                                                          (t.dprincipal -t.dprincipalco,
                                                           0
                                                          )
                                                     * 0.1,
                                                     2
                                                    ),    -- Si aplica el 10%.
                                                 ROUND
                                                    (  NVL
                                                          (t.dprincipal -t.dprincipalco,
                                                           0
                                                          )
                                                     * 0.2,
                                                     2
                                                    )
                                                )
                                          -- Si aplica el 20% y cierra decode.
                                                 ,
                                             2
                                            )
                                        )
-- Cierro Round y al apremio obtenido del decode, le resto el apremio cobrado.
                                      + ROUND
                                           (DECODE
                                               (GREATEST
                                                   (liquida.liqffinperiodovol,
                                                    TO_DATE ('30/06/2004',
                                                             'DD/MM/YYYY'
                                                            )
                                                   ),
                                                liquida.liqffinperiodovol, 0
                                                                -- Devuelve 0.
                                                                            ,
                                                t.dintereses + t.dinteresesco --para los intereses cobrados en A/F anulados
                                               )
                               -- Devuelve los intereses desde la funcion CTD.
                                                ,
                                            2
                                           )                        -- INTERES
                                      + NVL (liquida.liqcostas, 0)  -- Costas.
                                      - t.dprincipalco
                                     )                      -- Ingresos a cta.
                                      ,
                                     2
                                    ),                        -- Cierra Round.
                                 '9999990D90'
                                )
                            ),
                         8,
                         ' '
                        ) totsuma        -- Da formato y cierra LPAD.
             FROM dual) totsuma,
             
             -- AGR, Fin obtencion de los importes de la deuda en Ejecutiva.
             --nuestro
             --      LIQUIDA.LIQIMPORTE IMPORTE,
             'CONCEPTO IMPOSITIVO:    '
          || mex.mexdesc
          || albaadm.descripcion_iae (EXP.expid) mexdescrip,
          
          -- PAra la +ltima notificaci?n antes de tener la gesti?n en Alba
          DECODE (liquida.liqreferenciacon,
                  NULL, pli.plifiniperliq,
                  NULL
                 ) finicioper,
          DECODE (liquida.liqreferenciacon,
                  NULL, pli.pliffinperliq,
                  NULL
                 ) ffinper,
             
             --        pli.plifiniperliq FInicioPer,
             --      pli.pliffinperliq FFinPer,
             'LIQUIDACION:    '
          || REPLACE (liq.liqdescrlarga, CHR (0), '') liquidacion,
          
          -- Sujeto Pasivo
          albaadm.nombre_exp (EXP.expid, 'SUJETOPASIVO') sujetopasivo,
          albaadm.dni_exp (EXP.expid, 'SUJETOPASIVO') dni_sujetopasivo,
          
          -- Dir INDICADA AL PEDIR NOTIFICACION
          REPLACE (alba.direccion_rdi (ntf.rdiid), CHR (0), '') dir_notif,
          albaadm.municipio_rdi_sincp (ntf.rdiid) mun_notif,
          albaadm.provincia_rdi (ntf.rdiid) prov_notif,
          NVL (albaadm.codigopostal_rdi (ntf.rdiid),
               '00000') cod_postal_notif,
          
          -- Expediente
          'EXPEDIENTE:' || EXP.expcod codigo_exp,
          
          --     los datos que siguen ser+n modificados cuando se gestione el impuetos
          --     en Alba y se migren los datos de los expedientes
          --       pli.pliobserva DESGLOSE,
          -- joanca 19/02/2008: Si es MULORD se trae el Codigo de Expediente antiguo.
          --          '    ' linea1,
          DECODE
             (EXP.mexid,
              (SELECT mexid
                 FROM alba.exacciones
                WHERE mexcodigo = 'MULORD'), (SELECT    'EXPEDIENTE DE ORIGEN    : '
                                                     || SUBSTR (dbj.dbjreg,
                                                                30,
                                                                15
                                                               )
                                                FROM albaadm.detobjeto dbj,
                                                     albaadm.seobjetos seo
                                               WHERE dbj.seoid = seo.seoid
                                                 AND seo.expid = EXP.expid
                                                 AND dbjfhasta IS NULL),
-- AGR, a?adido porque para los expedientes de 'sanciones de ordenanza' (12275519,12282782), devuelve mas de una ocurrencia.),
              '    '
             ) linea1,
          
             -- Fin joanca 19/02/2008
             DECODE (dac.daccodigo,
                     'REDEUDOR', 'NUMERO RELACION DEUD.: ',
                     'NUMERO CERTIFICACION.:'
                    )
          || aej.aejcodigo linea2,
             'FECHA FIN VOLUNTARIA    : '
          || TO_CHAR (liq.liqffinperiodovol, 'dd/mm/yyyy') linea3,
             'SUJETO PASIVO           : '
          || albaadm.nombre_exp (EXP.expid, 'SUJETOPASIVO') linea4,
             'DNI SUJETO PASIVO       : '
          || albaadm.dni_exp (EXP.expid, 'SUJETOPASIVO') linea5,
             
             --          'OBJETO TRIBUTARIO       :' || replace(albaadm.DIRECCION_EXP(EXP.EXPID, 'OBJETO', 'SUJETOPASIVO'), chr(0), '') LINEA6,
             -- agr 09/2017 Modifico el Objeto tributario. Los expediente de sancion tienen que buscar el expediente de gestión de ingresos.
             'OBJETO TRIBUTARIO       :'
          || REPLACE
                (albaadm.direccion_exp
                    (DECODE
                        (EXP.mexid,
                         223,
-- 223 es la exacción de los exp. de sancion. En estos casos debe buscar en el exp de G.Ingresos.
                         (SELECT mai.expid
                            FROM alba.liquidaciones l,
                                 alba.seobjetos sanc,
                                 alba.expsancion exs,
                                 alba.maestroactosinsp mai
                           WHERE l.liqid = liquida.liqid
                             AND l.expid = sanc.expid
                             AND sanc.seoid = exs.seoid
                             AND exs.exiid = mai.exiid
                             AND mai.atiid = 168
                             AND NVL (mai.maiestadoinsp, '¬') <> 'A'
                             AND ROWNUM = 1),
                         EXP.expid
                        ),
                     'OBJETO',
                     'SUJETOPASIVO'
                    ),
                 CHR (0),
                 ''
                ) linea6,
          
          -- fin 09/2017 agr
          -- joanca 19/02/2008: Si es MULORD se trae el Codigo de Expediente antiguo.
          --          ' ' LINEA7,
          DECODE
             (EXP.mexid,
              (SELECT mexid
                 FROM albaadm.exacciones
                WHERE mexcodigo = 'MULORD'), (SELECT    'OBSERVACIONES           : '
                                                     || SUBSTR (dbj.dbjreg,
                                                                599,
                                                                160
                                                               )
                                                FROM albaadm.detobjeto dbj,
                                                     albaadm.seobjetos seo
                                               WHERE dbj.seoid = seo.seoid
                                                 AND seo.expid = EXP.expid
                                                 AND dbjfhasta IS NULL),
-- AGR, a?adido porque para los expedientes de 'sanciones de ordenanza' (12275519,12282782), devuelve mas de una ocurrencia.
              (SELECT albaadm.obtener_objeto_tributario(liquida.liqid, 'N', 'S') FROM DUAL) -- G7184
             ) linea7,
          
          --Gesta 9270 (Inicio). Campo Linea71, añadido 18/12/2017, recuperado de copia de agosto de 2017 enviada por el ITAS bajo petición de este gesta
          DECODE
             (EXP.mexid,
              (SELECT mexid
                 FROM albaadm.exacciones
                WHERE mexcodigo = 'MULORD'), (SELECT SUBSTR (dbj.dbjreg,
                                                             665,
                                                             70
                                                            )
                                                FROM albaadm.detobjeto dbj,
                                                     albaadm.seobjetos seo
                                               WHERE dbj.seoid = seo.seoid
                                                 AND seo.expid = EXP.expid
                                                 AND dbjfhasta IS NULL),
-- AGR, a?adido porque para los expedientes de 'sanciones de ordenanza' (12275519,12282782), devuelve mas de una ocurrencia.
              ' '
             ) linea71,
          
          ----Gesta 927(Fin).
          DECODE
             (EXP.mexid,
              (SELECT mexid
                 FROM albaadm.exacciones
                WHERE mexcodigo = 'MULORD'), (SELECT    'DESCRIPCION             : '
                                                     || SUBSTR (dbj.dbjreg,
                                                                779,
                                                                80
                                                               )
                                                FROM albaadm.detobjeto dbj,
                                                     albaadm.seobjetos seo
                                               WHERE dbj.seoid = seo.seoid
                                                 AND seo.expid = EXP.expid
                                                 AND dbjfhasta IS NULL),
-- AGR, a?adido porque para los expedientes de 'sanciones de ordenanza' (12275519,12282782), devuelve mas de una ocurrencia.
              ' '
             ) linea8,
          
          -- Fin joanca 19/02/2008
          liq.liqnumerorecliquidacion,
             'PERIODO LIQUIDADO: DEL '
          || DECODE (liquida.liqreferenciacon,
                     NULL, TO_CHAR (liquida.liqfiniperiodoliq, 'DD/MM/YYYY'),
                     ' '
                    )
          || ' AL '
          || DECODE (liquida.liqreferenciacon,
                     NULL, TO_CHAR (liquida.liqffinperiodoliq, 'DD/MM/YYYY'),
                     ' '
                    ) periodoliquidado,
          
          --       'PERIODO LIQUIDADO: DEL '||TO_CHAR(LIQUIDA.LIQFINIPERIODOLIQ,'DD/MM/YYYY') ||
          --       ' AL '|| TO_CHAR( LIQUIDA.LIQFFINPERIODOLIQ,'DD/MM/YYYY') PeriodoLiquidado,
          --componer el c?digo de barras, el primero es fijo 90
          --el siguiente valor depende del formato
          (   '90521'
           || albaadm.corta_etiqueta (cadena, 'PROVINCIA')
           || albaadm.corta_etiqueta (cadena, 'MUNICIPIO')
           || albaadm.corta_etiqueta (cadena, 'DCEMISORA')
           || albaadm.corta_etiqueta (cadena, 'REFERENCIA')
           || SUBSTR (albaadm.corta_etiqueta (cadena, 'REFERENCIADC'), 2, 2)
           || '1'
           || albaadm.corta_etiqueta (cadena, 'TRIBUTO')
           || albaadm.corta_etiqueta (cadena, 'EJERCICIO')
           || albaadm.corta_etiqueta (cadena, 'ULTIMODIGITOVENCIMIENTO')
           || albaadm.corta_etiqueta (cadena, 'FECHAJULIANAVENCIMIENTO')
           ||
              --        lpad(to_char((nvl(LIQUIDA.LIQIMPORTE,0)+
              --                      nvl(LIQUIDA.LIQAPREMIO,0)+
              --                      nvl(LIQUIDA.LIQINTERESES,0)+
              --                      nvl(LIQUIDA.LIQCOSTAS,0)                      )*100),8,'0'))       CODIGO_BARRAS
              LPAD
                 (TO_CHAR
                     (  (SELECT ROUND
                                   ((  NVL (liquida.liqimporte, 0)
                                     + (ROUND
                                           (DECODE
                                               (GREATEST
                                                   (liquida.liqffinperiodovol,
                                                    TO_DATE ('30/06/2004',
                                                             'DD/MM/YYYY'
                                                            )
                                                   ),
                                                liquida.liqffinperiodovol, ROUND
                                                   (  NVL (t.dprincipal -t.dprincipalco,
                                                           0
                                                          )
                                                    * 0.1,
                                                    2
                                                   ),     -- Si aplica el 10%.
                                                ROUND
                                                   (  NVL (t.dprincipal -t.dprincipalco,
                                                           0
                                                          )
                                                    * 0.2,
                                                    2
                                                   )
                                               )
                                          -- Si aplica el 20% y cierra decode.
                                                ,
                                            2
                                           )
                                       )
-- Cierro Round y al apremio obtenido del decode, le resto el apremio cobrado.
                                     + ROUND
                                          (DECODE
                                              (GREATEST
                                                   (liquida.liqffinperiodovol,
                                                    TO_DATE ('30/06/2004',
                                                             'DD/MM/YYYY'
                                                            )
                                                   ),
                                               liquida.liqffinperiodovol, 0
                                                                -- Devuelve 0.
                                                                           ,
                                               t.dintereses + t.dinteresesco --para los intereses cobrados en A/F anulados
                                              )
                               -- Devuelve los intereses desde la funcion CTD.
                                               ,
                                           2
                                          )
                                     + NVL (liquida.liqcostas, 0)
                                     - t.dprincipalco
                                    ),
                                    2
                                   )
                           FROM dual)
                      * 100
                     ),
                  8,
                  '0'
                 )
           || '0'
          ) codigo_barras,
          (   '2'
           || albaadm.corta_etiqueta (cadena, 'PROVINCIA')
           || albaadm.corta_etiqueta (cadena, 'MUNICIPIO')
           || albaadm.corta_etiqueta (cadena, 'DCEMISORA')
           || albaadm.corta_etiqueta (cadena, 'REFERENCIA')
           || SUBSTR (albaadm.corta_etiqueta (cadena, 'REFERENCIADC'), 2, 2)
           || albaadm.corta_etiqueta (cadena, 'TRIBUTO')
           || albaadm.corta_etiqueta (cadena, 'EJERCICIO')
           || TO_CHAR (liquida.liqteorvtoeje, 'YY')
           ||
              --        lpad(to_char((nvl(LIQUIDA.LIQIMPORTE,0)+
              --                      nvl(LIQUIDA.LIQAPREMIO,0)+
              --                      nvl(LIQUIDA.LIQINTERESES,0)+
              --                      nvl(LIQUIDA.LIQCOSTAS,0)                      )*100),8,'0'))       CODIGO_BARRAS2
              LPAD
                 (TO_CHAR
                     (  (SELECT ROUND
                                   ((  NVL (liquida.liqimporte, 0)
                                                                 -- IMP. PPAL.
                                     + (ROUND
                                           (DECODE
                                               (GREATEST
                                                   (liquida.liqffinperiodovol,
                                                    TO_DATE ('30/06/2004',
                                                             'DD/MM/YYYY'
                                                            )
                                                   ),
                                                liquida.liqffinperiodovol, ROUND
                                                   (  NVL (t.dprincipal -t.dprincipalco,
                                                           0
                                                          )
                                                    * 0.1,
                                                    2
                                                   ),     -- Si aplica el 10%.
                                                ROUND
                                                   (  NVL (t.dprincipal -t.dprincipalco,
                                                           0
                                                          )
                                                    * 0.2,
                                                    2
                                                   )
                                               )
                                          -- Si aplica el 20% y cierra decode.
                                                ,
                                            2
                                           )
                                       )                            -- APREMIO
                                     + ROUND
                                          (DECODE
                                              (GREATEST
                                                   (liquida.liqffinperiodovol,
                                                    TO_DATE ('30/06/2004',
                                                             'DD/MM/YYYY'
                                                            )
                                                   ),
                                               liquida.liqffinperiodovol, 0
                                                                -- Devuelve 0.
                                                                           ,
                                               t.dintereses + t.dinteresesco --para los intereses cobrados en A/F anulados
                                              )
                               -- Devuelve los intereses desde la funcion CTD.
                                               ,
                                           2
                                          )                       -- INTERESES
                                     + NVL (liquida.liqcostas, 0)
                                     - t.dprincipalco
                                    ),
                                    2
                                   )                                 -- COSTAS
                           FROM dual)
                      * 100
                     ),
                  8,
                  '0'
                 )
          ) codigo_barras2,
          'NUMERO RECIBO:' || liquida.liqnumerorecliquidacion numero_valor,
          SUBSTR (liquida.liqnumerorecliquidacion, 1, 4) ejercicio,
          
          --NTF.NTFSERADE CODIGO_BARRAS3
          NVL (ntf.ntfsicergrupo, ntf.ntfserade) codigo_barras3
     FROM                                                        -- Expediente
          albaadm.exacciones mex,
          albaadm.expingre EXP,
          -- OBJETO
          albaadm.seobjetos seo,
          
          --   DETERMINANTESOBJETO       DEO,
          --   DETOBJETO                 DET,
          --  CALCDETERMINANTESVAL      CAL,
          (SELECT liquida.*,
                  albaadm.cabecera_modalidad2
                     (liqid,
                      (SELECT ROUND
                                 ((  NVL (liquida.liqimporte, 0)
                                                              -- Importe Ppal.
                                   + (ROUND
                                         (DECODE
                                             (GREATEST
                                                   (liquida.liqffinperiodovol,
                                                    TO_DATE ('30/06/2004',
                                                             'DD/MM/YYYY'
                                                            )
                                                   ),
                                              liquida.liqffinperiodovol, ROUND
                                                   (  NVL (t.dprincipal -t.dprincipalco,
                                                           0
                                                          )
                                                    * 0.1,
                                                    2
                                                   ),     -- Si aplica el 10%.
                                              ROUND
                                                   (  NVL (t.dprincipal -t.dprincipalco,
                                                           0
                                                          )
                                                    * 0.2,
                                                    2
                                                   )
                                             )
                                          -- Si aplica el 20% y cierra decode.
                                              ,
                                          2
                                         )
                                     )
-- Cierro Round y al apremio obtenido del decode, le resto el apremio cobrado.
                                   + ROUND
                                        (DECODE
                                            (GREATEST
                                                   (liquida.liqffinperiodovol,
                                                    TO_DATE ('30/06/2004',
                                                             'DD/MM/YYYY'
                                                            )
                                                   ),
                                             liquida.liqffinperiodovol, 0
                                                                -- Devuelve 0.
                                                                         ,
                                             t.dintereses + t.dinteresesco --para los intereses cobrados en A/F anulados
                                            )
                               -- Devuelve los intereses desde la funcion CTD.
                                             ,
                                         2
                                        )                           -- INTERES
                                   + NVL (liquida.liqcostas, 0)     -- Costas.
                                   - t.dprincipalco
                                  )                         -- Ingresos a Cta.
                                   ,
                                  2
                                 ) importe_deu
                             -- Cierra Round y concatena el resto del formato.
                         FROM TABLE
                                 (alba.pkg_calculoimportes.ctd (liquida.liqid,
                                                                SYSDATE,
                                                                2,
                                                                0,
                                                                1
                                                               )
                                 ) t)
                     ) cadena
             FROM albaadm.liquidaciones liquida) liq,
          albaadm.liquidaciones liquida,
          albaadm.propuestaliquidaciones pli,
          -- NOTIFICACIONES
          albaadm.remesas reme,
          albaadm.notificaciones ntf,
          albaadm.propuestasnotificacion pnt,
          albaadm.unidadesnotf udn,
          -- RESULTADOSNOTIF
          albaadm.cabremesas crm,
          albaadm.detallesactuacionesejecu dae,
          albaadm.actuacionesejecutiva aej,
          albaadm.definicionactuacioneseje dac,
          albaadm.tiposactuaciones tia,
          TABLE
                                 (alba.pkg_calculoimportes.ctd (liquida.liqid,
                                                                SYSDATE,
                                                                2,
                                                                0,
                                                                1
                                                               )
                                 ) t
    WHERE
          -- EXPEDIENTE
          EXP.mexid = mex.mexid
      -- VEHICULOS
      AND seo.expid = EXP.expid
      -- AND  DET.SeoId                 = SEO.SeoId
      -- AND  DET.DBJVersion            IN (SELECT MAX(DBJVersion) FROM DETOBJETO V WHERE DET.SeoId = V.SeoId)
      -- AND  DET.DBJID = DEO.DBJID
      -- AND  CAL.DVAID = DEO .DVAID
      -- AND  DEO.DBDID IN  (SELECT MAX(DBDID) FROM DETERMINANTESOBJETO       DEO1 WHERE DEO.DBJID = DEO1.DBJID)
      -- LIQUIDACIONES
      AND EXP.expid = liq.expid
      AND pli.pliid = liq.pliid
      AND liquida.liqid = liq.liqid
      AND liq.liqxestado <> 13                -- AGR, No muestra las cobradas.
      -- NOTIFICACIONES
      AND pnt.pntid = ntf.pntid
      AND ntf.ntfid = reme.ntfid
      AND reme.crmid = crm.crmid
      AND crm.udnid = udn.udnid
      AND pnt.pntorigenid = liquida.liqid
      -- ACTUACIONESEJECUTIVA
      AND dae.liqid = liq.liqid
      AND aej.aejid = dae.aejid
      AND aej.dacid = dac.dacid
      AND tia.tiaid = dac.tiaid
      AND tia.tiacodigo = 'REDEU'
      -- Para que coja solo las notificaciones de la remesa -- NUEVO
      and nvl (ntf.ntfcrmid, ntf.ntfcrmidant) = crm.crmid;
