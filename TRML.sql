CREATE OR REPLACE
PACKAGE TRML as

    type tipoCGenerico is ref cursor;

    type tCadena is record(texto varchar2(15000));
    type tCadena_lista is table of tCadena;
    
    type tInformeLibro is record
    (
        expediente varchar2(30),
        infractor varchar2(120),
        infraccion varchar2(2000),
        sancion varchar2(20),
        articulo alba.infracciones.infarticulo%type,
        apartado alba.infracciones.infapartado%type,
        libro alba.libro_resoluciones.lbrnumero%type,
        boletin varchar2(30),
        denunciante varchar2(300),
        lugar varchar2(1000),
        fecha varchar2(30),
        hora varchar2(30),
        matricula varchar2(30),
        nif  varchar2(80),
        licitacion varchar2(300),
        informe alba.libro_resol_inf.lriinforme%type,
        motivo varchar2(250),
        localidad varchar2(300),
        nexpediente varchar2(30),
        nombre varchar2(120),
        infarticulo alba.infracciones.infarticulo%type,
        infapartado alba.infracciones.infapartado%type,
        cuantia varchar2(20),
        decdescr alba.decoingresos.decdescr%type,
        lbrnumero alba.libro_resoluciones.lbrnumero%type,	
        dlbid alba.det_libro_resol.dlbid%type,
        dlbfhoramod alba.det_libro_resol.dlbfhoramod%type,
        lbrid alba.libro_resoluciones.lbrid%type,
        expid alba.det_libro_resol.expid%type,
        mulid alba.det_libro_resol.mulid%type,
        seoidexp alba.multas.seoidexp%type,
        liquidacion varchar2(20),
        impLiq varchar2(20),
        fSancionFirme varchar2(30),
        fFinVoluntaria varchar2(30),
        fInicioEjecutiva varchar2(30),
        fNotApremio varchar2(30),
        edictoApremio varchar2(30),
        fnotsan varchar2(30),
        a1 varchar2(15000),
        b1 clob,
        a2 varchar2(15000),
        b2 clob,
        a3 varchar2(15000),
        b3 clob,
        a4 varchar2(15000),
        b4 clob,
        a5 varchar2(15000),
        b5 clob,
        a6 varchar2(15000),
        b6 clob,
        a7 varchar2(15000),
        b7 clob,
        a8 varchar2(15000),
        b8 clob,
        a9 varchar2(15000),
        b9 clob,
        a10 varchar2(15000),
        b10 clob,
        a11 varchar2(15000),
        b11 clob,
        a12 varchar2(15000),
        b12 clob,
        a13 varchar2(15000),
        b13 clob,
        a14 varchar2(15000),
        b14 clob,
        a15 varchar2(15000),
        b15 clob
    );
    type tInformeLibro_lista is table of tInformeLibro;
    
    type tValores is record
    (
        id number,
        nombre varchar2(20),
        descripcion varchar2(100)
    );
    type tValores_lista is table of tValores;

    --------------------------------------------------------------------------------
    -- Registro Interfaz de Salida Publicación BOP
    --------------------------------------------------------------------------------
    type bop is record
    (
        infractor varchar2(300),
        nif varchar2(30),
        expediente varchar2(30),
        boletin varchar2(30),
        fecha varchar2(20),
        hora varchar2(20),
        lugar varchar2(1000),
        matricula varchar2(30),
        denunciante varchar2(300),
        normativa_puntos varchar2(3000),
        cuantia varchar2(30)
    );
    type bop_lista is table of bop;


    
    --------------------------------------------------------------------------------
    -- Registro Interfaz de Salida emultas
    --------------------------------------------------------------------------------
    type mul is record
    (
    	-- Denuncia
    	mulid number, mulnumbol number, expid number, expediente varchar2(12), tnotden varchar2(4),
    	tnotsan varchar2(4), fcomcon date, fecomcon date,
      matricula varchar2(60), marca varchar2(100), tipo varchar2(60),
    	-- Titular
    	titular varchar2(100), nift varchar2(9), tpt number, domt varchar2(200), loct varchar2(100),
    	prot varchar2(100), cpt varchar2(7), rdiidt number, peridt number, peridpt number,
    	-- Conductor
    	conductor varchar2(100), nifc varchar2(9), tpc number, domc varchar2(200), locc varchar2(100),
    	proc varchar2(100), cpc varchar2(7), rdiidc number, peridc number, peridpc number,
    	-- Tutor
    	tutor varchar2(100), niftt varchar2(9), tptt number, domtt varchar2(200), loctt varchar2(100),
    	prott varchar2(100), cptt varchar2(7), rdiidtt number, peridtt number, peridptt number,
    	-- Cobros
    	csl number,
    	-- Denuncia
    	notden number, fnotden date, libinc number, flibinc date, notdenrdiid number,
    	notdenperid number, notdenperv number, notdenacto varchar2(4),
    	-- Sanción
    	notsan number, fnotsan date, libsan number, flibsan date, notsanrdiid number,
    	notsanperid number, notsanperv number, notsanacto varchar2(4),
    	-- Requerimiento
    	notreq number, fnotreq date, libreq number, flibreq date, notreqrdiid number,
    	notreqperid number, notreqperv number, notreqacto varchar2(4),
    	-- Atestado
    	ate number, notate number, fnotate date, libate number, flibate date, notaterdiid number,
    	notateperid number, notateperv number, notateacto varchar2(4),      
    	-- Alegación
    	aleg number, faleg date, fraleg date, notaleg number, fnotaleg date,libaleg number,
    	flibaleg date, notalegrdiid number, notalegperid number,notalegperv number,
    	notalegacto varchar2(4), alegesmid number,
    	-- Recurso
    	rec number, frec date, frrec date, notrec number, fnotrec date, librec number,flibrec date,
    	notrecrdiid number, notrecperid number, notrecperv number,notrecacto varchar2(4), recesmid number,
    	-- Recurso Extraordinario de Revisión
    	rer number, frer date, frrer date, notrer number, fnotrer date, librer number, flibrer date,
    	notrerrdiid number, notrerperid number, notrerperv number, notreracto varchar2(4), reresmid number,
    	-- Baja
    	baja number, fbaja date, frbaja date, notbaja number, fnotbaja date, libbaja number, flibbaja date,
        notbajardiid number, notbajaperid number, notbajaperv number, notbajaacto varchar2(4), bajaesmid number,
    	-- Informe Denunciante
    	tipoinfd number, infd number, finfd date, frinfd date, notinfd number, fnotinfd date, libinfd number,
    	flibinfd date, notinfdrdiid number, notinfdperid number, notinfdperv number, notinfdacto varchar2(4),
    	infdesmid number,  		
    	-- Informe Requerimiento
    	tipoinfq number, infq number, finfq date, frinfq date, notinfq number, fnotinfq date, libinfq number,
    	flibinfq date, notinfqrdiid number, notinfqperid number, notinfqperv number, notinfqacto varchar2(4),
    	infqesmid number, 		
    	-- Estado
    	cestado number, estado varchar2(200), ffirmeza date, finivol date, ffinvol date,
    	-- Prescripción
    	forigenCalculoPrescripcion date,
        fprescripcion date,
    	-- Libro Sanción Firme
    	libSanFir number, flibSanFir date,
      -- BOP denuncia
    	fnotdenIntento date, libbopden number, flibbopden date,
    	-- BOP sanción
    	fnotsanIntento date, libbopsan number, flibbopsan date,
    	-- BOP requerimientos
    	fnotreqIntento date, libbopreq number, flibbopreq date,    	
    	-- BOP atestados
    	fnotateIntento date, libbopate number, flibbopate date,         
    	-- BOP alegaciones
    	fnotalegIntento date, libbopaleg number, flibbopaleg date,    	
    	-- BOP recursos
    	fnotrecIntento date, libboprec number, flibboprec date,    	
    	-- BOP recursos extraordinarios de revisión
    	fnotrerIntento date, libboprer number, flibboprer date,
    	-- BOP recursos providencia apremio
    	fnotrpaIntento date, libboprpa number, flibboprpa date,     	
    	-- Consulta Datos
    	fconsultaDatos date, libcondat number, flibcondat date,     	      
    	--
    	origenDenuncia number,
    	-- Recurso Providencia de Apremio
    	rpa number, frpa date, frrpa date, notrpa number, fnotrpa date, librpa number, flibrpa date,
    	notrpardiid number, notrpaperid number, notrpaperv number,notrpaacto varchar2(4), rpaesmid number,
    	numeroLibroApremio varchar2(20), fResolucionLibroApremio varchar2(20),
    	-- Providencia de Apremio
    	notpa number, fnotpa date,
        -- Datos Liquidacion
        liqid number, liquidacion varchar2(20), importeLiqTexto varchar2(20), importeLiqNum number, edictoApremio varchar2(100),
        fSancionFirmeLiq varchar2(20), fFinVoluntariaLiq varchar2(20), fInicioEjecutivaLiq varchar2(20), estadoLiq number,
        intentoNotPA varchar2(100), bopPa varchar2(100),
        -- Requerimiento Juzgado
    	rej number, frej date, frrej date, rejesmid number,
    	-- Recurso Económico Administrativo REA
    	rea number, frea date, frrea date, notrea number, fnotrea date, librea number, flibrea date,
    	notreardiid number, notreaperid number, notreaperv number,notreaacto varchar2(4), reaesmid number,
      -- Anotación de puntos
      xml number,
      -- Error en vehículo
      errorenvehiculo number,
      -- Estado Cobro
      cestadocobro number, estadocobro varchar2(200), importeexpte number, fcobro date,
      cobrado number, total number, pendiente number,
      -- Prescripción y Caducidad
      pres number,
      cpres varchar2(1000),
      cad number,
      ccad varchar2(1000),
      info varchar2(2000)
    );
    type mul_lista is table of mul;
    
    
    
    --------------------------------------------------------------------------------
    -- Registro Interfaz de Salida fmultas
    --------------------------------------------------------------------------------
    type fmul is record
    (
        mulid number,
        expediente number,
        peridD number,
        perversionD number,
        rdiidD number,
        peridT number,
        perversionT number,
        rdiidT number,
        --
        ntfid number,
        tipoNot varchar2(10),
        fLimitePago varchar2(20),
        Emisora varchar2(6),
        Referencia varchar2(10),
        DC varchar2(2),
        Identificacion varchar2(10),
        Importe varchar2(20),
        Importe2 number,
        Importe3 varchar2(20),
        Importe4 number,
        nExpediente varchar2(30),
        Boletin varchar2(30),
        --
        fIncoacion varchar2(30),
        Lugar varchar2(1000),
        fDenuncia varchar2(30),
        Hora varchar2(30),
        Matricula varchar2(30),
        Vehiculo varchar2(80),
        --
        nifD varchar2(80),
        nombreD varchar2(120),
        domicilioD varchar2(500),
        cpD varchar2(80),
        localidadD varchar2(80),
        provinciaD varchar2(80),
        --
        denunciante varchar2(300),
        motivoNoEntrega varchar2(400),
        hecho varchar2(2000),
        normativa varchar2(300),
        calificacion varchar2(200),
        tiempoRetirada varchar2(80),
        --
        nifT varchar2(80),												--(1.6)
        nombreT varchar2(120),											--(1.6)
        domicilioT varchar2(500),										--(1.6)
        cpT varchar2(80),												--(1.6)
        localidadT varchar2(80),										--(1.6)
        provinciaT varchar2(80),										--(1.6)
        --
        velocidad varchar2(80),										    --(1.8,1.9,3.8)
        limiteVelocidad varchar2(80),						            --(1.8,1.9,3.8)
        textoCinemometro varchar2(80),							        --(1.8,1.9,3.8)
        --	
        alcoholAire varchar2(80),									    --(1.7,3.7)
        alcoholSangre varchar2(80),								        --(1.7,3.7)
        fNotDenunica varchar2(80),									    --(3.*)
        --	
        lugarOrg varchar2(1000),											--(1.6)
        fechaOrg varchar2(80),											--(1.6)	
        horaOrg varchar2(80),											--(1.6)
        matriculaOrg varchar2(80),									    --(1.6)
        hechoOrg varchar2(2000),											--(1.6)
        cbsicer varchar2(50),
        cbcuaderno60 varchar2(50),
        firma varchar2(80),
        --
        a1 varchar2(15000),
        b1 clob,
        a2 varchar2(15000),
        b2 clob,
        a3 varchar2(15000),
        b3 clob,
        a4 varchar2(15000),
        b4 clob,
        a5 varchar2(15000),
        b5 clob,
        a6 varchar2(15000),
        b6 clob,
        a7 varchar2(15000),
        b7 clob,
        a8 varchar2(15000),
        b8 clob,
        a9 varchar2(15000),
        b9 clob,
        a10 varchar2(15000),
        b10 clob,
        a11 varchar2(15000),
        b11 clob,
        a12 varchar2(15000),
        b12 clob,
        a13 varchar2(15000),
        b13 clob,
        a14 varchar2(15000),
        b14 clob,
        a15 varchar2(15000),
        b15 clob,
  
        informe varchar2(50),
        -- Ejecutiva
        recibo varchar2(100),
        sancionFirme varchar2(100),
        finVoluntaria varchar2(100),
        inicioEjecutiva varchar2(100),
        importeSancionFirme varchar2(2000),
        providenciaApremio varchar2(100),
        intentoNotPA varchar2(100),
        edictoPA varchar2(100),
        bopPa varchar2(100),
        numeroLibroApremio varchar2(20),
        fResolucionLibroApremio varchar2(20),
        caja varchar2(10),
        lote varchar2(10),
        indice varchar2(10),
        -- Fotos
        fot01 varchar2(200),
        fot02 varchar2(200),
        fot03 varchar2(200),
        fot04 varchar2(200)
    );
    type fmul_lista is table of fmul;

    type fReimpresion is record
    (
        ntfid number,    
        tipoNot varchar2(10),
        fLimitePago varchar2(20),
        Emisora varchar2(6),
        Referencia varchar2(10),
        DC varchar2(2),
        Identificacion varchar2(10),
        Importe varchar2(20),
        nExpediente varchar2(30),
        Boletin varchar2(30),
        fIncoacion varchar2(30),
        Lugar varchar2(1000),
        fDenuncia varchar2(30),
        Hora varchar2(30),
        Matricula varchar2(30),
        Vehiculo varchar2(80),
        nifD varchar2(80),
        nombreD varchar2(120),
        domicilioD varchar2(500),
        cpD varchar2(80),
        localidadD varchar2(80),
        provinciaD varchar2(80),
        denunciante varchar2(300),
        motivoNoEntrega varchar2(400),
        hecho varchar2(2000),
        normativa varchar2(300),
        calificacion varchar2(200),
        tiempoRetirada varchar2(80),
        nifT varchar2(80),
        nombreT varchar2(120),
        domicilioT varchar2(500),
        cpT varchar2(80),
        localidadT varchar2(80),
        provinciaT varchar2(80),
        velocidad varchar2(80),
        limiteVelocidad varchar2(80),
        textoCinemometro varchar2(80),
        alcoholAire varchar2(80),
        alcoholSangre varchar2(80),
        fNotDenunica varchar2(80),
        lugarOrg varchar2(1000),
        fechaOrg varchar2(80),
        horaOrg varchar2(80),
        matriculaOrg varchar2(80),
        hechoOrg varchar2(2000),
        cbsicer varchar2(50),
        cbcuaderno60 varchar2(50),
        firma varchar2(80),
        a1 varchar2(4000),
        b11 varchar2(4000),b12 varchar2(4000),b13 varchar2(4000),b14 varchar2(4000),  
        a2 varchar2(4000),
        b21 varchar2(4000),b22 varchar2(4000),b23 varchar2(4000),b24 varchar2(4000),
        a3 varchar2(4000),
        b31 varchar2(4000),b32 varchar2(4000),b33 varchar2(4000),b34 varchar2(4000),
        a4 varchar2(4000),
        b41 varchar2(4000),b42 varchar2(4000),b43 varchar2(4000),b44 varchar2(4000),
        a5 varchar2(4000),
        b51 varchar2(4000),b52 varchar2(4000),b53 varchar2(4000),b54 varchar2(4000),
        a6 varchar2(4000),
        b61 varchar2(4000),b62 varchar2(4000),b63 varchar2(4000),b64 varchar2(4000),
        a7 varchar2(4000),
        b71 varchar2(4000),b72 varchar2(4000),b73 varchar2(4000),b74 varchar2(4000),
        a8 varchar2(4000),
        b81 varchar2(4000),b82 varchar2(4000),b83 varchar2(4000),b84 varchar2(4000),
        a9 varchar2(4000),
        b91 varchar2(4000),b92 varchar2(4000),b93 varchar2(4000),b94 varchar2(4000),
        a10 varchar2(4000),
        b101 varchar2(4000),b102 varchar2(4000),b103 varchar2(4000),b104 varchar2(4000),
        a11 varchar2(4000),
        b111 varchar2(4000),b112 varchar2(4000),b113 varchar2(4000),b114 varchar2(4000),
        a12 varchar2(4000),
        b121 varchar2(4000),b122 varchar2(4000),b123 varchar2(4000),b124 varchar2(4000),
        a13 varchar2(4000),
        b131 varchar2(4000),b132 varchar2(4000),b133 varchar2(4000),b134 varchar2(4000),
        a14 varchar2(4000),
        b141 varchar2(4000),b142 varchar2(4000),b143 varchar2(4000),b144 varchar2(4000),
        a15 varchar2(4000),
        b151 varchar2(4000),b152 varchar2(4000),b153 varchar2(4000),b154 varchar2(4000),
      
        -- Ejecutiva
        recibo varchar2(100),
        sancionFirme varchar2(100),
        finVoluntaria varchar2(100),
        inicioEjecutiva varchar2(100),
        importeSancionFirme varchar2(2000),
        providenciaApremio varchar2(100),
        intentoNotPA varchar2(100),
        edictoPA varchar2(100),
        bopPa varchar2(100),
        numeroLibroApremio varchar2(20),
        fResolucionLibroApremio varchar2(20),
        caja varchar2(10),
        lote varchar2(10),
        indice varchar2(10),
        -- Fotos        
        fot01 varchar2(100),
        fot02 varchar2(100),
        fot03 varchar2(100),
        fot04 varchar2(100)        
    );
    type fReimpresion_lista is table of fReimpresion;

    -------------------------------------------------------------------------------
    -- Registro Variables Privadas
    --------------------------------------------------------------------------------
    type mul2 is record
    (
    	-- Variables Globales
    	vTipoSp number,
    	vEsni number,
    	vEsdr number,
    	vEsda number,
    	vEsdv number,
    	vEstado number,
    	vTemp number,
    	vFtemp date,
    	vFechaPrescripcion date,
    	vFechaOrigenPrescripcion date,
    	vAtocodigo varchar2(20),
    	vEstado2 varchar2(100),
    	vEstado3 varchar2(100),
    	vEstado4 varchar2(100),
    	vExpte varchar2(20),
    	vTidomid number,
    	vTdmresid number,
    	vTotalResoluciones number,
    	vParalizado number,
    	vIdConductor number,
    	vGrado number,
    	vGrado2 number,
    	vcalculoprescripciondenuncia number, --0»prescrito, 1»no prescrito
    	vescritoduplicado boolean,
      fechadenuncia date,
      total number,
      dev number
    );
    
    type prescripcion is record
    (
        tipoFecha varchar2(200),
        fecha date,
        dias number,
        limite number,
        fechaLimite date,
        enPlazo number
    );
    type prescripcion_lista is table of prescripcion;
    
    /* Documento Cobratorio */
    type dc is record
    (
        cdc varchar2(10), --0 OK, 1 Imposible generar documento
        ddc varchar2(200), --0 null, 1 Mensaje de imposibilidad de generar el documento
        --
        fLimitePago varchar2(20),
        Emisora varchar2(6),
        Referencia varchar2(16),
        DC varchar2(2),
        Identificacion varchar2(10),
        Importe varchar2(20),
        Importe2 number,
        Importe3 varchar2(20),
        Importe4 number,
        Expediente varchar2(30),
        Boletin varchar2(30),
        --
        fDenuncia varchar2(30),
        Hora varchar2(30),
        Matricula varchar2(30),
        Vehiculo varchar2(80),
        --
        nifD varchar2(80),
        nombreD varchar2(120),
        domicilioD varchar2(500),
        cpD varchar2(80),
        localidadD varchar2(80),
        provinciaD varchar2(80),
        --
        cbcuaderno60 varchar2(50),
        --
        informe varchar2(50),
        cbcuaderno60num varchar2(50),
        --
        lugar varchar2(1000),
        hecho varchar2(2000),
        mulid number,
        --
        liqid number,
        expid number,
        principal varchar2(20),
        costas varchar2(20),
        apremio varchar2(20),
        intereses varchar2(20),
        acuenta varchar2(20)
    );
    type dc_lista is table of dc;
    

    /* Documento Cobratorio */
    type eov is record
    (
        esmid number,
        mulid number,
        Boletin varchar2(30),        
        Expediente varchar2(30),
        fechadoc varchar2(20), 
        tipodoc varchar2(200),
        estadodoc varchar2(200)
    );
    type eov_lista is table of eov;    
    

    type fechas is record
    (
        descripcion varchar2(200),
        fecha date
    );
    type fechas_lista is table of fechas;
    
    type fechas2 is record
    (
        descripcion varchar2(200),
        fecha date,
        dias number        
    );
    type fechas_lista2 is table of fechas2;    
    
    type tea is record
    (
	   cestado number,
	   estado varchar2(500),
	   esmid number,
	   mulid number,
     rea number,
	   pleno varchar2(200),
	   exp_tea varchar2(200),
	   numeroLibro number,
	   libro number,
	   firma date,
	   notrea number,
	   fnotrea date,
	   crmnumero number,
	   crmcodigo varchar2(20),
	   iescrito varchar2(2000),
	   cestadoliq number,
	   estadoliq varchar2(500),
     liq varchar2(20)
    );
    type tea_lista is table of tea;
    
    --Liq
    type liq is record
    (
        liqid number,
        liquidacion varchar2(12),
        expediente varchar2(12),
        estado varchar2(300)        
    );
    type liq_lista is table of liq;     
    
    
    type agente_reg is record
    (
        ageid	number,
        codigo varchar2(20),
        numero number,
        usuid	number,
        login varchar2(100),
        nombre varchar2(300),
        email varchar2(100),
        cestado number,
        estado varchar2(100),
        datos clob
    );
    type agente_lista is table of agente_reg; 
    
    procedure procesaRies;
    
    function agente(vAgcodigo varchar2) return agente_lista pipelined;
    
    procedure cargaFicheroDenuncias;
    procedure generaEstados(vFechaHora varchar2);
    procedure matestado (vyear number);
    procedure matEstadoEscritos;
    procedure matDatos;
    
    function eescrito(vEsmid number) return tea_lista pipelined;
    function eescrito_tca(vEsmid number) return tea_lista pipelined;
    function eescrito_tea(vEsmid number) return tea_lista pipelined;    
    
    function muestraFechas(vMulid number) return fechas_lista2 pipelined;
    
    function muestraCalculoCaducidadSim(vMulid number) return prescripcion_lista pipelined;
    function muestraCalculoPrescripcionSim(vMulid number) return prescripcion_lista pipelined;
    function muestraCalculoPrescripcion(vMulid number) return prescripcion_lista pipelined;
    function muestraCalculoPrescripcion4(vMulid number) return prescripcion_lista pipelined;
    function muestraCalculoPrescripcion6(vMulid number) return prescripcion_lista pipelined; 
    
    function dbop(vId number) return bop_lista pipelined;
    
    function emultas(vID number) return mul_lista pipelined;

    function dmultas(vMulidPar number, vAlgidPar number) return fmul_lista pipelined;
    
    --procedure generaRemesaMultas (in_lbrid number);

    function fmultas(vcrmid number) return fmul_lista pipelined;
    
    procedure unificarSicer(vCrmid in number);
    
    function lc(vcadena varchar2) return varchar2;
    procedure errorEnVehiculo(vMulid number);

    procedure generaLibroMultas (
             in_lestado      in  number,
            in_usuidmod     in  number,
            in_sysdate      in  date,
            in_motivo       in  varchar2,
            out_procesados  out number,
            out_salida      out varchar2,
            in_fdesde		in  varchar2,
            in_fhasta		in  varchar2,
            in_tipoboletin  in  number,
            in_tipodocumento in number,
            in_estadodocumento in number            
            );

    procedure generaLibroMultasBatch (
             in_lestado      in  number,
            in_usuidmod     in  number,
            in_sysdate      in  date,
            in_motivo       in  varchar2,
            in_fdesde		in  varchar2,
            in_fhasta		in  varchar2,
            in_tipoboletin  in  number,
            in_tipodocumento in number,
            in_estadodocumento in number            
            );            
            
    procedure firmaLibro (in_lbrid number);            

    procedure generaPropNotifMultas (
            in_lbrid        in  number,
            in_usuidmod     in  number,
            in_sysdate      in  date,
            in_motivo       in  varchar2,
            out_procesados  out number,
            out_salida      out varchar2
            );
                                                             
    procedure procesaEscrito(paresmid varchar2,parboletin varchar2,partipodocumento number,
								parfechadocumento varchar2,parobservaciones varchar2, parusuario number,parresultado in out varchar2,
								partipoestado number, parfechaestado varchar2,
								pnif varchar2, pnombre varchar2, pape1 varchar2, pape2 varchar2, pperid varchar2, pperversion varchar2,
								pdireccion varchar2, plocalidad varchar2, pprovincia varchar2, pcp varchar2, prdiid varchar2, ptipopersona number);
								
    /* Obtención de los informes asociados a un determinado libro-tipo */
    function obtenerInformesLibro(lLbrId in number) return tValores_lista pipelined;
    
    function nuevaVersion14(vMulid number, vPerid number, vPerversion number, vRdiid number, vFechaComCon date)
        return number;
        
    function nuevaVersion16(vMulid number, vPerid number, vPerversion number, vRdiid number, vFechaComCon date)
        return number;
    
    /* Obtención de los datos a mostrar en el informe asociado a un libro */
    function informeLibroDeResolucion(lLbrId in number, sSujeto in varchar2,
                lEstado in number, bConsDetalle in number, bEsDetalle in number,
                lLriId in number) return tInformeLibro_lista pipelined;
                
    function ficheroMultas(vCrmid number) return fReimpresion_lista pipelined;

    function fbMultas(vCrmid number) return clob;    
    
    function ficheroIndexacionMultas(vLbrid number) return fReimpresion_lista pipelined;
    

    -- Analiza si existe el boletín, en caso negativo lo crea y devuelve el mulid
    -- sobre el que obtener el documento cobratorio
    procedure generarDocCobratorio(vMulid number,tipobol varchar2, boletin varchar2,fecha varchar2,
                                  hora varchar2,matricula varchar2,infraccion varchar2, usuario number,
                                  vOut in out number);

    -- Recibe el mulid de generarDocCobratorio y muestra los datos del documento
    function obtenerDocCobratorio(vMulid number) return dc_lista pipelined;
    
    -- Vínvulo mult@web
    function expteWeb(expte varchar2) return varchar2;
    function expteWeb2(expte varchar2,usuario varchar2) return varchar2;
    function esmidWeb(vEsmid number) return varchar2;
    function esmidWebResol(vEsmid number) return varchar2;
    function mulidWeb(mulid number) return varchar2;
    function bolWeb(bol varchar2) return varchar2;

    -- Carga de escritos desde fichero
    function cargaFicheroEscritos(vId number) return varchar2;
    
    -- Obtener direccion
    function obtenerDireccion(vDireccion varchar2,vLocalidad varchar2,vProvincia varchar2,vCP varchar2) return number;
    
    -- Obtener persona
    function obtenerPersona(vNif varchar2,vNombre varchar2,vApe1 varchar2,vApe2 varchar2, vPerversion in out number,
    vTipoPersona alba.tipospersona.tpeid%type) return number;
    
    -- Funciones de Recuperación
    function recuperaDireccionesEscritos return varchar2;
    
    function recuperaEscritosPerdidos return varchar2;
    
   
    procedure regeneraExpedientesBotellon;
    
    --function oficina_virtual(vId number) return dc_lista pipelined;
    
    function iBoletin(vBoletin number) return varchar2;    
    
    function esNumero(p_string_value varchar2) return number;
    
    procedure recuperaPropuesta(vCrmnumeroIn number);    
    
    function imgToHTML(vBoletin number) return varchar2;
    
    procedure resolverEscritos(vOut in out varchar2); 
    
    procedure clonar_resol(oEsmid in number, dEsmid in number);
    
    function escritosOV(vNif in varchar2:=null, vFechadoc in varchar2:=null) return eov_lista pipelined;
    
    function parametro(esmid number, resid number, par varchar2) return varchar2;
    
    function liqEscritos(esmid number) return liq_lista pipelined;
    
    function menu(usuario varchar2, etiqueta varchar2) return varchar2;
    
    function btoc(B BLOB) return clob;
    
    function esmidDoc(vEsmid number) return varchar2;
    
    function verNotif(vSicer varchar2) return varchar2;
    
    function crmWeb(vcrm varchar2) return varchar2;
    
    function doc(liq varchar2,nif varchar2) return clob;    
    
end trml;