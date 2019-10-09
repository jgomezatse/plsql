create or replace package body mul_boletines_externos
is

--------------------------------------------------------------------------------
    cursor c_multacar(pnomfich_cargado varchar2,ptipoboletin varchar2)
    is
        
        select*
        from alba.multacar
        where est_procesado       ='PENDIENTE'
        and tipoboletin           =ptipoboletin
        and upper(nomfich_cargado)=upper(pnomfich_cargado) ;

--------------------------------------------------------------------------------
    tpkg_usuidmod  number;
    tpkg_motivomod varchar2(200):=
    'Carga denuncias, (Pkg "Mul_Boletines_Externos").';

--------------------------------------------------------------------------------
    pkg_sysdate date;
type ty_idpersona
is
    record
    (
        perid      number,
        perversion number) ;

--------------------------------------------------------------------------------
procedure ptratamiento_errores(
        ptexto_error    varchar2,
        pnombre_proceso varchar2,
        pcodigo         number default 20999)
is
    -- AGR, Descripcion de funcionalidad y parametros.MUL_BOLETINES_EXTERNOS
    -- 1. El primer parametro es el texto que se quiere elevar a la aplicacion,
    -- debe tratarse de un mensaje comprensible al usario final.
    -- 2. El segundo es una referencia para que el desarrollador localice el lugar
    -- exacto donde el codigo falla.
    -- 3. El tercer parametro es la codificacion propia del error, debe ser
    -- obligatorio si el error es funcional.
    -- OBJETIVO:    Este procedimiento con la ayuda de 'Raise_Application_Error',
    -- eleva el error a la capa de aplicacion independientemente
    --          de la tecnologia utilizada.
    -- Muestra un mensaje con tres partes:  1. Un texto legible al usr. de la
    -- aplicacion final.
    --                                      2. Una referencia para la localizacion
    -- del codigo que falla.
    --                                      3. Los codigos y mensajes propios de
    -- Oracle, por si el error no es unicamente funcional.
begin
    raise_application_error(-1*pcodigo,ptexto_error ||chr(10) ||'Error en: ' ||
    pnombre_proceso|| ':' ||chr(10) ||'ORA-'||lpad(abs(sqlcode),5,'0') -- SQLCODE,
    -- muestra el tipico "ORA-00904:"
    ||chr(10) ||sqlerrm) ; -- SQLERRM, Muestra el mensaje "PL/SQL: numeric or
    -- value error"

exception

when others then
    raise;

end;

--------------------------------------------------------------------------------
procedure pcarga_zonaazul(
        in_tmmid    number,
        in_usuidmod number,
        in_sysdate  date,
        in_motivo   varchar2,
        out_procesados out number,
        out_salida out varchar2)
is
    tpat number:=37363336444243; -- Pruebas con un numero cualquiera.
    
    cursor cleer_txt(ptmmid number)
    is
        
        select tmd.tmdorden,
            decode(substr(tmdregistro,1,1),'C','CABECERA','I','DETALLE','F','TOTAL')
            tiporeg,
            tmdobservaciones,
            max(decode(cdc.cdccodigo,'TIPO',substr(replace(tmdregistro,'}{'),
            cdc.cdcposicion,cdc.cdclongitud),null)) tipo
            -- AGR, Cabcera.
            ,
            max(decode(substr(tmdregistro,1,1),'C',decode(cdc.cdccodigo,'FECHASOPORTE',
            substr(replace(tmdregistro,'}{'),cdc.cdcposicion,cdc.cdclongitud),null),null
            )) fechasoporte,
            max(decode(substr(tmdregistro,1,1),'C',decode(cdc.cdccodigo,'FECHAINICIO',
            substr(replace(tmdregistro,'}{'),cdc.cdcposicion,cdc.cdclongitud),null),null
            )) fechainicio,
            max(decode(substr(tmdregistro,1,1),'C',decode(cdc.cdccodigo,'FECHAFIN',
            substr(replace(tmdregistro,'}{'),cdc.cdcposicion,cdc.cdclongitud),null),null
            )) fechafin
            -- AGR, Detalle.
            ,
            max(trim(decode(substr(tmdregistro,1,1),'I',decode(cdc.cdccodigo,
            'NUMEROSECUENCIAL',substr(replace(tmdregistro,'}{'),cdc.cdcposicion,
            cdc.cdclongitud),null),null))) numerosecuencial,
            max(trim(decode(substr(tmdregistro,1,1),'I',decode(cdc.cdccodigo,
            'FECHAINFRACCION',substr(replace(tmdregistro,'}{'),cdc.cdcposicion,
            cdc.cdclongitud),null),null))) fechainfraccion,
            max(trim(decode(substr(tmdregistro,1,1),'I',decode(cdc.cdccodigo,
            'HORAINFRACCION',substr(replace(tmdregistro,'}{'),cdc.cdcposicion,
            cdc.cdclongitud),null),null))) horainfraccion,
            max(trim(decode(substr(tmdregistro,1,1),'I',decode(cdc.cdccodigo,
            'MINUTOINFRACCION',substr(replace(tmdregistro,'}{'),cdc.cdcposicion,
            cdc.cdclongitud),null),null))) minutoinfraccion,
            max(trim(decode(substr(tmdregistro,1,1),'I',decode(cdc.cdccodigo,
            'NUMEROBOLETIN',substr(replace(tmdregistro,'}{'),cdc.cdcposicion,
            cdc.cdclongitud),null),null))) numeroboletin,
            max(trim(decode(substr(tmdregistro,1,1),'I',decode(cdc.cdccodigo,'CALLE',
            substr(replace(tmdregistro,'}{'),cdc.cdcposicion,cdc.cdclongitud),null),null
            ))) calle,
            max(trim(decode(substr(tmdregistro,1,1),'I',decode(cdc.cdccodigo,
            'INDDELANFRENT',substr(replace(tmdregistro,'}{'),cdc.cdcposicion,
            cdc.cdclongitud),null),null))) inddelanfrent,
            max(trim(decode(substr(tmdregistro,1,1),'I',decode(cdc.cdccodigo,
            'DELANTEFRENTE',substr(replace(tmdregistro,'}{'),cdc.cdcposicion,
            cdc.cdclongitud),null),null))) delantefrente,
            max(trim(decode(substr(tmdregistro,1,1),'I',decode(cdc.cdccodigo,
            'CODIGOINFRAC',substr(replace(tmdregistro,'}{'),cdc.cdcposicion,
            cdc.cdclongitud),null),null))) codigoinfrac,
            max(trim(decode(substr(tmdregistro,1,1),'I',decode(cdc.cdccodigo,
            'NOTIFICADO',substr(replace(tmdregistro,'}{'),cdc.cdcposicion,
            cdc.cdclongitud),null),null))) notificado,
            max(trim(decode(substr(tmdregistro,1,1),'I',decode(cdc.cdccodigo,
            'MASCARAMAT',substr(replace(tmdregistro,'}{'),cdc.cdcposicion,
            cdc.cdclongitud),null),null))) mascaramat,
            max(trim(decode(substr(tmdregistro,1,1),'I',decode(cdc.cdccodigo,'MATRICULA'
            ,substr(replace(tmdregistro,'}{'),cdc.cdcposicion,cdc.cdclongitud),null),
            null))) matricula,
            max(trim(decode(substr(tmdregistro,1,1),'I',decode(cdc.cdccodigo,
            'DENUNCIANTE',substr(replace(tmdregistro,'}{'),cdc.cdcposicion,
            cdc.cdclongitud),null),null))) denunciante,
            max(trim(decode(substr(tmdregistro,1,1),'I',decode(cdc.cdccodigo,'AGENTE',
            substr(replace(tmdregistro,'}{'),cdc.cdcposicion,cdc.cdclongitud),null),null
            ))) agente,
            max(trim(decode(substr(tmdregistro,1,1),'I',decode(cdc.cdccodigo,'GRUA',
            substr(replace(tmdregistro,'}{'),cdc.cdcposicion,cdc.cdclongitud),null),null
            ))) grua,
            max(trim(decode(substr(tmdregistro,1,1),'I',decode(cdc.cdccodigo,
            'EXPEDIENTE',substr(replace(tmdregistro,'}{'),cdc.cdcposicion,
            cdc.cdclongitud),null),null))) expediente,
            max(trim(decode(substr(tmdregistro,1,1),'I',decode(cdc.cdccodigo,'ERROR',
            substr(replace(tmdregistro,'}{'),cdc.cdcposicion,cdc.cdclongitud),null),null
            ))) error,
            max(trim(decode(substr(tmdregistro,1,1),'I',decode(cdc.cdccodigo,'RELLENO',
            substr(replace(tmdregistro,'}{'),cdc.cdcposicion,cdc.cdclongitud),null),null
            ))) relleno,
            max(trim(decode(substr(tmdregistro,1,1),'I',decode(cdc.cdccodigo,'MATRICULA'
            ,substr(replace(tmdregistro,'}{'),cdc.cdcposicion,cdc.cdclongitud),null),
            null))) motivo,
            max(trim(decode(substr(tmdregistro,1,1),'I',decode(cdc.cdccodigo,'FIN',
            substr(replace(tmdregistro,'}{'),cdc.cdcposicion,cdc.cdclongitud),null),null
            ))) fin,
            max(trim(decode(substr(tmdregistro,1,1),'I',decode(cdc.cdccodigo,
            'DENUNCIANTE2',substr(replace(tmdregistro,'}{'),cdc.cdcposicion,
            cdc.cdclongitud),null),null))) denunciante2,
            max(trim(decode(substr(tmdregistro,1,1),'I',decode(cdc.cdccodigo,'MARCA',
            substr(replace(tmdregistro,'}{'),cdc.cdcposicion,cdc.cdclongitud),null),null
            ))) marca,
            max(trim(decode(substr(tmdregistro,1,1),'I',decode(cdc.cdccodigo,'MODELO',
            substr(replace(tmdregistro,'}{'),cdc.cdcposicion,cdc.cdclongitud),null),null
            ))) modelo
            -- AGR, Pie.
            ,
            max(decode(substr(tmdregistro,1,1),'F',decode(cdc.cdccodigo,
            'NUMEROREGISTROS',substr(replace(tmdregistro,'}{'),cdc.cdcposicion,
            cdc.cdclongitud),null),null)) numeroregistros,
            max(decode(substr(tmdregistro,1,1),'F',decode(cdc.cdccodigo,'FECHAINCORPORA'
            ,substr(replace(tmdregistro,'}{'),cdc.cdcposicion,cdc.cdclongitud),null),
            null)) fechaincorpora,
            max(decode(substr(tmdregistro,1,1),'F',decode(cdc.cdccodigo,'NUMINCORPORA',
            substr(replace(tmdregistro,'}{'),cdc.cdcposicion,cdc.cdclongitud),null),null
            )) numincorpora
        from alba.temporalcargasdetalle tmd,
            alba.temporalcargasmaestro tmm,
            alba.tpcarga tpc,
            alba.cargasdetallefilas cdf,
            alba.cargasdetallecolumnas cdc,
            alba.decodificadora dec
        where tpc.tpcid  =tmm.tpcid
        and tmm.tmmid    =tmd.tmmid
        and tpc.tpcid    =cdf.tpcid
        and cdf.cdfid    =cdc.cdfid
        and cdc.cdcxtipo =dec.decid
        and cdf.cdfcodigo=decode(substr(tmdregistro,1,1),'C','CABECERA','I','DETALLE'
            ,'F','TOTAL')
        and substr(tmdregistro,1,1)='I' -- AGR, SOLO MUESTRA EL DETALLE.
        and tmd.tmdobservaciones  is null
        and tmd.tmmid              =ptmmid -- 18304
        group by tmdorden,
            tmdregistro,
            tmdobservaciones;
    
    ttmmarchivo              varchar2(200) ;
    ttmmfechacarga           date;
    ttotalfichero            number;
    ttmmregistrosbien        number;
    ttmmregistrosmal         number;
    test                     varchar2(20):='I';
    tdelantede               varchar2(250) ;
    tfrentea                 varchar2(250) ;
    tcodmotivononotificacion number;
    tcodtipoboletin          number;
    tviaid                   number;
    tinfid                   number;
    tid_denunciante1 ty_idpersona;
    tid_denunciante2 ty_idpersona;
    i number:=1; -- Es un 1..i de posicion de tabla, por eso inicializo a 1.
begin
    
    --  insert into depuracion.b_agr_marcatiempo values ('Carga Multacar',Sysdate,
    -- null);
    -- AGR, Inserto cabecera.
    
    select upper(tmm.tmmarchivo),
        tmmfechacarga,
        tmmregistrosbien+tmmregistrosmal,
        tmmregistrosbien,
        tmmregistrosmal
    into ttmmarchivo,
        ttmmfechacarga,
        ttotalfichero,
        ttmmregistrosbien,
        ttmmregistrosmal
    from alba.temporalcargasmaestro tmm
    where tmmid=in_tmmid;
    begin
        
        insert
        into alba.maestra_carga
            (
                nomfich_cargado,
                path,
                fecha_carga,
                total_fichero,
                total_correctos,
                total_incidencias,
                descr,
                fichero_bad,
                fichero_dsc,
                estado_boletin,
                estado_exp
            )
            values
            (
                ttmmarchivo,
                null,
                ttmmfechacarga,
                ttotalfichero,
                ttmmregistrosbien,
                ttmmregistrosmal,
                'Carga Zona Azul',
                null,
                null,
                'CARGADOS',
                'PENDIENTES'
            ) ;
    
    exception
    
    when dup_val_on_index then
        null;
        ttmmarchivo:=to_char(ttmmfechacarga,'RRRRMMDD') ||'$'||ttmmarchivo||'.txt';
        -- Renombro el fichero
        
        insert
        into alba.maestra_carga
            (
                nomfich_cargado,
                path,
                fecha_carga,
                total_fichero,
                total_correctos,
                total_incidencias,
                descr,
                fichero_bad,
                fichero_dsc,
                estado_boletin,
                estado_exp
            )
            values
            (
                ttmmarchivo,
                null,
                ttmmfechacarga,
                ttotalfichero,
                ttmmregistrosbien,
                ttmmregistrosmal,
                'Carga Zona Azul',
                null,
                null,
                'CARGADOS',
                'PENDIENTES'
            ) ;
    
    end;
    
    -- AGR, Fin inserto cabecera.
    
    select decid
    into tcodmotivononotificacion
    from table(alba.utl_alba.deco('MOTIVO',1)) ;
    
    select decid
    into tcodtipoboletin
    from table(alba.utl_alba.deco('BOLETIN','ZA')) ;
    --  For i In 1..TTmmRegistrosBien Loop
    --      Null;
    --  End Loop;
    
    for reg in cleer_txt(in_tmmid)
    loop
        
        -- Transfrormando algunos datos.
        
        select decode(reg.inddelanfrent,'D',reg.delantefrente)
        into tdelantede
        from dual;
        
        select decode(reg.inddelanfrent,'F',reg.delantefrente)
        into tfrentea
        from dual;
        
        --dbms_output.put_line (Reg.TMDOBSERVACIONES                ||Reg.Motivo||' x
        -- '||TCODMOTIVONONOTIFICACION||' x '||TCODTIPOBOLETIN||' x '||
        -- Reg.DENUNCIANTE||' x '||Reg.DENUNCIANTE2 ||' x '||reg.motivo);
        --      Select Decode(Nvl(
        --      Replace(        SubStr  (RawToHex(Reg.TMDOBSERVACIONES              |
        -- |Reg.Motivo),1,99),
        --          To_Char(Tpat))
        --      ,'0'),'0','P','I') Into TEst From Dual;
        --dbms_output.put_line (Reg.TMDOBSERVACIONES                ||Reg.Motivo||' x
        -- '||TCODMOTIVONONOTIFICACION||' x '||TCODTIPOBOLETIN||' x '||
        -- Reg.DENUNCIANTE||' x '||Reg.DENUNCIANTE2 ||' x '||reg.motivo);
        
        select viaid
        into tviaid
        from alba.vias
        where viacodigo=reg.calle;
        
        select infid
        into tinfid
        from alba.infracciones
        where infcodigo=reg.codigoinfrac;
        
        select per.perid,
            per.perversion
        into tid_denunciante1.perid,
            tid_denunciante1.perversion
        from alba.personas per
        where periden=reg.denunciante
        and rownum   =1;
        begin -- Si no viene el denunciante 2, de momento el proceso continua
            -- cargando.
            
            select per.perid,
                per.perversion
            into tid_denunciante2.perid,
                tid_denunciante2.perversion
            from alba.personas per
            where periden=reg.denunciante2
            and rownum   =1;
        
        exception
        
        when no_data_found then
            null;
        
        end;
        -- Fin transformacion.
        begin
            
            insert
            into alba.multacar
                (
                    boletin,
                    tipoboletin,
                    annoboletin,
                    fechadenuncia,
                    horadenuncia,
                    codigovia,
                    vialugar,
                    delantede,
                    frentea,
                    condirecciona,
                    codigohechodenunciado,
                    descripcionhd,
                    observacioneshd,
                    matricula,
                    marcavehiculo,
                    tipovehiculo,
                    modelovehiculo,
                    codigodenunciante1,
                    nifdenunciante1,
                    nombredenunciante1,
                    domiciliodenunciante1,
                    cpdenunciante1,
                    localidaddenunciante1,
                    provinciadenunciante1,
                    codigodenunciante2,
                    nifdenunciante2,
                    nombredenunciante2,
                    domiciliodenunciante2,
                    cpdenunciante2,
                    localidaddenunciante2,
                    provinciadenunciante2,
                    grua,
                    notificadaenmano,
                    motivononotificacion,
                    medidavelocidad,
                    medidaalcohol,
                    atestado,
                    niftitular,
                    nombretitular,
                    domiciliotitular,
                    cptitular,
                    localidadtitular,
                    provinciatitular,
                    nifconductor,
                    nombreconductor,
                    domicilioconductor,
                    cpconductor,
                    localidadconductor,
                    provinciaconductor,
                    niftutor,
                    nombretutor,
                    domiciliotutor,
                    cptutor,
                    localidadtutor,
                    provinciatutor,
                    nomfich_cargado,
                    codmotivononotificacion,
                    codtipoboletin,
                    viaid,
                    infid,
                    ageid,
                    perid_denunc1,
                    perversion_denunc1,
                    perid_denunc2,
                    perversion_denunc2,
                    est_procesado,
                    fhoramod,
                    usuidmod,
                    motivomod
                )
                values
                (
                    reg.numeroboletin,
                    'ZA',
                    to_number(to_char(to_date(reg.fechainfraccion),'RRRR')),
                    reg.fechainfraccion,
                    reg.horainfraccion
                    ||':'
                    ||reg.minutoinfraccion,
                    reg.calle,
                    null,
                    tdelantede,
                    tfrentea,
                    null,
                    reg.codigoinfrac,
                    null,
                    null,
                    reg.matricula,
                    reg.marca,
                    null,
                    reg.modelo,
                    null,
                    reg.denunciante,
                    null,
                    null,
                    null,
                    null,
                    null,
                    null,
                    reg.denunciante2,
                    null,
                    null,
                    null,
                    null,
                    null,
                    nvl(reg.grua,'N'),
                    'N',
                    null,
                    null,
                    null,
                    null,
                    null,
                    null,
                    null,
                    null,
                    null,
                    null,
                    null,
                    null,
                    null,
                    null,
                    null --LOCALIDADCONDUCTOR
                    ,
                    null,
                    null,
                    null,
                    null,
                    null,
                    null --LOCALIDADTUTOR
                    ,
                    null,
                    ttmmarchivo,
                    tcodmotivononotificacion,
                    tcodtipoboletin,
                    tviaid,
                    tinfid,
                    null,
                    tid_denunciante1.perid,
                    tid_denunciante1.perversion,
                    tid_denunciante2.perid,
                    tid_denunciante2.perversion,
                    decode(test,'I','PENDIENTE','PROCESADO'),
                    in_sysdate,
                    in_usuidmod,
                    in_motivo
                ) ;
        
        exception
        
        when dup_val_on_index then
            null;
        
        end;
        out_procesados:=nvl(out_procesados,0)+1;
    
    end loop;
    --  insert into depuracion.b_agr_marcatiempo values ('Carga Multacar',NUll,
    -- Sysdate);
    --  Delete multacar where est_procesado = 'PENDIENTE'
    --      and rownum <= 3414;
    --  Commit;
    --  insert into depuracion.b_agr_marcatiempo values ('Proceso expedientes',
    -- Sysdate,null);
    out_salida:='Antes de Inciar la generacion de expedientes';
    pcarga_bol_multacar(ttmmarchivo,in_usuidmod,in_motivo,'ZA') ;
    --  Commit;
    out_salida:='Generados los expedientes '||out_procesados||' de sanciones.';
    --  insert into depuracion.b_agr_marcatiempo values ('Proceso expedientes',
    -- null, Sysdate);

end pcarga_zonaazul;

--------------------------------------------------------------------------------
function finserta_incidencia
    (
        reg alba.temp_interf_multacar%rowtype,
        pcodigoincidencia varchar2,
        ptexto_incidencia varchar2,
        ptipo_incidencia  varchar2
    )
    return boolean
is
    texiste_incidencia    boolean:=true;
    tno_existe_incidencia boolean:=false;
    --Regaux Alba.Temp_Interf_MultaCar%RowType;

procedure inserta
is
begin
    
    insert
    into alba.incid_txt_multacar
        (
            nomfich_cargado,
            boletin,
            tipoboletin,
            annoboletin,
            fechadenuncia,
            horadenuncia,
            codigovia,
            vialugar,
            delantede,
            frentea,
            condirecciona,
            codigohechodenunciado,
            descripcionhd,
            observacioneshd,
            matricula,
            marcavehiculo,
            tipovehiculo,
            modelovehiculo,
            codigodenunciante1,
            nifdenunciante1,
            nombredenunciante1,
            domiciliodenunciante1,
            cpdenunciante1,
            localidaddenunciante1,
            provinciadenunciante1,
            codigodenunciante2,
            nifdenunciante2,
            nombredenunciante2,
            domiciliodenunciante2,
            cpdenunciante2,
            localidaddenunciante2,
            provinciadenunciante2,
            grua,
            notificadaenmano,
            motivononotificacion,
            medidavelocidad,
            medidaalcohol,
            atestado,
            niftitular,
            nombretitular,
            domiciliotitular,
            cptitular,
            localidadtitular,
            provinciatitular,
            nifconductor,
            nombreconductor,
            domicilioconductor,
            cpconductor,
            localidadconductor,
            provinciaconductor,
            niftutor,
            nombretutor,
            domiciliotutor,
            cptutor,
            localidadtutor,
            provinciatutor,
            fichero1 --,Foto_Color
            ,
            fichero2 --,Foto_Lectura
            ,
            fichero3 --,Boletin_Pdf
            ,
            fichero4 --,Fichero_Otros
            ,
            codincid,
            incidencia,
            tipo_incidencia
            --boletin2-- Respectivamente Codigo de incidencia, texto de la incidencia y
            -- Tipo de incidencia.
        )
        values
        (
            reg.nomfich_cargado,
            reg.boletin,
            reg.tipoboletin,
            reg.annoboletin,
            reg.fechadenuncia,
            reg.horadenuncia,
            reg.codigovia,
            reg.vialugar,
            reg.delantede,
            reg.frentea,
            reg.condirecciona,
            reg.codigohechodenunciado,
            reg.descripcionhd,
            reg.observacioneshd,
            reg.matricula,
            reg.marcavehiculo,
            reg.tipovehiculo,
            reg.modelovehiculo,
            reg.codigodenunciante1,
            reg.nifdenunciante1,
            reg.nombredenunciante1,
            reg.domiciliodenunciante1,
            reg.cpdenunciante1,
            reg.localidaddenunciante1,
            reg.provinciadenunciante1,
            reg.codigodenunciante2,
            reg.nifdenunciante2,
            reg.nombredenunciante2,
            reg.domiciliodenunciante2,
            reg.cpdenunciante2,
            reg.localidaddenunciante2,
            reg.provinciadenunciante2,
            reg.grua,
            reg.notificadaenmano,
            reg.motivononotificacion,
            reg.medidavelocidad,
            reg.medidaalcohol,
            reg.atestado,
            reg.niftitular,
            reg.nombretitular,
            reg.domiciliotitular,
            reg.cptitular,
            reg.localidadtitular,
            reg.provinciatitular,
            reg.nifconductor,
            reg.nombreconductor,
            reg.domicilioconductor,
            reg.cpconductor,
            reg.localidadconductor,
            reg.provinciaconductor,
            reg.niftutor,
            reg.nombretutor,
            reg.domiciliotutor,
            reg.cptutor,
            reg.localidadtutor,
            reg.provinciatutor,
            reg.fichero1 --,Reg.Foto_Color
            ,
            reg.fichero2 --,Reg.Foto_Lectura
            ,
            reg.fichero3 --,Reg.Boletin_Pdf
            ,
            reg.fichero4 --,Reg.Fichero_Otros
            ,
            pcodigoincidencia,
            ptexto_incidencia,
            ptipo_incidencia
            --reg.boletin2
        ) ;

end inserta;
begin
    -- AGR, Elimino de MultaCar. Como la validacion de la existencia de los
    -- ficheros de imagen y pdf del boletin es posterior
    -- a la carga con SQL*LOAD, es necesario eliminarlo de la tabla "multacar", en
    -- caso de que se detecte que no se ha cargado
    -- alguno de estos ficheros.
    --
    texiste_incidencia:=true;
    
    /*if ptipo_incidencia in('20097','20098','20099') then
        
        delete
        from albaadm.multacar_blob
        where boletin  =reg.boletin
        and tipoboletin=reg.tipoboletin
        and annoboletin=reg.annoboletin;
        
        delete
        from albaadm.multacar
        where boletin  =reg.boletin
        and tipoboletin=reg.tipoboletin
        and annoboletin=reg.annoboletin;
    
    end if;*/
    inserta() ;
    
    if ptipo_incidencia='NO_INVALIDA_CARGA' then
        
        return tno_existe_incidencia;
    
    else
        
        return texiste_incidencia ;
    
    end if;

exception

when dup_val_on_index then -- En caso de que se ejecute la carga del mismo
    -- boletin modifica la/s incidencia/s.
    
    if reg.boletin=0 then -- Multacar repite mucho él error de mandar un boletin
        -- con código 0, voy a eliminarlo de la tabla para evitar el problema.
        
        /*delete alba.incid_txt_multacar
        where boletin  =reg.boletin
        and tipoboletin=reg.tipoboletin
        and annoboletin=reg.annoboletin;*/
        
        inserta() ;
    
    else
        
        if ptipo_incidencia<>'NO_INVALIDA_CARGA' then
            
            update alba.incid_txt_multacar
            set codincid=codincid
                ||' - '
                || pcodigoincidencia,-- Codigo Incidencia. Concatena con salto de carro.
                incidencia=incidencia
                ||chr(13)
                || ptexto_incidencia,
                tipo_incidencia=tipo_incidencia
                ||chr(13)
                ||ptipo_incidencia -- Descripcion Incidencia. Concatena con salto de carro.
            where boletin  =reg.boletin
            and tipoboletin=reg.tipoboletin
            and annoboletin=reg.annoboletin;
            --        And Instr(CodIncid, PCodigoIncidencia) = 0;
        
        end if;
    
    end if;
    
    if ptexto_incidencia='NO_INVALIDA_CARGA' then
        
        return tno_existe_incidencia;
    
    else
        
        return texiste_incidencia ;
    
    end if;

when others then
    ptratamiento_errores('Error al registrar la incidencia: '||reg.boletin ||' - '
    ||reg.tipoboletin ||' - '||reg.annoboletin,
    'Mul_Boletines_Externos.FInserta_Incidencia',20023) ;

end finserta_incidencia;

--------------------------------------------------------------------------------
procedure pcarga_img_multacar(
        pnomfich_cargado varchar2)
is
    existe_incidencia boolean:=false;
begin
    
  /*
    for reg in
    (
        select tem.*
        from alba.temp_interf_multacar tem,
            alba.multacar mul
        where upper(mul.nomfich_cargado)=upper(pnomfich_cargado)
        and tem.boletin                 =to_char(mul.boletin)
            ||decode(mul.boletin2,null,null,'@'
            ||mul.boletin2)
        and decode(tem.tipoboletin,'H','CH','R','SR',tem.tipoboletin)=mul.tipoboletin
            -- Este Decode se podra eliminar el dia que se modifique el ctl de la carga.
            --                And tem.ANNOBOLETIN = Mul.Annoboletin
        order by tem.boletin
    )
    loop
        begin
            
            -- AGR, si nos fijamo Reg.Boletin, trae el numero de boletin con la dichosa
            -- arroba y es necesaria eliminarla. para enviarla a FInserta_Errores.
            
            select substr(reg.boletin,1,decode(instr(reg.boletin,'@'),0,length(
                reg.boletin),instr(reg.boletin,'@')-1))
            into reg.boletin
            from dual;
            
            if reg.tipoboletin='CO' then
                
                if dbms_lob.getlength(reg.foto_color)=0 then
                    existe_incidencia                  :=finserta_incidencia(reg,20097,
                    'Error en el fichero de imagen 1: En el Boletin N?: '||reg.boletin||
                    ', El Fichero de la foto a color (Fichero1: '||reg.fichero1||
                    ' ), no esta disponible.','DATO OBLIGATORIO') ;
                
                end if;
                
                --if dbms_lob.getlength(reg.foto_lectura)=0 then
                --    existe_incidencia                    :=finserta_incidencia(reg,20098,
                --    'Error en el fichero de imagen 2: En el Boletin N?: '||reg.boletin||
                --    ', El Fichero de la foto de Lectura (Fichero2: '||reg.fichero2||
                --    ' ), no esta disponible.','DATO OBLIGATORIO') ;
                
                --end if;
                --  If dbms_lob.getlength(Reg.Boletin_PDF) = 0  Then
                --          Existe_incidencia := FInserta_Incidencia    (Reg, 20099,    '
                -- Error en el fichero de imagen 3: En el Boletin N?: '||Reg.Boletin||', El
                -- Fichero .pdf del boletin (Fichero3: '||Reg.Fichero3||' ), no esta
                -- disponible',
                --                              'DATO OBLIGATORIO');
                --  End If;
                ----        If Not Existe_incidencia Then
                ----            Update Alba.MultaCar_Blob MC    Set Foto_Color =
                -- Reg.Foto_Color, Foto_Lectura = Reg.Foto_Lectura, Boletin_PDF =
                -- Reg.Boletin_PDF, Fichero_Otros = Reg.Fichero_Otros
                ----            Where  Boletin  = Reg.Boletin And TipoBoletin =
                -- Reg.TipoBoletin And AnnoBoletin = Reg.AnnoBoletin;
                ----            Existe_incidencia := FALSE; -- La colocamos a False y
                -- continua el Loop.
                ----        End If;
            
            else -- El registro es de tipo 'BB', 'ZA' o 'RA' y trae alguna foto o
                -- documento, la graba.
                
                if reg.fichero1   is not null and dbms_lob.getlength(reg.foto_color)=0 then
                    existe_incidencia:=finserta_incidencia(reg,20097,
                    'Error en el fichero de imagen 1: En el Boletin N?: '||reg.boletin||
                    ', El Fichero de la foto a color (Fichero1: '||reg.fichero1||
                    ' ), no esta disponible.','DATO OBLIGATORIO') ;
                
                end if;
                
                if reg.fichero2   is not null and dbms_lob.getlength(reg.foto_lectura)=0 then
                    existe_incidencia:=finserta_incidencia(reg,20098,
                    'Error en el fichero de imagen 2: En el Boletin N?: '||reg.boletin||
                    ', El Fichero de la foto de Lectura (Fichero2: '||reg.fichero2||
                    ' ), no esta disponible.','DATO OBLIGATORIO') ;
                
                end if;
                
                if reg.fichero3   is not null and dbms_lob.getlength(reg.boletin_pdf)=0 then
                    existe_incidencia:=finserta_incidencia(reg,20097,
                    'Error en el fichero de imagen 3: En el Boletin N?: '||reg.boletin||
                    ', El Fichero del boletin .pdf (Fichero3: '||reg.fichero3||
                    ' ), no esta disponible.','DATO OBLIGATORIO') ;
                
                end if;
                
                if reg.fichero4 is not null and dbms_lob.getlength(reg.fichero_otros)=0
                    then
                    existe_incidencia:=finserta_incidencia(reg,20098,
                    'Error en el fichero de imagen 4: En el Boletin N?: '||reg.boletin||
                    ', El Fichero de libre disposicion (Fichero4: '||reg.fichero4||
                    ' ), no esta disponible.','DATO OBLIGATORIO') ;
                
                end if;
                ----        If Not Existe_incidencia Then
                ----            If Reg.Fichero1 Is Not Null Or Reg.Fichero2 Is Not Null Or
                -- Reg.Fichero3 Is Not Null Or Reg.Fichero4 Is Not Null Then
                ----                Update Alba.MultaCar_Blob MC    Set Foto_Color =
                -- Reg.Foto_Color, Foto_Lectura = Reg.Foto_Lectura, Boletin_PDF =
                -- Reg.Boletin_PDF, Fichero_Otros = Reg.Fichero_Otros
                ----                Where  Boletin  = Reg.Boletin And TipoBoletin =
                -- Reg.TipoBoletin And AnnoBoletin = Reg.AnnoBoletin;
                ----                Existe_incidencia := FALSE; -- La colocamos a False y
                -- continua el Loop.
                ----            End If;
                ----        End If;
            
            end if;
            
            if reg.tipoboletin<>'BB' then
                -- AGR, valida que no sean las mismas fotos.
                
                if reg.fichero1 is not null and reg.fichero2 is not null then
                    
                    if dbms_lob.compare(reg.foto_color,reg.foto_lectura)=0 then
                        existe_incidencia                                 :=finserta_incidencia(
                        reg,20096,'Error en los ficheros de imagen: En el Boletin N?: '||
                        reg.boletin||
                        ', El Fichero foto_color y el de foto_lectura son identicos (Fichero1-Fichero2).'
                        ,'FUNCIONAL DE CARGA TXT') ;
                    
                    end if;
                
                end if;
                
                if reg.fichero1 is not null and reg.fichero3 is not null then
                    
                    if dbms_lob.compare(reg.foto_color,reg.boletin_pdf)=0 then
                        existe_incidencia                                :=finserta_incidencia(
                        reg,20096,'Error en los ficheros de imagen: En el Boletin N?: '||
                        reg.boletin||
                        ', El Fichero foto_color y el de boletin_pdf son identicos (Fichero1-Fichero3).'
                        ,'FUNCIONAL DE CARGA TXT') ;
                    
                    end if;
                
                end if;
                
                if reg.fichero1 is not null and reg.fichero4 is not null then
                    
                    if dbms_lob.compare(reg.foto_color,reg.fichero_otros)=0 then
                        existe_incidencia                                  :=finserta_incidencia(
                        reg,20096,'Error en los ficheros de imagen: En el Boletin N?: '||
                        reg.boletin||
                        ', El Fichero foto_color y el de libre disposicion son identicos (Fichero1-Fichero4).'
                        ,'FUNCIONAL DE CARGA TXT') ;
                    
                    end if;
                
                end if;
                -- Fichero2
                
                if reg.fichero2 is not null and reg.fichero3 is not null then
                    
                    if dbms_lob.compare(reg.foto_lectura,reg.boletin_pdf)=0 then
                        existe_incidencia                                  :=finserta_incidencia(
                        reg,20096,'Error en los ficheros de imagen: En el Boletin N?: '||
                        reg.boletin||
                        ', El Fichero foto_lectura y el de foto_lectura son identicos (Fichero2-Fichero3).'
                        ,'FUNCIONAL DE CARGA TXT') ;
                    
                    end if;
                
                end if;
                
                if reg.fichero2 is not null and reg.fichero4 is not null then
                    
                    if dbms_lob.compare(reg.foto_lectura,reg.fichero_otros)=0 then
                        existe_incidencia                                    :=
                        finserta_incidencia(reg,20096,
                        'Error en los ficheros de imagen: En el Boletin N?: '||reg.boletin||
                        ', El Fichero foto_lectura y el de de libre disposicion son identicos (Fichero2-Fichero4).'
                        ,'FUNCIONAL DE CARGA TXT') ;
                    
                    end if;
                
                end if;
                -- Fichero3
                
                if reg.fichero3 is not null and reg.fichero4 is not null then
                    
                    if dbms_lob.compare(reg.boletin_pdf,reg.fichero_otros)=0 then
                        existe_incidencia                                   :=finserta_incidencia
                        (reg,20096,'Error en los ficheros de imagen: En el Boletin N?: '||
                        reg.boletin||
                        ', El Fichero boletin_pdf y el de de libre disposicion son identicos (Fichero3-Fichero4).'
                        ,'FUNCIONAL DE CARGA TXT') ;
                    
                    end if;
                
                end if;
                -- AGR, Fin validacion.
            
            end if;
            
            if not existe_incidencia then
                
                if reg.fichero1           is not null or reg.fichero2 is not null or reg.fichero3 is
                    not null or reg.fichero4 is not null then
                    
                    update alba.multacar_blob mc
                    set foto_color =reg.foto_color,
                        foto_lectura  =reg.foto_lectura,
                        boletin_pdf   =reg.boletin_pdf,
                        fichero_otros =reg.fichero_otros
                    where boletin  =reg.boletin
                    and tipoboletin=decode(reg.tipoboletin,'H','CH','R','SR',reg.tipoboletin)
                        ; -- Este Decode se podra eliminar el dia que se modifique el ctl de la
                    -- carga.
                    -- And AnnoBoletin = Reg.AnnoBoletin;
                    existe_incidencia:=false; -- La colocamos a False y continua el Loop.
                
                end if;
            
            end if;
            --  End If;
            
    
        
    
    exception
        
        when others then
            ptratamiento_errores(
            'Existen Errores al intentar cargar los ficheros de imagen de las fotografias. Referencia: '
            ||reg.boletin ||' - '||reg.tipoboletin ||' - '||reg.annoboletin,
            'Mul_Boletines_Externos.PCarga_Img_Multacar -Loop-',20007) ;
        
        end;
    
    end loop;
  

exception

when others then
    ptratamiento_errores(
    'Existen Errores al intentar cargar los ficheros de imagen de las fotografias.'
    ,'Mul_Boletines_Externos.PCarga_Img_Multacar',20008) ;
    */
    
    existe_incidencia:=false;      

end pcarga_img_multacar;

--------------------------------------------------------------------------------
procedure pgrabo_maestra_multacar(
        pnomfich        varchar2,
        ppath           varchar2,
        pfecha_carga    date,
        pdescr          varchar2,
        pfichero_bad    varchar2,
        pfichero_dsc    varchar2,
        pestado_boletin varchar2,
        pestado_exp     varchar2)
is
    faltan_parametros exception;
    tincidencias      number;
    tcorrectos        number;
    tdescr            varchar2(2000):=nvl(pdescr,
    'Carga de Fichero no especificada') ;
    tcuantos_exp   number;
    tcuantas_fotos number;
begin
    
    if pnomfich is null then
        raise faltan_parametros;
    
    end if;
    
    if substr(pnomfich,1,1)='Z' then
        tdescr               :='Carga de Boletines de Zona Azul';
    
    elsif substr(pnomfich,1,1)='D' then
        tdescr                  :='Carga de Boletines de Disciplina Vial';
    
    elsif substr(pnomfich,1,1)='B' then
        tdescr                  :='Carga de Boletines de Camaras Carril Bus';
    
    elsif substr(pnomfich,1,1)='H' then
        tdescr                  :='Carga de Boletines de Camaras Centro Historico';
    
    elsif substr(pnomfich,1,1)='T' then
        tdescr                  :='Carga de Boletines de Trafico (BlackBerry)';
    
    elsif substr(pnomfich,1,1)='R' then
        tdescr                  :='Carga de Ratificaciones de Controladores Aussa';
    
    end if;
    
    if substr(pnomfich,1,1)='R' and pestado_boletin='CARGADOS' and pestado_exp=
        'GENERADOS' then -- Si es una carga de Doc. de ratificacion ZA, se marca la
        -- carga en la tabla multacar.
        
        update albaadm.multacar
        set est_procesado='CARGADO' -- Se comprueba ademas que sea CARGADOS y
            -- GENERADOS.
        where nomfich_cargado=upper(pnomfich) ;
        commit;
        --        PDescarga_ExpYFotos (PNomFich, TPkg_UsuIdMod, TCuantos_Exp,
        -- TCuantas_Fotos);
    
    end if;
    
    select count(1)
    into tincidencias
    from alba.incid_txt_multacar
    where upper(nomfich_cargado)=upper(pnomfich)
    and tipo_incidencia        <>'NO_INVALIDA_CARGA';
    
    select count(1)
    into tcorrectos
    from alba.multacar
    where upper(nomfich_cargado)=upper(pnomfich) ;
    
    insert
    into alba.maestra_carga
        (
            nomfich_cargado,
            path,
            fecha_carga,
            total_fichero,
            total_correctos,
            total_incidencias,
            descr,
            fichero_bad,
            fichero_dsc,
            estado_boletin,
            estado_exp
        )
        values
        (
            pnomfich,
            ppath,
            sysdate,
            tincidencias+tcorrectos,
            tcorrectos,
            tincidencias,
            tdescr,
            pfichero_bad,
            pfichero_dsc,
            pestado_boletin,
            pestado_exp
        ) ;

exception

when dup_val_on_index then
    
    update alba.maestra_carga
    set total_fichero           =nvl(tincidencias+tcorrectos,total_fichero),
        total_correctos            =nvl(tcorrectos,total_correctos),
        total_incidencias          =nvl(tincidencias,total_incidencias),
        fichero_bad                =nvl(pfichero_bad,fichero_bad),
        fichero_dsc                =nvl(pfichero_dsc,fichero_dsc),
        estado_boletin             =nvl(pestado_boletin,estado_boletin),
        estado_exp                 =nvl(pestado_exp,estado_exp)
    where upper(nomfich_cargado)=upper(pnomfich) ;

when faltan_parametros then
    ptratamiento_errores(
    'Faltan parametros al intentar grabar la carga del fichero, por favor, avise al Dep. Informatica.'
    ,'Mul_Boletines_Externos.PGrabo_Maestra_MultaCar',20003) ;

when others then
    ptratamiento_errores(
    'Se ha producido un error al grabar los datos de fichero de carga, por favor, avise al Dep. Informatica.'
    ,'Mul_Boletines_Externos.PGrabo_Maestra_MultaCar',20013) ;

end pgrabo_maestra_multacar;

--------------------------------------------------------------------------------
procedure pelimina_carga(
        pnomfich varchar2)
is
    faltan_parametros exception;
begin
    
    if pnomfich is null then
        raise faltan_parametros;
    
    end if;
    
    /*delete
    from alba.multacar
    where upper(nomfich_cargado)=upper(pnomfich) ;
    
    delete
    from alba.multacar_blob
    where upper(nomfich_cargado)=upper(pnomfich) ;
    
    delete
    from alba.incid_txt_multacar
    where upper(nomfich_cargado)=upper(pnomfich) ;
    
    delete
    from alba.maestra_carga
    where upper(nomfich_cargado)=upper(pnomfich) ;
    
    delete
    from alba.tarea_programada
    where upper(descr) like '%'
        ||upper(pnomfich)
        ||'%';*/
    --    Commit;

exception

when faltan_parametros then
    ptratamiento_errores(
    'Faltan parametros al intentar eliminar los datos de la carga, por favor, avise al Dep. Informatica.'
    ,'Mul_Boletines_Externos.PElimina_Carga',20004) ;

end pelimina_carga;

--------------------------------------------------------------------------------
function ffichero_cargado_sn(
        pnomfich_cargado varchar2)
    return varchar2
is
    testado_boletin varchar2(200) ;
begin
    
    select estado_boletin
    into testado_boletin
    from alba.maestra_carga
    where upper(nomfich_cargado)=upper(pnomfich_cargado) ;
    
    return testado_boletin;

exception

when too_many_rows then
    ptratamiento_errores(
    'El Fichero se ha cargado mas de una vez, por favor, avise al Dep. Informatica.'
    ,'Mul_Boletines_Externos.FFichero_cargado_SN',20002) ;

end ffichero_cargado_sn;
-- AGR, Devuelve un Id de Vehiculo y el ID de expediente de IVTM, si existe
-- para la fecha de la denuncia.

--------------------------------------------------------------------------------
function fbuscomatri_en_ivtm(
        pmatricula     varchar2,
        pfechadenuncia date default sysdate)
    return ty_id_vehexp
is
    tdev_ids ty_id_vehexp;-- := Ty_Id_VehExp (Null,Null,Null,Null);
    --  TDev_Ids            b_agr_kk := b_agr_kk(Null,Null,Null,Null);
begin
    
    --    TDev_Ids.vehid := Null;  TDev_Ids.ExpId := Null; TDev_Ids.ExpCod := Null
    -- ; TDev_Ids.MexId := Null;
    
    select veh.vehid,
        veh.seoid,
        exp.expid,
        exp.expcod,
        exp.mexid
    into tdev_ids.vehid,
        tdev_ids.seoid,
        tdev_ids.expid,
        tdev_ids.expcod,
        tdev_ids.mexid -- AGR, se puede escribir Into TDev_Ids sin mas ..., pero
        -- prefiero asignar uno a uno los valores.
    from alba.vehiculos_d veh,
        alba.seobjetos seo,
        alba.expingre exp
    where veh.seoid     =seo.seoid
    and seo.expid       =exp.expid
    and exp.mexid       =2 -- El valor '2' => Que la exaccion es de IVTM.
    and veh.vehversion in
        (
            select max(v.vehversion)
            from alba.vehiculos_d v
            where veh.seoid=v.seoid
        )
    and veh.vehmatri=pmatricula --     -- Parametro
    and pfechadenuncia between exp.expfalta and nvl(exp.expfbaja,sysdate+1)
    and rownum=rownum;
    
    return tdev_ids;

exception

when no_data_found then
    
    return tdev_ids;

when too_many_rows then
    
    return tdev_ids; -- Si encuentra mas de un expediente de IVTM para esa fecha-
    -- hora, hay un problema en Alba, Ej: 9540FJS.

when others then
    ptratamiento_errores(
    'Error al intentar obtener los datos de la matricula, por favor, avise al Dep. Informatica. Matricula-FechaDenuncia: '
    ||pmatricula||'-'||pfechadenuncia,'Mul_Boletines_Externos.FBuscoMatri_en_IVTM'
    ,20018) ;

end fbuscomatri_en_ivtm;

--------------------------------------------------------------------------------
function fsolicito_datos_dgt(
        pmatricula      varchar2,
        pmarcavehiculo  varchar2,
        ptipovehiculo   varchar2,
        pmodelovehiculo varchar2)
    return number
is
    nveces    number;
    tvemid    number;
    tmulmarca varchar2(200) ;
    tmmtid    number;
begin
    
    select alba.vehiculos_mult_sq.nextval
    into tvemid
    from dual;
    
    tmulmarca:=substr(trim(pmarcavehiculo) || ' ' || ptipovehiculo || ' ' ||trim(
    pmodelovehiculo),1,100) ;
    
    insert
    into alba.vehiculos_mult
        (
            vemid,
            vemmatri,
            mas_mmtid,
            vemfhoramod,
            usuidmod,
            vemmotivo,
            vemmarca
        )
        values
        (
            tvemid,
            pmatricula,
            null,
            sysdate,
            nvl(tpkg_usuidmod,0),
            'Carga MultaCar',
            trim(tmulmarca)
        ) ;
    
    return tvemid;

exception

when dup_val_on_index then
    
    select vemid
    into tvemid
    from alba.vehiculos_mult
    where vemmatri=pmatricula;
    
    return tvemid;

when others then
    ptratamiento_errores('Error al Insertar en VEHICULOS_MULT, Matricula: '||
    pmatricula,'Mul_Boletines_Externos.FSolicito_Datos_DGT',20019) ;

end fsolicito_datos_dgt;

--------------------------------------------------------------------------------
function fgrabo_expediente(
        pexpcod        varchar2,
        pmexid         number,
        pfechadenuncia date)
    return number
is
    --  TExpCod Varchar2(20);
    texpid number;
begin
    
    --     PRCNUEVOCODIGOEXPINGRE(PMexId, To_Number(To_Char(PFechaDenuncia,'RRRR')
    -- ), TPkg_UsuIdMod, TPkg_MotivoMod , TExpCod);
    --     Commit;
    
    select alba.expingre_sq.nextval
    into texpid
    from dual;
    
    insert
    into alba.expingre
        (
            expid,
            expcod,
            expfalta,
            expfdecl,
            expfbaja,
            mexid,
            expfhoramod,
            usuidmod,
            expmotivo
        )
        values
        (
            texpid,
            pexpcod,
            pfechadenuncia,
            pfechadenuncia,
            null,
            pmexid,
            sysdate,
            tpkg_usuidmod,
            tpkg_motivomod
        ) ;
    
    return texpid;

end fgrabo_expediente;

--------------------------------------------------------------------------------
function fbusca_contrib_exaccion
    (
        pperid      number,
        pperversion number,
        pmexid      number
    )
    return number
is
    tconid number;
begin
    
    select max(conid)
    into tconid
    from alba.contribuyentes
    where perid   =pperid
    and mexid     =pmexid
    and perversion=pperversion;
    
    return tconid;

exception

when no_data_found then
    
    return null;

end fbusca_contrib_exaccion;

--------------------------------------------------------------------------------
function fasocio_contrib_exp(
        pexpid        number,
        pmexid        number,
        pperid        number,
        pperversion   number,
        ptipo_contrib varchar2)
    return number
is
    tconid number;
    teicid number;
    tdecid number;
begin
    tconid:=fbusca_contrib_exaccion(pperid,pperversion,pmexid) ;
    
    if tconid is null then -- Si no encuentra la persona-Exaccion la da de alta.
        
        select alba.contribuyentes_sq.nextval
        into tconid
        from dual;
        
        insert
        into alba.contribuyentes
            (
                conid,
                perid,
                mexid,
                perversion,
                confhoramod,
                usuidmod,
                conmotivo
            )
            --          Values                  (ExpedientesIngresosContr_Sq.NextVal
            -- ,PPerId ,PMexId ,PPerVersion    ,SysDate        ,TPkg_UsuIdMod  ,
            -- TPkg_MotivoMod  );
            values
            (
                tconid,
                pperid,
                pmexid,
                pperversion,
                sysdate,
                tpkg_usuidmod,
                tpkg_motivomod
            ) ;
    
    end if;
    
    select alba.expedientesingresoscontr_sq.nextval
    into teicid
    from dual;
    
    select decid
    into tdecid
    from table(alba.utl_alba.deco('TIPOCONTRIB',ptipo_contrib)) ;
    
    insert
    into alba.expedientesingresoscontr
        (
            eicid,
            conid,
            expid,
            eicxtipocontribuyente,
            eicobli,
            eicfhoramod,
            usuidmod,
            eicmotivo
        )
        values
        (
            teicid,
            tconid,
            pexpid,
            tdecid,
            'N',
            sysdate,
            nvl(tpkg_usuidmod,0),
            tpkg_motivomod
        ) ;
    
    return teicid;

exception

when others then
    ptratamiento_errores(
    'Error al asociar expedientes y contribuyentes, Id. expediente: '|| pexpid ||
    'Id. Persona-version: '|| pperid||'-'||pperversion,
    'Mul_Boletines_Externos.FAsocio_Contrib_Exp',20022) ;

end fasocio_contrib_exp;

--------------------------------------------------------------------------------
function fasocio_direcc_exp
    (
        peicid      number,
        prdiid      number,
        ptipo_direc varchar2
    )
    return number
is
    ttedid number;
    tdecid number;
    tfiaid number;
begin
    
    select alba.direccionesexpedientes_sq.nextval
    into ttedid
    from dual;
    
    select decid
    into tdecid
    from table(alba.utl_alba.deco('TIPODIREC',ptipo_direc)) ;
    
    select fiaid
    into tfiaid
    from alba.fiabilidad
    where fiacod='1'; -- FiaCod = 1 => 'ALTA'
    
    insert
    into alba.direccionesexpedientes
        (
            tedid,
            rdiid,
            eicid,
            tedxtipodireccion,
            fiaid,
            tedfhoramod,
            usuidmod,
            tedmotivo
        )
        values
        (
            ttedid,
            prdiid,
            peicid,
            tdecid,
            tfiaid,
            sysdate,
            tpkg_usuidmod,
            tpkg_motivomod
        ) ;
    
    return ttedid;

exception

when others then
    ptratamiento_errores(
    'Error al asociar expedientes y direcciones, Id. ExpedienteIngresoContribuyente: '
    || peicid ||'Id. Direccion: '|| prdiid,
    'Mul_Boletines_Externos.FAsocio_Direcc_exp',20015) ;

end fasocio_direcc_exp;

--------------------------------------------------------------------------------
function fcrea_objeto_alba
    (
        pexpid number
    )
    return number
is
    tseoid number;
begin
    
    select alba.seobjetos_sq.nextval
    into tseoid
    from dual;
    
    insert
    into alba.seobjetos
        (
            seoid,
            expid,
            seofhoramod,
            usuidmod,
            seomotivo
        )
        values
        (
            tseoid,
            pexpid,
            sysdate,
            tpkg_usuidmod,
            tpkg_motivomod
        ) ;
    
    return tseoid;

end fcrea_objeto_alba;

--------------------------------------------------------------------------------
function fgrabo_multa
    (
        pcursor_multacar c_multacar%rowtype,
        pnuevo_seoid number,
        pnuevo_expid number,
        pid_vehexpivtm ty_id_vehexp,
        pvemid number
    )
    return number
is
    tmulid alba.multas.mulid%type;
    tdecid_tipoboletinco number;
    tesmid alba.escritosmultas.esmid%type;
    tesmxtipo alba.escritosmultas.esmxtipo%type;
    tesmfescrito alba.escritosmultas.esmfescrito%type;
    reg c_multacar%rowtype;
    -- Datos Vehiculo.
    ttvtmid number;
    tvehmarca alba.vehiculos_d.vehmarca%type;
    tvehbasti alba.vehiculos_d.vehbasti%type;
    tvehxcolor alba.vehiculos_d.vehxcolor%type;
    tinfgrado alba.infracciones.infgrado%type;
    tinfimporte alba.infracciones.infimporte%type;
    tmuldenuncia alba.multas.muldenuncia%type;
    --  TDecId                  Number;
    dummymail varchar2(500) ;
begin
    reg:=pcursor_multacar;
    
    select alba.multas_sq.nextval
    into tmulid
    from dual;
    begin
        
        select decid
        into tdecid_tipoboletinco
        from table(albaadm.utl_alba.deco('BOLETIN',reg.tipoboletin)) ;
    
    exception
    
    when no_data_found then
        begin
            
            select usuemail
            into dummymail
            from alba.usuarios
            where usuid=tpkg_usuidmod;
        
        exception
        
        when others then
            null;
        
        end;
        --albaadm.Utl_alba.PEnvia_Mail    (PDe => Null, PPara => 'jgomezm@sevilla.org
        -- ', PAsunto => 'Error al generar expediente de multas.', PMensaje => 'El
        -- tipo de boletin -'||Reg.TipoBoletin||'-, no está dado de alta',
        -- PNombre_Fichero => '',
        --                                   PCc => 'Lourdes.Almendral@tecnocom.es',
        -- PBcc => Null );
        ptratamiento_errores('El tipo de boletin -'||reg.tipoboletin||
        '- no esta dado de alta en el sistema: Avise al Dep. Informática, boletin: '
        ||reg.boletin||'-'||reg.tipoboletin||'-'||reg.annoboletin,
        'Mul_Boletines_Externos.Carga_Bol_MultaCar',20019) ;
    
    end;
    --              Select DecId Into T From table(Utl_alba.Deco('GRUA'      ,'G1'
    -- ));
    -- Otros datos: De la Infraccion.
    begin
        
        select decode(infpatron,'',infdescripcion,'1',infdescripcion,infpatron),
            infgrado,
            infimporte
        into tmuldenuncia,
            tinfgrado,
            tinfimporte
        from alba.infracciones
        where infid=reg.infid;
    
    exception
    
    when no_data_found then
        ptratamiento_errores('El Id. de Infraccion: '|| reg.infid ||
        ' no esta cargado, avise al responsable. boletin: '||reg.boletin||'-'||
        reg.tipoboletin||'-'||reg.annoboletin,
        'Mul_Boletines_Externos.Carga_Bol_MultaCar',20005) ;
    
    when others then
        ptratamiento_errores('El Id. de Infraccion: '|| reg.infid ||
        ' no esta cargado, avise al responsable. boletin: '||reg.boletin||'-'||
        reg.tipoboletin||'-'||reg.annoboletin,
        'Mul_Boletines_Externos.Carga_Bol_MultaCar',20017) ;
    
    end;
    
    if pid_vehexpivtm.expid is not null then
        
        -- Otros datos: Del vehiculo.
        
        select tvi.tvtmid,
            veh.vehmarca,
            veh.vehbasti,
            veh.vehxcolor
        into ttvtmid,
            tvehmarca,
            tvehbasti,
            tvehxcolor
        from alba.vehiculos_d veh,
            alba.tivdgt_m tvi
        where veh.vehid=pid_vehexpivtm.vehid
        and veh.tvtmid =tvi.tvtmid;
    
    end if;
    
    --      dbms_output.put_line (Reg.CodMotivoNoNotificacion);
    --      Select DecId Into TDecId From Table (alba.utl_alba.deco('MOTIVO', 5));
    -- Siempre es "otros". ver decodificadora. Funcionalmente obliga a buscar en
    -- la columna multas.MotivoNoNotificacion
    -- Si tienen IVTM.
    
    insert
    into alba.multas
        (
            mulid,
            infid,
            mulsituinfra,
            multipobolet,
            mulsitutrami -- 1
            ,
            mulnotestado,
            viaid,
            mullugar,
            mulfrente,
            muldelante -- 2
            ,
            mulanobol,
            mulxboletin,
            mulnumbol,
            mulversion,
            seoidexp -- 3
            ,
            expidexp,
            seoidveh,
            expidveh,
            vemid,
            tvtmid -- 4
            ,
            mulfec,
            mulhor,
            mulmin,
            mulfirma,
            mulmarca -- 5
            ,
            mulbasti,
            mulxcolor,
            mulatestado,
            mulatefecresol,
            mulxateorginformante-- 6
            ,
            mulrec,
            mulimp,
            muldenuncia,
            mulnotmano,
            mulxmotivo -- 7
            ,
            mulxgrua,
            mulxmedida1,
            mulxmedida2,
            mulxmedida3,
            mulxmedida4 -- 8
            ,
            mulresmedida1,
            mulresmedida2,
            mulresmedida3,
            mulresmedida4,
            mulobservaciones -- 9
            ,
            mulretirada,
            mulobsdir,
            mulpermiso,
            mulfecper,
            mulclase --10
            ,
            mulfecnac,
            mulexpperm,
            age1_ageid,
            mulunidad1,
            mulpresencio1 --11
            ,
            age2_ageid,
            mulunidad2,
            mulpresencio2,
            muldtodeposito,
            mulimpdeposito --12
            ,
            mulfecgrab,
            mulfeciniprod,
            mulfecprescrip,
            mulfeccaducidad,
            mulfecrequer --13
            ,
            mulfecfinplcomcon,
            mulfecnotden,
            mulfecsancion,
            mulfecsanfir,
            mulfecejec --14
            ,
            mulfeccob,
            mulfecalegden,
            mulfecrecsan,
            mulfecanula,
            mulfecarch --15
            ,
            mulfdesde,
            mulfhasta,
            mulmiembrofse,
            mulfoto,
            mulpruebas --16
            ,
            mulfhoramod,
            usuidmod,
            mulmotivo,
            mulfvigencia,
            mulfecnotifcond --17
            ,
            mulmatri,
            descnonotif,
            boletin2 --18
        )
        values
        (
            tmulid,
            reg.infid,
            'D',
            'N',
            'N',
            'P',
            reg.viaid,
            null,
            reg.frentea,
            reg.delantede,
            reg.annoboletin,
            tdecid_tipoboletinco,
            reg.boletin,
            0,
            pnuevo_seoid,
            pnuevo_expid,
            pid_vehexpivtm.seoid,
            pid_vehexpivtm.expid,
            pvemid,
            ttvtmid,
            trunc(reg.fechadenuncia),
            substr(reg.horadenuncia,1,2),
            substr(reg.horadenuncia,4,2),
            'N',
            substr(nvl(tvehmarca,trim(reg.marcavehiculo)
            || ' '
            || reg.tipovehiculo
            || ' '
            ||trim(reg.modelovehiculo)),1,99),
            tvehbasti,
            tvehxcolor,
            null,
            null,
            null,
            null,
            tinfimporte,
            nvl(reg.descripcionhd,tmuldenuncia),
            reg.notificadaenmano,
            reg.codmotivononotificacion -- 190, Siempre es "otros". ver decodificadora.
            -- Funcionalmente obliga a buscar en la columna multas.MotivoNoNotificacion
            ,
            null,
            reg.mulxmedida1,
            reg.mulxmedida2,
            reg.mulxmedida3,
            reg.mulxmedida4,
            reg.mulresmedida1,
            reg.mulresmedida2,
            reg.mulresmedida3,
            reg.mulresmedida4,
            replace(reg.observacioneshd,'@',' '
            ||chr(13)
            ||chr(10)),
            null,
            null,
            null,
            null,
            null,
            null,
            null,
            reg.ageid,
            null,
            'N',
            null,
            null,
            'N',
            'N',
            null,
            pkg_sysdate,
            trunc(reg.fechadenuncia),
            null,
            null,
            null,
            null,
            null,
            null,
            null,
            null,
            null,
            null,
            null,
            null,
            null,
            pkg_sysdate,
            null,
            null,
            'N',
            'N',
            pkg_sysdate,
            tpkg_usuidmod,
            tpkg_motivomod,
            null,
            null,
            reg.matricula,
            reg.motivononotificacion,
            reg.boletin2
        ) ;
    begin -- AGR, Compruebo si existen escritos para el boletin.
        
        for kk in
        (
            select esmid,
                esmxtipo,
                esmfescrito
            into tesmid,
                tesmxtipo,
                tesmfescrito
            from alba.escritosmultas
            where esmnumbol=reg.boletin
            and mulid     is null
        )
        loop
            
            update alba.escritosmultas
            set mulid      =tmulid
            where esmnumbol=reg.boletin;
        
        end loop;
    
    exception
    
    when no_data_found then
        tesmid      :=null;
        tesmxtipo   :=null;
        tesmfescrito:=null;
    
    when others then
        ptratamiento_errores('Error',
        'Mul_Boletines_Externos.FGrabo_Multa: "Boletin: '||reg.boletin||' A?o: '||
        reg.annoboletin||' Tipo Boletin: '||tdecid_tipoboletinco||'"',20016) ;
    
    end;
    
    return tmulid;

exception

when others then
    ptratamiento_errores('Error','Mul_Boletines_Externos.FGrabo_Multa: "Boletin: '
    ||reg.boletin||' A?o: '||reg.annoboletin||' Tipo Boletin: '||
    tdecid_tipoboletinco||'"',20006) ;

end fgrabo_multa;
-- albades.pkg_cargaIVTM.pEnvia_Mail(remitente, receptor, asunto, mensaje, host
-- => 88.84.68.3)

--------------------------------------------------------------------------------
function fdev_maestra_carga(
        pnomfich_cargado varchar2)
    return alba.maestra_carga%rowtype
is
    tmaestra_carga alba.maestra_carga%rowtype;
begin
    
    select*
    into tmaestra_carga
    from alba.maestra_carga
    where upper(nomfich_cargado)=upper(pnomfich_cargado) ;
    
    return tmaestra_carga;

exception

when no_data_found then
    null;

end fdev_maestra_carga;

--------------------------------------------------------------------------------
function fbuscainsertapersona(
        pnif    varchar2,
        pnombre varchar2)
    return ty_idpersona
is
    tidpersona ty_idpersona;
    trazonsocial  varchar2(200):=pnombre;
    tpostoken1    number;
    tpostoken2    number;
    tnombre       varchar2(200) ;
    tapellido1    varchar2(200) ;
    tapellido2    varchar2(200) ;
    tperxorigen   number;
    tperxtipoiden number;
    nif_vacio     exception;
    nombre_vacio  exception;
begin
    
    if pnif is not null then
        -- AGR, Desmenuza el nombre y los apellidos de la persona.
        tpostoken1:=instr(pnombre,'@') ;
        
        if tpostoken1<>0 then
            
            select decode(instr(pnombre,'@',instr(pnombre,'@')+1),0,length(pnombre)+1 --
                -- Si es cero, el nombre no tiene mas '@' y tomo la posicion del final de
                -- la frase+1.
                ,instr(pnombre,'@',instr(pnombre,'@')+1)) -- Si no es cero el segundo
                -- apellido esta a partir de la posicion PosToken2.
            into tpostoken2
            from dual;
            
            tnombre   :=replace(substr(pnombre,1,tpostoken1-1),'@',' ') ;
            tapellido1:=replace(substr(pnombre,tpostoken1  +1,abs(tpostoken2-tpostoken1)
                                                           -1),'@',' ') ;
            tapellido2  :=replace(substr(pnombre,tpostoken2  +1),'@',' ') ;
            trazonsocial:=null;
        
        end if;
        -- AGR, Fin.
        begin
            
            -- AGR, Busca por si ya esta dado de alta.
            
            select per1.perid,
                per1.perversion
            into tidpersona
            from alba.personas per1
            where per1.periden=pnif
            and per1.perid    =
                (
                    select max(per2.perid)
                    from alba.personas per2
                    where per2.periden=per1.periden
                )
            and per1.perversion    =
                (
                    select max(per2.perversion)
                    from alba.personas per2
                    where per2.periden=per1.periden
                )                
            and pernombre=tnombre
            and perapell1=tapellido1
            and perapell2=tapellido2;
        
        exception
        
        when no_data_found then -- Si no encuentra Inserta si viene el nombre.
            
            if pnombre is not null then
                
                select decid
                into tperxtipoiden
                from table(albaadm.utl_alba.deco('PERXTIPOIDEN','NIF')) ;
                
                select decid
                into tperxorigen
                from table(albaadm.utl_alba.deco('ORIGENPERSON','ALTAMULTAS')) ;
                
                select alba.personas_sq.nextval,
                    1
                into tidpersona
                from dual;
                
                insert
                into alba.personas a
                    (
                        a.perid,
                        a.perversion,
                        a.tpeid,
                        a.fiaid,
                        a.perunico,
                        a.pernombre,
                        a.perpart1,
                        a.perapell1,
                        a.perpart2,
                        a.perapell2,
                        a.perrazon,
                        a.peranagrama,
                        a.pernomval,
                        a.periden,
                        a.peridenext,
                        a.perxtipoiden,
                        a.peridenval,
                        a.perxorigen,
                        a.perfhoramod,
                        a.usuidmod,
                        a.permotivo,
                        a.perfvigencia,
                        a.permigraperiden,
                        a.permigraperapelnomcorto,
                        a.tcoid,
                        a.perfallido,
                        a.perorigendep,
                        a.percategoria
                    )
                    values
                    (
                        tidpersona.perid,
                        tidpersona.perversion,
                        2,
                        5,
                        1,
                        tnombre,
                        null,
                        tapellido1,
                        null,
                        tapellido2,
                        trazonsocial,
                        null,
                        null,
                        pnif,
                        null,
                        tperxtipoiden,
                        'S',
                        tperxorigen,
                        sysdate,
                        tpkg_usuidmod,
                        'Multa de Trafico, Notif. en mano',
                        null,
                        null,
                        null,
                        null,
                        'N',
                        null,
                        null
                    ) ;
            
            else
                raise nombre_vacio;
            
            end if;
        
        end;
    
    else
        tidpersona.perid     :=null;
        tidpersona.perversion:=null;
        raise nif_vacio;
    
    end if;
    
    return tidpersona;

exception

when nif_vacio then
    ptratamiento_errores(
    'Error, El Nif de la persona esta vacio, es imposible darlo de alta.',
    'Mul_Boletines_Externos.FBuscaInsertaPersona',20020) ;

when nombre_vacio then
    ptratamiento_errores(
    'Error, El Nif de la persona no esta dado de alta en sistema, y el alta automatica falla porque el nombre esta vacio.'
    ,'Mul_Boletines_Externos.FBuscaInsertaPersona',20015) ;

when others then
    ptratamiento_errores('Error, al buscar personas, DNI: '||pnif,
    'Mul_Boletines_Externos.FBuscaInsertaPersona',20021) ;

end fbuscainsertapersona;

--------------------------------------------------------------------------------
function buscainsertaprovincia
    (
        pprovinciaconductor varchar2
    )
    return number
is
    tmprid number;
begin
    
    select mprid
    into tmprid
    from alba.provincia p
    where mprdesc=pprovinciaconductor
        --    And mprId = ( Select Max(p2.MprId) From alba.provincia  p2
        --                  Where    p2.mprdesc = PProvinciaConductor)
    and rownum=1;
    
    return tmprid;

exception

when no_data_found then
    
    select albaadm.provincia_sq.nextval
    into tmprid
    from dual;
    
    insert
    into alba.provincia a
        (
            a.mprid,
            a.mccid,
            a.mpaid,
            a.mprcodigo,
            a.mprdesc,
            a.mprfhoramod,
            a.usuidmod,
            a.mprmotivo,
            a.mprfvigencia,
            a.mprmostrable
        )
        values
        (
            tmprid,
            null,
            9,
            to_char(tmprid),
            upper(pprovinciaconductor),
            sysdate,
            0,
            'Alta Multacar',
            null,
            'V'
        ) ;
    
    return tmprid;

end buscainsertaprovincia;

--------------------------------------------------------------------------------
function buscainsertaprovinciamunic
    (
        plocalidadconductor varchar2,
        pprovinciaconductor varchar2 default null
    )
    return number
is
    tmunid number;
    tmprid number;
begin
    -- Si la provincia viene por parametro el MprId debe ser el introducido por
    -- parametro, no el vinculado al municipio.
    
    if pprovinciaconductor is not null then
        tmprid                :=buscainsertaprovincia(pprovinciaconductor) ; -- Sino
        -- existe la crea. Siempre devuelve un MprId      11
    
    end if;
    
    -- Fin.
    
    select munid,
        mprid
    into tmunid,
        tmprid
    from alba.municipios m
    where m.mundesc=plocalidadconductor
        --    And m.munId = ( Select Max(m2.MunId) From alba.Municipios  m2
        --                          Where    m2.mundesc = PLocalidadConductor
        --                          And m2.Mprid       = Nvl(TMprId,m2.Mprid)
        --                          And m2.MunId not in (83612,106567) )
    and m.mprid      =nvl(tmprid,m.mprid)
    and m.munid not in(83612,106567)
    and rownum       =1;
    
    return tmunid;

exception

when no_data_found then
    
    select alba.municipios_sq.nextval
    into tmunid
    from dual;
    
    insert
    into alba.municipios a
        (
            a.munid,
            a.mprid,
            a.muncodigo,
            a.mundigitodecontrol,
            a.muncun,
            a.mundesc,
            a.mundes50,
            a.mundescor,
            a.munfhoramod,
            a.usuidmod,
            a.munmotivo,
            a.munfvigencia,
            a.munmarca600,
            a.muncodcatastro
        )
        values
        (
            tmunid,
            tmprid,
            '000',
            '0',
            null,
            substr(upper(plocalidadconductor),1,70),
            substr(upper(plocalidadconductor),1,50),
            substr(upper(plocalidadconductor),1,25),
            sysdate,
            0,
            'Alta Multacar',
            null,
            null,
            null
        ) ;
    
    return tmunid;

end buscainsertaprovinciamunic;

--------------------------------------------------------------------------------
function fbuscainsertadirecc
    (
        pdomicilioconductor varchar2,
        pcpconductor        varchar2,
        plocalidadconductor varchar2,
        pprovinciaconductor varchar2
    )
    return number
is
    -- AGR, Variables para la insercion del acceso.
    taccid         number;
    tviaidgenerica number;
    taccxori       number;
    tcodid         number;
    taccdirnoest   varchar2(200):=trim(pdomicilioconductor||' '||
    plocalidadconductor||' '||pcpconductor) ;
    -- AGR, Variables para la insercion de la direccion.
    trdiid number;
    tmunid number;
    tviaid number;
  
  error_des varchar2(300);  
  
begin
    -- AGR, Si vienen los datos completos del conductor
    
  error_des:='comprueba_nulo_domicilio_conductor';
  
    if pdomicilioconductor is not null and
        (
            (
                plocalidadconductor is not null and pprovinciaconductor is not null
            )
            or pcpconductor is not null
        )
        then
        tmunid:=buscainsertaprovinciamunic(plocalidadconductor,pprovinciaconductor) ;
        --101588
        begin -- Busco la Via que viene como domicilio del conductor, basandome en el
            -- municipio TmunId.
            
      error_des:='inserta_vía';
      
            select viaid
            into tviaid
            from albaadm.vias
            where viadesc=substr(pdomicilioconductor,1,200) -- La busco tambien a traves
                -- de su descripocion exacta.
            and viacodigo in
                (
                    select min(viacodigo)
                    from albaadm.vias
                    where viadesc=substr(pdomicilioconductor,1,200)
                )
            and munid =tmunid ---------------------
            and rownum=1;
        
        exception
        
        when no_data_found then
            
            error_des:='inserta_nueva_vía';
      
      select albaadm.vias_sq.nextval
            into tviaid
            from dual;
            
            insert
            into albaadm.vias
                (
                    viaid,
                    tviid,
                    munid,
                    viacodigo,
                    viadesc,
                    viadescorta,
                    viafhoramod,
                    usuidmod,
                    viamotivo,
                    viafvigencia
                )
                values
                (
                    tviaid,
                    290,
                    tmunid,
                    '0000N',
                    substr(pdomicilioconductor,1,200),
                    substr(pdomicilioconductor,1,200),
                    sysdate,
                    0,
                    'Alta Multacar.',
                    null
                ) ;
        
        end;
        tcodid:=buscarcodpostal(plocalidadconductor,pcpconductor) ; -- AGR, Busca es
        -- Id. del codigo postal y sino lo inserta.
        
        select decid
        into taccxori
        from table(albaadm.utl_alba.deco('ORIGENACCESO','MANUAL')) ;
        
        select alba.accesos_sq.nextval
        into taccid
        from dual;
    
    error_des:='inserta_accesos';
        
        insert
        into alba.accesos
            (
                accid,
                viaid,
                accnum1,
                accinum1,
                accnum2,
                accinum2,
                acckm1,
                accxorigen,
                accfhoramod,
                usuidmod,
                accmotivo,
                accdirnoest,
                codid
            )
            values
            (
                taccid,
                tviaid,
                null,
                null,
                null,
                null,
                null,
                taccxori,
                sysdate,
                tpkg_usuidmod,
                tpkg_motivomod,
                null,
                tcodid
            ) ;
        
        select alba.direccion_sq.nextval
        into trdiid
        from dual;
        
    error_des:='inserta_dirección';
    
        insert
        into alba.direccion
            (
                rdiid,
                accid,
                rdicodigo,
                rdixpor,
                rdixpiso,
                rdixpuerta,
                rdifhoramod,
                usuidmod,
                rdimotivo
            )
            values
            (
                trdiid,
                taccid,
                '-',
                null,
                null,
                null,
                sysdate,
                tpkg_usuidmod,
                tpkg_motivomod
            ) ;
    
    else
        trdiid:=null;
    
    end if;
    
    return trdiid;

exception  
when others then

  dbms_output.put_line('KO. ' || SQLERRM);
  dbms_output.put_line('LOCALIZACIÓN ERROR: <' || error_des ||'>');
  
  return 0;


end fbuscainsertadirecc;

--------------------------------------------------------------------------------
function prepara
    return number
is
begin
    null;

end prepara;


--------------------------------------------------------------------------------
-- Carga_Bol_MultaCar
--------------------------------------------------------------------------------
procedure pcarga_bol_multacar
    (
        pfichero     varchar2,
        pusuidmod    varchar2,
        pmulmotivo   varchar2,
        ptipoboletin varchar2
    )
is
    tmexid albaadm.exacciones.mexid%type;
    tmulxmotivo albaadm.multas.mulxmotivo%type;
    regid_vehexpivtm ty_id_vehexp;
    tid_contrib alba.obj_alba.ty_id_contrib;
    tid_direcc alba.obj_alba.ty_id_direcc;
    tvemid       number;
    tnuevo_expid number;
    tnuevo_seoid number;
    teicid       number;
    ttedid       number;
    tmulid       number;
    tidpersona ty_idpersona;
    trdiid number;
    -- Utilizo contadores pero quizas sea mejor realizar recuentos al final del
    -- proceso.
    denuncias_a_la_dgt number:=0;
    errores_en_datos   number:=0;
    tcuantos           number:=0;
    i                  number:=1; -- Inicio a 1 porque son posiciones de una
    -- tabla.
    tablacodexp alba.obj_alba.tyt_expcod:=alba.obj_alba.tyt_expcod(null,null) ;
    ttipoboletin                 varchar2(2) ;
    desactivar_busqueda_vehiculo varchar2(2) ; -- AGR sus valores son 'S'/'N'.
    tnotificadaenmano            varchar2(2) ;
    ndummy                       number;
  error_des varchar2(1000);
begin
    
    if ptipoboletin is null then
        begin
    
      error_des:='tipo_boletin';
            
            select tipoboletin
            into ttipoboletin
            from alba.multacar
            where nomfich_cargado=pfichero
            group by tipoboletin;
        
        exception
        
        when too_many_rows then
            ptratamiento_errores(
            'Error, no se pueden cargar en una misma carga de fichero dos tipos de boletines distintos, Avise a Dep. Informatica: '
            ,' MUL_Crea_Jobs.PCarga_Bol_MultaCar',20025) ;
        
        when no_data_found then
            ptratamiento_errores('Error, No Existen datos para el fichero de carga: '||
            pfichero||', Avise a Dep. Informatica: ',
            ' MUL_Crea_Jobs.PCarga_Bol_MultaCar',20026) ;
        
        end;
    
    end if;
    --depuracion.kk;-- eliminar
    pkg_sysdate  :=sysdate;
    tpkg_usuidmod:=pusuidmod;
    
  error_des:='tipo_exacción';
  
    select mexid
    into tmexid
    from alba.exacciones
    where mexcodigo='MULTAS'; -- Tipo de exaccion
    
    --      Select DecId Into TDecId_SujetoPasivo   From table(utl.deco('
    -- TIPOCONTRIB'   ,'SUJETOPASIVO' ));
    
  error_des:='parámetro_general';
    select parid
    into tmulxmotivo
    from alba.parametrosgeneralesapl
    where parcodigo='MULCONDAUS';
    
  error_des:='cálculo_total_expedientes';
    select count(1)
    into tcuantos
    from alba.multacar
    where est_procesado='PENDIENTE'
    and nomfich_cargado=pfichero
    and tipoboletin    =ptipoboletin; -- Obtiene el numero de expedientes que se
    -- van a generar.
    tablacodexp.extend(tcuantos) ;
    tablacodexp:=alba.obj_alba.freserva_codexp(tmexid,tcuantos) ; -- Obtiene
    -- codigos de expediente de una exaccion determinada almacenandolos en una
    -- tabla.
    i:=1; -- Vuelvo a Inicializar a 1 porque la utilizo para indicar posiciones de
    -- tabla.
    
    error_des:='parámetro_general_buscar en IVTM';
  select parvalor
    into desactivar_busqueda_vehiculo
    from alba.parametrosgeneralesapl
    where parcodigo='BUSQVEHICULO'; -- AGR, 18/03/2011, parametriza si el proceso
    -- debe buscar en IVTM o no.
    
    for reg in c_multacar(pfichero,ptipoboletin)
    loop
        
        -- AGR, Coloco a Null, los valores de la direccion y datos del contribuyente.
        
    error_des:='tipo_contribuyente';
        select null,
            null,
            null,
            null,
            null,
            null
        into tid_contrib
        from dual;
        
        error_des:='tipo_dirección';
    select null,
            null,
            null,
            null,
            null
        into tid_direcc
        from dual;
        
    
    error_des:='analiza_datos_empresa';
        tvemid               :=null;
        teicid               :=null;
        ttedid               :=null;
        tidpersona.perid     :=null;
        tidpersona.perversion:=null;
        trdiid               :=null;
        -- AGR,20/02/2012 En los casos en los que nos mandan datos de empresa, en vez
        -- de datos de conductor no se tendrán en cuenta; ni la notificación, ni los
        -- datos enviados de la empresa.
        tnotificadaenmano:=reg.notificadaenmano;
        
    
    
        if substr(reg.nifconductor,1,1)<>'X' then
            begin
                ndummy:=to_number(substr(reg.nifconductor,1,1)) ;
            
            exception
            
            when others then
                tnotificadaenmano:='N';
            
            end;
        
        end if;
        -- Fin 20/02/2012.
        -- AGR, Buscamos el vehiculo en Expedientes de IVTM abiertos, durante el dia
        -- de la denuncia.
        -- AGR, 18/03/2011, deja de buscarse al titular en IVTM, (supongo que lo
        -- buscara directamente en DGT), en caso de que se active este parametro.
        
    error_des:='desactivar_busca_vehículos';
        if desactivar_busqueda_vehiculo='N' then
            regid_vehexpivtm             :=fbuscomatri_en_ivtm(reg.matricula,to_date(
            to_char(reg.fechadenuncia,'DD/MM/YYYY') ||' '||reg.horadenuncia,
            'DD/MM/RRRR hh24:mi')) ;
        
        else
            regid_vehexpivtm.expid:=null; -- Cargando el numero de expediente a nulo, no
            -- intentara buscarlo en IVTM.
        
        end if;
        -- AGR, 18/03/2011, Fin.
        -- AGR, OBTENCION DE DATOS DE TITULAR-CONDUCTOR Y DIRECCIONES.
        -- AGR, Comienzo a grabar datos del expediente y si se puede los datos del
        -- titular-conductor y sus direcciones.
    error_des:='obtención_titular_conductor';
        tnuevo_expid:=fgrabo_expediente(tablacodexp(i) .expcod,tmexid,
        reg.fechadenuncia) ; -- Grabo Expediente, y obtengo un ExpId.
        
    error_des:='analiza_notificación_en_mano';
        if tnotificadaenmano='S' then -- AGR, Si es notificada en mano, se supone que
            -- deben venir los datos del conductor,  ni busca en IVTM ni en DGT, toma
            -- los datos del boletin.
            -- AGR, Se intenta obtener los datos del conductor cargados en el boletin.
      error_des:='inserta_persona';
            tidpersona:=fbuscainsertapersona(reg.nifconductor,reg.nombreconductor) ;
      error_des:='inserta_dirección';
      error_des:=error_des || ' BOLETÍN: <' || reg.boletin ||'>';
      error_des:=error_des || ' DOMICILIO: <' || reg.domicilioconductor ||'>';
      error_des:=error_des || ' CP: <' || reg.cpconductor ||'>';
      error_des:=error_des || ' LOCALIDAD: <' || reg.localidadconductor ||'>';
      error_des:=error_des || ' PROVINCIA: <' || reg.provinciaconductor ||'>';
      
            trdiid    :=fbuscainsertadirecc(reg.domicilioconductor,reg.cpconductor,
            reg.localidadconductor,reg.provinciaconductor) ;
            
      error_des:='analiza_datos_conductor';
            if tidpersona.perid is not null then -- Hay datos del conductor.
        error_des:='asocia_contribuyente';
                teicid             :=fasocio_contrib_exp(tnuevo_expid,tmexid,
                tidpersona.perid,tidpersona.perversion,ptipo_contrib => 'CONDUCTOR') ;
                
                update alba.multacar
                set est_procesado='PROCESADO'
                where boletin    =reg.boletin
                and tipoboletin  =reg.tipoboletin
                and annoboletin  =reg.annoboletin;
            
            end if;
            
      error_des:='analiza_dirección_conductor';
            if trdiid is not null and teicid is not null then
        error_des:='asocia_dirección_expediente';
                ttedid   :=fasocio_direcc_exp(teicid,trdiid,ptipo_direc => 'NOTIFICACION')
                ;
                
                update alba.multacar
                set est_procesado='PROCESADO'
                where boletin    =reg.boletin
                and tipoboletin  =reg.tipoboletin
                and annoboletin  =reg.annoboletin;
            
            end if;
            --                If TIDPersona.PerId Is Null Or TRdiId Is Null Or TEicId Is
            -- Null Then
            -- AGR, No se puede encontrar al titular del vehiculo, se grabara para
            -- solicitar datos a la DGT desde la matricula.
      error_des:='solicita_datos_dgt';
            tvemid:=fsolicito_datos_dgt(reg.matricula,reg.marcavehiculo,reg.tipovehiculo
            ,reg.modelovehiculo) ; -- Se graban los datos del vehiculo grabados por el
            -- agente.
            --                    Update Alba.MultaCar Set Est_procesado = 'OJO PDTE.
            -- DGT'  Where Boletin = Reg.Boletin And TipoBoletin = Reg.TipoBoletin And
            -- AnnoBoletin = Reg.AnnoBoletin;
            --                    Denuncias_a_La_DGT := Denuncias_a_La_DGT + 1;   --
            -- Contador de denuncias que quedan pendientes de completar datos por la
            -- DGT.
            --                End If;
            --          End If;
      
        error_des:='analiza_datos_conductor_IVTM';
        elsif regid_vehexpivtm.expid is not null then -- Si encuentra expediente IVTM
            -- , en Alba.-- Desde que se activo la NO búsqueda en IVTM, éste Elsif no se
            -- ejecuta. parametro =  BUSQVEHICULO
            -- AGR, Obtenemos algunos datos necesarios de persona y direccion.
            begin
                
                -- Buscamos a sujeto pasivo del expediente de IVTM.
                error_des:='busca_sujeto_pasivo_IVTM';
                select tipocontrib,
                    perid,
                    perversion,
                    mexid,
                    conid,
                    eicid
                into tid_contrib
                from table(alba.obj_alba.fbusca_contrib_exp(regid_vehexpivtm.expid,
                    'SUJETOPASIVO')) tpcon; -- AGR, Obtiene a la persona 'SUJETOPASIVO' del
                -- expediente.
                
                -- Buscamos la direccion de notificacion del del expediente de IVTM..
                error_des:='busca_dirección_notif';
                select tipocontrib,
                    tipodirec,
                    rdiid,
                    accid,
                    viaid
                into tid_direcc
                from table(alba.obj_alba.fbusca_direcc_exp(regid_vehexpivtm.expid,
                    'NOTIFICACION','SUJETOPASIVO')) tpdir
                where tipocontrib='SUJETOPASIVO'
                and tipodirec    ='NOTIFICACION'; -- AGR, Obtiene la direccion de '
                -- NOTIFICACION' del expediente.
                teicid:=fasocio_contrib_exp(tnuevo_expid,tmexid,tid_contrib.perid,
                tid_contrib.perversion,ptipo_contrib => 'TITULAR') ;
                ttedid:=fasocio_direcc_exp(teicid,tid_direcc.rdiid,ptipo_direc =>
                'NOTIFICACION') ;
                
                update alba.multacar
                set est_procesado='PROCESADO'
                where boletin    =reg.boletin
                and tipoboletin  =reg.tipoboletin
                and annoboletin  =reg.annoboletin;
            
            exception
            
            when no_data_found then
                tvemid:=fsolicito_datos_dgt(reg.matricula,reg.marcavehiculo,
                reg.tipovehiculo,reg.modelovehiculo) ;
                
                update alba.multacar
                set est_procesado='PROCESADO PDTE. DGT'
                where boletin    =reg.boletin
                and tipoboletin  =reg.tipoboletin
                and annoboletin  =reg.annoboletin;
                
                denuncias_a_la_dgt:=denuncias_a_la_dgt+1; -- Contador de denuncias que
                -- quedan pendientes de completar datos por la DGT.
            
            when others then
                errores_en_datos:=errores_en_datos+1;
                
                update alba.multacar
                set est_procesado='CON_ERRORES'
                where boletin    =reg.boletin
                and tipoboletin  =reg.tipoboletin
                and annoboletin  =reg.annoboletin;
                
                ptratamiento_errores('Error, con el expediente :'||regid_vehexpivtm.expcod
                ||', por favor, avise al Dep. Informatica.',
                'Mul_Boletines_Externos.Carga_Bol_MultaCar',20014) ;
            
            end;
        
        else
            -- AGR, No se puede encontrar al titular del vehiculo, se grabara para
            -- solicitar datos a la DGT desde la matricula.
            tvemid:=fsolicito_datos_dgt(reg.matricula,reg.marcavehiculo,reg.tipovehiculo
            ,reg.modelovehiculo) ;
            
            update alba.multacar
            set est_procesado='PROCESADO PDTE. DGT'
            where boletin    =reg.boletin
            and tipoboletin  =reg.tipoboletin
            and annoboletin  =reg.annoboletin;
            
            denuncias_a_la_dgt:=denuncias_a_la_dgt+1; -- Contador de denuncias que
            -- quedan pendientes de completar datos por la DGT.
        
        end if;
        -- AGR, FIN OBTENCIoN DE DATOS DE TITULAR-CONDUCTOR Y DIRECCIONES.
        -- Comprobamos si el primer denunciante es agente o no.
        
        if reg.ageid is null then -- La denuncia NO es de un agente. Hay que grabar a
            -- los denunciantes.
            teicid:=fasocio_contrib_exp(tnuevo_expid,tmexid,reg.perid_denunc1,
            reg.perversion_denunc1,ptipo_contrib => 'DENUNCIANTE') ;
            -- No es necesario almacenar la direccion del denunciante 1.
            
            if reg.perid_denunc2 is not null then
                teicid              :=fasocio_contrib_exp(tnuevo_expid,tmexid,
                reg.perid_denunc2,reg.perversion_denunc2,ptipo_contrib => 'DENUNCIANTE2') ;
                -- No es necesario almacenar la direccion del denunciante 2.
            
            end if;
        
        end if;
        -- Creo_Objeto;                 If PId_VehExpIVTM.EXPID Is Not Null Then
        tnuevo_seoid:=fcrea_objeto_alba(tnuevo_expid) ;
        tmulid      :=fgrabo_multa(reg,tnuevo_seoid,tnuevo_expid,regid_vehexpivtm,
        tvemid) ;
        i:=i+1; -- Avanzo una posicion en la tabla de codigos de expediente, (CodExp)
        -- .
    
    end loop;

exception

when others then

  dbms_output.put_line('KO. ' || SQLERRM);
  dbms_output.put_line('LOCALIZACIÓN ERROR: <' || error_des ||'>');

    ptratamiento_errores('Error, Avise a Dep. Informatica AMR.',
    'depdep.Mul_Boletines_Externos.PCarga_Bol_MultaCar',20012) ;

end pcarga_bol_multacar;
-- Agr, para arreglar cosas.

--------------------------------------------------------------------------------
procedure arregla_exp_sin_denunciante
is
    
    cursor c
    is
        
        select*
        from alba.multacar
        where nomfich_cargado='T2009000001.TXT'
        and notificadaenmano ='S';
    
    tidpersona mul_boletines_externos.ty_idpersona;
    trdiid             number;
    teicid             number;
    tnuevo_expid       number;
    tmexid             number;
    tvemid             number;
    ttedid             number;
    denuncias_a_la_dgt number:=0;
    problemas          exception;
begin
    
    for reg in c
    loop
        --            Select Null,Null,Null,Null,Null,Null    Into TId_Contrib
        -- From Dual;
        --            Select Null,Null,Null,Null              Into TId_Direcc
        -- From Dual;
        tvemid               :=null;
        teicid               :=null;
        ttedid               :=null;
        tidpersona.perid     :=null;
        tidpersona.perversion:=null;
        trdiid               :=null;
        
        select expidexp
        into tnuevo_expid
        from alba.multas
        where mulnumbol=reg.boletin;
        
        select mexid
        into tmexid
        from alba.exacciones
        where mexcodigo='MULTAS'; -- Tipo de exaccion
        ----                If Reg.NotificadaEnMano = 'S' Then-- AGR, Si es
        -- notificada en mano, se supone que deben venir los datos del conductor.
        -- AGR, Se intenta obtener los datos del conductor cargados en el boletin.
        tidpersona:=fbuscainsertapersona(reg.nifconductor,reg.nombreconductor) ;
        trdiid    :=fbuscainsertadirecc(reg.domicilioconductor,reg.cpconductor,
        reg.localidadconductor,reg.provinciaconductor) ;
        
        if tidpersona.perid is not null then -- Hay datos del conductor.
            teicid             :=fasocio_contrib_exp(tnuevo_expid,tmexid,
            tidpersona.perid,tidpersona.perversion,ptipo_contrib => 'CONDUCTOR') ;
            
            update alba.multacar
            set est_procesado='PROCESADO'
            where boletin    =reg.boletin
            and tipoboletin  =reg.tipoboletin
            and annoboletin  =reg.annoboletin;
        
        elsif trdiid is not null then
            ttedid      :=fasocio_direcc_exp(teicid,trdiid,ptipo_direc => 'NOTIFICACION'
            ) ;
            
            update alba.multacar
            set est_procesado='PROCESADO'
            where boletin    =reg.boletin
            and tipoboletin  =reg.tipoboletin
            and annoboletin  =reg.annoboletin;
        
        else
            raise problemas;
        
        end if;
        ----            End If;
    
    end loop;

exception

when problemas then
    ptratamiento_errores('Error problemas con la direccion o el conductor.',
    'Mul_Boletines_Externos.arregla_exp_sin_denunciante',20011) ;

end arregla_exp_sin_denunciante;
-- fin agr, para arreglar cosas.

--------------------------------------------------------------------------------
function fdev_incidencias(
        pfichero varchar2)
    return tyt_incidencia pipelined
is
begin
    
    for reg in
    (
        select codincid,
            incidencia,
            tipo_incidencia -- replace(Incidencia,chr(10),'@')
        from alba.incid_txt_multacar
        where nomfich_cargado=pfichero
    )
    loop
        pipe row(reg) ;
    
    end loop;
    
    return;

end fdev_incidencias;

--------------------------------------------------------------------------------
procedure pcorreo_errores(
        pfichero  varchar2,
        pusuidmod number)
is
    tfichero_bad       varchar2(255) ;
    tfichero_dsc       varchar2(255) ;
    ttotal_incidencias number;
    ttotal_fichero     number;
    tcontador_lineas   number:=0;
    tasunto            varchar2(2000) ;
    tpara clob ;
    tcc clob;
    tbcc clob ;
    ttxtbody clob;
    ttxtpie clob:=chr(13) ||
    'Se ha descartado toda la carga, por favor corrija las incidencias e intente cargar de nuevo.'
    ;
    ttipoboletin varchar2(2000) ;
begin
    
    select total_fichero,
        total_incidencias,
        fichero_bad,
        fichero_dsc
    into ttotal_fichero,
        ttotal_incidencias,
        tfichero_bad,
        tfichero_dsc
    from alba.maestra_carga
    where nomfich_cargado=pfichero;
    
    if ttotal_incidencias<>0 and tfichero_bad is null and tfichero_dsc is null
        then -- Sólo se enviará correo Si existen incidencias de error de carga.
        tpara:=albaadm.utl_alba.femailusuario(pusuidmod) ;
        begin
            
            select decode(tipoboletin,'CO','Disciplina Vial','BB','BlacBerry','ZA',
                'Zona Azul','RA','Ratificacion de Controlador ZA','H',
                'Camaras Centro Historico','CH','Camaras Centro Historico')
            into ttipoboletin
            from albaadm.multacar
            where upper(nomfich_cargado)=upper(pfichero)
            and rownum                  =1; -- Solo es necesario un registro puesto que
            -- cada carga es de un unico tipo.
        
        exception
        
        when no_data_found then
            ttipoboletin:=null;
        
        end;
        tasunto:='- '||to_char(ttotal_incidencias) ||' Incidencas - en: '||upper(
        pfichero) ||' de "'||ttipoboletin||'".';
        ttxtbody:='Se han detectado '||to_char(ttotal_incidencias) ||
        ' incidencias en el fichero de carga de "'||ttipoboletin||
        '", de un total de '||to_char(ttotal_fichero) ||', el fichero de carga es: '
        ||upper(pfichero) ||'.'||chr(13) ||chr(13) ||chr(13) ;
        tcontador_lineas:=1; -- Considero la primera linea.
        ttxtbody        :=ttxtbody||
        '                                 - Descripcion del Problema -'||chr(13) ||
        chr(13) ;
        
        for reg in
        (
            select incidencia
            from table(albaadm.mul_boletines_externos.fdev_incidencias(pfichero))
            where tipo_incidencia<>'NO_INVALIDA_CARGA'
        )
        loop
            ttxtbody:=ttxtbody||reg.incidencia||chr(13) ;
        
        end loop;
        --albaadm.utl_ALBA.PEnvia_Mail (PDe => Null, PPara => TPara, PAsunto =>
        -- TAsunto, PMensaje => TTxtBody||TtxtPie, PNombre_Fichero => '',
        --                               PCc => 'Lourdes.Almendral@tecnocom.es', PBcc
        -- => 'jgomezm@sevilla.org');
        -- En caso en la carga tambien aparezca errores de formato en el fichero de
        -- carga, es decir, *.bad y *.dsc, se genera otro correo.
        
        if tfichero_bad is not null or tfichero_dsc is not null then
            tasunto        :='Error en el Formato del Fichero de carga.';
            ttxtbody       :='Existen errores de Formato en el Fichero de carga"'||
            ttipoboletin||'", Avise a Departamento Informatica de la AMR.'||chr(13) ||
            chr(13) ;
            ttxtbody:=ttxtbody ||'Fichero/s de error: '|| tfichero_bad || '   ' ||
            tfichero_dsc||chr(13) ||chr(13) ;
            ttxtbody:=ttxtbody ||
            'Por favor, revise tambien el Fichero de *.log, de la carga.'||chr(13) ||chr
            (13) ;
            --albaadm.utl_ALBA.PEnvia_Mail (   PDe => Null, PPara => TPara, PAsunto =>
            -- TAsunto, PMensaje => TTxtBody||TtxtPie, PNombre_Fichero => '',
            --                                 PCc => 'Lourdes.Almendral@tecnocom.es' ,
            -- PBcc => 'jgomezm@sevilla.org'   );
        
        end if;
    
    end if;

exception

when others then
    ptratamiento_errores(
    'Error al intentar enviar el correo electronico. Avise a Dep. Informatica AMR.'
    ,'Mul_Boletines_Externos.PCorreo_Errores',20011) ;

end pcorreo_errores;

--------------------------------------------------------------------------------
procedure pcorreo_para_tramite(
        pfichero  varchar2,
        pusuidmod number)
is
    tfichero_bad       varchar2(255) ;
    tfichero_dsc       varchar2(255) ;
    ttotal_incidencias number;
    ttotal_fichero     number;
    tcontador_lineas   number:=0;
    tasunto            varchar2(2000) ;
    tpara clob ;
    tcc clob;
    tbcc clob ;
    ttxtbody clob;
    ttxtpie clob:=chr(13) ||'Para Dep. Multas.';
    ttipoboletin        varchar2(2000) ;
    tno_invalidan_carga varchar2(2) ;
    ttotal_a_tramite    number;
begin
    
    select total_fichero,
        total_incidencias,
        fichero_bad,
        fichero_dsc
    into ttotal_fichero,
        ttotal_incidencias,
        tfichero_bad,
        tfichero_dsc
    from alba.maestra_carga
    where nomfich_cargado=pfichero;
    
    select decode(count(1),0,'No','Si'),
        count(1)
    into tno_invalidan_carga,
        ttotal_a_tramite -- Bucamos si existen, incidencias que no invalidan la carga
        -- , pero si se debe informar a tramitación.
    from alba.incid_txt_multacar
    where nomfich_cargado=pfichero
    and tipo_incidencia  ='NO_INVALIDA_CARGA';
    
    if ttotal_incidencias=0 and tno_invalidan_carga='Si' and tfichero_bad is null
        and tfichero_dsc   is null then -- Sólo se enviará en correo a tramitación en
        -- caso de que no tenga incidencias.
        tpara:=albaadm.utl_alba.femailusuario(pusuidmod) ;
        begin
            
            select decode(tipoboletin,'CO','Disciplina Vial','BB','BlacBerry','ZA',
                'Zona Azul','RA','Ratificacion de Controlador ZA','H',
                'Camaras Centro Historico','CH','Camaras Centro Historico')
            into ttipoboletin
            from albaadm.multacar
            where upper(nomfich_cargado)=upper(pfichero)
            and rownum                  =1; -- Solo es necesario un registro puesto que
            -- cada carga es de un unico tipo.
        
        exception
        
        when no_data_found then
            ttipoboletin:=null;
        
        end;
        tasunto:='A trámite - '||to_char(ttotal_a_tramite) ||' boletines de '||
        ttipoboletin||' carga: "'||upper(pfichero) ||'".';
        ttxtbody:='Se han detectado '||to_char(ttotal_a_tramite) ||' boletines de "'
        ||ttipoboletin||
        '" para un trámite de corrección, el Nº total de boletines a completar es '||
        to_char(ttotal_a_tramite) ||' , el fichero de carga es: '||upper(pfichero) ||
        '.'||chr(13) ||chr(13) ||chr(13) ;
        tcontador_lineas:=1; -- Considero la primera linea.
        ttxtbody        :=ttxtbody||
        '                                 - Descripcion del trámite -'||chr(13) ||chr
        (13) ;
        
        for reg in
        (
            select incidencia
            from table(mul_boletines_externos.fdev_incidencias(pfichero))
            where tipo_incidencia='NO_INVALIDA_CARGA'
        )
        loop
            ttxtbody:=ttxtbody||reg.incidencia||chr(13) ;
        
        end loop;
        --albaadm.utl_ALBA.PEnvia_Mail (PDe => Null, PPara => TPara, PAsunto =>
        -- TAsunto, PMensaje => TTxtBody||TtxtPie, PNombre_Fichero => '',
        --                               PCc => 'Lourdes.Almendral@tecnocom.es', PBcc
        -- => 'jgomezm@sevilla.org');
    
    end if;

exception

when others then
    ptratamiento_errores(
    'Error al intentar enviar el correo electronico. Avise a Dep. Informatica AMR.'
    ,'Mul_Boletines_Externos.PCorreo_Para_tramite',20011) ;

end pcorreo_para_tramite;

--------------------------------------------------------------------------------
procedure pcorreo_incidencias(
        pfichero  varchar2,
        pusuidmod number)
is
begin
    pcorreo_errores(pfichero,pusuidmod) ;

end pcorreo_incidencias;
------  Procedure PDescarga_ExpYFotos (PFDesde Date, PFHasta Date, PCuantos_Exp
-- Out Number, PCuantas_Fotos Out Number) Is
------      Cursor CDescarga (PFDesde Date, PFHasta Date) Is
------          Select  img.tipoboletin, img.boletin, img.annoboletin,
-- tab.nexpediente, tab.nifd, tab.nombred,
------                  img.fichero1, img.fichero2, img.fichero3, img.fichero4,
------                  img.tipoboletin||'?'||img.boletin||'?'||img.annoboletin
-- ||'?'||tab.nexpediente||'?'||tab.nifd||'?'||tab.nombred||'?'||
------                  Decode(img.fichero1,'','',tab.nexpediente||'_Fichero1.'
-- ||Substr(img.fichero1,Instr(img.fichero1,'.')+1))||'?'||
------                  Decode(img.fichero2,'','',tab.nexpediente||'_Fichero2.'
-- ||Substr(img.fichero1,Instr(img.fichero1,'.')+1))||'?'||
------                  Decode(img.fichero3,'','',tab.nexpediente||'_Fichero3.'
-- ||Substr(img.fichero1,Instr(img.fichero1,'.')+1))||'?'||
------                  Decode(img.fichero4,'','',tab.nexpediente||'_Fichero4.'
-- ||Substr(img.fichero1,Instr(img.fichero1,'.')+1))||chr(13)||chr(10)
-- CADENA_FICHERO,
------                  Decode(img.fichero1,'','',tab.nexpediente||'_Fichero1.'
-- ||Substr(img.fichero1,Instr(img.fichero1,'.')+1))
-- NOM_FICHERO1,
------                  img.foto_color,
------                  Decode(img.fichero2,'','',tab.nexpediente||'_Fichero2.'
-- ||Substr(img.fichero1,Instr(img.fichero1,'.')+1))
-- NOM_FICHERO2,
------                  img.foto_lectura,
------                  Decode(img.fichero3,'','',tab.nexpediente||'_Fichero3.'
-- ||Substr(img.fichero1,Instr(img.fichero1,'.')+1))
-- NOM_FICHERO3,
------                  img.boletin_pdf,
------                  Decode(img.fichero4,'','',tab.nexpediente||'_Fichero4.'
-- ||Substr(img.fichero1,Instr(img.fichero1,'.')+1))
-- NOM_FICHERO4,
------                  img.fichero_otros
------            From alba.vm_multasestados mulx, alba.Multacar_blob img,
-- Table (trml.dmultas(Mulx.mulid,0)) tab
------              Where mulx.mulfecgrab Between PFDesde And PFHasta
------              And img.boletin     = mulx.mulnumbol
------              And img.tipoboletin = Decode(mulx.origenDenuncia,1,'BB',2,'
-- ZA',0,'CO')
------              And img.annoboletin = Substr(mulx.expediente,1,4);
------
------      TPath           Varchar2(1000) := '/MultasLob/out/';
------      TFichero_txt    Clob;
------      Tlista_Zip      Utl.TyLista_Ficheros := Utl.TyLista_Ficheros() ;
------      i           Number := 1;
------      TBody       Varchar2(2000);
------      TSubject    Varchar2(2000);
------      Dummy       Number;
------      Error       Varchar2(2000);
------      TNombreFichero_Txt Varchar2(200) := To_Char(Sysdate,'
-- RRRR_MM_DD_HH24_MI_SS')||'.txt';
------      TNombreFichero_Zip Varchar2(200) := To_Char(Sysdate,'
-- RRRR_MM_DD_HH24_MI_SS')||'.zip';
------  Begin
------      PCuantos_Exp    := 0;
------      PCuantas_Fotos  := 0;
------      For Reg In CDescarga (PFDesde, PFHasta) Loop
------          If Reg.fichero1 is not null Then
------              Utl_Lob.BlobToFichero(TPath||Reg.Nom_fichero1,
-- Reg.foto_color);
------              Tlista_Zip.Extend;
------              Tlista_Zip(i) := TPath||Reg.Nom_fichero1;
------              i := i+1;
------          End If;
------          If Reg.fichero2 is not null Then
------              Utl_Lob.BlobToFichero(TPath||Reg.Nom_fichero2,
-- Reg.foto_lectura);
------              Tlista_Zip.Extend;
------              Tlista_Zip(i) := TPath||Reg.Nom_fichero2;
------              i := i+1;
------          End If;
------          If Reg.fichero3 is not null Then
------              Utl_Lob.BlobToFichero(TPath||Reg.Nom_fichero3,
-- Reg.boletin_pdf);
------              Tlista_Zip.Extend;
------              Tlista_Zip(i) := TPath||Reg.Nom_fichero3;
------              i := i+1;
------          End If;
------          If Reg.fichero4 is not null Then
------              Utl_Lob.BlobToFichero(TPath||Reg.Nom_fichero4,
-- Reg.fichero_otros);
------              Tlista_Zip.Extend;
------              Tlista_Zip(i) := TPath||Reg.Nom_fichero3;
------              i := i+1;
------          End If;
------          TFichero_txt    := TFichero_txt || Reg.Cadena_Fichero;
------          PCuantos_Exp := PCuantos_Exp +1;
------      End Loop;
------          Tlista_Zip.Extend;
------          Tlista_Zip(i) := TPath||TNombreFichero_Txt;     -- A?ade el
-- fichero de texto que contiene el indice.
------
------          PCuantas_Fotos := i-1; -- Del bucle siempre se sale con una
-- vuelta de mas, se debe restar uno.
------
------          Utl_Lob.ClobToFichero(TPath||TNombreFichero_Txt, TFichero_txt);
-- Escribe el fichero de texto o indice.
------
------          TSubject := 'Envio de '||Pcuantas_Fotos||' ficheros de imagen y
-- su indice, comprimidos, para su almacenamiento.';
------          TBody    := 'Se han enviado '||Pcuantas_Fotos||' ficheros de
-- imagenes, y su indice, comprimidos, para su almacenamiento en gestion de
-- duplicados. En total se han enviado '
------                       ||PCuantos_Exp||' expedientes.'||chr(13)||chr(13)|
-- |chr(13)||'Multas@Mail';
------
------          Dummy := Utl_Zip.Comprime(Tlista_Zip,TPath||TNombreFichero_Zip)
-- ;
--------            Dbms_output.put_line(alba.Utl_Zip.Comprime(Tlista_Zip,TPath
-- ||TNombreFichero_Zip));
------
------          Dummy := Utl_Mail.EnviarCorreo (    '192.168.1.150','
-- Alba.Notificacion@sevilla.org', 'agonzalez.agenrec@sevilla.org','','',
------                                                      TSubject , TBody,
-- Error, TPath||TNombreFichero_zip);
------   End PDescarga_ExpYFotos;
-- AGR 26/02/2012 comentado hasta abajo porque el trml permite compilar.

--------------------------------------------------------------------------------
procedure pdescarga_expyfotos(
        pnombrefichero varchar2,
        pusuidmod      number,
        pcuantos_exp out number,
        pcuantas_fotos out number)
is
    
    cursor cdescarga(cnombrefichero varchar2)
    is
        
        select img.tipoboletin,
            img.boletin,
            img.annoboletin,
            tab.nexpediente,
            tab.nifd,
            tab.nombred,
            img.fichero1,
            img.fichero2,
            img.fichero3,
            img.fichero4,
            img.tipoboletin
            ||'?'
            ||img.boletin
            ||'?'
            ||img.annoboletin
            ||'?'
            ||tab.nexpediente
            ||'?'
            ||tab.nifd
            ||'?'
            ||tab.nombred
            ||'?'
            || decode(img.fichero1,'','',tab.nexpediente
            ||'_Fichero1.'
            ||substr(img.fichero1,instr(img.fichero1,'.')+1))
            ||'?'
            || decode(img.fichero2,'','',tab.nexpediente
            ||'_Fichero2.'
            ||substr(img.fichero2,instr(img.fichero2,'.')+1))
            ||'?'
            || decode(img.fichero3,'','',tab.nexpediente
            ||'_Fichero3.'
            ||substr(img.fichero3,instr(img.fichero3,'.')+1))
            ||'?'
            || decode(img.fichero4,'','',tab.nexpediente
            ||'_Fichero4.'
            ||substr(img.fichero4,instr(img.fichero4,'.')+1))
            ||chr(13)
            ||chr(10) cadena_fichero,
            decode(img.fichero1,'','',tab.nexpediente
            ||'_Fichero1.'
            ||substr(img.fichero1,instr(img.fichero1,'.')+1)) nom_fichero1,
            img.foto_color,
            decode(img.fichero2,'','',tab.nexpediente
            ||'_Fichero2.'
            ||substr(img.fichero2,instr(img.fichero2,'.')+1)) nom_fichero2,
            img.foto_lectura,
            decode(img.fichero3,'','',tab.nexpediente
            ||'_Fichero3.'
            ||substr(img.fichero3,instr(img.fichero3,'.')+1)) nom_fichero3,
            img.boletin_pdf,
            decode(img.fichero4,'','',tab.nexpediente
            ||'_Fichero4.'
            ||substr(img.fichero4,instr(img.fichero4,'.')+1)) nom_fichero4,
            img.fichero_otros
        from alba.multas mul,
            alba.multacar_blob img,
            table(trml.dmultas(mul.mulid,0)) tab
        where upper(img.nomfich_cargado)=cnombrefichero
        and img.boletin                 =mul.mulnumbol
        and mul.mulxboletin            in decode(substr(upper(cnombrefichero),1,1),
            'R',
            (
                select decid
                from table(albaadm.utl_alba.deco('BOLETIN','ZA'))
            )
        ,-- AGR, 04/05/2010, el nombre del fichero nos da el tipo de carga, en este
        -- caso 'R', nos dice que son doc. de ratificacion de 'ZA', por eso se busca
        -- en boletines de 'ZA'.
        (
            select decid
            from table(albaadm.utl_alba.deco('BOLETIN',img.tipoboletin))
        )
            )
            --              And img.annoboletin = Substr(mulx.expediente,1,4);  -- Se ha
            -- eliminado porque en expedientes de finales de a?o no coincide el a?o del
            -- boletin y el a?o del expediente.
        and mul.mulid=
            (
                select max(mul1.mulid)
                from alba.multas mul1
                where mul1.mulanobol=mul.mulanobol
                and mul1.mulnumbol  =mul.mulnumbol
                and mul1.mulxboletin=mul.mulxboletin
            )
        order by tab.nexpediente;
        
        tpath varchar2(1000):='/Padrones/MultasLob/out/'; -- AGR, al Path no se le
        -- puede hacer Upper().
        tfichero_txt clob;
        tlista_zip albaadm.utl_mail.tylista_ficheros:=
        albaadm.utl_mail.tylista_ficheros() ;
        i                  number:=1;
        dummy              number;
        error              varchar2(2000) ;
        tnombrefichero_txt varchar2(200):=upper(to_char(sysdate,
        'RRRR_MM_DD_HH24_MI_SS') ||'_'||replace(lower(pnombrefichero),'.txt','') ||
        '.txt') ;
        tnombrefichero_zip varchar2(200):=upper(to_char(sysdate,
        'RRRR_MM_DD_HH24_MI_SS') ||'_'||replace(lower(pnombrefichero),'.txt','') ||
        '.zip') ;
        tasunto varchar2(2000) ;
        tmensaje clob;
        tcc clob;
        tpara clob;
        tbcc clob ;
        ttipoboletin      varchar2(255) ;
        no_contiene_fotos exception;
    begin
        pcuantos_exp  :=0;
        pcuantas_fotos:=0;
        --        Tlista_Zip      := utl.TyLista_Ficheros() ;
        
        for reg in cdescarga(upper(pnombrefichero))
        loop
            
            if reg.fichero1 is not null then
                alba.utl_lob.blobtofichero(tpath||reg.nom_fichero1,reg.foto_color) ;
                tlista_zip.extend;
                tlista_zip(i):=tpath||reg.nom_fichero1;
                i            :=i+1;
            
            end if;
            
            if reg.fichero2 is not null then
                alba.utl_lob.blobtofichero(tpath||reg.nom_fichero2,reg.foto_lectura) ;
                tlista_zip.extend;
                tlista_zip(i):=tpath||reg.nom_fichero2;
                i            :=i+1;
            
            end if;
            
            if reg.fichero3 is not null then
                alba.utl_lob.blobtofichero(tpath||reg.nom_fichero3,reg.boletin_pdf) ;
                tlista_zip.extend;
                tlista_zip(i):=tpath||reg.nom_fichero3;
                i            :=i+1;
            
            end if;
            
            if reg.fichero4 is not null then
                alba.utl_lob.blobtofichero(tpath||reg.nom_fichero4,reg.fichero_otros) ;
                tlista_zip.extend;
                tlista_zip(i):=tpath||reg.nom_fichero4;
                i            :=i+1;
            
            end if;
            tfichero_txt:=tfichero_txt || reg.cadena_fichero;
            pcuantos_exp:=pcuantos_exp+1;
        
        end loop;
        tlista_zip.extend;
        tlista_zip(i):=tpath||tnombrefichero_txt; -- A?ade a la lista el fichero de
        -- texto que contiene el indice.
        pcuantas_fotos:=i-1; -- Del bucle siempre se sale con una vuelta de mas, se
        -- debe restar uno.
        begin
            
            select decode(tipoboletin,'CO','Disciplina Vial','BB','BlackBerry','ZA',
                'Zona Azul','RA','Ratificacion Controlador ZA')
            into ttipoboletin
            from alba.multacar_blob
            where upper(nomfich_cargado)=upper(pnombrefichero)
            and rownum                  =1; -- Solo es necesario un registro puesto que
            -- cada carga es de un unico tipo.
        
        exception
        
        when no_data_found then
            raise no_contiene_fotos;
        
        end;
        
        if pcuantos_exp=0 or pcuantas_fotos=0 then -- Si hay Expedientes y
            -- fotografias que enviar
            tasunto:='Incidencia al buscar doc. digitalizados en la carga del fichero '
            || upper(pnombrefichero) ||'.';
            tmensaje:=
            'La busqueda de los expedientes y documentos digitales en Alba, sobre la carga del fichero '
            || upper(pnombrefichero) ||', de '||ttipoboletin||'.'||
            ' no ha encontrado datos.'||chr(13) ||chr(13) ||
            ' From alba.multas mul, alba.Multacar_blob img, Table (trml.dmultas(Mul.mulid,0)) tab                                    
Where Upper(img.nomfich_cargado) = '
            ||pnombrefichero||
            '                                    
And img.boletin     = mul.mulnumbol                                    
And mul.mulxboletin in Decode(Substr(Upper(CNombreFichero),1,1),''R'',    (Select DECID From Table(albaadm.utl_ALBA.deco(''BOLETIN'',''ZA''))),                                                                                                             
(Select DECID From Table(albaadm.utl_ALBA.deco(''BOLETIN'',img.tipoboletin)) ))                                    
And mul.expidexp = (Select Max(mul1.expidexp) From alba.multas mul1                                                            
Where mul1.MULANOBOL     = mul.MULANOBOL                                                            
And mul1.MULNUMBOL         = mul.MULNUMBOL                                                            
And mul1.MULXBOLETIN     = mul.MULXBOLETIN)     '
            ;
            tpara:=albaadm.utl_alba.femailusuario(pusuidmod) ;
            --albaadm.utl_ALBA.PEnvia_Mail (PDe => Null, PPara => TPara, PAsunto =>
            -- TAsunto, PMensaje => TMensaje, PNombre_Fichero => '',
            --                 PCc => '', PBcc => TBcc);
        
        else
            alba.utl_lob.clobtofichero(tpath||tnombrefichero_txt,tfichero_txt) ; --
            -- Escribe el fichero de texto o indice.
            tasunto:='Aviso! Se ha dejado un fichero '|| upper(tnombrefichero_zip) ||
            ', con documentos digitalizados, para su archivo.';
            tmensaje:=
            'Ma?ana estara disponible en el area de almacenamiento comun ATSe-DataSur '
            ||pcuantas_fotos||chr(13) ||
            'documentos digitalizados y su indice de carga, todo comprimido en formato *.zip, para que se archive en "Gestion de Duplicados".'
            ||chr(13) ||'Los documentos corresponden a la carga del fichero '||upper(
            pnombrefichero) ||', de '||ttipoboletin||'.'||chr(13) ||
            'El envio contiene los documentos digitalizados de '||pcuantos_exp||
            ' expedientes.';
            dummy:=albaadm.utl_zip.comprime(tlista_zip,tpath||tnombrefichero_zip) ;
            -- AGR, 13/05/2010, eliminamos el correo de aviso a DataSur.
            -- TPara       := 'csr@cibernossur.es';--'csr@cibernossur.es';--'
            -- agonzalez.agenrec@sevilla.org';--'csr@cibernossur.es';--'csr@datasur.es';
            -- AGR, eliminados el correo al usuario. TCc ||','||
            -- albaadm.utl_ALBA.FEmailUsuario ( PUsuIdMod );
            tpara:=albaadm.utl_alba.femailusuario(pusuidmod) ;
            --albaadm.utl_ALBA.PEnvia_Mail (PDe => Null, PPara => Nvl(Tpara,'')  ,
            -- PAsunto => TAsunto, PMensaje => TMensaje, PNombre_Fichero => '',
            --                                 PCc => TCc, PBcc => Null);
        
        end if;
    
    exception
    
    when no_contiene_fotos then
        tpara  :=tcc ||','|| albaadm.utl_alba.femailusuario(pusuidmod) ;
        tasunto:='La carga del fichero '|| upper(pnombrefichero) ||
        ', no contiene pruebas fotograficas.';
        tmensaje:='No se trata de una incidencia pero la carga del fichero '||upper(
        pnombrefichero) ||
        ', puede ser que no contenga pruebas fotograficas, se recomienda revisar, el fichero a mano.'
        ;
        --albaadm.utl_ALBA.PEnvia_Mail (PDe => Null, PPara => TPara  , PAsunto =>
        -- TAsunto, PMensaje => TMensaje, PNombre_Fichero => '',
        --PCc => Null, PBcc => Null);
    
    when others then
        ptratamiento_errores('Error en el fichero. '||pnombrefichero||
        ', avise a Dep Informatica.','Mul_Boletines_Externos.PDescarga_ExpYFotos',
        20024) ;
    
    end pdescarga_expyfotos;

--------------------------------------------------------------------------------
function finforme_cargas(
        pnomfich_cargado albaadm.maestra_carga.nomfich_cargado%type)
    return tyt_informe_cargas pipelined
is
begin
    
    for reg in cinforme_cargas(pnomfich_cargado)
    loop
        pipe row(reg) ;
    
    end loop;
    
    return;

end finforme_cargas;

--------------------------------------------------------------------------------
procedure arregla_dir_notifmano_bb
is
    texpidexp number;
    trdiid    number;
    tidpersona mul_boletines_externos.ty_idpersona;
    teicid   number;
    dummy    number;
    procesar boolean:=true;
begin
    tpkg_usuidmod :=0;
    tpkg_motivomod:='Carga denuncias, (Pkg "Mul_Boletines_Externos").';
    
    for reg in
    (
        select*
        from alba.multacar m
        where m.tipoboletin        ='BB'
        and m.notificadaenmano     ='S'
        and m.nomfich_cargado not in('T2012000154.TXT')
        and cpconductor not like '410%' -- no tiene expediente
        and boletin not in(60018215,60048479,60048403,60017677,1062184341964)
            --                        AND MUL.MULNUMBOL = M.BOLETIN
        and m.cpconductor<>'00000'
            --                        and boletin in (60017677)
        order by boletin
    )
    loop
        
        if reg.notificadaenmano='S' then-- AGR, Si es notificada en mano, se supone
            -- que deben venir los datos del conductor,  ni busca en IVTM ni en DGT,
            -- toma los datos del boletin.
            tidpersona.perid     :=null;
            tidpersona.perversion:=null;
            trdiid               :=null;
            begin
                
                select expidexp
                into texpidexp
                from alba.multas
                where mulnumbol=reg.boletin
                and mulxboletin=523;
            
            exception
            
            when no_data_found then
                ptratamiento_errores('Error al buscar expediente: '||reg.boletin ||' - '||
                reg.tipoboletin ||' - '||reg.annoboletin,
                'Mul_Boletines_Externos.Arregla_dir_NotifMano_BB',20999) ;
            
            end;
            begin
                
                select rdiid
                into trdiid
                from table(depuracion.obj_alba.fbusca_direcc_exp(texpidexp,'NOTIFICACION',
                    'CONDUCTOR')) t;
                
                procesar:=true;
            
            exception
            
            when no_data_found then
                procesar:=false;
            
            end;
            
            if procesar then
                
                update alba.direccionesexpedientes de
                set de.tedfvigencia  =sysdate-1
                where rdiid          =trdiid
                and tedxtipodireccion=283;
                
                update alba.direccion
                set rdifvigencia=sysdate-1
                where rdiid     =trdiid; -- 15862288
                tidpersona     :=fbuscainsertapersona(reg.nifconductor,reg.nombreconductor)
                ;
                --                TRdiId      := FBuscaInsertaDirecc (
                -- Reg.DomicilioConductor, Reg.CPConductor, Reg.LocalidadConductor,
                -- Reg.ProvinciaConductor);
                -- AGR, Se intenta obtener los datos del conductor cargados en el boletin.
                trdiid:=fbuscainsertadirecc(reg.domicilioconductor,reg.cpconductor,
                reg.localidadconductor,reg.provinciaconductor) ;
                
                if tidpersona.perid is not null then -- Hay datos del conductor.
                    teicid             :=fasocio_contrib_exp(texpidexp,21,tidpersona.perid,
                    tidpersona.perversion,ptipo_contrib => 'CONDUCTOR') ;
                    --Update Alba.Multacar Set Est_Procesado = 'PROCESADO'            Where
                    -- Boletin   = Reg.Boletin And TipoBoletin = Reg.TipoBoletin And
                    -- AnnoBoletin = Reg.AnnoBoletin;
                
                end if;
                
                if trdiid is not null and teicid is not null then
                    dummy    :=fasocio_direcc_exp(teicid,trdiid,ptipo_direc => 'NOTIFICACION')
                    ;
                    --Update Alba.Multacar Set Est_Procesado = 'PROCESADO'            Where
                    -- Boletin   = Reg.Boletin And TipoBoletin = Reg.TipoBoletin And
                    -- AnnoBoletin = Reg.AnnoBoletin;
                
                end if;
                --                If TIDPersona.PerId Is Null Or TRdiId Is Null Or TEicId
                -- Is Null Then
                -- AGR, No se puede encontrar al titular del vehiculo, se grabara para
                -- solicitar datos a la DGT desde la matricula.
                --                TVemid := FSolicito_Datos_DGT(Reg.Matricula,
                -- Reg.MarcaVehiculo, Reg.TipoVehiculo, Reg.ModeloVehiculo); -- Se graban
                -- los datos del vehiculo grabados por el agente.
                --                    Update Alba.MultaCar Set Est_procesado = 'OJO PDTE.
                -- DGT'  Where Boletin = Reg.Boletin And TipoBoletin = Reg.TipoBoletin And
                -- AnnoBoletin = Reg.AnnoBoletin;
                --                    Denuncias_a_La_DGT := Denuncias_a_La_DGT + 1;   --
                -- Contador de denuncias que quedan pendientes de completar datos por la
                -- DGT.
                --                End If;
                --          End If;
            
            end if;
        
        end if;
    
    end loop;

end arregla_dir_notifmano_bb;

end mul_boletines_externos;