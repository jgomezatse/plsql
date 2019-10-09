create or replace package fic as


      type cadena is record
      (
      -- Denuncia
      fichero varchar2(200),
      salida varchar2(4000)
      );
      type lista_cadenas is table of cadena;
      
      function er(vIn varchar2,vEr varchar2) return varchar2;
      
      function token2(vIn varchar2,vPosicion number,vSeparador varchar2 default chr(172)) return varchar2;
      
      function separa_filas2(vIdGrupo number) return lista_cadenas pipelined;
      
      function separa_filas(vIdGrupo number) return lista_cadenas pipelined;
      
      function token(stringvalues varchar2, indice number, delim VARCHAR2 default chr(172)) return varchar2;

end fic
;
/

--------------------------------------------------------------------------------
create or replace package body fic is
--------------------------------------------------------------------------------


    function token2(vIn varchar2,vPosicion number,vSeparador varchar2 default chr(172)) return varchar2 is
    begin
        return REGEXP_SUBSTR(vIn,'[^('||vSeparador||')]*',1,vPosicion);
    end;     
    
    function separa_filas2(vIdGrupo number)
    return lista_cadenas pipelined is
    
    cursor ctxt(vidgrupo number) is
    select regexp_substr(a.idclob,'[^'||a.separador||']+', 1, level) registro, idtext
    from (
    select to_char(idclob) idclob, idtext, upper(substr(idtext,-3)) ext,
    decode(upper(substr(idtext,-3)),'TXT',CHR(10)||CHR(13),'XML','<REGISTRO',CHR(10)||CHR(13)) separador
    from albaadm.idtmp
    where idg=vidgrupo
    and idclob is not null
    and upper(substr(idtext,-3)) in ('TXT','XML')
    and idtext is not null) a
    connect by regexp_substr(a.idclob, '[^'||a.separador||']+', 1, level) is not null;
    
    v cadena;
       
    begin
    
        v.salida:='';
        for t in cTxt(vIdGrupo) loop
          v.fichero:=t.idtext;
          v.salida:=t.registro;
          pipe row(v);
        end loop;
          
        return;

    end separa_filas2;    
    
    function er(vIn varchar2,vEr varchar2) return varchar2 is
    begin
      if regexp_like(vIn,vEr) then
        return vIn;
      else
        return '#'||vIn;
      end if;
    end;
    
    ----------------------------------------------------------------------------
    function separa_filas(vIdGrupo number)
    return lista_cadenas pipelined is
    
    cursor ctxt(vidgrupo number) is
    select idclob, idtext, upper(substr(idtext,-3)) ext
    from albaadm.idtmp
    where idg=vIdGrupo
    and idclob is not null
    and upper(substr(idtext,-3)) in ('TXT','XML')
    and idtext is not null
    ;    
    
    lindice number;
    v cadena;
    scopia clob;
    sseparador varchar2(20):=chr(10);
       
    begin
    

    
        v.salida:='';
        
        for t in cTxt(vIdGrupo) loop
        
          if t.ext='XML' then
              sseparador:='<REGISTRO';
          elsif t.ext='TXT' then
              sseparador:=chr(10);
          else
              sseparador:=chr(10);
          end if;

          v.fichero:=t.idtext;
          scopia:=t.idclob;
          lindice := instr(scopia,sseparador);
          
          while lindice > 0 loop
            v.salida := substr(substr(scopia, 1, lindice - 1),1,4000);
            pipe row(v);
            sCopia := substr(sCopia, lIndice + length(sSeparador));
            lIndice := instr(sCopia,sSeparador);
          end loop;
          --scopia:=regexp_replace(scopia,'[^A-9]');
          --if scopia is not null and scopia<>'' then
            v.salida := scopia;
            pipe row(v);
          --end if;              
        end loop;
          
        return;

    end separa_filas;
    
    ----------------------------------------------------------------------------
    FUNCTION Token(
        stringvalues varchar2,
        indice       number,
        delim        VARCHAR2 default chr(172))
      RETURN VARCHAR2
    IS
      start_pos NUMBER; -- Posición inicial de cada substring
      end_pos   number; -- Posición final de cada substring
      --delim varchar2(10):='¬';
    BEGIN
      -- Si el primer indice es uno
      IF indice    = 1 THEN
        start_pos := 1; -- La posición inicial sera 1
      ELSE
        /* Se calcula la posición del delimitador segun el substring que se desea conseguir  */
        /*             Ejm: 12;13;  Se desea el inidice 2 del delim ; --> start_pos=3        */
        start_pos := instr(stringvalues, delim, 1, indice - 1);
        -- Si la posicion inicial es 0 se retorna null
        IF start_pos = 0 THEN
          RETURN NULL;
        ELSE
          -- Se calcula la posición inicial del substring sin importar el largo del delimitador
          start_pos := start_pos + LENGTH(delim);
        END IF;
      END IF;
      -- Se calcula la posición final del substring
      end_pos   := instr(stringvalues, delim, start_pos, 1);
      if end_pos = 0 then -- Se retorna el ultimo valor del arreglo
        RETURN trim(SUBSTR(stringvalues, start_pos));
      else -- Se retorna el valor del arreglo segun el inidice y delim indicado
        RETURN trim(SUBSTR(stringvalues, start_pos, end_pos - start_pos));
      END IF;
    END Token;

end fic
;
/