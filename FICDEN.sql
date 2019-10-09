--select ficden.preproceso(1073655) from dual

create or replace package ficden as

  function preproceso(vIdg number) return clob;

end ficden
;
/


create or replace package body ficden is

-- VARIABLES
salida clob;


-- FUNCIONES
----------------------------------------------------------------------------------------
procedure imprimir(fila number, campo varchar2, valor varchar2, mensaje varchar2) is

begin

    salida:= salida || to_clob(to_char(fila) ||'¬'|| campo ||'¬'|| valor ||'¬'|| mensaje);
    -- Retorno de carro
    salida:= salida || to_clob(chr(13) || chr(10));

end;


----------------------------------------------------------------------------------------
function novacio(texto varchar2, fila number, campo varchar2) return number is

cursor cTexto is
select texto from dual where texto is null;

resultado number:=0;

begin

  resultado:=0;

  for i in cTexto loop
  
    imprimir(fila,campo,texto,'Valor no puede estar vacío');
    resultado:=1;

  end loop;
  
  return resultado;

end;


----------------------------------------------------------------------------------------
function numero(texto varchar2, fila number, campo varchar2) return number is

cursor cTexto is
select texto from dual where REGEXP_LIKE(texto,'[^0-9]');

resultado number:=0;

begin

  resultado:=0;

  for i in cTexto loop

    imprimir(fila,campo,texto,'Valor numérico obligatorio');
    resultado:=1;

  end loop;
  
  return resultado;

end;

----------------------------------------------------------------------------------------
function fecha(texto varchar2, fila number, campo varchar2) return number is

cursor cTexto is
select texto from dual where REGEXP_LIKE(texto,'^d{1,2}/d{1,2}/d{2,4}$');
--select texto from dual where REGEXP_LIKE(texto,'[^0-9]');

resultado number:=0;

begin

  resultado:=0;

  for i in cTexto loop
  
    imprimir(fila,campo,texto,'Valor de tipo fecha obligatorio dd/mm/aaaa');
    resultado:=1;
  
  end loop;
  
  return resultado;

end;
    

----------------------------------------------------------------------------------------
procedure analizarBoletin(fila number,boletin varchar2,tipoboletin varchar2, 
          añoboletin varchar2,FechaDenuncia varchar2, HoraDenuncia varchar2)  is
          
cursor cMul(vMulnumbol varchar2) is
select 1 from alba.multas m where m.mulnumbol=to_number(vMulnumbol);

verror number:=0;

begin
  
     
  verror:=novacio(boletin,fila,'boletín');
  
  if verror=0 then verror:=numero(boletin,fila,'boletín'); end if;
  
  if verror=0 then
  
      --Comprueba que el boletín no se haya grabado antes
      for i in cMul(boletin) loop
        imprimir(fila,boletin,'boletín','Existe en base de datos');
      end loop;  
  
  end if;
  
end;


----------------------------------------------------------------------------------------
function preproceso(vIdg number) return clob is
    
cursor ctxt is
select 
rownum fila,
a.fichero nomfich_cargado,
fic.token(a.salida,1) boletin,
fic.token(a.salida,2) tipoboletin,
fic.token(a.salida,3) añoboletin,
fic.token(a.salida,4) FechaDenuncia,
fic.token(a.salida,5) HoraDenuncia,
lpad(fic.token(a.salida,6),5,'0') CodigoVia,
fic.token(a.salida,7) ViaLugar,
fic.token(a.salida,8) DelanteDe,
fic.token(a.salida,9) frentea,
fic.token(a.salida,10) ConDireccionA,
fic.token(a.salida,11) codigohechodenunciado,
substr(fic.token(a.salida,12),1,250) descripcionhd,
substr(fic.token(a.salida,13),1,250) observacioneshd,
fic.token(a.salida,14) Matricula,
fic.token(a.salida,15) marcavehiculo,
fic.token(a.salida,16) tipovehiculo,
fic.token(a.salida,17) modelovehiculo,
fic.token(a.salida,18) códigodenunciante1,
fic.token(a.salida,19) nifdenunciante1,
fic.token(a.salida,20) NombreDenunciante1,
fic.token(a.salida,21) domiciliodenunciante1,
fic.token(a.salida,22) CPDenunciante1,
fic.token(a.salida,23) localidaddenunciante1,
fic.token(a.salida,24) provinciadenunciante1,
fic.token(a.salida,25) codigodenunciante2,
fic.token(a.salida,26) nifdenunciante2,
fic.token(a.salida,27) nombredenunciante2,
fic.token(a.salida,28) domiciliodenunciante2,
fic.token(a.salida,29) cpdenunciante2,
fic.token(a.salida,30) LocalidadDenunciante2,
fic.token(a.salida,31) provinciadenunciante2,
fic.token(a.salida,32) grua,
fic.token(a.salida,33) NotificadaEnMano,
substr(fic.token(a.salida,34),1,200) MotivoNoNotificacion,
fic.token(a.salida,35) medidavelocidad,
fic.token(a.salida,36) medidaalcohol,
fic.token(a.salida,37) atestado,
fic.token(a.salida,38) NIFTitular,
fic.token(a.salida,39) nombretitular,
fic.token(a.salida,40) domiciliotitular,
fic.token(a.salida,41) cptitular,
fic.token(a.salida,42) localidadtitular,
fic.token(a.salida,43) ProvinciaTitular,
fic.token(a.salida,44) nifconductor,
fic.token(a.salida,45) nombreconductor,
fic.token(a.salida,46) DomicilioConductor,
fic.token(a.salida,47) cpconductor,
fic.token(a.salida,48) localidadconductor,
fic.token(a.salida,49) provinciaconductor,
fic.token(a.salida,50) niftutor,
fic.token(a.salida,51) nombretutor,
fic.token(a.salida,52) domiciliotutor,
fic.token(a.salida,53) cptutor,
fic.token(a.salida,54) localidadtutor,
fic.token(a.salida,55) provinciatutor,
fic.token(a.salida,56) fichero1, null foto_color,
fic.token(a.salida,57) fichero2, null foto_lectura,
fic.token(a.salida,58) fichero3, null boletin_pdf,
fic.token(a.salida,59) fichero4, null fichero_otros,
sysdate fhoramod, 0 usuidmod, 'Carga de Datos' motivomod,
null boletin2
from 
table(fic.separa_filas(1073655)) a
--table(fic.separa_filas(vIdg)) a
where (regexp_replace(a.salida,'[^A-9]')) is not null
;    


begin

    salida:=null;
    
    -- recorre el fichero 
    for t in cTxt loop

      analizarBoletin(t.fila,t.boletin,t.tipoboletin,t.añoboletin,t.FechaDenuncia,t.HoraDenuncia);
      --numeroNoVacio(t.boletin,t.fila,'boletín');

      
    end loop;
    
    return salida;

end;



end ficden;
/
