/*  create table tm_multasestado as  */
/*  select a.* from multas m, table(depuracion.trml.emultas(m.mulid)) a  */
/*  where m.mulid=a.mulid  */
/*  and rownum<2  */
/*  order by 1 desc  */
/*    */
/*  select * from tm_multasestado  */

--ALTER TABLE depuracion.tm_multasestado MOVE TABLESPACE DEP_DATOS_4M

--truncate table depuracion.tm_multasestado
--/



declare
    
    cursor cMul is
    select m.mulid from alba.multas m
    where m.mulfecgrab>=to_date('01/08/2006','dd/mm/yyyy');
	    
    parresultado varchar2(400);
    i number:=0;
    
begin

	
	for l in cMul loop
	

				insert into depuracion.tm_multasestado
				select a.*, sysdate
				from alba.multas m, table(trml.emultas(m.mulid)) a
				where m.mulid=l.mulid
				and m.mulid=a.mulid;
	
				i:=i+1;
				
				if i=1000 then
					commit;
					i:=0;
				end if;
				
	end loop;

    commit;
    
    parresultado := 'OK. El proceso finalizó correctamente.';
    dbms_output.put_line(parresultado);
	  ----------------------------------------------------------------
	  -- FINALIZACION DEL PROCESO CON EXCEPCION
	  ----------------------------------------------------------------
exception
    when others then
       parresultado := 'KO. ' || SQLERRM;         
       dbms_output.put_line(parresultado);
       rollback;
end;
/


/*
select 
Floor(Round((max(a.horaactual)-min(a.horaactual))*24,2)) ||':'||
round( ( (max(a.horaactual)-min(a.horaactual))*24-Floor((max(a.horaactual)-min(a.horaactual))*24) )*60 ) "TIEMPO DE EJECUCIÓN",
count(*) "TOTAL EXPEDIENTES PROCESADOS"
from depuracion.tm_multasestado a



select * from tm_multasestado

select count(1) total from tm_multasestado union select count(1) total from albaadm.vm_multasestados



select
(select count(1) from albaadm.vm_multasestados) prod,
(select count(1) from tm_multasestado) dep,
((select count(1) from albaadm.vm_multasestados)-
(select count(1) from tm_multasestado)) dif
from dual
*/
