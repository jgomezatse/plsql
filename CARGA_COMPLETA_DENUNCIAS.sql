create or replace procedure carga_completa_denuncias is

--declare

cursor cCarga(vId number) is
select distinct a.fichero nomfich_cargado
from
table(depuracion.fic.separa_filas(vId)) a
where fic.token(a.salida,1) is not null
;

cursor cGrupos is
select distinct idg
from alba.ficheros f
where f.ficgrupo=1
and not exists 
(select 1 from alba.maestra_carga a 
where upper(a.nomfich_cargado)=upper(f.ficnombre));

vIdg number:=0;

begin



  for i in cGrupos loop
  
      vIdg:=i.idg;

      insert into alba.maestra_carga m
      select distinct a.fichero nomfich_cargado,
      '\\varios\fotosradar$\MULTA-CAR\'|| substr(a.fichero,1,length(a.fichero)-4) path,
      trunc(sysdate) fecha_carga,
      0 total_fichero,
      0 total_correctos,
      0 total_incidencias,
      'Carga datos' descr,
      null fichero_bad,
      null fichero_dsc,
      '' estado_boletin,
      '' estado_exp
      from
      table(depuracion.fic.separa_filas(vIdg)) a
      where fic.token(a.salida,1) is not null
      ;
      
      commit;
      
      insert into alba.temp_interf_multacar
      select 
      a.fichero nomfich_cargado,
      fic.token(a.salida,1) boletin,
      decode(fic.token(a.salida,2),'C','CO','Z','ZA','T','TR','H','CH','B','CB',fic.token(a.salida,2)) tipoboletin,
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
      table(depuracion.fic.separa_filas(vIdg)) a
      where fic.token(a.salida,1) is not null
      and not exists (select 1 from alba.multas m where m.mulnumbol=fic.token(fic.token(a.salida,1),1,'@'))
      ;
      
      commit;
      
      -- CARGA TODOS LOS FICHEROS QUE PERTENECEN A UN MISMO GRUPO
      for cg in cCarga(vIdg) loop
      
        alba.mul_boletines_externos.pgrabo_maestra_multacar(cg.nomfich_cargado);
        commit;
        
      end loop;
  
  end loop;


  -- GENERA EXPEDIENTES
  alba.trml.cargaFicheroDenuncias;
  commit;


end;
------------------------------------------------------------------------------------------------------------------------
--  END CARGA FICHEROS
------------------------------------------------------------------------------------------------------------------------