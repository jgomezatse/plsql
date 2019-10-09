-- PASAR A HISTÓRICO LOS PRESCRITOS Y COBRADOS SIN ESCRITOS
insert into multasestadosh
select *
from multasestados m
where 
not exists (select 1 from escritosmultas e where e.mulid=m.mulid)
and substr(m.expediente,1,4) in ('2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016')
and cestado>10000
;

select cestado, estado, count(*) total
from multasestados m
where 
not exists (select 1 from escritosmultas e where e.mulid=m.mulid)
and substr(m.expediente,1,4) in ('2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016')
and cestado>10000
group by cestado,estado
;

--24/05/2018 TOTAL: 2.222.714 TOTAL MULTASESTADOS: 3.468.898
select count(*) total 
from multasestados m
where 
not exists (select 1 from escritosmultas e where e.mulid=m.mulid)
and substr(m.expediente,1,4) in ('2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016')
and cestado>10000
;


CREATE TABLE "ALBAADM"."MULTASESTADOSH" 
 ("MULID" NUMBER, 
"CESTADO" NUMBER, 
"MULFECGRAB" DATE NOT NULL ENABLE, 
"FPRESCRIPCION" DATE, 
"ESTADO" VARCHAR2(200 BYTE), 
"EXPEDIENTE" VARCHAR2(12 BYTE), 
"MULNUMBOL" NUMBER, 
"EXPID" NUMBER, 
"LIQID" NUMBER, 
"LIQUIDACION" VARCHAR2(20 BYTE), 
"PERIDT" NUMBER, 
"PERIDC" NUMBER, 
"PERIDTT" NUMBER, 
"ORIGENDENUNCIA" NUMBER, 
"FECHA" DATE, 
"XML" NUMBER, 
"CESTADOCOBRO" NUMBER, 
"ESTADOCOBRO" VARCHAR2(200 BYTE), 
"FCOBRO" DATE, 
"COBRADO" NUMBER, 
"TOTAL" NUMBER, 
"PENDIENTE" NUMBER, 
"INFO" VARCHAR2(2000 BYTE)
 );
 


CREATE INDEX "ALBAADM"."MULTASESTADOSH_CESTADO" ON "ALBAADM"."MULTASESTADOSH" ("CESTADO");

create index "ALBAADM"."MULTASESTADOSH_I01" on "ALBAADM"."MULTASESTADOSH" ("MULID"); 

create index "ALBAADM"."MULTASESTADOSH_I02" on "ALBAADM"."MULTASESTADOSH" ("ESTADO", "MULFECGRAB", "ORIGENDENUNCIA");

CREATE INDEX "ALBAADM"."MULTASESTADOSH_I03" ON "ALBAADM"."MULTASESTADOSH" ("LIQID", "EXPEDIENTE");
