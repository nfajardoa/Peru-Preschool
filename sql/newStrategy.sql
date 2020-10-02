--observaciones únicas completas no nulas
select *
	from (	select * ,count(n_doc) 
				over (partition by n_doc, paterno, materno, nombres, fnac order by n_doc)
				from siagie_raw sr) as rep
	where rep.count > 4 
	order by rep.count desc, rep.n_doc desc
	
--observaciones repetidas
select *
	from (	select * ,count(n_doc)
				over (partition by cod_mod, anexo, periodo, nivel, turno, grado, seccion, fecha_mat, estado_mat, id_persona, 
				tipo_doc, n_doc, paterno, materno, nombres, fnac, validado_reniec, discapacidad,  lengua_materna, mujer order by n_doc)
				from siagie_raw sr) as rep
	where rep.count > 1 
	order by rep.count desc, rep.n_doc desc, rep.unico desc
	

--observaciones repetidas
select *, row_number()
	over (partition by rep.count order by rep.n_doc)
	from (	select * , count(n_doc)
				over (partition by cod_mod, anexo, periodo, nivel, turno, grado, seccion, fecha_mat, estado_mat, id_persona, 
				tipo_doc, n_doc, paterno, materno, nombres, fnac, validado_reniec, discapacidad,  lengua_materna, mujer order by n_doc)
				from siagie_raw sr) as rep
	where rep.count > 2
	order by rep.count desc, rep.n_doc desc
	
--observaciones repetidas !!! esta siiii
select *
	from (	select * , row_number()
				over (partition by cod_mod, anexo, periodo, nivel, turno, grado, seccion, fecha_mat, estado_mat, id_persona, 
				tipo_doc, n_doc, paterno, materno, nombres, fnac, validado_reniec, discapacidad,  lengua_materna, mujer order by n_doc, unico desc)
				from siagie_raw sr) as rep
	where rep.row_number > 1
	order by rep.n_doc asc, rep.unico desc, rep.row_number desc
	
--numero de ceros en el campo unico (deberian ser los campos repetidos)
select count(unico) from siagie_raw sr where sr.unico = 0

--observaciones repetidas que putas es eso??????
select sum(rep.count)
	from (	select *, count(n_doc) 
				over (partition by cod_mod, anexo, periodo, nivel, turno, grado, seccion, fecha_mat, estado_mat, id_persona, 
				tipo_doc, n_doc, paterno, materno, nombres, fnac, validado_reniec, discapacidad,  lengua_materna, mujer order by n_doc)
				from siagie_raw sr) as rep
	where rep.count > 1
	group by rep.count
	order by rep.count desc
	

--observaciones repetidas
select sum(dup.count)
from (select count(sr.n_doc)
	from siagie_raw sr
	group by cod_mod, anexo, periodo, nivel, turno, grado, seccion, fecha_mat, estado_mat, id_persona, 
			tipo_doc, n_doc, paterno, materno, nombres, fnac, validado_reniec, discapacidad,  lengua_materna, mujer
	having count(sr.n_doc) > 1) as dup
	
--cu'antos nombres+apellidos null?, rta: 0
select count(*) 
from siagie_raw sr
where paterno is null and materno is null and nombres is null

--cu'antos apellidos null?, rta: 2
select count(*) 
from siagie_raw sr
where paterno is null and materno is null

--cu'ales apellidos null?,  id_persona: (25075614, 25974915)
select *
from siagie_raw sr
where paterno is null and materno is null

--cu'antos nombres null?, rta: 1
select count(*) 
from siagie_raw sr
where nombres is null

--cuales registros con nombres en null? rta: (id persona: 23885713, paterno: riu, materno: saeba, fnac: 2005-08-03 
select *
from siagie_raw sr
where nombres is null

--cuantos registros sin fecha de nacimiento?, rta: 7
select count(*)
from siagie_raw sr
where fnac is null 

--cu'ales registros no tienen fecha de nacimeinto?
select *
from siagie_raw sr
where fnac is null

--cuantos null en sexo?
select count(*)
from siagie_raw sr
where mujer is null

--cuantos registros sin documento
select count(*)
from siagie_raw sr
where n_doc is null

--------siagie 2013---------------------------------------------------------
--
----------------------------------------------------------------------------
--numero de observaciones 7,755,087
select count(*)
from siagie2013_raw sr

--observaciones unicas 7,755,087
select sum(uniq.count)
from (select count(*)
	from siagie2013_raw sr
	group by cod_mod, anexo, periodo, nivel, turno, grado, seccion, fecha_mat, estado_mat, id_persona, 
			tipo_doc, n_doc, paterno, materno, nombres, fnac, validado_reniec, discapacidad,  lengua_materna, mujer
	having count(*) = 1) as uniq

--cu'antos nombres+apellidos null?, rta: 0
select count(*) 
from siagie2013_raw sr
where paterno is null and materno is null and nombres is null

--cu'antos nombres null?, rta: 4
select count(*) 
from siagie2013_raw sr
where nombres is null

--cuales registros con nombres en null? rta: id_persona(23885713, 24709473, 24689708, 20197574)
select *
from siagie2013_raw sr
where nombres is null

--cu'antos apellidos null?, rta: 1
select count(*) 
from siagie2013_raw sr
where paterno is null and materno is null

--cu'al apellidos null?, rta: 1 id_persona: 6594680
select * 
from siagie2013_raw sr
where paterno is null and materno is null

--cu'antos registros sin fecha de nacimiento?, rta: 15
select count(*)
from siagie2013_raw sr
where fnac is null

--cu'ales registros sin fecha de nacimiento?, rta: 15
select *
from siagie2013_raw sr
where fnac is null

--cuantos null en sexo? rta: 0
select count(*)
from siagie2013_raw sr
where mujer is null

--cuantos registros sin documento
select count(*)
from siagie2013_raw sr
where n_doc is null

--------siagie 2014---------------------------------------------------------
--
----------------------------------------------------------------------------
--numero de observaciones 7,669,467
select count(*)
from siagie2014_raw sr

--observaciones unicas 7,669,467
select sum(uniq.count)
from (select count(*)
	from siagie2014_raw sr
	group by cod_mod, anexo, periodo, nivel, turno, grado, seccion, fecha_mat, estado_mat, id_persona, 
			tipo_doc, n_doc, paterno, materno, nombres, fnac, validado_reniec, discapacidad,  lengua_materna, mujer
	having count(*) = 1) as uniq

--cu'antos nombres+apellidos null?, rta: 0
select count(*)
from siagie2014_raw sr
where paterno is null and materno is null and nombres is null

--cu'antos nombres null?, rta: 2
select count(*)
from siagie2014_raw sr
where nombres is null

--cuales registros con nombres en null? rta: id_persona(23885713, 24709473)
select *
from siagie2014_raw sr
where nombres is null

--cu'antos apellidos null?, rta: 2
select count(*)
from siagie2014_raw sr
where paterno is null and materno is null

--cu'al apellidos null?, rta: 1 id_persona: (25075614, 25974915)
select *
from siagie2014_raw sr
where paterno is null and materno is null

--cu'antos registros sin fecha de nacimiento?, rta: 7
select count(*)
from siagie2014_raw sr
where fnac is null

--cu'ales registros sin fecha de nacimiento?, rta: 7
select *
from siagie2014_raw sr
where fnac is null

--cuantos null en sexo? rta: 0
select count(*)
from siagie2014_raw sr
where mujer is null

--cuantos registros sin documento
select count(*)
from siagie2014_raw sr
where n_doc is null

--------ece----------------------------------------------------------------
--ece
----------------------------------------------------------------------------
--cu'antas pruebas por a~no se realizaron
select distinct periodo, count(*)
over (partition by periodo order by periodo)
from ece_raw er 

--cu'antas pruebas por a~no se pueden cruzar con siegie
--numero de coincidencias de las pruebas con nombres y codigo colegio, rta: 4079327
select distinct periodo, count(*)
over (partition by periodo order by periodo)
from (select er.periodo, er.materno, er.paterno, er.nombres 
	from siagie_raw sr
	join ece_raw er
	on sr.paterno = er.paterno
	and sr.materno = er.materno
	and sr.nombres = er.nombres
	and cast(sr.cod_mod as integer) = cast(er.cod_mod7 as integer)) as pc
	
--cu'antas pruebas por a~no se pueden cruzar con siagie 2015
--numero de coincidencias de las pruebas con nombres 6570554
select distinct periodo, count(*)
over (partition by periodo order by periodo)
from (select er.periodo, er.materno, er.paterno, er.nombres from siagie_raw sr
	join ece_raw er
	on sr.paterno = er.paterno
	and sr.materno = er.materno
	and sr.nombres = er.nombres) as pc
	
--cu'antas pruebas por a~no se pueden cruzar con siegie 2013
--numero de coincidencias de las pruebas con nombres
select distinct periodo, count(*)
over (partition by periodo order by periodo)
from (select er.periodo, er.materno, er.paterno, er.nombres from siagie2013_raw sr
	join ece_raw er
	on sr.paterno = er.paterno
	and sr.materno = er.materno
	and sr.nombres = er.nombres) as pc
	
--cu'ales registros por a~no est'an repetidos
select *
from (	select distinct periodo, paterno, materno, nombres, count(*)
			over (partition by periodo, paterno, materno, nombres order by periodo)
			from ece_raw er) as rep
where rep.count > 1
order by count desc, periodo asc

--cu'antos registros por a~no est'an repetidos
select periodo, sum(rep.count)
from (	select distinct periodo, paterno, materno, nombres, count(*)
			over (partition by periodo, paterno, materno, nombres order by periodo)
			from ece_raw er) as rep
where rep.count > 1
group by periodo
order by periodo asc

--resumen
select rep.periodo, sum(rep.count) as homonimos, total.count as total
from (	select distinct er1.periodo, er1.paterno, er1.materno, er1.nombres, count(*)
			over (partition by er1.periodo, er1.paterno, er1.materno, er1.nombres order by er1.periodo)
			from ece_raw er1) as rep
join (	select distinct er.periodo, count(*)
			over (partition by er.periodo order by er.periodo)
			from ece_raw er) as total
on rep.periodo = total.periodo
where rep.count > 1
group by rep.periodo, total
order by rep.periodo asc

--nombres irregulares
--tienen números o caracteres invalidos?

--existe una estrategia unica de identificacion?
-- nombres, fnac y sexo como id, cuantas coincidencias de nombre, fnac y sexo tienen diferente id persona?
select * from
	(select *, count(*)
		over(	partition by dp.paterno, dp.materno, dp.nombres, dp.fnac, dp.mujer 
				order by dp.paterno, dp.materno, dp.nombres)
		from (	select distinct sr.id_persona, sr.paterno, sr.materno, sr.nombres, 
					sr.fnac, sr.mujer, row_number()
				over( partition by sr.id_persona order by sr.id_persona)
				from siagie_raw sr) as dp) as homonimos
order by homonimos.count desc

			select *, count(*)
			over ( partition by sr.id_persona order by sr.id_persona)
			from siagie_raw sr
			
