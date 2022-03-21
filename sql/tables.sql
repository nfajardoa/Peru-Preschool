
create table siagie2013_raw (
cod_mod integer,
anexo smallint,
periodo smallint,
nivel text,
turno smallint,
grado smallint,
seccion text,
fecha_mat date,
estado_mat smallint,
id_persona bigint,
tipo_doc smallint,
ndoc integer,
paterno text,
materno text,
nombres text,
fnac date,
validado_reniec smallint,
discapacidad smallint,
lengua_materna smallint,
mujer bool,
unico bool
);

create table siagie2014_raw (
cod_mod integer,
anexo smallint,
periodo smallint,
nivel text,
turno smallint,
grado smallint,
seccion text,
fecha_mat date,
estado_mat smallint,
id_persona bigint,
tipo_doc smallint,
ndoc integer,
paterno text,
materno text,
nombres text,
fnac date,
validado_reniec smallint,
discapacidad smallint,
lengua_materna smallint,
mujer bool,
unico bool
);

create table siagie2015_raw (
cod_mod integer,
anexo smallint,
periodo smallint,
nivel text,
turno smallint,
grado smallint,
seccion text,
fecha_mat date,
estado_mat smallint,
id_persona bigint,
tipo_doc smallint,
ndoc integer,
paterno text,
materno text,
nombres text,
fnac date,
validado_reniec smallint,
discapacidad smallint,
lengua_materna smallint,
mujer bool,
unico bool
);

create table ece (
mc boolean,
tipo_eva smallint,
periodo smallint,
ubigeo int,
cod_mod int,
anexo smallint,
rural boolean,
estatal boolean,
unidocente boolean,
seccion text,
cod_est smallint,
paterno text,
materno text,
nombres text,
ndoc int,
mujer boolean,
grupo_l integer,
grupo_m integer,
grupo_h integer,
grupo_t integer,
m500_l real, 
m500_m real,
m500_h real,
m500_t real,
aj_l real,
aj_m real,
aj_h real,
aj_t real,
ise real
);

create table sisfoh (
ubigeo integer,
cod_ccpp text,
numero integer,
secuencia integer,
nhogar smallint,
nucleo smallint,
paterno text,
materno text,
nombres text,
ndoc integer,
mujer boolean,
fnac date,
periodo smallint
);

create table students_siagie (
paterno text,
materno text,
nombres text,
mujer boolean,
ndoc integer not null unique,
fnac date,
homonyms smallint,
primary key (paterno, materno, nombres, mujer, fnac)
);

create table student_id (
paterno text,
materno text,
nombres text,
mujer boolean,
ndoc integer not null unique,
fnac date,
homonyms smallint,
valid_ndoc smallint,
valid_fnac smallint,
filter_stage smallint,
year smallint,
year_check bool,
primary key (paterno, materno, nombres, mujer, ndoc, fnac)
);

SELECT con.*
       FROM pg_catalog.pg_constraint con
            INNER JOIN pg_catalog.pg_class rel
                       ON rel.oid = con.conrelid
            INNER JOIN pg_catalog.pg_namespace nsp
                       ON nsp.oid = connamespace
       WHERE nsp.nspname = 'public'
             AND rel.relname = 'student_id';

 ALTER TABLE student_id 
  DROP CONSTRAINT student_id_pkey;
            
ALTER TABLE student_id
  ADD CONSTRAINT student_id_pkey
    PRIMARY KEY (paterno, materno, nombres, mujer, ndoc, fnac);
   
delete from ece where tipo_eva = 1 and periodo = 2015;
delete from ece where tipo_eva = 2 and periodo = 2016;

alter table student_id 
add constraint primary_id
primary key (ndoc, paterno, materno, nombres, fnac, mujer);

create table schools (
	cod_mod int,
	anexo smallint,
	codlocal int,
	cen_edu text,
	niv_mod text,
	tipoprog smallint,
	gestion smallint,
	ges_dep text,
	tipo_local smallint,
	tiporeg smallint,
	tipssexo text,
	cod_tur text,
	fecha_apertura date,
	fecha_cierre date,
	ubigeo int,
	codooii int,
	codccpp int,
	cen_pob text,
	cp_etnico bool,
	len_etnica text,
	region_nat smallint,
   	registro smallint,
   	quintil smallint,
   	tipoice  smallint,
	nlat_cp numeric,
	nlong_cp numeric,
	nlat_ie numeric,
	nlong_ie numeric,
	instalacion_paredes smallint,
	instalacion_techo smallint,
	instalacion_pisos smallint,
	instalacion_energia smallint,
	instalacion_agua smallint,
	instalacion_agua_cont smallint,
	instalacion_inodoros smallint,
	mesas_sillas_buen_estado smallint,
	mesas_sillas_pintar smallint,
	mesas_sillas_arreglar smallint,
	mesas_sillas_sin smallint,
	distancia_ugel text,
	distancia_municipalidad text,
	distancia_inicial text,
	censo_2010 bool,
	censo_2011 bool,
	censo_2015 bool,
	codcp_inei bigint,
	capital smallint,
	z smallint,
	estrato smallint,
	dominio smallint,
	agrupacion smallint,
	numero_ua int,
	centro bool,
	distancia_km numeric,
	error_nlat_cp numeric,
	error_nlong_cp numeric,
	activo bool,
	primary key (cod_mod, anexo)
);

create table matriculation (
	cod_mod int, 
	anexo smallint, 
	periodo smallint, 
	nivel text, 
	turno text, 
	grado smallint, 
	seccion text,
	ndoc int, 
	paterno text, 
	materno text, 
	nombres text, 
	fnac date, 
	mujer bool,
	primary key (cod_mod, anexo, periodo, ndoc, paterno, materno, nombres, fnac, mujer),
	foreign key (cod_mod, anexo) references schools (cod_mod, anexo),
	foreign key (ndoc, paterno, materno, nombres, fnac, mujer) references student_id (ndoc, paterno, materno, nombres, fnac, mujer)
);

update matriculation set seccion = lpad(seccion, 2, '0') where seccion in ('1','2','3','4','5','6','7','8','9');
ALTER TABLE matriculation
	ADD COLUMN matriculation_id SERIAL unique not null;


create table matriculation_ece (
	cod_mod int, 
	anexo smallint, 
	periodo smallint, 
	grado smallint, 
	seccion text,
	ndoc int, 
	paterno text, 
	materno text, 
	nombres text, 
	fnac date, 
	mujer bool
);

alter table ece_forms 
	add constraint ece_id
	foreign key ();

insert into matriculation (cod_mod, anexo, periodo, nivel, turno, grado, seccion, ndoc, paterno, materno, nombres, fnac, mujer) values
	(),
	();

create table test_scores (
	matriculation_id int,
	seccion text,
	cod_est smallint,
	grupo_l integer,
	grupo_m integer,
	grupo_h integer,
	grupo_t integer,
	m500_l numeric,
	m500_m numeric,
	m500_h numeric,
	m500_t numeric,
	aj_l numeric,
	aj_m numeric,
	aj_h numeric,
	aj_t numeric,
	ise numeric,
	primary key (matriculation_id, seccion, cod_est),
	foreign key (matriculation_id) references matriculation (matriculation_id)
);

alter table ece_treatment rename column tipo_eva to grado;
UPDATE ece_treatment SET grado = 7 WHERE grado = 1;
UPDATE ece_treatment SET grado = 9 WHERE grado = 2;
UPDATE ece_treatment SET grado = 13 WHERE grado = 3;

alter table test_scores 
	add constraint ece_forms_key
	unique (matriculation_id, seccion, cod_est);


alter table ece_treatment 
	add constraint ece_id_key1
	foreign key (periodo, cod_mod, anexo) references matriculation (periodo, cod_mod, anexo);

alter table ece_treatment 
	add constraint ece_id_key2
	foreign key (seccion, cod_est) references test_scores (seccion, cod_est):
	
ALTER TABLE student_id 
	ADD COLUMN student_id SERIAL unique not null;

ALTER TABLE student_id 
	ADD COLUMN year_check bool;

update student_id 
	set year_check = true 
where EXTRACT(MONTH FROM fnac) <= 3 and EXTRACT(DAY FROM fnac) <= 31;

ALTER TABLE student_id 
	ADD COLUMN year smallint;

update student_id 
	set year = EXTRACT(YEAR FROM fnac) + 5
where year_check = true;

update student_id 
	set year = EXTRACT(YEAR FROM fnac) + 6
where year_check is null;

	
create table families (
	student_id int,
	family_id int,
	primary key (student_id, family_id),
	foreign key (student_id) references student_id (student_id)
);

ALTER TABLE public.conversion_ccpp ADD CONSTRAINT unique_idccpp UNIQUE (idccpp);
ALTER TABLE public.conversion_ccpp ADD CONSTRAINT conversion_ccpp_pk PRIMARY KEY ("cluster",idccpp);


ALTER TABLE families
ADD idccpp int8;

alter table families 
	add constraint families_ccpp
	foreign key (idccpp) references conversion_ccpp (idccpp);

ALTER TABLE public.sisfoh_corrected DROP COLUMN d_r_a;
ALTER TABLE public.sisfoh_corrected DROP COLUMN npisos;
ALTER TABLE public.sisfoh_corrected DROP COLUMN sisfoh_cse;
ALTER TABLE public.sisfoh_corrected DROP COLUMN estado_identidad;
ALTER TABLE public.sisfoh_corrected DROP COLUMN resid;
ALTER TABLE public.sisfoh_corrected DROP COLUMN color;
ALTER TABLE public.sisfoh_corrected DROP COLUMN sinpinta;
ALTER TABLE public.sisfoh_corrected DROP COLUMN firma;
ALTER TABLE public.sisfoh_corrected DROP COLUMN firmano;

DELETE FROM student_id 
WHERE filter_stage = 2;

ALTER TABLE students_siagie 
	ADD COLUMN match_id SERIAL unique not null;

ALTER TABLE sisfoh 
	ADD COLUMN id_sisfoh SERIAL unique not null;

ALTER TABLE ece 
	ADD COLUMN id_ece SERIAL unique not null;




