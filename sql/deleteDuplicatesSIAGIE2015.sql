		
alter table siagie_raw owner to eduardo

alter table siagie_raw add id_maticula serial not null;

select * from siagie_raw sr where fnac_dia = 1000

update siagie_raw 
	set fnac_dia = 1000
	where id_maticula in
		(select id_maticula from
			(select
				*,
				row_number()
				over
					(partition by
						cod_mod, anexo, periodo, nivel, turno,
						grado, seccion, fecha_mat, estado_mat, id_persona,
						tipo_doc, n_doc, paterno, materno, nombres, fnac,
						validado_reniec, discapacidad,  lengua_materna, mujer
					order by
						n_doc, unico desc)
				from siagie_raw sr) as rep
		where
			rep.row_number > 1
		order by
			rep.id_maticula, rep.row_number desc, rep.n_doc asc, rep.unico desc);

delete from siagie_raw 
       where id_maticula in
		(select id_maticula from
			(select
				*,
				row_number()
				over
					(partition by
						cod_mod, anexo, periodo, nivel, turno,
						grado, seccion, fecha_mat, estado_mat, id_persona,
						tipo_doc, n_doc, paterno, materno, nombres, fnac,
						validado_reniec, discapacidad,  lengua_materna, mujer
					order by
						n_doc, unico desc)
				from siagie_raw sr) as rep
		where
			rep.row_number > 1
		order by
			rep.id_maticula, rep.row_number desc, rep.n_doc asc, rep.unico desc);
