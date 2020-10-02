---------------------------------------------------------------------------------------
-- Busca eliminar los registros duplicados de la tabla siagie
-- retorna nada
---------------------------------------------------------------------------------------
create or replace function delete_duplicates_siagie()
	returns void
	language plpgsql
	as $$
declare
	-----------------------------------------------------------------------------------
	-- Cursor: filas repetidas sin incluir la primera coincidencia, preservando el mayor
	-- valor del campo unico
	-----------------------------------------------------------------------------------
	repetidos_cursor cursor for
		select
		*
		from
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
			rep.n_doc asc, rep.unico desc, rep.row_number desc;
	-- end repetidos_cursor
-- Start program
begin
	for repetido in repetidos_cursor loop
		raise notice 'deleting % % % % repeticion % unico %',
			repetido.n_doc,
			repetido.nombres,
			repetido.paterno,
			repetido.materno,
			repetido.row_number,
			repetido.unico;
		--delete from siagie_raw where current of repetidos_cursor;
	end loop;
-- end program
end;
$$;

select delete_duplicates_siagie();