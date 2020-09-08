
setwd("Documents/BRIQ/Stephanie/")

###### IDENTIFICADOS [7056084] *

estudiantes_siagie_identificados <- tbl(db_conn, "siagie_raw") %>%
  filter(!is.na(fnac), !is.na(n_doc)) %>%
  group_by(n_doc, paterno, materno, nombres, fnac) %>%
  mutate(n = n()) %>%
  filter(n == 1) %>%
  collect()

###### CAMBIO [462399] Total
# [228701] observaciones unicas con grupos (n_doc, paterno, materno, nombres, fnac) 

estudiantes_siagie_cambio <- tbl(db_conn, "siagie_raw") %>%
  filter(!is.na(fnac), !is.na(n_doc)) %>%
  group_by(n_doc, paterno, materno, nombres, fnac) %>%
  mutate(n = n()) %>%
  filter(n > 1) %>%
  dplyr::select(-n) %>%
  group_by(cod_mod, n_doc, paterno, materno, nombres, fnac) %>%
  mutate(n = n()) %>%
  filter(n == 1) %>%
  collect()

###### INCONSISTENTES [1882] Total
# [912] observaciones unicas con grupos (n_doc, paterno, materno, nombres, fnac) 

estudiantes_siagie_inconsistentes <- tbl(db_conn, "siagie_raw") %>%
  filter(!is.na(fnac), !is.na(n_doc)) %>%
  group_by(n_doc, paterno, materno, nombres, fnac) %>%
  mutate(n = n()) %>%
  filter(n > 1) %>%
  dplyr::select(-n) %>%
  group_by(cod_mod, n_doc, paterno, materno, nombres, fnac) %>%
  mutate(n = n()) %>%
  filter(n > 1) %>%
  collect()

estudiantes_siagie_inconsistentes_final <- estudiantes_siagie_inconsistentes %>%
  dplyr::select(-n) %>%
  mutate(estado_mat = min(estado_mat),
         validado_reniec = min(validado_reniec)) %>%
  summarize(across(everything(), decide_drops), .groups = "keep")

## Continuacion de cambio
estudiantes_siagie_cambio %<>%
  dplyr::select(-n) %>%
  group_by(n_doc, paterno, materno, nombres, fnac) %>%
  mutate(n = n())

###### estudio mat == 1 & n == 1 [179290] Total
# [179290] observaciones unicas con grupos (n_doc, paterno, materno, nombres, fnac) 

estudiantes_siagie_cambio_matdefinitiva <- estudiantes_siagie_cambio %>% 
  filter(estado_mat == 1) %>% 
  dplyr::select(-n) %>%
  mutate(n = n()) %>%
  filter(n == 1)

###### estudio (mat == 1 & n>1) | (estudio_mat > 1) [100,282] Total
# [49411] observaciones unicas con grupos (n_doc, paterno, materno, nombres, fnac) 

estudiantes_siagie_cambio_matvar <- 
  anti_join(estudiantes_siagie_cambio, estudiantes_siagie_cambio_matdefinitiva, 
          by = c("n_doc", "paterno", "materno", "nombres", "fnac"))

###### (estado_mat == 1 & n>1) [18,254] Total
# [9122] observaciones unicas con grupos (n_doc, paterno, materno, nombres, fnac) 

estudiantes_siagie_cambio_matdefinitiva_match <- 
  estudiantes_siagie_cambio_matvar %>% filter(estado_mat == 1)

###### (estado_mat>1) [81642] Total
# [40289] observaciones unicas con grupos (n_doc, paterno, materno, nombres, fnac) 

estudiantes_siagie_cambio_matvar_g1 <- 
  anti_join(estudiantes_siagie_cambio_matvar %>% filter(estado_mat > 1),
            estudiantes_siagie_cambio_matdefinitiva_match, 
            by = c("n_doc", "paterno", "materno", "nombres", "fnac"))

###### (estado_mat>1 & n=1) [12] Total
# [12] observaciones unicas con grupos (n_doc, paterno, materno, nombres, fnac) 

estudiantes_siagie_cambio_matvar_g1_n1 <- 
  estudiantes_siagie_cambio_matvar_g1 %>% 
  dplyr::select(-n) %>%
  mutate(n = n()) %>%
  filter(n == 1) # Indetificadas con MATCH EXTERNO (M) **

###### (estado_mat>1 & n>1) [81630] Total
# [40277] observaciones unicas con grupos (n_doc, paterno, materno, nombres, fnac) 

estudiantes_siagie_cambio_matvar_g1_nmax <- 
  estudiantes_siagie_cambio_matvar_g1 %>% 
  dplyr::select(-n) %>%
  mutate(n = n()) %>%
  filter(n > 1)

###### (estado_mat == 2 & n = 1) [38545] Total (Eliminando todas aquellas que ya tenian estado_mat == 1)
# [38545] observaciones unicas con grupos (n_doc, paterno, materno, nombres, fnac) 

estudiantes_siagie_cambio_matvar_g1_nmax_mat2 <- 
  estudiantes_siagie_cambio_matvar_g1_nmax %>% 
  dplyr::select(-n) %>%
  filter(estado_mat == 2) %>%
  mutate(n = n()) %>%
  filter(n == 1) ## Identificada **

###### (estado_mat == 2 & n > 1) | (estado_mat > 2) [4525] Total (Eliminando todas aquellas que ya tenian estado_mat == 1)
# [1732] observaciones unicas con grupos (n_doc, paterno, materno, nombres, fnac) 
  
estudiantes_siagie_cambio_matvar_g1_nmax_matg2 <- 
  anti_join(estudiantes_siagie_cambio_matvar_g1_nmax, estudiantes_siagie_cambio_matvar_g1_nmax_mat2, 
            by = c("n_doc", "paterno", "materno", "nombres", "fnac"))

###### (estado_mat == 2 & n > 1) [3034] Total (Eliminando todas aquellas que ya tenian estado_mat == 1)
# [1506] observaciones unicas con grupos (n_doc, paterno, materno, nombres, fnac) 

estudiantes_siagie_cambio_matvar_g1_nmax_mat2_nmax <- 
  estudiantes_siagie_cambio_matvar_g1_nmax_matg2 %>% 
  filter(estado_mat == 2) %>%
  dplyr::select(-n) # Indetificadas con MATCH EXTERNO (M) **

###### (estado_mat == 3 & n == 1) [0] Total (Eliminando todas aquellas que ya tenian estado_mat == 1 & == 2 recursivamente)
# [0] observaciones unicas con grupos (n_doc, paterno, materno, nombres, fnac) 

###### (estado_mat == 3 & n > 1) | (estado_mat > 3) [452] Total (Eliminando todas aquellas que ya tenian estado_mat == 1 & == 2 recursivamente)
# [226] observaciones unicas con grupos (n_doc, paterno, materno, nombres, fnac) 

estudiantes_siagie_cambio_matvar_g1_nmax_matg2_nmax <- 
  anti_join(estudiantes_siagie_cambio_matvar_g1_nmax_matg2, estudiantes_siagie_cambio_matvar_g1_nmax_mat2_nmax,
            by = c("n_doc", "paterno", "materno", "nombres", "fnac"))

###### TERMINA

# FALTA terminar  recursion hasta estado_mat == 5. Aunque se puede saltar ese paso dadas las observaciones que se tienen 








  filter(unico == 1) %>%
  group_by(n_doc, paterno, materno, nombres, fnac) %>%
  mutate(n = n()) %>%
  filter(n > 1) %>%
  dplyr::select(-n, -unico) %>%
  group_by(cod_mod, n_doc, paterno, materno, nombres, fnac) %>%
  left_join(tbl(db_conn, "ece_raw"), by = c("n_doc", "paterno", "materno", "nombres", "mujer")) %>%
  collect()

estudiantes_siagie %>%
  dplyr::select(-n) %>%
  group_by(cod_mod, n_doc, paterno, materno, nombres, fnac) %>%
  count() %>% 
  arrange(nombres, n) %>%
  filter(n==1)
  View() #Delete if n==1

estudiantes_siagie_id <- tbl(db_conn, "siagie_raw") %>%
  group_by(id_persona, n_doc, paterno, materno, nombres, fnac) %>%
  mutate(n = n()) %>%
  filter(n > 1) %>%
  collect()

estudiantes_siagie_id_2 <- tbl(db_conn, "siagie_raw") %>%
  group_by(id_persona) %>%
  mutate(n = n()) %>%
  filter(n > 1) %>%
  collect()

estudiantes_siagie_id_3 <- tbl(db_conn, "siagie_raw") %>%
  group_by(id_persona, n_doc) %>%
  mutate(n = n()) %>%
  filter(n > 1) %>%
  collect()

full_join(estudiantes_siagie_id %>% dplyr::select(-n,-unico), estudiantes_siagie_id_3 %>% dplyr::select(-n,-unico))

estudiantes_siagie_colegio <- tbl(db_conn, "siagie_raw") %>%
  group_by(cod_mod, n_doc, paterno, materno, nombres, fnac) %>%
  mutate(n = n()) %>%
  filter(n > 1) %>%
  collect()

estudiantes_siagie_colegio %>%
  dplyr::select(-n) %>%
  group_by(cod_mod, n_doc, paterno, materno, nombres, fnac, unico) %>%
  count() %>% 
  arrange(nombres, n) %>%
  View()


estudiantes_ece_smov_g2 <- tbl(db_conn, "ece_raw") %>%
    group_by(codgeo, cod_mod7, paterno, materno, nombres, mujer) %>%
    count() %>%
    filter(n() > 1) %>%
  collect()  

estudiantes_ece_smov %<>% arrange(n)

patricia_bereca <- tbl(db_conn, "ece_raw") %>%
  filter(paterno == "bereca", materno == "murayari", nombres = "patricia", mujer == 1) %>%
  collect()

cod_ugel <- tbl(db_conn, "ece_raw") %>% group_by(cod_ugel, nom_ugel) %>% count() %>% collect() %>% rowid_to_column("id")
codgeo <- tbl(db_conn, "ece_raw") %>% group_by(codgeo) %>% count() %>% collect() %>% rowid_to_column("id")
cod_eva <- tbl(db_conn, "ece_raw") %>% group_by(cod_eva) %>% count() %>% collect() %>% rowid_to_column("id")

table <- list.files("ECE/") %>% 
  str_subset("ECE") %>%
  paste0("ECE/", .) %>%
  map(~ read_with_name_excel(.x, n_max = 0) %>% ece_dictionary())

table %<>% reduce(bind_rows)



names_int <- ls(pattern = "test_") %>% 
  map(identity_transformer, envir = globalenv()) %>% 
  map(~ names(.x) %>% str_to_lower) %>%
  reduce(intersect)

names_union <- ls(pattern = "test_") %>% 
  map(identity_transformer, envir = globalenv()) %>% 
  map(~ names(.x) %>% str_to_lower) %>%
  reduce(union)

setdiff(names_union, names_int) %>% sort()

names_union %>% str_subset("m500_")
names_union %>% str_subset("peso_")
names_union %>% str_subset("cod_eva")
names_union %>% str_subset("grupo")
names_union %>% str_subset("sexo")



bind_rows(test_1, test_10)

test <- estudiantes_siagie %>%
  mutate(cod_mod = map_chr(cod_mod, codmod_2_codmod7))

test %>% ungroup() %>% filter(cod_mod == cod_mod7) %>% dplyr::select(cod_mod, cod_mod7, periodo.y, periodo.x) %>% View()
test %>% ungroup() %>% distinct(n_doc, paterno, materno, nombres, fnac)
