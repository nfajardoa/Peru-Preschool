
db_conn <- dbConnect(RPostgres::Postgres(), dbname = "peru", host = "localhost", 
                     port = 5432, user = "rwriter", password = "12345")

census_keep <- c("dept", "nom_dept", "secuencia", "nhogar", "prov", "nom_prov", "dist", "nom_dist", "cod_cen_pob",
                 "cen_pob", "ubigeo", "id_cen_pob", "nroviv", "tipo_doc", "n_doc", "paterno", "materno", 
                 "nombres", "fnac_dia", "fnac_mes", "fnac_anio", "fnac", "mujer", "edad")

census_id <- c("dept", "secuencia", "nhogar", "prov", "dist", "cod_cen_pob", "id_cen_pob", "nroviv", 
               "paterno", "materno", "nombres", "fnac_dia", "fnac_mes", "fnac_anio", "mujer")

read_n_copy_dta2(db_conn, "barrido_censal.dta",
                step_len = 100000, 
                approx_len = 24000000, 
                extra_steps = 10, 
                return.results = TRUE,
                table = "census", 
                table2 = "inhabitants",
                ids = census_id,
                keep_vars = census_keep,
                temporary = FALSE, 
                overwrite = FALSE, 
                append = TRUE)


siagie_corrected <- siagie %>% 
  filter(fnac_anio >= 1932, fnac_anio <= 2015, unico == 1) %>% 
  dplyr::select(-c(unico, grupo_unico)) %>%
  arrange(rowSums(is.na(.))) %>%
  distinct(cod_mod, anexo, n_doc, paterno, materno, nombres, fnac, .keep_all = TRUE)

files <- list.files(path = "ECE/") %>% str_subset("(.xlsx)|(.xls)") %>% paste0("ECE/", .)
ece <- map(files, ~ read_with_name_excel(.x, n_max = 100) %>% ece_dictionary()) %>% reduce(bind_rows)

test <- read_with_name_excel("ECE/ECE_2016_2P_Nominada.xlsx")
test %>% ece_dictionary()

## Create tables in database (if not already created)
read_n_copy_dta(db_conn, "barrido_censal.dta",
                step_len = 100000, 
                approx_len = 24000000, 
                extra_steps = 10, 
                return.results = TRUE,
                table = "census_raw", 
                temporary = FALSE,
                overwrite = FALSE, 
                append = TRUE)

read_n_copy_dta(db_conn, "sit_final_cons_2015.dta",
                step_len = 100000, 
                approx_len = 7000000, 
                extra_steps = 10, 
                return.results = TRUE,
                table = "siagie_raw", 
                temporary = FALSE,
                overwrite = FALSE, 
                append = TRUE)

read_n_copy_excel(db_conn, "ECE/", 
                  "ece_raw", 
                  temporary = FALSE,
                  overwrite = FALSE, 
                  append = TRUE)
