#Raw data
## Create tables in database (if not already created)
read_n_copy_dta(
  db_conn, "sit_final_cons_2015.dta",
  step_len = 100000, 
  approx_len = 7000000, 
  extra_steps = 10, 
  return.results = TRUE,
  table = "siagie2015_raw", 
  temporary = FALSE,
  overwrite = FALSE, 
  append = TRUE)

read_n_copy_dta(
  db_conn, "sit_final_cons_2014.dta",
  step_len = 100000, 
  approx_len = 7000000, 
  extra_steps = 10, 
  return.results = TRUE,
  table = "siagie2014_raw", 
  temporary = FALSE,
  overwrite = FALSE, 
  append = TRUE)

read_n_copy_dta(
  db_conn, "sit_final_cons_2013.dta",
  step_len = 100000, 
  approx_len = 7000000, 
  extra_steps = 10, 
  return.results = TRUE,
  table = "siagie2013_raw", 
  temporary = FALSE,
  overwrite = FALSE, 
  append = TRUE)

read_n_copy_dta(
  connection = db_conn, 
  file = "barrido_censal.dta",
  step_len = 100000, 
  approx_len = 24000000, 
  extra_steps = 100, 
  return.results = TRUE,
  table = "sisfoh", 
  temporary = FALSE, 
  overwrite = FALSE, 
  append = TRUE)

read_n_copy_excel(
  db_conn, "ECE/", 
  "ece", 
  guess_max = n_max,
  temporary = FALSE,
  overwrite = FALSE, 
  append = TRUE)

#Forms data
source('~/Documents/BRIQ/Stephanie/preschool_cuestionarios.R')
dictionary <- consolidate_dictionaries() %>% arrange(variable, value) %>% mutate(across(everything(), stri_enc_toutf8))
dbWriteTable(db_conn, name = "ece_forms", cuestionario)
write_csv(dictionary, "llave_provisional.csv", na = ".")

#Schools data
source('~/Documents/BRIQ/Stephanie/preschool_schools.R')
dbWriteTable(db_conn, name = "centros_poblados", unidades_atencion)
dbWriteTable(db_conn, name = "schools", schools)
