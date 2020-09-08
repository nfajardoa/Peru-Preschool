
db_conn <- dbConnect(RPostgres::Postgres(), dbname = "peru", host = "localhost", 
                     port = 5432, user = "rwriter", password = "12345")

## Create tables in database (if not already created)

read_n_copy_dta(db_conn, "barrido_censal.dta", 100000, 24000000, extra_steps = 10, return.results = TRUE,
                table = "census", temporary = FALSE, overwrite = FALSE, append = TRUE)

read_n_copy_dta(db_conn, "sit_final_cons_2015.dta", 100000, 7000000, extra_steps = 10, return.results = TRUE,
                table = "siagie", temporary = FALSE, overwrite = FALSE, append = TRUE)

