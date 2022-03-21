######## ENROLLMENT DATABASE FILTERING PROCEDURE

## Step 1: Depurate and save new enrollment databases by year
## Delete obs with missing ID and birthday from enrollment databases.
## Group by n_doc, paterno, materno, nombres, fnac, validado_reniec
## validado_reniec is 1 if id number does exist in the official registries (it is no necessarily a name id match)
## Keep only one individual observation (with the highest estado_mat)

# 2013
tbl(db_conn, "siagie2013_raw") %>%
  filter(!is.na(fnac), !is.na(n_doc)) %>%
  group_by(n_doc, paterno, materno, nombres, fnac, validado_reniec) %>%
  mutate(translado_2013 = n() - 1) %>%
  arrange(estado_mat, .by_group = TRUE) %>%
  filter(row_number() == 1, validado_reniec == 1) %>%
  compute(
    name = "siagie2013",
    temporary = FALSE
  )

# 2014
tbl(db_conn, "siagie2014_raw") %>%
  filter(!is.na(fnac), !is.na(n_doc)) %>%
  group_by(n_doc, paterno, materno, nombres, fnac, validado_reniec) %>%
  mutate(translado_2014 = n() - 1) %>%
  arrange(estado_mat, .by_group = TRUE) %>%
  filter(row_number() == 1, validado_reniec == 1) %>%
  compute(
    name = "siagie2014",
    temporary = FALSE
  )

# 2015
tbl(db_conn, "siagie2015_raw") %>%
  filter(!is.na(fnac), !is.na(n_doc)) %>%
  group_by(n_doc, paterno, materno, nombres, fnac, validado_reniec) %>%
  mutate(translado_2015 = n() - 1) %>%
  arrange(estado_mat, .by_group = TRUE) %>%
  filter(row_number() == 1, validado_reniec == 1) %>%
  compute(
    name = "siagie2015",
    temporary = FALSE
  )

## Step 2: Obtain SQL queries
## Filter rows appearing only once (by n_doc, paterno, materno, nombres, fnac, mujer)
## Select columns paterno, materno, nombres, n_doc, fnac, mujer, cod_mod, anexo, grado, translado_{year}
## Rename columns: cod_mod_{year} = cod_mod, anexo_{year} = anexo, grado_{year} = grado

enrollment_2013 <- tbl(db_conn, "siagie2013") %>%
  group_by(n_doc, paterno, materno, nombres, fnac, mujer) %>%
  filter(n() == 1) %>%
  dplyr::select(paterno, materno, nombres, n_doc, fnac, mujer, cod_mod, anexo, grado, translado_2013) %>%
  rename(cod_mod_2013 = cod_mod, anexo_2013 = anexo, grado_2013 = grado)

enrollment_2014 <- tbl(db_conn, "siagie2014") %>%
  group_by(n_doc, paterno, materno, nombres, fnac, mujer) %>%
  filter(n() == 1) %>%
  dplyr::select(paterno, materno, nombres, n_doc, fnac, mujer, cod_mod, anexo, grado, translado_2014) %>%
  rename(cod_mod_2014 = cod_mod, anexo_2014 = anexo, grado_2014 = grado)

enrollment_2015 <- tbl(db_conn, "siagie2015") %>%
  group_by(n_doc, paterno, materno, nombres, fnac, mujer) %>%
  filter(n() == 1) %>%
  dplyr::select(paterno, materno, nombres, n_doc, fnac, mujer, cod_mod, anexo, grado, translado_2015) %>%
  rename(cod_mod_2015 = cod_mod, anexo_2015 = anexo, grado_2015 = grado)

## Step 3: Create a new table by full-joining the step 2 queries
## Full join by paterno, materno, nombres, n_doc, fnac, mujer
## Select no repeating values for paterno, materno, nombres
## Write new table in database: "enrollment"
### No homonyms but inconsistent id numbers. birthday and sex are consistent.

ls(pattern = "enrollment_") %>%
  map(~ rlang::parse_expr(.x) %>% eval()) %>%
  reduce(full_join, by = c("paterno", "materno", "nombres", "n_doc", "fnac", "mujer")) %>%
  group_by(paterno, materno, nombres) %>%
  filter(n() == 1) %>%
  compute(
    name = "enrollment",
    temporary = FALSE
  )

## Step 4: Perform SQL checks
## Check 1: Student appears on all years (2013, 2014, 2015)
## Check 2: Student appears on 2013 and 2014 (Secure if student is in last year of schooling, grado_2014 == 16)
## Check 3: Student appears on 2014 and 2015 (Secure if student is in first year of schooling, grado_2014 == 2)
## Check 4: Student appears on 2013 and 2015 (Non secure, possible name typos or mismatching)

# *SQL code start*
# alter table enrollment
# add column check_1 int2,
# add column check_2 int2,
# add column check_3 int2,
# add column check_4 int2;
#
# update enrollment set check_1 = translado_2013 + translado_2014 + translado_2015 + 1;
# update enrollment set check_2 = translado_2013 + translado_2014 + 1;
# update enrollment set check_3 = translado_2014 + translado_2015 + 1;
# update enrollment set check_4 = translado_2013 + translado_2015 + 1;
# *SQL code finish*

## Step 5: Copy "check_1" complying observations with no repeated id numbers to unique student/id table "students_siagie".
## variables in table paterno, materno, nombres, fnac, mujer, ndoc, valid_ndoc, security_level
## valid_ndoc == TRUE, since every number exists on the official registries, however no name/id match is guaranteed
## security_level == 1, highest possible certainty of identification of individual students
## SQL table details: "students_siagie"
### primary key (paterno, materno, nombres, mujer, fnac)
### ndoc unique constraint
### id serial

table <- tbl(db_conn, "enrollment") %>%
  filter(check_1 == 1) %>%
  dplyr::select(n_doc, paterno, materno, nombres, fnac, mujer) %>%
  group_by(n_doc) %>%
  filter(n() == 1) %>%
  rename(ndoc = n_doc) %>%
  mutate(
    valid_ndoc = TRUE,
    security_level = 1,
  ) %>%
  group_by(ndoc) %>%
  filter(n() == 1) %>%
  group_by(paterno, materno, nombres) %>%
  mutate(homonyms = n()-1) %>%
  collect()

###### FIRST FILTER (most secure, security_level == 1)
###### Identified: 5508664
###### By: father's surname, mother's surname, names, sex, birthday, unique ID number.

dbWriteTable(db_conn, name = "students_siagie", table, temporary = FALSE, append = TRUE)

## Step 6: Generate "check_2" and "check_3" most consistent queries as in step 4.

check_2 <- tbl(db_conn, "enrollment") %>%
  filter(check_2 == 1, is.na(check_1)) %>%
  dplyr::select(-matches("(check)|(translado)")) %>%
  group_by(n_doc) %>%
  filter(n() == 1, grado_2014 == 16)

check_3 <- tbl(db_conn, "enrollment") %>%
  filter(check_3 == 1, is.na(check_1)) %>%
  dplyr::select(-matches("(check)|(translado)")) %>%
  group_by(n_doc) %>%
  filter(n() == 1, grado_2014 == 2)

## Step 7: Get id numbers already existing in students_siagie table

ids <- tbl(db_conn, "students_siagie") %>%
  dplyr::select(ndoc) %>%
  collect()

## Step 8: Bind rows from step 6 queries, and copy to "students_siagie" table
## variables in table paterno, materno, nombres, fnac, mujer, ndoc, valid_ndoc, security_level
## valid_ndoc == TRUE, since every number exists on the official registries, however no name/id match is guaranteed
## security_level == 1, highest possible certainty of identification of individual students
## SQL table details: "students_siagie"
### primary key (paterno, materno, nombres, mujer, fnac)
### ndoc unique constraint
### id serial

table <- union(check_2, check_3) %>%
  dplyr::select(n_doc, paterno, materno, nombres, fnac, mujer) %>%
  rename(ndoc = n_doc) %>%
  mutate(
    valid_ndoc = TRUE,
    security_level = 1
  ) %>%
  group_by(ndoc) %>%
  filter(n() == 1) %>%
  group_by(paterno, materno, nombres) %>%
  mutate(homonyms = n()-1) %>%
  collect() %>%
  anti_join(ids)

###### SECOND FILTER (most secure, security_level == 1)
###### Identified: 392454
###### By: father's surname, mother's surname, names, sex, birthday, unique ID number.

dbWriteTable(db_conn, name = "students_siagie", table, temporary = FALSE, append = TRUE)

###### FIRST CHECK (security_level == 1)
###### Identified: 5508664 (first filter) + 329128 (second filter) = 5837792
###### By: father's surname, mother's surname, names, sex, birthday, unique ID number.
###### Details: No homonyms

tbl(db_conn, "students_siagie") %>%
  filter(homonyms > 0)

# Clear variables
rm(check_2, check_3, table, ids)

## Step 9: Get remaining observations from step 6 (less consistent)

check_2 <- tbl(db_conn, "enrollment") %>%
  filter(check_2 == 1, is.na(check_1)) %>%
  dplyr::select(-matches("(check)|(translado)")) %>%
  group_by(n_doc) %>%
  filter(n() == 1, grado_2014 != 16)

check_3 <- tbl(db_conn, "enrollment") %>%
  filter(check_3 == 1, is.na(check_1)) %>%
  dplyr::select(-matches("(check)|(translado)")) %>%
  group_by(n_doc) %>%
  filter(n() == 1, grado_2014 != 2)

## Step 10: Update id numbers already existing in students_siagie table

ids <- tbl(db_conn, "students_siagie") %>%
  dplyr::select(ndoc) %>%
  collect()

## Step 11: Bind rows from step 9 queries

table <- union(check_2, check_3) %>%
  dplyr::select(n_doc, paterno, materno, nombres, fnac, mujer) %>%
  rename(ndoc = n_doc) %>%
  mutate(
    valid_ndoc = TRUE,
    security_level = 2
  ) %>%
  group_by(ndoc) %>%
  filter(n() == 1) %>%
  group_by(paterno, materno, nombres) %>%
  mutate(homonyms = n()-1) %>%
  collect() %>%
  anti_join(ids)

###### THIRD FILTER (security_level == 2)
###### Identified: 715557
###### By: father's surname, mother's surname, names, sex, birthday, unique ID number.

dbWriteTable(db_conn, name = "students_siagie", table, temporary = FALSE, append = TRUE)

###### SECOND CHECK (security_level %in% c(1,2))
###### Identified: 5837792 (first check) + 715557 (third filter) = 6616349
###### By: father's surname, mother's surname, names, sex, birthday, unique ID number.
###### Details: No homonyms

tbl(db_conn, "students_siagie") %>%
  filter(homonyms > 0)

# Clear variables
rm(check_2, check_3, table, ids)

## Step 12: Prepare data for check join the student id table

ece <- tbl(db_conn, "ece") %>%
  dplyr::select(paterno, materno, nombres, mujer, ndoc) %>%
  rename(ndoc_ece = ndoc)

sisfoh <- tbl(db_conn, "sisfoh") %>%
  dplyr::select(paterno, materno, nombres, mujer, fnac, ndoc) %>%
  rename(
    ndoc_sisfoh = n_doc,
    fnac_sisfoh = fnac
  )

## Step 13: Join queries
join_ece <- tbl(db_conn, "students_siagie") %>%
  mutate(mujer = as.integer(mujer)) %>%
  inner_join(ece)

join_sisfoh <- tbl(db_conn, "students_siagie") %>%
  mutate(mujer = as.integer(mujer)) %>%
  inner_join(sisfoh)

## Step 14: Bind rows from step 13 queries and check document

join <- inner_join(join_ece, join_sisfoh) %>%
  collect()

table <- join %>%
  mutate(
    ndoc_ece = str_remove_all(ndoc_ece, "[:alpha:]") %>% as.integer(),
    ndoc_sisfoh = str_remove_all(ndoc_sisfoh, "[:alpha:]") %>% as.integer(),
    across(where(is.integer), ~ if_else(.x == 99999999, NA_integer_, .x)),
    mujer = as.logical(mujer),
    valid_fnac = validate_two(fnac, fnac_sisfoh),
    valid_ndoc = case_when(
      ndoc == ndoc_sisfoh & ndoc == ndoc_ece ~ 1L,
      ndoc == ndoc_sisfoh & is.na(ndoc_ece) ~ 2L,
      is.na(ndoc_sisfoh) & ndoc == ndoc_ece ~ 3L,
      ndoc != ndoc_sisfoh & ndoc == ndoc_ece ~ 4L,
      ndoc == ndoc_sisfoh & ndoc != ndoc_ece ~ 5L,
      ndoc_sisfoh == ndoc_ece & ndoc != ndoc_ece ~ 6L,
      is.na(ndoc_sisfoh) & ndoc != ndoc_ece ~ 7L,
      ndoc != ndoc_sisfoh & is.na(ndoc_ece) ~ 8L,
      ndoc != ndoc_sisfoh & ndoc != ndoc_ece ~ 9L,
      is.na(ndoc_sisfoh) & is.na(ndoc_ece) ~ 10L
    )
  ) %>% 
  group_by(ndoc) %>%
  arrange(valid_ndoc, valid_fnac, .by_group = TRUE) %>%
  filter(row_number()==1) %>%
  filter(valid_ndoc <= 3, valid_fnac == 1) %>%
  dplyr::select(-matches("(ndoc_)|(fnac_)")) %>%
  group_by(paterno, materno, nombres) %>%
  mutate(
    homonyms = n()-1,
    filter_stage = 1
  )

###### FIRST JOIN FILTER (security_level == 1)
###### Identified: 2,087,518
###### By: father's surname, mother's surname, names, sex, birthday, unique ID number.
###### Matching observations in sisfoh and ece
###### ECE by: paterno, materno, nombres, mujer, ndoc
###### sisfoh by: paterno, materno, nombres, mujer, fnac, ndoc

dbWriteTable(db_conn, name = "student_id", table, temporary = FALSE, append = TRUE)

# Clear variables
rm(ece, sisfoh, join_ece, join_sisfoh, join, table)

## Step 15: Update id numbers already existing in student_id table

ids <- tbl(db_conn, "student_id") %>%
  dplyr::select(ndoc) %>%
  collect()

## Step 16: Remove already identified individuals from students_siagie

table <- tbl(db_conn, "students_siagie") %>%
  collect() %>%
  anti_join(ids)

dbWriteTable(db_conn, name = "students_siagie", table, temporary = FALSE, overwrite = TRUE)

# Clear variables
rm(ids, table)

## Step 17: Select individuals that haven't changed school from 2013 to 2015

filter_school <- tbl(db_conn, "enrollment") %>%
  filter(cod_mod_2013 == cod_mod_2014, cod_mod_2014 == cod_mod_2015) %>%
  dplyr::select(paterno, materno, nombres, n_doc, fnac, mujer, cod_mod_2013) %>%
  rename(ndoc = n_doc, cod_mod = cod_mod_2013)

## Step 18: Join tables "students_siagie" and "filter_school"

join_baseline <- inner_join(tbl(db_conn, "students_siagie"), filter_school) %>%  
  compute(
    name = "students_siagie_codmod",
    temporary = TRUE
  )

## Step 19: Prepare data for inner-joining with the "join_baseline" table

ece <- tbl(db_conn, "ece_raw") %>%
  dplyr::select(paterno, materno, nombres, mujer, n_doc, cod_mod7) %>%
  rename(ndoc_ece = n_doc, cod_mod = cod_mod7) %>%
  mutate(cod_mod = as.integer(cod_mod))

sisfoh <- tbl(db_conn, "sisfoh_raw") %>%
  dplyr::select(paterno, materno, nombres, mujer, fnac, n_doc) %>%
  rename(
    ndoc_sisfoh = n_doc,
    fnac_sisfoh = fnac
  )

## Step 20: Join queries
join_ece <- join_baseline %>%
  mutate(mujer = as.integer(mujer)) %>%
  inner_join(ece)

join_sisfoh <- join_baseline %>%
  mutate(mujer = as.integer(mujer)) %>%
  inner_join(sisfoh)

## Step 21: Bind rows from step 13 queries and check document

join <- inner_join(join_ece, join_sisfoh) %>%
  collect()

table <- join %>%
  mutate(
    ndoc_ece = str_remove_all(ndoc_ece, "[:alpha:]") %>% as.integer(),
    ndoc_sisfoh = str_remove_all(ndoc_sisfoh, "[:alpha:]") %>% as.integer(),
    across(where(is.integer), ~ if_else(.x == 99999999, NA_integer_, .x)),
    mujer = as.logical(mujer),
    diff_fnac = (fnac %--% fnac_sisfoh) %>% as.period(),
    diff_fnac = case_when(
      diff_fnac == period(seconds = 0) ~ 1, #Identical
      diff_fnac == period(year = 1) ~ 2, #sisfoh date is a possible typo (upwards)
      diff_fnac == period(month = 1) ~ 2,
      diff_fnac == period(day = 1) ~ 2,
      diff_fnac == period(year = -1) ~ -2, #sisfoh date is a possible typo (downwards)
      diff_fnac == period(month = -1) ~ -2,
      diff_fnac == period(day = -1) ~ -2,
      TRUE ~ 3 # Very different
    ),
    valid_fnac = case_when(
      fnac == fnac_sisfoh ~ 1L,
      fnac != fnac_sisfoh & diff_fnac == 2 ~ 2L,
      fnac != fnac_sisfoh & diff_fnac == -2 ~ 3L,
      fnac != fnac_sisfoh ~ 4L,
      is.na(fnac_sisfoh) ~ 5L,
    ),
    valid_ndoc = case_when(
      ndoc == ndoc_sisfoh & ndoc == ndoc_ece ~ 1L,
      ndoc == ndoc_sisfoh & is.na(ndoc_ece) ~ 2L,
      is.na(ndoc_sisfoh) & ndoc == ndoc_ece ~ 3L,
      ndoc != ndoc_sisfoh & ndoc == ndoc_ece ~ 4L,
      ndoc == ndoc_sisfoh & ndoc != ndoc_ece ~ 5L,
      ndoc_sisfoh == ndoc_ece & ndoc != ndoc_ece ~ 6L,
      is.na(ndoc_sisfoh) & ndoc != ndoc_ece ~ 7L,
      ndoc != ndoc_sisfoh & is.na(ndoc_ece) ~ 8L,
      ndoc != ndoc_sisfoh & ndoc != ndoc_ece ~ 9L,
      is.na(ndoc_sisfoh) & is.na(ndoc_ece) ~ 10L
    )
  ) %>% 
  group_by(ndoc) %>%
  arrange(valid_ndoc, valid_fnac, .by_group = TRUE) %>%
  filter(row_number()==1) %>%
  filter(valid_ndoc <= 6) %>%
  mutate(
    ndoc = if_else(valid_ndoc == 6, ndoc_sisfoh, ndoc),
    fnac = if_else(valid_ndoc == 6, fnac_sisfoh, fnac),
  ) %>%
  dplyr::select(-matches("(ndoc_)|(fnac_)|(diff_)|(cod_mod)")) %>%
  group_by(paterno, materno, nombres) %>%
  mutate(
    homonyms = n()-1,
    filter_stage = 2L
  )

###### SECOND JOIN FILTER (security_level == 1)
###### Identified: 62,745
###### By: father's surname, mother's surname, names, sex, birthday, unique ID number, cod_mod.
###### Matching observations in sisfoh and ece
###### ECE by: paterno, materno, nombres, mujer, ndoc, cod_mod
###### sisfoh by: paterno, materno, nombres, mujer, fnac, ndoc

dbWriteTable(db_conn, name = "student_id", table, temporary = FALSE, append = TRUE)

# Clear variables
rm(ece, sisfoh, join_ece, join_sisfoh, join, join_baseline, table, filter_school)

## Step 22: Update id numbers already existing in student_id table

ids <- tbl(db_conn, "student_id") %>%
  dplyr::select(ndoc) %>%
  collect()

## Step 23: Remove already identified individuals from students_siagie
## valid_ndoc == 6 individuals are still in the table, since ndocs were changed

table <- tbl(db_conn, "students_siagie") %>%
  collect() %>%
  anti_join(ids)

dbWriteTable(db_conn, name = "students_siagie", table, temporary = FALSE, overwrite = TRUE)

# Clear variables
rm(ids, table)

## Step 24: Select individuals that haven't changed school from 2013 to 2014 or 2014 to 2015

filter_school <- tbl(db_conn, "enrollment") %>%
  filter((cod_mod_2013 == cod_mod_2014 & is.na(cod_mod_2015)) | (cod_mod_2014 == cod_mod_2015 & is.na(cod_mod_2013))) %>%
  dplyr::select(paterno, materno, nombres, n_doc, fnac, mujer, cod_mod_2014) %>%
  rename(ndoc = n_doc, cod_mod = cod_mod_2014)

## Step 25: Join tables "students_siagie" and "filter_school"

join_baseline <- inner_join(tbl(db_conn, "students_siagie"), filter_school) %>%  
  compute(
    name = "students_siagie_codmod2",
    temporary = TRUE
  )

## Step 26: Prepare data for inner-joining with the "join_baseline" table

ece <- tbl(db_conn, "ece_raw") %>%
  dplyr::select(paterno, materno, nombres, mujer, n_doc, cod_mod7) %>%
  rename(ndoc_ece = n_doc, cod_mod = cod_mod7) %>%
  mutate(cod_mod = as.integer(cod_mod))

sisfoh <- tbl(db_conn, "sisfoh_raw") %>%
  dplyr::select(paterno, materno, nombres, mujer, fnac, n_doc) %>%
  rename(
    ndoc_sisfoh = n_doc,
    fnac_sisfoh = fnac
  )

## Step 27: Join queries
join_ece <- join_baseline %>%
  mutate(mujer = as.integer(mujer)) %>%
  inner_join(ece)

join_sisfoh <- join_baseline %>%
  mutate(mujer = as.integer(mujer)) %>%
  inner_join(sisfoh)

## Step 28: Bind rows from step 13 queries and check document

join <- inner_join(join_ece, join_sisfoh) %>%
  collect()

table <- join %>%
  mutate(
    ndoc_ece = str_remove_all(ndoc_ece, "[:alpha:]") %>% as.integer(),
    ndoc_sisfoh = str_remove_all(ndoc_sisfoh, "[:alpha:]") %>% as.integer(),
    across(where(is.integer), ~ if_else(.x == 99999999, NA_integer_, .x)),
    mujer = as.logical(mujer),
    diff_fnac = (fnac %--% fnac_sisfoh) %>% as.period(),
    diff_fnac = case_when(
      diff_fnac == period(seconds = 0) ~ 1, #Identical
      diff_fnac == period(year = 1) ~ 2, #sisfoh date is a possible typo (upwards)
      diff_fnac == period(month = 1) ~ 2,
      diff_fnac == period(day = 1) ~ 2,
      diff_fnac == period(year = -1) ~ -2, #sisfoh date is a possible typo (downwards)
      diff_fnac == period(month = -1) ~ -2,
      diff_fnac == period(day = -1) ~ -2,
      TRUE ~ 3 # Very different
    ),
    valid_fnac = case_when(
      fnac == fnac_sisfoh ~ 1L,
      fnac != fnac_sisfoh & diff_fnac == 2 ~ 2L,
      fnac != fnac_sisfoh & diff_fnac == -2 ~ 3L,
      fnac != fnac_sisfoh ~ 4L,
      is.na(fnac_sisfoh) ~ 5L,
    ),
    valid_ndoc = case_when(
      ndoc == ndoc_sisfoh & ndoc == ndoc_ece ~ 1L,
      ndoc == ndoc_sisfoh & is.na(ndoc_ece) ~ 2L,
      is.na(ndoc_sisfoh) & ndoc == ndoc_ece ~ 3L,
      ndoc != ndoc_sisfoh & ndoc == ndoc_ece ~ 4L,
      ndoc == ndoc_sisfoh & ndoc != ndoc_ece ~ 5L,
      ndoc_sisfoh == ndoc_ece & ndoc != ndoc_ece ~ 6L,
      is.na(ndoc_sisfoh) & ndoc != ndoc_ece ~ 7L,
      ndoc != ndoc_sisfoh & is.na(ndoc_ece) ~ 8L,
      ndoc != ndoc_sisfoh & ndoc != ndoc_ece ~ 9L,
      is.na(ndoc_sisfoh) & is.na(ndoc_ece) ~ 10L
    )
  ) %>% 
  group_by(ndoc) %>%
  arrange(valid_ndoc, valid_fnac, .by_group = TRUE) %>%
  filter(row_number()==1) %>%
  filter(valid_ndoc <= 6) %>%
  mutate(
    ndoc = if_else(valid_ndoc == 6, ndoc_sisfoh, ndoc),
    fnac = if_else(valid_ndoc == 6, fnac_sisfoh, fnac),
  ) %>%
  dplyr::select(-matches("(ndoc_)|(fnac_)|(diff_)|(cod_mod)")) %>%
  group_by(paterno, materno, nombres) %>%
  mutate(
    homonyms = n()-1,
    filter_stage = 3L
  )

###### THIRD JOIN FILTER (security_level == 1)
###### Identified: 5796
###### By: father's surname, mother's surname, names, sex, birthday, unique ID number, cod_mod.
###### Matching observations in sisfoh and ece
###### ECE by: paterno, materno, nombres, mujer, ndoc, cod_mod
###### sisfoh by: paterno, materno, nombres, mujer, fnac, ndoc

dbWriteTable(db_conn, name = "student_id", table, temporary = FALSE, append = TRUE)

# Clear variables
rm(ece, sisfoh, join_ece, join_sisfoh, join, join_baseline, table, filter_school)

## Step 29: Update id numbers already existing in student_id table

ids <- tbl(db_conn, "student_id") %>%
  dplyr::select(ndoc) %>%
  collect()

## Step 30: Remove already identified individuals from students_siagie
## valid_ndoc == 6 individuals are still in the table, since ndocs were changed

table <- tbl(db_conn, "students_siagie") %>%
  collect() %>%
  anti_join(ids)

dbWriteTable(db_conn, name = "students_siagie", table, temporary = FALSE, overwrite = TRUE)

# Clear variables
rm(ids, table)

### CHANGE OF STRATEGY ######################################################################
### SELECT INDIVIDUALS BY YEAR ##############################################################

## Step 31: Create sisfoh raw query

sisfoh <- tbl(db_conn, "sisfoh_raw") %>%
  dplyr::select(paterno, materno, nombres, mujer, fnac, n_doc) %>%
  rename(
    ndoc_sisfoh = n_doc,
    fnac_sisfoh = fnac
  )

## Step 32: Select individuals in 2013

filter_school_2013 <- tbl(db_conn, "enrollment") %>%
  filter(!is.na(cod_mod_2013)) %>%
  dplyr::select(paterno, materno, nombres, n_doc, fnac, mujer, cod_mod_2013) %>%
  rename(ndoc = n_doc, cod_mod = cod_mod_2013)

join_baseline_2013 <- inner_join(tbl(db_conn, "students_siagie"), filter_school_2013) %>%  
  compute(
    name = "students_siagie_codmod_2013",
    temporary = TRUE
  )

ece_2013 <- tbl(db_conn, "ece_raw") %>%
  filter(periodo == 2013) %>%
  dplyr::select(paterno, materno, nombres, mujer, cod_mod7) %>%
  rename(cod_mod = cod_mod7) %>%
  mutate(cod_mod = as.integer(cod_mod))

join_ece_2013 <- join_baseline_2013 %>%
  mutate(mujer = as.integer(mujer)) %>%
  inner_join(ece_2013) %>%
  compute(
    name = "join_ece_2013",
    temporary = TRUE
  )

join_sisfoh_2013 <- join_baseline_2013 %>%
  mutate(mujer = as.integer(mujer)) %>%
  inner_join(sisfoh)

## Step 33: Select individuals in 2014

filter_school_2014 <- tbl(db_conn, "enrollment") %>%
  filter(!is.na(cod_mod_2014)) %>%
  dplyr::select(paterno, materno, nombres, n_doc, fnac, mujer, cod_mod_2014) %>%
  rename(ndoc = n_doc, cod_mod = cod_mod_2014)

join_baseline_2014 <- inner_join(tbl(db_conn, "students_siagie"), filter_school_2014) %>%  
  compute(
    name = "students_siagie_codmod_2014",
    temporary = TRUE
  )

ece_2014 <- tbl(db_conn, "ece_raw") %>%
  filter(periodo == 2014) %>%
  dplyr::select(paterno, materno, nombres, mujer, n_doc, cod_mod7) %>%
  rename(ndoc_ece = n_doc, cod_mod = cod_mod7) %>%
  mutate(cod_mod = as.integer(cod_mod))

join_ece_2014 <- join_baseline_2014 %>%
  mutate(mujer = as.integer(mujer)) %>%
  inner_join(ece_2014) %>%
  compute(
    name = "join_ece_2014",
    temporary = TRUE
  )

join_sisfoh_2014 <- join_baseline_2014 %>%
  mutate(mujer = as.integer(mujer)) %>%
  inner_join(sisfoh)

## Step 34: Select individuals in 2015

filter_school_2015 <- tbl(db_conn, "enrollment") %>%
  filter(!is.na(cod_mod_2015)) %>%
  dplyr::select(paterno, materno, nombres, n_doc, fnac, mujer, cod_mod_2015) %>%
  rename(ndoc = n_doc, cod_mod = cod_mod_2015)

join_baseline_2015 <- inner_join(tbl(db_conn, "students_siagie"), filter_school_2015) %>%  
  compute(
    name = "students_siagie_codmod_2015",
    temporary = TRUE
  )

ece_2015 <- tbl(db_conn, "ece_raw") %>%
  filter(periodo == 2015) %>%
  dplyr::select(paterno, materno, nombres, mujer, n_doc, cod_mod7) %>%
  rename(ndoc_ece = n_doc, cod_mod = cod_mod7) %>%
  mutate(cod_mod = as.integer(cod_mod))

join_ece_2015 <- join_baseline_2015 %>%
  mutate(mujer = as.integer(mujer)) %>%
  inner_join(ece_2015) %>%
  compute(
    name = "join_ece_2015",
    temporary = TRUE
  )

join_sisfoh_2015 <- join_baseline_2015 %>%
  mutate(mujer = as.integer(mujer)) %>%
  inner_join(sisfoh)

## JOINS

join_2013 <- inner_join(join_ece_2013, join_sisfoh_2013) %>%
  collect()

join_2014 <- inner_join(join_ece_2014, join_sisfoh_2014) %>%
  collect()

join_2015 <- inner_join(join_ece_2015, join_sisfoh_2015) %>%
  collect()

table_2013 <- join_2013 %>%
  mutate(
    ndoc_ece = NA_integer_,
    ndoc_sisfoh = str_remove_all(ndoc_sisfoh, "[:alpha:]") %>% as.integer(),
    across(where(is.integer), ~ if_else(.x == 99999999, NA_integer_, .x)),
    mujer = as.logical(mujer),
    diff_fnac = (fnac %--% fnac_sisfoh) %>% as.period(),
    diff_fnac = case_when(
      diff_fnac == period(seconds = 0) ~ 1, #Identical
      diff_fnac == period(year = 1) ~ 2, #sisfoh date is a possible typo (upwards)
      diff_fnac == period(month = 1) ~ 2,
      diff_fnac == period(day = 1) ~ 2,
      diff_fnac == period(year = -1) ~ -2, #sisfoh date is a possible typo (downwards)
      diff_fnac == period(month = -1) ~ -2,
      diff_fnac == period(day = -1) ~ -2,
      TRUE ~ 3 # Very different
    ),
    valid_fnac = case_when(
      fnac == fnac_sisfoh ~ 1L,
      fnac != fnac_sisfoh & diff_fnac == 2 ~ 2L,
      fnac != fnac_sisfoh & diff_fnac == -2 ~ 3L,
      fnac != fnac_sisfoh ~ 4L,
      is.na(fnac_sisfoh) ~ 5L,
    ),
    valid_ndoc = case_when(
      ndoc == ndoc_sisfoh & ndoc == ndoc_ece ~ 1L,
      ndoc == ndoc_sisfoh & is.na(ndoc_ece) ~ 2L,
      is.na(ndoc_sisfoh) & ndoc == ndoc_ece ~ 3L,
      ndoc != ndoc_sisfoh & ndoc == ndoc_ece ~ 4L,
      ndoc == ndoc_sisfoh & ndoc != ndoc_ece ~ 5L,
      ndoc_sisfoh == ndoc_ece & ndoc != ndoc_ece ~ 6L,
      is.na(ndoc_sisfoh) & ndoc != ndoc_ece ~ 7L,
      ndoc != ndoc_sisfoh & is.na(ndoc_ece) ~ 8L,
      ndoc != ndoc_sisfoh & ndoc != ndoc_ece ~ 9L,
      is.na(ndoc_sisfoh) & is.na(ndoc_ece) ~ 10L
    )
  ) %>% 
  group_by(ndoc) %>%
  arrange(valid_ndoc, valid_fnac, .by_group = TRUE) %>%
  filter(row_number()==1) 

table_2014 <- join_2014 %>%
  mutate(
    ndoc_ece = str_remove_all(ndoc_ece, "[:alpha:]") %>% as.integer(),
    ndoc_sisfoh = str_remove_all(ndoc_sisfoh, "[:alpha:]") %>% as.integer(),
    across(where(is.integer), ~ if_else(.x == 99999999, NA_integer_, .x)),
    mujer = as.logical(mujer),
    diff_fnac = (fnac %--% fnac_sisfoh) %>% as.period(),
    diff_fnac = case_when(
      diff_fnac == period(seconds = 0) ~ 1, #Identical
      diff_fnac == period(year = 1) ~ 2, #sisfoh date is a possible typo (upwards)
      diff_fnac == period(month = 1) ~ 2,
      diff_fnac == period(day = 1) ~ 2,
      diff_fnac == period(year = -1) ~ -2, #sisfoh date is a possible typo (downwards)
      diff_fnac == period(month = -1) ~ -2,
      diff_fnac == period(day = -1) ~ -2,
      TRUE ~ 3 # Very different
    ),
    valid_fnac = case_when(
      fnac == fnac_sisfoh ~ 1L,
      fnac != fnac_sisfoh & diff_fnac == 2 ~ 2L,
      fnac != fnac_sisfoh & diff_fnac == -2 ~ 3L,
      fnac != fnac_sisfoh ~ 4L,
      is.na(fnac_sisfoh) ~ 5L,
    ),
    valid_ndoc = case_when(
      ndoc == ndoc_sisfoh & ndoc == ndoc_ece ~ 1L,
      ndoc == ndoc_sisfoh & is.na(ndoc_ece) ~ 2L,
      is.na(ndoc_sisfoh) & ndoc == ndoc_ece ~ 3L,
      ndoc != ndoc_sisfoh & ndoc == ndoc_ece ~ 4L,
      ndoc == ndoc_sisfoh & ndoc != ndoc_ece ~ 5L,
      ndoc_sisfoh == ndoc_ece & ndoc != ndoc_ece ~ 6L,
      is.na(ndoc_sisfoh) & ndoc != ndoc_ece ~ 7L,
      ndoc != ndoc_sisfoh & is.na(ndoc_ece) ~ 8L,
      ndoc != ndoc_sisfoh & ndoc != ndoc_ece ~ 9L,
      is.na(ndoc_sisfoh) & is.na(ndoc_ece) ~ 10L
    )
  ) %>% 
  group_by(ndoc) %>%
  arrange(valid_ndoc, valid_fnac, .by_group = TRUE) %>%
  filter(row_number()==1) 

table_2015 <- join_2015 %>%
  mutate(
    ndoc_ece = str_remove_all(ndoc_ece, "[:alpha:]") %>% as.integer(),
    ndoc_sisfoh = str_remove_all(ndoc_sisfoh, "[:alpha:]") %>% as.integer(),
    across(where(is.integer), ~ if_else(.x == 99999999, NA_integer_, .x)),
    mujer = as.logical(mujer),
    diff_fnac = (fnac %--% fnac_sisfoh) %>% as.period(),
    diff_fnac = case_when(
      diff_fnac == period(seconds = 0) ~ 1, #Identical
      diff_fnac == period(year = 1) ~ 2, #sisfoh date is a possible typo (upwards)
      diff_fnac == period(month = 1) ~ 2,
      diff_fnac == period(day = 1) ~ 2,
      diff_fnac == period(year = -1) ~ -2, #sisfoh date is a possible typo (downwards)
      diff_fnac == period(month = -1) ~ -2,
      diff_fnac == period(day = -1) ~ -2,
      TRUE ~ 3 # Very different
    ),
    valid_fnac = case_when(
      fnac == fnac_sisfoh ~ 1L,
      fnac != fnac_sisfoh & diff_fnac == 2 ~ 2L,
      fnac != fnac_sisfoh & diff_fnac == -2 ~ 3L,
      fnac != fnac_sisfoh ~ 4L,
      is.na(fnac_sisfoh) ~ 5L,
    ),
    valid_ndoc = case_when(
      ndoc == ndoc_sisfoh & ndoc == ndoc_ece ~ 1L,
      ndoc == ndoc_sisfoh & is.na(ndoc_ece) ~ 2L,
      is.na(ndoc_sisfoh) & ndoc == ndoc_ece ~ 3L,
      ndoc != ndoc_sisfoh & ndoc == ndoc_ece ~ 4L,
      ndoc == ndoc_sisfoh & ndoc != ndoc_ece ~ 5L,
      ndoc_sisfoh == ndoc_ece & ndoc != ndoc_ece ~ 6L,
      is.na(ndoc_sisfoh) & ndoc != ndoc_ece ~ 7L,
      ndoc != ndoc_sisfoh & is.na(ndoc_ece) ~ 8L,
      ndoc != ndoc_sisfoh & ndoc != ndoc_ece ~ 9L,
      is.na(ndoc_sisfoh) & is.na(ndoc_ece) ~ 10L
    )
  ) %>% 
  group_by(ndoc) %>%
  arrange(valid_ndoc, valid_fnac, .by_group = TRUE) %>%
  filter(row_number()==1) 

table <- bind_rows(table_2013, table_2014, table_2015) %>%
  group_by(ndoc) %>%
  arrange(valid_ndoc, valid_fnac, .by_group = TRUE) %>%
  filter(row_number()==1) %>%
  filter(valid_ndoc <= 6) %>%
  mutate(
    ndoc = if_else(valid_ndoc == 6, ndoc_sisfoh, ndoc),
    fnac = if_else(valid_ndoc == 6, fnac_sisfoh, fnac),
  ) %>%
  dplyr::select(-matches("(ndoc_)|(fnac_)|(diff_)|(cod_mod)")) %>%
  group_by(paterno, materno, nombres) %>%
  mutate(
    homonyms = n()-1,
    filter_stage = 4L
  )

## Duplicates were found
## ERROR:  duplicate key value violates unique constraint "student_id_pkey1"
## Key (paterno, materno, nombres, mujer, fnac)=(taipe, hernandez, naysha mayli, t, 2007-01-27) already exists.
## Duplicates were found because of the last stage filter (4) valid_ndoc == 6 change.

ids <- tbl(db_conn, "student_id") %>%
  dplyr::select(ndoc) %>%
  collect()

table %<>% anti_join(ids)

###### FOURTH JOIN FILTER (security_level == 1)
###### Identified: 65289 (without duplicates) 5x2 (duplicates) 2,221,348 (Total)
###### By: father's surname, mother's surname, names, sex, birthday, unique ID number, cod_mod.
###### Matching observations in sisfoh and ece
###### ECE by: paterno, materno, nombres, mujer, ndoc, cod_mod, periodo
###### sisfoh by: paterno, materno, nombres, mujer, fnac, ndoc

dbWriteTable(db_conn, name = "student_id", table, temporary = FALSE, append = TRUE)

# Clear variables
rm(list = ls(pattern = "(ece_)|(filter_school_)|(join_)|(table)|(data)|(sisfoh)"))

## Step 29: Update id numbers already existing in student_id table

ids <- tbl(db_conn, "student_id") %>%
  dplyr::select(ndoc) %>%
  collect()

## Step 30: Remove already identified individuals from students_siagie
## valid_ndoc == 6 individuals are still in the table, since ndocs were changed

table <- tbl(db_conn, "students_siagie") %>%
  collect() %>%
  anti_join(ids)

dbWriteTable(db_conn, name = "students_siagie", table, temporary = FALSE, overwrite = TRUE)

# Clear variables
rm(ids, table)

### CHANGE OF STRATEGY ######################################################################
### SELECT ALL OBSERVATIONS FROM INDIVIDUAL TABLES ##########################################

## ECE

ece <- tbl(db_conn, "ece_raw") %>%
  dplyr::select(paterno, materno, nombres, mujer, n_doc) %>%
  rename(ndoc_ece = n_doc)

join_ece <- tbl(db_conn, "students_siagie") %>%
  mutate(mujer = as.integer(mujer)) %>%
  inner_join(ece) %>%
  compute(
    name = "join_ece",
    temporary = TRUE
  )

## sisfoh

sisfoh <- tbl(db_conn, "sisfoh_raw") %>%
  dplyr::select(paterno, materno, nombres, mujer, fnac, n_doc) %>%
  rename(
    ndoc_sisfoh = n_doc,
    fnac_sisfoh = fnac
  )

join_sisfoh <- tbl(db_conn, "students_siagie") %>%
  mutate(mujer = as.integer(mujer)) %>%
  inner_join(sisfoh) %>%
  compute(
    name = "join_sisfoh",
    temporary = TRUE
  )

## FULL JOIN

join <- full_join(join_ece, join_sisfoh) %>%
  collect()

table <- join %>%
  mutate(
    ndoc_ece = str_remove_all(ndoc_ece, "[:alpha:]") %>% as.integer(),
    ndoc_sisfoh = str_remove_all(ndoc_sisfoh, "[:alpha:]") %>% as.integer(),
    across(where(is.integer), ~ if_else(.x == 99999999, NA_integer_, .x)),
    mujer = as.logical(mujer),
    diff_fnac = (fnac %--% fnac_sisfoh) %>% as.period(),
    diff_fnac = case_when(
      diff_fnac == period(seconds = 0) ~ 1, #Identical
      diff_fnac == period(year = 1) ~ 2, #sisfoh date is a possible typo (upwards)
      diff_fnac == period(month = 1) ~ 2,
      diff_fnac == period(day = 1) ~ 2,
      diff_fnac == period(year = -1) ~ -2, #sisfoh date is a possible typo (downwards)
      diff_fnac == period(month = -1) ~ -2,
      diff_fnac == period(day = -1) ~ -2,
      TRUE ~ 3 # Very different
    ),
    valid_fnac = case_when(
      fnac == fnac_sisfoh ~ 1L,
      fnac != fnac_sisfoh & diff_fnac == 2 ~ 2L,
      fnac != fnac_sisfoh & diff_fnac == -2 ~ 3L,
      fnac != fnac_sisfoh ~ 4L,
      is.na(fnac_sisfoh) ~ 5L,
    ),
    valid_ndoc = case_when(
      ndoc == ndoc_sisfoh & ndoc == ndoc_ece ~ 1L,
      ndoc == ndoc_sisfoh & is.na(ndoc_ece) ~ 2L,
      is.na(ndoc_sisfoh) & ndoc == ndoc_ece ~ 3L,
      ndoc != ndoc_sisfoh & ndoc == ndoc_ece ~ 4L,
      ndoc == ndoc_sisfoh & ndoc != ndoc_ece ~ 5L,
      ndoc_sisfoh == ndoc_ece & ndoc != ndoc_ece ~ 6L,
      is.na(ndoc_sisfoh) & ndoc != ndoc_ece ~ 7L,
      ndoc != ndoc_sisfoh & is.na(ndoc_ece) ~ 8L,
      ndoc != ndoc_sisfoh & ndoc != ndoc_ece ~ 9L,
      is.na(ndoc_sisfoh) & is.na(ndoc_ece) ~ 10L
    )
  ) %>% 
  group_by(ndoc) %>%
  arrange(valid_ndoc, valid_fnac, .by_group = TRUE) %>%
  filter(row_number()==1) %>%
  filter(valid_ndoc <= 3) %>%
  dplyr::select(-matches("(ndoc_)|(fnac_)|(diff_)")) %>%
  group_by(paterno, materno, nombres) %>%
  mutate(
    homonyms = n()-1,
    filter_stage = 5L
  )

#ids
ids <- tbl(db_conn, "student_id") %>%
  dplyr::select(ndoc) %>%
  collect()

table %<>% anti_join(ids)
  
###### FIFTH CHECK JOIN FILTER (security_level == 1)
###### Identified: 2,716,772
###### By: father's surname, mother's surname, names, sex, birthday, unique ID number.
###### Details: No homonyms

dbWriteTable(db_conn, name = "student_id", table, temporary = FALSE, append = TRUE)

# Clear variables
rm(list = ls(pattern = "(ece)|(sisfoh)|(join)|(ids)|(table)"))

## Step 29: Update id numbers already existing in student_id table

ids <- tbl(db_conn, "student_id") %>%
  dplyr::select(ndoc) %>%
  collect()

## Step 30: Remove already identified individuals from students_siagie
## valid_ndoc == 6 individuals are still in the table, since ndocs were changed

table <- tbl(db_conn, "students_siagie") %>%
  collect() %>%
  anti_join(ids)

dbWriteTable(db_conn, name = "students_siagie", table, temporary = FALSE, overwrite = TRUE)

# Clear variables
rm(ids, table)





tbl(db_conn, "students_siagie") %>%
  group_by(paterno, materno, nombres) %>%
  filter(n() == 1) %>%
  ungroup() %>%
  tally()


tbl(db_conn, "student_id") %>%
  group_by(paterno, materno, nombres) %>%
  filter(n() == 1) %>%
  ungroup() %>%
  tally()





dates <- tbl(db_conn, "enrollment") %>%
  dplyr::select(paterno, materno, nombres) %>%
  group_by(paterno, materno, nombres) %>%
  mutate(n = n()) %>%
  collect()

dates <- tbl(db_conn, "enrollment") %>%
  dplyr::select(paterno, materno, nombres, mujer) %>%
  group_by(paterno, materno, nombres, mujer) %>%
  mutate(n = n()) %>%
  collect()

estudiantes <- tbl(db_conn, "enrollment") %>%
  filter(!is.na(translado)) %>%
  collect()

estudiantes_2 <- tbl(db_conn, "enrollment") %>%
  filter(check_1 == 1 | check_2 == 1, is.na(translado)) %>%
  collect()

estudiantes_3 <- tbl(db_conn, "enrollment") %>%
  filter(check_3 == 1, is.na(translado)) %>%
  collect()

estudiantes_4 <- tbl(db_conn, "enrollment") %>%
  filter(is.na(check_1), is.na(check_2), is.na(check_3), is.na(translado)) %>%
  collect()



%>%
  collect() %>% 
  group_by(paterno, materno, nombres, mujer) %>%
  mutate(n_doc = ifelse(n_doc < 10000000, NA, n_doc),
         fnac = as.Date(ifelse(fnac > date("2015-12-31") | fnac < date("1916-01-01"), NA, fnac), origin = "1970-01-01"),
         materno = str_squish(materno) %>% str_trim() %>% replace_non_ascii(),
         paterno = str_squish(paterno) %>% str_trim() %>% replace_non_ascii(),
         nombres = str_squish(nombres) %>% str_trim() %>% replace_non_ascii(),
         across(where(is.character), str_delete_empty),
         across(where(is.character), ~ str_remove_all(.x, "[:digit:]")),
         n = n())

save.image("enrollment_s1_1801.RData")

## Step 3: Bind rows of all enrollment tables and reduce by deleting repeated obs, then:
# 1. If ID has less than 8 ciphers, change it to missing.
# 2. Keep birthday only if it lays between 1916-01-01 and 2015-12-31.
# 3. Replace non-ascii characters and clean all string variables (materno, paterno, nombres)
# 4. Count by groups (paterno, materno, nombres, mujer)

enrollment <- ls(pattern = "enrollment_") %>%
  map(~ rlang::parse_expr(.x) %>% eval) %>%
  reduce(bind_rows) %>%
  distinct() %>%
  arrange() %>%
  group_by(paterno, materno, nombres, mujer) %>%
  mutate(n_doc = ifelse(n_doc < 10000000, NA, n_doc),
         fnac = as.Date(ifelse(fnac > date("2015-12-31") | fnac < date("1916-01-01"), NA, fnac), origin = "1970-01-01"),
         materno = str_squish(materno) %>% str_trim() %>% replace_non_ascii(),
         paterno = str_squish(paterno) %>% str_trim() %>% replace_non_ascii(),
         nombres = str_squish(nombres) %>% str_trim() %>% replace_non_ascii(),
         across(where(is.character), str_delete_empty),
         across(where(is.character), ~ str_remove_all(.x, "[:digit:]")),
         n = n())

# Save progress:
save.image("enrollment_s1.RData")
rm(enrollment_2013, enrollment_2014, enrollment_2015)

# Load progress: 
load("enrollment_s1.RData")

## Step 4: Select all unique observations and write resulting table into database.

enrollment_unique <- enrollment %>%
  filter(n == 1) %>%
  select(-n) 
dbWriteTable(db_conn, name = "estudiantes", enrollment_unique)

## Step 5: Select all non-coherent ID/birthday observations, turn ID/birthday to missing and write resulting table into database.

enrollment_nonunique <- enrollment %>%
  filter(n > 1) %>%
  arrange(paterno, materno, nombres) %>% 
  mutate(n_doc = NA) %>% 
  select(-n) %>% 
  distinct()

dbWriteTable(db_conn, name = "estudiantes", enrollment_repeated, append = TRUE)

repeated <-tbl(db_conn, "estudiantes") %>% 
  group_by(n_doc) %>% 
  mutate(n = n()) %>%
  filter(n > 1, !is.na(n_doc)) %>% 
  collect()

repeated_test <- filter(repeated, between(row_number(), 115, 116))

###### SECOND FILTER
###### Identified: 730936 (unique)
###### By: Father's surname, mother's surname, names, birthday. 

## Step 1: 
# 1. Delete obs with missing birthday and reported ID from enrollment databases.
# 2. Retrieve cleaned tables without repeated values for all years.

# 2013
enrollment_2013 <- tbl(db_conn, "siagie2013_raw") %>%
  filter(!is.na(fnac), is.na(n_doc)) %>%
  select(n_doc, paterno, materno, nombres, fnac, mujer) %>% 
  group_by(paterno, materno, nombres, fnac, mujer) %>%
  distinct() %>%
  collect()

# 2014
enrollment_2014 <- tbl(db_conn, "siagie2014_raw") %>%
  filter(!is.na(fnac), is.na(n_doc)) %>%
  select(n_doc, paterno, materno, nombres, fnac, mujer) %>% 
  group_by(paterno, materno, nombres, fnac, mujer) %>%
  distinct() %>%
  collect()

# 2015
enrollment_2015 <- tbl(db_conn, "siagie2015_raw") %>%
  filter(!is.na(fnac), is.na(n_doc)) %>%
  select(n_doc, paterno, materno, nombres, fnac, mujer) %>% 
  group_by(paterno, materno, nombres, fnac, mujer) %>%
  distinct() %>%
  collect()

## Step 2: Bind rows of all enrollment tables and reduce by deleting repeated obs, then:
# 1. Replace non-ascii characters and clean all string variables (materno, paterno, nombres)
# 2. Count by groups (paterno, materno, nombres, fnac, mujer)

enrollment <- ls(pattern = "enrollment_") %>%
  map(~ rlang::parse_expr(.x) %>% eval) %>%
  reduce(bind_rows) %>%
  distinct() %>%
  arrange() %>%
  group_by(paterno, materno, nombres, fnac, mujer) %>%
  mutate(materno = str_squish(materno) %>% str_trim() %>% replace_non_ascii(),
         paterno = str_squish(paterno) %>% str_trim() %>% replace_non_ascii(),
         nombres = str_squish(nombres) %>% str_trim() %>% replace_non_ascii(),
         across(where(is.character), str_delete_empty),
         across(where(is.character), ~ str_remove_all(.x, "[:digit:]")))

save.image("enrollment_2.RData")
rm(enrollment_2013, enrollment_2014, enrollment_2015)

# Load progress: 
load("enrollment_2.RData")

# Step 3: Write resulting table into database.

dbWriteTable(db_conn, name = "estudiantes", enrollment, append = TRUE)

###### THIRD FILTER
###### Identified: 3 (unique)
###### By: ID, Father's surname, mother's surname, names. 

## Step 1: 
# 1. Delete obs with reported birthday and missing ID from enrollment databases.
# 2. Retrieve cleaned tables without repeated values for all years.

# 2013
enrollment_2013 <- tbl(db_conn, "siagie2013_raw") %>%
  filter(is.na(fnac), !is.na(n_doc)) %>%
  select(n_doc, paterno, materno, nombres, fnac, mujer) %>% 
  group_by(paterno, materno, nombres, fnac, mujer) %>%
  distinct() %>%
  collect()

# 2014
enrollment_2014 <- tbl(db_conn, "siagie2014_raw") %>%
  filter(is.na(fnac), !is.na(n_doc)) %>%
  select(n_doc, paterno, materno, nombres, fnac, mujer) %>% 
  group_by(paterno, materno, nombres, fnac, mujer) %>%
  distinct() %>%
  collect()

# 2015
enrollment_2015 <- tbl(db_conn, "siagie2015_raw") %>%
  filter(is.na(fnac), !is.na(n_doc)) %>%
  select(n_doc, paterno, materno, nombres, fnac, mujer) %>% 
  group_by(paterno, materno, nombres, fnac, mujer) %>%
  distinct() %>%
  collect()

## Step 2: Bind rows of all enrollment tables and reduce by deleting repeated obs, then:
# 1. Replace non-ascii characters and clean all string variables (materno, paterno, nombres)
# 2. Count by groups (paterno, materno, nombres, fnac, mujer)

enrollment <- ls(pattern = "enrollment_") %>%
  map(~ rlang::parse_expr(.x) %>% eval) %>%
  reduce(bind_rows) %>%
  distinct() %>%
  arrange() %>%
  group_by(paterno, materno, nombres, mujer) %>%
  mutate(n_doc = ifelse(n_doc < 10000000, NA, n_doc),
         materno = str_squish(materno) %>% str_trim() %>% replace_non_ascii(),
         paterno = str_squish(paterno) %>% str_trim() %>% replace_non_ascii(),
         nombres = str_squish(nombres) %>% str_trim() %>% replace_non_ascii(),
         across(where(is.character), str_delete_empty),
         across(where(is.character), ~ str_remove_all(.x, "[:digit:]")))

rm(enrollment_2013, enrollment_2014, enrollment_2015)

# Step 3: Write resulting table into database.

dbWriteTable(db_conn, name = "estudiantes", enrollment, append = TRUE)

###### FOURTH FILTER
###### Identified: 15 (unique)
###### By: ID, Father's surname, mother's surname, names. 

## Step 1: 
# 1. Delete obs with reported birthday and missing ID from enrollment databases.
# 2. Retrieve cleaned tables without repeated values for all years.

# 2013
enrollment_2013 <- tbl(db_conn, "siagie2013_raw") %>%
  filter(is.na(fnac), is.na(n_doc)) %>%
  select(n_doc, paterno, materno, nombres, fnac, mujer) %>% 
  group_by(paterno, materno, nombres, fnac, mujer) %>%
  distinct() %>%
  collect()

# 2014
enrollment_2014 <- tbl(db_conn, "siagie2014_raw") %>%
  filter(is.na(fnac), is.na(n_doc)) %>%
  select(n_doc, paterno, materno, nombres, fnac, mujer) %>% 
  group_by(paterno, materno, nombres, fnac, mujer) %>%
  distinct() %>%
  collect()

# 2015
enrollment_2015 <- tbl(db_conn, "siagie2015_raw") %>%
  filter(is.na(fnac), is.na(n_doc)) %>%
  select(n_doc, paterno, materno, nombres, fnac, mujer) %>% 
  group_by(paterno, materno, nombres, fnac, mujer) %>%
  distinct() %>%
  collect()

## Step 2: Bind rows of all enrollment tables and reduce by deleting repeated obs, then:
# 1. Replace non-ascii characters and clean all string variables (materno, paterno, nombres)
# 2. Count by groups (paterno, materno, nombres, fnac, mujer)

enrollment <- ls(pattern = "enrollment_") %>%
  map(~ rlang::parse_expr(.x) %>% eval) %>%
  reduce(bind_rows) %>%
  distinct() %>%
  arrange() %>%
  group_by(paterno, materno, nombres, mujer) %>%
  mutate(materno = str_squish(materno) %>% str_trim() %>% replace_non_ascii(),
         paterno = str_squish(paterno) %>% str_trim() %>% replace_non_ascii(),
         nombres = str_squish(nombres) %>% str_trim() %>% replace_non_ascii(),
         across(where(is.character), str_delete_empty),
         across(where(is.character), ~ str_remove_all(.x, "[:digit:]")))

rm(enrollment_2013, enrollment_2014, enrollment_2015)

# Step 3: Write resulting table into database.

dbWriteTable(db_conn, name = "estudiantes", enrollment, append = TRUE)

###### FINAL PROCEDURE & CHECKS
###### Total Identified: 8954538

# Check 1: Grouping by names, birthday and sex and check number of duplicates
# Assumption: No more than one individual can be born the same day with the exact name, given a sex.

enrollment_nonunique <- tbl(db_conn, "estudiantes") %>% 
  group_by(paterno, materno, nombres, fnac, mujer) %>%
  mutate(n = n()) %>%
  filter(n > 1) %>%
  select(-n) %>%
  collect() %>%
  mutate(n_doc = handle_non_unique(n_doc)) %>%
  distinct()

enrollment <- tbl(db_conn, "estudiantes") %>% 
  group_by(paterno, materno, nombres, fnac, mujer) %>%
  mutate(n = n()) %>%
  filter(n == 1) %>%
  select(-n) %>%
  collect() %>%
  bind_rows(enrollment_nonunique)

# Check 2: Grouping by names, id and sex and check number of duplicates
# Assumption: ID number is correct for those that have the same names and sex, but different birthdays.

enrollment %<>% group_by(paterno, materno, nombres, n_doc, mujer) %>%
  mutate(n = n())

enrollment_nonunique <- enrollment %>%
  filter(n > 1, !is.na(n_doc)) %>%
  select(-n) %>%
  mutate(fnac = handle_nonunique(fnac)) %>%
  distinct()

enrollment %<>% filter(n == 1 | is.na(n_doc)) %>%
  select(-n) %>%
  bind_rows(enrollment_nonunique)

# Progress:
dbWriteTable(db_conn, name = "estudiantes", enrollment, overwrite = TRUE)

# Check 3: Delete exact document duplicates
# Assumption: No imputation can be made if two different people share the same ID.

enrollment_nonuniqueid <- enrollment %>%
  filter(!is.na(n_doc)) %>%
  group_by(n_doc) %>%
  mutate(n = n()) %>%
  filter(n > 1)

enrollment_nonuniqueid %>% summarise(handle_nonunique_id(across()))

enrollment %<>% filter(!is.na(n_doc))




mutate(fnac = as.Date(ifelse(fnac > date("2015-12-31") | fnac < date("1916-01-01"), NA, fnac), origin = "1970-01-01")) %>%
  ungroup() %>%
  distinct() %>%
  arrange(paterno, materno, nombres, fnac, n_doc)











## filter level 2 security classifications
table1 <- table %>% 
  mutate(security_level = case_when(
    valid_ndoc == 1 ~ security_level,
    valid_ndoc == 2 & between(valid_fnac, 2, 3) ~ security_level + 2L,
    valid_ndoc == 2 & between(valid_fnac, 4, 5) ~ security_level + 3L,
    valid_ndoc == 3 ~ security_level,
    valid_ndoc == 4 & between(valid_fnac, 1, 3) ~ security_level + 3L,
    valid_ndoc == 4 & between(valid_fnac, 4, 5) ~ security_level + 4L,
    valid_ndoc == 5 & valid_fnac == 1 ~ security_level + 4L,
    valid_ndoc == 5 & between(valid_fnac, 2, 3) ~ security_level + 5L,
    valid_ndoc == 5 & between(valid_fnac, 4, 5) ~ security_level + 6L,
    valid_ndoc == 6 ~ security_level + 2L)
  )
