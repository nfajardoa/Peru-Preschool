######## ENROLLMENT DATABASE FILTERING PROCEDURE

## Step 1: Depurate and save new enrollment databases by year
## Delete obs with missing ID and birthday from enrollment databases.
## Group by ndoc, paterno, materno, nombres, fnac, mujer
## translado_* is the number of times a group registry is in the data minus 1
## validado_reniec is 1 if id number does exist in the official registries (it is no necessarily a name id match)
## Keep only one individual observation (with the highest estado_mat) and valid id number

# 2013
tbl(db_conn, "siagie2013_raw") %>%
  filter(!is.na(fnac), !is.na(ndoc)) %>%
  group_by(ndoc, paterno, materno, nombres, fnac, mujer) %>%
  mutate(translado_2013 = n() - 1) %>%
  arrange(estado_mat, .by_group = TRUE) %>%
  head(8000000) %>%
  filter(row_number() == 1, validado_reniec == 1) %>%
  compute(
    name = "siagie2013",
    temporary = FALSE
  )

# 2014
tbl(db_conn, "siagie2014_raw") %>%
  filter(!is.na(fnac), !is.na(ndoc)) %>%
  group_by(ndoc, paterno, materno, nombres, fnac, mujer) %>%
  mutate(translado_2014 = n() - 1) %>%
  arrange(estado_mat, .by_group = TRUE) %>%
  head(8000000) %>%
  filter(row_number() == 1, validado_reniec == 1) %>%
  compute(
    name = "siagie2014",
    temporary = FALSE
  )

# 2015
tbl(db_conn, "siagie2015_raw") %>%
  filter(!is.na(fnac), !is.na(ndoc)) %>%
  group_by(ndoc, paterno, materno, nombres, fnac, mujer) %>%
  mutate(translado_2015 = n() - 1) %>%
  arrange(estado_mat, .by_group = TRUE) %>%
  head(8000000) %>%
  filter(row_number() == 1, validado_reniec == 1) %>%
  compute(
    name = "siagie2015",
    temporary = FALSE
  )

## Step 2: Obtain SQL queries
## Select columns paterno, materno, nombres, ndoc, fnac, mujer, cod_mod, anexo, grado, translado_{year}
## Rename columns: cod_mod_{year} = cod_mod, anexo_{year} = anexo, grado_{year} = grado

enrollment_2013 <- tbl(db_conn, "siagie2013") %>%
  dplyr::select(paterno, materno, nombres, ndoc, fnac, mujer, cod_mod, anexo, grado, estado_mat, translado_2013) %>%
  rename(cod_mod_2013 = cod_mod, anexo_2013 = anexo, grado_2013 = grado, estado_mat_2013 = estado_mat)

enrollment_2014 <- tbl(db_conn, "siagie2014") %>%
  dplyr::select(paterno, materno, nombres, ndoc, fnac, mujer, cod_mod, anexo, grado, estado_mat, translado_2014) %>%
  rename(cod_mod_2014 = cod_mod, anexo_2014 = anexo, grado_2014 = grado, estado_mat_2014 = estado_mat)

enrollment_2015 <- tbl(db_conn, "siagie2015") %>%
  dplyr::select(paterno, materno, nombres, ndoc, fnac, mujer, cod_mod, anexo, grado, estado_mat, translado_2015) %>%
  rename(cod_mod_2015 = cod_mod, anexo_2015 = anexo, grado_2015 = grado, estado_mat_2015 = estado_mat)

## Step 3: Create a new table by full-joining the step 2 queries
## Full join by paterno, materno, nombres, ndoc, fnac, mujer
## Number repeating values for paterno, materno, nombres (homonimos)
## Write new table in database: "enrollment"

ls(pattern = "enrollment_") %>%
  map(~ rlang::parse_expr(.x) %>% eval()) %>%
  reduce(full_join, by = c("paterno", "materno", "nombres", "ndoc", "fnac", "mujer")) %>%
  group_by(paterno, materno, nombres) %>%
  mutate(homonimos = n()-1) %>%
  ungroup() %>%
  mutate(
    check_1 = !is.na(cod_mod_2013) & !is.na(cod_mod_2014) & !is.na(cod_mod_2015),
    check_2 = !is.na(cod_mod_2013) & !is.na(cod_mod_2014),
    check_3 = !is.na(cod_mod_2014) & !is.na(cod_mod_2015),
    check_4 = !is.na(cod_mod_2013) & !is.na(cod_mod_2015),
    ) %>%
  arrange(desc(check_1), desc(check_2), desc(check_3), desc(check_4)) %>%
  head(8000000) %>%
  compute(
    name = "enrollment",
    temporary = FALSE
  )

## Step 5: Copy observations with no repeated id numbers to unique student/id table "students_siagie".
## variables in table paterno, materno, nombres, fnac, mujer, ndoc, homonyms
## SQL table details: "students_siagie"
### primary key (paterno, materno, nombres, mujer, fnac)
### ndoc unique constraint
### id serial

table <- tbl(db_conn, "enrollment") %>%
  dplyr::select(ndoc, paterno, materno, nombres, fnac, mujer) %>%
  filter(!is.na(paterno), !is.na(materno), !is.na(nombres), !is.na(mujer)) %>%
  group_by(ndoc) %>%
  filter(n() == 1) %>%
  group_by(paterno, materno, nombres) %>%
  mutate(homonyms = n()-1) %>%
  group_by(paterno, materno, nombres, mujer, fnac, ndoc) %>%
  filter(n() == 1) %>%
  collect()

dbWriteTable(db_conn, name = "students_siagie", table, temporary = FALSE, append = TRUE)

## RUN THIS SQL
## ALTER TABLE students_siagie 
## ADD COLUMN match_id SERIAL unique not null;
##############################################

# Commented out checks part ###################################################################
## Step 5: Copy "check_1" complying observations with no repeated id numbers to unique student/id table "students_siagie".
## variables in table paterno, materno, nombres, fnac, mujer, ndoc, valid_ndoc, security_level
## valid_ndoc == TRUE, since every number exists on the official registries, however no name/id match is guaranteed
## security_level == 1, highest possible certainty of identification of individual students
## SQL table details: "students_siagie"
### primary key (paterno, materno, nombres, mujer, fnac)
### ndoc unique constraint
### id serial
# 
# tbl(db_conn, "enrollment") %>%
#   filter(check_1 == TRUE) %>%
#   dplyr::select(ndoc, paterno, materno, nombres, fnac, mujer) %>%
#   group_by(ndoc) %>%
#   filter(n() == 1) %>%
#   group_by(paterno, materno, nombres) %>%
#   mutate(
#     homonyms = n()-1,
#     security_level = 1,
#     ) %>%
#   compute(
#     name = "students_siagie",
#     temporary = FALSE
#     )
# 
# ###### FIRST FILTER (most secure, security_level == 1)
# ###### Identified: 5828944
# ###### By: father's surname, mother's surname, names, sex, birthday, unique ID number.
# 
# ## Step 6: Generate "check_2" and "check_3" most consistent queries as in step 4.
# 
# check_2 <- tbl(db_conn, "enrollment") %>%
#   filter(check_2 == 1, is.na(check_1)) %>%
#   dplyr::select(-matches("(check)|(translado)")) %>%
#   group_by(ndoc) %>%
#   filter(n() == 1, grado_2014 == 16)
# 
# check_3 <- tbl(db_conn, "enrollment") %>%
#   filter(check_3 == 1, is.na(check_1)) %>%
#   dplyr::select(-matches("(check)|(translado)")) %>%
#   group_by(ndoc) %>%
#   filter(n() == 1, grado_2014 == 2)
# 
# ## Step 7: Get id numbers already existing in students_siagie table
# 
# ids <- tbl(db_conn, "students_siagie") %>%
#   dplyr::select(ndoc) %>%
#   collect()
# 
# ## Step 8: Bind rows from step 6 queries, and copy to "students_siagie" table
# ## variables in table paterno, materno, nombres, fnac, mujer, ndoc, valid_ndoc, security_level
# ## valid_ndoc == TRUE, since every number exists on the official registries, however no name/id match is guaranteed
# ## security_level == 1, highest possible certainty of identification of individual students
# ## SQL table details: "students_siagie"
# ### primary key (paterno, materno, nombres, mujer, fnac)
# ### ndoc unique constraint
# ### id serial
# 
# table <- union(check_2, check_3) %>%
#   dplyr::select(ndoc, paterno, materno, nombres, fnac, mujer) %>%
#   mutate(
#     valid_ndoc = TRUE,
#     security_level = 1
#   ) %>%
#   group_by(ndoc) %>%
#   filter(n() == 1) %>%
#   group_by(paterno, materno, nombres) %>%
#   mutate(homonyms = n()-1) %>%
#   collect() %>%
#   anti_join(ids)
# 
# ###### SECOND FILTER (most secure, security_level == 1)
# ###### Identified: 392454
# ###### By: father's surname, mother's surname, names, sex, birthday, unique ID number.
# 
# dbWriteTable(db_conn, name = "students_siagie", table, temporary = FALSE, append = TRUE)
# 
# ###### FIRST CHECK (security_level == 1)
# ###### Identified: 5508664 (first filter) + 329128 (second filter) = 5837792
# ###### By: father's surname, mother's surname, names, sex, birthday, unique ID number.
# ###### Details: No homonyms
# 
# tbl(db_conn, "students_siagie") %>%
#   filter(homonyms > 0)
# 
# # Clear variables
# rm(check_2, check_3, table, ids)
# 
# ## Step 9: Get remaining observations from step 6 (less consistent)
# 
# check_2 <- tbl(db_conn, "enrollment") %>%
#   filter(check_2 == 1, is.na(check_1)) %>%
#   dplyr::select(-matches("(check)|(translado)")) %>%
#   group_by(ndoc) %>%
#   filter(n() == 1, grado_2014 != 16)
# 
# check_3 <- tbl(db_conn, "enrollment") %>%
#   filter(check_3 == 1, is.na(check_1)) %>%
#   dplyr::select(-matches("(check)|(translado)")) %>%
#   group_by(ndoc) %>%
#   filter(n() == 1, grado_2014 != 2)
# 
# ## Step 10: Update id numbers already existing in students_siagie table
# 
# ids <- tbl(db_conn, "students_siagie") %>%
#   dplyr::select(ndoc) %>%
#   collect()
# 
# ## Step 11: Bind rows from step 9 queries
# 
# table <- union(check_2, check_3) %>%
#   dplyr::select(ndoc, paterno, materno, nombres, fnac, mujer) %>%
#   mutate(
#     valid_ndoc = TRUE,
#     security_level = 2
#   ) %>%
#   group_by(ndoc) %>%
#   filter(n() == 1) %>%
#   group_by(paterno, materno, nombres) %>%
#   mutate(homonyms = n()-1) %>%
#   collect() %>%
#   anti_join(ids)
# 
# ###### THIRD FILTER (security_level == 2)
# ###### Identified: 715557
# ###### By: father's surname, mother's surname, names, sex, birthday, unique ID number.
# 
# dbWriteTable(db_conn, name = "students_siagie", table, temporary = FALSE, append = TRUE)
# 
# ###### SECOND CHECK (security_level %in% c(1,2))
# ###### Identified: 5837792 (first check) + 715557 (third filter) = 6616349
# ###### By: father's surname, mother's surname, names, sex, birthday, unique ID number.
# ###### Details: No homonyms
# 
# tbl(db_conn, "students_siagie") %>%
#   filter(homonyms > 0)
# 
# # Clear variables
# rm(check_2, check_3, table, ids)
#
## Step 12: Prepare data for check join the student id table
## Join is made with paterno, materno, nombres, mujer

ece <- tbl(db_conn, "ece") %>%
  group_by(tipo_eva, periodo, cod_mod, anexo, seccion, paterno, materno, nombres, mujer, ndoc) %>%
  mutate(homonyms_test.1 = n() - 1) %>%
  group_by(tipo_eva, periodo, cod_mod, anexo, paterno, materno, nombres, mujer, ndoc) %>%
  mutate(homonyms_test.2 = n() - 1) %>%
  group_by(tipo_eva, periodo, paterno, materno, nombres, mujer, ndoc) %>%
  mutate(homonyms_test.3 = n() - 1) %>%
  group_by(periodo, paterno, materno, nombres, mujer, ndoc) %>%
  mutate(homonyms_test.4 = n() - 1) %>%
  ungroup() %>%
  left_join(tbl(db_conn, 'school_cluster')) %>%
  rename(ndoc_ece = ndoc, cluster_ece = cluster) %>%
  select(id_ece, tipo_eva, periodo, paterno, materno, nombres, mujer, matches('homonyms_test'), ndoc_ece, cluster_ece) %>%
  compute(name = 'ece_filtered', temporary = TRUE)

sisfoh <- tbl(db_conn, "sisfoh") %>%
  dplyr::select(id_sisfoh, ubigeo, ubigeo_ccpp, cen_pob, paterno, materno, nombres, mujer, fnac, ndoc, numero) %>%
  rename(idccpp = ubigeo_ccpp, nomcp = cen_pob, ndoc_sisfoh = ndoc, fnac_sisfoh = fnac) %>% 
  left_join(tbl(db_conn, 'conversion_ccpp') %>% select(idccpp, cluster), by = 'idccpp') %>%
  left_join(tbl(db_conn, 'conversion_ccpp') %>% select(ubigeo, nomcp, cluster), by = c('ubigeo', 'nomcp')) %>%
  mutate(cluster_sisfoh = coalesce(cluster.x, cluster.y)) %>%
  select(-matches('\\.')) %>%
  compute(name = 'sisfoh_filtered', temporary = TRUE)

## Step 13: Join queries
join_ece <- tbl(db_conn, "students_siagie") %>%
  inner_join(tbl(db_conn, "ece_filtered")) %>%
  distinct() %>%
  group_by(paterno, materno, nombres, mujer) %>%
  mutate(homonyms_ece = n() - 1) %>%
  ungroup() %>%
  dplyr::select(match_id, id_ece, tipo_eva, periodo, matches('homonyms_test'), ndoc_ece, cluster_ece, homonyms_ece)

join_sisfoh <- tbl(db_conn, "students_siagie") %>%
  inner_join(tbl(db_conn, "sisfoh_filtered")) %>%
  distinct() %>%
  group_by(paterno, materno, nombres, mujer) %>%
  mutate(homonyms_sisfoh = n() - 1) %>%
  ungroup() %>%
  dplyr::select(match_id, id_sisfoh, fnac_sisfoh, ndoc_sisfoh, cluster_sisfoh, homonyms_sisfoh, numero)

## Step 14: Bind rows from step 13 queries and check document

join <- inner_join(join_ece, join_sisfoh) %>%
  distinct() %>%
  left_join(tbl(db_conn, "students_siagie")) %>% 
  distinct() %>%
  collect()

table <- join %>%
  mutate(
    table_row = row_number(),
    across(matches('homonyms'), as.integer),
    diff_fnac = (fnac %--% fnac_sisfoh) %>% as.period(),
    diff_fnac = case_when(
      diff_fnac == period(seconds = 0) ~ 1L, #Identical
      diff_fnac == period(year = 1) ~ 2L, #sisfoh date is a possible typo (upwards)
      diff_fnac == period(month = 1) ~ 2L,
      diff_fnac == period(day = 1) ~ 2L,
      diff_fnac == period(year = -1) ~ -2L, #sisfoh date is a possible typo (downwards)
      diff_fnac == period(month = -1) ~ -2L,
      diff_fnac == period(day = -1) ~ -2L,
      TRUE ~ 3L # Very different
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
    ),
    valid_cluster = cluster_ece == cluster_sisfoh,
    security_level = case_when(
      # Secure match between all 3 tables
      homonyms_ece == 0 & homonyms_sisfoh == 0 & homonyms == 0 ~ 1L, #Single full match (just one test)
      valid_ndoc == 1 ~ 2L, #FULL MATCH, ids and names in the three databases are equal (can have more than one test)
      valid_ndoc == 2 & homonyms_ece == 0 ~ 3L, #Lost id in ece but unique match. Sisfoh matches by id.
      is.na(ndoc_ece) & homonyms_test.4 == 0 & homonyms_sisfoh == 0 & homonyms == 0 ~ 4L, #Unique match with multiple tests without id in ece
      dplyr::between(valid_ndoc, 3, 4) & valid_fnac == 1 ~ 5L, #Matching id in ece and exact birthdate in sisfoh (even when id in sisfoh is wrong)
      valid_ndoc == 10 & valid_fnac == 1 & homonyms_ece == 0 ~ 6L,
      # Secure match between enrollment and sisfoh, but with some uncertainty in tests.
      # (valid_ndoc == 2 | valid_ndoc == 5) ~ 7L, #Matches have to be checked.
      # valid_ndoc == 10 & dplyr::between(valid_fnac, 2, 3) & homonyms_ece == 0 ~ 8L, #Matches have to be checked.
      # Secure match between enrollment and tests, but with some uncertainty in sisfoh.
      # dplyr::between(valid_ndoc, 3, 4) & dplyr::between(valid_fnac, 2, 3) ~ 9L, #Matches have to be checked.
      TRUE ~ NA_integer_
      )
    )

# # Individual checks
# # Check of (valid_ndoc == 2 | valid_ndoc == 5) ~ 7L,
# table.7 <- table %>%
#   filter(security_level == 7L) %>%
#   group_by(nombres, paterno, materno, mujer) %>%
#   filter(n() == 1 | valid_cluster == TRUE, valid_fnac == min(valid_fnac)) %>%
#   group_by(id_ece, ndoc, nombres, paterno, materno, mujer) %>%
#   mutate(matches = n()) %>%
#   group_by(ndoc, nombres, paterno, materno, mujer) %>%
#   filter(valid_fnac == min(valid_fnac)) %>%
#   mutate(repetitions = n(), sisfoh_group = cur_group_id()) %>%
#   filter(matches <= repetitions)
# 
# # valid_ndoc == 10 & dplyr::between(valid_fnac, 2, 3) & homonyms_ece == 0 ~ 8L
# table.8 <- table %>% 
#   filter(security_level == 8L) %>% 
#   group_by(nombres, paterno, materno, mujer) %>%
#   mutate(sisfoh_group = cur_group_id())
# 
# # Check of dplyr::between(valid_ndoc, 3, 4) & dplyr::between(valid_fnac, 2, 3) ~ 9L
# table.9 <- table %>% 
#   filter(security_level == 9L) %>% 
#   group_by(ndoc_sisfoh, nombres, paterno, materno, mujer) %>% 
#   mutate(matches = n()) %>%
#   group_by(id_ece, nombres, paterno, materno, mujer) %>% 
#   mutate(repetitions = n(), sisfoh_group = if_else(matches < repetitions, cur_group_id(), NA_integer_))
#
## Remaining observations
# Sure match in tests and no variation in sisfoh
# If homonyms_sisfoh are == 0 | matches == 1 then sure match.
# Cluster TRUE. 
# table_remaining.1 <- table %>%
#   anti_join(table %>% filter(!is.na(security_level)), by = c('ndoc_sisfoh', 'nombres', 'paterno', 'materno'))
#   
#   
#   filter(is.na(security_level), dplyr::between(valid_ndoc, 3, 4)) %>%
#   group_by(paterno, materno, nombres, mujer) %>%
#   mutate(matches = n())
# 
#   filter(n() == 1) %>%
#   mutate(security_level = 10L) %>%
#   select(ndoc, nombres, paterno, materno, fnac, mujer, match_id, id_ece, id_sisfoh, security_level, table_row)
# 
# Sure match in sisfoh and no variation in tests
# table_remaining.2 <- table %>%
#   filter(is.na(security_level)) %>%
#   anti_join(table_remaining.1, by = 'table_row')
# 
#   filter(homonyms_sisfoh == 0, homonyms == 0) %>%
#   group_by(paterno, materno, nombres, mujer) %>%
#   filter(n() == 1) %>%
#   mutate(security_level = 11L) %>%
#   select(ndoc, nombres, paterno, materno, fnac, mujer, match_id, id_ece, id_sisfoh, security_level, table_row)
# 
# Not sure in anything, no left variation.
# table_remaining.3 <- table %>%
#   anti_join(table.7, by = 'table_row') %>%
#   anti_join(table.8, by = 'table_row') %>%
#   anti_join(table.9, by = 'table_row') %>%
#   anti_join(table_remaining.1, by = 'table_row') %>%
#   anti_join(table_remaining.2, by = 'table_row') %>%
#   filter(is.na(security_level)) %>%
#   group_by(paterno, materno, nombres, mujer) %>%
#   filter(n() == 1) %>%
#   mutate(security_level = 12L) %>%
#   select(ndoc, nombres, paterno, materno, fnac, mujer, match_id, id_ece, id_sisfoh, security_level, table_row)
# 
# Consolidate data
table_end <- table %>%
  filter(!security_level %in% 7:9 & !is.na(security_level)) %>%
  #select(ndoc, nombres, paterno, materno, fnac, mujer, match_id, id_ece, id_sisfoh, security_level, table_row) %>%
  #bind_rows(table.7) %>%
  #bind_rows(table.8) %>%
  #bind_rows(table.9) %>%
  #bind_rows(table_remaining.1) %>%
  #bind_rows(table_remaining.2) %>%
  select(ndoc, nombres, paterno, materno, fnac, mujer, match_id, id_ece, id_sisfoh, security_level) %>%
  group_by(paterno, materno, nombres) %>%
  mutate(homonyms = n()-1L) %>%
  ungroup() %>%
  mutate(
    filter_stage = 1L,
    pyear_check = if_else(month(fnac) <= 3 & day(fnac) <= 31, TRUE, FALSE),
    pyear = if_else(year_check == TRUE, year(fnac) + 5L, year(fnac) + 6L),
    across(where(is.numeric), as.integer)
  )

###### FIRST JOIN FILTER 
###### Identified: 3859141 (inner join)
###### By: father's surname, mother's surname, names, sex
###### birthday, unique ID number.
###### Matching observations in sisfoh and ece
###### ECE by: paterno, materno, nombres, mujer, ndoc
###### sisfoh by: paterno, materno, nombres, mujer, fnac, ndoc

dbWriteTable(db_conn, name = "student_id", table_end, temporary = FALSE, append = TRUE)

# Clear variables
rm(ece, sisfoh, join_ece, join_sisfoh, join)

################################################# SECOND ITERATIONS

ece <- tbl(db_conn, "ece") %>%
  group_by(tipo_eva, periodo, cod_mod, anexo, seccion, paterno, materno, nombres, mujer, ndoc) %>%
  mutate(homonyms_test = n() - 1) %>%
  ungroup() %>%
  left_join(tbl(db_conn, 'school_cluster')) %>%
  rename(ndoc_ece = ndoc, cluster_ece = cluster) %>%
  select(tipo_eva, periodo, paterno, materno, nombres, mujer, homonyms_test, ndoc_ece, cluster_ece)

sisfoh <- tbl(db_conn, "sisfoh") %>%
  dplyr::select(ubigeo, ubigeo_ccpp, cen_pob, paterno, materno, nombres, mujer, fnac, ndoc) %>%
  rename(idccpp = ubigeo_ccpp, nomcp = cen_pob, ndoc_sisfoh = ndoc, fnac_sisfoh = fnac) %>% 
  left_join(tbl(db_conn, 'conversion_ccpp') %>% select(idccpp, cluster), by = 'idccpp') %>%
  left_join(tbl(db_conn, 'conversion_ccpp') %>% select(ubigeo, nomcp, cluster), by = c('ubigeo', 'nomcp')) %>%
  mutate(cluster_sisfoh = coalesce(cluster.x, cluster.y)) %>%
  select(-matches('\\.'))

## Step 13: Join queries
join_ece <- tbl(db_conn, "students_siagie") %>%
  inner_join(ece) %>%
  distinct() %>%
  group_by(paterno, materno, nombres, mujer) %>%
  mutate(homonyms_ece = n() - 1) %>%
  ungroup() %>%
  dplyr::select(match_id, tipo_eva, periodo, homonyms_test, ndoc_ece, cluster_ece, homonyms_ece)

join_sisfoh <- tbl(db_conn, "students_siagie") %>%
  inner_join(sisfoh) %>%
  distinct() %>%
  group_by(paterno, materno, nombres, mujer) %>%
  mutate(homonyms_sisfoh = n() - 1) %>%
  ungroup() %>%
  dplyr::select(match_id, fnac_sisfoh, ndoc_sisfoh, cluster_sisfoh, homonyms_sisfoh)

## Step 14: Bind rows from step 13 queries and check document

join <- inner_join(join_ece, join_sisfoh) %>%
  distinct() %>%
  left_join(tbl(db_conn, "students_siagie")) %>% 
  distinct() %>%
  collect()

table <- join %>%
  mutate(
    across(matches('homonyms'), as.integer),
    diff_fnac = (fnac %--% fnac_sisfoh) %>% as.period(),
    diff_fnac = case_when(
      diff_fnac == period(seconds = 0) ~ 1L, #Identical
      diff_fnac == period(year = 1) ~ 2L, #sisfoh date is a possible typo (upwards)
      diff_fnac == period(month = 1) ~ 2L,
      diff_fnac == period(day = 1) ~ 2L,
      diff_fnac == period(year = -1) ~ -2L, #sisfoh date is a possible typo (downwards)
      diff_fnac == period(month = -1) ~ -2L,
      diff_fnac == period(day = -1) ~ -2L,
      TRUE ~ 3L # Very different
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
    ),
    valid_cluster = cluster_ece == cluster_sisfoh,
    security_level = case_when(
      valid_ndoc == 1 ~ 1L, #FULL MATCH, ids and names in the three databases are equal.
      
      
      
      
      
      
    )
  )


%>% 
  group_by(ndoc) %>%
  arrange(valid_ndoc, valid_fnac, .by_group = TRUE) %>%
  filter(row_number()==1) %>%
  filter(valid_ndoc <= 3) %>%
  dplyr::select(-matches("(ndoc_)|(fnac_)|(diff)")) %>%
  group_by(paterno, materno, nombres) %>%
  mutate(homonyms = n()-1L) %>%
  ungroup() %>%
  mutate(
    filter_stage = 1L,
    year_check = if_else(month(fnac) <= 3 & day(fnac) <= 31, TRUE, FALSE),
    year = if_else(year_check == TRUE, year(fnac) + 5L, year(fnac) + 6L),
    across(where(is.numeric), as.integer)
  )

###### FIRST JOIN FILTER 
###### Identified: 5384200 (full join)
###### By: father's surname, mother's surname, names, sex
###### birthday, unique ID number.
###### Matching observations in sisfoh and ece
###### ECE by: paterno, materno, nombres, mujer, ndoc
###### sisfoh by: paterno, materno, nombres, mujer, fnac, ndoc

dbWriteTable(db_conn, name = "student_id", table, temporary = FALSE, append = TRUE)

# Clear variables
rm(ece, sisfoh, join_ece, join_sisfoh, join, table)

#################################################### NDOC JOIN

## Step 15: Prepare data for check join the student id table
## Join is made with ndoc

ece <- tbl(db_conn, "ece") %>%
  dplyr::select(paterno, materno, nombres, mujer, ndoc)

sisfoh <- tbl(db_conn, "sisfoh") %>%
  dplyr::select(paterno, materno, nombres, mujer, fnac, ndoc) %>%
  rename(
    fnac_sisfoh = fnac
  )

## Collect ids from past "student_id" table

ids <- tbl(db_conn, "student_id") %>%
  dplyr::select(ndoc) 

## Step 16: Join queries
join_ece <- tbl(db_conn, "students_siagie") %>%
  anti_join(ids) %>%
  inner_join(ece, by = c("ndoc", "mujer")) %>%
  mutate(
    paterno = coalesce(paterno.x, paterno.y),
    materno = coalesce(materno.x, materno.y),
    nombres = coalesce(nombres.x, nombres.y),
    ) %>%
  dplyr::select(paterno, materno, nombres, mujer, ndoc, fnac) %>%
  rename(ndoc_ece = ndoc)

join_sisfoh <- tbl(db_conn, "students_siagie") %>%
  anti_join(ids) %>%
  inner_join(sisfoh, by = c("ndoc", "mujer")) %>%
  mutate(
    paterno = coalesce(paterno.x, paterno.y),
    materno = coalesce(materno.x, materno.y),
    nombres = coalesce(nombres.x, nombres.y),
  ) %>%
  dplyr::select(paterno, materno, nombres, mujer, ndoc, fnac, fnac_sisfoh) %>%
  rename(ndoc_sisfoh = ndoc)

## Step 17: Bind rows from step 15 queries and check

join <- inner_join(join_ece, join_sisfoh) %>% distinct() %>% collect()

table <- join %>%
  mutate(
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
      ndoc_sisfoh == ndoc_ece ~ 1L,
      is.na(ndoc_ece) ~ 2L,
      is.na(ndoc_sisfoh) ~ 3L,
      ndoc_sisfoh != ndoc_ece ~ 9L
    ),
    ndoc = case_when(
      valid_ndoc == 1 ~ ndoc_ece,
      valid_ndoc == 2 ~ ndoc_sisfoh,
      valid_ndoc == 3 ~ ndoc_ece,
      valid_ndoc == 9 & valid_fnac <= 3 ~ ndoc_sisfoh,
    )
  ) %>% 
  dplyr::select(paterno, materno, nombres, mujer, ndoc, fnac, valid_ndoc, valid_fnac) %>%
  group_by(ndoc) %>%
  arrange(valid_ndoc, valid_fnac, .by_group = TRUE) %>%
  filter(row_number()==1) %>%
  filter(valid_ndoc == 1L | (valid_ndoc == 9 & valid_fnac <= 3)) %>%
  group_by(paterno, materno, nombres) %>%
  mutate(homonyms = n()-1L) %>%
  ungroup() %>%
  mutate(
    filter_stage = 2L,
    year_check = if_else(month(fnac) <= 3 & day(fnac) <= 31, TRUE, FALSE),
    year = if_else(year_check == TRUE, year(fnac) + 5L, year(fnac) + 6L),
    across(where(is.numeric), as.integer)
  )

###### SECOND JOIN FILTER
###### Identified: 16229 (full join)
###### By: ndoc, mujer
###### father's surname, mother's surname, names, sex (from siagie)
###### birthdate only if consistent
###### Matching observations in sisfoh and ece
###### ECE by: ndoc, mujer
###### sisfoh by: ndoc, mujer

dbWriteTable(db_conn, name = "student_id", table, temporary = FALSE, append = TRUE)

###### Summary: 

# Clear variables
rm(ece, sisfoh, join_ece, join_sisfoh, join, table, ids)

####################################################### RUN IN SQL ####################################################### 
# ALTER TABLE student_id 
# ADD COLUMN student_id SERIAL unique not null;
##########################################################################################################################

# COMMENTED THE SCHOOL FILTER ################################################################
# ## Step 15: Update id numbers already existing in student_id table
# 
# ids <- tbl(db_conn, "student_id") %>%
#   dplyr::select(ndoc) %>%
#   collect()
# 
# ## Step 16: Remove already identified individuals from students_siagie
# 
# table <- tbl(db_conn, "students_siagie") %>%
#   collect() %>%
#   anti_join(ids)
# 
# dbWriteTable(db_conn, name = "students_siagie", table, temporary = FALSE, overwrite = TRUE)
# 
# # Clear variables
# rm(ids, table)
# 
# ## Step 17: Select individuals that haven't changed school from 2013 to 2015
# 
# filter_school <- tbl(db_conn, "enrollment") %>%
#   filter(cod_mod_2013 == cod_mod_2014, cod_mod_2014 == cod_mod_2015) %>%
#   dplyr::select(paterno, materno, nombres, ndoc, fnac, mujer, cod_mod_2013) %>%
#   rename(cod_mod = cod_mod_2013)
# 
# ## Step 18: Join tables "students_siagie" and "filter_school"
# 
# join_baseline <- inner_join(tbl(db_conn, "students_siagie"), filter_school) %>%  
#   compute(
#     name = "students_siagie_codmod18",
#     temporary = TRUE
#   )
# 
# ## Step 19: Prepare data for inner-joining with the "join_baseline" table
# 
# ece <- tbl(db_conn, "ece") %>%
#   dplyr::select(paterno, materno, nombres, mujer, ndoc, cod_mod) %>%
#   rename(ndoc_ece = ndoc)
# 
# sisfoh <- tbl(db_conn, "sisfoh") %>%
#   dplyr::select(paterno, materno, nombres, mujer, fnac, ndoc) %>%
#   rename(
#     ndoc_sisfoh = ndoc,
#     fnac_sisfoh = fnac
#   )
# 
# ## Step 20: Join queries
# join_ece <- join_baseline %>%
#   inner_join(ece)
# 
# join_sisfoh <- join_baseline %>%
#   inner_join(sisfoh)
# 
# ## Step 21: Bind rows from step 13 queries and check document
# 
# join <- inner_join(join_ece, join_sisfoh) %>%
#   collect()
# 
# table <- join %>%
#   mutate(
#     diff_fnac = (fnac %--% fnac_sisfoh) %>% as.period(),
#     diff_fnac = case_when(
#       diff_fnac == period(seconds = 0) ~ 1, #Identical
#       diff_fnac == period(year = 1) ~ 2, #sisfoh date is a possible typo (upwards)
#       diff_fnac == period(month = 1) ~ 2,
#       diff_fnac == period(day = 1) ~ 2,
#       diff_fnac == period(year = -1) ~ -2, #sisfoh date is a possible typo (downwards)
#       diff_fnac == period(month = -1) ~ -2,
#       diff_fnac == period(day = -1) ~ -2,
#       TRUE ~ 3 # Very different
#     ),
#     valid_fnac = case_when(
#       fnac == fnac_sisfoh ~ 1L,
#       fnac != fnac_sisfoh & diff_fnac == 2 ~ 2L,
#       fnac != fnac_sisfoh & diff_fnac == -2 ~ 3L,
#       fnac != fnac_sisfoh ~ 4L,
#       is.na(fnac_sisfoh) ~ 5L,
#     ),
#     valid_ndoc = case_when(
#       ndoc == ndoc_sisfoh & ndoc == ndoc_ece ~ 1L,
#       ndoc == ndoc_sisfoh & is.na(ndoc_ece) ~ 2L,
#       is.na(ndoc_sisfoh) & ndoc == ndoc_ece ~ 3L,
#       ndoc != ndoc_sisfoh & ndoc == ndoc_ece ~ 4L,
#       ndoc == ndoc_sisfoh & ndoc != ndoc_ece ~ 5L,
#       ndoc_sisfoh == ndoc_ece & ndoc != ndoc_ece ~ 6L,
#       is.na(ndoc_sisfoh) & ndoc != ndoc_ece ~ 7L,
#       ndoc != ndoc_sisfoh & is.na(ndoc_ece) ~ 8L,
#       ndoc != ndoc_sisfoh & ndoc != ndoc_ece ~ 9L,
#       is.na(ndoc_sisfoh) & is.na(ndoc_ece) ~ 10L
#     )
#   ) %>% 
#   group_by(ndoc) %>%
#   arrange(valid_ndoc, valid_fnac, .by_group = TRUE) %>%
#   filter(row_number()==1) %>%
#   filter(valid_ndoc <= 6) %>%
#   mutate(
#     ndoc = if_else(valid_ndoc == 6, ndoc_sisfoh, ndoc),
#     fnac = if_else(valid_ndoc == 6, fnac_sisfoh, fnac),
#   ) %>%
#   dplyr::select(-matches("(ndoc_)|(fnac_)|(diff_)|(cod_mod)")) %>%
#   group_by(paterno, materno, nombres) %>%
#   mutate(
#     homonyms = n()-1,
#     filter_stage = 2L
#   )
# 
# ###### SECOND JOIN FILTER (security_level == 1)
# ###### Identified: 27088
# ###### By: father's surname, mother's surname, names, sex, birthday, unique ID number, cod_mod.
# ###### Matching observations in sisfoh and ece
# ###### ECE by: paterno, materno, nombres, mujer, ndoc, cod_mod
# ###### sisfoh by: paterno, materno, nombres, mujer, fnac, ndoc
# 
# dbWriteTable(db_conn, name = "student_id", table, temporary = FALSE, append = TRUE)
# 
# # Clear variables
# rm(ece, sisfoh, join_ece, join_sisfoh, join, join_baseline, table, filter_school)
# 
# ## Step 22: Update id numbers already existing in student_id table
# 
# ids <- tbl(db_conn, "student_id") %>%
#   dplyr::select(ndoc) %>%
#   collect()
# 
# ## Step 23: Remove already identified individuals from students_siagie
# ## valid_ndoc == 6 individuals are still in the table, since ndocs were changed
# 
# table <- tbl(db_conn, "students_siagie") %>%
#   collect() %>%
#   anti_join(ids)
# 
# dbWriteTable(db_conn, name = "students_siagie", table, temporary = FALSE, overwrite = TRUE)
# 
# # Clear variables
# rm(ids, table)
# 
# ## Step 24: Select individuals that haven't changed school from 2013 to 2014 or 2014 to 2015
# 
# filter_school <- tbl(db_conn, "enrollment") %>%
#   filter((cod_mod_2013 == cod_mod_2014 & is.na(cod_mod_2015)) | (cod_mod_2014 == cod_mod_2015 & is.na(cod_mod_2013))) %>%
#   dplyr::select(paterno, materno, nombres, ndoc, fnac, mujer, cod_mod_2014) %>%
#   rename(cod_mod = cod_mod_2014)
# 
# ## Step 25: Join tables "students_siagie" and "filter_school"
# 
# join_baseline <- inner_join(tbl(db_conn, "students_siagie"), filter_school) %>%  
#   compute(
#     name = "students_siagie_codmod25",
#     temporary = TRUE
#   )
# 
# ## Step 26: Prepare data for inner-joining with the "join_baseline" table
# 
# ece <- tbl(db_conn, "ece") %>%
#   dplyr::select(paterno, materno, nombres, mujer, ndoc, cod_mod) %>%
#   rename(ndoc_ece = ndoc)
# 
# sisfoh <- tbl(db_conn, "sisfoh") %>%
#   dplyr::select(paterno, materno, nombres, mujer, fnac, ndoc) %>%
#   rename(
#     ndoc_sisfoh = ndoc,
#     fnac_sisfoh = fnac
#   )
# 
# ## Step 27: Join queries
# join_ece <- join_baseline %>%
#   inner_join(ece)
# 
# join_sisfoh <- join_baseline %>%
#   inner_join(sisfoh)
# 
# ## Step 28: Bind rows from step 13 queries and check document
# 
# join <- inner_join(join_ece, join_sisfoh) %>%
#   collect()
# 
# table <- join %>%
#   mutate(
#     diff_fnac = (fnac %--% fnac_sisfoh) %>% as.period(),
#     diff_fnac = case_when(
#       diff_fnac == period(seconds = 0) ~ 1, #Identical
#       diff_fnac == period(year = 1) ~ 2, #sisfoh date is a possible typo (upwards)
#       diff_fnac == period(month = 1) ~ 2,
#       diff_fnac == period(day = 1) ~ 2,
#       diff_fnac == period(year = -1) ~ -2, #sisfoh date is a possible typo (downwards)
#       diff_fnac == period(month = -1) ~ -2,
#       diff_fnac == period(day = -1) ~ -2,
#       TRUE ~ 3 # Very different
#     ),
#     valid_fnac = case_when(
#       fnac == fnac_sisfoh ~ 1L,
#       fnac != fnac_sisfoh & diff_fnac == 2 ~ 2L,
#       fnac != fnac_sisfoh & diff_fnac == -2 ~ 3L,
#       fnac != fnac_sisfoh ~ 4L,
#       is.na(fnac_sisfoh) ~ 5L,
#     ),
#     valid_ndoc = case_when(
#       ndoc == ndoc_sisfoh & ndoc == ndoc_ece ~ 1L,
#       ndoc == ndoc_sisfoh & is.na(ndoc_ece) ~ 2L,
#       is.na(ndoc_sisfoh) & ndoc == ndoc_ece ~ 3L,
#       ndoc != ndoc_sisfoh & ndoc == ndoc_ece ~ 4L,
#       ndoc == ndoc_sisfoh & ndoc != ndoc_ece ~ 5L,
#       ndoc_sisfoh == ndoc_ece & ndoc != ndoc_ece ~ 6L,
#       is.na(ndoc_sisfoh) & ndoc != ndoc_ece ~ 7L,
#       ndoc != ndoc_sisfoh & is.na(ndoc_ece) ~ 8L,
#       ndoc != ndoc_sisfoh & ndoc != ndoc_ece ~ 9L,
#       is.na(ndoc_sisfoh) & is.na(ndoc_ece) ~ 10L
#     )
#   ) %>% 
#   group_by(ndoc) %>%
#   arrange(valid_ndoc, valid_fnac, .by_group = TRUE) %>%
#   filter(row_number()==1) %>%
#   filter(valid_ndoc <= 6) %>%
#   mutate(
#     ndoc = if_else(valid_ndoc == 6, ndoc_sisfoh, ndoc),
#     fnac = if_else(valid_ndoc == 6, fnac_sisfoh, fnac),
#   ) %>%
#   dplyr::select(-matches("(ndoc_)|(fnac_)|(diff_)|(cod_mod)")) %>%
#   group_by(paterno, materno, nombres) %>%
#   mutate(
#     homonyms = n()-1,
#     filter_stage = 3L
#   )
# 
# ###### THIRD JOIN FILTER (security_level == 1)
# ###### Identified: 350
# ###### By: father's surname, mother's surname, names, sex, birthday, unique ID number, cod_mod.
# ###### Matching observations in sisfoh and ece
# ###### ECE by: paterno, materno, nombres, mujer, ndoc, cod_mod
# ###### sisfoh by: paterno, materno, nombres, mujer, fnac, ndoc
# 
# dbWriteTable(db_conn, name = "student_id", table, temporary = FALSE, append = TRUE)
# 
# # Clear variables
# rm(ece, sisfoh, join_ece, join_sisfoh, join, join_baseline, table, filter_school)
# 
# ## Step 29: Update id numbers already existing in student_id table
# 
# ids <- tbl(db_conn, "student_id") %>%
#   dplyr::select(ndoc) %>%
#   collect()
# 
# ## Step 30: Remove already identified individuals from students_siagie
# ## valid_ndoc == 6 individuals are still in the table, since ndocs were changed
# 
# table <- tbl(db_conn, "students_siagie") %>%
#   collect() %>%
#   anti_join(ids)
# 
# dbWriteTable(db_conn, name = "students_siagie", table, temporary = FALSE, overwrite = TRUE)
# 
# # Clear variables
# rm(ids, table)
# 
# ### CHANGE OF STRATEGY ######################################################################
# ### SELECT INDIVIDUALS BY YEAR ##############################################################
# 
# ## Step 31: Create sisfoh raw query
# 
# sisfoh <- tbl(db_conn, "sisfoh") %>%
#   dplyr::select(paterno, materno, nombres, mujer, fnac, ndoc) %>%
#   rename(
#     ndoc_sisfoh = ndoc,
#     fnac_sisfoh = fnac
#   )
# 
# ## Step 32: Select individuals in 2013
# 
# filter_school_2013 <- tbl(db_conn, "enrollment") %>%
#   filter(!is.na(cod_mod_2013)) %>%
#   dplyr::select(paterno, materno, nombres, ndoc, fnac, mujer, cod_mod_2013) %>%
#   rename(cod_mod = cod_mod_2013)
# 
# join_baseline_2013 <- inner_join(tbl(db_conn, "students_siagie"), filter_school_2013) %>%  
#   compute(
#     name = "students_siagie_codmod_201332",
#     temporary = TRUE
#   )
# 
# ece_2013 <- tbl(db_conn, "ece") %>%
#   filter(periodo == 2013) %>%
#   dplyr::select(paterno, materno, nombres, mujer, ndoc, cod_mod) %>%
#   rename(ndoc_ece = ndoc)
# 
# join_ece_2013 <- join_baseline_2013 %>%
#   inner_join(ece_2013) %>%
#   compute(
#     name = "join_ece_201332",
#     temporary = TRUE
#   )
# 
# join_sisfoh_2013 <- join_baseline_2013 %>%
#   inner_join(sisfoh)
# 
# ## Step 33: Select individuals in 2014
# 
# filter_school_2014 <- tbl(db_conn, "enrollment") %>%
#   filter(!is.na(cod_mod_2014)) %>%
#   dplyr::select(paterno, materno, nombres, ndoc, fnac, mujer, cod_mod_2014) %>%
#   rename(cod_mod = cod_mod_2014)
# 
# join_baseline_2014 <- inner_join(tbl(db_conn, "students_siagie"), filter_school_2014) %>%  
#   compute(
#     name = "students_siagie_codmod_201433",
#     temporary = TRUE
#   )
# 
# ece_2014 <- tbl(db_conn, "ece") %>%
#   filter(periodo == 2014) %>%
#   dplyr::select(paterno, materno, nombres, mujer, ndoc, cod_mod) %>%
#   rename(ndoc_ece = ndoc)
# 
# join_ece_2014 <- join_baseline_2014 %>%
#   inner_join(ece_2014) %>%
#   compute(
#     name = "join_ece_201433",
#     temporary = TRUE
#   )
# 
# join_sisfoh_2014 <- join_baseline_2014 %>%
#   inner_join(sisfoh)
# 
# ## Step 34: Select individuals in 2015
# 
# filter_school_2015 <- tbl(db_conn, "enrollment") %>%
#   filter(!is.na(cod_mod_2015)) %>%
#   dplyr::select(paterno, materno, nombres, ndoc, fnac, mujer, cod_mod_2015) %>%
#   rename(cod_mod = cod_mod_2015)
# 
# join_baseline_2015 <- inner_join(tbl(db_conn, "students_siagie"), filter_school_2015) %>%  
#   compute(
#     name = "students_siagie_codmod_201534",
#     temporary = TRUE
#   )
# 
# ece_2015 <- tbl(db_conn, "ece") %>%
#   filter(periodo == 2015) %>%
#   dplyr::select(paterno, materno, nombres, mujer, ndoc, cod_mod) %>%
#   rename(ndoc_ece = ndoc)
# 
# join_ece_2015 <- join_baseline_2015 %>%
#   inner_join(ece_2015) %>%
#   compute(
#     name = "join_ece_201534",
#     temporary = TRUE
#   )
# 
# join_sisfoh_2015 <- join_baseline_2015 %>%
#   inner_join(sisfoh)
# 
# ## JOINS
# 
# join_2013 <- inner_join(join_ece_2013, join_sisfoh_2013) %>%
#   collect()
# 
# join_2014 <- inner_join(join_ece_2014, join_sisfoh_2014) %>%
#   collect()
# 
# join_2015 <- inner_join(join_ece_2015, join_sisfoh_2015) %>%
#   collect()
# 
# table_2013 <- join_2013 %>%
#   mutate(
#     diff_fnac = (fnac %--% fnac_sisfoh) %>% as.period(),
#     diff_fnac = case_when(
#       diff_fnac == period(seconds = 0) ~ 1, #Identical
#       diff_fnac == period(year = 1) ~ 2, #sisfoh date is a possible typo (upwards)
#       diff_fnac == period(month = 1) ~ 2,
#       diff_fnac == period(day = 1) ~ 2,
#       diff_fnac == period(year = -1) ~ -2, #sisfoh date is a possible typo (downwards)
#       diff_fnac == period(month = -1) ~ -2,
#       diff_fnac == period(day = -1) ~ -2,
#       TRUE ~ 3 # Very different
#     ),
#     valid_fnac = case_when(
#       fnac == fnac_sisfoh ~ 1L,
#       fnac != fnac_sisfoh & diff_fnac == 2 ~ 2L,
#       fnac != fnac_sisfoh & diff_fnac == -2 ~ 3L,
#       fnac != fnac_sisfoh ~ 4L,
#       is.na(fnac_sisfoh) ~ 5L,
#     ),
#     valid_ndoc = case_when(
#       ndoc == ndoc_sisfoh & ndoc == ndoc_ece ~ 1L,
#       ndoc == ndoc_sisfoh & is.na(ndoc_ece) ~ 2L,
#       is.na(ndoc_sisfoh) & ndoc == ndoc_ece ~ 3L,
#       ndoc != ndoc_sisfoh & ndoc == ndoc_ece ~ 4L,
#       ndoc == ndoc_sisfoh & ndoc != ndoc_ece ~ 5L,
#       ndoc_sisfoh == ndoc_ece & ndoc != ndoc_ece ~ 6L,
#       is.na(ndoc_sisfoh) & ndoc != ndoc_ece ~ 7L,
#       ndoc != ndoc_sisfoh & is.na(ndoc_ece) ~ 8L,
#       ndoc != ndoc_sisfoh & ndoc != ndoc_ece ~ 9L,
#       is.na(ndoc_sisfoh) & is.na(ndoc_ece) ~ 10L
#     )
#   ) %>% 
#   group_by(ndoc) %>%
#   arrange(valid_ndoc, valid_fnac, .by_group = TRUE) %>%
#   filter(row_number()==1) 
# 
# table_2014 <- join_2014 %>%
#   mutate(
#     diff_fnac = (fnac %--% fnac_sisfoh) %>% as.period(),
#     diff_fnac = case_when(
#       diff_fnac == period(seconds = 0) ~ 1, #Identical
#       diff_fnac == period(year = 1) ~ 2, #sisfoh date is a possible typo (upwards)
#       diff_fnac == period(month = 1) ~ 2,
#       diff_fnac == period(day = 1) ~ 2,
#       diff_fnac == period(year = -1) ~ -2, #sisfoh date is a possible typo (downwards)
#       diff_fnac == period(month = -1) ~ -2,
#       diff_fnac == period(day = -1) ~ -2,
#       TRUE ~ 3 # Very different
#     ),
#     valid_fnac = case_when(
#       fnac == fnac_sisfoh ~ 1L,
#       fnac != fnac_sisfoh & diff_fnac == 2 ~ 2L,
#       fnac != fnac_sisfoh & diff_fnac == -2 ~ 3L,
#       fnac != fnac_sisfoh ~ 4L,
#       is.na(fnac_sisfoh) ~ 5L,
#     ),
#     valid_ndoc = case_when(
#       ndoc == ndoc_sisfoh & ndoc == ndoc_ece ~ 1L,
#       ndoc == ndoc_sisfoh & is.na(ndoc_ece) ~ 2L,
#       is.na(ndoc_sisfoh) & ndoc == ndoc_ece ~ 3L,
#       ndoc != ndoc_sisfoh & ndoc == ndoc_ece ~ 4L,
#       ndoc == ndoc_sisfoh & ndoc != ndoc_ece ~ 5L,
#       ndoc_sisfoh == ndoc_ece & ndoc != ndoc_ece ~ 6L,
#       is.na(ndoc_sisfoh) & ndoc != ndoc_ece ~ 7L,
#       ndoc != ndoc_sisfoh & is.na(ndoc_ece) ~ 8L,
#       ndoc != ndoc_sisfoh & ndoc != ndoc_ece ~ 9L,
#       is.na(ndoc_sisfoh) & is.na(ndoc_ece) ~ 10L
#     )
#   ) %>% 
#   group_by(ndoc) %>%
#   arrange(valid_ndoc, valid_fnac, .by_group = TRUE) %>%
#   filter(row_number()==1) 
# 
# table_2015 <- join_2015 %>%
#   mutate(
#     diff_fnac = (fnac %--% fnac_sisfoh) %>% as.period(),
#     diff_fnac = case_when(
#       diff_fnac == period(seconds = 0) ~ 1, #Identical
#       diff_fnac == period(year = 1) ~ 2, #sisfoh date is a possible typo (upwards)
#       diff_fnac == period(month = 1) ~ 2,
#       diff_fnac == period(day = 1) ~ 2,
#       diff_fnac == period(year = -1) ~ -2, #sisfoh date is a possible typo (downwards)
#       diff_fnac == period(month = -1) ~ -2,
#       diff_fnac == period(day = -1) ~ -2,
#       TRUE ~ 3 # Very different
#     ),
#     valid_fnac = case_when(
#       fnac == fnac_sisfoh ~ 1L,
#       fnac != fnac_sisfoh & diff_fnac == 2 ~ 2L,
#       fnac != fnac_sisfoh & diff_fnac == -2 ~ 3L,
#       fnac != fnac_sisfoh ~ 4L,
#       is.na(fnac_sisfoh) ~ 5L,
#     ),
#     valid_ndoc = case_when(
#       ndoc == ndoc_sisfoh & ndoc == ndoc_ece ~ 1L,
#       ndoc == ndoc_sisfoh & is.na(ndoc_ece) ~ 2L,
#       is.na(ndoc_sisfoh) & ndoc == ndoc_ece ~ 3L,
#       ndoc != ndoc_sisfoh & ndoc == ndoc_ece ~ 4L,
#       ndoc == ndoc_sisfoh & ndoc != ndoc_ece ~ 5L,
#       ndoc_sisfoh == ndoc_ece & ndoc != ndoc_ece ~ 6L,
#       is.na(ndoc_sisfoh) & ndoc != ndoc_ece ~ 7L,
#       ndoc != ndoc_sisfoh & is.na(ndoc_ece) ~ 8L,
#       ndoc != ndoc_sisfoh & ndoc != ndoc_ece ~ 9L,
#       is.na(ndoc_sisfoh) & is.na(ndoc_ece) ~ 10L
#     )
#   ) %>% 
#   group_by(ndoc) %>%
#   arrange(valid_ndoc, valid_fnac, .by_group = TRUE) %>%
#   filter(row_number()==1) 
# 
# table <- bind_rows(table_2013, table_2014, table_2015) %>%
#   group_by(ndoc) %>%
#   arrange(valid_ndoc, valid_fnac, .by_group = TRUE) %>%
#   filter(row_number()==1) %>%
#   filter(valid_ndoc <= 6) %>%
#   mutate(
#     ndoc = if_else(valid_ndoc == 6, ndoc_sisfoh, ndoc),
#     fnac = if_else(valid_ndoc == 6, fnac_sisfoh, fnac),
#   ) %>%
#   dplyr::select(-matches("(ndoc_)|(fnac_)|(diff_)|(cod_mod)")) %>%
#   group_by(paterno, materno, nombres) %>%
#   mutate(
#     homonyms = n()-1,
#     filter_stage = 4L
#   )
# 
# ## Duplicates were found
# ## ERROR:  duplicate key value violates unique constraint "student_id_pkey1"
# ## Key (paterno, materno, nombres, mujer, fnac)=(taipe, hernandez, naysha mayli, t, 2007-01-27) already exists.
# ## Duplicates were found because of the last stage filter (4) valid_ndoc == 6 change.
# 
# ids <- tbl(db_conn, "student_id") %>%
#   dplyr::select(ndoc) %>%
#   collect()
# 
# table %<>% anti_join(ids)
# 
# ###### FOURTH JOIN FILTER (security_level == 1)
# ###### Identified: 65289 (without duplicates) 5x2 (duplicates) 2,221,348 (Total)
# ###### By: father's surname, mother's surname, names, sex, birthday, unique ID number, cod_mod.
# ###### Matching observations in sisfoh and ece
# ###### ECE by: paterno, materno, nombres, mujer, ndoc, cod_mod, periodo
# ###### sisfoh by: paterno, materno, nombres, mujer, fnac, ndoc
# 
# dbWriteTable(db_conn, name = "student_id", table, temporary = FALSE, append = TRUE)
# 
# # Clear variables
# rm(list = ls(pattern = "(ece_)|(filter_school_)|(join_)|(table)|(data)|(sisfoh)"))
# 
# ## Step 29: Update id numbers already existing in student_id table
# 
# ids <- tbl(db_conn, "student_id") %>%
#   dplyr::select(ndoc) %>%
#   collect()
# 
# ## Step 30: Remove already identified individuals from students_siagie
# ## valid_ndoc == 6 individuals are still in the table, since ndocs were changed
# 
# table <- tbl(db_conn, "students_siagie") %>%
#   collect() %>%
#   anti_join(ids)
# 
# dbWriteTable(db_conn, name = "students_siagie", table, temporary = FALSE, overwrite = TRUE)
# 
# # Clear variables
# rm(ids, table)
# 
# ### CHANGE OF STRATEGY ######################################################################
# ### SELECT ALL OBSERVATIONS FROM INDIVIDUAL TABLES ##########################################
# 
# ## ECE
# 
# ece <- tbl(db_conn, "ece") %>%
#   dplyr::select(paterno, materno, nombres, mujer, ndoc) %>%
#   rename(ndoc_ece = ndoc)
# 
# join_ece <- tbl(db_conn, "students_siagie") %>%
#   inner_join(ece) %>%
#   compute(
#     name = "join_ece_last",
#     temporary = TRUE
#   )
# 
# ## sisfoh
# 
# sisfoh <- tbl(db_conn, "sisfoh") %>%
#   dplyr::select(paterno, materno, nombres, mujer, fnac, ndoc) %>%
#   rename(
#     ndoc_sisfoh = ndoc,
#     fnac_sisfoh = fnac
#   )
# 
# join_sisfoh <- tbl(db_conn, "students_siagie") %>%
#   inner_join(sisfoh) %>%
#   compute(
#     name = "join_sisfoh_last",
#     temporary = TRUE
#   )
# 
# ## FULL JOIN
# 
# join <- full_join(join_ece, join_sisfoh) %>%
#   collect()
# 
# table <- join %>%
#   mutate(
#     diff_fnac = (fnac %--% fnac_sisfoh) %>% as.period(),
#     diff_fnac = case_when(
#       diff_fnac == period(seconds = 0) ~ 1, #Identical
#       diff_fnac == period(year = 1) ~ 2, #sisfoh date is a possible typo (upwards)
#       diff_fnac == period(month = 1) ~ 2,
#       diff_fnac == period(day = 1) ~ 2,
#       diff_fnac == period(year = -1) ~ -2, #sisfoh date is a possible typo (downwards)
#       diff_fnac == period(month = -1) ~ -2,
#       diff_fnac == period(day = -1) ~ -2,
#       TRUE ~ 3 # Very different
#     ),
#     valid_fnac = case_when(
#       fnac == fnac_sisfoh ~ 1L,
#       fnac != fnac_sisfoh & diff_fnac == 2 ~ 2L,
#       fnac != fnac_sisfoh & diff_fnac == -2 ~ 3L,
#       fnac != fnac_sisfoh ~ 4L,
#       is.na(fnac_sisfoh) ~ 5L,
#     ),
#     valid_ndoc = case_when(
#       ndoc == ndoc_sisfoh & ndoc == ndoc_ece ~ 1L,
#       ndoc == ndoc_sisfoh & is.na(ndoc_ece) ~ 2L,
#       is.na(ndoc_sisfoh) & ndoc == ndoc_ece ~ 3L,
#       ndoc != ndoc_sisfoh & ndoc == ndoc_ece ~ 4L,
#       ndoc == ndoc_sisfoh & ndoc != ndoc_ece ~ 5L,
#       ndoc_sisfoh == ndoc_ece & ndoc != ndoc_ece ~ 6L,
#       is.na(ndoc_sisfoh) & ndoc != ndoc_ece ~ 7L,
#       ndoc != ndoc_sisfoh & is.na(ndoc_ece) ~ 8L,
#       ndoc != ndoc_sisfoh & ndoc != ndoc_ece ~ 9L,
#       is.na(ndoc_sisfoh) & is.na(ndoc_ece) ~ 10L
#     )
#   ) %>% 
#   group_by(ndoc) %>%
#   arrange(valid_ndoc, valid_fnac, .by_group = TRUE) %>%
#   filter(row_number()==1) %>%
#   filter(valid_ndoc <= 3) %>%
#   dplyr::select(-matches("(ndoc_)|(fnac_)|(diff_)")) %>%
#   group_by(paterno, materno, nombres) %>%
#   mutate(
#     homonyms = n()-1,
#     filter_stage = 5L
#   )
# 
# #ids
# ids <- tbl(db_conn, "student_id") %>%
#   dplyr::select(ndoc) %>%
#   collect()
# 
# table %<>% anti_join(ids)
#   
# ###### FIFTH CHECK JOIN FILTER (security_level == 1)
# ###### Identified: 2,716,772
# ###### By: father's surname, mother's surname, names, sex, birthday, unique ID number.
# ###### Details: No homonyms
# 
# dbWriteTable(db_conn, name = "student_id", table, temporary = FALSE, append = TRUE)
# 
# # Clear variables
# rm(list = ls(pattern = "(ece)|(sisfoh)|(join)|(ids)|(table)"))
# 
# ## Step 29: Update id numbers already existing in student_id table
# 
# ids <- tbl(db_conn, "student_id") %>%
#   dplyr::select(ndoc) %>%
#   collect()
# 
# ## Step 30: Remove already identified individuals from students_siagie
# ## valid_ndoc == 6 individuals are still in the table, since ndocs were changed
# 
# table <- tbl(db_conn, "students_siagie") %>%
#   collect() %>%
#   anti_join(ids)
# 
# dbWriteTable(db_conn, name = "students_siagie", table, temporary = FALSE, overwrite = TRUE)
# 
# # Clear variables
# rm(ids, table)

###########################################

















siagie_2013 <- tbl(db_conn, "siagie2013") %>%
  inner_join(tbl(db_conn, "student_id")) %>%
  dplyr::select(cod_mod, anexo, periodo, nivel, turno, grado, seccion, ndoc, paterno, materno, nombres, fnac, mujer) %>%
  inner_join(tbl(db_conn, "schools")) %>%
  dplyr::select(cod_mod, anexo, periodo, nivel, turno, grado, seccion, ndoc, paterno, materno, nombres, fnac, mujer) %>%
  collect()
  
dbWriteTable(db_conn, "matriculation", siagie_2013, append = TRUE)
rm(siagie_2013)

siagie_2014 <- tbl(db_conn, "siagie2014") %>%
  inner_join(tbl(db_conn, "student_id")) %>%
  dplyr::select(cod_mod, anexo, periodo, nivel, turno, grado, seccion, ndoc, paterno, materno, nombres, fnac, mujer) %>%
  inner_join(tbl(db_conn, "schools")) %>%
  dplyr::select(cod_mod, anexo, periodo, nivel, turno, grado, seccion, ndoc, paterno, materno, nombres, fnac, mujer) %>%
  collect()

dbWriteTable(db_conn, "matriculation", siagie_2014, append = TRUE)  
rm(siagie_2014)  

siagie_2015 <- tbl(db_conn, "siagie2015") %>%
  inner_join(tbl(db_conn, "student_id")) %>%
  dplyr::select(cod_mod, anexo, periodo, nivel, turno, grado, seccion, ndoc, paterno, materno, nombres, fnac, mujer) %>%
  inner_join(tbl(db_conn, "schools")) %>%
  dplyr::select(cod_mod, anexo, periodo, nivel, turno, grado, seccion, ndoc, paterno, materno, nombres, fnac, mujer) %>%
  collect()

dbWriteTable(db_conn, "matriculation", siagie_2015, append = TRUE)  
rm(siagie_2015)

### Matriculation

matriculation_1 <- tbl(db_conn, "ece") %>%
  dplyr::select(cod_mod, anexo, periodo, tipo_eva, seccion, ndoc, paterno, materno, nombres, mujer) %>%
  inner_join(tbl(db_conn, "student_id")) %>%
  dplyr::select(cod_mod, anexo, periodo, tipo_eva, seccion, ndoc, paterno, materno, nombres, fnac, mujer) %>%
  inner_join(tbl(db_conn, "schools")) %>%
  dplyr::select(cod_mod, anexo, periodo, tipo_eva, seccion, ndoc, paterno, materno, nombres, fnac, mujer) %>%
  collect() %>%
  mutate(
    grado = case_when(
      tipo_eva == 1 ~ 7,
      tipo_eva == 2 ~ 9,
      tipo_eva == 3 ~ 13,
    )
  ) %>%
  dplyr::select(-tipo_eva)
  
matriculation_2 <- tbl(db_conn, "ece") %>%
  dplyr::select(cod_mod, anexo, periodo, tipo_eva, seccion, ndoc, paterno, materno, nombres, mujer) %>%
  inner_join(tbl(db_conn, "student_id"), by = c("ndoc", "mujer"), suffix = c(".ece", "")) %>%
  dplyr::select(cod_mod, anexo, periodo, tipo_eva, seccion, ndoc, paterno, materno, nombres, fnac, mujer) %>%
  inner_join(tbl(db_conn, "schools")) %>%
  dplyr::select(cod_mod, anexo, periodo, tipo_eva, seccion, ndoc, paterno, materno, nombres, fnac, mujer) %>%
  collect() %>%
  mutate(
    grado = case_when(
      tipo_eva == 1 ~ 7,
      tipo_eva == 2 ~ 9,
      tipo_eva == 3 ~ 13,
    )
  ) %>%
  dplyr::select(-tipo_eva)

matriculation_3 <- tbl(db_conn, "ece") %>%
  dplyr::select(cod_mod, anexo, periodo, tipo_eva, seccion, ndoc, paterno, materno, nombres, mujer) %>%
  inner_join(tbl(db_conn, "student_id"), by = c("paterno", "materno", "nombres", "mujer"), suffix = c(".ece", "")) %>%
  dplyr::select(cod_mod, anexo, periodo, tipo_eva, seccion, ndoc, paterno, materno, nombres, fnac, mujer) %>%
  inner_join(tbl(db_conn, "schools")) %>%
  dplyr::select(cod_mod, anexo, periodo, tipo_eva, seccion, ndoc, paterno, materno, nombres, fnac, mujer) %>%
  collect() %>%
  mutate(
    grado = case_when(
      tipo_eva == 1 ~ 7,
      tipo_eva == 2 ~ 9,
      tipo_eva == 3 ~ 13,
    )
  ) %>%
  dplyr::select(-tipo_eva)

matriculation <- full_join(matriculation_1, matriculation_2) %>%
  full_join(matriculation_3) %>%
  distinct()
rm(matriculation_1, matriculation_2, matriculation_3)

dbWriteTable(db_conn, name = "matriculation_ece", matriculation, append = TRUE)

matriculation <- tbl(db_conn, "matriculation_ece") %>%
  semi_join(tbl(db_conn, "student_id")) %>%
  anti_join(tbl(db_conn, "matriculation"), by = c("cod_mod", "anexo", "periodo", "ndoc", "paterno", "materno", "nombres", "fnac", "mujer")) %>%
  dplyr::select(cod_mod, anexo, periodo, grado, seccion, ndoc, paterno, materno, nombres, fnac, mujer) %>%
  group_by(cod_mod, anexo, periodo, ndoc, paterno, materno, nombres, fnac, mujer) %>%
  compute(name = "matriculation_ece2", temporary = TRUE)

matriculation_dup <- dplyr::filter(matriculation, n() > 1) %>% collect()
matriculation_sin <- dplyr::filter(matriculation, n() == 1) %>% collect()
  
dbWriteTable(db_conn, name = "matriculation", matriculation_sin, append = TRUE)

#################### ECE 
test_scores_1 <- tbl(db_conn, "matriculation") %>%
  inner_join(tbl(db_conn, "ece"), 
             by = c("cod_mod", "anexo", "periodo", "ndoc", "mujer"),
             suffix = c(".mat", "")) %>%
  dplyr::select(matriculation_id, seccion, cod_est, starts_with("grupo"), starts_with("m500"), starts_with("aj"), ise) %>%
  collect()

test_scores_2 <- tbl(db_conn, "matriculation") %>%
  inner_join(tbl(db_conn, "ece"), 
             by = c("cod_mod", "anexo", "periodo", "paterno", "materno", "nombres", "mujer"),
             suffix = c(".mat", "")) %>%
  dplyr::select(matriculation_id, seccion, cod_est, starts_with("grupo"), starts_with("m500"), starts_with("aj"), ise) %>%
  collect()

test_scores <- bind_rows(test_scores_1, test_scores_2) %>% distinct(matriculation_id, seccion, cod_est, .keep_all = TRUE)
dbWriteTable(db_conn, name = "test_scores", test_scores, append = TRUE)
rm(list = ls(pattern = "test_scores"))

ggplot(tbl(db_conn, "ece") %>% select(m500_m, m500_l)) + geom_histogram(aes(x=m500_m))
ggplot(tbl(db_conn, "test_scores") %>% select(m500_m, m500_l)) + geom_histogram(aes(x=m500_m))

#################### ECE 
test_scores_1 <- tbl(db_conn, "matriculation") %>%
  inner_join(tbl(db_conn, "ece"), 
             by = c("cod_mod", "anexo", "periodo", "ndoc", "mujer"),
             suffix = c(".mat", "")) %>%
  dplyr::select(matriculation_id, seccion, cod_est, starts_with("grupo"), starts_with("m500"), starts_with("aj"), ise) %>%
  collect()

test_scores_2 <- tbl(db_conn, "matriculation") %>%
  inner_join(tbl(db_conn, "ece"), 
             by = c("cod_mod", "anexo", "periodo", "paterno", "materno", "nombres", "mujer"),
             suffix = c(".mat", "")) %>%
  dplyr::select(matriculation_id, seccion, cod_est, starts_with("grupo"), starts_with("m500"), starts_with("aj"), ise) %>%
  collect()

test_scores <- bind_rows(test_scores_1, test_scores_2) %>% distinct(matriculation_id, seccion, cod_est, .keep_all = TRUE)
dbWriteTable(db_conn, name = "test_scores", test_scores, append = TRUE)
