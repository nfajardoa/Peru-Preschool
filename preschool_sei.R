#### SOCIOECONOMIC INDEX
variable_dictionary <- read_excel("ECE/Otros/survey_summaries.xlsx", sheet = 'sisfoh')
ise_names <- str_subset(variable_dictionary$variable, '^ise_')

### SISFOH
tbl(db_conn, "sisfoh") %>%
  rename_with(translate_variables, names_dictionary = variable_dictionary) %>%
  select(
    ubigeo, idccpp, nomcp,
    numero, periodo, relationship_hh, education,
    starts_with("ise_house"), starts_with("ise_utilities"),
    starts_with("ise_assets"), starts_with("ise_other")
  ) %>%
  left_join(tbl(db_conn, 'conversion_ccpp') %>% select(idccpp, cluster), by = 'idccpp') %>%
  left_join(tbl(db_conn, 'conversion_ccpp') %>% select(ubigeo, nomcp, cluster), by = c('ubigeo', 'nomcp')) %>%
  mutate(cluster = coalesce(cluster.x, cluster.y)) %>%
  select(-matches('^cluster\\.'), -ubigeo, -idccpp, -nomcp) %>%
  relocate(cluster, .before = 1) %>%
  group_by(numero) %>%
  mutate(across(any_of(ise_names), max, na.rm = TRUE)) %>%
  filter(relationship_hh %in% c(1, 2)) %>%
  window_order(desc(education)) %>%
  filter(row_number() == 1) %>%
  mutate(
    ise_house.wall = case_when(
      ise_house.wall == 1 ~ 5L,
      ise_house.wall == 2 ~ 2L,
      ise_house.wall == 3 ~ 4L,
      ise_house.wall == 4 ~ 2L,
      ise_house.wall == 5 ~ 2L,
      ise_house.wall == 6 ~ 3L,
      ise_house.wall == 7 ~ 1L,
      ise_house.wall == 8 ~ 1L,
      is.na(ise_house.wall) ~ NA_integer_
    ),
    ise_house.floor = case_when(
      ise_house.floor == 1 ~ 6L,
      ise_house.floor == 2 ~ 5L,
      ise_house.floor == 3 ~ 4L,
      ise_house.floor == 4 ~ 3L,
      ise_house.floor == 5 ~ 2L,
      ise_house.floor == 6 ~ 1L,
      ise_house.floor == 7 ~ 1L,
      is.na(ise_house.floor) ~ NA_integer_
    ),
    ise_house.roof = case_when(
      ise_house.roof == 1 ~ 7L,
      ise_house.roof == 2 ~ 5L,
      ise_house.roof == 3 ~ 6L,
      ise_house.roof == 4 ~ 4L,
      ise_house.roof == 5 ~ 3L,
      ise_house.roof == 6 ~ 2L,
      ise_house.roof == 7 ~ 4L,
      ise_house.roof == 8 ~ 1L,
      is.na(ise_house.roof) ~ NA_integer_
    ),
    education = case_when(
      education == 1 ~ 0,
      education == 2 ~ 3,
      education == 3 ~ 6,
      education == 4 ~ 11,
      education == 5 ~ 14,
      education == 6 ~ 16,
      education == 7 ~ 19,
      is.na(education) ~ NA_real_
    ),
    across(matches("(^ise_utilities)|(^ise_assets)|(^ise_other)"), ~ case_when(
      . == 1 ~ TRUE,
      is.na(.) ~ NA,
      TRUE ~ FALSE
    )),
    across(!matches("(^education)|(^ise_utilities)|(^ise_assets)|(^ise_other)"), as.integer)
  ) %>%
  select(-cluster, -relationship_hh) %>%
  compute(name = "ise_reduced", temporary = FALSE, overwrite = TRUE)

### ECE
ece_ise <- tbl(db_conn, 'ece_survey') %>%
  mutate(education = greatest(education.mother, education.father)) %>%
  select(periodo, tipo_eva, cod_mod, anexo, seccion, cod_est, education, matches('ise_')) %>%
  left_join(tbl(db_conn, 'ece') %>% select(id_ece, periodo, tipo_eva, cod_mod, anexo, seccion, cod_est)) %>%
  left_join(tbl(db_conn, 'student_id') %>% select(id_ece, id_sisfoh)) %>%
  select(id_sisfoh, periodo, matches('^education'), all_of(ise_names)) %>%
  left_join(tbl(db_conn, "sisfoh") %>% select(numero, id_sisfoh)) %>%
  select(numero, periodo, matches('^education'), all_of(ise_names)) %>%
  collect()

dbWriteTable(db_conn, 'ise_reduced', ece_ise, append = TRUE, temporary = FALSE)

## Save data
data_ise <- tbl(db_conn, "ise_reduced") %>% collect()
saveRDS(data_ise, "families/ise_reduced.rds")

## Read data
data_ise <- readRDS("ise_reduced.rds")

#ISE
ise <- data_ise %>%
  run_ise(m = 10, maxit = 50, visitSequence = "monotone", cluster.seed = 343, n.core = 20)

saveRDS(ise, file = "imputed_ise.rds")
