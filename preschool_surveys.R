##### QUESTIONNAIRES
# ## Generate key
# c("2015_2p", "2016_2p", "2016_4p", "2018_4p", "2015_2s", "2016_2s", "2018_2s") %>%
#   map(~ read_excel(path = "ECE/Otros/survey_summaries.xlsx", sheet = .x)) %>%
#   reduce(full_join, by = c("definition")) %>%
#   rowwise() %>%
#   mutate(n_questions = c_across(starts_with("name")) %>% na.omit() %>% length()) %>%
#   ungroup() %>%
#   mutate(
#     survey_table = case_when(
#       str_detect(definition, "^noise_") ~ NA_integer_,
#       n_questions >= 5 | str_detect(definition, "(^ise_assets)|(retention)|(^language\\.home)|(^language$)|(relocated)|(preschool\\.time)|(^age\\.)") ~ 1L, # Main table
#       n_questions == 3 & str_detect(definition, "school\\.") ~ 2L, # Highschool pedagogical practices
#       TRUE ~ NA_integer_
#     )
#   ) %>%
#   mutate(across(where(is.character), str_trim)) %>%
#   write_csv(file = "ECE/Otros/survey_key.csv")
# 
# ## Generate dictionary
# survey_key <- read_csv("ECE/Otros/survey_key.csv") %>%
#   select(definition, starts_with("name"), survey_table) %>%
#   pivot_longer(cols = starts_with("name"), names_to = c("year", "type"), names_pattern = "name_(.*)_(.*)", values_to = "name") %>%
#   drop_na(name)
# 
# ## Upload to database
# list(c("2015", "2p"), c("2016", "2p"), c("2016", "4p"), c("2018", "4p"), c("2015", "2s"), c("2016", "2s"), c("2018", "2s")) %>%
#   walk(~ read_eceform(.x, survey_key = survey_key) %>%
#     dbWriteTable(db_conn, paste("eceqtn", paste(.x, collapse = "_"), sep = "_"), .))
# 
# ## Generate consolidated tables
# # Overall table
# list(c("2015", "2p"), c("2016", "2p"), c("2016", "4p"), c("2018", "4p"), c("2015", "2s"), c("2016", "2s"), c("2018", "2s")) %>%
#   map(~ tbl(db_conn, paste("eceqtn", paste(.x, collapse = "_"), sep = "_")) %>%
#     select(periodo, tipo_eva, cod_mod, anexo, seccion, cod_est, matches(select_ecetable(survey_key, 1))) %>%
#     collect() %>%
#     mutate(
#       across(starts_with("retention") & where(is.numeric), ~ if_else(.x > 0, TRUE, FALSE)),
#       retention = select(., starts_with("retention")) %>% apply(1, function(x) any(x, na.rm = TRUE))
#     ) %>%
#     select(-matches("retention\\."))) %>%
#   reduce(union_all) %>%
#   mutate(
#     ise_assets.autocamioneta = case_when(
#       periodo == 2015 & is.na(ise_assets.auto) & is.na(ise_assets.camioneta) ~ NA,
#       periodo == 2015 ~ any(ise_assets.auto, ise_assets.camioneta, na.rm = TRUE),
#       TRUE ~ ise_assets.autocamioneta
#     ),
#     language.home = case_when(
#       language.home.mother == language.home.father ~ language.home.mother,
#       !is.na(language.home.mother) & is.na(language.home.father) ~ language.home.mother,
#       is.na(language.home.mother) & !is.na(language.home.father) ~ language.home.father,
#       language.home.mother != language.home.father ~ 6L,
#       !is.na(language.home.mother) & !is.na(language.home.father) ~ language,
#     ),
#     relocated = if_else(relocated > 0, TRUE, FALSE)
#   ) %>%
#   select(
#     periodo, tipo_eva, cod_mod, anexo, seccion, cod_est, matches("preschool"),
#     matches("ise_house"), matches("ise_utilities"), matches("ise_assets"), matches("ise_other"),
#     matches("^education"), matches("^language"), matches("^age"), matches("n\\."),
#     matches("books"), matches("study"), matches("grades"), relocated, retention, exp_education,
#     -c(ise_assets.auto, ise_assets.camioneta)
#   ) %>%
#   dbWriteTable(db_conn, "ece_survey", .)
# 
# # Teaching Quality
# list(c("2015", "2s"), c("2016", "2s"), c("2018", "2s")) %>%
#   map(~ tbl(db_conn, paste("eceqtn", paste(.x, collapse = "_"), sep = "_")) %>%
#     select(periodo, tipo_eva, cod_mod, anexo, seccion, cod_est, matches(select_ecetable(survey_key, 2)))) %>%
#   reduce(union_all) %>%
#   compute(name = "ece_tquality", temporary = FALSE)
# 
# ### Treatment

treatment <- ls(pattern = "^form_") %>%
  include_name() %>%
  rename(cod_mod = cod_mod) %>%
  mutate(
    cod_mod = as.integer(cod_mod),
    anexo = as.integer(anexo),
    cod_est = as.integer(cod_est),
    tipo_eva = case_when(
      tipo_eva == "2p" ~ 1L,
      tipo_eva == "4p" ~ 2L,
      tipo_eva == "2s" ~ 3L
    ),
    seccion = str_to_lower(seccion),
    preschool = str_sub(preschool, 1, 5),
    preschool = case_when(
      str_detect(preschool, "No") ~ 0L,
      TRUE ~ 1L
    )
  )

dbWriteTable(db_conn, name = "ece_treatment", treatment)

#### TREATMENT

inconsistencies <- tbl(db_conn, "test_scores") %>%
  inner_join(tbl(db_conn, "matriculation")) %>%
  inner_join(tbl(db_conn, "ece_treatment")) %>%
  inner_join(tbl(db_conn, "student_id")) %>%
  group_by(student_id) %>%
  mutate(preschool_check = sum(preschool, na.rm = TRUE)) %>%
  filter(preschool_check != n(), preschool_check != 0) %>%
  collect()
