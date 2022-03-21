#### RENAME SISFOH
variable_dictionary <- read_excel("ECE/Otros/survey_summaries.xlsx", sheet = "sisfoh")

tbl(db_conn, "student_id") %>%
  left_join(tbl(db_conn, "sisfoh") %>%
              rename_with(translate_variables, names_dictionary = variable_dictionary) %>%
              mutate(
                welfare = welfare.vasodeleche + welfare.comedor + welfare.comedorescolar + welfare.papilla + welfare.panfar + welfare.juntos + welfare.mivivienda + welfare.otro,
                welfare = if_else(welfare > 0L, 1L, 0L),
                healthcare = healthcare.essalud + healthcare.militar + healthcare.privado + healthcare.sis,
                healthcare = if_else(healthcare > 0L, 1L, 0L),
                spanish = if_else(idioma01 == 0 | idioma01 == 4, 1L, 0L),
                children = if_else(relationship_hh %in% c(3, 5, 7), 1L, 0L)
              ) %>%
              arrange(numero, children) %>%
              group_by(numero, children) %>%
              mutate(first_born = if_else(children == 1L & fnac == min(fnac), 1L, 0L)) %>%
              select(numero, id_sisfoh, relationship_hh, children, first_born, household.size, welfare, healthcare, spanish, puntaje)) %>%
  left_join( # Retrieve treatment (Survey) and test information
    tbl(db_conn, "ece") %>%
      left_join(tbl(db_conn, "ece_survey")) %>%
      mutate(
        spanish_survey = coalesce(language.home, language) %>% `!=`(1),
        spanish_survey = if_else(spanish_survey == TRUE, 1L, 0L),
      ) %>%
      select(id_ece, cod_mod, seccion, preschool, tipo_eva, periodo, estatal, unidocente, grupo_l, grupo_m, m500_l, m500_m, matches("grades"), exp_education, relocated, spanish_survey) %>%
      rename(preschool_ate = preschool)
  ) %>%
  mutate(
    age_diff = periodo - pyear,
    age_diff = case_when(
      tipo_eva == 1 ~ abs(age_diff - 2),
      tipo_eva == 2 ~ abs(age_diff - 4),
      tipo_eva == 3 ~ abs(age_diff - 8)
    ),
    inconsistent = if_else(age_diff > 3 | age_diff < 2 | pyear >= periodo, 1L, 0L),
    pyear = case_when(
      tipo_eva == 1 & (inconsistent == 1L) ~ periodo - 2 - as.integer(pyear_check),
      tipo_eva == 2 & (inconsistent == 1L) ~ periodo - 4 - as.integer(pyear_check),
      tipo_eva == 3 & (inconsistent == 1L) ~ periodo - 8 - as.integer(pyear_check),
      TRUE ~ pyear
    ),
  ) %>%
  arrange(match_id, tipo_eva, inconsistent, age_diff) %>%
  group_by(match_id, tipo_eva) %>%
  filter(row_number() == 1 | between(age_diff, 2, 3)) %>%
  compute(name = "clean_id", temporary = FALSE)

## Execute families algorithm with corrected pyear
source("preschool_families.R")

tbl(db_conn, "clean_id") %>%
  left_join(
    tbl(db_conn, "families") %>%
      rename(year = periodo) %>%
      left_join(tbl(db_conn, "education_supply")) %>%
      select(numero, year, cluster, rural, uniparental, section_a) %>%
      rename(pyear = year) %>%
      mutate(preschool_itt = section_a > 0)
  ) %>%
  mutate(
    across(matches("(pyear)|(estatal)|(unidocente)|(relocated)|(rural)|(uniparental)|(preschool_itt)"), as.integer),
    spanish = coalesce(spanish_survey, spanish),
    preschool_itt = if_else(is.na(preschool_itt), 0L, preschool_itt)
  ) %>%
  select(-spanish_survey) %>%
  group_by(numero) %>%
  mutate(across(matches("(rural)|(uniparental)|(welfare)|(healthcare)|(household.size)|(spanish)"), max)) %>%
  group_by(match_id, tipo_eva) %>%
  mutate(across(matches("(preschool_ate)|(grades)|(exp_education)|(relocated)"), max)) %>%
  group_by(match_id) %>%
  mutate(across(matches("(preschool_ate)|(grades)|(exp_education)|(relocated)"), ~ if_else(is.na(.), max(.), .))) %>%
  group_by(cod_mod, seccion, periodo) %>%
  mutate(
    unidocente = sum(unidocente),
    estatal = sum(estatal)
  ) %>%
  group_by(cod_mod, seccion) %>%
  mutate(
    unidocente = if_else(is.null(unidocente), max(unidocente), unidocente),
    estatal = if_else(is.null(estatal), max(estatal), estatal)
  ) %>%
  compute(name = "regression_table", temporary = FALSE)

###### ANALYSIS DATA ########################################################################################################################
analysis_variables <- c(
  "match_id", "mujer", "numero", "pyear_check", "pyear",
  "children", "household.size", "welfare", "healthcare", "spanish", "puntaje",
  "first_born", "rural", "estatal", "unidocente", "periodo", "uniparental",
  "preschool_itt", "preschool_ate", "m500_l", "m500_m"
)

##### Second stage
dataset_ses <- tbl(db_conn, 'regression_table') %>% 
  filter(tipo_eva == 1) %>%
  select(all_of(analysis_variables)) %>%
  collect() %>%
  mutate(
    cohort = cut(pyear, include.lowest = FALSE, right = FALSE, dig.lab = 1, breaks = c(2002, 2005, 2010, 2013, 2017)),
    preschool_ate = (preschool_ate > 0) %>% as.numeric()
  ) %>%
  group_by(numero) %>%
  mutate(
    switcher_itt = mean(preschool_itt, na.rm = TRUE),
    switcher_itt = (switcher_itt > 0 & switcher_itt < 1),
    switcher_ate = mean(preschool_ate, na.rm = TRUE),
    switcher_ate = (switcher_ate > 0 & switcher_ate < 1),
  ) %>%
  mutate(
    unidocente = unidocente > 0,
    estatal = estatal > 0,
    nospanish = 1 - spanish,
    across(matches("(switcher)|(mujer)|(estatal)|(unidocente)"), ~ as.factor(.x) %>% fct_recode("0" = "FALSE", "1" = "TRUE")),
    across(matches("(mujer)|(first_born)|(pyear_check)|(estatal)|(unidocente)|(rural)|(nospanish)|(welfare)|(healthcare)|(itt)|(ate)"), as.factor),
  )

#### Fourth stage
dataset_fos <- tbl(db_conn, 'regression_table') %>% 
  filter(tipo_eva == 2) %>%
  select(all_of(analysis_variables)) %>%
  collect() %>%
  mutate(
    cohort = cut(pyear, include.lowest = FALSE, right = FALSE, dig.lab = 1, breaks = c(2002, 2005, 2010, 2013, 2017)),
    preschool_ate = (preschool_ate > 0) %>% as.numeric()
  ) %>%
  group_by(numero) %>%
  mutate(
    switcher_itt = mean(preschool_itt, na.rm = TRUE),
    switcher_itt = (switcher_itt > 0 & switcher_itt < 1),
    switcher_ate = mean(preschool_ate, na.rm = TRUE),
    switcher_ate = (switcher_ate > 0 & switcher_ate < 1),
  ) %>%
  mutate(
    unidocente = unidocente > 0,
    estatal = estatal > 0,
    nospanish = 1 - spanish,
    across(matches("(switcher)|(mujer)|(estatal)|(unidocente)"), ~ as.factor(.x) %>% fct_recode("0" = "FALSE", "1" = "TRUE")),
    across(matches("(mujer)|(first_born)|(pyear_check)|(estatal)|(unidocente)|(rural)|(nospanish)|(welfare)|(healthcare)|(itt)|(ate)"), as.factor),
  )

#### Sixth stage
dataset_sis <- tbl(db_conn, 'regression_table') %>% 
  filter(tipo_eva == 3) %>%
  select(all_of(analysis_variables)) %>%
  collect() %>%
  mutate(
    cohort = cut(pyear, include.lowest = FALSE, right = FALSE, dig.lab = 1, breaks = c(2002, 2005, 2010, 2013, 2017)),
    preschool_ate = (preschool_ate > 0) %>% as.numeric()
  ) %>%
  group_by(numero) %>%
  mutate(
    switcher_itt = mean(preschool_itt, na.rm = TRUE),
    switcher_itt = (switcher_itt > 0 & switcher_itt < 1),
    switcher_ate = mean(preschool_ate, na.rm = TRUE),
    switcher_ate = (switcher_ate > 0 & switcher_ate < 1),
  ) %>%
  mutate(
    unidocente = unidocente > 0,
    estatal = estatal > 0,
    nospanish = 1 - spanish,
    across(matches("(switcher)|(mujer)|(estatal)|(unidocente)"), ~ as.factor(.x) %>% fct_recode("0" = "FALSE", "1" = "TRUE")),
    across(matches("(mujer)|(first_born)|(pyear_check)|(estatal)|(unidocente)|(rural)|(nospanish)|(welfare)|(healthcare)|(itt)|(ate)"), as.factor),
  )

saveRDS(dataset_ses, "dataset_ses.rds")
saveRDS(dataset_fos, "dataset_fos.rds")
saveRDS(dataset_sis, "dataset_sis.rds")
