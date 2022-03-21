### Retrieve full Padron
padron_separated  <- list.files("Padron Unidades Educativas/Padron/") %>%
  str_subset("(zips)|(pdfs)", negate = TRUE) %>%
  paste0("Padron Unidades Educativas/Padron/", .) %>%
  map(read_with_name_dbf, as.is = TRUE)

padron_joint <- padron_separated %>%
  reduce(coalesce_join, by = c("cod_mod", "anexo"), reverse = TRUE) %>%
  mutate(
    cod_mod = cod_mod %>% as.integer(cod_mod),
    anexo = anexo %>% as.integer(anexo)
  )

#### SCHOOLS IN ECE
# ece_schools <- tbl(db_conn, "ece") %>%
#   select(cod_mod, anexo) %>%
#   distinct() %>%
#   collect()
# 
##### SCHOOLS IN SIAGIE
# siagie_schools <- list(
#   tbl(db_conn, "siagie2013") %>% select(cod_mod, anexo),
#   tbl(db_conn, "siagie2014") %>% select(cod_mod, anexo),
#   tbl(db_conn, "siagie2015") %>% select(cod_mod, anexo)
#   ) %>%
#   reduce(union_all) %>%
#   distinct() %>%
#   collect()
# 
## Total number of schools
## Padron
# padron_schools <- list.files("Padron Unidades Educativas/Padron/") %>%
#   str_subset("(zips)|(pdfs)", negate = TRUE) %>%
#   paste0("Padron Unidades Educativas/Padron/", .) %>%
#   map(read_with_name_dbf, as.is = TRUE) %>%
#   map(select, cod_mod, anexo) %>%
#   reduce(bind_rows) %>%
#   mutate(across(everything(), safe_integer)) %>%
#   distinct()
# 
# Databases
# schools <- bind_rows(ece_schools, siagie_schools) %>%
#   distinct()
# 
# # Check if all schools in databases are reported in Padron
# anti_join(schools, padron_schools) # 404 Schools in cleaned matriculation databases are not registered in any school censuses
# 
## NORMALIZE LOCATION
# Read correction files
correction <- read_csv("Padron Unidades Educativas/correction.csv") %>%
  drop_na(codccpp) %>%
  select(codccpp, codccpp_corrected) %>%
  mutate(across(everything(), as.integer))

correction_codmod <- read_csv("Padron Unidades Educativas/correction.csv") %>%
  drop_na(cod_mod) %>%
  select(cod_mod, anexo, codccpp_corrected) %>%
  rename(codccpp = codccpp_corrected) %>%
  mutate(across(everything(), as.integer))

# Check corrections
# 
# cp <- readRDS("Centros poblados/centros_poblados.rds") %>% as_tibble %>% select(codccpp, ubigeo, matches('nomcp'))
# 
# left_join(
#   correction,
#   padron_joint %>% mutate(codccpp = as.integer(codccpp)) %>%
#     select(cod_mod, codccpp, cen_pob, cen_edu, codgeo)
# ) %>%
#   left_join(cp %>% rename(codccpp_corrected = codccpp)) %>%
#   View()
# 
# left_join(
#   correction_codmod,
#   padron_joint %>%
#     mutate(codccpp = as.integer(codccpp)) %>%
#     select(cod_mod, anexo, codccpp, cen_pob, cen_edu, codgeo),
#   by = c('cod_mod', 'anexo'),
#   suffix = c('', '_false')
# ) %>%
#   left_join(cp) %>%
#   View()
# 
### Get all reported locations of each school (We want to have the most recent one per school)
# towns <- list.files("Padron Unidades Educativas/Padron/") %>%
#   str_subset("(zips)|(pdfs)", negate = TRUE) %>%
#   paste0("Padron Unidades Educativas/Padron/", .) %>%
#   map(read_with_name_dbf, variable = "codccpp", as.is = TRUE) %>%
#   map(select, cod_mod, anexo, starts_with("20")) %>%
#   reduce(full_join, by = c("cod_mod", "anexo")) %>%
#   mutate(
#     cod_mod = cod_mod %>% as.integer(cod_mod),
#     anexo = anexo %>% as.integer(anexo)
#   )
# 
# #Retrieve name of towns
# name_towns <- list.files("Padron Unidades Educativas/Padron/") %>%
#   str_subset("(zips)|(pdfs)", negate = TRUE) %>%
#   paste0("Padron Unidades Educativas/Padron/", .) %>%
#   map(read_with_name_dbf, variable = "cen_pob", as.is = TRUE) %>%
#   map(select, cod_mod, anexo, niv_mod, starts_with("20")) %>%
#   reduce(coalesce_join, by = c("cod_mod", "anexo")) %>%
#   mutate(
#     cod_mod = cod_mod %>% as.integer(cod_mod),
#     anexo = anexo %>% as.integer(anexo)
#   )
# 
# # Retrieve all possible village codes associated with reported schools in Padron
# codccpp_schools <- inner_join(schools, towns) %>%
#   pivot_longer(cols = starts_with('20'), names_to = "year", values_to = "codccpp", values_drop_na = TRUE) %>%
#   group_by(cod_mod, anexo) %>%
#   left_join(correction) %>%
#   mutate(codccpp = coalesce(codccpp_corrected, codccpp)) %>%
#   select(-codccpp_corrected) %>%
#   coalesce_join(correction_codmod, by = c("cod_mod", "anexo"), join = left_join, reverse = TRUE)
# 
# conversion_table <- read_csv('Centros poblados/conversion_table.csv') %>%
#   mutate(across(where(is.numeric) & !starts_with('idccpp'), as.integer), idccpp = as.integer64(idccpp))
# 
# missing_schools <- anti_join(
#   codccpp_schools,
#   inner_join(codccpp_schools, conversion_table) %>% distinct(cod_mod, anexo)) %>%
#   distinct(cod_mod, anexo)
# 
# # Manual matching :'(
# manual_1 <- semi_join(codccpp_schools, missing_schools) %>%
#   distinct(cod_mod, anexo) %>%
#   left_join(name_towns) %>%
#   rowwise() %>%
#   mutate(name_town = c_across(starts_with("20")) %>% remove_missingcodes() %>% na.omit() %>% unique() %>% reduce(paste)) %>%
#   select(cod_mod, anexo, niv_mod, name_town)
# 
# manual_2 <- semi_join(codccpp_schools, missing_schools) %>%
#   distinct(cod_mod, anexo) %>%
#   left_join(towns) %>%
#   rowwise() %>%
#   mutate(town = c_across(starts_with("20")) %>% na.omit() %>% unique() %>% reduce(paste) %>% as.character()) %>%
#   select(cod_mod, anexo, town)
# 
# manual <- full_join(manual_1, manual_2) %>%
#   left_join(padron_joint %>% select(cod_mod, anexo, codgeo, cen_edu))
# 
### DATA FROM Padron
# Determine services
conversion_table <- read_csv('Centros poblados/conversion_table.csv') %>%
  select(codccpp, cluster) %>%
  mutate(across(where(is.numeric), as.integer))

# niv_mod <- list.files("Padron Unidades Educativas/Padron/") %>%
#   str_subset("(zips)|(pdfs)", negate = TRUE) %>%
#   paste0("Padron Unidades Educativas/Padron/", .) %>%
#   map(read_with_name_dbf, variable = "niv_mod", as.is = TRUE) %>%
#   map(select, cod_mod, anexo, starts_with("20")) %>%
#   reduce(full_join, by = c("cod_mod", "anexo")) %>%
#   mutate(
#     cod_mod = safe_integer(cod_mod),
#     anexo = safe_integer(anexo)
#   ) %>%
#   group_by(cod_mod, anexo) %>%
#   pivot_longer(cols = starts_with("20"), names_to = "year", values_to = "niv_mod") %>%
#   mutate(year = as.integer(year), across(where(is.character), str_to_lower)) %>%
#   drop_na(niv_mod) %>%
#   distinct()
# 
#saveRDS(niv_mod, file = 'Centros poblados/schools_nivmod.rds')
niv_mod <- readRDS(file = 'Centros poblados/schools_nivmod.rds')

# set.seed(2306)
# codccpp <- list.files("Padron Unidades Educativas/Padron/") %>%
#   str_subset("(zips)|(pdfs)", negate = TRUE) %>%
#   paste0("Padron Unidades Educativas/Padron/", .) %>%
#   map(read_with_name_dbf, variable = "codccpp", as.is = TRUE) %>%
#   map(select, cod_mod, anexo, starts_with("20")) %>%
#   reduce(full_join, by = c("cod_mod", "anexo")) %>%
#   mutate(
#     cod_mod = safe_integer(cod_mod),
#     anexo = safe_integer(anexo)
#   ) %>%
#   pivot_longer(cols = starts_with("20"), names_to = "year", values_to = "codccpp") %>%
#   left_join(correction) %>%
#   mutate(codccpp = coalesce(codccpp_corrected, codccpp)) %>%
#   select(-codccpp_corrected) %>%
#   coalesce_join(correction_codmod, by = c("cod_mod", "anexo"), join = left_join, reverse = TRUE) %>%
#   semi_join(conversion_table) %>%
#   group_by(cod_mod, anexo) %>% 
#   filter(year == max(year)) %>% 
#   sample_n(1)
################################################################# Two preschools have uncertain locations. 
#saveRDS(codccpp, file = 'Centros poblados/schools_codccpp.rds')
codccpp <- readRDS(file = 'Centros poblados/schools_codccpp.rds')

### Schools table
# PRONOEI
pronoei_dictionary <- read_excel("ECE/Otros/survey_summaries.xlsx", sheet = 'ininoe')
missing_pronoei <- read_csv("Padron Unidades Educativas/missing_pronoei.csv") %>%
  mutate(across(everything(), as.integer))

ininoe <- 2011:2019 %>%
  map( ~ read_ininoeform(.x, survey_key = pronoei_dictionary) %>% mutate(year = .x, .before = 1))

ininoe_ise <- ininoe %>%
  map(select, year, cod_mod, anexo, starts_with('ise')) %>%
  reduce(bind_rows) %>%
  anti_join(missing_pronoei)

ininoe_info <- ininoe %>%
  map(select, -year, -starts_with('ise')) %>%
  reduce(full_join, by = c('cod_mod', 'anexo'), suffix = c("", '-m')) %>%
  reduce_with_function(reduce_fun = 'minmode', not_coalesce = c('books', 'tables', 'tables.need', 'time.school', 'time.town', 'transport.school', 'transport.town', 'cod_mod.alt')) %>%
  reduce_with_function(reduce_fun = 'lastmode', not_coalesce = c('books', 'tables', 'tables.need', 'time.school', 'time.town', 'transport.school', 'transport.town')) %>% 
  reduce_with_function(reduce_fun = 'mean') %>%
  mutate(
    across(where(is.numeric), ~ if_else(is.nan(.x), 0, as.numeric(.x))),
    distance.school = (time.school * transport.school) / 3600,
    distance.town = (time.town * transport.town) / 3600,
    across(matches('(cod_mod)|(anexo)|(fechareg)'), as.integer),
    tables.adequate = as.logical(tables.adequate)
    ) %>%
  relocate(cod_mod, anexo, fechareg, cod_mod.alt, books, .before = 1) %>%
  anti_join(missing_pronoei)

dbWriteTable(db_conn, name = "ininoe_info", ininoe_info, temporary = FALSE, overwrite = TRUE)

# plot <- ininoe %>%
#   select(cod_mod, anexo, starts_with("fechareg")) %>%
#   pivot_longer(cols = starts_with("fechareg"), names_to = "periodo") %>%
#   mutate(periodo = str_replace_all(periodo, "^fechareg", "2011") %>% str_replace_all("-m", "+1")) %>%
#   rowwise() %>%
#   mutate(periodo = rlang::parse_expr(periodo) %>% eval(), across(everything(), as.integer))
# 
# ggplot(plot) +
#   geom_histogram(aes(x = value), bins = 45) +
#   facet_grid(rows = vars(periodo)) +
#   scale_y_continuous(breaks = extended_range_breaks(), expand = c(0, 0), labels = number_format(accuracy = 1, big.mark = "")) +
#   scale_x_continuous(breaks = extended_range_breaks(), expand = c(0, 0), labels = number_format(accuracy = 1, big.mark = "")) +
#   theme_thesis() +
#   labs(x = "Year of Establishment", y = "Frecuency")
#
#ENROLLMENT
dictionary_preschool <- read_excel("ECE/Otros/survey_summaries.xlsx", sheet = 'resultados_preschool')
dictionary_school <- read_excel("ECE/Otros/survey_summaries.xlsx", sheet = 'resultados_school')

enrollment_preschool <- 2003:2019 %>%
  map(read_resultsform, survey_key = dictionary_preschool, table = "preschool") %>%
  bind_rows()

enrollment_school <- 2003:2019 %>%
  map(read_resultsform, survey_key = dictionary_school, table = "school") %>%
  bind_rows()

enrollment_preschool %>%
  group_by(year, niv_mod) %>%
  dplyr::count() %>%
  View()

#TEACHERS

#BUILDINGS
perra <- read.dbf("Padron Unidades Educativas/Cedulas/locales/local/Local2006.dbf")

perra <- read.dbf("Padron Unidades Educativas/Padron/Padron_2015.dbf")

###EDUCATION SUPPLY
# fechareg <- list.files("Padron Unidades Educativas/Padron/") %>%
#   str_subset("(zips)|(pdfs)", negate = TRUE) %>%
#   str_subset("(200[8-9])|(201[0-9])") %>%
#   # Get all after 2007
#   paste0("Padron Unidades Educativas/Padron/", .) %>%
#   map(read_with_name_dbf, variable = "fechareg", as.is = TRUE) %>%
#   map(select, cod_mod, anexo, starts_with("20")) %>%
#   reduce(full_join, by = c("cod_mod", "anexo")) %>%
#   mutate(
#     cod_mod = safe_integer(cod_mod),
#     anexo = safe_integer(anexo)
#   ) %>%
#   pivot_longer(cols = starts_with("20"), names_to = "year", values_to = "fechareg") %>%
#   mutate(
#     year = as.integer(year),
#     fechareg = chr2year(fechareg) %>% dmy() %>% round_date(unit = "year") %>% year(),
#     fechareg = if_else(fechareg > year, NA_integer_, fechareg)
#   ) %>%
#   group_by(cod_mod, anexo) %>%
#   summarise(fechareg = calculate_mode(fechareg, na.rm = TRUE)) %>%
#   ungroup()
# 
# fecharet <- list.files("Padron Unidades Educativas/Padron/") %>%
#   str_subset("(zips)|(pdfs)", negate = TRUE) %>%
#   str_subset("(200[8-9])|(201[0-9])") %>%
#   # Get all after 2007
#   paste0("Padron Unidades Educativas/Padron/", .) %>%
#   map(read_with_name_dbf, variable = "fecharet", as.is = TRUE) %>%
#   map(select, cod_mod, anexo, starts_with("20")) %>%
#   reduce(full_join, by = c("cod_mod", "anexo")) %>%
#   mutate(
#     cod_mod = safe_integer(cod_mod),
#     anexo = safe_integer(anexo)
#   ) %>%
#   pivot_longer(cols = starts_with("20"), names_to = "year", values_to = "fecharet") %>%
#   mutate(
#     year = as.integer(year),
#     fecharet = chr2year(fecharet) %>% dmy() %>% round_date(unit = "year") %>% year(),
#     fecharet = if_else(fecharet > year, NA_integer_, fecharet)
#   ) %>%
#   group_by(cod_mod, anexo) %>%
#   summarise(fecharet = calculate_mode(fecharet, na.rm = TRUE)) %>%
#   ungroup()
# 
# fechareg_alt <- niv_mod %>%
#   drop_na(niv_mod) %>%
#   group_by(cod_mod, anexo) %>%
#   summarise(regyear = min(year)) %>%
#   ungroup()
# 
# fecharet_alt <- niv_mod %>%
#   drop_na() %>%
#   group_by(cod_mod, anexo) %>%
#   summarise(retyear = max(year)) %>%
#   ungroup()
# 
# fechareg_pronoei <-
#   ininoe_info %>%
#   select(cod_mod, anexo, fechareg) %>%
#   drop_na(fechareg)
# 
# fecharet_pronoei <-
#   ininoe %>%
#   map(select, cod_mod, anexo, year) %>%
#   reduce(bind_rows) %>%
#   group_by(cod_mod, anexo) %>%
#   filter(year == max(year)) %>%
#   rename(fecharet = year) %>%
#   drop_na(fecharet) %>%
#   anti_join(missing_pronoei)
# 
# dates <- summarise(niv_mod, niv_mod = calculate_mode(niv_mod, na.rm = TRUE), .groups = "drop") %>%
#   full_join(fechareg) %>%
#   full_join(fechareg_alt) %>%
#   full_join(fechareg_pronoei, by = c('cod_mod', 'anexo'), suffix = c('', '.pronoei')) %>%
#   full_join(fecharet) %>%
#   full_join(fecharet_alt) %>%
#   full_join(fecharet_pronoei, by = c('cod_mod', 'anexo'), suffix = c('', '.pronoei')) %>%
#   group_by(cod_mod, anexo) %>%
#   mutate(
#     across(matches("year"), as.integer),
#     opening = min(fechareg.pronoei, fechareg, regyear, na.rm = TRUE) %>% as.integer(),
#     closing = max(fecharet.pronoei, fecharet, retyear, na.rm = TRUE) %>% as.integer(),
#     opening = if_else(opening <= 2000L, 2000L, opening),
#     closing = if_else(closing <= 2000L, 2000L, closing),
#   )
# 
#### 21 missing pronoei (Report local, but not activity)
### missing_pronoei <- dates %>% filter(is.na(retyear)) %>% select(cod_mod, anexo, matches('\\.pronoei'))
### ininoe_2015 <- read.dbf("Padron Unidades Educativas/Cedulas/locales/pronoei/LocalPRONOEI2015.dbf") %>%
###   rename_with(str_to_lower) %>%
###   select(cod_mod, anexo, codgeo, codooii) %>%
###   mutate(across(everything(), safe_integer)) %>%
###   inner_join(missing_pronoei)
### write_csv(missing_pronoei, "Padron Unidades Educativas/missing_pronoei.csv")
# 
# dates %<>%
#   transmute(year = list(opening:closing), open = 1L) %>%
#   unnest(cols = year) %>%
#   ungroup()
# 
# readr::write_csv(dates, "Padron Unidades Educativas/years_schools.csv")
dates <- read_csv('Padron Unidades Educativas/years_schools.csv')

school_cluster <- inner_join(codccpp, conversion_table) %>%
  mutate(year.codccpp = as.integer(year)) %>%
  select(-year) %>%
  mutate(across(where(is.numeric), as.integer))

dbWriteTable(db_conn, name = "school_cluster", school_cluster, temporary = FALSE, overwrite = TRUE)

education_supply <- inner_join(codccpp, dates, by = c("cod_mod", "anexo"), suffix = c(".codccpp", "")) %>%
  left_join(conversion_table) %>%
  left_join(niv_mod) %>%
  arrange(cod_mod, anexo, year) %>%
  group_by(cod_mod, anexo) %>%
  fill(niv_mod, .direction = "downup") %>%
  ungroup() %>%
  mutate(
    across(matches("year") | where(is.numeric), as.integer),
    niv_mod = case_when(
      str_detect(niv_mod, '(a)|(b)|(f)') ~ niv_mod,
      str_detect(niv_mod, '(e)') ~ "special",
      str_detect(niv_mod, '(d)|(c)|(g)') ~ "alt",
      str_detect(niv_mod, '(k)|(l)|(m)|(t)') ~ "high",
    )) %>%
  group_by(cluster, year) %>%
  pivot_wider(id_cols = c(cluster, year), names_from = niv_mod, names_prefix = "section_", values_from = open, values_fn = sum, values_fill = 0) %>%
  select(sort(tidyselect::peek_vars())) %>%
  relocate(cluster, year, .before = 1) %>%
  ungroup() %>%
  mutate(
    section_a = rowSums(across(matches('section_a[235]'))),
    total = rowSums(across(matches("(section_[a-z][0-9])|(section_[a-z]{3})")))
  ) %>%
  complete(cluster, year) %>%
  mutate(across(where(is.numeric), replace_na, value = 0L)) %>% 
  arrange(cluster, year) %>%
  group_by(cluster) %>%
  mutate(
    section_a.l = lag(section_a),
    section_a5.l = lag(section_a5),
    a5.building = section_a5 == 1 & section_a5.l == 0,
    a.building = section_a == 1 & section_a.l == 0,
    ab_cutoff = (section_b0 > 0) & (a.building == TRUE),
    ab_cutoff.1 = (lag(section_b0, 1) > 0) & (a.building == TRUE),
    ab_cutoff.2 = (lag(section_b0, 2) > 0) & (a.building == TRUE),
    ab_cutoff.3 = (lag(section_b0, 3) > 0) & (a.building == TRUE),
    a5.building1 = section_a5 == 1 + section_a5.l,
    a.building1 = section_a == 1 + section_a.l,
    ab_cutoff1 = (section_b0 > 0) & (a.building1 == TRUE),
    ab_cutoff1.1 = (lag(section_b0, 1) > 0) & (a.building1 == TRUE),
    ab_cutoff1.2 = (lag(section_b0, 2) > 0) & (a.building1 == TRUE),
    ab_cutoff1.3 = (lag(section_b0, 3) > 0) & (a.building1 == TRUE),
    a5.buildingn = section_a5 > section_a5.l,
    a.buildingn = section_a > section_a.l,
    ab_cutoffn = (section_b0 > 0) & (a.buildingn == TRUE),
    ab_cutoffn.1 = (lag(section_b0, 1) > 0) & (a.buildingn == TRUE),
    ab_cutoffn.2 = (lag(section_b0, 2) > 0) & (a.buildingn == TRUE),
    ab_cutoffn.3 = (lag(section_b0, 3) > 0) & (a.buildingn == TRUE),
  )

dbWriteTable(db_conn, name = "education_supply", education_supply, temporary = FALSE, overwrite = TRUE)

education_supply %>%
  group_by(year, a5.building, ab_cutoff.2) %>%
  dplyr::count() %>%
  filter(ab_cutoff.2 == TRUE) %>%
  group_by(year) %>%
  mutate(sum_year = sum(n)) %>%
  ungroup() %>%
  mutate(sum_total = sum(n)) %>%
  View()
