##### CCPP Consolidation and Correction
# ccpp_classification <- read_excel("Centros poblados/ListadoCentroPobladosMTC.xlsx", na = c("Sin Reg", "", "NA")) %>%
#   mtc_dictionary()
# 
# directorio_ccpp <- readRDS("Centros poblados/directorio.rds")
# 
# centros_poblados <- st_read("Centros poblados/CP_P.shp") %>%
#   rename_with(str_to_lower) %>%
#   rename(codccpp = codcp) %>%
#   mutate(
#     codccpp = as.integer(codccpp),
#     idccpp = coalesce(cpinei, cpinei2) %>% as.integer64(),
#     ubigeo = as.integer(ubigeo),
#     ubigeo = if_else(is.na(ubigeo), str_sub(idccpp, 1, 6) %>% as.integer(), ubigeo),
#     check.match = correct_ubigeo(idccpp) == ubigeo
#   ) %>%
#   select(ubigeo, codccpp, idccpp, nomcp, check.match) %>%
#   mutate(across(where(is.character), ~ clean_character(.x) %>% str_to_lower()))
# 
# # Correct ubigeos
# correction_ubigeos <- left_join(centros_poblados %>% as_tibble() %>% select(-geometry), directorio_ccpp, by = "idccpp", suffix = c("", ".dir")) %>%
#   drop_na(idccpp) %>%
#   mutate(check.match = correct_ubigeo(idccpp) == ubigeo) %>%
#   filter(check.match == FALSE) %>%
#   select(codccpp, ubigeo.dir, idccpp) %>%
#   rename(ubigeo = ubigeo.dir)
# 
# centros_poblados %<>% left_join(correction_ubigeos, by = 'codccpp', suffix = c('', '.correct')) %>%
#   mutate(
#     ubigeo = coalesce(ubigeo.correct, ubigeo),
#     idccpp = coalesce(idccpp.correct, idccpp),
#     check.match = correct_ubigeo(idccpp) == ubigeo
#   ) %>%
#   select(!matches('correct'))
# 
# wrong.ubigeos <- centros_poblados %>% filter(check.match == FALSE) %>% as_tibble() %>% select(-geometry) %>%
#   left_join(ccpp_classification, by = c('idccpp'), suffix = c('', '.mtc')) %>%
#   drop_na(ubigeo.mtc) %>%
#   select(codccpp, nomcp, ccpp, idccpp, ubigeo.mtc) %>%
#   rename(ubigeo = ubigeo.mtc)
# 
# centros_poblados %<>%
#   left_join(wrong.ubigeos %>% select(codccpp, ubigeo), by = 'codccpp', suffix = c('', '.correct')) %>%
#   mutate(
#     ubigeo = coalesce(ubigeo.correct, ubigeo),
#     check.match = correct_ubigeo(idccpp) == ubigeo,
#     ubigeo = if_else(check.match == FALSE, correct_ubigeo(idccpp), ubigeo, ubigeo)
#   ) %>%
#   select(!matches('(correct)|(check)')) %>%
#   left_join(directorio_ccpp %>% select(-ubigeo), suffix = c('', '1'), by = 'idccpp') %>%
#   left_join(ccpp_classification %>% select(-ubigeo) %>% rename(nomcp = ccpp), suffix = c('', '2'), by = 'idccpp') %>%
#   select(codccpp, ubigeo, idccpp, idccpp4, matches('nomcp'), nregion, capital, rural, category, matches('2015'), matches('2017'), height)
# 
# saveRDS(centros_poblados, 'Centros poblados/centros_poblados.rds')
centros_poblados <- readRDS('Centros poblados/centros_poblados.rds')

# ccpp_families <- tbl(db_conn, 'sisfoh') %>%
#   select(ubigeo, ubigeo_ccpp, cen_pob) %>%
#   distinct() %>%
#   collect() %>%
#   mutate(across(matches('ubigeo'), as.integer64)) %>%
#   rename(idccpp = ubigeo_ccpp, nomcp = cen_pob)
# 
# write_csv(ccpp_families, 'Centros poblados/ccpp_families-sisfoh.csv')
ccpp_families <- read_csv("Centros poblados/ccpp_families-sisfoh.csv") %>%
  mutate(across(where(is.numeric), as.integer64))

## Wrong ubigeos
names_dictionary <- centros_poblados %>% select(codccpp, matches('nomcp')) %>% as_tibble() %>% select(-geometry) %>%
  pivot_longer(matches('nomcp'), values_to = 'nomcp') %>% 
  drop_na(nomcp) %>%
  distinct(codccpp, nomcp)

nomatches <- anti_join(ccpp_families, centros_poblados %>% as_tibble() %>% select(idccpp)) %>%
  anti_join(centros_poblados %>% as_tibble() %>% select(ubigeo, nomcp)) %>%
  drop_na(nomcp) %>%
  stringdist_left_join(names_dictionary, distance_col = 'distance', by = 'nomcp', max_dist = 2) %>%
  left_join(centros_poblados %>% as_tibble() %>% select(-geometry) %>% select(codccpp, ubigeo, idccpp), suffix = c('.sisfoh', ''), by = 'codccpp') %>%
  mutate(
    check.ubigeo = ubigeo.sisfoh == ubigeo,
    check.ubigeo5 = get_ubigeo(ubigeo.sisfoh, 5) == get_ubigeo(ubigeo, 5),
    check.ubigeo4 = get_ubigeo(ubigeo.sisfoh, 4) == get_ubigeo(ubigeo, 4),
    check.ubigeo2 = get_ubigeo(ubigeo.sisfoh, 2) == get_ubigeo(ubigeo, 2),
    check.idccpp = idccpp.sisfoh == idccpp,
    check.idccpp4 = get_idccpp(idccpp.sisfoh) == get_idccpp(idccpp)
  )

na_values <- nomatches %>% filter(is.na(nomcp.y)) %>% select(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x)

## Choose the ones that are not repeated
matches.1 <- nomatches %>%
  group_by(nomcp.x) %>%
  filter(n() == 1, !is.na(codccpp)) %>%
  select(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x, codccpp) %>%
  ungroup() %>%
  distinct() 

## With consistent ubigeo
nomatches.1 <- nomatches %>%
  anti_join(matches.1, by = c("ubigeo.sisfoh", "idccpp.sisfoh", "nomcp.x")) %>%
  group_by(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x) %>%
  arrange(distance) %>%
  filter(distance == min(distance), check.ubigeo == TRUE | is.na(check.ubigeo))

area_by.all <- nomatches.1 %>%
  left_join(centros_poblados %>% select(codccpp)) %>%
  group_by(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x) %>%
  summarise(
    polygon = st_convex_hull(st_union(geometry)) %>% 
      st_buffer(dist = 1000) %>% 
      st_area() %>% 
      units::set_units(km^2)
  )

nomatches.1 %<>% left_join(area_by.all, by = c("ubigeo.sisfoh", "idccpp.sisfoh", "nomcp.x"))

set.seed(59)
matches.2 <- nomatches.1 %>%
  filter(polygon < units::set_units(100, km^2)) %>%
  select(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x, codccpp) %>%
  slice_sample(n = 1) %>%
  distinct()

# With consistent ubigeo till province level
nomatches.2 <- nomatches %>%
  anti_join(matches.1, by = c("ubigeo.sisfoh", "idccpp.sisfoh", "nomcp.x")) %>%
  anti_join(matches.2, by = c("ubigeo.sisfoh", "idccpp.sisfoh", "nomcp.x")) %>%
  group_by(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x) %>%
  arrange(distance) %>%
  filter(distance == min(distance), check.ubigeo5 == TRUE | is.na(check.ubigeo5))

area_by.all.2 <- nomatches.2 %>%
  left_join(centros_poblados %>% select(codccpp)) %>%
  group_by(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x) %>%
  summarise(
    polygon = st_convex_hull(st_union(geometry)) %>% 
      st_buffer(dist = 1000) %>% 
      st_area() %>% 
      units::set_units(km^2)
  )

nomatches.2 %<>% left_join(area_by.all.2, by = c("ubigeo.sisfoh", "idccpp.sisfoh", "nomcp.x"))

set.seed(14)
matches.3 <- nomatches.2 %>%
  filter(polygon < units::set_units(100, km^2)) %>%
  select(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x, codccpp) %>%
  slice_sample(n = 1) %>%
  distinct()

nomatches.3 <- nomatches %>%
  anti_join(matches.1, by = c("ubigeo.sisfoh", "idccpp.sisfoh", "nomcp.x")) %>%
  anti_join(matches.2, by = c("ubigeo.sisfoh", "idccpp.sisfoh", "nomcp.x")) %>%
  anti_join(matches.3, by = c("ubigeo.sisfoh", "idccpp.sisfoh", "nomcp.x")) %>%
  group_by(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x) %>%
  filter(distance == min(distance), check.ubigeo4 == TRUE | is.na(check.ubigeo4))

area_by.all.3 <- nomatches.3 %>%
  left_join(centros_poblados %>% select(codccpp)) %>%
  group_by(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x) %>%
  summarise(
    polygon = st_convex_hull(st_union(geometry)) %>% 
      st_buffer(dist = 1000) %>% 
      st_area() %>% 
      units::set_units(km^2)
  )

nomatches.3 %<>% left_join(area_by.all.3, by = c("ubigeo.sisfoh", "idccpp.sisfoh", "nomcp.x"))

set.seed(36)
matches.4 <- nomatches.3 %>%
  filter(polygon < units::set_units(100, km^2)) %>%
  select(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x, codccpp) %>%
  slice_sample(n = 1) %>%
  distinct()

matches.first <- bind_rows(matches.1, matches.2, matches.3, matches.4) %>%
  rename_with(~ str_remove_all(.x, '(\\.sisfoh)|(\\.x)'))

write_csv(matches.first, 'Centros poblados/correction_ccpp-sisfoh.csv')

rm(list = ls(pattern = '(^area_)|(^matches\\.[0-9]$)|(^nomatches)'))
############# SECOND ITERATION (increase distance)

nomatches <- na_values %>% rename_with(~ str_remove_all(.x, '(\\.sisfoh)|(\\.x)')) %>%
  stringdist_left_join(names_dictionary, distance_col = 'distance', by = 'nomcp', max_dist = 4) %>%
  left_join(centros_poblados %>% as_tibble() %>% select(-geometry) %>% select(codccpp, ubigeo, idccpp), suffix = c('.sisfoh', ''), by = 'codccpp') %>%
  mutate(
    check.ubigeo = ubigeo.sisfoh == ubigeo,
    check.ubigeo5 = get_ubigeo(ubigeo.sisfoh, 5) == get_ubigeo(ubigeo, 5),
    check.ubigeo4 = get_ubigeo(ubigeo.sisfoh, 4) == get_ubigeo(ubigeo, 4),
    check.ubigeo2 = get_ubigeo(ubigeo.sisfoh, 2) == get_ubigeo(ubigeo, 2),
    check.idccpp = idccpp.sisfoh == idccpp,
    check.idccpp4 = get_idccpp(idccpp.sisfoh) == get_idccpp(idccpp)
  )

na_values <- nomatches %>% filter(is.na(nomcp.y)) %>% select(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x)

## Choose the ones that are not repeated
matches.1 <- nomatches %>%
  group_by(nomcp.x) %>%
  filter(n() == 1, !is.na(codccpp)) %>%
  select(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x, codccpp) %>%
  ungroup() %>%
  distinct() 

## With consistent ubigeo
nomatches.1 <- nomatches %>%
  anti_join(matches.1, by = c("ubigeo.sisfoh", "idccpp.sisfoh", "nomcp.x")) %>%
  group_by(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x) %>%
  arrange(distance) %>%
  filter(distance == min(distance), check.ubigeo == TRUE | is.na(check.ubigeo))

area_by.all <- nomatches.1 %>%
  left_join(centros_poblados %>% select(codccpp)) %>%
  group_by(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x) %>%
  summarise(
    polygon = st_convex_hull(st_union(geometry)) %>% 
      st_buffer(dist = 1000) %>% 
      st_area() %>% 
      units::set_units(km^2)
  )

nomatches.1 %<>% left_join(area_by.all, by = c("ubigeo.sisfoh", "idccpp.sisfoh", "nomcp.x"))

set.seed(59)
matches.2 <- nomatches.1 %>%
  filter(polygon < units::set_units(100, km^2)) %>%
  select(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x, codccpp) %>%
  slice_sample(n = 1) %>%
  distinct()

# With consistent ubigeo till province level
nomatches.2 <- nomatches %>%
  anti_join(matches.1, by = c("ubigeo.sisfoh", "idccpp.sisfoh", "nomcp.x")) %>%
  anti_join(matches.2, by = c("ubigeo.sisfoh", "idccpp.sisfoh", "nomcp.x")) %>%
  group_by(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x) %>%
  arrange(distance) %>%
  filter(distance == min(distance), check.ubigeo5 == TRUE | is.na(check.ubigeo5))

area_by.all.2 <- nomatches.2 %>%
  left_join(centros_poblados %>% select(codccpp)) %>%
  group_by(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x) %>%
  summarise(
    polygon = st_convex_hull(st_union(geometry)) %>% 
      st_buffer(dist = 1000) %>% 
      st_area() %>% 
      units::set_units(km^2)
  )

nomatches.2 %<>% left_join(area_by.all.2, by = c("ubigeo.sisfoh", "idccpp.sisfoh", "nomcp.x"))

set.seed(14)
matches.3 <- nomatches.2 %>%
  filter(polygon < units::set_units(100, km^2)) %>%
  select(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x, codccpp) %>%
  slice_sample(n = 1) %>%
  distinct()

nomatches.3 <- nomatches %>%
  anti_join(matches.1, by = c("ubigeo.sisfoh", "idccpp.sisfoh", "nomcp.x")) %>%
  anti_join(matches.2, by = c("ubigeo.sisfoh", "idccpp.sisfoh", "nomcp.x")) %>%
  anti_join(matches.3, by = c("ubigeo.sisfoh", "idccpp.sisfoh", "nomcp.x")) %>%
  group_by(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x) %>%
  filter(distance == min(distance), check.ubigeo4 == TRUE | is.na(check.ubigeo4))

area_by.all.3 <- nomatches.3 %>%
  left_join(centros_poblados %>% select(codccpp)) %>%
  group_by(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x) %>%
  summarise(
    polygon = st_convex_hull(st_union(geometry)) %>% 
      st_buffer(dist = 1000) %>% 
      st_area() %>% 
      units::set_units(km^2)
  )

nomatches.3 %<>% left_join(area_by.all.3, by = c("ubigeo.sisfoh", "idccpp.sisfoh", "nomcp.x"))

set.seed(36)
matches.4 <- nomatches.3 %>%
  filter(polygon < units::set_units(100, km^2)) %>%
  select(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x, codccpp) %>%
  slice_sample(n = 1) %>%
  distinct()

matches.second <- bind_rows(matches.1, matches.2, matches.3, matches.4) %>%
  rename_with(~ str_remove_all(.x, '(\\.sisfoh)|(\\.x)'))

write.table(matches.second, 'Centros poblados/correction_ccpp-sisfoh.csv', sep = ",", append = TRUE, col.names = FALSE, row.names = FALSE)

rm(list = ls(pattern = '(^area_)|(^matches\\.[0-9]$)|(^nomatches)'))
############# THIRD ITERATION (increase distance)
nomatches <- na_values %>% rename_with(~ str_remove_all(.x, '(\\.sisfoh)|(\\.x)')) %>%
  stringdist_left_join(names_dictionary, distance_col = 'distance', by = 'nomcp', max_dist = 12) %>%
  left_join(centros_poblados %>% as_tibble() %>% select(-geometry) %>% select(codccpp, ubigeo, idccpp), suffix = c('.sisfoh', ''), by = 'codccpp') %>%
  mutate(
    check.ubigeo = ubigeo.sisfoh == ubigeo,
    check.ubigeo5 = get_ubigeo(ubigeo.sisfoh, 5) == get_ubigeo(ubigeo, 5),
    check.ubigeo4 = get_ubigeo(ubigeo.sisfoh, 4) == get_ubigeo(ubigeo, 4),
    check.ubigeo2 = get_ubigeo(ubigeo.sisfoh, 2) == get_ubigeo(ubigeo, 2),
    check.idccpp = idccpp.sisfoh == idccpp,
    check.idccpp4 = get_idccpp(idccpp.sisfoh) == get_idccpp(idccpp)
  )

na_values <- nomatches %>% filter(is.na(nomcp.y)) %>% select(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x)

## Choose the ones that are not repeated
matches.1 <- nomatches %>%
  group_by(nomcp.x) %>%
  filter(n() == 1, !is.na(codccpp)) %>%
  select(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x, codccpp) %>%
  ungroup() %>%
  distinct() 

## With consistent ubigeo
nomatches.1 <- nomatches %>%
  anti_join(matches.1, by = c("ubigeo.sisfoh", "idccpp.sisfoh", "nomcp.x")) %>%
  group_by(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x) %>%
  arrange(distance) %>%
  filter(distance == min(distance), check.ubigeo == TRUE | is.na(check.ubigeo))

area_by.all <- nomatches.1 %>%
  left_join(centros_poblados %>% select(codccpp)) %>%
  group_by(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x) %>%
  summarise(
    polygon = st_convex_hull(st_union(geometry)) %>% 
      st_buffer(dist = 1000) %>% 
      st_area() %>% 
      units::set_units(km^2)
  )

nomatches.1 %<>% left_join(area_by.all, by = c("ubigeo.sisfoh", "idccpp.sisfoh", "nomcp.x"))

set.seed(59)
matches.2 <- nomatches.1 %>%
  filter(polygon < units::set_units(100, km^2)) %>%
  select(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x, codccpp) %>%
  slice_sample(n = 1) %>%
  distinct()

# With consistent ubigeo till province level
nomatches.2 <- nomatches %>%
  anti_join(matches.1, by = c("ubigeo.sisfoh", "idccpp.sisfoh", "nomcp.x")) %>%
  anti_join(matches.2, by = c("ubigeo.sisfoh", "idccpp.sisfoh", "nomcp.x")) %>%
  group_by(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x) %>%
  arrange(distance) %>%
  filter(distance == min(distance), check.ubigeo5 == TRUE | is.na(check.ubigeo5))

area_by.all.2 <- nomatches.2 %>%
  left_join(centros_poblados %>% select(codccpp)) %>%
  group_by(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x) %>%
  summarise(
    polygon = st_convex_hull(st_union(geometry)) %>% 
      st_buffer(dist = 1000) %>% 
      st_area() %>% 
      units::set_units(km^2)
  )

nomatches.2 %<>% left_join(area_by.all.2, by = c("ubigeo.sisfoh", "idccpp.sisfoh", "nomcp.x"))

set.seed(14)
matches.3 <- nomatches.2 %>%
  filter(polygon < units::set_units(100, km^2)) %>%
  select(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x, codccpp) %>%
  slice_sample(n = 1) %>%
  distinct()

nomatches.3 <- nomatches %>%
  anti_join(matches.1, by = c("ubigeo.sisfoh", "idccpp.sisfoh", "nomcp.x")) %>%
  anti_join(matches.2, by = c("ubigeo.sisfoh", "idccpp.sisfoh", "nomcp.x")) %>%
  anti_join(matches.3, by = c("ubigeo.sisfoh", "idccpp.sisfoh", "nomcp.x")) %>%
  group_by(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x) %>%
  filter(distance == min(distance), check.ubigeo4 == TRUE | is.na(check.ubigeo4))

area_by.all.3 <- nomatches.3 %>%
  left_join(centros_poblados %>% select(codccpp)) %>%
  group_by(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x) %>%
  summarise(
    polygon = st_convex_hull(st_union(geometry)) %>% 
      st_buffer(dist = 1000) %>% 
      st_area() %>% 
      units::set_units(km^2)
  )

nomatches.3 %<>% left_join(area_by.all.3, by = c("ubigeo.sisfoh", "idccpp.sisfoh", "nomcp.x"))

set.seed(36)
matches.4 <- nomatches.3 %>%
  filter(polygon < units::set_units(100, km^2)) %>%
  select(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x, codccpp) %>%
  slice_sample(n = 1) %>%
  distinct()

matches.third <- bind_rows(matches.1, matches.2, matches.3, matches.4) %>%
  rename_with(~ str_remove_all(.x, '(\\.sisfoh)|(\\.x)'))

write.table(matches.third, 'Centros poblados/correction_ccpp-sisfoh.csv', sep = ",", append = TRUE, col.names = FALSE, row.names = FALSE)

rm(list = ls(pattern = '(^area_)|(^matches\\.[0-9]$)|(^nomatches)'))
############# FOURTH ITERATION (increase distance)

nomatches <- na_values %>% rename_with(~ str_remove_all(.x, '(\\.sisfoh)|(\\.x)')) %>%
  stringdist_left_join(names_dictionary, distance_col = 'distance', by = 'nomcp', max_dist = 24) %>%
  left_join(centros_poblados %>% as_tibble() %>% select(-geometry) %>% select(codccpp, ubigeo, idccpp), suffix = c('.sisfoh', ''), by = 'codccpp') %>%
  mutate(
    check.ubigeo = ubigeo.sisfoh == ubigeo,
    check.ubigeo5 = get_ubigeo(ubigeo.sisfoh, 5) == get_ubigeo(ubigeo, 5),
    check.ubigeo4 = get_ubigeo(ubigeo.sisfoh, 4) == get_ubigeo(ubigeo, 4),
    check.ubigeo2 = get_ubigeo(ubigeo.sisfoh, 2) == get_ubigeo(ubigeo, 2),
    check.idccpp = idccpp.sisfoh == idccpp,
    check.idccpp4 = get_idccpp(idccpp.sisfoh) == get_idccpp(idccpp)
  )

na_values <- nomatches %>% filter(is.na(nomcp.y)) %>% select(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x)

## Choose the ones that are not repeated
matches.1 <- nomatches %>%
  group_by(nomcp.x) %>%
  filter(n() == 1, !is.na(codccpp)) %>%
  select(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x, codccpp) %>%
  ungroup() %>%
  distinct() 

## With consistent ubigeo
nomatches.1 <- nomatches %>%
  anti_join(matches.1, by = c("ubigeo.sisfoh", "idccpp.sisfoh", "nomcp.x")) %>%
  group_by(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x) %>%
  arrange(distance) %>%
  filter(distance == min(distance), check.ubigeo == TRUE | is.na(check.ubigeo))

area_by.all <- nomatches.1 %>%
  left_join(centros_poblados %>% select(codccpp)) %>%
  group_by(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x) %>%
  summarise(
    polygon = st_convex_hull(st_union(geometry)) %>% 
      st_buffer(dist = 1000) %>% 
      st_area() %>% 
      units::set_units(km^2)
  )

nomatches.1 %<>% left_join(area_by.all, by = c("ubigeo.sisfoh", "idccpp.sisfoh", "nomcp.x"))

set.seed(59)
matches.2 <- nomatches.1 %>%
  filter(polygon < units::set_units(100, km^2)) %>%
  select(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x, codccpp) %>%
  slice_sample(n = 1) %>%
  distinct()

# With consistent ubigeo till province level
nomatches.2 <- nomatches %>%
  anti_join(matches.1, by = c("ubigeo.sisfoh", "idccpp.sisfoh", "nomcp.x")) %>%
  anti_join(matches.2, by = c("ubigeo.sisfoh", "idccpp.sisfoh", "nomcp.x")) %>%
  group_by(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x) %>%
  arrange(distance) %>%
  filter(distance == min(distance), check.ubigeo5 == TRUE | is.na(check.ubigeo5))

area_by.all.2 <- nomatches.2 %>%
  left_join(centros_poblados %>% select(codccpp)) %>%
  group_by(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x) %>%
  summarise(
    polygon = st_convex_hull(st_union(geometry)) %>% 
      st_buffer(dist = 1000) %>% 
      st_area() %>% 
      units::set_units(km^2)
  )

nomatches.2 %<>% left_join(area_by.all.2, by = c("ubigeo.sisfoh", "idccpp.sisfoh", "nomcp.x"))

set.seed(14)
matches.3 <- nomatches.2 %>%
  filter(polygon < units::set_units(100, km^2)) %>%
  select(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x, codccpp) %>%
  slice_sample(n = 1) %>%
  distinct()

nomatches.3 <- nomatches %>%
  anti_join(matches.1, by = c("ubigeo.sisfoh", "idccpp.sisfoh", "nomcp.x")) %>%
  anti_join(matches.2, by = c("ubigeo.sisfoh", "idccpp.sisfoh", "nomcp.x")) %>%
  anti_join(matches.3, by = c("ubigeo.sisfoh", "idccpp.sisfoh", "nomcp.x")) %>%
  group_by(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x) %>%
  filter(distance == min(distance), check.ubigeo4 == TRUE | is.na(check.ubigeo4))

area_by.all.3 <- nomatches.3 %>%
  left_join(centros_poblados %>% select(codccpp)) %>%
  group_by(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x) %>%
  summarise(
    polygon = st_convex_hull(st_union(geometry)) %>% 
      st_buffer(dist = 1000) %>% 
      st_area() %>% 
      units::set_units(km^2)
  )

nomatches.3 %<>% left_join(area_by.all.3, by = c("ubigeo.sisfoh", "idccpp.sisfoh", "nomcp.x"))

set.seed(36)
matches.4 <- nomatches.3 %>%
  filter(polygon < units::set_units(100, km^2)) %>%
  select(ubigeo.sisfoh, idccpp.sisfoh, nomcp.x, codccpp) %>%
  slice_sample(n = 1) %>%
  distinct()

matches.fourth <- bind_rows(matches.1, matches.2, matches.3, matches.4) %>%
  rename_with(~ str_remove_all(.x, '(\\.sisfoh)|(\\.x)'))

write.table(matches.fourth, 'Centros poblados/correction_ccpp-sisfoh.csv', sep = ",", append = TRUE, col.names = FALSE, row.names = FALSE)

rm(list = ls(pattern = '(^area_)|(^matches\\.[0-9]$)|(^nomatches)'))
