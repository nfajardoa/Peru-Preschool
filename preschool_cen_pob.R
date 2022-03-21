######## TOWN/VILLAGE IDENTIFICATION PROCEDURE

codes_census <- tbl(db_conn, "census_raw") %>%
  dplyr::select(ubigeo, cen_pob, cod_cen_pob, id_cen_pob) %>%
  distinct %>%
  collect()

codes_ece <- tbl(db_conn, "ece_raw") %>%
  dplyr::select(cod_mod7) %>%
  distinct %>%
  collect()

codes_2013 <- tbl(db_conn, "siagie2013") %>%
  dplyr::select(cod_mod) %>%
  distinct %>%
  collect()

codes_2014 <- tbl(db_conn, "siagie2014") %>%
  dplyr::select(cod_mod) %>%
  distinct %>%
  collect()

codes_2015 <- tbl(db_conn, "siagie2015") %>%
  dplyr::select(cod_mod) %>%
  distinct %>%
  collect()

codes_siagie <- ls(pattern = "codes_20") %>%
  map(~ rlang::parse_expr(.x) %>% eval()) %>%
  reduce(full_join)

ccpp <- read_excel("LISTADO_DE_CCPP INEI-2017V1.xlsx", skip = 2) %>%
  summarise(across(where(is.character), str_to_lower)) %>%
  rename_with(~ str_to_lower(.x) %>% str_replace_all(" ", "_")) %>%
  rename(cen_pob_inei = nombre_centro_poblado,
         codcp_inei = codccpp_2015) %>%
  dplyr::select(cod_dist, cod_ccpp, cen_pob_inei, codcp_inei)

padron <- read_dta("Padron Unidades Educativas/colegios_codccp_fechas.dta", encoding='latin1') %>%
  mutate(across(where(is.character), ~ str_to_lower(.x) %>% str_trim() %>% if_else(. == "", NA_character_, .)))  

ccpp_padron <- padron %>%
  filter(!is.na(codcp_inei)) %>%
  inner_join(ccpp)

ccpp_padron2 <- padron %>%
  filter(!is.na(codcp_inei)) %>%
  inner_join(ccpp %>% rename(idccpp = codcp_inei))

test <- codes_ece %>% 
  rename(cod_mod = cod_mod7) %>%
  inner_join(ccpp_padron %>% dplyr::select(cod_mod))

test2 <- codes_siagie %>% 
  inner_join(ccpp_padron %>% dplyr::select(cod_mod) %>% mutate(cod_mod = as.numeric(cod_mod)))



