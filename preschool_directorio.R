names <- c("idccpp4", "nomcp", "nregion", "height", "population.2017", "male.2017", "female.2017", "houses.2017", "occupied.2017", "disoccupied.2017")

directorio_ccpp <- list.files(path = "Centros poblados/directorio/", full.names = TRUE) %>%
  map(~ read_excel(.x, skip = 5, col_names = names, na = c("-")) %>%
    drop_na(nomcp) %>%
    mutate(
      across(where(is.character), clean_character),
      ubigeo = stri_detect_regex(nomcp, "^distrito (.)*") %>% if_else(. == FALSE, NA, .),
      ubigeo = if_else(ubigeo == TRUE, idccpp4, NA_character_)
    ) %>%
    fill(ubigeo, .direction = "down") %>%
    filter(str_detect(nomcp, "(^departamento)|(^provincia)|(^distrito)|(region)|(callao)", negate = TRUE)) %>%
    mutate(
      idccpp = paste0(ubigeo, idccpp4) %>% as.integer64(),
      across(!matches("(idccpp)|(nomcp)|(nregion)"), chr2int)
    ) %>%
    replace_na(value = 0, )) %>%
  bind_rows()

directorio_ccpp$nregion <- fct_recode(directorio_ccpp$nregion,
  "1" = "chala",
  "7" = "janca",
  "9" = "omagua",
  "6" = "puna",
  "4" = "quechua",
  "8" = "rupa rupa",
  "5" = "suni",
  "2" = "yunga fluvial",
  "3" = "yunga maritima"
)

saveRDS(directorio_ccpp, 'Centros poblados/directorio.rds')
