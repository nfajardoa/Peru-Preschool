
read_n_copy_dta <- function(connection, file, step_len, approx_len, table, extra_steps = 100, return.results = FALSE, ...) {
  steps <- seq(0, to = approx_len + (extra_steps)*step_len, by = step_len)
  results <- imap(steps, function (skip, step) {
    data <- read_dta(file, skip = skip, n_max = step_len, encoding='latin1') %>% extra_dictionary()
    if (nrow(data) != 0) {
      dbWriteTable(connection, table, data, ...)
      if (nrow(data) == step_len) {return(paste("Step:", step, "-- Successfully completed"))}
      if (nrow(data) < step_len) {return(paste("Completed data retrieval in Step:", step))}
    } else {return(paste("No data was retrieved in Step:", step))}
  })
  if (return.results == TRUE) {
    return(results)
  } else {return()}
}

read_n_copy_dta2 <- function(connection, file, step_len, approx_len, 
                             table, table2, ids, keep_vars, extra_steps = 100, 
                             return.results = FALSE, ...) {
  steps <- seq(0, to = approx_len + (extra_steps)*step_len, by = step_len)
  results <- imap(steps, function (skip, step) {
    data <- read_dta(file, skip = skip, n_max = step_len, encoding='latin1') %>% extra_dictionary()
    data2 <- data %>% 
      arrange(rowSums(is.na(.))) %>% 
      distinct(!!!syms(ids), .keep_all = TRUE) %>%
      dplyr::select(all_of(keep_vars))
    data %<>% dplyr::select(-(all_of(keep_vars)))
      if (nrow(data) != 0) {
        dbWriteTable(connection, table2, data2, ...)
        dbWriteTable(connection, table, data, ...)
        if (nrow(data) == step_len) {return(paste("Step:", step, "-- Successfully completed"))}
        if (nrow(data) < step_len) {return(paste("Completed data retrieval in Step:", step))}
      } else {return(paste("No data was retrieved in Step:", step))}
  })
  if (return.results == TRUE) {
    return(results)
  } else {return()}
}

translate_names <- function(object, names, key) {
  names(object) %<>% str_to_lower()
  match_condition <- paste0(names, collapse = "$)|(^") %>% paste0("(^", ., "$)")
  if (names %>% `%in%`(names(object)) %>% any()) {
    names(object) %<>% str_replace_all(match_condition, key)
  }
  return(object)
}

ece_dictionary <- function(database) {
  cod_eva <- attr(database, "database_name") %>%
    str_remove_all("(Nominada.xlsx)|(_)|(ECE/)") %>%
    str_to_lower()
  type <- cod_eva %>%
    str_sub(-2, -1) %>%
    str_to_lower()
  cod_eva %<>% str_sub(1, -3)
  year <- str_extract(cod_eva, "2([0-9]*)")
  cod_eva %<>% str_extract("(ece)|(mc)")
  database %<>%
    translate_names(c("nom_dre", "dre"), "nom_dre") %>%
    translate_names(c("nom_ugel", "ugel"), "nom_ugel") %>%
    translate_names(c("region", "region26", "departamento"), "nom_dept") %>%
    translate_names(c("provincia"), "nom_prov") %>%
    translate_names(c("distrito"), "nom_dist") %>%
    translate_names(c("cen_pob", "ccpp"), "cen_pob") %>%
    translate_names(c("caracteristica", "caracteristica2"), "caracteristica") %>%
    translate_names(c("sexo", "sexo_estu"), "sexo") %>%
    translate_names(c("dni_est", "dni_estu", "n_doc"), "n_doc") %>%
    translate_names(c("dsc_seccion", "seccion"), "seccion") %>%
    translate_names(c("gestion", "gestion2"), "gestion") %>%
    translate_names(c("grupo_3m", "grupo_m", "grupo_ece_2s_2015_m"), "grupo_m") %>%
    translate_names(c("grupo_3c", "grupo_c", "grupo_ece_2s_2015_c", "grupo_l"), "grupo_l") %>%
    translate_names(c("grupo_hge"), "grupo_h") %>%
    translate_names(c("grupo_cta"), "grupo_t") %>%
    translate_names(c("codest", "cor_est", "cod_al"), "cod_est") %>%
    translate_names(c("aj_h", "aj_hge"), "aj_h") %>%
    translate_names(c("aj_cta"), "aj_t") %>%
    translate_names(c("aj_c", "aj_lectura"), "aj_l") %>%
    translate_names(c("aj_m", "aj_matematica"), "aj_m") %>%
    translate_names(c("peso_c", "peso_c_11", "peso_c_mc", "peso_c1", "peso_final_mc_c"), "aj_l") %>%
    translate_names(c("peso_m", "peso_m_11", "peso_m_mc", "peso_m1", "peso_final_mc_m"), "aj_m") %>%
    translate_names(c(
      "cod_eva", "cod_eva07", "cod_eva11", "cod_eva12", "cod_eva13",
      "cod_eva14", "cod_eva15", "cod_eva16"
    ), "cod_eva") %>%
    translate_names(c("medida500_hge"), "m500_h") %>%
    translate_names(c("medida500_cta"), "m500_t") %>%
    translate_names(c(
      "m500_c", "m500_l", "m500_c_08", "m500_c_09", "m500_c_10", "m500_c_11",
      "m500_c_2014", "m500_c_2015", "m500_l", "m500_c_07", "medida500_l"
    ), "m500_l") %>%
    translate_names(c(
      "m500_m", "m500_m_08", "m500_m_09", "m500_m_10", "m500_m_11",
      "m500_m_2014", "m500_m_2015", "m500_m_07", "medida500_m"
    ), "m500_m")
  if (nrow(database) > 0) {
    database %<>% 
      filter(!(is.na(cod_mod7) | str_detect(cod_mod7, "fuente:"))) %>%
      mutate(across(where(is_character), str_to_lower)) %>%
      mutate(across(where(is_character), ~ ifelse(.x == "#null!" | .x == "" | .x == ".", NA, .x))) %>%
      mutate(across(where(is_character), ~ str_replace_all(.x, "[[:punct:]]", ""))) %>%
      mutate(across(where(is_character), ~ stringi::stri_trans_general(.x, 'latin-ascii'))) %>%
      mutate(across(starts_with("m500_") | starts_with("aj_"), as.numeric)) %>%
      mutate(tipo_eva = case_when(all_of(type) == "2p" ~ 1,
                                  all_of(type) == "4p" ~ 2,
                                  all_of(type) == "2s" ~ 3),
             periodo = all_of(year)
             ) %>%
      dummify("sexo", "mujer", "(^m$)|(^mujer)", "(^h$)|(^hombre)", drop_variable = TRUE) %>%
      dummify("area", "rural", "(^rural$)", "(^urban.$)", drop_variable = TRUE) %>%
      dummify("gestion", "estatal", "(^estatal$)", "(^no.estatal)", drop_variable = TRUE) %>%
      dummify("caracteristica", "unidocente", "(^unidocente)", "(^polidocente)", drop_variable = TRUE) %>%
      codify_ecegroup("l") %>%
      codify_ecegroup("m") %>% 
      codify_ecegroup("t") %>% 
      codify_ecegroup("h")
    
    if (!("cod_eva" %>% `%in%`(names(database)))) {
      database %<>% mutate(cod_eva = all_of(cod_eva))
    } else {
      database %<>% mutate(
        cod_eva = 
          case_when(
            str_detect(all_of(cod_eva), "ece") ~ "ece",
            str_detect(all_of(cod_eva), "mc")  ~ "mc")
      )
    }
    
    if ("peso" %>% `%in%`(names(database))) {
      database %<>%
        mutate(
          aj_l = peso,
          aj_m = peso
        ) %>%
        dplyr::select(-peso)
    }
    
    if (c("nombre1", "nombre2") %>% `%in%`(names(database)) %>% all()) {
      database %<>%
        mutate(nombres = if_else(!is.na(nombre2), paste(nombre1, nombre2), nombre1)) %>%
        dplyr::select(-c(nombre1, nombre2))
    }
  }
  return(database)
}

dummify <- function(database, variable, name, true_pattern, false_pattern, drop_variable = FALSE) {
  if (variable %>% `%in%`(names(database))) {
    database %<>%
      mutate(!!sym(name) := case_when(
        str_detect(!!sym(variable), true_pattern) ~ 1,
        str_detect(!!sym(variable), false_pattern) ~ 0)
        )
    if (drop_variable == TRUE) {
      database %<>% dplyr::select(-!!sym(variable))
    }
    return(database)
  } else {
    return(database)
  }
}

codify_ecegroup <- function(database, group) {
  variable <- paste("grupo", group, sep = "_")
  if (variable %>% `%in%`(names(database))) {
    database %<>%
      mutate(!!sym(variable) := case_when(str_detect(!!sym(variable), "(^<.nivel.1)|(^previo.al.inicio)") ~ 0,
                                          str_detect(!!sym(variable), "(^en.inicio)|(^nivel.1)") ~ 1,
                                          str_detect(!!sym(variable), "(^en.proceso)|(^nivel.2)") ~ 2,
                                          str_detect(!!sym(variable), "(^satisfactorio)|(^nivel.3)") ~ 3)
      )
  } else {return(database)}
}

extra_dictionary <- function(database) {
  database %<>%
    translate_names(c("dpto"), "dept") %>%
    translate_names(c("nombre01"), "nombres") %>%
    translate_names(c("apemat01", "apemat"), "materno") %>%
    translate_names(c("apepat01", "apepat"), "paterno") %>%
    translate_names(c("doc01"), "tipo_doc") %>% 
    translate_names(c("ndoc01"), "n_doc") %>% 
    translate_names(c("nombredd"), "nom_dept") %>% 
    translate_names(c("nombredi"), "nom_dist") %>% 
    translate_names(c("nombrepp"), "nom_prov") %>% 
    translate_names(c("cenpob"), "cen_pob") %>% 
    translate_names(c("year"), "periodo") %>%
    translate_names(c("fecnac"), "fnac") %>%
    translate_names(c("unique"), "unico") %>%
    translate_names(c("cod_ccpp"), "cod_cen_pob") %>%
    translate_names(c("idccpp"), "id_cen_pob")
    
  if (nrow(database) > 0) {
    database %<>%
      mutate(across(where(is_character), str_to_lower)) %>%
      mutate(across(where(is_character), ~ ifelse(.x == "" | .x == ".", NA, .x))) %>%
      mutate(across(where(is_character), ~ str_replace_all(.x, "[[:punct:]]", ""))) %>%
      mutate(across(where(is_character), ~ stringi::stri_trans_general(.x, 'latin-ascii')))
    if (names(database) %>% length() %>% '=='(24)) {
      database %<>% 
        dplyr::select(-c(grupo_unico, sit_mat, sit_final))
      
      if ("sexo" %>% `%in%`(names(database))) {
        database %<>%
          mutate(mujer = case_when(sexo == 2 ~ 0,
                                   sexo == 1 ~ 1)
                 ) %>%
          dplyr::select(-sexo)
      }
      if ("fnac" %>% `%in%`(names(database))) {
        database %<>%
          mutate(fnac_dia = day(fnac),
                 fnac_mes = month(fnac),
                 fnac_anio = year(fnac)
          )
      }
      database %<>% zap_missing() %>% zap_label() %>% zap_labels()
    } else if (names(database) %>% length() %>% '=='(195)) {
      
      database %<>% 
        dummify("gesta01", "gestacion", "(^si$)", "(^no)|(2)", drop_variable = TRUE) %>%
        dummify("consistente", "consistente", "(^si$)", "(^no)") %>%
        dplyr::select(-fechanac)
        
      if ("sexo" %>% `%in%`(names(database))) {
        database %<>%
          mutate(mujer = case_when(sexo == 1 ~ 0,
                                   sexo == 2 ~ 1)
          ) %>%
          dplyr::select(-sexo)
      }
      if ("sisfoh_cse" %>% `%in%`(names(database))) {
        database %<>%
          mutate(sisfoh_cse = case_when(str_detect(sisfoh_cse, "01: pobre extremo") ~ 1,
                                        str_detect(sisfoh_cse, "02: pobre no extremo") ~ 2,
                                        str_detect(sisfoh_cse, "03: no pobre") ~ 3)
          )
      }
      if ("estado_identidad" %>% `%in%`(names(database))) {
        database %<>%
          mutate(estado_identidad = case_when(
            str_detect(estado_identidad, "01: identificado") ~ 1,
            str_detect(estado_identidad, "02: recuperada por imputacion") ~ 2,
            str_detect(estado_identidad, "03: identificado pero duplicado") ~ 3,
            str_detect(estado_identidad, "04: no identificado") ~ 4)
          )
      }
      if (c("fnac_dia", "fnac_mes", "fnac_anio") %>% `%in%`(names(database)) %>% all()) {
        database %<>%
          mutate(fnac = as_date(paste(fnac_anio, fnac_mes, fnac_dia, sep = "-")))
      }
      database %<>% zap_missing() %>% zap_label() %>% zap_labels()
    }
  }
  return(database)
}

read_with_name_excel <- function(file, ...) {
  table <- read_excel(file, ...)
  attr(table, 'database_name') <- file
  return(table)
}

read_n_copy_excel <- function(connection, path, table, return.results = FALSE, ...) {
  files <- list.files(path = path) %>% str_subset("(.xlsx)|(.xls)") %>% paste0(path, .)
  results <- imap(files, function (file, step) {
    data <- read_with_name_excel(file) %>% ece_dictionary()
    if (nrow(data) != 0) {
      dbWriteTable(connection, table, data, ...)
      return(paste("Step:", step, "-- Successfully completed"))
    } else {return(paste("No data was retrieved in Step:", step))}
  })
  if (return.results == TRUE) {
    return(results)
  } else {return()}
}

codmod_2_codmod7 <- function(cod_mod) {
  zeros <- 7 - (stringi::stri_length(cod_mod))
  rep(0, zeros) %>% paste(collapse = "") %>% paste0(., cod_mod)
}

decide_drops <- function(...) {
  values <- list(...)[[1]]
  if (values %>% is.na() %>% all()) {return(NA)}
  condition <- unique(values) %>% length() %>%'=='(1)
  value <- head(..., 1)
  ifelse(condition, value, NA)
}
