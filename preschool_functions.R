
read_n_copy_dta <- function(connection, file, step_len, approx_len, table,
                            select_vars = NULL, extra_steps = 100, return.results = FALSE, ...) {
  # Set steps for reading
  steps <- seq(0, to = approx_len + (extra_steps) * step_len, by = step_len)

  results <- imap(steps, function(skip, step) {
    data <- read_dta(file, skip = skip, n_max = step_len, encoding = "latin1") %>%
      extra_dictionary(table = table, select_vars = select_vars)

    if (nrow(data) != 0) {
      dbWriteTable(connection, table, data, ...)
      if (nrow(data) == step_len) {
        return(paste("Step:", step, "-- Successfully completed"))
      }
      if (nrow(data) < step_len) {
        return(paste("Completed data retrieval in Step:", step))
      }
    } else {
      return(paste("No data was retrieved in Step:", step))
    }
  })

  if (return.results == TRUE) {
    return(results)
  } else {
    return()
  }
}

translate_names <- function(x) {
  names_dictionary <- tribble(
    ~definitions, ~name,
    list("dpto"), "dept",
    list("nombre01"), "nombres",
    list("apemat01", "apemat"), "materno",
    list("apepat01", "apepat"), "paterno",
    list("doc01"), "tipo_doc",
    list("dni_est", "dni_estu", "n_doc", "ndoc01"), "ndoc",
    list("nombredd"), "nom_dept",
    list("nombredi"), "nom_dist",
    list("nombrepp"), "nom_prov",
    list("cenpob"), "cen_pob",
    list("year"), "periodo",
    list("fecnac"), "fnac",
    list("unique"), "unico",
    list("cod_cen_pob"), "cod_ccpp",
    list("codgeo"), "ubigeo",
    list("idccpp"), "ubigeo_ccpp",
    list("nom_dre", "dre"), "nom_dre",
    list("nom_ugel", "ugel"), "nom_ugel",
    list("region", "region26", "departamento"), "nom_dept",
    list("provincia"), "nom_prov",
    list("distrito"), "nom_dist",
    list("cod_mod7"), "cod_mod",
    list("cen_pob", "ccpp"), "cen_pob",
    list("nucl01"), "nucleo",
    list("caracteristica", "caracteristica2"), "caracteristica",
    list("sexo", "sexo_estu"), "sexo",
    list("dsc_seccion", "seccion"), "seccion",
    list("gestion", "gestion2"), "gestion",
    list("grupo_3m", "grupo_m", "grupo_ece_2s_2015_m"), "grupo_m",
    list("grupo_3c", "grupo_c", "grupo_ece_2s_2015_c", "grupo_l"), "grupo_l",
    list("grupo_hge"), "grupo_h",
    list("grupo_cta"), "grupo_t",
    list("codest", "cor_est", "cod_al"), "cod_est",
    list("aj_h", "aj_hge"), "aj_h",
    list("aj_cta"), "aj_t",
    list("aj_c", "aj_lectura"), "aj_l",
    list("aj_m", "aj_matematica"), "aj_m",
    list("peso_c", "peso_c_11", "peso_c_mc", "peso_c1", "peso_final_mc_c"), "aj_l",
    list("peso_m", "peso_m_11", "peso_m_mc", "peso_m1", "peso_final_mc_m"), "aj_m",
    list(
      "cod_eva", "cod_eva07", "cod_eva11", "cod_eva12", "cod_eva13",
      "cod_eva14", "cod_eva15", "cod_eva16"
    ), "mc",
    list("medida500_hge"), "m500_h",
    list("medida500_cta"), "m500_t",
    list(
      "m500_c", "m500_l", "m500_c_08", "m500_c_09", "m500_c_10", "m500_c_11",
      "m500_c_2014", "m500_c_2015", "m500_l", "m500_c_07", "medida500_l"
    ), "m500_l",
    list(
      "m500_m", "m500_m_08", "m500_m_09", "m500_m_10", "m500_m_11", "m500_m_2014",
      "m500_m_2015", "m500_m_07", "medida500_m"
    ), "m500_m"
  ) %>%
    unnest(definitions) %>%
    mutate(definitions = as.character(definitions))

  dictionary <- setNames(names_dictionary$name, paste0("(?i)\\b", names_dictionary$definitions, "\\b"))

  x %<>% str_to_lower() %>%
    str_squish() %>%
    str_replace_all(dictionary)
  return(x)
}

ece_dictionary <- function(database) {
  attr(database, "database_name") %>%
    str_remove_all("(Nominada.xlsx)|(_)|(ECE/)") %>%
    str_to_lower() %T>%
    {
      assign("type", value = str_sub(., -2, -1) %>% str_to_lower(), envir = parent.env(environment()))
    } %>%
    str_sub(1, -3) %T>%
    {
      assign("year", value = str_extract(., "2([0-9]*)") %>% as.integer(), envir = parent.env(environment()))
    } %>%
    str_extract("(ece)|(mc)")

  matching_pattern <- "(^nom_)|(^ie$)|(cod_dre)|(cod_ugel)|(cen_pob)"

  database %<>%
    rename_with(translate_names) %>%
    dplyr::select(!matches(matching_pattern))

  if (nrow(database) > 0) {
    database %<>%
      dplyr::filter(!(if_any(1 | matches("(cod_mod)|(mc)"), ~ str_detect(., "(Fuente)|(fuente)")) | is.na(cod_mod))) %>%
      mutate(across(matches("(cod_mod)|(anexo)|(ndoc)|(cod_est)|(ubigeo)"), chr2int)) %>%
      mutate(across(starts_with("m500_") | starts_with("aj_"), as.numeric)) %>%
      mutate(across(where(is.character), clean_character)) %>%
      mutate(
        tipo_eva = case_when(
          all_of(type) == "2p" ~ 1L,
          all_of(type) == "4p" ~ 2L,
          all_of(type) == "2s" ~ 3L
        ),
        periodo = all_of(year),
      ) %>%
      dummify("sexo", "mujer", "(^m$)|(^mujer)", "(^h$)|(^hombre)", drop_variable = TRUE) %>%
      dummify("area", "rural", "(^rural$)", "(^urban.$)", drop_variable = TRUE) %>%
      dummify("gestion", "estatal", "(^estatal$)", "(^no.estatal)", drop_variable = TRUE) %>%
      dummify("caracteristica", "unidocente", "(^unidocente)", "(^polidocente)", drop_variable = TRUE) %>%
      codify_ecegroup("l") %>%
      codify_ecegroup("m") %>%
      codify_ecegroup("t") %>%
      codify_ecegroup("h")

    if (!("mc" %>% `%in%`(names(database)))) {
      database %<>% mutate(mc = FALSE)
    } else {
      database %<>% mutate(
        mc =
          case_when(
            str_detect(all_of(mc), "ece") ~ FALSE,
            str_detect(all_of(mc), "mc") ~ TRUE
          )
      )
    }

    if ("peso" %>% `%in%`(names(database))) {
      database %<>%
        mutate(
          aj_l = peso %>% as.numeric(),
          aj_m = peso %>% as.numeric()
        ) %>%
        dplyr::select(-peso)
    }

    if (c("nombre1", "nombre2") %>% `%in%`(names(database)) %>% all()) {
      database %<>%
        mutate(nombres = if_else(!is.na(nombre2), paste(nombre1, nombre2), nombre1)) %>%
        dplyr::select(-c(nombre1, nombre2))
    }

    if (c("seccion", "id_seccion") %>% `%in%`(names(database)) %>% all()) {
      database %<>%
        mutate(
          seccion = case_when(
            periodo %in% c(2016, 2018) ~ id_seccion,
            TRUE ~ if_else(seccion %in% c("0", "1"), "01", seccion)
          )
        ) %>%
        dplyr::select(-c(id_seccion))
    }
  }

  return(database)
}

dummify <- function(database, variable, name, true_pattern, false_pattern, drop_variable = FALSE, variable_nse = FALSE) {
  if (variable %>% `%in%`(names(database))) {
    database %<>%
      mutate(!!sym(name) := case_when(
        str_detect(!!sym(variable), true_pattern) ~ TRUE,
        str_detect(!!sym(variable), false_pattern) ~ FALSE
      ))
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
      mutate(!!sym(variable) := case_when(
        str_detect(!!sym(variable), "(^<.nivel.1)|(^previo.al.inicio)") ~ 0L,
        str_detect(!!sym(variable), "(^en.inicio)|(^nivel.1)") ~ 1L,
        str_detect(!!sym(variable), "(^en.proceso)|(^nivel.2)") ~ 2L,
        str_detect(!!sym(variable), "(^satisfactorio)|(^nivel.3)") ~ 3L
      ))
  } else {
    return(database)
  }
}

extra_dictionary <- function(database, table = NULL, select_vars = NULL) {
  if (!is.null(select_vars)) {
    database %<>%
      rename_with(translate_names) %>%
      dplyr::select(all_of(select_vars)) %>%
      mutate(across(matches("fnac"), as_date)) %>%
      mutate(across(matches("seccion"), as.character)) %>%
      mutate(across(where(is.character), clean_character)) %>%
      mutate(across(matches("(ndoc)|(numero)"), chr2int)) %>%
      mutate(across(starts_with("id_persona"), as.integer64)) %>%
      mutate(across(where(is.double) & !(where(is.Date)), as.integer))
  } else {
    database %<>%
      rename_with(translate_names) %>%
      mutate(across(matches("fnac"), as_date)) %>%
      mutate(across(matches("seccion"), as.character)) %>%
      mutate(across(where(is.character), clean_character)) %>%
      mutate(across(matches("(ndoc)|(numero)"), chr2int)) %>%
      mutate(across(starts_with("id_persona"), as.integer64)) %>%
      mutate(across(where(is.double) & !(where(is.Date)), as.integer))
  }

  if (table %>% str_detect("siagie")) { # Siagie

    database %<>% dplyr::select(-c(grupo_unico, sit_mat, sit_final))

    if ("sexo" %>% `%in%`(names(database))) {
      database %<>%
        mutate(mujer = case_when(
          sexo == 2 ~ FALSE,
          sexo == 1 ~ TRUE
        )) %>%
        dplyr::select(-sexo)
    }

    if ("unico" %>% `%in%`(names(database))) {
      database %<>%
        mutate(unico = case_when(
          unico == 0 ~ FALSE,
          unico == 1 ~ TRUE
        ))
    }

    database %<>%
      zap_missing() %>%
      zap_label() %>%
      zap_labels()
  } else if (table %>% str_detect("sisfoh")) { # Sisfoh

    database %<>%
      dummify("gesta01", "gestacion", "(^si$)", "(^no)|(2)", drop_variable = TRUE) %>%
      dummify("consistente", "consistente", "(^si$)", "(^no)")

    if ("aniofin" %>% `%in%`(names(database))) {
      database %<>%
        rename(periodo = aniofin) %>%
        filter(periodo %in% c(2012, 2013))
    }

    if ("sexo" %>% `%in%`(names(database))) {
      database %<>%
        mutate(mujer = case_when(
          sexo == 1 ~ FALSE,
          sexo == 2 ~ TRUE
        )) %>%
        dplyr::select(-sexo)
    }

    if ("sisfoh_cse" %>% `%in%`(names(database))) {
      database %<>%
        mutate(
          sisfoh_cse = case_when(
            str_detect(sisfoh_cse, "01: pobre extremo") ~ 1L,
            str_detect(sisfoh_cse, "02: pobre no extremo") ~ 2L,
            str_detect(sisfoh_cse, "03: no pobre") ~ 3L
          )
        )
    }

    if ("estado_identidad" %>% `%in%`(names(database))) {
      database %<>%
        mutate(
          estado_identidad = case_when(
            str_detect(estado_identidad, "01: identificado") ~ 1L,
            str_detect(estado_identidad, "02: recuperada por imputacion") ~ 2L,
            str_detect(estado_identidad, "03: identificado pero duplicado") ~ 3L,
            str_detect(estado_identidad, "04: no identificado") ~ 4L
          )
        )
    }

    if (c("fnac_dia", "fnac_mes", "fnac_anio") %>% `%in%`(names(database)) %>% all()) {
      database %<>%
        mutate(
          fnac_mes = case_when(
            fnac_mes > 12 ~ 12L,
            fnac_mes < 1 ~ 1L,
            TRUE ~ as.integer(fnac_mes)
          ),
          fnac_dia = case_when(
            fnac_dia > 31 ~ 31L,
            fnac_dia < 1 ~ 1L,
            TRUE ~ as.integer(fnac_dia)
          ),
          fnac = paste(fnac_anio, fnac_mes, fnac_dia, sep = "-") %>% filter_date()
        ) %>%
        dplyr::select(-fnac_anio, -fnac_mes, -fnac_dia)
    }

    database %<>%
      zap_missing() %>%
      zap_label() %>%
      zap_labels()
  }

  return(database)
}

eceform_dictionary <- function(database) {
  database %<>%
    mutate(
      across(c(cod_mod, anexo, cod_est), as.integer),
      across(where(is.character), clean_character),
      across(matches("(^ise_utilities)|(^ise_house)|(education)|(exp_education)|(^relocated)|(^guardian)|(^age\\.)|(^preschool)|(^language)"), translate_variables),
      across(starts_with("n."), ~ str_replace_all(.x, "mas de 15", "16") %>% chr2int()),
    ) %>%
    dummify("guardian.sex", "guardian.female", "(^m$)|(^mujer)", "(^h$)|(^hombre)", drop_variable = TRUE) %>%
    binarize("ise_utilities.water", true = "^cano dentro de casa$") %>%
    binarize("ise_utilities.sewage", true = "^bano propio con desague$") %>%
    binarize("ise_utilities.light", true = "^electricidad$") %>%
    binarize("noise_utilities.kitchen", true = "(^gas$)|(^electricidad$)") %>%
    binarize("(^b-)|(ise_assets)|(ise_other)", true = "si", false = "no") %>%
    orderize("(^t-)", scale = "time") %>%
    orderize("(^-1t-)", scale = "time_reversed") %>%
    orderize("(^aa-)", scale = "agreement_augmented") %>%
    orderize("(^-1aa-)", scale = "agreement_augmented_reversed") %>%
    orderize("(^a-)", scale = "agreement") %>%
    orderize("(^i-)", scale = "importance") %>%
    orderize("(^r-)", scale = "rank")

  if ("guardian" %>% `%in%`(names(database))) {
    database %<>%
      mutate(
        guardian = case_when(
          str_detect(guardian, "^mama$") ~ 1L,
          str_detect(guardian, "^papa$") ~ 2L,
          str_detect(guardian, "^abueloa$") ~ 3L,
          str_detect(guardian, "^hermanoa$") ~ 4L,
          str_detect(guardian, "(^otro familiar$)|(^no familiar$)") ~ 5L,
          is.na(guardian) ~ NA_integer_
          # TRUE ~ guardian
        )
      )
  }

  if (grepl("^age", names(database)) %>% any()) {
    database %<>%
      mutate(across(starts_with("age"), ~ case_when(
        str_detect(., "(^menor o igual a 25 anos$)|(menos de 12 anos$)") ~ 1L,
        str_detect(., "(^de 26 a 35 anos$)|(12 anos$)") ~ 2L,
        str_detect(., "(^de 36 a 45 anos$)|(13 anos$)") ~ 3L,
        str_detect(., "(^de 46 a 55 anos$)|(14 anos$)") ~ 4L,
        str_detect(., "(^de 56 a 65 anos$)|(15 anos a mas$)") ~ 5L,
        str_detect(., "^mas de 65 anos$") ~ 6L,
        is.na(.) ~ NA_integer_
        # TRUE ~ .
      )))
  }

  if (grepl("language", names(database)) %>% any()) {
    database %<>%
      mutate(across(starts_with("language"), ~ case_when(
        str_detect(., "^castellano$") ~ 1L,
        str_detect(., "^quechua$") ~ 2L,
        str_detect(., "^aimara$") ~ 3L,
        str_detect(., "(^lengua originaria$)|(^4$)") ~ 4L,
        str_detect(., "(^lengua extranjera$)|(^5$)") ~ 5L,
        is.na(.) ~ NA_integer_
        # TRUE ~ .
      )))
  }

  if (grepl("(education)|(exp_education)", names(database)) %>% any()) {
    database %<>%
      mutate(across(matches("(education)|(exp_education)"), ~ case_when(
        str_detect(., "^sin estudios$") ~ 0,
        str_detect(., "^primaria incompleta$") ~ 3,
        str_detect(., "^primaria completa$") ~ 6,
        str_detect(., "^secundaria incompleta$") ~ 8.5,
        str_detect(., "^secundaria completa$") ~ 11,
        str_detect(., "^educacion ocupacional incompleta$") ~ 7.5,
        str_detect(., "^educacion ocupacional completa$") ~ 9,
        str_detect(., "^superior no universitaria incompleta$") ~ 12.5,
        str_detect(., "^superior no universitaria completa$") ~ 14,
        str_detect(., "^superior universitaria incompleta$") ~ 13.5,
        str_detect(., "^superior universitaria completa$") ~ 16,
        str_detect(., "^posgrado$") ~ 19,
        is.na(.) ~ NA_real_
        # TRUE ~ .
      )))
  }

  if ("income" %>% `%in%`(names(database))) {
    database %<>%
      mutate(
        income = case_when(
          str_detect(income, "^menos de 200 soles$") ~ 1L,
          str_detect(income, "^de 201 a 500 soles$") ~ 2L,
          str_detect(income, "^de 501 a 800 soles$") ~ 3L,
          str_detect(income, "^de 801 a 1200 soles$") ~ 4L,
          str_detect(income, "^de 1201 a 2000 soles$") ~ 5L,
          str_detect(income, "^de 2001 a 5000 soles$") ~ 6L,
          str_detect(income, "^de 5001 a 8000 soles$") ~ 7L,
          str_detect(income, "^mas de 8000 soles$") ~ 8L,
          is.na(income) ~ NA_integer_
          # TRUE ~ income
        )
      )
  }

  if ("ise_house.wall" %>% `%in%`(names(database))) {
    database %<>%
      mutate(
        ise_house.wall = case_when(
          str_detect(ise_house.wall, "^ladrillo o bloque de cemento$") ~ 5L,
          str_detect(ise_house.wall, "^piedra sillar con cal o cemento$") ~ 2L,
          str_detect(ise_house.wall, "^adobe o tapia$") ~ 4L,
          str_detect(ise_house.wall, "^quincha$") ~ 2L,
          str_detect(ise_house.wall, "^piedra con barro$") ~ 2L,
          str_detect(ise_house.wall, "^madera o tablas$") ~ 3L,
          str_detect(ise_house.wall, "^esteras$") ~ 1L,
          str_detect(ise_house.wall, "^otro$") ~ 1L,
          is.na(ise_house.wall) ~ NA_integer_,
          # TRUE ~ ise_house.wall
        )
      )
  }

  if ("ise_house.floor" %>% `%in%`(names(database))) {
    database %<>%
      mutate(
        ise_house.floor = case_when(
          str_detect(ise_house.floor, "^parquet$") ~ 6L,
          str_detect(ise_house.floor, "^pisos asfalticos$") ~ 5L,
          str_detect(ise_house.floor, "^losetas$") ~ 4L,
          str_detect(ise_house.floor, "^entablado$") ~ 3L,
          str_detect(ise_house.floor, "^cemento$") ~ 2L,
          str_detect(ise_house.floor, "^tierra$") ~ 1L,
          str_detect(ise_house.floor, "^otro$") ~ 1L,
          is.na(ise_house.floor) ~ NA_integer_,
          # TRUE ~ ise_house.floor
        )
      )
  }

  if ("ise_house.roof" %>% `%in%`(names(database))) {
    database %<>%
      mutate(
        ise_house.roof = case_when(
          str_detect(ise_house.roof, "^concreto armado$") ~ 7L,
          str_detect(ise_house.roof, "^madera$") ~ 5L,
          str_detect(ise_house.roof, "^tejas$") ~ 6L,
          str_detect(ise_house.roof, "^planchas$") ~ 4L,
          str_detect(ise_house.roof, "^cana o estera con barro$") ~ 3L,
          str_detect(ise_house.roof, "^esteras$") ~ 2L,
          str_detect(ise_house.roof, "^paja u hojas de palmera$") ~ 4L,
          str_detect(ise_house.roof, "^otro$") ~ 1L,
          is.na(ise_house.roof) ~ NA_integer_,
          # TRUE ~ ise_house.roof
        )
      )
  }

  if (grepl("use", names(database)) %>% any()) {
    database %<>%
      mutate(across(starts_with("use"), ~ case_when(
        str_detect(., "^no tienen este vehiculo$") ~ 0L,
        str_detect(., "^si es de uso familiar$") ~ 1L,
        str_detect(., "^si es para trabajar$") ~ 2L,
        is.na(.) ~ NA_integer_
        # TRUE ~ .
      )))
  }

  if (grepl("retention\\.", names(database)) %>% any()) {
    database %<>%
      mutate(across(starts_with("retention."), ~ case_when(
        str_detect(., "^no ha repetido$") ~ 0L,
        str_detect(., "^ha repetido 1 vez$") ~ 1L,
        str_detect(., "^ha repetido 2 veces$") ~ 2L,
        str_detect(., "^ha repetido 3 veces$") ~ 3L,
        is.na(.) ~ NA_integer_
        # TRUE ~ .
      )))
  }

  if ("preschool" %>% `%in%`(names(database))) {
    database %<>%
      mutate(
        preschool = case_when(
          str_detect(preschool, "^asistio a un pronoei$") ~ 3L,
          str_detect(preschool, "^nido jardin o cei$") ~ 2L,
          str_detect(preschool, "^si$") ~ 1L,
          str_detect(preschool, "^no$") ~ 0L,
          str_detect(preschool, "^no se$") | is.na(preschool) ~ NA_integer_,
          # TRUE ~ preschool
        )
      )
  }

  if ("preschool.time" %>% `%in%`(names(database))) {
    database %<>%
      mutate(
        preschool.time = case_when(
          str_detect(preschool.time, "^no$") ~ 0L,
          str_detect(preschool.time, "(^menos de 1 ano$)|(^menos de un ano$)") ~ 0L,
          str_detect(preschool.time, "^1 ano$") ~ 1L,
          str_detect(preschool.time, "^2 anos$") ~ 2L,
          str_detect(preschool.time, "^3 anos$") ~ 3L,
          is.na(preschool.time) ~ NA_integer_,
          # TRUE ~ preschool.time
        )
      )
  }

  if ("relocated" %>% `%in%`(names(database))) {
    database %<>%
      mutate(
        relocated = case_when(
          str_detect(relocated, "^publica$") ~ 1L,
          str_detect(relocated, "^privada$") ~ 2L,
          str_detect(relocated, "^no$") ~ 0L,
          is.na(relocated) ~ NA_integer_,
          # TRUE ~ relocated
        )
      )
  }

  if ("relocate.motive" %>% `%in%`(names(database))) {
    database %<>%
      mutate(
        relocate.motive = case_when(
          str_detect(relocate.motive, "^porque la ensenanza no es buena$") ~ 1L,
          str_detect(relocate.motive, "^porque los companeros de mi hijoa no lo tratan bien$") ~ 2L,
          str_detect(relocate.motive, "^porque los docentes no tratan bien a mi hijoa$") ~ 3L,
          str_detect(relocate.motive, "^porque no cuenta con los materiales necesarios para ensenar a mi hijoa$") ~ 4L,
          str_detect(relocate.motive, "^porque tiene una infraestructura inadecuada$") ~ 5L,
          is.na(relocate.motive) ~ NA_integer_,
          # TRUE ~ relocate.motive
        )
      )
  }

  if ("books.n" %>% `%in%`(names(database))) {
    database %<>%
      mutate(
        books.n = as.character(books.n),
        books.n = case_when(
          str_detect(books.n, "(^ninguno$)|(^1$)") ~ 0L,
          str_detect(books.n, "(^entre 1 y 5 libros$)|(^2$)") ~ 1L,
          str_detect(books.n, "(^entre 6 y 10 libros$)|(^3$)") ~ 2L,
          str_detect(books.n, "(^entre 11 y 20 libros$)|(^4$)") ~ 3L,
          str_detect(books.n, "(^entre 21 y 50 libros$)|(^5$)") ~ 4L,
          str_detect(books.n, "(^entre 51 y 100 libros$)|(^6$)") ~ 5L,
          str_detect(books.n, "(^mas de 100 libros$)|(^7$)") ~ 6L,
          is.na(books.n) ~ NA_integer_,
          # TRUE ~ books.n
        )
      )
  }

  if ("school.location" %>% `%in%`(names(database))) {
    database %<>%
      mutate(
        school.location = case_when(
          str_detect(school.location, "^dentro del centro poblado$") ~ 1L,
          str_detect(school.location, "^fuera del centro poblado pero dentro del distrito$") ~ 2L,
          str_detect(school.location, "^fuera del distrito$") ~ 3L,
          is.na(school.location) ~ NA_integer_,
          # TRUE ~ school.location
        )
      )
  }

  if ("planlector" %>% `%in%`(names(database))) {
    database %<>%
      mutate(
        planlector = case_when(
          str_detect(planlector, "^no lo recuerdo$") ~ -1L,
          str_detect(planlector, "^no me han informado$") ~ 0L,
          str_detect(planlector, "^si me han informado$") ~ 1L,
          is.na(planlector) ~ NA_integer_,
          # TRUE ~ planlector
        )
      )
  }

  if (grepl("school.distance", names(database)) %>% any()) {
    database %<>%
      mutate(
        across(matches("school.distance"), ~ case_when(
          str_detect(., "^menos de 15 minutos$") ~ 7.5,
          str_detect(., "^entre 5 y 15 minutos$") ~ 10,
          str_detect(., "(^entre 15 y 30 minutos$)|(^entre 16 y 30 minutos$)") ~ 22.5,
          str_detect(., "(^entre 30 minutos y 1 hora$)|(^entre 31 minutos y 1 hora$)") ~ 45,
          str_detect(., "^entre 1 y 2 horas$") ~ 90,
          str_detect(., "(^entre 1 a 3 horas$)|(^mas de 2 horas$)") ~ 120,
          str_detect(., "^entre 3 a 6 horas$") ~ 270,
          str_detect(., "^mas de 6 horas$") ~ 360,
          is.na(.) ~ NA_real_,
          # TRUE ~ .
        ))
      )
  }

  if (grepl("school.transport", names(database)) %>% any()) {
    database %<>%
      mutate(
        across(matches("school.transport"), ~ case_when(
          str_detect(., "(^a pie$)|(^animal de carga$)") ~ 5L,
          str_detect(., "(^auto o taxi$)|(^lancha o peque peque$)|(^moto o mototaxi$)|(^moto$)") ~ 40L,
          str_detect(., "^bicicleta$") ~ 15L,
          str_detect(., "(^bus o custer$)|(^camion$)|(^colectivo$)|(^combi o bus$)|(^transporte fluvial$)") ~ 30L,
          str_detect(., "^otro$") | is.na(.) ~ NA_integer_,
          # TRUE ~ .
        ))
      )
  }

  database %<>% rename_with(~ str_remove_all(.x, "(^b-)|(^t-)|(^-1t-)|(^aa-)|(^-1aa-)|(^a-)|(^i-)|(^r-)"))
  return(database)
}

merge_factors <- function(variable, name, title = "cuestionario") {
  if (!name %in% c("cod_mod7", "anexo", "cod_est", "seccion", "ise", "asis_dia2_cues", "ind", "periodo", "tipo_eva")) {
    dir <- getwd()
    dir.create(paste0(dir, "/dictionary"), showWarnings = FALSE)
    setwd(paste0(dir, "/dictionary"))
    variable %<>% as.factor()
    dictionary <- tibble(levels = unique(variable), value = as.numeric(unique(variable)))
    save(dictionary, file = paste0(title, "_", name, ".RData"))
    variable %<>% as.numeric()
    setwd(dir)
  }
  return(variable)
}

consolidate_dictionaries <- function() {
  dir <- getwd()
  setwd(paste0(dir, "/dictionary"))
  result <- list.files() %>% map_dfr(load_dictionary)
  setwd("..")
  return(result)
}

load_dictionary <- function(file) {
  load(file)
  dictionary %>% add_column(.before = 1, variable = file %>% str_remove_all("(cuestionario_)|(.RData)"))
}

read_with_name_excel <- function(file, ...) {
  table <- read_excel(file, ...)
  attr(table, "database_name") <- file
  return(table)
}

read_with_name_dbf <- function(file, variable, ...) {
  print(file)

  table <- read.dbf(file, ...) %>%
    as_tibble() %>%
    rename_with(str_to_lower) %>%
    rename_with(str_replace_all, pattern = "(nom_cp)|(nomcp)", replacement = "cen_pob") %>%
    rename_with(str_replace_all, pattern = "area$", replacement = "area_med")

  if ("codcpsig" %in% names(table)) {
    table %<>%
      select(-codccpp) %>%
      rename_with(str_replace_all, pattern = "(codcpsig)|(codcp_med)|(cod_cp)", replacement = "codccpp")
  } else {
    table %<>%
      rename_with(str_replace_all, pattern = "(codcpsig)|(codcp_med)|(cod_cp)", replacement = "codccpp")
  }

  if (!missing(variable)) {
    year <- file %>%
      str_extract("([:digit:]{4}).dbf$") %>%
      str_remove(".dbf")
    if (variable == "codccpp") {
      table %<>% mutate(
        codccpp := chr2int(codccpp) %>% replace_na(value = -1),
        !!year := codccpp
      )
    }
    if (variable == "niv_mod") {
      table %<>% mutate(!!year := niv_mod)
    } else if (variable == "codlocal") {
      table %<>% mutate(!!year := safe_integer64(codlocal))
    } else if (variable == "codgeo") {
      table %<>% mutate(!!year := codgeo)
    } else if (variable == "cen_pob") {
      table %<>% mutate(!!year := cen_pob)
    } else if (variable == "area_med") {
      table %<>% mutate(!!year := area_med)
    } else if (variable == "fechareg") {
      table %<>% mutate(!!year := fechareg)
    } else if (variable == "fecharet") {
      table %<>% mutate(!!year := fecharet)
    }
  }
  return(table)
}

read_n_copy_excel <- function(connection, path, table, return.results = FALSE, ...) {
  files <- list.files(path = path) %>%
    str_subset("(.xlsx)|(.xls)") %>%
    paste0(path, .)
  results <- imap(files, function(file, step) {
    data <- read_with_name_excel(file, na = c("#NULL!", "", "NA"), trim_ws = TRUE) %>% ece_dictionary()
    if (nrow(data) != 0) {
      dbWriteTable(connection, table, data, ...)
      return(paste("Step:", step, "-- Successfully completed"))
    } else {
      return(paste("No data was retrieved in Step:", step))
    }
  })
  if (return.results == TRUE) {
    return(results)
  } else {
    return()
  }
}

codmod_2_codmod7 <- function(cod_mod) {
  zeros <- 7 - (stringi::stri_length(cod_mod))
  rep(0, zeros) %>%
    paste(collapse = "") %>%
    paste0(., cod_mod)
}

decide_drops <- function(...) {
  values <- list(...)[[1]]
  if (values %>% is.na() %>% all()) {
    return(NA)
  }
  condition <- unique(values) %>%
    length() %>%
    "=="(1)
  value <- head(..., 1)
  ifelse(condition, value, NA)
}

str_delete_empty <- function(x) {
  x[stri_isempty(x)] <- NA
  return(x)
}

handle_nonunique <- function(x) {
  n <- length(x)
  x %<>% na.omit()
  if (length(x) == 1) {
    x <- rep(x, n)
  } else {
    x <- rep(NA, n)
  }
  return(x)
}

handle_nonunique_id <- function(x) {
  n <- nrow(x)
  x %<>% rowwise() %>% mutate(name = sort(c(nombres, paterno, materno)) %>% paste0(collapse = "") %>% str_remove_all(" ") %>% metaphone())
  difference <- x$name %>%
    str_remove_all(" ") %>%
    metaphone()
  return(x)
}

reduce_str_groups <- function(group_a, group_b) {
  bind_rows(group_a, group_b) %>%
    arrange(value) %>%
    group_by(value) %>%
    distinct() %>%
    mutate(n = n()) %>%
    arrange(desc(n)) %>%
    group_by(value) %>%
    summarise(group = merge_str_groups(group), .groups = "drop")
}

iterate_group_str <- function(data, n = 10) {
  if (n < 1) {
    warning("n is lower than 1, setting n = 1")
  }
  if (n > 26) {
    warning("n is greater than 26, setting n = 26")
  }
  order <- c("value", letters[1:n - 1])

  newgroups <- data$value %>%
    group_str(precision = 1, strict = TRUE) %>%
    as_tibble_col("group") %>%
    distinct() %>%
    separate(group, order, sep = ", ", extra = "merge", fill = "right") %>%
    dplyr::filter(!is.na(a)) %>%
    remove_empty("cols")

  if (nrow(newgroups) == 0) {
    return(data)
  }

  delete_vars <- newgroups %>%
    dplyr::select(-value) %>%
    pivot_longer(cols = everything(), values_to = "value") %>%
    dplyr::select(value) %>%
    drop_na() %>%
    pull()

  definitions <- newgroups %>%
    pivot_longer(cols = everything(), values_to = "value") %>%
    dplyr::select(value) %>%
    drop_na() %>%
    left_join(data, by = "value") %>%
    rename(definition = group)

  newgroups %<>% join_by_columns(definitions)

  data %<>% dplyr::filter(!value %in% all_of(delete_vars)) %>%
    rows_update(newgroups, by = "value")

  return(data)
}

join_by_columns <- function(groups, definitions) {
  variables <- groups %>% names()
  if (length(variables) == 1) {
    return(group)
  }

  result <- map(variables, ~ left_join(groups, definitions %>% rename(!!.x := value), by = .x)) %>%
    reduce(full_join, by = variables) %>%
    rowwise() %>%
    mutate(group = c_across(starts_with("definition")) %>% merge_str_groups()) %>%
    dplyr::select(value, group)

  return(result)
}

merge_str_groups <- function(groups) {
  groups %>%
    map(~ str_split(.x, ", ")) %>%
    unlist() %>%
    as_tibble_col("values") %>%
    distinct() %>%
    drop_na() %>%
    pull() %>%
    paste(collapse = ", ")
}

detect_value <- function(string) {
  value <- str_split(string, ",", simplify = TRUE)[1]
  if (is.na(value)) {
    return(string)
  } else {
    return(value)
  }
}

construct_dictionary <- function(data, step_size, overlap = 0.5) {
  steps <- seq.int(0, to = nrow(data), by = step_size)
  dictionary <- future_map2(head(steps, -1), tail(steps, -1), group_str_recursively, data = data, overlap = overlap) %>%
    reduce(reduce_str_groups)
}

group_str_recursively <- function(start, end, data, overlap) {
  overlap <- 1 - overlap
  step <- (end - start) * overlap
  lower_bound <- (start + step) %>% floor()
  upper_bound <- min(nrow(data), end + step) %>% ceiling()

  group_a <- data %>%
    slice(start:end) %>%
    pull() %>%
    group_str(precision = 1, strict = TRUE) %>%
    as_tibble_col("group") %>%
    distinct() %>%
    mutate(value = map_chr(group, detect_value))

  group_b <- data %>%
    slice(seq(lower_bound, upper_bound)) %>%
    pull() %>%
    group_str(precision = 1, strict = TRUE) %>%
    as_tibble_col("group") %>%
    distinct() %>%
    mutate(value = map_chr(group, detect_value))

  result <- reduce_str_groups(group_a, group_b)
  iterated_result <- result %>% iterate_group_str()

  while (nrow(result) > nrow(iterated_result)) {
    result <- iterated_result
    iterated_result <- result %>% iterate_group_str()
  }

  return(result)
}

group_str <- function(strings,
                      precision = 2,
                      strict = FALSE,
                      trim.whitespace = TRUE,
                      remove.empty = TRUE,
                      verbose = FALSE) {
  # coerce to character, if necessary
  if (!is.character(strings)) strings <- as.character(strings)

  # trim white spaces
  if (trim.whitespace) strings <- unname(sapply(strings, trim))

  # remove empty values
  if (remove.empty) {
    removers <- which(sjmisc::is_empty(strings, first.only = F))
    if (length(removers) > 0) strings <- strings[-removers]
  }

  # create matrix from string values of variable
  m <- stringdist::stringdistmatrix(strings, strings, method = "dl", useNames = "strings")

  # init variable that contains "close" pairs
  pairs <- list()

  # create progress bar
  if (verbose) pb <- utils::txtProgressBar(min = 0, max = ncol(m), style = 3)

  # iterate matrix
  for (i in seq_len(nrow(m))) {
    # update progress bar
    if (verbose) utils::setTxtProgressBar(pb, i)

    # check if current element is already grouped
    if (!findInPairs(rownames(m)[i], pairs)) {
      # current row element has not been grouped
      # yet, so go on...
      pairvector <- c()

      for (j in seq_len(ncol(m))) {
        # check if we found a pair's distance that
        # is within the maximum requested distance
        # i.e. which are "close" enough
        if (!is.na(m[i, j]) && m[i, j] <= precision) {
          # go through all rows of this column and
          # check if there's a better match for the
          # currently compared token
          foundBetterToken <- !strict
          for (cnt in seq_len(nrow(m))) {
            if (!is.na(m[cnt, j]) && !is.na(m[i, cnt])) {
              if (strict) {
                if (m[cnt, j] > 0 && m[cnt, j] < m[i, j]) {
                  foundBetterToken <- TRUE
                  break
                }
              } else {
                if (m[cnt, j] <= precision && m[i, cnt] <= precision) {
                  foundBetterToken <- FALSE
                  break
                }
              }
            }
          }

          # in the current column, there's no better
          # matching of strings, so we pick this values
          # and add it to our results
          if (!foundBetterToken) {
            # remember string value
            token <- colnames(m)[j]

            # check if we already found a string value
            # within this column. if not, add string values
            # to "close" pairs of this column
            if (!any(pairvector == token) && !findInPairs(token, pairs)) pairvector <- c(pairvector, token)
          }
        }
      }

      # now we have a vector with all "close" string values
      # from the current row's value
      pairvector <- sort(pairvector)

      # check if we already have saved these values to our list
      # if not, add "close" values as new list element
      if (!any(unlist(lapply(pairs, function(x) length(x) == length(pairvector) && any(x == pairvector))))) pairs <- c(pairs, list(pairvector))
    }
  }

  # we now have a list, where each list element
  # is a vector of "close" string values
  strings.new <- rep(NA, length(strings))

  # go through each list element
  for (i in seq_len(length(pairs))) {
    r <- pairs[[i]]
    # find vector indices of "close" values in
    # original string
    indices <- unlist(lapply(r, function(x) which(strings == x)))
    strings.new[indices] <- paste0(pairs[[i]], collapse = ", ")
  }

  if (verbose) close(pb)

  # return new vector, where all single "close"
  # values are replaced by the group of closed values.
  # e.g. the three values "hello", "holle" and "hole"
  # will be "recoded" into on value "hello, holle, hole"
  strings.new
}

# helper function that finds elements in
# final list of grouped elements
findInPairs <- function(curel, pairs) {
  elfound <- FALSE
  if (length(pairs) > 0) {
    for (ll in seq_len(length(pairs))) {
      pel <- pairs[[ll]]
      if (!is.na(curel) && any(pel == curel)) {
        return(TRUE)
      }
    }
  }
  elfound
}

fuzzy_grep <- function(x, pattern, precision = NULL) {
  if (is.null(precision)) precision <- round(nchar(pattern) / 3)
  if (precision > nchar(pattern)) {
    return(NULL)
  }
  p <- sprintf("(%s){~%i}", pattern, precision)
  grep(pattern = p, x = x, ignore.case = FALSE)
}

string_dist_matrix <- function(string) {
  l <- length(string)
  m <- matrix(nrow = l, ncol = l)
  for (i in 1:(l - 1)) {
    for (j in (i + 1):l) {
      pos <- string_dist(string[i], string[j])
      if (pos == -1) pos <- 8
      m[i, j] <- m[j, i] <- pos
    }
  }

  rownames(m) <- string
  colnames(m) <- string

  m
}

string_dist <- function(s1, s2) {
  if (is.na(s1) || is.na(s2)) {
    return(-1)
  }

  if (nchar(s1) > nchar(s2)) {
    x <- s2
    pattern <- s1
  } else {
    x <- s1
    pattern <- s2
  }

  len <- nchar(pattern)
  if (len > 8) len <- 8

  for (p in 1:len) {
    pos <- grep(pattern = sprintf("(%s){~%i}", pattern, p), x = x, ignore.case = FALSE)
    if (length(pos)) {
      return(p)
    }
  }

  return(-1)
}

chr2int <- function(x, big = FALSE) {
  x %<>% as.character()
  x[is.na(x)] <- NA_character_
  x[(x == "")] <- NA_character_
  x[!is.na(x)] %<>%
    str_remove_all("[[:alpha:]]") %>%
    str_squish() %>%
    str_remove_all(" ") %>%
    if_else(. == "99999999", NA_character_, .)

  if (big == FALSE) {
    return(as.integer(x))
  } else {
    return(as.integer64(x))
  }
}

clean_character <- function(x) {
  x[(x == "") | (x == "\\.") | (x == "#null!")] <- NA_character_
  x[!is.na(x)] %<>%
    str_to_lower() %>%
    str_squish() %>%
    str_replace_all("[[:punct:]]", "") %>%
    stri_trans_general(id = "Latin-ASCII")
  return(x)
}

validate_two <- function(x, y) {
  result <- rep(4L, length(x))
  result[x == y] <- 1L
  result[is.na(y)] <- 5L
  return(result)
}

vector_mode <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }

  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

coalesce_join <- function(x, y,
                          by = NULL, suffix = c(".x", ".y"),
                          reverse = FALSE,
                          join = dplyr::full_join, ...) {
  joined <- join(x, y, by = by, suffix = suffix, ...)
  # names of desired output
  cols <- union(names(x), names(y))

  to_coalesce <- names(joined)[!names(joined) %in% cols]
  suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
  # remove suffixes and deduplicate
  to_coalesce <- unique(substr(
    to_coalesce,
    1,
    nchar(to_coalesce) - nchar(suffix_used)
  ))

  if (reverse == FALSE) {
    coalesced <- purrr::map_dfc(to_coalesce, ~ dplyr::coalesce(
      joined[[paste0(.x, suffix[1])]],
      joined[[paste0(.x, suffix[2])]]
    ))
  } else if (reverse == TRUE) {
    coalesced <- purrr::map_dfc(to_coalesce, ~ dplyr::coalesce(
      joined[[paste0(.x, suffix[2])]],
      joined[[paste0(.x, suffix[1])]]
    ))
  }

  names(coalesced) <- to_coalesce

  dplyr::bind_cols(joined, coalesced)[cols]
}

reduce_with_function <- function(data, reduce_fun, not_coalesce, suffix = "-m", ...) {
  variables <- names(data) %>% str_remove_all(suffix)

  if (!missing(not_coalesce)) {
    to_coalesce <- names(which(table(variables) > 1)) %>% setdiff(not_coalesce)
    pattern <- paste0("(^", paste0(not_coalesce, collapse = paste0("(", suffix, ")*$)|(^")), ")")
    not_coalesce <- names(which(table(variables) == 1)) %>% c(str_subset(names(data), pattern))
  } else {
    to_coalesce <- names(which(table(variables) > 1))
    not_coalesce <- names(which(table(variables) == 1))
  }

  if (reduce_fun == "mode") {
    coalesced <- purrr::map_dfc(to_coalesce, ~ data %>%
      select(matches(paste0("(^", .x, "(", suffix, ")*$)"))) %>%
      mutate(!!sym(.x) := apply(X = ., MARGIN = 1, FUN = calculate_mode, na.rm = TRUE)) %>%
      select(!!sym(.x)))
  } else if (reduce_fun == "lastmode") {
    coalesced <- purrr::map_dfc(to_coalesce, ~ data %>%
      select(matches(paste0("(^", .x, "(", suffix, ")*$)"))) %>%
      mutate(!!sym(.x) := apply(X = ., MARGIN = 1, FUN = calculate_mode, na.rm = TRUE, return_index = "last")) %>%
      select(!!sym(.x)))
  } else if (reduce_fun == "minmode") {
    coalesced <- purrr::map_dfc(to_coalesce, ~ data %>%
      select(matches(paste0("(^", .x, "(", suffix, ")*$)"))) %>%
      mutate(!!sym(.x) := apply(X = ., MARGIN = 1, FUN = calculate_mode, na.rm = TRUE, fun = "min")) %>%
      select(!!sym(.x)))
  } else if (reduce_fun == "maxmode") {
    coalesced <- purrr::map_dfc(to_coalesce, ~ data %>%
      select(matches(paste0("(^", .x, "(", suffix, ")*$)"))) %>%
      mutate(!!sym(.x) := apply(X = ., MARGIN = 1, FUN = calculate_mode, na.rm = TRUE, fun = "max")) %>%
      select(!!sym(.x)))
  } else if (reduce_fun == "mean") {
    coalesced <- purrr::map_dfc(to_coalesce, ~ data %>%
      select(matches(paste0("(^", .x, "(", suffix, ")*$)"))) %>%
      mutate(!!sym(.x) := apply(X = ., MARGIN = 1, FUN = mean, na.rm = TRUE)) %>%
      select(!!sym(.x)))
  }
  dplyr::bind_cols(data %>% select(all_of(not_coalesce)), coalesced)
}

identify_date <- function(date, ...) {
  date %<>%
    str_replace_all("(dd-mm)|(DD-MM)", "01-01") %>%
    str_replace_all("31-11-2013", "30-11-2013") %>%
    # "2679707" "2679713" "2679715" are inconsistent
    str_replace_all("-013$", "-2013$") %>%
    # "1776537" is inconsistent
    str_replace_all("^91-", "^01-") %>%
    # "1429067" is inconsistent
    str_replace_all("-197$", "1997") %>%
    # "1127851" is inconsistent
    str_replace_all("-0002$", "-2000$") %>%
    # "1505627" is inconsistent
    str_replace_all("-0206", "-2006") %>%
    # "1398932" is inconsistent
    str_replace_all("-(20[2-9])(.)$", "200\\2$") %>%
    str_replace_all("-(1[0-7])(..)$", "19\\2") %>%
    #  [1] "0415554" "0421859" "0504423" "0706184" "0539601" "0741140" "1176650" "1176692" "1144534"
    # [10] "0937862" "1247006" "0611681" "1118660" "1058049" "1059351" "0807479" "1212216" "0627844"
    # [19] "1120690" "0539650" "0544866" "0740746" "1224930" "1128727" "0612648" were changed
    parse_date_time(...)
}

filter_date <- function(x) {
  x1 <- x %>%
    parse_date_time(orders = "ymd") %>%
    as_date()
  x1[is.na(x1)] <- x[is.na(x1)] %>%
    str_extract_all("^(.*)-(.*)-") %>%
    paste0(01) %>%
    parse_date_time(orders = "ymd") %>%
    `+`(months(1)) %>%
    floor_date(unit = "month") %>%
    `-`(days(1)) %>%
    as_date()
  x1[year(x1) > 2013] <- NA_Date_
  return(x1)
}

include_name <- function(x, vars) {
  if (missing(vars)) {
    vars <- c("cod_mod7", "anexo", "seccion", "cod_est", "preschool")
  }
  years <- str_extract(x, "(?<=_).+(?=_)")
  types <- str_extract(x, "..$")
  data <- x %>%
    map(~ rlang::parse_expr(.x) %>% eval()) %>%
    map(dplyr::select, any_of(vars)) %>%
    imap(~ mutate(.x, periodo = all_of(years)[.y] %>% as.integer(), tipo_eva = all_of(types)[.y]), .before = 1) %>%
    reduce(bind_rows)
  return(data)
}

import_clustering <- function(dir) {
  idnum <- dir %>%
    str_extract("clustering_result([:digit:]+)/") %>%
    str_extract("[:digit:]+")
  idname <- paste0("id", idnum)
  noisename <- paste0("noise", idnum)
  cluster_number <- list.files(dir, pattern = "(cluster_)") %>%
    length()
  clusters <- list.files(dir, pattern = "(cluster_)") %>%
    paste0(dir, .) %>%
    map(read_cluster) %>%
    reduce(bind_rows) %>%
    arrange(id)
  noise_points <- read_cluster(paste0(dir, "noise.txt"), noise = TRUE, last_cluster = cluster_number)
  bind_rows(clusters, noise_points) %>%
    select(codccpp, id, noise) %>%
    rename(!!idname := id, !!noisename := noise) %>%
    mutate(across(where(is.numeric), as.integer))
}

read_cluster <- function(x, names = c("id", "long", "lat", "codccpp"), noise = FALSE, last_cluster) {
  cluster_group <- read.delim(x, sep = " ", comment.char = "#", header = FALSE)
  names(cluster_group) <- names
  if (noise == TRUE & !missing(last_cluster)) {
    cluster_group %<>% mutate(id = row_number() + last_cluster, noise = TRUE) %>% as_tibble()
  } else {
    cluster_id <- str_extract(x, "_([:digit:]+).txt$") %>%
      str_extract("[:digit:]+") %>%
      as.numeric() %>%
      `+`(1)
    cluster_group %<>% mutate(id = cluster_id, noise = FALSE) %>% as_tibble()
  }
  return(cluster_group)
}

remove_missingcodes <- function(x) {
  x[x == -1] <- NA
  x[x == 0] <- NA
  return(x)
}

calculate_mode <- function(x, na.rm = FALSE, return_index = 1, fun = NULL) {
  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }
  if (is_empty(x)) {
    return(NA)
  }
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  mode <- ux[tab == max(tab)]
  if (length(mode) > 1) {
    if (return_index == "last") {
      return(tail(mode, 1))
    } else if (missing(fun) | is.null(fun)) {
      return(mode[return_index])
    } else {
      return(get(fun)(mode))
    }
  } else if (length(mode) == 1) {
    return(mode)
  } else {
    stop("Mode cannot be calculated")
  }
}

mtc_dictionary <- function(x) {
  x %>%
    rename_with(~ str_to_lower(.x) %>%
      stri_trans_general(id = "Latin-ASCII")) %>%
    mutate(
      across(where(is.character), clean_character),
      idccpp = ubigeo %>% as.integer64(),
      ubigeo = str_sub(ubigeo, 1, 6) %>% as.integer64(),
      categoria = as.integer(categoria),
      categoria = if_else(categoria == 15 & categoria2 == "pueblo", 4L, categoria),
      rural = case_when(
        clasificacion_inei == "rural" ~ 1L,
        clasificacion_inei == "urbano" ~ 0L,
        clasificacion_inei == "na" ~ NA_integer_,
      ),
      rural = if_else(viviendas < 100, 1L, rural)
    ) %>%
    select(-categoria2, -clasificacion_inei) %>%
    rename(
      category = categoria, 
      houses.2015 = viviendas, 
      population.2015 = habitantes
    ) %>%
    select(ubigeo, idccpp, ccpp, capital, rural, category, matches('2015'))
}

highlight_palette <- function(colours, target = c(0, 0),
                              range = range(target), values = NULL,
                              replace_colour = "red4") {
  target <- (target - range[1]) / diff(range)
  ramp <- scales::colour_ramp(colours)
  force(values)
  function(x) {
    # Decide what values to replace
    replace <- x > target[1] & x < target[2]
    if (length(x) == 0) {
      return(character())
    }
    if (!is.null(values)) {
      xs <- seq(0, 1, length.out = length(values))
      f <- stats::approxfun(values, xs)
      x <- f(x)
    }
    out <- ramp(x)
    # Actually replace values
    out[replace] <- replace_colour
    out
  }
}

add_area <- function(cluster_id, data, dist = 1000) {
  name <- paste0("area_", cluster_id)
  data %>%
    group_by_at(cluster_id) %>%
    filter(!!sym(cluster_id) != 0, n() > 1) %>%
    summarise(!!name := st_convex_hull(st_union(geometry)) %>%
      st_buffer(dist = dist) %>%
      st_area() %>%
      units::set_units(km^2)) %>%
    as_tibble() %>%
    select(!!cluster_id, !!name)
}

select_id <- function(x, var, threshold = units::set_units(4 * pi, km^2), start = FALSE) {
  if (start == TRUE) {
    group_by_at(x, var) %>%
      mutate(
        id = if_else(!!sym(paste0("area_", var)) <= threshold, !!sym(var), 0L),
        id_src = if_else(!!sym(paste0("area_", var)) <= threshold, var, NA_character_)
      ) %>%
      ungroup()
  } else {
    group_by_at(x, var) %>%
      mutate(
        id = if_else(!!sym(paste0("area_", var)) <= threshold, !!sym(var), id),
        id_src = if_else(!!sym(paste0("area_", var)) <= threshold, var, id_src)
      ) %>%
      ungroup()
  }
}

absorb_groups <- function(data, group_1, group_2) {

  conversion <- data %>%
    ungroup() %>%
    select({{ group_1 }}, {{ group_2 }}) %>%
    group_by({{ group_2 }}) %>%
    arrange({{ group_1 }}) %>%
    mutate(group = if_else(!is.na({{ group_2 }}) & row_number() != 1, NA_integer_, {{ group_1 }})) %>%
    fill(group, .direction = "down") %>%
    ungroup()
  
  rows = nrow(data) + 1
  
  while (rows != nrow(data)) {
    conversion %<>%
      group_by({{ group_1 }}) %>%
      arrange(group) %>%
      mutate(group = if_else(row_number() != 1, NA_integer_, group)) %>%
      fill(group, .direction = "down") %>%
      group_by({{ group_2 }}) %>%
      arrange(group) %>%
      mutate(group = if_else(!is.na({{ group_2 }}) & row_number() != 1, NA_integer_, group)) %>%
      fill(group, .direction = "down") %>%
      ungroup()
    
    conversion_1 <- conversion %>%
      select({{ group_1 }}, group) %>%
      drop_na() %>%
      distinct()
    conversion_2 <- conversion %>%
      select({{ group_2 }}, group) %>%
      drop_na() %>%
      distinct()
    
    name_1 <- names(dplyr::select(data, {{ group_1 }}))
    name_2 <- names(dplyr::select(data, {{ group_2 }}))
    
    result <- data %>%
      left_join(conversion_1, by = name_1) %>%
      left_join(conversion_2, by = name_2, suffix = c("", "2")) %>%
      mutate(group = coalesce(group, group2)) %>%
      select(-group2)
    
    rows <- result %>% nrow()
  }
  
  result %<>% 
    mutate(group := if_else(is.na(group), max(group, na.rm = TRUE) + {{group_2}}, group)) %>%
    group_by(group) %>%
    mutate(group = cur_group_id())
  
return(result)
  
}

recodify_id <- function(x) {
  clusters <- x %>%
    mutate(
      id_src = chr2int(id_src),
      id_src = case_when(
        id_src > 15 ~ id_src / 100,
        id_src > 2 ~ id_src / 10,
        TRUE ~ id_src %>% as.numeric()
      )
    ) %>%
    group_by(id, id_src) %>%
    mutate(group_id2 = if_else(id != 0, cur_group_id(), NA_integer_))

  conversion <- clusters %>%
    ungroup() %>%
    select(group_id, group_id2) %>%
    drop_na(group_id) %>%
    group_by(group_id2) %>%
    arrange(group_id) %>%
    mutate(group = if_else(!is.na(group_id2) & row_number() != 1, NA_integer_, group_id)) %>%
    fill(group, .direction = "down") %>%
    ungroup()
  
  result = nrow(x) + 1
  
  while (result != nrow(x)) {
    conversion %<>%
      group_by(group_id) %>%
      arrange(group) %>%
      mutate(group = if_else(row_number() != 1, NA_integer_, group)) %>%
      fill(group, .direction = "down") %>%
      group_by(group_id2) %>%
      arrange(group) %>%
      mutate(group = if_else(!is.na(group_id2) & row_number() != 1, NA_integer_, group)) %>%
      fill(group, .direction = "down") %>%
      ungroup()

    conversion_1 <- conversion %>%
      select(group_id, group) %>%
      distinct()
    conversion_2 <- conversion %>%
      select(group_id2, group) %>%
      drop_na() %>%
      distinct()

    result <- clusters %>%
      left_join(conversion_1, by = "group_id") %>%
      left_join(conversion_2, by = "group_id2", suffix = c("", "2")) %>%
      nrow()
  }
  
  clusters %<>%
    left_join(conversion_1, by = "group_id") %>%
    left_join(conversion_2, by = "group_id2", suffix = c("", "2")) %>%
    mutate(group = coalesce(group, group2)) %>%
    select(-matches("^group.")) %>%
    group_by(group) %>%
    arrange(group, desc(id_src)) %>%
    mutate(
      across(matches("(^id$)|(^id_src$)"), ~ case_when(
        !is.na(group) & row_number() != 1 ~ NA_real_,
        TRUE ~ as.numeric(.)
      )),
      id_src = if_else(id == 0 & !is.na(group), 0, id_src),
      id = if_else(id_src == 0, group, as.integer(id))
    ) %>%
    fill(id, id_src, .direction = "down") %>%
    ungroup() %>%
    select(-group) %>%
    filter(id != 0 | id_src == 0) %>%
    group_by(id, id_src) %>%
    mutate(cluster = cur_group_id())

  last_cluster <- clusters %>%
    pull(cluster) %>%
    max()

  noise <- x %>%
    ungroup() %>%
    select(-group_id) %>%
    anti_join(clusters, by = "codccpp") %>%
    mutate(
      id_src = chr2int(id_src),
      id_src = case_when(
        id_src > 15 ~ id_src / 100,
        id_src > 2 ~ id_src / 10,
        TRUE ~ id_src %>% as.numeric()
      ),
      cluster = row_number() + all_of(last_cluster)
    )

  bind_rows(clusters, noise) %>%
    ungroup() %>%
    select(codccpp, cluster) %>%
    arrange(cluster, codccpp)
}

chr2year <- function(x) {
  x[!is.na(x)] %<>%
    str_to_lower() %>%
    str_squish() %>%
    stri_trans_general(id = "Latin-ASCII") %>%
    str_replace_all("(dd)|(mm)", "01") %>%
    str_replace_all("(1075)", "1975") %>%
    str_replace_all("(-013)", "2013") %>%
    stri_replace_all_regex("-([2-9])([0-9])-", "-0$2-")
  return(x)
}

summarize_ise <- function(mids, ise_type) {
  if (!"mids" %in% class(mids)) {
    calculate_ise(mids, ise_type) %>%
      mutate(!!ise_type := rowMeans(across(everything()))) %>%
      select(!!sym(ise_type)) %>%
      as_tibble()
  } else {
    1:mids$m %>%
      map_dfc(~ mice::complete(mids, .x) %>% calculate_ise(ise_type, .x)) %>%
      mutate(!!ise_type := rowMeans(across(everything()))) %>%
      select(!!sym(ise_type)) %>%
      as_tibble()
  }
}

calculate_ise <- function(data, ise_type, i) {
  if (ise_type != "summary") {
    data %<>% select(starts_with(ise_type)) %>% mutate(across(everything(), as_factor))
  }
  if (!missing(i)) {ise_type <- paste0(ise_type, i)}
  corr_matrix <- hetcor(data) %>% `$`(correlations)
  pca <- principal(r = corr_matrix, n.obs = nrow(data), covar = TRUE, scores = TRUE, rotate = "none")
  if (ise_type == "summary") {
    vars_mean <- data %>%
      summarise(across(everything(), ~ as.numeric(.x) %>% mean())) %>%
      mutate(across(starts_with("ise_"), ~0)) %>%
      as.matrix()
    ise_type <- "ise"
  } else {
    vars_mean <- data %>%
      summarise(across(everything(), ~ as.numeric(.x) %>% mean())) %>%
      as.matrix()
  }
  vars_sd <- data %>%
    summarise(across(everything(), ~ as.numeric(.x) %>% sd())) %>%
    as.matrix()
  ise <- data %>%
    mutate(across(everything(), as.numeric)) %>%
    as.matrix() %>%
    `-`(vars_mean %x% rep(1, nrow(data))) %>%
    `%*%`(t(vars_sd^-1) * pca$weights) %>%
    as.numeric()
  tibble(!!ise_type := ise)
}

aggregate_ise <- function(mids) {
  data <- c("ise_house", "ise_utilities", "ise_assets", "ise_other") %>%
    map_dfc(summarize_ise, mids = mids)
  if (!"mids" %in% class(mids)) {
    edu_data <- mids %>% 
      select(starts_with("education")) %>%
      mutate(
        across(everything(), as.numeric), 
        edumax = apply(., 1, function(x) max(x))
        ) %>%
      select(edumax)
  } else {
    edu_data <- 1:mids$m %>%
      map(function(number) {
        mice::complete(mids, number) %>%
          select(starts_with("education")) %>%
          mutate(across(everything(), as.numeric)) %>%
          as.matrix()
      }) %>%
      reduce(`+`) %>%
      `/`(mids$m) %>%
      as_tibble() %>%
      mutate(edumax = apply(., 1, function(x) max(x))) %>%
      select(edumax) 
  }
  add_column(data, calculate_ise(add_column(edu_data, data), ise_type = "summary"))
}

binarize <- function(data, variable, true, false) {
  if (!missing(false)) {
    data %<>%
      mutate(
        across(matches(variable), ~ case_when(
          str_detect(., true) ~ TRUE,
          str_detect(., false) ~ FALSE,
          is.na(.) ~ NA,
          # TRUE ~ .
        ))
      )
  } else {
    data %<>%
      mutate(
        across(matches(variable), ~ case_when(
          str_detect(., true) ~ TRUE,
          is.na(.) ~ NA,
          # TRUE ~ .
        ))
      )
  }
  return(data)
}

orderize <- function(data, variable, scale) {
  if (scale == "time") {
    data %<>%
      mutate(
        across(matches(variable), ~ case_when(
          str_detect(., "nunca$") ~ 0L,
          str_detect(., "(^a veces$)|(^pocas veces$)|(^unas cuantas veces al ano)") ~ 1L,
          str_detect(., "(^frecuentemente$)|(^muchas veces)|(^unas cuantas veces al mes)") ~ 2L,
          str_detect(., "(siempre$)|(^una o mas veces por semana$)") ~ 3L,
          is.na(.) ~ NA_integer_
          # TRUE ~ .
        ))
      )
    return(data)
  } else if (scale == "time_reversed") {
    data %<>%
      mutate(
        across(matches(variable), ~ case_when(
          str_detect(., "nunca$") ~ 3L,
          str_detect(., "(^a veces$)|(^pocas veces$)|(^unas cuantas veces al ano)") ~ 2L,
          str_detect(., "(^frecuentemente$)|(^muchas veces)|(^unas cuantas veces al mes)") ~ 1L,
          str_detect(., "(siempre$)|(^una o mas veces por semana$)") ~ 0L,
          is.na(.) ~ NA_integer_
          # TRUE ~ .
        ))
      )
    return(data)
  } else if (scale == "rank") {
    data %<>%
      mutate(
        across(matches(variable), ~ case_when(
          str_detect(., "(^muy baja)|(^muy mal)|(^regular)|(menos de 10)") ~ 0L,
          str_detect(., "(^baja)|(^mal)|(^no muy bueno)|(entre 11 y 14)") ~ 1L,
          str_detect(., "(^alta)|(^bien)|(^muy bueno)|(entre 15 y 17)") ~ 2L,
          str_detect(., "(^muy alta)|(^muy bien)|(^excelente)|(entre 18 y 20)") ~ 3L,
          is.na(.) ~ NA_integer_
          # TRUE ~ .
        ))
      )
  } else if (scale == "agreement") {
    data %<>%
      mutate(
        across(matches(variable), ~ case_when(
          str_detect(., "^totalmente en desacuerdo$") ~ 0L,
          str_detect(., "^en desacuerdo$") ~ 1L,
          str_detect(., "^de acuerdo$") ~ 2L,
          str_detect(., "^totalmente de acuerdo$") ~ 3L,
          is.na(.) ~ NA_integer_
          # TRUE ~ .
        ))
      )
  } else if (scale == "agreement_augmented") {
    data %<>%
      mutate(
        across(matches(variable), ~ case_when(
          str_detect(., "(^ni de acuerdo ni en desacuerdo$)|(^nunca me han solicitado participar por este motivo$)|(^no conozco si se realiza esta actividad$)|(^no conozco sobre este aspecto$)") ~ -1L,
          str_detect(., "(^muy en desacuerdo$)|(^nada satisfecho$)") ~ 0L,
          str_detect(., "(^en desacuerdo$)|(^poco satisfecho$)") ~ 1L,
          str_detect(., "(^de acuerdo$)|(^satisfecho$)") ~ 2L,
          str_detect(., "(^muy de acuerdo$)|(^muy satisfecho$)") ~ 3L,
          is.na(.) ~ NA_integer_
          # TRUE ~ .
        ))
      )
  } else if (scale == "agreement_augmented_reversed") {
    data %<>%
      mutate(
        across(matches(variable), ~ case_when(
          str_detect(., "(^ni de acuerdo ni en desacuerdo$)|(^nunca me han solicitado participar por este motivo$)|(^no conozco si se realiza esta actividad$)|(^no conozco sobre este aspecto$)") ~ -1L,
          str_detect(., "(^muy en desacuerdo$)|(^nada satisfecho$)") ~ 3L,
          str_detect(., "(^en desacuerdo$)|(^poco satisfecho$)") ~ 2L,
          str_detect(., "(^de acuerdo$)|(^satisfecho$)") ~ 1L,
          str_detect(., "(^muy de acuerdo$)|(^muy satisfecho$)") ~ 0L,
          is.na(.) ~ NA_integer_
          # TRUE ~ .
        ))
      )
  } else if (scale == "importance") {
    data %<>%
      mutate(
        across(matches(variable), ~ case_when(
          str_detect(., "^nada importante$") ~ 0L,
          str_detect(., "^poco importante$") ~ 1L,
          str_detect(., "^importante$") ~ 2L,
          str_detect(., "^muy importante$") ~ 3L,
          is.na(.) ~ NA_integer_
          # TRUE ~ .
        ))
      )
  }
}

translate_variables <- function(x, names_dictionary) {
  if (missing(names_dictionary)) {
    names_dictionary <- tribble(
      ~definitions, ~variable,
      # Guardian
      list("soy su mama"), "mama",
      list("soy su papa"), "papa",
      list("soy su abueloa"), "abueloa",
      list("soy su hermanoa"), "hermanoa",
      list("soy otro familiar yo apoderado primo tio pareja del papa o la mama etc", "soy otro familiar yo apoderadoa primo tio pareja del papa o la mama etc", "soy otro familiar yo apoderado"), "otro familiar",
      list("no soy su familiar pero soy su apoderadoa"), "otro familiar",
      # Language
      list("una lengua amazonica ashaninka shipibo awajun etc", "ashaninka", "awajun", "matsigenga", "shawi", "shipibokonibo", "otra lengua originaria"), "lengua originaria",
      list("una lengua extranjera", "una lengua extranjera ingles frances etc", "lengua extranjera ingles frances etc"), "lengua extranjera",
      # Water
      list("del cano dentro de la casa"), "cano dentro de casa",
      list("del rio acequia riachuelos manantial o similar", "del rio"), "rio",
      list("del cano fuera de la casa"), "cano fuera de casa",
      list("del camion cisterna aguatero u otro similar", "del camion cisterna"), "camion cisterna",
      list("de un pozo"), "pozo",
      list("del pilon de uso publico"), "pilon de uso publico",
      # Sewage
      list("el bano esta dentro de la casa y podemos jalar la palanca o cadena", "el bano esta dentro de la casa y se puede jalar"), "bano propio con desague",
      list("el bano esta fuera de la casa es compartido con los vecinos y podemos jalar la palanca o cadena", "el bano esta fuera de la casa es compartido con los vecinos y podemos jalar la", "el bano compartido se puede jalar"), "bano compartido con desague",
      list("tenemos un bano propio que no esta conectado a un desague solo tiene un pozo", "bano propio no esta conectado a un desague"), "bano propio con pozo",
      list("tenemos un bano propio que no esta conectado a un desague pero tiene un tratamiento quimico", "bano propio no esta conectado a un desague pero tiene un tratamiento quimico", "bano propio con pozo pero tiene un tratamiento quimico"), "bano propio con tratamiento",
      list("tenemos un bano compartido con los vecinos que no esta conectado a desague solo tiene un pozo", "tenemos un bano compartido con los vecinos que no esta conectado a desague so", "tenemos un bano compartido con los vecinos que no esta conectado a un desague", "bano compartido no esta conectado a desague"), "bano compartido con pozo",
      list("tenemos un bano compartido con los vecinos que no esta conectado a un desague pero tiene un tratamiento quimico", "tenemos un bano propio que no esta conectado a un desague pero tiene un tratami", "bano compartido no esta conectado a un desague pero tiene un tratamiento quimico", "bano compartido con pozo pero tiene un tratamiento quimico"), "bano compartido con tratamiento",
      list("no tenemos bano"), "sin bano",
      # Roof
      list("concreto armado cemento y ladrillo"), "concreto armado",
      list("planchas de calamina fibra de cemento o similares", "planchas de calamina fibra de cemento eternit o similares"), "planchas",
      # Wall
      list("piedra o sillar con cal o cemento"), "piedra sillar con cal o cemento",
      list("quincha cana con barro"), "quincha",
      # Floor
      list("madera entablado", "madera entablados"), "entablado",
      list("losetas mayolicas terrazos o similares"), "losetas",
      list("parquet o madera pulida"), "parquet",
      list("pisos asfalticos vinilicos o similares"), "pisos asfalticos",
      # Age
      list("mas de 66 anos"), "mas de 65 anos",
      # Education
      list("despues de la universidad ha seguido estudiando maestria o doctorado", "despues de la universidad ha seguido estudiando", "posgrado maestria o doctorado", "posgrado maestria yo doctorado"), "posgrado",
      list("educacion tecnica incompleta", "superior no universitaria incompleta pedagogica tecnica artistica o militarpolicial escuela de sub oficiales"), "superior no universitaria incompleta",
      list("educacion tecnica completa", "superior no universitaria completa pedagogica tecnica artistica o militarpolicial escuela de sub oficiales"), "superior no universitaria completa",
      list("superior universitaria incompleta o militarpolicial escuela de oficiales"), "superior universitaria incompleta",
      list("superior universitaria completa o militarpolicial escuela de oficiales"), "superior universitaria completa",
      # Quit
      list("no dejaria la escuela"), "no",
      list("si dejaria la escuela"), "si",
      # Expectations
      list("terminara la primaria"), "primaria completa",
      list("no secundaria completa", "no terminare secundaria", "no terminare la secundaria"), "secundaria incompleta",
      list("terminara la secundaria", "terminare la secundaria", "toda la secundaria"), "secundaria completa",
      list("terminara una carrera tecnica", "terminare una carrera tecnica", "carrera tecnica"), "superior no universitaria completa",
      list("terminara una carrera universitaria", "terminare una carrera universitaria", "carrera universitaria"), "superior universitaria completa",
      list("terminara una maestria o doctorado", "terminare una maestria o doctorado", "maestria o doctorado"), "posgrado",
      # Grades
      list("menos de 10"), "muy baja",
      list("entre 11 y 14"), "baja",
      list("entre 15 y 17"), "alta",
      list("entre 18 y 20"), "muy alta",
      # Relocation
      list("si el ano pasado estudio en una escuela publica", "no estudie en esta misma escuela"), "no",
      list("si el ano pasado estudio en una escuela privada", "si el ano pasado estudie en una escuela privada"), "privada",
      list("no el estudiante estudio en esta misma escuela", "si el ano pasado estudie en una escuela publica"), "publica",
      # Preschool
      list("asistio a un nido jardin o cei", "asistio al nido jardin o cei", "asistio a un nidojardin o centro de educacion inicial"), "nido jardin o cei",
      list("no asistio a ningun centro de educacion inicial", "no asistio a ninguna institucion de educacion inicial"), "no",
    ) %>%
      unnest(definitions) %>%
      mutate(definitions = as.character(definitions))
  }

  dictionary <- setNames(names_dictionary$variable, paste0("^", names_dictionary$definitions, "$"))

  x %<>% str_to_lower() %>%
    str_squish() %>%
    str_replace_all(dictionary)
  return(x)
}

read_eceform <- function(filter, survey_key, table_id, dir = "ECE/Otros/Questionnares/") {
  file <- paste0(dir, "ECECuestionario_", filter[1], "_", filter[2], ".csv")
  survey_key %<>% filter(year %in% filter, type %in% filter)
  dictionary <- setNames(survey_key$definition, paste0("(?i)\\b", survey_key$name, "\\b"))
  if (!missing(table_id)) {
    survey_key %<>% filter(survey_table %in% table_id)
  }
  variables <- survey_key$definition
  data <- read_csv(file, locale = locale(decimal_mark = ","), na = c("", "NA", "#NULL!"), col_types = "c") %>%
    rename_with(~ str_replace_all(.x, dictionary)) %>%
    select(cod_mod, anexo, seccion, cod_est, all_of(variables)) %>%
    eceform_dictionary() %>%
    mutate(
      periodo = as.integer(filter[1]),
      tipo_eva = case_when(
        all_of(filter)[2] == "2p" ~ 1L,
        all_of(filter)[2] == "4p" ~ 2L,
        all_of(filter)[2] == "2s" ~ 3L
      ),
      .before = 1
    )
  return(data)
}

select_ecetable <- function(survey_key, table) {
  survey_key %>%
    filter(survey_table == table) %>%
    pull(definition) %>%
    str_remove_all("(^b-)|(^t-)|(^-1t-)|(^aa-)|(^-1aa-)|(^a-)|(^i-)|(^r-)") %>%
    unique() %>%
    paste(collapse = "$)|(^") %>%
    paste0("(^", ., "$)")
}

sortnmode <- function(x) {
  sort(x) %>% calculate_mode(na.rm = TRUE)
}

read_ininoeform <- function(x, survey_key, dir = "Padron Unidades Educativas/Cedulas/locales/pronoei/LocalPRONOEI") {
  x %<>% as.integer()
  file <- paste0(dir, x, ".dbf")
  survey_key %<>% filter(year == x)
  dictionary <- setNames(survey_key$definition, paste0("^", survey_key$name, "$"))
  variables <- survey_key$definition
  data <- read.dbf(file, as.is = TRUE) %>%
    as_tibble() %>%
    rename_with(~ str_to_lower(.x) %>% str_replace_all(dictionary)) %>%
    select(cod_mod, anexo, all_of(variables)) %>%
    mutate(
      across(matches("(\\.school)|(\\.town)"), str_replace_all, "X", "1"),
      across(matches("(^service.water$)|(^tables.adequate$)"), ~ case_when(
        . %in% c("S", "1") ~ 1L,
        . %in% c("N", "2") ~ 0L,
        TRUE ~ NA_integer_
      )),
      across(!starts_with("niv_mod"), chr2int),
      ise_house.wall = case_when(
        ise_house.wall == 1 ~ 1L, # Estera, cartón o plástico.
        ise_house.wall == 2 ~ 2L, # Eternit o fibra de concreto.
        ise_house.wall == 3 ~ 3L, # Madera
        ise_house.wall == 4 ~ 2L, # Piedra con barro, cal o cemento.
        ise_house.wall == 5 ~ 2L, # Quincha
        ise_house.wall == 6 ~ 4L, # Adobe o tapial.
        ise_house.wall == 7 ~ 5L, # Ladrillo o concreto
        ise_house.wall == 8 ~ 1L, # Otro
        is.na(ise_house.wall) ~ NA_integer_
      ),
      ise_house.roof = case_when(
        ise_house.roof == 1 ~ 4L, # Paja, hoja de palmera, etc
        ise_house.roof == 2 ~ 2L, # Estera, cartón o plástico
        ise_house.roof == 3 ~ 1L, # Lata o latón.
        ise_house.roof == 4 ~ 3L, # Caña con barro
        ise_house.roof == 5 ~ 4L, # Calamina
        ise_house.roof == 6 ~ 4L, # Fibra de cemento
        ise_house.roof == 7 ~ 6L, # Teja
        ise_house.roof == 8 ~ 5L, # Madera
        ise_house.roof == 9 ~ 7L, # Concreto armado
        ise_house.roof == 10 ~ 1L, # Otro
        is.na(ise_house.roof) ~ NA_integer_
      ),
      ise_house.floor = case_when(
        ise_house.floor == 1 ~ 1L, # Tierra
        ise_house.floor == 2 ~ 3L, # Madera
        ise_house.floor == 3 ~ 2L, # Cemento
        ise_house.floor == 4 ~ 4L, # Loseta, cerámico o similar
        ise_house.floor == 5 ~ 5L, # Vinílico, pisopak o similar
        ise_house.floor == 6 ~ 6L, # Parquet o madera pulida
        ise_house.floor == 7 ~ 1L, # Otro
        is.na(ise_house.floor) ~ NA_integer_
      ),
      across(matches("(^ise_utilities)|(^tables.adequate$)"), ~ case_when(
        . == 1 ~ TRUE,
        is.na(.) ~ NA,
        TRUE ~ FALSE
      )),
      ise_utilities.water = if_else(service.water == 0L, FALSE, ise_utilities.water, ise_utilities.water),
      across(where(is.character), str_to_lower),
    ) %>%
    select(-service.water)

  if (x %in% c(2011, 2012, 2013)) {
    data %<>%
      mutate(
        tables = 1 - (tables.repair / tables),
        tables = if_else(is.nan(tables), 0, tables),
        time.school = lubridate::duration(days = min(days.school, 8/24), hours = hours.school, minutes = minutes.school), #Cut days!
        time.town = lubridate::duration(days = min(days.town, 8/24), hours = hours.town, minutes = minutes.town)
      ) %>%
      select(-matches("(^days)|(^hours)|(^minutes)"), -tables.repair) %>%
      pivot_longer(
        cols = matches("(\\.school)|(\\.town)") & !matches("(^time)"),
        values_to = "value",
        names_to = c("type.transport", "to.transport"),
        names_sep = "\\."
      ) %>%
      group_by(cod_mod, anexo) %>%
      arrange(value) %>%
      filter(!is.na(value) | row_number() == 1) %>%
      select(-value) %>%
      ungroup() %>%
      mutate(
        across(matches("type\\.transport"), ~ case_when(
          str_detect(., "(^airplane$)") ~ 200L,
          str_detect(., "(^foot$)|(^horse$)") ~ 5L,
          str_detect(., "^canoe$") ~ 15L,
          str_detect(., "(^bus$)|(^ship$)") ~ 30L,
          TRUE ~ NA_integer_
        ))
      ) %>%
      pivot_wider(
        names_prefix = "transport.",
        names_from = "to.transport",
        values_from = "type.transport",
        values_fn = mean
      )
  } else if (x %in% c(2016, 2017)) {
    data %<>%
      mutate(
        books = rowSums(across(starts_with("books")), na.rm = TRUE)
      ) %>%
      select(-starts_with("books."))
  }

  if ("years" %>% `%in%`(names(data)) & "months" %>% `%in%`(names(data))) {
    data %<>%
      mutate(
        fechareg = case_when(
          months > 6 ~ all_of(x) - years - 1L,
          months <= 6 ~ all_of(x) - years
        )
      ) %>%
      select(-years, -months)
  }

  return(data)
}

theme_thesis <- function(base_size = 9,
                         base_family = "Alegreya Sans",
                         base_line_size = base_size / 170,
                         base_rect_size = base_size / 170,
                         ticks = TRUE) {
  theme_classic(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size
  ) %+replace%
    theme(
      plot.title = element_text(color = rgb(25, 43, 65, maxColorValue = 255), face = "bold", hjust = 0),
      axis.title = element_text(color = rgb(20, 20, 20, maxColorValue = 255), size = rel(0.9), hjust = 1, margin = margin(t = 0, r = 20, b = 20, l = 0)),
      axis.text = element_text(color = rgb(105, 105, 105, maxColorValue = 255), size = rel(0.75)),
      axis.title.y = element_text(margin = margin(0, 4, 0, 0), angle = 90),
      axis.title.x = element_text(margin = margin(4, 0, 0, 0), angle = 0),
      panel.grid.major = element_line(rgb(200, 200, 200, maxColorValue = 255), linetype = "dotted", size = rel(3)),
      legend.title = element_text(color = rgb(20, 20, 20, maxColorValue = 255), size = rel(0.9)),
      legend.box.spacing = unit(0, "mm"),
      legend.text = element_text(size = rel(0.75), color = "#7F7F7F", margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "pt")),
      legend.background = element_rect(fill = "transparent", colour = "transparent"),
      panel.background = element_rect(fill = "transparent", colour = "transparent"),
      panel.spacing = unit(1, "lines"),
      plot.background = element_rect(fill = "transparent", colour = "transparent"),
      plot.margin=grid::unit(c(0,0,0,0),"cm"),
      strip.text.x = element_text(size = rel(1.25), color = "#7F7F7F", face = "bold", margin = margin(t = 0, r = 0, b = 6, l = 0, unit = "pt")),
      strip.text.y = element_text(size = rel(1.25), color = "#7F7F7F", face = "bold", margin = margin(t = 0, r = 1, b = 0, l = 2, unit = "pt"), angle = 90),
      strip.background = element_rect(fill = "transparent", colour = "transparent"),
      strip.placement = "outside",
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
      complete = TRUE
    )
}

read_resultsform <- function(x, survey_key, table, dir = "Padron Unidades Educativas/Cedulas/resultados/") {
  if (table == "preschool") {
    survey_key %<>%
      filter(
        before.2006 == (x < 2006),
        equal.2006 == (x == 2006),
        equal.2007 == (x == 2007),
        before.2010 == (x < 2010),
        after.2014 == (x > 2014),
        after.2017 == (x > 2017)
      )
  } else if (table == "school") {
    survey_key %<>% filter(before.2008 == (x < 2008))
  }
  
  dictionary <- setNames(survey_key$definition, paste0("^", survey_key$name, "$"))
  variables <- survey_key$definition
  file <- paste0(dir, 'Resultados', x, ".dbf")
  data <- read.dbf(file, as.is = TRUE) %>%
    as_tibble() %>%
    rename_with(~ str_to_lower(.x) %>% str_replace_all(dictionary)) %>%
    select(cod_mod, anexo, niv_mod, starts_with('cod_car'), ges_dep, matches('(^area$)|(^area_med$)'), cuadro, tipdato, matches("\\.(boys)|(girls)$")) %>%
    mutate(
      cuadro = chr2int(cuadro),
      across(!matches("(niv_mod)|(ges_dep)"), safe_integer),
      across(where(is.character), clean_character))
  
  if (table == "preschool") {
    data %<>%
      filter(str_detect(niv_mod, "a")) %>%
      mutate(
        year = all_of(x),
        cuadro = case_when(
          year %in% c(2003:2007) & cuadro == 5100 ~ "preschool",
          year %in% c(2008:2009) & cuadro == 500 ~ "preschool",
          year %in% c(2010:2019) & cuadro == 101 ~ "preschool", #After 2015 is C101
          TRUE ~ NA_character_
        )
      ) %>%
      drop_na(cuadro) %>%
      mutate(
        tipdato = case_when(
          # Preschool
          year %in% c(2003:2009) & cuadro == "preschool" & tipdato == 1 ~ "passed",
          year %in% c(2003:2009) & cuadro == "preschool" & tipdato == 2 ~ "retired",
          year %in% c(2003:2009) & cuadro == "preschool" & tipdato == 3 ~ "dead",
          year %in% c(2010:2019) & cuadro == "preschool" & tipdato == 1 ~ "passed",
          year %in% c(2010:2019) & cuadro == "preschool" & tipdato == 2 ~ "retired",
          year %in% c(2010:2019) & cuadro == "preschool" & tipdato == 3 ~ "transferred",
          year %in% c(2010:2019) & cuadro == "preschool" & tipdato == 4 ~ "dead",
          TRUE ~ NA_character_
        )
      ) %>%
      drop_na(tipdato) %>%
      relocate(year, .before = 1)
  } else if (table == "school") {
    data %<>%
      filter(niv_mod %in% c('b0', 'f0')) %>%
      mutate(
      year = all_of(x),
      cuadro = case_when(
        year %in% c(2003:2007) & cuadro == 5100 ~ "enrollment",
        year %in% c(2003:2007) & cuadro == 5200 ~ "retirement",
        year %in% c(2008:2009) & cuadro == 501 ~ "enrollment",
        year %in% c(2008:2009) & cuadro == 502 ~ "retirement",
        year %in% c(2010:2019) & cuadro == 101 ~ "enrollment",
        year %in% c(2010:2019) & cuadro == 102 ~ "retirement",
        TRUE ~ NA_character_)
      ) %>%
      drop_na(cuadro) %>%
      mutate(
      tipdato = case_when(
        # Enrollment
        year %in% c(2003:2019) & cuadro == "enrollment" & tipdato == 1 ~ "passed", #For all years
        year %in% c(2010:2015) & cuadro == "enrollment" & tipdato == 2 ~ "passed",
        year %in% c(2016:2019) & cuadro == "enrollment" & between(tipdato, 2, 4) ~ "passed",
        
        year %in% c(2003:2008) & cuadro == "enrollment" & tipdato == 4 ~ "failed",
        year %in% c(2010:2015) & cuadro == "enrollment" & tipdato == 3 ~ "failed",
        year %in% c(2016:2019) & cuadro == "enrollment" & tipdato == 5 ~ "failed",
        
        year %in% c(2009) & cuadro == "enrollment" & tipdato == 7 ~ "transferred",
        year %in% c(2010:2015) & cuadro == "enrollment" & tipdato == 4 ~ "transferred",
        year %in% c(2016:2019) & cuadro == "enrollment" & tipdato == 6 ~ "transferred",
        
        year %in% c(2003:2008) & cuadro == "enrollment" & tipdato == 7 ~ "retired",
        year %in% c(2009) & cuadro == "enrollment" & tipdato == 8 ~ "retired",
        year %in% c(2010:2014) & cuadro == "enrollment" & tipdato == 5 ~ "retired",
        year %in% c(2015) & cuadro == "enrollment" & tipdato == 6 ~ "retired",
        year %in% c(2016:2019) & cuadro == "enrollment" & between(tipdato, 7, 8) ~ "retired",
        
        year %in% c(2003:2008) & cuadro == "enrollment" & tipdato == 8 ~ "dead",
        year %in% c(2009) & cuadro == "enrollment" & tipdato == 9 ~ "dead",
        year %in% c(2010:2014) & cuadro == "enrollment" & tipdato == 6 ~ "dead",
        year %in% c(2015) & cuadro == "enrollment" & tipdato == 7 ~ "dead",
        year %in% c(2016:2019) & cuadro == "enrollment" & tipdato == 9 ~ "dead",
        
        # Retirement
        year %in% c(2003) & cuadro == "retirement" & tipdato == 1 ~ "poverty",
        year %in% c(2003) & cuadro == "retirement" & tipdato == 2 ~ "sickness",
        year %in% c(2004) & cuadro == "retirement" & between(tipdato, 1, 3) ~ "poverty",
        year %in% c(2004) & cuadro == "retirement" & tipdato == 8 ~ "sickness",
        year %in% c(2005:2011) & cuadro == "retirement" & between(tipdato, 1, 3) ~ "poverty",
        year %in% c(2005:2011) & cuadro == "retirement" & tipdato == 5 ~ "sickness",
        year %in% c(2012) & cuadro == "retirement" & between(tipdato, 1, 2) ~ "poverty",
        year %in% c(2012) & cuadro == "retirement" & tipdato == 4 ~ "sickness",
        year %in% c(2013) & cuadro == "retirement" & tipdato %in% c(1, 4) ~ "poverty",
        year %in% c(2013) & cuadro == "retirement" & tipdato == 3 ~ "sickness",
        year %in% c(2014:2019) & cuadro == "retirement" & tipdato %in% c(1, 4, 5) ~ "poverty",
        year %in% c(2014:2019) & cuadro == "retirement" & tipdato == 3 ~ "sickness",
        cuadro == "retirement" ~ "other",
        TRUE ~ NA_character_)
    ) %>%
    drop_na(tipdato) %>%
    relocate(year, .before = 1)
  }
  return(data)
}

read_teachersform <- function(x, survey_key, table, dir = "Padron Unidades Educativas/Cedulas/resultados/") {
  if (table == "preschool") {
    survey_key %<>%
      filter(
        before.2006 == (x < 2006),
        equal.2006 == (x == 2006),
        equal.2007 == (x == 2007),
        before.2010 == (x < 2010),
        after.2014 == (x > 2014),
        after.2017 == (x > 2017)
      )
  } else if (table == "school") {
    survey_key %<>% filter(before.2008 == (x < 2008))
  }
  
  dictionary <- setNames(survey_key$definition, paste0("^", survey_key$name, "$"))
  variables <- survey_key$definition
  file <- paste0(dir, 'Resultados', x, ".dbf")
  data <- read.dbf(file, as.is = TRUE) %>%
    as_tibble() %>%
    rename_with(~ str_to_lower(.x) %>% str_replace_all(dictionary)) %>%
    select(cod_mod, anexo, niv_mod, starts_with('cod_car'), ges_dep, matches('(^area$)|(^area_med$)'), cuadro, tipdato, matches("\\.(boys)|(girls)$")) %>%
    mutate(
      cuadro = chr2int(cuadro),
      across(!matches("(niv_mod)|(ges_dep)"), as.integer),
      across(where(is.character), clean_character))
  
  if (table == "preschool") {
    data %<>%
      filter(str_detect(niv_mod, "a")) %>%
      mutate(
        year = all_of(x),
        cuadro = case_when(
          year %in% c(2003:2007) & cuadro == 5100 ~ "preschool",
          year %in% c(2008:2009) & cuadro == 500 ~ "preschool",
          year %in% c(2010:2019) & cuadro == 101 ~ "preschool", #After 2015 is C101
          TRUE ~ NA_character_
        )
      ) %>%
      drop_na(cuadro) %>%
      mutate(
        tipdato = case_when(
          # Preschool
          year %in% c(2003:2009) & cuadro == "preschool" & tipdato == 1 ~ "passed",
          year %in% c(2003:2009) & cuadro == "preschool" & tipdato == 2 ~ "retired",
          year %in% c(2003:2009) & cuadro == "preschool" & tipdato == 3 ~ "dead",
          year %in% c(2010:2019) & cuadro == "preschool" & tipdato == 1 ~ "passed",
          year %in% c(2010:2019) & cuadro == "preschool" & tipdato == 2 ~ "retired",
          year %in% c(2010:2019) & cuadro == "preschool" & tipdato == 3 ~ "transferred",
          year %in% c(2010:2019) & cuadro == "preschool" & tipdato == 4 ~ "dead",
          TRUE ~ NA_character_
        )
      ) %>%
      drop_na(tipdato) %>%
      relocate(year, .before = 1)
  } else if (table == "school") {
    data %<>%
      filter(niv_mod %in% c('b0', 'f0')) %>%
      mutate(
        year = all_of(x),
        cuadro = case_when(
          year %in% c(2003:2007) & cuadro == 5100 ~ "enrollment",
          year %in% c(2003:2007) & cuadro == 5200 ~ "retirement",
          year %in% c(2008:2009) & cuadro == 501 ~ "enrollment",
          year %in% c(2008:2009) & cuadro == 502 ~ "retirement",
          year %in% c(2010:2019) & cuadro == 101 ~ "enrollment",
          year %in% c(2010:2019) & cuadro == 102 ~ "retirement",
          TRUE ~ NA_character_)
      ) %>%
      drop_na(cuadro) %>%
      mutate(
        tipdato = case_when(
          # Enrollment
          year %in% c(2003:2019) & cuadro == "enrollment" & tipdato == 1 ~ "passed", #For all years
          year %in% c(2010:2015) & cuadro == "enrollment" & tipdato == 2 ~ "passed",
          year %in% c(2016:2019) & cuadro == "enrollment" & between(tipdato, 2, 4) ~ "passed",
          
          year %in% c(2003:2008) & cuadro == "enrollment" & tipdato == 4 ~ "failed",
          year %in% c(2010:2015) & cuadro == "enrollment" & tipdato == 3 ~ "failed",
          year %in% c(2016:2019) & cuadro == "enrollment" & tipdato == 5 ~ "failed",
          
          year %in% c(2009) & cuadro == "enrollment" & tipdato == 7 ~ "transferred",
          year %in% c(2010:2015) & cuadro == "enrollment" & tipdato == 4 ~ "transferred",
          year %in% c(2016:2019) & cuadro == "enrollment" & tipdato == 6 ~ "transferred",
          
          year %in% c(2003:2008) & cuadro == "enrollment" & tipdato == 7 ~ "retired",
          year %in% c(2009) & cuadro == "enrollment" & tipdato == 8 ~ "retired",
          year %in% c(2010:2014) & cuadro == "enrollment" & tipdato == 5 ~ "retired",
          year %in% c(2015) & cuadro == "enrollment" & tipdato == 6 ~ "retired",
          year %in% c(2016:2019) & cuadro == "enrollment" & between(tipdato, 7, 8) ~ "retired",
          
          year %in% c(2003:2008) & cuadro == "enrollment" & tipdato == 8 ~ "dead",
          year %in% c(2009) & cuadro == "enrollment" & tipdato == 9 ~ "dead",
          year %in% c(2010:2014) & cuadro == "enrollment" & tipdato == 6 ~ "dead",
          year %in% c(2015) & cuadro == "enrollment" & tipdato == 7 ~ "dead",
          year %in% c(2016:2019) & cuadro == "enrollment" & tipdato == 9 ~ "dead",
          
          # Retirement
          year %in% c(2003) & cuadro == "retirement" & tipdato == 1 ~ "poverty",
          year %in% c(2003) & cuadro == "retirement" & tipdato == 2 ~ "sickness",
          year %in% c(2004) & cuadro == "retirement" & between(tipdato, 1, 3) ~ "poverty",
          year %in% c(2004) & cuadro == "retirement" & tipdato == 8 ~ "sickness",
          year %in% c(2005:2011) & cuadro == "retirement" & between(tipdato, 1, 3) ~ "poverty",
          year %in% c(2005:2011) & cuadro == "retirement" & tipdato == 5 ~ "sickness",
          year %in% c(2012) & cuadro == "retirement" & between(tipdato, 1, 2) ~ "poverty",
          year %in% c(2012) & cuadro == "retirement" & tipdato == 4 ~ "sickness",
          year %in% c(2013) & cuadro == "retirement" & tipdato %in% c(1, 4) ~ "poverty",
          year %in% c(2013) & cuadro == "retirement" & tipdato == 3 ~ "sickness",
          year %in% c(2014:2019) & cuadro == "retirement" & tipdato %in% c(1, 4, 5) ~ "poverty",
          year %in% c(2014:2019) & cuadro == "retirement" & tipdato == 3 ~ "sickness",
          cuadro == "retirement" ~ "other",
          TRUE ~ NA_character_)
      ) %>%
      drop_na(tipdato) %>%
      relocate(year, .before = 1)
  }
  return(data)
}

safe_integer <- function(x) {
  as.character(x) %>% as.integer()
}

safe_integer64 <- function(x) {
  as.character(x) %>% as.integer64()
}

safe_numeric <- function(x) {
  as.character(x) %>% as.numeric()
}

correct_ubigeo <- function(x) {
  x[x <= 999999999 & !is.na(x)] %<>% str_sub(1, 5) %>% as.integer()
  x[x >  999999999 & !is.na(x)] %<>% str_sub(1, 6) %>% as.integer()
  x
}

get_idccpp <- function(x) {
  x[x <= 999999999 & !is.na(x)] %<>% str_sub(6, 9) %>% as.integer()
  x[x >  999999999 & !is.na(x)] %<>% str_sub(7, 10) %>% as.integer()
  x
}

get_ubigeo <- function(x, n) {
x %>% str_pad(6, pad = "0") %>% str_sub(1, n)
}

count_answers.ise <- function(x) {
  answers <- names(x) %>% str_subset('(ise)|(education)')
  x[answers] %>% is.na() %>% `!` %>% sum() %>% as.integer()
}

run_ise <- function(data, parallelized = TRUE, ...) {
  data %<>% mutate(n_answers = apply(., 1, count_answers.ise))
  answer_threshold <- (max(data$n_answers)*0.65) %>% round(digits = 0)
  to_complete <- data %>% 
    filter(n_answers >= all_of(answer_threshold)) %>%
    mutate(across(matches('(^ise)|(education)'), as_factor))
  no_missings = to_complete$n_answers %>%`==`(max(to_complete$n_answers)) %>% all()
  if (no_missings == TRUE) {
    ise <- aggregate_ise(to_complete)
  }  else if (parallelized == TRUE) {
    model <- parmice(to_complete, ...)
    ise <- model %>% aggregate_ise()
  } else if (parallelized == FALSE) {
    model <- mice(to_complete, ...)
    ise <- model %>% aggregate_ise()
  }
  
  to_complete %<>%
    select(matches("(numero)|(periodo)")) %>%
    add_column(ise)
  return(list(data = to_complete, mice = model))
}

parmice <- function(data, m = 5, seed = NA, cluster.seed = NA, n.core = NULL,
                     n.imp.core = NULL, cl.type = "PSOCK", ...) {
  
  # check if data complete
  if (sum(is.na(data)) == 0) {
    stop("Data has no missing values")
  }
  
  # check if arguments match CPU specifications
  if (!is.null(n.core)) {
    if (n.core > parallel::detectCores()) {
      stop("Number of cores specified is greater than the number of logical cores in your CPU")
    }
  }
  
  # determine course of action when not all arguments specified
  if (!is.null(n.core) & is.null(n.imp.core)) {
    n.imp.core <- m
    warning(paste("Number of imputations per core not specified: n.imp.core = m =", m, "has been used"))
  }
  if (is.null(n.core) & !is.null(n.imp.core)) {
    n.core <- parallel::detectCores() - 1
    warning(paste("Number of cores not specified. Based on your machine a value of n.core =", parallel::detectCores() - 1, "is chosen"))
  }
  if (is.null(n.core) & is.null(n.imp.core)) {
    specs <- match.cluster(n.core = parallel::detectCores() - 1, m = m)
    n.core <- specs$cores
    n.imp.core <- specs$imps
  }
  if (!is.na(seed)) {
    if (n.core > 1) {
      warning("Be careful; the specified seed is equal for all imputations. Please consider specifying cluster.seed instead.")
    }
  }
  
  # create arguments to export to cluster
  args <- match.call(mice, expand.dots = TRUE)
  args[[1]] <- NULL
  args$m <- n.imp.core
  
  # make computing cluster
  cl <- parallel::makeCluster(n.core, type = cl.type)
  parallel::clusterExport(cl,
                          varlist = c(
                            "data", "m", "seed", "cluster.seed",
                            "n.core", "n.imp.core", "cl.type"
                          ),
                          envir = environment()
  )
  parallel::clusterExport(cl,
                          varlist = "to_complete",
                          envir = parent.frame()
  )
  parallel::clusterExport(cl,
                          varlist = "do.call"
  )
  parallel::clusterEvalQ(cl, library(mice))
  if (!is.na(cluster.seed)) {
    parallel::clusterSetRNGStream(cl, cluster.seed)
  }
  
  # generate imputations
  imps <- parallel::parLapply(cl = cl, X = 1:n.core, function(x) do.call(mice, as.list(args), envir = environment()))
  parallel::stopCluster(cl)
  
  # postprocess clustered imputation into a mids object
  imp <- imps[[1]]
  if (length(imps) > 1) {
    for (i in 2:length(imps)) {
      imp <- ibind(imp, imps[[i]])
    }
  }
  # let imputation matrix correspond to grand m
  for (i in 1:length(imp$imp)) {
    colnames(imp$imp[[i]]) <- 1:imp$m
  }
  
  imp
}

match.cluster <- function(n.core, m) {
  cores <- 1:n.core
  imps <- 1:m
  out <- data.frame(
    results = as.vector(cores %*% t(imps)),
    cores = cores,
    imps = rep(imps, each = n.core)
  )
  which <- out[out[, "results"] == m, ]
  which[order(which$cores, decreasing = T), ][1, 2:3]
}

ggsave_pdf <- function(file, ...) {
  ggsave(file = file, ...)
  extrafont::embed_fonts(file, outfile=file)
}

int_breaks <- function(x, n = 5) {
  l <- pretty(x, n)
  l[abs(l %% 1) < .Machine$double.eps ^ 0.5] 
}

boxplot_ci <- function(x) {
  ans <- boxplot.stats(x)
  data.frame(ymin = ans$conf[1], ymax = ans$conf[2], y = ans$stats[3])
}

compare_cols.itt <- function(treat, reg_vars, dataset) {
  formula <- paste(c(treat, "~ . -switcher_itt - numero"), collapse = '') %>% as.formula()
  dataset %<>% select(all_of(treat), switcher_itt, all_of(reg_vars))
  comparison <- compareGroups(
    formula, 
    data = dataset,
    method = c(puntaje = NA, household.size = NA),
  )
  alltab <- createTable(comparison, hide.no = "0", show.p.mul = TRUE)
  falsetab <- createTable(update(comparison, subset = switcher_itt == "0"), hide.no = "0", show.p.mul = TRUE)
  truetab <- createTable(update(comparison, subset = switcher_itt == "1"), hide.no = "0", show.p.mul = TRUE)
  cbind(`Whole Sample` = alltab, `Non-Switchers` = falsetab, `Switchers` = truetab)
}

compare_cols.ate <- function(treat, reg_vars, dataset) {
  formula <- paste(c(treat, "~ . -switcher_ate - numero"), collapse = '') %>% as.formula()
  dataset %<>% select(all_of(treat), switcher_ate, all_of(reg_vars))
  comparison <- compareGroups(
    formula, 
    data = dataset,
    method = NA,
  )
  alltab <- createTable(comparison, hide.no = "0", show.p.mul = TRUE)
  falsetab <- createTable(update(comparison, subset = switcher_ate == "0"), hide.no = "0", show.p.mul = TRUE)
  truetab <- createTable(update(comparison, subset = switcher_ate == "1"), hide.no = "0", show.p.mul = TRUE)
  cbind(`Whole Sample` = alltab, `Non-Switchers` = falsetab, `Switchers` = truetab)
}

pscore_reg.itt <- function(data, reg_vars, bayesian = TRUE) {
  
   data %<>%
    mutate(weight_fe = as.numeric(preschool_itt) - 1) %>%
    group_by(numero) %>%
    mutate(weight_fe = (weight_fe - mean(weight_fe))^2 %>% mean()) %>%
    drop_na(!!!reg_vars, preschool_itt)
  
  if (bayesian == TRUE) {
    q_x <- multinom(preschool_itt ~ . - numero,
                     data = data %>% dplyr::select(preschool_itt, all_of(reg_vars)),
                     na.action = "na.pass"
    )
    p_x <- multinom(switcher_itt ~ . - numero,
                     data = data %>% dplyr::select(switcher_itt, all_of(reg_vars)),
                     na.action = "na.pass"
    )
  } else {
    q_x <- arm::bayesglm(preschool_itt ~ . - numero,
                          family = binomial(link = "logit"),
                          data = data %>% dplyr::select(preschool_itt, all_of(reg_vars)),
                          na.action = "na.pass"
    )
    p_x <- arm::bayesglm(switcher_itt ~ . - numero,
                          family = binomial(link = "logit"),
                          data = data %>% dplyr::select(switcher_itt, all_of(reg_vars)),
                          na.action = "na.pass"
    )
  }

  data %<>% 
    add_column(ratio = (fitted(q_x)/fitted(p_x))) %>%
    ungroup() %>%
    mutate(
      weight_fe = (ratio/weight_fe)*(mean(as.numeric(switcher_itt)-1)/mean(as.numeric(preschool_itt)-1))
    ) %>%
    filter(weight_fe != Inf) %>%
    mutate(
      math = (m500_m - 500)/100,
      lang = (m500_l - 500)/100
    ) %>%
    mutate(preschool = as.integer(preschool_itt) - 1) %>%
    select(-matches("preschool_"))
  
  return(data)
}

pscore_reg.ate <- function(data, reg_vars, bayesian = TRUE) {
  
  data %<>%
    mutate(weight_fe = as.numeric(preschool_ate) - 1) %>%
    group_by(numero) %>%
    mutate(weight_fe = (weight_fe - mean(weight_fe))^2 %>% mean()) %>%
    drop_na(!!!reg_vars, preschool_ate)
  
  if (bayesian == TRUE) {
    q_x <- multinom(preschool_ate ~ . - numero,
                    data = data %>% dplyr::select(preschool_ate, all_of(reg_vars)),
                    na.action = "na.pass"
    )
    p_x <- multinom(switcher_ate ~ . - numero,
                    data = data %>% dplyr::select(switcher_ate, all_of(reg_vars)),
                    na.action = "na.pass"
    )
  } else {
    q_x <- arm::bayesglm(preschool_ate ~ . - numero,
                         family = binomial(link = "logit"),
                         data = data %>% dplyr::select(preschool_ate, all_of(reg_vars)),
                         na.action = "na.pass"
    )
    p_x <- arm::bayesglm(switcher_ate ~ . - numero,
                         family = binomial(link = "logit"),
                         data = data %>% dplyr::select(switcher_ate, all_of(reg_vars)),
                         na.action = "na.pass"
    )
  }
  
  data %<>% 
    add_column(ratio = (fitted(q_x)/fitted(p_x))) %>%
    ungroup() %>%
    mutate(
      weight_fe = (ratio/weight_fe)*(mean(as.numeric(switcher_ate)-1)/mean(as.numeric(preschool_ate)-1))
    ) %>%
    filter(weight_fe != Inf) %>%
    mutate(
      math = (m500_m - 500)/100,
      lang = (m500_l - 500)/100
    ) %>%
    mutate(preschool = as.integer(preschool_ate) - 1) %>%
    select(-matches("preschool_"))
  
  return(data)
}