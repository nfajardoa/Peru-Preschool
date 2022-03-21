### FAMILY LOCATION HISTORY...
# I assume that households do not change the observed 2012/2013 composition. That is, siblings will always share the same roof.
# I proxy their year by year location using the school-cluster of tests.

families <- tbl(db_conn, "student_id") %>%
  left_join(
    by = "id_sisfoh",
    tbl(db_conn, "sisfoh") %>%
      rename_with(translate_variables, names_dictionary = variable_dictionary) %>%
      select(id_sisfoh, numero, relationship_hh, civilstatus) %>%
      mutate(both_parents = if_else(relationship_hh == 1 & (civilstatus == 2 | civilstatus == 3), 1L, 0L)) %>%
      group_by(numero) %>%
      mutate(both_parents = sum(both_parents))
  ) %>%
  left_join(
    tbl(db_conn, "ece") %>%
      left_join(tbl(db_conn, "school_cluster")) %>%
      select(id_ece, periodo, cluster, rural)
  ) %>%
  select(numero, periodo, cluster, rural, both_parents) %>%
  group_by(numero) %>%
  arrange(numero) %>%
  distinct() %>%
  full_join(tbl(db_conn, "clean_id") %>%
    select(numero, pyear) %>%
    rename(periodo = pyear) %>%
    distinct()) %>%
  group_by(numero, periodo) %>%
  arrange(numero, periodo) %>%
  collect() %>%
  summarize(
    cluster = calculate_mode(cluster, na.rm = TRUE, fun = "c"),
    rural = calculate_mode(rural, na.rm = TRUE, fun = "c"),
    uniparental = sum(both_parents) %>% `==`(0),
    .groups = "drop"
  ) %>%
  group_by(numero) %>%
  fill(cluster, rural, uniparental, .direction = "downup")

dbWriteTable(db_conn, 'families', families, overwrite = TRUE)
