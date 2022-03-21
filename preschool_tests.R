### STANDARIZED TESTS
# Plots per year
exams <- tbl(db_conn, 'ece') %>%
  select(mc, tipo_eva, periodo, m500_m, m500_l, aj_l, aj_m) %>%
  # mutate(
  #   #aj_m = if_else(is.na(aj_m) & mc == FALSE, 1, aj_m),
  #   math = if_else(periodo == 2007, m500_m * aj_m, m500_m),
  #   #aj_l = if_else(is.na(aj_l) & mc == FALSE, 1, aj_l),
  #   lang = if_else(periodo == 2007, m500_l * aj_l, m500_l)
  # ) %>%
  group_by(periodo) %>%
  summarise(
    math = mean(m500_m),
    lang = mean(m500_l),
    ) %>%
  collect()

exams_summary <- read_excel('ECE/Otros/ECE2P_summary.xlsx')

exams <- tbl(db_conn, 'ece') %>%
  select(mc, tipo_eva, periodo, m500_m, m500_l, aj_l, aj_m) %>%
  # mutate(
  #   aj_m = if_else(is.na(aj_m) & mc == FALSE, 1, aj_m),
  #   math = m500_m * aj_m,
  #   aj_l = if_else(is.na(aj_l) & mc == FALSE, 1, aj_l),
  #   lang = m500_l * aj_l
  # ) %>%
  group_by(periodo) %>%
  summarise(math = mean(m500_m, na.rm = TRUE), lang = mean(m500_l, na.rm = TRUE)) %>%
  collect()

ggplot(exams) + geom_histogram(aes(x = m500_l), bins = 100) + facet_grid(rows = vars(tipo_eva)) 
ggplot(exams) + geom_density(aes(x = m500_l, group = as.factor(periodo), color = as.factor(periodo))) + facet_grid(cols = vars(mc)) 
ggplot(exams) + geom_density(aes(x = m500_l, group = as.factor(periodo), color = as.factor(periodo)))


ggplot(exams) + geom_histogram(aes(x = m500_m)) + facet_grid(cols = vars(mc)) 

exams %<>%
  mutate(
    aj_m = if_else(is.na(aj_m), 1, aj_m),
    final_m = m500_m * aj_m,
    aj_l = if_else(is.na(aj_l), 1, aj_l),
    final_l = m500_l * aj_l
    )

summary((exams$m500_m*exams$aj_m)[exams$mc == TRUE])
summary((exams$m500_m*exams$aj_m)[exams$mc == FALSE])

summary(exams$m500_m*exams$aj_m)
summary(exams$m500_l[exams$mc == TRUE])
