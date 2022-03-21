### Read tables
dataset_ses <- readRDS("dataset_ses.rds")
dataset_fos <- readRDS("dataset_fos.rds")
dataset_sis <- readRDS("dataset_sis.rds")

### Compare groups
regression_variables <- c("numero", "mujer", "cohort", "first_born", "pyear_check", "estatal", "unidocente", #Individual level
                          "puntaje", "rural", "household.size", "nospanish", "welfare", "nospanish", "healthcare", "uniparental") #Family-level

treatment_variables <- c("preschool_itt", "preschool_ate", "switcher_itt.corr", "switcher_ate.corr")

outcome_variables <- c("m500_l", "m500_m")

balance_itt <- list(dataset_ses, dataset_fos, dataset_sis) %>%
  map(~ compare_cols.itt("preschool_itt", regression_variables, dataset = .))

saveRDS(balance_itt, file = "balance_itt.rds")

balance_itt %>% map(export2latex)

balance_att <- list(dataset_ses, dataset_fos, dataset_sis) %>%
  map(~ compare_cols.ate("preschool_ate", regression_variables, .))

### REGRESSIONS

regression_variables <- c("numero", "puntaje", "rural", "household.size", "nospanish", "welfare", "nospanish", "healthcare", "uniparental") #Family-level
individual_variables <- c("")

dataset_sis <- readRDS("dataset_sis.rds")

test <- pscore_reg.itt(dataset_sis, regression_variables)                                
lm_robust(math ~ preschool_itt, data = test, clusters = numero, fixed_effects = ~numero, weights = weight_fe, se_type = "stata")

test2 <- pscore_reg.itt(dataset_sis, regression_variables, bayesian = FALSE)                                
lm_robust(lang ~ preschool_itt, data = test2, clusters = numero, fixed_effects = ~numero, weights = weight_fe, se_type = "stata")

test <- pscore_reg.ate(dataset_sis, regression_variables)                                
lm_robust(lang ~ preschool_ate, data = test, clusters = numero, fixed_effects = ~numero, weights = weight_fe, se_type = "stata")

test <- pscore_reg.ate(dataset_sis, regression_variables)                                
lm_robust(math ~ preschool_ate, data = test, clusters = numero, fixed_effects = ~numero, weights = weight_fe, se_type = "stata")



covariates <- subset(dataset_st, select = c(first_born, household.size, welfare, healthcare, spanish, estatal, unidocente, rural, uniparental))
balance_raw.st <- bal.tab(preschool_itt ~ covariates, data = dataset_st, s.d.denom = "pooled", quick = TRUE)
balance_raw.st %>% summary



treatment <- tbl(db_conn, 'treatment') %>%
  filter(pyear >= 2000) %>%
  mutate(preschool = preschool > 0) %>%
  left_join(tbl(db_conn, 'ece') %>% select(id_ece, tipo_eva, periodo, m500_l, m500_m)) %>%
  group_by(numero) %>%
  mutate(n.siblings = n()) %>%
  group_by(match_id) %>%
  mutate(child = n()) %>%
  filter(n.siblings > child) %>%
  arrange(numero, match_id, periodo) %>%
  collect()

results <- treatment %>% 
  mutate(preschool = mean(preschool, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    age = periodo - year(fnac),
    section_a = if_else(is.na(section_a), FALSE, section_a),
    robust = if_else(is.nan(preschool), FALSE, TRUE),
    preschool = if_else(preschool > 0.5, TRUE, FALSE), 
    preschool = if_else(robust == FALSE, section_a, preschool)
    ) %>%
  group_by(numero) %>%
  mutate(
    treatment.siblings = mean(preschool),
    treatment.preschool = mean(section_a),
  )

results$pyear <- cut(results$pyear,
                     include.lowest = TRUE,
                     right = TRUE,
                     dig.lab = 1,
                     breaks = c(2000, 2008, 2012, 2018)
)

plot_data <- results %>%
  group_by(tipo_eva, preschool, pyear) %>%
  summarise(
    math_mn = mean(m500_m, na.rm = TRUE), lang_mn = mean(m500_l, na.rm = TRUE), 
    math_up = math_mn + sd(m500_m, na.rm = TRUE), math_lw = math_mn - sd(m500_m, na.rm = TRUE), 
    lang_up = lang_mn + sd(m500_l, na.rm = TRUE), lang_lw = lang_mn - sd(m500_l, na.rm = TRUE)
    ) %>%
  mutate(
    tipo_eva = case_when(
      tipo_eva == 1 ~ '8 y/o',
      tipo_eva == 2 ~ '10 y/o',
      tipo_eva == 3 ~ '14 y/o',
    ) %>% as_factor()
  ) %>%
  pivot_longer(cols = matches('(math)|(lang)'), names_pattern = "(.*)_(..)$", names_to = c("test", "name")) %>% 
  pivot_wider(id_cols = c(tipo_eva, preschool, pyear, test), names_from = name, values_from = value)

ggplot(plot_data) + geom_point(aes(x = tipo_eva, y = mn, color = preschool, group = preschool)) +
  geom_errorbar(aes(x = tipo_eva, ymin=lw, ymax=up, color = preschool)) +
  facet_grid(cols = vars(pyear), rows = vars(test)) + 
  theme_thesis() + labs(x = 'Test', y = 'Math score', color = 'Preschool')
  
final <- results %>%
  filter(
    (treatment.siblings > 0 & treatment.preschool > 0) | (treatment.siblings == 0 & treatment.preschool == 0),
    treatment.siblings < 1, treatment.preschool < 1
  ) %>%
  arrange(numero, pyear) %>%
  mutate(
    older = if_else(row_number() == 1, !preschool, NA),
    constant = xor(preschool, dplyr::lag(preschool, 1)),
    constant = sum(constant, na.rm = TRUE) %>% `<=`(1)
    ) %>%
  fill(older, .direction = 'down') %>%
  filter(older == TRUE, constant == TRUE)

plot_data2 <- final %>%
  group_by(tipo_eva, preschool, pyear) %>%
  summarise(
    math_mn = mean(m500_m, na.rm = TRUE), lang_mn = mean(m500_l, na.rm = TRUE), 
    math_up = math_mn + sd(m500_m, na.rm = TRUE), math_lw = math_mn - sd(m500_m, na.rm = TRUE), 
    lang_up = lang_mn + sd(m500_l, na.rm = TRUE), lang_lw = lang_mn - sd(m500_l, na.rm = TRUE)
  ) %>%
  mutate(
    tipo_eva = case_when(
      tipo_eva == 1 ~ '8 y/o',
      tipo_eva == 2 ~ '10 y/o',
      tipo_eva == 3 ~ '14 y/o',
    ) %>% as_factor()
  ) %>%
  pivot_longer(cols = matches('(math)|(lang)'), names_pattern = "(.*)_(..)$", names_to = c("test", "name")) %>% 
  pivot_wider(id_cols = c(tipo_eva, preschool, pyear, test), names_from = name, values_from = value)

ggplot(plot_data2) + geom_point(aes(x = tipo_eva, y = mn, color = preschool, group = preschool)) +
  geom_errorbar(aes(x = tipo_eva, ymin=lw, ymax=up, color = preschool)) +
  facet_grid(cols = vars(pyear), rows = vars(test)) + 
  theme_thesis() + labs(x = 'Test', y = 'Math score', color = 'Preschool')

regression <- final %>%
  mutate(
    #pyear = if_else(preschool == FALSE, 0, as.numeric(pyear)),
    preschool = as.numeric(preschool)
    ) %>%
  select(match_id, numero, pyear, tipo_eva, preschool, mujer, age, m500_l, m500_m) %>%
  group_by(match_id, tipo_eva) %>%
  mutate(
    across(matches('m500'), mean, na.rm = TRUE)
  ) %>%
  filter(row_number() == 1)

plot_data3 <- regression %>%
  group_by(tipo_eva, preschool, pyear, mujer) %>%
  summarise(
    math_mn = mean(m500_m, na.rm = TRUE), lang_mn = mean(m500_l, na.rm = TRUE), 
    math_up = math_mn + sd(m500_m, na.rm = TRUE), math_lw = math_mn - sd(m500_m, na.rm = TRUE), 
    lang_up = lang_mn + sd(m500_l, na.rm = TRUE), lang_lw = lang_mn - sd(m500_l, na.rm = TRUE)
  ) %>%
  mutate(
    tipo_eva = case_when(
      tipo_eva == 1 ~ '8 y/o',
      tipo_eva == 2 ~ '10 y/o',
      tipo_eva == 3 ~ '14 y/o',
    ) %>% as_factor()
  ) %>%
  pivot_longer(cols = matches('(math)|(lang)'), names_pattern = "(.*)_(..)$", names_to = c("test", "name")) %>% 
  pivot_wider(id_cols = c(tipo_eva, preschool, pyear, test, mujer), names_from = name, values_from = value) %>%
  mutate(
    pyear = as_factor(pyear),
    preschool = as_factor(preschool)
  )

plot_data3$trt_low <- as.numeric(plot_data3$tipo_eva) - 0.1
plot_data3$trt_high <- as.numeric(plot_data3$tipo_eva) + 0.1
limits_low <- aes(y = lw, yend = lw, x = trt_low, xend = trt_high, color = preschool)
limits_high <- aes(y = up, yend = up, x = trt_low, xend = trt_high, color = preschool)

ggplot(plot_data3) + geom_point(aes(x = tipo_eva, y = mn, color = preschool, group = preschool)) +
  geom_segment(limits_low) + geom_segment(limits_high) +
  facet_grid(cols = vars(test), rows = vars(mujer)) + 
  theme_thesis() + labs(x = 'Test', y = 'Score', color = 'Preschool')

reg_m = feols(m500_m ~ preschool + mujer + age | pyear + numero + tipo_eva, regression)
reg_l = feols(m500_l ~ preschool + mujer + age | pyear + numero + tipo_eva, regression)
summary(reg_m, se = 'threeway')
summary(reg_l, se = 'threeway')

reg_m1 = feols(m500_m ~ preschool + mujer + age | pyear + numero, regression %>% filter(tipo_eva == 1))
reg_l1 = feols(m500_l ~ preschool + mujer + age | pyear + numero, regression %>% filter(tipo_eva == 1))
summary(reg_m, se = 'twoway')
summary(reg_l, se = 'twoway')

reg_m2 = feols(m500_m ~ preschool  + mujer + age | pyear + numero, regression %>% filter(tipo_eva == 2))
reg_l2 = feols(m500_l ~ preschool  + mujer + age | pyear + numero, regression %>% filter(tipo_eva == 2))
summary(reg_m, se = 'twoway')
summary(reg_l, se = 'twoway')

reg_m3 = feols(m500_m ~ preschool| pyear + numero, regression %>% filter(tipo_eva == 3))
reg_l3 = feols(m500_l ~ preschool| pyear + numero, regression %>% filter(tipo_eva == 3))
summary(reg_m, se = 'twoway')
summary(reg_l, se = 'twoway')

reg_ma = feols(m500_m ~ preschool | pyear + numero + tipo_eva, regression)
reg_la = feols(m500_l ~ preschool | pyear + numero + tipo_eva, regression)
summary(reg_m, se = 'threeway')
summary(reg_l, se = 'threeway')

reg_ma1 = feols(m500_m ~ preschool | pyear + numero, regression %>% filter(tipo_eva == 1))
reg_la1 = feols(m500_l ~ preschool | pyear + numero, regression %>% filter(tipo_eva == 1))
summary(reg_m, se = 'twoway')
summary(reg_l, se = 'twoway')

reg_ma2 = feols(m500_m ~ preschool  | pyear + numero, regression %>% filter(tipo_eva == 2))
reg_la2 = feols(m500_l ~ preschool  | pyear + numero, regression %>% filter(tipo_eva == 2))
summary(reg_m, se = 'twoway')
summary(reg_l, se = 'twoway')

reg_ma3 = feols(m500_m ~ preschool | pyear + numero, regression %>% filter(tipo_eva == 3))
reg_la3 = feols(m500_l ~ preschool | pyear + numero, regression %>% filter(tipo_eva == 3))
summary(reg_m, se = 'twoway')
summary(reg_l, se = 'twoway')

etable(reg_ma, reg_m, reg_ma1, reg_m1, reg_ma2, reg_m2, reg_ma3, reg_m3, tex = TRUE)
etable(reg_la, reg_l, reg_la1, reg_l1, reg_la2, reg_l2, reg_la3, reg_l3, tex = TRUE)

regression2 <- regression %>%
  mutate(pyear = if_else(preschool == FALSE, 0, as.numeric(pyear)))

attgt <- att_gt(
  yname = "m500_m",
  tname = "tipo_eva",
  idname = "match_id",
  gname = "preschool",
  xformla = ~1,
  data = regression2
)

static <- did_imputation(
  data = regression, 
  yname = "m500_m", 
  gname = "preschool", 
  tname = "tipo_eva", 
  idname = "match_id",
  first_stage = ~ 0 | pyear + numero
  )






regression <- final %>%
  arrange(periodo, .by_group = TRUE) %>%
  mutate(
    first.treated = xor(preschool, dplyr::lag(preschool, 1)),
    first.treated = if_else(first.treated == TRUE, periodo, NA_integer_),
    first.treated = dplyr::first(na.omit(first.treated))
    ) %>%
  fill(first.treated, .direction = 'downup') %>%
  replace_na(first.treated, value = 0L) %>%
  filter(preschool == TRUE | (periodo < first.treated | first.treated == 0L)) %>%
  group_by(numero, periodo) %>%
  summarise(
    across(matches('(m500)|(first.treated)'), mean),
    ) %>%
  mutate(
    preschool = (first.treated <= periodo) & (first.treated != 0)
  )

lm(m500_l ~ preschool, data = regression) %>% summary
lm(m500_m ~ preschool, data = regression) %>% summary
plm(m500_m ~ preschool, data = regression, index = c("numero", "periodo"), model = "within", effect = "twoways")
feols(m500_m ~ preschool | numero + periodo, regression)
feols(m500_l ~ preschool | numero + periodo, regression)

prueba.de.dios <- att_gt(yname = "m500_m", 
       tname = "periodo", 
       idname = "numero",
       gname = "first.treated", 
       xformla = ~1,
       data = regression,
       allow_unbalanced_panel = TRUE
       )

ggdid(prueba.de.dios)
aleph <- aggte(prueba.de.dios, type = "simple")

table(regression$first.treated)


%>%
  pivot_longer(cols = c(m500_l, m500_m), names_to = 'area', values_to = "score") %>%
  pivot_wider(id_cols = c(numero, area), names_from = periodo, values_from = score, values_fn = mean)







results <- tbl(db_conn, 'matriculation') %>%
  inner_join(tbl(db_conn, 'test_scores')) %>%
  left_join(tbl(db_conn, 'ece_treatment')) %>%
  inner_join(tbl(db_conn, 'student_id')) %>%
  filter(valid_fnac <= 3) %>%
  select(cod_mod, anexo, periodo, grado, seccion, student_id, matriculation_id, mujer, pyear, preschool, starts_with('grupo'), starts_with('m500'), starts_with('aj')) %>%
  left_join(tbl(db_conn, 'school_cluster') %>% select(cod_mod, anexo, cluster)) %>%
  left_join(tbl(db_conn, 'education_supply') %>% select(cluster, pyear, section_A)) %>%
  left_join(tbl(db_conn, 'ccpp') %>% select(cluster, rural, categoria)) %>%
  left_join(tbl(db_conn, 'families')) %>%
  left_join(tbl(db_conn, 'conversion_ccpp'), by = c('idccpp'), suffix = c('.school', '.family')) %>%
  collect()

test <- results %>%
  group_by(family_id) %>%
  filter(preschool %>% sum() < n(), c(student_id) %>% unique() %>% length() %>% `>`(1))

summary_tests_l <- test %>%
  group_by(preschool) %>%
  summarise(m500_l = summary(m500_l) %>% list())

summary_tests_l
summary_tests_l$m500_l

summary_tests_m <- test %>%
  group_by(preschool) %>%
  summarise(m500_m = summary(m500_m) %>% list())

summary_tests_m
summary_tests_m$m500_m

summary2_tests_l <- test %>%
  group_by(preschool, rural) %>%
  summarise(m500_l = summary(m500_l) %>% list())

summary2_tests_l
summary2_tests_l$m500_l

summary2_tests_m <- test %>%
  group_by(preschool, rural) %>%
  summarise(m500_m = summary(m500_m) %>% list())

summary2_tests_m
summary2_tests_m$m500_m

ggplot(test) + geom_histogram(aes(x=m500_l)) + facet_grid(rows = vars(preschool))
ggplot(test) + geom_histogram(aes(x=m500_m)) + facet_grid(rows = vars(preschool))

ggplot(test) + geom_histogram(aes(x=m500_l)) + facet_grid(cols = vars(periodo), rows = vars(preschool))
ggplot(test) + geom_histogram(aes(x=m500_m)) + facet_grid(cols = vars(rural), rows = vars(preschool))

write_csv(regression, 'preliminary.csv')




