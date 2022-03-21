### Set up parallelization
plan(multisession, workers = 3)

### Read tables
dataset_ses <- readRDS("dataset_ses.rds")
dataset_fos <- readRDS("dataset_fos.rds")
dataset_sis <- readRDS("dataset_sis.rds")

### Compare groups
covariates <- c(
  "numero", "mujer", "cohort", "first_born", "pyear_check", "estatal", "unidocente", # Individual level
  "puntaje", "rural", "household.size", "nospanish", "welfare", "nospanish", "healthcare", "uniparental"  # Family-level
  )

balance_itt <- list(dataset_ses, dataset_fos, dataset_sis) %>%
  future_map(~ compare_cols.itt("preschool_itt", covariates, dataset = .))

saveRDS(balance_itt, file = "balance_itt.rds")

balance_att <- list(dataset_ses, dataset_fos, dataset_sis) %>%
  future_map(~ compare_cols.ate("preschool_ate", covariates, .))

saveRDS(balance_ate, file = "balance_ate.rds")

### REGRESSIONS
regression_variables <- c("numero", "puntaje", "rural", "household.size", "nospanish", "welfare", "nospanish", "healthcare", "uniparental") # Family-level
individual_variables <- c("mujer", "cohort", "first_born", "pyear_check", "estatal", "unidocente")

# Formulas
math_formula <- paste(c("math ~ preschool", paste0("preschool:", individual_variables)), collapse = "+") %>% as.formula()
lang_formula <- paste(c("lang ~ preschool", paste0("preschool:", individual_variables)), collapse = "+") %>% as.formula()

ps_ses.itt <- pscore_reg.itt(dataset_ses, regression_variables)
ps_ses.ate <- pscore_reg.ate(dataset_ses, regression_variables)

itt_ses <- list(math_formula, lang_formula) %>%
  future_map(~ lm_robust(.x, data = ps_ses.itt, clusters = numero, fixed_effects = ~numero, weights = weight_fe, se_type = "stata"))
ate_ses <- list(math_formula, lang_formula) %>%
  future_map(~ lm_robust(.x, data = ps_ses.ate, clusters = numero, fixed_effects = ~numero, weights = weight_fe, se_type = "stata"))

modelsummary(c(itt_ses, ate_ses),  gof_omit = "R2", statistic = c("conf.int", "s.e. = {std.error}", "p value = {p.value}"), conf_level = 0.95, output = "latex")

ps_fos.itt <- pscore_reg.itt(dataset_fos, regression_variables)
ps_fos.ate <- pscore_reg.ate(dataset_fos, regression_variables)

itt_fos <- list(math_formula, lang_formula) %>%
  future_map(~ lm_robust(.x, data = ps_fos.itt, clusters = numero, fixed_effects = ~numero, weights = weight_fe, se_type = "stata"))
ate_fos <- list(math_formula, lang_formula) %>%
  future_map(~ lm_robust(.x, data = ps_fos.ate, clusters = numero, fixed_effects = ~numero, weights = weight_fe, se_type = "stata"))

modelsummary(c(itt_fos, ate_fos), output = "latex")

ps_sis.itt <- pscore_reg.itt(dataset_sis, regression_variables)
ps_sis.ate <- pscore_reg.ate(dataset_sis, regression_variables)

itt_sis <- list(math_formula, lang_formula) %>%
  future_map(~ lm_robust(.x, data = ps_sis.itt, clusters = numero, fixed_effects = ~numero, weights = weight_fe, se_type = "stata"))
ate_sis <- list(math_formula, lang_formula) %>%
  future_map(~ lm_robust(.x, data = ps_sis.ate, clusters = numero, fixed_effects = ~numero, weights = weight_fe, se_type = "stata"))

modelsummary(c(itt_sis, ate_sis), output = "latex")

save.image(file = "estimations.RData")