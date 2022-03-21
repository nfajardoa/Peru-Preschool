
# If upper space is too few add plot.margin = grid::unit(c(1,0,0,0),"mm") 

setwd("Documents/BRIQ/Stephanie/")
source("~/Documents/BRIQ/Stephanie/preschool_functions.R")

packages <- c(
  "DBI", "extrafont", "dbplyr", "readxl", "ggthemes", "scales", "ggforce", "ggpubr",
  "purrr", "dtplyr", "furrr", "dplyr", "tidyverse", "magrittr", "stargazer", "ggbrace",
  "sf", "viridis", "statar"
)

# Load packages
invisible(lapply(packages, library, character.only = TRUE))
extrafont::loadfonts(device = "pdf", quiet = TRUE)

### Database
db_conn <- dbConnect(RPostgres::Postgres(),
  dbname = "peru", host = "localhost",
  port = 5432, user = "rwriter", password = "12345"
)

### Colors
colors <- c(
  "#e66101", "#5e3c99", "#fdb863", "#b2abd2",
  "#76BED0", "#F7CB15", "#F55D3E", "#878E88",
  "#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c",
  "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#b15928", "#ffff99"
)

### Plots

enrollment_wb <- read_excel("plots/enrollment_WB.xlsx", na = "NA", col_types = "numeric") %>%
  pivot_longer(cols = matches("(gross)|(net)"), names_pattern = "(.*)_(.*)", names_to = c("series", "type"))

ggsave_pdf(
  file = "plots/enrollment_wb.pdf",
  plot =
    ggplot(data = enrollment_wb) +
      geom_line(aes(x = year, y = value, group = type, color = type)) +
      facet_grid(cols = vars(series)) +
      scale_y_continuous(
        breaks = breaks_width(width = 10),
        expand = c(0, 0),
        labels = number_format(accuracy = 1, big.mark = ""),
        sec.axis = dup_axis(breaks = extended_range_breaks())
      ) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_color_manual(values = colors[1:2]) +
      labs(y = "Percentage points (%)", color = "") +
      theme_thesis() +
      theme(axis.title.y.right = element_blank(), axis.title.x = element_blank(), legend.position = "bottom"),
  device = "pdf", height = 60, width = 150, units = "mm"
)

### OVERALL
ece <- tbl(db_conn, "ece") %>%
  select(tipo_eva, periodo, rural, estatal, mujer, unidocente, m500_l, m500_m) %>%
  mutate(
    tipo_eva = case_when(
      tipo_eva == 1 ~ "3rd Stage",
      tipo_eva == 2 ~ "4th Stage",
      tipo_eva == 3 ~ "6th Stage"
    )
  ) %>%
  arrange(tipo_eva, periodo) %>%
  collect() %>%
  mutate(
    tipo_eva = as_factor(tipo_eva),
    rural_periodo = interaction(rural, as_factor(periodo)),
    mujer_periodo = interaction(mujer, as_factor(periodo)),
    unidocente_periodo = interaction(unidocente, as_factor(periodo)),
    estatal_periodo = interaction(estatal, as_factor(periodo)),
  )

ggsave(
  file = "plots/ece_summary_lang.png",
  plot =
    ggplot(data = ece, aes(x = periodo, y = m500_l, group = periodo)) +
      stat_boxplot(geom = "errorbar", color = "#141414", lwd = 0.15, width = 0.25) +
      stat_boxplot(
        aes(ymin = ..lower.., ymax = ..upper..),
        lwd = 0.25,
        fill = colors[5],
        color = "white",
        outlier.color = colors[5],
        outlier.fill = colors[5],
        outlier.size = 0.01,
        outlier.alpha = 0.01,
        outlier.shape = 19,
      ) +
      facet_grid(~tipo_eva, scales = "free_x") +
      scale_y_continuous(
        breaks = breaks_width(width = 100),
        expand = c(0, 0),
        labels = number_format(accuracy = 1, big.mark = ""),
        sec.axis = dup_axis(breaks = extended_range_breaks())
      ) +
      scale_x_continuous(
        expand = c(0.01, 0.01),
        breaks = int_breaks,
        labels = number_format(accuracy = 1, big.mark = "")
      ) +
      labs(y = "Raw Score (Language)") +
      theme_thesis() +
      theme(axis.title.y.right = element_blank(), axis.title.x = element_blank(), legend.position = "bottom"),
  device = "png", height = 60, width = 150, units = "mm", dpi = "retina", bg = "white"
)

ggsave(
  file = "plots/ece_summary_math.png",
  plot =
    ggplot(data = ece, aes(x = periodo, y = m500_m, group = periodo)) +
      stat_boxplot(geom = "errorbar", color = "#141414", lwd = 0.15, width = 0.25) +
      stat_boxplot(
        aes(ymin = ..lower.., ymax = ..upper..),
        lwd = 0.25,
        fill = colors[5],
        color = "white",
        outlier.color = colors[5],
        outlier.fill = colors[5],
        outlier.size = 0.01,
        outlier.alpha = 0.01,
        outlier.shape = 19,
      ) +
      facet_grid(~tipo_eva, scales = "free_x") +
      scale_y_continuous(
        breaks = breaks_width(width = 100),
        expand = c(0, 0),
        labels = number_format(accuracy = 1, big.mark = ""),
        sec.axis = dup_axis(breaks = extended_range_breaks())
      ) +
      scale_x_continuous(
        expand = c(0.01, 0.01),
        breaks = int_breaks,
        labels = number_format(accuracy = 1, big.mark = "")
      ) +
      labs(y = "Raw Score (Mathematics)") +
      theme_thesis() +
      theme(axis.title.y.right = element_blank(), axis.title.x = element_blank(), legend.position = "bottom", strip.text.x = element_blank()),
  device = "png", height = 55, width = 150, units = "mm", dpi = "retina", bg = "white"
)

ggsave(
  file = "plots/ece_summary_obs.png",
  plot =
    ggplot(data = ece %>%
      group_by(tipo_eva, periodo) %>%
      count(name = "obs") %>%
      mutate(obs = obs / 1000)) +
      geom_col(aes(x = periodo, y = obs), fill = colors[5]) +
      coord_cartesian(ylim = c(450, 550)) +
      facet_grid(~tipo_eva, scales = "free_x") +
      scale_y_continuous(
        breaks = breaks_width(width = 25),
        expand = c(0, 0),
        labels = number_format(accuracy = 3, big.mark = ""),
        sec.axis = dup_axis(breaks = extended_range_breaks())
      ) +
      scale_x_continuous(
        expand = c(0, 0),
        breaks = int_breaks,
        labels = number_format(accuracy = 1, big.mark = "")
      ) +
      labs(y = "N. of Obs. (thous.)") +
      theme_thesis() +
      theme(axis.title.y.right = element_blank(), axis.title.x = element_blank(), legend.position = "bottom", strip.text.x = element_blank()),
  device = "png", height = 25, width = 150, units = "mm", dpi = "retina", bg = "white"
)

ggsave(
  file = "plots/ece_rural_lang_3rd.png",
  plot =
    ggplot(data = ece %>% filter(tipo_eva == "3rd Stage")) +
      geom_boxplot(aes(x = as_factor(periodo), y = m500_l, group = rural_periodo, fill = rural),
        lwd = 0.15,
        color = rgb(20, 20, 20, maxColorValue = 255),
        outlier.size = 0.01,
        outlier.alpha = 0.1
      ) +
      facet_grid(~tipo_eva, scales = "free") +
      scale_y_continuous(
        breaks = breaks_width(width = 100),
        expand = c(0, 0),
        labels = number_format(accuracy = 1, big.mark = ""),
        sec.axis = dup_axis(breaks = extended_range_breaks())
      ) +
      scale_x_discrete() +
      scale_fill_manual(values = colors[3:4], breaks = c(TRUE, FALSE), labels = c("Rural", "Urban")) +
      labs(y = "Raw Score", fill = "") +
      theme_thesis() +
      theme(axis.title.y.right = element_blank(), axis.title.x = element_blank(), legend.position = "none"),
  device = "png", height = 54, width = 150, units = "mm", dpi = "retina", bg = "white"
)

ggsave(
  file = "plots/ece_rural_lang.png",
  plot =
    ggplot(data = ece %>% filter(tipo_eva != "3rd Stage")) +
      geom_boxplot(aes(x = as_factor(periodo), y = m500_l, group = rural_periodo, fill = rural),
        lwd = 0.15,
        color = rgb(20, 20, 20, maxColorValue = 255),
        outlier.size = 0.01,
        outlier.alpha = 0.1
      ) +
      facet_grid(~tipo_eva, scales = "free") +
      scale_y_continuous(
        breaks = breaks_width(width = 100),
        expand = c(0, 0),
        labels = number_format(accuracy = 1, big.mark = ""),
        sec.axis = dup_axis(breaks = extended_range_breaks())
      ) +
      scale_x_discrete() +
      scale_fill_manual(values = colors[3:4], breaks = c(TRUE, FALSE), labels = c("Rural", "Urban")) +
      labs(y = "Raw Score", fill = "") +
      theme_thesis() +
      theme(axis.title.y.right = element_blank(), axis.title.x = element_blank(), legend.position = "bottom"),
  device = "png", height = 60, width = 150, units = "mm", dpi = "retina", bg = "white"
)

ggsave(
  file = "plots/ece_rural_math_3rd.png",
  plot =
    ggplot(data = ece %>% filter(tipo_eva == "3rd Stage")) +
      geom_boxplot(aes(x = as_factor(periodo), y = m500_m, group = rural_periodo, fill = rural),
        lwd = 0.15,
        color = rgb(20, 20, 20, maxColorValue = 255),
        outlier.size = 0.01,
        outlier.alpha = 0.1
      ) +
      facet_grid(~tipo_eva, scales = "free") +
      scale_y_continuous(
        breaks = breaks_width(width = 100),
        expand = c(0, 0),
        labels = number_format(accuracy = 1, big.mark = ""),
        sec.axis = dup_axis(breaks = extended_range_breaks())
      ) +
      scale_x_discrete() +
      scale_fill_manual(values = colors[3:4], breaks = c(TRUE, FALSE), labels = c("Rural", "Urban")) +
      labs(y = "Raw Score", fill = "") +
      theme_thesis() +
      theme(axis.title.y.right = element_blank(), axis.title.x = element_blank(), legend.position = "none"),
  device = "png", height = 54, width = 150, units = "mm", dpi = "retina", bg = "white"
)

ggsave(
  file = "plots/ece_rural_math.png",
  plot =
    ggplot(data = ece %>% filter(tipo_eva != "3rd Stage")) +
      geom_boxplot(aes(x = as_factor(periodo), y = m500_m, group = rural_periodo, fill = rural),
        lwd = 0.15,
        color = rgb(20, 20, 20, maxColorValue = 255),
        outlier.size = 0.01,
        outlier.alpha = 0.1
      ) +
      facet_grid(~tipo_eva, scales = "free") +
      scale_y_continuous(
        breaks = breaks_width(width = 100),
        expand = c(0, 0),
        labels = number_format(accuracy = 1, big.mark = ""),
        sec.axis = dup_axis(breaks = extended_range_breaks())
      ) +
      scale_x_discrete() +
      scale_fill_manual(values = colors[3:4], breaks = c(TRUE, FALSE), labels = c("Rural", "Urban")) +
      labs(y = "Raw Score", fill = "") +
      theme_thesis() +
      theme(axis.title.y.right = element_blank(), axis.title.x = element_blank(), legend.position = "bottom"),
  device = "png", height = 60, width = 150, units = "mm", dpi = "retina", bg = "white"
)

ggsave(
  file = "plots/ece_mujer_lang_3rd.png",
  plot =
    ggplot(data = ece %>% filter(tipo_eva == "3rd Stage") %>% drop_na(mujer)) +
      geom_boxplot(aes(x = as_factor(periodo), y = m500_l, group = mujer_periodo, fill = mujer),
        lwd = 0.15,
        color = rgb(20, 20, 20, maxColorValue = 255),
        outlier.size = 0.01,
        outlier.alpha = 0.1
      ) +
      facet_grid(~tipo_eva, scales = "free") +
      scale_y_continuous(
        breaks = breaks_width(width = 100),
        expand = c(0, 0),
        labels = number_format(accuracy = 1, big.mark = ""),
        sec.axis = dup_axis(breaks = extended_range_breaks())
      ) +
      scale_x_discrete() +
      scale_fill_manual(values = colors[3:4], breaks = c(TRUE, FALSE), labels = c("Male", "Female")) +
      labs(y = "Raw Score", fill = "") +
      theme_thesis() +
      theme(axis.title.y.right = element_blank(), axis.title.x = element_blank(), legend.position = "none"),
  device = "png", height = 54, width = 150, units = "mm", dpi = "retina", bg = "white"
)

ggsave(
  file = "plots/ece_mujer_lang.png",
  plot =
    ggplot(data = ece %>% filter(tipo_eva != "3rd Stage")) +
      geom_boxplot(aes(x = as_factor(periodo), y = m500_l, group = mujer_periodo, fill = mujer),
        lwd = 0.15,
        color = rgb(20, 20, 20, maxColorValue = 255),
        outlier.size = 0.01,
        outlier.alpha = 0.1
      ) +
      facet_grid(~tipo_eva, scales = "free") +
      scale_y_continuous(
        breaks = breaks_width(width = 100),
        expand = c(0, 0),
        labels = number_format(accuracy = 1, big.mark = ""),
        sec.axis = dup_axis(breaks = extended_range_breaks())
      ) +
      scale_x_discrete() +
      scale_fill_manual(values = colors[3:4], breaks = c(TRUE, FALSE), labels = c("Male", "Female")) +
      labs(y = "Raw Score", fill = "") +
      theme_thesis() +
      theme(axis.title.y.right = element_blank(), axis.title.x = element_blank(), legend.position = "bottom"),
  device = "png", height = 60, width = 150, units = "mm", dpi = "retina", bg = "white"
)

ggsave(
  file = "plots/ece_mujer_math_3rd.png",
  plot =
    ggplot(data = ece %>% filter(tipo_eva == "3rd Stage") %>% drop_na(mujer)) +
      geom_boxplot(aes(x = as_factor(periodo), y = m500_m, group = mujer_periodo, fill = mujer),
        lwd = 0.15,
        color = rgb(20, 20, 20, maxColorValue = 255),
        outlier.size = 0.01,
        outlier.alpha = 0.1
      ) +
      facet_grid(~tipo_eva, scales = "free") +
      scale_y_continuous(
        breaks = breaks_width(width = 100),
        expand = c(0, 0),
        labels = number_format(accuracy = 1, big.mark = ""),
        sec.axis = dup_axis(breaks = extended_range_breaks())
      ) +
      scale_x_discrete() +
      scale_fill_manual(values = colors[3:4], breaks = c(TRUE, FALSE), labels = c("Male", "Female")) +
      labs(y = "Raw Score", fill = "") +
      theme_thesis() +
      theme(axis.title.y.right = element_blank(), axis.title.x = element_blank(), legend.position = "none"),
  device = "png", height = 54, width = 150, units = "mm", dpi = "retina", bg = "white"
)

ggsave(
  file = "plots/ece_mujer_math.png",
  plot =
    ggplot(data = ece %>% filter(tipo_eva != "3rd Stage")) +
      geom_boxplot(aes(x = as_factor(periodo), y = m500_m, group = mujer_periodo, fill = mujer),
        lwd = 0.15,
        color = rgb(20, 20, 20, maxColorValue = 255),
        outlier.size = 0.01,
        outlier.alpha = 0.1
      ) +
      facet_grid(~tipo_eva, scales = "free") +
      scale_y_continuous(
        breaks = breaks_width(width = 100),
        expand = c(0, 0),
        labels = number_format(accuracy = 1, big.mark = ""),
        sec.axis = dup_axis(breaks = extended_range_breaks())
      ) +
      scale_x_discrete() +
      scale_fill_manual(values = colors[3:4], breaks = c(TRUE, FALSE), labels = c("Male", "Female")) +
      labs(y = "Raw Score", fill = "") +
      theme_thesis() +
      theme(axis.title.y.right = element_blank(), axis.title.x = element_blank(), legend.position = "bottom"),
  device = "png", height = 60, width = 150, units = "mm", dpi = "retina", bg = "white"
)

ggsave(
  file = "plots/ece_estatal_lang_3rd.png",
  plot =
    ggplot(data = ece %>% filter(tipo_eva == "3rd Stage")) +
      geom_boxplot(aes(x = as_factor(periodo), y = m500_l, group = estatal_periodo, fill = estatal),
        lwd = 0.15,
        color = rgb(20, 20, 20, maxColorValue = 255),
        outlier.size = 0.01,
        outlier.alpha = 0.1
      ) +
      facet_grid(~tipo_eva, scales = "free") +
      scale_y_continuous(
        breaks = breaks_width(width = 100),
        expand = c(0, 0),
        labels = number_format(accuracy = 1, big.mark = ""),
        sec.axis = dup_axis(breaks = extended_range_breaks())
      ) +
      scale_x_discrete() +
      scale_fill_manual(values = colors[3:4], breaks = c(TRUE, FALSE), labels = c( "Public", "Private")) +
      labs(y = "Raw Score", fill = "") +
      theme_thesis() +
      theme(axis.title.y.right = element_blank(), axis.title.x = element_blank(), legend.position = "none"),
  device = "png", height = 54, width = 150, units = "mm", dpi = "retina", bg = "white"
)

ggsave(
  file = "plots/ece_estatal_lang.png",
  plot =
    ggplot(data = ece %>% filter(tipo_eva != "3rd Stage")) +
      geom_boxplot(aes(x = as_factor(periodo), y = m500_l, group = estatal_periodo, fill = estatal),
        lwd = 0.15,
        color = rgb(20, 20, 20, maxColorValue = 255),
        outlier.size = 0.01,
        outlier.alpha = 0.1
      ) +
      facet_grid(~tipo_eva, scales = "free") +
      scale_y_continuous(
        breaks = breaks_width(width = 100),
        expand = c(0, 0),
        labels = number_format(accuracy = 1, big.mark = ""),
        sec.axis = dup_axis(breaks = extended_range_breaks())
      ) +
      scale_x_discrete() +
      scale_fill_manual(values = colors[3:4], breaks = c(TRUE, FALSE), labels = c( "Public", "Private")) +
      labs(y = "Raw Score", fill = "") +
      theme_thesis() +
      theme(axis.title.y.right = element_blank(), axis.title.x = element_blank(), legend.position = "bottom"),
  device = "png", height = 60, width = 150, units = "mm", dpi = "retina", bg = "white"
)

ggsave(
  file = "plots/ece_estatal_math_3rd.png",
  plot =
    ggplot(data = ece %>% filter(tipo_eva == "3rd Stage")) +
      geom_boxplot(aes(x = as_factor(periodo), y = m500_m, group = estatal_periodo, fill = estatal),
        lwd = 0.15,
        color = rgb(20, 20, 20, maxColorValue = 255),
        outlier.size = 0.01,
        outlier.alpha = 0.1
      ) +
      facet_grid(~tipo_eva, scales = "free") +
      scale_y_continuous(
        breaks = breaks_width(width = 100),
        expand = c(0, 0),
        labels = number_format(accuracy = 1, big.mark = ""),
        sec.axis = dup_axis(breaks = extended_range_breaks())
      ) +
      scale_x_discrete() +
      scale_fill_manual(values = colors[3:4], breaks = c(TRUE, FALSE), labels = c( "Public", "Private")) +
      labs(y = "Raw Score", fill = "") +
      theme_thesis() +
      theme(axis.title.y.right = element_blank(), axis.title.x = element_blank(), legend.position = "none"),
  device = "png", height = 54, width = 150, units = "mm", dpi = "retina", bg = "white"
)

ggsave(
  file = "plots/ece_estatal_math.png",
  plot =
    ggplot(data = ece %>% filter(tipo_eva != "3rd Stage")) +
      geom_boxplot(aes(x = as_factor(periodo), y = m500_m, group = estatal_periodo, fill = estatal),
        lwd = 0.15,
        color = rgb(20, 20, 20, maxColorValue = 255),
        outlier.size = 0.01,
        outlier.alpha = 0.1
      ) +
      facet_grid(~tipo_eva, scales = "free") +
      scale_y_continuous(
        breaks = breaks_width(width = 100),
        expand = c(0, 0),
        labels = number_format(accuracy = 1, big.mark = ""),
        sec.axis = dup_axis(breaks = extended_range_breaks())
      ) +
      scale_x_discrete() +
      scale_fill_manual(values = colors[3:4], breaks = c(TRUE, FALSE), labels = c( "Public", "Private")) +
      labs(y = "Raw Score", fill = "") +
      theme_thesis() +
      theme(axis.title.y.right = element_blank(), axis.title.x = element_blank(), legend.position = "bottom"),
  device = "png", height = 60, width = 150, units = "mm", dpi = "retina", bg = "white"
)

## Settlements
settlements.2017 <- readRDS('Centros poblados/directorio.rds') %>%
  select(population.2017, nomcp) %>%
  filter(population.2017 != 0) %>%
  mutate(nomcp = str_to_title(nomcp)) %>%
  pivot_longer(cols = c(population.2017), names_sep = "\\.", names_to = c("variable", "year"))

settlements.2015 <- read_excel('Centros poblados/ListadoCentroPobladosMTC.xlsx', na = c('Sin Reg', 'NA', "")) %>%
  rename_with(str_to_lower) %>%
  rename(nomcp = ccpp, population.2015 = habitantes) %>%
  select(nomcp, population.2015) %>%
  filter(population.2015 != 0) %>%
  mutate(nomcp = str_to_title(nomcp)) %>%
  pivot_longer(population.2015, names_sep = "\\.", names_to = c("variable", "year"))

settlements <- bind_rows(settlements.2015, settlements.2017)

# Number of big settlements (>= 2000)
settlements.2017 %>%
  mutate(big = value >= 2000) %>%
  count(big)

settlements.2015 %>%
  mutate(big = value >= 2000) %>%
  count(big)

ggsave_pdf(
  file = "plots/settlements_density.pdf",
  plot =
    ggplot(settlements) + 
    stat_ecdf(aes(x = value, color = year)) +
    coord_cartesian(xlim = c(1, 1000)) +
    scale_y_continuous(
      breaks = pretty_breaks(),
      expand = c(0, 0),
      labels = percent_format(),
      sec.axis = dup_axis(breaks = pretty_breaks())
    ) +
    scale_x_continuous(
      breaks = pretty_breaks(n = 10),
      expand = c(0, 0)
    ) +
    scale_color_manual(values = colors[1:2]) +
    labs(y = "Cum. pct. (settlements)", x = "N. of inhabitants per settlement" , color = "") +
    theme_thesis() +
    theme(axis.title.y.right = element_blank(), legend.position = "bottom", plot.margin = grid::unit(c(1,0,0,0),"mm")),
  device = "pdf", height = 60, width = 70, units = "mm"
)

# Density difference
settlements_diff <- settlements %>%
  group_by(year) %>%
  count(value) %>%
  pivot_wider(id_cols = value, values_from = n, names_from = year, values_fill = 0) %>%
  mutate(
    diff = `2015` - `2017`,
    type = case_when(
      diff >  0 ~ "2015 > 2017",
      diff <  0 ~ "2015 < 2017",
      diff == 0 ~ "2015 = 2017",
    )
    )

settlement_names <- settlements %>% 
  filter(nomcp %in% c("San Juan De Lurigancho", "Vitarte", "Ventanilla", "Lima", "Chimbote", "Iquitos"), value > 485) %>%
  mutate(nomcp = nomcp %>% str_replace_all("De", "de"), value = value/1000) %>%
  arrange(nomcp, year) %>%
  select(nomcp, year, value) %>%
  mutate(x = c(-50, -50, 50, 50, 50, 50, -50, -50, -50, -50, 50, 50))

ggsave_pdf(
  file = "plots/settlements_population.pdf",
  plot =
    ggplot() + 
    geom_point(data=settlements_diff, aes(x=diff, y=value/1000, color = type), alpha = 0.5, size = 0.25) +
    geom_path(data=settlement_names, mapping=aes(x=x, y=value, group=nomcp), size = 0.25) +
    scale_y_continuous(
      expand = c(0, 0),
      labels = number_format(big.mark = ""),
      sec.axis = dup_axis(breaks = extended_range_breaks())
    ) +
    scale_x_continuous(
      expand = c(0, 0),
      breaks = extended_range_breaks()
    ) +
    scale_color_manual(breaks = c("2015 > 2017", "2015 < 2017", "2015 = 2017"), values = c(colors[1:2], "black")) +
    labs(y = "N. of inhabitants per settlement (thous.)", x = "N. of settlements", color = "") +
    theme_thesis() +
    theme(axis.title.y.right = element_blank(), legend.position = "bottom", plot.margin = grid::unit(c(1,0,0,0),"mm")),
  device = "pdf", height = 60, width = 70, units = "mm"
)

cluster_size <- read_csv("Centros poblados/cluster_list.csv") %>%
  mutate(across(where(is.numeric), as.integer)) %>%
  select(starts_with("id")) %>%
  pivot_longer(cols = everything()) %>%
  group_by(name, value) %>%
  count(name = "size") %>%
  mutate(noise = value == 0) %>%
  group_by(name, size, noise) %>%
  count(name = "n.clusters")

clusters <- readRDS(file = "Centros poblados/clusters.rds")
  
clusters_summary <- clusters %>%
  group_by(cluster, area_cluster) %>%
  count() %>%
  filter(n != 1) %>%
  mutate(area_cluster = as.numeric(area_cluster))

ggsave(
  file = "plots/cluster_areanum.png",
  plot =
    ggplot(clusters_summary) + 
    geom_point(aes(y=n, x=area_cluster), alpha = 0.5, size = 0.25, color = colors[5]) +
    scale_y_continuous(
      expand = c(0, 0),
      labels = number_format(big.mark = ""),
      sec.axis = dup_axis(breaks = extended_range_breaks())
    ) +
    scale_x_continuous(
      expand = c(0, 0),
      breaks = extended_range_breaks(),
      labels = number_format(big.mark = "", accuracy = 2)
    ) +
    labs(y = "N. of settlements per cluster", x = "Area (km)", color = "") +
    theme_thesis() +
    theme(axis.title.y.right = element_blank(), legend.position = "bottom", plot.margin = grid::unit(c(1,0,0,0),"mm")),
  device = "png", height = 60, width = 70, units = "mm"
)

ggsave(
  file = "plots/cluster_area.png",
  plot =
    ggplot(clusters_summary) + 
    geom_histogram(aes(x=area_cluster, y=..count../sum(..count..)), fill = colors[5], bins = 50) +
    scale_y_continuous(
      expand = c(0, 0),
      labels = percent_format(accuracy = 1),
      sec.axis = dup_axis(breaks = extended_range_breaks())
    ) +
    scale_x_continuous(
      expand = c(0, 0),
      breaks = extended_range_breaks(),
      labels = number_format(big.mark = "", accuracy = 2)
    ) +
    labs(y = "Frequency", x = "Area (km)", color = "") +
    theme_thesis() +
    theme(axis.title.y.right = element_blank(), legend.position = "bottom", plot.margin = grid::unit(c(1,0,0,0),"mm")),
  device = "png", height = 60, width = 70, units = "mm"
)

centros_poblados <- readRDS(file = "Centros poblados/centros_poblados.rds") %>%
  st_drop_geometry() %>%
  left_join(clusters)

settlements <- centros_poblados %>%
  group_by(ubigeo, cluster) %>%
  summarise(
    population = sum(population.2017, na.rm = TRUE)
  ) %>%
  group_by(ubigeo) %>%
  summarise(
    population = sum(population),
    n = n(),
    density = population/n
  ) %>%
  mutate(density.q = xtile(density, n = 100))

peru <- st_read("Centros poblados/distritos/DISTRITOS.shp") %>%
  rename(ubigeo = IDDIST) %>%
  select(ubigeo) %>%
  mutate(ubigeo = as.integer(ubigeo)) %>%
  inner_join(settlements)

ggsave(
  file = "plots/cluster_number.png",
  plot =
    ggplot(peru) + geom_sf(aes(fill = n), color = NA) +
    theme_thesis() +
    scale_fill_viridis(name="N. of Clusters") +
    theme(axis.title.y = element_blank(), axis.line=element_blank(), axis.text.x=element_blank(), legend.title=element_text(size=5),
          axis.text.y=element_blank(), axis.ticks=element_blank(),legend.position = "left", plot.margin = grid::unit(c(1,0,0,0),"mm")),
  device = "png", height = 60, width = 70, units = "mm"
)

ggsave(
  file = "plots/cluster_population.png",
  plot =
    ggplot(peru) + geom_sf(aes(fill = density.q), color = NA) +
    theme_thesis() +
    scale_fill_viridis(name="Percentiles \n(Avg. Pop. per Cluster)") +
    theme(axis.title.y = element_blank(), axis.line=element_blank(), axis.text.x=element_blank(), legend.title=element_text(size=5),
          axis.text.y=element_blank(), axis.ticks=element_blank(),legend.position = "left", plot.margin = grid::unit(c(1,0,0,0),"mm")),
  device = "png", height = 60, width = 70, units = "mm"
)

education_supply <- tbl(db_conn, "education_supply") %>%
  collect() %>%
  group_by(year) %>%
  summarise(
    section_a = sum(section_a)/1000,
    section_b0 = sum(section_b0)/1000,
    section_f0 = sum(section_f0)/1000,
  ) %>%
  pivot_longer(cols = matches("section")) %>%
  mutate(
    name = case_when(
      name == "section_a" ~ "Preschool",
      name == "section_b0" ~ "Primary",
      name == "section_f0" ~ "Secondary",
    )
  )
  
cohorts <- data.frame(
  date_start= as_date(c(2005, 2013)),
  date_end = as_date(c(2010, 2017))
  )

ggsave_pdf(
  file = "plots/edu_supply.pdf",
  plot =
    ggplot(data = education_supply %>% filter(year > 2003)) +
    geom_rect(data = cohorts, aes(xmin = date_start, xmax = date_end, ymin = -Inf, ymax = Inf),
              fill = "gray", alpha= 0.3) +
    geom_line(aes(x = year, y = value, group = name, color = name)) +
    facet_grid(~name, scales = "free_y") +
    scale_y_continuous(
      expand = c(0, 0),
      labels = number_format(accuracy = 1, big.mark = ""),
      sec.axis = dup_axis(breaks = extended_range_breaks())
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_color_manual(values = colors[5:7]) +
    labs(y = "N. of Schools (thous.)", color = "") +
    theme_thesis() +
    theme(axis.title.y.right = element_blank(), axis.title.x = element_blank(), legend.position = "none"),
  device = "pdf", height = 60, width = 150, units = "mm"
)

support <- readRDS(file = "failure.rds") %>%
  mutate(preschool_itt = as.numeric(preschool_itt) - 1)
## Reordering support$type
support$type <- fct_relevel(
  support$type,
  "Second Stage", "Fourth Stage", "Sixth Stage"
)

ggsave_pdf(
  file = "plots/support.pdf",
  plot =
    ggplot(support %>% filter(preschool_itt == 1)) +
    geom_density(aes(x = q_x, linetype = switcher_itt), bw = 0.01) +
    facet_grid(~type, scales = "free_y") +
    scale_y_continuous(
      expand = c(0, 0),
      labels = number_format(accuracy = 1, big.mark = ""),
      sec.axis = dup_axis(breaks = extended_range_breaks())
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_color_manual(values = colors[5:7]) +
    labs(color = "") +
    theme_thesis() +
    theme(axis.title.y= element_blank(), axis.title.x = element_blank(), legend.position = "botom"),
  device = "pdf", height = 60, width = 150, units = "mm"
)

ggsave_pdf(
  file = "plots/support2.pdf",
  plot =
    ggplot(support %>% filter(preschool_itt == 1)) +
    geom_density(aes(x = p_x, linetype = switcher_itt), bw = 0.01) +
    facet_grid(~type, scales = "free_y") +
    scale_y_continuous(
      expand = c(0, 0),
      labels = number_format(accuracy = 1, big.mark = ""),
      sec.axis = dup_axis(breaks = extended_range_breaks())
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_color_manual(values = colors[5:7]) +
    labs(color = "") +
    theme_thesis() +
    theme(axis.title.y= element_blank(), axis.title.x = element_blank(), legend.position = "botom"),
  device = "pdf", height = 60, width = 150, units = "mm"
)


