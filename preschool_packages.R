### PACKAGES LOADING AND INSTALLATION

# Package names
packages <- c("nnet", 'extrafont', 'matrixStats', "mice", "devtools", "foreign", "haven", "DBI", 'fastDummies', 
              "glue", "stringr", "magrittr", "lubridate", "scales", "skimr", "readxl", "ggthemes", "showtext",
              "bit64", "stringi", "sjmisc", "stringdist", "rgeos", "geosphere", "rgdal", "textshape",
              "textclean", "stringr", "janitor", "rnaturalearth", "tictoc", "RColorBrewer", "sf", "pstest",
              'questionr', 'styler', "polycor", 'psych', 'fuzzyjoin', "estimatr", "data.table", "dbplyr", "estimatr",
              "purrr", "dtplyr", "furrr", "dplyr", "tidyverse", "stargazer", "parallel", "cobalt", "compareGroups", "furrr", "modelsummary")

packages_github <- c("ropenscilabs/rnaturalearthhires", "ropenscilabs/rnaturalearthdata", 'daranzolin/ViewPipeSteps')

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Install packages not yet installed
installed_packages <- packages_github %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  devtools::install_github(packages_github[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))
invisible(lapply(packages_github %>% str_extract(pattern = "/(.*)$") %>% str_remove_all("/"), 
                 library, character.only = TRUE))

# Options
options(future.globals.maxSize = 891289600, na.action = "na.pass")
extrafont::loadfonts(device = "pdf", quiet = TRUE)
