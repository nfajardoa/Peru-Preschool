### PACKAGES LOADING AND INSTALLATION

# Package names

packages <- c("haven", "dplyr", "DBI", "dbplyr", "purrr", "readxl", "glue", "stringr", "magrittr", "tidyr", "lubridate", "tidyverse", "bit64") 
# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# Options
options(future.globals.maxSize = 891289600)
