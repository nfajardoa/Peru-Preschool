setwd("Documents/BRIQ/Stephanie/")

source('~/Documents/BRIQ/Stephanie/preschool_packages.R') #Load packages
source('~/Documents/BRIQ/Stephanie/preschool_functions.R') #Load functions

#extrafont::font_import(paths = '~/.local/share/fonts/') #Run Once
#remotes::install_version("Rttf2pt1", version = "1.3.8")

## The POSTGRESQL Database have to be manually set before using this code
db_conn <- dbConnect(RPostgres::Postgres(), dbname = "peru", host = "localhost", 
                     port = 5432, user = "rwriter", password = "12345")

source('~/Documents/BRIQ/Stephanie/preschool_database.R') #Run only once
source('~/Documents/BRIQ/Stephanie/preschool_enrollment_filter.R') #Run only once
source('~/Documents/BRIQ/Stephanie/preschool_schools.R') #Run only once




