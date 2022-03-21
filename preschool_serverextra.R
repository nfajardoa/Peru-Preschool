setwd("Documents/BRIQ/Stephanie/")

source('preschool_packages.R') #Load packages
source('preschool_functions.R') #Load functions

data_ise <- readRDS("ise_reduced.rds")

#ISE
ise <- data_ise %>%
  run_ise(m = 10, maxit = 50, visitSequence = "monotone", cluster.seed = 343, n.core = 30)

saveRDS(ise, file = "imputed_ise.rds")



