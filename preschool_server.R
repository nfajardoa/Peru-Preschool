
# Set working directory
setwd("Documents/BRIQ/Stephanie/")

# Load packages and functions
source("preschoolpolev_packages.R")
source("preschoolpolev_functions.R")

# Plan Multiprocessing
plan(multiprocess, workers = 60)

# Load csv data
last_names <- read_csv("last_names.csv")

# Execute 

